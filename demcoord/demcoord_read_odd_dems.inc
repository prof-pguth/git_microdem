{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2026 Peter L. Guth  }
{----------------------------------------}
{ include file for demcoord              }
{________________________________________}


   procedure ReadNGDCGRD98file(DEMFileName : PathStr; var Error : boolean);
   var
      Inf : file;
      RawHeader : packed array[1..128] of byte;
      Header : packed array[0..31] of LongInt;
      UndefinedCode,i,j,z   : integer;
      ZMult : float64;
      Dir : DirStr;
      bName : NameStr;
      Ext  : ExtStr;
      SwapBytes : boolean;
      Row  : array[0..24000] of SmallInt;
      LongRow : array[0..24000] of Integer;
   begin
      Error := true;
      AssignFile(inf,DEMFilename);
      reset(inf,1);

      BlockRead(inf,RawHeader,128);
      for i := 0 to 31 do begin
         Header[i] := MakeLongInt(Rawheader[i*4+1],Rawheader[i*4+2],Rawheader[i*4+3],Rawheader[i*4+4],true);
         {$IfDef  RecordReadDEM} WriteLineToDebugFile(IntToStr(i) + '   ' + IntToStr(Header[i])); {$EndIf}
      end;

      SwapBytes := true;
      if (Header[0] <> 1000000001) then begin
         for i := 0 to 31 do begin
            Header[i] := MakeLongInt(Rawheader[i*4+4],Rawheader[i*4+3],Rawheader[i*4+2],Rawheader[i*4+1],true);
            {$IfDef  RecordReadDEM} WriteLineToDebugFile(IntToStr(i) + '   ' + IntToStr(Header[i])); {$EndIf}
         end;
         SwapBytes := false;
         if (Header[0] <> 1000000001) then exit;
      end;

      FSplit(DEMFileName,Dir,BName,Ext);
      with DEMheader do begin
         AreaName := bName;
         DEMUsed := ArcSecDEM;
         DataSpacing  := SpaceDegrees;
         UndefinedCode := Header[17];
         UTMZone := GetUTMZone(Header[8]);
         NumCol := Header[12];
         NumRow  := Header[7];
         DEMheader.DEMySpacing := Header[6] / 3600;
         DEMheader.DEMxSpacing := Header[11] / 3600;

         if (Header[16] = 10) then ZMult := 0.1 else ZMult := 1;

         if UpperCase(ptCopy(bName,1,5)) = 'ETOPO' then DEMheader.DEMSWCornerX := -180
         else DEMheader.DEMSWCornerX := Header[8] + Header[9] /60 + Header[10] / 3600;
         DEMheader.DEMSWCornerY := Header[3] + Header[4] /60 + Header[5] / 3600 - pred(NumRow) * DEMheader.DEMySpacing;

         DigitizeDatum := NAD83d;
         //StringToByteArray('WGS84',DEMheader.DMAMapDefinition.h_DatumCode);
         DEMheader.h_DatumCode := 'WGS84';
         ElevUnits := euMeters;

         if not AllocateDEMMemory(InitDEMnone) then exit;
         StartProgress('Read');
         for j := pred(NumRow) downto 0 do begin
            if (j mod 500 = 0) then UpdateProgressBar((NumRow-j)/pred(NumRow));
            if Header[18] = 2 then BlockRead(inf,Row,NumCol*2)
            else BlockRead(inf,LongRow,NumCol*4);
            for i := 0 to pred(NumCol) do begin
               if (Header[18] = 2) then begin
                  z := Row[i];
                  if SwapBytes then z := Swap(Row[i]);
               end
               else begin
                  z := LongRow[i];
                  if SwapBytes then Int4Swap(z);
               end;
               if (z = UndefinedCode) then SetGridMissing(i,j)
              else SetGridElevation(i,j,z*ZMult);
            end {for j};
         end {for i};
         EndProgress;
         CheckMaxMinElev;
      end;
      Error := false;
      closeFile(inf);
   end;


   {$IfDef ExImportNOSEEZFiles}
   {$Else}

   procedure ReadNOAAEEZfile(FName: PathStr);
   var
      HeaderLines,UndefinedCode,i,j,k,err,z  : integer;
      Dir : DirStr;
      bName : NameStr;
      Ext  : ExtStr;
      PRUFile  : text;
      zs : tStringList;
   begin
      {$IfDef RecordReadDEM} WriteLineToDebugFile('Open NOAA EEZ file '+ FName); {$EndIf}

      FSplit(FName,Dir,BName,Ext);
      with DEMheader do begin
         AreaName := bName;
         DEMUsed := UTMBasedDEM;
         DataSpacing  := SpaceMeters;
         assignFile(PRUfile,FName);
         reset(PRUfile);
         HeaderLines := 0;

         repeat
            inc(HeaderLines);
            readln(PRUFile,TStr);
         until ptCopy(TStr,23,6) = '<IROW>';
         reset(PRUfile);
         for i := 1 to pred(HeaderLines) do readln(PRUFile);
         readln(PRUFile,NumRow);
         readln(PRUFile,NumCol);
         readln(PRUfile,MaxElev);
         readln(PRUfile,MinElev);
         readln(PRUfile,UndefinedCode);
         for i := 1 to 4 do readln(PRUfile);  {latitude of corners}
         readln(PRUfile,DEMSWCornerX);
         readln(PRUfile);                     {max xutm}
         readln(PRUfile,DEMSWCornerY);
         readln(PRUfile);                     {max yutm}
         readln(PRUFile,DEMxSpacing);
         readln(PRUFile,DEMySpacing);
         readln(PRUfile,UTMZone);
         closeFile(PRUfile);

         DigitizeDatum := NAD83d;
         DEMheader.h_DatumCode := 'WGS84';
         //StringToByteArray('WGS84', DEMheader.DMAMapDefinition.h_DatumCode);
         ElevUnits := euMeters;
         if ReallyReadDEM then begin
            if not AllocateDEMMemory(InitDEMMissing) then exit;
            if FileExists(Dir + bName + '.DAT') then try
               InsureFileIsNotReadOnly(Dir + bName + '.DAT');
               zs := tStringList.Create;
               zs.LoadFromFile(Dir + bName + '.DAT');
               StartProgress('Read EEZ bathy ' + bName);
   {$I-}
               k := 0;
               if zs.Count > 0 then try
                  for i := 0 to pred(NumCol) do begin
                     if i mod 25 = 0 then UpdateProgressBar(i/pred(NumCol));
                     for j := pred(NumRow) downto 0 do begin
                        TStr := zs.strings[k];
                        inc(k);
                        val(TStr,z,err);
                        if z <> UndefinedCode then SetGridElevation(i,j,z);  //DTEDCols[i]^[j] := z;
                     end {for j};
                  end {for i};
                  Error := false;
                  zs.Free;
                  CheckMaxMinElev;
               except
                  on EInOutError do Error := true;
               end;
   {$I+}
            finally
               EndProgress;
            end;
         end;
      end {with};
   end {proc ReadNOAAEEZfile};
   {$EndIf}


procedure ReadNEDBIL(fName : PathStr; var Error : boolean; SureAboutZUnits : boolean = false);
type
   bRow = array[0..MaxElevArraySize] of byte;
   aRow = array[0..MaxElevArraySize] of integer;
   SingleBytes = array[1..4] of byte;
   tFloatRow = array[0..MaxElevArraySize] of SingleBytes;
var
   Row : ^aRow;
   Row8bit : ^bRow;
   fRow : ^tFloatRow;
   Row16Bit : tElevColPointer;
   NumBits,
   err,i,k,x,y : integer;
   NoData,
   value : float64;
   Inf : file;
   FP_file,
   SwapEm : boolean;
   Dir : DirStr;
   bName : NameStr;
   Ext : ExtStr;
   DataFile : tStringList;


   function ReadValue(Input : ShortString) : integer;
   var
      i,err : integer;
   begin
      for i := length(Input) downto 1 do
         if not (Input[i] in ['0'..'9','-']) then Delete(Input,i,1);
      Val(Input,Result,err);
   end;

   function ReadRealValue(Input : ShortString) : float64;
   var
      i,err : integer;
   begin
      for i := length(Input) downto 1 do
         if not (Input[i] in ['0'..'9','-','.','e','E']) then Delete(Input,i,1);
      Val(Input,Result,err);
   end;


   function MakeSingle(v : SingleBytes) : Single;
   var
      t : byte;
   begin
      {$IfDef RecordReadMakeSingle} WriteLineToDebugFile('Make single: ' + IntToStr(v[1]) + '/' + IntToStr(v[2]) + '/'+ IntToStr(v[3]) + '/' + IntToStr(v[4])); {$EndIf}
      if SwapEm then begin
         t := v[4];
         v[4] := v[1];
         v[1] := t;
         t := v[2];
         v[2] := v[3];
         v[3] := t;
      end;
      Move(v,Result,4);
      {$IfDef RecordReadMakeSingle} WriteLineToDebugFile('MakeSingle OK'); {$EndIf}
   end;

   procedure CheckLatLongOrUTM;
   var
      Hemi : AnsiChar;
      DatCode : ShortString;
   begin
      with DEMheader do begin
         if (abs(DEMSWCornerX) > 180) then begin
            DataSpacing := SpaceMeters;
            DEMUsed := UTMBasedDEM;
            UTMZone := MDDef.DefaultUTMZone;
            LatHemi := MDDef.DefaultLatHemi;
            {$IfDef VCL}
               GetMapParameters(Hemi,UTMZone,DatCode,true);
            {$EndIf}
         end
         else begin
            UtmZone := GetUTMZone(DEMSWCornerX + NumCol * 0.5 * DEMxSpacing);
         end;
      end;
   end;

begin {procedure ReadNEDBIL}
   {$IfDef RecordReadDEM} WriteLineToDebugFile('ReadNEDBIL in'); {$EndIf}
   FSplit(fName,Dir,bName,Ext);
   Error := true;
   FP_file := (Bname = 'DEMFLOAT') or ExtEquals(Ext, '.FLT');
   if not FileExists(Dir + bName + '.hdr') then begin
      if ShowSatProgress then MessageToContinue('NED files incomplete');
      exit;
   end;

   with DEMheader do begin
      AreaName := bName;
      DataFile := tStringList.Create;
      DataFile.LoadFromFile(Dir + bName + '.hdr');
      {$IfDef RecordReadDEM} WriteLineToDebugFile('hdr file'); WriteStringListToDebugFile(DataFile); {$EndIf}
      DEMUsed := ArcSecDEM;
      DigitizeDatum := NAD83d;
      DataSpacing := SpaceDegrees;
      MinElev := -11000;
      MaxElev := 8000;
      NoData := MaxSmallInt;

      if FP_file then begin
         NumCol := ReadValue(DataFile.Strings[0]);
         NumRow := ReadValue(DataFile.Strings[1]);
         DEMSWCornerX := ReadRealValue(ptTrim(AfterSpecifiedCharacter(DataFile.Strings[2],' ')));
         DEMSWCornerY := ReadRealValue(ptTrim(AfterSpecifiedCharacter(DataFile.Strings[3],' ')));
         DEMySpacing := ReadRealValue(ptTrim(AfterSpecifiedCharacter(DataFile.Strings[4],' ')));
         NoData := ReadRealValue(ptTrim(AfterSpecifiedCharacter(DataFile.Strings[5],' ')));
         DEMxSpacing := DEMySpacing;
         TStr := UpperCase(ptTrim(AfterSpecifiedCharacter(DataFile.Strings[6],' ')));
         {$IfDef RecordReadDEM} WriteLineToDebugFile(UpperCase(ptTrim(AfterSpecifiedCharacter(DataFile.Strings[6],' ')))); {$EndIf}
         Swapem := TStr[1] = 'M';
         CheckLatLongOrUTM;
      end
      else begin
         TStr := ptTrim(AfterSpecifiedCharacter(DataFile.Strings[0],' '));
         Swapem := (TStr <> 'I');
         NumRow := ReadValue(DataFile.Strings[2]);
         NumCol := ReadValue(DataFile.Strings[3]);
         NumBits := ReadValue(DataFile.Strings[5]);
         if (DataFile.Count > 9) then NoData := ReadValue(DataFile.Strings[9]);

         DataFile.LoadFromFile(Dir + bName + '.blw');
         {$IfDef RecordReadDEM} WriteLineToDebugFile('blw file'); WriteStringListToDebugFile(DataFile); {$EndIf}
         Val(ptTrim(DataFile.Strings[0]),DEMxSpacing,err);
         Val(ptTrim(DataFile.Strings[3]),DEMySpacing,err);
         Val(ptTrim(DataFile.Strings[4]),DEMSWCornerX,err);

         Val(ptTrim(DataFile.Strings[5]),DEMSWCornerY,err);
         DEMySpacing := abs(DEMySpacing);
         DEMSWCornerY := DEMSWCornerY - pred(NumRow) * DEMySpacing;
         CheckLatLongOrUTM;
      end;

      if (NumCol > MaxColsInRAM) or (NumRow > MaxElevArraySize) then begin
         {$IfDef RecordReadDEM} WriteLineToDebugFile(DEMtooLargeString); {$EndIf}
         MessageToContinue(DEMtooLargeString);
         Self.Destroy;
         {$IfDef MSWindows}
            Self := Nil;
         {$EndIf}
         exit;
      end;

      if FileExists(Dir + bName + '.prj') then begin
         DataFile.LoadFromFile(Dir + bName + '.prj');
         {$IfDef RecordReadDEM} WriteLineToDebugFile('prj file'); WriteStringListToDebugFile(DataFile); {$EndIf}
         for k := 0 to pred(DataFile.Count) do begin
            TStr := DataFile.Strings[k];
            if UpperCase(ptCopy(TStr,1,6)) = 'ZUNITS' then begin
               Delete(TStr,1,8);
               TStr := ptTrim(UpperCase(TStr));
               for i := length(TStr) downto 1 do
                  if not (TStr[i] in ['A'..'Z']) then Delete(TStr,i,1);
               if TStr = 'DECIMETER' then ElevUnits := euDeciMeters;
               if TStr = 'CENTIMETER' then ElevUnits := euCentiMeters;
               if TStr = 'METER' then ElevUnits := euMeters;
            end;
            if UpperCase(ptCopy(TStr,1,5)) = 'DATUM' then begin
               Delete(TStr,1,5);
               TStr := ptTrim(UpperCase(TStr));
               if (TStr = 'WGS84') then DigitizeDatum := WGS84d;
               if (TStr = 'NAD83') then DigitizeDatum := NAD83d;
            end;
         end;
      end
      else begin
         ElevUnits := euMeters;
      end;
      DataFile.Free;

      if FP_file or (NumBits in [8,16,32]) then assignFile(inf,FName);

      if FP_file then begin
         New(fRow);
         reset(inf,4*(NumCol));
         DEMheader.ElevUnits := euMeters;
         DEMPrecision := FloatingPointDEM;
      end
      else begin
         if (NumBits = 32) then begin
            New(Row);
            reset(inf,4*(NumCol));
         end
         else if (NumBits = 16) then begin
            New(Row16Bit);
            reset(inf,2*(DEMheader.NumCol));
         end
         else if (NumBits = 8) then begin
            New(Row8bit);
            reset(inf,NumCol);
         end
         else begin
            exit;
         end;
      end;

      if ReallyReadDEM then begin
         if not AllocateDEMMemory(InitDEMMissing) then exit;
         {$IfDef RecordReadDEM} WriteLineToDebugFile('AllocateDEMMemory OK, start read'); {$EndIf}
         StartProgress('DEM');
         for y := pred(NumRow) downto 0 do begin
            if (y mod 50 = 0) then UpdateProgressBar( (NumRow-y)/NumRow);
            if FP_file then begin
               BlockRead(inf,frow^,1);
               for x := 0 to pred(NumCol) do begin
                  Value := MakeSingle(frow^[x]);
                  if (abs(Value - NoData) > 0.001) then SetGridElevation(x,y,Value);
               end;
            end
            else begin
               if (NumBits = 32) then begin
                  BlockRead(inf,row^,1);
                  if SwapEm then for x := 0 to pred(NumCol) do Int4Swap(Row^[x]);
                  for x := 0 to pred(NumCol) do if (abs(Row^[x] - NoData) > 0.001) then SetGridElevation(x,y,Row^[x]);
               end;
               if (NumBits = 16) then begin
                  BlockRead(inf,row16Bit^,1);
                  if SwapEm then for x := 0 to pred(NumCol) do Row16Bit^[x] := Swap(Row16Bit^[x]);
                  for x := 0 to pred(NumCol) do if (Row16Bit^[x] <> MaxSmallInt) and (Row16Bit^[x] <> -MaxSmallInt) then SetGridElevation(x,y,Row16Bit^[x]);
               end;
               if (NumBits = 8) then begin
                  BlockRead(inf,row8Bit^,1);
                  for x := 0 to pred(NumCol) do if (abs(Row8Bit^[x] - NoData) > 0.001) then SetGridElevation(x,y,Row8Bit^[x]);
               end;
            end;
         end;
         EndProgress;
         DeleteMissingDataPoints;
         CheckMaxMinElev;
      end;

      try
         closeFile(inf);
      except
         on EinOutError do begin
            {$IfDef Record} WriteLineToDebugFile('Problem with ' + fName); {$EndIf}
         end;
      end;

      if FP_file then Dispose(fRow)
      else if (NumBits = 8) then Dispose(Row8Bit)
      else if (NumBits = 16) then Dispose(Row16Bit)
      else if (NumBits = 32) then Dispose(Row);
      DefineDEMvariables(True);
      Error := false;
      {$IfDef RecordReadDEM} WriteLineToDebugFile('NED read out'); {$EndIf}
   end;
end {procedure ReadNEDBIL};


{$IfDef ExcludeUKOSDEM}
{$Else}
      function ReadOSfile(DEMFileName : PathStr) : boolean;
      begin
         {$IfDef RecordReadDEM} WriteLineToDebugFile('ReadOSfile ' + DEMFileName); {$EndIf}
         DEMheader.DataSpacing := SpaceMeters;
         DEMheader.DEMUsed := UTMBasedDEM;
         DEMheader.DigitizeDatum := UK_OS_grid;
         DEMMapProj := CreateUKOSprojection;
         ReadASCIIArcGrid(DEMFileName,Self,Result,true);
      end;
{$EndIf}


      procedure TryOddball(var DEMFileName : PathStr);
      var
         ofName : PathStr;
      begin
         if ExtEquals(Ext, '.IMG') then begin
            ofName := ChangeFileExt(DEMFileName,'.LBL');
            if FileExists(OfName) then begin
               DEMFileName := ofName;
               Ext := '.lbl';
            end;
         end;

         if ExtEquals(Ext, '.FRQ') then begin
            ofName := ChangeFileExt(DEMFileName,'.TIF');
            if FileExists(OfName) then begin
               DEMFileName := ofName;
               Ext := '.tif';
            end;
         end;

         if ExtEquals(Ext, '.HDR') then  begin
            if FileExists(ChangeFileExt(DEMFileName,'.flt')) then begin
               DEMFileName := ChangeFileExt(DEMFileName,'.flt');
               Ext := '.flt';
            end;

            if FileExists(ChangeFileExt(DEMFileName,'.bil')) then  begin
               DEMFileName := ChangeFileExt(DEMFileName,'.bil');
               Ext := '.bil';
            end;
         end
         else if ExtEquals(Ext, '.PRU') then ReadNOAAEEZfile(DEMFileName)
         else if ExtEquals(Ext, '.G03') or ExtEquals(Ext, '.G98') then ReadNGDCGRD98file(DEMFileName,Error)
         {$IfDef ExcludeUKOSDEM} {$Else} else if ExtEquals(Ext, '.NTF') then Error := ReadOSfile(DEMFileName) {$EndIf}
         else if (UpperCase(ExtractFileNameNoExt(DEMFileName)) = 'DEMFLOAT') or ExtEquals(Ext, '.FLT') or ExtEquals(Ext, '.BIL') then begin
            ReadNEDBIL(DEMFileName,Error);
         end
         else if ExtEquals(Ext, '.ASC') then begin
            Error := not TryGDAL(DEMFileName);
         end
         else if ExtEquals(Ext, '.GRD') or ExtEquals(Ext, '.TXT') then begin
            ReadASCIIArcGrid(DEMFileName,Self,Error,ReallyReadDEM);
         end
         else if ExtEquals(Ext, '.DTM') then begin
            ReadFusionDTM(DEMFileName,Self,Error);
         end;

         if Error and (not GDALGridFormat(Ext)) then begin
            {$IfDef ExDTED}{$Else} ReadDMADTED(Error,DEMFileName); {$EndIf}
            Error := not TryGDAL(DEMFileName);
         end;
      end;



function GetNIMADTEDHeader(var InputFile : file) : tStringList;
var
   TStr : String[35];
   UHL  : tuhl;
   DSI  : tDSI;
   ACC  : tACC;
   i : integer;
begin
   Result := tStringList.Create;

   Seek(InputFile,0);
   BlockRead(InputFile,UHL,80);
   BlockRead(InputFile,DSI,648);
   BlockRead(InputFile,ACC,2700);

   Move(UHL[21],TStr[1],4);   TStr[0] := #4;   Result.Add('Long spacing: ' + TStr);
   Move(UHL[25],TStr[1],4);   TStr[0] := #4;   Result.Add('Lat spacing: ' + TStr);
   Move(UHL[29],TStr[1],4);   TStr[0] := #4;   Result.Add('Vert accuracy: ' + TStr);
   Move(UHL[33],TStr[1],3);   TStr[0] := #3;   Result.Add('UHL Security: ' + TStr);
   Move(UHL[36],TStr[1],12);  TStr[0] := #12;  Result.Add('Unique reference: ' + TStr);
   Move(UHL[48],TStr[1],4);   TStr[0] := #4;   Result.Add('Num long lines: ' + TStr);
   Move(UHL[52],TStr[1],4);   TStr[0] := #4;   Result.Add('Num lat points: ' + TStr);
   Move(UHL[56],TStr[1],1);   TStr[0] := #1;   Result.Add('Multiple accuracy: ' + TStr);
   Move(DSI[4],TStr[1],1);    TStr[0] := #1;   Result.Add('DSI Security: ' + Tstr);
   Move(DSI[5],TStr[1],2);    TStr[0] := #2;   Result.Add('Control and release: ' + Tstr);
   Move(DSI[7],TStr[1],27);   TStr[0] := #27;  Result.Add('Handling: ' + Tstr);
   Move(DSI[60],TStr[1],5);   TStr[0] := #5;   Result.Add('Series: ' + Tstr);
   Move(DSI[65],TStr[1],15);  TStr[0] := #15;  Result.Add('Unique ref: ' + Tstr);
   Move(DSI[88],TStr[1],2);   TStr[0] := #2;   Result.Add('Data edition: ' + Tstr);
   Move(DSI[90],TStr[1],1);   TStr[0] := #1;   Result.Add('Match/merge: ' + Tstr);
   Move(DSI[91],TStr[1],4);   TStr[0] := #4;   Result.Add('Maintenance date: ' + Tstr);
   Move(DSI[95],TStr[1],4);   TStr[0] := #4;   Result.Add('Merge date: ' + Tstr);
   Move(DSI[99],TStr[1],4);   TStr[0] := #4;   Result.Add('Maint code: ' + Tstr);
   Move(DSI[103],TStr[1],8);  TStr[0] := #8;   Result.Add('Producer code: ' + Tstr);
   Move(DSI[127],TStr[1],9);  TStr[0] := #9;   Result.Add('Product spec: ' + Tstr);
   Move(DSI[136],TStr[1],2);  TStr[0] := #2;   Result.Add('Spec amend: ' + Tstr);
   Move(DSI[138],TStr[1],4);  TStr[0] := #4;   Result.Add('Spec date: ' + Tstr);
   Move(DSI[142],TStr[1],3);  TStr[0] := #3;   Result.Add('Vert datum: ' + Tstr);
   Move(DSI[145],TStr[1],5);  TStr[0] := #5;   Result.Add('Horiz datum: ' + Tstr);
   Move(DSI[150],TStr[1],10); TStr[0] := #10;  Result.Add('Digit system: ' + Tstr);
   Move(DSI[160],TStr[1],4);  TStr[0] := #4;   Result.Add('Compilation date: ' + Tstr);
   Move(DSI[205],TStr[1],7);  TStr[0] := #7;   Result.Add('Lat SW: ' + Tstr);
   Move(DSI[212],TStr[1],8);  TStr[0] := #8;   Result.Add('Long SW: ' + Tstr);
   Move(DSI[220],TStr[1],7);  TStr[0] := #7;   Result.Add('Lat NW: ' + Tstr);
   Move(DSI[227],TStr[1],8);  TStr[0] := #8;   Result.Add('Long NW: ' + Tstr);
   Move(DSI[235],TStr[1],7);  TStr[0] := #7;   Result.Add('Lat NE: ' + Tstr);
   Move(DSI[242],TStr[1],8);  TStr[0] := #8;   Result.Add('Long NE: ' + Tstr);
   Move(DSI[250],TStr[1],7);  TStr[0] := #7;   Result.Add('Lat SE: ' + Tstr);
   Move(DSI[257],TStr[1],8);  TStr[0] := #8;   Result.Add('Long SE: ' + Tstr);
   Move(DSI[290],TStr[1],2);  TStr[0] := #2;   Result.Add('Partial indicator: ' + TStr);
   TStr := '';  for i := 292 to 392 do TStr := TStr + DSI[i];   Result.Add('DMA use: ' + TStr);
   TStr := '';  for i := 393 to 492 do TStr := TStr + DSI[393]; Result.Add('Producing nation: ' + TStr);
   Move(acc[4],TStr[1],4);  TStr[0] := #4;  Result.Add('Absolute horiz acc (m): ' + TStr);
   Move(acc[8],TStr[1],4);  TStr[0] := #4;  Result.Add('Absolute vert  acc (m): ' + TStr);
   Move(acc[12],TStr[1],4); TStr[0] := #4;  Result.Add('Relative horiz acc (m): ' + TStr);
   Move(acc[16],TStr[1],4); TStr[0] := #4;  Result.Add('Relative vert  acc (m): ' + TStr);
   Result.Add('');
   Result.Add('');
   ApplicationProcessMessages;
end;


   procedure ReadDMADTED(var Error : boolean; InputFileName : PathStr);
   { takes as input DMA binary cell, and creates full cell}
   {DTED format has 3428 byte header, then each column has 12 additional bytes besides the data (2 bytes per elevation); file should have no additional data}
   label
      Restart;
   type
      BigElevColType = array[0..9999] of SmallInt;    //increased for DTED3
      DTEDColType    = array[1..10006] of SmallInt;   //increased for DTED3
   var
      InputFile    : file;
      BigZ         : ^BigElevColType;
      EndRow,EndCol,InNumRow,
      StartRow,StartCol : integer;
      DataColumn    : ^DtedCOLType;
      DTEDFileHeader : array[1..3248] of byte;
      NewCol,i,j,Err,Min,Sec,interval,NumRead : integer;
      Lat,Long : float64;
      TStr,TStr2     : string[255];
      SecondRun : boolean;


      procedure ReadColumn;
      var
         i,j,NumRead  : integer;
      begin
         with DEMheader do begin
            BlockRead(InputFile,DataColumn^,2*InNumRow + 12,NumRead);
            for i := StartRow to EndRow do begin
               j := i - StartRow;
               if (Lo(DataColumn^[i+5]) = 255) and (Hi(DataColumn^[i+5]) = 255) then begin
                  BigZ^[j] := MaxSmallInt;
               end
               else begin
                  {$R-} BigZ^[j] := Swap(DataColumn^[i+5]);

                  if BigZ^[j] < 0 then begin
                     BigZ^[j] := -(BigZ^[j] and $7fff);
                     if BigZ^[j] < -2500 then BigZ^[j] := Swap(DataColumn^[i+5]);   //GIS DataDepot bad implementation of DTED specs
                  end;

                  {$R+}
                  if abs(BigZ^[j]) = 9999 then BigZ^[j] := MaxSmallInt;
               end;
            end {for i};
         end {with};
      end;

      procedure DTEDHeaderCalculations;
      begin
         DEMheader.DEMUsed := ArcSecondDEM;
         DEMheader.UTMZone := GetUTMZone(DEMSWcornerLong+0.05);
         LatSizeMap := 1.0 * pred(DEMheader.NumRow) * DEMheader.DEMySpacing;
         LongSizeMap := 1.0 * pred(DEMheader.NumCol) * DEMheader.DEMxSpacing;
      end;


   begin {proc ReadDMADTED}
      {$IfDef RecordDTED} WriteLineToDebugFile('Read DTED, ' + InputFileName);  {$EndIf}
      if not FileExists(InputFileName) then begin
         Error := true;
         exit;
      end;

      Error := false;
      SecondRun := false;
     Restart:;
      with DEMheader do begin
         AreaName := ExtractFileName(InputFileName);
         DataSpacing  := SpaceDegrees;
         MinElev := MaxSmallInt;
         MaxElev := -MaxSmallInt;
         FileMode := 0;
         assignFile(InputFile,InputFileName);
         reset(InputFile,1);

         Seek(InputFile,0);
         BlockRead(InputFile,DTEDFileHeader,3248,NumRead);
         if (NumRead < 3248) then begin
            Error := true;
            CloseFile(InputFile);
            exit;
         end;

         Move(DTEDFileHeader[1],TStr[1],4);
         TStr[0] := #4;
         if TStr <> 'UHL1' then begin
            Error := true;
            CloseFile(InputFile);
            exit;
         end;

         {$IfDef RecordDTEDHeader}
            if (not SecondRun) and (not DEMMergeInProgress) then begin
               GetNIMADTEDHeader(InputFile,DTEDHeader);
               WriteStringListToDebugFile(DTEDHeader);
               DTEDHeader.Free;
            end;
         {$EndIf}

         Seek(InputFile,20);
         BlockRead(InputFile,TStr[1],8);
         TStr[0] := #8;
         Val(ptCopy(TStr,1,4),Interval,err);

         DEMxSpacing := 0.1 * Interval / 3600;
         Val(ptCopy(TStr,5,4),Interval,err);
         DEMySpacing  := 0.1 * Interval / 3600;

         Seek(InputFile,0);
         TStr[0] := #20;
         BlockRead(InputFile,TStr[1],20);
         seek(InputFile,80);
         TStr2[0] :=  #3;
         if Length(AreaName) < 8 then begin
            AreaName := Tstr[20] + ptCopy(TStr,14,2) + TStr[12] + ptCopy(TStr,5,3);
            AreaName := ptTrim(UpperCase(AreaName));
         end;
         BlockRead(InputFile,TStr2,3);
         if (ptCopy(TStr,1,4) <> 'UHL1') and (TStr2 <> 'DSI') then begin
            {this is not a DTED data file}
            error := true;
            CloseFile(InputFile);
            exit;
         end {if};
         new(BigZ);
         new(DataColumn);

         if not SecondRun then begin
            Val(ptCopy(TStr,5,3),Long,Err);
            Val(ptCopy(TStr,8,2),Min,Err);
            Val(ptCopy(TStr,10,2),Sec,Err);
            Long := Long + Min / 60  + Sec / 3600;
            if TStr[12] = 'W' then Long := -Long;
            DEMSWcornerLong := Long;
            Val(ptCopy(TStr,14,2),Lat,Err);
            Val(ptCopy(TStr,16,2),Min,Err);
            Val(ptCopy(TStr,18,2),Sec,Err);
            Lat := Lat + Min / 60 + Sec / 3600;
            if TStr[20] = 'S' then Lat := -Lat;
            DEMSWcornerLat := Lat;
            DEMSWCornerX := DEMSWcornerLong;
            DEMSWCornerY := DEMSWcornerLat;
            {$IfDef RecordDTED} WriteLineToDebugFile('Compute DTED '+ TStr + '   SW: ' + LatLongDegreeToString(DEMSWcornerLat,DEMSWcornerLong,DecSeconds)); {$EndIf}
         end;

         Seek(InputFile,47);
         BlockRead(InputFile,TStr[1],8);
         if not SecondRun then begin
            TStr[0] := #8;
            Val(ptCopy(TStr,1,4),NumCol,err);
            Val(ptCopy(TStr,5,4),NumRow,err);
         end;
         Seek(InputFile,3428);

         if not SecondRun then begin
            StartCol := 0;
            StartRow := 0;
            EndCol := pred(NumCol);
            EndRow := pred(NumRow);
            InNumRow := NumRow;
         end;

         if ReallyReadDEM then begin
            if AllocateDEMMemory(true) then begin
               Seek(InputFile,3428 + StartCol*(2*InNumRow + 12));
               if ShowDEMReadingProgress then StartProgress('Read DTED: ' + AreaName);
               i := StartCol;
               NewCol := 0;
               while i <= EndCol do begin
                  if ShowDEMReadingProgress then UpdateProgressBar((i-StartCol) / (EndCol-StartCol));
                  Seek(InputFile,3428 + i*(2*InNumRow + 12));
                  ReadColumn;
                  for j := 0 to pred(NumRow) do SetGridElevation(NewCol,j,BigZ^[j]);
                  inc(i);
                  inc(NewCol);
               end {while};
               EndProgress;
            end
            else Error := true;
            CheckMaxMinElev;
            {$IfDef RecordDTED} WriteLineToDebugFile('DTED, Rows:' + IntToStr(StartRow) + ' to ' +  IntToSt(EndRow) + ' Cols:' + IntToStr(StartCol) + ' to ' + IntToStr(EndCol) + '  Spacing: ' + HorizontalDEMSpacing); {$EndIf}
         end;
         close(InputFile);
         DigitizeDatum := WGS84d;
         DTEDHeaderCalculations;
      end {with};

      Dispose(BigZ);
      Dispose(DataColumn);
      {$IfDef RecordDTED} WriteLineToDebugFile('Final Read DTED Spacing: ' + HorizontalDEMSpacing); {$EndIf}
   end {proc ReadDMADTED};





