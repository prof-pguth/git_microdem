{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{-----------------------------------}
{ include file for demcoord         }
{___________________________________}


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
         StringToByteArray('WGS84',DEMheader.DMAMapDefinition.h_DatumCode);
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
         StringToByteArray('WGS84', DEMheader.DMAMapDefinition.h_DatumCode);
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
   //TStr: ShortString;
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

begin
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
               if TStr = 'DECIMETER' then ElevUnits := DeciMeters;
               if TStr = 'CENTIMETER' then ElevUnits := CentiMeters;
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
end;


{$IfDef ExcludeUKOSDEM}
{$Else}
      function ReadOSfile(DEMFileName : PathStr) : boolean;
      begin
         {$IfDef RecordReadDEM} WriteLineToDebugFile('ReadOSfile ' + DEMFileName); {$EndIf}
         DEMheader.DataSpacing := SpaceMeters;
         DEMheader.DEMUsed := UTMBasedDEM;
         DEMheader.DigitizeDatum := UK_OS_grid;
         DEMMapProjection := CreateUKOSprojection;
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



