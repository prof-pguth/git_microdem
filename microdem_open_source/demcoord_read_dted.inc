{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2023 Peter L. Guth  }
{___________________________________}


{include file for demcoord.pas}


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


