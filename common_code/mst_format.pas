unit mst_format;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of ianMICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2022 Peter L. Guth   }
{____________________________________}



{$I nevadia_defines.inc}

{$IfDef RecordProblems}  //normally only defined for debugging specific problems
  //{$Define RecordMST}
  //{$Define RecordFullMST}
{$EndIf}

interface

uses
   Petmar_types,DEMMapf;


procedure ImportMSTSidescan(fName : PathStr = '');
procedure View81Ssidescanfile(MapForm : tMapForm);
procedure Subset81Ssidescanfile;


implementation

uses
   Graphics,SysUtils,Classes,
   DEMDefs,BaseMap,
   Petimage_form,
   PetDBUtils,Petmar_db,
   Petmar,PetImage,SideImg;

type
   tImageFileHeader = record
      MSTiffID : array[1..4] of ANSIchar;
      OffsetToIFD : int32;
   end;

   tIFD = record
      Tag,DataType : int16;
      Count,ValueOffset : int32;
   end;

   tMSTInfo = record  //this is PLG record structure, read from IFD
      SonarDataInfo3Size,SonarDataInfo3Offset,SonarLines,
      ScrollDirection,RightChannelSize,RightChannelOffset,
      LeftChannelSize,LeftChannelOffset,
      FathometerSize,FathometerOffset,
      Nav6Count,Nav6Offset,
      NavInterpolationTimeout,NavInfoCount,BinsPerChannel : int32;
      MinLat,MaxLat,
      MinLong,MaxLong : float64;
      StartTime,EndTime : Cardinal;
   end;

   tNavRec6 =  record
      SysTime : Cardinal;
      Lat,Long,
      SOGKnots,
      COG,
      TD1,TD2,Heading,
      PortStartLat,PortStartLong,
      PortEndLat,PortEndLong,
      StbdStartLat,StbdStartLong,
      StbdEndLat,StbdEndLong : float32;
      SonarActive : boolean;
      LaybackX,LaybackY : float32;
      LaybackEnabled : boolean;
   end;

   tSonarDataInfo3 = record
      SysTime : Cardinal;
      Range,Frequency,RangeDelay,Altitude : Int16;
      Gains : array[1..16] of Int16;
   end;
   tFathometerRec2 = record
      SysTime : Cardinal;
      WaterDepth,
      Altitide : float64;
   end;

var
   MSTInfo : tMSTInfo;


procedure ImportMSTSidescan;
var
   outname : PathStr;
   inf : file;
   i,x,y : int32;
   EntryCount : Int16;
   Channel : array[0..2047] of byte;
   IFD : tIFD;
   p0 : pRGB;
   DefaultFilter : byte;
   Bitmap : tMyBitmap;
   HaveFile : boolean;
   ImageFileHeader  : tImageFileHeader;
   NavRec6 : tNavRec6;
   SonarDataInfo3 : tSonarDataInfo3;
   FathometerRec2 : tFathometerRec2;
   SidescanMap : integer;
   MSTresults : tStringList;
   FilesWanted : TStringList;


   procedure OpenFile;
   var
      i : integer;
   begin
      {$IfDef RecordMST} WriteLineToDebugFile(fName); {$EndIf}
      assignFile(inf,FName);
      reset(inf,1);
      BlockRead(inf,ImageFileHeader,8);
      if (ImageFileHeader.MSTiffID[1] = 'M') and (ImageFileHeader.MSTiffID[2] = 'S') and (ImageFileHeader.MSTiffID[3] = 'T') and (ImageFileHeader.MSTiffID[4] = 'L') then with MSTInfo do begin
         FathometerSize := 0;
         seek(inf,ImageFileHeader.OffsetToIFD);
         BlockRead(inf,EntryCount,2);
         {$IfDef RecordMST} WriteLineToDebugFile('MST entry count=' + IntToStr(EntryCount));  {$EndIf}
         for i := 1 to EntryCount do begin
            BlockRead(inf,IFD,12);
            {$IfDef RecordMST} WriteLineToDebugFile(IntegerToString(IFD.Tag,8) + IntegerToString(IFD.DataType,8) + IntegerToString(IFD.Count,8) + IntegerToString(IFD.ValueOffset,8)); {$EndIf}
            case IFD.Tag of
                 0 : begin end; //end of structure
               254 : if (IFD.ValueOffset <> 1) then MessageToContinue('Unsupported compression ' + IntegerToString(IFD.ValueOffset));
               255 : begin end; //CondensedImage

               259 : SonarLines := IFD.ValueOffset;
               260 : BinsPerChannel := IFD.ValueOffset;
               261 : ScrollDirection := IFD.ValueOffset;
               262 : begin end;  //TimeCorrelation
               266 : NavInfoCount := IFD.ValueOffset;
               268 : begin end;  //Undocumented, but zero
               271 : begin end;  //SurveyPlotterImage
               280 : begin end;  //Annotation3Count
               285 : begin end;  //Y2KTimeCorrelation
               286 : begin end;  //FathometerCount
               288 : begin end;  //MagnetometerCount
               289 : begin end;  //Magnetometer
               294 : begin end;  //Outdated, Marker4Count
               295 : begin end;  //3.30 Marker 4
               296 : begin  //Fathometer2
                         FathometerSize := IFD.Count;
                         FathometerOffset := IFD.ValueOffset;
                     end;
               298 : begin   //range, freq, etc.
                        SonarDataInfo3Size := IFD.Count;
                        SonarDataInfo3Offset := IFD.ValueOffset;
                     end;
               299 : begin
                         LeftChannelSize := IFD.Count;
                         LeftChannelOffset := IFD.ValueOffset;
                     end;
               300 : begin
                         RightChannelSize := IFD.Count;
                         RightChannelOffset := IFD.ValueOffset;
                     end;
               301 : begin end;  //CreatorVersion
               303 : begin end;  //MagnetometerParams2
               304 : NavInterpolationTimeout := IFD.ValueOffset;
               305 : begin end;  //SurveyPlotterParams4
               306 : begin end;  //Marker5Count
               307 : begin end;  //Marker5
               308 : begin //NavInfo6
                        Nav6Count := IFD.Count;
                        Nav6Offset := IFD.ValueOffset;
                     end;
               311 : begin end;   //TVG type
               else  WriteLineToDebugFile('Undefined:' + IntegerToString(IFD.Tag,8) + IntegerToString(IFD.DataType,8) +
                  IntegerToString(IFD.Count,8) + IntegerToString(IFD.ValueOffset,8));
            end;
         end;

         {$IfDef RecordMST} WriteLineToDebugFile('SonarDataInfo3Offset=' + IntToStr(SonarDataInfo3Offset));  {$EndIf}
         Seek(inf,SonarDataInfo3Offset);
         for i := 1 to SonarDataInfo3Size do begin
            BlockRead(inf,SonarDataInfo3,sizeOf(tSonarDataInfo3));
            if (i = 1) then MSTInfo.StartTime := SonarDataInfo3.SysTime;
            if (i = SonarDataInfo3Size) then MSTInfo.EndTime := SonarDataInfo3.SysTime;
         end;

         if (FathometerSize > 0) then        begin
            {$IfDef RecordMST} WriteLineToDebugFile('FathometerOffset=' + IntToStr(FathometerOffset)); {$EndIf}
            Seek(inf,FathometerOffset);
            for i := 1 to FathometerSize do          begin
               BlockRead(inf,FathometerRec2,sizeOf(FathometerRec2));
               {$IfDef RecordMST} if (i < 10) or (i > FathometerSize -10) then WriteLineToDebugFile(IntToStr(i) + '==' + IntToStr(FathometerRec2.SysTime)); {$EndIf}
            end;
         end;

         {$IfDef RecordMST} WriteLineToDebugFile('Nav6Offset=' + IntToStr(Nav6Offset),true); {$EndIf}
         Seek(inf,Nav6Offset);
         for i := 1 to Nav6Count do       begin
            BlockRead(inf,NavRec6,sizeOf(tNavRec6));
            {$IfDef RecordMST} if (i < 10) or (i > Nav6Count -10) then WriteLineToDebugFile(IntToStr(i) + '==' + IntToStr(NavRec6.SysTime)); {$EndIf}

               MSTresults.Add(RealToString(NavRec6.Lat/60,11,-8) + ',' + RealToString(NavRec6.long/60,12,-8)  + ',' + RealToString(NavRec6.COG,8,-2)  + ','
                         + RealToString(NavRec6.SOGKnots,8,-2)  + ',' + IntToStr(NavRec6.SysTime)+ ','  + ExtractFileNameNoExt(FName)+
                          ',' + IntToStr(i)  + ',' +  RealToString(NavRec6.PortEndLat/60,11,-8) + ',' + RealToString(NavRec6.PortEndlong/60,12,-8)
                          + ',' +  RealToString(NavRec6.StbdEndLat/60,11,-8) + ',' + RealToString(NavRec6.StbdEndlong/60,12,-8) );

               if NavRec6.PortEndLat/60 > MSTInfo.MaxLat then MSTInfo.MaxLat := NavRec6.PortEndLat/60;
               if NavRec6.StbdEndLat/60 > MSTInfo.MaxLat then MSTInfo.MaxLat := NavRec6.StbdEndLat/60;

               if NavRec6.PortEndLat/60 < MSTInfo.MinLat then MSTInfo.MinLat := NavRec6.PortEndLat/60;
               if NavRec6.StbdEndLat/60 < MSTInfo.MinLat then MSTInfo.MinLat := NavRec6.StbdEndLat/60;

               if NavRec6.PortEndLong/60 > MSTInfo.MaxLong then MSTInfo.MaxLong := NavRec6.PortEndLong/60;
               if NavRec6.StbdEndLong/60 > MSTInfo.MaxLong then MSTInfo.MaxLong := NavRec6.StbdEndLong/60;

               if NavRec6.PortEndLong/60 < MSTInfo.MinLong then MSTInfo.MinLong := NavRec6.PortEndLong/60;
               if NavRec6.StbdEndLong/60 < MSTInfo.MinLong then MSTInfo.MinLong := NavRec6.StbdEndLong/60;
         end {for i};
      end
      else MessageToContinue('Not MST file');
   end {proC};


      procedure SaveCSVfile;
      var
        on2 : PathStr;
      begin
         on2 := ChangeFileExt(OutName,DefaultDBExt);
         if FileExists(on2) then begin
            OutName := on2;
         end
         else begin
            MSTResults.SaveToFile(Outname);
         end;
         MSTResults.Free;

         SidescanMap := SetUpVectorMap(true,true, Undefinedproj, VectorMapName);
         VectorMap[SideScanMap].MapDraw.MaximizeLatLongMapCoverage(MSTInfo.MinLat-0.0005,MSTInfo.MinLong-0.0005,MSTInfo.MaxLat+0.0005,MSTInfo.MaxLong+0.0005);
         VectorMap[SideScanMap].DoCompleteMapRedraw;
         VectorMap[SideScanMap].LoadDataBaseFile(Outname);
         VectorMap[SideScanMap].Caption := ExtractFileName('MST: ' + OutName);
      end;


begin
   StopSplashing;
   HaveFile := FileExists(fName);
   FilesWanted := tStringList.Create;
   if fName = '' then begin
      fName := InputSideScanFileName;
      FilesWanted.Add(InputSideScanFileName);
   end
   else FilesWanted.Add(fName);

   DefaultFilter := 0;
   if HaveFile or GetMultipleFiles('MST file','MST|*.mst', FilesWanted,DefaultFilter) then begin
      fName := FilesWanted.Strings[0];
      SideImg.SetSideScanColorTable;

      MSTresults := tStringList.Create;
      MSTResultS.Add('LAT,LONG,COG,SOG_KNOTS,SYS_TIME,MST_FILE,PING,PORT_LAT,PORT_LONG,STAR_LAT,STAR_LONG');

      MSTInfo.MaxLat := -500;
      MSTInfo.MinLat := 500;
      MSTInfo.MaxLong := -500;
      MSTInfo.MinLong := 500;

      if (FilesWanted.Count > 1) then begin
         OutName := ExtractFilePath(fName) + 'mst_track.csv';
         if FileExists(OutName) then begin
         end
         else begin
            FilesWanted.Sort;
            StartProgress('MST');
            for i := 0 to pred(FilesWanted.Count) do begin
               UpDateProgressBar(i/FilesWanted.Count);
               fName := FilesWanted.Strings[i];
               OpenFile;
               CloseFile(inf);
            end;
            EndProgress;
            OutName := ExtractFilePath(fName) + 'mst_track.csv';
            SaveCSVfile;
         end;
      end
      else begin
         InputSideScanFileName := FilesWanted.Strings[0];
         fName := InputSideScanFileName;
         OutName := ChangeFileExt(fName,'.csv');
         OpenFile;
         with MSTInfo do begin
            {$IfDef RecordMST} WriteLineToDebugFile('Min values: ' + LatLongDegreeToString(MSTInfo.MinLat,MSTInfo.MinLong)+ ' Max values: ' + LatLongDegreeToString(MSTInfo.MaxLat,MSTInfo.MaxLong)); {$EndIf}

            SaveCSVfile;
            Seek(inf,Nav6Offset);
            for i := 1 to Nav6Count do begin
               BlockRead(inf,NavRec6,sizeOf(tNavRec6));
               {$IfDef RecordMST}
                  if (i < 10) or (i > Nav6Count -10) then begin
                      WriteLineToDebugFile( IntToStr(i) + '  ' + 'Center: ' + LatLongDegreeToString(NavRec6.Lat/60,NavRec6.long/60,DecDegrees),true);
                      WriteLineToDebugFile('Port: ' + LatLongDegreeToString(NavRec6.PortEndLat/60,NavRec6.PortEndlong/60,DecDegrees));
                      WriteLineToDebugFile('Stbd: ' + LatLongDegreeToString(NavRec6.StbdEndLat/60,NavRec6.StbdEndlong/60,DecDegrees));
                  end;
               {$EndIf}
               VectorMap[SideScanMap].MapDraw.MapSymbolAtLatLongDegree(VectorMap[SideScanMap].Image1.Canvas,NavRec6.Lat/60,NavRec6.long/60,FilledBox,2,claRed);
               VectorMap[SideScanMap].MapDraw.MapSymbolAtLatLongDegree(VectorMap[SideScanMap].Image1.Canvas,NavRec6.PortEndLat/60,NavRec6.PortEndlong/60,FilledBox,2,claLime);
               VectorMap[SideScanMap].MapDraw.MapSymbolAtLatLongDegree(VectorMap[SideScanMap].Image1.Canvas,NavRec6.StbdEndLat/60,NavRec6.StbdEndlong/60,FilledBox,2,claBlue);
            end {for i};

            CreateBitmap(Bitmap,2 * BinsPerChannel,SonarLines);

            StartProgress('Sonar');
            Seek(inf,LeftChannelOffset);
            for y := 0 to pred(SonarLines) do begin
               if ScrollDirection = 1 then p0 := Bitmap.Scanline[pred(SonarLines)-y]
               else p0 := Bitmap.Scanline[y];
               BlockRead(inf,Channel,BinsPerChannel);
               UpdateProgressBar(y/2/SonarLines);
               for x := 0 to pred(BinsPerChannel) do begin
                  p0[BinsPerChannel-x] := SideScanColorTable[Channel[x]];
               end {for x};
            end {for y};

            Seek(inf,RightChannelOffset);
            for y := 0 to pred(SonarLines) do begin
               UpdateProgressBar((SonarLines+y)/2/SonarLines);
               if ScrollDirection = 1 then p0 := Bitmap.Scanline[pred(SonarLines)-y]
               else p0 := Bitmap.Scanline[y];
               BlockRead(inf,Channel,BinsPerChannel);
               for x := 0 to pred(BinsPerChannel) do begin
                  p0[BinsPerChannel+x] := SideScanColorTable[Channel[x]];
               end {for x};
            end {for y};
            EndProgress;
            PetImage_form.DisplayBitmap(Bitmap,fName);
            Bitmap.Free;
         end;
         CloseFile(inf);
      end;
   end;
end {proc};

type
   t81sFileHeader = packed record
      Recognition : array[1..3] of ANSIchar;
      nToReadIndex : byte;                          //3 means a 400 byte V6 SportScan
      TotalBytes,
      nToRead  : word;
      Date : array[1..12] of ANSIchar;
      Time : array[1..9] of ANSIchar;
      Reserved : array[29..36] of byte;
      Reserved16 : byte;
      SidescanChannel : byte;                     //1=Stbd, 2=Port, 3=Both
      Gain : byte;                                //0 ti 40 in 1 dB increments
      Reserved40_46 : array[40..46] of byte;
      LatString : array[1..12] of ANSIChar;
      LongString : array[1..12] of ANSIChar;
      RepetitionRate : word;                      //time between pings, ms
      GPSspeed : byte;                            //0.1 knots
      GPSheading : word;                          //0.1 degree
      OperatingFrequency : byte;                  //0=Low, 1=High
      ChannelBalance : byte;
      Win881ssRecordVersion : byte;
      LastReserved : array[79..99] of byte;
   end;
   t81sPingRecord = packed record
      VersionRecognition : array[100..102] of ANSIchar;
      HeadID : byte;
      SerialStatus : byte;
      SportScanType : byte;                      //single or dual frequency
      SidescanChannel : byte;                    //1=Stbd, 2=Port, 3=Both
      Range : byte;                              //15, 30, 60, 90, or 120 m
      Reserved108_109 : array[1..2] of byte;
      DataBytes : word;
      SonarEchoReturn : array[112..511] of byte;   //either 200 or 400 bytes per channel
      TerminationByte : byte;
   end;
   tEndRec = packed record
      ZeroFill : array[513..637] of byte;
      PointerPrevious : word;
   end;

procedure View81Ssidescanfile(MapForm : tMapForm);
var
   ssf : file;
   Header : t81sFileHeader;
   Ping : t81sPingRecord;
   fName : PathStr;
   EndRec : tEndRec;
   DoVert,DoHoriz : boolean;
   NRecs : integer;
   bmp : tMyBitmap;
   x,y: Integer;
   SidescanRunlength,
   Lat1,Long1,Lat,Long,LineHeading : float64;
   Results : tStringList;
begin
    SetSideScanColorTable;
    fName := InputSideScanFileName;
    while Petmar.GetFileFromDirectory('sidescan image','*.81s',fName) do begin
        InputSideScanFileName := fName;
        NRecs := GetFileSize(fName) div 640;
        {$IfDef RecordMST} WriteLineToDebugFile(fName + '   recs=' + IntToStr(NRecs));  {$EndIf}
        CreateBitmap(bmp,400,Nrecs);
        if not FileExists(ChangeFileExt(fname,DefaultDBExt)) then begin
           Results := tStringList.Create;
           Results.Add('TIME,LAT,LONG,SPEED,HEADING,SWATH,LAYBACK,FREQ,CHANNEL,PING');
        end
        else Results := Nil;

        ShowHourglassCursor;
        y := 0;

        assignFile(ssf,fName);
        reset(ssf,1);
        while not eof(ssf) do begin
           try
              BlockRead(ssf,Header,sizeOf(Header));
              BlockRead(ssf,Ping,sizeOf(Ping));
              BlockRead(ssf,EndRec,sizeOf(EndRec));

              if (Header.LatString[1] = #0) and (Header.LatString[2] = #0) then begin
                 Lat := -999;
                 Long := -999;
              end
              else begin
                 Lat := StrToFloat(Copy(Header.LatString,2,2)) + StrToFloat(Copy(Header.LatString,5,6)) / 60;
                 Long := StrToFloat(Copy(Header.LongString,1,3)) + StrToFloat(Copy(Header.LongString,5,6)) / 60;
              end;
              if (y=0) then begin
                 Lat1 := Lat;
                 Long1 := Long;
              end;
              if (Results <> Nil) then Results.Add(Copy(Header.Time,1,8) + ',' + RealToString(Lat,-14,-8) + ',' + RealToString(-Long,-15,-8) + ',' +
                     RealToString(0.1 * Header.GPSspeed,-8,1) + ',' + RealToString(0.01 * Header.GPSheading,-8,1) + ',' +
                     IntToStr(Ping.Range) + ',' + IntToStr(MDDef.SonarMapDef.SidescanLayback) + ',' + IntToStr(Header.OperatingFrequency) +
                     ',' + IntToStr(Header.SidescanChannel) + ',' + IntToStr(y));

              for x := 0 to 199 do begin
                 bmp.Canvas.Pixels[200+x,y] := ConvertPlatformColorToTColor(SideScanColorTable[2*Ping.SonarEchoReturn[112+2*x]]);
                 bmp.Canvas.Pixels[199-x,y] := ConvertPlatformColorToTColor(SideScanColorTable[2*Ping.SonarEchoReturn[113+2*x]]);
              end;
              inc(y);
           except
              x := 1;
           end;
        end;
        if (Results <> Nil) then begin
           fName := ChangeFileExt(fname,'.csv');
           StringList2CSVtoDB(Results,fName,true);
           //Results := Nil;
        end;

        closeFile(ssf);
        VincentyCalculateDistanceBearing(Lat1,Long1,Lat,Long,SidescanRunlength,LineHeading);
        DoHoriz := (LineHeading > 315) or (LineHeading < 45) or ( (LineHeading > 135) and (LineHeading < 225) );
        DoVert := not DoHoriz;

        {$IfDef RecordMST} WriteLineToDebugFile('heading=' + RealToString(LineHeading,-12,0) + '   length=' + RealToString(SidescanRunlength,-12,1) + ' m'); {$EndIf}

         PETImage.PlotOrientedLine(Bmp,50,50,45,360+LineHeading,claRed,2,true);
         PETImage.PlotOrientedLine(Bmp,BMP.Width-50,BMP.Height-50,45,360+LineHeading,claRed,2,true);

         if DoHoriz then begin
            if (LineHeading > 315) or (LineHeading < 45) then begin
            end
            else begin
               PetImage.Drehen180Grad(Bmp);
            end;
         end;

        if DoVert then begin
            if (LineHeading > 45) and (LineHeading < 135) then begin
               PetImage.Drehen270Grad(Bmp);
            end
            else begin
               PetImage.Drehen90Grad(Bmp);
            end;
        end;

        PetImage_form.DisplayBitmap(bmp,ExtractFileName(fName));
        bmp.Free;

        ShowDefaultCursor;
    end;
end;


procedure Subset81Ssidescanfile;
var
   ssf,outssf : file;
   Header : t81sFileHeader;
   Ping : t81sPingRecord;
   fName : PathStr;
   EndRec : tEndRec;
   NRecs : integer;
   x,y,YFirst,YLast : Int32;
begin
    fName := InputSideScanFileName;
    if Petmar.GetFileFromDirectory('sidescan image','*.81s',fName) then  begin
        InputSideScanFileName := fName;
        NRecs := GetFileSize(fName) div 640;
        YFirst := 0;
        YLast := pred(NRecs);
        assignFile(ssf,fName);
        repeat
           reset(ssf,1);
           fName := ExtractFileNameNoExt(fname) + '_sub.81s';
           Petmar.GetFileNameDefaultExt('Subset file','*.81s',fName);
           assignFile(outssf,fName);
           rewrite(outssf,1);
           Petmar.ReadDefault('First record',YFirst);
           Petmar.ReadDefault('Last record',YLast);
           ShowHourglassCursor;
           y := 0;
           while not eof(ssf) do begin
              try
                 BlockRead(ssf,Header,sizeOf(Header));
                 BlockRead(ssf,Ping,sizeOf(Ping));
                 BlockRead(ssf,EndRec,sizeOf(EndRec));
                 if (y >= YFirst) and (Y <= YLast) then begin
                    BlockWrite(outssf,Header,sizeOf(Header));
                    BlockWrite(outssf,Ping,sizeOf(Ping));
                    BlockWrite(outssf,EndRec,sizeOf(EndRec));
                 end;
                inc(y);
              except
              end;
           end;
           CloseFile(Outssf);
        until not AnswerIsYes('Another subset');
        closeFile(ssf);
        ShowDefaultCursor;
    end;
end;


initialization
finalization
   {$IfDef RecordMST} WriteLineToDebugFile('RecordMST active in mst_format'); {$EndIf}
end.
