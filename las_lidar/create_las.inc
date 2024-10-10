{ tCreateLasFile }



procedure FillHeaderField(var TheArray : array of ANSIChar; aLength : integer; Fill : shortString);
var
   i : integer;
begin
   for i := 0 to pred(aLength) do TheArray[i] := ' ';
   for i := 0 to pred(Length(Fill)) do TheArray[i] := Fill[succ(i)];
end;


procedure tCreateLasFile.ZeroHeaderRange;
begin
   NewLASHeader.MaxX := -99e39;
   NewLASHeader.MinX := 99e39;
   NewLASHeader.MaxY := -99e39;
   NewLASHeader.MinY := 99e39;
   NewLASHeader.MaxZ := -99e39;
   NewLASHeader.MinZ := 99e39;
end;


procedure tCreateLasFile.DefineHeader;
var
   i : integer;
   wMonth,wDay,wYear : word;
begin
   with NewLASHeader do  begin
      FillHeaderField(FileSignature,4,'LASF');
      ZeroHeaderRange;
      FillHeaderField(SystemID,32,'IMPORT/TRANSFORM');
      FillHeaderField(GenerateSoftware,32,'MICRODEM');
      DecodeDate(now,wYear,wmonth,wDay);
      CreateDay := AnnualJulianDay(wYear,wMonth,wDay);
      CreateYear := wYear;
      VersionMajor := #1;
      VersionMinor := #2;
      NumPtRecs := 0;
      for I := 1 to 5 do NumPtsByReturn[i] := 0;
      FileSourceID := 0;
      GlobalEncoding := 1;
      GUID_data1 := 0;
      GUID_data2 := 0;
      GUID_data3 := 0;
      for i:= 1 to 8 do GUID_data4[i] := ' ';
      HeaderSize := SizeOf(tLASHeader);
   end;
end;


procedure tCreateLasFile.FlushBuffer;
begin
   if (PointsInOutputBuffer > 0) then begin
      if NewPtType = 0 then BlockWrite(NewLasFile,OutputBuffer0^,PointsInOutputBuffer* NewLasHeader.PointDataRecLen);
      if NewPtType = 1 then BlockWrite(NewLasFile,OutputBuffer1^,PointsInOutputBuffer* NewLasHeader.PointDataRecLen);
      if NewPtType = 2 then BlockWrite(NewLasFile,OutputBuffer2^,PointsInOutputBuffer* NewLasHeader.PointDataRecLen);
      if NewPtType = 3 then BlockWrite(NewLasFile,OutputBuffer3^,PointsInOutputBuffer* NewLasHeader.PointDataRecLen);
      if NewPtType = 4 then BlockWrite(NewLasFile,OutputBuffer4^,PointsInOutputBuffer* NewLasHeader.PointDataRecLen);
      if NewPtType = 5 then BlockWrite(NewLasFile,OutputBuffer5^,PointsInOutputBuffer* NewLasHeader.PointDataRecLen);
      if NewPtType = 6 then BlockWrite(NewLasFile,OutputBuffer6^,PointsInOutputBuffer* NewLasHeader.PointDataRecLen);
      if NewPtType = 7 then BlockWrite(NewLasFile,OutputBuffer7^,PointsInOutputBuffer* NewLasHeader.PointDataRecLen);
      if NewPtType = 8 then BlockWrite(NewLasFile,OutputBuffer8^,PointsInOutputBuffer* NewLasHeader.PointDataRecLen);
      PointsInOutputBuffer := 0;
   end;
end;


procedure tCreateLasFile.ShotOverheadOutputBuffer(lp0: tLidarPointType0);
var
   Return : integer;
begin
   inc(NewLasHeader.NumPtRecs);
   Petmath.CompareValueToExtremes((NewLasHeader.XscaleFac * lp0.xPt) + NewLasHeader.XOffset,NewLasHeader.MinX,NewLasHeader.MaxX);
   Petmath.CompareValueToExtremes((NewLasHeader.YscaleFac * lp0.yPt) + NewLasHeader.YOffset,NewLasHeader.MinY,NewLasHeader.MaxY);
   Petmath.CompareValueToExtremes((NewLasHeader.ZscaleFac * lp0.zPt) + NewLasHeader.ZOffset,NewLasHeader.MinZ,NewLasHeader.MaxZ);
   Return := lp0.ReturnFlags mod 8;
   if Return in [1..5] then inc(NewLasHeader.NumPtsByReturn[Return]);
   if (PointsInOutputBuffer = MaxLASPtsToRead) then FlushBuffer;
end;


procedure tCreateLasFile.ShotOverheadOutputBuffer(lp6: tLidarPointType6);
var
   Return : integer;
begin
   inc(NewLasHeader.NumPtRecs);
   Petmath.CompareValueToExtremes((NewLasHeader.XscaleFac * lp6.xPt) + NewLasHeader.XOffset,NewLasHeader.MinX,NewLasHeader.MaxX);
   Petmath.CompareValueToExtremes((NewLasHeader.YscaleFac * lp6.yPt) + NewLasHeader.YOffset,NewLasHeader.MinY,NewLasHeader.MaxY);
   Petmath.CompareValueToExtremes((NewLasHeader.ZscaleFac * lp6.zPt) + NewLasHeader.ZOffset,NewLasHeader.MinZ,NewLasHeader.MaxZ);
   Return := lp6.ReturnFlags mod 16;
   if Return in [1..5] then inc(NewLasHeader.NumPtsByReturn[Return]);
   if (PointsInOutputBuffer = MaxLASPtsToRead) then FlushBuffer;
end;


procedure tCreateLasFile.AddShotToOutputBuffer(lf : tLAS_data; Pt : integer);
begin
   inc(PointsInOutputBuffer);
   if (lf.LidarPointType = 0) then begin
      OutputBuffer0^[PointsInOutputBuffer] := lf.LidarPoints0^[Pt];
      ShotOverheadOutputBuffer(lf.LidarPoints0^[Pt]);
   end
   else if (lf.LidarPointType = 1) then begin
      OutputBuffer1^[PointsInOutputBuffer] := lf.LidarPoints1^[Pt];
      ShotOverheadOutputBuffer(lf.LidarPoints1^[Pt].lp0);
   end
   else if (lf.LidarPointType = 2) then begin
      OutputBuffer2^[PointsInOutputBuffer] := lf.LidarPoints2^[Pt];
      ShotOverheadOutputBuffer(lf.LidarPoints2^[Pt].lp0);
   end
   else if (lf.LidarPointType = 3) then begin
      OutputBuffer3^[PointsInOutputBuffer] := lf.LidarPoints3^[Pt];
      ShotOverheadOutputBuffer(lf.LidarPoints3^[Pt].lp0);
   end
   else if (lf.LidarPointType = 4) then begin
      OutputBuffer4^[PointsInOutputBuffer] := lf.LidarPoints4^[Pt];
      ShotOverheadOutputBuffer(lf.LidarPoints0^[Pt]);
   end
   else if (lf.LidarPointType = 5) then begin
      OutputBuffer5^[PointsInOutputBuffer] := lf.LidarPoints5^[Pt];
      ShotOverheadOutputBuffer(lf.LidarPoints0^[Pt]);
   end
   else if (lf.LidarPointType = 6) then begin
      OutputBuffer6^[PointsInOutputBuffer] := lf.LidarPoints6^[Pt];
      ShotOverheadOutputBuffer(lf.LidarPoints6^[Pt]);
   end
   else if (lf.LidarPointType = 7) then begin
      OutputBuffer7^[PointsInOutputBuffer] := lf.LidarPoints7^[Pt];
      ShotOverheadOutputBuffer(lf.LidarPoints6^[Pt]);
   end
   else if (lf.LidarPointType = 8) then begin
      OutputBuffer8^[PointsInOutputBuffer] := lf.LidarPoints8^[Pt];
      ShotOverheadOutputBuffer(lf.LidarPoints6^[Pt]);
   end
   else begin
      dec(PointsInOutputBuffer);
   end;
end;

procedure tCreateLasFile.AddShotToOutputBuffer(lp0: tLidarPointType0);  //needed for vegetation density
begin
   inc(PointsInOutputBuffer);
   OutputBuffer0^[PointsInOutputBuffer] := lp0;
   ShotOverheadOutputBuffer(lp0);
end;


procedure tCreateLasFile.AddShotToOutputBuffer(lp1: tLidarPointType1);
begin
   inc(PointsInOutputBuffer);
   OutputBuffer1^[PointsInOutputBuffer] := lp1;
   ShotOverheadOutputBuffer(lp1.lp0);
end;


constructor tCreateLasFile.Create;
begin
   inherited;
   OutputBuffer0 := Nil;
   OutputBuffer1 := Nil;
   OutputBuffer2 := Nil;
   OutputBuffer3 := Nil;
   OutputBuffer4 := Nil;
   OutputBuffer5 := Nil;
   OutputBuffer6 := Nil;
   OutputBuffer7 := Nil;
   OutputBuffer8 := Nil;
end;


destructor tCreateLasFile.Destroy;
begin
   FlushBuffer;
   if (NewPtType = 0) then Dispose(OutPutBuffer0);
   if (NewPtType = 1) then Dispose(OutPutBuffer1);
   if (NewPtType = 2) then Dispose(OutPutBuffer2);
   if (NewPtType = 3) then Dispose(OutPutBuffer3);
   if (NewPtType = 4) then Dispose(OutPutBuffer4);
   if (NewPtType = 5) then Dispose(OutPutBuffer5);
   if (NewPtType = 6) then Dispose(OutPutBuffer6);
   if (NewPtType = 7) then Dispose(OutPutBuffer7);
   if (NewPtType = 8) then Dispose(OutPutBuffer8);
   seek(NewLasFile,0);
   BlockWrite(NewLasFile,NewLasHeader,SizeOf(NewLasHeader));
   {$IfDef RecordCreateLASfiles}
      WriteLineToDebugFile('tCreateLasFile.Destroy new LAS file ' +  NewLasFileName);
      WriteLineToDebugFile(AllRangeString(NewLasHeader));
   {$EndIf}
   CloseFile(NewLasFile);
   if (NewLasHeader.NumPtRecs = 0) then begin
      {$IfDef RecordCreateLASfiles} WriteLineToDebugFile('Delete, no points in file'); {$EndIf}
      SysUtils.DeleteFile(NewLasFileName);
   end;
end;


procedure tCreateLasFile.CreateNewLASfile(fName : PathStr; NewLasProjectionDefinition : tlasProjectionDefinition; CopyLASHeader : tLASHeader);
begin
    CreateNewLASfile(fName,NewlasProjectionDefinition,ord(CopyLASHeader.PointDataFormat));
    if NewLasProjectionDefinition.LasProj.ThisIsUTMfile then begin
       NewLASHeader.Xoffset := CopyLasHeader.XOffset;
       NewLASHeader.Yoffset := CopyLasHeader.YOffset;
      {$IfDef RecordCreateLASfiles} WriteLineToDebugFile('tCreateLasFile.CreateNewLASfile with copy ' + AllFactorsString(NewLasHeader)); {$EndIf}
    end;
end;


procedure tCreateLasFile.CreateNewLASfile(fName : PathStr; NewlasProjectionDefinition : tlasProjectionDefinition; PtType : byte);

      procedure InitializeOutputBuffer;
      begin
         if (NewPtType = 0) then New(OutPutBuffer0);
         if (NewPtType = 1) then New(OutPutBuffer1);
         if (NewPtType = 2) then New(OutPutBuffer2);
         if (NewPtType = 3) then New(OutPutBuffer3);
         if (NewPtType = 4) then New(OutPutBuffer4);
         if (NewPtType = 5) then New(OutPutBuffer5);
         if (NewPtType = 6) then New(OutPutBuffer6);
         if (NewPtType = 7) then New(OutPutBuffer7);
         if (NewPtType = 8) then New(OutPutBuffer8);
         PointsInOutputBuffer := 0;
         ZeroHeaderRange;
         {$IfDef RecordCreateLASfiles} WriteLineToDebugFile('tCreateLasFile.InitializeOutputBuffer ' + AllRangeString(NewLasHeader)); {$EndIf}
      end;


var
   VarLenRec : tVarLenRecHeader;
   VarSize,NumKeys : integer;
   GeoKeys : array[0..MaxGeoKeys,1..4] of word;

         procedure FillGeoKey(i,v1,v2,v3,v4 : integer);
         begin
            Geokeys[i,1] := v1;
            Geokeys[i,2] := v2;
            Geokeys[i,3] := v3;
            Geokeys[i,4] := v4;
         end;

begin
   NewLasFileName := ChangeFileExt(fName,'.las');
   AssignFile(NewLasFile,NewLASFileName);
   rewrite(NewLasFile,1);

   DefineHeader;
   NewPtType := PtType;
   if (PtType = 0) then NewLasHeader.PointDataFormat := #0
   else if (PtType = 1) then NewLasHeader.PointDataFormat := #1
   else if (PtType = 2) then NewLasHeader.PointDataFormat := #2
   else if (PtType = 3) then NewLasHeader.PointDataFormat := #3
   else if (PtType = 4) then NewLasHeader.PointDataFormat := #4
   else if (PtType = 5) then NewLasHeader.PointDataFormat := #5
   else if (PtType = 6) then NewLasHeader.PointDataFormat := #6
   else if (PtType = 7) then NewLasHeader.PointDataFormat := #7
   else if (PtType = 8) then NewLasHeader.PointDataFormat := #8;
   NewLASHeader.PointDataRecLen := LidarPointTypeRecordBaseLength(PtType);

   if (NewLasProjectionDefinition.LASProj.ModelType = LasLatLong) or NewlasProjectionDefinition.LasProj.ThisIsUTMFile then begin
      NewLasHeader.NumVarLenRecs := 1;
      NewLASHeader.ZscaleFac := 0.01;
      NewLASHeader.Xoffset := 0;
      NewLASHeader.Yoffset := 0;
      NewLASHeader.Zoffset := 0;
      if (NewLasProjectionDefinition.LASProj.ModelType = LasLatLong) then begin
         NewLASHeader.XscaleFac := 0.000001;
         NewLASHeader.YscaleFac := 0.000001;
         NumKeys := 5;
         FillGeoKey(1, 1024,0,1,2);        //set projection type as geographic
         FillGeoKey(2, 2048,0,1,4269);     //set NAD83
         FillGeoKey(3, 2054,0,1,9102);     //GeogAngularUnitsGeoKey
         FillGeoKey(4, 4096,0,1,5103);     //VerticalCSTypeGeoKey, NAVD88
         FillGeoKey(5, 4099,0,1,9001) ;    //NewLasProjectionDefinition.LasProjection.VerticalUnitsGeoKey);     //VerticalUnitsGeoKey: Linear_Meter
      end
      else if NewLasProjectionDefinition.LasProj.ThisIsUTMfile then begin
         NewLASHeader.XscaleFac := 0.01;
         NewLASHeader.YscaleFac := 0.01;
         NumKeys := 4;
         FillGeoKey(1, 1024,0,1,1);        //set projection type as projected
         FillGeoKey(2, 3072,0,1,NewLasProjectionDefinition.LasProj.ProjectedCSTypeGeoKey);
         FillGeoKey(3, 3076,0,1,NewLasProjectionDefinition.LasProj.ProjLinearUnitsGeoKey);
         FillGeoKey(4, 4099,0,1,NewLasProjectionDefinition.LasProj.VerticalUnitsGeoKey);
      end;
      VarSize := succ(NumKeys) * 8;

      NewLasHeader.OffsetToData := SizeOf(NewLasHeader)  + NewLasHeader.NumVarLenRecs * Sizeof(tVarLenRecHeader) + VarSize;
      BlockWrite(NewLasFile,NewLasHeader,SizeOf(NewLasHeader));

      with VarLenRec do begin
         Reserved := 43707;
         FillHeaderField(UserID,16,'LASF_Projection');
         RecordId := 34735;
         RecLenAfterHeader := VarSize;
         FillHeaderField(Description,32,'Projection Parameters');
      end;
      BlockWrite(NewLasFile,VarLenRec,SizeOf(VarLenRec));
      FillGeoKey(0, 1,1,0,NumKeys);           //set number of keys
      BlockWrite(NewLasFile,GeoKeys[0],VarSize);
   end
   else begin
      NewLasHeader.NumVarLenRecs := 0;
      NewLasHeader.OffsetToData := SizeOf(NewLasHeader);
      BlockWrite(NewLasFile,NewLasHeader,SizeOf(NewLasHeader));
   end;
   InitializeOutputBuffer;
   {$IfDef RecordLASheader}
      WriteLineToDebugFile('write new LAS header ' + NewLasFileName);
      //WriteLineToDebugFile('  header size= ' + IntToStr(SizeOf(NewLasHeader))  + '  ProjLinearUnitsGeoKey=' + IntToStr(NewLasProjectionDefinition.ProjLinearUnitsGeoKey));
      if NewLasProjectionDefinition.LasProjection.ThisIsUTMfile then WriteLineToDebugFile('UTM file');
      //if NewLasProjectionDefinition.LASLatLong then WriteLineToDebugFile('Lat/Long file');
   {$EndIf}
   {$IfDef RecordCreateLASfiles} WriteLineToDebugFile('tCreateLasFile.CreateNewLASfile ' + AllFactorsString(NewLasHeader)); {$EndIf}
end;

