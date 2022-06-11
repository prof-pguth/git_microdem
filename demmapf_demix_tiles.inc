{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of ianMICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2015 Peter L. Guth   }
{____________________________________}


{include file for demmapf.pas}

procedure ResampleHalfSecondForDEMIX(DEM : integer);
var
   NewDEM,NewDEM2 : integer;
   fName : PathStr;
begin
   {$If Defined(RecordCreateGeomorphMaps) or Defined(RecordDEMIX)} WriteLineToDebugFile('');  WriteLineToDebugFile('ResampleForDEMIX DEM=' + IntToStr(DEM) + '  ' + DEMGlb[DEM].AreaName); {$EndIf}
   SaveBackupDefaults;
   MDDef.DefDEMMap := mtDEMBlank;

   MDDef.LidarGridProjection := ArcSecDEM;


   MDdef.DefLidarGeoGridSizeX := 0.5;
   MDdef.DefLidarGeoGridSizeY := 0.5;

   {$If Defined(RecordCreateGeomorphMaps) or Defined(RecordDEMIX)} WriteLineToDebugFile('ResampleForDEMIX 0.5 sec area');  {$EndIf}
   MDDef.LasDEMPixelIs := 1; //area
   fName := ExtractFilePath(DEMGlb[DEM].DEMFileName) + 'md_' + DEMGlb[DEM].AreaName + '_0.5sec_area.tif';
   NewDEM := DEMGlb[DEM].ResampleByAveraging(true,false,fName);
   CloseSingleDEM(newDEM);

   {$If Defined(RecordCreateGeomorphMaps) or Defined(RecordDEMIX)} WriteLineToDebugFile('ResampleForDEMIX 0.5 sec point');  {$EndIf}
   MDDef.LasDEMPixelIs := 2; //point
   fName := ExtractFilePath(DEMGlb[DEM].DEMFileName) + 'md_' + DEMGlb[DEM].AreaName + '_0.5sec_point.tif';
   NewDEM := DEMGlb[DEM].ResampleByAveraging(true,false,fName);
   CloseSingleDEM(newDEM);

   {$If Defined(RecordCreateGeomorphMaps) or Defined(RecordDEMIX)} WriteLineToDebugFile('ResampleForDEMIX 1 sec area');  {$EndIf}
   MDdef.DefLidarGeoGridSizeX := 1;
   MDdef.DefLidarGeoGridSizeY := 1;
   MDDef.LasDEMPixelIs := 1; //area

   fName := ExtractFilePath(DEMGlb[DEM].DEMFileName) + 'md_' + DEMGlb[DEM].AreaName + '_1sec_area.tif';
   {$If Defined(RecordCreateGeomorphMaps) or Defined(RecordDEMIX)} WriteLineToDebugFile('ResampleForDEMIX, ' + fName);  {$EndIf}
   NewDEM := DEMGlb[DEM].ResampleByAveraging(true,false,fName);

   fName := ExtractFilePath(DEMGlb[DEM].DEMFileName) + 'md_' + DEMGlb[DEM].AreaName + '_upsample_0.5sec_area.tif';
   {$If Defined(RecordCreateGeomorphMaps) or Defined(RecordDEMIX)} WriteLineToDebugFile('ResampleForDEMIX, ' + fName);  {$EndIf}
   DEMGlb[NewDEM].ReinterpolateLatLongDEM(NewDEM2,0.5);
   DEMGlb[NewDEM2].SaveAsGeotiff(FName);
   CloseSingleDEM(newDEM2);
   CloseSingleDEM(newDEM);


   MDDef.LasDEMPixelIs := 2; //point

   fName := ExtractFilePath(DEMGlb[DEM].DEMFileName) + 'md_' + DEMGlb[DEM].AreaName + '_1sec_point.tif';
   {$If Defined(RecordCreateGeomorphMaps) or Defined(RecordDEMIX)} WriteLineToDebugFile('ResampleForDEMIX, ' + fName);  {$EndIf}
   NewDEM := DEMGlb[DEM].ResampleByAveraging(true,false,fName);

   fName := ExtractFilePath(DEMGlb[DEM].DEMFileName) + 'md_' + DEMGlb[DEM].AreaName + '_upsample_0.5sec_point.tif';
   {$If Defined(RecordCreateGeomorphMaps) or Defined(RecordDEMIX)} WriteLineToDebugFile('ResampleForDEMIX, ' + fName);  {$EndIf}
   DEMGlb[NewDEM].ReinterpolateLatLongDEM(NewDEM2,0.5);
   DEMGlb[NewDEM2].SaveAsGeotiff(FName);
   CloseSingleDEM(newDEM2);
   CloseSingleDEM(newDEM);

   RestoreBackupDefaults;
   {$If Defined(RecordCreateGeomorphMaps) or Defined(RecordDEMIX)} WriteLineToDebugFile('Out ResampleForDEMIX DEM=' + IntToStr(DEM) + '  ' + DEMGlb[DEM].AreaName);  {$EndIf}
end;




procedure ResampleForDEMIX(DEM : integer);
var
   NewDEM,NewDEM2 : integer;
   fName : PathStr;
begin
   {$If Defined(RecordCreateGeomorphMaps) or Defined(RecordDEMIX)} WriteLineToDebugFile('');  WriteLineToDebugFile('ResampleForDEMIX DEM=' + IntToStr(DEM) + '  ' + DEMGlb[DEM].AreaName); {$EndIf}
   SaveBackupDefaults;
   MDDef.DefDEMMap := mtDEMBlank;


   (*
   MDDef.LidarGridProjection := UTMDEM;
   MDDef.LasDEMPixelIs := 1; //area


   fName := ExtractFilePath(DEMGlb[DEM].DEMFileName) + 'md_' + DEMGlb[DEM].AreaName + '_0.5sec_area.tif';
   NewDEM := DEMGlb[DEM].ResampleByAveraging(true,false,fName);
   CloseSingleDEM(newDEM);
   *)


   MDDef.LidarGridProjection := ArcSecDEM;


   (*
   MDdef.DefLidarGeoGridSizeX := 0.5;
   MDdef.DefLidarGeoGridSizeY := 0.5;

   {$If Defined(RecordCreateGeomorphMaps) or Defined(RecordDEMIX)} WriteLineToDebugFile('ResampleForDEMIX 0.5 sec area');  {$EndIf}
   MDDef.LasDEMPixelIs := 1; //area
   fName := ExtractFilePath(DEMGlb[DEM].DEMFileName) + 'md_' + DEMGlb[DEM].AreaName + '_0.5sec_area.tif';
   NewDEM := DEMGlb[DEM].ResampleByAveraging(true,false,fName);
   CloseSingleDEM(newDEM);

   {$If Defined(RecordCreateGeomorphMaps) or Defined(RecordDEMIX)} WriteLineToDebugFile('ResampleForDEMIX 0.5 sec point');  {$EndIf}
   MDDef.LasDEMPixelIs := 2; //point
   fName := ExtractFilePath(DEMGlb[DEM].DEMFileName) + 'md_' + DEMGlb[DEM].AreaName + '_0.5sec_point.tif';
   NewDEM := DEMGlb[DEM].ResampleByAveraging(true,false,fName);
   CloseSingleDEM(newDEM);
   *)

   {$If Defined(RecordCreateGeomorphMaps) or Defined(RecordDEMIX)} WriteLineToDebugFile('ResampleForDEMIX 1 sec area');  {$EndIf}
   MDdef.DefLidarGeoGridSizeX := 1;
   MDdef.DefLidarGeoGridSizeY := 1;
   MDDef.LasDEMPixelIs := 1; //area

   fName := ExtractFilePath(DEMGlb[DEM].DEMFileName) + 'md_' + DEMGlb[DEM].AreaName + '_1sec_area.tif';
   {$If Defined(RecordCreateGeomorphMaps) or Defined(RecordDEMIX)} WriteLineToDebugFile('ResampleForDEMIX, ' + fName);  {$EndIf}
   NewDEM := DEMGlb[DEM].ResampleByAveraging(true,false,fName);





   (*
   fName := ExtractFilePath(DEMGlb[DEM].DEMFileName) + 'md_' + DEMGlb[DEM].AreaName + '_upsample_0.5sec_area.tif';
   {$If Defined(RecordCreateGeomorphMaps) or Defined(RecordDEMIX)} WriteLineToDebugFile('ResampleForDEMIX, ' + fName);  {$EndIf}
   DEMGlb[NewDEM].ReinterpolateLatLongDEM(NewDEM2,0.5);
   DEMGlb[NewDEM2].SaveAsGeotiff(FName);
   CloseSingleDEM(newDEM2);





   fName := ExtractFilePath(DEMGlb[DEM].DEMFileName) + 'md_' + DEMGlb[DEM].AreaName + '_1sec_reinterpolate_15m.tif';
   DEMGlb[NewDEM].ReinterpolateUTMDEM(NewDEM2,15,DEMGlb[DEM].DEMHeader.UTMzone,fName);
   DEMGlb[NewDEM2].SaveAsGeotiff(FName);
   CloseSingleDEM(newDEM2);

   fName := ExtractFilePath(DEMGlb[DEM].DEMFileName) + 'md_' + DEMGlb[DEM].AreaName + '_1sec_reinterpolate_25m.tif';
   DEMGlb[NewDEM].ReinterpolateUTMDEM(NewDEM2,25,DEMGlb[DEM].DEMHeader.UTMzone,fName);
   DEMGlb[NewDEM2].SaveAsGeotiff(FName);
   CloseSingleDEM(newDEM2);

   fName := ExtractFilePath(DEMGlb[DEM].DEMFileName) + 'md_' + DEMGlb[DEM].AreaName + '_1sec_reinterpolate_30m.tif';
   DEMGlb[NewDEM].ReinterpolateUTMDEM(NewDEM2,30,DEMGlb[DEM].DEMHeader.UTMzone,fName);
   DEMGlb[NewDEM2].SaveAsGeotiff(FName);
   CloseSingleDEM(newDEM2);
   *)

   CloseSingleDEM(newDEM);



   MDDef.LasDEMPixelIs := 2; //point
   fName := ExtractFilePath(DEMGlb[DEM].DEMFileName) + 'md_' + DEMGlb[DEM].AreaName + '_1sec_point.tif';
   {$If Defined(RecordCreateGeomorphMaps) or Defined(RecordDEMIX)} WriteLineToDebugFile('ResampleForDEMIX, ' + fName);  {$EndIf}
   NewDEM := DEMGlb[DEM].ResampleByAveraging(false,false,fName);
   DEMGlb[NewDEM].SetUpMap(NewDEM,true,mtDEMBlank);

   (*
   fName := ExtractFilePath(DEMGlb[DEM].DEMFileName) + 'md_' + DEMGlb[DEM].AreaName + '_upsample_0.5sec_point.tif';
   {$If Defined(RecordCreateGeomorphMaps) or Defined(RecordDEMIX)} WriteLineToDebugFile('ResampleForDEMIX, ' + fName);  {$EndIf}
   DEMGlb[NewDEM].ReinterpolateLatLongDEM(NewDEM2,0.5);
   DEMGlb[NewDEM2].SaveAsGeotiff(FName);
   CloseSingleDEM(newDEM2);
   *)


   CloseSingleDEM(newDEM);

   if (DEMGlb[DEM].DEMSWcornerLat > 50) and (DEMGlb[DEM].DEMSWcornerLat < 60) then begin
      {$If Defined(RecordCreateGeomorphMaps) or Defined(RecordDEMIX)} WriteLineToDebugFile('ResampleForDEMIX 1.5 sec point');  {$EndIf}
      MDdef.DefLidarGeoGridSizeX := 1.5;
      MDdef.DefLidarGeoGridSizeY := 1;
      MDDef.LasDEMPixelIs := 2; //point
      fName := ExtractFilePath(DEMGlb[DEM].DEMFileName) + 'md_' + DEMGlb[DEM].AreaName + '_1.5x1sec_point.tif';
      NewDEM := DEMGlb[DEM].ResampleByAveraging(false,false,fName);
      CloseSingleDEM(newDEM);
   end;

   RestoreBackupDefaults;
   {$If Defined(RecordCreateGeomorphMaps) or Defined(RecordDEMIX)} WriteLineToDebugFile('Out ResampleForDEMIX DEM=' + IntToStr(DEM) + '  ' + DEMGlb[DEM].AreaName);  {$EndIf}
end;



const
   DEMIXLongLetters  : array[0..9] of ANSIchar = ('A','B','C','D','F','G','H','J','K','L');
   DEMIXLatLetters : array[0..9] of ANSIchar = ('M','P','Q','R','T','U','V','X','Y','Z');

          function DEMIXXMinuteIncr(inLat : float64) : integer;
          var
             Lat : integer;
          begin
             if (inLat >= 0) then Lat := trunc(inLat + 0.001)
             else Lat := round(abs(inLat)-1);
             if (Lat < 50) then Result := 6
             else if Lat < 60 then Result := 10
             else if Lat < 70 then Result := 12
             else if Lat < 80 then Result := 20
             else if Lat < 85 then Result := 30
             else Result := 60;
          end;


function DEMIXtileFill(DEM : integer; AreaBox : sfBoundBox) : integer;
var
   Col,Row,Pts,Valid : integer;
   bbgrid : tGridLimits;
   Missing : float64;
   bb : sfBoundBox;
   FilledGrid : boolean;

begin
   DEMGlb[DEM].ComputeMissingData(Missing);
   FilledGrid := Missing >= 99.9;
   {$IfDef RecordDEMIX} writeLineToDebugFile('DEMIXtileFill in, DEM=' + IntToStr(DEM) + ' ' + DEMGlb[DEM].AreaName +  ' grid full %=' + RealToString(Missing,-12,2)); {$EndIf}
   Result:= DEMGlb[DEM].SelectionMap.LoadDEMIXtileOutlines(AreaBox,true);
   GISdb[Result].MyData.First;
   while not GISdb[Result].MyData.eof do begin
      Pts := 0;
      Valid := 0;
      bb := GISdb[Result].MyData.GetRecordBoundingBox;
      DEMGlb[DEM].LatLongDegreeToDEMGridInteger(bb.ymin,bb.xmin,bbgrid.xgridlow,bbgrid.ygridlow);
      DEMGlb[DEM].LatLongDegreeToDEMGridInteger(bb.ymax,bb.xmax,bbgrid.xgridhigh,bbgrid.ygridhigh);
      for Col := bbgrid.xgridlow to bbgrid.xgridhigh do begin
         for Row := bbgrid.ygridlow to bbgrid.ygridhigh do begin
            inc(Pts);
            if DEMGlb[DEM].GridInDataSet(Col,Row) and (FilledGrid or (not DEMGlb[DEM].MissingDataInGrid(Col,Row))) then begin
               inc(valid);
            end;
         end;
      end;
      GISdb[Result].MyData.Edit;
      GISdb[Result].MyData.SetFieldByNameAsFloat('GRID_FULL',100 * Valid / Pts);
      {$IfDef RecordDEMIX} writeLineToDebugFile('File boundary, ' + GridLimitsToString(bbgrid) + RealToString(100 * Valid / Pts,8,2)); {$EndIf}
      GISdb[Result].MyData.Next;
   end;
   {$IfDef RecordDEMIX} writeLineToDebugFile('DEM contains parts of tiles=' + IntToStr(GISdb[Result].MyData.TotRecsInDB)); {$EndIf}
end;


function TMapForm.LoadDEMIXtileOutlines(WantBoundBoxGeo : sfBoundBox; AddGridFull : boolean = false) : integer;
const
   SegsPerSide = 1;
var
   xminutes,yminutes,LongHi,LowLat,HighLat,LongLow : float64;
   FirstLat, LastLat, FirstLong, LastLong, Lat,Long, yt,y,i,xtiles,NumTiles : integer;
   DEMIXFName : PathStr;
   xch : integer;
   ShapeFileCreator : tShapeFileCreation;
begin
   {$IfDef RecordCarto} WriteLineToDebugFile('TMapForm.LoadDEMIXtileOutlines in ' + sfBoundBoxToString(MapDraw.MapCorners.BoundBoxGeo)); {$EndIf}
   if MapDraw.AFullWorldMap then begin
      WantBoundBoxGeo.XMin := -180;
      WantBoundBoxGeo.XMax := 180;
      WantBoundBoxGeo.YMin := -90;
      WantBoundBoxGeo.YMax := 90;
      FirstLat := -90;
      LastLat := 89;
      FirstLong := -180;
      LastLong := 179;
      DEMIXFName := MDTempDir + 'demix_tiles_24k_v0.8.dbf';
   end
   else begin
      if (MapDraw.MapCorners.BoundBoxGeo.yMin < 0) then FirstLat := trunc(MapDraw.MapCorners.BoundBoxGeo.yMin - 1)
      else FirstLat := trunc(MapDraw.MapCorners.BoundBoxGeo.yMin);
      LastLat := trunc(MapDraw.MapCorners.BoundBoxGeo.yMax+1);
      if (MapDraw.MapCorners.BoundBoxGeo.xMin < 0) then FirstLong := trunc(MapDraw.MapCorners.BoundBoxGeo.xMin - 1)
      else FirstLong := trunc(MapDraw.MapCorners.BoundBoxGeo.xMin);
      LastLong := trunc(MapDraw.MapCorners.BoundBoxGeo.xMax+1);
      DEMIXFName := Petmar.NextFileNumber(MDTempDir, 'demix_tiles_',DefaultDBExt);
   end;

   ShapeFileCreator := tShapeFileCreation.Create(WGS84DatumConstants,DEMIXFName,true,5);
   ShapeFileCreator.Table.InsureFieldPresentAndAdded(ftString,'LETTERS',2);
   if AddGridFull then ShapeFileCreator.Table.InsureFieldPresentAndAdded(ftFloat,'GRID_FULL',9,2);

   NumTiles := 0;
   yminutes := 6 / 60;
   yt := 10;
   StartProgress('tiles');
   for Lat := FirstLat to LastLat do begin
      {$IfDef RecordCartoFull} WriteLineToDebugFile('Lat=' + IntToStr(Lat)); {$EndIf}
      UpdateprogressBar((Lat-FirstLat)/(LastLat - FirstLat));

      xminutes := DEMIXXMinuteIncr(Lat) / 60;
      xtiles := 60 div DEMIXXMinuteIncr(Lat);
      for y := 0 to pred(yt) do begin
         LowLat := Lat + Y * yminutes;
         HighLat := LowLat + yminutes;
         if MapDraw.AFullWorldMap or ((HighLat > WantBoundBoxGeo.YMin) and (LowLat < WantBoundBoxGeo.YMax)) then begin
            for Long := FirstLong to LastLong do begin
               {$IfDef RecordCartoFull} WriteLineToDebugFile('Long=' + IntToStr(Long)); {$EndIf}
               LongLow := Long;
               LongHi := Long + xminutes;
               xch := 0;
               for i := 0 to pred(xtiles) do  begin
                  LongLow := Long + i * xminutes;
                  LongHi := LongLow + xminutes;
                  if MapDraw.AFullWorldMap or ((LongLow < WantBoundBoxGeo.xMax) and (LongHi > WantBoundBoxGeo.xMin)) then begin
                     ShapeFileCreator.RecordName := LatToString3(Lat) + DEMIXLatLetters[y] + LongToString4(Long) + DEMIXLongLetters[xch];
                     {$IfDef RecordCartoFull} WriteLineToDebugFile(ShapeFileCreator.RecordName); {$EndIf}
                     ShapeFileCreator.AddBoxLatLongToShapeFile(LowLat,LongLow,HighLat,LongHi);
                     ShapeFileCreator.Table.Edit;
                     ShapeFileCreator.Table.SetFieldByNameAsString('LETTERS',DEMIXLongLetters[xch] + DEMIXLatLetters[y]);
                     ShapeFileCreator.Table.Post;
                     inc(NumTiles);
                  end;
                  inc(xch);
               end;
            end;
         end;
      end;
   end;
   ShapeFileCreator.CloseShapeFiles;
   Result := LoadDataBaseFile(DEMIXFName,true,NumTiles < 100);
   AddProjectionFile(DEMIXFName);
   EndProgress;
   {$IfDef RecordCarto} WriteLineToDebugFile('TMapForm.LoadDEMIXtileOutlines out'); {$EndIf}
end;


function DEMIXtileBoundingBox(tName : shortString) : sfBoundBox;
var
   i,MinIncr,BaseLat : integer;
begin
   //N30AW118B
   tName := UpperCase(tName);
   if (Length(tname) = 9) then begin
      BaseLat := StrToInt(copy(tname,2,2));
      if tName[1] = 'S' then begin
         BaseLat := -BaseLat;
         MinIncr := DEMIXXMinuteIncr(BaseLat);
      end
      else MinIncr := DEMIXXMinuteIncr(BaseLat);
      for I := 0 to 9 do if DEMIXLatLetters[i] = tName[4] then begin
         Result.YMin := BaseLat + (i * 6) / 60;
         Result.YMax := BaseLat + (succ(i) * 6) / 60;
         Break;
      end;
      for I := 0 to 9 do if DEMIXLongLetters[i] = tName[9] then begin
         Result.YMin := BaseLat + (i * MinIncr) / 60;
         Result.YMax := BaseLat + (succ(i) * MinIncr) / 60;
         Break;
      end;
   end;
end;


procedure TMapForm.Strobl10ktiles1Click(Sender: TObject);
begin
   if (MapDraw.DEMonMap <> 0) then DEMIXtileFill(MapDraw.DEMonMap, MapDraw.MapCorners.BoundBoxGeo)
   else LoadDEMIXtileOutlines(MapDraw.MapCorners.BoundBoxGeo);
end;



