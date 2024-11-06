{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program       }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2024 Peter L. Guth   }
{____________________________________}


{include file for demmapf.pas}


procedure ResampleForUTM_GeoComparison(DEM : integer; OutPath : PathStr = '');
//   9/8/2023, check if this is really needed; only called by TMapForm.ComapreUTMvsgeographic1Click(Sender: TObject);
var
   NewDEM1, NewDEM2, NewDEM3 : integer;
   fName : PathStr;
begin
   {$If Defined(RecordCreateGeomorphMaps) or Defined(RecordDEMIXFull)} WriteLineToDebugFile(''); WriteLineToDebugFile('ResampleForDEMIX DEM=' + IntToStr(DEM) + '  ' + DEMGlb[DEM].AreaName); {$EndIf}
   SaveBackupDefaults;
   MDDef.DefDEMMap := mtDEMBlank;
   MDDef.LidarGridProjection := ArcSecDEM;
   MDDef.LasDEMPixelIs := PixelIsArea;

   if (OutPath = '') then OutPath := ExtractFilePath(DEMGlb[DEM].DEMFileName);

   {$If Defined(RecordCreateGeomorphMaps) or Defined(RecordDEMIXFull)} WriteLineToDebugFile('ResampleForDEMIX 1 sec area'); {$EndIf}
   MDdef.DefLidarGeoGridSizeX := 1;
   MDdef.DefLidarGeoGridSizeY := 1;
   fName := OutPath +  DEMGlb[DEM].AreaName + '_1sec.tif';
   {$If Defined(RecordCreateGeomorphMaps) or Defined(RecordDEMIXFull)} WriteLineToDebugFile('ResampleForDEMIX, ' + fName); {$EndIf}
   NewDEM1 := DEMGlb[DEM].ResampleByAveraging(true,fName);

   MDDef.LidarGridProjection := UTMbasedDEM;
   MDdef.DefLidarXGridSize := 30;
   MDdef.DefLidarYGridSize := 30;
   fName := OutPath +  DEMGlb[DEM].AreaName + '_30m.tif';
   {$If Defined(RecordCreateGeomorphMaps) or Defined(RecordDEMIXFull)} WriteLineToDebugFile('ResampleForDEMIX, ' + fName); {$EndIf}
   NewDEM2 := DEMGlb[DEM].ResampleByAveraging(true,fName);

   MDDef.LidarGridProjection := UTMbasedDEM;
   MDdef.DefLidarXGridSize := DEMGlb[NewDEM1].AverageSpace;
   MDdef.DefLidarYGridSize := MDdef.DefLidarXGridSize;
   fName := OutPath +  DEMGlb[DEM].AreaName + '_' + IntToStr(round(MDdef.DefLidarXGridSize)) + '_m.tif';
   {$If Defined(RecordCreateGeomorphMaps) or Defined(RecordDEMIXFull)} WriteLineToDebugFile('ResampleForDEMIX, ' + fName); {$EndIf}
   NewDEM3 := DEMGlb[DEM].ResampleByAveraging(true,fName);

   RestoreBackupDefaults;
   {$If Defined(RecordCreateGeomorphMaps) or Defined(RecordDEMIXFull)} WriteLineToDebugFile('Out ResampleForDEMIX DEM=' + IntToStr(DEM) + '  ' + DEMGlb[DEM].AreaName); {$EndIf}
end;




const
   DEMIXLongLetters : array[0..9] of ANSIchar = ('A','B','C','D','F','G','H','J','K','L');
   DEMIXLatLetters  : array[0..9] of ANSIchar = ('M','P','Q','R','T','U','V','X','Y','Z');

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


procedure CreateDEMIXTileShapefile(var fName : PathStr; WantBoundBoxGeo : sfBoundBox; AddGridFull : boolean = false; AddTileSize : boolean = false);
const
   SegsPerSide = 1;
var
   xminutes,yminutes,LongHi,LowLat,HighLat,LongLow,Wide,High,Bearing : float64;
   FirstLat, LastLat, FirstLong, LastLong, Lat,Long, yt,y,i,xtiles : integer;
   xch : integer;
   bb : sfBoundBox;
   ShapeFileCreator : tShapeFileCreation;
begin
   {$If Defined(RecordCartoFull)} WriteLineToDebugFile('TMapForm.LoadDEMIXtileOutlines in ' + sfBoundBoxToString(WantBoundBoxGeo)); {$EndIf}
   if (WantBoundBoxGeo.ymin < 0) then FirstLat := trunc(WantBoundBoxGeo.ymin - 1)
   else FirstLat := trunc(WantBoundBoxGeo.yMin);
   LastLat := trunc(WantBoundBoxGeo.yMax+1);
   if (WantBoundBoxGeo.xMin < 0) then FirstLong := trunc(WantBoundBoxGeo.xMin - 1)
   else FirstLong := trunc(WantBoundBoxGeo.xMin);
   LastLong := trunc(WantBoundBoxGeo.xMax+1);

   if (fName = '') then fName := Petmar.NextFileNumber(MDTempDir, 'demix_tiles_',DefaultDBExt);

   ShapeFileCreator := tShapeFileCreation.Create(WGS84DatumConstants,fName,true,5);
   ShapeFileCreator.Table.InsureFieldPresentAndAdded(ftString,'LETTERS',2);
   if AddGridFull then begin
      ShapeFileCreator.Table.InsureFieldPresentAndAdded(ftFloat,'GRID_FULL',9,2);
      ShapeFileCreator.Table.InsureFieldPresentAndAdded(ftFloat,'RELIEF',9,2);
   end;
   if AddTileSize then begin
      ShapeFileCreator.Table.InsureFieldPresentAndAdded(ftFloat,'TILE_WIDE',9,3);
      ShapeFileCreator.Table.InsureFieldPresentAndAdded(ftFloat,'TILE_HIGH',9,3);
      ShapeFileCreator.Table.InsureFieldPresentAndAdded(ftFloat,'TILE_KM2',9,3);
   end;

   yminutes := 6 / 60;
   yt := 10;
   StartProgress('tiles');
   for Lat := FirstLat to LastLat do begin
      {$If Defined(RecordCartoFull)} WriteLineToDebugFile('Lat=' + IntToStr(Lat)); {$EndIf}
      UpdateprogressBar((Lat-FirstLat)/(LastLat - FirstLat));

      xminutes := DEMIXXMinuteIncr(Lat) / 60;
      xtiles := 60 div DEMIXXMinuteIncr(Lat);
      for y := 0 to pred(yt) do begin
         LowLat := Lat + Y * yminutes;
         HighLat := LowLat + yminutes;
         if ((HighLat > WantBoundBoxGeo.YMin) and (LowLat < WantBoundBoxGeo.YMax)) then begin
            for Long := FirstLong to LastLong do begin
               {$If Defined(RecordCartoFull)} WriteLineToDebugFile('Long=' + IntToStr(Long)); {$EndIf}
               LongLow := Long;
               LongHi := Long + xminutes;
               xch := 0;
               for i := 0 to pred(xtiles) do  begin
                  LongLow := Long + i * xminutes;
                  LongHi := LongLow + xminutes;
                  if ((LongLow < WantBoundBoxGeo.xMax) and (LongHi > WantBoundBoxGeo.xMin)) then begin
                     ShapeFileCreator.RecordName := LatToString3(Lat) + DEMIXLatLetters[y] + LongToString4(Long) + DEMIXLongLetters[xch];
                     {$IfDef RecordCartoFull} WriteLineToDebugFile(ShapeFileCreator.RecordName); {$EndIf}
                     bb.xmin := LongLow;
                     bb.xmax := LongHi;
                     bb.ymin := LowLat;
                     bb.ymax := HighLat;
                     ShapeFileCreator.AddBoundBoxGeoToShapeStream(bb);
                     ShapeFileCreator.Table.Edit;
                     ShapeFileCreator.Table.SetFieldByNameAsString('LETTERS',DEMIXLongLetters[xch] + DEMIXLatLetters[y]);
                     if AddTileSize then begin
                        VincentyCalculateDistanceBearing(LowLat,0.5*(LongLow + LongHi),HighLat,0.5*(LongLow + LongHi), High,Bearing);
                        VincentyCalculateDistanceBearing(0.5*(LowLat+HighLat),LongLow,0.5*(LowLat+HighLat),LongHi, Wide,Bearing);
                        ShapeFileCreator.Table.SetFieldByNameAsFloat('TILE_WIDE',0.001 * Wide);
                        ShapeFileCreator.Table.SetFieldByNameAsFloat('TILE_HIGH',0.001 * High);
                        ShapeFileCreator.Table.SetFieldByNameAsFloat('TILE_KM2',0.001 * Wide * 0.001 * High);
                     end;
                     ShapeFileCreator.Table.Post;
                  end;
                  inc(xch);
               end;
            end;
         end;
      end;
   end;
   ShapeFileCreator.CloseShapeFiles;
   AddProjectionFile(fName);
   EndProgress;
   {$If Defined(RecordCarto)} WriteLineToDebugFile('TMapForm.LoadDEMIXtileOutlines out '+ sfBoundBoxToString(WantBoundBoxGeo)); {$EndIf}
end;


function DEMIXtileFill(DEM : integer; AreaBox : sfBoundBox; OpenTable : boolean = true) : integer;
var
   Col,Row : integer;
   Pts,Valid : int64;
   bbgrid : tGridLimits;
   Full,z,ElevMin,ElevMax,Relief : float32;
   bb : sfBoundBox;
   fName : PathStr;
begin
   {$IfDef RecordDEMIXTiles} writeLineToDebugFile('DEMIXtileFill in, DEM=' + IntToStr(DEM)); {$EndIf}
   if ValidDEM(DEM) then try
      StartProgress('DEMIX tiles');
      fName := NextFileNumber(MDtempDir,DEMGLB[DEM].AreaName+ '_demix_tile_fill_','.dbf');
      if (DEMGLB[DEM].SelectionMap = Nil) then Result := LoadDEMIXtileOutlinesNoMap(FName,AreaBox,true,false,OpenTable)
      else Result := DEMGLB[DEM].SelectionMap.LoadDEMIXtileOutlines(fName,AreaBox,true,OpenTable);

      if (Result <> 0) then try
         {$IfDef RecordDEMIXTiles} writeLineToDebugFile('DEMIXtileFill, db created recs=' + IntToStr(GISdb[Result].MyData.TotRecsInDB)); {$EndIf}
         GISdb[Result].MyData.First;
         GISdb[Result].EmpSource.Enabled := false;
         while not GISdb[Result].MyData.eof do begin
            Pts := 0;
            Valid := 0;
            ElevMin := 99e39;
            ElevMax := -99e39;
            bb := GISdb[Result].MyData.GetRecordBoundingBox;
            DEMGlb[DEM].LatLongDegreeToDEMGridInteger(bb.ymin,bb.xmin,bbgrid.xgridlow,bbgrid.ygridlow);
            DEMGlb[DEM].LatLongDegreeToDEMGridInteger(bb.ymax,bb.xmax,bbgrid.xgridhigh,bbgrid.ygridhigh);
            for Col := bbgrid.xgridlow to bbgrid.xgridhigh do begin
               UpdateProgressBar((bbgrid.xgridhigh - Col) / (bbgrid.xgridhigh - bbgrid.xgridlow));
               for Row := bbgrid.ygridlow to bbgrid.ygridhigh do begin
                  inc(Pts);
                  if DEMGlb[DEM].GetElevMetersOnGrid(Col,Row,z) then begin  //GridInDataSet(Col,Row) and ((not DEMGlb[DEM].MissingDataInGrid(Col,Row))) then begin
                     inc(valid);
                     PetMath.CompareValueToExtremes(z,ElevMin,ElevMax);
                  end;
               end;
            end;
            GISdb[Result].MyData.Edit;
            Full := 100 * Valid / Pts;
            GISdb[Result].MyData.CarefullySetFloat32('GRID_FULL',Full,0.01);
            Relief := (ElevMax-ElevMin);
            GISdb[Result].MyData.CarefullySetFloat32('RELIEF',Relief,0.01);
            {$IfDef RecordDEMIXTiles} writeLineToDebugFile('File boundary, ' + GridLimitsToString(bbgrid) + RealToString(Full,8,2)); {$EndIf}
            GISdb[Result].MyData.Next;
         end;
      finally
      end;
   finally
      EndProgress;
      if OpenTable then GISdb[Result].ShowStatus;
   end;
   {$IfDef RecordDEMIXTiles} writeLineToDebugFile('DEM contains parts of tiles=' + IntToStr(GISdb[Result].MyData.TotRecsInDB)); {$EndIf}
end;


function LoadDEMIXtileOutlinesNoMap(var DEMIXFName : PathStr; WantBoundBoxGeo : sfBoundBox; AddGridFull : boolean = false; AddTileSize : boolean = false; OpenTable : boolean = true) : integer;
begin
   {$If Defined(RecordCarto)} WriteLineToDebugFile('LoadDEMIXtileOutlinesNoMap in ' + sfBoundBoxToString(WantBoundBoxGeo)); {$EndIf}
   CreateDEMIXTileShapefile(DEMIXFName,WantBoundBoxGeo,AddGridFull,AddTileSize);
   OpenNumberedGISDataBase(Result,DEMIXFName,OpenTable);
   {$If Defined(RecordCarto)} WriteLineToDebugFile('LoadDEMIXtileOutlinesNoMap out'); {$EndIf}
end;


function TMapForm.LoadDEMIXtileOutlines(var DEMIXFName : PathStr; WantBoundBoxGeo : sfBoundBox; AddGridFull : boolean = false; AddTileSize : boolean = false; OpenTable : boolean = true) : integer;
begin
   {$If Defined(RecordCarto)} WriteLineToDebugFile('TMapForm.LoadDEMIXtileOutlines in ' + sfBoundBoxToString(MapDraw.MapCorners.BoundBoxGeo)); {$EndIf}
   CreateDEMIXTileShapefile(DEMIXFName,WantBoundBoxGeo,AddGridFull,AddTileSize);
   Result := LoadDataBaseFile(DEMIXFName,OpenTable);
   {$If Defined(RecordCarto)} WriteLineToDebugFile('TMapForm.LoadDEMIXtileOutlines out'); {$EndIf}
end;


procedure TMapForm.ClipDEMtoFullDEMIXTiles(NewName : PathStr = '');
begin
   if ValidDEM(MapDraw.DEMonMap) then begin
       ClipTheDEMtoFullDEMIXTiles(MapDraw.DEMonMap);
       DEMGlb[MapDraw.DEMonMap].SelectionMap.ReloadDEMClick(Nil);
   end;
end;


function DEMIXtilesOnDEM(DEM : integer; RecordFill : tStringList = Nil) : tStringList;
var
   db : integer;
   Name : shortstring;
begin
   if ValidDEM(DEM) then begin
      Result := tStringList.Create;
      db := DEMIXtileFill(DEM,DEMGlb[DEM].DEMBoundBoxGeo,False);
      GISdb[DB].ApplyGISFilter('GRID_FULL >' + IntToStr(MDDef.DEMIX_Tile_Full));
      GISdb[DB].Empsource.Enabled := false;
      {$If Defined(RecordCarto)} WriteLineToDebugFile('TMapForm.DEMIXtilesOnMap, tiles=' + IntToStr(GISdb[db].MyData.FiltRecsInDB)); {$EndIf}
      while not GISdb[db].MyData.Eof do begin
         Name := GISdb[db].MyData.GetFieldByNameAsString('NAME');
         Result.Add(Name);
         if (RecordFill <> Nil) then begin
             RecordFill.Add(Name + ',' + RealToString(GISdb[db].MyData.GetFieldByNameAsFloat('GRID_FULL'),-8,-2));
         end;
         GISdb[db].MyData.Next;
      end;
      CloseAndNilNumberedDB(db);
   end;
end;


function NumFilledDEMIXtilesOnDEM(DEM : integer) : integer;
var
   db : integer;
   //Name : shortstring;
begin
   if ValidDEM(DEM) then begin
      db := DEMIXtileFill(DEM,DEMGlb[DEM].DEMBoundBoxGeo,False);
      GISdb[DB].ApplyGISFilter('GRID_FULL >' + IntToStr(MDDef.DEMIX_Tile_Full));
      Result := GISdb[db].MyData.FiltRecsInDB;
      CloseAndNilNumberedDB(db);
   end
   else Result := 0;
end;


function TMapForm.DEMIXtilesOnMap(RecordFill : tStringList = Nil) : tStringList;
var
   bb : sfBoundBox;
   db : integer;
   Name : shortstring;
   fName : PathStr;
begin
   Result := tStringList.Create;
   bb := MapDraw.MapCorners.BoundBoxGeo;
   if ValidDEM(MapDraw.DEMonMap) then begin
      db := DEMIXtileFill(MapDraw.DEMonMap,bb,False);
      GISdb[DB].ApplyGISFilter('GRID_FULL >' + IntToStr(MDDef.DEMIX_Tile_Full));
      GISdb[DB].Empsource.Enabled := false;
   end
   else begin
      fName := NextFileNumber(MDtempDir,'demix_tile_fill_','.dbf');
      LoadDEMIXtileOutlines(fName,bb,False,False,False);
   end;
   {$If Defined(RecordCarto)} WriteLineToDebugFile('TMapForm.DEMIXtilesOnMap, tiles=' + IntToStr(GISdb[db].MyData.FiltRecsInDB)); {$EndIf}
   while not GISdb[db].MyData.Eof do begin
      Name := GISdb[db].MyData.GetFieldByNameAsString('NAME');
      Result.Add(Name);
      if (RecordFill <> Nil) then begin
          RecordFill.Add(Name + ',' + RealToString(GISdb[db].MyData.GetFieldByNameAsFloat('GRID_FULL'),-8,-2));
      end;
      GISdb[db].MyData.Next;
   end;
   CloseAndNilNumberedDB(db);
end;


function DEMIXtileBoundingBox(tName : shortString; PixelIsAreaSafe : boolean = false) : sfBoundBox;
var
   i,MinIncr,BaseLat,BaseLong : integer;
begin
   tName := UpperCase(tName);
   if (Length(tname) = 9) then begin
      BaseLat := StrToInt(copy(tname,2,2));
      if tName[1] = 'S' then begin
         BaseLat := -BaseLat;
      end;
      MinIncr := DEMIXXMinuteIncr(BaseLat);
      for I := 0 to 9 do if DEMIXLatLetters[i] = tName[4] then begin
         Result.YMin := BaseLat + (i * 6) / 60;
         Result.YMax := BaseLat + (succ(i) * 6) / 60;
         Break;
      end;
      BaseLong := StrToInt(copy(tname,6,3));
      if tName[5] = 'W' then BaseLong := -BaseLong;
      for I := 0 to 9 do if DEMIXLongLetters[i] = tName[9] then begin
         Result.XMin := BaseLong + (i * MinIncr) / 60;
         Result.XMax := BaseLong + (succ(i) * MinIncr) / 60;
         Break;
      end;
   end;
   if PixelIsAreaSafe then begin
      SafeAdjustGeoBoundBoxForPixelIsArea(Result);
   end;
end;

procedure DEMIXtileCentroid(tName : shortString; var Lat,Long : float32);
var
   bb : sfBoundBox;
begin
   bb := DEMIXtileBoundingBox(tName);
   Long := 0.5 * (bb.xmin + bb.xmax);
   Lat  := 0.5 * (bb.ymin + bb.ymax);
end;







