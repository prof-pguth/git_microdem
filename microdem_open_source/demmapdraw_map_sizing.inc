
{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of ianMICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2019 Peter L. Guth   }
{                                    }
{ include file for demmapdraw        }
{____________________________________}


procedure tMapDraw.BoundingBoxGeoToDataGrid;
var
   bb : sfBoundBox;
begin
   if DEMMap then begin  //starts at the SW corner
      LatLongDegreeToDataGrid(MapCorners.BoundBoxGeo.ymin,MapCorners.BoundBoxGeo.xmin,MapCorners.BoundBoxDataGrid.xmin,MapCorners.BoundBoxDataGrid.ymin);
      LatLongDegreeToDataGrid(MapCorners.BoundBoxGeo.ymax,MapCorners.BoundBoxGeo.xmax,MapCorners.BoundBoxDataGrid.xmax,MapCorners.BoundBoxDataGrid.ymax);
   end
   else begin           //starts at the NW corner
      LatLongDegreeToDataGrid(MapCorners.BoundBoxGeo.ymin,MapCorners.BoundBoxGeo.xmin,bb.xmin,bb.ymax);
      LatLongDegreeToDataGrid(MapCorners.BoundBoxGeo.ymax,MapCorners.BoundBoxGeo.xmax,bb.xmax,bb.ymin);
      MapCorners.BoundBoxDataGrid := bb;
   end;
   ClipDataGrid(MapCorners.BoundBoxDataGrid);
end;


procedure tMapDraw.BoundingBoxGeoToProjected;
begin
   LatLongDegreeToProjectedCoords(MapCorners.BoundBoxGeo.ymin,MapCorners.BoundBoxGeo.xmin,MapCorners.BoundBoxProj.xmin,MapCorners.BoundBoxProj.ymin);
   LatLongDegreeToProjectedCoords(MapCorners.BoundBoxGeo.ymax,MapCorners.BoundBoxGeo.xmax,MapCorners.BoundBoxProj.xmax,MapCorners.BoundBoxProj.ymax);
end;


procedure tMapDraw.BoundingBoxDataGridToGeo;
begin
   if DEMMap then begin  //starts at the SW corner
      DataGridToLatLongDegree(MapCorners.BoundBoxDataGrid.xmin,MapCorners.BoundBoxDataGrid.ymin,MapCorners.BoundBoxGeo.ymin,MapCorners.BoundBoxGeo.xmin);
      DataGridToLatLongDegree(MapCorners.BoundBoxDataGrid.xmax,MapCorners.BoundBoxDataGrid.ymax,MapCorners.BoundBoxGeo.ymax,MapCorners.BoundBoxGeo.xmax);
   end
   else begin           //starts at the NW corner
      DataGridToLatLongDegree(MapCorners.BoundBoxDataGrid.xmin,MapCorners.BoundBoxDataGrid.ymax,MapCorners.BoundBoxGeo.ymin,MapCorners.BoundBoxGeo.xmin);
      DataGridToLatLongDegree(MapCorners.BoundBoxDataGrid.xmax,MapCorners.BoundBoxDataGrid.ymin,MapCorners.BoundBoxGeo.ymax,MapCorners.BoundBoxGeo.xmax);
   end;
end;

procedure tMapDraw.BoundingBoxProjToDataGrid;
begin
   if DEMMap then begin  //starts at the SW corner
      ProjectedCoordsToDataGrid(MapCorners.BoundBoxProj.xmin,MapCorners.BoundBoxProj.ymin,MapCorners.BoundBoxDataGrid.xmin,MapCorners.BoundBoxDataGrid.ymin);
      ProjectedCoordsToDataGrid(MapCorners.BoundBoxProj.xmax,MapCorners.BoundBoxProj.ymax,MapCorners.BoundBoxDataGrid.xmax,MapCorners.BoundBoxDataGrid.ymax);
   end
   else begin           //starts at the NW corner
      ProjectedCoordsToDataGrid(MapCorners.BoundBoxProj.xmin,MapCorners.BoundBoxProj.ymin,MapCorners.BoundBoxDataGrid.xmin,MapCorners.BoundBoxDataGrid.ymax);
      ProjectedCoordsToDataGrid(MapCorners.BoundBoxProj.xmax,MapCorners.BoundBoxProj.ymax,MapCorners.BoundBoxDataGrid.xmax,MapCorners.BoundBoxDataGrid.ymin);
   end;
   ClipDataGrid(MapCorners.BoundBoxDataGrid);
end;


procedure tMapDraw.BoundingBoxDataGridToProj;
begin
   if DEMMap then begin  //starts at the SW corner
      DataGridToProjectedCoords(MapCorners.BoundBoxDataGrid.xmin,MapCorners.BoundBoxDataGrid.ymin,MapCorners.BoundBoxProj.xmin,MapCorners.BoundBoxProj.ymin);
      DataGridToProjectedCoords(MapCorners.BoundBoxDataGrid.xmax,MapCorners.BoundBoxDataGrid.ymax,MapCorners.BoundBoxProj.xmax,MapCorners.BoundBoxProj.ymax);
   end
   else begin           //starts at the NW corner
      DataGridToProjectedCoords(MapCorners.BoundBoxDataGrid.xmin,MapCorners.BoundBoxDataGrid.ymax,MapCorners.BoundBoxProj.xmin,MapCorners.BoundBoxProj.ymin);
      DataGridToProjectedCoords(MapCorners.BoundBoxDataGrid.xmax,MapCorners.BoundBoxDataGrid.ymin,MapCorners.BoundBoxProj.xmax,MapCorners.BoundBoxProj.ymax);
   end;
end;


procedure TMapDraw.SetFullMapCoverage;
begin
   {$If Defined(ShowProjectedLimits) or Defined(RecordOpenVectorMap)} writeLineToDebugFile('TMapDraw.SetFullMapCoverage in'); {$EndIf}

   if (VectorIndex <> 0) then begin
       PrimMapProj.InitialProjectedBoundingBox(MapCorners.BoundBoxProj);
       MapCorners.BoundBoxGeo := PrimMapProj.ProjectionGeoBox;
   end;

   if DEMMap then begin
      MinMapElev := DEMGlb[DEMonMap].DEMheader.MinElev;
      MaxMapElev := DEMGlb[DEMonMap].DEMheader.MaxElev;
      {$If Defined(RecordElevationScaling)} WriteLineToDebugFile('TMapDraw.SetFullMapCoverage first ' + MapZRangeString); {$EndIf}
      ScaleMapElevationsToDEM;
      MapCorners.BoundBoxDataGrid := DEMGlb[DEMonMap].DEMBoundBoxDataGrid;
      MapCorners.BoundBoxProj := DEMGlb[DEMonMap].DEMBoundBoxProjected;
      {$If Defined(RecordElevationScaling)} WriteLineToDebugFile('TMapDraw.SetFullMapCoverage final ' + MapZRangeString); {$EndIf}
   end;
   {$IfDef ExSat}
   {$Else}
      if ValidSatOnMap then begin
         MapCorners.BoundBoxDataGrid := SatImage[SatOnMap].SatelliteBoundBoxDataGrid(SatImage[SatOnMap].BandForSize);
         MapCorners.BoundBoxProj := SatImage[SatOnMap].SatelliteBoundBoxProj(SatImage[SatOnMap].BandForSize);
         //SatImage[SatOnMap].SatViewFromBoundBoxDataGrid(SatView,MapCorners.BoundBoxDataGrid);
      end;
   {$EndIf}
   MapAspectCheck;

   {$IfDef ShowProjectedLimits} if (VectorIndex = 0) then WriteLineToDebugFile('SetFullMapCoverage, data grid limits: ' +  sfBoundBoxToString(MapCorners.BoundBoxDataGrid)); {$EndIf}
   {$If Defined(ShowProjectedLimits) or Defined(RecordOpenVectorMap)} WriteLineToDebugFile('SetFullMapCoverage, projected limits: ' +  sfBoundBoxToString(MapCorners.BoundBoxProj)); {$EndIf}
   {$If Defined(ShowProjectedLimits) or Defined(RecordOpenVectorMap)} WriteLineToDebugFile('SetFullMapCoverage, Map geo limits: ' +  sfBoundBoxToString(MapCorners.BoundBoxGeo,4)); {$EndIf}
   {$If Defined(ShowUTMZone)} WriteLineToDebugFile('TMapDraw.SetFullMapCoverage mid, UTM=' + PrimMapProj.UTMZoneString); {$EndIf}
   {$If Defined(ShowUTMZone)} WriteLineToDebugFile('TMapDraw.SetFullMapCoverage out, UTM=' + PrimMapProj.UTMZoneString); {$EndIf}
end;


function tMapDraw.SizeOfPixel(x,y : integer) : float64;
var
   Lat,Long,Lat2,Long2,Bearing : float64;
begin
   ScreenToLatLongDegree(x,y,Lat,Long);
   ScreenToLatLongDegree(succ(x),y,Lat2,Long2);
   VincentyCalculateDistanceBearing(Lat,Long,Lat2,Long2,Result,Bearing);
end;


function tMapDraw.ResizeMapByPercentage(Percent : integer) : boolean;
begin
   {$IfDef RecordMapBlow} WriteLineToDebugFile('tMapDraw.ResizeMapByPercentage, pc=' + IntToStr(Percent) +   MapSizeString); {$EndIf}
   Result := true;
   MapXSize := round((MapCorners.BoundBoxDataGrid.xmax - MapCorners.BoundBoxDataGrid.xmin) * 0.01 * Percent);
   MapYSize := round((MapCorners.BoundBoxDataGrid.ymax - MapCorners.BoundBoxDataGrid.ymin) * 0.01 * Percent);
   {$IfDef RecordMapBlow} WriteLineToDebugFile('tMapDraw.ResizeMapByPercentage, first resize ' +   MapSizeString); {$EndIf}
   CurrentZoomLevel := Percent;
   MapAspectCheck;
   CheckMapNotTooLarge;
end;


function tMapDraw.GetMapAspectRatio : float64;
var
   D1,D2,Bear1,Bear2,Lat,Long : float64;
begin
   if (BasicProjection = bpLatLong) then begin
       Long := MapCorners.BoundBoxProj.XMin + 0.5*(MapCorners.BoundBoxProj.XMax - MapCorners.BoundBoxProj.XMin);  //center of the map
       Lat := MapCorners.BoundBoxProj.YMin + 0.5*(MapCorners.BoundBoxProj.YMax - MapCorners.BoundBoxProj.YMin);  //center of the map
       VincentyCalculateDistanceBearing(Lat,Long,Lat,MapCorners.BoundBoxProj.XMax,D1,Bear1);
       VincentyCalculateDistanceBearing(Lat,Long,MapCorners.BoundBoxProj.YMax,Long,D2,Bear2);
       Result := D1/D2;
       {$If Defined(AspectCheck)} WriteLineToDebugFile('tMapDraw.GetMapAspectRatio out, D1=' + RealToString(D1,-12,-2) + ' at=' + RealToString(Bear1,-8,1) + '  D2=' + RealToString(D2,-12,-2)+ ' at=' + RealToString(Bear2,-8,1)); {$EndIf}
   end
   else begin
      Result := (MapCorners.BoundBoxProj.XMax-MapCorners.BoundBoxProj.XMin) / (MapCorners.BoundBoxProj.YMax-MapCorners.BoundBoxProj.YMin);
   end;
   {$If Defined(AspectCheck)} WriteLineToDebugFile('tMapDraw.GetMapAspectRatio out, proj ' +  sfBoundBoxToString(MapCorners.BoundBoxProj) + ' ratio=' + RealToString(Result,-8,-4)); {$EndIf}
end;


procedure tMapDraw.MapAspectCheck;
var
   AspectRatio,ScreenRatio : float64;


      procedure CheckProjectionToUTM;
      var
         uMaxLat,uMinLong,uMinLat,uMaxLong : float64;
      begin
         PrimMapProj.InverseProjectDegrees(MapCorners.BoundBoxProj.XMin,MapCorners.BoundBoxProj.YMax,uMaxLat,uMinLong);
         PrimMapProj.InverseProjectDegrees(MapCorners.BoundBoxProj.XMax,MapCorners.BoundBoxProj.YMin,uMinLat,uMaxLong);
         if MDDef.ShiftMercToUTM and (PrimMapProj.PName in [MercatorEllipsoid]) then begin
            if ((uMaxLong-uMinLong) < MDDef.MercShiftLongLimit) and ((not MDDef.MercShiftSingleZone) or (GetUTMZone(uMaxLong) = GetUTMZone(uMinLong))) then begin
               PrimMapProj.PName := UTMEllipsoidal;
               PrimMapProj.h_DatumCode := MDdef.PreferPrimaryDatum;
               PrimMapProj.DefineDatumFromUTMZone(PrimMapProj.h_DatumCode,GetUTMZone(uMinLong),MDDef.DefaultLatHemi,'tMapDraw.CheckProjectionToUTM');
               PrimMapProj.lat0 :=  0;
               PrimMapProj.long0 := (-177 + pred(GetUTMZone(uMinLong)) * 6) *  DegToRad;
               PrimMapProj.GetProjectParameters;
               BaseTitle := PrimMapProj.GetProjectionName;
               PrimMapProj.ForwardProjectDegrees(uMaxLat,uMinLong,MapCorners.BoundBoxProj.XMin,MapCorners.BoundBoxProj.YMax);
               PrimMapProj.ForwardProjectDegrees(uMinLat,uMaxLong,MapCorners.BoundBoxProj.XMax,MapCorners.BoundBoxProj.YMin);
            end;
         end;
      end;

begin
   {$If Defined(RecordMaximizeMapCoverage) or Defined(AspectCheck) or Defined(RecordMapResize)} WriteLineToDebugFile('tMapDraw.MapAspectCheck in, ' + MapSizeString); {$EndIf}
   {$If Defined(ShowProjectedLimits) or Defined(AspectCheck) or Defined(RecordMapResize)} WriteLineToDebugFile('MapAspectCheck in, proj ' +  sfBoundBoxToString(MapCorners.BoundBoxProj)); {$EndIf}
   {$If Defined(AspectCheck)} if (VectorIndex = 0) then WriteLineToDebugFile('MapAspectCheck in, data: ' +  sfBoundBoxToString(MapCorners.BoundBoxDataGrid,1)); {$EndIf}

   if PrimMapProj.Pname = UTMEllipsoidal then BasicProjection := bpUTM
   else if PrimMapProj.Pname = PlateCaree then BasicProjection := bpLatLong
   else BasicProjection := bpOther;

   if (VectorIndex <> 0) and (PrimMapProj.Pname = MercatorEllipsoid) then CheckProjectionToUTM;

   AspectRatio := GetMapAspectRatio;
   ScreenRatio := MapXSize / MapYSize;
   {$If Defined(AspectCheck)} WriteLineToDebugFile('Aspect ratio=' + RealToString(AspectRatio,-12,-4) + '  Screen Ratio=' + RealToString(ScreenRatio,-12,-4) + '  ' + MapSizeString); {$EndIf}

   if (AspectRatio < ScreenRatio) then MapXSize := abs(round(MapXSize * AspectRatio / ScreenRatio))
   else MapYSize := abs(round(MapYSize / AspectRatio * ScreenRatio));

   MapCorners.ProjDX := (MapCorners.BoundBoxProj.xmax - MapCorners.BoundBoxProj.xmin) / pred(MapXSize);
   MapCorners.ProjDY := (MapCorners.BoundBoxProj.ymax - MapCorners.BoundBoxProj.ymin) / pred(MapYSize);

   if (VectorIndex = 0) then begin
      MapCorners.GridDX := (MapCorners.BoundBoxDataGrid.xmax - MapCorners.BoundBoxDataGrid.xmin) / pred(MapXSize); {Grid increments each row of pixels}
      MapCorners.GridDY := (MapCorners.BoundBoxDataGrid.ymax - MapCorners.BoundBoxDataGrid.ymin) / pred(MapYSize);
   end;

   ScreenPixelSize := abs(SizeOfPixel(MapXSize div 2,MapYSize div 2));
   if (ScreenPixelSize < 0.00001) then ScreenPixelSize := 25000;

   if (BasicProjection = bpLatLong) then MapCorners.BoundBoxGeo := MapCorners.BoundBoxProj else MapCorners.BoundBoxGeo := GetBoundBoxGeo;
   RedefineWGS84DatumConstants(0.5*(MapCorners.BoundBoxGeo.xmax+MapCorners.BoundBoxGeo.xmin),PrimMapProj.LatHemi);
   if (BasicProjection = bpUTM) then MapCorners.BoundBoxUTM := MapCorners.BoundBoxProj else MapCorners.BoundBoxUTM := GetBoundBoxUTM;

   {$If Defined(ShowProjectedLimits)} WriteLineToDebugFile('MapAspectCheck out, UTM: ' +  sfBoundBoxToString(MapCorners.BoundBoxUTM,1)); {$EndIf}
   {$If Defined(ShowProjectedLimits)} WriteLineToDebugFile('MapAspectCheck out, geo: ' +  sfBoundBoxToString(MapCorners.BoundBoxGeo,6)); {$EndIf}
   {$If Defined(ShowProjectedLimits)} WriteLineToDebugFile('MapAspectCheck out, proj: ' +  sfBoundBoxToString(MapCorners.BoundBoxProj,2)); {$EndIf}
   {$If Defined(ShowProjectedLimits)} if (VectorIndex = 0) then WriteLineToDebugFile('MapAspectCheck out, data: ' +  sfBoundBoxToString(MapCorners.BoundBoxDataGrid,1)); {$EndIf}
   {$If Defined(AspectCheck)} WriteLineToDebugFile('tMapDraw.MapAspectCheck out, ' + MapSizeString); {$EndIf}
end;


procedure tMapDraw.MaximizeLatLongMapCoverage(bb : sfBoundBox; xsize: integer = -99; ysize : integer = -99);  {all in degrees}
begin
    MaximizeLatLongMapCoverage(bb.ymin,bb.xmin,bb.ymax,bb.xmax,xsize,ysize);
end;


procedure tMapDraw.MaximizeLatLongMapCoverage(uMinLat,uMinLong,uMaxLat,uMaxLong : float64; xsize: integer = -99; ysize : integer = -99); {all in degrees}
begin
   {$IfDef RecordMaximizeMapCoverage} WriteLineToDebugFile('tMapDraw.MaximizeLatLongMapCoverage in, ' + MapSizeString); {$EndIf}
   MinOfPairFirst(uMinLat,uMaxLat);
   MinOfPairFirst(uMinLong,uMaxLong);

   MapCorners.BoundBoxGeo.ymin := UMinLat;
   MapCorners.BoundBoxGeo.xmin := UMinLong;
   MapCorners.BoundBoxGeo.ymax := UMaxLat;
   MapCorners.BoundBoxGeo.xmax := UMaxLong;
   {$IfDef RecordMaximizeMapCoverage} writeLineToDebugFile('MaximizeLatLongMapCoverage, in geo limits: ' +  sfBoundBoxToString(MapCorners.BoundBoxGeo,6)); {$EndIf}

   if (XSize > 0) then begin
      MapXSize := XSize;
      MapYSize := XSize;
   end
   else begin
      MapXSize := MDDef.DefaultMapXSize;
      MapYSize := MDDef.DefaultMapYSize;
   end;

   if (VectorIndex = 0) then begin
      {$IfDef RecordMaximizeMapCoverage} if (VectorIndex = 0) then writeLineToDebugFile('MaximizeLatLongMapCoverage, initial grid limits: ' +  sfBoundBoxToString(MapCorners.BoundBoxDataGrid,2)); {$EndIf}
      BoundingBoxGeoToDataGrid;

      {$IfDef RecordMaximizeMapCoverage} if (VectorIndex = 0) then writeLineToDebugFile('MaximizeLatLongMapCoverage, first grid limits: ' +  sfBoundBoxToString(MapCorners.BoundBoxDataGrid,2)); {$EndIf}

      while ((MapCorners.BoundBoxDataGrid.xmax - MapCorners.BoundBoxDataGrid.xmin) < 5) or ((MapCorners.BoundBoxDataGrid.ymax - MapCorners.BoundBoxDataGrid.ymin) < 5) do begin
         MapCorners.BoundBoxDataGrid.xmin := MapCorners.BoundBoxDataGrid.xmin - 1;
         MapCorners.BoundBoxDataGrid.ymin := MapCorners.BoundBoxDataGrid.ymin - 1;
         MapCorners.BoundBoxDataGrid.xmax := MapCorners.BoundBoxDataGrid.xmax + 1;
         MapCorners.BoundBoxDataGrid.ymax := MapCorners.BoundBoxDataGrid.ymax + 1;
      end;

     {$IfDef RecordMaximizeMapCoverage} if (VectorIndex = 0) then writeLineToDebugFile('MaximizeLatLongMapCoverage, grid limits: ' +  sfBoundBoxToString(MapCorners.BoundBoxDataGrid,2)); {$EndIf}

      DataGridToProjectedCoords(MapCorners.BoundBoxDataGrid.xmin,MapCorners.BoundBoxDataGrid.ymin,MapCorners.BoundBoxProj.xmin,MapCorners.BoundBoxProj.ymin);
      DataGridToProjectedCoords(MapCorners.BoundBoxDataGrid.xmax,MapCorners.BoundBoxDataGrid.ymax,MapCorners.BoundBoxProj.xmax,MapCorners.BoundBoxProj.ymax);
     {$IfDef RecordMaximizeMapCoverage} writeLineToDebugFile('MaximizeLatLongMapCoverage, proj limits: ' +  sfBoundBoxToString(MapCorners.BoundBoxProj,4)); {$EndIf}
      if DEMMap then begin
         ScaleMapElevationsToDEM;
      end;
   end
   else begin
      {$IfDef RecordMaximizeMapCoverage} WriteLineToDebugFile('Before CheckProjectionToUTM:  ' + MapSizeString); {$EndIf}
      if (abs(uMaxLat) > 86) or (uMaxLat - uMinLat > 160) then exit;
      if (uMaxLong - uMinLong > 270) then exit;
      PrimMapProj.FullWorld := false;
      PrimMapProj.ForwardProjectDegrees(uMaxLat,uMinLong,MapCorners.BoundBoxProj.XMin,MapCorners.BoundBoxProj.YMax);
      PrimMapProj.ForwardProjectDegrees(uMinLat,uMaxLong,MapCorners.BoundBoxProj.XMax,MapCorners.BoundBoxProj.YMin);
      {$IfDef RecordMaximizeMapCoverage} writeLineToDebugFile('MaximizeLatLongMapCoverage, proj limits: ' +  sfBoundBoxToString(MapCorners.BoundBoxProj,4)); {$EndIf}
   end;
   MapAspectCheck;
   {$IfDef RecordMaximizeMapCoverage} WriteLineToDebugFile('tMapForm.MaximizeMapCoverage out, ' + MapSizeString); {$EndIf}
end;


function TMapDraw.RecenterMap(Lat,Long : float64; Percent : integer) : boolean;
var
   xs,ys,xproj,yproj  : float64;
begin
   {$If Defined(RecordMapDraw) or Defined(RecordZoomIn)} writeLineToDebugFile('Recenter map on ' + LatLongDegreeToString(Lat,Long)); {$EndIf}
    if DEMMap and (not DEMGlb[DEMonMap].LatLongDegreeInDEM(Lat,Long)) then begin
       Result := false;
       exit;
    end;
   Result := true;
   xs := (MapCorners.BoundBoxProj.xmax - MapCorners.BoundBoxProj.xmin) * Percent * 0.01 / 2;
   ys := (MapCorners.BoundBoxProj.ymax - MapCorners.BoundBoxProj.ymin) * Percent * 0.01 / 2;
   PrimMapProj.ForwardProjectDegrees(Lat,Long,xproj,yproj);
   MapCorners.BoundBoxProj.xmin := xproj - xs;
   MapCorners.BoundBoxProj.xmax := xproj + xs;
   MapCorners.BoundBoxProj.ymin := yproj - ys;
   MapCorners.BoundBoxProj.ymax := yproj + ys;

   if (VectorIndex = 0) then begin
      BoundingBoxProjToDataGrid;
   end;
   MapAspectCheck;
end;


procedure tMapDraw.CheckMapNotTooLarge;
var
   AspectRatio : float64;
begin
   if (MapXsize > MaxScreenXMax) or (MapYsize > MaxScreenYMax) then begin
      MessageToContinue('Max size allowed is ' + IntToStr(MaxScreenXMax) + ' by ' + IntToStr(MaxScreenYMax) + MessLineBreak + 'You need: ' +  IntToStr(MapXSize) + ' by ' + IntToStr(MapYSize) );
      AspectRatio := MapXSize / MapYSize;
      if (MapXsize> MaxScreenXMax) then begin
         MapXSize := MaxScreenXMax;
         MapYSize := round(MapYSize / AspectRatio);
      end;
      if (MapYsize > MaxScreenYMax) then begin
         MapYSize := MaxScreenYMax;
         MapXSize := round(MapYSize * AspectRatio);
      end;
   end;
   ResetMarginalia;
end;


procedure TMapDraw.DefineNewDEMMap(CurDEM : integer; mt : tMapType);
var
   TStr : ANSIString;
begin
   if ValidDEM(CurDEM) then begin
      {$If Defined(ShowUTMZone)} WriteLineToDebugFile('TMapDraw.DefineNewDEMMap in, UTM=' + IntToStr(DEMGLB[CurDEM].DEMheader.UTMZone) + DEMGLB[CurDEM].DEMheader.LatHemi); {$EndIf}
      if DEMGLB[CurDEM].ElevationDEM then TStr := 'DEM' else TStr := 'Grid';
      BaseTitle := RemoveUnderscores(TStr + ': ' + DEMGLB[CurDEM].AreaName);
      DEMonMap := CurDEM;
      (*'
      if DEMGLB[CurDEM].LandCoverGrid then begin
         MapType := mtLandCover;
      end
      else *)
      begin
         if (mt = mtDEMReflectElevMerge) then begin
            mt := mtGrayReflect;
            MapMerge := mmElevation;
         end;
         MapType := mt;
      end;
      AdjustProjectionForUTMZone('DefineNewDEMMap mid',MDDef.PreferPrimaryDatum,DEMGLB[CurDEM].DEMheader.UTMZone,DEMGLB[CurDEM].DEMheader.LatHemi);
      SetFullMapCoverage;
      {$If Defined(ShowProjectedLimits) or Defined(ShowUTMZone)} WriteLineToDebugFile('TMapDraw.DefineNewDEMMap out, UTM=' + PrimMapProj.UTMZoneString); {$EndIf}
      {$If Defined(ShowProjectedLimits) or Defined(RecordShortDefineDatum)} WriteLineToDebugFile('Map data grid limits: ' +  sfBoundBoxToString(MapCorners.BoundBoxDataGrid)); {$EndIf}
   end;
end;


{$IfDef ExSat}
{$Else}
   procedure tMapDraw.DefineSatMap(CurSat : integer; mt : tMapType);
   begin
      {$IfDef RecordSat} WriteLineToDebugFile('tMapDraw.DefineSatMap enter, datum=' + SatImage[CurSat].ImageMapProjection.h_DatumCode); {$EndIf}
      SATonMap := CurSat;
      MapType := mt;
      PrimMapProj := SatImage[CurSat].ImageMapProjection;
      if (SatImage[CurSat].ImageMapProjection.PName = PlateCaree) then
         SatImage[CurSat].ImageMapProjection.DefineDatumFromUTMZone(MDdef.PreferPrimaryDatum,GetUTMZone(SatImage[CurSat].RegVars.UpLeftX + (MapCorners.BoundBoxDataGrid.xmin + ColsDisplayed div 2) * SatImage[CurSat].BandXSpace[SatImage[CurSat].BandForSize]),
           SatImage[CurSat].ImageMapProjection.LatHemi,'definesatmap');
      SetFullMapCoverage;
      {$IfDef RecordSat} WriteLineToDebugFile('tMapDraw.DefineSatMap out'); {$EndIf}
   end;
{$EndIf}


























