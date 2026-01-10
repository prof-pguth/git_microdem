{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2026 Peter L. Guth  }
{________________________________________}
{                                        }
{ include file for demmapf               }
{________________________________________}


procedure MatchAnotherDEMMap(DEM,CurDEM : integer);
begin
   if ValidDEM(DEM) and ValidDEM(CurDEM) and (DEMGlb[DEM].SelectionMap <> Nil) and (DEMGlb[DEM].SelectionMap <> Nil) then begin
      if DEMGlb[CurDEM].SelectionMap.FullMapSpeedButton.Enabled then begin
         DEMGlb[DEM].SelectionMap.MapDraw.MapCorners.BoundBoxProj := DEMGlb[CurDEM].SelectionMap.MapDraw.MapCorners.BoundBoxProj;
         DEMGlb[DEM].SelectionMap.SubsetAndZoomMapFromProjectedBounds;
      end;
      if (DEMGlb[DEM].SelectionMap.MapDraw.MapXSize <> DEMGlb[CurDEM].SelectionMap.MapDraw.MapXSize) or (DEMGlb[DEM].SelectionMap.MapDraw.MapYSize <> DEMGlb[CurDEM].SelectionMap.MapDraw.MapYSize) then begin
         DEMGlb[DEM].SelectionMap.SetMapPixelSize(DEMGlb[CurDEM].SelectionMap.MapDraw.ScreenPixelSize);
      end;
   end;
end;


procedure TMapForm.CheckAndDrawNewCorners;
var
   AvgElev : float32;
begin
   if MapDraw.DEMMap then begin
      DEMGlb[MapDraw.DEMonMap].ClipDEMGrid(MapDraw.MapCorners.BoundBoxDataGrid.xmin,MapDraw.MapCorners.BoundBoxDataGrid.ymax);
      DEMGlb[MapDraw.DEMonMap].ClipDEMGrid(MapDraw.MapCorners.BoundBoxDataGrid.xmax,MapDraw.MapCorners.BoundBoxDataGrid.ymin);
      if (DEMGlb[MapDraw.DEMonMap].DEMheader.ElevUnits = euNanotesla) or (MapDraw.UsePercentiles) then begin
         MapDraw.MinMapElev := DEMGlb[MapDraw.DEMonMap].FindPercentileElev(MDDef.MinElevPercentile);
         MapDraw.MaxMapElev := DEMGlb[MapDraw.DEMonMap].FindPercentileElev(MDDef.MaxElevPercentile);
      end
      else begin
         if MDDef.AutoElevationReset then DEMGlb[MapDraw.DEMonMap].BoxAreaExtremeElevations(MapDraw.MapAreaDEMGridLimits,MapDraw.MinMapElev,MapDraw.MaxMapElev,AvgElev);
      end;
   end;
   MapDraw.MapDrawValid := false;
   FormResize(nil);
   MaxUpperLeftDrawMap(MapDraw.MapXSize,MapDraw.MapYSize);
   UnsubsetSpeedButton22.Enabled := true;
end;


procedure TMapForm.ResizeByPercentage(Percent : integer);
begin
   {$If Defined(RecordMapDraw) or Defined(RecordMapResize)} WriteLineToDebugFile('TMapForm.ResizeByPercentage ' + Caption + '  PC=' + IntToStr(Percent) + '  ' + MapDraw.MapSizeString); {$EndIf}
   if LockMaps then begin
      MessageToContinue('Maps locked for digitizing');
   end
   else begin
      if (Percent < -9) then begin
         Percent := 100;
         while round(0.0001 * (MapDraw.MapCorners.BoundBoxDataGrid.xmax - MapDraw.MapCorners.BoundBoxDataGrid.xmin) * (MapDraw.MapCorners.BoundBoxDataGrid.ymax - MapDraw.MapCorners.BoundBoxDataGrid.ymin) * sqr(Percent)) > 25000000 do begin
            dec(Percent,5);
         end;
         if (Percent < 100) then ReadDefault('Zoom factor to use',Percent);
      end;

      if MapDraw.ResizeMapByPercentage(Percent) then begin
         //FormResize(Nil);
         DoCompleteMapRedraw;
      end;
      {$If Defined(RecordMapDraw) or Defined(RecordMapResize)} WriteLineToDebugFile('TMapForm.ResizeByPercentage  PC=' + IntToStr(Percent) + '  ' + MapDraw.MapSizeString); {$EndIf}
   end;
end;


procedure TMapForm.SubsetAndZoomMap(NewX1,Newy1,NewX2,Newy2 : integer);
var
   NewCorners : CurScreenMapType;
begin
    {$If Defined(RecordSubsetZoom) or Defined(RecordMapResize)}  WriteLineToDebugFile('TMapForm.SubsetAndZoomMap in, desired NW: ' + MapDraw.ScreenToLatLongString(NewX1,NewY1) + '  SE: ' + MapDraw.ScreenToLatLongString(NewX2,NewY2)); {$EndIf}
    {$IfDef RecordMapResize} WriteLineToDebugFile('Subset and zoom, screen coords  ' + IntToStr(NewX1) + '/' + IntToStr(NewY1) + '  ' + IntToStr(NewX2) + '/' + IntToStr(NewY2) + '  ' + MapDraw.MapSizeString); {$EndIf}
    {$IfDef RecordMapResize} WriteLineToDebugFile('Subset and zoom, starting projected  ' + sfBoundBoxToString(MapDraw.MapCorners.BoundBoxProj,4) + '  ' + sfBoundBoxSizeToString(MapDraw.MapCorners.BoundBoxProj,2)); {$EndIf}
    {$IfDef RecordMapResize} WriteLineToDebugFile('Subset and zoom, starting geo  ' + sfBoundBoxToString(MapDraw.MapCorners.BoundBoxGeo,4) + '  ' + sfBoundBoxSizeToString(MapDraw.MapCorners.BoundBoxGeo,2)); {$EndIf}

     NewCorners := MapDraw.MapCorners;
     MapDraw.ScreenToProjectedCoords(NewX2,Newy2,NewCorners.BoundBoxProj.xMax, NewCorners.BoundBoxProj.YMin); //lower right
     MapDraw.ScreenToProjectedCoords(NewX1,Newy1,NewCorners.BoundBoxProj.xMin, NewCorners.BoundBoxProj.YMax); //upper left, must go second because these are used in ScreenToProjectedCoords

     {$If Defined(RecordSubsetZoom) or Defined(RecordMapResize)} WriteLineToDebugFile('Subset and zoom, desired projected  ' + sfBoundBoxToString(NewCorners.BoundBoxProj,4) + '  ' + sfBoundBoxSizeToString(NewCorners.BoundBoxProj,2)); {$EndIf}
     MapDraw.MapCorners := NewCorners;
     SubsetAndZoomMapFromProjectedBounds;
     {$IfDef Defined(RecordSubsetZoom) or Defined(RecordMapResize)} WriteLineToDebugFile('Subset and zoom, final projected  ' + sfBoundBoxToString(NewCorners.BoundBoxProj,4) + '  ' + sfBoundBoxSizeToString(NewCorners.BoundBoxProj,2)); {$EndIf}
     {$IfDef RecordMapResize} WriteLineToDebugFile('Subset and zoom, final geo  ' + sfBoundBoxToString(NewCorners.BoundBoxGeo,4) + '  ' + sfBoundBoxSizeToString(NewCorners.BoundBoxGeo,2)); {$EndIf}
end;


procedure TMapForm.SubsetAndZoomMapFromGeographicBounds(GeoBox : sfBoundBox;  Redraw : boolean = true);
begin
    MapDraw.LatLongDegreeToProjectedCoords(GeoBox.YMax,GeoBox.xmin,MapDraw.MapCorners.BoundBoxProj.xMin, MapDraw.MapCorners.BoundBoxProj.YMax);
    MapDraw.LatLongDegreeToProjectedCoords(GeoBox.YMin,GeoBox.xmax,MapDraw.MapCorners.BoundBoxProj.xMax, MapDraw.MapCorners.BoundBoxProj.YMin);
    SubsetAndZoomMapFromProjectedBounds(Redraw);
end;


procedure TMapForm.SubsetAndZoomMapFromProjectedBounds(Redraw : boolean = true);
begin
    MapDraw.ZeroTickInt;
    if (MapDraw.VectorIndex <> 0) then begin
       {$If Defined(RecordMapDraw) or Defined(RecordMapResize)} WriteLineToDebugFile('TMapForm.SubsetAndZoomMapFromProjectedBounds Resize vector map'); {$EndIf}
       MapDraw.PrimMapProj.FullWorld := false;
       {$If Defined(RecordMapDraw) or Defined(RecordMapResize)} WriteLineToDebugFile('new NW corner: ' + LatLongDegreeToString(MouseDownLat,MouseDownLong)+ ' SE corner: ' + LatLongDegreeToString(MouseUpLat,MouseUpLong)); {$EndIf}
    end;
    //else begin
        MapDraw.BoundingBoxProjToDataGrid;
        {$IfDef RecordMapResize} WriteLineToDebugFile('SubsetAndZoomMapFromProjectedBounds, desired DataGrid ' + sfBoundBoxToString(MapDraw.MapCorners.BoundBoxDataGrid,4) + '  ' + sfBoundBoxSizeToString(MapDraw.MapCorners.BoundBoxDataGrid,2)); {$EndIf}
        {$IfDef RecordMapResize} WriteLineToDebugFile('SubsetAndZoomMapFromProjectedBounds, after aspect check ' + sfBoundBoxToString(MapDraw.MapCorners.BoundBoxProj,4) + '  '  + MapDraw.MapSizeString); {$EndIf}
    //end;
    MapDraw.MapXSize := MDDef.DefaultMapXSize;
    MapDraw.MapYSize := MDDef.DefaultMapYSize;

//check if this is needed

    MapDraw.MapAspectCheck;

    {$IfDef RecordMapResize} WriteLineToDebugFile('SubsetAndZoomMapFromProjectedBounds, call form resize'); {$EndIf}
    //FormResize(Nil);
    {$IfDef RecordMapResize} WriteLineToDebugFile('SubsetAndZoomMapFromProjectedBounds, call DoCompleteMapRedraw'); {$EndIf}
    if Redraw then DoCompleteMapRedraw;
    FullMapSpeedButton.Enabled := true;
    {$IfDef RecordMapResize} WriteLineToDebugFile('SubsetAndZoomMapFromProjectedBounds, out'); {$EndIf}
end;


procedure TMapForm.BlowUpTheMap(BlowUp : float64);
begin
   MapDraw.MapXSize := round(MapDraw.MapXSize * BlowUp);
   MapDraw.MapYSize := round(MapDraw.MapYSize * BlowUp);
   Speedbutton22.Enabled := true;
   N11view1.Enabled := true;
   MapDraw.MapAspectCheck;
   //FormResize(Nil);
   DoCompleteMapRedraw;
end;


function TMapForm.CenterMapOnLatLong(Lat,Long : float64; HowZoom : tHowZoom) : boolean;
begin
   {$IfDef ReordMapResize} WriteLineToDebugFile('TMapForm.CenterMapOnLatLong ' + LatLongDegreeToString(Lat,Long)); {$EndIf}
   case HowZoom of
      hzZoomIn  : Result := MapDraw.RecenterMap(Lat,Long,50);
      hzNoZoom  : Result := MapDraw.RecenterMap(Lat,Long,100);
      hzZoomOut : Result := MapDraw.RecenterMap(Lat,Long,200);
      hzFullZoom : Result := MapDraw.RecenterMap(Lat,Long,-99);
   end;
   if Result then begin
      //FormResize(Nil);
      DoCompleteMapRedraw;
   end;
   {$IfDef ReordMapResize} WriteLineToDebugFile('TMapForm.CenterMapOnLatLong out'); {$EndIf}
end;


procedure TMapForm.MaxUpperLeftDrawMap(XSize : integer = 0; ysize : integer = 0);
begin
   {$IfDef RecordMapDraw} WriteLineToDebugFile('TMapForm.MaxUpperLeftDrawMap in, ' + MapTypeName(MapDraw.MapType)); {$EndIf}
   with MapDraw do begin
      if (ysize = 0) then ClientHeight := MDdef.DefaultMapYSize
      else ClientHeight := YSize + ToolAndPanelHeights;
      if (xsize = 0) then ClientWidth := MDdef.DefaultMapXSize
      else ClientWidth := xsize;
      MapXSize := XSize;
      MapYSize := YSize;
      {$IfDef RecordMapDraw} WriteLineToDebugFile('MaxUpperLeftDrawMap set size at ' + IntToStr(XSize) + ' by ' + IntToStr(YSize));  {$EndIf}
      Top := 0;
      Left := 0;
      MapDraw.ZeroTickInt;
      DoCompleteMapRedraw;
   end;
   {$IfDef RecordMapDraw} WriteLineToDebugFile('TMapForm.MaxUpperLeftDrawMap out, ' + MapTypeName(MapDraw.MapType)); {$EndIf}
end;


procedure TMapForm.RedrawMapForDataGrid(LeftGrid,TopGrid,RightGrid,BottomGrid : float64; xsize,ysize : integer);
begin
   MapDraw.MapCorners.BoundBoxDataGrid.xmin := LeftGrid;
   MapDraw.MapCorners.BoundBoxDataGrid.ymax := TopGrid;
   MapDraw.MapCorners.BoundBoxDataGrid.xmax := RightGrid;
   MapDraw.MapCorners.BoundBoxDataGrid.ymin := BottomGrid;
   if MapDraw.DEMMap then begin
       DEMGlb[MapDraw.DEMonMap].ClipDEMGrid(MapDraw.MapCorners.BoundBoxDataGrid.xmin,MapDraw.MapCorners.BoundBoxDataGrid.ymax);
       DEMGlb[MapDraw.DEMonMap].ClipDEMGrid(MapDraw.MapCorners.BoundBoxDataGrid.xmax,MapDraw.MapCorners.BoundBoxDataGrid.ymin);
   end;
   {$IfDef ExSat}
   {$Else}
      if ValidSatImage(MapDraw.SatOnMap) then begin
         SatImage[MapDraw.SATonMap].ClipSatGrid(SatImage[MapDraw.SatOnMap].BandForSize,MapDraw.MapCorners.BoundBoxDataGrid.xmin,MapDraw.MapCorners.BoundBoxDataGrid.ymax);
         SatImage[MapDraw.SATonMap].ClipSatGrid(SatImage[MapDraw.SatOnMap].BandForSize,MapDraw.MapCorners.BoundBoxDataGrid.xmax,MapDraw.MapCorners.BoundBoxDataGrid.ymin);
      end;
   {$EndIf}

   MapDraw.BoundingBoxDataGridToProj;
   MapDraw.BoundingBoxDataGridToGeo;

   {$IfDef RecordMapResize} WriteLineToDebugFile('TMapForm.RedrawMapForDataGrid, data grid' + sfBoundBoxToString(MapDraw.MapCorners.BoundBoxDataGrid,2)); {$EndIf}
   MapDraw.MapXSize := xsize;
   MapDraw.MapYSize := ysize;
   {$IfDef RecordMapResize} WriteLineToDebugFile('TMapForm.RedrawMapForDataGrid, ' + MapTypeName(MapDraw.MapType) + '  redraw ' + MapDraw.MapSizeString); {$EndIf}
   SizeIsCorrectThankYou := true;
   MapDraw.MapAspectCheck;
   CheckAndDrawNewCorners;
   SizeIsCorrectThankYou := false;
end;


procedure TMapForm.DrapeMapFromTwoGridCorners(xu1,yu1,xu4,yu4 : float64); {order is left,bottom,right,top}
var
   xs,ys : integer;


         procedure GetDisplayToCoverRegion(var xgl,ygl,xgr,ygr : float64);
         begin
            MinOfPairFirst(xgl,xgr);
            MinOfPairFirst(ygl,ygr);
            SatImage[MapDraw.SatOnMap].ClipSatGrid(SatImage[MapDraw.SatOnMap].BandForSize,xgl,ygl);
            SatImage[MapDraw.SatOnMap].ClipSatGrid(SatImage[MapDraw.SatOnMap].BandForSize,xgr,ygr);
            MapDraw.MapCorners.BoundBoxDataGrid.xmin := trunc(xgl);
            MapDraw.MapCorners.BoundBoxDataGrid.ymin := trunc(ygl);
         end;


begin
   {$IfDef RecordDrape} WriteLineToDebugFile('TMapForm.DrapeMapFromTwoGridCorners enter'); {$EndIf}
      if MapDraw.DEMMap or ValidSatImage(MapDraw.SatOnMap) then begin
         MinOfPairFirst(xu1,xu4);
         MinOfPairFirst(yu1,yu4);
         MapDraw.MapCorners.BoundBoxDataGrid.xmin := xu1;
         MapDraw.MapCorners.BoundBoxDataGrid.ymin := yu1;
         MapDraw.MapCorners.BoundBoxDataGrid.ymax := yu4;
         MapDraw.MapCorners.BoundBoxDataGrid.xmax := xu4;
         if MapDraw.DEMMap then begin
            DEMGLb[Mapdraw.DEMonMap].ClipDEMGrid(xu1,yu1);
            DEMGLb[Mapdraw.DEMonMap].ClipDEMGrid(xu4,yu4);
            xs := round(xu4 - xu1);
            ys := round(yu4 - yu1);
         end
         else begin
            {$IfDef ExSat}
            {$Else}
               GetDisplayToCoverRegion(xu4,yu4,xu1,yu1);
               xs := MapDraw.ColsDisplayed;
               ys := MapDraw.RowsDisplayed;
            {$EndIf}
         end;
      end;
      if abs(xs) > MDdef.MaxDrapeXSize then xs := MDdef.MaxDrapeXSize;
      if abs(ys) > MDdef.MaxDrapeYSize then ys := MDdef.MaxDrapeYSize;
      {$IfDef RecordDrape} WriteLineToDebugFile('Call MaxUpperLeft map size: ' + IntToStr(xs) + 'x' + IntToStr(ys));  {$EndIf}
      MaxUpperLeftDrawMap(xs,ys);
   {$IfDef RecordDrape} WriteLineToDebugFile('TMapForm.DrapeMapFromTwoGridCorners exit'); {$EndIf}
end;


