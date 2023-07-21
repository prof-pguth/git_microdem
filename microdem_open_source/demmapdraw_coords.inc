
{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program       }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2023 Peter L. Guth   }
{------------------------------------}
{ include file for demmapdraw        }
{____________________________________}


procedure TMapDraw.LatLongDegreeToDataGrid(Lat,Long : float64; var xg,yg : float32);
begin
   if DEMMap then begin
      DEMGlb[DEMonMap].LatLongDegreeToDEMGrid(Lat,Long,xg,yg);
   end
   else if ValidSatOnMap then begin
      SatImage[SatOnMap].LatLongDegreeToSatGrid(SatImage[SatOnMap].BandForSize,Lat,Long,xg,yg);
   end;
end;

procedure TMapDraw.LatLongDegreeToDataGrid(Lat,Long : float64; var xg,yg : float64);
var
   xg32,yg32 : float32;
begin
   if DEMMap then begin
      DEMGlb[DEMonMap].LatLongDegreeToDEMGrid(Lat,Long,xg32,yg32);
   end
   else if ValidSatOnMap then begin
      SatImage[SatOnMap].LatLongDegreeToSatGrid(SatImage[SatOnMap].BandForSize,Lat,Long,xg32,yg32);
   end;
   xg := xg32;
   yg := yg32;
end;

procedure tMapDraw.LatLongDegreeToScreen(Lat,Long : float64; var XPic,YPic : integer);
var
   xg,yg : float64;
begin
   XPic := MaxSmallInt;
   YPic := MaxSmallInt;
   if MapDrawValid then begin
      if (BasicProjection = bpLatLong) then begin
         if Long0_360 and (Long < 0) then Long := Long + 360;
         XPic := Round( (Long - MapCorners.BoundBoxProj.xmin) / MapCorners.ProjDX);
         YPic := Round( (MapCorners.BoundBoxProj.ymax - Lat) / MapCorners.ProjDY);
      end
      else begin
         PrimMapProj.ForwardProjectDegrees(Lat,Long,xg,yg,false);
         ProjectedCoordsToScreen(xg,yg,XPic,YPic);
      end;
   end;
end;


procedure TMapDraw.DataGridToProjectedCoords(X,Y : float32; var XProj,YProj : float64);
begin
   if DEMMap then begin
      xproj := DEMGlb[DEMonMap].ComputeSWCornerX  + x * DEMGlb[DEMonMap].DEMHeader.DEMxSpacing;
      yproj := DEMGlb[DEMonMap].ComputeSWCornerY  + y * DEMGlb[DEMonMap].DEMheader.DEMySpacing;
   end
   else if ValidSatOnMap then begin
      xproj := SatImage[SatOnMap].RegVars.UpLeftX + x * SatImage[SatOnMap].BandXSpace[SatImage[SatOnMap].BandForSize];
      yproj := SatImage[SatOnMap].RegVars.UpLeftY - y * SatImage[SatOnMap].BandYSpace[SatImage[SatOnMap].BandForSize];
   end;
end;


procedure TMapDraw.ProjectedCoordsToDataGrid(X,Y : float64; var XProj,YProj : float64);
begin
   if DEMMap then begin
      xproj := (x - DEMGlb[DEMonMap].ComputeSWCornerX) / DEMGlb[DEMonMap].DEMHeader.DEMxSpacing;
      yproj := (y - DEMGlb[DEMonMap].ComputeSWCornerY) / DEMGlb[DEMonMap].DEMheader.DEMySpacing;
   end
   else if ValidSatOnMap then begin
      xproj := round((x - SatImage[SatOnMap].RegVars.UpLeftX) / SatImage[SatOnMap].BandXSpace[SatImage[SatOnMap].BandForSize]);
      yproj := round((SatImage[SatOnMap].RegVars.UpLeftY - y) / SatImage[SatOnMap].BandYSpace[SatImage[SatOnMap].BandForSize]);
   end;
end;

procedure TMapDraw.ProjectedCoordsToScreen(XProj,YProj : float64; var XPic,YPic : integer);
begin
   XPic := Round( (XProj - MapCorners.BoundBoxProj.xmin) / MapCorners.ProjDX);
   YPic := Round( (MapCorners.BoundBoxProj.ymax - YProj) / MapCorners.ProjDY);
end;


procedure TMapDraw.ScreenToProjectedCoords(XPic,YPic : integer; var XProj,YProj : float64);
begin
   xProj := MapCorners.BoundBoxProj.xmin + xpic * MapCorners.ProjDX;
   yProj := MapCorners.BoundBoxProj.ymax - ypic * MapCorners.ProjDY;
end;


function TMapDraw.ScreenLocStr(x,y : integer) : shortstring;
var
   xg,yg : float32;
   Lat,Long : float64;
begin
   Result := '';
   if (VectorIndex = 0) then begin
      ScreenToDataGrid(x,y,xg,yg);
      DataGridToLatLongDegree(xg,yg,Lat,Long);
      Result := PrimMapProj.PreferLocationString(Lat,Long);
      {$IfDef RecordConversion} WriteLineToDebugFile('TMapForm.ScreenLocationString, screen=' + x.ToString + '/' + y.ToString + '   grid=' + RealToString(xg,-18,-6) + '/' + RealToString(yg,-18,-6) + ' ' + Result); {$EndIf}
   end;
end;


procedure TMapDraw.ScreenToLatLongDegree(XPic,YPic : integer; var Lat,Long : float64);
var
   xp,yp : float64;
begin
   if (BasicProjection = bpLatLong) then begin
      Long := MapCorners.BoundBoxProj.xmin + xpic * MapCorners.ProjDX;
      Lat  := MapCorners.BoundBoxProj.ymax - ypic * MapCorners.ProjDY;
   end
   else begin
      ScreenToProjectedCoords(xpic,ypic,xp,yp);
      PrimMapProj.InverseProjectDegrees(xp,yp,Lat,Long);
   end;
end;

procedure TMapDraw.ScreenToUTM(xpic,ypic : integer; var xutm,yutm : float64; ForceSlow : boolean = false);
var
   Lat,Long : float64;
begin
   if IsThisMapUTM and (not ForceSlow) then begin
      xutm := MapCorners.BoundBoxUTM.xmin + xpic * MapCorners.ProjDX;
      yutm := MapCorners.BoundBoxUTM.ymax - ypic * MapCorners.ProjDY;
   end
   else begin
      ScreenToLatLongDegree(xpic,ypic,Lat,Long);
      WGS84DatumConstants.LatLongDegreeToUTM(Lat,Long,xutm,yutm);
   end;
end;


procedure TMapDraw.ScreenToDEMGrid(xpic,ypic : integer; var XGrid,YGrid : float32);
var
   Lat,Long : float64;
begin
   if DEMMap then begin
      ScreenToDataGrid(xpic,ypic,XGrid,YGrid);
   end
   else if ValidDEM(DEMonMap) then  begin
      ScreenToLatLongDegree(xpic,ypic,Lat,long);
      DEMGlb[DEMonMap].LatLongDegreeToDEMGrid(Lat,Long,xgrid,ygrid);
   end;
end;


procedure TMapDraw.ScreenToDEMGrid(xpic,ypic : integer; var XGrid,YGrid : integer);
var
   xf,yf : float32;
begin
   ScreenToDEMGrid(xpic,ypic,xf,yf);
   xgrid := round(xf);
   ygrid := round(yf);
end;


function TMapDraw.ScreenToDataGrid(XPic,YPic : integer; var XGrid,YGrid : float32) : boolean;
begin
   Result := true;
   if DEMMap then begin
      XGrid := MapCorners.BoundBoxDataGrid.xmin + (MapCorners.GridDX * XPic);
      YGrid := MapCorners.BoundBoxDataGrid.ymax - (MapCorners.GridDY * YPic);
   end
   {$IfDef ExSat}
   {$Else}
      else if ValidSatOnMap then begin
         XGrid := MapCorners.BoundBoxDataGrid.xmin + (XPic / MapXSize * ColsDisplayed);
         YGrid := MapCorners.BoundBoxDataGrid.ymin + (YPic / MapYSize * RowsDisplayed);
      end
   {$EndIf}
   else Result := false;
end {proc ScreenToDataGrid};


function TMapDraw.ScreenToSatelliteDataGrid(Band, XPic,YPic : integer; var XGrid,YGrid : integer) : boolean;
var
   xproj,yproj : float64;
begin
   Result := true;
   ScreenToProjectedCoords(xpic,ypic,xproj,yproj);
   xgrid := round((xproj - SatImage[SatOnMap].RegVars.UpLeftX) / SatImage[SatOnMap].BandXSpace[Band]);
   ygrid := round((SatImage[SatOnMap].RegVars.UpLeftY - yProj) / SatImage[SatOnMap].BandYSpace[Band]);
   Result := SatImage[SatOnMap].SatGridInDataSet(Band,XGrid,YGrid);
end {proc ScreenToDataGrid};


function tMapDraw.ScreenPositionToLatLongDegreeString(x,y : integer) : ShortString;
var
   Lat,Long : float64;
begin
    ScreenToLatLongDegree(x,y,Lat,Long);
    Result := LatLongDegreeToString(Lat,Long,MDDef.OutPutLatLongMethod);
end;


procedure tMapDraw.DataGridToScreen(XGrid,YGrid : float32; var XPic,YPic : integer; OutsideData : boolean = false);
begin
   XPic := -9999;
   YPic := -9999;
   if DEMMap then begin
      {$IfDef RecordGridToScreen} WriteLineToDebugFile('Grids: ' + realtoString(xgrid,8,-2) + ' &' + RealToString(ygrid,8,-2)); {$EndIf}
      XPic := Round( (XGrid - MapCorners.BoundBoxDataGrid.xmin) / MapCorners.GridDX);
      YPic := Round( (MapCorners.BoundBoxDataGrid.ymax - YGrid) / MapCorners.GridDY);
   end
   {$IfDef ExSat}
   {$Else}
   else if ValidSatOnMap then begin
      XPic := Round( (XGrid - MapCorners.BoundBoxDataGrid.xmin) / MapCorners.GridDX);   //   / pred(SatView.ColsDisplayed) * MapXSize);
      YPic := Round( (YGrid - MapCorners.BoundBoxDataGrid.ymin) / MapCorners.GridDY);   //   / pred(SatView.RowsDisplayed) * MapYSize);
   end;
   {$EndIf}
end {GridToScreen};


procedure TMapDraw.LatLongRadiansToScreen(Lat,Long : float64; var XPic,YPic : integer);
begin
   LatLongDegreeToScreen(Lat / DegToRad, Long / DegToRad, XPic,YPic);
end;

procedure TMapDraw.DEMGridToScreen(xg,yg : float32; var xs,ys : integer);
var
   Lat,Long : float64;
begin
   if ValidDEMonMap then begin
      if DEMMap then begin
         xs := round( (xg - MapCorners.BoundBoxDataGrid.xmin) / MapCorners.GridDX);
         ys := round( (MapCorners.BoundBoxDataGrid.ymax - yg) / MapCorners.GridDY);
      end
      else begin
         DEMGlb[DEMonMap].DEMGridToLatLongDegree(xg,yg,Lat,Long);
         LatLongDegreetoScreen(Lat,Long,xs,ys);
      end;
   end;
end;


procedure TMapDraw.UTMToScreen(XUTM,YUTM : float64; var XPic,YPic : integer; ReallyUTM : boolean = false);
var
   Lat,Long : float64;
begin
   if IsThisMapUTM then begin
      XPic := Round((XUTM - MapCorners.BoundBoxUTM.xmin) / MapCorners.ProjDX);
      YPic := Round((MapCorners.BoundBoxUTM.ymax - YUTM) / MapCorners.ProjDY);
   end
   else begin
       WGS84DatumConstants.UTMtoLatLongDegree(xutm,yutm,Lat,Long);
       LatLongDegreeToScreen(Lat,Long,xpic,ypic);
   end;
end;

procedure TMapDraw.UTMToScreen(XUTM,YUTM : float32; var XPic,YPic : integer; ReallyUTM : boolean = false);
var
   Lat,Long : float64;
begin
   if IsThisMapUTM then begin
      XPic := Round((XUTM - MapCorners.BoundBoxUTM.xmin) / MapCorners.ProjDX);
      YPic := Round((MapCorners.BoundBoxUTM.ymax - YUTM) / MapCorners.ProjDY);
   end
   else begin
       WGS84DatumConstants.UTMtoLatLongDegree(xutm,yutm,Lat,Long);
       LatLongDegreeToScreen(Lat,Long,xpic,ypic);
   end;
end;


procedure TMapDraw.MGRSToScreen(MGRS : shortString; var XPic,YPic : integer);
var
   Lat,Long : float64;
begin
   PrimMapProj.MGRStoLatLong(MGRS,Lat,Long);
   LatLongDegreeToScreen(Lat,Long,XPic,YPic);
end;


procedure tMapDraw.ShowMapGridLimits(Where : shortString);
begin
    {$IfDef RecordProblems}
       WriteLineToDebugFile(Where);
       WriteLineToDebugFile('  x grid range: ' + RealToString(MapCorners.BoundBoxDataGrid.xmin,-18,2) + '/' + RealToString(MapCorners.BoundBoxDataGrid.xmax,-18,2) +
           '  cols=' + IntToStr(Round(MapCorners.BoundBoxDataGrid.xmax - MapCorners.BoundBoxDataGrid.xmin)));
       WriteLineToDebugFile('  y grid range: ' + RealToString(MapCorners.BoundBoxDataGrid.ymin,-18,2) + '/' + RealToString(MapCorners.BoundBoxDataGrid.ymax,-18,2)  +
           '  rows=' + IntToStr(Round(MapCorners.BoundBoxDataGrid.ymax - MapCorners.BoundBoxDataGrid.ymin)));
   {$EndIf}
end;


function tMapDraw.MapLimits : tGridLimits;
begin
   Result.XGridLow := 0;
   Result.YGridLow := 0;
   Result.XGridHigh := pred(MapXSize);
   Result.YGridHigh := pred(MapYSize);
end;

function tMapDraw.SatLimits : tGridLimits;
begin
   Result.XGridLow := round(MapCorners.BoundBoxDataGrid.xmin);
   Result.XGridHigh := round(MapCorners.BoundBoxDataGrid.xmax);
   Result.YGridLow := round(MapCorners.BoundBoxDataGrid.ymax);
   Result.YGridHigh := round(MapCorners.BoundBoxDataGrid.ymin);
end;


procedure tMapDraw.LatLongDegreeToUTM(Lat,Long : float64; var xutm,yutm : float64);
begin
   PrimMapProj.LatLongDegreeToUTM(Lat,Long,xutm,yutm);
end;


procedure tMapDraw.UTMtoLatLongDegree(xutm,yutm : float64; var Lat,Long : float64);
begin
   PrimMapProj.UTMtoLatLongDegree(xutm,yutm,Lat,Long);
end;


procedure tMapDraw.FindMapUTMLimits;
var
   x,y : array[1..6] of float64;
begin
    if (VectorIndex = 0) or (ZoomableVectorMap) or (ScreenPixelSize < 1000) then begin
       if (PrimMapProj.Pname = UTMellipsoidal) then begin
          MapCorners.BoundBoxUTM := MapCorners.BoundBoxProj;
       end
       else begin
          PrimMapProj.LatLongDegreeToUTM(MapCorners.BoundBoxGeo.ymax,MapCorners.BoundBoxGeo.xmin,x[1],y[1]);
          PrimMapProj.LatLongDegreeToUTM(MapCorners.BoundBoxGeo.ymax,0.5*(MapCorners.BoundBoxGeo.xmin+MapCorners.BoundBoxGeo.xmax),x[2],y[2]);
          PrimMapProj.LatLongDegreeToUTM(MapCorners.BoundBoxGeo.ymax,MapCorners.BoundBoxGeo.xmax,x[3],y[3]);
          PrimMapProj.LatLongDegreeToUTM(MapCorners.BoundBoxGeo.ymin,MapCorners.BoundBoxGeo.xmin,x[4],y[4]);
          PrimMapProj.LatLongDegreeToUTM(MapCorners.BoundBoxGeo.ymin,0.5*(MapCorners.BoundBoxGeo.xmin+MapCorners.BoundBoxGeo.xmax),x[5],y[5]);
          PrimMapProj.LatLongDegreeToUTM(MapCorners.BoundBoxGeo.ymin,MapCorners.BoundBoxGeo.xmax,x[6],y[6]);
          MapCorners.BoundBoxUTM.xmin := Petmath.MinInArray(6,x);
          MapCorners.BoundBoxUTM.xmax := Petmath.MaxInArray(6,x);
          MapCorners.BoundBoxUTM.ymin := Petmath.MinInArray(6,y);
          MapCorners.BoundBoxUTM.ymax := Petmath.MaxInArray(6,y);
       end;
   end
   else begin
      MapCorners.BoundBoxUTM.ymin := -9999;
      MapCorners.BoundBoxUTM.ymax := -9999;
      MapCorners.BoundBoxUTM.xmin := -9999;
      MapCorners.BoundBoxUTM.xmax := -9999;
   end;
end;


procedure tMapDraw.ScreenBoxToLatLongMinMax(xlo,ylo,xhi,yhi : integer; var CurScrLatLow,CurScrLongLow,CurScrLatHigh,CurScrLongHigh : float64);
var
   Lats,Longs : array[1..6] of float64;
   {$IfDef RecordMapLimits}
      i : integer;
      xutm,yutm : float64;
   {$EndIf}
begin
   {$IfDef RecordMapLimits} WriteLineToDebugFile('tMapDraw.ScreenBoxToLatLongMinMax in, Box x range: ' + IntToStr(xlo) + ' to ' + intToStr(xhi) + '   Box y range: ' + IntToStr(ylo) + ' to ' + intToStr(yhi)); {$EndIf}

   ScreenToLatLongDegree(xlo,ylo,Lats[1],longs[1]);      //NW corner
   ScreenToLatLongDegree((xlo+xhi) div 2,ylo,Lats[5],longs[5]);  //top center
   ScreenToLatLongDegree(xhi,ylo,Lats[4],longs[4]);  //NE corner
   if longs[4] < longs[5] then longs[2] := 179.99;

   {$IfDef RecordMapLimits} WriteLineToDebugFile('Top: ' + LatLongDegreeToString(Lats[1],Longs[1]) + '   ' + LatLongDegreeToString(Lats[5],Longs[5]) + '   ' +  LatLongDegreeToString(Lats[4],Longs[4]) ); {$EndIf}

   ScreenToLatLongDegree(xlo,yhi,Lats[3],longs[3]); //SW corner
   ScreenToLatLongDegree((xlo+xhi) div 2,yhi,Lats[6],longs[6]);   //bottom center
   ScreenToLatLongDegree(xhi,yhi,Lats[2],longs[2]);   //SE corner
   if longs[2] < longs[6] then longs[2] := 179.99;

   {$IfDef RecordMapLimits}
      WriteLineToDebugFile('Bot: ' + LatLongDegreeToString(Lats[2],Longs[2]) + '   ' + LatLongDegreeToString(Lats[6],Longs[6]) + '   ' + LatLongDegreeToString(Lats[3],Longs[3]) );
      for i := 1 to 6 do  WriteLineToDebugFile(IntToStr(i) + '   ' + LatLongDegreeToString(Lats[i],Longs[i]) );
      for i := 1 to 6 do begin
         PrimaryMapDatum.ForwardProjectDegrees(Lats[i],Longs[i],xutm,yutm);
         WriteLineToDebugFile(IntToStr(i) + '   ' + RealToString(xutm,12,-2) + RealToString(yutm,12,-2) );
      end;
   {$EndIf}

   HeapSort(6,Lats);
   CurScrLatHigh := Lats[6];
   CurScrLatLow := Lats[1];
   HeapSort(6,Longs);
   CurScrLongHigh := Longs[6];
   CurScrLongLow := Longs[1];
   {$IfDef RecordMapLimits} WriteLineToDebugFile('Lat range:  ' + RealToString(CurScrLatLow,12,4) +  RealToString(CurScrLatHigh,12,4) + '  Long range: ' + RealToString(CurScrLongLow,12,4) +  RealToString(CurScrLongHigh,12,4)); {$EndIf}
end;


procedure TMapDraw.DataGridToLatLongDegree(xgrid,ygrid : float32; var lat,long : float64);
begin
   if (VectorIndex <> 0) then begin
   end
   else if DEMMap then begin
      DEMGlb[DEMonMap].DEMGridToLatLongDegree(XGrid,YGrid,Lat,Long);
   end
   {$IfDef ExSat}
   {$Else}
      else if ValidSatOnMap then begin
        SatImage[SatOnMap].SatGridToLatLongDegree(SatImage[SatOnMap].BandForSize,XGrid,YGrid,Lat,Long);
      end;
   {$EndIf}
end;


procedure tMapDraw.LatLongDegreeToProjectedCoords(Lat,Long : float64; var XProj,YProj : float64);
begin
   XProj := -9999;
   YProj := -9999;
   if not MapDrawValid then exit;
   if (Long > 180) then Long := Long - 360;

   if (Long > MapCorners.BoundBoxGeo.xmin + 360) then Long := Long - 360
   else if (Long < MapCorners.BoundBoxGeo.xmin - 5) then Long := Long + 360;

   PrimMapProj.ForwardProjectDegrees(Lat,Long,XProj,YProj,false);
end;


procedure TMapDraw.MapGridToDEMGrid(xg,yg : float32; var xg1,yg1 : float32);
var
   Lat,Long : float64;
begin
   if ValidDEMonMap then begin
      if DEMMap then begin
         xg1 := xg;
         yg1 := yg;
      end
      {$IfDef ExSat}
      {$Else}
         else if ValidSatOnMap then begin
            SatImage[SatOnMap].SatGridToLatLongDegree(SatImage[SatOnMap].BandForSize,xg,yg,Lat,Long);
            DEMGlb[DEMonMap].LatLongDegreetoDEMGrid(Lat,Long,xg1,yg1);
         end;
      {$EndIf}
   end
   else begin
      xg1 := -9999;
      yg1 := -9999;
   end;
end;


function TMapDraw.ProjectedCoordinatesStringFromScreen(x,y : integer; LongForm : boolean = true) : shortString;
var
   Lat,Long, XProj,YProj : float64;
begin
    if ValidProjectedCoordinates then begin
       ScreenToProjectedCoords(X,Y, XProj,YProj);
       Result := 'x=' + RealToString(xproj,-18,1) + '    y=' + RealToString(yproj,-18,1);
    end
    else begin
       ScreenToLatLongDegree(x,y,Lat,Long);
       Result := ProjectedCoordinatesStringFromLatLong(lat,long,LongForm);
    end;
end;


function TMapDraw.ProjectedCoordinatesStringFromLatLong(lat,long : float64; LongForm : boolean = true) : shortString;

      function AssembleString(mp : tMapProjection) : ShortString;
      var
         xproj,yproj : float64;
      begin
         if (mp.PName in [AlbersEqAreaConicalEllipsoid,MercatorEllipsoid,PolarStereographicEllipsoidal,LambertConformalConicEllipse,
             UK_OS,Finn_GK,GeneralTransverseMercator,LamAzEqAreaEllipsoidal,IrishGrid]) then begin
                 mp.ForwardProjectDegrees(Lat,Long,xproj,yproj);
                 Result := 'x=' + RealToString(xproj,-18,1) + '  y=' + RealToString(yproj,-18,1);
                 if LongForm then Result := mp.GetProjectionName + ' Projected meters ' + Result;
          end;
      end;

begin
    Result := '';
    if ValidProjectedCoordinates then Result := AssembleString(PrimMapProj);
end;





