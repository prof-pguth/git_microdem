{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2026 Peter L. Guth  }
{----------------------------------------}
{     include file for demmapdraw        }
{________________________________________}


   procedure tMapDraw.DrawTissotIndicatrix(Canvas : tCanvas; lat,long : float64);
      var
         LastY,LastX,i,x1,y1,xp : integer;
         Maph,Mapk,Lat2,Long2 : float64;
         Prime : boolean;
         TStr : shortstring;
      begin
         //{$IfDef RecordTissot} WriteLineToDebugFile('DrawTissotIndicatrix at ' + LatLongDegreeToString(Lat,Long) + '   Map pixel size=' + RealToString(ScreenPixelSize,-18,-4)); {$EndIf}
         Canvas.Pen.Color := ConvertPlatformColorToTColor(MDDef.TissotColor);
         Canvas.Pen.Width := MDDef.TissotLineWidth;
         Canvas.Brush.Style := bsClear;

         i := 0;
         while (i <= 360) do begin
            VincentyPointAtDistanceBearing(Lat,Long,MDDef.TissotRadius*ScreenPixelSize,i,Lat2,Long2);
            LatLongDegreeToScreen(Lat2,Long2,x1,y1);
            if (i = 0) or (abs(LastX - x1) > 100) or (abs(LastY - y1) > 100) then Canvas.MoveTo(x1,y1) else Canvas.LineTo(x1,y1);
            LastX := x1;
            LastY := y1;
            inc(i,2);
         end;

         if MDDef.ShowTissotHK then begin
            LatLongDegreeToScreen(Lat,long,x1,y1);
            ScreenSymbol(Canvas,x1,y1,Cross,5,claRed);
            xp := 15;
            GetMapScaleFactor(Lat,Long,Maph,Mapk,Prime);
            if Prime then TStr := '''=' else TStr := '=';
            Canvas.Font.Style := [fsBold];
            Canvas.Brush.Style := bsSolid;
            Canvas.TextOut(x1+xp,y1-15,'h' + TStr + RealToString(MapH,-12,MDDef.TissotHKdecimals));
            Canvas.TextOut(x1+xp,y1+5,'k' + TStr + RealToString(MapK,-12,MDDef.TissotHKdecimals));
            Canvas.Font.Style := [];
          end;
      end;


   procedure tMapDraw.DrawTissotIndicatrixOverlay(var Bitmap : tMyBitmap);
   var
      lat,long,dlat : float64;
      x,y : integer;
      OneDrawn : boolean;


      procedure DrawOnParallel(clat : float64; LongInt : float32 = -99);
      var
         long : float64;
      begin
         {$IfDef RecordTissot} WriteLineToDebugFile('DrawTissotIndicatrixOverlay along parallel= ' + RealToString(cLat,-12,-6) + '  spacing=' + RealToString(MDDef.TissotLongGridInc,-12,-2)); {$EndIf}
         long := -180;
         if LongInt < 0 then begin
            LongInt := MDDef.TissotLongGridInc;
         end
         else long := long + LongInt / 2;

         while (long < 180) do begin
             if (Long <= MapCorners.BoundBoxGeo.xmax) and (Long >= MapCorners.BoundBoxGeo.xmin) then begin
                DrawTissotIndicatrix(Bitmap.Canvas,clat,long);
                if LatLongOnScreen(cLat,Long) then OneDrawn := true;
             end;
             long := long + LongInt;
         end;
      end;


   begin
      if (PrimMapProj = Nil) then exit;
      if (not MDDef.TissotSpaceByPixels) then begin
         if (VectorIndex <> 0) and MDDef.SimpleTissotCylindrical and PrimMapProj.FullWorld and (PrimMapProj.Pname in [CylindricalEqualArea,MillerCylindrical,MercatorEllipsoid]) then begin
            {$IfDef RecordTissot} WriteLineToDebugFile('Cylindrical DrawOnParallelOption'); {$EndIf}
            DrawOnParallel(0,60);
            DrawOnParallel(30,60);
            DrawOnParallel(60,60);
            DrawOnParallel(-30,60);
            DrawOnParallel(-60,60);
         end
         else begin
            {$IfDef RecordTissot} WriteLineToDebugFile('Conic DrawOnParallelOption'); {$EndIf}
            if (PrimMapProj.Pname in [AlbersEqAreaConicalEllipsoid,LambertConformalConicEllipse]) then begin
               DrawOnParallel(PrimMapProj.Phi1 / DegToRad);
               DrawOnParallel(PrimMapProj.Phi2 / DegToRad);
               DrawOnParallel(0.5 * (PrimMapProj.Phi1 + PrimMapProj.Phi2) / DegToRad);
               dlat := 0.5 * (PrimMapProj.Phi2 - PrimMapProj.Phi1) / DegToRad;
               DrawOnParallel(PrimMapProj.Phi1 / DegToRad - dLat);
               DrawOnParallel(PrimMapProj.Phi1 / DegToRad - 2*dLat);
               DrawOnParallel(PrimMapProj.Phi2 / DegToRad + dLat);
               DrawOnParallel(PrimMapProj.Phi2 / DegToRad + 2*dLat);
            end
            else begin
               lat := -90;
               while (lat < 90) do begin
                  if (Lat <= MapCorners.BoundBoxGeo.ymax) and (Lat >= MapCorners.BoundBoxGeo.ymin) then begin
                     DrawOnParallel(Lat);
                  end;
                  lat := lat + MDDef.TissotLatGridInc;
               end;
            end;
         end;
      end;

      if MDDef.TissotSpaceByPixels or (not OneDrawn) then begin
         x := MDDef.TissotPixelSpacing div 2;
         while x <= MapXSize do begin
            y :=  MDDef.TissotPixelSpacing div 2;
            while y <= MapYSize do begin
               ScreenToLatLongDegree(x,y,Lat,Long);
               DrawTissotIndicatrix(Bitmap.Canvas,lat,long);
               inc(y,MDDef.TissotPixelSpacing);
            end;
            inc(x,MDDef.TissotPixelSpacing);
         end;
      end
   end;


