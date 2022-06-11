{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of ianMICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2015 Peter L. Guth   }
{____________________________________}


{ include file for demcoord }

procedure tDEMDataSet.GetStraightRouteLatLongDegree(Lat1,Long1,Lat2,Long2 : float64; StraightAlgorithm : tStraightAlgorithm; var NumPoints : integer; var Lats,Longs,dists : Petmath.bfarray32);
begin
   {$IfDef RecordLOSAlgorithm} WriteLineToDebugFile('tDEMDataSet.GetStraightRouteLatLongDegree');  {$EndIf}
   GetStraightRoute(true,Lat1,Long1,Lat2,Long2,StraightAlgorithm,NumPoints,Longs,Lats,dists);
end;


procedure tDEMDataSet.GetStraightRouteDEMGrid(Lat1,Long1,Lat2,Long2 : float64; StraightAlgorithm : tStraightAlgorithm;  var NumPoints : integer; var xgrids,ygrids,dists : Petmath.bfarray32);
var
   DeltaX,DeltaY,XStep,YStep,direction,
   i,err,x,y,x1,x2,y1,y2 : integer;
   SectLen,Bearing,
   fd,yg,xg,xg1,yg1,xg2,yg2,Heading : float64;
   GridCoords : tStringList;
   TStr : ShortString;

      procedure AssignValues(StringCount,ProfileCount : integer);
      begin
         TStr := GridCoords.Strings[StringCount];
         Val(ptCopy(TStr,1,12),Dists[ProfileCount],err);
         Val(ptCopy(TStr,13,12),XGrids[ProfileCount],err);
         Val(ptCopy(TStr,25,12),YGrids[ProfileCount],err);
      end;

begin
   {$IfDef RecordLOSAlgorithm} WriteLineToDebugFile('tDEMDataSet.GetStraightRouteDEMGrid enter'); {$EndIf}
   if MDDef.wf.LOSAlgorithm in [losMicrodemFractional,losMicrodemConstant] then begin
      {$IfDef RecordLOSAlgorithm} WriteLineToDebugFile('tDEMDataSet.GetStraightRouteDEMGrid, losMICRODEM');  {$EndIf}
      GetStraightRoute(false,Lat1,Long1,Lat2,Long2,StraightAlgorithm,NumPoints,xgrids,ygrids,dists);
   end
   else begin
      LatLongDegreeToDEMGrid(Lat1,Long1,xg1,yg1);
      LatLongDegreeToDEMGrid(Lat2,Long2,xg2,yg2);
      VincentyCalculateDistanceBearing(Lat1,Long1,Lat2,Long2,SectLen,Bearing);
      xgrids[0] := xg1;
      ygrids[0] := yg1;
      dists[0] := 0;
      if (MDDef.wf.LOSAlgorithm = losBresenham) then begin
         {$IfDef RecordLOSAlgorithm} WriteLineToDebugFile('tDEMDataSet.GetStraightRouteDEMGrid, losBresenham'); {$EndIf}
         x1 := round(xg1);
         y1 := round(yg1);
         x2 := round(xg2);
         y2 := round(yg2);
         NumPoints := 0;
         x := x1;
         y := y1;
         if (x1 > x2) then XStep := -1 else XStep := 1;
         if (y1 > y2) then YStep := -1 else YStep := 1;
         DeltaX := abs(x2 - x1);
         DeltaY := abs(y2 - y1);
         if DeltaX = 0 then direction := -1 else direction := 0;
         while not ((x = x2) and (y = y2)) do  begin
            inc(NumPoints);
            xgrids[NumPoints] := x;
            ygrids[NumPoints] := y;
            dists[NumPoints] := DistanceMetersBetweenPoints(xg1,yg1,x,y,Heading);

            if (direction < 0) then begin
               inc(y,YStep);
               inc(direction,DeltaX);
            end
            else  begin
               inc(x,XStep);
               dec(direction,DeltaY);
            end {if};
         end {while};
      end
      else if (MDDef.wf.LOSAlgorithm = losYoeli) then begin  //not optimized for speed
         {$IfDef RecordLOSAlgorithm} WriteLineToDebugFile('tDEMDataSet.GetStraightRouteDEMGrid, losYoeli'); {$EndIf}
         GridCoords := tStringList.Create;
         GridCoords.Sorted := true;
         if (yg2 > yg1) then begin
            y1 := trunc(yg1+1);
            y2 := trunc(yg2);
         end
         else begin
            y1 := trunc(yg2 + 1);
            y2 := trunc(yg1);
         end;
         if abs(yg2-yg1) > 0.001 then for y := y1 to y2 do begin
            fd := (y-yg1) / (yg2-yg1);
            xg := xg1 + ( fd ) * (xg2-xg1);
            GridCoords.Add(RealToString(fd*SectLen,12,2) + RealToString(Xg,12,4) + RealToString(Y,12,4));
         end;

         if (xg2 > xg1) then begin
            x1 := trunc(xg1+1);
            x2 := trunc(xg2);
         end
         else begin
            x1 := trunc(xg2 + 1);
            x2 := trunc(xg1);
         end;
         if (abs(xg2-xg1) > 0.001) then for x := x1 to x2 do begin
            fd := (x-xg1) / (xg2-xg1);
            yg := yg1 + ( fd ) * (yg2-yg1);
            GridCoords.Add(RealToString(fd*SectLen,12,2) + RealToString(X,12,4) + RealToString(YG,12,4));
         end;

         NumPoints := GridCoords.Count;
         for i := 1 to pred(NumPoints) do AssignValues(pred(i),i);

         {$IfDef RecordLOSAlgorithm} WriteStringListToDebugFile(GridCoords); {$EndIf}

         GridCoords.Free;
      end;
   end;
   {$IfDef RecordFullStraightRoute}
   for i := 0 to NumPoints do WriteLineToDebugFile(IntToStr(i) + RealToString(xgrids[i],8,1) + RealToString(ygrids[i],8,1) + RealToString(dists[i],12,1));
   {$EndIf}
end;


procedure tDEMDataSet.GetStraightRoute(LatLong : boolean; Lat1,Long1,Lat2,Long2 : float64; StraightAlgorithm : tStraightAlgorithm; var NumPoints : integer; var xgrids,ygrids,dists : Petmath.bfarray32);
var
   I : integer;
   InBounds : boolean;
   xUTM1,xUTM2,yUTM1,yUTM2,
   Lat,Long,Lat4,Long4,
   dx,dy,dLat,dLong,x,y,
   dDistance,FullDistance,Bearing : float64;
begin
   {$IfDef RecordLOSAlgorithm} WriteLineToDebugFile('tDEMDataSet.GetStraightRoute enter');  {$EndIf}

   VincentyCalculateDistanceBearing(Lat1,Long1,Lat2,Long2,FullDistance,Bearing);
   dDistance := FullDistance / NumPoints;

   for i := 0 to NumPoints do begin
      Dists[i] := dDistance * i;
      VincentyPointAtDistanceBearing(Lat1,Long1,Dists[i],Bearing,Lat,Long);
      if LatLong then  begin
         XGrids[i] := Long;
         YGrids[i] := Lat;
      end
      else LatLongDegreeToDEMGrid(Lat,Long,xgrids[i],ygrids[i]);
   end;


(*
   if (StraightAlgorithm = saLatLong) then begin
      dLat := (Lat2-Lat1) / NumPoints;
      dLong := (Long2-Long1) / NumPoints;
      for i := 0 to NumPoints do begin
         Lat4 := Lat1+ i * dLat;
         Long4 := Long1 + i * dLong;
         if LatLong then begin
            XGrids[i] := Long4;
            YGrids[i] := Lat4;
         end
         else LatLongDegreeToDEMGrid(Lat4,Long4,xgrids[i],ygrids[i]);
         Dists[i] := i * dDistance;
      end;
   end
   else if ((StraightAlgorithm = saDEMGrid) and (DEMheader.DEMUsed = ArcSecDEM)) then begin
      LatLongDegreeToDEMGrid(Lat1,Long1,xgrids[0],ygrids[0]);
      LatLongDegreeToDEMGrid(Lat2,Long2,xgrids[NumPoints],ygrids[NumPoints]);

      dx := (xgrids[NumPoints]-xgrids[0]) / NumPoints;
      dy := (ygrids[NumPoints]-ygrids[0]) / NumPoints;

      for i := 0 to NumPoints do begin
         XGrids[i] := XGrids[0] + i * dx;
         YGrids[i] := YGrids[0] + i * dy;
         if LatLong then DEMGridToLatLongDegree( XGrids[i],YGrids[i], YGrids[i], XGrids[i]);
         Dists[i] := i * dDistance;
      end;
   end
   else if (StraightAlgorithm = saUTM)
         or ((StraightAlgorithm = saSmart) and (FullDistance < MDDef.wf.SmartSwitchOver) and (Lat1 > 0) and (Lat2 > 0))
         or ((StraightAlgorithm = saDEMGrid) and (DEMheader.DEMUsed = UTMBasedDEM)) then  begin
      DEMMapProjection.LatLongDegreetoUTM(Lat1,Long1,XUTM1,YUTM1);
      DEMMapProjection.LatLongDegreeToUTM(Lat2,Long2,XUTM2,YUTM2);
      dx := (xutm2-xutm1) / NumPoints;
      dy := (yutm2-yutm1) / NumPoints;
      dDistance := sqrt( sqr(xutm2-xutm1) + sqr(yutm2-yutm1)) / NumPoints;

      for i := 0 to NumPoints do begin
         x := xutm1 + i * dx;
         y := yutm1 + i * dy;
         if LatLong then DEMMapProjection.UTMtoLatLongDegree(x,y,ygrids[i],xgrids[i])
         else UTMtoDEMGrid(x,y,xgrids[i],ygrids[i],InBounds);
         Dists[i] := i * dDistance;
      end;
   end
   else {if (StraightAlgorithm = saVincenty) or ((StraightAlgorithm = saSmart) and (FullDistance > MDDef.wf.SmartSwitchOver)) then} begin
      for i := 0 to NumPoints do begin
         Dists[i] := dDistance * i;
         VincentyPointAtDistanceBearing(Lat1,Long1,Dists[i],Bearing,Lat,Long);
         if LatLong then  begin
            XGrids[i] := Long;
            YGrids[i] := Lat;
         end
         else LatLongDegreeToDEMGrid(Lat,Long,xgrids[i],ygrids[i]);
      end;
   end;
*)
end;
