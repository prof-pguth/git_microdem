{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{----------------------------------------}
{ include file for demcoord              }
{________________________________________}


procedure tDEMDataSet.GetStraightRouteLatLongDegree(Lat1,Long1,Lat2,Long2 : float64; StraightAlgorithm : tStraightAlgorithm; var NumPoints : integer; var Lats,Longs,dists : bfarray32);
begin
   {$IfDef RecordLOSAlgorithm} WriteLineToDebugFile('tDEMDataSet.GetStraightRouteLatLongDegree'); {$EndIf}
   GetStraightRoute(true,Lat1,Long1,Lat2,Long2,StraightAlgorithm,NumPoints,Longs,Lats,dists);
end;


procedure tDEMDataSet.GetStraightRouteDEMGrid(Lat1,Long1,Lat2,Long2 : float64; StraightAlgorithm : tStraightAlgorithm;  var NumPoints : integer; var xgrids,ygrids,dists : bfarray32);
var
   err : integer;
   xg1,yg1,xg2,yg2,SectLen,Bearing : float64;
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
      {$IfDef RecordLOSAlgorithm} WriteLineToDebugFile('tDEMDataSet.GetStraightRouteDEMGrid, losMICRODEM'); {$EndIf}
      GetStraightRoute(false,Lat1,Long1,Lat2,Long2,StraightAlgorithm,NumPoints,xgrids,ygrids,dists);
   end
   else begin
      LatLongDegreeToDEMGrid(Lat1,Long1,xg1,yg1);
      LatLongDegreeToDEMGrid(Lat2,Long2,xg2,yg2);
      VincentyCalculateDistanceBearing(Lat1,Long1,Lat2,Long2,SectLen,Bearing);
      xgrids[0] := xg1;
      ygrids[0] := yg1;
      dists[0] := 0;
   end;
   {$IfDef RecordFullStraightRoute} for i := 0 to NumPoints do WriteLineToDebugFile(IntToStr(i) + RealToString(xgrids[i],8,1) + RealToString(ygrids[i],8,1) + RealToString(dists[i],12,1)); {$EndIf}
end;


procedure tDEMDataSet.GetStraightRouteLatLongWithElevs(Lat1,Long1,Lat2,Long2 : float64; var NumPoints : integer; var xgrids,ygrids,dists,elevs : bfarray32);
var
   I : integer;
   xg,yg : float64;
   Lat,Long,dDistance,FullDistance,Bearing : float64;
begin
   {$IfDef RecordLOSAlgorithm} WriteLineToDebugFile('tDEMDataSet.GetStraightRouteLatLongWithElevs enter'); {$EndIf}
   VincentyCalculateDistanceBearing(Lat1,Long1,Lat2,Long2,FullDistance,Bearing);
   dDistance := FullDistance / NumPoints;
   //Lats[0] := Lat;
   //Longs[0] := Long;
   //GetElevFromLatLongDegree(Lat,Long,Elevs[0]);
   for i := 0 to NumPoints do begin
      Dists[i] := dDistance * i;
      VincentyPointAtDistanceBearing(Lat1,Long1,Dists[i],Bearing,Lat,Long);
      GetElevFromLatLongDegree(Lat,Long,Elevs[i]);
      LatLongDegreeToDEMGrid(Lat,Long,xg,yg);
      xgrids[i] := xg;
      ygrids[i] := yg;
   end;
end;


procedure tDEMDataSet.GetStraightRoute(LatLong : boolean; Lat1,Long1,Lat2,Long2 : float64; StraightAlgorithm : tStraightAlgorithm; var NumPoints : integer; var xgrids,ygrids,dists : bfarray32);
var
   I : integer;
   xg,yg : float64;
   Lat,Long,dDistance,FullDistance,Bearing : float64;
begin
   {$IfDef RecordLOSAlgorithm} WriteLineToDebugFile('tDEMDataSet.GetStraightRoute enter'); {$EndIf}

   VincentyCalculateDistanceBearing(Lat1,Long1,Lat2,Long2,FullDistance,Bearing);
   dDistance := FullDistance / NumPoints;
   for i := 0 to NumPoints do begin
      Dists[i] := dDistance * i;
      VincentyPointAtDistanceBearing(Lat1,Long1,Dists[i],Bearing,Lat,Long);
      if LatLong then  begin
         XGrids[i] := Long;
         YGrids[i] := Lat;
      end
      else begin
         LatLongDegreeToDEMGrid(Lat,Long,xg,yg);
         xgrids[i] := xg;
         ygrids[i] := yg;
      end;
   end;

(*
   dDistance := FullDistance / NumPoints;
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
