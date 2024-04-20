{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program       }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2024 Peter L. Guth   }
{------------------------------------}
{ include file for demcoord          }
{____________________________________}


function tDEMDataSet.GridPointsIntervisible(xg1,yg1,ObsUp,xg2,yg2,TargetUp : float64; var Distance,BlockDistance : float64) : boolean;
   {elevations of observer (z1) and observed point (z2), above terrain (ground elevation + observer/target height)}
   {xg1 and yg1 are the coordinates in the data grid array of the observer}
   {xg2 and yg2 are the coordinates in the data grid array of the target}
   {MapForm is a map on which the results of the calculations can be shown; if it is NIL, no drawing}
var
   Lat1,Long1,Lat2,Long2 : float64;
begin
   DEMGridToLatLongDegree(xg1,yg1,Lat1,Long1);
   DEMGridToLatLongDegree(xg2,yg2,Lat2,Long2);
   Result := LatLongDegreePointsIntervisible(Lat1,Long1,ObsUp,Lat2,Long2,TargetUp,Distance,BlockDistance);
end;


procedure tDEMDataSet.HorizonBlocking(Lat,Long,AzimuthTrue,DistanceToGoOut,ObsUp : float64; var BlockAngle,BlockDist,BlockLat,BlockLong : float64; StraightAlgorithm : DemDefs.tStraightAlgorithm);
var
   i,PixLong : integer;
   Lat2,Long2 : float64;
   z : float32;
   fxgrids,fygrids,fdists,elevs : ^Petmath.bfarray32;
   VisPoints : array[0..MaxOnRay] of boolean;
begin
   GetElevFromLatLongDegree(Lat,Long,z);

   {$IfDef RecordHorizon}
      WriteLineToDebugFile('HorizonBlocking for ' + LatLongDegreeToString(Lat,Long) + ' z=' + RealToString(z,-12,1));
      WriteLineToDebugFile('ObsUp=' + RealToString(ObsUp,-12,1) + ' Azimuth (True)=' + RealToString(AzimuthTrue,-12,2) + ' Dist Out=' + RealToString(DistanceToGoOut,-12,1));
   {$EndIf}

   z := z + ObsUp;

   PixLong := round(DistanceToGoOut / (AverageSpace * MDDef.HorizonRadialMultiple));
   if (PixLong > PetMath.MaxFArrayPts) then PixLong := pred(PetMath.MaxFArrayPts);

   VincentyPointAtDistanceBearing(Lat,Long,DistanceToGoOut,AzimuthTrue,Lat2,Long2);
   new(fxgrids);
   new(fygrids);
   new(fdists);
   New(elevs);

   GetStraightRouteDEMGrid(Lat,Long,Lat2,Long2,StraightAlgorithm,PixLong,fxgrids^,fygrids^,fdists^);
   GetVisiblePoints(ObsUp,0,-89,89,true,true,PixLong,fxgrids^,fygrids^,fdists^,elevs^,VisPoints);
   BlockAngle := 89;
   BlockDist := 1;
   for i := PixLong downto 1 do begin
      if VisPoints[i] then begin
          BlockAngle := ArcTan(-(elevs^[0] + ObsUp - (elevs^[i] - DropEarthCurve(fDists^[i])) ) / fDists^[i]) / DegToRad;
          DEMGridToLatLongDegree(fxgrids^[i],fygrids^[i],BlockLat,BlockLong);
          BlockDist := fDists^[i];
         {$IfDef RecordHorizon}
            WriteLineToDebugFile(' elevs^[0] =' + RealToString(elevs^[0] ,-12,2) + ' elevs^[i]=' + RealToString(elevs^[i],-12,2));
            WriteLineToDebugFile(' fDists^[i]=' + RealToString(fDists^[i],-12,2)) + ' BlockAngle=' + RealToString(BlockAngle,-12,2) + ' BlockDist=' + RealToString(BlockDist,-12,0));
         {$EndIf}
          break;
      end;
   end {i loop};
   Dispose(fxgrids);
   Dispose(fygrids);
   Dispose(fdists);
   Dispose(Elevs);
   {$IfDef RecordHorizon}
      WriteLineToDebugFile(' BlockAngle=' + RealToString(BlockAngle,-12,2) + ' BlockDist=' + RealToString(BlockDist,-12,0));
      WriteLineToDebugFile(' Curvature=' + RealToString(DropEarthCurve(BlockDist),-12,2)) + ' Blocked=' + LatLongDegreeToString(BlockLat,BlockLong,DecMinutes));
   {$EndIf}
end;


function tDEMDataSet.VisibleElevation(xg1,yg1,z1,xg2,yg2 : float64) : float64;
{returns lowest antenna elevation that would be visible at the target location (xg1,yg1)}
var
   PointOnRay,PixLong : integer;
   PointElev : float32;
   W_Lat,W_Long,Lat2,Long2,
   MaxTanAngle,SectLen,Heading,TanAngle : float64;
   xgrids,ygrids,dists : ^Petmath.bfarray32;
begin
   New(xgrids);
   New(ygrids);
   New(dists);
   Result := -999;
   MaxTanAngle := -999;
   SectLen := DistanceMetersBetweenPoints(xg1,yg1,xg2,yg2,Heading);
   PixLong := round(2 * SectLen / AverageXSpace);
   DEMGridToLatLongDegree(xg1,yg1,W_Lat,W_Long);
   DEMGridToLatLongDegree(xg2,yg2,Lat2,Long2);
   GetStraightRoute(false,W_Lat,W_Long,Lat2,Long2,MDDef.wf.StraightAlgorithm,PixLong,xgrids^,ygrids^,dists^);
   For PointOnRay := 1 to PixLong do begin
      if GetElevMeters(xgrids^[PointOnRay],ygrids^[PointOnRay],PointElev) then begin
         PointElev := PointElev - DropEarthCurve(Dists^[PointOnRay]);
         TanAngle := (PointElev - z1) / Dists^[PointOnRay];  {line to target}
         if (TanAngle > MaxTanAngle) then MaxTanAngle := TanAngle;
         Result := z1 + (MaxTanAngle * SectLen) - PointElev;
      end;
   end;
   if Result < 0 then Result := 0;
   Dispose(xgrids);
   Dispose(ygrids);
   Dispose(dists);
end;


procedure tDEMDataSet.GetVisiblePoints(ObsElev,W_TargetUp,MinViewPort,MaxViewPort : float64; ObserverTerrainHug,TargetTerrainHug : boolean; PointsPerRay : integer;
   var xgrids,ygrids,dists,elevs : Petmath.bfarray32;  var VisPoints : array of boolean);
var
   NewTanAngle,TanAngle,MaxTanAngle,zt,DeltaZ,TargetCushionUp : float64;
   PointOnRay : integer;
   veg_ht : float32;
   AboveVeg : boolean;
   {$IfDef RecordGetVisiblePoints} tStr : shortstring; {$EndIf}
begin
   {$IfDef RecordGetVisiblePoints} WriteLineToDebugFile('tDEMDataSet.GetVisiblePoints in'); {$EndIf}
   MaxTanAngle := -999;
   GetElevMeters(xgrids[0],ygrids[0],elevs[0]);
   VisPoints[0] := true;
   if ObserverTerrainHug then begin
      ObsElev := elevs[0] + ObsElev;
   end
   else begin
      if (ObsElev < elevs[0]) then begin  //observer below ground
         For PointOnRay := 0 to PointsPerRay do VisPoints[PointOnRay] := false;
         exit;
      end;
   end;
   MinViewPort := TanDeg(MinViewPort);
   MaxViewPort := TanDeg(MaxViewPort);
   TargetCushionUp := W_TargetUp + 0.0001;
   veg_ht := 0;
   {$IfDef RecordGetVisiblePoints} WriteLineToDebugFile('tDEMDataSet.GetVisiblePoints PointsPerRay=' + IntToStr(PointsPerRay)); {$EndIf}
   For PointOnRay := 1 to PointsPerRay do begin
      VisPoints[PointOnRay] := false;
      if GetElevMeters(xgrids[PointOnRay],ygrids[PointOnRay],elevs[PointOnRay]) then begin
         {$IfDef ExVegDensity} {$Else} if (VegGrid[1] > 0) then DEMGlb[VegGrid[1]].GetJustVegHeight(xgrids[PointOnRay],ygrids[PointOnRay],veg_ht); {$EndIf}
         if TargetTerrainHug then begin
            zt := elevs[PointOnRay] + W_TargetUp;
            AboveVeg := (TargetCushionUp > Veg_ht);
         end
         else begin
            zt := W_TargetUp;
            AboveVeg := (TargetCushionUp > elevs[PointOnRay] + Veg_ht);
         end;
         DeltaZ := DropEarthCurve(Dists[PointOnRay]) + ObsElev;
         TanAngle :=  (Zt - DeltaZ) / Dists[PointOnRay];  {line to target}
         NewTanAngle := (elevs[PointOnRay]  + Veg_Ht - DeltaZ) / Dists[PointOnRay]; {line along ground which blocks the view for points farther away}
         VisPoints[PointOnRay] := AboveVeg and (TanAngle >= MaxTanAngle) and (TanAngle >= MinViewPort) and (TanAngle <= MaxViewPort);
      end
      else begin
         if MDDef.MissingDataBlocksLOS then begin
            NewTanAngle := 9999;
         end;
      end;
      if (NewTanAngle > MaxTanAngle) and (Dists[PointOnRay] > MDDef.wf.ClosestBlockingDistance) then MaxTanAngle := NewTanAngle;
   end;
   {$IfDef RecordGetVisiblePoints} if VisPoints[PointsPerRay] then TStr := 'visible' else TStr := 'masked';  WriteLineToDebugFile('tDEMDataSet.GetVisiblePoints, last pt= ' + tStr); {$EndIf}
end;



