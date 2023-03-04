{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of ianMICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2023 Peter L. Guth   }
{____________________________________}

{include file for demcoord.pas}



procedure tDEMDataSet.WoodPointClassify(Col,Row : integer; var PointType : tPointType);
{after Wood, 1996, algorithm 5.1}
{used GRASS C code available on the web}
var
   MaxCurve,MinCurve,SlopeDeg,CrossC,
   SlopeCurvature,PlanCurvature  : float64;
begin
   if GetEvansParams(Col,Row,MDDef.WoodRegionRadiusPixels,SlopeDeg,SlopeCurvature,PlanCurvature,CrossC,MaxCurve,MinCurve) then begin
      if (SlopeDeg < MDDef.MinSlopeToClassify) then PointType := FlatPoint
      else if (slopeDeg > MDDef.SlopeClassTolerance) then begin
         if (CrossC > MDDef.ConvexClassTolerance) then PointType := RidgePoint
         else if (CrossC < -MDDef.ConvexClassTolerance) then PointType := ValleyPoint
         else PointType := OtherPoint;
      end
      else if (MaxCurve > MDDef.ConvexClassTolerance) then begin
         if (MinCurve > MDDef.ConvexClassTolerance) then PointType := PeakPoint
         else if (MinCurve < -MDDef.ConvexClassTolerance) then PointType := PassPoint
         else PointType := RidgePoint;
      end
      else if (MinCurve < -MDDef.ConvexClassTolerance) then begin
         if (MaxCurve < -MDDef.ConvexClassTolerance) then PointType := PitPoint
         else PointType := ValleyPoint
      end
      else PointType := OtherPoint;
   end
   else begin
      PointType := EdgePoint;
      if MissingDataInGrid(Col,Row) then PointType := MissingPoint;
   end;
end;

function tDEMDataSet.AmbiguosRidgeValley(SWCol,SWRow : integer; var CouldKnow : boolean) : boolean;
var
   Elev  : tElevFloatArray;
begin
   Result := false;
   if GetElevSquareMeters(SWCol,SWRow,Elev) then begin
      CouldKnow := true;
      Result := ((Elev[1] > Elev [4]) and  (Elev[3] > Elev [4]) and (Elev[3] > Elev [2]) and (Elev[1] > Elev [2])) or
                ((Elev[1] < Elev [4]) and  (Elev[3] < Elev [4]) and (Elev[3] < Elev [2]) and (Elev[1] < Elev [2]));
  end
  else CouldKnow := false;
end;

function tDEMDataSet.HighLowPointNeighbors(Col,Row,Region : integer; Tolerance : float32; var Higher,Lower : integer) : boolean;
var
   znw,zw,zsw,zn,z,zs,zne,ze,zse : float32;
begin
   Result := SurroundedPointElevs(Col,Row,znw,zw,zsw,zn,z,zs,zne,ze,zse,Region);
   if Result then begin
      Higher := 0;
      if (z < znw - Tolerance) then inc(Higher);
      if (z < zn - Tolerance) then inc(Higher);
      if (z < zne - Tolerance) then inc(Higher);
      if (z < zsw - Tolerance) then inc(Higher);
      if (z < zs - Tolerance) then inc(Higher);
      if (z < zse - Tolerance) then inc(Higher);
      if (z < zw - Tolerance) then inc(Higher);
      if (z < ze - Tolerance) then inc(Higher);

      Lower := 0;
      if (z > znw + Tolerance) then inc(Lower);
      if (z > zn + Tolerance) then inc(Lower);
      if (z > zne + Tolerance) then inc(Lower);
      if (z > zsw + Tolerance) then inc(Lower);
      if (z > zs + Tolerance) then inc(Lower);
      if (z > zse + Tolerance) then inc(Lower);
      if (z > zw + Tolerance) then inc(Lower);
      if (z > ze + Tolerance) then inc(Lower);
   end;
end;

function tDEMDataSet.ClassifyAPoint(Col,Row : integer) : tPointType;
var
   znw,zw,zsw,zn,z,zs,zne,ze,zse : float32;
   Higher,Lower : integer;
begin
   //if SurroundedPointElevs(Col,Row,znw,zw,zsw,zn,z,zs,zne,ze,zse,MDDef.WoodRegionSize) then begin
   if  (MDDef.RidgePointClassify = raSimple2) and HighLowPointNeighbors(Col,Row,MDDef.WoodRegionRadiusPixels,MDDef.MinDeltaZToClassify,Higher,Lower) then begin
       (*
          Higher := 0;
         if (z < znw - MDDef.MinDeltaZToClassify) then inc(Higher);
         if (z < zn - MDDef.MinDeltaZToClassify) then inc(Higher);
         if (z < zne - MDDef.MinDeltaZToClassify) then inc(Higher);
         if (z < zsw - MDDef.MinDeltaZToClassify) then inc(Higher);
         if (z < zs - MDDef.MinDeltaZToClassify) then inc(Higher);
         if (z < zse - MDDef.MinDeltaZToClassify) then inc(Higher);
         if (z < zw - MDDef.MinDeltaZToClassify) then inc(Higher);
         if (z < ze - MDDef.MinDeltaZToClassify) then inc(Higher);

         Lower := 0;
         if (z > znw + MDDef.MinDeltaZToClassify) then inc(Lower);
         if (z > zn + MDDef.MinDeltaZToClassify) then inc(Lower);
         if (z > zne + MDDef.MinDeltaZToClassify) then inc(Lower);
         if (z > zsw + MDDef.MinDeltaZToClassify) then inc(Lower);
         if (z > zs + MDDef.MinDeltaZToClassify) then inc(Lower);
         if (z > zse + MDDef.MinDeltaZToClassify) then inc(Lower);
         if (z > zw + MDDef.MinDeltaZToClassify) then inc(Lower);
         if (z > ze + MDDef.MinDeltaZToClassify) then inc(Lower);
         *)

         if (Higher = 8) then Result := PitPoint
         else if (Lower = 8) then Result := PeakPoint
         else if (Higher = 0) and (Lower = 0) then Result := FlatPoint
         else if (Higher >= MDDef.ValleyRidgeThreshold) then Result := ValleyPoint
         else if (Lower >= MDDef.ValleyRidgeThreshold) then Result := RidgePoint
         else Result := OtherPoint;
      end
      else if SurroundedPointElevs(Col,Row,znw,zw,zsw,zn,z,zs,zne,ze,zse,MDDef.WoodRegionRadiusPixels) then begin
         if (z < znw - MDDef.MinDeltaZToClassify) and (z < zn - MDDef.MinDeltaZToClassify)  and (z < zne - MDDef.MinDeltaZToClassify) and
            (z < zsw - MDDef.MinDeltaZToClassify) and (z < zs - MDDef.MinDeltaZToClassify)  and (z < zse - MDDef.MinDeltaZToClassify) and
            (z < zw - MDDef.MinDeltaZToClassify) and (z < ze - MDDef.MinDeltaZToClassify) then Result := PitPoint
         else if (z = znw) and (z = zn)  and (z = zne) and
            (z = zsw) and (z = zs)  and (z = zse) and
            (z = zw) and (z = ze) then Result := FlatPoint
         else if (z > znw + MDDef.MinDeltaZToClassify) and (z > zn + MDDef.MinDeltaZToClassify)  and (z > zne + MDDef.MinDeltaZToClassify) and
            (z > zsw + MDDef.MinDeltaZToClassify) and (z > zs + MDDef.MinDeltaZToClassify)  and (z > zse + MDDef.MinDeltaZToClassify) and
            (z > zw + MDDef.MinDeltaZToClassify) and (z > ze + MDDef.MinDeltaZToClassify) then Result := PeakPoint
         else if ((zn > znw + MDDef.MinDeltaZToClassify) and (zn > zne + MDDef.MinDeltaZToClassify)) and
            ((z > zw + MDDef.MinDeltaZToClassify) and (z > ze + MDDef.MinDeltaZToClassify)) and
            ((zs > zsw + MDDef.MinDeltaZToClassify) and (zs > zse + MDDef.MinDeltaZToClassify)) then Result := RidgePoint {NS trend}
         else if ((zw > znw + MDDef.MinDeltaZToClassify) and (zw > zsw + MDDef.MinDeltaZToClassify)) and
            ((z > zn + MDDef.MinDeltaZToClassify) and (z > zs + MDDef.MinDeltaZToClassify)) and
            ((ze > zne + MDDef.MinDeltaZToClassify) and (ze > zse + MDDef.MinDeltaZToClassify)) then Result := RidgePoint {WE trend}
         else if ((zn < znw - MDDef.MinDeltaZToClassify) and (zn < zne - MDDef.MinDeltaZToClassify)) and
            ((z < zw - MDDef.MinDeltaZToClassify) and (z < ze - MDDef.MinDeltaZToClassify)) and
            ((zs < zsw - MDDef.MinDeltaZToClassify) and (zs < zse - MDDef.MinDeltaZToClassify)) then Result := ValleyPoint {NS trend}
         else if ((zw < znw - MDDef.MinDeltaZToClassify) and (zw < zsw - MDDef.MinDeltaZToClassify)) and
            ((z < zn - MDDef.MinDeltaZToClassify) and (z < zs - MDDef.MinDeltaZToClassify)) and
            ((ze < zne - MDDef.MinDeltaZToClassify) and (ze < zse - MDDef.MinDeltaZToClassify)) then Result := ValleyPoint {WE trend}
         else Result := OtherPoint;
   end
   else begin
      if MissingDataInGrid(Col,Row) then Result := MissingPoint
      else Result := EdgePoint;
   end;
end;


procedure tDEMDataSet.InitializeTerrainCategory(var TerrainCat : tTerrainCatDefinition);
begin
   with TerrainCat do begin
      UseElevation := true;
      UseSlope := true;
      UseRelief := false;
      UseAspect := false;
      CatColor := claLime;
      CatMinElev  := round(DEMheader.MinElev);
      CatMaxElev  := round(DEMheader.MaxElev);
      CatMaxRelief := round(CatMaxElev - CatMinElev);

      CatMinRelief := 0;
      CatReliefRadius := 500;
      if (CatReliefRadius < 5 * AverageXSpace) then CatReliefRadius := round(5 * AverageXSpace);
      CatMinSlope := 0;
      CatMaxSlope := 255;
      CatAspects := [cdN,cdNE,cdE,cdSE,cdS,cdSW,cdW,cdNW];
   end;
end;


function tDEMDataSet.TerrainCategoryLabel(TerrainCategory : tTerrainCatDefinition) : ShortString;
var
   TStr : ShortString;
begin
   with TerrainCategory do begin
      if not ElevationDEM then Result := 'z=' + RealToString(CatMinElev,-12,1) + '--' + RealToString(CatMaxElev,-12,1)
      else begin
         Result := '';
         if UseElevation then Result := 'z=' + RealToString(CatMinElev,-12,1) + '--' + RealToString(CatMaxElev,-12,1);
         if UseSlope then Result := Result + '  slope=' + IntToStr(round(CatMinSlope)) + '--' + IntToStr(round(CatMaxSlope)) + '%';
         if UseRelief then Result := Result + '  relief=' + IntToStr(CatMinRelief) + '--' + IntToStr(CatMaxRelief);
         if UseAspect then begin
            TStr := '';
            if cdN in CatAspects then TStr := TStr + 'N,';
            if cdNE in CatAspects then TStr := TStr + 'NE,';
            if cdE in CatAspects then TStr := TStr + 'E,';
            if cdSE in CatAspects then TStr := TStr + 'SE,';
            if cdS in CatAspects then TStr := TStr + 'S,';
            if cdSW in CatAspects then TStr := TStr + 'SW,';
            if cdW in CatAspects then TStr := TStr + 'W,';
            if cdNW in CatAspects then TStr := TStr + 'NW,';
            if Length(TStr) = 20 then TStr := ''
            else if (Length(TStr) > 0) then begin
               TStr := '    Aspect=' + TStr;
               Delete(TStr,Length(TStr),1);
            end;
            Result := Result + TStr;
         end;
      end;
   end;
end;












