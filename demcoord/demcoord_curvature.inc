{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2026 Peter L. Guth  }
{----------------------------------------}
{ include file for demcoord              }
{________________________________________}


8/26/2024:  this is currently not used


procedure tDEMDataSet.PlanCMoments(GridLimits: tGridLimits; var PlanCMoment : tMomentVar);
var
   zvs : ^bfarray32;
begin
   New(zvs);
   GetPlanCInLongArray(GridLimits,PlancMoment.NPts,zvs^,false);
   moment(zvs^,PlanCMoment,msBeforeMedian);
   Dispose(zvs);
end;

procedure tDEMDataSet.ProfCMoments(GridLimits: tGridLimits;  var ProfCMoment : tMomentVar);
var
   zvs : ^bfarray32;
begin
   New(zvs);
   GetProfCInLongArray(GridLImits,ProfCMoment.NPts,zvs^,false);
   moment(zvs^,ProfCMoment,msBeforeMedian);
   Dispose(zvs);
end;


function tDEMDataSet.GetEvansParams(Col,Row,RegionSize : integer; var SlopeDeg,SlopeCurvature,PlanCurvature,crossc,MaxCurve,MinCurve : float64) : boolean;
const
   Tolerance = 0.000000000001;
var
   SqED,z1,z2,z3,z4,z6,z7,z8,z9,a,b,c,d,e,SqABC,
   xsp,ysp,asp : float64;
   znw,zw,zsw,zn,z,zs,zne,ze,zse : float32;
begin
   if SurroundedPointElevs(Col,Row,znw,zw,zsw,zn,z,zs,zne,ze,zse,RegionSize) then begin
      Result := true;
      ysp := AverageYSpace * RegionSize;
      xsp := XSpaceByDEMrow^[Row] * RegionSize;

      Asp := 0.5 * (xsp + ysp);
      z7 := zsw - z;
      z8 := zs - z;
      z9 := zse - z;
      z4 := zw - z;
      //z5 := 0;
      z6 := ze - z;
      z1 := znw - z;
      z2 := zn - z;
      z3 := zne - z;

      A := ((z1 + z3 + z4 + z6 + z7 + z9) / 6 - (z2 + {z5 +} z8) / 3) / XSp / YSp;
      B := ((Z1 + z2 + z3 + z7 + z8 + z9) / 6 - (z4 + {z5 +} z6) / 3) / XSp / YSp;
      C := (z3 + z7 -z1 - z9) / 4  / XSp / YSp;
      D := (z3 + z6 + z9 - z1 - z4 - z7) / 6 / XSp;
      E := (z1 + z2 + z3 - z7 - z8 - z9) / 6 / YSp;
      //F := (2 * ( z2 + z4 + z6 - z8) - (z1 + z3 + z7 + z9) {+ 5 * z5} ) / 9;

      SqABC := sqrt(sqr(a-b) + sqr(c));
      MaxCurve := 20 * (ASp)*(-a - b + SqABC);
      MinCurve := 20 * (ASp)*(-a - b - SqABC);
      SqED := (sqr(E) + sqr(D));
      if (SqED > Tolerance) then begin
         SlopeCurvature {Profile convexity} := -200 * (A * sqr(D) + B * sqr(E) + C * D * E) / SqED / Math.Power(1 + SqED, 1.5);
         PlanCurvature {Plan convexity} := 200 * (B * sqr(D) + A * sqr(E) - C * D * E) / Math.Power(1 +SqED, 1.5);
         crossc := -20 * (ASp)*(b*d*d + a*e*e - c*d*e)/  SqED;
      end
      else begin
         SlopeCurvature := 0;
         PlanCurvature := 0;
         CrossC := 0;
      end;
      SlopeDeg := arctan(sqrt(sqED)) / DegToRad;
   end
   else Result := false;
end;


function tDEMDataSet.GetCurvature(Col,Row : integer; var PlanCurvature,SlopeCurvature : float64) : boolean;
const
   Tolerance = 0.000000000001;
var
   xsp,ysp,
   znw,zw,zsw,zn,z,zs,zne,ze,zse,SqED,SqPQ,SqGH,P,Q,R,S,T,
   a,b,c,d,e,f,g,h,z1,z2,z3,z4,z6,z7,z8,z9,v1,v2,v3,v4,v5 : float64;
begin
   {all algorithms use the original notation; have the elevations normalized to the central point, z5; and retain but comment out any computations involving z5}
   //only works if all 9 neighbors are defined, so undefined near data voids and on edge of the grid
   if SurroundedPointElevs(Col,Row,znw,zw,zsw,zn,z,zs,zne,ze,zse,MDDef.CurvRegionSize) then begin
      GetCurvature := true;
      SlopeCurvature := 0;
      PlanCurvature := 0;

      ysp := AverageYSpace * MDDef.CurvRegionSize;
      xsp := XSpaceByDEMrow^[Row] * MDDef.CurvRegionSize;

      if (MDDef.CurvatureMethod = cmEvans) or (MDDef.CurvatureMethod = cmZevenbergenAndThorne) then begin
         {   1      2      3}
         {   4      5      6}
         {   7      8      9}
         z7 := zsw - z;
         z8 := zs - z;
         z9 := zse - z;
         z4 := zw - z;
         //z5 := 0;
         z6 := ze - z;
         z1 := znw - z;
         z2 := zn - z;
         z3 := zne - z;
         if (MDDef.CurvatureMethod = cmEvans) then begin
           {Evans, 1979, as defined in Woods PhD thesis}
            A := ((z1 + z3 + z4 + z6 + z7 + z9) / 6 - (z2 + {z5 +} z8) / 3) / xsp / ysp;
            B := ((Z1 + z2 + z3 + z7 + z8 + z9) / 6 - (z4 + {z5 +} z6) / 3) / xsp / ysp;
            C := (z3 + z7 -z1 - z9) / 4  / xsp / ysp;
            D := (z3 + z6 + z9 - z1 - z4 - z7) / 6 / xsp;
            E := (z1 + z2 + z3 - z7 - z8 - z9) / 6 / xsp;
            //F := (2 * ( z2 + z4 + z6 - z8) - (z1 + z3 + z7 + z9) {+ 5 * z5} ) / 9;

            SqED := (sqr(E) + sqr(D));
            if (SqED > Tolerance) then begin
               SlopeCurvature {Profile convexity} := -200 * (A * sqr(D) + B * sqr(E) + C * D * E) / SqED / Math.Power(1 + SqED, 1.5);
               PlanCurvature {Plan convexity} := 200 * (B * sqr(D) + A * sqr(E) - C * D * E) / Math.Power(1 + SqED, 1.5);
            end;
         end
         else if (MDDef.CurvatureMethod = cmZevenbergenAndThorne) then begin
            //A := ( (z1 + z3 + z7 + z9) / 4 - (z2 + z4 + z6 + z8) / 2 {+ z5}) / sqr(AverageXSpace) / sqr(AverageYSpace);
            //B := ( (Z1 + z3 - z7 - z9) / 4 - (z2 - z8) / 2 ) / sqr(AverageXSpace) / (AverageYSpace);
            //C := ( (-z1 + z3  - z7 + z9) / 4 + (z4 - z6) / 2 ) / (AverageXSpace) / sqr(AverageYSpace);
            D := ( (z4 + z6) / 2 {- z5}) / sqr(xsp);
            E := ( (z2 + z8) / 2 {- z5})  / sqr(ysp);
            F := ( -z1 + z3 + z7 - z9) / (4 * xsp * ysp);
            G := (-z4 + z6)  / (2 * xsp);
            H := (z2 - z8)  / (2 * ysp);
            //I := z5;
            SqGH := (sqr(G) + sqr(H));
            If (SqGH > Tolerance) then begin
               SlopeCurvature := 100 * -2 * (D * sqr(G) + E * sqr(H) + F * G * H) / SqGH;
               PlanCurvature := 100 * 2 * (D * sqr(H) + E * sqr(G) - F * G * H) / SqGH;
               //LaplacianCurvature := 100 * 2 * E + D;
            end
         end;
      end
      else begin
         {   7      8      9}
         {   4      5      6}
         {   1      2      3}
         z1 := zsw - z;
         z2 := zs - z;
         z3 := zse - z;
         z4 := zw - z;
         z6 := ze - z;
         z7 := znw - z;
         z8 := zn - z;
         z9 := zne - z;

         if (MDDef.CurvatureMethod = cmHeerdegenAndBeran) then begin
            {after Heerdegen, R.C., and Beran, M.A., 1982, Quantifying source areas through land surface curvature and shape: Journal of Hydrology, vol.57, p.359-373; modified to
             --work with variable x and y spacing;
             --have same sign for Plan curvature as others;
             --multiply both curvatures by 100;}
            v1 := z1 + z3 + z4 + z6 + z7 + z9;
            v2 := z1 + z2 + z3 + z7 + z8 + z9;
            v3 := z1 - z3 - z7 + z9;
            v4 := -z1 + z3 - z4 + z6 - z7 + z9;
            v5 := -z1 - z2 - z3 + z7 + z8 + z9;

            a := (0.3 * v1 - 0.2 * v2) / sqr(xsp);
            b := (0.3 * v2 - 0.2 * v1) / sqr(ysp);
            c := v3 / 4 / xsp / ysp;
            d := v4 / 6 / xsp;
            e := v5 / 6 / ysp;

            SqED := (sqr(E) + sqr(D));

            if SqED > Tolerance then  begin
               PlanCurvature := -200 * e * (c * d - 2 * a * e) / Math.Power(SqED,1.5);
               SlopeCurvature := -200 * (a * sqr(d) + b * sqr(e) + c * d * e) / (SqED * Math.Power((1.0 + SqED), 1.5));
            end;
         end
         else if (MDDef.CurvatureMethod = cmShary) then begin
            {after Shary, 1991, as given in Florinsky, 1998;  --multiply both curvatures by 100;}
            //r = d2z/dx2
            r := ( (z1 + z3 + z4 + z6 + z7 + z9) - 2 * (z2 + {z5 +} z8) ) / (3 * sqr(xsp));
            //r = d2z/dy2
            t := ( (z1 + z2 + z3 + z7 + z8 + z9) - 2 * (z4 + {z5 +} z6) ) / (3 * sqr(ysp));
            //s=d2z/dxdy
            s := (z3 + z7 - z1 - z9) / (4 * xsp * ysp);
            //p=dz/dx
            p := (z3 + z6 + z9 - z1 - z4 - z7) / (6 * xsp);
            //q=dz/dy
            q := (z1 + z2 + z3 - z7 - z8 - z9) / (6 * ysp);

            SqPQ := (sqr(P) + sqr(Q));
            if SqPQ > Tolerance then begin
               SqPQ := SqPQ * sqrt ( 1 + SqPQ);
               SlopeCurvature := -100 * (p * p * r  + 2 * P * q * s + q * q * t) / SqPQ;
               PlanCurvature := -100 * (q * q * r  - 2 * P * q * s + p * p * t) / SqPQ;
            end;
         end;
      end;
   end
   else GetCurvature := false;
end;

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

   procedure tDEMDataSet.GetPlanCInLongArray(GridLimits: tGridLimits; var NPts : int64; var Values : Petmath.bfarray32; IncludeSeaLevel : boolean = true);
   var
      Col,Row,Incr : integer;
      SlopeDeg,crossc,MaxCurve,MinCurve,
      PlanCurvature,SlopeCurvature : float64;
    begin
      NPts := 0;
      Incr := GetSamplingSize(GridLimits);
      Col := GridLimits.XGridLow;
      while (Col <= GridLimits.XGridHigh) do begin
         Row := GridLimits.YGridLow;
         while (Row <= GridLimits.YGridHigh) do begin
            if (IncludeSeaLevel) or (not SeaLevelCell(Col,Row)) then begin
               if GetEvansParams(Col,Row,1,SlopeDeg,SlopeCurvature,PlanCurvature,crossc,MaxCurve,MinCurve)  then begin
                  Values[Npts] := PlanCurvature;
                  inc(NPts);
               end;
            end;
            inc(Row,Incr);
         end;
         inc(Col,Incr);
      end;
   end;


   procedure tDEMDataSet.GetProfCInLongArray(GridLimits: tGridLimits; var NPts : int64; var Values : Petmath.bfarray32; IncludeSeaLevel : boolean = true);
   var
      Col,Row,Incr : integer;
      SlopeDeg,crossc,MaxCurve,MinCurve,
      PlanCurvature,SlopeCurvature : float64;
    begin
      NPts := 0;
      Incr := GetSamplingSize(GridLimits);
      Col := GridLimits.XGridLow;
      while (Col <= GridLimits.XGridHigh) do begin
         Row := GridLimits.YGridLow;
         while (Row <= GridLimits.YGridHigh) do begin
            if (IncludeSeaLevel) or (not SeaLevelCell(Col,Row)) then begin
               if GetEvansParams(Col,Row,1,SlopeDeg,SlopeCurvature,PlanCurvature,crossc,MaxCurve,MinCurve)  then begin
                  Values[Npts] := SlopeCurvature;
                  inc(NPts);
               end;
            end;
            inc(Row,Incr);
         end;
         inc(Col,Incr);
      end;
   end;
