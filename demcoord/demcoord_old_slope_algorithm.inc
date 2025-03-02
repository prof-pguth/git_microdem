{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{----------------------------------------}
{ include file for demcoord              }
{________________________________________}



const
(*
   smEvansYoung = 0;
   smHorn = 1;
   smEightNeighborsWeightedByDistance = 2;
   smGuthHybrid = 3;
   smFrameFiniteDifference = 4;
   smZevenbergenThorne = 5;
   smSimpleDifference = 6;
   smONeillAndMark = 7;
   smSteepestNeighbor = 8;
   smAverageNeighbor = 9;
   smMaxDownHillSlope = 10;
   LastSlopeMethod = 10;
*)


function tDEMDataSet.OldGetSlopeAndAspect(MDDef.SlopeCompute,Col,Row : integer; var SlopeAsp : tSlopeAspectRec; Radius : integer = 0) : boolean;
var
   sl     : tFourFloats;
   AspDir : tAspectDir;
var
   j : integer;
   Lat,Long : float64;


         procedure MaxDownHill(z,z1,z2,Dist : float64; var Max : float64; Dir1,Dir2 : tCompassDirection; var AspDir : tCompassDirection);  inline;
         //used only for MaxDownhillSlope, not recommended
         var
            d1,d2 : float64;
         begin {proc MaxDownHill}
            d1 := (z - z1);
            d2 := (z - z2);
            if (d1 = d2) then if Odd(Random(100)) then d1 := 0 else d2 := 0;
            if (d1 > d2) and (d1 > 0) then begin
               AspDir := Dir1;
               Max := d1 / Dist;
            end
            else if (d2 > d1) and (d2 > 0) then begin
               AspDir := Dir2;
               Max := d2 / Dist;
            end
            else begin
               Max := 0;
               AspDir := cdPit;
            end;
         end {proc MaxDownHill};


         procedure GetAspect(var SlopeAsp : tSlopeAspectRec); inline;
         begin
            if (abs(SlopeAsp.dzdx) < 0.001) and (abs(SlopeAsp.dzdy) < 0.001) then begin
               SlopeAsp.AspectDir := MaxSmallInt;
               SlopeAsp.AspectDirGrid := MaxSmallInt;
               SlopeAsp.Dir := cdFlat;
            end
            else begin
               //modified atan2 function
               //   standard math convention puts 0 on the x axis and angles increases counterclockwise,
               //   use geographic conventions, where N (0) is on the y axis, and angles increase clockwise
               SlopeAsp.AspectDirGrid := HeadingOfLine(SlopeAsp.dzdx,SlopeAsp.dzdy) + 180;
               if (SlopeAsp.AspectDirGrid > 360) then SlopeAsp.AspectDirGrid := SlopeAsp.AspectDirGrid - 360;
               if (SlopeAsp.AspectDirGrid < 0) then SlopeAsp.AspectDirGrid := SlopeAsp.AspectDirGrid + 360;

               SlopeAsp.AspectDir := SlopeAsp.AspectDirGrid + SlopeAsp.GridTrueAngle;
               if (SlopeAsp.AspectDir > 360) then SlopeAsp.AspectDir := SlopeAsp.AspectDir - 360;
               if (SlopeAsp.AspectDir < 0) then SlopeAsp.AspectDir := SlopeAsp.AspectDir + 360;

               if IsPit(SlopeAsp) then begin
                  SlopeAsp.Dir := cdPit;
               end
               else begin
                  SlopeAsp.Dir := AspectDir8FromAspect(SlopeAsp.AspectDir);
               end;
            end;
         end;

         procedure MaxSlopeComputations(var SlopeAsp : tSlopeAspectRec; var sl : tFourFloats; var AspDir : tAspectDir);
         //used only for SteepestNeighbor,AverageNeighbor; not recommended
         //not inline because if you want to use it you can take the performance hit

                  procedure MaxSlope(z,z1,z2,Dist : float64; var Max : float64; Dir1,Dir2 : tCompassDirection; var AspDir : tCompassDirection);
                  const
                     Opp : array[tCompassDirection] of tCompassDirection = (cdS,cdSW,cdW,cdNW,cdN,cdNE,cdE,cdSE,cdFlat,cdPit);
                  var
                     d1,d2 : float64;
                  begin {proc MaxSlope}
                     d1 := (z - z1);
                     d2 := (z - z2);
                     if (d1 = d2) then if Odd(Random(100)) then d1 := 0 else d2 := 0;

                     if abs(d1) > abs(d2) then begin
                        if d1 < 0 then AspDir := Opp[Dir1] else AspDir := Dir1;
                        d1 := abs(d1);
                        Max := d1 / Dist;
                     end
                     else begin
                        if d2 < 0 then AspDir := Opp[Dir2] else AspDir := Dir2;
                        d2 := abs(d2);
                        Max := d2 / Dist;
                     end;
                  end {proc MaxSlope};

         begin
             MaxSlope(SlopeAsp.z,SlopeAsp.zs,SlopeAsp.zn,SlopeAsp.dy,sl[1],cdS,cdN,SlopeAsp.Dir);
             MaxSlope(SlopeAsp.z,SlopeAsp.zw,SlopeAsp.ze,SlopeAsp.dx,sl[2],cdW,cdE,AspDir[2]);
             MaxSlope(SlopeAsp.z,SlopeAsp.zsw,SlopeAsp.znw,SlopeAsp.Dia_Space,sl[3],cdSW,cdNW,AspDir[3]);
             MaxSlope(SlopeAsp.z,SlopeAsp.zse,SlopeAsp.zne,SlopeAsp.Dia_Space,sl[4],cdSE,cdNE,AspDir[4]);
         end;

         procedure AddSlope(var SlopeAsp : tSlopeAspectRec; z,z1,z2,Dist : float64);  inline;
         //used only for average neighbors method, not recommended
         var
            d1,d2 : float64;
         begin
            d1 := (z - z1);
            d2 := (z - z2);
            d1 := abs(d1);
            SlopeAsp.Slope := SlopeAsp.Slope + d1 / Dist;
            d2 := abs(d2);
            SlopeAsp.Slope := SlopeAsp.Slope + d2 / Dist;
         end;


         procedure ComputeSecondOrderPartials(var SlopeAsp : tSlopeAspectRec); inline;
         var
            s3,s4,s5,s6 : float64;
         begin
         (*
         //https://github.com/OSGeo/grass/blob/main/raster/r.slope.aspect/main.c#L954
         //https://grass.osgeo.org/grass84/manuals/r.slope.aspect.html
         //Horn's formula is used to find the first order derivatives in x and y directions; 2d order not specified.
                                ____________________________
                                |c1      |c2      |c3      |
                                |        |  north |        |
                                ----------------------------
                                |c4      |c5      |c6      |
                                |  west  | ns_med |  east  |
                                ----------------------------
                                |c7      |c8      |c9      |
                                |        |  south |        |
                                ----------------------------

                /* compute second order derivatives */
                             s3 = c7 - c9 + c3 - c1;
                             s4 = c1 + c3 + c7 + c9 - c5 * 8.;
                             s5 = c4 * 4. + c6 * 4. - c8 * 2. - c2 * 2.;
                             s6 = c8 * 4. + c2 * 4. - c4 * 2. - c6 * 2.;

                             dxx = -(s4 + s5) / ((3. / 32.) * H * H);
                             dyy = -(s4 + s6) / ((3. / 32.) * V * V);
                             dxy = -s3 / ((1. / 16.) * H * V);
          *)
              with SlopeAsp do begin
                 s3 := zsw - zse + zne - znw;
                 s4 := znw + zne + zsw + zse - z * 8.0;
                 s5 := zw * 4.0 + ze * 4.0 - zs * 2.0 - zn * 2.0;
                 s6 := zs * 4.0 + zn * 4.0 - zw * 2.0 - ze * 2.0;
              end;
              SlopeAsp.dxx := -(s4 + s5) / ( (3.0 / 32.0) * 8 * SlopeAsp.dx * 8 * SlopeAsp.dx);
              SlopeAsp.dyy := -(s4 + s6) / ( (3.0 / 32.0) * 8 * SlopeAsp.dy * 8 * SlopeAsp.dy);
              SlopeAsp.dxy := -s3 / ( (1.0 / 16.0) * 8 * SlopeAsp.dx * 8 * SlopeAsp.dy);
         end;



begin
   Result := false;
   SlopeAsp.AspectDir := MaxSmallInt;
   SlopeAsp.Slope := 0;
   if (Radius = 0) then Radius := MDDef.SlopeRegionRadius;

   if SurroundedPointElevs(Col,Row,SlopeAsp.znw,SlopeAsp.zw,SlopeAsp.zsw,SlopeAsp.zn,SlopeAsp.z,SlopeAsp.zs,SlopeAsp.zne,SlopeAsp.ze,SlopeAsp.zse,Radius) then begin
      Result := true;
      PixelSpacingAndRotation(Col,Row,Lat,Long,SlopeAsp.dx,SlopeAsp.dy,SlopeAsp.GridTrueAngle,MDDef.QuickSlopeSpacings);
      SlopeAsp.dx := SlopeAsp.dx * Radius;
      SlopeAsp.dy := SlopeAsp.dy * Radius;


      if (MDDef.SlopeAlg in [smEvansYoung,smZevenbergenThorne,smHorn,smEightNeighborsWeightedByDistance,smFrameFiniteDifference,smSimpleDifference,smONeillAndMark]) then begin
         if (MDDef.SlopeAlg in [smEvansYoung]) then begin //preferred method, Evans
            SlopeAsp.dzdx := (+SlopeAsp.zne+SlopeAsp.ze+SlopeAsp.zse-SlopeAsp.znw-SlopeAsp.zw-SlopeAsp.zsw) / SlopeAsp.dx / 6;
            SlopeAsp.dzdy := (+SlopeAsp.znw+SlopeAsp.zn+SlopeAsp.zne-SlopeAsp.zsw-SlopeAsp.zs-SlopeAsp.zse) / SlopeAsp.dy / 6;
         end
         else if (MDdef.SlopeAlg = smHorn) then begin  //Horn method
            SlopeAsp.dzdx := 0.125 * ( SlopeAsp.zne + (2 * SlopeAsp.ze) + SlopeAsp.zse  - SlopeAsp.znw - (2 * SlopeAsp.zw) - SlopeAsp.zsw) / SlopeAsp.dx;
            SlopeAsp.dzdy := 0.125 * ( SlopeAsp.znw + (2 * SlopeAsp.zn) + SlopeAsp.zne  - SlopeAsp.zsw - (2 * SlopeAsp.zs) - SlopeAsp.zse) / SlopeAsp.dy;
         end
         else if (MDDef.SlopeAlg = smZevenbergenThorne) then begin  //Fleming and Hoffer, 1979; Ritter, 1987; Zevenbergen and Thorne, 1987; O'Neill & Mark, 1987
            SlopeAsp.dzdy := (SlopeAsp.zn - SlopeAsp.zs) * 0.5 / SlopeAsp.dy;
            SlopeAsp.dzdx := (SlopeAsp.ze - SlopeAsp.zw) * 0.5 / SlopeAsp.dx;
         end
         else if (MDdef.SlopeAlg = smEightNeighborsWeightedByDistance) then begin  //Unwin, 1981
            SlopeAsp.dzdy := 1 / (4 + 2 * sqrt_2) * ((SlopeAsp.znw + Sqrt_2 * SlopeAsp.zn + SlopeAsp.zne) - (SlopeAsp.zsw + Sqrt_2 * SlopeAsp.zs + SlopeAsp.zse)) / SlopeAsp.dy;
            SlopeAsp.dzdx := 1 / (4 + 2 * sqrt_2) * ((SlopeAsp.zne + Sqrt_2 * SlopeAsp.ze + SlopeAsp.zse) - (SlopeAsp.znw + Sqrt_2 * SlopeAsp.zw + SlopeAsp.zsw)) / SlopeAsp.dx;
         end
         else if (MDDef.SlopeAlg = smFrameFiniteDifference) then begin
            SlopeAsp.dzdy := (SlopeAsp.znw - SlopeAsp.zsw + SlopeAsp.zne - SlopeAsp.zse) * 0.25 / SlopeAsp.dy;
            SlopeAsp.dzdx := (SlopeAsp.zse - SlopeAsp.zsw + SlopeAsp.zne - SlopeAsp.znw) * 0.25 / SlopeAsp.dx;
         end
         else if (MDDef.SlopeAlg = smSimpleDifference) then begin
            SlopeAsp.dzdy := (SlopeAsp.z - SlopeAsp.zs) * 0.5 / SlopeAsp.dy;
            SlopeAsp.dzdx := (SlopeAsp.z - SlopeAsp.zw) * 0.5 / SlopeAsp.dx;
         end
         else if (MDDef.SlopeAlg in [smONeillAndMark]) then begin
            SlopeAsp.dzdy := (SlopeAsp.zn - SlopeAsp.z) / SlopeAsp.dy;
            SlopeAsp.dzdx := (SlopeAsp.ze - SlopeAsp.z) / SlopeAsp.dx;
         end;
         ComputeSecondOrderPartials(SlopeAsp);
         SlopeAsp.Slope := sqrt(sqr(SlopeAsp.dzdx) + sqr(SlopeAsp.dzdy));
         GetAspect(SlopeAsp);
      end
      else begin //these methods probably have no good reasons to recommend them any longer
         SlopeAsp.Dia_Space := DiagSpaceByDEMrow^[Row] * Radius;
         if (MDDef.SlopeAlg in [smGuthHybrid]) then begin  //no longer recommended
             MaxSlopeComputations(SlopeAsp,sl,AspDir);
             for j := 2 to 4 do if sl[j] > sl[1] then sl[1] := sl[j];
             SlopeAsp.Slope := Sl[1];
             SlopeAsp.dzdx := (SlopeAsp.zne + SlopeAsp.ze + SlopeAsp.zse - SlopeAsp.zsw - SlopeAsp.zw - SlopeAsp.znw) / 6 / SlopeAsp.dx;
             SlopeAsp.dzdy := (SlopeAsp.znw + SlopeAsp.zn + SlopeAsp.zne - SlopeAsp.zsw - SlopeAsp.zs - SlopeAsp.zse) / 6 / SlopeAsp.dy;
             GetAspect(SlopeAsp);
         end
         else if (MDDef.SlopeAlg in [smSteepestNeighbor,smAverageNeighbor]) then begin
            MaxSlopeComputations(SlopeAsp,sl,AspDir);
            for j := 2 to 4 do if sl[j] > sl[1] then begin
                sl[1] := sl[j];
                SlopeAsp.Dir := AspDir[j];
            end {for j};
            SlopeAsp.Slope := Sl[1];
            SlopeAsp.AspectDir := 45.0 * Ord(SlopeAsp.Dir);
            if (MDDef.SlopeAlg = smAverageNeighbor) then begin
               SlopeAsp.Slope := 0;
               AddSlope(SlopeAsp,SlopeAsp.z,SlopeAsp.zs,SlopeAsp.zn,SlopeAsp.dy);
               AddSlope(SlopeAsp,SlopeAsp.z,SlopeAsp.zw,SlopeAsp.ze,SlopeAsp.dx);
               AddSlope(SlopeAsp,SlopeAsp.z,SlopeAsp.zsw,SlopeAsp.znw,SlopeAsp.Dia_Space);
               AddSlope(SlopeAsp,SlopeAsp.z,SlopeAsp.zse,SlopeAsp.zne,SlopeAsp.Dia_Space);
               SlopeAsp.Slope := SlopeAsp.Slope / 8;
            end;
         end
         else if (MDDef.SlopeAlg = smMaxDownhillSlope) then begin
            MaxDownHill(SlopeAsp.z,SlopeAsp.zs,SlopeAsp.zn,SlopeAsp.dy,sl[1],cdS,cdN,SlopeAsp.Dir);
            MaxDownHill(SlopeAsp.z,SlopeAsp.zw,SlopeAsp.ze,SlopeAsp.dx,sl[2],cdW,cdE,AspDir[2]);
            MaxDownHill(SlopeAsp.z,SlopeAsp.zsw,SlopeAsp.znw,SlopeAsp.Dia_Space,sl[3],cdSW,cdNW,AspDir[3]);
            MaxDownHill(SlopeAsp.z,SlopeAsp.zse,SlopeAsp.zne,SlopeAsp.Dia_Space,sl[4],cdSE,cdNE,AspDir[4]);
            for j := 2 to 4 do if sl[j] > sl[1] then begin
                sl[1] := sl[j];
                SlopeAsp.Dir := AspDir[j];
            end {for j};
            SlopeAsp.Slope := Sl[1];
            SlopeAsp.AspectDir := 45.0 * Ord(SlopeAsp.Dir);
         end;
         if IsPit(SlopeAsp) then SlopeAsp.Dir := cdPit;
      end;
      SlopeAsp.SlopePercent := 100 * SlopeAsp.Slope;
      SlopeAsp.SlopeDegree := ArcTan(SlopeAsp.Slope) / DegToRad;
   end;
end {proc GetSlopeAndAspect};

