{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program       }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2024 Peter L. Guth   }
{------------------------------------}
{   include file for demcoord.pas    }
{____________________________________}




function tDEMDataSet.RoughnessFromSlopeSTD(x,y,Radius : integer; var Roughness : float32) : boolean;
const
   MaxPoints = 51 * 51;
var
   i,j : integer;
   MomentVar : tMomentVar;
   SlopeAsp : tSlopeAspectRec;
   sl : array[1..MaxPoints] of float32;
begin
   if (Radius * Radius <= MaxPoints) then begin
      MomentVar.Npts := 0;
      Radius := Radius div 2;
      for I := -Radius to Radius do begin
         for J := -Radius to Radius do begin
            if GetSlopeAndAspect(x+i,y+j,SlopeAsp) then begin
               inc(MomentVar.Npts);
               sl[MomentVar.Npts] := SlopeAsp.SlopePercent;
            end;
         end;
      end;
      Result := MomentVar.NPts > 5;
      if Result then begin
         moment(sl,MomentVar,msAfterStdDev);
         Roughness := MomentVar.std_dev;
      end;
   end;
end;


function IsPit(var SlopeAsp : tSlopeAspectRec) : boolean; inline;
begin
   Result := (SlopeAsp.z < SlopeAsp.zne) and (SlopeAsp.z < SlopeAsp.znw) and (SlopeAsp.z < SlopeAsp.zn) and (SlopeAsp.z < SlopeAsp.ze) and
         (SlopeAsp.z < SlopeAsp.zw) and (SlopeAsp.z < SlopeAsp.zse) and (SlopeAsp.z < SlopeAsp.zsw) and (SlopeAsp.z < SlopeAsp.zs);
end;



function tDEMDataSet.GetSlopeAndAspect(Col,Row : integer; var SlopeAsp : tSlopeAspectRec; NeedSecondOrder : boolean = false; Radius : integer = 0) : boolean;
var
   Ring2 : tSlopeAspectRec;
   Lat,Long : float64;

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
               FindCompassAngleInRange(SlopeAsp.AspectDirGrid);
               SlopeAsp.AspectDir := SlopeAsp.AspectDirGrid + SlopeAsp.GridTrueAngle;
               FindCompassAngleInRange(SlopeAsp.AspectDir);
               if IsPit(SlopeAsp) then begin
                  SlopeAsp.Dir := cdPit;
               end
               else begin
                  SlopeAsp.Dir := AspectDir8FromAspect(SlopeAsp.AspectDir);
               end;
            end;
         end;

         (*
         procedure ComputeSecondOrderPartials(var SlopeAsp : tSlopeAspectRec); inline;
         var
            s3,s4,s5,s6 : float64;
         begin

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

                dx = ((c1 + c4 + c4 + c7) - (c3 + c6 + c6 + c9)) / H;
                dy = ((c7 + c8 + c8 + c9) - (c1 + c2 + c2 + c3)) / V;


                /* compute second order derivatives */
                             s3 = c7 - c9 + c3 - c1;
                             s4 = c1 + c3 + c7 + c9 - c5 * 8.;
                             s5 = c4 * 4. + c6 * 4. - c8 * 2. - c2 * 2.;
                             s6 = c8 * 4. + c2 * 4. - c4 * 2. - c6 * 2.;

                             dxx = -(s4 + s5) / ((3. / 32.) * H * H);
                             dyy = -(s4 + s6) / ((3. / 32.) * V * V);
                             dxy = -s3 / ((1. / 16.) * H * V);

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
         *)


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
      with SlopeAsp do begin
         // first order partial derivatives
         if (MDDef.SlopeAlgorithm = smZevenbergenThorne) then begin
            dzdx := (ze - zw) / (2 * dx);
            dzdy := (zn - zs) / (2 * dy);
         end
         else if (MDDef.SlopeAlgorithm = smEvansYoung) then begin
            dzdx := (+zne+ze+zse-znw-zw-zsw) / (6 * dx);
            dzdy := (+znw+zn+zne-zsw-zs-zse) / (6 * dy);
         end
         else if (MDDef.SlopeAlgorithm = smHorn) then begin
            dzdx := ( zne + (2 * ze) + zse  - znw - (2 * zw) - zsw) / (8 * dx);
            dzdy := ( znw + (2 * zn) + zne  - zsw - (2 * zs) - zse) / (8 * dy);
         end;

         if NeedSecondOrder then begin

            // second order partial derivatives
            if (not MDDef.CD2) and SurroundedPointElevs(Col,Row,Ring2.znw,Ring2.zw,Ring2.zsw,Ring2.zn,Ring2.z,Ring2.zs,Ring2.zne,Ring2.ze,Ring2.zse,2) then begin
               //CD1 method, getting the second order partials from a 5x5 window, which will be in the Ring2 elevations
               if (MDDef.SlopeAlgorithm = smZevenbergenThorne) then begin
                  dxx := (Ring2.ze - 2 * Ring2.z + Ring2.zw) / (4 * sqr(dx));
                  dyy := (Ring2.zn - 2 * Ring2.z + Ring2.zs) / (4 * sqr(dy));
               end
               else if (MDDef.SlopeAlgorithm = smEvansYoung) then begin
                  dxx := (Ring2.znw + Ring2.zw + Ring2.zsw + Ring2.zne + Ring2.ze + Ring2.zse - 2 * (Ring2.z + Ring2.zn + Ring2.zs)) / (6 * sqr(dx));
                  dyy := (Ring2.znw + Ring2.zn + Ring2.zne + Ring2.zsw + Ring2.zs + Ring2.zse - 2 * (Ring2.z + Ring2.ze + Ring2.zw)) / (6 * sqr(dx));
               end
               else if (MDDef.SlopeAlgorithm = smHorn) then begin
                  dxx := (Ring2.znw + 2 * Ring2.zw + Ring2.zsw + Ring2.zne + 2 * Ring2.ze + Ring2.zse - 2 * (Ring2.z + 2 * Ring2.zn + Ring2.zs)) / (8 * sqr(dx));
                  dyy := (Ring2.znw + 2 * Ring2.zn + Ring2.zne + Ring2.zsw + 2 * Ring2.zs + Ring2.zse - 2 * (Ring2.z + 2 * Ring2.ze + Ring2.zw)) / (8 * sqr(dx));
               end;
               //this changed 11 October 2024
               dxy := (Ring2.zne - Ring2.znw + Ring2.zsw - Ring2.zse) / (4 * dy * dy);
           end
           else begin
               //CD2 method, getting the second order partials from just the 3x3 window
               if (MDDef.SlopeAlgorithm = smZevenbergenThorne) then begin
                  dxx := (ze - 2 * z + zw) / (2 * sqr(dx));
                  dyy := (zn - 2 * z + zs) / (2 * sqr(dy));
               end
               else if (MDDef.SlopeAlgorithm = smEvansYoung) then begin
                  dxx := (znw + zw + zsw + zne + ze + zse - 2 * (z + zn + zs)) / (3 * sqr(dx));
                  dyy := (znw + zn + zne + zsw + zs + zse - 2 * (z + ze + zw)) / (3 * sqr(dx));
               end
               else if (MDDef.SlopeAlgorithm = smHorn) then begin
                  dxx := (znw + 2 * zw + zsw + zne + 2 * ze + zse - 2 * (z + 2 * zn + zs)) / (4 * sqr(dx));
                  dyy := (znw + 2 * zn + zne + zsw + 2 * zs + zse - 2 * (z + 2 * ze + zw)) / (4 * sqr(dx));
               end;
               //this changed 11 October 2024
               dxy := (zne - znw + zsw - zse) / (4 * dx * dy);  //same for all 3 algorithms
            end;
         end;
      end;
      //ComputeSecondOrderPartials(SlopeAsp);
      SlopeAsp.Slope := sqrt(sqr(SlopeAsp.dzdx) + sqr(SlopeAsp.dzdy));
      SlopeAsp.SlopePercent := 100 * SlopeAsp.Slope;
      SlopeAsp.SlopeDegree := ArcTan(SlopeAsp.Slope) / DegToRad;
      GetAspect(SlopeAsp);
   end;
end {proc GetSlopeAndAspect};


procedure tDEMDataSet.GetSlopesInLongArray(GridLimits: tGridLimits; var NPts : int64; var Values : bfarray32; IncludeSeaLevel : boolean = true);
var
   Col,Row,Incr : integer;
   SlopeAspectRec : tSlopeAspectRec;
begin
   NPts := 0;
   Incr := GetSamplingSize(GridLimits);
   Incr := Incr * MDDef.StatSampleIncr;
   Col := GridLimits.XGridLow;
   while (Col <= GridLimits.XGridHigh) do begin
      Row := GridLimits.YGridLow;
      while (Row <= GridLimits.YGridHigh) do begin
         if (IncludeSeaLevel) or (not SeaLevelCell(Col,Row)) then begin
            if GetSlopeAndAspect(Col,Row,SlopeAspectRec) then begin
               Values[Npts] := SlopeAspectRec.SlopePercent;
               inc(NPts);
            end;
         end;
         inc(Row,Incr);
      end;
      inc(Col,Incr);
   end;
end;


function tDEMDataSet.SlopePercent(XGrid,YGrid : integer; var Slope : float64; Radius : integer) : boolean;
var
   SlpAsp : tSlopeAspectRec;
begin
   Result := GetSlopeAndAspect(xgrid,ygrid,SlpAsp,false,Radius);
   if Result then Slope := SlpAsp.SlopePercent else Slope := -9999;
end;


function tDEMDataSet.GetSlopeAndAspectFromLatLong(Lat,Long : float64; var SlopeAspectRec : tSlopeAspectRec) : boolean;
var
   xg,yg  : integer;
begin
    LatLongDegreeToDEMGridInteger(Lat,Long,xg,yg);
    Result := GetSlopeAndAspect(xg,yg,SlopeAspectRec,false);
end;


function tDEMDataSet.SlopePercentFromLatLong(Lat,Long : float64) : float64;
var
   xgrid,ygrid : float64;
   SlopeAsp : tSlopeAspectRec;
begin
   LatLongDegreeToDEMGrid(Lat,Long,xgrid,ygrid);
   GetSlopeAndAspect(round(xgrid),round(ygrid),SlopeAsp,false);
   Result := SlopeAsp.SlopePercent;
end;


function tDEMDataSet.QuickRelief(Col,Row,BoxSize : integer; var Relief,Summit,BaseLevel,GeoRelief,Dropoff,Elev_Relf : float32) : boolean;
var
   Limits : tGridLimits;
begin
   SimpleBoxAroundPoint(Col,Row,BoxSize,Limits.XGridLow,Limits.YGridLow,Limits.XGridHigh,Limits.YGridHigh);
   Result := QuickRelief(Col,Row,Limits,Relief,Summit,BaseLevel,GeoRelief,Dropoff,Elev_relf);
end;


function tDEMDataSet.QuickRelief(Col,Row : integer; Limits : tGridLimits; var Relief,Summit,BaseLevel,GeoRelief,Dropoff,Elev_Relf : float32) : boolean;
var
   z,ave : float32;
begin
   Result := false;
   if GetElevMeters(Col,Row,z) then begin
      BoxAreaExtremeElevations(Limits,BaseLevel,Summit,ave);
      Relief := Summit - Baselevel;
      GeoRelief := Summit - z;
      Dropoff := z - BaseLevel;
      Elev_relf := (ave-BaseLevel) / (Summit - BaseLevel);
      Result := true;
   end;
end;


function tDEMDataSet.PointHasSpecifiedRelief(Col,Row,BoxSize,SampleFactor : integer; Relief : float64) : boolean;
var
   x1,x2,y1,y2,x,y : integer;
   z,zt : float32;
begin
   SimpleBoxAroundPoint(Col,Row,BoxSize,x1,y1,x2,y2);
   if GetElevMeters(Col,Row,zt) then begin
      x := x1;
      while x <= x2 do begin
        y := y1;
        while y <= y2 do begin
           if GetElevMeters(x,y,z) and (zt > z + Relief) then begin
             Result := true;
             exit;
            end;
            inc(y,SampleFactor);
        end;
        inc(x,SampleFactor);
      end;
   end;
   Result := false;
end;


function tDEMDataSet.GetRelief(Col,Row,BoxSize : integer; var AvgElev,Relief,ElevStdDev,PClower,TPI : float32) : boolean;
var
   z,MinZ,MaxZ,zc : float32;
   NumLower,NumInBox,x1,x2,y1,y2,x,y : integer;
   zs : ^bfarray32;
   MomentVar : tMomentVar;
begin
   MinZ := MaxSmallInt;
   MaxZ := -MaxSmallInt;
   SimpleBoxAroundPoint(Col,Row,BoxSize,x1,y1,x2,y2);
   MomentVar.NPts := 0;
   NumLower := 0;
   NumInBox := 0;
   if GetElevMeters(Col,Row,zc) then begin
      New(zs);
      for x := x1 to x2 do begin
         for y := y1 to y2 do begin
            inc(NumInBox);
            if GetElevMeters(x,y,z) then  begin
               if (z > MaxZ) then MaxZ := z;
               if (z < MinZ) then MinZ := z;
               if (z < zc) then inc(NumLower);
               zs^[MomentVar.NPts] := z;
               inc(MomentVar.NPts);
             end
             else inc(MomentVar.Missing);
         end;
      end;
      Relief := MaxZ - MinZ;
      if EnoughPoints(MomentVar) then begin
         Moment(zs^,MomentVar,msAfterStdDev);
         Result := true;
         PCLower := 100 * NumLower / MomentVar.NPts;
         TPI:= (zc - AvgElev);
      end
      else Result := false;
      Dispose(zs);
      AvgElev := MomentVar.mean;
      ElevStdDev := MomentVar.std_dev;
   end
   else Result := false;
end;



procedure tDEMDataSet.BothOpennessMoments(GridLimits: tGridLimits; var UpOpenMoment,DownOpenMoment : tMomentVar);
var
   ups, downs : ^bfarray32;
begin
   New(ups);
   New(downs);
   GetBothOpennessInLongArray(GridLImits,UpOpenMoment.NPts,ups^,downs^,false);
   DownOpenMoment := UpOpenMoment;
   moment(ups^,UpOpenMoment,msBeforeMedian);
   moment(downs^,DownOpenMoment,msBeforeMedian);
   Dispose(ups);
   Dispose(downs);
end;


procedure tDEMDataSet.SlopeMoments(GridLimits: tGridLimits; var SlopeMoment : tMomentVar; MomentStop : tMomentStop = msAll);
var
   zvs : ^bfarray32;
begin
   New(zvs);
   SlopeMomentsWithArray(GridLimits,SlopeMoment,zvs^,MomentStop);
   Dispose(zvs);
end;


procedure tDEMDataSet.RoughnessMomentsWithArray(GridLimits: tGridLimits; var MomentVar : tMomentVar; var zvs : bfarray32; MomentStop : tMomentStop = msAll);
begin
   InitializeMomentVar(MomentVar);
   GetRoughnessInLongArray(GridLimits,MomentVar.Npts,zvs);
   moment(zvs,MomentVar,MomentStop);
end;


procedure tDEMDataSet.GetRoughnessInLongArray(GridLimits: tGridLimits; var NPts : int64; var Values : bfarray32);
var
   Col,Row,Incr : integer;
   Ruff1 : float32;
begin
   NPts := 0;
   Incr := GetSamplingSize(GridLimits);
   Incr := Incr * MDDef.StatSampleIncr;
   Col := GridLimits.XGridLow;
   while (Col <= GridLimits.XGridHigh) do begin
      Row := GridLimits.YGridLow;
      while (Row <= GridLimits.YGridHigh) do begin
          if RoughnessFromSlopeSTD(Col,Row,MDDef.RoughnessBox,Ruff1) then begin
            Values[Npts] := Ruff1;
            inc(NPts);
         end;
         inc(Row,Incr);
      end;
      inc(Col,Incr);
   end;
end;


procedure tDEMDataSet.SlopeMomentsWithArray(GridLimits: tGridLimits; var SlopeMoment : tMomentVar; var zvs : bfarray32; MomentStop : tMomentStop = msAll);
var
   Nover30,Nover50 : integer;
   //I: Integer;
begin
   nOver30 := 0;
   nOver50 := 0;
   InitializeMomentVar(SlopeMoment);
   GetSlopesInLongArray(GridLimits,SlopeMoment.NPts,zvs,false);

(*   for I := 0 to pred(SlopeMoment.NPts) do begin
      if (zvs[i] >= 30) then begin
         inc(nOver30);
         if zvs[i] >= 50 then inc(nOver50);
      end;
   end;
   Over30PercentSlope := 100.0 * nOver30 / SlopeMoment.NPts;
   Over50PercentSlope := 100.0 * nOver50 / SlopeMoment.NPts;
*)
   Moment(zvs,SlopeMoment,MomentStop);
end;


function tDEMDataSet.ElevationMoments(GridLimits: tGridLimits; MomentStop : tMomentStop = msAll) : tMomentVar;
var
   zvs : ^bfarray32;
begin
   New(zvs);
   ElevationMomentsWithArray(GridLimits,Result,zvs^,MomentStop);
   Dispose(zvs);
end;


procedure tDEMDataSet.ElevationMomentsWithArray(GridLimits: tGridLimits; var MomentVar : tMomentVar; var zvs : bfarray32; MomentStop : tMomentStop = msAll);
begin
   InitializeMomentVar(MomentVar);
   GetElevationsInLongArray(GridLimits,MomentVar.NPts,zvs,true);
   moment(zvs,MomentVar,MomentStop);
end;

procedure tDEMDataSet.ElevationStatistics(GridLimits: tGridLimits; var Mean,Std : float32; var NPts : int64);
var
   MomentVar : tMomentVar;
begin
   MomentVar := ElevationMoments(GridLimits,msAfterStdDev);
   Mean := MomentVar.mean;
   Std := MomentVar.std_dev;
   Npts := MomentVar.NPts;
end;


procedure tDEMDataSet.GetSlopeMeanStd(GridLimits: tGridLimits; var Mean,Std : float32);
var
   zvs : ^bfarray32;
   MomentVar : tMomentVar;
begin
   New(zvs);
   RoughnessMomentsWithArray(GridLimits,MomentVar, zvs^,msAfterStdDev);
   Dispose(zvs);
   Mean := MomentVar.mean;
   Std := MomentVar.std_dev;
end;

procedure tDEMDataSet.GetRoughnessMeanStd(GridLimits: tGridLimits; var Mean,Std : float32);
var
   zvs : ^bfarray32;
   MomentVar : tMomentVar;
begin
   New(zvs);
   SlopeMomentsWithArray(GridLimits,MomentVar,zvs^,msAfterStdDev);
   Dispose(zvs);
   Mean := MomentVar.mean;
   Std := MomentVar.std_dev;
end;


function tDEMDataSet.CreatePartDEMHistogram(GridLimits: tGridLimits) : TThisBaseGraph;
var
   Distributions,Legends : tStringList;
   Values : ^bfarray32;
   Max,Min,BinSize : float32;
   NPts : int64;
   fName : PathStr;
begin
   {$IfDef RecordDEMstats} WriteLineToDebugFile('tDEMDataSet.CreatePartDEMHistogramn in'); {$EndIf}
   Distributions := tStringList.Create;
   Legends := tStringList.Create;
   New(Values);
   GetElevationsInLongArray(GridLimits,NPts,Values^);
   HeapSort(NPts,Values^);
   Max := Values^[pred(NPts)];
   Min := Values^[0];
   fName := Petmar.NextFileNumber(MDtempDir,AreaName + '_' ,'.z');
   Distributions.Add(SaveSingleValueSeries(npts,Values^,fName));
   Legends.Add(AreaName);
   Dispose(Values);
   BinSize := (Max - Min) / 200;
   Result := CreateMultipleHistogram(MDDef.CountHistograms,Distributions,Legends,ElevUnitsAre(DEMHeader.ElevUnits),AreaName + ' histogram',200,Min,Max,BinSize);
   {$IfDef RecordDEMstats} WriteLineToDebugFile('tDEMDataSet.CreatePartDEMHistogramn out'); {$EndIf}
end;


function tDEMDataSet.CreateWholeDEMHistogram : TThisBaseGraph;
begin
   Result := CreatePartDEMHistogram(FullDEMGridLimits);
end;

{$EndIf}


function tDEMDataSet.ContourLineCrossing(x, y: integer; z : float64): boolean;
const
   b = 0.001;
var
  zs : array[0..8] of float32;
  i : integer;
begin
   Result := false;
   GetNineElevMeters(x,y,zs[1],zs[2],zs[3],zs[4],zs[0],zs[5],zs[6],zs[7],zs[8]);
   for i := 0 to 8 do zs[i] := zs[i] - z;
   if abs(zs[0]) < b then begin
      for i := 1 to 8 do begin
          if abs(zs[i] - zs[0]) > b then begin
             Result := true;
             exit;
          end;
      end;
   end
   else if zs[0] > 0 then begin
      for i := 1 to 8 do begin
          if zs[i] < 0 then begin
             Result := true;
             exit;
          end;
      end;
   end
   else if zs[0] < 0 then begin
      for i := 1 to 8 do begin
          if zs[i] > 0 then begin
             Result := true;
             exit;
          end;
      end;
   end;
end;


function tDEMDataSet.FigureEntropy : float64;
type
   tCountArray = array[0..5000] of LongInt;
   tCountReals = array[0..5000] of float64;
var
   CountArray : ^tCountArray;
   CountReals : ^tCountReals;
   NumValues,
   i,j,z : integer;
begin
   if (DEMheader.MaxElev - DEMheader.MinElev > 5000) or (DEMheader.DEMPrecision = FloatingPointDEM) then begin
      Result := -1;
      exit;
   end;
   New(CountArray);
   New(CountReals);
   for i := 0 to 5000 do begin
      CountArray^[i] := 0;
      CountReals^[i] := 0;
   end;

   NumValues := 0;
   for i := 1 to (DEMheader.NumCol-2) do begin
      for j := 1 to (DEMheader.NumRow-2) do begin
         z := SmallIntElevations[i]^[j];
         if (z <> MaxSmallInt) then begin
            inc(CountArray^[z-round(DEMheader.MinElev)]);
            inc(NumValues);
         end {if};
      end {for j};
   end {for i};

   Result := 0;
   for z := round(DEMheader.MaxElev) downto round(DEMheader.MinElev) do begin
      i := z - round(DEMheader.MinElev);
      if CountArray^[i] > 0 then Result := Result - CountArray^[i]/NumValues * ln(CountArray^[i]/NumValues);
   end;
   Dispose(CountArray);
   Dispose(CountReals);
end;



procedure tDEMDataSet.DisposeNormals;
var
   i : integer;
begin
   for i := 1 to 3 do CloseSingleDEM(Normals[i]);
end;


function tDEMDataSet.NormalAtPoint(Col,Row : integer; var n1,n2,n3 : float32) : boolean;
var
  SlopAsp : tSlopeAspectRec;
begin
   Result := GetSlopeAndAspect(Col,Row,SlopAsp);
   if Result then begin
      if (SlopAsp.SlopeDegree < 0.0001) then SlopAsp.AspectDir := 0;
      n1 := sinDeg(SlopAsp.SlopeDegree) * sinDeg(SlopAsp.AspectDir);
      n2 := sinDeg(SlopAsp.SlopeDegree) * cosDeg(SlopAsp.AspectDir);
      n3 := cosDeg(SlopAsp.SlopeDegree);
   end;
end;

function tDEMDataSet.DownhillVectorAtPoint(Col,Row : integer; var n1,n2,n3 : float32) : boolean;
var
  SlopAsp : tSlopeAspectRec;
begin
   Result := GetSlopeAndAspect(Col,Row,SlopAsp);
   if Result then begin
      if (SlopAsp.SlopeDegree < 0.0001) then SlopAsp.AspectDir := 0;
      SlopAsp.SlopeDegree := 90 - SlopAsp.SlopeDegree;
      n1 := sinDeg(SlopAsp.SlopeDegree) * sinDeg(SlopAsp.AspectDir);
      n2 := sinDeg(SlopAsp.SlopeDegree) * cosDeg(SlopAsp.AspectDir);
      n3 := cosDeg(SlopAsp.SlopeDegree);
   end;
end;

procedure tDEMDataSet.InitializeNormals(var NumPts : Integer);
var
   j,Col,Row     : integer;
   ns : array[1..3] of float32;
begin
   if ValidDEM(Normals[1]) then exit;

   {$If Defined(RecordDEMMemoryAllocations) or Defined(RecordNormalInit) or Defined(RecordSSO)} WriteLineToDebugFile('allocate tDEMDataSet.InitializeNormals, pointer size=' + IntToStr(DEMheader.NumRow * SizeOf(VectorType))); {$EndIf}
   for j := 1 to 3 do begin
      if not ValidDEM(Normals[j]) then begin
         Normals[j] := CloneAndOpenGridSetMissing(FloatingPointDEM,AreaName + '_normals_' + IntToStr(j),euUndefined);
      end;
   end;
      for Col := 0 to pred(DEMheader.NumCol) do begin
         for Row := 0 to pred(DEMheader.NumRow) do begin
            if NormalAtPoint(Col,Row,ns[1],ns[2],ns[3]) then begin
               for j := 1 to 3 do DEMGlb[Normals[j]].SetGridElevation(Col,Row,ns[j]);
            end;
         end;
      end;
   {$If Defined(RecordDEMMemoryAllocations) or Defined(RecordNormalInit)or Defined(RecordSSO)} WriteLineToDebugFile('allocated tDEMDataSet.InitializeNormals'); {$EndIf}
end;


function tDEMDataSet.FindReliefInflectionGraph(xg,yg : integer; var Distance,Relief : float64) : boolean;
var
   i,Plat : integer;
   Summit,BaseLevel,GeoRelief,ThisRelief,ThisDistance,Dropoff,AvgElev : float32;
begin
    i := Round(MDDef.FirstBoxSize / AverageSpace);
    Result := false;
    Relief := -12;
    repeat
       ThisDistance := i * AverageSpace;
       QuickRelief(xg,yg,round(ThisDistance),ThisRelief,Summit,BaseLevel,GeoRelief,Dropoff,AvgElev);
       if (ThisRelief > Relief) then begin
          Plat := 0;
          Relief := ThisRelief;
          Distance := ThisDistance;
       end
       else begin
          inc(Plat);
          if (Plat > MDDef.PlateauTolerance) then begin
             Result := true;
          end;
       end;
       inc(i);
    until Result or (i > DEMheader.NumCol) or (i > DEMheader.NumRow) or (ThisDistance > MDdef.LastBoxSize);
end;


procedure tDEMDataSet.FractalBox(GridLimits: tGridLimits; var FracDim,r : float32; SkipDraw : boolean = false; CloseGraph : boolean = false);
var
   Results : tStringList;
   DoResults : boolean;

   function DoBox(Col,Row,Step : integer; var area : float64) : boolean;
   var
      a,b,c,d : float32;
      e, w,x,y,z, o,p,q,r, sa,sb,sc,sd, aa,ab,ac,ad : float64;
      XSpace,YSpace,DiaSpace : float64;
   begin
      Result := false;
      //     d        c
      //     a        b
      try
         if GetElevMeters(Col,Row,a) and GetElevMeters(Col+Step,Row,b) and
            GetElevMeters(Col+Step,Row+Step,c) and GetElevMeters(Col,Row+Step,d) then begin
                e := 0.25 * (a + b + c + d);
                XSpace := Step;
                YSpace := Step;
                DiaSpace := 0.5 * sqrt_2 * Step;

                w := sqrt( sqr(a-b) + sqr(XSpace));
                x := sqrt( sqr(b-c) + sqr(YSpace));
                y := sqrt( sqr(c-d) + sqr(XSpace));
                z := sqrt( sqr(a-d) + sqr(YSpace));

                o := sqrt( sqr(a-e) + sqr(DiaSpace));
                p := sqrt( sqr(b-e) + sqr(DiaSpace));
                q := sqrt( sqr(c-e) + sqr(DiaSpace));
                r := sqrt( sqr(d-e) + sqr(DiaSpace));

                sa := 0.5 * (w + p + o);
                sb := 0.5 * (x + p + q);
                sc := 0.5 * (y + q + r);
                sd := 0.5 * (z + o + r);

                aa := sqrt( abs( sa * (sa - w) * (sa - p) * (sa - o)));
                ab := sqrt( abs( sb * (sb - x) * (sb - p) * (sb - q)));
                ac := sqrt( abs( sc * (sc - y) * (sc - q) * (sc - r)));
                ad := sqrt( abs( sd * (sd - z) * (sd - o) * (sd - r)));

                area := aa + ab + ac + ad;
                Result := true;
         end;
      except
         on Exception do begin
            Result := false;
            {$IfDef TriPrismErrors} MessageToContinue('TriPrism DoBox error'); {$EndIf}
         end;
      end;
   end;

   function DoStepArea(Step : integer) : float64;
   var
      Col,Row,Total,Missing : integer;
      MissingMult,EdgeMult,
      AreaInc    : float64;
   begin
      try
         Result := 0;
         Missing := 0;
         Total := 0;
         Col := GridLimits.XGridLow;
         while (Col < pred(GridLimits.XGridHigh-Step)) do begin
            Row := GridLimits.YGridLow;
            while (Row < pred(GridLimits.YGridHigh-Step))  do begin
               if DoBox(Col,Row,Step,AreaInc) then Result := Result + AreaInc
               else inc(Missing);
               inc(Total);
               inc(Row,Step);
            end;
            inc(Col,Step);
         end;
         if (Result > 0.00001) and (Total > Missing) then begin
            if (Missing > 0) then begin
               MissingMult := Total / (Total - Missing);
            end
            else MissingMult := 1;
            EdgeMult := (GridLimits.XGridHigh-GridLimits.XGridLow) * (GridLimits.YGridHigh-GridLimits.YGridLow) /  (Total * Step * Step);
            if DoResults then Results.Add(IntegerToString(Total,8) + IntegerToString(Missing,8) + RealToString(Step * AverageSpace,8,1) + RealToString(MissingMult,10,2)
                 + RealToString(EdgeMult,8,2) + RealToString(Result,18,2) + RealToString(Result * MissingMult * EdgeMult,18,2));
            Result := Result * MissingMult * EdgeMult;
         end;
      except
         on Exception do begin
            Result := 0.0;
            {$IfDef TriPrismErrors} MessageToContinue('TriPrism DoStepArea error'); {$EndIf}
         end;
      end;
   end;

var
   Step,n,Valid : integer;
   a,b,Area : float32;
   Graph :  TThisBaseGraph;
   v         : array[1..2] of float32;
   TStr      : ShortString;
   rFile : file;
begin
   {$IfDef TriPrismResults}
      DoResults := true;
      WriteLineToDebugFile('');
   {$Else}
      DoResults := (not SkipDraw);
   {$EndIf}
   ShowHourglassCursor;
    try
      try
         Graph := TThisBaseGraph.Create(Application);
         //Graph.GraphDraw.SkipDrawing := SkipDraw;
         if DoResults then begin
            Results := tStringList.Create;
            Results.Add('Triangular prism fractal dimension, ' + AreaName);
            Results.Add('');
            Results.Add('  Boxes   Holes   Size (m)   Hole    Edge     Raw area         Corrected Area');
            Results.Add('==============================================================================');
         end;

         with Graph,GraphDraw do begin
            VertLabel := 'log(surface area)';
            HorizLabel := 'log(resolution area)';
            OpenDataFile(rfile);
            Valid := 0;
            Step := 1;
            repeat
               Area := DoStepArea(Step);
               if (Area > 0.0001) then begin
                  v[1] := ln(sqr(Step));
                  v[2] := ln(area);
                  BlockWrite(rfile,v,1);
                  inc(Valid);
               end;
               Step := Step * 2;
            until (Step > DEMheader.NumCol) or (Step > DEMheader.NumRow);
            CloseFile(RFile);
         end;

         Graph.AutoScaleAndRedrawDiagram;
         if (Valid >= 4) then begin
            Graph.FitGraph(true,2,Graph.GraphDraw.DataFilesPlotted.Strings[0], a,b,r,n);
            FracDim := (2 - b);
            TStr := RealToString(FracDim,-6,2) + ' ';
            if DoResults then begin
               Results.Add('');
               Results.Add('Fractal dimension: ' + TStr);
            end;
         end
         else begin
            FracDim := -999;
            TStr := '';
            {$IfDef TriPrismErrors} MessageToContinue('Not enough valid (' + IntToStr(Valid) + ')'); {$EndIf}
         end;

         if DoResults then begin
            if not SkipDraw then begin
               Graph.Caption := 'Triangular prism fractal, ' + TStr + AreaName;
               Petmar.DisplayAndPurgeStringList(Results,'Triangular Prism Fractal, ' + AreaName);
            end;
            {$IfDef TriPrismResults}
               WriteStringListToDebugFile(Results);
               Results.Free;
            {$EndIf}
         end;
      except
          on Exception do begin
            FracDim := -999;
            {$IfDef TriPrismErrors} MessageToContinue('TriPrism overall exception'); {$EndIf}
          end;
      end;
   finally
      if CloseGraph and (Graph <> Nil) then begin
         Graph.Close;
         Graph.Free;
      end;
   end;
   ShowDefaultCursor;
end;


procedure tDEMDataSet.EntireDEMFractalBox;
var
   FracDim,r  : float32;
begin
   FractalBox(FullDEMGridLimits,FracDim,r);
end;


procedure tDEMDataSet.VariogramGamma(GridLimits: tGridLimits; var EastWest,NorthSouth,NESW,NWSE : float32);
var
   Num,Col,Row : integer;
   Elev  : tElevFloatArray;
begin
   ClipDEMGridInteger(GridLimits.XGridLow,GridLimits.YGridLow);
   ClipDEMGridInteger(GridLimits.XGridHigh,GridLimits.YGridHigh);
   Num := 0;
   EastWest := 0;
   NorthSouth := 0;
   NESW := 0;
   NWSE := 0;
   for Col := GridLimits.XGridLow to pred(GridLimits.XGridHigh) do begin
      for Row := GridLimits.YGridLow to pred(GridLimits.YGridHigh) do begin
          if GetElevSquareMeters(Col + 0.5, Row - 0.5,Elev) then begin
             inc(Num);
             EastWest := EastWest + sqr(Elev[4]-Elev[3]);
             NorthSouth := NorthSouth + sqr(Elev[4]-Elev[1]);
             NESW := NESW + sqr(Elev[3]-Elev[1]);
             NWSE := NWSE + sqr(Elev[4]-Elev[2]);
          end;
      end;
   end;
   EastWest := EastWest / Num / sqr(AverageXSpace);
   NorthSouth := NorthSouth / Num / sqr(AverageYSpace);
   NESW := NESW / Num / sqr(AverageDiaSpace);
   NWSE := NWSE / Num / sqr(AverageDiaSpace);
end;


procedure tDEMDataSet.ComputeVariogram(GridLimits: tGridLimits);  //SkipDrawing : boolean);
const
   MaxDist = 5000;
type
   SVarrayType = array[0..MaxDist] of float32;
var
   ThisGraph : tThisBaseGraph;
   SVArray,DistX : array[1..4] of SVArrayType;
   Dist,MaxHoriz,n,Col,Row,i,j : integer;
   NumPts : array[1..4,0..MaxDist] of LongInt;
   v1,v2,v3,MaxSemiVar,dFactor, a,b,r,EastWest,NorthSouth,NESW,NWSE,
   SlopeEastWest,SlopeNorthSouth,SlopeNESW,SlopeNWSE : float32;
   z,zp   : float32;
   Rfile  : file;
   v      : array[1..2] of float32;
   TStr   : ShortString;
   Results : tStringList;


    procedure CheckNeighbor(DoCol,DoRow,i,DirInt : integer);
    begin
       if GetElevMeters(DoCol,DoRow,z) then begin
          SVArray[DirInt][i] := SVArray[DirInt][i] + sqr(zp - z);
          inc(NumPts[DirInt,i]);
       end;
    end {proc};


    procedure DistanceArray;
    var
       i : integer;
    begin
      if MDDef.VariogramOptionsRecord.OldMethod then begin
         for i := 1 to Dist do begin
            DistX[1][i] := dFactor * i * AverageYSpace;
            DistX[2][i] := dFactor * i * AverageXSpace;
            DistX[3][i] := dFactor * i * AverageDiaSpace;
            DistX[4][i] := dFactor * i * AverageDiaSpace;
         end;
      end;
    end;


begin
   {$IfDef RecordVariogram} WriteLineToDebugFile('tDEMDataSet.ComputeVariogram in for ' + AreaName);{$EndIf}
   with MDDef.VariogramOptionsRecord do if DoGraph or DoSlopes then begin
      Dist := round(DistanceOut / AverageXSpace);

      for j := 1 to 4 do begin
         for i := 0 to Dist do begin
            SVArray[j][i] := 0;
            NumPts[j,i] := 0;
         end {for i};
      end {for j};

      OldMethod := true;

      Col := GridLimits.XGridLow;
      if ShowSatProgress then StartProgress('Variogram ' + AreaName);
      while Col <= GridLimits.XGridHigh do begin
         if ShowSatProgress then UpdateProgressBar((Col-GridLimits.XGridLow)/(GridLimits.XGridHigh-GridLimits.XGridLow));
         Row := GridLimits.YGridLow;
         while Row <= GridLImits.YGridHigh do begin
            if GetElevMeters(Col,Row,zp) then begin
               if OldMethod then begin
                  i := 1;
                  while (i <= Dist) do begin
                     CheckNeighbor(Col,Row+i,i,1);
                     CheckNeighbor(Col,Row-i,i,1);
                     CheckNeighbor(Col+i,Row,i,2);
                     CheckNeighbor(Col-i,Row,i,2);
                     CheckNeighbor(Col+i,Row+i,i,3);
                     CheckNeighbor(Col-i,Row-i,i,3);
                     CheckNeighbor(Col-i,Row+i,i,4);
                     CheckNeighbor(Col+i,Row-i,i,4);
                     inc(i,GraphSkip);
                  end {for i};
               end;
            end {if};
            inc(Row,Skip);
         end {while};
         inc(Col,Skip);
      end {while};
      EndProgress;

      dFactor := 1;
      DistanceArray;

     {$IfDef RecordVariogram} WriteLineToDebugFile('tDEMDataSet.ComputeVariogram normalize'); {$EndIf}
      MaxSemiVar := 0;
      MaxHoriz := 0;
      for j := 1 to 4 do begin
         i := 1;
         while (i <= Dist) do begin
            if (NumPts[j,i] < PointsRequired) then SVArray[j][i] := MaxInt
            else begin
               {$IfDef RecordVariogramFull} WriteLineToDebugFile(IntegerToString(i,6) + IntegerToString(j,6) + RealToString(SVArray[j][i],18,2) + IntegerToString(NumPts[j,i],8)); {$EndIf}
               v1 := SVArray[j][i];
               v2 := NumPts[j,i];
               v3 := DistX[j][i];
               SVArray[j][i] := 0.5 * SVArray[j][i] / NumPts[j,i] / DistX[j][i];
               if SemiVar then SVArray[j][i] := sqrt(SVArray[j][i]);
               if (SVArray[j][i] > MaxSemiVar) then MaxSemiVar := SVArray[j][i];
               if (i > MaxHoriz) then MaxHoriz := 1;
            end;
            inc(i,GraphSkip);
         end {while};
      end {for j};
      {$IfDef RecordVariogram} WriteLineToDebugFile('tDEMDataSet.ComputeVariogram graph'); {$EndIf}
      ThisGraph := TThisBaseGraph.Create(Application);
      //ThisGraph.GraphDraw.SkipDrawing := SkipDrawing;
      TStr := AreaName;
      ThisGraph.GraphDraw.VertLabel := 'Variance (m)';
      if SemiVar then ThisGraph.GraphDraw.VertLabel := 'Semi ' + ThisGraph.GraphDraw.VertLabel;
      if LogLog then begin
         ThisGraph.GraphDraw.HorizLabel := 'Log Distance (m)';
         DFactor := 1;
         ThisGraph.GraphDraw.VertLabel := 'Log ' + ThisGraph.GraphDraw.VertLabel;
      end
      else begin
         DFactor := 0.001;
         ThisGraph.GraphDraw.HorizLabel := 'Distance (km)';
      end;
      DistanceArray;

      {if (not BaseGraf.CreateGraphHidden) then} ThisGraph.Caption := TStr + ' Semivariograms 4 directions';
      ThisGraph.GraphDraw.LegendList := tStringList.Create;
      ThisGraph.GraphDraw.LegendList.Add('N-S');
      ThisGraph.GraphDraw.LegendList.Add('E-W');
      ThisGraph.GraphDraw.LegendList.Add('NE-SW');
      ThisGraph.GraphDraw.LegendList.Add('NW-SE');

      {$IfDef RecordVariogram} WriteLineToDebugFile('tDEMDataSet.ComputeVariogram create files'); {$EndIf}
      for i := 1 to 4 do begin
         ThisGraph.GraphDraw.FileColors256[1] := ConvertTColorToPlatformColor(WinGraphColors[i]);
         ThisGraph.OpenDataFile(Rfile);
         j := 1;
         while (j <= Dist) do begin
            v[2] := SVArray[i][j];
            if v[2] < pred(MaxInt) then begin
               v[1] := DistX[i][j];
               if v[1] < dFactor * DistanceOut then begin
                  if LogLog then begin
                     v[1] := log10(v[1]);
                     v[2] := log10(v[2]);
                  end;
                  BlockWrite(rfile,v,1);
               end;
            end;
            inc(j,GraphSkip);
         end;
         CloseFile(Rfile);
      end;
      {$IfDef RecordVariogram} WriteLineToDebugFile('tDEMDataSet.ComputeVariogram autodraw'); {$EndIf}
      ThisGraph.AutoScaleAndRedrawDiagram;
   end;

   Results := Nil;
   if MDDef.VariogramOptionsRecord.DoSlopes or MDDef.VariogramOptionsRecord.DoGamma then begin
      Results := tStringList.Create;
   end;

   if MDDef.VariogramOptionsRecord.DoSlopes then begin
      SlopeNorthSouth := -999;
      SlopeEastWest   := -999;
      SlopeNESW       := -999;
      SlopeNWSE       := -999;
      for i := 0 to pred(ThisGraph.GraphDraw.DataFilesPlotted.Count) do begin
         ThisGraph.BestSegmentFitGraph(2,ThisGraph.GraphDraw.DataFilesPlotted.Strings[i], a,b,r,n,3,0.10);
         if (b > 0.01) and (b < 10) then case i  of
            0 : SlopeNorthSouth := b;
            1 : SlopeEastWest   := b;
            2 : SlopeNESW       := b;
            3 : SlopeNWSE       := b;
         end;
      end;
      Results.Add('Variogram Slopes and fractal Dimensions, ' + AreaName);
      Results.Add('N-S:'+ RealToString(SlopeNorthSouth,10,4) + RealToString(FracDimFromSlope2(SlopeNorthSouth),10,3));
      Results.Add('NE-SW:'+ RealToString(SlopeNESW,8,4) + RealToString(FracDimFromSlope2(SlopeNESW),10,3));
      Results.Add('E-W:'+ RealToString(SlopeEastWest,10,4) + RealToString(FracDimFromSlope2(SlopeEastWest),10,3));
      Results.Add('SE-NW:'+ RealToString(SlopeNWSE,8,4) + RealToString(FracDimFromSlope2(SlopeNWSE),10,3));
      Results.Add('');
      {$IfDef RecordVariogram} WriteStringListToDebugFile(Results); {$EndIf}
      if not MDDef.VariogramOptionsRecord.DoGraph then ThisGraph.Close;
   end;

   if MDDef.VariogramOptionsRecord.DoGamma then begin
      VariogramGamma(GridLimits,EastWest,NorthSouth,NESW,NWSE);
      Results.Add('Variogram Gammas, ' + AreaName);
      Results.Add('N-S:'+ RealToString(NorthSouth,10,4));
      Results.Add('NE-SW:'+ RealToString(NESW,8,4));
      Results.Add('E-W:'+ RealToString(EastWest,10,4));
      Results.Add('SE-NW:'+ RealToString(NWSE,8,4));
   end;
   if (Results <> Nil) then begin
      if MDDef.VariogramOptionsRecord.ShowTextOutput then Petmar.DisplayAndPurgeStringList(Results,'Variogram ' + AreaName)
      else Results.Free;
   end;
end;



{$IfDef ExDEMReports}
{$Else}
procedure tDEMDataSet.PointParameters(xgrid,ygrid : float64);
var
   Results,Results2 : tStringList;
   i,Col,Row : integer;
   {$IfDef MultipleCurvatureMethods} PlanCurv,SlopeCurv : float64; {$EndIf}
   Upward,Downward : float64;
   z : float32;
   SlopeAspectRec : tSlopeAspectRec;
begin
   {$If Defined(TimePointParameters)} WriteLineToDebugFile('tDEMDataSet.PointParameters in'); {$EndIf}
   ShowHourGlassCursor;
   Col := round(xgrid);
   Row := round(ygrid);
   Results := tStringList.Create;
   Results.Add(DEMLocationString(xgrid,ygrid));
   Results.Add('');
   if GetElevMeters(XGrid,YGrid,z) then Results.Add('Elev: ' + RealToString(z,8,2) + ' m');

   if GetSlopeAndAspect(Col,Row,SlopeAspectRec) then begin
      Results.Add('Slope:' + RealToString(SlopeAspectRec.SlopePercent,7,2) + '%' + RealToString(SlopeAspectRec.SlopeDegree,8,2) + '°');
      Results.Add('Aspect: ' + RealToString(SlopeAspectRec.AspectDir,6,1) + '°');
   end;
   {$If Defined(TimePointParameters)} WriteLineToDebugFile('slope/aspect done'); {$EndIf}


   if RoughnessFromSlopeSTD(Col,Row,3,z) then Results.Add('Roughness 3x3: ' + RealToString(z,8,2) + '%');
   if RoughnessFromSlopeSTD(Col,Row,5,z) then Results.Add('Roughness 5x5: ' + RealToString(z,8,2) + '%');
   if RoughnessFromSlopeSTD(Col,Row,7,z) then Results.Add('Roughness 7x7: ' + RealToString(z,8,2) + '%');
   if RoughnessFromSlopeSTD(Col,Row,9,z) then Results.Add('Roughness 9x9: ' + RealToString(z,8,2) + '%');
   if RoughnessFromSlopeSTD(Col,Row,11,z) then Results.Add('Roughness 11x11: ' + RealToString(z,6,2) + '%');
   {$If Defined(TimePointParameters)} WriteLineToDebugFile('roughness done'); {$EndIf}

   {$If Defined(MultipleCurvatureMethods) and Defined(CurvaturePointParameters)}
      //this one is off by default, since curvature is so messy
      if GetCurvature(round(xgrid),round(ygrid),PlanCurv,SlopeCurv) then begin
         Results.Add('Plan curvature: ' + RealToString(PlanCurv,12,6));
         Results.Add('Slope curvature:' + RealToString(SlopeCurv,12,6));
      end;
     {$If Defined(TimePointParameters)} WriteLineToDebugFile('curvature done'); {$EndIf}
   {$EndIf}

   {$IfDef ExGeostats}
   {$Else}
      {$IfDef MultipleCurvatureMethods}
         //this one is off by default, since curvature is so messy
         if GetEvansParams(Col,Row,MDDef.WoodRegionRadiusPixels,MaxSlp,SlopeCurvature,PlanCurvature,crossc,MaxCurve,MinCurve) then begin
            Results.Add('');
            Results.Add('Evans curvature (Region size=' + IntToStr(MDDef.WoodRegionRadiusPixels) + '):');
            Results.Add('   Plan curvature: ' + RealToString(PlanCurvature,13,6));
            Results.Add('   Slope curvature:' + RealToString(SlopeCurvature,12,6));
            Results.Add('   Cross curvature:' + RealToString(CrossC,12,6));
            Results.Add('   Max curvature: ' + RealToString(MaxCurve,14,6));
            Results.Add('   Min curvature:' + RealToString(MinCurve,14,6));
            {$If Defined(TimePointParameters)} WriteLineToDebugFile('Evans done'); {$EndIf}
         end;
      {$EndIf}

      Results2 := tStringList.Create;
      if FigureOpenness(Col,Row,MDDef.OpenGridBoxSize,Upward,Downward,Results2) then begin
         Results.Add('');
         Results.Add('Openness parameters (' + IntToStr(MDDef.OpenGridBoxSize) + ' m)');
         Results.Add('  Upward openness: ' + RealToString(Upward,12,2) + '°');
         Results.Add('  Downward openness:' + RealToString(Downward,12,2) + '°');
         Results.Add('');
         for i := 0 to pred(Results2.Count) do Results.Add('      ' + Results2.Strings[i]);
         Results.Add('');
         {$If Defined(TimePointParameters)} WriteLineToDebugFile('Openness done'); {$EndIf}
      end;
      Results2.Free;

      {$If Defined(PointSSOPointParameters)}
         //this one is off by default, since it is so slow for lidar
         if PointSSOComputations(Col,Row,MDDef.SSOBoxSizeMeters, SSOvars,false,true,false) then  begin
            Results.Add('SSO parameters (' + IntToStr(MDDef.SSOBoxSizeMeters) + ' m)');
            Results.Add('  Points in box: ' + IntToStr(SSOvars.NumPts));
            Results.Add('  Organization ln(s2/s3): ' + RealToString(SSOvars.s2s3,12,2));
            Results.Add('  Organization direction: ' + RealToString(SSOvars.TheDipDirs[3],6,1) + '°');
            Results.Add('  Relief: ' + RealToString(SSOvars.ElevRange,12,2) + ' m');
            Results.Add('  Region average elev: ' + RealToString(SSOvars.AvgElev,12,2) + ' m');
            Results.Add('  Region elev std dev: ' + RealToString(SSOvars.StdDevElev,12,2));
            Results.Add('  Region max slope: ' + RealToString(100*SSOvars.MaxSlope,12,2) + '%');
            Results.Add('  Roughness factor: ' + RealToString(SSOvars.RoughnessFactor,8,3));
            {$If Defined(TimePointParameters)} WriteLineToDebugFile('SSO done'); {$EndIf}
         end;
      {$EndIf}
   {$EndIf}
   ModalStringListDisplay(Results,'Point parameters');
   ShowDefaultCursor;
   {$If Defined(TimePointParameters)} WriteLineToDebugFile('tDEMDataSet.PointParameters out'); {$EndIf}
end;


procedure tDEMDataSet.SlopeMethodsReportFromLatLong(lat,Long : float64; Title : shortstring = '');
var
   Col,Row : integer;
begin
   if LatLongDegreeToDEMGridInteger(Lat,Long,Col,Row) then SlopeMethodsReport(Col,Row,Title);
end;


procedure tDEMDataSet.SlopeMethodsReport(xp,yp : integer; Title : shortstring = '');
{$IfDef ExGeostats}
begin
{$Else}
var
   Results : tStringList;
   CurMethod : byte;
   SlopeAspectRec : tSlopeAspectRec;
begin
   SaveBackupDefaults;
   Results := tStringList.Create;
   Results.Add(AreaName);
   if (Title <> '') then Results.Add(Title);
   Results.Add('Point: ' + DEMLocationString(xp,yp));
   Results.Add('Classified as: ' + PointTypeName(ClassifyAPoint(xp,yp)));
   Results.Add('Col=' + IntToStr(xp) + '  & row=' + IntToStr(yp));
   Results.Add('');
   Results.Add('Neighborhood elevations (m)');
   GetSlopeAndAspect(xp,yp,SlopeAspectRec);
   Results.Add(  RealToString( SlopeAspectRec.znw,13,2) + RealToString( SlopeAspectRec.zn,13,2) + RealToString( SlopeAspectRec.zne,13,2) +  MessLineBreak +
                 RealToString( SlopeAspectRec.zw,13,2)  + RealToString( SlopeAspectRec.z,13,2)   + RealToString( SlopeAspectRec.ze,13,2)  +  MessLineBreak +
                 RealToString( SlopeAspectRec.zsw,13,2) + RealToString( SlopeAspectRec.zs,13,2)  + RealToString( SlopeAspectRec.zse,13,2) +  MessLineBreak + '  ' + MessLineBreak );
   Results.Add('');
   Results.Add('         Slope             Aspect         Method');
   for CurMethod := FirstSlopeMethod to LastSlopeMethod do begin
      MDDef.SlopeAlgorithm := CurMethod;
      GetSlopeAndAspect(xp,yp,SlopeAspectRec);
      Results.Add(RealToString(SlopeAspectRec.SlopePercent,8,2) + '%   ' + RealToString(SlopeAspectRec.SlopeDegree,8,2) + '°   ' + RealToString(SlopeAspectRec.AspectDir,8,1) + '°    ' + SlopeMethodName(MDDef.SlopeAlgorithm));
   end;
   DisplayAndPurgeStringList(Results,'Point Slope Algorithms');
   RestoreBackupDefaults
{$EndIf}
end;


procedure tDEMDataSet.DirectionalSlopesReport(Lat,Long : float64);
var
   MenuStr : AnsiString;
   x,y : integer;
   xgf,ygf : float64;
   SlopeAspectRec : tSlopeAspectRec;
begin
   LatLongDegreeToDEMGrid(Lat,Long,xgf,ygf);
   x := round(xgf);
   y := round(ygf);

   GetSlopeAndAspect(x,y,SlopeAspectRec);
   MenuStr := 'Point: ' + DEMLocationString(x,y) +  MessLineBreak +  MessLineBreak + 'Elevations: ' +  MessLineBreak +
              RealToString( SlopeAspectRec.znw,13,2) + RealToString( SlopeAspectRec.zn,13,2) + RealToString( SlopeAspectRec.zne,13,2) +  MessLineBreak +
              RealToString( SlopeAspectRec.zw,13,2)  + RealToString( SlopeAspectRec.z,13,2)   + RealToString( SlopeAspectRec.ze,13,2)  +  MessLineBreak +
              RealToString( SlopeAspectRec.zsw,13,2) + RealToString( SlopeAspectRec.zs,13,2)  + RealToString( SlopeAspectRec.zse,13,2) +  MessLineBreak + '  ' + MessLineBreak +
              'Slopes: ' +  MessLineBreak  +
              RealToString( 100*(SlopeAspectRec.znw-SlopeAspectRec.z)/AverageDiaSpace,12,2) + '%' + RealToString( 100*(SlopeAspectRec.zn-SlopeAspectRec.z)/AverageYSpace,12,2) + '%' + RealToString( 100*(SlopeAspectRec.zne-SlopeAspectRec.z)/AverageDiaSpace,12,2) + '%' +  MessLineBreak +
              RealToString( 100*(SlopeAspectRec.zw-SlopeAspectRec.z)/AverageXSpace,12,2) + '%' + '             ' + RealToString( 100*(SlopeAspectRec.ze-SlopeAspectRec.z)/AverageXSpace,12,2) + '%' +  MessLineBreak +
              RealToString( 100*(SlopeAspectRec.zsw-SlopeAspectRec.z)/AverageDiaSpace,12,2) + '%' + RealToString( 100*(SlopeAspectRec.zs-SlopeAspectRec.z)/AverageYSpace,12,2) + '%' + RealToString( 100*(SlopeAspectRec.zse-SlopeAspectRec.z)/AverageDiaSpace,12,2) + '%'+  MessLineBreak + '  ' +
              MessLineBreak +
              'Point slope:' + MessLineBreak +   RealToString(SlopeAspectRec.SlopePercent,8,2) + '%   ' + RealToString(SlopeAspectRec.SlopeDegree,8,2) + '°   aspect ' + RealToString(SlopeAspectRec.AspectDir,8,1) + '°    ' +
                   SlopeMethodName(MDDef.SlopeAlgorithm);
   MessageToContinue(MenuStr);
end;

{$EndIf}

procedure tDEMDataSet.BoxStatsDB(BoxSize : integer = 0);
const
   MaxSize = 50000;
var
   Results : tStringList;
   Col,Row,x,y : integer;
   z1: float32;
   Lat,Long : float64;
   MomentVar : tMomentVar;
   fName : PathStr;
   xs : array[0..MaxSize] of float32;
begin
   if (BoxSize = 0) then ReadDefault('Box size (grid cells)',BoxSize);

   Results := tStringList.Create;
   Results.Add('LAT,LONG,MEAN,STD_DEV,MEDIAN,N');

   Col := 0;
   StartProgress('DB create');
   while Col <= DEMheader.NumCol do begin
      UpdateProgressBar(Col/DEMheader.NumCol);
      Row := 0;
      while Row <= DEMheader.NumRow do begin
         MomentVar.NPts := 0;
         for x := Col to Col + pred(BoxSize) do begin
            for y := Row to Row + pred(BoxSize) do begin
               if GetElevMeters(x,y,z1) then begin
                  xs[MomentVar.NPts] := z1;
                  inc(MomentVar.NPts);
               end;
            end;
         end;
         if (MomentVar.NPts > 0) then begin
            Moment(xs,MomentVar,msAll);
            DEMGridToLatLongDegree(Col + succ(Boxsize div 2), Row + succ(Boxsize div 2),Lat,Long);
            Results.Add(RealToString(Lat,-12,-7) + ',' + RealToString(Long,-12,-7) + ',' + RealToString(MomentVar.mean,-12,-2) + ',' + RealToString(MomentVar.std_dev,-12,-2) + ',' + RealToString(MomentVar.Median,-12,-2) + ',' + IntToStr(MomentVar.NPts));
         end;
         inc(Row,BoxSize);
      end;
      inc(Col,BoxSize);
   end;
   EndProgress;
   fName := System.IOUtils.TPath.Combine(MDTempDir, AreaName + '_stats.csv');

   {$IfDef NoMapOptions}
   {$Else}
      SelectionMap.StringListToLoadedDatabase(Results,fName);
   {$EndIf}
end;


{$IfDef AllowDEMGeomorph}
   procedure tDEMDataSet.GetBoxGridSizeDiameter(BoxSizeMeters : integer; var XBoxGridSize,YBoxGridSize : integer; var BoxSizeString : shortstring);
   begin
       XBoxGridSize := round(BoxSizeMeters / AverageXSpace);
       YBoxGridSize := round(BoxSizeMeters / AverageYSpace);
       BoxSizeString := ' (' +  IntToStr(BoxSizeMeters) + ' m)' ;
   end;
{$EndIf}


function tDEMDataSet.InTerrainCategory(x,Y : integer; TerrainCategory : tTerrainCatDefinition) : boolean;
var
   Relief,Summit,BaseLevel,GeoRelief,Dropoff,AvgElev,z : float32;
   SlopeAspectRec : tSlopeAspectRec;
begin
   Result := false;
   if GetElevMeters(x,y,z) then with TerrainCategory do begin
      if (not ElevationDEM) then Result := (z >= CatMinElev) and (z <= CatMaxElev)
      else begin
         Result := true;
         if UseElevation then Result := (z >= CatMinElev) and (z <= CatMaxElev);
         if UseSlope or UseAspect then begin
            if not GetSlopeAndAspect(x,y,SlopeAspectRec) then begin
               Result := false;
               exit;
            end;
         end;
         if UseSlope then Result := Result and (SlopeAspectRec.SlopePercent >= CatMinSlope) and (SlopeAspectRec.SlopePercent <= CatMaxSlope);
         if UseRelief then begin
            QuickRelief(x,y,CatReliefRadius,Relief,Summit,BaseLevel,GeoRelief,Dropoff,AvgElev);
            Result := Result and (Relief >= CatMinRelief) and (Relief <= CatMaxRelief);
         end;
         if UseAspect then Result := Result and ((SlopeAspectRec.Dir in CatAspects) or (SlopeAspectRec.SlopePercent < 0.0001));
      end;
   end;
end;


function tDEMDataSet.IsSpire(Col,Row,dx,dy : integer; var SpireHeightM : float32; var NumLower : integer) : boolean;
var
   z,zp : float32;
   Npts : integer;

      function ElevMeters(x,y : integer) : float32;
      begin
         if GetElevMeters(x,y,z) then begin
             SpireheightM := SpireHeightM + z;
             Inc(Npts);
         end;
      end;

begin
    Result := false;
    if GetElevMeters(Col,Row,z) then begin
       NumLower := 0;
       if GetElevMeters(Col+dx,Row,zp) and (z > zp + MDDef.SpireHeight) then inc(NumLower);
       if GetElevMeters(Col-dx,Row,zp) and (z > zp + MDDef.SpireHeight) then inc(NumLower);
       if GetElevMeters(Col,Row+dy,zp) and (z > zp + MDDef.SpireHeight) then inc(NumLower);
       if GetElevMeters(Col,Row-dy,zp) and (z > zp + MDDef.SpireHeight) then inc(NumLower);
       if GetElevMeters(Col+dx,Row+dy,zp) and (z > zp + MDDef.SpireHeight) then inc(NumLower);
       if GetElevMeters(Col-dx,Row+dy,zp) and (z > zp + MDDef.SpireHeight) then inc(NumLower);
       if GetElevMeters(Col-dx,Row+dy,zp) and (z > zp + MDDef.SpireHeight) then inc(NumLower);
       if GetElevMeters(Col+dx,Row-dy,zp) and (z > zp + MDDef.SpireHeight) then inc(NumLower);
       Result := NumLower >= MDDef.SpireNeighborTolerance;
       if Result then begin
          SpireHeightM := 0;
          SpireHeightM := ElevMeters(Col+dx,Row) + ElevMeters(Col-dx,Row) + ElevMeters(Col,Row+dy) + ElevMeters(Col,Row-dy) + ElevMeters(Col+dx,Row+dy) + ElevMeters(Col-dx,Row+dy) + ElevMeters(Col-dx,Row+dy) + ElevMeters(Col+dx,Row-dy);
          SpireHeightM := SpireHeightM / NPts;
          SpireHeightM := z - SpireHeightM;
       end;
    end;
end;



{$IfDef ExGeostats}
{$Else}

procedure tDEMDataSet.GetBothOpennessInLongArray(GridLimits: tGridLimits; var NPts : int64;   var UpValues,DownValues : bfarray32; IncludeSeaLevel : boolean = true);
var
   Col,Row,Incr : integer;
   UpOpen,DownOpen : float64;
begin
   NPts := 0;
   Incr := GetSamplingSize(GridLimits);
   Col := GridLimits.XGridLow;
   while (Col <= GridLimits.XGridHigh) do begin
      Row := GridLimits.YGridLow;
      while (Row <= GridLimits.YGridHigh) do begin
         if (IncludeSeaLevel) or (not SeaLevelCell(Col,Row)) then begin
            if FigureOpenness(Col,Row,MDDef.OpenGridBoxSize,UpOpen,DownOpen) then begin
               UpValues[Npts] := UpOpen;
               DownValues[Npts] := DownOpen;
               inc(NPts);
            end;
         end;
         inc(Row,Incr);
      end;
      inc(Col,Incr);
   end;
end;


function tDEMDataSet.FindLocationOfMaximum(GridLimits: tGridLimits; var xloc,yloc : integer; var LocMax : float32) : boolean;
var
   z   : float32;
   x,y : integer;
begin
   LocMax := -99e39;
   for x := GridLimits.XGridLow to GridLimits.XGridHigh do begin
      for y := GridLimits.YGridLow to GridLimits.YGridHigh do begin
         if GetElevMeters(x,y,z) then begin
            if (z > LocMax) then begin
               LocMax := z;
               xloc := x;
               yloc := y;
            end;
         end;
      end;
   end;
   Result := (LocMax > -99e39);
end;


function tDEMDataSet.FindLocationOfMinimum(GridLimits: tGridLimits; var xloc,yloc : integer; var LocMin : float32) : boolean;
var
   z   : float32;
   x,y : integer;
begin
   LocMin := 99e39;
   for x := GridLimits.XGridLow to GridLimits.XGridHigh do begin
      for y := GridLimits.YGridLow to GridLimits.YGridHigh do begin
         if GetElevMeters(x,y,z) then begin
            if (z < LocMin) then begin
               LocMin := z;
               xloc := x;
               yloc := y;
            end;
         end;
      end;
   end;
   Result := (LocMin < 99e39);
end;


function tDEMDataSet.FindLocationOfMultipleMaxima(GridLimits: tGridLimits; var NPts : integer; var Locations : array of tGridZ) : boolean;
var
   i : integer;

      procedure ReEnterPoints(NPts : integer);
      var
         i : integer;
      begin
         for i := 0 to pred(Npts) do begin
            SetGridElevation(Locations[i].x,Locations[i].y,Locations[i].z);
         end;
      end;

begin
   for i := 0 to pred(Npts) do begin
      if not FindLocationOfMaximum(GridLimits,Locations[i].x,Locations[i].y,Locations[i].z) then begin
         Result := false;
         ReEnterPoints(i);
         NPts := i;
         exit;
      end;
      SetGridMissing(Locations[i].x,Locations[i].y);
   end;
   ReEnterPoints(NPts);
   Result := true;
end;






