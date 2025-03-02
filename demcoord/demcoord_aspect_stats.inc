{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{----------------------------------------}
{ include file for demcoord              }
{________________________________________}



{ tAspectStats }

function CreateAspectRose(DEM : integer) : tThisBaseGraph;
var
   AspectStats : tAspectStats;
begin
   AspectStats.Create(DEM);
   AspectStats.FillFromGrid(DEMGlb[DEM].FullDEMGridLimits);
   AspectStats.CreateRose;
   AspectStats.Destroy;
end;


constructor tAspectStats.Create(theDEM : integer);
begin
   Npts := 0;
   AspX := 0;
   AspY := 0;
   DEM := theDEM;
   FillChar(AspectFreqValsTrue,SizeOf(AspectFreqValsTrue),0);
   FillChar(AspectFreqValsGrid,SizeOf(AspectFreqValsGrid),0);
end;

destructor tAspectStats.Destroy;
begin
   inherited;
end;


procedure tAspectStats.AddAspect(Aspect : float32);
var
   AspInt : integer;
begin
   AspInt := round(Aspect);
   if (AspInt = 360) then AspInt := 0;
   inc(AspectFreqValsGrid[AspInt]);
   inc(Npts);
end;


procedure tAspectStats.AddPoint(SlopeAspectRec: tSlopeAspectRec);
var
   AspInt : integer;
begin
   if (SlopeAspectRec.AspectDirTrue < 3600) then begin
      AspX := AspX + CosDeg(SlopeAspectRec.AspectDirTrue);
      AspY := AspY + SinDeg(SlopeAspectRec.AspectDirTrue);
      AspInt := round(SlopeAspectRec.AspectDirTrue);
      AspInt := CompassAngleInRangeInteger(AspInt);
      inc(AspectFreqValsTrue[AspInt]);
      inc(Npts);
   end
   else begin
       //MessageToContinue('SlopeAspectRec.AspectDir wrong, ' + RealToString(SlopeAspectRec.AspectDir,-12,-2));
   end;
end;


procedure tAspectStats.FillFromGrid(GridLimits : tGridLimits);
var
   x,y : integer;
   SlopeAspectRec: tSlopeAspectRec;
   z : float32;
begin
   for x := GridLimits.XGridLow to GridLimits.XGridHigh do begin
      for y := GridLimits.YGridLow to GridLimits.YGridHigh do begin
         if DEMGlb[DEM].ElevationDEM then begin
            if DEMGlb[DEM].GetSlopeAndAspect(MDDef.SlopeCompute,x,y,SlopeAspectRec) then
               AddPoint(SlopeAspectRec);
         end
         else if DEMGlb[DEM].GetElevMeters(x,y,z) then begin
            if (z >= 0) and (z <= 360) then begin
               SlopeAspectRec.AspectDirTrue := z;
               AddPoint(SlopeAspectRec);
            end;
         end;
      end;
   end;
end;


function tAspectStats.CreateRose(BaseLegend : shortstring = '') : tThisBaseGraph;
var
   ULtitle,Report : shortstring;
begin
   Result := TThisBaseGraph.Create(Application);
   Result.RoseColor := WinGraphColors[DEM mod 16];
   Report := '';
   VectorAverage;
   ULtitle := 'Vector Avg: ' + RealToString(AvgAspectMag,-8,2) + ' ' + RealToString(AvgAspectDir,-8,1) + '°';
   Result.DrawAspectRose(AspectFreqValsTrue,ULtitle,BaseLegend);
end;


function tAspectStats.QueensAspect: float64;
var
   Diagonal : integer;
begin
   Diagonal := round(arctan(DEMGlb[DEM].AverageYSpace/DEMGlb[DEM].AverageXSpace) / DegToRad);
   Result := 45 * (AspectFreqValsGrid[0] + AspectFreqValsGrid[Diagonal] + AspectFreqValsGrid[90] + AspectFreqValsGrid[180-Diagonal] + AspectFreqValsGrid[180] + AspectFreqValsGrid[180+Diagonal] +
                   AspectFreqValsGrid[270] + AspectFreqValsGrid[360-Diagonal] + AspectFreqValsGrid[360]) / NPts;
end;


procedure tAspectStats.VectorAverage;
var
   tx,ty : float64;
begin
   tx := aspx /  Npts;
   ty := aspy /  Npts;
   AvgAspectDir := HeadingOfLine(tx,ty);
   AvgAspectMag := sqrt(sqr(tx) + sqr(tY));
end;



