{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2023 Peter L. Guth  }
{-----------------------------------}
{include file for demcoord.pas      }
{___________________________________}

function OnGridNode(Xincr,YIncr : float32) : boolean; inline;
begin
   Result := ( (abs(XIncr) < 0.01) or (abs(XIncr) > 0.99) ) and ( (abs(YIncr) < 0.01) or (abs(YIncr) > 0.99))
end;



function tDEMDataSet.InterpolateBiCubicNumericalRecipes(xgrid,ygrid : float32; var z : float32) : boolean;
//uses code from Numerical Recipes
var
   Elev,xgrad,ygrad,crossgrad  : tElevFloatArray;


       function FindGradients(Index,xg,yg : integer) : boolean;
       var
          znw,zw,zsw,zn,z,zs,zne,ze,zse : float32;
       begin
          Result := SurroundedPointElevs(xg,yg,znw,zw,zsw,zn,z,zs,zne,ze,zse);
          if Result then begin
             xgrad[Index] :=  (zw-ze) / (2 * XSpaceByDEMrow[yg]);
             ygrad[Index] :=  (zn-zs) / (2 * AverageYSpace);
             crossgrad[Index] := (zne-zsw + znw-zse) / sqr(2*DiagSpaceByDEMrow[yg]);
          end
       end;


var
   xg,yg : integer;
   grad1,grad2 : float32;
begin
   //1, 4 go CCW from the SW corner
   xg := trunc(xgrid);
   yg := trunc(ygrid);
   Result := false;
   if (xg >= 1) and (yg >= 1) and (xg < DEMHeader.NumCol - 2) and (yg < DEMHeader.NumRow - 2) then begin
     if FindGradients(1,xg,yg) and FindGradients(2,xg+1,yg) and FindGradients(3,xg+1,yg+1) and FindGradients(4,xg,yg+1) then begin
        //function bicuint(y,y1,y2,y12: tElevFloatarray; x1l,x1u,x2l,x2u : integer; x1,x2: float32;  VAR ansy,ansy1,ansy2: float32) : boolean;

        Result := bicuint(Elev,xgrad,ygrad,crossgrad,  xg,xg+1,yg,yg+1,  Xgrid,Ygrid,  z,grad1,grad2);
     end;
   end;
end;




function tDEMDataSet.InterpolateBiCubicVisioTerra(xgrid,ygrid : float32; var z : float32) : boolean;
var
   Elev,xgrad,ygrad,crossgrad  : tElevFloatArray;

// this uses the notation in Gross and Cosseaux, 2022, in the figure page 25
//        dy-1  dy0  dy1   dy2
//  dx-1   znw2 zn2  zne2 zne3
//  dx0    znw  zn  zne   zne4
//  dx1    zw   z   ze     ze2
//  dx2    zsw  zs  zse    zse2


// this uses the notation in MICRODEM
//following the DTED convention, the origin on the grid describing the DEM has a normal  Cartesian origin in the SW corner
//        dx-1  dx0  dx1   dx2
//  dy+2   znw2 zn2  zne2 zne3
//  dy+1    znw  zn  zne   zne4
//  dy0     zw   z   ze     ze2
//  dy-1    zsw  zs  zse    zse2



var
   xg,yg,i : integer;
   znw2,zn2,zne2,zne3,znw,zn,zne,zne4,zw,ze,ze2zsw,zs,zse,zse2,zsw,ze2,
   xincr,yincr : float32;
   wx,wy,dx,dy : array[-1..2] of float32;
begin
   xg := trunc(xgrid);
   yg := trunc(ygrid);
   XIncr := frac(XGrid);
   YIncr := frac(YGrid);
   Result := false;

   if OnGridNode(Xincr,YIncr) then begin
      //point desired is on a grid node
      Result := GetElevMetersOnGrid(round(xgrid),round(ygrid),z);
   end
   else begin
      if SurroundedPointElevs(xg,yg,znw,zw,zsw,zn,z,zs,zne,ze,zse) then begin //get 3x3 group of points
         Result := true;
         if GetElevMetersOnGrid(xg-1,yg+2,znw2) and GetElevMetersOnGrid(xg,yg+2,zn2) and GetElevMetersOnGrid(xg+1,yg+2,zne2) and GetElevMetersOnGrid(xg+2,yg+2,zne3) and   //top row 4x4
            GetElevMetersOnGrid(xg+2,yg+1,zne4) and  //third row from bottom, only new point not in 3x3
            GetElevMetersOnGrid(xg+2,yg,ze2) and  //second row from bottom, only new point not in 3x3
            GetElevMetersOnGrid(xg+2,yg-1,zse2)  then begin //bottom row, only new point not in 3x3
            dx[-1] := 2 - xincr;
            dx[0] := 1- xincr;
            dx[1] := xincr;
            dx[2] := xincr + 1;

            dy[-1] := 2 - yincr;
            dy[0] := 1 - yincr;
            dy[1] := yincr;
            dy[2] := yincr + 1;

            wx[-1] := 1/2*(4 - 8*dx[-1] + 5*dx[-1]*dx[-1] - dx[-1]*dx[-1]*dx[-1]);
            wx[0] := 1-2.5*dx[0]*dx[0]+1.5*dx[0]*dx[0]*dx[0];
            wx[1] := 1-2.5*dx[1]*dx[1]+1.5*dx[1]*dx[1]*dx[1];
            wx[2] := 1/2*(4-8*dx[2]+5*dx[2]*dx[2]-dx[2]*dx[2]*dx[2]);

            {$IfDef RecordBicubic}
               WriteLineToDebugFile('x incr:' + RealToString(xincr,8,2));
               WriteLineToDebugFile('dx: ' + RealToString(dx[-1],6,2 ) + RealToString(dx[0],6,2 ) + RealToString(dx[1],6,2 ) + RealToString(dx[2],6,2 ) );
               WriteLineToDebugFile('wx: ' + RealToString(wx[-1],6,2 ) + RealToString(wx[0],6,2 ) + RealToString(wx[1],6,2 ) + RealToString(wx[2],6,2 ) );
            {$EndIf}

            wy[-1] := 1/2*(4-8*dy[-1]+5*dy[-1]*dy[-1]-dy[-1]*dy[-1]*dy[-1]);
            wy[0] := 1-2.5*dy[0]*dy[0]+1.5*dy[0]*dy[0]*dy[0];
            wy[1] := 1-2.5*dy[1]*dy[1]+1.5*dy[1]*dy[1]*dy[1];
            wy[2] := 1/2*(4-8*dy[2]+5*dy[2]*dy[2]-dy[2]*dy[2]*dy[2]);

            {$IfDef RecordBicubic}
               WriteLineToDebugFile('y incr:' + RealToString(yincr,8,2));
               WriteLineToDebugFile('dy: ' + RealToString(dy[-1],6,2 ) + RealToString(dy[0],6,2 ) + RealToString(dy[1],6,2 ) + RealToString(dy[2],6,2 ) );
               WriteLineToDebugFile('wy: ' + RealToString(wy[-1],6,2 ) + RealToString(wy[0],6,2 ) + RealToString(wy[1],6,2 ) + RealToString(wy[2],6,2 ) );
            {$EndIf}


            //this is w2 of Gross and Cosseaux, 2022; to adjust the slopes for w1, multiply all weights by 2; for w3, divide by 2
            //slopes for w1= -1,  w2 = -0.5,  w3=-0.25
            for I := -1 to 2 do begin
               wx[i] := wx[i] * MDDef.BicubicSlope / 0.5;
               wy[i] := wy[i] * MDDef.BicubicSlope / 0.5;
            end;


           // check this!!


            z :=  wx[-1] * wy[2] * znw2 + wx[0] * wy[2] * zn2 + wx[1] * wy[2] * zne2   + wx[2] * wy[2] * zne3 +
                  wx[-1] * wy[1] * znw  + wx[0] * wy[1] * zn  + wx[1] * wy[1] *  zne   + wx[2] * wy[1] * zne4 +
                  wx[-1] * wy[0] *  zw  + wx[0] * wy[0] *  z  + wx[1] * wy[0] *   ze   + wx[2] * wy[0] *  ze2 +
                  wx[-1] * wy[-1] * zsw + wx[0] * wy[-1] * zs + wx[-1] * wy[-1] * zse  + wx[-1] * wy[-1] * zse2;
         end
         else begin  //revert to bilinear
             z :=  (1-XIncr) * (1-YIncr)  * zsw + (1-XIncr) * YIncr  * znw + XIncr * (1-YIncr) * zse + XIncr * YIncr * zne;
         end;
      end;
   end;
end;





function tDEMDataSet.ZinMeters(z : float64) : float64;
begin
   if Not MissingData(z) then Result := z * ElevationMultiple;
end;


function tDEMDataSet.GetElevMetersOnGrid(x,y : integer; var z  : float32) : boolean;
begin
   z := ThisDEMMissingValue;
   Result := false;
   if GridInDataSet(x,y) then begin
      if (DEMheader.DEMPrecision = FloatingPointDEM) then z := ShortFloatElevations^[x]^[y]
      else if (DEMheader.DEMPrecision = SmallIntDEM) then z := SmallIntElevations[x]^[y]
      else if (DEMheader.DEMPrecision = WordDEM) then z := WordElevations^[x]^[y]
      else if (DEMheader.DEMPrecision = LongWordDEM) then z := LongWordElevations^[x]^[y]
      else if (DEMheader.DEMPrecision = ByteDEM) then z := ByteElevations^[x]^[y];
      if MissingElevation(z) then begin
         //z := ThisDEMMissingValue;
         //Result := false;
      end
      else begin
         z := zinMeters(z);
         Result := true;
      end;
   end;
end;



function tDEMDataSet.GetElevMeters(XGrid,YGrid : float64; var z  : float32) : boolean;
{interpolate to get point elevation}
var
   dSE,dSW,dNE,dNW,sum,XIncr,YIncr : float64;
   i,Num  : integer;
   Elev   : tElevFloatArray;
begin
   Result := false;
   z := ThisDEMMissingValue;
   if (XGrid < -0.01) or (YGrid < -0.01) or (XGrid > pred(DEMheader.NumCol) + 0.01) or (YGrid > pred(DEMheader.NumRow) + 0.01) then exit;
   if (MDDef.wf.ElevInterpolation = piNearestGrid) or MDDef.NoDEMInterpolations or LandCoverGrid or (DEMheader.ElevUnits in [euIntCode]) then begin
      Result := GetElevMetersOnGrid(round(xgrid),round(ygrid),z);
      exit;
   end;

   XIncr := frac(XGrid);
   YIncr := frac(YGrid);
   if OnGridNode(Xincr,YIncr) then begin //point desired is on a grid node
      Result := GetElevMetersOnGrid(round(xgrid),round(ygrid),z);
   end
   else begin
     if (MDDef.wf.ElevInterpolation = piBicubicVT) and InterpolateBiCubicVisioTerra(round(xgrid),round(ygrid),z) then begin
     //if InterpolateBiCubicNumericalRecipes(round(xgrid),round(ygrid),z) then begin

     end
     else if (MDDef.wf.ElevInterpolation = piBicubicNR) and InterpolateBiCubicNumericalRecipes(round(xgrid),round(ygrid),z) then begin

     end
     else if GetElevSquareMeters(XGrid,YGrid,Elev) then begin {elevations surrounding a point: 1=SW, 2=SE, 3=NE, 4=NW, 5=1=SW}
          if MDDef.wf.ElevInterpolation in [piBilinear] then begin
             z := (  (1-XIncr) * (1-YIncr)  * Elev[1] + (1-XIncr) * YIncr * Elev[4] + XIncr * (1-YIncr) * Elev[2] + XIncr * YIncr * Elev[3]);
          end
          else if  MDDef.wf.ElevInterpolation = piTriangle then begin
             if (Yincr < 1 - XIncr) then z := YIncr * Elev[4] + (1 - YIncr - XIncr) * Elev[1] + XIncr * Elev[2]
             else z := (1-YIncr) * Elev[2] + (YIncr + XIncr -1) * Elev[3] + (1-XIncr) * Elev[4];
          end
          else if MDDef.wf.ElevInterpolation in [piWeightedR,piWeightedR2] then begin
             if MDDef.wf.ElevInterpolation in [piWeightedR] then begin
                dSW := 1 / sqrt( sqr(Xincr) + sqr(yIncr));
                dSE := 1 / sqrt( sqr(1-Xincr) + sqr(yIncr));
                dNW := 1 / sqrt( sqr(Xincr) + sqr(1-yIncr));
                dNE := 1 / sqrt( sqr(1-Xincr) + sqr(1-yIncr));
             end
             else begin
                dSW := 1 / ( sqr(Xincr) + sqr(yIncr));
                dSE := 1 / ( sqr(1-Xincr) + sqr(yIncr));
                dNW := 1 / ( sqr(Xincr) + sqr(1-yIncr));
                dNE := 1 / ( sqr(1-Xincr) + sqr(1-yIncr));
             end;
             sum := dSW + dNW + dSE + dNE;
             z := (dSw / Sum) * Elev[1] + (dNW / Sum) * Elev[4] + (dSE / Sum) * Elev[2] + (dNE / Sum) * Elev[3];
          end
          else if ( MDDef.wf.ElevInterpolation = piSWGrid) then begin
             Result := GetElevMeters(trunc(xgrid),trunc(ygrid),z);
          end;
      end
      else begin
         if MissingElevation(Elev[1]) and MissingElevation(Elev[2]) and MissingElevation(Elev[3]) and MissingElevation(Elev[4]) then begin
            z := ThisDEMMissingValue;
         end
         else begin //average to valid surrounding elevations
            Sum := 0;
            Num := 0;
            for i := 1 to 4 do begin
              if not MissingElevation(Elev[i]) then begin
                 Sum := Sum + Elev[i];
                 inc(Num);
              end;
            end;
            z := Sum / Num;
         end;
      end;
   end;
   Result := not MissingElevation(z);
end {proc GetElev};


function tDEMDataSet.MissingElevation(z : float32) : boolean;
begin
   Result := abs(z - ThisDEMMissingValue) < 0.001;
end;


function tDEMDataSet.MissingDataInGrid(XGrid,YGrid : integer) :  boolean;
begin
   if GridInDataSet(xgrid,ygrid) then begin
      if (DEMheader.DEMPrecision = FloatingPointDEM) then Result := abs(ShortFloatElevations^[xgrid]^[ygrid] - ThisDEMMissingValue) < 0.001
      else if (DEMheader.DEMPrecision = SmallIntDEM) then Result := SmallIntElevations[xgrid]^[ygrid] = MaxSmallInt
      else if (DEMheader.DEMPrecision = WordDEM) then Result := WordElevations^[xgrid]^[ygrid] = MaxWord
      else if (DEMheader.DEMPrecision = LongWordDEM) then Result := LongWordElevations^[xgrid]^[ygrid] = MaxLongWord
      else if (DEMheader.DEMPrecision = ByteDEM) then begin
         if (DEMheader.ElevUnits = euImagery) and MDDef.IgnoreHistogramZero then Result := (ByteElevations^[xgrid]^[ygrid] = 0)
         else Result := (ByteElevations^[xgrid]^[ygrid] = 255);
      end;
   end
   else Result := false;
end;


function tDEMDataSet.GetElevSquareMeters(XGrid,YGrid : float64; var Elev  : tElevFloatArray) : boolean;
   {get four elevations from data set surrounding given point; elevations surrounding a point: 1=SW, 2=SE, 3=NE, 4=NW, 5=1=SW}
var
   Col,Row,i : integer;
begin
   for i := 1 to 5 do Elev[i] := ThisDEMMissingValue;
   Col := Trunc(XGrid);
   Row := Trunc(YGrid);
   Result := true;

   if GetElevMetersOnGrid(Col,Row,Elev[1]) then begin
      if (Row < pred(DEMheader.NumRow)) and (Col < pred(DEMheader.NumCol)) then begin
         Result := GetElevMetersOnGrid(succ(Col),Row,Elev[2]) and GetElevMetersOnGrid(succ(Col),succ(Row),Elev[3]) and GetElevMetersOnGrid(Col,succ(Row),Elev[4]);
      end
      else if (Row = pred(DEMheader.NumRow)) and (Col = pred(DEMheader.NumCol)) or (Row = pred(DEMheader.NumRow)) then begin
         for i := 2 to 4 do Elev[i] := Elev[1];
      end
      else if Col = pred(DEMheader.NumCol) then begin
         Result := GetElevMetersOnGrid(Col,Row,Elev[2]) and GetElevMetersOnGrid(Col,succ(Row),Elev[3]) and GetElevMetersOnGrid(Col,succ(Row),Elev[4]);
      end;
      Elev[5] := Elev[1];
   end;
end;


procedure tDEMDataSet.GetElevationsInLongArray(GridLimits: tGridLimits; var NPts : int64; var Values : Petmath.bfarray32; IncludeSeaLevel : boolean = true);
var
   Col,Row,Incr : integer;
   z : float32;
begin
   {$IfDef RecordMoments} WriteLineToDebugFile('tDEMDataSet.GetElevationsInLongArray in'); {$EndIf}
   Incr := GetSamplingSize(GridLimits);
   Incr := Incr * MDDef.StatSampleIncr;
   {$IfDef RecordMoments} WriteLineToDebugFile('tDEMDataSet.GetElevationsInLongArray, incr=' + IntToStr(incr)); {$EndIf}
   NPts := 0;
   ShowHourglassCursor;
   Col := GridLimits.XGridLow;
   while (Col <= GridLimits.XGridHigh) do begin
      Row := GridLimits.YGridLow;
      while (Row <= GridLimits.YGridHigh) do begin
         if GetElevMeters(Col,Row,z) then begin
            if (IncludeSeaLevel) or (abs(z) > 0.01) then begin
               Values[NPts] := z;
               inc(NPts);
            end;
         end;
         inc(Row,Incr);
      end;
      inc(Col,Incr);
   end;
   {$IfDef RecordMoments} WriteLineToDebugFile('tDEMDataSet.GetElevationsInLongArray out'); {$EndIf}
end;


function tDEMDataSet.SetGridElevation(Col,Row : int32; z : float64) : boolean;
var
   zi : int64;
begin
   Result := GridInDataSet(Col,Row);
   if Result then begin
      if (DEMheader.DEMPrecision = FloatingPointDEM) then ShortFloatElevations^[Col]^[Row] := z
      else if (DEMheader.DEMPrecision = ByteDEM) then begin
         zi := ValidByteRange(round(z));
         ByteElevations^[Col]^[Row] := zi;
      end
      else if (DEMheader.DEMPrecision = WordDEM) then begin
         if (z > MaxWord) then z := MaxWord;
         if (z < 0) then z := 0;
         WordElevations^[Col]^[Row] := round(z);
      end
      else if (DEMheader.DEMPrecision = LongWordDEM) then begin
         if (z > MaxLongWord) then z := MaxLongWord;
         if (z < 0) then z := 0;
         zi := round(z);
         LongWordElevations^[Col]^[Row] := zi;
      end
      else begin
         if (z > MaxSmallInt) then z := MaxSmallInt;
         if (z < -MaxSmallInt) then z := MaxSmallInt;
         zi := round(z);
         SmallIntElevations[Col]^[Row] := zi;
      end;
   end;
end;

function tDEMDataSet.SetGridElevationLatLongDegree(Lat,Long : float64; z : float64) : boolean;
var
   xg,yg : int32;
begin
   LatLongDegreeToDEMGridInteger(Lat,Long,xg,yg);
   Result := SetGridElevation(xg,yg,z);
end;


procedure tDEMDataSet.SetGridMissing(Col,Row : int32);
begin
   if GridInDataSet(Col,Row) then begin
      if (DEMheader.DEMPrecision = FloatingPointDEM) then ShortFloatElevations^[Col]^[Row] := ThisDEMMissingValue
      else if (DEMheader.DEMPrecision = ByteDEM) then ByteElevations^[Col]^[Row] := ThisDEMMissingValue
      else if (DEMheader.DEMPrecision = WordDEM) then WordElevations^[Col]^[Row] := ThisDEMMissingValue
      else if (DEMheader.DEMPrecision = LongWordDEM) then LongWordElevations^[Col]^[Row] := ThisDEMMissingValue
      else SmallIntElevations[Col]^[Row] := ThisDEMMissingValue;
   end;
end;


procedure tDEMDataSet.SetGridMissing(x,y : float64);
begin
   SetGridElevation(round(x),round(y),ThisDEMMissingValue);
end;

//  ****************  elevation percentiles

procedure tDEMDataSet.GetElevPercentiles(GridLimits: tGridLimits);
var
   zvs : ^bfarray32;
   NPts,i : int64;
begin
   if (ZPercens = Nil) then begin
      {$IfDef RecordElevPercentiles} WriteLineToDebugFile('tDEMDataSet.GetElevPercentiles for ' + AreaName); {$EndIf}
      new(ZPercens);
      New(zvs);
      GetElevationsInLongArray(GridLimits,NPts,zvs^,false);
      Petmath.HeapSort(NPts,zvs^);
      for I := 1 to 999 do begin
         ZPercens^[i] := zvs^[round(0.001 * Npts * i)];
      end;
      Dispose(zvs);
   end;
end;


function tDEMDataSet.FindPercentileElevation(Percentile : float64) : float64;
var
   n : integer;
begin
   if (ZPercens = Nil) then GetElevPercentiles(FullDEMGridLimits);
   n := round(10*Percentile);
   Petmath.ValueInRange(n,1,999);
   Result := Zpercens^[n];
end;


function tDEMDataSet.PercentileOfElevation(z : float64) : float64;
var
   n : integer;
begin
   if (ZPercens = Nil) then GetElevPercentiles(FullDEMGridLimits);
   Result := 99.9;
   if z >= Zpercens^[999] then exit;
   for n := 1 to 999 do  begin
      if Zpercens^[n] >= z then begin
         Result := 0.1 * n;
         exit;
      end;
   end;
end;


function tDEMDataSet.PercentileOfPoint(xloc,yloc : integer; GridLimits : tGridLimits) : float64;
var
   z,zp   : float32;
   x,y,NPts,NLower : integer;
begin
   if GetElevMeters(xloc,yloc,zp) then begin
      NPts := 0;
      NLower := 0;
      for x := GridLimits.XGridLow to GridLimits.XGridHigh do begin
         for y := GridLimits.YGridLow to GridLimits.YGridHigh do begin
            if GetElevMeters(x,y,z) then begin
               inc(NPts);
               if (z < zp) then inc(NLower);
            end;
         end;
      end;
      Result := 100 * NLower / NPts;
   end
   else Result := -99;
end;


procedure tDEMDataSet.CloseElevPercentiles;
begin
   if (Zpercens <> Nil) then begin
      Dispose(ZPercens);
      ZPercens := Nil;
   end;
end;

