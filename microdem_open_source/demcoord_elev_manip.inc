{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of ianMICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2015 Peter L. Guth   }
{____________________________________}

{include file for demcoord.pas}


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
      else if (DEMheader.DEMPrecision = SmallIntDEM) then z := DTEDCols[x]^[y]
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


function tDEMDataSet.GetElevMetersOnGridInteger(x,y : integer; var z  : int64) : boolean;
begin
   z := ThisDEMMissingValue;
   Result := false;
   if GridInDataSet(x,y) then begin
      if (DEMheader.DEMPrecision = SmallIntDEM) then z := DTEDCols[x]^[y]
      else if (DEMheader.DEMPrecision = WordDEM) then z := WordElevations^[x]^[y]
      else if (DEMheader.DEMPrecision = LongWordDEM) then z := LongWordElevations^[x]^[y]
      else if (DEMheader.DEMPrecision = ByteDEM) then z := ByteElevations^[x]^[y];
      if MissingElevation(z) then begin
         z := ThisDEMMissingValue;
         Result := false;
      end
      else begin
         Result := true;
      end;
   end;
end;


(*
function tDEMDataSet.GetRawGridElevation(Col,Row : integer; var z : float32) : boolean;
begin
   if GridInDataSet(Col,Row) and (not(MissingDataInGrid(Col,Row))) then begin
      if (DEMheader.DEMPrecision = FloatingPointDEM) then z := ShortFloatElevations^[Col]^[Row]
      else if (DEMheader.DEMPrecision = WordDEM) then z := WordElevations^[Col]^[Row]
      else if (DEMheader.DEMPrecision = LongWordDEM) then z := LongWordElevations^[Col]^[Row]
      else if (DEMheader.DEMPrecision = ByteDEM) then z := ByteElevations^[Col]^[Row]
      else z := DTEDCols[Col]^[Row];
      z := zInMeters(z);
      Result := not MissingElevation(z);
   end
   else begin
      Result := false;
      z := ThisDEMMissingValue;
   end;
end;

function tDEMDataSet.GridElevSureInData(x,y : integer) : float32;
begin
   if MissingDataInGrid(x,y) then begin
      Result := ThisDEMMissingValue;
   end
   else begin
      if (DEMheader.DEMPrecision = FloatingPointDEM) then Result := ShortFloatElevations^[x]^[y]
      else if (DEMheader.DEMPrecision = SmallIntDEM) then Result := DTEDCols[x]^[y]
      else if (DEMheader.DEMPrecision = WordDEM) then Result := WordElevations^[x]^[y]
      else if (DEMheader.DEMPrecision = LongWordDEM) then Result := LongWordElevations^[x]^[y]
      else if (DEMheader.DEMPrecision = ByteDEM) then Result := ByteElevations^[x]^[y];
      if MissingElevation(Result) then Result := ThisDEMMissingValue
      else Result := zinMeters(Result);
   end;
end;

*)

(*
function tDEMDataSet.GridElevMeters(x,y : integer) : float32;
begin
   if GridInDataSet(x,y) then begin
      Result := GridElevSureInData(x,y);
   end
   else begin
      Result := ThisDEMMissingValue;
   end;
end;


function tDEMDataSet.GetRawGridElevation(Col,Row : integer; var z : Integer) : boolean;
var
   zf : float32;
begin
   Result := GetRawGridElevation(Col,Row,zf);
   if Result then z := round(zf);
end;
*)





(*
procedure tDEMDataSet.InterpolateBiCubic(xgrid,ygrid : float64; var z : float32);
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
   if (xg >= 1) and (yg >= 1) and (xg < DEMHeader.NumCol - 2) and (yg < DEMHeader.NumRow - 2) then begin
     if FindGradients(1,xg,yg) and FindGradients(2,xg+1,yg) and FindGradients(3,xg+1,yg+1) and FindGradients(4,xg,yg+1) then begin
        bcuint(Elev,xgrad,ygrad,crossgrad,xg,xg+1,yg,yg+1,Xgrid,Ygrid,z,grad1,grad2);
     end;
   end;
end;
*)

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

   YIncr := frac(YGrid);
   XIncr := frac(XGrid);
   //if ((abs(XIncr) < 0.01) and (abs(YIncr) < 0.01)) or ((abs(XIncr) > 0.99) and (abs(YIncr) > 0.99)) then begin
   if ( (abs(XIncr) < 0.01) or (abs(XIncr) > 0.99) ) and ( (abs(YIncr) < 0.01) or (abs(YIncr) > 0.99)) then begin
      //point desired is on a grid node
      Result := GetElevMetersOnGrid(round(xgrid),round(ygrid),z);
   end
   else begin
     if GetElevSquareMeters(XGrid,YGrid,Elev) then begin {elevations surrounding a point: 1=SW, 2=SE, 3=NE, 4=NW, 5=1=SW}
          if MDDef.wf.ElevInterpolation in [piBilinear] then begin
             z := (  (1-XIncr) * (1-YIncr)  * Elev[1] + (1-XIncr) * YIncr  * Elev[4] + XIncr * (1-YIncr) * Elev[2] + XIncr * YIncr * Elev[3]);
          end
          else if (MDDef.wf.ElevInterpolation = piBicubic) then begin
             //InterpolateBiCubic(xgrid,ygrid,z);
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
         else begin
            //average to valid surrounding elevations
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
      //Result := MissingElevation(GridElevSureInData(xgrid,ygrid));
      if (DEMheader.DEMPrecision = FloatingPointDEM) then Result := abs(ShortFloatElevations^[xgrid]^[ygrid] - ThisDEMMissingValue) < 0.001
      else if (DEMheader.DEMPrecision = SmallIntDEM) then Result := DTEDCols[xgrid]^[ygrid] = MaxSmallInt
      else if (DEMheader.DEMPrecision = WordDEM) then Result := WordElevations^[xgrid]^[ygrid] = MaxWord
      else if (DEMheader.DEMPrecision = LongWordDEM) then Result := LongWordElevations^[xgrid]^[ygrid] = MaxLongWord
      else if (DEMheader.DEMPrecision = ByteDEM) then begin
         //Result := ByteElevations^[xgrid]^[ygrid] = 255;
         if (DEMheader.ElevUnits = euImagery) and MDDef.IgnoreHistogramZero then Result := (ByteElevations^[xgrid]^[ygrid] = 0)
         else Result := (ByteElevations^[xgrid]^[ygrid] = 255);
      end;
   end
   else Result := false;
end;


function tDEMDataSet.GetElevSquareMeters(XGrid,YGrid : float64; var Elev  : tElevFloatArray) : boolean;
   {get four elevations from data set surrounding given point;  elevations surrounding a point: 1=SW, 2=SE, 3=NE, 4=NW, 5=1=SW}
var
   Col,Row,i : integer;
begin
   for i := 1 to 5 do Elev[i] := ThisDEMMissingValue;
   //if GridInDataSet(XGrid,YGrid) then begin
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
   {$IfDef RecordMoments} WriteLineToDebugFile('tDEMDataSet.GetElevationsInLongArray, incr=' + IntToStr(incr));  {$EndIf}
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
   {$IfDef RecordMoments} WriteLineToDebugFile('tDEMDataSet.GetElevationsInLongArray out');  {$EndIf}
end;


function tDEMDataSet.SetGridElevation(Col,Row : integer; z : float64) : boolean;
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
         DTEDCols[Col]^[Row] := zi;
      end;
   end;
end;

function tDEMDataSet.SetGridElevationLatLongDegree(Lat,Long : float64; z : float64) : boolean;
var
   xg,yg : integer;
begin
   LatLongDegreeToDEMGridInteger(Lat,Long,xg,yg);
   Result := SetGridElevation(xg,yg,z);
end;


procedure tDEMDataSet.SetGridMissing(Col,Row : integer);
begin
   if GridInDataSet(Col,Row) then begin
      if (DEMheader.DEMPrecision = FloatingPointDEM) then ShortFloatElevations^[Col]^[Row] := ThisDEMMissingValue
      else if (DEMheader.DEMPrecision = ByteDEM) then ByteElevations^[Col]^[Row] := ThisDEMMissingValue
      else if (DEMheader.DEMPrecision = WordDEM) then WordElevations^[Col]^[Row] := ThisDEMMissingValue
      else if (DEMheader.DEMPrecision = LongWordDEM) then LongWordElevations^[Col]^[Row] := ThisDEMMissingValue
      else DTEDCols[Col]^[Row] := ThisDEMMissingValue;
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

