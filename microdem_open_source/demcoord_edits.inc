{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of ianMICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2023 Peter L. Guth   }
{------------------------------------}
{    include file for demcoord.pas   }
{____________________________________}


function tDEMDataSet.DetrendDEM(Normalize : boolean = true; FilterRadius : integer = 2) : integer;
var
   x,y : integer;
   Sum,znw,zw,zsw,zn,z,zs,zne,ze,zse,
   FactorNS,FactorEW,FactorDiag : float32;

begin
   Result := CloneAndOpenGridSetMissing(FloatingPointDEM,AreaName + '_detrend_residual',DEMHeader.ElevUnits);

   Normalize := AnswerIsYes('Normal distances to the y dimension pixel spacing');

   if Normalize then begin
      //factors to get the elevations at the Y spacing distance from the center pixel, assuming the same slope prevailed
      FactorNS := 1;
      FactorEW := AverageYSpace / AverageXSpace;  //equal to 1 for rectangular grids, probably > 1 for geographic grids
      FactorDiag := AverageYSpace / AverageDiaSpace;   //sqrt(2) for UTM grids, something close for geographic
   end;

   StartProgress('Detrend/residual');
   for x := 0 to pred(DEMheader.NumCol) do begin
      if (x mod 50 = 0) then UpDateProgressBar(x/pred(DEMheader.NumCol));
      for y := 0 to pred(DEMheader.NumRow) do begin
         if SurroundedPointElevs(x,y,znw,zw,zsw,zn,z,zs,zne,ze,zse) then begin
            if Normalize then begin
                znw :=  (zNW-z) * FactorDiag;
                zne :=  (zNE-z) * FactorDiag;
                zsw :=  (zSW-z) * FactorDiag;
                zse :=  (zSE-z) * FactorDiag;
                ze :=  (ze-z) * FactorEW;
                zw :=  (zw-z) * FactorEW;
                zn :=  (zn-z) * FactorNS;
                zs :=  (zs-z) * FactorNS;
                Sum := (zn+zne+ze+zse+zs+zsw+zw+znw)/8;
            end
            else Sum := z-(zn+zne+ze+zse+zs+zsw+zw+znw)/8;
            DEMGlb[Result].SetGridElevation(x,y,sum);
         end;
      end;
   end;
end;



procedure tDEMDataSet.SubtractaDEM(AddDEM : integer);
begin
   AddaDEM(AddDEM,-1);
end;

procedure tDEMDataSet.AddaDEM(AddDEM : integer; Mult : integer = 1);
var
   x,y  : integer;
   Lat,Long : float64;
   z,z2 : float32;
begin
    StartProgress('DEM math');
    for x := 0 to pred(DEMheader.NumCol) do begin
       if (x mod 50 = 0) then UpDateProgressBar(x/pred(DEMheader.NumCol));
       for y := 0 to pred(DEMheader.NumRow) do
          if GetElevMeters(x,y,z) then begin
             DEMGridToLatLongDegree(x,y,Lat,Long);
             if DEMGlb[AddDEM].GetElevFromLatLongDegree(Lat,Long,z2) then begin
                SetGridElevation(x,y,z + Mult * z2);
             end
             else begin
                SetGridMissing(x,y);
             end;
          end
          else SetGridMissing(x,y);
    end;
    EndProgress;
end;



function tDEMDataSet.ResampleByAveraging(OpenMap : boolean; SaveCountGrid : boolean = true; SaveName : PathStr = '') : integer;
var
   NewGrid,CountGrid,x,y,xi,yi : integer;
   Lat,Long : float64;
   z,z1 : float32;
   NewHeadRecs : tDEMheader;
   NewAreaName : shortstring;
begin
   {$If Defined(RecordCreateNewDEM) or Defined(RecordDEMIXResample)} writeLineToDebugFile('tDEMDataSet.ResampleByAveraging in, ' + AreaName + '  ' + KeyDEMParams); {$EndIf}
   NewGrid := SelectionMap.MakeTempGrid;
   {$If Defined(RecordCreateNewDEM) or Defined(RecordDEMIXResample)} WriteLineToDebugFile('ResampleByAveraging ' + DEMGlb[NewGrid].AreaName + '  ' + DEMGlb[NewGrid].KeyDEMParams); {$EndIf}
   NewHeadRecs := DEMGlb[NewGrid].DEMheader;
   CloseSingleDEM(NewGrid);
   if SaveName = '' then NewAreaName := AreaName
   else NewAreaName := ExtractFileNameNoExt(SaveName);

   NewHeadRecs.RasterPixelIsGeoKey1025 := MDDef.LasDEMPixelIs;
   NewHeadRecs.DEMPrecision := WordDEM;
   NewHeadRecs.ElevUnits := Undefined;
   OpenAndZeroNewDEM(true,NewHeadRecs,CountGrid,MDTempDir + NewAreaName + '_resample_density',InitDEMvalue,0);
   {$If Defined(RecordCreateNewDEM) or Defined(RecordDEMIXResample)} WriteLineToDebugFile('ResampleByAveraging, ' + DEMGlb[CountGrid].AreaName + '  ' + DEMGlb[CountGrid].KeyDEMParams); {$EndIf}

   NewHeadRecs.DEMPrecision := FloatingPointDEM;
   NewHeadRecs.ElevUnits := euMeters;
   OpenAndZeroNewDEM(true,NewHeadRecs,Result,MDTempDir + NewAreaName,InitDEMvalue,0);
   {$If Defined(RecordCreateNewDEM) or Defined(RecordDEMIXResample)} WriteLineToDebugFile('ResampleByAveraging, ' + DEMGlb[Result].AreaName + '  ' + DEMGlb[Result].KeyDEMParams); {$EndIf}

   StartProgress('Get z values ' + ExtractFileName(SaveName));
   for x := 0 to pred(DEMHeader.NumCol) do begin
       UpdateProgressBar(x/DEMHeader.NumCol);
       for y := 0 to pred(DEMHeader.NumRow) do begin
          if GetElevMeters(x,y,z) then begin
             DEMGridToLatLongDegree(x,y,Lat,Long);
             DEMGlb[CountGrid].LatLongDegreeToDEMGridInteger(Lat,Long,xi,yi);
             DEMGlb[CountGrid].IncrementGridValue(xi,yi);
             if DEMGlb[Result].GetElevMetersOnGrid(xi,yi,z1) then begin
                DEMGlb[Result].SetGridElevation(xi,yi,z1+Z);
             end
             else begin
                DEMGlb[Result].SetGridElevation(xi,yi,Z);
             end;
          end;
       end;
   end;

   StartProgress('Average values ' + ExtractFileName(SaveName));
   for x := 0 to pred(DEMGlb[Result].DEMHeader.NumCol) do begin
       UpdateProgressBar(x/DEMGlb[Result].DEMHeader.NumCol);
       for y := 0 to pred(DEMGlb[Result].DEMHeader.NumRow) do begin
          if DEMGlb[CountGrid].GetElevMetersOnGrid(x,y,z) and (z > 0) and DEMGlb[Result].GetElevMetersOnGrid(x,y,z1) then begin
             DEMGlb[Result].SetGridElevation(x,y,z1/z);
          end
          else begin
             DEMGlb[Result].SetGridMissing(x,y);
          end;
       end;
   end;

   if (SaveName <> '') then begin
      DEMGlb[Result].SaveAsGeotiff(SaveName);
      {$IfDef RecordSaveAverageResampleDEMformat} SaveName := ChangeFileExt(SaveName,'.dem'); DEMGlb[Result].WriteNewFormatDEM(SaveName); {$EndIf}
   end;
   if SaveCountGrid then begin
      DEMGlb[CountGrid].SetUpMap(CountGrid,true,mtElevSpectrum);
   end
   else begin
      CloseSingleDEM(CountGrid);
   end;
   if OpenMap then DEMGlb[Result].SetUpMap(Result,true,SelectionMap.MapDraw.MapType);
   EndProgress;
   {$IfDef RecordCreateNewDEM} writeLineToDebugFile('TMapForm.Averagetonewprojection1Click grids out'); {$EndIf}
end;


procedure tDEMDataSet.FilterStrip(NewDEM, FilterLap : integer; GridLimits : tGridLimits; FilterCategory : tFilterCat; Filter : FilterType);
label
   DataIsMissing;
const
   MaxFilterSize = 5000;
var
   ItsZ,znw,zw,zsw,zn,z,zs,zne,ze,zse,z1 : float32;
   Col,Row,x,y,Removed,NumSame,i,j,fv,Margin,Missing : integer;
   Sum, Min,Max,FilterValue,Mag     : float64;
   MomentVar : tMomentVar;
   Aspects : farray;
   xs    : array[0..MaxFilterSize] of float32;
begin
   for Row := GridLimits.YGridLow to GridLimits.YGridHigh do begin
      if (CountInStrips mod 50 = 0) and ShowSatProgress then begin
         UpdateProgressBar(CountInStrips/DEMheader.NumRow);
      end;
      if ThreadsWorking then TInterlocked.Increment(CountInStrips)
      else inc(CountInStrips);
      if MDdef.FilterGridsToEdge then Margin := 0
      else Margin := FilterLap;
      if (Row >= Margin) and (Row <= pred(DEMGlb[NewDEM].DEMheader.NumRow)-Margin) then begin
         for Col := GridLimits.XGridLow to GridLimits.XGridHigh do begin
            if (Col >= Margin) and (Col <= pred(DEMGlb[NewDEM].DEMheader.NumCol)-Margin) then begin
               if (FilterCategory = fcParamIsotrop) then begin
                  if SurroundedPointElevs(Col,Row,znw,zw,zsw,zn,z,zs,zne,ze,zse) then begin
                     DEMGlb[NewDEM].SetGridElevation(Col,Row,(zw + zn + ze + zs + 41 * z) / 45);
                  end;
               end
               else if (FilterCategory = fcNumNeigh) then begin
                  DEMGlb[NewDEM].SetGridElevation(Col,Row,ValidNeighborsInBox(Col,Row,FilterLap));
               end
               else if (FilterCategory = fcNeighbors) then begin
                  if (ValidNeighborsInBox(Col,Row,FilterLap) >= MDDef.ExpandNeighborsRequired) then begin
                     if GetElevMeters(Col,Row,z1) then DEMGlb[NewDEM].SetGridElevation(Col,Row,z1);
                  end;
               end
               else if (FilterCategory = fcFilFile) then begin
                  FilterValue := 0;
                  Sum := 0;
                  for i := -FilterLap to FilterLap do begin
                     for j := -FilterLap to FilterLap do begin
                        if MissingDataInGrid(Col+i,Row+j) then begin
                           if (not MDdef.FilterGridsToEdge) then goto DataIsMissing;
                        end
                        else if GetElevMeters(Col+i,Row+j,z1) then begin
                           fv := Filter[i+succ(FilterLap),j+Succ(FilterLap)];
                           Sum := Sum + fv;
                           FilterValue := FilterValue + fv * z1;
                        end;
                     end;
                  end;
                  if (Sum > 0) then DEMGlb[NewDEM].SetGridElevation(Col,Row,FilterValue/Sum);
                  DataIsMissing: ;
               end
               else begin
                  MomentVar.NPts := 0;
                  Sum := 0;
                  Min := 9999999;
                  Max := -9999999;
                  NumSame := 0;
                  Missing := 0;
                  if (FilterCategory = fcDissimilarNeighbors) then GetElevMeters(Col,Row,ItsZ);
                  for x := (Col-FilterLap) to (Col + FilterLap) do begin
                     for y := (Row-FilterLap) to (Row + FilterLap) do begin
                        if GetElevMeters(x,y,z1) then begin
                           if FilterCategory in [fcSum] then Sum := Sum + z1
                           else if (FilterCategory = fcDissimilarNeighbors) then begin
                              if abs(ItsZ-z1) < 0.01 then inc(NumSame);
                           end
                           else if FilterCategory in [fcMedian,fcMean,fcSTD] then xs[MomentVar.NPts] := z1
                           else if FilterCategory in [fcVectAvg] then begin
                              Aspects[MomentVar.NPts] := z1;
                           end
                           else Petmath.CompareValueToExtremes(z1,Min,Max);
                           inc(MomentVar.NPts);
                        end
                        else inc(Missing);
                     end;
                  end;

                  if (MomentVar.NPts > 0) then begin
                     if FilterCategory in [fcVectAvg] then begin
                        z1 := VectorAverage(MomentVar.NPts,Aspects,Mag);
                        if z1 > -90 then DEMGlb[NewDEM].SetGridElevation(Col,Row,z1);
                     end
                     else if (FilterCategory = fcDissimilarNeighbors) then begin
                        if NumSame >= MDDef.ExpandNeighborsRequired then DEMGlb[NewDEM].SetGridElevation(Col,Row,ItsZ)
                        else inc(Removed);
                     end
                     else if (FilterCategory = fcMedian) then begin
                        if MomentVar.NPts > Missing then DEMGlb[NewDEM].SetGridElevation(Col,Row,Petmath.Median(xs,MomentVar.NPts))
                        else DEMGlb[NewDEM].SetGridMissing(Col,Row);
                     end
                     else if (FilterCategory in [fcMean,fcSTD]) then begin
                        Moment(xs,MomentVar,msAfterStdDev);
                        if (FilterCategory in [fcMean]) then DEMGlb[NewDEM].SetGridElevation(Col,Row,MomentVar.mean)
                        else DEMGlb[NewDEM].SetGridElevation(Col,Row,MomentVar.sdev);
                     end
                     else if (FilterCategory = fcSum) then DEMGlb[NewDEM].SetGridElevation(Col,Row,Sum)
                     else if (FilterCategory = fcMin) then DEMGlb[NewDEM].SetGridElevation(Col,Row,Min)
                     else if (FilterCategory = fcMax) then DEMGlb[NewDEM].SetGridElevation(Col,Row,Max);
                  end;
               end;
            end;
         end;
      end;
   end {for};
end;


procedure tDEMDataSet.FilterThisDEM(FilterCategory : tFilterCat; var NewDEM : integer; BoxSize : integer = 0; FilterName : PathStr = '');
const
   MaxSize = 1000;
var
   Title         : Shortstring;
   NewHeadRecs   : tDEMheader;
   FilterLap,Sum : integer;
   Filter   : FilterType;
   {$IfDef NoParallelFor}
   {$Else}
       PartLimits : tGridLimitsArray;
       NewDEM1 : integer;
   {$EndIf}
begin
   {$IfDef RecordDEMEdits}  WriteLineToDebugFile('tDEMDataSet.FilterThisDEM in'); {$EndIf}
   SaveBackupDefaults;
   NewDEM := 0;
   NewHeadRecs := DEMheader;
   NewHeadRecs.DEMPrecision := FloatingPointDEM;
   if (FilterCategory = fcFilFile) then begin
      if not ReadFilter(Filter,BoxSize,FilterLap,Sum,FilterName,false) then begin
         MessageToContinue('Missing filter ' + FilterName);
         exit;
      end;
   end
   else begin
      if (BoxSize = 0) then begin
         FilterLap := 2;
         ReadDefault('Box size (postings each side of grid point)',FilterLap);
      end;
      while sqr(succ(2*FilterLap)) > MaxSize do dec(FilterLap);
      if (FilterCategory = fcNumNeigh) then begin
         NewHeadRecs.DEMPrecision := ByteDEM;
      end
      else if (FilterCategory = fcDissimilarNeighbors) then begin
         ReadDefault('Min neighbors same category required',MDDef.ExpandNeighborsRequired);
      end;
   end;

   case FilterCategory of
       fcVectAvg : Title := 'Vector Average';
       fcMedian : Title := 'Median' ;
       fcMean   : Title := 'Mean';
       fcMin    : Title := 'Min';
       fcMax    : Title := 'Max';
       fcSum    : Title := 'Sum';
       fcSTD    : Title := 'Std Dev';
       fcNumNeigh : Title := 'Num nearest neighbors, box =' + IntToStr(BoxSize);
       fcNeighbors : Title := 'Neighbors';
       fcDissimilarNeighbors : Title := 'Dissimilar neighbors';
       fcParamIsotrop : Title  := 'Par Isotrop';
       fcFilFile : Title := ExtractFileNameNoExt(FilterName);
   end;
   if (not (FilterCategory in [fcDissimilarNeighbors])) then begin
      Title := Title + ' (box size = ' + IntToStr(2*FilterLap+1) + ')';
   end;
   {$IfDef RecordDEMEdits} WriteLineToDebugFile('tDEMDataSet.FilterThisDEM setup done'); {$EndIf}
   OpenAndZeroNewDEM(true,NewHeadRecs,NewDEM,Title + '_filter_' + AreaName,InitDEMmissing);
   {$IfDef RecordDEMEdits} WriteLineToDebugFile('tDEMDataSet.FilterThisDEM OpenZero done'); {$EndIf}
   EndProgress;
   ShowHourglassCursor;

   MDdef.FilterGridsToEdge := true;
   StartProgress(DEMGlb[NewDEM].AreaName);
   CountInStrips := 0;

   {$IfDef NoParallelFor}
       ThreadsWorking := false;
       FilterStrip(NewDEM,FilterLap,FullDEMGridLimits,FilterCategory,Filter);
   {$Else}
      PartLimits := GetLimitsForParallelLoops(FullDEMGridLimits);
      NewDEM1 := NewDEM;
      TParallel.For(1,MDdef.MaxThreadsForPC,
         procedure (Value: Integer)
         begin
           FilterStrip(NewDEM1,FilterLap,PartLimits[Value],FilterCategory,Filter);
         end);
      ThreadsWorking := false;
   {$EndIf}
   DEMGlb[NewDEM].CheckMaxMinElev;
   RestoreBackupDefaults;
   {$IfDef RecordDEMEdits} WriteLineToDebugFile('tDEMDataSet.FilterThisDEM out'); {$EndIf}
end;


procedure tDEMDataSet.MultiplyGridByConstant(Aconst : float64);
var
   x,y  : integer;
   z : float32;
begin
    StartProgress('Multiply ' + AreaName);
    for x := 0 to pred(DEMheader.NumCol) do begin
       if (x mod 50 = 0) then UpDateProgressBar(x/pred(DEMheader.NumCol));
       for y := 0 to pred(DEMheader.NumRow) do begin
          if GetElevMeters(x,y,z) then begin
             SetGridElevation(x,y,aConst*z);
          end;
       end;
    end;
    EndProgress;
end;


procedure tDEMDataSet.SetEntireGridToConstant(z : float64);
var
   Col,Row,rc : integer;
begin
   StartProgress('Initialize grid to ' + RealToString(z,-12,-2) + ' ' + AreaName);
   rc := ProgressIncrement(DEMheader.NumCol);
   for Col := 0 to pred(DEMheader.NumCol) do begin
      if Col mod rc = 0 then UpdateProgressBar(Col/DEMheader.NumCol);
      for Row := 0 to pred(DEMheader.NumRow) do SetGridElevation(Col,Row,z);
   end;
   EndProgress;
end;


procedure tDEMDataSet.FillHolesSelectedBoxFromReferenceDEM(GridLimits : tGridLimits; RefDEM : integer; HoleFill : tHoleFill);
var
   Col,Row,x,y,Changed : integer;
   SameGrids : boolean;
   Lat,Long,xg,yg  : float64;
   z,z2,z3,z4 : float32;
begin
   {$IfDef RecordDEMEdits} WriteLineToDebugFile('tDEMDataSet.FillHolesSelectedBoxFromReferenceDEM in, DEM=' + AreaName + '  ' + KeyDEMParams(true)); {$EndIf}
   {$IfDef RecordDEMEdits} WriteLineToDebugFile(   ' fill from ' + DEMGlb[RefDEM].AreaName + '  ' + DEMGlb[RefDEM].KeyDEMParams(true)); {$EndIf}
   if (RefDEM = 0) then exit;
   if ShowSatProgress then StartProgress('Fill grid ' + AreaName);
   Changed := 0;
   SameGrids := SecondGridIdentical(RefDEM);

   if HoleFill = hfJustReferencePostings then begin
      for Col := 0 to pred(DEMGlb[RefDEM].DEMHeader.NumCol) do begin
         if ShowSatProgress and (Col mod 10 = 0) then UpdateProgressBar(Col/DEMGlb[RefDEM].DEMHeader.NumCol);
         for Row := 0 to pred(DEMGlb[RefDEM].DEMHeader.NumRow) do begin
             if DEMGLB[RefDEM].GetElevMeters(Col,Row,z2) then begin
                DEMGLB[RefDEM].DEMGridToLatLongDegree(Col,Row,Lat,Long);
                LatLongDegreeToDEMGrid(Lat,Long,xg,yg);
                SetGridElevation(Round(xg),round(yg),z2);
                inc(Changed);
             end;
         end;
      end;
   end
   else begin
      for Col := GridLimits.XGridLow to GridLimits.XGridHigh do begin
         if ShowSatProgress and (Col mod 10 = 0) then UpdateProgressBar((Col-GridLimits.XGridLow)/(GridLimits.XGridHigh-GridLimits.XGridLow));
         for Row := GridLimits.YGridLow to GridLimits.YGridHigh do begin
             if (not SameGrids) then DEMGridToLatLongDegree(Col,Row,Lat,Long);
             if (HoleFill = hfSeaLevel) then begin
                if SameGrids then begin
                   x := Col;
                   y := Row;
                end
                else begin
                   DEMGLB[RefDEM].LatLongDegreeToDEMGrid(Lat,Long,xg,yg);
                   x := round(xg);
                   y := round(yg);
                end;
                if DEMGLB[RefDEM].GetElevMeters(pred(x),pred(y),z) and  (abs(z) < 0.0001) and DEMGLB[RefDEM].GetElevMeters(pred(x),y,z2) and (abs(z2) < 0.0001) and
                   DEMGLB[RefDEM].GetElevMeters(x,pred(y),z3) and (abs(z3) < 0.0001) and DEMGLB[RefDEM].GetElevMeters(x,y,z4) and (abs(z4) < 0.0001) then begin
                      SetGridElevation(Col,Row,0);
                      inc(Changed);
                end;
             end;
             if (HoleFill = hfEverything) then begin
                if SameGrids then begin
                   if DEMGLB[RefDEM].GetElevMeters(Col,Row,z) then begin
                      SetGridElevation(Col,Row,z);
                      inc(Changed);
                   end;
                end
                else begin
                   if DEMGLB[RefDEM].GetElevFromLatLongDegree(Lat,Long,z) then begin
                      SetGridElevation(Col,Row,z);
                      inc(Changed);
                   end;
                end;
             end;
             if (HoleFill = hfOnlyHole) and (MissingDataInGrid(Col,Row)) or ((HoleFill = hfOnlyValid) and GetElevMeters(Col,Row,z2)) then begin
                if SameGrids then begin
                   if DEMGLB[RefDEM].GetElevMeters(Col,Row,z) then begin
                      SetGridElevation(Col,Row,z);
                      inc(Changed);
                   end;
                end
                else begin
                   if DEMGLB[RefDEM].GetElevFromLatLongDegree(Lat,Long,z) then begin
                      SetGridElevation(Col,Row,z);
                      inc(Changed);
                   end;
                end;
             end;
         end;
      end;
   end;
   CheckMaxMinElev;
   if ShowSatProgress then EndProgress;
   {$IfDef RecordDEMEdits} WriteLineToDebugFile('Points changed=' + IntToStr(Changed));  {$EndIf}
end;


const
   N=1;
   S=2;
   E=3;
   W=4;
   NE=5;
   SW=6;
   SE=7;
   NW=8;

procedure tDEMDataSet.SpecialInterpolateAcrossHoles;
var
   HolesFilled,TotalHoles : int64;
   //NewDEM,
   n,Col,Row,i,x,y,xc,yc : integer;
   ThisWeight,Sum : float64;
   zs,zn,ze,zw : float32;
   Found : array[1..8] of boolean;
begin
   StartProgressAbortOption('Fill holes ' + AreaName);
   HolesFilled := 0;
   TotalHoles := 0;
   for Col := 1 to (DEMheader.NumCol-2) do begin
      if (Col mod 25 = 0) then UpdateProgressBar(Col/DEMheader.NumCol);
      for Row := (DEMheader.NumRow-2) downto 1 do begin
         if MissingDataInGrid(Col,Row) then begin
            inc(TotalHoles);
            Sum := 0;
            n := 0;

            if GetElevMeters(Col,pred(Row),zs) then begin
               inc(n);
               Sum := Sum + zs;
            end;
            if GetElevMeters(pred(Col),Row,zw) then begin
               inc(n);
               Sum := Sum + zw;
            end;
            if GetElevMeters(succ(Col),Row,ze) then begin
               inc(n);
               Sum := Sum + ze;
            end;
            if GetElevMeters(Col,succ(Row),zn) then begin
               inc(n);
               Sum := Sum + zn;
            end;
            if n > 0 then SetGridElevation(Col,Row,Sum / n);
         end;
      end;
      if WantOut then break;
   end;
   CheckMaxMinElev;
   EndProgress;
   (*
   if ShowResults then begin
      MessageToContinue('Total holes: ' + IntToStr(TotalHoles) + MessLineBreak + 'Filled: ' + IntToStr(HolesFilled) + '  (' + RealToString(100.0*HolesFilled/TotalHoles,-8,2) + '%)',true);
   end;
   *)
end;




procedure tDEMDataSet.InterpolateAcrossHoles(ShowResults : boolean);
var
   HolesFilled,TotalHoles : int64;
   NewDEM,i,x,y,xc,yc : integer;
   ThisWeight,Sum,Weights : float64;
   z : float32;
   Found : array[1..8] of boolean;
begin
   StartProgressAbortOption('Fill holes ' + AreaName);
   SetNewDEM(NewDEM);
   HolesFilled := 0;
   TotalHoles := 0;
   for x := 1 to (DEMheader.NumCol-2) do begin
      if (x mod 25 = 0) then UpdateProgressBar(x/DEMheader.NumCol);
      for y := (DEMheader.NumRow-2) downto 1 do begin
         if MissingDataInGrid(x,y) then begin
            inc(TotalHoles);
            Sum := 0;
            Weights := 0;
            for i := 1 to 8 do Found[i] := false;

            xc := x;
            yc := succ(y);
            while (yc < pred(DEMheader.NumRow)) and (yc <= y + MDdef.FillHoleRadius) and MissingDataInGrid(xc,yc) do inc(yc);
            if GetElevMeters(xc,yc,z) then begin
               ThisWeight := 1 / sqr(yc - y);
               Weights := Weights + ThisWeight;
               Sum := Sum + ThisWeight * z;
               Found[N] := true;
            end;

            yc := pred(y);
            while (yc > 0) and (yc > y - MDdef.FillHoleRadius) and MissingDataInGrid(xc,yc) do dec(yc);
            if GetElevMeters(xc,yc,z) then begin
               ThisWeight := 1 / sqr(yc - y);
               Weights := Weights + ThisWeight;
               Sum := Sum + ThisWeight * z;
               Found[S] := true;
            end;

            yc := y;
            xc := succ(x);
            while (xc < pred(DEMheader.NumCol)) and (xc < x + MDdef.FillHoleRadius) and MissingDataInGrid(xc,yc) do inc(xc);
            if GetElevMeters(xc,yc,z) then begin
               ThisWeight := 1 / sqr(xc - x);
               Weights := Weights + ThisWeight;
               Sum := Sum + ThisWeight * z;
               Found[E] := true;
            end;

            xc := pred(x);
            while (xc > 0) and (xc > x - MDdef.FillHoleRadius) and MissingDataInGrid(xc,yc) do dec(xc);
            if GetElevMeters(xc,yc,z) then begin
               ThisWeight := 1 / sqr(xc - x);
               Weights := Weights + ThisWeight;
               Sum := Sum + ThisWeight * z;
               Found[W] := true;
            end;

            xc := succ(x);
            yc := succ(y);
            while (xc < pred(DEMheader.NumCol)) and (yc < pred(DEMheader.NumRow)) and (yc <= y + MDdef.FillHoleRadius) and MissingDataInGrid(xc,yc) do begin
               inc(yc);
               inc(xc);
            end;
            if GetElevMeters(xc,yc,z) then begin
               ThisWeight := 1 / sqr(yc - y) / sqrt_2;
               Weights := Weights + ThisWeight;
               Sum := Sum + ThisWeight * z;
               Found[NE] := true;
            end;
            xc := pred(x);
            yc := pred(y);
            while (yc >= 0) and (xc > 0) and (yc <= y - MDdef.FillHoleRadius) and MissingDataInGrid(xc,yc) do begin
               dec(yc);
               dec(xc);
            end;
            if GetElevMeters(xc,yc,z) then begin
               ThisWeight := 1 / sqr(yc - y) / sqrt_2;
               Weights := Weights + ThisWeight;
               Sum := Sum + ThisWeight * z;
               Found[SW] := true;
            end;

            xc := succ(x);
            yc := pred(y);
            while (yc >= 0) and (xc < pred(DEMheader.NumCol)) and (yc <= y - MDdef.FillHoleRadius) and MissingDataInGrid(xc,yc) do begin
               dec(yc);
               inc(xc);
            end;
            if GetElevMeters(xc,yc,z) then begin
               ThisWeight := 1 / sqr(yc - y) / sqrt_2;
               Weights := Weights + ThisWeight;
               Sum := Sum + ThisWeight * z;
               Found[SE] := true;
            end;

            xc := pred(x);
            yc := succ(y);
            while (xc >= 0) and (yc < pred(DEMheader.NumRow)) and (yc <= y + MDdef.FillHoleRadius) and MissingDataInGrid(xc,yc) do begin
               dec(xc);
               inc(yc);
            end;
            if GetElevMeters(xc,yc,z) then begin
               ThisWeight := 1 / sqr(yc - y) / sqrt_2;
               Weights := Weights + ThisWeight;
               Sum := Sum + ThisWeight * z;
               Found[NW] := true;
            end;

            if (MDDef.LooseFillHoles and ((Found[N] and Found[S]) or (Found[E] and Found[W])) or (Found[NE] and Found[SW]) or (Found[NW] and Found[SE]))
                  or ( (Found[N] or Found[NE]) and (Found[E] or Found[SE]) or (Found[S] or Found[SW]) and (Found[W] or Found[NW])) then begin
               DEMGlb[NewDEM].SetGridElevation(x,y,Sum / Weights);
               {$IfDef RecordProblems} inc(HolesFilled); {$EndIf}
            end;
         end;
      end;
      if WantOut then break;
   end;

   for x := 0 to DEMheader.NumCol-1 do begin
      if (x mod 25 = 0) then UpdateProgressBar(x/DEMheader.NumCol);
      for y := (DEMheader.NumRow-1) downto 0 do begin
         if MissingDataInGrid(x,y) and (GetElevMeters(x,y,z) ) then begin
            SetGridElevation(x,y,z);
         end;
      end;
   end;
   CloseSingleDEM(NewDEM);
   CheckMaxMinElev;
   EndProgress;
   if ShowResults then begin
      MessageToContinue('Total holes: ' + IntToStr(TotalHoles) + MessLineBreak + 'Filled: ' + IntToStr(HolesFilled) + '  (' + RealToString(100.0*HolesFilled/TotalHoles,-8,2) + '%)',true);
   end;
end;


procedure tDEMDataSet.ReclassifyRange(MinRange,MaxRange,NewZ : float64);
var
   x,y : integer;
   z : float32;
begin
   for x := 0 to pred(DEMheader.NumCol) do begin
      for y := 0 to pred(DEMheader.NumRow) do begin
         if GetElevMeters(x,y,z) and (z >= MinRange) and (z <= MaxRange) then SetGridElevation(x,y,NewZ);
      end;
   end;
end;

procedure tDEMDataSet.MissingDataToSeaLevelStrip(SeaLevel : float64; PartLimits :  tGridLimits);
var
   x,y : integer;
begin
   for x := PartLimits.XGridLow to PartLimits.XGridHigh do begin
      for y := PartLimits.YGridLow to PartLimits.YGridHigh do begin
         if MissingDataInGrid(x,y) then SetGridElevation(x,y,SeaLevel);
      end;
   end;
end;

procedure tDEMDataSet.MissingDataToConstantVelue(SeaLevel : float64 = 0);
var
   //PartLimits :  tGridLimitsArray;
   x,y : integer;
begin
   //PartLimits := GetLimitsForParallelLoops(FullDEMGridLimits);
   ShowHourGlassCursor;
   for x := 0 to pred(DEMheader.NumCol) do begin
      for y := 0 to pred(DEMheader.NumRow) do begin
         if MissingDataInGrid(x,y) then SetGridElevation(x,y,SeaLevel);
      end;
   end;

   (*
   TParallel.For(1,MaxThreadsAllowed,
        procedure (Value: Integer)
        begin
          MissingDataToSeaLevelStrip(SeaLevel,PartLimits[Value]);
        end);
   ThreadsWorking := false;
   *)
   ShowDefaultCursor;
end;


procedure tDEMDataSet.MarkInRangeMissing(LowVal,HighVal : float64; var NumPts : integer);
var
   x, y : integer;
   z : float32;
begin
   LowVal := LowVal - 0.0000001;
   HighVal := HighVal + 0.000001;
   NumPts := 0;
   if ShowSatProgress and (DEMheader.NumCol > 1500) then StartProgress('Mark missing ' + AreaName);
   for x := 0 to pred(DEMheader.NumCol) do begin
      if ShowSatProgress and (DEMheader.NumCol > 1500) and (x mod 100 = 0) then UpdateProgressBar(x/DEMheader.NumCol);
      for y := 0 to pred(DEMheader.NumRow) do begin
         if GetElevMeters(x,y,z) then begin
            if (z >= LowVal) and (z <= HighVal) then begin
               SetGridMissing(x,y);
               inc(NumPts);
            end;
         end;
      end;
   end;
   if ShowSatProgress and (DEMheader.NumCol > 1500) then EndProgress;
   if (NumPts > 0) then CheckMaxMinElev;
end;

procedure tDEMDataSet.MarkOutsideRangeMissing(LowVal,HighVal : float64; var NumPts : integer);
var
   x, y : integer;
   z : float32;
begin
   LowVal := LowVal - 0.0000001;
   HighVal := HighVal + 0.000001;
   NumPts := 0;
   if ShowSatProgress and (DEMheader.NumCol > 1500) then StartProgress('Mark missing ' + AreaName);
   for x := 0 to pred(DEMheader.NumCol) do begin
      if ShowSatProgress and (DEMheader.NumCol > 1500) and (x mod 100 = 0) then UpdateProgressBar(x/DEMheader.NumCol);
      for y := 0 to pred(DEMheader.NumRow) do begin
         if GetElevMeters(x,y,z) then begin
            if (z <= LowVal) or (z >= HighVal) then begin
               SetGridMissing(x,y);
               inc(NumPts);
            end;
         end;
      end;
   end;
   if ShowSatProgress and (DEMheader.NumCol > 1500) then EndProgress;
   if (NumPts > 0) then CheckMaxMinElev;
end;


procedure tDEMDataSet.MarkAboveMissing(LowVal : float64; var NumPts : integer);
var
   x, y : integer;
   z : float32;
begin
   NumPts := 0;
   if ShowSatProgress and (DEMheader.NumCol > 1500) then StartProgress('Mark missing ' + AreaName);
   for x := 0 to pred(DEMheader.NumCol) do begin
      if ShowSatProgress and (DEMheader.NumCol > 1500) and (x mod 100 = 0) then UpdateProgressBar(x/DEMheader.NumCol);
      for y := 0 to pred(DEMheader.NumRow) do begin
         if GetElevMeters(x,y,z) then begin
            if (z >= LowVal) then begin
               SetGridMissing(x,y);
               inc(NumPts);
            end;
         end;
      end;
   end;
   if ShowSatProgress and (DEMheader.NumCol > 1500) then EndProgress;
   if (NumPts > 0) then CheckMaxMinElev;
end;


procedure tDEMDataSet.MarkBelowMissing(HighVal : float64; var NumPts : integer);
var
   x, y : integer;
   z : float32;
begin
   NumPts := 0;
   if ShowSatProgress and (DEMheader.NumCol > 1500) then StartProgress('Mark missing ' + AreaName);
   for x := 0 to pred(DEMheader.NumCol) do begin
      if ShowSatProgress and (DEMheader.NumCol > 1500) and (x mod 100 = 0) then UpdateProgressBar(x/DEMheader.NumCol);
      for y := 0 to pred(DEMheader.NumRow) do begin
         if GetElevMeters(x,y,z) then begin
            if (z <= HighVal) then begin
               SetGridMissing(x,y);
               inc(NumPts);
            end;
         end;
      end;
   end;
   if ShowSatProgress and (DEMheader.NumCol > 1500) then EndProgress;
   if (NumPts > 0) then CheckMaxMinElev;
end;


function tDEMDataSet.ScaleZtoByte(z : float64) : byte;
begin
    Result := round(254 * (z - DEMHeader.MinElev) / (DEMHeader.MaxElev - DEMHeader.MinElev));
end;


procedure tDEMDataSet.RoundToByteRange;
var
   x, y : integer;
   z : float32;
begin
   for x := 0 to pred(DEMheader.NumCol) do begin
      for y := 0 to pred(DEMheader.NumRow) do begin
         if GetElevMeters(x,y,z) then begin
            z := round(254 * (z - DEMHeader.MinElev) / (DEMHeader.MaxElev - DEMHeader.MinElev));
            SetGridElevation(x,y,ScaleZtoByte(z));
         end;
      end;
   end;
end;


procedure tDEMDataSet.MarkElevationRangeAsConstant(var NumPts : integer);
var
   x,y       : integer;
   zHi,zLo,z,nz : float32;
   TStr      : ShortString;
begin
    TStr := 'Constant elevation';
    nz := 0;
    ReadDefault('New ' + TStr,nz);
    zHi := 0;
    ReadDefault('Hi z value to mark as ' + TStr,zHi);
    zLo := 0;
    ReadDefault('Lo z value to mark as ' + TStr,zLo);

    NumPts := 0;
    if ShowSatProgress and (DEMheader.NumCol > 1500) then StartProgress('Mask '+ AreaName);
    for x := 0 to pred(DEMheader.NumCol) do  begin
       if (x mod 100 = 0) and ShowSatProgress  and (DEMheader.NumCol > 1500) then UpdateProgressBar( x / DEMheader.NumCol);
       for y := 0 to pred(DEMheader.NumRow) do begin
          if GetElevMeters(x,y,z) and (z >= zLo-0.0001) and (z <= zHi+0.0001) then begin
             SetGridElevation(x,y,nz);
             inc(NumPts);
          end;
       end;
    end;
    EndProgress;
    CheckMaxMinElev;
end;


procedure tDEMDataSet.DeleteMissingDataPoints;
var
   Col,Row : integer;
   z : float32;
begin
    if (MDDef.AssumeNegativeValuesMissing and (DEMHeader.MinElev < 0)) or (MDDef.AssumeMinus99Missing and (DEMHeader.MinElev < -98)) or
       (MDDef.AssumeMinus999Missing and (DEMHeader.MinElev <-998)) or (MDDef.AssumeMinus9999Missing and (DEMHeader.MinElev <-99980)) then begin
       for Col := 0 to pred(DEMheader.NumCol) do begin
          for Row := 0 to pred(DEMheader.NumRow) do begin
             if GetElevMeters(Col,Row,z) and DEMDef_routines.MissingData(z) then SetGridMissing(Col,Row);
          end
       end;
       CheckMaxMinElev;
    end;
end;



