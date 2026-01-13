{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2026 Peter L. Guth  }
{----------------------------------------}
{   include file for demstat             }
{________________________________________}


procedure SSIMcheck(DoThinning : boolean);
const
   Windows : array[1..5] of integer = (7,9,11,13,15);
var
   DEM1,DEM2,thin,i,db : integer;
   Grids : array[1..5] of integer;
   bb : sfBoundBox;
   Mean,Std : float32;
   NPts : int64;
   aLine,AreaName : shortstring;
   Results : tStringList;
   gl1,gl2 : tGridLimits;
   SSIM,Luminance,Contrast,Structure : float64;
begin
   GetTwoCompatibleGrids('SSIM',false, DEM1,DEM2,false);
   Thin := 1;
   Results := tStringList.Create;

   if DoThinning then begin
      for i := 1 to 5 do begin
         Grids[i] := MakeSSIMMap(true,false,DEM1,DEM2,1,11,Thin);
         Thin := Thin * 2;
      end;
      Results.Add('DEMIX_TILE,LAT,LONG,GRID_FULL,TILE_AVG,POINTS_1,POINTS_2,POINTS_4,POINTS_8,POINTS_16');
   end
   else begin
      for i := 1 to 5 do begin
         AreaName := 'SSIM_window_' + IntToStr(Windows[i]) + 'x' + IntToStr(Windows[i]);
         Grids[i] := MakeSSIMMap(true,false,DEM1,DEM2,1,Windows[i],Thin,AreaName);
      end;
      Results.Add('DEMIX_TILE,LAT,LONG,GRID_FULL,W_7x7,W_9x9,W_11x11,W_13x13,W_15x15');
   end;
   db := DEMIXtileFill(DEM1,DEMGlb[DEM1].DEMBoundBoxGeo);
   GISdb[db].ApplyGISFilter('GRID_FULL>' + IntToStr(MDDef.DEMIX_Tile_Full));

    while not GISdb[db].MyData.eof do begin
       bb := GISdb[db].MyData.GetRecordBoundingBox;

       gl1 := DEMglb[DEM1].sfBoundBox2tGridLimits(bb);
       gl2 := DEMglb[DEM2].sfBoundBox2tGridLimits(bb);

       aLine := GISdb[db].MyData.GetFieldByNameAsString('NAME') + ',' + RealToString(0.5*(bb.YMin + bb.YMax),-12,-4)  + ',' + RealToString(0.5*(bb.xMin + bb.xMax),-12,-4) +
          RealToString(GISdb[db].MyData.GetFieldByNameAsFloat('GRID_FULL'),-8,-2);

       if DoThinning then begin
          ComputeSSIM(DEM1,DEM2,gl1,SSIM,Luminance,Contrast,Structure);
          aLine := aline + ',' + RealToString(SSIM,-12,-8);
       end;

       for i := 1 to 5 do begin
          gl1 := DEMglb[Grids[i]].sfBoundBox2tGridLimits(bb);
          DEMglb[Grids[i]].ElevationStatistics(gl1,Mean,Std,NPts);
          aLine := aline + ',' + RealToString(Mean,-12,-8);
       end;
       Results.Add(aLine);
       GISdb[db].MyData.Next;
    end;
    DEMglb[DEM1].SelectionMap.StringListToLoadedDatabase(Results,Petmar.NextFileNumber(MDTempDir,'SSMI_comparison','.dbf'));
end;


procedure NormalizeDEMforSSIM(DEM : integer; What : shortstring);
var
   Min,Max : float32;
   NormDB : tMyData;
   fName : PathStr;
begin
   if ValidDEM(DEM) then begin
      {$IfDef RecordSSIMNormalization} WriteLineToDebugFile('NormalizeDEMforSSIM in for ' + DEMGlb[DEM].AreaName); {$EndIf};
      fName := DEMIXSettingsDir + 'ssim_normalization.dbf';
      NormDB := tMyData.Create(fName);
      NormDB.ApplyFilter('CRITERION=' + QuotedStr(What));
      if (NormDB.FiltRecsInDB = 1) then begin
         Min := NormDB.GetFieldByNameAsFloat('MIN');
         Max := NormDB.GetFieldByNameAsFloat('MAX');
      end
      else begin
         Min := DEMGlb[DEM].DEMheader.MinElev;
         Max := DEMGlb[DEM].DEMheader.MaxElev;
         Petmar.ReadDefault('SSIM normalization min for ' + What,Min);
         Petmar.ReadDefault('SSIM normalization max for ' + What,Max);
         NormDB.Insert;
         NormDB.SetFieldByNameAsString('CRITERION',What);
         NormDB.SetFieldByNameAsFloat('MIN',Min);
         NormDB.SetFieldByNameAsFloat('MAX',Max);
         NormDB.Post;
      end;
      WantShowProgress := false;
      DEMGlb[DEM].ClipToVerticalRange(Min,Max);
      {$IfDef RecordSSIMNormalization} WriteLineToDebugFile('NormalizeDEMforSSIM z range clipped'); {$EndIf};
      DEMGlb[DEM].AddConstantToGrid(-Min);
      {$IfDef RecordSSIMNormalization} WriteLineToDebugFile('NormalizeDEMforSSIM min set to 0'); {$EndIf};
      DEMGlb[DEM].MultiplyGridByConstant(1/(Max-Min));
      {$IfDef RecordSSIMNormalization} WriteLineToDebugFile('NormalizeDEMforSSIM range set to 1'); {$EndIf};
      DEMGlb[DEM].CheckMaxMinElev;
      WantShowProgress := true;
      {$IfDef RecordSSIMNormalization} WriteLineToDebugFile('NormalizeDEMforSSIM out'); {$EndIf};
   end;
end;


function ComputeSSIM(DEM1,DEM2 : integer; gl1 : tGridLimits; var SSIM,Luminance,Contrast,Structure : float64) : boolean; inline;
//grid should have been normalized before calling, range 0 to 1
const
  MaxIntensity = 1;
  c1 = 0.01;    //C1 := sqr(0.01 * MaxIntensity);
  c2 = 0.03;    //C2 := sqr(0.03 * MaxIntensity);
  c3 = 0.015;   //C3 := c2 / 2;
var
  r,covar,Mean1,Mean2,StdDev1,StdDev2,MeanDiff,MeanAbsDiff : float64;
  NPts : int64;
begin
   {$IfDef RecordSSIMFullTooMuch} WriteLineToDebugFile('ComputeSSIM '  + DEMGlb[DEM1].AreaName + ',' + DEMGlb[DEM2].AreaName); {$EndIf}
   Result := CovariancesFromTwoGrids(gl1,DEM1,DEM2,NPts,r,covar,Mean1,Mean2,StdDev1,StdDev2,MeanDiff,MeanAbsDiff,false);  //do not record failures, there could be a huge number if grid has voids
   if Result then begin
      {$IfDef RecordSSIMFullTooMuch} WriteLineToDebugFile('Covariance done, covar=' + RealToString(Covar,-12,2)); {$EndIf}
      Luminance := (2 * Mean1 * Mean2 + C1*SSIM_fudge) / (Sqr(Mean1) + Sqr(Mean2) + C1*SSIM_fudge);
      Contrast := (2 * stddev1 * stddev2 + C2*SSIM_fudge) / (sqr(StdDev1) + sqr(StdDev2) + C2*SSIM_fudge);
      Structure := (Covar + c3*SSIM_fudge) / (stddev1 * stddev2 + C3*SSIM_fudge);
      SSIM := Luminance * Contrast * Structure;
      {$IfDef RecordSSIMFullTooMuch} WriteLineToDebugFile('ComputeSSIM '  + DEMGlb[DEM1].AreaName + ',' + DEMGlb[DEM2].AreaName + ',' +
         RealToString(ssim,-12,8) + ',' + RealToString(luminance,-12,8) + ',' + RealToString(contrast,-12,8) + ',' + RealToString(structure,-12,8));
      {$EndIf}
   end;
end;


function MakeSSIMMap(OpenMap,AlreadyNormalized : boolean; DEM1,DEM2,NumberOfGrids,WindowSize : integer; ThinFactor : integer = 1; AreaName : shortstring = '') : integer;
//assumes an elevation grid for the normalization
//normalizes the two grids, but then reloads them
{$IfDef RepeatProblematicComputations}
   label
      Restart;
{$EndIf}
const
   Names : array[1..4] of shortstring = ('SSIM','Luminance','Contrast','Structure');
var
  r,covar,Mean1,Mean2,StdDev1,StdDev2 : float64;
  xoffset,yoffset,x,y,xl,yl,i : integer;
  gl1 : tGridLimits;
  ng : array[1..4] of integer;
  nv : array[1..4] of float64;
  GridLimits : tGridLimits;
  TStr : shortstring;
begin
  if ValidDEM(DEM1) and ValidDEM(DEM2) then begin
     {$IfDef RecordSSIMVeryFull} WriteLineToDebugFile('ComputeSSIM in grids=' + IntToStr(NumberOfGrids) + ' '  + AreaName + ' DEM1=' + DEMglb[DEM1].AreaName + ' DEM2=' + DEMglb[DEM2].AreaName); {$EndIf}
     if DEMglb[DEM1].SecondGridJustOffset(DEM2,xoffset,yoffset) then begin
         HeavyDutyProcessing := true;
         if not AlreadyNormalized then begin
            NormalizeDEMforSSIM(DEM1,'Elev');
            NormalizeDEMforSSIM(DEM2,'Elev');
            {$IfDef RecordSSIMFull} WriteLineToDebugFile('Grids normalized'); {$EndIf}
         end;

         GridLimits := DEMglb[DEM1].FullDEMGridLimits;
         DEMglb[DEM1].FilledGridBox(GridLimits);

         if (ThinFactor > 1) then begin
            if (AreaName = '') then AreaName := 'SSIM_thin_' + IntToStr(ThinFactor) + '_' + DEMGlb[DEM1].AreaName + '_' + DEMGlb[DEM2].AreaName;
            ng[1] := DEMGlb[DEM1].ThinAndOpenGridSetMissing(ThinFactor,FloatingPointDEM,AreaName,euUndefined);
         end
         else begin
            for i := 1 to NumberOfGrids do begin
               if (AreaName = '') then AreaName := Names[i] + '_' + DEMGlb[DEM1].AreaName + '_' + DEMGlb[DEM2].AreaName
               else AreaName := AreaName + '_' + Names[i];
               ng[i] := DEMGlb[DEM1].CloneAndOpenGridSetMissing(FloatingPointDEM,AreaName,euUndefined);
            end;
         end;

         WindowSize := WindowSize div 2;
         if ShowSatProgress then StartProgress('Make SSIM Map ' + AreaName);
         x := GridLimits.xgridLow;
         while x <= GridLimits.xgridHigh do begin
            gl1.XGridLow := ThinFactor * x - WindowSize;
            gl1.XGridHigh := ThinFactor * x + WindowSize;
            if ShowSatProgress and (x mod 250 = 0) then begin
               UpdateProgressBar(x/DEMglb[DEM1].DEMHeader.NumCol);
            end;
            y := GridLimits.ygridLow;
            while y <= GridLimits.ygridHigh do begin
                gl1.YGridLow := ThinFactor * y - WindowSize;
                gl1.YGridHigh := ThinFactor * y + WindowSize;
                if ComputeSSIM(DEM1,DEM2,gl1,nv[1],nv[2],nv[3],nv[4]) then begin
                   for i := 1 to NumberOfGrids do begin
                      if (not IsNAN(nv[i])) then DEMGlb[ng[i]].SetGridElevation(x,y,nv[i]);
                   end;
                end;
                inc(y,1);
            end;
            inc(x,1);
         end;
         for i := 1 to NumberOfGrids do begin
            DEMglb[ng[i]].CheckMaxMinElev;
            if OpenMap then DEMglb[ng[i]].SetUpMap(true,mtElevSpectrum);
         end;
         if (not AlreadyNormalized) then begin
            DEMGlb[DEM1].ReloadDEM(true);
            DEMGlb[DEM2].ReloadDEM(true);
            {$IfDef RecordSSIMFull} WriteLineToDebugFile('Reload Grids'); {$EndIf}
         end;
         Result := ng[1];
         HeavyDutyProcessing := false;
         {$IfDef RecordSSIMFull} WriteLineToDebugFile('ComputeSSIM out, grid= ' + IntToStr(Result) + '  ' + AreaName); {$EndIf}
      end
      else begin
         TStr := 'Incompatible grids for SSIM, ' + DEMglb[DEM1].AreaName + ' and ' + DEMglb[DEM2].AreaName;
         {$IfDef RecordDEMIX} WriteLineToDebugFile(TStr); {$EndIf}
         {$IfDef RecordDEMIX} WriteLineToDebugFile(DEMGlb[DEM1].AreaName + '  ' + DEMGlb[DEM1].KeyParams(true)); {$EndIf}
         {$IfDef RecordDEMIX} WriteLineToDebugFile(DEMGlb[DEM2].AreaName + '  ' + DEMGlb[DEM2].KeyParams(true)); {$EndIf}
         MessageToContinue(TStr);
      end;
  end
  else begin
     Result := 0;
     TStr := 'function MakeSSIMMap fails, DEM1=' +  IntToStr(DEM1) + '  DEM2=' + IntToStr(DEM2);
     {$IfDef RecordSSIMFull} WriteLineToDebugFile(Tstr); {$EndIf}
  end;
end {function MakeSSIMMap};



function RemoveAreaFromDEMName(Area,DEMarea : shortstring) : shortstring;
begin
   Result := DEMarea;
   Delete(Result,1,succ(length(Area)));
end;


procedure DoSSIMandFUVForAnArea(AreaName : shortstring; Overwrite,AreasInsteadOfTiles : boolean);
//July 2025, changed to only allow one mode at a time
var
   DEMIXtileDB,i,OnCriterion : integer;
   ResultsAnalysis,TheCriteria : tStringList;
   SavefNameAnalysis,WetnessName : PathStr;
   aLine,TileName : shortstring;
   bb : sfBoundBox;
   TileFull : float32;


   function ProcessDEMTileSSIM(Ref,Test : integer; bb : sfBoundBox) : float32;
   var
      SSIM,Luminance,Contrast,Structure : float64;
      RefGridLimits,TestGridLimits : tGridLimits;
   begin
      if ValidDEM(Ref) and ValidDEM(Test) then begin
         RefGridLimits := DEMglb[Ref].sfBoundBox2tGridLimits(bb);
         ComputeSSIM(Ref,Test,RefGridLimits,SSIM,Luminance,Contrast,Structure);
         Result := 1 - SSIM;
         if (Result > 1) then Result := 1;
      end
      else begin
         Result := -999;
      end;
   end;


   procedure DoCriterion(Criterion : ANSIString; usingPointGrids,usingAreaGrids : tDEM_int_array; ClearDerived : boolean = true);
   var
      i,UsingRef : integer;
      Criterion2,WhatsMissing : shortstring;
      RefGridLimits : tGridLimits;
      gl1 : tGridLimits;
      Mean,Std : float32;
      NPts : int64;

            function TileSetUp : shortstring;
            begin
                bb := GISdb[DEMIXtileDB].MyData.GetRecordBoundingBox;
                TileName := GISdb[DEMIXtileDB].MyData.GetFieldByNameAsString('NAME');
                TileFull := GISdb[DEMIXtileDB].MyData.GetFieldByNameAsFloat('GRID_FULL');
                Result := TileName  + ',' + AreaName + ',' + RealToString(0.5*(bb.YMin+bb.YMax),-12,4) + ',' + RealToString(0.5*(bb.xMin+bb.xMax),-12,4) + ',' + RealToString(TileFull,-8,-2) + ',';
            end;

             procedure FigureSSIMstats(DEM : integer);
             begin
                if ValidDEM(DEM) then begin
                   gl1 := DEMglb[DEM].sfBoundBox2tGridLimits(bb);
                   DEMglb[DEM].ElevationStatistics(gl1,Mean,Std,NPts);
                   {$IfDef RecordSSIM}
                      if IsNAN(1-Mean) then begin
                         WriteLineToDebugFile('NAN fail for tile ' + TileName);
                         DEMglb[DEM].ElevationStatistics(gl1,Mean,Std,NPts);
                      end;
                   {$EndIf}
                   aLine := aline + ',' + RealToString(1-Mean,-12,8);
                end
                else begin
                   {$IfDef RecordDEMIX} WriteLineToDebugFile('Fail for tile ' + TileName); {$EndIf}
                   aLine := aline + ',' + '-999';
                end;
             end;


             function MissingDEMs : boolean;
             var
                i : integer;
             begin
                WhatsMissing := '';
                if (NumPtDEMs > 0) then begin
                   for i := 0 to NumPtDEMs do begin
                      if not ValidDEM(usingPointGrids[i]) then begin
                         WhatsMissing := WhatsMissing + '  ' + DEMglb[PointDEMs[i]].AreaName;
                      end;
                   end;
                end;
                if (NumAreaDEMs > 0) then begin
                   for i := 0 to NumAreaDEMs do begin
                      if not ValidDEM(usingAreaGrids[i]) then begin
                         WhatsMissing := WhatsMissing + '  ' + DEMglb[AreaDEMs[i]].AreaName;
                      end;
                   end;
                end;
                if (WhatsMissing <> '') then begin
                   HighlightLineToDebugFile(Criterion + ' has missing data grids for ' + AreaName + '  ' + WhatsMissing);
                   Result := true;
                end
                else Result := false;;
             end;

   var
      FUV : float64;
   begin {procedure DoCriterion}
       {$If Defined(RecordDEMIXFull) or Defined(RecordFUVcreate)} WriteLineToDebugFile(AreaName + ' Criterion=' + Criterion); {$EndIf}
       if MissingDEMs then exit;

       if MDDef.DoFUV or MDDef.DoPartials or MDDef.DoCurvatures then begin //do FUV correlation coefficients first before normalizing grids for SSIM
          {$If Defined(RecordDEMIXTimeCriterion)}  Stopwatch := TStopwatch.StartNew; {$EndIf}
          Criterion2 := Criterion + '_FUV';   //StringReplace(Criterion,'SSIM','FUV',[rfReplaceAll, rfIgnoreCase]);

          if DEMIXanalysismode in [DEMIXneo] then begin
             aLine := TileSetup + Criterion2;
             if (NumAreaDEMs > 0) then begin
                for i := 1 to NumAreaDEMs do begin
                   FUV := GetFUVForPairGrids(DEMglb[usingAreaGrids[0]].FullDEMGridLimits,usingAreaGrids[0],usingAreaGrids[i]);
                   aLine := aline + ',' + RealToString(FUV,-12,8);
                end;
             end;
             if (NumPtDEMs > 0) then begin
                for i := 1 to NumPtDEMs do begin
                   FUV := GetFUVForPairGrids(DEMglb[usingPointGrids[0]].FullDEMGridLimits,usingPointGrids[0],usingPointGrids[i]);
                   {$If Defined(RecordFUV)} WriteLineToDebugFile(IntToStr(i) + ' ref=' + IntToStr(usingPointGrids[0]) +
                       ' test=' + IntToStr(usingPointGrids[i]) + ' FUV=' + RealToString(FUV,-12,-6)); {$EndIf}
                   aLine := aline + ',' + RealToString(FUV,-12,8);
                end;
             end;
             ResultsAnalysis.Add(aline);
             {$If Defined(RecordFUV)} WriteLineToDebugFile(aLine); {$EndIf}
          end
          else begin
             GISdb[DEMIXtileDB].MyData.First;
             while not GISdb[DEMIXtileDB].MyData.eof do begin
                aLine := TileSetup + Criterion2;
                bb := GISdb[DEMIXtileDB].MyData.GetRecordBoundingBox;
                if (NumAreaDEMs > 0) then begin
                   RefGridLimits := DEMglb[usingAreaGrids[0]].sfBoundBox2tGridLimits(bb);
                   for i := 1 to NumAreaDEMs do begin
                      {$If Defined(RecordFUVbb)} WriteLineToDebugFile(DEMglb[usingAreaGrids[i]].AreaName + '  bb=' + GridLimitsToString(RefGridLimits)); {$EndIf}
                      aLine := aline + ',' + RealToString(GetFUVForPairGrids(RefGridLimits,usingAreaGrids[0],usingAreaGrids[i]),-12,8);
                   end;
                end;

                if (NumPtDEMs > 0) then begin
                   for i := 1 to NumPtDEMs do begin
                      UsingRef := RefGridForThisPointGrid(UsingPointGrids,i);
                      RefGridLimits := DEMglb[UsingRef].sfBoundBox2tGridLimits(bb);
                      {$If Defined(TrackCovariance)} WriteLineToDebugFile(''); WriteLineToDebugFile('GridLimits: ' + GridLimitsToString(RefGridLimits)); {$EndIf}
                      {$If Defined(RecordFUVbb)} WriteLineToDebugFile(DEMglb[usingPointGrids[i]].AreaName + '  bb=' + GridLimitsToString(RefGridLimits)); {$EndIf}
                      aLine := aline + ',' + RealToString(GetFUVForPairGrids(RefGridLimits,UsingRef,usingPointGrids[i]),-12,8);
                   end;
                end;
                ResultsAnalysis.Add(aline);
                GISdb[DEMIXtileDB].MyData.Next;
             end;
          end;
          {$IfDef RecordDEMIXTimeCriterion} WriteLineToDebugFile('FUV ' + Criterion + '   ' + RealToString(Stopwatch.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}
       end;

       if MDDef.DoSSIM then begin
          InitializePointAndAreaGrids(PtSSIMGrids, AreaSSIMGrids);
          //normalize grids for SSIM
          {$IfDef RecordDEMIXTimeCriterion}  Stopwatch := TStopwatch.StartNew; {$EndIf}
          Criterion2 := Criterion + '_SSIM';   //StringReplace(Criterion,'SSIM','FUV',[rfReplaceAll, rfIgnoreCase]);

          for i := dmxFirstArea to NumAreaDEMs do NormalizeDEMforSSIM(AreaGrids[i],Criterion);
          for i := dmxFirstPoint to NumPtDEMs do NormalizeDEMforSSIM(PointGrids[i],Criterion);
          {$IfDef RecordDEMIXFull} WriteLineToDebugFile('Grids normalized for Criterion=' + Criterion); {$EndIf}
          wmdem.SetPanelText(2, TileName + ' SSIM grids',true);

          for i := 1 to NumAreaDEMs do begin
             AreaSSIMGrids[i] := MakeSSIMMap(MDDef.OpenSavedMapsFUVSSIM, true,usingAreaGrids[0],usingAreaGrids[i],1,11);
          end;

          for i := 1 to NumPtDEMs do begin
             UsingRef := RefGridForThisPointGrid(UsingPointGrids,i);
             PtSSIMGrids[i] := MakeSSIMMap(MDDef.OpenSavedMapsFUVSSIM,true,UsingRef,usingPointGrids[i],1,11);
             {$IfDef RecordDEMIXSSIMGrid}
                 if ValidDEM(PtSSIMGrids[i]) then TStr := 'Good PtSSIMGrid=' + IntToStr(PtSSIMGrids[i]) + ' '
                 else TStr := 'Fail PtSSIMGrid ';
                 WriteLineToDebugFile(TStr + TileName + ' Ref DEM=' + IntToStr(ThisRefDEM) + ' TestDEM=' + IntToStr(ThisTestDEM));
             {$EndIf}
          end;
          {$IfDef RecordDEMIXFull} WriteLineToDebugFile('SSIM maps created for Criterion=' + Criterion); {$EndIf}

          wmdem.SetPanelText(3, Criterion + ' average tiles',true);
          GISdb[DEMIXtileDB].MyData.First;
          while not GISdb[DEMIXtileDB].MyData.eof do begin
             aLine := TileSetUp + Criterion;
             wmdem.SetPanelText(2, TileName + ' SSIM',true);
             for i := 1 to NumAreaDEMs do begin
                FigureSSIMstats(AreaSSIMGrids[i]);
             end;
             for i := 1 to NumPtDEMs do begin
                FigureSSIMstats(PtSSIMGrids[i]);
             end;
             ResultsAnalysis.Add(aline);
             GISdb[DEMIXtileDB].MyData.Next;
          end;
          for i := 1 to NumPtDEMs do CloseSingleDEM(PtSSIMGrids[i]);
          for i := 1 to NumAreaDEMs do CloseSingleDEM(AreaSSIMGrids[i]);
          {$IfDef RecordDEMIXTimeCriterion} WriteLineToDebugFile('SSIM ' + What + ' Do criterion  ' + RealToString(Stopwatch.Elapsed.TotalSeconds/60,-12,-4) + ' min'); {$EndIf}
       end;
       if ClearDerived then ClearDerivedGrids;
       wmdem.SetPanelText(2,'',true);
   end {procedure DoCriterion};


   procedure UpdateCriteriaPanel(Criterion : shortstring; Together : integer = 1);
   begin
       inc(OnCriterion,Together);
       wmdem.SetPanelText(3,DEMIXModeName + '  ' + IntToStr(Oncriterion)  + '/' + IntToStr(TheCriteria.Count) + ' ' + Criterion,true);
   end;


          procedure PartialOrCurvature;
          var
             Choice : shortstring;
             i,j : integer;
             CritName : shortstring;
             fName : PathStr;
          begin
             if MDDef.DoPartials then begin
                Choice := ' -p';
             end
             else if MDDef.DoCurvatures then begin
                Choice := ' -a'
             end;

             {$If Defined(RecordFUVcreateFull)} WriteLineToDebugFile('RUN_LSPcalculator point DEMs'); {$EndIf}
             for i := dmxFirstPoint to NumPtDEMs do begin
                wmdem.SetPanelText(3,DEMGlb[PointDEMs[i]].AreaName,true);
                RUN_LSPcalculator(PointDEMs[i],choice);
             end;
             {$If Defined(RecordFUVcreateFull)} WriteLineToDebugFile('RUN_LSPcalculator area DEMs'); {$EndIf}
             for i := dmxFirstArea to NumAreaDEMs do begin
                wmdem.SetPanelText(3,DEMGlb[AreaDEMs[i]].AreaName,true);
                RUN_LSPcalculator(AreaDEMs[i],Choice);
             end;

             for j := 0 to pred(TheCriteria.Count) do begin
                 CritName := TheCriteria.Strings[j];
                 UpdateCriteriaPanel(CritName);
                 if CritName = 'ELEV' then begin
                    DoCriterion('ELEV',PointDEMs,AreaDEMs);
                 end
                 else begin
                     {$If Defined(RecordFUVcreateFull)} WriteLineToDebugFile(CritName + ' open grids'); {$EndIf}
                     for i := dmxFirstPoint to NumPtDEMs do begin
                        fName := MDtempDir + DEMglb[PointDEMs[i]].AreaName + '_' + CritName + '.tif';
                        {$If Defined(RecordFUVcreateFull)} WriteLineToDebugFile(fName); {$EndIf}
                        PointGrids[i] := OpenNewDEM(fName,false);
                     end;
                     {$If Defined(RecordFUVcreateFull)} WriteLineToDebugFile(CritName + ' open area DEMs'); {$EndIf}
                     for i := dmxFirstArea to NumAreaDEMs do begin
                        fName := MDtempDir + DEMglb[AreaDEMs[i]].AreaName + '_' + CritName + '.tif';
                        {$If Defined(RecordFUVcreateFull)} WriteLineToDebugFile(fName); {$EndIf}
                        AreaGrids[i] := OpenNewDEM(fName,false);
                     end;
                     {$If Defined(RecordFUVcreateFull)} WriteLineToDebugFile(CritName + ' call DoCriterion'); {$EndIf}
                     DoCriterion(CritName,PointGrids,AreaGrids);
                 end;
             end;
          end;


var
   Success : boolean;
   j : integer;
   fName : PathStr;
   HowDone,CritName,Choice : shortstring;
   FullTiles : tStringList;
begin {procedure DoSSIMandFUVForAnArea}
    if DEMIXanalysismode in [DEMIXneo,DEMIXtraditional] then begin
       if MDDef.DoFUV then SaveFNameAnalysis := FUVresultsDir + AreaName + '_fuv_results.csv';
       if MDDef.DoSSIM then SaveFNameAnalysis := SSIMresultsDir + AreaName + '_ssim_results.csv';
       if MDDef.DoPartials then SaveFNameAnalysis := PartialsresultsDir + AreaName + '_partials_results.csv';
       if MDDef.DoCurvatures then SaveFNameAnalysis := CurvaturesResultsDir + AreaName + '_curvatures_results.csv';
    end
    else begin
       MessageToContinue('Undefined DEMIXanalysismode');
       exit;
    end;
    if Overwrite then begin
       if FileExists(SaveFNameAnalysis) then File2Trash(SaveFNameAnalysis);
    end
    else begin
       if FileExists(SaveFNameAnalysis) then begin
         exit;
       end;
    end;

    SaveBackupDefaults;
    if MDDef.DoSSIM or MDDef.DoFUV or MDDef.DoPartials or MDDef.DoCurvatures then begin
       OnCriterion := 0;

       {$If Defined(TimeGridsForArea)} Stopwatch2 := TStopwatch.StartNew; {$EndIf}
       {$IfDef RecordDEMIX} HighLightLineToDebugFile('AreaSSIMandFUVComputations area=' + AreaName); {$EndIf}
       wmdem.SetPanelText(3, 'Load DEMs',true);
       ShowSatProgress :=  false;
       ResultsAnalysis := tStringList.Create;
       if MDDef.DoFUV then begin
          TheCriteria := GetListDEMIXOrderedCriteria(DemixSettingsDir + 'demix_fuv_parameters.dbf');
          SafeMakeDir(FUVresultsDir);
       end;
       if MDDef.DoSSIM then begin
          TheCriteria := GetListDEMIXOrderedCriteria(DemixSettingsDir + 'demix_ssim_parameters.dbf');
          SafeMakeDir(SSIMresultsDir);
       end;
       if MDDef.DoPartials then begin
          TheCriteria := GetListDEMIXOrderedCriteria(DemixSettingsDir + 'demix_partials_criteria.dbf');
          SafeMakeDir(PartialsResultsDir);
       end;
       if MDDef.DoCurvatures then begin
          TheCriteria := GetListDEMIXOrderedCriteria(DemixSettingsDir + 'demix_curvatures_criteria.dbf');
          SafeMakeDir(CurvaturesResultsDir);
       end;

       if OpenBothPixelIsDEMs(AreaName,'',DEMIX_Ref_1sec,DEMIX_test_dems,MDDef.OpenSavedMapsFUVSSIM) then begin
          {$If Defined(RecordFUVcreate)} ShowDEMIXGrids(AreaName + ' DEMs opened',PointDEMs,AreaDEMs); {$EndIf}
          InitializePointAndAreaGrids(PointGrids,AreaGrids);
          aline := 'DEMIX_TILE,AREA,LAT,LONG,GRID_FULL,CRITERION';
          for i := 1 to NumAreaDEMs do begin
             aline := aLine + ',' + RemoveAreaFromDEMName(AreaName,DEMGlb[AreaDEMs[i]].AreaName);
          end;
          for i := 1 to NumPtDEMs do begin
             aline := aLine + ',' + RemoveAreaFromDEMName(AreaName,DEMGlb[PointDEMs[i]].AreaName);
          end;
          ResultsAnalysis.Add(aLine);

          if AreasInsteadOfTiles then begin
             FullTiles := tStringList.Create;
             FullTiles.Add('NAME,GRID_FULL,LAT_HI,LONG_HI,LAT_LOW,LONG_LOW');
             FullTiles.Add('FULL_AREA,100,' + RealToString(DEMGlb[PointDEMs[0]].DEMBoundBoxGeo.YMax,-12,-6) + ',' +  RealToString(DEMGlb[PointDEMs[0]].DEMBoundBoxGeo.xMax,-12,-6) + ',' +
                                              RealToString(DEMGlb[PointDEMs[0]].DEMBoundBoxGeo.YMin,-12,-6) + ',' +  RealToString(DEMGlb[PointDEMs[0]].DEMBoundBoxGeo.xMin,-12,-6));
             DEMIXtileDB := StringList2CSVtoDB(FullTiles,MDTempDir + 'outline.dbf',false,false,false);
          end
          else begin
             DEMIXtileDB := DEMIXtileFill(PointDEMs[0],DEMGlb[PointDEMs[0]].DEMBoundBoxGeo,false);
             GISdb[DEMIXtileDB].ApplyGISFilter(FilterForDEMIXtilesToUse);
          end;
          {$If Defined(RecordFUVcreate)} WriteLineToDebugFile(AreaName + ' num tiles=' + IntToStr(GISdb[DEMIXtileDB].MyData.FiltRecsInDB)); {$EndIf}

          if MDDef.DoPartials or MDDef.DoCurvatures then begin
             PartialOrCurvature;
          end
          else begin
              for j := 0 to pred(TheCriteria.Count) do begin
                 CritName := TheCriteria.Strings[j];
                 UpdateCriteriaPanel(CritName);
                 if CritName = 'ELEV' then DoCriterion('ELEV',PointDEMs,AreaDEMs)
                 else begin
                    CreateLSPgrids(false,CritName,AreaName,PointGrids,AreaGrids);
                    DoCriterion(CritName,PointGrids,AreaGrids);
                 end;
              end;
          end;

           if (ResultsAnalysis.Count > 1) then begin
              ResultsAnalysis.SaveToFile(SavefNameAnalysis);
              {$IfDef RecordDEMIXFull} WriteLineToDebugFile('Save to ' + SaveFNameAnalysis); {$EndIf}
           end
           else begin
              {$IfDef RecordDEMIXFull} HighlightLineToDebugFile('No results for ' + SaveFNameAanlysis); {$EndIf}
           end;
           ResultsAnalysis.Free;

          if (not MDDef.OpenSavedMapsFUVSSIM) then CloseAllDEMs;
          CloseAndNilNumberedDB(DEMIXtileDB);
          CleanUpTempDirectory;  //lot of files created by SAGA and WBT
       end
       else begin
          {$IfDef RecordDEMIX} HighLightLineToDebugFile('Missing DEM files for area ' + AreaName); {$EndIf}
       end;
       {$If Defined(TimeGridsForArea)} WriteLineToDebugFile(AreaName + ' completed ' + RealToString(Stopwatch2.Elapsed.TotalSeconds/3600,-12,-4) + ' hours');  {$EndIf}
    end;
    RestoreBackupDefaults;
end {procedure DoSSIMandFUVForAnArea};



procedure AreaSSIMandFUVComputations(Overwrite,AreasInsteadOfTiles : boolean; Areas : tStringList);
var
   Area  : shortstring;
   i     : integer;
begin
   try
      GetDEMIXPaths(true);
      {$IfDef RecordDEMIX} HighLightLineToDebugFile('AreaSSIMandFUVComputations in, selected areas=' + IntToStr(Areas.Count)); {$EndIf}

      if MDDef.DEMIX_Geo_Tiles then begin
          if MDDef.DoFUV then SafeMakeDir(FUVresultsDir);
          if MDDef.DoSSIM then SafeMakeDir(SSIMresultsDir);
          if MDDef.DoPartials then SafeMakeDir(PartialsResultsDir);
          if MDDef.DoCurvatures then SafeMakeDir(CurvaturesResultsDir);
      end
      else begin
      end;
      LockStatusBar := true;
      wmdem.SetPanelText(0, 'Started ' + TimeToStr(Now),true);
      HeavyDutyProcessing := true;
      if MDDef.ProcessLoopsForward then begin
         for i := 0 to pred(Areas.Count) do begin
            Area := Areas.Strings[i];
            wmdem.SetPanelText(1, IntToStr(succ(i)) + '/' + IntToStr(Areas.Count) + '  ' + Area + ' FUV',true);
            DoSSIMandFUVForAnArea(Area,Overwrite,AreasInsteadOfTiles);
         end;
      end
      else begin
         for i := pred(Areas.Count) downto 0 do begin
            Area := Areas.Strings[i];
            wmdem.SetPanelText(1, IntToStr(succ(i)) + '/' + IntToStr(Areas.Count) + '  ' + Area + ' FUV',true);
            DoSSIMandFUVForAnArea(Area,Overwrite,AreasInsteadOfTiles);
         end;
      end;
   finally
      EndDEMIXProcessing(0,true);
   end;
   {$IfDef RecordDEMIX} HighLightLineToDebugFile('AreaSSIMandFUVComputations out'); {$EndIf}
end {procedure AreaSSIMandFUVComputations};


