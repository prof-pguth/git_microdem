{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
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
          ComputeSSIM(DEM1,DEM2,gl1,{gl2,}SSIM,Luminance,Contrast,Structure);
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
    DEMglb[DEM1].SelectionMap.StringListToLoadedDatabase(Results,Petmar.NextFileNumber(MDTempDir,'SSMI_comparison_','.dbf'));
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



    function GetFUVForPair(RefGridLimits : tGridLimits; Grid1,Grid2 : integer) : float64;
    var
       Mean1,Mean2,StdDev1,StdDev2,r,covar,MeanDiff,MeanAbsDiff : float64;
       NPts : int64;
    begin
       if ValidDEM(Grid1) and ValidDEM(Grid2) then begin
          {$IfDef TrackSWcorner} DEMGlb[Grid1].WriteToDebugSWCornerForComputations('AreaSSIMComputations'); DEMGlb[Grid2].WriteToDebugSWCornerForComputations('AreaSSIMComputations'); {$EndIf}
          if CovariancesFromTwoGrids(RefGridLimits,Grid1,Grid2,Npts,r,covar,Mean1,Mean2,StdDev1,StdDev2,MeanDiff,MeanAbsDiff) then begin
             Result := 1-sqr(r);
             {$If Defined(TrackCovariance)} WriteLineToDebugFile('FUV=' + RealToString(Result,10,8) + '  ' + DEMGlb[Grid1].AreaName + '  ' + DEMGlb[Grid2].AreaName); {$EndIf}
          end
          else begin
             {$If Defined(RecordDEMIXfull)} WriteLineToDebugFile('DoCovarianceForPair computation Fail for tile ' + TileName + ' TileFill=' + RealToString(TileFull,-8,-2) + '%'); {$EndIf}
             Result := -999;
             {$IfDef RepeatProblematicComputations}
                MessageToContinue('Check debugger for computation fail');
                CovariancesFromTwoGrids(RefGridLimits,Grid1,Grid2, r,covar,Mean1,Mean2,StdDev1,StdDev2);
             {$EndIf}
          end;
       end
       else begin
          Result := -999;
          {$If Defined(RecordDEMIXfull)} WriteLineToDebugFile('Invalid grid, DoCovarianceForPair Fail for tile ' + ' ' + DEMglb[Grid1].AreaName + ' ' + DEMglb[Grid2].AreaName); {$EndIf}
       end;
    end;


procedure DoSSIMandFUVForAnArea(AreaName : shortstring; Overwrite,AreasInsteadOfTiles : boolean);
var
   DEMIXtileDB,{NumRef,j,}i : integer;
   ResultsSSIM,ResultsFUV : tStringList;
   SavefNameSSIM,SavefNameFUV,
   //fName,
   WetnessName : PathStr;
   aLine,TileName : shortstring;
   bb : sfBoundBox;
   TileFull : float32;
   //MissingFiles : boolean;

   function ProcessDEMTileSSIM(Ref,Test : integer; bb : sfBoundBox) : float32;
   var
      SSIM,Luminance,Contrast,Structure : float64;
      RefGridLimits,TestGridLimits : tGridLimits;
   begin
      if ValidDEM(Ref) and ValidDEM(Test) then begin
         RefGridLimits := DEMglb[Ref].sfBoundBox2tGridLimits(bb);
         ComputeSSIM(Ref,Test,RefGridLimits,SSIM,Luminance,Contrast,Structure);
         Result := 1-SSIM;
         if (Result > 1) then Result := 1;
      end
      else begin
         Result := -999;
      end;
   end;


   procedure DoCriterion(Criterion : ANSIString; usingPointGrids,usingAreaGrids : tDEM_int_array; ClearDerived : boolean = true);
   var
      i,{ThisRefDEM,ThisTestDEM,}UsingRef : integer;
      Criterion2,WhatsMissing,What{,TStr} : shortstring;
      RefGridLimits{,TestGridLimits} : tGridLimits;
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
   begin
       {$IfDef RecordDEMIXFull} WriteLineToDebugFile(AreaName + ' Criterion=' + Criterion); {$EndIf}
       if MissingDEMs then exit;

       if MDDef.DoFUV then begin //do FUV correlation coefficients first before normalizing grids for SSIM
          {$If Defined(RecordDEMIXTimeCriterion)}  Stopwatch := TStopwatch.StartNew; {$EndIf}
          Criterion2 := StringReplace(Criterion,'SSIM','FUV',[rfReplaceAll, rfIgnoreCase]);

          if DEMIXanalysismode in [DEMIXneo] then begin
             aLine := TileSetup + Criterion2;
             if (NumAreaDEMs > 0) then begin
                for i := 1 to NumAreaDEMs do begin
                   FUV := GetFUVForPair(DEMglb[usingAreaGrids[0]].FullDEMGridLimits,usingAreaGrids[0],usingAreaGrids[i]);
                   aLine := aline + ',' + RealToString(FUV,-12,8);
                end;
             end;
             if (NumPtDEMs > 0) then begin
                for i := 1 to NumPtDEMs do begin
                   FUV := GetFUVForPair(DEMglb[usingPointGrids[0]].FullDEMGridLimits,usingPointGrids[0],usingPointGrids[i]);
                   aLine := aline + ',' + RealToString(FUV,-12,8);
                end;
             end;
             ResultsFUV.Add(aline);
          end
          else begin

             //this does not appear to get a limits for the tiles
             //it might have been broken when the option was created to work for areas instead of tiles

             GISdb[DEMIXtileDB].MyData.First;
             while not GISdb[DEMIXtileDB].MyData.eof do begin
                aLine := TileSetup + Criterion2;
                if (NumAreaDEMs > 0) then begin
                   if DEMIXanalysismode in [DEMIXneo] then RefGridLimits := DEMglb[usingAreaGrids[0]].FullDEMGridLimits
                   else RefGridLimits := DEMglb[usingAreaGrids[0]].sfBoundBox2tGridLimits(bb);
                   for i := 1 to NumAreaDEMs do begin
                      aLine := aline + ',' + RealToString(GetFUVForPair(RefGridLimits,usingAreaGrids[0],usingAreaGrids[i]),-12,8);
                   end;
                end;

                if (NumPtDEMs > 0) then begin
                   if DEMIXanalysismode in [DEMIXneo] then begin
                      RefGridLimits := DEMglb[usingPointGrids[0]].FullDEMGridLimits;
                   end
                   else begin
                      UsingRef := RefGridForThisPointGrid(UsingPointGrids,i);
                      RefGridLimits := DEMglb[UsingRef].sfBoundBox2tGridLimits(bb);
                   end;
                   {$If Defined(TrackCovariance)} WriteLineToDebugFile(''); WriteLineToDebugFile('GridLimits: ' + GridLimitsToString(RefGridLimits)); {$EndIf}
                   for i := 1 to NumPtDEMs do begin
                      aLine := aline + ',' + RealToString(GetFUVForPair(RefGridLimits,UsingRef,usingPointGrids[i]),-12,8);
                   end;
                end;
                ResultsFUV.Add(aline);
                GISdb[DEMIXtileDB].MyData.Next;
             end;
          end;
          {$IfDef RecordDEMIXTimeCriterion} WriteLineToDebugFile('FUV ' + Criterion + '   ' + RealToString(Stopwatch.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}
       end;


       if MDDef.DoSSIM then begin
          InitializePointAndAreaGrids(PtSSIMGrids, AreaSSIMGrids);
          //normalize grids for SSIM
          {$IfDef RecordDEMIXTimeCriterion}  Stopwatch := TStopwatch.StartNew; {$EndIf}
          What := BeforeSpecifiedCharacterAnsi(Criterion,'_');
          for i := dmxFirstArea to NumAreaDEMs do NormalizeDEMforSSIM(AreaGrids[i],What);
          for i := dmxFirstPoint to NumPtDEMs do NormalizeDEMforSSIM(PointGrids[i],What);
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
             ResultsSSIM.Add(aline);
             GISdb[DEMIXtileDB].MyData.Next;
          end;
          for i := 1 to NumPtDEMs do CloseSingleDEM(PtSSIMGrids[i]);
          for i := 1 to NumAreaDEMs do CloseSingleDEM(AreaSSIMGrids[i]);
          {$IfDef RecordDEMIXTimeCriterion} WriteLineToDebugFile('SSIM ' + What + ' Do criterion  ' + RealToString(Stopwatch.Elapsed.TotalSeconds/60,-12,-4) + ' min'); {$EndIf}
       end;
       if ClearDerived then ClearDerivedGrids;
       wmdem.SetPanelText(2,'',true);
   end;


var
   Success : boolean;
   //TStr,
   HowDone : shortstring;
   //fName2 : PathStr;
   FullTiles : tStringList;
begin {procedure DoSSIMandFUVForAnArea}
    if DEMIXanalysismode in [DEMIXneo,DEMIXtraditional] then begin
       SaveFNameSSIM := SSIMresultsDir + AreaName + '_ssim_results.csv';
       SaveFNameFUV := FUVresultsDir + AreaName + '_fuv_results.csv';
    end
    else begin
       MessageToContinue('Undefined DEMIXanalysismode');
       exit;
    end;
    SaveBackupDefaults;
    if Overwrite then begin
       if MDDef.DoSSIM and FileExists(SaveFNameSSIM) then File2Trash(SaveFNameSSIM);
       if MDDef.DoFUV and FileExists(SaveFNameFUV) then File2Trash(SaveFNameFUV);
    end
    else begin
       if (MDDef.DoSSIM and (FileExists(SaveFNameSSIM))) then MDDef.DoSSIM := false;
       if (MDDef.DoFUV and (FileExists(SaveFNameFUV))) then MDDef.DoFUV := false;
    end;

    if MDDef.DoSSIM or MDDef.DoFUV then begin
       {$If Defined(TimeGridsForArea)} Stopwatch2 := TStopwatch.StartNew; {$EndIf}
       {$IfDef RecordDEMIX} HighLightLineToDebugFile('AreaSSIMandFUVComputations area=' + AreaName); {$EndIf}
       wmdem.SetPanelText(3, 'Load DEMs',true);
       ShowSatProgress :=  false;
       if MDDef.DoFUV then begin
          ResultsFUV := tStringList.Create;
          ResultsFUV.SaveToFile(SaveFNameFUV);
       end;
       if MDDef.DoSSIM then begin
          ResultsSSIM := tStringList.Create;
          ResultsSSIM.SaveToFile(SaveFNameSSIM);
       end;

       if OpenBothPixelIsDEMs(AreaName,'',DEMIX_Ref_1sec,DEMIX_test_dems,MDDef.OpenSavedMapsFUVSSIM) then begin
          {$If Defined(TrackPixelIs) or Defined(RecordDEMIXFull)} ShowDEMIXGrids(AreaName + ' DEMs opened',PointDEMs,AreaDEMs); {$EndIf}
          InitializePointAndAreaGrids(PointGrids,AreaGrids);
          aline := 'DEMIX_TILE,AREA,LAT,LONG,GRID_FULL,CRITERION';
          for i := 1 to NumAreaDEMs do aline := aLine + ',' + ALOS_centroid_names[i];
          for i := 1 to NumPtDEMs do aline := aLine + ',' + SRTM_centroid_names[i];
          if MDDef.DoFUV then ResultsFUV.Add(aLine);
          if MDDef.DoSSIM then ResultsSSIM.Add(aLine);

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
          {$If Defined(RecordDEMIXFull)} WriteLineToDebugFile(AreaName + ' num tiles=' + IntToStr(GISdb[DEMIXtileDB].MyData.FiltRecsInDB)); {$EndIf}

          if MDDef.SSIM_hill then begin
             wmdem.SetPanelText(3,DEMIXModeName + ' 3 Hillshade',true);
             {$If Defined(TimeOpenCreateGrids)} Stopwatch := TStopwatch.StartNew; {$EndIf}
             HowDone := CreateDEMIXhillshadeGrids(AreaName,MDDef.OpenSavedMapsFUVSSIM,true);
             {$If Defined(TimeOpenCreateGrids)} WriteLineToDebugFile('Hillshade' + HowDone + RealToString(Stopwatch.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}
             DoCriterion('HILL_SSIM',PointGrids,AreaGrids);
          end;

          if MDDef.SSIM_Slope or MDDef.SSIM_ruff then begin
             wmdem.SetPanelText(3,DEMIXModeName + ' 4,5 Slope/ruff',true);
             {$If Defined(TimeOpenCreateGrids)} Stopwatch := TStopwatch.StartNew; {$EndIf}
             CreateDEMIXSlopeRoughnessGrids(AreaName,MDDef.OpenSavedMapsFUVSSIM,true);
             {$If Defined(TimeOpenCreateGrids)} WriteLineToDebugFile('MD Slope/ruff' + RealToString(Stopwatch.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}
             DoCriterion('SLOPE_SSIM',PointGrids2,AreaGrids2,false);
             DoCriterion('RUFF_SSIM',PointGrids,AreaGrids);
          end;

          if MDDef.SSIM_ROTOR then begin //created by WbE with Jupyter Notebook
             wmdem.SetPanelText(3,DEMIXModeName + ' 1 ROTOR',true);
             {$If Defined(TimeOpenCreateGrids)} Stopwatch := TStopwatch.StartNew; {$EndIf}
             Success := OpenGridsCreatedByExternalProgram(MDDef.OpenSavedMapsFUVSSIM,'WBT',AreaName,'rotor_',PointGrids,AreaGrids);
             {$If Defined(TimeOpenCreateGrids)} WriteLineToDebugFile('WbW Rotor created ' + RealToString(Stopwatch.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}
             if Success then begin
                DoCriterion('ROTOR_SSIM',PointGrids,AreaGrids);
             end
             else begin
                ClearDerivedGrids;
                {$IfDef RecordDEMIX} HighLightLineToDebugFile('ROTOR criterion fail for area=' + AreaName); {$EndIf}
             end;
          end;

          if MDDef.SSIM_HAND then begin //created by WbW via Jupyter or via WbtT if not found
             wmdem.SetPanelText(3,DEMIXModeName + ' 2 HAND',true);
             {$If Defined(TimeOpenCreateGrids)} Stopwatch := TStopwatch.StartNew; {$EndIf}
             if not OpenGridsCreatedByExternalProgram(MDDef.OpenSavedMapsFUVSSIM,'WBT',AreaName,'HAND_',PointGrids,AreaGrids) then begin
                 WBT_CreateDEMIX_HANDGrids(MDDef.OpenSavedMapsFUVSSIM);
             end;
             {$If Defined(TimeOpenCreateGrids)} WriteLineToDebugFile('WbT HAND created ' + RealToString(Stopwatch.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}
             DoCriterion('HAND_SSIM',PointGrids,AreaGrids);
          end;

          if MDDef.SSIM_Openness then begin
             wmdem.SetPanelText(3,DEMIXModeName + ' 6,7 Openness',true);
             {$If Defined(TimeOpenCreateGrids)} Stopwatch := TStopwatch.StartNew; {$EndIf}
             HowDone := CreateDEMIXOpennessGrids(AreaName,MDDef.OpenSavedMapsFUVSSIM,true);
             {$If Defined(TimeOpenCreateGrids)} WriteLineToDebugFile('MD Openness' + HowDone + RealToString(Stopwatch.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}
             DoCriterion('OPEND_SSIM',PointGrids2,AreaGrids2,false);
             DoCriterion('OPENU_SSIM',PointGrids,AreaGrids);
          end;

          if MDDef.SSIM_RRI then begin
             wmdem.SetPanelText(3,DEMIXModeName + ' 8 RRI',true);
             {$If Defined(TimeOpenCreateGrids)} Stopwatch := TStopwatch.StartNew; {$EndIf}
             CreateDEMIXRRIgrids(AreaName,MDDef.OpenSavedMapsFUVSSIM,true);
             {$If Defined(TimeOpenCreateGrids)} WriteLineToDebugFile('MD RRI created   ' + RealToString(Stopwatch.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}
             DoCriterion('RRI_SSIM',PointGrids,AreaGrids);
          end;

          if MDDef.SSIM_LS then begin //created by WbE with Jupyter Notebook
             wmdem.SetPanelText(3,DEMIXModeName + ' 9 LS',true);
             {$If Defined(TimeOpenCreateGrids)} Stopwatch := TStopwatch.StartNew; {$EndIf}
             Success := OpenGridsCreatedByExternalProgram(MDDef.OpenSavedMapsFUVSSIM,'wbt',AreaName,'sed_trans_',PointGrids,AreaGrids);
             {$If Defined(TimeOpenCreateGrids)} WriteLineToDebugFile('WbW LS ' + TStr + ' ' + RealToString(Stopwatch.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}
             if Success then DoCriterion('LS_SSIM',PointGrids,AreaGrids)
             else begin
                ClearDerivedGrids;
                {$IfDef RecordDEMIX} HighLightLineToDebugFile('LS criterion fail for area=' + AreaName); {$EndIf}
             end;
          end;

          if MDDef.SSIM_flow then begin //created by WbE with Jupyter Notebook
             wmdem.SetPanelText(3,DEMIXModeName + ' 10 FLOW',true);
             {$If Defined(TimeOpenCreateGrids)} Stopwatch := TStopwatch.StartNew; {$EndIf}
             //if not OpenGridsCreatedByExternalProgram(MDDef.OpenSavedMapsFUVSSIM,'WBT',AreaName,'FLOW_ACCUM_',PointGrids,AreaGrids) then begin
                WBT_CreateDEMIX_Flow_AccumulationGrids(false,MDDef.OpenSavedMapsFUVSSIM); //will be created with WbT
             //end;
             {$If Defined(TimeOpenCreateGrids)} WriteLineToDebugFile('WbW Flow created   ' + RealToString(Stopwatch.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}
             DoCriterion('ACCUM_SSIM',PointGrids,AreaGrids);
          end;

          if MDDef.SSIM_flow then begin //created by WbE with Jupyter Notebook
             wmdem.SetPanelText(3,DEMIXModeName + ' 10 LOG FLOW',true);
             {$If Defined(TimeOpenCreateGrids)} Stopwatch := TStopwatch.StartNew; {$EndIf}
             //if not OpenGridsCreatedByExternalProgram(MDDef.OpenSavedMapsFUVSSIM,'WBT',AreaName,'FLOW_ACCUM_',PointGrids,AreaGrids) then begin
                WBT_CreateDEMIX_Flow_AccumulationGrids(true,MDDef.OpenSavedMapsFUVSSIM); //will be created with WbT
             //end;
             {$If Defined(TimeOpenCreateGrids)} WriteLineToDebugFile('WbW LOG Flow created   ' + RealToString(Stopwatch.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}
             DoCriterion('LOGFA_SSIM',PointGrids,AreaGrids);
          end;

          if MDDef.SSIM_ProfC then begin //created by WbT
             wmdem.SetPanelText(3,DEMIXModeName + ' 11 PROFC',true);
             {$If Defined(TimeOpenCreateGrids)} Stopwatch := TStopwatch.StartNew; {$EndIf}
             for i := dmxFirstPoint to NumPtDEMs do PointGrids[i] := WBT_ProfileCurvature(MDDef.OpenSavedMapsFUVSSIM,DEMGlb[PointDEMs[i]].GeotiffDEMName);
             for i := dmxFirstArea to NumAreaDEMs do AreaGrids[i] := WBT_ProfileCurvature(MDDef.OpenSavedMapsFUVSSIM,DEMGlb[AreaDEMs[i]].GeotiffDEMName);
             {$If Defined(TimeOpenCreateGrids)} WriteLineToDebugFile('WbT PROFC created   ' + RealToString(Stopwatch.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}
             DoCriterion('PROFC_SSIM',PointGrids,AreaGrids);
          end;

          if MDDef.SSIM_PlanC then begin //created by WbT
             wmdem.SetPanelText(3,DEMIXModeName + ' 12 PLANC',true);
             {$If Defined(TimeOpenCreateGrids)} Stopwatch := TStopwatch.StartNew; {$EndIf}
             for i := dmxFirstPoint to NumPtDEMs do PointGrids[i] := WBT_PlanCurvature(MDDef.OpenSavedMapsFUVSSIM,DEMGlb[PointDEMs[i]].GeotiffDEMName);
             for i := dmxFirstArea to NumAreaDEMs do AreaGrids[i] := WBT_PlanCurvature(MDDef.OpenSavedMapsFUVSSIM,DEMGlb[AreaDEMs[i]].GeotiffDEMName);
             {$If Defined(TimeOpenCreateGrids)} WriteLineToDebugFile('WbT PLANC created   ' + RealToString(Stopwatch.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}
             DoCriterion('PLANC_SSIM',PointGrids,AreaGrids);
          end;

          if MDDef.SSIM_TangC then begin //created by WbT
             wmdem.SetPanelText(3,DEMIXModeName + ' 13 TANGC',true);
             {$If Defined(TimeOpenCreateGrids)} Stopwatch := TStopwatch.StartNew; {$EndIf}
             for i := dmxFirstPoint to NumPtDEMs do PointGrids[i] := WBT_TangentialCurvature(MDDef.OpenSavedMapsFUVSSIM,DEMGlb[PointDEMs[i]].GeotiffDEMName);
             for i := dmxFirstArea to NumAreaDEMs do AreaGrids[i] := WBT_TangentialCurvature(MDDef.OpenSavedMapsFUVSSIM,DEMGlb[AreaDEMs[i]].GeotiffDEMName);
             {$If Defined(TimeOpenCreateGrids)} WriteLineToDebugFile('WbT TANGC created   ' + RealToString(Stopwatch.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}
             DoCriterion('TANGC_SSIM',PointGrids,AreaGrids);
          end;

          if MDDef.SSIM_wet then begin //created by WbT
             wmdem.SetPanelText(3,DEMIXModeName + ' 14 WETIN',true);
             {$If Defined(TimeOpenCreateGrids)} Stopwatch := TStopwatch.StartNew; {$EndIf}
             if OpenGridsCreatedByExternalProgram(MDDef.OpenSavedMapsFUVSSIM,'WBT',AreaName,'wetin_',PointGrids,AreaGrids) then begin
                HowDone := ' opened ';
             end
             else begin
                for i := dmxFirstPoint to NumPtDEMs do PointGrids[i] := WBT_WetnessIndex(MDDef.OpenSavedMapsFUVSSIM,true,DEMGlb[PointDEMs[i]].GeotiffDEMName,WetnessName);
                for i := dmxFirstArea to NumAreaDEMs do AreaGrids[i] := WBT_WetnessIndex(MDDef.OpenSavedMapsFUVSSIM,true,DEMGlb[AreaDEMs[i]].GeotiffDEMName,WetnessName);
                HowDone := ' created ';
             end;
             {$If Defined(TimeOpenCreateGrids)} WriteLineToDebugFile('WbT Wet Index' + HowDone + RealToString(Stopwatch.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}
             DoCriterion('WETIN_SSIM',PointGrids,AreaGrids);
          end;

          if MDDef.SSIM_ConvergeIndex then begin //created by SAGA or WbW with Jupyter Notebook
             wmdem.SetPanelText(3,DEMIXModeName + ' 15 CONIN',true);
             {$If Defined(TimeOpenCreateGrids)} Stopwatch := TStopwatch.StartNew; {$EndIf}

             //Success := OpenGridsCreatedByExternalProgram(MDDef.OpenSavedMapsFUVSSIM,'WBT',AreaName,'CONIN_',PointGrids,AreaGrids);
             HeavyDutyProcessing := true;
             Success := SAGACreateDEMIX_ConIn_Grids(MDDef.OpenSavedMapsFUVSSIM,AreaName,'CONIN_');

             {$If Defined(TimeOpenCreateGrids)} WriteLineToDebugFile('WBT ConIn ' + RealToString(Stopwatch.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}
             if Success then DoCriterion('CONIN_SSIM',PointGrids,AreaGrids)
             else ClearDerivedGrids;
          end;

          if MDDef.SSIM_TPI then begin
             wmdem.SetPanelText(3,DEMIXModeName + ' 16 TPI',true);
             {$If Defined(TimeOpenCreateGrids)} Stopwatch := TStopwatch.StartNew; {$EndIf}
             HowDone := CreateDEMIXTPIGrids(AreaName,MDDef.OpenSavedMapsFUVSSIM,true);
             {$If Defined(TimeOpenCreateGrids)} WriteLineToDebugFile('TPI' + HowDone + RealToString(Stopwatch.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}
             DoCriterion('TPI_SSIM',PointGrids,AreaGrids);
          end;

          if MDDef.SSIM_elev then begin
             //Elevation is done last, so we no longer need elevation to create derived grids, and can normalize it
             wmdem.SetPanelText(3,DEMIXModeName + ' 17 ELEV',true);
             DoCriterion('ELEV_SSIM',PointDEMs,AreaDEMs);
          end;

          if MDDef.DoSSIM then begin
             if (ResultsSSIM.Count > 1) then begin
                ResultsSSIM.SaveToFile(SavefNameSSIM);
                {$IfDef RecordDEMIXFull} WriteLineToDebugFile('Save to ' + SaveFNameSSIM); {$EndIf}
             end
             else begin
                {$IfDef RecordDEMIXFull} HighlightLineToDebugFile('No results for ' + SaveFNameSSIM); {$EndIf}
             end;
             ResultsSSIM.Free;
          end;

          if MDDef.DoFUV then begin
             if (ResultsFUV.Count > 1) then begin
                ResultsFUV.SaveToFile(SavefNameFUV);
                {$IfDef RecordDEMIXFull} WriteLineToDebugFile('Save to ' + SaveFNameFUV); {$EndIf}
             end
             else begin
                {$IfDef RecordDEMIXFull} HighlightLineToDebugFile('No results for ' + SaveFNameFUV); {$EndIf}
             end;
             ResultsFUV.Free;
          end;

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



procedure AreaSSIMandFUVComputations(Overwrite,AreasInsteadOfTiles : boolean; Areas : tStringList = Nil);
var
   Area  : shortstring;
   i     : integer;
   LocalAreas : boolean;
begin
   if not (MDDef.DoFUV or MDDef.DoSSIM) then begin
      if not AnswerIsYes('Neither FUV/SSIM selected; proceed anyway to create and save grids') then exit;
   end;

   try
      {$IfDef RecordDEMIXFull} WriteLineToDebugFile('AreaSSIMandFUVComputations in'); {$EndIf}
      GetDEMIXPaths(true);
      {$IfDef RecordDEMIXFull} WriteLineToDebugFile('Finished GetDEMIXPaths'); {$EndIf}
      LocalAreas := (Areas = Nil);
      if LocalAreas then Areas := DEMIX_AreasWanted(not MDDef.DEMIX_all_areas);
      {$IfDef RecordDEMIX} WriteLineToDebugFile('AreaSSIMandFUVComputations in, selected areas=' + IntToStr(Areas.Count)); {$EndIf}
      if MDDef.DoSSIM then SafeMakeDir(SSIMresultsDir);
      if MDDef.DoFUV then SafeMakeDir(FUVresultsDir);
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
      if LocalAreas then Areas.Free;
      EndDEMIXProcessing(0,true);
   end;
   {$IfDef RecordDEMIX} HighLightLineToDebugFile('AreaSSIMandFUVComputations out'); {$EndIf}
end {procedure AreaSSIMandFUVComputations};


