{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}
{                                   }
{   include file for demstat        }
{___________________________________}


procedure SSIMcheck(DoThinning : boolean);
const
   Windows : array[1..5] of integer = (7,9,11,13,15);
var
   DEM1,DEM2,thin,i,db : integer;
   Grids : array[1..5] of integer;
   bb : sfBoundBox;
   Mean,Std : float32;
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

   //fName := '';

   db := DEMIXtileFill(DEM1,DEMGlb[DEM1].DEMBoundBoxGeo);

   GISdb[db].ApplyGISFilter('GRID_FULL>' + IntToStr(DEMIX_Tile_Full));

    while not GISdb[db].MyData.eof do begin
       bb := GISdb[db].MyData.GetRecordBoundingBox;

       gl1 := DEMglb[DEM1].sfBoundBox2tGridLimits(bb);
       gl2 := DEMglb[DEM2].sfBoundBox2tGridLimits(bb);

       aLine := GISdb[db].MyData.GetFieldByNameAsString('NAME') + ',' + RealToString(0.5*(bb.YMin + bb.YMax),-12,-4)  + ',' + RealToString(0.5*(bb.xMin + bb.xMax),-12,-4) +
          RealToString(GISdb[db].MyData.GetFieldByNameAsFloat('GRID_FULL'),-8,-2);

       if DoThinning then begin
          ComputeSSIM(DEM1,DEM2,gl1,gl2,SSIM,Luminance,Contrast,Structure);
          aLine := aline + ',' + RealToString(SSIM,-12,-8);
       end;

       for i := 1 to 5 do begin
          gl1 := DEMglb[Grids[i]].sfBoundBox2tGridLimits(bb);
          DEMglb[Grids[i]].ElevationStatistics(gl1,Mean,Std);
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
      if NormDB.FiltRecsInDB = 1 then begin
         Min := NormDB.GetFieldByNameAsFloat('MIN');
         Max := NormDB.GetFieldByNameAsFloat('MAX');
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
end;


function ComputeSSIM(DEM1,DEM2 : integer; gl1,gl2 : tGridLimits; var SSIM,Luminance,Contrast,Structure : float64) : boolean;
//grid should have been normalized before calling
const
  MaxIntensity = 1;
  c1 = 0.01;    //C1 := sqr(0.01 * MaxIntensity);
  c2 = 0.03;    //C2 := sqr(0.03 * MaxIntensity);
  c3 = 0.015;   //C3 := c2 / 2;
var
  r,covar,Mean1,Mean2,StdDev1,StdDev2 : float64;
begin
   {$IfDef RecordSSIMFull} WriteLineToDebugFile('ComputeSSIM '  + DEMGlb[DEM1].AreaName + ',' + DEMGlb[DEM2].AreaName); {$EndIf}
   Result := CovariancesFromTwoGrids(gl1,DEM1,DEM2,r,covar,Mean1,Mean2,StdDev1,StdDev2,false);  //do not record failures, there could be a huge number if grid has voids
   if Result then begin
      {$IfDef RecordSSIMFull} WriteLineToDebugFile('Covariance done, covar=' + RealToString(Covar,-12,2)); {$EndIf}
      Luminance := (2 * Mean1 * Mean2 + C1*SSIM_fudge) / (Sqr(Mean1) + Sqr(Mean2) + C1*SSIM_fudge);
      Contrast := (2 * stddev1 * stddev2 + C2*SSIM_fudge) / (sqr(StdDev1) + sqr(StdDev2) + C2*SSIM_fudge);
      Structure := (Covar + c3*SSIM_fudge) / (stddev1 * stddev2 + C3*SSIM_fudge);
      SSIM := Luminance * Contrast * Structure;
      {$IfDef RecordSSIMFull} WriteLineToDebugFile('ComputeSSIM '  + DEMGlb[DEM1].AreaName + ',' +
          DEMGlb[DEM2].AreaName + ',' + RealToString(ssim,-12,8) + ',' + RealToString(luminance,-12,8) + ',' +
          RealToString(contrast,-12,8) + ',' + RealToString(structure,-12,8));
      {$EndIf}
   end;
end;



function MakeSSIMMap(OpenMap,AlreadyNormalized : boolean; DEM1,DEM2,NumberOfGrids,WindowSize : integer; ThinFactor : integer = 1; AreaName : shortstring = '') : integer;
//assumes an elevation grid for the normalization
//this normalizes the two grids, but then reloads them
{$IfDef RepeatProblematicComputations}
   label
      Restart;
{$EndIf}
const
   Names : array[1..4] of shortstring = ('SSIM','Luminance','Contrast','Structure');
var
  r,covar,Mean1,Mean2,StdDev1,StdDev2 : float64;
  xoffset,yoffset,x,y,xl,yl,i : integer;
  gl1,gl2 : tGridLimits;
  ng : array[1..4] of integer;
  nv : array[1..4] of float64;
  GridLimits : tGridLimits;
  TStr : shortstring;
begin
  if ValidDEM(DEM1) and ValidDEM(DEM2) then begin
     {$IfDef RecordSSIMFull} WriteLineToDebugFile('ComputeSSIM in grids=' + IntToStr(NumberOfGrids) + ' '  + AreaName + ' DEM1=' + DEMglb[DEM1].AreaName + ' DEM2=' + DEMglb[DEM2].AreaName); {$EndIf}
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
         StartProgress('Make SSIM Map ' + AreaName);
         x := GridLimits.xgridLow;
         while x <= GridLimits.xgridHigh do begin
            gl1.XGridLow := ThinFactor * x - WindowSize;
            gl1.XGridHigh := ThinFactor * x + WindowSize;
            gl2.XGridLow := ThinFactor * x + xoffset - WindowSize;
            gl2.XGridHigh := ThinFactor * x + xoffset + WindowSize;

            if (x mod 25 = 0) then begin
               UpdateProgressBar(x/DEMglb[DEM1].DEMHeader.NumCol);
            end;
            y := GridLimits.ygridLow;
            while y <= GridLimits.ygridHigh do begin
                gl1.YGridLow := ThinFactor * y - WindowSize;
                gl1.YGridHigh := ThinFactor * y + WindowSize;
                gl2.YGridLow := ThinFactor * y + yoffset - WindowSize;
                gl2.YGridHigh := ThinFactor * y + yoffset + WindowSize;
                if ComputeSSIM(DEM1,DEM2,gl1,gl2,nv[1],nv[2],nv[3],nv[4]) then begin
                   for i := 1 to NumberOfGrids do begin
                      DEMGlb[ng[i]].SetGridElevation(x,y,nv[i]);
                   end;
                end;
                inc(y,1);
            end;
            inc(x,1);
         end;
         for i := 1 to NumberOfGrids do begin
            DEMglb[ng[i]].CheckMaxMinElev;
            if OpenMap then DEMglb[ng[i]].SetUpMap(ng[i],true,mtElevSpectrum);
         end;
         if not AlreadyNormalized then begin
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
         {$IfDef RecordDEMIX} WriteLineToDebugFile(DEMGlb[DEM1].AreaName + '  ' + DEMGlb[DEM1].KeyDEMParams(true)); {$EndIf}
         {$IfDef RecordDEMIX} WriteLineToDebugFile(DEMGlb[DEM2].AreaName + '  ' + DEMGlb[DEM2].KeyDEMParams(true)); {$EndIf}
         MessageToContinue(TStr);
      end;
  end
  else begin
     Result := 0;
     TStr := 'function MakeSSIMMap fails, DEM1=' +  IntToStr(DEM1) + '  DEM2=' + IntToStr(DEM2);
     {$IfDef RecordSSIMFull} WriteLineToDebugFile(Tstr); {$EndIf}
  end;
end {function MakeSSIMMap};



   procedure DoSSIMandFUVForAnArea(AreaName : shortstring; Overwrite : boolean);
   var
      DEMIXtileDB,NumRef,i,j : integer;
      ResultsSSIM,ResultsFUV : tStringList;
      SavefNameSSIM,SavefNameFUV,
      fName,GridName,BreachName,WetnessName : PathStr;
      aLine,TileName : shortstring;
      bb : sfBoundBox;
      TileFull : float32;
      MissingFiles : boolean;

      function ProcessDEMTileSSIM(Ref,Test : integer; bb : sfBoundBox) : float32;
      var
         SSIM,Luminance,Contrast,Structure : float64;
         RefGridLimits,TestGridLimits : tGridLimits;
      begin
          if ValidDEM(Ref) and ValidDEM(Test) then begin
             RefGridLimits := DEMglb[Ref].sfBoundBox2tGridLimits(bb);
             TestGridLimits := DEMglb[Test].sfBoundBox2tGridLimits(bb);
             ComputeSSIM(Ref,Test,RefGridLimits,TestGridLimits,SSIM,Luminance,Contrast,Structure);
             Result := 1-SSIM;
             if (Result > 1) then Result := 1;
          end
          else begin
             Result := -999;
          end;
      end;

       function DoCovarianceForPair(RefGridLimits : tGridLimits; Grid1,Grid2 : integer) : float64;
       var
          Mean1,Mean2,StdDev1,StdDev2,r,covar : float64 ;
       begin
          if ValidDEM(Grid1) and ValidDEM(Grid2) then begin
             {$IfDef TrackSWcorner} DEMGlb[Grid1].WriteToDebugSWCornerForComputations('AreaSSIMComputations'); DEMGlb[Grid2].WriteToDebugSWCornerForComputations('AreaSSIMComputations'); {$EndIf}
             if CovariancesFromTwoGrids(RefGridLimits,Grid1,Grid2,r,covar,Mean1,Mean2,StdDev1,StdDev2) then begin
                Result := 1-sqr(r);
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
             {$If Defined(RecordDEMIXfull)} WriteLineToDebugFile('Invalid grid, DoCovarianceForPair Fail for tile ' + TileName + ' ' + DEMglb[Grid1].AreaName + ' ' + DEMglb[Grid2].AreaName); {$EndIf}
          end;
       end;


      procedure DoCriterion(Criterion : ANSIString; usingPointGrids,usingAreaGrids : tDEM_int_array; ClearDerived : boolean = true);
      var
         i,ThisRefDEM,ThisTestDEM,UsingRef : integer;
         Criterion2 : shortstring;
         RefGridLimits,TestGridLimits : tGridLimits;
         What,TStr : shortstring;
         gl1 : tGridLimits;
         Mean,Std : float32;
         MissingGrids : boolean;

         function TileSetUp : shortstring;
         begin
             bb := GISdb[DEMIXtileDB].MyData.GetRecordBoundingBox;
             TileName := GISdb[DEMIXtileDB].MyData.GetFieldByNameAsString('NAME');
             TileFull := GISdb[DEMIXtileDB].MyData.GetFieldByNameAsFloat('GRID_FULL');
             Result := TileName  + ',' + AreaName + ',' + RealToString(0.5*(bb.YMin+bb.YMax),-12,4) + ',' + RealToString(0.5*(bb.xMin+bb.xMax),-12,4) + ',' + RealToString(TileFull,-8,-2) + ',';
         end;

      begin
          {$IfDef RecordDEMIXFull} WriteLineToDebugFile('Criterion=' + Criterion); {$EndIf}
          ZeroPointAndAreaGrids(PtSSIMGrids, AreaSSIMGrids);
          MissingGrids := false;
          for i := 0 to NumPtDEMs do if not ValidDEM(usingPointGrids[i]) then MissingGrids := true;
          for i := 0 to NumAreaDEMs do if not ValidDEM(usingAreaGrids[i]) then MissingGrids := true;
          if MissingGrids then begin
             HighlightLineToDebugFile(Criterion + ' has missing data grids for ' + AreaName);
             exit;
          end;
          if MDDef.DoFUV and (Overwrite or (not FileExists(SaveFNameFUV))) then begin
             //do correlation coefficients first before normalizing grids for SSIM
             {$If Defined(RecordFUVstepsFull)} Stopwatch := TStopwatch.StartNew; {$EndIf}
             GISdb[DEMIXtileDB].MyData.First;
             while not GISdb[DEMIXtileDB].MyData.eof do begin
                Criterion2 := StringReplace(Criterion,'SSIM','FUV',[rfReplaceAll, rfIgnoreCase]);
                aLine := TileSetup + Criterion2;

                RefGridLimits := DEMglb[usingAreaGrids[0]].sfBoundBox2tGridLimits(bb);
                for i := 1 to NumAreaDEMs do begin
                   aLine := aline + ',' + RealToString(DoCovarianceForPair(RefGridLimits,usingAreaGrids[0],usingAreaGrids[i]),-12,8);
                end;

                for i := 1 to NumPtDEMs do begin
                   UsingRef := RefGridForThisPointGrid(UsingPointGrids,i);
                   RefGridLimits := DEMglb[UsingRef].sfBoundBox2tGridLimits(bb);
                   aLine := aline + ',' + RealToString(DoCovarianceForPair(RefGridLimits,UsingRef,usingPointGrids[i]),-12,8);
                end;
                ResultsFUV.Add(aline);
                GISdb[DEMIXtileDB].MyData.Next;
             end;
             {$IfDef RecordDEMIXFull} WriteLineToDebugFile('FUV done for Criterion=' + Criterion); {$EndIf}
             {$If Defined(RecordFUVstepsFull)} Elapsed := Stopwatch.Elapsed; WriteLineToDebugFile('FUV ' + Criterion + '   ' + RealToString(Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}
          end;

          if MDDef.DoSSIM and (Overwrite or (not FileExists(SaveFNameSSIM))) and (abs(DEMGlb[PointDEMs[0]].ComputeSWCornerY) < 50) then begin
             //normalize grids for SSIM
             {$If Defined(RecordFUVsteps)} Stopwatch := TStopwatch.StartNew; {$EndIf}
             What := BeforeSpecifiedCharacterAnsi(Criterion,'_');
             for i := dmxFirstArea to NumAreaDEMs do NormalizeDEMforSSIM(AreaGrids[i],What);
             for i := dmxFirstPoint to NumPtDEMs do NormalizeDEMforSSIM(PointGrids[i],What);
             {$IfDef RecordDEMIXFull} WriteLineToDebugFile('Grids normalized for Criterion=' + Criterion); {$EndIf}
             wmdem.SetPanelText(2, TileName + ' SSIM grids',true);

             for i := 1 to NumAreaDEMs do begin
                AreaSSIMGrids[i] := MakeSSIMMap(false, true,usingAreaGrids[0],usingAreaGrids[i],1,11);
             end;

             for i := 1 to NumPtDEMs do begin
                ThisRefDEM := RefGridForThisPointGrid(usingPointGrids,PointDEMs[i]);
                ThisTestDEM := usingPointGrids[i];
                PtSSIMGrids[i] := MakeSSIMMap(false,true,ThisRefDEM,ThisTestDEM,1,11);
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
                   if ValidDEM(AreaSSIMGrids[i]) then begin
                      gl1 := DEMglb[AreaSSIMGrids[i]].sfBoundBox2tGridLimits(bb);
                      DEMglb[AreaSSIMGrids[i]].ElevationStatistics(gl1,Mean,Std);
                      aLine := aline + ',' + RealToString(1-Mean,-12,8);
                   end
                   else begin
                      {$IfDef RecordDEMIX} WriteLineToDebugFile('Fail for tile ' + TileName + ' Area DEM=' + DEMglb[usingAreaGrids[i]].AreaName); {$EndIf}
                      aLine := aline + ',' + '-999';
                   end;
                end;
                for i := 1 to NumPtDEMs do begin
                   if ValidDEM(PtSSIMGrids[i]) then begin
                      gl1 := DEMglb[PtSSIMGrids[i]].sfBoundBox2tGridLimits(bb);
                      DEMglb[PtSSIMGrids[i]].ElevationStatistics(gl1,Mean,Std);
                      aLine := aline + ',' + RealToString(1-Mean,-12,8);
                   end
                   else begin
                      {$IfDef RecordDEMIX} WriteLineToDebugFile('SSIM Fail (invalid PtSSIMGrids) ' + TileName + ' Point DEM=' + DEMglb[usingPointGrids[i]].AreaName); {$EndIf}
                      aLine := aline + ',' + '-999';
                   end;
                end;
                ResultsSSIM.Add(aline);
                GISdb[DEMIXtileDB].MyData.Next;
                {$If Defined(RecordFUVstepsFull)} WriteLineToDebugFile('SSIM ' + What + ' Do criterion  ' + RealToString(Stopwatch.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}
             end;
             for i := 1 to NumPtDEMs do CloseSingleDEM(PtSSIMGrids[i]);
             for i := 1 to NumAreaDEMs do CloseSingleDEM(AreaSSIMGrids[i]);
          end;
          if ClearDerived then ClearDerivedGrids;
          wmdem.SetPanelText(2,'',true);
      end;

   var
      Success : boolean;
      TStr : shortstring;
      fName2 : PathStr;
   begin {procedure DoSSIMandFUVForAnArea}
       SaveFNameSSIM := SSIMresultsDir + AreaName + '_ssim_results.csv';
       SaveFNameFUV := FUVresultsDir + AreaName + '_fuv_results.csv';
       if Overwrite or (MDDef.DoSSIM and (not FileExists(SaveFNameSSIM))) or (MDDef.DoFUV and (not FileExists(SaveFNameFUV))) then begin
          {$IfDef RecordDEMIXFull} HighLightLineToDebugFile('AreaSSIMandFUVComputations area=' + AreaName); {$EndIf}
          wmdem.SetPanelText(3, 'Load DEMs',true);
          ShowSatProgress :=  false;
          if OpenBothPixelIsDEMs(AreaName,'',DEMIX_Ref_1sec,DEMIX_test_dems,MDDef.OpenMapsFUVSSIM) then begin
             ZeroPointAndAreaGrids(PointGrids,AreaGrids);
             {$If Defined(TrackPixelIs) or Defined(RecordDEMIXFull)} ShowDEMIXGrids('DEMs opened',PointDEMs,AreaDEMs); {$EndIf}

             ResultsFUV := tStringList.Create;
             ResultsSSIM := tStringList.Create;
             aline := 'DEMIX_TILE,AREA,LAT,LONG,GRID_FULL,CRITERION';
             for i := 1 to NumAreaDEMs do aline := aLine + ',' + ALOS_centroid_names[i];
             for i := 1 to NumPtDEMs do aline := aLine + ',' + SRTM_centroid_names[i];
             ResultsFUV.Add(aLine);
             ResultsSSIM.Add(aLine);

             DEMIXtileDB := DEMIXtileFill(PointDEMs[0],DEMGlb[PointDEMs[0]].DEMBoundBoxGeo,false);
             GISdb[DEMIXtileDB].ApplyGISFilter('GRID_FULL>' + IntToStr(DEMIX_Tile_Full));

             if MDDef.SSIM_hill then begin
                wmdem.SetPanelText(3, 'Hillshade',true);
                {$If Defined(RecordFUVsteps)} Stopwatch := TStopwatch.StartNew; {$EndIf}
                CreateDEMIXhillshadeGrids(MDDef.OpenMapsFUVSSIM);
            	 {$If Defined(RecordFUVsteps)} WriteLineToDebugFile('Hillshade created   ' + RealToString(Stopwatch.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}
                DoCriterion('HILL_SSIM',PointGrids,AreaGrids);
             end;

             if MDDef.SSIM_TPI then begin
                wmdem.SetPanelText(3, 'TPI',true);
                {$If Defined(RecordFUVsteps)} Stopwatch := TStopwatch.StartNew; {$EndIf}
                for i := dmxFirstPoint to NumPtDEMs do PointGrids[i] := BoxCarDetrendDEM(MDDef.OpenMapsFUVSSIM,PointDEMs[i],DEMGlb[PointDEMs[i]].FullDEMGridLimits,3);
                for i := dmxFirstArea to NumAreaDEMs do AreaGrids[i] := BoxCarDetrendDEM(MDDef.OpenMapsFUVSSIM,AreaDEMs[i],DEMGlb[AreaDEMs[i]].FullDEMGridLimits,3);
            	 {$If Defined(RecordFUVsteps)} WriteLineToDebugFile('TPI created ' + RealToString(Stopwatch.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}
                DoCriterion('TPI_SSIM',PointGrids,AreaGrids);
             end;


             if MDDef.SSIM_Slope or MDDef.SSIM_ruff then begin
                wmdem.SetPanelText(3, 'Slope/ruff',true);
                {$If Defined(RecordFUVsteps)} Stopwatch := TStopwatch.StartNew; {$EndIf}
                CreateDEMIXSlopeRoughnessGrids(MDDef.OpenMapsFUVSSIM);
            	 {$If Defined(RecordFUVsteps)} WriteLineToDebugFile('MD Slope/ruff created   ' + RealToString(Stopwatch.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}
                DoCriterion('SLOPE_SSIM',PointGrids2,AreaGrids2,false);
                DoCriterion('RUFF_SSIM',PointGrids,AreaGrids);
             end;

             if MDDef.SSIM_Openness then begin
                wmdem.SetPanelText(3, 'Openness',true);
                {$If Defined(RecordFUVsteps)} Stopwatch := TStopwatch.StartNew; {$EndIf}
                CreateDEMIXOpennessGrids(MDDef.OpenMapsFUVSSIM);
            	 {$If Defined(RecordFUVsteps)} WriteLineToDebugFile('MD Openness created   ' + RealToString(Stopwatch.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}
                DoCriterion('OPEND_SSIM',PointGrids2,AreaGrids2,false);
                DoCriterion('OPENU_SSIM',PointGrids,AreaGrids);
             end;

             if MDDef.SSIM_RRI then begin
                wmdem.SetPanelText(3, 'RRI',true);
                {$If Defined(RecordFUVsteps)} Stopwatch := TStopwatch.StartNew; {$EndIf}
                for i := dmxFirstPoint to NumPtDEMs do PointGrids[i] := MakeTRIGrid(PointDEMs[i],nmRRI,MDDef.OpenMapsFUVSSIM);
                for i := dmxFirstArea to NumAreaDEMs do AreaGrids[i] := MakeTRIGrid(AreaDEMs[i],nmRRI,MDDef.OpenMapsFUVSSIM);
            	 {$If Defined(RecordFUVsteps)} WriteLineToDebugFile('MD RRI created   ' + RealToString(Stopwatch.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}
                DoCriterion('RRI_SSIM',PointGrids,AreaGrids);
             end;

             if MDDef.SSIM_ConvergeIndex then begin
                wmdem.SetPanelText(3, 'Convergence Index',true);
                {$If Defined(RecordFUVsteps)} Stopwatch := TStopwatch.StartNew; {$EndIf}
                //SAGACreateDEMIX_ConIn_Grids(MDDef.OpenMapsFUVSSIM);

                Success := OpenGridsCreatedByExternalProgram('saga',AreaName,'CONIN_');
                if not Success then begin
                   Success := SAGACreateDEMIX_ConIn_Grids(MDDef.OpenMapsFUVSSIM,AreaName,'CONIN_');
                end;
                {$If Defined(RecordFUVsteps)} WriteLineToDebugFile('SAGA ConIn ' + RealToString(Stopwatch.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}
                DoCriterion('CONIN',PointGrids,AreaGrids);
             end;

             if MDDef.SSIM_LS then begin
                wmdem.SetPanelText(3, 'LS',true);
                {$If Defined(RecordFUVsteps)} Stopwatch := TStopwatch.StartNew; {$EndIf}
                Success := OpenGridsCreatedByExternalProgram('wbt',AreaName,'sed_trans_');
                {$If Defined(RecordFUVsteps)} WriteLineToDebugFile('WbW LS ' + TStr + ' ' + RealToString(Stopwatch.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}
                if Success then DoCriterion('LS_SSIM',PointGrids,AreaGrids);
             end;

             if MDDef.SSIM_ROTOR then begin
                wmdem.SetPanelText(3,'ROTOR',true);
                {$If Defined(RecordFUVsteps)} Stopwatch := TStopwatch.StartNew; {$EndIf}
                Success := OpenGridsCreatedByExternalProgram('WBT',AreaName,'rotor_log_');
            	 {$If Defined(RecordFUVsteps)} WriteLineToDebugFile('WbW Rotor created ' + RealToString(Stopwatch.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}
                if Success then begin
                   DoCriterion('ROTOR_SSIM',PointGrids,AreaGrids);
                end;
             end;

             if MDDef.SSIM_flow then begin
                wmdem.SetPanelText(3, 'FLOW',true);
                {$If Defined(RecordFUVsteps)} Stopwatch := TStopwatch.StartNew; {$EndIf}
                if not OpenGridsCreatedByExternalProgram('WBT',AreaName,'FLOW_ACCUM_') then begin
                   CreateDEMIX_Flow_AccumulationGrids(MDDef.OpenMapsFUVSSIM);
                end;
             	 {$If Defined(RecordFUVsteps)} WriteLineToDebugFile('WbW Flow created   ' + RealToString(Stopwatch.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}

                DoCriterion('ACCUM_SSIM',PointGrids,AreaGrids);
             end;


             if MDDef.SSIM_HAND then begin
                wmdem.SetPanelText(3, 'HAND',true);

                {$If Defined(RecordFUVsteps)} Stopwatch := TStopwatch.StartNew; {$EndIf}
                if not OpenGridsCreatedByExternalProgram('WBT',AreaName,'HAND_') then begin
                    CreateDEMIX_HANDGrids(MDDef.OpenMapsFUVSSIM);
                end;
             	 {$If Defined(RecordFUVsteps)} WriteLineToDebugFile('WbT HAND created   ' + RealToString(Stopwatch.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}

                DoCriterion('HAND_SSIM',PointGrids,AreaGrids);
             end;


             if MDDef.SSIM_ProfC then begin
                wmdem.SetPanelText(3, 'PROFC',true);
                {$If Defined(RecordFUVsteps)} Stopwatch := TStopwatch.StartNew; {$EndIf}
                for i := dmxFirstPoint to NumPtDEMs do PointGrids[i] := WBT_ProfileCurvature(MDDef.OpenMapsFUVSSIM,DEMGlb[PointDEMs[i]].GeotiffDEMName);
                for i := dmxFirstArea to NumAreaDEMs do AreaGrids[i] := WBT_ProfileCurvature(MDDef.OpenMapsFUVSSIM,DEMGlb[AreaDEMs[i]].GeotiffDEMName);
            	 {$If Defined(RecordFUVsteps)} WriteLineToDebugFile('WbT PROFC created   ' + RealToString(Stopwatch.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}
                DoCriterion('PROFC_SSIM',PointGrids,AreaGrids);
             end;

             if MDDef.SSIM_PlanC then begin
                wmdem.SetPanelText(3, 'PLANC',true);
                {$If Defined(RecordFUVsteps)} Stopwatch := TStopwatch.StartNew; {$EndIf}
                for i := dmxFirstPoint to NumPtDEMs do PointGrids[i] := WBT_PlanCurvature(MDDef.OpenMapsFUVSSIM,DEMGlb[PointDEMs[i]].GeotiffDEMName);
                for i := dmxFirstArea to NumAreaDEMs do AreaGrids[i] := WBT_PlanCurvature(MDDef.OpenMapsFUVSSIM,DEMGlb[AreaDEMs[i]].GeotiffDEMName);
            	 {$If Defined(RecordFUVsteps)} WriteLineToDebugFile('WbT PLANC created   ' + RealToString(Stopwatch.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}
                DoCriterion('PLANC_SSIM',PointGrids,AreaGrids);
             end;

             if MDDef.SSIM_TangC then begin
                wmdem.SetPanelText(3, 'TANGC',true);
                {$If Defined(RecordFUVsteps)} Stopwatch := TStopwatch.StartNew; {$EndIf}
                for i := dmxFirstPoint to NumPtDEMs do PointGrids[i] := WBT_TangentialCurvature(MDDef.OpenMapsFUVSSIM,DEMGlb[PointDEMs[i]].GeotiffDEMName);
                for i := dmxFirstArea to NumAreaDEMs do AreaGrids[i] := WBT_TangentialCurvature(MDDef.OpenMapsFUVSSIM,DEMGlb[AreaDEMs[i]].GeotiffDEMName);
            	 {$If Defined(RecordFUVsteps)} WriteLineToDebugFile('WbT TANGC created   ' + RealToString(Stopwatch.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}
                DoCriterion('TANGC_SSIM',PointGrids,AreaGrids);
             end;

             if MDDef.SSIM_wet then begin
                wmdem.SetPanelText(3, 'WETIN',true);
                {$If Defined(RecordFUVsteps)} Stopwatch := TStopwatch.StartNew; {$EndIf}
                for i := dmxFirstPoint to NumPtDEMs do PointGrids[i] := WBT_WetnessIndex(MDDef.OpenMapsFUVSSIM,true,DEMGlb[PointDEMs[i]].GeotiffDEMName,WetnessName);
                for i := dmxFirstArea to NumAreaDEMs do AreaGrids[i] := WBT_WetnessIndex(MDDef.OpenMapsFUVSSIM,true,DEMGlb[AreaDEMs[i]].GeotiffDEMName,WetnessName);
             	 {$If Defined(RecordFUVsteps)} Elapsed := Stopwatch.Elapsed; WriteLineToDebugFile('WbT Wet Index created   ' + RealToString(Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}
                DoCriterion('WETIN_SSIM',PointGrids,AreaGrids);
             end;

             if MDDef.SSIM_elev then begin
                //Elevation is done last, so we no longer need elevation to create derived grids, and can normalize it
                wmdem.SetPanelText(3, 'ELEV',true);
                DoCriterion('ELEV_SSIM',PointDEMs,AreaDEMs);
             end;

             if MDDef.DoSSIM then begin
                if (ResultsSSIM.Count > 1) then begin
                   ResultsSSIM.SaveToFile(SavefNameSSIM);
                   {$IfDef RecordDEMIX} WriteLineToDebugFile('Save to ' + SaveFNameSSIM); {$EndIf}
                end
                else begin
                   {$IfDef RecordDEMIX} HighlightLineToDebugFile('No results for ' + SaveFNameSSIM); {$EndIf}
                end;
             end;
             ResultsSSIM.Free;

             if MDDef.DoFUV then begin
                if (ResultsFUV.Count > 1) then begin
                   ResultsFUV.SaveToFile(SavefNameFUV);
                   {$IfDef RecordDEMIX} WriteLineToDebugFile('Save to ' + SaveFNameFUV); {$EndIf}
                end
                else begin
                   {$IfDef RecordDEMIX} HighlightLineToDebugFile('No results for ' + SaveFNameFUV); {$EndIf}
                end;
             end;
             ResultsFUV.Free;

             if (not MDDef.OpenMapsFUVSSIM) then CloseAllDEMs;
             CloseAndNilNumberedDB(DEMIXtileDB);
             CleanUpTempDirectory;  //lot of files created by SAGA and WBT
          end
          else begin
             {$IfDef RecordDEMIXFull} HighLightLineToDebugFile('Missing DEM files for area ' + AreaName); {$EndIf}
          end;
       end;
   end {procedure DoSSIMandFUVForAnArea};



procedure AreaSSIMandFUVComputations(Overwrite : boolean; Areas : tStringList = Nil);
var
   Area  : shortstring;
   i     : integer;
   LocalAreas : boolean;
begin {procedure AreaSSIMComputations}
   if MDDef.DoFUV or MDDef.DoSSIM then begin
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
         if MDDef.ProcessLoopsForward then begin
            for i := 0 to pred(Areas.Count) do begin
               Area := Areas.Strings[i];
               wmdem.SetPanelText(1, IntToStr(succ(i)) + '/' + IntToStr(Areas.Count) + '  ' + Area,true);
               DoSSIMandFUVForAnArea(Area,Overwrite);
            end;
         end
         else begin
            for i := pred(Areas.Count) downto 0 do begin
               Area := Areas.Strings[i];
               wmdem.SetPanelText(1, IntToStr(succ(i)) + '/' + IntToStr(Areas.Count) + '  ' + Area,true);
               DoSSIMandFUVForAnArea(Area,Overwrite);
            end;
         end;
      finally
         if LocalAreas then Areas.Free;
         EndDEMIXProcessing;
      end;
      {$IfDef RecordDEMIX} HighLightLineToDebugFile('AreaSSIMandFUVComputations out'); {$EndIf}
   end
   else begin
      MessageToContinue('Neither FUV/SSIM selected');
   end;
end {procedure AreaSSIMComputations};



