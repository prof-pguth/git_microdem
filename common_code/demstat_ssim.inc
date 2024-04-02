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
      Results.Add('DEMIX_TILE,LAT,LONG,TILE_AVG,POINTS_1,POINTS_2,POINTS_4,POINTS_8,POINTS_16');
   end
   else begin
      for i := 1 to 5 do begin
         AreaName := 'SSIM_window_' + IntToStr(Windows[i]) + 'x' + IntToStr(Windows[i]);
         Grids[i] := MakeSSIMMap(true,false,DEM1,DEM2,1,Windows[i],Thin,AreaName);
      end;
      Results.Add('DEMIX_TILE,LAT,LONG,W_7x7,W_9x9,W_11x11,W_13x13,W_15x15');
   end;

   //fName := '';

   db := DEMIXtileFill(DEM1,DEMGlb[DEM1].DEMBoundBoxGeo);

   GISdb[db].ApplyGISFilter('GRID_FULL>' + IntToStr(MDDef.DEMIX_Full));

    while not GISdb[db].MyData.eof do begin
       bb := GISdb[db].MyData.GetRecordBoundingBox;

       gl1 := DEMglb[DEM1].sfBoundBox2tGridLimits(bb);
       gl2 := DEMglb[DEM2].sfBoundBox2tGridLimits(bb);

       aLine := GISdb[db].MyData.GetFieldByNameAsString('NAME') + ',' + RealToString(0.5*(bb.YMin + bb.YMax),-12,-4)  + ',' + RealToString(0.5*(bb.xMin + bb.xMax),-12,-4);

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
     {$IfDef RecordSSIMFull} WriteLineToDebugFile('ComputeSSIM in, grids=' + IntToStr(NumberOfGrids) + '  '  + AreaName); {$EndIf}
     {$IfDef RecordSSIMFull} WriteLineToDebugFile('  DEM1=' + DEMglb[DEM1].AreaName + '  DEM2=' + DEMglb[DEM2].AreaName); {$EndIf}
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
            gl2.XGridLow := ThinFactor * x + yoffset - WindowSize;
            gl2.XGridHigh := ThinFactor * x + yoffset + WindowSize;

            if (x mod 25 = 0) then UpdateProgressBar(x/DEMglb[DEM1].DEMHeader.NumCol);
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
      Results : tStringList;
      fName,SavefName,GridName : PathStr;
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
          {$IfDef TrackSWcorner} DEMGlb[Grid1].WriteToDebugSWCornerForComputations('AreaSSIMComputations'); DEMGlb[Grid2].WriteToDebugSWCornerForComputations('AreaSSIMComputations'); {$EndIf}
          if ValidDEM(Grid1) and ValidDEM(Grid2) then begin
             if CovariancesFromTwoGrids(RefGridLimits,Grid1,Grid2, r,covar,Mean1,Mean2,StdDev1,StdDev2) then begin
                Result := 1-sqr(r);
             end
             else begin
                {$IfDef RecordDEMIX} WriteLineToDebugFile('DoCovarianceForPair computation Fail for tile ' + TileName + ' TileFill=' + RealToString(TileFull,-8,-2) + '%'); {$EndIf}
                Result := -999;
                {$IfDef RepeatProblematicComputations}
                   MessageToContinue('Check debugger for computation fail');
                   CovariancesFromTwoGrids(RefGridLimits,Grid1,Grid2, r,covar,Mean1,Mean2,StdDev1,StdDev2);
                {$EndIf}
             end;
          end
          else begin
             Result := -999;
             {$IfDef RecordDEMIX} WriteLineToDebugFile('Invalid grid, DoCovarianceForPair Fail for tile ' + TileName + ' grid1=' + IntToStr(Grid1) + ' grid2=' + IntToStr(Grid2)); {$EndIf}
          end;
       end;


      procedure DoCriterion(Criterion,Count : ANSIString; usingPointGrids,usingAreaGrids : tDEM_int_array);
      var
         i,ThisRefDEM,ThisTestDEM : integer;
         Criterion2 : shortstring;
         RefGridLimits,TestGridLimits : tGridLimits;
         What,TStr : shortstring;
         gl1 : tGridLimits;
         Mean,Std : float32;
         MissingGrids : boolean;
      begin
          {$IfDef RecordDEMIX} WriteLineToDebugFile('Criterion=' + Criterion); {$EndIf}
          wmdem.SetPanelText(3, Criterion + ' ' + Count,true);
          ZeroPointAndAreaGrids(PtSSIMGrids, AreaSSIMGrids);
          MissingGrids := false;
          for i := 0 to NumPtDEMs do if not ValidDEM(usingPointGrids[i]) then MissingGrids := true;
          for i := 0 to NumAreaDEMs do if not ValidDEM(usingAreaGrids[i]) then MissingGrids := true;
          if MissingGrids then begin
             HighlightLineToDebugFile(Criterion + ' has missing data grids for ' + AreaName);
             exit;
          end;

          if MDDef.DoFUV then begin
             //do correlation coefficients first before normalizing grids for SSIM
             GISdb[DEMIXtileDB].MyData.First;
             while not GISdb[DEMIXtileDB].MyData.eof do begin
                bb := GISdb[DEMIXtileDB].MyData.GetRecordBoundingBox;
                TileName := GISdb[DEMIXtileDB].MyData.GetFieldByNameAsString('NAME');
                TileFull := GISdb[DEMIXtileDB].MyData.GetFieldByNameAsFloat('GRID_FULL');
                wmdem.SetPanelText(2, TileName + ' FUV',true);
                Criterion2 := StringReplace(Criterion,'SSIM','FUV',[rfReplaceAll, rfIgnoreCase]);
                aLine := TileName  + ',' + AreaName + ',' + RealToString(0.5*(bb.YMin+bb.YMax),-12,4) + ',' + RealToString(0.5*(bb.xMin+bb.xMax),-12,4) + ',' + Criterion2;

                RefGridLimits := DEMglb[usingPointGrids[0]].sfBoundBox2tGridLimits(bb);
                for i := 1 to NumPtDEMs do begin
                   wmdem.SetPanelText(3, Criterion + ' ' + Count + ' Point DEM=' + IntToStr(i),true);
                   aLine := aline + ',' + RealToString(DoCovarianceForPair(RefGridLimits,RefGridForThisPointGrid(UsingPointGrids,i),usingPointGrids[i]),-12,6);
                end;

                RefGridLimits := DEMglb[usingAreaGrids[0]].sfBoundBox2tGridLimits(bb);
                for i := 1 to NumAreaDEMs do begin
                   wmdem.SetPanelText(3, Criterion + ' ' + Count + ' Area DEM=' + IntToStr(i),true);
                   aLine := aline + ',' + RealToString(DoCovarianceForPair(RefGridLimits,usingAreaGrids[0],usingAreaGrids[i]),-12,6);
                end;
                Results.Add(aline);

                GISdb[DEMIXtileDB].MyData.Next;
             end;
             {$IfDef RecordDEMIX} WriteLineToDebugFile('FUV done for Criterion=' + Criterion); {$EndIf}
          end;

          if MDDef.DoSSIM and (abs(DEMGlb[PointDEMs[0]].ComputeSWCornerY) < 50) then begin
             //normalize grids for SSIM
             wmdem.SetPanelText(2, TileName + ' SSIM normalize',true);
             What := BeforeSpecifiedCharacterAnsi(Criterion,'_');
             for i := dmxFirstPoint to NumPtDEMs do NormalizeDEMforSSIM(PointGrids[i],What);
             for i := dmxFirstArea to NumAreaDEMs do NormalizeDEMforSSIM(AreaGrids[i],What);
             {$IfDef RecordDEMIX} WriteLineToDebugFile('Grids normalized for Criterion=' + Criterion); {$EndIf}
             wmdem.SetPanelText(2, TileName + ' SSIM grids',true);
             for i := 1 to NumPtDEMs do begin
                wmdem.SetPanelText(3, Criterion + ' ' + Count + ' Point DEM=' + IntToStr(i),true);
                ThisRefDEM := RefGridForThisPointGrid(usingPointGrids,PointDEMs[i]);
                ThisTestDEM := usingPointGrids[i];
                PtSSIMGrids[i] := MakeSSIMMap(false,true,ThisRefDEM,ThisTestDEM,1,11);
                {$IfDef RecordDEMIXSSIMGrid}
                    if ValidDEM(PtSSIMGrids[i]) then TStr := 'Good PtSSIMGrid=' + IntToStr(PtSSIMGrids[i]) + ' '
                    else TStr := 'Fail PtSSIMGrid ';
                    WriteLineToDebugFile(TStr + TileName + ' Ref DEM=' + IntToStr(ThisRefDEM) + ' TestDEM=' + IntToStr(ThisTestDEM));
                {$EndIf}
             end;
             for i := 1 to NumAreaDEMs do begin
                wmdem.SetPanelText(3, Criterion + ' ' + Count + ' Area DEM=' + IntToStr(i),true);
                AreaSSIMGrids[i] := MakeSSIMMap(false, true,usingAreaGrids[0],usingAreaGrids[i],1,11);
             end;
             {$IfDef RecordDEMIX} WriteLineToDebugFile('SSIM maps created for Criterion=' + Criterion); {$EndIf}

             wmdem.SetPanelText(3, Criterion + ' ' + Count + ' average tiles',true);
             GISdb[DEMIXtileDB].MyData.First;
             while not GISdb[DEMIXtileDB].MyData.eof do begin
                bb := GISdb[DEMIXtileDB].MyData.GetRecordBoundingBox;
                TileName := GISdb[DEMIXtileDB].MyData.GetFieldByNameAsString('NAME');
                wmdem.SetPanelText(2, TileName + ' SSIM',true);
                aLine := TileName + ',' + AreaName + ',' + RealToString(0.5*(bb.YMin+bb.YMax),-12,4) + ',' + RealToString(0.5*(bb.xMin+bb.xMax),-12,4) + ',' + Criterion;
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
                for i := 1 to NumAreaDEMs do begin
                   if ValidDEM(AreaSSIMGrids[i]) then begin
                      gl1 := DEMglb[AreaSSIMGrids[i]].sfBoundBox2tGridLimits(bb);
                      DEMglb[AreaSSIMGrids[i]].ElevationStatistics(gl1,Mean,Std);
                      aLine := aline + ',' + RealToString(1-Mean,-12,6);
                   end
                   else begin
                      {$IfDef RecordDEMIX} WriteLineToDebugFile('Fail for tile ' + TileName + ' Area DEM=' + DEMglb[usingAreaGrids[i]].AreaName); {$EndIf}
                      aLine := aline + ',' + '-999';
                   end;
                end;
                Results.Add(aline);
                GISdb[DEMIXtileDB].MyData.Next;
             end;
             for i := 1 to NumPtDEMs do CloseSingleDEM(PtSSIMGrids[i]);
             for i := 1 to NumAreaDEMs do CloseSingleDEM(AreaSSIMGrids[i]);
          end;
      end;

      procedure ClearDerivedGrids;
      var
         i : integer;
      begin
          for i := -1 to NumPtDEMs do CloseSingleDEM(PointGrids[i]);
          for i := -1 to NumAreaDEMs do CloseSingleDEM(AreaGrids[i]);
          for i := dmxFirstPoint to NumPtDEMs do CloseSingleDEM(PointGrids2[i]);
          for i := dmxFirstArea to NumAreaDEMs do CloseSingleDEM(AreaGrids2[i]);
          wmdem.SetPanelText(2, '',true);
      end;


   begin {procedure DoSSIMandFUVForAnArea}
       SaveFName := SSIMresultsDir + AreaName + '_ssim_results.csv';
       if (not Overwrite) and FileExists(SaveFName) then begin
          {$IfDef RecordDEMIXFull} HighLightLineToDebugFile('Area=' + AreaName + '  file exists=' + SaveFName); {$EndIf}
       end
       else begin
          {$IfDef RecordDEMIXFull} HighLightLineToDebugFile('AreaSSIMComputations area=' + AreaName); {$EndIf}
          wmdem.SetPanelText(3, 'Load DEMs',true);
          if OpenBothPixelIsDEMs(AreaName,'',DEMIX_Ref_1sec,DEMIX_test_dems,false) then begin
             ZeroPointAndAreaGrids(PointGrids,AreaGrids);
             {$If Defined(TrackPixelIs)} ShowDEMIXGrids('DEMs opened',PointDEMs,AreaDEMs); {$EndIf}

             Results := tStringList.Create;
             aline := 'DEMIX_TILE,AREA,LAT,LONG,CRITERION';
             for i := 1 to NumPtDEMs do aline := aLine + ',' + PointNames[i];
             for i := 1 to NumAreaDEMs do aline := aLine + ',' + AreaNames[i];
             Results.Add(aLine);

             DEMIXtileDB := DEMIXtileFill(PointDEMs[0],DEMGlb[PointDEMs[0]].DEMBoundBoxGeo,true);
             GISdb[DEMIXtileDB].ApplyGISFilter('GRID_FULL>' + IntToStr(MDDef.DEMIX_Full));

             if MDDef.SSIM_hill then begin
                CreateDEMIXhillshadeGrids;
                {$If Defined(TrackPixelIs)} ShowDEMIXGrids('Hillshades created',PointGrids,AreaGrids); {$EndIf}
                DoCriterion('HILL_SSIM','1/10',PointGrids,AreaGrids);
                ClearDerivedGrids;
             end;

             if MDDef.SSIM_Slope or MDDef.SSIM_ruff then begin
                CreateDEMIXSlopeRoughnessGrids;
                {$IfDef RecordDEMIXFull} WriteLineToDebugFile('Slope and roughness grids created'); {$EndIf}
                DoCriterion('SLOPE_SSIM','2/10',PointGrids2,AreaGrids2);
                DoCriterion('RUFF_SSIM','3/10',PointGrids,AreaGrids);
                ClearDerivedGrids;
             end;

             if MDDef.SSIM_TPI then begin
                wmdem.SetPanelText(3, 'Compute TPI',true);
                for i := dmxFirstPoint to NumPtDEMs do PointGrids[i] := DEMGlb[PointDEMs[i]].BoxCarDetrendDEM(false,DEMGlb[PointDEMs[i]].FullDEMGridLimits,3);
                for i := dmxFirstArea to NumAreaDEMs do AreaGrids[i] := DEMGlb[AreaDEMs[i]].BoxCarDetrendDEM(false,DEMGlb[AreaDEMs[i]].FullDEMGridLimits,3);
                DoCriterion('TPI_SSIM','4/10',PointGrids,AreaGrids);
                ClearDerivedGrids;
             end;

             if MDDef.SSIM_RRI then begin
                wmdem.SetPanelText(3, 'Compute RRI',true);
                for i := dmxFirstPoint to NumPtDEMs do PointGrids[i] := MakeTRIGrid(PointDEMs[i],nmRRI,false);
                for i := dmxFirstArea to NumAreaDEMs do AreaGrids[i] := MakeTRIGrid(AreaDEMs[i],nmRRI,false);
                DoCriterion('RRI_SSIM','5/10',PointGrids,AreaGrids);
                ClearDerivedGrids;
             end;

             if MDDef.SSIM_flow then begin
                wmdem.SetPanelText(3, 'Compute flow accumulation 6/10',true);
                //NOT opening map, using Log, and using D8
                for i := dmxFirstPoint to NumPtDEMs do PointGrids[i] := WBT_FlowAccumulation(False,True,True,DEMGlb[PointDEMs[i]].GeotiffDEMName,GridName);
                for i := dmxFirstArea to NumAreaDEMs do AreaGrids[i] := WBT_FlowAccumulation(False,True,True,DEMGlb[AreaDEMs[i]].GeotiffDEMName,GridName);
                {$IfDef RecordDEMIXFull} WriteLineToDebugFile('Flow accumulation grids created'); {$EndIf}
                DoCriterion('ACCUM_SSIM','6/10',PointGrids,AreaGrids);
                ClearDerivedGrids;
             end;

             if MDDef.SSIM_wet then begin
                wmdem.SetPanelText(3, 'Compute wetness index 7/10',true);
                for i := dmxFirstPoint to NumPtDEMs do PointGrids[i] := WBT_WetnessIndex(false,true,DEMGlb[PointDEMs[i]].GeotiffDEMName);
                for i := dmxFirstArea to NumAreaDEMs do AreaGrids[i] := WBT_WetnessIndex(false,true,DEMGlb[AreaDEMs[i]].GeotiffDEMName);
                DoCriterion('WETIN_SSIM','7/10',PointGrids,AreaGrids);
                ClearDerivedGrids;
             end;

             if MDDef.SSIM_LS then begin
                wmdem.SetPanelText(3, 'Compute LS factor 8/10',true);
                for i := dmxFirstPoint to NumPtDEMs do PointGrids[i] := SAGA_LSFactor(false,DEMGlb[PointDEMs[i]].GeotiffDEMName);
                for i := dmxFirstArea to NumAreaDEMs do AreaGrids[i] := SAGA_LSFactor(false,DEMGlb[AreaDEMs[i]].GeotiffDEMName);
                DoCriterion('LS_SSIM','8/10',PointGrids,AreaGrids);
                ClearDerivedGrids;
             end;

             if MDDef.SSIM_HAND then begin
                wmdem.SetPanelText(3, 'Compute HAND 9/10', true);
                for i := dmxFirstPoint to NumPtDEMs do PointGrids[i] := WBT_ElevAboveStream(false,DEMGlb[PointDEMs[i]].GeotiffDEMName,GridName);
                for i := dmxFirstArea to NumAreaDEMs do AreaGrids[i] := WBT_ElevAboveStream(false,DEMGlb[AreaDEMs[i]].GeotiffDEMName,GridName);;
                DoCriterion('HAND_SSIM','9/10',PointGrids,AreaGrids);
                ClearDerivedGrids;
             end;

             if MDDef.SSIM_elev then begin
                //Elevation is done last, so we no longer need elevation to create derived grids, and can normalize it
                wmdem.SetPanelText(3, 'Compute elevation',true);
                DoCriterion('ELEV_SSIM','10/10',PointDEMs,AreaDEMs);
             end;

             if Results.Count > 1 then begin
                Results.SaveToFile(SavefName);
                {$IfDef RecordDEMIX} WriteLineToDebugFile('SSIM/FUV save to ' + SaveFName); {$EndIf}
             end
             else begin
                {$IfDef RecordDEMIX} HighlightLineToDebugFile('No SSIM/FUV results for ' + SaveFName); {$EndIf}
             end;
             Results.Free;
             CloseAllDEMs;
             CloseAndNilNumberedDB(DEMIXtileDB);
             CleanUpTempDirectory;  //lot of files created by SAGA and WBT
          end
          else begin
             {$IfDef RecordDEMIX} HighLightLineToDebugFile('Missing DEM files for area ' + AreaName); {$EndIf}
          end;
       end;
   end {procedure DoSSIMandFUVForAnArea};



procedure AreaSSIMComputations(Overwrite : boolean);
var
   Areas : tStringList;
   Area  : shortstring;
   i     : integer;
begin {procedure AreaSSIMComputations}
   if MDDef.DoFUV or MDDef.DoSSIM then begin
      try
         {$IfDef RecordDEMIXFull} WriteLineToDebugFile('AreaSSIMComputations in'); {$EndIf}
         GetDEMIXPaths(true);
         {$IfDef RecordDEMIXFull} WriteLineToDebugFile('Finished GetDEMIXPaths'); {$EndIf}
         Areas := DEMIX_AreasWanted;
         {$IfDef RecordDEMIX} WriteLineToDebugFile('AreaSSIMComputations in, selected areas=' + IntToStr(Areas.Count)); {$EndIf}
         SafeMakeDir(SSIMresultsDir);
         LockStatusBar := true;
         wmdem.SetPanelText(0, 'Started ' + TimeToStr(Now),true);
         for i := 0 to pred(Areas.Count) do begin
            Area := Areas.Strings[i];
            wmdem.SetPanelText(1, IntToStr(succ(i)) + '/' + IntToStr(Areas.Count) + '  ' + Area,true);
            DoSSIMandFUVForAnArea(Area,Overwrite);
            CloseAllDEMs;
         end;
      finally
         Areas.Free;
         EndDEMIXProcessing;
      end;
      {$IfDef RecordDEMIX} HighLightLineToDebugFile('AreaSSIMComputations out'); {$EndIf}
   end
   else begin
      MessageToContinue('Neither FUV/SSIM selected');
   end;
end {procedure AreaSSIMComputations};



