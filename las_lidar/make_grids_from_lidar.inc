{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{________________________________________}
{                                        }
{   include file for make_grid           }
{________________________________________}



procedure CreateDEMsfromLidar;
var
   CloudFNames,DEMsToCreate : PathStr;
   Clouds : tStringList;
   I : Integer;
begin
   {$IfDef LidarGlobalDEMs} WriteLineToDebugFile('Twmdem.CreateDEMsfromlidar1Click in'); {$EndIf}
   StopSplashing;
   CloudFNames := 'c:\microdem\clouds.txt';
   DEMsToCreate := 'c:\microdem\match_tandemx_dems.dbf';
   Clouds := tStringList.Create;
   Clouds.LoadFromFile(CloudFNames);
   for I := 0 to pred(Clouds.Count) do begin
      {$IfDef LidarGlobalDEMs} WriteLineToDebugFile('Clouds.Strings[i]'); {$EndIf}
       if ValidPath(Clouds.Strings[i]) then begin
          OverlayPointClouds(Nil,Clouds.Strings[i]);
          if (pt_cloud_opts_fm.LasFiles[1].CloudName <> '') then begin
             pt_cloud_opts_fm.AutoSaveDir := 'c:\temp\';
             pt_cloud_opts_fm.DEMrulesFName := DEMstoCreate;
             pt_cloud_opts_fm.BitBtn50Click(Nil);
          end
          else begin
             {$IfDef RecordProblems} HighlightLineToDebugFile('No LAS files in ' + Clouds.Strings[i]); {$EndIf}
          end;
          pt_cloud_opts_fm_Close;
          CloseAllWindowsAndData;
       end
       else begin
          {$IfDef RecordProblems} HighlightLineToDebugFile('Invalid dirctory  ' + Clouds.Strings[i]); {$EndIf}
       end;
   end;
   {$IfDef LidarGlobalDEMs} WriteLineToDebugFile('Twmdem.CreateDEMsfromlidar1Click out'); {$EndIf}
end;


function MakeGridFromLidarPointCloud(TheCloudName : shortString; PCGridMaker : tPCGridMaker; BaseMap : tMapForm;  UsePC : tUsePC; LasFiles : tLasFiles;
        HorizDatum,VertDatum : shortstring; MaxAreaZ,MinAreaZ : float32; AutoSaveDir : PathStr; ShowMeanDensityGrid : boolean) : integer;

    function MakeGridFromLidarCloud: integer;
    var
       NewHeadRecs : tDEMheader;
       AirReturnDEM,
       BlankDEM,
       ClassDEM,
       DSMDEM,
       FirstIntensityDEM,
       LastIntensityDEM,
       MaxIntensityDEM,
       MaxScanAngleDEM,
       MeanIntensityDEM,
       MeanReturnHeightDEM,
       MeanReturnStdDEM,
       MinIntensityDEM,
       MinScanAngleDEM,
       NewBuildingDEM,
       NewDensity,
       NewDistance,
       FirstReturnDensity,
       FirstReturnDensityForMean,
       NewGroundDensity,
       NewGroundMax,
       NewGroundMean,
       NewGroundMin,
       NewGroundNearest,
       NewGroundXYZ,
       NewOtherDEM,
       NewPointIDdem,
       NewRGBGrid,
       NewUnclassDEM,
       NewUserDataDEM,
       NewVegDEM,
       MeanFirstReturn,
       MeanLastReturn,
       NVSDEM,
       OverlapDEM,
       DSMDensity,
       NVSDensity,
       SecondReturnDensity,
       SingleReturnDensity,
       LastReturnDensity,
       TempDEM,
       CheckDEM,
       Intensity,
       i,RecsRead : integer;
       zShot{,zCrit}: float64;
       fName,OutDir : PathStr;
       Count : int64;
       Ext : ExtStr;
       LasData : Las_Lidar.tLAS_data;
       VegDensity : array[1..MaxVegLayers] of integer;

         procedure CreateNewGrids;

              function NewGridName(What : shortString) : ShortString;
              begin
                 Result := What + '_' + TheCloudName;
              end;

              function MissingDataGrid(GridName : ShortString; InitialValueMode : integer = InitDEMMissing) : integer;
              begin
                 OpenAndZeroNewDEM(true,NewHeadRecs,Result,GridName,InitialValueMode);
                 CheckDEM := Result;
                 {$IfDef RecordMakeGrid} WriteLineToDebugFile('Missing data grid out, ' + DEMGlb[Result].FullDEMParams); {$EndIf}
              end;

         var
            i : integer;
         begin
             NewHeadRecs := DEMGlb[TempDEM].DEMheader;
             NewHeadRecs.RasterPixelIsGeoKey1025 := MDDef.LasDEMPixelIs;
             //NewHeadRecs.DEMPrecision := LongWordDEM;
             //NewHeadRecs.h_DatumCode := Edit35.Text;
             //NewHeadRecs.VerticalCSTypeGeoKey := VertDatumCode(Edit36.Text);
             NewHeadRecs.h_DatumCode := HorizDatum;
             NewHeadRecs.VerticalCSTypeGeoKey := VertDatumCode(VertDatum);

             NewHeadRecs.DEMPrecision := FloatingPointDEM;
             NewHeadRecs.ElevUnits := euMeters;
             if (PCGridMaker in [pcgmMeanStd]) then begin
                MeanReturnHeightDEM := MissingDataGrid(NewGridname('Mean_return_ht'));
                OpenAndZeroNewDEM(true,NewHeadRecs,MeanReturnStdDEM,NewGridname('Mean_return_std'),InitDEMvalue,0);
             end
             else if (PCGridMaker in [pcgmMeanFirst]) then begin
                MeanFirstReturn := MissingDataGrid(NewGridname('Point_cloud_DSM_Mean_first_return'),InitDEMmissing);
                MeanLastReturn := MissingDataGrid(NewGridname('Point_cloud_NVS_Mean_last_return'),InitDEMmissing);
                NewHeadRecs.ElevUnits := euUndefined;
                NewHeadRecs.DEMPrecision := WordDEM;
                FirstReturnDensityForMean := MissingDataGrid(NewGridname('First_return__density'),InitDEMZero);
                LastReturnDensity := MissingDataGrid(NewGridname('Last_return__density'),InitDEMZero);
             end
             else if (PCGridMaker in [pcgmThreeDEMs,pcgmCeilFloor]) then begin
                if (MDDef.MakePCFloor) then NVSDEM := MissingDataGrid(NewGridname('Point_cloud_NVS_min'),InitDEMmissing);
                if (MDDef.MakePCCeiling) then DSMdem := MissingDataGrid(NewGridname('Point_cloud_DSM_max'),InitDEMmissing);
                NewHeadRecs.ElevUnits := euUndefined;
                NewHeadRecs.DEMPrecision := WordDEM;
                if (MDDef.MakePCFloor) then NVSdensity := MissingDataGrid(NewGridname('Point_cloud_NVS_density'),InitDEMZero);
                if (MDDef.MakePCCeiling) then DSMdensity := MissingDataGrid(NewGridname('Point_cloud_DSM_density/'),InitDEMzero);
             end;

             if (PCGridMaker in [pcgmThreeDEMs,pcgmGrndPtDTM]) then begin
                {$IfDef RecordMakeGrid} WriteLineToDebugFile('start pcgmGrndPtDTM'); {$EndIf}
                NewHeadRecs.DEMPrecision := FloatingPointDEM;
                NewHeadRecs.ElevUnits := euMeters;
                if (MDDef.DTMoption in [dtmAll,dtmMean]) then NewGroundMean := MissingDataGrid(NewGridname('PointCloud_DTM_Mean'));
                if (MDDef.DTMoption in [dtmAll,dtmMax]) then NewGroundMax := MissingDataGrid(NewGridname('DTM_ground_pts_Max'));
                if (MDDef.DTMoption in [dtmAll,dtmMin]) then NewGroundMin := MissingDataGrid(NewGridname('DTM_ground_pts_Min'));
                if (MDDef.DTMoption in [dtmAll,dtmNearest]) then begin
                   NewGroundNearest := MissingDataGrid(NewGridname('DTM_ground_pts_Nearest'));
                   NewDistance := MissingDataGrid(NewGridname('DTM_ground_pts_Dist'));
                end;
                {$IfDef RecordMakeGrid} WriteLineToDebugFile('done prep for pcgmGrndPtDTM'); {$EndIf}
             end
             else if (PCGridMaker = pcgmVegVox) then begin
                OutDir := ExtractFilePath(DEMGlb[BaseMap.MapDraw.DEMonMap].DEMFileName);
                GetDosPath('Vegetation density grids',OutDir);
                if (OutDir = '') then exit;
                ReadDefault('Layers',MDDef.VegDensityHeights);
                if (MDDef.VegDensityHeights > MaxVegLayers) then MDDef.VegDensityHeights := MaxVegLayers;
                MDDef.DiscardHighPointsVegDensity := AnswerIsYes('Discard points above ' + IntToStr(MDDef.VegDensityHeights) + 'm');

                StartProgress('open');
                for i := 1 to MDDef.VegDensityHeights do begin
                   NewHeadRecs.DEMPrecision := ByteDEM;
                   NewHeadRecs.ElevUnits := euUndefined;
                   UpdateProgressBar(i/MaxVegLayers);
                   OpenAndZeroNewDEM(true,NewHeadRecs,VegDensity[i],'Voxel up to ' + IntToStr(i),InitDEMvalue,0);
                end;
                EndProgress;
             end
             else if (PCGridMaker = pcgmDensityVox) then begin
                GetDosPath('Point cloud density voxels',OutDir);
                if (OutDir = '') then exit;
                MDDef.VoxBinHeight := 1;
                while (round(MaxAreaZ-MinAreaZ) div MDDef.VoxBinHeight) > MDDef.VegDensityHeights do inc(MDDef.VoxBinHeight);
                StartProgress('open');
                for i := 1 to MaxVegLayers do begin
                   NewHeadRecs.DEMPrecision := SmallIntDEM;
                   NewHeadRecs.ElevUnits := euUndefined;
                   UpdateProgressBar(i/MaxVegLayers);
                   OpenAndZeroNewDEM(true,NewHeadRecs,VegDensity[i],'Voxel height ' + IntToStr(i * MDDef.VoxBinHeight),InitDEMvalue,0);
                end;
                EndProgress;
             end
             else if (PCGridMaker in [pcgmScanAngle]) then begin
                NewHeadRecs.DEMPrecision := SmallIntDEM;  //byte would work for scan angle, but we need negatives
                NewHeadRecs.ElevUnits := euUndefined;
                OpenAndZeroNewDEM(true,NewHeadRecs,MinScanAngleDEM,NewGridname('Min_scan_angle'),InitDEMvalue,-9999);
                OpenAndZeroNewDEM(true,NewHeadRecs,MaxScanAngleDEM,NewGridname('Max_scan_angle'),InitDEMvalue,9999);
             end
             else if (PCGridMaker in [pcgmPointSourceID,pcgmUserData]) then begin
                NewHeadRecs.ElevUnits := euUndefined;
                NewHeadRecs.DEMPrecision := WordDEM;
                NewPointIDDEM := MissingDataGrid(NewGridname('Point_ID'));
                NewHeadRecs.DEMPrecision := ByteDEM;
                NewUserDataDEM := MissingDataGrid(NewGridname('User_data'));
             end
             else if (PCGridMaker in [pcgmAllIntensity,pcGMMaxIntensity,pcgmMinIntensity,pcgmFirstIntensity,pcgmLastIntensity]) then begin
                NewHeadRecs.DEMPrecision := WordDEM;       //intensities go over 255
                NewHeadRecs.ElevUnits := euImagery;
                if (PCGridMaker in [pcgmAllIntensity,pcGMMaxIntensity]) then MaxIntensityDEM := MissingDataGrid(NewGridname('Lidar_max_intensity'));
                if (PCGridMaker in [pcgmAllIntensity,pcgmMinIntensity]) then MinIntensityDEM := MissingDataGrid(NewGridname('Lidar_min_intensity'));
                if (PCGridMaker in [pcgmFirstIntensity,pcgmAllIntensity]) then FirstIntensityDEM := MissingDataGrid(NewGridname('Lidar_first_return_intensity'));
                if (PCGridMaker in [pcgmLastIntensity,pcgmAllIntensity]) then LastIntensityDEM := MissingDataGrid(NewGridname('Lidar_last_return_intensity'));
                if (PCGridMaker in [pcgmAllIntensity]) then begin
                   OpenAndZeroNewDEM(true,NewHeadRecs,NewDensity,NewGridname('Num_ground_returns'),InitDEMvalue,0);
                   NewHeadRecs.DEMPrecision := FloatingPointDEM;
                   OpenAndZeroNewDEM(true,NewHeadRecs,MeanIntensityDEM,NewGridname('Lidar_mean_intensity'),InitDEMvalue,0);
                end;
             end
             else if (PCGridMaker in [pcgmClass]) then begin
                NewHeadRecs.DEMPrecision := WordDEM;
                NewHeadRecs.ElevUnits := euUndefined;
                OpenAndZeroNewDEM(true,NewHeadRecs,NewDensity,NewGridname('Total_density'),InitDEMvalue,0);
                OpenAndZeroNewDEM(true,NewHeadRecs,NewVegDEM,NewGridname('Vegetation_density'),InitDEMvalue,0);
                OpenAndZeroNewDEM(true,NewHeadRecs,NewUnclassDEM,NewGridname('Unclassified_density'),InitDEMvalue,0);
                OpenAndZeroNewDEM(true,NewHeadRecs,NewGroundDensity,NewGridname('Ground_density'),InitDEMvalue,0);
                OpenAndZeroNewDEM(true,NewHeadRecs,NewBuildingDEM,NewGridname('Building_density'),InitDEMvalue,0);
                OpenAndZeroNewDEM(true,NewHeadRecs,NewOtherDEM,NewGridname('Other_density'),InitDEMvalue,0);
             end
             else if (PCGridMaker = pcgmClassification) then begin
                NewHeadRecs.DEMPrecision := ByteDEM;
                //need to deal with LAS 1.4
                NewHeadRecs.ElevUnits := euLASClass13;
                OpenAndZeroNewDEM(true,NewHeadRecs,ClassDEM,NewGridname('Classification'),InitDEMmissing);
                //CheckDEM := ClassDEM;
             end;
             if (PCGridMaker in [pcgmRGB]) then begin
                NewHeadRecs.ElevUnits := euRGB;
                NewRGBGrid := MissingDataGrid(NewGridname('rgb'));
             end;

             NewHeadRecs.ElevUnits := euUndefined;
             NewHeadRecs.DEMPrecision := WordDEM;
             if (PCGridMaker in [pcgmAllIntensity,pcgmClass,pcgmPointCount,pcgmThreeKeyDensities,pcgmMeanStd]) then NewDensity := MissingDataGrid(NewGridname('Total_pt_density'),InitDEMzero);
             if (PCGridMaker = pcgmSecondRet) then SecondReturnDensity := MissingDataGrid(NewGridname('Second_return_density'),InitDEMzero);
             if (PCGridMaker = pcgmSingleRet) then SingleReturnDensity := MissingDataGrid(NewGridname('Single_return_density'),InitDEMzero);
             if (PCGridMaker = pcgmOverlap) then OverlapDEM := MissingDataGrid(NewGridname('Overlap_density'),InitDEMzero);
             if (PCGridMaker in [pcgmFirstRet,pcgmThreeKeyDensities]) then FirstReturnDensity := MissingDataGrid(NewGridname('First_return_density'),InitDEMzero);
             if (PCGridMaker in [pcgmThreeKeyDensities,pcgmGround,pcgmGrndPtDTM]) then NewGroundDensity := MissingDataGrid(NewGridname('Ground_return_density'),InitDEMzero);
             if (PCGridMaker in [pcgmAirNonLastReturn]) then AirReturnDEM := MissingDataGrid(NewGridname('Non_last_return_density'),InitDEMzero);
             if (PCGridMaker = pcgmBlank) then BlankDEM := MissingDataGrid(NewGridname('Blank_grid'));
         end {procedure CreateNewGrids};


           procedure OneLasLayer(Layer : integer);
           var
              xgrid,ygrid,k,i,j,LasClass : integer;
              zGrid,Dist,xf,yf : float32;
              NoFilter,OK : boolean;
           begin
              NoFilter := NoFilterWanted;  //not checking for noise, or a user filter
              {$IfDef RecordMakeGrid} WriteLineToDebugFile('One layer, layer=' + IntToStr(Layer) + '  points=' + IntToStr(LasFiles[Layer].TotalCloudPts) + ' filter=' + TrueOrFalse(not NoFilter)); {$EndIf}
              for k := 0 to Pred(LasFiles[Layer].LAS_fnames.Count) do begin
                 ThreadTimers.OverallGauge9.Progress := round(100 * k / LasFiles[Layer].LAS_fnames.Count);
                 fName := LasFiles[Layer].LAS_fnames.Strings[k];
                 LasData := Las_Lidar.tLAS_data.Create(fName);
                  {$If Defined(RecordMakeGridFull) or Defined(RecordGridFileNames)} WriteLineToDebugFile('One layer using=' + fName); {$EndIf}
                  LasData.PrepDataRead;
                  for i := 0 to LasData.ReadsRequired do begin
                      if (i mod 5 = 0) then ThreadTimers.Gauge1.Progress := round(100 * i/LasData.ReadsRequired);
                      LasData.ReadPoints(RecsRead);
                      for j := 1 to RecsRead do begin
                          if NoFilter or LasData.MeetsFilter(j) then begin
                             if FindDEMGridCellOfShot(LasData,CheckDEM,J,xgrid,ygrid) then begin
                                LASclass := LasData.LASClassification(j);
                                zShot := Lasdata.ExpandLAS_Z(j);

                                if (PCGridMaker in [pcgmGrndPtDTM,pcgmCeilFloor,pcgmThreeDEMs]) then begin
                                     if LasData.GroundReturn(j) then begin
                                        if ValidDEM(NewGroundDensity) then DEMGlb[NewGroundDensity].IncrementGridValue(xgrid,ygrid);
                                        if ValidDEM(NewGroundMean) then begin
                                           DEMGlb[NewGroundMean].SetGridElevation(xgrid,ygrid,zShot+Zgrid);
                                        end;
                                        if ValidDEM(NewGroundNearest) then begin
                                           Dist := sqr(xf-xgrid) + sqr(yf-ygrid);
                                           if DEMGlb[NewDistance].GetElevMetersOnGrid(xgrid,ygrid,zGrid) and (Dist < zgrid) then begin
                                              DEMGlb[NewGroundNearest].SetGridElevation(xgrid,ygrid,zShot);
                                              DEMGlb[NewDistance].SetGridElevation(xgrid,ygrid,Dist);
                                           end;
                                        end;
                                        if ValidDEM(NewGroundMax) then begin
                                           if (not DEMGlb[NewGroundMax].GetElevMetersOnGrid(xgrid,ygrid,zGrid)) or (zShot > zGrid) then DEMGlb[NewGroundMax].SetGridElevation(xgrid,ygrid,zShot);
                                        end;
                                        if ValidDEM(NewGroundMin) then begin
                                           if (not DEMGlb[NewGroundMin].GetElevMetersOnGrid(xgrid,ygrid,zGrid)) or (zShot < zGrid) then DEMGlb[NewGroundMin].SetGridElevation(xgrid,ygrid,zShot);
                                        end;
                                     end;
                                     if ValidDEM(DSMDEM) then begin
                                        if (not DEMGlb[DSMDEM].GetElevMetersOnGrid(xgrid,ygrid,zGrid)) or (zshot > zGrid) then DEMGlb[DSMDEM].SetGridElevation(xgrid,ygrid,zShot);
                                     end;
                                     if ValidDEM(NVSDEM) then begin
                                        if (not DEMGlb[NVSDEM].GetElevMetersOnGrid(xgrid,ygrid,zGrid)) or (zshot < zGrid) then DEMGlb[NVSDEM].SetGridElevation(xgrid,ygrid,zShot);
                                     end;
                                end
                                else begin
                                   if (PCGridMaker in [pcgmMeanStd]) then begin
                                      DEMGlb[NewDensity].IncrementGridValue(xgrid,ygrid);
                                      if (not DEMGlb[MeanReturnHeightDEM].GetElevMetersOnGrid(xgrid,ygrid,zGrid)) then ZGrid := 0;
                                      DEMGlb[MeanReturnHeightDEM].SetGridElevation(xgrid,ygrid,zShot+Zgrid);
                                   end;
                                   if (MeanFirstReturn <> 0) then begin
                                      if LasData.FirstReturn(j) then begin
                                         DEMGlb[FirstReturnDensityForMean].IncrementGridValue(xgrid,ygrid);
                                         if DEMGlb[MeanFirstReturn].GetElevMetersOnGrid(xgrid,ygrid,zGrid) then ZGrid := zShot+Zgrid
                                         else ZGrid := zshot;
                                         DEMGlb[MeanFirstReturn].SetGridElevation(xgrid,ygrid,Zgrid);
                                      end;
                                   end;
                                   if (MeanLastReturn <> 0) then begin
                                      if LasData.LastReturn(j) then begin
                                         DEMGlb[LastReturnDensity].IncrementGridValue(xgrid,ygrid);
                                         if (DEMGlb[MeanLastReturn].GetElevMetersOnGrid(xgrid,ygrid,zGrid)) then ZGrid := zShot+Zgrid
                                         else zGrid := zshot;
                                         DEMGlb[MeanLastReturn].SetGridElevation(xgrid,ygrid,Zgrid);
                                      end;
                                   end;
                                   begin
                                      if (NewDensity <> 0) then DEMGlb[NewDensity].IncrementGridValue(xgrid,ygrid);
                                      if (FirstReturnDensity <> 0) and LasData.FirstReturn(j) then DEMGlb[FirstReturnDensity].IncrementGridValue(xgrid,ygrid);
                                      if (SingleReturnDensity <> 0) and (LasData.ReturnsInPulse(j) = 1) then DEMGlb[SingleReturnDensity].IncrementGridValue(xgrid,ygrid);
                                      if (OverlapDEM <> 0) and LASData.OverlapPoint(j) then DEMGlb[OverlapDEM].IncrementGridValue(xgrid,ygrid);
                                      if (SecondReturnDensity <> 0) and (LASData.ReturnNumber(j) = 2) then DEMGlb[SecondReturnDensity].IncrementGridValue(xgrid,ygrid);
                                      if (AirReturnDEM <> 0) and LasData.AirReturn(j) then DEMGlb[AirReturnDEM].IncrementGridValue(xgrid,ygrid);
                                       if (MinScanAngleDEM <> 0) and (MaxScanAngleDEM <> 0) then begin
                                        zShot := LasData.GetScanAngle(j);
                                        if DEMGlb[MinScanAngleDEM].GetElevMeters(xgrid,ygrid,zGrid) and (zshot < zGrid) then DEMGlb[MinScanAngleDEM].SetGridElevation(xgrid,ygrid,zShot);
                                        if DEMGlb[MaxScanAngleDEM].GetElevMeters(xgrid,ygrid,zGrid) and (zshot > zGrid) then DEMGlb[MaxScanAngleDEM].SetGridElevation(xgrid,ygrid,zShot);
                                      end;

                                      if (PCGridMaker in [pcgmMaxIntensity,pcgmMinIntensity,pcgmAllIntensity,pcgmFirstIntensity,pcgmLastIntensity]) then begin
                                         Intensity := LasData.GetShotMeasuredIntensity(j);
                                         if (not DEMGlb[MaxIntensityDEM].GetElevMeters(xgrid,ygrid,zGrid)) or (Intensity > zGrid) then DEMGlb[MaxIntensityDEM].SetGridElevation(xgrid,ygrid,Intensity);
                                         if (MinIntensityDEM <> 0) then if (not DEMGlb[MinIntensityDEM].GetElevMeters(xgrid,ygrid,zGrid) or (Intensity < zGrid)) then DEMGlb[MinIntensityDEM].SetGridElevation(xgrid,ygrid,Intensity);
                                         if (MeanIntensityDEM <> 0) then begin
                                            if (not DEMGlb[MeanIntensityDEM].GetElevMeters(xgrid,ygrid,zGrid)) then zgrid := 0;
                                            DEMGlb[MeanIntensityDEM].SetGridElevation(xgrid,ygrid,Intensity + zGrid);
                                         end;
                                         if (FirstIntensityDEM <> 0) then DEMGlb[FirstIntensityDEM].SetGridElevation(xgrid,ygrid,Intensity);
                                         if (LastIntensityDEM <> 0) then DEMGlb[LastIntensityDEM].SetGridElevation(xgrid,ygrid,Intensity);
                                      end;
                                   end;
                                end{if};
                             end;
                          end;
                      end {for j};
                  end {for i};
                 FreeAndNil(LasData);
              end {for k};
           end {procedure OneLasLayer};


           procedure CheckMap(DEM : integer; FillHoles : boolean = true; MapType : tMapType = mtElevRainbow; Save : boolean = false);
           var
              fName : PathStr;
           begin
              if ValidDEM(DEM) then begin
                 if FillHoles and MDDef.PCAutoFillHoles then begin
                    DEMGlb[DEM].InterpolateAcrossHoles(false);
                 end;
                 {$IfDef RecordMakeGrid} WriteLineToDebugFile('CheckMap, DEM=' + IntToStr(DEM) + '  ' + DEMGlb[DEM].AreaName); {$EndIf}
                 if Save then begin
                    fName := AutoSaveDir + DEMGlb[DEM].AreaName + '.tif';
                    {$IfDef RecordMakeGrid} WriteLineToDebugFile('Save DEM, ' + fName); {$EndIf}
                    DEMGlb[DEM].WriteNewFormatDEM(fName);
                 end;
                 DEMGlb[DEM].SetUpMap(true,MapType);
                 {$IfDef RecordMakeGrid} WriteLineToDebugFile(DEMGlb[DEM].zRange + ' geo range=' + sfBoundBoxToString( DEMGlb[DEM].SelectionMap.MapDraw.MapCorners.BoundBoxGeo,6)); {$EndIf}
                 if (PCGridMaker <> pcgmBlank) and (DEMGlb[DEM].DEMheader.MaxElev = 0) then begin
                    {$IfDef RecordMakeGrid} WriteLineToDebugFile('Closing, empty DEM=' + IntToStr(DEM) + ' ' + DEMGlb[DEM].AreaName); {$EndIf}
                    CloseSingleDEM(DEM);
                 end;
              end;
           end;

              procedure FinishMeanStdDevProcessing(MeanDEM,STDDEM,CountDEM : integer);
              var
                 Col,Row : integer;
                 std,sumsq,sum,z2 : float32;
              begin
                for Col := 0 to pred(DEMGlb[CountDEM].DEMheader.NumCol) do begin
                   for Row := 0 to pred(DEMGlb[CountDEM].DEMheader.NumRow) do begin
                      if DEMGlb[CountDEM].GetElevMeters(Col,Row,z2) then begin
                         if (z2 < 0.01) then begin
                            DEMGlb[CountDEM].SetGridMissing(Col,Row);
                            DEMGlb[MeanDEM].SetGridMissing(Col,Row);
                            if (StdDEM <> 0) then DEMGlb[StdDEM].SetGridMissing(Col,Row);
                         end
                         else begin
                            DEMGlb[MeanDEM].GetElevMeters(Col,Row,sum);
                            DEMGlb[MeanDEM].SetGridElevation(Col,Row,sum/z2);
                            if (StdDEM <> 0) then begin
                               if (z2 > 1.001) then begin
                                  DEMGlb[StdDEM].GetElevMeters(Col,Row,sumsq);
                                  std := sqrt( (z2 * sumsq - sqr(sum)) / z2 / (z2 - 1));
                                  DEMGlb[StdDEM].SetGridElevation(Col,Row,std);
                               end
                               else DEMGlb[StdDEM].SetGridMissing(Col,Row);
                            end;
                         end;
                      end;
                   end;
                 end;
              end;


        function DTMOptionName : shortstring;
        begin
           case MDDef.DTMoption of
              dtmMax : Result := 'max';
              dtmMean : Result := 'mean';
              dtmMin : Result := 'min';
              dtmNearest : Result := 'nearest';
           end;
        end;

        procedure MakeMapDisplays;
        var
           i : integer;
        begin
           ShowHourglassCursor;
           if (PCGridMaker = pcgmVegVox) then begin
              for i := 1 to MDDef.VegDensityHeights do begin
                 DEMGlb[VegDensity[i]].MarkInRangeMissing(0,0,Count);
                 fName := OutDir + 'las_density_'+ IntToStr(i) + '.dem';
                 DEMGlb[VegDensity[i]].WriteNewFormatDEM(fName);
                 CloseSingleDEM(VegDensity[i]);
              end;
           end
           else if (PCGridMaker = pcgmDensityVox) then begin
              for i := 1 to MDDef.VegDensityHeights do begin
                 DEMGlb[VegDensity[i]].MarkInRangeMissing(0,0,Count);
                 fName := OutDir + 'las_density_'+ IntToStr(i) + '_' + IntToStr(round(MinAreaZ + Pred(i))) + '.dem';
                 DEMGlb[VegDensity[i]].WriteNewFormatDEM(fName);
                 CloseSingleDEM(VegDensity[i]);
              end;
           end
           else if (PCGridMaker in [pcgmGrndPtDTM]) then begin
               if (NewGroundMean <> 0) then begin
                  FinishMeanStdDevProcessing(NewGroundMean,0,NewGroundDensity);
                  if ShowMeanDensityGrid then CheckMap(NewGroundDensity)
                  else CloseSingleDEM(NewGroundDensity);
                  Result := NewGroundMean;
               end;
               CloseSingleDEM(NewDistance);
           end
           else if (PCGridMaker in [pcgmMeanStd]) then begin
              FinishMeanStdDevProcessing(MeanReturnHeightDEM,MeanReturnStdDEM,NewDensity);
           end;
           if (MeanFirstReturn <> 0) then begin
              FinishMeanStdDevProcessing(MeanFirstReturn,0,FirstReturnDensityForMean);
           end;
           if (MeanLastReturn <> 0) then begin
              FinishMeanStdDevProcessing(MeanLastReturn,0,LastReturnDensity);
           end;

           if (PCGridMaker in [pcgmAllIntensity]) then begin
              FinishMeanStdDevProcessing(MeanIntensityDEM,0,NewDensity);
           end
           else if (PCGridMaker in [pcgmScanAngle]) then begin
               //we start with way out of range elevations, and now mark missing the cells with no returns
               if (MinScanAngleDEM <> 0) then DEMGlb[MinScanAngleDEM].MarkInRangeMissing(9998,10000,Count);
               if (MaxScanAngleDEM <> 0) then DEMGlb[MaxScanAngleDEM].MarkInRangeMissing(-10000,-9998,Count);
           end
           else if (PCGridMaker = pcgmClassification) then begin
              if (ClassDEM <> 0) then begin
                 CheckMap(ClassDEM,false,mtLASclass);
                 if MDDef.PCAutoFillHoles then begin
                    ModeFilterDEM(ClassDEM,1,true);
                    CloseSingleDEM(ClassDEM);
                    CheckDEM := LastDEMLoaded;
                 end;
              end;
           end;

           if (NewRGBGrid <> 0) then begin
              CheckMap(NewRGBGrid,false,mtRGBimagery);
              if MDDef.PCAutoFillHoles then begin
                 DEMGlb[NewRGBGrid].RGBFilterDEM(1,true);
                 CloseSingleDEM(NewRGBGrid);
                 NewRGBGrid := LastDEMLoaded;
              end;
           end;

           CheckMap(DSMDEM,true,mtIHSReflect,true);
           CheckMap(NVSDEM,true,mtIHSReflect,true);
           CheckMap(MeanFirstReturn,true,mtIHSReflect,true);
           CheckMap(MeanLastReturn,true,mtIHSReflect,true);
           CheckMap(NewGroundMean,true,mtIHSReflect,true);

           CheckMap(AirReturnDEM,false);
           CheckMap(BlankDEM,false);
           CheckMap(FirstIntensityDEM,true,mtElevGray);
           CheckMap(LastIntensityDEM,true,mtElevGray);
           CheckMap(MaxIntensityDEM,true,mtElevGray);
           CheckMap(MinIntensityDEM,true,mtElevGray);
           CheckMap(MaxScanAngleDEM);
           CheckMap(MeanIntensityDEM,true,mtElevGray);
           CheckMap(MeanReturnHeightDEM,true,mtIHSReflect);
           CheckMap(MeanReturnStdDEM);
           CheckMap(MinIntensityDEM,true,mtElevGray);
           CheckMap(MinScanAngleDEM);
           CheckMap(NewBuildingDEM,false);
           CheckMap(NewDensity,false);

           CheckMap(NewGroundDensity,false);
           CheckMap(NewGroundMax,true,mtIHSReflect);
           CheckMap(NewGroundMin,true,mtIHSReflect);
           CheckMap(NewGroundNearest,true,mtIHSReflect);

           CheckMap(NewOtherDEM,false);
           CheckMap(NewPointIDDEM,false);
           CheckMap(NewUnclassDEM,false);
           CheckMap(NewUserDataDEM,false);
           CheckMap(NewVegDEM,false);
           CheckMap(OverlapDEM,false);
           CheckMap(SecondReturnDensity,false);
           CheckMap(SingleReturnDensity,false);

           CloseSingleDEM(FirstReturnDensityForMean);
           CloseSingleDEM(LastReturnDensity);
        end {procedure MakeMapDisplays};


    begin {MakeGridFromLidarCloud}
       {$If Defined(RecordMakeGrid) or Defined(TrackPointCloud)} WriteLineToDebugFile(''); WriteLineToDebugFile('Tpt_cloud_opts_fm.BitBtn9Click (Make grid) in, ' + IntToStr(ord(PCGridMaker))); {$EndIf}
       ShowHourglassCursor;
       TempDEM := BaseMap.MakeTempGrid;
      {$IfDef RecordMakeGrid} WriteLineToDebugFile('CreateGridToMatchMap done, TempDEM=' + IntToStr(TempDEM) + ' ' + DEMGlb[TempDEM].GridDefinition + ' ' + DEMGlb[TempDEM].DEMSizeString + ' ' + DEMGlb[TempDEM].ColsRowsString); {$EndIf}
      AirReturnDEM := 0;
      BlankDEM := 0;
      ClassDEM := 0;
      DSMDEM := 0;
      MaxIntensityDEM := 0;
      FirstIntensityDEM := 0;
      LastIntensityDEM := 0;
      MaxScanAngleDEM := 0;
      MeanIntensityDEM := 0;
      MeanReturnHeightDEM := 0;
      MeanReturnStdDEM := 0;
      MinIntensityDEM := 0;
      MinScanAngleDEM := 0;
      NewBuildingDEM := 0;
      NewDensity := 0;
      NewDistance := 0;
      FirstReturnDensity := 0;
      FirstReturnDensityForMean := 0;
      LastReturnDensity := 0;
      MeanFirstReturn := 0;
      MeanLastReturn := 0;
      NewGroundDensity := 0;
      NewGroundMax := 0;
      NewGroundMean := 0;
      NewGroundMin := 0;
      NewGroundNearest := 0;
      NewGroundXYZ := 0;
      NewPointIDdem := 0;
      NewRGBGrid := 0;
      NewUnclassDEM := 0;
      NewUserDataDEM := 0;
      NewVegDEM := 0;
      NVSDEM := 0;
      OverlapDEM := 0;
      SecondReturnDensity := 0;
      SingleReturnDensity := 0;

       //CheckEditString(Edit2.Text,zcrit);
       CreateNewGrids;
       CloseSingleDEM(TempDEM);

        if (PCGridMaker <> pcgmBlank) then begin
           StartThreadTimers('Point cloud',1,true);
           for i := 1 to MaxClouds do if UsePC[i] and (LasFiles[i] <> Nil) then OneLasLayer(i);
           EndThreadTimers;
        end;

       {$If Defined(RecordMakeGrid) or Defined(TrackPointCloud)} WriteLineToDebugFile('Grid creation over'); {$EndIf}
       MakeMapDisplays;
       Result := CheckDEM;
       {$If Defined(RecordMakeGrid) or Defined(TrackPointCloud)} WriteLineToDebugFile('MakeGridFromLidarCloud out'); {$EndIf}
    end {MakeGridFromLidarCloud};


begin {MakeLidarGrid}
   Result := MakeGridFromLidarCloud;
end {MakeLidarGrid};


