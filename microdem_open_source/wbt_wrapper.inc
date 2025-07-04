{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{________________________________________}
{                                        }
{   include file for md_use_tools        }
{________________________________________}


function WhiteBoxPresent : boolean;
begin
   Result := FileExists(WhiteBoxfName);
   if Not Result then begin
      WhiteBoxfName := ':\gis_software\wbt_2.4.0\whitebox_tools.exe';
      FindDriveWithFile(WhiteBoxfName);
      Result := FileExists(WhiteBoxfName);
      if Not Result then begin
         Result := GetExistingFileName('whitebox_tools.exe','*.exe',WhiteBoxfName);
         if (Not Result) then MessageToContinue('WhiteBoxTools exe missing');
      end;
   end;
end;


function ExecuteWBandOpenMap(OpenMap : boolean; cmd : ansistring; OutName : PathStr; TheElevUnits : tElevUnit; MapType :tMapType = mtElevRainbow) : integer;
var
   TStr : shortstring;
   NPts : int64;
begin
   {$IfDef RecordWBT} WriteLineToDebugFile('ExecuteWBandOpenMap, cmd=' + cmd); {$EndIf}
   WinExecAndWait32(cmd,true,MDDef.LogDOScommands);
   {$IfDef RecordWBT} WriteLineToDebugFile('WinExecAndWait32 over'); {$EndIf}
   if FileExists(OutName) then begin
      if LoadNewDEM(Result,OutName,false) then begin
         DEMGlb[Result].DEMheader.ElevUnits := TheElevUnits;
         if not PossibleElevationUnits(DEMGlb[Result].DEMheader.ElevUnits) then DEMGlb[Result].DEMHeader.VerticalCSTypeGeoKey := VertCSUndefined;
         if (theElevUnits = euAspectDeg) then DEMGlb[Result].MarkOutsideRangeMissing(0,360,NPts,true);
         if OpenMap then CreateDEMSelectionMap(Result,true,true,MapType);
         UpdateMenusForAllMaps;
         {$If Defined(RecordMapProj)} WriteLineToDebugFile('ExecuteWBandOpenMap ' + DEMGlb[Result].AreaName + '  ' + DEMGlb[Result].DEMMapProj.GetProjName); {$EndIf}
      end;
      {$IfDef RecordWBT} WriteLineToDebugFile('ExecuteWBandOpenMap map opened'); {$EndIf}
   end
   else begin
      TStr := 'Whitebox failure, try command in DOS window: ' + cmd;
      if HeavyDutyProcessing then HighlightLineToDebugFile(TStr)
      else MessageToContinue(TStr);
   end;
   {$IfDef RecordWBT} WriteLineToDebugFile('ExecuteWBandOpenMap out'); {$EndIf}
end;


function WBT_breach_depression(DEMName : PathStr; var BreachName : PathStr) : integer;
//https://www.whiteboxgeo.com/manual/wbt_book/available_tools/hydrological_analysis.html#BreachDepressionsLeastCost
var
   cmd : ansistring;
begin
   if (BreachName = '') then BreachName := MDTempDir + 'wbt_breach_' + ExtractFileNameNoExt(DEMName)  + '.tif';
   if not FileExists(BreachName) then begin
      cmd := WhiteBoxfName + WBNoCompress + ' -r=BreachDepressionsLeastCost -v --dem=' + DEMName + ' -o=' + BreachName + ' --dist=1000 --max_cost=100.0 --flat_increment=0.001 --fill_deps=True';  //--min_dist removed, added last two 4/29/24
      {$IfDef RecordWBT} WriteLineToDebugFile('WBT_breach_depression cmd=' + cmd); {$EndIf}
      WinExecAndWait32(cmd,true,MDDef.LogDOScommands);
   end;
end;


function WBT_FlowAccumulation(OpenMap,Log,D8 : boolean; DEMName : PathStr; var BreachName, FlowAccName : PathStr) : integer;
//https://www.whiteboxgeo.com/manual/wbt_book/available_tools/hydrological_analysis.html#D8FlowAccumulation
var
   cmd : ansistring;
   TStr,TStr2,TStr3,TStr4 : shortstring;
begin
   if WhiteBoxPresent and FileExistsErrorMessage(DEMName) then begin
      Result := 0;
      if not FileExists(BreachName) then WBT_breach_depression(DEMName,BreachName);
      TStr := 'num_cells_';
      if Log then TStr3 := ' --log' else TStr3 := '';
      if Log then TStr4 := 'log_' else TStr4 := '';
      if D8 then TStr2 := 'D8' else TStr2 := 'FD8';
      if (FlowAccName = '') then FlowAccName := MDTempDir + 'wbt_' + TStr4 + 'flow_accumulation_' + TStr2 + '_' + TStr + ExtractFileNameNoExt(DEMName) + '.tif';
      cmd := WhiteBoxfName +  WBNoCompress + '-r=' + TStr2 +'FlowAccumulation -v -dem=' + BreachName + ' -o=' + FlowAccName + ' --out_type=cells' + TStr3;
      Result := ExecuteWBandOpenMap(OpenMap,cmd,FlowAccName,euUndefined,mtElevRainbow);
   end;
end;


function WBT_extract_streams(OpenMap : boolean; DEMName : PathStr; var BreachName,FlowAccumulationName,StreamName : PathStr; Threshhold : float32 = 100.0) : integer;
//  https://www.whiteboxgeo.com/manual/wbt_book/available_tools/stream_network_analysis.html#ExtractStreams
//  -r=ExtractStreams -v --wd="/path/to/data/" --flow_accum='d8accum.tif' -o='output.tif' --threshold=100.0 --zero_background=false
//  could convert to vector file, https://www.whiteboxgeo.com/manual/wbt_book/available_tools/stream_network_analysis.html#rasterstreamstovector
var
   FlowAccName : PathStr;
   cmd : ansistring;
begin
   WBT_FlowAccumulation(false,false,True,DEMName,BreachName,FlowAccName);
   if (StreamName = '') then StreamName := MDTempDir + 'wbt_streams_' + ExtractFileNameNoExt(DEMName) + '.tif';
   if FileExists(StreamName) then begin
      LoadNewDEM(Result,StreamName,OpenMap);
   end
   else begin
      cmd := WhiteBoxfName + WBNoCompress + '-r=ExtractStreams -v --flow_accum=' + FlowAccName + ' -o=' + StreamName + ' --threshold=' + RealToString(Threshhold,-8,1);  // + ' --zero_background=false';
      Result := ExecuteWBandOpenMap(OpenMap,cmd,StreamName,euUndefined,mtElevRainbow);
   end;
end;


function WBT_ElevAboveStream(OpenMap : boolean; DEMName : PathStr; BreachName,FlowAccumulationName,StreamName,HANDName : PathStr; Threshhold : float32 = 100.0) : integer;
// whitebox_tools -r=ElevationAboveStream -v --wd="/path/to/data/" --dem='dem.tif' --streams='streams.tif' -o='output.tif'
// calls extract streams (threshhold 100), which calls flow accumulation (d8, not log), which calls depression breaching (dist=1000, max_cost=100.0)
var
   cmd : ansistring;
begin
   if WhiteBoxPresent and FileExistsErrorMessage(DEMName) then begin
      {$IfDef RecordWBT} WriteLineToDebugFile('WBT_ElevAboveStream in, ' + DEMName); {$EndIf}
      if not FileExists(BreachName) then WBT_breach_depression(DEMName,BreachName);
      WBT_extract_streams(false,DEMName,BreachName,FlowAccumulationName,StreamName,Threshhold);
      if (HANDName = '') then HandName := MDTempDir + 'wbt_elev_above_stream_' + ExtractFileNameNoExt(DEMName) + '.tif';
      cmd := WhiteBoxfName + WBNoCompress + '-r=ElevationAboveStream -v --dem=' + BreachName + ' --streams=' + StreamName + ' -o=' + HANDName;
      Result := ExecuteWBandOpenMap(OpenMap,cmd,HandName,euUndefined,mtElevRainbow);
      {$IfDef RecordWBT} WriteLineToDebugFile('WBT_ElevAboveStream out, ' + HandName); {$EndIf}
   end;
end;


function WBT_WetnessIndex(OpenMap,D8 : boolean; DEMName : PathStr; WetnessName : PathStr = '') : integer;
//whitebox_tools -r=WetnessIndex -v --wd="/path/to/data/" --sca='flow_accum.tif' --slope='slope.tif' -o=output.tif
var
   SlopeName,FlowAccName,BreachName : PathStr;
   cmd : ansistring;
begin
   if WhiteBoxPresent and FileExistsErrorMessage(DEMName) then begin
      {$IfDef RecordWBT} WriteLineToDebugFile('WBT_WetnessIndex in, ' + DEMName); {$EndIf}
      FlowAccName := '';
      BreachName := '';
      WBT_FlowAccumulation(false,D8,false,DEMName,BreachName,FlowAccName);

      SlopeName := MDTempDir + 'wbt_slope_deg_' + ExtractFileNameNoExt(DEMName) + '.tif';
      cmd := WhiteBoxfName + WBNoCompress + '-r=Slope -v --dem=' + BreachName + ' -o=' + SlopeName + ' --units="degrees"';
      WinExecAndWait32(cmd,true,MDDef.LogDOScommands);

      if (WetnessName = '') then WetnessName := MDTempDir + 'wbt_wetness_index_' + ExtractFileNameNoExt(DEMName) + '.tif';
      cmd := WhiteBoxfName + WBNoCompress + '-r=WetnessIndex -v --sca=' + FlowAccName + ' --slope=' + SlopeName + ' -o=' + WetnessName + ' --units="degrees"';
      Result := ExecuteWBandOpenMap(OpenMap,cmd,WetnessName,euUndefined,mtElevRainbow);
      {$IfDef RecordWBT} WriteLineToDebugFile('WBT_WetnessIndex out, ' + WetnessName); {$EndIf}
   end;
end;


function WBT_GroundClassify(InName,OutName : PathStr) : shortString;
begin
   Result := WhiteBoxfName + ' -r=LidarGroundPointFilter -v -i=' + InName + ' -o=' + OutName + ' --radius=' + RealToString(MDDef.WBGroundClassRadius,-12,1);
end;


function WBT_LidarSegmentationBasedFilter(InName,OutName : PathStr) : shortString;
begin
   Result := WhiteBoxfName + ' -r=LidarSegmentationBasedFilter -v -i=' + InName + ' -o=' + OutName + ' --radius=' + RealToString(MDDef.WBSegFilterRadius,-12,1);
end;


function WBT_DeNoise(InName,OutName : PathStr) : shortString;
begin
   Result := WhiteBoxfName + WBNoCompress + '-r=LidarRemoveOutliers -v -i=' + InName + ' -o=' + OutName + ' --radius=' + RealToString(MDDef.WBDeNoiseRadius,-12,1) + ' --elev_diff=' + RealToString(MDDef.WBDeNoiseElevDiff,-12,1);
end;


procedure WBT_GridFillMissingData(InName : PathStr; TheElevUnits : tElevUnit; OutName : PathStr = '');
var
   cmd : ansistring;
begin
   if WhiteBoxPresent and FileExistsErrorMessage(InName) then begin
      if (OutName = '') then OutName := MDTempDir + 'wbt_fill_holes_' + ExtractFileNameNoExt(InName) + '.tif';
      cmd := WhiteBoxfName +  WBNoCompress + '-r=FillMissingData -v -i=' + InName + ' -o=' + OutName + ' --filter=' + IntToStr(MDdef.FillHoleRadius);
      ExecuteWBandOpenMap(False,cmd,OutName,TheElevUnits);
   end;
end;


function WBT_SlopeMap(OpenMap : boolean; InName : PathStr; OutName : PathStr = '') : integer;
var
   cmd : ansistring;
begin
  if WhiteBoxPresent and FileExistsErrorMessage(InName) then begin
     if (OutName = '') then OutName := MDTempDir + 'wbt_slope_percent_' + ExtractFileNameNoExt(InName) + '.tif';
     cmd := WhiteBoxfName + WBNoCompress + '-r=Slope -v --dem=' + InName + ' -o=' + OutName + ' --units="percent"';
     Result := ExecuteWBandOpenMap(OpenMap,cmd,OutName,euPercentSlope,MDDef.DefSlopeMap);
  end;
end;



function WBT_MultidirectionalHillshadeMap(OpenMap : boolean; DEM : integer; OutName : PathStr = '') : integer;
//https://www.whiteboxgeo.com/manual/wbt_book/available_tools/geomorphometric_analysis.html#Hillshade
var
   cmd : ansistring;
begin
  if WhiteBoxPresent then begin
     if Outname = '' then OutName := MDTempDir + 'wbt_multi_hillshade_' + DEMGlb[DEM].AreaName + '.tif';
//     >>./whitebox_tools -r=Hillshade -v --wd="/path/to/data/" -i=DEM.tif -o=output.tif --azimuth=315.0 --altitude=30.0
     cmd := WhiteBoxfName + WBNoCompress + '-r=MultidirectionalHillshade -v -i=' + DEMGlb[DEM].GeotiffDEMName + ' -o=' + OutName +
         ' --altitude=30' + ' --full_mode';
     Result := ExecuteWBandOpenMap(OpenMap,cmd,OutName,euUndefined,mtElevGray);
  end;
end;


function WBT_HillshadeMap(OpenMap : boolean; DEM : integer; OutName : PathStr = '') : integer;
//https://www.whiteboxgeo.com/manual/wbt_book/available_tools/geomorphometric_analysis.html#Hillshade
var
   cmd : ansistring;
begin
  if WhiteBoxPresent then begin
     if Outname = '' then OutName := MDTempDir + 'wbt_hillshade_' + DEMGlb[DEM].AreaName + '.tif';
//     >>./whitebox_tools -r=Hillshade -v --wd="/path/to/data/" -i=DEM.tif -o=output.tif --azimuth=315.0 --altitude=30.0
     cmd := WhiteBoxfName + WBNoCompress + '-r=Hillshade -v -i=' + DEMGlb[DEM].GeotiffDEMName + ' -o=' + OutName + ' --azimuth=' + RealToString(MDdef.RefPhi,-8,1) + ' --altitude=' + RealToString(90-MDdef.RefTheta,-8,1);
     Result := ExecuteWBandOpenMap(OpenMap,cmd,OutName,euUndefined,mtElevGray);
  end;
end;


function WBT_TRI(OpenMap : boolean; InName : PathStr; OutName : PathStr = '') : integer;
var
   cmd : ansistring;
begin
  if WhiteBoxPresent and FileExistsErrorMessage(InName) then begin
     if (OutName = '') then OutName := MDTempDir + 'wbt_TRI_' + ExtractFileNameNoExt(InName) + '.tif';
     cmd := WhiteBoxfName + WBNoCompress + '-r=RuggednessIndex -v --dem=' + InName + ' -o=' + OutName;
     Result := ExecuteWBandOpenMap(Openmap,cmd,OutName,euMeters,mtElevRainbow);
  end;
end;

function WBT_AvgNormVectAngDev(OpenMap : boolean; InName : PathStr; filtersize : integer) : integer;
var
   cmd : ansistring;
   OutName : PathStr;
begin
  if WhiteBoxPresent and FileExistsErrorMessage(InName) then begin
     OutName := MDTempDir + 'wbt_avg_norm_vect_dev_' + FilterSizeStr(FilterSize) + '_' + ExtractFileNameNoExt(InName) + '.tif';
     cmd := WhiteBoxfName + WBNoCompress + '-r=AverageNormalVectorAngularDeviation -v --dem=' + InName + ' -o=' + OutName + ' --filter=' + IntToStr(FilterSize);
     Result := ExecuteWBandOpenMap(OpenMap,cmd,OutName,euDegrees,mtElevSpectrum);
  end;
end;

function WBT_CircularVarianceOfAspect(OpenMap : boolean; InName : PathStr; filtersize : integer) : integer;
var
   cmd : ansistring;
   OutName : PathStr;
begin
  if WhiteBoxPresent and FileExistsErrorMessage(InName) then begin
     OutName := MDTempDir + 'wbt_circ_var_aspect_' + FilterSizeStr(FilterSize) + '_' + ExtractFileNameNoExt(InName) + '.tif';
     cmd := WhiteBoxfName + WBNoCompress + '-r=CircularVarianceOfAspect -v --dem=' + InName + ' -o=' + OutName + ' --filter=' + IntToStr(FilterSize);
     Result := ExecuteWBandOpenMap(Openmap,cmd,OutName,euUndefined,mtElevSpectrum);
  end;
end;


function WBT_DrainageBasins(InName : PathStr) : integer;
//   2/2/2024, this produces way too many basins with both tools; size does not make much difference
var
   cmd : ansistring;
   IntName,BasinName,BreachName,IsobasinName : PathStr;
begin
   if WhiteBoxPresent and FileExistsErrorMessage(InName) then begin
      BreachName := MDTempDir + 'wbt_breach.tif';
      IntName := MDTempDir + 'wbt_d8.tif';
      BasinName := MDTempDir + 'wbt_basins.tif';
      IsoBasinName := MDTempDir + 'wbt_isobasins.tif';

      cmd := WhiteBoxfName + WBNoCompress + ' -r=BreachDepressionsLeastCost -v --dem=' + InName + ' -o=' + BreachName + ' --dist=1000 --max_cost=100.0 --min_dist';
      WinExecAndWait32(cmd,true,MDDef.LogDOScommands);

      cmd := WhiteBoxfName + WBNoCompress + ' -r=D8Pointer -v --dem=' + InName + ' -o=' + IntName;
      WinExecAndWait32(cmd,true,MDDef.LogDOScommands);

      cmd := WhiteBoxfName + WBNoCompress + ' -r=Basins -v --d8_pntr=' + IntName + ' -o=' + BasinName;
      Result := ExecuteWBandOpenMap(True,cmd,BasinName,euUndefined,mtElevSpectrum);

      cmd := WhiteBoxfName + WBNoCompress + ' -r=Isobasins -v --dem=' + BreachName + ' -o=' + IsobasinName + ' --size=100000';
      Result := ExecuteWBandOpenMap(True,cmd,ISOBasinName,euUndefined,mtElevSpectrum);
   end;
end;


procedure WBT_MultiscaleRoughness(InName : PathStr);
var
   cmd : ansistring;
   NewGrid : integer;
   OutName1,OutName2 : PathStr;
begin
  if WhiteBoxPresent and FileExistsErrorMessage(InName) then begin
     OutName1 := MDTempDir + 'wbt_ms_rough_mag_' + ExtractFileNameNoExt(InName) + '.tif';
     OutName2 := MDTempDir + 'wbt_ms_rough_scale_' + ExtractFileNameNoExt(InName) + '.tif';
     cmd := WhiteBoxfName  + WBNoCompress + '-r=MultiScaleRoughness -v --dem=' + InName + ' -out_mag=' + OutName1 + ' -out_scale=' + OutName2 + ' --min_scale=1 --max_scale=1000 --step=5';
     ExecuteWBandOpenMap(True,cmd,OutName1,euMeters,mtElevSpectrum);
     if FileExists(OutName2) then begin
        NewGrid := OpenNewDEM(OutName2);
        {$IfDef RecordWBT} WriteLineToDebugFile('second map opened'); {$EndIf}
     end;
  end;
end;


function WBT_AspectMap(OpenMap : boolean; InName : PathStr; OutName : PathStr = '') : integer;
var
   cmd : ansistring;
begin
  if WhiteBoxPresent and FileExistsErrorMessage(InName) then begin
     if (OutName = '') then OutName := MDTempDir + 'wbt_aspect_' + ExtractFileNameNoExt(InName) + '.tif';
     cmd := WhiteBoxfName + WBNoCompress + '-r=Aspect -v --dem=' + InName + ' -o=' + OutName;   // + ' --units="percent"';
     Result := ExecuteWBandOpenMap(Openmap,cmd,OutName,euAspectDeg,mtDEMAspect);
  end;
end;


procedure WBT_GeotiffMetadata(InName : PathStr);
var
   cmd : ansistring;
   OutName : PathStr;
   bFile : tStringList;
begin
  if WhiteBoxPresent and FileExistsErrorMessage(InName) then begin
     OutName := MDTempDir + 'wbt-metadata_' + ExtractFileNameNoExt(InName) + '.txt';
     cmd := WhiteBoxfName + ' -r=PrintGeoTiffTags -v  --input=' + InName + ' >' + OutName;
     bfile := tStringList.Create;
     bfile.Add(cmd);
     EndBatchFile(MDTempDir + 'wbt_gt_meta.bat',bfile);
     ShowInNotepadPlusPlus(OutName,ExtractFileName(OutName));
  end;
end;


function WBT_MakeDTM(Tool : shortstring; OpenMap : boolean; InDirectory,OutName : PathStr; GridSize : float64; AssignProjection : shortstring = '') : integer;
var
   cmd : ansistring;
   FilesWanted : tStringList;
   i,NewDEM : integer;
   fName : PathStr;
begin
  {$IfDef RecordUseOtherPrograms} WriteLineToDebugFile('WBNearNeighCreate, infile=' + InName + '  outfile=' + OutName); {$EndIf}
  if WhiteBoxPresent then begin
      SetColorForProcessing;
      MDDef.ShowWinExec := true;
      cmd := WhiteBoxfName + ' -r=' + Tool + ' ' + WBNoCompress + ' -v --wd=' + InDirectory + ' --resolution=' + RealToString(GridSize,-12,-1) + ' --radius=' + RealToString(GridSize * MDDef.FillHoleRadius,-12,-1) +
         ' --exclude_cls=0,1,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18';
     {$IfDef RecordWBT_DEM} WriteLineToDebugFile('cmd=' + cmd); {$EndIf}
     if (OutName = '') then WinExecAndWait32(cmd,false,MDDef.LogDOScommands)
     else begin
        WinExecAndWait32(cmd,true,MDDef.LogDOScommands);
        MDDef.ShowWinExec := false;
        FilesWanted := tStringList.Create;
        FindMatchingFiles(InDirectory,'*.tif',FilesWanted,0);
        if (AssignProjection <> '') then begin
            {$IfDef RecordWBT_DEM} WriteLineToDebugFile('assign projection'); {$EndIf}
            for i := 0 to pred(FIlesWanted.Count) do begin
               wmdem.SetPanelText(2,IntToStr(i) + '/' + IntToStr(FilesWanted.Count));
               fName := FilesWanted.Strings[i];
               GDALAssignProjectionViaWKTorEPSG(fName,'EPSG:' + AssignProjection);
            end;
        end;
        {$IfDef RecordWBT_DEM} WriteLineToDebugFile('call merge'); {$EndIf}
        NewDEM := MergeMultipleDEMsHere(FilesWanted,true,false);
        DEMGlb[NewDEM].WriteNewFormatDEM(OutName);
     end;
  end;
  SetColorForWaiting;
  wmdem.SetPanelText(2,'');
  MDDef.ShowWinExec := false;
  {$IfDef RecordWBT_DEM} WriteLineToDebugFile('WBT_MakeDTM out'); {$EndIf}
end;


function WBT_BNearNeighCreate(OpenMap : boolean; InDirectory,OutName : PathStr; GridSize : float64; AssignProjection : shortstring = '') : integer;
begin
  {$IfDef RecordWBT_DEM} WriteLineToDebugFile('WBT_BNearNeighCreate in'); {$EndIf}
   Result := WBT_MakeDTM('LidarNearestNeighbourGridding',OpenMap,InDirectory,OutName,GridSize,AssignProjection)
end;

function WBT_IDWCreate(OpenMap : boolean; InDirectory,OutName : PathStr; GridSize : float64; AssignProjection : shortstring = '') : integer;
begin
  {$IfDef RecordWBT_DEM} WriteLineToDebugFile('WBT_IDWCreate in'); {$EndIf}
  Result := WBT_MakeDTM('LidarIdwInterpolation ',OpenMap,InDirectory,OutName,GridSize,AssignProjection)
end;


function WBT_ProfileCurvature(OpenMap : boolean; InName : PathStr; OutName : PathStr = '') : integer;
var
   cmd : ansistring;
begin
  if WhiteBoxPresent and FileExistsErrorMessage(InName) then begin
     if OutName = '' then OutName := MDTempDir + 'wbt_profile_curvature_' + ExtractFileNameNoExt(InName) + '.tif';
     cmd := WhiteBoxfName + WBNoCompress + '-r=ProfileCurvature -v --dem=' + InName + ' -o=' + OutName;
     Result := ExecuteWBandOpenMap(OpenMap,cmd,OutName,euPerMeter,MDDef.DefCurveMap);
  end;
end;

function WBT_PlanCurvature(OpenMap : boolean; InName : PathStr; OutName : PathStr = '') : integer;
var
   cmd : ansistring;
begin
  if WhiteBoxPresent and FileExistsErrorMessage(InName) then begin
     if OutName = '' then OutName := MDTempDir + 'wbt_profile_curvature_' + ExtractFileNameNoExt(InName) + '.tif';
     cmd := WhiteBoxfName + WBNoCompress + '-r=PlanCurvature -v --dem=' + InName + ' -o=' + OutName;
     Result := ExecuteWBandOpenMap(OpenMap,cmd,OutName,euPerMeter,MDDef.DefCurveMap);
  end;
end;

function WBT_TangentialCurvature(OpenMap : boolean; InName : PathStr; OutName : PathStr = '') : integer;
var
   cmd : ansistring;
begin
  if WhiteBoxPresent and FileExistsErrorMessage(InName) then begin
     if (OutName = '') then OutName := MDTempDir + 'wbt_tangential_curvature_' + ExtractFileNameNoExt(InName) + '.tif';
     cmd := WhiteBoxfName + WBNoCompress + '-r=TangentialCurvature -v --dem=' + InName + ' -o=' + OutName;
     Result := ExecuteWBandOpenMap(OpenMap,cmd,OutName,euPerMeter,MDDef.DefCurveMap);
  end;
end;


function WBT_MinimalCurvature(OpenMap : boolean; InName : PathStr; OutName : PathStr = '') : integer;
var
   cmd : ansistring;
begin
  if WhiteBoxPresent and FileExistsErrorMessage(InName) then begin
     if (OutName = '') then OutName := MDTempDir + 'wbt_minimal_curvature_' + ExtractFileNameNoExt(InName) + '.tif';
     cmd := WhiteBoxfName + WBNoCompress + '-r=MinimalCurvature -v --dem=' + InName + ' -o=' + OutName;
     Result := ExecuteWBandOpenMap(OpenMap,cmd,OutName,euPerMeter,MDDef.DefCurveMap);
  end;
end;


function WBT_MaximalCurvature(OpenMap : boolean; InName : PathStr; OutName : PathStr = '') : integer;
var
   cmd : ansistring;
begin
  if WhiteBoxPresent and FileExistsErrorMessage(InName) then begin
     if (OutName = '') then OutName := MDTempDir + 'wbt_maximal_curvature_' + ExtractFileNameNoExt(InName) + '.tif';
     cmd := WhiteBoxfName + WBNoCompress + '-r=MaximalCurvature -v --dem=' + InName + ' -o=' + OutName;
     Result := ExecuteWBandOpenMap(OpenMap,cmd,OutName,euPerMeter,MDDef.DefCurveMap);
  end;
end;


function WBT_MeanCurvature(OpenMap : boolean; InName : PathStr; OutName : PathStr = '') : integer;
var
   cmd : ansistring;
begin
  if WhiteBoxPresent and FileExistsErrorMessage(InName) then begin
     if (OutName = '') then OutName := MDTempDir + 'wbt_mean_curvature_' + ExtractFileNameNoExt(InName) + '.tif';
     cmd := WhiteBoxfName + WBNoCompress + '-r=MeanCurvature -v --dem=' + InName + ' -o=' + OutName;
     Result := ExecuteWBandOpenMap(OpenMap,cmd,OutName,euPerMeter,MDDef.DefCurveMap);
  end;
end;


function WBT_GaussianCurvature(OpenMap : boolean; InName : PathStr; OutName : PathStr = '') : integer;
var
   cmd : ansistring;
begin
  if WhiteBoxPresent and FileExistsErrorMessage(InName) then begin
     if (OutName = '') then OutName := MDTempDir + 'wbt_gaussian_curvature_' + ExtractFileNameNoExt(InName) + '.tif';
     cmd := WhiteBoxfName + WBNoCompress + '-r=GaussianCurvature -v --dem=' + InName + ' -o=' + OutName;
     Result := ExecuteWBandOpenMap(OpenMap,cmd,OutName,euPerMeter,MDDef.DefCurveMap);
  end;
end;


function WBT_FeaturePreserveSmooth(OpenMap : boolean; InName : PathStr; zUnits : byte; OutName : PathStr = '') : integer;
var
   cmd : ansistring;
begin
   if (OutName = '') then OutName := MDtempDir + 'wbt_FeaturePreserveSmooth_' + ExtractFileNameNoExt(InName) + '.tif';
   cmd := WhiteBoxfName + ' -r=FeaturePreservingSmoothing -v -i=' + InName + ' -o=' + OutName;
   Result := ExecuteWBandOpenMap(OpenMap,cmd,OutName,zUnits,MDDef.DefElevMap);
end;

procedure WBT_PennockLandformClass(InName : PathStr; SmoothFirst : boolean);
var
   tName,OutName : PathStr;
   bfile : tStringList;
begin
  {$IfDef RecordUseOtherPrograms} WriteLineToDebugFile('WhiteBoxPennockLandformClass, infile=' + InName + '  outfile=' + OutName); {$EndIf}
  if WhiteBoxPresent and FileExistsErrorMessage(InName) then begin
     OutName := NextFileNumber(MDTempDir,'wbt_pennock_' + ExtractFileNameNoExt(InName) + '_','.tif');
     bfile := tStringList.Create;
     if SmoothFirst then begin
        tName := MDtempDir + 'wbt_FeaturePreserveSmooth_' + ExtractFileNameNoExt(InName) + '.tif';
        bfile.Add(WhiteBoxfName + ' -r=FeaturePreservingSmoothing -v -i=' + InName + ' -o=' + tName);
        inName := tName;
     end;
     bfile.Add(WhiteBoxfName  + WBNoCompress + '-r=PennockLandformClass -v -i=' + InName + ' -o=' + OutName);
     EndBatchFile(MDTempDir + 'wb_pennock.bat',bfile);
     {$IfDef RecordUseOtherPrograms} WriteLineToDebugFile('open outfile=' + OutName); {$EndIf}
     OpenNewDEM(OutName);
  end;
  {$IfDef RecordUseOtherPrograms} WriteLineToDebugFile('WhiteBoxPennockLandformClass out'); {$EndIf}
end;



procedure WBT_KappaIndex(ClassifiedName,ReferenceName : PathStr; HTMLname : PathStr = '');
// https://www.whiteboxgeo.com/manual/wbt_book/available_tools/mathand_stats_tools.html#KappaIndex
var
   cmd : shortstring;
begin
  if WhiteBoxPresent and FileExistsErrorMessage(ClassifiedName) and FileExistsErrorMessage(ReferenceName) then begin
     if (HTMLName = '') then HTMLname :=  NextFileNumber(MDtempDir,'wbt_kappa_index_' + ExtractFileNameNoExt(ClassifiedName) + '_', '.html');
     cmd := WhiteBoxfName + ' -r=KappaIndex -v --i1=' + ClassifiedName + ' --i2=' + ReferenceName + ' -o=' + HTMLName;
     WriteLineToDebugFile(cmd);
     WinExecAndWait32(cmd,true,MDDef.LogDOScommands);
  end;
end;


function WBT_Geomorphons(OpenMap : boolean; InName : PathStr; Search : integer=50; Skip : integer = 0) : integer;
// https://www.whiteboxgeo.com/manual/wbt_book/available_tools/geomorphometric_analysis.html#Geomorphons
var
   OutName : PathStr;
   cmd : ansistring;
begin
  {$If Defined(RecordUseOtherPrograms) or Defined(RecordWBT)} WriteLineToDebugFile('WhiteBoxGeomorphons, infile=' + InName + '  outfile=' + OutName); {$EndIf}
  if WhiteBoxPresent and FileExistsErrorMessage(InName) then begin
     OutName := NextFileNumber(MDTempDir,'wbt_geomorphons_search_' + IntToStr(Search) + '_skip_' + IntToStr(Skip) + '_' + ExtractFileNameNoExt(InName) + '_','.tif');
     cmd := WhiteBoxfName  + WBNoCompress + ' -r=Geomorphons -v -i=' + InName + ' -o=' + OutName + ' --search=' + IntToStr(Search) + ' --threshold=0.0 --skip=' + IntToStr(Skip) + ' --forms=True';
     Result := ExecuteWBandOpenMap(OpenMap,cmd,OutName,euGeomorphon,mtLandCover);
  end;
   {$If Defined(RecordUseOtherPrograms) or Defined(RecordWBT)} WriteLineToDebugFile('WhiteBoxGeomorphons out'); {$EndIf}
end;



