{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}
{                                   }
{   include file for md_use_tools   }
{___________________________________}



function WhiteBoxPresent : boolean;
begin
   if (WhiteBoxfName) = '' then WhiteBoxfName := ProgramRootDir + 'wbt\whitebox_tools.exe';
   Result := FileExists(WhiteBoxfName);
   if Not Result then begin
      Result := GetExistingFileName('whitebox_tools.exe','*.exe',WhiteBoxfName);
      if (Not Result) then MessageToContinue('WhiteBoxTools exe missing');
   end;
end;


function ExecuteWBandOpenMap(cmd : ansistring; OutName : PathStr; TheElevUnits : tElevUnit; MapType :tMapType = mtElevRainbow; OpenMap : boolean = false) : integer;
begin
   {$IfDef RecordWBT} WriteLineToDebugFile('ExecuteWBandOpenMap, cmd=' + cmd); {$EndIf}
   WinExecAndWait32(cmd,true,MDDef.LogDOScommands);
   {$IfDef RecordWBT} WriteLineToDebugFile('WinExecAndWait32 over'); {$EndIf}
   if FileExists(OutName) then begin
      if LoadNewDEM(Result,OutName,false) then begin
         DEMGlb[Result].DEMheader.ElevUnits := TheElevUnits;
         if OpenMap then CreateDEMSelectionMap(Result,true,true,MapType);
      end;
      {$IfDef RecordWBT} WriteLineToDebugFile('ExecuteWBandOpenMap map opened'); {$EndIf}
   end
   else MessageToContinue('Whitebox failure, try command in DOS window: ' + cmd);
   {$IfDef RecordWBT} WriteLineToDebugFile('ExecuteWBandOpenMap out'); {$EndIf}
end;


function WBT_breach_depression(InName : PathStr; var BreachName : PathStr) : integer;
var
   cmd : ansistring;
begin
   BreachName := MDTempDir + 'wbt_breach_' + ExtractFileNameNoExt(InName)  + '.tif';
   if not FileExists(BreachName) then begin
      cmd := WhiteBoxfName + WBNoCompress + ' -r=BreachDepressionsLeastCost -v --dem=' + InName + ' -o=' + BreachName + ' --dist=1000 --max_cost=100.0 --min_dist';
      WinExecAndWait32(cmd,true,MDDef.LogDOScommands);
   end;
end;


function WBT_FlowAccumulation(OpenMap,Log,D8 : boolean; InName : PathStr; var FlowAccName : PathStr) : integer;
var
   BreachName : PathStr;
   cmd : ansistring;
   TStr,TStr2,TStr3 : shortstring;
begin
   if WhiteBoxPresent and FileExistsErrorMessage(InName) then begin
      BreachName := '';
      WBT_breach_depression(InName,BreachName);
      if FileExists(BreachName) then begin
         TStr := 'num_cells_';
         if Log then TStr3 := ' --log' else TStr3 := '';
         if D8 then TStr2 := 'D8' else TStr2 := 'FD8';
         if (FlowAccName = '') then FlowAccName := MDTempDir + 'wbt_flow_accumulation_' + TStr2 + '_' + TStr + ExtractFileNameNoExt(InName) + '.tif';
         cmd := WhiteBoxfName +  WBNoCompress + '-r=' + TStr2 +'FlowAccumulation -v -dem=' + BreachName + ' -o=' + FlowAccName + ' --out_type=cells' + TStr3;
         Result := ExecuteWBandOpenMap(cmd,FlowAccName,euUndefined,mtElevRainbow,OpenMap);
      end
      else begin
         Result := 0;
      end;
   end;
end;


function WBT_extract_streams(OpenMap : boolean; InName : PathStr; var StreamName : PathStr) : integer;
// whitebox_tools -r=ExtractStreams -v --wd="/path/to/data/" --flow_accum='d8accum.tif' -o='output.tif' --threshold=100.0 --zero_background=false
var
   FlowAccName : PathStr;
   cmd : ansistring;
begin
   FlowAccName := '';
   //function WBT_FlowAccumulation(OpenMap,Log,D8 : boolean; InName : PathStr; var FlowAccName : PathStr) : integer;
   WBT_FlowAccumulation(false,false,True,InName,FlowAccName);
   StreamName := MDTempDir + 'wbt_streams_' + ExtractFileNameNoExt(InName) + '.tif';
   if FileExists(StreamName) then begin
      LoadNewDEM(Result,StreamName,OpenMap);
   end
   else begin
      cmd := WhiteBoxfName + WBNoCompress + '-r=ExtractStreams -v --flow_accum=' + FlowAccName + ' -o=' + StreamName + ' --threshold=100.0 --zero_background=false';
      //WinExecAndWait32(cmd,true,MDDef.LogDOScommands);
      Result := ExecuteWBandOpenMap(cmd,StreamName,euUndefined,mtElevRainbow,OpenMap);
   end;
end;


function WBT_ElevAboveStream(OpenMap : boolean; InName : PathStr; OutName : PathStr = '') : integer;
// whitebox_tools -r=ElevationAboveStream -v --wd="/path/to/data/" --dem='dem.tif' --streams='streams.tif' -o='output.tif'
var
   StreamName : PathStr;
   cmd : ansistring;
begin
   if WhiteBoxPresent and FileExistsErrorMessage(InName) then begin
      {$IfDef RecordWBT} WriteLineToDebugFile('WBT_ElevAboveStream in, ' + InName); {$EndIf}

      StreamName := '';
      WBT_extract_streams(false,InName,StreamName);

      if (OutName = '') then Outname := MDTempDir + 'wbt_elev_above_stream_' + ExtractFileNameNoExt(InName) + '.tif';
      cmd := WhiteBoxfName + WBNoCompress + '-r=ElevationAboveStream -v --dem=' + InName + ' --streams=' + StreamName + ' -o=' + OutName;
      Result := ExecuteWBandOpenMap(cmd,OutName,euUndefined,mtElevRainbow,OpenMap);
      {$IfDef RecordWBT} WriteLineToDebugFile('WBT_ElevAboveStream out, ' + OutName); {$EndIf}
   end;
end;


function WBT_WetnessIndex(OpenMap,D8 : boolean; InName : PathStr; OutName : PathStr = '') : integer;
//whitebox_tools -r=WetnessIndex -v --wd="/path/to/data/" --sca='flow_accum.tif' --slope='slope.tif' -o=output.tif
var
   SlopeName,{BreachName,}FlowAccName : PathStr;
   cmd : ansistring;
begin
   if WhiteBoxPresent and FileExistsErrorMessage(InName) then begin
      {$IfDef RecordWBT} WriteLineToDebugFile('WBT_WetnessIndex in, ' + InName); {$EndIf}
      //BreachName := '';
      //WBT_breach_depression(InName,BreachName);

      FlowAccName := '';
      WBT_FlowAccumulation(false,D8,false,InName,FlowAccName);

      SlopeName := MDTempDir + 'wbt_slope_deg_' + ExtractFileNameNoExt(InName) + '.tif';
      cmd := WhiteBoxfName + WBNoCompress + '-r=Slope -v --dem=' + InName + ' -o=' + SlopeName + ' --units="degrees"';
      WinExecAndWait32(cmd,true,MDDef.LogDOScommands);

      if (OutName = '') then Outname := MDTempDir + 'wbt_wetness_index_' + ExtractFileNameNoExt(InName) + '.tif';
      cmd := WhiteBoxfName + WBNoCompress + '-r=WetnessIndex -v --sca=' + FlowAccName + ' --slope=' + SlopeName + ' -o=' + OutName + ' --units="degrees"';
      Result := ExecuteWBandOpenMap(cmd,OutName,euUndefined,mtElevRainbow,OpenMap);
      {$IfDef RecordWBT} WriteLineToDebugFile('WBT_WetnessIndex out, ' + OutName); {$EndIf}
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


procedure WBT_GridFillMissingData(InName : PathStr; TheElevUnits : tElevUnit);
var
   OutName : PathStr;
   cmd : ansistring;
begin
   if WhiteBoxPresent and FileExistsErrorMessage(InName) then begin
      OutName := MDTempDir + 'wbt_fill_holes_' + ExtractFileNameNoExt(InName) + '.tif';
      cmd := WhiteBoxfName +  WBNoCompress + '-r=FillMissingData -v -i=' + InName + ' -o=' + OutName + ' --filter=' + IntToStr(MDdef.FillHoleRadius);
      ExecuteWBandOpenMap(cmd,OutName,TheElevUnits);
   end;
end;


function WBT_SlopeMap(InName : PathStr) : integer;
var
   cmd : ansistring;
   OutName : PathStr;
begin
  if WhiteBoxPresent and FileExistsErrorMessage(InName) then begin
     OutName := MDTempDir + 'wbt_slope_percent_' + ExtractFileNameNoExt(InName) + '.tif';
     cmd := WhiteBoxfName + WBNoCompress + '-r=Slope -v --dem=' + InName + ' -o=' + OutName + ' --units="percent"';
     Result := ExecuteWBandOpenMap(cmd,OutName,euPercentSlope);
  end;
end;


function WBT_TRI(InName : PathStr) : integer;
var
   cmd : ansistring;
   OutName : PathStr;
begin
  if WhiteBoxPresent and FileExistsErrorMessage(InName) then begin
     OutName := MDTempDir + 'wbt_TRI_' + ExtractFileNameNoExt(InName) + '.tif';
     cmd := WhiteBoxfName + WBNoCompress + '-r=RuggednessIndex -v --dem=' + InName + ' -o=' + OutName;
     Result := ExecuteWBandOpenMap(cmd,OutName,euMeters);
  end;
end;

function WBT_AvgNormVectAngDev(InName : PathStr; filtersize : integer) : integer;
var
   cmd : ansistring;
   OutName : PathStr;
begin
  if WhiteBoxPresent and FileExistsErrorMessage(InName) then begin
     OutName := MDTempDir + 'wbt_avg_norm_vect_dev_' + FilterSizeStr(FilterSize) + '_' + ExtractFileNameNoExt(InName) + '.tif';
     cmd := WhiteBoxfName + WBNoCompress + '-r=AverageNormalVectorAngularDeviation -v --dem=' + InName + ' -o=' + OutName + ' --filter=' + IntToStr(FilterSize);
     Result := ExecuteWBandOpenMap(cmd,OutName,euzDegrees,mtElevSpectrum);
  end;
end;

function WBT_CircularVarianceOfAspect(InName : PathStr; filtersize : integer) : integer;
var
   cmd : ansistring;
   OutName : PathStr;
begin
  if WhiteBoxPresent and FileExistsErrorMessage(InName) then begin
     OutName := MDTempDir + 'wbt_circ_var_aspect_' + FilterSizeStr(FilterSize) + '_' + ExtractFileNameNoExt(InName) + '.tif';
     cmd := WhiteBoxfName + WBNoCompress + '-r=CircularVarianceOfAspect -v --dem=' + InName + ' -o=' + OutName + ' --filter=' + IntToStr(FilterSize);
     Result := ExecuteWBandOpenMap(cmd,OutName,euUndefined,mtElevSpectrum);
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
      Result := ExecuteWBandOpenMap(cmd,BasinName,euUndefined,mtElevSpectrum);

      cmd := WhiteBoxfName + WBNoCompress + ' -r=Isobasins -v --dem=' + BreachName + ' -o=' + IsobasinName + ' --size=100000';
      Result := ExecuteWBandOpenMap(cmd,ISOBasinName,euUndefined,mtElevSpectrum);
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
     ExecuteWBandOpenMap(cmd,OutName1,euMeters);
     if FileExists(OutName2) then begin
        NewGrid := OpenNewDEM(OutName2);
        {$IfDef RecordWBT} WriteLineToDebugFile('second map opened'); {$EndIf}
     end;
  end;
end;


procedure WBT_AspectMap(InName : PathStr);
var
   cmd : ansistring;
   OutName : PathStr;
begin
  if WhiteBoxPresent and FileExistsErrorMessage(InName) then begin
     OutName := MDTempDir + 'wbt_aspect_' + ExtractFileNameNoExt(InName) + '.tif';
     cmd := WhiteBoxfName + WBNoCompress + '-r=Aspect -v --dem=' + InName + ' -o=' + OutName;   // + ' --units="percent"';
     ExecuteWBandOpenMap(cmd,OutName,euAspectDeg);
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


procedure WBT_BNearNeighCreate(InName,OutName : PathStr; GridSize : float64);
var
   cmd : ansistring;
begin
  {$IfDef RecordUseOtherPrograms} WriteLineToDebugFile('WBNearNeighCreate, infile=' + InName + '  outfile=' + OutName); {$EndIf}
  if WhiteBoxPresent and FileExistsErrorMessage(InName) then begin
     cmd := WhiteBoxfName + WBNoCompress + '-r=LidarNearestNeighbourGridding -v -i=' + InName + ' -o=' + OutName + ' --resolution=' + RealToString(GridSize,-12,-1) + ' --radius=' + RealToString(GridSize * MDDef.FillHoleRadius,-12,-1);
     ExecuteWBandOpenMap(cmd,OutName,euMeters);
  end;
end;


procedure WBT_IDWCreate(InName,OutName : PathStr; GridSize : float64);
var
   cmd : ansistring;
begin
  {$IfDef RecordUseOtherPrograms} WriteLineToDebugFile('WBIDWCreate, infile=' + InName + '  outfile=' + OutName); {$EndIf}
  if WhiteBoxPresent and FileExistsErrorMessage(InName) then begin
     cmd := WhiteBoxfName  + WBNoCompress + '-r=LidarIdwInterpolation -v -i=' + InName + ' -o=' + OutName + ' --resolution=' + RealToString(GridSize,-12,-1) + ' --radius=' + RealToString(GridSize * MDDef.FillHoleRadius,-12,-1);
     ExecuteWBandOpenMap(cmd,OutName,euMeters);
  end;
end;


function WBT_ProfileCurvature(InName : PathStr) : integer;
var
   cmd : ansistring;
   OutName : PathStr;
begin
  if WhiteBoxPresent and FileExistsErrorMessage(InName) then begin
     OutName := MDTempDir + 'wbt_profile_curvature_' + ExtractFileNameNoExt(InName) + '.tif';
     cmd := WhiteBoxfName + WBNoCompress + '-r=ProfileCurvature -v --dem=' + InName + ' -o=' + OutName;
     ExecuteWBandOpenMap(cmd,OutName,euPerMeter);
  end;
end;


function WBT_TangentialCurvature(InName : PathStr) : integer;
var
   cmd : ansistring;
   OutName : PathStr;
begin
  if WhiteBoxPresent and FileExistsErrorMessage(InName) then begin
     OutName := MDTempDir + 'wbt_tangential_curvature_' + ExtractFileNameNoExt(InName) + '.tif';
     cmd := WhiteBoxfName + WBNoCompress + '-r=TangentialCurvature -v --dem=' + InName + ' -o=' + OutName;
     ExecuteWBandOpenMap(cmd,OutName,euPerMeter);
  end;
end;


function WBT_MinimalCurvature(InName : PathStr) : integer;
var
   cmd : ansistring;
   OutName : PathStr;
begin
  if WhiteBoxPresent and FileExistsErrorMessage(InName) then begin
     OutName := MDTempDir + 'wbt_minimal_curvature_' + ExtractFileNameNoExt(InName) + '.tif';
     cmd := WhiteBoxfName + WBNoCompress + '-r=MinimalCurvature -v --dem=' + InName + ' -o=' + OutName;
     ExecuteWBandOpenMap(cmd,OutName,euPerMeter);
  end;
end;


function WBT_MaximalCurvature(InName : PathStr) : integer;
var
   cmd : ansistring;
   OutName : PathStr;
begin
  if WhiteBoxPresent and FileExistsErrorMessage(InName) then begin
     OutName := MDTempDir + 'wbt_maximal_curvature_' + ExtractFileNameNoExt(InName) + '.tif';
     cmd := WhiteBoxfName + WBNoCompress + '-r=MaximalCurvature -v --dem=' + InName + ' -o=' + OutName;
     ExecuteWBandOpenMap(cmd,OutName,euPerMeter);
  end;
end;


function WBT_MeanCurvature(InName : PathStr) : integer;
var
   cmd : ansistring;
   OutName : PathStr;
begin
  if WhiteBoxPresent and FileExistsErrorMessage(InName) then begin
     OutName := MDTempDir + 'wbt_mean_curvature_' + ExtractFileNameNoExt(InName) + '.tif';
     cmd := WhiteBoxfName + WBNoCompress + '-r=MeanCurvature -v --dem=' + InName + ' -o=' + OutName;
     ExecuteWBandOpenMap(cmd,OutName,euPerMeter);
  end;
end;


function WBT_GaussianCurvature(InName : PathStr) : integer;
var
   cmd : ansistring;
   OutName : PathStr;
begin
  if WhiteBoxPresent and FileExistsErrorMessage(InName) then begin
     OutName := MDTempDir + 'wbt_gaussian_curvature_' + ExtractFileNameNoExt(InName) + '.tif';
     cmd := WhiteBoxfName + WBNoCompress + '-r=GaussianCurvature -v --dem=' + InName + ' -o=' + OutName;
     ExecuteWBandOpenMap(cmd,OutName,euPerMeter);
  end;
end;


procedure WBT_Geomorphons(InName : PathStr);
var
   OutName : PathStr;
   bfile : tStringList;
   cmd : ansistring;
begin
  {$IfDef RecordUseOtherPrograms} WriteLineToDebugFile('WhiteBoxGeomorphons, infile=' + InName + '  outfile=' + OutName); {$EndIf}
  if WhiteBoxPresent and FileExistsErrorMessage(InName) then begin
     OutName := NextFileNumber(MDTempDir,'wbt_geomorphons_' + ExtractFileNameNoExt(InName) + '_','.tif');
     bfile := tStringList.Create;
     cmd := WhiteBoxfName  + WBNoCompress + ' -r=Geomorphons -v -i=' + InName + ' -o=' + OutName + ' --search=50 --threshold=0.0 --tdist=0 --forms=True';
     bfile.Add(cmd);
     EndBatchFile(MDTempDir + 'wbt_geomorphon.bat',bfile);
     {$IfDef RecordUseOtherPrograms} WriteLineToDebugFile('open outfile=' + OutName); {$EndIf}
     OpenNewDEM(OutName);
  end;
  {$IfDef RecordUseOtherPrograms} WriteLineToDebugFile('WhiteBoxGeomorphons out'); {$EndIf}
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
        tName := MDtempDir + 'wbt_denoise_' + ExtractFileNameNoExt(InName) + '.tif';
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
