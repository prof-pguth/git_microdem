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


function WhiteBoxGroundClassify(InName,OutName : PathStr) : shortString;
begin
   Result := WhiteBoxfName + ' -r=LidarGroundPointFilter -v -i=' + InName + ' -o=' + OutName + ' --radius=' + RealToString(MDDef.WBGroundClassRadius,-12,1);
end;


function WhiteBoxLidarSegmentationBasedFilter(InName,OutName : PathStr) : shortString;
begin
   Result := WhiteBoxfName + ' -r=LidarSegmentationBasedFilter -v -i=' + InName + ' -o=' + OutName + ' --radius=' + RealToString(MDDef.WBSegFilterRadius,-12,1);
end;


function WhiteBoxDeNoise(InName,OutName : PathStr) : shortString;
begin
   Result := WhiteBoxfName + WBNoCompress + '-r=LidarRemoveOutliers -v -i=' + InName + ' -o=' + OutName + ' --radius=' + RealToString(MDDef.WBDeNoiseRadius,-12,1) + ' --elev_diff=' + RealToString(MDDef.WBDeNoiseElevDiff,-12,1);
end;


function ExecuteWBandOpenMap(cmd : ansistring; OutName : PathStr; TheElevUnits : tElevUnit; MapType :tMapType = mtElevRainbow) : integer;
begin
   {$IfDef RecordWBT} WriteLineToDebugFile('ExecuteWBandOpenMap, cmd=' + cmd); {$EndIf}
   WinExecAndWait32(cmd);
   {$IfDef RecordWBT} WriteLineToDebugFile('WinExecAndWait32 over'); {$EndIf}
   if FileExists(OutName) then begin
      if LoadNewDEM(Result,OutName,false) then begin
         DEMGlb[Result].DEMheader.ElevUnits := TheElevUnits;
         CreateDEMSelectionMap(Result,true,true,MapType);
      end;
      {$IfDef RecordWBT} WriteLineToDebugFile('ExecuteWBandOpenMap map opened'); {$EndIf}
   end
   else MessageToContinue('Whitebox failure, try command in DOS window: ' + cmd);
   {$IfDef RecordWBT} WriteLineToDebugFile('ExecuteWBandOpenMap out'); {$EndIf}
end;


procedure WhiteBoxGridFillMissingData(InName : PathStr; TheElevUnits : tElevUnit);
var
   OutName : PathStr;
   cmd : ansistring;
begin
   if WhiteBoxPresent and FileExistsErrorMessage(InName) then begin
      OutName := MDTempDir + 'wbfill_holes_' + ExtractFileNameNoExt(InName) + '.tif';
      cmd := WhiteBoxfName +  WBNoCompress + '-r=FillMissingData -v -i=' + InName + ' -o=' + OutName + ' --filter=' + IntToStr(MDdef.FillHoleRadius);
      ExecuteWBandOpenMap(cmd,OutName,TheElevUnits);
   end;
end;


function WhiteBoxSlopeMap(InName : PathStr) : integer;
var
   cmd : ansistring;
   OutName : PathStr;
begin
  if WhiteBoxPresent and FileExistsErrorMessage(InName) then begin
     OutName := MDTempDir + 'wb_slope_' + ExtractFileNameNoExt(InName) + '.tif';
     cmd := WhiteBoxfName + WBNoCompress + '-r=Slope -v --dem=' + InName + ' -o=' + OutName + ' --units="percent"';
     Result := ExecuteWBandOpenMap(cmd,OutName,PercentSlope);
  end;
end;


function WhiteBox_TRI(InName : PathStr) : integer;
var
   cmd : ansistring;
   OutName : PathStr;
begin
  if WhiteBoxPresent and FileExistsErrorMessage(InName) then begin
     OutName := MDTempDir + 'WB_TRI_' + ExtractFileNameNoExt(InName) + '.tif';
     cmd := WhiteBoxfName + WBNoCompress + '-r=RuggednessIndex -v --dem=' + InName + ' -o=' + OutName;
     Result := ExecuteWBandOpenMap(cmd,OutName,euMeters);
  end;
end;

function WhiteBox_AverageNormalVectorAngularDeviation(InName : PathStr; filtersize : integer) : integer;
var
   cmd : ansistring;
   OutName : PathStr;
begin
  if WhiteBoxPresent and FileExistsErrorMessage(InName) then begin
     OutName := MDTempDir + 'WB_avg_norm_vect_dev_' + FilterSizeStr(FilterSize) + '_' + ExtractFileNameNoExt(InName) + '.tif';
     cmd := WhiteBoxfName + WBNoCompress + '-r=AverageNormalVectorAngularDeviation -v --dem=' + InName + ' -o=' + OutName + ' --filter=' + IntToStr(FilterSize);
     Result := ExecuteWBandOpenMap(cmd,OutName,zDegrees,mtElevSpectrum);
  end;
end;

function WhiteBox_CircularVarianceOfAspect(InName : PathStr; filtersize : integer) : integer;
var
   cmd : ansistring;
   OutName : PathStr;
begin
  if WhiteBoxPresent and FileExistsErrorMessage(InName) then begin
     OutName := MDTempDir + 'WB_circ_var_aspect_' + FilterSizeStr(FilterSize) + '_' + ExtractFileNameNoExt(InName) + '.tif';
     cmd := WhiteBoxfName + WBNoCompress + '-r=CircularVarianceOfAspect -v --dem=' + InName + ' -o=' + OutName + ' --filter=' + IntToStr(FilterSize);
     Result := ExecuteWBandOpenMap(cmd,OutName,undefined,mtElevSpectrum);
  end;
end;


function WhiteBoxDrainageBasins(InName : PathStr) : integer;
//   2/2/2024, this produces way too many basins with both tools; size does not make much difference
var
   cmd : ansistring;
   IntName,BasinName,BreachName,IsobasinName : PathStr;
begin
   if WhiteBoxPresent and FileExistsErrorMessage(InName) then begin
      BreachName := MDTempDir + 'WB_breach.tif';
      IntName := MDTempDir + 'WB_d8.tif';
      BasinName := MDTempDir + 'WB_basins.tif';
      IsoBasinName := MDTempDir + 'WB_isobasins.tif';

      cmd := WhiteBoxfName + WBNoCompress + ' -r=BreachDepressionsLeastCost -v --dem=' + InName + ' -o=' + BreachName + ' --dist=1000 --max_cost=100.0 --min_dist';
      WinExecAndWait32(cmd);

      cmd := WhiteBoxfName + WBNoCompress + ' -r=D8Pointer -v --dem=' + InName + ' -o=' + IntName;
      WinExecAndWait32(cmd);

      cmd := WhiteBoxfName + WBNoCompress + ' -r=Basins -v --d8_pntr=' + IntName + ' -o=' + BasinName;
      Result := ExecuteWBandOpenMap(cmd,BasinName,undefined,mtElevSpectrum);

      cmd := WhiteBoxfName + WBNoCompress + ' -r=Isobasins -v --dem=' + BreachName + ' -o=' + IsobasinName + ' --size=100000';
      Result := ExecuteWBandOpenMap(cmd,ISOBasinName,undefined,mtElevSpectrum);
   end;
end;


procedure WhiteBoxMultiscaleRoughness(InName : PathStr);
var
   cmd : ansistring;
   NewGrid : integer;
   OutName1,OutName2 : PathStr;
begin
  if WhiteBoxPresent and FileExistsErrorMessage(InName) then begin
     OutName1 := MDTempDir + 'wb_ms_rough_mag_' + ExtractFileNameNoExt(InName) + '.tif';
     OutName2 := MDTempDir + 'wb_ms_rough_scale_' + ExtractFileNameNoExt(InName) + '.tif';
     cmd := WhiteBoxfName  + WBNoCompress + '-r=MultiScaleRoughness -v --dem=' + InName + ' -out_mag=' + OutName1 + ' -out_scale=' + OutName2 + ' --min_scale=1 --max_scale=1000 --step=5';
     ExecuteWBandOpenMap(cmd,OutName1,euMeters);
     if FileExists(OutName2) then begin
        NewGrid := OpenNewDEM(OutName2);
        {$IfDef RecordWBT} WriteLineToDebugFile('second map opened'); {$EndIf}
     end;
  end;
end;


procedure WhiteBoxAspectMap(InName : PathStr);
var
   cmd : ansistring;
   OutName : PathStr;
begin
  if WhiteBoxPresent and FileExistsErrorMessage(InName) then begin
     OutName := MDTempDir + 'wb_aspect_' + ExtractFileNameNoExt(InName) + '.tif';
     cmd := WhiteBoxfName + WBNoCompress + '-r=Aspect -v --dem=' + InName + ' -o=' + OutName;   // + ' --units="percent"';
     ExecuteWBandOpenMap(cmd,OutName,AspectDeg);
  end;
end;


procedure WhiteBoxGeotiffMetadata(InName : PathStr);
var
   cmd : ansistring;
   OutName : PathStr;
   bFile : tStringList;
begin
  if WhiteBoxPresent and FileExistsErrorMessage(InName) then begin
     OutName := MDTempDir + 'wb-metadata_' + ExtractFileNameNoExt(InName) + '.txt';
     cmd := WhiteBoxfName + ' -r=PrintGeoTiffTags -v  --input=' + InName + ' >' + OutName;
     bfile := tStringList.Create;
     bfile.Add(cmd);
     EndBatchFile(MDTempDir + 'wb_gt_meta.bat',bfile);
     ShowInNotepadPlusPlus(OutName,ExtractFileName(OutName));
  end;
end;


procedure WBNearNeighCreate(InName,OutName : PathStr; GridSize : float64);
var
   cmd : ansistring;
begin
  {$IfDef RecordUseOtherPrograms} WriteLineToDebugFile('WBNearNeighCreate, infile=' + InName + '  outfile=' + OutName); {$EndIf}
  if WhiteBoxPresent and FileExistsErrorMessage(InName) then begin
     cmd := WhiteBoxfName + WBNoCompress + '-r=LidarNearestNeighbourGridding -v -i=' + InName + ' -o=' + OutName + ' --resolution=' + RealToString(GridSize,-12,-1) + ' --radius=' + RealToString(GridSize * MDDef.FillHoleRadius,-12,-1);
     ExecuteWBandOpenMap(cmd,OutName,euMeters);
  end;
end;


procedure WBIDWCreate(InName,OutName : PathStr; GridSize : float64);
var
   cmd : ansistring;
begin
  {$IfDef RecordUseOtherPrograms} WriteLineToDebugFile('WBIDWCreate, infile=' + InName + '  outfile=' + OutName); {$EndIf}
  if WhiteBoxPresent and FileExistsErrorMessage(InName) then begin
     cmd := WhiteBoxfName  + WBNoCompress + '-r=LidarIdwInterpolation -v -i=' + InName + ' -o=' + OutName + ' --resolution=' + RealToString(GridSize,-12,-1) + ' --radius=' + RealToString(GridSize * MDDef.FillHoleRadius,-12,-1);
     ExecuteWBandOpenMap(cmd,OutName,euMeters);
  end;
end;


function WhiteBoxProfileCurvature(InName : PathStr) : integer;
var
   cmd : ansistring;
   OutName : PathStr;
begin
  if WhiteBoxPresent and FileExistsErrorMessage(InName) then begin
     OutName := MDTempDir + 'wb_profile_curvature_' + ExtractFileNameNoExt(InName) + '.tif';
     cmd := WhiteBoxfName + WBNoCompress + '-r=ProfileCurvature -v --dem=' + InName + ' -o=' + OutName;
     ExecuteWBandOpenMap(cmd,OutName,euPerMeter);
  end;
end;


function WhiteBoxTangentialCurvature(InName : PathStr) : integer;
var
   cmd : ansistring;
   OutName : PathStr;
begin
  if WhiteBoxPresent and FileExistsErrorMessage(InName) then begin
     OutName := MDTempDir + 'wb_tangential_curvature_' + ExtractFileNameNoExt(InName) + '.tif';
     cmd := WhiteBoxfName + WBNoCompress + '-r=TangentialCurvature -v --dem=' + InName + ' -o=' + OutName;
     ExecuteWBandOpenMap(cmd,OutName,euPerMeter);
  end;
end;


function WhiteBoxMinimalCurvature(InName : PathStr) : integer;
var
   cmd : ansistring;
   OutName : PathStr;
begin
  if WhiteBoxPresent and FileExistsErrorMessage(InName) then begin
     OutName := MDTempDir + 'wb_minimal_curvature_' + ExtractFileNameNoExt(InName) + '.tif';
     cmd := WhiteBoxfName + WBNoCompress + '-r=MinimalCurvature -v --dem=' + InName + ' -o=' + OutName;
     ExecuteWBandOpenMap(cmd,OutName,euPerMeter);
  end;
end;


function WhiteBoxMaximalCurvature(InName : PathStr) : integer;
var
   cmd : ansistring;
   OutName : PathStr;
begin
  if WhiteBoxPresent and FileExistsErrorMessage(InName) then begin
     OutName := MDTempDir + 'wb_maximal_curvature_' + ExtractFileNameNoExt(InName) + '.tif';
     cmd := WhiteBoxfName + WBNoCompress + '-r=MaximalCurvature -v --dem=' + InName + ' -o=' + OutName;
     ExecuteWBandOpenMap(cmd,OutName,euPerMeter);
  end;
end;


function WhiteBoxMeanCurvature(InName : PathStr) : integer;
var
   cmd : ansistring;
   OutName : PathStr;
begin
  if WhiteBoxPresent and FileExistsErrorMessage(InName) then begin
     OutName := MDTempDir + 'wb_mean_curvature_' + ExtractFileNameNoExt(InName) + '.tif';
     cmd := WhiteBoxfName + WBNoCompress + '-r=MeanCurvature -v --dem=' + InName + ' -o=' + OutName;
     ExecuteWBandOpenMap(cmd,OutName,euPerMeter);
  end;
end;


function WhiteBoxGaussianCurvature(InName : PathStr) : integer;
var
   cmd : ansistring;
   OutName : PathStr;
begin
  if WhiteBoxPresent and FileExistsErrorMessage(InName) then begin
     OutName := MDTempDir + 'wb_gaussian_curvature_' + ExtractFileNameNoExt(InName) + '.tif';
     cmd := WhiteBoxfName + WBNoCompress + '-r=GaussianCurvature -v --dem=' + InName + ' -o=' + OutName;
     ExecuteWBandOpenMap(cmd,OutName,euPerMeter);
  end;
end;


procedure WhiteBoxGeomorphons(InName : PathStr);
var
   OutName : PathStr;
   bfile : tStringList;
   cmd : ansistring;
begin
  {$IfDef RecordUseOtherPrograms} WriteLineToDebugFile('WhiteBoxGeomorphons, infile=' + InName + '  outfile=' + OutName); {$EndIf}
  if WhiteBoxPresent and FileExistsErrorMessage(InName) then begin
     OutName := NextFileNumber(MDTempDir,'wb_geomorphons_' + ExtractFileNameNoExt(InName) + '_','.tif');
     bfile := tStringList.Create;
     cmd := WhiteBoxfName  + WBNoCompress + ' -r=Geomorphons -v -i=' + InName + ' -o=' + OutName + ' --search=50 --threshold=0.0 --tdist=0 --forms=True';
     bfile.Add(cmd);
     EndBatchFile(MDTempDir + 'wb_geomorphon.bat',bfile);
     {$IfDef RecordUseOtherPrograms} WriteLineToDebugFile('open outfile=' + OutName); {$EndIf}
     OpenNewDEM(OutName);
  end;
  {$IfDef RecordUseOtherPrograms} WriteLineToDebugFile('WhiteBoxGeomorphons out'); {$EndIf}
end;


procedure WhiteBoxPennockLandformClass(InName : PathStr; SmoothFirst : boolean);
var
   tName,OutName : PathStr;
   bfile : tStringList;
begin
  {$IfDef RecordUseOtherPrograms} WriteLineToDebugFile('WhiteBoxPennockLandformClass, infile=' + InName + '  outfile=' + OutName); {$EndIf}
  if WhiteBoxPresent and FileExistsErrorMessage(InName) then begin
     OutName := NextFileNumber(MDTempDir,'wb_pennock_' + ExtractFileNameNoExt(InName) + '_','.tif');
     bfile := tStringList.Create;
     if SmoothFirst then begin
        tName := MDtempDir + 'denoise.tif';
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
