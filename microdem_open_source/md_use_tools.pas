unit md_use_tools;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2026 Peter L. Guth  }
{________________________________________}


{$I nevadia_defines.inc}


//{$Define ConvertDBFtoDB}


{$IfDef RecordProblems}  //normally only defined for debugging specific problems
   {$IFDEF DEBUG}
      //{$Define RecordWBT}
      //{$Define RecordLSPcalculator}
      //{$Define RecordSAGA}
      //{$Define RecordSAGARanges}
      //{$Define RecordSAGA_JustResult}
      //{$Define RecordSAGALS}
      //{$Define RecordSAGAFull}
      //{$Define SAGA_HillValley}
      //{$Define OpenLasTools}
      //{$Define RecordACOLITE}
      //{$Define RecordSubsetOpen}
      //{$Define RecordUseOtherPrograms}
      //{$Define RecordSaveProblems}
      //{$Define RecordReformat}
      //{$Define RecordMapProj}
      //{$Define RecordWBT_DEM}
      {$Define RecordFFT}
   {$Else}
   {$EndIf}
{$EndIf}


interface


uses
//needed for inline of the core DB functions
   Petmar_db,
   Data.DB,
   {$IfDef UseFireDacSQLlite}
      FireDAC.Comp.Client, FireDAC.Comp.Dataset,FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteWrapper,
   {$EndIf}

   {$IfDef UseTDBF}
      dbf,
   {$EndIf}

   {$IfDef UseTCLientDataSet}
      DBClient,
   {$EndIf}
//end DB declarations

   {$IfDef MSWindows}
      Windows,Messages,
   {$EndIf}

   {$IfDef VCL}
      Forms, Graphics,ExtCtrls,Grids,Controls,
   {$EndIf}

   System.IOutils,System.UITypes,System.Math,System.UIConsts,System.Diagnostics,
   Vcl.StdCtrls,
   SysUtils, Classes,StrUtils,
   petmar,Petmar_types,PetMath,
   DEMMapf,DEMMapDraw,DEMDefs,BaseMap,DEM_NLCD;


{$If Defined(ExLAStools) or Defined(ExPointCloud)}
{$Else}
   type
      tlas2Las = (lasAssignProjection,lasToUTM,lasAssignEPSGreprojectUTM,lasAssignUTM,lasAssignGeo,lasReprojectSpecifiedtoGeo);

   procedure LasToLasTools(How : tlas2Las);
   function GetLASToolsFileName(var fName : PathStr) : boolean;
   function lastools_txt2las_cmd(inName : PathStr; UTMzone : shortString; ParseVals : shortstring = '') : shortstring;
   procedure BlastTinCreate(InName,OutName : PathStr; GridSize : float64);
   procedure CallLasInfo;
   procedure LAStoolsTextToLAS;
   procedure Lastools_DEMToLAZ(InName,OutName : PathStr; Extra : shortString = '');
   procedure ConvertDEMtoLAZ(Memo1 : tMemo);
{$EndIf}


{$IfDef ExWhiteBox}
{$Else}
   function WhiteBoxPresent : boolean;
   function WBT_GroundClassify(InName,OutName : PathStr) : shortString;
   function WBT_LidarSegmentationBasedFilter(InName,OutName : PathStr) : shortString;
   function WBT_DeNoise(InName,OutName : PathStr) : shortString;

   function WBT_SlopeMap(OpenMap : boolean; InName : PathStr; OutName : PathStr = '') : integer;
   function WBT_HillshadeMap(OpenMap : boolean; DEM : integer; OutName : PathStr = '') : integer;
   function WBT_MultidirectionalHillshadeMap(OpenMap : boolean; DEM : integer; OutName : PathStr = '') : integer;

   function WBT_TRI(OpenMap : boolean; InName : PathStr; OutName : PathStr = '') : integer;
   function WBT_AspectMap(OpenMap : boolean; InName : PathStr; OutName : PathStr = '') : integer;

   function WBT_AvgNormVectAngDev(OpenMap : boolean; InName : PathStr; filtersize : integer) : integer;
   function WBT_SphericalStdDevOfNormals(OpenMap : boolean; InName : PathStr; filtersize : integer) : integer;

   function WBT_CircularVarianceOfAspect(OpenMap : boolean; InName : PathStr; filtersize : integer) : integer;
   function WBT_DrainageBasins(InName : PathStr) : integer;
   function WBT_Geomorphons(OpenMap : boolean; InName : PathStr; Search : integer=50; Skip : integer = 0) : integer;
   function WBT_BNearNeighCreate(OpenMap : boolean; InDirectory,OutName : PathStr; GridSize : float64; AssignProjection : shortstring = '') : integer;
   function WBT_WetnessIndex(OpenMap,D8 : boolean; DEMName : PathStr; WetnessName : PathStr = '') : integer;
   function WBT_Breach_Depression(DEMName : PathStr; var BreachName : PathStr) : integer;
   function WBT_FlowAccumulation(OpenMap,Log,D8 : boolean; DEMName : PathStr; var BreachName, FlowAccName : PathStr) : integer;
   function WBT_Extract_Streams(OpenMap : boolean; DEMName : PathStr; var BreachName,FlowAccumulationName,StreamName : PathStr; Threshhold : float32 = 100.0) : integer;
   function WBT_ElevAboveStream(OpenMap : boolean; DEMName : PathStr; BreachName,FlowAccumulationName,StreamName,HANDName : PathStr; Threshhold : float32 = 100.0) : integer;
   function WBT_FeaturePreserveSmooth(OpenMap : boolean; InName : PathStr; zUnits : byte; OutName : PathStr = '') : integer;
   function WBT_IDWCreate(OpenMap : boolean; InDirectory,OutName : PathStr; GridSize : float64; AssignProjection : shortstring = '') : integer;
   procedure WBT_PennockLandformClass(InName : PathStr; SmoothFirst : boolean);
   procedure WBT_GridFillMissingData(InName : PathStr; TheElevUnits : tElevUnit; OutName : PathStr = '');
   procedure WBT_GeotiffMetadata(InName : PathStr);
   procedure WBT_MultiscaleRoughness(InName : PathStr);
   procedure WBT_KappaIndex(ClassifiedName,ReferenceName : PathStr; HTMLname : PathStr = '');
   function WBT_Gaussian(OpenMap : boolean; InName : PathStr; Sigma : float32 = 0.75; OutName : PathStr = ''; TheElevUnits : tElevUnit = euUndefined; MapType : tMapType = mtElevSpectrum) : integer;

  //curvatures
     function WBT_ProfileCurvature(OpenMap : boolean; InName : PathStr; OutName : PathStr = ''): integer;
     function WBT_PlanCurvature(OpenMap : boolean; InName : PathStr; OutName : PathStr = ''): integer;
     function WBT_TangentialCurvature(OpenMap : boolean; InName : PathStr; OutName : PathStr = '') : integer;
     function WBT_MinimalCurvature(OpenMap : boolean; InName : PathStr; OutName : PathStr = ''): integer;
     function WBT_MaximalCurvature(OpenMap : boolean; InName : PathStr; OutName : PathStr = ''): integer;
     function WBT_MeanCurvature(OpenMap : boolean; InName : PathStr; OutName : PathStr = ''): integer;
     function WBT_GaussianCurvature(OpenMap : boolean; InName : PathStr; OutName : PathStr = ''): integer;
{$EndIf}


{$IfDef ExSAGA}
{$Else}
   procedure SAGA_all_DEMs_remove_sinks;
   function SagaTRIMap(OpenMap : boolean; InName : PathStr; OutName : PathStr = '') : integer;
   function SagaTPIMap(OpenMap : boolean; InName : PathStr; OutName : PathStr = '') : integer;

   function SagaVectorRuggednessMap(OpenMap : boolean; InName : PathStr; Radius : integer) : integer;
   function SAGA_LSFactor(OpenMap : boolean; InName : PathStr; LSGridName : PathStr = '') : integer;
   function SAGA_ConvergenceIndex(OpenMap : boolean; InName : PathStr; ConIndexGridName : PathStr = '') : integer;

   function SagaSinkRemoval(InName : PathStr; OutName : PathStr = '') : integer;
   function SagaChannelNetworkGrid(OpenMap : boolean; InName : PathStr; OutGridName : PathStr = '') : integer;
   function SagaChannelShapefile(InName : PathStr; ChannelName : PathStr = '') : integer;
   function SAGAedgeContaminationMap(InName : PathStr; OutName : PathStr = '') : integer;
   function SagaWatershedBasins(InName : PathStr; BasinGrid : PathStr = ''; ChannelNetwork : PathStr = ''; OutName : PathStr = '') : integer;
   function SAGA_WatershedBasinsWangLiu(InName : PathStr) : integer;
   function SAGA_StrahlerOrderGrid(InName : PathStr; OutName : PathStr = '') : integer;
   function SAGA_FlowAccumulationParallizeable(InName : PathStr; OutName : PathStr = '') : integer;
   function SAGA_Slope_percent(OpenMap : boolean; SlopeMethod : char; InName : PathStr; SlopeFName : PathStr = '') : integer;
   function SAGA_Aspect(OpenMap : boolean; InName : PathStr; AspectFName : PathStr = '') : integer;
   function SAGA_PlanCurvature(OpenMap : boolean; InName : PathStr; SlopeMethod : char = '3'; PlanCurvatureFName : PathStr = '') : integer;
   function SAGA_ProfileCurvature(OpenMap : boolean; InName : PathStr; SlopeMethod : char = '3'; OutName : PathStr = '') : integer;
   function SAGA_TangentialCurvature(OpenMap : boolean; InName : PathStr; SlopeMethod : char = '3'; OutName : PathStr = '') : integer;

   function SAGA_CurvatureClassification(OpenMap : boolean; DEMName : PathStr; CurvatureClassFName : PathStr = '') : integer;
   function SAGA_IwahashiAndPikeClassification(OpenMap : boolean; DEMName : PathStr; Classes : integer = 12; ClassFName : PathStr = '') : integer;
   function SAGA_HillValleyIndexes(OpenMap : boolean; DEMName : PathStr; ValleyIndexName : PathStr = ''; HillIndexName : PathStr = '') : integer;
   function SAGA_Geomorphons(OpenMap : boolean; DEMName : PathStr; GeomorphonsFName : PathStr = '') : integer;
{$EndIf}


{$IfDef ExGRASS}
{$Else}
   function ClearGRASSdirectory : shortstring;
   procedure GetGrassExtensionsNow(InName : PathStr);
   function GrassSlopeMap(InName : PathStr; OpenMap : boolean = true; OutName : PathStr = '') : integer;
   function GrassProfileCurvatureMap(InName : PathStr; OpenMap : boolean = true; OutName : PathStr = '') : integer;
   function GrassTangentialCurvatureMap(InName : PathStr; OpenMap : boolean = true; OutName : PathStr = '') : integer;
   function GrassAspectMap(InName : PathStr; OpenMap : boolean = true; OutName : PathStr = '') : integer;
   function GrassVectorRuggedness(InName : PathStr; WindowSize : integer; OutName : PathStr = '') : integer;
   function GrassTRIMap(OpenMap : boolean; InName : PathStr; OutName : PathStr = '') : integer;
   function GrassTPIMap(InName : PathStr; OpenMap : boolean = true; OutName : PathStr = '') : integer;
   procedure GRASS_partialDerivatives(DEM : integer; var Grids : tPartialGrids;  OpenMap : boolean = true);
   function Grass_dx_partial(InName : PathStr; OpenMap : boolean = true; OutName : PathStr = '') : integer;
   function Grass_dy_partial(InName : PathStr; OpenMap : boolean = true; OutName : PathStr = '') : integer;
   function Grass_dxx_partial(InName : PathStr; OpenMap : boolean = true; OutName : PathStr = '') : integer;
   function Grass_dyy_partial(InName : PathStr; OpenMap : boolean = true; OutName : PathStr = '') : integer;
   function Grass_dxy_partial(InName : PathStr; OpenMap : boolean = true; OutName : PathStr = '') : integer;
{$EndIf}


{$IfDef ExOTB}
{$Else}
   procedure OTB_ConcatenateImages(InNames: tStringList; OutName : PathStr);
   procedure OTB_KMeansClassification(InName, OutName : PathStr);
   procedure OTB_Segmentation(InName, OutName : PathStr);
   procedure OTB_PanSharpen(PanName,OrthoName, OutName : PathStr);
   procedure OTB_PickForKMeansClustering(SatOnMap : integer);
   procedure OTB_PickToConcatenateImages(SatOnMap : integer);
{$EndIf}

{$IfDef ExRVT}
{$Else}
   procedure RVTgrids(DEM : integer);
{$EndIf}

procedure FusionTinCreate(InName,OutName : PathStr; GridSize : float64; GridZone : integer; HemiChar : ansichar);

function MCC_lidarPresent : boolean;

function GPSBabel_fit2gpx(inname,outname : PathStr) : boolean;

procedure ACOLITEprocessing(MapOwner : tMapForm; OpenMaps : boolean = true);

procedure laslibReproject(ask : boolean);

procedure AddEGMtoDBfromSphHarmonics(DBonTable : integer; Do2008 : boolean);

function RUN_LSPcalculator(DEM : integer; Options : shortstring; OpenMap : boolean = true; degree : integer = 3) : integer;
procedure fft_tools(OpenMap : boolean; DEM : integer);
function fft_Compare(RefDEM : integer; TestDEMs : tDEMBooleanArray; rt : float32 = 0.75) : PathStr;


procedure ExpandOutName(InName : PathStr; BaseName : shortString; var OutName : PathStr);

implementation


uses
   DEMDef_routines,
   DEMCoord,
   DEMeros,
   PetDButils,
   DEM_Manager,
   DEMDataBase,
   gdal_tools,
   {$IfDef ExPointCloud}
   {$Else}
      las_lidar,
   {$EndIf}
   geotiff,
   dem_indexes,
   nevadia_main;

const
   WBNoCompress = ' --compress_rasters=false ';

{$i saga_wrapper.inc}

{$I wbt_wrapper.inc}

{$I grass_wrapper.inc}


{$If Defined(ExLAStools) or Defined(ExPointCloud)}
{$Else}
   {$I lastools_wrapper.inc}
{$EndIf}

procedure ExpandOutName(InName : PathStr; BaseName : shortString; var OutName : PathStr);
begin
   if (OutName = '') then OutName := MDTempDir + BaseName + '_' + ExtractFileNameNoExt(InName) + '.tif';
end;


function ClearGRASSdirectory : shortstring;
begin
   ClearGRASSdirectory := 'rd /S /Q ' + MDtempDir + 'grass1';
end;


function BBtoPathString(bb : sfBoundBox; Decs : integer = 2) : shortstring;
begin
   Result := RealToString(bb.ymin,-8,Decs) + '_' + RealToString(bb.xmin,-8,Decs) + '_' + RealToString(bb.ymax,-8,Decs) + '_' + RealToString(bb.xmax,-8,Decs);
end;


function RUN_LSPcalculator(DEM : integer; Options : shortstring; OpenMap : boolean = true; degree : integer = 3) : integer;
var
   cmd : shortstring;
   OutName : PathStr;
begin
   Result := 0;
   lsp_calculator_fName := 'J:\gis_software\xiceph\lsp_calculator.exe';
   FindDriveWithFile(lsp_calculator_fName);
   if Not FileExists(lsp_calculator_fName) then begin
      if not GetExistingFileName('lsp_calculator.exe','*.exe',lsp_calculator_fName) then begin
         {$IfDef RecordLSPcalculator} WriteLineToDebugFile('could not find lsp_calculator.exe'); {$EndIf}
         exit;
      end;
   end;

   if FileExists(lsp_calculator_fName) then begin
     outname :=  MDtempDir + 'LSP_C_' + DEMGlb[DEM].AreaName;
     OutName := OutName + '_deg' + IntToStr(degree);

     cmd := lsp_calculator_fName + ' -i ' + DEMGlb[DEM].GeotiffDEMName + ' -o ' + OutName + ' ' + Options;
     if (Degree = 4) then cmd := cmd + ' -d 4';

     {$IfDef RecordLSPcalculator} WriteLineToDebugFile(cmd); {$EndIf}
     WinExecAndWait32(cmd,true,MDdef.ShowWinExec);
     OutName := Outname + Options + '.tif';
     OutName := StringReplace(OutName,'--','_',[rfReplaceAll, rfIgnoreCase]);
     {$IfDef RecordLSPcalculator} WriteLineToDebugFile(cmd); {$EndIf}
     WinExecAndWait32(cmd,true,MDdef.ShowWinExec);
     if FileExists(OutName) then begin
        Result := OpenNewDEM(OutName,false);
        //DEMGlb[Result].DEMheader.ElevUnits := eucurv_kncc;
        DEMGlb[Result].DEMHeader.VerticalCSTypeGeoKey := VertCSUndefined;
        if OpenMap then CreateDEMSelectionMap(Result,true,true,mtDEMBlank);
     end
     else begin
         {$IfDef RecordLSPcalculator} WriteLineToDebugFile('could not find ' + OutName); {$EndIf}
     end;
   end;
end;


var
   BaseDir : PathStr;


function CMD_FFT_process(DEM : integer; var ResultsDir : PathStr) : shortstring;
begin
   ResultsDir := MDtempDir + 'fft_results_' + DEMglb[DEM].AreaName;
   Result := BaseDir + 'fft-process --input ' + DEMGlb[DEM].GeotiffDEMName +  ' --output ' + ResultsDir + ' >' + ResultsDir + '_process.txt';
   {$IfDef RecordFFT} WriteLineToDebugFile(Result); {$EndIf}
end;

function FFT_process(DEM : integer) : PathStr;
//https://github.com/xiceph/physical-geomorphometry-tools/tree/main/fft-tools/packages/fft-process
var
   cmd : shortstring;
begin
   wmDEM.SetPanelText(3, 'Process: ' + DEMglb[DEM].AreaName ,true);
   cmd := CMD_FFT_process(DEM,Result);
   //result := MDtempDir + 'fft_results_' + DEMglb[DEM].AreaName;
   //cmd := BaseDir + 'fft-process --input ' + DEMGlb[DEM].GeotiffDEMName +  ' --output ' + result + ' >' + Result + '_process.txt';
   {$IfDef RecordFFT} WriteLineToDebugFile(cmd); {$EndIf}
   WinExecAndWait32(cmd,true,MDdef.ShowWinExec);
   {$IfDef RecordFFT} WriteLineToDebugFile('Returned'); {$EndIf}
end;


function FFT_polar(DEM : integer; FFTResult : PathStr) : PathStr;
var
   cmd : shortstring;
begin
   wmDEM.SetPanelText(3, 'Polar: ' + DEMglb[DEM].AreaName ,true);
   Result := MDtempDir + 'fft_results_' + DEMglb[DEM].AreaName + '\polar';
   cmd := BaseDir + 'fft-polar --input ' + FFTresult + ' --output ' + Result;
   {$IfDef RecordFFT} WriteLineToDebugFile(cmd); {$EndIf}
   WinExecAndWait32(cmd,true,MDdef.ShowWinExec);
end;


procedure FFT_analyze(DEM : integer; PolarResult : PathStr; var Summary,Spectrum : PathStr);
var
   cmd : shortstring;
begin
   wmDEM.SetPanelText(3, 'Analyze: ' + DEMglb[DEM].AreaName ,true);
   Summary := MDtempDir + 'fft_results_' + DEMglb[DEM].AreaName + '\summary.csv';
   Spectrum := MDTempDir + 'fft_results_' + DEMglb[DEM].AreaName + '\spectrum.html';
   cmd := BaseDir + 'fft-analyze --input ' + PolarResult + ' --output ' + Summary + ' --mode radial-mean --plot ' + Spectrum;
   {$IfDef RecordFFT} WriteLineToDebugFile(cmd); {$EndIf}
   WinExecAndWait32(cmd,true,MDdef.ShowWinExec);
end;


function FFT_filter(DEM : integer; FFTresult : PathStr) : PathStr;
var
   cmd : shortstring;
begin
   wmDEM.SetPanelText(3, 'Filter: ' + DEMglb[DEM].AreaName ,true);
   Result := MDTempDir + 'fft_results_' + DEMglb[DEM].AreaName + '\filtered';
   cmd := BaseDir + 'fft-filter --input ' + FFTresult + ' --output ' + Result + ' --min-wavelength 20 --max-wavelength 500 --taper-width 0.2';
   {$IfDef RecordFFT} WriteLineToDebugFile(cmd); {$EndIf}
   WinExecAndWait32(cmd,true,MDdef.ShowWinExec);
   {$IfDef RecordFFT} WriteLineToDebugFile('Returned'); {$EndIf}
end;


function FFT_inverse_DEM(DEM : integer; Filtered : PathStr) : PathStr;
var
   cmd : shortstring;
begin
   wmDEM.SetPanelText(3, 'Inverse: ' + DEMglb[DEM].AreaName ,true);
   Result := MDTempDir + 'fft_results_' + DEMglb[DEM].AreaName + '\filtered_dem.tif';
   cmd := BaseDir + 'fft-inverse --input ' + Filtered + ' --output ' + Result;
   {$IfDef RecordFFT} WriteLineToDebugFile(cmd); {$EndIf}
   WinExecAndWait32(cmd,true,MDdef.ShowWinExec);
end;


procedure fft_tools(OpenMap : boolean; DEM : integer);
//https://github.com/xiceph/physical-geomorphometry-tools/tree/main/fft-tools
var
   Summary,Spectrum,Filtered,FilteredDEM,
   FFTresult,PolarResult : PathStr;
   cmd : shortstring;
begin
   {$IfDef RecordFFT} WriteLineToDebugFile('procedure fft_tools in ' + DEMglb[DEM].AreaName); {$EndIf}
   SetColorForProcessing;
   BaseDir := 'F:\gis_software\xiceph\';
   FindDriveWithPath(BaseDir);
   FFTresult := FFT_process(DEM);
   PolarResult := FFT_polar(DEM,FFTresult);
   FFT_analyze(DEM,PolarResult,Summary,Spectrum);
   Filtered := FFT_filter(DEM,FFTresult);
   FilteredDEM := FFT_inverse_DEM(DEM,Filtered);

   SetColorForWaiting;
   {$IfDef RecordFFT} WriteLineToDebugFile('procedure fft_tools out ' + DEMglb[DEM].AreaName); {$EndIf}
end;




function fft_Compare(RefDEM : integer; TestDEMs : tDEMBooleanArray; rt : float32 = 0.75) : PathStr;
//https://github.com/xiceph/physical-geomorphometry-tools/tree/main/fft-tools/packages/fft-compare
//https://github.com/xiceph/physical-geomorphometry-tools/tree/main/fft-tools



   function CheckForGeoDEM(DEM : integer) : integer;
   var
      XUTM,YUTM : float64;
      futmName,mdName : PathStr;
      tDEM : integer;
      GridLimits : tGridLimits;
   begin
      if (DEMglb[DEM].DEMheader.DEMUsed = ArcSecDEM) then begin
         futmName := MDtempDir + 'futm_' + DEMglb[DEM].AreaName + '.tif';
         if not FileExists(futmName) then begin
             DEMglb[DEM].DEMGridtoUTM(0,0,XUTM,YUTM);
             DEMglb[DEM].DEMheader.DEMUsed := UTMBasedDEM;
             DEMglb[DEM].DEMheader.DataSpacing := SpaceMeters;
             DEMglb[DEM].DEMheader.DEMxSpacing := DEMglb[DEM].AverageXSpace;
             DEMglb[DEM].DEMheader.DEMySpacing := DEMglb[DEM].AverageYSpace;
             DEMglb[DEM].DEMheader.SWCornerX := xutm;
             DEMglb[DEM].DEMheader.SWCornerY := yutm;
             DEMglb[DEM].FilledOutlineGridBox(GridLimits);
             mdName := ChangeFileExt(futmName,'.dem');
             DEMglb[DEM].SaveSpecifiedPartOfDEM(mdName,GridLimits);
             DEMglb[DEM].ReloadDEM(true);
             tDEM := OpenNewDEM(mdName,false);
             DEMglb[tDEM].SaveAsGeotiff(futmName);
             CloseSingleDEM(tDEM);
         end;
         Result := OpenNewDEM(futmName,true);
      end
      else Result := DEM;
   end;

var
   RefResult,fName : PathStr;
   CompareName,cmd : shortstring;
   BatFile : tStringList;
   DEM,NumTests : integer;
   CorrectTestDEMs : array[1..10] of integer;
   TestResult : array[1..10] of PathStr;
begin
   {$IfDef RecordFFT} WriteLineToDebugFile('procedure fft_compare in, ref= ' + DEMglb[RefDEM].AreaName); {$EndIf}
   SetColorForProcessing;
   RefDEM := CheckForGeoDEM(RefDEM);
   for DEM := 1 to 10 do CorrectTestDEMs[DEM] := 0;
   NumTests := 0;
   for DEM := 1 to MaxDEMDataSets do begin
      if TestDEMs[DEM] and ValidDEM(DEM) then begin
         inc(NumTests);
         CorrectTestDEMs[NumTests] := CheckForGeoDEM(DEM);
      end;
   end;

   {$IfDef RecordFFT} WriteLineToDebugFile('procedure fft_compare in ' + CompareName); {$EndIf}
   BaseDir := 'F:\gis_software\xiceph\';
   FindDriveWithPath(BaseDir);

   BatFile := tStringList.Create;
   BatFile.Add(CMD_FFT_process(RefDEM,RefResult));
   for DEM := 1 to NumTests do begin
      CompareName := DEMglb[RefDEM].AreaName + '_' + DEMglb[CorrectTestDEMs[DEM]].AreaName;
      BatFile.Add(CMD_FFT_process(CorrectTestDEMs[DEM],TestResult[DEM]));
      Result := MDtempDir + 'fft_compare_' + CompareName;
      cmd := BaseDir + 'fft-compare --input-a ' + RefResult + ' --input-b ' + TestResult[DEM] + ' --output ' + Result + ' --plot ' + Result + '\' + CompareName + '.html' +
         ' --retention-threshold ' + RealToString(rt,-8,2) + ' --coherence-threshold 0.5' + ' >' + Result + '_compare_results.txt';
      BatFile.Add(cmd);
   end;
   fName := mdTempDir + 'compare_' + CompareName + '.bat';
   BatFile.SaveToFile(fName);
   WinExecAndWait32(fName,true,MDdef.ShowWinExec);

   for DEM := 1 to NumTests do begin
      CompareName := DEMglb[RefDEM].AreaName + '_' + DEMglb[CorrectTestDEMs[DEM]].AreaName;
      Result := MDtempDir + 'fft_compare_' + CompareName;
      System.SysUtils.RenameFile(Result + '\comparison_summary.csv', Result + '\' + CompareName + '_comparison.csv');
   end;
   SetColorForWaiting;
   {$IfDef RecordFFT} WriteLineToDebugFile('procedure fft_compare out, ref= ' + DEMglb[RefDEM].AreaName); {$EndIf}
end;


procedure AddEGMtoDBfromSphHarmonics(DBonTable : integer; Do2008 : boolean);
var
   Output : tStringList;
   Lat,Long : float64;
   fName,TStr : shortstring;
   i : integer;
   EGMdir,EXEname,OutName : PathStr;
begin
   {$IfDef RecordExports} WriteLineToDebugFile('AddEGMtoDBfromSphHarmonics in'); {$EndIf}
   GISdb[DBonTable].EmpSource.Enabled := false;
   GISdb[DBonTable].MyData.First;
   Output := tStringList.Create;
   if (Do2008) then begin
      EGMDir := ProgramRootDir + 'EGM2008_Spherical_Harmonics\';
      EXEName := EGMDir + 'hsynth_WGS84.exe';
      fName := 'EGM2008_AL';
      OutName := 'OUTPUT.DAT';
   end
   else begin
      EGMDir := ProgramRootDir + 'EGM96_Spherical_Harmonics\';
      EXEName := EGMDir + 'F477.exe';
      fName := 'EGM96_AL';
      OutName := 'outf477.dat';
   end;
   DeleteFileIfExists(EGMDir + OutName);

   while not GISdb[DBonTable].MyData.eof do begin
      if GISdb[DBonTable].GetLatLongToRepresentRecord(Lat,Long) then begin
         Output.Add(RealToString(Lat,12,8) + RealToString(Long,14,8));
      end;
      GISdb[DBonTable].MyData.Next;
   end;
   Output.SaveToFile(EGMDir + 'INPUT.DAT');
   {$IfDef RecordExports} WriteLineToDebugFile('INPUT.DAT created'); {$EndIf}
   Output.Clear;
   CHDir(EGMDir);
   WinExecAndWait32(EXEname);
   {$IfDef RecordExports} WriteLineToDebugFile('NGA code complete'); {$EndIf}
   Output.LoadFromFile(EGMDir + OutName);
   GISdb[DBonTable].MyData.InsureFieldPresentAndAdded(ftFloat,fName,10,3);
   GISdb[DBonTable].MyData.First;
   i := 0;
   while not GISdb[DBonTable].MyData.eof do begin
      if GISdb[DBonTable].GetLatLongToRepresentRecord(Lat,Long) then begin
         TStr := Trim(Output.Strings[i]);
         TStr := Trim(AfterSpecifiedCharacter(TStr,' '));
         TStr := Trim(AfterSpecifiedCharacter(TStr,' '));
         GISdb[DBonTable].MyData.Edit;
         GISdb[DBonTable].MyData.SetFieldByNameAsString(fName,Tstr);
         inc(i);
      end;
      GISdb[DBonTable].MyData.Next;
   end;
   {$IfDef RecordExports} WriteLineToDebugFile('AddEGMtoDBfromSphHarmonics out'); {$EndIf}
end;


procedure FusionTinCreate(InName,OutName : PathStr; GridSize : float64; GridZone : integer; HemiChar : ansichar);
var
   cmd : ansistring;
   tName : PathStr;
   ext : ExtStr;
begin
   {$IfDef RecordUseOtherPrograms} WriteLineToDebugFile('FusionTinCreate, infile=' + InName + '  outfile=' + OutName); {$EndIf}
   tName := NextFileNumber(MDtempDir,'fusion_int_dem_', '.dtm');
   cmd := ProgramRootDir + 'fusion\tinsurfacecreate ' + tName + ' ' + RealToString(GridSize,-12,-2) + ' m m 1 ' +  IntToStr(GridZone) + ' 2 2 ' + InName;
   WinExecAndWait32(cmd);

   if FileExists(tName) then begin
      OpenNewDEM(tName);
      if (OutName <> '') then begin
         Ext := UpperCase(ExtractFileExt(OutName));
         if (Ext = '.TIF') then DEMGlb[LastDEMLoaded].SaveAsGeotiff(OutName)
         else DEMGlb[LastDEMLoaded].WriteNewFormatDEM(OutName);
      end;
   end
   else begin
      MessageToContinue('Fusion failed (out of memory?)');
   end;
end;


procedure laslibReproject(ask : boolean);
var
   FilesWanted : tStringList;
   DefaultFilter : byte;
   fName : PathStr;
   i : integer;
begin
   {$IfDef RecordReformat} WriteLineToDebugFile('laslibReproject'); {$EndIf}
   if Ask then begin
      ReadDefault('Assign (Source) EPSG',MDDef.a_epsg);
      ReadDefault('Target EPSG',MDDef.t_epsg);
   end
   else begin
      MDDef.a_epsg := 27700;
      MDDef.t_epsg := 32630;
   end;
   FilesWanted := tStringList.Create;
   FilesWanted.Add(MainMapData);
   DefaultFilter := 1;
   if GetMultipleFiles('lidar files for lablib reprojection','LAZ|*.laz;*.las',FilesWanted,DefaultFilter) then begin
      for I := 0 to pred(FilesWanted.Count) do begin
         fName := FilesWanted.Strings[i];
         {$IfDef RecordReformat} WriteLineToDebugFile(fName); {$EndIf}
         GDALreprojectLASfile(fName,MDDef.t_epsg,MDDef.a_epsg);
      end;
   end;
   {$IfDef RecordReformat} WriteLineToDebugFile('laslibReproject out'); {$EndIf}
end;



function GPSBabel_fit2gpx(inname,outname : PathStr) : boolean;
var
   GPSBabelEXEName : PathStr;
begin
   GPSBabelEXEName := 'j:\gis_software\gpsbabel\gpsbabel.exe';
   Result := FileExists(GPSBabelExeName);
   if Result then begin
      WinExecAndWait32('"' + GPSBabelExeName + '" -i garmin_fit -o gpx -f ' + inName + ' -F ' + OutName);
      if MDDef.DeleteFIT and FileExists(OutName) then File2Trash(InName);
   end
   else begin
      MessageToContinue('Failure, Missing GPSbabel');
   end;
end;


function MCC_lidarPresent : boolean;
begin
   Result := true;
   if (mcc_lidarFName = '') or (not FileExists(mcc_lidarFName)) then begin
      mcc_lidarFName := 'C:\microdem\mcc_lidar\bin\mcc-lidar.exe';
      if not GetFileFromDirectory('mcc_lidar.exe','mcc_lidar.exe',mcc_lidarFName) then begin
         MessageToContinue('Cannot find mcc-lidar.exe');
         Result := false;
      end;
   end;
end;


procedure ACOLITEprocessing(MapOwner : tMapForm; OpenMaps : boolean = true);
const
   s2res : array[0..2] of integer = (10,20,60);
var
   Base,OutPath,set_File,fName : PathStr;
   i : integer;
   TheFiles,
   paramsfile,fileNames : tStringList;


   procedure OpenMap(fName : PathStr; Check : shortstring);
   var
      Result : integer;
   begin
      if StrUtils.AnsiContainsText(fName,Check) then begin
         Result := OpenNewDEM(fName);
         DEMGlb[Result].DEMheader.ElevUnits := euUndefined;
         DEMGlb[Result].SelectionMap.MapDraw.MapType := mtElevSpectrum;
         DEMGlb[Result].SelectionMap.DoBaseMapRedraw;
      end;
   end;


begin
   {$IfDef RecordACOLITE} WriteLineToDebugFile('ACOLITEprocessing in '); {$EndIf}
   if (UpperCase(ExtractFileExt(MDDef.acolite_fName)) <> '.EXE') then MDDef.acolite_fName := '';
   if not FileExists(MDDef.acolite_fName) then begin
      {$IfDef RecordACOLITE} WriteLineToDebugFile('ACOLITE exe not found'); {$EndIf}
      MDDef.acolite_fName := ProgramRootDir + 'acolite_py_win_20210802.0\acolite_py_win\dist\acolite\acolite.exe';
      if not FileExists(MDDef.acolite_fName) then begin
         GetExistingFileName('ACOLITE EXE','*.exe',MDDef.acolite_fName);
      end;
   end;
   if not FileExists(MDDef.acolite_fName) then begin
      MessageToContinue('ACOLITE exe not found');
      exit;
   end;
   if (UpperCase(ExtractFileExt(MDDef.acolite_fName)) <> '.EXE') then begin
      MessageToContinue('Requires acolite.exe');
      MDDef.acolite_fName := '';
      exit;
   end;

   TheFiles := tStringList.Create;
   FindMatchingFiles(SatImage[MapOwner.MapDraw.SatOnMap].LandsatDir,'*.jp2',TheFiles,8);
   if SatImage[MapOwner.MapDraw.SatOnMap].IsSentinel2 and (TheFiles.Count = 0) then begin
      MessageToContinue('No JP2 files found in directory');
   end
   else begin
      set_file := Petmar.NextFileNumber(MDTempDir, 'acolite_','.txt');
      paramsfile := tStringList.Create;

      Base := MainMapData + 'acolite\';
      SafeMakeDir(Base);
      OutPath := NextFilePath(Base + LastSubDir(SatImage[MapOwner.MapDraw.SatOnMap].LandsatDir) + '_' + BBtoPathString(MapOwner.MapDraw.MapCorners.BoundBoxGeo));

      paramsfile.add('## ACOLITE settings');
      paramsfile.add('inputfile=' + SatImage[MapOwner.MapDraw.SatOnMap].LandsatDir);
      paramsfile.add('output=' + OutPath);
      paramsfile.add('polygon=');
      paramsfile.add('l2w_parameters=' + MDDef.l2w_Params);
      paramsfile.add('rgb_rhot=True');
      paramsfile.add('rgb_rhos=True');
      paramsfile.add('map_l2w=True');
      if SatImage[MapOwner.MapDraw.SatOnMap].IsSentinel2 then paramsfile.add('s2_target_res=' + IntToStr(S2Res[MDDef.acoliteS2res]));

      paramsfile.add('limit=' + RealToString(MapOwner.MapDraw.MapCorners.BoundBoxGeo.YMin,-8,-4) + ',' + RealToString(MapOwner.MapDraw.MapCorners.BoundBoxGeo.XMin,-8,-4) + ',' +
           RealToString(MapOwner.MapDraw.MapCorners.BoundBoxGeo.YMax,-8,-4) + ',' + RealToString(MapOwner.MapDraw.MapCorners.BoundBoxGeo.XMax,-8,-4));
      paramsfile.SaveToFile(set_file);
      paramsfile.free;
      WinExecAndWait32(MDDef.acolite_fName + ' --cli --settings=' + set_file);
      {$IfDef RecordACOLITE} WriteLineToDebugFile('ACOLITE ran'); {$EndIf}
      FileNames := Nil;
      Petmar.FindMatchingFiles(OutPath,'*.tif',FileNames);
      for I := 0 to pred(FileNames.Count) do begin
         fName := FileNames.Strings[i];

         if MDDef.acolite_delete_misc and StrUtils.AnsiContainsText(fName,'raa.tif') then SysUtils.DeleteFile(fName)
         else if MDDef.acolite_delete_misc and StrUtils.AnsiContainsText(fName,'sza.tif') then SysUtils.DeleteFile(fName)
         else if MDDef.acolite_delete_misc and StrUtils.AnsiContainsText(fName,'vza.tif') then SysUtils.DeleteFile(fName)
         else if MDDef.acolite_delete_misc and StrUtils.AnsiContainsText(fName,'flags.tif') then SysUtils.DeleteFile(fName)
         else if MDDef.acolite_delete_rhos and StrUtils.AnsiContainsText(fName,'_rhos_') then SysUtils.DeleteFile(fName)
         else if MDDef.acolite_delete_rhot and StrUtils.AnsiContainsText(fName,'_rhot_') then SysUtils.DeleteFile(fName)
         else begin
            if OpenMaps then begin
               OpenMap(fName,'tur_nechad2016');
               OpenMap(fName,'spm_nechad20');
               OpenMap(fName,'ndci');
               OpenMap(fName,'chl_re_mishra');
            end;
         end;
      end;
      if MDDef.acolite_delete_nc then begin
         Petmar.FindMatchingFiles(OutPath,'*.nc',FileNames);
         for I := 0 to pred(FileNames.Count) do begin
            fName := FileNames.Strings[i];
            SysUtils.DeleteFile(fName);
         end;
      end;
      FileNames.Destroy;
   end;
   TheFiles.Destroy;
   {$IfDef RecordACOLITE} WriteLineToDebugFile('ACOLITEprocessing out'); {$EndIf}
end;



{$IfDef ExRVT}
{$Else}

    procedure RVTgrids(DEM : integer);
    {Relief Visualization Toolbox}
    var
       RVTEXE,NewDEMName,NewDir : PathStr;
       bfile : tStringList;
    begin
       {$IfDef RecordSaveProblems} WriteLineToDebugFile('RVTgrids in'); {$EndIf}

       RVTEXE := 'J:\gis_software\rvt\RVT_2.2.1_Win64.exe';
       FindDriveWithFile(RVTEXE);
       if ValidPath(ExtractFilePath(RVTEXE)) then begin
          NewDir := ExtractFilePath(DEMGlb[DEM].DEMFileName) + 'rvt\';
          SafeMakeDir(NewDir);
          NewDEMName := NewDir + ExtractFileNameNoExt(DEMGlb[DEM].DEMFileName) + '.tif';
          DEMGlb[DEM].SaveAsGeotiff(NewDEMName);
          bfile := TStringList.Create;
          Bfile.add(NewDEMName);
          bFile.SaveToFile(ExtractFilePath(RVTExe) + 'settings\process_files.txt');
          bfile.Clear;
          bfile.Add('cd ' + ExtractFilePath(RVTexe));
          bFile.Add(RVTEXE);
          EndBatchFile(MDTempDir + 'rvt.bat',bfile);
       end
       else MessageToContinue('Requires ' + RVTEXE);
    end;
{$EndIf}


{$IfDef ExOTB}
{$Else}

  procedure OTB_PickToConcatenateImages(SatOnMap : integer);
  var
     InNames: tStringList;
     OutName : PathStr;
     i : integer;
  begin
     if ValidSatImage(SatOnMap) then begin
        InNames := tStringList.Create;
        for i := 1 to SatImage[SatOnMap].NumBands do
           if SatImage[SatOnMap].IsLandsatImageAnalysisBand(i) then
              InNames.Add(SatImage[SatOnMap].TiffImage[i].TiffFileName);
        OutName := ExtractFilePath(SatImage[SatOnMap].TiffImage[1].TiffFileName) + 'merge.tif';
        OTB_ConcatenateImages(InNames,OutName);
     end;
  end;


  procedure OTB_PickForKMeansClustering(SatOnMap : integer);
  var
     InName,OutName : PathStr;
  begin
     {$If Defined(RecordOTB)} WriteLineToDebugFile('TMapForm.Kmeansclustering1Click in');{$EndIf}
     InName := ExtractFilePath(SatImage[SatOnMap].TiffImage[1].TiffFileName) + 'merge.tif';
     if not FileExists(InName) then begin
        {$If Defined(RecordOTB)} WriteLineToDebugFile('Concatenate to ' + InName);{$EndIf}
        OTB_PickToConcatenateImages(SatOnmap);
     end;
     OutName := ExtractFilePath(SatImage[SatOnMap].TiffImage[1].TiffFileName) + 'merge_cluster.tif';
     {$If Defined(RecordOTB)} WriteLineToDebugFile('try to k-means to ' + OutName);{$EndIf}
     OTB_KMeansClassification(InName, OutName);
     if FileExists(OutName) then begin
        OpenNewDEM(OutName);
     end
     else begin
        {$If Defined(RecordOTB)} WriteLineToDebugFile('failed creation, ' + OutName);{$EndIf}
     end;
  end;


    procedure StartOTBbatchFile(var BatchFile : tstringList);
    var
       tf : tstringList;
       i : integer;
    begin
       OTB_Dir := ProgramRootDir + 'OTB\';
       ChDir(Otb_dir);
       BatchFile := tStringList.Create;
       tf := tstringlist.create;
       tf.LoadFromFile(OTB_dir + 'start_devenv.bat');
       for I := 0 to pred(tf.Count) do if trim(tf.Strings[i]) <> '@cmd' then BatchFile.Add(tf.strings[i]);
       tf.free;
    end;


    procedure OTB_ConcatenateImages(InNames: tStringList; OutName : PathStr);
    var
       BatchFile : tStringList;
       tstr : ansistring;
       i : integer;
    begin
       StartOTBbatchFile(BatchFile);
       tStr := ' ';
       for I := 0 to pred(InNames.Count) do tStr := tstr + InNames.Strings[i] + ' ';
       BatchFile.Add('otbcli_ConcatenateImages -il ' + TStr + '-out ' + OutName);
       EndBatchFile(Otb_dir + 'otb_concat.bat', BatchFile);
    end;


    procedure OTB_Segmentation(InName, OutName : PathStr);
    var
       BatchFile : tStringList;
    begin
       StartOTBbatchFile(BatchFile);
       BatchFile.Add('otbcli_Segmentation -in ' + InName + '  -mode raster -mode.raster.out ' + OutName + ' -filter watershed');
       EndBatchFile(Otb_dir + 'otb_segment.bat', BatchFile);
    end;


    procedure OTB_PanSharpen(PanName,OrthoName, OutName : PathStr);
    var
       BatchFile : tStringList;
    begin
       StartOTBbatchFile(BatchFile);
       BatchFile.Add('otbcli_Pansharpening -inp ' + PanName +  ' -inxs ' + OrthoName + ' -out ' + OutName + ' uint16');
       EndBatchFile(Otb_dir + 'otb_pan_sharpen.bat', BatchFile);
    end;

    procedure OTB_KMeansClassification(InName, OutName : PathStr);
    var
       BatchFile : tStringList;
    begin
       StartOTBbatchFile(BatchFile);
       BatchFile.Add('otbcli_KMeansClassification -in ' + InName + ' -ts 1000 -nc 25 -maxit 1000 -out ' + OutName + ' uint8');
       EndBatchFile(Otb_dir + 'otb_kmeans.bat', BatchFile);
    end;

{$EndIf}



initialization
finalization
end.




