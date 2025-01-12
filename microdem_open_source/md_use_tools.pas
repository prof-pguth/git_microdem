unit md_use_tools;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{________________________________________}


{$I nevadia_defines.inc}


//{$Define ConvertDBFtoDB}


{$IfDef RecordProblems}  //normally only defined for debugging specific problems
   {$IFDEF DEBUG}
      //{$Define RecordWBT}
      //{$RecordSAGA}
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
      {$Define RecordMapProj}
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


{$IfDef ExLAStools}
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
   function WBT_ProfileCurvature(OpenMap : boolean; InName : PathStr; OutName : PathStr = ''): integer;
   function WBT_HillshadeMap(OpenMap : boolean; DEM : integer; OutName : PathStr = '') : integer;

   function WBT_PlanCurvature(OpenMap : boolean; InName : PathStr; OutName : PathStr = ''): integer;
   function WBT_TangentialCurvature(OpenMap : boolean; InName : PathStr; OutName : PathStr = '') : integer;
   function WBT_MinimalCurvature(OpenMap : boolean; InName : PathStr): integer;
   function WBT_MaximalCurvature(OpenMap : boolean; InName : PathStr): integer;
   function WBT_MeanCurvature(OpenMap : boolean; InName : PathStr): integer;
   function WBT_GaussianCurvature(OpenMap : boolean; InName : PathStr): integer;

   function WBT_TRI(OpenMap : boolean; InName : PathStr; OutName : PathStr = '') : integer;
   function WBT_AvgNormVectAngDev(InName : PathStr; filtersize : integer) : integer;
   function WBT_CircularVarianceOfAspect(OpenMap : boolean; InName : PathStr; filtersize : integer) : integer;
   function WBT_DrainageBasins(InName : PathStr) : integer;
   function WBT_Geomorphons(OpenMap : boolean; InName : PathStr; Search : integer=50; Skip : integer = 0) : integer;
   function WBT_AspectMap(OpenMap : boolean; InName : PathStr; OutName : PathStr = '') : integer;
   function WBT_BNearNeighCreate(OpenMap : boolean; InName,OutName : PathStr; GridSize : float64) : integer;
   function WBT_WetnessIndex(OpenMap,D8 : boolean; DEMName : PathStr; var WetnessName : PathStr) : integer;
   function WBT_Breach_Depression(DEMName : PathStr; var BreachName : PathStr) : integer;
   function WBT_FlowAccumulation(OpenMap,Log,D8 : boolean; DEMName : PathStr; var BreachName, FlowAccName : PathStr) : integer;
   function WBT_Extract_Streams(OpenMap : boolean; DEMName : PathStr; var BreachName,FlowAccumulationName,StreamName : PathStr; Threshhold : float32 = 100.0) : integer;
   function WBT_ElevAboveStream(OpenMap : boolean; DEMName : PathStr; BreachName,FlowAccumulationName,StreamName,HANDName : PathStr; Threshhold : float32 = 100.0) : integer;
   function WBT_FeaturePreserveSmooth(OpenMap : boolean; InName : PathStr; zUnits : byte; OutName : PathStr = '') : integer;

   procedure WBT_IDWCreate(OpenMap : boolean; InName,OutName : PathStr; GridSize : float64);
   procedure WBT_PennockLandformClass(InName : PathStr; SmoothFirst : boolean);
   procedure WBT_GridFillMissingData(InName : PathStr; TheElevUnits : tElevUnit; OutName : PathStr = '');
   procedure WBT_GeotiffMetadata(InName : PathStr);
   procedure WBT_MultiscaleRoughness(InName : PathStr);
   procedure WBT_KappaIndex(ClassifiedName,ReferenceName : PathStr; HTMLname : PathStr = '');
{$EndIf}


{$IfDef ExSAGA}
{$Else}
   procedure SAGA_all_DEMs_remove_sinks;
   function SagaTRIMap(OpenMap : boolean; InName : PathStr; OutName : PathStr = '') : integer;
   function SagaTPIMap(OpenMap : boolean; InName : PathStr) : integer;
   function SagaVectorRuggednessMap(OpenMap : boolean; InName : PathStr; Radius : integer) : integer;
   function SagaSinkRemoval(InName : PathStr; OutName : PathStr = '') : integer;
   function SagaChannelNetworkGrid(OpenMap : boolean; InName : PathStr; OutGridName : PathStr = '') : integer;
   function SagaChannelShapefile(InName : PathStr; ChannelName : PathStr = '') : integer;
   function SAGAedgeContaminationMap(InName : PathStr; OutName : PathStr = '') : integer;
   function SagaWatershedBasins(InName : PathStr; BasinGrid : PathStr = ''; ChannelNetwork : PathStr = ''; OutName : PathStr = '') : integer;
   function SAGA_WatershedBasinsWangLiu(InName : PathStr) : integer;
   function SAGA_StrahlerOrderGrid(InName : PathStr; OutName : PathStr = '') : integer;
   function SAGA_FlowAccumulationParallizeable(InName : PathStr; OutName : PathStr = '') : integer;
   function SAGA_LSFactor(OpenMap : boolean; InName : PathStr; LSGridName : PathStr = '') : integer;
   function SAGA_Slope_percent(OpenMap : boolean; SlopeMethod : char; InName : PathStr; SlopeFName : PathStr = '') : integer;
   function SAGA_Aspect(OpenMap : boolean; InName : PathStr; AspectFName : PathStr = '') : integer;
   function SAGA_ConvergenceIndex(OpenMap : boolean; InName : PathStr; ConIndexGridName : PathStr = '') : integer;
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
{$EndIf}

procedure RVTgrids(DEM : integer);

procedure FusionTinCreate(InName,OutName : PathStr; GridSize : float64; GridZone : integer; HemiChar : ansichar);

function MCC_lidarPresent : boolean;

function GPSBabel_fit2gpx(inname,outname : PathStr) : boolean;

procedure ACOLITEprocessing(MapOwner : tMapForm; OpenMaps : boolean = true);

procedure laslibReproject(ask : boolean);

procedure AddEGMtoDBfromSphHarmonics(DBonTable : integer; Do2008 : boolean);


implementation


uses
   DEMDef_routines,
   DEMCoord,
   DEMeros,
   PetDButils,
   DEM_Manager,
   DEMDataBase,
   gdal_tools,
   las_lidar,
   geotiff,
   nevadia_main;

const
   WBNoCompress = ' --compress_rasters=false ';


{$i saga_wrapper.inc}

{$I wbt_wrapper.inc}

{$I lastools_wrapper.inc}


function ClearGRASSdirectory : shortstring;
begin
   ClearGRASSdirectory := 'rd /S /Q ' + MDtempDir + 'grass1';
end;


function BBtoPathString(bb : sfBoundBox; Decs : integer = 2) : shortstring;
begin
   Result := RealToString(bb.ymin,-8,Decs) + '_' + RealToString(bb.xmin,-8,Decs) + '_' + RealToString(bb.ymax,-8,Decs) + '_' + RealToString(bb.xmax,-8,Decs);
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




const
   GetGrassExtensions : boolean = false;


function ExecuteGrassAndOpenMap(var BatchFile : tstringList; BatchName,OutName : PathStr; eu : tElevUnit; mt : tMapType; OpenMap : boolean = true) : integer;
begin
   {$IfDef RecordWBT} WriteLineToDebugFile('ExecuteGrassAndOpenMap, bf=' + BatchName); {$EndIf}
   BatchName := Petmar.NextFileNumber(MDTempDir,BatchName,'.bat');
   BatchFile.Add(ClearGRASSdirectory);
   EndBatchFile(BatchName,batchfile);

   {$IfDef RecordWBT} WriteLineToDebugFile('Batch file over'); {$EndIf}
   if FileExists(OutName) then begin
      Result := OpenNewDEM(OutName,false);
      DEMGlb[Result].DEMheader.ElevUnits := eu;
      if not PossibleElevationUnits(DEMGlb[Result].DEMheader.ElevUnits) then DEMGlb[Result].DEMHeader.VerticalCSTypeGeoKey := VertCSUndefined;

      if OpenMap then CreateDEMSelectionMap(Result,true,true,mt);
      {$IfDef RecordWBT} WriteLineToDebugFile('ExecuteGrassAndOpenMap map opened'); {$EndIf}
      {$If Defined(RecordMapProj)} WriteLineToDebugFile('ExecuteGrassAndOpenMap ' + DEMGlb[Result].AreaName + '  ' + DEMGlb[Result].DEMMapProj.GetProjName); {$EndIf}
   end
   else MessageToContinue('Grass failure, try command in DOS window: ' + BatchName);
   {$IfDef RecordWBT} WriteLineToDebugFile('ExecuteGrassAndOpenMap out'); {$EndIf}
end;


function AssembleGrassCommand(InName : PathStr; GridName,CommandName,NewLayer,BatchName : ShortString; eu : tElevUnit; mt : tMapType;
   OutName : PathStr = ''; OpenMap : boolean = true; TypeStr : shortstring = '32') : integer;
var
   BatchFile : tStringList;

      procedure StartGrassBatchFile(var BatchFile : tStringList; InName : PathStr);
      begin
         BatchFile := tStringList.Create;
         BatchFile.Add(ClearGrassDirectory);
         //if (GrassEXE = 'grass83') then begin
            BatchFile.Add('call "C:\OSGeo4W\bin\o4w_env.bat"');
            BatchFile.Add(SetGDALdataStr);
            BatchFile.Add('set USE_PATH_FOR_GDAL_PYTHON=YES');
            BatchFile.Add(MDDef.GRASSexe + ' -c ' + InName + ' ' + MDTempDir + 'grass1\ --exec r.in.gdal input=' + InName + ' output=mymap |more');
         //end;
      end;

begin
  if FileExistsErrorMessage(InName) then begin
     if (OutName = '') then OutName := MDTempDir + GridName + ExtractFileNameNoExt(InName) + '.tif';

     BatchFile := tStringList.Create;
     BatchFile.Add(ClearGrassDirectory);
     BatchFile.Add('call "C:\OSGeo4W\bin\o4w_env.bat"');
     BatchFile.Add(SetGDALdataStr);
     BatchFile.Add('set USE_PATH_FOR_GDAL_PYTHON=YES');
     BatchFile.Add(MDDef.GrassEXE + ' -c ' + InName + ' ' + MDTempDir + 'grass1\ --exec r.in.gdal input=' + InName + ' output=mymap |more');
     BatchFile.Add(MDDef.GrassEXE + ' ' + MDTempDir + 'grass1\PERMANENT --exec ' + CommandName  + ' |more');
     BatchFile.Add(MDDef.GrassEXE + ' ' + MDTempDir + 'grass1\PERMANENT --exec r.out.gdal input=' + NewLayer + ' out=' + OutName + ' type=Float' + TypeStr + ' --overwrite --quiet |more');

     if GetGrassExtensions then begin   //add these to get the extensions; they need to be done with a grass workspace set up, so they are here
        BatchFile.Add(MDDef.GrassEXE + ' ' + MDTempDir + 'grass1\PERMANENT --exec g.extension r.vector.ruggedness |more');
        BatchFile.Add(MDDef.GrassEXE + ' ' + MDTempDir + 'grass1\PERMANENT --exec g.extension r.tri |more');
        BatchFile.Add(MDDef.GrassEXE + ' ' + MDTempDir + 'grass1\PERMANENT --exec g.extension r.tpi |more');
        GetGrassExtensions := false;
     end;
     Result := ExecuteGrassAndOpenMap(BatchFile,BatchName,OutName,eu,mt,OpenMap);
  end;
end;


function GrassVectorRuggedness(InName : PathStr; WindowSize : integer; OutName : PathStr = '') : integer;
var
   PartialResults : shortstring;
begin
   (*
      //Oct 2024, this might now work, but has not been tested recently
      //this only is valid for GRASS 82, and we are using 78
      PartialResults :=  'strength=' + MDTempDir + 'grass_vector_strength' + ExtractFileNameNoExt(InName) + '.tif' +
                      ' fisher=' + MDTempDir + 'grass_fisher_k' + ExtractFileNameNoExt(InName) + '.tif';
   *)
   PartialResults := 'size=' + IntToStr(WindowSize);
   Result := AssembleGrassCommand(InName,'grass_vector_ruggedness_' + FilterSizeStr(WindowSize) + '_','r.vector.ruggedness elevation=mymap output=rugged ' +
      PartialResults +  ' nprocs=-1','rugged','GrassVectorRugged_',euUndefined,mtElevSpectrum,OutName);
end;


procedure GetGrassExtensionsNow(InName : PathStr);
begin
   GetGrassExtensions := true;
   GrassSlopeMap(InName);
end;


function GrassDownsampleAverage(InName : PathStr) : integer;
begin
   //this requires a region, which will define the new spacing
   //unclear if would change the projection, say from UTM to geographic
   Result := AssembleGrassCommand(InName,'grass_downsample_','r.resamp.stats elevation=mymap slope=slope format=percent','slope','GrassSlope_',euMeters,mtElevSpectrum);
end;


function GrassTRIMap(OpenMap : boolean; InName : PathStr; OutName : PathStr = '') : integer;
begin
   Result := AssembleGrassCommand(InName,'grass_TRI_','r.tri input=mymap output=tri ','tri','GrassTRI_',euUndefined,mtElevSpectrum,OutName,OpenMap);
end;


function GrassTPIMap(InName : PathStr; OpenMap : boolean = true; OutName : PathStr = '') : integer;
begin
//r.tpi input=elevation@PERMANENT minradius=1 maxradius=25 steps=5 output=tpi
//maxradius=25 fails (but might work in the user interface for GRASS?
//variants of the option below, trying to get for just a single radius, failed
//Result := AssembleGrassCommand(InName,'grass_TPI_','r.tpi input=mymap minradius=1 maxradius=5 steps=2 output=tpi','tpi','GrassTPI_',Undefined,mtElevSpectrum,'64');
   Result := AssembleGrassCommand(InName,'grass_TPI_','r.tpi input=mymap minradius=1 maxradius=15 steps=5 output=tpi','tpi','GrassTPI_',euUndefined,mtElevSpectrum,OutName,OpenMap,'64');
end;


function GrassSlopeMap(InName : PathStr; OpenMap : boolean = true; OutName : PathStr = '') : integer;
//uses Horn algorithm
begin
   Result := AssembleGrassCommand(InName,'grass_slope_','r.slope.aspect elevation=mymap slope=slope format=percent','slope','GrassSlope_',euPercentSlope,MDDef.DefSlopeMap,OutName,OpenMap);
end;

function GrassAspectMap(InName : PathStr; OpenMap : boolean = true; OutName : PathStr = '') : integer;
begin
   Result := AssembleGrassCommand(InName,'grass_aspect_','r.slope.aspect elevation=mymap aspect=aspect format=percent -n','aspect','GrassAspect_',euAspectDeg,mtDEMaspect,OutName,OpenMap);
end;


//additional GRASS curvatures in
//   https://grass.osgeo.org/grass83/manuals/r.param.scale.html
//   https://grass.osgeo.org/grass83/manuals/v.surf.rst.html

function Grass_dx_partial(InName : PathStr; OpenMap : boolean = true; OutName : PathStr = '') : integer;
begin
   Result := AssembleGrassCommand(InName,'grass_dx_partial_','r.slope.aspect elevation=mymap dx=dx','dx','GrassDX_',euPerMeter,mtElevSpectrum,OutName,OpenMap);
end;


function Grass_dy_partial(InName : PathStr; OpenMap : boolean = true; OutName : PathStr = '') : integer;
begin
   Result := AssembleGrassCommand(InName,'grass_dy_partial_','r.slope.aspect elevation=mymap dy=dy','dy','GrassDY_',euPerMeter,mtElevSpectrum,OutName,OpenMap);
end;

function Grass_dxx_partial(InName : PathStr; OpenMap : boolean = true; OutName : PathStr = '') : integer;
begin
   Result := AssembleGrassCommand(InName,'grass_dxx_partial_','r.slope.aspect elevation=mymap dxx=dxx','dxx','GrassDXX_',euPerMeter,mtElevSpectrum,OutName,OpenMap);
end;


function Grass_dyy_partial(InName : PathStr; OpenMap : boolean = true; OutName : PathStr = '') : integer;
begin
   Result := AssembleGrassCommand(InName,'grass_dyy_partial_','r.slope.aspect elevation=mymap dyy=dyy','dyy','GrassDYY_',euPerMeter,mtElevSpectrum,OutName,OpenMap);
end;


function Grass_dxy_partial(InName : PathStr; OpenMap : boolean = true; OutName : PathStr = '') : integer;
begin
   Result := AssembleGrassCommand(InName,'grass_dxy_partial_','r.slope.aspect elevation=mymap dxy=dxy','dxy','GrassDXY_',euPerMeter,mtElevSpectrum,OutName,OpenMap);
end;


function GrassProfileCurvatureMap(InName : PathStr; OpenMap : boolean = true; OutName : PathStr = '') : integer;
begin
   Result := AssembleGrassCommand(InName,'grass_profile_curvature_','r.slope.aspect elevation=mymap pcurvature=pcurve','pcurve','GrassProfCurv_',euPerMeter,MDDef.DefCurveMap,OutName,OpenMap);
end;


function GrassTangentialCurvatureMap(InName : PathStr; OpenMap : boolean = true; OutName : PathStr = '') : integer;
begin
   Result := AssembleGrassCommand(InName,'grass_tangential_curvature_','r.slope.aspect elevation=mymap tcurvature=tcurve','tcurve','GrassTang_',euPerMeter,MDDef.DefCurveMap,OutName,OpenMap);
end;



procedure RVTgrids(DEM : integer);
{Relief Visualization Toolbox}
var
   RVTEXE,NewDEMName,NewDir : PathStr;
   bfile : tStringList;
begin
   {$IfDef RecordSaveProblems} WriteLineToDebugFile('RVTgrids in'); {$EndIf}

   RVTEXE := 'C:\microdem\rvt\RVT_1.3_Win64.exe';
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



{$IfDef ExOTB}
{$Else}

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
       //BatchFile.Add('otbcli_KMeansClassification -in ' + InName + ' -ts 1000 -nc 25 -maxit 1000 -nodata 0 -out ' + OutName + ' uint8');  //nodata removed, Oct 2021
       BatchFile.Add('otbcli_KMeansClassification -in ' + InName + ' -ts 1000 -nc 25 -maxit 1000 -out ' + OutName + ' uint8');
       EndBatchFile(Otb_dir + 'otb_kmeans.bat', BatchFile);
    end;

{$EndIf}



initialization
finalization
end.




(*
procedure TauDEMOp(DEM : integer); //TauDEM : tTauDEM);
var
   NewDEMName,NewDir : PathStr;
   bfile : tStringList;
begin
   {$IfDef RecordSaveProblems} WriteLineToDebugFile('TauDEMOp in'); {$EndIf}

   TaudemDir := 'C:\Program Files\TauDEM\TauDEM5Exe\';
   if ValidPath(TauDEMDir) then begin
      NewDir := ExtractFilePath(DEMGlb[DEM].DEMFileName) + 'taudem\';
      SafeMakeDir(NewDir);
      NewDEMName := NewDir + ExtractFileNameNoExt(DEMGlb[DEM].DEMFileName) + '.tif';
      DEMGlb[DEM].SaveAsGeotiff(NewDEMName);
      bfile := tStringList.Create;
      bFile.Add(ExtractShortPathName(TauDEMDir + 'PitRemove.exe') + ' ' + NewDEMName);
      bFile.Add(ExtractShortPathName(TauDEMDir + 'D8FlowDir.exe') + ' ' +  NewDEMName);
      bFile.Add(ExtractShortPathName(TauDEMDir + 'DInfFlowDir.exe') + ' ' +  NewDEMName);
      bFile.Add(ExtractShortPathName(TauDEMDir + 'AreaD8.exe') + ' ' +  NewDEMName);
      bFile.Add(ExtractShortPathName(TauDEMDir + 'AreaDInf.exe') + ' ' +  NewDEMName);
      bFile.Add(ExtractShortPathName(TauDEMDir + 'Gridnet.exe') + ' ' +  NewDEMName);
      bFile.Add(ExtractShortPathName(TauDEMDir + 'PeukerDouglas.exe') + ' ' + NewDEMName);
      //not working yet
      //bFile.Add(ExtractShortPathName(TauDEMDir + 'streamnet.exe') + ' ' + NewDEMName);
      bFile.Add(ExtractShortPathName(TauDEMDir + 'TWI.exe') + ' ' + NewDEMName);
      EndBatchFile(MDTempDir + 'taudem.bat',bfile);
   end
   else MessageToContinue('Requires ' + TauDEMDir);
end;
*)


