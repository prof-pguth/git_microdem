unit md_use_tools;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}

{$I nevadia_defines.inc}


//{$Define ConvertDBFtoDB}


{$IfDef RecordProblems}  //normally only defined for debugging specific problems
   {$IFDEF DEBUG}
      //{$Define RecordWBT}
      //{$Define RecordSAGA}
      //{$Define RecordSAGAresult}

      //{$Define OpenLasTools}
      //{$Define RecordACOLITE}
      //{$Define RecordSubsetOpen}
      //{$Define RecordUseOtherPrograms}
      //{$Define RecordSaveProblems}
      //{$Define RecordOGR}
      //{$Define RecordGeoPDF}
      //{$Define RecordReformat}
      //{$Define RecordSAGAFull}
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

   System.IOutils,System.UITypes,System.Math,System.UIConsts,
   SysUtils, Classes,StrUtils,
   petmar,Petmar_types,PetMath,
   DEMMapf,DEMMapDraw,DEMDefs,BaseMap,DEM_NLCD;


{$IfDef ExLAStools}
{$Else}
   function GetLASToolsFileName(var fName : PathStr) : boolean;
   function lastools_txt2las_cmd(inName : PathStr; UTMzone : shortString; ParseVals : shortstring = '') : shortstring;
   procedure BlastTinCreate(InName,OutName : PathStr; GridSize : float64);
   procedure CallLasInfo;
   procedure LAStoolsTextToLAS;
   procedure Lastools_DEMToLAZ(InName,OutName : PathStr; Extra : shortString = '');
{$EndIf}


{$IfDef ExWhiteBox}
{$Else}
   function WhiteBoxPresent : boolean;
   function WBT_GroundClassify(InName,OutName : PathStr) : shortString;
   function WBT_LidarSegmentationBasedFilter(InName,OutName : PathStr) : shortString;
   function WBT_DeNoise(InName,OutName : PathStr) : shortString;
   function WBT_SlopeMap(InName : PathStr) : integer;
   function WBT_ProfileCurvature(InName : PathStr): integer;
   function WBT_TangentialCurvature(InName : PathStr): integer;
   function WBT_MinimalCurvature(InName : PathStr): integer;
   function WBT_MaximalCurvature(InName : PathStr): integer;
   function WBT_MeanCurvature(InName : PathStr): integer;
   function WBT_GaussianCurvature(InName : PathStr): integer;
   function WBT_TRI(InName : PathStr) : integer;
   function WBT_AvgNormVectAngDev(InName : PathStr; filtersize : integer) : integer;
   function WBT_CircularVarianceOfAspect(InName : PathStr; filtersize : integer) : integer;
   function WBT_DrainageBasins(InName : PathStr) : integer;
   function WBT_FlowAccumulation(OpenMap,Log,D8 : boolean; InName : PathStr; OutName : PathStr = '') : integer;
   function WBT_WetnessIndex(OpenMap,D8 : boolean; InName : PathStr; OutName : PathStr = '') : integer;
   procedure WBT_IDWCreate(InName,OutName : PathStr; GridSize : float64);
   procedure WBT_PennockLandformClass(InName : PathStr; SmoothFirst : boolean);
   procedure WBT_Geomorphons(InName : PathStr);
   procedure WBT_GridFillMissingData(InName : PathStr; TheElevUnits : tElevUnit);
   procedure WBT_BNearNeighCreate(InName,OutName : PathStr; GridSize : float64);
   procedure WBT_GeotiffMetadata(InName : PathStr);
   procedure WBT_AspectMap(InName : PathStr);
   procedure WBT_MultiscaleRoughness(InName : PathStr);
{$EndIf}


{$IfDef ExSAGA}
{$Else}
   procedure SAGA_all_DEMs_remove_sinks;
   function SagaTRIMap(InName : PathStr) : integer;
   function SagaTPIMap(InName : PathStr) : integer;
   function SagaVectorRuggednessMap(InName : PathStr; Radius : integer) : integer;
   function SagaSinkRemoval(InName : PathStr; OutName : PathStr = '') : integer;
   function SagaChannelNetwork(InName : PathStr; OutName : PathStr = ''; ShpName : PathStr = '') : integer;
   function SagaChannelShapefile(InName : PathStr; ChannelName : PathStr = '') : integer;
   function SAGAedgeContaminationMap(InName : PathStr; OutName : PathStr = '') : integer;
   function SagaWatershedBasins(InName : PathStr; BasinGrid : PathStr = ''; ChannelNetwork : PathStr = ''; OutName : PathStr = '') : integer;
   function SAGA_WatershedBasinsWangLiu(InName : PathStr) : integer;
   function SAGA_StrahlerOrderGrid(InName : PathStr; OutName : PathStr = '') : integer;
   function SAGA_FlowAccumulationParallizeable(InName : PathStr; OutName : PathStr = '') : integer;
   function SAGA_LSFactor(OpenMap : boolean; InName : PathStr; OutName : PathStr = '') : integer;
{$EndIf}


{$IfDef ExGRASS}
{$Else}
   procedure GetGrassExtensionsNow(InName : PathStr);
   function GrassSlopeMap(InName : PathStr) : integer;
   function GrassAspectMap(InName : PathStr) : integer;
   function GrassVectorRuggedness(InName : PathStr; WindowSize : integer) : integer;
   function GrassProfileCurvatureMap(InName : PathStr) : integer;
   function GrassTangentialCurvatureMap(InName : PathStr) : integer;
   function GrassTRIMap(InName : PathStr) : integer;
   function GrassTPIMap(InName : PathStr) : integer;
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
   geotiff,
   nevadia_main;

const
   WBNoCompress = ' --compress_rasters=false ';
   ClearGRASSdirectory = 'rd /S /Q c:\mapdata\temp\grass1';


{$i saga_wrapper.inc}

{$I wbt_wrapper.inc}


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
      EGMDir := ProgramRootDir +  'EGM96_Spherical_Harmonics\';
      EXEName := EGMDir +  'F477.exe';
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
   GPSBabelEXEName := 'G:\gis_software\gpsbabel\GPSBabel\gpsbabel.exe';
   Result := FileExists(GPSBabelExeName);
   if Result then begin
      WinExecAndWait32('"' + GPSBabelExeName + '" -i garmin_fit -o gpx -f ' + inName + ' -F ' + OutName);
      if MDDef.DeleteFIT and FileExists(OutName) then File2Trash(InName);
   end
   else begin
      MessageToContinue('Failure, Missing: ' + GPSBabelExeName);
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
         DEMGlb[Result].DEMheader.ElevUnits := unDefined;
         DEMGlb[Result].SelectionMap.MapDraw.MapType := mtElevSpectrum;
         DEMGlb[Result].SelectionMap.DoBaseMapRedraw;
      end;
   end;


begin
//S2A_MSIL1C_20210507T162901_N0300_R083_T16RCU_20210507T204152.SAFE
//S2A_MSI_2021_09_24_16_45_14_T16RCU_L2W_chl_re_mishra.tif
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
   GrassEXE = 'grass78';
   //GrassPath = 'H:\gis_software\grass_gis_8.2\';        only needed for 8.2
   GetGrassExtensions : boolean = false;


procedure StartGrassBatchFile(var BatchFile : tStringList; InName : PathStr);
begin
   BatchFile := tStringList.Create;
   BatchFile.Add(ClearGrassDirectory);

   if (GrassEXE = 'grass78') then begin
      BatchFile.Add('call "C:\OSGeo4W\bin\o4w_env.bat"');
      BatchFile.Add(SetGDALdataStr);
      BatchFile.Add('set USE_PATH_FOR_GDAL_PYTHON=YES');
      BatchFile.Add('grass78 -c ' + InName + ' c:\mapdata\temp\grass1\ --exec r.in.gdal input=' + InName + ' output=mymap |more');
   end
   else begin
      //this is not yet working
      //BatchFile.Add('call ' + GrassPath + 'grass82.bat');
   end;
end;


function ExecuteGrassAndOpenMap(var BatchFile : tstringList; BatchName,OutName : PathStr; eu : tElevUnit; mt : tMapType) : integer;
begin
   {$IfDef RecordWBT} WriteLineToDebugFile('ExecuteGrassAndOpenMap, bf=' + BatchName); {$EndIf}
   BatchName := Petmar.NextFileNumber(MDTempDir,BatchName,'.bat');
   BatchFile.Add(ClearGRASSdirectory);
   EndBatchFile(BatchName,batchfile);

   {$IfDef RecordWBT} WriteLineToDebugFile('Batch file over'); {$EndIf}
   if FileExists(OutName) then begin
      Result := OpenNewDEM(OutName,false);
      DEMGlb[Result].DEMheader.ElevUnits := eu;
      CreateDEMSelectionMap(Result,true,true,mt);
      {$IfDef RecordWBT} WriteLineToDebugFile('ExecuteGrassAndOpenMap map opened'); {$EndIf}
   end
   else MessageToContinue('Grass failure, try command in DOS window: ' + BatchName);
   {$IfDef RecordWBT} WriteLineToDebugFile('ExecuteGrassAndOpenMap out'); {$EndIf}
end;



function AssembleGrassCommand(InName : PathStr; GridName,CommandName,NewLayer,BatchName : ShortString; eu : tElevUnit; mt : tMapType; TypeStr : shortstring = '32') : integer;
var
   OutName : PathStr;
   BatchFile : tStringList;
begin
  if FileExistsErrorMessage(InName) then begin
     OutName := MDTempDir + GridName + ExtractFileNameNoExt(InName) + '.tif';
     StartGRASSbatchFile(BatchFile,InName);

     BatchFile.Add(GrassEXE + ' c:\mapdata\temp\grass1\PERMANENT --exec ' + CommandName  + ' |more');
     BatchFile.Add(GrassEXE + ' c:\mapdata\temp\grass1\PERMANENT --exec r.out.gdal input=' + NewLayer + ' out=' + OutName + ' type=Float' + TypeStr + ' --overwrite --quiet |more');
     //add these to get the extensions; they need to be done with a grass workspace set up
     if GetGrassExtensions then begin
        BatchFile.Add(GrassEXE + ' c:\mapdata\temp\grass1\PERMANENT --exec g.extension r.vector.ruggedness |more');
        BatchFile.Add(GrassEXE + ' c:\mapdata\temp\grass1\PERMANENT --exec g.extension r.tri |more');
        BatchFile.Add(GrassEXE + ' c:\mapdata\temp\grass1\PERMANENT --exec g.extension r.tpi |more');
        GetGrassExtensions := false;
     end;
     Result := ExecuteGrassAndOpenMap(BatchFile,BatchName,OutName,eu,mt);
  end;
end;

function GrassVectorRuggedness(InName : PathStr; WindowSize : integer) : integer;
var
   PartialResults : shortstring;
begin
   (*
      //this only is valid for GRASS 82, and we are using 78
      PartialResults :=  'strength=' + MDTempDir + 'grass_vector_strength' + ExtractFileNameNoExt(InName) + '.tif' +
                      ' fisher=' + MDTempDir + 'grass_fisher_k' + ExtractFileNameNoExt(InName) + '.tif';
   *)
   PartialResults := 'size=' + IntToStr(WindowSize);
   Result := AssembleGrassCommand(InName,'grass_vector_ruggedness_' + FilterSizeStr(WindowSize) + '_','r.vector.ruggedness elevation=mymap output=rugged ' +
      PartialResults +  ' nprocs=-1','rugged','GrassVectorRugged_',Undefined,mtElevSpectrum);
   //Result := AssembleGrassCommand(InName,'grass_vector_ruggedness_','r.vector.ruggedness elevation=mymap slope=slope aspect=aspect output=rugged nprocs=-1','rugged','GrassRugged_',Undefined,mtElevSpectrum);
end;


procedure GetGrassExtensionsNow(InName : PathStr);
begin
   GetGrassExtensions := true;
   AssembleGrassCommand(InName,'grass_slope_','r.slope.aspect elevation=mymap slope=slope format=percent','slope','GrassSlope_',PercentSlope,mtElevSpectrum);
end;


function GrassDownsampleAverage(InName : PathStr) : integer;
begin
   //this requires a region, which will define the new spacing
   //unclear if would change the projection, say from UTM to geographic
   Result := AssembleGrassCommand(InName,'grass_downsample_','r.resamp.stats elevation=mymap slope=slope format=percent','slope','GrassSlope_',euMeters,mtElevSpectrum);
end;


function GrassTRIMap(InName : PathStr) : integer;
begin
   Result := AssembleGrassCommand(InName,'grass_TRI_','r.tri input=mymap output=tri ','tri','GrassTRI_',Undefined,mtElevSpectrum);
end;

function GrassTPIMap(InName : PathStr) : integer;
begin
//r.tpi input=elevation@PERMANENT minradius=1 maxradius=25 steps=5 output=tpi
//maxradius=25 fails (but might work in the user interface for GRASS?
//variants of the option below, trying to get for just a single radius, failed
//Result := AssembleGrassCommand(InName,'grass_TPI_','r.tpi input=mymap minradius=1 maxradius=5 steps=2 output=tpi','tpi','GrassTPI_',Undefined,mtElevSpectrum,'64');

   Result := AssembleGrassCommand(InName,'grass_TPI_','r.tpi input=mymap minradius=1 maxradius=15 steps=5 output=tpi','tpi','GrassTPI_',Undefined,mtElevSpectrum,'64');
end;


function GrassSlopeMap(InName : PathStr) : integer;
begin
   Result := AssembleGrassCommand(InName,'grass_slope_','r.slope.aspect elevation=mymap slope=slope format=percent','slope','GrassSlope_',PercentSlope,mtElevSpectrum);
end;

function GrassAspectMap(InName : PathStr) : integer;
begin
   Result := AssembleGrassCommand(InName,'grass_aspect_','r.slope.aspect elevation=mymap aspect=aspect format=percent -n','aspect','GrassAspect_',AspectDeg,mtDEMaspect);
end;


function GrassProfileCurvatureMap(InName : PathStr) : integer;
begin
   Result := AssembleGrassCommand(InName,'grass_profile_curvature_','r.slope.aspect elevation=mymap pcurvature=pcurve','pcurve','GrassProfCurv_',euPerMeter,mtElevSpectrum);
end;


function GrassTangentialCurvatureMap(InName : PathStr) : integer;
begin
   Result := AssembleGrassCommand(InName,'grass_tangential_curvature_','r.slope.aspect elevation=mymap pcurvature=tcurve','tcurve','GrassTang_',euPerMeter,mtElevSpectrum);
end;



procedure RVTgrids(DEM : integer);
{Relief Visualization Toolbox}
var
   RVTEXE,NewDEMName,NewDir : PathStr;
   bfile : tStringList;
begin
   {$IfDef RecordSaveProblems} WriteLineToDebugFile('RVTgrids in'); {$EndIf}

   RVTEXE := 'C:\microdem\rvt\RVT_1.3_Win64.exe';
   if PathIsValid(ExtractFilePath(RVTEXE)) then begin
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


function GetLASToolsFileName(var fName : PathStr) : boolean;
var
   BaseName : PathStr;
begin
   Result := true;
   if not FileExists(fName) then begin
      {$If Defined(RecordGDAL) or Defined(RecordGDALOpen) or Defined(OpenLasTools)} HighlightLineToDebugFile('GetGDALFileNames in, but not in ' + fName); {$EndIf}
      BaseName := ExtractFileName(fName);
      LAStools_BinDir := ProgramRootDir + 'lastools\bin\';
      fName := LAStools_BinDir + BaseName;
      if not FileExists(fName) then begin
         {$If Defined(RecordGDAL) or Defined(RecordGDALOpen) or Defined(OpenLasTools)} HighlightLineToDebugFile('File not in ' + fName); {$EndIf}
         if GetDOSPath('LAStools binary directory, something like ' +  LAStools_BinDir,LAStools_BinDir) then begin
            fName := LAStools_BinDir + BaseName;
            if not FileExists(fName) then begin
               {$If Defined(RecordGDAL) or Defined(RecordGDALOpen) or Defined(OpenLasTools)} HighlightLineToDebugFile('After user said, but no go for  ' + fName); {$EndIf}
               MessageToContinue('Could not find ' + fName);
               Result := false;
            end;
         end
         else begin
            Result := false;
         end;
      end;
   end;
end;


procedure LAStoolsTextToLAS;
var
   fName,PName : PathStr;
   cmd : ANSIString;
   UTMzone,ParseVals : shortstring;
   i : integer;
   DefaultFilter : byte;
   Ext : extstr;
   theFileNames,bf : tStringList;
begin
   pName := LAStools_BinDir + 'txt2las.exe';
   if GetLASToolsFileName(pName) then begin
      DefaultFilter := 1;
      theFileNames := tStringList.Create;
      theFileNames.Add(MainMapData);
      if GetMultipleFiles('input ascii file','xyz ASCII file|*.txt;*.csv;*.xyz;*.xyzi;*.yxz',theFileNames,DefaultFilter) then begin
         Ext := UpperCase(ExtractFileExt(theFileNames.Strings[i]));
         ParseVals := '';
         if (Ext = '.TXT') or (Ext = '.CSV') then begin
            ParseVals := 'xyzirc';
            GetString('line interpretation',ParseVals,false,['a'..'z']);
         end;
         UTMzone := '31U';
         GetString('UTM zone',UTMzone,true,ReasonableTextChars);
         bf := tStringList.Create;
         for I := 0 to pred(TheFileNames.Count) do begin
            fName := theFileNames.Strings[i];
            cmd := lastools_txt2las_cmd(fName,UTMZone,ParseVals);
            bf.Add('REM   ' + IntToStr(i) + '/' + IntToStr(TheFileNames.Count) + '   started ' + TimeToStr(now));
            bf.Add('echo %time%');
            bf.add(cmd);
         end;
         EndBatchFile(MDTempDir + 'lastools_txt2las.bat',bf);
      end;
      TheFileNames.Free;
   end;
end;


procedure BlastTinCreate(InName,OutName : PathStr; GridSize : float64);
var
   cmd : ansistring;
   pName : PathStr;
begin
  {$IfDef RecordUseOtherPrograms} WriteLineToDebugFile('BlastTinCreate, infile=' + InName + '  outfile=' + OutName); {$EndIf}
   OutName :=  ChangeFileExt(OutName,'.asc');
   pName := LAStools_BinDir + 'txt2las.exe';
   if GetLASToolsFileName(pName) then begin
      cmd := pName + ' -step ' + RealToString(GridSize,-12,-2) + ' -i ' +InName + ' -o ' + OutName;
      WinExecAndWait32(cmd);
      OpenNewDEM(OutName);
   end;
end;


procedure Lastools_DEMToLAZ(InName,OutName : PathStr; Extra : shortString = '');
var
   cmd : ansistring;
   pName : PathStr;
begin
   pName := LAStools_BinDir + 'demzip.exe';
   if GetLASToolsFileName(pName) then begin
      cmd := pName + ' -i ' + InName + ' -o ' + OutName  + Extra;
      WinExecAndWait32(cmd);
   end;
end;


function lastools_txt2las_cmd(inName : PathStr; UTMzone : shortString; ParseVals : shortstring = '') : shortstring;
var
   pName : PathStr;
begin
   pName := LAStools_BinDir + 'txt2las.exe';
   if GetLASToolsFileName(pName) then begin
     if ParseVals = '' then begin
        ParseVals := LowerCase(ExtractFileExt(inName));
        Delete(ParseVals,1,1);
     end;
     if (UTMZone <> '') then UTMZone := ' -utm ' + UTMzone;
     Result := pName + ' -parse ' + ParseVals + UTMZone + ' -v  -i ' + inName;
   end;
end;


procedure CallLasInfo;
var
   fName,OutName,PName : PathStr;
   cmd : ANSIString;
   i : integer;
   DefaultFilter : byte;
   theFileNames : tStringList;
begin
   pName := LAStools_BinDir + '\lasinfo.exe';
   if GetLASToolsFileName(pName) then begin
      DefaultFilter := 1;
      theFileNames := tStringList.Create;
      theFileNames.Add(LastLidarDirectory);
      if GetMultipleFiles('input LAS file','LAS file|*.las;*.laz',theFileNames,DefaultFilter) then begin
         for I := 0 to pred(TheFileNames.Count) do begin
            fName := theFileNames.Strings[i];
            OutName := MDtempDir + ExtractFileNameNoExt(fName) + '_las_info.txt';
            cmd := pName +  ' -i ' + fName + ' -o ' + OutName;
            WinExecAndWait32(cmd);
            ShowInNotepadPlusPlus(OutName,ExtractFileName(OutName));
        end;
      end;
      TheFileNames.Free;
   end;
end;


initialization
finalization
  {$IfDef RecordGeoPDF} WriteLineToDebugFile('RecordGeoPDF active in md_use_tools'); {$EndIf}
  {$IfDef RecordGDAL} WriteLineToDebugFile('RecordGDAL active in md_use_tools'); {$EndIf}
  {$IfDef RecordOGR} WriteLineToDebugFile('RecordOGR active in md_use_tools'); {$EndIf}
  {$IfDef RecordUseOtherPrograms} WriteLineToDebugFile('RecordUseOtherPrograms active in md_use_tools'); {$EndIf}
  {$IfDef RecordReformat} WriteLineToDebugFile('RecordReformat active in md_use_tools'); {$EndIf}
  {$IfDef RecordSaveProblems} WriteLineToDebugFile('RecordSaveProblems active in md_use_tools'); {$EndIf}
end.




(*
procedure TauDEMOp(DEM : integer); //TauDEM : tTauDEM);
var
   NewDEMName,NewDir : PathStr;
   bfile : tStringList;
begin
   {$IfDef RecordSaveProblems} WriteLineToDebugFile('TauDEMOp in'); {$EndIf}

   TaudemDir := 'C:\Program Files\TauDEM\TauDEM5Exe\';
   if PathIsValid(TauDEMDir) then begin
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


