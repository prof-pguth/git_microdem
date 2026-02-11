unit demix_definitions;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2026 Peter L. Guth  }
{________________________________________}


// DEMIX options under active development.
//Some options are hard coded and not error-trapped, and some options may not longer work.  Used with caution

{$I nevadia_defines.inc}


{$Define IncludeDEMIX_external_programs}


{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   {$Define RecordDEMIX}
   //{$Define TrackCriteriaList}

   //{$Define RecordNeoDEMIX}
   //{$Define RecordOpenExternalProgramGrids}
   //{$Define RecordDEMIXLoad}
   //{$Define RecordTestDEMstart}
   //{$Define RecordDEMIXFilters}
   //{$Define RecordDiluvium}
   //{$Define TrackAverageStats}
   //{$Define RecordDEMIX_CONIN}
   //{$Define RecordGridFromVector}
   //{$Define Record3DEPX}
   //{$Define RecordDEMIXRefDEM}
   //{$Define Record3DEPXFull}
   //{$Define RecordDEMIX_evaluations_graph}
   //{$Define RecordComputeDEMIX_Diff_Dist}
   //{$Define RecordDEMIXLSgrids}
   //{$Define RecordDEMIXhillshades}
   //{$Define RecordDiluviumFull}
   //{$Define Rec_DEMIX_Landcover}
   //{$Define RecordDEMIXStart}
   //{$Define RecordDEMIXsave}
   //{$Define RecordCreateHalfSec}
   //{$Define RecordHalfSec}
   //{$Define RecordTileStats}
   //{$Define Record3DEPXAlreadyDone}
   //{$Define RecordDEMIX_colors}
   //{$Define RecordTileProcessing}
   //{$Define RecordDEMIXties}   //only enable for small test DB, or tracking crash
   //{$Define Record3DEPXFull}
   //{$Define RecordDEMIXNames}

   //{$Define RecordCriteriaEvaluation}
   //{$Define RecordDEMIXSortGraph}
   //{$Define RecordGridCompare}
   //{$Define RecordUseTile}

   //{$Define RecordDEMIXMovies}
   //{$Define ShowDEMIXWhatsOpen}
{$EndIf}


interface

uses
//needed for inline of core DB functions
   Petmar_db,
   Data.DB,

   {$IfDef UseFireDacSQLlite}
      FireDAC.Stan.ExprFuncs,
      FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
      FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
      FireDAC.Stan.Async, FireDAC.DApt, FireDAC.UI.Intf, FireDAC.Stan.Def,
      FireDAC.Stan.Pool, FireDAC.Phys, FireDAC.Comp.Client, FireDAC.Comp.DataSet,
      FireDAC.Phys.SQLite, FireDAC.Comp.UI,
   {$EndIf}

   {$IfDef UseTDBF}
      dbf,
   {$EndIf}

   {$IfDef UseTCLientDataSet}
      DBClient,
   {$EndIf}
//end core DB functions definitions

    System.SysUtils,System.Classes,System.UITypes,System.Diagnostics,
    StrUtils,dbGrids,
    VCL.ExtCtrls,VCL.Forms, VCL.Graphics, VCL.Controls,VCL.StdCtrls,
    WinAPI.Windows,
    Petmar,Petmar_types,BaseGraf,
    DEMDefs;

const
   JustTileStats : boolean = false;
   MaskForDiluvium = false;

const
   MaxTiles = 3500;
   MaxCriteria = 20;
   MaxAreas = 250;
   MaxClusters = 20;
   MaxDEMs = 10;

const
   DEMIX_vert_datum_code : integer = 0;

const
   DEMIXOpenMap = false;
   DEMIXCloseMap = true;

   DEMisDSM = 1;
   DEMisDTM = 2;

   MeanParams : array[1..3] of shortstring = ('ELVD_MEAN','SLPD_MEAN','RUFD_MEAN');
   StdDevParams : array[1..3] of shortstring = ('ELVD_STD','SLPD_STD','RUFD_STD');
   MedianParams : array[1..3] of shortstring = ('ELVD_MED','SLPD_MED','RUFD_MED');

   DiffParams : array[1..3] of shortstring = ('ELVD','SLPD','RUFD');
   ParamSuffixes : array[1..5] of shortstring = ('_AVD','_STD','_MAE','_RMSE','_LE90');
   ShortParamSuffixes : array[1..5] of shortstring = ('AVD','STD','MAE','RMSE','LE90');
   LongParamSuffixes : array[1..10] of shortstring = ('MIN','MAX','MEAN','AVD','STD','MED','RMSE','MAE','LE90','N');

   NumTileCharacters = 10;
   TileCharacters : array[1..NumTileCharacters] of shortstring = ('AVG_ELEV','AVG_ROUGH','AVG_SLOPE','BARREN_PC','FOREST_PC','RELIEF','URBAN_PC','WATER_PC','MIN_ELEV','MAX_ELEV');

   RefDEMType : array[1..2] of shortstring = ('DSM','DTM');

   NumLandTypes = 8;
   LandTypes : array[1..NumLandTypes] of shortstring = ('ALL','FLAT','GENTLE','STEEP','CLIFF','URBAN','FOREST','BARREN');
   opByCluster = 1;
   opByDEM = 2;

   TileStatsString = 'AREA,DEMIX_TILE,LAT,LONG';
   AreaString = ',DEM,REF_TYPE,LAND_TYPE,LANDTYP_PC';
   SlopeDiffStatsString = ',SLPD_MIN,SLPD_Max,SLPD_Mean,SLPD_AVD,SLPD_STD,SLPD_MED,SLPD_RMSE,SLPD_MAE,SLPD_LE90,SLPD_N';
   ElevDiffStatsString = ',ELVD_MIN,ELVD_Max,ELVD_Mean,ELVD_AVD,ELVD_STD,ELVD_MED,ELVD_RMSE,ELVD_MAE,ELVD_LE90,ELVD_N';
   RufDiffStatsString = ',RUFD_MIN,RUFD_Max,RUFD_Mean,RUFD_AVD,RUFD_STD,RUFD_MED,RUFD_RMSE,RUFD_MAE,RUFD_LE90,RUFD_N';

const
   MaxDEMIXDEM = 15;
   NumDEMIXtestDEM : integer = 0;
   NewFormatDEMIXDB : integer = 0;
   DEMIXtraditional = 1;
   DEMIXneo         = 2;
   DEMIXanalysismode : integer = 1;

const
   fuvmMixed = 1;
   fuvmPartials = 2;
   fuvmCurves = 3;
   fuvmDiffDist = 4;

var
   DEMIXDEMTypeName,                                           //name of folder with the DEMs, in case there are multiple versions
   DEMIXshort     : array[1..MaxDEMIXDEM] of shortstring;      //short name used to identify DEM (comparing of multiple versions not currently implemented)
   DEMIXDEMcolors : array[1..MaxDEMIXDEM] of tPlatformColor;
   CriteriaFamily : shortstring;
   DEMIXModeName : shortstring;
   DEMIXMode : integer;

   type
   tDEMIXindexes = array[1..MaxDEMIXDEM] of integer;
   tDEMIXfloats = array[1..MaxDEMIXDEM] of float32;

const
   Ref1SecPointStr = '_ref_1sec_point';
   Ref1SecAreaStr =  '_ref_1sec_area';
   Ref1_5SecPointStr = '_ref_1.5x1sec_point';



const
   yasBestEval = 0;
   yasSlope = 1;
   yasRuff = 2;
   yasRelief = 3;
   yasBarren = 4;
   yasForest = 5;
   yasLatitude = 6;
   yasBestEvalColoredBySlope = 7;
   MovieByTestDEM : boolean = false;

var
   RefDEMs,TestDEMs,
   UsingRefDEMs,
   RefSlopeMap,RefRuffMap,RefRRI,RefTPI,
   RefHillshade,TestHillshade,TestTPI,
   SlopeMap,TestRuffMap,TestRRI : tDEMIXindexes;

   TestSeries : array[1..MaxDEMIXDEM] of shortstring;
   DEMIX_DB,HalfSecRefDTM,HalfSecRefDSM,HalfSecDTM,HalfSecALOS,HalfSecCOP,
   DEMIXRefDEM,RefDTMpoint,RefDTMarea,RefDSMpoint,RefDSMarea, COPRefDTM, COPRefDSM : integer;

   DEMIX_Base_DB_Path,
   DEMIX_final_DB_dir,
   DEMIX_GIS_dbName,

   DEMIX_criteria_tolerance_fName,
   SSIMresultsDir, FUVresultsDir, PartialsResultsDir,CurvaturesResultsDir,

   GeomorphonsDir,
   DEMIX_diff_dist,

   DEMIX_Ref_1sec,DEMIX_Ref_dsm_1sec,DEMIX_test_dems,
   DEMIX_Under_ref_dtm,DEMIX_Under_test_dems,                //locations for the 1" DEMs used in comparison

   DEMIX_Ref_Half_sec,
   DEMIX_profile_test_dir,

   DEMIX_area_lc100,

   DEMIX_area_dbName,
   DEMIX_Ref_Merge,

   AreaListFName,

   DEMIX_distrib_graph_dir,DEMIX_diff_maps_dir,DEMIX_3DEP_Dir,

   GeodeticFName, IceSatFName, LandCoverFName,
   LocalDatumAddFName,LocalDatumSubFName,
   RefDSMPointFName,RefDSMareaFName,RefDTMPointFName,RefDTMareaFName, COPRefDTMFName,COPRefDSMFName : PathStr;

//create or edit database
   procedure MakeDBForParamStats(Option,DBonTable : integer);
   procedure DEMIX_SSIM_FUV_transpose_kmeans_new_db(DBonTable : integer);
   procedure ComputeDEMIX_Diff_Dist_tile_stats(Overwrite : boolean; AreasWanted : tStringList = nil);
   procedure RankDEMS(DBonTable : integer; TheDEMs : tStringList);
   function AverageScoresOfDEMs(DBonTable : integer; DEMs : tStringList; CriteriaFilter : shortstring; Ext : ExtStr = '_SCR'; Filters : tStringList = nil; Labels : tStringList = Nil) : integer;
   procedure ModeOfDifferenceDistributions;
   procedure AddTileCharacteristicsToDB(DBonTable : integer);

   procedure EvalRangeAndBestEvalForCriterion(DBonTable : integer);
   procedure CreateFinalDiffDistDB;
   procedure MergeCSV(Mode : integer);
   procedure AddPercentPrimaryData(DBonTable : integer);

//clusters function
   procedure TileCharateristicsWhiskerPlotsByCluster(DBonTable : integer; NoFilteringToGetAllTiles : boolean; FilterToUse : tStringList = Nil; {UseStringsInCriterion : boolean = false;} SingleCriterion : shortstring = '');
   procedure DEMIX_COP_clusters_tile_stats(DBonTable : integer);
   procedure DEMIX_clusters_per_tile(DBonTable : integer);

   procedure DEMIX_SSIM_FUV_clusters_diversity_graphs(DBonTable : integer; ColorByDEM : boolean = true);
   function DEMIX_SSIM_FUV_cluster_sensitivity_graph(DBonTable : integer) : tThisBaseGraph;
   function DEMIX_SSIM_FUV_clusters_graph(DBonTable : integer) : tThisBaseGraph;


//inventory and reports
   procedure InventoryDBwithDSMandDTMbyArea;
   procedure InventoryDEMIXdifferenceStats;
   procedure CheckTestDEMs;
   procedure VerifyTestDEMcoverages;
   procedure ComputeDEMIX_Summary_stats_DB(GeoTiles : boolean = true);
   procedure InventoryDEMIX_SSIM_FUV_Stats;
   procedure DeleteFilesForATestArea;
   procedure FindFilesWith42112;
   procedure FixFilesWith42112;
   procedure EvalRangeAndStatsByCriterion(DBonTable : integer; aField : shortstring = '');
   procedure InventoryCriteriaEachDEMIXtile(DB : integer);
   procedure InventoryPercentileByCriterionEachDEMIXtile(DB : integer);
   procedure FindTilesInAreaForCoast;
   procedure InventoryAllDEMIXdata;
   procedure PruneMisnamedReferenceDTMs;
   procedure InventoryAreasAndTilesByCountry(DB : integer);



{$IfDef IncludeDEMIX_external_programs}
   procedure ClearDerivedGrids;
   function ExternalProgramOutPutFile(i : integer; aProgram,Param,AreaName : shortstring; IsPoint : boolean) : PathStr;
   //this should be rolled into a DEMIX_grid object; for now it is global variables

         //the newer code using a set of arrays for the point and area DEMs, which can have corresponding array for derived grids like slope
         //-1 for the high latitude (currently none for area grid),
         //0 for the 1" reference,
         // >=1 for test DEMs
         type
            tDEM_int_array = array [-1..MaxDEMIXDEM] of integer; //-1 for high latitude ref DEM, 0 for ref DEMs, others for the test DEMs
         var
            NumPtDEMs,
            NumAreaDEMs : integer;
            PointDEMs,AreaDEMs,      //used for referenced and test DEMs
            AreaGrids,PointGrids,    //used for derived parameters
            AreaGrids2,PointGrids2,  //used for parameters created at the same time (slope/roughness, openness upward and downward
            PtSSIMGrids, AreaSSIMGrids : tDEM_int_array;
            dmxFirstPoint,dmxFirstArea : integer;

         procedure InitializePointAndAreaGrids(var PointGrids,AreaGrids : tDEM_int_array; InitValue : integer = 0);
         function RefGridForThisPointGrid(WhatGroup : tDEM_int_array; i : integer) : integer;
         procedure ShowDEMIXgrids(WhatFor : shortstring; PointGrids,AreaGrids : tDEM_int_array);

         //grids created by MICRODEM
         function CreateDEMIXDerivedGrids(Which : shortstring; AreaName : shortstring; OpenMaps : boolean = false; SaveMaps : boolean = false) : shortstring;
         procedure CreateDEMIXSlopeRoughnessGrids(AreaName : shortstring; OpenMaps : boolean = false; SaveMaps : boolean = false);
         function CreateDEMIXOpennessGrids(AreaName : shortstring; OpenMaps : boolean = false; SaveMaps : boolean = false) : shortstring;

         //links to grids created by other programs
         function OpenGridsCreatedByExternalProgram(OpenMaps : boolean; aProgram,AreaName,Param : shortString; var PointGrids,AreaGrids : tDEM_int_array) : boolean;
         procedure WBT_CreateDEMIX_GeomorphonGrids(OpenMaps : boolean = false);
         procedure WBT_CreateDEMIX_HANDGrids(OpenMaps : boolean = false);
         procedure WBT_CreateDEMIX_Flow_AccumulationGrids(Log : boolean; OpenMaps : boolean = false);

         procedure LSP_Calc_Grids(kwat : shortstring; OpenMaps : boolean = false);
         function SAGACreateDEMIX_ConIn_Grids(OpenMaps : boolean; AreaName,aParam : shortstring) : boolean;
         procedure CreateLSPgrids(OpenMaps : boolean; Param,AreaName : shortstring; var PointGrids,AreaGrids : tDEM_int_array);
{$EndIf}



procedure OneDegreeTilesToCoverTestAreas;

procedure TrimReferenceDEMsToDEMIXtiles;

function AreDEMIXscoresInDB(db : integer) : boolean;
procedure ComputeAverageScoresForSelectedCriteria(db : integer; DEMs,CriteriaList : tStringList; var Scores : tDEMIXfloats; var NumTies : integer; var WinnerString : shortstring);
procedure ComputeAverageEvaluationsForSelectedCriteria(db : integer; DEMs,CriteriaList : tStringList; var Scores : tDEMIXfloats);

procedure CriteriaInSSIM_FUV_db(db : integer);

function IsDEMIX_signedCriterion(Criterion : shortstring) : boolean;

procedure ClusterCompositionByDBfield(DBonTable : integer);
procedure ClusterFrequencyForSelectedField(DBonTable : integer);
procedure AreasInClusters(DB : integer);

procedure FilterTableForDEMIXevaluation(DBonTable,Value : integer; anOperator : shortString = '<=');

procedure MakeTerrainGridsFromMICRODEM(DataDir : PathStr; DEMIndex : integer; IsPoint : boolean);

function LinkedGraphofCriteriaEvaluations(DBonTable : integer; What : shortstring; ClusterOption : boolean): tThisBaseGraph;

function ID_DEMIX_DB_type(db : integer) : byte;
procedure MakeLandParamFilters(LandParam : shortstring; var GeomorphFilters,Labels : tStringList; BinSize : integer = 0);
procedure ImportLandParamFilters(fName : PathStr; var GeomorphFilters,Labels : tStringList);
procedure ImportLandParamFiltersLong(fName : PathStr; var GeomorphFilters,Labels,Lowers,Uppers : tStringList);

function ContinueExperimentalDEMIX : boolean;
function NoSuffixCriterion(Criterion : shortstring) : shortstring;
function TileCharacteristicsInDB(DB : integer; Warn : boolean = false) : boolean;
procedure TrackCriteriaList(UseLSPs : tStringList; Where : shortstring);

function InsureFUVinLSPname(aName : shortstring) : shortstring;
function CreateSingleLSPGrid(OpenMaps : boolean; DEM : integer; Param : shortstring) : integer;

function GetDSMandDTMTileNamesFromLatLong(DBonTable : integer; Lat,Long : float64; var DTMName,DSMName : PathStr): boolean;
function GetDSMandDTMFileNamesFromLatLong(DBonTable : integer; Lat,Long : float64; var DTMName,DSMName : PathStr): boolean;
function IdentifyDEMIXtileAsDTMorDSM(Tile : shortString) : shortstring;
function GeneralizeReferenceName(Name : shortstring) : shortstring;

   {$IfDef ExternalProgramsSaveCriteria}
      MD_out_ref_dir,MD_out_test_dir,
      wbt_out_ref_dir,wbt_out_test_dir,
      saga_out_ref_dir,saga_out_test_dir : PathStr;
   {$EndIf}

   {$IfDef IncludeVectorCriteria}
      Stream_valley_dir,
      ChannelMissesDir : PathStr;
   {$EndIf}

   {$IfDef IncludeVectorCriteria}
    //vector (channel network, ridges, valleys) comparisons
       procedure ClassificationAgreement(Overwrite : boolean; AreasWanted : tstringlist = nil);
       procedure CompareChannelNetworks(Overwrite : boolean; Area : shortstring);
       procedure ChannelNetworkMissPercentages(Overwrite : boolean; AreasWanted : tstringlist = nil);
       procedure DEMIX_CreateGridsFromVectors(Overwrite : boolean);
   {$EndIf}

     {$IfDef DEMIX_SAGA_channels}
      //directories for channel criterion calculations, currently replaced with Python WbW creation
      DEMIX_test_DEMs_no_sink, DEMIX_ref_DEMs_no_sink,
      DEMIX_test_DEMs_channels, DEMIX_ref_DEMs_channels,
      DEMIX_test_DEMs_channel_grids, DEMIX_ref_DEMs_channel_grids : PathStr;

      procedure CreateChannelNetworkGridsFromVectors(Overwrite : boolean; AreasWanted : tstringlist = nil);
      procedure BatchRemoveSinksInDEMIX_DEMS(Overwrite : boolean; AreasWanted : tstringlist = nil);
      procedure BatchCreateVectorChannelNewtwork(Overwrite : boolean; AreasWanted : tstringlist = nil);
      procedure ChannelNetworkMapComparison(Overwrite : boolean; AreaName,TestDEMName : shortstring);
      procedure MultistepChannelNetworks(Overwrite : boolean);
      procedure InventoryChannelDataByArea;
   {$EndIf}

   {$IfDef ExternalProgramFUV_SSIM} function SAGACreateDEMIX_LS_Grids(AreaName,aParam : shortstring; OpenMaps : boolean = false) : boolean; {$EndIf}

   {$IfDef IncludeCoastalDEMs}
      DEMIX_diluvium_dtms,DEMIX_delta_dtms,DEMIX_coastal_dtms : PathStr;  //used for inventories, when mixing comparison modes
   {$EndIf}

   {$IfDef IncludeCoastalDEMs} procedure CheckLowElevationAreas; {$EndIf}


{$IfDef IncludeOldDEMIX_RefDEM_Create}
    //create reference DEMs
       procedure DEMIX_CreateReferenceDEMsFromSource(Overwrite : boolean; DataDirs : tStringList = Nil);
       procedure DEMIX_GDAL_Ref_DEM_datum_shift(Overwrite : boolean; DataDirs : tStringList = Nil);
       procedure DEMIX_Ref_DEM_full_chain(overwrite : boolean);
       procedure ShiftDEMsto_UTM_WGS84_EGM2008(Overwrite : boolean; DataDirs : tStringList = Nil);
       procedure DEMIX_MergeReferenceDEMs(Overwrite : boolean; DataDirs : tStringList = Nil);
       procedure CreateLandCoverGrids;
       procedure MoveReferenceDSMs;

    //create reference DEMs, non-3DEP
       procedure DEMIX_merge_source(Areas : tStringList = Nil);
       procedure DEMIX_CreateReferenceDEMs(Overwrite : boolean; ResampleMode : byte; Areas : tStringList = Nil);
       procedure DEMIXCreateHalfSecRefDEMs(AreaName : shortstring = '');
       procedure ResampleForDEMIXOneSecDEMs(Overwrite : boolean; CloseAfter : boolean; DEM : integer; OpenMap : boolean = false; OutPath : PathStr = ''; ResampleMode : byte = 1);
    //create test DEMs
       procedure DiluviumDTMforTestAreas(Overwrite : boolean; Areas : tStringList = Nil);
       procedure DeltaDTMforTestAreas(Overwrite : boolean; Areas : tStringList = Nil);
       procedure CoastalDTMforTestAreas(Overwrite : boolean; Areas : tStringList = Nil);
       procedure CreateDEMIXTestAreaDEMs(Overwrite : boolean);
       procedure AllHallucinatingDTMsforCoastalAreas(Overwrite : boolean);

       procedure TilesInEachElevRangeForTestAreas;
       procedure MaskWaterInReferenceAndTestDEMs;
{$EndIf}


implementation

uses
   Nevadia_Main,
   DEMstat,Make_grid,PetImage,PetImage_form,new_petmar_movie,DEMdatabase,PetDButils,Pick_several_dems,
   Geotiff, BaseMap, GDAL_tools, DEMIX_filter, DEMstringgrid,DEM_NLCD,
   DEMCoord,DEMMapf,DEMDef_routines,DEM_Manager,DEM_indexes,PetMath,
   MD_use_tools,
   DEMIX_control,DEMIX_graphs;


{$include demix_create_database.inc}

{$include demix_clusters.inc}

{$include demix_create_lsp_grids.inc}

{$include demix_inventory_check_dems.inc}

{$IfDef IncludeDEMIX_external_programs}
   {$include demix_external_program_derived_grids.inc}
{$EndIf}

{$IfDef IncludeOldDEMIX_RefDEM_Create}
   {$include demix_create_ref_dems.inc}
   {$include demix_create_test_dems.inc}
{$EndIf}

{$IfDef IncludeVectorCriteria}
   {$include demix_channels.inc}
{$EndIf}


function GeneralizeReferenceName(Name : shortstring) : shortstring;
begin
   Result := StringReplace(Name, '_cop', '',[rfIgnoreCase]);
   Result := StringReplace(Result, '_srtm', '',[rfIgnoreCase]);
   Result := StringReplace(Result, '_alos', '',[rfIgnoreCase]);
end;


function IdentifyDEMIXtileAsDTMorDSM(Tile : shortString) : shortstring;
begin
   Result := '';
   Tile := UpperCase(Tile);
   if (Copy(Tile,1,3) = 'ES_') or (Copy(Tile,1,3) = 'IC_') then begin
        if ANSIcontainsStr(Tile,'MDS') then Result := 'DSM'
        else if ANSIcontainsStr(Tile,'MDT') then Result := 'DTM';
   end;
   if (Copy(Tile,1,3) = 'CA_') then begin
        if ANSIcontainsStr(Tile,'DSM') then Result := 'DSM'
        else if ANSIcontainsStr(Tile,'DTM') then Result := 'DTM';
   end;
end;




function GetDSMandDTMTileNamesFromLatLong(DBonTable : integer; Lat,Long : float64; var DTMName,DSMName : PathStr): boolean;
var
   TStr : shortstring;
begin
  if GISdb[DBonTable].MyData.FieldExists('CRITERION') then TStr := 'CRITERION=' + QuotedStr('SLOPE') + ' AND ' else TStr := '';
  GISdb[DBonTable].ApplyGISFilter(TStr + PointVeryCloseGeoFilter('LAT','LONG',Lat,Long,0.01));
  if (GISdb[DBonTable].MyData.FiltRecsInDB = 2) then begin
     TStr := GISdb[DBonTable].MyData.GetFieldByNameAsString('DEMIX_TILE');
     if IdentifyDEMIXtileAsDTMorDSM(TStr) = 'DSM' then DSMName := TStr else DTMName := TStr;
     GISdb[DBonTable].MyData.Next;
     TStr := GISdb[DBonTable].MyData.GetFieldByNameAsString('DEMIX_TILE');
     if IdentifyDEMIXtileAsDTMorDSM(TStr) = 'DSM' then DSMName := TStr else DTMName := TStr;
     Result := true;
  end
  else Result := false;
end;


function GetDSMandDTMFileNamesFromLatLong(DBonTable : integer; Lat,Long : float64; var DTMName,DSMName : PathStr): boolean;

   function TileName : ShortString;
   begin
     TileName := GISdb[DBonTable].MyData.GetFieldByNameAsString('DEMIX_TILE');
   end;

begin
  GISdb[DBonTable].ApplyGISFilter('CRITERION=' + QuotedStr('SLOPE') + ' AND ' + PointVeryCloseGeoFilter('LAT','LONG',Lat,Long,0.01));
  if (GISdb[DBonTable].MyData.FiltRecsInDB = 2) then begin
     DSMName := ExtractFilePath(GISdb[DBonTable].DBFullName) + GISdb[DBonTable].MyData.GetFieldByNameAsString('AREA') + '\source\' + TileName + '.tif';
     GISdb[DBonTable].MyData.Next;
     DTMName := ExtractFilePath(GISdb[DBonTable].DBFullName) + GISdb[DBonTable].MyData.GetFieldByNameAsString('AREA') + '\source\' + TileName + '.tif';
     Result := FileExists(DSMName) and FileExists(DTMName);
  end
  else Result := false;
end;




function InsureFUVinLSPname(aName : shortstring) : shortstring;
begin
    if StrUtils.AnsiContainsText(aName,'_FUV') then Result := aName
    else Result := aName + '_FUV';
end;

function NoSuffixCriterion(Criterion : shortstring) : shortstring;
begin
   Result := StringReplace(Criterion,'_FUV','',[rfIgnoreCase]);
end;


function TileCharacteristicsInDB(DB : integer; Warn : boolean = false) : boolean;
begin
   Result := GISdb[DB].MyData.FieldExists('BARREN_PC') and GISdb[DB].MyData.FieldExists('AVG_SLOPE');
   if Warn and (not Result) then MessageToContinue('Tile characteristics missing in ' + GISdb[db].dbName);
end;


procedure TrackCriteriaList(UseLSPs : tStringList; Where : shortstring);
begin
     WriteLineToDebugFile(Where);
     WriteStringListToDebugFile(UseLSPs,true);
end;


function ContinueExperimentalDEMIX : boolean;
begin
   Result := AnswerIsYes('This option is experimental and not tested recently; Continue');
end;


procedure ImportLandParamFilters(fName : PathStr; var GeomorphFilters,Labels : tStringList);
var
   Table : tMyData;
begin
   GeomorphFilters := tStringList.Create;
   Labels := tStringList.Create;
   Table := tMyData.Create(fName);
   while not Table.Eof do begin
      GeomorphFilters.Add(Table.GetFieldByNameAsString('FILTER'));
      Labels.Add(Table.GetFieldByNameAsString('LABEL'));
      Table.Next;
   end;
   Table.Destroy;
end;


procedure ImportLandParamFiltersLong(fName : PathStr; var GeomorphFilters,Labels,Lowers,Uppers : tStringList);
//includes upper and lower cut points when they will be used
var
   Table : tMyData;
begin
   GeomorphFilters := tStringList.Create;
   Labels := tStringList.Create;
   Lowers := tStringList.Create;
   Uppers := tStringList.Create;
   Table := tMyData.Create(fName);
   while not Table.Eof do begin
      GeomorphFilters.Add(Table.GetFieldByNameAsString('FILTER'));
      Labels.Add(Table.GetFieldByNameAsString('LABEL'));
      Lowers.Add(Table.GetFieldByNameAsString('LOWER'));
      Uppers.Add(Table.GetFieldByNameAsString('UPPER'));
      Table.Next;
   end;
   Table.Destroy;
end;


procedure MakeLandParamFilters(LandParam : shortstring; var GeomorphFilters,Labels : tStringList; BinSize : integer = 0);
var
   i,LowBin,HighBinBase,Value : integer;
begin
   Labels := tStringList.Create;
   GeomorphFilters := tStringList.Create;
   if LandParam = 'AVG_SLOPE' then begin
      LowBin := 5;
      if BinSize = 0 then BinSize := 5;
      HighBinBase := 70;
   end
   else if LandParam = 'AVG_ROUGH' then begin
      LowBin := 2;
      if BinSize = 0 then BinSize := 2;
      HighBinBase := 18;
   end
   else if LandParam = 'URBAN_PC' then begin
      LowBin := 5;
      if BinSize = 0 then BinSize := 5;
      HighBinBase := 40;
   end
   else begin
      LowBin := 10;
      if BinSize = 0 then BinSize := 10;
      HighBinBase := 90;
   end;
  Labels.Add('All tiles');
  Labels.Add(LandParam + '<' + IntToStr(LowBin) + '%');
  GeomorphFilters.Add('');
  GeomorphFilters.Add(LandParam + '<' + IntToStr(LowBin));
  Value := LowBin;
  while Value <= HighBinBase do begin
     GeomorphFilters.Add(LandParam + '>=' + IntToStr(Value) + ' AND ' + LandParam + '<' + IntToStr(Value + BinSize));
     Labels.Add(LandParam + ' ' + IntToStr(Value) + '-' + IntToStr(Value + BinSize)+ '%');
     Value := Value + BinSize;
  end;
  if (Value < 100) then begin
     GeomorphFilters.Add(LandParam + '>' + IntToStr(Value));
     Labels.Add(LandParam + '>' + IntToStr(Value) + '%');
  end;
  {$IfDef RecordDEMIXFilters} WriteLineToDebugFile('Labels'); WriteStringListToDebugFile(Labels); WriteLineToDebugFile('GeomorphFilters'); WriteStringListToDebugFile(GeomorphFilters);  {$EndIf}
end;


function ID_DEMIX_DB_type(db : integer) : byte;
var
   Criteria : tStringList;

   function FindPartialMatch(Key : shortString) : boolean;
   var
     Crit : shortstring;
     i : integer;
   begin
      Result := false;
      for i := 0 to pred(Criteria.Count) do begin
         Crit := UpperCase(Criteria.Strings[i]);
         if StrUtils.AnsiContainsText(Crit,Key) then begin
            Result := true;
            exit;
         end;
      end;

   end;

begin
   Result := ddbUndefined;
   if GISdb[db].MyData.FieldExists('CRITERION') then begin
     Criteria := GISdb[db].MyData.ListUniqueEntriesInDB('CRITERION');
     if FindPartialMatch('LE90') and FindPartialMatch('MAE') then Result := ddbDiffDist
     else if FindPartialMatch('FUV') then Result := ddbFUV
     else if FindPartialMatch('SSIM') then Result := ddbSSIM;
     Criteria.Destroy;
   end;
end;



procedure FilterTableForDEMIXevaluation(DBonTable,Value : integer; anOperator : shortString = '<=');
var
   i : integer;
   aFilter : shortstring;
   theFields : tStringList;

       procedure CheckFieldForFilter(fName : shortstring);
       begin
          if GISdb[DBonTable].MyData.FieldExists(fName) then begin
             if (length(aFilter) > 0) then aFilter := aFilter + ' OR ';
             aFilter := aFilter + fname + anOperator + IntToStr(Value);
          end;
       end;

begin
   aFilter := '';
   for i := 1 to MaxDEMIXDEM do begin
      CheckFieldForFilter(DEMIXshort[i]);
   end;

   if (length(aFilter) = 0) or StrUtils.AnsiContainsText(UpperCase(GISdb[DBonTable].dbName),'TRANSPOSE') then begin
      theFields := GISdb[DBonTable].MyData.FieldsInDataBase;
      for I := 0 to pred(theFields.Count) do begin
         if StrUtils.AnsiContainsText(theFields.Strings[i],'FUV') or StrUtils.AnsiContainsText(theFields.Strings[i],'SSIM') then begin
            CheckFieldForFilter(theFields.Strings[i]);
         end;
      end;
   end;
   if (length(aFilter) > 0) then GISdb[DBonTable].ApplyGISFilter(aFilter)
   else MessageToContinue('No DEMIX evaluation fields in DB');
end;


function IsDEMIX_signedCriterion(Criterion : shortstring) : boolean;
begin
   Result := StrUtils.AnsiContainsText(Criterion,'_MEAN') or StrUtils.AnsiContainsText(Criterion,'_MED');
end;


function AreDEMIXscoresInDB(db : integer) : boolean;
var
   i : integer;
begin
   Result := true;
   for I := 1 to NumDEMIXtestDEM do begin
      if not GISdb[db].MyData.FieldExists(DEMIXShort[i] + '_SCR') then begin
         Result := false;
         exit;
      end;
   end;
end;


procedure ComputeAverageScoresForSelectedCriteria(db : integer; DEMs,CriteriaList : tStringList; var Scores : tDEMIXfloats; var NumTies : integer; var WinnerString : shortstring);
var
   i,Opinions : integer;
   Criterion : shortstring;
   LowScore : float32;
begin
   if (not AreDEMIXscoresInDB(DB)) then begin
      RankDEMS(db,nil);
   end;

   GISdb[DB].EmpSource.Enabled := false;
   for i := 1 to NumDEMIXtestDEM do Scores[i] := 0;
   Opinions := 0;
   while not GISdb[DB].MyData.eof do begin
      Criterion := GISdb[DB].MyData.GetFieldByNameAsString('CRITERION');
      if (CriteriaList.IndexOf(Criterion) <> -1) then begin
         inc(Opinions);
         for i := 1 to DEMs.Count do begin
            Scores[i] := Scores[i] + GISdb[DB].MyData.GetFieldByNameAsFloat(DEMs[pred(i)] + '_SCR');
         end;
      end;
      GISdb[DB].MyData.Next;
   end;
   for I := 1 to NumDEMIXtestDEM do Scores[i] := Scores[i] / Opinions;
   LowScore := 999;
   WinnerString := '';
   for I := 1 to DEMs.Count do begin
      if Scores[i] < LowScore - 0.001 then begin
         LowScore := Scores[i];
         WinnerString := DEMs[i];
         NumTies := 1;
      end
      else if Scores[i] < LowScore + 0.001 then begin
         WinnerString := WinnerString + ';' + DEMs[i];
         inc(NumTies);
      end;
   end;
end;


procedure ComputeAverageEvaluationsForSelectedCriteria(db : integer; DEMs,CriteriaList : tStringList; var Scores : tDEMIXfloats);
var
   Criterion : shortstring;
   i,Opinions : integer;
begin
   GISdb[DB].EmpSource.Enabled := false;
   for i := 1 to NumDEMIXtestDEM do Scores[i] := 0;
   Opinions := 0;
   while not GISdb[DB].MyData.eof do begin
      Criterion := GISdb[DB].MyData.GetFieldByNameAsString('CRITERION');
      if (CriteriaList.IndexOf(Criterion) <> -1) then begin
         inc(Opinions);
         for i := 1 to DEMs.Count do begin
            Scores[i] := Scores[i] + GISdb[DB].MyData.GetFieldByNameAsFloat(DEMs[pred(i)]);
         end;
      end;
      GISdb[DB].MyData.Next;
   end;
   for i := 1 to DEMs.Count do Scores[i] := Scores[i] / Opinions;
end;



initialization
finalization

end.


