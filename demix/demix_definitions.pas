unit demix_definitions;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}


// 10/1/2023: DEMIX options under active development.
//Some options are hard coded and not error-trapped, and some options may not longer work.  Used with caution

{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   {$Define RecordDEMIX}
   {$Define RecordDEMIXLoad}
   {$Define RecordDiluvium}
   {$Define Record3DEPX}
   {$Define RecordDEMIX_evaluations_graph}
   {$Define RecordDiluviumFull}
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
   //{$Define RecordDEMIXVDatum}
   //{$Define RecordFullDEMIX}
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

    System.SysUtils,System.Classes,System.UITypes,
    StrUtils,dbGrids,
    VCL.ExtCtrls,VCL.Forms, VCL.Graphics, VCL.Controls,
    WinAPI.Windows,
    Petmar,Petmar_types,BaseGraf,
    DEMDefs;

const
   DEMIXSkipFilesAlreadyDone = true;
   FilterOutSeaLevel = false;
   JustTileStats : boolean = false;
   MaskForDiluvium = false;

const
   Ref1SecPointStr = '_ref_1sec_point';
   Ref1SecAreaStr =  '_ref_1sec_area';

const
   DEMIXOpenMap = false;
   DEMIXCloseMap = true;

   DEMisDTM = 2;
   DEMisDSM = 1;

   MeanParams : array[1..3] of shortstring = ('ELVD_MEAN','SLPD_MEAN','RUFD_MEAN');
   StdDevParams : array[1..3] of shortstring = ('ELVD_STD','SLPD_STD','RUFD_STD');
   MedianParams : array[1..3] of shortstring = ('ELVD_MED','SLPD_MED','RUFD_MED');

   DiffParams : array[1..3] of shortstring = ('ELVD','SLPD','RUFD');
   ParamSuffixes : array[1..5] of shortstring = ('_AVD','_STD','_RMSE','_MAE','_LE90');

   NumTileCharacters = 8;
   TileCharacters : array[1..NumTileCharacters] of shortstring = ('AVG_ELEV','AVG_ROUGH','AVG_SLOPE','BARREN_PC','FOREST_PC','RELIEF','URBAN_PC','WATER_PC');

   NCrits = 12;
   Crits : array[1..NCrits] of shortstring = ('ELEV_SSIM','RRI_SSIM','SLOPE_SSIM','HILL_SSIM','RUFF_SSIM','TPI_SSIM','ELEV_R2','SLOPE_R2','HILL_R2','RUFF_R2','TPI_R2','RRI_R2');

   RefDEMType : array[1..2] of shortstring = ('DSM','DTM');

   NumLandTypes = 8;
   LandTypes : array[1..NumLandTypes] of shortstring = ('ALL','FLAT','GENTLE','STEEP','CLIFF','URBAN','FOREST','BARREN');

   MaxDEMIXDEM = 10;

   RequiredTestDEMs : integer = 6;
   NumDEMIXDEM : integer = 0;

  opByCluster = 1;
  opByDEM = 2;

type
   tDEMIXindexes = array[1..MaxDEMIXDEM] of integer;
   tDEMIXfloats = array[1..MaxDEMIXDEM] of float32;


var
   DEMIXDEMTypeName : array[1..MaxDEMIXDEM] of shortstring;
   DEMIXshort : array[1..MaxDEMIXDEM] of shortstring;
   DEMIXDEMcolors : array[1..MaxDEMIXDEM] of tPlatformColor;

const
   NumPt = 6;
   NumArea = 1;
   PointNames : array[0..NumPt] of shortstring = ('REF_POINT','ASTER','COP','FABDEM','NASA','SRTM','TANDEM');
   AreaNames : array[0..NumArea] of shortstring = ('REF_AREA','ALOS');
type
   tDEM_int_array = array [0..NumPt] of integer;
var
   PointDEMs : tDEM_int_array;   //0 is the reference, rest the test DEMs
   AreaDEMs : tDEM_int_array;  //0 is the reference, rest the test DEMs

var
   RefDEMs,TestDEMs,
   UsingRefDEMs,
   RefSlopeMap,RefRuffMap,RefRRI,RefTPI,
   RefHillshade,TestHillshade,TestTPI,
   SlopeMap,TestRuffMap,TestRRI : tDEMIXindexes;

   TestSeries : array[1..MaxDEMIXDEM] of shortstring;
   DEMIX_DB,
   HalfSecRefDTM,HalfSecRefDSM,HalfSecDTM,HalfSecALOS,HalfSecCOP,
   DEMIXRefDEM,RefDTMpoint,RefDTMarea,RefDSMpoint,RefDSMarea, COPRefDTM, COPRefDSM : integer;

   DEMIX_Ref_1sec,DEMIX_test_dems,DEMIX_Ref_Half_sec,
   DEMIX_Base_DB_Path,DEMIX_profile_test_dir,

  //directories for the channel criterion calculations
   DEMIX_test_DEMs_no_sink, DEMIX_ref_DEMs_no_sink,
   DEMIX_test_DEMs_channels, DEMIX_ref_DEMs_channels,
   DEMIX_test_DEMs_channel_grids, DEMIX_ref_DEMs_channel_grids,
   ChannelMissesDir,DEMIX_diff_dist,DEMIX_area_lc100,

   DEMIX_Ref_Source,DEMIX_Ref_Merge,
   DEMIX_GIS_dbName,
   SSIMresultsDir,
   DEMIX_diluvium_dems,
   DEMIX_distrib_graph_dir,DEMIX_diff_maps_dir,DEMIX_3DEP_Dir,

   GeodeticFName, IceSatFName, LandCoverFName,
   LocalDatumAddFName,LocalDatumSubFName,
   RefDSMPointFName,RefDSMareaFName,RefDTMPointFName,RefDTMareaFName, COPRefDTMFName,COPRefDSMFName : PathStr;

   {$IfDef DEMIX_DB1}
      DEMIX_DB_v1 : integer;
      DEMIX_GIS_dbName_v1 : PathStr;
   {$EndIf}


//create or edit database
   procedure MakeDBForParamStats(Option,DBonTable : integer);
   procedure DEMIX_SSIM_R2_transpose_kmeans_new_db(DBonTable : integer);
   procedure ComputeDEMIX_tile_stats(AreaName : shortstring = '');
   procedure CreateDEMIX_GIS_database(AreaName : shortstring = '');
   procedure RankDEMS(DBonTable : integer);
   procedure SumsOfRankDEMS(DBonTable : integer);
   procedure ModeOfDifferenceDistributions;
   procedure ComputeDEMIX_Summary_stats(AreaName : shortstring = '');
   procedure AddTileCharacteristics(DBonTable : integer);
   procedure SwitchSSIMorR2Scoring(DBonTable : integer);
   procedure EvaluationRangeForCriterion(DBonTable : integer);


//clusters function
   procedure TileCharateristicsWhiskerPlotsByCluster(DBonTable : integer);
   procedure DEMIX_COP_clusters_tile_stats(DBonTable : integer);
   procedure DEMIX_clusters_per_tile(DBonTable : integer);

   procedure DEMIX_SSIM_R2_clusters_diversity_graphs(DBonTable : integer; ColorByDEM : boolean = true);
   function DEMIX_SSIM_R2_cluster_sensitivity_graph(DBonTable : integer) : tThisBaseGraph;
   function DEMIX_SSIM_R2_clusters_graph(DBonTable : integer) : tThisBaseGraph;


//create reference DEMs, 3DEP
   procedure DEMIX_Merge3DEPReferenceDEMs(Overwrite : boolean; DataDirs : tStringList = Nil);
   procedure DEMIX_Create3DEPReferenceDEMs(Overwrite : boolean; DataDirs : tStringList = Nil);
   procedure DEMIX_GDAL_3DEP_datum_shift(Overwrite : boolean; DataDirs : tStringList = Nil);
   procedure DEMIX_3DEP_full_chain(overwrite : boolean);

//create reference DEMs, non-3DEP
   procedure DEMIX_merge_Visioterra_source(AreaName : shortstring = '');
   procedure DEMIX_merge_source(Areas : tStringList = Nil);
   procedure DEMIX_CreateReferenceDEMs(Overwrite : boolean; ResampleMode : byte; Areas : tStringList = Nil);
   procedure DEMIXCreateHalfSecRefDEMs(AreaName : shortstring = '');
   procedure ResampleForDEMIXOneSecDEMs(CloseAfter : boolean; DEM : integer; OpenMap : boolean = false; OutPath : PathStr = ''; ResampleMode : byte = 1);


//create test DEMs
   function CreateDEMIXTestDEMsForArea(Overwrite : boolean; AreaName : ShortString; AreaRefDEM,PointRefDEM : integer) : boolean;
   procedure DiluviumDEMforTestAreas(Overwrite : boolean = true);
   procedure CreateTestAreaDEMs(Overwrite : boolean);

//inventory and reports
   procedure InventoryListRefereneDTMbyArea;
   procedure DEMIXTileSummary(DBonTable : integer);
   procedure DEMIXtile_inventory(DBonTable : integer);
   procedure InventoryDBwithDSMandDTMbyArea;
   procedure InventoryDEMIXdifferenceStats;
   procedure CheckTestDEMs;
   procedure Inventory3DEPtiles;
   procedure CheckReferenceDEMsAreEGMandPixelIs;
   procedure InventoryChannelDataByArea;
   procedure CheckDiluviumAreas;
   procedure VerifyAllMapsReadyForSSIM;
   procedure VerifyTestDEMcoverages;


procedure MaskWaterInReferenceDEMs;
procedure TrimReferenceDEMsToDEMIXtiles;



implementation

uses
   Nevadia_Main,
   DEMstat,Make_grid,PetImage,PetImage_form,new_petmar_movie,DEMdatabase,PetDButils,Pick_several_dems,
   Geotiff, BaseMap, GDAL_tools, DEMIX_filter, DEMstringgrid,DEM_NLCD,
   DEMCoord,DEMMapf,DEMDef_routines,DEM_Manager,DEM_indexes,PetMath,
   DEMIX_control;


{$include demix_create_database.inc}

{$include demix_clusters.inc}

{$include demix_create_ref_dems.inc}

{$include demix_create_test_dems.inc}

{$include demix_inventory_check_dems.inc}


initialization
finalization

end.


