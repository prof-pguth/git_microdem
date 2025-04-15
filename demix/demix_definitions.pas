unit demix_definitions;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{________________________________________}


// DEMIX options under active development.
//Some options are hard coded and not error-trapped, and some options may not longer work.  Used with caution

{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   {$Define RecordDEMIX}
   //{$Define RecordNeoDEMIX}
   //{$Define RecordOpenExternalProgramGrids}
   //{$Define RecordDEMIXLoad}
   //{$Define RecordTestDEMstart}
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

    System.SysUtils,System.Classes,System.UITypes,System.Diagnostics,
    StrUtils,dbGrids,
    VCL.ExtCtrls,VCL.Forms, VCL.Graphics, VCL.Controls,
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

var
   DEMIXDEMTypeName,                                           //name of folder with the DEMs, in case there are multiple versions
   DEMIXshort     : array[1..MaxDEMIXDEM] of shortstring;      //short name used to identify DEM (comparing of multiple versions not currently implemented)
   DEMIXDEMcolors : array[1..MaxDEMIXDEM] of tPlatformColor;
   NotRetiredDEMs : array[1..MaxDEMIXDEM] of boolean;
   CriteriaFamily : shortstring;
   DEMIXModeName : shortstring;

   type
   tDEMIXindexes = array[1..MaxDEMIXDEM] of integer;
   tDEMIXfloats = array[1..MaxDEMIXDEM] of float32;

const
   Ref1SecPointStr = '_ref_1sec_point';
   Ref1SecAreaStr =  '_ref_1sec_area';
   Ref1_5SecPointStr = '_ref_1.5x1sec_point';

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
         {$IfDef ExternalProgramFUV_SSIM}
           procedure WBT_CreateDEMIX_HANDGrids(OpenMaps : boolean = false);
           procedure WBT_CreateDEMIX_Flow_AccumulationGrids(Log : boolean; OpenMaps : boolean = false);
           function SAGACreateDEMIX_ConIn_Grids(OpenMaps : boolean; AreaName,aParam : shortstring) : boolean;
           function SAGACreateDEMIX_LS_Grids(AreaName,aParam : shortstring; OpenMaps : boolean = false) : boolean;
         {$EndIf}




const
   yasName = 0;
   yasSlope = 1;
   yasRuff = 2;
   yasRelief = 3;
   yasBestEval = 4;
   yasBarren = 5;
   yasForest = 6;
   yasBestEvalColoredBySlope = 8;
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


   DEMIX_final_DB_dir,

   SSIMresultsDir, FUVresultsDir, //set according to the mode

   MD_out_ref_dir,MD_out_test_dir,
   wbt_out_ref_dir,wbt_out_test_dir,
   saga_out_ref_dir,saga_out_test_dir,
   Stream_valley_dir,

   DEMIX_Ref_1sec,DEMIX_Ref_dsm_1sec,DEMIX_test_dems,
   DEMIX_Under_ref_dtm,DEMIX_Under_test_dems,                //locations for the 1" DEMs used in comparison
   DEMIX_diluvium_dtms,DEMIX_delta_dtms,DEMIX_coastal_dtms,  //used for inventories, when mixing comparison modes

   DEMIX_Ref_Half_sec,
   DEMIX_Base_DB_Path,
   DEMIX_profile_test_dir,

   GeomorphonsDir,
   ChannelMissesDir,DEMIX_diff_dist,

   DEMIX_area_lc100,

   DEMIX_area_dbName,
   DEMIX_criteria_dbName,
   DEMIX_Ref_Merge,
   DEMIX_GIS_dbName,

   AreaListFName,

   DEMIX_distrib_graph_dir,DEMIX_diff_maps_dir,DEMIX_3DEP_Dir,

   GeodeticFName, IceSatFName, LandCoverFName,
   LocalDatumAddFName,LocalDatumSubFName,
   RefDSMPointFName,RefDSMareaFName,RefDTMPointFName,RefDTMareaFName, COPRefDTMFName,COPRefDSMFName : PathStr;

  {$IfDef DEMIX_SAGA_channels}
      //directories for channel criterion calculations, currently replaced with Python WbW creation
      DEMIX_test_DEMs_no_sink, DEMIX_ref_DEMs_no_sink,
      DEMIX_test_DEMs_channels, DEMIX_ref_DEMs_channels,
      DEMIX_test_DEMs_channel_grids, DEMIX_ref_DEMs_channel_grids : PathStrl
   {$EndIf}


//create or edit database
   procedure MakeDBForParamStats(Option,DBonTable : integer);
   procedure DEMIX_SSIM_FUV_transpose_kmeans_new_db(DBonTable : integer);
   procedure ComputeDEMIX_Diff_Dist_tile_stats(Overwrite : boolean; AreasWanted : tStringList = nil);
   //procedure CreateDEMIX_diff_dist_DB_by_transposing(Overwrite : boolean);
   procedure RankDEMS(DBonTable : integer);
   function AverageScoresOfDEMs(DBonTable : integer; DEMs : tStringList; CriteriaFilter : shortstring; Ext : ExtStr = '_SCR'; Filters : tStringList = nil; Labels : tStringList = Nil) : integer;
   procedure ModeOfDifferenceDistributions;
   procedure AddTileCharacteristicsToDB(DBonTable : integer);
   procedure AddFieldsToDEMIXDB(DBonTable : integer; theFields : tStringList);
   procedure MakeWinsDB(DBonTable : integer; aField : shortstring);

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

//inventory and reports
   procedure DEMIXTileSummary(DBonTable : integer);
   procedure DEMIXtile_inventory(DBonTable : integer);
   procedure InventoryDBwithDSMandDTMbyArea;
   procedure InventoryDEMIXdifferenceStats;
   procedure CheckTestDEMs;
   procedure Inventory3DEPtiles;
   procedure CheckReferenceDEMsAreEGMandPixelIs;
   procedure CheckLowElevationAreas;
   procedure VerifyTestDEMcoverages;
   procedure ComputeDEMIX_Summary_stats_DB;
   procedure InventoryDEMIX_SSIM_FUV_Stats;
   procedure DeleteFilesForATestArea;
   procedure FindFilesWith42112;
   procedure FixFilesWith42112;
   procedure EvalRangeAndStatsByCriterion(DBonTable : integer; aField : shortstring = '');
   procedure GetRangesForSSIM;
   procedure InventoryCriteriaEachDEMIXtile(DB : integer);
   procedure InventoryPercentileByCriterionEachDEMIXtile(DB : integer);
   procedure FindTilesInAreaForCoast;
   procedure InventoryAllDEMIXdata;
   procedure InventoryWbWSaagaMDsavedGridsByArea;
   procedure PruneMisnamedReferenceDTMs;
   procedure TilesInEachElevRangeForTestAreas;
   procedure AddColorsForUnderDBs(DBonTable : integer);
   procedure InventoryAreasAndTilesByCountry(DB : integer);
   {$IfDef DEMIX_SAGA_channels} procedure InventoryChannelDataByArea; {$EndIf}


procedure ClassificationAgreement(Overwrite : boolean; AreasWanted : tstringlist = nil);
function ExtractDEMIXDEMName(var fName : PathStr) : shortstring;


//vector (channel network, ridges, valleys) comparisons
   procedure CompareChannelNetworks(Overwrite : boolean; Area : shortstring);
   procedure ChannelNetworkMissPercentages(Overwrite : boolean; AreasWanted : tstringlist = nil);
   procedure DEMIX_CreateGridsFromVectors(Overwrite : boolean);

   {$IfDef DEMIX_SAGA_channels}
      procedure CreateChannelNetworkGridsFromVectors(Overwrite : boolean; AreasWanted : tstringlist = nil);
      procedure BatchRemoveSinksInDEMIX_DEMS(Overwrite : boolean; AreasWanted : tstringlist = nil);
      procedure BatchCreateVectorChannelNewtwork(Overwrite : boolean; AreasWanted : tstringlist = nil);
      procedure ChannelNetworkMapComparison(Overwrite : boolean; AreaName,TestDEMName : shortstring);
      procedure MultistepChannelNetworks(Overwrite : boolean);
   {$EndIf}

procedure ClearDoubleProcessed;

procedure OneDegreeTilesToCoverTestAreas;

procedure MaskWaterInReferenceAndTestDEMs;
procedure TrimReferenceDEMsToDEMIXtiles;

function AreDEMIXscoresInDB(db : integer) : boolean;
procedure ComputeAverageScoresForSelectedCriteria(db : integer; DEMs,CriteriaList : tStringList; var Scores : tDEMIXfloats; var NumTies : integer; var WinnerString : shortstring);
procedure ComputeAverageEvaluationsForSelectedCriteria(db : integer; DEMs,CriteriaList : tStringList; var Scores : tDEMIXfloats);


procedure CreateCopHeadToHeaddb(db : integer);
procedure CriteriaInSSIM_FUV_db(db : integer);

function IsDEMIX_signedCriterion(Criterion : shortstring) : boolean;

procedure ClusterCompositionByDBfield(DBonTable : integer);
procedure ClusterFrequencyForSelectedField(DBonTable : integer);
procedure AreasInClusters(DB : integer);

procedure FilterTableForDEMIXevaluation(DBonTable,Value : integer; anOperator : shortString = '<=');

procedure ClearDerivedGrids;

function ExternalProgramOutPutFile(i : integer; aProgram,Param,AreaName : shortstring; IsPoint : boolean) : PathStr;
procedure MakeTerrainGridsFromMICRODEM(DataDir : PathStr; DEMIndex : integer; IsPoint : boolean);

function LinkedGraphofCriteriaEvaluations(DBonTable : integer; What : shortstring; ClusterOption : boolean): tThisBaseGraph;

procedure CriteriaRanges(AreaName : shortstring);
procedure ComputeCriteriaRanges;

function ID_DEMIX_DB_type(db : integer) : byte;

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

{$include demix_create_ref_dems.inc}

{$include demix_create_test_dems.inc}

{$include demix_inventory_check_dems.inc}

{$include demix_channels.inc}

{$include demix_external_program_derived_grids.inc}


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


      function ExtractDEMIXDEMName(var fName : PathStr) : shortstring;
      begin
         fName := UpperCase(fName);
         if (StrUtils.AnsiContainsText(fName,'TIE')) then Result :=  'TIE'
         else if (StrUtils.AnsiContainsText(fName,'ALOS')) then Result :=  'ALOS'
         else if (StrUtils.AnsiContainsText(fName,'COP')) then Result :=  'COP'
         else if (StrUtils.AnsiContainsText(fName,'ASTER')) then Result :=  'ASTER'
         else if (StrUtils.AnsiContainsText(fName,'FABDEM')) then Result :=  'FABDEM'
         else if (StrUtils.AnsiContainsText(fName,'FATHOM')) then Result :=  'FATHOM'
         else if (StrUtils.AnsiContainsText(fName,'NASA')) then Result :=  'NASA'
         else if (StrUtils.AnsiContainsText(fName,'SRTM')) then Result :=  'SRTM'
         else if (StrUtils.AnsiContainsText(fName,'GEDTM')) then Result :=  'GEDTM'   //must be first, or it will be seen at EDTM
         else if (StrUtils.AnsiContainsText(fName,'EDTM')) then Result :=  'EDTM'
         else if (StrUtils.AnsiContainsText(fName,'DILUV')) then Result :=  'DILUV'
         else if (StrUtils.AnsiContainsText(fName,'TANDEM')) then Result :=  'TANDEM'
         else if (StrUtils.AnsiContainsText(fName,'DELTA')) then Result :=  'DELTA'
         else if (StrUtils.AnsiContainsText(fName,'NEO_DTM')) then Result :=  'NEO_DTM'
         else if (StrUtils.AnsiContainsText(fName,'NEO_DSM')) then Result :=  'NEO_DSM';
      end;


procedure ComputeCriteriaRanges;
var
   Areas : tStringList;
   i : integer;
   TStr : shortstring;
begin
   GetDEMIXpaths;
   DEMIX_initialized := true;
   LockStatusBar := true;
   wmdem.SetPanelText(0, 'Started ' + TimeToStr(Now),true);
   //MDDef.DEMIX_mode := Mode;
   //SetParamsForDEMIXmode;
   Areas := DEMIX_AreasWanted(not MDDef.DEMIX_all_areas);
   for I := pred(Areas.Count) downto 0 do begin
      TStr := IntToStr(succ(i)) + '/' + IntToStr(Areas.Count) + '  ' + Areas.Strings[i];
      wmdem.SetPanelText(2,TStr,true);
      WriteLineToDebugFile(TStr);
      CriteriaRanges(Areas.Strings[i]);
   end;
   Areas.Destroy;
   EndDEMIXProcessing;
end;

procedure CriteriaRanges(AreaName : shortstring);
var
   //DEMIXtileDB,NumRef,
   i,j : integer;
   //ResultsSSIM,ResultsFUV : tStringList;
   //fName,
   //WetnessName : PathStr;


   procedure DoCriterion(Criterion : ANSIString; usingPointGrids,usingAreaGrids : tDEM_int_array; ClearDerived : boolean = true);
   var
      i{,ThisRefDEM,ThisTestDEM,UsingRef} : integer;
      //Criterion2,WhatsMissing,
      What,TStr : shortstring;
      //RefGridLimits,TestGridLimits : tGridLimits;
      gl1 : tGridLimits;
      //Mean,Std : float32;
      //NPts : int64;

      procedure CheckNormalization(DEM : integer; What : shortstring);
      var
         Min,Max,NewMin,NewMax : float32;
         NormDB : tMyData;
         fName : PathStr;
      begin
         if ValidDEM(DEM) then begin
            {$IfDef RecordSSIMNormalization} WriteLineToDebugFile('NormalizeDEMforSSIM in for ' + DEMGlb[DEM].AreaName); {$EndIf};
            fName := DEMIXSettingsDir + 'ssim_normalization.dbf';
            NormDB := tMyData.Create(fName);
            NormDB.ApplyFilter('CRITERION=' + QuotedStr(What));
            NewMin := DEMGlb[DEM].DEMheader.MinElev;
            NewMax := DEMGlb[DEM].DEMheader.MaxElev;

            if (NormDB.FiltRecsInDB = 0) then begin
               Min := NewMin;
               Max := NewMax;
               NormDB.Insert;
            end
            else if (NormDB.FiltRecsInDB = 1) then begin
               Min := NormDB.GetFieldByNameAsFloat('MIN');
               if NewMin < Min then Min := NewMin;
               Max := NormDB.GetFieldByNameAsFloat('MAX');
               if NewMax > Max then Max := NewMax;
               NormDB.Edit;
            end;
            //else begin
               NormDB.SetFieldByNameAsString('CRITERION',What);
               NormDB.SetFieldByNameAsFloat('MIN',Min);
               NormDB.SetFieldByNameAsFloat('MAX',Max);
               NormDB.Post;
            //end;
         end;
      end;



   begin
       {$IfDef RecordDEMIXFull} WriteLineToDebugFile(AreaName + ' Criterion=' + Criterion); {$EndIf}
       InitializePointAndAreaGrids(PtSSIMGrids, AreaSSIMGrids);
       {$If Defined(RecordFUVsteps)} Stopwatch := TStopwatch.StartNew; {$EndIf}
       What := BeforeSpecifiedCharacterAnsi(Criterion,'_');
       for i := dmxFirstArea to NumAreaDEMs do CheckNormalization(AreaGrids[i],What);
       for i := dmxFirstPoint to NumPtDEMs do CheckNormalization(PointGrids[i],What);
       {$IfDef RecordDEMIXFull} WriteLineToDebugFile('Grids normalized for Criterion=' + Criterion); {$EndIf}
       wmdem.SetPanelText(2, 'SSIM grids',true);
       for i := 1 to NumPtDEMs do CloseSingleDEM(PtSSIMGrids[i]);
       for i := 1 to NumAreaDEMs do CloseSingleDEM(AreaSSIMGrids[i]);
       {$If Defined(RecordFUVstepsFull)} WriteLineToDebugFile('SSIM ' + What + ' Do criterion  ' + RealToString(Stopwatch.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}
       if ClearDerived then ClearDerivedGrids;
       //wmdem.SetPanelText(2,'',true);
   end;

//var
   //Success : boolean;
   //TStr    : shortstring;
   //fName2  : PathStr;
   //FullTiles : tStringList;
begin {procedure DoSSIMandFUVForAnArea}
       {$If Defined(TimeGridsForArea)} Stopwatch2 := TStopwatch.StartNew; {$EndIf}
       {$IfDef RecordDEMIX} HighLightLineToDebugFile('AreaSSIMandFUVComputations area=' + AreaName); {$EndIf}
       wmdem.SetPanelText(3, 'Load DEMs',true);
       ShowSatProgress :=  false;
       if OpenBothPixelIsDEMs(AreaName,'',DEMIX_Ref_1sec,DEMIX_test_dems,MDDef.OpenSavedMapsFUVSSIM) then begin
          {$If Defined(TrackPixelIs) or Defined(RecordDEMIXFull)} ShowDEMIXGrids(AreaName + ' DEMs opened',PointDEMs,AreaDEMs); {$EndIf}
          InitializePointAndAreaGrids(PointGrids,AreaGrids);

{$IfDef ExternalProgramFUV_SSIM}

          if MDDef.SSIM_ROTOR then begin //created by WbE with Jupyter Notebook
             wmdem.SetPanelText(3,DEMIXModeName + ' 1 ROTOR',true);
             {$If Defined(TimeOpenCreateGrids)} Stopwatch := TStopwatch.StartNew; {$EndIf}
             Success := OpenGridsCreatedByExternalProgram(MDDef.OpenSavedMapsFUVSSIM,'WBT',AreaName,'rotor_',PointGrids,AreaGrids);
             {$If Defined(TimeOpenCreateGrids)} WriteLineToDebugFile('WbW Rotor created ' + RealToString(Stopwatch.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}
             if Success then begin
                DoCriterion('ROTOR_SSIM',PointGrids,AreaGrids);
             end
             else begin
                ClearDerivedGrids;
                {$IfDef RecordDEMIX} HighLightLineToDebugFile('ROTOR criterion fail for area=' + AreaName); {$EndIf}
             end;
          end;

          if MDDef.SSIM_HAND then begin //created by WbW via Jupyter or via WbtT if not found
             wmdem.SetPanelText(3,DEMIXModeName + ' 2 HAND',true);
             {$If Defined(TimeOpenCreateGrids)} Stopwatch := TStopwatch.StartNew; {$EndIf}
             if not OpenGridsCreatedByExternalProgram(MDDef.OpenSavedMapsFUVSSIM,'WBT',AreaName,'HAND_',PointGrids,AreaGrids) then begin
                 WBT_CreateDEMIX_HANDGrids(MDDef.OpenSavedMapsFUVSSIM);
             end;
             {$If Defined(TimeOpenCreateGrids)} WriteLineToDebugFile('WbT HAND created ' + RealToString(Stopwatch.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}
             DoCriterion('HAND_SSIM',PointGrids,AreaGrids);
          end;

          if MDDef.SSIM_LS then begin //created by WbE with Jupyter Notebook
             wmdem.SetPanelText(3,DEMIXModeName + ' 9 LS',true);
             {$If Defined(TimeOpenCreateGrids)} Stopwatch := TStopwatch.StartNew; {$EndIf}
             Success := OpenGridsCreatedByExternalProgram(MDDef.OpenSavedMapsFUVSSIM,'wbt',AreaName,'sed_trans_',PointGrids,AreaGrids);
             {$If Defined(TimeOpenCreateGrids)} WriteLineToDebugFile('WbW LS ' + TStr + ' ' + RealToString(Stopwatch.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}
             if Success then DoCriterion('LS_SSIM',PointGrids,AreaGrids)
             else begin
                ClearDerivedGrids;
                {$IfDef RecordDEMIX} HighLightLineToDebugFile('LS criterion fail for area=' + AreaName); {$EndIf}
             end;
          end;

          if MDDef.SSIM_flow then begin //created by WbE with Jupyter Notebook
             wmdem.SetPanelText(3,DEMIXModeName + ' 10 FLOW',true);
             {$If Defined(TimeOpenCreateGrids)} Stopwatch := TStopwatch.StartNew; {$EndIf}
             WBT_CreateDEMIX_Flow_AccumulationGrids(false,MDDef.OpenSavedMapsFUVSSIM); //will be created with WbT
             {$If Defined(TimeOpenCreateGrids)} WriteLineToDebugFile('WbW Flow created   ' + RealToString(Stopwatch.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}
             DoCriterion('ACCUM_SSIM',PointGrids,AreaGrids);
          end;

          if MDDef.SSIM_flow then begin //created by WbE with Jupyter Notebook
             wmdem.SetPanelText(3,DEMIXModeName + ' 10 LOG FLOW',true);
             {$If Defined(TimeOpenCreateGrids)} Stopwatch := TStopwatch.StartNew; {$EndIf}
             WBT_CreateDEMIX_Flow_AccumulationGrids(true,MDDef.OpenSavedMapsFUVSSIM); //will be created with WbT
             {$If Defined(TimeOpenCreateGrids)} WriteLineToDebugFile('WbW LOG Flow created   ' + RealToString(Stopwatch.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}
             DoCriterion('LOGFA_SSIM',PointGrids,AreaGrids);
          end;

          if MDDef.SSIM_ProfC then begin //created by WbT
             wmdem.SetPanelText(3,DEMIXModeName + ' 11 PROFC',true);
             {$If Defined(TimeOpenCreateGrids)} Stopwatch := TStopwatch.StartNew; {$EndIf}
             for i := dmxFirstPoint to NumPtDEMs do PointGrids[i] := WBT_ProfileCurvature(MDDef.OpenSavedMapsFUVSSIM,DEMGlb[PointDEMs[i]].GeotiffDEMName);
             for i := dmxFirstArea to NumAreaDEMs do AreaGrids[i] := WBT_ProfileCurvature(MDDef.OpenSavedMapsFUVSSIM,DEMGlb[AreaDEMs[i]].GeotiffDEMName);
             {$If Defined(TimeOpenCreateGrids)} WriteLineToDebugFile('WbT PROFC created   ' + RealToString(Stopwatch.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}
             DoCriterion('PROFC_SSIM',PointGrids,AreaGrids);
          end;

          if MDDef.SSIM_PlanC then begin //created by WbT
             wmdem.SetPanelText(3,DEMIXModeName + ' 12 PLANC',true);
             {$If Defined(TimeOpenCreateGrids)} Stopwatch := TStopwatch.StartNew; {$EndIf}
             for i := dmxFirstPoint to NumPtDEMs do PointGrids[i] := WBT_PlanCurvature(MDDef.OpenSavedMapsFUVSSIM,DEMGlb[PointDEMs[i]].GeotiffDEMName);
             for i := dmxFirstArea to NumAreaDEMs do AreaGrids[i] := WBT_PlanCurvature(MDDef.OpenSavedMapsFUVSSIM,DEMGlb[AreaDEMs[i]].GeotiffDEMName);
             {$If Defined(TimeOpenCreateGrids)} WriteLineToDebugFile('WbT PLANC created   ' + RealToString(Stopwatch.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}
             DoCriterion('PLANC_SSIM',PointGrids,AreaGrids);
          end;

          if MDDef.SSIM_TangC then begin //created by WbT
             wmdem.SetPanelText(3,DEMIXModeName + ' 13 TANGC',true);
             {$If Defined(TimeOpenCreateGrids)} Stopwatch := TStopwatch.StartNew; {$EndIf}
             for i := dmxFirstPoint to NumPtDEMs do PointGrids[i] := WBT_TangentialCurvature(MDDef.OpenSavedMapsFUVSSIM,DEMGlb[PointDEMs[i]].GeotiffDEMName);
             for i := dmxFirstArea to NumAreaDEMs do AreaGrids[i] := WBT_TangentialCurvature(MDDef.OpenSavedMapsFUVSSIM,DEMGlb[AreaDEMs[i]].GeotiffDEMName);
             {$If Defined(TimeOpenCreateGrids)} WriteLineToDebugFile('WbT TANGC created   ' + RealToString(Stopwatch.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}
             DoCriterion('TANGC_SSIM',PointGrids,AreaGrids);
          end;

          if MDDef.SSIM_wet then begin //created by WbT
             wmdem.SetPanelText(3,DEMIXModeName + ' 14 WETIN',true);
             {$If Defined(TimeOpenCreateGrids)} Stopwatch := TStopwatch.StartNew; {$EndIf}
             if OpenGridsCreatedByExternalProgram(MDDef.OpenSavedMapsFUVSSIM,'WBT',AreaName,'wetin_',PointGrids,AreaGrids) then begin
                //HowDone := ' opened ';
             end
             else begin
                for i := dmxFirstPoint to NumPtDEMs do PointGrids[i] := WBT_WetnessIndex(MDDef.OpenSavedMapsFUVSSIM,true,DEMGlb[PointDEMs[i]].GeotiffDEMName,WetnessName);
                for i := dmxFirstArea to NumAreaDEMs do AreaGrids[i] := WBT_WetnessIndex(MDDef.OpenSavedMapsFUVSSIM,true,DEMGlb[AreaDEMs[i]].GeotiffDEMName,WetnessName);
                //HowDone := ' created ';
             end;
             {$If Defined(TimeOpenCreateGrids)} WriteLineToDebugFile('WbT Wet Index' + HowDone + RealToString(Stopwatch.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}
             DoCriterion('WETIN_SSIM',PointGrids,AreaGrids);
          end;

          if MDDef.SSIM_ConvergeIndex then begin //created by SAGA or WbW with Jupyter Notebook
             wmdem.SetPanelText(3,DEMIXModeName + ' 15 CONIN',true);
             {$If Defined(TimeOpenCreateGrids)} Stopwatch := TStopwatch.StartNew; {$EndIf}
             Success := SAGACreateDEMIX_ConIn_Grids(MDDef.OpenSavedMapsFUVSSIM,AreaName,'CONIN_');
             {$If Defined(TimeOpenCreateGrids)} WriteLineToDebugFile('WBT ConIn ' + RealToString(Stopwatch.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}
             if Success then DoCriterion('CONIN_SSIM',PointGrids,AreaGrids)
             else ClearDerivedGrids;
          end;


{$EndIf}

          if MDDef.SSIM_hill then begin
             wmdem.SetPanelText(3,DEMIXModeName + ' 3 Hillshade',true);
             {$If Defined(TimeOpenCreateGrids)} Stopwatch := TStopwatch.StartNew; {$EndIf}
             //CreateDEMIXhillshadeGrids(AreaName,MDDef.OpenSavedMapsFUVSSIM,true);
             CreateDEMIXDerivedGrids('HILL_',AreaName,MDDef.OpenSavedMapsFUVSSIM,true);

             {$If Defined(TimeOpenCreateGrids)} WriteLineToDebugFile('Hillshade' + HowDone + RealToString(Stopwatch.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}
             DoCriterion('HILL_SSIM',PointGrids,AreaGrids);
          end;

          if MDDef.SSIM_RRI then begin
             wmdem.SetPanelText(3,DEMIXModeName + ' 8 RRI',true);
             {$If Defined(TimeOpenCreateGrids)} Stopwatch := TStopwatch.StartNew; {$EndIf}
             //CreateDEMIXRRIgrids(AreaName,MDDef.OpenSavedMapsFUVSSIM,true);
             CreateDEMIXDerivedGrids('RRI_',AreaName,MDDef.OpenSavedMapsFUVSSIM,true);

             {$If Defined(TimeOpenCreateGrids)} WriteLineToDebugFile('MD RRI created   ' + RealToString(Stopwatch.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}
             DoCriterion('RRI_SSIM',PointGrids,AreaGrids);
          end;

          if MDDef.SSIM_TPI then begin
             wmdem.SetPanelText(3,DEMIXModeName + ' 16 TPI',true);
             {$If Defined(TimeOpenCreateGrids)} Stopwatch := TStopwatch.StartNew; {$EndIf}
             //CreateDEMIXTPIGrids(AreaName,MDDef.OpenSavedMapsFUVSSIM,true);
             CreateDEMIXDerivedGrids('TRI_',AreaName,MDDef.OpenSavedMapsFUVSSIM,true);
             {$If Defined(TimeOpenCreateGrids)} WriteLineToDebugFile('TPI' + HowDone + RealToString(Stopwatch.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}
             DoCriterion('TPI_SSIM',PointGrids,AreaGrids);
          end;

          if MDDef.SSIM_Slope or MDDef.SSIM_ruff then begin
             wmdem.SetPanelText(3,DEMIXModeName + ' 4,5 Slope/ruff',true);
             {$If Defined(TimeOpenCreateGrids)} Stopwatch := TStopwatch.StartNew; {$EndIf}
             CreateDEMIXSlopeRoughnessGrids(AreaName,MDDef.OpenSavedMapsFUVSSIM,true);
             {$If Defined(TimeOpenCreateGrids)} WriteLineToDebugFile('MD Slope/ruff' + HowDone + RealToString(Stopwatch.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}
             DoCriterion('SLOPE_SSIM',PointGrids2,AreaGrids2,false);
             DoCriterion('RUFF_SSIM',PointGrids,AreaGrids);
          end;

          if MDDef.SSIM_Openness then begin
             wmdem.SetPanelText(3,DEMIXModeName + ' 6,7 Openness',true);
             {$If Defined(TimeOpenCreateGrids)} Stopwatch := TStopwatch.StartNew; {$EndIf}
             CreateDEMIXOpennessGrids(AreaName,MDDef.OpenSavedMapsFUVSSIM,true);
             {$If Defined(TimeOpenCreateGrids)} WriteLineToDebugFile('MD Openness' + HowDone + RealToString(Stopwatch.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}
             DoCriterion('OPEND_SSIM',PointGrids2,AreaGrids2,false);
             DoCriterion('OPENU_SSIM',PointGrids,AreaGrids);
          end;

          if MDDef.SSIM_elev then begin
             //Elevation done last, so we no longer need elevation to create derived grids, and can normalize it
             wmdem.SetPanelText(3,DEMIXModeName + ' 17 ELEV',true);
             DoCriterion('ELEV_SSIM',PointDEMs,AreaDEMs);
          end;

          if (not MDDef.OpenSavedMapsFUVSSIM) then CloseAllDEMs;
          //CloseAndNilNumberedDB(DEMIXtileDB);
          CleanUpTempDirectory;  //lot of files created by SAGA and WBT
       end
       else begin
          {$IfDef RecordDEMIX} HighLightLineToDebugFile('Missing DEM files for area ' + AreaName); {$EndIf}
       end;
       {$If Defined(TimeGridsForArea)} WriteLineToDebugFile(AreaName + ' completed ' + RealToString(Stopwatch2.Elapsed.TotalSeconds/3600,-12,-4) + ' hours');  {$EndIf}
end {procedure CriteriaRanges};




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
   Result := StrUtils.AnsiContainsText(Criterion,'MEAN') or StrUtils.AnsiContainsText(Criterion,'MED');
end;



procedure CreateCopHeadToHeaddb(db : integer);
const
   Outcomes : array[1..9] of shortstring = ('COP beats ALOS','COP ties ALOS','COP loses to ALOS',
                                       'COP beats FABDEM','COP ties FABDEM','COP loses to FABDEM',
                                       'COP beats TANDEM-X','COP ties TANDEM-X','COP loses to TANDEM-X');
const
   TheDEMs : array[1..9] of shortstring = ('ALOS','FABDEM','TANDEM','COAST','DILUV','DELTA','SRTM','NASA','ASTER');
var
   Counts : array[1..9,0..25] of integer;
   Criteria,Results : tStringList;
   aline,Criterion : shortstring;
   i,j,theIndex : integer;
   fName : PathStr;

         procedure BoxScore(offset,TheIndex : integer; aField : shortstring);
         var
            Win : shortstring;
         begin
            Win := GISdb[DB].MyData.GetFieldByNameAsString(aField);
            if Win = 'COP' then begin
               inc(Counts[1 + Offset,Criteria.Count]);
               inc(Counts[1 + Offset,theIndex]);
            end
            else if Win = 'TIE' then begin
               inc(Counts[2 + Offset,Criteria.Count]);
               inc(Counts[2 + Offset,theIndex]);
            end
            else begin
               inc(Counts[3 + Offset,Criteria.Count]);
               inc(Counts[3 + Offset,theIndex]);
            end;
         end;


begin
   if (not GISdb[DB].MyData.FieldExists('TOLERANCE')) then begin
      if AnswerIsYes('Rank DEMs to get TOLERANCEs') then RankDEMS(DB)
      else exit;
   end;
   GISdb[DB].EmpSource.Enabled := false;
   Criteria := GISdb[DB].MyData.ListUniqueEntriesInDB('CRITERION');
   Results := tStringList.Create;
   aLine := 'OUTCOME';
   for i := 0 to pred(Criteria.Count) do aline := aline + ',' + Criteria.Strings[i];
   Results.Add(aline + ',ALL');
   for i := 1 to 9 do
      for j := 0 to 25 do Counts[i,j] := 0;
   GISdb[DB].EmpSource.Enabled := false;
   GISdb[DB].MyData.First;
   while not GISdb[DB].MyData.eof do begin
      Criterion := GISdb[DB].MyData.GetFieldByNameAsString('CRITERION');
      theIndex := Criteria.IndexOf(Criterion);
      BoxScore(0,TheIndex,'COP_ALOS');
      BoxScore(3,TheIndex,'COP_FABDEM');
      BoxScore(6,TheIndex,'COP_TANDEM');
      GISdb[DB].MyData.Next;
   end;
   for I := 1 to 9 do begin
      aLine := Outcomes[i];
      for j := 0 to Criteria.Count do begin
         aline := aline + ',' + IntToStr(Counts[i,j])
      end;
      Results.Add(aline);
   end;
   fName := NextFileNumber(MDTempDir,'cop_boxscore_','.dbf');
   PetdbUtils.StringList2CSVtoDB(Results,fName);
   GISdb[DB].EmpSource.Enabled := true;
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
      RankDEMS(db);
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


