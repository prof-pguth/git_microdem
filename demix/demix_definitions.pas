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
   //{$Define RecordOpenExternalProgramGrids}
   //{$Define RecordDEMIXLoad}
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
   {//$Define RecordDEMIXhillshades}
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
   //NumGridParams = 10;

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
   ParamSuffixes : array[1..5] of shortstring = ('_AVD','_STD','_RMSE','_MAE','_LE90');

   NumTileCharacters = 10;
   TileCharacters : array[1..NumTileCharacters] of shortstring = ('AVG_ELEV','AVG_ROUGH','AVG_SLOPE','BARREN_PC','FOREST_PC','RELIEF','URBAN_PC','WATER_PC','MIN_ELEV','MAX_ELEV');

   RefDEMType : array[1..2] of shortstring = ('DSM','DTM');

   NumLandTypes = 8;
   LandTypes : array[1..NumLandTypes] of shortstring = ('ALL','FLAT','GENTLE','STEEP','CLIFF','URBAN','FOREST','BARREN');
   opByCluster = 1;
   opByDEM = 2;

const
   MaxDEMIXDEM = 10;
   //MaxUnretiredDEM = 7;
   NumDEMIXtestDEM : integer = 0;
   AllDEMIXTheDEMs : array[1..MaxDEMIXDEM] of shortstring = ('COP','ALOS','FABDEM','TANDEM','COAST','DILUV','DELTA','SRTM','NASA','ASTER');

type
   tDEMIXindexes = array[1..MaxDEMIXDEM] of integer;
   tDEMIXfloats = array[1..MaxDEMIXDEM] of float32;

var
   DEMIXDEMTypeName : array[1..MaxDEMIXDEM] of shortstring;
   DEMIXshort : array[1..MaxDEMIXDEM] of shortstring;
   DEMIXDEMcolors : array[1..MaxDEMIXDEM] of tPlatformColor;
   UseRetiredDEMs : array[1..MaxDEMIXDEM] of boolean;
   CriteriaFamily : shortstring;
   DEMIXModeName : shortstring;

const
   Ref1SecPointStr = '_ref_1sec_point';
   Ref1SecAreaStr =  '_ref_1sec_area';
   Ref1_5SecPointStr = '_ref_1.5x1sec_point';

//this should be rolled into a DEMIX_grid object; for now it is global variables
         const
            MaxPossGrids = 10;
            NumPtDEMs : integer = 6;
            NumAreaDEMs : integer = 1;
            PossPt = 8;
            PossArea = 2;

         //the newer code using a set of arrays for the point and area DEMs, which can have corresponding array for derived grids like slope
         //-1 for the high latitude (currently none for area grid),
         //0 for the 1" reference,
         //the rest for the test DEMs
         const
            SRTM_centroid_names : array[-1..PossPt] of shortstring = ('REF_HI_PNT','REF_POINT','COP','TANDEM','FABDEM','NASA','SRTM','ASTER','COAST','DELTA');
            ALOS_centroid_names : array[-1..PossArea] of shortstring = ('REF_HI_AREA','REF_AREA','ALOS','DILUV');
         type
            tDEM_int_array = array [-1..MaxPossGrids] of integer; //-1 for high latitude ref DEM, 0 for ref DEMs, others for the test DEMs
         var
            PointDEMs,AreaDEMs,      //used for referenced and test DEMs
            AreaGrids,PointGrids,    //used for derived parameters
            AreaGrids2,PointGrids2,  //used for parameters created at the same time (slope/roughness, openness upward and downward
            PtSSIMGrids, AreaSSIMGrids : tDEM_int_array;
            dmxFirstPoint,dmxFirstArea : integer;

         procedure ZeroPointAndAreaGrids(var PointGrids,AreaGrids : tDEM_int_array; InitValue : integer = 0);
         function RefGridForThisPointGrid(WhatGroup : tDEM_int_array; i : integer) : integer;
         procedure ShowDEMIXgrids(WhatFor : shortstring; PointGrids,AreaGrids : tDEM_int_array);

         //grids created by MICRODEM
         procedure CreateDEMIXhillshadeGrids(AreaName : shortstring; var HowDone : shortstring; OpenMaps : boolean = false; SaveMaps : boolean = false);
         procedure CreateDEMIXSlopeRoughnessGrids(AreaName : shortstring; var HowDone : shortstring; OpenMaps : boolean = false; SaveMaps : boolean = false);
         procedure CreateDEMIXOpennessGrids(AreaName : shortstring; var HowDone : shortstring; OpenMaps : boolean = false; SaveMaps : boolean = false);
         procedure CreateDEMIXTPIGrids(AreaName : shortstring; var HowDone : shortstring; OpenMaps : boolean = false; SaveMaps : boolean = false);
         procedure CreateDEMIXRRIgrids(AreaName : shortstring; OpenMaps : boolean = false; SaveMaps : boolean = false);

         //grids created by SAGA
         function SAGACreateDEMIX_LS_Grids(AreaName,aParam : shortstring; OpenMaps : boolean = false) : boolean;

const
   yasName = 0;
   yasSlope = 1;
   yasRuff = 2;
   yasRelief = 3;
   yasBestEval = 4;
   yasBarren = 5;
   yasForest = 6;
   //yasBestEvalByCriterion = 7;
   yasBestEvalColoredBySlope = 8;
   //yasBestEvalFilteredBySlope = 9;
   //xawEvaluation = 0;
   //xawScore = 1;
   //yawArea = 0;
   //yawTile = 1;
   //DEMIX_combined_graph : boolean = false;
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
   DEMListFName,

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
   procedure ComputeDEMIX_Diff_Dist_tile_stats(Overwrite : boolean);
   procedure CreateDEMIX_GIS_database_by_transposing(Overwrite : boolean);
   procedure RankDEMS(DBonTable : integer);
   function AverageScoresOfDEMs(DBonTable : integer; DEMs : tStringList; Ext : ExtStr = '_SCR'; Filters : tStringList = nil; Labels : tStringList = Nil) : integer;
   //procedure AddFilteredRankID(DBonTable : integer);
   procedure ModeOfDifferenceDistributions;
   procedure AddTileCharacteristicsToDB(DBonTable : integer);
   procedure AddFieldsToDEMIXDB(DBonTable : integer; theFields : tStringList);
   procedure MakeWinsDB(DBonTable : integer; aField : shortstring);

   procedure EvalRangeAndBestEvalForCriterion(DBonTable : integer);
   procedure CreateFinalDB;
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
   procedure ShifDEMsto_UTM_WGS84_EGM2008(Overwrite : boolean; DataDirs : tStringList = Nil);
   procedure DEMIX_MergeReferenceDEMs(Overwrite : boolean; DataDirs : tStringList = Nil);
   procedure CreateLandCoverGrids;
   procedure MoveReferenceDSMs;


//create reference DEMs, non-3DEP
   procedure DEMIX_merge_source(Areas : tStringList = Nil);
   procedure DEMIX_CreateReferenceDEMs(Overwrite : boolean; ResampleMode : byte; Areas : tStringList = Nil);
   procedure DEMIXCreateHalfSecRefDEMs(AreaName : shortstring = '');
   procedure ResampleForDEMIXOneSecDEMs(Overwrite : boolean; CloseAfter : boolean; DEM : integer; OpenMap : boolean = false; OutPath : PathStr = ''; ResampleMode : byte = 1);


//create test DEMs
   function CreateDEMIXTestDEMsForArea(Overwrite : boolean; AreaName : ShortString; AreaRefDEM,PointRefDEM : integer) : boolean;
   procedure DiluviumDTMforTestAreas(Overwrite : boolean; Areas : tStringList = Nil);
   procedure DeltaDTMforTestAreas(Overwrite : boolean; Areas : tStringList = Nil);
   procedure CoastalDTMforTestAreas(Overwrite : boolean; Areas : tStringList = Nil);
   procedure CreateTestAreaDEMs(Overwrite : boolean);
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
procedure ComputeAverageScoresForSelectedCriteria(db : integer; CriteriaList : tStringList; var Scores : tDEMIXfloats; var NumTies : integer; var WinnerString : shortstring);

procedure CreateCopHeadToHeaddb(db : integer);
procedure CriteriaInSSIM_FUV_db(db : integer);

function IsDEMIX_signedCriterion(Criterion : shortstring) : boolean;

procedure ClusterCompositionByDBfield(DBonTable : integer);
procedure ClusterFrequencyForSelectedField(DBonTable : integer);
procedure AreasInClusters(DB : integer);

procedure FilterTableForDEMIXevaluation(DBonTable,Value : integer; anOperator : shortString = '<=');

//links to other programs
   function OpenGridsCreatedByExternalProgram(OpenMaps : boolean; aProgram,AreaName,Param : shortString; var PointGrids,AreaGrids : tDEM_int_array) : boolean;
   procedure WBT_CreateDEMIX_HANDGrids(OpenMaps : boolean = false);
   procedure WBT_CreateDEMIX_Flow_AccumulationGrids(Log : boolean; OpenMaps : boolean = false);
   procedure WBT_CreateDEMIX_GeomorphonGrids(OpenMaps : boolean = false);
   function SAGACreateDEMIX_ConIn_Grids(OpenMaps : boolean; AreaName,aParam : shortstring) : boolean;

procedure ClearDerivedGrids;

function ExternalProgramOutPutFile(i : integer; aProgram,Param,AreaName : shortstring; IsPoint : boolean) : PathStr;
procedure MakeTerrainGridsFromMICRODEM(DataDir : PathStr; DEMIndex : integer; IsPoint : boolean);

function LinkedGraphofCriteriaEvaluations(DBonTable : integer; What : shortstring; ClusterOption : boolean): tThisBaseGraph;



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
      CheckFieldForFilter(AllDEMIXTheDEMs[i]);
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

procedure ComputeAverageScoresForSelectedCriteria(db : integer; CriteriaList : tStringList; var Scores : tDEMIXfloats; var NumTies : integer; var WinnerString : shortstring);
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
         for i := 1 to NumDEMIXtestDEM do begin
            Scores[i] := Scores[i] + GISdb[DB].MyData.GetFieldByNameAsFloat(DEMIXShort[i] + '_SCR');
         end;
      end;
      GISdb[DB].MyData.Next;
   end;
   for I := 1 to NumDEMIXtestDEM do Scores[i] := Scores[i] / Opinions;
   LowScore := 999;
   WinnerString := '';
   for I := 1 to NumDEMIXtestDEM do begin
      if Scores[i] < LowScore - 0.001 then begin
         LowScore := Scores[i];
         WinnerString := DEMIXShort[i];
         NumTies := 1;
      end
      else if Scores[i] < LowScore + 0.001 then begin
         WinnerString := WinnerString + ';' + DEMIXShort[i];
         inc(NumTies);
      end;
   end;
end;



initialization
finalization

end.


