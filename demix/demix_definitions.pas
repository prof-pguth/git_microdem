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
   //{$Define RecordDEMIXLoad}
   {$Define RecordDiluvium}
   //{$Define Record3DEPX}
   //{$Define RecordDEMIXRefDEM}
   //{$Define Record3DEPXFull}
   {$Define RecordDEMIX_evaluations_graph}
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

    System.SysUtils,System.Classes,System.UITypes,
    StrUtils,dbGrids,
    VCL.ExtCtrls,VCL.Forms, VCL.Graphics, VCL.Controls,
    WinAPI.Windows,
    Petmar,Petmar_types,BaseGraf,
    DEMDefs;

const
   JustTileStats : boolean = false;
   MaskForDiluvium = false;

const
   MaxTiles = 3000;
   MaxCriteria = 25;
   MaxAreas = 250;
   MaxClusters = 20;
   MaxDEMs = 10;
   NumGridParams = 10;

const
   DEMIX_vert_datum_code : integer = 0;

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

   NumTileCharacters = 10;
   TileCharacters : array[1..NumTileCharacters] of shortstring = ('AVG_ELEV','AVG_ROUGH','AVG_SLOPE','BARREN_PC','FOREST_PC','RELIEF','URBAN_PC','WATER_PC','MIN_ELEV','MAX_ELEV');

   //NCrits = 12;
   //Crits : array[1..NCrits] of shortstring = ('ELEV_SSIM','RRI_SSIM','SLOPE_SSIM','HILL_SSIM','RUFF_SSIM','TPI_SSIM','ELEV_FUV','SLOPE_FUV','HILL_FUV','RUFF_FUV','TPI_FUV','RRI_FUV');

   RefDEMType : array[1..2] of shortstring = ('DSM','DTM');

   NumLandTypes = 8;
   LandTypes : array[1..NumLandTypes] of shortstring = ('ALL','FLAT','GENTLE','STEEP','CLIFF','URBAN','FOREST','BARREN');
   opByCluster = 1;
   opByDEM = 2;



const
   MaxDEMIXDEM = 10;
   NumDEMIXtestDEM : integer = 0;
type
   tDEMIXindexes = array[1..MaxDEMIXDEM] of integer;
   tDEMIXfloats = array[1..MaxDEMIXDEM] of float32;

var
   DEMIXDEMTypeName : array[1..MaxDEMIXDEM] of shortstring;
   DEMIXshort : array[1..MaxDEMIXDEM] of shortstring;
   DEMIXDEMcolors : array[1..MaxDEMIXDEM] of tPlatformColor;
   UseRetiredDEMs : array[1..MaxDEMIXDEM] of boolean;
const
   Ref1SecPointStr = '_ref_1sec_point';
   Ref1SecAreaStr =  '_ref_1sec_area';
   Ref1_5SecPointStr = '1.5x1sec_point';

//this should be rolled into a DEMIX_grid object; for now it is global variables
         const
            MaxPossGrids = 10;
            NumPtDEMs : integer = 6;
            NumAreaDEMs : integer = 1;
            PossPt = 8;
            PossArea = 2;

            DEMIXsymsize : integer = 5;

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
            AreaGrids2,PointGrids2,
            PtSSIMGrids, AreaSSIMGrids : tDEM_int_array;
            dmxFirstPoint,dmxFirstArea : integer;

         procedure ZeroPointAndAreaGrids(var PointGrids,AreaGrids : tDEM_int_array);
         procedure CreateDEMIXhillshadeGrids(OpenMaps : boolean = false);
         procedure CreateDEMIXSlopeRoughnessGrids(OpenMaps : boolean = false);
         function RefGridForThisPointGrid(WhatGroup : tDEM_int_array; i : integer) : integer;
         procedure ShowDEMIXgrids(WhatFor : shortstring; PointGrids,AreaGrids : tDEM_int_array);


const
   yasName = 0;
   yasSlope = 1;
   yasRuff = 2;
   yasRelief = 3;
   yasBestEval = 4;
   yasBarren = 5;
   yasForest = 6;
   yasBestEvalByCriterion = 7;
   yasBestEvalBySlope = 8;
   xawEvaluation = 0;
   xawScore = 1;
   yawArea = 0;
   yawTile = 1;
   DEMIX_combined_graph : boolean = false;
   MovieByTestDEM : boolean = false;
   PanelsByTestDEM : boolean = false;
   DEMIX_Tile_Full : byte = 75;
var
   XAxisWhat,YAxisWhat,YAxisSort,YaxisTileOrArea : integer;
   DEMIXVertAxisLabel : ANSIstring;

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

   DEMIX_final_DB_dir,

   SSIMresultsDir,  //used internally, and set to one of the next three depending on which version is on
   DeltaSSIMresultsDir, DiluvSSIMresultsDir, RegularSSIMresultsDir,CoastalSSIMresultsDir,

   DEMIX_Ref_1sec,DEMIX_test_dems,DEMIX_diluvium_dtms,DEMIX_delta_dtms,DEMIX_coastal_dtms,  //locations for the 1" DEMs used in comparison

   DEMIX_Ref_Half_sec,
   DEMIX_Base_DB_Path,DEMIX_profile_test_dir,

  {$IfDef DEMIX_SAGA_channels}
      //directories for channel criterion calculations, currently replaced with Python WbW creation
      DEMIX_test_DEMs_no_sink, DEMIX_ref_DEMs_no_sink,
      DEMIX_test_DEMs_channels, DEMIX_ref_DEMs_channels,
      DEMIX_test_DEMs_channel_grids, DEMIX_ref_DEMs_channel_grids,
   {$EndIf}


   DEMIX_wbt_test_DEM_output,DEMIX_wbt_ref_DEM_output,
   ChannelMissesDir,DEMIX_diff_dist,

   DEMIX_area_lc100,

   DEMIX_area_dbName,
   //DEMIX_Ref_Source,
   DEMIX_Ref_Merge,
   DEMIX_GIS_dbName,

   AreaListFName,
   DEMListFName,

   DEMIX_distrib_graph_dir,DEMIX_diff_maps_dir,DEMIX_3DEP_Dir,

   GeodeticFName, IceSatFName, LandCoverFName,
   LocalDatumAddFName,LocalDatumSubFName,
   RefDSMPointFName,RefDSMareaFName,RefDTMPointFName,RefDTMareaFName, COPRefDTMFName,COPRefDSMFName : PathStr;

function DEMIX_mode_abbreviation(DEMIX_mode : integer) : shortstring;


//create or edit database
   procedure MakeDBForParamStats(Option,DBonTable : integer);
   procedure DEMIX_SSIM_FUV_transpose_kmeans_new_db(DBonTable : integer);
   procedure ComputeDEMIX_tile_stats(Overwrite : boolean);
   procedure CreateDEMIX_GIS_database_by_transposing(Overwrite : boolean);
   procedure RankDEMS(DBonTable : integer);
   procedure SumsOfRankDEMS(DBonTable : integer);
   procedure AddFilteredRankID(DBonTable : integer);
   procedure ModeOfDifferenceDistributions;
   procedure AddTileCharacteristicsToDB(DBonTable : integer);
   procedure AddFieldsToDEMIXDB(DBonTable : integer; theFields : tStringList);

   //procedure SwitchSSIMorFUVScoring(DBonTable : integer);
   procedure EvalRangeAndBestEvalForCriterion(DBonTable : integer);
   procedure CreateFinalDB;
   procedure AddCountryToDB(DB : integer);
   //procedure MergeCSVtoCreateFinalDB(SSIM_FUV : boolean);
   procedure MergeCSVtoCreateFinalDB(CSVdir : PathStr;FileDescription,dbName : shortstring);
   procedure AddPercentPrimaryData(DBonTable : integer);
   procedure AddAreaToDB(DBonTable : integer);


//clusters function
   procedure TileCharateristicsWhiskerPlotsByCluster(DBonTable : integer; NoFiltering : boolean; SingleCriterion : shortstring = '');
   procedure DEMIX_COP_clusters_tile_stats(DBonTable : integer);
   procedure DEMIX_clusters_per_tile(DBonTable : integer);

   procedure DEMIX_SSIM_FUV_clusters_diversity_graphs(DBonTable : integer; ColorByDEM : boolean = true);
   function DEMIX_SSIM_FUV_cluster_sensitivity_graph(DBonTable : integer) : tThisBaseGraph;
   function DEMIX_SSIM_FUV_clusters_graph(DBonTable : integer) : tThisBaseGraph;


//create reference DEMs
   procedure DEMIX_CreateReferenceDEMsFromSource(Overwrite : boolean; DataDirs : tStringList = Nil);
   procedure DEMIX_GDAL_Ref_DEM_datum_shift(Overwrite : boolean; DataDirs : tStringList = Nil);
   //procedure DEMIXRef_DEM_create_full_chain(overwrite : boolean);
   procedure DEMIX_Ref_DEM_full_chain(overwrite : boolean);
   procedure ShifDEMsto_UTM_WGS84_EGM2008(Overwrite : boolean; DataDirs : tStringList = Nil);
   procedure DEMIX_MergeReferenceDEMs(Overwrite : boolean; DataDirs : tStringList = Nil);
   procedure CreateLandCoverGrids;

//create reference DEMs, non-3DEP
   //procedure DEMIX_merge_Visioterra_source(AreaName : shortstring = '');
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

//inventory and reports
   procedure DEMIXTileSummary(DBonTable : integer);
   procedure DEMIXtile_inventory(DBonTable : integer);
   procedure InventoryDBwithDSMandDTMbyArea;
   procedure InventoryDEMIXdifferenceStats;
   procedure CheckTestDEMs;
   procedure Inventory3DEPtiles;
   procedure CheckReferenceDEMsAreEGMandPixelIs;
   {$IfDef DEMIX_SAGA_channels} procedure InventoryChannelDataByArea; {$EndIf}
   procedure CheckLowElevationAreas;
   //procedure CheckDeltaDTMAreas;
   procedure VerifyAllMapsReadyForSSIM;
   procedure VerifyTestDEMcoverages;
   procedure ComputeDEMIX_Summary_stats_DB;
   procedure InventoryDEMIX_SSIM_FUV_Stats;
   procedure DeleteFilesForATestArea;
   procedure FindFilesWith42112;
   procedure FixFilesWith42112;
   procedure EvalRangeAndStatsByCriterion(DBonTable : integer);
   procedure GetRangesForSSIM;
   procedure InventoryCriteriaEachDEMIXtile(DB : integer);
   procedure InventoryPercentileByCriterionEachDEMIXtile(DB : integer);
   procedure FindTilesInAreaForCoast;
   procedure InventoryAllDEMIXdata;


//channel network comparisons
  // function ChannelSHPToGrid(DEM,db : integer; fName : PathStr; PlotOrder : integer = 1) : integer;
   procedure CompareChannelNetworks(Overwrite : boolean; Area : shortstring);
   procedure ChannelNetworkMissPercentages(Overwrite : boolean; AreasWanted : tstringlist = nil);

   {$IfDef DEMIX_SAGA_channels}
      procedure CreateChannelNetworkGridsFromVectors(Overwrite : boolean; AreasWanted : tstringlist = nil);
      procedure BatchRemoveSinksInDEMIX_DEMS(Overwrite : boolean; AreasWanted : tstringlist = nil);
      procedure BatchCreateVectorChannelNewtwork(Overwrite : boolean; AreasWanted : tstringlist = nil);
      procedure ChannelNetworkMapComparison(Overwrite : boolean; AreaName,TestDEMName : shortstring);
      procedure MultistepChannelNetworks(Overwrite : boolean);
   {$EndIf}

procedure ClearDoubleProcessed;

procedure OneDegreeTilesToCoverTestAreas;



procedure MaskWaterInReferenceDEMs;
procedure TrimReferenceDEMsToDEMIXtiles;

function AreDEMIXscoresInDB(db : integer) : boolean;
procedure ComputeAverageScoresForSelectedCriteria(db : integer; CriteriaList : tStringList; var Scores : tDEMIXfloats; var NumTies : integer; var WinnerString : shortstring);

procedure CopHeadToHead(db : integer);
procedure CriteriaInSSIM_FUV_db(db : integer);

function IsDEMIX_signedCriterion(Criterion : shortstring) : boolean;

procedure ClusterCompositionByDBfield(DBonTable : integer);
procedure ClusterFrequencyForSelectedField(DBonTable : integer);
procedure AreasInClusters(DB : integer);

procedure FilterTableForDEMIXevaluation(DBonTable,Value : integer);
procedure CreateDEMIX_HANDGrids(OpenMaps : boolean = false);
procedure CreateDEMIX_Flow_AccumulationGrids(OpenMaps : boolean = false);

procedure MapsByClusterAndDEM(DBonTable : integer);



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



procedure MapsByClusterAndDEM(DBonTable : integer);
const
   PlotOrder : array[1..7] of shortstring = ('COP','TANDEM','FABDEM','ALOS','NASA','SRTM','ASTER');
var
   DEM,Cluster,NCluster : integer;
   Panels : tStringList;
   fName : PathStr;
   aFilter : shortstring;
   bmp : tMyBitmap;
begin
    Panels := tStringList.Create;
    GISdb[DBontable].EmpSource.Enabled := false;
    NCluster := GISdb[DBontable].MyData.NumUniqueEntriesInDB('CLUSTER');
    if GISdb[DBontable].MyData.FieldExists('CLUSTER') and GISdb[DBontable].MyData.FieldExists('DEM') then begin
       for DEM := 1 to NumDEMIXtestDEM  do begin
          for Cluster := 1 to NCluster do begin
             aFilter := 'DEM=' + QuotedStr(PlotOrder[DEM]) + ' AND CLUSTER=' + IntToStr(Cluster);
             GISdb[DBontable].ApplyGISFilter(aFilter);
             GISdb[DBontable].RedrawLayerOnMap;
             fName := NextFileNumber(MDtempDir,'dem_cluster_map_','.bmp');
             //SaveImageAsBMP(GISdb[DBontable].TheMapOwner.Image1,fName);
             GISdb[DBontable].TheMapOwner.Image1.Canvas.Font.Size := 14;
             aFilter := aFilter + ' (n=' + IntToStr(GISdb[DBontable].MyData.FiltRecsInDB) + ')';
             GISdb[DBontable].TheMapOwner.Image1.Canvas.TextOut(5,GISdb[DBontable].TheMapOwner.Image1.Height - 30,aFilter);
             GISdb[DBontable].TheMapOwner.OutlineMap;
             CopyImageToBitmap(GISdb[DBontable].TheMapOwner.Image1,bmp);
             if DEM < NumDEMIXtestDEM then bmp.Width := bmp.Width + 10;
             if Cluster < nCluster then bmp.Height := bmp.height + 10;
             bmp.SaveToFile(fName);
             Panels.Add(fName);
          end;
       end;
       GISdb[DBontable].ClearGISFilter;
       MakeBigBitmap(Panels,'','',NCluster);
    end
    else begin
       MessageToContinue('Required fields CLUSTER and DEM');
    end;
end;

function DEMIX_mode_abbreviation(DEMIX_mode : integer) : shortstring;
begin
   case DEMIX_mode of
      dmClassic : Result := 'ALL';
      dmAddCoastal : Result := 'U120';
      dmAddDiluvium : Result := 'U80';
      dmAddDelta : Result := 'U10';
   end;
end;

procedure FilterTableForDEMIXevaluation(DBonTable,Value : integer);
var
   i : integer;
   aFilter : shortstring;
begin
   aFilter := '';
   for i := 1 to NumDEMIXtestDEM do begin
      if GISdb[DBonTable].MyData.FieldExists(DEMIXshort[i]) then begin
         if length(aFilter) > 0 then aFilter := aFilter + ' OR ';
         aFilter := aFilter + DEMIXshort[i] + '<=' + IntToStr(Value);
      end;
   end;
   if length(aFilter) > 0 then GISdb[DBonTable].ApplyGISFilter(aFilter)
   else MessageToContinue('No DEMIX DEM fields in DB');
end;


function IsDEMIX_signedCriterion(Criterion : shortstring) : boolean;
begin
   Result := StrUtils.AnsiContainsText(Criterion,'MEAN') or StrUtils.AnsiContainsText(Criterion,'MED');
end;


function RefGridForThisPointGrid(WhatGroup : tDEM_int_array; i : integer) : integer;
begin
    if ValidDEM(i) then begin
       {$IfDef RecordDEMIXRefDEM} WriteLineToDebugFile(DEMGlb[WhatGroup[i]].AreaName + ' ' + RealToString(3600 * DEMglb[WhatGroup[i]].DEMHeader.DEMxSpacing,-8,-2) + 'sec'); {$EndIf}
       if (DEMglb[WhatGroup[i]].DEMHeader.DEMxSpacing > 1.05/3600) then Result := WhatGroup[-1]
       else Result := WhatGroup[0];
       {$IfDef RecordDEMIXRefDEM} WriteLineToDebugFile('Ref DEM for ' + DEMGlb[WhatGroup[i]].AreaName + ' is ' + DEMGlb[Result].AreaName); {$EndIf}
    end
    else begin
       {$IfDef RecordDEMIX} WriteLineToDebugFile('Invaild DEM=' + IntToStr(i) + ' in RefGridForThisPointGrid'); {$EndIf}
    end;
end;


procedure ShowDEMIXgrids(WhatFor : shortstring; PointGrids,AreaGrids : tDEM_int_array);
var
   i : integer;
begin
    HighlightLineToDebugFile(WhatFor);
    for i := -1 to NumPtDEMs do
       if ValidDEM(PointGrids[i]) then
          WriteLineToDebugFile(IntegerToString(PointGrids[i],3) + '  ' + DEMglb[PointGrids[i]].AreaName + '  ' + DEMglb[PointGrids[i]].GridCornerModelAndPixelIsString);
    for i := -1 to NumAreaDEMs do
       if ValidDEM(AreaGrids[i]) then
          WriteLineToDebugFile(IntegerToString(AreaGrids[i],3) + '  ' + DEMglb[AreaGrids[i]].AreaName + '  ' + DEMglb[AreaGrids[i]].GridCornerModelAndPixelIsString);
end;



procedure CreateDEMIXhillshadeGrids(OpenMaps : boolean = false);
var
   i : integer;
begin
    wmdem.SetPanelText(3, 'Compute Hillshade',true);
    for i := dmxFirstPoint to NumPtDEMs do begin
       PointGrids[i] := MakeSingleNewDerivativeMap('R',PointDEMs[i],0,OpenMaps);
       {$IfDef RecordDEMIXFull} WriteLineToDebugFile('Grid=' + IntToStr(PointGrids[i]) + ' Hillshade for ' + DEMGlb[PointDEMs[i]].AreaName); {$EndIf}
    end;
    for i := dmxFirstArea to NumAreaDEMs do begin
       {$IfDef RecordDEMIXFull} WriteLineToDebugFile('Hillshade for ' + DEMGlb[AreaDEMs[i]].AreaName); {$EndIf}
       AreaGrids[i] := MakeSingleNewDerivativeMap('R',AreaDEMs[i],0,OpenMaps);
       {$IfDef RecordDEMIXFull} WriteLineToDebugFile('Grid=' + IntToStr(AreaGrids[i]) + ' Hillshade for ' + DEMGlb[PointDEMs[i]].AreaName); {$EndIf}
    end;
end;


procedure CreateDEMIXSlopeRoughnessGrids(OpenMaps : boolean = false);
var
   i : integer;
begin
    wmdem.SetPanelText(3, 'Compute slope/roughness',true);
    for i := dmxFirstPoint to NumPtDEMs do PointGrids2[i] := 0;  //so slope grids are returned
    for i := dmxFirstArea to NumAreaDEMs do AreaGrids2[i] := 0;  //so slope grids are returned
    for i := dmxFirstPoint to NumPtDEMs do PointGrids[i] := CreateSlopeRoughnessSlopeStandardDeviationMap(PointDEMs[i],5,PointGrids2[i],OpenMaps);
    for i := dmxFirstArea to NumAreaDEMs do AreaGrids[i] := CreateSlopeRoughnessSlopeStandardDeviationMap(AreaDEMs[i],5,AreaGrids2[i],OpenMaps);
end;


procedure CreateDEMIX_HANDGrids(OpenMaps : boolean = false);
var
   i : integer;
   BreachName,FlowAccumulationName,StreamName,HANDName  : PathStr;
begin
   for i := dmxFirstPoint to NumPtDEMs do begin
      BreachName := '';
      FlowAccumulationName  := '';
      StreamName := '';
      HANDName := '';
      PointGrids[i] := WBT_ElevAboveStream(OpenMaps,DEMGlb[PointDEMs[i]].GeotiffDEMName,BreachName,FlowAccumulationName,StreamName,HANDName);
   end;
   for i := dmxFirstArea to NumAreaDEMs do begin
      BreachName := '';
      FlowAccumulationName  := '';
      StreamName := '';
      HANDName := '';
      AreaGrids[i] := WBT_ElevAboveStream(OpenMaps,DEMGlb[AreaDEMs[i]].GeotiffDEMName,BreachName,FlowAccumulationName,StreamName,HandName);
   end;
end;


procedure CreateDEMIX_Flow_AccumulationGrids(OpenMaps : boolean = false);
var
   i : integer;
   BreachName,FlowAccumulationName,StreamName,HANDName  : PathStr;
begin
    //using Log, and using D8
    for i := dmxFirstPoint to NumPtDEMs do begin
      BreachName := '';
      FlowAccumulationName  := '';
      StreamName := '';
      HANDName := '';
       PointGrids[i] := WBT_FlowAccumulation(OpenMaps,True,True,DEMGlb[PointDEMs[i]].GeotiffDEMName,BreachName,FlowAccumulationName);
    end;
    for i := dmxFirstArea to NumAreaDEMs do begin
      BreachName := '';
      FlowAccumulationName  := '';
      StreamName := '';
      HANDName := '';
       AreaGrids[i] := WBT_FlowAccumulation(OpenMaps,True,True,DEMGlb[AreaDEMs[i]].GeotiffDEMName,BreachName,FlowAccumulationName);
    end;
    {$IfDef RecordDEMIXFull} WriteLineToDebugFile('Flow accumulation grids created'); {$EndIf}
end;



procedure ZeroPointAndAreaGrids(var PointGrids,AreaGrids : tDEM_int_array);
var
   i : integer;
begin
   for i := -1 to MaxPossGrids do PointGrids[i] := 0;
   for i := -1 to MaxPossGrids do AreaGrids[i] := 0;
end;


procedure CopHeadToHead(db : integer);
const
   Outcomes : array[1..9] of shortstring = ('COP beats ALOS','COP ties ALOS','COP loses to ALOS',
                                       'COP beats FABDEM','COP ties FABDEM','COP loses to FABDEM',
                                       'COP beats TANDEM-X','COP ties TANDEM-X','COP loses to TANDEM-X');
var
   Counts : array[1..9,0..25] of integer;
   Win : shortstring;
   Criteria,Results : tStringList;
   aline,Criterion : shortstring;
   i,j,theIndex : integer;
   fName : PathStr;

         procedure BoxScore(offset,TheIndex : integer; Win : shortstring);
         begin
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
   if not GISdb[DB].MyData.FieldExists('CRITERION') then begin
      RankDEMS(DB);
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
      Win := GISdb[DB].MyData.GetFieldByNameAsString('COP_ALOS');
      BoxScore(0,TheIndex,Win);
      Win := GISdb[DB].MyData.GetFieldByNameAsString('COP_FABDEM');
      BoxScore(3,TheIndex,Win);
      Win := GISdb[DB].MyData.GetFieldByNameAsString('COP_TANDEM');
      BoxScore(6,TheIndex,Win);
      GISdb[DB].MyData.Next;
   end;
   for I := 1 to 9 do begin
      aLine := Outcomes[i];
      for j := 0 to Criteria.Count do begin
         //if j <> 0 then aline := aline + ',';
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


