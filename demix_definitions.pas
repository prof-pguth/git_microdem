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
   {$Define Record3DEPX}
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
   //DEMIXSkipFilesAlreadyDone = true;
   //FilterOutSeaLevel = false;
   JustTileStats : boolean = false;
   MaskForDiluvium = false;

const
   Ref1SecPointStr = '_ref_1sec_point';
   Ref1SecAreaStr =  '_ref_1sec_area';
   Ref1_5SecPointStr = '1.5x1sec_point';
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

   NumTileCharacters = 8;
   TileCharacters : array[1..NumTileCharacters] of shortstring = ('AVG_ELEV','AVG_ROUGH','AVG_SLOPE','BARREN_PC','FOREST_PC','RELIEF','URBAN_PC','WATER_PC');

   NCrits = 12;
   Crits : array[1..NCrits] of shortstring = ('ELEV_SSIM','RRI_SSIM','SLOPE_SSIM','HILL_SSIM','RUFF_SSIM','TPI_SSIM','ELEV_FUV','SLOPE_FUV','HILL_FUV','RUFF_FUV','TPI_FUV','RRI_FUV');

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
   MaxPossGrids = 8;
   NumPt : integer = 6;
   NumArea : integer = 1;
   PossPt = 6;
   PossArea = 2;
   PointNames : array[0..PossPt] of shortstring = ('REF_POINT','COP','TANDEM','FABDEM','NASA','SRTM','ASTER');
   AreaNames : array[0..PossArea] of shortstring = ('REF_AREA','ALOS','DILUV');
type
   tDEM_int_array = array [0..MaxPossGrids] of integer;
var
   PointDEMs : tDEM_int_array; //0 is the reference, rest the test DEMs
   AreaDEMs : tDEM_int_array;  //0 is the reference, rest the test DEMs


const
   yasName = 0;
   yasSlope = 1;
   yasRuff = 2;
   yasRelief = 3;
   yasBestEval = 4;
   xawEvaluation = 0;
   xawScore = 1;
   yawArea = 0;
   yawTile = 1;
   DEMIX_combined_graph : boolean = true;
   PanelsByTestDEM : boolean = true;
   MovieByTestDEM : boolean = true;
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

   SSIMresultsDir,  //used internally, and set to one of the next two depending on which version is on
   DiluvSSIMresultsDir, RegularSSIMresultsDir,

   DEMIX_Ref_1sec,DEMIX_test_dems,DEMIX_diluvium_dems,  //locations for the 1" DEMs used in comparison

   DEMIX_Ref_Half_sec,
   DEMIX_Base_DB_Path,DEMIX_profile_test_dir,

  //directories for the channel criterion calculations
   DEMIX_test_DEMs_no_sink, DEMIX_ref_DEMs_no_sink,
   DEMIX_test_DEMs_channels, DEMIX_ref_DEMs_channels,
   DEMIX_test_DEMs_channel_grids, DEMIX_ref_DEMs_channel_grids,
   ChannelMissesDir,DEMIX_diff_dist,DEMIX_area_lc100,


   DEMIX_area_dbName,
   DEMIX_Ref_Source,DEMIX_Ref_Merge,
   DEMIX_GIS_dbName,

   AreaListFName,

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
   procedure DEMIX_SSIM_FUV_transpose_kmeans_new_db(DBonTable : integer);
   procedure ComputeDEMIX_tile_stats(Overwrite : boolean);
   procedure CreateDEMIX_GIS_database;
   procedure RankDEMS(DBonTable : integer);
   procedure SumsOfRankDEMS(DBonTable : integer);
   procedure AddFilteredRankID(DBonTable : integer);
   procedure ModeOfDifferenceDistributions;
   procedure AddTileCharacteristics(DBonTable : integer);
   //procedure SwitchSSIMorFUVScoring(DBonTable : integer);
   procedure EvaluationRangeForCriterion(DBonTable : integer);


//clusters function
   procedure TileCharateristicsWhiskerPlotsByCluster(DBonTable : integer);
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
   procedure ComputeDEMIX_Summary_stats_DB;

procedure AddStatisticsToDEMIXdb(db : integer);

procedure ClearDoubleProcessed;


procedure MaskWaterInReferenceDEMs;
procedure TrimReferenceDEMsToDEMIXtiles;

function AreDEMIXscoresInDB(db : integer) : boolean;
procedure ComputeAverageScoresForSelectedCriteria(db : integer; CriteriaList : tStringList; var Scores : tDEMIXfloats; var NumTies : integer; var WinnerString : shortstring);

procedure CopHeadToHead(db : integer);
procedure CriteriaInSSIM_FUV_db(db : integer);

implementation

uses
   Nevadia_Main,
   DEMstat,Make_grid,PetImage,PetImage_form,new_petmar_movie,DEMdatabase,PetDButils,Pick_several_dems,
   Geotiff, BaseMap, GDAL_tools, DEMIX_filter, DEMstringgrid,DEM_NLCD,
   DEMCoord,DEMMapf,DEMDef_routines,DEM_Manager,DEM_indexes,PetMath,
   DEMIX_control,DEMIX_graphs;


{$include demix_create_database.inc}

{$include demix_clusters.inc}

{$include demix_create_ref_dems.inc}

{$include demix_create_test_dems.inc}

{$include demix_inventory_check_dems.inc}



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
   GISdb[DB].EmpSource.Enabled := false;
   Criteria := GISdb[DB].MyData.UniqueEntriesInDB('CRITERION');
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


procedure AddStatisticsToDEMIXdb(db : integer);
var
   StatsFName : PathStr;
   TheFields : tStringList;
begin
   if GISdb[db].MyData.FieldExists('AVG_SLOPE') and GISdb[db].MyData.FieldExists('AVG_ROUGH') and GISdb[db].MyData.FieldExists('RELIEF') then begin
      if not AnswerIsYes('Overwrite') then exit;
   end;
   if GISdb[db].MyData.FieldExists('AREA') then begin
      TheFields := tStringList.Create;
      TheFields.Add('AVG_SLOPE');
      TheFields.Add('AVG_ROUGH');
      TheFields.Add('RELIEF');
      if GISdb[db].MyData.FieldExists('DEMIX_TILE') then begin
         GISdb[DB].dbOpts.LinkTableName :=  DEMIXSettingsDir + 'tile_statistics.dbf';
         GISdb[DB].dbOpts.LinkFieldThisDB := 'DEMIX_TILE';
      end
      else begin
         GISdb[DB].dbOpts.LinkTableName :=  DEMIXSettingsDir + 'area_statistics.dbf';
      end;
      GISdb[DB].dbOpts.LinkFieldOtherDB := GISdb[DB].dbOpts.LinkTableName;
      GISdb[DB].dbOpts.LinkFieldThisDB := 'AREA';
      GISdb[DB].LinkTable := tMyData.Create(GISdb[DB].dbOpts.LinkTableName);
      GISdb[DB].FillFieldsFromJoinedTable(TheFields,false);
      TheFields.Destroy;
   end;
end;


function AreDEMIXscoresInDB(db : integer) : boolean;
var
   i : integer;
begin
   Result := true;
   for I := 1 to NumDEMIXDEM do begin
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
   for i := 1 to NumDEMIXDEM do Scores[i] := 0;
   Opinions := 0;
   while not GISdb[DB].MyData.eof do begin
      Criterion := GISdb[DB].MyData.GetFieldByNameAsString('CRITERION');
      if (CriteriaList.IndexOf(Criterion) <> -1) then begin
         inc(Opinions);
         for i := 1 to NumDEMIXDEM do begin
            Scores[i] := Scores[i] + GISdb[DB].MyData.GetFieldByNameAsFloat(DEMIXShort[i] + '_SCR');
         end;
      end;
      GISdb[DB].MyData.Next;
   end;
   for I := 1 to NumDEMIXDEM do Scores[i] := Scores[i] / Opinions;
   LowScore := 999;
   WinnerString := '';
   for I := 1 to NumDEMIXDEM do begin
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


