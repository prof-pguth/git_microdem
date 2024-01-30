unit demix_control;

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
   {$Define RecordMaskDiluvium}
   //{$Define Rec_DEMIX_Landcover}
   //{$Define RecordDEMIXStart}
   //{$Define RecordDEMIXsave}
   //{$Define RecordCreateHalfSec}
   //{$Define RecordHalfSec}
   //{$Define RecordTileStats}
   //{$Define Record3DEPXAlreadyDone}
   //{$Define RecordDEMIX_colors}
   //{$Define RecordTileProcessing}
   {$Define Record3DEPX}
   //{$Define Record3DEPXFull}
   //{$Define RecordDEMIXNames}

   //{$Define RecordCriteriaEvaluation}
   //{$Define RecordDEMIXSortGraph}
   //{$Define RecordGridCompare}
   //{$Define RecordDEMIXRefDEMopen}
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
   JustTileStats = false;
   MaskForDiluvium = false;



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

   NumTileCharacters = 7;
   TileCharacters : array[1..NumTileCharacters] of shortstring = ('AVG_ELEV','AVG_ROUGH','AVG_SLOPE','BARREN_PC','FOREST_PC','RELIEF','URBAN_PC');
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

var
   DEMIXDEMTypeName : array[1..MaxDEMIXDEM] of shortstring;   // = ('FABDEM','COP','ALOS','NASA','SRTM','ASTER');
   DEMIXshort : array[1..MaxDEMIXDEM] of shortstring;         // = ('FAB','COP','ALOS','NASA','SRTM','ASTER');
   DEMIXDEMcolors : array[1..MaxDEMIXDEM] of tPlatformColor;

//service functions and procedures
   function LoadDEMIXReferenceDEMs(AreaName : shortstring; var RefDEM : integer; OpenMaps : boolean = true) : boolean;
   function LoadDEMIXCandidateDEMs(AreaName : ShortString; aRefDEM : integer; OpenMaps : boolean = false; AllCandidates : boolean = true) : boolean;
   procedure LoadThisDEMIXTile(AreaName,TileName : shortstring; OpenMaps : boolean = true);

   procedure GetReferenceDEMsForTestDEM(TestSeries : shortstring; var UseDSM,UseDTM : integer);
   function GetReferenceDEMforTestDEM(ThisTestDEM : integer; RefDEMs : tDEMIXindexes) : integer;
   function CriterionTieTolerance(Criterion : shortstring) : float32;
   procedure GetFilterAndHeader(i,j : integer; var aHeader,aFilter : shortString);
   function DEMIX_AreasWanted(CanEditFile : boolean; AreaName : shortstring = '') : tStringList;

   function GetAreaNameForDEMIXTile(DB : integer; DemixTile : shortstring) : shortstring;

   procedure FilterInSignedCriteria(DBonTable : integer);
   function CreateFilterOutSignedCriteria(DBonTable : integer) : shortstring;
   procedure FilterOutSignedCriteria(DBonTable : integer);

   function AreaNameFromRefDEMName(fName : PathStr) : shortstring;

   function SymbolFromDEMName(DEMName : shortstring) : tFullSymbolDeclaration;
   function DEMIXColorFromDEMName(DEMName : shortstring) : tPlatformColor;
   function WhatTestDEMisThis(fName : PathStr) : shortstring;
   function IsDEMaDSMorDTM(DEMName : ShortString) : integer;
   function DEMIXTestDEMLegend(Horizontal : boolean = true) : tMyBitmap;

   procedure OpenDEMIXDatabaseForAnalysis;
   function GetDEMIXpaths(StartProcessing : boolean = true) : boolean;
   procedure EndDEMIXProcessing;
   procedure LoadDEMIXnames;

   procedure AddCountryToDB(DB : integer);
   function MakeHistogramOfDifferenceDistribution(Tile,param,Ref : shortstring) : tThisBaseGraph;
   procedure SummarizeEGM96toEGM2008shifts;
   procedure SetDirtAirballBackground(var Result : tThisBaseGraph; DEMType : shortstring);   //brown dirtball for STM, blue airball for DSM

//3DEP reference DEM processing pipeline
   procedure BatchGDAL_3DEP_shift(DataDirs : tStringList = Nil);
   procedure DEMIX_Create3DEPReferenceDEMs(DataDir : PathStr = '');
   procedure DEMIX_Merge3DEPReferenceDEMs(DataDir : PathStr = '');

//Other reference DEM processing pipeline
   procedure DEMIX_merge_source(Areas : tStringList = Nil);
   procedure DEMIX_CreateReferenceDEMs(ResampleMode : byte; Areas : tStringList = Nil);

//processing steps to create DEMIX data base
   procedure ComputeDEMIX_tile_stats(AreaName : shortstring = '');
   procedure CreateDEMIX_GIS_database(AreaName : shortstring = '');

//other processing steps
   procedure SequentialProcessAnArea;
   procedure DEMIXCreateHalfSecRefDEMs(AreaName : shortstring = '');
   procedure DEMIX_merge_Visioterra_source(AreaName : shortstring = '');

//DEMIX graphs
   procedure DEMIX_evaluations_graph(DBonTable : integer);
   procedure SlopeRoughnessWhiskerPlots(DBonTable : integer);
   function DEMIX_SSIM_R2_clusters_graph(DBonTable : integer) : tThisBaseGraph;
   procedure DEMIX_SSIM_R2_clusters_diversity_graphs(DBonTable : integer; ColorByDEM : boolean = true);
   function DEMIX_SSIM_R2_cluster_sensitivity_graph(DBonTable : integer) : tThisBaseGraph;
   function DEMIX_SSIM_R2_single_tile_graph(DBonTable : integer; tile : shortstring) :tThisBaseGraph;


//DEMIX SSIM/R2 database operations
   procedure DEMIX_SSIM_R2_transpose_kmeans_new_db(DBonTable : integer);
   procedure SwitchSSIMorR2Scoring(DBonTable : integer);
   procedure DEMIX_clusters_per_tile(DBonTable : integer);
   procedure VerifyAllMapsReadyForSSIM;


procedure MergeDEMIXtileStats;
procedure InventoryDEMIXdifferenceStats;



procedure AddTileCharacteristics(DBonTable : integer);
procedure EvaluationRangeForCriterion(DBonTable : integer);

procedure CompareRankings(DBonTable : integer);
procedure DifferentRankingsByCriteria(DBonTable : integer);


   procedure ModeOfDifferenceDistributions;

//DEMIX wine contest procedures based on database
   function DEMIXwineContestScoresGraph(DBonTable : integer; XScalelabel : shortstring; MinHoriz : float32 = 0.5; MaxHoriz : float32 = 5.5) : tThisBaseGraph;
   procedure WinsAndTies(DBonTable : integer);
   procedure DEMIX_graph_best_in_Tile(DBonTable : integer; SortByArea : boolean);
   procedure DEMIXisCOPorALOSbetter(DBonTable : integer);
   procedure BestDEMSbyCategory(DBonTable : integer);
   procedure ModeSTDPlot(DBonTable : integer);
   procedure DEMIXMeanMedianModeHistograms(db : integer);

   procedure MultipleBestByParametersSortByValue(DBonTable,Option : integer; var DEMsTypeUsing,TilesUsing,LandTypesUsing,CandidateDEMsUsing,CriteriaUsing,TileParameters : tStringList; ByPointFilters : boolean = false);


//DEMIX wine contest procedures moved from DEMdatabase 9/14/2023
   procedure RankDEMS(DBonTable : integer; UseAll : boolean = false);
   procedure SumsOfRankDEMS(DBonTable : integer);
   procedure DEMIXwineContestCriterionGraph(What,DBonTable : integer; AreaList : tStringList = nil; CriteriaUsed : tStringList = nil; LandTypePresent : tStringList = nil; DEMsPresent : tStringList = nil);
   procedure DEMIXTileSummary(DBonTable : integer);
   procedure DEMIXtile_inventory(DBonTable : integer);
   procedure DEMIXMeanMedianHistograms(db : integer);


procedure DEMIX_COP_clusters_tile_stats(DBonTable : integer);

procedure DiluviumDEMforTestAreas;

procedure MakeDBForParamStats(Option,DBonTable : integer);

procedure ResampleForDEMIXOneSecDEMs(DEM : integer; OpenMap : boolean = false; OutPath : PathStr = ''; ResampleMode : byte = 1);

procedure ReinterpolateTestDEMtoHalfSec(var DEM : integer; OpenMap : boolean);

function DEMsinIndex(Index : tDEMIXindexes) : integer;

procedure CheckReferenceDEMs;
procedure CheckTestDEMs;
function CreateTestDEMsForCurrentArea(AreaName: ShortString) : boolean;
procedure MaskAllDEMsToDiluvium;


    {$IfDef Old3DEP}
      procedure SubsetLargeUS3DEParea(DEM : integer = 0);
      procedure BatchSubset_3DEP_DEMs;
      procedure DEMIX_VDatum_shifts;
      procedure SummarizeVDatumShifts;
   {$EndIf}

   {$IfDef AllowEDTM}
      procedure ExtractEDTMforTestAreas;
   {$EndIf}

   {$IfDef OpenDEMIXAreaAndCompare}
      procedure OpenDEMIXArea(fName : PathStr = '');
   {$EndIf}

   {$IfDef OldDEMIXroutines}
      procedure TransposeDEMIXwinecontestGraph(DBonTable : integer);
   {$EndIf}

var
   RefDEMs,TestDEMs,
   UsingRefDEMs,
   RefSlopeMap,RefRuffMap,RefRRI,RefTPI,
   RefHillshade,TestHillshade,TestTPI,
   SlopeMap,TestRuffMap,TestRRI : tDEMIXindexes;

   TestSeries : array[1..MaxDEMIXDEM] of shortstring;
   DEMIX_DB_v2,
   HalfSecRefDTM,HalfSecRefDSM,HalfSecDTM,HalfSecALOS,HalfSecCOP,
   DEMIXRefDEM,RefDTMpoint,RefDTMarea,RefDSMpoint,RefDSMarea, COPRefDTM, COPRefDSM : integer;

   DEMIX_Ref_1sec,DEMIX_test_dems,DEMIX_Ref_Half_sec,
   DEMIX_Base_DB_Path,DEMIX_profile_test_dir,
   DEMIX_test_DEMs_channels, DEMIX_ref_DEMs_channels,
   DEMIX_test_DEMs_channel_grids, DEMIX_ref_DEMs_channel_grids,
   DEMIX_Ref_Source,DEMIX_Ref_Merge,
   DEMIX_GIS_dbName_v3,
   ChannelMissesDir,
   SSIMresultsDir,
   DEMIX_test_DEMs_no_sink, DEMIX_ref_DEMs_no_sink,
   DEMIX_distrib_graph_dir,DEMIX_diff_maps_dir,DEMIX_3DEP_Dir,

   GeodeticFName, IceSatFName, LandCoverFName,
   LocalDatumAddFName,LocalDatumSubFName,RefDSMPointFName,RefDSMareaFName,RefDTMPointFName,RefDTMareaFName, COPRefDTMFName,COPRefDSMFName : PathStr;

   {$IfDef DEMIX_DB1}
      DEMIX_DB_v1 : integer;
      DEMIX_GIS_dbName_v1 : PathStr;
   {$EndIf}

implementation

uses
   Nevadia_Main,
   DEMstat,Make_grid,PetImage,PetImage_form,new_petmar_movie,DEMdatabase,PetDButils,Pick_several_dems,
   Geotiff, BaseMap, GDAL_tools, DEMIX_filter, DEMstringgrid,DEM_NLCD,
   DEMCoord,DEMMapf,DEMDef_routines,DEM_Manager,DEM_indexes,PetMath;

var
   vd_path,DEMIX_area_dbName_v2,DEMIX_diff_dist,DEMIX_area_lc100 : PathStr;
   DoHorizontalShift,
   ElevDiffHists : boolean;


procedure InventoryDEMIXdifferenceStats;
var
   TheFiles,Results,Areas : tStringList;
   i,j,n : integer;
   fName : PathStr;
begin
   Areas := DEMIX_AreasWanted(true);
   n := Areas.Count;
   TheFiles := Nil;
   FindMatchingFiles(DEMIXresultsDir,'*.csv',TheFiles);
   Results := tStringList.Create;
   //Results.Sorted := true;
   for j := pred(Areas.Count) downto 0 do begin
      for i := 0 to pred(TheFiles.Count) do begin
         fName := UpperCase(ExtractFileNameNoExt(TheFiles.Strings[i]));
         if StrUtils.AnsiContainsText(fName,UpperCase(Areas.Strings[j])) then begin
            Areas.Delete(j);
            break
         end;
      end;
   end;
   if (Results.Count = 0) then MessageToContinue('Difference stats done for all areas, n=' + IntToStr (N))
   else DisplayAndPurgeStringList(Results,'Areas Missing DEMIX difference stats (n=' + IntToStr(Results.Count) + ')');
   TheFiles.Free;
end;


function GetListOfDEMIXtileStats : tStringList;
var
   i : integer;
   FName : PathStr;
begin
   Result := tStringList.Create;
   FindMatchingFiles(DEMIXresultsDir,'*.csv',Result);
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('GetListOfDEMIXtileStats, total csv files=' + IntToStr(Result.Count)); {$EndIf}
   for i := pred(Result.Count) downto 0 do begin
      fName := UpperCase(ExtractFileNameNoExt(Result.Strings[i]));
      if (not StrUtils.AnsiContainsText(fName,'DEMIX_TILES_USED')) or (StrUtils.AnsiContainsText(fName,'SUMMARY')) then begin
         Result.Delete(i);
      end
      else begin
         {$If Defined(RecordDEMIX)} WriteLineToDebugFile(fName); {$EndIf}
      end;
   end;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('GetListOfDEMIXtileStats, desired csv files=' + IntToStr(Result.Count)); {$EndIf}
end;

procedure MergeDEMIXtileStats;
var
   TheFiles,Results,Areas : tStringList;
   i,j,n : integer;
   fName : PathStr;
begin
   Areas := DEMIX_AreasWanted(true);
   n := Areas.Count;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Twmdem.MergeDEMIXtilestats1 in, areas=' + IntToStr(n)); {$EndIf}

   //TheFiles := Nil;
   //FindMatchingFiles(DEMIXresultsDir,'*.csv',TheFiles);
   //Results := tStringList.Create;
   //Results.Sorted := true;

   TheFiles := GetListOfDEMIXtileStats;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Twmdem.MergeDEMIXtilestats1 tile stat files=' + IntToStr(TheFiles.Count)); {$EndIf}
   (*

   for j := pred(Areas.Count) downto 0 do begin
      for i := pred(TheFiles.Count) downto 0 do begin
         fName := UpperCase(ExtractFileNameNoExt(TheFiles.Strings[i]));
         if not StrUtils.AnsiContainsText(fName,'DEMIX_TILES_USED') then begin
            TheFiles.Delete(i);
         end;
      end;
   end;
   *)
   if (TheFiles.Count > 1) then begin
      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Twmdem.MergeDEMIXtilestats1 to merge, areas=' + IntToStr(TheFiles.Count)); {$EndIf}
      fName := DEMIXresultsDir + 'DEMIX_TILES_USED_SUMMARY.csv';
      //fName := StringReplace(fName,'_dtm','',[rfReplaceAll, rfIgnoreCase]);
      MergeCSVFiles(TheFiles,fName);
   end
   else TheFiles.Free;
   Areas.Free;
end;


function AreaNameFromRefDEMName(fName : PathStr) : shortstring;
begin
   fName := UpperCase(ExtractFileName(fName));
   Result := BeforeSpecifiedString(fName,'_REF');
end;

procedure DifferentRankingsByCriteria(DBonTable : integer);
var
   sl : tStringList;
   counts : array[1..4] of tstringlist;
   aline : shortstring;
   n,i,j : integer;
   fName : PathStr;
begin
   GISdb[DBonTable].EmpSource.Enabled := false;
   sl := tStringList.Create;
   aline := 'DEMIX_TILE,ELEVD,SLPD,RUFD,ALL';
   sl.Add(aline);
   StartProgress('Different rankings');
   n := 0;
   GISdb[DBonTable].MyData.First;
   while not GISdb[DBonTable].MyData.eof do begin
       inc(n);
       UpdateProgressBar(n/ GISdb[DBonTable].MyData.FiltRecsInDB);
       for I := 1 to 4 do  begin
          counts[i] := tStringList.Create;
          Counts[i].Sorted := true;
       end;
       for I := 1 to 3 do begin
          for j := 1 to 5 do begin
             Counts[i].Add(GISdb[DBonTable].MyData.GetFieldByNameAsString(DiffParams[i] + ParamSuffixes[j]));
             Counts[4].Add(GISdb[DBonTable].MyData.GetFieldByNameAsString(DiffParams[i] + ParamSuffixes[j]));
          end;
       end;
       aline := GISdb[DBonTable].MyData.GetFieldByNameAsString('DEMIX_TILE');
      for I := 1 to 4 do  begin
         aLine := aLine + ',' + IntToStr(Counts[i].Count) ;
         Counts[i].Free;
      end;
      sl.Add(aline);
      GISdb[DBonTable].MyData.Next;
   end;

   fName := NextFileNumber(MDTempDir,'compare_ranking_','.dbf');
   StringList2CSVtoDB(sl,fName);

   EndProgress;
   GISdb[DBonTable].ShowStatus;

end;


procedure CompareRankings(DBonTable : integer);
   //DiffParams : array[1..3] of shortstring = ('ELVD','SLPD','RUFD');
   //ParamSuffixes : array[1..5] of shortstring = ('_AVD','_STD','_RMSE','_MAE','_LE90');
var
   Tile,Crit,aline,aline2,Scores,Tstr : shortstring;
   i,j,k,N,DEM : integer;
   rfile : file;
   theTiles,sl,sl2,Crits : tStringList;
   fName : PathStr;
   ScoreSheet,ScoreSheet2 : array[0..14] of shortstring;
begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('CompareRankings in'); {$EndIf}
   GetDEMIXpaths;
   GISdb[DBonTable].EmpSource.Enabled := false;
   theTiles := GISdb[DBonTable].MyData.UniqueEntriesInDB('DEMIX_TILE');
   //OutPath := 'c:\temp\ssim_tile_summary\';
   //SafeMakeDir(OutPath);

   Crits := tStringList.Create;
   sl := tStringList.Create;
   sl2 := tStringList.Create;

   aline := 'DEMIX_TILE';
   for I := 1 to 3 do begin
      for j := 1 to 5 do begin
         aline := aline + ',' + DiffParams[i] + ParamSuffixes[j];
         Crits.Add(DiffParams[i] + ParamSuffixes[j]);
      end;
   end;
   sl.Add(Aline);
   sl2.Add(Aline);

   LoadDEMIXnames;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('CompareRankings for tiles=' + IntToStr(TheTiles.Count)); {$EndIf}
   StartProgress('Compare rankings');
   for k := 0 to pred(theTiles.Count) do begin
      if (k mod 10 = 0) then wmdem.SetPanelText(1,IntToStr(k) + '/' + IntToStr(TheTiles.Count));
      Tile := theTiles[k];
      GISdb[DBonTable].ApplyGISFilter('DEMIX_TILE=' + QuotedStr(Tile) + ' AND REF_TYPE=' + QuotedStr('DTM'));
      GISdb[DBonTable].EmpSource.Enabled := false;
      aline := Tile;
      aline2 := Tile;

       while not GISdb[DBonTable].MyData.eof do begin
          Crit := UpperCase(GISdb[DBonTable].MyData.GetFieldByNameAsString('CRITERION'));
          n := Crits.IndexOf(Crit);
          if (n >= 0) then begin
            Scores := '';
            for DEM := 1 to NumDEMIXDEM do begin
                Scores := Scores + GISdb[DBonTable].MyData.GetFieldByNameAsString(DEMIXShort[DEM] + '_SCR') + '-';
             end;
             ScoreSheet[n] := Scores;
            TStr := GISdb[DBonTable].MyData.GetFieldByNameAsString('DEM_LOW_SC');
            Petmar.ReplaceCharacter(Tstr,',','-');
            ScoreSheet2[n] := tstr;
          end;
          GISdb[DBonTable].MyData.Next;
       end;
       for I := 0 to 14 do aline := aline + ',' + ScoreSheet[i];
       for I := 0 to 14 do aline2 := aline2 + ',' + ScoreSheet2[i];
       sl.Add(Aline);
       sl2.Add(Aline2);
   end;

   fName := NextFileNumber(MDTempDir,'compare_ranking_','.dbf');
   StringList2CSVtoDB(sl,fName);
   fName := NextFileNumber(MDTempDir,'compare_winners_','.dbf');
   StringList2CSVtoDB(sl2,fName);

   EndProgress;
   GISdb[DBonTable].ClearGISFilter;
   GISdb[DBonTable].ShowStatus;
   EndDEMIXProcessing;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('CompareRankings out'); {$EndIf}
end;



procedure MaskAllDEMsToDiluvium;
var
   j,DD : integer;
   NumPts : int64;
   Limits : tGridLimits;
   fName : Pathstr;
begin
   {$IfDef RecordMaskDiluvium} WriteLineToDebugFile('MaskAllDEMsToDiluvium in'); {$EndIf}
   for j := 1 to NumDEMIXDEM do begin
      if (DEMGlb[TestDEMs[j]].AreaName = 'DILUV') then begin
         DD := TestDEMs[j];
         //mark sea leval missing; high ground already missing
         DEMGlb[DD].MarkInRangeMissing(-0.01,0.01,NumPts);
      end;
   end;
   for j := 1 to MaxDEMDataSets do begin
      if ValidDEM(j) and (j <> DD) then begin
         //mark missing everything not valid land in DiluviumDEM
         MaskStripFromSecondGrid(j,TestDEMs[7], msSecondMissing);
         DEMGlb[j].CheckMaxMinElev;
      end;
   end;
   for j := 1 to MaxDEMDataSets do begin
      if ValidDEM(j) then begin
         //trim DEMs so they don't waste time for computations
         {$IfDef RecordMaskDiluvium} WriteLineToDebugFile(DEMGlb[j].AreaName + ' original size ' + DEMGlb[j].ColsRowsString); {$EndIf}
         Limits := DEMGlb[j].FullDEMGridLimits;
         DEMGlb[j].FilledGridBox(Limits);
         fname := NextFileNumber(MDtempDir,DEMGlb[j].AreaName + '_trim_','.dem');
         DEMGlb[j].WriteNewFormatDEM(Limits,fName);
         DEMGlb[j].DEMFileName := fName;
         DEMGlb[j].ReloadDEM(true);
         if (DEMGlb[j].SelectionMap <> nil) then DEMGlb[j].SelectionMap.DoBaseMapRedraw;
         {$IfDef RecordMaskDiluvium} WriteLineToDebugFile('New size ' + DEMGlb[j].ColsRowsString); {$EndIf}
      end;
   end;
   {$IfDef RecordMaskDiluvium}
      for j := 1 to MaxDEMDataSets do begin
         if ValidDEM(j) then begin
            WriteLineToDebugFile(DEMGlb[j].AreaName + '  ' + DEMGlb[j].ZRange);
         end;
      end;
      WriteLineToDebugFile('MaskAllDEMsToDiluvium out');
   {$EndIf}
end;

procedure ExtractDEMIXDEMName(var fName : PathStr);
begin
   fName := UpperCase(fName);
   if (StrUtils.AnsiContainsText(fName,'TIE')) then fName := 'TIE';
   if (StrUtils.AnsiContainsText(fName,'ALOS')) then fName := 'ALOS';
   if (StrUtils.AnsiContainsText(fName,'COP')) then fName := 'COP';
   if (StrUtils.AnsiContainsText(fName,'ASTER')) then fName := 'ASTER';
   if (StrUtils.AnsiContainsText(fName,'FABDEM')) then fName := 'FABDEM';
   if (StrUtils.AnsiContainsText(fName,'NASA')) then fName := 'NASA';
   if (StrUtils.AnsiContainsText(fName,'SRTM')) then fName := 'SRTM';
   if (StrUtils.AnsiContainsText(fName,'EDTM')) then fName := 'EDTM';
   if (StrUtils.AnsiContainsText(fName,'DILUV')) then fName := 'DILUV';
end;

function DEMIXColorFromDEMName(DEMName : shortstring) : tPlatformColor;
var
   i : integer;
begin
   ExtractDEMIXDEMName(DEMName);
   Result := RGBtrip(185,185,185);
   if DEMName = 'TIE' then Result := claBrown
   else begin
      for I := 1 to NumDEMIXDEM do
         if (Uppercase(DEMIXShort[i]) = DEMName) then Result := DEMIXDEMcolors[i];
   end;
   {$IfDef RecordDEMIX_colors} WriteLineToDebugFile('DEMIX color  ' + DEMName + '  ' + ColorStringFromPlatformColor(Result)); {$EndIf}

(*
   else if DEMName = 'ALOS' then Result := RGBtrip(230,159,0)
   else if DEMName = 'ASTER' then Result := RGBtrip(0,114,178)
   else if DEMName = 'COP' then Result := RGBtrip(86,180,233)
   else if DEMName = 'FABDEM' then Result := RGBtrip(204,121,167)
   else if DEMName = 'NASA' then Result := RGBtrip(0,158,115)
   else if DEMName = 'SRTM' then Result := RGBtrip(213,94,0)
   else if DEMName = 'EDTM' then Result := RGBtrip(115,43,245)
   else if DEMName = 'DILUV' then Result := RGBtrip(0,255,0);
*)
end;


function SymbolFromDEMName(DEMName : shortstring) : tFullSymbolDeclaration;
begin
   Result.Size := 4;
   Result.Color := DEMIXColorFromDEMName(DEMName);
   Result.DrawingSymbol := FilledBox;
end;


function IsDEMaDSMorDTM(DEMName : ShortString) : integer;
//works for the DEMIX naming conventions, where DSM must have that in the file name
begin
   if (StrUtils.AnsiContainsText(UpperCase(DEMName),'_DSM')) then Result := DEMisDSM
   else Result := DEMisDTM;
end;


function CreateTestDEMsForCurrentArea(AreaName: ShortString) : boolean;
var
   LoadResults,TStr : shortstring;
   AllTiles : tStringList;
   i,j,RefDEM : integer;
begin
   Result := true;
   {$IfDef RecordDEMIXLoad} WriteLineToDebugFile('TDemixFilterForm.LoadDEMsForArea in ' + AreaName); {$EndIf}
   LoadResults := '';
   if LoadDEMIXReferenceDEMs(AreaName,RefDEM,false) then begin
      LoadDEMIXCandidateDEMs(AreaName,RefDEMs[1],false,true);
      if (DEMsinIndex(TestDEMs) <> RequiredTestDEMs) then begin
         TStr := 'Did not find 6 test DEMs for  ' + AreaName;
         {$IfDef RecordDEMIX}
            HighlightLineToDebugFile(TStr);
            for i := 1 to MaxDemixDEM do begin
               if ValidDEM(TestDEMs[i]) then WriteLineToDebugFile('Found: ' + DEMGlb[TestDEMs[i]].AreaName);
            end;
         {$EndIf}
         Result := false;
      end;
   end;
   CloseAllDEMs;

   {$IfDef RecordDEMIX} WriteLineToDebugFile('TDemixFilterForm.LoadDEMsForArea out ' + AreaName + '  ' + LoadResults); {$EndIf}
end;


procedure CheckReferenceDEMs;
var
   FilesWanted,Areas,Results : tStringList;
   AreaName : shortstring;
   fName,NewName,JustName : PathStr;
   i,j,DTMs,DSMs : Integer;
begin
   GetDEMIXpaths(false);
   Areas := tStringList.Create;
   Areas.LoadFromFile(DEMIXSettingsDir + 'areas_list.txt');
   Results := tStringList.Create;
   Results.Add('AREA,REF_DTMs,REF_DSMs');
   FilesWanted := tStringList.Create;
   FindMatchingFiles(DEMIX_Ref_1sec,'*.tif',FilesWanted,1);
   for i := 0 to pred(Areas.Count) do begin
      AreaName := Areas.Strings[i];
      DSMs := 0;
      DTMs := 0;
      for j := 0 to pred(FilesWanted.Count) do begin
         fName := FilesWanted.Strings[j];
         if StrUtils.AnsiContainsText(UpperCase(fname),UpperCase(AreaName)) then begin
            JustName := ExtractFileName(fname);

            //when sure all the file names are harmonized, this can be removed
            if (not StrUtils.AnsiContainsText(UpperCase(JustName),'REF')) then begin
               NewName := '';
               if StrUtils.AnsiContainsText(UpperCase(JustName),'1sec') then NewName := StringReplace(JustName, '1sec', 'ref_1sec',[rfIgnoreCase]);
               if StrUtils.AnsiContainsText(UpperCase(JustName),'1.5') then NewName := StringReplace(JustName, '1.5', 'ref_1.5',[rfIgnoreCase]);
               if (NewName <> '') then begin
                  NewName := ExtractFilePath(fName) + NewName;
                  RenameFile(fName,NewName);
                  fName := NewName;
               end;
            end;

            if StrUtils.AnsiContainsText(UpperCase(fname),'DSM') then inc(DSMs)
            else inc(DTMs);
         end;
      end;
      Results.Add(AreaName + ',' + IntToStr(DTMs) + ',' + IntToStr(DSMs));
   end;
   fName := NextFileNumber(MDTempDir,'ref_dem_status_','.dbf');
   PetdbUtils.StringList2CSVtoDB(Results,fName);
end;


procedure CheckTestDEMs;
var
   ProblemAreas,FilesWanted,Areas,Results : tStringList;
   AreaName,Missing,Found : shortstring;
   fName,NewName,JustName : PathStr;
   i,j,k,DEMs : Integer;
begin
   GetDEMIXpaths(false);
   ProblemAreas := tStringList.Create;
   Areas := tStringList.Create;
   Areas.LoadFromFile(DEMIXSettingsDir + 'areas_list.txt');
   Results := tStringList.Create;
   Results.Add('AREA,REF_DEMs,MISSING');
   FilesWanted := tStringList.Create;
   FindMatchingFiles(DEMIX_test_dems,'*.dem',FilesWanted,1);
   for i := 0 to pred(Areas.Count) do begin
      AreaName := Areas.Strings[i];
      DEMs := 0;
      Found := '';
      for j := 0 to pred(FilesWanted.Count) do begin
         fName := FilesWanted.Strings[j];
         if StrUtils.AnsiContainsText(UpperCase(fname),UpperCase(AreaName)) then begin
            inc(DEMs);
            Found := Found + UpperCase(ExtractFileNameNoExt(fName));
         end;
      end;
      Missing := '-';
      if (DEMs <> RequiredTestDEMs) then begin
         for k := 1 to NumDemixDEM do begin
            if not StrUtils.AnsiContainsText(Found,UpperCase(DEMIXDEMTypeName[k])) then Missing := Missing + DEMIXDEMTypeName[k] + '-';
         end;
         ProblemAreas.Add(AreaName);
      end;
      Results.Add(AreaName + ',' + IntToStr(DEMs) + ',' + Missing);
   end;
   fName := NextFileNumber(MDTempDir,'test_dem_status_','.dbf');
   PetdbUtils.StringList2CSVtoDB(Results,fName);
   (*
   if (ProblemAreas.Count > 0) then begin
      if AnswerIsYes('Create test area DEMs for those not complete') then begin
         for I := 0 to pred(ProblemAreas.Count) do begin
            AreaName := ProblemAreas.Strings[i];
            //LoadDEMsForCurrentArea(AreaName,true,false);
         end;
      end;
   end;
   *)
   ProblemAreas.Free;
   FilesWanted.Free;
   Areas.Free;
end;


procedure VerifyAllMapsReadyForSSIM;
const
   NCrits = 6;
   Crits : array[1..NCrits] of shortstring = ('ELEV_','RRI_','SLOPE_','HILL_','RUFF_','TPI_');
   NDEMs = 9;
   DEMs : array[1..NDEMs] of shortstring = ('COP','FABDEM','NASA','SRTM','ASTER','ALOS','DILUV','dtm_ref_area','dtm_ref_point');
var
   Dirs,GoodTiles,BadTiles,TheFiles : tStringList;
   i,j,k : Integer;
   fName,fName2 : PathStr;
   Error,Tile : shortstring;
begin
   Dirs := tStringList.Create;
   GoodTiles := tStringList.Create;
   BadTiles := tStringList.Create;
   fName2 := 'C:\temp\ssim_global_norm\';
   Dirs := GetSubDirsInDirectory(fName2);
   StartProgress('Verify');
   for i := 0 to pred(Dirs.Count) do begin
      UpdateProgressBar(i/Dirs.Count);
      Tile := Dirs[i];
      if (Tile[1] = 'N') or (Tile[1] = 'S') then begin //in case other folders selected
         Error := '';
         TheFiles := Nil;
         FindMatchingFiles(fName2 + Tile,'*.tif',TheFiles);
         for k := 1 to NCrits do begin
            for j := 1 to NDEMs do begin
               fName := Crits[k] + DEMs[j] + '_norm.tif';
               if TheFiles.IndexOf(fName2 + Tile + '\' + fName) < 0 then begin
                  if (Error = '') then Error := 'files=' + IntToStr(TheFiles.Count) + '  ';
                  Error := Error + fName + '  ';
               end;
            end;
         end;
         TheFiles.Free;
         if (Error = '') then GoodTiles.Add(Tile)
         else begin
            BadTiles.Add(Tile + '  ' + Error);
            WriteLineToDebugFile(Tile + '  ' + Error);
         end;
      end;
   end;
   EndProgress;
   Dirs.Free;
   DisplayAndPurgeStringList(GoodTiles,'Good tiles=' + IntToStr(GoodTiles.Count));
   DisplayAndPurgeStringList(BadTiles,'Problem tiles=' + IntToStr(BadTiles.Count));
end;



function GetReferenceDEMforTestDEM(ThisTestDEM : integer; RefDEMs : tDEMIXindexes) : integer;
var
   bb : sfBoundBox;
   j : integer;
begin
   Result := 0;
   if ValidDEM(ThisTestDEM) then begin
      if StrUtils.AnsiContainsText(DEMGlb[ThisTestDEM].AreaName,'ALOS') then begin
          //bb := DEMIXtileBoundingBox(TileName,true);
          for j := 1 to MaxDemixDEM do begin
             if ValidDEM(RefDEMs[j]) then begin
                if ( not StrUtils.AnsiContainsText(DEMGlb[RefDEMs[j]].AreaName,'dsm')) and StrUtils.AnsiContainsText(DEMGlb[RefDEMs[j]].AreaName,'area') then begin
                   Result := RefDEMs[j];
                end;
             end;
          end;
      end
      else begin

   //Need to deal with the high lat DEMs

          //bb := DEMIXtileBoundingBox(TileName);
          for j := 1 to MaxDemixDEM do begin
             if ValidDEM(RefDEMs[j]) then begin
                if (not StrUtils.AnsiContainsText(DEMGlb[RefDEMs[j]].AreaName,'dsm')) and (not StrUtils.AnsiContainsText(DEMGlb[RefDEMs[j]].AreaName,'area')) then begin
                   Result := RefDEMs[j];
                end;
             end;
          end;
      end;
   end;
end;


procedure GetReferenceDEMsForTestDEM(TestSeries : shortstring; var UseDSM,UseDTM : integer);
begin
   TestSeries := UpperCase(TestSeries);
   if StrUtils.AnsiContainsText(TestSeries,'ALOS') or StrUtils.AnsiContainsText(TestSeries,'AW3D')  or StrUtils.AnsiContainsText(TestSeries,'DILUV') then begin
      UseDTM := RefDTMArea;
      UseDSM := RefDSMArea;
   end
   else if StrUtils.AnsiContainsText(TestSeries,'COP') then begin
      if (COPRefDTM <> 0) then UseDTM := COPRefDTM else UseDTM := RefDTMpoint;
      if (COPRefDSM <> 0) then UseDSM := COPRefDSM else UseDSM := RefDSMpoint;
   end
   else begin
      UseDTM := RefDTMPoint;
      UseDSM := RefDSMPoint;
   end;
end;


function GetAreaNameForDEMIXTile(DB : integer; DemixTile : shortstring) : shortstring;
var
   Table : tMyData;
   fName : PathStr;
begin
   Result := '';
   fName := DEMIXSettingsDir + 'demix_areas_tiles.dbf';
   if FileExists(fName) then begin
      Table := tMyData.Create(fName);
      Table.ApplyFilter('DEMIX_TILE=' + QuotedStr(DemixTile));
      Result := Table.GetFieldByNameAsString('AREA');
      Table.Destroy;
   end
   else begin
      MessageToContinue('File missing: ' + fName);
   end;
end;


procedure GetAreaDEMNames(TestAreaName : shortstring);
begin
   RefDTMareaFName  := '';
   COPRefDTMFName := '';
   COPRefDSMFName := '';
   RefDSMpointFName := '';
   RefDSMareaFName := '';

   //unfortunately we have not been totally consistent with the file naming
   RefDTMPointFName := DEMIX_Ref_1sec + TestAreaName + '_ref_1sec_point.tif';
   if not FileExists(RefDTMPointFName) then RefDTMPointFName := DEMIX_Ref_1sec + TestAreaName + '_dtm_ref_1sec_point.tif';
   if not FileExists(RefDTMPointFName) then begin
      RefDTMPointFName := DEMIX_Ref_1sec + TestAreaName + '_ref_1sec_point.dem';
      if not FileExists(RefDTMPointFName) then RefDTMPointFName := DEMIX_Ref_1sec + TestAreaName + '_dtm_ref_1sec_point.dem';
   end;

   //if not FileExists(RefDTMPointFName) then RefDTMPointFName := DEMIX_Ref_1sec + TestAreaName + '_dtm_1sec_point.tif';
   //if not FileExists(RefDTMPointFName) then RefDTMPointFName := DEMIX_Ref_1sec + TestAreaName + '_1sec_point.tif';

   if FileExists(RefDTMPointFName) then begin
      RefDTMareaFName := StringReplace(RefDTMPointFName, 'point', 'area',[rfIgnoreCase]);
      if (not FileExists(RefDTMareaFName)) then RefDTMareaFName := '';
      if (StrUtils.AnsiContainsText(RefDTMPointFName,'dtm')) then begin
         if MDDef.DEMIX_open_ref_DSM then begin
            RefDSMpointFName := StringReplace(RefDTMpointFName, 'dtm', 'dsm',[rfIgnoreCase]);
            RefDSMareaFName := StringReplace(RefDTMareaFName, 'dtm', 'dsm',[rfIgnoreCase]);
            if (not FileExists(RefDSMPointFName)) then RefDSMPointFName := '';
            if (not FileExists(RefDSMareaFName)) then RefDSMareaFName := '';
         end;
         COPRefDTMFName := StringReplace(RefDSMPointFName, '1sec', '1.5x1sec',[rfIgnoreCase]);
         COPRefDSMFName := StringReplace(COPRefDTMFName, 'dtm', 'dsm',[rfIgnoreCase]);
         if (not FileExists(COPRefDTMFName)) then COPRefDTMFName := '';
         if (not FileExists(COPRefDSMFName)) then COPRefDSMFName := '';
      end;
   end
   else begin
      RefDTMPointFName := '';
   end;
end;


procedure LoadThisDEMIXTile(AreaName,TileName : shortstring; OpenMaps : boolean = true);
var
   RefDEM,i : integer;
   bb : sfBoundBox;
begin
   {$If Defined(RecordDEMIXLoad)} WriteLineToDebugFile('LoadThisDEMIXTile in, open DEMs=' + IntToStr(NumDEMDataSetsOpen)); {$EndIf}
   GetDEMIXpaths(false);
   LoadDEMIXReferenceDEMs(AreaName,RefDEM,OpenMaps);
   bb := DEMIXtileBoundingBox(TileName);
   for I := 1 to MaxDEMIXDEM do begin
      if ValidDEM(RefDEMs[i]) then begin
         DEMGlb[RefDEMs[i]].AreaName := StringReplace(DEMGlb[RefDEMs[i]].AreaName, AreaName, TileName,[rfIgnoreCase]);
         DEMGlb[RefDEMs[i]].AreaName := StringReplace(DEMGlb[RefDEMs[i]].AreaName, '_1sec', '',[rfIgnoreCase]);
         if (DEMGlb[RefDEMs[i]].SelectionMap <> Nil) then DEMGlb[RefDEMs[i]].SelectionMap.SubsetAndZoomMapFromGeographicBounds(bb);
      end;
   end;

   {$If Defined(RecordDEMIXLoad)} WriteLineToDebugFile('LoadThisDEMIXTile ref loaded, open DEMs=' + IntToStr(NumDEMDataSetsOpen)); {$EndIf}
   UsingRefDEMs := RefDEMs;
   LoadDEMIXCandidateDEMs(AreaName,RefDEM,true);
   for I := 1 to MaxDEMIXDEM do begin
      if ValidDEM(TestDEMs[i]) then begin
         DEMGlb[TestDEMs[i]].AreaName := StringReplace(DEMGlb[TestDEMs[i]].AreaName, AreaName, TileName,[rfIgnoreCase]);
         if (DEMGlb[TestDEMs[i]].SelectionMap <> Nil) then DEMGlb[TestDEMs[i]].SelectionMap.SubsetAndZoomMapFromGeographicBounds(bb);
      end;
   end;
   ShowDefaultCursor;
   {$If Defined(RecordDEMIXLoad)} WriteLineToDebugFile('LoadThisDEMIXTile out, open DEMs=' + IntToStr(NumDEMDataSetsOpen)); {$EndIf}
end;


procedure AddTileCharacteristics(DBonTable : integer);
var
   theFields : tStringList;
   i : integer;
begin
   if not FileExists(GISdb[DBonTable].dbOpts.LinkTableName) then begin
      GISdb[DBonTable].dbOpts.LinkTableName := DEMIXSettingsDir + 'demix_tiles_characteristics.dbf';
      GISdb[DBonTable].dbOpts.LinkFieldThisDB := 'DEMIX_TILE';
      GISdb[DBonTable].dbOpts.LinkFieldOtherDB := 'DEMIX_TILE';
   end;

   GISDb[DBonTable].ClearGISFilter;
   GISDb[DBonTable].EmpSource.Enabled := false;
   ShowHourglassCursor;
   GISdb[DBonTable].ClearLinkTable(true);
   GISdb[DBonTable].LinkSecondaryTable(GISdb[DBonTable].dbOpts.LinkTableName);
   theFields := tStringList.Create;

   theFields.Add('LAT');
   theFields.Add('LONG');
   for I := 1 to NumTileCharacters do theFields.Add(TileCharacters[i]);
   GISdb[DBonTable].FillFieldsFromJoinedTable(TheFields,true);
   GISDb[DBonTable].ShowStatus;
end;


procedure SwitchSSIMorR2Scoring(DBonTable : integer);
//for 0-1 values with high score wins, reverses so low score wins
var
   i,j : integer;
   Eval : float32;
begin
   GISDb[DBonTable].ClearGISFilter;
   GISDb[DBonTable].EmpSource.Enabled := false;
   StartProgress('Switch scoring');
   j := 0;
   while not GISDb[DBonTable].MyData.eof do begin
      if (j mod 100 = 0) then begin
         UpdateProgressBar(j/GISDb[DBonTable].MyData.FiltRecsInDB);
         GISDb[DBonTable].EmpSource.Enabled := false;
      end;
      inc(j);
      GISDb[DBonTable].MyData.Edit;
      for I := 1 to NumDEMIXDEM do begin
         Eval := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(DEMIXDEMTypeName[i]);
         GISDb[DBonTable].MyData.SetFieldByNameAsFloat(DEMIXDEMTypeName[i],1-Eval);
      end;
      GISDb[DBonTable].MyData.Next;
   end;
   GISDb[DBonTable].ShowStatus;
end;


procedure EvaluationRangeForCriterion(DBonTable : integer);
//adds field with the range between the best and worst evaluations
var
   i : integer;
   Eval,Max,Min : float32;
begin
   GISdb[DBonTable].AddFieldToDataBase(ftFloat,'EVAL_RANGE',12,6);
   GISDb[DBonTable].ClearGISFilter;
   GISDb[DBonTable].EmpSource.Enabled := false;
   ShowHourglassCursor;
   while not GISDb[DBonTable].MyData.eof do begin
      Max := -99e39;
      Min := 99e39;
      for I := 1 to NumDEMIXDEM do begin
         Eval := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(DEMIXDEMTypeName[i]);
         PetMath.CompareValueToExtremes(Eval,Min,Max);
      end;
      GISDb[DBonTable].MyData.Edit;
      GISDb[DBonTable].MyData.SetFieldByNameAsFloat('EVAL_RANGE',Max-Min);
      GISDb[DBonTable].MyData.Next;
   end;
   GISDb[DBonTable].ShowStatus;
end;


procedure LoadDEMIXnames;
var
   table : tMyData;
   fName : PathStr;
   i : integer;
   aLine : shortstring;
begin
   fName := DEMIXSettingsDir + 'demix_dems.dbf';
   if (NumDEMIXDEM = 0) and FileExists(fName) then begin
      Table := tMyData.Create(fName);
      Table.ApplyFilter('USE=' + QuotedStr('Y'));
      NumDEMIXDEM := Table.FiltRecsInDB;
      RequiredTestDEMs := Table.FiltRecsInDB;
      for I := 1 to NumDEMIXDEM do begin
         DEMIXDEMTypeName[i] := Table.GetFieldByNameAsString('DEM_NAME');
         DEMIXshort[i] := Table.GetFieldByNameAsString('SHORT_NAME');
         DEMIXDEMcolors[i] := Table.PlatformColorFromTable;
         {$IfDef RecordDEMIX_colors} WriteLineToDebugFile('DEMStat ' +DEMIXshort[i] + '  ' + ColorStringFromPlatformColor(DEMIXDEMcolors[i])); {$EndIf}
         Table.Next;
      end;
      {$IfDef RecordDEMIXNames}
          aLine := IntToStr(NumDEMIXDEM);
          for I := 1 to NumDEMIXDEM do aline := aline + '  ' + DEMIXshort[i];
          HighlightLineToDebugFile('LoadDEMIXNames=' + aLine);
      {$EndIf}
      Table.Destroy;
   end;
end;


function DEMsinIndex(Index : tDEMIXindexes) : integer;
var
   i : integer;
begin
   Result := 0;
   for I := 1 to MaxDEMIXDEM do
      if ValidDEM(Index[i]) then
         inc(Result);
end;


function WhatTestDEMisThis(fName : PathStr) : shortstring;
var
   i : integer;
begin
   Result := '';
   if StrUtils.AnsiContainsText(fname,'REF') then exit;
   fName := UpperCase(fName);
   for i := 1 to NumDEMIXDEM do begin
      if StrUtils.AnsiContainsText(fname,UpperCase(DEMIXShort[i])) then begin
         Result := DEMIXShort[i];
         exit;
      end;
   end;
   {$If Defined(RecordWhatDEMisThis)} MessageToContinue('No DEM match in WhatTestDEMisThis for ' + fName); {$EndIf}
end;

procedure FilterInSignedCriteria(DBonTable : integer);
var
   aFilter : ANSIstring;
   i : integer;
begin
   aFilter := '';
   for I := 1 to 3 do begin
      aFilter := AddOrIfNeeded(aFilter) + 'CRITERION=' + QuotedStr(MeanParams[i]);
      aFilter := AddOrIfNeeded(aFilter) + 'CRITERION=' + QuotedStr(MedianParams[i]);
   end;
   GISdb[DBOntable].ApplyGISFilter(aFilter);
   GISdb[DBonTable].ShowStatus;
end;


function CreateFilterOutSignedCriteria(DBonTable : integer) : shortstring;
var
   i : integer;
begin
    Result := '' ;
    for I := 1 to 3 do begin
       Result := AddAndIfNeeded(Result) + 'CRITERION<>' + QuotedStr(MeanParams[i]);
       Result := AddAndIfNeeded(Result) + 'CRITERION<>' + QuotedStr(MedianParams[i]);
    end;
    Result := '(' + Result + ')';
end;


procedure FilterOutSignedCriteria(DBonTable : integer);
begin
    GISdb[DBOntable].ApplyGISFilter(CreateFilterOutSignedCriteria(DBonTable));
    GISdb[DBonTable].ShowStatus;
end;



procedure ReinterpolateTestDEMtoHalfSec(var DEM : integer; OpenMap : boolean);
var
   HalfSec : integer;
   fName : PathStr;
   Spacing : float32;
begin
   if ValidDEM(DEM) then begin
      if (DEMGlb[DEM].DEMheader.DEMUsed = ArcSecDEM) and (abs(DEMGlb[DEM].DEMheader.DEMxSpacing - 0.5) < 0.001) and (abs(DEMGlb[DEM].DEMheader.DEMySpacing - 0.5) < 0.001) then begin
         {$If Defined(RecordHalfSec)} WriteLineToDebugFile(DEMGlb[DEM].AreaName + ' already half sec'); {$EndIf}
      end
      else begin
         fName := MDtempDir + DEMGlb[DEM].AreaName + '_0.5sec.dem';
         Spacing := 0.5;
         HalfSec := DEMGlb[DEM].ReinterpolateLatLongDEM(Spacing,fName);
         CloseSingleDEM(DEM);
         DEM := HalfSec;
         if OpenMap then CreateDEMSelectionMap(DEM,true,true,mtIHSReflect);
         {$If Defined(RecordHalfSec)} WriteLineToDebugFile(DEMGlb[DEM].AreaName + ' reinterpolated'); {$EndIf}
      end;
   end
   else begin
      {$If Defined(RecordHalfSec)} WriteLineToDebugFile('Invalid DEM=' + IntToStr(DEM) + ' in ReinterpolateTestDEMtoHalfSec'); {$EndIf}
   end;
end;


      procedure GetFilterAndHeader(i,j : integer; var aHeader,aFilter : shortString);
      var
         RefFilter : shortstring;
      begin
         RefFilter := ' AND REF_TYPE=' + QuotedStr(RefDEMType[i]) + ' AND LAND_TYPE=' + QuotedStr('ALL');
         case j of
            1 : begin
                  aHeader := RefDEMType[i] + ' ' + 'ALL pixels  Tile avg slope > 18%';
                  aFilter := 'AVG_SLOPE > 18' + RefFilter;
                end;
            2 : begin
                  aHeader := RefDEMType[i] + ' ' + 'ALL pixels  Tile avg slope < 18%';
                  aFilter := 'AVG_SLOPE < 18' + RefFilter;
                end;
            3 : begin
                  aHeader := RefDEMType[i] + ' ' + 'ALL pixels  Tile relief < 25m';
                  aFilter := 'RELIEF < 25' + RefFilter;
                end;
            4 : begin
                  aHeader := RefDEMType[i] + ' ' + 'ALL pixels  Tile relief > 500m';
                  aFilter := 'RELIEF > 500' + RefFilter;
                end;
            5 : begin
                  aHeader := RefDEMType[i] + ' ' + 'ALL pixels  Tile avg rough > 10%';
                  aFilter := 'AVG_ROUGH > 10' + RefFilter;
                end;
            6 : begin
                  aHeader := RefDEMType[i] + ' ' + 'ALL pixels  Tile avg rough < 5%';
                  aFilter := 'AVG_ROUGH < 5' + RefFilter;
                end;
            7 : begin
                  aHeader := RefDEMType[i] + ' ' + 'ALL pixels  Tile urban > 25%';
                  aFilter := 'URBAN_PC > 25' + RefFilter;
                end;
            8 : begin
                  aHeader := RefDEMType[i] + ' ' + 'ALL pixels  Tile forest > 50%';
                  aFilter := 'FOREST_PC > 50' + RefFilter;
                end;
            9 : begin
                  aHeader := RefDEMType[i] + ' ' + 'ALL pixels  Tile barren > 25%';
                  aFilter := 'BARREN_PC > 25' + RefFilter;
                end;
         end;
      end;



function DEMIXTestDEMLegend(Horizontal : boolean = true) : tMyBitmap;
var
   i,Left,Top : integer;
begin
   CreateBitmap(Result,1500,250);
   LoadMyFontIntoWindowsFont(MDDef.LegendFont,Result.Canvas.Font);
   Left := 25;
   Top := 10;
   for i := 1 to NumDEMIXDEM do begin
      Result.Canvas.Pen.Color := ConvertPlatformColorToTColor(DEMIXColorFromDEMName(DEMIXShort[i]));
      Result.Canvas.Brush.Color := Result.Canvas.Pen.Color;
      Result.Canvas.Brush.Style := bsSolid;
      Result.Canvas.Rectangle(Left,Top,Left + 15,Top + 15);
      Result.Canvas.Brush.Style := bsClear;
      Result.Canvas.TextOut(Left + 20,Top,DEMIXShort[i]);
      if Horizontal then Left := Left + 30 + Result.Canvas.TextWidth(DEMIXShort[i])
     else Top := Top + 10 + Result.Canvas.TextHeight(DEMIXShort[i]);
   end;
   PutBitmapInBox(Result);
end;



procedure SetDirtAirballBackground(var Result : tThisBaseGraph; DEMType : shortstring);
begin
    if (DEMtype = 'DSM') then Result.GraphDraw.GraphBackgroundColor := RGB(219,236,237)
    else Result.GraphDraw.GraphBackgroundColor := RGB(237,237,221);
end;


function GetDEMIXpaths(StartProcessing : boolean = true) : boolean;
begin
   {$If Defined(RecordDEMIXStart)} WriteLineToDebugFile('GetDEMIXpaths in'); {$EndIf}
   DEMIX_Base_DB_Path := 'G:\wine_contest\';
   FindDriveWithPath(DEMIX_Base_DB_Path);
   if not PathIsValid(DEMIX_Base_DB_Path) then begin
      MessageToContinue('No valid DEMIX path, ' + DEMIX_Base_DB_Path);
      Result := false;
      exit;
   end;
   Result := true;

   if StartProcessing then begin
      HeavyDutyProcessing := true;
      WMdem.Color := clInactiveCaption;
      DEMIXProcessing := true;
   end;
   StopSplashing;

   //settings that can be changed, but constant here for DB creation
      ElevDiffHists := true;
      DoHorizontalShift := false;

      MDDef.DEMIX_Full := 75;
      MDDef.SlopeFlatBoundary := 12.5;
      MDDef.SlopeGentleBoundary := 25;
      MDDef.SlopeSteepBoundary := 50;
      MDDef.LandTypePointsNeeded := 100;
      MDDef.RoughnessBox := 5;
      MDDef.AutoMergeStartDEM := true;
      MDdef.DefaultMapXSize := 800;
      MDdef.DefaultMapYSize := 800;
      MDDef.TitleLabelFont.Size := 24;
      MDDef.LegendFont.Size := 20;
   {$If Defined(RecordDEMIXStart)} WriteLineToDebugFile('GetDEMIXpaths point 2'); {$EndIf}

   DEMIX_Ref_Source := DEMIX_Base_DB_Path + 'wine_contest_v2_ref_source\';
   DEMIX_Ref_Merge := DEMIX_Base_DB_Path + 'wine_contest_v2_ref_merge\';
   DEMIX_Ref_Half_sec := DEMIX_Base_DB_Path + 'wine_contest_v2_ref_0.5sec\';
   vd_path := DEMIX_Base_DB_Path + 'wine_contest_v2_vdatum\';
   DEMIXresultsDir := DEMIX_Base_DB_Path + 'wine_contest_v2_tile_stats\';
   DEMIX_diff_dist  := DEMIX_Base_DB_Path + 'wine_contest_v2_diff_dist\';
   DEMIX_diff_maps_dir  := DEMIX_Base_DB_Path + 'wine_contest_difference_maps\';
   DEMIXSettingsDir := ProgramRootDir + 'demix\';
   DEMIX_area_lc100  := DEMIX_Base_DB_Path + 'wine_contest_v2_lc100\';
   DEMIX_profile_test_dir := DEMIX_Base_DB_Path + 'wine_contest_v2_topo_profiles\';
   DEMIX_distrib_graph_dir := DEMIX_Base_DB_Path + 'wine_contest_v2_difference_distrib_graphs\';
   DEMIX_3DEP_Dir := DEMIX_Base_DB_Path + 'wine_contest_v2_3dep\';
   DEMIX_area_dbName_v2 := DEMIXSettingsDir + 'demix_test_areas_v3.dbf';
   DEMIX_GIS_dbName_v3 := DEMIX_Base_DB_Path + 'wine_contest_database\demix_gis_db_v2.5.dbf';

   DEMIX_Ref_1sec := DEMIX_Base_DB_Path + 'wine_contest_v2_ref_1sec\';
   DEMIX_test_dems := DEMIX_Base_DB_Path + 'wine_contest_v2_test_dems\';

   DEMIX_test_DEMs_no_sink := DEMIX_Base_DB_Path + 'area_test_dems_no_sink\';
   DEMIX_ref_DEMs_no_sink := DEMIX_Base_DB_Path + 'area_ref_dems_no_sink\';
   DEMIX_test_DEMs_channels := DEMIX_Base_DB_Path + 'area_test_dems_channels\';
   DEMIX_ref_DEMs_channels := DEMIX_Base_DB_Path + 'area_ref_dems_channels\';
   DEMIX_test_DEMs_channel_grids := DEMIX_Base_DB_Path + 'area_test_dems_channel_grids\';
   DEMIX_ref_DEMs_channel_grids := DEMIX_Base_DB_Path + 'area_ref_dems_channel_grids\';
   ChannelMissesDir := DEMIX_Base_DB_Path + 'channel_misses\';
   SSIMresultsDir := DEMIX_Base_DB_Path + 'SSIM_results\';

   //DEMIX_area_dbName_v2 := DEMIX_Base_DB_Path + 'wine_contest_v2_ref_source\demix_area_vert_datums.dbf';
   //DEMIXSettingsDir := DEMIX_Base_DB_Path + 'wine_contest_settings\';
   //DEMIX_Ref_1sec_v1 := DEMIX_Base_DB_Path + 'demix_reference_dems_v1\';
   //DEMIX_GIS_dbName_v1 := DEMIX_Base_DB_Path + 'wine_contest_database\demix_database_v1.dbf';
   {$If Defined(RecordDEMIXStart)} WriteLineToDebugFile('GetDEMIXpaths point 4'); {$EndIf}

   Geoid2008FName := 'g:\geoid\egm2008-1-vdatum.tif';
   FindDriveWithFile(Geoid2008FName);
   GeoidDiffFName := 'g:\geoid\egm96_to_egm2008.tif';
   FindDriveWithFile(GeoidDiffFName);

   LoadDEMIXnames;
   {$If Defined(RecordDEMIXStart)} WriteLineToDebugFile('GetDEMIXpaths out'); {$EndIf}
end;


procedure EndDEMIXProcessing;
begin
   HeavyDutyProcessing := false;
   DEMIXProcessing := false;
   WMdem.Color := clScrollBar;
   wmdem.ClearStatusBarPanelText;
   ShowDefaultCursor;
end;

(*
function DEMIXLoadRefDEMFromPath(AreaName : shortstring; LoadMap : boolean) : integer;
var
   FilesWanted : tStringList;
   j : integer;
   fName : PathStr;
begin
   FilesWanted := tStringList.Create;
   FindMatchingFiles(DEMIX_Ref_1sec,'*.tif',FilesWanted,1);
   for j := 0 to pred(FilesWanted.Count) do begin
      fName := FilesWanted.Strings[j];
      if StrUtils.AnsiContainsText(UpperCase(fname),UpperCase(AreaName)) then begin
         Result := OpenNewDEM(fName,LoadMap);
         exit;
      end;
   end;
   FilesWanted.Free;
end;


function ShortTestAreaName(TestAreaName : shortstring) : shortstring;
begin
   Result := StringReplace(TestAreaName,'_dtm','',[rfReplaceAll, rfIgnoreCase]);
   Result := StringReplace(Result,'_dsm','',[rfReplaceAll, rfIgnoreCase]);
end;
*)


function DEMIX_AreasWanted(CanEditFile : boolean; AreaName : shortstring = '') : tStringList;
var
   fName : PathStr;
   PickedNum : integer;
begin
   Result := tStringList.Create;
   if (AreaName = '') then begin
      fName := DEMIXSettingsDir + 'areas_list.txt';
      if FileExists(fName) or GetExistingFileName('DEMIX areas','*.txt',fName) then begin
         Result.LoadFromFile(fName);
         if CanEditFile then MultiSelectSingleColumnStringList('Areas to process', PickedNum,Result,false,true);
      end;
   end
   else begin
      Result.Add(AreaName);
   end;
end;



procedure AddCountryToDB(DB : integer);
var
   Table : tMyData;
   Area,Country : shortstring;
   i : integer;
begin
   GetDEMIXPaths;

   if FileExists(DEMIX_area_dbName_v2) then begin
     Table := tMyData.Create(DEMIX_area_dbName_v2);
     GISdb[db].AddFieldToDatabase(ftString,'COUNTRY',16);
     i := 0;
     while not Table.eof do begin
        inc(i);
        GISdb[db].EmpSource.Enabled := false;
        Country := Table.GetFieldByNameAsString('COUNTRY');
        Area := Table.GetFieldByNameAsString('AREA');
        wmdem.SetPanelText(2, 'Area: ' + IntToStr(succ(i)) + '/' + IntToStr(Table.FiltRecsInDB) + ' ' + Area);
        if StrUtils.AnsiContainsText(UpperCase(Area),'_DSM') then begin

        end
        else begin
           GISdb[db].ApplyGISFilter('AREA=' + QuotedStr(Area));
           GISdb[db].FillFieldWithValue('COUNTRY',Country);
        end;
        Table.Next;
     end;
     Table.Destroy;
     wmdem.SetPanelText(2,'');
     GISdb[db].ClearGISFilter;
     GISdb[db].ShowStatus;
   end
   else begin
       {$If Defined(RecordDEMIX)} WriteLineToDebugFile('AddCountryToDB fail, missing ' + DEMIX_area_dbName_v2); {$EndIf}
   end;
end;


procedure SummarizeEGM96toEGM2008shifts;
var
   Files,Summary : tStringList;
   i,DEM,Geoid : integer;
   TStr : shortstring;
   fName : PathStr;

   procedure AddPoint(col,row : integer);
   var
      Lat,Long : float64;
      z : float32;
   begin
      DEMglb[DEM].DEMGridToLatLongDegree(Col,Row,Lat,Long);
      DEMGlb[Geoid].GetElevFromLatLongDegree(Lat,Long,z);
      TStr := Tstr + ',' + RealToString(z,-12,-2);
   end;

begin
   GetDEMIXpaths(false);
   Geoid := OpenNewDEM(GeoidDiffFName,false);

   Summary := tstringlist.create;
   Summary.Add('AREA,SHIFT_NW,SHIFT_NE,SHIFT_CENT,SHIFT_SE,SHIFT_SW');
   Files := nil;
   Petmar.FindMatchingFiles(DEMIX_Ref_Half_sec,'*.tif',Files,0);
   StartProgress('EGM96 shift summary');
   for i := 0 to pred(Files.Count) do begin
      UpdateProgressbar(i/Files.Count);
      fName := Files.Strings[i];
      if StrUtils.AnsiContainsText(UpperCase(fname),'_DSM') then begin
      end
      else begin
         DEM := OpenNewDEM(fName,false);
         TStr := ExtractFileNameNoExt(fName);
         AddPoint(DEMglb[DEM].DEMHeader.NumRow,0);
         AddPoint(DEMglb[DEM].DEMHeader.NumRow,DEMglb[DEM].DEMHeader.NumCol);
         AddPoint(DEMglb[DEM].DEMHeader.NumRow div 2,DEMglb[DEM].DEMHeader.NumCol div 2);
         AddPoint(0,0);
         AddPoint(DEMglb[DEM].DEMHeader.NumCol,0);
         Summary.Add(TStr);
         CloseSingleDEM(DEM);
      end;
   end;
   CloseAllDEMs;
   Files.Destroy;
   EndProgress;
   fName := NextFileNumber(MDTempDir,'EGM96_shift_summary','.dbf');
   StringList2CSVtoDB(Summary,fName);
end;



procedure SequentialProcessAnArea;
//still in progress, 20 May 2023
var
   Areas : tStringList;
   i : integer;
   AreaName : shortstring;
begin
(*
   GetDEMIXpaths(true);
   Areas := DEMIX_AreasWanted;
   try
      for i := 0 to pred(Areas.Count) do begin
         AreaName := Areas.Strings[i];
         DEMIX_merge_source(AreaName);
         //DEMIX_VDatum_shifts(Areas.Strings[i]);
         DEMIX_CreateReferenceDEMs(AreaName);
         ComputeDEMIX_tile_stats(AreaName);
         CreateDEMIX_GIS_database(AreaName);
      end;
   finally
      EndDEMIXProcessing;
   end;
*)
end;


procedure OpenDEMIXDatabaseForAnalysis;
begin
   GetDEMIXpaths(false);
   if not FileExists(DEMIX_GIS_dbName_v3) then Petmar.GetExistingFileName('DEMIX db version 3','*.dbf',DEMIX_GIS_dbName_v3);
   {$IfDef DEMIX_DB1} if not FileExists(DEMIX_GIS_dbName_v1) then Petmar.GetExistingFileName('DEMIX db version 1','*.dbf',DEMIX_GIS_dbName_v1); {$EndIf}
   OpenNumberedGISDataBase(DEMIX_DB_v2,DEMIX_GIS_dbName_v3,false);
   GISdb[DEMIX_DB_v2].LayerIsOn := false;
   {$IfDef DEMIX_DB1}
      OpenNumberedGISDataBase(DEMIX_DB_v1,DEMIX_GIS_dbName_v1,false);
      GISdb[DEMIX_DB_v1].LayerIsOn := false;
   {$EndIf}
   DoDEMIXFilter(DEMIX_DB_v2);
end;


function CriterionTieTolerance(Criterion : shortstring) : float32;
var
   TieToleranceTable : tMyData;
begin
   TieToleranceTable := tMyData.Create(MDDef.DEMIX_criterion_tolerance_fName);
   TieToleranceTable.ApplyFilter('CRITERION=' + QuotedStr(Criterion));
   Result := TieToleranceTable.GetFieldByNameAsFloat('TOLERANCE');
   TieToleranceTable.Destroy;
end;


procedure DEMIXisCOPorALOSbetter(DBonTable : integer);
var
   RefFilter : shortstring;
   Compare,i,j,Opinions,db : integer;
   fName : PathStr;
   Findings,Criteria,DEMs : tStringList;


   procedure DoOne(Header,theFilter : shortstring);
   var
      Cop,ALOS,FAB,dem : integer;
      aLine : shortString;
      Counts : array[0..10] of integer;
   begin
      {$If Defined(RecordDEMIXFull)} WriteLineToDebugFile('DO-ONE  ' + theFilter); {$EndIf}
      WMDEM.SetPanelText(1,theFilter);
      GISdb[DBonTable].ApplyGISFilter(theFilter);
      GISdb[DBonTable].EmpSource.Enabled := false;
      Opinions := GISdb[DBonTable].MyData.FiltRecsInDB;
      if (Opinions >= 10) then begin
         if (Compare = 1) then begin
            GISdb[DBonTable].ApplyGISFilter(theFilter + ' AND COP_ALOS=' + QuotedStr('tie'));
            GISdb[DBonTable].ApplyGISFilter(theFilter + ' AND COP_ALOS=' + QuotedStr('alos'));
            ALOS := GISdb[DBonTable].MyData.FiltRecsInDB;
            GISdb[DBonTable].ApplyGISFilter(theFilter + ' AND COP_ALOS=' + QuotedStr('cop'));
            COP := GISdb[DBonTable].MyData.FiltRecsInDB;
            Findings.Add(Header + '  (n=' + IntToStr(Opinions) + '),' + RealToString(100.0 * alos/opinions,-8,-2)+ ','  + RealToString(100.0 * cop/opinions,-8,-2));
         end
         else if (Compare = 2) then begin
            GISdb[DBonTable].ApplyGISFilter(theFilter + ' AND DEM_LOW_SC=' + QuotedStr('alos'));
            ALOS := GISdb[DBonTable].MyData.FiltRecsInDB;
            GISdb[DBonTable].ApplyGISFilter(theFilter + ' AND DEM_LOW_SC=' + QuotedStr('cop'));
            COP := GISdb[DBonTable].MyData.FiltRecsInDB;
            GISdb[DBonTable].ApplyGISFilter(theFilter + ' AND DEM_LOW_SC=' + QuotedStr('fabdem'));
            FAB := GISdb[DBonTable].MyData.FiltRecsInDB;
            Findings.Add(Header + '  (n=' + IntToStr(Opinions) + '),' + RealToString(100.0 * alos/opinions,-8,-2) + ',' + RealToString(100.0 * cop/opinions,-8,-2) + ',' + RealToString(100.0 * fab/opinions,-8,-2));
         end
         else begin
            GISdb[DBonTable].MyData.First;
            for DEM := 0 to 10 do Counts[DEM] := 0;
            while not GISdb[DBonTable].MyData.EOF do begin
               aLine := GISdb[DBonTable].MyData.GetFieldByNameAsString('DEM_LOW_SC');
               for DEM := 0 to pred(DEMs.Count) do
                  if StrUtils.AnsiContainsText(aline,DEMs.Strings[DEM]) then inc(Counts[DEM]);
               GISdb[DBonTable].MyData.Next;
            end;
            aline := Header + '  (n=' + IntToStr(Opinions) + ')';
            for DEM := 0 to pred(DEMs.Count) do aline := aline + ',' + RealToString(100.0 * Counts[DEM]/opinions,-8,-2);
            Findings.Add(aLine);
         end;
      end;
   end;


var
   aHeader,aFilter,TStr : shortstring;
   n : integer;
begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXisCOPorALOSbetter in'); {$EndIf}
   try
      GISdb[DBonTable].ApplyGISFilter('');
      ShowHourglassCursor;
      Criteria := GISdb[DBonTable].MyData.UniqueEntriesInDB('CRITERION');
      DEMs := tStringList.Create;
      DEMs.LoadFromFile(DEMIXSettingsDir + 'demix_dems.txt');

      for Compare := 1 to 3 do begin
         for i := 1 to 2 do begin
            {$If Defined(RecordDEMIX)} HighlightLineToDebugFile('DEMIXisCOPorALOSbetter start ' + RefDEMType[i]); {$EndIf}
            ShowHourglassCursor;
            Findings := tStringList.Create;
            if (Compare = 1) then Findings.Add('FILTER,ALOS,COP')
            else if (Compare = 2) then Findings.Add('FILTER,ALOS,COP,FABDEM')
            else begin
               TStr := 'FILTER';
               for j := 0 to pred(DEMs.Count) do Tstr := Tstr + ',' + DEMs.Strings[j];
               Findings.Add(TStr);
            end;

            RefFilter := ' AND REF_TYPE=' + QuotedStr(RefDEMType[i]);
            for j := 1 to NumLandTypes do begin
               DoOne(RefDEMType[i] + ' ' + LandTypes[j] + ' pixels','LAND_TYPE=' + QuotedStr(LandTypes[j]) + RefFilter);
            end;
            Findings.Add('SKIP');

            if GISdb[DBonTable].MyData.FieldExists('PC_BARREN') then n := 9 else n := 8;
            for j := 1 to n do begin
               GetFilterAndHeader(i,j,aHeader,aFilter);
               DoOne(aHeader,aFilter);
            end;

            Findings.Add('SKIP');
            for j := 0 to pred(Criteria.Count) do begin
               DoOne(RefDEMType[i] + ' ALL pixels  ' + Criteria.Strings[j],'CRITERION=' + QuotedStr(Criteria.Strings[j]) + RefFilter );
            end;
            if Compare = 1 then TStr := '_cop_or_alos_'
            else if Compare = 2 then TStr := '_fab_cop_or_alos_'
            else TStr := '_share_first_';

            fName := NextFileNumber(MDTempDir,RefDEMType[i] + TStr,'.dbf');
            db := StringList2CSVtoDB(Findings,fName);
            if (Compare = 1) then TStr := 'COP or ALOS Winning Percentage'
            else if (Compare = 2) then TStr := 'COP or ALOS or FABDEM Winning Percentage'
            else TStr := 'DEM share of First Place';
            DEMIXwineContestScoresGraph(DB,Tstr + ' (%)',0,100);
         end;
      end;
   finally
      Criteria.Destroy;
      GISdb[DBonTable].ApplyGISFilter('');
      GISdb[DBonTable].ShowStatus;
   end;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXisCOPorALOSbetter out'); {$EndIf}
end;



procedure WinsAndTies(DBonTable : integer);
const
    nDEMs = 7;
    nT = 6;
    TheDEMs : array[1..NDEMs] of shortstring = ('TIES','ALOS_TIE','COP_TIE','FABDEM_TIE','NASA_TIE','SRTM_TIE','ASTER_TIE');
var
   Results : tStringList;
   aFilter : shortstring;


   procedure DoOne(ParameterList : tStringList; FilterField : shortstring);
   var
      Findings : array[1..nDEMs,1..nDEMs] of integer;
      i,j,k,DEM : integer;
   begin
      for DEM := 1 to 2 do begin
         wmdem.SetPanelText(2,RefDEMType[DEM]);
         for k := 1 to ParameterList.Count do begin
            wmdem.SetPanelText(3,ParameterList.Strings[pred(k)]);
            {$IfDef RecordDEMIX} HighlightLineToDebugFile('WinsAndTies, k=' + IntToStr(k)); {$EndIf}

            for i := 1 to nDEMs do begin
               for j := 2 to nDEMs do begin
                  Findings[i,j] := 0;
               end;
            end;

            for i := 1 to nDEMs do begin
               for j := 1 to nT do begin
                  aFilter := TheDEMs[i] + '=' + IntToStr(j)  + ' AND REF_TYPE=' + QuotedStr(RefDEMType[DEM]);
                  if (k > 1) then aFilter :=  aFilter + ' AND ' + FilterField + '=' + QuotedStr(ParameterList.Strings[pred(k)]);
                  GISdb[DBonTable].ApplyGISfilter(aFilter);
                  Findings[i,j] := GISdb[DBonTable].MyData.FiltRecsInDB;
               end;
            end;

            for j := 1 to nT do begin
               aFilter := ParameterList.Strings[pred(k)] + ',' + RefDEMType[DEM] + ',' + IntToStr(j);
               for i := 1 to nDEMs do begin
                  aFilter := aFilter + ',' + {IntToStr(nT) + ',' +} IntToStr(Findings[i,j]);
               end;
               Results.Add(afilter);
            end;
         end;
      end;
   end;

const
   nParams = 4;
   TheLumpedParams : array[1..NParams] of shortstring = ('*','ELVD','SLPD','RUFD');
var
   fName : PathStr;
   theParams : tstringlist;
   j : integer;
begin
   {$IfDef RecordDEMIX} HighlightLineToDebugFile('WinsAndTies in'); {$EndIf}
   try
      if not GISdb[DBonTable].MyData.FieldExists('COP_TIE') then GISdb[DBonTable].dbTablef.iesbyopinions1Click(Nil);
      GetDEMIXPaths(true);

      GISdb[DBonTable].EmpSource.Enabled := false;

      Results := tStringList.Create;
      aFilter := 'PARAMETER,REF_TYPE,NUM_TIES';
      for j := 1 to nDEMs do aFilter :=  aFilter + ',' + TheDEMs[j];
      Results.Add(aFilter);
      theParams := tstringlist.Create;
      for j := 1 to nParams do TheParams.Add(TheLumpedParams[j]);
      DoOne(TheParams,'CRIT_CAT');
      TheParams.Clear;
      TheParams.LoadFromFile(DEMIXSettingsDir + 'criteria_all.txt');
      DoOne(TheParams,'CRITERION');
      TheParams.Destroy;

      {$IfDef RecordDEMIX} HighlightLineToDebugFile('WinsAndTies, make db'); {$EndIf}
      fName := NextFileNumber(MDTempDir,MDTempDir + 'wins_and_ties_','.dbf');
      StringList2CSVtoDB(Results,fName);
   finally
      GISdb[DBonTable].ClearGISfilter;
      GISdb[DBonTable].ShowStatus;
      EndDEMIXProcessing;
   end;
end;


procedure DoDEMIX_DifferenceMaps(AreaName,ShortName,LongName : shortString; var Graph1,Graph2 : tThisBaseGraph);
var
   TestGrid,DSMgrid,DTMGrid,
   i,UseDSM,UseDTM : integer;
   Min,Max,BinSize : float32;
   DSMElevFiles,DSMLegendFiles,DTMElevFiles,DTMLegendFiles : tStringList;


      procedure ModifyGraph(Graph : tThisBaseGraph);
      var
         I : integer;
      begin
         for I := 1 to Graph.GraphDraw.LegendList.Count do begin
            Graph.GraphDraw.FileColors256[i] := DEMIXColorFromDEMName(Graph.GraphDraw.LegendList[pred(i)]);
         end;
         Graph.RedrawDiagram11Click(Nil);
         Graph.Image1.Canvas.Draw(Graph.GraphDraw.LeftMargin+15,Graph.GraphDraw.TopMargin+10,Graph.MakeLegend(Graph.GraphDraw.LegendList,false));
      end;


     procedure MakeDifferenceGrid(RefGrid : integer; RefType : shortstring; LegendFiles,ElevFiles : tStringList);

            function SaveValuesFromGrid(DEM : integer; What : shortstring) : ShortString;
            var
               Col,Row,Npts :integer;
               zs : ^bfarray32;
               z : float32;
            begin
               New(ZS);
               NPts := 0;
               for Col := 0 to pred(DEMGlb[DEM].DEMHeader.NumCol) do begin
                  for Row := 0 to pred(DEMGlb[DEM].DEMHeader.NumRow) do begin
                     if DEMGlb[DEM].GetElevMetersOnGrid(col,row,z) then begin
                       zs^[Npts] := z;
                       inc(NPts);
                     end;
                  end;
               end;

               if (NPts > 0) then begin
                  Result := DEMIXtempfiles + DEMGlb[DEM].AreaName + '_' + AreaName + '.z';
                  SaveSingleValueSeries(npts,zs^,Result);
               end;
               Dispose(zs);
            end;

     var
        DiffGrid : integer;
        fName : PathStr;
     begin
         DiffGrid := MakeDifferenceMap(RefGrid,TestGrid,RefGrid,0,true,false,false);
         DEMglb[DiffGrid].AreaName := AreaName + '_' + TestSeries[i] + '_' + ShortName + '_' + RefType;
         fName := DEMIXtempfiles + DEMglb[DiffGrid].AreaName + '.dem';
         DEMglb[DiffGrid].WriteNewFormatDEM(fName);
         ElevFiles.Add(SaveValuesFromGrid(DiffGrid,ShortName + '_' + RefType + '_'));
         LegendFiles.Add(TestSeries[i]);
         CloseSingleDEM(DiffGrid);
         if (ShortName <> 'elvd') then begin
            fName := AreaName + '_percent_diff_' + TestSeries[i] + '_' + ShortName + '_' + RefType;
            DiffGrid := PercentDifferentTwoGrids(RefGrid,TestGrid,fName);
            fName := DEMIXtempfiles + fName + '.dem';
            DEMglb[DiffGrid].WriteNewFormatDEM(fName);
            CloseSingleDEM(RefGrid);
         end;
     end;



begin
   {$IfDef RecordDEMIX} HighlightLineToDebugFile('start differences ' + LongName); {$EndIf}
   MDDef.DefaultGraphXSize := 1000;
   MDDef.DefaultGraphYSize := 600;
   DTMElevFiles := tStringList.Create;
   DTMLegendFiles := tStringList.Create;
   DSMElevFiles := tStringList.Create;
   DSMLegendFiles := tStringList.Create;

   for I := 1 to MaxDEMIXDEM do begin
      if ValidDEM(TestDEMs[i]) then begin
         GetReferenceDEMsForTestDEM(TestSeries[i],UseDSM,UseDTM);

         if (ShortName = 'elvd') then begin
            TestGrid := TestDEMs[i];
            DTMGrid := UseDTM;
         end;
         if (ShortName = 'slpd') then begin
            TestGrid := CreateSlopeMap(TestDEMs[i]);
            DTMGrid := CreateSlopeMap(UseDTM);
         end;
         if (ShortName = 'rufd') then begin
            TestGrid := CreateRoughnessSlopeStandardDeviationMap(TestDEMs[i],3);
            DTMGrid := CreateRoughnessSlopeStandardDeviationMap(UseDTM,3);
         end;

         {$IfDef RecordDEMIX} writeLineToDebugFile(Testseries[i] + ' DTMs ' + DEMGlb[DTMgrid].AreaName + '  ' + DEMGlb[Testgrid].AreaName + ' ' + IntToStr(DTMGrid) + '/' + IntToStr(TestGrid)); {$EndIf}

         MakeDifferenceGrid(DTMGrid,'dtm',DTMLegendFiles,DTMElevFiles);

         if (UseDSM <> 0) then begin
            if (ShortName = 'elvd') then begin
               DSMGrid := UseDSM;
            end;
            if (ShortName = 'slpd') then begin
               DSMGrid := CreateSlopeMap(UseDSM);
            end;
            if (ShortName = 'rufd') then begin
               DSMGrid := CreateRoughnessSlopeStandardDeviationMap(UseDSM,3);
            end;
            {$IfDef RecordDEMIX} writeLineToDebugFile(Testseries[i] + ' DSMs ' + DEMGlb[DSMgrid].AreaName + '  ' + DEMGlb[Testgrid].AreaName + ' ' + IntToStr(DSMGrid) + '/' + IntToStr(TestGrid)); {$EndIf}
            MakeDifferenceGrid(DSMGrid,'dsm',DSMLegendFiles,DSMElevFiles);
         end;
         if (ShortName <> 'elvd') then CloseSingleDEM(Testgrid);
         {$IfDef RecordDEMIX} WriteLineToDebugFile('After ' + TestSeries[i] + ', Open grids now=' + IntToStr(NumDEMDataSetsOpen) ); {$EndIf}
      end;
   end;
   {$IfDef RecordDEMIX} WriteLineToDebugFile('start graphs'); {$EndIf}

   if (ShortName = 'elvd') then begin
      Min := -50;
      Max := 50;
      BinSize := 0.25;
   end
   else if (ShortName = 'slpd') then begin
      Min := -50;
      Max := 50;
      BinSize := 0.25;
   end
   else if (ShortName = 'rufd') then begin
      Min := -20;
      Max := 20;
      BinSize := 0.15;
   end;

   Graph1 := CreateMultipleHistogram(MDDef.CountHistograms,DTMElevFiles,DTMLegendFiles,AreaName + ' DTM ' + LongName + ' difference','DTM ' + LongName + ' difference distribution',100,Min,Max,BinSize);
   ModifyGraph(Graph1);
   if (DSMElevFiles.Count > 0) then begin
      Graph2 := CreateMultipleHistogram(MDDef.CountHistograms,DSMElevFiles,DSMLegendFiles,AreaName + ' DSM ' + LongName + ' difference','DSM ' + LongName + ' difference distribution',100,Min,Max,BinSize);
      ModifyGraph(Graph2);
   end
   else begin
      Graph2 := Nil;
   end;
   {$IfDef RecordDEMIX} writeLineToDebugFile('done differences'); {$EndIf}
end;


function LoadDEMIXCandidateDEMs(AreaName : ShortString; aRefDEM : integer; OpenMaps : boolean = false; AllCandidates : boolean = true) : boolean;
var
   {$IfDef RecordDEMIX} AllDEMs, {$EndIf}
   WantSeries,ShortName : shortstring;
   IndexSeriesTable : tMyData;
   NumPts : int64;
   WantDEM,WantImage,Ser,i,GeoidGrid : integer;
   fName,SaveName : Pathstr;


         procedure MoveFromEGM96toEGM2008(var DEM : integer);
         //Reproject vertical datum to EGM2008 if required because DEM is EGM96
         var
           Col,Row,NewDEM : integer;
           z,z2 : float32;
           Lat,Long : float64;
         begin
            {$IfDef RecordDEMIXVDatum} writeLineToDebugFile('CheckVerticalDatumShift in, DEM=' + IntToStr(DEM)  + '  ' + DEMGlb[DEM].AreaName); {$EndIf}
            if ValidDEM(DEM) and (DEMGlb[DEM].DEMHeader.VerticalCSTypeGeoKey = VertCSEGM96) then begin
               NewDEM := DEMGlb[DEM].ResaveNewResolution(fcSaveFloatingPoint); //have to resave because input DEMs are all integer resolution
               DEMGlb[NewDEM].AreaName := DEMGlb[DEM].AreaName;  // + '_egm2008';
               DEMGlb[NewDEM].DEMHeader.VerticalCSTypeGeoKey := VertCSEGM2008;
               {$IfDef RecordDEMIXVDatum} writeLineToDebugFile('CheckVerticalDatumShift with shift ' + DEMGlb[DEM].AreaName); {$EndIf}
               z2 := 0;
               for Col := 0 to pred(DEMGlb[NewDEM].DEMHeader.NumCol) do begin
                  for Row := 0 to pred(DEMGlb[NewDEM].DEMHeader.NumRow) do begin
                      if DEMGlb[NewDEM].GetElevMetersOnGrid(Col,Row,z) then begin
                         DEMGlb[NewDEM].DEMGridToLatLongDegree(Col,Row,Lat,Long);
                         if DEMGlb[GeoidGrid].GetElevFromLatLongDegree(Lat,Long,z2) then begin
                            DEMGlb[NewDEM].SetGridElevation(Col,Row,z+z2);
                         end;
                      end;
                  end;
               end;
               CloseSingleDEM(DEM);
               {$IfDef RecordDEMIXLoad} writeLineToDebugFile('Closed DEM; Open DEMs=' + IntToStr(NumDEMdatasetsOpen)); {$EndIf}
               DEMGlb[NewDEM].CheckMaxMinElev;
               DEM := NewDEM;
               {$IfDef RecordDEMIXVDatum} writeLineToDebugFile('CheckVerticalDatumShift out, DEM=' + IntToStr(DEM) + '  ' + DEMGlb[DEM].AreaName); {$EndIf}
            end
            else begin
               {$IfDef RecordDEMIXVDatum} writeLineToDebugFile('CheckVerticalDatumShift out, not EGM96, DEM=' + IntToStr(DEM) + '  ' + DEMGlb[DEM].AreaName); {$EndIf}
            end;
         end;


begin
   {$If Defined(RecordDEMIXLoad)} writeLineToDebugFile('LoadDEMIXCandidateDEMs in; Open DEMs=, ' + IntToStr(NumDEMdatasetsOpen) + '   RefDEM=' + IntToStr(aRefDEM)); {$EndIf}
   Result := false;
   if not ValidDEM(aRefDEM) then exit;

   {$IfDef RecordDEMIX} AllDEMs := ''; {$EndIf}
   HeavyDutyProcessing := true;
   wmdem.SetPanelText(3,'');
   LoadDEMIXnames;

   for I := 1 to MaxDEMIXDEM do begin
      TestDEMs[i] := 0;
      TestSeries[i] := '';
   end;

   OpenIndexedSeriesTable(IndexSeriesTable);
   IndexSeriesTable.ApplyFilter('USE=' + QuotedStr('Y'));

   Ser := 0;
   for I := 1 to NumDEMIXDEM do begin
      WantSeries := DEMIXDEMTypeName[i];
      fName := DEMIX_test_dems + AreaName + '_' + DEMIXShort[i] + '.dem';
      if FileExists(fname) then begin
         {$If Defined(RecordDEMIXLoad)} writeLineToDebugFile('Loaded test DEM= ' + fName  ); {$EndIf}
         inc(Ser);
         TestDEMs[Ser] := OpenNewDEM(fName,OpenMaps);
         TestSeries[Ser] := WantSeries;
      end
      else begin
         {$If Defined(RecordDEMIXFull)} HighlightLineToDebugFile('Missing test DEM= ' + fName); {$EndIf}
      end;
   end;

   if (Ser = NumDEMIXDEM) then begin
      Result := true;
      IndexSeriesTable.Destroy;
      exit;
   end
   else begin
      {$If Defined(RecordDEMIXLoad)} writeLineToDebugFile('Reload LoadDEMIXCandidateDEMs in; Loaded only DEMs=, ' + IntToStr(Ser)); {$EndIf}
   end;





   GeoidGrid := OpenNewDEM(GeoidDiffFName,false,'geoid difference from EGM96 to EGM2008');  //to move DEMs from EGM96 to EGM2008
   GeoidDiffFName := DEMGlb[GeoidGrid].DEMFileName;

   IndexSeriesTable.First;
   Ser := 0;
   while not IndexSeriesTable.eof do begin
      WantSeries := IndexSeriesTable.GetFieldByNameAsString('SERIES');
      ShortName := IndexSeriesTable.GetFieldByNameAsString('SHORT_NAME');
      SaveName := DEMIX_test_dems + AreaName + '_' + shortname + '.dem';
      wmdem.SetPanelText(3,'Load candidate DEM ' + ShortName);
      if FileExists(SaveName) then begin
      end
      else if AllCandidates or (ShortName = 'COP') or (ShortName = 'ALOS') then begin
         {$If Defined(RecordFullDEMIX) or Defined(RecordDEMIXLoad)} writeLineToDebugFile('Try ' + WantSeries + ' ' + ShortName + '  ' + IntToStr(Ser) + '/' + IntToStr(IndexSeriesTable.FiltRecsInDB)); {$EndIf}
         {$If Defined(RecordFullDEMIX)} writeLineToDebugFile('Ref DEM=' + DEMGlb[RefDEM].AreaName + '  ' + sfBoundBoxToString(DEMGlb[RefDEM].DEMBoundBoxGeo,6)); {$EndIf}
         if LoadMapLibraryBox(WantDEM,WantImage,true,DEMGlb[aRefDEM].DEMBoundBoxGeo,WantSeries,false) and ValidDEM(WantDEM) then begin
            {$If Defined(RecordDEMIXLoad)} writeLineToDebugFile('LoadDEMIXCandidateDEMs done LoadMapLib; Open DEMs=, ' + IntToStr(NumDEMdatasetsOpen)); {$EndIf}
            inc(Ser);
            TestDEMs[Ser] := WantDEM;
            TestSeries[Ser] := ShortName;
            {$IfDef RecordDEMIXLoad} if (TestSeries[Ser] = 'ASTER') then writeLineToDebugFile('Loaded DEM:  ' + SaveName + ' ' + DEMGlb[TestDEMs[Ser]].PixelIsString); {$EndIf}

            {$IfDef RecordDEMIX}
               if not AllOfBoxInAnotherBox(DEMGlb[aRefDEM].DEMBoundBoxGeo,DEMGlb[WantDEM].DEMBoundBoxGeo) then begin
                  AllDEMs := AllDEMs + TestSeries[Ser] + ' (partial  ' + sfBoundBoxToString(DEMGlb[aRefDEM].DEMBoundBoxGeo) + ')  ';
               end;
            {$EndIf}
            DEMGlb[TestDEMs[Ser]].AreaName := TestSeries[Ser];
            DEMGlb[TestDEMs[Ser]].DEMFileName := NextFileNumber(MDTempDir, DEMGlb[TestDEMs[Ser]].AreaName + '_', '.dem');

            {$IfDef RecordDEMIXLoad} writeLineToDebugFile('Opened:' + WantSeries + '  Open DEMs=' + IntToStr(NumDEMdatasetsOpen)); {$EndIf}
            if (DEMGlb[TestDEMs[Ser]].DEMHeader.MinElev < 0.01) then DEMGlb[TestDEMs[Ser]].MarkInRangeMissing(0,0,NumPts);
            DEMGlb[TestDEMs[Ser]].DEMHeader.VerticalCSTypeGeoKey := IndexSeriesTable.GetFieldByNameAsInteger('VERT_DATUM');
            MoveFromEGM96toEGM2008(TestDEMs[Ser]);
            {$IfDef RecordDEMIXLoad} if TestSeries[Ser] = 'ASTER' then writeLineToDebugFile('EGM2003:  ' + SaveName + ' ' + DEMGlb[TestDEMs[Ser]].PixelIsString); {$EndIf}
            If OpenMaps or (AreaName <> '') then begin
               CreateDEMSelectionMap(TestDEMs[Ser],true,false,MDDef.DefDEMMap);
               if ValidDEM(aRefDEM) then begin
                  DEMGlb[TestDEMs[Ser]].SelectionMap.ClipDEMtoregion(DEMGlb[aRefDEM].DEMBoundBoxGeo);
                  {$IfDef RecordDEMIXLoad} writeLineToDebugFile('Clipped:' + WantSeries + '  Open DEMs=' + IntToStr(NumDEMdatasetsOpen)); {$EndIf}
               end;
            end;
            ClipTheDEMtoFullDEMIXTiles(TestDEMs[Ser]);
            if (AreaName <> '') then begin
               DEMGlb[TestDEMs[Ser]].WriteNewFormatDEM(SaveName);
            end;
            {$IfDef RecordDEMIXLoad} if (TestSeries[Ser] = 'ASTER') then writeLineToDebugFile('Saved DEM:  ' + SaveName + ' ' + DEMGlb[TestDEMs[Ser]].PixelIsString); {$EndIf}
            Result := true;
         end
         else begin
            {$IfDef RecordDEMIX} AllDEMs := AllDEMs + WantSeries + ' (missing)'; {$EndIf}
         end;
      end;
      IndexSeriesTable.Next;
   end;
   IndexSeriesTable.Destroy;
   CloseSingleDEM(GeoidGrid);
   HeavyDutyProcessing := false;
   {$IfDef RecordDEMIX} if (AllDEMs <> '') then HighlightLineToDebugFile(AreaName + ' DEM problem, ' + AllDEMs); {$EndIf}
   {$IfDef RecordDEMIXLoad} writeLineToDebugFile('LoadDEMIXCandidateDEMs out; Open DEMs=, ' + IntToStr(NumDEMdatasetsOpen)); {$EndIf}
end;



function LoadDEMIXReferenceDEMs(AreaName : shortstring; var RefDEM : integer; OpenMaps : boolean = true) : boolean;
var
   NumRefDEMs : integer;

         procedure ReferenceFileOpen(var DEM : integer; fName : PathStr);
         begin
            if FileExists(fName) then begin
               DEM := OpenNewDEM(FName,OpenMaps);   //must load map for the DEMIX tile computation
               if ValidDEM(DEM) and (RefDEM = 0) then RefDEM := DEM;
               inc(NumRefDEMs);
               RefDEMs[NumRefDEMs] := DEM;
               {$If Defined(RecordDEMIXRefDEMopen)} writeLineToDebugFile('RefDEM=' + IntToStr(DEM) + '  ' + DEMGlb[DEM].AreaName); {$EndIf}
            end
            else DEM := 0;
         end;

begin
   {$If Defined(RecordDEMIX)} writeLineToDebugFile('ProcessDEMIXtestarea in, open DEMs=' + IntToStr(NumDEMDataSetsOpen)); {$EndIf}
   RefDEM := 0;
   NumRefDEMs := 0;
   GetAreaDEMNames(AreaName);

   ReferenceFileOpen(RefDTMpoint,RefDTMpointFName);
   ReferenceFileOpen(RefDTMarea,RefDTMareaFName);
   ReferenceFileOpen(COPRefDTM,COPRefDTMFName);
   if MDDef.DEMIX_open_ref_DSM and ValidDEM(RefDTMpoint) then begin
      {$If Defined(RecordDEMIX)} writeLineToDebugFile('ProcessDEMIXtestarea start DSM, open DEMs=' + IntToStr(NumDEMDataSetsOpen)); {$EndIf}
      ReferenceFileOpen(RefDSMpoint,RefDSMpointFName);
      ReferenceFileOpen(RefDSMarea,RefDSMareaFName);
      ReferenceFileOpen(COPRefDSM,COPRefDSMFName);
   end;
   Result := ValidDEM(RefDEM);
   if Result then begin
      {$If Defined(RecordDEMIXload)} writeLineToDebugFile('ProcessDEMIXtestarea out, ref DEMs open with RefDEM=' + IntToStr(RefDEM) + ' and open DEMs=' + IntToStr(NumDEMDataSetsOpen)); {$EndIf}
   end
   else begin
      {$IfDef RecordDEMIXload} HighlightLineToDebugFile('Failure, ref DEMs open'); {$EndIf}
   end;
end;

(*
function LoadDEMsForCurrentArea(AreaName: ShortString;  LoadRefMaps,LoadTestDEMmaps : boolean) : boolean;
var
   LoadResults,TStr : shortstring;
   AllTiles : tStringList;
   i,j : integer;


      procedure LoadFromPath(var Which : tDEMIXindexes; aPath : PathStr; Ext : ANSIstring; LoadMaps : boolean; What : shortstring);
      var
         FilesWanted : tStringList;
         i,j,DEMs : integer;
         fName : PathStr;
      begin
         FilesWanted := tStringList.Create;
         FindMatchingFiles(aPath,Ext,FilesWanted,1);
         RemoveFilesThatDoNotHaveString(FilesWanted,AreaName);
         {$If Defined(RecordDEMIX)} writeLineToDebugFile(AreaName + ' ' + What + ' DEMs=' + IntToStr(FilesWanted.Count)); {$EndIf}
         DEMs := 0;
         for j := 0 to pred(FilesWanted.Count) do begin
            fName := FilesWanted.Strings[j];
            if StrUtils.AnsiContainsText(UpperCase(fname),UpperCase(AreaName)) then begin
               if (DEMs = MaxDemixDEM) then begin
                  MessageToContinue('Too many DEMs in ' + aPath);
               end
               else begin
                  if (What = 'Ref') then begin
                     if StrUtils.AnsiContainsText(UpperCase(fname),UpperCase('DSM')) and (not MDDef.DEMIX_open_ref_DSM) then begin
                        {$If Defined(RecordDEMIXLoad)} writeLineToDebugFile('Not opening DSM,  ' + fName); {$EndIf}
                     end
                     else if StrUtils.AnsiContainsText(fname,'ref') then begin
                        inc(DEMs);
                        Which[DEMs] := OpenNewDEM(fName,LoadMaps);
                     end
                  end
                  else begin
                     for i := 1 to NumDEMIXDEM do begin
                        if StrUtils.AnsiContainsText(fname,UpperCase(DEMIXShort[i])) then begin
                           inc(DEMs);
                           Which[i] := OpenNewDEM(fName,LoadMaps);
                        end;
                     end;
                  end;
               end;
              {$If Defined(RecordDEMIXLoad)} writeLineToDebugFile('DEM=' + IntToStr(Which[i]) + '   ' + fName); {$EndIf}
            end;
         end;
         FilesWanted.Free;
         LoadResults := LoadResults + What + '=' + IntToStr(DEMs) + '  ';
         {$IfDef RecordDEMIXLoad} WriteLineToDebugFile('Loaded ' + What + '=' + IntToStr(DEMs)); {$EndIf}
      end;

begin
   Result := true;
   MDDef.DEMIX_default_area := AreaName;
   //CurrentOperation.Text := 'Load DEMs for ' + AreaName;
   {$IfDef RecordDEMIXLoad} WriteLineToDebugFile('TDemixFilterForm.LoadDEMsForArea in ' + AreaName); {$EndIf}
   ZeroDEMs;
   LoadResults := '';

   if LoadOneSecRefCheckBox.Checked then LoadFromPath(RefDEMs,DEMIX_Ref_1sec,'*.tif',LoadRefMaps,'Ref');

   if CheckBox3.Checked then begin
      LoadFromPath(TestDEMs,DEMIX_test_dems,'*.dem',LoadTestDEMmaps,'Test');
      if (DEMsinIndex(TestDEMs) <> RequiredTestDEMs) then begin
         TStr := 'Found: ' + IntToStr(DEMsinIndex(TestDEMs)) + ' test DEMs; need ' + IntToStr(RequiredTestDEMs) + ' for ' + AreaName;
         //Memo8.Lines.Add(TStr);
         {$IfDef RecordDEMIX}
            HighlightLineToDebugFile(TStr);
            for i := 1 to MaxDemixDEM do begin
               if ValidDEM(TestDEMs[i]) then WriteLineToDebugFile('Found: ' + DEMGlb[TestDEMs[i]].AreaName);
            end;
         {$EndIf}
         Result := false;
      end;
   end;

   if CheckBox1.Checked then LoadFromPath(MergeDEMs,DEMIX_Ref_Merge,'*.dem',LoadRefMaps,'Merge');
   if CheckBox5.Checked then LoadFromPath(RefDEMsHalfSec,DEMIX_Ref_Half_sec,'*.tif',LoadRefMaps,'Ref_half_sec');
   if CheckBox6.Checked and (not CheckBox3.Checked) then begin
      //DEMs := 0;
      LoadFromPath(TestDEMs,DEMIX_test_dems,'*.dem',LoadTestDEMmaps,'COP & ALOS Test');
      LoadFromPath(TestDEMs,DEMIX_test_dems,'*.dem',LoadTestDEMmaps,'COP & ALOS Test');
      LoadFromPath(TestDEMs,DEMIX_test_dems,'*.dem',LoadTestDEMmaps,'COP & ALOS Test');
   end;

   ComboBox1.Items.Clear;
   AllTiles := nil;
   for i := 1 to MaxDemixDEM do begin
      if ValidDEM(RefDEMs[i]) and (AllTiles = Nil) then begin
         //AllTiles := DEMGlb[RefDEMs[i]].SelectionMap.DEMIXtilesOnMap;
         AllTiles := DEMIXTilesOnDEM(RefDEMs[i]);
      end;
   end;
   if (AllTiles <> Nil) then begin
      if (AllTiles.Count > 0) then begin
         for i := 0 to pred(AllTiles.Count) do ComboBox1.Items.Add(AllTiles.Strings[i]);
         ComboBox1.Text := AllTiles.Strings[0];
      end;
      AllTiles.Destroy;
   end;
   BitBtn34.Enabled := false;

   {$IfDef RecordDEMIX} WriteLineToDebugFile('TDemixFilterForm.LoadDEMsForArea out ' + AreaName + '  ' + LoadResults); {$EndIf}
end;
*)


procedure DiluviumDEMforTestAreas;
var
   TheFiles,Areas : tStringList;
   AreaName : Shortstring;
   fName,SaveName : PathStr;
   i,RefDEM,NewDEM,WantImage : Integer;
   db : TMyData;
begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DiluviumDEMforTestAreas in'); {$EndIf}
   try
      GetDEMIXpaths(true);
      fName := DEMIXSettingsDir + 'tiles_compare_diluvium.dbf';
      db := TMyData.Create(fName);
      Areas := db.UniqueEntriesInDB('AREA');
      for i := 0 to pred(Areas.Count) do begin
         AreaName := Areas.Strings[i];
         wmdem.SetPanelText(2,IntToStr(i) + '/' + IntToStr(Areas.Count) + '  ' +  AreaName);
         if LoadDEMIXReferenceDEMs(Areas.Strings[i],RefDEM,false) then begin
            if LoadMapLibraryBox(NewDEM,WantImage,true,DEMGlb[RefDEM].DEMBoundBoxGeo,'DILUV',true) and ValidDEM(NewDEM) then begin
               DEMGlb[NewDEM].SelectionMap.ClipDEMtoregion(DEMGlb[RefDEM].DEMBoundBoxGeo);
               DEMGlb[NewDEM].DEMHeader.VerticalCSTypeGeoKey := VertCSEGM2008;
               ClipTheDEMtoFullDEMIXTiles(NewDEM);
               SaveName := DEMIX_test_dems + AreaName + '_DILUV' + '.dem';
               DEMGlb[NewDEM].WriteNewFormatDEM(SaveName);
            end;
         end;
         CloseAllDEMs;
      end;
      Areas.Free;
      db.Destroy;
   finally
      EndDEMIXProcessing;
   end;
   //{$If Defined(RecordDEMIX)} WriteLineToDebugFile('DiluviumDEMforTestAreas out');  {$EndIf}
end;


{$include demix_graphs.inc}

{$include demix_create_database.inc}

{$include demix_create_ref_dems.inc}


{$IfDef Old3DEP}
   {$I old_demix_3dep_routines.inc}
{$EndIf}


{$If Defined(AllowEDTM) or Defined(OldDEMIXroutines)}
   {$I experimental_demix_criteria.inc}
{$EndIf}


{$IfDef OpenDEMIXAreaAndCompare}
   {$I open_demix_area.inc}
{$EndIf}




end.





