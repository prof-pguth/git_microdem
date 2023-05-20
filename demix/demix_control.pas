unit demix_control;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2023 Peter L. Guth  }
{___________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   {$Define RecordDEMIX}
   {$Define RecordDEMIXLoad}
   {$Define RecordDEMIXsave}
   {$Define RecordCreateHalfSec}
   //{$Define RecordDEMIXMovies}
   //{$Define RecordDEMIXVDatum}
   //{$Define RecordFullDEMIX}
   //{$Define ShowDEMIXWhatsOpen}
{$EndIf}


interface

uses
    System.SysUtils,System.Classes,StrUtils,VCL.ExtCtrls,VCL.Forms, VCL.Graphics, WinAPI.Windows,
    Petmar,Petmar_types,BaseGraf;


//service functions and procedures
   function LoadDEMIXReferenceDEMs(var RefDEM : integer) : boolean;
   function LoadDEMIXCandidateDEMs(AreaName : ShortString; RefDEM : integer; OpenMaps : boolean = false; AllCandidates : boolean = true) : boolean;
   //function LoadDEMIXareaDefinitions(cfName : pathStr = '') : boolean;
   procedure GetReferenceDEMsForTestDEM(TestSeries : shortstring; var UseDSM,UseDTM : integer);
   function SymbolFromDEMName(DEMName : shortstring) : tFullSymbolDeclaration;
   function DEMIXColorFromDEMName(DEMName : shortstring) : tPlatformColor;
   procedure OpenDEMIXArea(fName : PathStr = '');
   procedure ZeroDEMs;


//DEMIX wine contest procedures based on the database
   procedure WinsAndTies(DBonTable : integer);
   function BestByParameterSorting(DBonTable : integer; TileParam,Criterion,DEMtype : shortstring) : tThisBaseGraph;
   procedure MultipleBestByParameters(DBonTable : integer);
   function DEMIXwineContestScoresGraph(DBonTable : integer; XScalelabel : shortstring; MinHoriz : float32 = 0.5; MaxHoriz : float32 = 5.5) : tThisBaseGraph;
   procedure DEMIX_graph_best_in_Tile(DBonTable : integer; SortByArea : boolean);
   procedure DEMIXisCOPorALOSbetter(DBonTable : integer);
   procedure BestDEMSbyCategory(DBonTable : integer);


procedure GetDEMIXpaths(StartProcessing : boolean = true);


procedure DEMIX_CreateReferenceDEMs;
procedure DEMIX_merge_source;
procedure DEMIX_VDatum_shifts;

procedure ComputeDEMIX_tile_stats(FilesWanted : tStringList = Nil);
procedure CreateDEMIX_GIS_database;

procedure OpenDEMIXDatabaseForAnalysis;
function MakeGraphOfDifferenceDistribution(Tile,param,Ref : shortstring) : tThisBaseGraph;


const
   MaxTestDEM = 10;
var
   TestDEM : array[1..MaxTestDEM] of integer;
   TestSeries : array[1..MaxTestDEM] of shortstring;
   DEMIX_DB_v1, DEMIX_DB_v2,
   HalfSecRefDTM,HalfSecRefDSM,HalfSecDTM,HalfSecALOS,HalfSecCOP,
   DEMIXRefDEM,RefDTMpoint,RefDTMarea,RefDSMpoint,RefDSMarea, COPRefDTM, COPRefDSM : integer;

   DEMIX_Ref_Source,DEMIX_Ref_Merge,DEMIX_Ref_1sec,DEMIX_Ref_1sec_v1,
   DEMIX_test_dems,
   DEMIX_GIS_dbName_v2,DEMIX_GIS_dbName_v1,

   GeodeticFName, IceSatFName, LandCoverFName,
   LocalDatumAddFName,LocalDatumSubFName,RefDSMPointFName,RefDSMareaFName,RefDTMPointFName,RefDTMareaFName, COPRefDTMFName,COPRefDSMFName : PathStr;


implementation

uses
   Nevadia_Main,
   DEMstat,Make_grid,PetImage,PetImage_form,new_petmar_movie,DEMdatabase,PetDButils,Pick_several_dems,
   Geotiff, BaseMap, GDAL_tools, DEMIX_filter,
   DEMCoord,DEMdefs,DEMMapf,DEMDef_routines,DEM_Manager,DEM_indexes,Petmar_db,PetMath;

var
   vd_path,DEMIX_area_dbName_v2,DEMIX_data_base,DEMIX_diff_dist,DEMIX_area_lc100{,DEMIX_area_dbName}  : PathStr;



function DEMIX_AreasWanted : tStringList;
var
   aTable : tMyData;
   PickedNum : integer;
begin
   aTable := tMyData.Create(DEMIX_area_dbName_v2);
   Result := aTable.UniqueEntriesInDB('AREA');
   aTable.Destroy;
   GetFromListZeroBased('DEMIX test areas to process',PickedNum,Result,false,true);
   //for PickedNum := 0 to pred(Result.Count) do WriteLineToDebugFile('Picked ' + Result.Strings[PickedNum]);
end;



procedure GetDEMIXpaths(StartProcessing : boolean = true);
begin
   if StartProcessing then HeavyDutyProcessing := true;
   StopSplashing;

   DEMIX_Ref_Source := 'G:\wine_contest\wine_contest_v2_ref_source\';
   FindDriveWithPath(DEMIX_Ref_Source);
   DEMIX_Ref_Merge := 'G:\wine_contest\wine_contest_v2_ref_merge\';
   FindDriveWithPath(DEMIX_Ref_Merge);
   DEMIX_Ref_1sec := 'G:\wine_contest\wine_contest_v2_ref_1sec\';
   FindDriveWithPath(DEMIX_Ref_1sec);
   DEMIX_Ref_1sec_v1 := 'G:\wine_contest\demix_reference_dems_v1\';
   FindDriveWithPath(DEMIX_Ref_1sec_v1);
   vd_path := 'G:\wine_contest\wine_contest_v2_vdatum\';
   FindDriveWithPath(vd_path);
   DEMIXresultsDir := 'G:\wine_contest\wine_contest_v2_tile_stats\';
   FindDriveWithPath(DEMIXresultsDir);
   DEMIX_test_dems := 'G:\wine_contest\wine_contest_v2_test_dems\';
   FindDriveWithPath(DEMIX_test_dems);
   DEMIXSettingsDir := 'G:\wine_contest\wine_contest_settings\';
   FindDriveWithPath(DEMIXSettingsDir);
   DEMIX_data_base  := 'G:\wine_contest\wine_contest_database\';
   FindDriveWithPath(DEMIX_data_base);
   DEMIX_diff_dist  := 'G:\wine_contest\wine_contest_v2_diff_dist\';
   FindDriveWithPath(DEMIX_diff_dist);
   DEMIXSettingsDir := 'G:\wine_contest\wine_contest_settings\';
   FindDriveWithPath(DEMIXSettingsDir);
   DEMIX_area_lc100  := 'G:\wine_contest\wine_contest_v2_lc100\';
   FindDriveWithPath(DEMIX_area_lc100);

   DEMIX_area_dbName_v2 := 'G:\wine_contest\wine_contest_v2_ref_source\demix_area_vert_datums.dbf';
   FindDriveWithFile(DEMIX_area_dbName_v2);

   DEMIX_GIS_dbName_v2 := 'G:\wine_contest\wine_contest_database\demix_database_v1.92.dbf';
   FindDriveWithFile(DEMIX_GIS_dbName_v2);

   DEMIX_GIS_dbName_v1 := 'G:\wine_contest\wine_contest_database\demix_database_v1.dbf';
   FindDriveWithFile(DEMIX_GIS_dbName_v1);


   Geoid2008FName := 'g:\geoid\egm2008-1-vdatum.tif';
   FindDriveWithFile(Geoid2008FName);
   GeoidDiffFName := 'g:\geoid\egm96_to_egm2008.tif';
   FindDriveWithFile(GeoidDiffFName);
end;


function MakeGraphOfDifferenceDistribution(Tile,param,Ref : shortstring) : tThisBaseGraph;
var
   FilesWanted,Distributions,Legends : tStringList;
   fName : PathStr;
   i,j : integer;
   Min,Max,BinSize : float32;
begin
   //new_orleans_ALOS_N29ZW091L_elev_to_DTM.z
   MDDef.DefaultGraphXSize := 1000;
   MDDef.DefaultGraphYSize := 600;
   FilesWanted := tStringList.Create;
   FindMatchingFiles(DEMIX_diff_dist,'*.z',FilesWanted,0);

   Distributions := tStringList.Create;
   Legends := tStringList.Create;
   for i := 0 to pred(FilesWanted.Count) do begin
      fName := upperCase(FilesWanted.Strings[i]);
      if StrUtils.AnsiContainsText(fname,Tile) and StrUtils.AnsiContainsText(fname,UpperCase(Param)) and StrUtils.AnsiContainsText(fname,UpperCase(Ref)) then begin
         Distributions.Add(fName);
         for j := 1 to NumDEMIXDEM do begin
            if StrUtils.AnsiContainsText(fname,UpperCase(DEMIXDEMTypeName[j])) then begin
               Legends.Add(DEMIXDEMTypeName[j]);
            end;
         end;

         if StrUtils.AnsiContainsText(fname,'ELEV') then begin
            Min := -20;
            Max := 20;
            BinSize := 0.25;
         end;

         if StrUtils.AnsiContainsText(fname, 'SLOPE') then begin
            Min := -20;
            Max := 20;
            BinSize := 0.25;
         end;
         if StrUtils.AnsiContainsText(fname, 'RUFF') then begin
            Min := -20;
            Max := 20;
            BinSize := 0.15;
         end;
      end;
   end;

   if (Distributions.Count = 6) and (Legends.Count = 6) then begin
      Result := CreateMultipleHistogram(MDDef.CountHistograms,Distributions,Legends,Tile + '  ' + param + ' difference distribution',Tile + '  ' + param + ' difference',100,Min,Max,BinSize);
      for I := 1 to Result.GraphDraw.LegendList.Count do begin
         Result.GraphDraw.FileColors256[i] := DEMIXColorFromDEMName(Result.GraphDraw.LegendList[pred(i)]);
      end;
      Result.RedrawDiagram11Click(Nil);
      Result.Image1.Canvas.Draw(Result.GraphDraw.LeftMargin+15,Result.GraphDraw.TopMargin+10,Result.MakeLegend(Result.GraphDraw.LegendList,false));
   end;
   FilesWanted.Free;
end;


procedure OpenDEMIXDatabaseForAnalysis;
begin
   GetDEMIXpaths(false);
   if not FileExists(DEMIX_GIS_dbName_v2) then Petmar.GetFileNameDefaultExt('DEMIX db version 2','*.dbf',DEMIX_GIS_dbName_v2);
   if not FileExists(DEMIX_GIS_dbName_v1) then Petmar.GetFileNameDefaultExt('DEMIX db version 1','*.dbf',DEMIX_GIS_dbName_v1);
   OpenNumberedGISDataBase(DEMIX_DB_v2,DEMIX_GIS_dbName_v2,false);
   GISdb[DEMIX_DB_v2].LayerIsOn := false;
   OpenNumberedGISDataBase(DEMIX_DB_v1,DEMIX_GIS_dbName_v1,false);
   GISdb[DEMIX_DB_v1].LayerIsOn := false;
   DoDEMIXFilter(DEMIX_DB_v2);
end;



procedure CreateDEMIX_GIS_database;
var
   fName : PathStr;
   i     : byte;
   DEM,db : integer;
   FilesWanted : tStringList;
   Tiles,ElevDiff,SlopeDiff,RuffDiff,TransposeNames : tStringList;


      function TransposeDEMIXcriteria(DBonTable : integer) : PathStr;
      const
         MaxDEMs = 10;
         MaxCriteria = 25;
      var
         Headers,DEMs,Criteria,Output,Tiles : tStringList;
         Line : ANSIString;
         fName,CriteriaFile : PathStr;
         value,ThisDEM,Missing : shortstring;
         Cycles,Done,
         i,j,  Tile, Ref, aLandType,DEM,Criterion : Integer;
         Values : array[0..MaxDEMs,0..MaxCriteria] of shortstring;
      begin
         {$If Defined(RecordDEMIX)} WriteLineToDebugFile(''); WriteLineToDebugFile('TransposeDEMIXcriteria in, ' + GISdb[DBonTable].dbName); {$EndIf}
         Headers := tStringList.Create;
         Headers.LoadFromFile(DEMIXSettingsDir + 'demix_headers.txt');
         DEMs := tStringList.Create;
         DEMs.LoadFromFile(DEMIXSettingsDir + 'demix_dems.txt');

         CriteriaFile := DEMIXSettingsDir + 'demix_criteria_with_signed.txt';
         Criteria := tStringList.Create;
         Criteria.LoadFromFile(CriteriaFile);

         for Criterion := pred(Criteria.Count) downto 0 do begin
            if not GISdb[DBonTable].MyData.FieldExists(Criteria.Strings[Criterion]) then begin
               Criteria.Delete(Criterion);
            end;
         end;

         Output := tStringList.Create;
         Line := '';
         for i := 0 to pred(Headers.Count) do Line := Line + Headers.Strings[i] + ',';
         Line := Line + 'TOLERANCE,DEM_LOW_SC,COP_ALOS,';
         for i := 0 to pred(DEMs.Count) do begin
            Line := Line + DEMs.Strings[i] + '_SCR,';
         end;
         Line := Line + 'REC_ID,CRITERION,';
         for i := 0 to pred(DEMs.Count) do begin
            Line := Line + DEMs.Strings[i];
            if i < pred(DEMs.Count) then Line := Line + ',';
         end;
         Output.Add(Line);

         GISdb[DBonTable].ClearGISFilter;
         GISdb[DBonTable].EmpSource.Enabled := false;
         Tiles := GISdb[DBonTable].MyData.UniqueEntriesInDB('DEMIX_TILE');
         {$If Defined(RecordDEMIX)} WriteLineToDebugFile('TransposeDEMIXcriteria tiles=' + IntToStr(Tiles.Count)); {$EndIf}

         Cycles := Tiles.Count * 2 * MaxLandType;
         Done := 0;
         StartProgress('Transposing ' + GISdb[DBontable].DBName);
         for Tile := 0 to Pred(Tiles.Count) do begin
            wmdem.SetPanelText(2,IntToStr(succ(Tile)) + '/' + IntToStr(Tiles.Count));
            wmdem.SetPanelText(3,Tiles.Strings[Tile]);
            UpdateProgressBar(Done/Cycles);
            for Ref := 1 to 2 do begin
               Line := 'DEMIX_TILE=' + QuotedStr(Tiles[Tile]) + ' AND REF_TYPE=' + QuotedStr(RefDEMType[Ref]);
               GISdb[DBonTable].ApplyGISFilter(Line);
               GISdb[DBonTable].EmpSource.Enabled := false;
               if (GISdb[DBonTable].MyData.FiltRecsInDB > 0) then begin  //there are DSMs or DTMs for this tile
                  for aLandType := 1 to MaxLandType do begin
                     Line := 'DEMIX_TILE=' + QuotedStr(Tiles[Tile]) + ' AND REF_TYPE=' + QuotedStr(RefDEMType[Ref])  + ' AND LAND_TYPE=' + QuotedStr(LandType[aLandType]);
                     GISdb[DBonTable].ApplyGISFilter(Line);
                     GISdb[DBonTable].EmpSource.Enabled := false;
                     if (GISdb[DBonTable].MyData.FiltRecsInDB = DEMs.Count) then begin
                        for i := 0 to MaxDems do
                           for j := 0 to MaxCriteria do
                               Values[i,j] := '-9999';

                        while not GISdb[DBonTable].MyData.eof do begin
                           GISdb[DBonTable].EmpSource.Enabled := false;
                           ThisDEM := UpperCase(GISdb[DBonTable].MyData.GetFieldByNameAsString('DEM'));

                           //clean up problem entries; no time to track down where these crept in
                           ThisDEM := StringReplace(ThisDEM,'_2','',[rfReplaceAll, rfIgnoreCase]);     //unclear where this is from
                           ThisDEM := StringReplace(ThisDEM,'_9','',[rfReplaceAll, rfIgnoreCase]);     //unclear where this is from
                           ThisDEM := StringReplace(ThisDEM,'_2022','',[rfReplaceAll, rfIgnoreCase]);  //poor choice for the name of an area
                           if StrUtils.AnsiContainsText(ThisDEM,'_') then begin
                              i := Length(ThisDEM);
                              while ThisDEM[i] <> '_' do dec(i);
                              Delete(ThisDEM,1,i);
                           end;

                           DEM := DEMs.IndexOf(ThisDEM);
                           if (DEM = -1) then begin
                              MessageToContinue('-1 index for ' + GISdb[DBonTable].MyData.GetFieldByNameAsString('DEM'));
                           end
                           else begin
                              for Criterion := 0 to pred(Criteria.Count) do begin
                                 value := GISdb[DBonTable].MyData.GetFieldByNameAsString(Criteria[Criterion]);
                                 if (value <> '') then Values[DEM,Criterion] := value;
                              end;
                           end;
                           GISdb[DBonTable].MyData.Next;
                        end;

                        for Criterion := 0 to pred(Criteria.Count) do begin
                           Line := '';
                           for i := 0 to pred(Headers.Count) do Line := Line + GISdb[DBonTable].MyData.GetFieldByNameAsString(Headers[i]) + ',';
                           Line := Line + '99.99,Not_yet_done,Not4,';
                           for DEM := 0 to pred(DEMs.Count) do Line := Line + '9.9,';
                           Line := Line + '123456,' + Criteria[Criterion] + ',';

                           for DEM := 0 to pred(DEMs.Count) do Line := Line + Values[DEM,Criterion] + ',';
                           Delete(Line,Length(Line),1);
                           Output.Add(Line);
                        end;
                     end
                     else begin
                        {$If Defined(RecordDEMIX)}  //this is to track down why some tiles do not have 6 records
                           if (GISdb[DBonTable].MyData.FiltRecsInDB > 0) then begin  //if it was 0, it was something like CLIFF in the flatlands
                              Missing := '  Present: ';
                              GISdb[DBonTable].MyData.First;
                              while not GISdb[DBonTable].MyData.eof do begin
                                 Missing := Missing + GISdb[DBonTable].MyData.GetFieldByNameAsString('DEM') + ', ';
                                 GISdb[DBonTable].MyData.Next;
                              end;
                              WriteLineToDebugFile('filter=' + GISdb[DBonTable].MyData.Filter + '   Matches=' + IntToStr(GISdb[DBonTable].MyData.FiltRecsInDB) + Missing);
                           end;
                        {$EndIf}
                     end;
                  end;
               end;
            end;
         end;
         EndProgress;
         GISdb[DBonTable].ClearGISFilter;
         Headers.Free;
         Criteria.Free;
         DEMs.Free;
         Result := ChangeFileExt(GISdb[DBonTable].dbFullName, '_transpose_' + ExtractFileNameNoExt(CriteriaFile) + '.dbf');
         DeleteFileIfExists(Result);
         Result := ChangeFileExt(Result,'.csv');
         OutPut.SaveToFile(Result);
         OutPut.Free;
         GISdb[DBonTable].ShowStatus;
         {$If Defined(RecordDEMIX)} WriteLineToDebugFile('TransposeDEMIXcriteria out, created ' + Result); {$EndIf}
      end;



   procedure MergeAndTranspose(var Diffs : tStringList; fName : PathStr);
   begin
      if (Diffs.Count > 0) then begin
         {$If Defined(RecordDEMIX)} WriteLineToDebugFile('MergeAndTranspose ' + fName + ' with regions=' + IntToStr(Diffs.Count)); {$EndIf}
         MergeCSVFiles(Diffs,fName);
         db := OpenMultipleDataBases('',fName);
         GISdb[db].AddSequentialIndex('REC_ID',false);
         fName := TransposeDEMIXcriteria(DB);
         TransposeNames.Add(fName);
         CloseAndNilNumberedDB(db);
      end
      else begin
         {$If Defined(RecordDEMIX)} WriteLineToDebugFile('No MergeAndTranspose ' + fName + '  with regions=' + IntToStr(Diffs.Count)); {$EndIf}
         Diffs.Free;
      end;
   end;

var
   Areas : tStringList;
   KeepThisOne : boolean;
   j : integer;
begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('MergeDEMIXCSV in'); {$EndIf}
   GetDEMIXpaths;
   ShowHourglassCursor;
   Areas := DEMIX_AreasWanted;
   Areas.Sorted := false;
   for i := pred(Areas.Count) downto 0 do begin
      fName := Areas.Strings[i];
      if StrUtils.AnsiContainsText(UpperCase(fname),'_DTM') then Areas.Strings[i] := Copy(fName,1,length(fName)-4);
      if StrUtils.AnsiContainsText(UpperCase(fname),'_DSM') then Areas.Delete(i);
   end;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('MergeDEMIXCSV picked areas=' + IntToStr(Areas.Count)); {$EndIf}


   FindMatchingFiles(DEMIXresultsDir,'*.csv',FilesWanted,0);
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('CSV total results=' + IntToStr(FilesWanted.Count)); {$EndIf}
   for j := pred(FilesWanted.Count) downto 0 do begin
      KeepThisOne := false;
      for i := pred(Areas.Count) downto 0 do begin
         if StrUtils.AnsiContainsText(FilesWanted.Strings[j],Areas.Strings[i]) then begin
            KeepThisOne := true;
         end;
      end;
      if Not KeepThisOne then FilesWanted.Delete(j);
   end;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('CSV in these areas=' + IntToStr(FilesWanted.Count)); {$EndIf}


   //FilesWanted := tStringList.Create;
   //FilesWanted.Add(DEMIXresultsDir);
   //i := 1;
   //if not GetMultipleFiles('DEMIX CSV files to process','*.csv',FilesWanted,i) then exit;

   try
      Tiles := tStringList.Create;
      ElevDiff := tStringList.Create;
      SlopeDiff := tStringList.Create;
      RuffDiff := tStringList.Create;
      TransposeNames := tStringList.Create;
      for i := 0 to pred(FilesWanted.Count) do begin
        fName := uppercase(FilesWanted.Strings[i]);
        if StrUtils.AnsiContainsText(fname,'DEMIX_TILES_USED_') then Tiles.Add(fName);
        if StrUtils.AnsiContainsText(fname,'ELEV_DIFF_STATS_') then ElevDiff.Add(fName);
        if StrUtils.AnsiContainsText(fname,'SLOPE_DIFF_STATS_') then SlopeDiff.Add(fName);
        if StrUtils.AnsiContainsText(fname,'RUFF_DIFF_STATS_') or StrUtils.AnsiContainsText(fname,'ROUGHNESS_DIFF_STATS_')  then RuffDiff.Add(fName);
      end;
      if (Tiles.Count > 1) then begin
         fName := ExtractFilePath(fName) + 'DEMIX_TILES_USED_SUMMARY.csv';
         fName := StringReplace(fName,'_dtm','',[rfReplaceAll, rfIgnoreCase]);
         MergeCSVFiles(Tiles,fName);
      end
      else Tiles.Free;

      fName := DEMIX_data_base + 'latest_demix_database.csv';

      wmdem.SetPanelText(1,'1/3  Elev');
      MergeAndTranspose(ElevDiff,DEMIX_data_base + 'elev_merge_difference_ref_dem.csv');
      wmdem.SetPanelText(1,'2/3  Slope');
      MergeAndTranspose(SlopeDiff,DEMIX_data_base + 'slope_merge_difference_ref_dem.csv');
      wmdem.SetPanelText(1,'3/3  Ruff');
      MergeAndTranspose(RuffDiff,DEMIX_data_base + 'ruff_merge_difference_ref_dem.csv');

      MergeCSVFiles(TransposeNames,fName);
      db := OpenMultipleDataBases('',fName);
      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('MergeDEMIXCSV merged and db opened'); {$EndIf}
      wmdem.ClearStatusBarPanelText;

      RankDEMS(DB);
      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('MergeDEMIXCSV DEMs ranked'); {$EndIf}
      CloseAndNilNumberedDB(db);
   finally
      HeavyDutyProcessing := false;
      ShowDefaultCursor;
   end;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('MergeDEMIXCSV out, created ' + fName); {$EndIf}
end;


procedure ComputeDEMIX_tile_stats(FilesWanted : tStringList = Nil);
//a number of options are disabled, and might require changes to the code to get working again
const
   MaxRefStore = 100;
var
   DoHorizontalShift,
   ElevDiffHists : boolean;
   MomentVar,FlatMomentVar,SteepMomentVar,GentleMomentVar,CliffMomentVar,ForestMomentVar,UrbanMomentVar,BarrenMomentVar : tMomentVar;
   SlopeMomentVar,ElevMomentVar : array[1..MaxRefStore] of tMomentVar;
   NumPits,NumPeaks : array[1..MaxRefStore] of integer;
   DEMIXtileDB,

   LandCoverGrid : integer;
   bbgrid : tGridLimits;

   IceSatFName,GeodeticFName,

   cfName,fName : PathStr;
   TestAreaName,DEMIXtile,LandTypeMask,TileHeader : shortstring;
   LatCent,LongCent,
   GridFull,Lat,Long : float64;
   zRef : float32;
   TileStats,
   ICESatStats,
   LagStats,
   SlopeMoments,
   ElevMoments,
   ElevDiffStats,
   RufDiffStats,
   SlopeDiffStats,
   PitsPeaks,
   CorrelationCoefficientsStats,
   ElevMomentDiffStats,SlopeMomentDiffStats,
   GeodeticStats : tStringList;
   zs,zsSteep,ZSFlat,zsGentle,zsCliff,zsForest,zsUrban,zsBarren : ^bfarray32;
   Rules,
   GeodeticTable,
   IceSatTable : tMyData;
   SlopeAsp,SlopeAspectTest : tSlopeAspectRec;
   ErrorLog : tStringList;
   ElevFiles,LegendFiles : tStringList;


         procedure InitializeStringLists;
         const
            AreaString = ',DEM,REF_TYPE,GRID_FULL,LAND_TYPE,LANDTYP_PC';
            SlopeDiffStatsString = ',SLPD_MIN,SLPD_Max,SLPD_Mean,SLPD_AVD,SLPD_STD,SLPD_MED,SLPD_RMSE,SLPD_MAE,SLPD_LE90,SLPD_N';
            ElevDiffStatsString = ',ELVD_MIN,ELVD_Max,ELVD_Mean,ELVD_AVD,ELVD_STD,ELVD_MED,ELVD_RMSE,ELVD_MAE,ELVD_LE90,ELVD_N';
            RufDiffStatsString = ',RUFD_MIN,RUFD_Max,RUFD_Mean,RUFD_AVD,RUFD_STD,RUFD_MED,RUFD_RMSE,RUFD_MAE,RUFD_LE90,RUFD_N';

            IcePointsStatsString = 'DICE_MIN,DICE_Max,DICE_Mean,DICE_AVD,DICE_STD,DICE_MEDI,DICE_RMSE,DICE_MAE,DICE_N,DICE_AMED';
            BMPointsStatsString = 'DBM_MIN,DBM_Max,DBM_Mean,DBM_AVD,DBM_STD,DBM_MEDI,DBM_RMSE,DBM_MAE,DBM_N,DBM_AMED';

            ElevMomentsString = 'ELEV_MIN,ELEV_Max,ELEV_Mean,ELEV_ADEV,ELEV_STD,ELEV_SKEW,ELEV_KURT,ELEV_MED,N';
            SlopeMomentsString = 'SLP_MIN,SLP_Max,SLP_Mean,SLP_ADEV,SLP_STDV,SLP_SKEW,SLP_KURT,SLP_MED,SLP_N';

            SlopeMomentsDiffString = 'SDIFF_MIN,SDIFF_Max,SDIFF_Mean,SDIFF_STD,SDIFF_SKEW,SDIFF_KURT,SDIFF_MED,SDIFF_N';
            ElevMomentsDiffString = 'EDIFF_MIN,EDIFF_Max,EDIFF_Mean,EDIFF_STD,EDIFF_SKEW,EDIFF_KURT,EDIFF_MED,EDIFF_N';

            LagStatsString = 'Lag_Max,Lag_Mean,Lag_STD,LAG_MED,LAG_LE90,LAG_NPTS';
            TileStatsString = 'AREA,DEMIX_TILE,LAT,LONG,AVG_ELEV,AVG_SLOPE,AVG_ROUGH,RELIEF,FOREST_PC,URBAN_PC,BARREN_PC';
         begin
            TileStats := tStringList.Create;
            TileStats.Add(TileStatsString);

            ElevDiffStats := tStringList.Create;
            ElevDiffStats.Add(TileStatsString + AreaString + ElevDiffStatsString);

            RufDiffStats := tStringList.Create;
            RufDiffStats.Add(TileStatsString + AreaString + RufDiffStatsString);

            SlopeDiffStats := tStringList.Create;
            SlopeDiffStats.Add(TileStatsString + AreaString + SlopeDiffStatsString);

            (*
            LagStats := tStringList.Create;
            LagStats.Add(TileStatsString + AreaString + LagStatsString);

            ElevMomentDiffStats := tStringList.Create;
            ElevMomentDiffStats.Add(TileStatsString + AreaString + ElevMomentsDiffString);

            SlopeMomentDiffStats := tStringList.Create;
            SlopeMomentDiffStats.Add(TileStatsString + AreaString + SlopeMomentsDiffString);

            SlopeMoments := tStringList.Create;
            SlopeMoments.Add(MomentStr + ',' + SlopeMomentsString);

            ElevMoments := tStringList.Create;
            ElevMoments.Add(MomentStr + ',' + ElevMomentsString);

            PitsPeaks := tStringList.Create;
            PitsPeaks.Add(TileStatsString + AreaString + 'NUM_PITS,NUM_PEAKS,DIFF_PITS,DIFF_PEAKS');

            ICESatStats := tStringList.Create;
            ICESATStats.Add(TileStatsString + AreaString + IcePointsStatsString);

            GeodeticStats := tStringList.Create;
            GeodeticStats.Add(TileStatsString + AreaString + BMPointsStatsString);

            CorrelationCoefficientsStats := tStringList.Create;
            CorrelationCoefficientsStats.Add(TileStatsString + AreaString + 'R2,INTERCEPT,SLOPE');
            *)

            {$IfDef RecordDEMIXFull} writeLineToDebugFile('InitializeStringLists out, string lists created'); {$EndIf}
         end;

         function ShortTestAreaName(TestAreaName : shortstring) : shortstring;
         begin
            Result := StringReplace(TestAreaName,'_dtm','',[rfReplaceAll, rfIgnoreCase]);
            Result := StringReplace(Result,'_dsm','',[rfReplaceAll, rfIgnoreCase]);
         end;

         function LineHeader(DEM : integer; RefType : shortstring) : shortstring;
         var
            t1,t2 : shortstring;
         begin
             if (DEM = 0) then t1 := 'xxx' else t1 := DEMGlb[DEM].AreaName;
             Result := t1 + ',' + {t2 + ',' +} RefType + ',' + RealToString(GridFull,-8,2) + ',' + LandTypeMask + ',';
         end;


         procedure SortListWithHeader(var sl : tStringList);
         var
            Header : ANSIString;
         begin
            Header := sl.Strings[0];
            sl.Delete(0);
            sl.Duplicates := dupIgnore;
            sl.Sort;
            sl.Insert(0,Header);
         end;

         procedure FinalizeStringLists(FinalSaveResults : boolean = true);

               procedure ProcessStringList(fName : PathStr; var sl : tStringList; DoStats : boolean = false);
               begin
                  {$IfDef RecordFullDEMIX} writeLineToDebugFile(fName + '  lines=' + IntToStr(sl.Count)); {$EndIf}
                  if (sl.count > 1) then begin
                     fName := DEMIXresultsDir + TestAreaName + fname + '_' + '.csv';
                     sl.SaveToFile(fName);
                  end
                  else fName := '';
                  sl.Destroy;
               end;

         begin
            {$IfDef RecordFullDEMIX} writeLineToDebugFile('DEMIX start string list processing'); {$EndIf}
            ProcessStringList('DEMIX_tiles_used',TileStats);

            ProcessStringList('_elev_diff_stats',ElevDiffStats,true);
            ProcessStringList('_ruff_diff_stats',RufDiffStats,true);
            ProcessStringList('_slope_diff_stats',SlopeDiffStats,true);

            (*
            ProcessStringList('ICESat_best_diff_stats',ICESATStats);
            ProcessStringList('Geodetic_control_stats',GeodeticStats);

            SortListWithHeader(ElevMoments);
            ProcessStringList('Elevation_moments',ElevMoments);
            SortListWithHeader(SlopeMoments);
            ProcessStringList('Slope_moments',SlopeMoments);

            ProcessStringList('Elevation_moments_diff',ElevMomentDiffStats);
            ProcessStringList('Slope_moments_diff',SlopeMomentDiffStats);
            ProcessStringList('Pits_and_peaks',PitsPeaks);
            ProcessStringList('Horizonatal_lags',LagStats);
            ProcessStringList('Full_tile_correlation',CorrelationCoefficientsStats);
            *)
         end;



   procedure ProcessDEMIXtestarea;


         function GridBoundingBox(DEM,RefDEM : integer; Clip : boolean = false) : tGridLimits;
         var
            bb : sfBoundBox;
         begin
            bb := GISdb[DEMIXtileDB].MyData.GetRecordBoundingBox;

            if Clip and (RefDEM <> 0) then begin
               if bb.xMin < DEMGlb[RefDEM].DEMBoundBoxGeo.xmin then bb.xMin := DEMGlb[RefDEM].DEMBoundBoxGeo.xmin;
               if bb.xMax > DEMGlb[RefDEM].DEMBoundBoxGeo.xmax then bb.xMax := DEMGlb[RefDEM].DEMBoundBoxGeo.xmax;
               if bb.yMin < DEMGlb[RefDEM].DEMBoundBoxGeo.ymin then bb.yMin := DEMGlb[RefDEM].DEMBoundBoxGeo.ymin;
               if bb.yMax > DEMGlb[RefDEM].DEMBoundBoxGeo.ymax then bb.yMax := DEMGlb[RefDEM].DEMBoundBoxGeo.ymax;
            end;

            DEMGlb[DEM].LatLongDegreeToDEMGridInteger(bb.ymin,bb.xmin,Result.xgridlow,Result.ygridlow);
            DEMGlb[DEM].LatLongDegreeToDEMGridInteger(bb.ymax,bb.xmax,Result.xgridhigh,Result.ygridhigh);
         end;

         procedure WriteTileDetails(bb: tGridLimits);
         var
            cols,rows : integer;
         begin
             cols := succ(bbgrid.xgridhigh-bbgrid.xgridLow);
             rows := succ(bbgrid.ygridhigh-bbgrid.ygridLow);
             WriteLineToDebugFile(DEMIXTile + RealToString(GridFull,8,2) + '%  ' + GridLimitstoString(bbgrid) + ' cols=' + IntToStr(cols) +  ' rows=' + IntToStr(rows) + ' points=' + IntToStr(cols*rows));
         end;

         function UseThisTile : boolean;
         var
            bb : sfBoundBox;
         begin
            GridFull := GISdb[DEMIXtileDB].MyData.GetFieldByNameAsFloat('GRID_FULL');
            Result := GridFull >= MDDef.DEMIX_Full;
            DEMIXtile := GISdb[DEMIXtileDB].MyData.GetFieldByNameAsString('NAME');
            bb := GISdb[DEMIXtileDB].MyData.GetRecordBoundingBox;
            LatCent := 0.5 * (bb.ymax + bb.ymin);
            LongCent := 0.5 * (bb.xmax + bb.xmin);
         end;

         function MomentStatsString(MomentVar : tMomentVar) : shortstring;
         begin
            Result := RealToString(MomentVar.MinZ,-8,2) + ',' + RealToString(MomentVar.MaxZ,-8,2) + ',' + RealToString(MomentVar.Mean,-8,2) + ',' +
                RealToString(MomentVar.avg_dev,-8,2) + ',' + RealToString(MomentVar.sdev,-8,2) + ',' + RealToString(MomentVar.median,-8,2) + ',' + RealToString(MomentVar.rmse,-8,2)  + ',' +
                RealToString(MomentVar.mae,-8,2)  + ',' + RealToString(MomentVar.LE90,-12,-2) + ',' + IntToStr(MomentVar.NPts);
         end;


         function MomentDifferenceString(aMomentVar,RefMomentVar : tMomentVar) : shortstring;
         begin
             Result := RealToString(aMomentVar.MinZ-RefMomentVar.minZ,-8,2) + ',' + RealToString(aMomentVar.MaxZ-RefMomentVar.MaxZ,-8,2) + ',' +
                            RealToString(aMomentVar.Mean-RefMomentVar.mean,-8,2) + ',' + RealToString(aMomentVar.sdev-RefMomentVar.sdev,-8,2) + ',' +
                            RealToString(aMomentVar.skew-RefMomentVar.skew,-8,2) + ',' + RealToString(aMomentVar.curt-RefMomentVar.Curt,-8,2) + ',' +
                            RealToString(aMomentVar.median-RefMomentVar.median,-8,2) + ',' + IntToStr(aMomentVar.NPts);
         end;


(*
         procedure DoICESat(DEM : integer);
         var
            Lat,Long,MedianAbs : float64;
            i : integer;
            z,z2 : float32;
            MomentVar : tMomentVar;
         begin
            if (IceSatFName = '') then exit;
            {$IfDef RecordDEMIX} writeLineToDebugFile('ICESat-2 ' + DEMGLB[DEM].AreaName); {$EndIf}
            GridFull := 9999;
            IceSatTable.First;
            MomentVar.Npts := 0;
            while not ICESatTable.eof do begin
               Lat := IceSatTable.GetFieldByNameAsFloat('LAT');
               Long := IceSatTable.GetFieldByNameAsFloat('LONG');
               Z := IceSatTable.GetFieldByNameAsFloat('ICESAT_GRD');
               if DEMGlb[DEM].GetElevFromLatLongDegree(Lat,Long,z2) then begin
                  zs^[MomentVar.NPts] := z2-z;
                  inc(MomentVar.Npts);
               end;
               ICESatTable.Next;
            end;
            moment(zs^,MomentVar,msAll);
            for i := 0 to pred(MomentVar.NPts) do zs^[i] := abs(zs^[i]);
            MedianAbs := Median(zs^,MomentVar.NPts,false);
            ICESatStats.Add(LineHeader(DEM,0) + MomentStatsString(MomentVar) + ',' + RealToString(MedianAbs,-8,2));
         end;


         procedure DoGeodetic(DEM : integer);
         var
            Lat,Long,MedianAbs : float64;
            i : integer;
            z,z2 : float32;
            MomentVar : tMomentVar;
         begin
            if (GeodeticFName = '') then exit;
            {$IfDef RecordDEMIX} writeLineToDebugFile('geodetic ' + DEMGLB[DEM].AreaName); {$EndIf}
            GridFull := 9999;
            GeodeticTable.First;
            MomentVar.Npts := 0;
            while not GeodeticTable.eof do begin
               Lat := GeodeticTable.GetFieldByNameAsFloat('LAT');
               Long := GeodeticTable.GetFieldByNameAsFloat('LONG');
               Z := GeodeticTable.GetFieldByNameAsFloat('Z_EGM2008');
               if DEMGlb[DEM].GetElevFromLatLongDegree(Lat,Long,z2) then begin
                  zs^[MomentVar.NPts] := z2-z;
                  inc(MomentVar.Npts);
               end;
               GeodeticTable.Next;
            end;
            if MomentVar.NPts > 0 then begin
               moment(zs^,MomentVar,msAll);
               for i := 0 to pred(MomentVar.NPts) do zs^[i] := abs(zs^[i]);
               MedianAbs := Median(zs^,MomentVar.NPts,false);
               GeodeticStats.Add(LineHeader(DEM,0) + MomentStatsString(MomentVar) + ',' + RealToString(MedianAbs,-8,2));
            end;
         end;
*)

            procedure ZeroMomentVar;
            begin
               MomentVar.Npts := 0;
               FlatMomentVar.Npts := 0;
               GentleMomentVar.Npts := 0;
               SteepMomentVar.Npts := 0;
               CliffMomentVar.Npts := 0;

               UrbanMomentVar.Npts := 0;
               ForestMomentVar.Npts := 0;
               BarrenMomentVar.Npts := 0;
            end;


            procedure LandCover(Lat,Long : float32; Difference : float32);
            //this is hard coded for a particular land cover data set, LC100 from Copernicus
            var
               z : float32;
            begin
               if (LandCoverGrid <> 0) and DEMGlb[LandCoverGrid].GetElevFromLatLongDegree(Lat,Long,z) then begin
                  if round(z) in [111..126] then begin
                     zsForest^[ForestMomentVar.NPts] := Difference;
                     inc(ForestMomentVar.Npts);
                  end
                  else if round(z) in [30,60,70] then begin
                     zsBarren^[BarrenMomentVar.NPts] := Difference;
                     inc(BarrenMomentVar.Npts);
                  end
                  else if round(z) in [50] then begin
                     zsUrban^[UrbanMomentVar.NPts] := Difference;
                     inc(UrbanMomentVar.Npts);
                  end;
               end;
            end;


            procedure LandTypeDiff(Difference : float32);
            begin
               zs^[MomentVar.NPts] := Difference;
               inc(MomentVar.Npts);
               if (SlopeAsp.SlopePercent < MDDef.SlopeFlatBoundary) then begin
                  zsflat^[FlatMomentVar.NPts] := Difference;
                  inc(FlatMomentVar.Npts);
               end
               else if (SlopeAsp.SlopePercent < MDDef.SlopeGentleBoundary) then begin
                  zsGentle^[GentleMomentVar.NPts] := Difference;
                  inc(GentleMomentVar.Npts);
               end
               else if (SlopeAsp.SlopePercent < MDDef.SlopeSteepBoundary) then begin
                  zsSteep^[SteepMomentVar.NPts] := Difference;
                  inc(SteepMomentVar.Npts);
               end
               else begin
                  zsCliff^[CliffMomentVar.NPts] := Difference;
                  inc(CliffMomentVar.Npts);
               end;
            end;



           function WriteDifferenceResults(DEM,REFDEM : integer; RefType : shortstring; var WhichStats : tStringList) : integer;

                     procedure WriteDifferenceResult(ltMask : shortString; var sl : tStringList; DEM,RefDEM : integer; var thezs : bfarray32; var theMoments : tMomentVar; Percent : float32);
                     var
                        i : integer;
                        aLine : shortstring;
                     begin
                        if (theMoments.NPts > MDDef.LandTypePointsNeeded) then begin
                           LandTypeMask := ltMask;
                           moment(thezs,theMoments,msAll);
                           for i := 0 to pred(theMoments.NPts) do thezs[i] := abs(thezs[i]);
                           theMoments.LE90 := Percentile(90,thezs,theMoments.NPts,false);
                           aline := TileHeader + LineHeader(DEM,RefType)  + RealToString(Percent,-12,-2) + ',' + MomentStatsString(theMoments);
                           sl.Add(aLine);
                           inc(Result);
                        end
                        else begin
                           {$If Defined(RecordDEMIXGridCompare)} WriteLineToDebugFile('WriteDifferenceResult failed for ' + LandTypeMask + ' theMoments.NPts=' + IntToStr(theMoments.NPts) ); {$EndIf}
                        end;
                     end;

           begin
               Result := 0;
               WriteDifferenceResult('ALL',WhichStats,DEM,RefDEM,zs^,MomentVar,100);
               WriteDifferenceResult('FLAT',WhichStats,DEM,RefDEM,zsFlat^,FlatMomentVar,(100 * FlatMomentVar.NPts/MomentVar.NPts));
               WriteDifferenceResult('GENTLE',WhichStats,DEM,RefDEM,zsGentle^,GentleMomentVar,(100 * GentleMomentVar.NPts/MomentVar.NPts));
               WriteDifferenceResult('STEEP',WhichStats,DEM,RefDEM,zsSteep^,SteepMomentVar,(100 * SteepMomentVar.NPts/MomentVar.NPts));
               WriteDifferenceResult('CLIFF',WhichStats,DEM,RefDEM,zsCliff^,CliffMomentVar,(100 * CliffMomentVar.NPts/MomentVar.NPts));

               if (LandCoverGrid <> 0) then begin
                  WriteDifferenceResult('FOREST',WhichStats,DEM,RefDEM,zsForest^,ForestMomentVar,(100 *ForestMomentVar.NPts/MomentVar.NPts));
                  WriteDifferenceResult('URBAN',WhichStats,DEM,RefDEM,zsUrban^,UrbanMomentVar,(100 * UrbanMomentVar.NPts/MomentVar.NPts));
                  WriteDifferenceResult('BARREN',WhichStats,DEM,RefDEM,zsBarren^,BarrenMomentVar,(100 * BarrenMomentVar.NPts/MomentVar.NPts));
               end;
               LandTypeMask := 'ALL';
           end;


           procedure CompareDifferencesToReferenceDEM(DEM,RefDEM : integer; RefType : shortstring);
           //do full DEM pixel by pixel comparison to reference DEM
           var
              Ruff1,Ruff2,z,Difference : float32;
              NumRuff,NumSlope,NumElev,
              xg,yg,Col,Row : integer;
              TStr : shortstring;
            begin
               if (RefDEM = 0) then begin
                  {$If Defined(RecordFullDEMIX) or Defined(RecordDEMIXGridCompare)} writeLineToDebugFile('Fail (RefDEM=0), comparison to reference, ' + DEMGLB[DEM].AreaName); {$EndIf}
               end
               else begin
                  {$If Defined(RecordFullDEMIX) or Defined(RecordDEMIXGridCompare)} writeLineToDebugFile('Comparison to reference, ' + DEMGLB[DEM].AreaName); {$EndIf}
                  bbgrid := GridBoundingBox(DEM,RefDEM,true);

                  NumElev := 0;
                  NumSlope := 0;
                  NumRuff := 0;

                  ZeroMomentVar;
                  {$IfDef RecordFullDEMIX} WriteTileDetails(bbgrid); {$EndIf}
                  for Col := bbgrid.xgridlow to bbgrid.xgridhigh do begin
                     for Row := bbgrid.ygridlow to bbgrid.ygridhigh do begin
                        if DEMGlb[DEM].GetElevMetersOnGrid(col,row,z) then begin
                           DEMGlb[DEM].DEMGridToLatLongDegree(col,row,lat,long);
                           DEMGlb[RefDEM].LatLongDegreeToDEMGridInteger(Lat,Long,xg,yg);
                           if DEMGlb[RefDEM].GetElevMetersOnGrid(xg,yg,zref) then begin
                              Difference := z-zref;
                              if DEMGlb[RefDEM].GetSlopeAndAspect(xg,yg,SlopeAsp) then begin
                                 LandTypeDiff(Difference);
                              end;
                              LandCover(Lat,long,Difference);
                           end;
                        end;
                     end;
                  end;

                  if (MomentVar.NPts > 0) then begin
                     if ElevDiffHists then begin
                        fName := DEMIX_diff_dist + DEMGLB[DEM].AreaName + '_' + DEMIXtile + '_slope_to_' + RefType  + '.z';
                        ElevFiles.Add(SaveSingleValueSeries(MomentVar.npts,zs^,fName));
                        LegendFiles.Add(ExtractFileNameNoExt(fName));
                     end;
                     NumElev := WriteDifferenceResults(DEM,REFDEM,RefType,ElevDiffStats);
                  end;

                  ZeroMomentVar;
                  for Col := bbgrid.xgridlow to bbgrid.xgridhigh do begin
                     for Row := bbgrid.ygridlow to bbgrid.ygridhigh do begin
                        if DEMGlb[DEM].GetSlopeAndAspect(col,row,SlopeAspectTest) then begin
                           DEMGlb[DEM].DEMGridToLatLongDegree(col,row,lat,long);
                           if DEMGlb[RefDEM].GetSlopeAndAspectFromLatLong(Lat,Long,SlopeAsp) then begin
                              Difference := SlopeAspectTest.SlopePercent - SlopeAsp.SlopePercent;
                              LandTypeDiff(Difference);
                              LandCover(Lat,Long,Difference);
                           end;
                        end;
                     end;
                  end;
                  if (MomentVar.NPts > 1) then begin
                     if ElevDiffHists then begin
                        fName := DEMIX_diff_dist + DEMGLB[DEM].AreaName + '_' + DEMIXtile + '_elev_to_' + RefType + '.z';
                        SaveSingleValueSeries(MomentVar.npts,zs^,fName);
                     end;
                     NumSlope := WriteDifferenceResults(DEM,REFDEM,RefType,SlopeDiffStats);
                  end;

                  ZeroMomentVar;
                  for Col := bbgrid.xgridlow to bbgrid.xgridhigh do begin
                     for Row := bbgrid.ygridlow to bbgrid.ygridhigh do begin
                        if DEMGlb[DEM].RoughnessFromSlopeSTD(Col,Row,MDDef.RoughnessBox,Ruff1) then begin
                           DEMGlb[DEM].DEMGridToLatLongDegree(col,row,lat,long);
                           DEMGlb[RefDEM].LatLongDegreeToDEMGridInteger(Lat,Long,xg,yg);
                           if DEMGlb[RefDEM].RoughnessFromSlopeSTD(xg,yg,MDDef.RoughnessBox,Ruff2) then begin
                              Difference := Ruff1 - Ruff2;
                              if DEMGlb[RefDEM].GetSlopeAndAspect(xg,yg,SlopeAsp) then begin
                                 LandTypeDiff(Difference);
                              end;
                              LandCover(Lat,long,Difference);
                           end;
                        end;
                     end;
                  end;
                  if (MomentVar.NPts > 1) then begin
                     if ElevDiffHists then begin
                        fName := DEMIX_diff_dist + DEMGLB[DEM].AreaName + '_' + DEMIXtile + '_ruff_to_' + RefType + '.z';
                        SaveSingleValueSeries(MomentVar.npts,zs^,fName);
                     end;
                     NumRuff := WriteDifferenceResults(DEM,REFDEM,RefType,RufDiffStats);
                  end;

                  TStr := TestAreaName + '   ' + DEMIXTile + '  ' + RefType + '  elev=' + IntToStr(NumElev) +  '  slope=' + IntToStr(NumSlope) +  '  ruff=' + IntToStr(NumRuff)  +
                        '  total=' + IntToStr(NumElev+NumSlope+NumRuff);
                  wmdem.SetPanelText(3,TStr);
               end;
               {$IfDef RecordFullDEMIX}  WriteLineToDebugFile(TStr); {$EndIf}
            end;


         procedure GetTileStatistics(RefDEM : integer);
         var
           ElevMomentVar,SlopeMomentVar,RoughMomentVar : tMomentVar;
           Col,Row,NPts : integer;
           ForestPC,UrbanPC,BarrenPC : float32;
         begin
            if UseThisTile then begin
               bbgrid := GridBoundingBox(RefDEM,0);
               ZeroMomentVar;
               ElevMomentVar.Npts := 0;
               SlopeMomentVar.Npts := 0;
               RoughMomentVar.Npts := 0;
               NPts := 0;
               DEMGlb[RefDEM].ElevationMomentsWithArray(bbgrid,ElevMomentVar,zs^);
               DEMGlb[RefDEM].SlopeMomentsWithArray(bbgrid,SlopeMomentVar,zs^);

               DEMGlb[RefDEM].GetRoughnessInLongArray(bbgrid,RoughMomentVar.NPts,zs^);
               Moment(zs^,RoughMomentVar,msAll);

               if (LandCoverGrid <> 0) then begin
                  for Col := bbgrid.xgridlow to bbgrid.xgridhigh do begin
                     for Row := bbgrid.ygridlow to bbgrid.ygridhigh do begin
                        DEMGlb[RefDEM].DEMGridToLatLongDegree(Col,Row,Lat,Long);
                        LandCover(Lat,long,Difference);
                        inc(NPts);
                     end;
                  end;
                  ForestPC := 100 * ForestMomentVar.Npts / NPts;
                  UrbanPC := 100 * UrbanMomentVar.Npts / NPts;
                  BarrenPC := 100 * BarrenMomentVar.Npts / NPts;
               end
               else begin
                  ForestPC := -9999;
                  UrbanPC := -9999;
                  BarrenPC := -9999;
               end;

               TileHeader := ShortTestAreaName(TestAreaName) + ',' + DEMIXTile + ',' + RealToString(LatCent,-12,-6) + ',' + RealToString(LongCent,-12,-6) + ',' + RealToString(ElevMomentVar.Mean,-12,-2)   + ','  +
                  RealToString(SlopeMomentVar.Mean,-12,-2)  + ',' + RealToString(RoughMomentVar.Mean,-12,-2)  + ',' +
                  RealToString(ElevMomentVar.Maxz - ElevMomentVar.Minz,-12,-2) + ',' + RealToString(ForestPC,-12,-2)  + ',' + RealToString(UrbanPC,-12,-2) + ',' + RealToString(BarrenPC,-12,-2);
               TileStats.Add(TileHeader);
               TileHeader := TileHeader + ',';
            end;
         end;


   label
      NoLandCover;
   var
      Ser,i,j,UseDSM,UseDTM : integer;
      TStr : shortstring;
      //NoneMissing : boolean;
      LandCoverFName : PathStr;
      DEMIXtileDB2 : integer;
   begin
      {$If Defined(RecordDEMIXFull)} HighlightLineToDebugFile('ProcessDEMIXtestarea in ' + TestAreaName); {$EndIf}

      ReportErrors := false;
      GetDEMIXPaths;
      DEMIXtileDB := 0;
      LandCoverGrid := 0;

      for I := 1 to MaxRefStore do begin
        InitializeMomentVar(SlopeMomentVar[i]);
        InitializeMomentVar(ElevMomentVar[i]);
        NumPits[i] := 0;
        NumPeaks[i] := 0;
      end;

      wmdem.SetPanelText(3,'Load reference DEMs');

      if LoadDEMIXReferenceDEMs(DEMIXRefDEM) then begin
         DEMIXtileDB := DEMIXtileFill(DEMIXRefDEM,DEMGlb[DEMIXRefDEM].DEMBoundBoxGeo,false);
         GISdb[DEMIXtileDB].ApplyGISFilter('GRID_FULL<' + IntToStr(MDDef.DEMIX_Full));
         {$IfDef RecordDEMIX} WriteLineToDebugFile('DTM tiles=' + IntToStr(GISdb[DEMIXtileDB].MyData.FiltRecsInDB)); {$EndIf}
         GISdb[DEMIXtileDB].DeleteAllSelectedRecords;

         if ValidDEM(RefDSMpoint) then begin
            wmdem.SetPanelText(3,'Restrict to tiles in both DSM and DTM');
            DEMIXtileDB2 := DEMIXtileFill(RefDSMpoint,DEMGlb[RefDTMarea].DEMBoundBoxGeo,false);
            GISdb[DEMIXtileDB2].ApplyGISFilter('GRID_FULL<' + IntToStr(MDDef.DEMIX_Full));
            {$IfDef RecordDEMIX} WriteLineToDebugFile('DSM tiles=' + IntToStr(GISdb[DEMIXtileDB2].MyData.FiltRecsInDB)); {$EndIf}
            GISdb[DEMIXtileDB2].DeleteAllSelectedRecords;

            GISdb[DEMIXtileDB].MyData.First;
            while not GISdb[DEMIXtileDB].MyData.eof do begin
               TStr := GISdb[DEMIXtileDB].MyData.GetFieldByNameAsString('NAME');
               GISdb[DEMIXtileDB2].ApplyGISFilter('NAME=' + QuotedStr(TStr));
               if (GISdb[DEMIXtileDB2].MyData.FiltRecsInDB = 0) then begin
                  GISdb[DEMIXtileDB].MyData.Edit;
                  GISdb[DEMIXtileDB].MyData.Delete;
               end
               else GISdb[DEMIXtileDB].MyData.Next;
            end;
            CloseAndNilNumberedDB(DEMIXtileDB2);
            {$IfDef RecordDEMIX} WriteLineToDebugFile('Common tiles=' + IntToStr(GISdb[DEMIXtileDB].MyData.FiltRecsInDB)); {$EndIf}
         end;

         if (GISdb[DEMIXtileDB].MyData.FiltRecsInDB = 0) then begin
            TStr := 'No filled DEMIX tiles on ' + TestAreaName;
            ErrorLog.Add(Tstr);
            {$IfDef RecordDEMIX} WriteLineToDebugFile(TStr); {$EndIf}
         end
         else begin
            {$If Defined(RecordFullDEMIX) or Defined(TrackDEMIX_DEMs)} OpenDEMsToDebugFile('DEMs start loading'); {$EndIf}

            //DoReferenceCriteria;
            GISdb[DEMIXtileDB].MyData.First;  //placed here to see where in the tile processing the program is at

            if LoadDEMIXCandidateDEMs(TestAreaName,DEMIXRefDEM) then begin
               fName := DEMIX_area_lc100 + TestAreaName + '.tif';
               if FileExists(fname) then begin
                  LandCoverGrid := OpenNewDEM(fName);
               end
               else begin
                  Lat := 0.5 * (DEMGlb[DEMIXRefDEM].SelectionMap.MapDraw.MapCorners.BoundBoxGeo.YMax + DEMGlb[DEMIXRefDEM].SelectionMap.MapDraw.MapCorners.BoundBoxGeo.YMin);
                  Long := 0.5 * (DEMGlb[DEMIXRefDEM].SelectionMap.MapDraw.MapCorners.BoundBoxGeo.XMax + DEMGlb[DEMIXRefDEM].SelectionMap.MapDraw.MapCorners.BoundBoxGeo.XMin);
                  LandCoverFName := GetLC100_fileName(Lat,Long);
                  {$IfDef RecordDEMIX} writeLineToDebugFile('Landcover=' + LandCoverfName); {$EndIf}
                  if FileExists(LandCoverFName) then begin
                     LandCoverGrid := GDALsubsetimageandopen(DEMGlb[DEMIXRefDEM].SelectionMap.MapDraw.MapCorners.BoundBoxGeo,true,LandCoverFName);
                     DEMGlb[LandCoverGrid].DEMHeader.ElevUnits := GLCS_LC100;
                     DEMGlb[LandCoverGrid].WriteNewFormatDEM(fName);
                  end
                  else begin
                     TStr := 'No landcover on ' + TestAreaName + ' ' + LandCoverFName;
                     ErrorLog.Add(Tstr);
                     {$IfDef RecordDEMIX} WriteLineToDebugFile(TStr); {$EndIf}
                     goto NoLandCover;
                  end;
               end;
               if ValidDEM(LandCoverGrid) then begin
                  New(zsForest);
                  New(zsUrban);
                  New(zsBarren);
               end;

               new(zs);
               New(zssteep);
               New(zsflat);
               New(zsGentle);
               New(zsCliff);
               {$IfDef RecordDEMIX} writeLineToDebugFile(TestAreaName + '  ProcessDEMIXtestarea in, zs created'); {$EndIf}

               if ElevDiffHists then begin
                  ElevFiles := tStringList.Create;
                  LegendFiles := tStringList.Create;
               end;

               j := 0;
               GISdb[DEMIXtileDB].MyData.First;
               while not GISdb[DEMIXtileDB].MyData.eof do begin
                  HeavyDutyProcessing := true;
                  inc(j);
                  wmdem.SetPanelText(2,'Tile: ' + IntToStr(j) + '/' + IntToStr(GISdb[DEMIXtileDB].MyData.FiltRecsinDB));
                  if UseThisTile then begin
                     GetTileStatistics(DEMIXRefDEM);
                     for i := 1 to MaxTestDEM do begin
                        if ValidDEM(TestDEM[i]) then begin
                           {$If Defined(RecordFullDEMIX) or Defined(RecordDEMIXLoops)}
                              WriteLineToDebugFile(DEMIXTile + ' Start tests for DEM=' + IntToStr(TestDEM[i]) + '/' + IntToStr(Ser) + '  Series=' + TestSeries[i] + '  DEM=' + DEMGlb[TestDEM[i]].AreaName);
                           {$EndIf}
                           GetReferenceDEMsForTestDEM(TestSeries[i],UseDSM,UseDTM);
                           CompareDifferencesToReferenceDEM(TestDEM[i],UseDTM,'DTM');
                           CompareDifferencesToReferenceDEM(TestDEM[i],UseDSM,'DSM');
                           {$If Defined(RecordFullDEMIX)} writeLineToDebugFile('All tests done for ' + TestSeries[i]); {$EndIf}
                        end;
                     end;
                  end;
                  GISdb[DEMIXtileDB].MyData.Next;
               end;
               {$If Defined(RecordFullDEMIX)} for i := 1 to 2 do writeLineToDebugFile(''); {$EndIf}
               Dispose(zs);
               Dispose(zssteep);
               Dispose(zsflat);
               Dispose(zsGentle);
               Dispose(zsCliff);
               if ValidDEM(LandCoverGrid) then begin
                  Dispose(zsForest);
                  Dispose(zsUrban);
                  Dispose(zsBarren);
               end;
               if ElevDiffHists then begin
                  ElevFiles.Destroy;
                  LegendFiles.Destroy;
               end;
            end
            else begin
               TStr := 'Missing reference DEM on ' + TestAreaName;
               ErrorLog.Add(Tstr);
               {$IfDef RecordDEMIX} WriteLineToDebugFile(TStr); {$EndIf}
            end;
            {$If Defined(RecordFullDEMIX)} writeLineToDebugFile('call CloseAndNilNumberedDB(DEMIXtileDB)'); {$EndIf}
            CloseAndNilNumberedDB(DEMIXtileDB);
            NoLandCover:;
         end;
      end
      else begin
         {$If Defined(RecordDEMIX)} HighlightLineToDebugFile(TestAreaName + ',  did not load reference data'); {$EndIf}
      end;
      ReportErrors := true;
      {$If Defined(RecordFullDEMIX) or Defined(TrackDEMIX_DEMs)} OpenDEMsToDebugFile('closing all DEMs'); {$EndIf}
      CloseAllDEMs;
    end;


var
   DataPath : pathStr;
   i : integer;
   DefaultFilter : byte;
   TStr : shortstring;
   aTable : tMyData;
begin
   {$IfDef RecordDEMIX} writeLineToDebugFile('Start ComputeDEMIXstats'); {$EndIf}
   try
      GetDEMIXpaths;
      ErrorLog := tStringList.Create;
      SaveBackupDefaults;
      MDdef.ConfirmDBEdits := false;
      MDdef.DefaultMapXSize := 800;
      MDdef.DefaultMapYSize := 800;

      //settings that can be changed
      ElevDiffHists := true;
      DoHorizontalShift := false;

      MDDef.DEMIX_Full := 75;
      MDDef.SlopeFlatBoundary := 12.5;
      MDDef.SlopeGentleBoundary := 25;
      MDDef.SlopeSteepBoundary := 50;
      MDDef.LandTypePointsNeeded := 100;
      LandTypeMask := 'ALL';

      if DoHorizontalShift then SafeMakeDir(DEMIXresultsDir + 'lags\' );
      if ElevDiffHists then begin
         SafeMakeDir(DEMIX_diff_dist + 'elev_diff_hists\' );
         SafeMakeDir(DEMIX_diff_dist + 'slope_diff_hists\' );
         SafeMakeDir(DEMIX_diff_dist + 'ruff_diff_hists\' );
      end;

      FilesWanted := DEMIX_AreasWanted;

      {$IfDef RecordDEMIX} writeLineToDebugFile('Areas to process=' + IntToStr(FilesWanted.Count)); {$EndIf}

      for i := 0 to pred(FilesWanted.Count) do begin
         wmdem.SetPanelText(1,'Area: ' + IntToStr(succ(i)) + '/' + IntToStr(FilesWanted.Count));
         cFName := FilesWanted.Strings[i];
         if StrUtils.AnsiContainsText(UpperCase(cfName),'_DSM') then begin
            //this will be the done same time as its DTM
         end
         else begin
            TestAreaName := ExtractFileNameNoExt(cfName);

            if FileExists(DEMIXresultsDir +'Elev_diff_stats_' + TestAreaName + '.csv')  then begin
               {$IfDef RecordDEMIX} writeLineToDebugFile(TestAreaName + ' already processed'); {$EndIf}
            end
            else begin
               if StrUtils.AnsiContainsText(TestAreaName,'.dsm') then begin
                  RefDSMPointFName := DEMIX_Ref_1sec + TestAreaName + '_ref_1sec_point.tif';
                  RefDSMareaFName := StringReplace(RefDSMPointFName, 'point', 'area',[rfIgnoreCase]);
                  if not FileExists(RefDSMPointFName) then RefDSMPointFName := '';
                  if not FileExists(RefDSMareaFName) then RefDSMareaFName := '';
               end
               else begin
                  RefDSMPointFName := '';
                  RefDSMareaFName := '';

                  RefDTMPointFName := DEMIX_Ref_1sec + TestAreaName + '_ref_1sec_point.tif';
                  RefDTMareaFName := StringReplace(RefDTMPointFName, 'point', 'area',[rfIgnoreCase]);
                  COPRefDTMFName := StringReplace(RefDSMPointFName, '1sec', '1.5x1sec',[rfIgnoreCase]);
                  COPRefDSMFName := StringReplace(COPRefDTMFName, 'dtm', 'dsm',[rfIgnoreCase]);

                  if not FileExists(RefDTMPointFName) then RefDTMPointFName := '';
                  if not FileExists(RefDTMareaFName) then RefDTMareaFName  := '';
                  if not FileExists(COPRefDTMFName) then COPRefDTMFName := '';
                  if not FileExists(COPRefDSMFName) then COPRefDSMFName := '';
               end;

               if FileExists(RefDTMPointFName) and FileExists(RefDTMareaFName) then begin
                  {$IfDef RecordDEMIX} writeLineToDebugFile('Start process ' + TestAreaName); {$EndIf}
                  InitializeStringLists;
                  ProcessDEMIXtestarea;
                  FinalizeStringLists(false);
               end
               else begin
                  {$IfDef RecordDEMIX} writeLineToDebugFile(TestAreaName + ' missing 1 sec reference DEMs'); {$EndIf}
               end;
            end;
         end;
         //if (i=10) then break;
      end;
      {$IfDef RecordDEMIXFull} WriteLineToDebugFile('All processing done'); {$EndIf}
      FilesWanted.Destroy;
   finally;
      HeavyDutyProcessing := false;
      ShowDefaultCursor;
      RestoreBackupDefaults;
      DisplayAndPurgeStringList(ErrorLog,'DEMIX_VDatum_shifts Problems');
      wmdem.ClearStatusBarPanelText;
   end;
   {$IfDef RecordDEMIX} writeLineToDebugFile('End ComputeDEMIXstats'); {$EndIf}
end;


procedure DEMIX_VDatum_shifts;
//processes any csv files created by VDATUM, which are then converted to dbf files and the DEM shifted in x, y, and z
var
  fName,fName2 : PathStr;
  AreaName,TStr : shortstring;
  Merged,Shifts,ErrorLog : tStringList;
  i,j,db,DEM  : Integer;
  dx,dy,dz : float32;
begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIX_VDatum_shifts in'); {$EndIf}
   try
      GetDEMIXPaths;
      ErrorLog := tStringList.Create;
      Shifts := tStringList.Create;
      FindMatchingFiles(vd_path,'*.csv',Shifts,0);
      for I := 0 to pred(Shifts.Count) do begin
         AreaName := ExtractFileNameNoExt(Shifts.Strings[i]);
         {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Start ' + AreaName); {$EndIf}
         wmdem.SetPanelText(2,'Area: ' + IntToStr(succ(i)) + '/' + IntToStr(Shifts.Count));
         wmdem.SetPanelText(3,AreaName);
         fName := DEMIX_Ref_Merge + AreaName + '.dem';
         if not FileExists(fname) then begin
            ErrorLog.Add('Merged DEM missing: ' + fName);
         end
         else begin
            db := AnalyzeVDatumShift(Shifts.Strings[i],ErrorLog);
            if ValidDB(db) then begin
               dx := GISdb[db].MyData.FieldAverage('X_SHIFT');
               dy := GISdb[db].MyData.FieldAverage('Y_SHIFT');
               dz := GISdb[db].MyData.FieldAverage('VERT_SHIFT');
               if (abs(dx) < 0.01) and (abs(dy) < 0.01) and (abs(dz) < 0.01) then begin
                  ErrorLog.Add(AreaName + ' no change in dx, dy, and dz');
               end
               else begin
                  {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Load ' + fName); {$EndIf}
                  DEM := OpenNewDEM(fName,false);
                  if DEMGlb[DEM].DEMHeader.VerticalCSTypeGeoKey = VertCSEGM2008 then begin
                     {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEM already EGM2008=' + IntToStr(DEMGlb[DEM].DEMHeader.VerticalCSTypeGeoKey)); {$EndIf}
                  end
                  else begin
                     {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEM was ' + IntToStr(DEMGlb[DEM].DEMHeader.VerticalCSTypeGeoKey)); {$EndIf}
                     DEMGlb[DEM].DEMHeader.VerticalCSTypeGeoKey := VertCSEGM2008;
                     DEMGlb[DEM].DEMHeader.DEMSWCornerX := DEMGlb[DEM].DEMHeader.DEMSWCornerX + dx;
                     DEMGlb[DEM].DEMHeader.DEMSWCornerY := DEMGlb[DEM].DEMHeader.DEMSWCornerY + dy;
                     {$If Defined(RecordDEMIX)} WriteLineToDebugFile('For EGM2008 added dz =' + RealToString(dz,-8,-2)); {$EndIf}
                     DEMGlb[DEM].AddConstantToGrid(dz);
                     {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Writing DEM format ' + fName); {$EndIf}
                     DEMGlb[DEM].WriteNewFormatDEM(DEMGlb[DEM].DEMFileName);
                  end;
                  CloseSingleDEM(DEM);
               end;
               CloseAndNilNumberedDB(db);
            end;
         end;
      end;
   finally
       HeavyDutyProcessing := false;
       Shifts.Free;
       wmdem.ClearStatusBarPanelText;
       DisplayAndPurgeStringList(ErrorLog,'DEMIX_VDatum_shifts Problems');
   end;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIX_VDatum_shifts out'); {$EndIf}
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


procedure ConvertASCtoGeotiffDEMs(aPath : PathStr);
var
   ASCIIDEMs : tStringList;
   i,NewDEM : integer;
begin
   ASCIIDEMs := tStringList.Create;
   FindMatchingFiles(aPath,'*.asc',ASCIIDEMs,5);
   if (ASCIIDEMs.Count > 0) then begin
      //convert ASC files to Tiff, there must be a single WKT projection file in the directory
      for I := 0 to pred(ASCIIDEMs.Count) do begin
         {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Convert ASC file= ' + ASCIIDEMs.Strings[i]); {$EndIf}
         NewDEM := OpenNewDEM(ASCIIDEMs.Strings[i],false);
         CloseSingleDEM(NewDEM);
      end;
   end;
   ASCIIDEMs.Free;
end;




procedure DEMIX_merge_source;
var
   Areas,DEMs,ASCIIDEMs,ErrorLog : tStringList;
   i,VDatumCode,Fixed,NewDEM,AnArea,LocalToWGS84,WGS84toEGM2008 : integer;
   AreaMergeName, fName, AreaPath,
   VDatumListFName,VDatumFilesFName : PathStr;
   AreaName,TStr : shortstring;
   VDatumList,VDatumFiles : tMyData;
begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIX_merge_source in'); {$EndIf}
   try
      GetDEMIXPaths(true);
      ErrorLog := tStringList.Create;
      Areas := DEMIX_AreasWanted;
      if (Areas.Count = 0) then begin
         {$If Defined(RecordDEMIX)} WriteLineToDebugFile('No areas selected'); {$EndIf}
      end
      else begin
         LocalToWGS84 := 0;
         WGS84toEGM2008 := 0;
         VDatumListFName := DEMIX_Ref_Source + 'demix_area_vert_datums.dbf';
         VDatumFilesFName := DEMIX_Ref_Source + 'vert_datum_files.dbf';
         VDatumList := tMyData.Create(VDatumListFName);
         VDatumFiles := tMyData.Create(VDatumFilesFName);
         for anArea := 0 to pred(Areas.Count) do begin
            wmdem.SetPanelText(2, 'Area: ' + IntToStr(succ(anArea)) + '/' + IntToStr(Areas.Count));
            AreaName := Areas.Strings[AnArea];
            AreaPath := DEMIX_Ref_Source + AreaName;
            wmdem.SetPanelText(3, AreaName);
            AreaMergeName := DEMIX_Ref_Merge + AreaName + '.dem';
            if FileExists(AreaMergeName) then begin
               {$If Defined(RecordDEMIX)} WriteLineToDebugFile('File Existed ' + AreaMergeName); {$EndIf}
            end
            else begin
               VDatumList.ApplyFilter('AREA=' + QuotedStr(AreaName));
               if (VDatumList.FiltRecsInDB = 1) then begin
                  VDatumCode := VDatumList.GetFieldByNameAsInteger('VERT_CS');
               end
               else begin
                  VDatumCode := 0;
               end;

               wmdem.SetPanelText(3, 'Convert ASC to Geotiff');
               ConvertASCtoGeotiffDEMs(Areas.Strings[AnArea]);

               if (VDatumCode = 0) then begin
                  ErrorLog.Add(AreaName + ' undefined VDatumCode');
               end
               else begin
                  DEMs := Nil;
                  FindMatchingFiles(AreaPath,'*.tif',DEMs,5);
                  if (DEMs.Count > 0) then begin
                     wmdem.SetPanelText(3, 'Merge DEMs=' + IntToStr(DEMs.Count));
                     {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Merge files= ' + IntToStr(DEMs.Count) + ' for ' + AreaMergeName); {$EndIf}
                     if DEMs.Count = 1 then NewDEM := OpenNewDEM(DEMs.Strings[0],false)
                     else NewDEM := MergeMultipleDEMsHere(DEMs,false,false);  //Frees DEMs
                     {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Merge files over, MinZ=' + RealToString(DEMGlb[NewDEM].DEMheader.MinElev,-12,-2)); {$EndIf}
                     if (abs(DEMGlb[NewDEM].DEMheader.MinElev) < 0.001) then begin
                        //mark sea level as missing for analysis along coast
                        DEMGlb[NewDEM].MarkInRangeMissing(-0.001,0.001,Fixed);
                        {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Sea level missing done, pts removed=' + IntToStr(Fixed)); {$EndIf}
                     end;
                     DEMGlb[NewDEM].DEMheader.VerticalCSTypeGeoKey := VDatumCode;

                     if (VDatumCode = VertCSNAVD88)  then begin
                        DEMGlb[NewDEM].CSVforVDatum(vd_path + AreaName + '.csv');
                        TStr := AreaName + ' VDatumCSV created for NAVD88; run through NOAA VDATUM';
                        {$If Defined(RecordDEMIX)} WriteLineToDebugFile(TStr); {$EndIf}
                        ErrorLog.Add(TStr);
                     end
                     else if (VDatumCode <> VertCSEGM2008) then begin
                        //shift to EGM2008
                        VDatumFiles.ApplyFilter('VERT_CS=' + IntToStr(VDatumCode));
                        if (VDatumFiles.FiltRecsInDB = 1) then begin
                           wmdem.SetPanelText(3, 'Vertical datum shift');
                           GeoidWGS84ellipsoidToLocalVDatum := VDatumFiles.GetFieldByNameAsString('VERT_SUB');
                           //FindDriveWithFile(GeoidWGS84ellipsoidToLocalVDatum);
                           LoadDatumShiftGrids(LocalToWGS84,WGS84toEGM2008);
                           DEMGlb[NewDEM].MoveToEGM2008(WGS84toEGM2008,LocalToWGS84);
                           {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Shifted to EGM2008 using ' + DEMGLB[LocalToWGS84].AreaName + ' and ' + DEMGLB[WGS84toEGM2008].AreaName); {$EndIf}
                           CloseSingleDEM(LocalToWGS84);
                        end;
                     end;
                     ShowHourglassCursor;
                     wmdem.SetPanelText(3, 'Write DEM ' + AreaMergeName);
                     DEMGlb[NewDEM].CheckMaxMinElev;
                     DEMGlb[NewDEM].WriteNewFormatDEM(AreaMergeName);
                     {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Merge saved to ' + AreaMergeName); {$EndIf}
                     CloseSingleDEM(NewDEM);
                     CleanUpTempDirectory;  //might be many tiled or compressed DEMs expanded
                  end
                  else begin
                     TStr := 'No TIF files found in ' + AreaPath;
                     {$If Defined(RecordDEMIX)} WriteLineToDebugFile(TStr); {$EndIf}
                     ErrorLog.Add(TStr);
                  end;
               end;
            end;
         end {for area};
         VDatumList.Free;
         VDatumFiles.Free;
      end;
      Areas.Free;
   finally
      HeavyDutyProcessing := false;
      wmdem.ClearStatusBarPanelText;
      CloseSingleDEM(WGS84toEGM2008);
      DisplayAndPurgeStringList(ErrorLog,'DEMIX_merge_source Problems');
   end;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIX_merge_source out'); {$EndIf}
end;


procedure DEMIX_CreateReferenceDEMs;
var
   fName : PathStr;
   ErrorLog,Areas : tStringList;
   i,j,WantedDEM : integer;
   AreaName,TStr : shortstring;
begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXreferenceDEMs in'); {$EndIf}
   try
      GetDEMIXPaths;
      Areas := tStringList.Create;
      ErrorLog := tStringList.Create;
      //FindMatchingFiles(DEMIX_Ref_Merge,'*.dem',Areas,0);
      Areas := DEMIX_AreasWanted;

      if (Areas.Count > 0) then begin
        for i := 0 to pred(Areas.Count) do begin
           AreaName := Areas.Strings[i];  //(Areas[i]);
           wmdem.SetPanelText(2, 'Area: ' + IntToStr(succ(i)) + '/' + IntToStr(Areas.Count));
           wmdem.SetPanelText(3, AreaName);
           fName := DEMIX_Ref_1sec + AreaName + '_ref_1sec_area.tif';
           if FileExists(fName) then begin
              {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXreferenceDEMs already done ' + fName ); {$EndIf}
           end
           else begin
              {$If Defined(RecordDEMIX)} HighlightLineToDebugFile('DEMIXreferenceDEMs for ' + AreaName); {$EndIf}
              fName := DEMIX_Ref_Merge + AreaName + '.dem';
              WantedDEM := OpenNewDEM(fName,true);   //need to open map to create the subset
              if (DEMGlb[WantedDEM].DEMheader.VerticalCSTypeGeoKey = VertCSNAVD88)  then begin
                 TStr := DEMGlb[WantedDEM].AreaName  + ' is still NAVD88; run VDATUM conversion';
                 {$If Defined(RecordDEMIX)} WriteLineToDebugFile(TStr); {$EndIf}
                 ErrorLog.Add(TStr);
              end
              else if (DEMGlb[WantedDEM].DEMheader.VerticalCSTypeGeoKey <> VertCSEGM2008) then begin
                 TStr := DEMGlb[WantedDEM].AreaName  + ' not EGM2008; ref 1" DEMs not created; Re-Merge Code=' + IntToStr(DEMGlb[WantedDEM].DEMheader.VerticalCSTypeGeoKey);
                 {$If Defined(RecordDEMIX)} WriteLineToDebugFile(TStr); {$EndIf}
                 ErrorLog.Add(TStr);
                 DeleteFileIfExists(Areas[i]);
              end
              else begin
                 {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Call ResampleForDEMIXOneSecDEMs ' + AreaName); {$EndIf}
                 ResampleForDEMIXOneSecDEMs(WantedDEM,DEMIX_Ref_1sec,false);
              end;
              CloseSingleDEM(WantedDEM);
           end;
        end;
      end
      else begin
          {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXreferenceDEMs, no areas ' + DEMIX_Ref_Merge); {$EndIf}
      end;
   finally
      Areas.Free;
      HeavyDutyProcessing := false;
      wmdem.ClearStatusBarPanelText;
      DisplayAndPurgeStringList(ErrorLog,'DEMIX_CreateReferenceDEMs Problems');
   end;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXreferenceDEMs out'); {$EndIf}
end;




procedure DEMIXisCOPorALOSbetter(DBonTable : integer);
var
   which,RefFilter : shortstring;
   Compare,i,j,ties,opinions,db : integer;
   fName : PathStr;
   Findings,Criteria,DEMs : tStringList;


   procedure DoOne(Header,theFilter : shortstring);
   var
      Total,Cop,ALOS,Ties,FAB,dem : integer;
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
            ties := GISdb[DBonTable].MyData.FiltRecsInDB;
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
            for j := 1 to MaxLandType do begin
               DoOne(RefDEMType[i] + ' ' + LandType[j] + ' pixels','LAND_TYPE=' + QuotedStr(LandType[j]) + RefFilter);
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




procedure DEMIX_graph_best_in_Tile(DBonTable : integer; SortByArea : boolean);
var
   RefFilter : shortstring;
   CriteriaTable : tMyData;
   BigBMPFiles : tStringList;
   fName,TileSortName : PathStr;
   ch : ANSIchar;


    procedure OneLoop;
    const
       InterAreaSkip = 2;
    var
       NumAreas,NumTiles : integer;
       TileList,AreaList : tStringList;
       i,j,k,CurrentY,DEM,Center,NumTies : integer;
       Criterion,Best,TStr,AreaFilter : shortstring;
       aDEM : array[1..10] of shortstring;
       Graph : tThisBaseGraph;
       rfile : array[1..10] of file;
       v : array[1..2] of float32;
       Symbol : tFullSymbolDeclaration;
       fName : PathStr;
       bmp : tMyBitmap;
       TieTolerance : float32;

                procedure LocateBest(Best : shortstring; Center : float32);
                var
                   DEM : integer;
                begin
                    for DEM := 1 to NumDEMIXDEM do begin
                       if (Best = DEMIXDEMTypeName[DEM]) then begin
                          v[2] := CurrentY;
                          v[1] := Center;
                          BlockWrite(rfile[DEM],v,1);
                       end;
                    end;
                end;


    begin
       {$If Defined(RecordDEMIX)} WriteLineToDebugFile('One loop in, ' + RefFilter); {$EndIf}
       ShowHourglassCursor;
       GISdb[DBonTable].ApplyGISFilter(RefFilter);
       GISdb[DBonTable].EmpSource.Enabled := false;
       NumTiles := GISdb[DBonTable].MyData.NumUniqueEntriesInDB('DEMIX_TILE');
       GISdb[DBonTable].EmpSource.Enabled := false;

       AreaList := tStringList.Create;
       if SortByArea then begin
          NumAreas := GISdb[DBonTable].MyData.NumUniqueEntriesInDB('AREA');
          GISdb[DBonTable].EmpSource.Enabled := false;
          fName := DEMIXSettingsDir +  'demix_areas_sorted_by_lat.txt';
          AreaList.LoadFromFile(fName);
       end
       else begin
          AreaList.Add('by sort');
          fName := TileSortName;
          TileList := tStringList.Create;
          TileList.LoadFromFile(fName);
          AreaFilter := '';
          NumAreas := 1;
       end;

       MDDef.DefaultGraphXSize := 1200;
       MDDef.DefaultGraphYSize := NumTiles * 20 + 50;

      Graph := tThisBaseGraph.Create(Application);

      Graph.GraphDraw.LegendList := tStringList.Create;
      for DEM := 1 to NumDEMIXDEM do begin
         Symbol := SymbolFromDEMName(DEMIXDEMTypeName[DEM]);
         Symbol.DrawingSymbol := FilledBox;
         Graph.OpenPointFile(rfile[DEM],Symbol);
         Graph.GraphDraw.LegendList.Add(DEMIXDEMTypeName[DEM]);
      end;

      Graph.GraphDraw.TopLabel := RefFilter  + '  n=' + IntToStr(NumTiles);
      Graph.GraphDraw.HorizLabel := '';
      Graph.Caption := Graph.GraphDraw.TopLabel;
      Graph.GraphDraw.GraphAxes := NoGrid;
      Graph.GraphDraw.MinHorizAxis := 0;
      Graph.GraphDraw.MaxHorizAxis := 20;
      Graph.GraphDraw.MaxVertAxis := NumTiles + InterAreaSkip * NumAreas;
      Graph.GraphDraw.GraphLeftLabels := tStringList.Create;
      Graph.GraphDraw.GraphBottomLabels := tStringList.Create;
      Graph.GraphDraw.TopLeftLabel := ExtractFileNameNoExt(fName);

      CriteriaTable.ApplyFilter('');
      while not CriteriaTable.eof do begin
         TStr := CriteriaTable.GetFieldByNameAsString('CRITERION');
         TieTolerance := CriterionTieTolerance(TStr);
         TStr := IntToStr(CriteriaTable.GetFieldByNameAsInteger('AXIS_VALUE')) + ',' + TStr + ' (' + RealToString(TieTolerance,-8,-2) + ')';
         Graph.GraphDraw.GraphBottomLabels.Add(TStr);
         CriteriaTable.next;
      end;

       CurrentY := NumTiles + InterAreaSkip * NumAreas;

       for i := 0 to pred(AreaList.Count) do begin
          {$If Defined(RecordDetailedDEMIX)} WriteLineToDebugFile(AreaList.Strings[i]); {$EndIf}
          WMDEM.SetPanelText(0,IntToStr(i) + '/' + IntToStr(AreaList.Count) );
          WMDEM.SetPanelText(1,RefFilter);
          if SortByArea then AreaFilter := ' AND AREA=' + QuotedStr(AreaList.Strings[i]);
          GISdb[DBonTable].ApplyGISFilter(RefFilter + AreaFilter);
          TileList := GISdb[DBonTable].MyData.UniqueEntriesInDB('DEMIX_TILE');

          if SortByArea and (TileList.Count > 0) then begin
             TStr := IntToStr(CurrentY-TileList.Count div 2) + ',' + GISdb[DBonTable].MyData.GetFieldByNameAsString('AREA');
             Graph.GraphDraw.GraphLeftLabels.Add(TStr);
          end;

          for j := 0 to pred(TileList.Count) do begin
             dec(CurrentY);
             if not SortByArea then begin
                TStr := IntToStr(CurrentY) + ',' + TileList.Strings[j];
                Graph.GraphDraw.GraphLeftLabels.Add(TStr);
                {$If Defined(RecordDetailedDEMIX)} WriteLineToDebugFile('Left label=' + TStr); {$EndIf}
             end;

             TStr := RefFilter + AreaFilter + ' AND DEMIX_TILE=' + QuotedStr(TileList.Strings[j]);
             GISdb[DBonTable].ApplyGISFilter(TStr);
             {$If Defined(RecordDetailedDEMIX)} WriteLineToDebugFile(TStr); {$EndIf}
             GISdb[DBonTable].EmpSource.Enabled := false;
             while not GISdb[DBonTable].MyData.eof do begin
                Criterion := GISdb[DBonTable].MyData.GetFieldByNameAsString('CRITERION');
                Best := uppercase(GISdb[DBonTable].MyData.GetFieldByNameAsString('DEM_LOW_SC'));
                {$If Defined(RecordDetailedDEMIX)} WriteLineToDebugFile(Criterion + '   ' + Best); {$EndIf}

                CriteriaTable.ApplyFilter('CRITERION=' + QuotedStr(Criterion));
                if (CriteriaTable.FiltRecsInDB = 1) then begin
                   Center := CriteriaTable.GetFieldByNameAsInteger('AXIS_VALUE');
                   ch := 'X';
                   if StrUtils.AnsiContainsText(Best,',') then ch := ',';
                   if StrUtils.AnsiContainsText(Best,';') then ch := ';';
                   if StrUtils.AnsiContainsText(Best,'-') then ch := '-';

                   if (ch in [',',';','-']) then begin //this is a tie for best
                      NumTies := 0;
                      repeat
                         inc(NumTies);
                         aDEM[NumTies] := BeforeSpecifiedCharacterANSI(Best,ch,true,true);
                      until Best = '';
                      for DEM := 1 to NumTies do begin
                         LocateBest(aDEM[DEM],Center-0.3 + DEM * 0.15);
                      end;
                   end
                   else begin //single best
                      LocateBest(Best,Center);
                   end;
                end;
                GISdb[DBonTable].MyData.Next;
             end;
          end;

          if (TileList.Count > 0) then dec(CurrentY,InterAreaSkip);  //add a blank line between areas, but only if this area had tiles
       end;

       for DEM := 1 to NumDEMIXDEM do begin
          CloseFile(rfile[DEM]);
       end;

       {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Call Graph.AutoScaleAndRedrawDiagram'); {$EndIf}
       Graph.AutoScaleAndRedrawDiagram(false,false,false,false);
       Graph.GraphDraw.BottomMargin := 150;
       Graph.GraphDraw.TopMargin := 50;
       Graph.Height := 1250;
       Graph.GraphDraw.LLlegend := true;
       Graph.GraphDraw.MinVertAxis := CurrentY;
       Graph.RedrawDiagram11Click(Nil);
       fName := NextFileNumber(MDTempDir,'best_dem_graph_','.png');
       SaveImageAsBMP(Graph.Image1,fName);
       BigBMPfiles.Add(fName);
       {$If Defined(RecordDEMIX)} WriteLineToDebugFile('One loop out'); {$EndIf}
    end;


var
   i,j : integer;
begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIX_graph_best_in_Tile in'); {$EndIf}
   try
      SaveBackupDefaults;
      fName := DEMIXSettingsDir + 'demix_criteria_order_with_gaps.dbf';
      CriteriaTable := tMyData.Create(fName);

      if (not SortByArea) then begin
         if (not GetExistingFileName('sorting order for tiles','*.txt',TileSortName)) then exit;
      end;

      for i := 1 to 2 do begin
         BigBMPfiles := tStringList.Create;
         if SortByArea then begin
            for j := 1 to MaxLandType do begin
               RefFilter := 'REF_TYPE=' + QuotedStr(RefDEMType[i]) + ' AND LAND_TYPE=' + QuotedStr(LandType[j]);
               OneLoop;
            end;
         end
         else begin
            RefFilter := 'REF_TYPE=' + QuotedStr(RefDEMType[i]) + ' AND LAND_TYPE=' + QuotedStr('ALL');
            OneLoop;
         end;
         fName := NextFileNumber(MDtempDir,'criteria_by_tile_','.png');
         MakeBigBitmap(BigBMPfiles,'',fName,4);
         DisplayBitmap(fName,'');
      end;
   finally
      CriteriaTable.Destroy;
      GISdb[DBonTable].EmpSource.Enabled := true;
      GISdb[DBonTable].ClearGISFilter;
      WMDEM.SetPanelText(0,'');
      WMDEM.SetPanelText(1,'');
      EndProgress;
      RestoreBackupDefaults;
   end;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIX_graph_best_in_Tile out'); {$EndIf}
end;



function DEMIXwineContestScoresGraph(DBonTable : integer; XScalelabel : shortstring; MinHoriz : float32 = 0.5; MaxHoriz : float32 = 5.5) : tThisBaseGraph;
var
   DEMsPresent : tstringList;
   Symbol : tFullSymbolDeclaration;
   DEM,OnTile : integer;
   rfile : array[1..10] of file;
   v : array[1..2] of float32;


         procedure DoRefType;
         var
            Tile,DEM,SlopeCat : integer;
            TStr : shortstring;
         begin
            GISdb[DBonTable].MyData.First;
            while not GISdb[DBonTable].MyData.eof do begin
               GISdb[DBonTable].EmpSource.Enabled := false;
               TStr := GISdb[DBonTable].MyData.GetFieldByNameAsString('FILTER');
               inc(OnTile);
               if (TStr = 'SKIP') then begin
                  Result.GraphDraw.GraphLeftLabels.Add(IntToStr(OnTile) + ',------------------------');
               end
               else begin
                  Result.GraphDraw.GraphLeftLabels.Add(IntToStr(OnTile) + ',' + TStr);
                  for DEM := 0 to pred(DEMsPresent.Count) do begin
                     v[2] := Ontile;
                     v[1] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(DEMsPresent.Strings[DEM]);
                     BlockWrite(rfile[succ(DEM)],v,1);
                  end;
               end;
               GISdb[DBonTable].MyData.Next;
            end;
         end;

var
   i,x : integer;
   fName : PathStr;
   found : boolean;
begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXwineContestScoresGraph in, table=' + IntToStr(DBontable)); {$EndIf}

   if GISdb[DBonTable].MyData.FieldExists('FILTER') then begin
      SaveBackupDefaults;
      GISdb[DBonTable].EmpSource.Enabled := false;

      DEMsPresent := tStringList.Create;
      DEMsPresent := GISdb[DBonTable].MyData.FieldsInDataBase;
      MDDef.DefaultGraphXSize := 1200;
      MDDef.DefaultGraphYSize := 1400;

      Result := tThisBaseGraph.Create(Application);
      Result.GraphDraw.LegendList := tStringList.Create;
      Result.GraphDraw.HorizLabel := XScaleLabel;
      Result.Caption := GISdb[DBonTable].DBName;

      for DEM := pred(DEMsPresent.Count) downto 0 do begin
         Found := false;
         for i := 1 to NumDEMIXDEM do begin
            if DEMIXDEMTypeName[i] = UpperCase(DEMsPresent.Strings[DEM]) then begin
               found := true;
               {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Found: ' + DEMsPresent.Strings[DEM]); {$EndIf}
            end;
         end;
         if not Found then begin
            {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Not found: ' + DEMsPresent.Strings[DEM]); {$EndIf}
            DEMsPresent.Delete(DEM);
         end;
       end;

      for DEM := pred(DEMsPresent.Count) downto 0 do begin
         for i := 1 to NumDEMIXDEM do begin
            if DEMIXDEMTypeName[i] = UpperCase(DEMsPresent.Strings[DEM]) then begin
               Symbol := SymbolFromDEMName(DEMsPresent.Strings[DEM]);
               Result.OpenPointFile(rfile[succ(DEM)],Symbol);
               Result.GraphDraw.LegendList.Add(DEMsPresent.Strings[DEM]);
            end;
         end;
       end;

      Result.GraphDraw.GraphLeftLabels := tStringList.Create;

      OnTile := 0;

      DoRefType;

      for DEM := 1 to DEMsPresent.Count do begin
         CloseFile(rfile[DEM]);
      end;

      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Call Graph.AutoScaleAndRedrawDiagram, MaxVertAxis should be ' + IntToStr(OnTile)); {$EndIf}
      Result.GraphDraw.GraphAxes := XPartGridOnly;
      Result.GraphDraw.MinHorizAxis := MinHoriz;
      Result.GraphDraw.MaxHorizAxis := MaxHoriz;
      Result.GraphDraw.MinVertAxis := 0;
      Result.GraphDraw.MaxVertAxis := OnTile + 1;
      Result.GraphDraw.ShowHorizAxis0 := true;
      Result.GraphDraw.LeftMargin := 600;

      Result.AutoScaleAndRedrawDiagram(false,false,false,false);
      Result.Height := 1200;
      Result.RedrawDiagram11Click(Nil);
      fName := NextFileNumber(MDTempDir,'best_dem_graph_','.png');
      SaveImageAsBMP(Result.Image1,fName);

      DEMsPresent.Free;
      GISdb[DBonTable].ApplyGISFilter('');
      GISdb[DBonTable].EmpSource.Enabled := true;
      RestoreBackupDefaults;

      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Graphsforwinecontest1Click out, MaxVertAxis=' + RealToString(Result.GraphDraw.MaxVertAxis,-12,-2)); {$EndIf}
   end
   else begin
      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXwineContestScoresGraph in, requires BestDEMSbyCategory'); {$EndIf}
      BestDEMSbyCategory(DBonTable);
   end;
end;




procedure MultipleBestByParameters(DBonTable : integer);
//currently set up to pick parameters in the IDE, and then run
const
   NumCriteria = 3;
   NumParameter = 4;
   Criteria : array[1..NumCriteria] of shortstring = ('ELVD_RMSE','SLPD_MAE','RUFD_RMSE');
   Parameter  : array[1..NumParameter] of shortstring = ('RELIEF','AVG_SLOPE','AVG_ROUGH','FOREST_PC');
(*
   NumCriteria = 6;
   NumParameter = 7;
   //Criteria : array[1..9] of shortstring = ('ELVD_AVD','ELVD_RMSE','ELVD_MAE', 'SLPD_AVD','SLPD_RMSE','SLPD_MAE','RUFD_AVD','RUFD_RMSE','RUFD_MAE');
   Criteria : array[1..NumCriteria] of shortstring = ('ELVD_RMSE','ELVD_MAE', 'SLPD_RMSE','SLPD_MAE','RUFD_RMSE','RUFD_MAE');
   Parameter  : array[1..NumParameter] of shortstring = ('RELIEF','AVG_ELEV','AVG_SLOPE','AVG_ROUGH','FOREST_PC','URBAN_PC','BARREN_PC');
*)
var
    Gr : TThisBaseGraph;
    Movie : tStringList;
    fName : PathStr;
    BigBitmap : tMyBitmap;
    DEMtype : shortstring;
    ap,LeftStart : integer;

    procedure MakeGraph(aP : integer; TheCriteria,TheParameter,TheDEM : shortstring);
    var
       Bitmap : tMyBitmap;
       SourceRect, DestRect : TRect;
       UsefulWidth : integer;
    begin
      Gr := BestByParameterSorting(DBonTable,TheParameter,TheCriteria,theDEM);
      CopyImageToBitmap(gr.Image1,Bitmap);
      fName := MDTempDir + 'frame_' + TheParameter + '_' + TheCriteria + '_' + theDEM + '.bmp';
      Bitmap.SaveToFile(fName);
      if aP = 1 then begin
         UseFulWidth := Bitmap.Width - Gr.GraphDraw.LeftMargin;
         CreateBitmap(BigBitmap,Bitmap.Width + pred(NumCriteria * 2) * (UsefulWidth + 10), Bitmap.Height);
         BigBitmap.Canvas.Draw(0,0,Bitmap);
         LeftStart := Bitmap.Width + 10;
      end
      else begin
         SourceRect := Rect(Gr.GraphDraw.LeftMargin,0,Bitmap.Width,Bitmap.Height);
         DestRect := Rect(LeftStart,0,LeftStart + UsefulWidth,Bitmap.Height);
         BigBitmap.Canvas.CopyRect(DestRect,Bitmap.Canvas,SourceRect);
         LeftStart := LeftStart + UsefulWidth + 10;
      end;
      Movie.Add(fName);
      Gr.Destroy;
      Bitmap.Free;
    end;

var
   aCriteria,aParameter,k : integer;
begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('MultipleBestByParameters in, table=' + IntToStr(DBontable)); {$EndIf}
      for aParameter := 1 to NumParameter do begin
         Movie := tStringList.Create;
         ap := 1;
         for aCriteria := 1 to NumCriteria do begin
            for k := 1 to 2 do begin
               MakeGraph(ap,Criteria[aCriteria],Parameter[aParameter],RefDEMType[k]);
               inc(ap);
            end;
         end;
         fName := MDTempDir + Parameter[aParameter] + '_DSM_and_DTM_criteria.bmp';

         MakeBigBitmap(Movie,'DSM (blue) and DTM (tan)',fName,Movie.Count);
         DeleteMultipleFiles(MDTempDir,'frame_*.bmp');

         BigBitmap.Width := LeftStart;
         DisplayBitmap(BigBitmap);
         BigBitmap.Free;
         {$If Defined(RecordDEMIX)} WriteLineToDebugFile('MultipleBestByParameters done graph=' + IntToStr(aParameter)); {$EndIf}
      end;


(*
   for k := 1 to 2 do begin
      DEMtype := RefDEMType[k];
      for aParameter := 1 to 7 do begin
         Movie := tStringList.Create;
         for aCriteria := 1 to 9 do begin
            MakeGraph(aCriteria,aParameter);
         end;
         fName := MDTempDir + Parameter[aParameter] + '_' + DEMtype + '_criteria.bmp';
         MakeBigBitmap(Movie,DEMType,fName,Movie.Count);
      end;
   end;
*)
(*
   for k := 1 to 2 do begin
      DEMtype := RefDEMType[k];
      for aCriteria := 1 to 9 do begin
         Movie := tStringList.Create;
         for aParameter := 1 to 7 do begin
            MakeGraph(aCriteria,aParameter);
         end;
         fName := MDTempDir + Criteria[aCriteria]  + '_' + DEMtype + '_parameters.bmp';
         MakeBigBitmap(Movie,DEMtype,fName,Movie.Count);
      end;
   end;
*)
   GISdb[DBonTable].ClearGISFilter;
   GISdb[DBonTable].ShowStatus;
end;



function BestByParameterSorting(DBonTable : integer; TileParam,Criterion,DEMtype : shortstring) : tThisBaseGraph;
var
   TileValue : float32;
   rfile: file;
   i : integer;
   v : array[1..3] of float32;
   Winners : shortstring;
begin
    GISdb[DBonTable].ApplyGISFilter('REF_TYPE=' + QuotedStr(DEMType) + ' AND LAND_TYPE=' + QuotedStr('ALL') + ' AND CRITERION=' + QuotedStr(Criterion));
    {$If Defined(RecordDEMIX)} WriteLineToDebugFile('BestByParameterSorting, param=' + TileParam + '  filter=' + GISdb[DBonTable].MyData.Filter); {$EndIf}
    GISdb[DBonTable].EmpSource.Enabled := false;
    Result := tThisBaseGraph.Create(Application);
    Result.GraphDraw.VertLabel := RemoveUnderscores({GISdb[DBonTable].MyData.Filter + '  ' +} TileParam);
    Result.GraphDraw.HorizLabel := Criterion;
    Result.Caption := GISdb[DBonTable].DBName;
    Result.OpenXYColorFile(rfile);

    if DEMtype = 'DSM' then Result.GraphDraw.GraphBackgroundColor := RGB(219,236,237)
    else Result.GraphDraw.GraphBackgroundColor := RGB(237,237,221);

    while not GISdb[DBonTable].MyData.eof do begin
       TileValue := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(TileParam);
       Winners := GISdb[DBonTable].MyData.GetFieldByNameAsString('DEM_LOW_SC');
       for i := 1 to NumDEMIXDEM do begin
          if StrUtils.AnsiContainsText(Winners,DEMIXDEMTypeName[i]) then begin
             v[3] := ConvertPlatformColorToTColor(DEMIXColorFromDEMName(DEMIXDEMTypeName[i]));
             v[2] := TileValue;
             v[1] := i;
             BlockWrite(rfile,v,1);
          end;
       end;
       GISdb[DBonTable].MyData.Next;
    end;
    CloseFile(rfile);
    Result.GraphDraw.MinHorizAxis := 0.5;
    Result.GraphDraw.MaxHorizAxis := 6.5;
    Result.GraphDraw.GraphAxes := YFullGridOnly;
    Result.AutoScaleAndRedrawDiagram(true,false);
    Result.Height := 1200;
    Result.Width := 250;
    Result.GraphDraw.MinVertAxis := 0;
    Result.RedrawDiagram11Click(Nil);
end;



procedure WinsAndTies(DBonTable : integer);
const
    nDEMs = 7;
    nT = 6;
    nParams = 4;
    TheDEMs : array[1..NDEMs] of shortstring = ('TIES','ALOS_TIE','COP_TIE','FABDEM_TIE','NASA_TIE','SRTM_TIE','ASTER_TIE');
    TheParams : array[1..NParams] of shortstring = ('*','ELVD','SLPD','RUFD');
var
   Findings : array[1..nDEMs,1..nDEMs] of integer;
   aFilter : shortstring;
   Results : tStringList;
   i,j,k : integer;
begin
   try
      GISdb[DBonTable].EmpSource.Enabled := false;
      for k := 1 to nParams do begin
         for i := 1 to nDEMs do begin
            for j := 2 to nDEMs do begin
               Findings[i,j] := 0;
            end;
         end;
         Results := tStringList.Create;
         aFilter := TheDEMs[1];
         for j := 2 to nDEMs do aFilter :=  aFilter + ',' + TheDEMs[j];
         Results.Add(aFilter);

         for i := 1 to nDEMs do begin
            for j := 1 to nT do begin
               if (k = 1) then aFilter := TheDEMs[i] + '=' + IntToStr(j)
               else aFilter := 'CRIT_CAT=' + QuotedStr(TheParams[k])+ ' AND ' + TheDEMs[i] + '=' + IntToStr(j);
               GISdb[DBonTable].ApplyGISfilter(aFilter);
               Findings[i,j] := GISdb[DBonTable].MyData.FiltRecsInDB;
            end;
         end;

         Results := tStringList.Create;
         aFilter := 'RANK_' + TheParams[k];
         for j := 1 to nDEMs do aFilter := aFilter + ',' + TheDEMs[j];
         Results.Add(aFilter);
         for j := 1 to nT do begin
            aFilter := IntToStr(j);
            for i := 1 to nDEMs do begin
               aFilter := aFilter + ',' + IntToStr(Findings[i,j]);
            end;
            Results.Add(afilter);
         end;
         Results.SaveToFile(MDTempDir + 'wins_and_ties_' + IntToStr(k) + '.csv');
         Results.Free;
      end;
   finally
      GISdb[DBonTable].ClearGISfilter;
      GISdb[DBonTable].ShowStatus;
   end;
end;




function DEMIXColorFromDEMName(DEMName : shortstring) : tPlatformColor;
begin
   DEMName := UpperCase(DEMName);
   if DEMName = 'TIE' then Result := claBrown
   else if DEMName = 'ALOS' then Result := RGBtrip(230,159,0)
   else if DEMName = 'ASTER' then Result := RGBtrip(0,114,178)
   else if DEMName = 'COP' then Result := RGBtrip(86,180,233)
   else if DEMName = 'FABDEM' then Result := RGBtrip(204,121,167)
   else if DEMName = 'NASA' then Result := RGBtrip(0,158,115)
   else if DEMName = 'SRTM' then Result := RGBtrip(213,94,0);
end;

function SymbolFromDEMName(DEMName : shortstring) : tFullSymbolDeclaration;
var
   DEM : integer;
begin
   Result.Size := 4;
   Result.Color := DEMIXColorFromDEMName(DEMName);
   Result.DrawingSymbol := FilledBox;
end;


procedure GetReferenceDEMsForTestDEM(TestSeries : shortstring; var UseDSM,UseDTM : integer);
begin
   if StrUtils.AnsiContainsText(TestSeries,'ALOS') or StrUtils.AnsiContainsText(TestSeries,'AW3D') then begin
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



procedure DoDEMIX_DifferenceMaps(AreaName,ShortName,LongName : shortString; var Graph1,Graph2 : tThisBaseGraph);
var
   TestGrid,DSMgrid,DTMGrid,DiffGrid,
   i,NoSlopeMap,UseDSM,UseDTM : integer;
   fName : PathStr;
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
         DiffGrid := MakeDifferenceMap(RefGrid,TestGrid,true,false,false);
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

   for I := 1 to MaxTestDEM do begin
      if ValidDEM(TestDEM[i]) then begin
         GetReferenceDEMsForTestDEM(TestSeries[i],UseDSM,UseDTM);

         if (ShortName = 'elvd') then begin
            TestGrid := TestDEM[i];
            DTMGrid := UseDTM;
         end;
         if (ShortName = 'slpd') then begin
            TestGrid := CreateSlopeMap(TestDEM[i]);
            DTMGrid := CreateSlopeMap(UseDTM);
         end;
         if (ShortName = 'rufd') then begin
            TestGrid := CreateRoughnessSlopeStandardDeviationMap(TestDEM[i],3);
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
   end;

   if (ShortName = 'slpd') then begin
      Min := -50;
      Max := 50;
      BinSize := 0.25;
   end;
   if (ShortName = 'rufd') then begin
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




var
   UseDSM,UseDTM,chm,
   RefSlopeMap,RefRuffMap,RefAspectMap,
   COP_ALOS_Diff,CopSlope,CopRuff,ALOSslope,ALOSruff,
   BestCOP_ALOS_elev, BestCOP_ALOS_slope,BestCOP_ALOS_ruff,
   COP_ALOS_DSM4,COP_ALOS_DTM4,COP_ALOS_DSM9,COP_ALOS_DTM9,
   DTMElevDiffMapALOS,DTMElevDiffMapCOP,DTMSlopeDiffMapALOS, DTMSlopeDiffMapCOP,DTMRuffDiffMapALOS,DTMRuffDiffMapCOP : integer;
   HalfSec,AirOrDirt,AirOrDirt2,AirOrDirt3 : array[1..10] of integer;



procedure ZeroDEMs;
var
   i : integer;
begin
   UseDSM := 0;
   UseDTM := 0;
   HalfSecRefDSM := 0;
   HalfSecRefDTM := 0;
   HalfSecALOS := 0;
   HalfSecCOP := 0;
   RefSlopeMap := 0;
   RefRuffMap := 0;
   COP_ALOS_DSM4 := 0;
   COP_ALOS_DTM4 := 0;
   COP_ALOS_DSM9 := 0;
   COP_ALOS_DTM9 := 0;
   COP_ALOS_Diff := 0;

   CopSlope := 0;
   CopRuff := 0;
   ALOSslope := 0;
   ALOSruff := 0;

   BestCOP_ALOS_elev := 0;
   BestCOP_ALOS_slope := 0;
   BestCOP_ALOS_ruff := 0;
   DTMElevDiffMapALOS := 0;
   DTMElevDiffMapCOP := 0;
   DTMSlopeDiffMapALOS := 0;
   DTMSlopeDiffMapCOP := 0;
   DTMRuffDiffMapALOS := 0;
   DTMRuffDiffMapCOP := 0;

   CHM := 0;
   for I := 1 to 10 do begin
      HalfSec[i] := 0;
      AirOrDirt[i] := 0;
      AirOrDirt2[i] := 0;
      AirOrDirt3[i] := 0;
   end;
end;




procedure OpenDEMIXArea(fName : PathStr = '');
//these are currently hard wired in the code, but eventually might be options enabled at run time
const
   DEMIX_Movie = false;
   OverwriteFiles = false;
   DEMIXhistograms = false;
   DEMIX_SaveDEMs = true;
   DEMIX_HalfSecondCompareMaps = true;
   DEMIX_GeomorphMapsBestDEM = true;
   DEMIX_MakeDifferenceMaps = true;
   DEMIX_GeomorphMaps = true;
var
   AreaName : shortstring;
   HalfSec,AirOrDirt,AirOrDirt2,AirOrDirt3 : array[1..10] of integer;
   i,Cols : integer;
   BigMap,Movie : tStringList;
   Graph1,Graph2,Graph3,Graph4 : tThisBaseGraph;
   SaveDir : PathStr;
   Spacing : float32;

                  procedure TryHalfSecCompares;
                  var
                     i : integer;
                  begin
                     if MDDef.DEMIX_DoHalfSecDEMs then begin
                        {$IfDef RecordDEMIX} writeLineToDebugFile('Start creation Half sec dems; Open DEMs=, ' + IntToStr(NumDEMdatasetsOpen)); {$EndIf}
                        for I := 1 to MaxTestDEM do begin
                           if ValidDEM(TestDEM[i]) then begin
                              {$IfDef RecordDEMIX} writeLineToDebugFile(DEMGlb[TestDEM[i]].AreaName + ' Initial DEM=' + IntToStr(TestDEM[i]) + '  ' + DEMGlb[TestDEM[i]].KeyDEMParams(true)); {$EndIf}
                              fName := MDtempDir + DEMGlb[TestDEM[i]].AreaName + '_0.5sec.dem';
                              Spacing := 0.5;
                              HalfSec[i] := DEMGlb[TestDEM[i]].ReinterpolateLatLongDEM(Spacing,fName);
                              //CloseSingleDEM(TestDEM[i]);
                              TestDEM[i] := HalfSec[i];
                              {$IfDef RecordDEMIX} writeLineToDebugFile(DEMGlb[TestDEM[i]].AreaName + ' now TestDEM=' + IntToStr(TestDEM[i])); {$EndIf}
                              CreateDEMSelectionMap(TestDEM[i],true,true,mtIHSReflect);
                              {$IfDef RecordDEMIX} writeLineToDebugFile(DEMGlb[TestDEM[i]].AreaName + ' Half sec DEM=' + IntToStr(TestDEM[i]) + '  ' + DEMGlb[TestDEM[i]].KeyDEMParams(true)); {$EndIf}
                              if MDDef.DEMIX_DoAirOrDirt then begin
                                 AirOrDirt[i] := AirBallDirtBallMap(TestDEM[i],HalfSecRefDSM ,HalfSecRefDTM, DEMGlb[TestDEM[i]].AreaName + '_canopy' ); //checks if point within canopy
                                 AirOrDirt2[i] := AirBallDirtBallMap(TestDEM[i],HalfSecRefDSM ,0,DEMGlb[TestDEM[i]].AreaName + 'air_dirt_dsm');     //compares to DSM
                                 AirOrDirt3[i] := AirBallDirtBallMap(TestDEM[i],0,HalfSecRefDTM,DEMGlb[TestDEM[i]].AreaName + 'air_dirt_dtm');     //compares to DTM
                                 {$IfDef RecordDEMIX} writeLineToDebugFile('air or dirt done, canopy=' + IntToStr(AirOrDirt[i]) + '  dsm=' + IntToStr(AirOrDirt2[i])   + '  dtm=' + IntToStr(AirOrDirt3[i]) ); {$EndIf}
                              end;
                            end;
                        end;
                        {$IfDef RecordDEMIX} writeLineToDebugFile('Done creation Half sec dems; Open DEMs=, ' + IntToStr(NumDEMdatasetsOpen)); {$EndIf}

                        if DEMIX_GeomorphMapsBestDEM then begin
                           {$IfDef RecordDEMIX} writeLineToDebugFile('DEMIX_GeomorphMapsBestDEM begin'); {$EndIf}
                           ALOSslope := -1;   //forces creation of slope and roughness maps
                           ALOSruff := CreateSlopeRoughnessSlopeStandardDeviationMap(TestDEM[1],3,ALOSslope);
                           DEMGlb[ALOSSlope].AreaName := 'alos_slope_%';
                           DEMGlb[ALOSRuff].AreaName := 'alos_roughness_%';

                           COPslope := -1;   //forces creation of slope and roughness maps
                           COPruff := CreateSlopeRoughnessSlopeStandardDeviationMap(TestDEM[2],3,copSlope);
                           DEMGlb[COPSlope].AreaName := 'cop_slope_%';
                           DEMGlb[COPRuff].AreaName := 'cop_roughness_%';

                           BestCOP_ALOS_elev := BestCopOrALOSmap(HalfSecRefDTM,TestDEM[1],TestDEM[2],1.5,'best_dem_elevation');
                           BestCOP_ALOS_slope := BestCopOrALOSmap(RefSlopeMap,ALOSslope,COPslope,1.5,'best_dem_slope');
                           BestCOP_ALOS_ruff := BestCopOrALOSmap(RefRuffMap,ALOSruff,COPruff,1.5,'best_dem_roughness');

                           if DEMIX_MakeDifferenceMaps then begin
                              DTMElevDiffMapALOS := MakeDifferenceMap(HalfSecRefDTM,TestDEM[1],true,false,false);
                              DTMElevDiffMapCOP := MakeDifferenceMap(HalfSecRefDTM,TestDEM[2],true,false,false);
                              DTMSlopeDiffMapALOS := MakeDifferenceMap(RefSlopeMap,ALOSslope,true,false,false);
                              DTMSlopeDiffMapCOP := MakeDifferenceMap(RefSlopeMap,COPSlope,true,false,false);
                              DTMRuffDiffMapALOS := MakeDifferenceMap(RefRuffMap,ALOSRuff,true,false,false);
                              DTMRuffDiffMapCOP := MakeDifferenceMap(RefRuffMap,COPRuff,true,false,false);
                           end;
                            {$IfDef RecordDEMIX} writeLineToDebugFile('DEMIX_GeomorphMapsBestDEM done'); {$EndIf}
                        end;

                        if DEMIX_HalfSecondCompareMaps then begin
                           {$IfDef RecordDEMIX} writeLineToDebugFile('Half sec candidates done, Make difference map'); {$EndIf}
                           COP_ALOS_Diff := MakeDifferenceMap(TestDEM[1],TestDEM[2],true,false,false,'COP-ALOS_difference');

                           {$IfDef RecordDEMIX} writeLineToDebugFile('Try ref DSM COP-ALOS'); {$EndIf}
                           COP_ALOS_DSM4 := TwoDEMHighLowMap(HalfSecRefDSM, TestDEM[1],TestDEM[2],MDDef.DEMIXSimpleTolerance,true,'COP-ALOS_compare_DSM-4');
                           COP_ALOS_DSM9 := TwoDEMHighLowMap(HalfSecRefDSM, TestDEM[1],TestDEM[2],MDDef.DEMIXSimpleTolerance,false,'COP-ALOS_compare_DSM-9');

                           {$IfDef RecordDEMIX} writeLineToDebugFile('Try ref DTM COP-ALOS'); {$EndIf}
                           COP_ALOS_DTM4 := TwoDEMHighLowMap(HalfSecRefDTM, TestDEM[1],TestDEM[2],MDDef.DEMIXSimpleTolerance,true,'COP-ALOS_compare_DTM-4');
                           COP_ALOS_DTM9 := TwoDEMHighLowMap(HalfSecRefDTM, TestDEM[1],TestDEM[2],MDDef.DEMIXSimpleTolerance,false,'COP-ALOS_compare_DTM-9');

                           if DEMIXhistograms then begin
                              HistogramsFromVATDEM(COP_ALOS_DSM4,HalfSecRefDSM,RefSlopeMap,RefRuffMap,RefAspectMap,Graph1,Graph2,Graph3,Graph4);
                              HistogramsFromVATDEM(COP_ALOS_DTM4,HalfSecRefDTM,RefSlopeMap,RefRuffMap,RefAspectMap,Graph1,Graph2,Graph3,Graph4);
                           end;
                        end;
                     end;

                     {$IfDef RecordDEMIX} writeLineToDebugFile('Half sec dems done; Open DEMs=' + IntToStr(NumDEMdatasetsOpen)); {$EndIf}
                  end;



         procedure AddImage(GraphImage : tImage);
         var
           Bitmap : tMyBitmap;
         begin
            if MDDef.DEMIXCompositeImage then begin
               CopyImageToBitmap(GraphImage,Bitmap);
               Bitmap.Height := Bitmap.Height + 25;
               fName := NextFileNumber(MDtempDir,'graph_4_biggie_','.bmp');
               Bitmap.SaveToFile(fName);
               BigMap.Add(fName);
            end;
         end;


         procedure AddFrame(DEM : integer; What : ShortString);
         begin
            if ValidDEM(DEM) then begin
               {$IfDef RecordDEMIXMovies} writeLineToDebugFile('Add frame, dem=' + IntToStr(DEM) + '  ' + DEMGlb[DEM].AreaName + '  ' + DEMGlb[DEM].SelectionMap.MapDraw.FullMapfName); {$EndIf}
               Movie.Add(DEMGlb[DEM].SelectionMap.MapDraw.FullMapfName);
            end
            else begin
               {$IfDef RecordDEMIXMovies} writeLineToDebugFile('Could not find map for DEM=' + IntToStr(DEM) + '  ' + What); {$EndIf}
            end;
         end;

         procedure SaveDEM(DEM : integer; fName : PathStr);
         begin
            if ValidDEM(DEM) then begin
               if OverwriteFiles or (not FileExists(fName)) then begin
                  {$IfDef RecordDEMIXsave} writeLineToDebugFile('Save ' + fName + ' elev units=' + ElevUnitsAre(DEMGlb[DEM].DEMheader.ElevUnits)); {$EndIf}
                  DEMGlb[DEM].WriteNewFormatDEM(fName);
               end;
               fName := ChangeFileExt(fName,'.png');
               SaveImageAsBMP(DEMGlb[DEM].SelectionMap.Image1,fName);
            end;
         end;


begin
   if FileExists(fName) or GetFileFromDirectory('DEMIX area database','*.dbf',fName) then begin
      {$IfDef RecordDEMIX} writeLineToDebugFile('OpenDEMIXArea ' + fName); {$EndIf}
      AreaName := ExtractFileNameNoExt(fName);
      if DEMIX_SaveDEMs then begin
         SaveDir := fName[1] + ':\aa_half_sec_test\' + AreaName + '\';
         //if PathIsValid(SaveDir) then exit;
         SafeMakeDir(SaveDir);
      end;
      try
         HeavyDutyProcessing := true;
         AutoOverwriteDBF := true;
         NakedMapOptions;  //also does SaveBackupDefaults;
         MDDef.ScaleBarLocation.DrawItem := true;
         MDDef.GIFfileLabels := false;
         MDdef.DefaultMapXSize := 1000;
         MDdef.DefaultMapYSize := 1000;
         MDDef.MapNameLocation.DrawItem := true;
         MDDef.MapNameLocation.MapPosition := lpNEmap;
         MDDef.MapNameLocation.DrawItem := true;
         MDDef.GridLegendLocation.DrawItem := true;

         ZeroDEMs;

         if true {LoadDEMIXareaDefinitions(fName)} then begin
            {$IfDef RecordDEMIX} writeLineToDebugFile('LoadDEMIXarea complete'); {$EndIf}
            if LoadDEMIXReferenceDEMs(DEMIXRefDEM) then begin
               {$IfDef RecordDEMIX} writeLineToDebugFile('LoadDEMIXReferenceDEMs complete; Open DEMs=, ' + IntToStr(NumDEMdatasetsOpen) + 'DEMIXRefDEM=' + IntToStr(DEMIXRefDEM)); {$EndIf}

               if MDDef.DEMIX_DoHalfSecDEMs then begin
                  //HalfSecRefDSM := CreateHalfSecRefDEM(RefDSMPoint,RefDSMArea);
                  //HalfSecRefDTM := CreateHalfSecRefDEM(RefDTMPoint,RefDTMArea);
exit;
                  //if FileExists(HalfSecRefDSMfName) then HalfSecRefDSM := OpenNewDEM(HalfSecRefDSMfName) else HalfSecRefDSM := CreateHalfSecRefDEM(RefDSMPoint,RefDSMArea);
                  //if FileExists(HalfSecRefDTMfName) then HalfSecRefDTM := OpenNewDEM(HalfSecRefDTMfName) else HalfSecRefDTM := CreateHalfSecRefDEM(RefDTMPoint,RefDTMArea);
                  DEMIXRefDEM := HalfSecRefDTM;
                  {$IfDef RecordDEMIX} writeLineToDebugFile('Half sec ref dems created; Open DEMs=' + IntToStr(NumDEMdatasetsOpen)); {$EndIf}
               end;

               (*
               if DEMIX_GeomorphMaps or DEMIX_GeomorphMapsBestDEM then begin
                  if FileExists(RuffMapfName) then begin
                     RefSlopeMap := OpenNewDEM(SlopeMapfName);
                     RefRuffMap := OpenNewDEM(RuffMapfName);
                  end
                  else begin
                     RefSlopeMap := -1;   //forces creation of slope and roughness maps
                     RefRuffMap := CreateSlopeRoughnessSlopeStandardDeviationMap(HalfSecRefDTM,3,RefSlopeMap);
                  end;
                  DEMGlb[RefSlopeMap].AreaName := 'ref_dtm_slope_%';
                  DEMGlb[RefRuffMap].AreaName := 'ref_dtm_roughness_%';
                  if FileExists(AspectMapfName) then HalfSecRefDSM := OpenNewDEM(AspectMapfName) else RefAspectMap := MakeAspectMap(HalfSecRefDTM);
               end;
               *)

               {$IfDef RecordDEMIX} writeLineToDebugFile('Slope and Ruff dems created; Open DEMs=' + IntToStr(NumDEMdatasetsOpen)); {$EndIf}

               if (MDDef.DEMIX_DoCHM) then begin
                  if ValidDEM(HalfSecRefDSM) and ValidDEM(HalfSecRefDTM) then begin
                     chm := MakeDifferenceMapOfBoxRegion(HalfSecRefDTM,HalfSecRefDSM,DEMGlb[HalfSecRefDTM].FullDEMGridLimits,true,false,false,AreaName + '_half_sec_chm');
                     //DEMglb[chm].SelectionMap.Elevationpercentiles1Click(Nil);
                     {$IfDef RecordDEMIX} writeLineToDebugFile('CHM created; Open DEMs=, ' + IntToStr(NumDEMdatasetsOpen)); {$EndIf}
                  end;
               end;

               {$IfDef RecordDEMIX} writeLineToDebugFile('Ref dems done; Open DEMs=' + IntToStr(NumDEMdatasetsOpen)); {$EndIf}

               {$IfDef ShowDEMIXWhatsOpen} writeStringListToDebugFile(GetWhatsOpen); {$EndIf}

               if MDDef.DEMIX_DoHalfSecDEMs or MDDef.DEMIX_DoElevParamGraphs or MDDef.DEMIX_DoAirOrDirt or MDDef.DEMIX_DoElevDiff or MDDef.DEMIX_DoSlopeDiff or MDDef.DEMIX_DoRuffDiff or DEMIX_GeomorphMapsBestDEM then begin
                  if LoadDEMIXCandidateDEMs('',DEMIXRefDEM,true,false) then begin
                     {$IfDef RecordDEMIX} HighlightLineToDebugFile('Candidate DEMs loaded'); {$EndIf}
                     {$IfDef ShowDEMIXWhatsOpen} writeLineToDebugFile('candidates loaded');  writeStringListToDebugFile(GetWhatsOpen); {$EndIf}
                     TryHalfSecCompares;
                     if MDDef.DEMIX_DoAirOrDirt then begin
                        if MDDef.DEMIXCompositeImage then begin
                           BigMap := tStringList.Create;
                           BigMap.Add(DEMGlb[RefDTMPoint].SelectionMap.MapDraw.FullMapfName);
                        end;
                        for I := 1 to MaxTestDEM do begin
                           if ValidDEM(TestDEM[i]) then begin
                              GetReferenceDEMsForTestDEM(TestSeries[i],UseDSM,UseDTM);
                              AirOrDirt[i] := AirBallDirtBallMap(TestDEM[i],UseDSM,UseDTM);
                              AirOrDirt2[i] := AirBallDirtBallMap(TestDEM[i],UseDSM,UseDSM);
                              if MDDef.DEMIXCompositeImage then begin
                                 BigMap.Add(DEMGlb[AirOrDirt[i]].SelectionMap.MapDraw.FullMapfName);
                                 if (AirOrDirt2[i] <> 0) then BigMap.Add(DEMGlb[AirOrDirt2[i]].SelectionMap.MapDraw.FullMapfName);
                              end;
                           end;
                        end;
                        DEMGlb[TestDEM[1]].SelectionMap.Allsamepixelsizeasthismap1Click(Nil);
                        if MDDef.DEMIXCompositeImage then MakeBigBitmap(BigMap,'');
                     end;

                     if MDDef.DEMIX_DoElevParamGraphs then ElevationSlopePlot(DEMListForAllOpenDEM);

                     if MDDef.DEMIX_DoElevDiff or MDDef.DEMIX_DoSlopeDiff or MDDef.DEMIX_DoRuffDiff then begin
                        if MDDef.DEMIXCompositeImage then BigMap := tStringList.Create;    //actually this will be graphs, not maps
                        if MDDef.DEMIX_DoElevDiff then begin
                           DoDEMIX_DifferenceMaps(AreaName,'elvd','Elevation',Graph1,Graph2);
                           AddImage(Graph1.Image1);
                           if (Graph2 <> Nil) then AddImage(Graph2.Image1);
                        end;
                        if MDDef.DEMIX_DoSlopeDiff then begin
                           DoDEMIX_DifferenceMaps(AreaName,'slpd','Slope',Graph1,Graph2);
                           AddImage(Graph1.Image1);
                           if (Graph2 <> Nil) then AddImage(Graph2.Image1);
                        end;
                        if MDDef.DEMIX_DoRuffDiff then begin
                           DoDEMIX_DifferenceMaps(AreaName,'rufd','Roughness',Graph1,Graph2);
                           AddImage(Graph1.Image1);
                           if (Graph2 <> Nil) then AddImage(Graph2.Image1);
                        end;
                        if MDDef.DEMIXCompositeImage then begin
                           if (BigMap.Count > 0) then begin
                              fName := NextFileNumber(DEMIXtempFiles,AreaName + '_difference_histograms_','.png');
                              if (RefDSMArea = 0) then Cols := 1 else Cols := 2;
                              MakeBigBitmap(BigMap,'',fName,Cols);
                              DisplayBitmap(fName);
                           end
                           else BigMap.Free;
                        end;
                     end;
                  end;
               end;
            end;
         end;

         {$IfDef ShowDEMIXWhatsOpen} writeStringListToDebugFile(GetWhatsOpen); {$EndIf}

         if DEMIX_Movie then begin
            Movie := tStringList.Create;
            fName := NextFileNumber(MDtempDir,'map_4_movie_','.mov');
            {$IfDef RecordDEMIXMovies} WriteLineToDebugFile('Making movie, ' + fName); {$EndIf}
            AddFrame(HalfSecRefDTM,'Ref DTM');
            AddFrame(HalfSecRefDSM,'Ref DSM');
            for i := 1 to 2 do begin
               AddFrame(AirOrDirt[i],'AirDirt');
               AddFrame(AirOrDirt2[i],'AirDirt2');
               AddFrame(AirOrDirt3[i],'AirDirt3');
            end;

            AddFrame(RefSlopeMap,'Ref slope');
            AddFrame(RefRuffMap,'Ref ruff');
            AddFrame(COP_ALOS_DSM4,'COP_ALOS_DSM4');
            AddFrame(COP_ALOS_DTM4,'COP_ALOS_DTM4');
            AddFrame(COP_ALOS_DSM9,'COP_ALOS_DSM9');
            AddFrame(COP_ALOS_DTM9,'COP_ALOS_DTM9');

            Movie.SaveToFile(fName);
            CreateNewMovie(fName);
            Movie.Free;
         end;

         (*
         if DEMIX_SaveDEMs then begin
            {$IfDef RecordDEMIX} writeLineToDebugFile('OpenDEMIXArea start saving DEMs'); {$EndIf}
            SaveDEM(TestDEM[1],SaveDir + ALOSHalfSecfName);
            SaveDEM(TestDEM[2],SaveDir + CopHalfSecfName);

            SaveDEM(HalfSecRefDSM,SaveDir + HalfSecRefDSMfName);
            SaveDEM(HalfSecRefDTM,SaveDir + HalfSecRefDTMfName);

            if DEMIX_HalfSecondCompareMaps then begin
               SaveDEM(COP_ALOS_Diff,SaveDir + COP_ALOS_DifffName);
               SaveDEM(COP_ALOS_DSM4,SaveDir + COP_ALOS_DSM4fName);
               SaveDEM(COP_ALOS_DTM4,SaveDir + COP_ALOS_DTM4fName);
               SaveDEM(COP_ALOS_DSM9,SaveDir + COP_ALOS_DSM9fName);
               SaveDEM(COP_ALOS_DTM9,SaveDir + COP_ALOS_DTM9fName);
            end;

            if DEMIX_GeomorphMaps then begin
               SaveDEM(RefSlopeMap,SaveDir + SlopeMapfName);
               SaveDEM(RefRuffMap,SaveDir + RuffMapfName);
               SaveDEM(RefAspectMap,SaveDir + AspectMapfName);
            end;

            if DEMIX_GeomorphMapsBestDEM then begin
               SaveDEM(BestCOP_ALOS_slope,SaveDir + BestCOP_ALOS_slopefName);
               SaveDEM(BestCOP_ALOS_ruff,SaveDir + BestCOP_ALOS_rufffName);
               SaveDEM(BestCOP_ALOS_elev,SaveDir + BestCOP_ALOS_elevfName);

               if DEMIX_MakeDifferenceMaps then begin
                  SaveDEM(DTMElevDiffMapALOS,SaveDir + DTMElevDiffMapALOSfName);
                  SaveDEM(DTMElevDiffMapCOP,SaveDir + DTMElevDiffMapCOPfName);
                  SaveDEM(DTMSlopeDiffMapALOS,SaveDir + DTMSlopeDiffMapALOSfName);
                  SaveDEM(DTMSlopeDiffMapCOP,SaveDir +  DTMSlopeDiffMapCOPfName );
                  SaveDEM(DTMRuffDiffMapALOS,SaveDir + DTMRuffDiffMapALOSfName);
                  SaveDEM(DTMRuffDiffMapCOP,SaveDir + DTMRuffDiffMapCOPfName);
               end;
            end;
         end;
         *)
      finally
         HeavyDutyProcessing := false;
         AutoOverwriteDBF := false;
         RestoreBackupDefaults;
         {$IfDef RecordDEMIX} writeLineToDebugFile('OpenDEMIXArea out'); {$EndIf}
      end;
   end;
end;

(*
function LoadDEMIXareaDefinitions(cfName : pathStr = '') : boolean;
var
   fName : PathStr;
   db : integer;
begin
   GeodeticFName := '';
   IceSatFName := '';
   LandCoverFName := '';
   LocalDatumAddFName := '';
   LocalDatumSubFName := '';
   RefDSMPointFName := '';
   RefDSMareaFName := '';
   RefDTMPointFName := '';
   RefDTMareaFName := '';
   COPRefDTMFName := '';
   COPRefDSMFName := '';

   DEMIXRefDEM := 0;
   //AddLocalVDatum := 0;
   //SubLocalVDatum := 0;
   RefDTMpoint := 0;
   RefDTMarea := 0;
   RefDSMpoint := 0;
   RefDSMarea := 0;
   //GeoidGrid := 0;
   COPRefDTM := 0;
   COPRefDSM := 0;

   {$IfDef RecordDEMIX} writeLineToDebugFile('File Read rules ' + cfName); {$EndIf}
   //Rules := tMyData.Create(cfName);
   db := OpenMultipleDataBases('DEMIX area rules',cfName, false);
   Result := GISdb[db].MyData.FieldExists('DATA') and GISdb[db].MyData.FieldExists('FILENAME');
   if Result then begin
      while (not GISdb[db].MyData.eof) do begin
         fName := GISdb[db].MyData.GetFieldByNameAsString('FILENAME');
         if (fName <> '') then begin
            if Not FileExists(fName) then fName[1] := cfName[1];   //fix for external hard drive which moves around
            if UpperCase(GISdb[db].MyData.GetFieldByNameAsString('DATA')) = 'GEODETIC' then GeodeticFName := fName;
            if UpperCase(GISdb[db].MyData.GetFieldByNameAsString('DATA')) = 'ICESAT2' then IceSatFName := fName;
            if UpperCase(GISdb[db].MyData.GetFieldByNameAsString('DATA')) = 'REF_DTM_PIXEL_IS_POINT' then RefDTMPointFName := fName;
            if UpperCase(GISdb[db].MyData.GetFieldByNameAsString('DATA')) = 'REF_DTM_PIXEL_IS_AREA' then RefDTMareaFName := fName;
            if UpperCase(GISdb[db].MyData.GetFieldByNameAsString('DATA')) = 'REF_DSM_PIXEL_IS_POINT' then RefDSMPointFName := fName;
            if UpperCase(GISdb[db].MyData.GetFieldByNameAsString('DATA')) = 'REF_DSM_PIXEL_IS_AREA' then RefDSMareaFName := fName;
            if UpperCase(GISdb[db].MyData.GetFieldByNameAsString('DATA')) = 'COP_REF_DTM' then COPRefDTMFName := fName;
            if UpperCase(GISdb[db].MyData.GetFieldByNameAsString('DATA')) = 'COP_REF_DSM' then COPRefDSMFName := fName;
            if UpperCase(GISdb[db].MyData.GetFieldByNameAsString('DATA')) = 'LOCAL_DATUM_ADD' then LocalDatumAddFName := fName;
            if UpperCase(GISdb[db].MyData.GetFieldByNameAsString('DATA')) = 'LOCAL_DATUM_SUB' then LocalDatumSubFName := fName;
            if UpperCase(GISdb[db].MyData.GetFieldByNameAsString('DATA')) = 'LANDCOVER' then LandCoverFName := fName;
         end;
         GISdb[db].MyData.Next;
      end;
      {$IfDef RecordDEMIXFull} writeLineToDebugFile('ProcessDEMIXtestarea in, rules read ' + ExtractFilePath(fName)); {$EndIf}
   end
   else begin
      MessageToContinue('Invalid file: ' + cfName);
   end;
   CloseAndNilNumberedDB(db);
end;
*)


function LoadDEMIXCandidateDEMs(AreaName : ShortString; RefDEM : integer; OpenMaps : boolean = false; AllCandidates : boolean = true) : boolean;
var
   {$IfDef RecordDEMIX} AllDEMs, {$EndIf}
   WantSeries,ShortName : shortstring;
   IndexSeriesTable : tMyData;
   WantDEM,WantImage,Ser,i,NumPts,GeoidGrid,NewDEM : integer;
   fName : Pathstr;
   Spacing : float32;


         procedure MoveFromEGM96toEGM2008(var DEM : integer);
         //Reproject vertical datum to EGM2008 if required because DEM is EGM96
         //we could not get this to run doing nothing for DEM already on EGM2008
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
   {$If Defined(RecordDEMIXLoad)} writeLineToDebugFile('LoadDEMIXCandidateDEMs in; Open DEMs=, ' + IntToStr(NumDEMdatasetsOpen) + '   RefDEM=' + IntToStr(RefDEM)); {$EndIf}
   Result := false;
   {$IfDef RecordDEMIX} AllDEMs := ''; {$EndIf}
   wmdem.SetPanelText(3,'');

   for I := 1 to MaxTestDEM do begin
      TestDEM[i] := 0;
      TestSeries[i] := '';
   end;

   OpenIndexedSeriesTable(IndexSeriesTable);
   IndexSeriesTable.ApplyFilter('USE=' + QuotedStr('Y'));

   Ser := 0;
   while not IndexSeriesTable.eof do begin
      WantSeries := IndexSeriesTable.GetFieldByNameAsString('SERIES');
      ShortName := IndexSeriesTable.GetFieldByNameAsString('SHORT_NAME');
      fName := DEMIX_test_dems + AreaName + '_' + shortname + '.dem';
      if FileExists(fname) then begin
         wmdem.SetPanelText(3,'Load saved candidate DEM ' + ShortName);
         inc(Ser);
         TestDEM[Ser] := OpenNewDEM(fName);
         TestSeries[Ser] := ShortName;
      end;
      IndexSeriesTable.Next;
   end;
   if (Ser = 6) then begin
      Result := true;
      IndexSeriesTable.Destroy;
      exit;
   end;

   GeoidGrid := OpenNewDEM(GeoidDiffFName,false,'geoid difference from EGM96 to EGM2008');  //to move DEMs from EGM96 to EGM2008
   GeoidDiffFName := DEMGlb[GeoidGrid].DEMFileName;

   //OpenIndexedSeriesTable(IndexSeriesTable);
   IndexSeriesTable.First;
   Ser := 0;
   while not IndexSeriesTable.eof do begin
      WantSeries := IndexSeriesTable.GetFieldByNameAsString('SERIES');
      ShortName := IndexSeriesTable.GetFieldByNameAsString('SHORT_NAME');
      wmdem.SetPanelText(3,'Load candidate DEM ' + ShortName);
      if AllCandidates or (ShortName = 'COP') or (ShortName = 'ALOS') then begin
         {$If Defined(RecordFullDEMIX) or Defined(RecordDEMIXLoad)} writeLineToDebugFile('Try ' + WantSeries + ' ' + ShortName + '  ' + IntToStr(Ser) + '/' + IntToStr(IndexSeriesTable.FiltRecsInDB)); {$EndIf}
         //wmdem.SetPanelText(0,WantSeries);
         {$If Defined(RecordFullDEMIX)} writeLineToDebugFile('Ref DEM=' + DEMGlb[RefDEM].AreaName + '  ' + sfBoundBoxToString(DEMGlb[RefDEM].DEMBoundBoxGeo,6)); {$EndIf}
         if LoadMapLibraryBox(WantDEM,WantImage,true,DEMGlb[RefDEM].DEMBoundBoxGeo,WantSeries,false) and ValidDEM(WantDEM) then begin
            {$If Defined(RecordDEMIXLoad)} writeLineToDebugFile('LoadDEMIXCandidateDEMs done LoadMapLib; Open DEMs=, ' + IntToStr(NumDEMdatasetsOpen)); {$EndIf}
            inc(Ser);
            TestDEM[Ser] := WantDEM;
            TestSeries[Ser] := ShortName;
            {$IfDef RecordDEMIX}
               if not AllOfBoxInAnotherBox(DEMGlb[RefDEM].DEMBoundBoxGeo,DEMGlb[WantDEM].DEMBoundBoxGeo) then begin
                  AllDEMs := AllDEMs + TestSeries[Ser] + ' (partial  ' + sfBoundBoxToString(DEMGlb[RefDEM].DEMBoundBoxGeo) + ')  ';
               end;
            {$EndIf}
            DEMGlb[TestDEM[Ser]].AreaName := TestSeries[Ser];
            DEMGlb[TestDEM[Ser]].DEMFileName := NextFileNumber(MDTempDir, DEMGlb[TestDEM[Ser]].AreaName + '_', '.dem');

            {$IfDef RecordDEMIXLoad} writeLineToDebugFile('Opened:' + WantSeries + '  Open DEMs=' + IntToStr(NumDEMdatasetsOpen)); {$EndIf}
            if (DEMGlb[TestDEM[Ser]].DEMHeader.MinElev < 0.01) then DEMGlb[TestDEM[Ser]].MarkInRangeMissing(0,0,NumPts);
            DEMGlb[TestDEM[Ser]].DEMHeader.VerticalCSTypeGeoKey := IndexSeriesTable.GetFieldByNameAsInteger('VERT_DATUM');
            MoveFromEGM96toEGM2008(TestDEM[Ser]);
            If OpenMaps or (AreaName <> '') then begin
               CreateDEMSelectionMap(TestDEM[Ser],true,false,MDDef.DefDEMMap);
               if ValidDEM(RefDEM) then begin
                  DEMGlb[TestDEM[Ser]].SelectionMap.ClipDEMtoregion(DEMGlb[RefDEM].DEMBoundBoxGeo);
                  {$IfDef RecordDEMIXLoad} writeLineToDebugFile('Clipped:' + WantSeries + '  Open DEMs=' + IntToStr(NumDEMdatasetsOpen)); {$EndIf}
               end;
            end;
            if (AreaName <> '') then begin
               //fName := SaveDEMs + ShortName + '.dem';
               fName := DEMIX_test_dems + AreaName + '_' + shortname + '.dem';
               DEMGlb[TestDEM[Ser]].WriteNewFormatDEM(FName);
            end;
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
   {$IfDef RecordDEMIX} if (AllDEMs <> '') then HighlightLineToDebugFile(' DEM problem, ' + AllDEMs); {$EndIf}
   {$IfDef RecordDEMIXLoad} writeLineToDebugFile('LoadDEMIXCandidateDEMs out; Open DEMs=, ' + IntToStr(NumDEMdatasetsOpen)); {$EndIf}
end;



function LoadDEMIXReferenceDEMs(var RefDEM : integer) : boolean;

         procedure ReferenceFileOpen(var DEM : integer; fName : PathStr);
         begin
            if FileExists(fName) then begin
               DEM := OpenNewDEM(FName);   //must load map for the DEMIX tile computation
               if ValidDEM(DEM) and (RefDEM = 0) then RefDEM := DEM;
            end
            else DEM := 0;
         end;

begin
   RefDEM := 0;
   ReferenceFileOpen(RefDTMpoint,RefDTMpointFName);
   ReferenceFileOpen(RefDTMarea,RefDTMareaFName);
   ReferenceFileOpen(RefDSMpoint,RefDSMpointFName);
   ReferenceFileOpen(RefDSMarea,RefDSMareaFName);
   ReferenceFileOpen(COPRefDTM,COPRefDTMFName);
   ReferenceFileOpen(COPRefDSM,COPRefDSMFName);
   Result := ValidDEM(RefDEM);
   if Result then begin
      {$If Defined(RecordDEMIX)} writeLineToDebugFile('ProcessDEMIXtestarea in, ref DEMs open with RefDEM=' + IntToStr(RefDEM)); {$EndIf}
   end
   else begin
      {$IfDef RecordDEMIX} HighlightLineToDebugFile('Failure, ref DEMs open'); {$EndIf}
   end;
end;



procedure BestDEMSbyCategory(DBonTable : integer);
var
   DEMs,Criteria,Besties,Findings,Findings2,AverageScore,FiltersTooFewOpinions : tStringList;
   i : integer;
   theSum,MaxSum : float64;
   theSums : array[0..10] of float64;
   fName : PathStr;
   aLine : shortstring;


   procedure DoOne(Header,theFilter : shortstring);
   var
      Total,DEM,Ties,ThisCat,Opinions : integer;
   begin
      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DO-ONE  ' + theFilter); {$EndIf}
      WMDEM.SetPanelText(1,theFilter);
      GISdb[DBonTable].ApplyGISFilter(theFilter);
      GISdb[DBonTable].EmpSource.Enabled := false;
      Opinions := GISdb[DBonTable].MyData.FiltRecsInDB;
      if (Opinions >= 10) then begin
         Ties := Opinions;
         Total := Opinions;
         aline := Header + '  (n=' + IntToStr(Opinions) + ')' + ',' + IntToStr(Opinions);
         for DEM := 0 to pred(DEMs.Count) do begin
            GISdb[DBonTable].EmpSource.Enabled := false;
            GISdb[DBonTable].ApplyGISFilter(theFilter + ' AND DEM_LOW_SC=' + QuotedStr(DEMs.Strings[DEM]) );
            ThisCat := GISdb[DBonTable].MyData.FiltRecsInDB;
            aLine := aLine + ',' + IntToStr(ThisCat);
            Ties := Ties - ThisCat;
         end;
         Besties.Add(aLine + ',' + IntToStr(Ties) );

         GISdb[DBonTable].ApplyGISFilter(theFilter);
         MaxSum := 0;
         for DEM := 0 to pred(DEMs.Count) do begin
            GISdb[DBonTable].EmpSource.Enabled := false;
            GISdb[DBonTable].MyData.FieldSum(DEMs.Strings[DEM] + '_SCR');
            theSums[DEM] := GISdb[DBonTable].MyData.FieldSum(DEMs.Strings[DEM] + '_SCR');
            if theSums[DEM] > maxSum then MaxSum := theSums[DEM];
            Findings.Add(Header + ',' + DEMs.Strings[DEM] + ',' + RealToString(theSums[DEM],12,-4));
         end;

         aline := Header + ' (n=' + IntToStr(Opinions) + ')' + ',' + IntToStr(Opinions);
         for DEM := 0 to pred(DEMs.Count) do begin
            aLine := aLine + ',' + RealToString(theSums[DEM] / MaxSum,12,-4);
         end;
         Findings2.Add(aLine);

         aline := Header + '  (n=' + IntToStr(Opinions) + ')' + ',' + IntToStr(Opinions);
         for DEM := 0 to pred(DEMs.Count) do begin
            aLine := aLine + ',' +  RealToString(theSums[DEM] / Opinions,12,-4);
         end;
         AverageScore.Add(aLine);
      end
      else begin
         FiltersTooFewOpinions.Add(theFilter + ',' + IntToStr(Opinions));
      end;
   end;


var
   Header,aFilter,RefFilter : ShortString;
   j,n,ScoresDB : integer;
   Graph : tThisBaseGraph;
   bmp : tMyBitmap;
   LegendfName : PathStr;
   BigBitmap : tStringList;
begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('BestDEMSbyCategory in, ' + GISdb[DBonTable].dbName); {$EndIf}
   Criteria := tStringList.Create;
   Criteria.LoadFromFile(DEMIXSettingsDir + 'demix_criteria.txt');
   DEMs := tStringList.Create;
   DEMs.LoadFromFile(DEMIXSettingsDir + 'demix_dems.txt');
   ShowHourglassCursor;
   if not GISdb[DBonTable].MyData.FieldExists(DEMs.Strings[0] + '_SCR') then begin
      RankDEMS(DBonTable);
   end;

   FiltersTooFewOpinions := tStringList.Create;
   FiltersTooFewOpinions.Add('FILTER,OPINIONS');

   Besties := tStringList.Create;
   aLine := 'FILTER,OPINIONS';
   for i := 0 to pred(DEMs.Count) do aLine := aLine + ',' + DEMs.Strings[i];
   Besties.Add(aLine + ',TIES');

   Findings2 := tStringList.Create;
   aLine := 'FILTER,OPINIONS';
   for i := 0 to pred(DEMs.Count) do aLine := aLine + ',' + DEMs.Strings[i];
   Findings2.Add(aLine);

   Findings := tStringList.Create;
   Findings.Add('FILTER,DEM,SCORE_SUM');

   for i := 1 to 2 do begin
      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('BestDEMSbyCategory DEM=' + RefDEMType[i] ); {$EndIf}
      AverageScore := tStringList.Create;
      aLine := 'FILTER,OPINIONS';
      for j := 0 to pred(DEMs.Count) do aLine := aLine + ',' + DEMs.Strings[j];
      AverageScore.Add(aLine);
      BigBitmap := tStringList.Create;
      RefFilter := ' AND REF_TYPE=' + QuotedStr(RefDEMType[i]);
      for j := 1 to MaxLandType do begin
         DoOne(RefDEMType[i] + ' ' + LandType[j] + ' pixels','LAND_TYPE=' + QuotedStr(LandType[j]) + RefFilter);
      end;
      AverageScore.Add('SKIP');

      if GISdb[DBonTable].MyData.FieldExists('PC_BARREN') then n := 9 else n := 8;
      for J := 1 to n do begin
         GetFilterAndHeader(i,j, Header,aFilter);
         DoOne(Header,aFilter);
      end;
      AverageScore.Add('SKIP');

      for j := 0 to pred(Criteria.Count) do begin
         DoOne(RefDEMType[i] + ' ALL pixels  ' + Criteria.Strings[j],'CRITERION=' + QuotedStr(Criteria.Strings[j]) + RefFilter );
      end;
      //if i=1 then AverageScore.Add('SKIP');

      fName := NextFileNumber(MDTempDir,RefDEMType[i] + '_average_scores_','.dbf');
      ScoresDB := StringList2CSVtoDB(AverageScore,fName);
      Graph := DEMIXwineContestScoresGraph(ScoresDB,'Average score',0.5, 6.25);
      LegendfName := NextFileNumber(MDTempDir,'legend_','.png');
      bmp := Graph.MakeLegend(Graph.GraphDraw.LegendList,false);
      SaveBitmap(bmp,LegendfName);
      Bmp.free;
      fName := NextFileNumber(MDTempDir,'best_graph_','.png');
      SaveImageAsBMP(Graph.Image1,fName);
      BigBitmap.Add(fName);
      BigBitmap.Add(LegendfName);
      fName := NextFileNumber(MDTempDir,'best_graph_with_legend_','.png');
      MakeBigBitmap(BigBitmap,'',fName,2);
      DisplayBitmap(fName,'');

      GISdb[DBonTable].ClearGISFilter;
      GISdb[DBonTable].ShowStatus;
      GISdb[DBonTable].EmpSource.Enabled := true;
      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Done BestDEMSbyCategory DEM=' + RefDEMType[i] ); {$EndIf}
   end;

      //unclear if these are really needed
         fName := ExtractFilePath(GISdb[DBonTable].dbFullName) + 'criteria_best_count.dbf';
         StringList2CSVtoDB(Besties,fName);

         fName := NextFileNumber(MDTempDir, 'multi_line_scores_','.dbf');
         StringList2CSVtoDB(Findings,fName);

         fName := NextFileNumber(MDTempDir,'single_line_scores_','.dbf');
         StringList2CSVtoDB(Findings2,fName);

         fName := NextFileNumber(MDTempDir,'filters_too_few_opinions_','.dbf');
         StringList2CSVtoDB(FiltersTooFewOpinions,fName);

   DEMs.Destroy;
   Criteria.Destroy;
   ShowDefaultCursor;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('BestDEMSbyCategory out, ' + GISdb[DBonTable].dbName); {$EndIf}
end;



end.
