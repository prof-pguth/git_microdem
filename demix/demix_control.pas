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
   {$Define RecordTileStats}

   {$Define RecordTileProcessing}
   {$Define Record3DEPX}
   {$Define RecordCriteriaEvaluation}
   {$Define RecordDEMIXSortGraph}
   {$Define RecordDEMIXSortGraph}
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
    StrUtils,
    VCL.ExtCtrls,VCL.Forms, VCL.Graphics, VCL.Controls,
    WinAPI.Windows,
    Petmar,Petmar_types,BaseGraf,
    DEMDefs;

const
   DEMIXSkipFilesAlreadyDone = false;
   FilterOutSeaLevel = false;

//service functions and procedures
   function LoadDEMIXReferenceDEMs(var RefDEM : integer; OpenMaps : boolean = true) : boolean;
   function LoadDEMIXCandidateDEMs(AreaName : ShortString; RefDEM : integer; OpenMaps : boolean = false; AllCandidates : boolean = true) : boolean;
   procedure GetReferenceDEMsForTestDEM(TestSeries : shortstring; var UseDSM,UseDTM : integer);
   function SymbolFromDEMName(DEMName : shortstring) : tFullSymbolDeclaration;
   function DEMIXColorFromDEMName(DEMName : shortstring) : tPlatformColor;
   procedure OpenDEMIXArea(fName : PathStr = '');
   procedure ZeroDEMs;
   function DEMIXTestDEMLegend : tMyBitmap;

   procedure GetDEMIXpaths(StartProcessing : boolean = true);
   procedure EndDEMIXProcessing;

   procedure AddCountryToDB(DB : integer);
   function MakeGraphOfDifferenceDistribution(Tile,param,Ref : shortstring) : tThisBaseGraph;
   procedure DEMIXtile_inventory(DBonTable : integer);
   procedure SummarizeVDatumShifts;
   procedure SummarizeEGM96toEGM2008shifts;
   procedure SetDirtAirballBackground(var Result : tThisBaseGraph; DEMType : shortstring);   //brown dirtball for STM, blue airball for DSM

//processing steps to create DEMIX data base
   procedure DEMIX_CreateReferenceDEMs(AreaName : shortstring = '');
   procedure DEMIXCreateHalfSecRefDEMs(AreaName : shortstring = '');
   procedure DEMIX_merge_source(AreaName : shortstring = '');
   procedure DEMIX_VDatum_shifts;
   procedure ComputeDEMIX_tile_stats(AreaName : shortstring = '');
   procedure CreateDEMIX_GIS_database(AreaName : shortstring = '');
   procedure SequentialProcessAnArea;
   procedure DEMIX_merge_Visioterra_source(AreaName : shortstring = '');

//3DEP processing pipeline
   procedure SubsetLargeUS3DEParea(DEM : integer = 0);
   procedure DatumShiftforUS3DEParea(DEM : integer = 0);
   procedure BatchGDAL_3DEP_shift;
   procedure BatchSubset_3DEP_DEMs;
   procedure DEMIX_Create3DEPReferenceDEMs;
   procedure DEMIX_Merge3DEPReferenceDEMs;


procedure OpenDEMIXDatabaseForAnalysis;


   procedure ModeOfDifferenceDistributions;


//DEMIX wine contest procedures based on the database
   function DEMIXwineContestScoresGraph(DBonTable : integer; XScalelabel : shortstring; MinHoriz : float32 = 0.5; MaxHoriz : float32 = 5.5) : tThisBaseGraph;
   procedure WinsAndTies(DBonTable : integer);
   procedure DEMIX_graph_best_in_Tile(DBonTable : integer; SortByArea : boolean);
   procedure DEMIXisCOPorALOSbetter(DBonTable : integer);
   procedure BestDEMSbyCategory(DBonTable : integer);
   procedure ModeSTDPlot(DBonTable : integer);
   procedure DEMIXMeanMedianModeHistograms(db : integer);

   procedure MultipleBestByParametersSortByValue(DBonTable,Option : integer; var DEMsTypeUsing,TilesUsing,LandTypesUsing,CandidateDEMsUsing,CriteriaUsing,TileParameters : tStringList; ByPointFilters : boolean = false);
   //procedure MakeCriteriaGraph(DB : integer; var DEMsTypeUsing,TilesUsing,LandTypesUsing,CandidateDEMsUsing,CriteriaUsing : tStringList);


const
   DEMisDTM = 2;
   DEMisDSM = 1;

const
   ResampleModeBoth = 1;
   ResampleModeHalfSec = 2;
   ResampleModeOneSec = 3;

const
   MeanParams : array[1..3] of shortstring = ('ELVD_MEAN','SLPD_MEAN','RUFD_MEAN');
   StdDevParams : array[1..3] of shortstring = ('ELVD_STD','SLPD_STD','RUFD_STD');
   MedianParams : array[1..3] of shortstring = ('ELVD_MED','SLPD_MED','RUFD_MED');
   (*
   AllCriteria : array[1..15] of shortstring = ('ELVD_AVD','ELVD_STD','ELVD_MAE','ELVD_RMSE','ELVD_LE90',
                                                'SLPD_AVD','SLPD_STD','SLPD_MAE','SLPD_RMSE','SLPD_LE90',
                                                'RUFD_AVD','RUFD_STD','RUFD_MAE','RUFD_RMSE','RUFD_LE90');
   *)
   DEMIXshort : array[1..NumDEMIXDEM] of shortstring = ('FAB','COP','ALOS','NASA','SRTM','ASTER');


procedure ResampleForDEMIXOneSecDEMs(DEM : integer; OpenMap : boolean = false; OutPath : PathStr = ''; ResampleMode : byte = 1);


function IsDEMaDSMorDTM(DEMName : ShortString) : integer;
function DEMIXLoadRefDEMFromPath(AreaName : shortstring; LoadMap : boolean) : integer;


const
   MaxTestDEM = 6;
var
   TestDEM : array[1..MaxTestDEM] of integer;
   TestSeries : array[1..MaxTestDEM] of shortstring;
   DEMIX_DB_v1, DEMIX_DB_v2,
   HalfSecRefDTM,HalfSecRefDSM,HalfSecDTM,HalfSecALOS,HalfSecCOP,
   DEMIXRefDEM,RefDTMpoint,RefDTMarea,RefDSMpoint,RefDSMarea, COPRefDTM, COPRefDSM : integer;

   DEMIX_Base_DB_Path,DEMIX_profile_test_dir,
   DEMIX_Ref_Source,DEMIX_Ref_Merge,DEMIX_Ref_1sec,DEMIX_Ref_1sec_v1,
   DEMIX_test_dems,DEMIX_Ref_Half_sec,
   DEMIX_GIS_dbName_v2,DEMIX_GIS_dbName_v1,
   DEMIX_distrib_graph_dir,DEMIX_diff_maps_dir,DEMIX_3DEP_Dir,

   GeodeticFName, IceSatFName, LandCoverFName,
   LocalDatumAddFName,LocalDatumSubFName,RefDSMPointFName,RefDSMareaFName,RefDTMPointFName,RefDTMareaFName, COPRefDTMFName,COPRefDSMFName : PathStr;


implementation

uses
   Nevadia_Main,
   DEMstat,Make_grid,PetImage,PetImage_form,new_petmar_movie,DEMdatabase,PetDButils,Pick_several_dems,
   Geotiff, BaseMap, GDAL_tools, DEMIX_filter,
   DEMCoord,DEMMapf,DEMDef_routines,DEM_Manager,DEM_indexes,PetMath;

var
   vd_path,DEMIX_area_dbName_v2,DEMIX_data_base,DEMIX_diff_dist,DEMIX_area_lc100 : PathStr;
   DoHorizontalShift,
   ElevDiffHists : boolean;


function DEMIXTestDEMLegend : tMyBitmap;
var
   i,Left,Top : integer;
begin
   CreateBitmap(Result,1500,250);
   LoadMyFontIntoWindowsFont(MDDef.LegendFont,Result.Canvas.Font);
   Left := 25;
   Top := 10;
   for i := 1 to NumDEMIXDEM do begin
      Result.Canvas.Pen.Color := ConvertPlatformColorToTColor(DEMIXColorFromDEMName(DEMIXDEMTypeName[i]));
      Result.Canvas.Brush.Color := Result.Canvas.Pen.Color;
      Result.Canvas.Brush.Style := bsSolid;
      Result.Canvas.Rectangle(Left,Top,Left + 15,Top + 15);
      Result.Canvas.Brush.Style := bsClear;
      Result.Canvas.TextOut(Left + 20,Top,DEMIXDEMTypeName[i]);
      Left := Left + 30 + Result.Canvas.TextWidth(DEMIXDEMTypeName[i]);
   end;
   PutBitmapInBox(Result);
end;



procedure SetDirtAirballBackground(var Result : tThisBaseGraph; DEMType : shortstring);
begin
    if (DEMtype = 'DSM') then Result.GraphDraw.GraphBackgroundColor := RGB(219,236,237)
    else Result.GraphDraw.GraphBackgroundColor := RGB(237,237,221);
end;


procedure GetDEMIXpaths(StartProcessing : boolean = true);
begin
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

   DEMIX_Base_DB_Path := 'G:\wine_contest\';
   FindDriveWithPath(DEMIX_Base_DB_Path);

   DEMIX_Ref_Source := DEMIX_Base_DB_Path + 'wine_contest_v2_ref_source\';
   DEMIX_Ref_Merge := DEMIX_Base_DB_Path + 'wine_contest_v2_ref_merge\';
   DEMIX_Ref_1sec := DEMIX_Base_DB_Path + 'wine_contest_v2_ref_1sec\';
   DEMIX_Ref_Half_sec := DEMIX_Base_DB_Path + 'wine_contest_v2_ref_0.5sec\';
   DEMIX_Ref_1sec_v1 := DEMIX_Base_DB_Path + 'demix_reference_dems_v1\';
   vd_path := DEMIX_Base_DB_Path + 'wine_contest_v2_vdatum\';
   DEMIXresultsDir := DEMIX_Base_DB_Path + 'wine_contest_v2_tile_stats\';
   DEMIX_test_dems := DEMIX_Base_DB_Path + 'wine_contest_v2_test_dems\';
   DEMIXSettingsDir := DEMIX_Base_DB_Path + 'wine_contest_settings\';
   DEMIX_data_base  := DEMIX_Base_DB_Path + 'wine_contest_database\';
   DEMIX_diff_dist  := DEMIX_Base_DB_Path + 'wine_contest_v2_diff_dist\';
   DEMIX_diff_maps_dir  := DEMIX_Base_DB_Path + 'wine_contest_difference_maps\';
   DEMIXSettingsDir := DEMIX_Base_DB_Path + 'wine_contest_settings\';
   DEMIX_area_lc100  := DEMIX_Base_DB_Path + 'wine_contest_v2_lc100\';
   DEMIX_profile_test_dir := DEMIX_Base_DB_Path + 'wine_contest_v2_topo_profiles\';
   DEMIX_distrib_graph_dir := DEMIX_Base_DB_Path + 'wine_contest_v2_difference_distrib_graphs\';
   DEMIX_3DEP_Dir := DEMIX_Base_DB_Path + 'wine_contest_v2_3dep\';

   DEMIX_area_dbName_v2 := DEMIX_Base_DB_Path + 'wine_contest_v2_ref_source\demix_area_vert_datums.dbf';
   DEMIX_GIS_dbName_v2 := DEMIX_Base_DB_Path + 'wine_contest_database\demix_database_v2.dbf';
   DEMIX_GIS_dbName_v1 := DEMIX_Base_DB_Path + 'wine_contest_database\demix_database_v1.dbf';

   Geoid2008FName := 'g:\geoid\egm2008-1-vdatum.tif';
   FindDriveWithFile(Geoid2008FName);
   GeoidDiffFName := 'g:\geoid\egm96_to_egm2008.tif';
   FindDriveWithFile(GeoidDiffFName);
end;


procedure EndDEMIXProcessing;
begin
   HeavyDutyProcessing := false;
   DEMIXProcessing := false;
   WMdem.Color := clScrollBar;
   wmdem.ClearStatusBarPanelText;
   ShowDefaultCursor;
end;


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

function DEMIX_AreasWanted(AreaName : shortstring = '') : tStringList;
var
   aTable : tMyData;
   PickedNum : integer;
begin
   if (AreaName = '') then begin
      aTable := tMyData.Create(DEMIX_area_dbName_v2);
      Result := aTable.UniqueEntriesInDB('AREA');
      aTable.Destroy;
      GetFromListZeroBased('DEMIX test areas to process',PickedNum,Result,false,true);
   end
   else begin
      Result := tStringList.Create;
      Result.Add(AreaName);
   end;
end;


procedure DEMIXCreateHalfSecRefDEMs(AreaName : shortstring = '');
var
   fName : PathStr;
   ErrorLog,Areas : tStringList;
   //j,
   i,WantedDEM : integer;
   TStr : shortstring;
begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXCreateHalfSecRefDEMs in'); {$EndIf}
   try
      GetDEMIXPaths;
      Areas := tStringList.Create;
      ErrorLog := tStringList.Create;
      Areas := DEMIX_AreasWanted(AreaName);
      if (Areas.Count > 0) then begin
        for i := 0 to pred(Areas.Count) do begin
           AreaName := Areas.Strings[i];
           wmdem.SetPanelText(2, 'Area: ' + IntToStr(succ(i)) + '/' + IntToStr(Areas.Count));
           wmdem.SetPanelText(3, AreaName);
           {$If Defined(RecordDEMIX)} HighlightLineToDebugFile('DEMIXreferenceDEMs for ' + AreaName); {$EndIf}
           fName := DEMIX_Ref_Half_sec + AreaName + '_ref_0.5sec.tif';
           if not FileExists(fName) then begin
              fName := DEMIX_Ref_Merge + AreaName + '.dem';
              if FileExists(fName) then begin
                 WantedDEM := OpenNewDEM(fName,true);   //need to open map to create the subset
                 if (DEMGlb[WantedDEM].DEMheader.VerticalCSTypeGeoKey = VertCSEGM2008) then begin
                    {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Call ResampleForDEMIXOneSecDEMs ' + AreaName); {$EndIf}
                    ResampleForDEMIXOneSecDEMs(WantedDEM,false,DEMIX_Ref_1sec,ResampleModeHalfSec);
                 end
                 else begin
                    TStr := DEMGlb[WantedDEM].AreaName  + ' not EGM2008; vert datum=' + IntToStr(DEMGlb[WantedDEM].DEMheader.VerticalCSTypeGeoKey);
                    if AnswerIsYes(TStr + '  Proceed anyway') then begin
                       ResampleForDEMIXOneSecDEMs(WantedDEM,false,DEMIX_Ref_1sec,ResampleModeHalfSec);
                    end
                    else begin
                       {$If Defined(RecordDEMIX)} WriteLineToDebugFile(TStr); {$EndIf}
                       ErrorLog.Add(TStr);
                    end;
                 end;
                 CloseSingleDEM(WantedDEM);
              end
              else begin
                 TStr := AreaName  + ' no merged source DEMs';
                 {$If Defined(RecordDEMIX)} WriteLineToDebugFile(TStr); {$EndIf}
                 ErrorLog.Add(TStr);
              end;
           end;
        end;
      end
      else begin
          {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXreferenceDEMs, no areas ' + DEMIX_Ref_Merge); {$EndIf}
      end;
   finally
      Areas.Free;
      EndDEMIXProcessing;
      DisplayAndPurgeStringList(ErrorLog,'DEMIXCreateHalfSecRefDEMs Problems');
   end;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXCreateHalfSecRefDEMs out'); {$EndIf}
end;


procedure AddCountryToDB(DB : integer);
var
   Table : tMyData;
   Area,Country : shortstring;
   i : integer;
begin
   GetDEMIXPaths;
   Table := tMyData.Create(DEMIX_area_dbName_v2);
   GISdb[db].AddFieldToDatabase(ftString,'COUNTRY',16);
   StartProgress('Add country');
   i := 0;
   while not Table.eof do begin
      UpdateProgressBar(i/Table.FiltRecsInDB);
      GISdb[db].EmpSource.Enabled := false;
      Country := Table.GetFieldByNameAsString('COUNTRY');
      Area := Table.GetFieldByNameAsString('AREA');
      if StrUtils.AnsiContainsText(UpperCase(Area),'_DSM') then begin

      end
      else begin
         Area := ShortTestAreaName(Area);
         GISdb[db].ApplyGISFilter('AREA=' + QuotedStr(Area));
         GISdb[db].FillFieldWithValue('COUNTRY',Country);
      end;
      Table.Next;
   end;
   Table.Destroy;
   GISdb[db].ClearGISFilter;
   GISdb[db].ShowStatus;
end;


procedure DEMIX_Merge3DEPReferenceDEMs;
var
   fName{,DoneName} : PathStr;
   ErrorLog,
   Areas,
   MergeList,
   Files : tStringList;
   i,{j,}DEM : integer;
   TStr,Area : shortstring;

      function DoOne(Which : shortstring) : boolean;
      var
         j : integer;
      begin
         MergeList := tStringList.Create;
         for j := 0 to pred(Files.Count) do begin
            fName := UpperCase(Files.Strings[j]);
            if StrUtils.AnsiContainsText(fname,UpperCase(Which)) then begin
               if StrUtils.AnsiContainsText(fname,UpperCase(Area)) then begin
                  MergeList.Add(fName);
               end;
            end;
         end;
         Result := (MergeList.Count > 0);
         if Result then begin
            {$If Defined(RecordDEMIX)} WriteLineToDebugFile('area=' + Area + '  merge=' + IntToStr(MergeList.Count)); {$EndIf}
            DEM := MergeMultipleDEMsHere(MergeList,false,false);
            //3DEP only has DTM, and we don't put that in unless there is also a DSM
            fName := DEMIX_Ref_1sec + Area + '_ref_1sec_' + Which + '.tif';
            DEMGlb[DEM].SavePartOfDEMWithData(fName);
         end
         else begin
            TStr := 'no files in ' + Area;
            ErrorLog.Add(Area);
            {$If Defined(RecordDEMIX)} WriteLineToDebugFile(Which + '  ' + TStr); {$EndIf}
            MergeList.Free;
         end;
      end;


begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIX_Merge3DEPReferenceDEMs in'); {$EndIf}
   try
      GetDEMIXPaths;
      ErrorLog := tStringList.Create;
      Files := nil;
      Petmar.FindMatchingFiles(DEMIX_3DEP_Dir,'*.tif',Files,0);
      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Found TIFs=' + IntToStr(Files.Count)); {$EndIf}
      Areas := tStringList.Create;
      Areas.Sorted := true;
      Areas.Duplicates := dupIgnore;
      for i := pred(Files.Count) downto 0 do begin
         fName := ExtractFileNameNoExt(Files.Strings[i]);
         if (StrUtils.AnsiContainsText(UpperCase(fname),'_ref_')) then begin
            Area := BeforeSpecifiedString(UpperCase(fName),'_SUB_');
            Areas.Add(Area);
         end
         else begin
            Files.Delete(i);
         end;
      end;
      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Found ref TIFs=' + IntToStr(Files.Count)); {$EndIf}
      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Found areas=' + IntToStr(Areas.Count)); {$EndIf}
      for i := 0 to pred(Areas.Count) do begin
         Area := Areas.Strings[i];
         if not DoOne('point') then begin
            //no point doing area if there was not Point
            DoOne('area');
         end;
      end;
   finally
      EndDEMIXProcessing;
      DisplayAndPurgeStringList(ErrorLog,'DEMIX_Create3DEPReferenceDEMs Problems');
   end;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIX_Merge3DEPReferenceDEMs out'); {$EndIf}
end;


procedure DEMIX_Create3DEPReferenceDEMs;
var
   fName,DoneName : PathStr;
   ErrorLog,Files : tStringList;
   i,{j,}WantedDEM : integer;
   //TStr : shortstring;
begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIX_Create3DEPReferenceDEMs in'); {$EndIf}
   try
      GetDEMIXPaths;
      ErrorLog := tStringList.Create;
      Files := nil;
      Petmar.FindMatchingFiles(DEMIX_3DEP_Dir,'*.tif',Files,0);
      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Found TIFs=' + IntToStr(Files.Count)); {$EndIf}
      for i := pred(Files.Count) downto 0 do begin
         fName := Files.Strings[i];
         if (not StrUtils.AnsiContainsText(UpperCase(fname),'EGM2008')) then Files.Delete(i);
         if (StrUtils.AnsiContainsText(UpperCase(fname),'_ref_')) then Files.Delete(i);
      end;
      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Found EGM2008 TIFs=' + IntToStr(Files.Count)); {$EndIf}
      for i := 0 to pred(Files.Count) do begin
         fName := Files.Strings[i];
         wmdem.SetPanelText(2, IntToStr(succ(i)) + '/' + IntToStr(Files.Count));
         DoneName := ChangeFileExt(fName,'_ref_1sec_point.tif');
         if DEMIXSkipFilesAlreadyDone and FileExists(DoneName) then begin
            {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIX_Create3DEPReferenceDEMs already done ' + fName ); {$EndIf}
         end
         else begin
            WantedDEM := OpenNewDEM(fName,true);   //need to open map to create the subset
            ResampleForDEMIXOneSecDEMs(WantedDEM,false,DEMIX_3DEP_Dir,ResampleModeOneSec);
            CloseSingleDEM(WantedDEM);
          end;
      end;
   finally
      EndDEMIXProcessing;
      DisplayAndPurgeStringList(ErrorLog,'DEMIX_Create3DEPReferenceDEMs Problems');
   end;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIX_Create3DEPReferenceDEMs out'); {$EndIf}
end;


procedure BatchSubset_3DEP_DEMs;
var
   FilesWanted : tStringList;
   DefaultFilter : byte;
   DEM,i  : integer;
   fName : PathStr;
begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('BatchSubset_3DEP_DEMs in'); {$EndIf}
   try
      GetDEMIXpaths;
      FilesWanted := tStringList.Create;
      FilesWanted.Add(DEMIX_3DEP_Dir);
      DefaultFilter := 1;
      if GetMultipleFiles('for 3DEP subset','*.dem',FilesWanted,DefaultFilter) then begin
         for I := 0 to pred(FilesWanted.Count) do begin
            wmdem.SetPanelText(2,'Area: ' + IntToStr(succ(i)) + '/' + IntToStr(FilesWanted.Count));
            fName := FilesWanted.Strings[i];
            DEM := OpenNewDEM(fName,true);
            SubsetLargeUS3DEParea(DEM);
            CloseSingleDEM(DEM);
         end;
      end;
   finally
      EndDEMIXProcessing;
   end;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('BatchSubset_3DEP_DEMs out'); {$EndIf}
end;


procedure BatchGDAL_3DEP_shift;
var
   FilesWanted : tStringList;
   DefaultFilter : byte;
   DEM,i  : integer;
   fName : PathStr;
begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('BatchGDAL_3DEP_shift in'); {$EndIf}
   try
      GetDEMIXpaths;
      FilesWanted := tStringList.Create;
      FilesWanted.Add(DEMIX_3DEP_Dir);
      DefaultFilter := 1;
      if GetMultipleFiles('for GDAL datum transfer','*.tif',FilesWanted,DefaultFilter) then begin
         for I := 0 to pred(FilesWanted.Count) do begin
            wmdem.SetPanelText(2,'GDAL 3DEP datum shift: ' + IntToStr(succ(i)) + '/' + IntToStr(FilesWanted.Count));
            fName := FilesWanted.Strings[i];
            if (FileExists(ChangeFileExt(fName,'_egm2008.tif'))) or StrUtils.AnsiContainsText(UpperCase(fname),'EGM2008') then begin
               //already done EGM2008 shift
            end
            else begin
               DEM := OpenNewDEM(fName,false);
               DatumShiftforUS3DEParea(DEM);
               CloseSingleDEM(DEM);
            end;
         end;
      end;
   finally
      EndDEMIXProcessing;
   end;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('BatchGDAL_3DEP_shift out'); {$EndIf}
end;


procedure DatumShiftforUS3DEParea(DEM : integer = 0);
//for a single DEM, called by routine above for many
var
   InName,SaveName : PathStr;
   UTMZone,s_SRSstring,t_srsstring : shortstring;
begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DatumShiftforUS3DEParea in'); {$EndIf}
   try
      GetDEMIXpaths;
      if not ValidDEM(DEM) then begin
         DEM := OpenNewDEM('',false);
      end;
      InName := DEMGlb[DEM].GeotiffDEMName;
      SaveName := ChangeFileExt(InName,'_egm2008.tif');
      UTMZone := AddDayMonthLeadingZero(DEMGlb[DEM].DEMHeader.UTMzone);
      s_SRSstring := ' -s_srs EPSG:269' + UTMzone + '+5703';
      t_srsstring := ' -t_srs EPSG:326' + UTMzone + '+3855';
      CompositeDatumShiftWithGDAL(InName,SaveName,s_SRSstring,t_srsstring);
   finally
       EndDEMIXProcessing;
   end;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DatumShiftforUS3DEParea out'); {$EndIf}
end;


procedure SubsetLargeUS3DEParea(DEM : integer = 0);
const
   Extra = 0.0051;
var
   db,SubArea : integer;
   bb : sfBoundBox;
   filter : shortstring;
   fName : PathStr;
   JustOne : boolean;
   GridLimits : tGridLimits;

   procedure CornerFilter(ExtraSW,ExtraNE : float32; What : shortstring);
   begin
      GISdb[db].MyData.First;
      bb.xmin := GISdb[db].MyData.GetFieldByNameAsFloat('LONG_LOW');
      bb.ymin := GISdb[db].MyData.GetFieldByNameAsFloat('LAT_LOW');
      {$If Defined(Record3DEPX)} WriteLineToDebugFile(What + ' ' + LatLongDegreeToString(bb.ymin,bb.xmin,DecDegrees)); {$EndIf}
      //limit to no more than 2 tiles in each direction
      bb.xmax := bb.xmin + 0.2;
      bb.ymax := bb.ymin + 0.2;
      //move so we are either just inside or just outside the tiles we want
      bb.XMax := bb.xmax + ExtraNE;
      bb.yMax := bb.ymax + ExtraNE;
      bb.xmin := bb.xmin + ExtraSW;
      bb.ymin := bb.ymin + ExtraSW;
      Filter := MakeCornersGeoFilter(bB);
      GISdb[db].ApplyGISFilter(Filter);
      {$If Defined(Record3DEPX)} WriteLineToDebugFile(What + ' ' + Filter + ' bb=' + sfBoundBoxToString(bb,2) + ' tiles=' + IntToStr(GISdb[db].MyData.FiltRecsInDB)); {$EndIf}
   end;

begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('SubsetLargeUS3DEParea in'); {$EndIf}
   try
      GetDEMIXpaths;
      if ValidDEM(DEM) then begin
         JustOne := false;
      end
      else begin
         DEM := OpenNewDEM;
         JustOne := true;
      end;
      if ValidDEM(DEM) then begin
         SubArea := 0;
         if ValidDEM(DEM) then begin
            fName := DEMIX_3DEP_Dir + DEMGlb[DEM].AreaName + '_sub_1' + '.tif';
            if FileExists(fName) then begin
               db := 0;
            end
            else begin
               db := DEMIXtileFill(DEM,DEMGlb[DEM].SelectionMap.MapDraw.MapCorners.BoundBoxGeo);
               {$If Defined(Record3DEPX)} WriteLineToDebugFile('SubsetLargeUS3DEParea partial tiles=' + IntToStr(GISdb[db].MyData.FiltRecsInDB)); {$EndIf}
               Filter := 'GRID_FULL<' + RealToString(MDDef.DEMIX_Full,-8,-1);
               GISdb[db].ApplyGISFilter(Filter);
               {$If Defined(Record3DEPX)} WriteLineToDebugFile('SubsetLargeUS3DEParea too empty tiles=' + IntToStr(GISdb[db].MyData.FiltRecsInDB)); {$EndIf}
               GISdb[db].DeleteAllSelectedRecords(true);
               GISdb[db].ClearGIsfilter;
               {$If Defined(Record3DEPX)} WriteLineToDebugFile('SubsetLargeUS3DEParea exporting tiles=' + IntToStr(GISdb[db].MyData.FiltRecsInDB)); {$EndIf}
               wmdem.SetPanelText(3,DEMGlb[DEM].AreaName + ': tiles=' + IntToStr(GISdb[db].MyData.FiltRecsInDB));

               repeat
                  //the first tile in the db is at the lower left
                  //we want to merge no more than 4 tiles due to issues with larger Geotiffs
                  //use slight buffer for UTM grid rotation
                  CornerFilter(-Extra,+Extra, 'SubsetLargeUS3DEParea to export ');

                  DEMGlb[DEM].SelectionMap.SubsetAndZoomMapFromGeographicBounds(bb,true);
                  inc(SubArea);
                  GridLimits := DEMGlb[DEM].SelectionMap.MapDraw.MapAreaDEMGridLimits;
                  fName := DEMIX_3DEP_Dir + DEMGlb[DEM].AreaName + '_sub_' + IntToStr(SubArea) + '.tif';
                  {$If Defined(Record3DEPX)} WriteLineToDebugFile('SubsetLargeUS3DEParea save tiff=' + ' x=' + IntToStr(GridLimits.XGridLow) + '-' + IntToStr(GridLimits.XGridHigh) + ' / ' +
                     ' y=' + IntToStr(GridLimits.YGridLow) + '-' + IntToStr(GridLimits.YGridHigh) + ' ' + fName); {$EndIf}
                  DEMGlb[DEM].SaveGridSubsetGeotiff(GridLimits,fName);
                  HeavyDutyProcessing := true;

                  //Adjust to only get tiles that were 100% exported, so make box just a bit smaller
                  CornerFilter(+Extra,-Extra,'SubsetLargeUS3DEParea to delete');
                  GISdb[db].DeleteAllSelectedRecords(true);
                  GISdb[db].ClearGIsfilter;
                  {$If Defined(Record3DEPX)} WriteLineToDebugFile('SubsetLargeUS3DEParea remaining tiles=' + IntToStr(GISdb[db].MyData.FiltRecsInDB)); {$EndIf}
                  wmdem.SetPanelText(3,DEMGlb[DEM].AreaName + ': remaining tiles=' + IntToStr(GISdb[db].MyData.FiltRecsInDB));
               until GISdb[db].MyData.FiltRecsInDB = 0;
            end;
         end;
      end;
   finally
       CloseAndNilNumberedDB(db);
       if JustOne then begin
          DEMGlb[DEM].SelectionMap.RestoreFullMap;
          EndDEMIXProcessing;
       end;
   end;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('SubsetLargeUS3DEParea out'); {$EndIf}
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


procedure SummarizeVDatumShifts;
var
   Files,Summary : tstringlist;
   fName : PathStr;
   I : integer;
   db : tMyData;
   TStr : shortstring;
begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('SummarizeVDatumShifts in'); {$EndIf}
   GetDEMIXpaths(false);
   Files := nil;
   Petmar.FindMatchingFiles(vd_path,'*.dbf',Files,0);

   Summary := tstringlist.create;
   Summary.Add('AREA,LAT,LONG,MEAN_DX,MEAN_DY,MEAN_DZ,MEDIAN_DZ,STD_DZ,MAX_DX,MAX_DY,MAX_DZ,MIN_DX,MIN_DY,MIN_DZ');
   if (Files.Count > 0) then for I := 0 to pred(Files.Count) do begin
      fName := Files.Strings[i];
      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('SummarizeVDatumShifts ' + fName); {$EndIf}
      db := tMyData.Create(fName);
      if db.FieldExists('X_SHIFT') and db.FieldExists('Y_SHIFT') and db.FieldExists('VERT_SHIFT') then begin
         TStr := ExtractFileNameNoExt(db.TableName)  + ',' ;
         TStr := TStr + RealToString(db.FieldAverage('LAT'),-12,-3) + ',' +  RealToString(db.FieldAverage('LONG'),-12,-3) + ',';
         TStr := TStr + RealToString(db.FieldAverage('X_SHIFT'),-8,-2) + ',' + RealToString(db.FieldAverage('Y_SHIFT'),-8,-2) + ',' +
           RealToString(db.FieldAverage('VERT_SHIFT'),-8,-2) + ',' + RealToString(db.FieldMedian('VERT_SHIFT'),-8,-2) + ',' +
           RealToString(db.FieldStdDev('VERT_SHIFT'),-8,-2) + ',';
         TStr := TStr + RealToString(db.FindFieldMax('X_SHIFT'),-8,-2) + ',' + RealToString(db.FindFieldMax('Y_SHIFT'),-8,-2) + ',' + RealToString(db.FindFieldMax('VERT_SHIFT'),-8,-2) + ',';
         TStr := TStr + RealToString(db.FindFieldMin('X_SHIFT'),-8,-2) + ',' + RealToString(db.FindFieldMin('Y_SHIFT'),-8,-2) + ',' + RealToString(db.FindFieldMin('VERT_SHIFT'),-8,-2);
         Summary.Add(TStr);
         {$If Defined(RecordDEMIX)} WriteLineToDebugFile(TStr); {$EndIf}
      end
      else begin
         {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Shift files problem ' + fName); {$EndIf}
      end;
      db.Destroy;
   end;
   Files.Destroy;
   fName := NextFileNumber(MDTempDir,'VDATUM_shift_summary','.dbf');
   StringList2CSVtoDB(Summary,fName);
end;


procedure DEMIXtile_inventory(DBonTable : integer);
var
   TileList,Findings,DEMs : tStringList;
   TStr : shortstring;
   fName : PathStr;
   i : Integer;
begin
   GISdb[DBonTable].EmpSource.Enabled := false;
   TileList := GISdb[DBonTable].MyData.UniqueEntriesInDB('DEMIX_TILE');
   Findings := tStringList.Create;
   Findings.Add('COUNTRY,AREA,DEMIX_TILE,DEMS');
   StartProgress('Inventory tiles');
   for i := 0 to pred(TileList.Count) do begin
      wmdem.SetPanelText(3,'Tile ' + IntToStr(i) + '/' + IntToStr(TileList.Count));

      GISdb[DBonTable].EmpSource.Enabled := false;
      if (i mod 10 = 0) then UpdateProgressBar(i/TileList.Count);

      GISdb[DBonTable].ApplyGISFilter('DEMIX_TILE=' + QuotedStr(TileList.Strings[i]),false);
      DEMs := GISdb[DBonTable].MyData.UniqueEntriesInDB('REF_TYPE');
      TStr := DEMs[0];
      if (DEMs.Count > 1) then TStr := TStr + ' + ' +  DEMs[1];
      Findings.Add(GISdb[DBonTable].MyData.GetFieldByNameAsString('COUNTRY') + ',' + GISdb[DBonTable].MyData.GetFieldByNameAsString('AREA') + ',' +  TileList.Strings[i] + ',' + TStr);
   end;
   GISdb[DBonTable].ClearGISFilter;
   fName := NextFileNumber(MDTempDir,'DEMIX_tile_inventory','.dbf');
   StringList2CSVtoDB(Findings,fName);
   GISdb[DBonTable].ShowStatus;
   wmdem.SetPanelText(3,'');

end;


procedure SequentialProcessAnArea;
//still in progress, 20 May 2023
var
   Areas : tStringList;
   i : integer;
   AreaName : shortstring;
begin
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
end;


function IsDEMaDSMorDTM(DEMName : ShortString) : integer;
//works for the DEMIX naming conventions, where DSM must have that in the file name
begin
   if (StrUtils.AnsiContainsText(UpperCase(DEMName),'DSM')) then Result := DEMisDSM
   else Result := DEMisDTM;
end;



procedure ModeOfDifferenceDistributions;
var
   FilesWanted,Modes,TileList : tStringList;
   fName,fName2 : PathStr;
   //UseTile : boolean;
   i,j,NPts,db,atile : integer;
   BinSize,Mode : float32;
   Tile,param,Ref,aLine,Area : shortstring;
   Values : ^Petmath.bfarray32;
   MomentVar : tMomentVar;
begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('ModeOfDifferenceDistributions in'); {$EndIf}
   try
      if not Petmar.GetExistingFileName('Ordered list of tiles','*.txt',fName) then exit;
      GetDEMIXpaths;
      Modes := tStringList.Create;
      aLine := 'AREA,TILE,REF_TYPE,CRITERION';
      for I := 1 to NumDEMIXDEM do begin
         aline := aline + ',' + DEMIXshort[i] + '_MODE' + ',' + DEMIXshort[i] + '_MEAN' + ',' + DEMIXshort[i] + '_MEDN' + ',' + DEMIXshort[i] + '_STD';
      end;
      Modes.Add(aLine);
      TileList := tStringList.Create;
      TileList.LoadFromFile(fName);
      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('tiles=' + IntToStr(TileList.Count)); {$EndIf}
      FilesWanted := tStringList.Create;
      FindMatchingFiles(DEMIX_diff_dist,'*.z',FilesWanted,0);
      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('z files=' + IntToStr(FilesWanted.Count)); {$EndIf}

      StartProgress('Modes');
      for atile := 0 to pred(TileList.Count) do begin
         UpdateProgressBar(aTile/TileList.Count);
         for i := 0 to pred(FilesWanted.Count) do begin
            fName := upperCase(FilesWanted.Strings[i]);
            if StrUtils.AnsiContainsText(fname,TileList[aTile]) then begin
               if StrUtils.AnsiContainsText(fname,'COP') then begin
                  Area := BeforeSpecifiedString(ExtractFileName(FilesWanted.Strings[i]),'_COP');

                  Tile := ExtractFileName(fName);
                  Tile := AfterSpecifiedString(Tile,'COP_');
                  Tile := Copy(Tile,1,9);
                  if StrUtils.AnsiContainsText(fname,'DSM') then Ref := 'DSM' else Ref := 'DTM';

                  if StrUtils.AnsiContainsText(fname,'ELEV') then begin
                     BinSize := 0.10;
                     Param := 'elvd_mode';
                  end;

                  if StrUtils.AnsiContainsText(fname, 'SLOPE') then begin
                     BinSize := 0.25;
                     Param := 'slpd_mode';
                  end;
                  if StrUtils.AnsiContainsText(fname, 'RUFF') then begin
                     BinSize := 0.15;
                     Param := 'rufd_mode';
                  end;
                  aLine := Area + ',' + Tile + ',' + Ref + ',' + Param ;

                  for j := 1 to NumDEMIXDEM do begin
                     fName2 := StringReplace(fName,'COP',DEMIXDEMTypeName[j],[rfReplaceAll, rfIgnoreCase]);
                     if FileExists(fName2) then begin
                        New(Values);
                        LoadBFarray32(fName2,Values^,npts);
                        Mode := PetMath.Mode(Values^,npts,binsize);
                        InitializeMomentVar(MomentVar);
                        MomentVar.NPts := Npts;
                        moment(Values^,MomentVar,msAll);
                        Dispose(Values);
                     end
                     else Mode := -9999;
                     aLine := aLine + ',' + RealToString(Mode,-8,-2) + ',' + RealToString(MomentVar.Mean,-8,-2) + ',' + RealToString(MomentVar.Median,-8,-2) + ',' + RealToString(MomentVar.Sdev,-8,-2);
                  end;
                  Modes.Add(aline);
               end;
            end;
         end;
      end;
      fName := NextFileNumber(MDTempDir,'distrib_diff_modes_','.dbf');
      db := StringList2CSVtoDB(Modes,fName);
   finally
      TileList.Free;
      FilesWanted.Free;
      EndDEMIXProcessing;
   end;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('ModeOfDifferenceDistributions out'); {$EndIf}
end;



function MakeGraphOfDifferenceDistribution(Tile,param,Ref : shortstring) : tThisBaseGraph;
var
   FilesWanted,Distributions,Legends : tStringList;
   fName : PathStr;
   UseTile : boolean;
   i,j : integer;
   Min,Max,BinSize : float32;
begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('MakeGraphOfDifferenceDistribution in, ' + Tile + '  ' + Param + '  ' + Ref); {$EndIf}
   MDDef.DefaultGraphXSize := 1000;
   MDDef.DefaultGraphYSize := 600;
   FilesWanted := tStringList.Create;
   FindMatchingFiles(DEMIX_diff_dist,'*.z',FilesWanted,0);
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('MakeGraphOfDifferenceDistribution dist file=' + IntToStr(FilesWanted.Count)); {$EndIf}

   Distributions := tStringList.Create;
   Legends := tStringList.Create;
   for i := 0 to pred(FilesWanted.Count) do begin
      fName := upperCase(FilesWanted.Strings[i]);
      if StrUtils.AnsiContainsText(fname,UpperCase(Tile)) and StrUtils.AnsiContainsText(fname,UpperCase(Param)) then begin
         if (UpperCase(Ref) = 'DSM') then UseTile := StrUtils.AnsiContainsText(fname,'DSM')
         else UseTile := not StrUtils.AnsiContainsText(fname,'DSM');
         if UseTile then begin
            {$If Defined(RecordDEMIX)} WriteLineToDebugFile('MakeGraphOfDifferenceDistribution use ' + fName); {$EndIf}
            Distributions.Add(fName);
            for j := 1 to NumDEMIXDEM do begin
               if StrUtils.AnsiContainsText(fname,UpperCase(DEMIXDEMTypeName[j])) then begin
                  Legends.Add(DEMIXDEMTypeName[j]);
               end;
            end;

            if StrUtils.AnsiContainsText(fname,'ELEV') then begin
               Min := -5;
               Max := 5;
               BinSize := 0.10;
            end;
            if StrUtils.AnsiContainsText(fname, 'SLOPE') then begin
               Min := -10;
               Max := 10;
               BinSize := 0.25;
            end;
            if StrUtils.AnsiContainsText(fname, 'RUFF') then begin
               Min := -10;
               Max := 10;
               BinSize := 0.15;
            end;
         end;
      end;
   end;

   if (Distributions.Count = 6) and (Legends.Count = 6) then begin
      Result := CreateMultipleHistogram(MDDef.CountHistograms,Distributions,Legends,Tile + '  ' + param + ' difference distribution to reference ' + Ref,
        Tile + '  ' + param + ' difference',100,Min,Max,BinSize);
      for I := 1 to Result.GraphDraw.LegendList.Count do begin
         Result.GraphDraw.FileColors256[i] := DEMIXColorFromDEMName(Result.GraphDraw.LegendList[pred(i)]);
      end;
      Result.GraphDraw.InsideMarginLegend := lpNWMap;
      SetDirtAirballBackground(Result,Ref);

      Result.RedrawDiagram11Click(Nil);
      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('MakeGraphOfDifferenceDistribution out'); {$EndIf}
   end
   else begin
      Result := nil;
      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('MakeGraphOfDifferenceDistribution fail, Dist=' + IntToStr(Distributions.Count) + '  Leg=' + IntToStr(Legends.Count) ); {$EndIf}
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



procedure CreateDEMIX_GIS_database(AreaName : shortstring = '');
var
   fName : PathStr;
   i     : byte;
   //DEM,
   db : integer;
   FilesWanted : tStringList;
   Tiles,ElevDiff,SlopeDiff,RuffDiff,TransposeNames,ErrorLog : tStringList;


      function TransposeDEMIXcriteria(DBonTable : integer) : PathStr;
      const
         MaxDEMs = 10;
         MaxCriteria = 25;
      var
         Headers,DEMs,Criteria,Output,Tiles : tStringList;
         Line : ANSIString;
         //fName,
         CriteriaFile : PathStr;
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

         Cycles := Tiles.Count * 2 * NumLandTypes;
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
                  for aLandType := 1 to NumLandTypes do begin
                     Line := 'DEMIX_TILE=' + QuotedStr(Tiles[Tile]) + ' AND REF_TYPE=' + QuotedStr(RefDEMType[Ref])  + ' AND LAND_TYPE=' + QuotedStr(LandTypes[aLandType]);
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
                              ErrorLog.Add(Line + '  -1 index for ' + GISdb[DBonTable].MyData.GetFieldByNameAsString('DEM'));
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
      DeleteFileIfExists(fName);
      DeleteFileIfExists(ChangeFileExt(fName,'.dbf'));

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
   Areas := DEMIX_AreasWanted(AreaName);
   Areas.Sorted := false;
   for i := pred(Areas.Count) downto 0 do begin
      fName := Areas.Strings[i];
      if StrUtils.AnsiContainsText(UpperCase(fname),'_DTM') then Areas.Strings[i] := Copy(fName,1,length(fName)-4);
      if StrUtils.AnsiContainsText(UpperCase(fname),'_DSM') then Areas.Delete(i);
   end;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('MergeDEMIXCSV picked areas=' + IntToStr(Areas.Count)); {$EndIf}
   FilesWanted := Nil;
   FindMatchingFiles(DEMIXresultsDir,'*.csv',FilesWanted,0);
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('CSV total results=' + IntToStr(FilesWanted.Count)); {$EndIf}
   for j := pred(FilesWanted.Count) downto 0 do begin
      KeepThisOne := false;
      for i := pred(Areas.Count) downto 0 do begin
         if StrUtils.AnsiContainsText(FilesWanted.Strings[j],Areas.Strings[i]) then begin
            KeepThisOne := true;
         end;
      end;
      if Not KeepThisOne then begin
         {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Not Using: ' + FilesWanted.Strings[j]); {$EndIf}
         FilesWanted.Delete(j);
      end;
   end;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('CSV in these areas=' + IntToStr(FilesWanted.Count)); {$EndIf}

   try
      Tiles := tStringList.Create;
      ElevDiff := tStringList.Create;
      SlopeDiff := tStringList.Create;
      RuffDiff := tStringList.Create;
      TransposeNames := tStringList.Create;
      ErrorLog := tStringList.Create;
      for i := 0 to pred(FilesWanted.Count) do begin
         fName := uppercase(FilesWanted.Strings[i]);
         if StrUtils.AnsiContainsText(fname,'DEMIX_TILES_USED') then Tiles.Add(fName);
         if StrUtils.AnsiContainsText(fname,'ELEV_DIFF_STATS') then ElevDiff.Add(fName);
         if StrUtils.AnsiContainsText(fname,'SLOPE_DIFF_STATS') then SlopeDiff.Add(fName);
         if StrUtils.AnsiContainsText(fname,'RUFF_DIFF_STATS') then RuffDiff.Add(fName);
      end;
      if (Tiles.Count > 1) then begin
         {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Tiles=' + IntToStr(Tiles.Count)); {$EndIf}
         {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Elev diff=' + IntToStr(ElevDiff.Count)); {$EndIf}
         {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Slope diff=' + IntToStr(SlopeDiff.Count)); {$EndIf}
         {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Ruff diff=' + IntToStr(RuffDiff.Count)); {$EndIf}
         fName := ExtractFilePath(fName) + 'DEMIX_TILES_USED_SUMMARY.csv';
         fName := StringReplace(fName,'_dtm','',[rfReplaceAll, rfIgnoreCase]);
         MergeCSVFiles(Tiles,fName);
      end
      else Tiles.Free;

      DeleteFileIfExists(DEMIX_data_base + 'latest_demix_database.dbf');
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
      AddCountryToDB(DB);
      RankDEMS(DB,true);
      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('MergeDEMIXCSV DEMs ranked'); {$EndIf}
      //CloseAndNilNumberedDB(db);
   finally
      EndDEMIXProcessing;
      DisplayAndPurgeStringList(ErrorLog,'DEMIX Create Database Problems');
      CloseAllDatabases;
   end;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('MergeDEMIXCSV out, created ' + fName); {$EndIf}
end;


procedure ComputeDEMIX_tile_stats(AreaName : shortstring = '');
//a number of experimental options are disabled, and would require changes to the code to get working again
const
   MaxRefStore = 100;
var
   MomentVar,FlatMomentVar,SteepMomentVar,GentleMomentVar,CliffMomentVar,ForestMomentVar,UrbanMomentVar,BarrenMomentVar : tMomentVar;
   SlopeMomentVar,ElevMomentVar : array[1..MaxRefStore] of tMomentVar;
   NumPits,NumPeaks : array[1..MaxRefStore] of integer;
   DEMIXtileDB,

   LandCoverGrid : integer;
   bbgrid : tGridLimits;

   //IceSatFName,GeodeticFName,

   cfName,fName : PathStr;
   TestAreaName,DEMIXtile,LandTypeMask,TileHeader : shortstring;
   LatCent,LongCent,
   GridFull,Lat,Long : float64;
   zRef : float32;
   TileStats,

   ElevDiffStats,
   RufDiffStats,
   SlopeDiffStats  : tStringList;
   zs,zsSteep,ZSFlat,zsGentle,zsCliff,zsForest,zsUrban,zsBarren : ^bfarray32;
   //Rules,GeodeticTable,IceSatTable : tMyData;
   SlopeAsp,SlopeAspectTest : tSlopeAspectRec;
   ErrorLog : tStringList;
   ElevFiles,LegendFiles : tStringList;

   //{$I experiment_demix_criteria.inc}

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

            //CreateExperimentalStringLists;

            {$IfDef RecordDEMIXFull} writeLineToDebugFile('InitializeStringLists out, string lists created'); {$EndIf}
         end;


         function LineHeader(DEM : integer; RefType : shortstring) : shortstring;
         var
            t1{,t2} : shortstring;
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
                     fName := DEMIXresultsDir + TestAreaName + fname + '.csv';
                     sl.SaveToFile(fName);
                  end
                  else fName := '';
                  sl.Destroy;
               end;

         begin
            {$IfDef RecordFullDEMIX} writeLineToDebugFile('DEMIX start string list processing'); {$EndIf}
            ProcessStringList('_DEMIX_tiles_used',TileStats);

            ProcessStringList('_elev_diff_stats',ElevDiffStats,true);
            ProcessStringList('_ruff_diff_stats',RufDiffStats,true);
            ProcessStringList('_slope_diff_stats',SlopeDiffStats,true);

            //ProcessExperimentalStringLists;

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
            {$IfDef RecordUseTile} if not Result then WriteLineToDebugFile('Not doing tile=' + DEMIXTile + '  fill=' + IntToStr(round(GridFull))); {$EndIf}
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
                           {$If Defined(RecordGridCompare)} WriteLineToDebugFile('WriteDifferenceResult failed for ' + LandTypeMask + ' theMoments.NPts=' + IntToStr(theMoments.NPts) ); {$EndIf}
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
              NumRuff,NumSlope,NumElev,
              xg,yg{,Col,Row} : integer;
              TStr : shortstring;
              z,Difference : float32;

                  procedure DoElevations;
                  var
                     Col,Row : integer;
                  begin
                     NumElev := 0;
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
                           fName := DEMIX_diff_dist + DEMGLB[DEM].AreaName + '_' + DEMIXtile + '_elev_to_' + RefType  + '.z';
                           ElevFiles.Add(SaveSingleValueSeries(MomentVar.npts,zs^,fName));
                           LegendFiles.Add(ExtractFileNameNoExt(fName));
                        end;
                        NumElev := WriteDifferenceResults(DEM,REFDEM,RefType,ElevDiffStats);
                     end
                     else begin
                        TStr := 'No points for elevation difference ' + DEMGLB[DEM].AreaName;
                        {$If Defined(RecordFullDEMIX) or Defined(RecordGridCompare)} writeLineToDebugFile(TStr); {$EndIf}
                        ErrorLog.Add(TStr);
                     end;
                  end;


                  procedure DoSlopes;
                  var
                     Col,Row : integer;
                  begin
                     NumSlope := 0;
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
                           fName := DEMIX_diff_dist + DEMGLB[DEM].AreaName + '_' + DEMIXtile + '_slope_to_' + RefType + '.z';
                           SaveSingleValueSeries(MomentVar.npts,zs^,fName);
                        end;
                        NumSlope := WriteDifferenceResults(DEM,REFDEM,RefType,SlopeDiffStats);
                     end
                     else begin
                        TStr := 'No points for slope differences ' + DEMGLB[DEM].AreaName;
                        {$If Defined(RecordFullDEMIX) or Defined(RecordGridCompare)} writeLineToDebugFile(TStr); {$EndIf}
                        ErrorLog.Add(TStr);
                     end;
                  end;

                  procedure DoRoughness;
                  var
                     Col,Row : integer;
                     Ruff1,Ruff2 : float32;
                  begin
                     NumRuff := 0;
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
                     end
                     else begin
                        TStr := 'No points for roughness difference ' + DEMGLB[DEM].AreaName;
                        {$If Defined(RecordFullDEMIX) or Defined(RecordGridCompare)} writeLineToDebugFile(TStr); {$EndIf}
                        ErrorLog.Add(TStr);
                     end;
                  end;


            begin
               if (RefDEM = 0) then begin
                  //this is probably for DSM when we don't have one, so we won't log
                  //TStr := 'Fail (RefDEM=0), comparison to reference, ' + DEMGLB[DEM].AreaName;
                  //{$If Defined(RecordFullDEMIX) or Defined(RecordGridCompare)} WriteLineToDebugFile(TStr); {$EndIf}
                  //ErrorLog.Add(TStr);
               end
               else begin
                  {$If Defined(RecordFullDEMIX) or Defined(RecordGridCompare)} WriteLineToDebugFile('Comparison to reference=' + DEMGLB[RefDEM].AreaName + '  test=' + DEMGLB[DEM].AreaName); {$EndIf}
                  bbgrid := GridBoundingBox(DEM,RefDEM,true);
                  DoElevations;
                  DoSlopes;
                  DoRoughness;

                  TStr := TestAreaName + '   ' + DEMIXTile + '  ' + RefType + '  elev=' + IntToStr(NumElev) +  '  slope=' + IntToStr(NumSlope) +  '  ruff=' + IntToStr(NumRuff)  +
                        '  total=' + IntToStr(NumElev+NumSlope+NumRuff);
                  wmdem.SetPanelText(3,TStr);
                  {$If Defined(RecordTileProcessing) or Defined(RecordFullDEMIX)} WriteLineToDebugFile(TStr); {$EndIf}
               end;
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
            end
            else begin
               {$IfDef RecordTileStats} WriteLineToDebugFile('Not doing tile=' + DEMIXTile); {$EndIf}
            end;
         end;


   label
      NoLandCover;
   var
      Ser,i,j,UseDSM,UseDTM : integer;
      TStr : shortstring;
      LandCoverFName : PathStr;
      //DEMIXtileDB2 : integer;
      CandidateBoundBoxGeo,bb : sfBoundBox;
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

      if LoadDEMIXReferenceDEMs(DEMIXRefDEM,true) then begin
         DEMIXtileDB := DEMIXtileFill(DEMIXRefDEM,DEMGlb[DEMIXRefDEM].DEMBoundBoxGeo,false);

         //option below does not work; it would mean the maps did not have to be opened if we could get it working
         //DEMIXtileDB := LoadDEMIXtileOutlinesNoMap(DEMGlb[DEMIXRefDEM].DEMBoundBoxGeo,true);

         GISdb[DEMIXtileDB].ApplyGISFilter('GRID_FULL<' + IntToStr(MDDef.DEMIX_Full));
         {$IfDef RecordDEMIX} WriteLineToDebugFile('DTM tiles=' + IntToStr(GISdb[DEMIXtileDB].MyData.FiltRecsInDB)); {$EndIf}
         GISdb[DEMIXtileDB].DeleteAllSelectedRecords;

         if ValidDEM(RefDSMpoint) then begin
            CandidateBoundBoxGeo.XMin := MinFloat(DEMGlb[DEMIXRefDEM].DEMBoundBoxGeo.XMin,DEMGlb[RefDTMarea].DEMBoundBoxGeo.XMin);
            CandidateBoundBoxGeo.yMin := MinFloat(DEMGlb[DEMIXRefDEM].DEMBoundBoxGeo.YMin,DEMGlb[RefDTMarea].DEMBoundBoxGeo.YMin);
            CandidateBoundBoxGeo.Xmax := MaxFloat(DEMGlb[DEMIXRefDEM].DEMBoundBoxGeo.Xmax,DEMGlb[RefDTMarea].DEMBoundBoxGeo.Xmax);
            CandidateBoundBoxGeo.ymax := MaxFloat(DEMGlb[DEMIXRefDEM].DEMBoundBoxGeo.Ymax,DEMGlb[RefDTMarea].DEMBoundBoxGeo.Ymax);
         end
         else begin
            CandidateBoundBoxGeo := DEMGlb[DEMIXRefDEM].DEMBoundBoxGeo
         end;

         if (GISdb[DEMIXtileDB].MyData.FiltRecsInDB = 0) then begin
            TStr := 'No filled DEMIX tiles on ' + TestAreaName;
            ErrorLog.Add(Tstr);
            {$IfDef RecordDEMIX} WriteLineToDebugFile(TStr); {$EndIf}
         end
         else begin
            {$If Defined(RecordFullDEMIX) or Defined(TrackDEMIX_DEMs)} OpenDEMsToDebugFile('DEMs start loading'); {$EndIf}
            GISdb[DEMIXtileDB].MyData.First;  //placed here to see where in the tile processing the program is at

            if LoadDEMIXCandidateDEMs(TestAreaName,DEMIXRefDEM,true) then begin
               fName := DEMIX_area_lc100 + TestAreaName + '.tif';
               if FileExists(fname) then begin
                  LandCoverGrid := OpenNewDEM(fName);
                  if DEMGlb[LandCoverGrid].DEMHeader.ElevUnits <> GLCS_LC100 then begin
                     DEMGlb[LandCoverGrid].DEMHeader.ElevUnits := GLCS_LC100;
                     DEMGlb[LandCoverGrid].WriteNewFormatDEM(fName);
                  end;
               end
               else begin
                  bb := DEMGlb[DEMIXRefDEM].SelectionMap.MapDraw.MapCorners.BoundBoxGeo;
                  Lat := 0.5 * (bb.YMax + bb.YMin);
                  Long := 0.5 * (bb.XMax + bb.XMin);
                  LandCoverFName := GetLC100_fileName(Lat,Long);
                  {$IfDef RecordDEMIX} writeLineToDebugFile('Landcover=' + LandCoverfName); {$EndIf}
                  if FileExists(LandCoverFName) then begin
                     LandCoverGrid := GDALsubsetimageandopen(bb,true,LandCoverFName);
                     if ValidDEM(LandCoverGrid) then begin
                        DEMGlb[LandCoverGrid].DEMHeader.ElevUnits := GLCS_LC100;
                        DEMGlb[LandCoverGrid].WriteNewFormatDEM(fName);
                     end
                     else begin
                        TStr := 'Load landcover error on ' + TestAreaName + ' ' + LandCoverFName;
                        ErrorLog.Add(Tstr);
                        {$IfDef RecordDEMIX} WriteLineToDebugFile(TStr); {$EndIf}
                        goto NoLandCover;
                     end;
                  end
                  else begin
                     TStr := 'Missing landcover file on ' + TestAreaName + ' ' + LandCoverFName;
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
                           {$If Defined(RecordFullDEMIX) or Defined(RecordDEMIXLoops) or Defined(RecordCriteriaEvaluation)}
                              WriteLineToDebugFile(DEMIXTile + ' Start tests for DEM=' + IntToStr(TestDEM[i]) + '/' + IntToStr(Ser) +
                                  '  Series=' + TestSeries[i] + '  DEM=' + DEMGlb[TestDEM[i]].AreaName);
                           {$EndIf}
                           GetReferenceDEMsForTestDEM(TestSeries[i],UseDSM,UseDTM);
                           CompareDifferencesToReferenceDEM(TestDEM[i],UseDTM,'DTM');
                           CompareDifferencesToReferenceDEM(TestDEM[i],UseDSM,'DSM');
                           {$If Defined(RecordFullDEMIX)} writeLineToDebugFile('All tests done for ' + TestSeries[i]); {$EndIf}
                        end
                        else begin
                           TStr := 'Missing test dem=' + IntToStr(i) + ' for ' + TestAreaName;
                           ErrorLog.Add(Tstr);
                           {$If Defined(RecordDEMIX) or Defined(RecordCriteriaEvaluation)} WriteLineToDebugFile(TStr); {$EndIf}
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
               {$If Defined(RecordDEMIX) or Defined(RecordCriteriaEvaluation)}  WriteLineToDebugFile(TStr); {$EndIf}
            end;
            {$If Defined(RecordDEMIX) or Defined(RecordCriteriaEvaluation)}  writeLineToDebugFile('call CloseAndNilNumberedDB(DEMIXtileDB)'); {$EndIf}
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
   //DataPath,
   DTMArea : pathStr;
   i : integer;
   //DefaultFilter : byte;
   //TStr : shortstring;
   //aTable : tMyData;
   FilesWanted : tStringList;
begin {ComputeDEMIX_tile_stats}
   {$IfDef RecordDEMIX} writeLineToDebugFile('Start ComputeDEMIXstats'); {$EndIf}
   try
      GetDEMIXpaths;
      ErrorLog := tStringList.Create;
      SaveBackupDefaults;
      MDdef.ConfirmDBEdits := false;
      MDdef.DefaultMapXSize := 800;
      MDdef.DefaultMapYSize := 800;

      LandTypeMask := 'ALL';

      //if DoHorizontalShift then SafeMakeDir(DEMIXresultsDir + 'lags\' );

      FilesWanted := DEMIX_AreasWanted(AreaName);

      {$IfDef RecordDEMIX} writeLineToDebugFile('Areas to process=' + IntToStr(FilesWanted.Count)); {$EndIf}

      for i := 0 to pred(FilesWanted.Count) do begin
         wmdem.SetPanelText(1,'Area: ' + IntToStr(succ(i)) + '/' + IntToStr(FilesWanted.Count));
         cFName := FilesWanted.Strings[i];
         if StrUtils.AnsiContainsText(UpperCase(cfName),'_DSM') then begin
            //this will be the done same time as its DTM
         end
         else begin
            TestAreaName := ShortTestAreaName(ExtractFileNameNoExt(cfName));
            DTMArea := ExtractFileNameNoExt(cfName);
            if DEMIXSkipFilesAlreadyDone and FileExists(DEMIXresultsDir + TestAreaName + '_Elev_diff_stats.csv') and
                  FileExists(DEMIXresultsDir + TestAreaName + '_Slope_diff_stats.csv') and
                  FileExists(DEMIXresultsDir + TestAreaName + '_Ruff_diff_stats.csv') then begin
               {$IfDef RecordDEMIX} writeLineToDebugFile(TestAreaName + ' already processed'); {$EndIf}
            end
            else begin
               DeleteFileIfExists(DEMIXresultsDir + TestAreaName + '_Elev_diff_stats.csv');
               DeleteFileIfExists(DEMIXresultsDir + TestAreaName + '_Slope_diff_stats.csv');
               DeleteFileIfExists(DEMIXresultsDir + TestAreaName + '_Ruff_diff_stats.csv');
               DeleteFileIfExists(DEMIXresultsDir + TestAreaName + '_Elev_diff_stats.dbf');
               DeleteFileIfExists(DEMIXresultsDir + TestAreaName + '_Slope_diff_stats.dbf');
               DeleteFileIfExists(DEMIXresultsDir + TestAreaName + '_Ruff_diff_stats.dbf');

               RefDTMPointFName := DEMIX_Ref_1sec + DTMArea + '_ref_1sec_point.tif';
               RefDTMareaFName := StringReplace(RefDTMPointFName, 'point', 'area',[rfIgnoreCase]);
               COPRefDTMFName := StringReplace(RefDSMPointFName, '1sec', '1.5x1sec',[rfIgnoreCase]);
               COPRefDSMFName := StringReplace(COPRefDTMFName, 'dtm', 'dsm',[rfIgnoreCase]);

               if not FileExists(RefDTMPointFName) then RefDTMPointFName := '';
               if not FileExists(RefDTMareaFName) then RefDTMareaFName  := '';
               if not FileExists(COPRefDTMFName) then COPRefDTMFName := '';
               if not FileExists(COPRefDSMFName) then COPRefDSMFName := '';

               if StrUtils.AnsiContainsText(DTMArea,'_dtm') then begin
                  RefDSMpointFName := StringReplace(RefDTMpointFName, 'dtm', 'dsm',[rfIgnoreCase]);
                  RefDSMareaFName := StringReplace(RefDTMareaFName, 'dtm', 'dsm',[rfIgnoreCase]);
                  if not FileExists(RefDSMPointFName) then RefDSMPointFName := '';
                  if not FileExists(RefDSMareaFName) then RefDSMareaFName := '';
               end
               else begin
                  RefDSMPointFName := '';
                  RefDSMareaFName := '';
               end;

               if FileExists(RefDTMPointFName) and FileExists(RefDTMareaFName) then begin
                  {$IfDef RecordDEMIX} writeLineToDebugFile('Start process for ' + TestAreaName); {$EndIf}
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
      EndDEMIXProcessing;
      RestoreBackupDefaults;
      DisplayAndPurgeStringList(ErrorLog,'DEMIX_VDatum_shifts Problems');
   end;
   {$IfDef RecordDEMIX} writeLineToDebugFile('End ComputeDEMIXstats'); {$EndIf}
end {ComputeDEMIX_tile_stats};



procedure DEMIX_VDatum_shifts;
//processes any csv files created by VDATUM, which are then converted to dbf files and the DEM shifted in x, y, and z
var
  fName{,fName2} : PathStr;
  AreaName{,TStr} : shortstring;
  //Merged,
  Shifts,ErrorLog : tStringList;
  i,{j,}db{,DEM}  : Integer;
  //dx,dy,dz : float32;
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
            VerticalDatumShiftWithVDATUM(AreaName,0,db,fName,ErrorLog);
            (*
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
            *)
         end;
      end;
   finally
       EndDEMIXProcessing;
       Shifts.Free;
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


procedure DEMIX_merge_Visioterra_source(AreaName : shortstring = '');
var
   Areas,{DEMs,ASCIIDEMs,}ErrorLog : tStringList;
   //i,VDatumCode,
   Fixed,NewDEM,AnArea : integer;
   AreaMergeName, {fName,} AreaPath,
   //VDatumListFName,VDatumFilesFName : PathStr;
   TStr : shortstring;


        procedure Merge(AreaPath : PathStr; SearchName,AreaMergeName : shortstring);
        var
           DEMs : tStringList;
        begin
            DEMs := Nil;
            FindMatchingFiles(AreaPath,SearchName,DEMs,5);
            if (DEMs.Count > 0) then begin
               wmdem.SetPanelText(3, 'Merge DEMs=' + IntToStr(DEMs.Count));
               {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Merge files= ' + IntToStr(DEMs.Count) + ' for ' + AreaMergeName); {$EndIf}
               if (DEMs.Count = 1) then NewDEM := OpenNewDEM(DEMs.Strings[0],false)
               else NewDEM := MergeMultipleDEMsHere(DEMs,false,false);  //Frees DEMs
               {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Merge files over, MinZ=' + RealToString(DEMGlb[NewDEM].DEMheader.MinElev,-12,-2)); {$EndIf}
               if FilterOutSeaLevel then begin
                  if (abs(DEMGlb[NewDEM].DEMheader.MinElev) < 0.001) then begin
                     //mark sea level as missing for analysis along coast
                     DEMGlb[NewDEM].MarkInRangeMissing(-0.001,0.001,Fixed);
                     {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Sea level missing done, pts removed=' + IntToStr(Fixed)); {$EndIf}
                  end;
               end;
               DEMGlb[NewDEM].DEMheader.VerticalCSTypeGeoKey := VertCSEGM2008;
               ShowHourglassCursor;
               wmdem.SetPanelText(3, 'Write DEM ' + AreaMergeName);
               DEMGlb[NewDEM].CheckMaxMinElev;
               DEMGlb[NewDEM].WriteNewFormatDEM(AreaMergeName);
               {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Merge saved to ' + AreaMergeName); {$EndIf}
               CloseSingleDEM(NewDEM);
               CleanUpTempDirectory;  //might be many tiled or compressed DEMs expanded
            end
            else begin
               TStr := 'No ' + SearchName + ' files found in ' + AreaPath;
               {$If Defined(RecordDEMIX)} WriteLineToDebugFile(TStr); {$EndIf}
               ErrorLog.Add(TStr);
            end;
       end;


begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIX_merge_source in'); {$EndIf}
   try
      GetDEMIXPaths(true);
      ErrorLog := tStringList.Create;
      Areas := DEMIX_AreasWanted (AreaName);
      if (Areas.Count = 0) then begin
         {$If Defined(RecordDEMIX)} WriteLineToDebugFile('No areas selected'); {$EndIf}
      end
      else begin
         for anArea := 0 to pred(Areas.Count) do begin
            wmdem.SetPanelText(2, 'Area: ' + IntToStr(succ(anArea)) + '/' + IntToStr(Areas.Count));
            AreaName := Areas.Strings[AnArea];
            AreaPath := DEMIX_Ref_Source + AreaName + '\';
            AreaMergeName := DEMIX_Ref_1sec + AreaName + '_ref_1sec_area.tif';
            Merge(AreaPath,'*_pia.tif',AreaMergeName);
            AreaMergeName := DEMIX_Ref_1sec + AreaName + '_ref_1sec_point.tif';
            Merge(AreaPath,'*_pip.tif',AreaMergeName);
          end {for area};
      end;
      Areas.Free;
   finally
      EndDEMIXProcessing;
      DisplayAndPurgeStringList(ErrorLog,'DEMIX_merge_source problems');
   end;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIX_merge_source out'); {$EndIf}
end;



procedure DEMIX_merge_source(AreaName : shortstring = '');
var
   Areas,DEMs,{ASCIIDEMs,}ErrorLog : tStringList;
   //i,
   Fixed,VDatumCode,NewDEM,AnArea,LocalToWGS84,WGS84toEGM2008 : integer;
   AreaMergeName, fName, AreaPath,
   VDatumListFName,VDatumFilesFName : PathStr;
   TStr : shortstring;
   VDatumList,VDatumFiles : tMyData;
begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIX_merge_source in'); {$EndIf}
   try
      GetDEMIXPaths(true);
      ErrorLog := tStringList.Create;
      Areas := DEMIX_AreasWanted (AreaName);
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
            if DEMIXSkipFilesAlreadyDone and FileExists(AreaMergeName) then begin
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
                     if (DEMs.Count = 1) then NewDEM := OpenNewDEM(DEMs.Strings[0],false)
                     else NewDEM := MergeMultipleDEMsHere(DEMs,false,false);  //Frees DEMs
                     {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Merge files over, MinZ=' + RealToString(DEMGlb[NewDEM].DEMheader.MinElev,-12,-2)); {$EndIf}
                     if FilterOutSeaLevel then begin
                        if (abs(DEMGlb[NewDEM].DEMheader.MinElev) < 0.001) then begin
                           //mark sea level as missing for analysis along coast
                           DEMGlb[NewDEM].MarkInRangeMissing(-0.001,0.001,Fixed);
                           {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Sea level missing done, pts removed=' + IntToStr(Fixed)); {$EndIf}
                        end;
                     end;
                     DEMGlb[NewDEM].DEMheader.VerticalCSTypeGeoKey := VDatumCode;

                     if (VDatumCode = VertCSNAVD88)  then begin
                        //no longer doing this
                        fName := vd_path + AreaName + '.csv';
                        if not FileExists(fName) then begin
                           DEMGlb[NewDEM].CSVforVDatum(0.2,fName);
                           TStr := AreaName + ' VDatumCSV created for NAVD88; run through NOAA VDATUM';
                           {$If Defined(RecordDEMIX)} WriteLineToDebugFile(TStr); {$EndIf}
                           ErrorLog.Add(TStr);
                        end;
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
      EndDEMIXProcessing;
      CloseSingleDEM(WGS84toEGM2008);
      DisplayAndPurgeStringList(ErrorLog,'DEMIX_merge_source problems');
   end;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIX_merge_source out'); {$EndIf}
end;


procedure DEMIX_CreateReferenceDEMs(AreaName : shortstring = '');
var
   fName : PathStr;
   ErrorLog,Areas : tStringList;
   i,{j,}WantedDEM : integer;
   TStr : shortstring;
begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXreferenceDEMs in'); {$EndIf}
   try
      GetDEMIXPaths;
      Areas := tStringList.Create;
      ErrorLog := tStringList.Create;
      Areas := DEMIX_AreasWanted(AreaName);

      if (Areas.Count > 0) then begin
        for i := 0 to pred(Areas.Count) do begin
           AreaName := Areas.Strings[i];  //(Areas[i]);
           wmdem.SetPanelText(2, 'Area: ' + IntToStr(succ(i)) + '/' + IntToStr(Areas.Count));
           wmdem.SetPanelText(3, AreaName);
           fName := DEMIX_Ref_1sec + AreaName + '_ref_1sec_area.tif';
           if DEMIXSkipFilesAlreadyDone and FileExists(fName) then begin
              {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXreferenceDEMs already done ' + fName ); {$EndIf}
           end
           else begin
              {$If Defined(RecordDEMIX)} HighlightLineToDebugFile('DEMIXreferenceDEMs for ' + AreaName); {$EndIf}
              fName := DEMIX_Ref_Merge + AreaName + '.dem';
              if FileExists(fName) then begin
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
                    ResampleForDEMIXOneSecDEMs(WantedDEM,false,DEMIX_Ref_1sec,ResampleModeOneSec);
                 end;
                 CloseSingleDEM(WantedDEM);
              end
              else begin
                 TStr := AreaName  + ' no merged DEM';
                 {$If Defined(RecordDEMIX)} WriteLineToDebugFile(TStr); {$EndIf}
                 ErrorLog.Add(TStr);
              end;
           end;
        end;
      end
      else begin
          {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXreferenceDEMs, no areas ' + DEMIX_Ref_Merge); {$EndIf}
      end;
   finally
      Areas.Free;
      EndDEMIXProcessing;
      DisplayAndPurgeStringList(ErrorLog,'DEMIX_CreateReferenceDEMs Problems');
   end;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXreferenceDEMs out'); {$EndIf}
end;


procedure DEMIXisCOPorALOSbetter(DBonTable : integer);
var
   //which,
   RefFilter : shortstring;
   Compare,i,j,{ties,}opinions,db : integer;
   fName : PathStr;
   Findings,Criteria,DEMs : tStringList;


   procedure DoOne(Header,theFilter : shortstring);
   var
      //Total,
      Cop,ALOS,Ties,FAB,dem : integer;
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
       i,j,CurrentY,DEM,Center,NumTies : integer;
       Criterion,Best,TStr,AreaFilter : shortstring;
       aDEM : array[1..10] of shortstring;
       Graph : tThisBaseGraph;
       rfile : array[1..10] of file;
       v : array[1..2] of float32;
       Symbol : tFullSymbolDeclaration;
       fName : PathStr;
       //bmp : tMyBitmap;
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
            for j := 1 to NumLandTypes do begin
               RefFilter := 'REF_TYPE=' + QuotedStr(RefDEMType[i]) + ' AND LAND_TYPE=' + QuotedStr(LandTypes[j]);
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

(*
      procedure ExperimentalOption;
      var
         DEMsPresent : tstringList;
         Symbol : tFullSymbolDeclaration;
         DEM,OnTile : integer;
         rfile : array[1..10] of file;
         v : array[1..2] of float32;
         i,x : integer;
         fName : PathStr;
         found : boolean;


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
      begin
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
      end;
      *)

begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXwineContestScoresGraph in, table=' + IntToStr(DBontable)); {$EndIf}

   if not GISdb[DBonTable].MyData.FieldExists('FILTER') then begin
      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('DEMIXwineContestScoresGraph in, requires BestDEMSbyCategory'); {$EndIf}
      BestDEMSbyCategory(DBonTable);
   end
   else begin
      //this has not been used in quite some time, and probably does not add much
      //ExperimentalOption;
   end;
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
                  aFilter := TheDEMs[i] + '=' + IntToStr(j)  +  ' AND REF_TYPE=' + QuotedStr(RefDEMType[DEM]);
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



procedure ResampleForDEMIXOneSecDEMs(DEM : integer; OpenMap : boolean = false; OutPath : PathStr = ''; ResampleMode : byte = 1);
var
   NewDEM,{New2,New3,}NewDEM4,NewDEM5{,PointOffset,AreaOffset} : integer;
   fName,BaseName : PathStr;
   Ext : ExtStr;
   //xfrac,yfrac : float64;


   function CreateOne(PixelIs : byte; xgridsize,ygridsize : float32; fName : PathStr) : integer;
   begin
      if FileExists(fName) then begin
         Result := OpenNewDEM(fName);
      end
      else begin
         MDdef.DefLidarGeoGridSizeX := xgridsize;
         MDdef.DefLidarGeoGridSizeY := ygridsize;
         MDDef.LasDEMPixelIs := PixelIs;
         {$If Defined(RecordCreateGeomorphMaps) or Defined(RecordDEMIX)} WriteLineToDebugFile('ResampleForDEMIX, ' + fName); {$EndIf}
         Result := DEMGlb[DEM].ResampleByAveraging(OpenMap,false,fName);
         {$If Defined(TrackDEMCorners)} DEMGlb[Result].WriteDEMCornersToDebugFile('ResampleForDEMIXOneSecDEMs, new DEM=' + IntToStr(Result)); {$EndIf}
      end;
   end;


begin
   {$If Defined(RecordCreateGeomorphMaps) or Defined(RecordDEMIX)} WriteLineToDebugFile(''); WriteLineToDebugFile('ResampleForDEMIX DEM=' + IntToStr(DEM) + '  ' + DEMGlb[DEM].AreaName); {$EndIf}
   if (DEMGlb[DEM].DEMFileName = '') then begin
      DEMGlb[DEM].WriteNewFormatDEM(DEMGlb[DEM].DEMFileName,' save DEM before resampling');
   end;
   {$If Defined(TrackDEMCorners)} DEMGlb[DEM].WriteDEMCornersToDebugFile('ResampleForDEMIXOneSecDEMs, starting DEM'); {$EndIf}
   SaveBackupDefaults;
   GetDEMIXPaths(false);
   MDDef.DefDEMMap := mtDEMBlank;
   MDDef.LidarGridProjection := ArcSecDEM;
   Ext := '.tif';

   if (OutPath = '') then begin
      OutPath := ExtractFilePath(DEMGlb[DEM].DEMFileName);
      BaseName := 'ref_';
   end
   else begin
      BaseName := DEMGlb[DEM].AreaName + '_ref_';
   end;

   if ResampleMode in [ResampleModeBoth,ResampleModeHalfSec] {or MDDef.UseHalfPixelAggregation} then begin
      fName := DEMIX_Ref_Half_sec + BaseName + '0.5sec' + Ext;
      NewDEM := CreateOne(PixelIsPoint,0.5,0.5,fName);
   end;

(*
   if true {MDDef.UseHalfPixelAggregation} then begin
      if DEMGlb[NewDEM].PixelCenterOnFullSecond then begin
         //sw corner is on a full seccond
         PointOffset := 0;
         AreaOffset := 1;
      end
      else begin
         //sw corner is on a half seccond
         PointOffset := 1;
         AreaOffset := 0;
      end;

      fName := OutPath + BaseName + '1sec_point_v2' + Ext;
      New2 := DEMGlb[NewDEM].HalfPixelAggregation(fName,PixelIsPoint,true,PointOffset);
      //this one does not have the correct shift
      fName := OutPath + BaseName + '1sec_area_v2' + Ext;
      New3 := DEMGlb[NewDEM].HalfPixelAggregation(fName,PixelIsArea,true,AreaOffset);
   end;
*)
   if ResampleMode in [ResampleModeBoth,ResampleModeOneSec] then begin
      fName := OutPath + BaseName + '1sec_point' + Ext;
      NewDEM4 := CreateOne(PixelIsPoint,1,1,fName);

      fName := OutPath + BaseName + '1sec_area' + Ext;
      NewDEM5 := CreateOne(PixelIsArea,1,1,fName);

      if (DEMGlb[DEM].DEMSWcornerLat > 50) and (DEMGlb[DEM].DEMSWcornerLat < 60) then begin
         fName := OutPath + BaseName + '1.5x1sec_point' + Ext;
         CreateOne(PixelIsPoint,1.5,1,'1.5x1sec' + Ext);
      end;
   end;

   CloseAllDEMs;
   RestoreBackupDefaults;
   //{$If Defined(RecordCreateGeomorphMaps) or Defined(RecordDEMIX)} WriteLineToDebugFile('Out ResampleForDEMIX DEM=' + IntToStr(DEM) + '  ' + DEMGlb[DEM].AreaName); {$EndIf}
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
   TestGrid,DSMgrid,DTMGrid,
   //DiffGrid,NoSlopeMap,
   i,UseDSM,UseDTM : integer;
   //fName : PathStr;
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
                              DTMElevDiffMapALOS := MakeDifferenceMap(HalfSecRefDTM,TestDEM[1],HalfSecRefDTM,HalfSecRefDTM,true,false,false);
                              DTMElevDiffMapCOP := MakeDifferenceMap(HalfSecRefDTM,TestDEM[2],HalfSecRefDTM,HalfSecRefDTM,true,false,false);
                              DTMSlopeDiffMapALOS := MakeDifferenceMap(RefSlopeMap,ALOSslope,RefSlopeMap,HalfSecRefDTM,true,false,false);
                              DTMSlopeDiffMapCOP := MakeDifferenceMap(RefSlopeMap,COPSlope,RefSlopeMap,HalfSecRefDTM,true,false,false);
                              DTMRuffDiffMapALOS := MakeDifferenceMap(RefRuffMap,ALOSRuff,RefRuffMap,HalfSecRefDTM,true,false,false);
                              DTMRuffDiffMapCOP := MakeDifferenceMap(RefRuffMap,COPRuff,RefRuffMap,HalfSecRefDTM,true,false,false);
                           end;
                            {$IfDef RecordDEMIX} writeLineToDebugFile('DEMIX_GeomorphMapsBestDEM done'); {$EndIf}
                        end;

                        if DEMIX_HalfSecondCompareMaps then begin
                           {$IfDef RecordDEMIX} writeLineToDebugFile('Half sec candidates done, Make difference map'); {$EndIf}
                           COP_ALOS_Diff := MakeDifferenceMap(TestDEM[1],TestDEM[2],TestDEM[1],TestDEM[1],true,false,false,'COP-ALOS_difference');
                           (*
                           {$IfDef RecordDEMIX} writeLineToDebugFile('Try ref DSM COP-ALOS'); {$EndIf}
                           COP_ALOS_DSM4 := TwoDEMHighLowMap(HalfSecRefDSM, TestDEM[1],TestDEM[2],HalfSecRefDSM, MDDef.DEMIXSimpleTolerance,true,'COP-ALOS_compare_DSM-4');
                           COP_ALOS_DSM9 := TwoDEMHighLowMap(HalfSecRefDSM, TestDEM[1],TestDEM[2],HalfSecRefDSM, MDDef.DEMIXSimpleTolerance,false,'COP-ALOS_compare_DSM-9');

                           {$IfDef RecordDEMIX} writeLineToDebugFile('Try ref DTM COP-ALOS'); {$EndIf}
                           COP_ALOS_DTM4 := TwoDEMHighLowMap(HalfSecRefDTM, TestDEM[1],TestDEM[2],HalfSecRefDSM, MDDef.DEMIXSimpleTolerance,true,'COP-ALOS_compare_DTM-4');
                           COP_ALOS_DTM9 := TwoDEMHighLowMap(HalfSecRefDTM, TestDEM[1],TestDEM[2],HalfSecRefDSM, MDDef.DEMIXSimpleTolerance,false,'COP-ALOS_compare_DTM-9');
                          *)
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
                     chm := MakeDifferenceMapOfBoxRegion(HalfSecRefDTM,HalfSecRefDSM,HalfSecRefDTM,0,DEMGlb[HalfSecRefDTM].FullDEMGridLimits,true,false,false,AreaName + '_half_sec_chm');
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
         EndDEMIXProcessing;
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
   WantDEM,WantImage,Ser,i,NumPts,GeoidGrid{,NewDEM} : integer;
   fName,SaveName : Pathstr;
   //Spacing : float32;


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
   {$If Defined(RecordDEMIXLoad)} writeLineToDebugFile('LoadDEMIXCandidateDEMs in; Open DEMs=, ' + IntToStr(NumDEMdatasetsOpen) + '   RefDEM=' + IntToStr(RefDEM)); {$EndIf}
   Result := false;
   {$IfDef RecordDEMIX} AllDEMs := ''; {$EndIf}
   wmdem.SetPanelText(3,'');

   for I := 1 to MaxTestDEM do begin
      TestDEM[i] := 0;
      TestSeries[i] := '';
   end;

   AreaName := ShortTestAreaName(AreaName);
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
               //fName := DEMIX_test_dems + AreaName + '_' + shortname + '.dem';
               DEMGlb[TestDEM[Ser]].WriteNewFormatDEM(SaveName);
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



function LoadDEMIXReferenceDEMs(var RefDEM : integer; OpenMaps : boolean = true) : boolean;

         procedure ReferenceFileOpen(var DEM : integer; fName : PathStr);
         begin
            if FileExists(fName) then begin
               DEM := OpenNewDEM(FName,OpenMaps);   //must load map for the DEMIX tile computation
               if ValidDEM(DEM) and (RefDEM = 0) then RefDEM := DEM;
               {$If Defined(RecordDEMIXRefDEMopen)} writeLineToDebugFile('RefDEM=' + IntToStr(DEM) + '  ' + DEMGlb[DEM].AreaName); {$EndIf}
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
   MaxSum : float64;
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
   j,ScoresDB : integer;
   Graph : tThisBaseGraph;
   bmp : tMyBitmap;
   LegendfName : PathStr;
   BigBitmap : tStringList;
   anImage : TImageDisplayForm;
begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('BestDEMSbyCategory in, ' + GISdb[DBonTable].dbName); {$EndIf}
   GetDEMIXPaths;
   Criteria := tStringList.Create;
   Criteria.LoadFromFile(DEMIXSettingsDir + 'criteria_all.txt');

   DEMs := tStringList.Create;
   DEMs.LoadFromFile(DEMIXSettingsDir + 'demix_dems_all_six.txt');
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
      for j := 1 to NumLandTypes do begin
         DoOne(RefDEMType[i] + ' ' + LandTypes[j] + ' pixels','LAND_TYPE=' + QuotedStr(LandTypes[j]) + RefFilter);
      end;
      AverageScore.Add('SKIP');

      //if GISdb[DBonTable].MyData.FieldExists('PC_BARREN') then n := 9 else n := 8;
      for J := 1 to 9 do begin
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
      if (Graph.GraphDraw.LegendList <> nil) and (Graph.GraphDraw.LegendList.Count > 0) then begin
         LegendfName := NextFileNumber(MDTempDir,'legend_','.png');
         bmp := Graph.MakeLegend(Graph.GraphDraw.LegendList,false);
         SaveBitmap(bmp,LegendfName);
         Bmp.free;
      end;

      fName := NextFileNumber(MDTempDir,'best_graph_','.png');
      SaveImageAsBMP(Graph.Image1,fName);
      BigBitmap.Add(fName);
      BigBitmap.Add(LegendfName);
      fName := NextFileNumber(MDTempDir,RefDEMType[i] + '_best_graph_with_legend_','.png');
      anImage := MakeBigBitmap(BigBitmap,'',fName,2);
      anImage.Destroy;

      //DisplayBitmap(fName,'');

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
   //ShowDefaultCursor;
   EndDEMIXProcessing;

   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('BestDEMSbyCategory out, ' + GISdb[DBonTable].dbName); {$EndIf}
end;


procedure ModeSTDPlot(DBonTable : integer);
const
   Params : array[1..3] of shortstring = ('elvd_mode','slpd_mode','rufd_mode');
var
   i,j : integer;
   ThisGraph : array[1..3] of TThisbasegraph;
   rfile : array[1..3] of file;
   v : array[1..3] of float32;
begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('ModeSTDPlot in, ' + GISdb[DBonTable].dbName); {$EndIf}
   with GISdb[DBonTable] do begin
      for I := 1 to 3 do  begin
         MyData.First;
         ApplyGISFilter('PARAM='+ QuotedStr(Params[i]) + ' AND REF_DEM=' + QuotedStr('DTM'));
         EmpSource.Enabled := false;
         ThisGraph[i] := TThisbasegraph.Create(Application);
         ThisGraph[i].GraphDraw.HorizLabel := Params[i];
         ThisGraph[i].GraphDraw.VertLabel := 'Standard Deviation';
         ThisGraph[i].OpenXYColorFile(rfile[i]);
         while not MyData.eof do begin
            for j := 1 to NumDEMIXDEM do begin
                v[1] := MyData.GetFieldByNameAsFloat(DEMIXshort[j] + '_MODE');
                v[2] := MyData.GetFieldByNameAsFloat(DEMIXshort[j] + '_STD');
                v[3] := ConvertPlatformColorToTColor(DEMIXColorFromDEMName(DEMIXDEMTypeName[j]));
                BlockWrite(rfile[i],v,1);
            end;
            MyData.Next;
         end;
         CloseFile(rfile[i]);
         ThisGraph[i].AutoScaleAndRedrawDiagram(true,true);
         ThisGraph[i].RedrawDiagram11Click(Nil);
         {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Done graph ' + IntToStr(i)); {$EndIf}
      end;
      EmpSource.Enabled := true;
      ClearGISFilter;
   end;
end;


procedure DEMIXMeanMedianModeHistograms(db : integer);
const
   Params : array[1..3] of shortstring = ('elvd_','slpd_','rufd_');
   ParamsLong : array[1..3] of shortstring = ('Elevation','Slope','Roughness');
   Stats : array[1..4] of shortstring = ('mean','medn','mode','std');
   StatsLong : array[1..4] of shortstring = ('Mean','Median','Mode','Std dev');
var
   Min,Max,BinSize : float32;
   Corner : byte;


   procedure MakeOne(j2,i2 : integer);
   var
      zvs : array[1..MaxTestDEM] of ^Petmath.bfarray32;
      Param,Stat : shortString;
      i,npts,DEM : integer;
      Dist,Legs : tStringList;
      Graph : tThisBaseGraph;
      TStr : shortstring;
   begin
      Param := Params[j2];
      Stat := Stats[i2];
      Dist := tStringList.Create;
      Legs := tStringList.Create;
      for DEM := 1 to MaxTestDEM do begin
         Legs.Add(DEMIXDEMTypeName[DEM]);
      end;
      GISdb[DB].ApplyGISFilter('REF_TYPE=' + QuotedStr('DTM') + ' AND CRITERION=' + QuotedStr(Param + 'MODE'));
      GISdb[DB].EmpSource.Enabled := false;
      if GISdb[db].MyData.FiltRecsInDB = 0 then begin
         {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Nothing for ' + GISdb[db].MyData.Filter); {$EndIf}
      end
      else begin
         for I := 1 to MaxTestDEM do New(zvs[i]);
         npts := 0;
          GISdb[DB].MyData.First;
          while not GISdb[DB].MyData.eof do begin
             for DEM := 1 to MaxTestDEM do begin
                zvs[DEM]^[npts] := GISdb[DB].MyData.GetFieldByNameAsFloat(DEMIXshort[DEM]  + '_' + Stat );
             end;
             inc(Npts);
             GISdb[DB].MyData.Next;
          end;

          for I := 1 to MaxTestDEM do begin
             Dist.Add(SaveSingleValueSeries(npts,zvs[i]^));
             Dispose(zvs[i]);
          end;
          TStr := StatsLong[i2] + '_' + Paramslong[j2] + '_difference_compared_to_reference_DTM';
          Graph := CreateMultipleHistogram(MDDef.CountHistograms,Dist,Legs,TStr,TStr,100,Min,Max,BinSize);

         for I := 1 to MaxTestDEM do begin
            Graph.GraphDraw.FileColors256[i] := DEMIXColorFromDEMName(Graph.GraphDraw.LegendList[pred(i)]);
         end;
         Graph.GraphDraw.InsideMarginLegend := Corner;
         Graph.RedrawDiagram11Click(Nil);
      end;
   end;

var
   i,j : integer;
begin
   Min := -4;
   Max := 4;
   BinSize := 0.5;
   Corner := lpNWMap;
   for i := 1 to 4 do begin
      if i = 4 then begin
         Min := 0;
         Max := 8;
         Corner := lpNEMap;
      end;
      for j := 1 to 3 do begin
         MakeOne(j,i);  //Params[j],Stats[i]);
      end;
   end;
end;

(*
procedure MakeCriteriaGraph(DB : integer; var DEMsTypeUsing,TilesUsing,LandTypesUsing,CandidateDEMsUsing,CriteriaUsing : tStringList);
//3 Aug 2023, not working and not clear it is worth revising
var
   aFilter,DEMstr : shortstring;
   i,j,k,m,DEM : integer;
   Graph : tThisBaseGraph;
   rfile : array[0..10] of file;
   Symbol : tFullSymbolDeclaration;
   v : array[1..2] of float32;
   AllGraphs : tStringList;
   fName : PathStr;
begin
   {$IfDef RecordFullDEMIX} WriteLineToDebugFile('Start TDemixFilterForm.DoCriteriaGraph'); {$EndIf}
   GISdb[DB].EmpSource.Enabled := false;
   AllGraphs := tStringList.Create;
   for i := 0 to pred(DEMsTypeUsing.Count) do begin
      for j := 0 to pred(TilesUsing.Count) do begin
         for k :=  0 to pred(LandTypesUsing.Count) do begin
            aFilter := 'REF_TYPE=' + QuotedStr(DEMsTypeUsing[i]) + ' AND DEMIX_TILE=' + QuotedStr(TilesUsing[j])+ ' AND LAND_TYPE=' + QuotedStr(LandTypesUsing[k]);
            GISdb[db].ApplyGISFilter(aFilter);
            if (GISdb[db].MyData.FiltRecsInDB > 0) then begin
               {$IfDef RecordFullDEMIX} WriteLineToDebugFile(aFilter); {$EndIf}
               Graph := tThisBaseGraph.Create(Application);

               Graph.GraphDraw.LegendList := tStringList.Create;
               for DEM := 0 to pred(CandidateDEMsUsing.Count) do begin
                  Symbol := SymbolFromDEMName(CandidateDEMsUsing[DEM]);
                  Symbol.DrawingSymbol := FilledBox;
                  Graph.OpenPointFile(rfile[DEM],Symbol);
                  Graph.GraphDraw.LegendList.Add(CandidateDEMsUsing[DEM]);
                  Graph.GraphDraw.LineSize256[DEM] := 2;
               end;

               Graph.Caption := aFilter;
               Graph.GraphDraw.GraphAxes := YFullGridOnly;
               Graph.GraphDraw.MinVertAxis := 999;
               Graph.GraphDraw.MaxVertAxis := -999;
               Graph.GraphDraw.HorizLabel := aFilter;
               Graph.GraphDraw.GraphLeftLabels := tStringList.Create;
               Graph.GraphDraw.GraphBottomLabels := tStringList.Create;
               Graph.GraphDraw.GraphBottomLabels := tStringList.Create;

               GISdb[db].MyData.first;
               while not GISdb[db].MyData.eof do begin
                  DEMstr := GISdb[db].MyData.GetFieldByNameAsString('DEM');
                  DEM := CandidateDEMsUsing.IndexOf(DEMstr);
                  if (DEM <> -1) then begin
                     for m := 0 to pred(CriteriaUsing.Count) do begin
                        Graph.GraphDraw.GraphBottomLabels.Add(IntToStr(m) + ',' + CriteriaUsing[m]);
                        v[1] := m;
                        v[2] := GISdb[db].MyData.GetFieldByNameAsFloat(CriteriaUsing[m]);
                        CompareValueToExtremes(v[2],Graph.GraphDraw.MinVertAxis,Graph.GraphDraw.MaxVertAxis);
                        BlockWrite(rfile[DEM],v,1);
                     end;
                  end;
                  GISdb[db].MyData.Next;
               end;
               Graph.GraphDraw.MinHorizAxis := -0.5;
               Graph.GraphDraw.MaxHorizAxis := CriteriaUsing.Count - 0.5;
               Graph.GraphDraw.MinVertAxis := Graph.GraphDraw.MinVertAxis - 1;
               Graph.GraphDraw.MaxVertAxis := Graph.GraphDraw.MaxVertAxis + 1;
               Graph.GraphDraw.SetShowAllLines(true,2);
               Graph.GraphDraw.VertGraphBottomLabels := false;
               Graph.GraphDraw.ShowVertAxis0 := true;
               Graph.AutoScaleAndRedrawDiagram(false,false,false,false);
               Graph.Height := MDDef.DEMIX_ysize;
               Graph.Width := MDDef.DEMIX_xsize;
               Graph.RedrawDiagram11Click(Nil);

               Graph.Image1.Canvas.Draw(Graph.GraphDraw.LeftMargin+15,Graph.GraphDraw.TopMargin+10,Graph.MakeLegend(Graph.GraphDraw.LegendList,false));
               fName := NextFileNumber(MDTempDir,'big_graph_','.png');
               SaveImageAsBMP(Graph.Image1,fName);
               AllGraphs.Add(fName);
            end;
         end;
      end;
   end;
   fName := NextFileNumber(MDtempDir,'criteria_by_tile_','.png');
   MakeBigBitmap(AllGraphs,'',fName,4);
   DisplayBitmap(fName,'');

   GISdb[DB].ClearGISFilter;
   GISdb[DB].EmpSource.Enabled := true;
   {$IfDef RecordFullDEMIX} WriteLineToDebugFile('End TDemixFilterForm.DoCriteriaGraph'); {$EndIf}
end;
*)


procedure MultipleBestByParametersSortByValue(DBonTable,Option : integer; var DEMsTypeUsing,TilesUsing,LandTypesUsing,CandidateDEMsUsing,CriteriaUsing,TileParameters : tStringList; ByPointFilters : boolean = false);
//const
(*
   NumCriteria = 3;
   Criteria : array[1..NumCriteria] of shortstring = ('ELVD_RMSE','SLPD_MAE','RUFD_RMSE');
   NumParameter = 4;
   Parameter  : array[1..NumParameter] of shortstring = ('RELIEF','AVG_SLOPE','AVG_ROUGH','FOREST_PC');
*)
(*
   NumCriteria = 6;
   Criteria : array[1..NumCriteria] of shortstring = ('ELVD_RMSE','ELVD_MAE', 'SLPD_RMSE','SLPD_MAE','RUFD_RMSE','RUFD_MAE');

   NumTileParameter = 7;
   TileParameter  : array[1..NumTileParameter] of shortstring = ('RELIEF','AVG_ELEV','AVG_SLOPE','AVG_ROUGH','FOREST_PC','URBAN_PC','BARREN_PC');
*)
var
   BigBitmap : tMyBitmap;
   fName : PathStr;
   UsefulWidth,
   GraphPanelsWide,
   LeftStart : integer;
   UseMax : float32;
   aLine : shortstring;
   Statistics : tStringList;


         function OnlyInTieForBestByParameterSorting(DBonTable : integer; UseMax : float32; TileParam,Criterion,DEMtype,LandType,HorizLabel,VertLabel : shortstring) : tThisBaseGraph;
         var
            TileValue : float32;
            rfile: file;
            i,TotalPoss : integer;
            v : array[1..3] of float32;
            Winners,CriterionFilter : shortstring;
            BestCount : array[1..NumDEMIXDEM] of int32;
         begin
             if Criterion = '' then CriterionFilter := CreateFilterOutSignedCriteria(DBonTable)
             else CriterionFilter := 'CRITERION=' + QuotedStr(Criterion);
             for i := 1 to NumDEMIXDEM do BestCount[i] := 0;
             TotalPoss := 0;
             GISdb[DBonTable].ApplyGISFilter('REF_TYPE=' + QuotedStr(DEMType) + ' AND LAND_TYPE=' + QuotedStr(LandType) + ' AND ' + CriterionFilter);
             {$If Defined(RecordDEMIXSortGraph)}
                WriteLineToDebugFile('BestByParameterSorting, param=' + TileParam + ' recs=' + IntToStr(GISdb[DBonTable].MyData.FiltRecsInDB) + '  filter=' + GISdb[DBonTable].MyData.Filter);
             {$EndIf}
             GISdb[DBonTable].EmpSource.Enabled := false;
             Result := tThisBaseGraph.Create(Application);
             Result.GraphDraw.HorizLabel := RemoveUnderscores(HorizLabel);
             Result.GraphDraw.VertLabel := RemoveUnderscores(VertLabel);
             Result.GraphDraw.RighJustifyHorizLabel := true;

             Result.Caption := GISdb[DBonTable].DBName;
             Result.OpenXYColorFile(rfile);

             SetDirtAirballBackground(Result,DEMType);

             while not GISdb[DBonTable].MyData.eof do begin
                TileValue := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(TileParam);
                Winners := GISdb[DBonTable].MyData.GetFieldByNameAsString('DEM_LOW_SC');
                inc(TotalPoss);
                for i := 1 to NumDEMIXDEM do begin
                   if StrUtils.AnsiContainsText(Winners,DEMIXDEMTypeName[i]) then begin
                      v[3] := ConvertPlatformColorToTColor(DEMIXColorFromDEMName(DEMIXDEMTypeName[i]));
                      v[2] := TileValue;
                      v[1] := i;
                      BlockWrite(rfile,v,1);
                      inc(BestCount[i]);
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
             Result.Width := 275;
             Result.GraphDraw.MinVertAxis := 0;
             Result.GraphDraw.MaxVertAxis := UseMax;
             Result.RedrawDiagram11Click(Nil);

             Winners := VertLabel + ',' + DEMType + ',' + LandType + ',' + IntToStr(TotalPoss);
             for i := 1 to NumDEMIXDEM do Winners := Winners + ',' + RealToString(100 * BestCount[i] / TotalPoss,-8,-2);
             Statistics.Add(Winners);
         end;



    procedure MakeGraph(aP : integer; TheCriteria,TheParameter,TheDEM,LandType,HorizLabel,VertLabel : shortstring);
    var
       Bitmap : tMyBitmap;
       FullWidth  : integer;
       SourceRect, DestRect : TRect;
       Gr : TThisBaseGraph;
    begin
      {$If Defined(RecordDEMIXSortGraph)} WriteLineToDebugFile('MakeGraph,   labels: ' + HorizLabel + '  ' + VertLabel); {$EndIf}
      GISdb[DBonTable].EmpSource.Enabled := false;
      if (aP = 1) then UseMax := 1.025 * GISdb[DBonTable].MyData.FindFieldMax(TheParameter);
      Gr := OnlyInTieForBestByParameterSorting(DBonTable,UseMax,TheParameter,TheCriteria,theDEM,LandType,HorizLabel,VertLabel);
      CopyImageToBitmap(gr.Image1,Bitmap);
      {$If Defined(RecordDEMIXSortGraph)}
         fName := Petmar.NextFileNumber(MDTempDir,'frame_' + TheParameter + '_' + TheCriteria + '_' + theDEM + '_','.bmp');
         Bitmap.SaveToFile(fName);
      {$EndIf}
      if (ap = 1) then begin
         UseFulWidth := Bitmap.Width - Gr.GraphDraw.LeftMargin;
         FullWidth := Bitmap.Width + pred(GraphPanelsWide) * (UsefulWidth + 10);
         CreateBitmap(BigBitmap,FullWidth, Bitmap.Height);
         BigBitmap.Canvas.Draw(0,0,Bitmap);
         LeftStart := Bitmap.Width + 10;
      end
      else begin
         SourceRect := Rect(Gr.GraphDraw.LeftMargin,0,Bitmap.Width,Bitmap.Height);
         DestRect := Rect(LeftStart,0,LeftStart + UsefulWidth,Bitmap.Height);
         BigBitmap.Canvas.CopyRect(DestRect,Bitmap.Canvas,SourceRect);
         LeftStart := LeftStart + UsefulWidth + 10;
      end;
      Gr.Destroy;
      Bitmap.Free;
      {$If Defined(RecordDEMIXSortGraph)} WriteLineToDebugFile('MakeGraph out, ap=' + IntToStr(ap) + '  LeftStart=' + IntToStr(LeftStart) + ' ' + BitmapSizeString(BigBitmap)); {$EndIf}
    end;

         procedure FinishBigMap(aName : shortstring);
         var
            BMP : tMyBitmap;
         begin
            fName := MDTempDir + 'Supp_fig_' + aName + '.png';
            BMP := DEMIXTestDEMLegend;
            BigBitmap.Height := BigBitmap.Height + BMP.Height;
            BigBitmap.Canvas.Draw((BigBitmap.Width div 2) - (bmp.Width div 2), BigBitmap.Height - bmp.Height,BMP);
            SaveBitmap(BigBitmap,fName);
            {$If Defined(RecordDEMIX)} WriteLineToDebugFile('MultipleBestByParameters done graph=' + aName  + ' ' + BitmapSizeString(BigBitmap)); {$EndIf}
            DisplayBitmap(BigBitmap);
            BigBitmap.Free;
         end;


var
   //DEMtype,
   aName : shortstring;
   ap,i : integer;
   aCriteria,aTileParameter,k,Sl : integer;
begin {procedure MultipleBestByParametersSortByValue}
   {$If Defined(RecordDEMIXSortGraph)} WriteLineToDebugFile('MultipleBestByParameters in, table=' + IntToStr(DBontable)); {$EndIf}

   Statistics := tStringList.Create;
   aline := 'GRAPH,REF_TYPE,LAND_TYPE,OPINIONS';
   for i := 1 to NumDEMIXDEM do aLine := aLine + ',' + DEMIXDEMTypeName[i];
   Statistics.Add(aLine);

//DEMsTypeUsing,TilesUsing,LandTypesUsing,CandidateDEMsUsing,CriteriaUsing

   if (Option = 0) then begin
      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Graph option 0'); {$EndIf}
      for aTileParameter := 0 to pred(TileParameters.Count) do begin
         wmdem.SetPanelText(1,TileParameters[aTileParameter]);
         GraphPanelsWide := CriteriaUsing.Count;
         for k := 0 to pred(DEMsTypeUsing.Count) do begin
            wmdem.SetPanelText(3,DEMsTypeUsing[k]);
            aName :=  'criteria_ref_' + DEMsTypeUsing[k] + '_results_sort_by_' + TileParameters[aTileParameter];
            ap := 1;
            for i := 0 to pred(CriteriaUsing.Count) do begin
               wmdem.SetPanelText(2,CriteriaUsing[i]);
               MakeGraph(ap,CriteriaUsing[i],TileParameters[aTileParameter],DEMsTypeUsing[k],'ALL',CriteriaUsing[i],aName);
               inc(ap);
            end;
            FinishBigMap(aName);
         end;
      end
   end
   else if (Option = 1) then begin
      {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Graph option 1'); {$EndIf}
      for aTileParameter := 0 to pred(TileParameters.Count) do begin
         wmdem.SetPanelText(1,TileParameters[aTileParameter]);
         aName := 'all_15_criteria_sort_by_' + TileParameters[aTileParameter];
         ap := 1;
         GraphPanelsWide := LandTypesUsing.Count * 2;
         for k := 0 to pred(DEMsTypeUsing.Count) do begin
            wmdem.SetPanelText(2,DEMsTypeUsing[k]);
            for sl := 0 to pred(LandTypesUsing.Count) do begin
               wmdem.SetPanelText(3,LandTypesUsing[sl]);
               MakeGraph(ap,'',TileParameters[aTileParameter],DEMsTypeUsing[k],LandTypesUsing[sl],LandTypesUsing[sl],aName);
               inc(ap);
            end;
         end;
         FinishBigMap(aName);
      end
   end
   else if (Option = 2) then begin
      for aTileParameter := 0 to pred(TileParameters.Count) do begin
         for aCriteria := 0 to pred(CriteriaUsing.Count) do begin
            aName := CriteriaUsing[aCriteria] + '_sort_by_' + TileParameters[aTileParameter];
            ap := 1;
            GraphPanelsWide := LandTypesUsing.Count * 2;
            for k := 0 to pred(DEMsTypeUsing.Count) do begin
               for sl := 0 to pred(LandTypesUsing.Count) do begin
                  MakeGraph(ap,CriteriaUsing[aCriteria],TileParameters[aTileParameter],DEMsTypeUsing[k],LandTypesUsing[sl],LandTypesUsing[sl],aName);
                  inc(ap);
               end;
            end;
            FinishBigMap(aName);
         end;
      end
   end
   else if (Option = 3) then begin
      for aTileParameter := 0 to pred(TileParameters.Count) do begin
         wmdem.SetPanelText(1,TileParameters[aTileParameter]);
         aName := 'criterion_sort_by_' + TileParameters[aTileParameter];
         ap := 1;
         GraphPanelsWide := CriteriaUsing.Count * 2;
         for aCriteria := 0 to pred(CriteriaUsing.Count) do begin
            for k := 0 to pred(DEMsTypeUsing.Count) do begin
               MakeGraph(ap,CriteriaUsing[aCriteria],TileParameters[aTileParameter],DEMsTypeUsing[k],'ALL',CriteriaUsing[aCriteria],TileParameters[aTileParameter]);
               inc(ap);
            end;
         end;
         FinishBigMap(aName);
         {$If Defined(RecordDEMIX)} WriteLineToDebugFile('MultipleBestByParameters done graph=' + IntToStr(aTileParameter)); {$EndIf}
      end;
   end;

   fName := NextFileNumber(MDTempDir,MDTempDir + 'dem_batting_average_','.dbf');
   StringList2CSVtoDB(Statistics,fName);
   GISdb[DBonTable].ClearGISFilter;
   GISdb[DBonTable].EmpSource.Enabled := true;
   GISdb[DBonTable].ShowStatus;
end {procedure MultipleBestByParametersSortByValue};




end.





