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
   //{$Define RecordDEMIXLoad}
   {$Define RecordDiluvium}
   //{$Define Record3DEPX}
   {$Define RecordDEMIX_evaluations_graph}
   {$Define RecordDiluviumFull}
   //{$Define TrackPixelIs}

   //{$Define ShowOpenBothPixelIsDEMs}   //see how opening and masking for Diluvium DEM is going; don't use unless there is a a problem
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
    DEMDefs,
    DEMIX_definitions;

const
   DEMIX_initialized : boolean = false;

   function GetDEMIXpaths(StartProcessing : boolean = true; DB : integer = 0) : boolean;
   procedure EndDEMIXProcessing(db : integer = 0);
   procedure LoadDEMIXnames;

   function DEMIXColorFromDEMName(DEMName : shortstring) : tPlatformColor;
   function WhatTestDEMisThis(fName : PathStr) : shortstring;
   function IsDEMaDSMorDTM(DEMName : ShortString) : integer;
   function DEMIXTestDEMLegend(Horizontal : boolean = true) : tMyBitmap;

   procedure CompareSeriousCompetitors(DBonTable : integer);

//service functions and procedures
   function LoadDEMIXReferenceDEMs(AreaName : shortstring; var RefDEM : integer; OpenMaps : boolean = true) : boolean;
   function LoadDEMIXCandidateDEMs(AreaName : ShortString;  OpenMaps : boolean = false; AllCandidates : boolean = true) : boolean;
   procedure LoadThisDEMIXTile(AreaName,TileName : shortstring; OpenMaps : boolean = true);

   procedure GetReferenceDEMsForTestDEM(TestSeries : shortstring; var UseDSM,UseDTM : integer);
   function GetReferenceDEMforTestDEM(ThisTestDEM : integer; RefDEMs : tDEMIXindexes) : integer;
   function CriterionTieTolerance(Criterion : shortstring) : float32;
   procedure GetFilterAndHeader(i,j : integer; var aHeader,aFilter : shortString);
   function DEMIX_AreasWanted(CanLimitAreas : boolean = true) : tStringList;

   function GetAreaNameForDEMIXTile(DB : integer; DemixTile : shortstring) : shortstring;

   procedure FilterInSignedCriteria(DBonTable : integer);
   function CreateFilterOutSignedCriteria(DBonTable : integer) : shortstring;
   procedure FilterOutSignedCriteria(DBonTable : integer);

   function AreaNameFromRefDEMName(fName : PathStr) : shortstring;
   function RemoveDTMorDSMfromAreaName(fName : PathStr) : shortstring;

   function SymbolFromDEMName(DEMName : shortstring) : tFullSymbolDeclaration;

   procedure OpenDEMIXDatabaseForAnalysis;


   function GetCountryForArea(Area : shortString) : shortstring;
   procedure SummarizeEGM96toEGM2008shifts;
   procedure SetDirtAirballBackground(var Result : tThisBaseGraph; DEMType : shortstring);   //brown dirtball for STM, blue airball for DSM



procedure MergeDEMIXtileStats;

procedure CompareRankings(DBonTable : integer);
procedure DifferentRankingsByCriteria(DBonTable : integer);

   procedure WinsAndTies(DBonTable : integer);
   procedure DEMIXisCOPorALOSbetter(DBonTable : integer);



procedure ReinterpolateTestDEMtoHalfSec(var DEM : integer; OpenMap : boolean);

function DEMsinIndex(Index : tDEMIXindexes) : integer;


procedure GetAreaDEMNames(TestAreaName : shortstring);

function ExtraToSpreadDEMs(DEMName : shortString; Extra : float32) : float32;
function OpenBothPixelIsDEMs(Area,Prefix : shortstring; RefDir,TestDir : PathStr; OpenMaps : boolean) : boolean;



   {$IfDef AllowEDTM}
      procedure ExtractEDTMforTestAreas;
   {$EndIf}

   {$IfDef Old3DEP}
      procedure SubsetLargeUS3DEParea(DEM : integer = 0);
      procedure BatchSubset_3DEP_DEMs;
      procedure DEMIX_VDatum_shifts;
      procedure SummarizeVDatumShifts;
   {$EndIf}

   {$IfDef OpenDEMIXAreaAndCompare}
      procedure OpenDEMIXArea(fName : PathStr = '');
   {$EndIf}

   {$IfDef OldDEMIXroutines}
      procedure TransposeDEMIXwinecontestGraph(DBonTable : integer);
   {$EndIf}


var
   ElevDiffHists : boolean;


implementation

uses
   Nevadia_Main,
   DEMstat,Make_grid,PetImage,PetImage_form,new_petmar_movie,DEMdatabase,PetDButils,Pick_several_dems,
   Geotiff, BaseMap, GDAL_tools, DEMIX_filter, DEMstringgrid,DEM_NLCD,
   DEMCoord,DEMMapf,DEMDef_routines,DEM_Manager,DEM_indexes,PetMath,
   DEMIX_graphs;

var
   vd_path : PathStr;
   DoHorizontalShift : boolean;



{$IfDef Old3DEP}
   {$I old_demix_3dep_routines.inc}
{$EndIf}


{$If Defined(AllowEDTM) or Defined(OldDEMIXroutines)}
   {$I experimental_demix_criteria.inc}
{$EndIf}


{$IfDef OpenDEMIXAreaAndCompare}
   {$I open_demix_area.inc}
{$EndIf}


function OpenBothPixelIsDEMs(Area,Prefix : shortstring; RefDir,TestDir : PathStr; OpenMaps : boolean) : boolean;
//opens reference DTMs, for both pixel-is-point and pixel-is-area
//0 in the array is the reference data, -1 is the 1.5 sec data
const
   Ext = '.tif';
var
   fName : PathStr;
   i : integer;

         procedure TryToOpen(fName : PathStr; var DEM : integer);
         begin
            if FileExists(fName) then begin
               DEM := OpenNewDEM(fName,OpenMaps);
               {$IfDef TrackSWCornerForComputations} DEMGlb[DEM].WriteToDebugSWCornerForComputations('OpenBothPixelIsDEMs'); {$EndIf}
            end
            else begin
               {$IfDef RecordDEMIX} WriteLineToDebugFile('Missing file ' + fName); {$EndIf};
               Result := false;
            end;
         end;

         procedure TrimDEM(DEM : integer);
         var
            Limits : tGridLimits;
         begin
            Limits := DEMGlb[DEM].FullDEMGridLimits;
            DEMGlb[DEM].FilledGridBox(Limits);
            fname := NextFileNumber(MDtempDir,DEMGlb[DEM].AreaName + '_trim_','.dem');
            DEMGlb[DEM].WriteNewFormatDEM(Limits,fName);
            DEMGlb[DEM].DEMFileName := fName;
            DEMGlb[DEM].ReloadDEM(true);
            if OpenMaps then DEMGlb[DEM].SelectionMap.DoBaseMapRedraw;
         end;

         procedure MaskToMatchDiluvium(DEM : integer);
         begin
            EditsDone := 0;
            MaskStripFromSecondGrid(DEM,AreaDEMs[2],msSecondMissing);
            DEMGlb[DEM].CheckMaxMinElev;
            {$IfDef ShowOpenBothPixelIsDEMs} if OpenMaps then DEMGlb[DEM].SelectionMap.DoBaseMapRedraw; {$EndIf};
         end;

var
   DataDir : PathStr;
begin
    {$IfDef ShowOpenBothPixelIsDEMs} OpenMaps := true; {$EndIf};
    Result := true;

    TryToOpen(RefDir + Prefix + area + '_dtm' + Ref1SecPointStr + Ext,PointDEMs[0]);
    if DEMGlb[PointDEMs[0]].DEMSWcornerLat > 50 then begin
       TryToOpen(RefDir + Prefix + area + '_dtm' + Ref1_5SecPointStr + Ext,PointDEMs[-1]);
    end;
    for i := 1 to NumPt do begin
       if PointNames[i] = 'DELTA' then DataDir := DEMIX_delta_dtms else DataDir := TestDir;
       TryToOpen(DataDir + Prefix + Area + '_' + PointNames[i]  + Ext,PointDEMs[i]);
    end;
    if ValidDEM(PointDEMs[-1]) then dmxFirstPoint := -1 else dmxFirstPoint := 0;

    TryToOpen(RefDir + Prefix + Area + '_dtm' + Ref1SecAreaStr + Ext,AreaDEMs[0]);
    for i := 1 to NumArea do begin
       if (AreaNames[i] = 'DILUV') then DataDir := DEMIX_diluvium_dems else DataDir := TestDir;
       TryToOpen(DataDir + Prefix + Area + '_' + AreaNames[i]  + Ext,AreaDEMs[i]);
    end;
   if ValidDEM(AreaDEMs[-1]) then dmxFirstArea := -1 else dmxFirstArea := 0;

   {$IfDef ShowOpenBothPixelIsDEMs} wmdem.Tile; MessageToContinue('After loading'); {$EndIf};

    //using Diluvium DEM, so have to mask everything to elevations under 80 m
    if (NumArea = 2) then begin
       for i := dmxFirstPoint to NumPt do MaskToMatchDiluvium(PointDEMs[i]);
       for i := dmxFirstArea to 1 do MaskToMatchDiluvium(AreaDEMs[i]);
       {$IfDef ShowOpenBothPixelIsDEMs} wmdem.Tile; MessageToContinue('After masking'); {$EndIf};

       for i := dmxFirstPoint to NumPt do TrimDEM(PointDEMs[i]);
       for i := dmxFirstArea to 2 do TrimDEM(AreaDEMs[i]);
       {$IfDef ShowOpenBothPixelIsDEMs} wmdem.Tile; MessageToContinue('After trimming'); {$EndIf};
    end;

    {$IfDef TrackPixelIs} ShowDEMIXgrids('OpenBothPixelIsDEMs', PointDEMs,AreaDEMs); {$EndIf};
    {$IfDef ShowOpenBothPixelIsDEMs} wmdem.Tile; MessageToContinue('Check maps'); {$EndIf};
end;


function ExtraToSpreadDEMs(DEMName : shortString; Extra : float32) : float32;
begin
    if (DEMName = 'COP') or (DEMName = 'SRTM') then Result := Extra
    else if (DEMName = 'FABDEM') or (DEMName = 'NASA') then Result := -Extra
    else Result := 0;
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
   TheFiles := GetListOfDEMIXtileStats;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Twmdem.MergeDEMIXtilestats1 tile stat files=' + IntToStr(TheFiles.Count)); {$EndIf}
   if (TheFiles.Count > 1) then begin
      fName := DEMIXresultsDir + 'DEMIX_TILES_USED_SUMMARY.dbf';
      StringList2CSVtoDB(TheFiles,fName,true);
   end
   else TheFiles.Free;
end;


function RemoveDTMorDSMfromAreaName(fName : PathStr) : shortstring;
begin
   Result := StringReplace(FName, '_dsm', '',[rfIgnoreCase]);
   Result := StringReplace(Result, '_dtm', '',[rfIgnoreCase]);
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
            for DEM := 1 to NumDEMIXtestDEM do begin
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

   EndDEMIXProcessing(dbOnTable);
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('CompareRankings out'); {$EndIf}
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
   if (StrUtils.AnsiContainsText(fName,'TANDEM')) then fName := 'TANDEM';
   if (StrUtils.AnsiContainsText(fName,'DELTA')) then fName := 'DELTA';
end;


function DEMIXColorFromDEMName(DEMName : shortstring) : tPlatformColor;
var
   i : integer;
begin
   ExtractDEMIXDEMName(DEMName);
   Result := RGBtrip(185,185,185);
   if (DEMName = 'TIE') then Result := claBrown
   else begin
      for I := 1 to NumDEMIXtestDEM do
         if (Uppercase(DEMIXShort[i]) = DEMName) then Result := DEMIXDEMcolors[i];
   end;
   {$IfDef RecordDEMIX_colors} WriteLineToDebugFile('DEMIX color  ' + DEMName + '  ' + ColorStringFromPlatformColor(Result)); {$EndIf}
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

   RefDTMPointFName := DEMIX_Ref_1sec + TestAreaName + '_dtm' + Ref1SecPointStr + '.tif';
   if FileExists(RefDTMPointFName) then begin
      RefDTMareaFName := StringReplace(RefDTMPointFName, '_point.tif', '_area.tif',[rfIgnoreCase]);
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
      {$IfDef RecordDEMIXLoad} WriteLineToDebugFile('GetAreaDEMNames out, DTM_point=' + RefDTMpointFName +'  DTM_area=' + RefDTMareaFName); {$EndIf}
   end
   else begin
      {$IfDef RecordDEMIXLoad} WriteLineToDebugFile('GetAreaDEMNames could not find DTM_point=' + RefDTMpointFName); {$EndIf}
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
   LoadDEMIXCandidateDEMs(AreaName,{RefDEM,}true);
   for I := 1 to MaxDEMIXDEM do begin
      if ValidDEM(TestDEMs[i]) then begin
         DEMGlb[TestDEMs[i]].AreaName := StringReplace(DEMGlb[TestDEMs[i]].AreaName, AreaName, TileName,[rfIgnoreCase]);
         if (DEMGlb[TestDEMs[i]].SelectionMap <> Nil) then DEMGlb[TestDEMs[i]].SelectionMap.SubsetAndZoomMapFromGeographicBounds(bb);
      end;
   end;
   ShowDefaultCursor;
   {$If Defined(RecordDEMIXLoad)} WriteLineToDebugFile('LoadThisDEMIXTile out, open DEMs=' + IntToStr(NumDEMDataSetsOpen)); {$EndIf}
end;


procedure LoadDEMIXnames;
var
   table : tMyData;
   fName,fName2 : PathStr;
   i : integer;
   TheDEMs : tStringList;
   aLine : shortstring;
begin
   //if (NumDEMIXDEM = 0) and FileExists(fName) then begin
   (*
    if DEMIX_Mode = dmNotYetDefined then begin

    end;
   *)
      TheDEMs := tStringList.Create;
      TheDEMs.LoadFromFile(DEMListFName);
      fName := DEMIXSettingsDir + 'demix_dems.dbf';
      Table := tMyData.Create(fName);
      for I := 1 to TheDEMs.Count do begin
         Table.ApplyFilter('SHORT_NAME=' + QuotedStr(TheDEMs.Strings[pred(i)]));
         DEMIXDEMTypeName[i] := Table.GetFieldByNameAsString('DEM_NAME');
         DEMIXshort[i] := Table.GetFieldByNameAsString('SHORT_NAME');
         DEMIXDEMcolors[i] := Table.PlatformColorFromTable;
      end;
      NumDEMIXtestDEM := TheDEMs.Count;
      //equiredTestDEMs := TheDEMs.Count;

      {$IfDef RecordDEMIXNames}
          aLine := IntToStr(NumDEMIXDEM);
          for I := 1 to NumDEMIXDEM do aline := aline + '  ' + DEMIXshort[i];
          HighlightLineToDebugFile('LoadDEMIXNames=' + aLine);
      {$EndIf}
      Table.Destroy;
      TheDEMs.Destroy;
   //end;
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
   for i := 1 to NumDEMIXtestDEM do begin
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
   Result.Canvas.Font.Size := MDDef.DEMIXlegendFontSize;
   Left := 25;
   Top := 10;
   for i := 1 to NumDEMIXtestDEM do begin
      if DEMIXDEMinUse[i] then begin
         Result.Canvas.Pen.Color := ConvertPlatformColorToTColor(DEMIXColorFromDEMName(DEMIXShort[i]));
         Result.Canvas.Brush.Color := Result.Canvas.Pen.Color;
         Result.Canvas.Brush.Style := bsSolid;
         Result.Canvas.Rectangle(Left,Top,Left + 15,Top + 15);
         Result.Canvas.Brush.Style := bsClear;
         Result.Canvas.TextOut(Left + 20,Top,DEMIXShort[i]);
         if Horizontal then Left := Left + 30 + Result.Canvas.TextWidth(DEMIXShort[i])
         else Top := Top + 10 + Result.Canvas.TextHeight(DEMIXShort[i]);
      end;
   end;
   PutBitmapInBox(Result);
end;


procedure SetDirtAirballBackground(var Result : tThisBaseGraph; DEMType : shortstring);
begin
    if (DEMtype = 'DSM') then Result.GraphDraw.GraphBackgroundColor := RGB(219,236,237)
    else Result.GraphDraw.GraphBackgroundColor := RGB(237,237,221);
end;


function PickWineContestLocation : boolean;
begin
   if PathIsValid(DEMIX_Base_DB_Path) then Result := true
   else Result := FindPath('DEMIX Wine contest location',':\Wine_contest\',DEMIX_Base_DB_Path);
end;


procedure SetParamsForDEMIXmode;
begin
   //DEMIX_mode = dmClassic then begin
      NumPt := 6;
      NumArea := 1;
      SSIMresultsDir := RegularSSIMresultsDir;
      AreaListFName := DEMIXSettingsDir + 'areas_list.txt';
      DEMListFName := DEMIXSettingsDir + 'dems_classic.txt';
   if (DEMIX_mode = dmAddDiluvium) then begin
      NumArea := 2;
      SSIMresultsDir := DiluvSSIMresultsDir;
      AreaListFName := DEMIXSettingsDir + 'areas_diluvium.txt';
      DEMListFName := DEMIXSettingsDir + 'dems_add_diluvium.txt';
   end
   else if (DEMIX_mode = dmAddDelta) then begin
      NumPt := 7;
      DEMListFName := DEMIXSettingsDir + 'dems_add_delta.txt';
   end;
end;

procedure RecognizeDEMIXVersion(DB : integer);
begin
   DEMIX_mode := dmClassic;
   if GISDB[db].MyData.FieldExists('DILUV') then DEMIX_mode := dmAddDiluvium
   else if GISDB[db].MyData.FieldExists('DELTA') then DEMIX_mode := dmAddDelta;
   SetParamsForDEMIXmode;
end;


function GetDEMIXpaths(StartProcessing : boolean = true; DB : integer = 0) : boolean;
begin
   {$If Defined(RecordDEMIXStart)} WriteLineToDebugFile('GetDEMIXpaths in'); {$EndIf}
   if DEMIX_initialized then Result := true
   else begin
      {$If Defined(RecordDEMIXStart)} WriteLineToDebugFile('GetDEMIXpaths off to PickWineContestLocation'); {$EndIf}
      Result := PickWineContestLocation;
   end;

   if (not Result) then exit;
   DEMIX_initialized := true;

   {$If Defined(RecordDEMIXStart)} WriteLineToDebugFile('Wine contest location done'); {$EndIf}

   if StartProcessing then begin
      HeavyDutyProcessing := true;
      WMdem.Color := clInactiveCaption;
      DEMIXProcessing := true;
   end;
   StopSplashing;

   //settings that can be changed, but constant here for DB creation
      ElevDiffHists := true;
      DoHorizontalShift := false;
      MDdef.MDRecordDebugLog := true;

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

   DEMIXSettingsDir := ProgramRootDir + 'demix\';
   DEMIX_area_dbName := DEMIXSettingsDir + 'demix_test_areas_v3.dbf';

   DEMIX_Ref_Source := DEMIX_Base_DB_Path + 'wine_contest_v2_ref_source\';
   DEMIX_Ref_Merge := DEMIX_Base_DB_Path + 'wine_contest_v2_ref_merge\';
   DEMIX_Ref_Half_sec := DEMIX_Base_DB_Path + 'wine_contest_v2_ref_0.5sec\';
   vd_path := DEMIX_Base_DB_Path + 'wine_contest_v2_vdatum\';
   DEMIXresultsDir := DEMIX_Base_DB_Path + 'wine_contest_v2_tile_stats\';
   DEMIX_diff_dist  := DEMIX_Base_DB_Path + 'wine_contest_v2_diff_dist\';
   DEMIX_diff_maps_dir  := DEMIX_Base_DB_Path + 'wine_contest_difference_maps\';
   DEMIX_area_lc100  := DEMIX_Base_DB_Path + 'wine_contest_v2_lc100\';
   DEMIX_profile_test_dir := DEMIX_Base_DB_Path + 'wine_contest_v2_topo_profiles\';
   DEMIX_distrib_graph_dir := DEMIX_Base_DB_Path + 'wine_contest_v2_difference_distrib_graphs\';
   DEMIX_3DEP_Dir := DEMIX_Base_DB_Path + 'wine_contest_v2_3dep\';
   DEMIX_GIS_dbName := DEMIX_Base_DB_Path + 'wine_contest_database\demix_gis_db_v2.5.dbf';

   DEMIX_test_DEMs_no_sink := DEMIX_Base_DB_Path + 'area_test_dems_no_sink\';
   DEMIX_ref_DEMs_no_sink := DEMIX_Base_DB_Path + 'area_ref_dems_no_sink\';
   DEMIX_test_DEMs_channels := DEMIX_Base_DB_Path + 'area_test_dems_channels\';
   DEMIX_ref_DEMs_channels := DEMIX_Base_DB_Path + 'area_ref_dems_channels\';
   DEMIX_test_DEMs_channel_grids := DEMIX_Base_DB_Path + 'area_test_dems_channel_grids\';
   DEMIX_ref_DEMs_channel_grids := DEMIX_Base_DB_Path + 'area_ref_dems_channel_grids\';
   ChannelMissesDir := DEMIX_Base_DB_Path + 'channel_misses\';

   RegularSSIMresultsDir := DEMIX_Base_DB_Path + 'SSIM_results\';
   DiluvSSIMresultsDir := DEMIX_Base_DB_Path + 'diluvium_SSIM_results\';
   DeltaSSIMresultsDir := DEMIX_Base_DB_Path + 'delta_SSIM_results\';

   DEMIX_Ref_1sec := DEMIX_Base_DB_Path + 'wine_contest_v2_ref_1sec\';
   DEMIX_test_dems := DEMIX_Base_DB_Path + 'wine_contest_v2_test_dems\';
   DEMIX_diluvium_dems := DEMIX_Base_DB_Path + 'diluvium_test_dems\';
   DEMIX_delta_dtms := DEMIX_Base_DB_Path + 'delta_test_dtms\';

   {$If Defined(RecordDEMIXStart)} WriteLineToDebugFile('GetDEMIXpaths point 4'); {$EndIf}

   Geoid2008FName := 'g:\geoid\egm2008-1-vdatum.tif';
   FindDriveWithFile(Geoid2008FName);
   GeoidDiffFName := 'g:\geoid\egm96_to_egm2008.tif';
   FindDriveWithFile(GeoidDiffFName);

   if ValidDB(DB) then begin
      RecognizeDEMIXVersion(DB);
   end
   else if (DEMIX_mode = dmNotYetDefined) then begin
      if AnswerIsYes('Classic DEMIX') then DEMIX_Mode := dmClassic
      else if AnswerIsYes('Add diluvium') then DEMIX_Mode := dmAddDiluvium
      else DEMIX_Mode := dmAddDelta;
   end;
   SetParamsForDEMIXmode;
   LoadDEMIXnames;

   {$If Defined(RecordDEMIXStart)} WriteLineToDebugFile('GetDEMIXpaths out'); {$EndIf}
end;


procedure EndDEMIXProcessing(db : integer = 0);
begin
   if HeavyDutyProcessing then begin
      CleanUpTempDirectory(false);
   end;
   HeavyDutyProcessing := false;
   DEMIXProcessing := false;
   WMdem.Color := clScrollBar;
   ReportErrors := true;
   LockStatusBar := false;
   wmdem.ClearStatusBarPanelText;
   ShowDefaultCursor;
   EndProgress;
   if ValidDB(db) then begin
      GISdb[DB].ClearGISFilter;
      GISdb[DB].ShowStatus;
   end;
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


function DEMIX_AreasWanted(CanLimitAreas : boolean = true) : tStringList;
var
   fName : PathStr;
   PickedNum : integer;
begin
   Result := tStringList.Create;
   fName := AreaListFName;
   if FileExists(fName) or GetExistingFileName('DEMIX areas','*.txt',fName) then begin
      Result.LoadFromFile(fName);
      if CanLimitAreas then MultiSelectSingleColumnStringList('Areas to process',PickedNum,Result,false,true);
   end;
end;


function GetCountryForArea(Area : shortString) : shortstring;
var
   Table : tMyData;
   Tile  : shortstring;
begin
   if FileExists(DEMIX_area_dbName) then begin
     Table := tMyData.Create(DEMIX_area_dbName);
     Table.ApplyFilter('AREA=' + QuotedStr(Area));
     if Table.FiltRecsInDB = 0 then Result := ''
     else Result := Table.GetFieldByNameAsString('COUNTRY');
   end;
   Table.Destroy;
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



procedure OpenDEMIXDatabaseForAnalysis;
begin
   if not FileExists(DEMIX_GIS_dbName) then Petmar.GetExistingFileName('DEMIX db version','*.dbf',DEMIX_GIS_dbName);
   OpenNumberedGISDataBase(DEMIX_DB,DEMIX_GIS_dbName,false);
   GetDEMIXpaths(false,DEMIX_DB);
   GISdb[DEMIX_DB].LayerIsOn := false;
   DoDEMIXFilter(DEMIX_DB);
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


procedure CompareSeriousCompetitors(DBonTable : integer);


   procedure OnePair(DEM1,DEM2 : shortstring);
   var
      eval1,eval2,tolerance : float32;
      tStr,fName,Criterion : shortstring;
   begin
      if GISdb[dbOnTable].MyData.FieldExists(DEM1) and GISdb[dbOnTable].MyData.FieldExists(DEM2) then begin
         fName := DEM1 + '_' + DEM2;
         GISdb[dbOnTable].AddFieldToDataBase(ftstring,fName,12);
         GISdb[dbOnTable].MyData.First;
         GISdb[dbOnTable].EmpSource.Enabled := false;
         while not GISdb[dbOnTable].MyData.eof do begin
            Criterion := GISdb[dbOnTable].MyData.GetFieldByNameAsString('CRITERION');
            if not IsDEMIX_signedCriterion(Criterion) then begin
               eval1 := GISdb[dbOnTable].MyData.GetFieldByNameAsFloat(DEM1);
               eval2 := GISdb[dbOnTable].MyData.GetFieldByNameAsFloat(DEM2);
               tolerance := GISdb[dbOnTable].MyData.GetFieldByNameAsFloat('TOLERANCE');
               if eval1 + Tolerance < Eval2 then tStr := DEM1
               else if eval2 + Tolerance < Eval1 then tStr := DEM2
               else tStr := 'TIE';
               GISdb[dbOnTable].MyData.Edit;
               GISdb[dbOnTable].MyData.SetFieldByNameAsString(fName,tStr);
            end;
            GISdb[dbOnTable].MyData.Next;
         end;
      end;
   end;

begin
   if not GISdb[DBonTable].MyData.FieldExists('CRITERION') then begin
      RankDEMS(DBonTable);
   end;

   OnePair('COP','ALOS');
   OnePair('COP','FABDEM');
   OnePair('COP','TANDEM');
   OnePair('COP','DILUV');
   OnePair('COP','DELTA');
   GISdb[dbOnTable].EmpSource.Enabled := true;
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
      EndDEMIXProcessing(DBonTable);
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


function LoadDEMIXCandidateDEMs(AreaName : ShortString; OpenMaps : boolean = false; AllCandidates : boolean = true) : boolean;
var
   i,Ser : integer;
   fName : Pathstr;
begin
   {$If Defined(RecordDEMIXLoad)} writeLineToDebugFile('LoadDEMIXCandidateDEMs in; Open DEMs=, ' + IntToStr(NumDEMdatasetsOpen)); {$EndIf}
   Result := false;
   LoadDEMIXnames;
   for I := 1 to MaxDEMIXDEM do begin
      TestDEMs[i] := 0;
      TestSeries[i] := '';
   end;
   Ser := 0;
   for I := 1 to NumDEMIXtestDEM do begin
      fName := DEMIX_test_dems + AreaName + '_' + DEMIXShort[i] + '.tif';
      if FileExists(fname) then begin
         inc(Ser);
         TestDEMs[Ser] := OpenNewDEM(fName,OpenMaps);
         TestSeries[Ser] := DEMIXShort[i];
      end
      else begin
         {$If Defined(RecordDEMIXFull)} HighlightLineToDebugFile('Missing test DEM= ' + fName); {$EndIf}
      end;
   end;

   Result := (Ser = NumDEMIXtestDEM);
   {$If Defined(RecordDEMIXLoad)} if not Result then writeLineToDebugFile('Reload LoadDEMIXCandidateDEMs in; Loaded only DEMs=, ' + IntToStr(Ser)); {$EndIf}
end;



function LoadDEMIXReferenceDEMs(AreaName : shortstring; var RefDEM : integer; OpenMaps : boolean = true) : boolean;
var
   NumRefDEMs : integer;

         procedure ReferenceFileOpen(var DEM : integer; fName : PathStr; What : shortString = '');
         begin
            if FileExists(fName) then begin
               DEM := OpenNewDEM(FName,OpenMaps);  //must load map for DEMIX tile computation
               if ValidDEM(DEM) and (RefDEM = 0) then RefDEM := DEM;
               inc(NumRefDEMs);
               RefDEMs[NumRefDEMs] := DEM;
               {$If Defined(RecordDEMIXRefLoad)} writeLineToDebugFile('RefDEM=' + IntToStr(DEM) + '  ' + DEMGlb[DEM].AreaName); {$EndIf}
            end
            else begin
               DEM := 0;
               {$If Defined(RecordDEMIX)} writeLineToDebugFile(What + ' ReferenceFileOpen missing=' + fName); {$EndIf}
            end;
         end;

begin
   {$If Defined(RecordDEMIX)} writeLineToDebugFile('ProcessDEMIXtestarea in, open DEMs=' + IntToStr(NumDEMDataSetsOpen) + '  AreaName=' + AreaName); {$EndIf}
   RefDEM := 0;
   NumRefDEMs := 0;
   GetAreaDEMNames(AreaName);
   {$IfDef RecordDEMIXLoad} WriteLineToDebugFile('LoadDEMIXReferenceDEMs need DTM_point=' + RefDTMpointFName +'  DTM_area=' + RefDTMareaFName); {$EndIf}

   ReferenceFileOpen(RefDTMpoint,RefDTMpointFName,'Ref DTM Point ');
   ReferenceFileOpen(RefDTMarea,RefDTMareaFName,'Ref DTM area ');
   ReferenceFileOpen(COPRefDTM,COPRefDTMFName,'Ref DTM COP ');
   {$If Defined(RecordDEMIXLoad)} writeLineToDebugFile('ProcessDEMIXtestarea, open ref DTMs=' + IntToStr(NumDEMDataSetsOpen)); {$EndIf}
   if MDDef.DEMIX_open_ref_DSM and ValidDEM(RefDTMpoint) then begin
      {$If Defined(RecordDEMIXLoad)} writeLineToDebugFile('ProcessDEMIXtestarea start DSM, open DEMs=' + IntToStr(NumDEMDataSetsOpen)); {$EndIf}
      ReferenceFileOpen(RefDSMpoint,RefDSMpointFName);
      ReferenceFileOpen(RefDSMarea,RefDSMareaFName);
      ReferenceFileOpen(COPRefDSM,COPRefDSMFName);
   end;
   Result := ValidDEM(RefDEM);
   if Result then begin
      {$If Defined(RecordDEMIXload)} writeLineToDebugFile('ProcessDEMIXtestarea out, ref DEMs open with RefDEM=' + IntToStr(RefDEM) + ' and open DEMs=' + IntToStr(NumDEMDataSetsOpen)); {$EndIf}
   end
   else begin
      {$IfDef RecordDEMIXload} HighlightLineToDebugFile('Failure, to open ref DEMs'); {$EndIf}
   end;
end;



initialization
   SSIMresultsDir := '';
finalization
end.


