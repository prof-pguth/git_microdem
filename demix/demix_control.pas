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
   //{$Define RecordDEMIXStart}
   //{$Define RecordDEMIXopenGrids}
   {$Define RecordDEMIXLoad}
   //{$Define TrackDEMboundingBox}      //must also be enabled in DEMCoord
   //{$Define Record3DEPX}
   //{$Define RecordDiluvium}
   //{$Define RecordDEMIX_evaluations_graph}
   //{$Define RecordDiluviumFull}
   //{$Define TrackPixelIs}

   //{$Define ShowOpenBothPixelIsDEMs}   //see how opening and masking for Diluvium DEM is going; don't use unless there is a a problem
   //{$Define Rec_DEMIX_Landcover}
   //{$Define RecordDEMIXsave}
   //{$Define RecordCreateHalfSec}
   //{$Define RecordHalfSec}
   //{$Define RecordTileStats}
   //{$Define Record3DEPXAlreadyDone}
   //{$Define RecordDEMIX_colors}
   //{$Define RecordTileProcessing}
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
    DEMDefs,
    DEMIX_definitions;

const
   DEMIX_initialized : boolean = false;

   function GetDEMIXpaths(StartProcessing : boolean = true; DB : integer = 0) : boolean;
   procedure EndDEMIXProcessing(db : integer = 0);
   procedure LoadDEMIXnames;

   function DEMIXColorFromDEMName(DEMName : shortstring) : tPlatformColor;
   function DEMIXColorForCriterion(Criterion : shortstring) : tColor;
   function WhatTestDEMisThis(fName : PathStr) : shortstring;
   function IsDEMaDSMorDTM(DEMName : ShortString) : integer;
   function DEMIXTestDEMLegend(Horizontal : boolean = true; DEMsUsed : tstringList = nil) : tMyBitmap;
   function FilterForDEMIXtilesToUse : shortstring;
   function FilterForDEMIXtilesToAvoid : shortstring;

   procedure CompareSeriousCompetitors(DBonTable : integer);

//service functions and procedures
   function OpenBothPixelIsDEMs(Area,Prefix : shortstring; RefDir,TestDir : PathStr; OpenMaps : boolean) : boolean;
   function LoadDEMIXReferenceDEMs(AreaName : shortstring; var RefDEM : integer; OpenMaps : boolean = true) : boolean;
   function LoadDEMIXCandidateDEMs(AreaName : ShortString;  OpenMaps : boolean = false; AllCandidates : boolean = true) : boolean;
   procedure LoadThisDEMIXTile(AreaName,TileName : shortstring; OpenMaps : boolean = true);
   procedure LoadCopAndLancoverForDEMIXTile(AreaName : shortstring; TileName : shortstring = '');

   procedure GetReferenceDEMsForTestDEM(TestSeries : shortstring; var UseDSM,UseDTM : integer);
   function GetReferenceDEMforTestDEM(ThisTestDEM : integer; RefDEMs : tDEMIXindexes) : integer;
   function CriterionTieTolerance(Criterion : shortstring) : float32;
   procedure GetFilterAndHeader(i,j : integer; var aHeader,aFilter : shortString);
   function DEMIX_AreasWanted(CanLimitAreas : boolean = true) : tStringList;

   function GetAreaNameForDEMIXTile(DB : integer; DemixTile : shortstring) : shortstring;
   function AreaNameFromRefDEMName(fName : PathStr) : shortstring;
   function RemoveDTMorDSMfromAreaName(fName : PathStr) : shortstring;

   procedure FilterInSignedCriteria(DBonTable : integer);
   function CreateFilterOutSignedCriteria(DBonTable : integer) : shortstring;
   procedure FilterOutSignedCriteria(DBonTable : integer);

   function SymbolFromDEMName(DEMName : shortstring) : tFullSymbolDeclaration;

   procedure OpenDEMIXDatabaseForAnalysis;
   procedure RecognizeDEMIXVersion(DB : integer);


   function GetCountryForArea(Area : shortString) : shortstring;
   procedure SummarizeEGM96toEGM2008shifts;
   procedure SetDirtAirballBackground(var Result : tThisBaseGraph; DEMType : shortstring);   //brown dirtball for STM, blue airball for DSM

procedure MergeDEMIXtileStats;

procedure CompareRankings(DBonTable : integer);
procedure DifferentRankingsByCriteria(DBonTable : integer);

   procedure WinsAndTies(DBonTable : integer);
   procedure DEMIXisCOPorALOSbetter(DBonTable : integer);

   function LoadLandcoverForDEMIXarea(AreaName : shortstring; OpenMap : boolean = true) : integer;
function ClipTheDEMtoFullDEMIXTiles(DEM : integer; NewName : PathStr = '') : boolean;

procedure SetParamsForDEMIXmode;


procedure ReinterpolateTestDEMtoHalfSec(var DEM : integer; OpenMap : boolean);

function DEMsinIndex(Index : tDEMIXindexes) : integer;

procedure OpenCopDEMandLandcoverForArea;
procedure OpenDEMIXAreaMaps;

procedure GetAreaDEMNames(TestAreaName : shortstring);

function ExtraToSpreadDEMs(DEMName : shortString; Extra : float32) : float32;
function PickWineContestDBLocation : boolean;
procedure PickDEMIXMode;


//demix maps
   function AirBallDirtBallMap(DEMonMap,DSM,DTM : integer; fName : PathStr = '') : integer;
   function TwoDEMHighLowMap(RefDEM,ALOS,COP : integer; SimpleTolerance : float32; FourCats : boolean; fName2 : PathStr; ShowMap : boolean = true) : integer;
   function BestCopOrALOSmap(RefDEM,ALOS,Cop : integer; Tolerance : float32; AName : shortString) : integer;
   function RGBBestOfThreeMap(RefDEM,ALOS,Cop,Fab,Merge : integer; Tolerance : float32; AName : shortString) : integer;
   procedure NumHighLowNeighborsMaps(DEM,Radius : integer; Tolerance : float32; var HighNeigh,LowNeigh : integer);



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
   DEMIX_graphs,Pick_DEMIX_mode;

var
   vd_path : PathStr;
   DoHorizontalShift : boolean;

   {$I demix_maps.inc}

   {$I demix_open_dems.inc}

{$IfDef Old3DEP}
   {$I old_demix_3dep_routines.inc}
{$EndIf}


{$If Defined(AllowEDTM) or Defined(OldDEMIXroutines)}
   {$I experimental_demix_criteria.inc}
{$EndIf}


{$IfDef OpenDEMIXAreaAndCompare}
   {$I open_demix_area.inc}
{$EndIf}


procedure PickDEMIXMode;
var
  PickDEMIXmodeForm: TPickDEMIXmodeForm;
begin
  {$IfDef RecordDEMIX} WriteLineToDebugFile('PickDEMIXMode in, DEMIX_mode=' + IntToStr(MDDef.DEMIX_mode)); {$EndIf}
  PickDEMIXmodeForm := TPickDEMIXmodeForm.Create(Application);
  PickDEMIXmodeForm.RadioGroup1.ItemIndex := MDDef.DEMIX_mode;
  InsureFormOnScreenCurrentLocation(PickDEMIXmodeForm,Mouse.CursorPos.X,Mouse.CursorPos.Y);
  PickDEMIXmodeForm.ShowModal;
  MDDef.DEMIX_mode := PickDEMIXmodeForm.RadioGroup1.ItemIndex;
  if MDDef.DEMIX_mode = dmNotYetDefined then begin
     MDDef.DEMIX_mode := dmClassic;
     MessageToContinue('Set to classic; it has to be defined');
  end;
  PickDEMIXmodeForm.Destroy;
  {$IfDef RecordDEMIX} WriteLineToDebugFile('PickDEMIXMode out'); {$EndIf}
end;



function FilterForDEMIXtilesToUse : shortstring;
begin
   Result := 'GRID_FULL>' + IntToStr(DEMIX_Tile_Full) + ' AND RELIEF>1';
end;

function FilterForDEMIXtilesToAvoid : shortstring;
begin
   Result := 'GRID_FULL<' + IntToStr(DEMIX_Tile_Full) + ' OR RELIEF<1';
end;


function DEMIXColorForCriterion(Criterion : shortstring) : tColor;
var
   Table : tMyData;
begin
   Table := tMyData.Create(DEMIX_criteria_dbName);
   Table.ApplyFilter('CRITERION=' + QuotedStr(Criterion));
   if Table.FiltRecsInDB = 1 then Result := Table.TColorFromTable
   else Result := clRed;
   Table.Destroy;
end;


function ClipTheDEMtoFullDEMIXTiles(DEM : integer; NewName : PathStr = '') : boolean;
const
   Fudge = 0.001;
var
   bb,bb2 : sfBoundBox;
   db : integer;
   {$If Defined(RecordCarto)} aLine : shortString; {$EndIf}
begin
   if ValidDEM(DEM) then begin
      {$If Defined(RecordCarto)} aLine := DEMGlb[DEM].AreaName + ' initial ' + DEMGlb[DEM].ColsRowsString; {$EndIf}
      bb := DEMGlb[DEM].DEMBoundBoxGeo;
      {$If Defined(RecordCartoFull)} WriteLineToDebugFile('TMapForm.ClipDEMtoFullDEMIXTiles, DEM in ' + sfBoundBoxToString(bb,8)); {$EndIf}
      db := DEMIXtileFill(DEM,bb,False);
      GISdb[DB].Empsource.Enabled := false;
      GISdb[DB].ApplyGISFilter('GRID_FULL >' + IntToStr(DEMIX_Tile_Full));
      if (GISdb[DB].MyData.FiltRecsInDB = 0) then begin
         Result := false;
      end
      else begin
         //GISdb[DB].DeleteAllSelectedRecords;
         bb2.xmax := GISdb[DB].MyData.FindFieldMax('LONG_HI') + Fudge;
         bb2.xmin := GISdb[DB].MyData.FindFieldMin('LONG_LOW') - Fudge;
         bb2.ymax := GISdb[DB].MyData.FindFieldMax('LAT_HI') + Fudge;
         bb2.ymin := GISdb[DB].MyData.FindFieldMin('LAT_LOW') - Fudge;
         {$If Defined(RecordCartoFull)} WriteLineToDebugFile('TMapForm.ClipDEMtoFullDEMIXTiles, tile boundaries ' + sfBoundBoxToString(bb2,8)); {$EndIf}
         bb.xmax := Petmath.MinFloat(bb.xMax,bb2.xMax);
         bb.xmin := Petmath.MaxFloat(bb.xmin,bb2.xMin);
         bb.ymax := Petmath.MinFloat(bb.yMax,bb2.yMax);
         bb.ymin := Petmath.MaxFloat(bb.yMin,bb2.YMin);
         {$If Defined(RecordCartoFull)} WriteLineToDebugFile('TMapForm.ClipDEMtoFullDEMIXTiles, map full tiles ' + sfBoundBoxToString(bb,8)); {$EndIf}
         //SubsetAndZoomMapFromGeographicBounds(bb);
         if (NewName = '') then NewName := DEMGlb[DEM].DEMFileName;
         DEMGlb[DEM].SaveGridSubsetGeotiff(DEMGlb[DEM].sfBoundBox2tGridLimits(bb),NewName);
         DEMGlb[DEM].DEMFileName := NewName;
         DEMGlb[DEM].SelectionMap.ReloadDEMClick(Nil);
         {$If Defined(RecordCarto)} aLine := aline + ' clipped ' + DEMGlb[DEM].ColsRowsString;   WriteLineToDebugFile(aLine); {$EndIf}
         Result := true;
      end;
      CloseAndNilNumberedDB(db);
   end;
end;



function ExtraToSpreadDEMs(DEMName : shortString; Extra : float32) : float32;
begin
    if (DEMName = 'COP') or (DEMName = 'SRTM') then Result := Extra
    else if (DEMName = 'FABDEM') or (DEMName = 'NASA') then Result := -Extra
    else Result := 0;
end;



procedure MergeDEMIXtileStats;


         function GetListOfDEMIXtileStats : tStringList;
         var
            i : integer;
            FName : PathStr;
         begin
            Result := tStringList.Create;
            FindMatchingFiles(Diff_dist_results_dir,'*.csv',Result);
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



var
   TheFiles{,Results,Areas} : tStringList;
   //i,j,n : integer;
   fName : PathStr;
begin
   TheFiles := GetListOfDEMIXtileStats;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Twmdem.MergeDEMIXtilestats1 tile stat files=' + IntToStr(TheFiles.Count)); {$EndIf}
   if (TheFiles.Count > 1) then begin
      fName := Diff_dist_results_dir + 'DEMIX_TILES_USED_SUMMARY.dbf';
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
   theTiles := GISdb[DBonTable].MyData.ListUniqueEntriesInDB('DEMIX_TILE');

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
   table : tMyData;
   fName,fName2 : PathStr;
   TheDEMs : tStringList;
   aLine : shortstring;
begin
   ExtractDEMIXDEMName(DEMName);
   Result := RGBtrip(185,185,185);
   if (DEMName = 'TIE') then Result := claBrown
   else begin
      fName := DEMIXSettingsDir + 'demix_dems.dbf';
      if FileExists(fName) then begin
         Table := tMyData.Create(fName);
         Table.ApplyFilter('SHORT_NAME=' + QuotedStr(DEMname));
         if (Table.FiltRecsInDB = 1) then begin
            Result := Table.PlatformColorFromTable;
         end;
      end;
   end;
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



procedure LoadDEMIXnames;
var
   table : tMyData;
   fName,fName2 : PathStr;
   i : integer;
   TheDEMs : tStringList;
   aLine : shortstring;
begin
   {$If Defined(RecordDEMIXStart)} WriteLineToDebugFile('LoadDEMIXnames in, DEMListFName=' + DEMListFName); {$EndIf}
   TheDEMs := tStringList.Create;
   if FileExists(DEMListFName) then begin
      TheDEMs.LoadFromFile(DEMListFName);
      fName := DEMIXSettingsDir + 'demix_dems.dbf';
      if FileExists(fName) then begin
         Table := tMyData.Create(fName);
         for I := 1 to TheDEMs.Count do begin
            Table.ApplyFilter('SHORT_NAME=' + QuotedStr(TheDEMs.Strings[pred(i)]));
            if (Table.FiltRecsInDB = 1) then begin
               DEMIXDEMTypeName[i] := Table.GetFieldByNameAsString('DEM_NAME');
               DEMIXshort[i] := Table.GetFieldByNameAsString('SHORT_NAME');
               DEMIXDEMcolors[i] := Table.PlatformColorFromTable;
               UseRetiredDEMs[i] := MDDef.DEMIX_graph_Retired_DEMs or ((DEMIXshort[i] <> 'ASTER') and (DEMIXshort[i] <> 'NASA') and (DEMIXshort[i] <> 'SRTM'));
            end
            else begin
               {$If Defined(RecordDEMIXStart)} WriteLineToDebugFile('LoadDEMIXnames problem DEM=' + TheDEMs.Strings[pred(i)]); {$EndIf}
            end;
         end;
         NumDEMIXtestDEM := TheDEMs.Count;

         {$IfDef RecordDEMIXNames}
             aLine := IntToStr(NumDEMIXDEM);
             for I := 1 to NumDEMIXDEM do aline := aline + '  ' + DEMIXshort[i];
             HighlightLineToDebugFile('LoadDEMIXNames=' + aLine);
         {$EndIf}
         Table.Destroy;
         TheDEMs.Destroy;
      end
      else begin
        {$If Defined(RecordDEMIXStart)} WriteLineToDebugFile('LoadDEMIXnames missing=' + fName); {$EndIf}
      end;
      {$If Defined(RecordDEMIXStart)} WriteLineToDebugFile('LoadDEMIXnames out'); {$EndIf}
   end
   else begin
     {$If Defined(RecordDEMIXStart)} WriteLineToDebugFile('DEMListFName missing=' + DEMListFName); {$EndIf}
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



function DEMIXTestDEMLegend(Horizontal : boolean = true; DEMsUsed : tstringList = nil) : tMyBitmap;
var
   i,Left,Top : integer;

         procedure AnEntry(DEM : shortString);
         begin
            Result.Canvas.Pen.Color := ConvertPlatformColorToTColor(DEMIXColorFromDEMName(DEM));
            Result.Canvas.Brush.Color := Result.Canvas.Pen.Color;
            Result.Canvas.Brush.Style := bsSolid;
            Result.Canvas.Rectangle(Left,Top,Left + 15,Top + 15);
            Result.Canvas.Brush.Style := bsClear;
            Result.Canvas.TextOut(Left + 20,Top,DEM);
            if Horizontal then Left := Left + 30 + Result.Canvas.TextWidth(DEM)
            else Top := Top + 10 + Result.Canvas.TextHeight(DEM);
         end;

begin
   CreateBitmap(Result,1500,250);
   LoadMyFontIntoWindowsFont(MDDef.LegendFont,Result.Canvas.Font);
   Result.Canvas.Font.Size := MDDef.DEMIXlegendFontSize;
   Left := 25;
   Top := 10;
   if (DEMsUsed <> Nil) then begin
      for i := 0 to pred(DEMsUsed.Count) do begin
         AnEntry(DEMsUsed.Strings[i]);
      end;
   end
   else begin
      for i := 1 to NumDEMIXtestDEM do begin
         if UseRetiredDEMs[i] then begin
            AnEntry(DEMIXShort[i]);
         end;
      end;
   end;
   PutBitmapInBox(Result);
end;


procedure SetDirtAirballBackground(var Result : tThisBaseGraph; DEMType : shortstring);
begin
    if (DEMtype = 'DSM') then Result.GraphDraw.GraphBackgroundColor := RGB(219,236,237)
    else Result.GraphDraw.GraphBackgroundColor := RGB(237,237,221);
end;


function PickWineContestDBLocation : boolean;
begin
   if PathIsValid(DEMIX_Base_DB_Path) then Result := true
   else Result := FindPath('DEMIX Wine contest location',':\Wine_contest\',DEMIX_Base_DB_Path);
end;


procedure SetParamsForDEMIXmode;
begin
   {$If Defined(RecordDEMIXStart)} WriteLineToDebugFile('SetParamsForDEMIXmode in, DEMIX_mode=' + IntToStr(MDDef.DEMIX_mode)); {$EndIf}

   if MDDef.DEMIX_mode = dmClassic then begin
      NumPtDEMs := 6;
      NumAreaDEMs := 1;
      //SSIMresultsDir := RegularSSIMresultsDir;
      //FUVresultsDir := RegularFUVresultsDir;
      AreaListFName := DEMIXSettingsDir + 'areas_list.txt';
      DEMListFName := DEMIXSettingsDir + 'dems_classic.txt';
      //DEMIX_Tile_Full := MDDef.DEMIX_full_all;
      DEMIXModeName := 'FULL';
      //GeomorphonsDir := UINFGeomorphonsDir;
   end
   else if (MDDef.DEMIX_mode = dmAddCoastal) then begin
      NumPtDEMs := 7;   //adds coastal
      NumAreaDEMs := 1;
      //SSIMresultsDir := CoastalSSIMresultsDir;
      //FUVresultsDir := CoastalFUVresultsDir;
      AreaListFName := DEMIXSettingsDir + 'areas_coastal.txt';
      DEMListFName := DEMIXSettingsDir + 'dems_add_coastal.txt';
      //DEMIX_Tile_Full := MDDef.DEMIX_full_U120;
      DEMIXModeName := 'U120';
      //GeomorphonsDir := U120GeomorphonsDir;
   end
   else if (MDDef.DEMIX_mode = dmAddDiluvium) then begin
      NumPtDEMs := 7;    //adds coatal
      NumAreaDEMs := 2; //adds dilumium
      //SSIMresultsDir := DiluvSSIMresultsDir;
      //FUVresultsDir := DILUVFUVresultsDir;
      AreaListFName := DEMIXSettingsDir + 'areas_diluvium.txt';
      DEMListFName := DEMIXSettingsDir + 'dems_add_diluvium.txt';
      //DEMIX_Tile_Full := MDDef.DEMIX_full_U80;
      DEMIXModeName := 'U80';
      //GeomorphonsDir := U80GeomorphonsDir;
   end
   else if (MDDef.DEMIX_mode = dmAddDelta) then begin
      NumPtDEMs := 8;    //adds coastal, delta
      NumAreaDEMs := 2;  //adds diluvium
      //SSIMresultsDir := DeltaSSIMresultsDir;
      //FUVresultsDir := DeltaFUVresultsDir;
      AreaListFName := DEMIXSettingsDir + 'areas_delta.txt';
      DEMListFName := DEMIXSettingsDir + 'dems_add_delta.txt';
      //DEMIX_Tile_Full := MDDef.DEMIX_full_U10;
      DEMIXModeName := 'U10';
      //GeomorphonsDir := U10GeomorphonsDir;
   end;

   Diff_dist_results_dir := DEMIX_Base_DB_Path + DEMIXModeName + '_diff_dist_results\';
   ChannelMissesDir := DEMIX_Base_DB_Path + DEMIXModeName + '_channel_misses\';
   GeomorphonsDir := DEMIX_Base_DB_Path + DEMIXModeName + '_geomorphons\';
   SSIMresultsDir := DEMIX_Base_DB_Path + DEMIXModeName + '_ssim_results\';
   FUVresultsDir := DEMIX_Base_DB_Path + DEMIXModeName + '_fuv_results\';

   DEMIX_Tile_Full := 25;
   {$If Defined(RecordDEMIXStart)} WriteLineToDebugFile('SetParamsForDEMIXmode out, DEMIX_mode=' + IntToStr(MDDef.DEMIX_mode)); {$EndIf}
end;


procedure RecognizeDEMIXVersion(DB : integer);
begin
   if ValidDB(DB) then begin
      if StrUtils.ANSIcontainsText(UpperCase(GISDB[db].DBName),'DEMIX') then begin
         if StrUtils.ANSIcontainsText(UpperCase(GISDB[db].DBName),'ALL') then MDDef.DEMIX_mode := dmClassic
         else if StrUtils.ANSIcontainsText(UpperCase(GISDB[db].DBName),'U120') then MDDef.DEMIX_mode := dmAddCoastal
         else if StrUtils.ANSIcontainsText(UpperCase(GISDB[db].DBName),'U80') then MDDef.DEMIX_mode := dmAddDiluvium
         else if StrUtils.ANSIcontainsText(UpperCase(GISDB[db].DBName),'U10') then MDDef.DEMIX_mode := dmAddDelta
      end
      else begin
         MDDef.DEMIX_mode := dmClassic;
         if GISDB[db].MyData.FieldExists('COAST') then MDDef.DEMIX_mode := dmAddCoastal
         else if GISDB[db].MyData.FieldExists('DELTA') then MDDef.DEMIX_mode := dmAddDelta
         else if GISDB[db].MyData.FieldExists('DILUV') then MDDef.DEMIX_mode := dmAddDiluvium;
      end;
      SetParamsForDEMIXmode;
   end;
end;


function GetDEMIXpaths(StartProcessing : boolean = true; DB : integer = 0) : boolean;
begin
   {$If Defined(RecordDEMIXStart)} WriteLineToDebugFile('GetDEMIXpaths in'); {$EndIf}
   if ValidDB(DB) then begin
      {$If Defined(RecordDEMIXStart)} WriteLineToDebugFile('GetDEMIXpaths Check DB'); {$EndIf}
      RecognizeDEMIXVersion(DB);
   end;
   if DEMIX_initialized then Result := true
   else begin
      {$If Defined(RecordDEMIXStart)} WriteLineToDebugFile('GetDEMIXpaths off to PickWineContestLocation'); {$EndIf}
      Result := PickWineContestDBLocation;
      if (MDDef.DEMIX_mode = dmNotYetDefined) then PickDEMIXMode;
   end;

   if (not Result) then exit;
   DEMIX_initialized := true;

   {$If Defined(RecordDEMIXStart)} WriteLineToDebugFile('Wine contest location done'); {$EndIf}

   if StartProcessing then begin
      HeavyDutyProcessing := true;
      WMdem.Color := clInactiveCaption;
      DEMIXProcessing := true;
      ToggleShowProgress(false);
   end;
   StopSplashing;

   //settings that can be changed, but constant here for DB creation
      ElevDiffHists := false;
      DoHorizontalShift := false;
      MDdef.MDRecordDebugLog := true;

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
   DEMIX_criteria_dbName := DEMIXSettingsDir + 'demix_criteria.dbf';

   //DEMIX_Ref_Source := DEMIX_Base_DB_Path + 'wine_contest_v2_ref_source\';
   DEMIX_Ref_Merge := DEMIX_Base_DB_Path + 'wine_contest_v2_ref_merge\';
   DEMIX_Ref_Half_sec := DEMIX_Base_DB_Path + 'wine_contest_v2_ref_0.5sec\';
   vd_path := DEMIX_Base_DB_Path + 'wine_contest_v2_vdatum\';
   DEMIX_diff_dist  := DEMIX_Base_DB_Path + 'wine_contest_v2_diff_dist\';
   DEMIX_profile_test_dir := DEMIX_Base_DB_Path + 'wine_contest_v2_topo_profiles\';
   DEMIX_distrib_graph_dir := DEMIX_Base_DB_Path + 'wine_contest_v2_difference_distrib_graphs\';
   DEMIX_3DEP_Dir := DEMIX_Base_DB_Path + 'wine_contest_v2_3dep\';

   //all_difference_results_dir := DEMIX_Base_DB_Path + 'all_difference_tile_stats\';
   DEMIX_diff_maps_dir  := DEMIX_Base_DB_Path + 'wine_contest_difference_maps\';
   DEMIX_area_lc100  := DEMIX_Base_DB_Path + 'wine_contest_lc100\';
   DEMIX_GIS_dbName := DEMIX_Base_DB_Path + 'wine_contest_database\demix_gis_db_v2.5.dbf';

   {$IfDef DEMIX_SAGA_channels}
      DEMIX_test_DEMs_no_sink := DEMIX_Base_DB_Path + 'area_test_dems_no_sink\';
      DEMIX_ref_DEMs_no_sink := DEMIX_Base_DB_Path + 'area_ref_dems_no_sink\';
      DEMIX_test_DEMs_channels := DEMIX_Base_DB_Path + 'area_test_dems_channels\';
      DEMIX_ref_DEMs_channels := DEMIX_Base_DB_Path + 'area_ref_dems_channels\';
      DEMIX_test_DEMs_channel_grids := DEMIX_Base_DB_Path + 'area_test_dems_channel_grids\';
      DEMIX_ref_DEMs_channel_grids := DEMIX_Base_DB_Path + 'area_ref_dems_channel_grids\';
   {$EndIf}

   //ChannelMissesDir := DEMIX_Base_DB_Path + 'channel_misses\';
   //GeomorphonsDir := DEMIX_Base_DB_Path + 'geomorphons\';
   //UINFGeomorphonsDir := DEMIX_Base_DB_Path + 'UINF_geomorphons\';
   //U120GeomorphonsDir := DEMIX_Base_DB_Path + 'U120_geomorphons\';
   //U80GeomorphonsDir := DEMIX_Base_DB_Path + 'U80_geomorphons\';
   //U10GeomorphonsDir := DEMIX_Base_DB_Path + 'U10_geomorphons\';

   MD_out_ref_dir := DEMIX_Base_DB_Path + 'md_out_ref_areas\';
   MD_out_test_dir := DEMIX_Base_DB_Path + 'md_out_test_areas\';
   wbt_out_ref_dir  := DEMIX_Base_DB_Path + 'wbt_out_ref_areas\';
   wbt_out_test_dir := DEMIX_Base_DB_Path + 'wbt_out_test_areas\';
   saga_out_ref_dir := DEMIX_Base_DB_Path + 'saga_out_ref_areas\';
   saga_out_test_dir := DEMIX_Base_DB_Path + 'saga_out_test_areas\';

   //RegularSSIMresultsDir := DEMIX_Base_DB_Path + 'all_ssim_results\';
   //CoastalSSIMresultsDir := DEMIX_Base_DB_Path + 'coastal_SSIM_results\';
   //DiluvSSIMresultsDir := DEMIX_Base_DB_Path + 'diluvium_SSIM_results\';
   //DeltaSSIMresultsDir := DEMIX_Base_DB_Path + 'delta_SSIM_results\';

   //RegularFUVresultsDir := DEMIX_Base_DB_Path + 'all_fuv_results\';
   //CoastalFUVresultsDir := DEMIX_Base_DB_Path + 'coastal_fuv_results\';
   //DiluvFUVresultsDir := DEMIX_Base_DB_Path + 'diluvium_fuv_results\';
   //DeltaFUVresultsDir := DEMIX_Base_DB_Path + 'delta_fuv_results\';

   DEMIX_final_DB_dir := DEMIX_Base_DB_Path + 'wine_contest_database\';

   DEMIX_Ref_1sec := DEMIX_Base_DB_Path + 'wine_contest_ref_1sec\';
   DEMIX_Ref_dsm_1sec := DEMIX_Base_DB_Path + 'wine_contest_ref_dsm_1sec\';
   DEMIX_test_dems := DEMIX_Base_DB_Path + 'wine_contest_test_dems\';
   DEMIX_diluvium_dtms := DEMIX_Base_DB_Path + 'diluvium_test_dems\';
   DEMIX_delta_dtms := DEMIX_Base_DB_Path + 'delta_test_dems\';
   DEMIX_coastal_dtms := DEMIX_Base_DB_Path + 'coastal_test_dems\';

   {$If Defined(RecordDEMIXStart)} WriteLineToDebugFile('GetDEMIXpaths point 4'); {$EndIf}

   Geoid2008FName := 'g:\geoid\egm2008-1-vdatum.tif';
   FindDriveWithFile(Geoid2008FName);
   GeoidDiffFName := 'g:\geoid\egm96_to_egm2008.tif';
   FindDriveWithFile(GeoidDiffFName);

   SafeMakeDir(MD_out_ref_dir);
   SafeMakeDir(MD_out_test_dir);
   SafeMakeDir(wbt_out_ref_dir);
   SafeMakeDir(wbt_out_test_dir);
   SafeMakeDir(saga_out_ref_dir);
   SafeMakeDir(saga_out_test_dir);

   SetParamsForDEMIXmode;
   LoadDEMIXnames;

   {$If Defined(RecordDEMIXStart) or Defined(RecordDEMIX)}
      WriteLineToDebugFile('GetDEMIXpaths out, DEMIX_mode=' + IntToStr(MDDef.DEMIX_mode) + ' ' + DEMIX_mode_abbreviation(MDDef.DEMIX_mode));
   {$EndIf}
end;


procedure EndDEMIXProcessing(db : integer = 0);
begin
   if HeavyDutyProcessing then begin
      CleanUpTempDirectory(false);
      HeavyDutyProcessing := false;
   end;
   ReportErrors := true;
   DEMIXProcessing := false;
   ToggleShowProgress(true);
   WMdem.Color := clScrollBar;
   LockStatusBar := false;
   wmdem.ClearStatusBarPanelText;
   EndProgress;
   if ValidDB(db) then begin
      GISdb[DB].ClearGISFilter;
      GISdb[DB].ShowStatus;
   end;
end;


function DEMIX_AreasWanted(CanLimitAreas : boolean = true) : tStringList;
var
   fName : PathStr;
   PickedNum : integer;
begin
   Result := tStringList.Create;
   fName := AreaListFName;
   if FileExists(fName) or GetExistingFileName('DEMIX areas','*.txt',fName) then begin
      Result.LoadFromFile(fName);
      if CanLimitAreas then MultiSelectSingleColumnStringList(DEMIX_mode_abbreviation(MDDef.DEMIX_mode) + ' Areas to process',PickedNum,Result,true,true);
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
     if (Table.FiltRecsInDB = 0) then Result := ''
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
   OnePair('COP','NASA');
   OnePair('COP','SRTM');
   OnePair('COP','ASTER');
   OnePair('COP','DILUV');
   OnePair('COP','DELTA');
   OnePair('COP','COAST');
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
      Criteria := GISdb[DBonTable].MyData.ListUniqueEntriesInDB('CRITERION');
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



initialization
   SSIMresultsDir := '';
finalization
end.


