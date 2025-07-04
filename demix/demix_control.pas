unit demix_control;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{________________________________________}

// 10/1/2023: DEMIX options under active development.
//Some options are hard coded and not error-trapped, and some options may not longer work.  Used with caution

{$I nevadia_defines.inc}

{$Define IncludeCurvatureLSPs}


//{$Define FastOpenness}  //for debugging, since openness is the slowest grid to generate


{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   {$Define RecordDEMIX}
   //{$Define RecordDEMIXneo}
   {$Define RecordDEMIXStart}
   //{$Define LoadDEMIXNames}
   //{$Define RecordDEMIXopenGrids}
   {$Define RecordTestDEMs}
   //{$Define RecordRangeScales}
   //{$Define RecordRangeScalesFull}

   //{$Define RecordDEMIXversion}
   //{$Define RecordDEMIXLoad}
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
   procedure EndDEMIXProcessing(db : integer = 0; CleanTempDir : boolean = false);
   procedure LoadDEMIXnames;
   function OpenFUVOrderedParams : tStringList;
   function GetListOfTestDEMsinUse(GeometricModel : shortstring = '') : tStringList;

   function DEMIXColorFromDEMName(DEMName : shortstring) : tPlatformColor;
   function DEMIXColorForCriterion(Criterion : shortstring) : tColor;
   function WhatTestDEMisThis(fName : PathStr) : shortstring;
   function IsDEMaDSMorDTM(DEMName : ShortString) : integer;
   function DEMIXTestDEMLegend(Horizontal : boolean = true; DEMsUsed : tstringList = nil; MaxWide : integer = 9999) : tMyBitmap;
   function FilterForDEMIXtilesToUse : shortstring;
   function FilterForDEMIXtilesToAvoid : shortstring;

//check these to see if still needed
   procedure CompareSeriousCompetitors(DBonTable : integer);
   procedure DEMIXisCOPorALOSbetter(DBonTable : integer);

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

function GetWinner(dbOnTable : integer; DEM1,DEM2 : shortstring) : shortstring;

function LoadLandcoverForDEMIXarea(AreaName : shortstring; OpenMap : boolean = true) : integer;
function ClipTheDEMtoFullDEMIXTiles(DEM : integer; NewName : PathStr = '') : boolean;

procedure SetParamsForDEMIXmode;

procedure ReinterpolateTestDEMtoHalfSec(var DEM : integer; OpenMap : boolean);

function DEMsinIndex(Index : tDEMIXindexes) : integer;

procedure OpenCopDEMandLandcoverForArea(CopLand : boolean = true);
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

procedure DifferentRankingsByTile(DBonTable : integer);
procedure FUVforRangeScales(LandCoverOption : boolean);

function DEMIXMomentStatsString(MomentVar : tMomentVar) : shortstring;
function DEMIXShortenDEMName(DEMName : shortstring) : shortstring;

   {$IfDef Old3DEP}
      procedure SubsetLargeUS3DEParea(DEM : integer = 0);
      procedure BatchSubset_3DEP_DEMs;
   {$EndIf}

   {$IfDef OpenDEMIXAreaAndCompare}
      procedure OpenDEMIXArea(fName : PathStr = '');
   {$EndIf}

   {$IfDef OldDEMIXroutines}
      procedure TransposeDEMIXwinecontestGraph(DBonTable : integer);
   {$EndIf}


var
   ElevDiffHists : boolean;
   OrderedFUVParams : tStringList;


const
   MaxOrderedParams = 20;
   NumScales = 4;
   Scales : array[1..NumScales] of shortstring = ('0.15sec','0.25sec','0.5sec','1sec');

   function GetFileNamesOfDEMinUse(var DataDir : PathStr) : tStringList;

procedure LandCoverBreakdowPointCloud;


implementation

uses
   Nevadia_Main,
   DEMstat,Make_grid,PetImage,PetImage_form,new_petmar_movie,DEMdatabase,PetDButils,Pick_several_dems,
   Geotiff, BaseMap, GDAL_tools, DEMIX_filter, DEMstringgrid,DEM_NLCD,
   DEMCoord,DEMMapf,DEMDef_routines,DEM_Manager,DEM_indexes,PetMath,
   MD_use_tools,
   DEMIX_graphs,Pick_DEMIX_mode,pick_demix_areas;

var
   DoHorizontalShift : boolean;

   {$I demix_maps.inc}

   {$I demix_open_dems.inc}

   {$IfDef Old3DEP}
      {$I old_demix_3dep_routines.inc}
   {$EndIf}


   {$If Defined(OldDEMIXroutines)}
      {$I experimental_demix_criteria.inc}
   {$EndIf}

   {$IfDef OpenDEMIXAreaAndCompare}
      {$I open_demix_area.inc}
   {$EndIf}


const
   MICRODEMcurvature = true;  //if not, using Whitebox



function OpenFUVOrderedParams : tStringList;
var
   fName : PathStr;
   Table : tMyData;
begin
   fName := DemixSettingsDir + 'demix_fuv_parameters.dbf';
   Table := tMyData.Create(fName);
   Table.ApplyFilter('USE=' + QuotedStr('Y'));
   Result := Table.ListUniqueEntriesInDB('PARAMETER',false);
   Table.Destroy;
end;


function GetListOfTestDEMsinUse(GeometricModel : shortstring = '') : tStringList;
var
   fName : PathStr;
   aFilter : shortstring;
   Table : tMyData;
begin
   fName := DemixSettingsDir + 'demix_dems.dbf';
   Table := tMyData.Create(fName);
   aFilter := 'USE=' + QuotedStr('Y');
   if (GeometricModel <> '') then aFilter := aFilter + ' AND MODEL=' + QuotedStr(GeometricModel);
   Table.ApplyFilter(aFilter);
   Result := Table.ListUniqueEntriesInDB('SHORT_NAME',false);
   Table.Destroy;
   {$IfDef RecordTestDEMs} WriteLineToDebugFile('GetListOfTestDEMsinUse, geometry=' + GeometricModel); WriteStringListToDebugFile(Result,true); {$EndIf}
end;


function DEMIXShortenDEMName(DEMName : shortstring) : shortstring;
//used for labels on graphs so they are shorter
begin
   Result := StringReplace(DEMName, 'slope_', '',[rfIgnoreCase]);
end;


function DEMIXMomentStatsString(MomentVar : tMomentVar) : shortstring;
begin
   Result := RealToString(MomentVar.MinZ,-8,2) + ',' + RealToString(MomentVar.MaxZ,-8,2) + ',' + RealToString(MomentVar.Mean,-8,2) + ',' +
       RealToString(MomentVar.avg_dev,-8,2) + ',' + RealToString(MomentVar.std_dev,-8,2) + ',' + RealToString(MomentVar.median,-8,2) + ',' + RealToString(MomentVar.rmse,-8,2)  + ',' +
       RealToString(MomentVar.mae,-8,2)  + ',' + RealToString(MomentVar.LE90,-12,-2) + ',' + IntToStr(MomentVar.NPts);
end;


function GetFileNamesOfDEMinUse(var DataDir : PathStr) : tStringList;
var
   TestDEMs,DEMFiles : tStringList;
   i,j : integer;
begin
   GetDEMIXpaths;
   if not ValidPath(DataDir) then begin
      DataDir := 'J:\aaa_neo_eval\oxnard\0.15_sec_tests\';
      GetDOSPath('with DEMs',DataDir);
   end;
   TestDEMs := GetListOfTestDEMsinUse;
   TestDEMs.Insert(0,'ref_DTM');

   DEMFiles := Nil;
   FindMatchingFiles(DataDir,'*.tif',DEMfiles);

   Result := tStringList.Create;
   for i := 0 to pred(TestDEMs.Count) do begin
      for j := 0 to pred(DEMfiles.Count) do begin
          if (Uppercase(ExtractFileNameNoExt(DEMfiles.Strings[j])) = UpperCase(TestDEMs.Strings[i])) then begin
             Result.Add(DEMfiles.Strings[j]);
          end;
      end;
   end;
   DEMFiles.Destroy;
   TestDEMs.Destroy;
   {$IfDef RecordRangeScales} WriteLineToDebugFile('DEMNs'); WriteStringListToDebugFile(Result,true); {$EndIf}
end;


procedure FUVforRangeScales(LandCoverOption : boolean);
var
   Values : ^bfarray32;
   RefDEMs,TestDEMs : array[0..MaxOrderedParams] of integer;
   ParamNames : array[0..MaxOrderedParams] of shortstring;
   DataDir, fName : PathStr;
   db,ESA_LC10 : integer;
   Area,aLine,bline : shortstring;
   DiffDistFindings,FUVFindings : tStringList;
   LastLSP : integer;


   {$IfDef TrackElevationPointers}
      procedure CheckElevationPointers(Report : shortstring);
      var
         i : integer;
         RefGood,TestGood : boolean;
         RefMess,TestMess : shortstring;
      begin
         RefGood := true;
         TestGood := true;
         RefMess := '';
         TestMess := '';
         for I := 1 to MaxOrderedParams do begin
            if not DEMGlb[RefDEMs[i]].ElevationStructuresAllocated then begin RefGood := false; RefMess := RefMess + ' ' + IntToStr(i); end;
            if not DEMGlb[TestDEMs[i]].ElevationStructuresAllocated then begin TestGood := false; TestMess := TestMess + ' ' + IntToStr(i); end;
         end;
         if RefGood and TestGood then WriteLineToDebugFile(Report + ' DEMs memory allocated');
         if not RefGood then HighLightLineToDebugFile(Report + ' Ref DEMs memory allocation problem' + RefMess);
         if not TestGood then HighLightLineToDebugFile(Report + ' Test DEMs memory allocation problem' + TestMess);
      end;
   {$EndIf}


   procedure LoadLSPs;
   var
      ad,Radius,i : integer;


         procedure CheckLSP_calc(kwat : shortstring);
         begin
            if (OrderedFUVParams.IndexOf(UpperCase(kwat)) <> -1) then begin
               wmDEM.SetPanelText(3,'Load LSP ' + kwat,true);
               inc(LastLSP);
               ParamNames[LastLSP] := UpperCase(kwat);
               RefDEMs[LastLSP] := RUN_LSPcalculator(RefDEMs[1],'--' + kwat,false);
               TestDEMs[LastLSP] := RUN_LSPcalculator(TestDEMs[1],'--' + kwat,false);
            end;
         end;


   begin
      {$IfDef RecordRangeScales} HighLightLineToDebugFile(DEMglb[TestDEMs[1]].AreaName + ' Load LSPs in'); {$EndIf}
      LastLSP := 1;
      ParamNames[1] := 'ELEV';

      if (OrderedFUVParams.IndexOf('OPENU') <> -1) then begin
        wmDEM.SetPanelText(3,'Load LSP Openness',true);
        ParamNames[LastLSP+1] := 'OPENU';
        ParamNames[LastLSP+2] := 'DOWNU';
        ad := 0;        //difference, not to be computed
        {$IfDef FastOpenness} Radius := 20; {$Else} Radius := 250; {$EndIf}

        RefDEMs[LastLSP+1] := -1;     //upward
        RefDEMs[LastLSP+2] := -1;     //downward
        CreateOpennessMap(false,DEMglb[RefDEMs[1]].FullDEMGridLimits,RefDEMs[1],Radius,-99,RefDEMs[LastLSP+1],RefDEMs[LastLSP+2],ad);
        DEMGlb[RefDEMs[LastLSP+1]].WriteNewFormatDEM(MDTempDir + 'openu_' + DEMglb[RefDEMs[1]].AreaName + '.dem');
        DEMGlb[RefDEMs[LastLSP+2]].WriteNewFormatDEM(MDTempDir + 'opend_' + DEMglb[RefDEMs[1]].AreaName + '.dem');

        TestDEMs[LastLSP+1] := -1;    //upward
        TestDEMs[LastLSP+2] := -1;    //downward
        CreateOpennessMap(false,DEMglb[TestDEMs[1]].FullDEMGridLimits,TestDEMs[1],Radius,-99,TestDEMs[LastLSP+1],TestDEMs[LastLSP+2],ad);
        DEMGlb[TestDEMs[LastLSP+1]].WriteNewFormatDEM(MDTempDir + 'openu_' + DEMglb[TestDEMs[1]].AreaName + '.dem');
        DEMGlb[TestDEMs[LastLSP+2]].WriteNewFormatDEM(MDTempDir + 'opend_' + DEMglb[TestDEMs[1]].AreaName + '.dem');
        inc(LastLSP,2);
      end;

      if (OrderedFUVParams.IndexOf('HILL') <> -1) then begin
         wmDEM.SetPanelText(3,'Load LSP Hillshade',true);
         inc(LastLSP);
         ParamNames[LastLSP] := 'HILL';
         RefDEMs[LastLSP] := CreateHillshadeMap(false,RefDEMs[1],MDTempDir + 'hillshade_' + DEMglb[RefDEMs[1]].AreaName + '.dem');
         TestDEMs[LastLSP] := CreateHillshadeMap(false,TestDEMs[1],MDTempDir + 'hillshade_' + DEMglb[TestDEMs[1]].AreaName + '.dem');
      end;

      if (OrderedFUVParams.IndexOf('SLOPE') <> -1) then begin
        wmDEM.SetPanelText(3,'Load LSP slope',true);
        RefDEMs[LastLSP+1] := 0;   //so slope returned
        TestDEMs[LastLSP+1] := 0;  //so slope returned
        ParamNames[LastLSP+1] := 'SLOPE';
        ParamNames[LastLSP+2] := 'RUFF';
        RefDEMs[LastLSP+2] := CreateSlopeRoughnessSlopeStandardDeviationMap(RefDEMs[1],5,RefDEMs[LastLSP+1],false);
        TestDEMs[LastLSP+2] := CreateSlopeRoughnessSlopeStandardDeviationMap(TestDEMs[1],5,TestDEMs[LastLSP+1],false);
        DEMGlb[RefDEMs[LastLSP+1]].WriteNewFormatDEM(MDTempDir + 'slope_' + DEMglb[RefDEMs[1]].AreaName + '.dem');
        DEMGlb[TestDEMs[LastLSP+1]].WriteNewFormatDEM(MDTempDir + 'slope_' + DEMglb[TestDEMs[1]].AreaName + '.dem');
        DEMGlb[RefDEMs[LastLSP+2]].WriteNewFormatDEM(MDTempDir + 'ruff_' + DEMglb[RefDEMs[1]].AreaName + '.dem');
        DEMGlb[TestDEMs[LastLSP+2]].WriteNewFormatDEM(MDTempDir + 'ruff_' + DEMglb[TestDEMs[1]].AreaName + '.dem');
        inc(LastLSP,2);
      end;

      if (OrderedFUVParams.IndexOf('TPI') <> -1) then begin
         wmDEM.SetPanelText(3,'Load LSP TPI',true);
         inc(LastLSP);
         ParamNames[LastLSP] := 'TPI';
         RefDEMs[LastLSP] := BoxCarDetrendDEM(false,RefDEMs[1],{DEMGlb[RefDEMs[1]].FullDEMGridLimits,}3,MDTempDir + 'tpi_' + DEMglb[RefDEMs[1]].AreaName + '.dem');
         TestDEMs[LastLSP] := BoxCarDetrendDEM(false,TestDEMs[1],{DEMGlb[TestDEMs[1]].FullDEMGridLimits,}3,MDTempDir + 'tpi_' + DEMglb[TestDEMs[1]].AreaName + '.dem');
      end;

      if (OrderedFUVParams.IndexOf('RRI') <> -1) then begin
         wmDEM.SetPanelText(3,'Load LSP RRI',true);
         inc(LastLSP);
         ParamNames[LastLSP] := 'RRI';
         RefDEMs[LastLSP] := MakeTRIGrid(RefDEMs[1],nmRRI,false,MDTempDir + 'rri_' + DEMglb[RefDEMs[1]].AreaName + '.dem');
         TestDEMs[LastLSP] := MakeTRIGrid(TestDEMs[1],nmRRI,false,MDTempDir + 'rri_' + DEMglb[TestDEMs[1]].AreaName + '.dem');
      end;

      {$IfDef IncludeCurvatureLSPs}
         if (OrderedFUVParams.IndexOf('PLANC') <> -1) then begin
            wmDEM.SetPanelText(3,'Load LSP PLANC',true);
            inc(LastLSP);
            ParamNames[LastLSP] := 'PLANC';
            if MICRODEMcurvature then begin
               RefDEMs[LastLSP] := CreateCurvatureMap(eucurv_plan,false,RefDEMs[1]);
               TestDEMs[LastLSP] := CreateCurvatureMap(eucurv_plan,false,TestDEMs[1]);
            end
            else begin
               RefDEMs[LastLSP] := WBT_PlanCurvature(false,DEMGlb[RefDEMs[1]].GeotiffDEMName,MDtempDir + 'wbt_plan_curv.tif');
               TestDEMs[LastLSP] := WBT_PlanCurvature(false,DEMGlb[TestDEMs[1]].GeotiffDEMName,MDtempDir + 'wbt_plan_curv.tif');
            end;
         end;
         if (OrderedFUVParams.IndexOf('PROFC') <> -1) then begin
            wmDEM.SetPanelText(3,'Load LSP PROFC',true);
            inc(LastLSP);
            ParamNames[LastLSP] := 'PROFC';
            if MICRODEMcurvature then begin
               RefDEMs[LastLSP] := CreateCurvatureMap(eucurv_prof,false,RefDEMs[1]);
               TestDEMs[LastLSP] := CreateCurvatureMap(eucurv_prof,false,TestDEMs[1]);
            end
            else begin
               RefDEMs[LastLSP] := WBT_ProfileCurvature(false,DEMGlb[RefDEMs[1]].GeotiffDEMName,MDtempDir + 'wbt_prof_curv.tif');
               TestDEMs[LastLSP] := WBT_ProfileCurvature(false,DEMGlb[TestDEMs[1]].GeotiffDEMName,MDtempDir + 'wbt_prof_curv.tif');
            end;
         end;
         if (OrderedFUVParams.IndexOf('TANGC') <> -1) then begin
            wmDEM.SetPanelText(3,'Load LSP TANGC',true);
            inc(LastLSP);
            ParamNames[LastLSP] := 'TANGC';
            if MICRODEMcurvature then begin
               RefDEMs[LastLSP] := CreateCurvatureMap(eucurv_tang,false,RefDEMs[1]);
               TestDEMs[LastLSP] := CreateCurvatureMap(eucurv_tang,false,TestDEMs[1]);
             end
             else begin
               RefDEMs[LastLSP] := WBT_TangentialCurvature(false,DEMGlb[RefDEMs[1]].GeotiffDEMName,MDtempDir + 'wbt_tang_curv.tif');
               TestDEMs[LastLSP] := WBT_TangentialCurvature(false,DEMGlb[TestDEMs[1]].GeotiffDEMName,MDtempDir + 'wbt_tang_curv.tif');
             end;
         end;

         CheckLSP_calc('knss');
         CheckLSP_calc('kncc');
         CheckLSP_calc('kncs');
      {$EndIf}
      {$IfDef RecordRangeScales} WriteLineToDebugFile('Load LSPs out, LastLSP=' + IntToStr(LastLSP)); {$EndIf}
   end;


   procedure ComputeOneFUVLine;
   var
      i,j,DiffMap : integer;
      MomentVar : tMomentVar;
      FUV : float32;
      cLine : shortstring;
      Min,Max,Mean,Median : float32;
   begin
      {$IfDef TrackElevationPointers} CheckElevationPointers('start ComputeOneFUVLine'); {$EndIf}

      if (DiffDistFindings <> Nil) then begin
          for j := 1 to LastLSP do begin
            {$IfDef RecordRangeScales} WriteLineToDebugFile(IntToStr(j) + ' ' + ParamNames[j] +
               ' ref=' + IntToStr(RefDEMs[j]) + ' test=' + IntToStr(TestDEMs[j])); {$EndIf}

            wmDEM.SetPanelText(3,'Diff Dist ' + ParamNames[j],true);
            DiffMap := MakeDifferenceMap(TestDEMs[j],RefDEMs[j],RefDEMs[j],0,false,false,false);
            if ValidDEM(DiffMap) then begin
              InitializeMomentVar(MomentVar);
              DEMglb[DiffMap].GetElevationsInLongArray(DEMglb[DiffMap].FullDEMGridLimits, MomentVar.NPts,Values^,Min,Max);
              //first get signed values
              moment(Values^,MomentVar,msAll);
              Min := MomentVar.MinZ;
              Max := MomentVar.MaxZ;
              Mean := MomentVar.Mean;
              Median := MomentVar.Median;
              //now get unsigned
              moment(Values^,MomentVar,msAll);
              for i := 1 to MomentVar.Npts do Values^[i] := abs(Values^[i]);
              moment(Values^,MomentVar,msAll);
              MomentVar.MinZ := Min;
              MomentVar.MaxZ := Max;
              MomentVar.Mean := Mean;
              MomentVar.Median := Median;
              cline := bLine + ',' + ParamNames[j] + ',' + DEMIXMomentStatsString(MomentVar);
              DiffDistFindings.Add(cline);
              CloseSingleDEM(DiffMap);
            end;
          end;
      end;

      for j := 1 to LastLSP do begin
         wmDEM.SetPanelText(3,'FUV ' + ParamNames[j],true);
         FUV := GetFUVForPairGrids(DEMglb[RefDEMs[j]].FullDEMGridLimits,TestDEMs[j],RefDEMs[j]);
         aline := aLine + ',' + RealToString(FUV,-12,-6);
         {$IfDef RecordRangeScalesFull} WriteLineToDebugFile(IntToStr(j) + ' ' + ParamNames[j] + ' fuv=' + RealToString(FUV,-12,-6)); {$EndIf}
      end;
      FUVFindings.Add(Aline);

      {$IfDef RecordRangeScalesFull} WriteLineToDebugFile('ComputeOneFUVLine  ' + aLine); {$EndIf}
      {$IfDef TrackElevationPointers} CheckElevationPointers('end ComputeOneFUVLine'); {$EndIf}
   end;

      procedure OneLandCover(LandType : shortstring; Code : integer);
      var
         Fixed : int64;
         j : integer;
      begin
         {$IfDef RecordRangeScales} WriteLineToDebugFile('OneLandCover, start ' + LandType); {$EndIf}
         wmDEM.SetPanelText(2,LandType,true);
         if (Code <> 0) then begin
            wmDEM.SetPanelText(3,'Mask grids',true);
            {$IfDef TrackElevationPointers} CheckElevationPointers('Start Masking ' + LandType); {$EndIf}
            DEMGLb[ESA_LC10].MarkOutsideRangeMissing(Code-0.01,Code+0.01,Fixed,false);
              for j := 1 to LastLSP do begin
                 if ValidDEM(RefDEMs[j]) and ValidDEM(TestDEMs[j]) then begin
                    MaskGridFromSecondGrid(RefDEMs[j],ESA_LC10, msSecondMissing);
                    MaskGridFromSecondGrid(TestDEMs[j],ESA_LC10, msSecondMissing);
                 end
                 else begin
                    {$IfDef RecordRangeScales} WriteLineToDebugFile('Tried to use bad grid'); {$EndIf}
                 end;
              end;
         end;
         Fixed := DEMglb[TestDEMs[1]].ComputeNumberValidPoints(DEMglb[TestDEMs[1]].FullDEMGridLimits);
         bline := Area + ', ,' + LandType + ',' + DEMglb[TestDEMs[1]].AreaName;
         aline := bline + ',' + IntToStr(Fixed);
         ComputeOneFUVLine;
         if (Code <> 0) then begin
            wmDEM.SetPanelText(3,'UnMask LSPs',true);
            DEMGLb[ESA_LC10].ReloadDEM(true);
            for j := 1 to LastLSP do begin
               DEMGlb[RefDEMs[j]].ReloadDEM(true);
               DEMGlb[TestDEMs[j]].ReloadDEM(true);
            end;
         end;
         {$IfDef RecordRangeScales} WriteLineToDebugFile('OneLandCover, end ' + LandType); {$EndIf}
         {$IfDef TrackElevationPointers} CheckElevationPointers('LSPs, after ' + LandType) {$EndIf}
      end;

      procedure CloseLSPs;
      var
         j : integer;
      begin
         for j := 1 to LastLSP do begin
            CloseSingleDEM(RefDEMs[j]);
            CloseSingleDEM(TestDEMs[j]);
         end;
      end;

      procedure SaveAndOpenDB(Findings : tStringList);
      begin
         {$IfDef RecordRangeScales} WriteLineToDebugFile('SaveAndOpenDB: ' + fName + '  Findings.Count=' + IntToStr(FUVFindings.Count)); {$EndIf}
         db := StringList2CSVtoDB(Findings,fName,true);
         MainGraphOptions(db,Nil,Nil);
      end;

      procedure StartFindings;
      var
         j : integer;
         TStr : shortstring;
      begin
         if LandCoverOption then TStr := 'LANDCOVER,DEM,NPTS' else Tstr := 'RESOLUTION';
         FUVFindings := tStringList.Create;
         aLine := 'AREA,DEMIX_TILE,' + TStr;
         for j := 0 to pred(OrderedFUVParams.Count) do aline := aline + ',' + OrderedFUVParams[j] + '_FUV';
         FUVFindings.Add(aLine);
         {$IfDef RecordRangeScalesFull} WriteLineToDebugFile('aline=' + aline); {$EndIf}

         DiffDistFindings := tStringList.Create;
         aLine := 'AREA,DEMIX_TILE,LANDCOVER,DEM,CRITERION';
         for j := 1 to 10 do aline := aline + ',' + LongParamSuffixes[j];
         DiffDistFindings.Add(aLine);
      end;

var
   TStr : shortstring;
   i,j : integer;
   TestDEMList : tStringList;
begin {procedure FUVforRangeScales}
   {$IfDef RecordRangeScales} WriteLineToDebugFile('FUVforRangeScale in'); {$EndIf}
   GetDEMIXpaths;
   SetColorForProcessing;
   LockStatusBar := true;
   Area := 'oxnard';
   DataDir := 'J:\aaa_neo_eval\' + Area + '\';
   GetDOSPath('with DEMs',DataDir);
      for i := 0 to MaxOrderedParams do begin
        //if i <> 1 then begin
           RefDEMs[i] := 0;
           TestDEMs[i] := 0;
           ParamNames[i] := '';
        //end;
      end;

   if LandCoverOption then begin
      {$IfDef RecordRangeScales} WriteLineToDebugFile('FUVforRangeScale Landcover options'); {$EndIf}
      New(Values);
      TestDEMList := GetListOfTestDEMsinUse;
      TestDEMList.Insert(0,'ref_DTM');
      StartFindings;
      for j := 1 to pred(TestDEMList.Count) do begin
        RefDEMs[1] := OpenNewDEM(DataDir + TestDEMList.Strings[0] + '.tif',false,'reference DTM');
        TestDEMs[1] := OpenNewDEM(DataDir + TestDEMList.Strings[j] + '.tif',false,'test DEM');
        wmDEM.SetPanelText(1,IntToStr(j) + '/' + IntToStr(pred(TestDEMList.Count)) + '  ' + DEMGlb[TestDEMs[1]].AreaName,true);
        if (j = 1) then ESA_LC10 := LoadLC10LandCover('',DEMGlb[RefDEMs[1]].DEMBoundBoxGeo,false);
        LoadLSPs;
        {$IfDef TrackElevationPointers} CheckElevationPointers('LSPs loaded'); {$EndIf}
        OneLandCover('All',0);
        if false then begin
          OneLandCover('Forest',10);
          OneLandCover('Shrub',20);
          OneLandCover('Grassland',30);
          OneLandCover('Urban',50);
          OneLandCover('Barren',60);
        end;
        wmDEM.SetPanelText(2,'',true);
        CloseLSPs;
      end;
      fName := DataDir + Area + '_FUV_by_LandCover.dbf';
      FUVFindings.SaveToFile(MDTempDir + 'fuv_results.csv') ;
      SaveAndOpenDB(FUVFindings);
      fName := DataDir + Area + '_diff_dist_by_LandCover.dbf';
      SaveAndOpenDB(DiffDistFindings);

      CloseSingleDEM(ESA_LC10);
      Dispose(Values);
   end
   else begin
      StartFindings;
      for i := 1 to NumScales do begin
         {$IfDef RecordRangeScales} HighlightLineToDebugFile('FUVforRangeScale, start ' + Scales[i]); {$EndIf}
         RefDEMs[1] := OpenNewDEM(DataDir + 'ref_dtm_' + Scales[i] + '.tif',false);
         TestDEMs[1] := OpenNewDEM(DataDir + 'Neodtm_' + Scales[i] + '.tif',false);
         LoadLSPs;
         aline := Area + ', ,' + Scales[i];
         ComputeOneFUVLine;
         CloseLSPs;
      end;
      fName := DataDir + Area + DEMglb[RefDEMs[1]].AreaName + '_FUV_Range_scales.dbf';
      SaveAndOpenDB(FUVFindings);
   end;

   //MessageToContinue('Track down why dbOnTable is not getting set');

   {$IfDef RecordRangeScales} WriteLineToDebugFile('FUVforRangeScale out'); {$EndIf}
   LockStatusBar := false;
   SetColorForWaiting;
end {procedure FUVforRangeScales};


procedure LandCoverBreakdowPointCloud;
var
   MaskDEM, RefDEM,ESA_LC10 : integer;
   DataDir,fName : PathStr;
   Area : shortstring;
   Findings : tStringList;
   aLine : shortstring;

      procedure OneLandCover(LandType : shortstring; Code : integer);
      var
         Fixed,Fixed2 : int64;
         j : integer;
      begin
         {$IfDef RecordRangeScales} HighlightLineToDebugFile('OneLandCover, start ' + LandType); {$EndIf}
         if (Code <> 0) then begin
            wmDEM.SetPanelText(1,'Mask grids',true);
            {$IfDef TrackElevationPointers} CheckElevationPointers('Start Masking ' + LandType); {$EndIf}
            DEMGLb[ESA_LC10].MarkOutsideRangeMissing(Code-0.01,Code+0.01,Fixed,false);
            if (Code <> 0) then begin
              //for j := 1 to LastLSP do begin
                 if ValidDEM(RefDEM) and ValidDEM(MaskDEM) then begin
                    MaskGridFromSecondGrid(RefDEM,ESA_LC10, msSecondMissing);
                    MaskGridFromSecondGrid(MaskDEM,ESA_LC10, msSecondMissing);
                 end;
              //end;
            end;
         end;
         Fixed := DEMglb[RefDEM].ComputeNumberValidPoints(DEMglb[RefDEM].FullDEMGridLimits);
         Fixed2 := DEMglb[MaskDEM].ComputeNumberValidPoints(DEMglb[MaskDEM].FullDEMGridLimits);
         aline := Area + ', ,' + LandType + ',' + IntToStr(Fixed) + ',' + IntToStr(Fixed2);
         Findings.Add(aline);
         if (Code <> 0) then begin
            wmDEM.SetPanelText(1,'UnMask LSPs',true);
            {$IfDef TrackElevationPointers} CheckElevationPointers('Start UnMasking ' + LandType); {$EndIf}
            DEMGLb[ESA_LC10].ReloadDEM(true);
            //for j := 1 to LastLSP do begin
               DEMGlb[RefDEM].ReloadDEM(true);
               DEMGlb[MaskDEM].ReloadDEM(true);
            //end;
         end;
         {$IfDef TrackElevationPointers} CheckElevationPointers('LSPs, after ' + LandType) {$EndIf}
      end;


begin {procedure LandCoverBreakdownPointCloud}
   {$IfDef RecordRangeScales} WriteLineToDebugFile('FUVforRangeScale in'); {$EndIf}
   GetDEMIXpaths;
   SetColorForProcessing;
   LockStatusBar := true;

   //theParams := OpenFUVOrderedParams;

   Area := 'oxnard';
   DataDir := 'J:\aaa_neo_eval\' + Area + '\';
   GetDOSPath('with DEMs',DataDir);

   Findings := tStringList.Create;
   aLine := 'AREA,DEMIX_TILE,LANDCOVER,REF_DTM,PC_DTM';
   Findings.Add(aLine);

     {$IfDef RecordRangeScales} WriteLineToDebugFile('FUVforRangeScale Landcover options'); {$EndIf}
      MaskDEM := OpenNewDEM(DataDir + 'Point_Cloud_dtm.tif',false,'mask DTM');  //this is not working yet
      RefDEM := OpenNewDEM(DataDir + 'ref_dtm.tif',false,'reference DTM');
       DEMglb[RefDEM].DEMFileName := MDTempDir + ExtractFileName(DEMglb[RefDEM].DEMFileName);
       DEMglb[RefDEM].WriteNewFormatDEM(DEMglb[RefDEM].DEMFileName);
       DEMglb[MaskDEM].DEMFileName := MDTempDir + ExtractFileName(DEMglb[MaskDEM].DEMFileName);
       DEMglb[MaskDEM].WriteNewFormatDEM(DEMglb[MaskDEM].DEMFileName);

      ESA_LC10 := LoadLC10LandCover('',DEMGlb[RefDEM].DEMBoundBoxGeo,false);
      {$IfDef TrackElevationPointers} CheckElevationPointers('LSPs loaded'); {$EndIf}
      OneLandCover('All',0);
      OneLandCover('Forest',10);
      OneLandCover('Shrub',20);
      OneLandCover('Grassland',30);
      OneLandCover('Urban',50);
      OneLandCover('Barren',60);
      CloseSingleDEM(ESA_LC10);
      CloseSingleDEM(RefDEM);
      CloseSingleDEM(MaskDEM);
   fName := MDtempDir + 'Compare_point_cloud_DTM_landcover.dbf';
   StringList2CSVtoDB(Findings,fName,true);
   LockStatusBar := false;
   SetColorForWaiting;
end {procedure LandCoverBreakdownPointCloud};




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
     MDDef.DEMIX_mode := dmFull;
     MessageToContinue('Set to classic; it has to be defined');
  end;
  PickDEMIXmodeForm.Destroy;
  {$IfDef RecordDEMIX} WriteLineToDebugFile('PickDEMIXMode out'); {$EndIf}
end;


function FilterForDEMIXtilesToUse : shortstring;
begin
   Result := 'GRID_FULL>' + IntToStr(MDDef.DEMIX_Tile_Full) + ' AND RELIEF>1';
end;

function FilterForDEMIXtilesToAvoid : shortstring;
begin
   Result := 'GRID_FULL<' + IntToStr(MDDef.DEMIX_Tile_Full) + ' OR RELIEF<1';
end;


function DEMIXColorForCriterion(Criterion : shortstring) : tColor;
var
   Table : tMyData;
begin
   Table := tMyData.Create(DEMIX_criteria_dbName);
   Table.ApplyFilter('CRITERION=' + QuotedStr(Criterion));
   if (Table.FiltRecsInDB = 1) then Result := Table.TColorFromTable
   else begin
      Result := clRed;
   end;
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
      GISdb[DB].ApplyGISFilter('GRID_FULL >' + IntToStr(MDDef.DEMIX_Tile_Full));
      if (GISdb[DB].MyData.FiltRecsInDB = 0) then begin
         Result := false;
      end
      else begin
         bb2.xmax := GISdb[DB].MyData.FindFieldMax('LONG_HI') + Fudge;
         bb2.xmin := GISdb[DB].MyData.FindFieldMin('LONG_LOW') - Fudge;
         bb2.ymax := GISdb[DB].MyData.FindFieldMax('LAT_HI') + Fudge;
         bb2.ymin := GISdb[DB].MyData.FindFieldMin('LAT_LOW') - Fudge;
         {$If Defined(RecordCartoFull)} WriteLineToDebugFile('TMapForm.ClipDEMtoFullDEMIXTiles, tile boundaries ' + sfBoundBoxToString(bb2,8)); {$EndIf}
         //bb.xmax := Petmath.MinFloat(bb.xMax,bb2.xMax);
         //bb.xmin := Petmath.MaxFloat(bb.xmin,bb2.xMin);
         //bb.ymax := Petmath.MinFloat(bb.yMax,bb2.yMax);
         //bb.ymin := Petmath.MaxFloat(bb.yMin,bb2.YMin);
         bb := IntersectionTwoGeoBoundBoxes(bb,bb2);

         {$If Defined(RecordCartoFull)} WriteLineToDebugFile('TMapForm.ClipDEMtoFullDEMIXTiles, map full tiles ' + sfBoundBoxToString(bb,8)); {$EndIf}
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
   TheFiles : tStringList;
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


procedure DifferentRankingsByTile(DBonTable : integer);
//this is slow and very brute force, but will not be called often; just take a break
const
   Params : array[1..3] of shortstring = ('ELVD*','SLPD*','RUFD*');
var
   sl,tiles : tStringList;
   aline,Tile : shortstring;
   i,j : integer;
   fName : PathStr;
begin
   if GISdb[DBonTable].MyData.FieldExists('DEM_LOW_SC') then begin
      GetDEMIXpaths(true,DBonTable);
      GISdb[DBonTable].EmpSource.Enabled := false;
      GISdb[DBonTable].ClearGISFilter;
      Tiles := GISdb[DBonTable].MyData.ListUniqueEntriesInDB('DEMIX_TILE');
      sl := tStringList.Create;
      aline := 'DEMIX_TILE,ELVD,SLPD,RUFD,ALL';
      sl.Add(aline);
      for i := 0 to pred(Tiles.Count) do begin
         Tile := Tiles.Strings[i];
         wmdem.SetPanelText(1,IntToStr(i) + '/' + IntToStr(Tiles.Count) + ' ' + Tile,true);
         aline := Tile + ',';
         for j := 1 to 3 do begin
            GISdb[DBonTable].ApplyGISFilter('DEMIX_TILE=' + QuotedStr(Tile) + ' AND CRITERION=' + QuotedStr(Params[j]));
            aline := aLine + IntToStr(GISdb[DBonTable].MyData.NumUniqueEntriesInDB('DEM_LOW_SC')) + ',';
            GISdb[DBonTable].EmpSource.Enabled := false;
         end;
         GISdb[DBonTable].ApplyGISFilter('DEMIX_TILE=' + QuotedStr(Tile));
         aline := aLine + IntToStr(GISdb[DBonTable].MyData.NumUniqueEntriesInDB('DEM_LOW_SC'));
         GISdb[DBonTable].EmpSource.Enabled := false;
         sl.Add(aline);
         //MessageToContinue(aline);
      end;
      fName := NextFileNumber(MDTempDir,'different_rankings_by_tile','.dbf');
      StringList2CSVtoDB(sl,fName);
      EndDEMIXProcessing(dbOnTable);
   end
   else MessageToContinue('Missing required field, DEM_LOW_SC');
end;



procedure CompareRankings(DBonTable : integer);
var
   Tile,Crit,aline,Scores,Tstr : shortstring;
   i,j,k,N,DEM : integer;
   //rfile : file;
   theTiles,sl,Crits : tStringList;
   fName : PathStr;
   ScoreSheet,ScoreSheet2 : array[0..14] of shortstring;
begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('CompareRankings in'); {$EndIf}
   GetDEMIXpaths;
   GISdb[DBonTable].EmpSource.Enabled := false;
   theTiles := GISdb[DBonTable].MyData.ListUniqueEntriesInDB('DEMIX_TILE');

   Crits := tStringList.Create;
   sl := tStringList.Create;

   aline := 'DEMIX_TILE';
   for I := 1 to 3 do begin
      for j := 1 to 5 do begin
         aline := aline + ',' + DiffParams[i] + ParamSuffixes[j];
         Crits.Add(DiffParams[i] + ParamSuffixes[j]);
      end;
   end;
   sl.Add(Aline);

   LoadDEMIXnames;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('CompareRankings for tiles=' + IntToStr(TheTiles.Count)); {$EndIf}
   StartProgress('Compare rankings');
   for k := 0 to pred(theTiles.Count) do begin
      if (k mod 10 = 0) then wmdem.SetPanelText(1,IntToStr(k) + '/' + IntToStr(TheTiles.Count));
      Tile := theTiles[k];
      GISdb[DBonTable].ApplyGISFilter('DEMIX_TILE=' + QuotedStr(Tile) + ' AND REF_TYPE=' + QuotedStr('DTM'));
      GISdb[DBonTable].EmpSource.Enabled := false;
      aline := Tile;

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
       sl.Add(Aline);
   end;

   fName := NextFileNumber(MDTempDir,'compare_ranking_','.dbf');
   StringList2CSVtoDB(sl,fName);
   EndDEMIXProcessing(dbOnTable);
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('CompareRankings out'); {$EndIf}
end;



function DEMIXColorFromDEMName(DEMName : shortstring) : tPlatformColor;
var
   i : integer;
   table : tMyData;
   fName : PathStr;
begin
   //DEMName := ExtractDEMIXDEMName(DEMName);
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
   j : integer;
begin
   Result := 0;
   if ValidDEM(ThisTestDEM) then begin
      if StrUtils.AnsiContainsText(DEMGlb[ThisTestDEM].AreaName,'ALOS') then begin
          for j := 1 to MaxDemixDEM do begin
             if ValidDEM(RefDEMs[j]) then begin
                if (not StrUtils.AnsiContainsText(DEMGlb[RefDEMs[j]].AreaName,'dsm')) and StrUtils.AnsiContainsText(DEMGlb[RefDEMs[j]].AreaName,'area') then begin
                   Result := RefDEMs[j];
                end;
             end;
          end;
      end
      else begin

   //Need to deal with the high lat DEMs
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
   if StrUtils.AnsiContainsText(TestSeries,'ALOS') or StrUtils.AnsiContainsText(TestSeries,'AW3D') or StrUtils.AnsiContainsText(TestSeries,'DILUV')
          or StrUtils.AnsiContainsText(TestSeries,'EDTM') or StrUtils.AnsiContainsText(TestSeries,'GEDTM') then begin
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
   fName : PathStr;
begin
    fName := DEMIXSettingsDir + 'demix_dems.dbf';
    {$If Defined(RecordDEMIXStart) or Defined(LoadDEMIXNames)} WriteLineToDebugFile('LoadDEMIXnames in ' + fName); {$EndIf}
    if FileExists(fName) then begin
       Table := tMyData.Create(fName);
       NumDEMIXtestDEM := 0;
       while not Table.eof do begin
          if Table.GetFieldByNameAsString('USE') = 'Y' then begin
             inc(NumDEMIXtestDEM);
             DEMIXDEMTypeName[NumDEMIXtestDEM] := Table.GetFieldByNameAsString('DEM_NAME');
             DEMIXshort[NumDEMIXtestDEM] := Table.GetFieldByNameAsString('SHORT_NAME');
             DEMIXDEMcolors[NumDEMIXtestDEM] := Table.PlatformColorFromTable;
            //NotRetiredDEMs[NumDEMIXtestDEM] := Table.GetFieldByNameAsString('RETIRED') = 'N';
             {$If Defined(LoadDEMIXNames)} WriteLineToDebugFile(IntToStr(NumDEMIXtestDEM) + '--' + DEMIXshort[NumDEMIXtestDEM] + '--' + DEMIXDEMTypeName[NumDEMIXtestDEM]); {$EndIf}
          end;
          Table.Next;
       end;
       Table.Destroy;
       {$If Defined(RecordDEMIXStart) or Defined(LoadDEMIXNames)} WriteLineToDebugFile('LoadDEMIXnames out, NumDEMIXtestDEM=' + IntToStr(NumDEMIXtestDEM)); {$EndIf}
    end
    else begin
      {$If Defined(RecordDEMIXStart) or Defined(LoadDEMIXNames)} WriteLineToDebugFile('LoadDEMIXnames missing=' + fName); {$EndIf}
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
         HalfSec := ReinterpolateLatLongDEM(OpenMap,DEM,Spacing,fName);
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



function DEMIXTestDEMLegend(Horizontal : boolean = true; DEMsUsed : tstringList = nil; MaxWide : integer = 9999) : tMyBitmap;
var
   i,Left,Top,StartFontSize : integer;

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
   StartFontSize := MDDef.DEMIXlegendFontSize;
   Result := Nil;
   repeat
      if (Result <> Nil) then Result.Destroy;
      CreateBitmap(Result,1500,900);
      LoadMyFontIntoWindowsFont(MDDef.LegendFont,Result.Canvas.Font);
      Result.Canvas.Font.Size := StartFontSize;
      Left := 25;
      Top := 10;
      if (DEMsUsed <> Nil) then begin
         for i := 0 to pred(DEMsUsed.Count) do begin
            AnEntry(DEMsUsed.Strings[i]);
         end;
      end
      else begin
         for i := 1 to NumDEMIXtestDEM do begin
            //if NotRetiredDEMs[i] or MDDef.DEMIX_graph_Retired_DEMs then begin
               AnEntry(DEMIXShort[i]);
            //end;
         end;
      end;
      PutBitmapInBox(Result);
      Dec(StartFontSize);
   until Result.Width < MaxWide;
end;


procedure SetDirtAirballBackground(var Result : tThisBaseGraph; DEMType : shortstring);
begin
    if (DEMtype = 'DSM') then Result.GraphDraw.GraphBackgroundColor := RGB(219,236,237)
    else Result.GraphDraw.GraphBackgroundColor := RGB(237,237,221);
end;


function PickWineContestDBLocation : boolean;
begin
   if ValidPath(DEMIX_Base_DB_Path) then Result := true
   else Result := FindPath('DEMIX Wine contest location',':\Wine_contest\',DEMIX_Base_DB_Path);
end;


procedure SetParamsForDEMIXmode;
begin
   {$If Defined(RecordDEMIXStart) or Defined(RecordDEMIXversion)} WriteLineToDebugFile('SetParamsForDEMIXmode in, DEMIX_mode=' + IntToStr(MDDef.DEMIX_mode)); {$EndIf}

   //these are needed for inventories
      DEMIX_delta_dtms := DEMIX_Base_DB_Path + 'U10_delta_test_dems\';
      DEMIX_diluvium_dtms := DEMIX_Base_DB_Path + 'U80_diluvium_test_dems\';
      DEMIX_coastal_dtms := DEMIX_Base_DB_Path + 'U120_coastal_test_dems\';

   if not MDDef.DEMIX_AllowCoastal then MDDef.DEMIX_mode := dmFull;

   if (MDDef.DEMIX_mode = dmFull) then begin
      NumPtDEMs := 6;
      NumAreaDEMs := 1;
      AreaListFName := DEMIXSettingsDir + 'areas_list.txt';
      DEMIXModeName := 'FULL';
      DEMIX_Under_ref_dtm := '';
      DEMIX_Under_test_dems := '';
   end
   else begin
     if (MDDef.DEMIX_mode = dmU120) then begin
        NumPtDEMs := 6;
        NumAreaDEMs := 2; //adds coastal
        AreaListFName := DEMIXSettingsDir + 'areas_coastal.txt';
        DEMIXModeName := 'U120';
     end
     else if (MDDef.DEMIX_mode = dmU80) then begin
        NumPtDEMs := 6;
        NumAreaDEMs := 3; //adds coastal, dilumium
        AreaListFName := DEMIXSettingsDir + 'areas_diluvium.txt';
        DEMIXModeName := 'U80';
     end
     else if (MDDef.DEMIX_mode = dmU10) then begin
        NumPtDEMs := 7;    //adds delta
        NumAreaDEMs := 3;  //adds coastal, diluvium
        AreaListFName := DEMIXSettingsDir + 'areas_delta.txt';
        DEMIXModeName := 'U10';
     end;
     DEMIX_Under_ref_dtm := DEMIX_Base_DB_Path + DEMIXModeName + '_coastal_ref_dtms\';
     DEMIX_Under_test_dems := DEMIX_Base_DB_Path + DEMIXModeName + '_coastal_test_dems\';
   end;
   NumDEMIXtestDEM := NumPtDEMs + NumAreaDEMs;

   //DEMListFName := DEMIXSettingsDir + 'dems_' + DEMIXModeName + '.txt';
   if DEMIXanalysismode in [DEMIXtraditional] then begin
      Diff_dist_results_dir := DEMIX_Base_DB_Path + DEMIXModeName + '_diff_dist_results\';
      SSIMresultsDir := DEMIX_Base_DB_Path + DEMIXModeName + '_ssim_results\';
      FUVresultsDir := DEMIX_Base_DB_Path + DEMIXModeName + '_fuv_results\';
   end
   else if DEMIXanalysismode in [DEMIXneo] then begin
      Diff_dist_results_dir := 'J:\demix_neo\diff_dists\';
      SSIMresultsDir := 'J:\demix_neo\ssim\';
      FUVresultsDir := 'J:\demix_neo\fuv\';
   end
   else MessageToContinue('Undefined DEMIXanalysismode ' + IntToStr(DEMIXanalysismode));

   ChannelMissesDir := DEMIX_Base_DB_Path + DEMIXModeName + '_channel_misses\';
   GeomorphonsDir := DEMIX_Base_DB_Path + DEMIXModeName + '_geomorphons\';

   //MDDef.DEMIX_Tile_Full := 25;
   {$If Defined(RecordDEMIXStart) or Defined(RecordDEMIXversion)} WriteLineToDebugFile('SetParamsForDEMIXmode out, DEMIX_mode=' + IntToStr(MDDef.DEMIX_mode) + ' ' + DEMIXModeName); {$EndIf}
end;


procedure RecognizeDEMIXVersion(DB : integer);
begin
   if ValidDB(DB) then begin
      {$IfDef RecordDEMIXversion} WriteLineToDebugFile('RecognizeDEMIXVersion in, ' + GISdb[db].dbName + ' ' + IntToStr(DB)); {$EndIf}
      if MDdef.DEMIX_AllowCoastal then begin
          if StrUtils.ANSIcontainsText(UpperCase(GISDB[db].DBName),'DEMIX') then begin
             if StrUtils.ANSIcontainsText(UpperCase(GISDB[db].DBName),'FULL') then begin
                MDDef.DEMIX_mode := dmFull;
             end
             else if StrUtils.ANSIcontainsText(UpperCase(GISDB[db].DBName),'U120') then begin
                MDDef.DEMIX_mode := dmU120;
             end
             else if StrUtils.ANSIcontainsText(UpperCase(GISDB[db].DBName),'U80') then begin
                MDDef.DEMIX_mode := dmU80;
             end
             else if StrUtils.ANSIcontainsText(UpperCase(GISDB[db].DBName),'U10') then begin
                MDDef.DEMIX_mode := dmU10;
             end;
             {$IfDef RecordDEMIXversion} WriteLineToDebugFile('RecognizeDEMIXVersion parsed, ' + GISdb[db].dbName + ' ' + IntToStr(MDDef.DEMIX_mode)); {$EndIf}
          end
          else begin
             MDDef.DEMIX_mode := dmFull;
             DEMIXModeName := 'FULL';
             if GISDB[db].MyData.FieldExists('COAST') then begin
                MDDef.DEMIX_mode := dmU120;
             end
             else if GISDB[db].MyData.FieldExists('DELTA') then begin
                MDDef.DEMIX_mode := dmU10;
             end
             else if GISDB[db].MyData.FieldExists('DILUV') then begin
                MDDef.DEMIX_mode := dmU80;
             end;
          end;
      end
      else MDDef.DEMIX_mode := dmFull;

      if ANSIContainsText(UpperCase(GISdb[db].DBname),'DIFF_DIST') then CriteriaFamily := 'Difference Distribution'
      else if ANSIContainsText(UpperCase(GISdb[db].DBname),'FUV_') then CriteriaFamily := 'FUV'
      else if ANSIContainsText(UpperCase(GISdb[db].DBname),'PT_CLASS_') then CriteriaFamily := 'Raster Classification'
      else if ANSIContainsText(UpperCase(GISdb[db].DBname),'CHANNEL_') then CriteriaFamily := 'Vector Mismatch';

      SetParamsForDEMIXmode;
      {$IfDef RecordDEMIXversion} WriteLineToDebugFile('RecognizeDEMIXVersion out, ' + GISdb[db].dbName + ' ' + DEMIXModeName + '  ' + CriteriaFamily); {$EndIf}
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
   wmdem.ClearStatusBarPanelText;
   {$If Defined(RecordDEMIXStart)} WriteLineToDebugFile('Wine contest location done'); {$EndIf}

   if StartProcessing then begin
      HeavyDutyProcessing := true;
      DEMIXProcessing := true;
      SetColorForProcessing;
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
      //MDDef.AutoMergeStartDEM := true;
      MDdef.DefaultMapXSize := 800;
      MDdef.DefaultMapYSize := 800;
      MDDef.TitleLabelFont.Size := 24;
      MDDef.LegendFont.Size := 20;
   {$If Defined(RecordDEMIXStart)} WriteLineToDebugFile('GetDEMIXpaths point 2'); {$EndIf}

   DEMIXSettingsDir := ProgramRootDir + 'demix_settings\';
   DEMIX_area_dbName := DEMIXSettingsDir + 'demix_test_areas_v3.dbf';
   DEMIX_criteria_dbName := DEMIXSettingsDir + 'demix_criteria.dbf';

   DEMIX_Ref_Merge := DEMIX_Base_DB_Path + 'wine_contest_v2_ref_merge\';
   DEMIX_Ref_Half_sec := DEMIX_Base_DB_Path + 'wine_contest_v2_ref_0.5sec\';
   DEMIX_diff_dist  := DEMIX_Base_DB_Path + 'wine_contest_v2_diff_dist\';
   DEMIX_profile_test_dir := DEMIX_Base_DB_Path + 'wine_contest_v2_topo_profiles\';
   DEMIX_distrib_graph_dir := DEMIX_Base_DB_Path + 'wine_contest_v2_difference_distrib_graphs\';
   DEMIX_3DEP_Dir := DEMIX_Base_DB_Path + 'wine_contest_v2_3dep\';

   //all_difference_results_dir := DEMIX_Base_DB_Path + 'all_difference_tile_stats\';
   DEMIX_diff_maps_dir  := DEMIX_Base_DB_Path + 'wine_contest_difference_maps\';
   DEMIX_GIS_dbName := DEMIX_Base_DB_Path + 'wine_contest_database\demix_gis_db_v2.5.dbf';

   {$IfDef DEMIX_SAGA_channels}
      DEMIX_test_DEMs_no_sink := DEMIX_Base_DB_Path + 'area_test_dems_no_sink\';
      DEMIX_ref_DEMs_no_sink := DEMIX_Base_DB_Path + 'area_ref_dems_no_sink\';
      DEMIX_test_DEMs_channels := DEMIX_Base_DB_Path + 'area_test_dems_channels\';
      DEMIX_ref_DEMs_channels := DEMIX_Base_DB_Path + 'area_ref_dems_channels\';
      DEMIX_test_DEMs_channel_grids := DEMIX_Base_DB_Path + 'area_test_dems_channel_grids\';
      DEMIX_ref_DEMs_channel_grids := DEMIX_Base_DB_Path + 'area_ref_dems_channel_grids\';
   {$EndIf}

   MD_out_ref_dir := DEMIX_Base_DB_Path + 'md_out_ref_areas\';
   MD_out_test_dir := DEMIX_Base_DB_Path + 'md_out_test_areas\';
   wbt_out_ref_dir  := DEMIX_Base_DB_Path + 'wbt_out_ref_areas\';
   wbt_out_test_dir := DEMIX_Base_DB_Path + 'wbt_out_test_areas\';
   saga_out_ref_dir := DEMIX_Base_DB_Path + 'saga_out_ref_areas\';
   saga_out_test_dir := DEMIX_Base_DB_Path + 'saga_out_test_areas\';

   SafeMakeDir(MD_out_ref_dir);
   SafeMakeDir(MD_out_test_dir);
   SafeMakeDir(wbt_out_ref_dir);
   SafeMakeDir(wbt_out_test_dir);
   SafeMakeDir(saga_out_ref_dir);
   SafeMakeDir(saga_out_test_dir);

   DEMIX_final_DB_dir := DEMIX_Base_DB_Path + 'wine_contest_database\';
   SafeMakeDir(DEMIX_final_DB_dir);

   DEMIX_area_lc100  := DEMIX_Base_DB_Path + 'wine_contest_lc100\';
   DEMIX_Ref_1sec := DEMIX_Base_DB_Path + 'wine_contest_ref_1sec\';
   DEMIX_Ref_dsm_1sec := DEMIX_Base_DB_Path + 'wine_contest_ref_dsm_1sec\';
   DEMIX_test_dems := DEMIX_Base_DB_Path + 'wine_contest_test_dems\';
   SafeMakeDir(DEMIX_area_lc100);
   SafeMakeDir(DEMIX_Ref_1sec);
   SafeMakeDir(DEMIX_Ref_dsm_1sec);
   SafeMakeDir(DEMIX_test_dems);

   Stream_valley_dir := DEMIX_Base_DB_Path + 'full_valleys_ridges\';


   {$If Defined(RecordDEMIXStart)} WriteLineToDebugFile('GetDEMIXpaths point 4'); {$EndIf}

   Geoid2008FName := 'g:\geoid\egm2008-1-vdatum.tif';
   FindDriveWithFile(Geoid2008FName);
   GeoidDiffFName := 'g:\geoid\egm96_to_egm2008.tif';
   FindDriveWithFile(GeoidDiffFName);

   //MDDef.CurveCompute.AlgorithmName := smLSQ;
   ///MDDef.CurveCompute.RequireFullWindow := true;
   //MDDef.CurveCompute.LSQorder := 2;
   //MDDef.CurveCompute.UsePoints := useAll;
   //MDDef.CurveCompute.WindowRadius := 1;
   SetCurvatureDefaults;
   MDDef.EvansApproximationAllowed := false;

   SetParamsForDEMIXmode;
   LoadDEMIXnames;
   OrderedFUVParams := OpenFUVOrderedParams;

   {$If Defined(RecordDEMIXStart) or Defined(RecordDEMIX)}
      WriteLineToDebugFile('GetDEMIXpaths out, DEMIX_mode=' + IntToStr(MDDef.DEMIX_mode) + ' ' + DEMIXModeName);
   {$EndIf}
end;


procedure EndDEMIXProcessing(db : integer = 0; CleanTempDir : boolean = false);
begin
   if HeavyDutyProcessing and CleanTempDir then begin
      CleanUpTempDirectory(false);
   end;
   HeavyDutyProcessing := false;
   ReportErrors := true;
   DEMIXProcessing := false;
   ToggleShowProgress(true);
   SetColorForWaiting;
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
   if CanLimitAreas then begin
      case ModeForAreaSelection of
         1 : begin
                Result.LoadFromFile(fName);
             end;
         2 : begin
                Result.LoadFromFile(fName);
                MultiSelectSingleColumnStringList(DEMIXModeName + ' Areas to process',PickedNum,Result,true,true);
             end;
         3 : begin
               fName := DEMIXSettingsDir;
               if GetExistingFileName('DEMIX areas','area*.txt',fName) then begin
                  Result.LoadFromFile(fName);
               end;
             end;
      end;
   end
   else begin
      Result.LoadFromFile(fName);
   end;


(*

   fName := AreaListFName;
   if FileExists(fName) or GetExistingFileName('DEMIX areas','*.txt',fName) then begin
      Result.LoadFromFile(fName);
      if CanLimitAreas then MultiSelectSingleColumnStringList(DEMIX_mode_abbreviation(MDDef.DEMIX_mode) + ' Areas to process',PickedNum,Result,true,true);
   end;
*)
end;


function GetCountryForArea(Area : shortString) : shortstring;
var
   Table : tMyData;
   //Tile  : shortstring;
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
   if ValidDB(DEMIX_DB) then begin
      GetDEMIXpaths(false,DEMIX_DB);
      GISdb[DEMIX_DB].LayerIsOn := false;
      DoDEMIXFilter(DEMIX_DB);
   end;
end;


function CriterionTieTolerance(Criterion : shortstring) : float32;
var
   TieToleranceTable : tMyData;
begin
   TieToleranceTable := tMyData.Create(MDDef.DEMIX_criterion_fName);
   TieToleranceTable.ApplyFilter('CRITERION=' + QuotedStr(Criterion));
   Result := TieToleranceTable.GetFieldByNameAsFloat('TOLERANCE');
   TieToleranceTable.Destroy;
end;


function GetWinner(dbOnTable : integer; DEM1,DEM2 : shortstring) : shortstring;
var
   eval1,eval2,tolerance : float32;
begin
   eval1 := GISdb[dbOnTable].MyData.GetFieldByNameAsFloat(DEM1);
   eval2 := GISdb[dbOnTable].MyData.GetFieldByNameAsFloat(DEM2);
   tolerance := GISdb[dbOnTable].MyData.GetFieldByNameAsFloat('TOLERANCE');
   if (eval1 + Tolerance < Eval2) then Result := DEM1
   else if (eval2 + Tolerance < Eval1) then Result := DEM2
   else Result := 'TIE';
end;



procedure CompareSeriousCompetitors(DBonTable : integer);


   procedure OnePair(DEM1,DEM2 : shortstring);
   var
      tStr,fName,Criterion : shortstring;
      j : integer;
   begin
      if GISdb[dbOnTable].MyData.FieldExists(DEM1) and GISdb[dbOnTable].MyData.FieldExists(DEM2) then begin
         fName := DEM1 + '_' + DEM2;
         GISdb[dbOnTable].AddFieldToDataBase(ftstring,fName,12);
         GISdb[dbOnTable].MyData.First;
         GISdb[dbOnTable].EmpSource.Enabled := false;
         j := 0;
         while not GISdb[dbOnTable].MyData.eof do begin
            if (j mod 25 = 0) then wmdem.SetPanelText(3,fName + '  ' + IntToStr(j) + '/' + IntToStr(GISdb[DBonTable].MyData.FiltRecsInDB),true);
            inc(j);
            Criterion := GISdb[dbOnTable].MyData.GetFieldByNameAsString('CRITERION');
            if IsDEMIX_signedCriterion(Criterion) then begin
               tStr := 'N/A';
            end
            else begin
               TStr := GetWinner(dbOnTable,DEM1,DEM2);
            end;
            GISdb[dbOnTable].MyData.Edit;
            GISdb[dbOnTable].MyData.SetFieldByNameAsString(fName,tStr);
            GISdb[dbOnTable].MyData.Next;
         end;
      end;
   end;

begin
   try
      ShowHourGlassCursor;
      OnePair('COP','ALOS');
      OnePair('COP','FABDEM');
      OnePair('COP','TANDEM');
      OnePair('COP','NASA');
      OnePair('COP','SRTM');
      OnePair('COP','ASTER');
      OnePair('COP','DILUV');
      OnePair('COP','DELTA');
      OnePair('COP','COAST');
   finally
      ShowDefaultCursor;
      GISdb[dbOnTable].EmpSource.Enabled := true;
      wmdem.SetPanelText(3,'',true);
   end;
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
      //var
         //I : integer;
      begin
         (*
         for I := 1 to Graph.GraphDraw.LegendList.Count do begin
            Graph.GraphDraw.FileColors256[i] := DEMIXColorFromDEMName(Graph.GraphDraw.LegendList[pred(i)]);
         end;
         *)
         Graph.RedrawDiagram11Click(Nil);
         Graph.Image1.Canvas.Draw(Graph.GraphDraw.LeftMargin+15,Graph.GraphDraw.TopMargin+10,Graph.MakeLegend);
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




(*
procedure FUVforMultipleTestDEMstoReference;
var
   DataDir, fName : PathStr;
   RefDEM,i,j,k,ad,Test,Ref2,Test2,Ref3,Test3,OpenBoxSize : integer;
   Area,aLine,DEMIX_Tile : shortstring;
   DEMNames,
   Diff_Dist_Findings,FUV_Findings,DEMfiles : tStringList;
   Values : ^bfarray32;


   procedure AddFUV(Criterion : shortstring; Ref,Test : integer);
   var
      DiffMap : integer;
      MomentVar : tMomentVar;
      NPts : int64;
      FUV : float32;
      k : integer;
      TStr : shortstring;
      Min,Max : float32;
   begin
      if (FUV_Findings <> Nil) then begin
         FUV := GetFUVForPairGrids(DEMglb[Ref].FullDEMGridLimits,Ref,Test);
         TStr := RealToString(FUV,-12,-6);
         aline := aLine + ',' + TStr;
         {$IfDef RecordRangeScalesFull} WriteLineToDebugFile('Ref=' + DEMglb[Ref].AreaName + ' Test=' + DEMglb[Test].AreaName + ' FUV=' + TStr); {$EndIf}
      end;

      if (Diff_Dist_Findings <> Nil) then begin
        DiffMap := MakeDifferenceMap(Test,Ref,Ref,0,false,false,false);
        InitializeMomentVar(MomentVar);
        DEMglb[DiffMap].GetElevationsInLongArray(DEMglb[DiffMap].FullDEMGridLimits, MomentVar.NPts,Values^,Min,Max);
        moment(Values^,MomentVar,msAll);
        TStr := AREA + ',' + DEMIX_TILE + ',' + DEMglb[Test].AreaName + ',' + CRITERION + ',' + DEMIXMomentStatsString(MomentVar);
        Diff_Dist_Findings.Add(TStr);
        CloseSingleDEM(DiffMap);
      end;
   end;

begin {procedure FUVforMultipleTestDEMstoReference}
   SetColorForProcessing;
   New(Values);
   Diff_Dist_Findings := Nil;
   FUV_Findings := Nil;

   DEMIX_TILE := 'None';
   {$IfDef RecordRangeScales} WriteLineToDebugFile('procedure FUVforMultipleTestDEMstoReference in, ' + DataDir); {$EndIf}
   DataDir := '';
   DEMNames := GetFileNamesOfDEMinUse(DataDir);
   Area := LastSubDir(DataDir);

     FUV_Findings := tStringList.Create;
     aLine := 'AREA,DEMIX_TILE,DEM';
     for j := 0 to pred(OrderedFUVParams.Count) do aline := aline + ',' + OrderedFUVParams[j] + '_FUV';
     FUV_Findings.Add(aLine);

     Diff_Dist_Findings := tStringList.Create;
     aLine := 'AREA,DEMIX_TILE,DEM,CRITERION';
     for j := 1 to 10 do aline := aline + ',' + LongParamSuffixes[j];
     Diff_Dist_Findings.Add(aLine);

     {$IfDef RecordRangeScales} HighlightLineToDebugFile('FUVforMultipleTestDEMstoReference, start refDEM=' + DEMNames[0]); {$EndIf}
     RefDEM := OpenNewDEM(DEMnames[0],false);
     if ValidDEM(RefDEM) then begin
        for i := 1 to pred(DEMNames.Count) do begin
            {$IfDef RecordRangeScalesFull} HighlightLineToDebugFile('Start test ' + DEMNames[i]); {$EndIf}
            wmDEM.SetPanelText(1,IntToStr(i) + '/' + IntToStr(DEMNames.Count) + ' ' +   DEMNames[i],true);
            if DEMnames[i] <> 'mia' then begin
              Test := OpenNewDEM(DEMnames[i],false);
              aline := Area + ',' + DEMIX_Tile + ',' + DEMglb[Test].AreaName;
              for j := 0 to pred(OrderedFUVParams.Count) do begin
                 {$IfDef RecordRangeScalesFull} WriteLineToDebugFile('Start ' + OrderedFUVParams[j]); {$EndIf}
                 wmDEM.SetPanelText(2,OrderedFUVParams[j],true);

                 if (OrderedFUVParams[j] = 'ELEV') then begin
                    AddFUV('ELEV',RefDEM,Test);
                 end
                 else if (OrderedFUVParams[j] = 'OPENU') then begin  //does both openness
                     ad := 0;        //difference, not to be computed
                     Ref2 := -1;     //downward
                     Ref3 := -1;     //upwardward
                     CreateOpennessMap(false,DEMglb[RefDEM].FullDEMGridLimits,RefDEM,-99,3,Ref3,Ref2,ad);
                     Test2 := -1;    //downward
                     Test3 := -1;    //upward
                     CreateOpennessMap(false,DEMglb[Test].FullDEMGridLimits,Test,-99,3,Test3,Test2,ad);
                     AddFUV('OPENU',Ref3,Test3);
                     {$If SaveUpwardOpenness}
                        DEMGlb[Ref3].SaveAsGeotiff('c:\temp\' + DEMGlb[Ref3].AreaName + '_up_open_maketable.tif');
                        DEMGlb[Test3].SaveAsGeotiff('c:\temp\' + DEMGlb[Test3].AreaName + '_up_open_maketable.tif');
                     {$EndIf}
                     AddFUV('OPEND',Ref2,Test2);
                 end
                 else if (OrderedFUVParams[j] = 'HILL') then begin
                    Ref2 := CreateHillshadeMap(false,RefDEM);
                    Test2 := CreateHillshadeMap(false,Test);
                    AddFUV('HILL',Ref2,Test2);
                 end
                 else if (OrderedFUVParams[j] = 'SLOPE') then begin //also does roughness
                     Ref2 := 0;
                     Test2 := 0;
                     Ref3 := CreateSlopeRoughnessSlopeStandardDeviationMap(RefDEM,5,Ref2,false);
                     Test3 := CreateSlopeRoughnessSlopeStandardDeviationMap(Test,5,Test2,false);
                     addFuv('SLOPE',Ref2,Test2);
                     addFuv('RUFF',Ref3,Test3);
                 end
                 else if (OrderedFUVParams[j] = 'TPI') then begin
                    Ref2 := MakeTPIGrid(RefDEM,nmRRI,false);
                    Test2 := MakeTPIGrid(Test,nmRRI,false);
                    addFuv('TPI',Ref2,Test2);
                 end
                 else if (OrderedFUVParams[j] = 'RRI') then begin
                     Ref2 := MakeTRIGrid(RefDEM,nmRRI,false);
                     Test2 := MakeTRIGrid(Test,nmRRI,false);
                     addFuv('RRI',Ref2,Test2);
                 end
                 else if (OrderedFUVParams[j] = 'PLANC') then begin
                     if MICRODEMcurvature then begin
                       Ref2 := CreateCurvatureMap(eucurv_plan,false,RefDEM);
                       Test2 := CreateCurvatureMap(eucurv_plan,false,Test);
                     end
                     else begin
                       Ref2 := WBT_PlanCurvature(false,DEMGlb[refDEM].GeotiffDEMName,MDtempDir + 'wbt_plan_curv.tif');
                       Test2 := WBT_PlanCurvature(false,DEMGlb[test].GeotiffDEMName,MDtempDir + 'wbt_plan_curv.tif');
                     end;
                     addFuv('PLANC',Ref2,Test2);
                 end
                 else if (OrderedFUVParams[j] = 'PROFC') then begin
                     if MICRODEMcurvature then begin
                       Ref2 := CreateCurvatureMap(eucurv_prof,false,refDEM);
                       Test2 := CreateCurvatureMap(eucurv_prof,false,Test);
                     end
                     else begin
                       Ref2 := WBT_ProfileCurvature(false,DEMGlb[refDEM].GeotiffDEMName,MDtempDir + 'wbt_prof_curv.tif');
                       Test2 := WBT_ProfileCurvature(false,DEMGlb[test].GeotiffDEMName,MDtempDir + 'wbt_prof_curv.tif');
                     end;
                     addFuv('PROFC',Ref2,Test2);
                 end
                 else if (OrderedFUVParams[j] = 'TANGC') then begin
                     if MICRODEMcurvature then begin
                       Ref2 := CreateCurvatureMap(eucurv_tang,false,refDEM);
                       Test2 := CreateCurvatureMap(eucurv_tang,false,Test);
                     end
                     else begin
                       Ref2 := WBT_TangentialCurvature(false,DEMGlb[refDEM].GeotiffDEMName,MDtempDir + 'wbt_tang_curv.tif');
                       Test2 := WBT_TangentialCurvature(false,DEMGlb[test].GeotiffDEMName,MDtempDir + 'wbt_tang_curv.tif');
                     end;
                     addFuv('TANGC',Ref2,Test2);
                 end;
                 CloseSingleDEM(Ref2);
                 CloseSingleDEM(Test2);
                 CloseSingleDEM(Ref3);
                 CloseSingleDEM(Test3);
              end;
              FUV_Findings.Add(Aline);
              {$IfDef RecordRangeScales} WriteLineToDebugFile(aLine); {$EndIf}
              CloseSingleDEM(Test);
            end;
        end;
        CloseSingleDEM(RefDEM);
        if (FUV_Findings <> nil) then begin
           fName := DataDir + Area + '_FUV' + '.dbf';
           StringList2CSVtoDB(FUV_Findings,fName,true);
           {$IfDef RecordRangeScales} WriteLineToDebugFile('FUVforScales_0_15s out, created ' + fName); {$EndIf}
        end;
        if (Diff_Dist_Findings <> nil) then begin
           fName := DataDir + Area + '_diff_dist' + '.dbf';
           StringList2CSVtoDB(Diff_dist_Findings,fName,true);
           {$IfDef RecordRangeScales} WriteLineToDebugFile('FUVforScales_0_15s out, created ' + fName); {$EndIf}
        end;
      Dispose(Values);
      wmDEM.ClearStatusBarPanelText;
      SetColorForWaiting;
   end;
end {procedure FUVforMultipleTestDEMstoReference};
*)


