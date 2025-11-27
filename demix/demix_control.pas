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
   //{$Define RecordDEMIXStart}
   //{$Define LoadDEMIXNames}
   //{$Define RecordDEMIXopenGrids}
   //{$Define RecordTestDEMs}
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
    StrUtils,
    dbGrids,
    VCL.ExtCtrls,VCL.Forms, VCL.Graphics, VCL.Controls,
    WinAPI.Windows,
    Petmar,Petmar_types,BaseGraf,
    DEMDefs,
    DEMIX_definitions;

const
   DEMIX_initialized : boolean = false;

   function GetDEMIXpaths(StartProcessing : boolean = true; DB : integer = 0) : boolean;
   procedure EndDEMIXProcessing(db : integer = 0; CleanTempDir : boolean = false);
   procedure LoadDEMIXnames(RestrictList : tStringList = nil);
   function GetListOfTestDEMsinUse(GeometricModel : shortstring = '') : tStringList;
   function GetTestDEMLongName(ShortName : shortstring) : shortstring;


   function DEMIXColorFromDEMName(DEMName : shortstring) : tPlatformColor;
   function DEMIXColorForCriterion(Criterion : shortstring; fName : PathStr = '') : tColor;
   function WhatTestDEMisThis(fName : PathStr) : shortstring;
   function IsDEMaDSMorDTM(DEMName : ShortString) : integer;
   function DEMIXTestDEMLegend(DEMsUsed : tstringList; Horizontal : boolean = true; MaxWide : integer = 9999) : tMyBitmap;
   function FilterForDEMIXtilesToUse : shortstring;
   function FilterForDEMIXtilesToAvoid : shortstring;

   function DEMIX_GetListOfAreas : tStringList;

//service functions and procedures
   function OpenBothPixelIsDEMs(Area,Prefix : shortstring; RefDir,TestDir : PathStr; OpenMaps : boolean) : boolean;
   function LoadDEMIXReferenceDEMs(AreaName : shortstring; var RefDEM : integer; OpenMaps : boolean = true) : boolean;
   function LoadDEMIXCandidateDEMs(AreaName : ShortString;  OpenMaps : boolean = false; AllCandidates : boolean = true) : boolean;
   procedure LoadThisDEMIXTile(AreaName,TileName : shortstring; OpenMaps : boolean = true);
   procedure LoadCopAndLancoverForDEMIXTile(AreaName : shortstring; TileName : shortstring = '');

   procedure GetReferenceDEMsForTestDEM(TestSeries : shortstring; var UseDSM,UseDTM : integer);
   function GetReferenceDEMforTestDEM(ThisTestDEM : integer; RefDEMs : tDEMIXindexes) : integer;
   function DEMIX_AreasWanted(CanLimitAreas : boolean = true) : tStringList;

   function GetAreaNameForDEMIXTile(DB : integer; DemixTile : shortstring) : shortstring;
   function AreaNameFromRefDEMName(fName : PathStr) : shortstring;
   function RemoveDTMorDSMfromAreaName(fName : PathStr) : shortstring;

   procedure FilterInSignedCriteria(DBonTable : integer);
   function CreateFilterOutSignedCriteria(DBonTable : integer) : shortstring;
   procedure FilterOutSignedCriteria(DBonTable : integer);

   function SymbolFromDEMName(DEMName : shortstring) : tFullSymbolDeclaration;

   procedure RecognizeDEMIXVersion(DB : integer);

   function GetCountryForArea(Area : shortString) : shortstring;
   procedure SummarizeEGM96toEGM2008shifts;
   procedure SetDirtAirballBackground(var Result : tThisBaseGraph; DEMType : shortstring);   //brown dirtball for STM, blue airball for DSM

procedure MergeDEMIXtileStats;


//rankings, winners, tolerances
    procedure CompareRankings(DBonTable : integer);
    procedure DifferentRankingsByCriteria(DBonTable : integer);
    function GetWinnerBetweenTwoDEMs(dbOnTable : integer; DEM1,DEM2 : shortstring; Tolerance : float32) : shortstring;
    procedure DifferentRankingsByTile(DBonTable : integer);
    function WinnerAndTies(DB : integer; DEMs : tStringList; Tolerance : float32) : shortstring;
    function CriterionTieTolerance(Criterion : shortstring) : float32;


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

{$IfDef IncludeEarlyDEMIXmaps}
    //demix maps
       function AirBallDirtBallMap(DEMonMap,DSM,DTM : integer; fName : PathStr = '') : integer;
       function TwoDEMHighLowMap(RefDEM,ALOS,COP : integer; SimpleTolerance : float32; FourCats : boolean; fName2 : PathStr; ShowMap : boolean = true) : integer;
       function BestCopOrALOSmap(RefDEM,ALOS,Cop : integer; Tolerance : float32; AName : shortString) : integer;
       function RGBBestOfThreeMap(RefDEM,ALOS,Cop,Fab,Merge : integer; Tolerance : float32; AName : shortString) : integer;
       procedure NumHighLowNeighborsMaps(DEM,Radius : integer; Tolerance : float32; var HighNeigh,LowNeigh : integer);
{$EndIf}

{$IfDef FUV_RangeScales}
   procedure FUVforRangeScales(LandCoverOption : boolean);
{$EndIf}

function DEMIXMomentStatsString(MomentVar : tMomentVar) : shortstring;
function DEMIXShortenDEMName(DEMName : shortstring) : shortstring;

   {$IfDef OpenDEMIXAreaAndCompare}
      procedure OpenDEMIXArea(fName : PathStr = '');
   {$EndIf}

   {$IfDef OldDEMIXroutines}
      procedure TransposeDEMIXwinecontestGraph(DBonTable : integer);
   {$EndIf}


var
   ElevDiffHists : boolean;
   FUVMode : integer;


const
   MaxOrderedParams = 20;
   NumScales = 4;
   Scales : array[1..NumScales] of shortstring = ('0.15sec','0.25sec','0.5sec','1sec');

   function GetFileNamesOfDEMinUse(var DataDir : PathStr) : tStringList;

procedure LandCoverBreakdowPointCloud;

procedure Get_DEMIX_CriteriaToleranceFName(db : integer);
function GetListDEMIXOrderedCriteria(DEMIX_criteria_tolerance_fName : PathStr) : tStringList;
procedure DEMIX_CriteriaToleranceFNameFromMode(Mode : integer);
procedure DeleteCSV_FilesforArea(AreaName : shortstring);
procedure GEDTM_problems(dbOnTable : integer);
procedure SaveGEDTMFamilyDEM(DEM1 : integer; fName1 : PathStr);


implementation

uses
   Nevadia_Main,
   DEMstat,Make_grid,PetImage,PetImage_form,new_petmar_movie,DEMdatabase,PetDButils,Pick_several_dems,
   Geotiff, BaseMap, GDAL_tools, DEMIX_filter, DEMstringgrid,DEM_NLCD,
   DEMCoord,DEMMapf,DEMDef_routines,DEM_Manager,DEM_indexes,PetMath,
   MD_use_tools,
   DEMIX_graphs,
   Pick_DEMIX_mode,
   pick_demix_areas;

var
   DoHorizontalShift : boolean;

   {$IfDef IncludeEarlyDEMIXmaps}
      {$I demix_maps.inc}
   {$EndIf}

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


procedure SaveGEDTMFamilyDEM(DEM1 : integer; fName1 : PathStr);
//the decimeters must have been fixed first
//removes Geotiff code 42112
//also saves the vertical datum
begin
    DEMGlb[DEM1].CheckMaxMinElev;
    DEMGlb[DEM1].DEMHeader.ElevUnits := euMeters;
    DEMGlb[DEM1].DEMHeader.VerticalCSTypeGeoKey := VertCSEGM2008;
    DEMGlb[DEM1].SaveAsGeotiff(fName1);
end;



procedure GEDTM_problems(dbOnTable : integer);
var
   DEM1,DEM2,i,j : integer;
   NPts : int64;
   Lat1,Long1,Lat2,Long2,Slope1,Slope2,DistanceMeters,Bearing : float64;
   Elev1,Elev2,Std,dElev,dSlope,MinMultiple,MaxMultiple : float32;
   Findings : tStringList;
   Area,Tile,zUnits,Change,aLine : shortstring;
   fName,fName1,fName2 : PathStr;
   bb : SfBoundBox;

        procedure StatsGEDTM;
        begin
          DEMglb[DEM1].DEMCenterPoint(Lat1,Long1);
          DEMglb[DEM1].ElevationStatistics(DEMglb[DEM1].FullDEMGridLimits,Elev1,Std,NPts);
          Slope1 := DEMglb[DEM1].AverageDEMslope(DEMglb[DEM1].FullDEMGridLimits);
        end;

        procedure Differences;
        begin
          dElev := Elev1 - Elev2;
          dSlope := Slope1 - Slope2;
          VincentyCalculateDistanceBearing(Lat1,Long1,Lat2,Long2,DistanceMeters,Bearing);
        end;

        function DifferenceString : shortstring;
        begin
          Result := RealToString(0.001 * DistanceMeters,-12,-4) + ',' + RealToString(dElev,-12,-2) + ',' + RealToString(dSlope,-12,-2);
        end;


 const
   GEDTMversions : array[1..4] of shortstring = ('EDTM','GEDTMv0','GEDTMv1_1','GEDTMv1_2');
 begin
   Findings := tStringList.Create;
   Findings.Add('AREA,DEMIX_TILE,GEDTM_ZS,REF_ELEV,REF_SLOPE,DIST_KM,ELEV_DIFF,SLOPE_DIFF,PROBLEM');
   GISdb[DBonTable].MyData.First;
   GISdb[DBonTable].EmpSource.Enabled := false;
   i := 0;
   WantShowProgress := false;


   Findings := tStringList.Create;
   Findings.Add('AREA,DEMIX_TILE,GEDTM_ZS,GEDTM_MAX,REF_MAX,MAX_MULT,CHANGE');
   while not GISdb[DBonTable].MyData.eof do begin
      inc(i);
      Area := GISdb[DBonTable].MyData.GetFieldByNameAsString('AREA');
      Tile := GISdb[DBonTable].MyData.GetFieldByNameAsString('DEMIX_TILE');
      wmDEM.SetPanelText(1,IntToStr(i) + '/' + IntToStr(GISdb[DBonTable].MyData.FiltRecsInDB) + '  ' + Tile,true);
      fName1 := MDDef.DEMIX_BaseDir + Area + '\' + Tile  + '_ref_test_dem\GEDTMv1_2.tif';
      fName2 := MDDef.DEMIX_BaseDir + Area + '\' + Tile +  '_ref_test_dem\ref_dtm_srtm.tif';
      if FileExists(fName1) and FileExists(fName2) then begin
          LoadNewDEM(DEM2,fName2,false);
          DEMglb[DEM2].DEMCenterPoint(Lat2,Long2);

          for j := 1 to 4 do begin
              fName1 := MDDef.DEMIX_BaseDir + Area + '\' + Tile  + '_ref_test_dem\' + GEDTMversions[j] + '.tif';

              LoadNewDEM(DEM1,fName1,false);
              DEMglb[DEM1].DEMCenterPoint(Lat1,Long1);
              zUnits := ElevUnitsAre(DEMglb[DEM1].DEMheader.ElevUnits);

              MaxMultiple := DEMGlb[DEM1].DEMheader.MaxElev / DEMGlb[DEM2].DEMheader.MaxElev;
              Change := 'None';

              if (MaxMultiple > 3) then begin
                  //this is in decimeters, but the Geotiff header does not have any units and meters were assumed when importing
                  DEMGlb[DEM1].MultiplyGridByConstant(0.1);
                  SaveGEDTMFamilyDEM(DEM1,fName1);
                  MaxMultiple := DEMGlb[DEM1].DEMheader.MaxElev / DEMGlb[DEM2].DEMheader.MaxElev;
                  Change := 'Times 0.1';
              end
              else if (MaxMultiple < 0.2) then begin
                  //this was in decimeters, but the Geotiff header does not have any units and meters were assumed when importing
                  DEMGlb[DEM1].MultiplyGridByConstant(10);
                  SaveGEDTMFamilyDEM(DEM1,fName1);
                  MaxMultiple := DEMGlb[DEM1].DEMheader.MaxElev / DEMGlb[DEM2].DEMheader.MaxElev;
                  Change := 'Times 10';
              end;
          end;

          aLine := Area + ',' + Tile + ',' + Zunits + ',' + RealToString(DEMGlb[DEM1].DEMheader.MaxElev,-8,-2) + ',' +
                        RealToString(DEMGlb[DEM2].DEMheader.MaxElev,-8,-2)+ ',' + RealToString(MaxMultiple,-8,-2) + ',' + Change;
          Findings.Add(aLine);
      end;
      CloseAllDEMs;
      GISdb[DBonTable].MyData.Next;
   end;


(*
   Findings := tStringList.Create;
   Findings.Add('AREA,DEMIX_TILE,GEDTM_ZS,REF_ELEV,REF_SLOPE,DIST_KM,ELEV_DIFF,SLOPE_DIFF,PROBLEM');
   while not GISdb[DBonTable].MyData.eof do begin
      inc(i);
      Area := GISdb[DBonTable].MyData.GetFieldByNameAsString('AREA');
      Tile := GISdb[DBonTable].MyData.GetFieldByNameAsString('DEMIX_TILE');
      wmDEM.SetPanelText(1,IntToStr(i) + '/' + IntToStr(GISdb[DBonTable].MyData.FiltRecsInDB) + '  ' + Tile,true);
      fName1 := MDDef.DEMIX_BaseDir + Area + '\' + Tile  + '_ref_test_dem\GEDTMv1_2.tif';
      fName2 := MDDef.DEMIX_BaseDir + Area + '\' + Tile +  '_ref_test_dem\ref_dtm_srtm.tif';
      if FileExists(fName1) and FileExists(fName2) then begin
          LoadNewDEM(DEM1,fName1,false);
          StatsGEDTM;
          zUnits := ElevUnitsAre(DEMglb[DEM1].DEMheader.ElevUnits);

          LoadNewDEM(DEM2,fName2,false);
          DEMglb[DEM2].DEMCenterPoint(Lat2,Long2);

          DEMglb[DEM2].ElevationStatistics(DEMglb[DEM2].FullDEMGridLimits,Elev2,Std,NPts);
          Slope2 := DEMglb[DEM2].AverageDEMslope;
          Differences;


          if (DistanceMeters > 5000) then begin
              CloseSingleDEM(DEM1);
              bb := DEMglb[2].DEMBoundBoxGeo;
              fName := 'https://s3.opengeohub.org/global/edtm/gedtm_rf_m_30m_s_20060101_20151231_go_epsg.4326.3855_v1.2.tif';
              DEM1 := GDAL_WebExtractFromMonsterTIFFforBoundingBox(fName,bb,false,fName1,fName1);
              DEMGlb[DEM1].MultiplyGridByConstant(0.1);
              DEMGlb[DEM1].SaveAsGeotiff(fName1);
              StatsGEDTM;
              Differences;
          end
          else if (abs(dElev) > 25) then begin
              //this is in decimeters, but the Geotiff header does not have any units and meters were assumed when importing
              DEMGlb[DEM1].MultiplyGridByConstant(0.1);
              DEMGlb[DEM1].CheckMaxMinElev;
              DEMGlb[DEM1].SaveAsGeotiff(fName1);
              StatsGEDTM;
              Differences;
          end;

          Findings.Add(Area + ',' + Tile + ',' + Zunits + ',' + RealToString(Elev2,-8,-2) + ',' + RealToString(Slope2,-8,-2) + ',' + DifferenceString );
      end
      else begin
          Findings.Add(Area + ',' + Tile + Zunits + ',-9999,-9999,-9999,-9999,-9999,Missing DEM');
      end;
      CloseAllDEMs;
      GISdb[DBonTable].MyData.Next;
   end;
*)

   fName := MDTempDir + 'gedtm_tile_location_problems.dbf';
   PetDBUtils.StringList2CSVtoDB(Findings,fName);
   GISdb[dbOnTable].ShowStatus;
   WantShowProgress := true;
end;



procedure DeleteCSV_FilesforArea(AreaName : shortstring);
begin
    DeleteFileIfExists(MDDef.DEMIX_BaseDir + 'aa_fuv_results\' + AreaName + '_fuv_results.csv');
    DeleteFileIfExists(MDDef.DEMIX_BaseDir + 'aa_partials_results\' + AreaName + '_fuv_partials.csv');
    DeleteFileIfExists(MDDef.DEMIX_BaseDir + 'aa_curvatures_results\' + AreaName + '_fuv_curvatures.csv');
    DeleteFileIfExists(MDDef.DEMIX_BaseDir + 'aa_diff_dist_results\' + AreaName + '_diff_dist.csv');
    DeleteFileIfExists(MDDef.DEMIX_BaseDir + 'aa_tile_stats\' + AreaName + '_tile_stats.csv');
end;



procedure DEMIX_CriteriaToleranceFNameFromMode(Mode : integer);
begin
    if Mode = fuvmMixed then DEMIX_criteria_tolerance_fName := DemixSettingsDir + 'demix_criteria_fuv.dbf';
    if Mode = fuvmPartials then DEMIX_criteria_tolerance_fName := DemixSettingsDir + 'demix_criteria_partials.dbf';
    if Mode = fuvmCurves then DEMIX_criteria_tolerance_fName := DemixSettingsDir + 'demix_criteria_curvatures.dbf';
    if Mode = fuvmDiffDist then DEMIX_criteria_tolerance_fName := DemixSettingsDir + 'demix_criteria_diff_dist.dbf';
    FUVMode := Mode;
end;


procedure Get_DEMIX_CriteriaToleranceFName(db : integer);
begin
   //if (db = 0) then DEMIX_criteria_tolerance_fName := DemixSettingsDir + 'demix_criteria_fuv.dbf';
   //if not FileExists(DEMIX_criteria_tolerance_fName) then begin
     if ValidDB(db) and AnsiContainsText(UpperCase(GISdb[db].dbName),'PARTIALS') then DEMIX_CriteriaToleranceFNameFromMode(fuvmPartials)
     else if ValidDB(db) and AnsiContainsText(UpperCase(GISdb[db].dbName),'CURVATURES') then DEMIX_CriteriaToleranceFNameFromMode(fuvmCurves)
     else if ValidDB(db) and AnsiContainsText(UpperCase(GISdb[db].dbName),'FUV') then DEMIX_CriteriaToleranceFNameFromMode(fuvmMixed)
     else if ValidDB(db) and AnsiContainsText(UpperCase(GISdb[db].dbName),'DIFF_DIST') then DEMIX_CriteriaToleranceFNameFromMode(fuvmDiffDist)
     else begin
        DEMIX_criteria_tolerance_fName := '';
        MessageToContinue('No defined file for criteria for ' + GISdb[db].dbName);
     end;
   //end;
   //if not FileExists(DEMIX_criteria_tolerance_fName) then begin
      //MessageToContinue('File for criteria missing: ' + DEMIX_criteria_tolerance_fName);
   //end;
end;

function GetListDEMIXOrderedCriteria(DEMIX_criteria_tolerance_fName : PathStr) : tStringList;
var
   Table : tMyData;
begin
   if FileExists(DEMIX_criteria_tolerance_fName) then begin
      Table := tMyData.Create(DEMIX_criteria_tolerance_fName);
      Table.ApplyFilter('USE=' + QuotedStr('Y'));
      Result := Table.ListUniqueEntriesInDB('CRITERION',false);
      Table.Destroy;
   end
   else begin
      MessageToContinue('Invalid file for criteria');
      exit;
   end;
end;


function CriterionTieTolerance(Criterion : shortstring) : float32;
var
   db : tMyData;
begin
   if FileExists(DEMIX_criteria_tolerance_fName) then begin
      db := tMyData.Create(DEMIX_criteria_tolerance_fName);
      db.ApplyFilter('CRITERION=' + QuotedStr(NoSuffixCriterion(Criterion)));
      if db.FiltRecsInDB = 1 then Result := db.GetFieldByNameAsFloat('TOLERANCE')
      else begin
         MessageToContinue('Criterion "' + Criterion + '" missing in ' + DEMIX_criteria_tolerance_fName);
      end;
      db.Destroy;
   end
   else MessageToContinue('Missing criteria/tolerance file ' + DEMIX_criteria_tolerance_fName);
end;


function GetTestDEMLongName(ShortName : shortstring) : shortstring;
var
   fName : PathStr;
   aFilter : shortstring;
   Table : tMyData;
begin
   fName := DemixSettingsDir + 'demix_dems.dbf';
   Table := tMyData.Create(fName);
   aFilter := 'SHORT_NAME=' + QuotedStr(ShortName);
   Table.ApplyFilter(aFilter);
   Result := Table.GetFieldByNameAsString('DEM_NAME');
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

function WinnerAndTies(DB : integer; DEMs : tStringList; Tolerance : float32) : shortstring;
var
   Evals : array[0..15] of float32;
   i,Besti : integer;
   Best : float32;
begin
   for I := 0 to pred(DEMs.Count) do begin
      Evals[i] := GISdb[db].MyData.GetFieldByNameAsFloat(DEMs.Strings[i]);
   end;
   Best := 999;
   for I := 0 to pred(DEMs.Count) do begin
      if Evals[i] < Best then begin
         Best := Evals[i];
         BestI := i;
      end;
   end;
   Result := DEMs.strings[BestI];
   for I := 0 to pred(DEMs.Count) do begin
      if (i <> BestI) and (Evals[i] <= Best + Tolerance) then begin
         Result := Result + '-' + DEMs.strings[I];
      end;
   end;
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
     MessageToContinue('Set to classic; must be defined');
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


function DEMIXColorForCriterion(Criterion : shortstring; fName : PathStr = '') : tColor;
var
   Table : tMyData;
begin
   if FName = '' then fName := DEMIX_criteria_tolerance_fName;  //DEMIX_criteria_dbName;
   Table := tMyData.Create(fName);
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


procedure LoadDEMIXnames(RestrictList : tStringList = nil);
var
   table : tMyData;
   fName : PathStr;
   ShortName : shortstring;
begin
    fName := DEMIXSettingsDir + 'demix_dems.dbf';
    {$If Defined(RecordDEMIXStart) or Defined(LoadDEMIXNames)} WriteLineToDebugFile('LoadDEMIXnames in ' + fName); {$EndIf}
    if FileExists(fName) then begin
       Table := tMyData.Create(fName);
       NumDEMIXtestDEM := 0;
       while not Table.eof do begin
          if (Table.GetFieldByNameAsString('USE') = 'Y') then begin
             ShortName := Table.GetFieldByNameAsString('SHORT_NAME');
             if (RestrictList = nil) or (RestrictList.IndexOf(ShortName) <> -1) then begin
                inc(NumDEMIXtestDEM);
                DEMIXDEMTypeName[NumDEMIXtestDEM] := Table.GetFieldByNameAsString('DEM_NAME');
                DEMIXshort[NumDEMIXtestDEM] := ShortName;
                DEMIXDEMcolors[NumDEMIXtestDEM] := Table.PlatformColorFromTable;
                {$If Defined(LoadDEMIXNames)} WriteLineToDebugFile(IntToStr(NumDEMIXtestDEM) + '--' + DEMIXshort[NumDEMIXtestDEM] + '--' + DEMIXDEMTypeName[NumDEMIXtestDEM]); {$EndIf}
             end;
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



function DEMIXTestDEMLegend(DEMsUsed : tstringList; Horizontal : boolean = true; MaxWide : integer = 9999) : tMyBitmap;
var
   ColorsUsed : tStringList;
   i : integer;
begin
   ColorsUsed := tStringList.Create;
   for i := 0 to pred(DEMsUsed.Count) do begin
      ColorsUsed.Add(IntToStr(ConvertPlatformColorToTColor(DEMIXColorFromDEMName(DEMsUsed[i]))));
   end;
   Result := MakeLegend(DEMsUsed,ColorsUsed,Horizontal,MaxWide);
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

   if (not MDDef.DEMIX_AllowCoastal) then MDDef.DEMIX_mode := dmFull;
   {$IfDef IncludeCoastalDEMs}
      //these are needed for inventories
         DEMIX_delta_dtms := DEMIX_Base_DB_Path + 'U10_delta_test_dems\';
         DEMIX_diluvium_dtms := DEMIX_Base_DB_Path + 'U80_diluvium_test_dems\';
         DEMIX_coastal_dtms := DEMIX_Base_DB_Path + 'U120_coastal_test_dems\';
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
    {$EndIf}


   if (MDDef.DEMIX_mode = dmFull) then begin
      NumPtDEMs := 6;
      NumAreaDEMs := 1;
      AreaListFName := DEMIXSettingsDir + 'areas_list.txt';
      DEMIXModeName := 'FULL';
      DEMIX_Under_ref_dtm := '';
      DEMIX_Under_test_dems := '';
   end;
   NumDEMIXtestDEM := NumPtDEMs + NumAreaDEMs;

   //DEMListFName := DEMIXSettingsDir + 'dems_' + DEMIXModeName + '.txt';
   if DEMIXanalysismode in [DEMIXtraditional] then begin
      Diff_dist_results_dir := DEMIX_Base_DB_Path + DEMIXModeName + '_diff_dist_results\';
      SSIMresultsDir := DEMIX_Base_DB_Path + DEMIXModeName + '_ssim_results\';
      FUVresultsDir := DEMIX_Base_DB_Path + DEMIXModeName + '_fuv_results\';
      PartialsResultsDir := DEMIX_Base_DB_Path + DEMIXModeName + '_partials_results\';
      CurvaturesResultsDir := DEMIX_Base_DB_Path + DEMIXModeName + '_curvatures_results\';
   end
   else if DEMIXanalysismode in [DEMIXneo] then begin
      Diff_dist_results_dir := 'J:\demix_neo\diff_dists\';
      SSIMresultsDir := 'J:\demix_neo\ssim\';
      FUVresultsDir := 'J:\demix_neo\fuv\';
   end
   else MessageToContinue('Undefined DEMIXanalysismode ' + IntToStr(DEMIXanalysismode));
   {$IfDef IncludeVectorCriteria}
      ChannelMissesDir := DEMIX_Base_DB_Path + DEMIXModeName + '_channel_misses\';
   {$EndIf}
   GeomorphonsDir := DEMIX_Base_DB_Path + DEMIXModeName + '_geomorphons\';

   //MDDef.DEMIX_Tile_Full := 25;
   {$If Defined(RecordDEMIXStart) or Defined(RecordDEMIXversion)} WriteLineToDebugFile('SetParamsForDEMIXmode out, DEMIX_mode=' + IntToStr(MDDef.DEMIX_mode) + ' ' + DEMIXModeName); {$EndIf}
end;


procedure RecognizeDEMIXVersion(DB : integer);
begin
   if ValidDB(DB) then begin
      {$IfDef RecordDEMIXversion} WriteLineToDebugFile('RecognizeDEMIXVersion in, ' + GISdb[db].dbName + ' ' + IntToStr(DB)); {$EndIf}
      {$IfDef IncludeCoastalDEMs}
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
      {$Else}
         MDDef.DEMIX_mode := dmFull;
      {$EndIf}

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
      LockStatusBar := true;
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
      MDdef.DefaultMapXSize := 800;
      MDdef.DefaultMapYSize := 800;
      MDDef.TitleLabelFont.Size := 24;
      MDDef.LegendFont.Size := 20;
   {$If Defined(RecordDEMIXStart)} WriteLineToDebugFile('GetDEMIXpaths point 2'); {$EndIf}

   DEMIXSettingsDir := ProgramRootDir + 'demix_settings\';
   DEMIX_area_dbName := DEMIXSettingsDir + 'demix_test_areas_v3.dbf';
   //DEMIX_criteria_dbName := DEMIXSettingsDir + 'demix_criteria.dbf';

   DEMIX_Ref_Merge := DEMIX_Base_DB_Path + 'wine_contest_v2_ref_merge\';
   DEMIX_Ref_Half_sec := DEMIX_Base_DB_Path + 'wine_contest_v2_ref_0.5sec\';
   DEMIX_diff_dist  := DEMIX_Base_DB_Path + 'wine_contest_v2_diff_dist\';
   DEMIX_profile_test_dir := DEMIX_Base_DB_Path + 'wine_contest_v2_topo_profiles\';
   DEMIX_distrib_graph_dir := DEMIX_Base_DB_Path + 'wine_contest_v2_difference_distrib_graphs\';
   DEMIX_3DEP_Dir := DEMIX_Base_DB_Path + 'wine_contest_v2_3dep\';
   DEMIX_diff_maps_dir  := DEMIX_Base_DB_Path + 'wine_contest_difference_maps\';

   {$IfDef DEMIX_SAGA_channels}
      DEMIX_test_DEMs_no_sink := DEMIX_Base_DB_Path + 'area_test_dems_no_sink\';
      DEMIX_ref_DEMs_no_sink := DEMIX_Base_DB_Path + 'area_ref_dems_no_sink\';
      DEMIX_test_DEMs_channels := DEMIX_Base_DB_Path + 'area_test_dems_channels\';
      DEMIX_ref_DEMs_channels := DEMIX_Base_DB_Path + 'area_ref_dems_channels\';
      DEMIX_test_DEMs_channel_grids := DEMIX_Base_DB_Path + 'area_test_dems_channel_grids\';
      DEMIX_ref_DEMs_channel_grids := DEMIX_Base_DB_Path + 'area_ref_dems_channel_grids\';
   {$EndIf}
   {$IfDef ExternalProgramsSaveCriteria}
       MD_out_ref_dir := DEMIX_Base_DB_Path + 'md_out_ref_areas\';
       MD_out_test_dir := DEMIX_Base_DB_Path + 'md_out_test_areas\';
       wbt_out_ref_dir  := DEMIX_Base_DB_Path + 'wbt_out_ref_areas\';
       wbt_out_test_dir := DEMIX_Base_DB_Path + 'wbt_out_test_areas\';
       saga_out_ref_dir := DEMIX_Base_DB_Path + 'saga_out_ref_areas\';
       saga_out_test_dir := DEMIX_Base_DB_Path + 'saga_out_test_areas\';
   {$EndIf}

   {$IfDef ExternalProgramsSaveCriteria}
       SafeMakeDir(MD_out_ref_dir);
       SafeMakeDir(MD_out_test_dir);
       SafeMakeDir(wbt_out_ref_dir);
       SafeMakeDir(wbt_out_test_dir);
       SafeMakeDir(saga_out_ref_dir);
       SafeMakeDir(saga_out_test_dir);
   {$EndIf}


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

   {$IfDef IncludeVectorCriteria}
      Stream_valley_dir := DEMIX_Base_DB_Path + 'full_valleys_ridges\';
   {$EndIf}

   {$If Defined(RecordDEMIXStart)} WriteLineToDebugFile('GetDEMIXpaths point 4'); {$EndIf}

   Geoid2008FName := 'g:\geoid\egm2008-1-vdatum.tif';
   FindDriveWithFile(Geoid2008FName);
   GeoidDiffFName := 'g:\geoid\egm96_to_egm2008.tif';
   FindDriveWithFile(GeoidDiffFName);

   SetCurvatureDefaults;
   MDDef.EvansApproximationAllowed := false;

   SetParamsForDEMIXmode;
   LoadDEMIXnames;

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


function DEMIX_GetListOfAreas : tStringList;
var
  i : integer;
  AreaDir,TileDir : PathStr;
begin
   Result := GetSubDirsInDirectory(MDDef.DEMIX_BaseDir);
   if (Result.Count = 0) then begin
      GetDOSPath('DEMIX base directory',MDDef.DEMIX_BaseDir);
      Result := GetSubDirsInDirectory(MDDef.DEMIX_BaseDir);
   end;
   for i := pred(Result.Count) downto 0 do begin
      if (UpperCase(Copy(Result.Strings[i],1,3)) = 'AA_') or (Result.Strings[i] = 'source') or (Result.Strings[i] = 'wgs_egm') or (Result.Strings[i] = 'merges') then Result.Delete(i);
      //for now Canadian data is not supported
      //if (UpperCase(Copy(Result.Strings[i],1,3)) = 'CA_') then Result.Delete(i);
   end;
   Result.Sort;
end;


function DEMIX_AreasWanted(CanLimitAreas : boolean = true) : tStringList;
begin
  if CanLimitAreas then Result := ModeForAreaSelection
  else Result := DEMIX_GetListOfAreas;
end;


function GetCountryForArea(Area : shortString) : shortstring;
var
   Table : tMyData;
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


function GetWinnerBetweenTwoDEMs(dbOnTable : integer; DEM1,DEM2 : shortstring; Tolerance : float32) : shortstring;
var
   eval1,eval2 : float32;
begin
   eval1 := GISdb[dbOnTable].MyData.GetFieldByNameAsFloat(DEM1);
   eval2 := GISdb[dbOnTable].MyData.GetFieldByNameAsFloat(DEM2);
   if (eval1 + Tolerance < Eval2) then Result := DEM1
   else if (eval2 + Tolerance < Eval1) then Result := DEM2
   else Result := 'TIE';
end;


procedure DoDEMIX_DifferenceMaps(AreaName,ShortName,LongName : shortString; var Graph1,Graph2 : tThisBaseGraph);
var
   TestGrid,DSMgrid,DTMGrid,
   i,UseDSM,UseDTM : integer;
   Min,Max,BinSize : float32;
   DSMElevFiles,DSMLegendFiles,DTMElevFiles,DTMLegendFiles : tStringList;


      procedure ModifyGraph(Graph : tThisBaseGraph);
      begin
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



begin {procedure DoDEMIX_DifferenceMaps}
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
            TestGrid := CreateRoughnessSlopeStandardDeviationMap(false,TestDEMs[i],3);
            DTMGrid := CreateRoughnessSlopeStandardDeviationMap(false,UseDTM,3);
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
               DSMGrid := CreateRoughnessSlopeStandardDeviationMap(false,UseDSM,3);
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
end {procedure DoDEMIX_DifferenceMaps};



initialization
   SSIMresultsDir := '';
finalization
end.



