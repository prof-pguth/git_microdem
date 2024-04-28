unit multigrid;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}



{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   {$IFDEF DEBUG}
      //{$Define RecordMultiGrids}
      //{$Define RecordHyperion}
      //{$Define RecordSentinel1}
      //{$Define RecordMultiGridsDetailed}
      //{$Define RecordCloseMultiGrids}
      //{$Define RecordSatClass}
      //{$Define RecordMultiGridGraph}
   {$Else}
   {$EndIf}
{$EndIf}


interface


uses
//needed for inline of the core DB functions
   Petmar_db,
   Data.DB,
   {$IfDef UseFireDacSQLlite}
      FireDAC.Comp.Client, FireDAC.Comp.Dataset,FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteWrapper,
   {$EndIf}

   {$IfDef UseTDBF}
      dbf,
   {$EndIf}

   {$IfDef UseTCLientDataSet}
      DBClient,
   {$EndIf}
//end DB declarations


   Windows,Graphics, Classes,SysUtils,Forms,Math,
   System.IOUtils,System.Diagnostics,
   StrUtils,

   Petmar_types,PETMAR,PETMath,BaseMap,
   DEMMapF,DEMCoord,Geotiff,
   BaseGraf,
   DEMdefs;

type
   tStatOpts = (soTable,soGraph,soCumDist);

type
   tMultiGridArray = class
   private
      TheFiles : tStringList;
      procedure TraingSetInitialization;
      procedure SumOrAverageGrid(SumGrid,AvgGrid,MedianGrid : integer);
      procedure CreateGrids(MinDEM,MaxDEM,RangeDEM : integer);
   public
      MG_Name,MG_short_name    : ShortString;
      MonthlyData : boolean;
      SatImageIndex,
      TrainingPointsDB,
      ClassesDB,IndexBand,
      FirstValidGrid,
      PossGrids,NumGrids   : integer;
      PointGraph : tThisBaseGraph;
      BasePath,
      SupClassDir,
      MainName   : PathStr;
      MapOwner : TMapForm;
      Color : tColor;
      CurrentClass : shortstring;
      CurrentColor : tPlatformColor;
      Grids : array[1..MaxGridsInMG] of integer;
      UseBand   : tUseBands;
      BandWavelengths  : tMaxBandsArray;

      constructor Create;
      destructor Destroy;

      procedure ReadMultigrids;
      procedure LoadFromSingleFileGeotiff(TiffImage : tTiffImage);
      procedure LoadFromENVI(fName : PathStr);
      procedure LoadLandsatMultigrid(SatImageIndex: integer; LoadAll : boolean);
      procedure LoadHyperion;
      procedure LoadMultigridsFromStringList(TheFilesSL : tStringList);
      function OpenNewGrid(theName: ShortString; zUnits :  tElevUnit; Precision : tDEMprecision): integer;

      procedure SaveMultigrids;
      procedure ReinterpolateAllToSameResolution;

      procedure DisplayNewGrid(NewDEM : integer);
      procedure DrawPointGraph(var PointGraph : tThisBaseGraph; Lat,Long : float64; Redraw : boolean = true);
      function AnnualParameterGraph(Lat,Long: float64; TStr : shortstring = '') : TThisbasegraph;
      procedure KeyPercentiles(Bands : boolean = true);
      procedure MakeSlopeCorrelationGrids;
      procedure NormalizeGrids;
      procedure ZeroAsMissing(MissingValue : float64; Save : boolean = true);

      procedure MultiplyGrids;
      procedure SumGrids;
      procedure CreateAverageValueGrid;
      function MedianGrids(OpenMap : boolean) : integer;
      procedure CreateMinValueGrid;
      procedure CreateMaxValueGrid;

      procedure CreateMinMaxRange;
      procedure CreateMinMaxMonthGrids;

      procedure FindHighValues(LowLimit,HighLimit : integer; MapOwner : tMapForm);

      procedure NewTrainingSet;
      procedure LoadTrainingSet(fName : PathStr = '');
      procedure AddTrainingPoint(Lat,Long : float64; Update : boolean = true);
      procedure AddTrainingBox(Lat1,Long1,Lat2,Long2 : float64);

      function BasicStats : integer;

      procedure NDVIPercentileTimeSeries(ByPercentile,ByJD : boolean);
      procedure NDVIPointTimeSeries(Lat,Long : float64);

      function BandAxisValue(Band : integer) : float64;
      function CreateAverageReflectanceGraph(xlo,ylo,xhi,yhi : integer) : tThisBaseGraph;
   end;


   tMonthlyDBArray = class
      private
      public
         MapOwner : TMapForm;
         DB_Name,LongName : shortstring;
         CurrentMonth : integer;
         DBs : array[1..12] of integer;
         constructor Create;
         destructor Destroy;
         procedure LoadMonthlyDBs(fList : tStringList);
   end;

   tLidarMatchedGrids = class
      private
      public
         MapOwner : TMapForm;
         LMGindex,
         DSMgrid,DTMgrid,NVSgrid,
         IntensityGrid,RGBgrid,NIRgrid,
         CHMgrid,ClassGrid,ChangeGrid : integer;
         MatchName : shortstring;
         constructor Create(i : integer; TheDir : PathStr = '');
         destructor Destroy;
         procedure OpenFromDirectory(TheDir : PathStr = '');
   end;


const
   MaxMontlyDBArray = 3;
   MaxLMG = 3;   //for lidar
   TempMG : integer = 0;
   MinTempMG : integer = 0;
   MaxTempMG : integer = 0;
   PrecipMG : integer = 0;
   SolarRad : integer = 0;
   ETOMG : integer = 0;

var
   MultiGridArray : array[1..MaxMultiGrid] of tMultiGridArray;
   MonthlyDBArray : array[1..MaxMontlyDBArray] of tMonthlyDBArray;
   lmg : array[1..MaxLMG] of tLidarMatchedGrids;

procedure OpenEGMgrids;

procedure OpenSentinel1Radar;

function OpenMultigridByDirectory : integer;
function OpenMultigridByStringList(sl : tStringList) : integer;

procedure ClassStatsForGrid(StatOpts : tStatOpts; CurDEM,CurDB : integer);
procedure OpenTheMultigrid(ThisOne : integer; fName : PathStr);

procedure OpenLandsatOrSentinel2Multigrid(ThisOne,aSatImageIndex : integer; LoadAll : boolean = false);

procedure OpenHyperionMultigrid(ThisOne : integer; fName : PathStr);
procedure CloseSingleMonthlyDBArray(var DB : integer);
procedure CloseSingleMultigrid(var MG : integer);
procedure CloseAllMultigrids;

function FindOpenMultigrid(var ThisOne : integer) : boolean;
function FindOpenMonthlyDBArray(var ThisOne : integer) : boolean;
function GetMultiGridPath : PathStr;
function NumOpenMonthlyDBArray : integer;

function OpenMonthlyMultiGrids(Parameter : shortstring = ''; OpenMaps : boolean = true) : integer;
function MultipleMultigridsAnnualParamrGraph(MG1,MG2,MG3 : integer; Lat,Long: float64; Extra : shortstring = '') : TThisbasegraph;
function ValidLMG(i : integer) : boolean;
function ValidMultiGrid(i : integer) : boolean;

procedure OpenTempPrecipEvap(OpenMaps : boolean = true);
procedure OpenDailyTemps(OpenMaps : boolean = true);
procedure OpenSolarRad(OpenMaps : boolean = true);


implementation


uses
   {$IfDef ExGeostats}
   {$Else}
      DEMStat,
   {$EndIf}

   PetDBUtils,
   PetImage,
   DEM_Manager,
   Map_Overlays,
   Hyp_display,
   gdal_tools,
   DataBaseCreate,

   Monthly_grids,

   DEMDatabase,DEMEros,
   lidar_multiple_grid_display,
   rgb_colors_three_params,
   Nevadia_Main;


procedure OpenSentinel1Radar;
var
   ThisOne,i,j,k,x,y : integer;
   NumPts : int64;
   Paths,Tiffs : tStringList;
   fName,mgName : PathStr;
   bmp : array[1..3] of tMyBitmap;
   mem : array[1..3] of tBmpMemory;


          procedure LoadOne(j : integer; fName : PathStr);
          begin
              {$If Defined(RecordSentinel1)} WriteLineToDebugFile('Load one=' + IntToStr(j) + '  ' + fName); {$EndIf}
              LoadNewDEM(MultiGridArray[ThisOne].Grids[j],fName,false);
              if (ExtractFileExt(fName) = '.tif') then begin
                 DEMGlb[MultiGridArray[ThisOne].Grids[j]].DEMheader.ElevUnits := euImagery;
                 DEMGlb[MultiGridArray[ThisOne].Grids[j]].MarkInRangeMissing(0,0,NumPts);
                 DEMGlb[MultiGridArray[ThisOne].Grids[j]].CheckMaxMinElev;
                 File2Trash(fName);
                 fName := ChangeFileExt(fName,'.dem');
                 {$If Defined(RecordSentinel1)} WriteLineToDebugFile('Resave one=' + fName); {$EndIf}
                 DEMGlb[MultiGridArray[ThisOne].Grids[j]].WriteNewFormatDEM(fName);
                 DEMGlb[MultiGridArray[ThisOne].Grids[j]].DEMFileName := fName;
              end;
              {$If Defined(RecordSentinel1)} WriteLineToDebugFile('Create selection map'); {$EndIf}
              CreateDEMSelectionMap(MultiGridArray[ThisOne].Grids[j],true,false,mtElevGray);
              {$If Defined(RecordSentinel1)} WriteLineToDebugFile('Load one out=' + fName); {$EndIf}
          end;


begin
   {$If Defined(RecordMultiGrids) or Defined(RecordSentinel1)} WriteLineToDebugFile('OpenSentinel1Radar'); {$EndIf}
   try
      HeavyDutyProcessing := true;
      Paths := tStringList.Create;
      Paths.Add(LastSatDir);
      if GetMultipleDirectories('Directories with Sentinel-1 images to warp',Paths) then begin
         for I := 0 to pred(Paths.Count) do begin
            fName := Paths.Strings[i];
            {$If Defined(RecordSentinel1)} WriteLineToDebugFile('Path=' + fName); {$EndIf}

            Tiffs := Nil;
            FindMatchingFiles(fName,'*.dem',Tiffs,6);
            if (Tiffs.Count = 0) then begin
               {$If Defined(RecordMultiGrids) or Defined(RecordSentinel1)} WriteLineToDebugFile('No DEMs'); {$EndIf}
               FreeAndNil(Tiffs);
               FindMatchingFiles(fName,'*.tiff',Tiffs,6);
               if (Tiffs.Count > 0) then begin
                  {$If Defined(RecordSentinel1)} WriteLineToDebugFile('No tifs, call ResampleSentinel_1 ' + fName); {$EndIf}
                  ResampleSentinel_1(fName);
                  FreeAndNil(Tiffs);
                  FindMatchingFiles(fName,'*.tif',Tiffs,6);
               end;
            end;

            if (Tiffs.Count >= 2) then begin
               if FindOpenMultigrid(ThisOne) then begin
                  MultiGridArray[ThisOne] := tMultiGridArray.Create;
                  MultiGridArray[ThisOne].BasePath := Paths[i];
                  MultiGridArray[ThisOne].MG_Name := 'Sentinel-1: ' + LastSubDir(Paths[i]);
                  for j := 1 to Tiffs.Count do begin
                    fName := Tiffs.Strings[pred(j)];
                    LoadOne(j,fName);
                  end;
                  MultiGridArray[ThisOne].Grids[3] := GridRatio(MultiGridArray[ThisOne].Grids[2],MultiGridArray[ThisOne].Grids[1],mtElevGray);

                  if CopyImageToBitmap(DEMGlb[MultiGridArray[ThisOne].Grids[3]].SelectionMap.Image1,bmp[3]) and CopyImageToBitmap(DEMGlb[MultiGridArray[ThisOne].Grids[2]].SelectionMap.Image1,bmp[2]) and
                      CopyImageToBitmap(DEMGlb[MultiGridArray[ThisOne].Grids[1]].SelectionMap.Image1,bmp[1]) then begin
                         for k := 1 to 3 do Mem[k] := tBMPMemory.Create(bmp[k]);
                           for y := 1 to pred(pred(Bmp[1].Height)) do begin
                              for x := 1 to pred(pred(bmp[1].Width)) do begin           //vv/vh for blue
                                 mem[3].SetRedChannel(x,y,mem[2].RedChannel(x,y));      //vh
                                 mem[3].SetGreenChannel(x,y,mem[1].GreenChannel(x,y));  //vv
                              end;
                           end;
                           DEMGlb[MultiGridArray[ThisOne].Grids[3]].SelectionMap.Image1.Picture.Graphic := bmp[3];
                          for k := 1 to 3 do begin
                             Mem[k].Destroy;
                             BMP[k].Destroy;
                          end;
                  end;
               end
               else begin
                  {$If Defined(RecordMultiGrids) or Defined(RecordSentinel1)} WriteLineToDebugFile('No open multigrid'); {$EndIf}
               end;
            end
            else begin
               {$If Defined(RecordMultiGrids) or Defined(RecordSentinel1)} WriteLineToDebugFile('No files at all'); {$EndIf}
            end;
            Tiffs.Destroy;
         end;
      end;
      Paths.Destroy;
   finally
      HeavyDutyProcessing := false;
   end;
end;


procedure OpenEGMgrids;
begin
   if not ValidDEM(EGM96_grid) then EGM96_grid := OpenNewDEM(Geoid96FName,false);
   if not ValidDEM(EGM2008_grid) then EGM2008_grid := OpenNewDEM(Geoid2008FName,false);
   if not ValidDEM(EGMdiff_grid) then EGMdiff_grid := OpenNewDEM(GeoidDiffFName,false);
end;


function ValidMultiGrid(i : integer) : boolean;
begin
   Result := (i in [1..MaxMultiGrid]) and (MultiGridArray[i] <> Nil);
end;


procedure SaveLastMultigrid(ThisOne : integer; var fName : PathStr);
begin
   if (ThisOne = 1) then LastMultigrid1 := fName
   else if (ThisOne = 2) then LastMultigrid2 := fName
   else if (ThisOne = 3) then LastMultigrid3 := fName;
end;


function OpenMultigridByDirectory : integer;
var
   fName : PathStr;
begin
   if FindOpenMultigrid(Result) then begin
      GetDOSPath('Multigrids',fName);
      OpenTheMultigrid(Result,fName);
      SaveLastMultigrid(Result,fName);
   end;
end;


function OpenMultigridByStringList(sl : tStringList) : integer;
begin
   if FindOpenMultigrid(Result) then begin
      MultiGridArray[Result] := tMultiGridArray.Create;
      MultiGridArray[Result].BasePath := extractFilePath(sl.Strings[0]);
      MultiGridArray[Result].MG_Name := '';
      MultiGridArray[Result].LoadMultigridsFromStringList(sl);
   end;
end;


procedure CloseAllMultigrids;
var
   i,j : integer;
begin
   {$IfDef RecordMultiGrids} WriteLineToDebugFile('CloseAllMultigrids in'); {$EndIf}
   for i := 1 to MaxMultiGrid do begin
      if (MultiGridArray[i] <> Nil) then begin
         {$IfDef RecordMultiGrids} WriteLineToDebugFile('Close #' + IntToStr(i)); {$EndIf}
         if (MultiGridArray[i].MapOwner <> Nil) then MultiGridArray[i].MapOwner.Destroy;
         MultiGridArray[i].Destroy;
         MultiGridArray[i] := Nil;
      end;
   end;

   for i := 1 to MaxMontlyDBArray do begin
      j := i;
      CloseSingleMonthlyDBArray(j);
      MonthlyDBArray[j] := nil;
   end;

   TempMG := 0;
   MinTempMG := 0;
   MaxTempMG := 0;
   PrecipMG := 0;
   ETOMG := 0;
   {$IfDef RecordMultiGrids} WriteLineToDebugFile('CloseAllMultigrids out'); {$EndIf}
end;


procedure OpenSolarRad;
begin
   if (PathIsValid(WorldClimate2Dir)) then begin
      if not ValidMultiGrid(SolarRad) then SolarRad := OpenMonthlyMultiGrids('Solar radiation (entire earth)',OpenMaps);
   end
   else MessageToContinue('Missing World Climate 2 data');
end;


procedure OpenDailyTemps;
begin
   {$IfDef AllowUSNAdataDownloads}  ClimateGetData;  {$EndIf}
   if (PathIsValid(WorldClimate2Dir)) then begin
      if (MaxTempMG = 0) then MaxTempMG := OpenMonthlyMultiGrids('Maximum temperature',OpenMaps);
      if (TempMG = 0) then TempMG := OpenMonthlyMultiGrids('Average temperature (entire earth)',OpenMaps);
      if (MinTempMG = 0) then MinTempMG := OpenMonthlyMultiGrids('Minimum temperature',OpenMaps);
   end
   else MessageToContinue('Missing World Climate 2 data');
end;


procedure OpenTempPrecipEvap;
begin
   {$IfDef AllowUSNAdataDownloads}  ClimateGetData;  {$EndIf}
   if (PathIsValid(WorldClimate2Dir)) then begin
      if (TempMG = 0) then TempMG := OpenMonthlyMultiGrids('Average temperature (entire earth)',OpenMaps);
      if (PrecipMG = 0) then PrecipMG := OpenMonthlyMultiGrids('Precipitation',OpenMaps);
      if (ETOMG = 0) then ETOMG := OpenMonthlyMultiGrids('Reference evapotranspiration',OpenMaps);
   end
   else MessageToContinue('Missing World Climate 2 data');
end;


function ValidLMG(i : integer) : boolean;
begin
   Result := (i > 0) and (i <= MaxLMG) and (lmg[i] <> nil);
end;


{ tMonthlyDBArray }

constructor tMonthlyDBArray.Create;
begin
   LastVectorMap := SetUpVectorMap(true,true);
   MapOwner := VectorMap[LastVectorMap];
end;

destructor tMonthlyDBArray.Destroy;
begin
   inherited;
end;


procedure tMonthlyDBArray.LoadMonthlyDBs(fList : tStringList);
var
  i: Integer;
begin
    for i := 1 to 12 do begin
       DBs[i] := MapOwner.OpenDBonMap('',fList.Strings[pred(i)],false,false);
    end;
end;


function OpenMonthlyMultiGrids(Parameter : shortstring = ''; OpenMaps : boolean = true) : integer;
var
   ThisOne,ForceMin,ForceMax,ForceMissing : integer;
   NumPts : int64;
   fName,tName : PathStr;
   Options,
   TheFiles : tStringList;
   ElevUnits : shortstring;
   Missing : integer;
   I : Integer;
   Table : tMyData;
begin
   {$IfDef RecordMultiGrids} WriteLineToDebugFile('OpenMonthlyMultiGrids in ' + Parameter); {$EndIf}
   StopSplashing;
   Result := -1;
   Table := tMyData.Create(MonthlyClimateFName);
   if (Parameter = '') then begin
      Options:= Table.ListUniqueEntriesInDB('PARAMETER');
      Parameter := GetFromList('Monthly time series',Options,True);
      Options.Free;
      if (Parameter = '') then exit;
   end;
   TheFiles := tStringList.Create;
   for I := 1 to 12 do begin
      Table.ApplyFilter('MONTH=' + IntToStr(i) + ' AND PARAMETER=' + QuotedStr(Parameter));
      if (Table.RecordCount = 1) then TheFiles.Add(Table.GetFieldByNameAsString('FILENAME'))
      else begin
         {$IfDef RecordMultiGrids} WriteLineToDebugFile('Did not find only one file, instead n=' + IntToStr(Table.RecordCount) + '  filter=' + Table.Filter); {$EndIf}
      end;
   end;
   {$IfDef RecordMultiGrids} WriteLineToDebugFile('Found files, n=' + IntToStr(TheFiles.Count)); {$EndIf}

   if (TheFiles.Count > 0) then begin
      DEMNowDoing := Calculating;
      fName := TheFiles.Strings[0];
      if (UpperCase(ExtractFileExt(fName)) = '.DBF') then begin
         if FindOpenMonthlyDBArray(ThisOne) then begin
            MonthlyDBArray[ThisOne].DB_Name := Table.GetFieldByNameAsString('SHORT_NAME');
            MonthlyDBArray[ThisOne].LongName := Parameter;
            MonthlyDBArray[ThisOne].LoadMonthlyDBs(TheFiles);
            for I := 1 to 12 do begin
               GISDB[MonthlyDBArray[ThisOne].DBs[i]].dbOpts.DirField := Table.GetFieldByNameAsString('VN_FIELD');
               GISDB[MonthlyDBArray[ThisOne].DBs[i]].dbOpts.MagField := Table.GetFieldByNameAsString('VE_FIELD');
               if GISDB[MonthlyDBArray[ThisOne].DBs[i]].dbOpts.DirField = '' then begin
                  GISDB[MonthlyDBArray[ThisOne].DBs[i]].dbOpts.DBAutoShow := dbasColorByNumeric;
                  GISDB[MonthlyDBArray[ThisOne].DBs[i]].dbOpts.dbColorMode := dbcmFieldLinear;
                  GISDB[MonthlyDBArray[ThisOne].DBs[i]].dbOpts.Symbol.Size := 1;
               end
               else begin
                  GISDB[MonthlyDBArray[ThisOne].DBs[i]].dbOpts.WindAutoSpace := false;
                  GISDB[MonthlyDBArray[ThisOne].DBs[i]].dbOpts.WindPixelSpace := 30;
                  GISDB[MonthlyDBArray[ThisOne].DBs[i]].dbOpts.VectorLineMult := 30;
                  GISDB[MonthlyDBArray[ThisOne].DBs[i]].dbOpts.GISVectorsMaxSpeed := Table.GetFieldByNameAsInteger('MAX');
                  GISDB[MonthlyDBArray[ThisOne].DBs[i]].dbOpts.GISVectorsByMaxSpeed := true;
                  MDDef.PlotArrowHead := true;
                  GISDB[MonthlyDBArray[ThisOne].DBs[i]].dbOpts.DBAutoShow := dbasVector;
               end;

               GISDB[MonthlyDBArray[ThisOne].DBs[i]].dbOpts.FloatColorField := Table.GetFieldByNameAsString('DB_FIELD');
               GISDB[MonthlyDBArray[ThisOne].DBs[i]].dbOpts.ColorMin := Table.GetFieldByNameAsInteger('MIN');
               GISDB[MonthlyDBArray[ThisOne].DBs[i]].dbOpts.ColorMax := Table.GetFieldByNameAsInteger('MAX');
            end;
            AddOverlay(MonthlyDBArray[ThisOne].MapOwner,ovoDatabases);
            MonthlyDBArray[ThisOne].MapOwner.MapDraw.MonthlyDBArrayOnMap := ThisOne;
         end;
      end
      else begin
         if FindOpenMultigrid(ThisOne) then begin
            Result := ThisOne;
            MultiGridArray[ThisOne] := tMultiGridArray.Create;
            MultiGridArray[ThisOne].Color := Table.GetFieldByNameAsInteger('COLOR');
            MultiGridArray[ThisOne].MG_Name := Parameter;
            MultiGridArray[ThisOne].MonthlyData := true;
            MultiGridArray[ThisOne].MG_Short_Name := Table.GetFieldByNameAsString('SHORT_NAME');
            ForceMin := Table.GetFieldByNameAsInteger('MIN');
            ForceMax := Table.GetFieldByNameAsInteger('MAX');
            if Table.GetFieldByNameAsString('MISSING') <> '' then ForceMissing := Table.GetFieldByNameAsInteger('MISSING')
            else ForceMissing := -999;
            {$IfDef RecordMultiGrids} WriteLineToDebugFile('MG=' + IntToStr(ThisOne) + '  ' + MultiGridArray[ThisOne].MG_Short_Name + '  ForceRange ' + IntToStr(ForceMin) + ' to ' + IntToStr(ForceMin)); {$EndIf}
            tName := LastDEMName;
            MultiGridArray[ThisOne].LoadMultigridsFromStringList(TheFiles);
            LastDEMName := tName;

            ElevUnits := Table.GetFieldByNameAsString('UNITS');
            for I := 1 to 12 do begin
               if (ForceMin <> -999) then DEMGlb[MultiGridArray[ThisOne].Grids[i]].DEMheader.MinElev := ForceMin;
               if (ForceMax <> -999) then DEMGlb[MultiGridArray[ThisOne].Grids[i]].DEMheader.MaxElev := ForceMax;
               if (ForceMissing <> -999) then DEMGlb[MultiGridArray[ThisOne].Grids[i]].MarkInRangeMissing(ForceMissing-0.0001,ForceMissing+0.0001,NumPts);

               DEMGlb[MultiGridArray[ThisOne].Grids[i]].AreaName := MonthName[i] + ' ' + Parameter;
               if Copy(ElevUnits,1,2) = 'mm' then DEMGlb[MultiGridArray[ThisOne].Grids[i]].DEMheader.ElevUnits := euMM
               else if (ElevUnits = 'm/s') or (ElevUnits = 'm/sec') then DEMGlb[MultiGridArray[ThisOne].Grids[i]].DEMheader.ElevUnits := euMetersPerSec
               else DEMGlb[MultiGridArray[ThisOne].Grids[i]].DEMheader.ElevUnits := euUndefined;
               {$IfDef RecordMultiGridFulls} WriteLineToDebugFile(IntToStr(i) + '  z range ' + RealToString(DEMGlb[MultiGridArray[ThisOne].Grids[i]].DEMheader.MinElev, -12, -2) + ' to ' + RealToString(DEMGlb[MultiGridArray[ThisOne].Grids[i]].DEMheader.MinElev, -12, -2)); {$EndIf}
            end;
            if OpenMaps then begin
               CreateDEMSelectionMap(MultiGridArray[ThisOne].Grids[1],false,false,mtElevSpectrum);
               MultiGridArray[ThisOne].MapOwner := DEMGlb[MultiGridArray[ThisOne].Grids[1]].SelectionMap;
               MultiGridArray[ThisOne].MapOwner.MapDraw.MultiGridOnMap := ThisOne;
               {$IfDef RecordMultiGridsFull} WriteLineToDebugFile('OpenMonthlyMultiGrids map created ' + MultiGridArray[ThisOne].MapOwner.MapDraw.MapSizeString); {$EndIf}
               MultiGridArray[ThisOne].MapOwner.MapDraw.LockZColors := true;
               MultiGridArray[ThisOne].MapOwner.SetMultibandToShowOnMap(1);
               {$IfDef RecordMultiGridsFull} WriteLineToDebugFile('OpenMonthlyMultiGrids set multibands done ' + MultiGridArray[ThisOne].MapOwner.MapDraw.MapSizeString); {$EndIf}
               if (ThisOne > 1) then begin
                  MultiGridArray[ThisOne].MapOwner.Top := MultiGridArray[pred(ThisOne)].MapOwner.Top + MultiGridArray[Pred(ThisOne)].MapOwner.Height + 5;
               end;
            end;
         end;
      end;
      DEMNowDoing := JustWandering;
   end;
   Table.Destroy;
   {$IfDef RecordMultiGrids} if ValidMultigrid(ThisOne) then WriteLineToDebugFile('OpenMonthlyMultiGrids done ' + Parameter); {$EndIf}
   if OpenMaps then StartMontlyTimeSeries;
   UpdateMenusForAllMaps;
   StopSplashing;
end;


function MultipleMultigridsAnnualParamrGraph(MG1,MG2,MG3 : integer; Lat,Long: float64; Extra : shortstring = '') : TThisbasegraph;
var
   i,NumGood,Plots : integer;
   z1 : float32;
   fName : PathStr;
   aLine : shortString;
   GoodData : boolean;

   procedure NewDataFile(MG : integer);
   var
      rfile : file;
      i : integer;
   begin
      if (MG <> 0) then begin
         inc(Plots);
         Result.GraphDraw.ShowLine[Plots] := true;
         Result.OpenDataFile(rfile,MultiGridArray[MG].Color);
         for i := 1 to 12 do begin
            if DEMGlb[MultiGridArray[MG].Grids[i]].GetElevFromLatLongDegree(Lat,Long,z1) then begin
               Result.AddPointToDataBuffer(rfile,i,z1);
            end
            else begin
               Gooddata := false;
            end;
         end;
         Result.ClosePointDataFile(rfile);
         Result.Caption := Result.Caption + ' ' + MultiGridArray[MG].MG_short_name;
         Result.GraphDraw.VertLabel := Result.GraphDraw.VertLabel + ' ' + MultiGridArray[MG].MG_short_name;
      end;
   end;


begin
   {$IfDef RecordMultiGrids} WriteLineToDebugFile('TwoMultigridsAnnualParameterGraph in ' + LatLongDegreeToString(Lat,Long,DecDegrees)); {$EndIf}
   GoodData := true;
   Result := TThisBaseGraph.Create(Application);
   Result.GraphDraw.MinHorizAxis := 1;
   Result.GraphDraw.MaxHorizAxis := 12;
   Result.GraphDraw.HorizLabel := 'Month';
   Result.GraphDraw.VertLabel := '';
   Plots := 0;
   NewDataFile(MG1);
   if GoodData then NewDataFile(MG2);
   if GoodData then NewDataFile(MG3);
   if GoodData then begin
      Result.SetUpGraphForm;
      Result.GraphDraw.LLcornerText := LatLongDegreeToString(Lat,Long,VeryShortDegrees) + ' ' + Extra;
      Result.Caption := LatLongDegreeToString(Lat,Long,VeryShortDegrees);
   end
   else begin
      Result.Destroy;
      Result := nil;
   end;
   {$IfDef RecordMultiGrids} WriteLineToDebugFile('TwoMultigridsAnnualParameterGraph out'); {$EndIf}
end;


function GetMultiGridPath : PathStr;
begin
   Result := NextFilePath(MainMapData + 'multigrid\multigrid');
end;


function NumOpenMonthlyDBArray : integer;
var
   i : integer;
begin
   Result := 0;
   for I := 1 to MaxMontlyDBArray do
      if (MonthlyDBArray[i] <> nil) then
         inc(result);
end;


function FindOpenMultigrid(var ThisOne : integer) : boolean;
var
   i : integer;
begin
   Result := true;
   for I := 1 to MaxMultiGrid do begin
      if (MultiGridArray[i] = nil) then begin
         ThisOne := i;
         exit;
      end;
   end;
   Result := false;
   MessageToContinue('Cannot load another multgrid, already loaded = ' + IntToStr(MaxMultiGrid));
end;


function FindOpenMonthlyDBArray(var ThisOne : integer) : boolean;
var
   i : integer;
begin
   Result := true;
   for I := 1 to MaxMontlyDBArray do begin
      if (MonthlyDBArray[i] = nil) then begin
         ThisOne := i;
         MonthlyDBArray[i] := tMonthlyDBArray.Create;
         exit;
      end;
   end;
   Result := false;
   MessageToContinue('Cannot load another');
end;


procedure CloseSingleMonthlyDBArray(var DB : integer);
begin
   if (DB <> 0) and (MonthlyDBArray[DB] <> nil) then begin
      MonthlyDBArray[DB].Destroy;
      MonthlyDBArray[DB] := Nil;
   end;
   {$IfDef RecordMultiGrids} WriteLineToDebugFile('CloseSingleMultigrid out'); {$EndIf}
end;

procedure CloseSingleMultigrid(var MG : integer);
begin
   {$IfDef RecordMultiGrids} WriteLineToDebugFile('CloseSingleMultigrid in, MG=' + IntToStr(MG)); {$EndIf}
   if (MG <> 0) and (MultiGridArray[MG] <> nil) then begin
      MultiGridArray[MG].Destroy;
      MultiGridArray[MG] := Nil;
   end;
   {$IfDef RecordMultiGrids} WriteLineToDebugFile('CloseSingleMultigrid out'); {$EndIf}
end;


procedure StripLandsatName(fName : ANSIString; var ls : ANSIString; var JD,Year : integer; var DecYear : float64);
var
   TStr : ANSIstring;
begin
   TStr := fName;
   Petmar_types.BeforeSpecifiedCharacterANSI(TStr,'_',true,true);
   ls := Petmar_types.BeforeSpecifiedCharacterANSI(TStr,'_',true,true);
   Year := StrToInt(Petmar_types.BeforeSpecifiedCharacterANSI(TStr,'_',true,true));
   JD := StrToInt(TStr);
   DecYear := Year + JD/AnnualJulianDay(Year,12,31);
end;


procedure tMultiGridArray.AddTrainingBox(Lat1,Long1,Lat2,Long2 : float64);
var
   x,y,xg1,yg1,xg2,yg2,Npts,incr : integer;
   Lat,Long : float64;
begin
   DEMGlb[Grids[1]].LatLongDegreeToDEMGridInteger(Lat1,Long1,xg1,yg1);
   DEMGlb[Grids[1]].LatLongDegreeToDEMGridInteger(Lat2,Long2,xg2,yg2);
   incr := 1;
   repeat
      NPts := succ(xg2-xg1) div incr * succ(yg2-yg1) div incr;
      inc(incr);
   until Npts <= MDDef.MaxPointsAddInBox;
   x := xg1;
   while x <= xg2 do begin
      y := yg1;
      while y <= yg2 do begin
         DEMGlb[Grids[1]].DEMGridToLatLongDegree(x,y,Lat,Long);
         AddTrainingPoint(Lat,Long,false);
         inc(y,incr);
      end;
      inc(x,incr);
   end;
   GISDB[TrainingPointsDB].dbOpts.DBAutoShow := dbasColorField;
   GISDB[TrainingPointsDB].RedrawLayerOnMap;
   GISDB[TrainingPointsDB].dbTablef.ShowStatus;
end;

procedure tMultiGridArray.AddTrainingPoint(Lat,Long : float64; Update : boolean = true);
var
   i,xg,yg : integer;
   z : float32;
begin
   GISDB[TrainingPointsDB].MyData.Insert;
   GISDB[TrainingPointsDB].MyData.SetFieldByNameAsFloat('LAT',Lat);
   GISDB[TrainingPointsDB].MyData.SetFieldByNameAsFloat('LONG',Long);
   GISDB[TrainingPointsDB].MyData.SetFieldByNameAsString('CLASS',CurrentClass);
   GISDB[TrainingPointsDB].MyData.SetColorFromPlatformColor(CurrentColor);
   for i := 1 to PossGrids do begin
      if (Grids[i] <> 0) then begin
         DEMGlb[Grids[i]].LatLongDegreeToDEMGridInteger(Lat,Long,xg,yg);
         if DEMGlb[Grids[i]].GetElevMeters(xg,yg,z) then begin
            if (i=1) then begin
               GISDB[TrainingPointsDB].MyData.SetFieldByNameAsInteger('SAT_COL', xg);
               GISDB[TrainingPointsDB].MyData.SetFieldByNameAsInteger('SAT_ROW', yg);
            end;
            GISDB[TrainingPointsDB].MyData.SetFieldByNameAsInteger('BAND_' + IntToStr(i), round(z));
         end;
      end;
   end;
   GISDB[TrainingPointsDB].MyData.Post;
   if Update then begin
      GISDB[TrainingPointsDB].dbOpts.DBAutoShow := dbasColorField;
      GISDB[TrainingPointsDB].RedrawLayerOnMap;
      GISDB[TrainingPointsDB].dbTablef.ShowStatus;
   end;
end;


function tMultiGridArray.AnnualParameterGraph(Lat,Long: float64; TStr : shortstring = '') : TThisbasegraph;
var
   i,db : integer;
   z : float32;
   fName : PathStr;
   Findings : tStringList;
begin
   {$IfDef RecordMultiGrids} WriteLineToDebugFile('tMultiGridArray.AnnualParameterGraph in ' + MG_Name); {$EndIf}
   Result := Nil;
   Findings := tStringList.Create;
   Findings.Add('MONTH,PARAMETER');
   for I := 1 to 12 do begin
      if DEMGlb[Grids[i]].GetElevFromLatLongDegree(Lat,Long,z) then begin
         Findings.Add(IntToStr(i) + ',' + RealToString(z,-12,-5));
      end;
   end;
   {$IfDef RecordMultiGrids} WriteLineToDebugFile('tMultiGridArray.AnnualParameterGraph grids read'); {$EndIf}
   if (Findings.Count > 2) then begin
      fName := Petmar.NextFileNumber(MDTempDir,'monthly_graph_','.dbf');
      db := MapOwner.StringListtoLoadedDatabase(Findings, fName);

      {$IfDef RecordMultiGrids} WriteLineToDebugFile('tMultiGridArray.AnnualParameterGraph db loaded'); {$EndIf}
      Result := GISdb[db].CreateScatterGram(GISdb[db].MonthFieldName,'PARAMETER',true,MG_Name + ' at ' + LatLongDegreeToString(Lat,Long,VeryShortDegrees),'Month',MG_Name);
      Result.GraphDraw.LLcornerText := LatLongDegreeToString(Lat,Long,VeryShortDegrees) + ' ' + Tstr;
      Result.GraphDraw.ShowLine[1] := true;
      Result.RedrawDiagram11Click(Nil);
      {$IfDef RecordMultiGrids} WriteLineToDebugFile('tMultiGridArray.AnnualParameterGraph scattergram created'); {$EndIf}
      CloseAndNilNumberedDB(db);
   end
   else begin
      {$IfDef RecordMultiGrids} WriteLineToDebugFile('tMultiGridArray.AnnualParameterGraph pts not found'); {$EndIf}
      Findings.Free;
   end;

   {$IfDef RecordMultiGrids} WriteLineToDebugFile('tMultiGridArray.AnnualParameterGraph out'); {$EndIf}
end;


procedure tMultiGridArray.TraingSetInitialization;
var
   fName : PathStr;
begin
   SupClassDir := ExtractFilePath(BasePath) + 'sup_class\';
   SafeMakeDir(SupClassDir);
   fName := ExtractFilePath(BasePath) + 'sup_class\' + MG_Name + '_classes' + '.dbf';
   if FileExists(fName) then Sysutils.DeleteFile(fName);
end;


procedure tMultiGridArray.NewTrainingSet;
var
   fName : PathStr;
   CreateDataBase : tCreateDataBase;
begin
   {$IfDef RecordSatClass} WriteLineToDebugFile('tMultiGridArray.NewTrainingSet in, BasePath=' + BasePath); {$EndIf}
   TraingSetInitialization;

   fName := NextFileNumber(SupClassDir,MG_Name + '_sup_class_' ,'.dbf');

   {$IfDef RecordSatClass} WriteLineToDebugFile('  Create=' + fName); {$EndIf}

   CreateDataBase := tCreateDataBase.Create(fName);
   CreateDataBase.AddLatLongToTable;
   CreateDataBase.AddSatBandInfo(PossGrids);
   CreateDataBase.WriteCorrectHeader;
   LoadTrainingSet(fName);

   {$IfDef RecordSatClass} WriteLineToDebugFile('TMapForm.Createtrainingset1Click out'); {$EndIf}
end;

procedure tMultiGridArray.LoadTrainingSet(fName : PathStr = '');
var
   CreateDataBase : tCreateDataBase;
begin
   {$IfDef RecordSatClass} WriteLineToDebugFile('tMultiGridArray.LoadTrainingset1Click in, fname=' + fName  + '  map is ' + MapOwner.Caption); {$EndIf}
   TraingSetInitialization;

   if (fName = '') then fName := ExtractFilePath(BasePath) + 'sup_class\';
   if (MapOwner <> Nil) then begin
      TrainingPointsDB := MapOwner.OpenDBonMap('Training set',fName);
      LastTrainSetFName := GISdb[TrainingPointsDB].dbFullName;
      GISdb[TrainingPointsDB].dbOpts.DBAutoShow := (dbasColorField);
      GISdb[TrainingPointsDB].RedrawLayerOnMap;

      fName := ExtractFilePath(BasePath) + 'sup_class\' + MG_Name + '_classes' + '.dbf';
      if Not FileExists(fName) then begin
         CreateDataBase := tCreateDataBase.Create(fName);
         CreateDataBase.AddAField('CLASS',ftString,25);
         CreateDataBase.AddAField('USE',ftString,1);
         CreateDataBase.AddAField('COLOR',ftInteger,9);
         CreateDataBase.AddAField('NPTS',ftInteger,9);
         CreateDataBase.WriteCorrectHeader;
      end;
      ClassesDB := MapOwner.OpenDBonMap('Classes',fName,false,false);
   end;
   {$IfDef RecordSatClass} WriteLineToDebugFile('LoadTrainingset1Click out,  TrainingPointsDB=' + IntToStr(TrainingPointsDB)  + '   ClassesDB =' + IntToStr(ClassesDB)); {$EndIf}
end;


procedure tMultiGridArray.NDVIPercentileTimeSeries(ByPercentile,ByJD : boolean);
var
   sl : tStringList;
   PC,JD,Year,i,j : integer;
   LS,TStr,TStr2 : ANSIstring;
   iName : shortstring;
   DecYear : float64;
begin
   sl := tStringList.Create;
   iName := Copy(DEMGlb[Grids[1]].AreaName,1,4);
   if ByPercentile then begin
      PC := 90;
      ReadDefault('Percentile',PC);
      sl.Add('JULIAN_DAY,' + iName + '_' + IntToStr(PC) + '_PC' + ',LANDSAT,YEAR');
   end
   else begin
      PC := 30;
      ReadDefault('NDVI',PC);
      sl.Add('JULIAN_DAY,'+ iName + '_' + IntToStr(PC) + ',LANDSAT,YEAR');
   end;
  StartProgress('Time series');

   for i := 1 to NumGrids do begin
      UpdateProgressBar(i/NumGrids);
      StripLandsatName(DEMGlb[Grids[i]].AreaName, ls, JD,Year,DecYear);
      if ByJD then TStr := IntToStr(Year) else TStr := RealToString(DecYear,-12,-4);
      if ByPercentile then TStr2 := RealToString(DEMGlb[Grids[i]].FindPercentileElevation(PC),-12,-3)
      else TStr2 := RealToString(DEMGlb[Grids[i]].PercentileOfElevation(PC),-12,-3);
      sl.Add(IntToStr(JD) + ',' +TStr2 + ',' + LS + ',' + TStr);
   end;
   EndProgress;
   GraphFromCSVfile(sl,true,false);
end;


procedure tMultiGridArray.NDVIPointTimeSeries(Lat,Long : float64);
var
   sl : tStringList;
   PC,JD,Year,i,j : integer;
   LS,TStr,TStr2 : ANSIstring;
   z : float32;
   iName : shortString;
   DecYear : float64;
begin
   iName := Copy(DEMGlb[Grids[1]].AreaName,1,4);
   sl := tStringList.Create;
   sl.Add('DEC_YEAR,' + iName + ',LANDSAT,YEAR,JULIAN_DAY,' + iName + '_PC');
   StartProgress('Time series');
   for i := 1 to NumGrids do begin
      UpdateProgressBar(i/NumGrids);
      StripLandsatName(DEMGlb[Grids[i]].AreaName, ls, JD,Year,DecYear);
      if DEMGlb[Grids[i]].GetElevFromLatLongDegree(Lat,Long,z) then begin
         TStr2 := RealToString(z,-12,-3);
         sl.Add(RealToString(DecYear,-12,-4) + ',' +TStr2 + ',' + LS + ',' + IntToStr(Year)+ ',' + IntToStr(JD) + ',' +
                RealToString(DEMGlb[Grids[i]].PercentileOfElevation(z),-12,-3));
      end;
   end;
   EndProgress;
   GraphFromCSVfile(sl,true,false);
end;


procedure OpenLandsatOrSentinel2Multigrid(ThisOne,aSatImageIndex : integer; LoadAll : boolean = false);
var
   HyperspectralForm : THyperspectralForm;
begin
   {$IfDef RecordMultiGrids} WriteLineToDebugFile('OpenLandatOrSentinel2Multigrid, satIndex=' + IntToStr(aSatImageIndex)); {$EndIf}
   MultiGridArray[ThisOne] := tMultiGridArray.Create;
   MultiGridArray[ThisOne].SatImageIndex := aSatImageIndex;
   MultiGridArray[ThisOne].BasePath := SatImage[aSatImageIndex].OriginalFileName;
   MultiGridArray[ThisOne].MG_Name := 'Image ' + MultiGridArray[ThisOne].MainName;
   MultiGridArray[ThisOne].LoadLandsatMultigrid(aSatImageIndex,LoadAll);

   HyperspectralForm := THyperspectralForm.Create(Application);
   HyperspectralForm.GrayBandNum := 5;
   HyperspectralForm.HyperionImage := false;
   HyperspectralForm.MultiGridUsed := ThisOne;
   HyperspectralForm.SetForHyperion;
   StopSplashing;
end;



procedure OpenHyperionMultigrid(ThisOne : integer; fName : PathStr);
var
   HyperspectralForm : THyperspectralForm;
   i : integer;
begin
   {$IfDef RecordMultiGrids} WriteLineToDebugFile('OpenHyperionMultigrid  ' + IntToStr(ThisOne) + ' in ' + fName); {$EndIf}
   MultiGridArray[ThisOne] := tMultiGridArray.Create;
   MultiGridArray[ThisOne].BasePath := fName;
   MultiGridArray[ThisOne].MG_Name := 'Hyperspectral image ' + MultiGridArray[ThisOne].MainName;

   SkipMenuUpdating := true;
   MultiGridArray[ThisOne].LoadHyperion;
   {$IfDef RecordMultiGrids} WriteLineToDebugFile('OpenHyperionMultigrid mid'); {$EndIf}
   if (MultiGridArray[ThisOne].NumGrids = 0) then begin
      MessageToContinue('No Hyperion TIF files found');
   end
   else begin
     HyperspectralForm := THyperspectralForm.Create(Application);
     HyperspectralForm.HyperionImage := true;
     HyperspectralForm.MultiGridUsed := ThisOne;
     HyperspectralForm.SetForHyperion;
   end;

   SkipMenuUpdating := false;
   WmDem.SetMenusForVersion;
   StopSplashing;
   {$IfDef RecordMultiGrids} WriteLineToDebugFile('OpenHyperionMultigrid out, bands=' + IntToStr(MultiGridArray[ThisOne].NumGrids)); {$EndIf}
end;


procedure OpenTheMultigrid(ThisOne : integer; fName : PathStr);
begin
   {$IfDef RecordMultiGrids} WriteLineToDebugFile('OpenTheMultigrid in ' + fName + '  Assign to MG=' + IntToStr(ThisOne)); {$EndIf}
   HeavyDutyProcessing := true;
   MultiGridArray[ThisOne] := tMultiGridArray.Create;
   MultiGridArray[ThisOne].BasePath := fName;
   MultiGridArray[ThisOne].ReadMultigrids;

   if (MultiGridArray[ThisOne].NumGrids > 0) then begin
      CreateDEMSelectionMap(MultiGridArray[ThisOne].Grids[1]);
      DEMGlb[MultiGridArray[ThisOne].Grids[1]].SelectionMap.MapDraw.MultiGridOnMap := ThisOne;
      DEMGlb[MultiGridArray[ThisOne].Grids[1]].SelectionMap.CheckProperTix;
   end
   else begin
      MultiGridArray[ThisOne].Destroy;
      ThisOne := 0;
   end;
   StopSplashing;
   HeavyDutyProcessing := false;
end;


procedure ClassStatsForGrid(StatOpts : tStatOpts; CurDEM,CurDB : integer);
var
   aClass,i,db : integer;
   Max,Lat,Long,z,MinV,MaxV,xf : float64;
   Results : tStringList;
   MomentVar : tMomentVar;
   Values : array[1..64000] of float32;
   fName : PathStr;
   WantField : ShortString;
   aGraph : tThisBaseGraph;
   rFile : file;
   v : tGraphPoint32;
begin
   with GISdb[CurDB] do begin
      WantField := 'CLASS';
      FieldRange(WantField,MinV,MaxV);
      if (StatOpts in [soTable,soGraph]) then begin
         Results := tStringList.Create;
         Results.Add('CLASS,NAME,NPTS,MEAN,STD_DEV,MIN,MAX,MEDIAN,PERC_5,PERC_10,QUANT_25,QUANT_75,PERC_90,PERC_95,COLOR');
      end
      else if (StatOpts in [soCumDist]) then begin
          aGraph := tThisBaseGraph.Create(Application);
          with aGraph,GraphDraw do begin
              VertLabel := 'Cumulative Percent';
              MinHorizAxis := 1e15;
              MaxHorizAxis := -1e15;
              HorizLabel := DEMGlb[CurDEM].AreaName;
              if MDDef.CumFreqNormAxis then VertAxisFunctionType := CumulativeNormalAxis;
              SetUpGraphForm;
              aGraph.GraphDraw.LegendList := tStringList.Create;
              aGraph.Caption := DEMGlb[CurDEM].AreaName + ' in training classes';
          end;
          Results := tStringList.Create;
          Results.Add('NAME,LINE_WIDTH,LINE_COLOR,FILENAME');
      end;

      for aClass := Round(MinV) to round(MaxV) do begin
           MyData.ApplyFilter(WantField + '=' + IntToStr(aClass));
           EmpSource.Enabled := false;
           MomentVar.NPts := 0;
           while not MyData.eof do begin
              if ValidLatLongFromTable(Lat,Long) then begin
                 inc(MomentVar.NPts);
                 if not DEMGlb[CurDEM].GetElevFromLatLongDegree(Lat,Long,Values[MomentVar.NPts]) then dec(MomentVar.NPts);
              end;
              MyData.Next;
           end;

           HeapSort(MomentVar.NPts,Values);

           if (StatOpts in [soTable,soGraph]) then begin
              Moment(Values,MomentVar,msAll);
              Results.Add(IntToStr(aClass) + ',' +  MyData.GetFieldByNameAsString('NAME') + ',' + IntToStr(MomentVar.NPts) + ',' +
                  RealToString(MomentVar.mean,-12,-2) + ',' + RealToString(MomentVar.std_dev,-12,-2) + ',' +
                  RealToString(values[1],-12,-2) + ',' + RealToString(Values[MomentVar.NPts],-12,-2) + ',' +
                  RealToString(MomentVar.Median,-12,-2) + ',' +
                  RealToString(Quantile(5,Values,MomentVar.NPts,true),-12,-2) + ',' + RealToString(Quantile(10,Values,MomentVar.NPts,true),-12,-2) + ',' +
                  RealToString(Quantile(25,Values,MomentVar.NPts,true),-12,-2) + ',' + RealToString(Quantile(75,Values,MomentVar.NPts,true),-12,-2) + ',' +
                  RealToString(Quantile(90,Values,MomentVar.NPts,true),-12,-2) + ',' + RealToString(Quantile(95,Values,MomentVar.NPts,true),-12,-2) + ',' +
                  MyData.GetFieldByNameAsString('COLOR')   );
           end
           else if (StatOpts in [soCumDist]) then with aGraph.GraphDraw do begin
              aGraph.OpenDataFile(rFile);
              for i := 1 to MomentVar.NPts do begin
                 v[1] := Values[i];
                 v[2] := (i-1) / (MomentVar.NPts-1) * 100;
                 aGraph.AddPointToDataBuffer(rfile,v[1],v[2]);
              end;
              aGraph.ClosePointDataFile(rfile);
              Petmath.CompareValueToExtremes(values[1],MinHorizAxis,MaxHorizAxis);
              Petmath.CompareValueToExtremes(values[MomentVar.NPts],MinHorizAxis,MaxHorizAxis);

              fName := aGraph.GraphDraw.DataFilesPlotted.Strings[pred(aGraph.GraphDraw.DataFilesPlotted.Count)];
              Results.Add(MyData.GetFieldByNameAsString('NAME') + ',3,' + MyData.GetFieldByNameAsString('COLOR') + ',' + fName);
              aGraph.GraphDraw.LegendList.Add(MyData.GetFieldByNameAsString('NAME'));
              aGraph.GraphDraw.FileColors256[aGraph.GraphDraw.LegendList.Count] := MyData.PlatformColorFromTable;
           end;
       end;
       if (StatOpts in [soTable,soGraph]) then begin
          fName := GISdb[CurDB].GridStatsName(CurDEM,'.csv');
          db := theMapOwner.StringListToLoadedDatabase(Results,fName);
          if (StatOpts in [soGraph]) then GISdb[db].dbTableF.Graphwithranges1Click(Nil);
       end
       else if (StatOpts in [soCumDist]) then begin
          aGraph.GraphDraw.DataFilesPlottedTable := Petmar.NextFileNumber(MDTempDir, 'prob_dist_',DefaultDBExt);
          StringList2CSVtoDB(Results,aGraph.GraphDraw.DataFilesPlottedTable,true);
          aGraph.GraphDraw.DataFilesPlotted.Clear;
          aGraph.RedrawDiagram11Click(Nil);
       end;
       ClearGISFilter;
   end;
end;


constructor tMultiGridArray.Create;
var
   i : integer;
begin
   PointGraph := Nil;
   MapOwner := Nil;
   TheFiles := Nil;
   NumGrids := 0;
   MonthlyData := false;
   FirstValidGrid := 1;
   SatImageIndex := 0;
   for i := 1 to MaxGridsInMG do Grids[i] := 0;
end;


procedure tMultiGridArray.LoadFromSingleFileGeotiff(TiffImage : tTiffImage);
var
   i,x,y,Band,NumRead : integer;
   Value : word;
   BigRow : ^tWordBigRow;
   ByteRow : ^tRow8Bit;
   fName  : pathStr;
begin
   {$IfDef RecordMultiGrids} WriteLineToDebugFile('tTIFFImage.LoadFromGeotiff in'); {$EndIf}
   with TiffImage do begin
      MainName := TiffImage.TIFFFileName;
      BasePath := ExtractFilePath(MainName) + '\multi_grids\';
      if not PathIsValid(BasePath) then SafeMakeDir(BasePath);
      NumGrids := TiffHeader.SamplesPerPixel;

      if PathIsValid(BasePath) then begin
         ReadMultigrids;
      end
      else begin
         StartProgress('Initialize');
         for I := 1 to TiffHeader.SamplesPerPixel do begin
            if (i mod 10 = 0) then UpDateProgressBar(i/TiffHeader.SamplesPerPixel);
            ReallyReadDEM := true;
            OpenDEMDataStructures(Grids[i]);
            DEMGlb[Grids[i]].DEMheader.ElevUnits := euImagery;
         end;
         {$IfDef RecordMultiGrids} WriteLineToDebugFile('tTIFFImage.OpenMultipleGrids done setup'); {$EndIf}

         StartProgress('Load');
         New(BigRow);
         New(ByteRow);
         for y := 0 to pred(TiffHeader.ImageLength) do begin
            if (y mod 50 = 0) then UpDateProgressBar(y/TiffHeader.ImageLength);
            SeekFileOffset({1,}y);
            if (TiffHeader.BitsPerSample = 8) then NumRead := SysUtils.FileRead(TiffHandle, ByteRow^,TiffHeader.ImageWidth*TiffHeader.SamplesPerPixel)
            else NumRead := SysUtils.FileRead(TiffHandle, BigRow^,2*TiffHeader.ImageWidth*TiffHeader.SamplesPerPixel);
            for x := 0 to pred(TiffHeader.ImageWidth) do begin
               for Band := 1 to TiffHeader.SamplesPerPixel do begin
                  if (TiffHeader.BitsPerSample = 8) then Value := ByteRow^[x*TiffHeader.SamplesPerPixel+ pred(Band)]
                  else begin
                     Value := BigRow^[x*TiffHeader.SamplesPerPixel+ pred(Band)];
                     if BigEndian then Value := swap(Value);
                  end;
                  DEMGlb[Grids[Band]].SetGridElevation(x,TiffHeader.ImageLength - y,Value);
               end;
            end;
         end;
         Dispose(BigRow);
         Dispose(ByteRow);
         {$IfDef RecordMultiGrids} WriteLineToDebugFile('tTIFFImage.OpenMultipleGrids done load'); {$EndIf}
         StartProgress('Check');
         for Band := 1 to NumGrids do begin
            if (Band mod 10 = 0) or (NumGrids < 25) then UpDateProgressBar(Band/NumGrids);
            DEMGlb[Grids[Band]].CheckMaxMinElev;
            fName := BasePath + ShortLandsatName(TiffImage.TIFFFileName) + '_Band_' + IntToStr(Grids[i]) + '.dem';
            DEMGlb[Grids[i]].WriteNewFormatDEM(fName);
         end;
         EndProgress;
      end;
      SaveMultigrids;
   end;
   {$IfDef RecordMultiGrids} WriteLineToDebugFile('tTIFFImage.OpenMultipleGrids out'); {$EndIf}
end;


function tMultiGridArray.BasicStats : integer;
var
   Mean,Std : float32;
   i : integer;
   Results : tStringList;
   fName : PathStr;
   ch : char;
begin
   fName := MDTempDir + MG_short_name + 'grid_stats.dbf';
   if FileExists(fName) then  begin
      Result := MapOwner.LoadDataBaseFile(fName);
   end
   else begin
      Results := tStringList.Create;
      Results.Add('BAND,MIN,MAX,MEAN,STD_DEV,USE');
      StartProgress('Stats');
      for i := 1 to MaxGridsInMG do begin
         if ValidDEM(Grids[i]) then begin
            UpdateProgressBar(i/NumGrids);
            DEMGlb[Grids[i]].ElevationStatistics(DEMGlb[Grids[i]].FullDEMGridLimits,Mean,Std);
            if (abs(DEMGlb[Grids[i]].DEMheader.MaxElev - DEMGlb[Grids[i]].DEMheader.MinElev) > 0.5) then ch := 'Y' else ch := 'N';
            Results.Add(DEMGlb[Grids[i]].AreaName + ',' + IntToStr(round(DEMGlb[Grids[i]].DEMheader.MinElev)) + ',' + IntToStr(round(DEMGlb[Grids[i]].DEMheader.MaxElev)) + ',' + RealToString(Mean,-12,-2) + ',' + RealToString(Std,-12,-2)+ ',' + ch);
         end;
      end;
      EndProgress;
      {$IfDef RecordMultiGrids} WriteStringListToDebugFile(Results); {$EndIf}
      Result := MapOwner.StringListToLoadedDatabase(Results,fName);
   end;
end;



procedure tMultiGridArray.LoadLandsatMultigrid;
var
   i,NumPts : int64;
   fName,mgName,MGPath : PathStr;


   function LoadBand(i : integer) : boolean;
   begin
       Result := false;
       if (SatImage[SatImageIndex].LandsatNumber in [4..9]) then begin
          if (i = 8) then exit;   //pan band
          if LoadAll then begin
             Result := true;
          end
          else begin
             Result := ((SatImage[SatImageIndex].LandSatNumber = 8) and (i < 9)) or  ((SatImage[SatImageIndex].LandSatNumber in [4,5,7]) and (i <> 6));
          end;
       end
       else if SatImage[SatImageIndex].IsSentinel2 then begin
          Result := (i <> 11);
       end;
   end;

        procedure ProcessBandFile(i : integer;SaveIt : boolean);
        begin
            mgName := MGPath + SatImage[SatImageIndex].BandShortName[i] + '.dem';
            if FileExists(mgName) then begin
               LoadNewDEM(Grids[i],mgName,false);
            end
            else begin
                fName := SatImage[SatImageIndex].BFileName[i];
                if FileExists(fName) then begin
                   {$IfDef RecordMultiGrids} WriteLineToDebugFile('Load ' + fName); {$EndIf}
                   LoadNewDEM(Grids[i],fName,false);
                   DEMGlb[Grids[i]].AreaName := SatImage[SatImageIndex].BandShortName[i];
                   DEMGlb[Grids[i]].DEMheader.ElevUnits := euImagery;
                   DEMGlb[Grids[i]].MarkInRangeMissing(0,0,NumPts);
                   DEMGlb[Grids[i]].CheckMaxMinElev;
                   if SaveIt then DEMGlb[Grids[i]].WriteNewFormatDEM(mgName);
                end
                else begin
                  {$IfDef RecordMultiGrids} WriteLineToDebugFile('Missing ' + fName); {$EndIf}
                end;
            end;
        end;


begin
   {$IfDef RecordMultiGrids}   WriteLineToDebugFile('tMultiGridArray.LoadLandsatMultigrid ' + BasePath); {$EndIf}
   if (SatImage[SatImageIndex].LandsatNumber in [1..9]) or IsThisSentinel2(basePath) then begin
      MGPath := SatImage[SatImageIndex].LandsatDir + 'multi_grids\';
      if not PathIsValid(MGPath) then SafeMakeDir(MGPath);

      if IsThisSentinel2(basePath) then begin
         PossGrids := 13;
         MG_Name := 'Sentinel-2';
      end
      else begin
         case SatImage[SatImageIndex].LandsatNumber of
            1..3 : PossGrids := 5;
            4,5 : PossGrids := 7;
            7   : PossGrids := 8;
            8,9 : PossGrids := 11;
         end;
         MG_Name := ShortLandsatName(SatImage[SatImageIndex].SceneBaseName);  //ShortLandsatName(ExtractFileName(SatImage[SatImageIndex].LandsatDir));
      end;
      MGPath := MGPath + MG_Name;

       for i := 1 to PossGrids do begin
         Grids[i] := 0;
         if LoadBand(i) then begin
            inc(NumGrids);
            UseBand[i] := true;
            ProcessBandFile(i,false);
         end
         else begin
            {$IfDef RecordMultiGrids} WriteLineToDebugFile('Skip ' + fName); {$EndIf}
         end;
       end;
   end;
end;


procedure tMultiGridArray.LoadHyperion;
var
   i,NumPts : int64;
   //SceneBaseName : ANSIstring;
   TStr : shortstring;
   fName,HyperionDir,mgName{,MGPath} : PathStr;
   Table : tMyData;
begin
   {$If Defined(RecordHyperion) or Defined(RecordMultiGrids)} WriteLineToDebugFile('tMultiGridArray.LoadHyperion in ' + BasePath); {$EndIf}
   {$If Defined(RecordHyperion)}Stopwatch := TStopwatch.StartNew; {$EndIf}
   HyperionDir := ExtractFilePath(BasePath);
   Table := tMyData.Create(SatBandNames);
   FirstValidGrid := 8;
   TStr := LastSubDir(BasePath);
   MG_Name := 'Hyperion ' + Copy(TStr,11,4) + ' JDay ' + Copy(TStr,15,3);
   MG_short_name := 'Hyperion ' + Copy(TStr,11,4) + ' JDay ' + Copy(TStr,15,3);
   StartProgress('Load Hyperion');
   for i := 1 to 242 do begin
      if (i mod 5 = 0) then UpdateProgressBar(i/242);
      Grids[i] := 0;

      if i in [1..7,58..76,225..242] then begin
          UseBand[i] := false;
      end
      else begin
         UseBand[i] := true;
         Table.ApplyFilter('SHORT_NAME=' + QuotedStr('HYP-' + IntToStr(i)));
         BandWavelengths[i] := Table.GetFieldByNameAsFloat('WAVE_NM');
         fName := BasePath + LastSubDir(BasePath) + '_B' + IntegerToString(i,3) +  '_L1GST.tif';
         ReplaceCharacter(fName,' ','0');
         ShowDEMReadingProgress := false;
         if FileExists(fName) then begin
            inc(NumGrids);
            inc(PossGrids);
             LoadNewDEM(Grids[i],fName,false);
             DEMGlb[Grids[i]].DEMheader.ElevUnits := euImagery;
             DEMGlb[Grids[i]].MarkInRangeMissing(0,0,NumPts);
             DEMGlb[Grids[i]].CheckMaxMinElev;
             DEMGlb[Grids[i]].AreaName := 'Band_' + IntToStr(i) + '_' + RealToString(BandWavelengths[i],-8,-2) + '_nm';
          end
          else begin
              {$If Defined(RecordHyperion)}  WriteLineToDebugFile('Missing ' + fName); {$EndIf}
          end;
      end;
      {$IfDef RecordMultiGridsDetailed} WriteLineToDebugFile('band ' + IntToStr(i) + '  in grid ' + IntToStr(Grids[i]) + '  ' + fName); {$EndIf}
   end;
   EndProgress;
   Table.Destroy;
   {$IfDef RecordHyperion} Elapsed := Stopwatch.Elapsed; WriteLineToDebugFile('Load Hyerion: ' + RealToString(Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}
   {$IfDef RecordMultiGridsDetailed} WriteLineToDebugFile('tMultiGridArray.LoadHyperion out ' + BasePath); {$EndIf}
end;


procedure tMultiGridArray.LoadFromENVI(fName : PathStr);
//http://www.exelisvis.com/docs/ENVIHeaderFiles.html for HDR file definitions
const
   MaxSize = 10000;
var
   i,x,y,Band,NumRead,Offset : integer;
   Value : word;
   BigRow : ^tWordBigRow;
   ByteRow : ^tRow8Bit;
   DEMHeader : tDEMheader;
   Header : tStringList;
   aLine,oneVal : ANSIstring;
   aValue : ANSIstring;
   Interleave : ShortString;
   IntelByteOrder : boolean;
   inf : file;
   ByteArray : array[0..MaxSize] of byte;
   WordArray : array[0..MaxSize] of Word;
   SmallIntArray : array[0..MaxSize] of int16;
   FloatArray : array[0..MaxSize] of float32;
begin
   {$IfDef RecordMultiGrids} WriteLineToDebugFile('tTIFFImage.LoadFromENVI in'); {$EndIf}

   BasePath := ExtractFilePath(fName) + '\multi_grids\';
   fName := ChangeFileExt(fName,'.hdr');
   Header := tStringList.Create;
   Header.LoadFromFile(fName);
   ZeroDEMHeader(DEMHeader,true);
   DEMHeader.ElevUnits := euImagery;
   for i := 0 to pred(Header.Count) do begin
      aline := UpperCase(ptTrim(Header.Strings[i]));
      if (i=0) and (aline <> 'ENVI') then begin
         exit;
      end;
      aValue := Petmar_types.BeforeSpecifiedCharacterANSI(aline,'=',true,true);
      aline := ptTrim(aline);
      if (avalue = 'SAMPLES') then begin
         DEMHeader.NumCol := StrToInt(aLine);
      end
      else if (avalue = 'LINES') then begin
         DEMHeader.NumRow := StrToInt(aLine);
      end
      else if (avalue = 'BANDS') then begin
         NumGrids :=  StrToInt(aLine);
      end
      else if (avalue = 'HEADER OFFSET') then begin
         Offset :=  StrToInt(aLine);
         if Offset <> 0 then MessageToContinue('Unsupported Offset ' + IntToStr(Offset));
      end
      else if (avalue = 'DATA TYPE') then begin
         x := StrToInt(aLine);
         case x of
            1 : DEMHeader.DEMPrecision := ByteDEM;
            2 : DEMHeader.DEMPrecision := SmallIntDEM;
            4 : DEMHeader.DEMPrecision := FloatingPointDEM;
            12 : DEMHeader.DEMPrecision := WordDEM;
            else begin
                MessageToContinue('Unsupported data type ' + IntToStr(x));
            end;
         end;
      end
      else if (avalue = 'INTERLEAVE') then begin
         Interleave := aLine;
         if (Interleave <> 'BSQ') then begin
            MessageToContinue('Unsupported interleave ' + aLine);
         end;
      end
      else if (avalue = 'BYTE ORDER') then begin
         IntelByteOrder := (aline = '0');
         if (not IntelByteOrder) then begin
            MessageToContinue('Unsupported byte order');
         end;
      end
      else if (avalue = 'MAP INFO') then begin
         Delete(aline,1,1);
         oneVal := Petmar_types.BeforeSpecifiedCharacterANSI(aline,',',true,true);
         oneVal := Petmar_types.BeforeSpecifiedCharacterANSI(aline,',',true,true);
         oneVal := Petmar_types.BeforeSpecifiedCharacterANSI(aline,',',true,true);
         oneVal := Petmar_types.BeforeSpecifiedCharacterANSI(aline,',',true,true);
         DEMHeader.DEMSWCornerX := StrToFloat(oneVal);
         oneVal := Petmar_types.BeforeSpecifiedCharacterANSI(aline,',',true,true);
         DEMHeader.DEMSWCornerY := StrToFloat(oneVal);
         oneVal := Petmar_types.BeforeSpecifiedCharacterANSI(aline,',',true,true);
         DEMHeader.DEMxSpacing := StrToFloat(oneVal);
         oneVal := Petmar_types.BeforeSpecifiedCharacterANSI(aline,',',true,true);
         DEMHeader.DEMySpacing := StrToFloat(oneVal);
         oneVal := Petmar_types.BeforeSpecifiedCharacterANSI(aline,',',true,true);
         DEMHeader.UTMZone := StrToInt(oneVal);
      end;
   end;
   DEMHeader.DEMSWCornerY := DEMHeader.DEMSWCornerY - pred(DEMHeader.NumRow) * DEMHeader.DEMySpacing;

   Header.Free;

   AssignFile(inf,ChangeFileExt(fName,'.IMG'));
   reset(inf,1);
   StartProgress('ENVI read');
   for I := 1 to NumGrids do begin
     UpdateProgressBar(i/NumGrids);
     OpenAndZeroNewDEM(true,DEMHeader,Grids[i],'',InitDEMnone);
     for y := 0 to pred(DEMHeader.NumRow) do begin
        if DEMHeader.DEMPrecision = ByteDEM then begin
           BlockRead(inf,ByteArray,DEMHeader.NumCol);
           for x := 0 to pred(DEMHeader.NumCol) do DEMGlb[Grids[i]].SetGridElevation(x,pred(DEMHeader.NumRow) - y,ByteArray[x]);
        end
        else if DEMHeader.DEMPrecision = SmallIntDEM then begin
           BlockRead(inf,SmallIntArray,DEMHeader.NumCol*2);
           for x := 0 to pred(DEMHeader.NumCol) do DEMGlb[Grids[i]].SetGridElevation(x,pred(DEMHeader.NumRow) - y,SmallIntArray[x]);
        end
        else if DEMHeader.DEMPrecision = FloatingPointDEM then begin
           BlockRead(inf,FloatArray,DEMHeader.NumCol*4);
           for x := 0 to pred(DEMHeader.NumCol) do DEMGlb[Grids[i]].SetGridElevation(x,pred(DEMHeader.NumRow) - y,FloatArray[x]);
        end
        else if DEMHeader.DEMPrecision = WordDEM then begin
           BlockRead(inf,WordArray,DEMHeader.NumCol*2);
           for x := 0 to pred(DEMHeader.NumCol) do DEMGlb[Grids[i]].SetGridElevation(x,pred(DEMHeader.NumRow) - y,WordArray[x]);
        end;
     end;
     DEMGlb[Grids[i]].CheckMaxMinElev;
   end;
   EndProgress;
   SaveMultigrids;
   {$IfDef RecordMultiGrids} WriteLineToDebugFile('tTIFFImage.OpenENVI out'); {$EndIf}
end;


procedure tMultiGridArray.MakeSlopeCorrelationGrids;
var
   x,y,n,Band,R_DEM,Slope_DEM : integer;
   a,b,siga,sigb,r : float32;
   Z : float32;
   xf,yf : array[1..MaxGridsInMG] of float32;
begin
   R_DEM := OpenNewGrid('r-squared',euUndefined,FloatingPointDEM);
   Slope_DEM := OpenNewGrid('slope',euUndefined,FloatingPointDEM);
   for x := 1 to NumGrids do xf[x] := x;
   for x := 0 to pred(DEMGlb[Grids[1]].DEMheader.NumCol) do begin
      if (x mod 50 = 0) then UpDateProgressBar(x/DEMGlb[Grids[1]].DEMheader.NumCol);
      for y := 0 to pred(DEMGlb[Grids[1]].DEMheader.NumRow) do begin
          if DEMGlb[Grids[1]].MissingDataInGrid(x,y) then begin
          end
          else begin
             for Band := 1 to NumGrids do begin
                if DEMGlb[Grids[Band]].GetElevMeters(x,y,z) then begin
                   yf[Band] := z;
                end;
             end;
             PetMath.Fit(xf,yf,NumGrids, a,b,siga,sigb,r);
             DEMGlb[R_DEM].SetGridElevation(x,y,sqr(r));
             DEMGlb[Slope_DEM].SetGridElevation(x,y,b);
          end;
      end;
   end;
   DisplayNewGrid(R_DEM);
   DisplayNewGrid(Slope_DEM);
end;


procedure tMultiGridArray.MultiplyGrids;
var
   x,y,n,Band : integer;
   Mult,Z : float32;
begin
   StartProgress('Multiply ' + MG_Name);
   Mult := 3;
   ReadDefault('multiply every point by',Mult);
   for Band := 1 to NumGrids do begin
      UpDateProgressBar(Band/NumGrids);
      for x := 0 to pred(DEMGlb[Grids[Band]].DEMheader.NumCol) do begin
         for y := 0 to pred(DEMGlb[Grids[1]].DEMheader.NumRow) do begin
             if DEMGlb[Grids[Band]].GetElevMeters(x,y,z) then begin
                DEMGlb[Grids[Band]].SetGridElevation(x,y,z * Mult);
             end;
         end;
      end;
      DEMGlb[Grids[Band]].CheckMaxMinElev;
      if UpperCase(ExtractFileExt(DEMGlb[Grids[Band]].DEMFileName)) = '.DEM' then
         DEMGlb[Grids[Band]].WriteNewFormatDEM(DEMGlb[Grids[Band]].DEMFileName)
      else DEMGlb[Grids[Band]].SaveAsGeotiff(DEMGlb[Grids[Band]].DEMFileName);
   end;
   EndProgress;
end;



procedure tMultiGridArray.ZeroAsMissing(MissingValue : float64; Save : boolean = true);
var
   x,y,n,Band : integer;
   Z : float32;
begin
   StartProgress('Zero to missing ' + MG_Name);
   for Band := 1 to NumGrids do begin
      UpDateProgressBar(Band/NumGrids);
      for x := 0 to pred(DEMGlb[Grids[Band]].DEMheader.NumCol) do begin
         for y := 0 to pred(DEMGlb[Grids[1]].DEMheader.NumRow) do begin
             if DEMGlb[Grids[Band]].GetElevMeters(x,y,z) then begin
                if abs(z-MissingValue) < 0.001 then DEMGlb[Grids[Band]].SetGridMissing(x,y);
             end;
         end;
      end;
      DEMGlb[Grids[Band]].CheckMaxMinElev;
      if Save then begin
         if UpperCase(ExtractFileExt(DEMGlb[Grids[Band]].DEMFileName)) = '.DEM' then
            DEMGlb[Grids[Band]].WriteNewFormatDEM(DEMGlb[Grids[Band]].DEMFileName)
         else DEMGlb[Grids[Band]].SaveAsGeotiff(DEMGlb[Grids[Band]].DEMFileName);
      end;
   end;
   EndProgress;
end;


procedure tMultiGridArray.ReadMultigrids;
var
   i : integer;
   fName,BaseName : PathStr;
begin
   {$IfDef RecordMultiGrids} WriteLineToDebugFile('tMultiGridArray.ReadMultigrids in'); {$EndIf}
   MG_Name := Petmar.LastSubDir(BasePath);

   if TheFiles = Nil then begin
      Petmar.FindMatchingFiles(BasePath,'*.dem',TheFiles);
      if (TheFiles.Count = 0) then begin
         Petmar.FindMatchingFiles(BasePath + 'multi_grids\','*.dem',TheFiles);
      end;
      if (TheFiles.Count = 0) then Petmar.FindMatchingFiles(BasePath,'*.tif',TheFiles);
   end;

   {$IfDef RecordMultiGrids} WriteLineToDebugFile('tMultiGridArray.ReadMultigrids found grids=' + IntToStr(TheFiles.Count)); {$EndIf}

   if (TheFiles.Count > 0) then begin
      NumGrids := TheFiles.Count;
      BaseName := ChangeFileExt(TheFiles.Strings[0],'');
      while BaseName[length(BaseName)] in ['0'..'9'] do Delete(BaseName,length(BaseName),1);

      for i := 1 to NumGrids do begin
         if (i mod 10 = 0) or (NumGrids < 25) then UpDateProgressBar(i/NumGrids);
         fName := BaseName + IntToStr(i) + '.dem';
         if not FileExists(fName) then fName := TheFiles.Strings[pred(i)];
         LoadNewDEM(Grids[i],fName,false);
      end;

      fName := System.IOUtils.TPath.Combine(BasePath, 'wavelengths.txt');
      if FileExists(fName) then begin
         TheFiles.Clear;
         TheFiles.LoadFromFile(fName);
         for i := 0 to pred(TheFiles.Count) do begin
            BandWavelengths[succ(i)] := StrToFloat(ptTrim(TheFiles.Strings[i]));
         end;
      end;
   end;
   TheFiles.Free;
   {$IfDef RecordMultiGrids} WriteLineToDebugFile('tMultiGridArray.ReadMultigrids out'); {$EndIf}
end;


procedure tMultiGridArray.LoadMultigridsFromStringList(TheFilesSL : tStringList);
var
   i : integer;
   fName,BaseName : PathStr;
begin
   {$IfDef RecordMultiGrids} WriteLineToDebugFile('tMultiGridArray.LoadMultigrids in'); {$EndIf}
   HeavyDutyProcessing := true;
   NumGrids := TheFilesSL.Count;
   if (TheFilesSL.Count > 0) then begin
      TheFiles := tStringList.Create;
      for i := 1 to NumGrids do begin
         if (i mod 10 = 0) or (NumGrids < 25) then UpDateProgressBar(i/NumGrids);
         fName := TheFilesSL.Strings[pred(i)];
         TheFiles.Add(fName);
         if not FileExists(fName) then fName := ChangeFileExt(fName,'.dem');
         DEMNowDoing := Calculating;
         LoadNewDEM(Grids[i],fName,false);
      end;
   end;
   TheFiles.Free;
   DEMNowDoing := JustWandering;
   HeavyDutyProcessing := false;
   {$IfDef RecordMultiGrids} WriteLineToDebugFile('tMultiGridArray.LoadMultigrids out, NumGrids =' + IntToStr(NumGrids) ); {$EndIf}
end;


procedure tMultiGridArray.SaveMultigrids;
var
   i : integer;
   fName : PathStr;
begin
   if not PathIsValid(BasePath+ 'multi_grids\') then SafeMakeDir(BasePath+ 'multi_grids\');
   StartProgress('Write multigrids');
   for i := 1 to NumGrids do begin
      UpDateProgressBar(i/NumGrids);
      fName := BasePath + 'multi_grids\Band_' + IntToStr(Grids[i]) + '.dem';
      DEMGlb[Grids[i]].WriteNewFormatDEM(fName);
   end;
   EndProgress;
end;


procedure tMultiGridArray.NormalizeGrids;
var
   x,y,n,Band : integer;
   fName : PathStr;
   Max,Z : float32;
begin
   StartProgress('Normalize_' + MG_Name);
   for x := 0 to pred(DEMGlb[Grids[1]].DEMheader.NumCol) do begin
      if (x mod 500 = 0) then UpDateProgressBar(x/DEMGlb[Grids[1]].DEMheader.NumCol);
      for y := 0 to pred(DEMGlb[Grids[1]].DEMheader.NumRow) do begin
          Max := 0;
          for Band := 1 to NumGrids do begin
             if DEMGlb[Grids[Band]].GetElevMeters(x,y,z) then begin
                if (z > Max) then Max := z;
             end;
          end;
          Max := 32000 / Max;
          for Band := 1 to NumGrids do begin
             if DEMGlb[Grids[Band]].GetElevMeters(x,y,z) then begin
                DEMGlb[Grids[Band]].SetGridElevation(x,y,z * Max);
             end;
          end;
      end;
   end;
   SafeMakeDir(BasePath + 'norm_grids\');
   for Band := 1 to NumGrids do begin
      if (x mod 500 = 0) then UpDateProgressBar(Band/NumGrids);
      fName := BasePath + 'norm_grids\Band_' + IntToStr(Band) + '.dem';
      DEMGlb[Grids[Band]].WriteNewFormatDEM(fName);
   end;
   EndProgress;
end;

(*
procedure tMultiGridArray.CloudBrighten;
var
   x,y,n,Band,CloudDEM : integer;
   fName : PathStr;
   Z : float64;
   Table : tMyData;
   BandMults : array[1..MaxBands] of float64;
begin
   fName := ExtractFilePath(MainName) + 'cloud_mask.dem';
   if not FileExists(fName) then begin
       MessageToContinue('Missing ' + fName);
       exit;
   end;
   LoadNewDEM(CloudDEM,fName,false);

   fName := ExtractFilePath(MainName) + 'cloud_shading.dbf';
   if not FileExists(fName) then begin
       MessageToContinue('Missing ' + fName);
       exit;
   end;

   Table := tMyData.Create(fName);
   while not Table.eof do begin
       BandMults[Table.GetFieldByNameAsInteger('BAND')] := Table.GetFieldByNameAsFloat('SHADING');
       Table.Next;
   end;
   Table.Destroy;

   StartProgress('Cloud brighten');
   for x := 0 to pred(DEMGlb[Grids[1]].DEMheader.NumCol) do begin
      if (x mod 10 = 0) then UpDateProgressBar(x/DEMGlb[Grids[1]].DEMheader.NumCol);
      for y := 0 to pred(DEMGlb[Grids[1]].DEMheader.NumRow) do begin
          if DEMGlb[CloudDEM].GetElevMeters(x,y,z) then begin
            for Band := 1 to NumGrids do begin
               if DEMGlb[Grids[Band]].GetElevMeters(x,y,z) then begin
                  DEMGlb[Grids[Band]].SetGridElevation(x,y,z /BandMults[Band]);
               end;
            end;
          end;
      end;
   end;

   StartProgress('Save');
   BasePath := ExtractFilePath(MainName) + '\cloud_free_grids\';
   SafeMakeDir(BasePath);
   for Band := 1 to NumGrids do begin
      if (Band mod 10 = 0) then UpDateProgressBar(Band/NumGrids);
      fName := BasePath + 'Band_' + IntToStr(Band) + '.dem';
      DEMGlb[Grids[Band]].WriteNewFormatDEM(fName);
   end;
   EndProgress;
   CloseSingleDEM(CloudDEM);
end;
*)

function tMultiGridArray.OpenNewGrid(theName : ShortString; zUnits : tElevUnit; Precision : tDEMprecision) : integer;
var
   NewHeader : tDEMheader;
begin
   NewHeader := DEMGlb[Grids[1]].DEMheader;
   NewHeader.DEMPrecision := Precision;
   NewHeader.ElevUnits := zUnits;
   OpenAndZeroNewDEM(true,NewHeader,Result,ptTrim(theName),InitDEMnone);
   DEMGlb[Result].DefineDEMVariables(true);
   StartProgress(theName);
end;


procedure tMultiGridArray.DisplayNewGrid(NewDEM : integer);
begin
   DEMGlb[NewDEM].SetUpMap(NewDEM,true,mtElevSpectrum);
end;


procedure tMultiGridArray.ReinterpolateAllToSameResolution;
var
   Band,BaseBand,NewGrid : integer;
   Resolution : float64;
   //fName : PathStr;
begin
   {$IfDef RecordMultiGridsDetailed} WriteLineToDebugFile('tMultiGridArray.ReinterpolateAllToSameResolution in'); {$EndIf}
   Resolution := 9999;
   for Band := 1 to MaxGridsInMG do begin
      if ValidDEM(Grids[Band]) then begin
          if (DEMGlb[Grids[Band]].DEMheader.DEMySpacing < Resolution) then begin
             Resolution := DEMGlb[Grids[Band]].DEMheader.DEMySpacing;
             BaseBand := Band;
          end;
      end;
   end;
   {$IfDef RecordMultiGridsDetailed} WriteLineToDebugFile('tMultiGridArray.ReinterpolateAllToSameResolution, ' + RealToString(Resolution,-12,-2)); {$EndIf}
   for Band := 1 to MaxGridsInMG do begin
      if ValidDEM(Grids[Band]) then begin
         if abs(DEMGlb[Grids[Band]].DEMheader.DEMySpacing - Resolution) < 0.01 then begin
            {$IfDef RecordMultiGridsDetailed} WriteLineToDebugFile('Resolution good on  ' + ExtractFileName(DEMGlb[Grids[Band]].DEMFileName)); {$EndIf}
         end
         else begin
            NewGrid := DEMGlb[Grids[Band]].ReinterpolateUTMDEM(Resolution,-99,DEMGlb[Grids[Band]].DEMFileName);
            //DEMGlb[Grids[Band]].FreeDEMMemory;
            DEMGlb[Grids[Band]].ReloadDEM(true);
            {$IfDef RecordMultiGridsDetailed} WriteLineToDebugFile('Reinterpolated ' + ExtractFileName(DEMGlb[Grids[Band]].DEMFileName)); {$EndIf}
         end;
      end
      else begin
         {$IfDef RecordMultiGridsDetailed} WriteLineToDebugFile('Invalid, band=' + IntToStr(Band)); {$EndIf}
      end;
   end;
   {$IfDef RecordMultiGridsDetailed} WriteLineToDebugFile('ReinterpolateAllToSameResolution out'); {$EndIf}
end;


procedure tMultiGridArray.CreateMaxValueGrid;
begin
   CreateGrids(0,1,0);
end;


procedure tMultiGridArray.CreateMinValueGrid;
begin
   CreateGrids(1,0,0);
end;


procedure tMultiGridArray.CreateMinMaxRange;
begin
   CreateGrids(1,1,1);
end;


procedure tMultiGridArray.CreateGrids(MinDEM,MaxDEM,RangeDEM : integer);
var
   x,y,n,Band : integer;
   Z,Min,Max : float32;
begin
   if (MinDEM <> 0) then MinDEM := OpenNewGrid('Min_value_' + MG_Name,DEMGlb[Grids[1]].DEMheader.ElevUnits,FloatingPointDEM);
   if (MaxDEM <> 0) then MaxDEM := OpenNewGrid('Max_value_' + MG_Name,DEMGlb[Grids[1]].DEMheader.ElevUnits,FloatingPointDEM);
   if (RangeDEM <> 0) then RangeDEM := OpenNewGrid('Range_value_' + MG_Name,DEMGlb[Grids[1]].DEMheader.ElevUnits,FloatingPointDEM);
   for x := 0 to pred(DEMGlb[Grids[1]].DEMheader.NumCol) do begin
      if (x mod 50 = 0) then UpDateProgressBar(x/DEMGlb[Grids[1]].DEMheader.NumCol);
      for y := 0 to pred(DEMGlb[Grids[1]].DEMheader.NumRow) do begin
          Min := 99e39;
          Max := -99e39;
          N := 0;
          for Band := 1 to NumGrids do begin
             if DEMGlb[Grids[Band]].GetElevMeters(x,y,z) then begin
                CompareValueToExtremes(z,Min,Max);
                inc(n);
             end;
          end;
          if (n > 0) then begin
             if (MinDEM <> 0) then DEMGlb[MinDEM].SetGridElevation(x,y,Min);
             if (MaxDEM <> 0) then DEMGlb[MaxDEM].SetGridElevation(x,y,Max);
             if (RangeDEM <> 0) then DEMGlb[RangeDEM].SetGridElevation(x,y,Max-Min);
          end;
      end;
   end;
   if (MinDEM <> 0) then DisplayNewGrid(MinDEM);
   if (MaxDEM <> 0) then DisplayNewGrid(MaxDEM);
   if (RangeDEM <> 0) then DisplayNewGrid(RangeDEM);
end;


procedure tMultiGridArray.CreateMinMaxMonthGrids;
var
   x,y,n,Band,MinDEM,MaxDEM : integer;
   MinMonth,MaxMonth : byte;
   Z,Min,Max : float32;
begin
   MinDEM := OpenNewGrid('Month_min_' + MG_Name,DEMGlb[Grids[1]].DEMheader.ElevUnits,ByteDEM);
   MaxDEM := OpenNewGrid('Month_max_' + MG_Name,DEMGlb[Grids[1]].DEMheader.ElevUnits,ByteDEM);
   for x := 0 to pred(DEMGlb[Grids[1]].DEMheader.NumCol) do begin
      if (x mod 50 = 0) then UpDateProgressBar(x/DEMGlb[Grids[1]].DEMheader.NumCol);
      for y := 0 to pred(DEMGlb[Grids[1]].DEMheader.NumRow) do begin
          Min := 99e39;
          Max := -99e39;
          N := 0;
          for Band := 1 to NumGrids do begin
             if DEMGlb[Grids[Band]].GetElevMeters(x,y,z) then begin
                if z < Min then begin
                   Min := z;
                   MinMonth := Band;
                end;
                if z > Max then begin
                   Max := z;
                   MaxMonth := Band;
                end;
                inc(n);
             end;
          end;
          if (n > 0) then begin
             DEMGlb[MinDEM].SetGridElevation(x,y,MinMonth);
             DEMGlb[MaxDEM].SetGridElevation(x,y,MaxMonth);
          end;
      end;
   end;
   DisplayNewGrid(MinDEM);
   DisplayNewGrid(MaxDEM);
end;


procedure tMultiGridArray.CreateAverageValueGrid;
begin
   SumOrAverageGrid(0,1,0);
end;

procedure tMultiGridArray.SumGrids;
begin
   SumOrAverageGrid(1,0,0);
end;


function tMultiGridArray.MedianGrids(OpenMap : boolean) : integer;
var
   x,y,n,Band : integer;
   Z : float32;
   Values : array[1..MaxGridsInMG] of float32;
begin
   Result := OpenNewGrid('Median_value_' + MG_Name,DEMGlb[Grids[1]].DEMheader.ElevUnits,FloatingPointDEM);
   for x := 0 to pred(DEMGlb[Grids[1]].DEMheader.NumCol) do begin
      if (x mod 50 = 0) then UpDateProgressBar(x/DEMGlb[Grids[1]].DEMheader.NumCol);
      for y := 0 to pred(DEMGlb[Grids[1]].DEMheader.NumRow) do begin
          n := 0;
          for Band := 1 to NumGrids do begin
             if DEMGlb[Grids[Band]].GetElevMeters(x,y,z) then begin
                inc(n);
                Values[n] := z;
             end;
          end;
          if (n > 0) then begin
             z := Petmath.Median(Values,n);
             DEMGlb[Result].SetGridElevation(x,y,z);
          end;
      end;
   end;
   If OpenMap then DisplayNewGrid(Result);
end;


procedure tMultiGridArray.SumOrAverageGrid(SumGrid,AvgGrid,MedianGrid : integer);
var
   x,y,n,Band : integer;
   Sum : float64;
   z : float32;
   //Values : array[1..MaxGridsInMG] of float32;
begin
   If (SumGrid <> 0) then SumGrid := OpenNewGrid('Sum_' + MG_Name,DEMGlb[Grids[1]].DEMheader.ElevUnits,FloatingPointDEM);
   If (AvgGrid <> 0) then AvgGrid := OpenNewGrid('Average_value_' + MG_Name,DEMGlb[Grids[1]].DEMheader.ElevUnits,FloatingPointDEM);
   for x := 0 to pred(DEMGlb[Grids[1]].DEMheader.NumCol) do begin
      if (x mod 50 = 0) then UpDateProgressBar(x/DEMGlb[Grids[1]].DEMheader.NumCol);
      for y := 0 to pred(DEMGlb[Grids[1]].DEMheader.NumRow) do begin
          Sum := 0;
          n := 0;
          for Band := 1 to NumGrids do begin
             if DEMGlb[Grids[Band]].GetElevMeters(x,y,z) then begin
                Sum := Sum + z;
                inc(n);
             end;
          end;
          if (n > 0) then begin
             If (SumGrid <> 0) then DEMGlb[SumGrid].SetGridElevation(x,y,Sum);
             If (AvgGrid <> 0) then DEMGlb[AvgGrid].SetGridElevation(x,y,Sum/n);
          end;
      end;
   end;
   If (SumGrid <> 0) then DisplayNewGrid(SumGrid);
   If (AvgGrid <> 0) then DisplayNewGrid(AvgGrid);
end;


function tMultiGridArray.CreateAverageReflectanceGraph(xlo,ylo,xhi,yhi : integer) : tThisBaseGraph;
var
   x,y,n,Band : integer;
   TStr : shortstring;
   Sum : float64;
   z : float32;
   Results : tStringList;
begin
   {$IfDef RecordMultiGridGraph} WriteLineToDebugFile(' tMultiGridArray.CreateAverageReflectanceGraph in' + '  low corner=' + IntToStr(xlo) + ',' + IntToStr(ylo) + '  hi  corner=' + IntToStr(xhi) + ',' + IntToStr(xhi)); {$EndIf}
   StartProgress('Graph');
   Results := tStringList.Create;
   if (xlo=xhi) and (ylo=yhi) then TStr := 'Point DN' else TStr := 'Mean DN';

   Results.Add('WAVELENGTH,' + TStr);
   for Band := 1 to NumGrids do begin
      if ValidDEM(Grids[Band]) then begin
         UpDateProgressBar(Band/NumGrids);
         Sum := 0;
         n := 0;
         for x := xlo to xhi do begin
            for y := ylo to yhi do begin
              if DEMGlb[Grids[Band]].GetElevMeters(x,y,z) then begin
                 Sum := Sum + z;
                 inc(n);
              end;
            end;
         end;
         if (n > 0) then begin
            Results.Add(RealToString(BandWavelengths[Band],-12,-2) + ',' + RealToString(Sum/n,-12,-2));
         end;
      end;
   end;
   EndProgress;
   Result := GraphFromCSVfile(Results,false,false);
   {$IfDef RecordMultiGridGraph} WriteLineToDebugFile(' tMultiGridArray.CreateAverageReflectanceGraph out'); {$EndIf}
end;



procedure tMultiGridArray.FindHighValues(LowLimit,HighLimit : integer; MapOwner : tMapForm);
label
   Done;
var
   x,y,Band,Band2{,n} : integer;
   Z : float32;
   Lat,Long : float64;
   Line : ANSIstring;
   Results : tStringList;
   fName : PathStr;
begin
   Results := tStringList.Create;
   if True then begin
      Line := 'LAT,LONG,BAND,DN';
   end
   else begin
      Line := 'LAT,LONG';
      for Band := 1 to NumGrids do Line := Line + ',BAND_' + IntToStr(Band);
   end;
    Results.Add(Line);
    StartProgress('Range');
   for x := 0 to pred(DEMGlb[Grids[1]].DEMheader.NumCol) do begin
      if (x mod 50 = 0) then UpDateProgressBar(x/DEMGlb[Grids[1]].DEMheader.NumCol);
      for y := 0 to pred(DEMGlb[Grids[1]].DEMheader.NumRow) do begin
          for Band := 1 to NumGrids do begin
             if DEMGlb[Grids[Band]].GetElevMeters(x,y,z) then begin
                if (z >= LowLimit) and (z <= HighLimit) then begin
                   DEMGlb[Grids[Band]].DEMGridToLatLongDegree(x,y,Lat,Long);
                   if True then begin
                      Results.Add(RealToString(Lat,-12,-8) + ',' + RealToString(Long,-12,-8) + ',' + IntToStr(Band) + ',' + IntToStr(round(z)));
                   end
                   else begin
                      Line := RealToString(Lat,-12,-8) + ',' + RealToString(Long,-12,-8);
                      for Band2 := 1 to NumGrids do begin
                         DEMGlb[Grids[Band2]].GetElevMeters(x,y,z);
                         Line := Line + ',' + IntToStr(round(z));
                      end;
                      Results.Add(Line);
                      goto Done;
                  end;
                end;
             end;
          end;
          Done:;
      end;
   end;
   EndProgress;
   fName := MDTempDir + 'extremes.csv';
   Results.SaveToFile(fName);
   MapOwner.LoadDataBaseFile(fName);
   Results.Free;
end;


destructor tMultiGridArray.Destroy;
var
   i : integer;
begin
   {$IfDef RecordCloseMultiGrids} WriteLineToDebugFile('tMultiGridArray.Destroy in'); {$EndIf}
   SkipMenuUpdating := true;
   for i := 1 to NumGrids do begin
      if ValidDEM(Grids[i]) then begin
         DEMGlb[Grids[i]].SelectionMap := nil;
         CloseSingleDEM(Grids[i]);
      end;
   end;
   if (PointGraph <> Nil) then begin
      PointGraph.CanCloseGraph := true;
      PointGraph.Destroy;
   end;
   SkipMenuUpdating := false;
   WmDem.SetMenusForVersion;
   {$IfDef RecordCloseMultiGrids} WriteLineToDebugFile('tMultiGridArray.Destroy out'); {$EndIf}
end;


procedure tMultiGridArray.KeyPercentiles(Bands : boolean = true);
var
   i : integer;
   fName : PathStr;
   Results : tStringList;
   TStr : shortString;
begin
   Results := tStringList.Create;
   if Bands then TStr := 'BAND' else TStr := 'GRID';

   Results.Add(TStr + ',MIN,PC_0_1,PC_1,PC_50,PC_75,PC_95,PC_99,PC_99_9,MAX');
   StartProgress('Percentiles');
   for i := 1 to NumGrids do with DEMGlb[Grids[i]] do begin
      if (i mod 10 = 0) then UpdateProgressBar(i/NumGrids);
      if Bands then TStr := IntToStr(i) else TStr := DEMGlb[Grids[i]].AreaName;
      Results.Add( TStr + ',' +  RealToString(DEMheader.MinElev,-18,-4)  + ',' +
           RealToString(FindPercentileElevation(0.1),-18,-4)  + ',' +
           RealToString(FindPercentileElevation(1),-18,-4)  + ',' +
           RealToString(FindPercentileElevation(50),-18,-4)  + ',' +
           RealToString(FindPercentileElevation(75),-18,-4)  + ',' +
           RealToString(FindPercentileElevation(95),-18,-4)  + ',' +
           RealToString(FindPercentileElevation(99),-18,-4)  + ',' +
           RealToString(FindPercentileElevation(99.9),-18,-4)  + ',' +
           RealToString(DEMheader.MaxElev,-18,-4) );
   end;
   fName := MDTempDir + 'key_percentiles.csv';
   StringList2CSVtoDB(Results,fName);
   //OpenDataBase(fName);
   EndProgress;
end;


function tMultiGridArray.BandAxisValue(Band : integer) : float64;
begin
    if MDDef.BandsByWavelength and (BandWavelengths[Band] > -9) then Result := BandWavelengths[Band]
    else Result := Band;
end;


procedure tMultiGridArray.DrawPointGraph(var PointGraph : tThisBaseGraph; Lat,Long : float64; Redraw : boolean = true);
var
   //Col,Row,
   xg,yg{,Band} : integer;

   procedure MakeGraphFile(x1,y1 : integer; Color : tColor);
   var
      Band : integer;
      Refs : array[1..500] of float32;
      rfile : file;
      Max{,x} : float32;
   begin
       PointGraph.OpenDataFile(rfile,Color);
       for Band := 1 to MaxGridsInMG do begin
          if ValidDEM(Grids[Band]) then begin
             DEMGlb[Grids[Band]].GetElevMetersOnGrid(x1,y1,Refs[Band]);
          end;
       end;
       if MDDef.SatMultiBandNormalize then begin
          Max := Refs[1];
          for Band := 2 to NumGrids do if Refs[Band] > Max then Max := Refs[Band];
          for Band := 1 to NumGrids do Refs[Band] := 100 * Refs[Band] / Max;
       end;
       for Band := 1 to MaxGridsInMG do begin
          if ValidDEM(Grids[Band]) then begin
             PointGraph.AddPointToDataBuffer(rfile,BandAxisValue(Band),Refs[Band]);
          end;
       end;
       PointGraph.ClosePointDataFile(rfile);
   end;

begin
   if (PointGraph = Nil) then begin
      PointGraph := tThisBaseGraph.Create(Application);
      //PointGraph.CanCloseGraph := true;
      PointGraph.GraphDraw.LegendList := tStringList.Create;
      if MDDef.BandsByWavelength and (BandWavelengths[1] > -9) then PointGraph.GraphDraw.HorizLabel := 'Wavelength (nm)'
      else PointGraph.GraphDraw.HorizLabel := 'Band';
      PointGraph.GraphDraw.VertLabel := 'Reflectance';
   end;

   DEMGlb[Grids[FirstValidGrid]].LatLongDegreeToDEMGridInteger(Lat,Long,xg,yg);
   if DEMGlb[Grids[FirstValidGrid]].GridInDataSet(xg,yg) then begin
       MakeGraphFile(xg,yg,-1);
       if Redraw then begin
          PointGraph.Caption := LatLongDegreeToString(Lat,Long,MDDef.OutPutLatLongMethod) + '  col=' + IntToStr(xg) + '  row=' + IntToStr(yg);
          PointGraph.GraphDraw.LegendList.Add(PointGraph.Caption);
          PointGraph.AutoScaleAndRedrawDiagram;
       end;
   end;
end;


{ tLidarMatchedGrids }

constructor tLidarMatchedGrids.Create(i : integer; TheDir : PathStr = '');
begin
   OpenFromDirectory(TheDir);
end;


destructor tLidarMatchedGrids.Destroy;
begin
   CloseSingleDEM(DSMgrid);
   CloseSingleDEM(DTMgrid);
   CloseSingleDEM(NVSgrid);
   CloseSingleDEM(IntensityGrid);
   CloseSingleDEM(RGBgrid);
   CloseSingleDEM(NIRgrid);
   CloseSingleDEM(ClassGrid);
   CloseSingleDEM(CHMGrid);
   CloseSingleDEM(ChangeGrid);
end;


procedure tLidarMatchedGrids.OpenFromDirectory(TheDir : PathStr  = '');
var
   TheFiles : tStringList;
   FirstDEM : integer;

   function TryOne(keywords : shortString) : integer;
   var
      i : integer;
      fName : PathStr;
   begin
      Result := 0;
      for I := 0 to PRED(TheFiles.Count) do begin
         fName := TheFiles.Strings[i];
         if UpperCase(Copy(ExtractFileName(fName),1,length(keywords))) = Keywords then begin
            LoadNewDEM(Result,fName,false);
            if (FirstDEM = 0) then FirstDEM := result;
            exit;
         end;
      end;
   end;

begin
   if PathIsValid(TheDir) then LastLidarMulti := TheDir
   else begin
      if LastLidarMulti = '' then LastLidarMulti := MainMapData;
      GetDOSPath('Lidar grids',LastLidarMulti);
   end;
   TheFiles := Nil;
   FirstDEM := 0;
   MatchName := LastSubDir(LastLidarMulti);
   Petmar.FindMatchingFiles(LastLidarMulti,'*.dem',TheFiles);
   DSMgrid := TryOne('DSM');
   DTMgrid := TryOne('DTM');
   NVSgrid := TryOne('NVS');
   IntensityGrid := TryOne('LIDAR_MAX_INTENSITY');
   RGBgrid := TryOne('RGB');
   NIRgrid := TryOne('NIR');
   ClassGrid := TryOne('CLASSIFICATION');
   CHMGrid := TryOne('CHM');
   ChangeGrid := TryOne('CHANGE');
   CreateDEMSelectionMap(FirstDEM,false);
   MapOwner := DEMGlb[FirstDem].SelectionMap;
   MapOwner.MapDraw.MapType := MDDef.DefLidarElevMap;
   MapOwner.DrawColoredMap1Click(Nil);
 end;



var
   i : integer;
initialization
   for i := 1 to MaxMultiGrid do MultiGridArray[i] := nil;
   for i := 1 to MaxMontlyDBArray do MonthlyDBArray[i] := nil;
   for i := 1 to MaxLMG do lmg[i] := nil;
finalization
   {$IfDef RecordMultiGrids} WriteLineToDebugFile('RecordMultiGrids in multigrid');   {$EndIf}
   {$IfDef RecordCloseMultiGrids} WriteLineToDebugFile('RecordCloseMultiGrids in multigrid'); {$EndIf}
   {$IfDef RecordSatClass} WriteLineToDebugFile('RecordSatClass in multigrid'); {$EndIf}
   {$IfDef RecordClosingProblems} WriteLineToDebugFile('Closing multigrid out'); {$EndIf}
end.
