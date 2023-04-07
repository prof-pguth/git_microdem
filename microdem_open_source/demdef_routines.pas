unit demdef_routines;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of ianMICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2023 Peter L. Guth   }
{____________________________________}


{$I nevadia_defines.inc}


//{$Define ConvertDBFtoDB}


{$IfDef RecordProblems}  //normally only defined for debugging specific problems
   {$IFDEF DEBUG}
      //{$Define RecordParallelLoops}
      //{$Define RecordUseOtherPrograms}
      //{$Define RecordFan}
      //{$Define RecordUpdate}
      //{$Define RecordDirs}
      //{$Define RecordLoadDefault}
      //{$Define RecordFindUTM}
      //{$Define RecordGDAL}
      //{$Define RecordNaturalEarthFileNames}
      //{$Define RecordInitialization}
      //{$IfDef RecordInitializationDetailed}
      //{$Define RecordINIfiles}
      //{$Define Options}
      //{$Define RecordDetailedStartup}
      //{$Define RecordProjects}
      //{$Define RecordFont}
      //{$Define RecordDefaultColors}
      //{$Define RecordDefault}
      //{$Define MessageStartup}
      //{$Define RecordFanConversions}
      //{$Define RecordTerrainCategories}
      //{$Define RecordFont}
      //{$Define RecordGazOps}
      //{$Define RecordPath}
      //{$Define RecordWebDownloads}
      //{$Define ShowMenuLimits}
      //{$Define RecordProgramMode}
      //{$Define RecordAtLeastPartOfBoxInAnotherBox}
      //{$Define RecordPointInBox}
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

   {$IfDef UseBDETables}
      dbTables,
   {$EndIf}

   {$IfDef UseTDBF}
      dbf,
   {$EndIf}

   {$IfDef UseTCLientDataSet}
      DBClient,
   {$EndIf}
//end needed for inline core DB functions

   System.IOutils,System.UITypes,System.Math,System.UIConsts,
   SysUtils, Classes,StrUtils,

   {$IfDef MSWindows}
      Windows,Messages,
   {$EndIf}

   {$IfDef VCL}
      Forms, Graphics,ExtCtrls,Grids,Controls,
   {$EndIf}

   {$IfDef ExDatum}
   {$Else}
      BaseMap,
   {$EndIf}

   {$IfDef NoClustering}
   {$Else}
      MVClusterClientDataSet,
   {$EndIf}
   petmar,Petmar_types,DEMDefs,petmar_ini_file;

type
   tGridLimitsArray  = array[1..MaxThreadsAllowed] of tGridLimits;
var
   GlobalDRGMap : boolean;

function IsThisATIGERshapefile(fName : PathStr) : boolean;
function CheckIfTigerOrOSM(fName : PathStr) : boolean;

{$IfDef ExOSM}
{$Else}
   function IsThisAnOSMshapefile(fName : PathStr) : boolean;
{$EndIf}

function CurvAlgName(Alg : tVerticalCurvAlg) : ShortString;
function DirectionName(Dir : DEMDefs.tCompassDirection) : shortstring;
function SlopeMethodName(Method : byte) : shortstring;
function ShortSlopeMethodName(Method : byte) : shortstring;

function PointTypeName(PointType : tPointType) : ShortString;
function PointTypeColor(PointType : tPointType) : tColor;
function IntervisiblityAlgorithmName(iva :  tIntervisibilityAlgorithm) : ShortString;
function LOSAlgorithmDescription(iva :  tIntervisibilityAlgorithm) : ShortString;

function StringToTerrainCategory(str : ShortString) : tTerrainCatDefinition;
function TerrainCategoryToString(tc : tTerrainCatDefinition) : ShortString;
function SpeedString(MetersPerSec : float64) : ShortString;

function IsReflectanceMap(MapType : tMapType) : boolean;
function IsSlopeMap(MapType : tMapType) : boolean;
function IsElevationMap(MapType : tMapType) : boolean;
function IsOtherGridMap(MapType : tMapType) : boolean;

function IsSatelliteMap(MapType : tMapType) : boolean;
function IsSatelliteColorImage(MapType : tMapType) : boolean;

procedure SetReflectanceDefaults;
procedure SetContourDefaults;
procedure SetSlopedefaultColors(var NumCats : SmallInt; var SlopeCut : ColorCutArrayType; var SlopeColors : tColorArray);

procedure SetDefaults(SetDirectories : boolean = false);
procedure SetExpertOptions(Expert : boolean);

procedure SetStructuralGeologyDefaults;
procedure SetPhysicalGeographyDefaults;
procedure SetEconDefaults;
procedure SetShipwrecksDefaults;
procedure SetRemoteSensingDefaults;

procedure HideAllOceanOptions;
procedure SetDefaultMapSizeToScreen;

procedure SaveBackupDefaults;
procedure RestoreBackupDefaults;
procedure LoadMDdefaults;
procedure SaveMDdefaults;
procedure ProcessIniFile(iniWhat : tIniWhat; SectionRestoreDefaults : ShortString = ''; ExplicitName : PathStr = '');
procedure RecreateIniFile;

procedure SetPerspectiveDefaults(var WorkingPersOpts : tPerspectiveOptions);
procedure SetFlyDefaults;
procedure SetLOSDefaults;
procedure ResetStratColDefaults;

procedure SetGeomorphDefaults;

//function MrSidEnabled : boolean;

{$IfDef ExGeomorphGrids}
{$Else}
   procedure SetAllCurvatures(Setting : boolean);
   procedure SetAllSlopes(Setting : boolean);
   procedure SetAllOrganization(Setting : boolean);
{$EndIf}


{$IfDef ExTiger}
{$Else}
   procedure SetTIGERDefaults;
   procedure WriteTigerDefaults;
{$EndIf}

{$IfDef ExPLSS}
{$Else}
   procedure SetPLSSDefaults;
{$EndIf}

procedure SetGazDefaults;
procedure SetGeologyOptions(Allow : boolean);
procedure SetFlatProfile;

procedure SetDEMIXdirs(Ask : boolean = false);

procedure SetSmallGraphs;
procedure SetDefaultDirectories;
procedure SetBaseDirectory;
procedure MakeRequiredDirectories;
procedure SetRootDirectoryFiles;

procedure GetRangeFactor(var Factor : float64);
procedure SetOptionsForMapWithNoMarginalia;
procedure GetMapDataDirectory;
procedure ReadCoordsLatLongFromStreamProfileResults(Coords : tStringList; i : integer; var Lat,Long : float64);

procedure AddFreeDiskSpaceToDebugFile;
function MissingData(z : float64) : boolean;

procedure ToggleShowProgress(ShowIt : boolean);
procedure CheckRequiredFiles;

procedure InitializeVegColors;
function RidgeAlgorithmName : string;
function ClassBoxName : string;

procedure InitializeMICRODEM;
function PointInBoundingBox(Lat,Long : float64; BBox : sfBoundBox) : boolean;  inline;
function AtLeastPartOfBoxInAnotherBox(BoxToTest,BoxAskingAbout : sfBoundBox) : boolean;
function AllOfBoxInAnotherBox(BoxToTest,BoxAskingAbout : sfBoundBox) : boolean;
procedure InitializeBoundBox(var bb : sfBoundBox);

function ExpandIconFileName(var fName : PathStr) : boolean;

function StandardLatLongString(Lat,Long : float64) : shortstring;
function GridLimitsToString(Limits : tGridLimits) : shortstring;
function sfBoundBoxToString(Limits : sfBoundBox; Decs : integer = 4) : shortstring;
function sfBoundBoxSizeToString(Limits : sfBoundBox; Decs : integer = 2) : shortstring;


{$IfDef ExDRGimport}
{$Else}
   procedure SetDRGDefaults;
{$EndIf}

{$IfDef ExAdvancedGIS}
{$Else}
   procedure GetMaskingOptions(AllowTiger,AllowRadius : boolean);
{$EndIf}

{$IfDef ExViewshed}
{$Else}
   procedure InitializeWeaponsFan(var WeaponsFan : tWeaponsFan);
   procedure InitializeWeaponsFanColors(var WeaponsFan : tWeaponsFan);

   procedure AddFanToWeaponsTable(PrimaryMapDatum : tMapProjection; EditIt,AddBasicParameters : boolean; var WeaponsTable : tMyData; WeaponsFan : tWeaponsFan);
   procedure AddMDDefaultsToWeaponsTable(var WeaponsTable : tMyData);
   procedure WeaponsTableToMDdefaults(TheData : tMyData);
   function WeaponsTableToFan(PrimaryMapDatum : tMapProjection; WeaponsTable : tMyData) : tWeaponsFan;
   function WeaponsTableBasicParametersToFan(PrimaryMapDatum : tMapProjection; WeaponsTable : tMyData) : tWeaponsFan;

   procedure ResetDefaultFanAlgorithm;
{$EndIf}

{$IfDef ExGazetteer}
{$Else}
   procedure SetUpGazFile(var GazColors : tMyData; var NameStr : ShortString; FilterUse : boolean = true);
   function USGSGazeeteerFile(fName : PathStr) : boolean;
   procedure PickGazFeatures;
{$EndIf}

{$IfDef VCL}
   procedure RecolorFan(var Bitmap : tMyBitmap; Color : tPlatformColor);
   procedure CheckFile(fName : PathStr);
   function USNAcomputer : boolean;
   procedure BackupProgramEXE(sname : shortstring = '');
   procedure RestorePreviousEXE;
   procedure PickForNotePadPlusPlus(DefFilt : byte);
   procedure EndBatchFile(fName : PathStr; var BatchFile : tStringList; Wait : boolean = true; Log : boolean = true);
{$EndIf}


{$IfDef AllowGeomorphometry}
   function BoxGridSize : shortString;
{$EndIf}


{$IfDef NoClustering}
{$Else}
   procedure DefineMICRODEMClusteringOptions(var MVClusterClientDataSet : tMVClusterClientDataSet);
{$EndIf}

{$IfDef RecordDefault}
   //procedure ShowKeyDefaults(Mess : shortString);
{$EndIf}

{$IfDef RecordIniMemoryOverwrite}
   procedure IniMemOverwriteCheck(Where : shortstring);
{$EndIf}

function LengthConversion(Code : integer) : float64;
function ThisIsETRS89(Tstr : ANSIstring) : boolean;
function ThisIsWGS84(Tstr : ANSIstring) : boolean;
function ThisIsNAD83(Tstr : ANSIstring) : boolean;

procedure QuadTickNearestHere(var Lat,Long : float64; QuadTickSize : float64);  inline;
function FindUTMZone(ASCIIProjectionData :  ANSIString; var UTMzone : int16; var Hemi : ANSIchar) : boolean;
function GetLimitsForParallelLoops(GridLimits : tGridLimits; ReqMult : integer = 1) : tGridLimitsArray;
procedure SimpleProfiles;
function CheckFileNameForSpaces(var fName : PathStr) : boolean;
function WarnAboutSpaces(fName : PathStr) : boolean;
function AskUserAboutMemory(MemNeed : int64) : boolean;
function GridLimitsString(GridLimits : tGridLimits) : shortstring;

{$IfDef RecordDirs}
   procedure RecordDirs(When : shortstring);
{$EndIf}

function DEMGridString(xgrid,ygrid : float32) : shortstring;

function UNIXTimeToDateTime(UnixTime: LongWord): TDateTime;
function DateTimeToUNIXTime(DelphiTime : TDateTime): LongWord;
function DateTimeToGPStime(DelphiTime : TDateTime): LongWord;
function GPSTimeToDateTime(GPSTime: LongWord): TDateTime;

function GetFieldFromXMLMetadata(fName : PathStr; Field : Shortstring) : shortstring;

function MapTypeName(MapType : integer) : shortstring;
function BoundBoxToGridLimits(bb : sfBoundBox) : tGridLimits;
function SWcornerString(Lat,Long : float64; TileSize : integer) : shortString;
function NWcornerString(Lat,Long : float64; TileSize : integer) : shortString;

function ZUnitCategory (Zunit : tElevUnit) : shortstring;
function RecycleCompressFile : boolean;

function AspectDir8FromAspect(AspectDir : float32) : tCompassDirection;  inline;
function ElevUnitsAre(Code : byte) : shortstring;

procedure VerticalDatumShift(DEM : integer; vdShift : tvdShift);

var
   VegDenstColors : array[0..255] of tPlatformColor;


implementation


uses
   {$If Defined(VCL)}
      Nevadia_Main,
      Toggle_db_use,
      DEMOptions,
      PETGraphColors,
      KML_Creator,
      DEMMapf,
      DEM_Manager,
   {$EndIf}

   {$IfDef ExTIGER}
   {$Else}
     DEMTiger,
   {$EndIf}

   {$IfDef ExAdvancedGIS}
   {$Else}
      Mask_Opts_Form,
   {$EndIf}

   {$IfDef ExGIS}
   {$Else}
      demdatabase,
   {$EndIf}

   {$IfDef ExMagVar}
   {$Else}
      DEMMagVar,
   {$EndIf}

   {$IfDef ExDatum}
   {$Else}
      DEMCoord,
   {$EndIf}

   {$IfDef ExcludeExternalTools}
   {$Else}
      MD_Use_tools,
   {$EndIf}

   PETMath,
   PetImage,
   PetDBUtils,
   Make_Tables,
   DEM_indexes,
   DEMStat,
   DataBaseCreate;


procedure VerticalDatumShift(DEM : integer; vdShift : tvdShift);
var
   i,geoidGrid : Integer;
   Merge : tDEMbooleans;
   fName : PathStr;
   TheShift : shortString;
begin
   GetGeoid;
   if (vdShift = vdEGM96toEGM2008) then fName := GeoidDiffFName
   else fName := Geoid2008FName;
   GeoidGrid := OpenNewDEM(fName,false);

   case vdShift of
      vdWGS84toEGM2008 : TheShift := 'gs84_to_egm2008';
      vdEGM2008toWGS84 : TheShift := 'egm2008_to_wgs84';
      vdEGM96toEGM2008 : TheShift := 'egm96_to_egm2008';
   end;

   if (vdShift = vdEGM2008toWGS84) then DEMGlb[GeoidGrid].MultiplyGridByConstant(-1);
   for i := 1 to MaxDEMDataSets do Merge[i] := false;
   Merge[DEM] := true;
   Merge[GeoidGrid] := true;
   SumDEMs(DEM, Merge,DEMGlb[DEM].AreaName + '_vdatum_shift_' + TheShift);
   CloseSingleDEM(GeoidGrid);
end;



function ElevUnitsAre(Code : byte) : shortstring;
begin
   case Code of
      euMeters	 : Result := 	' m'	;
      Feet	 : Result := 	' m'	;
      TenthMgal	 : Result := 	' mgal'	;
      Milligal	 : Result := 	' mgal'	;
      TenthGamma	 : Result := 	' gamma'	;
      Decimeters	 : Result := 	 ' m'	;
      Gammas	 : Result := 	' gamma'	;
      HundredthMGal	 : Result := 	' mgal'	;
      DeciFeet	 : Result := 	' m'	;
      Centimeters	 : Result := 	' m'	;
      //Unused10	 : Result := 	''	;
      HundredthMa	 : Result := 	' Ma'	;
      PercentSlope	 : Result := 	'%'	;
      Undefined	 : Result := 	''	;
      zDegrees	 : Result := 	'°'	;
      //Unused0	 : Result := 	''	;
      lnElev	 : Result := 	'ln(z)'	;
      LogElev	 : Result := 	'log(z)'	;
      //Unused1	 : Result := 	'°'	;
      //Unused2	 : Result := 	''	;
      zPercent	 : Result := 	'%'	;
      //Unused3	 : Result := 	''	;
      //Unused4	 : Result := 	'°'	;
      //Unused5	 : Result := 	''	;
      //Unused6	 : Result := 	''	;
      //Unused7	 : Result := 	''	;
      NLCD2001up	 : Result := 	' NLCD'	;
      LandFire	 : Result := 	' LandFire'	;
      Nanotesla	 : Result := 	' nT'	;
      NLCD1992	 : Result := 	'NLCD 1992'	;
      euIntCode	 : Result := 	' code'	;
      //unused8	 : Result := 	''	;
      //unused9	 : Result := 	''	;
      GLOBCOVER	 : Result := 	'GlobCover'	;
      GLC2000	 : Result := 	'GLC2000'	;
      euImagery	 : Result := 	' Imagery'	;
      euMM	 : Result := 	' mm'	;
      euMetersPerSec	 : Result := 	' m/s'	;
      zMperM	 : Result := 	' m/m'	;
      euKM	 : Result := 	' km'	;
      CCAP	 : Result := 	' C-CAP'	;
      euLASclass13	 : Result := 	' LAS13'	;
      euLASclass14	 : Result := 	' LAS14'	;
      euRGB	 : Result := 	' RGB'	;
      euMonth	 : Result := 	' month'	;
      CCI_LC	 : Result := 	 'CCI-LC'	;
      S2GLC	 : Result := 	'S2GLC'	;
      NLCD_Change	 : Result := 	'dNLCD'	;
      GLCS_LC100	 : Result := 	'GLCS_LC100'	;
      Meybeck	 : Result := 	'Meybeck'	;
      Geomorphon	 : Result := 	'Geomorphon'	;
      Iwahashi	 : Result := 	'Iwahashi'	;
      ESRI2020	 : Result := 	'ESRI2020'	;
      AspectDeg	 : Result := 	'°'	;
      euPennock	 : Result := 	'Pennock'	;
      euPerMeter	 : Result := 	'/m'	;
      WorldCover10m	 : Result := 	'WorldCover 10m'	;
      euNDVI	 : Result := 	'NDVI'	;
      euNBR	 : Result := 	'NBR'	;
      euDifference : Result := 'diff';
      euElevDiff : Result := 'diff m';
      else Result := '';
   end;
end;


function AspectDir8FromAspect(AspectDir : float32) : tCompassDirection;  inline;
begin
   if (AspectDir < 22.5) or (AspectDir > 337.5) then Result := cdN
   else if (AspectDir < 67.5) then Result :=cdNE
   else if (AspectDir < 112.5) then Result :=cdE
   else if (AspectDir < 157.5) then Result :=cdSE
   else if (AspectDir < 202.5) then Result :=cdS
   else if (AspectDir < 247.5) then Result :=cdSW
   else if (AspectDir < 292.5) then Result :=cdW
   else Result :=cdNW;
end;




function ZUnitCategory (Zunit : tElevUnit) : shortstring;
begin
   if zunit in [euMeters,Decimeters,Feet,DeciFeet,Centimeters] then Result := 'Elevation'
   else if zunit in [PercentSlope] then Result := 'Slope'
   else Result := 'Grid';
end;


function DEMGridString(xgrid,ygrid : float32) : shortstring;
begin
   Result := 'DEM grid: x=' + RealToString(Xgrid,-8,2) + '  y=' +  RealToString(Ygrid,-8,2);
end;


function RecycleCompressFile : boolean;
begin
   Result := true;
end;

function BoundBoxToGridLimits(bb : sfBoundBox) : tGridLimits;
begin
   Result.XGridLow := round(bb.XMin);
   Result.YGridLow := round(bb.YMin);
   Result.XGridHigh := round(bb.XMax);
   Result.YGridHigh := round(bb.YMax);
end;

procedure InitializeBoundBox(var bb : sfBoundBox);
begin
   bb.ymin := 99e39;
   bb.ymax := -99e39;
   bb.xmin := 999e39;
   bb.xmax := -999e39;
end;

function SWcornerString(Lat,Long : float64; TileSize : integer) : shortString;
begin
   if Lat > 0 then begin
      Result := 'N' + IntegerToString(trunc(Lat) div TileSize * TileSize,2);
   end
   else begin
      Result := 'S' + IntegerToString(trunc(abs(Lat-TileSize) / TileSize) * TileSize,2);
   end;
   if Long > 0 then begin
      Result :=  Result + 'E' + IntegerToString(trunc(Long / TileSize) * TileSize,3);
   end
   else begin
      Result := Result + 'W' + IntegerToString(trunc(abs(Long-TileSize) / TileSize) * TileSize,3);
   end;
   ReplaceCharacter(Result,' ','0');
end;

function NWcornerString(Lat,Long : float64; TileSize : integer) : shortString;
begin
   if Lat > 0 then begin
      Result := 'N' + IntegerToString(TileSize + trunc(Lat) div TileSize * TileSize,2);
   end
   else begin
      Result := 'S' + IntegerToString(-TileSize + trunc(abs(Lat-TileSize) / TileSize) * TileSize,2);
   end;
   if Long > 0 then begin
      Result :=  Result + 'E' + IntegerToString(trunc(Long / TileSize) * TileSize,3);
   end
   else begin
      Result := Result + 'W' + IntegerToString(trunc(abs(Long-TileSize) / TileSize) * TileSize,3);
   end;
   ReplaceCharacter(Result,' ','0');
end;


function MapTypeName(MapType : integer) : shortstring;
begin
   case MapType of
      mtIHSReflect : Result :=  'IHSReflect';
      mtGrayReflect :  Result :=  'GrayReflect';
      mtDEMContour   : Result :=  'Contour';
      mtSlopeGrayScaleReversed   : Result :=   'SlopeGrayScaleReversed';
      mtDEMReflectElevMerge   : Result :=  'ReflectElevMerge';
      mtDEMBlank   : Result :=   'Blank';
      mtDEMaspect   : Result :=   'aspect';
      mtDEMaspectSlope : Result := 'aspect/slope';
      mtElevGray   : Result :=  'ElevGray';
      mtElevGrayReversed   : Result :=   'ElevGrayRev';
      mtSatImageGray   : Result :=   'SatImage';
      mtAnaglyph   : Result :=   'Anaglyph';
      mtElevBands   : Result :=   'ElevBands';
      mtElevTerrain   : Result :=   'ElevTerrain';
      mtBlueGreenReflect   : Result :=   'BlueGreenReflect';
      mtElevRainbow   : Result :=  'ElevRainbow';
      mtElevIHS   : Result :=   'ElevIH';
      mtMergeTwoDEMs   : Result :=  'MergeTwoDEM';
      mtSatBlank   : Result :=   'SatBlank';
      mtElevSpectrum   : Result :=  'ElevSpectrum';
      mtVector   : Result :=   'Vector';
      mtNone   : Result :=  'None';
      mtElevFromTable   : Result := 'ElevFromTable';
      mtElevDefinedPalette   : Result :=  'ElevDefinedPalette';
      mtNoChange   : Result :=  'NoChange';
      mtDEMMask   : Result :=   'DEMMask';
      mtDEMVATTable   : Result :=  'DEMVATTable';
      mtElevContrast   : Result :=  'ElevContrast';
      mtFlowDir360   : Result :=  'FlowDir360';
      mtFlowDirArc   : Result :=  'FlowDirArc';
      mtFlowDirTau   : Result :=   'FlowDirTau';
      mt6ColorsReflect   : Result :=   '6ColorsReflect';
      mtSlopeStandardCats   : Result :=  'SlopeStandardCats';
      mtSlopeTrafficCats   : Result :=  'SlopeTrafficCats';
      mtSlopeGrayScale   : Result :=  'SlopeGrayScale';
      mtSlopeRainbow   : Result :=  'SlopeRainbow';
      mtSlopePastel   : Result :=  'SlopePastel';
      mtSlopeGoNoGo   : Result :=  'SlopeGoNoGo';
      mtRefGrayBlue   : Result :=  'RefGrayBlue';
      mtRefColorGray   : Result := 'RefColorGray';
      mtOpenness   : Result := 'Openness';
      mtLASclass   : Result := 'LASclass';
      mtRGB   : Result :=  'RGB';
      mtGYRReflect   : Result := 'Gr/Yel/red';
      mtGGRReflect   : Result := 'Gr/gray/red';
      mtSatTrueColor   : Result := 'True color';
      mtSatFalseColor   : Result := 'False color NIR';
      mtSatPickColor   : Result := 'Select false color';
      mtUnenhancedRGB  : Result := 'Unenhanced RGB';
   end;
end;


function GetFieldFromXMLMetadata(fName : PathStr; Field : Shortstring) : shortstring;
var
   sl : tStringList;
   i : integer;
   TStr,search : ansistring;
begin
   Result := '';
   sl := tStringList.Create;
   sl.LoadFromFile(fName);
   Search := UpperCase('<' + Uppercase(Field) + '>');
   for i := 0 to pred(sl.Count) do begin
      TStr := UpperCase(sl.Strings[i]);
      if StrUtils.AnsiContainsText(UpperCase(TStr),search) then begin
         TStr := Petmar_types.AfterSpecifiedString(TStr,Search);
         Result := Petmar_types.BeforeSpecifiedString(TStr,'<');
         exit;
      end;
   end;
   sl.Destroy;
end;


function UNIXTimeToDateTime(UnixTime: LongWord): TDateTime;
var
  TimeZoneInformation: TTimeZoneInformation;
begin
  GetTimeZoneInformation(TimeZoneInformation);
  Result := StrToDate('01/01/1970') + (UnixTime/(24*3600)) - ((TimeZoneInformation.Bias + TimeZoneInformation.DaylightBias) / (24 * 60));
end;

function DateTimeToUNIXTime(DelphiTime : TDateTime): LongWord;
var
  MyTimeZoneInformation: TTimeZoneInformation;
begin
  GetTimeZoneInformation(MyTimeZoneInformation);
  Result := round(DelphiTime - StrToDate('01/01/1970') + ((MyTimeZoneInformation.Bias) / (24 * 60))) * (24 * 3600);
end;

function DateTimeToGPStime(DelphiTime : TDateTime): LongWord;
begin
   Result := DateTimeToUNIXTime(DelphiTime) - 315964800;
end;

function GPSTimeToDateTime(GPSTime: LongWord): TDateTime;
begin
   Result := UnixTimeToDateTime(GPSTime + 315964800);
end;


procedure RecordDirs(When : shortstring);
begin
   {$IfDef RecordDirs} WriteLineToDebugFile(When + ' MainMapData=' + MainMapData + '   GT_Datum_fName=' + GT_Datum_fName); {$EndIf}
end;


function GridLimitsToString(Limits : tGridLimits) : shortstring;
begin
   Result := 'x=' + RealToString(Limits.XGridLow,-12,-4) + ' to ' + RealToString(Limits.XGridHigh,-12,-4) + ' y=' + RealToString(Limits.YGridLow,-12,-4) + ' to ' + RealToString(Limits.YGridHigh,-12,-4);
end;



function sfBoundBoxToString(Limits : sfBoundBox; Decs : integer = 4) : shortstring;
begin
   Result := 'x=' + RealToString(Limits.XMin,-12,-Decs) + ' to ' + RealToString(Limits.XMax,-12,-Decs) + '  y=' + RealToString(Limits.YMin,-12,-Decs) + ' to ' + RealToString(Limits.YMax,-12,-Decs);
end;

function sfBoundBoxSizeToString(Limits : sfBoundBox; Decs : integer = 2) : shortstring;
begin
   Result := RealToString(Limits.XMax - Limits.XMin,-12,-Decs) + ' x ' + RealToString(Limits.YMax-Limits.YMin,-12,-Decs);
end;


{$IfDef RecordIniMemoryOverwrite}
   procedure IniMemOverwriteCheck(Where : shortstring);
   begin
      if (MDDef.PerspOpts.PersWidth = 0) and (MDDef.PerspOpts.PersWidth = 0) then begin
         {$IfDef RecordProblems} HighlightLineToDebugFile('Corrupt INI file ' + Where); {$EndIf}
         RecreateINIfile;
         wmdem.Viewdebuglog1Click(Nil);
         MessageToContinue('Please forward Debug log to Prof Guth (just popped up in Notepad)');
      end;
   end;
{$EndIf}


procedure RecreateIniFile;
var
   fName : PathStr;
begin
  fName := IniFileName;
  SysUtils.DeleteFile(fName);
  LoadMDdefaults;
end;


function GridLimitsString(GridLimits : tGridLimits) : shortstring;
begin
   Result := 'Left:  ' + IntToStr(GridLimits.XGridLow) + ' Right: ' + IntToStr(GridLimits.XGridHigh) + ' Top:  ' + IntToStr(GridLimits.YGridHigh) + ' Bottom: ' + IntToStr(GridLimits.YGridLow);
end;


function AskUserAboutMemory(MemNeed : int64) : boolean;
begin
   Result := (MemNeed > MDDef.MapSizeToVerify * 1024 * 1024);
end;

function WarnAboutSpaces(fName : PathStr) : boolean;
begin
   if StrUtils.AnsiContainsText(fName,' ') then begin
      Result := true;
      MessageToContinue('Space in path or file name may lead to problems' + MessLineBreak + AnsiReplaceStr(fName,' ','***SPACE_HERE*****') + MessLineBreak + 'You should fix it and have been warned to expect problems');
      {$IfDef RecordProblems}
         WriteLineToDebugFile('**********');
         WriteLineToDebugFile('Warned about spaces in filename ' + fName);
         WriteLineToDebugFile('**********');
      {$EndIf}
   end
   else Result := false;
end;

function CheckFileNameForSpaces(var fName : PathStr) : boolean;
var
   OldName,Path : PathStr;
begin
   if FileExists(fName) then begin
      Path := ExtractFilePath(fName);
      if not StrUtils.AnsiContainsText(Path,' ') then begin
         if StrUtils.AnsiContainsText(ExtractFileName(fName),' ') then begin
            OldName := fName;
            fName := SpacesToUnderScores(fName);
            SysUtils.ReNameFile(OldName,fName);
         end;
      end;
      Result := WarnAboutSpaces(fName);
   end;
end;


procedure SimpleProfiles;
begin
   MDdef.CurvAlg := vcNoCurvature;
   MDdef.LOSVisible := false;
   MDdef.DrawLOS := false;
   {$IfDef VCL}
      ChangeDEMNowDoing(MultipleLOS);
   {$EndIf}
end;


function GetLimitsForParallelLoops(GridLimits : tGridLimits; ReqMult : integer = 1) : tGridLimitsArray;
var
   LastMax,BandSize : integer;

            function GetPartLimits(Limits : tGridLimits; Value : integer) :  tGridLimits;
            begin
               Result := Limits;
               if Value > 1 then Result.YGridLow := LastMax + 1;
               if (Value < MDdef.MaxThreadsForPC) then Result.YGridHigh := LastMax + BandSize - 2;
               LastMax := Result.YGridHigh;
            end;

var
   i : integer;
begin
   {$IfDef RecordParallelLoops} WriteLineToDebugFile('GetLimitsForParallelLoops  ' + GridLimits.YGridLow.ToString + '   ' + GridLimits.YGridHigh.ToString ); {$EndIf}
   if MDdef.MaxThreadsForPC < 1 then MDdef.MaxThreadsForPC := 1;

   BandSize := (GridLimits.YGridHigh - GridLimits.YGridLow) div MDdef.MaxThreadsForPC;
   {$IfDef RecordParallelLoops} WriteLineToDebugFile('Initial band size=' + IntToStr(BandSize) ); {$EndIf}
   while (BandSize mod ReqMult <> 0) do inc(BandSize);
   {$IfDef RecordParallelLoops} WriteLineToDebugFile('Final band size=' + IntToStr(BandSize) ); {$EndIf}

   for i := 1 to MDdef.MaxThreadsForPC do  begin
      Result[i] := GetPartLimits(GridLimits,i);
      {$IfDef RecordParallelLoops} WriteLineToDebugFile(i.ToString + '  ' + Result[i].YGridLow.ToString + '   ' + Result[i].YGridHigh.ToString ); {$EndIf}
   end;
   ThreadsWorking := true;
end;


procedure QuadTickNearestHere(var Lat,Long : float64; QuadTickSize : float64);
begin
   Lat := QuadTickSize * round(Lat / QuadTickSize);
   Long := QuadTickSize * round(Long / QuadTickSize);
end;


function FindUTMZone(ASCIIProjectionData :  ANSIString; var UTMzone : int16; var Hemi : ANSIChar) : boolean;
var
   Zone : shortstring;
   len : integer;
begin
   Result := false;
   ASCIIProjectionData := UpperCase(ASCIIProjectionData);
   {$IfDef RecordFindUTM} WriteLineToDebugFile(ASCIIProjectionData); {$EndIf}
   ReplaceCharacter(ASCIIProjectionData,'_',' ');
   {$IfDef RecordFindUTM} WriteLineToDebugFile(ASCIIProjectionData); {$EndIf}
   StripBlanks(ASCIIProjectionData);
   {$IfDef RecordFindUTM} WriteLineToDebugFile(ASCIIProjectionData); {$EndIf}
   if StrUtils.AnsiContainsText(ASCIIProjectionData,'UTM') then begin
      Zone := AfterSpecifiedString(ASCIIProjectionData,'UTMZONE');
      if (Zone = '') then Zone := AfterSpecifiedString(ASCIIProjectionData,'UTMSONE');
      if (Zone <> '') then begin
         if Zone[2] in ['0'..'9'] then len := 2
         else len := 1;
         Hemi := Zone[Len + 1];
         Zone := Copy(Zone,1,len);
         UTMZone := StrToInt(Zone);
         Result := true;
      end;
   end;
   {$IfDef RecordFindUTM} WriteLineToDebugFile('Leaving FindUTMzone, zone=' + IntToStr(UTMZone) + '  hemi=' + Hemi); {$EndIf}
end;


{$IfDef VCL}
procedure EndBatchFile(fName : PathStr; var BatchFile : tStringList; Wait : boolean = true; Log : boolean = true);
begin
   BatchFile.SaveToFile(fName);
   {$IfDef RecordGDAL} if not HeavyDutyProcessing then WriteLineToDebugFile('EndBatchFile created,fname=' + fname); {$EndIf}
   WinExecAndWait32(fName,Wait,log);
   {$IfDef RecordGDAL} if not HeavyDutyProcessing then WriteLineToDebugFile('EndBatchFile done'); {$EndIf}
   BatchFile.Free;
end;
{$EndIf}


{$IfDef AllowGeomorphometry}
function BoxGridSize : shortString;
begin
   Result := ' (' +  IntToStr(MDDef.GeomorphBoxSizeMeters) + ' m)'
end;
{$EndIf}


function LengthConversion(Code : integer) : float64;
//used for LAS and Geotiff
//this factor convertS the vertical and horizontal values to meters
begin
   case Code of
       Linear_Meters_9001 : Result := 1.0;
       Linear_Foot_9002 : Result := 0.3048;
       Linear_Foot_US_Survey_9003 : Result := 1200/3937;
   end;
end;


function ThisIsETRS89(Tstr : ANSIstring) : boolean;
begin
   Result := StrUtils.AnsiContainsText(TStr,'ETRS') and StrUtils.AnsiContainsText(TStr,'89');
end;


function ThisIsWGS84(Tstr : ANSIstring) : boolean;
begin
   Result := (StrUtils.AnsiContainsText(TStr,'WGS') and StrUtils.AnsiContainsText(TStr,'84')) or  StrUtils.AnsiContainsText(TStr,'GRS 1980') or StrUtils.AnsiContainsText(TStr,'GRS80')
        or (StrUtils.AnsiContainsText(TStr,'SIRGAS-ROU98'));
end;

function ThisIsNAD83(Tstr : ANSIstring) : boolean;
begin
   Result := StrUtils.AnsiContainsText(TStr,'NAD') and StrUtils.AnsiContainsText(TStr,'83');
end;



function IsThisATIGERshapefile(fName : PathStr) : boolean;
begin
   if FileExists(ChangeFileExt(fName,'.shp')) then begin
      fName := Uppercase(ExtractFileNameNoExt(FName));
      Result := StrUtils.AnsiContainsText(fName,'TL_') and ( StrUtils.AnsiContainsText(fName,'EDGES') or StrUtils.AnsiContainsText(fName,'ROADS') );
   end
   else Result := false;
end;


{$IfDef ExOSM}
{$Else}
   function IsThisAnOSMshapefile(fName : PathStr) : boolean;
   begin
      fName := UpperCase(fName);
      Result := FileExists(ChangeFileExt(fName,'.SHP')) and (StrUtils.AnsiContainsText(fName,'OPENSTREETMAP') or StrUtils.AnsiContainsText(fName,'OSM')or StrUtils.AnsiContainsText(fName,'PLANET_'));
   end;
{$EndIf}

function CheckIfTigerOrOSM(fName : PathStr) : boolean;
begin
   Result := IsThisATIGERshapefile(fName) {$IfDef ExOSM}{$Else} or IsThisAnOSMshapefile(fName) {$EndIf};
end;


function SpeedString(MetersPerSec : float64) : ShortString;
begin
   case MDDef.SpeedUnit of
      spMPS   : Result := RealToString(MetersPerSec,-12,-2) + ' m/sec';
      spKPH   : Result := RealToString(MetersPerSec * 3.6,-12,-1) + ' kph';
      spMPH   : Result := RealToString(MetersPerSec * 2.23694,-12,-1) + ' mph';
      spKnots : Result := RealToString(MetersPerSec * 1.94384,-12,-1) + ' knots';
   end;
end;


function StandardLatLongString(Lat,Long : float64) : shortstring;
begin
   Result := RealToString(Lat,12,8) + RealToString(Long,14,8);
end;


procedure ReadCoordsLatLongFromStreamProfileResults(Coords : tStringList; i : integer; var Lat,Long : float64);
var
   aLine : shortstring;
begin
   aline := Coords.Strings[i];
   Lat := StrToFloat(Petmar_types.BeforeSpecifiedCharacterANSI(aline,',',true,true));
   Long := StrToFloat(Petmar_types.BeforeSpecifiedCharacterANSI(aline,',',true,true));
end;


function ExpandIconFileName(var fName : PathStr) : boolean;
begin
   if not FileExists(fName) then begin
      fName := MainMapData + 'Icons\' + fName;
   end;
   Result := FileExists(fName);
end;


{$IfDef ExGeomorphGrids}
{$Else}
      procedure SetAllCurvatures(Setting : boolean);
      begin
          MDDef.DoCrossCurve := Setting;
          MDDef.DoMaxCurve := Setting;
          MDDef.DoMinCurve := Setting;
          MDDef.DoSlopeCurve := Setting;
          MDDef.DoPlanCurve := Setting;
      end;

      procedure SetAllSlopes(Setting : boolean);
      begin
         MDDef.DoSlopePC := setting;
         MDDef.DoSlopeDeg := setting;
         MDDef.DoSlopeSin := setting;
         MDDef.DoSlopeLogTan := setting;
         MDDef.DoSlopeSqrtSin := setting;
         MDDef.DoAspect := setting;
         MDDef.DoAspectNS := setting;
         MDDef.DoAspectEW := setting;
         MDDef.DoSlopeMperM := setting;
         MDDef.DoEWSlope := setting;
         MDDef.DoNSSlope := setting;
      end;

      procedure SetAllOrganization(Setting : boolean);
      begin
          MDDef.DoS1S2 := Setting;
          MDDef.DoS2S3 := Setting;
          MDDef.DoFabDir90 := Setting;
          MDDef.DoRoughness := Setting;
          MDDef.DoFabDir180 := Setting;
          MDDef.DoFabDir360 := Setting;
      end;

{$EndIf}


function AllOfBoxInAnotherBox(BoxToTest,BoxAskingAbout : sfBoundBox) : boolean;
begin
   Result := ((BoxToTest.xmin <= BoxAskingAbout.xmax) and (BoxToTest.xmin >= BoxAskingAbout.xmin) and (BoxToTest.xmax <= BoxAskingAbout.xmax) and (BoxToTest.xmax >= BoxAskingAbout.xmin) and
              (BoxToTest.ymin <= BoxAskingAbout.ymax) and (BoxToTest.ymin >= BoxAskingAbout.ymin) and (BoxToTest.ymax <= BoxAskingAbout.ymax) and (BoxToTest.ymax >= BoxAskingAbout.ymin));
end;


function AtLeastPartOfBoxInAnotherBox(BoxToTest,BoxAskingAbout : sfBoundBox) : boolean;

   function TestOne : boolean;
   begin
      Result := PointInBoundingBox(BoxToTest.ymax,BoxToTest.xmax,BoxAskingAbout) or PointInBoundingBox(BoxToTest.ymax,BoxToTest.xmin,BoxAskingAbout) or
                PointInBoundingBox(BoxToTest.ymin,BoxToTest.xmax,BoxAskingAbout) or PointInBoundingBox(BoxToTest.ymin,BoxToTest.xmin,BoxAskingAbout);
   end;

begin
   {$IfDef RecordAtLeastPartOfBoxInAnotherBox} WriteLineToDebugFile('AtLeastPartOfBoxInAnotherBox, BoxToTest, ' + sfBoundBoxToString(BoxToTest) + '  BoxAskingAbout, ' + sfBoundBoxToString(BoxAskingAbout)); {$EndIf}

   Result := TestOne;
   if (not Result) then begin
       Result := ((BoxToTest.ymin < BoxAskingAbout.ymax) and (BoxToTest.ymax > BoxAskingAbout.ymin) and (BoxToTest.xmin < BoxAskingAbout.xmax) and (BoxToTest.xmax > BoxAskingAbout.xmin));
   end;
   if (not Result) then begin
       Result := ((BoxToTest.ymin < BoxAskingAbout.ymin) and (BoxToTest.ymax > BoxAskingAbout.ymax) and (BoxToTest.xmin < BoxAskingAbout.xmin) and (BoxToTest.xmax > BoxAskingAbout.xmax));
   end;
   {$IfDef RecordAtLeastPartOfBoxInAnotherBox} WriteLineToDebugFile('AtLeastPartOfBoxInAnotherBox ' + TrueOrFalse(Result)); {$EndIf}
end;


function PointInBoundingBox(Lat,Long : float64; BBox : sfBoundBox) : boolean;
begin
   {$IfDef RecordPointInBox} WriteLineToDebugFile('PointInBoundingBox   BoxToTest, x=' + RealToString(Long,-12,4) +  ' y=' + RealToString(Lat,-12,4) ); {$EndIf}
   Result := (Lat >= bbox.YMin) and (Lat <= bbox.YMax) and (Long >= bbox.XMin) and (Long <= bbox.XMax);
   {$IfDef RecordPointInBox} WriteLineToDebugFile('PointInBoundingBox  ' + TrueOrFalse(Result)); {$EndIf}
end;

function IsReflectanceMap(MapType : tMapType) : boolean;
begin
   Result := MapType in [mtGrayReflect,mtBlueGreenReflect,mtIHSReflect,mt6ColorsReflect,mtRefGrayBlue,mtRefGrayBlue,mtRefGrayColor,mtRefColorGray,mtGYRReflect,mtGGRReflect,mtGrCyBlReflect,mt6ColorVAToverlay];
end;

function IsSlopeMap(MapType : tMapType) : boolean;
begin
   Result := MapType in [mtSlopeStandardCats,mtSlopeTrafficCats,mtSlopeGrayScale,mtSlopeGrayScaleReversed,mtSlopeRainbow,mtSlopePastel,mtSlopeGoNoGo];
end;

function IsElevationMap(MapType : tMapType) : boolean;
begin
   Result := MapType in [mtElevGray,mtElevFromTable,mtElevDefinedPalette,mtElevBands,mtElevTerrain,mtElevIHS,mtElevSpectrum,mtElevRainbow,mtElevLandSea,mtElevContrast,mtElevGrayReversed];
end;

function IsOtherGridMap(MapType : tMapType) : boolean;
begin
   Result := (MapType in [mtDEMContour,mtDEMaspect,mtDEMaspectSlope,mtDEMBlank,mtMergeTwoDEMs,mtDEMMask,mtDEMVATTable,
        mtFlowDir360,mtFlowDirArc,mtFlowDirTau,mtDEMContour,mtDEMReflectElevMerge,
        mtNoChange, mtDEMMask,mtOpenness,mtLASclass,mtRGB,mtLandCover]);
end;


function IsSatelliteMap(MapType : tMapType) : boolean;
begin
   Result := (MapType in [mtSatImageGray,mtSatBlank]) or IsSatelliteColorImage(MapType);
end;

function IsSatelliteColorImage(MapType : tMapType) : boolean;
begin
   Result := MapType in [mtSatTrueColor,mtSatFalseColor,mtSatPickColor,mtSatFalseVeg,mtUnenhancedRGB];
end;


procedure SetDefaultMapSizeToScreen;
begin
   {$IfDef VCL}
       MDDef.DefaultMapXSize := wmDEM.ClientWidth - 30;
       MDDef.DefaultMapYSize := wmDEM.ClientHeight - 130;
   {$EndIf}
end;


procedure InitializeMICRODEM;

      {$IfDef RecordProblems}
         procedure InitializeDebugLog;
         begin
            {$IfDef MessageStartup}  MessageToContinue('InitializeMICRODEM in'); {$EndIf}

            {$IfDef MSWindows}
               DebugFileName := ProgramRootDir + 'logs\';
               SafeMakeDir(DebugFileName);
               DebugFileName := DebugFileName + ExtractFileNameNoExt(Forms.Application.ExeName) + '_debug_log.txt';
               SafeMakeDir(MyDataPath);
            {$Else}
               {$IfDef Android}
                  DebugFileName := TPath.GetSharedDownloadsPath;
                  {$IfDef SameDebugName}
                     DebugFileName := ptCopy(DebugFileName,1,length(DebugFileName) - 8) + 'microdem/logs/microdem_debug_log.txt';
                  {$Else}
                     DebugFileName := ptCopy(DebugFileName,1,length(DebugFileName) - 8) + 'microdem/logs/microdem_debug_log_' + CurrentTimeForFileName + '.txt';
                  {$EndIf}
               {$EndIf}

               {$IfDef iOS}
                  DebugFileName := TPath.GetHomePath + '/microdem_debug_log.txt';
               {$EndIf}

               {$IfDef MacOS}
                  DebugFileName := ProgramRootDir + '/microdem_debug_log.txt';
               {$EndIf}
            {$EndIf}

            OpenDebugFile;
            WriteLineToDebugFile('Debug file opened');
            {$IfDef MessageStartup}  MessageToContinue('Debug file opened: ' + Petmar_types.DebugFileName); {$EndIf}
         end;
      {$EndIf}

var
   wYear,wmonth,wDay : word;
begin
    MDDef.MDRecordDebugLog := true;
    MDDef.MaxDebugLinesToKeep := 1250;
    MDDef.FinalLinesToKeep := 10;
    MDDef.InitialLinesToKeep := 7;

     {$IfDef MSWindows}
        ProgramRootDir := UpperCase(ExtractFilePath(ParamStr(0)));
        ChDir(ProgramRootDir);
        TrilobiteComputer := FileExists('olenellus.txt');
     {$EndIf}

     InitializePetmar;

     {$IfDef RecordProblems}
        InitializeDebugLog;
     {$EndIf}

     {$IfDef ExGIS}
     {$Else}
        InitializeDEMdbs;
     {$EndIf}

     {$IfDef RecordInitialization} WriteLineToDebugFile('InitializeMICRODEM Call InitilializeDEMCoord'); {$EndIf}
     InitilializeDEMCoord;
     {$IfDef RecordInitialization} WriteLineToDebugFile('InitializeMICRODEM Call LoadMDdefaults'); {$EndIf}
     LoadMDdefaults;
     {$IfDef RecordInitialization} WriteLineToDebugFile('InitializeMICRODEM Back from LoadMDdefaults'); {$EndIf}

     {$IfDef ExMagVar}
     {$Else}
        InitializeMagneticVariation;
     {$EndIf}

     if not PathIsValid(MainMapData) then begin
        {$IfDef RecordInitialization} WriteLineToDebugFile('InitializeMICRODEM Invalid MainMapData'); {$EndIf}
        {$IfDef FMX}
           {$IfDef MSWindows}
              MainMapData := 'c:\mapdata\ianMDdata\';
           {$EndIf}

           {$IfDef Android}
              MainMapData := TPath.GetSharedDownloadsPath;
              MainMapData := ptCopy(MainMapData,1,length(MainMapData) - 8) + 'microdem';
           {$EndIf}

           {$IfDef iOS}
              MainMapData := TPath.GetSharedDownloadsPath;
           {$EndIf}

           {$IfDef MacOS}
              MainMapData := TPath.GetDocumentsPath;
           {$EndIf}
        {$Else}
           MainMapData := 'c:\mapdata\';
           if not PathIsValid(MainMapData) then GetDOSPath('Main map data',MainMapData);
        {$EndIf}
     end;

     {$IfDef MSWindows}
        MDTempDir := MainMapData + 'temp\';
        {$IfDef RecordInitialization} WriteLineToDebugFile('InitializeMICRODEM Call clean up tempdir'); {$EndIf}
        CleanUpTempDirectory;
     {$Else}
        {$If Defined(Android) or Defined(MacOS)}
           MDTempDir := TPath.GetDocumentsPath;
        {$EndIf}

        {$IfDef iOS}
           MDTempDir := ProgramRootDir;
        {$EndIf}
     {$EndIf}

      DecodeDate(now,wYear,wmonth,wDay);
      MDDef.SingleJulianDay := AnnualJulianDay(wYear,wMonth,wDay);

     {$IfDef RecordInitialization}
        WriteLineToDebugFile('MainMapData=' + MainMapData);
        WriteLineToDebugFile('ProgramRootDir=' + ProgramRootDir);
        WriteLineToDebugFile('InitializeMICRODEM out,  MDTempDir=' + MDTempDir);
     {$EndIf}
end;


function RidgeAlgorithmName : string;
begin
   case MDDef.RidgePointClassify of
     raWood    : Result := 'Wood';
     raSimple2 : Result := 'Simple2 (t=' + IntToStr(MDDef.ValleyRidgeThreshold) + ')';
     raSimple  : Result := 'Simple';
   end;
   Result := Result + ' (r=' + IntToStr(MdDef.WoodRegionRadiusPixels) + ' posts)';
end;

function ClassBoxName : string;
begin
   case MDDef.ClassLimitMethod of
        clmMeanStd : Result := '(Mean/Std)';
        clm5_95    : Result := '(5-95)';
        clm10_90   : Result := '(10-90)';
        clm25_75   : Result := '(25-75)';
        clmMinMax  : Result := '(Min/Max)';
   end;
end;

{$IfDef ExAdvancedGIS}
{$Else}
   procedure GetMaskingOptions(AllowTiger,AllowRadius : boolean);
   var
     MaskOptsForm : TMaskOptsForm;
   begin
      MaskOptsForm := TMaskOptsForm.Create(Application);
      MaskOptsForm.Edit1.Text := IntToStr(MDDef.MaskDistance);
      if MDDef.MaskShapesIn then MaskOptsForm.RadioGroup1.ItemIndex := 1
      else MaskOptsForm.RadioGroup1.ItemIndex := 0;
      MaskOptsForm.CheckBox1.Checked := MDDef.FilterAllTigerRoads;
      MaskOptsForm.CheckBox2.Checked := MDDef.ShowMasks;
      MaskOptsForm.CheckBox1.Enabled := AllowTiger;
      MaskOptsForm.BitBtn1.Enabled := AllowTiger;
      MaskOptsForm.Label1.Enabled := AllowRadius;
      MaskOptsForm.Edit1.Enabled := AllowRadius;
      MaskOptsForm.ShowModal;
   end;
{$EndIf}


{$IfDef VCL}

procedure RestorePreviousEXE;
var
   fName,fName2 : PathStr;
   BatFile : tStringList;
begin
   {$IfDef RecordUpdate} WriteLineToDebugFile('RestorePreviousEXE; in'); {$EndIf}
   DEM_Manager.CloseAllWindowsAndData;
   BackupProgramEXE;
   fName2 := ProgramRootDir + 'backup\';
   if GetFileFromDirectory('md exe to restore','*.exe',fName2) then begin
      ChDir(ProgramRootDir);
      fName := ProgramRootDir + 'restore_md.bat';
      BatFile := tStringList.Create;
      BatFile.Add('REM wait program close');
      BatFile.Add('ping 127.0.0.1 -n ' + IntToStr(MDDef.UpdateDelay) + ' > nul');
      BatFile.Add('del backup\microdem.exe /F');
      BatFile.Add('del microdem.exe /F');
      BatFile.Add('ping 127.0.0.1 -n ' + IntToStr(MDDef.UpdateDelay) + ' > nul');
      BatFile.Add('copy ' + fName2 + ' microdem.exe');
      BatFile.Add('ping 127.0.0.1 -n ' + IntToStr(MDDef.UpdateDelay) + ' > nul');
      BatFile.Add('microdem');
      BatFile.SaveToFile(fName);
      BatFile.Free;
      ExecuteFile(fName, '', ProgramRootDir);
      wmdem.Close;
   end;
   {$IfDef RecordUpdate} WriteLineToDebugFile('RestorePreviousEXE out'); {$EndIf}
end;


procedure BackupProgramEXE(sname : shortstring = '');
var
   NewName : PathStr;
begin
  ChDir(ProgramRootDir);
  SafeMakeDir(ProgramRootDir + 'backup\');
  if (sName = '') then begin
     {$IfDef Win64}
        sName := 'MD64';
     {$EndIf}
     {$IfDef Win32}
        SName := 'MD32';
     {$EndIf}
  end;
  NewName := ProgramRootDir + 'backup\' + sName +  '_' + BuildString + '.exe';
  if not FileExists(NewName) then CopyFile(Application.ExeName, NewName);
end;
{$EndIf}


procedure InitializeVegColors;
var
   i : integer;
begin
   {$IfDef VCL}
      for I := 1 to MDDef.VegDensityGraphMaxDensity do begin
         VegDenstColors[i] := ConvertTColorToPlatformColor(RainbowColorFunct(i,1,MDDef.VegDensityGraphMaxDensity));
      end;
      VegDenstColors[0] := VegDenstColors[1];
      for I :=succ(MDDef.VegDensityGraphMaxDensity) to 255 do begin
         VegDenstColors[i] :=  VegDenstColors[MDDef.VegDensityGraphMaxDensity];
      end;
   {$EndIf}
end;


procedure CheckRequiredFiles;
begin
   {$IfDef VCL}
      {$IfDef SQLiteDefaultDBs}
         wmdem.ConvertDBFsfor64bit1Click(nil);
      {$EndIf}
      if MDDef.SkipWebUpdates then begin
         {$IfDef RecordUpdate} WriteLineToDebugFile('CheckRequiredFiles skipped'); {$EndIf}
      end
      else begin
         {$IfDef RecordUpdate} WriteLineToDebugFile('CheckRequiredFiles in'); {$EndIf}
         CheckFile(ExtractFileName(TableDefinitionsFileName));
         CheckFile(ExtractFileName(CSVImportRulesFName));
         CheckFile(ExtractFileName(GazOptFName));
         CheckFile(ExtractFileName(GT_Datum_fName));
         CheckFile(ExtractFileName(GT_Ellipse_fName));
         CheckFile(ExtractFileName(ColorBrewerName));
         CheckFile(ExtractFileName(HardLimitColorPaletteFName));
         CheckFile(ExtractFileName(LandCoverFName));

         CheckFile('KeyHH.exe');
         CheckFile('md_movie.exe');
         CheckFile('7z.exe');
         CheckFile('7z.dll');

         {$IfDef ExGeography}
         {$Else}
            CheckFile(ExtractFileName(MonthlyClimateFName));
            CheckFile(ExtractFileName(KoppenDefFName));
         {$EndIf}

         {$IfDef ExSat}
         {$Else}
            CheckFile(ExtractFileName(SatBandNames));
            CheckFile(ExtractFileName(TM_RGB_fname));
         {$EndIf}

         {$IfDef ExPointCloud}
         {$Else}
            CheckFile(ExtractFileName(LasRulesName));
         {$EndIf}

         {$IfDef ExMagVar}
         {$Else}
            CheckFile(ExtractFileName(www_mag_mod_fName));
         {$EndIf}

         {$IfDef ExPLSS}
         {$Else}
            CheckFile(ExtractFileName(PLSSMerfName));
         {$EndIf}

         {$IfDef ExOSM}
         {$Else}
            CheckFile(ExtractFileName(OSMRoadRules));
            CheckFile(ExtractFileName(OSMGroupRules));
         {$EndIf}

         {$IfDef ExTIGER}
         {$Else}
            CheckFile(ExtractFileName(TigerShapeRules));
            CheckFile(ExtractFileName(TigerIndex));
         {$EndIf}
      end;
      {$IfDef RecordUpdate} WriteLineToDebugFile('CheckRequiredFiles out, MainMapData=' + MainMapData); {$EndIf}
   {$EndIf}
end;


procedure ProcessIniFile(iniWhat : tIniWhat; SectionRestoreDefaults : ShortString = ''; ExplicitName : PathStr = '');
var
   MDIniFile : tMDiniFile;

   procedure PerspectiveOptions;
   begin
      {$If Defined(RecordINIfiles)} WriteLineToDebugFile('PerspectiveOptions'); {$EndIf}
       with MDINIfile,MDDef,PerspOpts do begin
          {$IfDef Nevadia}
             Aparameter('Persp','OutLineCrests',OutLineCrests,false);
             AParameter('Persp','PersWidth',PersWidth,300);
             AParameter('Persp','PersHeight',PersHeight,225);
          {$Else}
             Aparameter('Persp','OutLineCrests',OutLineCrests,true);
             AParameter('Persp','PersWidth',PersWidth,800);
             AParameter('Persp','PersHeight',PersHeight,400);
          {$EndIf}

          AParameter('Persp','UsersSky',UsersSky,true);
          AParameter('Persp','TitlePerspectiveViewPort',TitlePerspectiveViewPort,false);
          AParameter('Persp','OutlineCrests',OutlineCrests,false);
          AParameter('Persp','PersViewDepth',PersViewDepth,2500);
          AParameter('Persp','PersAzimuth',PersAzimuth,90);

          {$IfDef VCL}
             AParameter('Persp','rgbtSky.rgbtred',rgbtSky.rgbtred,0) ;
             AParameter('Persp','rgbtSky.rgbtgreen',rgbtSky.rgbtgreen,255) ;
             AParameter('Persp','rgbtSky.rgbtblue',rgbtSky.rgbtblue,255) ;
          {$EndIf}

          {$IfDef FMX}
             AParameter('Persp','HFOVSetting',HFOVSetting,3);
             AParameter('Persp','VFOVSetting',VFOVSetting,3);
             AParameter('Persp','DepthSetting',DepthSetting,3);
             AParameter('Persp','ObsUpSetting',ObsUpSetting,3);
          {$EndIf}

          AParameter('Persp','NapEarth',NapEarth,true) ;
          AParameter('Persp','PanWidth',PanWidth,3000);
          AParameter('Persp','PanHeight',PanHeight,300);
          Aparameter('Persp','NoVE',NoVE,false);
          Aparameter('Persp','VertLabelInc',VertLabelInc,2);
          Aparameter('Persp','HorizLabelInc',HorizLabelInc,10);

         Aparameter('Persp','SaveAsProgramDefaults',SaveAsProgramDefaults,TRUE);
         AParameter('Persp','PersDrapedBetweenImages',PersDrapedBetweenImages,250);
         AParameter('Persp','PersDrapedBetweenProf',PersDrapedBetweenProf,10);
         AParameter('Persp','PersDistBetweenProf',PersDistBetweenProf,250);
         AParameter('Persp','PersFirstProfile',PersFirstProfile,250);
         AParameter('Persp','PersMeshSpace',PersMeshSpace,125);
         AParameter('Persp','CrestSeparator',CrestSeparator,750);
         AColorParameter('Persp','CrestColor',CrestColor,claBlack);
         Aparameter('Persp','CrestLineWidth',CrestLineWidth,1);
         AParameter('Persp','PerspAnaglyphShift',PerspAnaglyphShift,30);
         AParameter('Persp','PerspAnaglyphSeperate',PerspAnaglyphSeperate,50);
         Aparameter('Persp','ViewShedFanWithPerspective',ViewShedFanWithPerspective,FALSE);
         Aparameter('Persp','PersVaryResolutionAlongRadial',PersVaryResolutionAlongRadial,FALSE);

         Aparameter('Persp','LabelPerspectiveViewPort',LabelPerspectiveViewPort,TRUE);
         Aparameter('Persp','TitlePerspectiveViewPort',TitlePerspectiveViewPort,TRUE);
         AParameter('Persp','PersVaryResRanges[1]',PersVaryResRanges[1],5000);
         Aparameter('Persp','PersVaryResFactors[1]',PersVaryResFactors[1],2);
         AParameter('Persp','PersVaryResRanges[2]',PersVaryResRanges[2],10000);
         Aparameter('Persp','PersVaryResFactors[2]',PersVaryResFactors[2],4);
         AParameter('Persp','PersVaryResRanges[3]',PersVaryResRanges[3],50000);
         Aparameter('Persp','PersVaryResFactors[3]',PersVaryResFactors[3],8);
         AParameter('Persp','PersVaryResRanges[4]',PersVaryResRanges[4],100000);
         Aparameter('Persp','PersVaryResFactors[4]',PersVaryResFactors[4],16);
         Aparameter('Persp','CloudBackground',CloudBackground,FALSE);

         AParameter('Persp','PerspAbsElev',PerspAbsElev,2500);
         Aparameter('Persp','CloudBitmapName',CloudBitmapName,'');
         Aparameter('Persp','ShowLocationSensitivity',ShowLocationSensitivity,False);

         AParameterShortFloat('Persp','PersHFOV',PersHFOV,40.0);
         AParameterShortFloat('Persp','PersVFOV',PersVFOV,30.0) ;
         AParameterShortFloat('Persp','PanHFOV',PanHFOV,400.0) ;
         AParameterShortFloat('Persp','PanVFOV',PanVFOV,40.0) ;
         AparameterShortFloat('Persp','PerspectiveSkyAngle',PerspectiveSkyAngle,4.0);
         AparameterShortFloat('Persp','PersObsUp',PersObsUp,250.0);

          if (IniWhat = iniWrite) then IniFile.WriteInteger('Persp','PerpsectiveStereo',ord(PerpsectiveStereo));
          if (IniWhat = iniRead) then PerpsectiveStereo := tPerpsectiveStereo(IniFile.ReadInteger('Persp','PerpsectiveStereo',ord(psNone)));
          if (iniWhat = iniInit) then PerpsectiveStereo := psNone;

          if (IniWhat = iniWrite) then IniFile.WriteInteger('Persp','WhichPerspective',ord(WhichPerspective));
          if (IniWhat = iniRead) then WhichPerspective := tPerspectiveType(IniFile.ReadInteger('Persp','WhichPerspective',ord(ReflectancePerspective)));
          if (iniWhat = iniInit) then WhichPerspective := ReflectancePerspective;
       end;
   end;

   procedure TissotOptions;
   begin
      {$IfDef ExTissot}
      {$Else}
         {$If Defined(RecordINIfiles)} WriteLineToDebugFile('TissotOptions'); {$EndIf}
         with MDINIfile,MDDef do begin
            AParameter('Tissot','TissotHKdecimals',TissotHKdecimals,2);
            AParameter('Tissot','TissotRadius',TissotRadius,25);
            AParameter('Tissot','DrawTissotDefault',DrawTissotDefault,false);
            AParameter('Tissot','TissotLineWidth',TissotLineWidth,2);
            AParameter('Tissot','TissotPixelSpacing',TissotPixelSpacing,100);
            AParameter('Tissot','TissotSpaceByPixels',TissotSpaceByPixels,true);
            AParameter('Tissot','ShowTissotHK',ShowTissotHK,true);
            AParameter('Tissot','SimpleTissotCylindrical',SimpleTissotCylindrical,true);
            AParameterShortFloat('Tissot','TissotLatGridInc',TissotLatGridInc,30);
            AParameterShortFloat('Tissot','TissotLongGridInc',TissotLongGridInc,30);
            AColorParameter('Tissot','TissotColor',TissotColor,claRed);
         end;
      {$EndIf}
   end;

   procedure FlyOptions;
   begin
      {$IfDef ExFly}
      {$Else}
         {$If Defined(RecordINIfiles)} WriteLineToDebugFile('FlyOptions'); {$EndIf}
         with MDINIfile,MDDef,FlyOptions do begin
            AParameterFloat('Fly','FlyHFOV',FlyHFOV,40);
            AParameterFloat('Fly','FlyVFOV',FlyVFOV,20);
            AParameterFloat('Fly','DeltaHeading',DeltaHeading,2);
            AParameterFloat('Fly','TargetFOV1',TargetFOV1,5.87);
            AParameterFloat('Fly','TargetFOV2',TargetFOV2,1.65);
            AColorParameter('Fly','FightLineColor',FightLineColor,claBlack);
            AParameter('Fly','FlyDepth',MDDef.PerspOpts.FlyDepth,7500);
            AParameter('Fly','LiveFlyMovie',LiveFlyMovie,false);
            AParameter('Fly','FlySideBySide',FlySideBySide,true);
            AParameter('Fly','ShowFlyScene',ShowFlyScene,false);
            AParameter('Fly','ShowFlyThroughRoute',ShowFlyThroughRoute,true);
            AParameter('Fly','LiveFlyAutoNap',LiveFlyAutoNap,true);
            AParameter('Fly','FlyHeight',FlyHeight,500);
            AParameter('Fly','FlyThroughHeight',FlyThroughHeight,240);
            AParameter('Fly','FlyThroughWidth',FlyThroughWidth,320);
            AParameter('Fly','FlySceneSeparation',FlySceneSeparation,250);
            AParameter('Fly','DeltaZ',DeltaZ,100);
            AParameter('Fly','DeltaSpeed',DeltaSpeed,50);
            AParameter('Fly','NumTargetViews',NumTargetViews,1);
            AParameter('Fly','NumFlyDrapes',NumFlyDrapes,1);
            AParameter('Fly','LiveFlyDelay',LiveFlyDelay,100);
            AParameter('Fly','FlightLineSize',FlightLineSize,3);
            AParameter('Fly','FlyCrossTrackheight',FlyCrossTrackheight,1500);
            AParameter('Fly','FlyCrossTrackDistance',FlyCrossTrackDistance,3000);
            AParameter('Fly','NumSideProfiles',NumSideProfiles,5);
            AParameter('Fly','SideProfileSpacing',SideProfileSpacing,250);
            AParameter('Fly','ShowAlongTrackProfiles',ShowAlongTrackProfiles,false);
            AParameter('Fly','ShowCrossTrackProfiles',ShowCrossTrackProfiles,false);
            AParameter('Fly','MinTerrainFlyAbove',MinTerrainFlyAbove,30);
         end;
         {$If Defined(RecordINIfiles)} WriteLineToDebugFile('ProcessIniFile after flying'); {$EndIf}
      {$EndIf}
   end;

   procedure GeomorphOptions;
   begin
      {$If Defined(RecordINIfiles)} WriteLineToDebugFile('GeomorphOptions'); {$EndIf}
       MDINIfile.AParameter('Geomorph','StatSampleIncr',MDDef.StatSampleIncr,1);

      {$IfDef ExComplexGeostats}
      {$Else}
         with MDINIfile,MDDef do begin
            AParameter('Geomorph','TrendSurfaceOverVoids',TrendSurfaceOverVoids,true);
            AParameter('Geomorph','CurOrdTrendSurf',CurOrdTrendSurf,1);
            AParameter('Geomorph','TrendSurfMap',TrendSurfMap,true);
            AParameter('Geomorph','TrendDoGraph',TrendDoGraph,false);
            AParameter('Geomorph','TrendMapDev',TrendMapDev,true);
            AParameter('Geomorph','TrendOpenMaps',TrendOpenMaps,true);
            AParameter('Geomorph','GeomorphAllDEMs',GeomorphAllDEMs,true);
            AParameter('Geomorph','AutoSaveGeomorphGrids',AutoSaveGeomorphGrids,false);
            AParameter('Geomorph','TrendHistDev',TrendHistDev,true);
            AParameter('Geomorph','TrendText',TrendText,true);
            AParameter('Geomorph','ValleyRidgeThreshold',ValleyRidgeThreshold,7);

            AParameter('Geomorph','RoughnessBox',RoughnessBox,5);

            AParameter('Geomorph','GeomorphMapsFullDEM',GeomorphMapsFullDEM,true);
            AParameter('Geomorph','GrainOrgGraph',GrainOrgGraph,true);
            AParameter('Geomorph','GrainFlatGraph',GrainFlatGraph,true);
            AParameter('Geomorph','GrainNets',GrainNets,false);
            AParameter('Geomorph','GrainDir',GrainDir,true);
            AParameter('Geomorph','GrainAspects',GrainAspects,false);
            AParameter('Geomorph','GrainText',GrainText,true);
            AParameter('Geomorph','GrainReliefGraph',GrainReliefGraph,true);
            AParameter('Geomorph','GrainSampleSeparation',GrainSampleSeparation,500);
            AParameter('Geomorph','FirstBoxSize',FirstBoxSize,500);
            AParameter('Geomorph','LastBoxSize',LastBoxSize,15000);
            AParameter('Geomorph','BoxSizeIncr',BoxSizeIncr,500);
            AParameter('Geomorph','CrestThreadInterval',CrestThreadInterval,500);
            AParameter('Geomorph','GeomorphNameModifier',GeomorphNameModifier,'NORM_');
            AParameter('Geomorph','CrestMaxGap',CrestMaxGap,2);
            AParameter('Geomorph','OpenGridMaps',OpenGridMaps,false);
            AParameter('Geomorph','MaxMigration',MaxMigration,25);

            AParameter('Geomorph','ShiftLoX',ShiftLoX,0);
            AParameter('Geomorph','ShiftHighX',ShiftHighX,25);
            AParameter('Geomorph','ShiftLoY',ShiftLoY,0);
            AParameter('Geomorph','ShiftHighY',ShiftHighY,15);
            AParameter('Geomorph','WoodRegionRadiusPixels',WoodRegionRadiusPixels,1);
            AParameter('Geomorph','SSOSampleIncr',SSOSampleIncr,1);
            AParameter('Geomorph','MapSampleIncr',MapSampleIncr,1);
            AParameter('Geomorph','ShowRegularHistogram',ShowRegularHistogram,true);
            AParameter('Geomorph','ShowCumulativeHistogram',ShowCumulativeHistogram,true);
            AParameter('Geomorph','ShowStrahlerHistogram',ShowStrahlerHistogram,true);
            AParameter('Geomorph','ShowNormalHistogram',ShowNormalHistogram,true);
            AParameter('Geomorph','ShowHistogramText',ShowHistogramText,true);
            AParameter('Geomorph','GemorphAtlasFilterSize',GemorphAtlasFilterSize,5);
            AParameter('Geomorph','FilterGridsToEdge',FilterGridsToEdge,false);
            AParameter('Geomorph','GemorphAtlasMatchNeed',GemorphAtlasMatchNeed,60);
            AParameter('Geomorph','GeomorphBoxSizeMeters',GeomorphBoxSizeMeters,500);
            AParameter('Geomorph','ReliefBoxSizeMeters',ReliefBoxSizeMeters,500);

            AParameter('Geomorph','MomentsBoxSizeMeters',MomentsBoxSizeMeters,750);

            AParameter('Geomorph','OpenGridBoxSize',OpenGridBoxSize,100);
            AParameter('Geomorph','OpenBoxSizeMeters',OpenBoxSizeMeters,750);

            AParameter('Geomorph','SSOGridBoxSize',SSOGridBoxSize,100);
            AParameter('Geomorph','SSOBoxSizeMeters',SSOBoxSizeMeters,750);

            AParameter('Geomorph','GeomorphElevsNeeded',GeomorphElevsNeeded,25);
            AParameter('Geomorph','MinPointsForSSO',MinPointsForSSO,100);
            AParameter('Geomorph','FabricCalcThin',FabricCalcThin,4);
            AParameter('Geomorph','MomentCalcThin',MomentCalcThin,4);
            AParameter('Geomorph','OpennesstCalcThin',OpennessCalcThin,1);
            AParameter('Geomorph','ReliefCalcThin',ReliefCalcThin,4);

            AParameter('Geomorph','LimitSSODirByStrength',LimitSSODirByStrength,true);
            AParameter('Geomorph','LimitSSODirByFlatness',LimitSSODirByFlatness,true);
            AParameter('Geomorph','RoadTrendRegion',RoadTrendRegion,30);
            AParameter('Geomorph','RoadTrendSensitivity',RoadTrendSensitivity,5);
            AParameter('Geomorph','EdgeFilterGap',EdgeFilterGap,4);
            AParameter('Geomorph','MeanMedianBoxSize', MeanMedianBoxSize,101);
            AParameter('Geomorph','GemorphSSOPoles', GemorphSSOPoles,true);
            AParameter('Geomorph','GemorphAspectRose', GemorphAspectRose,true);
            AParameter('Geomorph','PointFabricTerrBlowup',PointFabricTerrBlowup,false);
            AParameter('Geomorph','PyramidLevels',PyramidLevels,3);
            AParameter('Geomorph','BatchRegionSize1',BatchRegionSize[1],150);
            AParameter('Geomorph','BatchRegionSize2',BatchRegionSize[2],250);
            AParameter('Geomorph','BatchRegionSize3',BatchRegionSize[3],500);
            AParameter('Geomorph','BatchRegionSize4',BatchRegionSize[4],750);
            AParameter('Geomorph','BatchRegionSize5',BatchRegionSize[5],1000);
            AParameter('Geomorph','ElevBinSize',ElevBinSize,1);

            AParameter('Geomorph','DoElevHist',DoElevHist,true);
            AParameter('Geomorph','DoSlopeHist',DoSlopeHist,true);
            AParameter('Geomorph','DoRuffHist',DoRuffHist,true);
            AParameter('Geomorph','DoAspectHist',DoAspectHist,true);


            AParameterShortFloat('Geomorph','HistElevBinSize',HistElevBinSize,20);
            AParameterShortFloat('Geomorph','HistSlopeBinSize',HistSlopeBinSize,1);
            AParameterShortFloat('Geomorph','HistRuffSlopeBinSize',HistRuffBinSize, 0.5);
            AParameterShortFloat('Geomorph','HistAspectBinSize',HistAspectBinSize, 2);


            AParameterShortFloat('Geomorph','HistBinSize',HistBinSize,1);
            AParameterShortFloat('Geomorph','BicubicSlope',BicubicSlope,1);

            AParameterShortFloat('Geomorph','ElevHistBinSize',ElevHistBinSize,5);
            AParameterShortFloat('Geomorph','SlopeHistBinSize',SlopeHistBinSize,2.5);

            AColorParameter('Geomorph','GrainColor',GrainColor,claRed);

            {$IfDef ExGeomorphGrids}
            {$Else}
               AParameter('Geomorph','DoCrossCurve',DoCrossCurve,false);
               AParameter('Geomorph','DoMaxCurve',DoMaxCurve,false);
               AParameter('Geomorph','DoMinCurve',DoMinCurve,false);
               AParameter('Geomorph','DoSlopeCurve',DoSlopeCurve,true);
               AParameter('Geomorph','DoPlanCurve',DoPlanCurve,true);
               AParameter('Geomorph','DoUpOpen',DoUpOpen,true);
               AParameter('Geomorph','DoDownOpen',DoDownOpen,true);
               AParameter('Geomorph','DoDiffOpen',DoDiffOpen,true);
               AParameter('Geomorph','DoRelief1',DoRelief1,true);
               AParameter('Geomorph','DoAvgElev',DoAvgElev,false);
               AParameter('Geomorph','DoElevStd',DoElevStd,false);
               AParameter('Geomorph','DoREL',DoREL,true);
               AParameter('Geomorph','DoTPI',DoTPI,true);
               AParameter('Geomorph','DoRelief2',DoRelief2,false);
               AParameter('Geomorph','DoSummit',DoSummit,false);
               AParameter('Geomorph','DoBaseLevel',DoBaseLevel,false);
               AParameter('Geomorph','DoGeophysical',DoGeophysical,false);
               AParameter('Geomorph','DoDropoff',DoDropoff,false);
               AParameter('Geomorph','DoElevRelief',DoElevRelief,false);

               AParameter('Geomorph','DoS1S2',DoS1S2,false);
               AParameter('Geomorph','DoS2S3',DoS2S3,true);
               AParameter('Geomorph','DoFabDir90',DoFabDir90,true);
               AParameter('Geomorph','DoFabDir180',DoFabDir180,true);
               AParameter('Geomorph','DoFabDir360',DoFabDir360,true);
               AParameter('Geomorph','DoRoughness',DoRoughness,false);
               AParameter('Geomorph','DoAvgVectStrength',DoAvgVectStrength,false);

               AParameter('Geomorph','DoSlopePC',DoSlopePC,true);
               AParameter('Geomorph','DoSlopeMperM',DoSlopeMperM,false);
               AParameter('Geomorph','DoSlopeDeg',DoSlopeDeg,false);
               AParameter('Geomorph','DoSlopeSin',DoSlopeSin,false);
               AParameter('Geomorph','DoSlopeLogTan',DoSlopeLogTan,false);
               AParameter('Geomorph','DoSlopeLnTan',DoSlopeLnTan,false);
               AParameter('Geomorph','DoSlopeSqrtSin',DoSlopeSqrtSin,false);
               AParameter('Geomorph','DoNSslope',DoNSslope,true);
               AParameter('Geomorph','DoEWslope',DoEWslope,true);
               AParameter('Geomorph','DoAspect',DoAspect,true);
               AParameter('Geomorph','DoAspectNS',DoAspectNS,false);
               AParameter('Geomorph','DoAspectEW',DoAspectEW,false);
               //AParameter('Geomorph','DoElevMoments',DoElevMoments,false);
               //AParameter('Geomorph','DoSlopeMoments',DoSlopeMoments,false);
               //AParameter('Geomorph','DoPlanCurvMoments',DoPlanCurvMoments,false);
               //AParameter('Geomorph','DoProfCurvMoments',DoProfCurvMoments,false);
               AParameter('Geomorph','DoMean',DoMean,false);
               AParameter('Geomorph','DoSTD',DoSTD,false);
               AParameter('Geomorph','DoSkew',DoSkew,false);
               AParameter('Geomorph','DoKurt',DoKurt,false);
               AParameter('Geomorph','SignedSlopeComponents',SignedSlopeComponents,true);
            {$EndIf}

            AParameterShortFloat('Geomorph','GeomorphSlopeCut1',GeomorphSlopeCut[1],10);
            AParameterShortFloat('Geomorph','GeomorphSlopeCut2',GeomorphSlopeCut[2],20);
            AParameterShortFloat('Geomorph','GeomorphSlopeCut3',GeomorphSlopeCut[3],30);
            AParameterShortFloat('Geomorph','GeomorphSlopeCut4',GeomorphSlopeCut[4],40);
            AParameterShortFloat('Geomorph','FabColorMin',FabColorMin,100);
            AParameterShortFloat('Geomorph','FabColorMax',FabColorMax,1000);
            AParameterShortFloat('Geomorph','CurveFlatVal',CurveFlatVal,0.105);
            AParameterShortFloat('Geomorph','SlopeCut1',SlopeCut1,50);
            AParameterShortFloat('Geomorph','SlopeCut2',SlopeCut2,25);
            AParameterShortFloat('Geomorph','SlopeCut3',SlopeCut3,12.5);
            AParameterShortFloat('Geomorph','ConvexCut',ConvexCut,50);
            AParameterShortFloat('Geomorph','RoughnessCut',RoughnessCut,50);
            AParameterShortFloat('Geomorph','SlopeClassTolerance',SlopeClassTolerance,1.0);
            AParameterShortFloat('Geomorph','ConvexClassTolerance',ConvexClassTolerance,0.01);
            AParameterShortFloat('Geomorph','MinSlopeToClassify',MinSlopeToClassify,1);
            AParameterShortFloat('Geomorph','MinDeltaZToClassify',MinDeltaZToClassify,0.5);
            AParameterShortFloat('Geomorph','MinSSOStrength',MinSSOStrength,2);
            AParameterShortFloat('Geomorph','MaxSSOFlat',MaxSSOFlat,4);
            AParameterShortFloat('Geomorph','HistogramTailClipSize',HistogramTailClipSize,0.5);
            AParameterShortFloat('Geomorph','MinWaveHeight',MinWaveHeight,10);
            AParameterShortFloat('Geomorph','FlatnessCutoff',FlatnessCutoff,12.0);
            AParameterShortFloat('Geomorph','OrganizationCutoff',OrganizationCutoff,1);
            AParameter('Geomorph','IncludeFractalMeasures',IncludeFractalMeasures,false);
            AParameter('Geomorph','IncludeGammaMeasures',IncludeGammaMeasures,true);
            AParameter('Geomorph','IncludeProfCMeasures',IncludeProfCMeasures,true);
            AParameter('Geomorph','IncludePlanCMeasures',IncludePlanCMeasures,true);
            AParameter('Geomorph','IncludeFabricMeasures',IncludeFabricMeasures,true);
            AParameter('Geomorph','IncludeSlopeMeasures',IncludeSlopeMeasures,true);
            AParameter('Geomorph','IncludeWavelength',IncludeWavelength,true);
            AParameter('Geomorph','IncludeBasicElevation',IncludeBasicElevation,true);
            AParameter('Geomorph','IncludeAdvancedElevation',IncludeAdvancedElevation,true);
            AParameter('Geomorph','IncludeMissingHoles',IncludeMissingHoles,true);
            AParameter('Geomorph','IncludeOpenness',IncludeOpenness,true);
            AParameter('Geomorph','GeomorphNewDB',GeomorphNewDB,true);
            AParameter('Geomorph','FabricAmplitudeDistance',FabricAmplitudeDistance,500);
            AParameter('Geomorph','DoFabricAmplitude',DoFabricAmplitude,false);
            AParameter('Geomorph','ElevMoments',ElevMoments,true);
            AParameter('Geomorph','SlopeMoments',SlopeMoments,true);
            AParameter('Geomorph','RoughnessMoments',RoughnessMoments,true);
            AParameter('Geomorph','PlanCurvMoments',PlanCurvMoments,false);
            AParameter('Geomorph','SlopeCurvMoments',SlopeCurvMoments,false);
            AParameter('Geomorph','GraphsOfMoments',GraphsOfMoments,true);
            AParameter('Geomorph','LongMoments',LongMoments,false);
            AParameter('Geomorph','CountHistograms',CountHistograms,true);
            AParameter('Geomorph','WavelengthCompDist',WavelengthCompDist,10000);
            AParameter('Geomorph','ShowElevFreq',ShowElevFreq,true);
            AParameter('Geomorph','ShowSlopeFreq',ShowSlopeFreq,true);
            AParameter('Geomorph','ShowElevSlope',ShowElevSlope,true);
            AParameter('Geomorph','ShowCumSlope',ShowCumSlope,true);
            AParameter('Geomorph','ShowAspectRose',ShowAspectRose,true);
            AParameter('Geomorph','ShowElevSlopeDeg',ShowElevSlopeDeg,false);
            AParameter('Geomorph','DuneSamplingInterval',DuneSamplingInterval,2500);
            AParameter('Geomorph','PeakPitPostings',PeakPitPostings,10);
            AParameter('Geomorph','WaveHtValuesNeeded',WaveHtValuesNeeded,4);
            AParameter('Geomorph','GrainLengthMultiple',GrainLengthMultiple,12);
            AParameter('Geomorph','GrainLineWidth',GrainLineWidth,2);
            AParameter('Geomorph','PlateauTolerance',PlateauTolerance,5);
            AParameter('Geomorph','StatGrafReverseYAxis',StatGrafReverseYAxis,false);
            AParameter('Geomorph','FlipHistogram',FlipHistogram,false);
            AParameter('Geomorph','FabColorByField',FabColorByField,false);
            AParameter('Geomorph','SSOallInTable',SSOallInTable,false);
            AParameter('Geomorph','IwashPikeCats',IwashPikeCats,16);
            AParameter('Geomorph','CorrectAspectTrue',CorrectAspectTrue,true);
            AParameter('Geomorph','DoWavelength',DoWaveLength,false);

            AParameter('Geomorph','OverWriteClassDEMs',OverWriteClassDEMs,false);
            AParameter('Geomorph','ExpandNeighborsRequired',ExpandNeighborsRequired,5);
            AParameter('Geomorph','ShrinkNeighborsRequired',ShrinkNeighborsRequired,5);
            AParameter('Geomorph','ExpandRadius',ExpandRadius,1);
            AParameter('Geomorph','ShrinkRadius',ShrinkRadius,1);
            AParameter('Geomorph','MinRegionPoints',MinRegionPoints,25);
            AParameter('Geomorph','LagInc',LagSearchRadius,50);
            AParameter('Geomorph','LagCenterShift',LagCenterShift,100);

            AParameter('Geomorph','FindWaveLength',FindWaveLength,true);
            AParameter('Geomorph','PlotCrest',PlotCrest,true);
            AParameter('Geomorph','FloodAlg',FloodAlg,1);
            AParameter('Geomorph','QuickSlopeSpacings',QuickSlopeSpacings,true);

            if (IniWhat = iniWrite) then IniFile.WriteInteger('Geomorph','RidgePointClassify',ord(RidgePointClassify));
            if (IniWhat = iniRead) then RidgePointClassify := tRidgeAlgorithm(IniFile.ReadInteger('Geomorph','RidgePointClassify',ord(raSimple2)));
            if (iniWhat = iniInit) then RidgePointClassify := raSimple2;

            with LocalOptimaOpts do begin
               AParameterFloat('LocalOpt','MaxSlope',MaxSlope,20);
               AParameter('LocalOpt','ColInc',ColInc,50);
               AParameter('LocalOpt','RowInc',RowInc,50);
               AParameter('LocalOpt','NPts',NPts,5);
               AParameter('LocalOpt','EdgeBuffer',EdgeBuffer,true);
               AParameter('LocalOpt','RoadMaskDistance',RoadMaskDistance,250);
               AParameter('LocalOpt','BitmapMask',BitmapMask,false);
               AParameter('LocalOpt','RoadMask',RoadMask,false);
               AParameter('LocalOpt','SlopeMask',SlopeMask,false);
               AParameter('LocalOpt','OptimaBoxSize',OptimaBoxSize,500);
               if (IniWhat = iniWrite) then IniFile.WriteInteger('LocalOpt','DEMRegion',ord(LocalOptimaOpts_DEMRegion));
               if (IniWhat = iniRead) then LocalOptimaOpts_DEMRegion := tDEMRegion(IniFile.ReadInteger('LocalOpt','DEMRegion',ord(drEntireDEM)));
               if (iniWhat = iniInit) then LocalOptimaOpts_DEMRegion := drEntireDEM;
            end;
            {$If Defined(RecordINIfiles)} WriteLineToDebugFile('GeomorphOptions out'); {$EndIf}
            {$IfDef MultipleCurvatureMethods}
               AParameter('Geomorph','CurvRegionSize',CurvRegionSize,1);
               if (IniWhat = iniWrite) then IniFile.WriteInteger('Geomorph','CurvatureMethod',ord(CurvatureMethod));
               if (IniWhat = iniRead) then CurvatureMethod := tCurvatureMethod(IniFile.ReadInteger('Geomorph','CurvatureMethod',ord(cmZevenbergenAndThorne)));
               if (iniWhat = iniInit) then CurvatureMethod := cmZevenbergenAndThorne;
            {$EndIf}
         end;
      {$EndIf}

     {$IfDef ExVariogram}
     {$Else}
         with MDINIfile,MDDef.VariogramOptionsRecord do begin
            AParameter('Variogram','DistanceOut',DistanceOut,25000);
            AParameter('Variogram','PointsRequired',PointsRequired,100);
            AParameter('Variogram','DoGraph',DoGraph,true);
            AParameter('Variogram','DoGamma',DoGamma,false);
            AParameter('Variogram','DoSlopes',DoSlopes,true);
            AParameter('Variogram','LogLog',LogLog,false);
            AParameter('Variogram','SemiVar',SemiVar,false);
            AParameter('Variogram','OldMethod',OldMethod,false);
            AParameter('Variogram','ShowTextOutput',ShowTextOutput,true);
            AParameter('Variogram','Skip',Skip,2);
            AParameter('Variogram','GraphSkip',GraphSkip,2);
         end;
      {$EndIf}

      {$IfDef ExDrainage}
      {$Else}
         with MDINIfile,MDDef do begin
            AParameter('Drain','DrainageSlopeAlgorithm',DrainageSlopeAlgorithm,smEightNeighborsUnweighted);
            AParameter('Drain','DrainageArrowSeparation',DrainageArrowSeparation,12);
            AParameter('Drain','DrainageArrowWidth',DrainageArrowWidth,2);
            AParameter('Drain','DrainageArrowLength',DrainageArrowLength,10);
            AParameter('Drain','DrainageArrowWidth',DrainageVectAvgArrowWidth,2);
            AParameter('Drain','DrainageArrowLength',DrainageVectAvgArrowLength,5);
            AParameter('Drain','DrainageSeedRadius',DrainageSeedRadius,1);
            AParameter('Drain','DrainagePointSlope',DrainagePointSlope,true);
            AParameter('Drain','DrainageVectorAverage',DrainageVectorAverage,false);
            AColorParameter('Drain','DrainageArrowColor',DrainageArrowColor,claBlue);
            AColorParameter('Drain','DrainageArrowColor',DrainageVectAvgArrowColor,claCyan);
            AParameterShortFloat('Drain','ReservoirTop',ReservoirTop,2.5);
            AParameterShortFloat('Drain','ReservoirLowestLevel',ReservoirLowestLevel,0.5);
            AParameterShortFloat('Drain','FloodStep',FloodStep,0.1);
         end;
      {$EndIf}

     {$IfDef ExTerrainClassify}
     {$Else}
         {$If Defined(RecordINIfiles)} WriteLineToDebugFile('TerrainClassify'); {$EndIf}
         with MDINIfile,MDDef do begin
            AParameterShortFloat('PitsPeaks','PeakRadius',PeakRadius,100);
            AParameterShortFloat('PitsPeaks','PeakHeight',PeakHeight,100);
            AParameterShortFloat('PitsPeaks','PitHeight',PitHeight,100);
            AParameterShortFloat('PitsPeaks','PitRadius',PitRadius,100);
            AParameterShortFloat('PitsPeaks','SpireRadius',SpireRadius,100);
            AParameterShortFloat('PitsPeaks','SpireHeight',SpireHeight,100);
            AParameterShortFloat('PitsPeaks','AcrossWallHeight',AcrossWallHeight,0.75);
            AParameterShortFloat('PitsPeaks','AlongWallHeight',AlongWallHeight,0.15);
            AParameter('PitsPeaks','WallRegion',WallRegion,2);
            AParameter('PitsPeaks','RoofRegion',RoofRegion,2);
            AParameter('PitsPeaks','SpireNeighborTolerance',SpireNeighborTolerance,8);
            AParameterShortFloat('PitsPeaks','PeakRoofMinSlope',PeakRoofMinSlope,10);
            AParameterShortFloat('PitsPeaks','PeakRoofSlopeTol',PeakRoofSlopeTol,5);
            AParameterShortFloat('PitsPeaks','PeakRoofAspectTol',PeakRoofAspectTol,10);
            AParameter('PitsPeaks','PeakRoofNeighbors',PeakRoofNeighbors,5);
            AParameterShortFloat('PitsPeaks','BuldingMaxSlope',BuildingMaxSlope,10);
            AParameterShortFloat('PitsPeaks','MinRoofEdgeSlope',MinRoofEdgeSlope,100);
            AParameterShortFloat('PitsPeaks','MinBldgSize',MinBldgSize,10);
            AParameterShortFloat('PitsPeaks','MaxBldgSize',MaxBldgSize,25);

            AParameter('PitsPeaks','OverWriteFeatureDBs',OverWriteFeatureDBs,true);
            ASymbol('PitsPeaks','PitSymbol',PitSymbol,FilledBox,claRed,3);
            ASymbol('PitsPeaks','PeakSymbol',PeakSymbol,FilledBox,claLime,3);
            ASymbol('PitsPeaks','SpireSymbol',SpireSymbol,FilledBox,claBlue,3);
            ASymbol('PitsPeaks','WallSymbol',WallSymbol,FilledBox,claRed,3);
            ASymbol('PointCloud','BuldingEdge',BuildingEdge,FilledBox,claRed,1);

           with MDDef.MEMPowerDefaults do begin
               AParameter('Geomorph','NumProfiles', NumProfiles,15);
               AParameter('Geomorph','NumPoles', NumPoles,35);
               AParameter('Geomorph','LogLogPlot', LogLogPlot,true);
               AParameterShortFloat('Geomorph','FirstFreq', FirstFreq,0.01);
               AParameterShortFloat('Geomorph','LastFreq', LastFreq,0.3);
               AParameterShortFloat('Geomorph','ValidDataRequired', ValidDataRequired,0.9);
           end;
         end;
      {$EndIf}

      {$IfDef ExPrincComp}
      {$Else}
         with MDINIfile,MDDef do begin
            AParameter('PrincComp','PCCorrelation',PCCorrelation,true);
            AParameter('PrincComp','PCEigenValues',PCEigenValues,true);
            AParameter('PrincComp','PCVarCovar',PCVarCovar,false);
            AParameter('PrincComp','PCResults',PCResults,true);
            AParameterShortFloat('PrincComp','MinPCToShow',MinPCToShow,0.25);
         end;
      {$EndIf}

      {$IfDef RecordINIfiles} WriteLineToDebugFile('ProcessIniFile after Geomorph'); {$EndIf}
   end;

   procedure LOSoptions;
   begin
      {$If Defined(RecordINIfiles)} WriteLineToDebugFile('LOSOptions'); {$EndIf}
      with MDIniFile,MDDef do begin
         AParameter('LOS','ShowOpenGLLOSline',ShowOpenGLLOSline,true);
         AParameter('LOS','MissingDataBlocksLOS',MissingDataBlocksLOS,false);
         AParameter('LOS','LOSShowPitch',LOSShowPitch,false);
         AParameter('LOS','ShowMaskedAirspace',ShowMaskedAirspace,false);
         AParameter('LOS','DrawLOSProtractor',DrawLOSProtractor,false);
         AParameter('LOS','ShowObserverMaskingCircle',ShowObserverMaskingCircle,false);
         AParameter('LOS','ShowDEMTerrainProfile',ShowDEMTerrainProfile,true);
         AParameter('LOS','ShowLOSDataBase',ShowLOSDataBase,false);
         AParameter('LOS','SimpleTopoProfilesOnly',SimpleTopoProfilesOnly,false);
         AParameter('LOS','LOSVisShowLine',LOSVisShowLine,true);
         AParameter('LOS','DrawLOS',DrawLOS,true);
         AParameter('LOS','ProfileShowZLevel',ProfileShowZLevel,false);
         AParameter('LOS','ProfileShowSeaLevel',ProfileShowSeaLevel,true);
         AParameter('LOS','LOSLeftTixLabels',LOSLeftTixLabels,true);

         AParameter('LOS','LOSVisible',LOSVisible,true);
         AParameter('LOS','DefaultTargetTerrainHug',DefaultTargetTerrainHug,true);
         AParameter('LOS','DefaultObserverTerrainHug',DefaultObserverTerrainHug,true);
         AParameter('LOS','LOSMaxElev',LOSMaxElev,3000);
         AParameter('LOS','LOSMinElev',LOSMinElev,0);
         AParameter('LOS','ForceLOSHorizTickSpacing',ForceLOSHorizTickSpacing,false);
         AParameter('LOS','LOSHorizTickSpacing_KM',LOSHorizTickSpacing_KM,true);
         AParameter('LOS','LOSParallelProfileSep',LOSParallelProfileSep,500);
         AParameter('LOS','ShowAllProfiles',ShowAllProfiles,true);
         AParameter('LOS','TerrainProfileWidth',TerrainProfileWidth,1);
         AParameter('LOS','MaskedAirspaceWidth',MaskedAirspaceWidth,1);
         AParameter('LOS','LOSConnectionWidth',LOSConnectionWidth,1);
         AParameter('LOS','PitchLineWidth',PitchLineWidth,1);
         AParameter('LOS','LOSVisLineWidth',LOSVisLineWidth,2);
         AParameter('LOS','DefaultTargetASL',DefaultTargetASL,2400);
         AParameter('LOS','DefaultObserverASL',DefaultObserverASL,2400);
         AParameter('LOS','NumLOSParallelProfile',NumLOSParallelProfile,5);
         AParameter('LOS','DropDownPerProfile',DropDownPerProfile,10);
         AParameter('LOS','LabelMultipleProf',LabelMultipleProf,true);
         AParameter('LOS','DoGrazingFields',DoGrazingFields,false);
         AParameter('LOS','LOSClHt',LOSClHt,500);
         AParameter('LOS','LOSClWid',LOSClWid,800);

         AColorParameter('LOS','OpenGLLosLineColor',OpenGLLosLineColor,claRed);
         AColorParameter('LOS','TerrainProfileColor',TerrainProfileColor,claGreen);
         AColorParameter('LOS','MaskedAirspaceColor',MaskedAirspaceColor,claBlue);
         AColorParameter('LOS','LOSConnectionColor',LOSConnectionColor,claBlack);
         AColorParameter('LOS','PitchLineColor',PitchLineColor,claBlack);

         AParameterShortFloat('LOS','LOSPixelSize',LOSPixelSize,30);
         AParameterShortFloat('LOS','LOSVertExag',LOSVertExag,1);
         AParameterShortFloat('LOS','LosHorizTick',LosHorizTick,1);
         AParameterShortFloat('LOS','OpenGLLosLineWidth',OpenGLLosLineWidth,5);
         AParameterShortFloat('LOS','LOSMinPitch',LOSMinPitch,-5);
         AParameterShortFloat('LOS','LOSMaxPitch',LOSMaxPitch,5);

         if (IniWhat = iniWrite) then IniFile.WriteInteger('LOS','ShowPointCloundOnProfile',ord(spcNone));
         if (IniWhat = iniRead) then ShowPointCloundOnProfile := tShowPointCloundOnProfile(IniFile.ReadInteger('LOS','ShowPointCloundOnProfile',ord(spcNone)));
         if (iniWhat = iniInit) then ShowPointCloundOnProfile := spcNone;
         ASymbol('LOS','VisPtSymbol',VisPtSymbol,FilledBox,claLime,4);
         ASymbol('LOS','MaskPtSymbol',MaskPtSymbol,FilledBox,claRed,4);
        {$IfDef ExPointCloud}
        {$Else}
            AParameter('LOS','LOSshowIntervisibilityFromLAS',LOSshowIntervisibilityFromLAS,false);
            AParameter('LOS','LOSshowPointCloudRangePoints',LOSshowPointCloudRangePoints,false);
            AParameter('LOS','LOSshowPointCloudRangeLines',LOSshowPointCloudRangeLines,false);
            AParameter('LOS','PointCloudFloorWidth',PointCloudFloorWidth,1);
            AParameter('LOS','PointCloudCeilingWidth',PointCloudCeilingWidth,1);
            AColorParameter('LOS','PointCloudFloorColor',PointCloudFloorColor,ConvertTColorToPlatformColor(clBrown));
            AColorParameter('LOS','PointCloudCeilingColor',PointCloudCeilingColor,claLime);
        {$EndIf}
        {$IfDef ExFresnel}
        {$Else}
            AParameter('LOS','DrawFresnel',DrawFresnel,false);
            AParameter('LOS','FresnelZone1Width',FresnelZone1Width,1);
            AParameter('LOS','FresnelZone2Width',FresnelZone2Width,1);
            AParameterShortFloat('LOS','FresnelFreq',FresnelFreq,100);
            AParameterShortFloat('LOS','RadioK',RadioK,1.33);
            AColorParameter('LOS','FresnelZone1Color',FresnelZone1Color,claMaroon);
            AColorParameter('LOS','FresnelZone2Color',FresnelZone2Color,claPurple);
        {$EndIf}
      end;
   end;


   procedure TigerDefaults;
   begin
      {$IfDef ExTiger}
      {$Else}
      with  MDIniFile,MDDef,TigrDef do begin
         AParameter('Tiger','AutoTigerOnImages',AutoTigerOnImages,false);
         AParameter('Tiger','ShowTIGERButton',ShowTIGERButton,true);
         AParameter('Tiger','AutoTigerOnDEMs',AutoTigerOnDEMs,false);
         AParameter('Tiger','MaxAutoTigerCounties',MaxAutoTigerCounties,25);
         AParameter('Tiger','MajorRoadWidth',MajorRoadWidth,4);
         AParameter('Tiger','WaterWidth1',WaterWidth1,2);
         AParameter('Tiger','DrawRailroad',DrawRailroad,true);
         AParameter('Tiger','DrawPipeline',DrawPipeline,true);
         AParameter('Tiger','AppearMajorRoad',AppearMajorRoad,10000);
         AParameter('Tiger','AppearRoadCat2',AppearRoadCat2,250);
         AParameter('Tiger','AppearRoadCat3',AppearRoadCat3,150);
         AParameter('Tiger','DrawMajorRoad',DrawMajorRoad,true);
         AParameter('Tiger','DrawRoadCat2',DrawRoadCat2,true);
         AParameter('Tiger','DrawRoadCat3',DrawRoadCat3,true);
         AParameter('Tiger','DrawRoadCat4',DrawRoadCat4,true);
         AParameter('Tiger','DrawRoadCat5',DrawRoadCat5,true);
         AParameter('Tiger','DrawRoadCat6',DrawRoadCat6,true);
         AParameter('Tiger','DrawRoadCat7',DrawRoadCat7,true);
         AParameter('Tiger','DrawWaterCat1',DrawStreams,true);
         AParameter('Tiger','DrawWaterCat2',DrawCoastline,true);
         AParameter('Tiger','DrawPowerLine',DrawPowerLine,true);
         AParameter('Tiger','DrawBoundary',DrawBoundary,false);
         AParameter('Tiger','WaterWidth2',WaterWidth2,1);
         AParameter('Tiger','BoundaryWidth',BoundaryWidth,1);
         AParameter('Tiger','RoadCat2Width',RoadCat2Width,2);
         AParameter('Tiger','RoadCat3Width',RoadCat3Width,2);
         AParameter('Tiger','RoadCat4Width',RoadCat4Width,1);
         AParameter('Tiger','RoadCat5Width',RoadCat5Width,1);
         AParameter('Tiger','RoadCat6Width',RoadCat6Width,1);
         AParameter('Tiger','RoadCat7Width',RoadCat7Width,1);
         AParameter('Tiger','RailroadWidth',RailroadWidth,2);
         AParameter('Tiger','PowerLineWidth',PowerLineWidth,2);
         AParameter('Tiger','PipelineWidth',PipelineWidth,2);
         AParameter('Tiger','AppearRoadCat4',AppearRoadCat4,100);
         AParameter('Tiger','AppearRoadCat5',AppearRoadCat5,60);
         AParameter('Tiger','AppearRoadCat6',AppearRoadCat6,40);
         AParameter('Tiger','AppearRoadCat7',AppearRoadCat7,20);
         AParameter('Tiger','AppearStream',AppearStream,1000);
         AParameter('Tiger','AppearCoast',AppearCoast,10000);
         AParameter('Tiger','AutoAppear',AutoAppear,true);
         AParameter('Tiger','LabelEveryTigerFeature',LabelEveryTigerFeature,false);
         AParameter('Tiger','DrawLabels',DrawLabels,true);
         AParameter('Tiger','AppearLabels',AppearLabels,5);
         AParameter('Tiger','ShowNeighborsOnTIGERCounty',ShowNeighborsOnTIGERCounty,false);
         AParameter('Tiger','MakeTigerMapGrayscale',MakeTigerMapGrayscale,false);
         AParameter('Tiger','SubdueTigerBase',SubdueTigerBase,false);
         AParameter('Tiger','FilterAllTigerRoads',FilterAllTigerRoads,true);
         AColorParameter('Tiger','MajorRoadColor',MajorRoadColor,claRed);
         AColorParameter('Tiger','RoadCat2Color',RoadCat2Color,RGBtrip(190,0,0));
         AColorParameter('Tiger','RoadCat3Color',RoadCat3Color,RGBtrip(160,0,0));
         AColorParameter('Tiger','RoadCat4Color',RoadCat4Color,RGBtrip(140,0,0));
         AColorParameter('Tiger','RoadCat5Color',RoadCat5Color,RGBtrip(120,0,0));
         AColorParameter('Tiger','RoadCat6Color',RoadCat6Color,RGBtrip(90,0,0));
         AColorParameter('Tiger','RoadCat7Color',RoadCat7Color,claTeal);
         AColorParameter('Tiger','WaterColor1',WaterColor1,claBlue);
         AColorParameter('Tiger','WaterColor2',WaterColor2,claNavy);
         AColorParameter('Tiger','BoundaryColor',BoundaryColor,claBlack);
         AColorParameter('Tiger','RailroadColor',RailroadColor,claAqua);
         AColorParameter('Tiger','PowerLineColor',PowerLineColor,claFuchsia);
         AColorParameter('Tiger','PipelineColor',PipelineColor,claMaroon);
         {$IfDef RecordINIfiles} WriteLineToDebugFile('ProcessIniFile after TIGER');     {$EndIf}
      end;
      {$EndIf}
   end;

   procedure PointCloudDefaults;
   begin
     {$IfDef ExPointCloud}
     {$Else}
      with MDIniFile,MDDef do begin
         AParameter('PointCloud','ClassifyParams',lastoolsClassifyParams,'');
         AParameter('PointCloud','GroundParams',LastoolsGroundParams,'-town');
         AParameter('PointCloud','CatFilter',ls.CatFilter,0);
         AParameter('PointCloud','UserDataRecordFilter',ls.UserDataRecordFilter,1);

         AParameter('PointCloud','RetFilter',ls.RetFilter,0);
         AParameter('PointCloud','ScanAngleFilter',ls.ScanAngleFilter,0);
         AParameter('PointCloud','PointIDFilter',ls.PointIDFilter,1);
         AParameter('PointCloud','BadPointFilter',ls.BadPointFilter,1);
         AParameter('PointCloud','ScanAngleFiltered',ls.ScanAngleFiltered,false);
         AParameter('PointCloud','Filters',ls.Filters,false);
         AParameter('PointCloud','DiscardLowPointsNoise',ls.DiscardLowPointsNoise,true);
         AParameter('PointCloud','DiscardHighNoise',ls.DiscardHighNoise,true);
         AParameter('PointCloud','DiscardOverlap',ls.DiscardOverlap,true);
         AParameter('PointCloud','AutoZoomOpenLas',AutoZoomOpenLas,false);
         AParameter('PointCloud','GroundClassOnly',ls.GroundClassOnly,false);
         AParameter('PointCloud','AirReturnsOnly',ls.AirReturnsOnly,false);
         AParameter('PointCloud','SingleReturnsOnly',ls.SingleReturnsOnly,false);
         AParameter('PointCloud','FirstReturnsOnly',ls.FirstReturnsOnly,false);
         AParameter('PointCloud','LastReturnsOnly',ls.LastReturnsOnly,false);
         AParameter('PointCloud','AssumeLAS14classes',ls.AssumeLAS14classes,true);
         AParameter('PointCloud','DTMoption',DTMoption,2);
         AParameter('PointCloud','DefLidarElevMap',DefLidarElevMap,mtGrayReflect);

         AParameter('PointCloud','LidarPanClose',LidarPanClose,10);
         AParameter('PointCloud','LidarPanFar',LidarPanFar,500);

         AParameter('PointCloud','BlockSize',BlockSize,255);
         AParameter('PointCloud','MaxPtsInCell',MaxPtsInCell,32000);
         AParameter('PointCloud','UseNoise',UseNoise,false);
         AParameter('PointCloud','UseOverlap',UseOverlap,false);
         AParameter('PointCloud','AutoThinByBlock',AutoThinByBlock,false);
         AParameter('PointCloud','MinPtsRequiredPercentile',MinPtsRequiredPercentile,100);
         AParameter('PointCloud','NeedViewer3Dversion2',NeedViewer3Dversion2,true);
         AParameter('PointCloud','LidarGridProjection',LidarGridProjection,UTMBasedDEM);
         AParameter('PointCloud','WKTLidarProj',WKTLidarProj,'');
         AParameter('PointCloud','ShowCloudOutlines',ShowCloudOutlines,false);
         AParameter('PointCloud','AutoDrawMapLAS', AutoDrawMapLAS,true);
         AParameter('PointCloud','AutoReDrawMapLAS', AutoReDrawMapLAS,true);
         AParameter('PointCloud','LASPC99', LASPC99,true);
         AParameter('PointCloud','AutoShowSlice',AutoShowSlice,false);
         AParameter('PointCloud','SlicerIHSMerge',SlicerIHSMerge,false);
         AParameter('PointCloud','SliceShapeWide',SliceShapeWide,2);
         AParameter('PointCloud','ClouderXSize',ClouderXSize,-99);
         AParameter('PointCloud','AutoSliceMore',AutoSliceMore,false);
         AParameter('PointCloud','ExperimentalSliceOptions',ExperimentalSliceOptions,false);
         AParameter('PointCloud','ClouderYSize',ClouderYSize,-99);
         AParameter('PointCloud','CloudMemoryThinFactor',CloudMemoryThinFactor,1);
         AParameter('PointCloud','CloudSliceThinFactor',CloudSliceThinFactor,1);
         AParameter('PointCloud','CloudMapThinFactor',CloudMapThinFactor,1);
         AParameter('PointCloud','CloudOpenGLThinFactor',CloudOpenGLThinFactor,1);
         AParameter('PointCloud','CheckLasOnMap',CheckLasOnMap,true);
         AParameter('PointCloud','DifferentFloorSliceThickness',DifferentFloorSliceThickness,true);
         AParameter('PointCloud','PtSlicerDefView',PtSlicerDefView,1);
         AParameter('PointCloud','LOSSliceBuffer',LOSSliceBuffer,5);
         AParameter('PointCloud','SlicerUseCorrectScaling',SlicerUseCorrectScaling,false);
         AParameter('PointCloud','LASMaxSubsetSize',LASMaxSubsetSize,75);
         AParameter('PointCloud','ShowCloudDensity',ShowCloudDensity,false);
         AParameter('PointCloud','LOSShowVoxelDensity',LOSShowVoxelDensity,false);
         AParameter('PointCloud','LabelLAStiles',LabelLAStiles,false);
         AParameter('PointCloud','PickLASDirs',PickLASDirs,true);
         AParameter('PointCloud','LasPointDensityBasedOn',LasPointDensityBasedOn,1);

         AParameter('PointCloud','LoadLASclassificationToMemory',LoadLASclassificationToMemory,true);
         AParameter('PointCloud','LoadLASreturnNumberToMemory',LoadLASreturnNumberToMemory,true);
         AParameter('PointCloud','LoadLASRGBToMemory',LoadLASRGBToMemory,true);

         AParameter('PointCloud','LasThinFactor',LasThinFactor,1);
         AParameter('PointCloud','MemoryPointCloudMaxPts',MemoryPointCloudMaxPts,TheMaxPointsMemPtCloud);
         AParameter('PointCloud','PCDefaultFilter',PCDefaultFilter,1);
         AParameter('PointCloud','BuildingMinNumLower',BuildingMinNumLower,2);
         AParameter('PointCloud','BuildingMaxNumLower',BuildingMaxNumLower,2);
         AParameter('PointCloud','CloudThinFactor',CloudThinFactor,50);
         AParameter('PointCloud','LasOpacity',LasOpacity,50);
         AParameter('PointCloud','LasDEMPixelIs',LasDEMPixelIs,1);
         AParameter('PointCloud','LasElevChecks',LasElevChecks,false);
         AParameter('PointCloud','LasLegend',LasLegend,true);
         AParameter('PointCloud','MakePCFloor',MakePCFloor,true);
         AParameter('PointCloud','MakePCCeiling',MakePCCeiling,true);
         AParameter('PointCloud','PCAutoFillHoles',PCAutoFillHoles,false);

         AParameter('PointCloud','PointCloudColorFilter',PointCloudColorFilter,false);
         AParameter('PointCloud','RedLow',RedLow,143);
         AParameter('PointCloud','RedHigh',RedHigh,210);
         AParameter('PointCloud','GreenLow',GreenLow,170);
         AParameter('PointCloud','GreenHigh',GreenHigh,225);
         AParameter('PointCloud','BlueLow',BlueLow,160);
         AParameter('PointCloud','BlueHigh',BlueHigh,215);
         AParameter('PointCloud','LatLongSlices',LatLongSlices,false);
         AParameter('PointCloud','ForceSquarePixels',ForceSquarePixels,true);

         AParameter('PointCloud','LVISGabonGraphs',LVISGabonGraphs,1);
         AParameterFloat('PointCloud','CloudFilterTolerance',CloudFilterTolerance,0.5);
         AParameterShortFloat('PointCloud','DefLidarXGridSize',DefLidarXGridSize,1);
         AParameterShortFloat('PointCloud','DefLidarYGridSize',DefLidarYGridSize,1);
         AParameterShortFloat('PointCloud','DefLidarGeoGridSizeX',DefLidarGeoGridSizeX,1);
         AParameterShortFloat('PointCloud','DefLidarGeoGridSizeY',DefLidarGeoGridSizeY,1);
         AParameterShortFloat('PointCloud','DefWKTGridSize',DefWKTGridSize,50);
         AParameterShortFloat('PointCloud','DSMpercentile',DSMpercentile,50);
         AParameterShortFloat('PointCloud','DTMpercentile',DTMpercentile,50);
         AParameterShortFloat('PointCloud','Midpercentile',Midpercentile,50);
         AParameterShortFloat('PointCloud','BuildingMaxHeight',BuildingMaxHeight,100);
         AParameterShortFloat('PointCloud','CloudSliceThick',CloudSliceThick,1);
         AParameterShortFloat('PointCloud','LowValidZinPointCloud',LowValidZinPointCloud,-500);
         AParameterShortFloat('PointCloud','MaxValidZinPointCloud',MaxValidZinPointCloud,9999);
         AParameterShortFloat('PointCloud','MaxValidHAGZinPointCloud',MaxValidHAGinPointCloud,500);
         AParameterShortFloat('PointCloud','FloorSliceThick',FloorSliceThick,5);
         AParameterShortFloat('PointCloud','CloudSliceJump',CloudSliceJump,5);
         AParameterShortFloat('PointCloud','BuildingMinHeight',BuildingMinHeight,3);
         AParameterShortFloat('PointCloud','BuildingMaxHeight',BuildingMaxHeight,100);
         AParameterShortFloat('PointCloud','MCC_scale',ls.MCC_scale,1.5);
         AParameterShortFloat('PointCloud','MCC_thresh',ls.MCC_thresh,0.3);

         ASymbol('PointCloud','CloudSym1',MDDef.CloudSymbol[1],FilledBox,claLime,3);
         ASymbol('PointCloud','CloudSym2',MDDef.CloudSymbol[2],FilledBox,claRed,3);
         ASymbol('PointCloud','CloudSym3',MDDef.CloudSymbol[3],FilledBox,claBlue,3);
         ASymbol('PointCloud','CloudSym4',MDDef.CloudSymbol[4],FilledBox,claMaroon,3);
         ASymbol('PointCloud','CloudSym5',MDDef.CloudSymbol[5],FilledBox,claGreen,3);

         ASymbol('PointCloud','CloudMapSym1',MDDef.CloudMapSymbol[1],FilledBox,claLime,1);
         ASymbol('PointCloud','CloudMapSym2',MDDef.CloudMapSymbol[2],FilledBox,clared,1);
         ASymbol('PointCloud','CloudMapSym3',MDDef.CloudMapSymbol[3],FilledBox,claBlue,1);
         ASymbol('PointCloud','CloudMapSym4',MDDef.CloudMapSymbol[4],FilledBox,claMaroon,1);
         ASymbol('PointCloud','CloudMapSym5',MDDef.CloudMapSymbol[5],FilledBox,claGreen,1);

         ASymbol('PointCloud','SlicerDigPtSym',SlicerDigPtSym,FilledBox,claRed,3);
         ASymbol('PointCloud','SlicePtSym',SlicePtSym,FilledBox,claRed,2);
         AColorParameter('PointCloud','SliceShapeColor',SliceShapeColor,claRed);
         if (IniWhat = iniWrite) then IniFile.WriteInteger('PointCloud','LasColorCoding',ord(MDDef.ls.ColorCoding));
         if (IniWhat = iniRead) then MDDef.ls.ColorCoding := tLasColorCoding(IniFile.ReadInteger('PointCloud','LasColorCoding',ord(lasccElevation)));
         if (iniWhat = iniInit) then MDDef.ls.ColorCoding := lasccElevation;

         AParameter('IceSat2','ic2.DoFilter',icesat2.DoFilter,true);
         AParameter('IceSat2','ic2.BoxSize',icesat2.BoxSize,1);
         AParameter('IceSat2','ic2.PCPtsRequired',icesat2.PCPtsRequired,25);
         AParameter('IceSat2','ic2.UseBeam1',icesat2.UseBeam[1],true);
         AParameter('IceSat2','ic2.UseBeam2',icesat2.UseBeam[2],true);
         AParameter('IceSat2','ic2.UseBeam3',icesat2.UseBeam[3],true);
         AParameter('IceSat2','ic2.UseBeam4',icesat2.UseBeam[4],true);
         AParameter('IceSat2','ic2.UseBeam5',icesat2.UseBeam[5],true);
         AParameter('IceSat2','ic2.UseBeam6',icesat2.UseBeam[6],true);
         AParameter('IceSat2','ic2.BeamConf1',icesat2.BeamConfidence[1],3);
         AParameter('IceSat2','ic2.BeamConf2',icesat2.BeamConfidence[2],3);
         AParameter('IceSat2','ic2.BeamConf3',icesat2.BeamConfidence[3],3);
         AParameter('IceSat2','ic2.BeamConf4',icesat2.BeamConfidence[4],3);
         AParameter('IceSat2','ic2.BeamConf5',icesat2.BeamConfidence[5],3);
         AParameter('IceSat2','ic2.BeamConf6',icesat2.BeamConfidence[6],3);
      end;
      {$EndIf}

      with MDIniFile,MDDef do begin
         AParameterShortFloat('ExtTools','WBGroundClassRadius',WBGroundClassRadius,2);
         AParameterShortFloat('ExtTools','WBSegFilterRadius',WBSegFilterRadius,5);
         AParameterShortFloat('ExtTools','WBDeNoiseRadius',WBDeNoiseRadius,10);
         AParameterShortFloat('ExtTools','WBDenoiseElevDiff',WBDenoiseElevDiff,25);
      end;


      {$IfDef ExPowerLine}
      {$Else}
         with MDIniFile,MDDef do begin
            AParameter('PowerLine','PL_FirstVacantVoxel',PL_FirstVacantVoxel,2);
            AParameter('PowerLine','PL_LastVacantVoxel',PL_LastVacantVoxel,15);
            AParameter('PowerLine','PL_MaxOccupiedVoxels',PL_MaxOccupiedVoxels,1);
            AParameter('PowerLine','PL_NeighborsRequired',PL_NeighborsRequired,2);
            AParameter('PowerLine','PL_PlotOccupied',PL_PlotOccupied,true);
            AParameter('PowerLine','PL_PlotFirstNeighbor',PL_PlotFirstNeighbor,true);
            AParameter('PowerLine','PL_PlotSecondNeighbor',PL_PlotSecondNeighbor,true);
         end;
      {$EndIf}

      {$IfDef RecordINIfiles} WriteLineToDebugFile('ProcessIniFile after PointCloud'); {$EndIf}
   end;


   procedure SonarDefaults;
   begin
     {$IfDef ExSidescan}
     {$Else}
      with MDIniFile,MDDef,SonarMapDef do begin
         AParameterFloat('Sonar','MinDraft',MinDraft,1.5);
         AParameterFloat('Sonar','InstrumentDepth',InstrumentDepth,2);
         AParameterFloat('Sonar','TowDistance',TowDistance,50);
         AParameterFloat('Sonar','MinPC',MinPC,1);
         AParameterFloat('Sonar','MaxPC',MaxPC,99);
         AParameterFloat('Sonar','ChirpTVG',ChirpTVG,1);
         AParameterFloat('Sonar','ChirpGain',ChirpGain,2.0);
         AColorParameter('Sonar','SSGridColor',SSGridColor,claLime);

         AParameter('Sonar','BottomValue',BottomValue,5000);
         AParameter('Sonar','WhiteIsStrongReturn',WhiteIsStrongReturn,true);
         AParameter('Sonar','SlantRangeCorrect',SlantRangeCorrect,true);
         AParameter('Sonar','DefaultPixelsWide',DefaultPixelsWide,500);
         AParameter('Sonar','DefaultPixelsHigh',DefaultPixelsHigh,1200);
         AParameter('Sonar','SideDefFilter',SideDefFilter,1);
         AParameter('Sonar','CustomPalette',CustomPalette,false);
         AParameter('Sonar','SidescanLayback',SidescanLayback,30);
         AParameter('Sonar','GridSpaceAcross',GridSpaceAcross,10);
         AParameter('Sonar','GridSpaceAlong',GridSpaceAlong,10);
         AParameter('Sonar','OverlayGrid',OverlayGrid,true);
         AParameter('Sonar','SSGridWidth',SSGridWidth,1);
         AParameter('Sonar','ShowChirpBottomTrack',ShowChirpBottomTrack,false);
         AParameter('Sonar','ChirpXThinning',ChirpXThinning,1);
         AParameter('Sonar','ChirpXDupe',ChirpXDupe,1);
         AParameter('Sonar','ChirpYThinning',ChirpYThinning,1);
         AParameter('Sonar','ChirpReturnsToShow',ChirpReturnsToShow,1200);
      end;
      {$EndIf}
   end;


   procedure WeaponsFanAlg;
   begin
     {$IfDef ExViewshed}
     {$Else}
      with  MDIniFile,MDDef,wf do begin
         AParameterShortFloat('WeapFanAlg','MaskAreaInterval',MaskAreaInterval,30.0);
         AParameterShortFloat('WeapFanAlg','MaskRaySpacingDeg',MaskRaySpacingDeg,5.0);
         AParameterShortFloat('WeapFanAlg','FanDEMSpaceMultiple',FanDEMSpaceMultiple,0.5);
         AParameterShortFloat('WeapFanAlg','FanMapSpaceMultiple',FanMapSpaceMultiple,0.5);
         AParameterShortFloat('WeapFanAlg','SmartSwitchOver',SmartSwitchOver,25000);
         AParameter('WeapFanAlg','FanViewerMustBeGrid',FanViewerMustBeGrid,false);
         AParameter('WeapFanAlg','FanTargetMustBeGrid',FanTargetMustBeGrid,false);
         AParameter('WeapFanAlg','ClosestBlockingDistance',ClosestBlockingDistance,0);
         AParameter('WeapFanAlg','RadialSkip',RadialSkip,1);
         // AParameter('WeaponFan','LOSAlgorithm',LOSAlgorithm,losMicrodemFractional);
         if (IniWhat = iniWrite) then IniFile.WriteInteger('WeapFanAlg','LOSAlgorithm',ord(LOSAlgorithm));
         if (IniWhat = iniRead) then LOSAlgorithm := tLOSAlgorithm(IniFile.ReadInteger('WeapFanAlg','LOSAlgorithm',ord(losMicrodemFractional)));
         if (iniWhat = iniInit) then LOSAlgorithm := losMicrodemFractional;

         //AParameter('WeaponFan','FanMethod',FanMethod,fmRadialIHS);
         if (IniWhat = iniWrite) then IniFile.WriteInteger('WeapFanAlg','FanMethod',ord(MDDef.wf.FanMethod));
         if (IniWhat = iniRead) then FanMethod := tFanMethod(IniFile.ReadInteger('WeapFanAlg','FanMethod',ord(fmRadialIHS)));
         if (iniWhat = iniInit) then FanMethod := fmRadialIHS;

         //AParameter('WeaponFan','FanShowVisible',FanShowVisible,fsVisible);
         if (IniWhat = iniWrite) then IniFile.WriteInteger('WeapFanAlg','FanShowVisible',ord(FanShowVisible));
         if (IniWhat = iniRead) then FanShowVisible := tFanShow(IniFile.ReadInteger('WeaponFaAlgorithm','FanShowVisible',ord(fsVisible)));
         if (iniWhat = iniInit) then FanShowVisible := fsVisible;

         //AParameter('WeaponFan','ElevInterpolation',ElevInterpolation,piBilinear);
         if (IniWhat = iniWrite) then IniFile.WriteInteger('WeapFanAlg','ElevInterpolation',ord(ElevInterpolation));
         if (IniWhat = iniRead) then ElevInterpolation := tElevInterpolation(IniFile.ReadInteger('WeapFanAlg','ElevInterpolation',ord(piBilinear)));
         if (iniWhat = iniInit) then ElevInterpolation := piBilinear;

         //AParameter('WeaponFan','StraightAlgorithm',StraightAlgorithm,saSmart);
         if (IniWhat = iniWrite) then IniFile.WriteInteger('WeapFanAlg','StraightAlgorithm',ord(StraightAlgorithm));
         if (IniWhat = iniRead) then StraightAlgorithm := tStraightAlgorithm(IniFile.ReadInteger('WeapFanAlg','StraightAlgorithm',ord(saSmart)));
         if (iniWhat = iniInit) then StraightAlgorithm := saSmart;

         //AParameter('WeaponFan','FanCurvAlg',FanCurvAlg,vcTM5441);
         if (IniWhat = iniWrite) then IniFile.WriteInteger('WeapFanAlg','FanCurvAlg',ord(FanCurvAlg));
         if (IniWhat = iniRead) then FanCurvAlg := tVerticalCurvAlg(IniFile.ReadInteger('WeapFanAlg','FanCurvAlg',ord(vcTM5441)));
         if (iniWhat = iniInit) then FanCurvAlg := vcTM5441;
      end;
      {$EndIf}
   end;


   procedure WeaponsFanDefaults;
   begin
     {$IfDef ExViewshed}
     {$Else}
      with MDIniFile,MDDef,wf do begin
         AParameterShortFloat('WeaponFan','ObsAboveGround', ObsAboveGround,2.0);
         AParameterShortFloat('WeaponFan','TargetAboveGround',TargetAboveGround,2.0);
         AParameterShortFloat('WeaponFan','MaskObsRange',MaskObsRange,2500);
         AParameterShortFloat('WeaponFan','DefWeaponsMinRange',DefWeaponsMinRange,0);
         AParameterShortFloat('WeaponFan','StartFanAngle',StartFanAngle,0);
         AParameterShortFloat('WeaponFan','EndFanAngle',EndFanAngle,360);
         AParameterShortFloat('WeaponFan','FanUpAngle',FanUpAngle,89);
         AParameterShortFloat('WeaponFan','FanDownAngle',FanDownAngle,-89);
         AColorParameter('WeaponFan','RangeCircleColor',RangeCircleColor,claRed);
         AColorParameter('WeaponFan','FanOutLineColor',FanOutLineColor,claBlack);
         AColorParameter('WeaponFan','FanColor',FanColor,claLime);
         AColorParameter('WeaponFan','MaskColor',MaskColor,claRed);
         AColorParameter('WeaponFan','ViewshedMixedPixelColor',ViewshedMixedPixelColor,claGreen);
         AParameter('WeaponFan','ShowSensorMaps',ShowSensorMaps,true);
         AParameter('WeaponFan','DisplayFanBitmaps',DisplayFanBitmaps,false);
         AParameter('WeaponFan','DoReqFlyHigh',DoReqFlyHigh,true);
         AParameter('WeaponFan','DoReqAntHigh',DoReqAntHigh,true);
         AParameter('WeaponFan','DoLOSProfile',DoLOSProfile,true);
         AParameter('WeaponFan','DoGrazingAngle',DoGrazingAngle,false);
         AParameter('WeaponFan','WeaponRouteSeparation',WeaponRouteSeparation,100);
         AParameter('WeaponFan','ShowFanLocation',ShowFanLocation,true);
         AParameter('WeaponFan','AmbushObserverNAP',AmbushObserverNAP,false);
         AParameter('WeaponFan','AmbushTargetNAP',AmbushTargetNAP,false);
         AParameter('WeaponFan','SaveFanRadialProfiles',SaveFanRadiaProfiles,false);
         AParameter('WeaponFan','FanOutLineWidth',FanOutLineWidth,2);
         AParameter('WeaponFan','OutlineFans',OutlineFans,false);
         AParameter('WeaponFan','FanOpacity',FanOpacity,50);
         AParameter('WeaponFan','ShowViewshedMixedPixels',ShowViewshedMixedPixels,true);
         AParameter('WeaponFan','ReportFanCoverage',ReportFanCoverage,true);
         AParameter('WeaponFan','GraphFanCoverage',GraphFanCoverage,true);
         AParameter('WeaponFan','FanMapZoom',FanMapZoom,50);
         AParameter('WeaponFan','MultipleFanAlgorithmsToUse',MultipleFanAlgorithmsToUse,3);
         AParameter('WeaponFan','OpenVegGridMap',OpenVegGridMap,false);
         AParameter('WeaponFan','UseVegInFans',UseVegInFans,true);
         AParameter('WeaponFan','DisplayAmbushRoute',DisplayAmbushRoute,true);
         AParameter('WeaponFan','ShowRegionBoundaries',ShowRegionBoundaries,true);
         AParameter('WeaponFan','AmbushCoverage',AmbushCoverage,true);
         AParameter('WeaponFan','ShowGridVegEffects',ShowGridVegEffects,true);
         AParameter('WeaponFan','FanSaveExt',FanSaveExt,'.gif');
         ASymbol('WeaponFan','DefaultCenterSymbol',DefaultCenterSymbol,Cross,claRed,3);

         //AParameter('WeaponFan','AmbushFanShow',AmbushFanShow,fsVisible);
         if (IniWhat = iniWrite) then IniFile.WriteInteger('WeaponFan','AmbushFanShow',ord(AmbushFanShow));
         if (IniWhat = iniRead) then AmbushFanShow := tFanShow(IniFile.ReadInteger('WeaponFan','AmbushFanShow',ord(fsVisible)));
         if (iniWhat = iniInit) then AmbushFanShow := fsVisible;

         //AParameter('WeaponFan','ViewshedColorScheme',ViewshedColorScheme,LegChloropleth);
         if (IniWhat = iniWrite) then IniFile.WriteInteger('WeaponFan','ViewshedColorScheme',ord(ViewshedColorScheme));
         if (IniWhat = iniRead) then ViewshedColorScheme := tLegendColors(IniFile.ReadInteger('WeaponFan','ViewshedColorScheme',ord(LegChloropleth)));
         if (iniWhat = iniInit) then ViewshedColorScheme := LegChloropleth;

         //AParameter('WeaponFan','ViewshedPaletteName',ViewshedPaletteName,BluetoDarkOrange,  12 steps');
         if (IniWhat = iniWrite) then IniFile.WriteString('WeaponFan','ViewshedPaletteName',ViewshedPaletteName);
         if (IniWhat = iniRead) then ViewshedPaletteName := IniFile.ReadString('WeaponFan','ViewshedPaletteName','BluetoDarkOrange,  12 steps');
         if (iniWhat = iniInit) then ViewshedPaletteName := 'BluetoDarkOrange,  12 steps';

         //AParameter('WeaponFan','FanPickMode',FanPickMode,fpSingle);
         if (IniWhat = iniWrite) then IniFile.WriteInteger('WeaponFan','FanPickMode',ord(FanPickMode));
         if (IniWhat = iniRead) then FanPickMode := tFanPickMode(IniFile.ReadInteger('WeaponFan','FanPickMode',ord(fpSingle)));
         if (iniWhat = iniInit) then FanPickMode := fpSingle;

         //AParameter('WeaponFan','CurvAlg',CurvAlg,vcTM5441);
         if (IniWhat = iniWrite) then IniFile.WriteInteger('WeaponFan','CurvAlg',ord(CurvAlg));
         if (IniWhat = iniRead) then CurvAlg := tVerticalCurvAlg(IniFile.ReadInteger('WeaponFan','CurvAlg',ord(vcTM5441)));
         if (iniWhat = iniInit) then CurvAlg := vcTM5441;

         {$IfDef RecordINIfiles}  WriteLineToDebugFile('ProcessIniFile after Weapons'); {$EndIf}
      end;
      {$EndIf}
   end;

   procedure DEMDefaultParameters;
   begin
      with MDIniFile,MDDef do begin
         AParameterShortFloat('DEM','MinElevPercentile',MinElevPercentile,1);
         AParameterShortFloat('DEM','MaxElevPercentile',MaxElevPercentile,99);
         AParameterShortFloat('DEM','MinImagePercentile',MinImagePercentile,1);
         AParameterShortFloat('DEM','MaxImagePercentile',MaxImagePercentile,97);
         AParameterShortFloat('DEM','PreferArcSecSpace',PreferArcSecSpace,0.1);
         AParameterShortFloat('DEM','PreferUTMSpace',PreferUTMSpace,10);
         AParameterShortFloat('DEM','PreferWKTSpace',PreferWKTSpace,1000);
         AParameter('DEM','ShowDEMGridCoords',ShowDEMGridCoords,false);
         AParameter('DEM','AutoFillHoles',AutoFillHoles,false);
         AParameter('DEM','MissingToSeaLevel',MissingToSeaLevel,false);
         AParameter('DEM','SeaLevelToMissing',SeaLevelToMissing,false);
         AParameter('DEM','NoDEMInterpolations',NoDEMInterpolations,false);
         AParameter('DEM','PromptToSaveNewDEMs',PromptToSaveNewDEMs,false);
         AParameter('DEM','DefaultDEMFilter',DefaultDEMFilter,1);
         AParameter('DEM','WrapETOPO',WrapETOPO,false);
         AParameter('DEM','DoubleEtopoImport',DoubleEtopoImport,false);
         AParameter('DEM','FavDEMSeries1',FavDEMSeries1,'');
         AParameter('DEM','FavDEMSeries2',FavDEMSeries2,'');
         AParameter('DEM','ConvertMGtoMD',ConvertMGtoMD,false);
         AParameter('DEM','DeleteAuxTiffFiles',DeleteAuxTiffFiles,true);

         if (IniWhat = iniWrite) then IniFile.WriteInteger('DEM','DEMZunits',ord(DEMZunits));
         if (IniWhat = iniRead) then DEMZunits := tDEMZunits(IniFile.ReadInteger('DEM','DEMZunits',ord(zuMeters)));
         if (iniWhat = iniInit) then DEMZunits := zuMeters;

         {$IfDef RecordINIfiles} WriteLineToDebugFile('ProcessIniFile after DEM'); {$EndIf}
      end;
   end;


   procedure StatcolParameters;
   begin
      {$IfDef ExStratcol}
      {$Else}
      with  MDIniFile,MDDef, ColDef do begin
         AParameter('StratCol','BoundaryTicks',BoundaryTicks,true);
         AParameter('StratCol','RaggedRightMargin',RaggedRightMargin,false);
         AParameter('StratCol','AutoShortLabels',AutoShortLabels,false);
         AParameter('StratCol','FancyUnitBases',FancyUnitBases,true);
         AParameter('StratCol','OverPrintLithology',OverPrintLithology,false);
         AParameter('StratCol','VariableResistance',VariableResistance,false);
         AParameter('StratCol','IgnoreCorrelationThickness',IgnoreCorrelationThickness,true);
         AParameter('StratCol','ColumnVerbiage',ColumnVerbiage,true);
         AParameter('StratCol','EnterLatLong',EnterLatLong,true);
         AParameter('StratCol','RapidColEntry',RapidColEntry,false);
         AParameter('StratCol','ShowAgeBar',ShowAgeBar,true);
         AParameter('StratCol','PixelsColumnWide',PixelsColumnWide,100);
         AParameter('StratCol','DefaultMyBitmapWidth',DefaultMyBitmapWidth,600);
         AParameter('StratCol','DefaultMyBitmapHeight',DefaultMyBitmapHeight,500);
         AParameter('StratCol','DefaultFontSize',DefaultFontSize,10);
         AParameter('StratCol','DefaultScaleFontSize',DefaultScaleFontSize,10);
         AParameter('StratCol','UsersMaxUnits',UsersMaxUnits,250);
         AParameter('StratCol','ColumnSeparation',ColumnSeparation,25);
         AParameter('StratCol','AbsThickness',AbsThickness,false);
         AParameter('StratCol','RightSideThickness',RightSideThickness,false);
         AParameterFloat('StratCol','DefaultThickness',DefaultThickness,400);
         AParameterFloat('StratCol','ScaleLabelOffset',ScaleLabelOffset,0);

         //AParameter('StratCol','ZoneTicks',ZoneTicks,NoZones);
         if (IniWhat = iniWrite) then IniFile.WriteInteger('StratCol','ZoneTicks',ord(ZoneTicks));
         if (IniWhat = iniRead) then ZoneTicks := ZoneSampleType(IniFile.ReadInteger('StratCol','ZoneTicks',ord(NoZones)));
         if (iniWhat = iniInit) then ZoneTicks := NoZones;

         //AParameter('StratCol','ThickLabelling',ThickLabelling,StartTop);
         if (IniWhat = iniWrite) then IniFile.WriteInteger('StratCol','ThickLabelling',ord(ThickLabelling));
         if (IniWhat = iniRead) then ThickLabelling := ThickLabelOption(IniFile.ReadInteger('StratCol','ThickLabelling',ord(StartTop)));
         if (iniWhat = iniInit) then ThickLabelling := StartTop;

         //AParameter('StratCol','ThickLabelUnits',ThickLabelUnits,luMeters);
         if (IniWhat = iniWrite) then IniFile.WriteInteger('StratCol','ThickLabelUnits',ord(ThickLabelUnits));
         if (IniWhat = iniRead) then ThickLabelUnits := tLabelUnits(IniFile.ReadInteger('StratCol','ThickLabelUnits',ord(luMeters)));
         if (iniWhat = iniInit) then ThickLabelUnits := luMeters;

         //AParameter('StratCol','TextDirection',TextDirection,textHorizontal);
         if (IniWhat = iniWrite) then IniFile.WriteInteger('StratCol','TextDirection',ord(TextDirection));
         if (IniWhat = iniRead) then TextDirection := tTextDirection(IniFile.ReadInteger('StratCol','TextDirection',ord(textHorizontal)));
         if (iniWhat = iniInit) then TextDirection := textHorizontal;

         //AParameter('StratCol','TextPlacement',TextPlacement,textInside);
         if (IniWhat = iniWrite) then IniFile.WriteInteger('StratCol','TextPlacement',ord(TextPlacement));
         if (IniWhat = iniRead) then TextPlacement := tTextPlacement(IniFile.ReadInteger('StratCol','TextPlacement',ord(textInside)));
         if (iniWhat = iniInit) then TextPlacement := textInside;

         //AParameter('StratCol','TextLabels',TextLabels,textNone);
         if (IniWhat = iniWrite) then IniFile.WriteInteger('StratCol','TextLabels',ord(TextLabels));
         if (IniWhat = iniRead) then TextLabels := tTextLabels(IniFile.ReadInteger('StratCol','TextLabels',ord(textNone)));
         if (iniWhat = iniInit) then TextLabels := textNone;

         //AParameter('StratCol','AlignColumns',AlignColumns,acTop);
         if (IniWhat = iniWrite) then IniFile.WriteInteger('StratCol','AlignColumns',ord(AlignColumns));
         if (IniWhat = iniRead) then AlignColumns := tAlignColumns(IniFile.ReadInteger('StratCol','AlignColumns',ord(acTop)));
         if (iniWhat = iniInit) then AlignColumns := acTop;

         //AParameter('StratCol','ColorAndPatternOptions',ColorAndPatternOptions,patBlackAndWhitePatterns);
         if (IniWhat = iniWrite) then IniFile.WriteInteger('StratCol','ColorAndPatternOptions',ord(ColorAndPatternOptions));
         if (IniWhat = iniRead) then ColorAndPatternOptions := tColorAndPatternOptions(IniFile.ReadInteger('StratCol','ColorAndPatternOptions',ord(patBlackAndWhitePatterns)));
         if (iniWhat = iniInit) then ColorAndPatternOptions := patBlackAndWhitePatterns;

         //AParameter('StratCol','LocationLabel',LocationLabel,llLatLong);
         if (IniWhat = iniWrite) then IniFile.WriteInteger('StratCol','LocationLabel',ord(LocationLabel));
         if (IniWhat = iniRead) then LocationLabel := tLocationLabel(IniFile.ReadInteger('StratCol','LocationLabel',ord(llLatLong)));
         if (iniWhat = iniInit) then LocationLabel := llLatLong;
      end;
      {$IfDef RecordINIfiles}
      WriteLineToDebugFile('ProcessIniFile after Stratcol');
      {$EndIf}

      {$EndIf}
   end;


   procedure ProgramFileSettings;
   begin
      with MDIniFile,MDDef do begin
            AParameter('Files','MainMapData',MainMapData,'');
            AParameter('Files','LastSatDir',LastSatDir,'');
            AParameter('Files','LastDEMName',LastDEMName,'');
            AParameter('Files','LastImageName',LastImageName,'');
            AParameter('Files','Last DB',LastDataBase,'');
            AParameter('Files','PhotoDir',PhotoDir,'');
            AParameter('Files','LastLidarMult',LastLidarMulti,'');
            AParameter('Files','LastOSMoverlay',LastOSMoverlay,'');
            AParameter('Files','MapLibDir',MapLibDir,'');
            AParameter('Files','lastools_bindir',lastools_bindir,'');

            {$IfDef ExGDAL}
            {$Else}
               AParameter('GDAL','GDALtools_Dir',GDALtools_Dir,'');
               AParameter('GDAL','DontBugMeAboutGDAL',DontBugMeAboutGDAL,true);
               AParameter('GDAL','RouteGeotiffExportGDAL',RouteGeotiffExportGDAL,true);
               AParameterShortFloat('GDAL','GDALThinFactor',GDALThinFactor,0.1);
            {$EndIf}

            AParameter('Files','SagaCMD',SagaCMD,'H:\gis_software\saga-8.2.1_x64\saga_cmd.exe');
            AParameter('Files','WhiteBoxFName',WhiteBoxFName,'');

            AParameter('DEMIX','DEMIX_criterion_tolerance_fName',DEMIX_criterion_tolerance_fName,'');
            AParameter('DEMIX','DEMIX_base_dir',DEMIX_base_dir,'');
            AParameter('DEMIX','DEMIX_xsize',DEMIX_xsize,900);
            AParameter('DEMIX','DEMIX_ysize',DEMIX_ysize,600);
            AParameter('DEMIX','DEMIX_DoCHM',DEMIX_DoCHM,true);
            AParameter('DEMIX','DEMIX_DoAirOrDirt',DEMIX_DoAirOrDirt,true);
            AParameter('DEMIX','DEMIX_DoElevDiff',DEMIX_DoElevDiff,true);
            AParameter('DEMIX','DEMIX_DoSlopeDiff',DEMIX_DoSlopeDiff,true);
            AParameter('DEMIX','DEMIX_DoRuffDiff',DEMIX_DoRuffDiff,true);
            AParameter('DEMIX','DEMIX_DoHalfSecDEMs',DEMIX_DoHalfSecDEMs,true);
            AParameter('DEMIX','DEMIX_DoElevParamGraphs',DEMIX_DoElevParamGraphs,true);
            AParameter('DEMIX','DEMIXCompositeImage',DEMIXCompositeImage,true);
            AParameterShortFloat('DEMIX','DEMIXSimpleTolerance',DEMIXSimpleTolerance,2.0);

            {$IfDef ExMrSID}
            {$Else}
              //AParameter('Files','MrSIDDecode',MrSIDDecodeName,'');
              //AParameter('Files','MrSIDInfo',MrSidInfoName,'');
            {$EndIf}

            {$IfDef ExWMS}
            {$Else}
               AParameter('Files','FavoriteWMS',FavoriteWMS,'');
            {$EndIf}

            {$IfDef MSWindows}
               AParameter('Files','mcc_lidarFName',mcc_lidarFName,'');
               AParameter('Files','SmallScaleWorldOutlines',SmallScaleWorldOutlines,'');
               AParameter('Files','MedScaleWorldOutlines',MedScaleWorldOutlines,'');
               AParameter('Files','LargeScaleWorldOutlines',LargeScaleWorldOutlines,'');

               AParameter('Files','LastCompressedFile',LastCompressedFile,'');
               AParameter('Files','WriteDEMDir',WriteDEMDir,'');
               AParameter('Files','WriteSatDir',WriteSatDir,'');
               AParameter('Files','CopyFilesFromDir',CopyFilesFromDir,'');
               AParameter('Files','CopyFilesToDir',CopyFilesToDir,'');
               AParameter('Files','LastSavedLOSfName',LastSavedLOSfName,'');
               AParameter('Files','LastTrainSetFName',LastTrainSetFName,'');
               AParameter('Files','GADMDir',GADMDir,'');
               AParameter('Files','PointCloud',VasaProjectFName,'');
               AParameter('Files','VecMapName',VectorMapName,'');
               AParameter('Files','Viewsheds',SaveViewshedDir,'');
               AParameter('Files','StateSHP',StateGISFileName,'');
               AParameter('Files','CountySHP',CountyGISFileName,'');
               AParameter('Files','RoadSHP',HighwayGISFileName,'');
               AParameter('Files','RiverSHP',RiversGISFileName,'');
               AParameter('Files','LastScanMap',LastScanMapName,'');
               AParameter('Files','LastOverlay',LastOverlayName,'');
               AParameter('Files','WorldFileOverlay',LastWorldFileOverlay,'');
               AParameter('Files','ETOPO_DEM', ETOPODEMName,'');
               AParameter('Files','BlueMarbleFName', BlueMarbleFName,'');
               AParameter('Files','LastFanTable',LastFanTable,'');
               AParameter('Files','CloudBitmap',CloudBitmapName,'');
               AParameter('Files','LastDesktop',LastDesktop,'');
               AParameter('Files','ElevDBTable',ElevationFixedPalette,'');
               AParameter('Files','PreferFilter',PreferFilter,'');

               AParameter('Files','VasaKeyPoints',VasaKeyPoints,'');
               AParameter('Files','VasaSideLimits',VasaSideLimits,'');
               AParameter('Files','VasaBinFreq',VasaBinFreq,'');
               AParameter('Files','VasaBoxOutlines',VasaBoxOutlines,'');
               AParameter('Files','VasaArtDBfName',VasaArtDBfName,'');
               AParameter('Files','VasaArtefactsFName',VasaArtefactsFName,'');
               AParameter('Files','GeoidDiffFName',GeoidDiffFName,'');

               {$IfDef ExTIN}
               {$Else}
                  AParameter('Files','LastTIN',LastTINName,'');
               {$EndIf}

               {$IfDef ExGeoStats}
               {$Else}
                  AParameter('Files','Geomorph_Atlas',GeomorphAtlasDir,'');
               {$EndIf}

               {$IfDef ExComplexGeoStats}
               {$Else}
                  AParameter('Files','BlockInputDir',BlockInputDir,'');
                  AParameter('Files','BlockOutputDir',BlockOutputDir,'');
               {$EndIf}

               {$IfDef ExPointCloud}
               {$Else}
                  AParameter('Files','LastLidarDir',LastLidarDirectory,'');
                  AParameter('Files','LastLidar2Dir',LastLidar2Directory,'');
                  AParameter('Files','LastLidar3Dir',LastLidar3Directory,'');
                  AParameter('Files','LastLidar4Dir',LastLidar4Directory,'');
                  AParameter('Files','LastLidar5Dir',LastLidar5Directory,'');
                  AParameter('Files','SlicerMask',DEMDefs.SlicerMaskField,'');
                  AParameter('Files','LastLVIS',LastLVISFileName,'');
               {$EndIf}

               {$IfDef ExHyperspectral}
               {$Else}
                  AParameter('Files','Last_Hyp_file',LastHypFile,'');
               {$EndIf}

               {$IfDef ExSidescan}
               {$Else}
                  AParameter('Files','SidescanFName',InputSideScanFileName,'');
                  AParameter('Files','SidescanIndexName',SidescanIndexFName,'');
                  AParameter('Files','ChirpPath',ChirpDataPath,'');
                  AParameter('Files','SidescanPath',SideDataPath,'');
                  AParameter('Files','Ocean Drift',OceanDriftFName,'');
                  AParameter('Files','Ocean Tide',OceanTideFName,'');
               {$EndIf}

               {$IfDef ExVegDensity}
               {$Else}
                  AParameter('Files','LastVegDensity1fName',LastVegDensity1fName,'');
                  AParameter('Files','LastVegDensity2fName',LastVegDensity2fName,'');
                  AParameter('Files','LastVegGrid',LastVegGrid,'');
               {$EndIf}

               {$IfDef ExMultiGrid}
               {$Else}
                  AParameter('Files','LastMultigrid1',LastMultigrid1,'');
                  AParameter('Files','LastMultigrid2',LastMultigrid2,'');
                  AParameter('Files','LastMultigrid3',LastMultigrid3,'');
               {$EndIf}

              {$IfDef ExPLSS}
              {$Else}
                  AParameter('Files','PLSS_Baseline',DefaultPLSSBaseline,'');
                  AParameter('Files','PLSS_file_1',PLSSFile[1],'');
                  AParameter('Files','PLSS_file_2',PLSSFile[2],'');
                  AParameter('Files','PLSS_file_3',PLSSFile[3],'');
                  AParameter('Files','PLSS_file_4',PLSSFile[4],'');
                  AParameter('Files','PLSS_file_5',PLSSFile[5],'');
             {$EndIf}

               {$IfDef ExStratcol}
               {$Else}
                  AParameter('Files','LastStratcol',LastStratColFile,'');
                  AParameter('Files','LithFileName',LithFileName,'');
               {$EndIf}

               {$IfDef SQLiteDefaultDBs}
                  AParameter('Files','ForceOverwriteDB',ForceOverwriteDB,true);
               {$EndIf}
           {$EndIf}
      {$IfDef RecordINIfiles} WriteLineToDebugFile('ProcessIniFile after Files'); {$EndIf}

     end;
  end;

   procedure MiscSettings;
   begin
      with MDIniFile,MDDef do begin
         {$IfDef IncludeFMXOptions}
            AParameter('FMX','DefMapSizeIndex',DefMapSizeIndex,1);
            AParameter('FMX','DefMapTypeIndex',MDDef.DefMapTypeIndex,5);
            AParameter('FMX','AutoStartGPS',AutoStartGPS,false);
            AParameter('FMX','DefScreenRotation',DefScreenRotation,1);
            AParameter('FMX','DefGPSUpdate',DefGPSUpdate,2);
            AParameter('FMX','GPSColorIndex',GPSColorIndex,0);
            AParameter('FMX','DefGPSSave',DefGPSSave,2);
         {$EndIf}

         AParameterShortFloat('Misc','OffsetDistance',OffsetDistance,5);
         AParameterShortFloat('Misc','FilterRadius',FilterRadius,2500);
         AParameterShortFloat('Misc','MaxMergeSlope',MaxMergeSlope,0.45);
         AParameterShortFloat('Misc','MinPercentile',MinPercentile,2);
         AParameterShortFloat('Misc','MaxPercentile',MaxPercentile,98);

         AParameterShortFloat('Misc','TideGaugeOffset',TideGaugeOffset,0.235);
         AParameterShortFloat('Misc','TopCutLevel',TopCutLevel,1);
         AParameterShortFloat('Misc','BottomCutLevel',BottomCutLevel,-1);

         AParameter('Misc','DEMIX_Full',DEMIX_Full,100);
         AParameterShortFloat('Misc','SlopeFlatBoundary',SlopeFlatBoundary,12.5);
         AParameterShortFloat('Misc','SlopeGentleBoundary',SlopeGentleBoundary,25);
         AParameterShortFloat('Misc','SlopeSteepBoundary',SlopeSteepBoundary,50);

         AParameter('Misc','LandTypePointsNeeded',LandTypePointsNeeded,50);

         AParameter('Misc','ColorizeInPlace',ColorizeInPlace,true);
         AParameter('Misc','DeleteFIT',DeleteFIT,true);
         AParameter('Misc','BlackLimit',BlackLimit,25);
         AParameter('Misc','Gray tolerance',GrayTolerance,25);
         AParameter('Misc','DefaultSaveImageType',DefaultSaveImageType,5);
         AParameter('Misc','DefaultReadImageType',DefaultReadImageType,1);
         AParameter('Misc','XPixAspect',XPixAspect,10);
         AParameter('Misc','YPixAspect',YPixAspect,8);
         AParameter('Misc','PictureMode',PictureMode,1);

         AParameter('Misc','VerifyAllWorldFiles',VerifyAllWorldFiles,false);
         //AParameter('Misc','HydrologyEnforceProfile',HydrologyEnforceProfile,true);
         AParameter('Misc','tnHeight',tnHeight,150);
         AParameter('Misc','tnQuality',tnQuality,95);
         AParameter('Misc','AutoCloseIndexMaps',AutoCloseIndexMaps,false);
         //AParameter('Misc','MaxBuildingGrid',MaxBuildingGrid,256);
         AParameter('Misc','TargetsAlongLine',TargetsAlongLine,true);
         AParameter('Misc','GrayScaleMerges',GrayScaleMerges,true);
         AParameter('Misc','TerrainCatPercentages',TerrainCatPercentages,false);
         AParameter('Misc','IHSTerrainCategory',IHSTerrainCategory,false);
         AParameter('Misc','OpenMultipleVectorMaps',OpenMultipleVectorMaps,false);
         AParameter('Misc','ShowGridDiffMap',ShowGridDiffMap,true);
         AParameter('Misc','ShowGridDiffHistogram',ShowGridDiffHistogram,false);
         AParameter('Misc','ShowScatterPlot',ShowScatterPlot,false);
         AParameter('Misc','ProgressTimeBins',ProgressTimeBins,20);
         AParameter('Misc','LogTCPcommands',LogTCPcommands,true);
         AParameter('Misc','LogTCPResults',LogTCPResults,true);
         AParameter('Misc','FillHoleDiameter',FillHoleRadius,2);
         AParameter('Misc','LandCoverMaskSize',LandCoverMaskSize,2);
         AParameter('Misc','PrinterMapLength',PrinterMapLength,2);
         AParameter('Misc','NumOffsets',NumOffsets,1);

         AColorParameter('Misc','ReplaceBorderColor',ReplaceBorderColor,claLime);
         AColorParameter('Misc','OffsetColor',OffsetColor,claBlack);
         AParameter('Misc','OffsetLineWidth',OffsetLineWidth,3);
         AParameter('Misc','RedistrictEvenness',RedistrictEvenness,5);
         AParameter('Misc','DrawRangeCircles',DrawRangeCircles,false);
         AParameter('Misc','PointSeparation',PointSeparation,1000);
         AParameter('Misc','DifferentiateHolesAndEdges',DifferentiateHolesAndEdges,false);
         AParameter('Misc','UseSeaLevel',UseSeaLevel,false);
         AParameter('Misc','DefaultVectorFilter',DefaultVectorFilter,1);
         AParameter('Misc','DefaultDRGFilter',DefaultDRGFilter,0);
         AParameter('Misc','ShowContourNeighborhood',ShowContourNeighborhood,false);
         AParameter('Misc','ShowMenus',ShowMenus,true);
         AParameter('Misc','AspectBoxRegion',AspectBoxRegion,false);
         AParameter('Misc','XAspect',XAspect,16);
         AParameter('Misc','YAspect',YAspect,9);
         AParameter('Misc','MergeInt',MergeInt,210);
         AParameter('Misc','MergeHue',MergeHue,195);
         AParameter('Misc','CumulativeGraph',CumulativeGraph,false);
         //AParameter('Misc','ShowMapToolbar',ShowMapToolbar,true);
         AParameter('Misc','AutoElevationReset',AutoElevationReset,true);
         AParameter('Misc','AutomaticNewMovieNames',AutomaticNewMovieNames,false);
         AParameter('Misc','DrapeExactly',DrapeExactly,false);
         AParameter('Misc','ShowMasks',ShowMasks,false);
         AParameter('Misc','LabelRouteTurningPoints',LabelRouteTurningPoints,true);
         AParameter('Misc','FigureCoverageOfRoads',FigureCoverageOfRoads,true);
         AParameter('Misc','NumMasksToAdd',NumMasksToAdd,3);
         AParameter('Misc','RotatingEarthOutlines',RotatingEarthOutlines,true);
         AParameter('Misc','ShowSDonElevSlope',ShowSDonElevSlope,true);
         AParameter('Misc','StayAwake',StayAwake,true);
         AParameter('Misc','ReverseZFields',ReverseZFields,false);

         {$IfDef VCL}
         AParameter('Misc','SB1PanelWidths[0]',SB1PanelWidths[0],200);
         AParameter('Misc','SB1PanelWidths[1]',SB1PanelWidths[1],360);
         AParameter('Misc','SB1PanelWidths[2]',SB1PanelWidths[2],240);
         AParameter('Misc','SB1PanelWidths[3]',SB1PanelWidths[3],300);
         {$EndIf}

         AParameter('Misc','GrayscaleChannels',GrayscaleChannels,false);
         AParameter('Misc','MinRGBColor',MinRGBColor,50);
         AParameter('Misc','MaxRGBColor',MaxRGBColor,200);
         AParameter('Misc','RangeRGBColor',RangeRGBColor,150);
         AParameter('Misc','DefElevsPercentile',DefElevsPercentile,true);

         {$IfDef VCL}
         AParameter('Misc','MaxDebugLinesToKeep',MaxDebugLinesToKeep,5000);
         {$Else}
         AParameter('Misc','MaxDebugLinesToKeep',MaxDebugLinesToKeep,1250);
         {$EndIf}
         AParameter('Misc','FinalLinesToKeep',FinalLinesToKeep,10);
         MDDef.InitialLinesToKeep := 7;
         AParameter('Misc','MDRecordDebugLog',MDRecordDebugLog,true);

         AParameter('Misc','MaskCode',MaskCode,1);
         AParameter('Misc','MaskMapShow',MaskMapShow,1);
         AParameter('Misc','XYZProduct',XYZProduct,2);

         if (IniWhat = iniWrite) then IniFile.WriteInteger('Misc','AutoOpen',ord(AutoOpen));
         if (IniWhat = iniRead) then AutoOpen := tAutoOpen(IniFile.ReadInteger('Misc','AutoOpen',ord(aoNothing)));
         if (iniWhat = iniInit) then AutoOpen := aoNothing;

         if (IniWhat = iniWrite) then IniFile.WriteInteger('Misc','ProgramOption',ord(ProgramOption));
         if (IniWhat = iniRead) then ProgramOption := tProgramOption(IniFile.ReadInteger('Misc','ProgramOption',ord(ExpertProgram)));
         if (iniWhat = iniInit) then ProgramOption := ExpertProgram;
      end;
   end;

   procedure KoppenSettings;
   begin
     {$IfDef ExGeography}
     {$Else}
         with MDIniFile,MDDef do begin
            AParameter('Geography','FirstSunlightDay',FirstSunlightDay,1);
            AParameter('Geography','LastSunLightDay',LastSunLightDay,365);
            AParameter('Geography','SunlightMapInterval',SunlightMapInterval,30);
            AParameter('Geography','SunlightPrecision',SunlightPrecision,5);
            AParameter('Geography','SunlightSingleDay',SunlightSingleDay,2);
            AParameter('Geography','VerifyTimeZone',VerifyTimeZone,false);
            AParameter('Geography','UTCOffset',UTCOffset,-4);
            AParameter('Geography','TZFromLong',TZFromLong,true);
            //AParameter('Geography','WindMonth',WindMonth0based,0);
            //AParameter('Geography','SunExtremes',SunExtremes,false);
            AParameter('Geography','MoonPhase',MoonPhase,true);
            AParameter('Geography','RiseSet',RiseSet,true);
            AParameter('Geography','MoveGeographyDBMemory', MoveGeographyDBMemory,false);

            with KoppenOpts do begin
               AParameter('Koppen','Width',KopWidth,300);
               AParameter('Koppen','Height',KopHeight,200);
               AParameter('Koppen','ShowTempAndRain',ShowTempAndRain,true);
               AParameter('Koppen','ShowLatLong',ShowLatLong,true);
               AParameter('Koppen','ShowElevation',ShowElevation,true);
               AParameter('Koppen','ShowSunRiseSet',ShowSunRiseSet,false);
               AParameter('Koppen','KoppenFontSize',KoppenFontSize,12);
               AParameter('Koppen','MaxTemp',MaxTemp,40);
               AParameter('Koppen','MapPrecip',MaxPrecip,70);
            end;
         end;
     {$EndIf}
   end;


   procedure GazSettings;
   begin
      {$IfDef ExGazetteer}
      {$Else}
      with MDIniFile,MDDef do begin
         AParameter('Gaz','DefaultGazetteerType',DefaultGazetteerType,0);
         AParameter('Gaz','ShiftGazPeaks',ShiftGazPeaks,true);
         AParameter('Gaz','ShiftGazPeaksDist',ShiftGazPeaksDist,250);
         AParameter('Gaz','GazPeakObsUp',GazPeakObsUp,25);
         AParameter('Gaz','AutoLabelGaz',AutoLabelGaz,false);
         AParameter('Gaz','LabelGazOnMap',LabelGazOnMap,true);
         AParameter('Gaz','GazMarkPeaksPerspective',GazMarkPeaksPerspective,true);
         AParameter('Gaz','GazMarkPeaksPerpsOnMap',GazMarkPeaksPerpsOnMap,true);
         AParameter('Gaz','ConvergingViews',ConvergingViews,false);
         AParameter('Files','LastGazFile',LastGazFile,'');
         AParameter('Menus','UseGazetteer',UseGazetteer,true);
      end;
      {$EndIf}
   end;


   procedure ReflectanceSettings;
   begin
      with MDIniFile,MDDef do begin
         AParameter('Reflect','UseRefDirs',MDDef.UseRefDirs,1);
         AParameter('Reflect','WaterCheck',MDDef.WaterCheck,false);
         AParameter('Reflect','LakeCheck',MDDef.LakeCheck,false);
         AParameter('Reflect','AviationDangerColors',MDDef.AviationDangerColors,False);
         AParameter('Reflect','MergeSat',MDDef.MergeSat,40);
         AParameter('Reflect','AutoGrayScaleReflectance',MDDef.AutoGrayScaleReflectance,true);
         AParameterShortFloat('Reflect','RefPhi',MDDef.RefPhi,335);
         AParameterShortFloat('Reflect','RefTheta',MDDef.RefTheta,45);
         AParameterShortFloat('Reflect','RefVertExag',MDDef.RefVertExag,1);
         AColorParameter('Reflect','WaterColor',MDDef.WaterColor,claBlue);
      end;
   end;


   procedure MicronetSettings;
   begin
      {$IfDef ExMICRONET}
      {$Else}
         with MDIniFile,MDDef,NetDef do begin
            {$IfDef AllowGeomorphometry}
               AParameter('Micronet','SSObyPole',SSObyPole,true);
            {$EndIf}
            AParameter('Micronet','NetDipFill',NetDipFill,2);
            AParameter('Micronet','AllowRightHandRule',AllowRightHandRule,true);
            AParameter('Micronet','NorthTick',NorthTick,true);
            AParameter('Micronet','CenterTick',CenterTick,true);
            AParameter('Micronet','FormLegend',FormLegend,false);
            AParameter('Micronet','ContinuousGrayScale',ContinuousGrayScale,false);
            AParameter('Micronet','AllBeachBallSize',AllBeachBallSize,25);
            AParameter('Micronet','M3BeachBallSize',M3BeachBallSize,20);
            AParameter('Micronet','M9BeachBallSize',M9BeachBallSize,50);
            AParameter('Micronet','MaxNumBeachBalls',MaxNumBeachBalls,50);
            AParameterFloat('Micronet','MaxScaleMagnitude',MaxScaleMagnitude,9);
            AParameterFloat('Micronet','MinScaleMagnitude',MinScaleMagnitude,3);
            AParameterFloat('Micronet','MaxColorMagnitude',MaxColorMagnitude,9);
            AParameterFloat('Micronet','MinColorMagnitude',MinColorMagnitude,3);
            AParameterFloat('Micronet','MaxColorDepth',MaxColorDepth,100);
            AParameterFloat('Micronet','MinColorDepth',MinColorDepth,0);
            AParameter('Micronet','NetScreenMult',NetScreenMult,2);
            AParameter('Micronet','NetColor',NetColor,1);
            AParameter('Micronet','ScreenSymbolSize',ScreenSymbolSize,3);
            AParameter('Micronet','CircleGridIncrement',CircleGridIncrement,15);
            AParameter('Micronet','NetLineWidth',NetLineWidth,1);
            AParameter('Micronet','GreatCircleLineWidth',GreatCircleLineWidth,2);
            AColorParameter('Micronet','NetLineColor',NetLineColor,claSilver);
            AColorParameter('Micronet','GreatCircleColor',GreatCircleColor,claRed);
            AParameter('Micronet','CreatNetHidden',CreateNetHidden,false);
            AParameterFloat('Micronet','MinContourConcentration',MinContourConcentration,0);
            AParameterFloat('Micronet','MaxContourConcentration',MaxContourConcentration,0);

            //AParameter('Micronet','DrawGridCircles',DrawGridCircles,ngNone);
            if (IniWhat = iniWrite) then IniFile.WriteInteger('Micronet','DrawGridCircles',ord(DrawGridCircles));
            if (IniWhat = iniRead) then DrawGridCircles :=  tNetGrid(IniFile.ReadInteger('Micronet','DrawGridCircles',ord(ngNone)));
            if (iniWhat = iniInit) then DrawGridCircles := ngNone;

            //AParameter('Micronet','NetUsed',NetUsed,Schmidt);
            if (IniWhat = iniWrite) then IniFile.WriteInteger('Micronet','NetUsed',ord(NetUsed));
            if (IniWhat = iniRead) then NetUsed := NetType(IniFile.ReadInteger('Micronet','NetUsed',ord(Schmidt)));
            if (iniWhat = iniInit) then NetUsed := Schmidt;

            //AParameter('Micronet','HemiSphereUsed',HemiSphereUsed,Lower);
            if (IniWhat = iniWrite) then IniFile.WriteInteger('Micronet','HemiSphereUsed',ord(HemiSphereUsed));
            if (IniWhat = iniRead) then HemiSphereUsed := tHemiSphere(IniFile.ReadInteger('Micronet','HemiSphereUsed',ord(Lower)));
            if (iniWhat = iniInit) then HemiSphereUsed := Lower;

            //AParameter('Micronet','InputDisplay',InputDisplay,Pole);
            if (IniWhat = iniWrite) then IniFile.WriteInteger('Micronet','InputDisplay',ord(InputDisplay));
            if (IniWhat = iniRead) then InputDisplay := tInputDisplay(IniFile.ReadInteger('Micronet','InputDisplay',ord(Pole)));
            if (iniWhat = iniInit) then InputDisplay := Pole;

            //AParameter('Micronet','NetContourColors',NetContourColors,ColorBands);
            if (IniWhat = iniWrite) then IniFile.WriteInteger('Micronet','NetContourColors',ord(NetContourColors));
            if (IniWhat = iniRead) then NetContourColors := tNetContourColors(IniFile.ReadInteger('Micronet','NetContourColors',ord(Spectrum)));
            if (iniWhat = iniInit) then NetContourColors := Spectrum;

            //AParameter('Micronet','BeachBallSize',BeachBallSize,bbsAll);
            if (IniWhat = iniWrite) then IniFile.WriteInteger('Micronet','BeachBallSize',ord(BeachBallSize));
            if (IniWhat = iniRead) then BeachBallSize := tBeachBallSize(IniFile.ReadInteger('Micronet','BeachBallSize',ord(bbsAll)));
            if (iniWhat = iniInit) then BeachBallSize := bbsAll;
         end;
      {$EndIf}
   end;


   procedure ContourSettings;
   begin
      with MDIniFile,MDDef do begin
         AColorParameter('Contour','ContourColor',ContourColor,claSilver);
         AColorParameter('Contour','IndexColor',IndexColor,claMaroon);
         AColorParameter('Contour','ZeroColor',ZeroColor,claBlue);
         AColorParameter('Contour','TopContColor',TopContColor,claLime);
         AColorParameter('Contour','BotContColor',BotContColor,claYellow);
         AColorParameter('Contour','OverlayContColor',OverlayContColor,claMaroon);
         AParameter('Contour','DelaunayLineThick',DelaunayLineThick,1);
         AParameter('Contour','ContourLineThick',ContourLineThick,1);
         AColorParameter('Contour','DelaunayLineColor',DelaunayLineColor,claMaroon);
         AColorParameter('Contour','ContourLineColor',ContourLineColor,claLime);
         AParameter('Contour','ShowDelauneyTriangles',ShowDelauneyTriangles,false);
         //AParameter('Contour','ExportContourShapeFile',ExportContourShapeFile,false);
         AParameter('Contour','LabelContours',LabelContours,false);
         AParameter('Contour','ContourLineWidth',ContourLineWidth,1);
         AParameter('Contour','DefaultContourInterval',DefaultContourInterval,50);
         AParameter('Contour','DefaultContourInterval',DefaultContourInterval,10) ;
         AParameter('Contour','IndexContWidth ',IndexContWidth,2) ;
         AColorParameter('Contour','OverlayContColor',OverlayContColor,claDarkGrey) ;
         AParameterShortFloat('Contour','MaxTriSide',MaxTriSide,0.00005) ;

         if (IniWhat = iniWrite) then IniFile.WriteInteger('Contour','ContourColors',ord(ccSpecified));
         if (IniWhat = iniRead) then ContourColors := tContourColors(IniFile.ReadInteger('Contour','ContourColors',ord(ccSpecified)));
         if (iniWhat = iniInit) then ContourColors := ccSpecified;
      end;
   end;


   procedure DatumProjectionSettings;
   begin
      with MDIniFile,MDDef do begin
         AParameterShortFloat('DatumProjection','MercShiftLongLimit',MercShiftLongLimit,6);
         AParameter('DatumProjection','DefaultUTMZone',DefaultUTMZone,11);
         AParameter('DatumProjection','RememberUTM',RememberUTM,true);
         AParameter('DatumProjection','PreferPrimaryDatum',PreferPrimaryDatum,'WGS84');
         AParameter('DatumProjection','DefaultDigitizeDatum',DefaultDigitizeDatum,'WGS84');
         AParameter('DatumProjection','SecondaryDatum',PreferSecondaryDatum,'WGS84');
         AParameter('DatumProjection','DefaultState',DefaultState,'');
         AParameter('DatumProjection','ShiftMercToUTM',ShiftMercToUTM,false);
         AParameter('DatumProjection','MercShiftSingleZone',MercShiftSingleZone,true);
         AParameter('DatumProjection','FullUTMGridLabels',FullUTMGridLabels,false);
         AParameter('DatumProjection','MaxEllipsoidalSpacing',MaxEllipsoidalSpacing,35);
         //AParameter('DatumProjection','BothDatumsWhileRoam',BothDatumsWhileRoam,false);
         AParameter('DatumProjection','MGRSandLatLongWhileRoam',MGRSandLatLongWhileRoam,false);
         AParameter('DatumProjection','ShowProjectedCoordinates',ShowProjectedCoordinates,false);
         ACharacter('DatumProjection','DefaultLatHemi',DefaultLatHemi,'N');
         ACharacter('DatumProjection','DefaultLongHemi',DefaultLongHemi,'W');

         if (IniWhat = iniWrite) then IniFile.WriteInteger('DatumProjection','DefaultVectorMapProjection',ord(MDDef.DefaultVectorMapProjection));
         if (IniWhat = iniRead) then MDDef.DefaultVectorMapProjection := tDefaultVectorMapProject(IniFile.ReadInteger('DatumProjection','DefaultVectorMapProjection',ord(MDDef.DefaultVectorMapProjection)));
         if (iniWhat = iniInit) then MDDef.DefaultVectorMapProjection := dvmMercator;
      end;
   end;



   procedure PLSSSettings;
   begin
     {$IfDef ExPLSS}
     {$Else}
         with MDIniFile,MDDef,PLSSDef do begin
            AParameter('Menus','ShowPLSS',ShowPLSS,false);
            AParameter('PLSS','PLSStoRAM',PLSStoRAM,false);
            AParameter('PLSS','PLSSShowQuarters',PLSSShowQuarters,false);
            AParameter('PLSS','PLSSShowSections',PLSSShowSections,true);
            AParameter('PLSS','PLSSShowTowns',PLSSShowTowns,true);
            AParameter('PLSS','PLSSLabelSections',PLSSLabelSections,true);
            AParameter('PLSS','PLSSLabelTowns',PLSSLabelTowns,true);
            AParameter('PLSS','PLSSsmartScaling',PLSSsmartScaling,true);
            AParameter('PLSS','PLSSQuartersInLabels',PLSSQuartersInLabels,true);
            AParameter('PLSS','PLSSLotsInLabels',PLSSLotsInLabels,true);
            AParameter('PLSS','PLSSTownWidth',PLSSTownWidth,3);
            AParameter('PLSS','PLSSQuarterWidth',PLSSQuarterWidth,1);
            AParameter('PLSS','PLSSAppearTowns',PLSSAppearTowns,250);
            AParameter('PLSS','PLSSAppearSections',PLSSAppearSections,50);
            AParameter('PLSS','PLSSAppearQuarters',PLSSAppearQuarters,10);
            AParameter('PLSS','PLSSRangeDef',PLSSRangeDef,0);
            AParameter('PLSS','PLSSTownShipDef',PLSSTownShipDef,0);

            {$IfDef Nevadia}
               AParameter('PLSS','PLSSSectionWidth',PLSSSectionWidth,1);
               AParameter('PLSS','AutoDrawPLSS',AutoDrawPLSS,true);
               AColorParameter('PLSS','PLSSTownColor',MDDef.PLSSDef.PLSSTownColor, RGBtrip(0,0,0));
               AColorParameter('PLSS','PLSSSectionColor',PLSSSectionColor,RGBtrip(0,128,128));
               AColorParameter('PLSS','PLSSQuarterColor',PLSSQuarterColor,RGBtrip(0,0,0));
               InitializeMyFont('SectFont',MDDef.PLSSDef.SectFont,'Courier New',8,RGBtrip(0,128,128));
               InitializeMyFont('TownFont',MDDef.PLSSDef.TownFont,'Courier New',8,cRGBtrip(0,0,0));
            {$Else}
               AParameter('PLSS','PLSSSectionWidth',PLSSSectionWidth,2);
               AParameter('PLSS','AutoDrawPLSS',AutoDrawPLSS,false);
               AColorParameter('PLSS','PLSSTownColor',MDDef.PLSSDef.PLSSTownColor,claRed);
               AColorParameter('PLSS','PLSSSectionColor',MDDef.PLSSDef.PLSSSectionColor,claLime);
               AColorParameter('PLSS','PLSSQuarterColor',PLSSQuarterColor,claBlue);
               InitializeMyFont('SectFont',MDDef.PLSSDef.SectFont,'Courier New',8,claLime);
               InitializeMyFont('TownFont',MDDef.PLSSDef.TownFont,'Courier New',8,claRed);
            {$EndIf}

            if (IniWhat = iniWrite) then IniFile.WriteInteger('PLSS','PLSSFormat',ord(PLSSFormat));
            if (IniWhat = iniRead) then PLSSFormat := tPLSSFormat(IniFile.ReadInteger('PLSS','PLSSFormat',ord(plssTRS)));
            if (iniWhat = iniInit) then PLSSFormat := plssTRS;
         end;

        {$IfDef RecordFont} WriteLineToDebugFile('PLSSSettings, town font: ' + MyFontToString(MDDef.PLSSDef.TownFont)); {$EndIf}
        {$IfDef RecordFont} WriteLineToDebugFile('PLSSSettings, sect font: ' + MyFontToString(MDDef.PLSSDef.SectFont)); {$EndIf}

      {$EndIf}
   end;

   procedure WindowsMenuSettings;
  {options to simplify menus and toolbars}
   begin
     {$IfDef VCL}
      with MDIniFile,MDDef do begin
         {$IfDef Nevadia}
            AParameter('Menus','ShowCartography',MDDef.ShowCartography,false);
            AParameter('Menus','ShowPointClouds',ShowPointClouds,false);
            AParameter('Menus','ShowGeologyOptions',ShowGeologyOptions,false);
            AParameter('Menus','ShowGeomorphometry',ShowGeomorphometry,false);
            AParameter('Menus','ShowClimateAndLight',ShowClimateAndLight,false);
            AParameter('Menus','ShowClimateStationDB',ShowClimateStationDB,false);
            AParameter('Menus','ShowIntervisibility',ShowIntervisibility,false);
            AParameter('Menus','ShowMethodCompare',ShowMethodCompare,false);
         {$Else}
            AParameter('Menus','ShowCartography',MDDef.ShowCartography,true);
            AParameter('Menus','ShowPointClouds',ShowPointClouds,true);
            AParameter('Menus','ShowGeologyOptions',ShowGeologyOptions,true);
            AParameter('Menus','ShowGeomorphometry',ShowGeomorphometry,true);
            AParameter('Menus','ShowClimateAndLight',ShowClimateAndLight,true);
            AParameter('Menus','ShowIntervisibility',ShowIntervisibility,true);
            AParameter('Menus','ShowMethodCompare',ShowMethodCompare,true);
         {$EndIf}
         AParameter('Menus','ShowDEMCompare',ShowDEMCompare,true);
         AParameter('Menus','ShowConversionAndAnalyze',ShowConversionAndAnalyze,true);
         AParameter('Menus','ShowViews',ShowViews,true);
         AParameter('Menus','ShowIcesat',ShowIcesat,false);
         AParameter('Menus','ShowDataBase',ShowDataBase,true);
         AParameter('Menus','ShowIntDB',ShowIntDB,true);
         AParameter('Menus','ShowTINs',ShowTINs,false);
         AParameter('Menus','ShowStereoNet',ShowStereoNet,false);
         AParameter('Menus','ShowStratCol',ShowStratCol,false);
         AParameter('Menus','ShowTernary',ShowTernary,false);
         AParameter('Menus','ShowMarineGeology',ShowMarineGeology,true);
         AParameter('Menus','AlwaysShowMapCoordinates',AlwaysShowMapCoordinates,false);
         AParameter('Menus','ShowOceanModels',ShowOceanModels,true);
         AParameter('Menus','ShowOceanographyOptions',ShowOceanographyOptions,false);
         AParameter('Menus','ShowSidescan',ShowSidescan,false);
         AParameter('Menus','ShowSubbottom',ShowSubbottom,false);
         AParameter('Menus','ShowExperimentalOptions',ShowExperimentalOptions,false);
         AParameter('Menus','ShowDataProperties',ShowDataProperties,true);
         AParameter('Menus','ShowDBonly',ShowDBonly,true);
         AParameter('Menus','ShowOpenImagery',ShowOpenImagery,true);
         AParameter('Menus','ShowTCPServer',ShowTCPServer,true);
         AParameter('Menus','ShowSHPButton',ShowSHPButton,true);
         AParameter('Menus','ShowUSGSQuadNames',ShowUSGSQuadNames,true);
         AParameter('Menus','US_Show_States',US_Show_States,true);
         AParameter('Menus','US_Show_Counties',US_Show_Counties,true);
         AParameter('Menus','US_Show_Roads',US_Show_Roads,true);
         AParameter('Menus','US_Show_Rivers',US_Show_Rivers,true);
         AParameter('Menus','ShowDBDateTimeSeries',ShowDBDateTimeSeries,true);
         AParameter('Menus','ShowDataManipulation',ShowDataManipulation,true);
         AParameter('Menus','ShowGlobalDEM',ShowGlobalDEM,true);
         AParameter('Menus','ShowBlueMarble',ShowBlueMarble,true);
         AParameter('Menus','ShowOpenGL',ShowOpenGL,true);
         AParameter('Menus','ShowMultigrids',ShowMultigrids,true);
         //AParameter('Menus','ShowGISlabs',ShowGISLabs,false);
         AParameter('Menus','ShowLabs',ShowLabs,false);
      end;
      {$EndIf}
   end;

   procedure DisplaySettings;
   begin
      with MDIniFile,MDDef do begin
      AParameterShortFloat('Display','ExtendZoneSize',ExtendZoneSize,0);
      AParameterShortFloat('Display','DefaultQuadSize',DefaultQuadSize,2.5);
      AParameter('Display','DecDegDecimals',DecDegDecimals,5);
      AParameter('Display','UseLongsTo360',UseLongsTo360,false);
      AParameter('Display','DualElevs',DualElevs,false);
      AParameter('Display','ASCIIZUnits',ASCIIZUnits,0);
      AParameter('Display','ASCIIXYFormat',ASCIIXYFormat,0);
      AParameter('Display','MaxLabelDecimals',MaxLabelDecimals,2);
      AParameter('Display','aRotateAngle',aRotateAngle,0);
      AParameter('Display','ClipZColors',ClipZColors,false);
      AParameter('Display','GraphicalCoordVerify',GraphicalCoordVerify,true);
      //AParameter('Display','ShowAllUnits',ShowAllUnits,true);
     // AParameter('Display','AutoLabelTheGridBlowups',AutoLabelTheGridBlowups,false);
      AParameter('Display','AvoidTextOverprints',AvoidTextOverprints,true);
      AParameter('Display','ElevDisplayMeters',ElevDisplayMeters,true);
      AParameter('Display','PlotArrowHead',PlotArrowHead,true);
      AParameter('Display','ReverseArrow',ReverseArrow,false);
      AParameter('Display','ShowRoamOnAllMaps',ShowRoamOnAllMaps,true);
      ASymbol('Display','aSym',aSym,Cross,claBlack,4);
      AParameter('Display','RoamAllZ',RoamAllZ,true);
      AColorParameter('Display','RoamSecondMapColor',RoamSecondMapColor,claBlack);
      AColorParameter('Display','MissingDataColor',MissingDataColor,claWhite);
      AColorParameter('Display','LowOffscaleColor',LowOffscaleColor,claBlack);
      AColorParameter('Display','HighOffscaleColor',HighOffscaleColor,claBlack);
      AColorParameter('Display','BlankMapColor',BlankMapColor,claSilver);

      if (IniWhat = iniWrite) then IniFile.WriteInteger('Display','OutPutLatLongMethod',ord(OutPutLatLongMethod));
      if (IniWhat = iniRead) then OutPutLatLongMethod := tLatLongMethod(IniFile.ReadInteger('Display','OutPutLatLongMethod',ord(DecDegrees)));
      if (iniWhat = iniInit) then OutPutLatLongMethod := DecDegrees;

      if (IniWhat = iniWrite) then IniFile.WriteInteger('Display','OutputAzimuthMethod',ord(OutputAzimuthMethod));
      if (IniWhat = iniRead) then OutputAzimuthMethod := tLatLongMethod(IniFile.ReadInteger('Display','OutputAzimuthMethod',ord(DecDegrees)));
      if (iniWhat = iniInit) then OutputAzimuthMethod := DecDegrees;

      if (IniWhat = iniWrite) then IniFile.WriteInteger('Display','OutputPitchMethod',ord(OutputPitchMethod));
      if (IniWhat = iniRead) then OutputPitchMethod := tLatLongMethod(IniFile.ReadInteger('Display','OutputPitchMethod',ord(DecDegrees)));
      if (iniWhat = iniInit) then OutputPitchMethod := DecDegrees;

      if (IniWhat = iniWrite) then IniFile.WriteInteger('Display','EnglishDistanceUnits',ord(EnglishDistanceUnits));
      if (IniWhat = iniRead) then EnglishDistanceUnits := tDistanceUnits(IniFile.ReadInteger('Display','EnglishDistanceUnits',ord(disMetric)));
      if (iniWhat = iniInit) then EnglishDistanceUnits := disMetric;

      if (IniWhat = iniWrite) then IniFile.WriteInteger('Display','DefaultElevationColors',ord(DefaultElevationColors));
      if (IniWhat = iniRead) then DefaultElevationColors := tMapType(IniFile.ReadInteger('Display','DefaultElevationColors',ord(mtElevRainbow)));
      if (iniWhat = iniInit) then DefaultElevationColors := mtElevRainbow;

      if (IniWhat = iniWrite) then IniFile.WriteInteger('Display','ZoomWindowMapType',ord(ZoomWindowMapType));
      if (IniWhat = iniRead) then DefaultElevationColors := tMapType(IniFile.ReadInteger('Display','ZoomWindowMapType',ord(ZoomWindowMapType)));
      if (iniWhat = iniInit) then ZoomWindowMapType := mtElevRainbow;

      if (IniWhat = iniWrite) then IniFile.WriteInteger('Display','RangeCircleUnit',ord(RangeCircleUnit));
      if (IniWhat = iniRead) then RangeCircleUnit := tRangeCircleUnit(IniFile.ReadInteger('Display','RangeCircleUnit',ord(rcMeter)));
      if (iniWhat = iniInit) then RangeCircleUnit := rcMeter;

      if (IniWhat = iniWrite) then IniFile.WriteInteger('Display','SpeedUnit',ord(SpeedUnit));
      if (IniWhat = iniRead) then SpeedUnit := tSpeedUnit(IniFile.ReadInteger('Display','SpeedUnit',ord(spKPH)));
      if (iniWhat = iniInit) then SpeedUnit := spKPH;

      if (IniWhat = iniWrite) then IniFile.WriteInteger('Display','MovieFormat',ord(MovieFormat));
      if (IniWhat = iniRead) then MovieFormat := tMovieFormat(IniFile.ReadInteger('Display','MovieFormat',ord(mfBMP)));
      if (iniWhat = iniInit) then MovieFormat := mfBMP;

      if (IniWhat = iniWrite) then IniFile.WriteInteger('Display','CheckPoint',ord(CheckPoint));
      if (IniWhat = iniRead) then CheckPoint := tCheckPoint(IniFile.ReadInteger('Display','CheckPoint',ord(CheckNothing)));
      if (iniWhat = iniInit) then CheckPoint := CheckNothing;
      end;
   end;

   procedure GeologySettings;
   begin
      with MDIniFile,MDDef do begin
         AParameter('Geology','ThreePointExtrap',ThreePointExtrap,500);
         AColorParameter('Geology','GeoContactColor',GeoContactColor,claRed);
         AParameter('Geology','GeoContactWidth',GeoContactWidth,3);
         AParameter('Geology','TraceContactsOnDEM',TraceContactsOnDEM,true);

         {$IfDef ExGeology}
         {$Else}
         AParameter('Geology','DefaultGeologySymbol',DefaultGeologySymbol,0);
         AParameter('Geology','StructGeologyShowNet',StructGeologyShowNet,false);
         AParameter('Geology','StructGeologyLabelVals',StructGeologyLabelVals,true);
         AParameter('Geology','PlateRotateContCrust',PlateRotateContCrust,true);
         AParameter('Geology','PlateRotateBoundaries',PlateRotateBoundaries,true);
         AParameter('Geology','RotateVectMult',RotateVectMult,10);
         AParameter('Geology','TernaryPercentageValues',TernaryPercentageValues,false);
         AParameter('Geology','BeachBallSwitchPixelSize',BeachBallSwitchPixelSize,50);
         AParameter('Geology','PlateVectors',PlateVectors,true);
         AParameter('Geology','ResultantPlateVectors',ResultantPlateVectors,true);
         AParameter('Geology','ShowContactsOnStereoNet',ShowContactsOnStereoNet,true);
         AParameter('Geology','ShowPlateRotation',ShowPlateRotation,true);
         AParameter('Geology','ShowSieve',ShowSieve,false);
         AParameter('Geology','AutoIncGeoColor',AutoIncGeoColor,true);
         AColorParameter('Geology','MapGeoSymColor', MapGeoSymColor,claBlack);
         AParameter('Geology','PlateTectonicVelocity', PlateTectonicVelocity,true);
         AParameter('Geology','PlateModel', PlateModel,'HS3-NUVEL-1A');
         AParameter('Geology','PlateVelocityDiagram', PlateVelocityDiagram,true);
         AParameter('Geology','PlateLabelVectors', PlateLabelVectors,true);
         AParameter('Geology','MoveGeologyDBMemory', MoveGeologyDBMemory,false);

         AParameter('Geology','PlateNumbers', PlateNumbers,false);
         AColorParameter('Geology','ColorFP1', ColorFP1,claRed);
         AColorParameter('Geology','ColorFP2', ColorFP2,claLime);

         AParameter('Geology','FPMinSlope1',FPMinSlope1,15);
         AParameter('Geology','FPMaxSlope1',FPMaxSlope1,45);
         AParameter('Geology','FPMinAsp1',FPMinAsp1,0);
         AParameter('Geology','FPMaxAsp1',FPMaxAsp1,180);
         AParameter('Geology','FPMinSlope2',FPMinSlope2,15);
         AParameter('Geology','FPMaxSlope2',FPMaxSlope2,45);
         AParameter('Geology','FPMinAsp2',FPMinAsp2,180);
         AParameter('Geology','FPMaxAsp2',FPMaxAsp2,360);
         AParameter('Geology','MagLineWidth',MagLineWidth,2);

         if (IniWhat = iniWrite) then IniFile.WriteInteger('Geology','BeachBallMap',ord(BeachBallMap));
         if (IniWhat = iniRead) then BeachBallMap := tBeachBallMap(IniFile.ReadInteger('Geology','BeachBallMap',ord(bbmSwitch)));
         if (iniWhat = iniInit) then BeachBallMap := bbmSwitch;
      {$EndIf}
      end;
   end;

   procedure GISBDSettings;
   begin
      {$IfDef RecordINIfiles} WriteLineToDebugFile('GISDBSettings'); {$EndIf}
      with MDIniFile,MDDef do begin
         AParameter('GISDB','DefaultEditDBsInGrid',DefaultEditDBsInGrid,true);
         AParameter('GISDB','AddFitNav',AddFitNav,true);
         AParameter('GISDB','DBfilterCaseInSensitive',DBfilterCaseInSensitive,true);
         AParameter('GISDB','SaveIntermediateDBs',SaveIntermediateDBs,false);
         AParameter('GISDB','DupeImportsAllowed',DupeImportsAllowed,false);
         AParameter('GISDB','DefDBFilter',DefDBFilter,0);
         AColorParameter('GISDB','DBOutlineColor',DBOutlineColor,claRed);
         AParameter('GISDB','DBOutlineWidth',DBOutlineWidth,2);
         AParameter('GISDB','DBfilterClearLayer',DBfilterClearLayer,true);
         AParameter('GISDB','GISSecondToolbar',GISSecondToolbar,false);
         AParameter('GISDB','DBfilterClearLayer',DBfilterClearLayer,true);
         AParameter('GISDB','DBfilterRedrawOnClose',DBfilterRedrawOnClose,true);
         AColorParameter('GISDB','ScaledSymbolColor',ScaledSymbolColor,claRed);
         AColorParameter('GISDB','FillColor',FillColor,claRed);
         AColorParameter('GISDB','AreaBorderColor',AreaBorderColor,claRed);
         AParameter('GISDB','SaveDBStatus',SaveDBStatus,true);
         AParameter('GISDB','SaveDBFilter',SaveDBFilter,false);
         AParameter('GISDB','AutoAssignNameField',AutoAssignNameField,true);
         AParameter('GISDB','QuickShapeFileCoding',QuickShapeFileCoding,true);
         AParameter('GISDB','TrackDatabaseRanges',TrackDatabaseRanges,true);
         AParameter('GISDB','AllowMemoryLinkDB',AllowMemoryLinkDB,true);
         AParameter('GISDB','AllowDBsToRAM',AllowDBstoRAM,true);
         AParameter('GISDB','TigertoCDS',TigertoCDS,true);
         AParameter('GISDB','DBRecShowToolbarTop',DBRecShowToolbarTop,true);
         AParameter('GISDB','AutoDBFieldNameFixes',MDDef.AutoDBFieldNameFixes,true);

         AParameter('GISDB','AllowFirstOfMultipleJoins',AllowFirstOfMultipleJoins,false);
         AParameter('GISDB','ModalDBDisplay',ModalDBDisplay,false);
         AParameter('GISDB','AllowEditDBInGrid',AllowEditDBInGrid,true);
         AParameter('GISDB','DBsOnAllMapsv2',DBsOnAllMaps,true);
         AParameter('GISDB','DBLegendN',DBLegendN,false);
         AParameter('GISDB','CommasToSemiColons',CommasToSemiColons,true);
         AParameter('GISDB','DBMinimizeOnOpen',DBMinimizeOnOpen,false);
         AParameter('GISDB','ApplySameFilterAllDBs',ApplySameFilterAllDBs,false);
         AParameter('GISDB','GISLabelSkip',GISLabelSkip,1);
         AParameter('GISDB','DbMinIntFieldSize',DbMinIntFieldSize,5);
         AParameter('GISDB','RecNumToShowDBProgress',RecNumToShowDBProgress,25000);
         AParameter('GISDB','CSVfileHeaders',CSVfileHeaders,'');
         AParameter('GISDB','Create3DShapefiles',Create3DShapefiles,true);
         AParameter('GISDB','UsePixelSizeRules',UsePixelSizeRules,true);
         AParameter('GISDB','DB_ID_grids',DB_ID_grids,true);
         AParameter('GISDB','DBRecHt',DBRecHt,400);
         AParameter('GISDB','DBRecWd',DBRecWd,400);
         AParameter('GISDB','AdvancedDBops',AdvancedDBops,true);
         AParameter('GISDB','ShowNewFieldStats',ShowNewFieldStats,true);
         AParameter('GISDB','HalfBoxSize',HalfBoxSize,250);
         AParameter('GISDB','NAvgReq',NAvgReq,5);
         AParameter('GISDB','MapLimitDB',MapLimitDB,false);
         AParameter('GISDB','DBPixelsToShow',DBPixelsToShow,2);
         AParameter('GISDB','FileHeader',FileHeader,'xyz');
         ASymbol('GISDB','DefGISSymbol.DrawingSymbol',DefGISSymbol,FilledBox,claRed,3);
         AParameter('GISDB','JoinRemoveLeadingZeros',JoinRemoveLeadingZeros,true);

         AParameter('GISDB','ImpossibleCost',ImpossibleCost,9999);
         AParameter('GISDB','BufferCost',BufferCost,100);
         AParameter('GISDB','BufferRounds',BufferRounds,2);
         AParameter('GISDB','CostSurfResolution',CostSurfResolution,30);
         AParameter('GISDB','StartFree',StartFree,3);
         AParameter('GISDB','MaxImpossibleInPath',MaxImpossibleInPath,5);
         AParameter('GISDB','PrecintField',PrecintField,'PRECINCT');
         AParameter('GISDB','LCPDist',LCPDist,true);
         AParameter('GISDB','LCPDiagonals', LCPDiagonals,true);
         AParameter('GISDB','LCPsavecost', LCPsavecost,true);
         AParameter('GISDB','LCPsavedir', LCPsavedir,true);
         AParameter('GISDB','LCPsavedist', LCPsavedist,true);
         AParameter('GISDB','LCPoverwrite', LCPoverwrite,true);
         AParameter('GISDB','LCP_ShortestDistance', LCP_ShortestDistance,false);
         AParameter('GISDB','LCP_LeastCost', LCP_LeastCost,true);
         AParameter('GISDB','ConfirmDBEdits', ConfirmDBEdits,true);

         AParameter('GISDB','LCPStartfName', LCPStartfName,'');
         AParameter('GISDB','LCPendfName', LCPendfName,'');
         AParameter('GISDB','LCProadfName', LCProadfName,'');
         {$IfDef ExOSM}
         {$Else}
            AParameter('GISDB','OSMtoCDS',OSMtoCDS,true);
         {$EndIf}

         //AColorParameter('GISDB','CanEditGIS',CanEditGIS,egisSometimes);
         if (IniWhat = iniWrite) then IniFile.WriteInteger('GISDB','CanEditGIS',ord(CanEditGIS));
         if (IniWhat = iniRead) then CanEditGIS := tCanEditGIS(IniFile.ReadInteger('GISDB','CanEditGIS',ord(egisSometimes)));
         if (iniWhat = iniInit) then CanEditGIS := egisSometimes;

         //AParameter('GISDB','DBColorScheme',DBColorScheme,LegRainbows);
         if (IniWhat = iniWrite) then IniFile.WriteInteger('GISDB','DBColorScheme',ord(DBColorScheme));
         if (IniWhat = iniRead) then DBColorScheme := tLegendColors(IniFile.ReadInteger('GISDB','DBColorScheme',ord(LegRainbows)));
         if (iniWhat = iniInit) then DBColorScheme := LegRainbows;

         {$IfDef VCL}
         if (IniWhat = iniWrite) then IniFile.WriteInteger('GISDB','AreaSymbolFill',ord(DefAreaFill));
         if (IniWhat = iniRead) then DefAreaFill := tBrushStyle(IniFile.ReadInteger('GISDB','DefAreaFill',ord(bsClear)));
         if (iniWhat = iniInit) then DefAreaFill := bsClear;
         {$EndIf}
      end;
   end;

   procedure MapDrawSettings;
   begin
      {$IfDef RecordINIfiles} WriteLineToDebugFile('MapDraweSettings'); {$EndIf}
      with MDIniFile,MDDef do begin
         {$IfDef VCL}
            AParameter('MapDraw','MapSizeToVerify',MapSizeToVerify,SystemMemorySize div 1024 div 1024 div 32);
            AParameter('MapDraw','DefaultMapXSize',DefaultMapXSize,Screen.Width-25);
            AParameter('MapDraw','DefaultMapYSize',DefaultMapYSize,Screen.Height-25);
         {$EndIf}

         {$IfDef FMX}
            AParameter('MapDraw','MapSizeToVerify',MapSizeToVerify,200);
            AParameter('MapDraw','DefaultMapXSize',DefaultMapXSize,1200);
            AParameter('MapDraw','DefaultMapYSize',DefaultMapYSize,800);
         {$EndIf}

         AParameter('MapDraw','MaxMapSize',MaxMapSize,24000);
         AParameter('MapDraw','MaxDrapeXSize',MaxDrapeXSize,2000);
         AParameter('MapDraw','MaxDrapeYSize',MaxDrapeYSize,2000);
         AParameter('MapDraw','DefaultTerrainXSize',DefaultTerrainXSize,600);
         AParameter('MapDraw','DefaultTerrainYSize',DefaultTerrainYSize,600);
         AParameter('MapDraw','SinglePixel',SinglePixel,true);
         AParameter('MapDraw','OverlayOpacity',OverlayOpacity,50);

         {$IfDef ExOSM}
         {$Else}
            AParameter('MapDraw','OSMOpacity',OSMOpacity,50);
            AParameter('MapDraw','OSMcheck',OSMcheck,true);
            AParameter('MapDraw','OSMmaxLength',OSMmaxLength,200);
         {$EndIf}

         AParameter('MapDraw','GridLabelsInsideMap',GridLabelsInsideMap,true);
         AParameter('MapDraw','WorldOutlinesOnGlobalDEM',WorldOutlinesOnGlobalDEM,true);
         AParameter('MapDraw','WorldOutlinesOnGlobalBlueMarble',WorldOutlinesOnGlobalBlueMarble,true);
         AParameter('MapDraw','BlowUpExtraMargin',BlowUpExtraMargin,250);
         AParameter('MapDraw','HighlightLineWdith',HighlightLineWdith,3);
         AParameter('MapDraw','ConPtsWidth',ConPtsWidth,2);
         AParameter('MapDraw','UseMapPanButtons',UseMapPanButtons,true);
         AParameter('MapDraw','DefaultUTMGridSpacing',DefaultUTMGridSpacing,15);
         AParameter('MapDraw','UTMGridLineWidth',UTMGridLineWidth,1);
         AParameter('MapDraw','PanOverlap',PanOverlap,2);
         //AParameter('MapDraw','UseBigElevationColorTables',UseBigElevationColorTables,false);
         AParameter('MapDraw','CartMovieSteps', CartMovieSteps,2);
         AParameter('MapDraw','InvertGrayScale',InvertGrayScale,false);
         AParameter('MapDraw','MonochromeColor',MonochromeColor,0);
         AParameter('MapDraw','LeftLatGlobalDEM',LeftLatGlobalDEM,0);
         AParameter('MapDraw','LargeScaleWorldOutlinePixelSize',LargeScaleWorldOutlinePixelSize,250);
         AParameter('MapDraw','MedScaleWorldOutlinePixelSize',MedScaleWorldOutlinePixelSize,1500);
         AParameter('MapDraw','SmallScaleWorldOutlinePixelSize',SmallScaleWorldOutlinePixelSize,10000);
         AParameter('MapDraw','AspectMapMode',AspectMapMode,0);

         AParameter('MapDraw','MaxSlopeOnMaps',MaxSlopeOnMaps,100);
         AParameter('MapDraw','NEautoScale',NEautoScale,true);
         AParameter('MapDraw','NEAutoSat',NEAutoSat,false);
         AParameter('MapDraw','NEAutoDEM',NEAutoDEM,false);

         AParameter('MapDraw','t_epsg',t_epsg,32631);
         AParameter('MapDraw','a_epsg',a_epsg,28992);
         AParameter('MapDraw','MapOverlayOpacity',MapOverlayOpacity,50);

         ASymbol('MapDraw','KeyLocationSymbol',KeyLocationSymbol,FilledBox,claRed,3);
         ASymbol('MapDraw','HighlightSymbol',HighlightSymbol,FilledBox,claBlack,3);
         AColorParameter('MapDraw','HighLightColor',HighLightColor,claBlack);
         AColorParameter('MapDraw','ConPtsColor',ConPtsColor,claBlue);
         AParameterShortFloat('MapDraw','BlowUpLongSize',BlowUpLongSize,0.075);
         AParameterShortFloat('MapDraw','BlowUpLatSize',BlowUpLatSize,0.05);
         AParameterShortFloat('MapDraw','LatLongCushion',LatLongCushion,0.025);

         AParameter('MapDraw','HighlightDiffMap',HighlightDiffMap,true);

         //AParameter('MapDrawing','StereoMode',StereoMode,smAnaglyph);
         if (IniWhat = iniWrite) then IniFile.WriteInteger('MapDraw','StereoMode',ord(StereoMode));
         if (IniWhat = iniRead) then StereoMode := tStereoMode(IniFile.ReadInteger('MapDraw','StereoMode',ord(smAnaglyph)));
         if (iniWhat = iniInit) then StereoMode := smAnaglyph;

         //AParameter('MapDrawing','ShadeOpts',ShadeOpts,soNone);
         if (IniWhat = iniWrite) then IniFile.WriteInteger('MapDraw','ShadeOpts',ord(ShadeOpts));
         if (IniWhat = iniRead) then ShadeOpts := tShadeOpts(IniFile.ReadInteger('MapDraw','ShadeOpts',ord(soNone)));
         if (iniWhat = iniInit) then ShadeOpts := soNone;

         //AParameter('MapDrawing','DefaultMap',DefaultMap,mtDEMReflectance);
         if (IniWhat = iniWrite) then IniFile.WriteInteger('MapDraw','DefaultMap',ord(DefDEMMap));
         if (IniWhat = iniRead) then DefDEMMap := tMapType(IniFile.ReadInteger('MapDraw','DefaultMap',ord(mtIHSReflect)));
         if (iniWhat = iniInit) then DefDEMMap := mtIHSReflect;

         if (IniWhat = iniWrite) then IniFile.WriteInteger('MapDraw','DefaultRefMap',ord(DefRefMap));
         if (IniWhat = iniRead) then DefRefMap := tMapType(IniFile.ReadInteger('MapDraw','DefRefMap',ord(mtIHSReflect)));
         if (iniWhat = iniInit) then DefRefMap := mtIHSReflect;

         if (IniWhat = iniWrite) then IniFile.WriteInteger('MapDraw','DefaultSlopeMap',ord(DefSlopeMap));
         if (IniWhat = iniRead) then DefSlopeMap := tMapType(IniFile.ReadInteger('MapDraw','DefaultSlopeMap',ord(mtSlopeTrafficCats)));
         if (iniWhat = iniInit) then DefSlopeMap := mtSlopeTrafficCats;

         if (IniWhat = iniWrite) then IniFile.WriteInteger('MapDraw','VegOptionMap',ord(VegOptionMap));
         if (IniWhat = iniRead) then VegOptionMap := tVegOptionMap(IniFile.ReadInteger('MapDraw','VegOptionMap',ord(voDTM)));
         if (iniWhat = iniInit) then VegOptionMap := voDTM;
      end;
   end;


   procedure SatelliteSettings;
   begin
      {$IfDef ExSat}
      {$Else}
         {$IfDef RecordINIfiles} WriteLineToDebugFile('SatelliteSettings'); {$EndIf}
         with MDIniFile,MDDef do begin
            AParameter('Sats','MaxSatRange',MaxSatRange,255);
            AParameter('Sats','MinSatRange',MinSatRange,0);
            AParameter('Sats','ClassifyIHSopacity',ClassifyIHSopacity,100);
            AParameter('Sats','IgnoreHistogramZero',IgnoreHistogramZero,true);
            AParameter('Sats','MaxPCBands',MaxPCBands,25);
            AParameter('Sats','LoadPCBands',LoadPCBands,true);
            AParameter('Sats','ImageryIconDirs',ImageryIconDirs,true);
            AParameter('Sats','ShowEnhancementGraphs',ShowEnhancementGraphs,true);
            AParameter('Sats','DefaultSatFilter',DefaultSatFilter,1);
            AParameter('Sats','SatImageCoords',SatImageCoords,false);
            AParameter('Sats','AverageImageReadings',AverageImageReadings,false);
            AParameter('Sats','QuickMapRedaw',QuickMapRedraw,true);
            AParameter('Sats','SatMultiBandTrueColor',SatMultiBandTrueColor,false);
            AParameter('Sats','SatMultiBandNormalize',SatMultiBandNormalize,false);
            AParameter('Sats','BandsByWavelength',BandsByWavelength,true);
            AParameter('Sats','BandsRequiredInBox',BandsRequiredInBox,5);
            AParameter('Sats','UnSupSamplesFullImage',UnSupSamplesFullImage,false);
            AParameter('Sats','UnSupClassFullImage',UnSupClassFullImage,false);
            AParameter('Sats','ShowSupClassDB',ShowSupClassDB,true);
            AParameter('Sats','MaxPointsAddInBox', MaxPointsAddInBox,250);
            AParameter('Sats','DeleteJP2', DeleteJP2,false);
            AParameter('Sats','dnConvert',dnConvert,0);

            {$IfDef ExACOLITE}
            {$Else}
               AParameter('Sats','l2w_Params',l2w_Params,'tur_nechad2016,spm_nechad2016,ndci,chl_re_mishra');
               AParameter('Sats','acolite_fName',acolite_fName,'');
               AParameter('Sats','acolite_delete_nc',acolite_delete_nc,false);
               AParameter('Sats','acolite_delete_misc',acolite_delete_misc,false);
               AParameter('Sats','acolite_delete_rhos',acolite_delete_rhos,false);
               AParameter('Sats','acolite_delete_rhot',acolite_delete_rhot,false);
               AParameter('Sats','acoliteS2res',acoliteS2res,0);
            {$EndIf}

            AParameterShortFloat('Sats','LowTailSize',LowTailSize,1);
            AParameterShortFloat('Sats','HighTailSize',HighTailSize,1);
            AParameterShortFloat('Sats','SatTrainStdDev',SatTrainStdDev,1.5);

            if (IniWhat = iniWrite) then IniFile.WriteInteger('Sats','ClassLimitMethod',ord(clmMeanStd));
            if (IniWhat = iniRead) then ClassLimitMethod := tClassLimitMethod(IniFile.ReadInteger('Sats','ClassLimitMethod',ord(clmMeanStd)));
            if (iniWhat = iniInit) then ClassLimitMethod := clmMeanStd;

            if (IniWhat = iniWrite) then IniFile.WriteInteger('Sats','ContrastEnhancement',ord(ContrastEnhancement));
            if (IniWhat = iniRead) then ContrastEnhancement := tContrastEnhancement(IniFile.ReadInteger('Sats','ContrastEnhancement',ord(TailLinearStretch)));
            if (iniWhat = iniInit) then ContrastEnhancement := TailLinearStretch;
         end;
      {$EndIf}
   end;

      {$IfDef ExDRGimport}
      {$Else}
      procedure DRGImportSettings;
      begin
         with MDIniFile,MDDef do begin
            AParameter('DRG','DRGCollar',DRGCollar,false);
            AParameter('DRG','DRGStructures',DRGStructures,true);
            AParameter('DRG','DRGTransport',DRGTransport,true);
            AParameter('DRG','DRGHydrography',DRGHydrography,true);
            AParameter('DRG','DRGShadedRelief',DRGShadedRelief,true);
            AParameter('DRG','DRGBoundaries',DRGBoundaries,false);
            AParameter('DRG','DRGOrthos',DRGOrthos,false);
            AParameter('DRG','DRGGrid',DRGGrid,true);
            AParameter('DRG','DRGContours',DRGContours,true);
            AParameter('DRG','DRGWoodland',DRGWoodland,false);
            AParameter('DRG','DRGPLSS',DRGPLSS,false);
            AParameter('DRG','DRGQuadClip',DRGQuadClip,true);
         end;
      end;
      {$EndIf}

   procedure MapGridSettings;
   begin
      {$IfDef RecordINIfiles} WriteLineToDebugFile('MapGridSetting'); {$EndIf}
      with MDIniFile,MDDef do begin
         AColorParameter('MapGrid','MapLatLongGridColor',MapLatLongGridColor,claBlack);
         AParameter('MapGrid','MapLatLongLineWidth',MapLatLongLineWidth,1);
         AParameter('MapGrid','UTMGridMaxPixelSize',UTMGridMaxPixelSize,1000);
         AParameter('MapGrid','ShowPrimaryGrid',ShowPrimaryGrid,true);
         AParameter('MapGrid','ShowSecondaryGrid',ShowSecondaryGrid,false);
         AParameter('MapGrid','GridLabelDecimals',GridLabelDecimals,0);
         AParameter('MapGrid','ShowNativeGrid',ShowNativeGrid,false);
         AParameter('MapGrid','LabelNativeGrid',LabelNativeGrid,false);

         AParameter('MapGrid','GeoSecGridLineWidth',GeoSecGridLineWidth,1);
         AColorParameter('MapGrid','SecondaryUTMColor',SecondaryUTMColor,claLime);
         AColorParameter('MapGrid','SecondaryGeoColor',SecondaryGeoColor,claRed);
         AColorParameter('MapGrid','MapGridColor',MapGridColor,claBlack);
         AParameter('MapGrid','UTMSecGridLineWidth',UTMSecGridLineWidth,1);

         AParameter('MapGrid','UTMGridLineWidth',UTMGridLineWidth,1);

         AColorParameter('MapGrid','SecondaryGeoColor',SecondaryGeoColor,claBlack);
         AParameter('MapGrid','GeoSecGridLineWidth',GeoSecGridLineWidth,1);

         AColorParameter('MapGrid','NativeGridColor',NativeGridColor,claBlack);
         AParameter('MapGrid','NativeGridLineWidth',NativeGridLineWidth,1);
         AParameterFloat('MapGrid','ScalebarDistortionTolerable',ScalebarDistortionTolerable,10);

         AParameter('MapGrid','LatLongGridTicks',LatLongGridTicks,false);
         AParameter('MapGrid','SecondGridOpacity',SecondGridOpacity,100);
         AParameter('MapGrid','HorizGratText',HorizGratText,true);
         AParameter('MapGrid','MDDef.MapTicks',MDDef.MapTicks,tixLatLong);

         //AParameter('MapGrids','CoordUse',CoordUse,coordLatLong);
         if (IniWhat = iniWrite) then IniFile.WriteInteger('MapGrid','CoordUse',ord(CoordUse));
         if (IniWhat = iniRead) then CoordUse := tCoordUse(IniFile.ReadInteger('MapGrid','CoordUse',ord(coordLatLong)));
         if (iniWhat = iniInit) then CoordUse := coordLatLong;

         //AParameter('MapGrids','GraticuleUnits',GraticuleUnits,amDegree);
         if (IniWhat = iniWrite) then IniFile.WriteInteger('MapGrid','GraticuleUnits',ord(GraticuleUnits));
         if (IniWhat = iniRead) then GraticuleUnits := tAngleMeasure(IniFile.ReadInteger('MapGrid','GraticuleUnits',ord(amDegree)));
         if (iniWhat = iniInit) then GraticuleUnits := amDegree;
      end;
   end;


   procedure MarginaliaLocation(tName : shortstring; var ItemLocation : tLegendLocation; DefSpot : byte; DefDraw : boolean);
   {tLegendLocation = record
      DrawItem,
      HorizontalLegend : boolean;
      LegendSize,
      MapPosition : byte;
   end;}
   begin
      with MDIniFile do begin
         AParameter('MapMargin',tName + '.DrawItem',ItemLocation.DrawItem,DefDraw);
         AParameter('MapMargin',tName + '.HorizontalLegend',ItemLocation.HorizontalLegend,true);
         AParameter('MapMargin',tName + '.LegendSize',ItemLocation.LegendSize,1);
         AParameter('MapMargin',tName + '.MapPosition',ItemLocation.MapPosition,DefSpot);

         (*
         if (IniWhat = iniWrite) then IniFile.WriteInteger('MapMarginalia',tName + '.MapPosition',ord(ItemLocation.MapPosition));
         if (IniWhat = iniRead) then ItemLocation.MapPosition := IniFile.ReadInteger('MapMarginalia',tName + '.MapPosition',DefSpot);
         if (iniWhat = iniInit) then ItemLocation.MapPosition := DefSpot;
         *)
      end;
   end;


   procedure SlopeGraySettings;
   begin
      {$IfDef RecordINIfiles} WriteLineToDebugFile('SlopeGraySettings'); {$EndIf}
      with MDIniFile,MDDef do begin
         AParameter('SlopeGray','SlopeMinGray',SlopeMinGray,25);
         AParameter('SlopeGray','SlopeMaxGray',SlopeMaxGray,250);
         AParameter('SlopeGray','MinSlopeForGray',MinSlopeForGray,0);
         AParameter('SlopeGray','MaxSlopeForGray',MaxSlopeForGray,100);
      end;
   end;

   procedure OpenGraySettings;
   begin
      {$IfDef RecordINIfiles} WriteLineToDebugFile('OpenGraySettings'); {$EndIf}
      with MDIniFile,MDDef do begin
         AParameter('OpenGray','OpenMinGray',OpenMinGray,120);
         AParameter('OpenGray','OpenMaxGray',OpenMaxGray,200);
         AParameter('OpenGray','MinUpward',MinUpward,0);
         AParameter('OpenGray','MaxUpward',MaxUpward,250);
      end;
   end;

   procedure RedChangeSettings;
   begin
      {$IfDef RecordINIfiles} WriteLineToDebugFile('RedChangeSettings'); {$EndIf}
      with MDIniFile,MDDef do begin
         AParameter('RedChangeSettings','ChangeMinRedColor',ChangeMinRedColor,120);
         AParameter('RedChangeSettings','ChangeMaxRedColor',ChangeMaxRedColor,200);
         AParameter('RedChangeSettings','ChangeMinRedValue',ChangeMinRedValue,2);
         AParameter('RedChangeSettings','ChangeMaxRedValue',ChangeMaxRedValue,20);
      end;
   end;

   procedure GreenChangeSettings;
   begin
      {$IfDef RecordINIfiles} WriteLineToDebugFile('GreenChangeSettings'); {$EndIf}
      with MDIniFile,MDDef do begin
         AParameter('GreenChangeSettings','ChangeMinGreenColor',ChangeMinGreenColor,120);
         AParameter('GreenChangeSettings','ChangeMaxGreenColor',ChangeMaxGreenColor,200);
         AParameter('GreenChangeSettings','ChangeMinGreenValue',ChangeMinGreenValue,2);
         AParameter('GreenChangeSettings','ChangeMaxGreenValue',ChangeMaxGreenValue,20);
      end;
   end;


var
   i : integer;
begin
   {$If Defined(RecordINIfiles) or Defined(RecordDetailedStartup)}
   case iniWhat of
      iniRead : WriteLineToDebugFile('ProcessIniFile in to read');
      iniWrite: WriteLineToDebugFile('ProcessIniFile in to write');
      iniInit : WriteLineToDebugFile('ProcessIniFile in to init');
   end;
   {$EndIf}

   {$IfDef DontWriteIniFile}
   if (iniWhat = iniWrite) then exit;
   {$EndIf}

   MDIniFile := tMDiniFile.OpenMDiniFile(iniWhat, SectionRestoreDefaults,ExplicitName);
   ApplicationProcessMessages;
   if (iniWhat = iniInit) and (SectionRestoreDefaults <> '') then begin
      {$IfDef RecordINIfiles} WriteLineToDebugFile('Just restore ' + SectionRestoreDefaults); {$EndIf}
      SectionRestoreDefaults := UpperCase(SectionRestoreDefaults);
      if (SectionRestoreDefaults = 'TIGER') then TigerDefaults;
      if (SectionRestoreDefaults = 'KOPPEN') then KoppenSettings;
      if (SectionRestoreDefaults = 'LOS') then LOSOptions;
      if (SectionRestoreDefaults = 'GAZETTEER') then GazSettings;
      if (SectionRestoreDefaults = 'WEAPFANALG') then WeaponsFanAlg;
      if (SectionRestoreDefaults = 'PLSS') then PLSSsettings;
      if (SectionRestoreDefaults = 'REFLECT') then ReflectanceSettings;
      if (SectionRestoreDefaults = 'FLYING') then FlyOptions;
      if (SectionRestoreDefaults = 'PERSP') then  PerspectiveOptions;
      if (SectionRestoreDefaults = 'WEAPONFAN') then WeaponsFanDefaults;
      if (SectionRestoreDefaults = 'STRATCOL') then StatcolParameters;
      if (SectionRestoreDefaults = 'MICRONET') then MicronetSettings;
      if (SectionRestoreDefaults = 'CONTOUR') then ContourSettings;
      if (SectionRestoreDefaults = 'SLOPEGRAY') then SlopeGraySettings;
      if (SectionRestoreDefaults = 'OPENGRAY') then OpenGraySettings;
      if (SectionRestoreDefaults = 'TISSOT') then TissotOptions;
      if (SectionRestoreDefaults = 'MENUS') then WindowsMenuSettings;
      if (SectionRestoreDefaults = 'REDCHANGESETTINGS') then RedChangeSettings;
      if (SectionRestoreDefaults = 'GREENCHANGESETTINGS') then GreenChangeSettings;
      MDiniFile.CloseMDiniFile;
      exit;
   end;

   {$If Defined(RecordINIfiles)} WriteLineToDebugFile('Breakpoint 1'); {$EndIf}
   FlyOptions;
   GeomorphOptions;
   LOSOptions;
   PerspectiveOptions;
   {$If Defined(RecordINIfiles)} WriteLineToDebugFile('Breakpoint 1.15'); {$EndIf}
   TigerDefaults;
   PointCloudDefaults;
   {$If Defined(RecordINIfiles)} WriteLineToDebugFile('Breakpoint 1.25'); {$EndIf}
   WeaponsFanDefaults;
   WeaponsFanAlg;
   SlopeGraySettings;
   OpenGraySettings;
   RedChangeSettings;
   GreenChangeSettings;

   TissotOptions;

   {$IfDef RecordINIfiles} WriteLineToDebugFile('Breakpoint 1.5'); {$EndIf}
   {$IfDef RecordIniMemoryOverwrite} IniMemOverwriteCheck('Breakpoint 1.5'); {$EndIf}

   SonarDefaults;
   DEMDefaultParameters;
   StatcolParameters;
   ProgramFileSettings;
   WindowsMenuSettings;
   MiscSettings;
   KoppenSettings;
   GazSettings;
   PLSSsettings;
   ReflectanceSettings;

   {$If Defined(RecordINIfiles) or Defined(RecordINIfiles)} WriteLineToDebugFile('Breakpoint 2'); {$EndIf}
   {$IfDef RecordIniMemoryOverwrite} IniMemOverwriteCheck('Breakpoint 2'); {$EndIf}
   MicronetSettings;
   ContourSettings;
   DatumProjectionSettings;
   DisplaySettings;
   GeologySettings;
   GISBDSettings;
   MapDrawSettings;
   SatelliteSettings;
   MapGridSettings;

   {$IfDef ExDRGimport}
   {$Else}
      DRGImportSettings;
   {$EndIf}

   {$If Defined(RecordINIfiles) or Defined(RecordINIfiles)} WriteLineToDebugFile('Breakpoint 3'); {$EndIf}
   {$IfDef RecordIniMemoryOverwrite} IniMemOverwriteCheck('Breakpoint 3'); {$EndIf}

   with MDIniFile,MDDef do begin
      AParameterShortFloat('Anaglyph','AnaglyphVertExag',AnaglyphVertExag,3);
      AParameter('Anaglyph','MaxAnaglyphShift',MaxAnaglyphShift,30);
      AParameter('Anaglyph','EWAnaglyphShift',EWAnaglyphShift,true);

      AParameter('Buffers','RidgeMaskRadius',RidgeMaskRadius,250);
      AParameter('Buffers','StreamMaskDist',StreamMaskDist,250);
      AParameter('Buffers','ExcludeStreamsInMask',ExcludeStreamsInMask,false);
      AParameter('Buffers','ExcludeRoadsInMask',ExcludeRoadsInMask,false);
      AParameter('Buffers','ExcludeTerrainCatInMask',ExcludeTerrainCatInMask,false);
      AParameter('Buffers','MaskShapesIn',MaskShapesIn,true);
      AParameter('Buffers','MaskDistance',MaskDistance,250);
      AParameter('Buffers','RegionsRidgeMask',RegionsRidgeMask,false);
      AParameter('Buffers','RegionsShapeFileMask',RegionsShapeFileMask,false);
      AParameter('Buffers','ShapeMaskNearPoints',ShapeMaskNearPoints,true);
      AParameter('Buffers','ShapeMaskNearLines',ShapeMaskNearLines,true);
      AParameter('Buffers','ShapeMaskNearInsideAreas',ShapeMaskNearInsideAreas,true);
      AParameter('Buffers','ShapePointBufferDist',ShapePointBufferDist,250);
      AParameter('Buffers','ShapeLineBufferDist',ShapeLineBufferDist,250);
      AParameter('Buffers','ShapeAreaBufferDist',ShapeAreaBufferDist,250);
      AParameter('Buffers','TreatLineAsPolygon',TreatLineAsPolygon,false);
      AParameter('Buffers','TreatPolygonAsLine',TreatPolygonAsLine,false);

      AColorParameter('Buffers','MapMaskColor',MapMaskColor,claRed);
      AParameter('Buffers','MaskOpacity',MaskOpacity,50);

      {$IfDef NoClustering}
      {$Else}
         if (IniWhat = iniWrite) then IniFile.WriteInteger('Cluster','ClusterInitialization',ord(MVClusterClientDataSet.ioStdDev));
         if (IniWhat = iniRead) then ClusterInitialization := TInitializationOption(IniFile.ReadInteger('Cluster','ClusterInitialization',ord(MVClusterClientDataSet.ioStdDev)));
         if (iniWhat = iniInit) then ClusterInitialization := MVClusterClientDataSet.ioStdDev;

         AParameter('Cluster','ClusterIterations',ClusterIterations,5);
         AParameter('Cluster','NumClusters',NumClusters,15);
         AParameterShortFloat('Cluster','ClusterConvergenceThreshold',ClusterConvergenceThreshold,500);
         AParameter('Cluster','ShowClusterScatterPlots',ShowClusterScatterPlots,true);
         AParameter('Cluster','ShowMaskScatterPlots',ShowMaskScatterPlots,true);
         AParameter('Cluster','ShowClusterHistograms',ShowClusterHistograms,false);
         AParameter('Cluster','ShowMaskHistograms',ShowMaskHistograms,false);
         AParameterShortFloat('Cluster','ClassDistancePower',ClassDistancePower,2);
      {$EndIf}

   {$If Defined(RecordINIfiles) or Defined(RecordINIfiles)} WriteLineToDebugFile('Breakpoint 4'); {$EndIf}

      AParameter('Digitize','RapidCycle',RapidCycle,true);
      AParameter('Digitize','LabelRegisterPoints',LabelRegisterPoints,false);
      AParameter('Digitize','ContDigitizeSeparation',ContDigitizeSeparation,5);
      AColorParameter('Digitize','DigitizeColor',DigitizeColor,claRed);
      AParameter('Digitize','DigitizeWidth',DigitizeWidth,3);
      if (IniWhat = iniWrite) then IniFile.WriteInteger('Digitize','DigitizeMode',ord(DigitizeMode));
      if (IniWhat = iniRead) then DigitizeMode := tDigitizeMode(IniFile.ReadInteger('Digitize','DigitizeMode',ord(dmPoint)));
      if (iniWhat = iniInit) then DigitizeMode:= dmPoint;

      AParameter('Export','GeoJSONG_zdec',GeoJSONG_zdec,1);
      AParameter('Export','GeoJSONP_zdec',GeoJSONP_zdec,1);
      AParameter('Export','GeoJSONP_zdec',GeoJSONP_xydec,1);
      //AParameter('ImpExport','DeleteTarGZ',DeleteTarGZ,false);

   {$If Defined(RecordINIfiles) or Defined(RecordINIfiles)} WriteLineToDebugFile('Breakpoint 5'); {$EndIf}

      AParameterShortFloat('Graph','GraphDensityXBlock',GraphDensityXBlock,1);
      AParameterShortFloat('Graph','GraphDensityYBlock',GraphDensityYBlock,1);
      AParameter('Graph','FrameLineWidth',FrameLineWidth,2);
      AParameter('Graph','TransparentGIF',TransparentGIF,true);
      AParameter('Graph','TransparentPNG',TransparentPNG,true);
      AParameter('Graph','TransparentLevel',TransparentLevel,255);
      AParameter('Graph','UseGif',UseGif,false);
      AParameter('Graph','GIFDefaultDelay',GIFDefaultDelay,500);
      AParameter('Graph','GIFDefaultSize',GIFFontSize,18);
      AParameter('Graph','GIFfileLabels',GIFfileLabels,false);
      AParameter('Graph','DefMarginLegend',DefMarginLegend,0);

      AParameter('Graph','JPEGQuality',JPEGQuality,50);
      AParameter('Graph','DefaultGraphXSize',DefaultGraphXSize,600);
      AParameter('Graph','DefaultGraphYSize',DefaultGraphYSize,400);
      AParameter('Graph','BigBM_nc',BigBM_nc,3);

      AParameter('Graph','NoHistFreqLabels',NoHistFreqLabels,false);
      AParameter('Graph','RoseBothEnds',RoseBothEnds,false);
      AParameter('Graph','DeliberateHistogram',AskHistogramBins,false);
      AParameter('Graph','CreateGraphHidden',CreateGraphHidden,true);
      AParameter('Graph','DetailRoseLegend',DetailRoseLegend,true);
      AParameter('Graph','PurgeBigGraphSubGraphs', PurgeBigGraphSubGraphs,true);
      AParameter('Graph','ImageDiffThreshhold', ImageDiffThreshhold,35);
      AParameter('Graph','ErosionCycles',ErosionCycles,1);

      {$IfDef AllowGeomorphometry}
         AParameter('Graph','QuantileRanges',QuantileRanges,true);
         AParameter('Graph','CumFreqNormAxis',CumFreqNormAxis,true);
      {$EndIf}

      {$IfDef VCL}
         AParameterShortFloat('Hardware','PrinterScale',PrinterScale,50000);
         AParameter('Hardware','EnableGridNetworkComputing',EnableGridNetworkComputing,true);
         AParameter('Hardware','DefaultServerIP',DefaultServerIP,'127.0.0.1');
         AParameter('Hardware','DefaultServerPort',DefaultServerPort,7676);
         AParameter('Hardware','MaxThreadsForPC',MaxThreadsForPC,8);
         AParameter('Hardware','UpdateDelay',UpdateDelay,4);
         AParameter('Hardware','IsUSNAcomputer',IsUSNAcomputer,false);
         AParameter('Hardware','ShowWinExec',ShowWinExec,true);
         AParameter('Hardware','LogDOSoutput',LogDOSoutput,false);
      {$EndIf}

      AParameterShortFloat('Horizon','HorizonIncrement',HorizonIncrement,5);
      AParameterShortFloat('Horizon','HorizonLength',HorizonLength,25000);
      AParameterShortFloat('Horizon','HorizonRadialMultiple',HorizonRadialMultiple,0.25);
      AParameterShortFloat('Horizon','AzToSat',AzToSat,180);
      AParameterShortFloat('Horizon','ElevToSat',ElevToSat,23);
      AParameter('Horizon','HorizonSkyMap',HorizonSkyMap,true);
      AParameter('Horizon','InvertSkyline',InvertSkyline,false);
      AParameter('Horizon','DaylightDuration',DaylightDuration,true);
      AParameter('Horizon','HorizonDistanceGraph',HorizonDistanceGraph,false);
      AParameter('Horizon','HorizonVertAngleGraph',HorizonVertAngleGraph,false);
      AParameter('Horizon','ShowMaskedToSat',ShowMaskedToSat,true);
      AParameter('Horizon','SolarPathMap',SolarPathMap,true);
      AParameter('Horizon','ShowSolstices',ShowSolstices,true);

      {$IfDef ExKML}
      {$Else}
         AParameter('Files','KML Logo 1',KMLLogo1FileName,'');
         AParameter('Files','KML Logo 2',KMLLogo2FileName,'');
         AParameter('KML','KML_DB_tables',KML_DB_tables,false);
         AParameter('KML','ZipKMLfiles',ZipKMLfiles,true);
         AParameter('KML','KMLCreateWorldFiles',KMLCreateWorldFiles,true);
         AParameter('KML','KMLOpenGoogleEarth',KMLOpenGoogleEarth,true);
         AParameter('KML','KML_DB_tables',KML_DB_tables,true);
         AParameter('KML','CleanKMLDirOnClosing',CleanKMLDirOnClosing,false);
         AParameter('KML','CleanUpHTML',CleanUpHTML,true);
         AParameter('KML','KMLTimeAnimations',KMLTimeAnimations,true);
         AParameter('KML','KMLDefaultDB',KMLDefaultDB,true);
         AParameter('KML','AskAboutKMLExport',AskAboutKMLExport,false);
         AParameter('KML','KMLTileSize',KMLTileSize,4000);
         AParameter('KML','KMLZoomSize',KMLZoomSize,100);
         AParameter('KML','KMLLabelGraticule',KMLLabelGraticule,false);
         AParameter('KML','KMLTopLevelFolder',KMLTopLevelFolder,false);
         AParameter('KML','KMLDefaultDB',KMLDefaultDB,true);
         AParameter('KML','KMLLabelExports',KMLLabelExports,true);
         AParameter('KML','ThumbSize',ThumbSize,500);
         AParameter('KML','KMLImageOpts',KMLImageOpts,2);
         AParameter('KML','DB2KMKLThin',DB2KMKLThin,1);
         AParameterShortFloat('KML','KML_Las_offset',KML_Las_offset,5);
      {$EndIf}

      AParameter('LandCover','LongLandCoverResults',LongLandCoverResults,false);

      {$If Defined(RecordINIfiles) or Defined(RecordINIfiles)} WriteLineToDebugFile('Breakpoint 6'); {$EndIf}

      AParameterShortFloat('LineDraw','DefVectorLineMult',DefVectorLineMult,10);
      AColorParameter('LineDraw','HorizonColor',HorizonColor,claBrown);
      AParameter('LineDraw','ConnectRecordColoring',ConnectRecordColoring,false);
      AParameter('LineDraw','HorizonWidth',HorizonWidth,3);
      AParameter('LineDraw','ConnectArrows',ConnectArrows,false);
      AParameter('LineDraw','ConnectArrowSpacing',ConnectArrowSpacing,5);

      AParameter('MapMargin','SpecifyLegendX',SpecifyLegendX,0);
      AParameter('MapMargin','LegendSingleHeight',LegendSingleHeight,25);
      AParameter('MapMargin','LegendGraphWidth',LegendGraphWidth,50);
      AParameter('MapMargin','LegendBarWidth',LegendBarWidth,15);
      AParameter('MapMargin','LegendTickSize',LegendTickSize,10);
      AParameter('MapMargin','ClipboardExports',ClipboardExports,2);
      AParameter('MapMargin','BoxAroundQuickMaps',BoxAroundQuickMaps,true);
      AParameter('MapMargin','OpenGLCleanOverlays',OpenGLCleanOverlays,true);

      MarginaliaLocation('LegendLocation',GridLegendLocation,lpNWMap,true);
      MarginaliaLocation('ScaleBarLocation',ScaleBarLocation,lpSEMap,true);
      MarginaliaLocation('TerrainCatLegend',TerrainCatLegend,lpSWMap,true);
      MarginaliaLocation('NorthArrowLocation',NorthArrowLocation,lpSWMap,false);
      MarginaliaLocation('MapNameLocation',MapNameLocation,lpNMap,false);

      AParameter('MissData','ASCIIMissingValue',ASCIIMissingValue,-9999);
      AParameter('MissData','DTEDMissingValue',DTEDMissingValue,-32767);
      AParameter('MissData','GeotiffMissingValue',GeotiffMissingValue,-10000);
      AParameter('MissData','AssumeMinus99Missing',AssumeMinus99Missing,false);
      AParameter('MissData','AssumeMinus999Missing',AssumeMinus999Missing,false);
      AParameter('MissData','AssumeMinus9999Missing',AssumeMinus9999Missing,false);
      AParameter('MissData','AssumeMinus99999Missing',AssumeMinus99999Missing,false);
      AParameter('MissData','AssumeMinus32767Missing',AssumeMinus32767Missing,false);
      AParameter('MissData','AssumeMinus999999Missing',AssumeMinus999999Missing,false);
      AParameter('MissData','AssumeNegativeValuesMissing',AssumeNegativeValuesMissing,false);

    {$If Defined(RecordINIfiles) or Defined(RecordINIfiles)} WriteLineToDebugFile('Breakpoint 7'); {$EndIf}

      {$IfDef ExMrSID}
      {$Else}
         AParameter('MrSID','MaxMrSidImageSize',MaxMrSidImageSize,4000);
         AParameter('MrSID','AskAboutSIDLevel',AskAboutSIDLevel,true);
      {$EndIf}

      {$IfDef ExMultiGrid}
      {$Else}
         AParameter('MultiGridOp','doEnvDEM',doEnvDEM,false);
         AParameter('MultiGridOp','doNPTsDEM',doNPTsDEM,false);
         AParameter('MultiGridOp','doSTDDEM',doSTDDEM,false);
         AParameter('MultiGridOp','doMedDEM',doMedDEM,false);
         AParameter('MultiGridOp','doFloorDEM',doFloorDEM,false);
         AParameter('MultiGridOp','doCeilingDEM',doCeilingDEM,false);
         AParameter('MultiGridOp','FuzzyMatches',FuzzyMatches,false);
      {$EndIf}

      {$IfDef ExGeomorphGrids}
      {$Else}
         AParameter('NewGrid','doDEMwithMax',doDEMwithMax,true);
         AParameter('NewGrid','doDEMwithMin',doDEMwithMin,true);
         AParameter('NewGrid','doMeanDEM',doMeanDEM,false);
         AParameter('NewGrid','doEnvDEM',doEnvDEM,false);
         AParameter('NewGrid','doNPTsDEM',doNPTsDEM,false);
         AParameter('NewGrid','doSTDDEM',doSTDDEM,false);
         AParameter('NewGrid','doMedDEM',doMedDEM,false);
         AParameter('NewGrid','doFloorDEM',doFloorDEM,false);
         AParameter('NewGrid','doCeilingDEM',doCeilingDEM,false);
         AParameterShortFloat('NewGrid','MinMaxGridTolerance',MinMaxGridTolerance,0.5);
      {$EndIf}

      {$IfDef ExOceanography}
      {$Else}
         AParameter('OceanModels','WindWidth',WindWidth,2);
         AParameter('OceanModels','TideWidth',TideWidth,2);
         AParameter('OceanModels','ShipWidth',ShipWidth,2);
         AParameter('OceanModels','WindCurrentWidth',WindCurrentWidth,2);
         AParameter('OceanModels','ResultantWidth',ResultantWidth,3);
         AColorParameter('OceanModels','WindColor',WindColor,claPurple);
         AColorParameter('OceanModels','TideColor',TideColor,claLime);
         AColorParameter('OceanModels','ShipColor',ShipColor,claBlue);
         AColorParameter('OceanModels','WindCurrentColor',WindCurrentColor,claYellow);
         AColorParameter('OceanModels','ResultantColor',ResultantColor,claRed);
      {$EndIf}

   {$If Defined(RecordINIfiles) or Defined(RecordINIfiles)} WriteLineToDebugFile('Breakpoint 8'); {$EndIf}

      {$IfDef ExOpenGL}
      {$Else}
         with OGLDefs do begin
            AParameterFloat('OpenGL','MoveIncr',MoveIncr,7.5);
            AParameter('OpenGL','DrawOGLAxes',DrawOGLAxes,true);
            //AParameter('OpenGL','MaxInitOpenGLTriangles',MaxInitOpenGLTriangles,1000000);
            //AParameter('OpenGL','MaxOpenGLPoints',MaxOpenGLPoints,25000000);
            AParameter('OpenGL','OpenGLDefaultWidth',OpenGLDefaultWidth,1200);
            AParameter('OpenGL','OpenGLDefaultHeight',OpenGLDefaultHeight,800);
            AParameter('OpenGL','OpenGLDefaultTopX',OpenGLDefaultTopX,25);
            AParameter('OpenGL','OpenGLDefaultTopY',OpenGLDefaultTopY,10);
         end;
         AParameter('OpenGL','OpenGL_VE',OpenGL_VE,false);
         AParameterShortFloat('OpenGL','OGLPointSize',OGLPointSize,1);
         AParameter('OpenGL','OGLObjectSize',OGLObjectSize,5);
      {$EndIf}

      AParameterShortFloat('Openness','OpennessHowHigh',OpennessHowHigh,0);
      AParameter('Openness','ConfirmOpennesDirections',ConfirmOpennesDirections,false);
      AParameter('Openness','OpennessHt',OpennessHt,opOnGround);
      AParameter('Palettes','ElevPalName',ElevPalName,'Terrain, 25 steps');

    {$If Defined(RecordINIfiles) or Defined(RecordINIfiles)} WriteLineToDebugFile('Breakpoint 9'); {$EndIf}

      {$IfDef ExPOTRACE}
      {$Else}
         AParameter('RasterVector','potrace_tsize', potrace_tsize,10);
         AParameter('RasterVector','potrace_invert', potrace_invert,false);
         AParameterShortFloat('RasterVector','potrace_black', potrace_black,0.5);
         AParameter('RasterVector','potrace_outline', potrace_outline,1);
         AParameterShortFloat('RasterVector','potrace_corner', potrace_corner,1);
      {$EndIf}

      AParameter('Slope','SlopeAlg',SlopeAlg,smEightNeighborsUnweighted);
      AParameter('Slope','SlopeRegionRadius',SlopeRegionRadius,1);
      AParameter('Slope','AspectRegionSize',AspectRegionSize,5);
      AParameter('Slope','NumSlopeBands',NumSlopeBands,5);

      AParameter('SpeedDist','AddDist',AddDist,true);
      AParameter('SpeedDist','Add3DDist',Add3DDist,false);
      AParameter('SpeedDist','AddCumDist',AddCumDist,true);
      AParameter('SpeedDist','UseMeters',UseMeters,true);
      AParameter('SpeedDist','AddSpeed',AddSpeed,true);
      AParameter('SpeedDist','AddAzimuth',AddAzimuth,true);

      {$IfDef VCL}
         AParameter('ProgramOptions','ShowMainToolbar',ShowMainToolbar,true);
         AParameter('ProgramOptions','ShowMapToolbar',ShowMapToolbar,true);
         AParameter('ProgramOptions','BackupEXEbeforeUpdate',BackupEXEbeforeUpdate,true);
         AParameter('ProgramOptions','RunOddballLocation', RunOddballLocation,false);

         AParameter('StateCountyMaps','PixelSizeToShowCounties',PixelSizeToShowCounties,300);
         AParameter('StateCountyMaps','PixelSizeToShowStates',PixelSizeToShowStates,1000);
         AParameter('StateCountyMaps','USOutlinesOnDEMs',USOutlinesOnDEMs,false);
         AParameter('StateCountyMaps','USOutlinesOnImagery',USOutlinesOnImagery,false);
         AColorParameter('StateCountyMaps','CountryOutline_Color',CountryOutline_Color,claRed);
         AParameter('StateCountyMaps','CountryOutline_Width',CountryOutline_Width,3);
         AColorParameter('StateCountyMaps','ProvinceOutline_Color',ProvinceOutline_Color,claRed);
         AParameter('StateCountyMaps','ProvinceOutline_Width',ProvinceOutline_Width,2);
         AColorParameter('StateCountyMaps','US_StateOutline_Color',US_StateOutline_Color,claBlack);
         AParameter('StateCountyMaps','US_StateOutline_Width',US_StateOutline_Width,2);
         AColorParameter('StateCountyMaps','US_CountyOutline_Color',US_CountyOutline_Color,claBlack);
         AParameter('StateCountyMaps','US_CountyOutline_Width',US_CountyOutline_Width,1);
         AColorParameter('StateCountyMaps','US_FennemanColor',US_FennemanColor,claBlack);
         AParameter('StateCountyMaps','US_FennemanWidth',US_FennemanWidth,1);
         AParameter('StateCountyMaps','US_Highway_Width',US_Highway_Width,2);
         AColorParameter('StateCountyMaps','US_Highway_Color',US_Highway_Color,claRed);
         AColorParameter('StateCountyMaps','US_River_Color',US_River_Color,claBlue);
         AParameter('StateCountyMaps','US_River_Width',US_River_Width,1);
         AParameter('Carto','CloseCartFormOnOpen',CloseCartFormOnOpen,true);
      {$EndIf}


   {$If Defined(RecordINIfiles) or Defined(RecordINIfiles)} WriteLineToDebugFile('Breakpoint 10'); {$EndIf}

      {$IfDef ExVegDensity}
      {$Else}
         AParameter('Veg','VegDensityGraphAverage',VegDensityGraphAverage,true);
         AParameter('Veg','VegDensityGraphMaxDensity',VegDensityGraphMaxDensity,15);
         AParameter('Veg','AutoLoadVegGrid',AutoLoadVegGrid,false);
         AParameter('Veg','AutoLoadVegDensityGrids',AutoLoadVegDensityGrids,false);
         AParameter('Veg','VegEffectsVoxels',VegEffectsVoxels,true);
         AParameter('Veg','VegDensityGroundPoints',VegDensityGroundPoints,true);
         AParameter('Veg','VegDensityBuildingPoints',VegDensityBuildingPoints,true);
         AParameter('Veg','VegDensityRandomizePoints',VegDensityRandomizePoints,true);
         AParameterShortFloat('Veg','VegGridRandomizationDistance',VegGridRandomizationDistance,0.6);
         AParameter('Veg','VegDensityHeights',VegDensityHeights,45);
         AParameter('Veg','MaxVegHeight',MaxVegHeight,45);
      {$EndIf}

      {$IfDef ExWebDownload}
      {$Else}
         AParameter('Web','GeocodeAddress',GeocodeAddress,'');
         AParameter('Web','GoogleAPIKey',GoogleAPIKey,'');
      {$EndIf}

      {$IfDef ExWMS}
      {$Else}
          AParameter('Web','WMSOpacity',WMSOpacityValue,75);
      {$EndIf}


   {$If Defined(RecordINIfiles) or Defined(RecordINIfiles)} WriteLineToDebugFile('Breakpoint 11 (before fonts)'); {$EndIf}

      {$IfDef VCL}
         AColorParameter('Maps','InsideMapGridColor',InsideMapGridColor,claNearWhite);
         InitializeMyFont('ContourLabelFont',ContourLabelFont,'TimesNewRoman',12,claBlack);
         InitializeMyFont('DefGisLabelFont1',DefGisLabelFont1,'Courier New',8,claBlack);
         InitializeMyFont('DefGisLabelFont2',DefGisLabelFont2,'Courier New',8,claBlack);
         InitializeMyFont('DefGISLegendFont',DefGISLegendFont,'Arial New',12,claBlack);
         InitializeMyFont('TitleLabelFont',TitleLabelFont,'Arial',18,claBlack);
         InitializeMyFont('CollarUnitsFont',CollarUnitsFont,'Arial',12,claBlack);
         InitializeMyFont('LegendFont',LegendFont,'Verdana',14,claBlack);
         InitializeMyFont('DefaultGraphFont',DefaultGraphFont,'Verdana',14,claBlack);
         InitializeMyFont('InsideGridFont',InsideGridFont,'Verdana',11,claBlack);

         InitializeMyFont('DBtableGridFont',DBtableGridFont,'Tahoma',8,claBlack);
         InitializeMyFont('DBtableTitleFont',DBtableTitleFont,'Tahoma',8,claBlack);
         InitializeMyFont('DBrecordTextFont',DBrecordTextFont,'Tahoma',8,claBlack);

         InitializeMyFont('LOSFont',LOSFont,'Verdana',14,claBlack);
         InitializeMyFont('PerspFont',PerspFont,'Verdana',14,claBlack);
      {$EndIf}
   end;

   for I := 1 to 8 do MDDef.OpennessDirs[i] := true;
   if (not (MDDef.DefaultUTMZone in [1..60])) then MDDef.DefaultUTMZone := 18;

   MDiniFile.CloseMDiniFile;

   {$If Defined(RecordINIfiles) or Defined(RecordINIfiles)} WriteLineToDebugFile('ProcessIniFile out'); {$EndIf}
end;


{$IfDef VCL}

procedure PickForNotePadPlusPlus(DefFilt : byte);
var
   FileNames,
   FilesWanted : TStringList;
   i,j: integer;
   fName,Dir : PathStr;
begin
   {$IfDef RecordDrape} WriteLineToDebugFile('TMapForm.Worldfileimages1Click in'); {$EndIf}
   FilesWanted := tStringList.Create;
   FilesWanted.Add(MainMapData);
   if GetMultipleFiles('Files to display','KML/KMZ|*.kml;*.kmz|XML|*.xml|VRML|*.wrl',FilesWanted,DefFilt) then begin
       for i := 0 to pred(FilesWanted.Count) do begin
          fName := FilesWanted[i];
          if FileExtEquals(fName,'.KML') or  FileExtEquals(fName,'.XML') or FileExtEquals(fName,'.WRL') then begin
             ShowInNotepadPlusPlus(fName);
          end
          else if FileExtEquals(fName,'.KMZ') then begin
             UnzipKMLtoKMLDir(Dir,fName);
             FileNames := Nil;
             Petmar.FindMatchingFiles(Dir,'*.KML',FileNames);
             for j := 0 to pred(FileNames.Count) do begin
                fName := FileNames.Strings[j];
                ShowInNotepadPlusPlus(fName);
             end;
             FileNames.Free;
          end;
       end;
   end;
end;


function USNAcomputer : boolean;
begin
   Result := (Copy(LocalIP,1,5) = '10.50') or (Copy(LocalIP,1,5) = '10.23') or (Copy(LocalIP,1,5) = '10.25') or MDDef.IsUSNAcomputer;
end;


procedure CheckFile(fName : PathStr);
var
  OutName,OutName2,InName : ANSIstring;
begin
   if (fName <> '') then begin
      OutName := fName;
      if UpperCase(ptcopy(OutName,1,3)) = 'C:\' then OutName2 := OutName
      else OutName2 := ProgramRootDir + OutName;
      if (FileExists(OutName2)) then exit;

      {$IfDef SQLiteDefaultDBs}
      if FileExtEquals(fName,'.db') then begin
         OldName := ChangeFileExt(fName,'.dbf');
         if FileExists(OldName) then begin
            ConvertDBFtoSQLite(OldName);
            exit;
         end;
      end;
      {$EndIf}

     if (copy(fname,1,7) = 'http://') or (copy(fname,1,8) = 'https://') then InName := fName
     else InName := WebDataDownLoadDir + LowerCase(ExtractFileName(fName));

     StopSplashing;
     SetCursorPos(600,600);
     if USNAComputer or AnswerIsYes('MICRODEM requires ' + fName +'; Allow automatic download now') then begin
        {$IfDef RecordUpdate} WriteLineToDebugFile(LocalIP + ' CheckFile Copy ' + InName + ' to ' + OutName2); {$EndIf}
        DownloadFileFromWeb(InName,OutName2);
     end;
  end;
end;
{$EndIf}


{$IfDef NoClustering}
{$Else}
procedure DefineMICRODEMClusteringOptions(var MVClusterClientDataSet : tMVClusterClientDataSet);
begin
   MVClusterClientDataSet.NIterations := MDDef.ClusterIterations;
   MVClusterClientDataSet.InitOption := MDDef.ClusterInitialization;
   MVClusterClientDataSet.ConvergenceThreshold := MDDef.ClusterConvergenceThreshold;
   MVClusterClientDataSet.NClusters := MDDef.NumClusters;
   MVClusterClientDataSet.AccumulateClusterStats := MDDef.ShowClusterResults and MDDef.IncludeClusterStatistics;
end;
{$EndIf}

procedure ToggleShowProgress(ShowIt : boolean);
begin
   ShowSatProgress := ShowIt;
   ShowDEMReadingProgress := ShowIt;
   {$IfDef VCL}
      SkipMenuUpdating := not ShowIt;
   {$EndIf}
   ReportErrors := ShowIt;
end;


procedure GetMapDataDirectory;
begin
   {$IfDef MessageStartupUnit} MessageToContinue('GetMapDataDirectory in ' + MainMapData); {$EndIf}
   repeat
      GetDosPath('Map data path',MainMapData);
   until (Uppercase(Copy(MainMapData,4,7)) = 'MAPDATA') or AnswerIsYes('Proceed with MAPDATA not in a drive root directory (not recommended)');
   {$IfDef MessageStartupUnit} MessageToContinue('GetMapDataDirectory out ' + MainMapData); {$EndIf}
end;


procedure SetOptionsForMapWithNoMarginalia;
begin
   MDDef.GridLegendLocation.DrawItem := false;
   MDDef.ScaleBarLocation.DrawItem := false;
end;


function USGSGazeeteerFile(fName : PathStr) : boolean;
begin
   Result := (UpperCase(Copy(ExtractFileName(fName),3,5)) = '_DECI') or (UpperCase(Copy(ExtractFileName(fName),3,5)) = '_FEAT');
end;

function CanadianGazeeteerFile(fName : PathStr) : boolean;
begin
   Result := UpperCase(Copy(ExtractFileName(fName),3,8)) = '_GEONAME';
end;


{$IfDef ExGazetteer}
{$Else}

procedure SetUpGazFile(var GazColors : tMyData; var NameStr : ShortString; FilterUse : boolean = true);
var
   USGSGaz,CanadianGaz : boolean;
begin
   {$IfDef RecordGazOps} WriteLineToDebugFile('SetUpGazFile ' + GazOptFName); {$EndIf}
   GazColors := tMyData.Create(GazOptFName);
   {$IfDef RecordGazOps} WriteLineToDebugFile('opened OK  check gaz type, ' + LastGazFile); {$EndIf}
   USGSGaz := USGSGazeeteerFile(LastGazFile);
   CanadianGaz := CanadianGazeeteerFile(LastGazFile);
   if USGSGaz then NameStr := 'USGS'
   else if CanadianGaz then NameStr := 'CCOG'
   else NameStr := 'NGA';
   If FilterUse then GazColors.ApplyFilter( 'GAZ=' + QuotedStr(NameStr) + ' AND USE=' + QuotedStr('Y'))
   else GazColors.ApplyFilter( 'GAZ=' + QuotedStr(NameStr));
   {$IfDef RecordGazOps} WriteLineToDebugFile('filtered OK'); {$EndIf}
   if USGSGaz then NameStr := 'FEATURE'
   else if CanadianGaz then NameStr := 'GEONAME'
   else NameStr := 'NAME';
   {$IfDef RecordGazOps} WriteLineToDebugFile('GazFilter=' + GazColors.Filter + '    Gaz NameStr=' + NameStr); {$EndIf}
end;


procedure PickGazFeatures;
var
  Table : tMyData;
  NameStr : ShortString;
begin
   SetUpGazFile(Table,NameStr,false);
   {$IfDef VCL}
      Toggle_db_use.VerifyRecordsToUse(Table,'NAME','Features to show');
   {$EndIf}
   Table.Destroy;
end;

{$EndIf}

function MissingData(z : float64) : boolean;
begin
   Result := (MDDef.AssumeMinus99Missing and (z > -99.01) and (z < -98.99)) or
             (MDDef.AssumeMinus999Missing and (z > -999.01) and (z < -998.99)) or
             (MDDef.AssumeMinus9999Missing and (z > -9999.01) and (z < -9998.99)) or
             (MDDef.AssumeMinus99999Missing and (z > -99999.01) and (z < -99998.99)) or
             (MDDef.AssumeMinus32767Missing and (z > -32767.01) and (z < -32766.99)) or
             (MDDef.AssumeMinus999999Missing and (z > -999999.01) and (z < -999998.99)) or
             (MDDef.AssumeNegativeValuesMissing and (z < -0.00000001));
end;


procedure SetSmallGraphs;
begin
    DEMDef_routines.SaveBackupDefaults;
    MDDef.DefaultGraphFont.Size := 10;
    MDDef.DefaultGraphXSize := 400;
    MDDef.DefaultGraphYSize := 300;
end;

procedure GetRangeFactor(var Factor : float64);
begin
   case MDDef.RangeCircleUnit of
      rcFeet : Factor := 1 / FeetToMeters;
      rcYard : Factor := 1 / 3 / FeetToMeters;
      rcMile : Factor := 0.001 * KM2Miles;   //  1 / 1609.344;
      rcNautMile : Factor := 0.001 * KM2NauticalMiles;   //   1 / 1852;
      else Factor := 1;
   end;
end;

procedure AddFreeDiskSpaceToDebugFile;
begin
   {$If Defined(MSWindows) or Defined(RecordProblems)} WriteLineToDebugFile('Microdem Temp Dir: ' + MDTempDir  +  '   Free space: ' + SmartMemorySizeBytes(DiskFree(ord(UpCase(MDTempDir[1])) - ord('A') + 1))); {$EndIf}
end;

function LOSAlgorithmDescription(iva :  tIntervisibilityAlgorithm) : ShortString;
begin
   with Iva do begin
      Result := LOSAlgorithmName[LOSAlgorithm] + ' / ' + StraightAlgorithmName[StraightAlgorithm] + '/' +
         BaseCurvAlgName[FanCurvAlg ] + '/' + ElevInterpolationName[ElevInterpolation];
      if (FanMethod = fmRadialIHS) then Result := Result + RealToString(FanDEMSpaceMultiple,-8,-2) + '-' + RealToString(FanMapSpaceMultiple,-8,-1);
      if (FanMethod = fmFanRadials) then Result := Result +  ' ' + RealToString(MaskRaySpacingDeg,-8,-2) + '°-' + RealToString(MaskAreaInterval,-8,-1) + 'm';
   end;
end;

function IntervisiblityAlgorithmName(iva :  tIntervisibilityAlgorithm) : ShortString;
var
   TStr,TStr2 : ShortString;
begin
   with iva do begin
      if (FanMethod = fmAllPointToPoint) then TStr := LOSAlgorithmName[LOSAlgorithm] + ' / '
      else TStr := '';
      Result := FanMethodName[FanMethod] +  '/ ' + TStr;
      case LOSAlgorithm of
         losMicrodemFractional : TStr2 := RealToString(FanDEMSpaceMultiple,-8,-2) + '-' + RealToString(FanMapSpaceMultiple,-8,-1);
         losMicrodemConstant : TStr2 := RealToString(MaskAreaInterval,-8,-1) + 'm';
         losYoeli :  TStr2 := 'grid sides';
         losBresenham : TStr2 := 'grid nodes';
      end;
      if (FanMethod = fmRadialIHS) then Result := Result +  TStr2;
      if (FanMethod = fmFanRadials) then Result := Result +  RealToString(MaskRaySpacingDeg,-8,-2) + '°-' + TStr2;
      if (FanMethod = fmAllPointToPoint) then begin
         if FanViewerMustBeGrid then Result := Result + '/Viewer on grid';
         if FanTargetMustBeGrid then Result := Result + '/Target on grid';
      end;
      Result := Result + '/' + StraightAlgorithmName[StraightAlgorithm] + '/' + BaseCurvAlgName[FanCurvAlg ] + '/' +  ElevInterpolationName[ElevInterpolation];
   end;
end;

procedure SaveBackupDefaults;
begin
   if (BackupMDDef = Nil) then New(BackupMDDef);
   BackupMDDef^ := MDDef;
end;

procedure RestoreBackupDefaults;
begin
   if (BackupMDDef <> Nil) then begin
      MDDef := BackupMDDef^;
      Dispose(BackupMDDef);
      BackupMDDef := Nil;
   end;
end;


{$IfDef ExGeography}
{$Else}
procedure SetKoppenOpts(var KoppenOpts :  tKoppenOpts);
begin
   ProcessIniFile(iniInit,'KOPPEN');
end;
{$EndIf}

procedure SetLOSDefaults;
begin
   ProcessIniFile(iniInit,'LOS');
end;


procedure SetGazDefaults;
begin
   ProcessIniFile(iniInit,'Gazetteer');
end;

function TerrainCategoryToString(tc : tTerrainCatDefinition) : ShortString;
begin
   with tc do begin
      Result := RealToString(CatMinElev,10,2) +       //1
      RealToString(CatMaxElev,10,2) +                 //11
      IntegerToString(CatReliefRadius,10) +           //21
      IntegerToString(CatMinRelief,10) +              //31
      IntegerToString(CatMaxRelief,10) +              //41
      RealToString(CatMinSlope,10,2) +                //51
      RealToString(CatMaxSlope,10,2) +                //61
      IntegerToString(ConvertPlatformColorToTColor(CatColor),12) +                  //71
          '11111111' +                                //83
          '0000';                                     //91
      if not (cdN in CatAspects) then Result[83] := '0';
      if not (cdNE in CatAspects) then Result[84] := '0';
      if not (cdE in CatAspects) then Result[85] := '0';
      if not (cdSE in CatAspects) then Result[86] := '0';
      if not (cdS in CatAspects) then Result[87] := '0';
      if not (cdSW in CatAspects) then Result[88] := '0';
      if not (cdW in CatAspects) then Result[89] := '0';
      if not (cdNW in CatAspects) then Result[90] := '0';
      if UseElevation then Result[91] := '1';
      if UseSlope then Result[92] := '1';
      if UseAspect then Result[93] := '1';
      if UseRelief then Result[94] := '1';
   end;
{$IfDef RecordTerrainCategories} WriteLineToDebugFile('TerrainCategoryToString=' + Result); {$EndIf}
end;


function StringToTerrainCategory(str : ShortString) : tTerrainCatDefinition;
var
   err : integer;
   Color : tColor;
begin
{$IfDef RecordTerrainCategories} WriteLineToDebugFile('StringToTerrainCategory=' + str); {$EndIf}
   with Result do begin
      Val(Copy(str,1,10),CatMinElev,err);
      Val(Copy(str,11,10),CatMaxElev,err);
      Val(Copy(str,21,10),CatReliefRadius,err);
      Val(Copy(str,31,10),CatMinRelief,err);
      Val(Copy(str,41,10),CatMaxRelief,err);
      Val(Copy(str,51,10),CatMinSlope,err);
      Val(Copy(str,61,10),CatMaxSlope,err);
      Val(Copy(str,71,12),Color,err);
      CatColor := ConvertTColorToPlatformColor(Color);
      CatAspects := [];
      if str[83] = '1' then CatAspects := CatAspects + [cdN];
      if str[84] = '1' then CatAspects := CatAspects + [cdNE];
      if str[85] = '1' then CatAspects := CatAspects + [cdE];
      if str[86] = '1' then CatAspects := CatAspects + [cdSE];
      if str[87] = '1' then CatAspects := CatAspects + [cdS];
      if str[88] = '1' then CatAspects := CatAspects + [cdSW];
      if str[89] = '1' then CatAspects := CatAspects + [cdW];
      if str[90] = '1' then CatAspects := CatAspects + [cdNW];
      UseElevation := str[91] = '1';
      UseSlope := str[92] = '1';
      UseAspect := str[93] = '1';
      UseRelief := str[94] = '1';
   end;
end;


{$IfDef ExViewshed}
{$Else}

procedure RecolorFan(var Bitmap : tMyBitmap; Color : tPlatFormColor);
{$IfDef VCL}
var
   x,y : integer;
   p0 : prgb;
begin
   for y := 0 to pred(Bitmap.Height) do begin
      p0 := Bitmap.Scanline[y];
      for x := 0 to pred(Bitmap.Width) do begin
         if (not SameColor(P0[x],RGBTripleWhite)) then p0[x] := Color;
      end;
   end;
{$Else}
begin
{$EndIf}
end;


procedure ResetDefaultFanAlgorithm;
begin
   ProcessIniFile(iniInit,'WeapFanAlg');
end;


procedure InitializeWeaponsFanColors(var WeaponsFan : tWeaponsFan);
begin
   WeaponsFan.ThisFanColor := MDdef.FanColor;
   WeaponsFan.ThisMaskColor := MDdef.MaskColor;
   WeaponsFan.FanShowWhat  := MDDef.wf.FanShowVisible;
end;


procedure InitializeWeaponsFan(var WeaponsFan : tWeaponsFan);
begin
   InitializeWeaponsFanColors(WeaponsFan);
   WeaponsFan.FanZoomFactor := MDDef.FanMapZoom;
   WeaponsFan.w_Lat := 100;
   WeaponsFan.Fan_Name := '';
   WeaponsFan.W_Range  := MDdef.MaskObsRange;
   if MDDef.DefaultObserverTerrainHug then WeaponsFan.W_Up := MDdef.ObsAboveGround
   else WeaponsFan.W_Up := MDDef.DefaultObserverASL;

   if MDDef.DefaultTargetTerrainHug then WeaponsFan.W_TargetUp := MDdef.TargetAboveGround
   else WeaponsFan.W_TargetUp := MDDef.DefaultTargetASL;

   WeaponsFan.StartAngle := MDdef.StartFanAngle;
   WeaponsFan.EndAngle   := MDdef.EndFanAngle;
   WeaponsFan.UpAngle    := MDDef.FanUpAngle;
   WeaponsFan.DownAngle  := MDDef.FanDownAngle;
   WeaponsFan.TargetTerrainHug := MDDef.DefaultTargetTerrainHug;
   WeaponsFan.ObserverTerrainHug := MDDef.DefaultObserverTerrainHug;
   WeaponsFan.noUseSensorNoTerrainBlock := false;
   WeaponsFan.FanFileName := '';
end;


function WeaponsTableBasicParametersToFan(PrimaryMapDatum : tMapProjection; WeaponsTable : tMyData) : tWeaponsFan;
var
   Lat,Long : float32;
begin
   with WeaponsTable do begin
      InitializeWeaponsFan(Result);
      Lat := WeaponsTable.GetFieldByNameAsFloat('LAT');
      Long := GetFieldByNameAsFloat('LONG');
      MolodenskiyTransformation(Lat,Long,Result.W_Lat,Result.W_Long,WGS84DatumConstants,PrimaryMapDatum);

      {$IfDef RecordFan}
         WriteLineToDebugFile('Fan location in file (WGS84): ' + LatLongDegreeToString(Lat,Long));
         WriteLineToDebugFile('Fan location for map: ' + LatLongDegreeToString(Result.W_Lat,Result.W_Long));
      {$EndIf}

      if WeaponsTable.FieldExists('NAME') then Result.Fan_Name := GetFieldByNameAsString('NAME')
      else Result.Fan_Name := 'Sensor';
      if WeaponsTable.FieldExists('IMAGE') then Result.FanFileName := GetFieldByNameAsString('IMAGE')
      else Result.FanFileName := '';

      Result.W_Range := GetFieldByNameAsFloat('SENSOR_RNG');
      Result.W_Up := GetFieldByNameAsFloat('SENSOR_UP');

      Result.StartAngle := GetFieldByNameAsInteger('LEFT_AZ');
      Result.EndAngle := GetFieldByNameAsInteger('RIGHT_AZ');

      Result.UpAngle := GetFieldByNameAsFloat('MAX_VERT');
      Result.DownAngle  := GetFieldByNameAsFloat('MIN_VERT');

      if WeaponsTable.FieldExists('MIN_RNG') then MDDef.DefWeaponsMinRange := GetFieldByNameAsFloat('MIN_RNG');
      if WeaponsTable.FieldExists('TARGET_UP') then Result.W_TargetUp := GetFieldByNameAsFloat('TARGET_UP');
   end;
end;


function WeaponsTableToFan(PrimaryMapDatum : tMapProjection; WeaponsTable : tMyData) : tWeaponsFan;
begin
  {$IfDef RecordFan} WriteLineToDebugFile('WeaponsTableToFan enter'); {$EndIf}
   with WeaponsTable do begin
      Result := WeaponsTableBasicParametersToFan(PrimaryMapDatum,WeaponsTable);
      if WeaponsTable.FieldExists('TERR_HUG') then Result.TargetTerrainHug := GetFieldByNameAsString('TERR_HUG') = 'Y';
      if WeaponsTable.FieldExists('OBS_HUG') then Result.ObserverTerrainHug := GetFieldByNameAsString('OBS_HUG') = 'Y';
      if WeaponsTable.FieldExists('NO_TERR_BL') then Result.noUseSensorNoTerrainBlock := GetFieldByNameAsString('NO_TERR_BL') = 'Y';
      Result.ThisFanColor := ConvertTColorToPlatformColor(GetFieldByNameAsInteger('VIS_COLOR'));
      Result.FanShowWhat := tFanshow(GetFieldByNameAsInteger('FAN_TYPE'));
   end;
  {$IfDef RecordFan} WriteLineToDebugFile('WeaponsTableToFan exit, fan at ' + LatLongDegreeToString(Result.W_Lat,Result.W_Long)); {$EndIf}
end;


procedure WeaponsTableToMDdefaults(TheData : tMyData);
begin
   MDDef.wf.FanShowVisible := tFanshow(TheData.GetFieldByNameAsInteger('FAN_TYPE'));
   MDDef.FanColor := ConvertTColorToPlatformColor(TheData.GetFieldByNameAsInteger('VIS_COLOR'));
end;


procedure AddMDDefaultsToWeaponsTable(var WeaponsTable : tMyData);
begin
   with WeaponsTable do begin
      SetFieldByNameAsInteger('FAN_TYPE',ord(MDDef.wf.FanShowVisible));
      SetFieldByNameAsInteger('VIS_COLOR',ConvertPlatformColorToTColor(MDDef.FanColor));
      if MDDef.ShowFanLocation then SetFieldByNameAsString('SHOW_LOC','Y');
      SetFieldByNameAsInteger('SYM_TYPE',ord(Cross));
      SetFieldByNameAsInteger('SYM_SIZE',5);
      SetFieldByNameAsInteger('SYM_COLOR',clBlack);
   end;
end;


procedure AddFanToWeaponsTable(PrimaryMapDatum : tMapProjection; EditIt,AddBasicParameters : boolean; var WeaponsTable : tMyData; WeaponsFan : tWeaponsFan);
var
   Lat,Long : float64;
begin
   {$IfDef RecordFan} WriteLineToDebugFile('AddFanToWeaponsTable in, Fan location on map: ' + LatLongDegreeToString(WeaponsFan.w_Lat,WeaponsFan.w_Long)); {$EndIf}
   if EditIt then WeaponsTable.Edit else WeaponsTable.Insert;
   RedefineWGS84DatumConstants(WeaponsFan.w_Long);
   MolodenskiyTransformation(WeaponsFan.w_Lat,WeaponsFan.w_Long,Lat,Long,PrimaryMapDatum,WGS84DatumConstants);
   {$IfDef RecordFan} WriteLineToDebugFile('Fan location in file (WGS84): ' + LatLongDegreeToString(Lat,Long)); {$EndIf}

   WeaponsTable.SetFieldByNameAsFloat('LAT',Lat);
   WeaponsTable.SetFieldByNameAsFloat('LONG',Long);

   WeaponsTable.SetFieldByNameAsString('USE','Y');
   if (WeaponsFan.Fan_Name = '') then WeaponsFan.Fan_Name := 'S_' + IntToStr(WeaponsTable.RecordCount);
   WeaponsTable.SetFieldByNameAsString('NAME',WeaponsFan.Fan_Name);
   WeaponsTable.SetFieldByNameAsFloat('SENSOR_RNG',WeaponsFan.W_Range);
   WeaponsTable.SetFieldByNameAsFloat('SENSOR_UP',WeaponsFan.W_UP);
   WeaponsTable.SetFieldByNameAsFloat('TARGET_UP',WeaponsFan.W_TargetUp);
   WeaponsTable.SetFieldByNameAsInteger('LEFT_AZ',round(WeaponsFan.StartAngle));
   WeaponsTable.SetFieldByNameAsInteger('RIGHT_AZ',round(WeaponsFan.EndAngle));
   WeaponsTable.SetFieldByNameAsString('USE','Y');
   WeaponsTable.SetFieldByNameAsFloat('MAX_VERT',WeaponsFan.UpAngle);
   WeaponsTable.SetFieldByNameAsFloat('MIN_VERT',WeaponsFan.DownAngle);
   WeaponsTable.SetFieldByNameAsFloat('MIN_RNG',MDDef.DefWeaponsMinRange);

   if AddBasicParameters then AddMDDefaultsToWeaponsTable(WeaponsTable);

   if WeaponsTable.FieldExists('TERR_HUG') then begin
      if WeaponsFan.TargetTerrainHug then WeaponsTable.SetFieldByNameAsString('TERR_HUG','Y')
      else WeaponsTable.SetFieldByNameAsString('TERR_HUG','N');
   end;

   if WeaponsTable.FieldExists('OBS_HUG') then begin
      if WeaponsFan.TargetTerrainHug then WeaponsTable.SetFieldByNameAsString('OBS_HUG','Y')
      else WeaponsTable.SetFieldByNameAsString('OBS_HUG','N');
   end;

   if WeaponsTable.FieldExists('NO_TERR_BL') then begin
      if WeaponsFan.noUseSensorNoTerrainBlock then WeaponsTable.SetFieldByNameAsString('NO_TERR_BL','Y')
      else WeaponsTable.SetFieldByNameAsString('NO_TERR_BL','N');
   end;
   WeaponsTable.Post;
   {$IfDef RecordFan} WriteLineToDebugFile('AddFanToWeaponsTable out'); {$EndIf}
end;

{$EndIf}




procedure SetDEMIXdirs(Ask : boolean = false);
//the option to get a path failed with a root directory, and we have enough hard coded that we did not want to change
var
   Ch : ansichar;
begin
   if PathIsValid(DEMIXSettingsDir) and PathIsValid(DEMIXresultsDir) and PathIsValid(DEMIXrefDataDir) then exit;
   ch := 'B';
   repeat
      inc(ch);
      MDDef.DEMIX_base_dir := ch + ':\';
      DEMIXSettingsDir := MDDef.DEMIX_base_dir + 'wine_contest_settings\';
      DEMIXresultsDir := MDDef.DEMIX_base_dir + 'wine_contest_results\';
      DEMIXrefDataDir := MDDef.DEMIX_base_dir +'wine_contest_reference_dems\';
      DEMIXTempFiles := MDDef.DEMIX_base_dir +'wine_contest_temp_files\';
   until (ch = 'Z') or (PathIsValid(DEMIXSettingsDir) and PathIsValid(DEMIXresultsDir) and PathIsValid(DEMIXrefDataDir));
   if (ch = 'Z') then MDDef.DEMIX_base_dir := 'DEMIX_MIA'
   else begin
      SafeMakeDir(DEMIXTempFiles);
      SafeMakeDir(DEMIXSettingsDir);
      SafeMakeDir(DEMIXresultsDir);
      SafeMakeDir(DEMIXrefDataDir);
   end;
end;

procedure SetRootDirectoryFiles;
begin
    {$IfDef RecordInitialization} WriteLineToDebugFile('SetRootDirectoryFiles in'); {$EndIf}
    GT_Datum_fName := ProgramRootDir + 'gt_datum' + DefaultDBExt;
    GT_Ellipse_fName := ProgramRootDir + 'gt_ellips' + DefaultDBExt;
    LasRulesName := ProgramRootDir + 'las_codes_v4' + DefaultDBExt;
    TM_RGB_fname := ProgramRootDir + 'tm_rgb_v3' + DefaultDBExt;
    CSVImportRulesFName := ProgramRootDir + 'CSV_IMPORT_RULES_v4'+ DefaultDBExt;
    SatBandNames := ProgramRootDir + 'sat_band_names_v20' + DefaultDBExt;
    ColorBrewerName := ProgramRootDir + 'color_palettes_v12' + DefaultDBExt;
    HardLimitColorPaletteFName := ProgramRootDir + 'hard_limit_color_palettes' + DefaultDBExt;
    TableDefinitionsFileName := ProgramRootDir + 'MD_TABLE_DEF_v2' + DefaultDBExt;
    GazOptFName := ProgramRootDir + 'gaz_symbols_v3' + DefaultDBExt;
    LandCoverFName := ProgramRootDir + 'land_cover_19' + DefaultDBExt;
    RangeCircleSizesfName := ProgramRootDir + 'range_circles' + DefaultDBExt;
    WKT_GCS_Proj_fName := ProgramRootDir + 'wkt_proj\gcs_wgs84.prj';
    if PathIsValid(ProgramRootDir + 'esri_proj') then begin
       RenameFile(ProgramRootDir + 'esri_proj', ExtractFilePath(WKT_GCS_Proj_fName));
    end;

    SetDEMIXdirs;

    {$IfDef ExMagVar}
    {$Else}
       www_mag_mod_fName  := ProgramRootDir + 'wmm_2020.cof';
    {$EndIf}

    {$IfDef ExPLSS}
    {$Else}
       PLSSMerfName := ProgramRootDir + 'plss_meridians' + DefaultDBExt;
    {$EndIf}
    {$IfDef ExTIGER}
    {$Else}
       TigerShapeRules := ProgramRootDir + 'tiger_rules_v6' + DefaultDBExt;
    {$EndIf}

    {$IfDef ExOSM}
    {$Else}
       OSMRoadRules := ProgramRootDir + 'osm_road_rules_v3' + DefaultDBExt;
       OSMGroupRules := ProgramRootDir + 'osm_map_rules_v2' + DefaultDBExt;
    {$EndIf}

    {$IfDef ExWMS}
    {$Else}
       WMS_servers_fName := ProgramRootDir + 'wms_servers_v8' + DefaultDBExt;
    {$EndIf}
    RecordDirs('end SetRootDirectoryFiles');
    {$IfDef RecordInitialization} WriteLineToDebugFile('SetRootDirectoryFiles out'); {$EndIf}
 end;


procedure SetDefaultDirectories;
begin
   {$IfDef  RecordInitialization} WriteLineToDebugFile('SetDefaultDirectories in, MainMapData=' + MainMapData); {$EndIf}
   {$IfDef VCL}
   MovieDir := MainMapData + 'MOVIES\';
   ImageDir := MainMapData + 'IMAGES\';
      {$IfDef ExTIN}
      {$Else}
         TINDir := MainMapData + 'TINS\';
      {$EndIf}
   {$EndIf}

   {$IfDef ExGazetteer}
   {$Else}
      GazetteerDir := MainMapData + 'Gazetteer\';
   {$EndIf}

   DBDir := MainMapData + 'DataBase\';

   ProjectDir := MainMapData + 'md-proj\';
   ShapeFileMaskFile := '';
   ShapeFileMaskDirectory := '';
   {$IfDef RecordInitializationDetailed} WriteLineToDebugFile('SetDefaultDirectories in, point 1'); {$EndIf}
   {$IfDef ExTIGER}
   {$Else}
      DEMTIGER.TigerIndex := ProgramRootDir + 'tiger_index' + DefaultDBExt;
   {$EndIf}

   {$IfDef MSWindows}
      SafeMakeDir(ProjectDir);
      CurrentProject := ProjectDir + 'CurrentProject\';
      SafeMakeDir(CurrentProject);
      SafeMakeDir(ProjectDir + 'Map_Layers');
      DeleteMultipleFiles(CurrentProject,'*.*');
      {$IfDef RecordInitializationDetailed} WriteLineToDebugFile('SetDefaultDirectories in, point 2'); {$EndIf}

      SafeMakeDir(MainMapData);
      SafeMakeDir(MovieDir);
      SafeMakeDir(ImageDir);
      SafeMakeDir(DBDir);
      SafeMakeDir(DBDir + 'Groups');
      SafeMakeDir(MainMapData + 'icons');
      SafeMakeDir(MainMapData + 'tiger_shapes');
      SafeMakeDir(MainMapData + 'temp');
      SafeMakeDir(ProjectDir + 'Map_Layers');

      {$IfDef ExGazetteer}
      {$Else}
         SafeMakeDir(GazetteerDir);
      {$EndIf}
      {$IfDef ExTIN}
      {$Else}
         SafeMakeDir(TINDir);
      {$EndIf}

      {$IfDef ExRiverNetworks}
      {$Else}
         SafeMakeDir(MainMapData + 'DRAINAGE');
      {$EndIf}

      {$IfDef ExGeology}
      {$Else}
         GeologyDir := MainMapData + 'geology\';
         SafeMakeDir(MainMapData + 'STRATCOL');
      {$EndIf}
   {$EndIf}
   {$IfDef RecordInitializationDetailed} WriteLineToDebugFile('SetDefaultDirectories in, point 3'); {$EndIf}


   {$IfDef ExGeography}
   {$Else}
      ClimateDir := MainMapData + 'climate\';
      WorldClimate2Dir := ClimateDir + '\world_climate_2.1\';
      ClimateStationFName := ClimateDir + 'climate_station_v3' + DefaultDBExt;
      GlobalWindsFName := ClimateDir + '\global_winds_v2' + DefaultDBExt;
      GlobalCurrentsFName := ClimateDir + '\ocean_currents'{_v2'} + DefaultDBExt;
      PiratesFName := DBDir + 'ASAM_21_JAN_16' + DefaultDBExt;
      MonthlyClimateFName := ProgramRootDir +  'monthly_climate_grids_v6.dbf';
      KoppenDefFName := ProgramRootDir + 'koppen_def_v4' + DefaultDBExt;
   {$EndIf}

   {$IfDef RecordInitializationDetailed} WriteLineToDebugFile('SetDefaultDirectories in, point 4'); {$EndIf}

   CheckGeoidNames;

   {$IfDef ExGeology}
   {$Else}
      MagAnomFile  := GeologyDir + 'geology_grids\EMAG2_V2.dem';
      SedThickFile := GeologyDir + 'geology_grids\glob_sed_v3.tif';
      SedTypeFile := GeologyDir + 'geology_grids\seabed_lithology_v1.dem';
      PredAgesFile := GeologyDir + 'geology_grids\ages_3_6.dem';
      ContCrustOutlineFile := GeologyDir + 'ut_plates_cont_crust\continental_crust.shp';
      CMT_fault_cent_fName := GeologyDir + 'cmt_quake_focal_mechs\cmt_fault_cent_mar_2022' + DefaultDBExt;
      GSFML_global_picks := GeologyDir + 'mag_anom_picks\GSFML_global_picks_oct_2018' + DefaultDBExt;
      PlateBoundaryFile := GeologyDir + 'usgs_plate_boundaries_complex\usgs_plate_boundaries.shp';
      VolcanoesDB := GeologyDir + 'smithsonian_volcanoes\volcanoes_4_11_0' + DefaultDBExt;
      MagneticAnomalyTimeScale := GeologyDir + 'time\cande_kent_time' + DefaultDBExt;
      EpochsTimeScale :=  'geologic_time_epochs';
      PeriodsTimeScale := 'geologic_time_periods';

      CurrentMotionsFile := GeologyDir + 'plate_motions\current_rotations_v4' + DefaultDBExt;
      PlatePolesFile := GeologyDir + 'plate_motions\total_poles' + DefaultDBExt;

      DSDP_db := GeologyDir + 'ocean_drilling\ocean_drilling_march_2022' + DefaultDBExt;
      Hotspot_db := GeologyDir + 'hot_spots\hotspot' + DefaultDBExt;
      PlateOutlinesfName := GeologyDir + 'plate_outlines\bird_argus_others_plates.shp';
   {$EndIf}
   {$IfDef RecordInitializationDetailed} WriteLineToDebugFile('SetDefaultDirectories in, point 5'); {$EndIf}

   BlueMarbleFName := MainMapData + 'nasa\world.topo.200410.3x5400x2700.png';
   CoastLineFile := DBDir + 'natural_earth_vector\50m_physical\ne_50m_coastline' + DefaultDBExt;
   RiversFile := DBDir + 'natural_earth_vector\50m_physical\ne_50m_rivers_lake_centerlines' + DefaultDBExt;

   //needed if map library is on an external drive and has changed
   if (MapLibDir <> '') and (not PathIsValid(MapLibDir)) then PickMapIndexLocation;

   {$IfDef MessageStartupUnit} MessageToContinue('end demdefs setdefaultdirectories'); {$EndIf}
   {$IfDef RecordInitialization} WriteLineToDebugFile('SetDefaultDirectories in, MainMapData=' + MainMapData); {$EndIf}
end;


procedure MakeRequiredDirectories;
begin
   {$IfDef MessageStartupUnit}  MessageToContinue('start demdefs MakeRequiredDirectories'); {$EndIf}
   {$IfDef  RecordInitialization} WriteLineToDebugFile('MakeRequiredDirectories in, MainMapData=' + MainMapData); {$EndIf}

   {$IfDef MSWindows}
      if (MainMapData = '') then MainMapData := 'c:\mapdata\';

      if (UpperCase(MainMapData) <> 'C:\MAPDATA\') then begin
         if (not MDDef.RunOddballLocation )then begin
            if AnswerIsYes('Sure you want main data location at ' + MainMapData +  '  (not recommended)') then begin
               MDDef.RunOddballLocation := true;
            end
            else MainMapData := 'c:\mapdata\';
         end;
      end;
   {$Else}
      MainMapData := System.IOutils.TPath.GetDocumentsPath;
   {$EndIf}
   SetDefaultDirectories;
   {$IfDef RecordProblems} WriteLineToDebugFile('MakeRequiredDirectories out, MainMapData=' + MainMapData); {$EndIf}
   {$IfDef MessageStartupUnit}  MessageToContinue('end demdefs MakeRequiredDirectories, MainMapData=' + MainMapData); {$EndIf}
end;


procedure SetBaseDirectory;
begin
   GetDosPath('Map data drive',MainMapData);
   MakeRequiredDirectories;
end;


procedure SetGeomorphDefaults;
begin
   ProcessIniFile(iniInit,'Geomorph');
end;


{$IfDef ExPLSS}
{$Else}
   procedure SetPLSSDefaults;
   begin
      ProcessIniFile(iniInit,'PLSS');
   end;
{$EndIf}

{$IfDef ExDRGimport}
{$Else}
   procedure SetDRGDefaults;
   begin
      ProcessIniFile(iniInit,'DRG');
   end;
{$EndIf}


{$IfDef ExTiger}
{$Else}

   procedure WriteTigerDefaults;
   var
      Table : tMyData;

         procedure AddTigerType(Caption : shortstring; Draw : boolean; Size : integer; LineWidth : integer; Color : tPlatformColor);
         var
            ch : char;
         begin
            Table.ApplyFilter('NAME=' + QuotedStr(Caption));
            if (Table.RecordCount = 1) then begin
              Table.Edit;
              if Draw then ch := 'Y' else ch := 'N';
              Table.SetFieldByNameAsString('PLOT',ch);
              Table.SetFieldByNameAsInteger('PIXEL_SIZE',Size);
              Table.SetLineColorAndWidth(Color,LineWidth);
              Table.Post;
            end;
         end;

   begin
      with MDdef.TigrDef do begin
         Table := tMyData.Create(TigerShapeRules);
         AddTigerType('railroad',DrawRailroad,5000,RailroadWidth,RailroadColor);
         AddTigerType('stream',DrawStreams,AppearStream,WaterWidth1,WaterColor1);
         AddTigerType('shore',DrawCoastline,AppearCoast,WaterWidth2,WaterColor2);
         AddTigerType('roads7',DrawRoadCat7,AppearRoadCat7,RoadCat7Width,RoadCat7Color);
         AddTigerType('roads6',DrawRoadCat6,AppearRoadCat6,RoadCat6Width,RoadCat6Color);
         AddTigerType('roads5',DrawRoadCat5,AppearRoadCat5,RoadCat5Width,RoadCat5Color);
         AddTigerType('roads4',DrawRoadCat4,AppearRoadCat4,RoadCat4Width,RoadCat4Color);
         AddTigerType('roads2',DrawRoadCat2,AppearRoadCat2,RoadCat2Width,RoadCat2Color);           //2d and 3d level roads
         AddTigerType('roads1',DrawMajorRoad,AppearMajorRoad,MajorRoadWidth,MajorRoadColor);
         AddTigerType('pipeline',DrawPipeline,AppearPipeLine,PipeLineWidth,PipelineColor);
         AddTigerType('powerline',DrawPowerLine,AppearPowerLine,PowerLineWidth,PowerLineColor);
         Table.Destroy;
      end;
   end;

   procedure SetTIGERDefaults;
   begin
      ProcessIniFile(iniInit,'TIGER');
   end;
{$EndIf}


procedure SetGeologyOptions(Allow : boolean);
begin
   {$IfDef VCL}
      (*  //these must now be deliberately enabled, since they have not been tested in a log time and are not used much
      MDdef.ShowStratCol := Allow;
      MDdef.ShowTernary := Allow;
      MDdef.ShowStereoNet := Allow;
      MDdef.ShowSieve := Allow;
      *)
      MDdef.ShowMarineGeology := Allow;
      MDdef.ShowGeomorphometry := Allow;
      MDdef.ShowMarineGeology := Allow;
      MDdef.ShowPlateRotation := Allow;
      MDdef.ShowGeologyOptions := Allow;
   {$EndIf}
end;


procedure HideAllOceanOptions;
begin
   {$IfDef VCL}
       MDDef.ShowOceanModels := false;
       MDDef.ShowSidescan := false;
       MDDef.ShowOceanographyOptions := false;
       MDDef.ShowSubbottom := false;
  {$EndIf}
end;


procedure SetFlatProfile;
begin
   MDdef.CurvAlg := vcNoCurvature;
   MDdef.DrawLOS := false;
   MDDef.LOSVisible := false;
   MDDef.SimpleTopoProfilesOnly := true;
end;


procedure SetExpertOptions(Expert : boolean);
begin
   MDDef.AdvancedDBops := Expert;
   MDDef.MapTicks := tixLatLong;
   MDDef.NEAutoScale := Expert;
   MDDef.ShowCartography := Expert;
   MDDef.ShowConversionAndAnalyze := Expert;
   MDDef.ShowDataProperties := Expert;
   MDDef.ShowDBDateTimeSeries := Expert;
   MDDef.ShowDBonly := Expert;
   MDDef.ShowDEMCompare := Expert;
   MDDef.ShowEnhancementGraphs := Expert;
   MDDef.ShowExperimentalOptions := Expert;
   MDDef.ShowGeomorphometry := Expert;
   MDDef.ShowGlobalDEM := Expert;
   MDDef.ShowIntDB := Expert;
   MDDef.ShowIntervisibility := Expert;
   MDDef.ShowMethodCompare := Expert;
   MDDef.ShowOpenGL := Expert;
   MDDef.ShowOpenImagery := Expert;
   MDDef.ShowPointClouds := Expert;
   MDDef.ShowSHPButton := Expert;
   MDDef.ShowTCPServer := Expert;
   MDDef.ShowTINs := Expert;
   MDDef.ShowViews := Expert;

   {$IfDef ExGazetteer}
   {$Else}
      MDDef.UseGazetteer := Expert;
   {$EndIf}

   {$IfDef ExPLSS}
   {$Else}
      MDDef.ShowPLSS := Expert;
   {$EndIf}
end;


procedure SetEconDefaults;
begin
   {$IfDef VCL}
      SetExpertOptions(false);
      HideAllOceanOptions;
      SetGeologyOptions(false);
      MDDef.ShowClimateAndLight := false;
      MDdef.AutoOpen := aoVector;
  {$EndIf}
end;


procedure SetShipwrecksDefaults;
begin
   {$IfDef VCL}
       HideAllOceanOptions;
       SetGeologyOptions(false);
       SetExpertOptions(false);
       MDDef.ShowVectorMaps := true;
       MDDef.MemoryPointCloudMaxPts := 16000000;
       MDDef.ShowClimateAndLight := true;
       MDDef.ShowOceanModels := true;
       MDDef.ShowSidescan := true;
       MDDef.ShowOceanographyOptions := true;
   {$EndIf}
end;


procedure SetPhysicalGeographyDefaults;
begin
   {$IfDef VCL}
       HideAllOceanOptions;
       SetGeologyOptions(false);
       SetExpertOptions(false);
       SetFlatProfile;
       MDDef.ShowBlueMarble := true;
       MDDef.ShowClimateAndLight := true;
       MDDef.ShowOpenImagery := true;
       MDDef.ShowCartography := true;
       MDDef.KML_DB_tables := false;
   {$EndIf}
end;


procedure SetStructuralGeologyDefaults;
begin
   {$IfDef VCL}
      HideAllOceanOptions;
      SetGeologyOptions(true);
      SetFlatProfile;
      SetExpertOptions(false);
      MDdef.CoordUse := coordLatLong;
      MDdef.MGRSandLatLongWhileRoam := false;
      MDdef.CheckPoint := CheckNothing;
      MDdef.ShowRoamOnAllMaps := true;
      MDDef.MissingDataColor := claWhite;
      MDDef.ShowGlobalDEM := true;
      MDDef.ShowOpenGL := true;
      MDDef.ShowClimateAndLight := false;
      MDDef.ShowDBonly := true;
   {$EndIf}
end;

procedure SetRemoteSensingDefaults;
begin
   {$IfDef VCL}
      HideAllOceanOptions;
      SetGeologyOptions(false);
      SetExpertOptions(false);
      MDDef.ShowClimateAndLight := false;
      MDDef.ShowOpenImagery := true;
      MDDef.ShowPointClouds :=  true;
   {$EndIf}
end;



function CurvAlgName(Alg : tVerticalCurvAlg) : ShortString;
begin
   Result := BaseCurvAlgName[Alg];
   {$IfDef ExFresnel}
   {$Else}
      if Alg = vcRadioLineOfSight then Result := Result + '  k=' + RealToString(MDdef.RadioK,-6,2);
   {$EndIf}
end;

function SlopeMethodName(Method : byte) : shortstring;
begin
   case Method of
      smSteepestNeighbor   : SlopeMethodName := 'Steepest Neighbor';
      smGuthHybrid         : SlopeMethodName := 'Hybrid (Steepest + 8 even)';
      smAverageNeighbor    : SlopeMethodName := 'Average Neighbor';
      smMaxDownHillSlope   : SlopeMethodName := 'Steepest Downhill';
      smFourNeighbors      : SlopeMethodName := '4 neighbors (Zevenbergen and Thorne)';
      smEightNeighborsWeighted  : SlopeMethodName := '8 neighbors (weight) (Horn)';
      smONeillAndMark      : SlopeMethodName := '3 neighbors';
      smEightNeighborsUnweighted  : SlopeMethodName := '8 neighbors (even) (Evans)';
      smEightNeighborsWeightedByDistance : SlopeMethodName := '8 neighbors (dist weight)';
      smFrameFiniteDifference : SlopeMethodName := 'Frame Finite Difference';
      smSimpleDifference : SlopeMethodName := 'Simple Difference';
    end {case};
end;


function ShortSlopeMethodName(Method : byte) : shortstring;
begin
   case Method of
      smSteepestNeighbor   : Result := 'SAN';
      smGuthHybrid         : Result := 'HYB';
      smAverageNeighbor    : Result := 'AVN';
      smMaxDownHillSlope   : Result := 'SDN';
      smFourNeighbors      : Result := 'N4';
      smEightNeighborsWeighted  : Result := 'N8S';
      smONeillAndMark      : Result := 'N3';
      smEightNeighborsUnweighted  : Result := 'N8E';
      smEightNeighborsWeightedByDistance : Result := 'N8R';
      smFrameFiniteDifference : Result := 'FFD';
      smSimpleDifference : Result := 'SD';
   end {case};
end;


function DirectionName(Dir : DEMdefs.tCompassDirection) : shortstring;
begin
   case Dir of
      cdFlat : DirectionName := 'Flat';
      cdPit  : DirectionName := 'Pit';
      cdN    : DirectionName := 'N';
      cdNE   : DirectionName := 'NE';
      cdSE   : DirectionName := 'SE';
      cdS    : DirectionName := 'S';
      cdSW   : DirectionName := 'SW';
      cdW    : DirectionName := 'W';
      cdNW   : DirectionName := 'NW';
      cdE    : DirectionName := 'E';
   end;
end;

function PointTypeName(PointType : tPointType) : ShortString;
begin
   case PointType of
      OtherPoint  : Result := 'Other';
      EdgePoint   : Result := 'Edge';
      FlatPoint   : Result := 'Flat';
      PitPoint    : Result := 'Pit';
      PeakPoint   : Result := 'Peak';
      PassPoint   : Result := 'Pass';
      RidgePoint  : Result := 'Ridge';
      ValleyPoint : Result := 'Valley';
      MissingPoint : Result := 'Missing';
   end {case};
end;


function PointTypeColor(PointType : tPointType) : tColor;
begin
   case PointType of
      OtherPoint  : Result := clWhite;
      EdgePoint   : Result := clWhite;
      MissingPoint : Result := clWhite;
      FlatPoint   : Result := clWhite;
      PitPoint    : Result := RGB(0,255,255);
      PeakPoint   : Result := clLime;
      PassPoint   : Result := clYellow;
      RidgePoint  : Result := clGreen;
      ValleyPoint : Result := clBlue;
   end {case};
end;

procedure SetReflectanceDefaults;
begin
   ProcessIniFile(iniInit,'Reflect');
end;

procedure SetSlopedefaultColors(var NumCats : SmallInt; var SlopeCut : ColorCutArrayType; var SlopeColors : tColorArray);
begin
   NumCats := 15;
   SlopeColors[0] := clBlack;
   SlopeColors[1] := RGB(0,50,0);
   SlopeColors[2] := RGB(0,75,0);
   SlopeColors[3] := RGB(0,100,0);
   SlopeColors[4] := RGB(0,125,0);
   SlopeColors[5] := RGB(0,150,0);
   SlopeColors[6] := RGB(0,200,0);
   SlopeColors[7] := RGB(0,255,0);
   SlopeColors[8] := RGB(125,125,0);
   SlopeColors[9] := RGB(255,255,0);
   SlopeColors[10] := RGB(75,0,0);
   SlopeColors[11] := RGB(125,0,0);
   SlopeColors[12] := RGB(175,0,0);
   SlopeColors[13] := RGB(255,0,0);
   SlopeColors[14] := RGB(255,0,255);

   SlopeCut[0] := 1;
   SlopeCut[1] := 3;
   SlopeCut[2] := 5;
   SlopeCut[3] := 10;
   SlopeCut[4] := 15;
   SlopeCut[5] := 20;
   SlopeCut[6] := 25;
   SlopeCut[7] := 30;
   SlopeCut[8] := 35;
   SlopeCut[9] := 45;
   SlopeCut[10] := 50;
   SlopeCut[11] := 60;
   SlopeCut[12] := 75;
   SlopeCut[13] := 100;
   SlopeCut[14] := 250;
end;


procedure SetFlyDefaults;
begin
   ProcessIniFile(iniInit,'Fly');
end;


procedure SetPerspectiveDefaults(var WorkingPersOpts : tPerspectiveOptions);
begin
   ProcessIniFile(iniInit,'Persp');
   WorkingPersOpts := MDDef.PerspOpts;
end;


procedure SetWeaponsFanDefaults;
begin
   ProcessIniFile(iniInit,'WeaponFan');
end;

procedure ResetStratColDefaults;
begin
   ProcessIniFile(iniInit,'Stratcol');
end;

procedure ResetMicronetDefaults;
begin
   ProcessIniFile(iniInit,'Micronet');
end;


procedure SetContourDefaults;
begin
   ProcessIniFile(iniInit,'Contour');
end;


procedure SetDefaults;
begin
   RecordDirs('Set Defaults in');
   ProcessIniFile(iniInit);
   RecordDirs('Set Defaults out');
   {$IfDef VCL}
      if (WriteSatDir = '') then WriteSatDir := MDTempDir;
      if (WriteDEMDir = '') then WriteDEMDir := MDTempDir;
   {$EndIf}
end;


(*
function MrSidEnabled :boolean;
begin
   Result := FileExists(MrSidDecodeName) and FileExists(MrSidInfoName);
end;
*)

procedure LoadMDdefaults;

         procedure ProcessTheIniFile;
         begin
            if FileExists(IniFileName) then begin
               {$IfDef RecordLoadDefault} WriteLineToDebugFile('old file existed'); {$EndIf}
               ProcessIniFile(iniRead);
            end
            else begin
               {$IfDef RecordLoadDefault} WriteLineToDebugFile('create new INI file'); {$EndIf}
               ProcessIniFile(iniInit);
               {$IfDef MSWindows}
                  ProcessIniFile(iniWrite);
               {$EndIf}
            end;
            {$IfDef RecordLoadDefault} WriteLineToDebugFile('ProcessTheIniFile, ' + MainMapData); {$EndIf}
         end;

         procedure TrySettingDefaultFName(var fName : PathStr; NewValue : PathStr);
         begin
            if (fName <> '') and FileExists(fName) then exit;
            if FileExists(NewValue) then fName := NewValue;
         end;


         procedure TrySettingDefaultDir(var fName : PathStr; NewValue : PathStr);
         begin
            if (PathIsValid(fName)) then exit;
            if PathIsValid(NewValue) then fName := NewValue;
         end;


begin
   {$IfDef MessageStartupUnit}  MessageToContinue('Load DEM defaults in '); {$EndIf}
   {$IfDef RecordProblems} WriteLineToDebugFile('Start LoadMDdefaults ProgramMode=' + IntToStr(ord(MDdef.ProgramOption ))); {$EndIf}
   ProcessTheIniFile;
   {$IfDef RecordDetailedStartup} WriteLineToDebugFile('LoadMDdefaults ini file done'); {$EndIf}
   MakeRequiredDirectories;
   {$IfDef RecordDetailedStartup} WriteLineToDebugFile('LoadMDdefaults MakeRequiredDirectories done'); {$EndIf}
   SetRootDirectoryFiles;
   {$IfDef RecordDetailedStartup} WriteLineToDebugFile('LoadMDdefaults SetRootDirectoryFiles done, MainMapData=' + MainMapData); {$EndIf}
   WGS84DatumConstants.DefineDatumFromUTMZone('WGS84',MDdef.DefaultUTMZone,MDDef.DefaultLatHemi,'WGS84DatumConstants');
   {$IfDef RecordDetailedStartup} WriteLineToDebugFile('LoadMDdefaults WGS84DatumConstants.Define'); {$EndIf}
   InitializeVegColors;

   if (MDDef.MaxElevPercentile - MDDef.MinElevPercentile) < 50 then begin
      MDDef.MinElevPercentile := 1;
      MDdef.MaxElevPercentile := 99;
   end;
   if (MDDef.MaxImagePercentile - MDDef.MinImagePercentile) < 50 then begin
      MDDef.MinImagePercentile := 1;
      MDdef.MaxImagePercentile := 97;
   end;

   {$IfDef VCL}
      DBDir := MainMapData + 'DataBase\';
      {$If Defined(RecordInitialization) or Defined(RecordDetailedStartup)} WriteLineToDebugFile('DBDirectory=' + DBDir); {$EndIf}

      {$IfDef ExGeoStats}
      {$Else}
         TrySettingDefaultDir(GeomorphAtlasDir,MainMapData + 'srtm_params_grids\');
      {$EndIf}

      if (VectorMapName = '') then VectorMapName := ProgramRootDir + 'worldmre.prj';
      TrySettingDefaultFName(ETOPODEMName,MainMapData + 'Etopo1\etopo5.dem');
      TrySettingDefaultFName(ETOPODEMName,MainMapData + 'Etopo1\etopo1.dem');

      TrySettingDefaultfName(SmallScaleWorldOutlines,DBDir + 'Natural_Earth_Vector\NE_110M_VECTORS' + DefaultDBExt);
      TrySettingDefaultfName(MedScaleWorldOutlines,DBDir + 'Natural_Earth_Vector\NE_50M_VECTORS' + DefaultDBExt);
      TrySettingDefaultfName(LargeScaleWorldOutlines,DBDir + 'Natural_Earth_Vector\NE_10M_VECTORS' + DefaultDBExt);

      {$IfDef RecordNaturalEarthFileNames}
         WriteLineToDebugFile('After TrySettingDefaultfName');
         WriteLineToDebugFile('   small: ' + SmallScaleWorldOutlines);
         WriteLineToDebugFile('  medium: ' + MedScaleWorldOutlines);
         WriteLineToDebugFile('   large: ' + LargeScaleWorldOutlines);
      {$EndIf}

      TrySettingDefaultFName(StateGISFileName,DBDir + 'us\states\STATESP020' + DefaultDBExt);
      TrySettingDefaultFName(CountyGISFileName,DBDir + 'us\counties\co99_d00' + DefaultDBExt);
      TrySettingDefaultFName(CountyGISFileName,DBDir + 'us\counties\COUNTYP020' + DefaultDBExt);
      TrySettingDefaultFName(HighwayGISFileName,DBDir + 'us\roads\roadtrl020' + DefaultDBExt);
      TrySettingDefaultFName(RiversGISFileName,DBDir + 'us\stream_waterbodies\hydrogl020' + DefaultDBExt);
   {$EndIf}

   {$IfDef MSWindows}
      //TrySettingDefaultFName(MrSidInfoName,ProgramRootDir + 'mrsid\bin\mrsidgeoinfo.exe');
      //TrySettingDefaultFName(MrSIDDecodeName,ProgramRootDir + 'mrsid\bin\mrsidgeodecode.exe');
      //TrySettingDefaultDir(TauDEMDir,ProgramRootDir + 'taudem\');
      TrySettingDefaultDir(GADMDir,MainMapData + 'GADM\');
      if (not PathIsValid(SaveViewshedDir)) then SaveViewshedDir := MDTempDir;
      if not FileExists(LastDesktop) then begin
        {$IfDef RecordProjects} WriteLineToDebugFile('LoadMDdefaults, cannot find LastDESKtop=' + LastDesktop); {$EndIf}
         LastDesktop := ProjectDir;
      end;
      TrySettingDefaultDir(MainMapData,ProgramRootDir[1] + ':\mapdata\');
      if (MainMapData = '') then GetDOSPath('Map data',MainMapData);
   {$EndIf}

   {$If Defined(RecordInitialization) or Defined(RecordDetailedStartup)} WriteLineToDebugFile('VCL/Windows options over'); {$EndIf}

   {$IfDef Android}
      {$IfDef RecordInitialization} WriteLineToDebugFile('Start android'); {$EndIf}
      SaveViewshedDir := TPath.GetDocumentsPath;
      {$IfDef RecordInitialization} WriteLineToDebugFile('SaveViewshedDir=' + SaveViewshedDir); {$EndIf}
   {$EndIf}

   {$IfDef ExStratcol}
   {$Else}
      TrySettingDefaultFName(LithFileName,ProgramRootDir + 'DSDP.NAM');
   {$EndIf}

   {$IfDef RecordProblems} WriteLineToDebugFile('End LoadMDdefaults ProgramMode=' + IntToStr(ord(MDdef.ProgramOption ))); {$EndIf}
   {$IfDef RecordIniMemoryOverwrite} IniMemOverwriteCheck('loadMDdefaults out'); {$EndIf}
   {$IfDef MessageStartupUnit} MessageToContinue('LoadMDdefaults out'); {$EndIf}
end {proc LoadMDdefaults};


procedure SaveMDdefaults;
begin
   {$IfDef RecordProgramMode} WriteLineToDebugFile('SaveMDdefaults ProgramMode=' + IntToStr(ord(MDdef.ProgramOption ))); {$EndIf}
   {$IfDef RecordProjects} WriteLineToDebugFile('SaveMDdefaults LastDESKtop=' + LastDesktop); {$EndIf}
   {$IfDef FMX}
      SysUtils.DeleteFile(IniFileName);
   {$EndIf}
   {$IfDef RecordIniMemoryOverwrite} IniMemOverwriteCheck('SaveMDdefaults'); {$EndIf}
   ProcessIniFile(iniWrite);
end;


initialization
   {$IfDef MessageStartup} MessageToContinue('start demdefs initialization'); {$EndIf}
   ShowDEMReadingProgress := true;
   ShowSatProgress := true;
   BackupMDDef := Nil;

   AspColorNE := RGBtrip(0,171,168);
   AspColorE := RGBtrip(0,104,192);
   AspColorSE := RGBtrip(108,0,163);
   AspColorS := RGBtrip(202,0,156);
   AspColorSW := RGBtrip(255,85,104);
   AspColorW := RGBtrip(255,171,71);
   AspColorNW := RGBtrip(244,250,0);
   AspColorN := RGBtrip(132,214,0);
finalization
   {$IfDef RecordFan} WriteLineToDebugFile('RecordFanProblems in demdef_routines'); {$EndIf}
   {$IfDef RecordDefaultColors} WriteLineToDebugFile('RecordDefaultColors in demdef_routines'); {$EndIf}
   {$IfDef MessageStartupUnit} WriteLineToDebugFile('MessageStartupUnitProblems in demdef_routines'); {$EndIf}
   {$IfDef RecordDefault} WriteLineToDebugFile('RecordDefaultProblems in demdef_routines'); {$EndIf}
   {$IfDef Options} WriteLineToDebugFile('Options in demdef_routines'); {$EndIf}
   {$IfDef RecordFanConversions} WriteLineToDebugFile('RecordFanConversions in demdef_routines'); {$EndIf}
   {$IfDef RecordTerrainCategories} WriteLineToDebugFile('RecordTerrainCategories in demdef_routines'); {$EndIf}
   {$IfDef RecordPath} WriteLineToDebugFile('RecordPathProblems in demdef_routines'); {$EndIf}
   {$IfDef RecordFan} WriteLineToDebugFile('RecordFanProblems in demdef_routines'); {$EndIf}
   {$IfDef RecordGazOps} WriteLineToDebugFile('RecordGazOps in demdef_routines'); {$EndIf}
   {$IfDef ShowMenuLimits} WriteLineToDebugFile('ShowMenuLimits in demdef_routines'); {$EndIf}
   {$IfDef RecordUpdate} WriteLineToDebugFile('RecordUpdateProblems in demdef_routines'); {$EndIf}
   {$IfDef RecordINIfiles} WriteLineToDebugFile('RecordINIfiles in demdef_routines'); {$EndIf}
   {$IfDef RecordLoadDefault} WriteLineToDebugFile('RecordLoadDefaultProblems in demdef_routines'); {$EndIf}
   {$IfDef RecordWebDownloads} WriteLineToDebugFile('RecordWebDownloads in demdef_routines'); {$EndIf}
   {$IfDef RecordParallelLoops} WriteLineToDebugFile('RecordParallelLoops in demdef_routines'); {$EndIf}
   {$IfDef RecordFont} WriteLineToDebugFile('RecordFont in demdef_routines'); {$EndIf}
end.




