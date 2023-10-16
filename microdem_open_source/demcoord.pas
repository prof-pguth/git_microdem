{$F+}

unit DEMCoord;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2023 Peter L. Guth  }
{___________________________________}


{$I nevadia_defines.inc}


{$IFDEF DEBUG}
   //{$Define NoInline}
   //{$Define NoParallelFor}
{$ELSE}
   //{$Define NoInline}
   //{$Define NoParallelFor}
{$ENDIF}

{$IfDef VCL}
   //{$Define IncludeBILWrite}
{$EndIf}


{$IfDef RecordProblems} //normally only defined for debugging specific problems

   {$IFDEF DEBUG}
      {$Define RecordDEMIX}
      //{$Define RecordMapType}
      //{$Define TimePointParameters}
      {$Define RecordDEMIXResample}
      //{$Define RecordsfBoundBox2tGridLimits}     //use with care; trashes the debug file
      //{$Define TrackHorizontalDatum}
      //{$Define RecordVertDatumShift}
      //{$Define TrackPixelIs}
      //{$Define TrackDEMCorners}
      //{$Define RecordVAT}
      //{$Define UKOS}
      //{$Define RecordHalfPixelShift}
      //{$Define RecordDEMEdits}
      //{$Define RecordClone}
      //{$Define RecordGeotiff}
      //{$Define ShortDEMLoad}
      //{$Define RecordSaveAverageResampleDEMformat}
      //{$Define RecordDEMCreation}
      //{$Define RecordSSO}
      //{$Define RecordDEMDigitizeDatum}
      //{$Define TimeLoadDEM}
      //{$Define RecordZ2ndDEM}
      //{$Define RecordDEMClose}
      //{$Define RecordNormalInit}
      //{$Define RecordMinMax}
      //{$Define RecordExtremeZ}
      //{$Define RecordUKOS}
      //{$Define GeotiffCorner}
      //{$Define RecordHorizon}
      //{$Define RecordZRange}
      //{$Define RecordDEMMemoryAllocations}
      //{$Define RecordFilter}
      //{$Define RecordReadDEM}
      //{$Define RecordReadMDDEM}
      //{$Define RecordLatSpacingValues}
      //{$Define RecordMultipleRefDirs}
      //{$Define ShowDEMSSOCalc}
      //{$Define RecordVariogram}
      //{$Define RecordElevPercentiles}
      //{$Define ShowFullDEMSSOCalc}  //big slowdown
      //{$Define RecordNLCD}
      //{$Define RecordMoments}
      //{$Define RecordSetup}
      //{$Define RecordCreateNewDEM}
      //{$Define RecordMapDraw}
      //{$Define RecordProjectionParameters}
      //{$Define GeotiffSave}
      //{$Define TimeLoadDEM}
      //{$Define RecordDefineDatum}
      //{$Define RecordDEMEdits}
      //{$Define RecordGridIdentical}
      //{$Define RecordGetGridLimits}
      //{$Define RecordHiResDEM}
      //{$Define TriPrismErrors}
      //{$Define TriPrismResults}
      //{$Define RecordLOS}
      //{$Define RecordLOSAlgorithm}
      //{$Define RecordConversion}
      //{$Define RecordCloseDEM}
      //{$Define RecordClosing}
      //{$Define RecordResample}
      //{$Define RecordWriteDEM}
      //{$Define RecordDTEDHeader}
      //{$Define RecordDEMMapProjection}
      //{$Define RecordASCIIExport}
      //{$Define RecordDEMHeader}
      //{$Define RecordFullWriteDEM}
      //{$Define RecordOpenMap}
      //{$Define RecordWorldFile}
      //{$Define RecordCloseDEM}
      //{$Define RecordGDAL}
      //{$Define RecordMerge}
      //{$Define RecordImport}
      //{$Define RecordShortDefineDatum}
      //{$Define TrackLastDEMName}
      //{$Define RecordReadMakeSingle}
      //{$Define RecordReadDEMVeryFull}     //real slowdown
      //{$Define RecordAllImport}           //real slowdown
      //{$Define RecordGetVisiblePoints}    //slowdown
      //{$Define RecordGetUTMStraightRoute} //significant slowdown
      //{$Define RecordGetStraightRoute}    //significant slowdown
      //{$Define RecordDetailedResample}    //severe slowdown
      //{$Define RecordFullStraightRoute}   //severe slowdown if called repeatedly
      //{$Define RecordPointClass}          //severe slowdown if called repeatedly
      //{$Define RecordDetailedHorizon}     //severe slowdown if called repeatedly
      //{$Define RecordReadDEMFull}         //significant slowdown for some types of DEM
   {$ELSE}
   {$ENDIF}
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
//end inline core DB

   {$IfDef VCL}
      Graphics,Forms,StdCtrls,ComCtrls,Controls,
   {$EndIf}

    {$IfDef MSWindows}
       Windows,
    {$EndIf}

   {$IfDef VCL}
      DEMMapF,
      BaseGraf,
   {$EndIf}

   {$IfDef ExVegDensity}
   {$Else}
      Veg_Density,
   {$EndIf}

   {$IfDef ExGeostats}
   {$Else}
      NetMainW,
   {$EndIf}

   {$IfDef ExNLCD}
   {$Else}
      DEM_NLCD,
   {$EndIf}

   System.Diagnostics,System.Threading,System.SyncObjs,System.UITypes,
   Classes,SysUtils,Math,StrUtils,System.IOUtils,

   Petmar_types,PETMAR,PETMath,BaseMap,
   DEMdefs;


{$Define CarlosErrorTrap}

{$IfDef CarlosErrorTrap}
const
   CarlosXRecord : integer = 600;
   CarlosYRecord : integer = 400;
{$EndIf}



type
   tNormal  = array[0..MaxElevArraySize] of VectorType32;  {runs S->N}
   tNormalPointer = ^tNormal;
   tNormals = array[0..MaxColsInRAM] of tNormalPointer;
   tNormalsPointer = ^tNormals;

   tShortFloatCol = array[0..MaxElevArraySize] of float32;
   pShortFloatCol = ^tShortFloatCol;
   tShortFloatElevations = array[0..MaxColsInRAM] of pShortFloatCol;
   pShortFloatElevations = ^tShortFloatElevations;

   tByteCol = array[0..MaxElevArraySize] of Byte;
   pByteCol = ^tByteCol;
   tByteElevations = array[0..MaxColsInRAM] of pByteCol;
   pByteElevations = ^tByteElevations;

   tSmallIntCol = array[0..MaxElevArraySize] of Int16;
   pSmallIntCol = ^tSmallIntCol;
   tSmallIntElevations = array[0..MaxColsInRAM] of pSmallIntCol;
   pSmallIntElevations = ^tSmallIntElevations;

   tWordCol = array[0..MaxElevArraySize] of Word;
   pWordCol = ^tWordCol;
   tWordElevations = array[0..MaxColsInRAM] of pWordCol;
   pWordElevations = ^tWordElevations;

   tLongWordCol = array[0..MaxElevArraySize] of LongWord;
   pLongWordCol = ^tLongWordCol;
   tLongWordElevations = array[0..MaxColsInRAM] of pLongWordCol;
   pLongWordElevations = ^tLongWordElevations;

   tDEMPointClassFloatArray = array[tPointType] of float64;
   tDEMPointClassIntArray = array[tPointType] of int32;
   tElevBoxArray = array[-1..1,-1..1] of float32;

   tSSOvars = record
      AvgElev,AvgSlope,MaxSlope,s1s2,s2s3,Shape,Strength,StdDevSlope,StdDevElev,ElevRange,QueensAspect,RoughnessFactor,AvgAspect,AspectStrength  : float64;
      x1sq,y1sq,z1sq : float64;
      TheDips,TheDipDirs : VectorType;
      NumPts,NumMissing,AspPts : Integer;
      V : tTrendVector;
   end;

   tAspectStats = object
      protected
         DEM : integer;
         AvgAspectDir,AvgAspectMag,
         AspX,AspY : float64;
      private
      public
         NPts : integer;
         AspectFreqValsTrue,AspectFreqValsGrid : CircleFreqType;
         constructor Create(theDEM : integer);
         destructor Destroy;
         function Report : shortstring;
         function CreateRose(BaseLegend : shortstring = '') : tThisBaseGraph;
         function QueensAspect : float64;
         procedure AddPoint(SlopeAspectRec : tSlopeAspectRec);
         procedure AddAspect(Aspect : float32);
         procedure VectorAverage;
         procedure FillFromGrid(GridLimits: tGridLimits);
   end;

   function CreateAspectRose(DEM : integer) : tThisBaseGraph;

type
   tSSOVarAnalysis = object
      protected
      private
      public
         DEM  : Integer;
         M,S  : ^tTrendMatrix;
         s2   : VectorType;
         SSOvars : tSSOvars;
         constructor Create(WhichDEM : integer);
         destructor Destroy;
         procedure NormalsInBox(GridLimits : tGridLimits);
         function ComputeEigenVectors : boolean;
   end;

type
   tDEMDataSet = class
      protected
      private
         ByteElevations        : pByteElevations;
         WordElevations        : pWordElevations;
         SmallIntElevations    : pSmallIntElevations;
         ShortFloatElevations  : pShortFloatElevations;
         LongWordElevations    : pLongWordElevations;
         Normals               : tNormalsPointer;
         RefMinElev,RefMaxElev : float64;
         DEMDatumShiftDone     : boolean;
         BytesPerElevation : int32;
         BytesPerColumn : int32;
         function InterpolateBiCubicVisioTerra(xgrid,ygrid : float32; var z : float32) : boolean;
         function InterpolateBiCubicNumericalRecipes(xgrid,ygrid : float32; var z : float32) : boolean;
         function CheckForUTMZones : boolean;
         procedure FilterStrip(NewDEM, FilterLap : integer; GridLimits : tGridLimits; FilterCategory : tFilterCat; Filter : FilterType);
      public
         ThisDEM   : integer;  //to access the global array
         AreaName       : ShortString;
         ShortName      : string10;
         DEMstatus      : tDEMstatus;
         DEMheader      : tDEMheader;
         DEMMetadata    : tStringList;
         DEMMapProjection  : BaseMap.tMapProjection;
         DEMFileName,
         VATFileName       : PathStr;
         GeotiffImageDesc  : shortstring;
         Zpercens           : ^floatarray1000;

         LatSizeMap,               {size of area in latitude, in degrees}
         LongSizeMap,              {size of area in longitude, in degrees}
         DEMSWcornerLat,           {local datum lat of lower left corner,  the point for pixel-is-point and SW corner for pixel-is-area}
         DEMSWcornerLong,          {local datum long of lower left corner, the point for pixel-is-point and SW corner for pixel-is-area}
         ComputeSWCornerX,         //  1/2 pixel shifted east for PixelIsArea used for computations which consider all grids pixel-is-point
         ComputeSWCornerY,         //  1/2 pixel shifted north for PixelIsArea used for computations which consider all grids pixel-is-point
         GeotiffNWCornerX,
         GeotiffNWCornerY,
         AverageGridTrue,
         AverageDiaSpace,
         AverageSpace,                   {average spacing in meters of average x and y spacing, for arc second grids}
         AverageXSpace,                  {average spacing in meters in x direction}
         AverageYSpace      : float64;   {average spacing in meters in y direction}
         DiagSpaceByDEMrow,
         XSpaceByDEMrow     : ^tShortFloatCol;
         VATrelatedGrid,
         FilterGrid,
         FilterGridValue,
         FanBlowUpDEM,
         DSMGrid            : integer;
         ThisDEMMissingValue : LongWord;
         Z_Mean,Z_Std : float32;
         ElevationMultiple,
         Over30PercentSlope,
         Over50PercentSlope   : float64;
         DEMAlreadyDefined,
         ElevationDEM,
         DEMMemoryAlreadyAllocated,
         HiddenGrid,
         UTMValidDEM        : boolean;
         cosAlt,sinAlt,
         RefPhi,RefAlt,RefWeight : array[1..MaxRefDir] of float64;
         VatLegendStrings : tStringList;

         {$IfDef VCL}
            SelectionMap       : TMapForm;
         {$EndIf}

         {$IfDef ExVegDensity}
         {$Else}
            VegGrid           : array[1..2] of integer;
            VegDensityLayers  : array[1..2] of tVegDensity;
         {$EndIf}

         {$IfDef ExNLCD}
         {$Else}
            NLCDCats : ^tNLCDCats;
         {$EndIf}

         constructor Create(WhichDEM : integer);
         destructor Destroy(CloseMap : boolean = true);

         function LandCoverGrid : boolean;
         function ElevationGrid : boolean;
         function ReadDEMNow(var tFile : PathStr; transformtoNewDatum : boolean) : boolean;
         function ReloadDEM(transformtoNewDatum : boolean) : boolean;
         procedure SetNewDEM(var NewDEM : integer);
         procedure FreeDEMMemory;

         function TheShortDEMName : ShortString;
         function SaveStatusString : shortstring;

         function PercentileOfPoint(xloc,yloc : integer; GridLimits: tGridLimits) : float64;
         function PercentileOfElevation(z : float64) : float64;
         function FindPercentileElevation(Percentile : float64) : float64;
         procedure GetElevPercentiles(GridLimits: tGridLimits);
         procedure CloseElevPercentiles;

         function ValidElevsInDEM : integer;
         procedure GetDEMLimits(var bLat,bLong : integer; var Lats,Longs : tFourFloats);
         function DEMBoundBoxGeo : sfBoundBox;
         function DEMBoundBoxProjected : sfBoundBox;
         function DEMBoundBoxDataGrid : sfBoundBox;
         function PixelBoundBoxGeo(Col,Row : integer) : sfBoundBox;
         function PixelBoundBoxUTM(Col,Row : integer) : sfBoundBox;
         function bbDEMGridPartOfDEMonMap(BaseMap : tMapForm) : sfBoundBox;
         function sfBoundBox2tGridLimits(sfBoundBox : sfboundBox) :tGridLimits;

         function PixelCenterOnFullSecond : boolean;

         function GeotiffDEMName : PathStr;

         procedure FindEdgeThisDEM(var NewDEM: integer; Dir : tCompassDirection);

         function MissingElevation(z : float32) : boolean;  inline;
         function MissingCol(Col : integer) : boolean;
         function MissingRow(Row : integer) : boolean;
         function MissingDataInGrid(XGrid,YGrid : integer) :  boolean;
         function ValidNeighborsInBox(XGrid,YGrid,Size : integer) :  integer;
         function ImmediateNeighborsMissing(XGrid,YGrid : integer) :  integer;
         function ImmediateNeighborsSameCode(XGrid,YGrid : integer) :  integer;
         procedure DeleteMissingDataPoints(CheckMaxMin : boolean = true);
         procedure ComputeMissingData(var Missing : float64); overload;
         procedure ComputeMissingData(GridLimits : tGridLimits; var Missing : float64); overload;

         function FilledGridBox(var GridLimits : tGridLimits) : boolean;
         function SecondGridIdentical(Map2 : integer) : boolean;
         function GetSamplingSize(GridLimits: tGridLimits)  : integer;

         procedure DEMCenterPoint(var Lat,Long : float64);

         procedure UTMtoLatLongDegree(XUTM,YUTM : float64; var Lat,Long : float64);
         procedure UTMToDEMGrid(XUTM,YUTM : float64;  var XGrid,YGrid : float32; var InBounds : boolean);
         procedure DEMGridtoUTM(XGrid,YGrid : float32; var XUTM,YUTM : float64);
         procedure DEMGridToLatLongDegree(XGrid,YGrid : float64; var Lat,Long : float64);

         procedure LatLongDegreetoUTM(Lat,Long : float64; var XUTM,YUTM : float64);
         function LatLongDegreeToDEMGrid(Lat,Long : float64; var XGrid,YGrid : float32) : boolean; overload; {$IfDef NoInLine} {$Else} inline; {$EndIf}
         function LatLongDegreeToDEMGrid(Lat,Long : float64; var XGrid,YGrid : float64) : boolean; overload; {$IfDef NoInLine} {$Else} inline; {$EndIf}

         function LatLongDegreeToDEMGridInteger(Lat,Long : float64; var XGrid,YGrid : int32) : boolean; {$IfDef NoInLine} {$Else} inline; {$EndIf}

         procedure ClipDEMGrid(var x,y : float64);  overload;
         procedure ClipDEMGrid(var x,y : float32);  overload;
         procedure ClipDEMGrid(var x,y : int32);  overload;

         function GridInDataSet(XGrid,YGrid : int32) : boolean; overload; {$IfDef NoInLine} {$Else} inline; {$EndIf}
         function GridInDataSet(XGrid,YGrid : float64) : boolean; overload; {$IfDef NoInLine} {$Else} inline; {$EndIf}
         function LatLongDegreeInDEM(Lat,Long : float64) : boolean;
         function LatLongNearestGridPointInDEM(Lat,Long : float64) : boolean;

         function GetElevMeters(XGrid,YGrid : float64; var z  : float32) : boolean;  overload; {$IfDef NoInLine} {$Else} inline; {$EndIf}   {interpolates from grid coordinates in DEM to return elevation of a point}
         function GetElevMetersOnGrid(x,y : int32; var z  : float32) : boolean;  overload; {$IfDef NoInLine} {$Else} inline; {$EndIf}       {grid coordinates in DEM to return elevation of a point}
         function GetElevFromUTM(x,y : float64; var z : float32) : boolean;
         function GetElevFromLatLongDegree(Lat,Long : float64; var z : float32) : boolean;

         function GetElevMetersFromSecondDEM(IdenticalGrids : boolean; Dem2,Col,Row : integer; var z  : float32) : boolean;
         function GetElevMetersFromThisAndSecondDEM(Dem2,Col,Row : integer; var z1,z2  : float32) : boolean;
         function GetSlopeAspectFromSecondDEM(Dem2 : integer; Col,Row : int32; var SlopeAspectRec : tSlopeAspectRec) : boolean;

         function GetElevSquareMeters(XGrid,YGrid : float64; var Elev : tElevFloatArray) : boolean; inline;
         function GetElev3x3Meters(XGrid,YGrid : int32; var Elev : tElevBoxArray) : boolean;  inline;
         function GetNineElevMeters(Col,Row : int32; var znw,zw,zsw,zn,z,zs,zne,ze,zse : float32) : boolean; inline;

         function MinElevAroundPoint(xgrid,ygrid : int32; var z : float32) : boolean;
         function MaxElevAroundPoint(xgrid,ygrid : int32; var z : float32) : boolean;
         procedure GetElevationsInLongArray(GridLimits: tGridLimits; var NPts : int64; var Values : Petmath.bfarray32; IncludeSeaLevel : boolean = true);

         procedure MoveToEGM2008(AddLocalVDatum,SubLocalVDatum : integer);

         function Geo_Z_Factor : float32;

         function ZinMeters(z : float64) : float64;
         procedure GetElevCol(XGrid: int32; var z : pSmallIntCol); {returns entire column of elevations, needed to pass to contouring routine}
         procedure GetFloatElevCol(XGrid: int32; var z : pShortFloatCol); {returns an entire column of elevations, needed to pass to contouring routine}

         procedure LocationOfMaximumZAroundPoint(var Lat,Long : float64; var MaxZ : float32; BoxSize : integer);

         procedure BoxAreaExtremeElevations(Limits : tGridLimits; var TheMinElev,TheMaxElev,AvgElev : float32);
         procedure AreaExtremeElevationsFromLatLong(LatLow,LongLow,LatHi,LongHigh : float64; var TheMinElev,TheMaxElev : float32);
         procedure SimpleBoxAroundPoint(Col,Row,BoxSize : integer; var x1,y1,x2,y2 : integer);
         function BoxAroundPointFromFullSizeMeters(Col,Row,BoxSize : integer) : tGridLimits;

         function SetGridElevationLatLongDegree(Lat,Long : float64; z : float64) : boolean;
         function SetGridElevation(Col,Row : int32; z : float64) : boolean;
         procedure IncrementGridValue(Col,Row : int32);
         procedure SetGridMissing(Col,Row : int32); overload;
         procedure SetGridMissing(x,y : float64); overload;
         procedure SetEntireGridMissing;
         procedure SetEntireGridToConstant(z : float64);
         procedure ReclassifyRange(MinRange, MaxRange, NewZ : float64);
         procedure ShiftGlobalDEM(NewLeftLong : float64);

         procedure CheckUK_OS;
         function RGBfromLongWord(x,y : integer; var r,g,b : byte) : boolean;
         procedure RoundToByteRange;
         function ScaleZtoByte(z : float64) : byte;

         procedure PixelSpacingAndRotation(Col,Row : integer; var Lat,Long : float64; var xdistance,ydistance,GridTrueAngle : float32; Quick : boolean = true);
         procedure GridSpacingDetails(Region : sfBoundBox; var AverageX,AverageY,AverageSpacing,AverageGridTrue : float64);
         function PixelSize(Col,Row : integer) : shortstring;

         procedure CheckMaxMinElev;

         function DEMLocationString(XGrid,YGrid : float64) : ShortString;
         function DistanceMetersBetweenPoints(xg1,yg1,xgrid,ygrid : float64; var Heading : float64) : float64;      {distance in meters, heading in degrees}

         function SeaLevelCell(x,y : integer) : boolean;
         function LakePoint(X,Y : integer) : boolean;
         function IsSurroundedPoint(Col,Row : integer) : boolean;
         function SurroundedPointElevs(Col,Row : integer; var znw,zw,zsw,zn,z,zs,zne,ze,zse : float32; RegionSize : integer = 1) : boolean; {determines point can be safely interpolated, surrounded by valid data points and not missing values}

         procedure SetUpMap(DEMNumber : integer; CheckElevs : boolean; inMapType : tMapType = mtDEMBlank; UsePC : boolean = true);

         {$IfDef VCL}
            procedure CreateVATforDEM(OptionToCopy : boolean = true);
            function LoadColorVATTable(var Colors : tVATColors) : boolean;
            procedure VATLegend;
            procedure VerifyDefaultPosition(Mess : AnsiString; var xg,yg : float32; var XUTM,YUTM : float64);
            function ExportASCII(x1,y1,x2,y2 : integer) : PathStr;
            procedure SaveAsGeoJSON;
            procedure ExpandSpecifiedZRange;
            procedure RoundZRangeByPercentileToByte;
            procedure GetRectangleFromDEMGrid(var GridLimits : tGridLimits);
            {$IfDef ExNLCD}
            {$Else}
               procedure CheckForLandCover;
            {$EndIf}
         {$EndIf}

         {$IfDef ExDEMReports}
         {$Else}
            procedure SlopeMethodsReport(xp,yp : integer; Title : shortstring = '');
            procedure SlopeMethodsReportFromLatLong(lat,Long : float64; Title : shortstring = '');
            procedure PointParameters(xgrid,ygrid : float64);
         {$EndIf}

         {$IfDef NoMapOptions}
         {$Else}
            function ValuesInRange(Min,Max : float64; ShowOnMap : boolean; Map : tMapForm) : integer;
         {$EndIf}

         {$IfDef ExGraphs}
         {$Else}
            procedure DrawCrossTrackProfile(var Graf : tThisBaseGraph; inLat,inLong,Azimuth,Distance,Spacing : float64);
         {$EndIf}

         procedure SetRasterPixelIsGeoKey1025(DoHalfPixelShift : boolean);

         function LatLongDegreePointsIntervisible(Lat1,Long1,ObsUp,Lat2,Long2,TargetUp : float64; var Distance,BlockDistance : float64) : boolean;
         function GridPointsIntervisible(xg1,yg1,ObsUp,xg2,yg2,TargetUp : float64; var Distance,BlockDistance : float64) : boolean;
         procedure MultiplyGridByConstant(Aconst : float64);
         procedure AddConstantToGrid(Aconst : float64);

         function MissingColorRGBTriple(x,y : float64) : tPlatformColor;
         procedure GetDEMPointsSum(var NPts : integer; var Sum : float64);

      //reflectance/hillshade operations
         function ReflectanceColor(TintedReflectance : tMapType; x,y : integer) : tcolor;
         function ReflectanceValue(x,y : integer) : integer;
         function RGBReflectanceColor(TintedReflectance : tMapType; Col,Row : integer) : tPlatformColor;
         function InterpolateReflectanceValue(x,y :  float64) : integer;
         procedure ReflectanceParams(Min : float64 = -9999; Max : float64 = -9999);

     //slope and aspect
         function GetSlopeAndAspect(Col,Row : integer; var SlopeAsp : tSlopeAspectRec) : boolean;
         function GetSlopeAndAspectFromLatLong(Lat,Long : float64; var SlopeAspectRec : tSlopeAspectRec) : boolean;
         function SlopePercent(XGrid,YGrid : integer) : float64;
         function SlopePercentFromLatLong(Lat,Long : float64) : float64;
         procedure RichardsonExtrapolationSlopeMaps(Save : boolean = false);

         function RoughnessFromSlopeSTD(x,y,Radius : integer; var Roughness : float32) : boolean;

         function FigureOpenness(Col,Row,RegionSizeMeters : integer; var  Upward,Downward : float64; Findings : tStringList = Nil) : boolean;

         function AllocateDEMMemory(InitDEM : byte; InitVal : float64 = 0) : boolean;

         procedure ResetPrimaryDatumZone(NewLong : float64);

         function ReinterpolateLatLongDEM(var SpacingArcSec : float32; fName : PathStr = '') : integer;
         function ReinterpolateUTMDEM(FloatSpacingMeters : float64; UTMzone : int16 = -99; fName : PathStr = '') : integer; //AddCaption : shortstring = '');
         function ResampleByAveraging(OpenMap : boolean; SaveCountGrid : boolean = true; SaveName : PathStr = '') : integer;

         procedure WriteNewFormatDEM(var FileName : PathStr; WhatFor : shortstring = '');  overload;
         procedure WriteNewFormatDEM(Limits : tGridLimits; var FileName : PathStr; WhatFor : shortstring = '');  overload;
         procedure SavePartOfDEMWithData(var FileName : PathStr);
         procedure SaveSpecifiedPartOfDEM(var FileName : PathStr; Limits : tGridLimits);
         procedure CSVforVDatum(Delta : float64 = -99;fName : PathStr = '');

         {$IfDef ExGeotiffWrite}
         {$Else}
            procedure SaveAsGeotiff(SaveName : PathStr = '');
            procedure SaveGridSubsetGeotiff(DEMGridLimits : tGridLimits; fName : PathStr = '');
         {$EndIf}

         {$IfDef AllowOddballDEMexports}
            function VRMLFile(GridPts : integer; xu1,yu1,GridSize : float64; TextureName : shortString; WalkAboutFile : tStringList = Nil; NumTextures : integer = 1; TextureInVRML : boolean = true) : tStringList;
            procedure SaveAsUSGSASCII(SaveName : PathStr = '');
            procedure SaveAsArcGridASCII(SaveName : PathStr = '');
            procedure Save16BitBSQ;
            procedure SavePGM(Bit16 : boolean);

            {$IfDef IncludeBILWrite}
               procedure WriteGridFloatFormatDEM(var FileName : PathStr);
               procedure WriteBILFormatDEM(var FileName : PathStr);
            {$EndIf}
         {$EndIf}

         {$IfDef ExDTED}
         {$Else}
            procedure SaveAsDTED(OutLatInterval,OutLongInterval : integer; OutName : PathStr = ''; ShowProgress : boolean = false);
         {$EndIf}

         function CloneAndOpenGridSetMissing(NewPrecision : tDEMprecision; Gridname : shortstring; ElevUnits : tElevUnit) : integer;
         function ThinAndOpenGridSetMissing(ThinFactor : integer; NewPrecision : tDEMprecision; Gridname : shortstring; ElevUnits : tElevUnit) : integer;

         function ThinThisDEM(fName : PathStr = ''; ThinFactor : integer = 0; DoItByAveraging : boolean = false; Offset : integer = 0) : integer;
         function HalfPixelAggregation(fName : PathStr; PixelIs : byte; SaveFile : boolean; Offset : integer = 0) : integer;

         procedure FilterThisDEM(FilterCategory : tFilterCat; var NewDEM : integer; BoxSize : integer = 0; FilterName : PathStr = '');
         procedure RGBFilterDEM(BufferSize : integer; JustDoHoles : boolean);

         function DetrendDEM(Normalize : boolean = true; FilterRadius : integer = 2) : integer;
         function BoxcarDetrendDEM(OpenMap : boolean; GridLimits : tGridLimits; FilterRadius : integer = 2) : integer;
         function ResaveNewResolution(FilterCategory : tFilterCat) : integer;

         function RectangleSubsetDEM(GridLimits : tGridLimits; FileName : PathStr = '') : PathStr; overload;
         function RectangleSubsetDEM(GridLimits : tGridLimits; OpenMaps : boolean) : integer; overload;
         function RectangleSubsetDEMInMemory(GridLimits : tGridLimits) : tDEMDataSet;
         procedure CutOutGeoBox(bb : sfBoundBox);

         function KeyDEMParams(short : boolean = false) : ShortString;
         function FullDEMParams : AnsiString;
         function SWcornerString : ShortString;
         function PixelIsString : AnsiString;
         function DEMMapProjectionString : shortstring;
         function DEMHorizontalSpacingSummary  : ShortString;
         function HorizontalDEMSpacing(short : boolean = false) : ShortString;
         function SimpleHorizontalDEMSpacing(BoxSize : integer) : ShortString;
         function GridDefinition : ShortString;
         function DEMSizeString : shortstring;
         function ColsRowsString : ShortString;
         function ZRange : ShortString;

         procedure TrackElevationRange(Where : shortstring);

         function NormalAtPoint(Col,Row : integer; var n1,n2,n3 : float32) : boolean;
         function DownhillVectorAtPoint(Col,Row : integer; var n1,n2,n3 : float32) : boolean;


         procedure GetStraightRouteLatLongDegree(Lat1,Long1,Lat2,Long2 : float64; StraightAlgorithm : tStraightAlgorithm; var NumPoints : integer; var Lats,Longs,dists : Petmath.bfarray32);
         procedure GetStraightRouteDEMGrid(Lat1,Long1,Lat2,Long2 : float64; StraightAlgorithm : tStraightAlgorithm; var NumPoints : integer; var xgrids,ygrids,dists : Petmath.bfarray32);
         procedure GetStraightRoute(LatLong : boolean; Lat1,Long1,Lat2,Long2 : float64; StraightAlgorithm : tStraightAlgorithm; var NumPoints : integer;  var xgrids,ygrids,dists : Petmath.bfarray32);
         procedure GetVisiblePoints(ObsElev,W_TargetUp,MinViewPort,MaxViewPort : float64; ObserverTerrainHug,TargetTerrainHug : boolean; PointsPerRay : integer;
                var xgrids,ygrids,dists,elevs : Petmath.bfarray32; var VisPoints : array of boolean);

         procedure LatLongDegreePointsRequiredAntenna(NPts : integer; Lat1,Long1,ObsUp,Lat2,Long2 : float64; var XGrids,YGrids,Dists,HeightsReq : Petmath.bfarray32);

         function VisibleElevation(xg1,yg1,z1,xg2,yg2 : float64) : float64;
         procedure HorizonBlocking(Lat,Long,AzimuthTrue,DistanceToGoOut,ObsUp : float64; var BlockAngle,BlockDist,BlockLat,BlockLong : float64; StraightAlgorithm : DemDefs.tStraightAlgorithm);
         function MaxHorizonAngle(Lat,Long,LeftAzimuthTrue,RightAzimuthTrue,AzIncrement,DistanceToGoOut,ObsUp : float64; StraightAlgorithm : DemDefs.tStraightAlgorithm) : float64;

         procedure InterpolateAcrossHoles(ShowResults : boolean);
         procedure SpecialInterpolateAcrossHoles;
         procedure FillHolesSelectedBoxFromReferenceDEM(GridLimits : tGridLimits; RefDEM : integer; HoleFill : tHoleFill);

         procedure MissingDataToConstantVelue(SeaLevel : float64 = 0);
         procedure MarkElevationRangeAsConstant(var NumPts : integer; CheckMaxMin : boolean = true);
         procedure MarkInRangeMissing(LowVal,HighVal : float64; var NumPts : int64; CheckMaxMin : boolean = true);
         procedure MarkOutsideRangeMissing(LowVal,HighVal : float64; var NumPts : int64; CheckMaxMin : boolean = true);
         procedure MarkAboveMissing(LowVal : float64; var NumPts : int64; CheckMaxMin : boolean = true);
         procedure MarkBelowMissing(HighVal : float64; var NumPts : int64; CheckMaxMin : boolean = true);

         function FullDEMGridLimits : tGridLimits;
         function SpecifyDEMGridLimits(xgl,ygl,xgh,ygh : float64) :  tGridLimits;
         function SpecifyDEMGridLimitsFromLatLong(LatLow,LongLow,LatHi,LongHi : float64) :  tGridLimits;

         procedure SetElevationMultiple;  //must be public
         procedure DefineDEMvariables(TransformToPreferDatum : boolean);
         procedure AssignProjectionFromDEM(var MapProjection : tMapProjection; DebugName : shortstring);

         function MetadataFileName : PathStr;

         function GetPerpendicularLineEnd(Lat,Long,SizeMeters,Trend : float64; var Lat1,Long1,Lat2,Long2 : float64; MustBeOnMap : boolean = false) : boolean;

         {$IfDef TrackDEMCorners}
            procedure WriteDEMCornersToDebugFile(Where : shortstring);
         {$EndIf}

         {$If Defined(TrackHorizontalDatum)}
            procedure TrackHorizontalDatumDebugLog(where : shortstring);
         {$EndIf}


         {$IfDef ExVegDensity}
         {$Else}
            procedure GetJustVegHeight(xgrid,ygrid : float32; var veg_ht : float32);
            procedure OpenVegGrid(var fName : PathStr; Which : integer = 1);
            procedure OpenDSMGrid;
            procedure CloseVegGrid(Which : integer);
            function GetElevMetersWithVeg(x,y : float64; var z : float32) : boolean;
            function GetElevMetersWithVegFromLatLong(Lat,Long : float64; var z : float32) : boolean;
         {$EndIf}

         {$IfDef AllowDEMGeomorph}
            procedure GetSlopesInLongArray(GridLimits: tGridLimits; var NPts : int64; var Values : Petmath.bfarray32; IncludeSeaLevel : boolean = true);
            procedure GetBoxGridSizeDiameter(BoxSizeMeters : integer; var XBoxGridSize,YBoxGridSize : integer; var BoxSizeString : shortstring);
            procedure GetPlanCInLongArray(GridLimits: tGridLimits; var NPts : int64; var Values : Petmath.bfarray32; IncludeSeaLevel : boolean = true);
            procedure GetProfCInLongArray(GridLimits: tGridLimits; var NPts : int64; var Values : Petmath.bfarray32; IncludeSeaLevel : boolean = true);
            procedure GetBothOpennessInLongArray(GridLimits: tGridLimits; var NPts : int64; var UpValues,DownValues : Petmath.bfarray32; IncludeSeaLevel : boolean = true);
            {$IfDef MultipleCurvatureMethods} function GetCurvature(Col,Row : integer; var PlanCurvature,SlopeCurvature : float64) : boolean; {$EndIf}
            procedure GetDEMMeanStd;
            procedure ElevationStatistics(var Mean,Std,AveDev : float32; UseZero : boolean);
            function ElevationMoments(GridLimits: tGridLimits) : tMomentVar;
            procedure ElevationMomentsWithArray(GridLimits: tGridLimits; var MomentVar : tMomentVar; var zvs : bfarray32);

            procedure SlopeMoments(GridLimits: tGridLimits; var SlopeMoment : tMomentVar);
            procedure SlopeMomentsWithArray(GridLimits: tGridLimits; var SlopeMoment : tMomentVar; var zvs : bfarray32);

            procedure RoughnessMomentsWithArray(GridLimits: tGridLimits; var MomentVar : tMomentVar; var zvs : bfarray32);
            procedure GetRoughnessInLongArray(GridLimits: tGridLimits; var NPts : int64; var Values : Petmath.bfarray32);

            procedure PlanCMoments(GridLimits: tGridLimits; var PlanCMoment : tMomentVar);
            procedure ProfCMoments(GridLimits: tGridLimits; var ProfCMoment : tMomentVar);
            procedure BothOpennessMoments(GridLimits: tGridLimits; var UpOpenMoment,DownOpenMoment : tMomentVar);

            function TerrainCategoryLabel(TerrainCategory : tTerrainCatDefinition) : ShortString;
            function AmbiguosRidgeValley(SWCol,SWRow : integer; var CouldKnow : boolean) : boolean;
            procedure InitializeTerrainCategory(var TerrainCat : tTerrainCatDefinition);
            function ClassifyAPoint(Col,Row : integer) : tPointType;
            function HighLowPointNeighbors(Col,Row,Region : integer; Tolerance : float32; var Higher,Lower : integer) : boolean;  inline;

            function GDAL_ScaleFactorString : shortstring;

            function GetRelief(Col,Row,BoxSize : integer; var AvgElev,Relief,ElevStdDev,PCLower,TPI : float32) : boolean;
            function InTerrainCategory(x,Y : integer; TerrainCategory : tTerrainCatDefinition) : boolean;

            function FindReliefInflectionGraph(xg,yg : integer; var Distance,Relief : float64) : boolean;
            function FindLocationOfMaximum(GridLimits: tGridLimits; var xloc,yloc : integer; var LocMax : float32) : boolean;
            function FindLocationOfMinimum(GridLimits: tGridLimits; var xloc,yloc : integer; var LocMin : float32) : boolean;
            function FindLocationOfMultipleMaxima(GridLimits: tGridLimits; var NPts : integer; var Locations : array of tGridZ) : boolean;
            function IsSpire(Col,Row,dx,dy : integer; var SpireHeightM : float32; var NumLower : integer) : boolean;
            function PointHasSpecifiedRelief(Col, Row, BoxSize,SampleFactor : integer; Relief: float64): boolean;
            procedure BoxStatsDB(BoxSize: integer = 0);
            function QuickRelief(Col,Row : integer; Limits : tGridLimits; var Relief,Summit,BaseLevel,GeoRelief,Dropoff,Elev_Relf : float32) : boolean; overload;
            function QuickRelief(Col,Row,BoxSize : integer; var Relief,Summit,BaseLevel,GeoRelief,Dropoff,Elev_Relf : float32) : boolean; overload;
            procedure WoodPointClassify(Col,Row : integer; var PointType : tPointType);
            function GetEvansParams(Col,Row,RegionSize : integer; var SlopeDeg,SlopeCurvature, PlanCurvature,crossc,MaxCurve,MinCurve : float64) : boolean;

            {$IfDef ExGeostats}
            {$Else}
               procedure DirectionalSlopesReport(Lat,Long : float64);
               function SSOComputations(GridLimits : tGridLimits; var SSOvars : tSSOvars; PlotResults : boolean; NetName,AspName : PathStr; UseMinSlope : float64 = 0; UseMaxSlope : float64 = 9999) : boolean;
               function FeatureSSOComputations(inFeatureDEM,inFeatureID,LoX,LoY,HiX,HiY : integer; var s1s2,s2s3,Trend,RoughnessFactor,DownDip : float64) : boolean;
               function PointSSOComputations(Col,Row,FullBoxSizeMeters : integer; var SSOvars : tSSOvars; PlotResults,Complete,PlotAspectFreq : boolean) : boolean;  inline;
               function SimplePointSSOComputations(PlotResults : boolean; Col,Row,FullBoxSizeMeters : integer; var s1s2,s2s3,Trend,RoughnessFactor : float64) : boolean;
               function OrientationTable(fName : PathStr; UseMap : tMapForm) : integer;
               procedure DoSSOStrip(Limits : tGridLimits; var Results : tStringList);
               function SSOByRegionSize(Col,Row: integer; var Maxs2s3,BoxSizeForMax,DirForMax,Relief : float32; var Results : tStringList) : boolean;
            {$EndIf}

            {$IfDef ExComplexGeostats}
            {$Else}
               function ContourLineCrossing(x,y : integer; z : float64) : boolean;
               procedure FractalBox(GridLimits: tGridLimits; var FracDim,r : float32; SkipDraw : boolean = false; CloseGraph : boolean = false);
               procedure EntireDEMFractalBox;
               function CreateWholeDEMHistogram : TThisBaseGraph;
               function CreatePartDEMHistogram(GridLimits: tGridLimits) : TThisBaseGraph;
               procedure InitializeNormals(var NumPts : Integer);
               procedure DisposeNormals;
               function FigureEntropy : float64;
               procedure ComputeVariogram(GridLimits: tGridLimits); //SkipDrawing : boolean);
               procedure VariogramGamma(GridLimits: tGridLimits; var EastWest,NorthSouth,NESW,NWSE : float32);
            {$EndIf}
        {$EndIf}
   end;


type
   tDEMDataSetArray = array[1..MaxDEMDataSets] of tDEMDataSet;


procedure InitilializeDEMCoord;

procedure FindCutPoints(Min,Max,NumCuts : integer; var Cut : ColorCutArrayType);
function DropEarthCurve(d : float64) : float64;

procedure PickSlopeAspectMethod(aMessage : shortstring; var SlopeMethod : byte);
procedure GetSampleBoxSize(WhichDEM : integer; var BoxSize : integer);

function RectSpacingFactor(DataSpacing : tSpacingUnit) : float64;

{$IfDef NoMapOptions}
{$Else}
   procedure MaskGrid(Map : tMapForm; DEM : integer; MatchCriteria : boolean; OnlyMissing : boolean = false);
{$EndIf}

function MaskValidPoint(Col,Row : integer) : boolean;
function NumDEMDataSetsOpen : integer;
procedure ZeroDEMHeader(var HeadRecs : tDEMheader; UTM : boolean);

function ValidDEM(DEM : integer) : boolean;


const
   ReadDEMStr = 'Read DEM';
   DEMFilterMasks = 'Any likely DEM|*.DEM*;*.GZ;*.tif;*.tiff;*.bil;*.flt;*.img;*.asc|' +
          'DEM file|*.dem|' +
          'GeoTIFF|*.tif;*.tiff|' +
          'GeoTIFF DEM.TIF|*dem.tif|' +
          'ESRI grid|w001001.adf|' +
          'ASCII Arc Grid|*.asc|' +
          'IMG file grid|*.img|' +
          'NED binary or gridfloat|*.BIL;*.DEMFLOAT;*.FLT|' +
          'SRTM Heightfield|*.hgts;*.hgt|' +
          'NGDC GRD98|*.G03|' +
          'NOS EEZ Bathymetry|*.PRU|' +
          'OS 5 or 20 km tile|*.ntf|' +
          'Surfer grid|*.grd|' +
          //'VTP BT|*.bt|' +
          {$IfDef ExDTED}
          {$Else}
             'DTED|*.dt*|' +
          {$EndIf}
          'All files|*.*|';

{$IfDef ExDTED}
{$Else}
   function GetNIMADTEDHeader(var InputFile : file) : tStringList;
{$EndIf}

function LoadNewDEM(var WantedDem : integer; var FullFileName : PathStr; CreateMap : boolean = true; DEMMessage : shortstring = 'New DEM';  MapTitleBar : shortstring = ''; DrawTheMap : boolean = true; ChangeDefaultName : boolean = true) : boolean;

function NewArea(TransformToNewDatum : boolean; var ImportingDEM : integer; DEMMessage : shortstring; var TFile : PathStr; DEMtoUse : integer = 0) : boolean;
procedure OpenDEMDataStructures(var WantedDEM : integer);
function OpenAndZeroNewDEM(TransformToNewDatum : boolean; NewHeader : tDEMheader; var WantedDEM : integer; DEMname : ShortString; InitDEMMode : byte; InitialValue : float64 = 0) : boolean;

function MakeNewBlankDEMMap(LatCorner,LongCorner,LatSize,LongSize : float64; xpixels,ypixels : integer) : integer;

procedure ReadASCIIArcGrid(FileName : PathStr; WantedDEM : tDEMDataSet; var Error : boolean; ReallyReadDEM : boolean);
procedure ReadFusionDTM(FileName : PathStr; WantedDEM : tDEMDataSet; var Error : boolean);

function CreateNewGlobalGrid(GreenwhichLeft : boolean = true; Resolution : tDEMprecision = FloatingPointDEM; Spacing : float64 = -99) : integer;

procedure ConvertUKOSDEMtoUTM(BigDEM : integer; ToUTM : boolean);
function UK_OS_projection(fName : PathStr) : boolean;

function ReadWorldFile(var MapProjection : tMapProjection; var DigitizeDatum : ShortString; FileName : PathStr; var RegVars : tRegVars) : boolean;
function ReadWorldFileValues(FileName : PathStr; var DeltaX,Xrot,YRot,DeltaY,UpLeftX,UpLeftY : float64) : boolean;
procedure SaveWorldFile(FName : PathStr; xPixelSize,yPixelSize,xutm,yutm : float64; DatumCode : ShortString; UTMZone : integer; SatLatHemi : AnsiChar);
function CreateWorldFileStringList(xPixelSize,yPixelSize,xutm,yutm : float64) : tStringList;
procedure GetDefaultWorldFile(var ReadFileName : PathStr);
function FindExistingWorldFile(var ReadFileName : PathStr) : boolean;
function ValidWorldFile(FileName : PathStr) : boolean;

procedure MaskStripFromSecondGrid(Limits : tGridLimits;  FirstGrid,SecondGrid : integer;  HowMask : tMaskGrid);

procedure VerticalDatumShiftWithVDATUM(AreaName : shortstring; DEM,db : integer; SaveName : PathStr; ErrorLog : tStringList = nil);
procedure VerticalDatumShiftWithGDAL(DEM : integer; var SaveName : PathStr);
procedure VerticalDatumShift(DEM : integer; vdShift : tvdShift);


const
   DEMtooLargeString = 'DEM too large';
   TooManyDEMsOpenString = 'Too many open DEMs; last closed';
   DEMRequestBeyondDataString = 'DEM request beyond data';

const
   CalculatingCurvature : boolean = true;
   WeKnowItsUTMZone : int16 = -99;
var
   MaskMaxVal,MaskMinVal : float64;
   GridMaskDEM : integer;
   DEMGlb : tDEMDataSetArray;
   GhostContour  : float64;
   RouteTiffThroughGDAL,
   OpeningNewGrid,
   SkipSphericalCheck,
   SubsequentDEM,
   FoundFootContours : boolean;
   DEMJustSubset : PathStr;
   GlobalDEMforGetElevCol : integer;
   SSOFailPts,SSOFailFlat,SSOfailOrg : int64;
   {$IfDef  ExGraphs}
   {$Else}
      RoseGraph : TThisBaseGraph;
   {$EndIf}



implementation

uses
   {$IfDef VCL}  Nevadia_Main, {$EndIf}

   {$IfDef ExIndexes}
   {$Else}
      DEM_Indexes,
   {$EndIf}

   {$IfDef ExGeostats}
   {$Else}
      DEMStat,
   {$EndIf}

   {$IfDef  ExGeotiff}
   {$Else}
     GeoTiff,
   {$EndIf}

   {$IfDef VCL}
      DEMMapDraw,
      GetLatLn,
      DEMLOS_draw,
      DEMXYZExport,
      Pick_Limits,
   {$EndIf}

   {$IfDef ExGDAL}
   {$Else}
      md_use_tools,
   {$EndIf}

   {$IfDef ExGIS}
   {$Else}
      DEMDatabase,
      PetDBUtils,
      Make_tables,
   {$EndIf}

   {$IfDef ExGeology}
   {$Else}
      Petmar_geology,
   {$EndIf}

   DEM_Manager,
   map_overlays,
   DEMDef_routines,
   las_lidar,
   Compress_form,
   gdal_tools,
   PetImage;

var
   CountInStrips : integer;

{$I demcoord_vis.inc}
{$I demcoord_straight.inc}
{$I demcoord_worldfile.inc}
{$I demcoord_read_dem.inc}
{$I demcoord_write_dem.inc}
{$I demcoord_elev_manip.inc}
{$I demcoord_vert_datum.inc}

{$IfDef ExDEMEdits}
{$Else}
   {$I demcoord_edits.inc}
{$EndIf}

{$IfDef ExGeostats}
{$Else}
   {$I demcoord_point_class.inc}
   {$I demcoord_geomorph.inc}
{$EndIf}

{$IfDef VCL}
   {$I ..\common_code\demcoord_graphics.inc}
{$EndIf}

{$IfDef ExVegDensity}
{$Else}
   {$I ..\common_code\demcoord_veg.inc}
{$EndIf}


{$IfDef TrackDEMCorners}
   procedure tDEMDataSet.WriteDEMCornersToDebugFile(Where : shortstring);
   begin
      HighlightLineToDebugFile(Where + '  '  + AreaName + '  ' + RasterPixelIsString(DEMHeader.RasterPixelIsGeoKey1025));
      WriteLineToDebugFile('Geotiff NW corner:       ' + RealToString(GeotiffNWCornerX,-18,-8) + '/' +  RealToString(GeotiffNWCornerY,-18,-8) );
      WriteLineToDebugFile('DEM SW corner:           ' + RealToString(DEMHeader.DEMSWCornerX,-18,-8) + '/' +  RealToString(DEMHeader.DEMSWCornerY,-18,-8) );
      WriteLineToDebugFile('Compute point SW corner: ' + RealToString(ComputeSWCornerX,-18,-8) + '/' +  RealToString(ComputeSWCornerY,-18,-8) );
   end;
{$EndIf}

{$If Defined(TrackHorizontalDatum)}
   procedure tDEMDataSet.TrackHorizontalDatumDebugLog(where : shortstring);
   begin
      WriteLineToDebugFile(Where + ' ' + AreaName + '  ' +  DEMMapProjection.h_DatumCode + '  ' + StringFromDatumCode(DEMheader.DigitizeDatum));
   end;
{$EndIf}



procedure tDEMDataSet.SetRasterPixelIsGeoKey1025(DoHalfPixelShift : boolean);
var
   TStr : shortstring;
begin
   ComputeSWCornerX := DEMHeader.DEMSWCornerX;
   ComputeSWCornerY := DEMHeader.DEMSWCornerY;

   if IsNan(GeotiffNWCornerX) then GeotiffNWCornerX := DEMHeader.DEMSWCornerX;
   if IsNan(GeotiffNWCornerY) then GeotiffNWCornerY := DEMHeader.DEMSWCornerY + DEMHeader.DEMySpacing * pred(DEMHeader.NumRow);
   TStr := '';
   if DoHalfPixelShift and (DEMHeader.RasterPixelIsGeoKey1025 in [PixelIsUndefined,PixelIsArea]) then begin
      ComputeSWCornerX := DEMHeader.DEMSWCornerX + 0.5 * DEMHeader.DEMxSpacing;
      ComputeSWCornerY := DEMHeader.DEMSWCornerY - 0.5 * DEMHeader.DEMySpacing;
      Tstr := ' half pixel shift applied';
   end;
   {$If Defined(TrackDEMCorners) or Defined(RecordHalfPixelShift)} WriteDEMCornersToDebugFile('SetRasterPixelIsGeoKey1025' + TStr); {$EndIf}
end;


function tDEMDataSet.PixelCenterOnFullSecond : boolean;
var
   xfrac,yfrac : float64;
begin
   xfrac := abs(frac(ComputeSWCornerX / DEMheader.DEMxSpacing));
   yfrac := abs(frac(ComputeSWCornerY / DEMheader.DEMySpacing));
   Result := (xfrac < 0.00000001) and (yfrac < 0.00000001);
end;


function tDEMDataSet.Geo_Z_Factor : float32;
begin
   Result := 1.0 / (111320.0 * CosDeg(0.5* LatSizeMap + DEMSWcornerLat));
end;


function tDEMDataSet.ThinThisDEM(fName : PathStr = ''; ThinFactor : integer = 0; DoItByAveraging : boolean = false; Offset : integer = 0) : integer;
var
   Col,Row,x,y,Npts : integer;
   z : float32;
   Sum       : float64;
   NewHeadRecs : tDEMheader;
   TStr : shortstring;
begin
   if (ThinFactor = 0) then begin
      ThinFactor := 3;
      if DoItByAveraging then begin
         ReadDefault('Average NxN (odd) pixel region to use (use with caution)',ThinFactor);
      end
      else begin
         ReadDefault('Decimation thin factor',ThinFactor);
      end;
   end;

   NewHeadRecs := DEMheader;
   NewHeadRecs.NumCol := DEMheader.NumCol div ThinFactor;
   NewHeadRecs.NumRow := DEMheader.NumRow div ThinFactor;
   NewHeadRecs.DEMySpacing := DEMheader.DEMySpacing * ThinFactor;
   NewHeadRecs.DEMxSpacing := DEMheader.DEMxSpacing * ThinFactor;
   if DoItByAveraging then begin
      NewHeadRecs.DEMSWCornerX := DEMheader.DEMSWCornerX + DEMheader.DEMxSpacing * 0.5 * ThinFactor;
      NewHeadRecs.DEMSWCornerY := DEMheader.DEMSWCornerY + DEMheader.DEMySpacing * 0.5 * ThinFactor;
      TStr := 'Mean_subset_';
   end
   else begin
      TStr := 'Thin_';
   end;

   OpenAndZeroNewDEM(true,NewHeadRecs,Result,TStr + IntToStr(ThinFactor) + '_' + AreaName,InitDEMmissing);

   StartProgress(DEMGlb[Result].AreaName);
   for Col := 0 to pred(DEMGlb[Result].DEMheader.NumCol) do begin
      UpdateProgressBar(Col / pred(DEMGlb[Result].DEMheader.NumCol));
      for Row := 0 to pred(DEMGlb[Result].DEMheader.NumRow) do begin
         if DoItByAveraging then begin
            Sum := 0;
            NPts := 0;
            for x := 0 to pred(ThinFactor) do
               for y := 0 to pred(ThinFactor) do
                  if GetElevMeters(Col * ThinFactor + x,Row * ThinFactor + y,z) then begin
                     Sum := Sum + z;
                     inc(Npts);
                  end;
            if (NPts > 0) then DEMGlb[Result].SetGridElevation(Col,Row,Sum / NPts);
         end
         else begin
            if GetElevMeters(Col * ThinFactor,Row * ThinFactor,z) then DEMGlb[Result].SetGridElevation(Col,Row,z);
         end;
      end;
   end {while};
   DEMGlb[Result].CheckMaxMinElev;
   EndProgress;
end;



function tDEMDataSet.HalfPixelAggregation(fName : PathStr; PixelIs : byte; SaveFile : boolean; Offset : integer = 0) : integer;
var
   Col,Row,{x,y,Npts,}bx,by,ThinFactor : integer;
   znw,zw,zsw,zn,z,zs,zne,ze,zse : float32;
   NewHeadRecs : tDEMheader;
   TStr : shortstring;
begin
   ThinFactor := 2;

   NewHeadRecs := DEMheader;
   NewHeadRecs.RasterPixelIsGeoKey1025 := PixelIs;
   NewHeadRecs.NumCol := (DEMheader.NumCol - Offset) div ThinFactor;
   NewHeadRecs.NumRow := (DEMheader.NumRow - Offset) div ThinFactor;
   NewHeadRecs.DEMySpacing := DEMheader.DEMySpacing * ThinFactor;
   NewHeadRecs.DEMxSpacing := DEMheader.DEMxSpacing * ThinFactor;

   if (Offset = 1) then begin
      NewHeadRecs.DEMSWCornerX := DEMheader.DEMSWCornerX + DEMheader.DEMxSpacing * ThinFactor - DEMheader.DEMxSpacing * Offset;
      NewHeadRecs.DEMSWCornerY := DEMheader.DEMSWCornerY + DEMheader.DEMySpacing * ThinFactor - DEMheader.DEMySpacing * Offset;
   end
   else begin
      Offset := 2;
      NewHeadRecs.DEMSWCornerX := DEMheader.DEMSWCornerX + DEMheader.DEMxSpacing * Offset;
      NewHeadRecs.DEMSWCornerY := DEMheader.DEMSWCornerY + DEMheader.DEMySpacing * Offset;
   end;
   TStr := 'Mean_subset_';

   if (fName = '') then fName := TStr + IntToStr(ThinFactor) + '_' + AreaName;

   OpenAndZeroNewDEM(true,NewHeadRecs,Result,ExtractFileNameNoExt(fName),InitDEMmissing);

   StartProgress(DEMGlb[Result].AreaName);
   for Col := 0 to pred(DEMGlb[Result].DEMheader.NumCol) do begin
      UpdateProgressBar(Col / pred(DEMGlb[Result].DEMheader.NumCol));
      for Row := 0 to pred(DEMGlb[Result].DEMheader.NumRow) do begin
         bx := Offset + Col * ThinFactor;
         by := Offset + Row * ThinFactor;
         if GetNineElevMeters(bx,by,znw,zw,zsw,zn,z,zs,zne,ze,zse) then begin
            z := zw/16 + zn/8 + zne/16 + zw/8 + z/4 + ze/8 + zsw/16 + zs/8 + zse/16;
            DEMGlb[Result].SetGridElevation(Col,Row,z);
         end;
      end;
   end {while};
   DEMGlb[Result].CheckMaxMinElev;
   DEMGlb[Result].SetUpMap(Result,true,SelectionMap.MapDraw.MapType);
   if SaveFile then  DEMGlb[Result].WriteNewFormatDEM(fName);

   EndProgress;
end;


function tDEMDataSet.ThinAndOpenGridSetMissing(ThinFactor : integer; NewPrecision : tDEMprecision; Gridname : shortstring; ElevUnits : tElevUnit) : integer;
var
   NewHeadRecs : tDEMheader;
begin
   NewHeadRecs := DEMheader;
   NewHeadRecs.DEMPrecision := NewPrecision;
   NewHeadRecs.NumCol := DEMheader.NumCol div ThinFactor;
   NewHeadRecs.NumRow := DEMheader.NumRow div ThinFactor;
   NewHeadRecs.DEMySpacing := DEMheader.DEMySpacing * ThinFactor;
   NewHeadRecs.DEMxSpacing := DEMheader.DEMxSpacing * ThinFactor;
   OpenAndZeroNewDEM(true,NewHeadRecs,Result,'Thin_' + IntToStr(ThinFactor) + '_' + AreaName,InitDEMmissing);
end;


function tDEMDataSet.CloneAndOpenGridSetMissing(NewPrecision : tDEMprecision; Gridname : shortstring; ElevUnits : tElevUnit) : integer;
var
   NewHeadRecs : tDEMheader;
begin
   {$If Defined(RecordCreateNewDEM) or Defined(RecordClone)} WriteLineToDebugFile('tDEMDataSet.CloneAndOpenGrid in, ElevUnits=' + IntToStr(ElevUnits)); {$EndIf}
   NewHeadRecs := DEMheader;
   NewHeadRecs.DEMPrecision := NewPrecision;
   NewHeadRecs.ElevUnits := ElevUnits;
  {$IfDef RecordCreateNewDEM} WriteLineToDebugFile('tDEMDataSet.CloneAndOpenGrid off to OpenAndZero'); {$EndIf}
   Result := 0;
   if OpenAndZeroNewDEM(true,NewHeadRecs,Result,Gridname,InitDEMMissing,0) then begin
      DEMGlb[Result].AreaName := GridName;
      AssignProjectionFromDEM(DEMGlb[Result].DEMMapProjection,'DEM=' + IntToStr(Result));
      DEMGlb[Result].DEMMapProjection.ProjectionSharedWithDataset := true;
   end;
  {$If Defined(RecordCreateNewDEM) or Defined(RecordClone)} WriteLineToDebugFile('tDEMDataSet.CloneAndOpenGrid out, ElevUnits=' + ElevUnitsAre(ElevUnits)); {$EndIf}
end;


procedure tDEMDataSet.TrackElevationRange(Where : shortstring);
begin
   WriteLineToDebugFile(Where + ' ' + AreaName + ' DEM=' + IntToStr(ThisDEM) + ' ' + ZRange);
end;


function tDEMDataSet.GDAL_ScaleFactorString : shortstring;
var
   Distance1,Distance2,Distance3 : float64;
begin
   if (DEMheader.DEMUsed = ArcSecDEM) then begin
     MetersPerDegree(DEMSWcornerLat + 0.5 * LatSizeMap,DEMSWcornerLong + 0.5 * LongSizeMap,Distance1,Distance2,Distance3);
     Result := RealToString(Distance3,-12,1);
   end
   else Result := '';
end;


procedure tDEMDataSet.GridSpacingDetails(Region : sfBoundBox; var AverageX,AverageY,AverageSpacing,AverageGridTrue : float64);
const
   Steps = 5;
var
   Lat,Long : float64;
   GridTrueAngle : float32;
   x,y,xg,yg,n : integer;
   dxs,dys : array[1..100] of float32;
begin
   AverageX := 0;
   AverageY := 0;
   AverageSpacing := 0;
   AverageGridTrue := 0;
   n := 0;
   for x := 0 to Steps do begin
      xg := round(Region.xmin) + round(x / Steps * (Region.xmax - Region.xmin));
      for y := 0 to Steps do begin
         inc(n);
         yg := round(Region.ymin) + round(y / Steps * (Region.ymax - Region.ymin));
         PixelSpacingAndRotation(xg,yg,Lat,Long,dxs[n],dys[n],GridTrueAngle,false);
         AverageGridTrue := AverageGridTrue  + GridTrueAngle;
      end;
   end;

   AverageX := Median(dxs,n);
   AverageY := Median(dys,n);
   AverageSpacing := 0.5 * (AverageX + AverageY);
   AverageGridTrue := AverageGridTrue / n;
   if IsNAN(AverageGridTrue) then AverageGridTrue := 0;
end;


procedure MaskStripFromSecondGrid(Limits : tGridLimits;  FirstGrid,SecondGrid : integer;  HowMask : tMaskGrid);
var
   Col,Row: integer;
   Lat,Long : float64;
   z,z2 : float32;
   SameGrid : boolean;
begin
   SameGrid := DEMGlb[FirstGrid].SecondGridIdentical(SecondGrid);

   for Row := Limits.YGridLow to Limits.YGridHigh do begin
      TInterlocked.Increment(ParallelRowsDone);
      if (ParallelRowsDone Mod 250 = 0) then UpdateProgressBar(ParallelRowsDone / DEMGlb[FirstGrid].DEMheader.NumRow);

      for Col := Limits.XGridLow to Limits.XGridHigh do begin
         if (not DEMGlb[FirstGrid].MissingDataInGrid(Col,Row)) then begin
            if SameGrid then begin
               if (HowMask = msSecondValid) then begin
                  if (not DEMGlb[SecondGrid].MissingDataInGrid(Col,Row)) then begin
                     DEMGlb[FirstGrid].SetGridMissing(Col,Row);
                     TInterlocked.Increment(EditsDone);
                  end;
               end
               else if (HowMask = msSecondMissing) then begin
                  if DEMGlb[SecondGrid].MissingDataInGrid(Col,Row) then begin
                     DEMGlb[FirstGrid].SetGridMissing(Col,Row);
                     TInterlocked.Increment(EditsDone);
                  end;
               end
               else begin
                  if DEMGlb[FirstGrid].GetElevMetersOnGrid(Col,Row,z2) then begin
                     if ((HowMask = msAboveSecond) and (z2 > z)) or  ((HowMask = msBelowSecond) and (z2 < z)) then begin
                        DEMGlb[FirstGrid].SetGridMissing(Col,Row);
                        TInterlocked.Increment(EditsDone);
                     end;
                  end;
               end;
            end
            else begin
               DEMGlb[FirstGrid].DEMGridToLatLongDegree(Col,Row,Lat,Long);
               if (HowMask = msSecondValid) then begin
                  if DEMGlb[SecondGrid].GetElevFromLatLongDegree(Lat,Long,z) then begin
                     DEMGlb[FirstGrid].SetGridMissing(Col,Row);
                     TInterlocked.Increment(EditsDone);
                  end;
               end
               else if (HowMask = msSecondMissing) then begin
                  if not DEMGlb[SecondGrid].GetElevFromLatLongDegree(Lat,Long,z) then begin
                     DEMGlb[FirstGrid].SetGridMissing(Col,Row);
                     TInterlocked.Increment(EditsDone);
                  end;
               end
               else begin
                  if DEMGlb[SecondGrid].GetElevFromLatLongDegree(Lat,Long,z) then begin
                     if DEMGlb[FirstGrid].GetElevMetersOnGrid(Col,Row,z2) then begin
                       if (HowMask = msAboveSecond) and (z2 > z) then begin
                          DEMGlb[FirstGrid].SetGridMissing(Col,Row);
                          TInterlocked.Increment(EditsDone);
                       end;
                       if (HowMask = msBelowSecond) and (z2 < z) then begin
                          DEMGlb[FirstGrid].SetGridMissing(Col,Row);
                          TInterlocked.Increment(EditsDone);
                       end;
                     end;
                  end;
               end;
            end;
         end;
      end;
   end;
end;


function ValidDEM(DEM : integer) : boolean;
begin
   Result := (DEM > 0) and (DEM <= MaxDEMDataSets) and (DEMGlb[DEM] <> Nil);
end;


function NumDEMDataSetsOpen : integer;
var
   i : integer;
begin
   Result := 0;
   for i := 1 to MaxDEMDataSets do
      if ValidDEM(i) and (not DEMGlb[i].HiddenGrid) then inc(Result);
end;


procedure tDEMDataSet.PixelSpacingAndRotation(Col,Row : integer; var Lat,Long : float64; var xdistance,ydistance,GridTrueAngle : float32; Quick : boolean = true);
//quick mode uses stored values from the DEM initial opening
//pixel sizes will agree within about 1 mm for a 1" DEM
//quick mode is about 3 times faster
var
   Lat1,Long1,Lat2,Long2,Lat3,Long3,Lat4,Long4,Bearing,xdist,ydist : float64;
begin
   Lat := -99;
   Long := -99;
   if Quick and (DEMheader.DEMUsed = ArcSecDEM) then begin
      xdistance := XSpaceByDEMrow^[Row];
      ydistance := AverageYSpace;
      GridTrueAngle := 0;
   end
   else if Quick and (DEMheader.DEMUsed <> ArcSecDEM) then begin
      //this uses the nominal spacing in projected meters, which is very slightly different from the actual ground spacing
      xdistance := DEMHeader.DEMxSpacing;
      ydistance := DEMHeader.DEMYSpacing;
      GridTrueAngle := AverageGridTrue; //computed at the center of DEM during opening;  this will have problems if the DEM covers a large area near edge of UTM zone
   end
   else begin
      //this calls routines to deal with all possible cases of the DEM geometry for other more unusual projections
      DEMGridToLatLongDegree(Col,Row,Lat,Long);
      DEMGridToLatLongDegree(Col,pred(Row),Lat1,Long1);
      DEMGridToLatLongDegree(Col,succ(Row),Lat2,Long2);
      VincentyCalculateDistanceBearing(Lat1,Long1,Lat2,Long2,yDist,Bearing);
      ydistance := 0.5 * yDist;
      if (Bearing > 180) then GridTrueAngle := - (360 - Bearing)
      else GridTrueAngle := - (360 - Bearing);

      DEMGridToLatLongDegree(pred(Col),Row,Lat3,Long3);
      DEMGridToLatLongDegree(succ(Col),Row,Lat4,Long4);
      VincentyCalculateDistanceBearing(Lat3,Long3,Lat4,Long4,xDist,Bearing);
      xdistance := 0.5 * xDist;
   end;
end;


function tDEMDataSet.PixelSize(Col,Row : integer) : shortstring;
var
   Lat1,Long1,Lat2,Long2,Lat3,Long3,Lat4,Long4,Distance,Bearing: float64;
   xdist,ydist,GridTrueAngle : float32;
begin
   //this calls routines to deal with all possible cases of the DEM geometry
   DEMGridToLatLongDegree(Col,Row,Lat1,Long1);  //SW corner
   DEMGridToLatLongDegree(Col,succ(Row),Lat2,Long2); // NW coner
   DEMGridToLatLongDegree(succ(Col),Row,Lat3,Long3); //SE corner
   DEMGridToLatLongDegree(succ(Col),succ(Row),Lat4,Long4); //NE corner
   VincentyCalculateDistanceBearing(Lat1,Long1,Lat2,Long2,Distance,Bearing);
   Result := 'west side: ' + RealToString(distance,8,4) + ' m' + MessLineBreak;
   VincentyCalculateDistanceBearing(Lat3,Long3,Lat4,Long4,Distance,Bearing);
   Result := Result + 'east side: ' + RealToString(distance,8,4) + ' m' + MessLineBreak;
   VincentyCalculateDistanceBearing(Lat3,Long3,Lat1,Long1,Distance,Bearing);
   Result := Result + 'south side: ' + RealToString(distance,8,4) +  ' m' + MessLineBreak;
   VincentyCalculateDistanceBearing(Lat2,Long2,Lat4,Long4,Distance,Bearing);
   Result := Result + 'north side: ' + RealToString(distance,8,4) +  ' m' + MessLineBreak;

   PixelSpacingAndRotation(Col,Row,Lat1,Long1,xdist,ydist,GridTrueAngle,true);
   Result := Result + MessLineBreak + 'Quick pixel size:    ' + RealToString(xdist,-8,3) + 'x' + RealToString(ydist,-8,3) +  ' m   Grid-true angle ' + RealToString(GridTrueAngle,-8,3) + DegSym;
   PixelSpacingAndRotation(Col,Row,Lat1,Long1,xdist,ydist,GridTrueAngle,false);
   Result := Result + MessLineBreak + 'Detailed pixel size: ' + RealToString(xdist,-8,3) + 'x' + RealToString(ydist,-8,3) +  ' m   Grid-true angle ' + RealToString(GridTrueAngle,-8,3) + DegSym;
end;



function tDEMDataSet.GeotiffDEMName : PathStr;
begin
   Result := DEMfileName;
   if UpperCase(ExtractFileExt(Result)) <> '.TIF' then begin
      Result := MDtempDir + 'temp_dem.tif';
      SaveAsGeotiff(Result);
   end;
end;


procedure ZeroDEMHeader(var HeadRecs : tDEMheader; UTM : boolean);
begin
   FillChar(HeadRecs,SizeOf(tDEMheader),0);
   HeadRecs.DEMPrecision := FloatingPointDEM;
   if UTM then begin
      Headrecs.DEMUsed := UTMBasedDEM;
      Headrecs.DataSpacing := SpaceMeters;
   end
   else begin
      Headrecs.DEMUsed := ArcSecDEM;
      Headrecs.DataSpacing  := SpaceDegrees;
   end;
   Headrecs.DigitizeDatum := WGS84d;
   HeadRecs.ElevUnits := euMeters;
   StringToByteArray(MDdef.PreferPrimaryDatum,HeadRecs.DMAMapDefinition.h_DatumCode);
   Headrecs.UTMZone   := MDdef.DefaultUTMZone;
   Headrecs.LatHemi   := MDdef.DefaultLatHemi;
   HeadRecs.VerticalCSTypeGeoKey := 0;
   HeadRecs.RasterPixelIsGeoKey1025 := 0;
   HeadRecs.wktString := '';
end;


function MaskValidPoint(Col,Row : integer) : boolean;
var
   z : float32;
begin
   Result := True;
   if (GridMaskDEM > 0) and (DEMGlb[GridMaskDEM] <> Nil) then begin
       if DEMGlb[GridMaskDEM].GetElevMeters(Col,Row,z) then Result := (z >= MaskMinVal) and (z <= MaskMaxVal)
       else Result := false;
   end;
end;


procedure tDEMDataSet.ResetPrimaryDatumZone(NewLong : float64);
var
   NewZone : integer;
begin
   NewZone := GetUTMZone(NewLong);
   if (NewZone <> DEMMapProjection.projUTMZone) then begin
      DEMMapProjection.DefineDatumFromUTMZone('WGS84',NewZone,DEMheader.LatHemi,'tDEMDataSet.ResetPrimaryDatumZone');
   end;
end;


procedure tDEMDataSet.DEMCenterPoint(var Lat,Long : float64);
begin
   Lat := DEMSWcornerLat + 0.5 * LatSizeMap;
   Long := DEMSWcornerLong + 0.5 * LongSizeMap;
end;


procedure tDEMDataSet.GetDEMPointsSum(var NPts : integer; var Sum : float64);
var
   Col,Row : integer;
   z : float32;
begin
    NPts := 0;
    Sum := 0;
    for Col := 0 to DEMheader.NumCol do begin
       for Row := 0 to DEMheader.NumRow do begin
          if GetElevMeters(Col,Row,z) then begin
             inc(NPts);
             Sum := Sum + z;
          end;
       end;
    end;
end;


function tDEMDataSet.ReloadDEM(TransformtoNewDatum : boolean) : boolean;
begin
   Result := ReadDEMNow(DEMFileName,TransformtoNewDatum);
end;


function tDEMDataSet.FullDEMGridLimits :  tGridLimits;
begin
   Result.XGridLow := 0;
   Result.YGridLow := 0;
   Result.XGridHigh := pred(DEMheader.NumCol);
   Result.YGridHigh := pred(DEMheader.NumRow);
end;


function tDEMDataSet.SpecifyDEMGridLimits(xgl,ygl,xgh,ygh : float64) : tGridLimits;
begin
   Result.XGridLow := round(xgl);
   Result.YGridLow := round(ygl);
   Result.XGridHigh := round(xgh);
   Result.YGridHigh := round(ygh);
end;


procedure tDEMDataSet.LocationOfMaximumZAroundPoint(var Lat,Long : float64; var MaxZ : float32; BoxSize : integer);
var
   z   : float32;
   x,y,xloc,yloc,xg,yg : integer;
begin
   MaxZ := -99e39;
   LatLongDegreeToDEMGridInteger(Lat,Long,xg,yg);
   for x := xg - round(BoxSize / AverageXSpace)  to xg + round(BoxSize / AverageXSpace) do begin
      for y := yg - round(BoxSize / AverageYSpace)  to yg + round(BoxSize / AverageYSpace) do  begin
         if GetElevMeters(x,y,z) and (z > MaxZ) then begin
            Maxz := z;
            xloc := x;
            yloc := y;
         end;
      end;
   end;
   DEMGridToLatLongDegree(xloc,yloc,Lat,Long);
end;


procedure tDEMDataSet.ShiftGlobalDEM(NewLeftLong: float64);
var
   StartCol,x : integer;
   TempDEM : integer;
begin
  {$IfDef RecordCreateNewDEM} WriteLineToDebugFile('tDEMDataSet.ShiftGlobalDEM, left edge=' + IntToStr(round(NewLeftLong))); {$EndIf}
   ShowHourglassCursor;
   StartCol := round((NewLeftLong - DEMheader.DEMSWCornerX) / DEMheader.DEMxSpacing);
   {$IfDef RecordCreateNewDEM} WriteLineToDebugFile('StartCol=' + IntToStr(StartCol)); {$EndIf}
   if (StartCol <> 0) then begin
      ReloadDEM(false);
      StartProgress('Shift');
      LoadNewDEM(TempDEM,DEMFileName,false);
      if (TempDEM <> 0) then begin
         for x := 0 to pred(DEMheader.NumCol) do begin
            if (x mod 100 = 0) then UpDateProgressBar(x/DEMheader.NumCol);
            if (DEMheader.DEMprecision = SmallIntDEM) then begin
                if (x < StartCol) then DEMGlb[TempDEM].GetElevCol(x,SmallIntElevations^[StartCol+x])
                else DEMGlb[TempDEM].GetElevCol(x,SmallIntElevations^[X-StartCol]);
            end;
            if (DEMheader.DEMprecision = FloatingPointDEM) then begin
                if (x < StartCol) then DEMGlb[TempDEM].GetFloatElevCol(x,ShortFloatElevations^[StartCol+x])
                else DEMGlb[TempDEM].GetFloatElevCol(x,ShortFloatElevations^[X-StartCol]);
            end;
         end;
         DEMheader.DEMSWCornerX := NewLeftLong;
         EndProgress;
         CloseSingleDEM(TempDEM);
         DEMAlreadyDefined := false;
         DefineDEMVariables(true);
      end;
   end;
end;


function tDEMDataSet.ZRange : ShortString;
var
   Dec : integer;
   Range : float32;
begin
   Range := DEMheader.MaxElev - DEMheader.MinElev;
   if Range < 1 then Dec := 3
   else if Range < 10 then Dec := 2
   else if Range < 1000 then Dec := 1
   else Dec := 0;
   Result := 'z range: ' + RealToString(DEMheader.MinElev,-18,-Dec) + ' to ' + RealToString(DEMheader.MaxElev,-18,-Dec) + ' ' + ElevUnitsAre(DEMheader.ElevUnits);
end;

function tDEMDataSet.ColsRowsString : ShortString;
begin
   Result := 'DEM size: ' + IntToStr(DEMheader.NumCol) + 'x' + IntToStr(DEMheader.NumRow);
end;

function tDEMDataSet.DEMSizeString : shortstring;
var
   BytesPer : int64;
begin
   case DEMheader.DEMprecision of
      WordDEM,SmallIntDEM : BytesPer := 2;
      LongWordDEM,FloatingPointDEM : BytesPer := 4;
      ByteDEM          : BytesPer := 1;
   end;
   BytesPer := DEMheader.NumCol * DEMheader.NumRow * BytesPer;
   Result := SmartMemorySizeBytes(BytesPer);
end;


procedure GetSampleBoxSize(WhichDEM : integer; var BoxSize : integer);
begin
   ReadDefault('Sample full box size (m), grid spacing=' + RealToString(DEMGlb[WhichDEM].AverageXSpace,-12,-2),BoxSize);
end;


function RectSpacingFactor(DataSpacing : tSpacingUnit) : float64;
begin
    case DataSpacing of
       Space100m : result := 100;
       Space10m  : result := 10;
       SpaceFeet,
       SpaceMeters : result := 1;
       SpaceIntFeet : Result := 0.3048;
       SpaceUSFeet: Result := 1200/3937;
       else result := 1;
    end;
end;


function tDEMDataSet.SeaLevelCell(x,y : integer) : boolean;
var
   xl,yl,xh,yh : integer;
   zf : float32;
begin
   result := true;
   xh := succ(x);
   yh := succ(y);
   xl := pred(x);
   yl := pred(y);
   ClipDEMGrid(xl,yl);
   ClipDEMGrid(xh,yh);
   for x := xl to xh do begin
      for y := yl to yh do begin
         if GetElevMetersOnGrid(x,y,zf) and (abs(zf) > 0.00001) then begin
            Result := false;
            exit;
         end;
      end;
   end;
end;


function tDEMDataSet.TheShortDEMName : ShortString;
begin
   if (ShortName <> '') then Result := ShortName
   else Result := AreaName;
end;


constructor tDEMDataSet.Create;
var
   i : integer;
begin
   {$If Defined(RecordReadDEM)} if not DEMMergeInProgress then WriteLineToDebugFile('Create DEM ' + IntToStr(WhichDEM)); {$EndIf}
   ThisDEM := WhichDEM;
   ZeroDEMHeader(DEMheader, false);
   ElevationMultiple := 1;
   AverageGridTrue := 0;
   DSMGrid := 0;
   VATrelatedGrid := 0;
   FanBlowUpDEM := 0;
   FilterGrid := 0;
   FilterGridValue := 0;
   Z_Mean := -99;
   Z_Std := -99;
   Zpercens := Nil;
   HiddenGrid := false;

   DEMMapProjection := tMapProjection.Create('DEM=' + IntToStr(ThisDEM));
   DEMMapProjection.PName := UndefinedProj;
   DEMMapProjection.h_DatumCode := MDdef.PreferPrimaryDatum;

   GeotiffNWCornerX := NaN;
   GeotiffNWCornerY := NaN;

   DEMDatumShiftDone := false;
   DEMAlreadyDefined := false;
   UTMValidDEM := true;
   AreaName := '';
   ShortName := '';
   VATFileName := '';
   GeotiffImageDesc := '';
   DEMMetadata  := tStringList.Create;
   XSpaceByDEMrow := Nil;
   DiagSpaceByDEMrow := Nil;
   Normals := Nil;
   DEMMemoryAlreadyAllocated := false;
   VatLegendStrings := Nil;

   {$IfDef NoMapOptions}
   {$Else}
      SelectionMap := Nil;
   {$EndIf}

   {$IfDef ExNLCD}
   {$Else}
      NLCDCats := Nil;
   {$EndIf}

   {$IfDef ExVegDensity}
   {$Else}
      for i := 1 to 2 do begin
         VegDensityLayers[i] := Nil;
         VegGrid[i] := 0;
      end;
   {$EndIf}

   {$If Defined(RecordReadDEM)} if not DEMMergeInProgress then WriteLineToDebugFile('Created DEM OK '+ IntToStr(WhichDEM)); {$EndIf}
   {$If Defined(RecordDEMCreation)} WriteLineToDebugFile('Created DEM OK '+ IntToStr(WhichDEM)); {$EndIf}
end;


procedure tDEMDataSet.FreeDEMMemory;
var
   j    : integer;
begin
    if (DEMheader.DEMPrecision = FloatingPointDEM) then begin
       {$IfDef RecordCloseDEM} WriteLineToDebugFile('HeadRecs.DEMPrecision = FloatingPointDEM'); {$EndIf}
       if (ShortFloatElevations <> Nil) then begin
          for j := 0 to pred(DEMheader.NumCol) do FreeMem(ShortFloatElevations^[j],BytesPerColumn);
          Dispose(ShortFloatElevations);
          ShortFloatElevations := Nil;
          {$IfDef RecordCloseDEM} WriteLineToDebugFile('short floats cleared'); {$EndIf}
       end;
    end
    else if (DEMheader.DEMPrecision = ByteDEM) then begin
       {$IfDef RecordCloseDEM} WriteLineToDebugFile('HeadRecs.DEMPrecision = ByteDEM'); {$EndIf}
       if (ByteElevations <> Nil) then begin
          for j := 0 to pred(DEMheader.NumCol) do FreeMem(ByteElevations^[j],BytesPerColumn);
          Dispose(ByteElevations);
          ByteElevations := Nil;
          {$IfDef RecordCloseDEM} WriteLineToDebugFile('bytes cleared'); {$EndIf}
       end;
    end
    else if (DEMheader.DEMPrecision = WordDEM) then begin
       {$IfDef RecordCloseDEM} WriteLineToDebugFile('HeadRecs.DEMPrecision = WordDEM'); {$EndIf}
       if (WordElevations <> Nil) then begin
          for j := 0 to pred(DEMheader.NumCol) do FreeMem(WordElevations^[j],BytesPerColumn);
          Dispose(WordElevations);
          WordElevations := Nil;
          {$IfDef RecordCloseDEM} WriteLineToDebugFile('words cleared'); {$EndIf}
       end;
    end
    else if (DEMheader.DEMPrecision = LongWordDEM) then begin
       {$IfDef RecordCloseDEM} WriteLineToDebugFile('HeadRecs.DEMPrecision = LongWordDEM'); {$EndIf}
       if (LongWordElevations <> Nil) then begin
          for j := 0 to pred(DEMheader.NumCol) do FreeMem(LongWordElevations^[j],BytesPerColumn);
          Dispose(LongWordElevations);
          LongWordElevations := Nil;
          {$IfDef RecordCloseDEM} WriteLineToDebugFile('longwords cleared'); {$EndIf}
       end;
    end
    else begin
       {$IfDef RecordCloseDEM} WriteLineToDebugFile('HeadRecs.DEMPrecision = SmallIntDEM'); {$EndIf}
       if (SmallIntElevations <> Nil) then begin
          for j := 0 to pred(DEMheader.NumCol) do FreeMem(SmallIntElevations^[j],BytesPerColumn);
          Dispose(SmallIntElevations);
          SmallIntElevations := Nil;
          {$IfDef RecordCloseDEM} WriteLineToDebugFile('small ints cleared'); {$EndIf}
       end;
    end;
    if (XSpaceByDEMrow <> Nil) then FreeMem(XSpaceByDEMrow,4*DEMheader.NumRow);
    if (DiagSpaceByDEMrow <> Nil) then FreeMem(DiagSpaceByDEMrow,4*DEMheader.NumRow);
    CloseElevPercentiles;
    DisposeNormals;
    DEMMemoryAlreadyAllocated := false;
end;


destructor tDEMDataSet.Destroy;
var
   Action: TCloseAction;
begin
   {$If Defined(RecordClosing) or Defined(RecordDEMClose)} if (not DEMMergeInProgress) then WriteLineToDebugFile('tDEMDataSet.Destroy DEM ' + AreaName); {$EndIf}

   {$IfDef EXNLCD}
   {$Else}
      if (NLCDCats <> Nil) then begin
         try
            Dispose(NLCDCats);
            NLCDcats := Nil;
         except
            on Exception do begin
               {$If Defined(RecordClosing) or Defined(RecordDEMClose)}  WriteLineToDebugFile('Problem disposing NLCDcats'); {$EndIf}
            end;
         end;
      end;
   {$EndIf}

   if (DEMMetadata <> nil) then try
      if (DEMMetadata.Count > 0) then DEMmetadata.SaveToFile(MetadataFileName);
      DEMMetadata.Free;
   finally
      DEMMetadata := nil;
   end;

   if not DEMMapProjection.ProjectionSharedWithDataset then FreeAndNil(DEMMapProjection);
   FreeDEMMemory;
   FreeAndNil(VatLegendStrings);
   {$IfDef ExVegDensity}
   {$Else}
       CloseVegGrid(0);
       if (VegDensityLayers[1] <> Nil) then VegDensityLayers[1].Destroy;
       if (VegDensityLayers[2] <> Nil) then VegDensityLayers[2].Destroy;
   {$EndIf}

   {$IfDef NoMapOptions}
   {$Else}
       if CloseMap and Assigned(SelectionMap) then try
          {$If Defined(RecordClosing) or Defined(RecordDEMClose)}  WriteLineToDebugFile('tDEMDataSet.Destroy has selection map'): {$EndIf}
          SelectionMap.MapDraw.ClosingMapNow := true;
          SelectionMap.Closable := true;
          SelectionMap.FormClose(Nil,Action);
       finally
          SelectionMap := Nil;
          {$If Defined(RecordClosing) or Defined(RecordDEMClose)} WriteLineToDebugFile('tDEMDataSet.Destroy finished close selection map'); {$EndIf}
       end;
   {$EndIf}


   {$If Defined(RecordClosing) or Defined(RecordDEMClose)} if not DEMMergeInProgress then WriteLineToDebugFile('tDEMDataSet.Destroy done ' + AreaName); {$EndIf}
end;



{********************** Elevation Routines *********************************}


function tDEMDataSet.ValidNeighborsInBox(XGrid,YGrid,Size : integer) :  integer;
var
   x,y : integer;
  z : float32;
begin
   Result := 0;
   for x := -Size to Size do
      for y := -Size to Size do
         if GridInDataSet(XGrid+x,YGrid+y) and (GetElevMeters(XGrid+x,YGrid+y,z)) then inc(Result);
end;


function tDEMDataSet.ImmediateNeighborsMissing(XGrid, YGrid: integer): integer;
begin
    Result := -1;
    if GridInDataSet(XGrid,YGrid) then begin
       Result := 0;
       if MissingDataInGrid(pred(XGrid), pred(YGrid)) then inc(Result);
       if MissingDataInGrid(XGrid,pred(YGrid)) then inc(Result);
       if MissingDataInGrid(pred(XGrid),YGrid) then inc(Result);
       if MissingDataInGrid(succ(XGrid),succ(YGrid)) then inc(Result);
       if MissingDataInGrid(XGrid,succ(YGrid)) then inc(Result);
       if MissingDataInGrid(succ(XGrid),YGrid) then inc(Result);
       if MissingDataInGrid(pred(XGrid),succ(YGrid)) then inc(Result);
       if MissingDataInGrid(succ(XGrid),pred(YGrid)) then inc(Result);
    end;
end;

function tDEMDataSet.ImmediateNeighborsSameCode(XGrid, YGrid: integer): integer;
var
   z,z2 : float32;
begin
   Result := 0;
   if GetElevMetersOnGrid(Xgrid,YGrid,z) then begin
       if GetElevMetersOnGrid(pred(XGrid),pred(YGrid),z2) and (abs(z-z2) < 0.01) then inc(Result);
       if GetElevMetersOnGrid(XGrid, pred(YGrid),z2) and (abs(z-z2) < 0.01) then inc(Result);
       if GetElevMetersOnGrid(pred(XGrid),YGrid,z2) and (abs(z-z2) < 0.01) then inc(Result);
       if GetElevMetersOnGrid(succ(XGrid),succ(YGrid),z2) and (abs(z-z2) < 0.01) then inc(Result);
       if GetElevMetersOnGrid(XGrid, succ(YGrid),z2) and (abs(z-z2) < 0.01) then inc(Result);
       if GetElevMetersOnGrid(succ(XGrid),YGrid,z2) and (abs(z-z2) < 0.01) then inc(Result);
       if GetElevMetersOnGrid(pred(XGrid),succ(YGrid),z2) and (abs(z-z2) < 0.01) then inc(Result);
       if GetElevMetersOnGrid(succ(XGrid),pred(YGrid),z2) and (abs(z-z2) < 0.01) then inc(Result);
   end;
end;

procedure tDEMDataSet.IncrementGridValue(Col,Row : integer);
var
   z : float32;
begin
   if GridInDataSet(Col,Row) then begin
      if MissingDataInGrid(Col,Row) then begin
         SetGridElevation(Col,Row,1);
      end
      else if GetElevMetersOnGrid(Col,Row,Z) then begin
         SetGridElevation(Col,Row,Succ(round(z)));
      end;
   end;
end;


function tDEMDataSet.GetElevMetersFromThisAndSecondDEM(Dem2,Col,Row : integer; var z1,z2  : float32) : boolean;
//added 30 Oct 22 when GetElevMetersFromSecondDEM ran much too slowly for one DEM in a series
var
   Lat,Long : float64;
begin
   Result := GetElevMeters(Col,Row,z1);
   if Result then begin
      DEMGridToLatLongDegree(Col,Row,Lat,Long);
      Result := DEMGlb[DEM2].GetElevFromLatLongDegree(Lat,Long, z2);
   end;
end;


function tDEMDataSet.GetElevMetersFromSecondDEM(IdenticalGrids : boolean; Dem2,Col,Row : integer; var z  : float32) : boolean;
var
   Lat,Long : float64;
begin
   if IdenticalGrids then Result := DEMGlb[DEM2].GetElevMeters(Col,Row,z)
   else begin
      DEMGridToLatLongDegree(Col,Row,Lat,Long);
      Result := DEMGlb[DEM2].GetElevFromLatLongDegree(Lat,Long, z);
      {$IfDef RecordZ2ndDEM}
         if not Result then begin
            Result := DEMGlb[DEM2].GetElevFromLatLongDegree(Lat,Long, z);
         end;
      {$EndIf}
   end;
end;


function tDEMDataSet.GetSlopeAspectFromSecondDEM(Dem2,Col,Row : integer; var SlopeAspectRec : tSlopeAspectRec) : boolean;
var
   Lat,Long : float64;
   Colf,Rowf : float32;
begin
   if SecondGridIdentical(DEM2) then Result := DEMGlb[DEM2].GetSlopeAndAspect(Col,Row,SlopeAspectRec)
   else begin
      DEMGridToLatLongDegree(Col,Row,Lat,Long);
      DEMGlb[DEM2].LatLongDegreeToDEMGrid(Lat,Long,Colf,Rowf);
      Result := DEMGlb[DEM2].GetSlopeAndAspect(round(Colf),round(Rowf), SlopeAspectRec);
   end;
end;


function tDEMDataSet.RGBfromLongWord(x,y : integer; var r,g,b : byte) : boolean;
var
   zi : LongWord;
begin
   Result := GridInDataSet(x,y);
   if Result then begin
      zi := LongWordElevations^[x]^[y];
      r := zi mod 256;
      zi := zi div 256;
      g := zi mod 256;
      zi := zi div 256;
      b := zi mod 256;
   end;
end;


function tDEMDataSet.FigureOpenness(Col,Row,RegionSizeMeters : integer; var  Upward,Downward : float64; Findings : tStringList = Nil) : boolean;
var
   UpAngle,DownAngle,zp : float32;
   RaysUsed : integer;

   procedure OneDirection(dx,dy : integer; Spacing : float64; Dir : ShortString);
   var
      TanAngle,MinTanAngle,MaxTanAngle,PointElev : float32;
      i : integer;
      FoundOne : boolean;
   begin
      MaxTanAngle := -999;
      MinTanAngle := 999;
      FoundOne := false;
      For i := 1 to round(RegionSizeMeters / Spacing) do begin
         if GetElevMeters(Col + i * dx,Row + i * dy,PointElev) then begin
            TanAngle := (PointElev - zp) / (i * Spacing);
            CompareValueToExtremes(TanAngle,MinTanAngle,MaxTanAngle);
            FoundOne := true;
         end;
      end {i loop};
      if FoundOne then begin
         if (Findings <> Nil) then begin
            Findings.Add(Dir + RealToString(90 - ArcTan(MaxTanAngle) / DegToRad,16,2) + DegSym + RealToString(90 + ArcTan(MinTanAngle) / DegToRad,18,2) + DegSym);
         end;
         UpAngle := UpAngle + MaxTanAngle;
         DownAngle := DownAngle + MinTanAngle;
         inc(RaysUsed);
      end;
   end;

begin
   Result := IsSurroundedPoint(Col,Row);
   if Not Result then exit;
   RaysUsed := 0;
   UpAngle := 0;
   DownAngle := 0;
   GetElevMeters(Col,Row,zp);
   case MDDef.OpennessHt of
       opAboveGround    : zp := zp + MDDef.OpennessHowHigh;
       opConstantHeight : zp := MDDef.OpennessHowHigh;
   end;

   if (Findings <> Nil) then begin
      Findings.Add('Direction Upward Openness   Downward Openness');
   end;

   if MDDef.OpennessDirs[1] then OneDirection(0,1,AverageYSpace,'N ');
   if MDDef.OpennessDirs[2] then OneDirection(1,1,AverageDiaSpace,'NE');
   if MDDef.OpennessDirs[3] then OneDirection(1,0,AverageXSpace,'E ');
   if MDDef.OpennessDirs[4] then OneDirection(1,-1,AverageDiaSpace,'SE');
   if MDDef.OpennessDirs[5] then OneDirection(0,-1,AverageYSpace,'S ');
   if MDDef.OpennessDirs[6] then OneDirection(-1,-1,AverageDiaSpace,'SW');
   if MDDef.OpennessDirs[7] then OneDirection(-1,0,AverageXSpace,'W ');
   if MDDef.OpennessDirs[8] then OneDirection(-1,1,AverageDiaSpace,'NW');

   Upward := 90 - ArcTan(UpAngle / RaysUsed) / DegToRad;
   Downward := 90 + ArcTan(DownAngle / RaysUsed) / DegToRad;
end;


function tDEMDataSet.LakePoint(X,Y : integer) : boolean;
var
   znw,zw,zsw,zn,z,zs,zne,ze,zse : float32;
begin
   Result := SurroundedPointElevs(x,y,znw,zw,zsw,zn,z,zs,zne,ze,zse) and (z <> 0) and (z=znw) and (z=zw) and (z=zsw) and (z=zn) and (z=zs) and (z=zne) and (z=ze) and (z=zse);
end;


function tDEMDataSet.LandCoverGrid : boolean;
begin
   Result := DEMheader.ElevUnits in [NLCD2001up,LandFire,NLCD1992,GLOBCOVER,GLC2000,CCAP,CCI_LC,S2GLC,NLCD_Change,GLCS_LC100,Meybeck,Geomorphon,Iwahashi,ESRI2020,euPennock,WorldCover10m,LCMAP,euSent2SLC];
end;

function tDEMDataSet.ElevationGrid : boolean;
begin
   Result := DEMheader.ElevUnits in [euMeters,Feet,Decimeters,DeciFeet,Centimeters];
end;


function tDEMDataSet.MinElevAroundPoint(xgrid,ygrid : integer; var z : float32) : boolean;
var
   i : integer;
   zs : array[1..9] of float32;
begin
   Result := SurroundedPointElevs(xgrid,ygrid,zs[1],zs[2],zs[3],zs[4],zs[5],zs[6],zs[7],zs[8],zs[9]);
   if Result then begin
      z := zs[1];
      for i := 2 to 9 do if (zs[i] < z) then z := zs[i];
   end;
end;


function tDEMDataSet.MaxElevAroundPoint(xgrid,ygrid : integer;  var z : float32) : boolean;
var
   i : integer;
   zs : array[1..9] of float32;
begin
   Result := SurroundedPointElevs(xgrid,ygrid,zs[1],zs[2],zs[3],zs[4],zs[5],zs[6],zs[7],zs[8],zs[9]);
   if Result then begin
      z := zs[1];
      for i := 1 to 9 do if (zs[i] > z) then z := zs[i];
   end;
end;


function tDEMDataSet.GetElevFromLatLongDegree(Lat,Long : float64; var z : float32) : boolean;
var
   xg,yg : float32;
begin
   Result := LatLongDegreeToDEMGrid(Lat,Long,xg,yg);
   if Result then Result := GetElevMeters(xg,yg,z)
   else z := 0;
end;

function tDEMDataSet.GetElevFromUTM(x,y : float64; var z : float32) : boolean;
var
   Lat,Long : float64;
begin
   UTMtoLatLongDegree(x,y,Lat,Long);
   Result := GetElevFromLatLongDegree(Lat,Long,z);
end;

procedure tDEMDataSet.GetElevCol(XGrid : integer; var z : pSmallIntCol);
begin
   if (SmallIntElevations[XGrid] <> Nil) and (XGrid >= 0) and (XGrid <= pred(DEMheader.NumCol)) then Move(SmallIntElevations[XGrid]^,z^,BytesPerColumn);
end {proc GetElevCol};

procedure tDEMDataSet.GetFloatElevCol(XGrid: integer; var z : pShortFloatCol); {returns an entire column of elevations, needed to pass to contouring routine}
begin
  if (ShortFloatElevations[XGrid] <> Nil) and (XGrid >= 0) and (XGrid <= pred(DEMheader.NumCol)) then
     Move(ShortFloatElevations[XGrid]^,z^,BytesPerColumn);
end {proc GetElevCol};



function tDEMDataSet.sfBoundBox2tGridLimits(sfBoundBox : sfboundBox) : tGridLimits;
var
   xlo,xhi,ylo,yhi,xtra : float32;
begin
   {$IfDef RecordsfBoundBox2tGridLimits} HighlightLineToDebugFile('sfBoundBox2tGridLimits: ' + AreaName);  WriteLineToDebugFile('Geo: ' + sfBoundBoxToString(sfBoundBox));   {$EndIf}
   LatLongDegreeToDEMGrid(sfBoundBox.YMin,sfBoundBox.XMin,xlo,ylo);
   LatLongDegreeToDEMGrid(sfBoundBox.YMax,sfBoundBox.XMax,xhi,yhi);
   {$IfDef RecordsfBoundBox2tGridLimits} WriteLineToDebugFile('DEM Grid, x=' + RealToString(xlo,-12,-2) + ' to ' + RealToString(xhi,-12,-2) +  '  y=' + RealToString(ylo,-12,-2) + ' to ' + RealToString(yhi,-12,-2) ); {$EndIf}
   ClipDEMGrid(xlo,ylo);
   ClipDEMGrid(xhi,yhi);
   {$IfDef RecordsfBoundBox2tGridLimits} WriteLineToDebugFile('clipped, x=' + RealToString(xlo,-12,-2) + ' to ' + RealToString(xhi,-12,-2) +  '  y=' + RealToString(ylo,-12,-2) + ' to ' + RealToString(yhi,-12,-2) ); {$EndIf}
   xtra := 0.00;  //0.0001;
   Result.XGridLow := round(xlo-xtra);
   Result.YGridLow := round(ylo-xtra);
   Result.XGridHigh := round(xhi+xtra);
   Result.YGridHigh := round(yhi+xtra);
   {$IfDef RecordsfBoundBox2tGridLimits}
      WriteLineToDebugFile('Final: ' + GridLimitsToString(Result) + 'Rows=' + IntToStr((succ(Result.XGridHigh-Result.XGridLow))) +
         'x' + IntToStr(succ(Result.YGridHigh-Result.YGridLow)));
   {$EndIf}
end;


function tDEMDataSet.bbDEMGridPartOfDEMonMap(BaseMap : tMapForm) : sfBoundBox;
begin
   LatLongDegreeToDEMGrid(BaseMap.MapDraw.MapCorners.BoundBoxGeo.YMin,BaseMap.MapDraw.MapCorners.BoundBoxGeo.XMin,Result.XMin,Result.YMin);
   LatLongDegreeToDEMGrid(BaseMap.MapDraw.MapCorners.BoundBoxGeo.YMax,BaseMap.MapDraw.MapCorners.BoundBoxGeo.XMax,Result.XMax,Result.YMax);
   //removed 9/10/2023
   //LatLongDegreeToDEMGrid(BaseMap.MapDraw.MapCorners.BoundBoxGeo.YMin,BaseMap.MapDraw.MapCorners.BoundBoxGeo.XMax,Result.XMax,Result.YMin);
   //LatLongDegreeToDEMGrid(BaseMap.MapDraw.MapCorners.BoundBoxGeo.YMax,BaseMap.MapDraw.MapCorners.BoundBoxGeo.XMin,Result.XMin,Result.YMax);
end;


function tDEMDataSet.PixelBoundBoxGeo(Col,Row : integer) : sfBoundBox;
var
   Lat,Long : float64;
begin
   DEMGridToLatLongDegree(Col,Row,Lat,Long);
   //if (DEMHeader.RasterPixelIsGeoKey1025 = PixelIsPoint) then begin
      Result.xMax := Long + 0.5 * DEMHeader.DEMxSpacing;
      Result.xMin := Long - 0.5 * DEMHeader.DEMxSpacing;
      Result.yMax := Lat + 0.5 * DEMHeader.DEMySpacing;
      Result.yMin := Lat - 0.5 * DEMHeader.DEMySpacing;
   (*end
   else begin
      Result.xMax := Long + DEMHeader.DEMxSpacing;
      Result.xMin := Long;
      Result.yMax := Lat;
      Result.yMin := Lat - DEMHeader.DEMySpacing;
   end;
   *)
end;


function tDEMDataSet.PixelBoundBoxUTM(Col,Row : integer) : sfBoundBox;
var
   xutm,yutm : float64;
begin
   DEMGridToUTM(Col,Row,xutm,yutm);
   Result.xMax := xutm + 0.5 * DEMHeader.DEMxSpacing;
   Result.xMin := xutm - 0.5 * DEMHeader.DEMxSpacing;
   Result.yMax := yutm + 0.5 * DEMHeader.DEMySpacing;
   Result.yMin := yutm - 0.5 * DEMHeader.DEMySpacing;
end;


function tDEMDataSet.ResaveNewResolution(FilterCategory : tFilterCat) : integer;
var
   Col,Row : integer;
   z1 : float32;
   Title         : Shortstring;
   NewHeadRecs   : tDEMheader;
begin
   NewHeadRecs := DEMheader;
   if (FilterCategory = fcSaveByte) then NewHeadRecs.DEMPrecision := ByteDEM
   else if (FilterCategory = fcSaveSmallInt) then NewHeadRecs.DEMPrecision := SmallIntDEM
   else if (FilterCategory in [fcSaveWord]) then NewHeadRecs.DEMPrecision := WordDEM
   else if (FilterCategory = fcSaveFloatingPoint) then NewHeadRecs.DEMPrecision := FloatingPointDEM;

   case FilterCategory of
      fcSaveByte : Title  := 'Byte';
      fcSaveSmallInt : Title  := 'Smallint';
      fcSaveWord : Title  := 'Word';
      fcSaveFloatingPoint : Title  := 'float32';
   end;

   OpenAndZeroNewDEM(true,NewHeadRecs,Result,Title + '_' + AreaName,InitDEMmissing);
   StartProgress(DEMGlb[Result].AreaName);
   for Row := 0 to pred(DEMGlb[Result].DEMheader.NumRow) do begin
      if (Col mod 100 = 0) then UpdateProgressBar(Col / pred(DEMGlb[Result].DEMheader.NumRow));
      for Col := 0 to pred(DEMGlb[Result].DEMheader.NumCol) do begin
         if GetElevMeters(Col,Row,z1) then DEMGlb[Result].SetGridElevation(Col,Row,z1);
      end;
   end {for};

   DEMGlb[Result].CheckMaxMinElev;
   EndProgress;
   //if (Removed > 0) then MessageToContinue('Points removed: ' + IntToStr(Removed));
end;


function tDEMDataSet.MetadataFileName: PathStr;
begin
   Result := ChangeFileExt(Self.DEMFileName,'_metadata.txt');
end;


procedure tDEMDataSet.FindEdgeThisDEM(var NewDEM : integer; Dir : tCompassDirection);
const
   MaxSize = 1000;
var
   Col,Row,Gap,Edges : integer;
   OnRun : boolean;
   z : float32;
   TStr : shortString;

       procedure DoRun(StartCol,StartRow,ColInc,RowInc : integer);
       var
          Col,Row : integer;
       begin
           Col := StartCol;
           Row := StartRow;
           Gap := 0;
           OnRun := false;
           while GridInDataSet(Col,Row) do begin
              if GetElevMeters(Col,Row,z) then  begin
                 if OnRun then begin
                 end
                 else begin
                    OnRun := true;
                    DEMGlb[NewDEM].SetGridElevation(Col,Row,1);
                    inc(Edges);
                 end;
              end
              else begin
                 if OnRun then inc(Gap);
                 if (Gap > MDDef.EdgeFilterGap) then begin
                     Gap := 0;
                     OnRun := false;
                 end;
              end;
              inc(Col,ColInc);
              inc(Row,RowInc);
           end;
           {$IfDef RecordFilter} WriteLineToDebugFile('col=' + IntToStr(StartCol) + '  row=' + IntToStr(StartRow) + '    edges=' + IntToStr(Edges)); {$EndIf}
         end;


begin
   ReadDefault('Gap size allowed (grid posts)',MDDef.EdgeFilterGap);
   case Dir of
     cdN: Tstr := 'north';
     cdNE: Tstr := 'northeast';
     cdE: Tstr := 'east';
     cdSE: Tstr := 'southeast';
     cdS: Tstr := 'south';
     cdSW: Tstr := 'southwest';
     cdW: Tstr := 'west';
     cdNW: Tstr := 'northwest';
   end;
   {$IfDef RecordFilter} WriteLineToDebugFile('tDEMDataSet.FindEdgeThisDEM in, dir=' + TStr): {$EndIf}

   NewDEM := CloneAndOpenGridSetMissing(ByteDEM,AreaName + TStr + '_edge',Undefined);

   DEMGlb[NewDEM].AreaName := TStr + ' edge (gap size = ' + IntToStr(MDDef.EdgeFilterGap) + ') ' + AreaName;
   StartProgress('Edge filter');
   Edges := 0;
   case Dir of
      cdE:  for Row := 0 to pred(DEMGlb[NewDEM].DEMheader.NumRow) do begin
               UpdateProgressBar(Row/DEMGlb[NewDEM].DEMheader.NumRow);
               DoRun(pred(DEMGlb[NewDEM].DEMheader.NumCol),Row,-1,0);
            end;
      cdSW: begin
              for Row := 0 to pred(DEMGlb[NewDEM].DEMheader.NumRow) do begin
                 UpdateProgressBar(Row/(DEMGlb[NewDEM].DEMheader.NumRow + DEMGlb[NewDEM].DEMheader.NumCol));
                 DoRun(0,Row,1,1);
              end;
              for Col := 1 to pred(DEMGlb[NewDEM].DEMheader.NumCol) do begin
                 UpdateProgressBar((Col + DEMGlb[NewDEM].DEMheader.NumRow)/(DEMGlb[NewDEM].DEMheader.NumRow + DEMGlb[NewDEM].DEMheader.NumCol));
                 DoRun(Col,0,1,1);
              end;
            end;
      cdW: for Row := 0 to pred(DEMGlb[NewDEM].DEMheader.NumRow) do begin
               UpdateProgressBar(Row/DEMGlb[NewDEM].DEMheader.NumRow);
               DoRun(0,Row,1,0);
           end;
    end;

   DEMGlb[NewDEM].CheckMaxMinElev;
   EndProgress;
   {$IfDef RecordFilter} WriteLineToDebugFile('tDEMDataSet.FindEdgeThisDEM out'); {$EndIf}
end;


procedure tDEMDataSet.BoxAreaExtremeElevations(Limits : tGridLimits; var TheMinElev,TheMaxElev,AvgElev : float32);
var
   x,y  : integer;
   NPts : int64;
   z    : float32;
   zsum : float64;
begin
   MinOfPairFirst(Limits.YGridLow,Limits.YGridHigh);
   ClipDEMGrid(Limits.XGridLow,Limits.YGridLow);
   ClipDEMGrid(Limits.XGridHigh,Limits.YGridHigh);
   TheMinElev := MaxSmallInt;
   TheMaxElev := -MaxSmallInt;
   zsum := 0;
   Npts := 0;
   for x := Limits.XGridLow to Limits.XGridHigh do begin
      for y := Limits.YGridLow to Limits.YGridHigh do begin
         if GetElevMeters(x,y,z) then begin
            CompareValueToExtremes(z,TheMinElev,TheMaxElev);
            zsum := zsum + z;
            inc(Npts);
         end;
      end {for y};
   end {for x};
   if (NPts > 1) then AvgElev := zsum / NPts;
   {$IfDef RecordMinMax} WriteLineToDebugFile('tDEMDataSet.BoxAreaExtremeElevations: ' + RealToString(TheMinElev,-12,-2) + ' to ' + RealToString(TheMaxElev,-12,-2)); {$EndIf}
end {proc AreaExtremeElevations};


function tDEMDataSet.SpecifyDEMGridLimitsFromLatLong(LatLow,LongLow,LatHi,LongHi : float64) :  tGridLimits;
begin
   LatLongDegreeToDEMGridInteger(LatLow,LongLow, Result.XGridLow, Result.YGridLow);
   LatLongDegreeToDEMGridInteger(LatHi,LongHi, Result.XGridHigh, Result.YGridHigh);
end;

procedure tDEMDataSet.AreaExtremeElevationsFromLatLong(LatLow,LongLow,LatHi,LongHigh : float64; var TheMinElev,TheMaxElev : float32);
var
   Limits : tGridLimits;
   avgelev : float32;
begin
   Limits := SpecifyDEMGridLimitsFromLatLong(LatLow,LongLow,LatHi,LongHigh);
   BoxAreaExtremeElevations(Limits,TheMinElev,TheMaxElev,avgelev);
end;


procedure tDEMDataSet.SetElevationMultiple;
begin
  case DEMheader.ElevUnits of
      Decimeters  : ElevationMultiple := 0.1;
      HundredthMa,
      Centimeters : ElevationMultiple := 0.01;
      DeciFeet    : ElevationMultiple := 0.1 * FeetToMeters;
      Feet        : ElevationMultiple := FeetToMeters;
      else ElevationMultiple := 1;
   end;
   {$IfDef RecordMinMax} WriteLineToDebugFile('tDEMDataSet.SetElevationMultiple ' + RealToString(ElevationMultiple,-12,-2)); {$EndIf}
end;


{$IfDef ExNLCD}
{$Else}
procedure tDEMDataSet.CheckForLandCover;
var
   TStr : shortstring;
begin
  {$IfDef RecordNLCD} WriteLineToDebugFile('tDEMDataSet.CheckForLandCover in'); {$EndIf}
   if (LandCoverGrid) then begin
      if (NLCDCats <> Nil) then begin
         Dispose(NLCDCats);
         NLCDCats := nil;
      end;
      New(NLCDCats);
      if (DEMheader.ElevUnits in [GLC2000]) then TStr := 'GLC-2000'
      else if (DEMheader.ElevUnits in [GLCS_LC100]) then TStr := 'GLCS-LC100'
      else if (DEMheader.ElevUnits in [S2GLC]) then TStr := 'S2GLC'
      else if (DEMheader.ElevUnits in [GLOBCOVER]) then TStr := 'GlobCover'
      else if (DEMheader.ElevUnits in [CCI_LC]) then TStr := 'CCI-LC'
      else if (DEMheader.ElevUnits in [NLCD2001up]) then TStr := 'NLCD-2001up'
      else if (DEMheader.ElevUnits in [NLCD1992]) then TStr := 'NLCD-1990'
      else if (DEMheader.ElevUnits in [WORLDCover10m]) then TStr := 'WORLDCOVER10M'
      else if (DEMheader.ElevUnits in [CCAP]) then TStr := 'C-CAP'
      else if (DEMheader.ElevUnits in [Meybeck]) then TStr := 'MEYBECK'
      else if (DEMheader.ElevUnits in [ESRI2020]) then TStr := 'ESRI2020'
      else if (DEMheader.ElevUnits in [Iwahashi]) then TStr := 'IWAHASHI'
      else if (DEMheader.ElevUnits in [Geomorphon]) then TStr := 'GEOMORPHON'
      else if (DEMheader.ElevUnits in [euPennock]) then TStr := 'PENNOCK'
      else if (DEMheader.ElevUnits in [LCMAP]) then TStr := 'LCMAP'
      else if (DEMheader.ElevUnits in [euSent2SLC]) then TStr := 'Sent-2_SLC'
      else if (DEMheader.ElevUnits in [NLCD_Change]) then TStr := 'NLCD-Change'
      else TStr := 'LANDFIRE';                                               //this is in the DB for the filter
      //if not StrUtils.AnsiContainsText(UpperCase(AreaName),UpperCase(TStr)) then AreaName := AreaName + ' ' + TStr;
      {$IfDef RecordNLCD} WriteLineToDebugFile('tDEMDataSet.CheckForLandCover with NLCD=' + TStr); {$EndIf}
      DEM_NLCD.SetUpNLCDCategories(false,TStr,NLCDCats^);
   end;
  {$IfDef RecordNLCD} WriteLineToDebugFile('tDEMDataSet.CheckForLandCover out'); {$EndIf}
end;
{$EndIf}



procedure tDEMDataSet.AssignProjectionFromDEM(var MapProjection : tMapProjection; DebugName : shortstring);
begin
   {$IfDef RecordCreateNewDEM} WriteLineToDebugFile('tDEMDataSet.AssignProjectionFromDEM in, projection=' + MapProjection.GetProjectionName); {$EndIf}
   MapProjection.projUTMZone := DEMheader.UTMZone;
   MapProjection.LatHemi := DEMheader.LatHemi;
   if (DEMheader.wktString <> '') then begin
      {$IfDef RecordProjectionParameters} MapProjection.ProjectionParamsToDebugFile('tDEMDataSet.AssignProjectionFromDEM, wkt=' + DEMheader.wktString); {$EndIf}
      MapProjection.DecodeWKTProjectionFromString(DEMheader.wktString);
   end
   else if DEMheader.DEMUsed in [ArcSecDEM] then MapProjection.PName := PlateCaree
   else if (DEMheader.DEMUsed = UTMBasedDEM) then begin
      MapProjection.PName := UTMellipsoidal;
      MapProjection.DefineDatumFromUTMZone(MapProjection.h_DatumCode,DEMheader.UTMZone,DEMMapProjection.LatHemi,'tDEMDataSet.DefineDEMVariables');
      MapProjection.StartUTMProjection(DEMheader.UTMZone);
   end;
   MapProjection.ProjDebugName := DebugName;
   {$IfDef RecordProjectionParameters} MapProjection.ProjectionParamsToDebugFile('tDEMDataSet.AssignProjectionFromDEM, after definition'); {$EndIf}
   {$IfDef RecordCreateNewDEM} WriteLineToDebugFile('tDEMDataSet.AssignProjectionFromDEM out, projection=' + MapProjection.GetProjectionName); {$EndIf}
end;




procedure tDEMDataSet.DefineDEMVariables(TransformToPreferDatum : boolean);
var
   l1,l2,xutm1,yutm1 : float64;
   TStr : shortstring;


      procedure InitializeDatum(Transform : boolean);

            procedure DoDatumShift;
            var
               DigitizeDatumConstants : tMapProjection;
            begin
               DEMDatumShiftDone := true;
               if WGSEquivalentDatum(DEMMapProjection.h_DatumCode) and WGSEquivalentDatum(MDdef.PreferPrimaryDatum) then exit;
               if (DEMMapProjection.h_DatumCode <> MDdef.PreferPrimaryDatum) then begin
                  DigitizeDatumConstants := tMapProjection.Create('digitize datum <> preferred');
                 {$IfDef RecordDefineDatum} WriteLineToDebugFile('initialize datum 5, DEM not on preferred datum'); {$EndIf}

                  if DEMheader.DigitizeDatum in [WGS72d,NAD27d,NAD83d,WGS84d,UK_OS_grid] then begin
                     TStr := StringFromDatumCode(DEMheader.DigitizeDatum);
                     DigitizeDatumConstants.DefineDatumFromUTMZone(TStr,DEMheader.UTMZone,DEMHeader.LatHemi,'tDEMDataSet.DefineDEMVariables DigDatum');
                     {$If Defined(RecordRedaDEM) or Defined(RecordDefineDatum)} WriteLineToDebugFile('DEM Datum transformed, Base was ' + LatLongDegreeToString(DEMSWcornerLat,DEMSWcornerLong)); {$EndIf}
                     MolodenskiyTransformation(DEMSWcornerLat,DEMSWcornerLong,DEMSWcornerLat,DEMSWcornerLong,DigitizeDatumConstants,DEMMapProjection);
                     {$If Defined(RecordReadDEM) or Defined(RecordDefineDatum)}
                        WriteLineToDebugFile('DEM Datum transformed, from ' + DigitizeDatumConstants.h_DatumCode + ' to ' + MDdef.PreferPrimaryDatum);
                        WriteLineToDebugFile('DEM Datum transformed, Base Now ' + LatLongDegreeToString(DEMSWcornerLat,DEMSWcornerLong));
                     {$EndIf}
                  end;

                  if (DEMheader.DEMUsed = UTMBasedDEM) then begin
                     DEMMapProjection.ForwardProjectDegrees(DEMSWcornerLat,DEMSWcornerLong,XUTM1,YUTM1);
                     DEMheader.DEMSWCornerX := XUTM1;
                     DEMheader.DEMSWCornerY := YUTM1;
                  end;
                  if (DEMheader.DEMUsed = ArcSecDEM) then begin
                     DEMheader.DEMSWCornerX := DEMSWcornerLong;
                     DEMheader.DEMSWCornerY := DEMSWcornerLat;
                  end;
                  StringToByteArray(MDdef.PreferPrimaryDatum, DEMheader.DMAMapDefinition.h_DatumCode);
                  DigitizeDatumConstants.Destroy;
               end;
            end;


      begin {proc InitializeDatum}
         {$IfDef RecordDefineDatum} WriteLineToDebugFile('InitializeDatum enter for DEM, proj=' + DEMMapProjection.GetProjectionName); {$EndIf}
         if (DEMheader.DigitizeDatum in [Rectangular]) then exit;

         if (DEMheader.WKTString <> '') then begin
            DEMMapProjection.DecodeWKTProjectionFromString(DEMheader.WKTString);
            DEMMapProjection.InverseProjectDegrees(DEMheader.DEMSWCornerX,DEMheader.DEMSWCornerY,DEMSWcornerLat,DEMSWcornerLong);
         end
         else if (DEMheader.DigitizeDatum in [Spherical{,unusedLamAzEqAreaSphere}]) then begin
            Transform := false;
            if (DEMheader.DEMUsed = UTMBasedDEM) then begin
               UTMToLatLongDegree(DEMheader.DEMSWCornerX,DEMheader.DEMSWCornerY,DEMSWcornerLat,DEMSWcornerLong);
            end
            else begin //Arc second DEM
               DEMSWcornerLat := DEMheader.DEMSWCornerY;
               DEMSWcornerLong := DEMheader.DEMSWCornerX;
               LongitudeAngleInRange(DEMSWcornerLong);
               DEMMapProjection.pName := PlateCaree;
               DEMMapProjection.Long0 := DegToRad * (DEMSWcornerLong + 0.5 * LongSizeMap);
               DEMMapProjection.Lat0 := 0.0;
               DEMMapProjection.ProjMapScale := 1.0;
               DEMMapProjection.a := 6378206.4;
            end {if};
         end
         else begin
           CheckForUTMZones;
           {$IfDef RecordDefineDatum} WriteLineToDebugFile('initialize datum 2, HeadRecs.UTMZone=' + IntToStr(DEMHeader.UTMZone) + ' proj=' + DEMMapProjection.GetProjectionName); {$EndIf}
           if (DEMMapProjection.PName = UTMEllipsoidal) then begin
               UTMtoLatLongDegree(DEMheader.DEMSWCornerX,DEMheader.DEMSWCornerY,DEMSWcornerLat,DEMSWcornerLong);
            end
            else if (DEMMapProjection.PName = PlateCaree) then begin
               {$IfDef RecordDefineDatum} WriteLineToDebugFile('initialize datum 4 (Lat/long)'); {$EndIf}
               DEMSWcornerLat := DEMheader.DEMSWCornerY;
               DEMSWcornerLong := DEMheader.DEMSWCornerX;
               if (DEMSWcornerLong > 180) then DEMSWcornerLong := DEMSWcornerLong - 360;
            end
            else begin
               DEMMapProjection.InverseProjectDegrees(DEMheader.DEMSWCornerX,DEMheader.DEMSWCornerY,DEMSWcornerLat,DEMSWcornerLong);
            end;
         end {if};

         if Transform and (not DEMDatumShiftDone) then begin
            //DoDatumShift;
         end;
         SetRasterPixelIsGeoKey1025(true);
         {$IfDef RecordDefineDatum} WriteLineToDebugFile('InitializeDatum exit, , proj=' + DEMMapProjection.GetProjectionName); {$EndIf}
      end {proc InitializeDatum};


     procedure DoSpacingCalcs;
     var
        i : integer;
     begin
        AverageXSpace := DEMheader.DEMxSpacing;
        AverageYSpace := DEMheader.DEMySpacing;
        XUTM1 := DEMheader.DEMSWCornerX + pred(DEMheader.NumCol) * AverageXSpace;
        YUTM1 := DEMheader.DEMSWCornerY + pred(DEMheader.NumRow) * AverageYSpace;
        UTMtoLatLongDegree(XUTM1,YUTM1,l1,l2);
        LatSizeMap := l1-DEMSWcornerLat;
        LongSizeMap := l2 - DEMSWcornerLong;;
        for i := 0 to Pred(DEMheader.NumRow) do begin   //valid for UTM and other projected DEMs
           XSpaceByDEMrow^[i] := AverageXSpace;
           DiagSpaceByDEMrow^[i] := sqrt(sqr(AverageXSpace) + sqr(AverageYSpace));
        end;
     end;

     procedure LatLongMapSpacing;
      var
         Lat,Long1,Long2,Distance,Bearing,Sum : float64;
         i : integer;
      begin
         {$IfDef RecordLatSpacingValues} WriteLineToDebugFile('tDEMDataSet.LatLongMapSpacing in SW corner of map ' + LatLongDegreeToString(BaseLat,BaseLong)); {$EndIf}
         LatSizeMap := pred(DEMheader.NumRow) * DEMheader.DEMySpacing;
         LongSizeMap := pred(DEMheader.NumCol) * DEMheader.DEMxSpacing;
         Long1 := DEMheader.DEMSWCornerX + Pred(DEMheader.NumRow div 2) * DEMheader.DEMxSpacing;
         Long2 := DEMheader.DEMSWCornerX + Succ(DEMheader.NumRow div 2) * DEMheader.DEMxSpacing;
         VincentyCalculateDistanceBearing(DEMSWcornerLat,Long1,DEMSWcornerLat+LatSizeMap,Long1,Distance,Bearing);
         {$IfDef RecordLatSpacingValues} WriteLineToDebugFile('Width of bottom of map ' + RealToString(Distance,-12,2)); {$EndIf}
         AverageYSpace := Distance / Pred(DEMheader.NumRow);
         Sum := 0;
         for i := 0 to Pred(DEMheader.NumRow) do begin
            Lat := DEMheader.DEMSWCornerY + i * DEMheader.DEMySpacing;
            VincentyCalculateDistanceBearing(Lat,Long1,Lat,Long2,Distance,Bearing);
            XSpaceByDEMrow^[i] := Distance * 0.5;
            DiagSpaceByDEMrow^[i] := sqrt(sqr(XSpaceByDEMrow^[i])+ sqr(AverageYSpace));
            Sum := Sum + XSpaceByDEMrow^[i];
            {$IfDef RecordLatSpacingValues} if (i mod 50 = 0) then WriteLineToDebugFile(IntegerToString(i,5) + RealToString(Lat,12,8) + RealToString(ExactXSpace^[i],10,4) + RealToString(ExactDiaSpace^[i],10,4)); {$EndIf}
         end;
         AverageXSpace := Sum / DEMheader.NumRow;
         if ((LatSizeMap > 3) or (LongSizeMap > 3)) and (DEMheader.DEMySpacing > 20 / 3600) then begin
            l1 := DEMSWcornerLat + 0.5 * LatSizeMap;
            AverageXSpace := abs(LongSizeMap * 111000 / pred(DEMheader.NumCol) * CosDeg(L1));
            AverageYSpace := abs(LatSizeMap * 111000 / pred(DEMheader.NumRow));
         end;
         AverageSpace := 0.5 * (AverageXSpace + AverageYSpace);
         {$IfDef RecordReadDEM} if not DEMMergeInProgress then WriteLineToDebugFile('AverageXSpace=' + RealToString(AverageXSpace,12,2) + '  AverageYSpace=' + RealToString(AverageYSpace,12,2) ); {$EndIf}
      end {proc LatLongMapSpacing};


begin {tDEMDataSet.DefineDEMVariables}
   if DEMAlreadyDefined then exit;

   DEMAlreadyDefined := true;
   {$If Defined(RecordReadDEM) or Defined(RecordDefineDatum) or Defined(RecordCreateNewDEM)}  WriteLineToDebugFile('tDEMDataSet.DefineDEMVariables ' + AreaName + ' in, proj=' + DEMMapProjection.GetProjectionName); {$EndIf}
   {$IfDef RecordProjectionParameters} DEMMapProjection.ProjectionParamsToDebugFile('tDEMDataSet.DefineDEMVariables in'); {$EndIf}
   {$IfDef RecordDEMDigitizeDatum}  WriteLineToDebugFile('tDEMDataSet.DefineDEMVariables in,digitize datum=' + StringFromDatumCode(DEMheader.DigitizeDatum)); {$EndIf}

   if (XSpaceByDEMrow = Nil) then GetMem(XSpaceByDEMrow,4*DEMheader.NumRow);
   if (DiagSpaceByDEMrow = Nil) then GetMem(DiagSpaceByDEMrow,4*DEMheader.NumRow);

   {$IfDef ExNLCD}
   {$Else}
      CheckForLandCover;
   {$EndIf}

   ElevationDEM := DEMheader.ElevUnits in [Feet,euMeters,DeciMeters,DeciFeet,Centimeters];
   if not (DEMheader.LatHemi in ['N','S']) then DEMheader.LatHemi := MDDef.DefaultLatHemi;
   SetElevationMultiple;
   DEMheader.MinElev := DEMheader.StoredMinElev * ElevationMultiple;;
   DEMheader.MaxElev := DEMheader.StoredMaxElev * ElevationMultiple;

   if DEMheader.DigitizeDatum in [Spherical] then begin
      if (MDdef.MapTicks <> tixNone) then MDdef.MapTicks := tixLatLong;
      MDdef.CoordUse := coordLatLong;
   end {if};
   if (DEMheader.DigitizeDatum = Rectangular) then MDdef.CoordUse := coordUTM;

   {$IfDef RecordCreateNewDEM} WriteLineToDebugFile('tDEMDataSet.DefineDEMVariables call init datum, projection=' + DEMMapProjection.GetProjectionName); {$EndIf}
   {$IfDef RecordProjectionParameters} DEMMapProjection.ProjectionParamsToDebugFile('tDEMDataSet.DefineDEMVariables step 2'); {$EndIf}

   AssignProjectionFromDEM(DEMMapProjection,AreaName);
   {$IfDef RecordCreateNewDEM} WriteLineToDebugFile('tDEMDataSet.DefineDEMVariables assigned projection=' + DEMMapProjection.GetProjectionName); {$EndIf}

   CheckUK_OS;
   InitializeDatum(TransformToPreferDatum);
   if (DEMheader.DigitizeDatum <> Rectangular) then  begin
      if (DEMheader.DEMUsed in [ArcSecDEM]) then begin
         {$IfDef RecordCreateNewDEM} WriteLineToDebugFile('ArcSec DEM 1'); {$EndIf}
         LatLongMapSpacing;
      end
      else begin
         {$IfDef RecordCreateNewDEM} WriteLineToDebugFile('UTM DEM, zone=' + IntToStr(DEMHeader.UTMZone)); {$EndIf}
         UTMtoLatLongDegree(DEMheader.DEMSWCornerX,DEMheader.DEMSWCornerY,DEMSWcornerLat,DEMSWcornerLong);
         DoSpacingCalcs;
      end;
   end;
   {$IfDef RecordProjectionParameters} DEMMapProjection.ProjectionParamsToDebugFile('after spacing checks'); {$EndIf}

   if (DEMSWcornerLat > 0) then DEMMapProjection.LatHemi := 'N' else DEMMapProjection.LatHemi := 'S';

   GridSpacingDetails(DEMBoundBoxDataGrid,AverageXSpace,AverageYSpace,AverageSpace,AverageGridTrue);
   AverageDiaSpace := Sqrt(Sqr(AverageXSpace) + Sqr(AverageYSpace));
   {$If Defined(RecordReadDEM) or Defined(RecordDefineDatum) or Defined(RecordCreateNewDEM)} WriteLineToDebugFile('tDEMDataSet.DefineDEMVariables ' + AreaName + ' out, ' + DEMMapProjection.KeyDatumParams); {$EndIf}
   {$IfDef RecordDEMMapProjection} DEMMapProjection.ProjectionParamsToDebugFile('tDEMDataSet.DefineDEMVariables in'); {$EndIf}
   {$IfDef RecordCreateNewDEM} WriteLineToDebugFile('tDEMDataSet.DefineDEMVariables in, proj=' + DEMMapProjection.GetProjectionName); {$EndIf}
   {$IfDef RecordDEMDigitizeDatum}  WriteLineToDebugFile('tDEMDataSet.DefineDEMVariables out, DigitizeDatum=' + StringFromDatumCode(DEMheader.DigitizeDatum)); {$EndIf}
end;


function tDEMDataSet.MissingCol(Col : integer) : boolean;
var
   i : integer;
begin
   for i := 0 to pred(DEMheader.NumRow) do if not MissingDataInGrid(Col,i) then begin
      Result := false;
      exit;
   end;
   Result := true;
end;


function tDEMDataSet.MissingRow(Row : integer) : boolean;
var
   i : integer;
begin
   for i := 0 to pred(DEMheader.NumCol) do
      if not MissingDataInGrid(i,Row) then begin
         Result := false;
         exit;
      end;
   Result := true;
end;


function tDEMDataSet.FilledGridBox(var GridLimits : tGridLimits) : boolean;
begin
   try
      ShowHourglassCursor;
      HeavyDutyProcessing := true;
      While MissingCol(GridLimits.XGridLow) do inc(GridLimits.XGridLow);
      While MissingRow(GridLimits.YGridLow) do inc(GridLimits.YGridLow);
      While MissingCol(GridLimits.XGridHigh) do dec(GridLimits.XGridHigh);
      While MissingRow(GridLimits.YGridHigh) do dec(GridLimits.YGridHigh);
      Result := (GridLimits.XGridHigh > GridLimits.XGridLow) and (GridLimits.YGridHigh > GridLimits.YGridLow);
   finally
      HeavyDutyProcessing := false;
      ShowDefaultCursor;
   end;
end;


procedure tDEMDataSet.CutOutGeoBox(bb : sfBoundBox);
var
   Lat,Long : float64;
   x,y : integer;
   {$IfDef RecordDEMEdits}
      InBox,OutBox : integer;
   {$EndIf}
begin
   {$IfDef RecordDEMEdits} WriteLineToDebugFile('tDEMDataSet.CutOutGeoBox in ' + AreaName + sfBoundBoxToString(bb)); {$EndIf}
   {$IfDef RecordDEMEdits}
      InBox := 0;
      OutBox := 0;
   {$EndIf}
   StartProgress('Check DEM');
   for x := 0 to pred(DEMheader.NumCol) do begin
       if (x mod 50 = 0) then UpDateProgressBar(x/pred(DEMheader.NumCol));
       for y := 0 to pred(DEMheader.NumRow) do begin
          DEMGridToLatLongDegree(x,y,Lat,Long);
          if  ( (Lat < bb.YMax) and (Lat > bb.YMin) and (Long < bb.XMax) and (Long > bb.XMin) ) then begin
               {$IfDef RecordDEMEdits}
                  inc(InBox);
               {$EndIf}
          end
          else begin
             {$IfDef RecordDEMEdits}
                inc(OutBox);
             {$EndIf}
             SetGridMissing(x,y);
          end;
       end;
   end;
   EndProgress;
   {$IfDef RecordDEMEdits} WriteLineToDebugFile('tDEMDataSet.CutOutGeoBox out, inbox= ' + IntToStr(InBox) + '  outbox' + IntToStr(OutBox)); {$EndIf}
end;


function tDEMDataSet.RectangleSubsetDEMinMemory(GridLimits : tGridLimits) : tDEMDataSet;
var
   z   : float32;
   x,y   : integer;
begin
    ClipDEMGrid(GridLimits.XGridLow,GridLimits.YGridLow);
    ClipDEMGrid(GridLimits.XGridHigh,GridLimits.YGridHigh);
    if FilledGridBox(GridLimits) then begin
       Result := tDEMDataSet.Create(99);
       Result.DEMheader := DEMheader;
       Result.DEMheader.NumCol := succ(GridLimits.XGridHigh-GridLimits.XGridLow);
       Result.DEMheader.NumRow := succ(GridLimits.YGridHigh-GridLimits.YGridLow);
       if not Result.AllocateDEMMemory(InitDEMnone) then exit;
       {$IfDef RecordWriteDEM} WriteLineToDebugFile('tDEMDataSet.RectangleSubsetDEM after trim'); {$EndIf}
       if ShowSatProgress and (GridLimits.XGridHigh - GridLimits.XGridLow > 4000) then StartProgress('Subset');
       for x := GridLimits.XGridLow to GridLimits.XGridHigh do begin
          {$IfDef RecordFullWriteDEM} WriteLineToDebugFile('x=' + IntToStr(x)); {$EndIf}
          if ShowSatProgress and (GridLimits.XGridHigh - GridLimits.XGridLow > 4000) and (x mod 25 = 0) then
             UpDateProgressBar( (x-GridLimits.XGridLow) / (GridLimits.XGridHigh-GridLimits.XGridLow) );
          for y := GridLimits.YGridLow to GridLimits.YGridHigh do begin
             {$IfDef RecordFullWriteDEM} WriteLineToDebugFile('y=' + IntToStr(y)); {$EndIf}
             if GetElevMeters(x,y,z) then Result.SetGridElevation(x-GridLimits.XGridLow,y-GridLimits.YGridLow,z);
          end;
       end;
       if ShowSatProgress and (GridLimits.XGridHigh - GridLimits.XGridLow > 4000) then EndProgress;
       Result.DEMheader.DEMSWCornerX := DEMheader.DEMSWCornerX + GridLimits.XGridLow * DEMheader.DEMxSpacing;
       Result.DEMheader.DEMSWCornerY := DEMheader.DEMSWCornerY + GridLimits.YGridLow * DEMheader.DEMySpacing;
    end
    else Result := Nil;
end;


function tDEMDataSet.RectangleSubsetDEM(GridLimits : tGridLimits; OpenMaps : boolean) : integer;
var
   fName : PathStr;
begin
   fName := MDTempDir + 'tempdem.dem';
   fName := RectangleSubsetDEM(GridLimits,fName);
   LoadNewDEM(Result,fName,OpenMaps);
end;


function tDEMDataSet.RectangleSubsetDEM(GridLimits : tGridLimits; FileName : PathStr = '') : PathStr;
var
   NewDEM : tDEMDataSet;
   Ext : ExtStr;
begin
   {$IfDef RecordWriteDEM} WriteLineToDebugFile('tDEMDataSet.RectangleSubsetDEM requested ' + GridLimitsString); {$EndIf}

    {$IfDef VCL}
       if (FileName = '') then begin
          FileName := WriteDEMDir + 'sub-' + AreaName;
          GetFileNameDefaultExt('subset DEM','DEM|*.DEM',FileName);
          DEMDefs.WriteDEMDir := ExtractFilePath(FileName);
       end;
    {$EndIf}

    NewDEM := RectangleSubsetDEMinMemory(GridLimits);
    if (NewDEM = Nil) then begin
       Result := '';
    end
    else begin
      Result := FileName;
      Ext := UpperCase(ExtractFileExt(FileName));
      if ExtEquals(Ext, '.TIF') then begin
         {$IfDef ExGeotiffWrite}
         {$Else}
            NewDEM.SaveAsGeotiff(FileName);
         {$EndIf}
      end
      {$IfDef AllowOddballDEMexports}
         else if ExtEquals(Ext, '.BIL') then NewDEM.WriteBILFormatDEM(FileName)
         else if ExtEquals(Ext, '.FLT') then NewDEM.WriteGridFloatFormatDEM(FileName)
      {$EndIf}
      else NewDEM.WriteNewFormatDEM(FileName);

      DEMJustSubset := FileName;
      NewDEM.Destroy;
    end;
   {$IfDef RecordWriteDEM} WriteLineToDebugFile('tDEMDataSet.RectangleSubsetDEM out'); {$EndIf}
end {proc Subset};


procedure tDEMDataSet.SetEntireGridMissing;
var
   Col,Row,nc : integer;
begin
   StartProgress('Initialize grid ' + AreaName);
   nc := (DEMheader.NumCol div 100);
   if nc = 0 then nc := 1;
   for Col := 0 to pred(DEMheader.NumCol) do begin
      if Col mod nc = 0 then UpdateProgressBar(Col/DEMheader.NumCol);
      for Row := 0 to pred(DEMheader.NumRow) do SetGridElevation(Col,Row,ThisDEMMissingValue);
   end;
   EndProgress;
end;


function tDEMDataSet.AllocateDEMMemory(InitDEM : byte; InitVal : float64 = 0) : boolean;
var
   Col, rc : int32;
begin
   {$If Defined(RecordCreateNewDEM) or Defined(RecordDEMMemoryAllocations)} WriteLineToDebugFile('tDEMDataSet.AllocateDEMMemory (' + ColsRowsString + ')  proj=' + DEMMapProjection.ProjDebugName); {$EndIf}

   if (DEMheader.NumCol <= MaxColsInRAM) and (DEMheader.NumRow <= MaxElevArraySize) then  begin
      AllocateDEMMemory := true;
      if (DEMheader.DEMPrecision in [FloatingPointDEM,LongWordDEM]) then BytesPerElevation := 4
      else if (DEMheader.DEMPrecision = ByteDEM) then BytesPerElevation := 1
      else if (DEMheader.DEMPrecision in [WordDEM,SmallIntDEM]) then BytesPerElevation := 2
      else MessageToContinue('tDEMDataSet.BytesPerElevation unhandled case');

      if (InitDEM = InitDEMMissing) then InitVal := ThisDEMMissingValue;
      BytesPerColumn := BytesPerElevation * DEMheader.NumRow;

      if Not DEMMemoryAlreadyAllocated then begin
         DEMMemoryAlreadyAllocated := true;
         if (DEMheader.DEMPrecision = FloatingPointDEM) then begin
            {$If Defined(RecordCreateNewDEM) or Defined(RecordDEMMemoryAllocations)} WriteLineToDebugFile('tDEMDataSet.AllocateDEMMemory, floats'); {$EndIf}
            New(ShortFloatElevations);
            ThisDEMMissingValue := MaxSmallInt;
         end
         else if (DEMheader.DEMPrecision = LongWordDEM) then begin
             {$If Defined(RecordCreateNewDEM) or Defined(RecordDEMMemoryAllocations)} WriteLineToDebugFile('tDEMDataSet.AllocateDEMMemory, Longword'); {$EndIf}
            New(LongWordElevations);
            ThisDEMMissingValue := MaxLongWord;
         end
         else if (DEMheader.DEMPrecision = WordDEM) then begin
            {$IfDef RecordDEMMemoryAllocations} WriteLineToDebugFile('tDEMDataSet.AllocateDEMMemory, word'); {$EndIf}
            New(WordElevations);
            ThisDEMMissingValue := MaxWord;
         end
         else if (DEMheader.DEMPrecision = ByteDEM) then begin
            {$IfDef RecordDEMMemoryAllocations} WriteLineToDebugFile('tDEMDataSet.AllocateDEMMemory, bytes'); {$EndIf}
            New(ByteElevations);
            ThisDEMMissingValue := 255;
         end
         else begin
            {$IfDef RecordDEMMemoryAllocations} WriteLineToDebugFile('tDEMDataSet.AllocateDEMMemory, int16'); {$EndIf}
            New(SmallIntElevations);
            ThisDEMMissingValue := MaxSmallInt;
         end;

         if ShowDEMReadingProgress then begin
            StartProgress('Allocate DEM memory');
            rc := ProgressIncrement(DEMheader.NumCol);
         end;
         for Col := 0 to pred(DEMheader.NumCol) do begin
            if ShowDEMReadingProgress and (Col mod rc = 0) then UpdateProgressBar(Col/DEMheader.NumCol);
            if (DEMheader.DEMPrecision = FloatingPointDEM) then GetMem(ShortFloatElevations^[Col],BytesPerColumn)
            else if (DEMheader.DEMPrecision = LongWordDEM) then GetMem(LongWordElevations^[Col],BytesPerColumn)
            else if (DEMheader.DEMPrecision = WordDEM) then GetMem(WordElevations^[Col],BytesPerColumn)
            else if (DEMheader.DEMPrecision = ByteDEM) then GetMem(ByteElevations^[Col],BytesPerColumn)
            else GetMem(SmallIntElevations^[Col],BytesPerColumn);
         end;
         if ShowDEMReadingProgress then EndProgress;
      end;
      {$IfDef RecordDEMMemoryAllocations} WriteLineToDebugFile('tDEMDataSet.AllocateDEMMemory, allocated'); {$EndIf}
      if (InitDEM = InitDEMmissing) then SetEntireGridMissing
      else if (InitDEM = InitDEMvalue) then SetEntireGridToConstant(InitVal);
      {$If Defined(RecordCreateNewDEM) or Defined(RecordDEMMemoryAllocations)} WriteLineToDebugFile('tDEMDataSet.AllocateDEMMemory out  proj=' + DEMMapProjection.ProjDebugName); {$EndIf}
   end
   else begin
      Result := false;
      {$If Defined(RecordCreateNewDEM) or Defined(RecordDEMMemoryAllocations)}
         HighlightLineToDebugFile(AreaName + '  Grid too large (Cols=' + IntToStr(DEMheader.NumCol) + ', rows=' + IntToStr(DEMheader.NumRow) + ')');
      {$EndIf}
   end;
end;


procedure tDEMDataSet.CheckMaxMinElev;
var
   Col,Row  : integer;
   z : float32;
begin
   {$IfDef RecordMinMax} WriteLineToDebugFile('enter tDEMDataSet.CheckMaxMinElev: ' + AreaName +  ZRange); {$EndIf}
   ShowHourglassCursor;
   DEMheader.MaxElev := -99e38;
   DEMheader.MinElev := 99e38;
   if ShowSatProgress and (DEMheader.NumCol > 1500) then StartProgress('Check min/max ' + AreaName);
   for Col := 0 to pred(DEMheader.NumCol) do begin
      if ShowSatProgress and (DEMheader.NumCol > 1500) and (Col mod 100 = 0) then UpdateProgressBar(Col/DEMheader.NumCol);
      for Row := 0 to pred(DEMheader.NumRow) do begin
         if GetElevMetersOnGrid(Col,Row,Z) then begin
            {$IfDef RecordExtremeZ} if (z < -100000) or (z > 100000) then WriteLineToDebugFile('col=' + IntToStr(col) + '  row=' + IntToStr(Row) + ' elev=' + RealToString(z,-18,0) ); {$EndIf}
            Petmath.CompareValueToExtremes(z,DEMheader.MinElev,DEMheader.MaxElev);
         end;
      end {for Row};
   end {for Col};
   {$IfDef RecordMinMax} WriteLineToDebugFile('exit tDEMDataSet.CheckMaxMinElev: ' + AreaName +  ZRange); {$EndIf}
  EndProgress;
end {proc};


procedure FindCutPoints(Min,Max,NumCuts : integer; var Cut : ColorCutArrayType);
var
   ZInc,BinMultiple,i : integer;
begin
   for i := 0 to NumCuts do Cut[i] := Min;
   if (1.0*Max - Min) > 10000 then BinMultiple := 100
   else if (Max - Min) > 5000 then BinMultiple := 50
   else if (Max - Min) > 2000 then BinMultiple := 10
   else if (Max - Min) > 1000 then BinMultiple := 5
   else if (Max - Min) >  250 then BinMultiple := 2
   else BinMultiple := 1;
   if (Min = Max) then exit;
   if (Min = 0) then begin
      {put first breakpoint to show the ocean}
      zInc := Max div (BinMultiple * NumCuts) * BinMultiple;
      Cut[0] := 1;
      Cut[1] := zInc;
   end
   else begin
      zInc := trunc((1.0*Max - Min) / (BinMultiple * NumCuts)) *  BinMultiple;
      Cut[0] := (Min div BinMultiple) * BinMultiple + zInc;
      Cut[1] := Cut[0] + zInc;
   end;
   for i := 2 to NumCuts do Cut[i] := Cut[pred(i)] + zInc;
   Cut[NumCuts] := Max;
   if (Min < 0) and (Max > 0) then begin
      for i := 0 to pred(NumCuts) do
         if Cut[i] <= 0 then ZInc := i;
         if ZInc = pred(NumCuts) then Cut[Zinc] := 0
         else begin
             if abs(Cut[Zinc]) < abs(Cut[succ(Zinc)]) then cut[ZInc] := 0
             else Cut[succ(ZInc)] := 0;
         end;
   end;
end;

function tDEMDataSet.DistanceMetersBetweenPoints(xg1,yg1,xgrid,ygrid : float64; var Heading : float64) : float64;
var
   Lat,Long,Lat2,Long2 : float64;
begin
   DEMGridToLatLongDegree(xg1,yg1,Lat,Long);
   DEMGridToLatLongDegree(xgrid,ygrid,Lat2,Long2);
   VincentyCalculateDistanceBearing(Lat,Long,Lat2,Long2,Result,Heading);
end;


procedure tDEMDataSet.CheckUK_OS;
begin
   if (DEMMapProjection.PName = UK_OS) then begin
      if UK_OS_projection(DEMFileName) or (DEMheader.DigitizeDatum = UK_OS_grid) then begin
         {$IfDef UKOS} WriteLineToDebugFile('tDEMDataSet.CheckUK_OS, opening'); {$EndIf}
         DEMMapProjection.PName := UK_OS;
         DEMMapProjection.GetProjectParameters;
         DEMheader.DigitizeDatum := UK_OS_grid;
      end;
   end;
end;


procedure tDEMDataSet.LatLongDegreetoUTM(Lat,Long : float64; var XUTM,YUTM : float64);
begin
   DEMMapProjection.ForwardProjectDegrees(Lat,Long,XUTM,YUTM);
end {proc LatLongtoUTM};


function tDEMDataSet.LatLongNearestGridPointInDEM(Lat, Long : float64): boolean;
var
  x,y : integer;
begin
   Result := LatLongDegreeInDEM(Lat,Long);
   if Result then begin
      LatLongDegreeToDEMGridInteger(Lat,Long,x,y);
      DEMGridToLatLongDegree(x,y,Lat,Long);
   end;
end;

procedure tDEMDataSet.UTMtoLatLongDegree(XUTM,YUTM : float64; var Lat,Long : float64);
begin
   DEMMapProjection.InverseProjectDegrees(xUTM,yUTM,Lat,Long);
end {proc UTMtoLatLong};


procedure tDEMDataSet.DEMGridToLatLongDegree(XGrid,YGrid : float64; var Lat,Long : float64);
begin
   if (DEMMapProjection <> Nil) then begin
      if (DEMMapProjection.Pname = PlateCaree) then begin
         Lat  := ComputeSWCornerY + DEMheader.DEMySpacing * YGrid;
         Long := ComputeSWCornerX + DEMheader.DEMxSpacing * XGrid;
      end
      else if (DEMheader.DigitizeDatum = Rectangular) then begin
         Lat  := 0;
         Long := 0;
      end
      else DEMMapProjection.InverseProjectDegrees(XGrid * DEMheader.DEMxSpacing + ComputeSWCornerX,YGrid *  DEMheader.DEMySpacing + ComputeSWCornerY,lat,long);
   end
   else begin
      MessageToContinue('should not be here in tDEMDataSet.DEMGridToLatLongDegree');
   end {if};
end {proc GridToLatLong};


procedure tDEMDataSet.DEMGridToUTM(XGrid,YGrid : float32; var XUTM,YUTM : float64);
var
   Lat,Long : float64;
begin
   if (DEMHeader.DEMUsed = UTMBasedDEM) then begin //(DEMMapProjection.Pname = UTMellipsoidal) then begin    //changed May 9, 2023
      XUTM := XGrid * DEMheader.DEMxSpacing + ComputeSWCornerX;
      YUTM := YGrid * DEMheader.DEMySpacing + ComputeSWCornerY;
   end
   else if (DEMheader.DigitizeDatum = Rectangular) then begin
      XUTM := xgrid;
      YUTM := ygrid;
   end
   else begin
      DEMGridToLatLongDegree(XGrid,YGrid,Lat,Long);
      WGS84DatumConstants.LatLongDegreeToUTM(Lat,Long,XUTM,YUTM);
   end;
end {proc DEMGridToUTM};


function tDEMDataSet.LatLongDegreeToDEMGrid(Lat,Long : float64; var XGrid,YGrid : float64) : boolean;
var
   xproj,yproj : float64;
begin
   if (DEMMapProjection.Pname = PlateCaree) then begin
      if LongSizeMap > 359 then begin
         if (Long < DEMBoundBoxGeo.xmin - 0.001) then Long := Long + 360;
         if (Long > DEMBoundBoxGeo.xmax + 0.001) then Long := Long - 360;
      end;
      XGrid := (Long - ComputeSWCornerX) / DEMheader.DEMxSpacing;
      YGrid := (Lat - ComputeSWCornerY) / DEMheader.DEMySpacing;
   end
   else begin
      DEMMapProjection.ForwardProjectDegrees(Lat,Long,xproj,yproj);
      XGrid := (Xproj - DEMBoundBoxProjected.xmin) / DEMheader.DEMxSpacing;
      YGrid := (Yproj - DEMBoundBoxProjected.ymin) / DEMheader.DEMySpacing;
   end;
   Result := GridInDataSet(XGrid,YGrid);
end {proc LatLongToDEMGrid};


function tDEMDataSet.LatLongDegreeToDEMGrid(Lat,Long : float64; var XGrid,YGrid : float32) : boolean;
var
   xproj,yproj : float64;
begin
   if (DEMMapProjection.Pname = PlateCaree) then begin
      if LongSizeMap > 359 then begin
         if (Long < DEMBoundBoxGeo.xmin - 0.001) then Long := Long + 360;
         if (Long > DEMBoundBoxGeo.xmax + 0.001) then Long := Long - 360;
      end;
      XGrid := (Long - ComputeSWCornerX) / DEMheader.DEMxSpacing;
      YGrid := (Lat - ComputeSWCornerY) / DEMheader.DEMySpacing;
   end
   else begin
      DEMMapProjection.ForwardProjectDegrees(Lat,Long,xproj,yproj);
      XGrid := (Xproj - DEMBoundBoxProjected.xmin) / DEMheader.DEMxSpacing;
      YGrid := (Yproj - DEMBoundBoxProjected.ymin) / DEMheader.DEMySpacing;
   end;
   Result := GridInDataSet(XGrid,YGrid);
end {proc LatLongToDEMGrid\};


function tDEMDataSet.LatLongDegreeToDEMGridInteger(Lat,Long : float64; var XGrid,YGrid : integer) : boolean;
//this works in the internal program representation of the grid as pixel-is-point
var
   xg,yg : float64;
begin
   Result := LatLongDegreeToDEMGrid(Lat,Long,xg,yg);
   XGrid := round(xg);
   YGrid := round(yg);
end;


function tDEMDataSet.GridInDataSet(XGrid,YGrid : integer) :  boolean;
begin
   Result := (XGrid >= 0) and (XGrid <= pred(DEMheader.NumCol)) and  (YGrid >= 0) and (YGrid <= pred(DEMheader.NumRow));
end {proc GridInDataSet};


function tDEMDataSet.GridInDataSet(XGrid,YGrid : float64) :  boolean;
begin
  Result := (XGrid >= 0) and (XGrid <= pred(DEMheader.NumCol)) and (YGrid >= 0) and (YGrid <= pred(DEMheader.NumRow));
end {proc GridInDataSet};


procedure tDEMDataSet.UTMToDEMGrid(XUTM,YUTM : float64; var XGrid,YGrid : float32; var InBounds : boolean);
var
   Lat,Long   : float64;
begin
   if (DEMheader.DEMUsed = UTMBasedDEM) then begin
      XGrid := (XUTM - ComputeSWCornerX) / DEMheader.DEMxSpacing;
      YGrid := (YUTM - ComputeSWCornerY) / DEMheader.DEMySpacing;
   end
   else begin
      DEMMapProjection.UTMToLatLongDegree(XUTM,YUTM,Lat,Long);
      LatLongDegreeToDEMGrid(Lat,Long,XGrid,YGrid);
   end;
   Inbounds := GridInDataSet(XGrid,YGrid);
end;


function tDEMDataSet.DEMLocationString(XGrid,YGrid : float64) : ShortString;
var
   Lat,Long : float64;
begin
   DEMGridToLatLongDegree(XGrid,YGrid,Lat,Long);
   Result := DEMMapProjection.PreferLocationString(Lat,Long);
end;


procedure tDEMDataSet.ReflectanceParams(Min : float64 = -9999; Max : float64 = -9999);
{initializes values used for reflectance map calculations}
const
   //https://pubs.usgs.gov/of/2012/1171/pdf/usgs_of2012-1171-Gantenbein_p101-106.pdf
   Azimuth3 : array[1..3] of float32 = (350,15,270);
   Alt3 : array[1..3] of float32 = (70,60,55);
   Weight3 : array[1..3] of float32 = (0.35,0.325,0.325);   //my approximation of their transparency settings
   //WBT
   Azimuth4 : array[1..4] of float32 = (225, 270, 315,360);
   Weight4 : array[1..4] of float32 = (0.1, 0.4, 0.4,0.1);
   //WBT full 360
   Azimuth8 : array[1..8] of float32 = (0, 45, 90, 135, 180, 225, 270, 315);
   Weight8 : array[1..8] of float32 = (0.15, 0.125, 0.1, 0.05, 0.1, 0.125, 0.15, 0.2);
var
   i : integer;
begin
   {$If Defined(RecordMultipleRefDirs)} WriteLineToDebugFile('tDEMDataSet.ReflectanceParams in, DEM=' + AreaName); {$EndIf}

   if (MDDef.MultShadeReliefMode = mhsFourFixed) then begin
      MDdef.UseRefDirs := 4;
      for I := 1 to MDdef.UseRefDirs do begin
         RefPhi[i] := Azimuth4[i];
         RefAlt[i] := (90-MDdef.RefTheta);
         RefWeight[i] := Weight4[i];
      end;
   end
   else if (MDDef.MultShadeReliefMode = mhsEightFixed) then begin
      MDdef.UseRefDirs := 8;
      for I := 1 to MDdef.UseRefDirs do begin
         RefPhi[i] := Azimuth8[i];
         RefAlt[i] := (90-MDdef.RefTheta);
         RefWeight[i] := Weight8[i];
      end;
   end
   else if (MDDef.MultShadeReliefMode = mhsThreeFixed) then begin
      MDdef.UseRefDirs := 3;
      for I := 1 to MDdef.UseRefDirs do begin
         RefPhi[i] := Azimuth3[i];
         RefAlt[i] := (90-Alt3[i]);
         RefWeight[i] := Weight3[i];
        {$If Defined(RecordMultipleRefDirs)} WriteLineToDebugFile(' dir ' + IntToStr(i) + RealToString(RefPhi[i],8,3)); {$EndIf}
      end;
   end
   else if (MDDef.MultShadeReliefMode = mhsSingleDirection) then begin
      MDdef.UseRefDirs := 1;
      RefPhi[1] := MDdef.RefPhi;
      RefAlt[2] := (90-MDdef.RefTheta);
      RefWeight[1] := 1 ;
   end
   else if (MDDef.MultShadeReliefMode = mhsPick)then begin
      for I := 1 to MDdef.UseRefDirs do begin
         RefPhi[i] := FindCompassAngleInRange(MDdef.RefPhi + pred(i) * 360 / MDdef.UseRefDirs);
         RefAlt[i] := (90-MDdef.RefTheta);
         RefWeight[i] := 1 / MDdef.UseRefDirs;
      end;
   end;
   {$If Defined(RecordMultipleRefDirs)} for I := 1 to MDdef.UseRefDirs do WriteLineToDebugFile(' dir ' + IntToStr(i) + RealToString(RefPhi[i],8,3)); {$EndIf}

   for I := 1 to MDdef.UseRefDirs do begin
      cosAlt[i] := cosDeg(RefAlt[i]);
      sinAlt[i] := sinDeg(RefAlt[i]);
   end;

   if (Min < -9998) and (Max < -9998) then begin
      RefMinElev := DEMheader.MinElev;   //min elevation in DEM, meters
      RefMaxElev := DEMheader.MaxElev;   //max elevation in DEM, meters
   end
   else begin
      RefMinElev := Min; //min elevation in DEM, meters
      RefMaxElev := Max; //max elevation in DEM, meters
   end;
end;


function tDEMDataSet.ReflectanceValue(x,y : integer) : integer;
{heavily modified for multidirectional hillshade}
{returns reflectance value that ranges from 0 (black) to 255 (white); MaxSmallInt if undefined}
{originally after Pelton, Colin, 1987, A computer program for hill-shading digital topographic data sets: Computers & Geosciences, vol.13, no.5, p.545-548.}
//https://desktop.arcgis.com/en/arcmap/10.3/tools/spatial-analyst-toolbox/how-hillshade-works.htm
//Hillshade = 255.0 * ((cos(Zenith_rad) * cos(Slope_rad)) + (sin(Zenith_rad) * sin(Slope_rad) * cos(Azimuth_rad - Aspect_rad)))
//If the calculation of the hillshade value is < 0, the output cell value will be = 0.
var
   sum,value  : float64;
   i : integer;
   SlopeAsp : tSlopeAspectRec;
begin
   Result := MaxSmallInt;
   if GetSlopeAndAspect(x,y,SlopeAsp) then begin
      // (SlopeAsp.AspectDir <= 360) or (SlopeAsp.AspectDir >= 0) then SlopeAsp.AspectDir := 180;  //deal with undefined case for flat pixel
      try
         Sum := 0;
         for I := 1 to MDdef.UseRefDirs do begin
            //allows multi-directions, which will all have the same sun alitutde and azimuths spread around the circle
            Value := RefWeight[i] * ( (cosAlt[i] * cosDeg(MDDef.RefVertExag * SlopeAsp.SlopeDegree)) + (sinAlt[i] * sinDeg(MDDef.RefVertExag * SlopeAsp.SlopeDegree * cosDeg(RefPhi[i] - SlopeAsp.AspectDir))));
            Sum := sum + Value;
         end;
         //Result := round(Sum / MDdef.UseRefDirs);
         Result := ValidByteRange(round(255 * Sum));
      except
          on Exception do Result := MaxSmallInt;
      end;
   end;
end;


function tDEMDataSet.InterpolateReflectanceValue(x,y : float64) : integer;
var
   Refs : array[1..4] of integer;
   xt,yt : integer;
   XIncr,YIncr : float64;
begin
   YIncr := frac(Y);
   XIncr := frac(X);
   if ((abs(XIncr) < 0.01) and (abs(YIncr) < 0.01)) or
      ((abs(XIncr) > 0.99) and (abs(YIncr) > 0.99)) then begin
      result := ReflectanceValue(round(x),round(y));
   end
   else begin
       xt := trunc(x);
       yt := trunc(y);
       Refs[1] := ReflectanceValue(xt,yt);
       Refs[2] := ReflectanceValue(succ(xt),yt);
       Refs[3] := ReflectanceValue(succ(xt),succ(yt));
       Refs[4] := ReflectanceValue(xt,succ(yt));
       if (Refs[1] = MaxSmallInt) or (Refs[2] = MaxSmallInt) or (Refs[3] = MaxSmallInt) or (Refs[4] = MaxSmallInt) then begin
          Result := MaxSmallInt;
       end
       else Result := round(  (1-XIncr) * (1-YIncr) * Refs[1]
                  +           (1-XIncr) *    YIncr  * Refs[4]
                  +              XIncr  * (1-YIncr) * Refs[2]
                  +              XIncr  *    YIncr  * Refs[3]);
   end;
end;


function tDEMDataSet.ReflectanceColor(TintedReflectance : tMapType; x,y : integer) : tcolor;
begin
   {$IfDef VCL}
      Result := ConvertPlatformColorToTColor(RGBReflectanceColor(TintedReflectance,x,y));
   {$EndIf}
end;


function tDEMDataSet.RGBReflectanceColor(TintedReflectance : tMapType; Col,Row : integer) : tPlatformColor;
var
   red,green,blue,zi,r : integer;
   zv : float32;
begin
   GetElevMeters(Col,Row,zv);
   if (TintedReflectance = mtGrayReflect) and (MDdef.WaterCheck and (abs(zv) < 0.001)) or (MDdef.LakeCheck and LakePoint(Col,Row)) then begin
      Result := MDdef.WaterColor;
   end
   else begin
      R := ReflectanceValue(Col,Row);
      if (R < MaxSmallInt) and (abs(RefMaxElev - RefMinElev) > 0.001) then begin
         if (TintedReflectance in [mtGrayReflect]) then begin
            Result := GrayRGBTrip(r);
         end
         else if (TintedReflectance in [mtIHSReflect]) then begin
            if (zv > RefMaxElev) then zv := RefMaxElev;
            if (zv < RefMinElev) then zv := RefMinElev;
            Result := RGBtripFromHSI((360.0 - ((zv - RefMinElev) / (RefMaxElev - RefMinElev) * 360.0)),MDDef.MergeSat,R);
         end
         else if (TintedReflectance in [mtRefGrayColor]) then begin
            if (zv < 0) then begin
               Result := RainbowRGBFunct(zv,RefMinElev,0);
            end
            else begin
               Result := GrayRGBTrip(r);
            end;
         end
         else if (TintedReflectance in [mtRefColorGray]) then begin
            if (zv > MDDef.CurrentSeaLevel) then begin
               Result := RainbowRGBFunct(zv,0,RefMaxElev);
            end
            else begin
               Result := GrayRGBTrip(r);
            end;
         end
         else if (TintedReflectance in [mtRefGrayBlue]) then begin
            if (zv < MDDef.CurrentSeaLevel) then begin
               Result := RGBTrip(0,0,r);
            end
            else begin
               Result := GrayRGBTrip(r);
            end;
         end
         else if (TintedReflectance = mtGYRReflect) then begin
             if (zv > MDDef.TopCutLevel) then begin
                Result := RGBTrip(0,r,0);
             end
             else if (zv < MDDef.BottomCutLevel) then begin
                Result := RGBTrip(r,0,0);
             end
             else begin
                Result := RGBTrip(r,r,0);
             end;
          end
         else if (TintedReflectance = mtGGRReflect) then begin
             if (zv > MDDef.TopCutLevel) then begin
                Result := RGBTrip(0,r,0);
             end
             else if (zv < MDDef.BottomCutLevel) then begin
                Result := RGBTrip(r,0,0);
             end
             else begin
                Result := RGBTrip(r,r,r);
             end;
          end
          else if (TintedReflectance = mtGrCyBlReflect) then begin
             if (zv > MDDef.TopCutLevel) then begin
                Result := RGBTrip(0,r,0);
             end
             else if (zv < MDDef.BottomCutLevel) then begin
                Result := RGBTrip(0,r,r);
             end
             else begin
                Result := RGBTrip(0,0,r);
             end;
          end
          else if (TintedReflectance = mtBlueGreenReflect) then begin
             if (zv > MDDef.CurrentSeaLevel) then begin
                Result := RGBTrip(0,r,0);
             end
             else begin
                Result := RGBTrip(0,0,r);
             end;
          end
          else if (TintedReflectance = mt6ColorVAToverlay) then begin
             if DEMglb[VATrelatedGrid].GetElevMeters(Col,Row,ZV) then begin
                zi := pred(round(zv));
                if zi in [3,4,5,6] then Red := 1 else Red := 0;
                if zi in [1,2,3] then Green := 1 else Green := 0;
                if zi in [0,1,5,6] then Blue := 1 else Blue := 0;
                Result := RGBTrip(r*Red,r*Green,r*Blue);
             end
             else Result := RGBTrip(r,r,r);
          end
          else if (TintedReflectance = mt6ColorsReflect) then begin
            // GetReflectanceRGB(TintedReflectance,zv,Red,Green,Blue);
             zi := round(zv - RefMinElev) * 6 div round(RefMaxElev - RefMinElev);
             { 0  : Blue}   { 1  : Cyan}  { 2  : Green} { 3  : Yellow}  { 4  : Red } { 5  : Magneta}
             if zi in [3,4,5,6] then Red := 1 else Red := 0;
             if zi in [1,2,3] then Green := 1 else Green := 0;
             if zi in [0,1,5,6] then Blue := 1 else Blue := 0;

             Result := RGBTrip(r*Red,r*Green,r*Blue);
          end;
       end
       else begin
          Result := MissingColorRGBTriple(Col,Row);
       end;
   end;
end;


function tDEMDataSet.DEMMapProjectionString : shortstring;
var
   sl : tstringList;
   i : integer;
begin
   Result := '';
   if (DEMMapProjection <> Nil) then begin
      sl := DEMMapProjection.ProjectionParametersList;
      for i := 1 to sl.count do Result := Result + MessLineBreak;
      sl.free;
   end;
end;


function tDEMDataSet.MissingColorRGBTriple(x,y : float64) : tPlatformColor;
begin
   Result := MDDef.MissingDataColor;
   if MDDef.DifferentiateHolesAndEdges and (not MissingDataInGrid(round(x),round(y))) then begin
      Result := RGBTrip(163,168,128);
   end;
end;


procedure tDEMDataSet.SimpleBoxAroundPoint(Col,Row,BoxSize : integer; var x1,y1,x2,y2 : integer);
var
   xincr,yincr : integer;
begin
   XIncr := round(0.5 * BoxSize / AverageXSpace);
   YIncr := round(0.5 * BoxSize / AverageYSpace);
   x1 := Col - XIncr;
   x2 := Col + XIncr;
   y1 := Row - YIncr;
   y2 := Row + YIncr;
   ClipDEMGrid(x1,y1);
   ClipDEMGrid(x2,y2);
end;


function tDEMDataSet.BoxAroundPointFromFullSizeMeters(Col,Row,BoxSize : integer) : tGridLimits;
var
   xreliefincr,yreliefincr : integer;
begin
   XReliefIncr := round(0.5 * BoxSize / AverageXSpace);
   YReliefIncr := round(0.5 * BoxSize / AverageYSpace);
   Result.XGridLow := Col - XReliefIncr;
   Result.XGridHigh := Col + XReliefIncr;
   Result.YGridLow := Row - YReliefIncr;
   Result.YGridHigh := Row + YReliefIncr;
   ClipDEMGrid(Result.XGridLow,Result.YGridLow);
   ClipDEMGrid(Result.XGridHigh,Result.YGridHigh);
end;


function tDEMDataSet.SurroundedPointElevs(Col,Row : integer; var znw,zw,zsw,zn,z,zs,zne,ze,zse : float32; RegionSize : integer = 1) : boolean;
{determines if point can be safely interpolated, or used for focal operations, that it is surrounded by valid data points}
begin
   Result := ((Col - RegionSize) >= 0) and ((Row - RegionSize) >= 0) and ((Col + RegionSize) < (DEMheader.NumCol-2)) and ((Row  + RegionSize) < (DEMheader.NumRow-2));
   if Result then begin
      Result := GetElevMetersOnGrid((Col - RegionSize),(Row - RegionSize),zsw) and GetElevMetersOnGrid(Col,(Row - RegionSize),zs) and GetElevMetersOnGrid((Col + RegionSize),(Row - RegionSize),zse) and
                GetElevMetersOnGrid((Col - RegionSize),Row,zw) and GetElevMetersOnGrid(Col,Row,z) and GetElevMetersOnGrid((Col + RegionSize),Row,ze) and
                GetElevMetersOnGrid((Col - RegionSize),(Row + RegionSize),znw) and GetElevMetersOnGrid(Col,(Row + RegionSize),zn) and GetElevMetersOnGrid((Col + RegionSize),(Row + RegionSize),zne);
   end {if};
end;


function tDEMDataSet.IsSurroundedPoint(Col,Row : integer) : boolean;
{determines if point can be safely interpolated--surrounded by valid data points and not missing MaxInt values}
var
   znw,zw,zsw,zn,z,zs,zne,ze,zse : float32;
begin
   Result := SurroundedPointElevs(Col,Row,znw,zw,zsw,zn,z,zs,zne,ze,zse);
end;


function tDEMDataSet.GetNineElevMeters(Col,Row : integer; var znw,zw,zsw,zn,z,zs,zne,ze,zse : float32) : boolean;
begin
   Result := GetElevMeters(pred(Col),pred(Row),zsw) and GetElevMeters(Col,pred(Row),zs) and GetElevMeters(succ(Col),pred(Row),zse) and GetElevMeters(pred(Col),Row,zw) and
      GetElevMeters(Col,Row,z) and GetElevMeters(succ(Col),Row,ze) and GetElevMeters(pred(Col),succ(Row),znw) and GetElevMeters(Col,succ(Row),zn) and GetElevMeters(succ(Col),succ(Row),zne);
end;

function tDEMDataSet.GetElev3x3Meters(XGrid,YGrid : integer; var Elev : tElevBoxArray) : boolean;
begin
   Result := SurroundedPointElevs(XGrid,YGrid, Elev[-1,-1],Elev[-1,0],Elev[-1,1],Elev[0,-1],Elev[0,0],Elev[0,1],Elev[1,-1],Elev[1,0],Elev[1,1]);
end;


function DropEarthCurve(d : float64) : float64;
   {d and DropEarthCurve in meters}
begin
  DropEarthCurve := 0;
  if CalculatingCurvature then case MDDef.CurvAlg of
      vcTM5441 : DropEarthCurve := Sqr(0.001*d) * 0.0676; {from Army TM5-441 (Feb 70) p.2-12}
      vcRadioLineOfSight : DropEarthCurve := Sqr(0.001*d) / (MDDef.RadioK * 0.25 * 51);
      vcYoeli : DropEarthCurve := 0.87 * sqr(0.001 * d) / (2 * 6370) * 1e3;
   end;
end;


procedure PickSlopeAspectMethod(aMessage : shortstring; var SlopeMethod : byte);
var
   StringList : Classes.tStringList;
   s1         : byte;
   i          : integer;
begin
   {$IfDef VCL}
      StringList := TStringList.Create;
      for s1 := FirstSlopeMethod to LastSlopeMethod do StringList.Add(SlopeMethodName(s1));
      i := ord(MDdef.SlopeAlg);
      GetFromListZeroBased('Desired slope method ' + aMessage,i,StringList);
      SlopeMethod := i;
      StringList.Free;
   {$EndIf}
end;


function tDEMDataSet.DEMBoundBoxGeo : sfBoundBox;
begin
  if (DEMheader.DEMUsed = ArcSecDEM) then begin
     Result := DEMBoundBoxProjected;
   end
   else begin
      Result.xMin := DEMSWcornerLong;
      Result.xMax := DEMSWcornerLong + LongSizeMap;
      Result.yMin := DEMSWcornerLat;
      Result.yMax := DEMSWcornerLat + LatSizeMap;
   end;
end;


function tDEMDataSet.DEMBoundBoxProjected : sfBoundBox;
begin
   Result.xmin := DEMHeader.DEMSWCornerX;
   Result.ymin := DEMHeader.DEMSWCornerY;
   Result.xmax := DEMHeader.DEMSWCornerX + pred(DEMHeader.Numcol) * DEMHeader.DEMxSpacing;
   Result.ymax := DEMHeader.DEMSWCornerY + pred(DEMHeader.NumRow) * DEMHeader.DEMySpacing;
end;


function tDEMDataSet.DEMBoundBoxDataGrid : sfBoundBox;
begin
   Result.xmin := 0;
   Result.ymin := 0;
   Result.xmax := pred(DEMheader.NumCol);
   Result.ymax := pred(DEMheader.NumRow);
end;


procedure tDEMDataSet.GetDEMLimits(var bLat,bLong : integer; var Lats,Longs :tFourFloats);
var
   Lat,Long : float64;
begin
   DEMGridToLatLongDegree(0,0,Lats[1],Longs[1]);
   DEMGridToLatLongDegree(0,pred(DEMheader.NumRow),Lats[2],Longs[2]);
   DEMGridToLatLongDegree(pred(DEMheader.NumCol),pred(DEMheader.NumRow),Lats[3],Longs[3]);
   DEMGridToLatLongDegree(pred(DEMheader.NumCol),0,Lats[4],Longs[4]);
   HeapSort(4,Lats);
   HeapSort(4,Longs);
   Lat := Lats[1]+0.0001;
   if (Lat < 0) then Lat := Lat - 1;
   bLat := trunc(Lat);

   Long := Longs[1]+0.0001;
   if (Long < 0) then Long := Long - 1;
   bLong := trunc(Long);
end;


function tDEMDataSet.SaveStatusString : shortstring;
begin
   case DEMStatus of
      dsSaved : Result := '';
      dsUnsaved : Result := 'unsaved--';
      dsUnsavedEdits : Result := 'unsaved edits--';
   end;
end;


function tDEMDataSet.GetPerpendicularLineEnd(Lat,Long,SizeMeters,Trend : float64; var Lat1,Long1,Lat2,Long2 : float64; MustBeOnMap : boolean = false) : boolean;
begin
   VincentyPointAtDistanceBearing(Lat,Long,SizeMeters,Trend+90,Lat1,Long1);
   VincentyPointAtDistanceBearing(Lat,Long,SizeMeters,Trend-90,Lat2,Long2);
   if not MustBeOnMap then begin
       if (not LatLongDegreeInDEM(Lat1,Long1)) then begin
          Lat1 := Lat;
          Long1 := Long;
          VincentyPointAtDistanceBearing(Lat,Long,2*SizeMeters,Trend-90,Lat2,Long2);
       end;
       if not LatLongDegreeInDEM(Lat2,Long2) then begin
          Lat2 := Lat;
          Long2 := Long;
          VincentyPointAtDistanceBearing(Lat,Long,2*SizeMeters,Trend+90,Lat2,Long2);
       end;
   end;
   Result := LatLongDegreeInDEM(Lat1,Long1) and LatLongDegreeInDEM(Lat2,Long2);
end;


procedure tDEMDataSet.SetUpMap(DEMNumber : integer; CheckElevs : boolean; inMapType : tMapType = mtDEMBlank; UsePC : boolean = true);
begin
   {$IfDef RecordSetup} WriteLineToDebugFile(AreaName + ' tDEMDataSet.SetUpMap, maptype=' + IntToStr(ord(inMapType)) + '  DEM=' + IntToStr(DEMNumber)); {$EndIf}
   if CheckElevs then CheckMaxMinElev;
   {$IfDef RecordSetup} WriteLineToDebugFile('Elev range: ' + DEMGlb[DEMNumber].zRange); {$EndIf}
   DefineDEMVariables(True);
   {$IfDef VCL}
      {$IfDef RecordMapType} WriteLineToDebugFile(AreaName + ' tDEMDataSet.SetUpMap, maptype=' + IntToStr(inMapType) + '  DEM=' + IntToStr(DEMNumber)); {$EndIf}
      CreateDEMSelectionMap(DEMNumber,true,UsePC,inMapType);
      {$IfDef RecordMapType} WriteLineToDebugFile(AreaName + ' tDEMDataSet.SetUpMap, maptype=' + IntToStr(inMapType) + '  DEM=' + IntToStr(DEMNumber)); {$EndIf}
      SelectionMap.Closable := true;
      SelectionMap.CheckProperTix;
      {$IfDef RecordSetup} WriteLineToDebugFile('tDEMDataSet.SetUpMap, projection=' + SelectionMap.MapDraw.PrimMapProj.GetProjectionName); {$EndIf}
   {$EndIf}
end;



{$IfDef NoMapOptions}
{$Else}

      function tDEMDataSet.LatLongDegreePointsIntervisible(Lat1,Long1,ObsUp,Lat2,Long2,TargetUp : float64; var Distance,BlockDistance : float64) : boolean;
      const
         MaxPts = 25000;
      type
        boolarr = array[0..MaxPts] of boolean;
      var
         NumPts,i : integer;
         Heading : float64;
         VisPoints : ^boolarr;
         xs,ys,ds,elevs : ^Petmath.bfarray32;
      begin
         Result := True;
         NumPts := 0;
         VincentyCalculateDistanceBearing(Lat1,Long1,Lat2,Long2,Distance,Heading);
         {$IfDef RecordLOS} WriteLineToDebugFile('tDEMDataSet.PointsIntervisible Len: ' + RealToString(Distance,8,2)); {$EndIf}
         if (Distance < MDDef.wf.ClosestBlockingDistance) then exit;
         new(xs);
         New(ys);
         New(ds);
         if (MDDef.wf.LOSAlgorithm = losMicrodemFractional) then NumPts := round(Distance / (AverageSpace * MDDef.wf.FanDEMSpaceMultiple));
         if (MDDef.wf.LOSAlgorithm = losMicrodemConstant) then NumPts := round(Distance / (MDDef.wf.MaskAreaInterval));
         if (NumPts > MaxPts) then NumPts := MaxPts;
         GetStraightRouteDEMGrid(Lat1,Long1,Lat2,Long2,MDDef.wf.StraightAlgorithm,NumPts,xs^,ys^,ds^);
         New(Elevs);
         New(VisPoints);
         GetVisiblePoints(ObsUp,TargetUp,-89,89,true,true,NumPts,xs^,ys^,ds^,elevs^,VisPoints^);
         for i := 1 to NumPts do begin
            if VisPoints^[i] then begin
               BlockDistance := ds^[i];
            end;
         end;
         Result := VisPoints^[NumPts];
         Dispose(Elevs);
         Dispose(VisPoints);
         Dispose(xs);
         Dispose(ys);
         Dispose(ds);
      end;

{$EndIf}


function tDEMDataSet.SecondGridIdentical(Map2 : integer) : boolean;
var
   Tolerance : float64;
begin
   {$IfDef RecordGridIdentical}
      WriteLineToDebugFile('Compare DEMs ' + IntToStr(Map2));
      WriteLineToDebugFile('  Rows: ' + IntToStr(DEMheader.NumRow) + '/' + IntToStr(DEMGlb[Map2].DEMheader.NumRow) + '  Cols: ' + IntToStr(DEMheader.NumCol) + '/' + IntToStr(DEMGlb[Map2].DEMheader.NumCol));
      WriteLineToDebugFile('  delta long int : ' + RealToString(abs(DEMheader.DEMxSpacing - DEMGlb[Map2].DEMheader.DEMxSpacing),-18,-6));
      WriteLineToDebugFile('  delta SW X: ' + RealToString(abs(DEMheader.DEMSWCornerX - DEMGlb[Map2].DEMheader.DEMSWCornerX),-18,-6));
      WriteLineToDebugFile('  delta SW Y: ' + RealToString(abs(DEMheader.DEMSWCornerY - DEMGlb[Map2].DEMheader.DEMSWCornerY),-18,-6));
   {$EndIf}
   Tolerance := DEMheader.DEMxSpacing * 0.5;
   Result := (DEMGlb[Map2] <> Nil) and (DEMheader.NumCol = DEMGlb[Map2].DEMheader.NumCol) and
      (DEMheader.NumRow = DEMGlb[Map2].DEMheader.NumRow) and
      (DEMheader.RasterPixelIsGeoKey1025 = DEMGlb[Map2].DEMheader.RasterPixelIsGeoKey1025) and
      (abs(DEMheader.DEMxSpacing - DEMGlb[Map2].DEMheader.DEMxSpacing) < Tolerance) and
      (abs(DEMheader.DEMSWCornerX - DEMGlb[Map2].DEMheader.DEMSWCornerX) < Tolerance) and
      (abs(DEMheader.DEMSWCornerY - DEMGlb[Map2].DEMheader.DEMSWCornerY) < Tolerance);
end;


procedure tDEMDataSet.SetNewDEM(var NewDEM : integer);
begin
   {$IfDef RecordCreateNewDEM} WriteLineToDebugFile('enter tDEMDataSet.SetNewDEM'); {$EndIf}
   NewDEM := 0;
   OpenDEMDataStructures(NewDEM);
   DEMGlb[NewDEM].DEMheader := DEMheader;
   DEMGlb[NewDEM].AreaName := AreaName;
   DEMGlb[NewDEM].DEMheader.DEMPrecision := FloatingPointDEM;
   DEMGlb[NewDEM].DEMheader.UTMZone := DEMMapProjection.projUTMZone;
   DEMGlb[NewDEM].DEMheader.LatHemi := DEMMapProjection.LatHemi;
   DEMGlb[NewDEM].DEMheader.DigitizeDatum := WGS84d;
   DEMGlb[NewDEM].DefineDEMVariables(true);
   DEMGlb[NewDEM].AllocateDEMMemory(InitDEMmissing);
   {$If Defined(RecordCreateNewDEM) or Defined(RecordResample)} WriteLineToDebugFile('exit tDEMDataSet.SetNewDEM, NewDEM=' + IntToStr(NewDEM)); {$EndIf}
end;


function tDEMDataSet.ReinterpolateUTMDEM(FloatSpacingMeters : float64;  UTMzone : int16 = -99; fName : PathStr = '') : integer;
begin
   {$IfDef RecordResample} WriteLineToDebugFile('tDEMDataSet.ReinterpolateUTMDEM in'); {$EndIf}
   if (FloatSpacingMeters < 0) then begin
      FloatSpacingMeters := MDDef.DefaultUTMGridSpacing;
      ReadDefault('spacing for new DEM (m)',FloatSpacingMeters);
   end;

   if (UTMZone < 0) then begin
      //MDdef.DefaultUTMZone := DEMMapProjection.projUTMZone;
      //ReadDefault('UTM zone',MDdef.DefaultUTMZone);
      MDdef.DefaultUTMZone := DEMHeader.UTMzone;
   end
   else MDdef.DefaultUTMZone := UTMzone;
   Result := SelectionMap.CreateGridToMatchMap(cgUTM,false,FloatingPointDEM,FloatSpacingMeters,FloatSpacingMeters,MDdef.DefaultUTMZone,PixelIsPoint);   //DEMHeader.RasterPixelIsGeoKey1025);

   DEMGlb[Result].DEMheader.VerticalCSTypeGeoKey := DEMheader.VerticalCSTypeGeoKey;
   DEMGlb[Result].DEMheader.ElevUnits := DEMheader.ElevUnits;

   DEMGlb[Result].AreaName := AreaName + '_utm_reint_' + RealToString(FloatSpacingMeters,-8,-2);
   DEMGlb[Result].FillHolesSelectedBoxFromReferenceDEM(DEMGlb[Result].FullDEMGridLimits,ThisDEM,hfEverything);
   if (fName <> '') then begin
      DEMGlb[Result].AreaName := ExtractFileNameNoExt(fName);
      DEMGlb[Result].WriteNewFormatDEM(fName);
   end;
   {$IfDef RecordResample} WriteLineToDebugFile('tDEMDataSet.ReinterpolateUTMDEM out, New DEM=' + IntToStr(Result)); {$EndIf}
end;


function tDEMDataSet.ReinterpolateLatLongDEM(var SpacingArcSec : float32; fName : PathStr = '') : integer;
begin
   if (SpacingArcSec < 0) then begin
      SpacingArcSec := 1;
      ReadDefault('Spacing for new DEM (sec)',SpacingArcSec);
   end;
   {$IfDef RecordResample} WriteLineToDebugFile('Resample ' + AreaName + ' Lat/Long,  Spacing sec=' + RealToString(SpacingArcSec,-8,2) + '  ' + KeyDEMParams(true)); {$EndIf}
   Result := SelectionMap.CreateGridToMatchMap(cgLatLong,false,FloatingPointDEM,SpacingArcSec,SpacingArcSec,MDdef.DefaultUTMZone,PixelIsPoint);   //DEMHeader.RasterPixelIsGeoKey1025);
   DEMGlb[Result].DEMheader.VerticalCSTypeGeoKey := DEMheader.VerticalCSTypeGeoKey;
   DEMGlb[Result].DEMheader.ElevUnits := DEMheader.ElevUnits;
   DEMGlb[Result].FillHolesSelectedBoxFromReferenceDEM(DEMGlb[Result].FullDEMGridLimits,ThisDEM,hfEverything);
   if (fName = '') then DEMGlb[Result].AreaName := AreaName + '_geo_reint_' + RealToString(SpacingArcSec,-8,-2)
   else begin
      DEMGlb[Result].AreaName := ExtractFileNameNoExt(fName);
      DEMGlb[Result].WriteNewFormatDEM(fName);
      {$IfDef RecordResample} WriteLineToDebugFile('Resample Lat/Long, saved to ' + fName + '   '  + DEMGlb[Result].KeyDEMParams(true)); {$EndIf}
   end;
end;


function tDEMDataSet.MaxHorizonAngle(Lat,Long,LeftAzimuthTrue,RightAzimuthTrue,AzIncrement,DistanceToGoOut,ObsUp : float64; StraightAlgorithm : DemDefs.tStraightAlgorithm) : float64;
var
   AzimuthTrue,BlockAngle,BlockDist,BlockLat,BlockLong : float64;
begin
   AzimuthTrue := LeftAzimuthTrue;
   Result := -30;
   while (AzimuthTrue <= RightAzimuthTrue) do begin
      HorizonBlocking(Lat,Long,AzimuthTrue,DistanceToGoOut,ObsUp,BlockAngle,BlockDist,BlockLat,BlockLong,StraightAlgorithm);
      if (BlockAngle > Result) then Result := BlockAngle;
      AzimuthTrue := AzimuthTrue + AzIncrement;
   end;
end;


function tDEMDataSet.ValidElevsInDEM : integer;
var
   x,y : integer;
begin
   Result := 0;
   for x := 0 to pred(DEMheader.NumCol) do begin
      for y := 0 to pred(DEMheader.NumRow) do begin
         if not MissingDataInGrid(x,y) then inc(Result);
      end;
   end;
end;


procedure tDEMDataSet.ComputeMissingData(var Missing : float64);
begin
   ComputeMissingData(FullDEMGridLimits,Missing);
end;

procedure tDEMDataSet.ComputeMissingData(GridLimits : tGridLimits; var Missing : float64);
var
   x,y,Miss : integer;
begin
   Miss := 0;
   for x := GridLimits.XGridLow to GridLimits.XGridHigh do
      for y := GridLimits.YGridLow to GridLimits.YGridHigh do
         if MissingDataInGrid(x,y) then inc(Miss);
   Missing := 100.0 * Miss / succ(GridLimits.XGridHigh - GridLimits.XGridLow) / succ(GridLimits.YGridHigh - GridLimits.YGridLow);
end;


function tDEMDataSet.CheckForUTMZones : boolean;
var
   NumZones : integer;
begin
   Result := false;
   if (DEMheader.DigitizeDatum = Spherical) then begin
      NumZones := succ(GetUTMZone(DEMSWcornerLong + LongSizeMap)- GetUTMZone(DEMSWcornerLong));
      if (NumZones > 1) and (not (AnswerIsYes('DEM covers ' + IntToStr(NumZones) + ' UTM zones; proceed'))) then exit;
      DEMMapProjection.h_DatumCode := 'WGS84';
      DEMMapProjection.projUTMZone := GetUTMZone(DEMSWcornerLong + 0.5 * LongSizeMap);
      if (DEMSWcornerLat > 0) then DEMMapProjection.LatHemi := 'N' else DEMMapProjection.LatHemi := 'S';
      {$IfDef VCL}
         GetMapParameters(DEMMapProjection.LatHemi,DEMMapProjection.projUTMZone,DEMMapProjection.h_DatumCode);
      {$EndIf}
      DEMMapProjection.DefineDatumFromUTMZone(DEMMapProjection.h_DatumCode,DEMMapProjection.projUTMZone,DEMMapProjection.LatHemi,'tDEMDataSet.CheckForUTMZones');
   end;
   Result := true;
end;


procedure tDEMDataSet.LatLongDegreePointsRequiredAntenna(NPts : integer; Lat1,Long1,ObsUp,Lat2,Long2 : float64; var XGrids,YGrids,Dists,HeightsReq : Petmath.bfarray32);
var
   PointOnRay : integer;
   ObserverTotalElevation,PointElev,DroppedZ,NewTanAngle,MaxTanAngle : float32;
begin
   {$IfDef RecordFullReqAnt} WriteLineToDebugFile('tDEMDataSet.LatLongDegreePointsRequiredAntenna in'); {$EndIf}
   GetStraightRoute(false,Lat1,Long1,Lat2,Long2,MDDef.wf.StraightAlgorithm,NPts,xgrids,ygrids,dists);
   {$IfDef RecordFullReqAnt} WriteLineToDebugFile('tDEMDataSet.LatLongDegreePointsRequiredAntenna DoRadial GetStraightRoute done'); {$EndIf}
   if GetElevFromLatLongDegree(Lat1,Long1,ObserverTotalElevation) then begin
      ObserverTotalElevation := ObserverTotalElevation + ObsUp;
      MaxTanAngle := -999;
      for PointOnRay := 1 to NPts do begin
         if GetElevMeters(xgrids[PointOnRay],ygrids[PointOnRay],PointElev) then begin
            DroppedZ := PointElev - DropEarthCurve(Dists[PointOnRay]);
            NewTanAngle := (DroppedZ - ObserverTotalElevation) / Dists[PointOnRay]; {line along ground which blocks the view for points farther away}
            if (NewTanAngle > MaxTanAngle) then MaxTanAngle := NewTanAngle;
            HeightsReq[PointOnRay] := ObserverTotalElevation + (MaxTanAngle * Dists[PointOnRay]) - DroppedZ;
         end
         else HeightsReq[PointOnRay] := 0;
      end;
   end;
   {$IfDef RecordFullReqAnt} WriteLineToDebugFile('tDEMDataSet.LatLongDegreePointsRequiredAntenna out'); {$EndIf}
end;


function tDEMDataSet.GetSamplingSize(GridLimits: tGridLimits) : integer;
begin
   Result := 1;
   while (1.0 * (succ(GridLimits.XgridHigh - GridLimits.XGridLow) div Result) * 1.0 * (succ(GridLimits.YGridHigh - GridLimits.YGridLow) div Result) ) > bfArrayMaxSize do begin
      inc(Result);
   end;
end;


{$IfDef NoMapOptions}
{$Else}
procedure MaskGrid(Map : tMapForm; DEM : integer; MatchCriteria : boolean; OnlyMissing : boolean = false);
var
   Col,Row : integer;
begin
   ShowHourglassCursor;
   if ValidDEM(GridMaskDEM) then begin
      for Col := 0 to DEMGlb[DEM].DEMheader.NumCol do
         for Row := 0 to DEMGlb[DEM].DEMheader.NumRow do
            if DEMGlb[GridMaskDEM].MissingDataInGrid(Col,Row) then DEMGlb[DEM].SetGridMissing(Col,Row)
            else if (not OnlyMissing) then begin
               if MatchCriteria then begin
                  if not(MaskValidPoint(Col,Row)) then DEMGlb[DEM].SetGridMissing(Col,Row);
               end
               else begin
                  if MaskValidPoint(Col,Row) then DEMGlb[DEM].SetGridMissing(Col,Row);
               end;
            end;
   end;
   DEMGlb[DEM].DEMstatus := dsUnsaved;
   Map.DoBaseMapRedraw;
end;


function tDEMDataSet.ValuesInRange(Min,Max : float64; ShowOnMap : boolean; Map : tMapForm) : integer;
var
   Col,Row,x,y : integer;
   Lat,Long : float64;
  z : float32;
   Bitmap : tMyBitmap;
begin
   Result := 0;
   if ShowOnMap then begin
      Map.DoFastMapRedraw;
      PetImage.CopyImageToBitmap(Map.Image1,Bitmap);
   end;
   for Col := 0 to pred(DEMheader.NumCol) do begin
      for Row := 0 to pred(DEMheader.NumRow) do   begin
         if GetElevMeters(Col,Row,z) then  begin
            if (z <= Max) and (z >= Min) then  begin
               inc(Result);
               if ShowOnMap then begin
                  DEMGridToLatLongDegree(Col,Row,Lat,Long);
                  Map.MapDraw.LatLongDegreeToScreen(Lat,Long,x,y);
                  Petmar.ScreenSymbol(Bitmap.Canvas,x,y,FilledBox,2,claRed);
               end;
            end;
         end;
      end;
   end;
   if ShowOnMap then  begin
      Map.Image1.Picture.Graphic := Bitmap;
      Bitmap.Free;
   end;
end;

{$EndIf}

function tDEMDataSet.LatLongDegreeInDEM(Lat,Long : float64) : boolean;
begin
   Result := PointInBoundingBox(Lat,Long,DEMBoundBoxGeo);
end;


function tDEMDataSet.DEMHorizontalSpacingSummary : ShortString;
begin
   case DEMheader.DEMUsed of
      UTMBasedDEM  : Result := 'UTM-like DEM';
      ArcSecDEM : Result := 'Geographic (lat/long) DEM';
   end;
   Result := Result + '  (' + ptTrim(DEMDefs.SpacingUnits[DEMheader.DataSpacing]) + ')';
end;


function tDEMDataSet.GridDefinition : ShortString;
var
   Decs : integer;
begin
   if (DEMheader.DEMUsed = UTMBasedDEM) then Decs := -2 else Decs := -8;
   Result := SWcornerString + '  dx=' + RealToString(DEMheader.DEMxSpacing,-12,Decs) + '  dy=' + RealToString(DEMheader.DEMySpacing,-12,Decs) + DEMDefs.SpacingUnits[DEMheader.DataSpacing];
end;


function tDEMDataSet.SimpleHorizontalDEMSpacing(BoxSize : integer) : ShortString;
begin
   if (DEMheader.DataSpacing in [SpaceDegrees]) then begin
      if (DEMheader.DEMySpacing > 0.25) then Result := RealToString(DEMheader.DEMySpacing * BoxSize,-12,-8) +  '°'
      else if (DEMheader.DEMySpacing > 1 / 119) then Result := RealToString(DEMheader.DEMySpacing*60 * BoxSize,-12,-4) + ''''
      else Result := RealToString(DEMheader.DEMxSpacing*3600 * BoxSize,-12,-4) +  '"';
   end
   else Result := RealToString(DEMheader.DEMySpacing,-12,-8) + DEMDefs.SpacingUnits[DEMheader.DataSpacing];
end;


function tDEMDataSet.HorizontalDEMSpacing(short : boolean = false) : ShortString;
begin
   if (DEMheader.DataSpacing in [SpaceDegrees]) then begin
      if (DEMheader.DEMySpacing > 0.25) then Result := RealToString(DEMheader.DEMySpacing,-12,-8) + 'x' + RealToString(DEMheader.DEMxSpacing,-12,-8) +  '°'
      else if (DEMheader.DEMySpacing > 1 / 119) then Result := RealToString(DEMheader.DEMySpacing*60,-12,-4) + 'x' + RealToString(DEMheader.DEMxSpacing*60,-12,-4) +  ''''
      else Result := RealToString(DEMheader.DEMxSpacing*3600,-12,-4) + 'x' + RealToString(DEMheader.DEMySpacing*3600,-12,-4) +  '"';
      if not short then Result := Result +  ' (about ' + RealToString(AverageXSpace,-12,2) + 'x' +  RealToString(AverageYSpace,-12,2) + ' m)';
   end
   else Result := RealToString(DEMheader.DEMySpacing,-12,-8) + 'x' + RealToString(DEMheader.DEMxSpacing,-12,-8) + DEMDefs.SpacingUnits[DEMheader.DataSpacing];
   if not short then Result := '  Horiz: ' + Result;
end;

function tDEMDataSet.SWcornerString : ShortString;
var
   Decs : integer;
begin
   if (DEMheader.DEMUsed = UTMBasedDEM) then Decs := -2 else Decs := -8;
   Result := 'SW corner x=' + RealToString(DEMheader.DEMSWCornerX,-18,Decs) + ' y=' + RealToString(DEMheader.DEMSWCornerY,-18,Decs)
end;


function tDEMDataSet.KeyDEMParams(short : boolean = false) : ShortString;
begin
   if (DEMheader.DEMPrecision = FloatingPointDEM) then Result := '4-byte real'
   else if (DEMheader.DEMPrecision = ByteDEM) then Result := 'byte'
   else if (DEMheader.DEMPrecision = WordDEM) then Result := 'word (unsigned 2 byte)'
   else if (DEMheader.DEMPrecision = LongWordDEM) then Result := 'long word (unsigned 4 byte)'
   else if (DEMheader.DEMPrecision = SmallIntDEM) then Result := '2-byte int';
   Result := ColsRowsString + '  ' + Result + '  ' + zRange + HorizontalDEMSpacing;
   if (not Short) then begin
      if DEMheader.DigitizeDatum in [0..14] then Result := Result + MessLineBreak + ' Datum: ' +  DigitizeDatumName[DEMheader.DigitizeDatum] ;
      Result := Result +  MessLineBreak + ' utm=' + IntToStr(DEMheader.UTMZone) + DEMheader.LatHemi;
   end;
end;


function tDEMDataSet.PixelIsString : AnsiString;
begin
   if (DEMHeader.RasterPixelIsGeoKey1025 = 1) then Result := 'Pixel is area'
   else if (DEMHeader.RasterPixelIsGeoKey1025 = 2) then Result := 'Pixel is point'
   else Result := '';
end;


function tDEMDataSet.FullDEMParams : AnsiString;
begin
   Result := 'DEM: ' + AreaName + MessLineBreak + KeyDEMParams(false) + MessLineBreak + SWCornerString + MessLineBreak + PixelIsString;
end;


procedure tDEMDataSet.ClipDEMGrid(var x,y : float64);
begin
   if (x < 0) then x := 0 else if (x > pred(DEMheader.NumCol)) then x := pred(DEMheader.NumCol);
   if (y < 0) then y := 0 else if (y > pred(DEMheader.NumRow)) then y := pred(DEMheader.NumRow);
end;

procedure tDEMDataSet.ClipDEMGrid(var x,y : float32);
begin
   if (x < 0) then x := 0 else if (x > pred(DEMheader.NumCol)) then x := pred(DEMheader.NumCol);
   if (y < 0) then y := 0 else if (y > pred(DEMheader.NumRow)) then y := pred(DEMheader.NumRow);
end;


procedure tDEMDataSet.ClipDEMGrid(var x,y : integer);
begin
   if (x < 0) then x := 0 else if x > pred(DEMheader.NumCol) then x := pred(DEMheader.NumCol);
   if (y < 0) then y := 0 else if y > pred(DEMheader.NumRow) then y := pred(DEMheader.NumRow);
end;


procedure InitilializeDEMCoord;
var
   i : integer;
begin
   {$IfDef MessageStartUpUnit} MessageToContinue('start demcoords initialization'); {$EndIf}
   for i := 1 to MaxDEMDataSets do DEMGlb[i] := Nil;
   SkipSphericalCheck := false;
   RouteTiffThroughGDAL := false;
   GridMaskDEM := 0;
   MaskMaxVal := 32767.0;
   MaskMinVal := 0;
   {$IfDef  ExGraphs}
   {$Else}
      RoseGraph := Nil;
   {$EndIf}
end;


{ tSSOVarAnalysis }

function tSSOVarAnalysis.ComputeEigenVectors: boolean;
var
   i,NRot : integer;
   Dip,Strike,DipDirect : float32;
begin
      if (SSOVars.NumPts / (SSOVars.NumPts + SSOVars.NumPts)  < 0.5) then Result := false
      else begin
         Result := true;
         Jacobi(M^,3,SSOVars.V,s^,NRot);
         Eigsrt(SSOVars.V,S^,3);
         if abs(SSOVars.v[2]) < 0.00001 then SSOVars.v[2] := 0.00001;
         if abs(SSOVars.v[3]) < 0.000012 then SSOVars.v[3] := 0.000012;

         {Shape/strength statistics after Fisher, Lewis, and Embleton, p.159}
         if abs(SSOvars.v[3]) < 0.00001 then begin
            SSOvars.s2s3 := 0;
            SSOvars.Shape := 0;
            SSOvars.s1s2 := MaxSmallInt;
            SSOvars.Strength := MaxSmallInt;
         end
         else begin
            SSOvars.s1s2 := ln(SSOvars.v[1]/ SSOvars.v[2]);
            SSOvars.s2s3 := ln(SSOvars.v[2]/ SSOvars.v[3]);
            SSOvars.Shape := ln(SSOvars.v[1]/ SSOvars.v[2])/ln( SSOvars.v[2]/ SSOvars.v[3]);
            SSOvars.Strength := ln(SSOvars.v[1]/ SSOvars.v[3]);
         end;

         for i := 1 to 3 do begin //compute eigenvectors
            S2[1] := s[1,i];
            S2[2] := s[2,i];
            S2[3] := s[3,i];
            CartesianToDipStrike(s2,Dip,Strike);
            if (Dip < 0) then begin
              Dip := abs(Dip);
              DipDirect := Strike - 90;
           end
           else DipDirect := Strike + 90;
           DipDirect := FindCompassAngleInRange(DipDirect);
           if DipDirect > 180 then DipDirect := DipDirect - 180;
           SSOVars.TheDips[i] := Dip;
           SSOVars.TheDipDirs[i] := DipDirect;
        end {for i};
      end;
end;


constructor tSSOVarAnalysis.Create(WhichDEM: integer);
begin
   {$If Defined(RecordSSO)} WriteLineToDebugFile(' tSSOVarAnalysis.Create in'); {$EndIf}
   DEM := WhichDEM;
   New(M);
   New(s);
   DEMGlb[DEM].InitializeNormals(SSOVars.NumPts);
end;


destructor tSSOVarAnalysis.Destroy;
begin
   {$If Defined(RecordSSO)} WriteLineToDebugFile(' tSSOVarAnalysis.Destroy in'); {$EndIf}
   Dispose(M);
   Dispose(s);
end;


procedure tSSOVarAnalysis.NormalsInBox(GridLimits: tGridLimits);
var
   Col,Row,j,k : integer;
begin
   SSOVars.x1sq := 0;
   SSOVars.y1sq := 0;
   SSOVars.z1sq := 0;
   SSOVars.NumPts := 0;
   SSOVars.NumMissing := 0;
   for j := 1 to 3 do
      for k := 1 to 3 do
         M^[j,k] := 0.0;

   DEMGlb[DEM].ClipDEMGrid(GridLimits.XGridLow,GridLimits.YGridLow);
   DEMGlb[DEM].ClipDEMGrid(GridLimits.XGridHigh,GridLimits.YGridHigh);
   Col := GridLimits.XGridLow;
   while (Col <= GridLimits.XGridHigh) do begin
      Row := GridLimits.YGridLow;
      while (Row <= GridLimits.YGridHigh) do begin
         if DEMGlb[DEM].Normals^[Col]^[Row][3] < pred(MaxSmallInt) then begin
            inc(SSOVars.NumPts);

            SSOVars.x1sq := SSOVars.x1sq + sqr(DEMGlb[DEM].Normals^[Col]^[Row][1]);
            SSOVars.y1sq := SSOVars.y1sq + sqr(DEMGlb[DEM].Normals^[Col]^[Row][2]);
            SSOVars.z1sq := SSOVars.z1sq + sqr(DEMGlb[DEM].Normals^[Col]^[Row][3]);

            for j := 1 to 3 do
               for k := 1 to 3 do
                  M^[j,k] := M^[j,k] + DEMGlb[DEM].Normals^[Col]^[Row][j] * DEMGlb[DEM].Normals^[Col]^[Row][k];
         end
         else inc(SSOVars.NumMissing);
         inc(Row);
      end {while};
      inc(Col);
   end {while};
end;


{ tAspectStats }

function CreateAspectRose(DEM : integer) : tThisBaseGraph;
var
   AspectStats : tAspectStats;
begin
   AspectStats.Create(DEM);
   AspectStats.FillFromGrid(DEMGlb[DEM].FullDEMGridLimits);
   AspectStats.CreateRose;
   AspectStats.Destroy;
end;

procedure tAspectStats.AddAspect(Aspect : float32);
var
   AspInt : integer;
begin
   AspInt := round(Aspect);
   if (AspInt = 360) then AspInt := 0;
   inc(AspectFreqValsGrid[AspInt]);
   inc(Npts);
end;


procedure tAspectStats.AddPoint(SlopeAspectRec: tSlopeAspectRec);
var
   AspInt : integer;
begin
   if (SlopeAspectRec.AspectDir < 32000) then begin
      AspX := AspX + CosDeg(SlopeAspectRec.AspectDir);
      AspY := AspY + SinDeg(SlopeAspectRec.AspectDir);
      AspInt := round(SlopeAspectRec.AspectDir);
      if (AspInt = 360) then AspInt := 0;
      inc(AspectFreqValsTrue[AspInt]);
      AspInt := round(SlopeAspectRec.AspectDirGrid);
      if (AspInt = 360) then AspInt := 0;
      inc(AspectFreqValsGrid[AspInt]);
      inc(Npts);
   end;
end;


constructor tAspectStats.Create(theDEM : integer);
begin
   Npts := 0;
   AspX := 0;
   AspY := 0;
   DEM := theDEM;
   FillChar(AspectFreqValsTrue,SizeOf(AspectFreqValsTrue),0);
   FillChar(AspectFreqValsGrid,SizeOf(AspectFreqValsGrid),0);
end;


procedure tAspectStats.FillFromGrid(GridLimits : tGridLimits);
var
   x,y : integer;
   SlopeAspectRec: tSlopeAspectRec;
  z : float32;
begin
   for x := GridLimits.XGridLow to GridLimits.XGridHigh do begin
      for y := GridLimits.YGridLow to GridLimits.YGridHigh do begin
         if DEMGlb[DEM].GetElevMeters(x,y,z) then begin
            if (z >= 0) and (z <= 360) then begin
               SlopeAspectRec.AspectDir := z;
               AddPoint(SlopeAspectRec);
            end;
         end;
      end;
   end;
end;

function tAspectStats.CreateRose(BaseLegend : shortstring = '') : tThisBaseGraph;
begin
   Result := TThisBaseGraph.Create(Application);
   Result.RoseColor := WinGraphColors[DEM mod 16];
   Result.DrawAspectRose(AspectFreqValsTrue,DEMGlb[DEM].AreaName + ' ' + Report,BaseLegend);
end;


destructor tAspectStats.Destroy;
begin
   inherited;
end;


function tAspectStats.QueensAspect: float64;
var
   Diagonal : integer;
begin
   Diagonal := round(arctan(DEMGlb[DEM].AverageYSpace/DEMGlb[DEM].AverageXSpace) / DegToRad);
   Result := 45 * (AspectFreqValsGrid[0] + AspectFreqValsGrid[Diagonal] + AspectFreqValsGrid[90] + AspectFreqValsGrid[180-Diagonal] + AspectFreqValsGrid[180] + AspectFreqValsGrid[180+Diagonal] +
                   AspectFreqValsGrid[270] + AspectFreqValsGrid[360-Diagonal] + AspectFreqValsGrid[360]) / NPts;
end;


procedure tAspectStats.VectorAverage;
var
   tx,ty : float64;
begin
   tx := aspx /  Npts;
   ty := aspy /  Npts;
   AvgAspectDir := HeadingOfLine(tx,ty);
   AvgAspectMag := sqrt(sqr(tx) + sqr(tY));
end;


function tAspectStats.Report : shortstring;
begin
   VectorAverage;
   Result := RealToString(AvgAspectMag,-8,3) + '  ' + RealToString(AvgAspectDir,-8,2) + '°' + '  ' + RealToString(QueensAspect,-8,2);
end;


initialization
   SubsequentDEM := false;
   OpeningNewGrid := false;
finalization
   {$IfDef RecordClosing} WriteLineToDebugFile('Closing read_dem in'); {$EndIf}
   {$IfDef RecordCloseDEM} WriteLineToDebugFile('RecordCloseDEM in read_dem'); {$EndIf}
   {$IfDef RecordReadDEM} WriteLineToDebugFile('RecordReadDEM in read_dem'); {$EndIf}
   {$IfDef TrackLastDEMName} WriteLineToDebugFile('TrackLastDEMName in read_dem'); {$EndIf}
   {$IfDef RecordReadDEMFull} WriteLineToDebugFile('RecordReadDEMFullProblems in demcoord (performance degrader)'); {$EndIf}
   {$IfDef RecordReadDEMVeryFull} WriteLineToDebugFile('RecordReadDEMVeryFullProblems in read_dem (severe slowdown)'); {$EndIf}
   {$IfDef RecordReadMakeSingle} WriteLineToDebugFile('RecordReadMakeSingleProblems in read_dem'); {$EndIf}
   {$IfDef RecordMerge} WriteLineToDebugFile('RecordMergeProblems in read_dem'); {$EndIf}
   {$IfDef RecordOpenMap} WriteLineToDebugFile('RecordOpenMapProblems in read_dem'); {$EndIf}
   {$IfDef RecordAllImport} WriteLineToDebugFile('RecordAllImportProblems in read_dem (real slowdown)'); {$EndIf}
   {$IfDef RecordGDAL} WriteLineToDebugFile('RecordGDALProblems in read_dem'); {$EndIf}
   {$IfDef RecordWorldFile} WriteLineToDebugFile('RecordWorldFileProblems in read_dem'); {$EndIf}
   {$IfDef RecordLOS} WriteLineToDebugFile('RecordLOSProblems in demcoord'); {$EndIf}
   {$IfDef RecordWriteDEM} WriteLineToDebugFile('RecordWriteDEMProblems in demcoord'); {$EndIf}
   {$IfDef RecordClosing} WriteLineToDebugFile('RecordClosingProblems in demcoord'); {$EndIf}
   {$IfDef RecordCloseDEM} WriteLineToDebugFile('RecordCloseDEM in demcoord'); {$EndIf}
   {$IfDef RecordResample} WriteLineToDebugFile('RecordResampleProblems in demcoord'); {$EndIf}
   {$IfDef RecordHorizon} WriteLineToDebugFile('RecordHorizonProblems in demcoord'); {$EndIf}
   {$IfDef RecordDetailedHorizon} WriteLineToDebugFile('RecordDetailedHorizonProblems in demcoord  (performance degrader)'); {$EndIf}
   {$IfDef RecordPointClass} WriteLineToDebugFile('RecordPointClassProblems in demcoord  (performance degrader)'); {$EndIf}
   {$IfDef RecordGetStraightRoute} WriteLineToDebugFile('RecordGetStraightRoute in demcoord  (performance degrader)'); {$EndIf}
   {$IfDef RecordDetailedResample} WriteLineToDebugFile('RecordDetailedResampleProblems in demcoord  (performance degrader)'); {$EndIf}
   {$IfDef RecordLOSAlgorithm} WriteLineToDebugFile('RecordLOSAlgorithm in demcoord'); {$EndIf}
   {$IfDef RecordFullStraightRoute} WriteLineToDebugFile('RecordFullStraightRoute in demcoord  (performance degrader)'); {$EndIf}
   {$IfDef RecordGetUTMStraightRoute} WriteLineToDebugFile('RecordGetUTMStraightRoute in demcoord  (performance degrader)'); {$EndIf}
   {$IfDef RecordDEMHeader} WriteLineToDebugFile('RecordDEMHeader in demcoord'); {$EndIf}
   {$IfDef Filter} WriteLineToDebugFile('Filter in demcoord  (big slowdown)'); {$EndIf}
   {$IfDef RecordDEMMapProjection} WriteLineToDebugFile('RecordDEMMapProjectionProblems in demcoord'); {$EndIf}
   {$IfDef RecordVariogram} WriteLineToDebugFile('RecordVariogram in demcoord'); {$EndIf}
   {$IfDef TriPrismErrors} WriteLineToDebugFile('TriPrismErrors in demcoord'); {$EndIf}
   {$IfDef TriPrismResults} WriteLineToDebugFile('TriPrismResults in demcoord'); {$EndIf}
   {$IfDef RecordGridIdentical} WriteLineToDebugFile('RecordGirdIdentical  in demcoord'); {$EndIf}
   {$IfDef RecordCreateNewDEM} WriteLineToDebugFile('RecordCreateNewDEM  in demcoord'); {$EndIf}
   {$IfDef RecordVegGrid} WriteLineToDebugFile('RecordVegGrid  in demcoord'); {$EndIf}
   {$IfDef RecordGetGridLimits} WriteLineToDebugFile('RecordVegGrid  in demcoord'); {$EndIf}
   {$IfDef RecordHiResDEM} WriteLineToDebugFile('RecordHiResDEM  in demcoord'); {$EndIf}
   {$IfDef RecordLatSpacingValues} WriteLineToDebugFile('RecordLatSpacingValues in demcoord'); {$EndIf}
   {$IfDef RecordMapDraw} WriteLineToDebugFile('RecordMapDraw in demcoord'); {$EndIf}
   {$IfDef RecordASCIIExport} WriteLineToDebugFile('RecordASCIIExportProblems in demcoord'); {$EndIf}
   {$IfDef RecordDEMMemoryAllocations} WriteLineToDebugFile('RecordDEMMemoryAllocations in demcoord'); {$EndIf}
   {$IfDef RecordSetup} WriteLineToDebugFile('RecordSetupProblems in demcoord'); {$EndIf}
   {$IfDef RecordVAT} WriteLineToDebugFile('RecordVAT in demcoord'); {$EndIf}
   {$IfDef RecordDEMEdits} WriteLineToDebugFile('RecordDEMEdits in demcoord'); {$EndIf}
   {$IfDef RecordNLCD} WriteLineToDebugFile('RecordNLCD in demcoord'); {$EndIf}
   {$IfDef NoParallelFor} WriteLineToDebugFile('NoParallelFor in demcoord'); {$EndIf}
   {$IfDef NoInline} WriteLineToDebugFile('NoInline in demcoord'); {$EndIf}
   {$IfDef RecordMoments} WriteLineToDebugFile('RecordMoments in demcoord'); {$EndIf}
   {$IfDef RecordDEMMapProjection} WriteLineToDebugFile('RecordDEMMapProjection'); {$EndIf}
end {unit}.


