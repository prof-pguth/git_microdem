unit Demdefs;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of ianMICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2022 Peter L. Guth   }
{____________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
{$EndIf}

interface

uses
   {$IfDef VCL}
      Windows,Graphics,
   {$EndIf}

   {$IfDef NoClustering}
   {$Else}
      MVClusterClientDataSet,
   {$EndIf}

   System.UITypes, System.UIConsts,System.Types,
   FMX.Types3D,FMX.Types,
   Petmar_types,petmar,PetMar_db,Petmath;

const
   WebProgramDownLoadDir = 'https://www.usna.edu/Users/oceano/pguth/microdem/win32/';
   WebDataDownLoadDir = 'https://www.usna.edu/Users/oceano/pguth/downloads/';
   GoogleAPIsURL = 'https://maps.googleapis.com/maps/api/geocode/xml?';   //need for geocoding
   PythonEXEname = 'C:\OSGeo4W\apps\Python39\python.exe';
   PythonScriptDir = 'C:\OSGeo4W\apps\Python39\Scripts\';

   {$IfDef VCL}
      {$IfDef Win64}
         {$IfDef MonsterGrids}
            MaxElevArraySize = 500000;
            MaxColsInRAM = 500000;
         {$Else}
            MaxElevArraySize = 124000;
            MaxColsInRAM = 250000;
         {$EndIf}
         TheMaxPointsMemPtCloud = 96000000; //64,000,000
         MaxDEMDataSets = 2500;
         MaxDEMsToMerge = 2500;
      {$Else}
         MaxColsInRAM = 92000;
         MaxElevArraySize = 30000;
         TheMaxPointsMemPtCloud = 16000000; //16,000,000
         MaxDEMDataSets = 400;
         MaxDEMsToMerge = 500;
      {$EndIf}
      OverlayFExt = '.bmp';  //much faster than PNG
      MovieFileExt = '.bmp';
      MaxGridsInMG = 242;
      MaxDataBase = 120;
      MaxSatAllowed = 100;
   {$Else}  //FMX
      MaxColsInRAM = 12000;
      MaxElevArraySize = 30000;
      TheMaxPointsMemPtCloud = 16000000; //16,000,000
      OverlayFExt = '.png';
      MovieFileExt = '.png';
      MaxDEMDataSets = 3;
      MaxDEMsToMerge = 2;
      MaxDataBase = 5;
      MaxSatAllowed = 2;
   {$EndIf}
   MaxMultiGrid = 8;

{$IfDef Win64}
const
   MaxPts = 48000000;
{$Else}
const
   MaxPts = 24000000;
{$EndIf}
type
   tPointXYZ = record
      x,y,z : double;
   end;
   tPointXYZI = record
      x,y,z : double;
      int,int2,int3 : byte;
   end;
   tPointXYZArray = array[1..MaxPts] of tPointXYZ;
   tPointXYZIArray = array[1..MaxPts] of tPointXYZI;

type
   tColoredPoint = record
      x,y,z : single;
      Color : TAlphaColor;
   end;
   tColoredPointBuffer = array[1..MaxPts] of tColoredPoint;

const
   MaxThreadsAllowed = 24;


const   //for DEMIX
   MaxLandType = 8;
   NumDEMIXDEM = 6;
   RefDEMType : array[1..2] of shortstring = ('DSM','DTM');
   LandType : array[1..MaxLandType] of shortstring = ('ALL','CLIFF','STEEP','GENTLE','FLAT','URBAN','FOREST','BARREN');
   DEMIXDEMType : array[1..NumDEMIXDEM] of shortstring = ('FABDEM','COP','ALOS','NASA','SRTM','ASTER');


//satellite imagery definitions
const
   MaxSatCols = 92000;
   MaxBands   = 242;
   MaxClass    = 25;
   MinVATValue = -32;
   MaxVATCats = 64000;
   MaxNLCDCategories = 255;
   MaxVegLayers = 50;
   TMhistfName = '_bfrq.dbf';

const
   InMemoryStringSizeLimit = 1250000;

const
    Linear_Meters_9001 = 9001;
    Linear_Foot_9002 = 9002;
    Linear_Foot_US_Survey_9003 = 9003;

type
   tAllRefs = array[1..MaxBands] of integer;
   tElevColType  = array[0..MaxElevArraySize] of SmallInt;  {runs S->N}
   tElevColPointer = ^tElevColType;
   tElevCol32Bit  = array[0..MaxElevArraySize] of Integer;  {runs S->N}
   tElevCol32BitPointer = ^tElevCol32bit;
   tElevCol8Bit  = array[0..MaxElevArraySize] of byte;  {runs S->N}
   tElevCol8BitPointer = ^tElevCol8bit;
type
   tFloatRow = array[0..DEMDefs.MaxColsInRAM] of float32;
   tDoubleRow = array[0..DEMDefs.MaxColsInRAM] of float64;
   tInt32Row = array[0..DEMDefs.MaxColsInRAM] of Int32;
   tIntRow = array[0..DEMDefs.MaxColsInRAM] of Int16;
   tWordRow = array[0..DEMDefs.MaxColsInRAM] of word;
   tByteRow = array[0..DEMDefs.MaxColsInRAM] of byte;
type
   tDEMBooleans = array[1..MaxDEMDataSets] of boolean;
   tvdShift = (vdWGS84toEGM2008,vdEGM2008toWGS84,vdEGM96toEGM2008);


{$IfDef ExRiverNetworks}
{$Else}
   const   //for Hydrosheds river basins
      MaxRiverBasinNodes = 850000; //160000;
      MaxRiverSegs = 950000;
   type
     tRiverNode = packed record
        Lat,Long : float64;
        Basin : ShortString;
        DOWN,UP_MAIN,UP_TRIB,UP_TRIB2 : string[15];
        CONT_MAIN,CONT_TRIB,CONT_TRIB2 : integer;
        UP_MAIN_O,UP_TRIB_O,DOWN_ORD,UP_TRIB2_O : byte;
      end;
      tRiverNetwork = array[1..MaxRiverBasinNodes] of tRiverNode;
      pRiverNetwork = ^tRiverNetwork;
{$EndIf}

{$IfDef ExPointCloud}
{$Else}
   type
      tLasColorCoding = (lasccClass,lasccIntensity,lasccElevation,lasccReturnNumber,lasccReturnsPulse,lasccScanAngle,lasccGPSTime,lasccRGB,lasccPointSourceID,lasccUserData,lasccCloudID);
      tLidarDataSettings = packed record
       DiscardLowPointsNoise,
       DiscardHighNoise,
       AssumeLAS14classes,
       DiscardOverlap,
       AirReturnsOnly,
       SingleReturnsOnly,
       GroundClassOnly,
       FirstReturnsOnly,
       LastReturnsOnly,
       ScanAngleFiltered,
       SimpleBuildingFilter,
       UserDataRecordFiltered,
       PointIDFiltered,
       Filters : boolean;
       ColorCoding : tLASColorCoding;
       PointIDFilter : integer;
       ScanAngleFilter,
       UserDataRecordFilter,
       BadPointFilter,
       CatFilter,RetFilter : byte;
       DeleteLASAfterTransformation : boolean;
       MCC_scale,MCC_thresh : float32;
   end;

    tExportMode = (exmKML,exmDBF);

      tIcesat2 = packed record
         DoFilter  : boolean;
         BoxSize : int16;
         PCPtsRequired : int16;
         UseBeam : array[1..6] of boolean;
         BeamConfidence : array[1..6] of byte;
      end;

   const
      LASFieldName : array[tLasColorCoding] of ShortString = ('CLASS','INTENSITY','ELEV','RETURN','RETS_PULSE','SCAN_ANGLE','GPS_TIME','RGB','PT_SOURCE','USR_DATA','Cloud_ID');
      MaxLasLayers = 6;

{$EndIf}


type
   tSatSubset = (ssImageCoords,ssGroundCoords,ssAllImage,ssGraphical);
   tNewSatBand = (nsbAddimages,nsbNDWI,nsbNBRNormalizedburnindex,nsbNDVI,nsbNDSIsoil,nsbRatio,nsbFilter,nsbVARI,nsbGrayscale,nsbNDSIsnow,nsbPickem,
         nsbNDBIbuilding,nsbTOARefSolar,nsbTOARef,nsbTOARadiance,nsbBrightness,nsbSentinelReflectance,nsbGDVI,nsbLST);
   tMaxBandsArray = array[1..MaxBands] of float64;

   tUseBands = array[1..MaxBands] of boolean;
   ClassType = record
      UseClass    : boolean;
      ClassSize   : Int32;
      ClassColor  : tPlatformColor;
      ClassName   : shortstring;
      Mean,
      StdDev,
      ClassMin,
      ClassMax,
      ClassPerc5,
      ClassPerc10,
      ClassPerc25,
      ClassPerc90,
      ClassPerc95,
      ClassLowLimit,
      ClassHighLimit   : array[1..MaxBands] of float64;
   end;
   tClassLimitMethod = (clmMinMax,clm5_95,clm10_90,clm25_75,clmMeanStd,clmUser);

   tRow32Bit = array[0..pred(MaxSatCols)] of LongWord;
   tWordRow16Bit = array[0..pred(MaxSatCols)] of Word;
   tWordValues = array[0..MaxWord16] of Int32;
   tRow8Bit = array[0..pred(MaxSatCols)] of byte;

   ClassesType = array[1..MaxClass] of ClassType;

   tImageRow  = array[0..pred(MaxSatCols)] of byte;

   tMergeLas = (mlOnMap,mlOnMask,mlDEMCovered,mlInBox,mlThin,mlTranslate,mlScaleUp,mlRGBFilter);

const
   MinRefCount = 0;
   MaxRefCount = MaxWord16;
   MaxRefDir = 8;
   dtmMax = 0;
   dtmMean = 1;
   dtmMin = 2;
   dtmNearest = 3;
   dtmAll = 4;

   opOnGround = 0;
   opAboveGround = 1;
   opConstantHeight = 2;

   PixelIsArea = 1;
   PixelIsPoint = 2;

type
   tMDVersion = (mdMicrodem,mdWhitebox,mdGDAL,mdListGeo);

   tDistFreq  = array[MinRefCount..MaxRefCount] of Int32;
   tColorByte = array[MinRefCount..MaxRefCount] of TColor;
   tColorIndex = array[MinRefCount..MaxRefCount] of byte;
   tColorLookUp  = array[1..3] of ^tColorIndex;
   tlongRGB      = array[0..pred(MaxSatCols)] of tPlatformColor;
   tRGBWordLookUp = array[0..MaxRefCount] of TPlatformcolor;

   tVATColors = array[MinVATValue..MaxVatCats] of TRGBTriple;
   tXYZImport = (xyzUTM,xyzRect,xyzLatLong,xyzLongLat,xyzDDMMSSS);
   tElevStretch = (esNone,esPercentile,esSD);
   //tSPCSUnits = (spcsMeters,spcsIntFeet,spcsUSFeet);
   tStraightAlgorithm = (saDEMGrid,saUTM,saLatLong,saVincenty,saSmart);
   tHoleFill = (hfSeaLevel,hfOnlyHole,hfEverything,hfOnlyValid);
   tDTMoption = byte;

   tAngUnits = (auRadian,auDegree,auArcMin,auArcSec);
   tPointType = (EdgePoint,MissingPoint,FlatPoint,PitPoint,PeakPoint,RidgePoint,ValleyPoint,OtherPoint,PassPoint);
   tElevInterpolation = (piBilinear,piBicubic,piTriangle,piWeightedR,piWeightedR2,piNearestGrid,piSWGrid);
   tCompassDirection = (cdN,cdNE,cdE,cdSE,cdS,cdSW,cdW,cdNW,cdFlat,cdPit);
   tFilterCat =(fcMin,fcMax,fcMean,fcMedian,fcParamIsotrop,fcSum,fcSTD,fcNeighbors,fcNumNeigh,fcSaveByte,fcSaveSmallInt,fcSaveWord,fcSaveFloatingPoint,fcDissimilarNeighbors,fcFilFile,fcVectAvg);
   tSpeedUnit =(spMPS,spKPH,spMPH,spKnots);
const
   CompassDirectionNames : array[tCompassDirection] of ShortString = ('N','NE','E','SE','S','SW','W','NW','Flat','Pit');
   NominalCompassDirection : array[tCompassDirection] of integer = (0,45,90,135,180,225,270,315,-99,-99);

const
   MaxClouds = 5;

type
   tMaskGrid = (msSecondValid,msSecondMissing,msAboveSecond,msBelowSecond);
   tSecondGrid = (g2Feature,g2CHM,g2Change);
   tRidgeTypemap = (rtmRidge,rtmStream,rtmAllPoints);

   tRidgeAlgorithm = (raWood,raSimple,raSimple2);
   tcgHow = (cgValuesGrid,cgRadiusDB,cgBox,cgCode,cgPointDensity);
   tRangeCircleUnit = (rcFeet,rcYard,rcMeter,rcMile,rcNautMile);
   tProgramOption = (ExpertProgram,GeologyProgram,GeographyProgram,RemoteSensingProgram,DragonPlotProgram);  //ShipwrecksProgram,TCPProgram,EconProgram);
   tCoordUse = (coordLatLong,CoordMGRS,coordUTM,coordFullUTM);
   tContourColors = (ccSpecified,ccSingle,ccChromaDepth,ccTerrain);
   tMapMerge = (mmNone,mmElevation,mmSlope,mmReflectance,mmAspect);
   tShadeOpts = (soNone,soReflectance,soIntensity);
   tDEMRegion = (drEntireDEM,drFullMap,drPickBox);
   tDEMstatus = (dsSaved,dsUnsavedEdits,dsUnsaved);

const  //map display modes, for particular data types and desired look
   mtIHSReflect = 0;
   mtGrayReflect = 1;
   mtElevSpectrum = 2;
   mtDEMContour = 3;
   mtSlopeGrayScaleReversed = 4;
   mtDEMReflectElevMerge = 5;
   mtDEMBlank = 6;
   mtDEMaspect = 7;
   mtElevGray = 8;
   mtElevGrayReversed = 9;
   mtSatImageGray = 10;
   mtAnaglyph = 11;
   mtElevBands = 12;
   mtElevTerrain = 13;
   mtBlueGreenReflect = 14;
   mtElevRainbow = 15;
   mtElevIHS = 16;
   mtElevLandSea = 17;
   mtMergeTwoDEMs = 18;
   mtSatBlank = 19;
   //mtElevSpectrum = 20;
   mtVector = 21;
   mtNone = 22;
   mtElevFromTable = 23;
   mtElevDefinedPalette = 24;
   mtNoChange = 25;
   mtDEMMask = 26;
   mtDEMVATTable = 27;
   mtElevContrast = 28;
   mtFlowDir360 = 29;
   mtFlowDirArc = 30;
   mtFlowDirTau = 31;
   mt6ColorsReflect = 32;
   mtSlopeStandardCats = 33;
   mtSlopeTrafficCats = 34;
   mtSlopeGrayScale = 35;
   mtSlopeRainbow = 36;
   mtSlopePastel = 37;
   mtSlopeGoNoGo = 38;
   mtRefGrayBlue = 39;
   mtRefGrayColor = 40;
   mtRefColorGray = 41;
   mtOpenness = 42;
   mtLASclass = 43;
   mtRGB = 44;
   mtGYRReflect = 45;
   mtGGRReflect = 46;
   mtSatTrueColor = 47;
   mtSatFalseColor = 48;
   mtSatPickColor = 52;
   mtUnenhancedRGB = 53;
   mtLandCover = 54;
   mtGrCyBlReflect = 55;
   mtDEMaspectSlope = 56;
   //mtDifferenceMap = 57;

type
   tMapType = byte;

const
   MapFromMICRODEMstr : ANSIString = 'Map from MICRODEM';
type
   tMapOwner = (moNone,moDrapeMap,moPointVerificationMap,moIndexMap,moImageSelectionMap,moDEMSelectionMap,moVectorMap,moMapDatabase,moEditMap,moHiResIntervis);
   tASCIIFormat = (UTMFormat,LatLongFormat,LongLatFormat,MGRSFormat,GenerateFormat,Recenter);
   tWhatLoad = (wlPoint,wlDesiredSize,wlMapCoverage);

const   //database display modes
   dbasDefault = 1;
   dbasColorField = 2;
   dbasIHSField = 3;
   dbasIconField = 4;
   dbasIconAll = 5;
   dbasTTFontSymbol = 6;
   dbasConnectTwoPointsInRec = 7;
   dbasZValues = 8;
   dbasColorByString = 10;
   dbasColorByNumeric = 11;
   dbasTerrainFabric = 12;
   dbasVector = 13;
   dbasConnectSeqPts = 14;
   dbasAnimate = 15;
   dbasTimeSeq = 16;
   dbasMonthlyTemp  = 17;
   dbasMonthlyRain  = 18;
   dbasMonthlyFilter = 19;
   dbasBuffers = 20;
   dbasBeachBall = 21;
   dbasDipStrike  = 22;
   dbasMultiFieldRGB  = 23;
   dbasColorPosNeg  = 24;
   dbasScaledSquares  = 25;
   dbasScaledCircles  = 26;
   dbasPointsInDB = 27;
   dbasGaz = 28;
   dbasQuakeMechColor = 29;
   dbasTernary = 30;
   dbasKoppen = 31;
   dbasZipatoneField = 32;
   dbasZipAToneAll = 33;
   dbasColorJoinField = 34;
   dbasTiger = 35;
   dbasOSM = 36;
   dbasQuakeIcons = 37;

   dbcmConstant = 0;
   dbcmFieldLinear = 1;
   dbcmFieldQuantile = 2;
   dbcmFieldLog = 3;

const
   LVISGabonHist = 0;
   LVISGabonCum  = 1;
   LVISGabonBoth = 2;

type
   tCanEditGIS = (egisNever,egisSometimes,egisAlways);
   tGISSymbolSize = (gisSizeDBfield,gisSizeConstant);
   tGISColorSource = (gisColorConstant,gisColorNumField,gisColorStringField,gisColorDBField);
   tDefaultVectorMapProject = (dvmMercator,dvmConusMerc,dvmConicalArea,dvmConicalConformal,dvmDefined);
   tVegOptionMap = (voDTM,voVeg,voDSM);
   tAddGeometry = (agCentroid,agAreaKM2,agAreaM2,agPerimeter,agLength,agZStats,asSinuousity,agNumPts,agElevationDeltas,agDirection,agEndPoints,agCompact,agMeanWidth,agShapeNum,agSchwartz,agP2A);
   tMultiFieldStats = (mfsSum,mfsMean,mfsMedian,mfsMin,mfsMax);

   tHowZoom = (hzNoZoom,hzZoomIn,hzZoomOut);
   tMarkShift = (msNone,msUTM,msLatLong,msBoth);
   tBasicProjection = (bpUTM,bpLatLong,bpOther);
   tFanPickMode = (fpSingle,fpMultipleAsk,fpMultipleSame);
   tSliceColorOpt = (scoElevation,scoClass,scoRetNum,scoRGB,scoCloudID,scoGray);

   tImageryFrame = array[0..1024*1024] of byte;
   tDEMbooleanArray = array[1..MaxDEMDataSets] of boolean;
   tSunriseSunsetAngle = (Sun,Civil,Nautical,Astronomical);

   {$IfDef ExGeography}
   {$Else}
      tKoppenDisplay = (kdClass,kdTemp,kdPrecip,kdRange);

      tSunriseOptions = record
         Day,Month,Year,DiffUTC : int16;
         Morning : boolean;
         SunAngle : tSunriseSunsetAngle;
      end;
  {$EndIf}

  {$IfDef ExGeostats}
  {$Else}
     tGeomporphBlock = (gbGrid,gbDB,gbMapOnly,gbPointNeighborhood,gbPolygon,gbDEM);
  {$EndIf}


  {$IfDef ExGeology}
  {$Else}
      tFocalPlaneWhat = (fpRoseStrike,fpRoseDipDir);
      tBroadCastPlaneData = record
         xutm1,yutm1,xutm2,yutm2,xutm3,yutm3,Lat,Long : float64;
         z1,z2,z3,Dip,aStrike,DipDir : float32;
         xglo,xghi,yglo,yghi : integer;
         DipAndStrike,SlopeStr       : string16;
      end;
  {$EndIf}

   tSlopeAspectRec = record
       z,znw,zw,zsw,zn,zs,zne,ze,zse,
       dzdx,dzdy,
       dx,dy,
       GridTrueAngle,
       Slope,
       SlopePercent,
       SlopeDegree,
       AspectDir,AspectDirGrid : float32;
       Dir : tCompassDirection;
   end;


   tLocalOptimaOpts = record
      ColInc,
      RowInc,
      RoadMaskDistance,
      NPts : int32;
      MaxSlope : float64;
      EdgeBuffer,
      RoadMask,
      SlopeMask,
      BitmapMask : boolean;
   end;

   {$IfDef ExSidescan}
   {$Else}
      tSonarMapDef = packed record
         SideDefFilter : byte;
         DefaultPixelsWide,
         DefaultPixelsHigh,
         BottomValue,
         GridSpaceAcross,
         GridSpaceAlong,
         SidescanLayback : integer;
         MinPC,
         MaxPC,
         TowDistance,
         MinDraft,
         InstrumentDepth : float64;
         OverlayGrid,
         SpeedCorrection,
         WhiteIsStrongReturn,
         SlantRangeCorrect,
         CustomPalette : boolean;
         SSGridWidth : byte;
         SSGridColor : tPlatformColor;
      end;
   {$EndIf}


  tDEMDoingWhat = (JustWandering,PlottingPointElevations,
        FirstDistancePoint,SecondDistancePoint,
        FirstZigDistance,LaterZigDistance,
        FirstBearingPoint,SecondBearingPoint,
        FirstSlopePoint,SecondSlopePoint,
        SeekingCenterMeshGrid,
        SeekingFlyThroughRoute,
        SeekingPerspective,SeekingSecondPerspective,

        SeekingLOS,SeekingSecondLOS,
        SeekingAverageProfile,SeekingSecondAverageProfile,
        SeekingTopoProfile,SimpleTopoProfileRight,
        MultipleLOS,MultipleTopoProfileRight,

        TerrainBlowup,
        NewCoverage,OutlineDBIrregularMask,
        FanSensitivity,DragEdit,RadiusDBEdit,InsertDBPoint,

        PlottingOffset,OpenMrSid,PtCloudExtractPoints,
        MoveMapBox,SeekingStreamProfile,CompareFanAlgorithms,
        ErasingPoints,StreamDistance,NewTrack,DeletePointDBRecs,EditPointDBRecs,

        StartAccumulatedCostSurface,StartLeastCostPath,
        PickSliceLocation,PickSlicePanorama,PickPointCloudStats,

        {$IfDef ExGeology}
        {$Else}
           SeekingFirstThreePoint,SeekingSecondThreePoint,SeekingThirdThreePoint,
           SeekingPlaneContact,
           SeekingThickness,
           TraceContact,
           PickEulerPole,GetGeologySymbols,
           SeekingLeftSideMagModels,SeekingRightSideMagModels,ProjectFocalMechToSurface,
           EarthquakeFocalMech,GetDriftVectors,GetSpreadingRate,GetStratcolColumn,
        {$EndIf}

        {$IfDef ExMilicons}
        {$Else}
           EditMilIcons,AddMilIcon,
        {$EndIf}

        {$IfDef ExPLSS}
        {$Else}
           PLSSposition,
        {$EndIf}
        
        {$IfDef ExDrainage}
        {$Else}
           FloodBasin,DrainageArea,
        {$EndIf}

        {$IfDef ExGeostats}
        {$Else}
           LagSizeSensitivity,
           GeomorphPointGraph,
        {$EndIf}

        {$IfDef ExFresnel}
        {$Else}
           FirstFresnelPoint,SecondFresnelPoint,
        {$EndIf}

        {$IfDef ExRedistrict}
        {$Else}
           RecolorRedistrict,RecolorRedistrictBox,
        {$EndIf}

        RegionDNs, EnsembleClassSummary,NDVIPointTimeSeries,
        SpectralReflectance,
        Scribble,
        NLCDClassification,NLCDBox,
        PickTrainingPoints,PickTrainingBox,

        GetIslandArea,
        FirstTimeSeries,SecondTimeSeries,
        VisPolarOrbiter,
        FirstRequiredAntenna,
        InteractiveLOS,
        GetRGBValues,
        FirstPointSelectionAlgorithm,SecondPointSelectionAlgorithm,
        SeekingFirstNewPanorama,SeekingSecondNewPanorama,
        RecolorShapeRecord,FloodFill,
        GeodeticBearing,UTMTrueDeviation,
        QuickWeaponsFan,EditWeaponsFans,RangeCircles,GrainByRegionSize,GetPointFabric,
        Calculating,
        ShapeFirstLine,ShapeFirstPolygon,ShapePolygon,ShapeTrack,
        ShapePoint,ShapeXYZPoint,ShapeLine,
        ShapePointsAlongFirstLine,ShapePointsAlongSecondLine,
        EditPointElevs,
        MapTissotIndicatrix,ShowMagneticVariation,
        SunriseClicking,
        VerifyingPoint,EditFlightPathOnMap,
        OpenMapsFromLibrary,SeekingFirstCircleFly,SeekingSecondCircleFly,SeekingThirdCircleFly,
        RouteObservation,LiveFly,LiveFly2,
        PickToBroadcast,RoamBroadcast,
        CalculateArea,CalculateVolume,GraphFilterDB,
        SubsetByOutline,SubsetLake,SubsetHole,ReplaceValuesByOutline,FillHolesByOutline,
        IDDataBaseOne,IDDataBaseAll,LabelIDDataBase,GraphicalResizeWindow,GetPointSymbols,CornerEditBox,FindBlockHorizon, IDDBforAction,
        GetGreatCircleRoute, PlotNorthArrow,DeleteSingleDBRecs,DeleteMultipleDBRecs,DeleteMultipleRecsAllDBs,
        MovePointDBRecs,EditDBRecs,EditZDBRecs,
        USCounty,DigitizeContourPoint,DigitizeContourStream,PickCenterAndScale,PickDBRecsToMove);


   tGridLimits = packed record
      XGridLow,YGridLow,XGridHigh,YGridHigh : Int32;
   end;

{$IfDef ExDTED}
{$Else}
   tUHL = array[1..80] of AnsiChar;
   tDSI = array[1..648] of AnsiChar;
   tACC = array[1..2700] of AnsiChar;
{$EndIf}

   tScreenXArray = array[0..MaxScreenXMax] of integer;
   tScreenXArrayPointer = ^tScreenXArray;

const
   UTMbasedDEM = 0;
   ArcSecDEM = 1;
   WKTDEM = 2;

const
   InitDEMzero = 0;
   InitDEMnone = 1;
   InitDEMvalue = 2;
   InitDEMmissing = 3;

type   //for DEM Header record
   tDEMAvailable = byte;
   tSpacingUnit = (SpaceMeters,oldSpaceSeconds,oldSpaceMinutes,SpaceKM,Space100m,SpaceFeet,SpaceKFeet,SpaceDegrees,DeadSpaceHundredthSecond,unusedMercProj100m,
        DeadOption,Space10m,DeadSpaceTenthSecond,DeadSpaceTenThousandthDegree,SpaceIntFeet,SpaceUSFeet);
   tElevUnit = byte;
const
   //these were initially types, using these values
   //over time some values were no longer used, and to keep the code simpler these were changed to constants
   euMeters = 0;
   Feet = 1;
   TenthMgal = 2;
   Milligal = 3;
   TenthGamma = 4;
   Decimeters = 5;
   Gammas = 6;
   HundredthMGal = 7;
   DeciFeet = 8;
   Centimeters = 9;
   //Unused10 = 10;
   HundredthMa = 11;
   PercentSlope = 12;
   Undefined = 13;
   zDegrees = 14;
   Unused0 = 15;
   lnElev = 16;
   LogElev = 17;
   //Unused1 = 18;
   //Unused2 = 19;
   zPercent = 20;
   //Unused3 = 21;
   //Unused4 = 22;
   //Unused5 = 23;
   //Unused6 = 24;
   //Unused7 = 25;
   NLCD2001up = 26;
   LandFire = 27;
   Nanotesla = 28;
   NLCD1992 = 29;
   euIntCode = 30;
   //unused8 = 31;
   //unused9 = 32;
   GLOBCOVER = 33;
   GLC2000 = 34;
   euImagery = 35;
   euMM = 36;
   euMetersPerSec = 37;
   zMperM = 38;
   euKM = 39;
   CCAP = 40;
   euLASclass13 = 41;
   euLASclass14 = 42;
   euRGB = 43;
   euMonth = 44;
   CCI_LC = 45;
   S2GLC = 46;
   NLCD_Change = 47;
   GLCS_LC100 = 48;
   Meybeck = 49;
   Geomorphon = 50;
   Iwahashi = 51;
   ESRI2020 = 52;
   AspectDeg = 53;
   euPennock = 54;
   euPerMeter = 55;
   WorldCover10m = 56;
   euNDVI = 57;
   euNBR = 58;
   euDifference = 59;
   euElevDiff = 60;
   HighElevUnits = 60;
(*
type
   tElevUnit = (euMeters,Feet,TenthMgal,Milligal,TenthGamma,
                  Decimeters,Gammas,HundredthMGal,DeciFeet,Centimeters,
                  Unused10,
                  HundredthMa,PercentSlope,Undefined,zDegrees,
                  Unused0,
                  lnElev,LogElev,
                  Unused1,Unused2,
                  zPercent,
                  Unused3,Unused4,Unused5,Unused6,Unused7,
                  NLCD2001up,LandFire,Nanotesla,NLCD1992,
                  euIntCode,unused8,unused9,GLOBCOVER,GLC2000,euImagery,euMM,euMetersPerSec,zMperM,euKM,CCAP,euLASclass13,
                  euLASclass14,euRGB,euMonth,CCI_LC,S2GLC,NLCD_Change,GLCS_LC100,Meybeck,Geomorphon,Iwahashi,ESRI2020,AspectDeg,euPennock,euPerMeter,WorldCover10m,euNDVI,euNBR);
const
   ElevUnitsAre : array[tElevUnit] of string10 =
       (' m',' m',' mgal',' mgal',' gamma',
        ' m',' gamma',' mgal',' m',' m',
        '',' Ma','%','','°',
        '','ln(z)','log(z)','°','',
        '%','','°','','',
        '',' NLCD',' LandFire',' nT','NLCD 1992',
        'code','','','GlobCover','GLC2000',' Imagery',' mm',' m/s',' m/m',' km',' C-CAP',' LAS1.3',' LAS1.4',' RGB',' month',
        ' CCI-LC','S2GLC','dNLCD','GLCS_LC100','Meybeck','Geomorphon','Iwahashi','ESRI2020','°','Pennock','/m','WorldCover 10m','NDVI','NBR');

   ElevUnitsAre : array[tElevUnit] of string16 =
        (' meters',' feet',' 0.1 mgal',' mgal',' 0.1 gam',
        ' decim',' gamma',' 0.01 mgal',' 0.1 ft',' cm',
        ' (?)',' 0.01 Ma',' %',' undefined','°',
        ' hundredth','ln(z)','log(z)','0.1°',' thousandth',
        '0.01 %','tenth','0.01°',' tens',' hundreds',
        ' thousands',' NLCD',' LandFire',' nT',' NLCD 1992',
        ' Integer code','','','GlobCover','GLC2000',' Imagery',' mm',' m/s',' m/m',' km',' C-CAP',' LAS1.3',' LAS1.4',
        'RGB',' month','CCI-LC','S2GLC','NLCD_change','GLCS_LC100','Meybeck','Geomorphon','Iwahashi','ESRI2020','°','Pennock','/m','WorldCover 10m',NDVI','NBR');
   *)

   VertCSEGM96 = 5773;
   VertCSEGM2008 = 3855;
   VertCSWGS84 = 4096;
   VertCSNAVD88 = 5703;

{$IfDef MultipleCurvatureMethods}
   type
      tCurvatureMethod = (cmEvans,cmShary,cmHeerdegenAndBeran,cmZevenbergenAndThorne);
   const
      CurvatureMethodName : array[tCurvatureMethod] of ShortString = ('Evans','Shary','Heer&Ber','Zev&Thor');
{$EndIf}


type
   tDigitizeDatum = (WGS72d,WGS84d,NAD27d,NAD83d,Spherical,UnusedddLocal,Rectangular,unusedLamAzEqAreaSphere,unusedSinusEllip,unusedPRDd,UK_OS_grid,ddDefined,unusedMarsD,unusedVenusD,ETRs89d);

   tDEMprecision = (SmallIntDEM,FloatingPointDEM,ByteDEM,WordDEM,LongWordDEM);

   tProjectType = (AlbersEqAreaConicalEllipsoid,Cassini,EquiDistantCylindrical,Gnomonic,HammerProj,LamAzEqArea,Mercator,MercatorEllipsoid,Mollweide,
      OrthoProj,PolarStereographicEllipsoidal,SinusProj,SinusEllipsoidal,OldStereographic,UTMEllipsoidal,VanDerGrinten,MillerCylindrical,
      CylindricalEqualArea,LambertConformalConicEllipse,UK_OS,Finn_GK,GeneralTransverseMercator,PlateCaree,LamAzEqAreaEllipsoidal,SphericalStereographic,
      WebMercator,IrishGrid,UndefinedProj,EqualEarth,CylindricalEqualAreaEllipsoidal,AzimuthalEquidistantEllipsoidal);

   tDMAMapRawDefinition = packed record
      h_Adat,
      h_f          : double;
      h_XDat,
      h_YDat,
      h_ZDat       : int16;
      h_DatumCode  : array[1..6] of byte;
      h_EllipsCode : array[1..3] of byte;
   end;

   tDEMheader = packed record  //for a DEM, version 4, introduced July 2014
      DEMUsed      : byte;
      DEMPrecision : tDEMprecision;
      DataSpacing  : tSpacingUnit;
      ElevUnits    : tElevUnit;
      StoredMaxElev,
      StoredMinElev,
      MaxElev,               {max elevation in data set}
      MinElev,               {min elevation in data set}
      DEMySpacing,           {y spacing}
      DEMxSpacing : float32; {x spacing}
      DEMSWCornerX,
      DEMSWCornerY : float64;
      VerticalCSTypeGeoKey,
      UTMZone  : Int16;     {6 degree UTM Zone number, USGS/MGRS standard: 1 = W177, 60 = E177}
      DMAMapDefinition  : tDMAMapRawDefinition;
      DigitizeDatum : tDigitizeDatum; {sets datum for DEM, and it is transformed to the desired local datum for use}
      LatHemi    : AnsiChar;       {N or S}
      NumCol,NumRow  : int32;
      RasterPixelIsGeoKey1025 : byte;
      WKTString : ANSIString;
   end;

{$IfDef AllowV1V2V3DEMHeaders}
   {$I old_dem_headers.inc}
{$EndIf}

   tPrinterLegend = packed record
      ShowTitle,
      ShowScaleBar,
      ShowDeclinationDiagram,
      ShowTerrainCatLegend,
      ShowGISLegend,
      ShowColorLegend  : boolean;
   end;

type
   tLandsatMetadata = record
      TM_No : byte;
      Sensor : ANSIChar;
      Path,Row,Year,JDay : int16;
      Date : ShortString;
      Sensor_ID,
      SceneTime : ShortString;
      LatHi,LongHi,LatLow,LongLow,
      CloudCover,
      SunAzimuth,
      SunElevation : float64;
      ImageQuality : int16;
      BandFileName : array[1..11] of PathStr;
      RadianceMult,RadianceAdd,
      ReflectanceMult,ReflectanceAdd : array[1..11] of float64;
      K1Const,K2Const  : array[10..11] of float64;
   end;

type
   tGDALinfo = record
       HDatum : ANSIstring;
       NumCol,NumRow : integer;
       dx,dy,
       ulLat,ulLong,
       cLat,cLong : float64;
       inEPSG,utmEPSG : integer;
       UTMZone,
       FIPS : int16;
       Hemi : ANSIchar;
   end;

const  {for ESRI shapefiles}
   sfMaxParts = 25000;
   sfMaxPoints = 5000000;

type   {for ESRI shapefiles}
   sfMainFileHeader = packed record
      FileCode : int32;
      Unused   : array[1..5] of int32;
      FileLength : int32;
      Version    : int32;
      ShapeType  : int32;
      BoundBox   : sfBoundBox;
      BoundBoxZMin,
      BoundBoxZMax,
      BoundBoxMMin,
      BoundBoxMMax : float64;
   end;
   sfIndexRecord = packed record
      Offset,ContentLength : Int32;
   end;
   sfPolyLineHeader = packed record
      RecordNumber,
      ContentLength,
      ShapeType : int32;
      BoundBox  : sfBoundBox;
      NumParts,NumPoints : int32;
   end;
   sfPoints = packed record
      x,y  : float64;
   end;
   sfPoints3D = packed record
      x,y,z : float64;
   end;
   sfPointsWithHeader = packed record
      RecordNumber,
      ContentLength,
      ShapeType : int32;
      x,y       : float64;
   end;
   sfPointsZWithHeader = packed record
      RecordNumber,
      ContentLength,
      ShapeType : int32;
      x,y,z     : float64;
   end;

type
   tLotsOfPoints = packed array[1..sfMaxPoints] of sfPointsWithHeader;
   tLotsOfPoints3D = packed array[1..sfMaxPoints] of sfPointsZWithHeader;

   tReprojectType = (rpjEqAreaConicToLatLong,rpjLatLongToUTM,rpjDatumShift,rpjUTMtoLatLong,rpjSpecifyShift,rpjLambConfConicToLatLong,rpjLambertAzEqAreaToLatLong,rpjArbitraryLatLong,rpjUseMDprjFile);
   tFieldsToAdd = (afBoundingBox,afLineMerge,afXYZ,afLatLong);
   tPartSize = packed array[1..sfMaxParts] of int32;

   //tTauDEM = (tdPitRemove,tdD8Flow,tdDinfFlow,tdD8ContArea,tdDInfContArea,tdGridNet,tdPeukerDouglas,tdStreamReachAndWatershed);
   tImageType = (itSat,itDRG);

const
   StraightAlgorithmName : array[tStraightAlgorithm] of ShortString = ('DEM Grid','UTM','Lat/Long','Geodetic','Smart');
   ElevInterpolationName : array[tElevInterpolation] of ShortString = ('Bilinear Interpolation','Bicubic interpolation','Grid triangle','1/R weighting','1/R² weighting','Nearest grid','SW grid');
   DigitizeDatumName : array[tDigitizeDatum] of ShortString = ('WGS72','WGS84','NAD27','NAD83','Sphere','Local','Rect','LamAzSph','PUR','Sinus El','UK OS','Defined','MARS','Venus','ETRS89');
   SpacingUnits : array[tSpacingUnit] of ShortString = (' m',' sec',' min',' km',' 100m',' ft','k ft',' deg','0.01 sec',' m(M)','100m(PS)','10m','0.1 sec','0.0001 deg',' Int Feet',' US feet');

type //for MICRONET
   tInputDisplay    = (Pole,DipDirDis,GreatCircle,Both);
   NetType          = (Schmidt,Wulff);
   tPoleOrLine      = (PolePlot,LinePlot);
   tHemisphere      = (Upper,Lower);
   PlotTypes        = (aLineation,aDipDirection,aDipAndStrike,aLatAndLong);
   tNetGrid         = (ngPolar,ngEquatorial,ngAzimuth,ngNone);
   tNetContourColors = (Spectrum,Rainbow,Terrain,GrayScale,ContrastBW,GrayDither);
   tBeachBallSize  = (bbsAll,bbsMs,bbsMw);
   tBeachBallColors  = (bbcAll,bbcMs,bbcMw,bbcDepth);

   {$IfDef ExMICRONET}
   {$Else}
      tMicronetDefaults = packed record
         DrawGridCircles     : tNetGrid;
         FormLegend,
         NorthTick,
         CenterTick,
         ContinuousGrayScale : boolean;
         NetUsed          : NetType;
         HemiSphereUsed   : tHemisphere;
         InputDisplay     : tInputDisplay;
         NetContourColors : tNetContourColors;
         NetScreenMult    : int16;
         BeachBallSize    : tBeachBallSize;
         BeachBallColor   : tBeachBallColors;
         MinContourConcentration,
         MaxContourConcentration,
         MaxColorMagnitude,
         MinColorMagnitude,
         MaxColorDepth,
         MinColorDepth,
         MaxScaleMagnitude,
         MinScaleMagnitude : float64;
         AllBeachBallSize,
         M3BeachBallSize,
         M9BeachBallSize,
         MaxNumBeachBalls,
         NetColor,
         ScreenSymbolSize,
         CircleGridIncrement : int16;
         NetLineWidth : byte;
         NetLineColor : tPlatformColor;
         GreatCircleLineWidth : byte;
         GreatCircleColor : tPlatformColor;
      end;
   {$EndIf}

{$IfDef ExStratcol}
{$Else}
   type  //for STRATCOL
      ThickLabelOption = (StartTop,StartBottom);
      tLabelUnits   = (luMeters,luFeet);
      tTextDirection = (textHorizontal,TextVertical);
      tTextPlacement = (textInside,TextBeside);
      tTextLabels = (textShort,textLong,textNone);
      tLocationLabel = (llNone,llLatLong,llText);
      ZoneSampleType   = (NoZones,ZoneTick,SampleTick);
      tAlignColumns = (acTop,acBase);
      tColorAndPatternOptions = (ColoredPatterns,patSolidColors,patBlackAndWhitePatterns,patNoPatterns);
      tStratColDefaults = packed record
         BoundaryTicks,
         RaggedRightMargin,
         AutoShortLabels,
         FancyUnitBases,
         OverPrintLithology,
         VariableResistance,
         IgnoreCorrelationThickness,
         ColumnVerbiage,
         EnterLatLong,
         RapidColEntry,
         ShowAgeBar         : boolean;
         TextDirection      : tTextDirection;
         TextPlacement      : tTextPlacement;
         ZoneTicks          : ZoneSampleType;
         ThickLabelling     : ThickLabelOption;
         ThickLabelUnits    : tLabelUnits;
         TextLabels         : tTextLabels;
         AlignColumns       : tAlignColumns;
         LocationLabel      : tLocationLabel;
         PixelsColumnWide,
         DefaultMyBitmapWidth,
         DefaultMyBitmapHeight,
         DefaultFontSize,
         DefaultScaleFontSize,
         UsersMaxUnits,
         ColumnSeparation  : int16;
         DefaultThickness : float64;
         ColorAndPatternOptions : tColorAndPatternOptions;
         ScaleLabelOffset : float64;
         AbsThickness,
         RightSideThickness : boolean;
      end {record};
{$EndIf}


type
   CurScreenMapType = packed record
      ProjDX,ProjDY,
      GridDX,GridDY      : float64;
      BoundBoxDataGrid,
      BoundBoxGeo,
      BoundBoxUTM,
      BoundBoxProj : sfBoundBox;
   end;
   LongReal        = array[0..100] of float64;
   LongRealPointer = ^LongReal;
   ColorCutArrayType = array[0..14] of float64;
   tGridZ = packed record
      x,y : Int32;
      z   : float64;
   end;

type
   tLOSAlgorithm = (losMicrodemFractional,losMicrodemConstant,losYoeli,losBresenham);
   tLOSVariety = (losVanilla,losMagModel,losAllDEMs,losAllDEMDropDown,losSimpleMagModel,losSimpleOne);
   tLOSResult = (losIsVisible,losBlockByTerrain,losCenterFresnelIntrusion,losEdgeFresnelIntrusion);

   tFanMethod = (fmFanRadials,fmAllPointToPoint,fmRadialIHS);
   tShowPointCloundOnProfile = (spcNone,spcPoints,spcDensity);
   tDEMZunits = (zuMeters,zuFeet);
   tFanShow = (fsMasked,fsVisible,fsBoth);
   tVerticalCurvAlg = (vcNoCurvature,vcTM5441,vcRadioLineOfSight,vcYoeli);
   tIntervisibilityAlgorithm = packed record
      LOSAlgorithm : tLOSAlgorithm;
      FanMethod : tFanMethod;
      FanShowVisible : tFanShow;
      FanViewerMustBeGrid,
      FanTargetMustBeGrid : boolean;
      FanCurvAlg : tVerticalCurvAlg;
      StraightAlgorithm : tStraightAlgorithm;
      ElevInterpolation : tElevInterpolation;
      MaskAreaInterval,
      MaskRaySpacingDeg,
      SmartSwitchOver,
      FanDEMSpaceMultiple,
      FanMapSpaceMultiple : float32;
      ClosestBlockingDistance : int32;
   end;

const
   BaseCurvAlgName : array[tVerticalCurvAlg] of ShortString = ('None','TM5-441','Radio','Yoeli');
   LOSAlgorithmName : array[tLOSAlgorithm] of ShortString = ('Scale radial','Const radial','Grid sides','Nearest grid');
   FanMethodName  : array[tFanMethod] of shortstring = ('Radials, discrete','Point to point','Radials, full');
   FanShowName  : array[tFanShow] of ShortString = ('Masked','Visible','Both');

type
   tWeaponsFan = packed record
      Fan_Name                               : shortstring;
      W_Lat,W_Long,W_Range,W_Up,W_TargetUp,
      StartAngle,EndAngle,UpAngle,DownAngle  : float64;
      FanZoomFactor,
      Vis_Hue,Vis_Sat,
      Mask_Hue,Mask_Sat                      : int32;
      noUseSensorNoTerrainBlock,                                     //allows for acoustic sensors
      ObserverTerrainHug,
      TargetTerrainHug                       : boolean;
      ThisMaskColor,
      ThisFanColor                           : tPlatformColor;
      FanShowWhat                            : tFanShow;
      FanFileName                            : PathStr;
   end;

   tDirToUse = array[1..8] of boolean;

   tTerrainCatDefinition = packed record
      CatReliefRadius,
      CatMinRelief,CatMaxRelief : int32;
      CatMinElev,CatMaxElev,
      CatMinSlope,CatMaxSlope   : float32;
      CatAspects                : Set of tCompassDirection;
      UseElevation,UseSlope,
      UseAspect,UseRelief       : boolean;
      CatColor                  : tPlatformColor;
   end;

   {$IfDef ExVariogram}
   {$Else}
      tVariogramOptionsRecord = packed record
         DoGraph,DoGamma,DoSlopes,OldMethod,
         LogLog,SemiVar,ShowTextOutput : boolean;
         DistanceOut,PointsRequired,
         Skip,GraphSkip : int32;
      end;
   {$EndIf}

   {$IfDef ExPLSS}
   {$Else}
      tPLSSFormat = (plssTRS,plssSTR);
      tPLSSDef = packed record
          PLSSShowQuarters,
          PLSSShowSections,
          PLSSShowTowns,
          PLSSLabelSections,
          PLSSLabelTowns,
          PLSSQuartersInLabels,
          PLSSLotsInLabels    : boolean;
          PLSSTownColor,
          PLSSSectionColor,
          PLSSQuarterColor   : tPlatformColor;
          PLSSTownWidth,
          PLSSSectionWidth,
          PLSSQuarterWidth   : byte;
          PLSSAppearTowns,
          PLSSAppearSections,
          PLSSAppearQuarters     : int16;
          AutoDrawPLSS,
          PLSStoRAM,
          PLSSsmartScaling : boolean;
          PLSSFormat : tPLSSFormat;
          SectFont,TownFont : tMyFont;
      end;
   {$EndIf}

   {$IfDef ExTiger}
   {$Else}
   tTigrDef = packed record
      ShowTIGERButton,
      ShowNeighborsOnTIGERCounty,
      AutoTigerOnImages,
      AutoTigerOnDEMs  : boolean;
      MaxAutoTigerCounties : int16;
      WaterColor1,
      WaterColor2,
      BoundaryColor,
      MajorRoadColor,
      RoadCat2Color,
      RoadCat3Color,
      RoadCat4Color,
      RoadCat5Color,
      RoadCat6Color,
      RoadCat7Color,
      RailroadColor,
      PowerLineColor,
      PipelineColor : tPlatformColor;

      WaterWidth1,
      WaterWidth2,
      BoundaryWidth,
      MajorRoadWidth,
      RoadCat2Width,
      RoadCat3Width,
      RoadCat4Width,
      RoadCat5Width,
      RoadCat6Width,
      RoadCat7Width,
      RailroadWidth,
      PowerLineWidth,
      PipelineWidth : Byte;

      AutoAppear,
      DrawLabels,
      DrawStreams,
      DrawCoastline,
      DrawBoundary,
      DrawMajorRoad,
      DrawRoadCat2,
      DrawRoadCat3,
      DrawRoadCat4,
      DrawRoadCat5,
      DrawRoadCat6,
      DrawRoadCat7,
      DrawRailroad,
      DrawPowerLine,
      DrawPipeline : boolean;

      AppearLabels,
      AppearStream,
      AppearCoast,
      AppearPipeLine,
      AppearPowerLine,
      AppearMajorRoad,
      AppearRoadCat2,
      AppearRoadCat3,
      AppearRoadCat4,
      AppearRoadCat5,
      AppearRoadCat6,
      AppearRoadCat7 : int16;
   end;
   {$EndIf}

   tOGLDefs = packed record
      MoveIncr  : float64;
      //MaxOpenGLPoints,
      //MaxInitOpenGLTriangles : int32;
      OpenGLDefaultTopx,
      OpenGLDefaultTopY,
      OpenGLDefaultWidth,
      OpenGLDefaultHeight : int16;
      DrawOGLAxes : boolean;
   end;

  {$IfDef ExGeography}
  {$Else}
     tKoppenOpts = packed record
        KopWidth,KopHeight : int32;
        ShowLatLong,
        ShowElevation,
        ShowSunRiseSet,
        ShowTempAndRain   : boolean;
        KoppenFontSize : byte;
        MaxTemp,MaxPrecip : int16;
     end;
  {$EndIf}

   tPerspectiveType = (FishnetPlainPerspective,FishnetChromaPerspective,ReflectancePerspective,BMPPerspective);
   tPerpsectiveStereo = (psNone,psAnaglyphShift,psAnaglyphDual,psStereoPair);
   tPerspectiveOptions = record
       {$IfDef FMX}
          HFOVSetting,
          VFOVSetting,
          DepthSetting,
          ObsUpSetting : byte;
       {$EndIf}
       ViewShedFanWithPerspective,
       NapEarth,
       SaveAsProgramDefaults,
       UsersSky,
       NoVE,
       OutLineCrests,
       LabelPerspectiveViewPort,
       CloudBackground,
       PersVaryResolutionAlongRadial,
       TitlePerspectiveViewPort : boolean;
       PerspAbsElev,
       PersFirstProfile,
       FlyDepth,
       PersViewDepth,
       PerspAnaglyphShift,
       PerspAnaglyphSeperate : int32;
       PersWidth,PersHeight,
       PanWidth,PanHeight,
       PersDistBetweenProf,
       PersDrapedBetweenProf,
       PersDrapedBetweenImages,
       CrestSeparator,
       PersAzimuth,
       PersMeshSpace        : int16;
       PersObsUp            : float32;
       WhichPerspective     : tPerspectiveType;
       PerpsectiveStereo : tPerpsectiveStereo;
       CrestColor : tPlatformColor;
       CrestLineWidth : byte;
       VertLabelInc,
       HorizLabelInc : byte;
       rgbtSky  : tRGBtriple;
       PersVaryResRanges : array[1..4] of int32;
       PersVaryResFactors : array[1..4] of byte;
   end;

   {$IfDef ExFly}
   {$Else}
      tFlyOptions = packed record
          LiveFlyAutoNap,
          LiveFlyMovie,
          FlySideBySide,
          ShowFlyScene,
          ShowFlyThroughRoute   : boolean;
          FlyVFOV,FlyHFOV,
          TargetFOV1,TargetFOV2,
          DeltaHeading          : float64;
          DeltaSpeed,
          FlyThroughHeight,
          FlyThroughWidth,
          FlySceneSeparation    : int32;
          NumTargetViews,
          NumFlyDrapes,
          DeltaZ,
          LiveFlyDelay,
          FlyHeight : int16;
        end;
     {$EndIf}

const
   MaxOnRay = 25000;

type
   tRayArray32 = array[0..MaxOnRay] of float32;
   tRayArray64 = array[0..MaxOnRay] of float64;

   tPolyLinePts = array[0..sfMaxPoints] of tPoint;
   tDoubleCoords = record
      Long,Lat : float64;
   end;
   tSingleCoords = record
      Long,Lat : float32;
   end;
   tdCoords = array[0..sfMaxPoints] of tDoubleCoords;
   tdElevs = array[0..sfMaxPoints] of float64;

   tLogRegistration = record
      MapX,MapY           : array[1..3] of float64;
      CornerX,CornerY     : array[1..3] of float64;
   end;

const
   MaxOverlays = 12;
type
   tOverlayOrder = (ovoUnused,ovoTiger,ovoGazetteer,ovoContours,ovoGrids,ovoPLSS,ovoDatabases,ovoSRTMWater,ovoCartoDB,ovoFans,ovoContoursDEM2,ovoWorldOutlines,ovoUSOutlines,ovoTissot,ovoSecondGrid,ovoVectors,ovoOSM);
const
   LayerName : array[tOverlayOrder] of ShortString = ('Unused','Tiger','Gazetteer','Contours','Grids','PLSS','Databases','SRTMWater',
       'CartoDB','Fans','ContoursDEM2','WorldOutlines','USOutlines','Tissot indicatrices','Second DEM/grid','Vectors','OSM');
const
   smEightNeighborsUnweighted = 0;
   smEightNeighborsWeighted = 1;
   smEightNeighborsWeightedByDistance = 2;
   smGuthHybrid = 3;
   smFrameFiniteDifference = 4;
   smFourNeighbors = 5;
   smSimpleDifference = 6;
   smONeillAndMark = 7;
   smSteepestNeighbor = 8;
   smAverageNeighbor = 9;
   smMaxDownHillSlope = 10;
const
    FirstSlopeMethod = smEightNeighborsUnweighted;
    LastSlopeMethod = smMaxDownHillSlope;
    tixUTM = 0;
    tixLatLong = 1;
    tixBoth = 2;
    tixNone = 3;

type
   tCreateGrid = (cgUTM,cgLatLong,cgSpecifyUTM,cgWKT);
   tMapTick      = byte;   
   tAutoOpen     = (aoNothing,aoProject,aoDEM,aoImage,aoHyper,aoMultigrid,aoLastPointCloud,aoShapeFile,aoVector,aoLastLidarMulti);
   tBeachBallMap = (bbmColor,bbmFocal,bbmSwitch);
   tCheckPoint = (CheckNothing,CheckReasonable,CheckAll);
   tContrastEnhancement = (NoEnhancement,HistogramEqualization,StraightLinearStretch,TailLinearStretch,CloudOnlyTailStretch,MaskRange,Custom);
   tMovieFormat = (mfBMP,mfJPEG,mfGeoTiff,mfBMPworld,mfJPEGworld);
   tStereoMode = (smNone,smAnaglyph,smPair);
   tDigitizeMode = (dmStream,dmPoint);
   temvc = (emvcBasicMask,emvcSelectedColor,emvcAllButSelectedColor,emvcFlattenLake,emvcAreaOfSingleColor,emvcSetElevation);
const
   dncDN = 0;
   dncRadiance = 1;
   dncReflectance = 2;
   dncReflectSun = 3;
   dncBrightness = 4;
   dncMDDefault = 5;
type
   tdnConvert = byte;

type
   tSatView = record
     WindowContrast : tContrastEnhancement;
     WindowContrastLowTailSize,
     WindowContrastHighTailSize : float32;
     BandInWindow,
     RedBand,GreenBand,BlueBand : int32;
     PanSharpenImage : boolean;
   end;

   tDefaultRecord = packed record
       {$If Defined(ExDRGimport) or Defined(ExGeoPDF)}
       {$Else}
          DRGQuadClip, DRGCollar,DRGStructures,DRGTransport,DRGHydrography,DRGShadedRelief,DRGBoundaries,DRGOrthos,DRGGrid,DRGContours,DRGWoodland,DRGPLSS : boolean;
       {$EndIf}

       {$IfDef ExTiger}
       {$Else}
          TigrDef : tTigrDef;
       {$EndIf}

       {$IfDef ExPLSS}
       {$Else}
          PLSSDef : tPLSSDef;
          PLSSRangeDef,PLSSTownShipDef : byte;
          ShowPLSS : boolean;
       {$EndIf}

       {$IfDef VCL}
           SB1PanelWidths : array[0..3] of int16;
           ShowMainToolbar : boolean;
        {options to simplify menus and toolbars}
           ShowOceanModels,
           ShowGeologyOptions,
           ShowGeomorphometry,
           ShowDEMCompare,
           ShowMethodCompare,
           ShowConversionAndAnalyze,
           ShowIcesat,
           ShowViews,
           ShowClimateAndLight,
           ShowClimateStationDB,
           ShowPlateRotation,
           ShowSieve,
           ShowVectorMaps,
           ShowDataBase,
           ShowIntDB,
           ShowStereoNet,
           ShowCartography,
           ShowTINs,
           ShowStratCol,
           ShowTernary,
           ShowMarineGeology,
           ShowDataProperties,
           ShowDBonly,
           ShowOceanographyOptions,
           ShowSidescan,
           ShowSubbottom,
           ShowOpenGL,
           ShowIntervisibility,
           ShowOpenImagery,
           ShowTCPServer,
           ShowSHPButton,
           ShowUSGSQuadNames,
           ShowGlobalDEM,
           ShowBlueMarble,
           ShowMultigrids,

           US_Show_States,
           US_Show_Counties,
           US_Show_Roads,
           US_Show_Rivers,
           ShowPointClouds,
           ShowExperimentalOptions,
           ShowDBDateTimeSeries,
           ShowDataManipulation,
           ShowLabs : boolean;
      {$EndIf}

      {$IfDef IncludeFMXOptions}
          GPSColor : tPlatformColor;
          GPSColorIndex : byte;
          AutoStartGPS : boolean;
          DefScreenRotation,
          DefMapSizeIndex,
          DefGPSUpdate,
          DefGPSSave,
          DefMapTypeIndex : byte;
      {$EndIf}

      {$IfDef ExMICRONET}
      {$Else}
        NetDef : tMicronetDefaults;
        NetDipFill : byte;
      {$EndIf}

      {$IfDef ExStratcol}
      {$Else}
         ColDef : tStratColDefaults;
      {$EndIf}

       {$IfDef ExFly}
       {$Else}
          FlyOptions            : tFlyOptions;
       {$EndIf}

       {$IfDef ExSidescan}
       {$Else}
         SonarMapDef : tSonarMapDef;
       {$EndIf}

      {$IfDef ExFresnel}
          RadioK : float32;
      {$Else}
          RadioK,
          FresnelFreq : float32;
          DrawFresnel : boolean;
          FresnelZone1Color,
          FresnelZone2Color : tPlatformColor;
          FresnelZone1Width,
          FresnelZone2Width : byte;
      {$EndIf}

      {$IfDef ExSidescan}
      {$Else}
        ChirpReturnsToShow : int16;
        ChirpXThinning,
        ChirpXDupe,
        ChirpYThinning : byte;
        ChirpGain, ChirpTVG : float64;
        ShowChirpBottomTrack : boolean;
      {$EndIf}

       {$IfDef ExVariogram}
       {$Else}
          VariogramOptionsRecord : tVariogramOptionsRecord;
       {$EndIf}

       {$IfDef ExTissot}
       {$Else}
         TissotLineWidth,
         TissotHKdecimals : byte;
         TissotPixelSpacing,
         TissotRadius : int16;
         TissotLatGridInc,
         TissotLongGridInc : float32;
         TissotColor : tPlatformColor;
         DrawTissotDefault,
         ShowTissotHK,
         SimpleTissotCylindrical,
         TissotSpaceByPixels : boolean;
       {$EndIf}

       {$IfDef ExDrainage}
       {$Else}
           DrainageSlopeAlgorithm : byte;
           DrainageArrowLength,
           DrainageArrowWidth,
           DrainageVectAvgArrowLength,
           DrainageVectAvgArrowWidth,
           DrainageArrowSeparation : byte;
           DrainageArrowColor : tPlatformColor;
           DrainageVectAvgArrowColor : tPlatformColor;
           DrainageSeedRadius : byte;
           DrainagePointSlope,DrainageVectorAverage : boolean;
       {$EndIf}

       {$IfDef AllowGeomorphometry}
           GrainNets,
           GrainFlatGraph,
           GrainOrgGraph,
           GrainDir,
           GrainAspects,
           GrainText : boolean;
           GeomorphNewDB,GeomorphAllDEMs : boolean;
           GeomorphSlopeCut : array[1..4] of float32;
           DoWavelength,
           FindWavelength,
           PlotCrest,
           SSOallInTable : boolean;
           OrganizationCutoff,
           FlatnessCutoff       : float32;
           LagCenterShift,
           LagSearchRadius : int16;
           PyramidLevels : byte;
           SSOSampleIncr: int16;
           OpenGridBoxSize,OpenBoxSizeMeters,
           SSOGridBoxSize,SSOBoxSizeMeters,
           MomentsBoxSizeMeters,
           ReliefBoxSizeMeters,
           GeomorphBoxSizeMeters : int32;
           GrainLengthMultiple,
           GrainLineWidth : byte;
           GrainColor : tPlatformColor;

           GeomorphNameModifier : ShortString;
           GrainSampleSeparation : int16;

           RoughnessBox : byte;

           SSObyPole     : boolean;
           ShowElevFreq,
           ShowSlopeFreq,
           ShowElevSlope,
           ShowCumSlope,
           ShowAspectRose,
           ShowElevSlopeDeg : boolean;

           ElevMoments,
           SlopeMoments,
           RoughnessMoments,
           PlanCurvMoments,
           SlopeCurvMoments,
           ShowRegularHistogram,
           ShowCumulativeHistogram,
           ShowStrahlerHistogram,
           ShowNormalHistogram,
           ShowHistogramText,
           GraphsOfMoments,
           LongMoments,
           CountHistograms,
           CumFreqNormAxis : boolean;

           TrendOpenMaps,
           TrendSurfaceOverVoids,
           TrendDoGraph,
           TrendMapDev,
           TrendSurfMap,
           TrendHistDev,
           TrendText,
           GrainReliefGraph,
           LimitSSODirByFlatness,
           LimitSSODirByStrength : boolean;
           MinSSOStrength,MaxSSOFlat : float32;
           MinPointsForSSO,ElevBinSize : int16;
           CurOrdTrendSurf,
           OpennessCalcThin,
           FabricCalcThin,
           MomentCalcThin,
           ReliefCalcThin : byte;
           BatchRegionSize : array[1..5] of integer;
       {$EndIf}


       {$IfDef ExPOTRACE}
       {$Else}
           potrace_tsize : integer;
           potrace_invert : boolean;
           potrace_corner,
           potrace_black : float32;
           potrace_outline : byte;
       {$EndIf}

       {$IfDef ExTerrainClassify}
       {$Else}
           SpireRadius,
           SpireHeight,
           PeakRadius,
           PeakHeight,
           PitRadius,
           PitHeight,
           MinRoofEdgeSlope,
           MinBldgSize,MaxBldgSize,
           PeakRoofMinSlope,
           PeakRoofSlopeTol,
           PeakRoofAspectTol,
           AcrossWallHeight,AlongWallHeight,BuildingMaxSlope : float32;
           PeakRoofNeighbors,
           CrestMaxGap,
           SpireNeighborTolerance,
           WallRegion,RoofRegion  : byte;
           OverWriteFeatureDBs,
           OverWriteClassDEMs : boolean;
           WallSymbol,SpireSymbol,PitSymbol,PeakSymbol,BuildingEdge : tFullSymbolDeclaration;
       {$EndIf}

       {$IfDef ExGeology}
       {$Else}
           MoveGeologyDBMemory,
           PlateRotateContCrust,
           PlateRotateBoundaries : boolean;
       {$EndIf}

       {$IfDef ExGazetteer}
       {$Else}
           DefaultGazetteerType  : byte;
           ShiftGazPeaks : boolean;
           ShiftGazPeaksDist : int16;
           GazPeakObsUp : int16;
           LabelGazOnMap,
           AutoLabelGaz,
           UseGazetteer,
           GazMarkPeaksPerspective,
           GazMarkPeaksPerpsOnMap : boolean;
       {$EndIf}

       {$IfDef ExGeography}
       {$Else}
           KoppenOpts  : tKoppenOpts;
           MoveGeographyDBMemory,
           RiseSet,MoonPhase : boolean;
       {$EndIf}

       {$IfDef NoClustering}
       {$Else}
           ClusterInitialization : MVClusterClientDataSet.tInitializationOption;
           ClusterIterations,
           NumClusters : byte;
           ShowClusterScatterPlots,
           ShowClusterHistograms : boolean;
       {$EndIf}

       {$IfDef ExPowerLine}
       {$Else}
         PL_FirstVacantVoxel,
         PL_LastVacantVoxel,
         PL_MaxOccupiedVoxels,
         PL_NeighborsRequired : byte;
         PL_PlotOccupied,
         PL_PlotFirstNeighbor,
         PL_PlotSecondNeighbor : boolean;
       {$EndIf}

       {$IfDef ExPrincComp}
       {$Else}
          PCCorrelation,
          PCEigenValues,
          PCVarCovar,
          PCResults : boolean;
       {$EndIf}

       {$IfDef ExOSM}
       {$Else}
          OSMcheck,OSMtoCDS : boolean;
          OSMOpacity : byte;
          OSMmaxLength : int16;
       {$EndIf}

       {$IfDef ExGeomorphGrids}
       {$Else}
          DoCrossCurve,
          DoMaxCurve,
          DoMinCurve,
          DoSlopeCurve,
          DoPlanCurve,
          DoUpOpen,
          DoDownOpen,
          DoDiffOpen,

          DoRelief1,
          DoAvgElev,
          DoElevStd,
          DoREL,
          DoTPI,

          DoRelief2,
          DoSummit,
          DoBaseLevel,
          DoGeophysical,
          DoDropoff,
          DoElevRelief,
          DoMean,
          DoSTD,
          DoSkew,
          DoKurt,

          DoS1S2,
          DoS2S3,
          DoFabDir90,
          DoFabDir180,
          DoFabDir360,
          DoRoughness,
          DoAvgVectStrength,

          DoSlopePC,
          DoSlopeDeg,
          DoSlopeSin,
          DoSlopeLogTan,
          DoSlopeLnTan,
          DoSlopeSqrtSin,
          DoSlopeMperM,
          DoNSSlope,
          DoEWSlope,
          DoAspect,
          DoAspectNS,
          DoAspectEW,
          doDEMwithMax,doDEMwithMin,
          doMeanDEM,doEnvDEM,doNPTsDEM,doSTDDEM,doMedDEM,doFloorDEM,doCeilingDEM,
          SignedSlopeComponents,
          GeomorphMapsFullDEM : boolean;
          PointFabricTerrBlowup,
          GemorphSSOPoles,GemorphAspectRose : boolean;
       {$EndIf}

       {$IfDef ExPointCloud}
       {$Else}
          Icesat2 : tIcesat2;
          LatLongSlices : boolean;
          CloudFilterTolerance : float64;
          DefLidarGeoGridSizeX,
          DefLidarGeoGridSizeY,
          DefWKTGridSize,
          DefLidarXGridSize,
          DefLidarYGridSize : float32;
          DefLidarElevMap : tMapType;
          WKTLidarProj : PathStr;
          ForceSquarePixels : boolean;

          LidarGridProjection,
          RedLow,RedHigh,GreenLow,GreenHigh,BlueLow,BlueHigh : byte;
          PointCloudColorFilter : boolean;
          LidarPanClose,LidarPanFar : int16;
          PointCloudFloorWidth,
          PointCloudCeilingWidth : byte;
          PointCloudFloorColor,
          PointCloudCeilingColor :  tPlatformColor;

          LasPointDensityBasedOn : byte;
          LoadLASclassificationToMemory,
          LoadLASRGBToMemory,
          LOSshowPointCloudRangePoints,
          LOSshowPointCloudRangeLines,
          LoadLASreturnNumberToMemory,
          CheckLasOnMap,
          AutoZoomOpenLAS,
          UseNoise,
          UseOverlap,
          GeoGridSquare,
          PickLASDirs : boolean;
          DTMoption : tDTMOption;

          LasOpacity : byte;
          LasDEMPixelIs : byte;
          LASMaxSubsetSize : int16;
          LOSshowIntervisibilityFromLAS : boolean;
          LasThinFactor : smallint;
          AutoThinByBlock,
          LASelevchecks,
          LabelLAStiles,
          LASlegend,
          LasAutoThin : boolean;
          ls : tLidarDataSettings;
          MinPtsRequiredPercentile : int16;

          MaxIntensity,MinIntensity,MinRGB,MaxRGB : word;
          LowValidZinPointCloud,
          MaxValidZinPointCloud,
          MaxValidHAGinPointCloud : float32;
          DSMpercentile,Midpercentile,DTMpercentile : float32;

          BlockSize : integer;
          MaxPtsInCell : int64;

          lastoolsClassifyParams,
          LastoolsGroundParams : shortString;
          CloudMapSymbol,
          CloudSymbol : array[1..MaxClouds] of tFullSymbolDeclaration;
       {$EndIf}

       {$IfDef ExWMS}
       {$Else}
          WMSOpacityValue : byte;
       {$EndIf}

       {$IfDef ExGDAL}
       {$Else}
          RouteGeotiffExportGDAL,
          DontBugMeAboutGDAL : boolean;
          GDALThinFactor : float32;
       {$EndIf}

       {$IfDef ExGeology}
       {$Else}
          PlateModel : shortString;
          PlateVelocityDiagram,
          PlateVectors,
          PlateLabelVectors,
          PlateNumbers,
          PlateTectonicVelocity : boolean;
          FPMinSlope1,
          FPMaxSlope1,
          FPMinAsp1,
          FPMaxAsp1,
          FPMinSlope2,
          FPMaxSlope2,
          FPMinAsp2,
          FPMaxAsp2 : integer;
          ColorFP1,
          ColorFP2 : tPlatformColor;
          MagLineWidth: byte;
      {$EndIf}

       ProgramOption        : tProgramOption;
       PreferPrimaryDatum,
       PreferSecondaryDatum,
       DefaultDigitizeDatum       : ShortString;
       OutputPitchMethod,
       OutputAzimuthMethod,
       OutPutLatLongMethod   : tLatLongMethod;
       ZoomWindowMapType,
       DefaultElevationColors,
       DefRefMap,
       DefSlopeMap,
       DefDEMMap             : tMapType;
       HighlightDiffMap,
       QuickSlopeSpacings    : boolean;
       MapTicks              : tMapTick;
       InvertGrayScale       : boolean;
       MonochromeColor        : byte;

       CoordUse              : tCoordUse;
       ContourColors         : tContourColors;
       PerspectiveSkyAngle   : float32;
       PersVFOV,PanVFOV,
       PersHFOV,PanHFOV : float32;

       PerspOpts    : tPerspectiveOptions;
       StatSampleIncr : int16;

       DefaultGeologySymbol  : byte;
       MapGeoSymColor : tPlatformColor;
       StructGeologyShowNet,
       StructGeologyLabelVals : boolean;
       AutoDBFieldNameFixes  : boolean;
      WBGroundClassRadius,
      WBSegFilterRadius,
      WBDeNoiseRadius,
      WBDenoiseElevDiff  : float32;

      DEMIX_Full : byte;
      DEMIX_base_dir,
      DEMIX_criterion_tolerance_fName : PathStr;
      DEMIX_xsize,DEMIX_ysize : integer;
      DEMIXCompositeImage,
      DEMIX_DoCHM,
      DEMIX_DoAirOrDirt,
      DEMIX_DoElevDiff,
      DEMIX_DoSlopeDiff,
      DEMIX_DoRuffDiff,
      DEMIX_DoElevParamGraphs,
      DEMIX_DoHalfSecDEMs : boolean;

      SlopeFlatBoundary,
      SlopeGentleBoundary,
      SlopeSteepBoundary : float32;
      LandTypePointsNeeded : int32;
       RotateVectMult : byte;
       AddFitNav,
       ConfirmDBEdits : boolean;
       AspectMapMode : byte;
       XAspect,YAspect : int32;

       ShowLocationSensitivity : boolean;
       TerrainCatPercentages : boolean;
       MaxMrSidImageSize : int32;
       AskAboutSIDLevel : boolean;

      ChangeMinRedColor,
      ChangeMaxRedColor,
      ChangeMinGreenColor,
      ChangeMaxGreenColor : byte;

      ChangeMinRedValue,
      ChangeMaxRedValue,
      ChangeMinGreenValue,
      ChangeMaxGreenValue : int16;

       MapNameLocation,
       GridLegendLocation,
       TerrainCatLegend,
       NorthArrowLocation,
       ScaleBarLocation : tLegendLocation;
       OpenGLCleanOverlays : boolean;
       ScalebarDistortionTolerable : float64;

       FightLineColor        : tPlatformColor;
       FlightLineSize        : byte;
       AutoElevationReset : boolean;
       GridLabelDecimals : byte;
       GridLabelsInsideMap : boolean;
       PictureMode,
       NumSideProfiles,SideProfileSpacing,
       FlyCrossTrackheight,FlyCrossTrackDistance : int32;
       ShowAlongTrackProfiles,ShowCrossTrackProfiles : boolean;
       OpennessDirs : tDirToUse;
       DefaultQuadSize : float32;
       ShowSDonElevSlope : boolean;
       GrayscaleChannels : boolean;
       MinRGBColor,MaxRGBColor,RangeRGBColor : byte;
       MinPercentile,MaxPercentile : float32;
       DefElevsPercentile : boolean;
       MinImagePercentile,MaxImagePercentile,
       MinElevPercentile,MaxElevPercentile : float32;
       MaxAnaglyphShift : int16;

       LVISGabonGraphs : byte;
       ShowMaskScatterPlots,
       ShowMaskHistograms : boolean;
       LocalOptimaOpts : tLocalOptimaOpts;
       MaskShapesIn : boolean;
       MaskDistance : int16;
       MaskCode : byte;
       MaskMapShow : byte;
       MaskOpacity : byte;
       CurveFlatVal : float32;
       //UseBigElevationColorTables : boolean;
       MissingDataBlocksLOS : boolean;
       AssumeNegativeValuesMissing : boolean;
       FilterAllTigerRoads : boolean;
       StereoMode : tStereoMode;
       ShadeOpts : tShadeOpts;
       DBsOnAllMaps : boolean;
       DbMinIntFieldSize : byte;
       NumOffsets : int32;
       OffsetDistance : float32;
       OffsetBearings : array[1..5] of float32;
       OffsetLineWidth : byte;
       OffsetColor : tPlatformColor;
       UTCOffset : int16;
       TZFromLong : boolean;
       LandCoverMaskSize : byte;
       AllowEditDBInGrid : boolean;
       LocalOptimaOpts_DEMRegion : tDEMRegion;
       PlotArrowHead,ReverseArrow : boolean;
       FilterRadius : float32;
       ImageryIconDirs : boolean;

       NumMasksToAdd : byte;
       DefVectorLineMult : float32;
       ConnectRecordColoring : boolean;
       HorizonColor : tPlatformColor;
       HorizonWidth : byte;
       ConnectArrowSpacing : byte;
       ConnectArrows : boolean;
       OptimaBoxSize : int16;
       ConvergingViews : boolean;
       DefWeaponsMinRange : float32;
       JPEGQuality : int32;
       DEMZunits : tDEMZunits;

       BlackLimit,
       GrayTolerance : byte;
       DefaultReadImageType,
       DefaultSaveImageType : integer;
       RealVegGrids : boolean;
       DBOutlineColor : tPlatformColor;
       DBOutlineWidth : byte;
       ShowMenus,
       AutoAssignNameField : boolean;
       FanOutLineColor : tPlatformColor;
       FanOutLineWidth : byte;
       OutlineFans : boolean;
       ShowMasks : boolean;
       LabelRegisterPoints : boolean;
       MaxLabelDecimals : int16;

       RoadTrendRegion,
       RoadTrendSensitivity : int16;
       ExtendZoneSize : float32;
       TransparentGIF,
       TransparentPNG : boolean;
       TransparentLevel : byte;
       UseGif,
       ShapeMaskNearPoints,
       ShapeMaskNearLines,
       ShapeMaskNearInsideAreas : boolean;
       ShapePointBufferDist,
       ShapeLineBufferDist,
       ShapeAreaBufferDist : int16;
       TreatLineAsPolygon,
       TreatPolygonAsLine,
       LOSShowPitch,
       TrackDatabaseRanges,
       DBRecShowToolbarTop,
       TigerToCDS : boolean;

       AllowMemoryLinkDB,
       AllowDBsToRAM,
       VerifyAllWorldFiles,
       ShowMaskedAirspace,
       DoLOSProfile,
       OpenVegGridMap : boolean;

       KMLLabelGraticule,
       KMLTopLevelFolder,
       KMLExportAllMaps,
       KMLExportLayers,
       KMLExportSeparateFans,
       KMLTimeAnimations,
       KMLDefaultDB,
       AskAboutKMLExport,
       CleanUpHTML,
       CleanKMLDirOnClosing,
       ShowKMLFile,
       KMLLabelExports,
       KML_DB_tables,
       ZipKMLfiles,
       ShortKML_UTM,
       KMLCreateWorldFiles,
       KMLOpenGoogleEarth  : boolean;
       KML_Las_offset : float32;
       DB2KMKLThin,
       KMLTileSize  : int16;
       KMLZoomSize,
       KMLOutputOption,
       KMLImageOpts : byte;

       GoogleAPIKey : shortstring;

       ErosionCycles,
       ImageDiffThreshhold : byte;

       ThumbSize : SmallInt;
       SatTrainStdDev : float32;
       DeleteJP2 : boolean;
       ClassLimitMethod : tClassLimitMethod;
       ClassDistancePower : float32;

       ClassifyIHSopacity : byte;
       FanOpacity : byte;
       OverlayOpacity : byte;
       UnSupSamplesFullImage,
       UnSupClassFullImage : boolean;
       KeyLocationSymbol : tFullSymbolDeclaration;
       GraticuleUnits : tAngleMeasure;
       HorizGratText,
       SaveDefaultHemis,
       LoadPCBands : boolean;
       MinPCToShow : float32;
       MaxPCBands,
       FanMapZoom : byte;
       PreferUTMSpace,
       PreferWKTSpace,
       PreferArcSecSpace : float32;
       DoGrazingAngle : boolean;
       RidgeMaskRadius : int16;
       ValleyRidgeThreshold : byte;
       RegionsRidgeMask,
       RegionsShapeFileMask,
       DefSubdueVectMap,
       DefGrayVectMap,
       DrawLOSProtractor : boolean;
       BeachBallMap : tBeachBallMap;
       BeachBallSwitchPixelSize : int16;
       InvertRidgeMask : boolean;
       ReservoirTop,
       ReservoirLowestLevel,
       FloodStep : float32;
       FloodAlg : byte;
       ShowObserverMaskingCircle : boolean;
       MakeTigerMapGrayscale,
       SubdueTigerBase : boolean;
       GIFDefaultDelay,GifFontSize : int16;
       GIFfileLabels,
       DBfilterClearLayer,
       DBfilterRedrawOnClose : boolean;
       TernaryPercentageValues : boolean;
       SecondGridOpacity : byte;
       QuickShapeFileCoding,
       StayAwake,
       ConfirmOpennesDirections : boolean;

       BlowUpExtraMargin : int16;
       ShowViewshedMixedPixels : boolean;
       ViewshedMixedPixelColor : tPlatformColor;
       DeleteFIT : boolean;
       LabelEveryTigerFeature : boolean;
       ViewshedColorScheme : tLegendColors;
       ViewshedPaletteName : shortstring;
       ShowLOSDataBase : boolean;
       IncludeFractalMeasures,
       IncludeGammaMeasures,
       IncludeProfCMeasures,
       IncludePlanCMeasures,
       IncludeFabricMeasures,
       IncludeSlopeMeasures,
       MilAirIcons,
       MilSeaIcons,
       MilLandIcons,
       LooseFillHoles,
       RegionsStreamMask,
       ExcludeStreamsInMask,
       ExcludeRoadsInMask   : boolean;
       StreamMaskDist : int32;
       MapSampleIncr : byte;
       ExcludeTerrainCatInMask,
       QuickMapRedraw,
       OpenGL_VE,
       AutoDrawMapLAS,
       AutoRedrawMapLAS,
       LASPC99,
       ColorizeInPlace,
       ShowCloudOutlines : boolean;
       CloudThinFactor,
       CloudMemoryThinFactor,CloudSliceThinFactor,CloudMapThinFactor,CloudOpenGLThinFactor : SmallInt;
       AutoShowSlice,
       ResultantPlateVectors : boolean;
       OGLObjectSize : byte;
       SaveSpires,SavePits,
       ShowDEMGridCoords : boolean;
       OGLPointSize,
       CloudSliceThick,
       FloorSliceThick,
       CloudSliceJump : float32;
       NeedViewer3Dversion2,
       AutoSliceMore,
       ExperimentalSliceOptions,
       DifferentFloorSliceThickness : boolean;

       FavDEMSeries1,
       FavDEMSeries2 : shortstring;
       MapTopTitle,
       JustSimpleGrid : boolean;
       GemorphAtlasFilterSize,
       GemorphAtlasMatchNeed : int16;
       MaxSatRange,
       MinSatRange  : byte;
       ShowEnhancementGraphs : boolean;
       AzToSat,
       ElevToSat : float32;
       ShowMaskedToSat : boolean;
       GeomorphElevsNeeded : integer;
       ShowMapToolbar : boolean;
       HistogramTailClipSize : float32;
       MinWaveHeight : float32;
       DuneSamplingInterval : integer;
       PeakPitPostings,
       ProgressTimeBins,
       WaveHtValuesNeeded : int16;
       ElevHistBinSize,SlopeHistBinSize,
       ConvexCut,RoughnessCut,SlopeCut1,SlopeCut2,SlopeCut3 : float32;
       IwashPikeCats : byte;
       OpennessHt : byte;
       OpennessHowHigh : float32;
       SlopeMinGray,SlopeMaxGray,
       OpenMinGray,OpenMaxGray : byte;
       MinSlopeForGray,MaxSlopeForGray : int16;
       MinUpward,MaxUpward : int16;
       ClouderXSize,ClouderYSize : int16;
       IncludeWavelength,
       IncludeBasicElevation,
       IncludeAdvancedElevation,
       IncludeMissingHoles,
       IncludeOpenness,
       CorrectAspectTrue,

       ExaggeratePtClass,
       AllowFirstOfMultipleJoins,
       RapidCycle,
       FlipHistogram,
       StatGrafReverseYAxis : boolean;
       SlicerDigPtSym,
       SlicePtSym : Petmar_types.tFullSymbolDeclaration;
       SliceShapeColor : tPlatformColor;
       SliceShapeWide : int32;
       UseVegInFans : boolean;
       FanPickMode : tFanPickMode;
       SaveLiveFlying : boolean;

       ShowClusterResults,
       IncludeClusterStatistics : boolean;
       ClusterConvergenceThreshold : float32;
       LabelEachGraph,
       EntireDEMGeostats,
       AutoSaveGeomorphGrids,
       IncludeBasinID : boolean;
       WavelengthCompDist : SmallInt;
       NoDEMInterpolations,
       DoEarthCurvature : boolean;
       MakePCFloor,MakePCCeiling,
       PCAutoFillHoles : boolean;

       DelaunayLineThick : byte;
       ContourLineThick : byte;
       DelaunayLineColor : tPlatformColor;
       ContourLineColor : tPlatformColor;
       ShowDelauneyTriangles : boolean;
       MaxTriSide : float32;

       {$IfDef ExACOLITE}
       {$Else}
          l2w_Params : shortstring;
          acolite_fName : PathStr;
          acolite_delete_nc,
          acolite_delete_misc,
          acolite_delete_rhos,
          acolite_delete_rhot : boolean;
          acoliteS2res : byte;
       {$EndIf}

       SagaCMD : PathStr;

       BuildingMinHeight,
       BuildingMaxHeight : float32;
       BuildingMinNumLower : int16;
       BuildingMaxNumLower : int16;
       PCDefaultFilter : byte;

       DeleteTarGZ : boolean;
       DefaultVectorMapProjection : tDefaultVectorMapProject;
       WorldOutlinesOnGlobalDEM,
       WorldOutlinesOnGlobalBlueMarble : boolean;
       ModalDBDisplay : boolean;
       FabricAmplitudeDistance : SmallInt;
       FabColorMin,FabColorMax : float32;
       FabColorByField,
       DoFabricAmplitude : boolean;
       HighlightLineWdith : byte;
       HighLightColor : TPlatformColor;

       MinRegionPoints : smallInt;

       HighlightSymbol : tFullSymbolDeclaration;
       PlateauTolerance : byte;
       CrestThreadInterval : SmallInt;
       ClipboardExports : byte;
       BicubicSlope : float32;

       ShowWinExec,
       LogDOSoutput,
       CommasToSemiColons,
       SatImageCoords,
       UseLongsTo360,
       LakeCheck,
       WaterCheck,
       DualElevs,
       AllowRightHandRule,
       ForceCrestComputations,

       SimpleTopoProfilesOnly,
       DrawLOS,
       LOSVisible,
       LabelMultipleProf,

       DrawRangeCircles,
       ShowContourNeighborhood,
       GraphicalCoordVerify,
       DrapeExactly,
       ReverseZFields,
       IgnoreHistogramZero,
       AskHistogramBins,
       UseSealevel,
       AmbushObserverNAP,
       AmbushTargetNAP,
       CumulativeGraph : boolean;
       LOSClHt,
       LOSClWid : Int16;
       VisPtSymbol,MaskPtSymbol : tFullSymbolDeclaration;

       CurvAlg   : tVerticalCurvAlg;

       DefaultSATFilter,
       DefaultDRGFilter,
       DefaultDEMFilter,
       DefaultVectorFilter : byte;
       EdgeFilterGap : byte;

       DefaultMapXSize,DefaultMapYSize,
       DefaultTerrainXSize,DefaultTerrainYSize,
       MaxDrapeXSize,
       MaxDrapeYSize : int32;
       LatLongCushion : float32;

       MaxImpossibleInPath,StartFree,CostSurfResolution,ImpossibleCost,BufferCost,BufferRounds : int16;
       PrecintField : shortstring;
       LCPsavecost,LCPsavedir,LCPsavedist,LCPoverwrite,
       LCP_ShortestDistance,LCP_LeastCost,
       LCPDist,LCPDiagonals : boolean;
       LCPRoadfName, LCPstartFName,LCPendFName : PathStr;

       PrinterMapLength,
       ContourLineWidth,
       DefaultContourInterval : int16;
       MergeInt,
       MergeHue,
       MergeSat,
       NumSlopeBands     : int16;

       PrinterScale : float32;

       UseRefDirs : integer;
       RefPhi,RefTheta,
       RefVertExag : float32;

      //weapons fans
       MaskObsRange,
       ObsAboveGround,
       TargetAboveGround,
       StartFanAngle,
       EndFanAngle,
       FanUpAngle,
       FanDownAngle  : float32;
       FanSaveExt    : ExtStr;
       AmbushFanShow     : tFanShow;
       RadialSkip : byte;

       ReplaceBorderColor,
       FanColor,
       MaskColor,
       RangeCircleColor : tPlatFormColor;
       MultipleFanAlgorithmsToUse : byte;

       CheckPoint        : tCheckPoint;

       LowTailSize,
       HighTailSize        : float32;
       ContrastEnhancement : tContrastEnhancement;

       WeaponRouteSeparation : int16;

       AutoOpen : tAutoOpen;
       SeaLevelToMissing,
       MissingToSeaLevel : boolean;
       MaxMergeSlope : float32;

      ElevDisplayMeters,
      WrapETOPO  : boolean;
      FillHoleRadius      : byte;
      RoamAllZ           : boolean;

      MGRSandLatLongWhileRoam,
      ShowProjectedCoordinates,
      AutoFillHoles      : boolean;
      ShowRoamOnAllMaps     : boolean;
      MovieFormat           : tMovieFormat;
      LabelRouteTurningPoints,
      AverageImageReadings,
      ClipZColors,
      RememberUTM : boolean;
      DefaultUTMZone        : int16;

      ShowPrimaryGrid,
      ShowSecondaryGrid,
      LabelNativeGrid,
      ShowNativeGrid : boolean;

      UTMGridLineWidth,UTMSecGridLineWidth,GeoSecGridLineWidth,NativeGridLineWidth : Byte;
      MapGridColor,SecondaryUTMColor,SecondaryGeoColor,NativeGridColor,
      LowOffscaleColor,HighOffscaleColor,
      MissingDataColor,WaterColor, BlankMapColor,
      ContourColor,IndexColor,TopContColor,
      OverlayContColor,
      BotContColor,ZeroColor : tPlatformColor;
      DecDegDecimals : int16;
      DupeImportsAllowed : boolean;
      dnConvert : tdnConvert;
      AlwaysShowMapCoordinates,
      ShowNewFieldStats : boolean;
      DBPixelsToShow : byte;
      DBLegendN,
      DB_ID_grids : boolean;
      DBRecHt,DBRecWd : Int16;
      CanEditGIS  : tCanEditGIS;
      AdvancedDBops,
      Create3DShapefiles,
      SaveIntermediateDBs,
      ConvertMGtoMD,
      UsePixelSizeRules : boolean;

      CSVfileHeaders : shortString;
      HalfBoxSize,NAvgReq : int16;

      MaxSlopeOnMaps : byte;

      DefGISSymbol : tFullSymbolDeclaration;

      MaxMapSize : int64;
      MaxEllipsoidalSpacing : byte;
      LabelContours    : boolean;
      IndexContWidth : byte;
      ShowFanLocation : boolean;

      AnaglyphVertExag : float32;

      OpenGridMaps,
      AviationDangerColors : boolean;
      IHSTerrainCategory : boolean;
      GrayscaleMerges : boolean;
      AmbushCoverage : boolean;
      GISSecondToolbar : boolean;
      ShowRegionBoundaries : boolean;
      UpdateDelay : byte;
      MeanMedianBoxSize : int16;
      MapSizeToVerify : int16;
      FullUTMGridLabels : boolean;
      DisplayAmbushRoute : boolean;
      DBfilterCaseInSensitive : boolean;
      FilterGridsToEdge : boolean;
      RecNumToShowDBProgress : int16;
      DefMarginLegend : byte;
      DefaultGraphXSize,DefaultGraphYSize : int32;
      NoHistFreqLabels : boolean;
      FrameLineWidth : byte;
      GraphDensityXBlock,
      GraphDensityYBlock : float32;
      BigBM_nc : byte;

      ExpandNeighborsRequired,
      ShrinkNeighborsRequired,
      ShrinkRadius,
      ExpandRadius : SmallInt;

      ContDigitizeSeparation : byte;

      MercShiftLongLimit : float32;
      ShiftMercToUTM,
      MercShiftSingleZone : boolean;

      t_epsg,a_epsg : integer;

      SaveDBFilter,
      SaveDBstatus : boolean;
      DefaultLatHemi : AnsiChar;
      DefaultLongHemi : AnsiChar;
      ASCIIZUnits : byte;
      ASCIIXYFormat : byte;
      IsUSNAComputer,
      TargetsAlongLine : boolean;

      NEAutoSat,
      NEAutoDEM,
      NEautoScale : boolean;
      SmallScaleWorldOutlinePixelSize : int16;
      MedScaleWorldOutlinePixelSize : int16;
      LargeScaleWorldOutlinePixelSize : int16;

      PrinterLegend : tPrinterLegend;
      AutoCloseIndexMaps : boolean;
      UseMapPanButtons   : boolean;
      BandsRequiredInBox : int16;
      BlowUpLongSize,
      BlowUpLatSize   : float32;

      {topo profiles}
       LOSPixelSize,
       LosHorizTick,
       LOSVertExag    : float32;
       LOSMinElev,
       LOSMaxElev     : int16;
       ForceLOSHorizTickSpacing,
       LOSHorizTickSpacing_KM : boolean;
       LOSParallelProfileSep : int32;
       ShowAllProfiles : boolean;
       ASCIIMissingValue : int32;
       EnglishDistanceUnits : tDistanceUnits;
       SaveFanRadiaProfiles : boolean;
       GeotiffMissingValue,
       DTEDMissingValue : int16;

       SatMultiBandTrueColor,
       SatMultiBandNormalize : boolean;
       FileHeader,
       DefaultServerIP : ShortString;
       DefaultServerPort : int16;

       GeoJSONG_zdec,GeoJSONP_zdec,GeoJSONP_xydec : byte;
       MaxPointsAddInBox : integer;

       DefaultState : ShortString;
       wf : tIntervisibilityAlgorithm;
       CompareIVA : array[1..6] of tIntervisibilityAlgorithm;
       HorizonIncrement,
       HorizonLength   : float32;
       {$IfDef MultipleCurvatureMethods}
          CurvatureMethod : tCurvatureMethod;
          CurvRegionSize : integer;
       {$EndIf}

       MEMPowerDefaults : Petmath.tMEMPowerDefaults;
       DifferentiateHolesAndEdges : boolean;

       SlopeClassTolerance,
       ConvexClassTolerance : float32;
       WoodRegionSize : int16;
       MinDeltaZToClassify,
       MinSlopeToClassify : float32;
       RidgePointClassify : tRidgeAlgorithm;

       OGLDefs : tOGLDefs;
       DigitizeMode : tDigitizeMode;
       DefaultUTMGridSpacing : int32;

       ShiftLoX,ShiftHighX,ShiftLoY,ShiftHighY : int16;
       PointSeparation : int32;
       VoxBinHeight : int16;
       ShowGridVegEffects,
       AutoLoadVegGrid,
       AutoLoadVegDensityGrids,
       VegEffectsVoxels,
       DiscardHighPointsVegDensity,
       VegDensityGraphAverage : boolean;

       VegDensityHeights : byte;
       MaxVegHeight,
       VegDensityGraphMaxDensity : int16;
       VegDensityBuildingPoints,
       VegDensityGroundPoints,
       VegDensityRandomizePoints : boolean;
       VegGridRandomizationDistance : float32;

       PixelSizeToShowCounties,
       PixelSizeToShowStates  : int16;

       USOutlinesOnDEMs,
       USOutlinesOnImagery : boolean;

       CountryOutline_Color : tPlatformColor;
       CountryOutline_Width : byte;
       ProvinceOutline_Color : tPlatformColor;
       ProvinceOutline_Width : byte;
       US_StateOutline_Color : tPlatformColor;
       US_StateOutline_Width : byte;
       US_CountyOutline_Color : tPlatformColor;
       US_CountyOutline_Width : byte;
       US_Highway_Color : tPlatformColor;
       US_Highway_Width : byte;
       US_River_Color : tPlatformColor;
       US_River_Width : byte;
       US_FennemanColor : tPlatformColor;
       US_FennemanWidth : byte;

       XYZImport : tXYZImport;
       XYZProduct : byte;
       ImportThinFactor : int16;

       OpenMultipleVectorMaps : boolean;

       SlopeAlg     : byte;
       AspectRegionSize,
       SlopeRegionRadius : int32;

       DeleteAuxTiffFiles : boolean;

       DefaultGraphFont,
       ContourLabelFont,
       TitleLabelFont,
       CollarUnitsFont,
       InsideGridFont,
       DefGisLabelFont1,
       DefGisLabelFont2,
       DefGisLegendFont,
       LegendFont,
       LOSfont,
       DBtableGridFont,
       DBtableTitleFont,
       DBrecordTextFont,
       PerspFont : Petmar_types.tMyFont;
       InsideMapGridColor : tPlatformColor;

       aSym   : tFullSymbolDeclaration;
       aRotateAngle : int32;
       DropDownPerProfile : int16;

       HorizonRadialMultiple : float32;
       InvertSkyline,
       DaylightDuration,
       HorizonDistanceGraph,
       HorizonVertAngleGraph : boolean;

       MaxMigration,
       FirstSunlightDay,
       LastSunLightDay,
       SunlightMapInterval,
       SunlightPrecision : int16;
       FirstBoxSize,
       LastBoxSize,
       BoxSizeIncr :  int16;
       CreateGraphHidden,CreateNetHidden : boolean;

       QuantileRanges : boolean;


       ReportFanCoverage : boolean;
       AutomaticNewMovieNames : boolean;
       EnableGridNetworkComputing : boolean;
       GraphFanCoverage : boolean;
       DefaultTargetTerrainHug : boolean;
       DefaultObserverTerrainHug : boolean;
       DefaultTargetASL,
       DefaultObserverASL : int16;

       DefaultCenterSymbol : tFullSymbolDeclaration;
       MaxThreadsForPC : byte;
       SpeedUnit : tSpeedUnit;
       PanOverlap : Byte;
       RoamSecondMapColor : tPlatformColor;
       GeoContactWidth : byte;
       AutoIncGeoColor : boolean;
       GeoContactColor,
       DigitizeColor : tPlatformColor;
       DigitizeWidth : byte;
       GISChloroplethPalName,
       ElevPalName   : shortstring;
       HorizonSkyMap : boolean;
       LatLongGridTicks : boolean;

       RunOddballLocation : boolean;

       ScaledSymbolColor : tPlatformColor;

       DefAreaFill            : tBrushStyle;
       FillColor,AreaBorderColor : tPlatformColor;
       ShowGridDiffMap,ShowGridDiffHistogram : boolean;
       MapLatLongGridColor : tPlatformColor;
       MapLatLongLineWidth : byte;

       RoseBothEnds,
       ShowSensorMaps : boolean;

       ShowColorLegend : boolean;
       SingleJulianDay : int16;
       SunlightSingleDay : byte;
       VerifyTimeZone,
       SolarPathMap,
       ShowSolstices,
       PromptToSaveNewDEMs,
       DoReqFlyHigh,
       DoReqAntHigh   : boolean;
       MinTerrainFlyAbove : int16;
       ShowScatterPlot,
       LogTCPcommands,
       LogTCPResults,
       MDRecordDebugLog : boolean;
       MaxDebugLinesToKeep,
       FinalLinesToKeep,
       InitialLinesToKeep : integer;

       FigureCoverageOfRoads : boolean;
       RangeCircleUnit   : tRangeCircleUnit;
       AvoidTextOverprints : boolean;
       //spcsSurveyUnits : tSPCSUnits;
       AssumeMinus32767Missing,
       AssumeMinus99Missing,
       AssumeMinus999Missing,
       AssumeMinus9999Missing,
       AssumeMinus99999Missing,
       AssumeMinus999999Missing : boolean;
       RotatingEarthOutlines : boolean;
       NumLOSParallelProfile : int16;
       DBColorScheme : tLegendColors;
       AutoGrayScaleReflectance,
       AspectBoxRegion : boolean;
       XPixAspect,YPixAspect : int16;
       HistBinSize : float32;

       DisplayFanBitmaps : boolean;

       DoubleEtopoImport,
       BoxAroundQuickMaps,
       TransparentIcons : boolean;
       ConPtsColor : tPlatformColor;
       ConPtsWidth : byte;
       LegendBarWidth,
       LegendTickSize : byte;
       tnHeight,tnQuality,
       SpecifyLegendX : SmallInt;
       SinglePixel,
       LongLandCoverResults : boolean;
       PtSlicerDefView : byte;
       LOSSliceBuffer : SmallInt;
       SlicerUseCorrectScaling,
       SlicerIHSMerge             : boolean;
       SliceColorOpt : tSliceColorOpt;
       SkipWebUpdates : Boolean;
       RedistrictEvenness : byte;
       LOSShowVoxelDensity,
       ShowCloudDensity  : boolean;
       ShowPointCloundOnProfile : tShowPointCloundOnProfile;
       LOSMinPitch : float32;
       LOSMaxPitch : float32;

       TerrainProfileColor,
       LOSConnectionColor,
       MaskedAirspaceColor,
       PitchLineColor : tPlatformColor;
       MemoryPointCloudMaxPts : int32;

       LOSConnectionWidth,
       TerrainProfileWidth,
       MaskedAirspaceWidth,
       PitchLineWidth : byte;
       ShowDEMTerrainProfile : boolean;
       LOSVisLineWidth : byte;
       LOSVisShowLine : boolean;
       LOSLeftTixLabels : boolean;
       ProfileShowZLevel : boolean;
       ProfileShowSeaLevel : boolean;

       ShowOpenGLLOSline : boolean;
       OpenGLLosLineWidth : float32;
       OpenGLLosLineColor : tPlatformColor;
       LegendSingleHeight,
       LegendGraphWidth : SmallInt;
       UTMGridMaxPixelSize : SmallInt;
       TraceContactsOnDEM,
       ShowContactsOnStereoNet,
       ThreePointVerbose : boolean;

       ThreePointExtrap : SmallInt;
       MinMaxGridTolerance : float32;
       FuzzyMatches,
       AnyNoDataMeansNoData : boolean;
       MapMaskColor : tPlatformColor;
       EWAnaglyphShift : boolean;
       VegOptionMap : tVegOptionMap;
       LeftLatGlobalDEM : int16;

       GeocodeAddress : shortString;
       BandsByWavelength,
       ShowSupClassDB,
       DBMinimizeOnOpen,
       MakeOGLMovie  : boolean;
       DoGrazingFields : boolean;
       GISLabelSkip,
       RoadMaskDistance : smallInt;
       BackupEXEbeforeUpdate : boolean;
       DetailRoseLegend,
       PurgeBigGraphSubGraphs,
       AddAzimuth,AddDist,AddCumDist,UseMeters,AddSpeed,Add3Ddist : boolean;

       ResultantColor,WindColor,TideColor,ShipColor,WindCurrentColor : tPlatformColor;
       ResultantWidth,WindWidth,TideWidth,ShipWidth,WindCurrentWidth : byte;
       CartMovieSteps : byte;
       CloseCartFormOnOpen : boolean;

       {$IfDef SQLiteDefaultDBs}
          ForceOverwriteDB : boolean;
       {$EndIf}

       JoinRemoveLeadingZeros,
       ApplySameFilterAllDBs,
       MapLimitDB,
       MultipleDBFiltersForGraphing,
       DefaultEditDBsInGrid,
       ModalEditDBRec : boolean;
       DefDBFilter : byte;
       TideGaugeOffset,
       TopCutLevel,
       BottomCutLevel,
       CurrentSeaLevel : float32;
   end;

var
   MDdef            : tDefaultRecord;
   BackupMDDef      : ^tDefaultRecord;
   MrSidEnabled       : boolean;
   LineColors10 : array[0..10] of tPlatformColor;

   {$IfDef VCL}
      AspColorNE,AspColorE,AspColorSE,AspColorS,AspColorSW,AspColorW,AspColorNW,AspColorN : TRGBTriple;
   {$EndIf}

   {$IfDef FMX}
      AspColorNE,AspColorE,AspColorSE,AspColorS,AspColorSW,AspColorW,AspColorNW,AspColorN : TAlphaColor;
   {$EndIf}

   ShowSatProgress,
   AutoZoomDEMs,
   ShowDEMReadingProgress : boolean;

   DEMIXSettingsDir,
   DEMIXresultsDir,
   DEMIXrefDataDir,
   DEMIXtempfiles,

   MainMapData,
   LasRulesName,
   TM_RGB_fname,

   {$IfDef ExWMS}
   {$Else}
      FavWMSLayer,
      WMS_servers_fName,
      FavoriteWMS,
   {$EndIf}

   MainHydroShedsDir,
   WKT_GCS_Proj_fName,
   StateGISFileName,
   CountyGISFileName,
   HighwayGISFileName,
   RiversGISFileName,
   RiversFile,
   ShapeFileMaskDirectory,
   ShapeFileMaskFile,
   CloudBitmapName,

   ColorBrewerName,
   HardLimitColorPaletteFName,
   LastTrainSetFName,
   ETOPODEMName,
   BlueMarbleFName,
   LastSatDir,
   LastDEMName,
   LastTINName,
   LastImageName,
   LastScanMapName,
   LastOverlayName,
   LastFanTable,
   LastDataBase,
   LastDesktop,
   LastLidarMulti,
   LastOSMoverlay,
   ElevationFixedPalette,

   LastWorldFileOverlay,
   GazOptFName,
   LandCoverFName,
   RangeCircleSizesfName,
   GT_Datum_fName,
   GT_Ellipse_fName,

   SatBandNames,
   LargeScaleWorldOutlines,
   MedScaleWorldOutlines,
   SmallScaleWorldOutlines,
   LastCompressedFile,
   TableDefinitionsFileName,
   LastSavedLOSfName,
   CoastLineFile,

   Geoid2008FName,
   Geoid96FName,
   GeoidDiffFName,

   DBDir,
   TigerShapeRules,
   CSVImportRulesFName,

   {$IfDef ExHyperspectral}
   {$Else}
      LastHypFile,
   {$EndIf}

   {$IfDef ExGazetteer}
   {$Else}
      LastGazFile,
   {$EndIf}

   {$IfDef ExOSM}
   {$Else}
      OSMRoadRules,
      OSMGroupRules,
   {$EndIf}

   {$IfDef ExGazetteer}
   {$Else}
      GazetteerDir,
   {$EndIf}

   {$IfDef ExMagVar}
   {$Else}
      www_mag_mod_fName,
   {$EndIf}

   {$IfDef ExPLSS}
   {$Else}
      DefaultPLSSBaseline,
      PLSSMerfName : PathStr;
      PLSSFile : array[1..5] of PathStr;
   {$EndIf}

   {$IfDef ExGeology}
   {$Else}
      MagneticAnomalyTimeScale,
      GSFML_global_picks,
      PlateBoundaryFile,
      CMT_fault_cent_fName,
      CurrentMotionsFile,
      ContCrustOutlineFile,
      PlatePolesFile,
      SedThickFile,
      SedTypeFile,
      DSDP_db,
      MagAnomFile,
      VolcanoesDB,
      PlateOutlinesfName,
      Hotspot_db,
      GeologyDir,
      PredAgesFile : PathStr;
      EpochsTimeScale,
      PeriodsTimeScale : shortstring;
   {$EndIf}

   {$IfDef ExGeography}
   {$Else}
      GlobalWindsFName,
      GlobalCurrentsFName,
      PiratesFName,
      MonthlyClimateFName,
      ClimateDir,
      WorldClimate2Dir,
      ClimateStationFName,
      KoppenDefFName,
   {$EndIf}

   {$IfDef ExMultiGrid}
   {$Else}
      LastMultigrid1,
      LastMultigrid2,
      LastMultigrid3,
   {$EndIf}

   {$IfDef ExStratcol}
   {$Else}
      LastStratColFile,
      LithFileName,
   {$EndIf}

   {$IfDef ExOceanography}
   {$Else}
      OceanDriftFName,
      OceanTideFName,
   {$EndIf}

   {$IfDef ExVegDensity}
   {$Else}
      LastVegDensity1fName,
      LastVegDensity2fName,
      LastVegGrid,
   {$EndIf}

   {$IfDef ExSidescan}
   {$Else}
      ChirpDataPath,
      SideDataPath,
      InputSideScanFileName,
      SidescanIndexFName,
   {$EndIf}

   {$IfDef ExGDAL}
   {$Else}
      GDALtools_Dir,
      //GDALtools_Data,
      GDAL_translate_name,
      GDAL_contour_name,
      GDAL_Warp_Name,
      GDAL_dem_name,
      GDAL_ogr_Name,
      GDAL_info_Name,
      GDAL_srs_info_Name,
   {$EndIf}

   {$IfDef ExOTB}
   {$Else}
       OTB_dir,
   {$EndIf}

   CloudCompareFName,
   WhiteBoxFName,

   {$IfDef MSWindows}
      CurrentProject,
      WriteDEMDir,
      WriteSatDir,
      ImageDir,
      MovieDir,
      KMLLogo1FileName,
      KMLLogo2FileName,
      VectorMapName,
      MapLibDir,
      GADMDir,
      mcc_lidarFName,
      MrSIDDecodeName,
      MrSidInfoName,

      PreferFilter,
      VasaProjectFName,
      VasaArtefactsFName,
      VasaArtDBfName,
      VasaKeyPoints,
      VasaSideLimits,
      VasaBinFreq,
      VasaBoxOutlines,

      {$IfDef ExGeoStats}
      {$Else}
         GeomorphAtlasDir,
      {$EndIf}
      {$IfDef ExComplexGeoStats}
      {$Else}
         BlockInputDir,
         BlockOutputDir,
      {$EndIf}
      {$IfDef ExTIN}
      {$Else}
         TINDir,
      {$EndIf}
      {$IfDef ExPointCloud}
      {$Else}
         LastLidarDirectory,
         LastLidar2Directory,
         LastLidar3Directory,
         LastLidar4Directory,
         LastLidar5Directory,
         LastLVISFileName,
      {$EndIf}
   {$EndIf}

   ProjectDir,
   PhotoDir,
   SaveViewshedDir,
   CopyFilesFromDir,
   CopyFilesToDir    : PathStr;

   {$IfDef ExPointCloud}
   {$Else}
   SlicerMaskField : shortstring;
   {$EndIf}

   {$IfDef ExGeology}
   {$Else}
   SedThickDEM,
   SedTypeDEM,
   PredAgesDEM : integer;
   {$EndIf}

   DEMStatsString : String;
   IndexDataOnLine : tMyData;
   LastDEMLoaded,
   LastVectorMap : integer;


{______________ declarations for MICRODEM software defaults _________________}

const
   IncMapTypes = 'Incompatible map types';
   ImpGridIncSpace = 'Impossible grid; increase data spacing';
   NoDEMCovers = 'No DEM coverage';

implementation


initialization
   {$IfDef MessageStartUpProblems}  MessageToContinue('Startup demdefs'); {$EndIf}
   {$IfDef ExGeology}
   {$Else}
      PredAgesDEM := 0;
      SedThickDEM := 0;
      SedTypeDEM := 0;
   {$EndIf}
   AutoZoomDEMs := false;
   LineColors10[0] := claRed;
   LineColors10[1] := claLime;
   LineColors10[2] := claBlue;
   LineColors10[3] := claFuchsia;
   LineColors10[4] := claPurple;
   LineColors10[5] := claNavy;
   LineColors10[6] := claAqua;
   LineColors10[7] := claTeal;
   LineColors10[8] := claYellow;
   LineColors10[9] := claOlive;
   LineColors10[10] := claMaroon;
finalization
end.




