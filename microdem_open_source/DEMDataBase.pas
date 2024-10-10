unit demdatabase;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}


{$I nevadia_defines.inc}

{$Define InlineSlowdowns}

{$IFDEF DEBUG}
   {$IfDef RecordProblems}  //normally only defined for debugging specific problems
      //{$Define RecordDEMIX}
      //{$Define RecordDBsort}
      //{$Define RecordClustering}
      //{$Define RecordCloseDB}
      //{$Define RecordCopyFieldLinkDB}
      //{$Define RecordLegend}
      //{$Define RecordDBNumericPlot}
      //{$Define RecordHyperion}
      //{$Define RecordDEMIXFull}
      //{$Define RecordSymbolColor}
      //{$Define RecordRedistrict}
      //{$Define RecordDataBaseTiming}
      //{$Define RecordDBPlot}
      //{$Define RecordFIT}
      //{$Define RecordKML}
      //{$Define RecordTiger}
      //{$Define LogModuleCreate}
      //{$Define RecordIcesat}
      {$Define RecordPlotFabric}

      //{$Define RecordFieldStatistics}
      //{$Define RecordOpenDataBase}

      //{$Define FieldFromDEM}
      //{$Define RecordBoundBox}

      //{$Define RecordSym}
      //{$Define RecordDBPlotDetailed}
      //{$Define RecordShapeFileGroup}
      //{$Define RecordFullOpenDB}
      //{$Define RecordFullShapeFileGroup}
      //{$Define RecordMonthlyFilter}
      //{$Define RecordLinkTable}
      //{$Define RecordDetailedSym}

      //{$Define RecordRangeTable}
      //{$Define RecordAddField}

      //{$Define RecordFilter}
      //{$Define RecordDataBaseSaveFiles}
      //{$Define RecordDataBaseSaveFileFull}
      //{$Define RecordDataSaveStatus}
      //{$Define RecordFont}
      //{$Define RecordDefineDBColorTable}
      //{$Define RecordDataBaseText}
      //{$Define RecordLegend}
      //{$Define RecordOpenDataBaseStructure}

      //{$Define RecordDataBaseLabels}
      //{$Define RecordEveryDataBaseLabel}

      //{$Define RecordNetworkFailures}
      //{$Define RecordNetworkNodes}
      //{$Define RecordBasinBreakdown}
      //{$Define RecordThalweg}

      //{$Define RecordMaskDEMShapeFile}
      //{$Define RecordZoomMap}
      //{$Define RecordIcons}
      //{$Define RecordDBGraphs}
      //{$Define RecordUnique}
      //{$Define RecordQuadFill

      //{$Define RecordDBindexes}
      //{$Define RecordMergeDB}
      //{$Define RecordFan}
      //{$Define ShowOtherDBsWithSameFilter}
      //{$Define RecordDataBasePlotProblemsEveryPoint}
      //{$Define RecordDBCount}
      //{$Define RecordGISvectors}
      //{$Define RecordCurrentRecord}
      //{$Define RecordStationTimeSeries}
      //{$Define RecordDataBase}
      //{$Define RecordGAZ}
      //{$Define RecordFilterDataBase}
      //{$Define RecordID}
      //{$Define RecordPointInArea}
      //{$Define RecordDipStrike}
      //{$Define RecordQueryGeoBox}
      //{$Define RecordBeachBall}
      //{$Define RecordValidScreenPosition}
      //{$Define RecordLineWidth}
   {$EndIf}
{$Else}
{$EndIf}

//{$Define IncludeRiverNetworks}



interface

uses

//needed for inline of the core DB functions
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
//end DB declarations

   System.UITypes,System.Classes,System.Math, System.UIConsts,System.Threading,
   StrUtils,SysUtils,
   iniFiles,

   {$IfDef VCL}
      VCL.Graphics,VCL.Buttons,VCL.StdCtrls,VCL.Controls,Forms,dbGrids,
      GIS_Scaled_symbols,
      DEMdbTable,
   {$EndIf}

   {$IfDef MSWindows}
      Windows,
   {$EndIf}

   {$IfDef ExGeology}
   {$Else}
      NetMainW,Demdips,
   {$EndIf}

   {$IfDef NoDBMaps}
   {$Else}
      DEMMapF,
   {$EndIf}

   {$IfDef NoDBGrafs}
   {$Else}
      BaseGraf,
   {$EndIf}

   DEMMapDraw,

   DEMDefs,
   Petmar_types,PETMAR,PETDBUtils,PetMath,
   Petmar_ini_file,
   DEMESRIShapeFile, FireDAC.Stan.StorageBin;

var
   AddBBtoShapeFiles : boolean = true;  //set to false before opening DBif you are doing many files and know it won't be needed

const
   NotAllowedDBtype = 'Not allowed for this type of database';
   DBMaskString = 'Any database|*.dbf;*.csv;*.kml;*.db;*.adb;*.shz;*.fit|' +
                  'Shapefile|*.shp;*.shz|' +
                  'dBase|*.dbf|' +
                  'CSV file|*.CSV;*.txt|' +
                  'TXT file|*.txt|' +
                  'SQLite file|*.db;*.sdb|' +
                  'XML file|*.xml|' +
                  'CDS file|*.cds|' +
                  'KML file|*.kml|' +
                  'xyz file|*.xyz|' +
                  'GPX|*.gpx|' +
                  'Garmin FIT|*.fit|' +
                  'All files|*.*';

const
   //dgMean = 1;
   //dgMedian = 2;
   dgPick = 3;
   dgAllValues = 4;
   dgAllScores = 5;
   //dgSimpleExample = 6;
   dgPercentBest = 7;
   dgArea = 8;
   //dgJust3Params = 9;
   dg7Params = 10;
   dgNormalizedDiff = 11;

type
  tHowRestrict = (resNone);
  tAddDEM = (adPickNearest,adElevDiff,adElevInterp,adElevNearest,adSlope,adElevAllGrids,adDeltaAllGrids,adElevNearestInt,adAvgElevInWindow);
  tdbGraphType = (dbgtN2Dgraph1,dbgtN2Dgraphsimplelines1, dbgtCluster1,dbgtMultiplegraphmatrix1, dbgtByLatitude1,dbgtByLongitude1,dbgtByLatitude,dbgtByLongitude2,dbgtN2Dgraph2series1,
     N2Dgraph2yaxes1,dbgtPlot1series1,dbgtPlotforsubsamples1,dbgtN2Dgraphcolorcodetext1, dbgtN2Dgraphcolorcoded1,dbgtByLatitude2,dbgtN2Dgraph2yaxes1,dbgtN2DgraphCOLORfield1,dbgtPlot,dbgtUnspecified);

  tDBSaveOptions = record
     LinkFieldThisDB,LinkFieldOtherDB,
     QFField1,QFField2,QFField3,
     SegSepField,
     XField,YField,ZField,
     IconField,
     RedField,BlueField,GreenField,
     StringColorField,NumericColorField,SizeField,
     FloatColorField,LabelField,SecondLabelField,
     TimeField,DirField,MagField : ShortString;
     MainFilter,GeoFilter,TimeFilter : AnsiString;
     LinkTableName,
     AllIconFName,
     ZipAToneFName,
     SymSizeField,
     TTSymbolFontName   : PathStr;

     VisCols          : Array100Boolean;
     AreaSymbolFill   : tBrushStyle;
     Symbol           : tFullSymbolDeclaration;
     LineColor,FillColor : TPlatformColor;
     LineWidth : int32;
     GisLabelFont1,
     GisLabelFont2 : Petmar_types.tMyFont;
     Opacity,
     DBAutoShow,
     IconScalingFactor,
     TTSymbolFontSize : byte;
     TTSymbolFontColor : tPlatformColor;
     TTSymbolFontChar : AnsiChar;
     ConstantSymSize,
     OutlinePolygons,
     GISVectorsByMaxSpeed,
     HideTable,ReverseColorTable : boolean;
     CatPCforLegend,
     VectorLineMult  : float32;
     zColorMin,zColorMax,
     SizeMin,SizeMax,
     ColorMin,ColorMax : float64;
     WindAutoSpace : boolean;
     WindPixelSpace : byte;
     VectorThinFactor : byte;
     VectorsByPolar,
     Grayscale,
     Subdue,
     LabelDBPlots,
     ShowBuffers,
     SymbolsWithBuffers,
     ConnectUnderPoints : boolean;
     GISVectorsMaxSpeed : float64;
     XLabelOff,
     ScaledSymMinSize,
     ScaledSymMaxSize,
     dbColorMode : byte;
     DBLegendLocation : tLegendParameters;
     DBColorScheme : tLegendColors;
     DBColorPaletteName : shortstring;
  end;


  TGISdataBaseModule = class(TDataModule)
    EmpSource: TDataSource;
    LinkSource1: TDataSource;
    FDStanStorageBinLink1: TFDStanStorageBinLink;
    {$IfDef UseFireDacSQLlite}
       FDGUIxWaitCursor1 : TFDGUIxWaitCursor;
    {$EndIf}
  private
    { Private declarations }
     DBOldFilter : string;
     DBWasFiltered : boolean;
     PointCGMName : PathStr;
     procedure InitializeDBValues;

     {$IfDef VCL}
        function FocalMechLegend: tMyBitmap;
     {$EndIf}

     {$IfDef RecordDataSaveStatus}
        procedure WriteRecordDataSaveStatus(WhereAreWe : ShortString);
     {$EndIf}
  public
    { Public declarations }
     MyData,LinkTable,NeighborTable,RangeTable,LinkRangeTable,LayerTable : TMyData;
     DBNumber,
     LinkColorTable,
     ShapeFileType,
     StringFields,
     NumericFields : integer;
     AreaRecordSize : byte;
     dbOpts : tDBSaveOptions;
     dbBoundBox : sfBoundBox;
     StringCategories : tStringList;

     {$IfDef VCL}
        dbtablef    : Tdbtablef;
        gis_scaled_form : Tgis_scaled_form;
     {$EndIf}

     {$IfDef NoDBMaps}
     {$Else}
        theMapOwner : DEMMapF.tMapForm;
     {$EndIf}

     {$IfDef NoDBGrafs}
     {$Else}
        theGraphOwner : TThisBaseGraph;
     {$EndIf}


     StringDataThere : tStringList;
     DBAuxDir,
     LayerTableFName,
     dbFullName,
     ShapeFileName        : PathStr;
     LayerIsOn,
     ItsAShapeFile,
     ItsAPointDB,
     ItsAGroupingFile,
     XYZFile,
     dbPlotted,
     dbIsUnsaved,
     ShapeGeoFilter,
     ItsFanFile,
     TernaryPlotUp,
     ItsTigerShapeFile,
     ItsOSMShapeFile,
     DoNotTrackDBSettings,
     FileNameFieldExists,
     LineColorPresent,
     AreaFillPresent,
     NameFieldExists,
     Redistricting,
     SecondLatLongFieldsPresent,
     CentroidPresent,
     DipStrikeFieldExists,
     DipAndStrikeFieldsExist,
     CameraOrientationExists,
     FontFieldExists,
     //TMIndex,
     FocalMechsPresent,
     PhotoLocationsPresent,
     StratcolPresent,
     TimeSeriesPresent,
     ImagePresent,
     WWWpresent,
     CanColorCode,
     VelNECompPresent,
     RGBColorPresent,
     ColorPresent,
     IconPresent,
     PointSymbolFieldsPresent,
     DEMIndex,
     GazDB,
     PLSSfile,
     NoGeometry,
     ItsBoxOutline,
     RestrictToMapOwner,
     AutoRedrawAllowed,
     KMLExportable,
     FanCanClose,
     ShowLocalMapOnly,
     SymbolizationIsCorrent : boolean;

     dbName,
     GraphUpperLeftText,
     dbLegendLabel : ShortString;

     MonthFieldName,
     LongFieldName,LatFieldName,ZFieldName,
     Lat2fName, Long2fName,
     NeighborLinkField     : ShortString;

     ImageFieldNames,
     IconFieldNames,
     WWWFieldNames     : array[1..5] of shortstring;

     dbXmin,dbXMax,
     dbYmin,dbYMax,
     dbZmin,dbZMax,
     MinComboBox1,MaxComboBox1,
     ZeroTol : float64;
     ColorDefTable : tColorTableDefinitions;
     ZipPatName    : string3;
     aShapeFile    : tShapeFile;
     PointsInMemory : ^tdCoords;
     NumPointsInMemory : integer;
     MaskIn : boolean;
     MaskingDistance : float64;
     LegendCaption : shortstring;

     {$IfDef NoDBGrafs}
     {$Else}
        LastGraph : tThisBaseGraph;
        LastGraphType : tdbGraphType;
     {$EndIf}

     {$IfDef ExGeography}
     {$Else}
        KoppenPresent : boolean;
        KoppenDisplay : tKoppenDisplay;
     {$EndIf}

     function InitializeTheTable(WhatDataBase : shortstring; FileWanted : PathStr = '') : boolean;
     function CloseDataBase : boolean;

     procedure ProcessDBIniFile(iniWhat : tIniWhat; SectionRestoreDefaults : ShortString = ''; ExplicitName : PathStr = '');
     function IniFileName : PathStr;
     procedure SetUpRangeTable(BaseName : PathStr; var Table : tMyData; ForceNew : boolean = false);
     function DBhasMapOrGraph : boolean;
     function CanPlot : boolean;

     //is this a special kind of database
        function IsICESat : boolean;
        function IsThisDEMIXdatabase : boolean;

     procedure RespondToChangedDB;
     procedure ToggleLayer(LayerOn : boolean);

     {$IfDef ExSaveDBstatus}
     {$Else}
        procedure SaveDataBaseStatus;
        procedure RestoreDataBaseStatus;
     {$EndIf}

     function SimplePointFile : boolean;
     procedure ShowStatus;

     //filtering
        function AssembleGISFilter : AnsiString;
        procedure ClearGISFilter;
        procedure SaveFilterStatus(RemoveFilter : boolean = false);
        procedure RestoreFilterStatus;
        procedure ApplyGISFilter(fString : AnsiString; DoShowStatus : boolean = true);
        procedure FilterDBByUseAndDisable(Used : boolean);
        procedure FilterForUseField(Use : boolean = true);
        procedure QueryGeoBox(HiLat,LowLong,LowLat,HighLong : float64; DisplayOnTable : boolean);  overload;
        procedure QueryGeoBox(bb : sfBoundBox; DisplayOnTable : boolean); overload;

     //coordinates
        function LatLongCornersPresent : boolean;
        function LatLongFieldsPresent : boolean;
        function ValidScreenPositionFromTable(var x,y : integer) : boolean;
        procedure LoadPointCoords;
        procedure DisposePointCoords;

     //db operations
        function NumericFieldLinkPossible(fName : ShortString) : boolean;
        function StringFieldLinkPossible(fName : ShortString) : boolean;
        function AssignField(var TheValue : ShortString; TheName : ShortString) : boolean;
        procedure FieldRange(Field : shortstring; var Min,Max : float64; Filtered : boolean = false; ForceCompute : boolean = false);
        procedure FieldRange32(Field : shortstring; var Min,Max : float32; Filtered : boolean = false; ForceCompute : boolean = false);
        procedure ClearFieldRange(aField: shortstring);
        function GetFloat32FromTableLinkPossible(FieldDesired : shortstring; var Value : float32) : boolean;
        function GetStringFromTableLink(FieldDesired : ShortString) : ShortString;
        function GetStringFromTableLinkPossible(FieldDesired : ShortString) : ShortString;

        function GetIntegerFromTableLink(FieldDesired : ShortString; var Value : integer) : boolean;
        function GetFloatFromTableLink(FieldDesired : shortstring; var Value : float64) : boolean;

        function FindFieldRangeLinkPossible(FieldDesired : shortString; var aMinVal,aMaxVal : float64) : boolean;  overload;
        function FindFieldRangeLinkPossible(FieldDesired : shortString; var Num,Valid  : integer; var Sum,aMinVal,aMaxVal : float64) : boolean; overload;
        function FindValidJoin(TheFilter : string) : boolean;  {$IfDef InlineSlowdowns} inline; {$EndIf}

        procedure ClearLinkTable(ZeroNames : boolean);
        function FieldSum(FieldDesired : shortstring; ReEnable : boolean = true) : float64;

     //add fields
        procedure AddSequentialIndex(fName : shortstring; Fill : boolean = true);
        procedure AddXYZfields;
        procedure AddImage;
        procedure AddWWW;
        procedure AddLatLong;
        function AddFieldToDataBase(ft : TFieldType; FName : ShortString; Length : integer; Decimals : integer = 0) : boolean;
        procedure AddConcatenatedField(NewField,WantedFieldName,SecondFieldName,ThirdFieldName,Preceder,Separator,SecondSeparator,Follower  : shortstring);
        procedure AddFieldGroupToTable(What : shortstring);
        procedure FillFieldsFromJoinedTable(var TheFields : tStringList; ForceOverWrite : boolean);
        procedure AddNavFields;
        function AddNavFieldDefinitions : boolean;
        procedure LimitFieldDecimals(SelectedColumn : shortstring; NumDec : integer);

        procedure AssignSymbol(aSymbol : tFullSymbolDeclaration);
        procedure AddSymbolToDB;
        procedure AddSymbolizationToLayerTable(Caption : shortstring);

     //edit fields
        procedure RemoveLeadingZerosInField(Table : tMyData; WantedFieldName : ShortString);
        procedure TrimStringFields(theField : ANSIString = '');
        procedure FillFieldWithValue(WantedFieldName : ShortString; WantedValue : shortString; ShowProgress : boolean = true);
        procedure FillUseField(UnfilterFirst : boolean; ch : char);
        procedure TrimOneStringField(SelectedColumn : shortstring);
        procedure TrimAllStringFields;

     //export database
        procedure ExportToXML(fName : PathStr);
        procedure ExportToSQLite;
        function ExportToKML(Ask,Default : boolean; SepExpField : ShortString = '') : PathStr;

     //legend routines
         function StringFieldLegend : tMyBitmap;
         function ChloroplethLegend(aLabel : shortString) : tMyBitmap;
         procedure DrawLayerLegend(MinX, MaxX: float64; aField: ShortString);
         procedure DoTheLegend(var Bitmap : tMyBitmap);
         function BarGraphLegend(Display : boolean = true; theLabel : shortstring = '') : tMyBitmap;
         function CreateDataBaseLegend(SimpleLegend : boolean = false) : tMyBitmap;
         procedure CreatePopupLegend(Title : shortstring = ''; SaveName : PathStr = '');


     function NumUniqueEntriesInDB(fName : shortstring) : integer;

     procedure DefineColorTable;
     function ComputeColorFromRecord(var Color : tColor) : boolean;
     procedure SetColorsFromDB(TheIndex : tMyData);

     function ImageForTable : boolean;

      procedure PickNumericFields(GraphType :  tdbGraphType; NFields : integer; l1 : shortstring; l2 : shortstring; l3 : shortstring; StringColor : boolean = false); overload;

     procedure FindClosestRecord(Lat, Long: float64; var ClosestRecNo: integer; var MinD : float64);

     procedure WriteDisplaySymbology(TheData : tMyData);
     procedure ClearImage;

     procedure PointsForLineAreaDB(AddFirst,AddLast,AddTurns : boolean; DistApart : float64 = -99);
     procedure ExtractPointsFromLineAndAddXYZ;
     function GetFullImageName(var fName : PathStr) : boolean;
     function GetRotatedImage(var bmp : tMyBitmap; var fName : PathStr) : boolean;

     procedure DBFieldUniqueEntries(FieldName : shortstring; var FieldsInDB : tStringList);

     procedure MarkRecordsOnDEM(fName : shortstring; DEM : integer);
     procedure PutInQuartilesBasedOnExistingSort(NumQ : integer = 0);

     procedure MergeDataBases(FileNames : tStringList);

     function ValidLatLongFromTable(var Lat,Long : float64) : boolean;
     function ValidLat2Long2FromTable(var Lat,Long : float64) : boolean;
     function GetLatLongToRepresentRecord(var Lat,Long : float64) : boolean;

     procedure SplitDateField(Format : tDateField);
     procedure TimeFieldsToDecYears;

     procedure LongestString(FieldName : shortstring; var Max : integer; var LS : shortstring);

     procedure DistAllRecsToPoint(Lat,Long : float64; Algs : integer; BaseName : shortstring = ''; Units : integer = 1);
     procedure DistanceToLinePolygon(IncludeLocationOnLine : boolean);

     procedure PurgeDeletedRecords;
     procedure DeleteAllSelectedRecords(ImSure : boolean = false);

     procedure GetPointArrayForDBField(FieldName : ShortString; var Values : Petmath.bfarray32; var NPts : int64; Min : float64 = 1; Max : float64 = -1);
     procedure LinearInterpolateAcrossGap(yfield,zField : shortstring);
     procedure RemoveDuplicatePositions;
     procedure FillTrackVoids;
     function FieldRMSE(fName : shortString) : float64;

     procedure CalculateAreaorCentroid(DoCentroid : boolean);
     procedure BackupDB;

     {$IfDef NoDBMaps}
     {$Else}
         procedure PlotDefaultSymbols(MapDraw : tMapDraw; var Bitmap : tMyBitmap);
         function DEMwithDBsMap : boolean;

         procedure MakeNewMonthlyFilterAndRedraw(Month : integer);
         procedure AssociateMap(MapForm : tMapForm);
         procedure PlotDBStringField(var Bitmap : tMyBitmap; WantField : shortstring = '');
         procedure PlotDBNumericField(var Bitmap : tMyBitmap);
         procedure PlotFieldOnMap(aField : shortstring; Minx : float64 = 9999; MaxX : float64 = -9999);
         procedure PlotSingleMonth(Month : integer);
         procedure QueryBox(MapDraw : tMapDraw; x1,y1,x2,y2 : integer; DisplayOnTable : boolean);
         procedure LabelRecordsOnMap(var Bitmap : tMyBitmap);
         procedure IdentifyRecord(xpic,ypic : integer; Lat,Long : float64; var RecsFound : integer;  ShowRec,LabelRec : boolean; var FeatureName : ShortString; ShowItModal : boolean = true; JustFilterDB : boolean = false);
         function FindAreaRecordWithPoint(Lat,Long : float64; AlreadyFiltered : boolean = true) : boolean;
         procedure ConnectSequentialPoints(Bitmap : tMyBitmap);
         procedure PlotRecord1to1Map(var NewMap : tMapForm; var Bitmap : tMyBitmap);
         procedure IntervisibilityFromPoint(ChangeSymbol : boolean; FromLat,FromLong,CameraElevation,TowerHeight : float64);
         procedure DisplayCurrentRecordOnMap(MapDraw : tMapDraw; Bitmap : tMyBitmap);
         procedure DisplayTheRecord(j : integer; ShowItModal : boolean = true; CanEdit : boolean = false);
         procedure PlotAndLabelPoints(Bitmap : tMyBitmap; Plot : boolean = true);
         procedure PlotAndLabelCurrentRecord(Bitmap : tMyBitmap; Plot : boolean = true);

         procedure ConnectTwoPointsInSameRecord(var Bitmap : tMyBitmap);
         procedure PlotVectorsOnMap(var Bitmap : tMyBitmap);
         procedure PlotIcons(Var Bitmap : tMyBitmap);
         procedure PlotColorField(var Bitmap : tMyBitmap);
         procedure PlotZipATone(var Bitmap : tMyBitmap);
         procedure PlotProportionalSymbolsOnMap(var Bitmap : tMyBitmap);

         procedure ShowOtherDBsWithSameFilter(ShowFilter,ShowN,ShowThisDB : boolean);
         procedure ChangeLatLongLocation(Lat,Long : float64);
         procedure OutlineCurrentViewOnMap;

         procedure RedrawLayerOnMap;
         procedure SaveCurrentDBaseSubset(fName : PathStr; ThinFactor : integer = 1; BatchRun : boolean = false; ForceAsk : boolean = false);
         procedure GISProportionalSymbols(dbShow : byte);
         procedure CloseGisScaledForm;
         procedure ZoommaptorecordWithBufferMeters(Buffer : float64);
         procedure Recentermaponrecord;
         procedure ZoomToDBCoverageOnMap;
     {$EndIf}

     {$IfDef VCL}   //long time since testing for another operating system, so be ready for work to make it work
        procedure AddMultiFieldStats(WhatFor : shortstring; What : tMultiFieldStats; TheFields : tStringList = Nil; NewFName : shortstring = '');
        procedure AddIcon(Fill : boolean = true);
        procedure AddGeometry(What : tAddGeometry);
        procedure AddBoundingBox;
        procedure SavePointShapeFile(DoSHP : boolean = true; fName : PathStr = '');
        procedure GetFieldValuesInArrayLinkPossible(FieldDesired : ShortString; var zs : bfarray32; var Npts,Missing : int64; var Min,Max : float64);
        function ExtractDBtoCSV(ThinFactor : integer; SepChar : ANSIchar) : tStringList;
        procedure ExtractXYZtoCSV(fName : PathStr; SepChar : ANSIchar);
        procedure DefineDBColorTable;
        function GetFieldPercentile(WantedField : ShortString; PC : float64) : float64;
         function PickField(Mess: ShortString; TypesAllowed : tSetFieldType; AllFields : boolean = false) : ShortString;
         function GetMultipleNumericFields(WhatFor : shortstring) : tStringList;
         procedure BitmapForDatabase(var sBitmap : tMyBitmap; Height : integer);
         procedure SetBitmapColorsForStringFieldPlot(var Bitmap : tMyBitmap; i,Total : integer); inline;

         procedure SubsetShapeFile(var fName: PathStr; ThinFactor : integer = 1; BatchRun : boolean = false);
         procedure DisplayTable(fString : AnsiString = 'NONE'; CompleteFilter : boolean = false);
         procedure IrregularFilterDB;
         procedure SetGazTableOptions;
         function GetMaskFieldName : string10;
         procedure RenameField(OldName, NewName: ShortString);
         procedure ChangeFieldType(OldName : ShortString; NewType : ANSIchar);
         procedure ChangeFieldDecimals(OldName : ShortString; NewDecimals : byte);

         procedure LinkSecondaryTable(var FileWanted : PathStr);
         function GetElevationField(CheckOverWrite : boolean = true) : shortstring;
         procedure LimitDBtoMapArea;
         procedure ColorButtonForSymbol(BitBtn1 : tBitBtn; Capt : shortString = '');
         procedure FillComboBoxFromSingleField(var ComboBox : tComboBox; Field : shortstring);
         procedure PrepColors(Bitmap : tMyBitmap);
         function GetAnalysisFields : tStringlist;
         procedure StartVATEdits;
         function GetFieldNameForDB(Prompt : ShortString; AllowOverWrite : boolean; SuggestedName: ShortString = ''): ShortString;
         procedure AddAndFillFieldFromDEM(AddDEM : tAddDEM; fName : PathStr = ''; DecimalsToUse : integer = 6);
         procedure DisplayFieldStatistics(WantedField : ShortString);
         function GetFieldStatistics(WantedField : ShortString) : tMomentVar;
         function GetFieldPercentiles(WantedField : ShortString) : floatarray1000;
         function RGBColorFromThreeNumericFields : tColor;
         procedure CountFieldsWithSubstring(FieldName : shortString);
         procedure CloseDBtableF(Reopen : boolean = false);
     {$EndIf}

     {$IfDef NoDBGrafs}
     {$Else}
         procedure HistogramByCategory(WantXField,FilterField : shortstring; AutoRescale : boolean; Summary : tStringList = nil);
         function OldCreateHistogramFromDataBase(RegHist: boolean;  WantXField, WantYField, WantZField : shortString; AllDBs: boolean; MinUse : float64 = 1; MaxUse : float64 = -1; BinSize : float64 = -99): TThisBaseGraph;
         function CreateHistogramFromDataBase(RegHist: boolean; Fields : tStringList; AllDBs: boolean; MinUse : float64 = 1; MaxUse : float64 = -1; BinSize : float64 = -99): TThisBaseGraph;
         function CreateHistogramFromClustersInDataBase(WantXField: shortString;  UseClusters: boolean): TThisBaseGraph;

         procedure SingleRose(AddTitle : shortString; Field1,Field2 : ShortString);
         function WaveFormGraph(SingleGraph : boolean) : tThisBaseGraph;
         function CreateScatterGram(anXField, anYField: ShortString; Color : tColor = clRed; Connect : boolean = false; Capt : shortstring = '';
            H_lab : shortstring = ''; V_lab : shortString = ''; NormProb : boolean = false) : TThisbasegraph;
         procedure AddSeriesToScatterGram(Graph : TThisbasegraph; Color : tColor; anXField,anYField : ShortString; Connect : boolean = false);
         function Stationtimeseries : tThisBaseGraph;
         function MakeGraph(Graphtype : tdbGraphType; Ask : boolean = true) : TThisbasegraph;
         function ActuallyDrawGraph(Graphtype : tdbGraphType) : TThisbasegraph;
         procedure MonthlyWindPlotCurrentPoint;
     {$EndIf}

     {$IfDef ExGeography}
     {$Else}
        procedure PlotKoppenStations(Bitmap : tMyBitmap);
        procedure StartClimateDisplay(Mode : integer);
     {$EndIf}

     {$IfDef ExRedistrict}
     {$Else}
        procedure SetRedistrictPattern(var Bitmap2 : tMyBitmap);
     {$EndIf}

     {$IfDef ExSidescan}
     {$Else}
        //procedure PlotSideScanCoverage(Bitmap : tMyBitmap);
        //procedure PlotSingleSideScanLeg(Bitmap : tMyBitmap);
     {$EndIf}

     {$IfDef IncludeRiverNetworks}
        procedure GetDrainageBasinStats(var StrahlerOrder : integer; var ThalwegLength,TotalLength : float64);
        procedure GetDrainageBasinNetwork;
        procedure FindDrainageBasinFromStreams;
     {$EndIf}

     {$IfDef ExGeostats}
     {$Else}
        procedure PlotFabric(var Bitmap : tMyBitmap);
        function SSOandaspectdiagrams(var s1s2,s2s3,Trend,RoughnessFactor : float64) : boolean;
     {$EndIf}

     {$IfDef ExSat}
     {$Else}
        function GridStatsName(CurDEM : integer; Ext : ExtStr) : PathStr;
     {$EndIf}

     {$IfDef ExGeology}
     {$Else}
        procedure PlotTernary(var Bitmap : tMyBitmap);
        function FocalMechanismColor : tPlatformColor;
        function QuakeRadius : integer;
        function QuakeColor : tPlatformColor;
        function GetFocalPlanes : tStringList;
        procedure FocalPlaneStats(What : tFocalPlaneWhat);
        procedure PlotEarthQuakesByMechanism(Bitmap : tMyBitmap);
        procedure QuakeFocalMechs(xpic,ypic : integer; Lat,Long : float64; var RecsFound : integer; var Bitmap : tMyBitmap;  NetRad : integer);
        function DrawFocalMechanism(NetRadius : integer) : tMyBitmap;
        procedure PlotDipsAndStrikes(Bitmap : tMyBitmap);
    {$EndIf}
  end;

procedure InitializeDEMdbs;


{$IfDef NoDBMaps}
{$Else}
      procedure LoadShapeFileGroup(MapOwner : tMapForm; fName : PathStr = '');
      procedure UpdateShapeFileGroup;
      procedure LegendFromShapeFileGroup(MapOwner : tMapForm; FName : PathStr);
      function OpenGazetteer(var i : integer; fName : PathStr; MapOwner : tMapForm) : boolean;
      function NumOpenDatabaseThisMap(MapOwner : tMapForm) : integer;
      procedure ConvertShapeFileToKML(fName : PathStr; MapOwner : tMapForm);
{$EndIf}

{$IfDef FMX}
   function OpenNumberedGISDataBase(var GISNum : integer; fName : PathStr; ShowTable : boolean = false; MapDraw : tMapDraw = nil) : boolean;
{$EndIf}

{$IfDef VCL}
   function OpenNumberedGISDataBase(var GISNum : integer; fName : PathStr; ShowTable : boolean = false; LocalMapOnly : boolean = false; MapOwner : tMapForm = nil) : boolean;
   function OpenMultipleDataBases(WhatFor : ANSIstring; fName : Pathstr = ''; ShowTable : boolean = true) : integer;
   function TotalNumOpenDatabase : integer;
   function CopyDatabaseAndOpen(GIS : TGISdataBaseModule; OpenLink : boolean = true) : integer;
   procedure OpenDBForModalEdit(fName : PathStr);
   function PickOpenGISDataBase(WhatFor : shortString; Ignore : integer = 0; Geometry : integer = 0; FieldNeeded : ShortString = '') : integer;
   procedure MaskDEMFromShapeFile(DEM,DBonTable : integer; UseShapeFile,MaskShapesAreIn : boolean; SingleRecord  : integer; MaskingDistance : float64; ProgressCount : shortstring = '');
   {$IfDef NoDBGrafs}
   {$Else}
      function GraphFromCSVfile(var sl : tStringList; ShowTable : boolean; Lines : boolean = false) : tThisBaseGraph;
   {$EndIf}
{$EndIf}

function FindOpenDataBase(var db : integer) : boolean;

procedure CloseAndNilNumberedDB(var i : integer);
function NumOpenDB : integer;
function ValidDB(theDB : integer) : boolean;
function ValidDBfName(fName : PathStr) : boolean;

procedure AdjustGazFeatureName(var FeatureName : ShortString);

procedure MakeLinesFromPoints(GISDataBase : TGISdataBaseModule; fName : PathStr = ''; ShapeTypeWanted : integer = -99; Thin : integer = -1);

procedure DoKMeansClustering(DBonTable : integer);
procedure ClusterMapLocation(DBonTable : integer; TheFilters : tStringList = nil);
procedure MapsByClusterAndDEM(DBonTable : integer);

procedure ComputeVDatumShift(dbOnTable : integer);
function AnalyzeVDatumShift(CSVName : PathStr; ErrorLog : tStringList = Nil) : integer;

function SortDataBase(DBOnTable : integer; Ascending : boolean; aField : shortString = ''; OutputDir : PathStr = '') : integer;
procedure SortAndReplaceDataBase(DBOnTable : integer; Ascending : boolean; aField : shortString = '');

function MakeFiltersForCluster(DBonTable : integer; IncludeBaseFilter : boolean) : tStringList;

{$IfDef IncludeRiverNetworks}
   procedure LoadNodeTableToMemory(NodeTable : tMyData; var NumNodes : integer; var RiverNetwork : pRiverNetwork; ReadLinks,ReadCont,ReadOrders : boolean);
{$EndIf}


const
   IDRecScreenTolerance : integer = 8;
   IDRecScreenIncr : integer = 1;
var
   GISdb : array[1..MaxDataBase] of TGISdataBaseModule;
   DBPlotOrder :  array[1..MaxDataBase] of byte;
   DBPlotNow :  array[1..MaxDataBase] of boolean;
   ClimateStationDB,
   KoppenGridDB,
   WindsDB,
   PiratesDB,
   DBEditting,
   DBEditRecord,
   ThinToPlot,
   dbxpic,dbypic : integer;
   HighLightDBOnWorld,
   AutoOverwriteDBF,
   ZeroRecordsAllowed : boolean;
   PreEditFilter : ANSIstring;


implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.DFM}


uses
   {$IfDef VCL}
      Nevadia_Main,
   {$EndIf}

   {$IfDef ExRedistrict}
   {$Else}
      demredistrict,
   {$EndIf}

   {$IfDef ExGeography}
   {$Else}
      KoppenGr,
   {$EndIf}

   {$IfDef ExSat}
   {$Else}
      DEMEROS,
   {$EndIf}

   {$IfDef ExGeology}
   {$Else}
      sc_Colmain,
      Petmar_geology,
   {$EndIf}

   {$IfDef NoClustering}
   {$Else}
      MVClusterClientDataSet,
   {$EndIf}

   {$IfDef VCL}
      DEMdbFieldPicker,
      DEMDbDisplay,
      db_display_options,
      Get_db_coloring,
      Petimage_form,
      rgb_colors_three_params,
      DEM_Gaz_Opts,
      DEMShowDbRecord,
      Map_Overlays,
      BaseMap,
      Thread_timers,
      demdbfilter,
      db_join,
      demstringgrid, DEM_indexes,
      zipatone,
      kml_opts,
      New_Field,
      {$IfDef ExDataManip}
      {$Else}
         DEMHandW,
      {$EndIf}
   {$EndIf}

   {$IfDef ExPLSS}
   {$Else}
      DEM_PLSS,
   {$EndIf}

   {$IfDef ExTiger}
   {$Else}
      DEMTiger,
   {$EndIf}

   {$IfDef ExcludeExternalTools}
   {$Else}
      MD_Use_tools,
   {$EndIf}

   {$IfDef ExGeoStats}
   {$Else}
      DEMStat,
   {$EndIf}

   {$IfDef ExSidescan}
   {$Else}
      SideImg,
   {$EndIf}

   DP_control,Dragon_Plot_init,

   {$IfDef ExDEMIX}
   {$Else}
      demix_definitions,
      DEMIX_graphs,
      DEMIX_Control,
   {$EndIf}

   Make_Tables,
   GPS_strings,
   DEM_Manager,
   clusteroptions,
   DEMCoord,
   PetImage,
   gdal_tools,
   DataBaseCreate,
   DEMDef_routines;

{$include demdatabase_special_cases.inc}


{$IfDef NoDBEdits}
{$Else}
   {$include demdatabase_field_edits.inc}
{$Endif}

{$IfDef NoDBMaps}
{$Else}
   {$include demdatabase_maps.inc}
   {$include demdatabase_legend.inc}
{$Endif}

{$IfDef NoDBGrafs}
{$Else}
   {$include demdatabase_graph.inc}
{$EndIf}

{$IfDef ExGeology}
{$Else}
   {$include demdatabase_geology.inc}
{$Endif}

{$IfDef IncludeRiverNetworks}
   {$include demdatabase_drainage_basin.inc}
{$EndIf}


procedure TGISdataBaseModule.BackupDB;
begin
   CopyFile(DBFullName,MyData.DBBakDir + DBName +  '_bak_' + CurrentTimeForFileName + DefaultDBExt);
end;


function TGISdataBaseModule.NumUniqueEntriesInDB(fName : shortstring) : integer;
begin
   EmpSource.Enabled := false;
   Result := MyData.NumUniqueEntriesInDB(fName);
end;

function SortDataBase(DBOnTable : integer; Ascending : boolean; aField : shortString = ''; OutputDir : PathStr = '') : integer;
var
   GridForm : tGridForm;
   Report : tStringList;
   SortedfName : PathStr;
   TStr : shortstring;
   ft,col : integer;
   NeedRestore : boolean;
begin
   {$IfDef RecordDBsort} WriteLineToDebugFile('SortDataBase ' + GISDB[DBonTable].dbName + '  ' + aField); {$EndIf}
   if (aField = '') then aField := GISDB[DBonTable].PickField('field to sort on',[ftString,ftSmallInt,ftFloat,ftInteger]);
   if aField = '' then exit;

   NeedRestore := false;
   if (GISDB[DBonTable].dbtablef <> Nil) and GISDB[DBonTable].dbtablef.AnyHiddenColumns then begin
      If not AnswerIsYes('Sort without hidden fields') then begin
         GISdb[DBonTable].dbTableF.SaveHiddenColumns;
         GISdb[DBonTable].dbTableF.UnHideColumns;
         NeedRestore := true;
      end;
   end;

   if (GISDB[DBonTable].MyData.GetFieldType(aField) = ftString) then ft := 0 else ft := 2;
   GridForm := tGridForm.Create(Application);
   GridForm.HideCorrelationControls;
   GridForm.ShowSortingControls(true);
   GridForm.Caption := 'Sorting';
   Report := GISdb[DBonTable].ExtractDBtoCSV(1,',');
   if (OutputDir = '') then OutPutDir := mdTempDir;
   TStr := '_sorted_' + AField + '_';
   SortedfName := NextFileNumber(OutputDir,GISdb[DBonTable].dbName + TStr ,'.csv');
   if StrUtils.AnsiContainsText(SortedfName,TStr + TStr) then begin
      SortedfName := StringReplace(SortedfName,TStr + TStr,TStr,[rfReplaceAll, rfIgnoreCase]);
      SortedfName := ExtractFileNameNoExt(SortedfName);
      while SortedfName[length(SortedfName)] in ['0'..'9'] do Delete(SortedfName,length(SortedfName),1);
      SortedfName := NextFileNumber(OutputDir,SortedfName,'.csv');
   end;
   Report.SaveToFile(SortedfName);
   Report.Free;
   GridForm.ReadCSVFile(SortedfName);

   col := -1;
   repeat
      inc(Col);
      TStr := GridForm.StringGrid1.Cells[Col,0];
   until (TStr = aField);
   GridForm.SetFormSize;
   SortGrid(GridForm.StringGrid1,pred(Col),ft,Ascending);

   StringGridToCSVFile(SortedfName,GridForm.StringGrid1,Nil);
   if (GISdb[DBonTable].theMapOwner <> nil) then Result := GISdb[DBonTable].theMapOwner.OpenDBonMap('',SortedfName)
   else OpenNumberedGISDataBase(Result,SortedfName,true);
   {$IfDef RecordDBsort} WriteLineToDebugFile('SortDataBase ' + GISDB[DBonTable].dbName + ' created ' + SortedfName); {$EndIf}
   GridForm.Close;

   if NeedRestore then begin
      GISdb[DBonTable].dbTableF.RestoreHiddenColumns;
      GISdb[DBonTable].dbTableF.HideColumns;
      GISdb[DBonTable].dbTableF.ShowStatus;
   end;
end;


procedure SortAndReplaceDataBase(DBOnTable : integer; Ascending : boolean; aField : shortString = '');
var
   NewDB : integer;
   SortedFName{,OutputDir} : PathStr;
begin
   //OutputDir := ExtractFilePath(GISdb[DBonTable].DBfullName);
   NewDB := SortDataBase(DBOnTable,Ascending,aField);  //,OutputDir);
   SortedFName := GISdb[NewDB].DBfullName;
   CloseAndNilNumberedDB(NewDB);
   GISdb[DBonTable].BackupDB;
   DeleteFileIfExists(GISdb[DBonTable].DBfullName);
   MoveFile(SortedFName,GISdb[DBonTable].DBfullName);
   GISdb[DBonTable].MyData.ClearBDEdata;
   GISdb[DBonTable].MyData.ReopenDatabase(GISdb[DBonTable].DBfullName);
end;


procedure MakeLinesFromPoints(GISDataBase : TGISdataBaseModule; fName : PathStr = ''; ShapeTypeWanted : integer = -99; Thin : integer = -1);
var
   i, fLength,fPrec : integer;
   aft : tFieldType;
   NewTable : tMyData;
   ThreeD,MultipleLines,TimeField,LineEndPoints : boolean;
   Field3D,MultipleField,TimeFName : ShortString;
   MultipleValues : tStringList;
   ShapeFileCreator : tShapeFileCreation;
   LastLat,LastLong : float64;

   procedure DoPoint;
   var
      Lat,Long : float64;
   begin
        if GISDataBase.ValidLatLongFromTable(Lat,Long) then begin
           if (abs(LastLat - Lat) > 0.00000001) or (abs(LastLong - Long) > 0.00000001) then begin
               if ThreeD then begin
                  if (GISDataBase.MyData.GetFieldByNameAsString(Field3D) <> '') then
                     ShapeFileCreator.AddPointWithZToShapeStream(Lat,Long,GISDataBase.MyData.GetFieldByNameAsFloat(Field3D));
               end
               else ShapeFileCreator.AddPointToShapeStream(Lat,Long);
               LastLat := Lat;
               LastLong := Long;
           end;
        end;
   end;

     procedure DoALine(LineName : string35; RecNum : integer);
     var
        i : integer;
        Lat1,Lat2,Long1,Long2 : float64;
        Time1,Time2 : shortstring;
      begin
         ShowHourglassCursor;
         GISDataBase.MyData.First;
         GISDataBase.ValidLatLongFromTable(Lat1,Long1);
         if (TimeField) then Time1 := GISDataBase.MyData.GetFieldByNameAsString(TimeFName);
         GISDataBase.EmpSource.Enabled := false;
         while not GISDataBase.MyData.eof do begin
            DoPoint;                 //this will have the first point
            for i := 1 to Thin do GISDataBase.MyData.Next;
         end;
         if (Thin > 1) then DoPoint;   //insure last point is included
         GISDataBase.ValidLatLongFromTable(Lat2,Long2);
         if (TimeField) then Time2 := GISDataBase.MyData.GetFieldByNameAsString(TimeFName);
         if ShapeFileCreator.PtsInShapeStream > 0 then begin
            NewTable.Insert;
            NewTable.SetFieldByNameAsInteger(RecNoFName,RecNum);
            NewTable.SetFieldByNameAsString('NAME',LineName);
            if TimeField then begin
               NewTable.SetFieldByNameAsString('TIME_START',Time1);
               NewTable.SetFieldByNameAsString('TIME_END',Time2);
            end;
            if LineEndPoints then begin
               NewTable.SetFieldByNameAsFloat('LAT_START',Lat1);
               NewTable.SetFieldByNameAsFloat('LONG_START',Long1);
               NewTable.SetFieldByNameAsFloat('LAT_END',Lat2);
               NewTable.SetFieldByNameAsFloat('LONG_END',Long2);
            end;
            NewTable.Post;
            ShapeFileCreator.ProcessShapeFileRecord;
         end;
      end;

begin
   LastLat := 99;
   LastLong := 999;
   if (fName = '') then GetFileNameDefaultExt('New shape file',DBNameMask,FName,false);
   if (fName <> '') then with GISDataBase do begin
      {$IfDef RecordMakeLineArea} WriteLineToDebugFile('MakeLinesFromPoints new file: ' + fName); {$EndIf}
      if (ShapeTypeWanted < 0) then begin
         if AnswerIsYes('Area shape file') then ShapeTypeWanted := 5 else ShapeTypeWanted := 3;
      end;

      ThreeD := AnswerIsYes('3D shapefile');
      if ThreeD then begin
         Field3D := PickField('Three D values' ,[ftInteger,ftFloat]);
         ShapeTypeWanted := ShapeTypeWanted + 10;
      end;

      MultipleLines := AnswerIsYes('Multiple records based on filter');
      if MultipleLines then begin
         MultipleField := PickField('Multiple lines',[ftInteger,ftString]);
      end;

      if (Thin < 0) then begin
         Thin := 1;
         ReadDefault('Thinning factor',Thin);
      end;

      TimeField := AnswerIsYes('Start/end times');
      if TimeField then begin
         TimeFName := PickField('Multiple lines' ,[ftInteger,ftString,ftFloat]);
      end;

      LineEndPoints := (ShapeTypeWanted in [3,13]) and AnswerIsYes('Line end points');

      Make_tables.DefineAndCreateANewTable(FName,false,false,false,true,true);
      NewTable := tMyData.Create(fName);
      if TimeField then begin
         aft := MyData.GetFieldType(TimeFName);
         fLength := MyData.GetFieldLength(TimeFName);
         fPrec := MyData.GetFieldPrecision(TimeFName);
         NewTable.InsureFieldPresentAndAdded(aft,'TIME_START',fLength,fPrec);
         NewTable.InsureFieldPresentAndAdded(aft,'TIME_END',fLength,fPrec);
      end;
      if LineEndPoints then begin
         NewTable.InsureFieldPresentAndAdded(ftFloat,'LAT_START',11,7);
         NewTable.InsureFieldPresentAndAdded(ftFloat,'LONG_START',12,7);
         NewTable.InsureFieldPresentAndAdded(ftFloat,'LAT_END',11,7);
         NewTable.InsureFieldPresentAndAdded(ftFloat,'LONG_END',12,7);
      end;

      System.Delete(Fname,Length(FName)-3,4);
      MapScreenX1 := -9999;
      MapScreenY1 := -9999;
      {$IfDef RecordMakeLineArea} WriteLineToDebugFile('Calling SetUpShapeFiles'); {$EndIf}
      ShapeFileCreator := tShapeFileCreation.Create(WGS84DatumConstants,fName,false,ShapeTypeWanted);
      ShowHourglassCursor;
      with GISDataBase do begin
         if MultipleLines then begin
           DBFieldUniqueEntries(MultipleField,MultipleValues);
           for I := 0 to pred(MultipleValues.Count) do begin
              EmpSource.Enabled := false;
              MyData.ApplyFilter(MultipleField + '=' + QuotedStr(MultipleValues.Strings[i]));
              DoALine(MultipleValues.Strings[i],succ(i));
           end;
           MultipleValues.Free;
           ClearGISFilter;
         end
         else DoALine(DBName,1);
         EmpSource.Enabled := true;
      end;
      {$IfDef RecordMakeLineArea} WriteLineToDebugFile('Closeing ShapeFile'); {$EndIf}
      ShapeFileCreator.CloseShapeFiles;
      GISDataBase.TheMapOwner.OpenDBonMap('',fName);
   end;
end;


function ValidDBfName(fName : PathStr) : boolean;
var
   Ext : ExtStr;
begin
   Ext := UpperCase(ExtractFileExt(fName));
   Result := (Ext = '.DBF') or (Ext = '.CSV') or (Ext = '.KML') or (Ext = '.DB') or (Ext = '.SDB') or (Ext = '.SHZ') or (Ext = '.FIT') or (Ext = '.SHP')
          or (Ext = '.TXT')   or (Ext = '.XML')  or (Ext = '.KML')  or (Ext = '.CDS') or (Ext = '.XYZ') or (Ext = '.GPX');
end;


function ValidDB(theDB : integer) : boolean;
begin
   Result := (theDB > 0) and (theDB <= MaxDataBase) and (GISdb[theDB] <> Nil) and (GISdb[theDB].MyData <> nil);
end;


function TGISdataBaseModule.IsICESat : boolean;
begin
    Result := MyData.FieldExists('H_CANOPY') and MyData.FieldExists('H_TE_BEST_') and MyData.FieldExists('H_TE_UNCER');
end;

procedure TGISdataBaseModule.CalculateAreaorCentroid(DoCentroid : boolean);
var
   area,xcent,ycent : float64;
   xpic,ypic : integer;
begin
   Area := aShapeFile.AreaAndCentroid(TheMapOwner.MapDraw.PrimMapProj,MyData.RecNo,Ycent,xcent);
   TheMapOwner.MapDraw.LatLongDegreeToScreen(ycent,xcent,xpic,ypic);
   if DoCentroid then begin
      PETMAR.ScreenSymbol(theMapOwner.Image1.Canvas,xpic,ypic,Box,3,claRed);
      MessageToContinue('Centroid: ' + LatLongDegreeToString(Ycent,xcent,MDDef.OutPutLatLongMethod),True);
   end
   else begin
      MessageToContinue('Area: ' + SmartAreaFormat(area),True);
   end;
end;


procedure TGISdataBaseModule.LimitFieldDecimals(SelectedColumn : shortstring; NumDec : integer);
var
   i,rc,nc : integer;
   Factor,Value : float64;
begin
   if (MyData.GetFieldType(SelectedColumn) = ftFloat) and (MyData.GetFieldPrecision(SelectedColumn) > NumDec) then begin
      Factor := System.Math.Power(10,NumDec);
      MyData.First;
      StartProgress('Limit decimals, ' + SelectedColumn);
      i := 0;
      MyData.ProgressVars(rc,nc);
      while not MyData.eof do begin
         if (I mod nc = 0) then begin
            EmpSource.Enabled := false;
            UpdateProgressBar(i/rc);
         end;
         inc(i);
         MyData.Edit;
         if MyData.CarefullyGetFieldByNameAsFloat64(SelectedColumn,Value) then begin
            Value := round(Value * Factor) / Factor;
            MyData.SetFieldByNameAsFloat(SelectedColumn,Value);
         end;
         MyData.Next;
      end;
      ChangeFieldDecimals(SelectedColumn,NumDec);
      ShowStatus;
   end;
end;


function TGISdataBaseModule.GetIntegerFromTableLink(FieldDesired : ShortString; var Value : integer) : boolean;
var
   TStr : shortstring;
begin
   TStr := GetStringFromTableLink(FieldDesired);
   if (TStr = '') then Result := false
   else begin
      Result := true;
      Value := StrToInt(TStr);
   end;
end;


function TGISdataBaseModule.FindValidJoin(TheFilter : string) : boolean;
var
   aFilter : shortstring;
   Recs : integer;
begin
   if LinkTable.IsStringField(dbOpts.LinkFieldOtherDB) then aFilter := dbOpts.LinkFieldOtherDB + '=' + QuotedStr(TheFilter)
   else aFilter := dbOpts.LinkFieldOtherDB + '=' + TheFilter;
   LinkTable.ApplyFilter(aFilter);
   Recs := LinkTable.RecordCount;
   Result := (Recs = 1) or ((Recs > 1) and MDDef.AllowFirstOfMultipleJoins);
end;


function TGISdataBaseModule.FindFieldRangeLinkPossible(FieldDesired : shortString; var aMinVal,aMaxVal : float64) : boolean;
var
   Num,Valid  : integer;
   sum : float64;
begin
   FindFieldRangeLinkPossible(FieldDesired,Num,Valid,Sum,aMinVal,aMaxVal);
   Result := Num > 0;
end;


procedure TGISdataBaseModule.TrimOneStringField(SelectedColumn : shortstring);
var
   Max : integer;
   LS : shortstring;
begin
   LongestString(SelectedColumn,Max,LS);
   MyData.TrimField(SelectedColumn,Max);
   RespondToChangedDB;
end;

procedure tGISdataBaseModule.TrimAllStringFields;
var
   i : integer;
   SelectedColumn : shortstring;
begin
    StartProgress('Trim fields');
    for i := 0 to pred(MyData.FieldCount) do begin
       UpdateProgressBar(i/MyData.FieldCount);
       if (MyData.GetFieldType(i) = ftString) then begin
          SelectedColumn := MyData.GetFieldName(i);
          TrimOneStringField(SelectedColumn );
       end;
    end;
    ShowStatus;
end;


function TGISdataBaseModule.FindFieldRangeLinkPossible(FieldDesired : shortString; var Num,Valid  : integer;  var Sum,aMinVal,aMaxVal : float64) : boolean;
var
   fVal : float64;
   Skip,i,trc : integer;
   ShortField : ShortString;
   UseLink,Found : boolean;
begin
   {$IfDef RecordRange} WriteLineToDebugFile('FindFieldRangeLinkPossible in ' + Table.TableName + '  ' + FieldDesired); {$EndIf}
   aMinVal := 0;
   aMaxVal := 0;
   Result := false;
   if (FieldDesired = '') then exit;
   Skip := 1;
   trc := Mydata.RecordCount;
   while (trc div Skip > 5000) do inc(Skip);
   trc := trc div 20;
   if trc < 1 then trc := 1;
   ShortField := FieldDesired;
   UseLink := LinkedField(ShortField);
   if UseLink then begin
      Result := LinkTable.IsNumericField(ShortField);
   end
   else begin
      Result := MyData.IsNumericField(ShortField);
   end;
   if not Result then exit;

   Num := 0;
   Valid := 0;
   Sum := 0;
   aMaxVal := -99e99;
   aMinVal := +99e99;
   MyData.First;
   {$IfDef VCL} if WantShowProgress and ShowSatProgress then StartProgress('Range Compute ' + FieldDesired); {$EndIf}
   repeat
      if WantShowProgress and ShowSatProgress and (Num mod trc = 0) then begin
         {$IfDef VCL} UpdateProgressBar(Num*Skip/trc); {$EndIf}
         if (EmpSource <> Nil) then EmpSource.Enabled := false;
      end;
      inc(Num);
      if WantOut then break;
      if UseLink then Found := GetFloatFromTableLink(ShortField,fval)
      else Found := MyData.CarefullyGetFieldByNameAsFloat64(FieldDesired,fVal);
      if Found then begin
         Inc(Valid);
         sum := sum + fVal;
         Petmath.CompareValueToExtremes(fVal,aMinVal,aMaxVal);
      end;
      for i := 1 to Skip do MyData.Next;
   until MyData.EOF;
   Result := Valid > 0;
   {$IfDef VCL} if WantShowProgress and ShowSatProgress then EndProgress; {$EndIf}
   {$IfDef RecordRange} WriteLineToDebugFile('FindFieldRangeLinkPossible out, range=' + RealToString(aMinVal,-12,-2) + ' to ' + RealToString(aMaxVal,-12,-2)); {$EndIf}
end;

function TGISdataBaseModule.GetFloat32FromTableLinkPossible(FieldDesired : shortstring; var Value : float32) : boolean;
var
   TStr : shortstring;
begin
   TStr := GetStringFromTableLinkPossible(FieldDesired);
   if (TStr = '') then Result := false
   else begin
      Result := true;
      Value := StrToFloat(TStr);
   end;
end;


function TGISdataBaseModule.GetFloatFromTableLink(FieldDesired : shortstring; var Value : float64) : boolean;
var
   TStr : shortstring;
begin
   TStr := GetStringFromTableLink(FieldDesired);
   if (TStr = '') then Result := false
   else begin
      Result := true;
      Value := StrToFloat(TStr);
   end;
end;


function TGISdataBaseModule.GetStringFromTableLink(FieldDesired : ShortString) : ShortString;
begin
   try
      if FindValidJoin(MyData.GetFieldByNameAsString(dbOpts.LinkFieldThisDB)) then begin
         Result := LinkTable.GetFieldByNameAsString(FieldDesired);
         Result := ptTrim(Result);
      end
      else Result := '';
   except
      on Exception do Result := '';
   end;
end;


function TGISdataBaseModule.GetStringFromTableLinkPossible(FieldDesired : ShortString) : ShortString;
begin
   if (FieldDesired = '') then begin
      Result := '';
   end
   else if (LinkTable <> Nil) and LinkedField(FieldDesired) then begin
      try
         if FindValidJoin(Mydata.GetFieldByNameAsString(dbOpts.LinkFieldThisDB)) then
            Result := LinkTable.GetFieldByNameAsString(FieldDesired)
         else Result := '';
      except
         on Exception do Result := '';
      end;
   end
   else Result := MyData.GetFieldByNameAsString(FieldDesired);
   Result := ptTrim(Result);
end;


procedure TGISdataBaseModule.PickNumericFields(GraphType :  tdbGraphType; NFields : integer; l1 : shortstring; l2 : shortstring; l3 : shortstring; StringColor : boolean = false);
{$IfDef FMX}
begin
{$EndIf}
{$IfDef VCL}
var
   FieldPicker : TFieldPicker;
   NumFields2,
   StringIntFields,
   NumerFields   : tStringList;
   i : integer;
begin
   FieldPicker := TFieldPicker.Create(Application);
   if (dbOpts.StringColorField = '') and (NFields < 4) then FieldPicker.HideAdvancedOptions;
   NumerFields := Nil;
   PetdbUtils.GetFields(MyData,dbopts.VisCols,NumericFieldTypes,NumerFields);
   if (LinkTable <> Nil) then begin
      PetdbUtils.GetFields(LinkTable,AllVis,NumericFieldTypes,NumFields2);
      for I := 0 to pred(NumFields2.Count) do NumerFields.Add('LINK-' + NumFields2.Strings[i]);
      NumFields2.Free;
   end;
   with FieldPicker do begin
      Label1.Caption := l1;
      Label2.Caption := l2;
      Label3.Caption := l3;

      CheckBox1.Checked := MDDef.ReverseZFields;
      CheckBox1.Visible := (NFields >= 3);
      ComboBox2.Visible := (NFields > 1);
      for i := 0 to pred(NumerFields.Count) do begin
         ComboBox1.Items.Add(NumerFields.Strings[i]);
         ComboBox2.Items.Add(NumerFields.Strings[i]);
         ComboBox3.Items.Add(NumerFields.Strings[i]);
         ComboBox5.Items.Add(NumerFields.Strings[i]);
         ComboBox6.Items.Add(NumerFields.Strings[i]);
      end;
      if (DbOpts.XField = '') then ComboBox1.Text := NumerFields.Strings[0] else ComboBox1.Text := DbOpts.XField;
      if (DbOpts.YField = '') then ComboBox2.Text := NumerFields.Strings[1] else ComboBox2.Text := DbOpts.YField;
      if StringColor then begin
         PetdbUtils.GetFields(MyData,dbopts.VisCols,[ftString,ftInteger,ftSmallInt],StringIntFields);
         if (DbOpts.StringColorField = '') then ComboBox4.Text := StringIntFields[0] else ComboBox4.Text := DbOpts.StringColorField;
         StringIntFields.Free;
      end;
      ComboBox5.Text := DbOpts.NumericColorField;
      ComboBox6.Text := DbOpts.SizeField;

      if (NumerFields.Count < 3) or (NFields < 3) then begin
         ComboBox3.Visible := false;
         ComboBox3.Text := '';
         Label3.Visible := false;
      end
      else begin
         if (NFields = 3) and StringColor then begin
            Label5.Visible := true;
            ComboBox4.Visible := true;
            ComboBox3.Visible := false;
            for i := 0 to pred(StringIntFields.Count) do ComboBox4.Items.Add(StringIntFields.Strings[i]);
         end
         else if (NFields = 3) and (DbOpts.ZField = '') then ComboBox3.Text := NumerFields.Strings[2] else ComboBox3.Text := DbOpts.ZField;
      end;
      NumerFields.Free;

      PetdbUtils.GetFields(LinkTable,dbOpts.VisCols,[ftString,ftInteger,ftSmallInt],NumerFields);
      for i := 0 to pred(NumerFields.Count) do ComboBox4.Items.Add(NumerFields.Strings[i]);

      if (GraphType in [dbgtN2Dgraphcolorcodetext1,dbgtN2Dgraph1,dbgtN2Dgraphsimplelines1]) then begin
         ComboBox5.Visible := false;
         ComboBox6.Visible := false;
         Label6.Visible := false;
         Label7.Visible := false;
      end;

      if (GraphType in [dbgtN2Dgraph1,dbgtN2Dgraphsimplelines1]) then begin
         ComboBox4.Visible := false;
         Label5.Visible := false;
      end;

      ShowModal;

      MDDef.ReverseZFields := CheckBox1.Checked;
      DbOpts.XField := ComboBox1.Text;
      DbOpts.YField := ComboBox2.Text;
      DbOpts.ZField := ComboBox3.Text;
      DbOpts.StringColorField := ComboBox4.Text;
      DbOpts.NumericColorField := ComboBox5.Text;
      DbOpts.SizeField := ComboBox6.Text;
      Free;
   end;
   NumerFields.Free;
{$EndIf}
end;


procedure TGISdataBaseModule.GetPointArrayForDBField(FieldName : ShortString; var Values : Petmath.bfarray32; var NPts : int64; Min : float64 = 1; Max : float64 = -1);
var
  fv : float32;
  rc : integer;
begin
   NPts := 0;
   {$IfDef RecordDataBaseTiming} WriteLineToDebugFile('GetPointArrayForDBField start'); {$EndIf}
   {$IfDef VCL} if WantShowProgress then StartProgress('Get ' + FieldName); {$EndIf}
   EmpSource.Enabled := false;
   MyData.First;
   rc := MyData.RecordCount;
   repeat
     {$IfDef VCL} if WantShowProgress and (NPts mod 1000 = 0) then UpdateProgressBar(Npts/rc); {$EndIf}
     if GetFloat32FromTableLinkPossible(FieldName,fv) then begin
        if (Min > Max) or ((fv >= Min) and (fv <= Max)) then begin
           values[NPts] := fv;
           inc(NPts);
        end;
     end;
     MyData.Next;
     if (NPts > bfArrayMaxSize) then break;
   until MyData.EOF;
   {$IfDef VCL} if WantShowProgress then EndProgress; {$EndIf}
end;


procedure TGISdataBaseModule.CountFieldsWithSubstring(FieldName : shortString);
var
   aString,bString : ShortString;
   i,Count : integer;
   Results : tStringList;

   procedure OneField(FieldName : shortstring);
   var
      j : integer;
   begin
      MyData.First;
      Count := 0;
      j := 0;
      StartProgress('Substring in ' + FieldName);
      while not MyData.eof do begin
          if (j Mod 100 = 0) then begin
             EmpSource.Enabled := false;
             UpDateProgressBar(j/MyData.FiltRecsInDB);
          end;
          inc(j);
         bString := MyData.GetFieldByNameAsString(FieldName);
         if StrUtils.AnsiContainsText(bString,aString) then inc(Count);
         MyData.Next;
      end;
      Results.Add(IntegerToString(Count,8) + '  in field ' + FieldName);
   end;

begin
   Petmar.GetString('Substring to search for',aString,false,ReasonableTextChars);
   Results := tStringList.Create;
   Results.Add(astring + '  in '  + dbName);
   Results.Add('');
   ShowHourglassCursor;
   EmpSource.Enabled := false;
   if FieldName <> '' then begin
      OneField(FieldName);
   end
   else begin
       for i := 0 to pred(MyData.FieldCount) do begin
          if (MyData.GetFieldType(i) = ftString) then begin
             FieldName := MyData.GetFieldName(i);
             OneField(FieldName);
          end;
       end;
   end;
   ShowStatus;
   Petmar.DisplayAndPurgeStringList(Results,astring + 'in ' + DBName);
end;


function TGISdataBaseModule.DEMwithDBsMap : boolean;
begin
   Result := (TheMapOwner <> Nil) and (TheMapOwner.MapDraw.DEMonMap <> 0) and (DEMGlb[TheMapOwner.MapDraw.DEMonMap] <> Nil);
end;


function TGISdataBaseModule.GetLatLongToRepresentRecord(var Lat,Long : float64) : boolean;
var
  bBox : sfBoundBox;
begin
   if LineOrAreaShapeFile(ShapeFileType) then begin
      if CentroidPresent then begin
         Lat := MyData.GetFieldByNameAsFloat('LAT_CENTRD');
         Long := MyData.GetFieldByNameAsFloat('LON_CENTRD');
      end
      else begin
         bbox := MyData.GetRecordBoundingBox;
         Lat := 0.5 * (bbox.YMax + bbox.YMin);
         Long := 0.5 * (bbox.xMax + bbox.yMin);
      end;
      Result := true;
   end
   else Result := ValidLatLongFromTable(Lat,Long);
end;


procedure TGISdataBaseModule.PurgeDeletedRecords;
begin
   EmpSource.Enabled := false;
   MyData.PurgeDeletedRecords;
   MyData.AssignEmpSource(EmpSource);
   ClearGISFilter;
   RedrawLayerOnMap;
   RespondToChangedDB;
end;


function TGISdataBaseModule.ExportToKML(Ask,Default : boolean; SepExpField : ShortString = '') : PathStr;
begin
   {$IfDef RecordKML} WriteLineToDebugFile('TGISdataBaseModule.ExportToKML in, MDDef.KMLImageOpts=' + IntToStr(MDDef.KMLImageOpts)); {$EndIf}
   SaveBackupDefaults;
   MDDef.AskAboutKMLExport := Ask;
   MDDef.KMLDefaultDB := Default;
   Result := KML_opts.ConvertToKML(DBNumber,'',Nil,false,SepExpField);
   dbIsUnsaved := false;
   RestoreBackupDefaults;
   ShowStatus;
   {$IfDef RecordKML} WriteLineToDebugFile('TGISdataBaseModule.ExportToKML out'); {$EndIf}
end;


procedure TGISdataBaseModule.DeleteAllSelectedRecords(ImSure : boolean = false);
var
   rc,i,nc : integer;
begin
   MyData.ProgressVars(rc,nc);
   if (rc > 0) then begin
      if ImSure or ((not MDdef.ConfirmDBEdits) or AnswerIsYes('Confirm delete ' + IntToStr(rc) + ' records in ' + DBName)) then begin
         EmpSource.Enabled := false;
         i := 0;
         MyData.First;
         StartProgress('Mark deletions');;
         while not MyData.eof do begin
            MyData.Edit;
            MyData.MarkRecordForDeletion;
            MyData.Next;
            inc(i);
            if (rc mod nc = 0) then begin
               UpdateProgressBar(i/rc);
            end;
         end;
         EndProgress;
         PurgeDeletedRecords;
         if PointShapeFile(ShapeFileType) then SavePointShapeFile;
      end
      else begin
         ClearGISFilter;
      end;
   end
   else begin
      ClearGISFilter;
   end;
end;


procedure TGISdataBaseModule.Recentermaponrecord;
var
   xcent,ycent : float64;
begin
   if ItsAPointDB then begin
      yCent := MyData.GetFieldByNameAsFloat(LatFieldName);
      xCent := MyData.GetFieldByNameAsFloat(LongFieldName);
      theMapOwner.CenterMapOnLatLong(ycent,xcent,hzFullZoom)
   end
   else begin
      aShapeFile.AreaAndCentroid(theMapOwner.MapDraw.PrimMapProj,MyData.RecNo,YCent,XCent);
      theMapOwner.CenterMapOnLatLong(ycent,xcent,hzNoZoom)
   end;
end;


procedure TGISdataBaseModule.DistanceToLinePolygon(IncludeLocationOnLine : boolean);
var
   LatOnLine,LongOnLine,Lat,Long,Lat2,Long2,Dist,MinDist : float64;
   DistDB,i,rc : integer;
begin
   {$IfDef RecordDistance} WriteLineToDebugFile('Tdbtablef.Distancetolinepolygon1Click in ' + IntToStr(GISdb[DBonTable].MyData.RecordCount)); {$EndIf}
   if LineOrAreaShapeFile(ShapeFileType) and (not CentroidPresent) then begin
      AddGeometry(agCentroid);
   end;
   DistDB := PickOpenGISDataBase('Line/Polygon for distances');
   if (DistDB = 0) then exit;
   
   AddFieldToDataBase(ftFloat,'DIST_LINE',10,4);
   if IncludeLocationOnLine then begin
      AddFieldToDataBase(ftFloat,'LAT_2_LINE',12,7);
      AddFieldToDataBase(ftFloat,'LON_2_LINE',12,7);
   end;
   MyData.First;
   rc := MyData.RecordCount;
   i := 0;
   StartProgress('Distances');
   while not MyData.eof do begin
      if (i mod 25 = 0) then begin
         EmpSource.Enabled := false;
         GISdb[DistDB].EmpSource.Enabled := false;
         UpdateProgressBar(i/rc);
      end;
      inc(i);
      GetLatLongToRepresentRecord(Lat,Long);
      GISdb[DistDB].MyData.First;
      MinDist := 99e39;
      while not GISdb[DistDB].MyData.eof do begin
         Dist := GISdb[DistDB].ashapefile.DistanceToLineOrPerimeter(GISdb[DistDB].TheMapOwner.MapDraw.PrimMapProj,GISdb[DistDB].MyData.RecNo, Lat,Long,Lat2,Long2);
         if (Dist < MinDist) then begin
            MinDist := Dist;
            LatOnLine := Lat2;
            LongOnLine := Long2;
         end;
         GISdb[DistDB].MyData.Next;
      end;
      MyData.Edit;
      MyData.SetFieldByNameAsFloat('DIST_LINE',MinDist);
      if IncludeLocationOnLine then begin
         MyData.SetFieldByNameAsFloat('LAT_2_LINE',LatOnLine);
         MyData.SetFieldByNameAsFloat('LON_2_LINE',LongOnLine);
      end;
      MyData.Next;
    end;
    ShowStatus;
   {$IfDef RecordDistance} WriteLineToDebugFile('Tdbtablef.Distancetolinepolygon1Click in'); {$EndIf}
end;


procedure TGISdataBaseModule.ZoommaptorecordWithBufferMeters(Buffer : float64);
var
   LatHi,LatLow,LongHi,LongLow : float64;
begin
   if ItsAPointDB then begin
      Recentermaponrecord;
   end
   else begin
      LatHi := MyData.GetFieldByNameAsFloat('LAT_HI');
      LatLow := MyData.GetFieldByNameAsFloat('LAT_LOW');
      LongHi := MyData.GetFieldByNameAsFloat('LONG_HI');
      LongLow := MyData.GetFieldByNameAsFloat('LONG_LOW');
      {$IfDef RecordZoomMap} WriteLineToDebugFile('ZoommaptorecordWithBufferMeters, BBox SW=' + LatLongDegreeToString(LatLow,LongLow) + 'NE=' + LatLongDegreeToString(LatHi,LongHi) ); {$EndIf}
      VincentyPointAtDistanceBearing(LatLow,LongLow,Buffer,225,LatLow,LongLow);
      {$IfDef RecordZoomMap} WriteLineToDebugFile('ZoommaptorecordWithBufferMeters, Buffer SW=' + LatLongDegreeToString(LatLow,LongLow) + 'NE=' + LatLongDegreeToString(LatHi,LongHi) ); {$EndIf}
      TheMapOwner.MapDraw.MaximizeLatLongMapCoverage(LatLow,LongLow,LatHi,LongHi);
      theMapOwner.DoCompleteMapRedraw;
   end;
end;


procedure TGISdataBaseModule.DistAllRecsToPoint(Lat,Long : float64; Algs : integer; BaseName : shortstring = ''; Units : integer = 1);
{$IfDef VCL}
var
   Lat2,Long2,Distance,Bearing : float64;
   i,Npts : integer;
   f1,f2,f3 : shortstring;
   OK : boolean;
begin
    f1 := '';
    f2 := '';
    f3 := '';
    if (BaseName = '') then begin
       f1 := 'DIST_KM';
       f2 := 'HAV_DIS_KM';
       f3 := 'COS_DIS_KM';
    end
    else begin
       if Units = 2 then begin
          f1 := BaseName + '_KM';
          f2 := BaseName + '_NM';
          f3 := BaseName + '_AZ';
       end
       else begin
          f1 := BaseName + '_KM';
          f2 := BaseName + '_COSD';
          f3 := BaseName + '_HAVD';
       end;
    end;

    AddFieldToDataBase(ftFloat,f1,12,4);
    if (f2 <> '') then AddFieldToDataBase(ftFloat,f2,12,4);
    if (f3 <> '') then AddFieldToDataBase(ftFloat,f3,12,4);

    EmpSource.Enabled := false;
    MyData.First;
    i := 0;
    Npts := MyData.FiltRecsInDB;
    StartProgress('Distances to point');
    while not MyData.Eof do begin
       if (I mod 100 = 0) then UpdateProgressBar(i/Npts);
       inc(i);
       if ItsAPointDB then begin
          OK := ValidLatLongFromTable(Lat2,Long2);
       end
       else begin
          OK := true;
          {$IfDef RecordDistance} WriteLineToDebugFile(IntToStr(MyData.RecNo) + '/' + IntToStr(MyData.RecordCount)); {$EndIf}
          aShapeFile.AreaAndCentroid(theMapOwner.MapDraw.PrimMapProj,MyData.RecNo,Lat2,Long2);
       end;
       if OK then begin
          VincentyCalculateDistanceBearing(Lat,Long,Lat2,Long2,Distance,Bearing);
          MyData.Edit;
          MyData.SetFieldByNameAsFloat(f1,0.001 * Distance);
          if (Units = 2) then begin
             MyData.SetFieldByNameAsFloat(f2,0.001 * Distance * KM2NauticalMiles);
             MyData.SetFieldByNameAsFloat(f3,Bearing);
          end
          else if (Algs >= 2) then begin
             Distance := DistanceInKMLawOfCosines(Lat,Long,Lat2, Long2);
             MyData.SetFieldByNameAsFloat(f2,Distance);
             if (Algs = 3) then begin
                Distance := DistanceInKMHaversine(Lat,Long,Lat2, Long2);
                MyData.SetFieldByNameAsFloat(f3,Distance);
             end;
          end;
          {$IfDef RecordDistance} WriteLineToDebugFile(LatLongDegreeToString(Lat,Long) + '  ' + LatLongDegreeToString(Lat2,Long2) + RealToString(0.001 * Distance,12,2)); {$EndIf}
       end;
       MyData.Next;
    end;
    ShowStatus;
{$Else}
begin
{$EndIf}
end;


procedure TGISdataBaseModule.ShowStatus;
begin
   {$IfDef VCL}
      if (DBTablef <> Nil) then DBTablef.ShowStatus;
   {$EndIf}
   EmpSource.Enabled := true;
   //ShowDefaultCursor;
   EndProgress;
end;

function TGISdataBaseModule.DBhasMapOrGraph : boolean;
begin
   {$IfDef VCL}
      {$IfDef NoDBGrafs}
         Result := (theMapOwner <> Nil);
      {$Else}
         Result := (theMapOwner <> Nil) or (theGraphOwner <> Nil);
      {$EndIf}
   {$Else}
      Result := false;
   {$EndIf}
end;


procedure TGISdataBaseModule.ToggleLayer(LayerOn : boolean);
var
   i : integer;
begin
   {$IfDef VCL}
      LayerIsOn := LayerOn;
      if (theMapOwner <> Nil) then begin
         EmpSource.Enabled := false;
         if MDdef.DBsOnAllMaps then begin
            for i := 0 to pred(WMDEM.MDIChildCount) do begin
               if WMDEM.MDIChildren[i] is tMapForm then begin
                  AssociateMap(WMDEM.MDIChildren[i] as tMapForm);
                  theMapOwner.DoFastMapRedraw;
               end;
            end;
         end
         else begin
            theMapOwner.DoFastMapRedraw;
         end;
      end;
   {$EndIf}
   ShowStatus;
end;


{$IfDef VCL}

   function TGISdataBaseModule.PickField(Mess: ShortString; TypesAllowed : tSetFieldType; AllFields : boolean = false) : ShortString;
   var
     NumFields2,FieldsInDB : tStringList;
     WantField,i  : integer;
   begin
      GetFields(MyData,DbOpts.VisCols,TypesAllowed,FieldsInDB,AllFields);
      if (LinkTable <> Nil) then begin
         PetdbUtils.GetFields(LinkTable,AllVis,NumericFieldTypes,NumFields2);
         for I := 0 to pred(NumFields2.Count) do FieldsInDB.Add('LINK-' + NumFields2.Strings[i]);
         NumFields2.Free;
      end;
      if (FieldsInDB.Count = 0) then Result := ''
      else begin
         WantField := 0;
         if MultiSelectSingleColumnStringList('Database Field for ' + Mess,WantField,FieldsInDB,true) then begin
            Result := FieldsInDB.Strings[WantField];
         end
         else Result := '';
      end;
      FieldsInDB.Free;
   end;


      function TGISdataBaseModule.GetMultipleNumericFields(WhatFor : shortstring) : tStringList;
      var
         PickedNum : integer;
      begin
         GetFields(MyData,dbOpts.VisCols,NumericFieldTypes,Result);
         EmpSource.Enabled := false;
         PickedNum := 1;
         if not GetMultipleFromList(WhatFor,PickedNum,Result,True) then Result.Clear;
      end;


      procedure TGISdataBaseModule.MakeNewMonthlyFilterAndRedraw(Month : integer);
      var
         NewTimeFilter : shortstring;
      begin
         {$If Defined(RecordSymProblems) or Defined(RecordMonthlyFilter)} WriteLineToDebugFile('MakeNewMonthlyFilterAndRedraw'); {$EndIf}
         if MyData.FieldExists('JAN_U_MS') then begin
            dbOpts.MagField := UpperCase(MonthName[Month]) + '_U_MS';
            dbOpts.DirField := UpperCase(MonthName[Month]) + '_V_MS';
         end
         else begin
            if MyData.FieldExists(MonthFieldName) then NewTimeFilter := MonthFieldName + '=' + IntToStr(Month);
            if (NewTimeFilter = dbOpts.TimeFilter) then begin
               {$If Defined(RecordSymProblems) or Defined(RecordMonthlyFilter)} WriteLineToDebugFile('Old time filter still good, ' + NewTimeFilter); {$EndIf}
               exit;
            end;
         end;
         dbOpts.TimeFilter := NewTimeFilter;
         {$If Defined(RecordSymProblems) or Defined(RecordMonthlyFilter)} WriteLineToDebugFile('dbOpts.TimeFilter=' + dbOpts.TimeFilter); {$EndIf}
         AssembleGISFilter;
         RedrawLayerOnMap;
      end;
{$EndIf}


{$IfDef VCL}

    function TGISdataBaseModule.GetFieldPercentile(WantedField : ShortString; PC : float64) : float64;
    var
       Missing,NPts,i  : int64;
       Min,Max : float64;
       zs : ^bfarray32;
    begin
       New(zs);
       GetFieldValuesInArrayLinkPossible(WantedField,zs^,Npts,Missing,Min,Max);
       Petmath.HeapSort(npts,zs^);
       i := round(0.01 * PC * NPts);
       Result := zs^[i];
       Dispose(zs);
    end;


procedure TGISdataBaseModule.DefineDBColorTable;
var
   NPts,i,Missing : int64;
   Min,Max : float64;
   zs : ^bfarray32;
begin
   {$If Defined(RecordDefineDBColorTable) or Defined(RecordDBNumericPlot)} WriteLineToDebugFile('TGISdataBaseModule.DefineDBColorTable in, dbAutoshow=' + IntToStr(dbasColorByString) + '  colorfield=' + dbOpts.FloatColorField); {$EndIf}
   EmpSource.Enabled := false;
   if (dbOpts.dbAutoShow in [dbasColorByNumeric]) then begin
      if (dbOpts.DBColorScheme = LegChloropleth) and (dbOpts.dbColorMode = dbcmFieldQuantile) then begin
         {$IfDef RecordQuantile} WriteLineToDebugFile('start quartile'); {$EndIf}
         New(zs);
         GetFieldValuesInArrayLinkPossible(dbOpts.FloatColorField,zs^,Npts,Missing,Min,Max);
         {$IfDef RecordQuantile} WriteLineToDebugFile('got values'); {$EndIf}
         PetMath.HeapSort(Npts,zs^);
         {$IfDef RecordQuantile} WriteLineToDebugFile('heap sorted'); {$EndIf}
         for i := 1 to ColorDefTable.ZTableEntries do begin
            ColorDefTable.ZTableValue[i] := zs^[(i * Npts div succ(ColorDefTable.ZTableEntries))];
            {$IfDef RecordQuantile} WriteLineToDebugFile('Quantile: ' + IntToStr(i) + RealToString(ColorDefTable.ZTableValue[i],8,2) + '  ' + IntToStr(i * Npts div succ(ColorDefTable.ZTableEntries))); {$EndIf}
         end;
         Dispose(zs);
         {$IfDef RecordQuantile} WriteLineToDebugFile('end quartile'); {$EndIf}
      end
      else begin
         {$If Defined(RecordDefineDBColorTable) or Defined(RecordDBNumericPlot)} WriteLineToDebugFile('start color ramp'); {$EndIf}
         DefineColorTableValues(dbOpts.DBColorPaletteName,dbOpts.ColorMin,dbOpts.ColorMax,ColorDefTable,dbOpts.ReverseColorTable);
         {$If Defined(RecordDefineDBColorTable) or Defined(RecordDBNumericPlot)} WriteLineToDebugFile('end color ramp'); {$EndIf}
      end;
   end;
   EmpSource.Enabled := true;
   {$If Defined(RecordDefineDBColorTable) or Defined(RecordDBNumericPlot)} WriteLineToDebugFile('TGISdataBaseModule.DefineDBColorTable out'); {$EndIf}
end;


function TGISdataBaseModule.GetElevationField(CheckOverWrite : boolean = true) : shortstring;
begin
   Result := 'ELEV';
   if not MyData.FieldExists(Result) then Result := 'Z';
   if MyData.FieldExists(Result) then begin
      If CheckOverwrite and (not AnswerIsYes('Field ' + Result + ' exists; Overwrite')) then begin
        Petmar.GetString('Elev field name',Result,true,DBaseFieldNameChars);
      end;
   end
   else Petmar.GetString('Elev field name',Result,true,DBaseFieldNameChars);;
end;


         procedure TGISdataBaseModule.AssociateMap(MapForm : tMapForm);
         begin
            if (MapForm <> nil) and ValidDB(DBNumber) and (MapForm.MapDraw <> nil) and (not MapForm.MapDraw.ClosingMapNow) then begin
               TheMapOwner := MapForm;
               TheMapOwner.MapDraw := MapForm.MapDraw;
               TheMapOwner.MapDraw.DBonThisMap[DBNumber] := true;
            end;
         end;


         function TGISdataBaseModule.CanPlot : boolean;
         begin
            Result := (TheMapOwner <> Nil) and (LatLongFieldsPresent or LatLongCornersPresent or ItsAShapeFile or XYZFile or ItsBoxOutline);
         end;


         procedure TGISdataBaseModule.BitmapForDatabase(var sBitmap : tMyBitmap; Height : integer);
         //small bitmap for icons and buttons
         var
            fName : PathStr;
         begin
            if (LayerIsOn) then begin
               if CanPlot and (SimplePointFile or XYZFile) then begin
                  if (dbOpts.DBAutoShow in [dbasConnectTwoPointsInRec,dbasConnectSeqPts]) then begin
                      ColorLineWidthBitmap(sBitmap,dbOpts.LineColor,dbOpts.LineWidth,false);
                  end
                  else if (dbOpts.DBAutoShow = dbasTTFontSymbol) then begin
                     if (dbOpts.TTSymbolFontName <> '') then begin
                        FreeAndNil(sBitmap);
                        CreateBitmap(Sbitmap,500,500);
                        sBitmap.Canvas.Font.Name := dbOpts.TTSymbolFontName;
                        sBitmap.Canvas.Brush.Style := bsClear;
                        sBitmap.Canvas.Font.Color := ConverTPlatFormColorToTColor(dbOpts.TTSymbolFontColor);
                        sBitmap.Canvas.Font.Size := dbOpts.TTSymbolFontSize;
                        sBitmap.Canvas.TextOut(250,250, dbOpts.TTSymbolFontChar);
                        GetImagePartOfBitmap(sBitmap);
                        MakeBitmapThumbnail(sBitmap,Height -4);
                     end;
                  end
                  else if (dbOpts.DBAutoShow = dbasIconAll) or (dbOpts.DBAutoShow = dbasIconField) then begin
                     if (dbOpts.DBAutoShow = dbasIconAll) then fName := dbOpts.AllIconFName
                     else fName := MyData.GetFieldByNameAsString(dbOpts.IconField);
                     if ExpandIconFileName(fName) then begin
                        FreeAndNil(sBitmap);
                        sBitmap := LoadBitmapFromFile(fName);
                        GetImagePartOfBitmap(sBitmap);
                        MakeBitmapThumbnail(sBitmap,Height -4);
                     end;
                  end
                  else if (dbOpts.DBAutoShow = dbasVector) then begin
                     PetImage.DrawLine(sBitmap,1,Height div 2, sBitmap.Width-1, Height Div 2);
                     PetImage.DrawLine(sBitmap,sBitmap.Width-1, Height Div 2,sBitmap.Width-8,Height div 2 - 4);
                     PetImage.DrawLine(sBitmap,sBitmap.Width-1, Height Div 2,sBitmap.Width-8,Height div 2 + 4);
                  end
                  else if (dbOpts.DBAutoShow = dbasConnectTwoPointsInRec) then begin
                     ColorLineWidthBitmap(sBitmap,dbOpts.LineColor,dbOpts.LineWidth,false);
                  end
                  else begin
                    BitmapSymbol(sBitmap,sBitmap.Width div 2, sBitmap.Height div 2,dbOpts.Symbol);
                  end;
               end
               else if CanPlot and LineShapeFile(ShapeFileType) then begin
                  ColorLineWidthBitmap(sBitmap,dbOpts.LineColor,dbOpts.LineWidth,false);
               end
               else if CanPlot and (AreaShapeFile(ShapeFileType) or ((Not ItsAShapeFile) and (LatLongCornersPresent))) then begin
                  if (dbOpts.DBAutoShow in [dbasColorByString,dbasColorByNumeric]) then begin
                     FreeAndNil(sBitmap);
                     DefineColorTable;
                     sBitmap := MakeColorScaleBitmap(30,14,dbOpts.DBColorScheme,ColorDefTable);
                  end
                  else DrawPatternOnBitmap(sBitmap, dbOpts.FillColor,dbOpts.LineColor,dbOpts.AreaSymbolFill,dbOpts.LineWidth);
               end;
            end;
         end;


         procedure TGISdataBaseModule.ColorButtonForSymbol(BitBtn1 : tBitBtn; Capt : shortString = '');
         var
            sBitmap : tMyBitmap;
         begin
            if (BitBtn1 = Nil) then exit;
            CreateBitmap(Sbitmap,25,20);
            BitmapForDatabase(sBitmap,20);
            if CanPlot and (SimplePointFile or XYZFile) then begin
               if (dbOpts.DBAutoShow = dbasTTFontSymbol) then BitBtn1.Caption := 'TT sym'
               else if (dbOpts.DBAutoShow = dbasIconAll) then BitBtn1.Caption := 'Icon'
               else if (dbOpts.DBAutoShow = dbasIconField) then begin
                  BitBtn1.Glyph := Nil;
                  BitBtn1.Caption := 'Icon';
               end
               else if (dbOpts.DBAutoShow = dbasConnectTwoPointsInRec) then  BitBtn1.Caption := 'Line'
               else BitBtn1.Caption := 'Symbol';
            end
            else if CanPlot and LineShapeFile(ShapeFileType) then BitBtn1.Caption := 'Line'
            else if CanPlot and (AreaShapeFile(ShapeFileType) or ((Not ItsAShapeFile) and (LatLongCornersPresent))) then BitBtn1.Caption := 'Area';
            BitBtn1.Glyph := sBitmap;
            sBitmap.Free;
            if (Capt <> '') then BitBtn1.Caption := Capt;
         end;


         procedure TGISdataBaseModule.PrepColors(Bitmap : tMyBitmap);
         var
            ThisPattern : Zipatone.tPatternRecord;
            x,y : integer;
         begin
            if LineShapeFile(ShapeFileType) then begin
               Bitmap.Canvas.Pen.Color := ConvertPlatformColorToTColor(dbOpts.LineColor);
               Bitmap.Canvas.Pen.Width := dbOpts.LineWidth;
               {$IfDef RecordLineWidth} WriteLineToDebugFile('TGISDataBase.PrepColors line width=' + IntToStr(dbOpts.LineSize)); {$EndIf}
            end
            else if AreaShapeFile(ShapeFileType) or LatLongCornersPresent then begin
               if (ZipPatName = '') then begin
                  Bitmap.Canvas.Pen.Color := ConvertPlatformColorToTColor(dbOpts.LineColor);
                  Bitmap.Canvas.Pen.Width := dbOpts.LineWidth;
                  Bitmap.Canvas.Brush.Color := ConvertPlatformColorToTColor(dbOpts.FillColor);
                  Bitmap.Canvas.Brush.Style := dbOpts.AreaSymbolFill;
               end
               else begin
                  ThisPattern := ZipATone.PatternFromString(ZipPatName);
                  Bitmap.Canvas.Brush.Bitmap := tMyBitmap.Create;  //have to go this way
                  Bitmap.Canvas.Brush.Bitmap.PixelFormat := pfDevice;
                  Bitmap.Canvas.Brush.Bitmap.Height := ThisPattern.NumCols;
                  Bitmap.Canvas.Brush.Bitmap.Width := ThisPattern.NumRows;
                  for y := 0 to pred(ThisPattern.NumRows) do
                     for x := 0 to pred(ThisPattern.NumCols) do
                        if (ThisPattern.PatternMasks[y div 8,x] and MaskBit[y mod 8]) > 0 then
                           Bitmap.Canvas.Brush.Bitmap.Canvas.Pixels[x,y] := ThisPattern.WinColor;
               end;
            end
            else if SimplePointFile then begin
               Bitmap.Canvas.Pen.Color := ConvertPlatformColorToTColor(dbOpts.LineColor);
            end;
         end;


         procedure TGISdataBaseModule.CloseGisScaledForm;
         begin
            if (gis_scaled_form <> Nil) then begin
               gis_scaled_form.Close;
               gis_scaled_form := Nil;
            end;
         end;


         function TGISdataBaseModule.RGBColorFromThreeNumericFields: tColor;
         var
            Color1Val,Color2Val,Color3Val : float32;
         begin
            Color1Val := 0;
            Color2Val := 0;
            Color3Val := 0;
            if (dbOpts.RedField <> '') then GetFloat32FromTableLinkPossible(dbOpts.RedField,Color1Val);
            if (dbOpts.GreenField <> '')then GetFloat32FromTableLinkPossible(dbOpts.GreenField,Color2Val);
            if (dbOpts.BlueField = '') then Result := rgb_colors_three_params.tColorFromRedGreen(Color1Val,Color2Val)
            else begin
               GetFloat32FromTableLinkPossible(dbOpts.BlueField,Color3Val);
               Result := rgb_colors_three_params.tColorFromRedGreenBlue(Color1Val,Color2Val,Color3Val);
            end;
         end;

         procedure TGISdataBaseModule.FillComboBoxFromSingleField(var ComboBox : tComboBox; Field : shortstring);
         var
            FieldValues : tStringList;
            i : integer;
            aft : tFieldType;
         begin
            ComboBox.Items.Clear;
            if (Field <> '') then  begin
               aft := MyData.GetFieldType(Field);
               if (Field = MonthFieldName) then begin
                  if aft in [ftInteger,ftSmallInt,ftFloat] then begin
                     for I := 1 to 12 do ComboBox.Items.Add(IntToStr(i));
                  end
                  else begin
                     for I := 1 to 12 do ComboBox.Items.Add(MonthName[i]);
                  end;
               end
               else begin
                  EmpSource.Enabled := false;
                  FieldValues := MyData.ListUniqueEntriesInDB(Field);
                  if aft in [ftInteger,ftSmallInt,ftFloat] then SortStringListNumerically(FieldValues);
                  EmpSource.Enabled := true;
                  ComboBox.Sorted := false;
                  for I := 0 to pred(FieldValues.Count) do ComboBox.Items.Add(FieldValues.Strings[i]);
                  FieldValues.Free;
               end;
            end;
         end;


         procedure TGISdataBaseModule.LinkSecondaryTable(var FileWanted : PathStr);
         begin
            {$IfDef RecordLinkTable} WriteLineToDebugFile('TGISDataBase.LinkSecondaryTable in, ' + FileWanted); {$EndIf}
            ClearLinkTable(false);

             if (FileWanted <> '') and (not FileExists(FileWanted)) then begin
                FileWanted := ExtractFilePath(FullDBName) + ExtractFileName(FileWanted);
             end;

             if (FileWanted = '') or (not FileExists(FileWanted)) then begin
               {$IfDef RecordLinkTable} WriteLineToDebugFile('TGISDataBase.LinkSecondaryTable call DB_Join.SetUpJoin'); {$EndIf}
                DB_Join.SetUpJoin(Self);
             end;

             if (dbOpts.LinkFieldThisDB = '') or (dbOpts.LinkFieldOtherDB = '') then begin
               {$IfDef RecordLinkTable} WriteLineToDebugFile('TGISDataBase.LinkSecondaryTable no valid link'); {$EndIf}
               ClearLinkTable(true);
             end
             else begin
                dbOpts.LinkTableName := FileWanted;
                //if MDDef.AllowMemoryLinkDB then DesiredDBMode := dbmCDS;
                LinkTable := tMyData.Create(dbOpts.LinkTableName);
                LinkTable.AssignEmpSource(LinkSource1);
                DesiredDBMode := dbmyDefault;
                {$IfDef RecordLinkTable}
                   WriteLineToDebugFile('LinkSecondaryTable Picked: ' + FileWanted);
                   WriteLineToDebugFile('LinkSecondaryTable Linking fields: ' + dbOpts.LinkFieldThisDB + ' and ' + dbOpts.LinkFieldOtherDB);
                {$EndIf}
                SetUpRangeTable(dbOpts.LinkTableName,LinkRangeTable);
             end;
         end;


         procedure TGISdataBaseModule.SubsetShapeFile(var fName : PathStr; ThinFactor : integer = 1; BatchRun : boolean = false);
         var
            Count : int64;
            ShapeFileCreator : tShapeFileCreation;
         begin
            EmpSource.Enabled := true;   //needed to get columns.visible
            SaveCurrentDBaseSubset(fName,ThinFactor,BatchRun);
            ShapeFileCreator := tShapeFileCreation.Create(WGS84DatumConstants,fName,false,ShapeFileType);
            MyData.First;
            Count := 0;
            StartProgress('Subset SHP');
            EmpSource.Enabled := false;
            repeat
               inc(Count);
               if (Count mod 1000 = 0) then begin
                  UpdateProgressBar(Count / MyData.RecordCount);
               end;
               aShapeFile.GetLineCoords(MyData.RecNo,true);
               ShapeFileCreator.PtsInShapeStream := aShapeFile.CurrentPolyLineHeader.NumPoints;
               ShapeFileCreator.NParts := aShapeFile.CurrentPolyLineHeader.NumParts;
               ShapeFileCreator.ShapeStreamCoords^ := aShapeFile.CurrentLineCoords^;
               ShapeFileCreator.PartSize :=  aShapeFile.CurrentLinePartSize;
               if ShapeFile3D(ShapeFileType) then ShapeFileCreator.ShapeStreamZs^ := aShapeFile.CurrentLineZs^;
               {$IfDef RecordDataBaseSaveFiles}
                  WriteLineToDebugFile(IntToStr(Count) +  '  DB record ' + IntToStr(MyData.RecNo),true);
                  WriteLineToDebugFile('Parts=' + IntToStr(ShapeFileCreator.NParts) + '  Pts=' + IntToStr(ShapeFileCreator.PtsInShapeStream));
                  for i := 0 to pred(ShapeFileCreator.PtsInShapeStream) do WriteLineToDebugFile(IntegerToString(i,6) + RealToString(ShapeFileCreator.ShapeStreamCoords^[i].Lat,12,7) + RealToString(ShapeFileCreator.ShapeStreamCoords^[i].Long,14,7));
                  WriteLineToDebugFile('----------');
               {$EndIf}
               ShapeFileCreator.ProcessRecordForShapeFile;
               MyData.Next;
            until MyData.EOF;
            EndProgress;
            ShapeFileCreator.CloseShapeFiles;
            AddProjectionFile(fName);
         end;


         function TGISdataBaseModule.GetAnalysisFields : tStringlist;
         {$IfDef NoClustering}
         begin
         {$Else}
         var
            j : integer;
            TStr : shortstring;
         begin
            Result := tStringList.Create;
            repeat
               {$IfDef RecordClustering} WriteLineToDebugFile('ClusTGISdataBaseModule.GetAnalysisFields for clustering'); {$EndIf}
               for j := 0 to pred(dbTablef.DBGrid1.Columns.Count) do  begin
                  TStr := dbTablef.DBGrid1.Columns[j].FieldName;
                  if NoStatsField(TStr) or (not(MyData.GetFieldType(TStr) in NumericFieldTypes)) then begin
                     dbOpts.VisCols[j] := false;
                     dbTablef.DBGrid1.Columns[j].Visible := false;
                  end;
               end;
               ManageDBDisplay(dbTablef.DBGrid1,dbTablef.DBonTable);
               Result.Clear;
               for j := 0 to pred(dbTablef.DBGrid1.Columns.Count) do if dbOpts.VisCols[j] then Result.Add(dbTablef.DBGrid1.Columns[j].FieldName);
            until (Result.Count > 0) and (Result.Count <= EdburgMaxVariables);
         {$EndIf}
         end;


         procedure TGISdataBaseModule.ShowOtherDBsWithSameFilter(ShowFilter,ShowN,ShowThisDB : boolean);
         var
            i : integer;
         begin
            {$IfDef ShowOtherDBsWithSameFilter} WriteLineToDebugFile('TGISDataBase.ShowOtherDBsWithSameFilter in'); {$EndIf}
            for I := 1 to MaxDataBase do if (GISdb[i] <> Nil) and (GISdb[i].LayerIsOn) then begin
                {$IfDef ShowOtherDBsWithSameFilter} WriteLineToDebugFile(GISDataBase[i].dbName); {$EndIf}
               if (DBPlotNow [i]) and (ShowThisDB or ((i <> DBNumber) and MDDef.ApplySameFilterAllDBs)) then begin
                  GISdb[i].MyData.ApplyFilter(MyData.Filter);
                  GISdb[i].dbTableF.ShowFilteredDB(ShowFilter,ShowN);
                  {$IfDef ShowOtherDBsWithSameFilter} WriteLineToDebugFile(GISDataBase[i].dbName + '  filter =' + GISDataBase[i].MyData.Filter ); {$EndIf}
               end;
            end;
         end;


         procedure TGISdataBaseModule.StartVATEdits;
         begin
            dbTablef.BitBtn9.Visible := true;
            dbTablef.BitBtn23.Visible := true;
            dbTablef.CheckBox1.Checked := true;
            dbTablef.VATEdit := true;
            dbTablef.Editrecordsingrid1Click(Nil);
         end;


         procedure TGISdataBaseModule.GetFieldValuesInArrayLinkPossible(FieldDesired : ShortString; var zs : bfarray32; var Npts,Missing : int64; var Min,Max : float64);
         var
            z : float64;
            TStr : string;
            Skip,rc,i : integer;
            Linking : boolean;
         begin
            Linking := LinkedField(FieldDesired);
            if (not Linking) and (not MyData.FieldExists(FieldDesired)) then begin
               NPts := 0;
               exit;
            end;
            NPts := 0;
            MyData.First;
            Missing := 0;
            Min := 99e39;
            Max := -99e39;
            Skip := 1;
            rc := MyData.RecordCount;
            while (rc div Skip > bfArrayMaxSize) do inc(Skip);
            StartProgress('Get values ' + FieldDesired);

            repeat
               if (NPts mod 1000 = 0) then begin
                  UpdateProgressBar(Npts/rc);
                  EmpSource.Enabled := false;
               end;

               if Linking then begin
                  if FindValidJoin(MyData.GetFieldByNameAsString(dbOpts.LinkFieldThisDB)) then TStr := LinkTable.GetFieldByNameAsString(FieldDesired)
                  else TStr := '';
               end
               else TStr := MyData.GetFieldByNameAsString(FieldDesired);
               TStr := ptTrim(TStr);
               if (TStr <> '') then begin
                  z := StrToFloat(TStr);
                  Petmath.CompareValueToExtremes(z,Min,Max);
                  zs[Npts] := z;
                  inc(NPts);
               end
               else inc(Missing);
               for i := 1 to Skip do MyData.Next;
            until MyData.eof;
            ShowStatus;
         end;


         function TGISdataBaseModule.GetFieldStatistics(WantedField : ShortString) : tMomentVar;
         var
             zs : ^bfarray32;
          begin
            New(zs);
            GetFieldValuesInArrayLinkPossible(WantedField,zs^,Result.Npts,Result.Missing,Result.MinZ,Result.MaxZ);
            if (Result.Npts > 0) then begin
               moment(zs^,Result,msAll);
            end
            else InitializeMomentVar(Result);
            Dispose(zs);
            EndProgress;
         end;


         function TGISdataBaseModule.GetFieldPercentiles(WantedField : ShortString) : floatarray1000;
         var
            zvs : ^bfarray32;
            NPts,i,Missing : int64;
            Minz,MaxZ : float64;
         begin
            New(zvs);
            GetFieldValuesInArrayLinkPossible(WantedField,zvs^,Npts,Missing,MinZ,MaxZ);
            Petmath.HeapSort(NPts,zvs^);
            Result[0] := MinZ;
            Result[1000] := MaxZ;
            for I := 1 to 999 do begin
               Result[i] := zvs^[round(0.001 * Npts * i)];
            end;
            Dispose(zvs);
         end;


function TGISdataBaseModule.FieldRMSE(fName : shortString) : float64;
var
   n : integer;
   z,Sum : float64;
begin
   EmpSource.Enabled := false;
   MyData.First;
   StartProgress('RMSE');
   Sum := 0;
   n := 0;
   while not MyData.eof do begin
      if MyData.CarefullyGetFieldByNameAsFloat64(fName,z) then begin
         inc(n);
         Sum := Sum + sqr(z);
      end;
      MyData.Next;
   end;
   if (n > 0) then Result := sqrt(Sum/n)
   else Result := -9999;
   ShowStatus;
end;


         procedure TGISdataBaseModule.DisplayFieldStatistics(WantedField : ShortString);
         var
            Col,i  : integer;

            GridForm : demstringgrid.tGridForm;
            TheFieldsInDB : tStringList;

                  procedure FieldResults(FieldDesired : string35);
                  var
                     MomentVar : tMomentVar;
                  begin
                     {$IfDef RecordFieldStatistics} WriteLineToDebugFile('Field results, ' + FieldDesired); {$EndIf}
                     MomentVar := GetFieldStatistics(FieldDesired);
                     Col := GridForm.StringGrid1.ColCount;
                     GridForm.StringGrid1.Cells[Col,0] := FieldDesired;
                     if (MomentVar.Npts > 1) then begin
                        GridForm.StringGrid1.Cells[Col,1] := RealToString(MomentVar.mean,-18,-4);
                        GridForm.StringGrid1.Cells[Col,2] := RealToString(MomentVar.avg_dev,-18,-4);
                        GridForm.StringGrid1.Cells[Col,3] := RealToString(MomentVar.std_dev,-18,-4);
                        GridForm.StringGrid1.Cells[Col,4] := RealToString(MomentVar.skew,-18,-4);
                        GridForm.StringGrid1.Cells[Col,5] := RealToString(MomentVar.curt,-18,-4);
                        GridForm.StringGrid1.Cells[Col,8] := RealToString(MomentVar.MinZ,-18,-4);
                        GridForm.StringGrid1.Cells[Col,9] := RealToString(MomentVar.MaxZ,-18,-4);
                        GridForm.StringGrid1.Cells[Col,10] := RealToString(MomentVar.Median,-18,-4);
                        GridForm.StringGrid1.Cells[Col,11] := RealToString(FieldRMSE(FieldDesired),-18,-4);
                     end;
                     GridForm.StringGrid1.Cells[Col,6] := IntToStr(MomentVar.Npts);
                     GridForm.StringGrid1.Cells[Col,7] := IntToStr(MomentVar.Missing);
                     GridForm.StringGrid1.ColCount := GridForm.StringGrid1.ColCount + 1;
                     GridForm.StringGrid1.FixedCols := 1;
                     GridForm.StringGrid1.FixedRows := 0;
                  end;

         begin
            {$IfDef RecordFieldStatistics} WriteLineToDebugFile('TGISdataBaseModule.DisplayFieldStatistics in'); {$EndIf}
            EmpSource.Enabled := false;
            GridForm := tGridForm.Create(Application);
            GridForm.Caption := dbName + ' statistics';
            GridForm.HideCorrelationControls;
            GridForm.StringGrid1.Cells[0,0] := 'Field';
            GridForm.StringGrid1.Cells[0,1] := 'Mean';
            GridForm.StringGrid1.Cells[0,2] := 'Avg Dev';
            GridForm.StringGrid1.Cells[0,3] := 'Std Dev';
            GridForm.StringGrid1.Cells[0,4] := 'Skewness';
            GridForm.StringGrid1.Cells[0,5] := 'Kurtosis';
            GridForm.StringGrid1.Cells[0,6] := 'n';
            GridForm.StringGrid1.Cells[0,7] := 'Missing';
            GridForm.StringGrid1.Cells[0,8] := 'Minimum';
            GridForm.StringGrid1.Cells[0,9] := 'Maximum';
            GridForm.StringGrid1.Cells[0,10] := 'Median';
            GridForm.StringGrid1.Cells[0,11] := 'RMSE';
            GridForm.StringGrid1.ColCount := 1;
            GridForm.StringGrid1.RowCount := 12;
            if (WantedField = 'ALL-FIELDS') or (WantedField = 'SELECT-FIELDS') then begin
               {$IfDef RecordFieldStatistics} WriteLineToDebugFile('TGISdataBaseModule.DisplayFieldStatistics all fields'); {$EndIf}
               TheFieldsInDB := nil;
               PetDBUtils.GetFields(MyData,dbOpts.VisCols,NumericFieldTypes,TheFieldsInDB);
               for i := 0 to pred(TheFieldsInDB.Count) do if (not NoStatsField(TheFieldsInDB.Strings[i])) then FieldResults(TheFieldsInDB.Strings[i]);
               TheFieldsInDB.Free;
            end
            else begin
               {$IfDef RecordFieldStatistics} WriteLineToDebugFile('TGISdataBaseModule.DisplayFieldStatistics Just 1'); {$EndIf}
               FieldResults(WantedField);
            end;
            EmpSource.Enabled := true;
         end;


         function TGISdataBaseModule.ExtractDBtoCSV(ThinFactor : integer; SepChar : ANSIchar) : tStringList;
         var
            J,k,Count : integer;
            TStr,TStr2 : ANSIString;
            CopyFields : tStringList;
         begin
            MyData.First;
            EmpSource.Enabled := true;
            CopyFields := tStringList.Create;
            for j := 0 to pred(dbTablef.DBGrid1.Columns.Count) do begin
               if (dbTablef.DBGrid1.Columns[j].Visible) then begin
                  CopyFields.Add(dbTablef.DBGrid1.Columns[j].Field.FieldName);
               end;
            end;
            EmpSource.Enabled := false;

            ApplicationProcessMessages;
            {$IfDef RecordDataBaseSaveFiles} WriteLineToDebugFile('TGISdataBaseModule.SaveCurrentDBaseSubset got fields list'); {$EndIf}

            Result := tStringList.Create;
            TStr := '';
            for k := 0 to pred(CopyFields.Count) do begin
               if (k > 0) then TStr := Tstr + SepChar;
               TStr := TStr + CopyFields.Strings[k];
            end;
            Result.Add(TStr);

           Count := 0;
           StartProgress('Extract CSV');
           MyData.First;
           repeat
               {$IfDef RecordDataBaseSaveFileFull} WriteLineToDebugFile('count=' + IntToStr(Count)); {$EndIf}
               if (Count mod 2000 = 0) then begin
                  UpdateProgressBar(Count / MyData.FiltRecsInDB);
                  EmpSource.Enabled := false;
               end;
               TStr := '';
               for k := 0 to pred(CopyFields.Count) do begin
                  if (k > 0) then TStr := Tstr + SepChar;
                  TStr2 := trim(MyData.GetFieldByNameAsString(CopyFields.Strings[k]));
                  if MDDef.CommasToSemiColons then begin
                     ReplaceCharacter(TStr2,',',';');
                  end;
                  TStr := TStr + TStr2;
               end;
               Result.Add(TStr);

               for k := 1 to ThinFactor do if (not MyData.EOF) then begin
                  inc(Count);
                  MyData.Next;
               end;
            until MyData.EOF;
            EndProgress;
            EmpSource.Enabled := true;
         end;

         procedure TGISdataBaseModule.ExtractXYZtoCSV(fName : PathStr; SepChar : ANSIchar);
         var
            Count : integer;
            sl : tStringList;
         begin
           Count := 0;
           StartProgress('Extract CSV');
           MyData.First;
           EmpSource.Enabled := false;
           sl := tStringList.Create;
           repeat
               if (Count mod 2000 = 0) then begin
                  UpdateProgressBar(Count / MyData.FiltRecsInDB);
                  EmpSource.Enabled := false;
               end;
               sl.Add(MyData.GetFieldByNameAsString('X') + SepChar + MyData.GetFieldByNameAsString('Y') + SepChar + MyData.GetFieldByNameAsString('Z')  );
               MyData.Next;
            until MyData.EOF;
            EndProgress;
            EmpSource.Enabled := true;
            sl.SaveToFile(fName);
            sl.Free;
         end;

         procedure TGISdataBaseModule.SaveCurrentDBaseSubset(fName : PathStr; ThinFactor : integer = 1; BatchRun : boolean = false; ForceAsk : boolean = false);
         var
            db : integer;
            Extract : tStringList;
         begin
            {$IfDef RecordDataBaseSaveFiles} WriteLineToDebugFile('TGISdataBaseModule.SaveCurrentDBaseSubset in, db=' + MyData.TableName); {$EndIf}
            if (MyData.FiltRecsInDB = 0) then exit;
            if (fName = '') or ForceAsk then begin
               fName := ExtractFilePath(LastDataBase);
               if not GetFileNameDefaultExt('dBase table',DBNameMask,fName) then exit;
            end;
            fName := SpacesToUnderScores(fName);
            if (ThinFactor <= 0) then ReadDefault('DB thin factor',ThinFactor);
            Extract := ExtractDBtoCSV(ThinFactor,',');
            PetDBUtils.StringList2CSVtoDB(Extract,fName,true);
            ShowStatus;
            if (not BatchRun) then begin
               if SimplePointFile and (ThinFactor = 1) and (not MyData.Filtered) and AnswerIsYes('Create shape file') then begin
                  SavePointShapeFile(true,fName);
               end;
               if MyData.Filtered and AnswerIsYes('Load new file') then begin
                  db := theMapOwner.OpenDBonMap('',fName);
                  if (GISdb[db].DBTablef <> Nil) then begin
                     GISdb[db].dbtablef.Trimblanksallstringfields1Click(Nil);
                  end;
               end;
            end;
         end;


         procedure TGISdataBaseModule.IrregularFilterDB;
         var
            ch : AnsiChar;
            xp,yp : integer;
            Bitmap : tMyBitmap;
            TStr : string;
            MaskFieldName : string10;
         begin
            {$IfDef RecordFilterDataBase} WriteLineToDebugFile('TGISDataBase.IrregularFilterDB in, recs=' + IntToStr(MyData.FiltRecsInDB)); {$EndIf}

            MaskFieldName := GetMaskFieldName;
            Bitmap := PetImage.LoadBitmapFromFile(MDTempDir + 'target-mask.bmp');
            EmpSource.Enabled := false;

            {$IfDef RecordFilterDataBase} WriteLineToDebugFile('Start through recs'); {$EndIf}
            MyData.First;
            while not MyData.eof do begin
               ch := '0';
               if ValidScreenPositionFromTable(xp,yp) and (Bitmap.Canvas.Pixels[xp,yp] = clBlack) then ch := '1';
               MyData.Edit;
               MyData.SetFieldByNameAsString(MaskFieldName, ch);
               MyData.Next;
            end;
            {$IfDef RecordFilterDataBase} WriteLineToDebugFile('Done'); {$EndIf}
            Bitmap.Free;
            if (MyData.Filter = '') then TStr := '' else TStr := MyData.Filter + ' AND ';
            dbOpts.MainFilter := TStr + '(' + MaskFieldName + ' = ' + QuotedStr('1') + ')';

            MyData.ApplyFilter(dbOpts.MainFilter);
            ShowStatus;
            {$IfDef RecordFilterDataBase} WriteLineToDebugFile('TGISDataBase.IrregularFilterDB out recs=' + IntToStr(MyData.FiltRecsInDB)); {$EndIf}
         end;


         procedure TGISdataBaseModule.SetGazTableOptions;
         var
            TStr : ShortString;
         begin
             DisplayTable;
             dbTablef.DBonTable := DBNumber;
             dbTablef.Panel3.Visible := true;
             dbTablef.BitBtn5.Visible := (theMapOwner <> Nil);
             dbTablef.BitBtn7.Visible := (theMapOwner <> Nil);
             dbTablef.Button5.Visible := (theMapOwner <> Nil);
             dbTablef.SimplePlot := true;
             dbTablef.NoStatsGraph := true;
             GazDB := true;
             if USGSGazeeteerFile(dbFullName) then TStr := 'USGS' else TStr := 'NGA';
             LastGazFile :=  dbFullName;
             dbTablef.Caption := TStr + ' gazetteer: ' + ExtractFileName(dbFullName);
         end;

         function TGISdataBaseModule.IsThisDEMIXdatabase : boolean;
         begin
             Result := (MyData.FieldExists('COP') and MyData.FieldExists('ALOS')) or
                       StrUtils.AnsiContainsText(dbName,'DEMIX') or
                       (MyData.FieldExists('U120_TILES') and MyData.FieldExists('U80_TILES')) or
                       MyData.FieldExists('DEMIX_TILE');  // or MyData.FieldExists('COP_WIN')
         end;

         procedure TGISdataBaseModule.DisplayTable(fString : AnsiString = 'NONE'; CompleteFilter : boolean = false);
         var
            FirstTime,ChangeUse : boolean;
         begin
            {$IfDef RecordDataBase} WriteLineToDebugFile('Enter display TheData'); {$EndIf}
            FirstTime := (dbTablef = Nil);
            if FirstTime then begin
               {$IfDef RecordDataBase} WriteLineToDebugFile('FirstTime'); {$EndIf}
               dbTablef := TdbTablef.Create(Application);
               if MDDef.GISSecondToolbar and NameFieldExists then dbTablef.Features.Visible := true;
               dbTablef.DBonTable := DBNumber;
               dbTablef.BaseMapBitmap := Nil;
               dbTablef.Restrictbymapscale1.Visible := ItsTigerShapeFile or ItsOSMShapeFile;
               dbTablef.Restrictbymapscale1.Checked := MDDef.UsePixelSizeRules;

               dbTablef.DEMIX1.Visible := MDDef.ShowDEMIX and IsThisDEMIXdatabase;
               dbTablef.BitBtn24.Visible := dbTablef.DEMIX1.Visible;

               if MDDef.DBMinimizeOnOpen then dbTablef.WindowState := wsMinimized;

               if MyData.FieldExists('BEAM') and MyData.FieldExists('TRACK_ID') then begin
                  dbOpts.QFField1 := 'DATE';
                  dbOpts.QFField2 := 'TRACK_ID';
                  dbOpts.QFField3 := 'BEAM';
               end;
            end;

            if (fstring = 'NONE') then fstring := ''
            else if MyData.FieldExists(fstring) then begin
               dbOpts.MainFilter := '';
               AssembleGISFilter;
               GetFilterString(dbNumber,fString,ChangeUse,true,fString);
            end
            else if (fstring <> 'NONE') and (not CompleteFilter) then begin
               {$IfDef RecordDataBase} WriteLineToDebugFile('Off to GetFilterString'); {$EndIf}
               GetFilterString(dbNumber,fString,ChangeUse);
               {$IfDef RecordDataBase} WriteLineToDebugFile('Back from GetFilterString, filter=' + fString); {$EndIf}
            end;
            if (not FirstTime) then ApplyGISFilter(fString);

            {$IfDef RecordDataBase} WriteLineToDebugFile('Filtering data base  Filter: ' + fString); {$EndIf}

            dbTablef.DBGrid1.DataSource := EmpSource;

            if FirstTime and (MyData <> Nil) then begin
               dbTablef.Panel1.Visible := ItsFanFile;
               if ItsTigerShapeFile then begin
                  dbTablef.BitBtn1.Visible := false;
               end;
            end;
            dbTablef.FormActivate(Nil);
            ShowStatus;
            {$IfDef RecordDataBase} WriteLineToDebugFile('   Filtered recs: ' + IntToStr(MyData.FiltRecsInDB)); {$EndIf}
         end;
{$EndIf}


            function TGISdataBaseModule.ValidScreenPositionFromTable(var x,y : integer) : boolean;
            var
               Lat,Long : float64;
               v : array[1..3] of float64;
            begin
               Result := false;
               if XYZfile then begin
                  {$IfDef NoDBGrafs}
                  {$Else}
                     if (TheGraphOwner <> Nil) then begin
                        if MyData.GetXYZFromTable(v[1],v[2],v[3]) then begin
                           x := TheGraphOwner.GraphDraw.GraphX(v[TheGraphOwner.GraphDraw.c1]);
                           y := TheGraphOwner.GraphDraw.GraphY(v[TheGraphOwner.GraphDraw.c2]);
                           Result := TheGraphOwner.GraphDraw.PtOnGraph(x,y);
                           exit;
                        end;
                     end;
                  {$EndIf}
               end
               else begin
                  if ValidLatLongFromTable(Lat,Long) then begin
                     if (not WGSEquivalentDatum(TheMapOwner.MapDraw.PrimMapProj.h_DatumCode)) then MolodenskiyTransformation(Lat,Long,Lat,Long,WGS84DatumConstants,TheMapOwner.MapDraw.PrimMapProj);
                     TheMapOwner.MapDraw.LatLongDegreeToScreen(Lat,Long,x,y);
                     Result := TheMapOwner.MapDraw.OnScreen(x,y);
                  end;
               end;
               {$IfDef RecordValidScreenPosition} if Result then WriteLineToDebugFile('ValidScreenPositionFromTable') else WriteLineToDebugFile('No ValidScreenPositionFromTable'); {$EndIf}
            end;


            procedure TGISdataBaseModule.SetBitmapColorsForStringFieldPlot(var Bitmap : tMyBitmap; i,Total : integer);
            var
               Color : tColor;
            begin
                Color := SelectedColorSchemeColorFunct(dbOpts.DBColorScheme,ColorDefTable,i,0,Total);
                Bitmap.Canvas.Brush.Color := Color;
                Bitmap.Canvas.Pen.Color := Color;
                dbopts.Symbol.Color := ConvertTColorToPlatformColor(color);
                if AreaShapeFile(ShapeFileType) and (dbOpts.OutlinePolygons) then begin
                   Bitmap.Canvas.Pen.Color := ConvertPlatformColorToTColor(dbOpts.lineColor);
                   Bitmap.Canvas.Pen.Width := dbOpts.LineWidth;
                end;
            end;

            procedure TGISdataBaseModule.AddBoundingBox;
            begin
               if LineOrAreaShapeFile(ShapeFileType) then begin
                  if (LatLongCornersPresent) and (not AnswerIsYes('Overwrite exiting corners')) then exit;
                  if (LatLongCornersPresent) or MyData.AddBoundingBox then begin
                     EmpSource.Enabled := false;
                     aShapeFile.AddFields(afBoundingBox,MyData);
                     RespondToChangedDB;
                  end;
               end;
            end;


            procedure OpenDBForModalEdit(fName : PathStr);
            var
               DBNum : integer;
             begin
               if DEMDataBase.OpenNumberedGISDataBase(DBNum,fName,true) then begin
                  GISdb[DBNum].dbTablef.FormStyle := fsNormal;
                  GISdb[DBNum].dbTablef.Hide;
                  GISdb[DBNum].dbTablef.CheckBox1.Checked := true;
                  GISdb[DBNum].dbTablef.EditSymbologyOnly := true;
                  GISdb[DBNum].dbTablef.Panel7.Visible := true;
                  if GISdb[DBNum].MyData.FieldExists('FILENAME') then begin
                     while not GISdb[DBNum].MyData.eof do begin
                        fName := GISdb[DBNum].MyData.GetFieldByNameAsString('FILENAME');
                        if not FileExists(fName) then MessageToContinue('File not found: ' + fName);
                        GISdb[DBNum].MyData.Next;
                     end;
                  end;
                  GISdb[DBNum].dbTablef.ShowModal;
               end;
            end;


            procedure TGISdataBaseModule.AddAndFillFieldFromDEM(AddDEM : tAddDEM; fName : PathStr = ''; DecimalsToUse : integer = 6);
            var
               Lat,Long,Avg,Delta : float64;
               z,sum : float32;
               x,y,i,j,UseDEM,Col,Row,n,ic,Window : integer;
               eName : ShortString;
               SlopeAspectRec : tSlopeAspectRec;
            begin
               {$IfDef FieldFromDEM} WriteLineToDebugFile('TGISdataBaseModule.AddAndFillFieldFromDEM in, opt=' + IntToStr(Ord(AddDEM))); {$EndIf}
                if (AddDEM in [adElevDiff,adElevNearest,adElevNearestInt,adElevInterp,adSlope,adAvgElevInWindow]) then begin
                   if not GetDEM(UseDEM, true,'Add field from') then exit;
                   if (AddDEM in [adAvgElevInWindow]) then begin
                      Window := 5;
                      ReadDefault('Window size (rectangle radius, pixels)',Window);
                   end;
                end;
               if (AddDEM = adPickNearest) then begin
                  fName := GetFieldNameForDB('Field to add',True,fName);
                  AddFieldToDataBase(ftFloat,fName,14,6);
               end;
               if (AddDEM = adElevDiff) or (AddDEM = adDeltaAllGrids) then begin
                  eName := 'ELEV';
                  if not MyData.FieldExists(eName) then eName := 'Z';
                  if (AddDEM = adElevDiff) then begin
                     fName := 'DZ';
                     fName := GetFieldNameForDB('Elev difference field name',True,fName);
                     AddFieldToDataBase(ftFloat,fName,8,2);
                  end;
                  if (AddDEM = adDeltaAllGrids) then AddFieldToDataBase(ftFloat,'AVG_DELTA',8,2);
               end;
               if (AddDEM = adElevAllGrids) or (AddDEM = adDeltaAllGrids) then begin
                  for i := 1 to MaxDEMDataSets do begin
                     if ValidDEM(i) then begin
                        fName := 'Z_' + DEMGlb[i].AreaName;
                        if length(fName) > 10 then fName := Copy(fName,1,10);
                        AddFieldToDataBase(ftFloat,fName,14,DecimalsToUse);
                     end;
                  end;
               end;
               if (AddDEM = adElevNearestInt) then begin
                  fName := GetFieldNameForDB('Field to add',True,fName);
                  AddFieldToDataBase(ftInteger,fName,8);
               end;
               if (AddDEM in [adElevInterp,adElevNearest,adAvgElevInWindow]) then begin
                  if (fName = '') then fName := GetElevationField;
                  if not MyData.FieldExists(fName) then begin
                     AddFieldToDataBase(ftFloat,fName,14,6);
                  end;
                  {$IfDef FieldFromDEM} WriteLineToDebugFile('new field=' + fName); {$EndIf}
               end;

               if (AddDEM = adSlope) then begin
                  AddFieldToDataBase(ftFloat,'SLOPE_PC',9,2);
                  AddFieldToDataBase(ftFloat,'ASPECT',5,1);
               end;

               i := 0;
               StartProgress('DEM add');
               MyData.First;
               EmpSource.Enabled := false;
               ic := MyData.FiltRecsInDB div 100;
               if ic = 0 then ic := 1;

               while (not MyData.Eof) do begin
                  if (I mod ic = 0) then begin
                     UpdateProgressBar(i/MyData.FiltRecsInDB);
                     EmpSource.Enabled := false;
                  end;

                  if ValidLatLongFromTable(Lat,Long) then begin
                      {$IfDef FieldFromDEM} if (I mod ic = 0) then  WriteLineToDebugFile('i=' + IntToStr(i) + ' got lat long'); {$EndIf}
                      MyData.Edit;
                      if (AddDEM = adElevAllGrids) then begin
                           for i := 1 to MaxDEMDataSets do begin
                              if ValidDEM(i) then begin
                                 if DEMGlb[i].GetElevFromLatLongDegree(Lat,Long,z) then begin
                                    fName := DEMGlb[i].AreaName;
                                    if length(fName) > 10 then fName := Copy(fName,1,10);
                                    MyData.SetFieldByNameAsFloat(fName,z);
                                 end;
                              end;
                           end;
                       end;
                      if (AddDEM = adDeltaAllGrids) then begin
                           n := 0;
                           Avg := 0;
                           for j := 1 to MaxDEMDataSets do begin
                              if ValidDEM(j) then begin
                                 if DEMGlb[j].GetElevFromLatLongDegree(Lat,Long,z) then begin
                                    fName := DEMGlb[j].AreaName;
                                    if length(fName) > 10 then fName := Copy(fName,1,10);
                                    Delta := z - MyData.GetFieldByNameAsFloat(eName);
                                    MyData.SetFieldByNameAsFloat(fName,delta);
                                    Avg := Avg + abs(Delta);
                                    inc(n);
                                 end;
                              end;
                           end;
                           if (n > 0) then MyData.SetFieldByNameAsFloat('AVG_DELTA',avg/n);
                      end;
                      if (AddDEM = adElevDiff) then begin
                         if DEMGlb[UseDEM].GetElevFromLatLongDegree(Lat,Long,z) then MyData.SetFieldByNameAsFloat(fName,MyData.GetFieldByNameAsFloat(eName) - z);
                      end;
                      if (AddDEM in [adElevNearest,adElevNearestInt]) then begin
                         DEMGlb[UseDEM].LatLongDegreeToDEMGridInteger(Lat,Long,Col,Row);
                         if DEMGlb[UseDEM].GetElevMeters(Col,Row,z) then begin
                            if (AddDEM in [adElevNearest]) then MyData.SetFieldByNameAsFloat(fName,z)
                            else MyData.SetFieldByNameAsInteger(fName,round(z));
                           {$IfDef FieldFromDEM} if (I mod ic = 0) then  WriteLineToDebugFile('i=' + IntToStr(i) + ' z=' + RealToString(z,-12,-2)); {$EndIf}
                         end;
                      end;
                      if (AddDEM in [adAvgElevInWindow]) then begin
                         n := 0;
                         Sum := 0;
                         DEMGlb[UseDEM].LatLongDegreeToDEMGridInteger(Lat,Long,Col,Row);
                         for x := Col - Window to Col + Window do begin
                            for y := Row - Window to Row + Window do begin
                               if DEMGlb[UseDEM].GetElevMeters(x,y,z) then begin
                                  inc(n);
                                  Sum := Sum + z;
                               end;
                            end;
                         end;
                         MyData.SetFieldByNameAsFloat(fName,Sum/n);
                      end;

                      if (AddDEM = adElevInterp) then begin
                         if DEMGlb[UseDEM].GetElevFromLatLongDegree(Lat,Long,z) then MyData.SetFieldByNameAsFloat(fName,z);
                      end;
                      if (AddDEM = adSlope) then begin
                         DEMGlb[TheMapOwner.MapDraw.DEMonMap].LatLongDegreeToDEMGridInteger(Lat,Long,x,y);
                         if DEMGlb[UseDEM].GetSlopeAndAspect(x,y,SlopeAspectRec) then begin
                            MyData.SetFieldByNameAsFloat('SLOPE_PC',SlopeAspectRec.SlopePercent);
                            MyData.CarefullySetFloat32('ASPECT',SlopeAspectRec.AspectDir,0.1);
                         end;
                      end;
                      if (AddDEM = adPickNearest) then begin
                         if DEMGlb[UseDEM].GetElevFromLatLongDegree(Lat,Long,z) then MyData.SetFieldByNameAsFloat(fName,z);
                      end;
                  end;
                  MyData.Next;
                  inc(i);
               end;
               ShowStatus;
            end;


            procedure TGISdataBaseModule.IntervisibilityFromPoint(ChangeSymbol : boolean; FromLat,FromLong,CameraElevation,TowerHeight : float64);
            var
               i : integer;
               TStr : shortstring;
               ch : char;
               TargetZ : float32;
               Lat,Long,Distance,BearingAngle,Value,Pitch,BlockDist : float64;
            begin
                AddFieldToDataBase(ftString,'VIS',1);
                AddFieldToDataBase(ftFloat,'DIST_MILES',10,2);
                AddFieldToDataBase(ftString,'AZ_DMS',12);
                AddFieldToDataBase(ftFloat,'PITCH',6,2);
                AddFieldToDataBase(ftFloat,'DIST_KM',10,2);
                AddFieldToDataBase(ftFloat,'AZIMUTH',5,1);
                AddFieldToDataBase(ftString,'PITCH_DMS',12);
                AddSymbolToDB;
                dbOpts.DBAutoShow := dbasPointsInDB;
                EmpSource.Enabled := false;
                i := 0;
                StartProgress('Intervisibility');
                MyData.First;
                while not MyData.eof do begin
                   inc(i);
                   if (i mod 10 = 0) then UpdateProgressBar(i/MyData.FiltRecsInDB);
                   ValidLatLongFromTable(Lat,Long);
                   VincentyCalculateDistanceBearing(FromLat,FromLong,Lat,Long,Distance,BearingAngle);
                   MyData.Edit;
                   MyData.SetFieldByNameAsString('PITCH','');
                   MyData.SetFieldByNameAsString('VIS','');
                   try
                      value := Distance * 0.001;
                      MyData.CarefullySetFloat('DIST_KM',value,0.01);
                      value := Distance * 0.62 * 0.001;
                      MyData.CarefullySetFloat('DIST_MILES',value,0.01);
                      MyData.CarefullySetFloat('AZIMUTH',BearingAngle,0.1);
                      TStr := ConvertToDegreesString(BearingAngle,NearestSecond);
                      MyData.SetFieldByNameAsString('AZ_DMS',TStr);
                      if DEMglb[TheMapOwner.MapDraw.DEMonMap].GetElevFromLatLongDegree(Lat,Long,TargetZ) then begin
                         Pitch := ArcTanDeg((TargetZ - CameraElevation - DropEarthCurve(Distance)) / Distance);
                         MyData.CarefullySetFloat('PITCH',Pitch,0.01);
                         MyData.SetFieldByNameAsString('PITCH_DMS',ConvertToDegreesString(Pitch,NearestSecond));
                         if DEMglb[TheMapOwner.MapDraw.DEMonMap].LatLongDegreePointsIntervisible(FromLat,FromLong,TowerHeight,Lat,Long,1,Distance,BlockDist) then ch := 'Y'
                         else ch := 'N';
                         MyData.SetFieldByNameAsString('VIS',ch);
                         if ChangeSymbol then begin
                            if (ch = 'Y') then MyData.PostPointSymbol(MDDef.VisPtSymbol)
                            else MyData.PostPointSymbol(MDDef.MaskPtSymbol);
                         end;
                      end;
                   except
                     on Exception do {writelinetodebugFile(TStr)};
                   end;
                   MyData.Next;
                end;
                EndProgress;
                if ChangeSymbol then RedrawLayerOnMap;
            end;


            procedure ConvertShapeFileToKML(fName : PathStr; MapOwner : tMapForm);
            var
               DBonTable : integer;
            begin
               if FileExists(fName) then begin
                  OpenNumberedGISDataBase(DBonTable,fName,false,false,MapOwner);
                  KML_opts.ConvertToKML(DBonTable,'');
                  CloseAndNilNumberedDB(DBonTable);
               end;
            end;


            procedure MaskDEMFromShapeFile(DEM,DBonTable : integer; UseShapeFile,MaskShapesAreIn : boolean; SingleRecord  : integer; MaskingDistance : float64; ProgressCount : shortstring = '');
            var
               oldDBSaveOptions : tDBSaveOptions;
               x,y,Col,Row,radius,MaskSize,OldType : integer;
               MapBMP,MaskBMP : tMyBitmap;
               c : tColor;
               GazColors : tMyData;
               NameStr,MenuStr : shortString;
               BMPMemory : tBMPMemory;


                 procedure GetLineSize;
                 begin
                    MaskSize := round(MaskingDistance * 2 / DEMGlb[GISdb[DBonTable].TheMapOwner.MapDraw.DEMonMap].SelectionMap.MapDraw.ScreenPixelSize);
                    if GISdb[DBonTable].ItsAPointDB then MaskSize := MaskSize div 2;
                    if (MaskSize < 1) then MaskSize := 1;
                 end;

                 procedure PointMask;
                 begin
                    GetLineSize;
                    GISdb[DBonTable].MyData.First;
                    While not GISdb[DBonTable].MyData.eof do begin
                       DEMGlb[DEM].SelectionMap.MapDraw.LatLongDegreeToScreen(GISdb[DBonTable].MyData.GetFieldByNameAsFloat('LAT'),GISdb[DBonTable].MyData.GetFieldByNameAsFloat('LONG'),x,y);
                       MaskBMP.Canvas.Ellipse(x-MaskSize,y-MaskSize,x+MaskSize,y+MaskSize);
                       GISdb[DBonTable].MyData.Next;
                    end;
                 end;


            begin
              {$IfDef RecordMaskDEMShapeFile} WriteLineToDebugFile('MaskDEMfromshapefile in , mask=' + RealToString(MaskingDistance,-12,-2)); {$EndIf}
               if DBonTable <> 0 then with GISdb[DBonTable] do begin
                 oldDBSaveOptions := dbOpts;
                 DEMDef_routines.SaveBackupDefaults;
                 SetOptionsForMapWithNoMarginalia;
                 DEMGlb[DEM].SelectionMap.DoFastMapRedraw;
                 c := clBlack;
                 dbOpts.LineColor := ConvertTColorToPlatformColor(C);
                 dbOpts.FillColor := ConvertTColorToPlatformColor(C);

                 CopyImageToBitmap(DEMGlb[DEM].SelectionMap.Image1,MapBMP);
                 CloneImageToBitmap(DEMGlb[DEM].SelectionMap.Image1,MaskBMP);

                 EmpSource.Enabled := false;
                 {$IfDef RecordMaskDEMShapeFileBMP} PetImage.SaveBitmap(MaskBMP,MDTempDir + 'cloned' + OverlayFExt); {$EndIf}
                 {$IfDef RecordMaskDEMShapeFile} WriteLineToDebugFile('MaskDEMfromshapefile prelims over'); {$EndIf}

                 if SimplePointFile then begin
                    MaskBMP.Canvas.Brush.Style := bsSolid;
                    MaskBMP.Canvas.Brush.Color := clBlack;
                    MaskBMP.Canvas.Pen.Color := clBlack;
                    MaskBMP.Canvas.Pen.Width := 1;
                    if UseShapefile then begin
                       GetLineSize;
                       GISdb[DBonTable].MyData.First;
                       While not GISdb[DBonTable].MyData.eof do begin
                          DEMGlb[DEM].SelectionMap.MapDraw.LatLongDegreeToScreen(GISdb[DBonTable].MyData.GetFieldByNameAsFloat('LAT'),GISdb[DBonTable].MyData.GetFieldByNameAsFloat('LONG'),x,y);
                          MaskBMP.Canvas.Ellipse(x-MaskSize,y-MaskSize,x+MaskSize,y+MaskSize);
                          GISdb[DBonTable].MyData.Next;
                       end;
                    end
                    else begin   //using gazetteer
                       PickGazFeatures;
                       SetUpGazFile(GazColors,NameStr);
                       MenuStr := '';
                       While not GazColors.Eof do begin
                          MenuStr := MenuStr + GazColors.GetFieldByNameAsString('NAME') + '  (' + IntToStr(GazColors.GetFieldByNameAsInteger('RADIUS')) + ' m)' + MessLineBreak;
                          GazColors.Next;
                       end;

                       if AnswerIsYes('Modify radius values' + MessLineBreak + MessLineBreak + MenuStr) then begin
                          GazColors.First;
                          while not GazColors.eof do begin
                             Radius := GazColors.GetFieldByNameAsInteger('RADIUS');
                             ReadDefault('Radius for ' + GazColors.GetFieldByNameAsString('NAME') + ' (m)',Radius);
                             GazColors.Edit;
                             GazColors.SetFieldByNameAsInteger('RADIUS',Radius);
                             GazColors.Next;
                          end;
                       end;

                       GazColors.First;
                       while not GazColors.eof do begin
                          MyData.ApplyFilter(GazColors.GetFieldByNameAsString('FILTER'));
                          MaskingDistance := GazColors.GetFieldByNameAsInteger('RADIUS');
                          PointMask;
                          GazColors.Next;
                       end;
                       GazColors.Destroy;
                    end;
                 end
                 else begin
                    {$IfDef RecordMaskDEMShapeFile} WriteLineToDebugFile('MaskDEMfromshapefile1Click not point shape file'); {$EndIf}
                    GetLineSize;
                    MaskBMP.Canvas.Pen.Color := clBlack;
                    MaskBMP.Canvas.Pen.Width := MaskSize;
                    MaskBMP.Canvas.Brush.Color := clBlack;
                    MaskBMP.Canvas.Brush.Style := bsSolid;
                    OldType := aShapeFile.MainFileHeader.ShapeType;
                    if MDDef.TreatLineAsPolygon and (OldType = 3) then aShapeFile.MainFileHeader.ShapeType := 5;
                    if MDDef.TreatLineAsPolygon and (OldType = 13) then aShapeFile.MainFileHeader.ShapeType := 15;
                    if MDDef.TreatPolygonAsLine and (OldType = 5) then aShapeFile.MainFileHeader.ShapeType := 3;
                    if MDDef.TreatPolygonAsLine and (OldType = 15) then aShapeFile.MainFileHeader.ShapeType := 13;
                    if (SingleRecord <> 0) then begin
                      {$IfDef RecordCurrentRecord} WriteLineToDebugFile('MaskDEMFromShapeFile, RecNo=' + IntToStr(SingleRecord)); {$EndIf}
                       aShapefile.PlotSingleRecordMap(TheMapOwner.MapDraw,MaskBMP,SingleRecord);
                    end
                    else begin
                       dbOpts.AreaSymbolFill := bsSolid;
                       dbOpts.LineWidth := MaskSize;
                       dbOpts.LineColor := claBlack;
                       aShapeFile.PlotAllRecords(TheMapOwner.MapDraw,MaskBMP);
                    end;
                    aShapeFile.MainFileHeader.ShapeType := OldType;
                 end;

                 EmpSource.Enabled := true;

                 {$IfDef RecordMaskDEMShapeFileBMP} PetImage.SaveBitmap(MaskBMP,MDTempDir + 'Mask_dbdisplayed.bmp'); {$EndIf}

                 if MDDef.ShowMasks then Petimage_form.DisplayBitmap(MaskBMP,'Mask from ' + dbName);

                    {$IfDef RecordMaskDEMShapeFile}
                       WriteLineToDebugFile('MaskDEMfromshapefile1Click MapZoomFactor =' + RealToString(TheMapOwner.MapDraw.MapZoomFactor,-8,4));
                       WriteLineToDebugFile('DEM size:  ' + IntToStr(DEMGlb[DEM].DEMheader.NumCol) + 'x' + IntToStr(DEMGlb[DEM].DEMheader.NumRow));
                       WriteLineToDebugFile('Map size:  ' + TheMapOwner.MapDraw.MapSizeString + '  BMP size:  ' + IntToStr(MaskBMP.Width) + 'x' + IntToStr(MaskBMP.Height));
                       WriteLineToDebugFile('MapCorners.BoundBoxDataGrid:  ' + sfBoundBoxToString(TheMapOwner.MapDraw.MapCorners.BoundBoxDataGrid));
                    {$EndIf}

                    BMPMemory := tBMPMemory.Create(MaskBMP);
                    StartProgress('Mask ' + ProgressCount);
                    for Col := 0 to pred(DEMGlb[DEM].DEMheader.NumCol) do begin
                       if (col mod 250 = 0) then UpDateProgressBar(Col/DEMGlb[DEM].DEMheader.NumCol);
                       for Row := 0 to pred(DEMGlb[DEM].DEMheader.NumRow) do begin
                          TheMapOwner.MapDraw.DEMGridToScreen(Col,Row,x,y);
                          if TheMapOwner.MapDraw.OnScreen(x,y) then begin
                            if (BMPMemory.SameColor(x,y,RGBTripleBlack)) then begin
                               if (not MaskShapesAreIn) then DEMGlb[DEM].SetGridMissing(Col,Row);
                            end
                            else begin
                               if (MaskShapesAreIn) then DEMGlb[DEM].SetGridMissing(Col,Row);
                            end;
                          end
                          else DEMGlb[DEM].SetGridMissing(Col,Row);
                       end;
                    end;
                    BMPMemory.Destroy;

                  EndProgress;
                  {$IfDef RecordMaskDEMShapeFile} WriteLineToDebugFile('MaskDEMfromshapefile1Click masking over'); {$EndIf}
                  {$IfDef RecordMaskDEMShapeFileBMP} PetImage.SaveBitmap(MaskBMP,MDTempDir + 'final_Mask.bmp'); {$EndIf}

                  FreeAndNil(MaskBMP);
                  FreeAndNil(MapBMP);
                  dbOpts := oldDBSaveOptions;
                  DEMDef_routines.RestoreBackupDefaults;
                  DEMGlb[DEM].CheckMaxMinElev;
                  DEMGlb[DEM].DEMstatus := dsUnsaved;
                  DEMGlb[DEM].SelectionMap.DoBaseMapRedraw;
               end;
               {$IfDef RecordMaskDEMShapeFile} WriteLineToDebugFile('MaskDEMfromshapefile1Click out'); {$EndIf}
            end;


            function OpenMultipleDataBases(WhatFor : ANSIstring; fName : Pathstr = ''; ShowTable : boolean = true) : integer;
            var
               FilesWanted : tStringList;
               i : integer;
            begin
               {$IfDef RecordOpenDataBase} WriteLineToDebugFile('OpenADataBase in,fName=' + fName); {$EndIf}
               if (fName = '') then begin
                  FilesWanted := tStringList.Create;
                  FilesWanted.Add(ExtractFilePath(LastDataBase));
                  if GetMultipleFiles('DB for ' + WhatFor,DBMaskString,FilesWanted,MDdef.DefDBFilter) then begin
                     for I := 0 to pred(FilesWanted.Count) do begin
                        fName := FilesWanted.Strings[i];
                        OpenNumberedGISDataBase(Result,fName,ShowTable);
                     end;
                     LastDataBase := FilesWanted.Strings[0];
                     FilesWanted.Free;
                  end;
               end
               else begin
                  OpenNumberedGISDataBase(Result,fName,ShowTable);
               end;
               {$IfDef RecordOpenDataBase} WriteLineToDebugFile('OpenADataBase out,fName=' + fName); {$EndIf}
            end;


            function CopyDatabaseAndOpen(GIS : TGISdataBaseModule; OpenLink : boolean = true) : integer;
            var
               fName : PathStr;

               procedure CopyOneFile(fName : PathStr);
               begin
                  if FileExists(fName) then begin
                     CopyFile(fName,MDTempDir + ExtractFileName(fName));
                  end;
               end;

            begin
               {$IfDef ExSaveDBstatus}
               {$Else}
                  GIS.SaveDataBaseStatus;
               {$EndIf}

               fName := GIS.dbFullName;
               CopyOneFile(ChangeFileExt(FName,'.shp'));
               CopyOneFile(ChangeFileExt(FName,'.shx'));
               CopyOneFile(ChangeFileExt(FName,DefaultDBExt));
               if OpenLink and (GIS.LinkTable <> nil) then CopyOneFile(GIS.LinkTable.TableName);

               OpenNumberedGISDataBase(Result,MDTempDir + ExtractFileName(fName));
               if OpenLink and (GIS.LinkTable <> nil) then begin
                  fName := MDTempDir + ExtractFileName(GIS.LinkTable.TableName);
                  GISdb[Result].LinkSecondaryTable(fName);
               end;
            end;


            function TotalNumOpenDatabase : integer;
            var
               i : integer;
            begin
               Result := 0;
               for i := 1 to MaxDataBase do if ValidDB(i) then inc(Result);
            end;


            function NumOpenDatabaseThisMap(MapOwner : tMapForm) : integer;
            var
               i : integer;
            begin
               {$IfDef RecordDBCount} WritelineToDebugFile('NumOpenDatabaseThisMap in ' + MapOwner.Caption); {$EndIf}
               if MDdef.DBsOnAllMaps then begin
                  Result := TotalNumOpenDatabase;
               end
               else begin
                  Result := 0;
                  for i := 1 to MaxDataBase do begin
                     if ValidDB(i) and (GISdb[i].theMapOwner <> Nil) and (GISdb[i].theMapOwner = MapOwner) then begin
                        inc(Result);
                     end;
                  end;
               end;
            end;


            function PickOpenGISDataBase(WhatFor : shortString; Ignore : integer = 0; Geometry : integer = 0; FieldNeeded : ShortString = '') : integer;
            var
               i      : integer;
               OpenDB : tStringList;

               function GoodGeometry(I : integer) : boolean;
               begin
                  if (Geometry = 0) then Result := true
                  else if (Geometry = 1) then Result := GISdb[i].ItsApointDB
                  else if (Geometry = 3) then Result := LineShapeFile(GISdb[i].ShapeFileType)
                  else if (Geometry = 5) then Result := AreaShapeFile(GISdb[i].ShapeFileType);
               end;

            begin
               Result := 0;
               OpenDB := tStringList.Create;
               for i := 1 to MaxDataBase do begin
                  if (I <> Ignore) and (GISdb[i] <> Nil) then begin
                     if GoodGeometry(i) or ((FieldNeeded = '') or GISdb[i].MyData.FieldExists(FieldNeeded)) then OpenDB.Add(IntToStr(i) + '  ' + GISdb[i].dbName);
                  end;
               end;
               if (OpenDB.Count > 0) and MultiSelectSingleColumnStringList(WhatFor,Result,OpenDB,true) then Val(ptTrim(Copy(OpenDB.Strings[Result],1,2)),Result,i)
               else Result := -99;
               OpenDB.Free;
            end;


            function TGISdataBaseModule.GetFieldNameForDB(Prompt : ShortString; AllowOverWrite : boolean; SuggestedName: ShortString = '') : ShortString;
            var
               ItExists : boolean;
               TStr : shortstring;
            begin
               TStr := SuggestedName;
               repeat
                  repeat
                     GetString('New Field (<= 10 chars)',TStr,true,DBaseFieldNameChars);
                  until length(TStr) < 11;

                  ItExists := MyData.FieldExists(TStr);
                  if AllowOverwrite and ItExists then begin
                     ItExists := not AnswerIsYes('Field ' + TStr + ' exists; Overwrite');
                  end;
               until not ItExists;
               Result := TStr;
            end;

            procedure TGISdataBaseModule.ChangeLatLongLocation(Lat,Long : float64);
            var
               z : float32;
               fName : PathStr;
            begin
               MyData.SetFieldByNameAsFloat(LatFieldName, Lat);
               MyData.SetFieldByNameAsFloat(LongFieldName, Long);
               if MyData.FieldExists('MGR/S') then begin
                  RedefineWGS84DatumConstants(Long);
                  MyData.SetFieldByNameAsString('MGRS',WGS84DatumConstants.LatLongToMGRS(Lat,Long));
               end;
               if MyData.FieldExists('Z') and ValidDEM(TheMapOwner.MapDraw.DEMonMap) and DEMGlb[TheMapOwner.MapDraw.DEMonMap].GetElevFromLatLongDegree(Lat,Long,z) then begin
                  MyData.SetFieldByNameAsFloat('Z',z);
               end;
               if ItsFanFile then begin
                  if MyData.FieldExists('IMAGE') then begin
                     fName := MyData.GetFieldByNameAsString('IMAGE');
                     DeleteFileIfExists(fName);
                     MyData.SetFieldByNameAsString('IMAGE','');
                     DEMGlb[1].SelectionMap.MapDraw.DeleteSingleMapLayer(DEMGlb[1].SelectionMap.MapDraw.AllFansCoverageFName);
                  end;
                  if MyData.FieldExists('COVERS') then MyData.SetFieldByNameAsString('COVERS', '');
                  if MyData.FieldExists('UNIQUE_COV') then MyData.SetFieldByNameAsString('UNIQUE_COV', '');
                  if MyData.FieldExists('SEE_TARGET') then MyData.SetFieldByNameAsString('SEE_TARGET', '');
               end;
            end;


            function TGISdataBaseModule.GetMaskFieldName : string10;
            var
               Pickins : tStringList;
               j : integer;
            begin
               Result := 'MASK';
               if MyData.FieldExists(Result) then begin
                  Pickins := tStringList.Create;
                  Pickins.Add(Result);
                  for j := 2 to 5 do begin
                     Result := 'MASK' + IntToStr(j);
                     if MyData.FieldExists(Result) then Pickins.Add(Result);
                  end;
                  j := 0;
                  Petmar.MultiSelectSingleColumnStringList('Mask field to use',j,Pickins);
                  Result := Pickins[j];
                  Pickins.Free;
                  EmpSource.Enabled := false;
                  MyData.ApplyFilter('');
                  while not MyData.EOF do begin
                      MyData.Edit;
                      MyData.SetFieldByNameAsString(Result, '0');
                      MyData.Next;
                  end;
                  EmpSource.Enabled := true;
               end
               else begin
                  AddFieldToDataBase(ftString,'MASK',1,0);
               end;
            end;


            function TGISdataBaseModule.FindAreaRecordWithPoint(Lat,Long : float64; AlreadyFiltered : boolean = true) : boolean;
            var
               theMainFilter : string;
               i : integer;
            begin
               {$IfDef RecordPointInArea} WriteLineToDebugFile('FindAreaRecordWithPoint in,  ' + DBName + ' ' + LatLongDegreeToString(Lat,Long)); {$EndIf}
               Result := false;
               if AreaShapeFile(ShapeFileType) or (LatLongCornersPresent) then begin
                  if not AlreadyFiltered then begin
                     if (dbOpts.MainFilter = '') then theMainFilter := '' else theMainFilter := dbOpts.MainFilter + ' AND ';
                     MyData.ApplyFilter(theMainFilter + PointInBoxGeoFilter(Lat,Long));
                  end;
                  {$IfDef RecordPointInArea} WriteLineToDebugFile('FindAreaRecordWithPoint Rec filter=' + MyData.Filter + '  Found recs=' + IntToStr(MyData.FiltRecsInDB)); {$EndIf}
                  if (MyData.FiltRecsInDB > 0) then begin
                     if (MyData.FiltRecsInDB = 1) or (TheMapOwner = Nil) then Result := true
                     else begin
                        if AreaShapeFile(ShapeFileType) then begin
                           {$IfDef RecordPointInArea}
                              WriteLineToDebugFile('Records found: ' + IntToStr(MyData.RecordCount));
                              MyData.First;
                              for i := 1 to MyData.FiltRecsInDB do begin
                                 if aShapeFile.PointInRecord(MyData.RecNo,Lat,Long) then
                                    WriteLineToDebugFile('Rec found in box: ' + IntToStr(MyData.RecNo))
                                 else WriteLineToDebugFile('Rec not in box: ' + IntToStr(MyData.RecNo));
                                 MyData.Next;
                              end;
                           {$EndIf}

                           MyData.First;
                           for i := 1 to MyData.FiltRecsInDB do begin
                              if aShapeFile.PointInRecord(MyData.RecNo,Lat,Long) then begin
                                 Result := true;
                                 {$IfDef RecordPointInArea} WriteLineToDebugFile('FindAreaRecordWithPoint out  ' + IntToStr(i)); {$EndIf}
                                 exit;
                              end;
                              MyData.Next;
                           end;
                        end
                        else begin
                           {$IfDef RecordPointInArea} WriteLineToDebugFile('Not area shape file'); {$EndIf}
                        end;
                     end;
                  end;
               end;
               {$IfDef RecordPointInArea} WriteLineToDebugFile('FindAreaRecordWithPoint out (drop through)'); {$EndIf}
            end;


            procedure LoadShapeFileGroup(MapOwner : tMapForm; fName : PathStr = '');
            var
               TheIndex : tMyData;
               i,dbnum : integer;
               fName2 : PathStr;
            begin
                if (FName = '') then begin
                   fName := DBDir + 'groups\';
                   if not Petmar.GetFileFromDirectory('shape file grouping',DefaultDBMask,fName) then exit;
                end;
                if FileExists(fName) then begin
                   TheIndex := tMyData.Create(fName);
                   for i := 0 to 9 do begin
                       TheIndex.ApplyFilter('ORDER = ' + IntToStr(i) + ' AND PLOT =' + QuotedStr('Y'));
                       while not TheIndex.EOF do begin
                          dbNum := 0;
                          fName2 := TheIndex.GetFieldByNameAsString('FILENAME');
                          if FileExists(fName2) then begin
                             OpenNumberedGISDataBase(dbNum,fName2,true,false,MapOwner);
                             GISdb[dbNum].SetColorsFromDB(TheIndex);
                             if TheIndex.GetFieldByNameAsString('PLOT') = 'Y' then GISdb[dbNum].dbOpts.DBAutoShow := dbasDefault
                             else GISdb[dbNum].LayerIsOn := false;
                          end;
                          TheIndex.Next;
                       end;
                   end;
                   TheIndex.Destroy;
                   MapOwner.MapDraw.MapOverlays.ovShapeFileGroup := fName;
                   Map_Overlays.AddOrSubtractOverlay(MapOwner,ovoDataBases,True);
                   MapOwner.DoFastMapRedraw;
                end;
            end;


            function OpenGazetteer(var i : integer; fName : PathStr; MapOwner : tMapForm) : boolean;
            begin
               Result := false;
               if (Fname = '') or (not FileExists(fName)) then begin
                  {$IfDef RecordGAZ} WriteLineToDebugFile('OpenGazetteer and asking about file'); {$EndIf}
                  if not DEM_Gaz_Opts.GetGazFileName(LastGazFile) then exit;
               end
               else LastGazFile := fName;
               if FileExists(LastGazFile) then begin
                  if OpenNumberedGISDataBase(i,fname,false,false,MapOwner) then begin
                     {$IfDef RecordGAZ} WriteLineToDebugFile('OpenGazetteer=' + LastGazFile); {$EndIf}
                      GISdb[i].SetGazTableOptions;
                      if (MapOwner <> Nil) then with MapOwner.MapDraw.MapCorners do begin
                         if (BoundBoxGeo.ymax - BoundBoxGeo.ymin < 15) then  begin
                            GISdb[i].MyData.ApplyFilter(MakePointGeoFilter(GISdb[i].LatFieldName,GISdb[i].LongFieldName,BoundBoxGeo.ymax,BoundBoxGeo.xmin,BoundBoxGeo.ymin,BoundBoxGeo.xmax));
                         end;
                         GISdb[i].ShowStatus;
                      end;
                      Result := true;
                   end;
                end;
            end;


            procedure TGISdataBaseModule.IdentifyRecord(xpic,ypic : integer; Lat,Long : float64; var RecsFound : integer;  ShowRec,LabelRec : boolean;  var FeatureName : ShortString;
                ShowItModal : boolean = true; JustFilterDB : boolean = false);
            var
               LatLow,LongLow,LatHigh,LongHigh : float64;
               Tol,i : integer;
               OldFilter : string;
               Bitmap : tMyBitmap;


               procedure  ProcessPointFile;
               var
                  j : integer;
               begin
                  {$IfDef RecordID} WriteLineToDebugFile('Point DB'); {$EndIf}
                  i := 0;
                  repeat
                      inc(i,IDRecScreenIncr);
                      if (TheMapOwner <> Nil) then begin
                         TheMapOwner.MapDraw.ScreenToLatLongDegree(xpic-i,ypic-i,LatHigh,LongLow);
                         TheMapOwner.MapDraw.ScreenToLatLongDegree(xpic+i,ypic+i,LatLow,LongHigh);
                         PetMath.MinOfPairFirst(LatLow,LatHigh);
                         PetMath.MinOfPairFirst(LongLow,LongHigh);
                         dbOpts.GeoFilter := MakePointGeoFilter(LatFieldName,LongFieldName,LatHigh,LongLow,LatLow,LongHigh);
                         AssembleGISFilter;
                      end
                      else begin
                        {$IfDef NoDBGrafs}
                        {$Else}
                           LongLow := theGraphOwner.GraphDraw.InvGraphX(xpic-i);
                           LongHigh := theGraphOwner.GraphDraw.InvGraphX(xpic+i);
                           LatLow := theGraphOwner.GraphDraw.InvGraphY(ypic+i);
                           LatHigh := theGraphOwner.GraphDraw.InvGraphY(ypic-i);
                           MyData.ApplyFilter('X' + ' >= ' + RealToString(LongLow,-12,-4) + ' AND ' + 'X' + ' <= ' + RealToString(LongHigh,-12,-4) + ' AND ' + 'Y' + ' >= ' +
                              RealToString(LatLow,-12,-4) + ' AND ' + 'Y' + ' <= ' + RealToString(LatHigh,-12,-4));
                        {$EndIf}
                      end;
                  until (MyData.FiltRecsInDB > 0) or (i >= IDRecScreenTolerance);

                  RecsFound := MyData.FiltRecsInDB;
                  DBEditRecord := MyData.RecNo;
                  if JustFilterDB then exit;

                  {$IfDef RecordID}
                     if (RecsFound = 0) then WriteLineToDebugFile('Final ID filter: ' + MyData.Filter);
                     WriteLineToDebugFile('filtered record count =' + IntToStr(MyData.FiltRecsInDB));
                  {$EndIf}

                  if (RecsFound > 0) then begin
                     if (ShowRec or LabelRec) then begin
                        if (RecsFound > 3) then ReadDefault('Show how many of ' + IntToStr(RecsFound) + ' records found',RecsFound);
                        MyData.First;
                        for j := 0 to pred(RecsFound) do begin
                           if ShowRec then begin
                              if (dbTablef <> Nil) then dbTablef.Highlightrecordonmap1Click(Nil);
                              DisplayTheRecord(j,ShowItModal,DEMNowDoing = EditDBRecs);
                           end;
                           if LabelRec then begin
                              theMapOwner.Image1.Canvas.TextOut(xpic+4,ypic+4,MyData.GetFieldByNameAsString(dbOpts.LabelField));
                           end;
                           if MyData.EOF then break;
                        end;
                     end;
                  end
                  else begin
                     {$IfDef RecordID} WriteLineToDebugFile('No recs found'); {$EndIf}
                  end;
               end;

               procedure ProcessShapeFile;
               var
                  j : integer;
               begin
                  Tol := 1;
                  repeat
                     TheMapOwner.MapDraw.ScreenToLatLongDegree(xpic-Tol,ypic-Tol,LatHigh,LongLow);
                     TheMapOwner.MapDraw.ScreenToLatLongDegree(xpic+Tol,ypic+Tol,LatLow,LongHigh);
                     dbOpts.GeoFilter := MakeGeoFilterFromCorners(LatHigh,LongLow,LatLow,LongHigh);
                     AssembleGISFilter;
                     {$IfDef RecordID} WriteLineToDebugFile('Filter tolerance =' + RealToString(Tol,-18,-6) + '   record count =' + IntToStr(MyData.FiltRecsInDB)); {$EndIf}
                     inc(Tol);
                  until (Tol > IDRecScreenTolerance) or (MyData.FiltRecsInDB >= 1);
                  {$If Defined(RecordIDProblems) or Defined(RecordRedistrict)}
                     WriteLineToDebugFile('filtered record count =' + IntToStr(MyData.FiltRecsInDB));
                     if (RecsFound = 0) then  begin
                        WriteLineToDebugFile('ID filter: ' + MyData.Filter + '  ' + LatLongDegreeToString(Lat,Long));
                     end;
                  {$EndIf}
                  RecsFound := 0;
                  if (MyData.FiltRecsInDB > 0) then begin
                       MyData.First;
                       for j := 0 to pred(MyData.FiltRecsInDB) do begin
                          if aShapeFile.PointInRecord(MyData.RecNo,Lat,Long) then begin
                             {$IfDef RecordID} WriteLineToDebugFile('Rec: ' + IntToStr(MyData.RecNo)); {$EndIf}

                             {$IfDef ExRedistrict}
                             {$Else}
                             if (DEMNowDoing = RecolorRedistrict) then  begin
                                {$IfDef RecordRedistrict} WriteLineToDebugFile('RecolorRedistrict, changed=' + IntToStr(MyData.RecNo) + ' in box=' + IntToStr(MyData.FiltRecsinDB)); {$EndIf}
                                MyData.Edit;
                                MyData.SetFieldByNameAsString('DISTRICT',RedistrictForm.ComboBox1.Text);
                                MyData.SetFieldByNameAsInteger('COLOR',RedistrictForm.CurrentColor);
                                RedistrictForm.ChangeBlock;
                                MyData.Next;
                             end
                             else {$EndIf} begin
                                CreateBitmap(Bitmap,TheMapOwner.MapDraw.MapXSize,TheMapOwner.MapDraw.MapYSize);

                                if AreaShapeFile(ShapeFileType) then Bitmap.Canvas.Pen.Width := 1
                                else if LineShapeFile(ShapeFileType) then Bitmap.Canvas.Pen.Width := IDRecScreenTolerance;

                                Bitmap.Canvas.Pen.Color := clBlack;
                                Bitmap.Canvas.Brush.Color := clBlack;
                                Bitmap.Canvas.Brush.Style := bsSolid;
                                DisplayCurrentRecordOnMap(TheMapOwner.MapDraw,Bitmap);

                                if (Bitmap.Canvas.Pixels[xpic,ypic] <> clWhite) then begin
                                   if ShowRec then DisplayTheRecord(j,ShowItModal,DEMNowDoing = EditDBRecs);
                                   if LabelRec then begin
                                      TheMapOwner.MapDraw.ScreenToLatLongDegree(xpic,ypic,LatHigh,LongLow);
                                      theMapOwner.AddPointToDB(LatHigh,LongLow,MyData.GetFieldByNameAsString(dbOpts.LabelField));
                                   end;
                                   if (dbOpts.LabelField <> '') then FeatureName := MyData.GetFieldByNameAsString(dbOpts.LabelField);
                                   inc(RecsFound);
                                end;
                                Bitmap.Free;
                             end;
                          end
                          else begin
                             {$IfDef RecordRedistrict} WriteLineToDebugFile('RecolorRedistrict, nothing in polygon  ' + IntToStr(MyData.RecNo) + '   ' + LatLongDegreeToString(Lat,Long)); {$EndIf}
                          end;
                          MyData.Next;
                       end {for j};
                  end;
               end;


            begin
               {$IfDef RecordID} WriteLineToDebugFile('tGISDataBase.IdentifyRecord enter ' + dbName); {$EndIf}
               PrevNextButtonsEnabled := false;
               EmpSource.Enabled := false;
               OldFilter := dbOpts.GeoFilter;

               AddSequentialIndex(RecNoFName,false);

               {$IfDef RecordID}
                  WriteLineToDebugFile('unfiltered record count =' + IntToStr(MyData.TotRecsInDB));
                  WriteLineToDebugFile('Old filter =' + OldFilter);
                  WriteLineToDebugFile('Main filter =' + dbOpts.MainFilter);
               {$EndIf}

               if (TheMapOwner <> Nil) and (xpic < -99) and (Ypic < -99) then begin
                  TheMapOwner.MapDraw.LatLongDegreeToScreen(Lat,Long,xpic,ypic);
               end;

               if SimplePointFile or XYZFile then begin
                  ProcessPointFile;
               end
               else if LatLongCornersPresent then begin
                  ProcessShapeFile;
               end;

               if (DEMNowDoing = IDDBforAction) then begin
                  if (RecsFound > 1) then begin
                     MessageToContinue('Too many records found');
                  end;
                  if (RecsFound = 1) and (DragonPlotForm[1] <> nil) then begin
                     DragonPlotForm[1].BitBtn13Click(nil);
                  end;
               end;
               if not JustFilterDB then begin
                  dbOpts.GeoFilter := OldFilter;
                  AssembleGISFilter;
               end;
               {$IfDef RecordID} WriteLineToDebugFile('Leaving tGISDataBase.IdentifyRecord, ' + DBName + ' filter=' + MyData.Filter + ' and recs=' + IntToStr(MyData.FiltRecsInDB)); {$EndIf}
               ShowStatus;
            end;


            procedure TGISdataBaseModule.OutlineCurrentViewOnMap;
            var
               Lat,Long,Lat2,Long2,HFOV,Az : float64;
               x,y : integer;
            begin
                if (theMapOwner <> Nil) and ValidLatLongFromTable(Lat,Long) and MyData.FieldExists('HFOV') and MyData.FieldExists('AZIMUTH') then begin
                   theMapOwner.Image1.Canvas.Pen.Color := clRed;
                   theMapOwner.Image1.Canvas.Pen.Width := 3;
                   HFOV := MyData.GetFieldByNameAsFloat('HFOV');
                   AZ := MyData.GetFieldByNameAsFloat('AZIMUTH');
                   VincentyPointAtDistanceBearing(Lat,Long,25000,Az-0.5*HFOV,Lat2,Long2);
                   theMapOwner.MapDraw.LatLongDegreeToScreen(Lat2,Long2,x,y);
                   theMapOwner.Image1.Canvas.MoveTo(x,y);
                   theMapOwner.MapDraw.LatLongDegreeToScreen(Lat,Long,x,y);
                   theMapOwner.Image1.Canvas.LineTo(x,y);
                   VincentyPointAtDistanceBearing(Lat,Long,25000,Az+0.5*HFOV,Lat2,Long2);
                   theMapOwner.MapDraw.LatLongDegreeToScreen(Lat2,Long2,x,y);
                   theMapOwner.Image1.Canvas.LineTo(x,y);
                end;
            end;

            procedure TGISdataBaseModule.AddMultiFieldStats(WhatFor : shortstring; What : tMultiFieldStats; TheFields : tStringList = Nil; NewFName : shortstring = '');
            var
               i,j,NPts : integer;
               value : float64;
               zs : ^bfarray32;
            begin
               if (TheFields = Nil) then TheFields := GetMultipleNumericFields(WhatFor);
               if NewFName = '' then NewFName := New_Field.AddNewField(Self,NewFName,ftFloat,18)
               else AddFieldToDataBase(ftFloat,NewFName,18,6);
               New(zs);
               StartProgress('Multiple field stats');
               MyData.First;
               i := 0;
               while not MyData.eof do begin
                  if (i mod 1000 = 0) then begin
                     UpdateProgressBar(i/MyData.FiltRecsInDB);
                     EmpSource.Enabled := false;
                  end;
                  inc(i);
                  NPts := 0;
                  for j := 0 to pred(TheFields.Count) do begin
                     if MyData.CarefullyGetFieldByNameAsFloat64(TheFields.Strings[j],value) then begin
                        zs^[Npts] := value;
                        inc(NPts);
                     end;
                  end;
                  if NPts > 0 then begin
                     if What in [mfsSum,mfsMean] then begin
                        Value := 0;
                        for j := 0 to pred(NPts) do Value := Value + zs^[j];
                        if What = mfsMean then Value := Value / Npts;
                     end;
                     if What = mfsMedian then Value := Median(zs^,NPts);
                     if What = mfsMin then Value := MinInArray(NPts,zs^);
                     if What = mfsMax then Value := MaxInArray(NPts,zs^);
                     MyData.Edit;
                     MyData.SetFieldByNameAsFloat(NewFName,value);
                  end;
                  MyData.Next;
               end;
               Dispose(zs);
               ShowStatus;
            end;


            procedure TGISdataBaseModule.QueryBox(MapDraw : tMapDraw; x1,y1,x2,y2 : integer; DisplayOnTable : boolean);
            var
               Lat1,Long1,Lat2,Long2 : float64;
            begin
               MapDraw.ScreenToLatLongDegree(x1,y1,Lat1,Long1);
               MapDraw.ScreenToLatLongDegree(x2,y2,Lat2,Long2);
               {$IfDef RecordDataBase} WriteLineToDebugFile('TGISDataBase.QueryBox   ' + LatLongDegreeToString(Lat1,Long1) + '   '+ LatLongDegreeToString(Lat2,Long2)); {$EndIf}
               QueryGeoBox(Lat1,Long1,Lat2,Long2,DisplayOnTable);
            end;


            procedure TGISdataBaseModule.LimitDBtoMapArea;
            begin
               if (TheMapOwner <> Nil) then with TheMapOwner.MapDraw.MapCorners do begin
                  QueryGeoBox(BoundBoxGeo.ymax,BoundBoxGeo.xmin,BoundBoxGeo.ymin,BoundBoxGeo.xmax,true);
               end;
            end;

            procedure TGISdataBaseModule.PlotSingleMonth(Month: integer);
            begin
               if MyData.FieldExists(MonthFieldName) then begin
                  dbOpts.TimeFilter := MonthFieldName + IntToStr(Month);
                  AssembleGISFilter;
                  RedrawLayerOnMap;
               end;
            end;



{$IfDef ExGeography}
{$Else}
   procedure TGISdataBaseModule.StartClimateDisplay(Mode : integer);
   begin
      dbOpts.dbColorMode := 1;
      dbOpts.DBAutoShow := Mode;
      if (Mode in [dbasMonthlyTemp,dbasMonthlyRain]) then begin
         GISProportionalSymbols(Mode);
      end
      else RedrawLayerOnMap;
   end;
{$EndIf}




procedure TGISdataBaseModule.ProcessDBIniFile(iniWhat : tIniWhat; SectionRestoreDefaults : ShortString = ''; ExplicitName : PathStr = '');
var
   MDIniFile : tMDiniFile;
begin
   {$IfDef RecordINIfiles} WriteLineToDebugFile('ProcessIniFile in, ' + ExplicitName); {$EndIf}
   MDIniFile := tMDiniFile.OpenMDiniFile(iniWhat, SectionRestoreDefaults,ExplicitName);

   {$IfDef VCL}
      MDIniFile.InitializeMyFont('GisLabelFont1',dbOpts.GisLabelFont1,'Courier New',8,claBlack);
      MDIniFile.InitializeMyFont('GisLabelFont2',dbOpts.GisLabelFont2,'Courier New',8,claBlack);
   {$EndIf}

   MDIniFile.AParameter('Fonts','TTSymbolFontName',dbOpts.TTSymbolFontName,'Tahoma');
   MDIniFile.AColorParameter('Fonts','TTSymbolFontColor',dbOpts.TTSymbolFontColor,claBlack);
   MDIniFile.ACharacter('Fonts','TTSymbolFontChar',dbOpts.TTSymbolFontChar,'*');
   MDIniFile.AParameter('Fonts','TTSymbolFontSize',dbOpts.TTSymbolFontSize,14);
   MDIniFile.AParameter('Fonts','XLabelOff',dbOpts.XLabelOff,4);

   MDIniFile.AParameter('fields','LinkFieldThisDB',dbOpts.LinkFieldThisDB,'');
   MDIniFile.AParameter('fields','LinkFieldOtherDB',dbOpts.LinkFieldOtherDB,'');
   MDIniFile.AParameter('fields','QFField1',dbOpts.QFField1,'');
   MDIniFile.AParameter('fields','QFField2',dbOpts.QFField2,'');
   MDIniFile.AParameter('fields','QFField3',dbOpts.QFField3,'');
   MDIniFile.AParameter('fields','XField',dbOpts.XField,'');
   MDIniFile.AParameter('fields','YField',dbOpts.YField,'');
   MDIniFile.AParameter('fields','ZField',dbOpts.ZField,'');
   MDIniFile.AParameter('fields','IconField',dbOpts.IconField,'');
   MDIniFile.AParameter('fields','RedField',dbOpts.RedField,'');
   MDIniFile.AParameter('fields','BlueField',dbOpts.BlueField,'');
   MDIniFile.AParameter('fields','GreenField',dbOpts.GreenField,'');
   MDIniFile.AParameter('fields','FloatColorField',dbOpts.FloatColorField,'');
   MDIniFile.AParameter('fields','SegSepField',dbOpts.SegSepField,'');
   MDIniFile.AParameter('fields','LabelField',dbOpts.LabelField,'');
   MDIniFile.AParameter('fields','SecondLabelField',dbOpts.SecondLabelField,'');
   MDIniFile.AParameter('fields','TimeField',dbOpts.TimeField,'');
   MDIniFile.AParameter('fields','DirField',dbOpts.DirField,'');
   MDIniFile.AParameter('fields','MagField',dbOpts.MagField,'');

   MDIniFile.AParameter('fields','StringColorField',dbOpts.StringColorField,'');
   MDIniFile.AParameter('fields','NumericColorField',dbOpts.NumericColorField,'');
   MDIniFile.AParameter('fields','SizeField',dbOpts.SizeField,'');

   MDIniFile.AParameter('db','LinkTableName',dbOpts.LinkTableName,'');
   MDIniFile.AParameter('db','AllIconFName',dbOpts.AllIconFName,'');
   MDIniFile.AParameter('db','ZipAToneFName',dbOpts.ZipAToneFName,'');

   if MDDef.SaveDBfilter then begin
      MDIniFile.AParameter('db','MainfFilter',dbOpts.MainFilter,'');
      MDIniFile.AParameter('db','TimeFilter',dbOpts.TimeFilter,'');
      MDIniFile.AParameter('db','GeoFilter',dbOpts.GeoFilter,'');
   end
   else begin
      dbOpts.MainFilter := '';
      dbOpts.TimeFilter := '';
      dbOpts.GeoFilter := '';
   end;

   MDIniFile.AColorParameter('db','FillColor',dbOpts.FillColor,MDDef.FillColor);
   MDIniFile.AParameter('db','SymSizeField',dbOpts.SymSizeField,'');
   MDIniFile.AParameter('db','SymbolSize',dbOpts.Symbol.Size,MDDef.DefGISSymbol.Size);

   if AreaShapeFile(ShapeFileType) then begin
      MDIniFile.AColorParameter('db','BasicColor',dbOpts.LineColor,claBlack);
      MDIniFile.AParameter('db','LineSize',dbOpts.LineWidth,1);
   end
   else begin
      MDIniFile.AColorParameter('db','BasicColor',dbOpts.LineColor,MDDef.AreaBorderColor);
      MDIniFile.AParameter('db','LineSize',dbOpts.LineWidth,2);
   end;

   if (IniWhat = iniInit) and (TheMapOwner <> Nil) and AreaShapeFile(ShapeFileType) and (ValidDEM(TheMapOwner.MapDraw.DEMonMap) or ValidSatImage(TheMapOwner.MapDraw.SatonMap)) then begin
      dbOpts.Opacity := 50;
   end
   else MDIniFile.AParameter('db','Opacity',dbOpts.Opacity,100);

   MDIniFile.AParameter('db','IconScalingFactor',dbOpts.IconScalingFactor,100);

   MDIniFile.AParameter('db','WindPixelSpace',dbOpts.WindPixelSpace,45);
   MDIniFile.AParameter('db','VectorThinFactor ',dbOpts.VectorThinFactor,1);
   MDIniFile.AParameter('db','dbColorMode',dbOpts.dbColorMode,0);
   MDIniFile.AParameter('db','DBAutoShow',dbOpts.DBAutoShow,1);
   MDIniFile.AParameter('db','HideTable',dbOpts.HideTable,false);
   MDIniFile.AParameter('db','WindAutoSpace',dbOpts.WindAutoSpace,false);
   MDIniFile.AParameter('db','VectorsByPolar',dbOpts.VectorsByPolar,false);
   MDIniFile.AParameter('db','LabelDBPlots',dbOpts.LabelDBPlots,false);
   MDIniFile.AParameter('db','ConnectUnderPoints',dbOpts.ConnectUnderPoints,false);
   MDIniFile.AParameter('db','Grayscale',dbOpts.Grayscale,false);
   MDIniFile.AParameter('db','ReverseColorTable',dbOpts.ReverseColorTable,false);
   MDIniFile.AParameter('db','Subdue',dbOpts.Subdue,false);
   MDIniFile.AParameter('db','ShowBuffers',dbOpts.ShowBuffers,false);
   MDIniFile.AParameter('db','SymbolsWithBuffers',dbOpts.SymbolsWithBuffers,false);
   MDIniFile.AParameter('db','ConstantSymSize',dbOpts.ConstantSymSize,true);
   MDIniFile.AParameter('db','GISVectorsByMaxSpeed',dbOpts.GISVectorsByMaxSpeed,true);
   MDIniFile.AParameter('db','OutlineProportionalPolygons',dbOpts.OutlinePolygons,false);
   MDIniFile.AParameter('db','ScaledSymMinSize',dbOpts.ScaledSymMinSize, 5);
   MDIniFile.AParameter('db','ScaledSymMaxSize',dbOpts.ScaledSymMaxSize, 50);
   MDIniFile.AParameterFloat('db','ColorMin',dbOpts.ColorMin,99);
   MDIniFile.AParameterFloat('db','ColorMax',dbOpts.ColorMax,-99);
   MDIniFile.AParameterFloat('db','SizeMin',dbOpts.SizeMin,99);
   MDIniFile.AParameterFloat('db','SizeMax',dbOpts.SizeMax,-99);
   MDIniFile.AParameterShortFloat('db','VectorLineMult',dbOpts.VectorLineMult, 10);
   MDIniFile.AParameterShortFloat('db','CatPCforLegend',dbOpts.CatPCforLegend, 2);

   MDIniFile.ASymbol('db','Symbol',dbOpts.Symbol,FilledBox,claRed,3);

   MDIniFile.AParameter('db','DBColorPaletteName',dbOpts.DBColorPaletteName,'Dark Red to Blue, 12 steps');
   MDIniFile.AParameter('db','DBColorScheme',dbOpts.DBColorScheme,1);

   MDIniFile.AParameter('db','DBLegendLocation.DrawItem',dbOpts.DBLegendLocation.DrawItem,false);
   MDIniFile.AParameter('db','DBLegendLocation.LegendSize',dbOpts.DBLegendLocation.LegendSize,1);
   MDIniFile.AParameter('db','DBLegendLocation.MapPosition',dbOpts.DBLegendLocation.MapPosition,lpNEmap);
   MDIniFile.AParameter('db','DBLegendLocation.HorizontalLegend',dbOpts.DBLegendLocation.HorizontalLegend,true);

   {$IfDef VCL}
      if (IniWhat = iniWrite) then MDIniFile.IniFile.WriteInteger('db','AreaSymbolFill',ord(dbOpts.AreaSymbolFill));
      if (IniWhat = iniRead) then dbOpts.AreaSymbolFill := tBrushStyle(MDIniFile.IniFile.ReadInteger('db','dbOpts.AreaSymbolFilll',ord(bsClear)));
      if (iniWhat = iniInit) then dbOpts.AreaSymbolFill := bsClear;
   {$EndIf}

   {$IfDef Android}
   {$Else}
      MDiniFile.CloseMDiniFile;
   {$EndIf}

   {$IfDef RecordINIfiles} WriteLineToDebugFile('ProcessIniFile out'); {$EndIf}
end;



function NumOpenDB : integer;
var
   i : integer;
begin
   Result := 0;
   for i := 1 to MaxDataBase do  begin
      if (GISdb[i] <> nil) then Result := result + 1;
   end;
end;


procedure CloseAndNilNumberedDB(var i : integer);
var
  ClosingDB : integer;
begin
   If ValidDB(i) then begin
      {$IfDef RecordCloseDB} WriteLineToDebugFile('CloseAndNilNumberedDB: ' + IntToStr(i) + '  '  + GISDB[i].dbName); {$EndIf}
      ClosingDB := i;
      GISdb[i].CloseDataBase;
      if (i <> 0) then GISdb[i] := Nil;
      i := 0;
      {$IfDef RecordCloseDB} WriteLineToDebugFile('CloseAndNilNumberedDB out ' + IntToStr(i)); {$EndIf}
   end;
end;


function OpenNumberedGISDataBase(var GISNum : integer; fName : PathStr; ShowTable : boolean = false; LocalMapOnly : boolean = false; MapOwner : tMapForm = nil) : boolean;

      function OpenGISDataBase(GISNum : integer; WhatGIS : shortstring; fName : PathStr; ShowTable : boolean = false; MapOwner : tMapForm = nil) : boolean;
      begin
         {$If Defined(RecordFullOpenDB) or Defined(LogModuleCreate)} if (UpperCase(ExtractFilePath(fName)) <> UpperCase(MDTempDir)) then WriteLineToDebugFile('OpenGISDataBase in, GISNum=' + IntToStr(GISnum) + '   ' + fName); {$EndIf}
         Result := false;
         GISdb[GISNum] := TGISDataBaseModule.Create(Application);
         {$If Defined(LogModuleCreate)} WriteLineToDebugFile('OpenGISDataBase fail, GISNum=' + IntToStr(GISnum) + '   ' + fName); {$EndIf}
            GISdb[GISNum].DBNumber := GISNum;
            if GISdb[GISNum].InitializeTheTable(WhatGIS,fName) then begin
               {$IfDef FMX}
                  if (MapDraw <> Nil) then begin
                     GISdb[GISNum].TheMapOwner.MapDraw := MapDraw;
                     MapDraw.DBonThisMap[GISNum] := true;
                  end;
               {$EndIf}
               Result := true;
            end;
         {$IfDef RecordFullOpenDB} if (UpperCase(ExtractFilePath(fName)) <> UpperCase(MDTempDir)) then WriteLineToDebugFile('OpenGISDataBase out ' + fName); {$EndIf}
      end;


var
   i : integer;
begin
   {$IfDef RecordOpenDataBase} if (UpperCase(ExtractFilePath(fName)) <> UpperCase(MDTempDir)) then WriteLineToDebugFile('OpenNumberedGISDataBase in ' + fName); {$EndIf}
   Result := FileExists(fName) and FindOpenDataBase(GISNum);
   if Result then begin
      {$IfDef VCL}
            Result := OpenGISDataBase(GISNum,'',fName,ShowTable,MapOwner);
            if Result then begin
               {$IfDef RecordOpenDataBase}  WriteLineToDebugFile('Opened db=' + IntToStr(GISNum)); {$EndIf}
               GISdb[GISNum].ShowLocalMapOnly := LocalMapOnly;
               if (MapOwner = nil) then GISdb[GISNum].DBPlotted := false
               else begin
                  GISdb[GISNum].AssociateMap(MapOwner);
                  if (MDDef.MapLimitDB) and ValidDEM(GISdb[GISNum].TheMapOwner.MapDraw.DEMonMap) and (DEMGlb[GISdb[GISNum].TheMapOwner.MapDraw.DEMonMap].LongSizeMap < 30) then begin
                     GISdb[GISNum].LimitDBtoMapArea;
                  end;
               end;
               if ShowTable and (not GISdb[GISNum].dbOpts.HideTable) then begin
                  {$IfDef RecordOpenDataBase} if (UpperCase(ExtractFilePath(fName)) <> UpperCase(MDTempDir)) then WriteLineToDebugFile('Call DisplayTable, GISNum=' + IntToStr(GISNum)); {$EndIf}
                  GISdb[GISNum].DisplayTable;
                  for i := 0 to pred(WMDEM.MDIChildCount) do begin
                     if (WMDEM.MDIChildren[i] is Tdb_display_opts) then begin
                        (WMDEM.MDIChildren[i] as Tdb_display_opts).LabelTheButtons;
                     end;
                  end;
               end;
            end
            else begin
               {$IfDef RecordOpenDataBase}  WriteLineToDebugFile('Open failed'); {$EndIf}
               GISNum := 0;
            end;
      {$EndIf}
   end;
   {$IfDef RecordOpenDataBase} if (UpperCase(ExtractFilePath(fName)) <> UpperCase(MDTempDir)) then WriteLineToDebugFile('OpenNumberedGISDataBase ' + IntToStr(GISNum) + ' out ' + fName); {$EndIf}
end;



function TGISdataBaseModule.NumericFieldLinkPossible(fName : ShortString) : boolean;
var
   ShortName : ShortString;
begin
   ShortName := fName;
   Result := MyData.IsNumericField(fName) or (LinkedField(ShortName) and LinkTable.IsNumericField(ShortName));
end;


function TGISdataBaseModule.StringFieldLinkPossible(fName : ShortString) : boolean;
var
   ShortName : ShortString;
begin
   ShortName := fName;
   Result := MyData.IsStringField(fName) or (LinkedField(ShortName) and LinkTable.IsStringField(ShortName));
end;


procedure TGISdataBaseModule.DefineColorTable;
var
   LocalName : ShortString;
   LocalField,LinkField : boolean;
begin
   {$If Defined(RecordDefineDBColorTable) or Defined(RecordDBNumericPlot)} WriteLineToDebugFile('TGISdataBaseModule.DefineColorTable in'); {$EndIf}
   LocalField := (MyData.FieldExists(dbOpts.FloatColorField));
   LocalName := dbOpts.FloatColorField;
   if not LocalField then begin
      LinkField := ((LinkTable <> Nil) and LinkedField(LocalName) and LinkTable.FieldExists(LocalName));
      if not LinkField then exit;
   end;
   if LocalField or LinkField then begin
      if (LocalField and MyData.IsNumericField(dbOpts.FloatColorField)) or (LinkField and LinkTable.IsNumericField(LocalName)) or
           (LocalField and MyData.IsStringField(dbOpts.FloatColorField)) or (LinkField and LinkTable.IsStringField(LocalName)) then begin
        {$IfDef VCL}  DefineDBColorTable; {$EndIf}
      end;
   end;
   {$If Defined(RecordDefineDBColorTable) or Defined(RecordDBNumericPlot)} WriteLineToDebugFile('TGISdataBaseModule.DefineColorTable out'); {$EndIf}
end;


function TGISdataBaseModule.GetFullImageName(var fName : PathStr) : boolean;
begin
   Result := false;
   fName := MyData.GetFieldByNameAsString(ImageFieldNames[1]);
   if (fName <> '') then begin
      if not FileExists(fName) then fName := ExtractFilePath(dbFullName) + fname;
      Result := FileExists(fName);
   end;
end;

function TGISdataBaseModule.GetRotatedImage(var bmp : tMyBitmap; var fName : PathStr) : boolean;
begin
   bmp := nil;
   Result := GetFullImageName(fName);
   if Result then begin
      bmp := PetImage.LoadBitmapFromFile(fname);
   end;
end;


procedure InitializeDEMdbs;
var
   i : integer;
begin
   {$IfDef RecordOpenDataNumberedBase} WriteLineToDebugFile('InitializeDEMdbs'); {$EndIf}
   for i := 1 to MaxDataBase do begin
      {$IfDef RecordOpenDataNumberedBase} WriteLineToDebugFile('nil=' + IntToStr(i)); {$EndIf}
      GISdb[i] := Nil;
      DBPlotOrder[i] := i;
      DBPlotNow[i] := true;
   end;
   HighLightDBOnWorld := true;

   {$IfDef ExPLSS}
   {$Else}
      for I := 1 to MaxPLSS do PLSS[i] := Nil;
   {$EndIf}

   {$IfDef NoDBMaps}
   {$Else}
      MapForShapeDigitizing := Nil;
   {$EndIf}

   NeedCentroid := false;
   ClimateStationDB := 0;
   KoppenGridDB := 0;
   WindsDB := 0;
   PiratesDB := 0;
end;


{$IfDef ExSat}
{$Else}
function TGISdataBaseModule.GridStatsName(CurDEM : integer; Ext : ExtStr) : PathStr;
begin
   Result := ExtractFilePath(dbFullName) + 'grid_stats\';
   SafeMakeDir(Result);
   Result := Result +  ptTrim(DEMGlb[CurDEM].AreaName) + '_classes' + Ext;
end;
{$EndIf}


{$IfDef RecordDataSaveStatus}
procedure TGISdataBaseModule.WriteRecordDataSaveStatus(WhereAreWe : ShortString);
begin
end;
{$EndIf}


procedure TGISdataBaseModule.PointsForLineAreaDB(AddFirst,AddLast,AddTurns : boolean; DistApart : float64 = -99);
var
   i,NPts : integer;
   BBox : sfBoundBox;
   Lat,Long,Dist,
   xRange,yrange : float64;
   Results,Route : tStringList;
   aName : shortString;
   fName : PathStr;

   procedure AddPoint;
   begin
      Results.Add(RealToString(Lat,-12,-8) + ',' + RealToString(Long,-12,-8) + ',' + aName + ',' + IntToStr(i));
   end;


begin
   {$IfDef RecordPointInArea} WriteLineToDebugFile('TGISdataBaseModule.PointsForLineAreaDB in'); {$EndIf}
   if ((DistApart > 0) or MyData.FieldExists('NPTS')) then begin
      Results := tStringList.Create;
      Results.Add('LAT,LONG,NAME,' + RecNoFName);
      MyData.First;
      aName := '';
      i := 0;
      while not MyData.EOF do begin
         if (DistApart < 0) then NPts := MyData.GetFieldByNameAsInteger('NPTS')
         else begin
            Dist := aShapeFile.LineLength(MyData.RecNo);
            NPts := trunc(Dist / DistApart);
         end;
         {$IfDef RecordPointInArea} WriteLineToDebugFile('  NPTs=' + IntToStr(NPts)); {$EndIf}
         if (Npts > 0) then begin
            if MyData.FieldExists('NAME') then aName := MyData.GetFieldByNameAsString('NAME');
            if AreaShapeFile(ShapeFileType) then begin
               BBox := MyData.GetRecordBoundingBox;
               XRange := BBox.xMax - BBox.xMin;
               yRange := BBox.YMax - BBox.YMin;
               for i := 1 to NPts do begin
                  repeat
                     Lat := BBox.YMin + Random * yRange;
                     Long := BBox.XMin + Random * xRange;
                  until aShapeFile.PointInRecord(MyData.RecNo,Lat,Long);
                  AddPoint;
               end;
            end
            else begin
               Route := tStringList.Create;
               Dist := aShapeFile.LineLength(MyData.RecNo);
               aShapeFile.GetLineCoords(MyData.RecNo);
               for i := 0 to pred(aShapeFile.CurrentPolyLineHeader.NumPoints) do begin
                  Route.Add(RealToString(aShapeFile.CurrentLineCoords^[i].Lat,-12,8) + RealToString(aShapeFile.CurrentLineCoords^[i].Long,-14,8));
               end;
               if (DistApart > 0) then Dist := DistApart
               else Dist := Dist/succ(Npts);

               ExpandRoute(0,Route,Dist,AddFirst,AddLast,AddTurns);
               {$IfDef RecordPointInArea} WriteLineToDebugFile('   Route.Count=' + IntToStr(Route.Count)); {$EndIf}
               for i := 1 to Route.Count do begin
                  {$IfDef RecordPointInArea} WriteLineToDebugFile('  ' + IntToStr(i)); {$EndIf}
                  ReadCoordsLatLongFromStreamProfileResults(Route,pred(i),Lat,Long);
                  AddPoint;
               end;
               Route.Free;
            end;
         end;
         MyData.Next;
      end;
      fName := ExtractFilePath(DBFullName);
      if AreaShapeFile(ShapeFileType) then fName := '_pts_in_polygons_'
      else fName := '_pts_along_lines_';
      fName := Petmar.NextFileNumber(ExtractFilePath(DBFullName),ExtractFileNameNoExt(DBFullName) + fName,'.csv');
      {$IfDef VCL}
         theMapOwner.StringListtoLoadedDatabase(Results,fName);
      {$Else}
         Results.SaveToFile(fName);
         Results.Free;
      {$EndIf}
   end
   else begin
      {$IfDef VCL} MessageToContinue('Requires field NPTS'); {$EndIf}
   end;
end;



procedure TGISdataBaseModule.ExtractPointsFromLineAndAddXYZ;
var
   i,NPts : integer;
   LastLat,LastLong,Lat,Long,Distance,Bearing,dLat,dLong,PointSep : float64;
   fName : PathStr;
   Results : tStringList;
  j,RecLong : Integer;

      procedure AddPoint;
      var
         z : float32;
      begin
         if DEMGlb[theMapOwner.MapDraw.DEMonMap].GetElevFromLatLongDegree(Lat,Long,z) then begin
            inc(RecLong);
            Results.Add(RealToString(Lat,-12,8) + ',' + RealToString(Long,-14,8) + ',' + RealToString(z,-8,-2) + ',' + IntToStr(RecLong));
         end;
      end;


begin
   PointSep := 1;
   Results := tStringList.Create;
   Results.Add('LAT,LONG,Z,' + RecNoFName);
   MyData.First;
   RecLong := 0;
   while not MyData.EOF do begin
      aShapeFile.GetLineCoords(MyData.RecNo);
      Lat := aShapeFile.CurrentLineCoords^[0].Lat;
      Long := aShapeFile.CurrentLineCoords^[0].Long;
      AddPoint;
      for i := 1 to pred(aShapeFile.CurrentPolyLineHeader.NumPoints) do begin
         LastLat := Lat;
         LastLong := Long;
         Lat := aShapeFile.CurrentLineCoords^[i].Lat;
         Long := aShapeFile.CurrentLineCoords^[i].Long;
         VincentyCalculateDistanceBearing(Lat,Long,LastLat,LastLong,Distance,Bearing);
         NPts := round(Distance/PointSep);
         if NPts = 0 then NPts := 1;

         dLat := (Lat - LastLat) / Npts;
         dLong := (Long - LastLong) / Npts;
         for j := 1 to Npts do begin
            Lat := LastLat + j * dLat;
            Long := LastLong + j * dLong;
            AddPoint;
         end;
      end;
      MyData.Next;
   end;

   fName := '_line_XYZ_points_';
   fName := Petmar.NextFileNumber(ExtractFilePath(DBFullName),ExtractFileNameNoExt(DBFullName) + fName,'.dbf');
   theMapOwner.StringListtoLoadedDatabase(Results,fName);
end;


function TGISdataBaseModule.IniFileName : PathStr;
var
   fName : PathStr;
begin
   {$IfDef VCL}
   Result := DBAuxDir + ExtractFileName(dbFullName) + '.ini';
   if not FileExists(Result) then begin
      fName :=  ExtractFileName(dbFullName) + '.ini';
      if FileExists(fName) then begin
         CopyFile(fName,Result);
         DeleteFileIfExists(fName);
      end;
   end;
   {$EndIf}
end;

{$IfDef ExSaveDBstatus}
{$Else}

      procedure TGISdataBaseModule.SaveDataBaseStatus;
      var
         fName : PathStr;
      begin
         fName := IniFileName;
         if ItsTigerShapeFile or ItsOSMShapeFile or DoNotTrackDBSettings then begin
            SysUtils.DeleteFile(fName)
         end
         else begin
            if (MDDef.SaveDBStatus) and (ExtractFilePath(dbFullName) <> MDTempDir) then begin
               {$IfDef RecordDataSaveStatus} WriteLineToDebugFile('SaveDataBaseStatus start ' +dbName); {$EndIf}
               ProcessDBIniFile(iniWrite,'',fName);
               {$IfDef RecordDataSaveStatus} WriteLineToDebugFile('SaveDataBaseStatus done ' + dbName); {$EndIf}
               {$IfDef RecordSymbolColor} WriteLineToDebugFile(dbName + ' SaveDataBaseStatus, color = ' + RGBString(dbOpts.Symbol.Color.rgbtRed,dbOpts.Symbol.Color.rgbtGreen,dbOpts.Symbol.Color.rgbtBlue)); {$EndIf}
            end;
         end;
      end;


      procedure TGISdataBaseModule.RestoreDataBaseStatus;
      var
         fname2 : PathStr;

               procedure CheckField(var aName : ShortString);
               var
                  ShortName : ShortString;
               begin
                  ShortName := aName;
                  if (not MyData.FieldExists(aName)) and (not LinkedField(ShortName)) then aName := '';
               end;

         procedure SetSymbols;
         begin
             if (DBNumber = 1) then begin
                dbOpts.Symbol.DrawingSymbol := MDDef.DefGISSymbol.DrawingSymbol;
             end
             else begin
                {$IfDef VCL}
                   dbOpts.FillColor := ConvertTColorToPlatformColor(WinGraphColors[DBNumber mod 15]);
                   dbOpts.LineColor := ConvertTColorToPlatformColor(WinGraphColors[DBNumber mod 15]);
                   dbOpts.Symbol.Color := ConvertTColorToPlatformColor(WinGraphColors[DBNumber mod 15]);
                {$EndIf}
                dbOpts.Symbol.Size := 3;

                case DBNumber of
                   1 : dbOpts.Symbol.DrawingSymbol := FilledBox;
                   2 : dbOpts.Symbol.DrawingSymbol := FilledCircle;
                   3 : dbOpts.Symbol.DrawingSymbol := FilledDiamond;
                   4 : dbOpts.Symbol.DrawingSymbol := FilledDownTri;
                   5 : dbOpts.Symbol.DrawingSymbol := FilledUpTri;
                   6 : dbOpts.Symbol.DrawingSymbol := Box;
                   else dbOpts.Symbol.DrawingSymbol := FilledBox;
                end;
             end;
             {$IfDef RecordSymbolColor} WriteLineToDebugFile('RestoreDataBaseStatus SetSymbols, color = ' + RGBString(dbOpts.Symbol.Color.rgbtRed,dbOpts.Symbol.Color.rgbtGreen,dbOpts.Symbol.Color.rgbtBlue)); {$EndIf}
          end;

      begin
         {$IfDef RecordDataSaveStatus} WriteLineToDebugFile('TGISDataBase.RestoreDataBaseStatus in ' + dbFullName); {$EndIf}

         fName2 := IniFileName;
         DBNumber := NumOpenDB;

          if (ShapeFileType = 11) then begin
             dbOpts.DBAutoShow := dbasZValues;
             dbOpts.ZColorMin := Ashapefile.MainFileHeader.BoundBoxZMin;
             dbOpts.ZColorMax := Ashapefile.MainFileHeader.BoundBoxZMax;
          end;

         if (not MDDef.SaveDBStatus) or (not FileExists(fName2)) or (ExtractFilePath(dbFullName) = MDTempDir) then begin
            {$IfDef RecordDataSaveStatus} WriteLineToDebugFile('iniInit'); {$EndIf}
            ProcessDBIniFile(iniInit,'',fName2);
            dbOpts.CatPCforLegend := MDDef.DefCatPCforLegend;
            SetSymbols;
         end
         else begin
            {$IfDef RecordDataSaveStatus} WriteLineToDebugFile('iniRead'); {$EndIf}
            ProcessDBIniFile(iniRead,'',fName2);
            {$IfDef RecordSymbolColor} WriteLineToDebugFile('RestoreDataBaseStatus iniRead, color = ' + RGBString(dbOpts.Symbol.Color.rgbtRed,dbOpts.Symbol.Color.rgbtGreen,dbOpts.Symbol.Color.rgbtBlue)); {$EndIf}

            if (dbOpts.Opacity < 10) then dbOpts.Opacity := 90;
            if (dbOpts.IconScalingFactor < 5) then dbOpts.IconScalingFactor := 100;
            if (dbOpts.Symbol.Size = 0) then dbOpts.Symbol.Size := 1;
            if (dbOpts.DBAutoShow = dbasTerrainFabric) and (not MyData.FieldExists('S1S2')) then dbOpts.DBAutoShow := dbasDefault;

            if (dbOpts.VectorLineMult < 0.00001) then dbOpts.VectorLineMult := 5;
            if (dbOpts.VectorThinFactor = 0) then dbOpts.VectorThinFactor := 1;
            if (dbOpts.MainFilter <> '') then MyData.ApplyFilter(dbOpts.MainFilter);
             CheckField(dbOpts.XField);
             CheckField(dbOpts.YField);
             CheckField(dbOpts.ZField);
             CheckField(dbOpts.IconField);
             CheckField(dbOpts.RedField);
             CheckField(dbOpts.BlueField);
             CheckField(dbOpts.GreenField);
             CheckField(dbOpts.FloatColorField);
             CheckField(dbOpts.LabelField);
             CheckField(dbOpts.SecondLabelField);
             CheckField(dbOpts.QFField1);
             CheckField(dbOpts.QFField2);
             CheckField(dbOpts.QFField3);
         end;
         SetUpRangeTable(dbFullName,RangeTable);
         {$IfDef RecordFont} WriteLineToDebugFile('SetDefaultdbOpts out File=' + dbName+ '  Gis font 1: ' + MyFontToString(dbOpts.GisLabelFont1) + '  Gis font 2: ' + MyFontToString(dbOpts.GisLabelFont2)); {$EndIf}
         {$IfDef RecordDataSaveStatus} WriteLineToDebugFile('SetDefaultdbOpts done with Display Mode=' + IntToStr(dbOpts.DBAutoShow)); {$EndIf}
      end;
{$EndIf}


function tGISdataBaseModule.AssembleGISFilter : AnsiString;
begin
   {$IfDef RecordQueryGeoBox}
      WriteLineToDebugFile('tGISdataBaseModule.AssembleFilter in, ' + dbName + '  Main filter: ' + dbOpts.MainFilter);
      WriteLineToDebugFile('  Time filter: ' + dbOpts.TimeFilter + '  Geo filter: ' + dbOpts.GeoFilter);
   {$EndIf}

   Result := dbOpts.MainFilter;
   if (dbOpts.TimeFilter <> '') then Result := PetDBUtils.AddAndIfNeeded(Result) + dbOpts.TimeFilter;
   if (dbOpts.GeoFilter <> '') then Result := PetDBUtils.AddAndIfNeeded(Result) + dbOpts.GeoFilter;
   MyData.ApplyFilter(Result);

   {$IfDef RecordQueryGeoBox} WriteLineToDebugFile('tGISdataBaseModule.AssembleFilter out, Final filter: ' + Result); {$EndIf}
end;


procedure TGISdataBaseModule.SetUpRangeTable(BaseName : PathStr; var Table : tMyData; ForceNew : boolean = false);
var
   fName : PathStr;
begin
   if MDDef.TrackDatabaseRanges and (not DoNotTrackDBSettings) then begin
      {$IfDef RecordRangeTable} WriteLineToDebugFile('TGISdataBaseModule.SetUpRangeTable for ' + ExtractFileName(BaseName)); {$EndIf}
      fName := DBAuxDir + 'range_' + ExtractFileName(BaseName);
      fName := ChangeFileExt(fName,DefaultDBExt);
      if (not FileExists(fName)) or ForceNew then begin
         MakeFieldRangeTable(fName);
         {$IfDef RecordRangeTable} WriteLineToDebugFile('Made new range table ' + fName); {$EndIf}
      end;
      Table := Nil;
      Table := tMyData.Create(fName);
      {$IfDef RecordRangeTable}
         if (Table=Nil) then WriteLineToDebugFile('TGISdataBaseModule.SetUpRangeTable failure')
         else WriteLineToDebugFile('TGISdataBaseModule.SetUpRangeTable ' + ExtractFileName(fName));
      {$EndIf}
   end
   else begin
      {$IfDef RecordRangeTable} WriteLineToDebugFile('TGISdataBaseModule.SetUpRangeTable, not tracking for ' + ExtractFileName(BaseName)); {$EndIf}
   end;
end;

procedure tGISdataBaseModule.ClearGISFilter;
begin
   MyData.ApplyFilter('');
   dbOpts.MainFilter := '';
   dbOpts.GeoFilter := '';
   dbOpts.TimeFilter := '';
end;


procedure tGISdataBaseModule.AssignSymbol(aSymbol : tFullSymbolDeclaration);
begin
   dbOpts.LineColor := aSymbol.Color;
   dbOpts.Symbol := aSymbol;
   {$IfDef VCL}
      if (DBTableF <> Nil) then RedrawLayerOnMap;
   {$EndIf}
end;


function tGISdataBaseModule.ComputeColorFromRecord(var Color : tColor) : boolean;
var
   TStr : shortString;
   Value : float64;
   i,err : integer;
begin
   if (dbOpts.dbAutoShow in [dbasColorPosNeg,dbasColorByString,dbasColorByNumeric]) then begin
     TStr := GetStringFromTableLinkPossible(dbOpts.FloatColorField);
     {$IfDef RecordDataInsideLoopPlots} WriteLineToDebugFile('TStr=' + TStr); {$EndIf}
     if dbOpts.dbAutoShow in [dbasColorPosNeg] then begin
         Val(TStr,Value,err);
         if (Abs(Value) < ZeroTol) then Color := clYellow
         else if (Value > 0) then Color := clGreen
         else Color := clRed;
         Result := true;
         exit;
      end
   end
   else if dbOpts.dbAutoShow in [dbasMultiFieldRGB] then begin
      {$IfDef VCL}
      Color := RGBColorFromThreeNumericFields;
      {$EndIf}
      Result := true;
      exit;
   end
   else TStr := '0';
   Result := (TStr <> '');
   if Result then begin
     if (dbOpts.dbAutoShow in [dbasColorByNumeric]) then Val(TStr,Value,err);
     {$IfDef RecordDataInsideLoopPlots} WriteLineToDebugFile('get color, mode=' + IntToStr(ord(MDDef.GISColorSource))); {$EndIf}
     if (dbOpts.DBAutoShow = dbasColorField) then Color := MyData.TColorFromTable
     else if (dbOpts.dbAutoShow in [dbasColorByNumeric]) then Color := SelectedColorSchemeColorFunct(dbOpts.DBColorScheme,ColorDefTable,Value,dbOpts.ColorMin,dbOpts.ColorMax)
     else if (dbOpts.dbAutoShow in [dbasColorByString]) then begin
           TStr := MyData.GetFieldByNameAsString(dbOpts.FloatColorField);
           for i := 0 to pred(StringDataThere.Count) do if (StringDataThere.Strings[i] = TStr) then break;
           Color := SelectedColorSchemeColorFunct(dbOpts.DBColorScheme,ColorDefTable,i,0,StringDataThere.Count);
     end
     else Color := clBlack;
   end;
end;


procedure AdjustGazFeatureName(var FeatureName : ShortString);
begin
   if Copy(FeatureName,length(FeatureName)-6,7) = ', Mount' then begin
      Delete(FeatureName,length(FeatureName)-6,7);
      FeatureName := 'Mount ' + FeatureName;
   end;
   if Copy(FeatureName,length(FeatureName)-4,5) = ', The' then begin
      Delete(FeatureName,length(FeatureName)-4,5);
      FeatureName := 'The ' + FeatureName;
   end;
end;


procedure TGISdataBaseModule.SaveFilterStatus(RemoveFilter : boolean = false);
begin
   DBOldFilter := MyData.Filter;
   DBWasFiltered := MyData.Filtered;
   if RemoveFilter then begin
      ClearGISFilter;
   end;
end;

procedure TGISdataBaseModule.RestoreFilterStatus;
begin
   if DBWasFiltered then begin
      MyData.ApplyFilter(DBOldFilter);
   end
   else begin
      ClearGISFilter;
   end;
end;


procedure TGISdataBaseModule.LoadPointCoords;
var
   Lat,Long : float64;
begin
    New(PointsInMemory);
    NumPointsInMemory := 0;
    MyData.First;
    while not MyData.EOF do begin
       if ValidLatLongFromTable(Lat,Long) then begin
          PointsInMemory^[NumPointsInMemory].Lat := Lat;
          PointsInMemory^[NumPointsInMemory].Long := Long;
       end
       else begin
          PointsInMemory^[NumPointsInMemory].Lat := -999;
          PointsInMemory^[NumPointsInMemory].Long := -999;
       end;
       inc(NumPointsInMemory);
       MyData.Next;
    end;
end;

procedure TGISdataBaseModule.DisposePointCoords;
begin
   Dispose(PointsInMemory);
   NumPointsInMemory := 0;
end;


{$IfDef VCL}
procedure TGISdataBaseModule.SavePointShapeFile(DoSHP : boolean = true; fName : PathStr = '');
var
   XYZ,s3D : boolean;
   NewShapeType,Done : integer;
   zf : ShortString;
   x,y,z : float64;
   ShapeFileCreator : tShapeFileCreation;
begin
   if (LatFieldName <> '') and (LongFieldName <> '') then begin
      if MyData.Filtered and (not AnswerIsYes('Entire (unfiltered) DB must be saved; proceed')) then exit;
      ShowHourglassCursor;
      EmpSource.Enabled := false;
      SaveFilterStatus(true);
      if (fName = '') then fName := dbFullName;
      XYZ := (MyData.FieldExists('X') and MyData.FieldExists('Y') and MyData.FieldExists('Z'));
      s3D := MyData.FieldExists('Z') or MyData.FieldExists('ELEV') or MyData.FieldExists('ELEVATION')or MyData.FieldExists('DEPTH') or MyData.FieldExists('DEPTH_M');
      if s3D then begin
         if MyData.FieldExists('Z') then zf := 'Z' else
         if MyData.FieldExists('ELEV') then zf := 'ELEV' else
         if MyData.FieldExists('DEPTH') then zf := 'DEPTH' else
         if MyData.FieldExists('DEPTH_M') then zf := 'DEPTH_M'
         else zf := 'ELEVATION';
      end;
      
      if XYZ or s3D then NewShapeType := 11 else NewShapeType := 1;

      ShapeFileCreator := tShapeFileCreation.Create(WGS84DatumConstants,fName,false,NewShapeType,DoSHP);
      MyData.First;
      ShapeFileCreator.RecsInShapeStream := 0;
      StartProgress('Create shp');
      Done := 0;
      while not MyData.eof do begin
         inc(Done);
         if (Done mod 500 = 0) then begin
            UpdateProgressBar(Done/MyData.RecordCount);
            EmpSource.Enabled := false;
         end;

         inc(ShapeFileCreator.RecsInShapeStream);
         if XYZ then begin
            X := MyData.GetFieldByNameAsFloat('X');
            Y := MyData.GetFieldByNameAsFloat('Y');
            Z := MyData.GetFieldByNameAsFloat('Z');
            ShapeFileCreator.ProcessPointForShapeFile(y,x,z);
         end
         else begin
            Y := MyData.GetFieldByNameAsFloat(LatFieldName);
            X := MyData.GetFieldByNameAsFloat(LongFieldName);
            LongitudeAngleInRange(x);
            if s3D then begin
               Z := MyData.GetFieldByNameAsFloat(zf);
               ShapeFileCreator.ProcessPointForShapeFile(y,x,z);
            end
            else ShapeFileCreator.ProcessPointForShapeFile(y,x);
         end;
         MyData.Next;
      end;
      dbBoundBox := ShapeFileCreator.glMainFileHeader.BoundBox;
      ShapeFileCreator.CloseShapeFiles;
      {$IfDef MSWindows}
      if (not XYZFile) and DoSHP then AddProjectionFile(fName);
      {$EndIf}
      RestoreFilterStatus;
      EmpSource.Enabled := true;
      ShowDefaultCursor;
      EndProgress;
   end;
end;
{$EndIf}


procedure TGISdataBaseModule.MergeDataBases(FileNames : tStringList);
var
   i,j,Max  : integer;
   MergingTable : tMyData;
   fName        : PathStr;
   fN           : ShortString;
   TStr         : ShortString;
begin
    {$IfDef RecordMergeDB} WriteLineToDebugFile('TGISDataBase.MergeDataBases in'); {$EndIf}
    StartProgress('Merge');
    ShowHourglassCursor;
    for j := 0 to pred(FileNames.Count) do begin
       EmpSource.Enabled := false;
       UpdateProgressBar(j/FileNames.Count);
       fName := FileNames.Strings[j];
       {$IfDef RecordMergeDB} WriteLineToDebugFile(fName); {$EndIf}
       if (upperCase(fName) <> UpperCase(MyData.TableName)) then begin
          MergingTable := tMyData.Create(fName);
          for i := 0 to pred(MyData.FieldCount) do begin
             fn := MyData.GetFieldName(i);
             if (MergingTable.FieldExists(fn)) then begin
                Max := MergingTable.GetFieldLength(fn);
                if MyData.GetFieldLength(fn) < Max then begin
                   MyData.TrimField(fn,Max);
                end;
             end;
          end;
          while not MergingTable.EOF do begin
               EmpSource.Enabled := false;
               MyData.Insert;
               //ApplicationProcessMessages;
               //EmpSource.Enabled := false;
               for i := 0 to pred(MyData.FieldCount) do begin
                  fn := MyData.GetFieldName(i);
                  if (MergingTable.FieldExists(fn)) then begin
                     TStr := MergingTable.GetFieldByNameAsString(fn);
                     MyData.SetFieldByNameAsString(fn, TStr);
                  end;
               end;
               MyData.Post;
               MergingTable.Next;
           end;
           MergingTable.Destroy;
       end;
    end;
    //EmpSource.Enabled := true;
    //EndProgress;
    ShowStatus;
    RedrawLayerOnMap;
end;


procedure TGISdataBaseModule.TimeFieldsToDecYears;
var
   i,rc,Year,Month,Day,Hour,Minute,ajd,YearLong : integer;
   MonthOnly : boolean;
begin
  //with MyData do begin
      AddFieldToDataBase(ftFloat,'DEC_YEAR',12,6);
      MonthOnly := not MyData.FieldExists('DAY');
      MyData.First;
      i := 0;
      rc := MyData.RecordCount;
      StartProgress('Dec year');
      while not EOF do begin
         EmpSource.Enabled := false;
         inc(i);
         {$IfDef VCL} if (i mod 500 = 0) then UpdateProgressBar(i/rc); {$EndIf}

         MyData.Edit;
         Year := MyData.GetFieldByNameAsInteger('YEAR');
         Month := MyData.GetFieldByNameAsInteger(MonthFieldName);
         if MonthOnly then begin
            MyData.SetFieldByNameAsFloat('DEC_YEAR',Year + (Month - 0.5) / 12);
         end
         else begin
            Day := MyData.GetFieldByNameAsInteger('DAY');
            Hour := 12;
            Minute := 30;
            if MyData.FieldExists('HOUR') then MyData.CarefullyGetFieldByNameAsInteger('HOUR',Hour);
            if MyData.FieldExists('MINUTE') then MyData.CarefullyGetFieldByNameAsInteger('MINUTE',Minute);
            ajd := pred(AnnualJulianDay(Year,Month,Day));
            yearLong := AnnualJulianDay(Year,12,31);
            MyData.SetFieldByNameAsFloat('DEC_YEAR',Year + (ajd + Hour / 24 + Minute / 24 / 60) / YearLong);
         end;
         MyData.Next;
      end;
      ShowStatus;
  // end;
end;


procedure TGISdataBaseModule.SplitDateField(Format : tDateField);
var
   tStr : ANSIString;
   Year,i : integer;
   SepChar : Ansichar;
   DateField : shortstring;
begin
   AddFieldToDataBase(ftInteger,'YEAR',4,0);
   AddFieldToDataBase(ftInteger,MonthFieldName,2,0);
   AddFieldToDataBase(ftInteger,'DAY',2,0);
   EmpSource.Enabled := false;
   if MyData.FieldExists('DATE_STR') then DateField := 'DATE_STR'
   else if MyData.FieldExists('DATE') then DateField := 'DATE'
   else if MyData.FieldExists('DATE_LABEL') then DateField := 'DATE_LABEL'
   else if MyData.FieldExists('DATEOFOCC') then DateField := 'DATEOFOCC'
   else begin
      {$IfDef VCL}
      DateField := PickField('Field for date',[ftString,ftDate]);
      {$EndIf}
   end;

   MyData.First;
   TStr := MyData.GetFieldByNameAsString(DateField);
   if StrUtils.AnsiContainsText(TStr,'/') then SepChar := '/'
   else if StrUtils.AnsiContainsText(TStr,'-') then SepChar := '-';

   i := 0;
   {$IfDef VCL} StartProgress('Year'); {$EndIf}
   While not MyData.eof do begin
      inc(i);
      {$IfDef VCL}
         if (i Mod 1000 = 0) then begin
            UpDateProgressBar(i/MyData.RecordCount);
            EmpSource.Enabled := false;
         end;
      {$EndIf}

      TStr := MyData.GetFieldByNameAsString(DateField);
      if (TStr <> '') then begin
         MyData.Edit;
         if Format in [dfYMDslash,dfMDYSlash] then begin
             if (Format = dfMDYSlash) then begin
                MyData.SetFieldByNameAsString(MonthFieldName,Petmar_types.BeforeSpecifiedCharacterANSI(TStr,'/',true,true));
                MyData.SetFieldByNameAsString('DAY',Petmar_types.BeforeSpecifiedCharacterANSI(TStr,'/',true,true));
                Year := StrToInt(TStr);
             end
             else begin
                Year := StrToInt(Petmar_types.BeforeSpecifiedCharacterANSI(TStr,'/',true,true));
                MyData.SetFieldByNameAsString(MonthFieldName,Petmar_types.BeforeSpecifiedCharacterANSI(TStr,'/',true,true));
                MyData.SetFieldByNameAsString('DAY',TStr);
             end;

             if (Year < 100) then begin
                if (Year < 20) then Year := Year + 2000
                else Year := Year + 1900;
             end;
             MyData.SetFieldByNameAsInteger('YEAR',Year);
         end
         else begin
             MyData.SetFieldByNameAsInteger('YEAR',StrToInt(Copy(TStr,1,4)));
             MyData.SetFieldByNameAsInteger(MonthFieldName,StrToInt(Copy(TStr,5,2)));
             MyData.SetFieldByNameAsInteger('DAY',StrToInt(Copy(TStr,7,2)));
         end;
      end;
      MyData.Next;
   end;
   ShowStatus;
end;


procedure TGISdataBaseModule.ClearFieldRange(aField : shortstring);
begin
   if MDDef.TrackDatabaseRanges and (not DoNotTrackDBSettings) and (RangeTable <> Nil) then begin
      {$IfDef RecordRangeTable} WriteLineToDebugFile('TGISdataBaseModule.ClearFieldRange for ' + aField); {$EndIf}
      RangeTable.ApplyFilter('FIELD_NAME=' + QuotedStr(aField));
      if (RangeTable.RecordCount = 1) then begin
         RangeTable.Edit;
         RangeTable.Delete;
         {$IfDef RecordRangeTable} WriteLineToDebugFile('TGISdataBaseModule.ClearFieldRange deleted ' + aField); {$EndIf}
      end
      else begin
         {$IfDef RecordRangeTable} WriteLineToDebugFile('TGISdataBaseModule.ClearFieldRange could not find ' + aField); {$EndIf}
      end;
   end;
end;


procedure TGISdataBaseModule.DBFieldUniqueEntries(FieldName : shortstring; var FieldsInDB : tStringList);
      var
         Count,rc : integer;
         TStr : ShortString;


      procedure UseCorrectTable(table : tMyData);
      begin
         try
            FieldsInDB := tStringList.Create;
            Table.First;
            Count := 0;
            if (Table.RecordCount > 0) then begin
               FieldsInDB.Sorted := true;
               FieldsInDB.Duplicates := dupIgnore;
               {$IfDef VCL} if WantShowProgress then StartProgressAbortOption('Find field values: ' + FieldName); {$EndIf}
               rc := Table.RecordCount;
               while not Table.EOF do begin
                 TStr := ptTrim(Table.GetFieldByNameAsString(FieldName));
                 if (TStr <> '') then FieldsInDB.Add(TStr);
                 Table.Next;
                 inc(Count);
                 {$IfDef VCL} if WantShowProgress and ((Count mod 500) = 0) then UpdateProgressBar(Count/rc); {$EndIf}
                 if WantOut then break;
               end;
            end;
         finally
           {$IfDef VCL} if WantShowProgress then EndProgress; {$EndIf}
         end;
      end;


begin
   {$IfDef RecordUnique} WriteLineToDebugFile('TGISdataBaseModule.DBFieldUniqueEntries in'); {$EndIf}
   EmpSource.Enabled := false;
   if (LinkTable <> Nil) and LinkedField(FieldName) then UseCorrectTable(LinkTable)
   else UseCorrectTable(MyData);
end;



procedure TGISdataBaseModule.FilterDBByUseAndDisable(Used : boolean);
var
   ch : AnsiChar;
begin
   if Used then ch := 'Y' else ch := 'N';
   MyData.ApplyFilter('USE= ' + QuotedStr(ch));
   dbOpts.MainFilter := MyData.Filter;
   EmpSource.Enabled := false;
end;

procedure TGISdataBaseModule.FilterForUseField(Use: boolean);
var
   ch : AnsiChar;
begin
   if Use then ch := 'Y' else ch := 'N';
   MyData.ApplyFilter('USE=' + QuotedStr(ch));
   ShowStatus;
end;


function TGISdataBaseModule.ValidLatLongFromTable(var Lat,Long : float64) : boolean;
var
   TStr,TStr2: shortstring;
begin
   Lat := 0;
   Long := 0;
   Result := false;
   if SimplePointFile or XYZFile then begin
      if (LatFieldName = '') or (LongFieldName = '') then exit;
      TStr := MyData.GetFieldByNameAsString(LatFieldName);
      TStr2 := MyData.GetFieldByNameAsString(LongFieldName);
      if (TStr = '') or (TStr2 = '') then exit;
      Lat := SysUtils.StrToFloat(TStr);
      Long := SysUtils.StrToFloat(TStr2);
   end
   else if LineShapeFile(ShapeFileType) then begin
      aShapeFile.LineCenter(MyData.RecNo,Long,Lat);
   end
   else if AreaShapeFile(ShapeFileType) and (aShapefile <> Nil) then begin
      {$IfDef VCL}
      aShapeFile.AreaAndCentroid(TheMapOwner.MapDraw.PrimMapProj,MyData.RecNo,Lat,Long)
      {$EndIf}
   end;
   Result := (abs(Lat) > 0.00001) or (abs(Long) > 0.00001);
end;


function TGISdataBaseModule.ValidLat2Long2FromTable(var Lat,Long : float64) : boolean;
begin
   Lat := MyData.GetFieldByNameAsFloat(Lat2fName);
   Long := MyData.GetFieldByNameAsFloat(Long2fName);
   Result := (abs(Lat) > 0.00001) or (abs(Long) > 0.00001);
end;


procedure TGISdataBaseModule.WriteDisplaySymbology(TheData : tMyData);
var
  ThisShapeType : integer;
begin
   ThisShapeType := 0;
   if ItsAGroupingFile then ThisShapeType := MyData.GetFieldByNameAsInteger('SHAPE_TYPE');
   if (ThisShapeType in [1,11]) or SimplePointFile then begin
      TheData.PostPointSymbol(dbOpts.Symbol);
   end
   else if (ThisShapeType in [3,13]) or LineShapeFile(ShapeFileType) then begin
      TheData.SetFieldByNameAsInteger('LINE_WIDTH', dbOpts.LineWidth);
      TheData.SetFieldByNameAsInteger('LINE_COLOR',ConvertPlatformColorToTColor(dbOpts.LineColor));
   end
   else if (ThisShapeType in [5,15]) or AreaShapeFile(ShapeFileType) then begin
      TheData.SetFieldByNameAsInteger('FILL_PAT',ord(dbOpts.AreaSymbolFill));
      TheData.SetFieldByNameAsInteger('FILL_COLOR', ConvertPlatformColorToTColor(dbOpts.FillColor));
      TheData.SetFieldByNameAsString('ALT_FILL',ZipPatName);
      TheData.SetFieldByNameAsInteger('LINE_WIDTH', dbOpts.LineWidth);
      TheData.SetFieldByNameAsInteger('LINE_COLOR', ConvertPlatformColorToTColor(dbOpts.LineColor));
   end;
end;


procedure TGISdataBaseModule.SetColorsFromDB(TheIndex : tMyData);
begin
   if ItsAGroupingFile then ShapeFileType := MyData.GetFieldByNameAsInteger('SHAPE_TYPE');
   if (ShapeFileType in [1,11]) or SimplePointFile then begin
      TheIndex.DefinePointSymbol(dbOpts.Symbol.DrawingSymbol,dbOpts.Symbol.Size,dbOpts.Symbol.Color);
      if (aShapeFile <> Nil) then aShapeFile.Symbol := dbOpts.Symbol;
   end
   else if LineShapeFile(ShapeFileType) then begin
      TheIndex.GetLineColorAndWidth(dbOpts.LineColor,dbOpts.LineWidth);
   end
   else if AreaShapeFile(ShapeFileType) then begin
      TheIndex.GetLineColorAndWidth(dbOpts.LineColor,dbOpts.LineWidth);
      dbOpts.AreaSymbolFill := tBrushStyle(TheIndex.GetFieldByNameAsInteger('FILL_PAT'));
      dbOpts.FillColor := ConvertTColorToPlatformColor(TheIndex.GetFieldByNameAsInteger('FILL_COLOR'));
      if TheIndex.FieldExists('ALT_FILL') then ZipPatName := TheIndex.GetFieldByNameAsString('ALT_FILL')
      else ZipPatName := '';
   end;
end;


procedure UpdateShapeFileGroup;
var
   TheIndex : tMyData;
   i : integer;
   fName : PathStr;
   ch : AnsiChar;
begin
    fName := DBDir + 'groups\';
    if Petmar.GetFileFromDirectory('shape file grouping',DefaultDBMask,fName) then begin
       TheIndex := tMyData.Create(fName);
       TheIndex.First;
       while not TheIndex.EOF do begin
         for i := 1 to MaxDataBase do begin
            if (GISdb[i] <> Nil) then begin
               if TheIndex.GetFieldByNameAsString('NAME') = (GISdb[i].dbFullName) then begin
                  TheIndex.Edit;
                  GISdb[i].WriteDisplaySymbology(TheIndex);
                  if GISdb[i].dbOpts.DBAutoShow = dbasDefault then ch := 'Y' else ch := 'N';
                  TheIndex.SetFieldByNameAsString('PLOT', ch);
               end;
            end;
         end;
         TheIndex.Next;
       end;
       TheIndex.Destroy;
    end;
end;


function FindOpenDataBase(var db : integer) : boolean;
var
   i : integer;
begin
   for i := 1 to MaxDataBase do begin
      if (GISdb[i] = Nil) then begin
         Result := true;
         db := i;
         exit;
      end;
   end;
   Result := false;
   {$IfDef VCL} MessageToContinue('Too many open databases'); {$EndIf}
end;


function tGISdataBaseModule.FieldSum(FieldDesired : shortstring; ReEnable : boolean = true) : float64;
var
   z : float32;
begin
   EmpSource.Enabled := false;
   Result := 0;
   MyData.First;
   repeat
      if GetFloat32FromTableLinkPossible(FieldDesired,z) then
         Result := Result + z;
      MyData.Next;
   until MyData.EOF;
   EmpSource.Enabled := ReEnable;
end;


{$IfDef ExGeostats}
{$Else}

      function tGISdataBaseModule.SSOandaspectdiagrams(var s1s2,s2s3,Trend,RoughnessFactor : float64) : boolean;
      var
         fLoX,fHiX,fLoY,fHiY,ID : integer;
         DownDip : float64;
      begin
         fLoX := MyData.GetFieldByNameAsInteger('BOUND_XMIN');
         fHiX := MyData.GetFieldByNameAsInteger('BOUND_XMAX');
         fLoY := MyData.GetFieldByNameAsInteger('BOUND_YMIN');
         fHiY := MyData.GetFieldByNameAsInteger('BOUND_YMAX');
         ID := MyData.GetFieldByNameAsInteger('ID');
         Result := DEMGLB[TheMapOwner.MapDraw.DEMonMap].FeatureSSOComputations(DEMGLB[TheMapOwner.MapDraw.DEMonMap].SelectionMap.MapDraw.FeatureGrid,ID,fLoX,fLoY,fHiX,fHiY,s1s2,s2s3,Trend,RoughnessFactor,DownDip);
      end;

{$EndIf}



procedure tGISdataBaseModule.FindClosestRecord(Lat,Long : float64; var ClosestRecNo : integer; var MinD : float64);
label
   AllDone;
var
   Distance,Bearing,
   Lat2,Long2,jump : float64;
   i : integer;
begin
   EmpSource.Enabled := false;
   ClosestRecNo := 0;
   jump := 0.005;
   repeat
       MyData.ApplyFilter( MakePointGeoFilter(LatFieldName,LongFieldName,Lat+jump,Long-Jump,Lat-Jump,Long+Jump) );
       Jump := 2 * Jump;
   until (MyData.RecordCount > 0) or (Jump > 0.25);
   MinD := 99e38;
   for I := 1 to MyData.RecordCount do begin
      if ValidLatLongFromTable(Lat2,Long2) then begin
         VincentyCalculateDistanceBearing(Lat,Long,Lat2,Long2,Distance,Bearing);
         if (Distance < MinD) then begin
            ClosestRecNo := MyData.RecNo;
            MinD := Distance;
         end;
      end;
   end;
   if (ClosestRecNo > 1) then begin
      MyData.First;
      for I := 1 to MyData.RecordCount do begin
         if ValidLatLongFromTable(Lat2,Long2) then begin
            VincentyCalculateDistanceBearing(Lat,Long,Lat2,Long2,Distance,Bearing);
            if (Distance < MinD + 0.00001) then exit;
         end;
      end;
   end;
end;


function TGISdataBaseModule.ImageForTable: boolean;
begin
   Result := ColorPresent or RGBColorPresent or PointSymbolFieldsPresent or IconPresent or LineColorPresent or MyData.FieldExists('PALETTE');
end;


procedure TGISdataBaseModule.ExportToSQLite;
begin
   EmpSource.Enabled := false;
   MyData.ExportToSQLite;
   dbIsUnsaved := false;
   EmpSource.Enabled := true;
end;


procedure TGISdataBaseModule.ExportToXML(fName : PathStr);
begin
   EmpSource.Enabled := false;
   MyData.ExportToXML(fName);
   dbIsUnsaved := false;
   EmpSource.Enabled := true;
end;


function TGISdataBaseModule.AssignField(var TheValue : ShortString; TheName : ShortString) : boolean;
begin
   Result := MyData.FieldExists(TheName);
   if Result then TheValue := TheName;
end;

procedure TGISdataBaseModule.LongestString(FieldName : shortstring; var Max : integer; var LS : shortstring);
var
   i : integer;
begin
   EmpSource.Enabled := false;
   Max := 0;
   MyData.First;
   ShowHourglassCursor;
   repeat
     i := Length(MyData.GetFieldByNameAsString(FieldName));
     if (i > Max) then begin
        Max := i;
        LS := MyData.GetFieldByNameAsString(FieldName);
     end;
     MyData.Next;
   until MyData.EOF;
end;


function TGISdataBaseModule.LatLongCornersPresent : boolean;
begin
   LatLongCornersPresent := (MyData <> Nil) and MyData.BoundingBoxPresent;
end;


function TGISdataBaseModule.LatLongFieldsPresent : boolean;

      procedure GetlatLongFieldNames(Table : tMyData; var LatFieldName,LongFieldName : ShortString);
      begin
         if Table.FieldExists('LONG',true,ftfloat) and Table.FieldExists('LAT',true,ftfloat) then begin
            LatFieldName := 'LAT';
            LongFieldName := 'LONG';
            exit;
         end;
         LongFieldName := '';
         if Table.FieldExists('INTPTLON',true,ftFloat) then LongFieldName := 'INTPTLON';
         if Table.FieldExists('GLON',true,ftFloat) then LongFieldName := 'GLON';
         if Table.FieldExists('POLE_LONG',true,ftFloat) then LongFieldName := 'POLE_LONG';
         if Table.FieldExists('LON',true,ftFloat) then LongFieldName := 'LON';
         if Table.FieldExists('LONDEC',true,ftFloat) then LongFieldName := 'LONDEC';
         if Table.FieldExists('LONGITUDE',true,ftFloat) then LongFieldName := 'LONGITUDE';
         if Table.FieldExists('DEC_LONG',true,ftFloat) then LongFieldName := 'DEC_LONG';
         if Table.FieldExists('LONG1',true,ftFloat) then LongFieldName := 'LONG1';
         if Table.FieldExists('LONG_',true,ftFloat) then LongFieldName := 'LONG_';
         if Table.FieldExists('LONG_CENT',true,ftFloat) then LongFieldName := 'LONG_CENT';
         if Table.FieldExists('LONG',true,ftFloat) then LongFieldName := 'LONG';

         LatFieldName := '';
         if Table.FieldExists('INTPTLAT',true,ftFloat) then LatFieldName := 'INTPTLAT';
         if Table.FieldExists('GLAT',true,ftFloat) then LatFieldName := 'GLAT';
         if Table.FieldExists('POLE_LAT',true,ftFloat) then LatFieldName := 'POLE_LAT';
         if Table.FieldExists('LATITUDE1',true,ftFloat) then LatFieldName := 'LATITUDE1';
         if Table.FieldExists('LATITUDE',true) then LatFieldName := 'LATITUDE';
         if Table.FieldExists('LATDEC',true,ftFloat) then LatFieldName := 'LATDEC';
         if Table.FieldExists('DEC_LAT',true,ftFloat) then LatFieldName := 'DEC_LAT';
         if Table.FieldExists('LAT1',true,ftFloat) then LatFieldName := 'LAT1';
         if Table.FieldExists('LAT_',true,ftFloat) then LatFieldName := 'LAT_';
         if Table.FieldExists('LAT_CENT',true,ftFloat) then LatFieldName := 'LAT_CENT';
         if Table.FieldExists('LAT',true,ftFloat) then LatFieldName := 'LAT';
      end;

begin
   if XYZFile or LineOrAreaShapefile(ShapeFileType) then Result := false
   else begin
      if (LatFieldName = '') or (LongFieldName = '') then GetLatLongFieldNames(MyData,LatFieldName,LongFieldName);
      Result := (LatFieldName <> '') and (LongFieldName <> '');
   end;
end;


procedure TGISdataBaseModule.InitializeDBValues;
var
   i : integer;
begin
    RestrictToMapOwner := false;
    ShowLocalMapOnly := false;
    AutoRedrawAllowed := true;
    dbIsUnsaved := false;
    PLSSfile := false;
    LayerIsOn := true;
    FanCanClose := false;
    ItsAShapeFile := false;
    ShapeGeoFilter := false;
    Redistricting := false;
    DoNotTrackDBSettings := false;
    ShapeFileType := 0;
    LinkColorTable := 0;
    KMLExporTable := true;
    ItsFanFile := false;
    TernaryPlotUp := false;
    dbPlotted := true;
    LegendCaption := '';
    PointCGMName := '';
    GraphUpperLeftText := '';
    SymbolizationIsCorrent := false;
    StringCategories := Nil;
    TheMapOwner := nil;
    MyData := Nil;
    LinkTable := Nil;
    NeighborTable := Nil;
    RangeTable := Nil;
    LinkRangeTable := Nil;
    LayerTable := Nil;
    AreaRecordSize := 1;
    PointsInMemory := Nil;
    aShapeFile := Nil;
    ZeroTol := 0;
    NumPointsInMemory := 0;
    for i := 0 to MaxFieldsInDB do dbOpts.VisCols[i] := true;

    {$IfDef VCL}
       dbTablef := Nil;
       gis_scaled_form := Nil;
    {$EndIf}

    {$IfDef NoDBGrafs}
    {$Else}
       theGraphOwner := Nil;
       LastGraph := Nil;
    {$EndIf}
end;


function TGISdataBaseModule.InitializeTheTable(WhatDataBase : shortstring; FileWanted : PathStr = '') : boolean;
label
   Retry;
var
   fName,NewFile,BasePath,tName : PathStr;
   i,j,k,l : integer;
   Dir : DirStr;
   bName : NameStr;
   Ext : ExtStr;
   WasCSVImport,
   Success : boolean;
   sl,FieldsInDB : tStringList;
   glIndexFile : file;
   MainFileHeader : sfMainFileHeader;
   {$IfDef RecordOpenDataBase} TStr : shortstring; {$EndIf}
begin
    if not FileExists(FileWanted) then begin
       {$IfDef RecordOpenDataBase} WriteLineToDebugFile('TGISDataBase.InitializeTheData try open missing ' + ExtractFileName(FileWanted)); {$EndIf}
       MessageToContinue('Missing: ' + FileWanted);
       Result := false;
       exit;
    end;
    {$If Defined(RecordOpenDataBase)} if (UpperCase(ExtractFilePath(FileWanted)) <> UpperCase(MDTempDir)) then WriteLineToDebugFile('TGISDataBase.InitializeTheData start, ' + ExtractFileName(FileWanted)); {$EndIf}

    {$IfDef VCL}
       if (FileWanted = '') then begin
          FileWanted := LastDataBase;
          if (WhatDataBase = '') then WhatDataBase := 'data base';
          if not GetFileMultipleMask(WhatDataBase,DBMaskString,FileWanted,MDdef.DefDBFilter) then begin
             Result := false;
             exit;
          end;
          LastDataBase := FileWanted;
          {$IfDef RecordOpenDataBase} WriteLineToDebugFile('TGISDataBase.InitializeTheData, picked: ' + FileWanted); {$EndIf}
       end;
    {$EndIf}

    ShowHourglassCursor;
    Ext := UpperCase(ExtractFileExt(FileWanted));
    InitializeDBValues;
    Result := true;

    {$IfDef ExGDAL}
    {$Else}
       if ExtEquals(Ext,'.gpx') or ExtEquals(Ext,'.fit')  then begin
          try
             HeavyDutyProcessing := true;
             WMdem.Color := clInactiveCaption;
             BasePath := 'c:\mapdata\tracks\';
             SafeMakeDir(BasePath);
             MDdef.Add3DDist := false;
             if ExtEquals(Ext,'.gpx') then begin
                {$IfDef RecordFIT} WriteLineToDebugFile('open GPX, picked: ' + FileWanted); {$EndIf}
                tName := MDtempDir + ExtractFileNameNoExt(FileWanted) + '.gpx';
                CopyFile(FileWanted,tName);
             end;
             if ExtEquals(Ext,'.fit') then begin
                {$IfDef RecordFIT} WriteLineToDebugFile('open FIT, picked: ' + FileWanted); {$EndIf}
                tName := BasePath + ExtractFileName(FileWanted);
                MoveFile(FileWanted,tName);
                FileWanted := tName;
                {$IfDef RecordFIT} WriteLineToDebugFile('open FIT, copied to: ' + FileWanted); {$EndIf}
                tName := MDtempDir + ExtractFileNameNoExt(FileWanted) + '.gpx';
                if GPSBabel_fit2gpx(FileWanted,tName) then begin
                   if GetFileSize(tName) < 500 then begin
                      {$IfDef RecordFIT} WriteLineToDebugFile('FIT only ' + SmartMemorySizeBytes(GetFileSize(tName))); {$EndIf}
                      if not AnswerIsYes('File very small, probably has no locations (indoor activity?). Try import anyway') then begin
                         Result := false;
                         exit;
                      end;
                   end;
                end
                else exit;
             end;
             if FileExists(tName) then begin
                {$IfDef RecordFIT} WriteLineToDebugFile('GPSBabel_fit2gpx created ' + tName); {$EndIf}
                FileWanted := BasePath;
                SaveBackupDefaults;
                MDDef.UseMeters := false;
                MDdef.AddSpeed := true;
                {$IfDef RecordFIT} WriteLineToDebugFile('call GPXtoDBF'); {$EndIf}
                GPXtoDBF(tName,FileWanted);
                {$IfDef RecordFIT} WriteLineToDebugFile('FIT processed ' + FileWanted); {$EndIf}
                RestoreBackupDefaults;
             end
             else begin
                {$IfDef RecordFIT} WriteLineToDebugFile('GPSBabel_fit2gpx failed'); {$EndIf}
                Result := false;
                exit;
             end;
          finally
             HeavyDutyProcessing := false;
             WMdem.Color := clScrollBar;
          end;
       end;
    {$EndIf}

    {$IfDef VCL}
       if ExtEquals(Ext,'.shz') then begin
          CheckFileNameForSpaces(FileWanted);
          UnzipSingleFile(FileWanted);
       end;
    {$EndIf}

    if ExtEquals(Ext,'.nmea') then begin
       TextNMEAfiletoDBF(FileWanted);
    end;

    FSplit(FileWanted,Dir,bName,Ext);
    dbIsUnsaved := (Dir = UpperCase(MDTempDir));
    DoNotTrackDBSettings := dbIsUnsaved;

    {$IfDef ExSaveDBstatus}
    {$Else}
       DBAuxDir := Dir + 'db_aux' + PathDelim;
       SafeMakeDir(DBAuxDir);
    {$EndIf}

    WasCSVImport := false;
    if ExtEquals(Ext, '.CSV') or ExtEquals(Ext, '.TXT') or ExtEquals(Ext, '.XYZ') or ExtEquals(Ext, '.KML') or ExtEquals(Ext, '.ASC') then begin
       {$IfDef VCL}
          {$IfDef RecordDataBase} WriteLineToDebugFile('Picked CSV ' + Filewanted); {$EndIf}
          if ExtEquals(Ext,'.KML') then begin
             sl := PointKMLtoStringList(FileWanted);
             FileWanted := ChangeFileExt(FileWanted,'.csv');
             sl.SaveToFile(FileWanted);
             sl.Free;
          end;
          NewFile := Dir + bName + DefaultDBExt;
          if AutoOverwriteDBF or (not FileExists(NewFile)) or AnswerIsYes(NewFile + ' already exists; overwrite') then begin
             CSVFileImportToDB(FileWanted);
             WasCSVImport := true;
          end;
          FileWanted := Dir + bName + DefaultDBExt;
          if Not FileExists(FileWanted) then begin
             Result := false;
             exit;
          end;
          Ext := DefaultDBExt;
          ShowHourglassCursor;
       {$EndIf}
    end;

    if IsItAShapeFile(FileWanted) then begin
       ItsAShapeFile := true;
       FileWanted := ChangeFileExt(FileWanted,DefaultDBExt);

       {$IfDef SQLiteDefaultDBs}
          NewFile := Dir + bName + DefaultDBExt;
          if bName[1] in ['0'..'9'] then bName := 'n_' + bName;
          bName := StringReplace(bName,'-','_',[rfReplaceAll, rfIgnoreCase]);
          FileWanted := Dir + bName  + DefaultDBExt;
          {$IfDef MSWindows}
             if UpperCase(FileWanted) <> UpperCase(NewFile) then RenameShapeFile(NewFile,FileWanted);
          {$EndIf}
      {$EndIf}
    end;

   {$IfDef SQLiteDefaultDBs}
      if FileExtEquals(FileWanted,'.db') or FileExtEquals(FileWanted,'.dbf') or FileExtEquals(FileWanted,'.shp') then begin
         NewFile := ChangeFileExt(FileWanted,DefaultDBExt);
         if not FileExists(NewFile) then begin
            {$IfDef RecordMyDataCreation} WriteLineToDebugFile('ConvertDBFtoSQLite'); {$EndIf}
            FileWanted := ChangeFileExt(FileWanted,'.dbf');
            if not ConvertDBFtoSQLite(FileWanted) then begin
               Self.Destroy;
               Result := false;
               exit;
            end;
         end;
         FileWanted := NewFile;
         {$IfDef RecordMyDataCreation} WriteLineToDebugFile('changed input file to ' + FileWanted); {$EndIf}
      end;
   {$EndIf}

     dbName := ExtractFileNameNoExt(FileWanted);
     dbFullName := FileWanted;

     {$IfDef ExOSM}
     {$Else}
        ItsOSMShapeFile := IsThisAnOSMShapefile(FileWanted);
     {$EndIf}
     ItsTigerShapeFile := IsThisaTigerShapefile(FileWanted);
     {$IfDef RecordTIGER} if ItsOSMShapeFile or ItsTigerShapeFile then WriteLineToDebugFile('TIGER/OSM') else WriteLineToDebugFile('TIGER/OSM'); {$EndIf}

     {$IfDef AllowDBsToCDS}}
        if {$IfDef ExOSM} {$EndIf} then begin
           if (DesiredDBMode <> dbmForceDefault) then DesiredDBMode := dbmCDS;
        end;
        {$IfDef RecordFullOpenDB} WriteLineToDebugFile('TGISDataBase.InitializeTheData Data calling tMyData.Create, Mode=' + IntToStr(ord(DesiredDBMode)) + '  filewanted=' + dbFullName); {$EndIf}
        if not MDDef.AllowDBstoRAM then DesiredDBMode := dbmDefault;
     {$EndIf}

    {$If Defined(RecordOpenDataBase) or Defined(BasicOpens)} WriteLineToDebugFile(dbFullName + '  ' + SmartMemorySizeBytes(GetFileSize(dbFullName))); {$EndIf}
     try
        MyData := tMyData.Create(dbFullName,DesiredDBMode);
     except
        On Exception do begin
                       {$If Defined(RecordProblems)} HighLightLineToDebugFile(dbFullName + ' load failed'); {$EndIf}
                       Self.Destroy;
                       Result := false;
                       exit;
                     end;
     end;

     DesiredDBMode := dbmyDefault;
     if ZeroRecordsAllowed then begin
     end
     else if (MyData.RecordCount = 0) then begin
        if HeavyDutyProcessing or AnswerIsYes('No records in ' + dbFullName + '; Delete file') then begin
           MyData.Destroy;
           DeleteShapeFile(dbFullName);
        end;
        Self.Destroy;
        Result := false;
        exit;
     end;
     ZeroRecordsAllowed := false;

     {$IfDef VCL}
         if MyData.FieldExists('ORDER') then RenameField('ORDER','PLOT_ORDER');
     {$EndIf}

     if WasCSVImport then begin
        {$IfDef RecordFullOpenDB} WriteLineToDebugFile('TrimAllStringFields'); {$EndIf}
        ShowHourglassCursor;
        MyData.TrimAllStringFields;
     end;
     if IsIcesat and (not MyData.FieldExists('ICESAT_GRD')) and AnswerIsYes('Clean up ICESat-2 import') then begin
        {$IfDef RecordIcesat} WriteLineToDebugFile('Call IcesatProcessCanopy'); {$EndIf}
        IcesatProcessCanopy(DBnumber,false);
        {$IfDef RecordIcesat} WriteLineToDebugFile('Return from IcesatProcessCanopy'); {$EndIf}
     end;

     MyData.AssignEmpSource(EmpSource);

     if ItsAShapeFile then begin
       Retry:;
        CheckShapeFileNameForSpaces(FileWanted);
        ShapeFileName := FileWanted;
        aShapeFile := tShapeFile.Create(ShapeFileName,success);
        InsureFileIsNotReadOnly(FileWanted);
        ShapeFileType := aShapeFile.MainFileHeader.ShapeType;

        {$IfDef ExOSM}
        {$Else}
           if ItsOSMShapeFile then aShapeFile.BadPointCheck := MDDef.OSMCheck;
        {$EndIf}

        {$IfDef ExGDAL}
        {$Else}
           if (aShapeFile <> Nil) and aShapeFile.NotLatLongShapeFile then begin
              aShapeFile.Destroy;
              {$IfDef RecordOpenDataBase} WriteLineToDebugFile(ShapeFileName + ' Not lat/long shape file; convert before using'); {$EndIf}
              BasePath := ExtractFilePath(ShapeFileName) + 'll_version\';
              if not ReprojectShapeFileToGeographic(FileWanted,BasePath) then exit;
              goto Retry;
           end;
        {$EndIf}
     end;

    dbName := ExtractFileNameNoExt(dbFullName);
    if (ItsOSMShapeFile) then begin
       dbName := Petmar.LastSubDir(dbFullName) +'_' + dbName;
    end;

    dbLegendLabel := RemoveUnderscores(dbName);
    BasePath := ExtractFilePath(FileWanted);
    if (BasePath <> '') and (BasePath[length(BasePath)] = '\') then Delete(BasePath,Length(BasePath),1);

     if ItsAShapeFile then begin
         dbBoundBox := aShapeFile.MainFileHeader.BoundBox;
         {$IfDef VCL}
            if (MyData.TotRecsInDB < 10000) and AddBBtoShapeFiles then begin
               if LineOrAreaShapeFile(ShapeFileType) then begin
                  if not(LatLongCornersPresent) then AddBoundingBox;
               end
               else begin
                  if not (LatLongFieldsPresent) then aShapeFile.AddFields(afLatLong,MyData);
               end;
               AddSequentialIndex(RecNoFName,false);
            end;
         {$EndIf}
         AddBBtoShapeFiles := true;
     end
     else begin
        fName := ChangeFileExt(dbFullName,'.shx');
        if FileExists(fName) then begin
           assignFile(glIndexFile,fName);
           reset(glIndexFile,1);
           BlockRead(glIndexFile,MainFileHeader,100);
           CloseFile(glIndexFile);
           dbBoundBox := MainFileHeader.BoundBox;
        end
        else begin
           dbBoundBox.XMin := -180;
           dbBoundBox.XMax := 180;
           dbBoundBox.YMin := -90;
           dbBoundBox.YMax := 90;
        end;
     end;

    CanColorCode := false;
    {$IfDef ExSaveDBstatus}
    {$Else}
       RestoreDataBaseStatus;
       {$IfDef RecordSymbolColor} WriteLineToDebugFile('TGISdataBaseModule.InitializeTheTable pt 2, color = ' + RGBString(dbOpts.Symbol.Color.rgbtRed,dbOpts.Symbol.Color.rgbtGreen,dbOpts.Symbol.Color.rgbtBlue)); {$EndIf}
    {$EndIf}

      {$IfDef RecordOpenDataBaseStructure}
         WriteLineToDebugFile('TGISDataBase.InitializeTheData fields checked');
         TempOutput := MyData.GetTableStructure;
         WriteLineToDebugFile('Structure of ' + dbName,true);
         WriteStringListToDebugFile(TempOutput);
         TempOutput.Free;
      {$EndIf}

      ColorPresent := MyData.FieldExists('COLOR');
      RGBColorPresent := MyData.FieldExists('RED') and MyData.FieldExists('GREEN') and MyData.FieldExists('BLUE');
      FileNameFieldExists := MyData.FieldExists('FILENAME');
      NameFieldExists  := MyData.FieldExists('NAME');
      DipAndStrikeFieldsExist := MyData.FieldExists('DIP') and MyData.FieldExists('STRIKE');
      DipStrikeFieldExists := MyData.FieldExists('DIPSTRIKE');

      MonthFieldName := 'MO';
      if not MyData.FieldExists(MonthFieldName) then MonthFieldName := 'MONTH';

      if MDdef.AutoAssignNameField and (dbOpts.LabelField = '') then begin
         dbOpts.LabelField := MyData.AssignLabelName;
      end;

      {$IfDef RecordDataBaseLabels} WriteLineToDebugFile(DBName + ' Label Field: ' + DBOpts.LabelField); {$EndIf}

      for i := 1 to 5 do WWWFieldNames[i] := '';
      AssignField(WWWFieldNames[1],'HYPERLINK');
      AssignField(WWWFieldNames[1],'URL1');
      AssignField(WWWFieldNames[1],'URL');
      AssignField(WWWFieldNames[1],'WWW');

      for i := 1 to 5 do ImageFieldNames[i] := '';
      AssignField(ImageFieldNames[1],'IMAGE');

      for i := 1 to 5 do IconFieldNames[i] := '';
      AssignField(IconFieldNames[1],'ICONNAME');
      AssignField(IconFieldNames[1],'ICON');

      GetFields(MyData,dbOpts.VisCols,[ftString],FieldsInDB);

      j := 0;  if WWWFieldNames[1] <> '' then inc(j);
      k := 0;  if ImageFieldNames[1] <> '' then inc(k);
      l := 0;  if IconFieldNames[1] <> '' then inc(l);

      for I := 0 to pred(FieldsInDB.Count) do begin
         if copy(FieldsInDB.Strings[i],1,4) = 'WWW_' then begin
            inc(j);
            WWWFieldNames[j] := FieldsInDB.Strings[i];
         end;
         if FieldsInDB.Strings[i] <> 'IMAGE' then begin
           if (copy(FieldsInDB.Strings[i],1,4) = 'IMG_') or (copy(FieldsInDB.Strings[i],1,5) = 'IMAGE') then begin
              inc(k);
              ImageFieldNames[k] := FieldsInDB.Strings[i];
           end;
         end;
         if copy(FieldsInDB.Strings[i],1,5) = 'ICON_' then begin
            inc(l);
            IconFieldNames[l] := FieldsInDB.Strings[i];
         end;
      end;
      WWWPresent := (WWWFieldNames[1] <> '');
      ImagePresent  := (ImageFieldNames[1] <> '');
      IconPresent := (IconFieldNames[1] <> '');
      if (dbOpts.IconField = '') then dbOpts.IconField := IconFieldNames[1];

      FieldsInDB.Free;

      DEMIndex := MyData.FieldExists('DEM_NAME');
      FontFieldExists := MyData.FieldExists('FONT_NAME');
      PointSymbolFieldsPresent := MyData.FieldExists('SYM_TYPE') and MyData.FieldExists('SYM_SIZE') and MyData.FieldExists('SYM_COLOR');
      LineColorPresent := MyData.FieldExists('LINE_WIDTH') and  MyData.FieldExists('LINE_COLOR');
      AreaFillPresent := MyData.FieldExists('FILL_PAT') and  MyData.FieldExists('FILL_COLOR') and LineColorPresent;
      FocalMechsPresent := MyData.FieldExists('FP1_STRIKE') and MyData.FieldExists('FP2_STRIKE');
      VelNECompPresent  := (MyData.FieldExists('VN') and MyData.FieldExists('VE')) or (MyData.FieldExists('VBAR') and MyData.FieldExists('UBAR'));
      if VelNECompPresent then begin
         if (dbOpts.MagField = '') then begin
            if (MyData.FieldExists('VN') and MyData.FieldExists('VE')) then begin
               dbOpts.MagField := 'VE';
               dbOpts.DirField := 'VN';
            end;
            if (MyData.FieldExists('VBAR') and MyData.FieldExists('UBAR')) then begin
               dbOpts.MagField := 'VBAR';
               dbOpts.DirField := 'UBAR';
            end;
         end;
      end;

      ItsBoxOutline := (MyData.FieldExists('X1') and MyData.FieldExists('Y2') and MyData.FieldExists('X3')) and MyData.FieldExists('Y4');
      XYZFile := ItsBoxOutline or ((MyData.FieldExists('X') and MyData.FieldExists('Y') and MyData.FieldExists('Z')) and (not MyData.FieldExists('LAT')));

      ItsAGroupingFile := MyData.FieldExists('SHAPE_TYPE') and MyData.FieldExists('FILTER') and MyData.FieldExists('PLOT') and MyData.FieldExists('PLOT_ORDER');

      PhotoLocationsPresent := MyData.FieldExists('LAT') and MyData.FieldExists('LONG') and MyData.FieldExists('HFOV') and MyData.FieldExists('ELEV');
      CameraOrientationExists := MyData.FieldExists('HEIGHT') and MyData.FieldExists('AZIMUTH') and MyData.FieldExists('HFOV') and MyData.FieldExists('VFOV') and MyData.FieldExists('DEPTH') and MyData.FieldExists('PITCH');

     {$IfDef ExGeography}
     {$Else}
         KoppenPresent := (MyData.FieldExists('JAN_TEMP') and MyData.FieldExists('JAN_PRECIP')) or MyData.FieldExists('KOPPEN');
         if KoppenPresent then begin
            KoppenGr.LoadKoppenDefs;
            dbOpts.DBAutoShow := dbasKoppen;
         end;
     {$EndIf}

     {$IfDef ExGeology}
     {$Else}
        StratColPresent := MyData.FieldExists('COL_NAME');
        if StratColPresent then ColMainF := TColMainF.Create(Application);
     {$EndIf}

      TimeSeriesPresent := MyData.FieldExists('TIME_SER');
      GazDB := false;
      if XYZFile then begin
         {$IfDef VCL}
            if MyData.FieldExists('EASTING') then RenameField('EASTING','X');
            if MyData.FieldExists('NORTHING') then RenameField('NORTHING','Y');
            if MyData.FieldExists('ELEVATION') then RenameField('ELEVATION','Z');
            if MyData.FieldExists('X_UTM') then RenameField('X_UTM','X');
            if MyData.FieldExists('Y_UTM') then RenameField('Y_UTM','Y');
         {$EndIf}
         LatFieldName := 'Y';
         LongFieldName := 'X';
      end;

      Lat2fName := '';
      Long2fName := '';
      if LatLongFieldsPresent then begin
        if (MyData.FieldExists('LAT2') and MyData.FieldExists('LONG2')) then begin
           Lat2fName := 'LAT2';
           Long2fName := 'LONG2';
        end
        else if (MyData.FieldExists('TLAT') and MyData.FieldExists('TLON')) then begin
           Lat2fName := 'TLAT';
           Long2fName := 'TLON';
        end;
      end;
      SecondLatLongFieldsPresent := (Lat2fName <> '');
      CentroidPresent := (MyData.FieldExists('LAT_CENTRD') and MyData.FieldExists('LON_CENTRD'));

      ZFieldName := '';
      if MyData.FieldExists('Z',true,ftFloat) then zFieldName := 'Z'
      else if MyData.FieldExists('ELEV',true,ftFloat) then zFieldName := 'ELEV'
      else if MyData.FieldExists('ELEVATION',true,ftFloat) then zFieldName := 'ELEVATION'
      else if MyData.FieldExists('DEPTH',true,ftFloat) then zFieldName := 'DEPTH';

      ItsAPointDB := PointShapeFile(ShapeFileType) or LatLongFieldsPresent or XYZfile;
      NoGeometry := (not ItsAPointDB) and (not LineOrAreaShapeFile(ShapeFileType)) and (not XYZfile);

       StringFields := 0;
       NumericFields := 0;

       for i := 0 to pred(MyData.FieldCount) do begin
          if (MyData.GetFieldName(i) <> LatFieldName) and (MyData.GetFieldName(i) <> LongFieldName) and
             (MyData.GetFieldType(i) in NumericFieldTypes) then CanColorCode := true;
          if (MyData.GetFieldType(i) in NumericFieldTypes) then inc(NumericFields);
          if (MyData.GetFieldType(i) in [ftString]) then inc(StringFields);
      end;
      {$IfDef RecordDataBase} WriteLineToDebugFile('TGISDataBase.InitializeTheData checked for magic fields'); {$EndIf}
      if not FileExists(dbOpts.LinkTableName) then begin
         dbOpts.LinkTableName := ExtractFilePath(dbFullName) + ExtractFileName(dbOpts.LinkTableName);
      end;
      if FileExists(dbOpts.LinkTableName) then begin
         {$IfDef RecordOpenDataBase} WriteLineToDebugFile('Open linked TheData: ' + dbOpts.LinkTableName); {$EndIf}
         {$IfDef VCL}
         LinkSecondaryTable(dbOpts.LinkTableName);
         {$EndIf}
      end
      else dbOpts.LinkTableName := '';

   if (dbOpts.FloatColorField = '') and (dbOpts.DBAutoShow in [dbasColorByNumeric,dbasMonthlyTemp,dbasMonthlyRain,dbasMultiFieldRGB,dbasColorPosNeg]) then dbOpts.DBAutoShow := dbasDefault;
   {$IfDef RecordSymbolColor} WriteLineToDebugFile('TGISdataBaseModule.InitializeTheTable pt 4, color = ' + RGBString(dbOpts.Symbol.Color.rgbtRed,dbOpts.Symbol.Color.rgbtGreen,dbOpts.Symbol.Color.rgbtBlue)); {$EndIf}

   if ItsTigerShapeFile then dbOpts.DBAutoShow := dbasTiger;
   if ItsOSMShapeFile then dbOpts.DBAutoShow := dbasOSM;
   MyData.First;

   {$IfDef ExSaveDBstatus}
   {$Else}
      fName := ChangeFileExt(DBAuxDir + 'neigh_' + ExtractFileName(dbFullName),DefaultDBExt);
      if FileExists(fName) then begin
         {$IfDef RecordOpenDataBase} WriteLineToDebugFile('Open neighbors TheData: ' + fName); {$EndIf}
         NeighborTable := tMyData.Create(fName);
         NeighborLinkField := NeighborTable.GetFieldName(0);
      end;

      LayerTableFName := ChangeFileExt(DBAuxDir + 'lyr_' + ExtractFileName(dbFullName),DefaultDBExt);
      if FileExists(LayerTableFName) then begin
         LayerTable := tMyData.Create(LayerTableFName);
      end;
      {$IfDef RecordSymbolColor} WriteLineToDebugFile('TGISdataBaseModule.InitializeTheTable pt 6, color = ' + RGBString(dbOpts.Symbol.Color.rgbtRed,dbOpts.Symbol.Color.rgbtGreen,dbOpts.Symbol.Color.rgbtBlue)); {$EndIf}

   {$EndIf}

   {$IfDef RecordOpenDataBase}
      if (UpperCase(ExtractFilePath(dbFullName)) <> UpperCase(MDTempDir)) then begin
         if CanPlot then TStr := 'Can plot it'
         else TStr := 'Cannot plot it';
         if ItsAShapeFile then TStr := TStr + ' Shape file type: ' + IntToStr(ShapeFileType)
         else TStr := TStr + ' Not shape file';
         WriteLineToDebugFile('TGISDataBase.InitializeTheData out, recs= ' + IntToStr(MyData.TotRecsInDB) + '  ' + TStr);
      end;
   {$EndIf}
   {$IfDef RecordFilter}
      WriteLineToDebugFile('dbOpened, MainFilter= "' + dbOpts.MainFilter + '"');
      WriteLineToDebugFile('dbOpened, Geofilter= "' + dbOpts.GeoFilter + '"');
      WriteLineToDebugFile('dbOpened, Timefilter= "' + dbOpts.TimeFilter + '"');
   {$EndIf}

   ShowStatus;
   {$IfDef RecordBoundBox} WriteLineToDebugFile('Opened  ' + dbName + ' ' + sfBoundBoxToString(dbBoundBox,6)); {$EndIf}
   {$IfDef RecordFont} WriteLineToDebugFile('File=' + dbName + ' Gis font 1: ' + MyFontToString(dbOpts.GisLabelFont1) + '  Gis font 2: ' + MyFontToString(dbOpts.GisLabelFont2)); {$EndIf}
   {$IfDef RecordIniMemoryOverwrite} IniMemOverwriteCheck('TGISdataBaseModule.InitializeTheTable out'); {$EndIf}
   {$IfDef RecordSymbolColor} WriteLineToDebugFile('TGISdataBaseModule.InitializeTheTable out, color = ' + RGBString(dbOpts.Symbol.Color.rgbtRed,dbOpts.Symbol.Color.rgbtGreen,dbOpts.Symbol.Color.rgbtBlue)); {$EndIf}
end;


procedure TGISdataBaseModule.MarkRecordsOnDEM(fName : shortstring; DEM : integer);
var
   i : integer;
   InDEM : integer;
begin
   {$IfDef RecordMaskDEMShapeFile}
      WriteLineToDebugFile('TGISDataBase.MarkRecordsOnDEM, fName=' + FName);
      WriteLineToDebugFile('filter=' + MakeCornersGeoFilter(DEMGlb[DEM].DEMBoundBoxGeo) + ' AND ' + MyData.GetFieldByNameAsString(fName) + '=' + QuotedStr(''));
   {$EndIf}
   MyData.ApplyFilter(MakeGeoFilterFromBoundingBox(DEMGlb[DEM].DEMBoundBoxGeo));   // + ' AND ' + fName + '=' + QuotedStr(''));
   if LineShapeFile(ShapeFileType) then begin
     MyData.First;
     while not MyData.eof do begin
       {$IfDef RecordMaskDEMShapeFile} WriteLineToDebugFile('TGISDataBase.MarkRecordsOnDEM, rec=' + IntToStr(MyData.RecNo)); {$EndIf}
        if ShapeFile3D(ShapeFileType) then
           aShapeFile.GetLineCoords(MyData.RecNo,true)
        else aShapeFile.GetLineCoordsAndZsFromDEM(DEM,MyData.RecNo);
        InDEM := 0;
        for I := 0 to pred(aShapeFile.CurrentPolyLineHeader.NumPoints) do begin
           if abs(aShapeFile.CurrentLineZs^[i]) < 32000 then inc(InDEM);
        end;

        if (InDEM > 3 * aShapeFile.CurrentPolyLineHeader.NumPoints div 4) and ( (abs(aShapeFile.CurrentLineZs^[0]) < 32000) or
              (abs(aShapeFile.CurrentLineZs^[pred(aShapeFile.CurrentPolyLineHeader.NumPoints)]) < 32000)) then begin
           MyData.Edit;
           MyData.SetFieldByNameAsString(fName, ptTrim(DEMGlb[DEM].AreaName));
        end;
        MyData.Next;
     end;
   end
   else MessageToContinue('Currently only implemented for lines');
end;


{$IfDef VCL}
   procedure TGISdataBaseModule.CloseDBtableF(Reopen : boolean = false);
   var
      Action : TCloseAction;
   begin
      if (dbTablef <> Nil) then begin
         {$IfDef RecordCloseDB} WriteLineToDebugFile('TGISdataBaseModule.CloseDBtableF in'); {$EndIf}
         if (dbTablef.GraphOwnerBitmap <> Nil) then FreeAndNil(dbTablef.GraphOwnerBitmap);
         dbTablef.DBonTable := 0;
         Action := caFree;
         dbTablef.FormClose(nil,Action);
         //dbTablef.Destroy;
         dbTablef := Nil;
         {$IfDef RecordCloseDB} WriteLineToDebugFile('TGISdataBaseModule.CloseDBtableF done'); {$EndIf}
      end;
      if Reopen then begin
         DisplayTable;
      end;
   end;
{$EndIf}


function TGISdataBaseModule.CloseDataBase : boolean;
//var
   //i : integer;
begin
   {$IfDef RecordCloseDB} WriteLineToDebugFile('TGISDataBase.CloseDataBase, ' + dbName); {$EndIf}
   if not (WMDEM.ProgramClosing) then begin
      LayerIsOn := false;
      ToggleLayer(LayerIsOn);
   end;

   ChangeDEMNowDoing(JustWandering);

   if (DBNumber = KoppenGridDB) then KoppenGridDB := 0;
   if (DBNumber = ClimateStationDB) then ClimateStationDB := 0;
   if (DBNumber = WindsDB) then WindsDB := 0;
   if (DBNumber = PiratesDB) then PiratesDB := 0;

   if ItsFanFile and (Not FanCanClose) then  begin
      MessageToContinue('Cannot close until you close the DEM');
      Result := false;
      exit;
   end;
   Result := true;

   {$IfDef ExSaveDBstatus}
   {$Else}
      if MDDef.SaveDBStatus then SaveDataBaseStatus;
   {$EndIf}

   {$IfDef RecordFont} WriteLineToDebugFile('TGISDataBase.CloseDataBase Gis font 1: ' + MyFontToString(dbOpts.GisLabelFont1) + '  font 2: ' + MyFontToString(dbOpts.GisLabelFont2)); {$EndIf}

   {$IfDef VCL}
      CloseGisScaledForm;
      CloseDBtableF;
   {$EndIf}

   {$IfDef ExRedistrict}
   {$Else}
      if (RedistrictForm <> Nil) then begin
         RedistrictForm.Closable := true;
         RedistrictForm.Close;
         RedistrictForm := Nil;
      end;
   {$EndIf}

   if (aShapeFile <> Nil) then begin
      aShapeFile.Destroy;
      aShapeFile := Nil;
   end;

   if (LayerTable <> Nil) then LayerTable.Destroy;
   if (LinkTable <> Nil) then LinkTable.Destroy;
   if (LinkRangeTable <> Nil) then LinkRangeTable.Destroy;
   if (RangeTable <> Nil) then RangeTable.Destroy;
   if (StringCategories <> Nil) then begin
      StringCategories.Free;
      StringCategories := Nil;
   end;

   if (MyData <> Nil) then begin
      MyData.Destroy;
      MyData := Nil;
   end;

   Self.Destroy;

   {$IfDef RecordCloseDB} WriteLineToDebugFile('TGISdataBaseModule.CloseDataBase closed OK'); {$EndIf}
end;


procedure TGISdataBaseModule.RespondToChangedDB;
begin
    ApplicationProcessMessages;
    MyData.AssignEmpSource(EmpSource);
    MyData.ApplyFilter(MyData.Filter);
    ShowStatus;
end;


procedure TGISdataBaseModule.ApplyGISFilter(fString : AnsiString; DoShowStatus : boolean = true);
begin
   EmpSource.Enabled := false;
   dbOpts.MainFilter := fString;
   MyData.ApplyFilter(fString);
   EmpSource.Enabled := false;
   if DoShowStatus then ShowStatus;
   //ApplicationProcessMessages;
end;


function TGISdataBaseModule.SimplePointFile : boolean;
begin
   Result := (not XYZFile) and (ItsAPointDB);
end;

procedure TGISdataBaseModule.ClearImage;
var
   fName : PathStr;
begin
   fName := MyData.GetFieldByNameAsString('IMAGE');
   DeleteFileIfExists(fName);
   MyData.Edit;
   MyData.SetFieldByNameAsString('IMAGE','');
   MyData.Post;
end;

procedure TGISdataBaseModule.ClearLinkTable(ZeroNames : boolean);
begin
   if ZeroNames then begin
      dbOpts.LinkTableName := '';
      dbOpts.LinkFieldThisDB := '';
      dbOpts.LinkFieldOtherDB := '';
   end;
   if (LinkTable <> Nil) then begin
      {$IfDef RecordLinkTable} WriteLineToDebugFile('TGISdataBaseModule.ClearLinkTable destroyed the Link Table'); {$EndIf}
      LinkTable.Destroy;
      LinkTable := Nil;
   end;
   if (LinkRangeTable <> Nil) then begin
      {$IfDef RecordLinkTable} WriteLineToDebugFile('TGISdataBaseModule.ClearLinkTable destroyed the Link Range Table'); {$EndIf}
      LinkRangeTable.Destroy;
      LinkRangeTable := Nil;
   end;
end;

procedure TGISdataBaseModule.QueryGeoBox(bb : sfBoundBox; DisplayOnTable : boolean);
begin
   QueryGeoBox(bb.ymax,bb.xmin,bb.ymin,bb.xmax,DisplayOnTable);
end;


procedure TGISdataBaseModule.QueryGeoBox(HiLat,LowLong,LowLat,HighLong : float64; DisplayOnTable : boolean);
//var
   //ps : float64;
begin
   {$IfDef RecordQueryGeoBox} WriteLineToDebugFile('TGISDataBase.QueryGeoBox in '+ dbName +'  SW:  ' + LatLongDegreeToString(LowLat,LowLong) + ' NE:  ' + LatLongDegreeToString(HiLat,HighLong)); {$EndIf}

    if (LatLongCornersPresent or LatLongFieldsPresent) then begin
       //if (TheMapOwner <> Nil) then ps := TheMapOwner.MapDraw.ScreenPixelSize
       //else ps := 500;

       if LatLongCornersPresent then begin
          dbOpts.GeoFilter := PetDBUtils.MakeGeoFilterFromCorners(HiLat,LowLong,LowLat,HighLong{,ps});
       end
       else if LatLongFieldsPresent then dbOpts.GeoFilter := PetDBUtils.MakePointGeoFilter(LatFieldName,LongFieldName,HiLat,LowLong,LowLat,HighLong);

       {$IfDef RecordQueryGeoBox} WriteLineToDebugFile('GeoFilter length= ' + IntToStr(length(dbOpts.GeoFilter)) + '  GeoFilter: ' + dbOpts.GeoFilter); {$EndIf}
       EmpSource.Enabled := false;
       AssembleGISFilter;
       if MyData.Filtered then begin

         {$IfDef RecordQueryGeoBox} WriteLineToDebugFile('Recs found: ' + IntToStr(MyData.FiltRecsInDB) + '  Filter: ' + MyData.Filter); {$EndIf}

         {$IfDef ExRedistrict}
         {$Else}
            if (DEMNowDoing = RecolorRedistrictBox) then begin
               ShowHourglassCursor;
               MyData.First;
               EmpSource.Enabled := false;
               MyData.ApplyFilter('POP > 0' + ' AND ' + dbOpts.GeoFilter);
               {$IfDef RecordRedistrict} WriteLineToDebugFile('RecolorRedistrictBox, changed=' + IntToStr(MyData.FiltRecsinDB)); {$EndIf}
               while not MyData.EOF do begin
                  MyData.Edit;
                  MyData.SetFieldByNameAsString('DISTRICT',RedistrictForm.ComboBox1.Text);
                  MyData.SetFieldByNameAsInteger('COLOR',RedistrictForm.CurrentColor);
                  MyData.Next;
                  RedistrictForm.ChangeBlock;
               end;
               ClearGISFilter;
               ShowDefaultCursor;
            end;
         {$EndIf}

         {$IfDef VCL}
            if DisplayOnTable and (dbTablef <> Nil) then begin
               {$IfDef RecordQueryGeoBox} WriteLineToDebugFile('TGISDataBase.QueryGeoBox plot with filter=' + MyData.Filter); {$EndIf}
               RedrawLayerOnMap;
            end;
         {$EndIf}
       end;
    end;
    EmpSource.Enabled := true;
   {$IfDef RecordQueryGeoBox} WriteLineToDebugFile('TGISDataBase.QueryGeoBox out, Recs found: ' + IntToStr(MyData.RecordCount)); {$EndIf}
end;


procedure TGISdataBaseModule.FieldRange32(Field : shortstring; var Min,Max : float32; Filtered : boolean = false; ForceCompute : boolean = false);
var
   min64,max64 : float64;
begin
   FieldRange(Field,Min64,Max64,Filtered,ForceCompute);
   Min := Min64;
   Max := Max64;
end;



procedure TGISdataBaseModule.FieldRange(Field : shortstring; var Min,Max : float64; Filtered : boolean = false; ForceCompute : boolean = false);
var
   Num,Valid : integer;
   Sum : float64;
   ShortName : ShortString;
   Linked : boolean;

   function MinMaxExists(Table : tMyData; aField : ShortString) : boolean;
   begin
      Result := false;
      Table.ApplyFilter('FIELD_NAME=' + QuotedStr(aField));
      if (Table.RecordCount > 0) then begin
         Result := true;
         Min := Table.GetFieldByNameAsFloat('FIELD_MIN');
         Max := Table.GetFieldByNameAsFloat('FIELD_MAX');
      end;
      Table.ApplyFilter('');
   end;

   procedure MinMaxAdd(Table : tMyData; aField : ShortString);
   begin
      {$IfDef RecordSym} WriteLineToDebugFile(Table.FullTableName + ' update field=' + aField + '  min=' + RealToString(Min,-12,-2) + ' to ' + '  max=' + RealToString(Max,-12,-2)); {$EndIf}
      Table.ApplyFilter('FIELD_NAME=' + QuotedStr(aField));
      if (Table.RecordCount = 0) then Table.Insert
      else Table.Edit;
      Table.SetFieldByNameAsString('FIELD_NAME',aField);
      Table.SetFieldByNameAsFloat('FIELD_MIN',Min);
      Table.SetFieldByNameAsFloat('FIELD_MAX',Max);
      Table.Post;
      Table.ApplyFilter('');
   end;


begin
   {$IfDef RecordSym} WriteLineToDebugFile('TGISdataBaseModule.FieldRange in, field=' + Field); {$EndIf}
   ShortName := Field;
   Linked := LinkedField(ShortName) and (LinkRangeTable <> Nil);
   {$IfDef RecordSym} if Linked and MDDef.TrackDatabaseRanges and (LinkRangeTable <> Nil) then WriteLineToDebugFile('Required link range table is NIL'); {$EndIf}

   if (not ForceCompute) then begin
      if Linked then begin
         if MinMaxExists(LinkRangeTable,ShortName) then exit;
      end
      else if (RangeTable <> Nil) then begin
         if MinMaxExists(RangeTable,field) then exit;
      end;
   end;
   {$IfDef RecordSym} WriteLineToDebugFile('TGISdataBaseModule.FieldRange need to use PetDBUtils.FindFieldRangeLinkPossible'); {$EndIf}
   EmpSource.Enabled := false;
   FindFieldRangeLinkPossible(Field,Num,Valid,Sum,Min,Max);
   if (MyData.Filter = '') and MDDef.TrackDatabaseRanges then begin
      {$IfDef RecordSym} WriteLineToDebugFile('TGISdataBaseModule.FieldRange recording ranges');      {$EndIf}
      if Linked then MinMaxAdd(LinkRangeTable,ShortName)
      else if (RangeTable <> Nil) then MinMaxAdd(RangeTable,Field);
   end;
   EmpSource.Enabled := true;
   {$IfDef RecordSym} WriteLineToDebugFile('TGISdataBaseModule.FieldRange out, field=' + Field + '  min=' + RealToString(Min,-12,-2) + ' to ' + '  max=' + RealToString(Max,-12,-2)); {$EndIf}
end;


initialization
   ThinToPlot := 1;
   ZeroRecordsAllowed := false;
   AutoOverwriteDBF := false;
finalization
   {$IfDef RecordClosing} WriteLineToDebugFile('Closing demdatabase in'); {$EndIf}
   {$IfDef RecordZoomMap} WriteLineToDebugFile('RecordZoomMap active in demdatabase'); {$EndIf}
   {$IfDef RecordLineWidth} WriteLineToDebugFile('RecordLineWidth active in demdatabase'); {$EndIf}
   {$IfDef RecordID} WriteLineToDebugFile('RecordIDProblems active in demdatabase'); {$EndIf}
   {$IfDef RecordSPCS} WriteLineToDebugFile('RecordSPCSProblems active in demdatabase'); {$EndIf}
   {$IfDef RecordDBPlot} WriteLineToDebugFile('RecordDBPlot active in demdatabase'); {$EndIf}
   {$IfDef RecordDataBasePlotProblemsEveryPoint} WriteLineToDebugFile('RecordDataBasePlotProblemsEveryPoint active in demdatabase'); {$EndIf}
   {$IfDef RecordTiger} WriteLineToDebugFile('RecordTIGER active in demdatabase'); {$EndIf}
   {$IfDef RecordCloseDB} WriteLineToDebugFile('RecordCloseDBProblems active in demdatabase'); {$EndIf}
   {$IfDef RecordOpenDataBase} WriteLineToDebugFile('RecordOpenDataBaseProblems active in demdatabase'); {$EndIf}
   {$IfDef RecordPointInArea} WriteLineToDebugFile('RecordPointInAreaProblems active in demdatabase'); {$EndIf}
   {$IfDef RecordBeachBall} WriteLineToDebugFile('RecordBeachBallProblems active in demdatabase'); {$EndIf}
   {$IfDef RecordMaskDEMShapeFile} WriteLineToDebugFile('RecordMaskDEMShapeFile active in demdatabase'); {$EndIf}
   {$IfDef RecordFilterDataBase} WriteLineToDebugFile('RecordFilterDataBase active in demdatabase'); {$EndIf}
   {$IfDef RecordLinkTable} WriteLineToDebugFile('RecordLinkTable active in demdatabase'); {$EndIf}
   {$IfDef RecordShapeFileGroup} WriteLineToDebugFile('RecordShapeFileGroup active in demdatabase'); {$EndIf}
   {$IfDef RecordQueryGeoBox} WriteLineToDebugFile('RecordQueryGeoBox active in demdatabase'); {$EndIf}
   {$IfDef RecordValidScreenPosition} WriteLineToDebugFile('RecordValidScreenPosition active in demdatabase'); {$EndIf}
   {$IfDef RecordDataBaseLabels} WriteLineToDebugFile('RecordDataBaseLabels active in demdatabase'); {$EndIf}
   {$IfDef RecordDataSaveStatus} WriteLineToDebugFile('RecordDataSaveStatus active in demdatabase'); {$EndIf}
   {$IfDef RecordGAZ} WriteLineToDebugFile('RecordGAZProblems active in demdatabase'); {$EndIf}
   {$IfDef RecordDipStrike} WriteLineToDebugFile('RecordDipStrike active in demdatabase'); {$EndIf}
   {$IfDef RecordNetworkends} WriteLineToDebugFile('RecordNetworkends active in demdatabase'); {$EndIf}
   {$IfDef RecordFan} WriteLineToDebugFile('RecordFanProblems active in demdatabase'); {$EndIf}
   {$IfDef RecordNetworkNodes} WriteLineToDebugFile('RecordNetworkNodes active in demdatabase'); {$EndIf}
   {$IfDef RecordDBindexes} WriteLineToDebugFile('RecordDBindexes active in demdatabase'); {$EndIf}
   {$IfDef RecordMergeDB} WriteLineToDebugFile('RecordMergeDB active in demdatabase'); {$EndIf}
   {$IfDef ShowOtherDBsWithSameFilter} WriteLineToDebugFile('ShowOtherDBsWithSameFilter active in demdatabase'); {$EndIf}
   {$IfDef RecordBasinBreakdown} WriteLineToDebugFile('RecordBasinBreakdown active in demdatabase'); {$EndIf}
   {$IfDef RecordAddField} WriteLineToDebugFile('RecordAddField active in demdatabase'); {$EndIf}
   {$IfDef RecordEveryDataBaseLabel} WriteLineToDebugFile('RecordEveryDataBaseLabel active in demdatabase'); {$EndIf}
   {$IfDef RecordGISvectors} WriteLineToDebugFile('RecordGISvectors active in demdatabase'); {$EndIf}
   {$IfDef RecordCurrentRecord} WriteLineToDebugFile('RecordCurrentRecord active in demdatabase'); {$EndIf}
   {$IfDef RecordIcons} WriteLineToDebugFile('RecordIcons active in demdatabase'); {$EndIf}
   {$IfDef RecordDataBaseText} WriteLineToDebugFile('RecordDataBaseText active in demdatabase'); {$EndIf}
   {$IfDef RecordOpenDataBaseStructure} WriteLineToDebugFile('RecordOpenDataBaseStructure active in demdatabase'); {$EndIf}
   {$IfDef RecordDataBaseSaveFiles} WriteLineToDebugFile('RecordDataBaseSaveFiles active in demdatabase'); {$EndIf}
   {$IfDef RecordFullShapeFileGroup} WriteLineToDebugFile('RecordFullShapeFileGroup active in demdatabase'); {$EndIf}
   {$IfDef RecordStationTimeSeries} WriteLineToDebugFile('RecordStationTimeSeries active in demdatabase'); {$EndIf}
   {$IfDef RecordMonthlyFilter} WriteLineToDebugFile('RecordMonthlyFilter active in demdatabase'); {$EndIf}
   {$IfDef RecordRedistrict} WriteLineToDebugFile('RecordRedistrict active in demdatabase'); {$EndIf}
   {$IfDef RecordDBPlotDetailed} WriteLineToDebugFile('RecordDBPlotDetailed active in demdatabase'); {$EndIf}
   {$IfDef RecordDBCount} WriteLineToDebugFile('RecordDBCount active in demdatabase'); {$EndIf}
   {$IfDef RecordFIT} WriteLineToDebugFile('RecordFIT active in demdatabase'); {$EndIf}
   {$IfDef RecordFieldStatistics} WriteLineToDebugFile('RecordFieldStatistics active in demdatabase'); {$EndIf}


   {$IfDef RecordClosing} WriteLineToDebugFile('Closing demdatabase out'); {$EndIf}
end.




