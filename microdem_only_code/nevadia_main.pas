unit nevadia_main;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch }
{   file verified 5/13/2018       }
{_________________________________}


{$I nevadia_defines.inc}


{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   {$IFDEF DEBUG}
      {$Define RecordDEMIX}
      //{$Define RecordFullDEMIX}
      //{$Define RecordDEMIXLoad}

      //{$Define RecordMenu}
      //{$Define TrackFormCreate}
      //{$Define RecordSatLoad}
      //{$Define RecordFileOps}
      //{$Define RecordGeoPDF}
      //{$Define RecordOpenVectorMap}
      //{$Define RecordShipwrecks}
      //{$Define Record3D}
      //{$Define RecordLabs}
      {$Define RecordBatch}
      //{$Define LidarGlobalDEMs}
      {$Define RecordCommandLine}
      //{$Define RecordCartography}
      //{$Define RecordFormActivate}
      //{$Define RecordUpdate}
      //{$Define RecordFirstRun}
      //{$Define RecordOpenVectorMap}
      //{$Define TimeLoadDEM}
      //{$Define RecordClosing}
      //{$Define RecordMGT}
      //{$Define RecordHelp}
      //{$Define RecordDBFconvert}
      //{$Define RecordFormResize}
      //{$Define StatusBarFormResize}
      //{$Define DrainageBasinStats}
      //{$Define RecordNodeIssues}
      //{$Define DrainageBasinStreamsInBasins}
      //{$Define FullDrainageBasinStats}
      //{$Define RecordOceanography}
      //{$Define RecordGeomorphDB}
      //{$Define RecordDefineDatum}
      //{$Define RecordGeostats}
      //{$Define RecordButton}
   {$Else}
       //{$Define RecordFormActivate}
       //{$Define RecordFirstRun}
       //{$Define MessageStartup}
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
      DataSnap.dbclient,
   {$EndIf}
//end DB declarations

   DEMDefs,Petmar_types,PETMAR, DEMMapDraw, DEMMapF,

   {$IfDef ExMovie}
   {$Else}
      //PetMovie,
   {$EndIf}

   {$IfDef ExFMX3D}
      FMX.Types3D,
   {$EndIf}

   System.Math.Vectors,System.Types,System.RTLConsts,System.Win.TaskbarCore,System.IOUtils,System.Diagnostics,System.UiTypes,
   Windows,ShlObj, WinInet,
   Vcl.Taskbar,
   Classes,Graphics,Forms,Controls,Menus,Printers,StdCtrls,ComObj,ClipBrd,
   Dialogs, ExtCtrls, ComCtrls, Buttons, ToolWin,SysUtils,ShellAPI,Messages,URLMon,Math,StrUtils;

const
   (*
   {$IfDef Win32}  ProgramName = 'MicroDEM-32';  {$EndIf}
   {$IfDef Win64}  ProgramName = 'MicroDEM-64';  {$EndIf}
   *)
   ProgramFunction = 'Freeware GIS';
type
  Twmdem = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Exit2: TMenuItem;
    Help1: TMenuItem;
    AboutMICRODEM1: TMenuItem;
    Window1: TMenuItem;
    View1: TMenuItem;
    LOS1: TMenuItem;
    Cascade1: TMenuItem;
    Tile1: TMenuItem;
    Perspective1: TMenuItem;
    Data1: TMenuItem;
    N1: TMenuItem;
    Contents1: TMenuItem;
    N3profiles1: TMenuItem;
    Options1: TMenuItem;
    Analyze1: TMenuItem;
    Hardware1: TMenuItem;
    ConvertCoordinates1: TMenuItem;
    StereoNet1: TMenuItem;
    Programlimits1: TMenuItem;
    N5: TMenuItem;
    Magneticmodel1: TMenuItem;
    N8: TMenuItem;
    Cancelpending1: TMenuItem;
    Tools1: TMenuItem;
    N9: TMenuItem;
    Header1: TMenuItem;
    N10: TMenuItem;
    DEMproperties1: TMenuItem;
    Flythrough1: TMenuItem;
    ToolBar1: TToolBar;
    NewSATButton: TSpeedButton;
    NewDEMButton: TSpeedButton;
    LOSButton: TSpeedButton;
    CoordButton: TSpeedButton;
    InOutButton: TSpeedButton;
    PerspectiveButton: TSpeedButton;
    MGTMagModelSpeedButton: TSpeedButton;
    FlySpeedButton: TSpeedButton;
    CloseallDEMs1: TMenuItem;
    CloseallImagery1: TMenuItem;
    CloseAlldataandwindows1: TMenuItem;
    Exitprogram1: TMenuItem;
    Exitprogram2: TMenuItem;
    Opendatasets1: TMenuItem;
    //Slopealgorithms1: TMenuItem;
    StatusBar1: TStatusBar;
    CircleAround: TMenuItem;
    LiveFlySpeedButton: TSpeedButton;
    StereoNetButton: TSpeedButton;
    Savedesktopandexitprogram1: TMenuItem;
    SpeedButton8: TSpeedButton;
    MetaData1: TMenuItem;
    MetadataPopupMenu: TPopupMenu;
    GeoTIFF1: TMenuItem;
    N16: TMenuItem;
    N17: TMenuItem;
    ESRIshapefile1: TMenuItem;
    SpeedButton9: TSpeedButton;
    Superimposedtopoprofiles1: TMenuItem;
    Viewdebuglog1: TMenuItem;
    Discussionforum1: TMenuItem;
    MultProfSpeedButton: TSpeedButton;
    DTED1: TMenuItem;
    TernarySpeedButton: TSpeedButton;
    StratcolSpeedButton: TSpeedButton;
    SeaFloorAgeSpeedButton: TSpeedButton;
    PredBathySpeedButton: TSpeedButton;
    PlateRotateSpeedButton: TSpeedButton;
    NASABlueMarbleSpeedButton: TSpeedButton;
    PopupMenu7: TPopupMenu;
    UTMordatumconversion1: TMenuItem;
    Stateplanecoordinatesystem1: TMenuItem;
    UKOSgrid1: TMenuItem;
    TINSpeedButton: TSpeedButton;
    Geology1: TMenuItem;
    Ternarydiagram1: TMenuItem;
    Stratcol2: TMenuItem;
    Platereconstructions1: TMenuItem;
    Newpanorama1: TMenuItem;
    PanoramaSpeedButton: TSpeedButton;
    FinnishGaussKruger1: TMenuItem;
    N4: TMenuItem;
    //Mapprojection1: TMenuItem;
    TigerSpeedButton: TSpeedButton;
    Conicconversions1: TMenuItem;
    Polarstereographic1: TMenuItem;
    Algorithms1: TMenuItem;
    Verticalearthcurvature1: TMenuItem;
    Horizontalearthcurvature1: TMenuItem;
    //Pointspacing1: TMenuItem;
    PanoramaAdmin1: TMenuItem;
    Oceanographicdata1: TMenuItem;
    CTD1: TMenuItem;
    Lightdata1: TMenuItem;
    Setupserver1: TMenuItem;
    IntDBSpeedButton: TSpeedButton;
    HypImageSpeedButton: TSpeedButton;
    MrSidimagery1: TMenuItem;
    N7: TMenuItem;
    Geostatisticalanalysis1: TMenuItem;
    QuickVectorMapSpeedButton: TSpeedButton;
    VectorMapButton: TSpeedButton;
    Computations1: TMenuItem;
    LandCoverSpeedButton: TSpeedButton;
    OpenPopupMenu1: TPopupMenu;
    Openproject1: TMenuItem;
    OpenDEM1: TMenuItem;
    OpenImage1: TMenuItem;
    OpenScannedmap1: TMenuItem;
    OpenVectormap1: TMenuItem;
    OpenTIGERcountymap1: TMenuItem;
    Openshapefilemap1: TMenuItem;
    OpenDatabasewithoutmap1: TMenuItem;
    Openhyperspectralimagery1: TMenuItem;
    Open1: TMenuItem;
    OpenUSstatesmap1: TMenuItem;
    OpenUScountiesmap1: TMenuItem;
    Correlationmatrix1: TMenuItem;
    Opengooglemaps1: TMenuItem;
    SpeedButton2: TSpeedButton;
    SpeedButton4: TSpeedButton;
    ToolsPopupMenu3: TPopupMenu;
    Loadimage1: TMenuItem;
    extEditor1: TMenuItem;
    Moviereplay1: TMenuItem;
    Militaryicongenerator1: TMenuItem;
    Webpagethumbnails1: TMenuItem;
    CompressDecompress1: TMenuItem;
    Spectrallibrary1: TMenuItem;
    SpeedButton5: TSpeedButton;
    VRML1: TMenuItem;
    Updatehelpfile1: TMenuItem;
    PClouder1: TMenuItem;
    Spectrallibrary2: TMenuItem;
    Sieve1: TMenuItem;
    LASfile1: TMenuItem;
    Showfirstlinestextfile1: TMenuItem;
    Openpointclouds1: TMenuItem;
    LatlongofPLSSlocation1: TMenuItem;
    Batchprocessing1: TMenuItem;
    MergewavelengthheightDBFs1: TMenuItem;
    Identifydirectory1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    N11: TMenuItem;
    UTM1: TMenuItem;
    //SPCS1: TMenuItem;
    OpenandmergeDEMs1: TMenuItem;
    ImportCTDfile1: TMenuItem;
    EXIFmetadata1: TMenuItem;
    EXIFimage1: TMenuItem;
    Clustergrids1: TMenuItem;
    Copyfile1: TMenuItem;
    Distancematrix1: TMenuItem;
    XYZshapefile1: TMenuItem;
    //Seriesdifferences1: TMenuItem;
    N81Sfileviewer1: TMenuItem;
    CopyDBFstoXML1: TMenuItem;
    BackupprogramEXE1: TMenuItem;
    Geocodeaddresstolatlong1: TMenuItem;
    Stereopair1: TMenuItem;
    MSTsidescanimport1: TMenuItem;
    N21: TMenuItem;
    XTFindex1: TMenuItem;
    Findmatchingfiles1: TMenuItem;
    XTFsidescan1: TMenuItem;
    N22: TMenuItem;
    Fullworldimage1: TMenuItem;
    Structuralgeologylab1: TMenuItem;
    SheepRange1: TMenuItem;
    Afar1: TMenuItem;
    Updatehelpfile2: TMenuItem;
    TulaFracturezonemagnetics1: TMenuItem;
    Platerotations1: TMenuItem;
    Sedimentationrates1: TMenuItem;
    Triplejunctions1: TMenuItem;
    Trenchgeometry1: TMenuItem;
    SedThickButton: TSpeedButton;
    Italyfocalmechs1: TMenuItem;
    Statsfortrainingset1: TMenuItem;
    Statesforblockgrids1: TMenuItem;
    Normalizegrids1: TMenuItem;
    Addnormaliziedstatsforblockgridstotrainingset1: TMenuItem;
    Geomorphatlas1: TMenuItem;
    GISlabs1: TMenuItem;
    Annapolisredistricting1: TMenuItem;
    Duckbeachsurveys1: TMenuItem;
    Physicalgeographylabs1: TMenuItem;
    Piracywindsandrain1: TMenuItem;
    OpenDEMwithoutmap1: TMenuItem;
    Megathrusts1: TMenuItem;
    Nightlights1: TMenuItem;
    Create1: TMenuItem;
    NewglobalgridGreenwich1: TMenuItem;
    NewglobalgridIDL1: TMenuItem;
    ConvertDBFsfor64bit1: TMenuItem;
    Landsatbrowseindex1: TMenuItem;
    ConvartDBFs1: TMenuItem;
    oSQLite1: TMenuItem;
    Mercator1: TMenuItem;
    Southpolarstereographic1: TMenuItem;
    Openmultigrids1: TMenuItem;
    Solarpositiln1: TMenuItem;
    Graysgame1: TMenuItem;
    MH370region1: TMenuItem;
    MicronetQuiz1: TMenuItem;
    Geoidandsedimentdistribution1: TMenuItem;
    MagMapButton: TSpeedButton;
    N27: TMenuItem;
    SpeedButton3: TSpeedButton;
    //Subbottomandsediments1: TMenuItem;
    Californiaoffshore1: TMenuItem;
    GulfofMexicoGLORIA1: TMenuItem;
    Atlantis1: TMenuItem;
    DEMsummarytable1: TMenuItem;
    Landsatfullsceneindex1: TMenuItem;
    Satellitepredictions1: TMenuItem;
    Annapolisebasemap1: TMenuItem;
    Bathymetrygrid1: TMenuItem;
    Chart12263180K1: TMenuItem;
    Chart12282125K1: TMenuItem;
    Chart12283110K1: TMenuItem;
    Subset81Ssidescan1: TMenuItem;
    GISdatasampler1: TMenuItem;
    N15: TMenuItem;
    imecores1: TMenuItem;
    Sealevelrise1: TMenuItem;
    N29: TMenuItem;
    Ages1: TMenuItem;
    Magneticanomaliesgrid1: TMenuItem;
    Sedimenttypegrid1: TMenuItem;
    Sedimentthicknessgrid1: TMenuItem;
    Fontsinstalled1: TMenuItem;
    Sensorcoverage1: TMenuItem;
    Unicodeicongenerator1: TMenuItem;
    UKOSgrid2: TMenuItem;
    HarpersFerryTerrainAnalysis1: TMenuItem;
    Nyquist1: TMenuItem;
    Onlinehelp1: TMenuItem;
    AnnapolisTM8scene1: TMenuItem;
    Introductorytutorials1: TMenuItem;
    N30: TMenuItem;
    OpenandmergeDEMdirectories1: TMenuItem;
    SpeedButton6: TSpeedButton;
    est1: TMenuItem;
    LatLong1: TMenuItem;
    Edit1: TMenuItem;
    EditDEMHeader1: TMenuItem;
    //OpenGLtwogrids1: TMenuItem;
    Timer2: TTimer;
    Seismicviewing1: TMenuItem;
    RestorepreviousprogramEXE1: TMenuItem;
    DBFfile1: TMenuItem;
    GeoPDF1: TMenuItem;
    Openlandcover1: TMenuItem;
    OpenGeoPDF1: TMenuItem;
    lasinfo1: TMenuItem;
    KMLKMZfile1: TMenuItem;
    XML1: TMenuItem;
    N23: TMenuItem;
    LAS1: TMenuItem;
    OpenGeoPDFimagelayer1: TMenuItem;
    OpenUSTopoGeoPDF1: TMenuItem;
    Allindividuallayers1: TMenuItem;
    LASlidarpointcloudsamples1: TMenuItem;
    Legislativeredistricting1: TMenuItem;
    Zipatoneeditor1: TMenuItem;
    LASdata1: TMenuItem;
    Other3Dpointclouds1: TMenuItem;
    OpenLASpointcloud1: TMenuItem;
    N6: TMenuItem;
    N24: TMenuItem;
    opengl1tofront: TMenuItem;
    Showfirstbytesofbinaryfile1: TMenuItem;
    Openimagewithmultiplebands1: TMenuItem;
    Openmultigridspickfiles1: TMenuItem;
    Solstice1: TMenuItem;
    Equinox1: TMenuItem;
    DecemberSolstice1: TMenuItem;
    Openclimatemonthlymultigrids1: TMenuItem;
    ClosePopupMenu: TPopupMenu;
    Closeallgraphs1: TMenuItem;
    Closeallmaps1: TMenuItem;
    Closealltexteditwindows1: TMenuItem;
    Differencetwobitmaps1: TMenuItem;
    Closeallpictureviewwindows1: TMenuItem;
    Equinoxeandsolstices1: TMenuItem;
    N31: TMenuItem;
    hreeviews1: TMenuItem;
    Monthlyclimateparameters1: TMenuItem;
    Createmoviefromstills1: TMenuItem;
    Experimental1: TMenuItem;
    Downloaddatasets1: TMenuItem;
    Lidarandbeacherosion1: TMenuItem;
    Closewindows1: TMenuItem;
    Mapprojectionsanddistortion1: TMenuItem;
    Monthlywinds1: TMenuItem;
    GeographyPopupMenu: TPopupMenu;
    Koppen1: TMenuItem;
    Climatestationsforclimographs1: TMenuItem;
    Classificationmap1: TMenuItem;
    Hurricanes1: TMenuItem;
    Dailytemperaturerange1: TMenuItem;
    Gridswithmonthlyprecipitationandtemperature1: TMenuItem;
    Geoid1: TMenuItem;
    PopupMenu8: TPopupMenu;
    Monthlyclimatologies1: TMenuItem;
    Addproject1: TMenuItem;
    OGRinfo1: TMenuItem;
    RenameJPGsfromlockscreen1: TMenuItem;
    Leastcostpath1: TMenuItem;
    Planarprojections1: TMenuItem;
    Cylindricalprojections1: TMenuItem;
    Conicprojections1: TMenuItem;
    All1: TMenuItem;
    Landsatimage1: TMenuItem;
    Monthlyinsolation1: TMenuItem;
    UTMprojectoiin1: TMenuItem;
    LabSpeedButton7: TSpeedButton;
    GeologyPopupMenu: TPopupMenu;
    Koppenmonthlytemperatureandprecipitationclimatologies1: TMenuItem;
    Sedimenttypes1: TMenuItem;
    Sdeimentthickness1: TMenuItem;
    N26: TMenuItem;
    N32: TMenuItem;
    Geoid2: TMenuItem;
    N33: TMenuItem;
    Platetectonics1: TMenuItem;
    Quickplatetectonicsmaps1: TMenuItem;
    N34: TMenuItem;
    Seismicfencediagram1: TMenuItem;
    Geology2: TMenuItem;
    NaturalEarthvectoroutlines1: TMenuItem;
    ETOPO11: TMenuItem;
    Slidesorter1: TMenuItem;
    Fileoperations1: TMenuItem;
    Movefileswithnamematch1: TMenuItem;
    RenameJPEGswithcreationtime1: TMenuItem;
    Mergemasp1: TMenuItem;
    Batchchangepartoffilenames1: TMenuItem;
    N12: TMenuItem;
    N13: TMenuItem;
    OpenandmergeGeotiffs1: TMenuItem;
    N14: TMenuItem;
    DragonPlot1: TMenuItem;
    Annapolislidar1: TMenuItem;
    Landsattimeseries1: TMenuItem;
    OpenSentinen2image1: TMenuItem;
    Openlidarmatchedgrids1: TMenuItem;
    Photos1: TMenuItem;
    Importfromcamera1: TMenuItem;
    RenameJPRGswithbasenamenumber1: TMenuItem;
    RenameJPEGSwithbaseandcreationtime1: TMenuItem;
    Labs1: TMenuItem;
    N18: TMenuItem;
    CloseallDBs1: TMenuItem;
    Remotesensinglabs1: TMenuItem;
    Datadownload25GB1: TMenuItem;
    CloseprogramupdateEXEnewversion7MBdownload1: TMenuItem;
    Spectrallibrary3: TMenuItem;
    N19: TMenuItem;
    RGBcolorlayers1: TMenuItem;
    GDALSRSinfo1: TMenuItem;
    WhiteboxGeotiff1: TMenuItem;
    GPS1: TMenuItem;
    GDALslopesarcsecondDEMs1: TMenuItem;
    Mediansatellitedatacontest1: TMenuItem;
    Makelittletilescontest1: TMenuItem;
    LidarandglobalDEMs1: TMenuItem;
    Guam1: TMenuItem;
    Geotiff2: TMenuItem;
    GDALWKT1: TMenuItem;
    N20: TMenuItem;
    N25: TMenuItem;
    N28: TMenuItem;
    Postprocesscontest1: TMenuItem;
    WKTprojection1: TMenuItem;
    ArcsecondspacingDTEDDGED1: TMenuItem;
    UTMspacingandgridtrueangle1: TMenuItem;
    CreateDEMsfromlidar1: TMenuItem;
    CategoriesfromallopenDEMs1: TMenuItem;
    Categoriesfromdatabase1: TMenuItem;
    Subsamplecomparethinaverage1: TMenuItem;
    Createcompositebitmap1: TMenuItem;
    Arcsecondrectangularpixels1: TMenuItem;
    DEMIXtiles1: TMenuItem;
    DEMcornerstable1: TMenuItem;
    netcdf1: TMenuItem;
    //GetGRASSextensions1: TMenuItem;
    ACOLITEallopensatelliteimages1: TMenuItem;
    Fatfingers1: TMenuItem;
    DEMIXwinecontest1: TMenuItem;
    UpdateEurekaValleyDEM1: TMenuItem;
    Closeprogramgetdebugversionoftheprogram7MB1: TMenuItem;
    KangarooIslandadditionalscenes1: TMenuItem;
    Openrecyclebin1: TMenuItem;
    Existingfile1: TMenuItem;
    Existingfile2: TMenuItem;
    Horizontalimageslider1: TMenuItem;
    Allgraphsononeimage1: TMenuItem;
    OpensingleLandsatband1: TMenuItem;
    listgeo1: TMenuItem;
    DEMIXelevationhistograms1: TMenuItem;
    DEMIXcreatereferenceDEMs1: TMenuItem;
    procedure Updatehelpfile1Click(Sender: TObject);
    procedure VRML1Click(Sender: TObject);
    procedure HypImageSpeedButtonClick(Sender: TObject);
    procedure Hyperspectralimagery1Click(Sender: TObject);
    procedure Exit2Click(Sender: TObject);
    procedure AboutMICRODEM1Click(Sender: TObject);
    procedure Newarea1Click(Sender: TObject);
    procedure LOS1Click(Sender: TObject);
    procedure Cascade1Click(Sender: TObject);
    procedure Tile1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Data1Click(Sender: TObject);
    procedure Loadimage1Click(Sender: TObject);
    procedure Contents1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Newsatelliteimage1Click(Sender: TObject);
    procedure Hardware1Click(Sender: TObject);
    procedure Elevationhistograms1Click(Sender: TObject);
    procedure ConvertCoordinates1Click(Sender: TObject);
    procedure StereoNet1Click(Sender: TObject);
    procedure Programlimits1Click(Sender: TObject);
    procedure Magneticmodel1Click(Sender: TObject);
    procedure Cancelpending1Click(Sender: TObject);
    procedure TextEditor1Click(Sender: TObject);
    procedure Header1Click(Sender: TObject);
    //procedure ContourGhosts1Click(Sender: TObject);
    procedure Vectormap1Click(Sender: TObject);
    //procedure Shorttable2Click(Sender: TObject);
    procedure NewDEMButtonClick(Sender: TObject);
    procedure NewSATButtonClick(Sender: TObject);
    procedure LOSButtonClick(Sender: TObject);
    procedure CoordButtonClick(Sender: TObject);
    procedure InOutButtonClick(Sender: TObject);
    procedure VectorMapButtonClick(Sender: TObject);
    procedure PerspectiveButtonClick(Sender: TObject);
    procedure MGTMagModelSpeedButtonClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FlySpeedButtonClick(Sender: TObject);
    procedure Options1Click(Sender: TObject);
    procedure Exitprogram2Click(Sender: TObject);
    procedure CloseallDEMs1Click(Sender: TObject);
    procedure CloseallImagery1Click(Sender: TObject);
    procedure CloseAlldataandwindows1Click(Sender: TObject);
    procedure Opendatasets1Click(Sender: TObject);
    //procedure Slopealgorithms1Click(Sender: TObject);
    procedure Flythrough1Click(Sender: TObject);
    procedure Perspective1Click(Sender: TObject);
    procedure Circlearound1Click(Sender: TObject);
    procedure LiveFlySpeedButtonClick(Sender: TObject);
    procedure StereoNetButtonClick(Sender: TObject);
    procedure Savedesktopandexitprogram1Click(Sender: TObject);
    procedure SpeedButton8Click(Sender: TObject);
    procedure MetaData1Click(Sender: TObject);
    procedure GeoTIFF1Click(Sender: TObject);
    procedure ESRIshapefile1Click(Sender: TObject);
    procedure Militaryicongenerator1Click(Sender: TObject);
    procedure SpeedButton9Click(Sender: TObject);
    procedure LOS2Click(Sender: TObject);
    procedure Discussionforum1Click(Sender: TObject);
    procedure MultProfSpeedButtonClick(Sender: TObject);
    procedure DTED1Click(Sender: TObject);
    procedure TernarySpeedButtonClick(Sender: TObject);
    procedure StratcolSpeedButtonClick(Sender: TObject);
    procedure SeaFloorAgeSpeedButtonClick(Sender: TObject);
    procedure PredBathySpeedButtonClick(Sender: TObject);
    procedure PlateRotateSpeedButtonClick(Sender: TObject);
    procedure NASABlueMarbleSpeedButtonClick(Sender: TObject);
    //procedure Stateplanecoordinatesystem1Click(Sender: TObject);
    procedure UKOSgrid1Click(Sender: TObject);
    procedure UTMordatumconversion1Click(Sender: TObject);
    procedure TINSpeedButtonClick(Sender: TObject);
    procedure Ternarydiagram1Click(Sender: TObject);
    procedure Stratcol2Click(Sender: TObject);
    procedure Platereconstructions1Click(Sender: TObject);
    procedure Newpanorama1Click(Sender: TObject);
    procedure PanoramaSpeedButtonClick(Sender: TObject);
    procedure FinnishGaussKruger1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Webpagethumbnails1Click(Sender: TObject);
    procedure TigerSpeedButtonClick(Sender: TObject);
    procedure Conicconversions1Click(Sender: TObject);
    procedure Polarstereographic1Click(Sender: TObject);
    procedure Verticalearthcurvature1Click(Sender: TObject);
    procedure Horizontalearthcurvature1Click(Sender: TObject);
    //procedure Pointspacing1Click(Sender: TObject);
    //procedure PanoramaAdmin1Click(Sender: TObject);
    procedure OpenTIGERcountrymap1Click(Sender: TObject);
    procedure Openproject1Click(Sender: TObject);
    procedure CTD1Click(Sender: TObject);
    procedure Lightdata1Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure StatusBar1DblClick(Sender: TObject);
    procedure OpenDatabase1Click(Sender: TObject);
    procedure Openshapefilemap1Click(Sender: TObject);
    procedure MrSidimagery1Click(Sender: TObject);
    procedure Geostatisticalanalysis1Click(Sender: TObject);
    procedure Computations1Click(Sender: TObject);
    procedure LandCoverSpeedButtonClick(Sender: TObject);
    procedure IntDBSpeedButtonClick(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure OpenTIGERcountymap1Click(Sender: TObject);
    procedure OpenDEM1Click(Sender: TObject);
    procedure OpenImage1Click(Sender: TObject);
    procedure OpenVectormap1Click(Sender: TObject);
    procedure OpenDatabasewithoutmap1Click(Sender: TObject);
    procedure Openhyperspectralimagery1Click(Sender: TObject);
    procedure OpenUSstatesmap1Click(Sender: TObject);
    procedure OpenUScountiesmap1Click(Sender: TObject);
    procedure Correlationmatrix1Click(Sender: TObject);
    procedure Opengooglemaps1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure Tools1Click(Sender: TObject);
    procedure CompressDecompress1Click(Sender: TObject);
    procedure Spectrallibrary1Click(Sender: TObject);
    procedure SpeedButton5Click(Sender: TObject);
    procedure Spectrallibrary2Click(Sender: TObject);
    procedure Sieve1Click(Sender: TObject);
    procedure LASfile1Click(Sender: TObject);
    procedure extEditor1Click(Sender: TObject);
    procedure Showfirstlinestextfile1Click(Sender: TObject);
    procedure Openpointclouds1Click(Sender: TObject);
    procedure StatusBar1Click(Sender: TObject);
    procedure LatlongofPLSSlocation1Click(Sender: TObject);
    procedure MergewavelengthheightDBFs1Click(Sender: TObject);
    procedure NLCD20011Click(Sender: TObject);
    procedure ToolBar1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure UTM1Click(Sender: TObject);
    //procedure SPCS1Click(Sender: TObject);
    procedure OpenandmergeDEMs1Click(Sender: TObject);
    procedure ImportCTDfile1Click(Sender: TObject);
    procedure EXIFmetadata1Click(Sender: TObject);
    procedure EXIFimage1Click(Sender: TObject);
    procedure Clustergrids1Click(Sender: TObject);
    procedure Copyfile1Click(Sender: TObject);
    procedure Distancematrix1Click(Sender: TObject);
    procedure XYZshapefile1Click(Sender: TObject);
    //procedure Seriesdifferences1Click(Sender: TObject);
    procedure N81Sfileviewer1Click(Sender: TObject);
    procedure CopyDBFstoXML1Click(Sender: TObject);
    procedure BackupprogramEXE1Click(Sender: TObject);
    procedure Stereopair1Click(Sender: TObject);
    procedure MSTsidescanimport1Click(Sender: TObject);
    procedure XTFindex1Click(Sender: TObject);
    procedure Findmatchingfiles1Click(Sender: TObject);
    procedure XTFsidescan1Click(Sender: TObject);
    procedure Fullworldimage1Click(Sender: TObject);
    procedure Micronetquiz1Click(Sender: TObject);
    procedure SheepRange1Click(Sender: TObject);
    procedure Afar1Click(Sender: TObject);
    procedure Updatehelpfile2Click(Sender: TObject);
    procedure TulaFracturezonemagnetics1Click(Sender: TObject);
    //procedure IndianOcean1Click(Sender: TObject);
    procedure Platerotations1Click(Sender: TObject);
    procedure Sedimentationrates1Click(Sender: TObject);
    //procedure BHRdirft1Click(Sender: TObject);
    //procedure Vasa1Click(Sender: TObject);
    //procedure Ghostship1Click(Sender: TObject);
    procedure Triplejunctions1Click(Sender: TObject);
    //procedure BearTrap1Click(Sender: TObject);
    //procedure Antares1Click(Sender: TObject);
    //procedure Combinedsurvey1Click(Sender: TObject);
    procedure Trenchgeometry1Click(Sender: TObject);
    procedure SedThickButtonClick(Sender: TObject);
    procedure Italyfocalmechs1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Statsfortrainingset1Click(Sender: TObject);
    procedure Statesforblockgrids1Click(Sender: TObject);
    procedure Normalizegrids1Click(Sender: TObject);
    procedure Addnormaliziedstatsforblockgridstotrainingset1Click(Sender: TObject);
    procedure Annapolisredistricting1Click(Sender: TObject);
    procedure Duckbeachsurveys1Click(Sender: TObject);
    procedure Piracywindsandrain1Click(Sender: TObject);
    procedure OpenDEMwithoutmap1Click(Sender: TObject);
    procedure Megathrusts1Click(Sender: TObject);
    procedure NewglobalgridGreenwich1Click(Sender: TObject);
    procedure NewglobalgridIDL1Click(Sender: TObject);
    procedure ConvertDBFsfor64bit1Click(Sender: TObject);
    procedure Landsatbrowseindex1Click(Sender: TObject);
    procedure Mercator1Click(Sender: TObject);
    procedure Southpolarstereographic1Click(Sender: TObject);
    procedure Openmultigrids1Click(Sender: TObject);
    procedure Solarpositiln1Click(Sender: TObject);
    procedure Graysgame1Click(Sender: TObject);
    procedure MH370region1Click(Sender: TObject);
    procedure Geoidandsedimentdistribution1Click(Sender: TObject);
    procedure MagMapButtonClick(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure Californiaoffshore1Click(Sender: TObject);
    procedure GulfofMexicoGLORIA1Click(Sender: TObject);
    procedure Atlantis1Click(Sender: TObject);
    procedure DEMsummarytable1Click(Sender: TObject);
    procedure Zeorlog1Click(Sender: TObject);
    procedure Landsatfullsceneindex1Click(Sender: TObject);
    procedure Satellitepredictions1Click(Sender: TObject);
    //procedure OpenTCPinterface1Click(Sender: TObject);
    procedure Maxwellplanning1Click(Sender: TObject);
    procedure Bathymetrygrid1Click(Sender: TObject);
    procedure Chart12263180K1Click(Sender: TObject);
    procedure Chart12282125K1Click(Sender: TObject);
    procedure Chart12283110K1Click(Sender: TObject);
    procedure Subset81Ssidescan1Click(Sender: TObject);
    procedure GISdatasampler1Click(Sender: TObject);
    procedure OpenScannedmap1Click(Sender: TObject);
    procedure Openlandcover1Click(Sender: TObject);
    procedure imecores1Click(Sender: TObject);
    procedure Sealevelrise1Click(Sender: TObject);
    procedure Ages1Click(Sender: TObject);
    procedure Magneticanomaliesgrid1Click(Sender: TObject);
    procedure Sedimentthicknessgrid1Click(Sender: TObject);
    procedure Sedimenttypegrid1Click(Sender: TObject);
    procedure Fontsinstalled1Click(Sender: TObject);
    procedure Sensorcoverage1Click(Sender: TObject);
    procedure Unicodeicongenerator1Click(Sender: TObject);
    procedure UKOSgrid2Click(Sender: TObject);
    procedure HarpersFerryTerrainAnalysis1Click(Sender: TObject);
    procedure Nyquist1Click(Sender: TObject);
    procedure Onlinehelp1Click(Sender: TObject);
    procedure AnnapolisTM8scene1Click(Sender: TObject);
    procedure OpenandmergeDEMdirectories1Click(Sender: TObject);
    procedure SanitizedXTF1Click(Sender: TObject);
    procedure SpeedButton6Click(Sender: TObject);
    procedure est1Click(Sender: TObject);
    //procedure Subbottomandsediments1Click(Sender: TObject);
    procedure LatLong1Click(Sender: TObject);
    procedure EditDEMHeader1Click(Sender: TObject);
    //procedure OpenGLtwogrids1Click(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
    procedure Seismicviewing1Click(Sender: TObject);
    procedure RestorepreviousprogramEXE1Click(Sender: TObject);
    procedure DBFfile1Click(Sender: TObject);
    procedure GeoPDF1Click(Sender: TObject);
    procedure OpenGeoPDF1Click(Sender: TObject);
    procedure lasinfo1Click(Sender: TObject);
    procedure KMLKMZfile1Click(Sender: TObject);
    procedure XML1Click(Sender: TObject);
    procedure Nightlights1Click(Sender: TObject);
    procedure OpenGeoPDFimagelayer1Click(Sender: TObject);
    procedure Allindividuallayers1Click(Sender: TObject);
    procedure LASlidarpointcloudsamples1Click(Sender: TObject);
    procedure Legislativeredistricting1Click(Sender: TObject);
    procedure Zipatoneeditor1Click(Sender: TObject);
    procedure Superimposedtopoprofiles1Click(Sender: TObject);
    procedure Other3Dpointclouds1Click(Sender: TObject);
    procedure LASdata1Click(Sender: TObject);
    procedure OpenLASpointcloud1Click(Sender: TObject);
    procedure opengl1tofrontClick(Sender: TObject);
    procedure Showfirstbytesofbinaryfile1Click(Sender: TObject);
    procedure Openimagewithmultiplebands1Click(Sender: TObject);
    procedure Solstice1Click(Sender: TObject);
    procedure DecemberSolstice1Click(Sender: TObject);
    procedure Equinox1Click(Sender: TObject);
    procedure Openclimatemonthlymultigrids1Click(Sender: TObject);
    procedure Evapospriaitonversustemperature1Click(Sender: TObject);
    procedure Closeallgraphs1Click(Sender: TObject);
    procedure Closeallmaps1Click(Sender: TObject);
    procedure Closealltexteditwindows1Click(Sender: TObject);
    procedure Differencetwobitmaps1Click(Sender: TObject);
    procedure Closeallpictureviewwindows1Click(Sender: TObject);
    procedure hreeviews1Click(Sender: TObject);
    procedure Monthlyclimateparameters1Click(Sender: TObject);
    //procedure Createmoviefromstills1Click(Sender: TObject);
    procedure Lidarandbeacherosion1Click(Sender: TObject);
    procedure Closewindows1Click(Sender: TObject);
    procedure Monthlywinds1Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Physicalgeographylabs1Click(Sender: TObject);
    procedure Climatestationsforclimographs1Click(Sender: TObject);
    procedure Classificationmap1Click(Sender: TObject);
    procedure Hurricanes1Click(Sender: TObject);
    procedure Dailytemperaturerange1Click(Sender: TObject);
    procedure Gridswithmonthlyprecipitationandtemperature1Click(Sender: TObject);
    procedure Geoid1Click(Sender: TObject);
    procedure Monthlyclimatologies1Click(Sender: TObject);
    procedure Addproject1Click(Sender: TObject);
    procedure OGRinfo1Click(Sender: TObject);
    procedure RenameJPGsfromlockscreen1Click(Sender: TObject);
    procedure Leastcostpath1Click(Sender: TObject);
    procedure Planarprojections1Click(Sender: TObject);
    procedure Cylindricalprojections1Click(Sender: TObject);
    procedure Conicprojections1Click(Sender: TObject);
    procedure All1Click(Sender: TObject);
    procedure Viewdebuglog1Click(Sender: TObject);
    procedure Landsatimage1Click(Sender: TObject);
    procedure Monthlyinsolation1Click(Sender: TObject);
    procedure UTMprojectoiin1Click(Sender: TObject);
    procedure LabSpeedButton7Click(Sender: TObject);
    procedure Structuralgeologylab1Click(Sender: TObject);
    procedure Koppenmonthlytemperatureandprecipitationclimatologies1Click(Sender: TObject);
    procedure Sedimenttypes1Click(Sender: TObject);
    procedure Sdeimentthickness1Click(Sender: TObject);
    procedure Geoid2Click(Sender: TObject);
    procedure Platetectonics1Click(Sender: TObject);
    procedure Quickplatetectonicsmaps1Click(Sender: TObject);
    procedure Seismicfencediagram1Click(Sender: TObject);
    procedure NaturalEarthvectoroutlines1Click(Sender: TObject);
    procedure Geology2Click(Sender: TObject);
    procedure Climate1Click(Sender: TObject);
    procedure ETOPO11Click(Sender: TObject);
    procedure Slidesorter1Click(Sender: TObject);
    procedure Movefileswithnamematch1Click(Sender: TObject);
    procedure RenameJPEGswithcreationtime1Click(Sender: TObject);
    procedure Mergemasp1Click(Sender: TObject);
    procedure Batchchangepartoffilenames1Click(Sender: TObject);
    procedure OpenandmergeGeotiffs1Click(Sender: TObject);
    procedure DragonPlot1Click(Sender: TObject);
    procedure Annapolislidar1Click(Sender: TObject);
    procedure Landsattimeseries1Click(Sender: TObject);
    procedure OpenSentinen2image1Click(Sender: TObject);
    procedure Openlidarmatchedgrids1Click(Sender: TObject);
    procedure Importfromcamera1Click(Sender: TObject);
    procedure RenameJPRGswithbasenamenumber1Click(Sender: TObject);
    procedure RenameJPEGSwithbaseandcreationtime1Click(Sender: TObject);
    procedure Labs1Click(Sender: TObject);
    procedure CloseallDBs1Click(Sender: TObject);
    //procedure Datadownloadpicksubset1Click(Sender: TObject);
    procedure CloseprogramupdateEXEnewversion7MBdownload1Click(Sender: TObject);
    procedure Spectrallibrary3Click(Sender: TObject);
    procedure RGBcolorlayers1Click(Sender: TObject);
    procedure GDALSRSinfo1Click(Sender: TObject);
    procedure WhiteboxGeotiff1Click(Sender: TObject);
    procedure GPS1Click(Sender: TObject);
    procedure GDALslopesarcsecondDEMs1Click(Sender: TObject);
    procedure Mediansatellitedatacontest1Click(Sender: TObject);
    procedure Makelittletilescontest1Click(Sender: TObject);
    procedure LidarandglobalDEMs1Click(Sender: TObject);
    procedure Guam1Click(Sender: TObject);
    procedure GDALWKT1Click(Sender: TObject);
    procedure N28Click(Sender: TObject);
    procedure Postprocesscontest1Click(Sender: TObject);
    procedure WKTprojection1Click(Sender: TObject);
    procedure ArcsecondspacingDTEDDGED1Click(Sender: TObject);
    procedure UTMspacingandgridtrueangle1Click(Sender: TObject);
    procedure CreateDEMsfromlidar1Click(Sender: TObject);
    procedure CategoriesfromallopenDEMs1Click(Sender: TObject);
    procedure Categoriesfromdatabase1Click(Sender: TObject);
    procedure Subsamplecomparethinaverage1Click(Sender: TObject);
    procedure Createcompositebitmap1Click(Sender: TObject);
    procedure Arcsecondrectangularpixels1Click(Sender: TObject);
    procedure DEMIXtiles1Click(Sender: TObject);
    procedure DEMcornerstable1Click(Sender: TObject);
    procedure netcdf1Click(Sender: TObject);
    procedure ACOLITEallopensatelliteimages1Click(Sender: TObject);
    procedure Fatfingers1Click(Sender: TObject);
    procedure Datadownloadpicksubset1Click(Sender: TObject);
    procedure Datadownload25GB1Click(Sender: TObject);
    procedure DEMIXwinecontest1Click(Sender: TObject);
    procedure UpdateEurekaValleyDEM1Click(Sender: TObject);
    procedure Closeprogramgetdebugversionoftheprogram7MB1Click(Sender: TObject);
    procedure KangarooIslandadditionalscenes1Click(Sender: TObject);
    procedure Openrecyclebin1Click(Sender: TObject);
    procedure Existingfile1Click(Sender: TObject);
    procedure Existingfile2Click(Sender: TObject);
    procedure Horizontalimageslider1Click(Sender: TObject);
    procedure Allgraphsononeimage1Click(Sender: TObject);
    procedure OpensingleLandsatband1Click(Sender: TObject);
    procedure listgeo1Click(Sender: TObject);
    procedure DEMIXelevationhistograms1Click(Sender: TObject);
    procedure DEMIXcreatereferenceDEMs1Click(Sender: TObject);
  private
    procedure SunViews(Which : integer);
    procedure SeeIfThereAreDebugThingsToDo;

    { Private declarations }
  public
    { Public declarations }
      ProgramClosing,NoAutoOpen,AskForDebugUpdateNow,AskForNewUpdateNow,ShowDragonPlot : boolean;
      procedure SetMenusForVersion;
      procedure FormPlacementInCorner(TheForm : Forms.tForm; FormPosition :  byte = lpSEMap);
      procedure HandleThreadTerminate(Sender: TObject);
      procedure SetPanelText(PanelNum : integer; What : shortString);
      procedure StartSealevelrise(BaseMap : tMapForm);
  end;

const
   IDDirToMark : PathStr = '';

var
   wmdem : Twmdem;
   ShowLoadButtons,LockStatusBar,
   SkipMenuUpdating,FirstRun : boolean;
   OnVasaPage : integer;


//procedure SunOrMoon(LocationSet : boolean; Lat,Long : float64);
function OpenGazFile(fName : PathStr = '') : integer;
procedure InsureFormOnScreen(Form4 : tForm; x,y : integer);


implementation

{$R *.DFM}

uses

{$IfDef ExIndexes}
{$Else}
   DEM_indexes,
{$EndIf}

{$IfDef ExGeology}
{$Else}
   sieve_main,
{$EndIf}

   PETImage,
   fat_fingers,


{$IfDef ExSat}
{$Else}
   DEMEros,
   GEOTIFF,
   DEM_Sat_Header,
   MrSidImagery,
   DEM_NLCD,
   {$IfDef ExHypImage}
   {$Else}
      Hyperspectral_image,
   {$EndIf}
{$EndIf}

{$IfDef ExAltimeter}
{$Else}
   AltCommR,
{$EndIf}

   {$IfDef ExSidescan}
   {$Else}
      ChirpGrf,
      MST_format,
      SideImg,
   {$EndIf}


{$IfDef ExGeography}
{$Else}
   sc_ColMain,
{$EndIf}

{$IfDef ExGeography}
{$Else}
   Sun_Position,
   KoppenGr,
   moon_montenbruk_pfleger,
{$EndIf}

{$IfDef ExConvert}
{$Else}
   DEMCnvrt,
{$EndIf}

//{$IfDef ExDP}
//{$Else}   //these are always compiled so that they remain current
   Dragon_plot_init,
   dp_control,
//{$EndIf}


{$IfDef ExVectorOverlay}
{$Else}
   DEMTerrC,
   DEMTiger,
{$EndIf}

{$IfDef ExDataManip}
{$Else}
   DEMHandW,
{$EndIf}

{$IfDef ExGeoStats}
{$Else}
    DEMStat,
    PETFouri,
    PetCorrl,
    NetMainW,
    demtrendopt,
    MEM_Power_Spect,
{$EndIf}

{$IfDef Ex3D}
{$Else}
   DEMPersW,
{$EndIf}

{$IfDef ExGIS}
{$Else}
   demdatabase,
   DataBaseCreate,
   DataBaseAddRec,
   DEMESRIShapeFile,
{$EndIf}

   {$IfDef ExSetOptions}
   {$Else}
      DEMOptions,
   {$EndIf}

   {$IfDef ExConvert}
   {$Else}
      //SPCS_Converter,
      UK_OS_Converter,
      computations,
      DEM_computations,
   {$EndIf}

   {$IfDef ExHypImage}
   {$Else}
      hyp_display,
   {$EndIf}

   {$IfDef ExMilIcons}
   {$Else}
      dem_milicon,
   {$EndIf}

   {$IfDef ExOceanography}
   {$Else}
      OCEANCAL,
   {$EndIf}

   {$IfDef ExTIN}
   {$Else}
      DEM_TIN,
   {$EndIf}

   {$IfDef ExRedistrict}
   {$Else}
      demredistrict,
   {$EndIf}

   {$IfDef ExTrackSat}
   {$Else}
      trackstarmain,
   {$EndIf}


   {$IfDef ExComplexGeoStats}
   {$Else}
      pick_geostats,
      DEM_Optimal_Lag,
   {$EndIf}

   {$IfDef ExTools}
   {$Else}
      compress_form,
      MD_use_tools,
      net_quiz,
   {$EndIf}

   {$IfDef ExLASlidar}
   {$Else}
      slicer_3d,
      las_lidar,
      lidar_multiple_grid_display,
      point_cloud_options,
   {$EndIf}


  {$IfDef ExExif}
  {$Else}
     JpegDumpForm,
  {$EndIf}

   {$IfDef ExFMX3D}
   {$Else}
   View3D_Main,
   {$EndIf}

   {$IfDef IncludeFMX3DMesh}
   MainForm_3dMeshDrape,
   {$EndIf}

   {$IfDef ExMultiGrid}
   {$Else}
      MultiGrid,
      Monthly_Grids,
   {$EndIf}


   {$IfDef ExCartography}
   {$Else}
      DEM_cart_proj,
   {$EndIf}

   {$IfDef ExMake_grid}
   {$Else}
      Make_grid,
   {$EndIf}

   {$IfDef ExDP}
   {$Else}
      //gps_sensor,
   {$EndIf}

   {$If Defined(Include2019datafusion) or Defined(Include2021datafusion)}
      experimental_md,
   {$EndIf}


   //ContGraf,


   Slider_sorter_form,
   NyqGraph,
   gdal_tools,

   PetImage_Form,
   TerSplsh,
   //U_SolarPos2,
   stereo_viewer,
   get_thumbnails,
   Main_Gray_game,
   DEM_manager,
   new_petmar_movie,

  // GetMapF,
   lcp_options,
   ScreenUnicode,
   DEMSlopeCompare,
   PetDBUtils,
   Make_tables,
   DEMDef_routines,
   zipatone,
   GetLatLn,
   KML_creator,

   PETMath,BaseGraf,
   DEMCoord,
   BaseMap,
   Thread_timers,
   PetEd32,
   line_from_points,
   Demeditw,
   NE_outlines,
   map_splitter,
   dem_gaz_opts, us_properties, demstringgrid,
   //gis_legend,
   Get_PLSS, //Get_SPCS,
   //dem_browser,
   DEMdbTable,
   Petmar_ini_file,
   db_display_options,
   toggle_db_use, map_overlays, U_SolarPos2;


{$IfDef IncludeASTERGDEMassessment}
   {$I c:\turbo\monster_microdem\research_options\astergdem.pas}
{$EndIf}

   {$I nevadia_main_batch.inc}



procedure InsureFormOnScreen(Form4 : tForm; x,y : integer);
begin
   if y + Form4.Height < wmDEM.ClientHeight then Form4.Top := y
   else Form4.Top := wmDEM.ClientHeight - Form4.Height - 10;
   if x + Form4.Width < wmDEM.ClientWidth then Form4.Left := x
   else Form4.Left := wmDEM.ClientWidth - Form4.Width - 10;
end;


procedure Twmdem.FormPlacementInCorner(TheForm : Forms.tForm; FormPosition :  byte = lpSEMap);
begin
   PlaceFormInCorner(Self,TheForm,FormPosition);
end;


procedure Twmdem.SetPanelText(PanelNum : integer; What : shortString);
begin
   if not LockStatusBar then begin
      wmDEM.StatusBar1.Panels[PanelNum].Text := What;
      ApplicationProcessMessages;
   end;
end;


procedure Twmdem.Fatfingers1Click(Sender: TObject);
begin
   StopSplashing;
   StartFatFingers(VectorMap[SetUpVectorMap(true,true)]);
end;


procedure Twmdem.Findmatchingfiles1Click(Sender: TObject);
var
   ThePath : PathStr;
   Ext : extstr;
   i,SubDirs : integer;
   TheFiles,TheMatches : tStringList;
   NameContains : shortString;
begin
   ThePath :=  '';
   GetDosPath('file matching',ThePath);
   SubDirs := 5;
   Petmar.ReadDefault('subdirectoy level',SubDirs);
   Ext := '.xtf';
   Petmar.GetString('file extension',Ext,false,ValidDosFileNameChars);
   Ext := '*' + Ext;
   NameContains := '000';
   Petmar.GetString('name contains',NameContains,false,ValidDosFileNameChars);
   TheFiles := nil;
   ShowHourglassCursor;

   Petmar.FindMatchingFiles(ThePath,Ext,TheFiles,SubDirs);
   TheMatches := tStringList.Create;
   TheMatches.Sorted := true;
   for i := 0 to pred(TheFiles.Count) do begin
      if (NameContains = '') or StrUtils.AnsiContainsText(TheFiles.Strings[i],NameContains) then TheMatches.Add(TheFiles.Strings[i]);
   end;
   TheFiles.Free;
   DisplayAndPurgeStringList(TheMatches,'Files containing ' + NameContains + '  , n=' + IntToStr(TheMatches.Count));
   ShowDefaultCursor;
end;


procedure Twmdem.HandleThreadTerminate(Sender: TObject);
begin
   inherited;
end;


procedure Twmdem.Vectormap1Click(Sender: TObject);
begin
   {$IfDef RecordMenu} writeLineToDebugFile('Twmdem.Vectormap1Click'); {$EndIf}
   VectorMapButtonClick(Sender);
end;


procedure Twmdem.AboutMICRODEM1Click(Sender: TObject);
begin
   PETMARAboutBox(ShortEXEName);
end;


procedure Twmdem.Newarea1Click(Sender: TObject);
begin
   {$If Defined(RecordMenu) or Defined(TimeLoadDEM))}  Petmar_types.WriteLineToDebugFile('Twmdem.Newarea1Click in'); {$EndIf}
   LastDEMLoaded := OpenNewDEM;
   if MDDef.AutoLoadVegGrid then DEMGlb[LastDEMLoaded].SelectionMap.Loadvegetationgrid1Click(Nil);
   if MDDef.AutoLoadVegDensityGrids then DEMGlb[LastDEMLoaded].SelectionMap.Loadvegetationlayers1Click(Nil);
   {$If Defined(RecordMenu) or Defined(TimeLoadDEM))}  Petmar_types.WriteLineToDebugFile('Twmdem.Newarea1Click out'); {$EndIf}
end;


procedure YPMaps(What : shortString);
var
   bDir,BaseName,dName,zName,fName : PathStr;
begin
   StopSplashing;
   BaseName := 'yp686_ops';
   bDir := MainMapData + BaseName + '\';
   if (Not PathIsValid(BDir)) then begin
      zName := BaseName + '.zip';
      dName := MainMapData + zName;
      if DownloadFileFromWeb(WebDataDownLoadDir + zName,dName) then begin
         ZipMasterUnzip(dName,MainMapData);
      end
      else begin
         MessageToContinue('Download fail : ' + zName);
         exit;
      end;
   end;
   if (what = 'Bathy') then begin
      fName := bDir + 'annap_bathy.DEM';
      LoadNewDEM(LastDEMLoaded,fName,true);
   end
   else begin
      fname := bDir + 'chart_'+  what + '.tif';
      OpenAndDisplayNewScene(Nil,fName,true,true,false);
   end;
end;


procedure Twmdem.LOS1Click(Sender: TObject);
begin
   {$IfDef RecordMenu} WriteLineToDebugFile('Twmdem.LOS1Click'); {$EndIf}
   if (Sender = SpeedButton3) or (MDDef.ProgramOption = GeologyProgram) then ChangeDEMNowDoing(SeekingTopoProfile)
   else ChangeDEMNowDoing(SeekingLOS);
end;


procedure Twmdem.Cascade1Click(Sender: TObject);
begin
   Cascade;
end;

procedure Twmdem.CategoriesfromallopenDEMs1Click(Sender: TObject);
begin
   LandCoverBarGraphs(false,false,false);
end;

procedure Twmdem.Categoriesfromdatabase1Click(Sender: TObject);
begin
   LandCoverBarGraphs(true,false,false);
end;

procedure Twmdem.Chart12263180K1Click(Sender: TObject);
begin
   YPMaps('12263');
end;

procedure Twmdem.Chart12282125K1Click(Sender: TObject);
begin
   YPMaps('12282');
end;

procedure Twmdem.Chart12283110K1Click(Sender: TObject);
begin
   YPMaps('12283');
end;

procedure Twmdem.Tile1Click(Sender: TObject);
begin
   Tile;
end;


procedure Twmdem.Timer2Timer(Sender: TObject);
//keeps screen active for GPS operations and long processing operations
//without this you can lose data if the operating system shuts you down
const
   KEYEVENTF_KEYDOWN = 0;
begin
   if MDDef.StayAwake then begin
      keybd_event(VK_F15, 0, KEYEVENTF_KEYDOWN, 0);
      keybd_event(VK_F15, 0, KEYEVENTF_KEYUP, 0);
   end;
end;


procedure Twmdem.Dailytemperaturerange1Click(Sender: TObject);
begin
   {$IfDef ExMultiGrid}
   {$Else}
      OpenDailyTemps;
   {$EndIf}
end;

procedure Twmdem.Data1Click(Sender: TObject);
begin
   {$IfDef ExDataManip}
   {$Else}
      StopSplashing;
      {$IfDef RecordMenu} writeLineToDebugFile('Twmdem.Data1Click (In<>Out)'); {$EndIf}
      DEMHandForm := TDEMHandForm.Create(Application);
      DEMHandForm.ShowModal;
   {$EndIf}
end;


procedure Twmdem.Datadownload25GB1Click(Sender: TObject);
begin
   DownloadandUnzipDataFileIfNotPresent('eureka_valley_landslide');
end;

procedure Twmdem.Datadownloadpicksubset1Click(Sender: TObject);
begin
   DownloadandUnzipDataFileIfNotPresent('kangaroo_island_pa');
end;

procedure Twmdem.DBFfile1Click(Sender: TObject);
begin
   if GetFileFromDirectory('dBase DBF file','*.dbf',LastDataBase) then ScreenDBFFileDump(LastDataBase);
end;

procedure Twmdem.DecemberSolstice1Click(Sender: TObject);
begin
    SunViews(2);
end;

procedure Twmdem.DEMcornerstable1Click(Sender: TObject);
begin
   DoGridSpacingAndDeclination(0);
end;

procedure Twmdem.DEMIXcreatereferenceDEMs1Click(Sender: TObject);
begin
   BatchResampleForDEMIX;
end;


procedure Twmdem.DEMIXelevationhistograms1Click(Sender: TObject);
var
   ElevFiles,LegendFiles : tStringList;
   DefaultFilter : byte;
   TStr : string;
   i : integer;
begin
   {$IfDef RecordDEMIX} writeLineToDebugFile('Twmdem.DEMIXelevationhistograms1Click in'); {$EndIf}
   StopSplashing;
   ElevFiles := tStringList.Create;
   ElevFiles.Add('H:\demix_wine_contest\wine_results\05-06-22--0904-50_elev_diffs\elev_diff_hists\');
   DefaultFilter := 1;
   if GetMultipleFiles('Files with elevation distributions','Files|*.z',ElevFiles,DefaultFilter) then begin
      LegendFiles := tStringList.Create;
      for I := 0 to pred(ElevFiles.Count) do begin
         TStr := ExtractFileNameNoExt(ElevFiles[i]);
         LegendFiles.Add(BeforeSpecifiedCharacter(TStr,'-'));
         {$IfDef RecordDEMIX} writeLineToDebugFile('Twmdem.DEMIXelevationhistograms1Click ' + Tstr); {$EndIf}
      end;
      CreateMultipleHistograms(MDDef.CountHistograms,ElevFiles,LegendFiles,'Elevation difference','Elevation difference distribution');
   end;
   {$IfDef RecordDEMIX} writeLineToDebugFile('Twmdem.DEMIXelevationhistograms1Click out'); {$EndIf}
end;

procedure Twmdem.DEMIXtiles1Click(Sender: TObject);
begin
   DEMIXtilesStats;
end;


procedure Twmdem.DEMIXwinecontest1Click(Sender: TObject);
begin
   ComputeDEMIXstats;
end;


procedure Twmdem.DEMsummarytable1Click(Sender: TObject);
var
   Results : tStringList;
   fName : PathStr;
   Missing : float64;
   i,Decs : integer;
   TStr : shortstring;
begin
   Results := tStringList.Create;
   Results.Add('DEM,LAT,LONG,MIN_Z,MAX_Z,HOLES_PC,SW_CornerX,SW_CornerY,SW_Corner,DX,DY,NUM_COL,NUM_ROW,PIXEL_IS,AVG_X_M,AVG_Y_M,AVG_SP_M');
   for i := 1 to MaxDEMDataSets do if ValidDEM(i) then begin
      DEMGlb[i].ComputeMissingData(Missing);
      if (DEMGlb[i].DEMheader.DEMUsed = UTMBasedDEM) then Decs := -2 else Decs := -8;
      if DEMGlb[i].DEMheader.DEMUsed = UTMBasedDEM then TStr := ''
      else TStr := LatLongDegreeToString(DEMGlb[i].DEMheader.DEMSWCornerY, DEMGlb[i].DEMheader.DEMSWCornerX,DecSeconds);

      Results.Add(DEMGlb[i].AreaName + ',' + RealToString(DEMGlb[i].DEMSWcornerLat + 0.5 * DEMGlb[i].LatSizeMap,-12,-3) + ',' +
          RealToString(DEMGlb[i].DEMSWcornerLong + 0.5 * DEMGlb[i].LongSizeMap,-12,-3)  + ',' +
          RealToString(DEMGlb[i].DEMheader.MinElev,-12,-1)  + ',' +  RealToString(DEMGlb[i].DEMheader.MaxElev,-12,-1)  + ',' +
          RealToString(Missing,-12,-3) + ',' +  RealToString(DEMGlb[i].DEMheader.DEMSWCornerX,-12,Decs)  + ',' +RealToString(DEMGlb[i].DEMheader.DEMSWCornerY,-12,Decs)  + ',' +
          TStr + ',' +
          RealToString(DEMGlb[i].DEMheader.DEMxSpacing,-12,Decs) + ',' + RealToString(DEMGlb[i].DEMheader.DEMySpacing,-12,Decs)  + ',' +
          IntToStr(DEMGlb[i].DEMheader.NumCol) + ',' + IntToStr(DEMGlb[i].DEMheader.NumRow) + ',' + IntToStr(DEMGlb[i].DEMheader.RasterPixelIsGeoKey1025)
          + ',' + RealToString(DEMGlb[i].AverageXSpace,-12,-2) + ',' + RealToString(DEMGlb[i].AverageYSpace,-12,-2)  + ',' + RealToString(DEMGlb[i].AverageSpace,-12,-2));
   end;
   fName := Petmar.NextFileNumber(MDTempDir,'dem_summary_','.csv');
   StringList2CSVtoDB(Results,fName);
end;


procedure Twmdem.Loadimage1Click(Sender: TObject);
begin
   OpenImageEditor;
end;


procedure Twmdem.Contents1Click(Sender: TObject);
begin
   {$IfDef RecordHelp} WriteLineToDebugFile('Twmdem.Contents1Click for help'); {$EndIf}
   StopSplashing;
   if not FileExists(ChangeFileExt(Application.ExeName,'.chm')) then Updatehelpfile1Click(Sender);

   case MDDef.ProgramOption of
      GeologyProgram : DisplayHTMLTopic('html\struct_geol_index.htm');
      else DisplayHTMLTopic('html\microdem.htm');
   end;
end;


procedure TWmDem.SetMenusForVersion;
var
   theLeft : integer;
   ExpertDEMVersion : boolean;

      procedure CheckButton(ButtonName : TSpeedButton; bname : shortstring = '');
      begin
         if ButtonName.Visible then begin
            ButtonName.Left := theLeft;
            inc(theLeft,ButtonName.Width);
            if bname = '' then bname := 'unnamed';
            {$If Defined(RecordButtonProblems) or Defined(RecordFirstRun)}
               if FirstRun then WriteLineToDebugFile(bName + '  ' + IntToStr(theLeft) + '  left=' + IntToStr(ButtonName.Left) + '  width=' + IntToStr(ButtonName.Width));
            {$EndIf}
         end
         else begin
            ButtonName.Left := 1500;
            {$If Defined(RecordButtonProblems) or Defined(RecordFirstRun)} if FirstRun then WriteLineToDebugFile('invisible, bname=' + bName); {$EndIf}
         end;
      end;

begin
   {$If Defined(RecordButtonProblems) or Defined(RecordFirstRun)}
      if FirstRun then begin
         WriteLineToDebugFile('TWmDem.SetMenusForVersion, program mode=' + IntToStr(ord(MDDef.ProgramOption)));
         if not MDDef.ShowMainToolbar then WriteLineToDebugFile('TWmDem.SetMenusForVersion, no main toolbar');
      end;
   {$EndIf}
   if (WmDEM = Nil) or (SkipMenuUpdating) or (ProgramClosing) then exit;
   FileMode := 2;

   {$IfDef IncludeFMX3DMesh}
      est1.Visible := TrilobiteComputer;
   {$Else}
      est1.Visible := false;
   {$EndIf}

   {$IfDef ExRedistrict}
      Legislativeredistricting1.Visible := false;
   {$EndIf}
   Open1.Visible := (MDDef.ProgramOption <> DragonPlotProgram);
   Toolbar1.Visible := MDDef.ShowMainToolbar and (MDDef.ProgramOption <> DragonPlotProgram);
   Introductorytutorials1.Visible := (MDDef.ProgramOption <> DragonPlotProgram);
   Help1.Visible := (MDDef.ProgramOption <> DragonPlotProgram) or TrilobiteComputer;

   ExpertDEMVersion := (MDDef.ProgramOption in [ExpertProgram,RemoteSensingProgram]) and (NumDEMDataSetsOpen > 0);
   RemoteSensingLabs1.Visible := (MDDef.ProgramOption in [ExpertProgram,RemoteSensingProgram]);
   SpeedButton6.Visible := (MDDef.ProgramOption in [ExpertProgram]);
   //HistoricShipwrecksLabs1.Visible := (MDDef.ProgramOption in [ShipwrecksProgram]) or MDDef.ShowLabs;
   StructuralGeologylab1.Visible := (MDDef.ProgramOption in [GeologyProgram]) or MDDef.ShowLabs;

   SpeedButton2.Visible := MDDef.ShowSHPButton;
   SpeedButton4.Visible := MDDef.ShowDBonly;
   SpeedButton9.Visible := (MDDef.ProgramOption in [ExpertProgram]) and MDDef.UseGazetteer;
   MultProfSpeedButton.Visible := (MDDef.ProgramOption in [ExpertProgram,GeologyProgram,RemoteSensingProgram]);
   MultProfSpeedButton.Enabled := (NumDEMDataSetsOpen > 1);

   GISdatasampler1.Visible := (MDDef.ProgramOption in [ExpertProgram,RemoteSensingProgram]);
   {$IfDef ExOpenGL}
      OpenGL1tofront.Visible := false;
   {$Else}
      OpenGL1tofront.Visible := (View3DForm <> nil) or (Map3D <> nil);
   {$EndIf}

   CloseWindows1.Visible := (MDDef.ProgramOption <> DragonPlotProgram);

   {$IfDef RecordProblems}
   {$Else}
      Viewdebuglog1.Visible := false;
   {$EndIf}

   Tools1.Visible := not (MDDef.ProgramOption in [DragonPlotProgram]);

   Data1.Visible := (MDDef.ProgramOption in [ExpertProgram,RemoteSensingProgram]) and MDDef.ShowDataManipulation;
   InOutButton.Visible := Data1.Visible;

   Header1.Visible := (NumDEMDataSetsOpen > 0) or (NumSatImageOpen > 0);

   {$IfDef ExIndexes}
      IntDBSpeedButton.Visible := false;
   {$Else}
      IntDBSpeedButton.Visible := FileExists(MapLibraryFName) and MDDef.ShowIntDB;
   {$EndIf}

   NewDEMButton.Visible := ShowLoadButtons;

   LandCoverSpeedButton.Visible := (MDDef.ProgramOption in [ExpertProgram,RemoteSensingProgram]);

   XTFsidescan1.Visible := MDDef.ShowSidescan;

   {$IfDef ExDP}
      DragonPlot1.Visible := false;
   {$Else}
      DragonPlot1.Visible := (MDDef.ProgramOption = ExpertProgram) and ShowDragonPlot;
   {$EndIf}

   Cascade1.Visible := (MDDef.ProgramOption <> DragonPlotProgram);
   Tile1.Visible := (MDDef.ProgramOption <> DragonPlotProgram);

   Hardware1.Visible := (MDDef.ProgramOption = ExpertProgram);
   OpenDataSets1.Visible := (MDDef.ProgramOption in [ExpertProgram,DragonPlotProgram,RemoteSensingProgram]);
   ProgramLimits1.Visible := (MDDef.ProgramOption in [ExpertProgram,RemoteSensingProgram]);

   PerspectiveButton.Visible := MDDef.ShowViews and (MDDef.ProgramOption in [ExpertProgram]);

   CircleAround.Visible := (MDDef.ProgramOption = ExpertProgram) and MDDef.ShowViews;
   PanoramaSpeedButton.Visible := (MDDef.ProgramOption in [ExpertProgram]) and MDDef.ShowViews;
   PanoramaSpeedButton.Enabled := (NumDEMDataSetsOpen > 0);
   FlySpeedButton.Visible := MDDef.ShowViews and (MDDef.ProgramOption in [ExpertProgram]);
   LiveFlySpeedButton.Visible := MDDef.ShowViews and (MDDef.ProgramOption in [ExpertProgram]);
   FlyThrough1.Visible := FlySpeedButton.Visible;
   LOSButton.Visible := (MDDef.ProgramOption in [ExpertProgram]);
   //SpeedButton3.Visible := not (MDDef.ProgramOption in [ShipwrecksProgram,EconProgram]);

   LOS1.Visible := LOSButton.Visible;
   LOS1.Caption := '&LOS';

   LOSButton.Enabled := (NumDEMDataSetsOpen > 0);
   SpeedButton3.Enabled := (NumDEMDataSetsOpen > 0);
   FlySpeedButton.Enabled := (NumDEMDataSetsOpen > 0);
   PerspectiveButton.Enabled := (NumDEMDataSetsOpen > 0);
   Horizontalearthcurvature1.Visible := ExpertDEMVersion;
   Clustergrids1.Visible := ExpertDEMVersion;

  // Pointspacing1.Visible := ExpertDEMVersion;

   Superimposedtopoprofiles1.Visible := (NumDEMDataSetsOpen > 1) and (MDDef.ShowDEMcompare) and (MDDef.ProgramOption = ExpertProgram);

   Newpanorama1.Visible := (NumDEMDataSetsOpen > 0) and (MDDef.ProgramOption = ExpertProgram);
   TIGERSpeedButton.Visible := MDDef.TigrDef.ShowTIGERButton and (MDDef.ProgramOption = ExpertProgram);
   OpenTIGERcountymap1.Visible := TIGERSpeedButton.Visible;
   Openshapefilemap1.Visible := (MDDef.ProgramOption in [ExpertProgram,GeographyProgram]);
   OpenDataBaseWithoutMap1.Visible := (MDDef.ProgramOption = ExpertProgram);

   //ContourGhosts1.Visible := ExpertDEMVersion;
   DEMProperties1.Visible := ExpertDEMVersion;
   View1.Visible := (NumDEMDataSetsOpen > 0) and (MDDef.ShowViews) and (MDDef.ProgramOption <> DragonPlotProgram);

   N3Profiles1.Visible := false;
   Options1.Visible := true;
   N1.visible := (MDDef.ProgramOption = ExpertProgram);
   N5.visible := (MDDef.ProgramOption = ExpertProgram);
   LoadImage1.visible := (MDDef.ProgramOption = ExpertProgram);
   n10.visible := (MDDef.ProgramOption = ExpertProgram);

   Analyze1.Visible := (MDDef.ProgramOption = ExpertProgram) or MDDef.ShowConversionAndAnalyze;

   NASABlueMarbleSpeedButton.visible := MDDef.ShowBlueMarble and FileExists(BlueMarblefName);
   NewSATButton.Visible := ShowLoadButtons and MDDef.ShowOpenImagery;
   OpenImage1.Visible := MDDef.ShowOpenImagery;

   Annapolisebasemap1.visible := (MDDef.ProgramOption = ExpertProgram);

   LiveFlySpeedButton.Enabled := (NumDEMDataSetsOpen > 0);

   Physicalgeographylabs1.Visible := (MDDef.ProgramOption = GeographyProgram) or MDDef.ShowLabs;
   GISlabs1.Visible := MDDef.ShowLabs;
   LabSpeedButton7.Visible := (MDDef.ProgramOption in [GeographyProgram,GeologyProgram]);

   SpeedButton5.Visible := Geostatisticalanalysis1.Visible;

   VectorMapButton.Visible := MDDef.ShowCartography;
   OpenVectorMap1.Visible :=  (MDDef.ProgramOption in [ExpertProgram,GeologyProgram,GeographyProgram,RemoteSensingProgram]) or MDDef.ShowCartography;

   SpeedButton8.Visible := (MDDef.ProgramOption in [ExpertProgram,GeologyProgram,GeographyProgram,RemoteSensingProgram]);

   Oceanographicdata1.Visible := (MDDef.ProgramOption = ExpertProgram) and MDDef.ShowOceanographyOptions;

   Perspective1.Visible := PerspectiveButton.Visible;
   Batchprocessing1.Visible := (MDDef.ProgramOption = ExpertProgram);
   Edit1.Visible := (MDDef.ProgramOption = ExpertProgram) and (NumDEMDataSetsOpen = 0);
   Photos1.Visible := (MDDef.ProgramOption = ExpertProgram);

   {$IfDef ExHypImage}
      HypImageSpeedButton.Visible := false;
      Openhyperspectralimagery1.Visible := false;
   {$Else}
      HypImageSpeedButton.Visible := (MDDef.ProgramOption in [ExpertProgram,RemoteSensingProgram]) and MDDef.ShowOpenImagery;
      Openhyperspectralimagery1.Visible := (MDDef.ProgramOption in [ExpertProgram,RemoteSensingProgram]) and MDDef.ShowOpenImagery;
   {$EndIf}

   {$IfDef IncludeASTERGDEMassessment}
      ASTERGDEMassessment1.Visible := false;
   {$EndIf}

   {$IfDef ExPointCloud}
      PClouder1.Visible := false;
   {$Else}
      PClouder1.Visible := (MDDef.ProgramOption in [ExpertProgram,RemoteSensingProgram]) and MDDef.ShowPointClouds;
      Other3Dpointclouds1.Visible := (MDDef.ProgramOption in [ExpertProgram]) and MDDef.ShowPointClouds;
      //FudgeLAScoordinates1.Visible := (MDDef.ProgramOption in [ExpertProgram]) and MDDef.ShowPointClouds;
   {$EndIf}

   {$IfDef ExTIN}
      TINSpeedButton.Visible := false;
   {$Else}
      TINSpeedButton.Visible := (MDDef.ProgramOption = ExpertProgram) and MDDef.ShowTINs;
   {$EndIf}

   {$IfDef ExConvert}
      CoordButton.Visible := false;
      Computations1.Visible := false;
   {$Else}
      Computations1.Visible := true;
      CoordButton.Visible := (not (MDDef.ProgramOption in [GeologyProgram])) and MDDef.ShowConversionAndAnalyze;
   {$EndIf}

   {$IfDef ExGeoStats}
      Geostatisticalanalysis1.Visible  := false;
   {$Else}
      Geostatisticalanalysis1.Visible := ExpertDEMVersion and MDDef.ShowGeomorphometry;;
   {$EndIf}

   {$IfDef ExGeography}
      SpeedButton1.Visible := false;
      SunriseButton.Visible := false;
      SunRiseSunset1.Visible := false;
      Moonrisemoonset1.Visible := false;
   {$EndIf}

   {$IfDef ExGeology}
      SeaFloorAgeSpeedButton.Visible := false;
      PlateRotateSpeedButton.Visible := false;
      TernarySpeedButton.Visible := false;
      StratcolSpeedButton.Visible := false;
      StereoNetButton.Visible := false;
      Geology1.Visible := false;
      PlateRotateSpeedButton.Visible := false;
      StereoNet1.Visible := false;
      SedThickButton.Visible := false;
      MagMapButton.Visible := false;
      SeaFloorAgeSpeedButton.Visible := false;
      PredBathySpeedButton.Visible := false;
   {$Else}
      SeaFloorAgeSpeedButton.Visible := MDDef.ShowMarineGeology and FileExists(PredAgesFile);
      PredBathySpeedButton.Visible := MDDef.ShowGlobalDEM and FileExists(ETopoDEMName);
      SedThickButton.Visible := (MDDef.ProgramOption in [ExpertProgram,GeologyProgram]);
      MagMapButton.Visible := (MDDef.ProgramOption in [ExpertProgram,GeologyProgram]);
      StereoNetButton.Visible := MDDef.ShowStereoNet;
      TernarySpeedButton.Visible := MDDef.ShowTernary;
      StratColSpeedButton.Visible := MDDef.ShowStratcol;
      Geology1.Visible := (MDDef.ShowStratcol or MDDef.ShowTernary or MDDef.ShowStereoNet);
      PlateRotateSpeedButton.Visible := MDDef.ShowPlateRotation and FileExists(ContCrustOutlineFile);
      StereoNet1.Visible := StereoNetButton.Visible;
      TernaryDiagram1.Visible := TernarySpeedButton.Visible;
      Stratcol2.Visible := StratColSpeedButton.Visible;
      Platereconstructions1.Visible := PlateRotateSpeedButton.Visible;
      Sieve1.Visible := MDDef.ShowSieve;
   {$EndIf}


   if (not MDDef.ShowMenus) then begin
      File1.Visible := false;
      View1.Visible := false;
      Analyze1.Visible := false;
      Geology1.Visible := false;
      Options1.Visible := false;
      Window1.Visible := false;
      Help1.Visible := false;
      Oceanographicdata1.Visible := false;
      PClouder1.Visible := false;
   end;

   {$If Defined(RecordFirstRun) or Defined(RecordButton)} if FirstRun then WriteLineToDebugFile('TWmDem.SetMenusForVersion Button check'); {$EndIf}
   theLeft := 1;
   CheckButton(LabSpeedButton7,'lab');
   CheckButton(SpeedButton2,'sp2 shape');
   CheckButton(SpeedButton4,'sp4 db');
   CheckButton(SpeedButton8,'sp8 proj');
   CheckButton(IntDBSpeedButton,'int db');
   CheckButton(NewDEMButton,'dem');
   CheckButton(PredBathySpeedButton,'etopo');
   CheckButton(SedThickButton,'sed');
   CheckButton(MagMapButton,'mag map');
   CheckButton(SeaFloorAgeSpeedButton,'sf age');
   CheckButton(NewSatButton,'sat');
   CheckButton(NASABlueMarbleSpeedButton,'bm');
   CheckButton(LandCoverSpeedButton,'lc');
   CheckButton(HypImageSpeedButton,'hyp');
   CheckButton(VectorMapButton,'vmb');
   CheckButton(QuickVectorMapSpeedButton,'qvm');
   CheckButton(TigerSpeedButton,'tiger sb');
   CheckButton(SpeedButton9,'gaz sb9');
   CheckButton(InOutButton,'in out');
   CheckButton(CoordButton,'coord');
   CheckButton(LOSButton,'LOS');
   CheckButton(SpeedButton3,'sp3');
   CheckButton(MultProfSpeedButton,'mp');
   CheckButton(PerspectiveButton,'per');
   CheckButton(PanoramaSpeedButton,'pan');
   CheckButton(FlySpeedButton,'fly');
   CheckButton(LiveFlySpeedButton,'lf');
   CheckButton(MGTMagModelSpeedButton,'mgt');
   CheckButton(SpeedButton5,'sp5');
   CheckButton(StereoNetButton,'ster');
   CheckButton(TernarySpeedButton,'tern');
   CheckButton(PlateRotateSpeedButton,'rot');
   CheckButton(StratColSpeedButton,'strat');
   CheckButton(TINSpeedButton,'tin');

   Caption := EXENameWithBuild;
   case MDDef.ProgramOption of
      GeologyProgram : Caption :=  'Geology ' + EXENameWithBuild;
      GeographyProgram : Caption := 'Physical Geography ' + EXENameWithBuild;
      RemoteSensingProgram : Caption := 'Remote Sensing ' + EXENameWithBuild;
      //TCPprogram : Caption := 'TCP server ' + EXENameWithBuild;
      {$If Defined(ExDP)} {$Else}DragonPlotProgram : Caption := DPcaption; {$EndIf}
   end;
   ShowDefaultCursor;
   {$If Defined(RecordButtonProblems) or Defined(RecordFirstRun)} if FirstRun then WriteLineToDebugFile('TWmDem.SetMenusForVersion out'); {$EndIf}
end;


procedure ProcessCommandLine(CommandLine : AnsiString);
//CommandLine comes in capitalized
var
   Key,Value : AnsiString;
   DEM,NewDEM : integer;
   Action,xval,yval : shortstring;
   infile,outfile,upfile,downfile : PathStr;

         function OpenADEM(OpenMap : boolean = false) : boolean;
         begin
            Result := FileExists(Infile) and LoadNewDEM(DEM,infile,OpenMap);
         end;

begin
   {$IfDef RecordCommandLine} writeLineToDebugFile('Process ? command line ' + Commandline); {$EndIf}
   Action := '';
   infile := '';
   outfile := '';
   xval := '';
   yval := '';
   upfile := '';
   downfile := '';
   Delete(CommandLine,1,1);      //the question mark
   ReplaceCharacter(CommandLine,'+','&');
   CommandLine := CommandLine + '&';
   while length(CommandLine) > 1 do begin
      Value := Petmar_Types.BeforeSpecifiedCharacterANSI(CommandLine,'&',true,true);
      Key := Petmar_Types.BeforeSpecifiedCharacterANSI(Value,'=',true,true);
      {$IfDef RecordCommandLine} WriteLineToDebugFile(Key + '   ' + Value); {$EndIf}
      if Key = 'ACT' then Action := Value;
      if Key = 'IN' then infile := Value;
      if Key = 'OUT' then outfile := Value;
      if Key = 'UPNAME' then upfile := Value;
      if Key = 'DOWNNAME' then downfile := Value;
      if Key = 'PTSEP' then MDDef.PointSeparation := StrToInt(Value);
      if Key = 'ROAD_FILE' then MDDef.LCPRoadfName := Value;
      if Key = 'START_PTS' then MDDef.LCPstartFName := Value;
      if Key = 'END_PTS' then MDDef.LCPendFName := Value;
      if Key = 'INI' then ProcessIniFile(iniRead,'',Value);
      if Key = 'X' then xval := Value;
      if Key = 'Y' then yval := Value;
      if Key = 'RAD' then MDDef.OpenBoxSizeMeters := StrToInt(Value);
      if Key = 'PROJ' then begin
          if Value = 'UTM' then MDDef.LidarGridProjection := 0;
          if Value = 'GEO' then MDDef.LidarGridProjection := 1;
          if Value = 'WKT' then MDDef.LidarGridProjection := 2;
      end;
      if Key = 'DX' then begin
          if MDDef.LidarGridProjection = 0 then MDdef.DefLidarXGridSize := StrToFloat(Value);
          if MDDef.LidarGridProjection = 1 then MDdef.DefLidarGeoGridSizeX := StrToFloat(Value);
          if MDDef.LidarGridProjection = 2 then MDdef.DefWKTGridSize := StrToFloat(Value);
      end;
      if Key = 'DY' then begin
          if MDDef.LidarGridProjection = 0 then MDdef.DefLidarYGridSize := StrToFloat(Value);
          if MDDef.LidarGridProjection = 1 then MDdef.DefLidarGeoGridSizeY := StrToFloat(Value);
      end;
      if Key = 'HOLES' then begin
          MDdef.FillHoleRadius := StrToInt(Value);
          MDDef.PCAutoFillHoles :=  MDdef.FillHoleRadius > 0;
      end;
      if Key = 'PIXELIS' then begin
          if Value = 'AREA' then MDDef.LasDEMPixelIs := 1;
          if Value = 'POINT' then MDDef.LasDEMPixelIs := 2;
      end;

      {$IfDef AllowGeomorphometry}
         if Key = 'BOXSIZE' then MDDef.GeomorphBoxSizeMeters := StrToInt(Value);
      {$EndIf}
   end;

   if Action = 'DEM2JSON' then begin
      if OpenADEM then begin
         DEMGlb[DEM].SaveAsGeoJSON;
      end;
   end;

   if Action = 'RESAMPAVG' then begin
      {$IfDef RecordCommandLine} writeLineToDebugFile('do resample averaging'); {$EndIf}
      if OpenADEM(true) then begin
         {$IfDef RecordCommandLine} writeLineToDebugFile('dem opened'); {$EndIf}
         NewDEM := DEMGlb[DEM].ResampleByAveraging(false, false,Outfile);
         {$IfDef RecordCommandLine} writeLineToDebugFile('resampled created'); {$EndIf}
      end;
   end;

   if Action = 'SLOPEMAP' then begin
      {$IfDef RecordCommandLine} writeLineToDebugFile('do slope map'); {$EndIf}
      if OpenADEM then begin
         {$IfDef RecordCommandLine} writeLineToDebugFile('dem opened'); {$EndIf}
         NewDEM := CreateSlopeMap(DEM,false);
         {$IfDef RecordCommandLine} writeLineToDebugFile('slope map created'); {$EndIf}
         DEMGlb[NewDEM].SaveAsGeotiff(outfile);
         {$IfDef RecordCommandLine} writeLineToDebugFile('geotiff saved'); {$EndIf}
      end;
   end;

   if Action = 'OPENMAP' then begin
      {$IfDef RecordCommandLine} writeLineToDebugFile('do openness map'); {$EndIf}
      if OpenADEM then begin
         {$IfDef RecordCommandLine} writeLineToDebugFile('dem opened'); {$EndIf}
         MDDef.DoUpOpen := (UpFile <> '');
         MDDef.DoDownOpen := (DownFile <> '');
         MDDef.DoDiffOpen := false;
         MakeMomentsGrid(DEM,'O',MDDef.OpenBoxSizeMeters,false);
         {$IfDef RecordCommandLine} writeLineToDebugFile('openness map created'); {$EndIf}
         if (UpFile <> '') then DEMGlb[MomentDEMs[UpOpenDEM]].SaveAsGeotiff(UpFile);
         if (DownFile <> '') then DEMGlb[MomentDEMs[DownOpenDEM]].SaveAsGeotiff(DownFile);
         {$IfDef RecordCommandLine} writeLineToDebugFile('geotiff saved'); {$EndIf}
      end;
   end;


   if Action = 'SLOPEUNCERTAIN' then begin
      {$IfDef RecordCommandLine} writeLineToDebugFile('do slope uncertainty'); {$EndIf}
      if OpenADEM then begin
         if (xval <> '') then CarlosXRecord := StrToInt(xVal);
         if (yval <> '') then CarlosYRecord := StrToInt(yVal);
         DEMGlb[DEM].RichardsonExtrapolationSlopeMaps(true);
      end;
   end;

   {$IfDef ExPointCloud}
   {$Else}
      if Action = 'LAS2JSON' then begin
         QuietActions := true;
         LAS2GeoJSON(infile);
      end;
   {$EndIf}

   {$IfDef AllowGeomorphometry}
      if Action = 'TERR_FABRIC' then begin
         if OpenADEM then begin
            DEMGlb[DEM].OrientationTable(OutFile,Nil);
         end;
      end;
   {$EndIf}

   if Action = 'LCP' then begin
      {$IfDef RecordCommandLine} writeLineToDebugFile('Command line Call LeastCostPathOptions'); {$EndIf}
      LeastCostPathOptions(1);
   end;

   DEM_Manager.CloseAllWindowsAndData;
   {$IfDef RecordCommandLine} WriteLineToDebugFile('ending command line ops'); {$EndIf}
   halt;
   wmdem.Close;
end;


procedure Twmdem.Fontsinstalled1Click(Sender: TObject);
var
   Results : tStringList;
   i : integer;
begin
   Results := tStringList.Create;
   for i := 0 to pred(Screen.Fonts.Count) do Results.Add(Screen.Fonts.Strings[i]);
   DisplayAndPurgeStringList(Results,'Installed system fonts');
end;



procedure Twmdem.FormActivate(Sender: TObject);
var
   TStr  : ShortString;
   i     : integer;

      procedure SetProgramOptions(TStr : ShortString);
      begin
         if (TStr = '-EXPERT')  then begin
            MDdef.ProgramOption := ExpertProgram;
         end;
         if (TStr = '-GEOLOGY') or (TStr = '-BRUNTON') then begin
            MDdef.ProgramOption := GeologyProgram;
            SetStructuralGeologyDefaults;
         end;

         if (TStr = '-MINIMIZE') then begin
            WindowState := wsMinimized;
         end;
         if (TStr = '-NOAUTOOPEN') then begin
            MDdef.AutoOpen := aoNothing;
         end;
         if (TStr = '-RESET') then begin
            ProcessIniFile(iniInit);
         end;
         if (TStr = '-NODEBUG') then begin
            MDdef.MDRecordDebugLog := false;
         end;
         if (TStr = '-DEBUG') then begin
            MDdef.MDRecordDebugLog := true;
         end;
         if (TStr = '-RESET') then begin
            ProcessIniFile(iniInit);
         end;
      end;


      procedure CheckDragonPlotOptions;
      begin
         if IsThisDP then MDDef.ProgramOption := DragonPlotProgram;
         if (MDDef.ProgramOption = DragonPlotProgram) then begin
            if IsThisDP then begin
              {$IfDef ExDP}
                 MDDef.ProgramOption := ExpertProgram;
                 ShowDragonPlot := false;
              {$Else}
                 if (UpperCase(ptTrim(ParamStr(1))) = '-DP') then DragonPlotDef.AdvancedOptions := true;
                 ShowDragonPlot := true;
                 BackupProgramEXE('DP');
                 {$IfDef RecordDirs}  RecordDirs('before start DP'); {$EndIf}
                 StartDragonPlot;
                 BorderIcons := [biMinimize,biMaximize];
              {$EndIf}
           end
           else begin
              ShowDragonPlot := false;
              MDDef.ProgramOption := ExpertProgram;
              //if TrilobiteComputer then UnCustomizeMDDefaultsForDragonPlot;
           end;
         end
         else begin
           //if TrilobiteComputer then UnCustomizeMDDefaultsForDragonPlot;
         end;
      end;

      procedure TryAutoOpen;
      begin
        {$IfDef ExAutoOpen}
        {$Else}
           {$IfDef RecordProblems} WriteLineToDebugFile('MDdef.AutoOpen=' + IntToStr(ord(MDdef.AutoOpen))); {$EndIf}
           if (MDdef.AutoOpen <> aoNothing) then begin
              if (MDdef.AutoOpen = aoVector) then begin
                 wmDEM.VectorMapButtonClick(QuickVectorMapSpeedButton);
              end
              else AutoOpenOptions;
           end;
           FirstRun := false;
        {$EndIf}
      end;


begin
   {$If Defined(MessageStartup) or Defined(TrackFormCreate)} MessageToContinue('start Twmdem.FormActivate, width=' + IntToStr(Width) + '  & height=' + IntToStr(Height)); {$EndIf}
   {$If Defined(RecordFormResize) or Defined(RecordFormActivate)} WriteLineToDebugFile('Twmdem.FormActivate in, width=' + IntToStr(Width) + '  & height=' + IntToStr(Height)); {$EndIf}

   if FirstRun then begin
     InitializeMICRODEM;
     {$IfDef RecordFormActivate} writeLineToDebugFile('Twmdem.FormActivate, initialize MD over'); {$EndIf}

      if (not MDDef.RunOddballLocation) then begin
         if (Copy(UpperCase(ProgramRootDir),2,11) <> ':\MICRODEM\') then begin
            if not AnswerIsYes('Really run program here (really not recommended)') then halt;
            MDDef.RunOddballLocation := true;
            SaveMDDefaults;
         end;
         if StrUtils.AnsiContainsText(ProgramRootDir,' ') then begin
            if not AnswerIsYes('Really run program from directory with space in path name (really NOT recommended)') then halt;
            MDDef.RunOddballLocation := true;
            SaveMDDefaults;
         end;
      end;

      if (WMDEM <> Nil) then begin
         StatusBar1.Visible := true;
         if (not LockStatusBar) then for i := 0 to 3 do WmDEM.StatusBar1.Panels[i].Width := MDDef.SB1PanelWidths[i];
         {$IfDef RecordFormResize} WriteLineToDebugFile('Twmdem.FormActivate menus set, width=' + IntToStr(Width) + '  & height=' + IntToStr(Height)); {$EndIf}
         {$If Defined(MessageStartup) or Defined(TrackFormCreate)} MessageToContinue('Twmdem.FormActivate menus set, width=' + IntToStr(Width) + '  & height=' + IntToStr(Height)); {$EndIf}
      end;
     {$IfDef RecordDirs}  RecordDirs('after set panel widths'); {$EndIf}

      PetImage.FullPaletteBitmap;
      AddFreeDiskSpaceToDebugFile;

     CheckRequiredFiles;
     {$IfDef RecordDirs} RecordDirs('after CheckRequiredFiles'); {$EndIf}
     {$IfDef ExGDAL}
     {$Else}
        {$IfDef MSWindows}
            GetGDALFileNames;
        {$EndIf}
     {$EndIf}

    {$IfDef NoCommandLineParameters}
    {$Else}
       if (ParamCount <> 0) then begin
           TStr := UpperCase(ptTrim(ParamStr(1)));
           if (ParamCount = 1) and (TStr[1] = '?') then begin
              ProcessCommandLine(TStr);
           end
           else begin
               for i := 1 to ParamCount do begin
                  TStr := UpperCase(ptTrim(ParamStr(i)));
                  {$IfDef RecordProblems} WriteLineToDebugFile('Command line parameter ' + IntToStr(i) + '=' + TStr); {$EndIf}
                  SetProgramOptions(TStr);
               end;
           end;
       end;
    {$EndIf}

    {$If Defined(MessageStartup) or Defined(TrackFormCreate)} MessageToContinue('Twmdem.FormActivate after command line, width=' + IntToStr(Width) + '  & height=' + IntToStr(Height)); {$EndIf}
    {$IfDef RecordDirs}  RecordDirs('after command line'); {$EndIf}
    {$IfDef RecordProblems} writeLineToDebugFile('MDdef.AutoOpen=' + IntToStr(ord(MDdef.AutoOpen)) + '  MDdef.ProgramOption=' + IntToStr(ord(MDdef.ProgramOption)) ); {$EndIf}

      if MDdef.ProgramOption in [GeologyProgram] then begin
         {$IfDef ExGeology}
         {$Else}
            {$IfDef RecordProblems} writeLineToDebugFile('StructuralGeologyProgram, call GeologyGetData'); {$EndIf}
            GeologyGetData;
         {$EndIf}
      end
      else if MDdef.ProgramOption in [GeographyProgram] then begin
         {$IfDef ExGeography}
         {$Else}
            {$IfDef RecordProblems} WriteLineToDebugFile('GeographyProgram, call ClimateGetData'); {$EndIf}
            ClimateGetData;
            MDDef.ShowBlueMarble := true;
            MDDef.ImageryIconDirs := false;
            TryAutoOpen;
         {$EndIf}
      end
      else if NoAutoOpen or (MDdef.AutoOpen = aoNothing) then begin
      end
      else if MDdef.ProgramOption in [ExpertProgram] then begin
         TryAutoOpen;
      end
      else if (MDdef.ProgramOption = RemoteSensingProgram) then begin
         SetRemoteSensingDefaults;
         TryAutoOpen;
      end;

     {$IfDef RecordProblems} WriteLineToDebugFile('MDdef.AutoOpen completed, Twmdem.FormActivate wsMaximized, width=' + IntToStr(Width) + '  & height=' + IntToStr(Height)); {$EndIf}

     CheckDragonPlotOptions;

     if (UpperCase(ptTrim(ParamStr(1))) = '-DataManip') then Data1Click(Sender);
     {$IfDef RecordProblems} WriteLineToDebugFile('ending FormActivate, first time'); {$EndIf}
     {$If Defined(MessageStartup) or Defined(TrackFormCreate)} MessageToContinue('Twmdem.FormActivate ending first time'); {$EndIf}
   end;
   FirstRun := false;
   WmDEM.StatusBar1.Panels[0].Text := '';
   SetMenusForVersion;
   {$If Defined(RecordFormResize) or Defined(TrackFormCreate)} WriteLineToDebugFile('Twmdem.FormActivate set menu versions'); {$EndIf}
   SeeIfThereAreDebugThingsToDo;
   Self.Visible := true;
   {$If Defined(RecordFormResize) or Defined(TrackFormCreate)} WriteLineToDebugFile('Twmdem.FormActivate end, width=' + IntToStr(Width) + '  & height=' + IntToStr(Height)); {$EndIf}
end;


procedure Twmdem.SeeIfThereAreDebugThingsToDo;

      procedure CheckSnyder;
      var
         Lat,Long,xutm,yutm : float64;
      begin
         WGS84DatumConstants.DefineDatumFromUTMZone('NAD27',18,'N','check Snyder examples');
         WriteLineToDebugFile('');
         WGS84DatumConstants.ForwardProjectDegrees(40.5, -73.5, xutm,yutm);
         WriteLineToDebugFile('Snyder example RawProjectDegrees to UTM, x= ' + RealToString(xutm,-12,-1) + '  y=' + RealToString(yutm,-12,-1));
         WriteLineToDebugFile('  should be 627106.5,4484124.4');
         WriteLineToDebugFile('');
         WGS84DatumConstants.InverseProjectDegrees(627106.5,4484124.4,Lat,Long);
         WriteLineToDebugFile('Snyder example InverseProjectDegrees ' + LatLongDegreeToString(Lat,Long));
         WriteLineToDebugFile('  should be 40.5, -73.5');
         Viewdebuglog1Click(Nil);
      end;


begin
   //CheckSnyder;
end;

function GetLanguageCaption(aTable : tMyData; Text,Language : shortstring) : shortstring;
begin
   ATable.ApplyFilter('TEXT=' + QuotedStr(Text));
   if Atable.FiltRecsInDB > 0 then begin
      Result := Atable.GetFieldByNameAsString(Language);
   end;
end;

procedure Twmdem.FormCreate(Sender: TObject);
var
   TStr  : ShortString;
   i : integer;
   fName : PathStr;
   BatchMode : boolean;
begin
  {$If Defined(MessageStartup) or Defined(TrackFormCreate)} MessageToContinue('start wmdem FormCreate'); {$EndIf}
  ClientWidth := 4000;
  ClientHeight := 2400;
  fName := ChangeFileExt(application.exename,'.ico');
  if FileExists(fName) then begin
     Application.MainFormOnTaskbar := false;
     Application.Icon.LoadFromFile(fName);
  end;
  if StrUtils.AnsiContainsText(Application.ExeName,'1') then Self.Color := clGradientInactiveCaption;
  if StrUtils.AnsiContainsText(Application.ExeName,'2') then Self.Color := clGradientActiveCaption;
  if StrUtils.AnsiContainsText(Application.ExeName,'3') then Self.Color := clMoneyGreen;
  if StrUtils.AnsiContainsText(Application.ExeName,'4') then Self.Color := cl3DLight;
  if StrUtils.AnsiContainsText(Application.ExeName,'5') then Self.Color := clInfoBk;
  if StrUtils.AnsiContainsText(Application.ExeName,'6') then Self.Color := clYellow;

  ProgramClosing := false;
  AskForDebugUpdateNow := false;
  AskForNewUpdateNow := false;
  NoAutoOpen := false;
  BatchMode := false;
  Self.Visible := false;

   {$IfDef NoCommandLineParameters}
   {$Else}
      for i := 1 to ParamCount do begin
         TStr := UpperCase(ptTrim(ParamStr(i)));
         if (TStr = '-GRID') or (TStr = '-MINIMIZE') or (TStr = '-TCP') then BatchMode := true;
         if (TStr = '-AONOTHING') then NoAutoOpen := true;
      end;
   {$EndIf}

   if (not BatchMode) then begin
      TerSplsh.MDStartSplashing;
      {$If Defined(TrackFormCreate)} MessageToContinue('FormCreate start splashing'); {$EndIf}
      if (GetScreenColorDepth < 24) then MessageToContinue('Problems likely w/ < 24 bit color');
   end;

   {$If Defined(TrackFormCreate)} MessageToContinue('FormCreate out'); {$EndIf}
   {$IfDef RecordFormResize} WriteLineToDebugFile('Twmdem.FormCreate out, width=' + IntToStr(Width) + '  & height=' + IntToStr(Height)); {$EndIf}
end;


procedure Twmdem.ACOLITEallopensatelliteimages1Click(Sender: TObject);
var
   i : integer;
begin
   for i := 1 to NumSatImageOpen do  begin
       if ValidSatImage(i) then begin
          ACOLITEprocessing(SatImage[i].SelectionMap,false);
       end;
   end;
end;

procedure Twmdem.Addnormaliziedstatsforblockgridstotrainingset1Click(Sender: TObject);
begin
   Statesforblockgrids1Click(Sender);
end;


procedure Twmdem.Addproject1Click(Sender: TObject);
begin
   RestoreMicrodemDesktop('',false);
end;


procedure Twmdem.Afar1Click(Sender: TObject);
{$IfDef ExGeology}
begin
{$Else}
var
   fName,dName, pName : PathStr;
   db : integer;

         procedure SetColors(fName : PathStr);
         var
            db : integer;
         begin
            {$IfDef RecordLabs} writeLineToDebugFile('Twmdem.Afar1Click SetColors: ' + fName); {$EndIf}
            DEMGlb[LastDEMLoaded].SelectionMap.MapDraw.AllowDataBaseDrawing := false;
            db := DEMGlb[LastDEMLoaded].SelectionMap.LoadDataBaseFile(fName);
            GISdb[db].dbOpts.DBAutoShow := dbasZValues;
            GISdb[db].dbOpts.LineWidth := 2;
            GISdb[db].dbOpts.ZColorMin := -500;
            GISdb[db].dbOpts.ZColorMax := 500;
            DEMGlb[LastDEMLoaded].SelectionMap.MapDraw.AllowDataBaseDrawing := true;
         end;

begin
   {$IfDef RecordLabs} WriteLineToDebugFile('Twmdem.Afar1Click (Geology labs) in'); {$EndIf}
   if (MDDef.ProgramOption = GeologyProgram) then begin
      SetStructuralGeologyDefaults;
   end;

   GeologyGetData;
   SetMenusForVersion;
   MDDef.MoveGeologyDBMemory := false;

   {$IfDef RecordLabs} writeLineToDebugFile('Twmdem.Afar1Click data ready'); {$EndIf}

   if (Sender = SheepRange1) then begin
      dName := 'sheep_range_nv_geology';
      DownloadandUnzipDataFileIfNotPresent(dName);
       pName := MainMapData + 'md-proj\' + dName + '.dbf';
       if not FileExists(pName) then begin
          DownloadFileFromWeb(WebDataDownLoadDir + dName + '.dbf',pName);
       end;
       RestoreMicrodemDesktop(pName);
   end;

   if (Sender = Atlantis1) then begin
      {$IfDef RecordLabs} writeLineToDebugFile('Atlantis'); {$EndIf}
      MDDef.NetDef.NetScreenMult := 1;
      MDDef.NetDef.BeachBallSize := bbsAll;

      dName := 'mar_core_complexes_v2';
      DownloadandUnzipDataFileIfNotPresent(dName);
      dName := MainMapData + dName + '\atlantis\';
      LastDEMLoaded := OpenNewDEM(dName + 'GMRTv2_6_atlantis.dem');
      db := DEMGlb[LastDEMLoaded].SelectionMap.LoadDataBaseFile(dName + 'atlantis_focal_mechs.dbf');
      GISdb[db].dbtablef.Reclassifyfocalmechanisms1Click(Nil);

      if GISdb[db].dbOpts.DBAutoShow <> dbasBeachBall then begin
         GISdb[db].dbOpts.DBAutoShow := dbasBeachBall;
         GISdb[db].RedrawLayerOnMap;
      end;

      DEMGlb[LastDEMLoaded].SelectionMap.LoadDataBaseFile(dName + 'atlantis_mag_picks.dbf');
      GISdb[db].PlotFieldOnMap('GEEK2007',0,20);
      {$IfDef RecordLabs} writeLineToDebugFile('done'); {$EndIf}
   end;

   if (Sender = GulfofMexicoGLORIA1) then begin
      {$IfDef RecordLabs} WriteLineToDebugFile('GulfofMexicoGLORIA1'); {$EndIf}
      dName := 'gulf_mexico_v4';
      MDDef.DBsOnAllMaps := false;
      DownloadandUnzipDataFileIfNotPresent(dName);
      LastDEMLoaded := OpenNewDEM(MainMapData + dName + '\gloria_geog\gloria_geog.tif');
      DEMGlb[LastDEMLoaded].SelectionMap.MapDraw.MapType := mtElevGray;
      DEMGlb[LastDEMLoaded].SelectionMap.DoBaseMapRedraw;

      DEMGlb[LastDEMLoaded].SelectionMap.LoadDataBaseFile(MainMapData + dName + '\oil\deepwater_horizon.dbf');
      DEMGlb[LastDEMLoaded].SelectionMap.LoadDataBaseFile(MainMapData + dName + '\oil\Platform.dbf');
      DEMGlb[LastDEMLoaded].SelectionMap.LoadDataBaseFile(MainMapData + dName + '\oil\ppl_arcs.dbf');
      DEMGlb[LastDEMLoaded].SelectionMap.LoadDataBaseFile(MainMapData + dName + '\geology_maps\gloria_geology_nad83.shp');
      GISdb[db].dbTablef.Legend1Click(nil);

      LastDEMLoaded := OpenNewDEM(MainMapData + dName + '\noaa_coastal\merge_gom_crm_v1.dem');
      {$IfDef RecordLabs} WriteLineToDebugFile('all data loaded');    {$EndIf}
      MDDef.DBsOnAllMaps := true;
      DEMGlb[LastDEMLoaded].SelectionMap.DoFastMapRedraw;
      DEMGlb[LastDEMLoaded].SelectionMap.SpeedButton8Click(nil);
      DisplayHTMLTopic('geology_course\labs\gloria_gom.htm');
      {$IfDef RecordLabs} WriteLineToDebugFile('finished');    {$EndIf}
   end;

   if (Sender = Californiaoffshore1) then begin
      dName := 'ca_offshore_v4';
      DownloadandUnzipDataFileIfNotPresent(dName);
      LastDEMLoaded := OpenNewDEM(MainMapData + dName + '\so_cal_crm_v2_3sec.dem');
      DEMGlb[LastDEMLoaded].SelectionMap.LoadDataBaseFile(MainMapData + dName + '\qfaults\sectionsALL.shp');
      DEMGlb[LastDEMLoaded].SelectionMap.LoadDataBaseFile(MainMapData + dName + '\AllWells\AllWells.dbf');
   end;

   if (Sender = MH370region1) then begin
      {$IfDef RecordLabs} WriteLineToDebugFile('Lab = MH370region'); {$EndIf}
      dName := MainMapData + 'etopo1\';
      SafeMakeDir(dName);
      fName := 'mh370_srtm30plus.dem';
      if not FileExists(dName + fName) then begin
         DownloadFileFromWeb(WebDataDownLoadDir + fName,dName + fName);
         {$IfDef RecordLabs} WriteLineToDebugFile('MH370region1 downloaded ' + fName); {$EndIf}
      end;
      OpenNewDEM(dName + fName,true,'MH370 region bathy');
      if (LastDEMLoaded = 0) then exit;
      {$IfDef RecordLabs} WriteLineToDebugFile('Bathy loaded'); {$EndIf}
      DEMGlb[LastDEMLoaded].SelectionMap.Continentalcrust1Click(nil);
      {$IfDef RecordLabs} WriteLineToDebugFile('Crust loaded'); {$EndIf}
      DEMGlb[LastDEMLoaded].SelectionMap.Coastlines1Click(Nil);
      {$IfDef RecordLabs} WriteLineToDebugFile('Coasts loaded'); {$EndIf}
      DEMGlb[LastDEMLoaded].SelectionMap.Plateboundaries1Click(nil);
      {$IfDef RecordLabs} WriteLineToDebugFile('Plate boundaries loaded'); {$EndIf}
      SeaFloorAgeSpeedButtonClick(nil);
      {$IfDef RecordLabs} WriteLineToDebugFile('Sea floor ages loaded'); {$EndIf}
   end;

    if (Sender = Afar1) then begin
       dName := 'afar';
       DownloadandUnzipDataFileIfNotPresent(dName);
       pName := MainMapData + 'afar\small_afar_3_sec_srtm.DEM';
       LastDEMLoaded := OpenNewDEM(pName);
       DEMGlb[LastDEMLoaded].SelectionMap.LoadDataBaseFile(MainMapData + 'afar\afar_cmt.dbf');
    end;

   if (Sender = TulaFracturezonemagnetics1) or (Sender = Triplejunctions1) or (Sender = Trenchgeometry1) or (Sender = MegaThrusts1) or (Sender = Italyfocalmechs1) or (Sender = Geoidandsedimentdistribution1) then begin
      {$IfDef RecordLabs} WriteLineToDebugFile('Load ETOPO'); {$EndIf}
      GetETOPO1;
      LastDEMLoaded := OpenNewDEM(ETOPODEMName);

      if (Sender = Geoidandsedimentdistribution1) then begin
         {$IfDef RecordLabs} WriteLineToDebugFile('Lab Geoidandsedimentdistribution1'); {$EndIf}
         dName := MainMapData + 'etopo1\';
         fName := 'topex_mgb_364.dbf';
         if not FileExists(dName + fName) then DownloadFileFromWeb(WebDataDownLoadDir + fName,dName + fName);
         MDDef.DBsOnAllMaps := false;
         db := DEMGlb[LastDEMLoaded].SelectionMap.LoadDataBaseFile(dName + fName,true,false);
         {$IfDef RecordLabs} WriteLineToDebugFile('altimeter db loaded'); {$EndIf}
         GISdb[db].dbOpts.DBAutoShow := dbasColorByNumeric;
         GISdb[db].dbOpts.FloatColorField := 'GEOID_ANOM';
         DEMGlb[LastDEMLoaded].SelectionMap.DoFastMapRedraw;
         {$IfDef RecordLabs} WriteLineToDebugFile('map redrawn'); {$EndIf}
      end;

      if (Sender = MegaThrusts1) then begin
         if (Not PathIsValid(MainMapData + 'megathrust\')) then begin
            dName := MainMapData + 'megathrust.zip';
            DownloadFileFromWeb(WebDataDownLoadDir + 'megathrust',dName);
            ZipMasterUnzip(dName,MainMapData);
         end;
         dName := MainMapData + 'megathrust\';
         DEMGlb[LastDEMLoaded].SelectionMap.LoadDataBaseFile(dName + 'chile_2010_quakes.dbf');
         DEMGlb[LastDEMLoaded].SelectionMap.LoadDataBaseFile(dName + 'indonesia_2004_quakes.dbf');
         DEMGlb[LastDEMLoaded].SelectionMap.LoadDataBaseFile(dName + 'japan_2011_quakes.dbf');
      end;

      if (Sender = TulaFracturezonemagnetics1) then begin
         dName := 'tula_fracture_zone_v2';
         {$IfDef RecordLabs} WriteLineToDebugFile('Lab: ' + dName); {$EndIf}
         if (Not PathIsValid(MainMapData + dName)) then begin
            pName := MainMapData + dName + '.zip';
            {$IfDef RecordLabs} WriteLineToDebugFile('download: ' + pName); {$EndIf}
            DownloadFileFromWeb(WebDataDownLoadDir + dName + '.zip',pName);
            ZipMasterUnzip(pName,MainMapData);
         end;
         dName := MainMapData + dName + '\';

         {$IfDef RecordLabs} WriteLineToDebugFile('Load shapefiles'); {$EndIf}
         SetColors(dName + 'tula_fracture_zone.shp');
         SetColors(dName + 'WEST03MV_line.shp');
         SetColors(dName + 'MRTN06WT_line.shp');
         SetColors(dName + 'ELT19_line.shp');
         DEMGlb[LastDEMLoaded].SelectionMap.DoCompleteMapRedraw;
         {$IfDef RecordLabs} writeLineToDebugFile('Completed'); {$EndIf}
      end;

       if (Sender = Italyfocalmechs1) then begin
           fName := 'italy_focal_mechs'+ DefaultDBExt;
           pName := DBDir + fName;
           if not FileExists(pName) then begin
              DownloadFileFromWeb(WebDataDownLoadDir + fName,pName);
           end;
           DEMGlb[LastDEMLoaded].SelectionMap.LoadDataBaseFile(pName);
       end;
   end;
   {$IfDef RecordLabs} WriteLineToDebugFile('Twmdem.Afar1Click (Geology labs) out'); {$EndIf}
{$EndIf}
end;


procedure Twmdem.Ages1Click(Sender: TObject);
begin
   {$IfDef ExGeology}
   {$Else}
      OpenNewDEM(PredAgesFile);
   {$EndIf}
end;

procedure Twmdem.All1Click(Sender: TObject);
begin
   {$IfDef ExGeology}
   {$Else}
      if AnswerIsYes('Several GB; proceed') then begin
         ClimateGetData(true);
         GeologyGetData(true);
         GetNaturalEarthData(True);
         GetETOPO1;
         DownloadandUnzipDataFileIfNotPresent('annapolis_data');
         DownloadandUnzipDataFileIfNotPresent('annap_tm8');
         DownloadandUnzipDataFileIfNotPresent('duck_beach');
      end;
   {$EndIf}
end;

procedure Twmdem.Allgraphsononeimage1Click(Sender: TObject);
var
   BottomMargin,
   i{,DEM} : integer;
   Findings : tStringlist;
   fName : PathStr;
   Bitmap : tMyBitmap;
begin
   Findings := tStringList.Create;
   BottomMargin := 45;
   for i := pred(WMDEM.MDIChildCount) downto 0 do begin
      if WMDEM.MDIChildren[i] is TThisBaseGraph then begin
         CopyImageToBitmap((WMDEM.MDIChildren[i] as TThisBaseGraph).Image1,Bitmap);
         Bitmap.Height := Bitmap.Height + BottomMargin;
         (*
         Bitmap.Canvas.Brush.Style := bsClear;
         Bitmap.Canvas.Pen.Width := 2;
         Bitmap.Canvas.Pen.Color := clBlack;
         Bitmap.Canvas.Rectangle(0,0,pred(Bitmap.Width),pred(Bitmap.Height));
         Bitmap.Canvas.Pen.Width := 2;
         Bitmap.Canvas.Pen.Color := clWhite;
         Bitmap.Canvas.Brush.Style := bsSolid;
         Bitmap.Canvas.Brush.Color := clWhite;
         Bitmap.Canvas.Rectangle(0,Bitmap.Height-BottomMargin,Bitmap.Width,Bitmap.Height);
         Bitmap.Canvas.Brush.Style := bsClear;
         Bitmap.Canvas.Font.Size := 24;

         DEM := (WMDEM.MDIChildren[i] as TMapForm).MapDraw.DEMonMap;
         if (DEM <> 0) then Bitmap.Canvas.TextOut(5, Bitmap.Height - (BottomMargin -5), RemoveUnderScores(DEMGLB[DEM].AreaName) + '  ' + DEMGLB[DEM].HorizontalDEMSpacing(true));
         *)
         fName := NextFileNumber(MDtempDir,'graph_4_biggie_','.bmp');
         Bitmap.SaveToFile(fName);
         Findings.Add(fName);
      end;
   end;
   if (Findings.Count > 0) then MakeBigBitmap(Findings,'')
   else Findings.Free;
end;

procedure Twmdem.Allindividuallayers1Click(Sender: TObject);
begin
   {$IfDef ExGDAL}
   {$Else}
      GDALconvertGeoPDF(gdalAllindividuallayers1);
   {$EndIf}
end;

procedure Twmdem.TulaFracturezonemagnetics1Click(Sender: TObject);
begin
   Afar1Click(Sender);
end;


procedure Twmdem.Annapolislidar1Click(Sender: TObject);
var
   pName : PathStr;
   Force : boolean;
begin
   pName := 'annapolis_data';
   Force := not PathIsValid(MainMapData + pname + '\dems\');
   DownloadandUnzipDataFileIfNotPresent(pName,Force);
   LastLidarDirectory := MainMapData + pname + '\las_2017_Anne_Arundel';
   LastLidar2Directory := MainMapData + pname + '\las_2011_Anne_Arundel';
   LastLidar3Directory := LastLidarDirectory;
   LastLidar4Directory := LastLidarDirectory;
   LastLidar5Directory := LastLidarDirectory;
   LastDEMName := MainMapData + pname + '\dems\NED_third_sec.tif';
   OpenNewDEM(LastDEMName);
   MDdef.AutoZoomOpenLAS := true;
   if LastDEMLoaded <> 0 then DEMGlb[LastDEMLoaded].SelectionMap.PointCloudSpeedButtonClick(Sender);
end;

procedure Twmdem.Annapolisredistricting1Click(Sender: TObject);
{$IfDef ExRedistrict}
begin
{$Else}
var
   pName : PathStr;
begin
   pName := 'annapolis_redistrict_2011';
   DownloadandUnzipDataFileIfNotPresent(pName);
   pName := MainMapData + pName + '\';
   LegislativeRedistrict(pName + 'blocks_with_population_2010\city_annapolis_blocks_2010.shp', pName + 'tl_2010_24003_edges\tl_2010_24003_edges.shp', pName + 'city_outline\city_annapolis.shx');
{$EndIf}
end;

procedure Twmdem.AnnapolisTM8scene1Click(Sender: TObject);
var
   pName : PathStr;
begin
   {$IfDef RecordLabs} WriteLineToDebugFile('Twmdem.AnnapolisTM8scene1Click in'); {$EndIf}
   pName := 'annap_tm8';
   DownloadandUnzipDataFileIfNotPresent(pName);
   {$IfDef RecordLabs} writeLineToDebugFile('Twmdem.AnnapolisTM8scene1Click data set'); {$EndIf}
   pName := MainMapData + pname + '\';
   LastImageName := pName + 'LC80150332015229LGN00\LC80150332015229LGN00_B1.TIF';
   OpenAndDisplayNewScene(Nil,LastImageName,true,true,true);
   DisplayHTMLTopic('microdemtutorials/sat_imagery/satellite_imagery_tutorial.htm');
   {$IfDef RecordLabs} WriteLineToDebugFile('Twmdem.AnnapolisTM8scene1Click out'); {$EndIf}
end;


procedure Twmdem.Arcsecondrectangularpixels1Click(Sender: TObject);
begin
   PixelRectangles;
end;



procedure Twmdem.ArcsecondspacingDTEDDGED1Click(Sender: TObject);
begin
   MakeDTEDTable;
end;

procedure Twmdem.N28Click(Sender: TObject);
begin
   GDALAssignProjection;
end;

procedure Twmdem.N81Sfileviewer1Click(Sender: TObject);
begin
   {$IfDef ExSidescan}
   {$Else}
      StopSplashing;
      mst_format.View81Ssidescanfile(nil);
   {$EndIf}
end;



procedure Twmdem.Newsatelliteimage1Click(Sender: TObject);
begin
   {$IfDef ExSat}
   {$Else}
      PickAndOpenImagery(itSat);
   {$EndIf}
end;


procedure Twmdem.Nightlights1Click(Sender: TObject);
var
   fName,pDir : PathStr;
begin
   pDir := MainMapData + 'night_lights\';
   if not PathIsValid(pDir) then begin
      fName := 'night_lights.zip';
      DownloadFileFromWeb(WebDataDownLoadDir + fName,MainMapData + fName);
      ZipMasterUnzip(MainMapData + fName,MainMapData);
   end;
   OpenAndDisplayNewScene(Nil,pdir + 'BlackMarble_2016_3km_gray_geo.tif',true,true,true);
end;


procedure Twmdem.FormClose(Sender: TObject; var Action: TCloseAction);
var
   fName : PathStr;
   BatFile : tStringList;
begin
   {$IfDef RecordClosing} WriteLineToDebugFile('Twmdem.FormClose in ' + BuildString  + ' dbfn=' + DebugFileName); {$EndIf}
   ShowHourglassCursor;
   SaveMDDefaults;
   {$IfDef RecordClosing} WriteLineToDebugFile('Defaults saved ' + '  dbfn=' + DebugFileName); {$EndIf}
   DEM_Manager.CloseAllWindowsAndData;
   {$IfDef RecordClosing} WriteLineToDebugFile('Windows closed ' + '  dbfn=' + DebugFileName); {$EndIf}
   if AskForDebugUpdateNow or AskForNewUpdateNow then begin
      if MDDef.BackupEXEbeforeUpdate then BackupprogramEXE1Click(Sender);
      DeleteFileIfExists(ProgramRootDir + 'backup\microdem.exe');
      if AskForNewUpdateNow then DownloadFileFromWeb(WebProgramDownLoadDir + 'microdem_update.exe',MDTempDir + 'microdem_update.exe')
      else DownloadFileFromWeb(WebProgramDownLoadDir + 'microdem_beta_update.exe',MDTempDir + 'microdem_beta_update.exe');
      fName := MDTempDir + 'restart.bat';
      BatFile := tStringList.Create;
      BatFile.Add('REM wait program close');
      BatFile.Add('ping 127.0.0.1 -n ' + IntToStr(MDDef.UpdateDelay) + ' > nul');
      BatFile.Add('del microdem.exe /F');
      if AskForNewUpdateNow then begin
         BatFile.Add(MDTempDir + 'microdem_update.exe');
         BatFile.Add('c:\microdem\microdem.exe');
      end
      else begin
         BatFile.Add(MDTempDir + 'microdem_beta_update.exe');
         BatFile.Add('c:\microdem\microdem_beta.exe');
         //BatFile.Add('move c:\microdem\backup\microdem.exe c:\microdem\microdem.exe');
      end;
      BatFile.SaveToFile(fName);
      BatFile.Free;
      ExecuteFile(fName, '', ProgramRootDir);
   end;
   Action := caFree;
   {$If Defined(RecordClosing) or Defined(RecordProblems)} writeLineToDebugFile('Twmdem.FormClose out, normal termination build ' + BuildString  + '  dbfn=' + DebugFileName); {$EndIf}
end;

procedure Twmdem.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
   {$IfDef RecordClosing} writeLineToDebugFile('Enter wmDEM formCloseQuery'); {$EndIf}
   ApplicationProcessMessages;
   CanClose := true;
end;


procedure Twmdem.Hardware1Click(Sender: TObject);
begin
   HardwareOnLine;
end;

procedure Twmdem.HarpersFerryTerrainAnalysis1Click(Sender: TObject);
{$IfDef ExPointCloud}
begin
{$Else}
var
   pName : PathStr;
begin
   pName := 'terrain_analysis_harpers_ferry';
   DownloadandUnzipDataFileIfNotPresent(pName);
   pName := MainMapData + pname + '\';
   LastDEMName := pName + 'harpers_ferry_ned_ninth.tif';
   LastLidarDirectory := pName + 'fema_las_lidar\';
   LastWorldFileOverlay := pName + 'USGS_OF-2000-297_1.kmz';
   OpenNewDEM(LastDEMName);
{$EndIf}
end;


procedure Twmdem.EditDEMHeader1Click(Sender: TObject);
begin
   EditDEMHeader;
end;


procedure Twmdem.Elevationhistograms1Click(Sender: TObject);
begin
   {$IfDef ExGeoStats}
   {$Else}
      DoElevationHistograms;
   {$EndIf}
end;


procedure Twmdem.Equinox1Click(Sender: TObject);
begin
   SunViews(3);
end;

procedure Twmdem.ConvertCoordinates1Click(Sender: TObject);
begin
   {$IfDef ExConvert}
   {$Else}
      CoordButtonClick(Sender);
   {$EndIf}
end;


procedure Twmdem.ConvertDBFsfor64bit1Click(Sender: TObject);

     procedure ConvertDB(NewName : PathStr);
     var
        OldName : PathStr;
     begin
        {$IfDef RecordDBFconvert} WriteLineToDebugFile('ConvertDB ' + NewName);   {$EndIf}
        OldName := ChangeFileExt(NewName,'.dbf');
        {$IfDef SQLiteDefaultDBs}
           if MDDef.ForceOverwriteDB then SysUtils.DeleteFile(NewName);
        {$EndIf}

        if FileExists(NewName) then begin
           {$IfDef RecordDBFconvert} WriteLineToDebugFile('   File already exists ' + NewName); {$EndIf}
        end
        else begin
           if FileExists(OldName) then begin
              ConvertDBFtoSQLite(OldName);
           end
           else begin
              {$IfDef RecordDBFconvert} WriteLineToDebugFile('   old file missing ' + OldName); {$EndIf}
           end;
        end;
     end;


begin
   {$IfDef SQLiteDefaultDBs} if MDDef.ForceOverwriteDB then MessageToContinue('Convert databases to SQLite'); {$EndIf}
   ConvertDB(TigerShapeRules);
   ConvertDB(LasRulesName);
   ConvertDB(CSVImportRulesFName);
   ConvertDB(SatBandNames);
   ConvertDB(ColorBrewerName);
   ConvertDB(TableDefinitionsFileName);
   ConvertDB(GazOptFName);
   ConvertDB(LandCoverFName);
   ConvertDB(GT_Datum_fName);
   ConvertDB(GT_Ellipse_fName);
   ConvertDB(RangeCircleSizesfName);
   ConvertDB(DataTypeFileName);
   ConvertDB(SeriesIndexFileName);
   ConvertDB(MapLibraryFName);

   {$IfDef ExWMS}
   {$Else}
      ConvertDB(WMS_servers_fName);
   {$EndIf}

   {$IfDef ExOSM}
   {$Else}
      ConvertDB(OSMRoadRules);
   {$EndIf}

   {$IfDef ExGeology}
   {$Else}
      ConvertDB(MagneticAnomalyTimeScale);
      ConvertDB(ContCrustOutlineFile);
      ConvertDB(CurrentMotionsFile);
      ConvertDB(PlatePolesFile);
      ConvertDB(DSDP_db);
   {$EndIf}

   {$IfDef SQLiteDefaultDBs}
      MDDef.ForceOverwriteDB := false;
   {$EndIf}
end;

procedure Twmdem.Graysgame1Click(Sender: TObject);
var
   GrayGameForm : TGrayGameForm;
begin
   StopSplashing;
   GrayGameForm := TGrayGameForm.Create(Application);
   GrayGameForm.ShowModal;
end;


procedure Twmdem.Gridswithmonthlyprecipitationandtemperature1Click(Sender: TObject);
begin
   {$IfDef ExMultiGrid}
   {$Else}
      OpenTempPrecipEvap;
   {$EndIf}
end;

procedure Twmdem.Guam1Click(Sender: TObject);
begin
   FinnishGaussKruger1Click(Sender);
end;

procedure Twmdem.GulfofMexicoGLORIA1Click(Sender: TObject);
begin
   Afar1Click(Sender);
end;

procedure Twmdem.SheepRange1Click(Sender: TObject);
begin
   Afar1Click(Sender);
end;


procedure Twmdem.StereoNet1Click(Sender: TObject);
begin
{$IfDef ExGeostats}
{$Else}
   NetForm := TNetForm.Create(Application);
   NetForm.UpDateDisplay;
   StopSplashing;
{$EndIf}
end;



procedure Twmdem.Programlimits1Click(Sender: TObject);
begin
   MessageToContinue(ShortEXEName + ' ' + BuildString + ' Limits' + MessLineBreak +
       'Max open DEMs: ' + IntToStr(MaxDEMDataSets) + MessLineBreak +
       '--Max cols: ' + IntToStr(DEMDefs.MaxColsInRAM) + MessLineBreak +
       '--Max rows: ' + IntToStr(MaxElevArraySize) + MessLineBreak + MessLineBreak +
      {$IfDef ExSat}
      {$Else}
          'Max open sat images:' + IntToStr(MaxSatAllowed) + MessLineBreak +
          '--Max cols: ' + IntToStr(MaxSatCols) + MessLineBreak +
          '--Max rows: unlimited'  + MessLineBreak + MessLineBreak +
       {$EndIf}
       'Max open vector maps: ' + IntToStr(DEMMapf.MaxVectorMap)+ MessLineBreak + MessLineBreak +
       'Max open GIS database: ' + IntToStr(MaxDataBase) +  MessLineBreak +
       'Max bitmap: ' + IntToStr(MaxScreenXMax),true ) ;
end;


procedure Twmdem.Quickplatetectonicsmaps1Click(Sender: TObject);
begin
   Platetectonics1Click(Sender);
end;

procedure Twmdem.Magneticanomaliesgrid1Click(Sender: TObject);
begin
   MagMapButtonClick(Sender);
end;

procedure Twmdem.Magneticmodel1Click(Sender: TObject);
begin
   {$IfDef ExGeology}
   {$Else}
      {$IfDef RecordMGT} writeLineToDebugFile('Twmdem.Magneticmodel1Click'); {$EndIf}
      DEMGlb[3].SelectionMap.BringToFront;
      ChangeDEMNowDoing(SeekingLeftSideMagModels);
   {$EndIf}
end;


procedure Twmdem.Makelittletilescontest1Click(Sender: TObject);
begin
   {$IfDef Include2021datafusion}  MakeLittleTiles;  {$EndIf}
end;

procedure Twmdem.Californiaoffshore1Click(Sender: TObject);
begin
   Afar1Click(Sender);
   DisplayHTMLTopic('geology_course\labs\seismic_reflection.htm');
end;


procedure Twmdem.Cancelpending1Click(Sender: TObject);
begin
   ChangeDEMNowDoing(JustWandering);
end;


procedure Twmdem.TextEditor1Click(Sender: TObject);
var
   FName : PathStr;
begin
   fName := ProgramRootDir;
   if GetFileFromDirectory('to edit','*.TXT',FName) then begin
      StopSplashing;
      ModalEditWindow(FName,'Editing ' + FName);
   end;
end;


procedure Twmdem.Header1Click(Sender: TObject);
{$IfDef ExDataManip}
begin
{$Else}
var
   CurDEM,CurImage : integer;
begin
   CurDEM := 1;
   CurImage := 0;
   GetDEMorImage(true,true,CurDEM,CurImage,true,'Header to display');
   if (CurDEM > 0) then ViewHeaderRecord(CurDEM);
   {$IfDef ExSat}
   {$Else}
      if (CurImage > 0) then DEM_sat_Header.ViewSatHeader(CurImage);
   {$EndIf}
{$EndIf}
end;


procedure Twmdem.Unicodeicongenerator1Click(Sender: TObject);
begin
   FormUnicode := TFormUnicode.Create(Application);
end;

procedure Twmdem.UpdateEurekaValleyDEM1Click(Sender: TObject);
var
   dName,fName : PathStr;
begin
   DownloadandUnzipDataFileIfNotPresent('New_ned_last_chance_range');
   fName := MainMapData + 'New_ned_last_chance_range.tif';
   if FileExists(fName) then begin
      dName := MainMapData + 'eureka_valley_landslide\dem\New_ned_last_chance_range.tif';
      Petmar.MoveFile(fName,dName);
      OpenNewDEM(dName);
   end;
end;

procedure Twmdem.Updatehelpfile1Click(Sender: TObject);
var
   HelpFileName : PathStr;
begin
   {$IfDef ExWebDownload}
   {$Else}
      HelpFileName := ProgramRootDir + 'microdem.chm';
      SysUtils.DeleteFile(HelpFileName);
      DownloadFileFromWeb(WebProgramDownLoadDir + 'microdem.chm',HelpFileName);
      UnblockFile(HelpFileName);
   {$EndIf}
end;


procedure Twmdem.Updatehelpfile2Click(Sender: TObject);
begin
   Updatehelpfile1Click(Sender);
   Contents1Click(Sender);
end;



procedure Twmdem.Monthlyclimateparameters1Click(Sender: TObject);
begin
   {$IfDef ExMultiGrid}
   {$Else}
      OpenMonthlyMultiGrids;
   {$EndIf}
end;

procedure Twmdem.Monthlyclimatologies1Click(Sender: TObject);
begin
   {$IfDef ExGeography}
   {$Else}
      ClimateGetData;
      PopUpMenu8.Popup(Mouse.CursorPos.X,Mouse.CursorPos.Y);
   {$EndIf}
end;

procedure Twmdem.Monthlyinsolation1Click(Sender: TObject);
begin
   {$IfDef ExMultiGrid}
   {$Else}
       OpenSolarRad;
       DisplayHTMLTopic('microdemtutorials/Phys_geography/insolation.htm');
   {$EndIf}
end;

procedure Twmdem.Monthlywinds1Click(Sender: TObject);
begin
   VectorMapButtonClick(Nil);
   VectorMap[LastVectorMap].Globalmonthlywinds1Click(Sender);
   DisplayHTMLTopic('microdemtutorials/Phys_geography/wind_climate.htm');
   StopSplashing;
end;


procedure Twmdem.MrSidimagery1Click(Sender: TObject);
begin
{$IfDef ExSat}
{$Else}
   MrSidImagery.MrSidInfo(LastImageName);
{$EndIf}
end;


procedure Twmdem.MSTsidescanimport1Click(Sender: TObject);
begin
{$IfDef ExSidescan}
{$Else}
   ImportMSTSidescan;
{$EndIf}
end;



procedure Twmdem.Showfirstbytesofbinaryfile1Click(Sender: TObject);
var
   bf : file;
   sl : tStringList;
   fName : PathStr;
   values : array[1..1024] of byte;
   i,Red : integer;
begin
   if GetFileFromDirectory('Binary file','*.*',fName) then begin
      assignFile(bf,fName);
      reset(bf,1);
      BlockRead(bf,values,1024,Red);
      sl := tStringList.Create;
      for i := 1 to red do sl.Add(IntToStr(values[i]));
      DisplayAndPurgeStringList(sl,fName);
      sl.Free;
      CloseFile(bf);
   end;
end;

procedure Twmdem.Showfirstlinestextfile1Click(Sender: TObject);
const
   fName : PathStr = '';
var
   j : Integer;
   FilesWanted : tStringList;
   DefaultFilter : byte;
begin
   if (fName = '') then fName := MainMapData;
   DefaultFilter := 1;
   FilesWanted := tStringList.Create;
   FilesWanted.Add(fName);
   if GetMultipleFiles('text file','files|*.txt;*.csv',FilesWanted ,DefaultFilter) then begin
      for j := 0 to pred(FilesWanted.Count) do begin
         fName := FilesWanted.Strings[j];
         ShowInNotepadPlusPlus(fName);
      end;
   end;
   FilesWanted.Free;
end;

procedure Twmdem.Sieve1Click(Sender: TObject);
{$IfDef ExGeology}
begin
{$Else}
var
  SieveMainForm : Sieve_main.TSieveMainForm;
begin
   SieveMainForm := TSieveMainForm.Create(Application);
   SieveMainForm.Show;
   StopSplashing;
{$EndIf}
end;


procedure Twmdem.NewDEMButtonClick(Sender: TObject);
begin
   {$IfDef RecordMenu} WriteLineToDebugFile('Twmdem.NewDEMButtonClick'); {$EndIf}
   Newarea1Click(Sender);
end;


procedure Twmdem.NewglobalgridGreenwich1Click(Sender: TObject);
begin
   CreateNewGlobalGrid(true);
end;

procedure Twmdem.NewglobalgridIDL1Click(Sender: TObject);
begin
   CreateNewGlobalGrid(false);
end;

procedure Twmdem.NewSATButtonClick(Sender: TObject);
begin
   {$IfDef RecordMenu} WriteLineToDebugFile('Twmdem.NewSATButtonClick'); {$EndIf}
   if MDDef.ImageryIconDirs then OpenSentinen2image1Click(Sender)
   else PickAndOpenImagery(itSat);
end;


procedure Twmdem.LOSButtonClick(Sender: TObject);
begin
   LOS1Click(Sender);
end;


procedure Twmdem.Subsamplecomparethinaverage1Click(Sender: TObject);
const
   Thinned = 1;
   Meaned  = 2;
   nl = 5;
   Levels : array[1..nl] of integer = (2,5,10,20,25);
var
   i,j,graph : integer;
   zvs : ^bfarray32;
   ElevMoments,SlopeMoments :  array[0..2,1..nl] of tMomentVar;
   DEMSpacing  :  array[1..2,1..nl] of float32;
   ThinDEM,MeanDEM : array[1..nl] of integer;
   ThisGraph : TThisBaseGraph;
   aName : shortstring;
   aPath,
   fName : PathStr;
   rfile : file;
   v : array[1..2] of float32;
   Table : tMyData;
   AllFiles : tStringList;
   LegendBitmap : tMyBitmap;
begin
   if (NumDEMDataSetsOpen > 0) then begin
      if AnswerIsYes('Close all open DEMs to continue') then CloseAllDEMs
      else exit;
   end;

   New(zvs);
   AllFiles := tStringList.Create;
   fname := 'c:\temp\tandemx_compare\tandemx_test_areas.dbf';
   Table := tMyData.Create(fName);
   while not Table.eof do begin
      aPath := Table.GetFieldByNameAsString('PATH');
      aName := Table.GetFieldByNameAsString('NAME');
      OpenNewDEM(aPath + aName + '\tdx-04.dem',false);
      OpenNewDEM(aPath + aName + '\tdx-10.dem',false);
      OpenNewDEM(aPath + aName + '\tdx-30.dem',false);

      for i := 1 to nl do begin
         SetPanelText(0,IntToStr(i));

         DEMGlb[1].ThinThisDEM(ThinDEM[i],Levels[i]);
         ElevMoments[Thinned,i] := DEMGlb[ThinDEM[i]].ElevationMoments( DEMGlb[ThinDEM[i]].FullDEMGridLimits);
         DEMGlb[ThinDEM[i]].SlopeMomentsWithArray(DEMGlb[ThinDEM[i]].FullDEMGridLimits,SlopeMoments[Thinned,i],zvs^);
         DEMGlb[ThinDEM[i]].AreaName := 'Thinned_' + IntToStr(Levels[i]);
         DEMSpacing[Thinned,i] := DEMGlb[ThinDEM[i]].AverageSpace;

         DEMGlb[1].ThinThisDEM(MeanDEM[i],Levels[i],true);
         ElevMoments[Meaned,i] := DEMGlb[MeanDEM[i]].ElevationMoments( DEMGlb[MeanDEM[i]].FullDEMGridLimits);
         DEMGlb[MeanDEM[i]].SlopeMomentsWithArray(DEMGlb[MeanDEM[i]].FullDEMGridLimits,SlopeMoments[Meaned,i],zvs^);
         DEMGlb[MeanDEM[i]].AreaName := 'Mean_' + IntToStr(Levels[i]);
         DEMSpacing[Meaned,i] := DEMGlb[MeanDEM[i]].AverageSpace;
      end;

      Pick_Geostats.DoGeoStatAnalysis;
      ElevationSlopePlot(0);
      exit;

      for graph := 1 to 5 do begin
         ThisGraph := TThisBaseGraph.Create(Application);
         ThisGraph.SetUpGraphForm;
         ThisGraph.Caption := '';
         ThisGraph.GraphDraw.HorizLabel := 'DEM spacing (m)';
         ThisGraph.GraphDraw.SetShowAllLines(true);
         ThisGraph.GraphDraw.SetShowAllPoints(false);

         case graph of
            1 : ThisGraph.GraphDraw.VertLabel := 'Average elevation (m)';
            2 : ThisGraph.GraphDraw.VertLabel := 'Mean slope (%)';
            3 : ThisGraph.GraphDraw.VertLabel := 'Max elevation (m)';
            4 : ThisGraph.GraphDraw.VertLabel := 'Min elevation (m)';
            5 : ThisGraph.GraphDraw.VertLabel := 'Elevation std dev';
         end;
         ThisGraph.GraphDraw.LegendList := tStringList.Create;
         ThisGraph.GraphDraw.LegendList.Add('TanDEM-X DEMs');
         ThisGraph.GraphDraw.LegendList.Add('Thinned by decimation');
         ThisGraph.GraphDraw.LegendList.Add('Thinned by averaging');

          ThisGraph.OpenDataFile(rfile);
          for j := 1 to 3 do begin
             if ValidDEM(j) then begin
               ElevMoments[0,1] := DEMGlb[j].ElevationMoments( DEMGlb[j].FullDEMGridLimits);
               DEMGlb[j].SlopeMomentsWithArray(DEMGlb[j].FullDEMGridLimits,SlopeMoments[0,1],zvs^);
                v[1] := DEMGlb[j].AverageSpace;
                case graph of
                   1 : v[2] := ElevMoments[0,1].mean;
                   2 : v[2] := SlopeMoments[0,1].mean;
                   3 : v[2] := ElevMoments[0,1].MaxZ;
                   4 : v[2] := ElevMoments[0,1].MinZ;
                   5 : v[2] := ElevMoments[0,1].sdev;
                end;
                ThisGraph.AddPointToDataBuffer(rfile,v[1],v[2]);
             end;
          end;
          ThisGraph.ClosePointDataFile(rfile);

         for I := 1 to 2 do begin
             ThisGraph.OpenDataFile(rfile);
             for j := 1 to nl do begin
                v[1] := DEMSpacing[i,j];
                case graph of
                   1 : v[2] := ElevMoments[i,j].mean;
                   2 : v[2] := SlopeMoments[i,j].mean;
                   3 : v[2] := ElevMoments[i,j].MaxZ;
                   4 : v[2] := ElevMoments[i,j].MinZ;
                   5 : v[2] := ElevMoments[i,j].sdev;
                end;
                ThisGraph.AddPointToDataBuffer(rfile,v[1],v[2]);
             end;
             ThisGraph.ClosePointDataFile(rfile);
         end;
         ThisGraph.AutoScaleAndRedrawDiagram;
         fName := NextFileNumber(MDTempDir,'graph_image_','.png');
         SaveImageAsBMP(ThisGraph.Image1,fName);
         AllFiles.Add(fName);
         if (Graph = 5) then begin
            ThisGraph.Legend1Click(Nil);
            LegendBitmap := tMyBitmap.Create;
            LegendBitmap.Assign(ClipBoard);
            LegendBitmap.Height :=  LegendBitmap.Height + 100;
            LegendBitmap.Canvas.Font.Size := 24;
            LegendBitmap.Canvas.Font.Style := [fsBold];
            LegendBitmap.Canvas.TextOut(10,LegendBitmap.Height - 50,aName);

            fName := NextFileNumber(MDTempDir,'legend_','.png');
            PetImage.SaveBitmap(LegendBitmap,fName);
            LegendBitmap.Destroy;
            AllFiles.Add(fName);
         end;
         ThisGraph.Destroy;
      end;
      CloseAllDEMs;
      Table.Next;
      CloseAllDEMs;
   end;
   Table.Destroy;
   Dispose(zvs);
   MakeBigBitmap(AllFiles,'TanDEM-X resampling');
end;


procedure Twmdem.Subset81Ssidescan1Click(Sender: TObject);
begin
   {$IfDef ExSidescan}
   {$Else}
      Subset81Ssidescanfile;
   {$EndIf}
end;

procedure Twmdem.Superimposedtopoprofiles1Click(Sender: TObject);
begin
   SimpleProfiles;
   ChangeDEMNowDoing(MultipleLOS);
end;


procedure Twmdem.Spectrallibrary1Click(Sender: TObject);
begin
   {$IfDef ExSat}
   {$Else}
      SpectralLibraryGraph('');
   {$EndIf}
end;

procedure Twmdem.Spectrallibrary2Click(Sender: TObject);
begin
   {$IfDef ExSat}
   {$Else}
      SpectralLibraryGraph('');
   {$EndIf}
end;


procedure Twmdem.Spectrallibrary3Click(Sender: TObject);
begin
   {$IfDef ExSat}
   {$Else}
      SpectralLibraryGraph('');
   {$EndIf}
end;

procedure Twmdem.SpeedButton2Click(Sender: TObject);
begin
   {$IfDef RecordOpenVectorMap} WriteLineToDebugFile('Twmdem.SpeedButton2Click in'); {$EndIf}
   Openshapefilemap1Click(Sender);
end;

procedure Twmdem.SpeedButton3Click(Sender: TObject);
begin
   SimpleProfiles;
   LOS1Click(Sender);
end;


procedure Twmdem.MagMapButtonClick(Sender: TObject);
var
   dName,fName : PathStr;
begin
   dName := MainMapData + 'geology\geology_grids\';
   fName := 'EMAG2_V2.tif';
   if not FileExists(dName + fName) then begin
      {$IfDef RecordMGT} WriteLineToDebugFile('Did not find, and try to download ' + fName); {$EndIf}
      DownloadFileFromWeb(WebDataDownLoadDir + fName,dName + fName);
   end;
   if FileExists(dName + fName) then begin
      OpenNewDEM(dName + fName,false);
      DEMGlb[LastDEMLoaded].DEMheader.ElevUnits := Nanotesla;
      CreateDEMSelectionMap(LastDEMLoaded,true,true);
   end
   else begin
      {$IfDef RecordMGT} WriteLineToDebugFile('Did not find, and failed download ' + fName); {$EndIf}
      MagMapButton.Enabled := false;
   end;
end;


procedure Twmdem.CoordButtonClick(Sender: TObject);
begin
   StopSplashing;
   PopUpMenu7.Popup(Mouse.CursorPos.X,Mouse.CursorPos.Y);
end;


procedure Twmdem.CopyDBFstoXML1Click(Sender: TObject);
var
   inDir,fName: PathStr;
   TheFiles : tStringList;
   i,GISNum : integer;
begin
   inDir := MainMapData;
   GetDOSPath('input directory',InDir);
   TheFiles := Nil;
   Petmar.FindMatchingFiles(InDir,DefaultDBMask,TheFiles);
   for i := 0 to pred(TheFiles.Count) do begin
      fName := TheFiles.Strings[i];
      if OpenNumberedGISDataBase(GISNum,fName) then begin
         fName := fName + '.xml';
         GISdb[GISNum].ExportToXML(fName);
         CloseAndNilNumberedDB(GISNum);
      end;
   end;
   TheFiles.Free;
end;


procedure CopyFiles(JustCopy : boolean);
var
   fName: PathStr;
   NameMustHave : ShortString;
   Files : tStringList;
   i,NumCopied : integer;
begin
   GetDOSPath('input directory',CopyFilesFromDir);
   GetDOSPath('output directory',CopyFilesToDir);
   NameMustHave := 'dem';
   PetMar.GetString('File name must contain',NameMustHave,false,ReasonableTextChars);
   NameMustHave := UpperCase(NameMustHave);
   NumCopied := 0;
   StartProgressAbortOption('Copy');
   Files := Nil;
   Petmar.FindMatchingFiles(CopyFilesFromDir,'*.*',Files,4);
   for I := 0 to pred(Files.Count) do begin
     UpdateProgressBar(i/Files.Count);
     fName := CopyFilesToDir + ExtractFileName(Files.Strings[i]);
     if (StrUtils.AnsiContainsText(ExtractFileName(UpperCase(fName)),NameMustHave) or (NameMustHave = '')) and (not FileExists(fName)) then begin
        if JustCopy then Petmar.CopyFile(Files.Strings[i],fName)
        else Petmar.MoveFile(Files.Strings[i],fName);
        inc(NumCopied);
     end;
     if WantOut then break;
   end;
   Files.Free;
   EndProgress;
   MessageToContinue('Files copied: ' + IntToStr(NumCopied));
end;


procedure Twmdem.Copyfile1Click(Sender: TObject);
begin
   CopyFiles(true);
end;

procedure Twmdem.Movefileswithnamematch1Click(Sender: TObject);
begin
   CopyFiles(False);
end;


procedure Twmdem.Correlationmatrix1Click(Sender: TObject);
var
   fName : PathStr;
   TStr : ShortString;
begin
   StopSplashing;
   fName := '';
   if (Sender = Correlationmatrix1)  then TStr := 'Correlation'
   else TStr := 'Distance';
   if GetFileFromDirectory(TStr + ' matrix','*.csv',fName) then OpenCorrelationMatrix(TStr,fName);
end;


procedure Twmdem.Createcompositebitmap1Click(Sender: TObject);
begin
   RestoreBigCompositeBitmap('');
end;

procedure Twmdem.CreateDEMsfromlidar1Click(Sender: TObject);
begin
   CreateDEMsfromLidar;
end;


procedure Twmdem.ImportCTDfile1Click(Sender: TObject);
{$IfDef ExOceanography}
begin
{$Else}
type
   tProfInfo = record
      No_seg : int32;
      Prof_type : string[4];
      Deep_Depth : int32;
   end;
var
   fName : PathStr;
   DataFileInMemory,Stations,SubFile,Points  : tStringList;
   aLine,bLine : AnsiString;
   TempLine,SalLine : array[1..5] of ANSIString;
   TStr : shortstring;
   MinSV,sofar_m,
   LowDepth,HighDepth,
   LowLat,HighLat,LowLong,HighLong,
   aLat,aLong : float64;
   ExtractSubset,IncludeXBT,StationsDB,DepthLimits,
   InBox,CreatePoints : boolean;
   Year,Month,Day,i,j,k,Start,
   OnLine,No_Depths,OnDepth,OnDepth2,Mismatches,
   No_prof : integer;
   DTG,Stn_Num,CruiseID : ShortString;
   ProfInfo : array[1..30] of tProfInfo;
   sv,Depth,Temperature,Salinity : array[1..6000] of float64;
begin
   StopSplashing;
   fName := '';
   Petmar.GetFileFromDirectory('CTD data, MEDS format','*.*', fName);
   {$IfDef RecordOceanography} WriteLineToDebugFile('Twmdem.ImportCTDfile1Click inf=' + fName); {$EndIf}
   CreatePoints := AnswerIsYes('Create points DB');
   StationsDB := AnswerIsYes('Create stations DB');
   ExtractSubset := AnswerIsYes('Extract MEDS file');
   IncludeXBT := AnswerIsYes('Include XBT stations');
   DepthLimits := AnswerIsYes('Depth limits');

   if ExtractSubset then begin
      LowLat := -80;
      HighLat := 80;
      LowLong := -180;
      HighLong := 180;
      ReadDefault('Low lat',LowLat);
      ReadDefault('High lat',HighLat);
      ReadDefault('Low long',LowLong);
      ReadDefault('High long',HighLong);
   end;

   if DepthLimits then begin
      LowDepth := 495;
      HighDepth := 505;
      ReadDefault('Upper depth limit',LowDepth);
      ReadDefault('Lower depth limit',HighDepth);
   end
   else begin
      LowDepth := -1;
      HighDepth := 9999;
   end;

   if CreatePoints then begin
      Points := tStringList.Create;
      Points.Add('LAT,LONG,DEPTH,TEMP_C,SALINITY,SOUND_VEL');
   end;

   ShowHourglassCursor;
   if StationsDB then begin
      Stations := tStringList.Create;
      Stations.Add('DTG,CRUISEID,STATION,LAT,LONG,DEPTH,MAX_TEMP,MIN_TEMP,MAX_SAL,MIN_SAL,SOFAR_M,INSTRUMENT');
   end;

   DataFileInMemory := tStringList.Create;
   DataFileInMemory.LoadFromFile(fName);

   if ExtractSubset then begin
      Subfile := tStringList.Create;
   end;

   OnLine := -1;
   MisMatches := 0;

   while online < pred(DataFileInMemory.Count) do begin
      inc(Online);
      if (Online mod 500 = 0) then SetPanelText(0,IntToStr(Online));

      aLine := DataFileInMemory.Strings[OnLine];
      aLat := StrToFloat(ptTrim(Copy(aline,63,8)));
      aLong := -StrToFloat(ptTrim(Copy(aline,71,9)));
      Year := StrToInt(Copy(aline,27,4));
      Month := StrToInt(Copy(aline,31,2));
      Day := StrToInt(Copy(aline,33,2));
      No_prof := StrToInt(Copy(aline,122,2));

      Stn_Num := copy(aline,41,12);
      DTG := copy(aline,27,12);
      CruiseID := copy(aline,17,10);

      if ExtractSubset then begin
         InBox := (aLat > LowLat) and (aLat < HighLat) and (aLong > LowLong) and (aLong < HighLong);
      end
      else InBox := true;

      for I := 1 to No_Prof do  begin
         Start := 130 + pred(i) * 14;
         ProfInfo[i].No_seg := StrToInt(ptTrim(Copy(aline,Start+1,2)));
         ProfInfo[i].Prof_Type := Copy(aline,Start+3,4);
      end;

      OnDepth := 0;
      OnDepth2 := 0;
      for i := 1 to No_Prof do begin
         for j := 1 to ProfInfo[i].No_seg do begin
            //Get next line from memory
            inc(OnLine);
            bLine := DataFileInMemory.Strings[OnLine];
            //save line for export later
            if ProfInfo[i].Prof_Type = 'TEMP' then TempLine[j] := bline
            else SalLine[j] := bline;
            //process the line
            No_Depths := StrToInt(ptTrim(Copy(bline,59,4)));
            for k := 1 to No_Depths do begin
               if ProfInfo[i].Prof_Type = 'TEMP' then begin
                  inc(OnDepth);
                  Depth[OnDepth] := StrToFloat(ptTrim(Copy(bline,64 + pred(k) * 17,6)));
                  Temperature[OnDepth] := StrToFloat(ptTrim(Copy(bline,64 + 8 + pred(k) * 17,9)));
               end
               else if ProfInfo[i].Prof_Type = 'PSAL' then begin
                  inc(OnDepth2);
                  Salinity[OnDepth2] := StrToFloat(ptTrim(Copy(bline,64 + 8 + pred(k) * 17,9)))
               end;
            end;
         end;
      end;

      if (OnDepth = OnDepth2) then begin
         if ExtractSubset and (OnDepth > 10) and InBox then begin
            SubFile.Add(aline);
            for i := 1 to ProfInfo[1].No_seg do SubFile.Add(TempLine[i]);
            for i := 1 to ProfInfo[1].No_seg do SubFile.Add(SalLine[i]);
         end;

         MinSV := 9999;
         if (No_Prof = 2) then begin
            for i := 1 to OnDepth do begin
               sv[i] := SoundVelocity(Temperature[i],Salinity[i],Depth[i]);
               if sv[i] < MinSV then begin
                  MinSv := sv[i];
                  Sofar_m := Depth[i];
               end;
            end;

            if InBox and CreatePoints then begin
               for i := 1 to OnDepth do begin
                  if (Salinity[i] > 0.1) and (Depth[i] > LowDepth) and (Depth[i] < HighDepth) then begin
                     {$IfDef RecordFullOceanography} writeLineToDebugFile(aLine); {$EndIf}
                     Points.Add(RealToString(aLat,-12,-4) + ',' +
                                RealToString(aLong,-12,-2) + ',' +
                                RealToString(Depth[i],-12,-2) + ',' +
                                RealToString(Temperature[i],-12,-3) + ',' +
                                RealToString(Salinity[i],-12,-4) + ',' +
                                RealToString(sv[i],-12,-4));
                  end;
               end;
            end;
         end
         else begin
            TStr := '  ,  ,  ,XBT';
         end;

         if StationsDB and ((No_Prof = 2) or IncludeXBT) then begin
            aLine := DTG + ',' + CruiseID + ',' + Stn_Num + ',' + RealToString(aLat,-12,-3) + ',' + RealToString(aLong,-12,-3) + ',' + RealToString(Depth[OnDepth],-12,-3) + ',' +
                RealToString(PetMath.MaxInArray(OnDepth,Temperature),-12,-2) + ',' +
                RealToString(PetMath.MinInArray(OnDepth,Temperature),-12,-2) + ',' +  TStr;
            Stations.Add(aLine);
         end;
      end
      else inc(MisMatches);

   end;

   if ExtractSubset then begin
      SubFile.SaveToFile(ExtractFilePath(fName) + ExtractFileNameNoExt(fName ) + '_sub');
   end;

   DataFileInMemory.Destroy;
   SetPanelText(0,'');

   VectorMapButtonClick(Nil);
   if StationsDB then begin
      {$IfDef RecordOceanography} WriteLineToDebugFile('File had stations=' + IntToStr(Stations.Count)); {$EndIf}
      fName := ChangeFileExt(fName,'.csv');
      VectorMap[LastVectorMap].StringListToLoadedDatabase(Stations,fName);
   end;

   if CreatePoints then begin
      ShowHourglassCursor;
      Points.SaveToFile('c:\temp\pts.csv');
      fName := ExtractFilePath(fName) + ExtractFileNameNoExt(fName ) + '_points.csv';
      VectorMap[LastVectorMap].StringListToLoadedDatabase(Points,fName);
   end;
   {$EndIf}
end;



procedure Twmdem.InOutButtonClick(Sender: TObject);
begin
   Data1Click(Sender);
end;

procedure Twmdem.IntDBSpeedButtonClick(Sender: TObject);
begin
   {$IfDef ExIndexes}
   {$Else}
      VectorMapButtonClick(Sender);
   {$EndIf}
end;

procedure Twmdem.Italyfocalmechs1Click(Sender: TObject);
begin
   Afar1Click(Sender);
end;


procedure Twmdem.KangarooIslandadditionalscenes1Click(Sender: TObject);
begin
   DownloadandUnzipDataFileIfNotPresent('kangaroo_island_PA_new_scenes');
end;

procedure Twmdem.KMLKMZfile1Click(Sender: TObject);
begin
   PickForNotePadPlusPlus(1);
end;


procedure Twmdem.Koppenmonthlytemperatureandprecipitationclimatologies1Click(Sender: TObject);
begin
   Gridswithmonthlyprecipitationandtemperature1Click(Sender);
end;


procedure Twmdem.VectorMapButtonClick(Sender: TObject);
begin
   {$If Defined(RecordOpenVectorMap) or Defined(BasicOpens)} WriteLineToDebugFile('Twmdem.VectorMapButtonClick in'); {$EndIf}
   GetNaturalEarthData;
   StopSplashing;
   if ((Sender = VectorMapButton) or (Sender = OpenVectorMap1)) and MDdef.ShowCartography then begin
      PickProjections(0);
   end
   else begin
      LastVectorMap := SetUpVectorMap(true,true);
      if (LastVectorMap <> 0) then begin
         {$IfDef RecordOpenVectorMap} WriteLineToDebugFile('Twmdem.VectorMapButtonClick success'); {$EndIf}
         StopSplashing;
         {$IfDef ExGeology}
         {$Else}
            if (Sender = PlateRotateSpeedButton) or (Sender = Triplejunctions1) or (Sender = PlateRotations1) then begin
               VectorMap[LastVectorMap].PlateRotationSetup;
            end;
        {$EndIf}
         if (Sender = IntDBSpeedButton) then begin
            {$IfDef RecordOpenVectorMap} WriteLineToDebugFile('Twmdem.VectorMapButtonClick to VectorMap[LastVectorMap].Maplibrary1Click'); {$EndIf}
            VectorMap[LastVectorMap].Maplibrary1Click(Sender);
         end;
         if (Sender = LASdata1) or (Sender = OpenLASpointcloud1) then begin
            VectorMap[LastVectorMap].PointCloudSpeedButtonClick(Sender);
         end;
      end;
   end;
   {$IfDef RecordOpenVectorMap} WriteLineToDebugFile('Twmdem.VectorMapButtonClick out'); {$EndIf}
end;


procedure Twmdem.MGTMagModelSpeedButtonClick(Sender: TObject);
begin
   {$IfDef RecordMGT} writeLineToDebugFile('Twmdem.MGTMagModelSpeedButtonClick'); {$EndIf}
   Magneticmodel1Click(Sender);
end;


procedure Twmdem.MH370region1Click(Sender: TObject);
begin
   Afar1Click(Sender);
end;

procedure Twmdem.SpeedButton4Click(Sender: TObject);
begin
   {$IfDef RecordMenu} WriteLineToDebugFile('Twmdem.SpeedButton4Click in'); {$EndIf}
   StopSplashing;
   OutsideCSVImport := true;
   OpenDataBase('');
   OutsideCSVImport := false;
   {$IfDef RecordMenu} writeLineToDebugFile('Twmdem.SpeedButton4Click out'); {$EndIf}
end;

procedure Twmdem.SpeedButton5Click(Sender: TObject);
begin
  {$IfDef ExComplexGeoStats}
  {$Else}
     Pick_Geostats.DoGeoStatAnalysis;
  {$EndIf}
end;

procedure Twmdem.SpeedButton6Click(Sender: TObject);
begin
   Tools1Click(Sender);
end;

procedure Twmdem.Labs1Click(Sender: TObject);
begin
   GeologyPopUpMenu.Popup(Mouse.CursorPos.X,Mouse.CursorPos.Y);
end;

procedure Twmdem.LabSpeedButton7Click(Sender: TObject);
begin
   if (MDDef.ProgramOption = GeologyProgram) then GeologyPopUpMenu.Popup(Mouse.CursorPos.X,Mouse.CursorPos.Y)
   else GeographyPopUpMenu.Popup(Mouse.CursorPos.X,Mouse.CursorPos.Y);
end;

procedure Twmdem.Options1Click(Sender: TObject);
begin
   {$IfDef ExSetOptions}
   {$Else}
      ChangeOptions;
      {$IfDef RecordProblems} WriteLineToDebugFile('Options change ProgramMode=' + IntToStr(ord(MDdef.ProgramOption )));   {$EndIf}
   {$EndIf}
end;


procedure Twmdem.Other3Dpointclouds1Click(Sender: TObject);
begin
   {$IfDef ExLasLidar}
   {$Else}
   StopSplashing;
   if GetFileMultipleMask('3D GIS project', 'Project|proj*.dbf|Other GIS formats|*.dbf;*.shp|Text or csv|*.txt;*.csv|Unregistered LAS|*.las',DEMDefs.VasaProjectFName,MDDef.PCDefaultFilter) then begin
      Slicer_3D.DB_3dSlices(Nil,Nil,Nil);
   end;
  {$EndIf}
end;

procedure Twmdem.Exitprogram2Click(Sender: TObject);
begin
   {$IfDef RecordClosing} writeLineToDebugFile('Twmdem.Exitprogram2Click Clicked exit program from pop up menu'); {$EndIf}
   CloseAlldataandwindows1Click(Nil);
   Close;
   {$IfDef RecordClosing} WriteLineToDebugFile('Twmdem.Exitprogram2Click out'); {$EndIf}
end;


procedure Twmdem.extEditor1Click(Sender: TObject);
var
   FilesWanted : tStringList;
   i : integer;
begin
   FilesWanted := tStringList.Create.Create;
   FilesWanted.Add(MainMapData);
   if GetMultipleFiles('text files','*.*',FilesWanted,MDDef.DefaultDEMFilter) then begin
      for i := 0 to pred(FilesWanted.Count) do
         QuickOpenEditWindow(FilesWanted.Strings[i],'');
   end;
end;


procedure Twmdem.CloseallDBs1Click(Sender: TObject);
begin
   CloseAllDataBases;
end;

procedure Twmdem.CloseallDEMs1Click(Sender: TObject);
begin
   DEMNowDoing := Calculating;
   CloseAllMultigrids;
   DEM_Manager.CloseAllDEMs;
end;


procedure Twmdem.Closeallgraphs1Click(Sender: TObject);
var
   i : integer;
begin
   for i := pred(WMDEM.MDIChildCount) downto 0 do begin
      if WMDEM.MDIChildren[i] is TThisBaseGraph then (WMDEM.MDIChildren[i] as TThisBaseGraph).Close;
      if WMDEM.MDIChildren[i] is TNetForm then (WMDEM.MDIChildren[i] as TNetForm).Close;
   end;
end;



procedure Twmdem.CloseallImagery1Click(Sender: TObject);
begin
   DEM_Manager.CloseAllImagery;
end;

procedure Twmdem.Closeallmaps1Click(Sender: TObject);
begin
   CloseAllMaps;
end;


function NumPictureViewWindows : integer;
var
   i : integer;
begin
   Result := 0;
   for i := pred(WMDEM.MDIChildCount) downto 0 do
      if WMDEM.MDIChildren[i] is TImageDisplayForm then begin
         inc(Result);
      end;
end;

function NumTextEditWindows : integer;
var
   i : integer;
begin
   Result := 0;
   for i := pred(WMDEM.MDIChildCount) downto 0 do
      if WMDEM.MDIChildren[i] is TPetEditf then begin
         inc(Result);
      end;
end;


procedure Twmdem.Closeallpictureviewwindows1Click(Sender: TObject);
var
   i : integer;
begin
   for i := pred(WMDEM.MDIChildCount) downto 0 do
      if WMDEM.MDIChildren[i] is TImageDisplayForm then (WMDEM.MDIChildren[i] as TImageDisplayForm).Close;
end;

procedure Twmdem.Closealltexteditwindows1Click(Sender: TObject);
var
   i : integer;
begin
   for i := pred(WMDEM.MDIChildCount) downto 0 do
      if WMDEM.MDIChildren[i] is TPetEditf then (WMDEM.MDIChildren[i] as TPetEditf).Close;
end;

procedure Twmdem.Closeprogramgetdebugversionoftheprogram7MB1Click(Sender: TObject);
begin
   {$IfDef RecordUpdate} writeLineToDebugFile('Twmdem.Closeprogramandupdate1Click in'); {$EndIf}
   if (UpperCase(ProgramRootDir) <> 'C:\MICRODEM\') then begin
      if not AnswerIsYes('This will download to c:\microdem\ and reopen there; Continue') then exit;
   end;
   DEM_Manager.CloseAllWindowsAndData;
   AskForDebugUpdateNow := true;
   Close;
end;

procedure Twmdem.CloseprogramupdateEXEnewversion7MBdownload1Click(Sender: TObject);
begin
   {$IfDef RecordUpdate} writeLineToDebugFile('Twmdem.Closeprogramandupdate1Click in'); {$EndIf}
   if (UpperCase(ProgramRootDir) <> 'C:\MICRODEM\') then begin
      if not AnswerIsYes('This will download to c:\microdem\ and reopen there; Continue') then exit;
   end;
   DEM_Manager.CloseAllWindowsAndData;
   AskForNewUpdateNow := true;
   Close;
end;

procedure Twmdem.Closewindows1Click(Sender: TObject);
begin
   ClosePopUpMenu.Popup(Mouse.CursorPos.X,Mouse.CursorPos.Y);
end;

procedure Twmdem.Clustergrids1Click(Sender: TObject);
begin
   {$IfDef AllowGeomorphometry}
      ClusterGrids(1,MaxDEMDataSets);
   {$EndIf}
end;

procedure Twmdem.Classificationmap1Click(Sender: TObject);
begin
   {$IfDef ExGeography}
   {$Else}
      VectorMapButtonClick(Nil);
      VectorMap[LastVectorMap].Koppengrid1Click(Sender);
   {$EndIf}
end;


procedure Twmdem.Climate1Click(Sender: TObject);
begin
   {$IfDef ExGeography}
   {$Else}
      ClimateGetData(true);
   {$EndIf}
end;

procedure Twmdem.Climatestationsforclimographs1Click(Sender: TObject);
begin
   {$IfDef ExGeography}
   {$Else}
      VectorMapButtonClick(Nil);
      VectorMap[LastVectorMap].KoppenClimateStations1Click(Sender);
      CreateKoppenLegend(false);
   {$EndIf}
end;

procedure Twmdem.CloseAlldataandwindows1Click(Sender: TObject);
begin
   DEM_Manager.CloseAllWindowsAndData;
   pt_cloud_opts_fm_Close;
   CloseGeoStatAnalysis;
end;


procedure Twmdem.Existingfile1Click(Sender: TObject);
{$IfDef ExMovie}
begin
{$Else}
begin
   StopSplashing;
   CreateNewMovie;
{$EndIf}
end;

procedure Twmdem.Existingfile2Click(Sender: TObject);
{$IfDef ExMovie}
begin
{$Else}
begin
   StopSplashing;
   CreateNewMovie('',true);
{$EndIf}
end;

procedure Twmdem.Exit2Click(Sender: TObject);
begin
   Closeallmaps1.Visible := NumOpenMaps > 0;
   Closeallgraphs1.Visible := NumOpenGraphs > 0;
   CloseAllImagery1.Visible := NumSatImageOpen > 0;
   Closeallpictureviewwindows1.Visible := NumPictureViewWindows > 0;
   Closealltexteditwindows1.Visible := NumTextEditWindows > 0;
   CloseallDEMs1.Visible := NumDEMDataSetsOpen > 0;
   ClosePopUpMenu.Popup(Mouse.CursorPos.X,Mouse.CursorPos.Y);
end;


procedure Twmdem.Opendatasets1Click(Sender: TObject);
var
   OpenData : tStringList;
begin
   OpenData := GetWhatsOpen;
   DisplayAndPurgeStringList(OpenData,'Open windows & data sets');
end;


procedure Twmdem.OpenDEM1Click(Sender: TObject);
begin
   Newarea1Click(Sender);
end;

procedure Twmdem.OpenDEMwithoutmap1Click(Sender: TObject);
begin
   OpenNewDEM('',false);
end;

procedure Twmdem.OGRinfo1Click(Sender: TObject);
begin
   {$IfDef ExGDAL}
   {$Else}
      GeoPDF1Click(Sender);
   {$EndIf}
end;


procedure Twmdem.Onlinehelp1Click(Sender: TObject);
begin
   ExecuteFile('https://www.usna.edu/Users/oceano/pguth/md_help/html/microdem.htm', '', '');
end;

procedure Twmdem.Flythrough1Click(Sender: TObject);
begin
   ChangeDEMNowDoing(SeekingFlyThroughRoute);
end;

procedure Twmdem.FlySpeedButtonClick(Sender: TObject);
begin
   ChangeDEMNowDoing(SeekingFlyThroughRoute);
end;


procedure Twmdem.PerspectiveButtonClick(Sender: TObject);
begin
   ChangeDEMNowDoing(SeekingPerspective);
end;


procedure Twmdem.Physicalgeographylabs1Click(Sender: TObject);
begin
   GeographyPopUpMenu.Popup(Mouse.CursorPos.X,Mouse.CursorPos.Y);
end;

procedure Twmdem.Piracywindsandrain1Click(Sender: TObject);
{$IfDef ExMultigrid}
begin
{$Else}
var
   fName : PathStr;
begin
   VectorMapButtonClick(Sender);
   fName := DBDir + 'somali_piracy_jan_2016_v2' + DefaultDBExt;
   if not FileExists(FName) then begin
       DownloadFileFromWeb(WebDataDownLoadDir + ExtractFileName(FName),FName);
   end;
   VectorMap[LastVectorMap].OpenDBonMap('',FName,true,true);
   VectorMap[LastVectorMap].Globalmonthlywinds1Click(nil);
   db_display_options.OpenMapTableOfContents(VectorMap[LastVectorMap],true);
   MDDef.ApplySameFilterAllDBs := true;
   OpenMonthlyMultiGrids('Precipitation');
{$EndIf}
end;


procedure Twmdem.Perspective1Click(Sender: TObject);
begin
   PerspectiveButtonClick(Sender);
end;


procedure Twmdem.Slidesorter1Click(Sender: TObject);
var
   TheFiles : tStringList;
   fName,fName2 : PathStr;

   procedure FindFiles;
   var
      i : integer;
   begin
      TheFiles := Nil;
      Petmar.FindMatchingFiles(PhotoDir,'*.*',TheFiles,6);
      for i := Pred(TheFiles.Count) downto 0 do begin
          if (not PetImage.ValidImageFileName(TheFiles.Strings[i])) then TheFiles.Delete(i);
      end;
   end;



begin
   if (PhotoDir = '') then PhotoDir := MainMapData;
   
   if GetDosPath('Photo directory',PhotoDir) then begin
      FindFiles;
      if (TheFiles.Count > 0) then begin
         fName := UpperCase(ExtractFileNameNoExt(TheFiles.Strings[0]));
         if (length(Fname) = 8) then begin
            if StrUtils.AnsiContainsText(ExtractFileNameNoExt(fName),'DSC') then begin
               RenamePhotoJPEGS(PhotoDir,'DSC');
               TheFiles.Free;
               FindFiles;
            end;
            if StrUtils.AnsiContainsText(ExtractFileNameNoExt(fName),'P') then begin
               RenamePhotoJPEGS(PhotoDir,'P');
               TheFiles.Free;
               FindFiles;
            end;
         end;
         TheFiles.SaveToFile(MDTempDir + 'slide_sorter.txt');
         fName2 := PhotoDir + 'photo_index' + DefaultDBExt;
         if not FileExists(fName2) then MakePhotoDB(PhotoDir);
         if (SlideSorterForm = Nil) then begin
            SlideSorterForm := TSlideSorterForm.Create(Application);
            SlideSorterForm.Show;
            SlideSorterForm.WindowState := TWindowState.wsMaximized;
         end;
         SlideSorterForm.PhotoDB := tMyData.Create(fName2);
         SlideSorterForm.LoadPictures;
         SlideSorterForm.Caption := SlideSorterForm.Caption + '  ' + PhotoDir;
      end;
      TheFiles.Free;
   end;
end;


procedure Twmdem.NLCD20011Click(Sender: TObject);
var
   Path : PathStr;
   tf : tStringList;
   TStr : string35;
   //ch : char;
begin
   tf := tStringList.Create;
   Path := MainMapData;
   GetDOSPath('with ' + Tstr + ' data',Path);
   if (Sender = UTM1) then begin
      TStr := 'utm.txt';
      PickUTMZone(MDDef.DefaultUTMZone);
      tf.Add(IntToStr(MDDef.DefaultUTMZone));
      tf.Add('WGS84');
   end
   else if (Sender = XYZshapefile1) then begin
      TStr := 'xyz_files.txt';
   end
   else if (Sender = Mercator1) then begin
      TStr := 'mercator.txt';
   end
   else if (Sender = SouthPolarStereographic1) then begin
      TStr := 'south_polar_stereographic.txt';
   end
   else if (Sender = UKOSgrid2) then begin
      TStr := 'UK_OS_Grid.txt';
   end;
   tf.SaveToFile(Path + TStr);
   tf.Free;
end;


procedure Twmdem.Normalizegrids1Click(Sender: TObject);
{$IfDef ExComplexGeoStats}
begin
{$Else}
var
   dbfDir,GridDir,fName : PathStr;
   TheFiles : tStringList;
   i,aDEM,nDEM : integer;
begin
   {$IfDef RecordGeostats} WriteLineToDebugFile('Twmdem.Normalizegrids1Click in'); {$EndIf}
   GetAtlasDirectories(dbfDir,GridDir);
   TheFiles := Nil;
   Petmar.FindMatchingFiles(GridDir,'*.dem',TheFiles,0);
   StartThreadTimers('Stat',1);
   for i := 0 to pred(TheFiles.Count) do begin
      ThreadTimers.OverallGauge9.Progress := round(100 * i/TheFiles.Count);
      fName := TheFiles.Strings[i];
      {$IfDef RecordGeostats} writeLineToDebugFile(fName); {$EndIf}
      aDEM := OpenNewDEM(fName,false);
      nDEM := MakeSingleNewDerivativeMap('n',aDEM,0,false);
      fName := ExtractFilePath(GridDir) + 'normalized\' + ExtractFileName(DEMGlb[aDEM].DEMFileName);
      DEMGlb[nDEM].WriteNewFormatDEM(fName);
      CloseSingleDEM(aDEM);
      CloseSingleDEM(nDEM);
   end;
   TheFiles.Free;
   EndThreadTimers;
{$EndIf}
end;


procedure Twmdem.Nyquist1Click(Sender: TObject);
var
   NyquistBaseGraph : TNyquistBaseGraph;
   i : integer;
begin
   for i := 1 to 4 do begin
      NyquistBaseGraph := TNyquistBaseGraph.Create(Application);
      with NyquistBaseGraph,GraphDraw do begin
         MaxHorizAxis := 240;
         MinVertAxis := -1.5;
         MaxVertAxis := 1.5;
         DrawGraph(Nil);
         GraphDrawn := true;
         case i of
            1 : SamplingInterval := 25;
            2 : SamplingInterval := 18;
            3 : SamplingInterval := 14;
            4 : SamplingInterval := 10;
         end;
         FormResize(Sender);
      end;
   end;
   StopSplashing;
end;


procedure Twmdem.RenameJPEGSwithbaseandcreationtime1Click(Sender: TObject);
var
   FilesWanted : TStringList;
   NewName,OldName : PathStr;
   NewBase,bt : shortstring;
   DefaultFilter : byte;
   i : integer;
begin
   DefaultFilter := 1;
   FilesWanted := tStringList.Create;
   FilesWanted.Add(PhotoDir);
   if GetMultipleFiles('Rename with base/sequence number','AnyFile|*.*',FilesWanted, DefaultFilter) then begin
      NewBase := '';
      GetString('New base name',NewBase,false,ValidDosFileNameChars);
      for i := 1 to FilesWanted.Count do begin
         OldName := FilesWanted.Strings[pred(i)];
         bt := NewBase + '_' + FileTimeFromFileName(OldName);
         NewName := ExtractFilePath(OldName) + bt + ExtractFileExt(OldName);
         while FileExists(NewName) do begin
            NewName := Petmar.NextFileNumber(ExtractFilePath(OldName),bt,ExtractFileExt(OldName));
         end;
         RenameFile(OldName,NewName);
      end;
   end;
end;

procedure Twmdem.RenameJPEGswithcreationtime1Click(Sender: TObject);
begin
   RenamePhotoJPEGS;
end;


procedure Twmdem.RenameJPGsfromlockscreen1Click(Sender: TObject);
var
   FilesWanted : TStringList;
   DefaultFilter : byte;
   i : integer;
begin
   DefaultFilter := 1;
   FilesWanted := tStringList.Create;
   FilesWanted.Add(MainMapData);
   if GetMultipleFiles('Change extension','AnyFile|*.*',FilesWanted, DefaultFilter) then begin
      for i := 0 to pred(FilesWanted.Count) do begin
         RenameFile(FilesWanted.Strings[i],FilesWanted.Strings[i] + '.jpg');
      end;
   end;
end;


procedure Twmdem.RenameJPRGswithbasenamenumber1Click(Sender: TObject);
var
   FilesWanted : TStringList;
   NewName,OldName : PathStr;
   NewBase : shortstring;
   DefaultFilter : byte;
   i : integer;
begin
   DefaultFilter := 1;
   FilesWanted := tStringList.Create;
   FilesWanted.Add(PhotoDir);
   if GetMultipleFiles('Rename with base/sequence number','AnyFile|*.*',FilesWanted, DefaultFilter) then begin
      NewBase := '';
      GetString('New base name',NewBase,false,ValidDosFileNameChars);
      for i := 1 to FilesWanted.Count do begin
         OldName := FilesWanted.Strings[pred(i)];
         NewName := ExtractFilePath(OldName) + NewBase + '_' + IntToStr(i) + ExtractFileExt(OldName);
         RenameFile(OldName,NewName);
      end;
   end;
end;


procedure Twmdem.RestorepreviousprogramEXE1Click(Sender: TObject);
begin
   RestorePreviousEXE;
end;


procedure Twmdem.RGBcolorlayers1Click(Sender: TObject);
var
   im : TImageDisplayForm;
begin
   im := OpenImageEditor;
   im.MakeRGBandgrayscaleseparates1Click(nil);
end;


procedure Twmdem.Trenchgeometry1Click(Sender: TObject);
begin
   Afar1Click(Sender);
end;

procedure Twmdem.Triplejunctions1Click(Sender: TObject);
begin
   PlateRotateSpeedButtonClick(Sender);
end;

procedure Twmdem.Circlearound1Click(Sender: TObject);
begin
   ChangeDEMNowDoing(SeekingFirstCircleFly);
end;


procedure Twmdem.LiveFlySpeedButtonClick(Sender: TObject);
begin
   ChangeDEMNowDoing(LiveFly);
end;

procedure Twmdem.StereoNetButtonClick(Sender: TObject);
begin
   StereoNet1Click(Sender);
end;


procedure Twmdem.Stereopair1Click(Sender: TObject);
begin
   Stereo_viewer.ShowStereoPair('','');
end;


procedure Twmdem.SanitizedXTF1Click(Sender: TObject);
{$IfDef ExSideScan}
begin
{$Else}
var
   dName : PathStr;
   i : integer;
begin
   MDDef.SonarMapDef.CustomPalette := true;
   Maxwellplanning1Click(Sender);
   dName := 'sanitized_wreck';
   DownloadandUnzipDataFileIfNotPresent(dName);
   for i := 1 to 4 do LoadSideScanFile(DEMGlb[LastDEMLoaded].SelectionMap,MainMapData + dName + '\sanitized_' + IntToStr(i) + '.xtf');
{$EndIf}
end;


procedure Twmdem.Satellitepredictions1Click(Sender: TObject);
begin
    {$IfDef ExTrackSat}
    {$Else}
       SatTractForm := TSatTractForm.Create(Application);
       wmdem.Vectormap1Click(Sender);
       VectorMap[LastVectorMap].Closable := false;
       VectorMap[LastVectorMap].Caption := 'GPS satellite predictions';
       SatTractForm.MapOwner := VectorMap[LastVectorMap];
       SatTractForm.BitBtn5.Enabled := false;
       SatTractForm.Button1Click(Sender);
    {$EndIf}
end;

procedure Twmdem.Savedesktopandexitprogram1Click(Sender: TObject);
begin
   SaveMicrodemDesktop;
   Exitprogram2Click(Sender);
end;


procedure Twmdem.Sdeimentthickness1Click(Sender: TObject);
begin
   SedThickButtonClick(Sender);
end;

procedure Twmdem.SpeedButton8Click(Sender: TObject);
begin
   RestoreMicrodemDesktop;
end;


procedure Twmdem.Mediansatellitedatacontest1Click(Sender: TObject);
begin
   {$IfDef Include2021datafusion}  Experimental_MD.MedianOfSatelliteData;  MakeLittleTiles; {$EndIf}
end;

procedure Twmdem.Megathrusts1Click(Sender: TObject);
begin
   Afar1Click(Sender);
end;

procedure Twmdem.Mercator1Click(Sender: TObject);
begin
   NLCD20011Click(Sender);
end;

procedure Twmdem.Mergemasp1Click(Sender: TObject);
begin
   {$IfDef ExGDAL}
   {$Else}
      GDALconvertGeoPDF(gdalMergeGeoPDF1);
   {$EndIf}
end;


procedure Twmdem.MergewavelengthheightDBFs1Click(Sender: TObject);
{$IfDef ExComplexGeoStats}
begin
{$Else}

const
   MaxParams = 50;
var
   NumParams : integer;
   ParamDEMs : array[1..MaxParams] of integer;
   ParamsUsed : array[1..MaxParams] of ShortString;
   GridDir,DBFdir : PathStr;
   TStr : ShortString;
   FieldsInDB,TheFiles : tStringList;
   Table : tMyData;


         procedure CreateGrid(i : integer);
         var
            fName : PathStr;
         begin
            {$IfDef RecordGeostats} WriteLineToDebugFile('CreateGrid in ' + ParamsUsed[i]); {$EndIf}
            fName := GridDir + ParamsUsed[i] + '.dem';
            if FileExists(fName) then begin
               LoadNewDEM(ParamDEMs[i],fName,false);
               {$IfDef RecordGeostats} writeLineToDebugFile('Grid existed ' + IntToStr(ParamDEMs[i]));   {$EndIf}
            end
            else begin
               OpenDEMDataStructures(ParamDEMs[i]);
               with DEMGlb[ParamDEMs[i]],DEMheader do begin
                  DEMheader.DEMPrecision := FloatingPointDEM;
                  DigitizeDatum := Spherical;
                  DEMySpacing := 2.5 /60;
                  DEMxSpacing := 2.5 / 60;
                  DEMSWCornerX := -180 + 0.5 * DEMxSpacing;
                  DEMSWCornerY := -56 + 0.5 * DEMySpacing;
                  NumCol := 360 * 24;
                  NumRow := (60+56) * 24;
                  DataSpacing := SpaceDegrees;
                  DEMUsed := ArcSecDEM;
                  DEMheader.UTMZone := GetUTMZone(DEMSWCornerX + 0.5 * NumCol * DEMxSpacing);
                  ElevUnits  := Undefined;
                  AllocateDEMMemory(true);
                  DefineDEMVariables(true);
                  {$IfDef RecordGeostats} writeLineToDebugFile('New grid created ' + IntToStr(ParamDEMs[i])); {$EndIf}
               end;
            end;
         end;


      procedure DoPart(FirstDEM,LastDEM : integer);
      var
         Lat,Long,xg,yg : float64;
         fName : PathStr;
         i,j,Col,Row : integer;
      begin
         if (LastDEM > NumParams) then LastDEM := NumParams;
         for i := FirstDEM to LastDEM do begin
            CreateGrid(i);
         end;
         StartProgress('Fill grids ' + IntToStr(FirstDEM) + ' to ' + IntToStr(LastDEM) + ' of ' + IntToStr(NumParams));
         for j := 0 to pred(TheFiles.Count) do begin
            UpdateProgressBar(j/TheFiles.Count);
            fName := TheFiles.Strings[j];
            Table := tMyData.Create(fName);
            while not Table.Eof do begin
                Lat := Table.GetFieldByNameAsFloat('LAT');
                Long := Table.GetFieldByNameAsFloat('LONG');
                DEMGlb[ParamDEMs[FirstDEM]].LatLongDegreeToDEMGrid(Lat,Long,xg,yg);
                col := round(xg);
                row := round(yg);
                for i := FirstDEM to LastDEM do begin
                    if Table.FieldExists(ParamsUsed[i])then begin
                       if (Table.GetFieldByNameAsString(ParamsUsed[i]) <> '') then begin
                          DEMGlb[ParamDEMs[i]].SetGridElevation(Col,Row,Table.GetFieldByNameAsFloat(ParamsUsed[i]));
                       end;
                    end;
                end;
                Table.Next;
            end;
            Table.Destroy;
         end;
         EndProgress;
         for i := FirstDEM to LastDEM do begin
            fName := GridDir + ParamsUsed[i] + '.dem';
            DEMGlb[ParamDEMs[i]].WriteNewFormatDEM(fName);
            CloseSingleDEM(ParamDEMs[i]);
         end;
      end;

var
   i : integer;
   fName : PathStr;
begin
   {$IfDef RecordGeostats} writeLineToDebugFile('Twmdem.MergewavelengthheightDBFs1Click in'); {$EndIf}
   StopSplashing;
   GetAtlasDirectories(dbfDir,GridDir);

   TheFiles := Nil;
   Petmar.FindMatchingFiles(DBFdir,DefaultDBMask,TheFiles,0);
   fName := TheFiles.Strings[0];
   Table := tMyData.Create(fName);
   GetFields(Table,AllVis,[ftFloat],FieldsInDB,true);
   NumParams := 0;
   for i := 0 to pred(FieldsInDB.Count) do begin
      TStr := FieldsInDB.Strings[i];
      if (TStr <> 'LAT') and (TStr <> 'LONG') then begin
         inc(NumParams);
         ParamsUsed[NumParams] := TStr;
      end;
   end;
   FieldsInDB.Free;
   Table.Destroy;

   {$IfDef RecordGeostats} writeLineToDebugFile('found fields'); {$EndIf}

   for i := 1 to NumParams do ParamDEMs[i] := 0;
   i := 1;
   while (i <= NumParams) do begin
      DoPart(i,I + 7);
      inc(i,8);
   end;
   TheFiles.Free;
   {$IfDef RecordGeostats} WriteLineToDebugFile('Twmdem.MergewavelengthheightDBFs1Click out'); {$EndIf}
{$EndIf}
end;


procedure Twmdem.MetaData1Click(Sender: TObject);
begin
   StopSplashing;
   MrSidImagery1.Visible := MrSidEnabled;
   MetaDataPopupMenu.PopUp(Mouse.CursorPos.X,Mouse.CursorPos.Y);
end;


procedure Twmdem.GDALslopesarcsecondDEMs1Click(Sender: TObject);
var
   CurDEM : integer;
   Distance1,Distance2,Distance3 : float64;
begin
   if GetDEM(CurDEM,true,'GDAL spacing') then begin
      if (DEMGlb[CurDEM].DEMheader.DEMUsed = ArcSecDEM) then begin
         MetersPerDegree(DEMGlb[CurDEM].DEMSWcornerLat + 0.5 * DEMGlb[CurDEM].LatSizeMap,DEMGlb[CurDEM].DEMSWcornerLong + 0.5 * DEMGlb[CurDEM].LongSizeMap,Distance1,Distance2,Distance3);
         GDAL_SlopeMap_ZT(DEMGlb[CurDEM].SelectionMap.GeotiffDEMNameOfMap,RealToString(Distance1,-12,1),MDTempDir + 'gdal_zt_ns' + '.tif');
         GDAL_SlopeMap_ZT(DEMGlb[CurDEM].SelectionMap.GeotiffDEMNameOfMap,RealToString(Distance2,-12,1),MDTempDir + 'gdal_zt_ew' + '.tif');
         GDAL_SlopeMap_ZT(DEMGlb[CurDEM].SelectionMap.GeotiffDEMNameOfMap,RealToString(Distance3,-12,1),MDTempDir + 'gdal_zt_avg' + '.tif');
         GDAL_SlopeMap_Horn(DEMGlb[CurDEM].SelectionMap.GeotiffDEMNameOfMap,RealToString(Distance1,-12,1),MDTempDir + 'gdal_horn_ns' + '.tif');
         GDAL_SlopeMap_Horn(DEMGlb[CurDEM].SelectionMap.GeotiffDEMNameOfMap,RealToString(Distance2,-12,1),MDTempDir + 'gdal_horn_ew' + '.tif');
         GDAL_SlopeMap_Horn(DEMGlb[CurDEM].SelectionMap.GeotiffDEMNameOfMap,RealToString(Distance3,-12,1),MDTempDir + 'gdal_horn_avg' + '.tif');
      end
      else MessageToContinue('Not arc second DEM');
   end;
end;


procedure Twmdem.GDALSRSinfo1Click(Sender: TObject);
{$IfDef ExGDAL}
begin
{$Else}
var
   fName,OutName : PathStr;
   defFilter : byte;
   i : int16;
   FilesWanted : tStringList;
begin
   FilesWanted := tStringList.Create;
   FilesWanted.Add(MainMapData);
   DefFilter := 1;
   if GetMultipleFiles('GeoPDF or Geotiff or IMG or SHP','*.PDF;*.TIF;*.TIFF;*.IMG;*.shp',FilesWanted ,DefFilter) then begin
       BatchGDALsrsinfo(FilesWanted);
       for i := 0 to pred(FilesWanted.Count) do begin
          fName := FilesWanted.Strings[i];
          OutName := MDTempDir + ExtractFileName(fName) + '.prj';
          ShowInNotepadPlusPlus(OutName,'GDAL_srs_info results ' + ExtractFileName(fName));
       end;
      FilesWanted.Free;
    end;
{$EndIf}
end;

procedure Twmdem.GDALWKT1Click(Sender: TObject);
begin
   GeotiffMetadata(mdGDAL,ExtractFilePath(LastImageName));
end;

procedure Twmdem.Geoid1Click(Sender: TObject);
var
   DEM : integer;
begin
   DEM := OpenNewDEM(Geoid2008FName);
   AddOrSubtractOverlay(DEMGlb[DEM].SelectionMap,ovoWorldOutlines,true);
   DEMGlb[DEM].SelectionMap.DoFastMapRedraw;
end;

procedure Twmdem.Geoid2Click(Sender: TObject);
begin
   Geoid1Click(Sender);
end;

procedure Twmdem.Geoidandsedimentdistribution1Click(Sender: TObject);
begin
   Afar1Click(Sender);
end;


procedure Twmdem.Geology2Click(Sender: TObject);
begin
{$IfDef ExGeology}
{$Else}
   GeologyGetData(true);
{$EndIf}
end;

procedure Twmdem.GeoPDF1Click(Sender: TObject);
{$IfDef ExGDAL}
begin
{$Else}
var
   fName,OutName : PathStr;
   defFilter : byte;
   i : int16;
   FilesWanted : tStringList;
begin
   FilesWanted := tStringList.Create;
   FilesWanted.Add(MainMapData);
   DefFilter := 1;
   if GetMultipleFiles('GeoPDF or Geotiff or IMG or SHP','*.PDF;*.TIF;*.TIFF;*.IMG;*.shp',FilesWanted ,DefFilter) then begin
       BatchGDALinfo(FilesWanted,false,i);
       for i := 0 to pred(FilesWanted.Count) do begin
          fName := FilesWanted.Strings[i];
          OutName := GDALinfoOutput(fName);
          ShowInNotepadPlusPlus(OutName,'GDALinfo results ' + ExtractFileName(fName));
       end;
      FilesWanted.Free;
    end;
{$EndIf}
end;


procedure Twmdem.Geostatisticalanalysis1Click(Sender: TObject);
begin
  {$IfDef ExComplexGeoStats}
  {$Else}
    Pick_Geostats.DoGeoStatAnalysis;
  {$EndIf}
end;


procedure Twmdem.GeoTIFF1Click(Sender: TObject);
begin
   GeotiffMetadata(mdMicrodem,ExtractFilePath(LastImageName));
end;



procedure Twmdem.OpenGeoPDF1Click(Sender: TObject);
begin
   {$IfDef ExGDAL}
   {$Else}
      GDALconvertGeoPDF(gdalOpenGeoPDF1);
   {$EndIf}
end;


procedure Twmdem.OpenGeoPDFimagelayer1Click(Sender: TObject);
begin
   {$IfDef ExGDAL}
   {$Else}
      GDALconvertGeoPDF(gdalOpenGeoPDFimagelayer1);
   {$EndIf}
end;


procedure Twmdem.Opengooglemaps1Click(Sender: TObject);
begin
   SimpleLoadGoogleMaps(-999,-999);
end;

procedure Twmdem.Openhyperspectralimagery1Click(Sender: TObject);
begin
   HypImageSpeedButtonClick(Sender);
end;

procedure Twmdem.OpenImage1Click(Sender: TObject);
begin
   PickAndOpenImagery(itSat);
end;


procedure Twmdem.Openimagewithmultiplebands1Click(Sender: TObject);
var
   Bands : tStringList;
begin
   {$IfDef RecordMenu} writeLineToDebugFile('Twmdem.Openimagewithmultiplebands1Click in'); {$EndIf}
    Bands := tStringList.Create;
    Bands.Add(LastImageName);
    GetMultipleFiles('Multiple Bands',GetSatMaskList(true),Bands,MDDef.DefaultSatFilter);
    Bands.Sorted := true;
    LastImageName := Bands.Strings[0];
    OpenAndDisplayNewScene(Bands,'',true,true,true);
   {$IfDef RecordMenu} WriteLineToDebugFile('Twmdem.Openimagewithmultiplebands1Click out'); {$EndIf}
end;

procedure Twmdem.Openlandcover1Click(Sender: TObject);
begin
   OpenNewDEM('',true,'landcover');
end;

procedure Twmdem.OpenLASpointcloud1Click(Sender: TObject);
begin
   LASdata1Click(Sender);
end;

procedure Twmdem.Openlidarmatchedgrids1Click(Sender: TObject);
begin
   {$IfDef ExMultigrid}
   {$Else}
      OpenLidarMulti;
   {$EndIf}
end;


procedure Twmdem.Openmultigrids1Click(Sender: TObject);
begin
   {$IfDef ExMultigrid}
      OpenMultigridByDirectory;
   {$EndIf}
end;


procedure Twmdem.Atlantis1Click(Sender: TObject);
begin
   Afar1Click(Sender);
   DisplayHTMLTopic('geology_course\oceanic_detachment_faults.htm');
end;


procedure Twmdem.ESRIshapefile1Click(Sender: TObject);
begin
   if GetFileFromDirectory('ESRI shapefile','*.shp',LastDataBase) then ScreenShapeFileDump(LastDataBase);
end;


procedure Twmdem.est1Click(Sender: TObject);
begin
   {$IfDef IncludeFMX3DMesh}
      MeshForm := TMeshForm.Create(Application);
      MeshForm.Show;
   {$EndIf}
end;

procedure Twmdem.ETOPO11Click(Sender: TObject);
begin
   GetETOPO1;
end;

procedure Twmdem.Evapospriaitonversustemperature1Click(Sender: TObject);
begin
   {$IfDef ExMultiGrid}
   {$Else}
      Gridswithmonthlyprecipitationandtemperature1Click(Sender);
   {$EndIf}
end;

procedure Twmdem.EXIFimage1Click(Sender: TObject);
begin
   EXIFmetadata1Click(Sender);
end;

procedure Twmdem.EXIFmetadata1Click(Sender: TObject);
begin
  {$IfDef ExExif}
  {$Else}
     StartExif;
  {$EndIf}
end;


procedure Twmdem.Fullworldimage1Click(Sender: TObject);
var
   fName : PathStr;
   BMP : tMyBitmap;
   WorldFile : tStringList;
   Size : float64;
begin
   fName := MainMapData;
   if GetGraphicsFileName('global world file',fName) then begin
      BMP := PetImage.LoadBitmapFromFile(fName);
      if (2 * BMP.Height = BMP.Width) then begin
          GetDefaultWorldFile(FName);
          Size := 360 / BMP.Width;
          WorldFile := CreateWorldFileStringList(Size,Size,-180,90);
          WorldFile.SaveToFile(FName);
          WorldFile.Free;
      end
      else MessageToContinue('Not world image');
      BMP.Free;
   end;
end;


procedure Twmdem.HypImageSpeedButtonClick(Sender: TObject);
begin
   {$IfDef ExHypImage}
   {$Else}
      OpenHyperspectralImage;
   {$EndIf}
end;


procedure Twmdem.imecores1Click(Sender: TObject);
{$IfDef NoTiming}
begin
{$Else}
var
   //SaveThreads,
   NewDEM : integer;


      procedure DrawMap(mt : tMapType);
      var
         RightImage,LeftImage : PathStr;
         MyBitmap : tMyBitmap;
      begin
          Stopwatch := TStopwatch.StartNew;
          if (mt = mtAnaglyph) then begin
            MyBitmap := Nil;
            DEMGlb[NewDEM].SelectionMap.MapDraw.MapType := mtGrayReflect;
            DEMGlb[NewDEM].SelectionMap.MapDraw.DeleteSingleMapLayer(DEMGlb[NewDEM].SelectionMap.MapDraw.BaseMapFName);
            DEMGlb[NewDEM].SelectionMap.MapDraw.DrawMapOnBMP(MyBitmap);
            DEMGlb[NewDEM].SelectionMap.MapDraw.AnaglyphBitmap(RightImage,LeftImage,MyBitmap);
            DEMGlb[NewDEM].SelectionMap.Image1.Picture.Graphic := MyBitmap;
            MyBitmap.Free;
          end
          else begin
             DEMGlb[NewDEM].SelectionMap.MapDraw.MapType := mt;
             DEMGlb[NewDEM].SelectionMap.DoBaseMapRedraw;
          end;
          WriteLineToDebugFile(RealToString(Stopwatch.Elapsed.TotalSeconds,-12,-4) + '   ' + MapTypeName(mt));
       end;


      procedure DoLots(What : shortString; ThreadsToUse : Integer);
      begin
         WriteLineToDebugFile('Threads=' + ThreadsToUse.ToString);
         MDdef.MaxThreadsForPC := ThreadsToUse;
         DrawMap(mtElevSpectrum);
         DrawMap(mtGrayReflect);
         DrawMap(mtIHSReflect);
         DrawMap(mtSlopeTrafficCats);
         DrawMap(mtDEMAspect);
         DrawMap(mtAnaglyph);
      end;

begin
   SaveBackupDefaults;

   NewDEM := OpenNewDEM('c:\mapdata\test-v4.dem');
   MDDef.MapTicks := tixNone;
   MDDef.DefaultMapXSize := 2048;
   MDDef.DefaultMapYSize := 2048;
   MDDef.FanSaveExt := '.png';
   //SaveThreads := MDdef.MaxThreadsForPC;

   DoLots('Threads',16);
   DoLots('Threads',8);
   DoLots('Threads',4);
   DoLots('Threads',2);
   DoLots('Threads',1);
   CloseSingleDEM(NewDEM);

   RestoreBackupDefaults;
{$EndIf}
end;


procedure Twmdem.Micronetquiz1Click(Sender: TObject);
begin
   {$IfDef ExLabs}
   {$Else}
      MicronetQuiz;
   {$EndIf}
end;


procedure Twmdem.Militaryicongenerator1Click(Sender: TObject);
begin
   {$IfDef ExMilIcon}
   {$Else}
      StopSplashing;
      MilIconsForm := TMilIconsForm.Create(Application);
      MilIconsForm.ShowModal;
   {$EndIf}
end;


function OpenGazFile(fName : PathStr = '') : integer;
begin
   wmdem.VectorMapButtonClick(Nil);
   if (fName <> '') then LastGazFile := fName;
   OpenGazetteer(Result,LastGazFile,VectorMap[LastVectorMap]);
   StopSplashing;
end;


procedure Twmdem.SpeedButton9Click(Sender: TObject);
begin
   OpenGazFile;
end;

procedure Twmdem.LOS2Click(Sender: TObject);
begin
   SimpleProfiles;
   ChangeDEMNowDoing(MultipleLOS);
end;


procedure Twmdem.MultProfSpeedButtonClick(Sender: TObject);
begin
   SimpleProfiles;
   ChangeDEMNowDoing(MultipleLOS);
end;


procedure Twmdem.Differencetwobitmaps1Click(Sender: TObject);
begin
   DifferenceTwoBitmaps;
end;

procedure Twmdem.Discussionforum1Click(Sender: TObject);
begin
   ExecuteFile('http://forums.delphiforums.com/microdem/start/', '', '');
end;


procedure Twmdem.Distancematrix1Click(Sender: TObject);
begin
   Correlationmatrix1Click(Sender);
end;


procedure Twmdem.DragonPlot1Click(Sender: TObject);
begin
  {$IfDef ExDP}
  {$Else}
     StartDragonPlot;
  {$EndIf}
end;


procedure Twmdem.DTED1Click(Sender: TObject);
{$IfDef ExDTED}
begin
{$Else}
var
   fName : PathStr;
   aFile : file;
   DTEDHeader : tStringList;
begin
   fName := LastDEMName;
   if GetFileFromDirectory('DTED','*.DT*',fname) then begin
      InsureFileIsNotReadOnly(fName);
      FileMode := 0;
      assignFile(afile,fName);
      reset(afile,1);
      DTEDHeader := GetNIMADTEDHeader(afile);
      DisplayAndPurgeStringList(DTEDHeader,ExtractFileName(fName));
      CloseFile(aFile);
   end;
{$EndIf}
end;


procedure Twmdem.Duckbeachsurveys1Click(Sender: TObject);
var
   pName : PathStr;
begin
    pName := 'duck_beach';
    DownloadandUnzipDataFileIfNotPresent(pName);
    LastDEMName := MainMapData + pName + '\';
    OpenNewDEM('');
    pName := MainMapData + pName + '\' +'FRF20040719_648_V20050729.dbf';
    if (LastDEMLoaded <> 0) then DEMGlb[LastDEMLoaded].SelectionMap.LoadDataBaseFile(pName);
end;

procedure Twmdem.TernarySpeedButtonClick(Sender: TObject);
{$IfDef ExGeoStats}
begin
{$Else}
var
   ThisGraph : TThisBaseGraph;
   Bitmap : tMyBitmap;
begin
   ThisGraph := TThisBaseGraph.Create(Application);
   ThisGraph.DrawTernaryDiagram(Bitmap);
   ThisGraph.GraphDraw.GraphDrawn := true;
   ThisGraph.GraphDraw.GraphType := gtTernary;
   ThisGraph.Image1.Picture.Graphic := bitmap;
   Bitmap.Free;
   StopSplashing;
{$EndIf}
end;


procedure Twmdem.StratcolSpeedButtonClick(Sender: TObject);
begin
{$IfDef ExGeology}
{$Else}
   ColMainF := TColMainF.Create(Application);
   StopSplashing;
{$EndIf}
end;


procedure Twmdem.Structuralgeologylab1Click(Sender: TObject);
begin
   {$IfDef ExGeology}
   {$Else}
      GeologyPopUpMenu.Popup(Mouse.CursorPos.X,Mouse.CursorPos.Y);
   {$EndIf}
end;

procedure Twmdem.SeaFloorAgeSpeedButtonClick(Sender: TObject);
begin
   {$IfDef ExGeology}
   {$Else}
      if (PredAgesDEM <> 0) and (DEMGlb[PredAgesDEM] <> Nil) and (DEMGlb[PredAgesDEM].DEMFileName = PredAgesFile)then begin
      end
      else begin
         PredAgesDEM := OpenNewDEM(PredAgesFile,false);
         if (PredAgesDEM <> 0) then DEMGlb[PredAgesDEM].SetUpMap(PredAgesDEM,false,mtElevSpectrum,DEMGlb[LastDEMLoaded].AreaName);
      end;
   {$EndIf}
end;


procedure Twmdem.StartSealevelrise(BaseMap : tMapForm);
{$IfDef ExLab}
begin
{$Else}
var
   fName,zname,dName : PathStr;
begin
   fName := MainMapData + 'tides\Univ_hawaii\tide_stations_v2.dbf';
   if not FileExists(fName) then begin
      zName := 'tides.zip';
      dName := MainMapData + zName;
      DownloadFileFromWeb(WebDataDownLoadDir + zName,dName);
      ZipMasterUnzip(dName,MainMapData);
   end;

   if FileExists(fName) then begin
      if (BaseMap = Nil) then begin
         GetETOPO1;
         LastDEMLoaded := OpenNewDEM(ETOPODEMName);
         DEMGlb[LastDEMLoaded].SelectionMap.LoadDataBaseFile(fName);
      end
      else begin
         BaseMap.LoadDataBaseFile(fName);
      end;
   end
{$EndIf}
end;


procedure Twmdem.Sealevelrise1Click(Sender: TObject);
begin
   StartSealevelrise(Nil);
end;

procedure Twmdem.Sedimentationrates1Click(Sender: TObject);
begin
   MDDef.ShowDBonly := true;
   OpenDatabase1Click(Sender);
end;

procedure Twmdem.Sedimentthicknessgrid1Click(Sender: TObject);
begin
   SedThickButtonClick(Sender);
end;

procedure Twmdem.Sedimenttypegrid1Click(Sender: TObject);
begin
   {$IfDef ExGeology}
   {$Else}
   if (SedTypeDEM <> 0) and (DEMGlb[SedTypeDEM] <> Nil) and (DEMGlb[SedTypeDEM].DEMFileName = SedTypeFile) then begin
   end
   else begin
      GeologyGetData;
      if FileExists(SedTypeFile) then begin
         OpenNewDEM(SedTypeFile);
         DEMGlb[LastDEMLoaded].VATLegend;
      end;
   end;
   {$EndIf}
end;


procedure Twmdem.Sedimenttypes1Click(Sender: TObject);
begin
   {$IfDef ExGeology}
   {$Else}
      Sedimenttypegrid1Click(Sender);
   {$EndIf}
end;


procedure Twmdem.SedThickButtonClick(Sender: TObject);
begin
   {$IfDef ExGeology}
   {$Else}
      if not ValidDEM(SedThickDEM) then begin
         GeologyGetData;
         //if not FileExists(SedThickFile) then DownloadFileFromWeb(WebDataDownLoadDir + ExtractFileName(SedThickFile),SedThickFile);
         SedThickDEM := OpenNewDEM(SedThickFile,false);
         if (SedThickDEM <> 0) then DEMGlb[LastDEMLoaded].SetUpMap(LastDEMLoaded,false,mtElevSpectrum,DEMGlb[LastDEMLoaded].AreaName);
      end;
   {$EndIf}
end;


procedure Twmdem.Seismicfencediagram1Click(Sender: TObject);
begin
   {$IfDef ExFMX3D}
   {$Else}
       StartSeismicViewing;
       DisplayHTMLTopic('html\fence_diagram.htm');
   {$EndIf}
end;

procedure Twmdem.Seismicviewing1Click(Sender: TObject);
begin
   {$IfDef ExFMX3D}
   {$Else}
      StartSeismicViewing;
   {$EndIf}
end;

procedure Twmdem.Sensorcoverage1Click(Sender: TObject);
var
   pName : PathStr;
begin
   pName := 'sensor_placement_lab';
   DownloadandUnzipDataFileIfNotPresent(pName);
   LastDEMName := MainMapData + pName + '\p28_ned1.DEM';
   OpenNewDEM(LastDEMName);
   LastDEMName := MainMapData + pName + '\p28_nlcd.DEM';
   OpenNewDEM(LastDEMName);
end;


procedure Twmdem.PredBathySpeedButtonClick(Sender: TObject);
begin
   OpenNewDEM(ETOPODEMName);
   if (LastDEMLoaded <> 0) and MDDef.WorldOutlinesOnGlobalDEM then begin
      Map_Overlays.AddOverlay(DEMGlb[LastDEMLoaded].SelectionMap,ovoWorldOutlines);
      DEMGlb[LastDEMLoaded].SelectionMap.DoFastMapRedraw;
   end;
end;


procedure Twmdem.PlateRotateSpeedButtonClick(Sender: TObject);
begin
   VectorMapButtonClick(Sender);
end;


procedure Twmdem.Platerotations1Click(Sender: TObject);
begin
   PlateRotateSpeedButtonClick(Sender);
end;


procedure Twmdem.Platetectonics1Click(Sender: TObject);
begin
   GetETOPO1;
   PredBathySpeedButtonClick(Nil);
   DEMGlb[LastDEMLoaded].SelectionMap.Globaltectonicsmap1Click(Nil);
end;

procedure Twmdem.NASABlueMarbleSpeedButtonClick(Sender: TObject);
var
  NewSatImage : integer;
begin
   {$IfDef ExSat}
   {$Else}
      GetBlueMarble;
      NewSatImage := OpenAndDisplayNewScene(nil,BlueMarblefName,true,true,false);
      if NewSatImage <> 0 then begin
         SatImage[NewSatImage].SelectionMap.MapSubsetAllowed := false;
         if MDDef.WorldOutlinesOnGlobalBlueMarble then begin
            Map_Overlays.AddOverlay(SatImage[NewSatImage].SelectionMap,ovoWorldOutlines);
            SatImage[NewSatImage].SelectionMap.DoFastMapRedraw;
         end;
         StopSplashing;
     end;
   {$EndIf}
end;


procedure Twmdem.NaturalEarthvectoroutlines1Click(Sender: TObject);
begin
   GetNaturalEarthData(true);
end;


procedure Twmdem.netcdf1Click(Sender: TObject);
begin
    GDAL_netcdf;
end;


procedure Twmdem.Statesforblockgrids1Click(Sender: TObject);
{$IfDef ExComplexGeoStats}
begin
{$Else}
var
   dbfDir,GridDir,dbName,fName : PathStr;
   Results,TheFiles : tStringList;
   aDEM,i,db : integer;
   MomentVar : tMomentVar;
begin
   {$IfDef RecordGeostats} WriteLineToDebugFile('TPickGeoStat.Statesforblockgrids1Click in'); {$EndIf}
   GetAtlasDirectories(dbfDir,GridDir);

   Results := tStringList.Create;
   Results.Add('PARAM,MEAN,STD_DEV,MIN,MAX,MEDIAN,PERC_5,PERC_10,QUANT_25,QUANT_75,PERC_90,PERC_95,SKEWNESS,KURTOSIS');

   db := OpenDataBase('training set points');
   dbName := GISdb[db].dbFullName;
   CloseAndNilNumberedDB(db);

   TheFiles := Nil;
   if Sender = Addnormaliziedstatsforblockgridstotrainingset1 then GridDir := GridDir + 'NORMALIZED\';

   Petmar.FindMatchingFiles(GridDir,'*.dem',TheFiles,0);
   StartThreadTimers('Stat',1);
   for i := 0 to pred(TheFiles.Count) do begin
      ThreadTimers.OverallGauge9.Progress := round(100 * i/TheFiles.Count);
      fName := TheFiles.Strings[i];
      {$IfDef RecordGeostats}     WriteLineToDebugFile(fName,true);   {$EndIf}
      aDEM := OpenNewDEM(fName);
      DEMGlb[aDEM].SelectionMap.OpenDBonMap('',dbname);
      {$IfDef RecordGeostats}   WriteLineToDebugFile(DEMGlb[aDEM].AreaName);   {$EndIf}
      GISdb[db].AddAndFillFieldFromDEM(adElevNearest,ptTrim(DEMGlb[aDEM].AreaName));
      MomentVar := DEMGlb[aDEM].ElevationMoments(DEMGlb[aDEM].FullDEMGridLimits);
      Results.Add(ptTrim(DEMGlb[aDEM].AreaName) + ',' +
                  RealToString(MomentVar.mean,-12,-4) + ',' + RealToString(MomentVar.sdev,-12,-4) + ',' +
                  RealToString(MomentVar.MinZ,-12,-4) + ',' + RealToString(MomentVar.MaxZ,-12,-2) + ',' +
                  RealToString(DEMGlb[aDEM].FindPercentileElevation(50),-12,-4) + ',' +
                  RealToString(DEMGlb[aDEM].FindPercentileElevation(5),-12,-4) + ',' +
                  RealToString(DEMGlb[aDEM].FindPercentileElevation(10),-12,-4) + ',' +
                  RealToString(DEMGlb[aDEM].FindPercentileElevation(25),-12,-4) + ',' +
                  RealToString(DEMGlb[aDEM].FindPercentileElevation(75),-12,-4) + ',' +
                  RealToString(DEMGlb[aDEM].FindPercentileElevation(90),-12,-4) + ',' +
                  RealToString(DEMGlb[aDEM].FindPercentileElevation(95),-12,-4) + ',' +
                  RealToString(MomentVar.Skew,-12,-4) + ',' + RealToString(MomentVar.Curt,-12,-4)  );
      CloseSingleDEM(aDEM);
   end;
   TheFiles.Free;
   EndThreadTimers;
   Results.SaveToFile(GridDir + 'param_stats.csv');
{$EndIf}
end;


procedure Twmdem.Statsfortrainingset1Click(Sender: TObject);
begin
   {$IfDef ExComplexGeoStats}
   {$Else}
      StatsForTrainingSets;
   {$EndIf}
end;


procedure Twmdem.UKOSgrid1Click(Sender: TObject);
begin
   FinnishGaussKruger1Click(Sender);
end;


procedure Twmdem.UKOSgrid2Click(Sender: TObject);
begin
   NLCD20011Click(Sender);
end;

procedure Twmdem.FinnishGaussKruger1Click(Sender: TObject);
{$IfDef ExConvert}
begin
{$Else}
var
   ConvertForm : TUKOSConvertForm;
begin
   ConvertForm := TUKOSConvertForm.Create(Application);
   if (Sender = UKOSgrid1) then begin
      ConvertForm.This_projection.PName := UK_OS;
      ConvertForm.Caption := 'United Kingdon Ordnance Survey converter';
   end;
   if (Sender = Guam1) then begin
      ConvertForm.This_projection.PName := AzimuthalEquidistantEllipsoidal;
      ConvertForm.Caption := 'Guam converter';
      ConvertForm.Edit1.Text := '37712.48';  //x
      ConvertForm.Edit2.Text := '35242';  //y
      ConvertForm.Edit3.Text := '13.339038461';  //lat
      ConvertForm.Edit4.Text := '144.635331292';  //long

      ConvertForm.This_projection.Lat0 := 13.472466353 * Petmar_types.DegToRad;
      ConvertForm.This_projection.Long0 := 144.748750706 * Petmar_types.DegToRad;
      ConvertForm.This_projection.false_east := 50000;
      ConvertForm.This_projection.false_north := 50000;
      ConvertForm.This_projection.a := 6378206.4;
      ConvertForm.This_projection.e2 := 0.00676866
   end;

   if (Sender = FinnishGaussKruger1) then begin
      ConvertForm.This_projection.PName := Finn_GK;
      ConvertForm.Caption := 'KKJ (Finnish National Coordinate System) Zone 3';
   end;

   if (Sender = WKTprojection1) then begin
      Petmar.GetExistingFileName('wkt projection','*.prj;*.wkt',MDDef.WKTLidarProj);
      ConvertForm.This_projection.InitializeProjectionFromWKT(MDDef.WKTLidarProj);
      ConvertForm.Caption := ExtractFileNameNoExt(MDDef.WKTLidarProj);
   end;

   if (Sender = PolarStereographic1) then begin
      ConvertForm.Caption := 'Polar Stereographic Converter';
      ConvertForm.This_projection.PName := PolarStereographicEllipsoidal;
      SetUpDefaultNewProjection(ConvertForm.This_projection);
      ConvertForm.Edit3.Text := '58.23746302';  //lat
      ConvertForm.Edit4.Text := '8.0835866';  //long
   end;
   if (Sender = PolarStereographic1) or (Sender = WKTprojection1) then begin
      ConvertForm.Parameters.TabVisible := false;
   end;
   ConvertForm.This_projection.GetProjectParameters;
   PointConvertDebug := true;
   ConvertForm.ShowParams;
   ConvertForm.ShowModal;
{$EndIf}
end;


procedure Twmdem.Conicconversions1Click(Sender: TObject);
begin
   FinnishGaussKruger1Click(Sender);
end;

procedure Twmdem.Conicprojections1Click(Sender: TObject);
begin
   PickProjections(3);
end;

procedure Twmdem.Polarstereographic1Click(Sender: TObject);
begin
   FinnishGaussKruger1Click(Sender);
end;


procedure Twmdem.Postprocesscontest1Click(Sender: TObject);
begin
   {$IfDef Include2021datafusion}  CombinePredictions; {$EndIf}
end;

procedure Twmdem.UTM1Click(Sender: TObject);
begin
    NLCD20011Click(Sender);
end;

procedure Twmdem.UTMordatumconversion1Click(Sender: TObject);
begin
   {$IfDef ExConvert}
   {$Else}
      DEMCnvrt.ConvertCoordinates;
   {$EndIf}
end;

procedure Twmdem.UTMprojectoiin1Click(Sender: TObject);
begin
   PickProjections(4);
end;

procedure Twmdem.UTMspacingandgridtrueangle1Click(Sender: TObject);
begin
   MakeUTMTable;
end;

procedure Twmdem.VRML1Click(Sender: TObject);
begin
   PickForNotePadPlusPlus(3)
end;


procedure Twmdem.TINSpeedButtonClick(Sender: TObject);
begin
   {$IfDef ExTIN}
   {$Else}
      StopSplashing;
      tTIN.Create(nil,'',false);
   {$EndIf}
end;


procedure Twmdem.ToolBar1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   if (Button = mbRight) then begin
      DisplayHTMLTopic('html\maintool.htm');
   end;
end;


procedure Twmdem.Tools1Click(Sender: TObject);
begin
   StopSplashing;
   {$IfDef ExMovie}
      LoadImage1.Visible:= false;
      Moviereplay1.Visible:= false;
   {$EndIf}
   {$IfDef ExMilIcon}
      Militaryicongenerator1.Visible:= false;
   {$EndIf}

   Militaryicongenerator1.Visible := MDDef.ProgramOption = ExpertProgram;
   Webpagethumbnails1.Visible := MDDef.ProgramOption = ExpertProgram;
   EXIFmetadata1.Visible := MDDef.ProgramOption = ExpertProgram;
   Spectrallibrary1.Visible := MDDef.ProgramOption = ExpertProgram;
   Identifydirectory1.Visible := MDDef.ProgramOption = ExpertProgram;
   Findmatchingfiles1.Visible := MDDef.ProgramOption = ExpertProgram;
   Copyfile1.Visible := MDDef.ProgramOption = ExpertProgram;
   ConvartDBFs1.Visible := MDDef.ProgramOption = ExpertProgram;
   StereoPair1.Visible := MDDef.ProgramOption = ExpertProgram;
   SlideSorter1.Visible := MDDef.ProgramOption = ExpertProgram;
   Experimental1.Visible := MDDef.ProgramOption = ExpertProgram;
   FileOperations1.Visible := MDDef.ProgramOption = ExpertProgram;
   ShowFirstLinesTextFile1.Visible := MDDef.ProgramOption = ExpertProgram;
   Showfirstbytesofbinaryfile1.Visible := MDDef.ProgramOption = ExpertProgram;
   Unicodeicongenerator1.Visible := MDDef.ProgramOption = ExpertProgram;
   Zipatoneeditor1.Visible := MDDef.ProgramOption = ExpertProgram;
   Fileoperations1.Visible := MDDef.ProgramOption = ExpertProgram;
   //Sentinel2dataprep1.Visible := MDDef.ProgramOption = ExpertProgram;
   //OpenTCPinterface1.Visible := MDDef.ProgramOption = ExpertProgram;
   BackupprogramEXE1.Visible := MDDef.ProgramOption in [ExpertProgram];
   ToolsPopupMenu3.PopUp(Mouse.CursorPos.X,Mouse.CursorPos.Y);
end;


procedure Twmdem.Ternarydiagram1Click(Sender: TObject);
begin
   TernarySpeedButtonClick(Sender);
end;

procedure Twmdem.Stratcol2Click(Sender: TObject);
begin
   StratcolSpeedButtonClick(Sender);
end;


procedure Twmdem.Planarprojections1Click(Sender: TObject);
begin
   PickProjections(1);
end;

procedure Twmdem.Platereconstructions1Click(Sender: TObject);
begin
   VectorMapButtonClick(PlateRotateSpeedButton);
end;


procedure Twmdem.Newpanorama1Click(Sender: TObject);
begin
   ChangeDEMNowDoing(SeekingFirstNewPanorama);
end;


procedure Twmdem.PanoramaSpeedButtonClick(Sender: TObject);
begin
   ChangeDEMNowDoing(SeekingFirstNewPanorama);
end;


procedure Twmdem.Maxwellplanning1Click(Sender: TObject);
var
   dName,pName : PathStr;
begin
   dName := 'ches_bay_sed';
   DownloadandUnzipDataFileIfNotPresent(dName);
   pName := MainMapData + dName + '\ches_bay_noaa_est_bathy.dem';
   LastDEMLoaded := OpenNewDEM(pName);
   YPMaps('12263');
   YPMaps('12282');
   YPMaps('12283');
end;


procedure Twmdem.FormDestroy(Sender: TObject);
begin
   OnResize := nil;
end;


procedure Twmdem.FormKeyDown(Sender: TObject; var Key: Word;  Shift: TShiftState);
begin
   if (Key =  VK_F1) then begin
      {$IfDef RecordProblems} writeLineToDebugFile(' Twmdem.FormKeyDown, Key =  VK_F1'); {$EndIf}
      if AnswerIsYes('Reset defaults, restart') then begin
         ProcessIniFile(iniInit);
         FirstRun := true;
         FormActivate(Sender);
      end;
   end;

end;

procedure Twmdem.Webpagethumbnails1Click(Sender: TObject);
begin
   {$IfDef ExTools}
   {$Else}
      Get_Thumbnails.AlbumInventory;
   {$EndIf}
end;


procedure Twmdem.WhiteboxGeotiff1Click(Sender: TObject);
begin
   GeotiffMetadata(mdWhitebox,ExtractFilePath(LastImageName));
end;

procedure Twmdem.WKTprojection1Click(Sender: TObject);
begin
   FinnishGaussKruger1Click(Sender);
end;

procedure Twmdem.XML1Click(Sender: TObject);
begin
   PickForNotePadPlusPlus(2);
end;

procedure Twmdem.XTFindex1Click(Sender: TObject);
{$IfDef ExSidescan}
begin
{$Else}
var
   //sideimage : Tsideimage;
   Infiles : tstringList;
   i,DefaultFilter : byte;
   GISNum,GISNum2 : integer;
begin
    StopSplashing;
    Infiles := tstringList.Create;
    Infiles.Add(InputSideScanFileName);
    DefaultFilter := 1;
    Petmar.GetMultipleFiles('sidescan data','XTF files|*.xtf',Infiles,DefaultFilter);
    if (Infiles.Count > 0) then begin
        for i := 0 to pred(Infiles.Count) do begin
           SetPanelText(0,IntToStr(i) + '/' + IntToStr(Infiles.Count));
           InputSideScanFileName := Infiles.Strings[i];
           if not FileExists(ChangeFileExt(InputSideScanFileName,DefaultDBExt)) then begin
              //SideImage :=
              TSideImage.Create(Application);
           end;
           InputSideScanFileName := ChangeFileExt(Infiles.Strings[i],DefaultDBExt);
           if FileExists(InputSideScanFileName) and OpenNumberedGISDataBase(GISNum,InputSideScanFileName,true) then begin
               InputSideScanFileName := ChangeFileExt(InputSideScanFileName,'_line' + DefaultDBExt);
               if not FileExists(InputSideScanFileName) then MakeLinesFromPoints(GISdb[GISNum],InputSideScanFileName,3,100);
               if OpenNumberedGISDataBase(GISNum2,InputSideScanFileName,true) then begin
                   GISdb[GISNum2].dbtablef.KML1Click(Nil);
                   CloseAndNilNumberedDB(GISNum2);
               end;
               CloseAndNilNumberedDB(GISNum);
           end;
        end;
        SetPanelText(0,'');
    end;
    Infiles.Free;
{$EndIf}
end;


procedure Twmdem.XTFsidescan1Click(Sender: TObject);
begin
   {$IfDef ExSidescan}
   {$Else}
      XTFFileInfo;
   {$EndIf}
end;

procedure Twmdem.XYZshapefile1Click(Sender: TObject);
begin
   NLCD20011Click(Sender);
end;

procedure Twmdem.Zeorlog1Click(Sender: TObject);
begin
   ClearDebugLog;
end;

procedure Twmdem.Zipatoneeditor1Click(Sender: TObject);
begin
   {$IfDef ExZipatone}
   {$Else}
      MapPatternEditor;
   {$EndIf}
end;

procedure Twmdem.TigerSpeedButtonClick(Sender: TObject);
begin
   LoadBlankVectorMapAndOverlay(true,false);
end;


procedure Twmdem.Solarpositiln1Click(Sender: TObject);
begin
   StopSplashing;
   SolorPosForm1 := TSolorPosForm1.Create(Application);
   SolorPosForm1.ShowModal;
end;

procedure Twmdem.Solstice1Click(Sender: TObject);
begin
   SunViews(1);
end;

procedure Twmdem.Southpolarstereographic1Click(Sender: TObject);
begin
   NLCD20011Click(Sender);
end;

procedure Twmdem.BackupprogramEXE1Click(Sender: TObject);
begin
   BackupProgramEXE;
end;

procedure Twmdem.Batchchangepartoffilenames1Click(Sender: TObject);
var
   fName,NewName,BaseName : PathStr;
   i : integer;
   NameContains,ChangeTo : shortString;
   FilesWanted : TStringList;
   DefaultFilter : byte;
begin
   DefaultFilter := 1;
   NameContains := 'DSC_';
   ChangeTo := 'NEW_';
   BaseName := '';

   repeat
      FilesWanted := tStringList.Create;
      FilesWanted.Add(BaseName);
      if GetMultipleFiles('Change file name','AnyFile|*.*',FilesWanted, DefaultFilter) then begin
         Petmar.GetString('phrase to replace',NameContains,false,ValidDosFileNameChars);
         Petmar.GetString('replacement phrase',ChangeTo,false,ValidDosFileNameChars);
         ShowHourglassCursor;
         BaseName := ExtractFilePath(FilesWanted.Strings[0]);
         for i := 0 to pred(FilesWanted.Count) do begin
            fName := FilesWanted.Strings[i];
            NewName := BaseName + StringReplace(ExtractFileNameNoExt(fName),NameContains,ChangeTo,[rfReplaceAll, rfIgnoreCase]) + ExtractFileExt(fName);
            RenameFile(fName,NewName);
         end;
      end;
      FilesWanted.Free;
      ShowDefaultCursor;
   until not AnswerIsYes('More files');
end;

procedure Twmdem.Bathymetrygrid1Click(Sender: TObject);
begin
   YPMaps('Bathy');
end;


procedure Twmdem.Opengl1tofrontClick(Sender: TObject);
begin
   {$IfDef ExFMX3D}
   {$Else}
      if Map3D <> Nil then begin
         Map3D.BringToFront;
         Map3D.Left := Self.Left + 10;
         Map3D.Top := Self.Top + 10;
      end;
      if View3DForm <> Nil then begin
         View3DForm.BringToFront;
         View3DForm.Left := Self.Left + 10;
         View3DForm.Top := Self.Top + 10;
      end;
   {$EndIf}
end;

procedure WriteQuickFile(fName : PathStr; OneLiner : string);
var
   Test : tStringList;
begin
   Test := tStringList.Create;
   Test.Add(OneLiner);
   Test.SaveToFile(fName);
   Test.Free;
end;


procedure Twmdem.GISdatasampler1Click(Sender: TObject);
{$IfDef ExLabs}
begin
{$Else}
var
   pName : PathStr;
begin
    pName := 'annapolis_data';
    DownloadandUnzipDataFileIfNotPresent(pName);
    pName := MainMapData + pname + '\';
    LastDEMName := pName + 'dems\aacounty_3dep_third_sec.tif';
    LastScanMapName := pName + 'UStopo_geopdf\annapolis_8152555.tif';
    LastGazFile := pName + 'usgs_gazetteer\MD_FEATURES_20210825.dbf';
    LastDataBase := pName + 'tiger_edges\tl_2021_24003_edges.shp';
    LastLidarDirectory := pName + 'las_2017_Anne_Arundel';
    if (OpenNewDEM(LastDEMName) <> 0) then begin
       {$IfDef RecordLabs} writeLineToDebugFile('Twmdem.GISdatasampler1Click load ' + LastDataBase); {$EndIf}
       DEMGlb[LastDEMLoaded].SelectionMap.LoadDataBaseFile(LastDataBase);
    end;
    MessageToContinue('Annapolis data present; default choices for data set');
{$EndIf}
end;


procedure Twmdem.GPS1Click(Sender: TObject);
begin
   //GPS_sensor.StartGPS;
end;

procedure Twmdem.Verticalearthcurvature1Click(Sender: TObject);
begin
   {$IfDef ExComplexGeoStats}
   {$Else}
      ShowVerticalEarthCurvature;
   {$EndIf}
end;



procedure Twmdem.Viewdebuglog1Click(Sender: TObject);
begin
   {$IfDef RecordProblems}
      StopSplashing;
      if (TheDebugLog <> Nil) then ShowInNotepadPlusPlus(DebugFilename,'Debug log (' + IntToStr(TheDebugLog.Count) + ' lines)')
   {$EndIf}
end;

procedure Twmdem.Horizontalearthcurvature1Click(Sender: TObject);
begin
   {$IfDef ExComplexGeoStats}
   {$Else}
      ShowHorizontalEarthCurvature;
   {$EndIf}
end;


procedure Twmdem.Horizontalimageslider1Click(Sender: TObject);
var
   fName1,fName2 : PathStr;
begin
   if GetFileFromDirectory('Left image',AllowedGraphicsFilters,fName1) and GetFileFromDirectory('Right image',AllowedGraphicsFilters,fName2) then begin
      ImageSplitHorizontal(fName1,fName2);
   end;
end;

procedure Twmdem.SunViews(Which : integer);
begin
   {$IfDef ExCartography}
   {$Else}
      MDDef.MapTicks := tixLatLong;
      if which in [0,1] then NewOrtographicMapForSeasonalTilt('June Solstice',23.5);
      if which in [0,2] then NewOrtographicMapForSeasonalTilt('December Solstice',-23.5);
      if which in [0,3] then NewOrtographicMapForSeasonalTilt('March or September Equinox',0);
      DisplayHTMLTopic('microdemtutorials/Phys_geography/tilt_seasons.htm');
   {$EndIf}
end;

procedure Twmdem.hreeviews1Click(Sender: TObject);
begin
   SunViews(0);
end;

procedure Twmdem.Hurricanes1Click(Sender: TObject);
begin
   VectorMapButtonClick(Nil);
   VectorMap[LastVectorMap].Hurricanes1Click(Sender);
end;

procedure Twmdem.Hyperspectralimagery1Click(Sender: TObject);
begin
   {$IfDef ExHypImage}
   {$Else}
      OpenHyperspectralImage;
   {$EndIf}
end;


procedure Twmdem.OpenTIGERcountrymap1Click(Sender: TObject);
begin
   TigerSpeedButtonClick(Sender);
end;


procedure Twmdem.OpenTIGERcountymap1Click(Sender: TObject);
begin
   TigerSpeedButtonClick(Sender);
end;


procedure Twmdem.OpenUScountiesmap1Click(Sender: TObject);
begin
   DEMMapf.LoadBlankVectorMapAndOverlay(false,false,CountyGISFileName);
end;

procedure Twmdem.OpenUSstatesmap1Click(Sender: TObject);
begin
   DEMMapf.LoadBlankVectorMapAndOverlay(false,false,StateGISFileName);
end;

procedure Twmdem.OpenVectormap1Click(Sender: TObject);
begin
   VectorMapButtonClick(Sender);
end;

procedure Twmdem.Openpointclouds1Click(Sender: TObject);
begin
   Other3Dpointclouds1Click(Sender);
end;

procedure Twmdem.Openproject1Click(Sender: TObject);
begin
   RestoreMicrodemDesktop;
end;


procedure Twmdem.Openrecyclebin1Click(Sender: TObject);
begin
   OpenRecycleBin;
end;

procedure Twmdem.OpenScannedmap1Click(Sender: TObject);
begin
   {$IfDef RecordMenu} writeLineToDebugFile('Twmdem.OpenScannedmap1Click (from Open menu)'); {$EndIf}
   PickAndOpenImagery(itDRG);
end;


procedure Twmdem.OpenSentinen2image1Click(Sender: TObject);
begin
   PickSatDirToOpen;
end;


procedure Twmdem.CTD1Click(Sender: TObject);
var
   FilesWanted : TStringList;
   DefaultFilter : byte;
   i : integer;
   fName : PathStr;
   InMessage : AnsiString;
   NewGraph  : TThisBaseGraph;
   Table : tMyData;
begin
   DefaultFilter := 1;
   FilesWanted := tStringList.Create;
   FilesWanted.Add('c:\yp686\');
   if (Sender = CTD1) then InMessage := 'CTD data'
   else InMessage := 'Light data';
   if Petmar.GetMultipleFiles(InMessage,DBNameMask,FilesWanted,DefaultFilter) then begin
      NewGraph := TThisBaseGraph.Create(Application);
      with NewGraph,GraphDraw do begin
         DBFYFieldName := 'DEPTH';
         if (Sender = CTD1) then begin
            fName := FilesWanted.Strings[0];
            Table := tMyData.Create(fName);
            if Table.FieldExists('TEMP') then DBFXFieldName := 'TEMP'
            else if Table.FieldExists('TEMP_C') then DBFXFieldName := 'TEMP_C';
            Table.Destroy;
            MaxHorizAxis := 30;
            Caption := 'CTD Cast';
         end
         else begin
            DBFXFieldName := 'UWPAR';
            MaxHorizAxis := 3000;
            MinHorizAxis := 0.1;
            Caption := 'PNF300 Cast';
            HorizAxisFunctionType := Log10Axis;
            HorizAxisFunct := Log10;
         end;
         MaxVertAxis := 10;
         HorizLabel := DBFXFieldName;
         VertLabel := 'Depth (m)';
         NormalCartesianY := false;
         SetUpGraphForm;
         for i := 1 to FilesWanted.Count do GraphDraw.DBFLineFilesPlotted.Add(FilesWanted.Strings[pred(i)]);
         RedrawDiagram11Click(Nil);
      end;
      StopSplashing;
   end;
   FilesWanted.Free;
end;


procedure Twmdem.Cylindricalprojections1Click(Sender: TObject);
begin
   PickProjections(2);
end;

procedure Twmdem.LandCoverSpeedButtonClick(Sender: TObject);
begin
   OpenNewDEM('',true,'landcover');
end;


procedure Twmdem.Landsatbrowseindex1Click(Sender: TObject);
begin
   {$IfDef ExComplexGeoStats}
   {$Else}
      CreateLandsatIndex(true);
   {$EndIf}
end;


procedure Twmdem.Landsatfullsceneindex1Click(Sender: TObject);
begin
   {$IfDef ExComplexGeoStats}
   {$Else}
      CreateLandsatIndex(false);
   {$EndIf}
end;

procedure Twmdem.Landsatimage1Click(Sender: TObject);
begin
   AnnapolisTM8scene1Click(Sender);
end;

procedure Twmdem.Landsattimeseries1Click(Sender: TObject);
var
   pName : PathStr;
begin
   {$IfDef RecordLabs} WriteLineToDebugFile('Landsattimeseries1Click in'); {$EndIf}
   pName := 'landsat_time_series';
   DownloadandUnzipDataFileIfNotPresent(pName);
   LastImageName := MainMapData + pname + '\';
   OpenAndDisplayNewScene(Nil,LastImageName,true,true,true);
   {$IfDef RecordLabs} WriteLineToDebugFile('Landsattimeseries1Click out'); {$EndIf}
end;

procedure Twmdem.LASdata1Click(Sender: TObject);
begin
   Point_cloud_options.OvelayPointClouds(Nil);
   StopSplashing;
end;

procedure Twmdem.LASfile1Click(Sender: TObject);
{$IfDef ExLASlidar}
begin
{$Else}
var
   LasData : Las_Lidar.tLAS_data;
   fName : PathStr;
   Results : tStringList;
begin
   fname := LastLidarDirectory;
   if GetFileFromDirectory('LAS file','*.LAS',fName) then begin
      LasData := Las_Lidar.tLAS_data.Create(fName);
      Results := LASdata.GetMetadata;
      fName := MDTempDir + ExtractFileNameNoExt(fName)  + '_metatdata.txt';
      Results.SaveToFile(fName);
      Results.Destroy;
      ShowInNotepadPlusPlus(fName);
      LasData.Destroy;
   end;
{$EndIf}
end;


procedure Twmdem.lasinfo1Click(Sender: TObject);
begin
   {$IfDef ExPointCloud}
   {$Else}
      CallLasInfo;
   {$EndIf}
end;

procedure Twmdem.LASlidarpointcloudsamples1Click(Sender: TObject);
{$IfDef ExPointCloud}
begin
{$Else}
var
   pName : PathStr;
begin
   pName := 'lidar_sampler';
   DownloadandUnzipDataFileIfNotPresent(pName);
   pName := MainMapData + pname + '\';
   LastLidarDirectory := pName + 'elsinore_denmark';

   //pName := 'lidar_sampler';
   //DownloadandUnzipDataFileIfNotPresent(pName);
   LastLidar2Directory := LastLidarDirectory;
   LastLidar3Directory := LastLidarDirectory;
   LastLidar4Directory := LastLidarDirectory;
   LastLidar5Directory := LastLidarDirectory;
   MDdef.AutoZoomOpenLAS := true;
   OvelayPointClouds(Nil,LastLidarDirectory);
   //VectorMapButtonClick(LASdata1);
{$EndIf}
end;

procedure Twmdem.LatLong1Click(Sender: TObject);
begin
   NLCD20011Click(Sender);
end;

procedure Twmdem.LatlongofPLSSlocation1Click(Sender: TObject);
var
   PLSSString : shortstring;
   Lat,Long : float64;
begin
   Get_PLSS.GetPLSSLocation(PLSSString,Lat,Long,Nil);
end;

procedure Twmdem.Leastcostpath1Click(Sender: TObject);
begin
   LeastCostPathOptions;
end;

procedure Twmdem.Legislativeredistricting1Click(Sender: TObject);
begin
   {$IfDef ExRedistrict}
   {$Else}
      Annapolisredistricting1Click(Sender);
   {$EndIf}
end;

procedure Twmdem.Lidarandbeacherosion1Click(Sender: TObject);
begin
   Duckbeachsurveys1Click(Sender);
end;


procedure Twmdem.LidarandglobalDEMs1Click(Sender: TObject);
begin
   BatchGlobalDEMs;
end;



procedure Twmdem.Lightdata1Click(Sender: TObject);
begin
   CTD1Click(Sender);
end;

procedure Twmdem.listgeo1Click(Sender: TObject);
begin
   GeotiffMetadata(mdListGeo,ExtractFilePath(LastImageName));
end;

procedure Twmdem.FormResize(Sender: TObject);
begin
   {$IfDef RecordFormResize} WriteLineToDebugFile('Twmdem.FormResize, ' + IntToStr(Width) + ' x ' + IntToStr(Height)); {$EndIf}
   inherited;
   if not FirstRun then SetMenusForVersion;
   {$IfDef StatusBarFormResize}
      if (not LockStatusBar) then SetPanelText(3, 'Left=' + IntToStr(Left) + ' Top=' + IntToStr(Top));
   {$EndIf}
end;


procedure Twmdem.StatusBar1Click(Sender: TObject);
begin
   {$IfDef ExSetOptions}
   {$Else}
      ChangeOptions;
      if AnswerIsYes('Restart initialize program') then begin
         FirstRun := true;
         FormActivate(Sender);
      end;
   {$EndIf}
end;

procedure Twmdem.StatusBar1DblClick(Sender: TObject);
begin
   StatusBar1Click(Sender);
end;


procedure Twmdem.Open1Click(Sender: TObject);
begin
   StopSplashing;
   {$IfDef ExDRGimport}
      OpenUSTopoGeoPDF1.Visible := false;
   {$Else}
      OpenUSTopoGeoPDF1.Visible := (MDDef.ProgramOption = ExpertProgram);
   {$EndIf}

   Openmultigrids1.visible := (MDDef.ProgramOption = ExpertProgram);
   OpenProject1.Visible := (MDDef.ProgramOption in [ExpertProgram,RemoteSensingProgram]);
   AddProject1.Visible := (MDDef.ProgramOption in [ExpertProgram,RemoteSensingProgram]);

   OpenandmergeDEMdirectories1.Visible := (MDDef.ProgramOption = ExpertProgram);
   OpenDEMwithoutmap1.Visible := (MDDef.ProgramOption = ExpertProgram);
   OpenScannedmap1.Visible := MDDef.ShowOpenImagery;

   Openlandcover1.Visible := (MDDef.ProgramOption in [ExpertProgram,RemoteSensingProgram]);
   OpenUScountiesmap1.Visible := (CountyGISFileName <> '') and VectorMapButton.Visible;
   OpenUSstatesmap1.Visible := (StateGISFileName <> '') and VectorMapButton.Visible;
   OpenPointClouds1.Visible := MDdef.ShowPointClouds;
   OpenandmergeDEMs1.Visible := (MDDef.ProgramOption in [ExpertProgram,GeographyProgram]);
   OpenandmergeDEMdirectories1.Visible := (MDDef.ProgramOption = ExpertProgram);
   OpenDEMwithoutmap1.Visible := (MDDef.ProgramOption = ExpertProgram);
   OpenSentinen2image1.Visible := (MDDef.ProgramOption in [ExpertProgram,RemoteSensingProgram]);
   OpenScannedmap1.Visible := (MDDef.ProgramOption in [ExpertProgram,RemoteSensingProgram]);
   OpenandmergeGeotiffs1.Visible := (MDDef.ProgramOption in [ExpertProgram,RemoteSensingProgram]);
   Openshapefilemap1.Visible := (MDDef.ProgramOption in [ExpertProgram,RemoteSensingProgram]);
   OpenLASpointcloud1.Visible := (MDDef.ProgramOption in [ExpertProgram,RemoteSensingProgram]);
   Openlidarmatchedgrids1.Visible := (MDDef.ProgramOption in [ExpertProgram,RemoteSensingProgram]);
   Openmultigridspickfiles1.Visible := (MDDef.ProgramOption in [ExpertProgram,RemoteSensingProgram]);
   Openimagewithmultiplebands1.Visible := (MDDef.ProgramOption in [ExpertProgram,RemoteSensingProgram]);

   OpenPopUpMenu1.Popup(Mouse.CursorPos.X,Mouse.CursorPos.Y);
end;

procedure Twmdem.OpenandmergeDEMdirectories1Click(Sender: TObject);
var
   DEMList,NewFiles : tStringList;
   i,WantedDEM : integer;
   aPath : PathStr;
begin
   {$IfDef RecordMenu} WriteLineToDebugFile('OpenandmergeDEMdirectories1 in'); {$EndIf}
   DEMList := tStringList.Create;
   aPath := ExtractFilePath(LastDEMname);
   repeat
      GetDOSPath('Directory with DEMs to merge',aPath);
      if (aPath <> '') then begin
         NewFiles := Nil;
         FindMatchingFiles(aPath,'*.*',NewFiles,1);
         for i := 0 to pred(NewFiles.Count) do begin
            DEMList.Add(NewFiles.Strings[i]);
         end;
         {$IfDef RecordMenu} WriteLineToDebugFile('   add ' + IntToStr(NewFiles.Count) + ' from ' + aPath); {$EndIf}
         NewFiles.Destroy;
      end;
   until aPath = '';
   if (DEMList.Count > 0) then begin
      MergeMultipleDEMsHere(WantedDEM,DEMList,true);
   end;
end;

procedure Twmdem.OpenandmergeDEMs1Click(Sender: TObject);
var
   DEMList : tStringList;
   WantedDEM : integer;
begin
   {$IfDef RecordMenu} WriteLineToDebugFile('Selected DEM Merge'); {$EndIf}
   DEMList := tStringList.Create;
   DEMList.Add(LastDEMName);
   if Petmar.GetMultipleFiles('DEMs to merge',DEMFilterMasks,DEMList,MDDef.DefaultDEMFilter) then begin
      {$IfDef RecordMenu} WriteStringListToDebugFile(DEMList); {$EndIf}
      if (DEMList.Count = 1) then begin
         OpenNewDEM(DEMList.Strings[0]);
         DEMList.Destroy;
      end
      else begin
         MergeMultipleDEMsHere(WantedDEM,DEMList,true);
         StopSplashing;
         SetMenusForVersion;
      end;
   end;
end;

procedure Twmdem.OpenandmergeGeotiffs1Click(Sender: TObject);
{$IfDef ExGDAL}
begin
{$Else}
var
   MergefName : PathStr;
   OutNames : tStringList;
begin
   MergeFName := '';
   OutNames := Nil;
   CallGDALMerge(MergefName,OutNames);
   OpenAndDisplayNewScene(Nil,MergefName,true,true,true);
{$EndIf}
end;

procedure Twmdem.Openclimatemonthlymultigrids1Click(Sender: TObject);
begin
   {$IfDef ExMultigrid}
   {$Else}
       OpenMonthlyMultiGrids;
   {$EndIf}
end;


procedure Twmdem.OpenDatabase1Click(Sender: TObject);
begin
   {$IfDef RecordMenu} WriteLineToDebugFile('Twmdem.OpenDatabase1Click in'); {$EndIf}
   StopSplashing;
   OpenDataBase('');
   {$IfDef RecordMenu} WriteLineToDebugFile('Twmdem.OpenDatabase1Click out'); {$EndIf}
end;

procedure Twmdem.OpenDatabasewithoutmap1Click(Sender: TObject);
begin
   OpenDatabase1Click(Sender);
end;


procedure Twmdem.CompressDecompress1Click(Sender: TObject);
begin
   {$IfDef ExTools}
   {$Else}
      StopSplashing;
      Compress_form.StartCompression;
   {$EndIf}
end;

procedure Twmdem.Computations1Click(Sender: TObject);
begin
   {$IfDef ExConvert}
   {$Else}
      StopSplashing;
      Computations.DoComputations;
   {$EndIf}
end;

procedure Twmdem.Openshapefilemap1Click(Sender: TObject);
begin
   {$IfDef RecordOpenVectorMap} writeLineToDebugFile('Twmdem.Openshapefilemap1 in'); {$EndIf}
   LoadBlankVectorMapAndOverlay(false,false);
end;

procedure Twmdem.OpensingleLandsatband1Click(Sender: TObject);
begin
   TreatThisAsSingleTif := true;
   PickAndOpenImagery(ItSat);
end;

procedure Twmdem.Importfromcamera1Click(Sender: TObject);
var
   FilesWanted : TStringList;
   DefaultFilter : byte;
   i : integer;
begin
   DefaultFilter := 1;
   FilesWanted := tStringList.Create;
   if GetMultipleFiles('Import pictures from camera','AnyFile|*.jpg',FilesWanted, DefaultFilter) then begin
      GetDOSPath('import location',PhotoDir);
      StartProgress('Import photos');
      for i := 0 to pred(FilesWanted.Count) do begin
         UpDateProgressBar(i/FilesWanted.Count);
         Petmar.MoveFile(FilesWanted.Strings[i],PhotoDir + ExtractFileName(FilesWanted.Strings[i]));
      end;
      FilesWanted.Free;
      EndProgress;
      MessageToContinue('Turn off camera');
      Slidesorter1Click(Sender);
   end;
end;



initialization
   {$IfDef MessageStartup} MessageToContinue('start wmaindem initialization'); {$EndIf}
   WMDEM := Nil;
   FirstRun := true;
   OnVasaPage := 0;
   ShowLoadButtons := true;
   SkipMenuUpdating := false;
   LockStatusBar := false;
finalization
   {$IfDef RecordMGT} WriteLineToDebugFile('RecordMGTProblems active in Wmaindem'); {$EndIf}
   {$IfDef RecordDBFconvert} WriteLineToDebugFile('RecordDBFconvertProblems active in Wmaindem'); {$EndIf}
   {$IfDef RecordLabs} WriteLineToDebugFile('RecordLabs active in Wmaindem'); {$EndIf}
   {$IfDef RecordFormResize} WriteLineToDebugFile('RecordFormResize active in Wmaindem'); {$EndIf}
   {$IfDef RecordClosing} WriteLineToDebugFile('RecordClosingProblems active in Wmaindem'); {$EndIf}
   {$IfDef RecordStartup} WriteLineToDebugFile('RecordStartupProblems active in Wmaindem'); {$EndIf}
   {$IfDef RecordFormActivate} WriteLineToDebugFile('RecordFormActivate active in Wmaindem'); {$EndIf}
   {$IfDef RecordMenu} WriteLineToDebugFile('RecordMenu active in Wmaindem'); {$EndIf}
   {$IfDef RecordLag} WriteLineToDebugFile('RecordLagProblems active in Wmaindem'); {$EndIf}
   {$IfDef RecordFullLag} WriteLineToDebugFile('RecordFullLagProblems active in Wmaindem'); {$EndIf}
   {$IfDef RecordMrSidStartup} WriteLineToDebugFile('RecordMrSidStartupProblems  active in Wmaindem');{$EndIf}
   {$IfDef FullDrainageBasinStats} WriteLineToDebugFile('FullDrainageBasinStats active in Wmaindem'); {$EndIf}
   {$IfDef DrainageBasinStats} WriteLineToDebugFile('DrainageBasinStats active in Wmaindem'); {$EndIf}
   {$IfDef RecordUpdate} WriteLineToDebugFile('RecordUpdateProblem active in WMainDEM');   {$EndIf}
   {$IfDef DrainageBasinStreamsInBasins} writeLineToDebugFile('DrainageBasinStreamsInBasins active in WMainDEM');    {$EndIf}
   {$IfDef RecordButton} WriteLineToDebugFile('RecordButtonProblems active in WMainDEM'); {$EndIf}
   {$IfDef RecordOpenVectorMap} writeLineToDebugFile('RecordOpenVectorMap active in WMainDEM'); {$EndIf}
   {$IfDef RecordHelp} WriteLineToDebugFile('RecordHelp active in WMainDEM'); {$EndIf}
   {$IfDef RecordGeostats} WriteLineToDebugFile('RecordGeostats active in WMainDEM'); {$EndIf}
   {$IfDef RecordOceanography} WriteLineToDebugFile('RecordOceanography active in WMainDEM'); {$EndIf}
   {$IfDef Record3D} WriteLineToDebugFile('Record3DProblems active in WMainDEM'); {$EndIf}
   {$IfDef RecordCartography} WriteLineToDebugFile('RecordCartography active in WMainDEM'); {$EndIf}
   {$IfDef RecordSatLoad} WriteLineToDebugFile('RecordSatLoad active in WMainDEM'); {$EndIf}
   {$IfDef RecordClosing} WriteLineToDebugFile('WMainDEM finalization complete'); {$EndIf}
end.


 (*
procedure Twmdem.Inventoryblockstatistics1Click(Sender: TObject);
var
   i,n : integer;
   Results,TheFiles : tStringList;
   Table : tMyData;
   Sum : float64;
   fName,dbfDir,GridDir :PathStr;
begin
   GetAtlasDirectories(dbfDir,GridDir);
   TheFiles := Nil;
   Petmar.FindMatchingFiles(DBFdir,DefaultDBMask,TheFiles,0);

   Results := tStringList.Create;
   Results.Add('NAME,NPTS,AVG');
   StartProgress('Inventory');
   for i := 0 to pred(TheFiles.Count) do  begin
      UpDateProgressBar(i/TheFiles.Count);
      fName := TheFiles.Strings[i];
      Table := tMyData.Create(fName);
      n := Table.RecordCount;
      if n = 0 then Sum := 0
      else Sum := Table.FieldSum('NPTS') / n;

      Results.Add(ExtractFileNameNoExt(fName) + ',' + IntToStr(n) + ',' + RealToString(Sum,-12,-2));
      Table.Destroy;
   end;
   Results.SaveToFile('C:\temp\inventory.csv');
   TheFiles.Free;
   Results.Free;
   EndProgress;
end;
*)


