unit nevadia_main;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}


{$I nevadia_defines.inc}

//{$Define IncludePython}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   {$IFDEF DEBUG}
      //{$Define RecordBatch}
      {$Define RecordCommandLine}
      {$Define RecordDEMIX}
      //{$Define RecordMerge}
      //{$Define RecordDragonPlot}
      //{$Define RecordFullDEMIX}
      //{$Define RecordDEMIXLoops}
      //{$Define RecordDEMIXGridCompare}
      //{$Define TrackDEMIX_DEMs}
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
      //{$Define LidarGlobalDEMs}
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

   {$IfDef UseTDBF}
      dbf,
   {$EndIf}

   {$IfDef UseTCLientDataSet}
      DataSnap.dbclient,
   {$EndIf}
//end DB declarations

   DEMDefs,Petmar_types,PETMAR, DEMMapDraw, DEMMapF,
   System.Math.Vectors,System.Types,System.RTLConsts,System.Win.TaskbarCore,System.IOUtils,System.Diagnostics,System.UiTypes,
   Windows,ShlObj, WinInet,
   Vcl.Taskbar,
   Classes,Graphics,Forms,Controls,Menus,Printers,StdCtrls,ComObj,ClipBrd,
   Dialogs, ExtCtrls, ComCtrls, Buttons, ToolWin,SysUtils,ShellAPI,Messages,URLMon,Math,StrUtils;

const
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
    CoordsPopupMenu7: TPopupMenu;
    UTMordatumconversion1: TMenuItem;
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
    TigerSpeedButton: TSpeedButton;
    Conicconversions1: TMenuItem;
    Polarstereographic1: TMenuItem;
    Algorithms1: TMenuItem;
    Verticalearthcurvature1: TMenuItem;
    Horizontalearthcurvature1: TMenuItem;
    Oceanographicdata1: TMenuItem;
    CTD1: TMenuItem;
    Lightdata1: TMenuItem;
    IntDBSpeedButton: TSpeedButton;
    HypImageSpeedButton: TSpeedButton;
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
    OpenandmergeDEMs1: TMenuItem;
    ImportCTDfile1: TMenuItem;
    EXIFmetadata1: TMenuItem;
    EXIFimage1: TMenuItem;
    Clustergrids1: TMenuItem;
    Copyfile1: TMenuItem;
    Distancematrix1: TMenuItem;
    XYZshapefile1: TMenuItem;
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
    OpenDEMwithoutmap1: TMenuItem;
    Megathrusts1: TMenuItem;
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
    Californiaoffshore1: TMenuItem;
    GulfofMexicoGLORIA1: TMenuItem;
    Atlantis1: TMenuItem;
    DEMsummarytable1: TMenuItem;
    Landsatfullsceneindex1: TMenuItem;
    Satellitepredictions1: TMenuItem;
    Subset81Ssidescan1: TMenuItem;
    N15: TMenuItem;
    Sealevelrise1: TMenuItem;
    N29: TMenuItem;
    Ages1: TMenuItem;
    Magneticanomaliesgrid1: TMenuItem;
    Sedimenttypegrid1: TMenuItem;
    Sedimentthicknessgrid1: TMenuItem;
    Fontsinstalled1: TMenuItem;
    Unicodeicongenerator1: TMenuItem;
    UKOSgrid2: TMenuItem;
    Nyquist1: TMenuItem;
    Onlinehelp1: TMenuItem;
    Introductorytutorials1: TMenuItem;
    N30: TMenuItem;
    OpenandmergeDEMdirectories1: TMenuItem;
    SpeedButton6: TSpeedButton;
    est1: TMenuItem;
    LatLong1: TMenuItem;
    Edit1: TMenuItem;
    EditDEMHeader1: TMenuItem;
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
    OpenSentinen2image1: TMenuItem;
    Openlidarmatchedgrids1: TMenuItem;
    Photos1: TMenuItem;
    //Importfromcamera1: TMenuItem;
    RenameJPRGswithbasenamenumber1: TMenuItem;
    RenameJPEGSwithbaseandcreationtime1: TMenuItem;
    Labs1: TMenuItem;
    N18: TMenuItem;
    CloseallDBs1: TMenuItem;
    Remotesensinglabs1: TMenuItem;
    CloseprogramupdateEXEnewversion7MBdownload1: TMenuItem;
    Spectrallibrary3: TMenuItem;
    N19: TMenuItem;
    RGBcolorlayers1: TMenuItem;
    GDALSRSinfo1: TMenuItem;
    WhiteboxGeotiff1: TMenuItem;
    GDALslopesarcsecondDEMs1: TMenuItem;
    Mediansatellitedatacontest1: TMenuItem;
    Makelittletilescontest1: TMenuItem;
    Guam1: TMenuItem;
    Geotiff2: TMenuItem;
    GDALWKT1: TMenuItem;
    N20: TMenuItem;
    N25: TMenuItem;
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
    DEMcornerstable1: TMenuItem;
    netcdf1: TMenuItem;
    ACOLITEallopensatelliteimages1: TMenuItem;
    Fatfingers1: TMenuItem;
    Closeprogramgetdebugversionoftheprogram7MB1: TMenuItem;
    Openrecyclebin1: TMenuItem;
    Existingfile1: TMenuItem;
    Existingfile2: TMenuItem;
    Horizontalimageslider1: TMenuItem;
    Allgraphsononeimage1: TMenuItem;
    OpensingleLandsatband1: TMenuItem;
    listgeo1: TMenuItem;
    DEMIXmergeCSVfiles1: TMenuItem;
    DEMIXtilesizebylatitude1: TMenuItem;
    DEMIXreferenceDEMcreation1: TMenuItem;
    Python1: TMenuItem;
    OpenSentinel1radarimagery1: TMenuItem;
    DEMIX1: TMenuItem;
    DemixAnalysisPopupMenu: TPopupMenu;
    N35: TMenuItem;
    BatchNDVI: TMenuItem;
    HistogramstoCSVfiles1: TMenuItem;
    Bringslicecontroltofront1: TMenuItem;
    Bringpointcloudcontroltofront1: TMenuItem;
    Viewlastexectiondebuglog1: TMenuItem;
    COPALOScomparetoreference1: TMenuItem;
    Pixelbypixelmapstatistics1: TMenuItem;
    COPALOShighlowgeomorphometry1: TMenuItem;
    Metadata2: TMenuItem;
    N37: TMenuItem;
    N38: TMenuItem;
    Pickdatadirectory1: TMenuItem;
    N3OpenDEMs1: TMenuItem;
    OpenandmergeDEMsgridsverylarge1: TMenuItem;
    Creatediffrencemaps1: TMenuItem;
    Mergesourcedatatiles1: TMenuItem;
    ProcessVDATUMshifts1: TMenuItem;
    Processdifferencestatisticspertile1: TMenuItem;
    VDATUMshiftinUSA1: TMenuItem;
    DEMIX2: TMenuItem;
    OpenDEMIXdatabase1: TMenuItem;
    Addversionnumbertoallfilesinapath1: TMenuItem;
    DEMIXdbCreatePopupMenu: TPopupMenu;
    //Fullprocessingchain1: TMenuItem;
    Perpendicularshortprofilesthroughpoint1: TMenuItem;
    N41: TMenuItem;
    SummarizeverticaldatumshiftforEGM96testDEMs1: TMenuItem;
    GDALshiftFor3DEP1: TMenuItem;
    OpenandmergeDEMswithfullDEMIXcoverage1: TMenuItem;
    Subsetlarge3DEPareas1: TMenuItem;
    Create3DEP1secrefDEMs1: TMenuItem;
    Merge3DEPreferenceDEMsbyarea1: TMenuItem;
    CreatehalfsecondreferenceDEMs1: TMenuItem;
    Merge1secreferenceDEMsfromVisioterra1: TMenuItem;
    Modesofdifferencedistributions1: TMenuItem;
    Advancedanalysis1: TMenuItem;
    MultipledNBRmaps1: TMenuItem;
    N28: TMenuItem;
    DEMIXhelp1: TMenuItem;
    Landformcategorieslegends1: TMenuItem;
    Extract1: TMenuItem;
    N43: TMenuItem;
    N44: TMenuItem;
    Pythontestrun1: TMenuItem;
    Listofsubdirectoriesfullpaths1: TMenuItem;
    Listofsubdirectoriesrelativenames1: TMenuItem;
    SumatraPDFhelpcontents1: TMenuItem;
    Noaddedlegends1: TMenuItem;
    Noaddedlegends2: TMenuItem;
    N36: TMenuItem;
    N45: TMenuItem;
    MergeSSIMandR2database1: TMenuItem;
    CheckfilesizesforSSIMimagemismatches1: TMenuItem;
    DiluviumDEMandDEMIXDBoverlap1: TMenuItem;
    CheckreferenceDEMs1: TMenuItem;
    ChecktestDEMs1: TMenuItem;
    DiluviumDEMfortestareas1: TMenuItem;
    CreaterangereferenceDEMs1: TMenuItem;
    Addprefixtoallfilesindirectory1: TMenuItem;
    Inventorydifferencestats1: TMenuItem;
    MergeDEMIXtilestats1: TMenuItem;
    N48: TMenuItem;
   // FillholesintestareaDEMs1: TMenuItem;
    //VectorchannelnetworksSAGA1: TMenuItem;
    //Createchannelnetworkgrids1: TMenuItem;
   // Channelnetworkcomparison1: TMenuItem;
    Channelnetworkmisspercentagesbytile1: TMenuItem;
    SSIM2: TMenuItem;
    MergemultipleTXTCSVintoDB1: TMenuItem;
    N49: TMenuItem;
    Overwrite4: TMenuItem;
    CreatetestareaDEMSskipifexists1: TMenuItem;
    CreatetestareaDEMs1: TMenuItem;
    Overwriteifexits1: TMenuItem;
    Overwriteifexits2: TMenuItem;
    Overwirteifexists1: TMenuItem;
    Skipifexits1: TMenuItem;
    MICRODEMgridformat1: TMenuItem;
    N51: TMenuItem;
    Overwriteifexits3: TMenuItem;
    Skipifexits2: TMenuItem;
    CheckreferenceDEMSareEGM2008withPixelIsset1: TMenuItem;
    Overwriteifexists1: TMenuItem;
    Overwriteifexists2: TMenuItem;
    Inventory3DEPtiles1: TMenuItem;
    SSIMR21: TMenuItem;
    InventoryDILUVIUMbytestarea1: TMenuItem;
    Partialchannelnetworkprocessing1: TMenuItem;
    Channelnetworkmultistep1: TMenuItem;
    InsureallreferenceDTMscorrectlynamed1: TMenuItem;
    N50: TMenuItem;
    MaskwaterinreferenceDEMs1: TMenuItem;
    ComputeDEMIXtilestats1: TMenuItem;
    VerifytestDEMcoverages1: TMenuItem;
    rimreferencedatatoDEMIXtiles1: TMenuItem;
    N53: TMenuItem;
    Full3DEPprocessingchair1: TMenuItem;
    Partial3DEPsteps1: TMenuItem;
    Overwriteifexists3: TMenuItem;
    Overwriteifexists4: TMenuItem;
    //OvOverwriteifexists1: TMenuItem;
    //Skipifexists2: TMenuItem;
    N39: TMenuItem;
    N54: TMenuItem;
    Inventorychanneldatabyarea1: TMenuItem;
    DatumshiftCanadianDEMs1: TMenuItem;
    MergeCanadianLidar1: TMenuItem;
    Reference1secDTMsfromCanadianlidar1: TMenuItem;
    Graphevaluationandscores1: TMenuItem;
    CleardoubleprocessedreferenceDEMtiles1: TMenuItem;
    Inventories1: TMenuItem;
    EditreferenceandtestDEMs1: TMenuItem;
    Experimentaltargetsforelimination1: TMenuItem;
    Fullchain1: TMenuItem;
    Partialprocessing1: TMenuItem;
    Overwirte1: TMenuItem;
    Skipeifpresent1: TMenuItem;
    Overwirte2: TMenuItem;
    Overwirte3: TMenuItem;
    InventorySSIMFUVCSVfiles1: TMenuItem;
    N40: TMenuItem;
    N42: TMenuItem;
    N47: TMenuItem;
    DeltaDTMfortestareas1: TMenuItem;
    Overwrite1: TMenuItem;
    Skipifexists3: TMenuItem;
    CreatefinalDB1: TMenuItem;
    N3DEPfileswithtag421121: TMenuItem;
    Fixtileswith42114foottag1: TMenuItem;
    MergeSSIMFUV1: TMenuItem;
    Onedegreetilestocovertestareas1: TMenuItem;
    N46: TMenuItem;
    GetrangesforSSIMhydro1: TMenuItem;
    OpenmapsforDEMIXtestarea1: TMenuItem;
    LoadCpopDEMandLNDCOERFORTEXTAREA1: TMenuItem;
    Createlandcovergrids1: TMenuItem;
    Mergechannelnetworkevaluations1: TMenuItem;
    Changemode1: TMenuItem;
    DEMIXtilesperareaandcoastalsubset1: TMenuItem;
    Overwirte4: TMenuItem;
    Skiipifpresent1: TMenuItem;
    InventoryallDEMIXdatafiles1: TMenuItem;
    Combineallcombinedimages1: TMenuItem;
    CoastalDEMfortestareas1: TMenuItem;
    OverwriteallthreecoastalDTMS1: TMenuItem;
    InventoryWbWfilesbyarea1: TMenuItem;
    N52: TMenuItem;
    N55: TMenuItem;
    Overwrite2: TMenuItem;
    Skipifpresent1: TMenuItem;
    Mergegeomorphonevaluatioins1: TMenuItem;
    Overwrite3: TMenuItem;
    Skipifdone1: TMenuItem;
    DeletereferenceDTMswithoutDTMinfilename1: TMenuItem;
    MovereferenceDSMs1: TMenuItem;
    DEMIXtilesineachareaforFULLU120U80andandU101: TMenuItem;
    Ridgesandvalleys1: TMenuItem;
    Overwrite5: TMenuItem;
    Overwrite6: TMenuItem;
    Mergeridgesandvalleys1: TMenuItem;
    Overwrite7: TMenuItem;
    Skipifpresent2: TMenuItem;
    N56: TMenuItem;
    Compareconvergenceindexfortestarea1: TMenuItem;
    Open4elevationrangeDEMIXDBs1: TMenuItem;
    UTMprojection1: TMenuItem;
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
    procedure Vectormap1Click(Sender: TObject);
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
    procedure OpenTIGERcountrymap1Click(Sender: TObject);
    procedure Openproject1Click(Sender: TObject);
    procedure CTD1Click(Sender: TObject);
    procedure Lightdata1Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure StatusBar1DblClick(Sender: TObject);
    procedure OpenDatabase1Click(Sender: TObject);
    procedure Openshapefilemap1Click(Sender: TObject);
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
    procedure OpenandmergeDEMs1Click(Sender: TObject);
    procedure ImportCTDfile1Click(Sender: TObject);
    procedure EXIFmetadata1Click(Sender: TObject);
    procedure EXIFimage1Click(Sender: TObject);
    procedure Clustergrids1Click(Sender: TObject);
    procedure Copyfile1Click(Sender: TObject);
    procedure Distancematrix1Click(Sender: TObject);
    procedure XYZshapefile1Click(Sender: TObject);
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
    //procedure Afar1Click(Sender: TObject);
    procedure Updatehelpfile2Click(Sender: TObject);
    //procedure TulaFracturezonemagnetics1Click(Sender: TObject);
    procedure Platerotations1Click(Sender: TObject);
    procedure Sedimentationrates1Click(Sender: TObject);
    procedure Triplejunctions1Click(Sender: TObject);
    procedure Trenchgeometry1Click(Sender: TObject);
    procedure SedThickButtonClick(Sender: TObject);
    procedure Italyfocalmechs1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Statsfortrainingset1Click(Sender: TObject);
    procedure Statesforblockgrids1Click(Sender: TObject);
    procedure Normalizegrids1Click(Sender: TObject);
    procedure Addnormaliziedstatsforblockgridstotrainingset1Click(Sender: TObject);
    //procedure Annapolisredistricting1Click(Sender: TObject);
    //procedure Duckbeachsurveys1Click(Sender: TObject);
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
   // procedure Californiaoffshore1Click(Sender: TObject);
    procedure GulfofMexicoGLORIA1Click(Sender: TObject);
    procedure Atlantis1Click(Sender: TObject);
    procedure DEMsummarytable1Click(Sender: TObject);
    procedure Zeorlog1Click(Sender: TObject);
    procedure Landsatfullsceneindex1Click(Sender: TObject);
    procedure Satellitepredictions1Click(Sender: TObject);
    procedure Subset81Ssidescan1Click(Sender: TObject);
    //procedure GISdatasampler1Click(Sender: TObject);
    procedure OpenScannedmap1Click(Sender: TObject);
    procedure Openlandcover1Click(Sender: TObject);
    //procedure Sealevelrise1Click(Sender: TObject);
    procedure Ages1Click(Sender: TObject);
    procedure Magneticanomaliesgrid1Click(Sender: TObject);
    procedure Sedimentthicknessgrid1Click(Sender: TObject);
    procedure Sedimenttypegrid1Click(Sender: TObject);
    procedure Fontsinstalled1Click(Sender: TObject);
    procedure Unicodeicongenerator1Click(Sender: TObject);
    procedure UKOSgrid2Click(Sender: TObject);
    //procedure HarpersFerryTerrainAnalysis1Click(Sender: TObject);
    procedure Nyquist1Click(Sender: TObject);
    procedure Onlinehelp1Click(Sender: TObject);
    //procedure AnnapolisTM8scene1Click(Sender: TObject);
    procedure OpenandmergeDEMdirectories1Click(Sender: TObject);
    //procedure SanitizedXTF1Click(Sender: TObject);
    procedure SpeedButton6Click(Sender: TObject);
    procedure est1Click(Sender: TObject);
    procedure LatLong1Click(Sender: TObject);
    procedure EditDEMHeader1Click(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
    procedure Seismicviewing1Click(Sender: TObject);
    procedure RestorepreviousprogramEXE1Click(Sender: TObject);
    procedure DBFfile1Click(Sender: TObject);
    procedure GeoPDF1Click(Sender: TObject);
    procedure OpenGeoPDF1Click(Sender: TObject);
    procedure lasinfo1Click(Sender: TObject);
    procedure KMLKMZfile1Click(Sender: TObject);
    procedure XML1Click(Sender: TObject);
    procedure OpenGeoPDFimagelayer1Click(Sender: TObject);
    procedure Allindividuallayers1Click(Sender: TObject);
    //procedure LASlidarpointcloudsamples1Click(Sender: TObject);
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
    procedure Slidesorter1Click(Sender: TObject);
    procedure Movefileswithnamematch1Click(Sender: TObject);
    procedure RenameJPEGswithcreationtime1Click(Sender: TObject);
    procedure Mergemasp1Click(Sender: TObject);
    procedure Batchchangepartoffilenames1Click(Sender: TObject);
    procedure OpenandmergeGeotiffs1Click(Sender: TObject);
    procedure DragonPlot1Click(Sender: TObject);
    //procedure Annapolislidar1Click(Sender: TObject);
    procedure OpenSentinen2image1Click(Sender: TObject);
    procedure Openlidarmatchedgrids1Click(Sender: TObject);
    procedure RenameJPRGswithbasenamenumber1Click(Sender: TObject);
    procedure RenameJPEGSwithbaseandcreationtime1Click(Sender: TObject);
    procedure Labs1Click(Sender: TObject);
    procedure CloseallDBs1Click(Sender: TObject);
    //procedure CloseprogramupdateEXEnewversion7MBdownload1Click(Sender: TObject);
    procedure Spectrallibrary3Click(Sender: TObject);
    procedure RGBcolorlayers1Click(Sender: TObject);
    procedure GDALSRSinfo1Click(Sender: TObject);
    procedure WhiteboxGeotiff1Click(Sender: TObject);
    procedure GDALslopesarcsecondDEMs1Click(Sender: TObject);
    procedure Mediansatellitedatacontest1Click(Sender: TObject);
    procedure Makelittletilescontest1Click(Sender: TObject);
    procedure Guam1Click(Sender: TObject);
    procedure GDALWKT1Click(Sender: TObject);
    procedure Postprocesscontest1Click(Sender: TObject);
    procedure WKTprojection1Click(Sender: TObject);
    procedure ArcsecondspacingDTEDDGED1Click(Sender: TObject);
    procedure UTMspacingandgridtrueangle1Click(Sender: TObject);
    procedure CreateDEMsfromlidar1Click(Sender: TObject);
    procedure CategoriesfromallopenDEMs1Click(Sender: TObject);
    procedure Categoriesfromdatabase1Click(Sender: TObject);
    procedure Createcompositebitmap1Click(Sender: TObject);
    procedure Arcsecondrectangularpixels1Click(Sender: TObject);
    procedure DEMcornerstable1Click(Sender: TObject);
    procedure netcdf1Click(Sender: TObject);
    procedure ACOLITEallopensatelliteimages1Click(Sender: TObject);
    procedure Fatfingers1Click(Sender: TObject);
   // procedure Closeprogramgetdebugversionoftheprogram7MB1Click(Sender: TObject);
    procedure Openrecyclebin1Click(Sender: TObject);
    procedure Existingfile1Click(Sender: TObject);
    procedure Existingfile2Click(Sender: TObject);
    procedure Horizontalimageslider1Click(Sender: TObject);
    procedure OpensingleLandsatband1Click(Sender: TObject);
    procedure listgeo1Click(Sender: TObject);
    //procedure DEMIXmergeandtransposewithmeanmedian1Click(Sender: TObject);
    procedure DEMIXtilesizebylatitude1Click(Sender: TObject);
    procedure Python1Click(Sender: TObject);
    procedure OpenSentinel1radarimagery1Click(Sender: TObject);
    procedure DEMIX1Click(Sender: TObject);
    procedure BatchNDVIClick(Sender: TObject);
    //procedure Annapolislidar8GB1Click(Sender: TObject);
    procedure HistogramstoCSVfiles1Click(Sender: TObject);
    procedure Bringslicecontroltofront1Click(Sender: TObject);
    procedure Bringpointcloudcontroltofront1Click(Sender: TObject);
    procedure Viewlastexectiondebuglog1Click(Sender: TObject);
    procedure COPALOScomparetoreference1Click(Sender: TObject);
    procedure Pixelbypixelmapstatistics1Click(Sender: TObject);
    procedure COPALOShighlowgeomorphometry1Click(Sender: TObject);
    procedure Metadata2Click(Sender: TObject);
    //procedure LoadDEMIXareareferenceDEMs1Click(Sender: TObject);
    procedure Pickdatadirectory1Click(Sender: TObject);
    procedure N3OpenDEMs1Click(Sender: TObject);
    procedure OpenandmergeDEMsgridsverylarge1Click(Sender: TObject);
    procedure Creatediffrencemaps1Click(Sender: TObject);
    procedure Mergesourcedatatiles1Click(Sender: TObject);
    procedure ProcessVDATUMshifts1Click(Sender: TObject);
    procedure VDATUMshiftinUSA1Click(Sender: TObject);
    procedure OpenDEMIXdatabase1Click(Sender: TObject);
    procedure Addversionnumbertoallfilesinapath1Click(Sender: TObject);
    //procedure Fullprocessingchain1Click(Sender: TObject);
    procedure Perpendicularshortprofilesthroughpoint1Click(Sender: TObject);
    procedure N41Click(Sender: TObject);
    procedure SummarizeverticaldatumshiftforEGM96testDEMs1Click(Sender: TObject);
    procedure OpenandmergeDEMswithfullDEMIXcoverage1Click(Sender: TObject);
    procedure Subsetlarge3DEPareas1Click(Sender: TObject);
    procedure CreatehalfsecondreferenceDEMs1Click(Sender: TObject);
    procedure Merge1secreferenceDEMsfromVisioterra1Click(Sender: TObject);
    procedure Modesofdifferencedistributions1Click(Sender: TObject);
    procedure Advancedanalysis1Click(Sender: TObject);
    procedure MultipledNBRmaps1Click(Sender: TObject);
    procedure DEMIXhelp1Click(Sender: TObject);
    procedure Landformcategorieslegends1Click(Sender: TObject);
    procedure Extract1Click(Sender: TObject);
    procedure Pythontestrun1Click(Sender: TObject);
    procedure Listofsubdirectoriesrelativenames1Click(Sender: TObject);
    procedure Listofsubdirectoriesfullpaths1Click(Sender: TObject);
    procedure SumatraPDFhelpcontents1Click(Sender: TObject);
    procedure Noaddedlegends1Click(Sender: TObject);
    procedure Noaddedlegends2Click(Sender: TObject);
    procedure N45Click(Sender: TObject);
    procedure MergeSSIMandR2database1Click(Sender: TObject);
    procedure CheckfilesizesforSSIMimagemismatches1Click(Sender: TObject);
    procedure DiluviumDEMandDEMIXDBoverlap1Click(Sender: TObject);
    procedure CheckreferenceDEMs1Click(Sender: TObject);
    procedure ChecktestDEMs1Click(Sender: TObject);
    procedure DiluviumDEMfortestareas1Click(Sender: TObject);
    procedure CreaterangereferenceDEMs1Click(Sender: TObject);
    procedure Addprefixtoallfilesindirectory1Click(Sender: TObject);
    procedure Inventorydifferencestats1Click(Sender: TObject);
    procedure MergeDEMIXtilestats1Click(Sender: TObject);
    procedure MergemultipleTXTCSVintoDB1Click(Sender: TObject);
    procedure Overwrite4Click(Sender: TObject);
    procedure CreatetestareaDEMSskipifexists1Click(Sender: TObject);
    procedure Overwriteifexits1Click(Sender: TObject);
    procedure Skipifexits1Click(Sender: TObject);
    procedure Overwirteifexists1Click(Sender: TObject);
    procedure Overwriteifexits2Click(Sender: TObject);
    procedure MICRODEMgridformat1Click(Sender: TObject);
    procedure Overwriteifexits3Click(Sender: TObject);
    procedure Skipifexits2Click(Sender: TObject);
    procedure CheckreferenceDEMSareEGM2008withPixelIsset1Click(Sender: TObject);
    procedure Overwriteifexists1Click(Sender: TObject);
    procedure Overwriteifexists2Click(Sender: TObject);
    procedure Inventory3DEPtiles1Click(Sender: TObject);
    procedure InventoryDILUVIUMbytestarea1Click(Sender: TObject);
    procedure InsureallreferenceDTMscorrectlynamed1Click(Sender: TObject);
    procedure MaskwaterinreferenceDEMs1Click(Sender: TObject);
    procedure ComputeDEMIXtilestats1Click(Sender: TObject);
    procedure VerifytestDEMcoverages1Click(Sender: TObject);
    procedure rimreferencedatatoDEMIXtiles1Click(Sender: TObject);
    procedure Overwriteifexists3Click(Sender: TObject);
    procedure Overwriteifexists4Click(Sender: TObject);
    //procedure OvOverwriteifexists1Click(Sender: TObject);
    //procedure Skipifexists2Click(Sender: TObject);
    procedure Inventorychanneldatabyarea1Click(Sender: TObject);
    procedure DatumshiftCanadianDEMs1Click(Sender: TObject);
    procedure Reference1secDTMsfromCanadianlidar1Click(Sender: TObject);
    procedure Graphevaluationandscores1Click(Sender: TObject);
    procedure CleardoubleprocessedreferenceDEMtiles1Click(Sender: TObject);
    procedure Overwirte1Click(Sender: TObject);
    procedure Skipeifpresent1Click(Sender: TObject);
    procedure DEMIX2Click(Sender: TObject);
    procedure Overwirte2Click(Sender: TObject);
    procedure Overwirte3Click(Sender: TObject);
    procedure InventorySSIMFUVCSVfiles1Click(Sender: TObject);
    procedure N42Click(Sender: TObject);
    procedure DeltaDTMfortestareas1Click(Sender: TObject);
    procedure Overwrite1Click(Sender: TObject);
    procedure Skipifexists3Click(Sender: TObject);
    procedure CreatefinalDB1Click(Sender: TObject);
    procedure N3DEPfileswithtag421121Click(Sender: TObject);
    procedure Fixtileswith42114foottag1Click(Sender: TObject);
    procedure Partialprocessing1Click(Sender: TObject);
    procedure MergeSSIMFUV1Click(Sender: TObject);
    procedure Onedegreetilestocovertestareas1Click(Sender: TObject);
    procedure GetrangesforSSIMhydro1Click(Sender: TObject);
    procedure OpenmapsforDEMIXtestarea1Click(Sender: TObject);
    procedure LoadCpopDEMandLNDCOERFORTEXTAREA1Click(Sender: TObject);
    procedure Createlandcovergrids1Click(Sender: TObject);
    procedure Mergechannelnetworkevaluations1Click(Sender: TObject);
    procedure Changemode1Click(Sender: TObject);
    procedure DEMIXtilesperareaandcoastalsubset1Click(Sender: TObject);
    procedure Overwirte4Click(Sender: TObject);
    procedure Skiipifpresent1Click(Sender: TObject);
    procedure InventorytestandrefereneDEMsbytestarea1Click(Sender: TObject);
    procedure InventoryallDEMIXdatafiles1Click(Sender: TObject);
    procedure Combineallcombinedimages1Click(Sender: TObject);
    procedure CoastalDEMfortestareas1Click(Sender: TObject);
    procedure SSIMR21Click(Sender: TObject);
    procedure InventoryWbWfilesbyarea1Click(Sender: TObject);
    procedure Overwrite2Click(Sender: TObject);
    procedure Skipifpresent1Click(Sender: TObject);
    procedure Mergegeomorphonevaluatioins1Click(Sender: TObject);
    procedure Overwrite3Click(Sender: TObject);
    procedure Skipifdone1Click(Sender: TObject);
    procedure DeletereferenceDTMswithoutDTMinfilename1Click(Sender: TObject);
    procedure MovereferenceDSMs1Click(Sender: TObject);
    procedure DEMIXtilesineachareaforFULLU120U80andandU101Click(
      Sender: TObject);
    procedure Overwrite5Click(Sender: TObject);
    procedure Overwrite6Click(Sender: TObject);
    procedure Mergeridgesandvalleys1Click(Sender: TObject);
    procedure Overwrite7Click(Sender: TObject);
    procedure Skipifpresent2Click(Sender: TObject);
    procedure Compareconvergenceindexfortestarea1Click(Sender: TObject);
    procedure Open4elevationrangeDEMIXDBs1Click(Sender: TObject);
    procedure UTMprojection1Click(Sender: TObject);
  private
    procedure SunViews(Which : integer);
    procedure SeeIfThereAreDebugThingsToDo;
    { Private declarations }
  public
    { Public declarations }
      ProgramClosing,NoAutoOpen,AskForDebugUpdateNow,AskForNewUpdateNow : boolean;
      procedure SetMenusForVersion;
      procedure FormPlacementInCorner(TheForm : Forms.tForm; FormPosition : byte = lpSEMap);
      procedure HandleThreadTerminate(Sender: TObject);
      procedure SetPanelText(PanelNum : integer; What : shortString; OverrideLock : boolean = false);
      procedure ClearStatusBarPanelText;
     // procedure StartSealevelrise(BaseMap : tMapForm);
  end;

const
   IDDirToMark : PathStr = '';

var
   wmdem : Twmdem;
   LockStatusBar,
   ClosingEverything,
   SkipMenuUpdating,FirstRun : boolean;


function OpenGazFile(fName : PathStr = '') : integer;
procedure InsureFormOnScreenCurrentLocation(Form4 : tForm; x,y : integer);
procedure SetColorForWaiting; inline
procedure SetColorForProcessing; inline


implementation

{$R *.DFM}

uses

{$IfDef ExIndexes}
{$Else}
   DEM_indexes,
{$EndIf}

{$IfDef ExSieve}
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

   Dragon_plot_init,
   dp_control,

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

   {$IfDef IncludePython}
      Simple_Python,
   {$EndIf}

   {$IfDef ExDEMIX}
   {$Else}
      demix_definitions,
      DEMIX_Control,
      DEMIX_cop_alos,
      demix_evals_scores_graphs,
      ssim_fuv_control,
   {$EndIf}

   ufrmMain,

   Slider_sorter_form,
   NyqGraph,
   gdal_tools,

   PetImage_Form,
   TerSplsh,
   stereo_viewer,
   get_thumbnails,
   Main_Gray_game,
   DEM_manager,
   new_petmar_movie,

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
   Demeditw,
   NE_outlines,
   map_splitter,
   dem_gaz_opts, us_properties, demstringgrid,
   Get_PLSS,
   DEMdbTable,
   Petmar_ini_file,
   db_display_options,
   toggle_db_use, map_overlays, U_SolarPos2;


{$I nevadia_main_batch.inc}


procedure SetColorForWaiting; inline
begin
   WMdem.Color := clScrollBar;
end;

procedure SetColorForProcessing; inline
begin
   WMdem.Color := clInactiveCaption;
end;


procedure InsureFormOnScreenCurrentLocation(Form4 : tForm; x,y : integer);
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

procedure Twmdem.CleardoubleprocessedreferenceDEMtiles1Click(Sender: TObject);
begin
   ClearDoubleProcessed;
end;

procedure Twmdem.ClearStatusBarPanelText;
var
   i : integer;
begin
   if (not LockStatusBar) then begin
      for i := 0 to 3 do
          wmDEM.StatusBar1.Panels[i].Text := '';
   end;
end;

procedure Twmdem.SetPanelText(PanelNum : integer; What : shortString; OverrideLock : boolean = false);
begin
   if OverrideLock or (not LockStatusBar) then begin
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
   ThePath := '';
   GetDosPath('file matching',ThePath);
   SubDirs := 5;
   Petmar.ReadDefault('subdirectoy level',SubDirs);
   Ext := '.xtf';
   Petmar.GetString('file extension',Ext,false,ValidDosFileNameChars);
   Ext := '*' + Ext;
   NameContains := '000';
   Petmar.GetString('name contains',NameContains,false,ValidDosFileNameChars);
   ShowHourglassCursor;
   TheFiles := nil;
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


procedure Twmdem.VDATUMshiftinUSA1Click(Sender: TObject);
begin
   AnalyzeVDatumShift('');
end;


procedure Twmdem.Vectormap1Click(Sender: TObject);
begin
   {$IfDef RecordMenu} WriteLineToDebugFile('Twmdem.Vectormap1Click'); {$EndIf}
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


procedure Twmdem.Changemode1Click(Sender: TObject);
begin
   MDDef.DEMIX_Mode := dmNotYetDefined;
   GetDEMIXPaths(false);
end;

procedure Twmdem.CheckfilesizesforSSIMimagemismatches1Click(Sender: TObject);
var
   fName,fName2 : PathStr;
   sl,Findings : tStringList;
   i : Integer;
   H1,H2,W1,W2 : integer;
begin
   fName := 'C:\temp\ssim_global_norm\aster_size_mismatch.csv';
   sl := tStringList.Create;
   sl.LoadFromFile(fName);
   Findings := tStringList.Create;
   for i := 0 to pred(sl.Count) do begin
      fName := sl.Strings[i];
      fName2 := Petmar_types.BeforeSpecifiedCharacterANSI(fName,',',true,true);
      GeotiffImageSize(fName,w1,h1);
      GeotiffImageSize(fName2,w2,h2);
      Findings.Add(fName + ',' + fName2 + ',' + IntToStr(w1) + ',' + IntToStr(h1) + ',' + IntToStr(w2)+ ',' + IntToStr(h2) );
   end;

   DisplayAndPurgeStringList(Findings,'Size mismatches');
   ShowDefaultCursor;
   sl.Destroy;
end;

procedure Twmdem.CheckreferenceDEMs1Click(Sender: TObject);
begin
   InventoryDBwithDSMandDTMbyArea;
end;

procedure Twmdem.CheckreferenceDEMSareEGM2008withPixelIsset1Click(Sender: TObject);
begin
   CheckReferenceDEMsAreEGMandPixelIs;
end;


procedure Twmdem.ChecktestDEMs1Click(Sender: TObject);
begin
   CheckTestDEMs;
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
      {$IfDef RecordMenu} WriteLineToDebugFile('Twmdem.Data1Click (In<>Out)'); {$EndIf}
      DEMHandForm := TDEMHandForm.Create(Application);
      DEMHandForm.ShowModal;
   {$EndIf}
end;


procedure Twmdem.DatumshiftCanadianDEMs1Click(Sender: TObject);
begin
   //DEMIX_vert_datum_code := 6647;
   ShifDEMsto_UTM_WGS84_EGM2008(True);
end;

procedure Twmdem.DBFfile1Click(Sender: TObject);
begin
   if GetFileFromDirectory('dBase DBF file','*.dbf',LastDataBase) then ScreenDBFFileDump(LastDataBase);
end;

procedure Twmdem.DecemberSolstice1Click(Sender: TObject);
begin
    SunViews(2);
end;

procedure Twmdem.DeletereferenceDTMswithoutDTMinfilename1Click(Sender: TObject);
begin
    PruneMisnamedReferenceDTMs;
end;

procedure Twmdem.DeltaDTMfortestareas1Click(Sender: TObject);
begin
   DeltaDTMforTestAreas(false);
end;

procedure Twmdem.DEMcornerstable1Click(Sender: TObject);
begin
   DoGridSpacingAndDeclination(0);
end;

procedure Twmdem.DEMIX1Click(Sender: TObject);
begin
   StopSplashing;
   DemixAnalysisPopUpMenu.Popup(Mouse.CursorPos.X,Mouse.CursorPos.Y);
end;


procedure Twmdem.DEMIX2Click(Sender: TObject);
begin
   StopSplashing;
   DEMIXdbCreatePopupMenu.Popup(Mouse.CursorPos.X,Mouse.CursorPos.Y);
end;

procedure Twmdem.DEMIXhelp1Click(Sender: TObject);
begin
   DisplayHTMLTopic('demix_sg2\wine_contest.html');
end;

procedure Twmdem.DEMIXtilesineachareaforFULLU120U80andandU101Click(Sender: TObject);
begin
   TilesInEachElevRangeForTestAreas;
end;

procedure Twmdem.DEMIXtilesizebylatitude1Click(Sender: TObject);
var
   WantBoundBoxGeo : sfBoundBox;
   fName : PathStr;
begin
   WantBoundBoxGeo.XMin := 0.01;
   WantBoundBoxGeo.XMax := 0.02;
   WantBoundBoxGeo.YMin := -85;
   WantBoundBoxGeo.YMax := 85;
   CreateDEMIXTileShapefile(fName,WantBoundBoxGeo,false,true);
end;


procedure Twmdem.DEMIXtilesperareaandcoastalsubset1Click(Sender: TObject);
begin
   FindTilesInAreaForCoast;
end;

procedure Twmdem.DEMsummarytable1Click(Sender: TObject);
begin
   MakeDEMsummaryTable;
end;


procedure Twmdem.LoadCpopDEMandLNDCOERFORTEXTAREA1Click(Sender: TObject);
begin
   OpenCopDEMandLandcoverForArea;
end;

procedure Twmdem.Loadimage1Click(Sender: TObject);
begin
   OpenImageEditor;
end;


procedure Twmdem.Contents1Click(Sender: TObject);
var
   fName : PathStr;
begin
   {$IfDef RecordHelp} WriteLineToDebugFile('Twmdem.Contents1Click for help'); {$EndIf}
   StopSplashing;
   fName := ChangeFileExt(Application.ExeName,'.chm');
   DisplayHTMLTopic('html\microdem.htm');
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

   {$IfDef IncludePython}
   {$Else}
      Pythontestrun1.Visible := false;
   {$EndIf}

   {$IfDef ExGeoPDF}
      OpenGeoPDF1.Visible := false;
      OpenGeoPDFimagelayer1.Visible := false;
      Allindividuallayers1.Visible := false;
      Mergemasp1.Visible := false;
   {$EndIf}

   Open1.Visible := (MDDef.ProgramOption <> DragonPlotProgram);
   Toolbar1.Visible := MDDef.ShowMainToolbar and (MDDef.ProgramOption <> DragonPlotProgram);
   Introductorytutorials1.Visible := (MDDef.ProgramOption <> DragonPlotProgram);
   Help1.Visible := (MDDef.ProgramOption <> DragonPlotProgram) or TrilobiteComputer;

   ExpertDEMVersion := (MDDef.ProgramOption in [ExpertProgram,RemoteSensingProgram]) and (NumDEMDataSetsOpen > 0);
   RemoteSensingLabs1.Visible := (MDDef.ProgramOption in [ExpertProgram,RemoteSensingProgram]);
   SpeedButton6.Visible := (MDDef.ProgramOption in [ExpertProgram]);
   //HistoricShipwrecksLabs1.Visible := (MDDef.ProgramOption in [ShipwrecksProgram]) or MDDef.ShowLabs;

   SpeedButton2.Visible := MDDef.ShowSHPButton;
   SpeedButton4.Visible := MDDef.ShowDBonly;
   SpeedButton9.Visible := (MDDef.ProgramOption in [ExpertProgram]) and MDDef.UseGazetteer;
   MultProfSpeedButton.Visible := (MDDef.ProgramOption in [ExpertProgram,GeologyProgram,RemoteSensingProgram]);
   MultProfSpeedButton.Enabled := (NumDEMDataSetsOpen > 1);

   Bringslicecontroltofront1.Visible := (SlicerForm <> Nil);
   Bringpointcloudcontroltofront1.Visible := (pt_cloud_opts_fm <> Nil);

   CloseWindows1.Visible := (MDDef.ProgramOption <> DragonPlotProgram);

   {$IfDef RecordProblems}
   {$Else}
      Viewdebuglog1.Visible := false;
   {$EndIf}

   Tools1.Visible := not (MDDef.ProgramOption in [DragonPlotProgram]);

   Data1.Visible := (MDDef.ProgramOption in [ExpertProgram,RemoteSensingProgram]) and MDDef.ShowDataManipulation;
   InOutButton.Visible := Data1.Visible;
   DEMIX1.Visible := (MDDef.ProgramOption in [ExpertProgram]) and MDDef.ShowDEMIX;
   DEMIX2.Visible := (MDDef.ProgramOption in [ExpertProgram]) and MDDef.ShowDEMIX;

   LandCoverSpeedButton.Visible := (MDDef.ProgramOption in [ExpertProgram,RemoteSensingProgram]);

   XTFsidescan1.Visible := MDDef.ShowSidescan;

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

   LOS1.Visible := LOSButton.Visible;
   LOS1.Caption := '&LOS';

   LOSButton.Enabled := (NumDEMDataSetsOpen > 0);
   SpeedButton3.Enabled := (NumDEMDataSetsOpen > 0);
   FlySpeedButton.Enabled := (NumDEMDataSetsOpen > 0);
   PerspectiveButton.Enabled := (NumDEMDataSetsOpen > 0);
   Horizontalearthcurvature1.Visible := ExpertDEMVersion;
   Clustergrids1.Visible := ExpertDEMVersion;

   Superimposedtopoprofiles1.Visible := (NumDEMDataSetsOpen > 1) and (MDDef.ShowDEMcompare) and (MDDef.ProgramOption = ExpertProgram);

   Newpanorama1.Visible := (NumDEMDataSetsOpen > 0) and (MDDef.ProgramOption = ExpertProgram);
   TIGERSpeedButton.Visible := MDDef.TigrDef.ShowTIGERButton and (MDDef.ProgramOption = ExpertProgram);
   OpenTIGERcountymap1.Visible := TIGERSpeedButton.Visible;
   Openshapefilemap1.Visible := (MDDef.ProgramOption in [ExpertProgram,GeographyProgram]);
   OpenDataBaseWithoutMap1.Visible := (MDDef.ProgramOption = ExpertProgram);

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
   NewSATButton.Visible := MDDef.ShowOpenImagery;
   OpenImage1.Visible := MDDef.ShowOpenImagery;

   LiveFlySpeedButton.Enabled := (NumDEMDataSetsOpen > 0);

   Physicalgeographylabs1.Visible := (MDDef.ProgramOption = GeographyProgram) or MDDef.ShowLabs;
   GISlabs1.Visible := MDDef.ShowLabs;

   SpeedButton5.Visible := Geostatisticalanalysis1.Visible;

   VectorMapButton.Visible := MDDef.ShowCartography;
   OpenVectorMap1.Visible := (MDDef.ProgramOption in [ExpertProgram,GeologyProgram,GeographyProgram,RemoteSensingProgram]) or MDDef.ShowCartography;

   SpeedButton8.Visible := (MDDef.ProgramOption in [ExpertProgram,GeologyProgram,GeographyProgram,RemoteSensingProgram]);

   Oceanographicdata1.Visible := (MDDef.ProgramOption = ExpertProgram) and MDDef.ShowOceanographyOptions;

   Perspective1.Visible := PerspectiveButton.Visible;
   Batchprocessing1.Visible := (MDDef.ProgramOption = ExpertProgram);
   Edit1.Visible := (MDDef.ProgramOption = ExpertProgram) and (NumDEMDataSetsOpen = 0);
   Photos1.Visible := (MDDef.ProgramOption = ExpertProgram);

   {$IfDef ExDP}
      DragonPlot1.Visible := false;
   {$Else}
      DragonPlot1.Visible := (MDDef.ProgramOption in [ExpertProgram,DragonPlotProgram]);
   {$EndIf}

   {$IfDef ExIndexes}
      IntDBSpeedButton.Visible := false;
   {$Else}
      IntDBSpeedButton.Visible := FileExists(MapLibraryFName) and MDDef.ShowIntDB;
   {$EndIf}
   {$IfDef ExOpenGL}
      OpenGL1tofront.Visible := false;
   {$Else}
      OpenGL1tofront.Visible := (View3DForm <> nil) or (Map3D <> nil);
   {$EndIf}

   {$IfDef IncludeGeologyLabs}
      LabSpeedButton7.Visible := (MDDef.ProgramOption in [GeographyProgram,GeologyProgram]);
      StructuralGeologylab1.Visible := (MDDef.ProgramOption in [GeologyProgram]) or MDDef.ShowLabs;
   {$Else}
      StructuralGeologylab1.Visible := false;
      LabSpeedButton7.Visible := (MDDef.ProgramOption in [GeographyProgram]);
   {$EndIf}

   {$IfDef ExHypImage}
      HypImageSpeedButton.Visible := false;
      Openhyperspectralimagery1.Visible := false;
   {$Else}
      HypImageSpeedButton.Visible := (MDDef.ProgramOption in [ExpertProgram,RemoteSensingProgram]) and MDDef.ShowOpenImagery;
      Openhyperspectralimagery1.Visible := (MDDef.ProgramOption in [ExpertProgram,RemoteSensingProgram]) and MDDef.ShowOpenImagery;
   {$EndIf}

   {$IfDef ExPointCloud}
      PClouder1.Visible := false;
   {$Else}
      PClouder1.Visible := (MDDef.ProgramOption in [ExpertProgram,RemoteSensingProgram]) and MDDef.ShowPointClouds;
      Other3Dpointclouds1.Visible := (MDDef.ProgramOption in [ExpertProgram]) and MDDef.ShowPointClouds;
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
   {$EndIf}

   {$IfDef ExSieve}
      Sieve1.Visible := false;
   {$Else}
      Sieve1.Visible := MDDef.ShowSieve;
   {$EndIf}

   OverwriteIfExists1.Enabled := MDDef.DEMIX_overwrite_enabled;
   OverwirteIfExists1.Enabled := MDDef.DEMIX_overwrite_enabled;
   OverwriteIfExits1.Enabled := MDDef.DEMIX_overwrite_enabled;
   //OverwriteIfExists2.Enabled := MDDef.DEMIX_overwrite_enabled;
   OverwriteIfExits3.Enabled := MDDef.DEMIX_overwrite_enabled;
   Overwrite4.Enabled := MDDef.DEMIX_overwrite_enabled;
   //OverwriteIfExits2.Enabled := MDDef.DEMIX_overwrite_enabled;

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
      GeologyProgram : Caption := 'Geology ' + EXENameWithBuild;
      GeographyProgram : Caption := 'Physical Geography ' + EXENameWithBuild;
      RemoteSensingProgram : Caption := 'Remote Sensing ' + EXENameWithBuild;
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
   FileList : tStringList;
   Action,xval,yval : shortstring;
   infile,outfile,upfile,downfile : PathStr;

         function OpenADEM(OpenMap : boolean = false) : boolean;
         begin
            Result := FileExists(Infile) and LoadNewDEM(DEM,infile,OpenMap);
         end;

begin
   {$IfDef RecordCommandLine} WriteLineToDebugFile('Process ? command line ' + Commandline); {$EndIf}
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
      if Key = 'FILELIST' then begin
         FileList := tStringList.Create;
         FileList.LoadFromFile(Value)
      end;

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
          MDDef.PCAutoFillHoles := MDdef.FillHoleRadius > 0;
      end;
      if Key = 'PIXELIS' then begin
          if Value = 'AREA' then MDDef.LasDEMPixelIs := 1;
          if Value = 'POINT' then MDDef.LasDEMPixelIs := 2;
      end;

      {$IfDef AllowGeomorphometry}
         if Key = 'BOXSIZE' then MDDef.GeomorphBoxSizeMeters := StrToInt(Value);
      {$EndIf}
   end;
   {$IfDef RecordCommandLine} WriteLineToDebugFile('action=' + Action); {$EndIf}

   if Action = 'DEM2JSON' then begin
      if OpenADEM then begin
         DEMGlb[DEM].SaveAsGeoJSON;
      end;
   end;

   {$IfDef ExPointCloud}
   {$Else}
      if Action = 'LAS2JSON' then begin
         QuietActions := true;
         LAS2GeoJSON(infile);
      end;
   {$EndIf}


   if Action = 'RESAMPAVG' then begin
      if OpenADEM(true) then begin
         {$IfDef RecordCommandLine} WriteLineToDebugFile('dem opened'); {$EndIf}
         NewDEM := DEMGlb[DEM].ResampleByAveraging(false, false,Outfile);
         {$IfDef RecordCommandLine} WriteLineToDebugFile('resampled created'); {$EndIf}
      end;
   end;

   if Action = 'SLOPEMAP' then begin
      if OpenADEM then begin
         {$IfDef RecordCommandLine} WriteLineToDebugFile('dem opened'); {$EndIf}
         NewDEM := CreateSlopeMap(DEM,false);
         {$IfDef RecordCommandLine} WriteLineToDebugFile('slope map created'); {$EndIf}
         DEMGlb[NewDEM].SaveAsGeotiff(outfile);
         {$IfDef RecordCommandLine} WriteLineToDebugFile('geotiff saved'); {$EndIf}
      end;
   end;

   if Action = 'DEMIX-CREATE-REF' then begin
      BatchResampleForDEMIX(FileList);
   end;

(*
   if Action = 'OPENMAP' then begin
      if OpenADEM then begin
         {$IfDef RecordCommandLine} WriteLineToDebugFile('dem opened'); {$EndIf}
         MDDef.DoUpOpen := (UpFile <> '');
         MDDef.DoDownOpen := (DownFile <> '');
         MDDef.DoDiffOpen := false;
         MakeMomentsGrid(DEM,'O',MDDef.OpenBoxSizeMeters,false);
         {$IfDef RecordCommandLine} WriteLineToDebugFile('openness map created'); {$EndIf}
         if (UpFile <> '') then DEMGlb[MomentDEMs[UpOpenDEM]].SaveAsGeotiff(UpFile);
         if (DownFile <> '') then DEMGlb[MomentDEMs[DownOpenDEM]].SaveAsGeotiff(DownFile);
         {$IfDef RecordCommandLine} WriteLineToDebugFile('geotiff saved'); {$EndIf}
      end;
   end;

*)

   {$IfDef AllowGeomorphometry}
      if Action = 'TERR_FABRIC' then begin
         if OpenADEM then begin
            DEMGlb[DEM].OrientationTable(OutFile,Nil);
         end;
      end;
   {$EndIf}

   if Action = 'LCP' then begin
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
         if (MDDef.ProgramOption = DragonPlotProgram) then begin
            {$IfDef RecordDragonPlot} WriteLineToDebugFile('CheckDragonPlotOptions and it is DragonPlotProgram'); {$EndIf}
              {$IfDef ExDP}
                 MDDef.ProgramOption := ExpertProgram;
                 ShowDragonPlot := false;
              {$Else}
                 if (UpperCase(ptTrim(ParamStr(1))) = '-DP') then DragonPlotDef.AdvancedOptions := true;
                 {$IfDef RecordDirs}  RecordDirs('before start DP'); {$EndIf}
                 StartDragonPlot;
              {$EndIf}
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
     FirstRun := false;
     {$IfDef RecordFormActivate} WriteLineToDebugFile('Twmdem.FormActivate, initialize MD over'); {$EndIf}

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

     {$IfDef ExGDAL}
     {$Else}
        {$IfDef MSWindows}
            GetGDALFileNames;
        {$EndIf}
     {$EndIf}

     if not ValidPath(MapLibDir) then PickMapIndexLocation;

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
                  if TStr = '-FUVSSIM' then begin
                     Self.Width := 750;
                     Self.Height := 450;
                     Self.Top := 100;
                     Self.Left := 100;
                     FUV_SSIM_Processing(dmFull,false);
                     Halt;
                     //Close;
                     exit;
                  end
                  else begin
                     SetProgramOptions(TStr);
                  end;
               end;
           end;
       end;
    {$EndIf}

    {$If Defined(MessageStartup) or Defined(TrackFormCreate)} MessageToContinue('Twmdem.FormActivate after command line, width=' + IntToStr(Width) + '  & height=' + IntToStr(Height)); {$EndIf}
    {$IfDef RecordDirs}  RecordDirs('after command line'); {$EndIf}
    {$IfDef RecordProblems} WriteLineToDebugFile('MDdef.AutoOpen=' + IntToStr(ord(MDdef.AutoOpen)) + '  MDdef.ProgramOption=' + IntToStr(ord(MDdef.ProgramOption)) ); {$EndIf}

      if MDdef.ProgramOption in [GeologyProgram] then begin
         {$If Defined(ExGeology) or Defined(ExLabDownloads)}
         {$Else}
            {$IfDef RecordProblems} WriteLineToDebugFile('StructuralGeologyProgram, call GeologyGetData'); {$EndIf}
            GeologyGetData;
         {$EndIf}
      end
      else if MDdef.ProgramOption in [GeographyProgram] then begin
         {$IfDef ExGeography}
         {$Else}
            {$IfDef RecordProblems} WriteLineToDebugFile('GeographyProgram, call ClimateGetData'); {$EndIf}
            MDDef.ShowBlueMarble := true;
            MDDef.ImageryIconDirs := false;
            TryAutoOpen;
         {$EndIf}
      end
      else if (MDdef.ProgramOption = RemoteSensingProgram) then begin
         SetRemoteSensingDefaults;
         TryAutoOpen;
      end
      else if (MDdef.ProgramOption = DragonPlotProgram) then begin
         CheckDragonPlotOptions;
      end
      else if NoAutoOpen or (MDdef.AutoOpen = aoNothing) then begin
      end
      else if MDdef.ProgramOption in [ExpertProgram] then begin
         TryAutoOpen;
      end;

     {$IfDef RecordProblems} WriteLineToDebugFile('MDdef.AutoOpen completed, Twmdem.FormActivate wsMaximized, width=' + IntToStr(Width) + '  & height=' + IntToStr(Height)); {$EndIf}

     if (UpperCase(ptTrim(ParamStr(1))) = '-DataManip') then Data1Click(Sender);
     {$IfDef RecordProblems} WriteLineToDebugFile('ending FormActivate, first time'); {$EndIf}
     {$If Defined(MessageStartup) or Defined(TrackFormCreate)} MessageToContinue('Twmdem.FormActivate ending first time'); {$EndIf}
   end;
   WmDEM.StatusBar1.Panels[0].Text := '';
   SetMenusForVersion;
   {$If Defined(RecordFormResize) or Defined(TrackFormCreate)} WriteLineToDebugFile('Twmdem.FormActivate set menu versions'); {$EndIf}
   SeeIfThereAreDebugThingsToDo;
   Self.Visible := true;
   {$If Defined(RecordFormResize) or Defined(TrackFormCreate)} WriteLineToDebugFile('Twmdem.FormActivate end, width=' + IntToStr(Width) + '  & height=' + IntToStr(Height)); {$EndIf}
end;


procedure Twmdem.SeeIfThereAreDebugThingsToDo;
//allows immediate execution of code for debugging

(*
procedure bicubic_interpolation;
//written by ChatGPT, 14 May 2023

const
  N = 4;

type
  real = float32;
  matrix = array[0..N-1, 0..N-1] of real;

var
  x: matrix;
  y: matrix;
  f: matrix;
  a: matrix;
  b: matrix;
  c: matrix;
  d: matrix;
  px, py: real;
  i, j: integer;


    //   interpolate(px,      x[1,1], x[2,2],     a[1,1],    a[1,2],     a[2,1],    a[2,2],   b[1,1],   b[1,2],    c[1,1],    c[1,2],     d[1,1], d[1,2]);

function interpolate(x: real; x1: real; x2: real; f00: real; f01: real; f10: real; f11: real; f20: real; f21: real; f30: real; f31: real): real;
var
  m0, m1, m2, m3, a0, a1, a2, a3: real;
begin
  m0 := f01;
  m1 := (f11 - f01) / (x2 - x1);
  m2 := (f10 - f00) / (x2 - x1);
  m3 := (f21 - f11 - f10 + f00) / ((x2 - x1) * (x2 - x1) * (x2 - x1));

  a0 := f00;
  a1 := m0;
  a2 := 3 * (f21 - f11) / ((x2 - x1) * (x2 - x1)) - 2 * m0 / (x2 - x1) - m1 / (x2 - x1);
  a3 := (2 * (f11 - f21) + m0 + m1 * (x2 - x1)) / ((x2 - x1) * (x2 - x1) * (x2 - x1));

  interpolate := a0 + a1 * (x - x1) + a2 * sqr(x - x1) + a3 * sqr(x - x1) * (x - x2);
end;

begin
  // initialize x and y values
  for i := 0 to N-1 do begin
      for j := 0 to N-1 do  begin
          x[i,j] := i;
          y[i,j] := j;
        end;
    end;

  // initialize f values
  for i := 0 to N-1 do  begin
      for j := 0 to N-1 do begin
          f[i,j] := sin(i) + cos(j);
        end;
    end;

  // calculate coefficients
  for i := 1 to N-2 do begin
      for j := 1 to N-2 do begin
          a[i,j] := f[i,j];
          b[i,j] := (f[i+1,j] - f[i-1,j]) / 2;
          c[i,j] := (f[i-1,j] - 2*f[i,j] + f[i+1,j]) / 2;
          d[i,j] := (f[i,j+1] - f[i,j-1]) / 2;
        end;
    end;

  // perform interpolation
  px := 1.5; // x-coordinate to interpolate
  py := 1.5; // y-coordinate to interpolate

   interpolate(px, x[1,1], x[2,2], a[1,1], a[1,2], a[2,1], a[2,2], b[1,1], b[1,2], c[1,1], c[1,2], d[1,1], d[1,2]);

end;
*)

      (*
      procedure Histies;
      var
         DEMwithVAT,ElevMap,SlopeMap,RuffMap,AspMap : integer;
         Area : shortstring;
         Dir : PathStr;
      begin
         ElevMap := 0;
         SlopeMap := 0;
         RuffMap := 0;
         AspMap := 0;
         Area := 'canyon_range';
         Dir := 'H:\aa_half_sec_test\' + Area + '\';
         DEMwithVAT := OpenNewDEM(Dir + 'cop-alos-dtm4.dem' );
         ElevMap  := OpenNewDEM(Dir + 'ref_dtm.dem' );
         SlopeMap  := OpenNewDEM(Dir + 'cop-alos-dtm4.dem' );
         RuffMap := OpenNewDEM(Dir + 'cop-alos-dtm4.dem');
         AspMap := OpenNewDEM(Dir + 'cop-alos-dtm4.dem' );

         DEMGlb[ElevMap].VATrelatedGrid := DEMwithVAT;
         DEMGlb[ElevMap].SelectionMap.MapDraw.MapType := mt6ColorVAToverlay;
         DEMGlb[ElevMap].SelectionMap.DoBaseMapRedraw;
         HistogramsFromVATDEM(DEMwithVAT,ElevMap,SlopeMap,RuffMap,AspMap);
      end;
      *)
(*
var
   BatchFile : tStringList;
   aName : PathStr;
   cmd : shortstring;
*)
begin
   if TrilobiteComputer then begin
   end;
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
(*
  if StrUtils.AnsiContainsText(Application.ExeName,'1') then Self.Color := clGradientInactiveCaption;
  if StrUtils.AnsiContainsText(Application.ExeName,'2') then Self.Color := clGradientActiveCaption;
  if StrUtils.AnsiContainsText(Application.ExeName,'3') then Self.Color := clMoneyGreen;
  if StrUtils.AnsiContainsText(Application.ExeName,'4') then Self.Color := cl3DLight;
  if StrUtils.AnsiContainsText(Application.ExeName,'5') then Self.Color := clInfoBk;
  if StrUtils.AnsiContainsText(Application.ExeName,'6') then Self.Color := clYellow;
*)
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

   {$IfDef IncludeGeologyLabs}
   {$Else}
   {$EndIf}
   {$IfDef AllowUSNAdataDownloads}
   {$Else}
      Downloaddatasets1.Visible := false;
   {$EndIf}
   {$IfDef AllowProgramWebUpdates}
   {$Else}
       Updatehelpfile1.Visible := false;
       Closeprogramgetdebugversionoftheprogram7MB1.Visible := false;
       CloseprogramupdateEXEnewversion7MBdownload1.Visible := false;
   {$EndIf}
   {$IfDef Include2021datafusion}
   {$Else}
       Mediansatellitedatacontest1.Visible := false;
       Makelittletilescontest1.Visible := false;
       Postprocesscontest1.Visible := false;
   {$EndIf}

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


procedure Twmdem.Addprefixtoallfilesindirectory1Click(Sender: TObject);
begin
   AddSuffixOrPrefixToFiles(true);
end;

procedure Twmdem.Addproject1Click(Sender: TObject);
begin
   RestoreMicrodemDesktop('',false);
end;


procedure Twmdem.Addversionnumbertoallfilesinapath1Click(Sender: TObject);
begin
   AddSuffixOrPrefixToFiles(false);
end;


procedure Twmdem.Advancedanalysis1Click(Sender: TObject);
begin
   StopSplashing;
   DemixAnalysisPopUpMenu.Popup(Mouse.CursorPos.X,Mouse.CursorPos.Y);
end;



procedure Twmdem.Ages1Click(Sender: TObject);
begin
   {$IfDef ExGeology}
   {$Else}
      OpenNewDEM(PredAgesFile);
   {$EndIf}
end;


procedure Twmdem.Allindividuallayers1Click(Sender: TObject);
begin
   {$If Defined(ExGDAL) or Defined(ExGeoPDF)}
   {$Else}
      GDALconvertGeoPDF(gdalAllindividuallayers1);
   {$EndIf}
end;


procedure Twmdem.Arcsecondrectangularpixels1Click(Sender: TObject);
begin
   PixelRectangles;
end;


procedure Twmdem.ArcsecondspacingDTEDDGED1Click(Sender: TObject);
begin
   MakeDTEDTable;
end;


procedure Twmdem.BatchNDVIClick(Sender: TObject);
begin
   Sentinel2BatchOps;
end;


procedure Twmdem.Bringpointcloudcontroltofront1Click(Sender: TObject);
begin
   pt_cloud_opts_fm.BringToFront;
   pt_cloud_opts_fm.Left := Self.Left + 10;
   pt_cloud_opts_fm.Top := Self.Top + 10;
end;

procedure Twmdem.Bringslicecontroltofront1Click(Sender: TObject);
begin
   //if (SlicerForm.SliceGraph[1].GISGraph <> nil) then SlicerForm.SliceGraph[1].BringToFront;
   SlicerForm.BringToFront;
   SlicerForm.Left := Self.Left + 10;
   SlicerForm.Top := Self.Top + 10;
end;


procedure Twmdem.N3DEPfileswithtag421121Click(Sender: TObject);
begin
   FindFilesWith42112;
end;

procedure Twmdem.N3OpenDEMs1Click(Sender: TObject);
begin
   OpenHalfSecCopALOS(false);
end;

procedure Twmdem.N41Click(Sender: TObject);
begin
   {$IfDef Old3DEP}
      SummarizeVDatumShifts;
   {$EndIf}
end;

procedure Twmdem.N42Click(Sender: TObject);
begin
   DeleteFilesForATestArea;
end;

procedure Twmdem.N45Click(Sender: TObject);
var
   FilesWanted : tStringList;
   i,DEM : integer;
   fName : PathStr;
begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Clip DEMs to DEMIX tile boundaries in'); {$EndIf}
   FilesWanted := tStringList.Create;
   FilesWanted.Add(LastDEMName);
   if GetMultipleFiles('DEM to clip',DEMFilterMasks,FilesWanted ,MDDef.DefaultDEMFilter) then begin
     {$If Defined(TimeLoadDEM))} WriteLineToDebugFile('Files picked ' + IntToStr(FilesWanted.Count)); {$EndIf}
     for i := 0 to pred(FilesWanted.Count) do begin
        SetPanelText(1,IntToStr(i) + '/' + IntToStr(FilesWanted.Count));
        fName := FilesWanted.Strings[i];
        LoadNewDEM(DEM,fName,false);
        ClipTheDEMtoFullDEMIXTiles(DEM);
        CloseSingleDEM(DEM);
     end;
   end;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Clip DEMs to DEMIX tile boundaries out'); {$EndIf}
end;

procedure Twmdem.Overwrite1Click(Sender: TObject);
begin
   CreateDEMIX_GIS_database_by_transposing(True);
end;

procedure Twmdem.Overwrite2Click(Sender: TObject);
begin
   ClassificationAgreement(true);
end;

procedure Twmdem.Overwrite3Click(Sender: TObject);
begin
   AllHallucinatingDTMsforCoastalAreas(true);
end;

procedure Twmdem.Overwrite4Click(Sender: TObject);
begin
   CreateTestAreaDEMs(True);
end;

procedure Twmdem.Overwrite5Click(Sender: TObject);
begin
   DEMIX_CreateGridsFromVectors(true);
end;

procedure Twmdem.Overwrite6Click(Sender: TObject);
begin
   DEMIX_CreateGridsFromVectors(false);
end;

procedure Twmdem.Overwrite7Click(Sender: TObject);
begin
   ChannelNetworkMissPercentages(True);
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


procedure Twmdem.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   {$IfDef RecordClosing} WriteLineToDebugFile('Twmdem.FormClose in ' + BuildString  + ' dbfn=' + DebugFileName); {$EndIf}
   ShowHourglassCursor;
   SaveMDDefaults;
   {$IfDef RecordClosing} WriteLineToDebugFile('Defaults saved ' + '  dbfn=' + DebugFileName); {$EndIf}
   DEM_Manager.CloseAllWindowsAndData;
   {$IfDef RecordClosing} WriteLineToDebugFile('Windows closed ' + '  dbfn=' + DebugFileName); {$EndIf}
   Action := caFree;
   {$If Defined(RecordClosing) or Defined(RecordProblems)} WriteLineToDebugFile('Twmdem.FormClose out, normal termination build ' + BuildString  + '  dbfn=' + DebugFileName); {$EndIf}
end;

procedure Twmdem.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
   {$IfDef RecordClosing} WriteLineToDebugFile('Enter wmDEM formCloseQuery'); {$EndIf}
   ApplicationProcessMessages;
   CanClose := true;
end;


procedure Twmdem.Hardware1Click(Sender: TObject);
begin
   HardwareOnLine;
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
        {$IfDef RecordDBFconvert} WriteLineToDebugFile('ConvertDB ' + NewName); {$EndIf}
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
   ConvertDB(LandCoverSeriesFName);
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

procedure Twmdem.Graphevaluationandscores1Click(Sender: TObject);
var
   db : integer;
begin
   StopSplashing;
   LastDataBase := DEMIX_final_DB_dir;
   db := OpenMultipleDataBases('DEMIX graphs');
   if ValidDB(db) then StartDEMIXgraphs(DB);
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
   {$IfDef IncludeGeologyLabs} Afar1Click(Sender); {$EndIf}
end;

procedure Twmdem.SheepRange1Click(Sender: TObject);
begin
   {$IfDef IncludeGeologyLabs} Afar1Click(Sender); {$EndIf}
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


procedure Twmdem.ProcessVDATUMshifts1Click(Sender: TObject);
begin
   {$IfDef Old3DEP}
      DEMIX_VDatum_shifts;
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


procedure Twmdem.Python1Click(Sender: TObject);
begin
   {$IfDef IncludePython}
      StartPython;
   {$EndIf}
end;

procedure Twmdem.Pythontestrun1Click(Sender: TObject);
begin
   {$IfDef IncludePython}
      TestPythonFile;
   {$EndIf}
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
      {$IfDef RecordMGT} WriteLineToDebugFile('Twmdem.Magneticmodel1Click'); {$EndIf}
      DEMGlb[3].SelectionMap.BringToFront;
      ChangeDEMNowDoing(SeekingLeftSideMagModels);
   {$EndIf}
end;


procedure Twmdem.Makelittletilescontest1Click(Sender: TObject);
begin
   {$IfDef Include2021datafusion} MakeLittleTiles; {$EndIf}
end;


procedure Twmdem.MaskwaterinreferenceDEMs1Click(Sender: TObject);
begin
   MaskWaterInReferenceAndTestDEMs;
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


procedure Twmdem.HistogramstoCSVfiles1Click(Sender: TObject);
var
   ThePath,fName : PathStr;
   i : integer;
   TheFiles,zFiles : tStringList;
   Graph : TThisBaseGraph;
begin
   ThePath := 'H:\demix_wine_contest\';
   GetDOSPath('location of .z files',ThePath);
   TheFiles := nil;
   Petmar.FindMatchingFiles(ThePath,'*.z',TheFiles,5);
   for i := 0 to pred(TheFiles.Count) do begin
       fName := TheFiles.Strings[i];
       zFiles := tStringList.Create;
       zfiles.Add(fName);
       Graph := CreateMultipleHistogram(MDDef.CountHistograms,zFiles,nil,'','');
       Graph.ViewGraphData(ChangeFileExt(fName, '.csv'));
       Graph.Destroy;
   end;
   TheFiles.Free;
end;


procedure Twmdem.Unicodeicongenerator1Click(Sender: TObject);
begin
   FormUnicode := TFormUnicode.Create(Application);
end;


procedure Twmdem.Updatehelpfile1Click(Sender: TObject);
var
   HelpFileName : PathStr;
begin
   {$IfDef ExWebDownload}
   {$Else}
      HelpFileName := ProgramRootDir + 'microdem.chm';
      SysUtils.DeleteFile(HelpFileName);
      //DownloadFileFromWeb(WebProgramDownLoadDir + 'microdem.chm',HelpFileName);
      DownloadFileFromWeb('https://microdem.org/microdem_downloads/microdem.chm',HelpFileName);
      UnblockFile(HelpFileName);
   {$EndIf}
end;


procedure Twmdem.Updatehelpfile2Click(Sender: TObject);
begin
   Updatehelpfile1Click(Sender);
   Contents1Click(Sender);
end;


procedure Twmdem.Modesofdifferencedistributions1Click(Sender: TObject);
begin
   ModeOfDifferenceDistributions;
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
      //ClimateGetData;
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
{$IfDef ExSieve}
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


procedure Twmdem.Skipifdone1Click(Sender: TObject);
begin
   AllHallucinatingDTMsforCoastalAreas(false);
end;

procedure Twmdem.Skiipifpresent1Click(Sender: TObject);
begin
   DEMIX_MergeReferenceDEMs(false);
end;

procedure Twmdem.Skipeifpresent1Click(Sender: TObject);
begin
   DEMIX_Ref_DEM_full_chain(false);
end;

procedure Twmdem.Skipifexists3Click(Sender: TObject);
begin
   CreateDEMIX_GIS_database_by_transposing(False);
end;

procedure Twmdem.Skipifexits1Click(Sender: TObject);
begin
   DEMIX_CreateReferenceDEMs(false,ResampleModeOneSec);
end;

procedure Twmdem.Skipifexits2Click(Sender: TObject);
begin
   DEMIX_vert_datum_code := 5703;
   DEMIX_GDAL_Ref_DEM_datum_shift(false);
end;

procedure Twmdem.Skipifpresent1Click(Sender: TObject);
begin
   ClassificationAgreement(false);
end;

procedure Twmdem.Skipifpresent2Click(Sender: TObject);
begin
   ChannelNetworkMissPercentages(False);
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


procedure Twmdem.Subset81Ssidescan1Click(Sender: TObject);
begin
   {$IfDef ExSidescan}
   {$Else}
      Subset81Ssidescanfile;
   {$EndIf}
end;

procedure Twmdem.Subsetlarge3DEPareas1Click(Sender: TObject);
begin
   {$IfDef Old3DEP}
      BatchSubset_3DEP_DEMs;
   {$EndIf}
end;

procedure Twmdem.SumatraPDFhelpcontents1Click(Sender: TObject);
var
   fName,ppName : PathStr;
begin
   StopSplashing;
   fName := ChangeFileExt(Application.ExeName,'.chm');
   ppName := 'C:\Program Files\SumatraPDF\SumatraPDF.exe';
   ExecuteFile(ppName, fName, ProgramRootDir);
end;

procedure Twmdem.SummarizeverticaldatumshiftforEGM96testDEMs1Click(Sender: TObject);
begin
   SummarizeEGM96toEGM2008shifts;
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
   LoadBlankVectorMapAndOverlay(false,false);
end;


procedure Twmdem.SpeedButton3Click(Sender: TObject);
begin
   SimpleProfiles;
   LOS1Click(Sender);
end;


procedure Twmdem.MagMapButtonClick(Sender: TObject);
var
   fName : PathStr;
begin
   fName := MainMapData + 'geology\geology_grids\EMAG2_V2.tif';
   if FileExists(fName) then begin
      OpenNewDEM(fName,false);
      DEMGlb[LastDEMLoaded].DEMheader.ElevUnits := euNanotesla;
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
   CoordsPopUpMenu7.Popup(Mouse.CursorPos.X,Mouse.CursorPos.Y);
end;


procedure Twmdem.COPALOScomparetoreference1Click(Sender: TObject);
begin
   CopAlosCompareReference;
end;


procedure Twmdem.COPALOShighlowgeomorphometry1Click(Sender: TObject);
begin
   HighLowCopAlosGeomorphometry;
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


procedure Twmdem.Copyfile1Click(Sender: TObject);
begin
   CopyFiles(true);
end;

procedure Twmdem.Movefileswithnamematch1Click(Sender: TObject);
begin
   CopyFiles(False);
end;


procedure Twmdem.MovereferenceDSMs1Click(Sender: TObject);
begin
   MoveReferenceDSMs;
end;

procedure Twmdem.Correlationmatrix1Click(Sender: TObject);
var
   fName : PathStr;
   TStr : ShortString;
begin
   StopSplashing;
   fName := '';
   if (Sender = Correlationmatrix1) then TStr := 'Correlation'
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


procedure Twmdem.Creatediffrencemaps1Click(Sender: TObject);
begin
   CreateDifferenceMaps;
end;


procedure Twmdem.CreatefinalDB1Click(Sender: TObject);
begin
   CreateFinalDB;
end;

procedure Twmdem.CreatehalfsecondreferenceDEMs1Click(Sender: TObject);
begin
   DEMIXCreateHalfSecRefDEMs;
end;

procedure Twmdem.Createlandcovergrids1Click(Sender: TObject);
begin
   CreateLandCoverGrids;
end;

procedure Twmdem.CreaterangereferenceDEMs1Click(Sender: TObject);
begin
   DEMIX_CreateReferenceDEMs(false,ResampleModeRange);
end;

procedure Twmdem.CreatetestareaDEMSskipifexists1Click(Sender: TObject);
begin
   CreateTestAreaDEMs(False);
end;

procedure Twmdem.ImportCTDfile1Click(Sender: TObject);
begin
   {$IfDef ExOceanography}
   {$Else}
      OceanCal.ImportCTDfile;
   {$EndIf}
end;


procedure Twmdem.InOutButtonClick(Sender: TObject);
begin
   Data1Click(Sender);
end;

procedure Twmdem.InsureallreferenceDTMscorrectlynamed1Click(Sender: TObject);
var
   TheFiles : tStringList;
   i : integer;
   fName,NewName : PathStr;
begin
   TheFiles := Nil;
   FindMatchingFiles(DEMIX_Ref_1sec,'*.tif',TheFiles);
   for i := 0 to pred(TheFiles.Count) do begin
      fName := theFiles.Strings[i];
      if (not StrUtils.AnsiContainsText(fName,'_dtm')) and (not StrUtils.AnsiContainsText(fName,'_dsm')) then begin
         if StrUtils.AnsiContainsText(fName,'_point') then NewName := StringReplace(fName,Ref1SecPointStr,'_dtm'+Ref1SecPointStr,[rfReplaceAll, rfIgnoreCase])
         else NewName := StringReplace(fName,Ref1SecAreaStr,'_dtm' + Ref1SecAreaStr,[rfReplaceAll, rfIgnoreCase]);
         SysUtils.RenameFile(fName,NewName);
      end;
   end;
   TheFiles.Free;
end;

procedure Twmdem.IntDBSpeedButtonClick(Sender: TObject);
begin
   {$IfDef ExIndexes}
   {$Else}
      VectorMapButtonClick(Sender);
   {$EndIf}
end;


procedure Twmdem.Inventory3DEPtiles1Click(Sender: TObject);
begin
    Inventory3DEPtiles;
end;

procedure Twmdem.InventoryallDEMIXdatafiles1Click(Sender: TObject);
begin
   InventoryAllDEMIXdata;
end;

procedure Twmdem.Inventorychanneldatabyarea1Click(Sender: TObject);
begin
   {$IfDef DEMIX_SAGA_channels} InventoryChannelDataByArea; {$EndIf}
end;

procedure Twmdem.Inventorydifferencestats1Click(Sender: TObject);
begin
   InventoryDEMIXdifferenceStats;
end;


procedure Twmdem.InventoryDILUVIUMbytestarea1Click(Sender: TObject);
begin
   CheckLowElevationAreas;
end;

procedure Twmdem.InventorySSIMFUVCSVfiles1Click(Sender: TObject);
begin
   InventoryDEMIX_SSIM_FUV_Stats;
end;

procedure Twmdem.InventorytestandrefereneDEMsbytestarea1Click(Sender: TObject);
begin
   //InventoryTestAndReferenceDEMs;
end;

procedure Twmdem.InventoryWbWfilesbyarea1Click(Sender: TObject);
begin
    InventoryWbWSaagaMDsavedGridsByArea;
end;

procedure Twmdem.Italyfocalmechs1Click(Sender: TObject);
begin
   {$IfDef IncludeGeologyLabs} Afar1Click(Sender); {$EndIf}
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
            ShowHourglassCursor;
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
   {$IfDef RecordMGT} WriteLineToDebugFile('Twmdem.MGTMagModelSpeedButtonClick'); {$EndIf}
   Magneticmodel1Click(Sender);
end;


procedure Twmdem.MH370region1Click(Sender: TObject);
begin
   {$IfDef IncludeGeologyLabs} Afar1Click(Sender); {$EndIf}
end;

procedure Twmdem.MICRODEMgridformat1Click(Sender: TObject);
begin
    ViewHeaderRecord(0);
end;

procedure Twmdem.SpeedButton4Click(Sender: TObject);
begin
   {$IfDef RecordMenu} WriteLineToDebugFile('Twmdem.SpeedButton4Click in'); {$EndIf}
   StopSplashing;
   OutsideCSVImport := true;
   OpenMultipleDataBases('');
   OutsideCSVImport := false;
   {$IfDef RecordMenu} WriteLineToDebugFile('Twmdem.SpeedButton4Click out'); {$EndIf}
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
      {$IfDef RecordProblems} WriteLineToDebugFile('Options change ProgramMode=' + IntToStr(ord(MDdef.ProgramOption ))); {$EndIf}
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

procedure Twmdem.Overwirte1Click(Sender: TObject);
begin
   DEMIX_Ref_DEM_full_chain(True);
end;

procedure Twmdem.Overwirte2Click(Sender: TObject);
begin
   ComputeDEMIX_Diff_Dist_tile_stats(true);
end;

procedure Twmdem.Overwirte3Click(Sender: TObject);
begin
   ComputeDEMIX_Diff_Dist_tile_stats(false);
end;

procedure Twmdem.Overwirte4Click(Sender: TObject);
begin
   DEMIX_MergeReferenceDEMs(true);
end;

procedure Twmdem.Overwirteifexists1Click(Sender: TObject);
begin
   DEMIX_CreateReferenceDEMs(true,ResampleModeOneSec);
end;

procedure Twmdem.Overwriteifexists1Click(Sender: TObject);
begin
    DEMIX_CreateReferenceDEMsFromSource(true);
end;

procedure Twmdem.Overwriteifexists2Click(Sender: TObject);
begin
   DEMIX_CreateReferenceDEMsFromSource(false);
end;

procedure Twmdem.Overwriteifexists3Click(Sender: TObject);
begin
   //DEMIXRef_DEM_create_full_chain(true);
end;

procedure Twmdem.Overwriteifexists4Click(Sender: TObject);
begin
   //DEMIXRef_DEM_create_full_chain(false);
end;



procedure Twmdem.Overwriteifexits1Click(Sender: TObject);
begin
   DEMIX_MergeReferenceDEMs(True);
end;

procedure Twmdem.Overwriteifexits2Click(Sender: TObject);
begin
   DEMIX_MergeReferenceDEMs(false);
end;

procedure Twmdem.Overwriteifexits3Click(Sender: TObject);
begin
   DEMIX_vert_datum_code := 5703;
   DEMIX_GDAL_Ref_DEM_datum_shift(true);
end;


procedure Twmdem.Exitprogram2Click(Sender: TObject);
begin
   {$IfDef RecordClosing} WriteLineToDebugFile('Twmdem.Exitprogram2Click Clicked exit program from pop up menu'); {$EndIf}
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


procedure Twmdem.Extract1Click(Sender: TObject);
begin
   {$IfDef AllowEDTM} ExtractEDTMforTestAreas; {$EndIf}
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
begin
   {$IfDef ExMovie}
   {$Else}
      StopSplashing;
      CreateNewMovie;
   {$EndIf}
end;

procedure Twmdem.Existingfile2Click(Sender: TObject);
begin
   {$IfDef ExMovie}
   {$Else}
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


procedure Twmdem.OpenDEMIXdatabase1Click(Sender: TObject);
begin
   OpenDEMIXDatabaseForAnalysis;
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


procedure Twmdem.Onedegreetilestocovertestareas1Click(Sender: TObject);
begin
   OneDegreeTilesToCoverTestAreas;
end;

procedure Twmdem.Onlinehelp1Click(Sender: TObject);
begin
   {$IfDef AllowUSNAhelp}
      ExecuteFile('https://www.usna.edu/Users/oceano/pguth/md_help/html/microdem.htm', '', '');
   {$EndIf}
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

procedure Twmdem.Pickdatadirectory1Click(Sender: TObject);
begin
   OpenHalfSecCopALOS(True);
end;

procedure Twmdem.Pixelbypixelmapstatistics1Click(Sender: TObject);
begin
   PixelByPixelCopAlos;
end;

procedure Twmdem.Perpendicularshortprofilesthroughpoint1Click(Sender: TObject);
begin
   ChangeDEMNowDoing(SeekingPerpendicularProfiles);
end;

procedure Twmdem.Perspective1Click(Sender: TObject);
begin
   PerspectiveButtonClick(Sender);
end;


procedure Twmdem.Slidesorter1Click(Sender: TObject);
begin
   StartSlideSorter;
end;


procedure Twmdem.NLCD20011Click(Sender: TObject);
var
   Path : PathStr;
   tf : tStringList;
   TStr : string35;
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


procedure Twmdem.Noaddedlegends1Click(Sender: TObject);
begin
   AllGraphsOneImage;
end;

procedure Twmdem.Noaddedlegends2Click(Sender: TObject);
begin
   AllGraphsOneImage(-99,true);
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
      {$IfDef RecordGeostats} WriteLineToDebugFile(fName); {$EndIf}
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


procedure Twmdem.Reference1secDTMsfromCanadianlidar1Click(Sender: TObject);
begin
   DEMIX_CreateReferenceDEMsFromSource(true);
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


procedure Twmdem.rimreferencedatatoDEMIXtiles1Click(Sender: TObject);
begin
   TrimReferenceDEMsToDEMIXtiles;
end;

procedure Twmdem.Trenchgeometry1Click(Sender: TObject);
begin
   {$IfDef IncludeGeologyLabs} Afar1Click(Sender); {$EndIf}
end;

procedure Twmdem.Triplejunctions1Click(Sender: TObject);
begin
   PlateRotateSpeedButtonClick(Sender);
end;

procedure Twmdem.Circlearound1Click(Sender: TObject);
begin
   ChangeDEMNowDoing(SeekingFirstCircleFly);
end;

procedure ListSubdirs(FullPath : boolean);
var
  theList : tStringList;
  I: Integer;
begin
   theList := tStringList.Create;
   if GetMultipleDirectories('sub directories',TheList) then begin
      if (not FullPath) then begin
         for I := 0 to pred(TheList.Count) do begin
            TheList.Strings[i] := LastSubDir(TheList.Strings[i]);
         end;
      end;
      DisplayAndPurgeStringList(TheList,'sub directories');
   end
   else TheList.Destroy;
end;

procedure Twmdem.Listofsubdirectoriesfullpaths1Click(Sender: TObject);
begin
   ListSubdirs(true);
end;

procedure Twmdem.Listofsubdirectoriesrelativenames1Click(Sender: TObject);
begin
   ListSubdirs(false);
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



procedure Twmdem.Satellitepredictions1Click(Sender: TObject);
begin
    {$IfDef ExTrackSat}
    {$Else}
       wmdem.Vectormap1Click(Sender);
       VectorMap[LastVectorMap].Caption := 'Satellite predictions';
       //VectorMap[LastVectorMap].Closable := false;
       StartSatelliteTracking(VectorMap[LastVectorMap]);
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
   {$IfDef Include2021datafusion} Experimental_MD.MedianOfSatelliteData; MakeLittleTiles; {$EndIf}
end;

procedure Twmdem.Megathrusts1Click(Sender: TObject);
begin
   {$IfDef IncludeGeologyLabs} Afar1Click(Sender); {$EndIf}
end;

procedure Twmdem.Mercator1Click(Sender: TObject);
begin
   NLCD20011Click(Sender);
end;

procedure Twmdem.Merge1secreferenceDEMsfromVisioterra1Click(Sender: TObject);
begin
   //DEMIX_merge_Visioterra_source;
end;

procedure Twmdem.Mergechannelnetworkevaluations1Click(Sender: TObject);
begin
   MergeCSV(3);
end;


procedure Twmdem.MergeDEMIXtilestats1Click(Sender: TObject);
begin
   MergeDEMIXtileStats;
end;


procedure Twmdem.Mergegeomorphonevaluatioins1Click(Sender: TObject);
begin
   MergeCSV(4);
end;

procedure Twmdem.Mergemasp1Click(Sender: TObject);
begin
   {$If Defined(ExGDAL) or Defined(ExGeoPDF)}
   {$Else}
      GDALconvertGeoPDF(gdalMergeGeoPDF1);
   {$EndIf}
end;


procedure Twmdem.MergemultipleTXTCSVintoDB1Click(Sender: TObject);
begin
   MergeMultipleCSVorTextFiles;
end;

procedure Twmdem.Mergeridgesandvalleys1Click(Sender: TObject);
begin
   MergeCSV(5);
end;

procedure Twmdem.Mergesourcedatatiles1Click(Sender: TObject);
begin
   DEMIX_merge_source;
end;

procedure Twmdem.MergeSSIMandR2database1Click(Sender: TObject);
begin
    //MergeSSIMandR2DB;
end;

procedure Twmdem.MergeSSIMFUV1Click(Sender: TObject);
begin
   if MDDef.DoFUV then MergeCSV(1);
   if MDDef.DoSSIM then MergeCSV(2);
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
               {$IfDef RecordGeostats} WriteLineToDebugFile('Grid existed ' + IntToStr(ParamDEMs[i]);: {$EndIf}
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
                  ElevUnits  := euUndefined;
                  AllocateDEMMemory(InitDEMMissing);
                  DefineDEMVariables(true);
                  {$IfDef RecordGeostats} WriteLineToDebugFile('New grid created ' + IntToStr(ParamDEMs[i])); {$EndIf}
               end;
            end;
         end;


      procedure DoPart(FirstDEM,LastDEM : integer);
      var
         Lat,Long : float64;
         xg,yg : float32;
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
   {$IfDef RecordGeostats} WriteLineToDebugFile('Twmdem.MergewavelengthheightDBFs1Click in'); {$EndIf}
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

   {$IfDef RecordGeostats} WriteLineToDebugFile('found fields'); {$EndIf}

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
   MetaDataPopupMenu.PopUp(Mouse.CursorPos.X,Mouse.CursorPos.Y);
end;


procedure Twmdem.Metadata2Click(Sender: TObject);
begin
   MetaData1Click(Sender);
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
   {$IfDef IncludeGeologyLabs} Afar1Click(Sender); {$EndIf}
end;


procedure Twmdem.Geology2Click(Sender: TObject);
begin
   {$If Defined(ExGeology) or Defined(ExLabDownloads)}
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
          OutName := GDALinfoOutputFName(fName);
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

procedure Twmdem.GetrangesforSSIMhydro1Click(Sender: TObject);
begin
   GetRangesForSSIM;
end;

procedure Twmdem.OpenGeoPDF1Click(Sender: TObject);
begin
   {$If Defined(ExGDAL) or Defined(ExGeoPDF)}
   {$Else}
      GDALconvertGeoPDF(gdalOpenGeoPDF1);
   {$EndIf}
end;


procedure Twmdem.OpenGeoPDFimagelayer1Click(Sender: TObject);
begin
   {$If Defined(ExGDAL) or Defined(ExGeoPDF)}
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
   {$IfDef RecordMenu} WriteLineToDebugFile('Twmdem.Openimagewithmultiplebands1Click in'); {$EndIf}
    Bands := tStringList.Create;
    Bands.Add(LastImageName);
    GetMultipleFiles('Multiple Bands',GetSatMaskList(true),Bands,MDDef.DefaultSatFilter);
    Bands.Sorted := true;
    LastImageName := Bands.Strings[0];
    OpenAndDisplaySatelliteScene(Bands,'',true,true,true);
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


procedure Twmdem.OpenmapsforDEMIXtestarea1Click(Sender: TObject);
begin
   OpenDEMIXAreaMaps;
end;

procedure Twmdem.Openmultigrids1Click(Sender: TObject);
begin
   {$IfDef ExMultigrid}
      OpenMultigridByDirectory;
   {$EndIf}
end;


procedure Twmdem.Atlantis1Click(Sender: TObject);
begin
   {$IfDef IncludeGeologyLabs} Afar1Click(Sender); {$EndIf}
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



procedure Twmdem.Micronetquiz1Click(Sender: TObject);
begin
   MicronetQuiz;
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

procedure Twmdem.SSIMR21Click(Sender: TObject);
begin
   FUV_SSIM_work;
end;

procedure Twmdem.LOS2Click(Sender: TObject);
begin
   SimpleProfiles;
   ChangeDEMNowDoing(MultipleLOS);
end;


procedure Twmdem.MultipledNBRmaps1Click(Sender: TObject);
begin
   Sentinel2Batch_dNBR;
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

procedure Twmdem.DiluviumDEMandDEMIXDBoverlap1Click(Sender: TObject);
var
   fName1,fName2 : Pathstr;
   sl1,sl2,sl3 : tStringList;
   tile : shortstring;
   i : Integer;
begin
   fName1 := DEMIXSettingsDir + 'tiles_demix2.6.txt';
   fName2 := DEMIXSettingsDir + 'tiles_diluvium_filled.txt';
   sl1 := tStringList.Create;
   sl1.LoadFromFile(fName1);
   sl2 := tStringList.Create;
   sl2.LoadFromFile(fName2);
   sl3 := tStringList.Create;
   for i := 0 to pred(sl1.Count) do begin
      Tile := sl1.Strings[i];
      if sl2.IndexOf(Tile) >= 0 then sl3.Add(tile);
   end;
   DisplayAndPurgeStringList(sl3,'Common tiles=' + IntToStr(sl3.Count));
   sl1.Destroy;
   sl2.Destroy;
end;

procedure Twmdem.DiluviumDEMfortestareas1Click(Sender: TObject);
begin
   DiluviumDTMforTestAreas(false);
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
     {$IfDef RecordDragonPlot} WriteLineToDebugFile('Twmdem.DragonPlot1Click'); {$EndIf}
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
   {$IfDef IncludeGeologyLabs}
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
         if (PredAgesDEM <> 0) then DEMGlb[PredAgesDEM].SetUpMap(PredAgesDEM,false,mtElevSpectrum);
      end;
   {$EndIf}
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
   {$If Defined(ExGeology) or Defined(ExLabDownloads)}
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
   {$If Defined(ExGeology) or Defined(ExLabDownloads)}
   {$Else}
      if not ValidDEM(SedThickDEM) then begin
         GeologyGetData;
         //if not FileExists(SedThickFile) then DownloadFileFromWeb(WebDataDownLoadDir + ExtractFileName(SedThickFile),SedThickFile);
         SedThickDEM := OpenNewDEM(SedThickFile,false);
         if (SedThickDEM <> 0) then DEMGlb[LastDEMLoaded].SetUpMap(LastDEMLoaded,false,mtElevSpectrum);
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
   //GetETOPO1;
   PredBathySpeedButtonClick(Nil);
   DEMGlb[LastDEMLoaded].SelectionMap.Globaltectonicsmap1Click(Nil);
end;

procedure Twmdem.NASABlueMarbleSpeedButtonClick(Sender: TObject);
var
  NewSatImage : integer;
begin
   {$IfDef ExSat}
   {$Else}
      //GetBlueMarble;
      NewSatImage := OpenAndDisplaySatelliteScene(nil,BlueMarblefName,true,true,false);
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
   {$IfDef AllowUSNAdataDownloads}  GetNaturalEarthData(True); {$EndIf}
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

   db := OpenMultipleDataBases('training set points');
   dbName := GISdb[db].dbFullName;
   CloseAndNilNumberedDB(db);

   TheFiles := Nil;
   if Sender = Addnormaliziedstatsforblockgridstotrainingset1 then GridDir := GridDir + 'NORMALIZED\';

   Petmar.FindMatchingFiles(GridDir,'*.dem',TheFiles,0);
   StartThreadTimers('Stat',1);
   for i := 0 to pred(TheFiles.Count) do begin
      ThreadTimers.OverallGauge9.Progress := round(100 * i/TheFiles.Count);
      fName := TheFiles.Strings[i];
      {$IfDef RecordGeostats}     WriteLineToDebugFile(fName,true); {$EndIf}
      aDEM := OpenNewDEM(fName);
      DEMGlb[aDEM].SelectionMap.OpenDBonMap('',dbname);
      {$IfDef RecordGeostats}   WriteLineToDebugFile(DEMGlb[aDEM].AreaName); {$EndIf}
      GISdb[db].AddAndFillFieldFromDEM(adElevNearest,ptTrim(DEMGlb[aDEM].AreaName));
      MomentVar := DEMGlb[aDEM].ElevationMoments(DEMGlb[aDEM].FullDEMGridLimits);
      Results.Add(ptTrim(DEMGlb[aDEM].AreaName) + ',' +
                  RealToString(MomentVar.mean,-12,-4) + ',' + RealToString(MomentVar.std_dev,-12,-4) + ',' +
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
   ConvertForm : TCoordConvertForm;
begin
   ConvertForm := TCoordConvertForm.Create(Application);
   if (Sender = UKOSgrid1) then begin
      ConvertForm.This_projection.PName := UK_OS;
      ConvertForm.Caption := 'United Kingdon Ordnance Survey converter';
   end;
   if (Sender = UTMprojection1) then begin
      ConvertForm.Caption := 'UTM projection converter';
      ConvertForm.This_projection.PName := UTMellipsoidal;
      ConvertForm.This_projection.h_DatumCode := MDDef.PreferPrimaryDatum;  //'WGS84';
      ConvertForm.This_projection.projUTMZone := MDDef.DefaultUTMZone;
   end;
   if (Sender = Guam1) then begin
      ConvertForm.Caption := 'Guam converter';
      ConvertForm.This_projection.PName := AzimuthalEquidistantEllipsoidal;
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


procedure Twmdem.Fixtileswith42114foottag1Click(Sender: TObject);
begin
   FixFilesWith42112;
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

procedure Twmdem.UTMprojection1Click(Sender: TObject);
begin
   FinnishGaussKruger1Click(Sender);
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

procedure Twmdem.Partialprocessing1Click(Sender: TObject);
begin

end;

(*
procedure Twmdem.Maxwellplanning1Click(Sender: TObject);
var
   dName,pName : PathStr;
begin
   dName := 'ches_bay_sed';
   DownloadandUnzipDataFileIfNotPresent(dName);
   pName := MainMapData + dName + '\ches_bay_noaa_est_bathy.dem';
   LastDEMLoaded := OpenNewDEM(pName);
end;
*)

procedure Twmdem.FormDestroy(Sender: TObject);
begin
   OnResize := nil;
end;


procedure Twmdem.FormKeyDown(Sender: TObject; var Key: Word;  Shift: TShiftState);
begin
   if (Key =  VK_F1) then begin
      {$IfDef RecordProblems} WriteLineToDebugFile(' Twmdem.FormKeyDown, Key =  VK_F1'); {$EndIf}
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
   i,Changed : integer;
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
      Changed := 0;
      if GetMultipleFiles('Change file name','AnyFile|*.*',FilesWanted, DefaultFilter) then begin
         Petmar.GetString('phrase to replace',NameContains,false,ValidDosFileNameChars);
         Petmar.GetString('replacement phrase',ChangeTo,false,ValidDosFileNameChars);
         ShowHourglassCursor;
         BaseName := ExtractFilePath(FilesWanted.Strings[0]);
         for i := 0 to pred(FilesWanted.Count) do begin
            fName := FilesWanted.Strings[i];
            if StrUtils.AnsiContainsText(fName,NameContains) then begin
            inc(Changed);
               NewName := BaseName + StringReplace(ExtractFileName(fName),NameContains,ChangeTo,[rfReplaceAll, rfIgnoreCase]);
               RenameFile(fName,NewName);
            end;
         end;
      end;
      FilesWanted.Free;
      ShowDefaultCursor;
   until not AnswerIsYes('Changed ' + IntToStr(Changed) + ' file names.  Change more files');
end;


procedure Twmdem.Opengl1tofrontClick(Sender: TObject);
begin
   {$IfDef ExFMX3D}
   {$Else}
      if (Map3D <> Nil) then begin
         Map3D.BringToFront;
         Map3D.Left := Self.Left + 10;
         Map3D.Top := Self.Top + 10;
      end;
      if (View3DForm <> Nil) then begin
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


procedure Twmdem.VerifytestDEMcoverages1Click(Sender: TObject);
begin
    VerifyTestDEMcoverages;
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

procedure Twmdem.Viewlastexectiondebuglog1Click(Sender: TObject);
begin
   {$IfDef RecordProblems}
      StopSplashing;
      if (TheDebugLog <> Nil) then ShowInNotepadPlusPlus(ExtractFilePath(DebugFilename) + 'last_MD_debug_file.txt','Last execution debug log (' + IntToStr(TheDebugLog.Count) + ' lines)')
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
   {$IfDef RecordMenu} WriteLineToDebugFile('Twmdem.OpenScannedmap1Click (from Open menu)'); {$EndIf}
   PickAndOpenImagery(itDRG);
end;


procedure Twmdem.OpenSentinel1radarimagery1Click(Sender: TObject);
begin
   OpenSentinel1Radar;
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


procedure Twmdem.Landformcategorieslegends1Click(Sender: TObject);
begin
   LandCoverBarGraphLegends;
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
   //AnnapolisTM8scene1Click(Sender);
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
   ChangeOptions;
   if AnswerIsYes('Restart initialize program') then begin
      FirstRun := true;
      FormActivate(Sender);
   end;
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


procedure Twmdem.Open4elevationrangeDEMIXDBs1Click(Sender: TObject);
begin
   StartDEMIXgraphs(-4);
end;

procedure MergeDEMs(Mode : integer);
var
   UseGDALvrt : boolean;
   DEMList,NewFiles : tStringList;
   i, NewDEM : integer;
   aPath : PathStr;
begin
   {$If Defined(RecordMenu) or Defined(RecordMerge)} WriteLineToDebugFile('Enter MergeDEMs, mode=' + IntToStr(Mode)); {$EndIf}
   UseGDALvrt := Mode in [dmMergeGDAL,dmMergeDirectories];
   DEMList := tStringList.Create;
   MergeSeriesName := '';
   if Mode in [dmMergeGDAL, dmMergeMDnative] then begin
      DEMList.Add(LastDEMName);
      if Petmar.GetMultipleFiles('DEMs to merge',DEMFilterMasks,DEMList,MDDef.DefaultDEMFilter) then begin
         {$IfDef RecordMenu} WriteStringListToDebugFile(DEMList); {$EndIf}
         if (DEMList.Count = 1) and (UpperCase(ExtractFileExt(DEMList.Strings[0])) <> '.ASC') then begin
            NewDEM := OpenNewDEM(DEMList.Strings[0]);
            DEMList.Destroy;
         end
         else begin
            NewDEM := MergeMultipleDEMsHere(DEMList,true,UseGDALvrt);
         end;
      end;
   end
   else begin
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
      until (aPath = '');
      if (DEMList.Count > 0) then begin
         NewDEM := MergeMultipleDEMsHere(DEMList,true,UseGDALvrt);
      end;
   end;
   StopSplashing;
   WMDEM.SetMenusForVersion;
   {$IfDef TrackDEMCorners} DEMGlb[NewDEM].WriteDEMCornersToDebugFile('Merge DEMs, mode=' + IntToStr(Mode)); {$EndIf}
   {$If Defined(RecordMenu) or Defined(RecordMerge)} WriteLineToDebugFile('Exit MergeDEMs, mode=' + IntToStr(Mode)); {$EndIf}
end;


procedure Twmdem.OpenandmergeDEMdirectories1Click(Sender: TObject);
begin
   MergeDEMs(dmMergeDirectories);
end;


procedure Twmdem.OpenandmergeDEMs1Click(Sender: TObject);
begin
   MergeDEMs(dmMergeGDAL);
end;

procedure Twmdem.OpenandmergeDEMsgridsverylarge1Click(Sender: TObject);
begin
   MergeDEMs(dmMergeMDnative);
end;

procedure Twmdem.OpenandmergeDEMswithfullDEMIXcoverage1Click(Sender: TObject);
begin
   //MergeDEMsForDEMIX;
end;

procedure Twmdem.OpenandmergeGeotiffs1Click(Sender: TObject);
{$IfDef ExGDAL}
begin
{$Else}
var
   MergefName : PathStr;
begin
   MergeFName := '';
   CallGDALMerge(MergefName,Nil);
   OpenAndDisplaySatelliteScene(Nil,MergefName,true,true,true);
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
   OpenMultipleDataBases('');
   {$IfDef RecordMenu} WriteLineToDebugFile('Twmdem.OpenDatabase1Click out'); {$EndIf}
end;

procedure Twmdem.OpenDatabasewithoutmap1Click(Sender: TObject);
begin
   OpenDatabase1Click(Sender);
end;


procedure Twmdem.CoastalDEMfortestareas1Click(Sender: TObject);
begin
   CoastalDTMforTestAreas(false);
end;

procedure Twmdem.Combineallcombinedimages1Click(Sender: TObject);
begin
   CombineAllPanelGraphs;
end;


procedure Twmdem.Compareconvergenceindexfortestarea1Click(Sender: TObject);
begin
   OpenCopDEMandLandcoverForArea(false);
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

procedure Twmdem.ComputeDEMIXtilestats1Click(Sender: TObject);
begin
   ComputeDEMIX_Summary_stats_DB;
end;

procedure Twmdem.Openshapefilemap1Click(Sender: TObject);
begin
   {$IfDef RecordOpenVectorMap} WriteLineToDebugFile('Twmdem.Openshapefilemap1 in'); {$EndIf}
   LoadBlankVectorMapAndOverlay(false,false);
end;

procedure Twmdem.OpensingleLandsatband1Click(Sender: TObject);
begin
   TreatThisAsSingleTif := true;
   PickAndOpenImagery(ItSat);
end;


initialization
   {$IfDef MessageStartup} MessageToContinue('start wmaindem initialization'); {$EndIf}
   WMDEM := Nil;
   FirstRun := true;
   SkipMenuUpdating := false;
   LockStatusBar := false;
   ClosingEverything := false;
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
   {$IfDef FullDrainageBasinStats} WriteLineToDebugFile('FullDrainageBasinStats active in Wmaindem'); {$EndIf}
   {$IfDef DrainageBasinStats} WriteLineToDebugFile('DrainageBasinStats active in Wmaindem'); {$EndIf}
   {$IfDef RecordUpdate} WriteLineToDebugFile('RecordUpdateProblem active in WMainDEM'); {$EndIf}
   {$IfDef RecordButton} WriteLineToDebugFile('RecordButtonProblems active in WMainDEM'); {$EndIf}
   {$IfDef RecordOpenVectorMap} WriteLineToDebugFile('RecordOpenVectorMap active in WMainDEM'); {$EndIf}
   {$IfDef RecordHelp} WriteLineToDebugFile('RecordHelp active in WMainDEM'); {$EndIf}
   {$IfDef RecordGeostats} WriteLineToDebugFile('RecordGeostats active in WMainDEM'); {$EndIf}
   {$IfDef RecordOceanography} WriteLineToDebugFile('RecordOceanography active in WMainDEM'); {$EndIf}
   {$IfDef Record3D} WriteLineToDebugFile('Record3DProblems active in WMainDEM'); {$EndIf}
   {$IfDef RecordCartography} WriteLineToDebugFile('RecordCartography active in WMainDEM'); {$EndIf}
   {$IfDef RecordSatLoad} WriteLineToDebugFile('RecordSatLoad active in WMainDEM'); {$EndIf}
   {$IfDef RecordClosing} WriteLineToDebugFile('WMainDEM finalization complete'); {$EndIf}
end.


