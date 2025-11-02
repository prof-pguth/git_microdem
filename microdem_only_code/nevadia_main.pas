unit nevadia_main;
//name of this main program unit goes back to a time when there were multiple related programs namde for Cambrian fossils
//over time all but this variant were either merged in, or retired

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{________________________________________}


{$I nevadia_defines.inc}

//{$Define IncludePython}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   {$IFDEF DEBUG}
      //{$Define RecordBatch}
      //{$Define RecordCommandLine}
      //DEMIX conditionals
          {$Define RecordDEMIX}
          //{$Define RecordDEMIXFull}
          //{$Define RecordDEMIXMakeTest}
          {$Define TimeMakeMaps}
          {$Define RecordGridOpenProblems}
          //{$Define DEMIXtrackFUV}
          //{$Define TrackMissingPercentages}
          //{$Define RecordRefDTM}
          //{$Define RecordDEMIXMakeRef}
          //{$Define RecordDEMIXMakeRefFull}
          {$Define RecordDEMIXDatumShift}
          {$Define RecordDEMIXDatumShiftFull}
          //{$Define RecordFUVcreateFull}
          {$Define TrackOpenOneDEM}
      //{$Define RecordMerge}
     //{$Define TrackVerticalDatum
      //{$Define RecordDragonPlot}
      //{$Define RecordDEMIXLoops}
      //{$Define RecordDEMIXGridCompare}
      //{$Define TrackDEMIX_DEMs}
      //{$Define RecordDEMIXLoad}
      //{$Define RecordMenu}
      //{$Define TrackFormCreate}
      //{$Define RecordSatLoad}
      //{$Define RecordFileOps}
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
    Openpointclouds1: TMenuItem;
    LatlongofPLSSlocation1: TMenuItem;
    Batchprocessing1: TMenuItem;
    MergewavelengthheightDBFs1: TMenuItem;
    Identifydirectory1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    N11: TMenuItem;
    //UTM1: TMenuItem;
    OpenandmergeDEMs1: TMenuItem;
    //ImportCTDfile1: TMenuItem;
    EXIFmetadata1: TMenuItem;
    EXIFimage1: TMenuItem;
    Clustergrids1: TMenuItem;
    Copyfile1: TMenuItem;
    //XYZshapefile1: TMenuItem;
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
   // Mercator1: TMenuItem;
    //Southpolarstereographic1: TMenuItem;
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
    Sealevelrise1: TMenuItem;
    N29: TMenuItem;
    Ages1: TMenuItem;
    Magneticanomaliesgrid1: TMenuItem;
    Sedimenttypegrid1: TMenuItem;
    Sedimentthicknessgrid1: TMenuItem;
    Fontsinstalled1: TMenuItem;
    Unicodeicongenerator1: TMenuItem;
    //UKOSgrid2: TMenuItem;
    Nyquist1: TMenuItem;
    Onlinehelp1: TMenuItem;
    Introductorytutorials1: TMenuItem;
    N30: TMenuItem;
    OpenandmergeDEMdirectories1: TMenuItem;
    SpeedButton6: TSpeedButton;
    //LatLong1: TMenuItem;
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
    Lidarandbeacherosion1: TMenuItem;
    Closewindows1: TMenuItem;
    Mapprojectionsanddistortion1: TMenuItem;
    Monthlywinds1: TMenuItem;
    GeographyPopupMenu: TPopupMenu;
    Koppen1: TMenuItem;
    Climatestationsforclimographs1: TMenuItem;
    Classificationmap1: TMenuItem;
   // Hurricanes1: TMenuItem;
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
    Spectrallibrary3: TMenuItem;
    N19: TMenuItem;
    RGBcolorlayers1: TMenuItem;
    GDALSRSinfo1: TMenuItem;
    WhiteboxGeotiff1: TMenuItem;
    Guam1: TMenuItem;
    Geotiff2: TMenuItem;
    GDALWKT1: TMenuItem;
    N20: TMenuItem;
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
    //BatchNDVI: TMenuItem;
    HistogramstoCSVfiles1: TMenuItem;
    Bringslicecontroltofront1: TMenuItem;
    Bringpointcloudcontroltofront1: TMenuItem;
    Viewlastexectiondebuglog1: TMenuItem;
    //COPALOScomparetoreference1: TMenuItem;
    Pixelbypixelmapstatistics1: TMenuItem;
    //COPALOShighlowgeomorphometry1: TMenuItem;
    Metadata2: TMenuItem;
    N37: TMenuItem;
    //Pickdatadirectory1: TMenuItem;
    //N3OpenDEMs1: TMenuItem;
    OpenandmergeDEMsgridsverylarge1: TMenuItem;
    //Creatediffrencemaps1: TMenuItem;
    Mergesourcedatatiles1: TMenuItem;
    ProcessVDATUMshifts1: TMenuItem;
    Processdifferencestatisticspertile1: TMenuItem;
    //VDATUMshiftinUSA1: TMenuItem;
    DEMIX2: TMenuItem;
    OpenDEMIXdatabase1: TMenuItem;
    Addversionnumbertoallfilesinapath1: TMenuItem;
    DEMIXdbCreatePopupMenu: TPopupMenu;
    //Fullprocessingchain1: TMenuItem;
    Perpendicularshortprofilesthroughpoint1: TMenuItem;
    //N41: TMenuItem;
    SummarizeverticaldatumshiftforEGM96testDEMs1: TMenuItem;
    GDALshiftFor3DEP1: TMenuItem;
    //OpenandmergeDEMswithfullDEMIXcoverage1: TMenuItem;
    Subsetlarge3DEPareas1: TMenuItem;
    Create3DEP1secrefDEMs1: TMenuItem;
    Merge3DEPreferenceDEMsbyarea1: TMenuItem;
    //CreatehalfsecondreferenceDEMs1: TMenuItem;
    Merge1secreferenceDEMsfromVisioterra1: TMenuItem;
    Modesofdifferencedistributions1: TMenuItem;
    Advancedanalysis1: TMenuItem;
    //MultipledNBRmaps1: TMenuItem;
    N28: TMenuItem;
    DEMIXhelp1: TMenuItem;
    Landformcategorieslegends1: TMenuItem;
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
    //DiluviumDEMandDEMIXDBoverlap1: TMenuItem;
    CheckreferenceDEMs1: TMenuItem;
    ChecktestDEMs1: TMenuItem;
    //DiluviumDEMfortestareas1: TMenuItem;
    //CreaterangereferenceDEMs1: TMenuItem;
    Addprefixtoallfilesindirectory1: TMenuItem;
    Inventorydifferencestats1: TMenuItem;
    MergeDEMIXtilestats1: TMenuItem;
    N48: TMenuItem;
    Channelnetworkmisspercentagesbytile1: TMenuItem;
    SSIM2: TMenuItem;
    MergemultipleTXTCSVintoDB1: TMenuItem;
    //Overwrite4: TMenuItem;
    //CreatetestareaDEMSskipifexists1: TMenuItem;
    CreatetestareaDEMs1: TMenuItem;
    Overwriteifexits1: TMenuItem;
    Overwriteifexits2: TMenuItem;
    Overwirteifexists1: TMenuItem;
    Skipifexits1: TMenuItem;
    MICRODEMgridformat1: TMenuItem;
    N51: TMenuItem;
    Overwriteifexits3: TMenuItem;
    Skipifexits2: TMenuItem;
    Overwriteifexists1: TMenuItem;
    Overwriteifexists2: TMenuItem;
    //Inventory3DEPtiles1: TMenuItem;
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
    //Overwriteifexists3: TMenuItem;
    //Overwriteifexists4: TMenuItem;
    N39: TMenuItem;
    Inventorychanneldatabyarea1: TMenuItem;
    //DatumshiftCanadianDEMs1: TMenuItem;
    MergeCanadianLidar1: TMenuItem;
    Reference1secDTMsfromCanadianlidar1: TMenuItem;
    Graphevaluationandscores1: TMenuItem;
    //CleardoubleprocessedreferenceDEMtiles1: TMenuItem;
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
    //Overwrite1: TMenuItem;
    Skipifexists3: TMenuItem;
    CreatefinalDB1: TMenuItem;
    N3DEPfileswithtag421121: TMenuItem;
    Fixtileswith42114foottag1: TMenuItem;
    Onedegreetilestocovertestareas1: TMenuItem;
    N46: TMenuItem;
    //GetrangesforSSIMhydro1: TMenuItem;
    OpenmapsforDEMIXtestarea1: TMenuItem;
    LoadCpopDEMandLNDCOERFORTEXTAREA1: TMenuItem;
    //Createlandcovergrids1: TMenuItem;
    Mergechannelnetworkevaluations1: TMenuItem;
    Changemode1: TMenuItem;
    DEMIXtilesperareaandcoastalsubset1: TMenuItem;
    //Overwirte4: TMenuItem;
    Skiipifpresent1: TMenuItem;
    InventoryallDEMIXdatafiles1: TMenuItem;
    Combineallcombinedimages1: TMenuItem;
    CoastalDEMfortestareas1: TMenuItem;
    OverwriteallthreecoastalDTMS1: TMenuItem;
    N55: TMenuItem;
   // Overwrite2: TMenuItem;
    Skipifpresent1: TMenuItem;
    Mergegeomorphonevaluatioins1: TMenuItem;
    //Overwrite3: TMenuItem;
    Skipifdone1: TMenuItem;
    DeletereferenceDTMswithoutDTMinfilename1: TMenuItem;
    MovereferenceDSMs1: TMenuItem;
    DEMIXtilesineachareaforFULLU120U80andandU101: TMenuItem;
    Ridgesandvalleys1: TMenuItem;
    //Overwrite5: TMenuItem;
    //Overwrite6: TMenuItem;
    Mergeridgesandvalleys1: TMenuItem;
   //Overwrite7: TMenuItem;
    Skipifpresent2: TMenuItem;
    N56: TMenuItem;
    //Compareconvergenceindexfortestarea1: TMenuItem;
    Open4elevationrangeDEMIXDBs1: TMenuItem;
    UTMprojection1: TMenuItem;
    AddEXIFtagsworkinprogress1: TMenuItem;
    N57: TMenuItem;
    //Criteriaranges1: TMenuItem;
    CompareUTMandgeographicslopes1: TMenuItem;
    Howbigisanarcsecond1: TMenuItem;
    Createcompositebitmap2: TMenuItem;
    DEMIXNeo1: TMenuItem;
    //N58: TMenuItem;
    MultipleDEMsonearea015secscale1: TMenuItem;
    CorrelationsingleDEMtoreferencealllandcovers1: TMenuItem;
    CorrelationmatrixamongallDEMsforALLpixels1: TMenuItem;
    Singletile1: TMenuItem;
    Graphdifferencedistributionsalllandcovers1: TMenuItem;
    Multipletiles1: TMenuItem;
    Differencedistributionstatistics1: TMenuItem;
    FUV2: TMenuItem;
    FUVforrangescales1: TMenuItem;
    FUVfor5DEMstoreference1: TMenuItem;
    FUVbyLandcover1DEMtoreference1: TMenuItem;
    CorrelationmatricesamongallDEMsjustALLlandcover1: TMenuItem;
    CorrelationmatrixsingleDEMtoallothersALLlandcoveronly1: TMenuItem;
    Areaevaluations1: TMenuItem;
    N59: TMenuItem;
    N60: TMenuItem;
    PicktestDEMs1: TMenuItem;
    N61: TMenuItem;
    N62: TMenuItem;
    N64: TMenuItem;
    N65: TMenuItem;
    ComparelandcoverinpointcloudDTM1: TMenuItem;
    Addlegendonlylastgraph1: TMenuItem;
    N63: TMenuItem;
    UTMbasedtilescreation1: TMenuItem;
    N66: TMenuItem;
    UTMbasedFUVcalculations1: TMenuItem;
    UTMbaseaverageslopebygeometry1: TMenuItem;
    GeographictilesreferenceandtestDEMs1: TMenuItem;
    CoastalDTMs1: TMenuItem;
    Channelnetworks1: TMenuItem;
    N54: TMenuItem;
    UTMbasedtilestatistics1: TMenuItem;
    UTMbasedmergeFUVresultsintoDB1: TMenuItem;
    UTMbasedmergetilestatisiticsintoDB1: TMenuItem;
    UTMbasedFUVcurvaturecalculations1: TMenuItem;
    UTMbasedFUVpartialscalculations1: TMenuItem;
    UTMbaseddifferencedistributionstatistics1: TMenuItem;
    UTMbasedmergedifferencedistribution1: TMenuItem;
    UTMbasedmergecurvaturesintoDB1: TMenuItem;
    UTMbasedmergepartialsintoDB1: TMenuItem;
    UTMbasedallareacomputations1: TMenuItem;
    UTMbasedCVSfilesbyarea1: TMenuItem;
    Mergesmalltilesinto10kmUTMblocks1: TMenuItem;
    MovemergedtilereferenceandtestDEMs1: TMenuItem;
    N25: TMenuItem;
    N44: TMenuItem;
    UTMbasedmergeallDB1: TMenuItem;
    Download10x10kmblocks1: TMenuItem;
    UTMbasedfilefillreport1: TMenuItem;
    DeleteresultsCSVforareas1: TMenuItem;
    CloneEXE1: TMenuItem;
    N67: TMenuItem;
    Geotiles1: TMenuItem;
    N43: TMenuItem;
    FixGEDTMlateadditonproblems1: TMenuItem;
    SmallcloneEXEwindow1: TMenuItem;
    InventoryofLC10files1: TMenuItem;
    N15: TMenuItem;
    N49: TMenuItem;
    CloneEXEformultithreading1: TMenuItem;
    Inventorysourcefiles1: TMenuItem;
    Compare10mand100mlandcover1: TMenuItem;
    Averagetileslopemapgeneration1: TMenuItem;
    Percentlandcovercategories1: TMenuItem;
    ilecharacterization1: TMenuItem;
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
    procedure Openpointclouds1Click(Sender: TObject);
    procedure StatusBar1Click(Sender: TObject);
    procedure LatlongofPLSSlocation1Click(Sender: TObject);
    procedure MergewavelengthheightDBFs1Click(Sender: TObject);
    //procedure NLCD20011Click(Sender: TObject);
    procedure ToolBar1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    //procedure UTM1Click(Sender: TObject);
    procedure OpenandmergeDEMs1Click(Sender: TObject);
    //procedure ImportCTDfile1Click(Sender: TObject);
    procedure EXIFmetadata1Click(Sender: TObject);
    procedure EXIFimage1Click(Sender: TObject);
    procedure Clustergrids1Click(Sender: TObject);
    procedure Copyfile1Click(Sender: TObject);
    //procedure XYZshapefile1Click(Sender: TObject);
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
    procedure Updatehelpfile2Click(Sender: TObject);
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
    procedure OpenDEMwithoutmap1Click(Sender: TObject);
    procedure Megathrusts1Click(Sender: TObject);
    procedure NewglobalgridGreenwich1Click(Sender: TObject);
    procedure NewglobalgridIDL1Click(Sender: TObject);
    procedure ConvertDBFsfor64bit1Click(Sender: TObject);
    procedure Landsatbrowseindex1Click(Sender: TObject);
    //procedure Mercator1Click(Sender: TObject);
    //procedure Southpolarstereographic1Click(Sender: TObject);
    procedure Openmultigrids1Click(Sender: TObject);
    procedure Solarpositiln1Click(Sender: TObject);
    procedure Graysgame1Click(Sender: TObject);
    procedure MH370region1Click(Sender: TObject);
    procedure Geoidandsedimentdistribution1Click(Sender: TObject);
    procedure MagMapButtonClick(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure GulfofMexicoGLORIA1Click(Sender: TObject);
    procedure Atlantis1Click(Sender: TObject);
    procedure DEMsummarytable1Click(Sender: TObject);
    procedure Zeorlog1Click(Sender: TObject);
    procedure Landsatfullsceneindex1Click(Sender: TObject);
    procedure Satellitepredictions1Click(Sender: TObject);
    procedure Subset81Ssidescan1Click(Sender: TObject);
    procedure OpenScannedmap1Click(Sender: TObject);
    procedure Openlandcover1Click(Sender: TObject);
    procedure Ages1Click(Sender: TObject);
    procedure Magneticanomaliesgrid1Click(Sender: TObject);
    procedure Sedimentthicknessgrid1Click(Sender: TObject);
    procedure Sedimenttypegrid1Click(Sender: TObject);
    procedure Fontsinstalled1Click(Sender: TObject);
    procedure Unicodeicongenerator1Click(Sender: TObject);
    //procedure UKOSgrid2Click(Sender: TObject);
    procedure Nyquist1Click(Sender: TObject);
    procedure Onlinehelp1Click(Sender: TObject);
    procedure OpenandmergeDEMdirectories1Click(Sender: TObject);
    procedure SpeedButton6Click(Sender: TObject);
    //procedure est1Click(Sender: TObject);
    //procedure LatLong1Click(Sender: TObject);
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
    procedure Slidesorter1Click(Sender: TObject);
    procedure Movefileswithnamematch1Click(Sender: TObject);
    procedure RenameJPEGswithcreationtime1Click(Sender: TObject);
    procedure Mergemasp1Click(Sender: TObject);
    procedure Batchchangepartoffilenames1Click(Sender: TObject);
    procedure OpenandmergeGeotiffs1Click(Sender: TObject);
    procedure DragonPlot1Click(Sender: TObject);
    procedure OpenSentinen2image1Click(Sender: TObject);
    procedure Openlidarmatchedgrids1Click(Sender: TObject);
    procedure RenameJPRGswithbasenamenumber1Click(Sender: TObject);
    procedure RenameJPEGSwithbaseandcreationtime1Click(Sender: TObject);
    procedure Labs1Click(Sender: TObject);
    procedure CloseallDBs1Click(Sender: TObject);
    procedure Spectrallibrary3Click(Sender: TObject);
    procedure RGBcolorlayers1Click(Sender: TObject);
    procedure GDALSRSinfo1Click(Sender: TObject);
    procedure WhiteboxGeotiff1Click(Sender: TObject);
    procedure Guam1Click(Sender: TObject);
    procedure GDALWKT1Click(Sender: TObject);
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
    procedure Openrecyclebin1Click(Sender: TObject);
    procedure Existingfile1Click(Sender: TObject);
    procedure Existingfile2Click(Sender: TObject);
    procedure Horizontalimageslider1Click(Sender: TObject);
    procedure OpensingleLandsatband1Click(Sender: TObject);
    procedure listgeo1Click(Sender: TObject);
    procedure DEMIXtilesizebylatitude1Click(Sender: TObject);
    procedure Python1Click(Sender: TObject);
    procedure OpenSentinel1radarimagery1Click(Sender: TObject);
    procedure DEMIX1Click(Sender: TObject);
    //procedure BatchNDVIClick(Sender: TObject);
    procedure HistogramstoCSVfiles1Click(Sender: TObject);
    procedure Bringslicecontroltofront1Click(Sender: TObject);
    procedure Bringpointcloudcontroltofront1Click(Sender: TObject);
    procedure Viewlastexectiondebuglog1Click(Sender: TObject);
    //procedure COPALOScomparetoreference1Click(Sender: TObject);
    procedure Pixelbypixelmapstatistics1Click(Sender: TObject);
    //procedure COPALOShighlowgeomorphometry1Click(Sender: TObject);
    procedure Metadata2Click(Sender: TObject);
    //procedure Pickdatadirectory1Click(Sender: TObject);
    //procedure N3OpenDEMs1Click(Sender: TObject);
    procedure OpenandmergeDEMsgridsverylarge1Click(Sender: TObject);
    //procedure Creatediffrencemaps1Click(Sender: TObject);
    procedure Mergesourcedatatiles1Click(Sender: TObject);
    procedure ProcessVDATUMshifts1Click(Sender: TObject);
    //procedure VDATUMshiftinUSA1Click(Sender: TObject);
    procedure OpenDEMIXdatabase1Click(Sender: TObject);
    procedure Addversionnumbertoallfilesinapath1Click(Sender: TObject);
    procedure Perpendicularshortprofilesthroughpoint1Click(Sender: TObject);
   // procedure N41Click(Sender: TObject);
    procedure SummarizeverticaldatumshiftforEGM96testDEMs1Click(Sender: TObject);
    //procedure OpenandmergeDEMswithfullDEMIXcoverage1Click(Sender: TObject);
    procedure Subsetlarge3DEPareas1Click(Sender: TObject);
    //procedure CreatehalfsecondreferenceDEMs1Click(Sender: TObject);
    procedure Merge1secreferenceDEMsfromVisioterra1Click(Sender: TObject);
    procedure Modesofdifferencedistributions1Click(Sender: TObject);
    procedure Advancedanalysis1Click(Sender: TObject);
    //procedure MultipledNBRmaps1Click(Sender: TObject);
    procedure DEMIXhelp1Click(Sender: TObject);
    procedure Landformcategorieslegends1Click(Sender: TObject);
    //procedure Extract1Click(Sender: TObject);
    procedure Pythontestrun1Click(Sender: TObject);
    procedure Listofsubdirectoriesrelativenames1Click(Sender: TObject);
    procedure Listofsubdirectoriesfullpaths1Click(Sender: TObject);
    procedure SumatraPDFhelpcontents1Click(Sender: TObject);
    procedure Noaddedlegends1Click(Sender: TObject);
    procedure Noaddedlegends2Click(Sender: TObject);
    procedure N45Click(Sender: TObject);
    procedure CheckfilesizesforSSIMimagemismatches1Click(Sender: TObject);
    //procedure DiluviumDEMandDEMIXDBoverlap1Click(Sender: TObject);
    procedure CheckreferenceDEMs1Click(Sender: TObject);
    procedure ChecktestDEMs1Click(Sender: TObject);
    //procedure DiluviumDEMfortestareas1Click(Sender: TObject);
    //procedure CreaterangereferenceDEMs1Click(Sender: TObject);
    procedure Addprefixtoallfilesindirectory1Click(Sender: TObject);
    procedure Inventorydifferencestats1Click(Sender: TObject);
    procedure MergeDEMIXtilestats1Click(Sender: TObject);
    procedure MergemultipleTXTCSVintoDB1Click(Sender: TObject);
    //procedure Overwrite4Click(Sender: TObject);
    //procedure CreatetestareaDEMSskipifexists1Click(Sender: TObject);
    procedure Overwriteifexits1Click(Sender: TObject);
    procedure Overwirteifexists1Click(Sender: TObject);
    procedure Overwriteifexits2Click(Sender: TObject);
    procedure MICRODEMgridformat1Click(Sender: TObject);
    procedure Overwriteifexits3Click(Sender: TObject);
    procedure Overwriteifexists1Click(Sender: TObject);
    procedure Overwriteifexists2Click(Sender: TObject);
    //procedure Inventory3DEPtiles1Click(Sender: TObject);
    procedure InventoryDILUVIUMbytestarea1Click(Sender: TObject);
    procedure InsureallreferenceDTMscorrectlynamed1Click(Sender: TObject);
    procedure ComputeDEMIXtilestats1Click(Sender: TObject);
    procedure VerifytestDEMcoverages1Click(Sender: TObject);
    procedure rimreferencedatatoDEMIXtiles1Click(Sender: TObject);
    //procedure Overwriteifexists3Click(Sender: TObject);
    //procedure Overwriteifexists4Click(Sender: TObject);
    procedure Inventorychanneldatabyarea1Click(Sender: TObject);
    //procedure DatumshiftCanadianDEMs1Click(Sender: TObject);
    procedure Reference1secDTMsfromCanadianlidar1Click(Sender: TObject);
    procedure Graphevaluationandscores1Click(Sender: TObject);
    //procedure CleardoubleprocessedreferenceDEMtiles1Click(Sender: TObject);
    procedure Overwirte1Click(Sender: TObject);
    procedure DEMIX2Click(Sender: TObject);
    procedure Overwirte2Click(Sender: TObject);
    procedure Overwirte3Click(Sender: TObject);
    procedure InventorySSIMFUVCSVfiles1Click(Sender: TObject);
    procedure N42Click(Sender: TObject);
    procedure DeltaDTMfortestareas1Click(Sender: TObject);
    //procedure Overwrite1Click(Sender: TObject);
    procedure CreatefinalDB1Click(Sender: TObject);
    procedure N3DEPfileswithtag421121Click(Sender: TObject);
    procedure Fixtileswith42114foottag1Click(Sender: TObject);
    procedure Onedegreetilestocovertestareas1Click(Sender: TObject);
    //procedure GetrangesforSSIMhydro1Click(Sender: TObject);
    procedure OpenmapsforDEMIXtestarea1Click(Sender: TObject);
    procedure LoadCpopDEMandLNDCOERFORTEXTAREA1Click(Sender: TObject);
    //procedure Createlandcovergrids1Click(Sender: TObject);
    procedure Mergechannelnetworkevaluations1Click(Sender: TObject);
    procedure Changemode1Click(Sender: TObject);
    procedure DEMIXtilesperareaandcoastalsubset1Click(Sender: TObject);
    //procedure Overwirte4Click(Sender: TObject);
    procedure InventorytestandrefereneDEMsbytestarea1Click(Sender: TObject);
    procedure InventoryallDEMIXdatafiles1Click(Sender: TObject);
    procedure Combineallcombinedimages1Click(Sender: TObject);
    procedure CoastalDEMfortestareas1Click(Sender: TObject);
    procedure SSIMR21Click(Sender: TObject);
    //procedure Overwrite2Click(Sender: TObject);
    procedure Skipifpresent1Click(Sender: TObject);
    procedure Mergegeomorphonevaluatioins1Click(Sender: TObject);
    ///procedure Overwrite3Click(Sender: TObject);
    procedure DeletereferenceDTMswithoutDTMinfilename1Click(Sender: TObject);
    procedure DEMIXtilesineachareaforFULLU120U80andandU101Click(Sender: TObject);
    //procedure Overwrite5Click(Sender: TObject);
    //procedure Overwrite6Click(Sender: TObject);
    procedure Mergeridgesandvalleys1Click(Sender: TObject);
    //procedure Overwrite7Click(Sender: TObject);
    procedure Skipifpresent2Click(Sender: TObject);
    //procedure Compareconvergenceindexfortestarea1Click(Sender: TObject);
    procedure Open4elevationrangeDEMIXDBs1Click(Sender: TObject);
    procedure UTMprojection1Click(Sender: TObject);
    procedure AddEXIFtagsworkinprogress1Click(Sender: TObject);
    //procedure Criteriaranges1Click(Sender: TObject);
    procedure CompareUTMandgeographicslopes1Click(Sender: TObject);
    procedure Howbigisanarcsecond1Click(Sender: TObject);
    procedure Createcompositebitmap2Click(Sender: TObject);
    //procedure N58Click(Sender: TObject);
    procedure MultipleDEMsonearea015secscale1Click(Sender: TObject);
    procedure CorrelationsingleDEMtoreferencealllandcovers1Click(
      Sender: TObject);
    procedure CorrelationmatrixamongallDEMsforALLpixels1Click(Sender: TObject);
    procedure Graphdifferencedistributionsalllandcovers1Click(Sender: TObject);
    procedure Differencedistributionstatistics1Click(Sender: TObject);
    procedure FUV2Click(Sender: TObject);
    procedure FUVforrangescales1Click(Sender: TObject);
    procedure FUVfor5DEMstoreference1Click(Sender: TObject);
    procedure FUVbyLandcover1DEMtoreference1Click(Sender: TObject);
    procedure CorrelationmatricesamongallDEMsjustALLlandcover1Click(Sender: TObject);
    procedure CorrelationmatrixsingleDEMtoallothersALLlandcoveronly1Click(Sender: TObject);
    procedure Areaevaluations1Click(Sender: TObject);
    procedure N60Click(Sender: TObject);
    procedure PicktestDEMs1Click(Sender: TObject);
    procedure N62Click(Sender: TObject);
    procedure N64Click(Sender: TObject);
    procedure ComparelandcoverinpointcloudDTM1Click(Sender: TObject);
    procedure Addlegendonlylastgraph1Click(Sender: TObject);
    procedure UTMbasedtilescreation1Click(Sender: TObject);
    procedure UTMbasedFUVcalculations1Click(Sender: TObject);
    procedure UTMbaseaverageslopebygeometry1Click(Sender: TObject);
    procedure UTMbasedtilestatistics1Click(Sender: TObject);
    procedure UTMbasedmergeFUVresultsintoDB1Click(Sender: TObject);
    //procedure Cleanuptilenames1Click(Sender: TObject);
    procedure UTMbasedmergetilestatisiticsintoDB1Click(Sender: TObject);
    procedure UTMbasedFUVcurvaturecalculations1Click(Sender: TObject);
    procedure UTMbasedFUVpartialscalculations1Click(Sender: TObject);
    procedure UTMbaseddifferencedistributionstatistics1Click(Sender: TObject);
    procedure UTMbasedmergedifferencedistribution1Click(Sender: TObject);
    procedure UTMbasedmergecurvaturesintoDB1Click(Sender: TObject);
    procedure UTMbasedmergepartialsintoDB1Click(Sender: TObject);
    procedure UTMbasedallareacomputations1Click(Sender: TObject);
    procedure UTMbasedCVSfilesbyarea1Click(Sender: TObject);
    procedure Mergesmalltilesinto10kmUTMblocks1Click(Sender: TObject);
    procedure MovemergedtilereferenceandtestDEMs1Click(Sender: TObject);
    procedure UTMbasedmergeallDB1Click(Sender: TObject);
    procedure Download10x10kmblocks1Click(Sender: TObject);
    procedure UTMbasedfilefillreport1Click(Sender: TObject);
    procedure DeleteresultsCSVforareas1Click(Sender: TObject);
    procedure CloneEXE1Click(Sender: TObject);
    procedure FixGEDTMlateadditonproblems1Click(Sender: TObject);
    procedure SmallcloneEXEwindow1Click(Sender: TObject);
    procedure InventoryofLC10files1Click(Sender: TObject);
    procedure CloneEXEformultithreading1Click(Sender: TObject);
    procedure Inventorysourcefiles1Click(Sender: TObject);
    procedure Compare10mand100mlandcover1Click(Sender: TObject);
    procedure Averagetileslopemapgeneration1Click(Sender: TObject);
    procedure Percentlandcovercategories1Click(Sender: TObject);
    procedure ilecharacterization1Click(Sender: TObject);
  private
    procedure SunViews(Which : integer);
    procedure SeeIfThereAreDebugThingsToDo;
    { Private declarations }
  public
    { Public declarations }
      ProgramClosing,NoAutoOpen : boolean;
      procedure SetMenusForVersion;
      procedure FormPlacementInCorner(TheForm : Forms.tForm; FormPosition : byte = lpSEMap);
      procedure HandleThreadTerminate(Sender: TObject);
      procedure SetPanelText(PanelNum : integer; What : shortString; OverrideLock : boolean = false);
      procedure ClearStatusBarPanelText;
  end;

//const
   //IDDirToMark : PathStr = '';

var
   wmdem : Twmdem;
   LockStatusBar,
   ClosingEverything,
   SkipMenuUpdating,FirstRun : boolean;

function OpenGazFile(fName : PathStr = '') : integer;
procedure InsureFormOnScreenCurrentLocation(Form4 : tForm; x,y : integer);
procedure SetColorForProcessing; inline
procedure SetColorForWaiting; inline
procedure SmallClonePlacement;
function ClonedExe : boolean;
procedure InsureFormIsOnScreen(TheForm : Forms.tForm);
         procedure PlaceFormInCorner(Owner,TheForm : Forms.tForm; FormPosition :  byte = lpSEMap);
         procedure CheckFormPlacement(TheForm : Forms.tForm);
         procedure PlaceFormAtMousePosition(TheForm : Forms.tForm);


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
   //moon_montenbruk_pfleger,
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

   {$IfDef ExPointCloud}
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

   {$IfDef ExMultiGrid}
   {$Else}
      MultiGrid,
      Monthly_Grids,
   {$EndIf}

   {$IfDef ExCartography}
   {$Else}
      DEM_cart_proj,
   {$EndIf}

   {$IfDef ExFMX3D}
   {$Else}
      FPointCloud,
   {$EndIf}

   {$IfDef ExMake_grid}
   {$Else}
      Make_grid,
   {$EndIf}

   {$IfDef IncludePython}
      Simple_Python,
   {$EndIf}

   {$IfDef ExDEMIX}
      //not tested recently; might have to disable options in code (add compiler directives) to get this to work
   {$Else}
      demix_definitions,
      DEMIX_Control,
      //DEMIX_cop_alos,
      demix_evals_scores_graphs,
      ssim_fuv_control,
      demix_neo_test_area,
      DEMIX_filter,
      DEMIX_graphs,
   {$EndIf}

   Las_files_grouping,

   ufrmMain,
   CCR.Exif,
   edit_exif_fields,

   Slider_sorter_form,
   NyqGraph,
   gdal_tools,
   compare_geo_utm,

   PetImage_Form,
   TerSplsh,
   stereo_viewer,
   get_thumbnails,
   Main_Gray_game,
   DEM_manager,
   new_petmar_movie,

   lcp_options,
   ScreenUnicode,
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

function ClonedEXE : boolean;
var
   ShortEXE : shortString;
begin
    ShortExe := UpperCase(ExtractFileNameNoExt(Application.ExeName));
    Result := (Length(ShortExe) = 3) and (copy(ShortExe,1,2) = 'MD');
end;


procedure SmallClonePlacement;
var
   i : integer;
begin
   if ClonedEXE then begin
      WMDEM.Width := 800;
      WMDEM.Height := 600;
      WMDEM.Top := 300;
      WMDEM.Left := 4000;
      MDDef.SB1PanelWidths[0] := 150;
      MDDef.SB1PanelWidths[1] := 275;
      MDDef.SB1PanelWidths[2] := 200;
      MDDef.SB1PanelWidths[3] := 240;
      for i := 0 to 3 do WmDEM.StatusBar1.Panels[i].Width := MDDef.SB1PanelWidths[i];
   end;
end;


procedure SetColorForWaiting; inline
begin
   WMdem.Color := clScrollBar;
   WMDEM.ClearStatusBarPanelText;
   ShowDefaultCursor;
end;

procedure SetColorForProcessing; inline
begin
   WMdem.Color := clInactiveCaption;
   ShowHourglassCursor;
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

         procedure InsureFormIsOnScreen(TheForm : Forms.tForm);
         begin
            if (TheForm.Left < 50) then TheForm.Left := 50;
            if (TheForm.Left > wmdem.Width - TheForm.Width) then TheForm.Left := wmdem.Width - TheForm.Width -25;
            if (TheForm.Top < 50) then TheForm.Top := 50;
            if (TheForm.Top + TheForm.Height) > wmdem.Height then TheForm.Top := WMDEM.Height - TheForm.Height - 50;
         end;


         procedure PlaceFormInCorner(Owner,TheForm : Forms.tForm; FormPosition :  byte = lpSEMap);
         begin
            TheForm.DefaultMonitor := dmMainForm;
            if FormPosition = lpCenterMap then begin
               TheForm.Left := (Owner.ClientWidth - TheForm.Width) div 2;
               TheForm.Top := (Owner.ClientHeight - TheForm.Height) div 2;
            end
            else begin
               if (FormPosition = lpNEMap) then TheForm.Top := 0
               else TheForm.Top := Owner.ClientHeight - TheForm.Height - 25;
               TheForm.Left := Owner.ClientWidth - TheForm.Width - 10;
            end;
            if (TheForm.Top < 0) then TheForm.Top := 10;
            if (TheForm.Left < 0) then TheForm.Left := 10;
         end;

         procedure CheckFormPlacement(TheForm : Forms.tForm);
         begin
            PlaceFormInCorner(wmdem,TheForm,lpCenterMap);
            {$IfDef TrackFormPlacement} WriteLineToDebugFile(TheForm.Caption + '  ' + FormSize(TheForm) +'  Placement: ' + IntToStr(TheForm.Left) + 'x' + IntToStr(TheForm.Top));  {$EndIf}
         end;

         procedure PlaceFormAtMousePosition(TheForm : Forms.tForm);
         begin
            TheForm.Top := Mouse.CursorPos.Y - TheForm.Height div 2;
            TheForm.Left := Mouse.CursorPos.X - TheForm.Width div 2;
            InsureFormIsOnScreen(TheForm);
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

procedure Twmdem.DeleteresultsCSVforareas1Click(Sender: TObject);
begin
    DEMIX_UTM_based_processing(udDeleteResultsCSVsForAreas);
end;

procedure Twmdem.DeltaDTMfortestareas1Click(Sender: TObject);
begin
   //DeltaDTMforTestAreas(false);
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
   //TilesInEachElevRangeForTestAreas;
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
   DEMIXNeo1.Visible := TrilobiteComputer;

   ExpertDEMVersion := (MDDef.ProgramOption in [ExpertProgram,RemoteSensingProgram]) and (NumDEMDataSetsOpen > 0);
   RemoteSensingLabs1.Visible := (MDDef.ProgramOption in [ExpertProgram,RemoteSensingProgram]);
   SpeedButton6.Visible := (MDDef.ProgramOption in [ExpertProgram]);

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

   //3D viewing options
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
      Newpanorama1.Visible := (NumDEMDataSetsOpen > 0) and (MDDef.ProgramOption = ExpertProgram);
      LiveFlySpeedButton.Enabled := (NumDEMDataSetsOpen > 0);



   Horizontalearthcurvature1.Visible := ExpertDEMVersion;
   Clustergrids1.Visible := ExpertDEMVersion;

   Superimposedtopoprofiles1.Visible := (NumDEMDataSetsOpen > 1) and (MDDef.ShowDEMcompare) and (MDDef.ProgramOption = ExpertProgram);

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

   DEMIXNeo1.Visible := TrilobiteComputer;

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

   {$IfDef CopALOSCompare}
   {$Else}
      //N3OpenDEMs1.Visible := false;
      //COPALOScomparetoreference1.Visible := false;
      //COPALOShighlowgeomorphometry1.Visible := false;
      //Creatediffrencemaps1.Visible := false;
      //Pickdatadirectory1.Visible := false;
      Pixelbypixelmapstatistics1.Visible := false;
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
   //Overwrite4.Enabled := MDDef.DEMIX_overwrite_enabled;
   //OverwriteIfExits2.Enabled := MDDef.DEMIX_overwrite_enabled;
   CoastalDEMfortestareas1.Visible := MDdef.DEMIX_AllowCoastal;
   //DiluviumDEMfortestareas1.Visible := MDdef.DEMIX_AllowCoastal;
   DeltaDTMfortestareas1.Visible := MDdef.DEMIX_AllowCoastal;
   OverwriteallthreecoastalDTMS1.Visible := MDdef.DEMIX_AllowCoastal;


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


      procedure ProcessCommandLine(CommandLine : AnsiString);
      //CommandLine comes in capitalized
      var
         Key,Value : AnsiString;
         SlopeOrCurveCompute : tSlopeCurveCompute;
         Upward,DownWard,Difference,BoxRadiusPixels,StartOnRadial,
         DEM,NewDEM,WhichCurvature : integer;
         ScaleFactor : byte;
         FileList : tStringList;
         SlopeDegree : boolean;
         Action,xval,yval : shortstring;
         infile,outfile,upfile,downfile,difffile : PathStr;

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
         difffile := '';
         WhichCurvature := 0;
         SlopeDegree := false;
         BoxRadiusPixels := 10;
         StartOnRadial := 1;

         //set program defaults; these will not be saved
             SlopeOrCurveCompute.AlgorithmName := smLSQ;
             SlopeOrCurveCompute.LSQorder := 2;
             SlopeOrCurveCompute.WindowRadius := 1;
             SlopeOrCurveCompute.RequireFullWindow := true;
             SlopeOrCurveCompute.UsePoints := UseAll;

         while (CommandLine[1] = '?') do Delete(CommandLine,1,1);      //the question mark
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
            if Key = 'DIFFNAME' then difffile := Value;
            if Key = 'PTSEP' then MDDef.PointSeparation := StrToInt(Value);
            if Key = 'ROAD_FILE' then MDDef.LCPRoadfName := Value;
            if Key = 'START_PTS' then MDDef.LCPstartFName := Value;
            if Key = 'END_PTS' then MDDef.LCPendFName := Value;
            if Key = 'INI' then ProcessIniFile(iniRead,'',Value);
            if Key = 'X' then xval := Value;
            if Key = 'Y' then yval := Value;
            if Key = 'SCALEFactor' then ScaleFactor := StrToInt(Value);
            if Key = 'RAD' then MDDef.OpennessBoxRadiusMeters := StrToInt(Value);
            if Key = 'SLOPE_RAD' then SlopeOrCurveCompute.WindowRadius := StrToInt(Value);
            if Key = 'SLOPE_UNIT' then SlopeDegree := Value = 'DEGREE';
            if Key = 'POLY_ORDER' then SlopeOrCurveCompute.LSQorder := StrToInt(Value);
            if Key = 'SLOPE_FULL' then SlopeOrCurveCompute.RequireFullWindow := (Value = 'YES');
            if Key = 'BOXSIZE' then MDDef.GeomorphBoxSizeMeters := StrToInt(Value);
            if Key = 'RADIUS' then BoxRadiusPixels := StrToInt(Value);
            if Key = 'START' then StartOnRadial := StrToInt(Value);
            if Key = 'CURVE_MODE' then WhichCurvature := CurveCodeFromName(Value);
            if Key = 'FILELIST' then begin
               FileList := tStringList.Create;
               FileList.LoadFromFile(Value)
            end;
            if Key = 'USE_PTS' then begin
               if Value = 'ALL' then SlopeOrCurveCompute.UsePoints := UseAll;
               if Value = 'EDGE' then SlopeOrCurveCompute.UsePoints := UseEdge;
               if Value = 'QUEEN' then SlopeOrCurveCompute.UsePoints := UseQueens;
            end;
            if Key = 'SLOPE_ALG' then begin
                if Value = 'LSQ' then SlopeOrCurveCompute.AlgorithmName := smLSQ;
                if Value = 'EVANS' then SlopeOrCurveCompute.AlgorithmName := smEvansYoung;
                if Value = 'ZQ' then SlopeOrCurveCompute.AlgorithmName := smZevenbergenThorne;
                if Value = 'HORN' then SlopeOrCurveCompute.AlgorithmName := smHorn;
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
         end;
         {$IfDef RecordCommandLine} WriteLineToDebugFile('command line parsed, action=' + Action); {$EndIf}

         if (Action = 'SLOPE_MAP') then begin
            if OpenADEM then begin
               {$IfDef RecordCommandLine} WriteLineToDebugFile('dem opened'); {$EndIf}
               CreateSlopeMapPercentAlgorithm(SlopeOrCurveCompute, false,DEM,outfile,SlopeDegree);
               {$IfDef RecordCommandLine} WriteLineToDebugFile('slope map created'); {$EndIf}
            end;
         end;

         if (Action = 'CURVE_MAP') then begin
            if OpenADEM then begin
               MDDef.CurveCompute := SlopeOrCurveCompute;
               {$IfDef RecordCommandLine} WriteLineToDebugFile('dem opened'); {$EndIf}
               CreateCurvatureMap(WhichCurvature,false,DEM,OutFile);
               {$IfDef RecordCommandLine} WriteLineToDebugFile('curvature map created'); {$EndIf}
            end;
         end;

         if (Action = 'ASPECT_MAP') then begin
            if OpenADEM then begin
               {$IfDef RecordCommandLine} WriteLineToDebugFile('dem opened'); {$EndIf}
               NewDEM := MakeAspectMap(false,DEM,outfile);
               {$IfDef RecordCommandLine} WriteLineToDebugFile('slope map created'); {$EndIf}
               //DEMGlb[NewDEM].SaveAsGeotiff(outfile);
               {$IfDef RecordCommandLine} WriteLineToDebugFile('geotiff saved'); {$EndIf}
            end;
         end;

         if (Action = 'OPENNESS_MAP') then begin
            if OpenADEM then begin
               {$IfDef RecordCommandLine} WriteLineToDebugFile('dem opened'); {$EndIf}
               Upward := -1;
               DownWard := -1;
               Difference := -1;
               MDDef.OpenStartRadialsAtPixel := StartOnRadial;
               CreateOpennessMap(false,DEMglb[DEM].FullDEMGridLimits,DEM,-99,BoxRadiusPixels,Upward,DownWard,Difference);
               {$IfDef RecordCommandLine} WriteLineToDebugFile('openness map created'); {$EndIf}
               if (UpFile <> '') then DEMGlb[MomentDEMs[Upward]].SaveAsGeotiff(UpFile);
               if (DownFile <> '') then DEMGlb[MomentDEMs[Downward]].SaveAsGeotiff(DownFile);
               if (DiffFile <> '') then DEMGlb[MomentDEMs[Difference]].SaveAsGeotiff(DiffFile);
               {$IfDef RecordCommandLine} WriteLineToDebugFile('geotiff saved'); {$EndIf}
            end;
         end;

         if (Action = 'MAD2K_MAP') then begin
            if OpenADEM then begin
               MakeMAD2KGrid(false,DEM,outfile,ScaleFactor);
            end;
         end;

         if (Action = 'IQRSLOPE_MAP') then begin
            if OpenADEM then begin
               CreateIQRSlopeMap(false,DEM,MDDef.GeomorphBoxSizeMeters,outfile);
            end;
         end;

         if (Action = 'TERR_FABRIC') then begin
            if OpenADEM then begin
               DEMGlb[DEM].OrientationTable(OutFile,Nil);
            end;
         end;

         if Action = 'RESAMP_AVG' then begin
            if OpenADEM(true) then begin
               {$IfDef RecordCommandLine} WriteLineToDebugFile('dem opened'); {$EndIf}
               DEMGlb[DEM].ResampleByAveraging(false,Outfile);
               {$IfDef RecordCommandLine} WriteLineToDebugFile('resampled created'); {$EndIf}
            end;
         end;

         if (Action = 'LCP') then begin
            LeastCostPathOptions(1);
         end;

        {$IfDef IncludeGeoJSONexport}
             if (Action = 'DEM2JSON') then begin
                if OpenADEM then begin
                   DEMGlb[DEM].SaveAsGeoJSON;
                end;
             end;

             if (Action = 'LAS2JSON') then begin
                QuietActions := true;
                LAS2GeoJSON(infile);
             end;
        {$EndIf}

         DEM_Manager.CloseAllWindowsAndData;
         {$IfDef RecordCommandLine} WriteLineToDebugFile('ending command line ops'); {$EndIf}
         halt;
         wmdem.Close;
      end;


      function SetProgramOptions(TStr : ShortString) : boolean;
      begin
         Result := true;
         if (TStr = '-EXPERT')  then begin
            MDdef.ProgramOption := ExpertProgram;
         end
         else if (TStr = '-GEOLOGY') or (TStr = '-BRUNTON') then begin
            MDdef.ProgramOption := GeologyProgram;
            SetStructuralGeologyDefaults;
         end
         else if (TStr = '-MINIMIZE') then begin
            WindowState := wsMinimized;
         end
         else if (TStr = '-NOAUTOOPEN') or (TStr = '-AONOTHING') then begin
            MDdef.AutoOpen := aoNothing;
         end
         else if (TStr = '-RESET') then begin
            ProcessIniFile(iniInit);
         end
         else if (TStr = '-NODEBUG') then begin
            MDdef.MDRecordDebugLog := false;
         end
         else if (TStr = '-DEBUG') then begin
            MDdef.MDRecordDebugLog := true;
         end
         else if (TStr = '-RESET') then begin
            ProcessIniFile(iniInit);
         end
         else begin
            Result := false;
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

      GetGDALFileNames;
      if not ValidPath(MapLibDir) then PickMapIndexLocation;

    {$IfDef NoCommandLineParameters}
    {$Else}
       if (ParamCount <> 0) then begin
          TStr := UpperCase(ptTrim(ParamStr(1)));
           if (TStr = '-FUVSSIM') then begin
              Self.Width := 750;
              Self.Height := 450;
              Self.Top := 100;
              Self.Left := 100;
              {$IfDef RecordProblems} WriteLineToDebugFile('Call FUV_SSIM_Processing'); {$EndIf}
              FUV_SSIM_Processing(dmFull,false,false);
              {$IfDef RecordProblems} WriteLineToDebugFile('Done FUV_SSIM_Processing, halting'); {$EndIf}
              Halt;
              exit;
           end;
          if not SetProgramOptions(TStr) then begin
              if (ParamCount = 1) and (TStr[1] = '?') then begin
                 ProcessCommandLine(TStr);
              end
              else begin
                  TStr := '?' + TStr;
                  for i := 2 to ParamCount do TStr := TStr + '+' + UpperCase(ptTrim(ParamStr(i)));
                  ProcessCommandLine(TStr);
              end;
          end;
       end;
    {$EndIf}

    TerSplsh.MDStartSplashing;

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

   PetImage.LoadWinGraphColors;

   WmDEM.StatusBar1.Panels[0].Text := '';
   SetMenusForVersion;

     if ClonedEXE then begin
        Self.Width := 800;
        Self.Height := 600;
        Self.Top := 100;
        Self.Left := 100;
        {$If Defined(RecordFormResize) or Defined(RecordFormActivate)} WriteLineToDebugFile('Twmdem.FormActivate MDn set, width=' + IntToStr(Width) + '  & height=' + IntToStr(Height)); {$EndIf}
     end;

   {$If Defined(RecordFormResize) or Defined(TrackFormCreate)} WriteLineToDebugFile('Twmdem.FormActivate set menu versions'); {$EndIf}
   SeeIfThereAreDebugThingsToDo;
   Self.Visible := true;
   {$If Defined(RecordFormResize) or Defined(TrackFormCreate)} WriteLineToDebugFile('Twmdem.FormActivate end, width=' + IntToStr(Width) + '  & height=' + IntToStr(Height)); {$EndIf}
end;


procedure Twmdem.SeeIfThereAreDebugThingsToDo;
//allows immediate execution of code for debugging
begin
end;


function GetLanguageCaption(aTable : tMyData; Text,Language : shortstring) : shortstring;
begin
   ATable.ApplyFilter('TEXT=' + QuotedStr(Text));
   if Atable.FiltRecsInDB > 0 then begin
      Result := Atable.GetFieldByNameAsString(Language);
   end;
end;


procedure Twmdem.FormCreate(Sender: TObject);
begin
   {$If Defined(MessageStartup) or Defined(TrackFormCreate)} MessageToContinue('start wmdem FormCreate'); {$EndIf}
   ClientWidth := 4000;
   ClientHeight := 2400;
   ProgramClosing := false;
   NoAutoOpen := false;
   Self.Visible := false;
   {$IfDef IncludeCoastalDEMs} InventoryDILUVIUMbytestarea1.Visible := true; {$Else} InventoryDILUVIUMbytestarea1.Visible := false; {$EndIf}
   {$If Defined(MessageStartup) or Defined(TrackFormCreate)} MessageToContinue('FormCreate out'); {$EndIf}
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


procedure Twmdem.AddEXIFtagsworkinprogress1Click(Sender: TObject);
(*
var
   TheAuthor,TheSubject : shortstring;
   FilesWanted : tStringList;
   DefaultFilter : byte;
   f : integer;
   LatDeg,LatMin,LongDeg,LongMin :LongWord;
   LatSec,LongSec : Currency;

      procedure ProcessFile(FileName : PathStr);
      var
        ExifData: TExifData;
      begin
        ExifData := TExifData.Create;
        try
          ExifData.LoadFromGraphic(FileName);
          if (TheAuthor <> '') then ExifData.Author := TheAuthor;
          if (TheSubject <> '') then ExifData.Subject := TheSubject;
          //ExifData.SetKeyWords(['tennis', 'Wimbledon', 'match', 'SW19']);
          ExifData.GPSLatitude.Assign(LatDeg,LatMin,LatSec, ltNorth);
          ExifData.GPSLongitude.Assign(LongDeg,LongMin,LongSec, lnWest);
          ExifData.SaveToGraphic(FileName);
        finally
          ExifData.Free;
        end;
      end;

begin
   TheAuthor := 'Peter L. Guth DS69';
   TheSubject := 'DS Reunion 2024';

   LatDeg := 37;
   LatMin := 22;
   LatSec := 26.5;
   LongDeg := 117;
   LongMin := 58;
   LongSec := 47.8;

   DefaultFilter := 1;
   FilesWanted := tStringList.Create;
   FilesWanted.Add(PhotoDir);
   if GetMultipleFiles('Add Exif metadata','AnyFile|*.*',FilesWanted, DefaultFilter) then begin
       for f := 0 to pred(FilesWanted.Count) do begin
          ProcessFile(FilesWanted.Strings[f]);
       end;
   end;
   FilesWanted.Destroy;
*)
begin
   AddEXIFfields;
end;

procedure Twmdem.Addlegendonlylastgraph1Click(Sender: TObject);
begin
   AllGraphsOneImage(-99,true,false);
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


procedure Twmdem.Areaevaluations1Click(Sender: TObject);
begin
   StartAreaEvals;
end;


procedure Twmdem.Bringpointcloudcontroltofront1Click(Sender: TObject);
begin
   pt_cloud_opts_fm.BringToFront;
   pt_cloud_opts_fm.Left := Self.Left + 10;
   pt_cloud_opts_fm.Top := Self.Top + 10;
end;

procedure Twmdem.Bringslicecontroltofront1Click(Sender: TObject);
begin
   SlicerForm.BringToFront;
   SlicerForm.Left := Self.Left + 10;
   SlicerForm.Top := Self.Top + 10;
end;


procedure Twmdem.N3DEPfileswithtag421121Click(Sender: TObject);
begin
   FindFilesWith42112;
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


procedure Twmdem.N60Click(Sender: TObject);
begin
   GetDEMIXpaths;
   VerifyRecordsToUse(DEMIX_criteria_tolerance_fName,'PARAMETER');
end;

procedure Twmdem.N62Click(Sender: TObject);
var
   theDEMs : tStringList;
   DataDir,fName : PathStr;
   i,j : integer;
begin
    DataDir := '';
    theDEMs := GetFileNamesOfDEMinUse(DataDir);
    for i := 0 to pred(TheDEMs.Count) do begin
       fName := TheDEMs[i];
       LoadNewDEM(j,fName,true);
    end;
end;

procedure Twmdem.N64Click(Sender: TObject);
const
   DEMs : array[1..4] of shortstring = ('ref_dtm','cop','fathom','gedtm');
var
   Paths : array[1..4] of PathStr;
   TheGrids : array[1..4] of integer;
   RefName,fName,BasePath : PathStr;
   theDEMs,Findings : tStringList;
   i,j,k : integer;
   aLine : shortstring;
begin
   BasePath := 'J:\aaa_neo_eval\silver_peak_range\curvature\';
   for i := 1 to 4 do Paths[i] := BasePath + DEMs[i] + '_d3\';
   Findings := tStringList.Create;
   aLine := 'CURVATURE';
   for j := 2 to 4 do aline := aline + ',' + 'FUV_' + Uppercase(DEMs[j]);
   Findings.Add(aLine);
   TheDEMs := nil;
   Petmar.FindMatchingFiles(Paths[1],'*.tif',TheDEMs,1);
   for i := 0 to pred(TheDEMs.Count) do begin
      RefName := TheDEMs.Strings[i];
      LoadNewDEM(TheGrids[1],RefName,false);
      aline := StringReplace(ExtractFileNameNoExt(RefName),'ref_dtm_','',[rfReplaceAll, rfIgnoreCase]);
      for j := 2 to 4 do  begin
        FName := Paths[j] + ExtractFileName(RefName);
        fName := StringReplace(fName,DEMs[1],DEMs[j],[rfReplaceAll, rfIgnoreCase]);
        LoadNewDEM(TheGrids[j],fName,false);

        aline := aline + ',' + RealToString(GetFUVForPairGrids(DEMglb[TheGrids[1]].FullDEMGridLimits,TheGrids[1],TheGrids[j]),-12,6);
      end;
      WriteLineToDebugFile(aLine);
      Findings.Add(aline);
      for k := 1 to 4 do begin
         CloseSingleDEM(j);
      end;
   end;
   RefName := MDTempDir + 'curvature_fuvs.dbf';
   StringList2CSVtoDB(Findings,RefName);
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
   DoElevationHistograms;
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
           {$IfDef RecordDBFconvert} WriteLineToDebugFile('File already exists ' + NewName); {$EndIf}
        end
        else begin
           if FileExists(OldName) then begin
              ConvertDBFtoSQLite(OldName);
           end
           else begin
              {$IfDef RecordDBFconvert} WriteLineToDebugFile('old file missing ' + OldName); {$EndIf}
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

procedure Twmdem.Graphdifferencedistributionsalllandcovers1Click(Sender: TObject);
begin
   CrossScaleDEMComparison(3);
end;

procedure Twmdem.Graphevaluationandscores1Click(Sender: TObject);
var
   db : integer;
begin
   StopSplashing;
   GetDEMIXpaths(false);
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


procedure Twmdem.Skipifpresent1Click(Sender: TObject);
begin
   {$IfDef IncludeVectorCriteria}
      ClassificationAgreement(false);
   {$EndIf}
end;

procedure Twmdem.Skipifpresent2Click(Sender: TObject);
begin
   {$IfDef IncludeVectorCriteria}
      ChannelNetworkMissPercentages(False);
   {$EndIf}
end;

procedure Twmdem.NewDEMButtonClick(Sender: TObject);
begin
   {$IfDef RecordMenu} WriteLineToDebugFile('Twmdem.NewDEMButtonClick'); {$EndIf}
   Newarea1Click(Sender);
end;


procedure Twmdem.NewglobalgridGreenwich1Click(Sender: TObject);
begin
   CreateNewGlobalGrid(PixelIsPoint,true,true);
end;

procedure Twmdem.NewglobalgridIDL1Click(Sender: TObject);
begin
   CreateNewGlobalGrid(PixelIsPoint,true,false);
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


procedure Twmdem.MovemergedtilereferenceandtestDEMs1Click(Sender: TObject);
var
   MergedDir,MainDir,Dest,TileDir,ThisDir,ThisDest: PathStr;
   AreaName : ShortString;
   Merges,Tiles,ManyMerges : tStringList;
   i,j,k : integer;
begin

MessageToContinue('Check carefully; last time this did not work');

   MergedDir := 'G:\demix_utm_tiles\';
   MainDir := 'G:\demix_utm_tiles\';
   ManyMerges := tStringList.Create;
   ManyMerges.Add(MergedDir);
   GetMultipleDirectories('Area with misplaced merged tiles', ManyMerges);
   for k := 0 to pred(ManyMerges.Count) do begin
       MergedDir := ManyMerges.Strings[k];
       AreaName := LastSubDir(MergedDir);
       Merges := GetSubDirsInDirectory(MergedDir);
       for I := 0 to pred(Merges.Count) do begin
           TileDir := MergedDir + Merges.Strings[i] + '\' +  Merges.Strings[i] +  '_ref_test_dem';
           System.IOUtils.TDirectory.Move(TileDir,MergedDir);
           System.IOUtils.TDirectory.Delete(Dest + AreaName);
       end;
   end;
   ManyMerges.Destroy;
exit;

MessageToContinue('Check carefully; last time this did not work');

   MergedDir := 'G:\demix_utm_tiles\aa_hrd_merges\';
   MainDir := 'G:\demix_utm_tiles\';
   ManyMerges := tStringList.Create;
   ManyMerges.Add(MergedDir);
   GetMultipleDirectories('Area with merged tiles', ManyMerges);
   for k := 0 to pred(ManyMerges.Count) do begin
       MergedDir := ManyMerges.Strings[k];
       AreaName := LastSubDir(MergedDir);
       Dest := MainDir + AreaName + '\';                // "G:\demix_utm_tiles\aa_hrd_merges\mx_mexico"
       SafeMakeDir(Dest);

       Merges := GetSubDirsInDirectory(MergedDir);
       for I := 0 to pred(Merges.Count) do begin
          ThisDir := MergedDir + Merges.Strings[i] + '\';     // "G:\demix_utm_tiles\aa_hrd_merges\mx_mexico\mx_h11b11"
          Tiles := GetSubDirsInDirectory(ThisDir);
          for j := 0 to pred(Tiles.Count) do begin
             TileDir := ThisDir + Tiles.Strings[j];    // "G:\demix_utm_tiles\aa_hrd_merges\mx_mexico\mx_h11b12\mx_h11b12_ref_test_dem"
             if StrUtils.AnsiContainsText(TileDir,'_ref_test_dem') then begin
                ThisDest := Dest + {Merges.Strings[i] +} '\' + Tiles.Strings[j];
                System.IOUtils.TDirectory.Move(TileDir,ThisDest);
             end;
          end;
          Tiles.Destroy;
       end;
       Merges.Destroy;
   end;
   ManyMerges.Destroy;
end;

procedure Twmdem.CorrelationmatricesamongallDEMsjustALLlandcover1Click(Sender: TObject);
begin
   CrossScaleDEMComparison(7);
end;

procedure Twmdem.Correlationmatrix1Click(Sender: TObject);
var
   fName : PathStr;
begin
   StopSplashing;
   fName := '';
   if GetFileFromDirectory('Correlation/distance matrix','*.csv',fName) then OpenCorrelationMatrix(ExtractFileNameNoExt(fName),fName);
end;


procedure Twmdem.CorrelationmatrixamongallDEMsforALLpixels1Click(Sender: TObject);
begin
   CrossScaleDEMComparison(1);
end;

procedure Twmdem.CorrelationmatrixsingleDEMtoallothersALLlandcoveronly1Click(Sender: TObject);
begin
   CrossScaleDEMComparison(7);
end;

procedure Twmdem.CorrelationsingleDEMtoreferencealllandcovers1Click(Sender: TObject);
begin
   CrossScaleDEMComparison(2);
end;

procedure Twmdem.Createcompositebitmap1Click(Sender: TObject);
begin
   RestoreBigCompositeBitmap('');
end;

procedure Twmdem.Createcompositebitmap2Click(Sender: TObject);
begin
   CreateBigCompositeBitmap;
end;

procedure Twmdem.CreateDEMsfromlidar1Click(Sender: TObject);
begin
   CreateDEMsfromLidar;
end;


procedure Twmdem.CreatefinalDB1Click(Sender: TObject);
begin
   CreateFinalDiffDistDB;
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
   {$IfDef IncludeCoastalDEMs} CheckLowElevationAreas; {$EndIf}
end;

procedure Twmdem.InventoryofLC10files1Click(Sender: TObject);
begin
   MakeDBwithLC10FileInventory;
end;

procedure Twmdem.Inventorysourcefiles1Click(Sender: TObject);
begin
   DEMIX_UTM_based_processing(udSourceFileSurvey);
end;

procedure Twmdem.InventorySSIMFUVCSVfiles1Click(Sender: TObject);
begin
   InventoryDEMIX_SSIM_FUV_Stats;
end;

procedure Twmdem.InventorytestandrefereneDEMsbytestarea1Click(Sender: TObject);
begin
   //InventoryTestAndReferenceDEMs;
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
   {$IfDef ExPointCloud}
   {$Else}
   StopSplashing;
   if GetFileMultipleMask('3D GIS project', 'Project|proj*.dbf|Other GIS formats|*.dbf;*.shp|Text or csv|*.txt;*.csv|Unregistered LAS|*.las',DEMDefs.VasaProjectFName,MDDef.PCDefaultFilter) then begin
      Slicer_3D.DB_3dSlices(Nil,Nil,Nil);
   end;
  {$EndIf}
end;

procedure Twmdem.Overwirte1Click(Sender: TObject);
begin
   //DEMIX_Ref_DEM_full_chain(True);
end;

procedure Twmdem.Overwirte2Click(Sender: TObject);
begin
   ComputeDEMIX_Diff_Dist_tile_stats(true);
end;

procedure Twmdem.Overwirte3Click(Sender: TObject);
begin
   ComputeDEMIX_Diff_Dist_tile_stats(false);
end;


procedure Twmdem.Overwirteifexists1Click(Sender: TObject);
begin
  //DEMIX_CreateReferenceDEMs(true,ResampleModeOneSec);
end;

procedure Twmdem.Overwriteifexists1Click(Sender: TObject);
begin
    //DEMIX_CreateReferenceDEMsFromSource(true);
end;

procedure Twmdem.Overwriteifexists2Click(Sender: TObject);
begin
   //DEMIX_CreateReferenceDEMsFromSource(false);
end;

procedure Twmdem.Overwriteifexits1Click(Sender: TObject);
begin
   //DEMIX_MergeReferenceDEMs(True);
end;

procedure Twmdem.Overwriteifexits2Click(Sender: TObject);
begin
   //DEMIX_MergeReferenceDEMs(false);
end;

procedure Twmdem.Overwriteifexits3Click(Sender: TObject);
begin
   //DEMIX_vert_datum_code := 5703;
   //DEMIX_GDAL_Ref_DEM_datum_shift(true);
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
   {$IfDef RecordClosing} WriteLineToDebugFile('Twmdem.Closeallgraphs1Click in'); {$EndIf}
   for i := pred(WMDEM.MDIChildCount) downto 0 do begin
      if WMDEM.MDIChildren[i] is TThisBaseGraph then begin
         {$IfDef RecordClosing} WriteLineToDebugFile('Close ' + (WMDEM.MDIChildren[i] as TThisBaseGraph).Caption); {$EndIf}
         (WMDEM.MDIChildren[i] as TThisBaseGraph).Close;
      end;
      if WMDEM.MDIChildren[i] is TNetForm then begin
         {$IfDef RecordClosing} WriteLineToDebugFile('Close ' + (WMDEM.MDIChildren[i] as TNetForm).Caption); {$EndIf}
         (WMDEM.MDIChildren[i] as TNetForm).Close;
      end;
   end;
   {$IfDef RecordClosing} WriteLineToDebugFile('Twmdem.Closeallgraphs1Click in'); {$EndIf}
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
      if WMDEM.MDIChildren[i] is TImageDispForm then begin
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
   {$IfDef RecordClosing} WriteLineToDebugFile('Twmdem.Closeallpictureviewwindows1Click in'); {$EndIf}
   for i := pred(WMDEM.MDIChildCount) downto 0 do begin
      if WMDEM.MDIChildren[i] is TImageDispForm then begin
          {$IfDef RecordClosing} WriteLineToDebugFile((WMDEM.MDIChildren[i] as TImageDispForm).Caption); {$EndIf}
          (WMDEM.MDIChildren[i] as TImageDispForm).Close;
      end;
   end;
   {$IfDef RecordClosing} WriteLineToDebugFile('Twmdem.Closeallpictureviewwindows1Click out'); {$EndIf}
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

procedure Twmdem.CloneEXE1Click(Sender: TObject);
var
   ch : ANSIChar;
   NewName : PathStr;
begin
  for ch := '2' to '7' do begin
     NewName := ProgramRootDir + 'MD' + ch + '.exe';
     DeleteFileIfExists(NewName);
     Petmar.CopyFile(Application.ExeName, NewName);
  end;
end;

procedure Twmdem.CloneEXEformultithreading1Click(Sender: TObject);
begin
   CloneEXE1Click(Sender);
   DEMIX_UTM_based_processing(udClearAreaLocks);
end;

procedure Twmdem.CloseAlldataandwindows1Click(Sender: TObject);
begin
   DEM_Manager.CloseAllWindowsAndData;
   Closeallpictureviewwindows1Click(Sender);
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
   if not FileExists(DEMIX_GIS_dbName) then Petmar.GetExistingFileName('DEMIX db version','*.dbf',DEMIX_GIS_dbName);
   OpenNumberedGISDataBase(DEMIX_DB,DEMIX_GIS_dbName,false);
   if ValidDB(DEMIX_DB) then begin
      GetDEMIXpaths(false,DEMIX_DB);
      GISdb[DEMIX_DB].LayerIsOn := false;
      DoDEMIXFilter(DEMIX_DB);
   end;
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
      ExecuteFile('https://www.usna.edu/Users/oceano/pguth/md_help/html/microdem.htm');
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


procedure Twmdem.PicktestDEMs1Click(Sender: TObject);
begin
   GetDEMIXpaths;
   VerifyRecordsToUse(DemixSettingsDir + 'demix_dems.dbf','SHORT_NAME');
end;

procedure Twmdem.Pixelbypixelmapstatistics1Click(Sender: TObject);
begin
   {$IfDef CopALOSCompare}
      PixelByPixelCopAlos;
   {$EndIf}
end;

procedure Twmdem.Percentlandcovercategories1Click(Sender: TObject);
begin
   DEMIX_UTM_based_processing(udLandcoverCats);
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



procedure Twmdem.SmallcloneEXEwindow1Click(Sender: TObject);
begin
   SmallClonePlacement;
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
   //DEMIX_CreateReferenceDEMsFromSource(true);
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
   FilesWanted.Destroy;
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
   FilesWanted.Destroy;
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
   FilesWanted.Destroy;
end;


procedure Twmdem.RestorepreviousprogramEXE1Click(Sender: TObject);
begin
   RestorePreviousEXE;
end;


procedure Twmdem.RGBcolorlayers1Click(Sender: TObject);
var
   im : TImageDispForm;
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


procedure Twmdem.Megathrusts1Click(Sender: TObject);
begin
   {$IfDef IncludeGeologyLabs} Afar1Click(Sender); {$EndIf}
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
   {$If Defined(ExGeoPDF)}
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


procedure Twmdem.Mergesmalltilesinto10kmUTMblocks1Click(Sender: TObject);
begin
   MoveSmallTilesInto10KTiles;
end;




procedure Twmdem.Mergesourcedatatiles1Click(Sender: TObject);
begin
   //DEMIX_merge_source;
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
                  DEMheader.h_DatumCode := 'Sphere';
                  DEMySpacing := 2.5 /60;
                  DEMxSpacing := 2.5 / 60;
                  SWCornerX := -180 + 0.5 * DEMxSpacing;
                  SWCornerY := -56 + 0.5 * DEMySpacing;
                  NumCol := 360 * 24;
                  NumRow := (60+56) * 24;
                  DataSpacing := SpaceDegrees;
                  DEMUsed := ArcSecDEM;
                  DEMheader.UTMZone := GetUTMZone(SWCornerX + 0.5 * NumCol * DEMxSpacing);
                  ElevUnits  := euUndefined;
                  AllocateDEMMemory(InitDEMMissing);
                  DefineDEMVariables(true);
                  {$IfDef RecordGeostats} WriteLineToDebugFile('New grid created ' + IntToStr(ParamDEMs[i])); {$EndIf}
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
   if GetMultipleFiles('Geotiff or IMG or SHP','*.TIF;*.TIFF;*.IMG;*.shp',FilesWanted ,DefFilter) then begin
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


procedure Twmdem.Averagetileslopemapgeneration1Click(Sender: TObject);
begin
   DEMIX_UTM_based_processing(udAverageSlopeMap);
end;

procedure Twmdem.ESRIshapefile1Click(Sender: TObject);
begin
   if GetFileFromDirectory('ESRI shapefile','*.shp',LastDataBase) then ScreenShapeFileDump(LastDataBase);
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



procedure Twmdem.ilecharacterization1Click(Sender: TObject);
begin
   TileCharacterization;
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


procedure Twmdem.MultipleDEMsonearea015secscale1Click(Sender: TObject);
begin
   //FUVforMultipleTestDEMstoReference;
end;


procedure Twmdem.MultProfSpeedButtonClick(Sender: TObject);
begin
   SimpleProfiles;
end;


procedure Twmdem.Differencedistributionstatistics1Click(Sender: TObject);
begin
   CrossScaleDEMComparison(4);
end;

procedure Twmdem.FUV2Click(Sender: TObject);
begin
   CrossScaleDEMComparison(5);
end;

procedure Twmdem.FUVbyLandcover1DEMtoreference1Click(Sender: TObject);
begin
   {$IfDef FUV_RangeScales}
      FUVforRangeScales(true);
   {$Else}
      MessageToContinue('Currently disabled');
   {$EndIf}
end;

procedure Twmdem.FUVfor5DEMstoreference1Click(Sender: TObject);
begin
   //FUVforMultipleTestDEMstoReference;
end;

procedure Twmdem.FUVforrangescales1Click(Sender: TObject);
begin
   {$IfDef FUV_RangeScales}
      FUVforRangeScales(false);
   {$Else}
      MessageToContinue('Currently disabled');
   {$EndIf}
end;

procedure Twmdem.Differencetwobitmaps1Click(Sender: TObject);
begin
   DifferenceTwoBitmaps;
end;


procedure Twmdem.Discussionforum1Click(Sender: TObject);
begin
   ExecuteFile('http://forums.delphiforums.com/microdem/start/', '', '');
end;


procedure Twmdem.Download10x10kmblocks1Click(Sender: TObject);
begin
   PrepList10x10kmTiles;
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
         if (PredAgesDEM <> 0) then DEMGlb[PredAgesDEM].SetUpMap(false,mtElevSpectrum);
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
         if (SedThickDEM <> 0) then DEMGlb[LastDEMLoaded].SetUpMap(false,mtElevSpectrum);
      end;
   {$EndIf}
end;


procedure Twmdem.Seismicfencediagram1Click(Sender: TObject);
begin
   {$IfDef ExFMX3D}
   {$Else}
       SeismicTo3DView;
       DisplayHTMLTopic('html\fence_diagram.htm');
   {$EndIf}
end;

procedure Twmdem.Seismicviewing1Click(Sender: TObject);
begin
   {$IfDef ExFMX3D}
   {$Else}
      SeismicTo3DView;
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
                  RealToString(DEMGlb[aDEM].FindPercentileElev(50),-12,-4) + ',' +
                  RealToString(DEMGlb[aDEM].FindPercentileElev(5),-12,-4) + ',' +
                  RealToString(DEMGlb[aDEM].FindPercentileElev(10),-12,-4) + ',' +
                  RealToString(DEMGlb[aDEM].FindPercentileElev(25),-12,-4) + ',' +
                  RealToString(DEMGlb[aDEM].FindPercentileElev(75),-12,-4) + ',' +
                  RealToString(DEMGlb[aDEM].FindPercentileElev(90),-12,-4) + ',' +
                  RealToString(DEMGlb[aDEM].FindPercentileElev(95),-12,-4) + ',' +
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
      //ConvertForm.Edit1.Text := '37712.48';  //x
      //ConvertForm.Edit2.Text := '35242';  //y
      //ConvertForm.Edit3.Text := '13.339038461';  //lat
      //ConvertForm.Edit4.Text := '144.635331292';  //long

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
      ConvertForm.This_projection.InitProjFromWKTfile(MDDef.WKTLidarProj);
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


procedure Twmdem.FixGEDTMlateadditonproblems1Click(Sender: TObject);
begin
   DEMIX_UTM_based_processing(udInvalidTiles);
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


procedure Twmdem.UTMbaseaverageslopebygeometry1Click(Sender: TObject);
begin
   DEMIX_UTM_based_processing(udPixelGeometrySlope);
end;

procedure Twmdem.UTMbasedallareacomputations1Click(Sender: TObject);
begin
   DEMIX_UTM_based_processing(20);
end;

procedure Twmdem.UTMbasedCVSfilesbyarea1Click(Sender: TObject);
begin
   DEMIX_UTM_based_processing(udCSVReportByArea);
end;

procedure Twmdem.UTMbaseddifferencedistributionstatistics1Click(Sender: TObject);
begin
   DEMIX_UTM_based_processing(11);
end;

procedure Twmdem.UTMbasedfilefillreport1Click(Sender: TObject);
begin
   DEMIX_UTM_based_processing(udFileFillReport);
end;

procedure Twmdem.UTMbasedFUVcalculations1Click(Sender: TObject);
begin
   DEMIX_UTM_based_processing(udFUVCalc);
end;

procedure Twmdem.UTMbasedFUVcurvaturecalculations1Click(Sender: TObject);
begin
   DEMIX_UTM_based_processing(10);
end;

procedure Twmdem.UTMbasedFUVpartialscalculations1Click(Sender: TObject);
begin
   DEMIX_UTM_based_processing(9);
end;

procedure Twmdem.UTMbasedmergeallDB1Click(Sender: TObject);
begin
   DEMIX_UTM_based_processing(21);
end;

procedure Twmdem.UTMbasedmergecurvaturesintoDB1Click(Sender: TObject);
begin
   DEMIX_UTM_based_processing(13);
end;

procedure Twmdem.UTMbasedmergedifferencedistribution1Click(Sender: TObject);
begin
   DEMIX_UTM_based_processing(12);
end;

procedure Twmdem.UTMbasedmergeFUVresultsintoDB1Click(Sender: TObject);
begin
   DEMIX_UTM_based_processing(6);
end;

procedure Twmdem.UTMbasedmergepartialsintoDB1Click(Sender: TObject);
begin
   DEMIX_UTM_based_processing(14);
end;

procedure Twmdem.UTMbasedmergetilestatisiticsintoDB1Click(Sender: TObject);
begin
   DEMIX_UTM_based_processing(8);
end;

procedure Twmdem.UTMbasedtilescreation1Click(Sender: TObject);
begin
   DEMIX_UTM_based_processing(1);
end;

procedure Twmdem.UTMbasedtilestatistics1Click(Sender: TObject);
begin
   DEMIX_UTM_based_processing(udTileStats);
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

procedure Twmdem.Howbigisanarcsecond1Click(Sender: TObject);
begin
   HowBigIsAnArcSecond;
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

procedure Twmdem.LASdata1Click(Sender: TObject);
begin
   OverlayPointClouds(Nil);
   StopSplashing;
end;

procedure Twmdem.LASfile1Click(Sender: TObject);
{$IfDef ExPointCloud}
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
   {$IfDef RecordFormResize} WriteLineToDebugFile('Twmdem.FormResize in, ' + IntToStr(Width) + ' x ' + IntToStr(Height)); {$EndIf}
   inherited;
   SmallClonePlacement;
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
   //CoastalDTMforTestAreas(false);
end;

procedure Twmdem.Combineallcombinedimages1Click(Sender: TObject);
begin
   CombineAllPanelGraphs;
end;


procedure Twmdem.Compare10mand100mlandcover1Click(Sender: TObject);
begin
   DEMIX_UTM_based_processing(udCompareLandcover);
end;

procedure Twmdem.ComparelandcoverinpointcloudDTM1Click(Sender: TObject);
begin
   LandCoverBreakdowPointCloud;
end;

procedure Twmdem.CompareUTMandgeographicslopes1Click(Sender: TObject);
begin
   CompareGeoUTM;
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
   {$IfDef RecordClosing} WriteLineToDebugFile('WMainDEM finalization complete'); {$EndIf}
end.


