unit demdbtable;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   //{$Define RecordMaskDEMShapeFile}
   {$IfDef Debug}
       {$Define RecordDEMIX}
       {$Define RecordDetailedDEMIX}
       //{$Define RecordCloseDB}
       //{$Define RecordDataBaseSaveFiles}
       //{$Define RecordDBPlot}
       //{$Define RecordCSVOut}
       //{$Define RecordCopyFieldLinkDB}
       //{$Define RecordTiger}
       //{$Define RecordFan}
       //{$Define RecordIcons}
       //{$Define RecordShowStatus}
       //{$Define Recordzvalueplot}
       //{$Define RecordNavigation}
       //{$Define RecordEditsDone}
       //{$Define RecordEditDB}
       //{$Define RecordMaskDEMShapeFile}
       //{$Define RecordIceSat}
       //{$Define RecordKML}
       //{$Define RecordHTML}
       //{$Define RecordGeostats}
       //{$Define RecordMapSizing}
       //{$Define RecordExports}
       //{$Define RecordFieldAdds}
       //{$Define RecordStatus}
       //{$Define RecordGraph}
       //{$Define RecordClustering}
       //{$Define RecordGeoCoding}
       //{$Define RecordMultigrid}
       //{$Define RecordQuickFilter}
       //{$Define RecordFillDEM}
       //{$Define RecordDBGrid1DrawColumnCell}
       //{$Define RecordOnDEM}
       //{$Define RecordOnDEMProblemsIn}
       //{$Define RecordOnDEMProblemsOut}
       //{$Define RecordTerrainProfiles}
       //{$Define RecordSortTable}
       //{$Define RecordCorrelationMatrix}

       //{$Define RecordFormActivate}
       //{$Define RecordFieldRename}
       //{$Define RecordSatellite}

       //{$Define RecordKoppen}
       //{$Define RecordDistance}
       //{$Define RecordSideScan}
       //{$Define RecordDataSaveStatus}
       //{$Define RecordDataBase}
       //{$Define RecordDataBaseImage}
       //{$Define RecordGeology}
       //{$Define RecordShapeFileEdits}
       //{$Define ExportCoords}
       //{$Define RecordMakeLineArea}
       //{$Define RecordDBfilter}
       //{$Define RecordOpenGL}
       //{$Define FindNeighbors}
       //{$Define RecordFont}
       //{$Define AverageNeighbors}
       //{$Define AverageNeighborsFull}
       //{$Define CountUniqueValues}
       //{$Define RecordCurrentRecord}
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

  System.Math.Vectors,System.Types,System.RTLConsts, System.UITypes,
  Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.Dialogs, Vcl.Menus, Vcl.Buttons, Vcl.StdCtrls, Vcl.Controls,
  Windows,  SysUtils, Classes, Graphics,  Forms, StrUtils,Math, Grids, DBGrids,ClipBrd,

  {$IfDef ExFMX3D}
  {$Else}
     FMX.Types3D,FMX.Objects3D,
  {$EndIf}
  BaseGraf, Petmar_types,PETMAR,DEMDefs;

type
  Tdbtablef = class(TForm)
    StatusBar1: TStatusBar;
    StatsPopupMenu1: TPopupMenu;
    PlotPopupMenu2: TPopupMenu;
    EditPopupMenu8: TPopupMenu;
    Sumforonefield1: TMenuItem;
    Fieldcorrelations1: TMenuItem;
    N3Dgraph1: TMenuItem;
    ColorDialog1: TColorDialog;
    N2Dgraph1: TMenuItem;
    Longeststring1: TMenuItem;
    Colorcodebynumericfield1: TMenuItem;
    Statisticsgroupedbyonefield1: TMenuItem;
    Histogram1: TMenuItem;
    Listuniquevalues1: TMenuItem;
    ReportPopupMenu4: TPopupMenu;
    Text1: TMenuItem;
    HTML1: TMenuItem;
    Label1: TMenuItem;
    QueryPopupMenu5: TPopupMenu;
    Selectionregion1: TMenuItem;
    Currentmaparea1: TMenuItem;
    Linelength1: TMenuItem;
    Linelengthsforonefield1: TMenuItem;
    GridCellPopupMenu6: TPopupMenu;
    Editpointlocation1: TMenuItem;
    Recentermaponrecord1: TMenuItem;
    Colorpoint1: TMenuItem;
    Highlightrecordonmap1: TMenuItem;
    WWW1: TMenuItem;
    Colorallrecords1: TMenuItem;
    Coloruncoloredrecords1: TMenuItem;
    Colorbasedonfield1: TMenuItem;
    N2: TMenuItem;
    Linkdatabase1: TMenuItem;
    N4: TMenuItem;
    N5: TMenuItem;
    Associateimage1: TMenuItem;
    Timer1: TTimer;
    N7: TMenuItem;
    Highlightcolor1: TMenuItem;
    N8: TMenuItem;
    Deleterecord1: TMenuItem;
    Linelength2: TMenuItem;
    Fanproperties1: TMenuItem;
    PopupMenu9: TPopupMenu;
    Plotfanlocations1: TMenuItem;
    DBFfile1: TMenuItem;
    N9: TMenuItem;
    Vectordata1: TMenuItem;
    N2Dgraphcolorcoded1: TMenuItem;
    Rosediagram1: TMenuItem;
    Calculatearea1: TMenuItem;
    Recordcoordinates1: TMenuItem;
    Editpointsymbol1: TMenuItem;
    Keyboardnewpointlocation1: TMenuItem;
    RecordDisplay1: TMenuItem;
    Recordscreencoordinates1: TMenuItem;
    Dipandstrikes1: TMenuItem;
    N10: TMenuItem;
    Redistrict1: TMenuItem;
    Recordedit1: TMenuItem;
    N11: TMenuItem;
    Coordinatestotextfile1: TMenuItem;
    N2Dgraphcolorcodetext1: TMenuItem;
    Focalmechanisms1: TMenuItem;
    Earthquakemechanisms1: TMenuItem;
    Ternarydiagram1: TMenuItem;
    Labelselectedrecords1: TMenuItem;
    Graphicallymovepoints1: TMenuItem;
    Graphicalpickandeditdatabase1: TMenuItem;
    Editlineweightandcolor1: TMenuItem;
    Coloralllinesegments1: TMenuItem;
    Coloralluncoloredlinesegments1: TMenuItem;
    Deleteallrecords1: TMenuItem;
    Forceredrawofmap1: TMenuItem;
    Insertrecord1: TMenuItem;
    Editrecordsingrid1: TMenuItem;
    Earthquakemechanisms2: TMenuItem;
    Beachballs1: TMenuItem;
    Histogrambyuniquevalues1: TMenuItem;
    Editfillpattern1: TMenuItem;
    Momentstatistics1: TMenuItem;
    Showarearecords1: TMenuItem;
    Font1: TMenuItem;
    CalculateVolume1: TMenuItem;
    Proportionalsquares1: TMenuItem;
    N12: TMenuItem;
    Shapefilesubset1: TMenuItem;
    N13: TMenuItem;
    Elevationsatbenchmarks1: TMenuItem;
    Stationtimeseries1: TMenuItem;
    Histogram2: TMenuItem;
    N2series1: TMenuItem;
    N3series1: TMenuItem;
    Stratigraphiccolumn1: TMenuItem;
    Animatefield1: TMenuItem;
    dbfStruct: TMenuItem;
    PlotXYFile1: TMenuItem;
    PlotallXYFiles1: TMenuItem;
    Maskdatabasewithshapefile1: TMenuItem;
    Countuniquevalues1: TMenuItem;
    Fillfieldforallrecords1: TMenuItem;
    Timeseries1: TMenuItem;
    FFT1: TMenuItem;
    Autocorrelation1: TMenuItem;
    Crosscorrelation1: TMenuItem;
    Fitfouriercurve1: TMenuItem;
    f1: TMenuItem;
    Singlefield2: TMenuItem;
    Allfields2: TMenuItem;
    Selectirregularregion1: TMenuItem;
    Singlefield1: TMenuItem;
    Twofields1: TMenuItem;
    Bylatitude1: TMenuItem;
    Bylongitude1: TMenuItem;
    Earthquakefocalmechanisms3D1: TMenuItem;
    Fieldcooccurrencematrix1: TMenuItem;
    Symmetric1: TMenuItem;
    Asymmetric1: TMenuItem;
    Addicons1: TMenuItem;
    Createlineshapefilefrompoints1: TMenuItem;
    AddelevationfromDEM1: TMenuItem;
    Addazimuthtotravelpath1: TMenuItem;
    Thindatabase1: TMenuItem;
    Getazimuthfrommagneticheading1: TMenuItem;
    Mutliple1: TMenuItem;
    Addconstanttofield1: TMenuItem;
    Legend1: TMenuItem;
    Changefieldsused1: TMenuItem;
    NearTIGERroads1: TMenuItem;
    AwayfromTIGERroads1: TMenuItem;
    Medianfilterfield1: TMenuItem;
    Distancefrompoint1: TMenuItem;
    Fieldarithmetic1: TMenuItem;
    Sumtwofields1: TMenuItem;
    Differencetwofields1: TMenuItem;
    Quotienttwofields1: TMenuItem;
    Addlatlongfromlinkeddatabase1: TMenuItem;
    AddDISTANCEAZIMUTHfields1: TMenuItem;
    N1: TMenuItem;
    N3: TMenuItem;
    Color1: TMenuItem;
    N6: TMenuItem;
    Cluster1: TMenuItem;
    XML1: TMenuItem;
    Insertcolorfield1: TMenuItem;
    Normalizefield1: TMenuItem;
    MaskDEMfromshapefile1: TMenuItem;
    Pointseparation1: TMenuItem;
    N2Dgraph2series1: TMenuItem;
    Plot: TMenuItem;
    AddslopefromDEM1: TMenuItem;
    Connectsequentialpoints1: TMenuItem;
    rimblanksinstringfield1: TMenuItem;
    Mask: TMenuItem;
    Labelsensors1: TMenuItem;
    Selectradiusaboutpoint1: TMenuItem;
    Maskdatabasewithgazetteer1: TMenuItem;
    Panel2: TPanel;
    BitBtn1: TBitBtn;
    BitBtn4: TBitBtn;
    Button1: TButton;
    BitBtn5: TBitBtn;
    BitBtn8: TBitBtn;
    BitBtn7: TBitBtn;
    Button5: TButton;
    BitBtn12: TBitBtn;
    Button4: TButton;
    BitBtn9: TBitBtn;
    HelpBtn: TBitBtn;
    CheckBox1: TCheckBox;
    Panel3: TPanel;
    Edit1: TEdit;
    Label2: TLabel;
    BitBtn11: TBitBtn;
    USEfield1: TMenuItem;
    MarkallY1: TMenuItem;
    MarkallN1: TMenuItem;
    Changelatlongfields1: TMenuItem;
    Principalcomponents1: TMenuItem;
    N14: TMenuItem;
    Exportpointstoregistrationtable1: TMenuItem;
    KML1: TMenuItem;
    Centr1: TMenuItem;
    Findneighbors1: TMenuItem;
    Showneighbors1: TMenuItem;
    IgnoreMissingData1: TMenuItem;
    Perspectiveview1: TMenuItem;
    PanoramaView1: TMenuItem;
    Setjoin1: TMenuItem;
    Showjoin1: TMenuItem;
    Clearjoin1: TMenuItem;
    InsertMASKfield1: TMenuItem;
    Insertfield1: TMenuItem;
    Grpahicallyinsertpoint1: TMenuItem;
    Sumofneighbors1: TMenuItem;
    Areaofeachrecord1: TMenuItem;
    Requiredantennaheight1: TMenuItem;
    FieldTitlePopupMenu7: TPopupMenu;
    Fieldstatistics1: TMenuItem;
    Sum1: TMenuItem;
    Createpointshapefile1: TMenuItem;
    Historgram1: TMenuItem;
    Listuniquevalues2: TMenuItem;
    Longeststring2: TMenuItem;
    Logoffield1: TMenuItem;
    DataDBFonlynogeometry1: TMenuItem;
    Layersymbology1: TMenuItem;
    ArcGISviewshedsensors1: TMenuItem;
    Panel1: TPanel;
    BitBtn6: TBitBtn;
    BitBtn10: TBitBtn;
    BitBtn14: TBitBtn;
    LOSButton: TSpeedButton;
    PanoramaSpeedButton: TSpeedButton;
    BitBtn15: TBitBtn;
    PerspectiveButton: TSpeedButton;
    BitBtn16: TBitBtn;
    Recolorfan1: TMenuItem;
    Recolorallfans1: TMenuItem;
    N15: TMenuItem;
    Legend2: TMenuItem;
    Editfont1: TMenuItem;
    ClearDBsettings1: TMenuItem;
    BitBtn17: TBitBtn;
    PopupMenu10: TPopupMenu;
    Picklineonmap1: TMenuItem;
    Picklinefromfile1: TMenuItem;
    Selectlinestodisplay1: TMenuItem;
    N16: TMenuItem;
    Indexnewlines1: TMenuItem;
    Createuniticons1: TMenuItem;
    Concatenatestringfields1: TMenuItem;
    FillField1: TMenuItem;
    Sum2: TMenuItem;
    Allrecordsmatchingsinglefield1: TMenuItem;
    Inserttimeanimationfields1: TMenuItem;
    BitBtn18: TBitBtn;
    BitBtn19: TBitBtn;
    N3Dshapefileprofile1: TMenuItem;
    DEMTerrainprofile1: TMenuItem;
    Octree1: TMenuItem;
    Thindatabase2: TMenuItem;
    Plot1series1: TMenuItem;
    ExportXYZtriples1: TMenuItem;
    CompareshapefileandDEMprofiles1: TMenuItem;
    Creategrid1: TMenuItem;
    Shiftpointrecords1: TMenuItem;
    AddUTMcoordfields1: TMenuItem;
    N3Dslicer1: TMenuItem;
    Mask1: TMenuItem;
    Centroidofeachrecord1: TMenuItem;
    Lengthofeachrecord1: TMenuItem;
    N17: TMenuItem;
    EvaluateXYProfiles1: TMenuItem;
    Frequencytable1: TMenuItem;
    CopyCoordinates: TMenuItem;
    InsertNewRecClipboard: TMenuItem;
    Stringfields1: TMenuItem;
    Copyfieldtostringfield1: TMenuItem;
    Addleadingzeros1: TMenuItem;
    Pastecoordinatesfromclipboard1: TMenuItem;
    Rangecircles1: TMenuItem;
    Insertdatefield1: TMenuItem;
    Findpointsinareas1: TMenuItem;
    ogglerecords1: TMenuItem;
    Insertlinecolorwidthfields1: TMenuItem;
    Insertareacolorsymbology1: TMenuItem;
    Loadfrommaplibrary1: TMenuItem;
    AllDBs1: TMenuItem;
    Allfields1: TMenuItem;
    Exportlatlongz1: TMenuItem;
    Fillpatternallrecords1: TMenuItem;
    Fillpatternalluncoloredrecords1: TMenuItem;
    Sortfield1: TMenuItem;
    Date1: TMenuItem;
    Splitdatefield1: TMenuItem;
    Addfontdefinition1: TMenuItem;
    Insertpointsymbol1: TMenuItem;
    Fillfontfield1: TMenuItem;
    Fieldpercentiles1: TMenuItem;
    Icons1: TMenuItem;
    Font2: TMenuItem;
    N18: TMenuItem;
    N19: TMenuItem;
    EditIcon1: TMenuItem;
    AddXYZfromjoinedtable1: TMenuItem;
    ColorfromRGBfloatfields1: TMenuItem;
    Resetjoin1: TMenuItem;
    Splitefield1: TMenuItem;
    Keepafte1: TMenuItem;
    Keepbefore1: TMenuItem;
    BitBtn20: TBitBtn;
    Mergedatabase1: TMenuItem;
    Dividefieldbyconstant1: TMenuItem;
    Copynumericfield1: TMenuItem;
    Mapcoordinates1: TMenuItem;
    AddMercator1: TMenuItem;
    AddMGRS1: TMenuItem;
    Fix360longs1: TMenuItem;
    N20: TMenuItem;
    ConvertUTMtolatlong1: TMenuItem;
    BitBtn21: TBitBtn;
    TimefieldHHMMSStohours1: TMenuItem;
    DEM1: TMenuItem;
    AddelevationdifferencefromDEM1: TMenuItem;
    Findneighborsinseconddatabase1: TMenuItem;
    Insertnewrecordatdistancebearing1: TMenuItem;
    DEMTerrainblowups1: TMenuItem;
    Viewshedfields1: TMenuItem;
    TrackBar1: TTrackBar;
    DechourstoHHMMSS1: TMenuItem;
    Timesequence1: TMenuItem;
    Timefieldshourminsectodechours1: TMenuItem;
    Timefieldstodecdays1: TMenuItem;
    SplittimestringHHMMSS1: TMenuItem;
    Renamefield1: TMenuItem;
    BitBtn22: TBitBtn;
    Features: TCheckBox;
    N21: TMenuItem;
    Sigmatee1: TMenuItem;
    Soundvelocity1: TMenuItem;
    Oceanography1: TMenuItem;
    Sigmatheta1: TMenuItem;
    Fillviewshedfields1: TMenuItem;
    CurrentflterYrestN1: TMenuItem;
    HLCheckbox: TCheckBox;
    Highlightfan1: TMenuItem;
    Plotfanlocations2: TMenuItem;
    Labelfans1: TMenuItem;
    Enablealloptions1: TMenuItem;
    InsertYEARMONTHDATE1: TMenuItem;
    MilitarytimetoHHMMSS1: TMenuItem;
    BitBtn2: TBitBtn;
    Quickfilter1: TMenuItem;
    Photolocations1: TMenuItem;
    Outlinecameraview1: TMenuItem;
    BitBtn23: TBitBtn;
    Copyshapefile1: TMenuItem;
    Zoommaptorecord1: TMenuItem;
    Perimeterofeachrecord1: TMenuItem;
    FindrecordsonDEM1: TMenuItem;
    Networkends1: TMenuItem;
    CreateDEM1: TMenuItem;
    Searchandreplace1: TMenuItem;
    Plotcoveragecircles1: TMenuItem;
    AddLatlong1: TMenuItem;
    Normalizedbasinprofile1: TMenuItem;
    ReplaceDialog1: TReplaceDialog;
    Bargraph1: TMenuItem;
    Addintegercodefield1: TMenuItem;
    N2DgraphCOLORfield1: TMenuItem;
    Oceanography2: TMenuItem;
    Driftmodel1: TMenuItem;
    Extractfiename1: TMenuItem;
    Dividetwofields1: TMenuItem;
    Adddecimaldegreesstring1: TMenuItem;
    Adddecimalminutesstring1: TMenuItem;
    Adddecimalsecondsstring1: TMenuItem;
    HTMLtableperrecord1: TMenuItem;
    Movie1: TMenuItem;
    Zvalues1: TMenuItem;
    Colorallrecords2: TMenuItem;
    Allnormalizedprofiles1: TMenuItem;
    Zstatistics1: TMenuItem;
    Cyclethroughterrainprofiles1: TMenuItem;
    Recordpoints1: TMenuItem;
    Recordboundingbox1: TMenuItem;
    Mergedatabases1: TMenuItem;
    Calculateperimeter1: TMenuItem;
    Multipartrecords1: TMenuItem;
    Pointinrecord1: TMenuItem;
    DayofweekfromYRMonDay1: TMenuItem;
    Dayssincefullmoon1: TMenuItem;
    Markholes1: TMenuItem;
    Recordswithholes1: TMenuItem;
    Arearecordstatistics1: TMenuItem;
    Find1: TMenuItem;
    FindDialog1: TFindDialog;
    Sinuosity1: TMenuItem;
    Gridfont1: TMenuItem;
    Terrainprofiles1: TMenuItem;
    Allprofiles1: TMenuItem;
    Eachprofileseparately1: TMenuItem;
    Elevationchangeofeachrecord1: TMenuItem;
    AddXYZfromshpfile1: TMenuItem;
    CreateXYZpointshapefile1: TMenuItem;
    Labeleverynthrecord1: TMenuItem;
    Treatasregulardatabase1: TMenuItem;
    Meanfilterfilter1: TMenuItem;
    N2Dgraphallopendatabases1: TMenuItem;
    AddrecordIDfield1: TMenuItem;
    Inserticonsfromfilterfile1: TMenuItem;
    TimeFieldAddSnakeyes: TMenuItem;
    Shapefilemetadata1: TMenuItem;
    Quickfilters1: TMenuItem;
    Pointinarea1: TMenuItem;
    Calculatecentroid1: TMenuItem;
    Downhilluphillsegments1: TMenuItem;
    Exportlinetopointdatabase1: TMenuItem;
    AddelevationfromDEMseries1: TMenuItem;
    Movie2: TMenuItem;
    N2dgraphs1: TMenuItem;
    Distancetootherrecords1: TMenuItem;
    Fillindrainagebasin1: TMenuItem;
    InsertJuliandate1: TMenuItem;
    TimefieldstodecJuliandays1: TMenuItem;
    Columnoperations1: TMenuItem;
    N22: TMenuItem;
    Legendfont1: TMenuItem;
    Tidepredictions1: TMenuItem;
    Splitdatefield2: TMenuItem;
    SplittimefieldHRMN1: TMenuItem;
    Timefieldstodecyears1: TMenuItem;
    Stickstadpoleplot1: TMenuItem;
    Sinoffield1: TMenuItem;
    Cosoffield1: TMenuItem;
    Multiplytwofields1: TMenuItem;
    N23: TMenuItem;
    N24: TMenuItem;
    Fixcompassangles1: TMenuItem;
    ChangeTIGERsymbology1: TMenuItem;
    Editgazetteersymbology1: TMenuItem;
    Panel7: TPanel;
    RadioGroup2: TRadioGroup;
    Gridtitlefont1: TMenuItem;
    Ridgecountaroundpoint1: TMenuItem;
    Changefieldtype1: TMenuItem;
    Geocodeaddresses1: TMenuItem;
    Maskfieldbystring1: TMenuItem;
    Gridfonts1: TMenuItem;
    Both1: TMenuItem;
    Addlatlongfromshpfile1: TMenuItem;
    DeliberateKMLGoogleEarthexport1: TMenuItem;
    LOStopoprofile1: TMenuItem;
    ProfileallDEMs1: TMenuItem;
    Insertnewfields1: TMenuItem;
    NewdbfwithspeedpathbyID1: TMenuItem;
    BitBtn28: TBitBtn;
    Removeleadingzeros1: TMenuItem;
    Monthlyanalysis1: TMenuItem;
    Rosediagram2: TMenuItem;
    Linearinterpolateacrossgaps1: TMenuItem;
    Koppenlatitudestats1: TMenuItem;
    Colorbasedonjoinedtable1: TMenuItem;
    ConvertDDMMSSHstringtoLatLong1: TMenuItem;
    LoadMSTfiles1: TMenuItem;
    ColorfromRGBintegerfields1: TMenuItem;
    AddnearestelevationfromDEM1: TMenuItem;
    SQLliteDB1: TMenuItem;
    Stringmanipulation1: TMenuItem;
    Removelowercasecharacters1: TMenuItem;
    AddUTMfromoffsets1: TMenuItem;
    AllDBsmultiplegraphs1: TMenuItem;
    MeanstdallDBs1: TMenuItem;
    Trimblanks1: TMenuItem;
    Multiplegraphmatrix1: TMenuItem;
    LOSterrainprofile1: TMenuItem;
    CDSformat1: TMenuItem;
    Mask2: TMenuItem;
    Trimblanksallstringfields1: TMenuItem;
    Flag1: TMenuItem;
    Addfromsubstring1: TMenuItem;
    ColorfromKoppencategory1: TMenuItem;
    Koppenicons1: TMenuItem;
    Removenumericalcharacters1: TMenuItem;
    Addfileextension1: TMenuItem;
    RemovefinalCharacter: TMenuItem;
    Insertimagefield1: TMenuItem;
    InsertWWWfield1: TMenuItem;
    Tincontour1: TMenuItem;
    N25: TMenuItem;
    Clearalldatabasesettings1: TMenuItem;
    Removemissingdata1: TMenuItem;
    Addboundingbox1: TMenuItem;
    MaskDEMgrid1: TMenuItem;
    Restrictbymapscale1: TMenuItem;
    Label9: TMenuItem;
    Directionofeachrecord1: TMenuItem;
    Editrecord1: TMenuItem;
    Calculate1: TMenuItem;
    Map1: TMenuItem;
    Show1: TMenuItem;
    Views1: TMenuItem;
    Load2: TMenuItem;
    Datastatistics1: TMenuItem;
    Listnonnumericvalues1: TMenuItem;
    Removenonnumericentries1: TMenuItem;
    Datumshift1: TMenuItem;
    Earthquakefocalmechanisms1: TMenuItem;
    QuickKMLexport1: TMenuItem;
    KMLexportdeliberate1: TMenuItem;
    Earthquakefocalmechanisms2: TMenuItem;
    Polestofocalplanes1: TMenuItem;
    Focalplanesonstereonet1: TMenuItem;
    Dipdirectionsforfocalplanes1: TMenuItem;
    Plotforsubsamples1: TMenuItem;
    N3DshapefileprofileLongitude1: TMenuItem;
    N3dshapefileprofileLatitude1: TMenuItem;
    N2Dgraphsimplelines1: TMenuItem;
    RecordcoordinatesCSV1: TMenuItem;
    Pointreflectancespectra1: TMenuItem;
    SupervisedClassification1: TMenuItem;
    Pointreflectancespectranormalized1: TMenuItem;
    N26: TMenuItem;
    InsertCLASSfield1: TMenuItem;
    Compareclassifications1: TMenuItem;
    Pickpointsfortimesequentialseries1: TMenuItem;
    Gradient1: TMenuItem;
    Export1: TMenuItem;
    RecordtoKML1: TMenuItem;
    Legned1: TMenuItem;
    Geocodelatlong1: TMenuItem;
    Averagestandarddeviation1: TMenuItem;
    N2Dgraphallopendatabaseslines1: TMenuItem;
    Gridstatistics1: TMenuItem;
    Graphwithranges1: TMenuItem;
    Reflectancespectrasingleclass1: TMenuItem;
    N27: TMenuItem;
    Openallclassfiles1: TMenuItem;
    Wavelengthmeanallclasses1: TMenuItem;
    WavelengthStdDevallclasses1: TMenuItem;
    ClassBanddiscrimination1: TMenuItem;
    Gridcumulativedistributions1: TMenuItem;
    Creategriddistancetoclasscentroid1: TMenuItem;
    N28: TMenuItem;
    Candidatestoremove1: TMenuItem;
    Pickgridandaddnearestvalue1: TMenuItem;
    Rosediagram3: TMenuItem;
    Gridmaps1: TMenuItem;
    Creategridnumberofbandswithclassbox1: TMenuItem;
    Distancetonearestneighbor1: TMenuItem;
    Creategrid3: TMenuItem;
    Areaofeachrecordm1: TMenuItem;
    KMLexportfilteredbyvalues1: TMenuItem;
    Extractclassfiles1: TMenuItem;
    Computeintervisibilityfrompoint1: TMenuItem;
    Distanceazimuthfromxycomponents1: TMenuItem;
    Monthlywinds1: TMenuItem;
    Featuregeomorphometry1: TMenuItem;
    Integer1: TMenuItem;
    PutpointsonDEMgridlocations1: TMenuItem;
    Trainingclassavailable1: TMenuItem;
    Parametercumulativedistribution1: TMenuItem;
    Cluster2: TMenuItem;
    Clusterfrequency1: TMenuItem;
    Colorarray1: TMenuItem;
    Creategrid4: TMenuItem;
    Satelliteaddreflectance1: TMenuItem;
    AddfieldfromopenDB1: TMenuItem;
    ZoomtoDBcoverage1: TMenuItem;
    HTML2: TMenuItem;
    Navigation1: TMenuItem;
    Firstandlastpoints1: TMenuItem;
    FindAddress1: TMenuItem;
    Otherdatabaseformats1: TMenuItem;
    Fonts1: TMenuItem;
    Renamefieldsfromreferencetable1: TMenuItem;
    InsertNAMEfield1: TMenuItem;
    PlacevaluesinDEM1: TMenuItem;
    InsertDATELABEL1: TMenuItem;
    Predefiendfields1: TMenuItem;
    Formatdatefield1: TMenuItem;
    MMDDYYYY1: TMenuItem;
    MDYYYY1: TMenuItem;
    YYYYMMDD1: TMenuItem;
    Structuralgeology1: TMenuItem;
    hreepointproblem1: TMenuItem;
    Removeifsubstringpresent1: TMenuItem;
    Multiplelinearregression1: TMenuItem;
    Extractfilename1: TMenuItem;
    FixExceldates1: TMenuItem;
    Translatefromtable1: TMenuItem;
    Vectoraverageinbox1: TMenuItem;
    Editfilename1: TMenuItem;
    PopupMenu3: TPopupMenu;
    Pointdensitymatchmaparea1: TMenuItem;
    ValuesfromDB1: TMenuItem;
    RadiusfromDB1: TMenuItem;
    Pointsinbox1: TMenuItem;
    Specifycode1: TMenuItem;
    N2Dgraph2yaxes1: TMenuItem;
    Featurestatistics1: TMenuItem;
    Recordnumber1: TMenuItem;
    BitBtn3: TBitBtn;
    N29: TMenuItem;
    N30: TMenuItem;
    Rosediagramstrikes1: TMenuItem;
    Rosediagramdipdirections1: TMenuItem;
    N31: TMenuItem;
    Insertdipdirectionsdipandstrike1: TMenuItem;
    N32: TMenuItem;
    AllDBsaddinterpolatedelevation1: TMenuItem;
    AddDBsfieldrange1: TMenuItem;
    Terrainfabric1: TMenuItem;
    Removemissingdata2: TMenuItem;
    Allfields3: TMenuItem;
    ByLatitude2: TMenuItem;
    byLongitude2: TMenuItem;
    Addspeedfromxyoruvcomponents1: TMenuItem;
    Cumulative1: TMenuItem;
    SplitdatefieldYYYYMMDD1: TMenuItem;
    Horizonblocking1: TMenuItem;
    Hideunusedfields1: TMenuItem;
    Rosediagram4: TMenuItem;
    Rosediagram01801: TMenuItem;
    Clusterlegend1: TMenuItem;
    Createpointsinpolygons1: TMenuItem;
    Createpointsalonglines1: TMenuItem;
    Createpointsalonglinesbyseparation1: TMenuItem;
    Createpointsfile1: TMenuItem;
    N33: TMenuItem;
    Randomizeduplicatepoints1: TMenuItem;
    InsertNPTSfield1: TMenuItem;
    Updaterecordnumbers1: TMenuItem;
    Limitdecimalplaces1: TMenuItem;
    Zipshapefile1: TMenuItem;
    N35: TMenuItem;
    AllDBsbynumericfield1: TMenuItem;
    rendsurface1: TMenuItem;
    Deadreckoning1: TMenuItem;
    Deadreckoningbackward1: TMenuItem;
    Assignuniquecolors1: TMenuItem;
    Addcountfromsubstring1: TMenuItem;
    Removesubstringandfollowingcharacters1: TMenuItem;
    ranslatefromtable1: TMenuItem;
    Flipbinnames1: TMenuItem;
    Removeinitialcharacter1: TMenuItem;
    XYZ1: TMenuItem;
    AddXYbox1: TMenuItem;
    Dividethreefields1: TMenuItem;
    Adjustzvaluesbasedonx1: TMenuItem;
    Distanceoffmap1: TMenuItem;
    StereoNetPoles1: TMenuItem;
    Stereonetdipdirections1: TMenuItem;
    Stereonetgreatcircles1: TMenuItem;
    Copydatafiles2: TMenuItem;
    raceroute1: TMenuItem;
    Quickfiltering2: TMenuItem;
    Graphicallydelete1: TMenuItem;
    Singlepoints1: TMenuItem;
    Singlepoints2: TMenuItem;
    woorthreefieldRGB1: TMenuItem;
    Positivenegative1: TMenuItem;
    Clearfieldranges1: TMenuItem;
    DeliberateCSVtxt1: TMenuItem;
    ColorfieldinDB1: TMenuItem;
    Iconfield1: TMenuItem;
    PointsymbolsinDB1: TMenuItem;
    Singleicon1: TMenuItem;
    Defaultssymbols1: TMenuItem;
    ColorcodebyDBstringfield1: TMenuItem;
    ShowDBsettings1: TMenuItem;
    Cleargeographicfilter1: TMenuItem;
    ALANDtoALANDKM21: TMenuItem;
    N36: TMenuItem;
    N37: TMenuItem;
    RestorebackupversionofDB1: TMenuItem;
    Averageofneighbors2: TMenuItem;
    MissingData0: TMenuItem;
    BackupDB1: TMenuItem;
    Latlongfields1: TMenuItem;
    Climatestations1: TMenuItem;
    Koppenclassification1: TMenuItem;
    Monthlytemperatures1: TMenuItem;
    Monthlyprecipitation1: TMenuItem;
    Iconsfromthumbnails1: TMenuItem;
    ZipATonefield1: TMenuItem;
    SingleZipatone1: TMenuItem;
    N38: TMenuItem;
    N39: TMenuItem;
    Createnormalilzeddatabase1: TMenuItem;
    Geomorphometryatlas1: TMenuItem;
    Plotwithcolorsfromthisfield1: TMenuItem;
    EditField1: TMenuItem;
    Deletefield1: TMenuItem;
    rimfield1: TMenuItem;
    ColorfieldinjoinedDB1: TMenuItem;
    Colorallrecordsterrainscale1: TMenuItem;
    Colorallrecordsspectrumscale1: TMenuItem;
    Colorallrecordsrainbowscale1: TMenuItem;
    N40: TMenuItem;
    N41: TMenuItem;
    Increasefieldlength1: TMenuItem;
    Lidarwaveform1: TMenuItem;
    Lineshapefile1: TMenuItem;
    Areashapefile1: TMenuItem;
    Lidarwaveforms1: TMenuItem;
    Distancebetweentwopointsinrecord1: TMenuItem;
    CreatepointSHXindex1: TMenuItem;
    LVISslices1: TMenuItem;
    LVIS1: TMenuItem;
    LVISslicescanopy1: TMenuItem;
    Geocode1: TMenuItem;
    Deleteunusedfields1: TMenuItem;
    Deletecurrentrecord1: TMenuItem;
    Addprojectedcoordinates1: TMenuItem;
    Zipshapefile2: TMenuItem;
    Includedebuglog1: TMenuItem;
    rimlengthallstringfields1: TMenuItem;
    Exportshapefilecliptomapextent1: TMenuItem;
    Currentfilter1: TMenuItem;
    Allover1: TMenuItem;
    Allopengridselevationdifference1: TMenuItem;
    Selectfields1: TMenuItem;
    Accumulatedcostsurface1: TMenuItem;
    Exporttextdeliberate1: TMenuItem;
    N42: TMenuItem;
    RedclassifyLoppen1: TMenuItem;
    Noverticalexaggeration1: TMenuItem;
    Verticalexagerration1: TMenuItem;
    Bycategory1: TMenuItem;
    Quickfilters2: TMenuItem;
    N43: TMenuItem;
    Fieldpercentiles2: TMenuItem;
    Reclassifyfocalmechanisms1: TMenuItem;
    Distancealgorithmcomparison1: TMenuItem;
    Multiplefieldstatistics1: TMenuItem;
    Sum3: TMenuItem;
    Mean1: TMenuItem;
    Median1: TMenuItem;
    Minimum1: TMenuItem;
    Maximum1: TMenuItem;
    Geomorphometrystats1: TMenuItem;
    Geomrophometrystaseachpointneighborhood1: TMenuItem;
    AllpointsinboxallopenDBs1: TMenuItem;
    Distancetolinepolygon1: TMenuItem;
    AWATERtoWATERKM21: TMenuItem;
    ClosestPointonLinePolygon1: TMenuItem;
    Heatmap1: TMenuItem;
    Verticaldatumshift1: TMenuItem;
    Sunabovethehorizon1: TMenuItem;
    Countofrecordswithsubstring1: TMenuItem;
    Countrecordswithsubstring1: TMenuItem;
    Addmaskfieldfromsubstringinmultiplefields1: TMenuItem;
    Pointcloudstatistics1: TMenuItem;
    Pointcloudpoints1: TMenuItem;
    Clearfield1: TMenuItem;
    GlobalDEMsandpointclouds1: TMenuItem;
    ICEsat5graphs1: TMenuItem;
    PointcloudstoanalyzeglobalDEMs1: TMenuItem;
    PointcloudandglobalDEMs1: TMenuItem;
    LATprofile1: TMenuItem;
    LONGprofile1: TMenuItem;
    Bothprofiles1: TMenuItem;
    LATprofile2: TMenuItem;
    LONGprofile2: TMenuItem;
    Distributionsummary1: TMenuItem;
    Retainfirstncharacters1: TMenuItem;
    ICESat2Beamsplitter1: TMenuItem;
    DistributionhistogramsthisDB1: TMenuItem;
    DistributionsummarythisDB1: TMenuItem;
    Cloudsummaries1: TMenuItem;
    N34: TMenuItem;
    QuickKMLexport2: TMenuItem;
    DeliberateKMLexport1: TMenuItem;
    Dirtandairshots1: TMenuItem;
    hisDBbylandcovercats1: TMenuItem;
    AddnearestelevationfromDEM2: TMenuItem;
    Integer2: TMenuItem;
    Cloudsummaries2: TMenuItem;
    hisDBbylandcovercategory1: TMenuItem;
    Distributionhistograms1: TMenuItem;
    hisDBslopecats1: TMenuItem;
    ICESat2canopyaddDEMdata1: TMenuItem;
    hisDBcanopyheights1: TMenuItem;
    hisDBbycanopyheight1: TMenuItem;
    hisDBByslope1: TMenuItem;
    hisDBbylandcover1: TMenuItem;
    hisDBall3groups1: TMenuItem;
    hisDBall3groupings1: TMenuItem;
    ICESat2filecleanup1: TMenuItem;
    RewriteinsubdirOGR1: TMenuItem;
    Allpointsinpolygon1: TMenuItem;
    AllpointsinpolygonallopenDBs1: TMenuItem;
    Distancetopointsintable1: TMenuItem;
    Pointslopebyalgorithm1: TMenuItem;
    Fuibn1: TMenuItem;
    AddDEMdata1: TMenuItem;
    Elevationslopeplots1: TMenuItem;
    Landcoversummary1: TMenuItem;
    Create1: TMenuItem;
    CreategeographicPRJfile1: TMenuItem;
    Markanddeleteallrecordspriortothisone1: TMenuItem;
    Selectallrecordsafterthisone1: TMenuItem;
    Colorbarchart1: TMenuItem;
    Percentfield1: TMenuItem;
    GDALsubsettomatchthisrecord1: TMenuItem;
    Removeduplicatepositions1: TMenuItem;
    Filltrackvoids1: TMenuItem;
    DEMIX1: TMenuItem;
    AddEGMfields1: TMenuItem;
    RMSE1: TMenuItem;
    DBGrid1: TDBGrid;
    EGM20081: TMenuItem;
    EGM961: TMenuItem;
    AddEGMfieldsfromalgorithms1: TMenuItem;
    ICESat21: TMenuItem;
    Latprofiles1: TMenuItem;
    Latprofiles2: TMenuItem;
    N44: TMenuItem;
    N45: TMenuItem;
    N46: TMenuItem;
    Lattimecolors1: TMenuItem;
    Boxplot1: TMenuItem;
    BestDEMbycategory1: TMenuItem;
    RankDEMs1: TMenuItem;
    N1degreetilestocoverrecordsintable1: TMenuItem;
    Sumscores1: TMenuItem;
    Graphmeanmedianbyterraincategory1: TMenuItem;
    DEMIXtilesummary1: TMenuItem;
    PickParam1: TMenuItem;
    Filteroutsignedcriteriameanandmedian1: TMenuItem;
    Hide1: TMenuItem;
    Allcriteriavalues1: TMenuItem;
    PercentageofcriteriawhereDEMisbest1: TMenuItem;
    Averageranksbyarea1: TMenuItem;
    COPoALOS1: TMenuItem;
    BestDEMpertilebycriteria1: TMenuItem;
    N7Elevationdifferencecriteria1: TMenuItem;
    FriedmanTest1: TMenuItem;
    Exportsortedtable1: TMenuItem;
    Ascending1: TMenuItem;
    Descending1: TMenuItem;
    N47: TMenuItem;
    Alphabetize1: TMenuItem;
    Createshapefilewithboundingboxforeachrecord1: TMenuItem;
    CreateDBwithcornersandcenterofeveryrecord1: TMenuItem;
    GraphsbestDEMpertilebycriteriasortbytilecharacteristics1: TMenuItem;
    Graphfilters1: TMenuItem;
    FilterforDEMIXtiles1: TMenuItem;
    NormalizeddifferencesfromreferenceDEM1: TMenuItem;
    Bestbysortedgeomorphometry1: TMenuItem;
    Stackedpercentages1: TMenuItem;
    AlphabetizefieldwithCSVsubfields1: TMenuItem;
    iesbyopinions1: TMenuItem;
    Wins1: TMenuItem;
    LoadthisDEM1: TMenuItem;
    Updatestatus1: TMenuItem;
    Averagebylatitude1: TMenuItem;
    Datumshift2: TMenuItem;
    Latlongelevofrecordcorners1: TMenuItem;
    Exporttablewithuniquerecords1: TMenuItem;
    DEMIXPopupMenu1: TPopupMenu;
    N48: TMenuItem;
    DEMIXtileinvertory1: TMenuItem;
    Filterforjustsignedcrirteria1: TMenuItem;
    Meanandmedianhistograms1: TMenuItem;
    AddIMAGEfieldfordifferencedistributiongraphs1: TMenuItem;
    Modestandarddeviationplots1: TMenuItem;
    AddDEMIXtilecentroid1: TMenuItem;
    Clustermaplocations1: TMenuItem;
    Clusterstatistics1: TMenuItem;
    Addaverageelevationinwindow1: TMenuItem;
    MaskallopenDEMgrids1: TMenuItem;
    Currenttest1: TMenuItem;
    Evaluationrangeforcriterion1: TMenuItem;
    //SSIMtodissimilarity1: TMenuItem;
    Addtilecharacteristics1: TMenuItem;
    Sorttable1: TMenuItem;
    Ascending2: TMenuItem;
    Descending2: TMenuItem;
    Clustercomposition1: TMenuItem;
    TransposeSSIMR2forclusters1: TMenuItem;
    ilecharacteristicsbytileforCopDEM1: TMenuItem;
    ClustersperDEMIXtile1: TMenuItem;
    Clusterdiversity1: TMenuItem;
    N49: TMenuItem;
    Clustersensitivity1: TMenuItem;
    Clusterwhiskerplotsforslopeandroughness1: TMenuItem;
    CreateDBwithparametersbyDEM1: TMenuItem;
    DEMIX2: TMenuItem;
    LoadtestandreferenceDEMs1: TMenuItem;
    BitBtn13: TBitBtn;
    N51: TMenuItem;
    N52: TMenuItem;
    Createnewtables1: TMenuItem;
    Modifythistable1: TMenuItem;
    Filterthistable1: TMenuItem;
    Comparerankingswithdifferentcriteria1: TMenuItem;
    DifferentrankingsbyCriteria1: TMenuItem;
    AssignDEMIXDEMcolors1: TMenuItem;
    estDEMlegend1: TMenuItem;
    estDEMlegend2: TMenuItem;
    Vertical1: TMenuItem;
    Graphbyareawithaveragescoreforselectedcriteria1: TMenuItem;
    Graphbytilewithaveragescoreforselectedcriteria1: TMenuItem;
    SingleFieldArithmeticPopupMenu: TPopupMenu;
    Addconstanttofield2: TMenuItem;
    Addconstanttofield3: TMenuItem;
    Dividefieldbyconstant2: TMenuItem;
    Singlefieldarithmetic1: TMenuItem;
    GraphSSIMFUVbyclustermeans1: TMenuItem;
    GraphSSIMFUVbyDEMmeans1: TMenuItem;
    Filterfor999valuesinanyevaluation1: TMenuItem;
    CiompareCOPtorivals1: TMenuItem;
    CopHeadtoheadrecord1: TMenuItem;
    InventoryFUVSSIMcriteriainDB1: TMenuItem;
    Copycolumntoclipboard1: TMenuItem;
    QuartilesinCLUSTERfieldbasedonsort1: TMenuItem;
    N50: TMenuItem;
    N53: TMenuItem;
    Addmultiplefields1: TMenuItem;
    N54: TMenuItem;
    BitBtn24: TBitBtn;
    Alltiles1: TMenuItem;
    Byclusters1: TMenuItem;
    Inventory1: TMenuItem;
    Evaluationrangebycriterion1: TMenuItem;
    Clustermeangraphs1: TMenuItem;
    Areasinclusters1: TMenuItem;
    AddCOPALOSpercentprimarydata1: TMenuItem;
    CopDEMandLandcoverforthistile1: TMenuItem;
    Loadmapsforthisarea1: TMenuItem;
    LoadCopDEMandLandcoverforarea1: TMenuItem;
    Filterfor0valuesinanyevaluation1: TMenuItem;
    CriteriaforeachDEMIXtile1: TMenuItem;
    PercentilesforCOPbycriterionforeachtile1: TMenuItem;
    Sumforallnumericfields1: TMenuItem;
    SortbyBESTEVAL1: TMenuItem;
    AddlatlongfieldstoDB1: TMenuItem;
    N55: TMenuItem;
    MapsbyclusterandDEM1: TMenuItem;
    N56: TMenuItem;
    Removerowsmissinganyevaluations1: TMenuItem;
    N57: TMenuItem;
    Averagetilecharacteristicsbycluster1: TMenuItem;
    Winpercentagebycriterion1: TMenuItem;
    Winpercentagesbyarea1: TMenuItem;
    GraphwinningpercentagebyDEM1: TMenuItem;
    OpenDEMIXgraphs1: TMenuItem;
    N58: TMenuItem;
    Graphofcriteriaforareaortile1: TMenuItem;
    GraphofPrimaryDataFractionbyClusters1: TMenuItem;
    AddcolorsforFULLU120U80U101: TMenuItem;
    SortandreplaceDB1: TMenuItem;
    SortandreplaceDB2: TMenuItem;
    Descending3: TMenuItem;
    Winlosstie1: TMenuItem;
    Filterforevaluations11: TMenuItem;
    LATEXtable1: TMenuItem;
    AddKoppenclass1: TMenuItem;
    Numberoftestareasandtilesbycountry1: TMenuItem;
    //Pointfilter1: TMenuItem;
    //Pointfilter2: TMenuItem;
    procedure N3Dslicer1Click(Sender: TObject);
    procedure Shiftpointrecords1Click(Sender: TObject);
    procedure Creategrid1Click(Sender: TObject);
    procedure Plot1series1Click(Sender: TObject);
    procedure Octree1Click(Sender: TObject);
    procedure DEMTerrainprofile1Click(Sender: TObject);
    procedure BitBtn18Click(Sender: TObject);
    procedure Inserttimeanimationfields1Click(Sender: TObject);
    procedure Allrecordsmatchingsinglefield1Click(Sender: TObject);
    procedure Concatenatestringfields1Click(Sender: TObject);
    procedure Indexnewlines1Click(Sender: TObject);
    procedure Selectlinestodisplay1Click(Sender: TObject);
    procedure Picklineonmap1Click(Sender: TObject);
    procedure Picklinefromfile1Click(Sender: TObject);
    procedure BitBtn17Click(Sender: TObject);
    procedure Asymmetric1Click(Sender: TObject);
    procedure Symmetric1Click(Sender: TObject);
    procedure Earthquakefocalmechanisms3D1Click(Sender: TObject);
    procedure Bylongitude1Click(Sender: TObject);
    procedure Bylatitude1Click(Sender: TObject);
    procedure Twofields1Click(Sender: TObject);
    procedure Selectirregularregion1Click(Sender: TObject);
    procedure Allfields2Click(Sender: TObject);
    procedure Singlefield2Click(Sender: TObject);
    procedure Singlefield1Click(Sender: TObject);
    procedure f1Click(Sender: TObject);
    procedure Fitfouriercurve1Click(Sender: TObject);
    procedure Crosscorrelation1Click(Sender: TObject);
    procedure Autocorrelation1Click(Sender: TObject);
    procedure FFT1Click(Sender: TObject);
    procedure PointSeparation1Click(Sender: TObject);
    procedure Fillfieldforallrecords1Click(Sender: TObject);
    procedure Maskdatabasewithshapefile1Click(Sender: TObject);
    procedure PlotallXYFiles1Click(Sender: TObject);
    procedure PlotXYFile1Click(Sender: TObject);
    procedure dbfStructClick(Sender: TObject);
    procedure DBGrid1DblClick(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BitBtn4Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Button1Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure BitBtn7Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure BitBtn8Click(Sender: TObject);
    procedure Countuniquevalues1Click(Sender: TObject);
    procedure Sumforonefield1Click(Sender: TObject);
    procedure BitBtn9Click(Sender: TObject);
    procedure Fieldcorrelations1Click(Sender: TObject);
    procedure N2Dgraph1Click(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure BitBtn12Click(Sender: TObject);
    procedure Longeststring1Click(Sender: TObject);
    procedure Colorcodebynumericfield1Click(Sender: TObject);
    procedure Statisticsgroupedbyonefield1Click(Sender: TObject);
    procedure Histogram1Click(Sender: TObject);
    procedure Listuniquevalues1Click(Sender: TObject);
    procedure Text1Click(Sender: TObject);
    procedure HTML1Click(Sender: TObject);
    procedure Label1Click(Sender: TObject);
    procedure Selectionregion1Click(Sender: TObject);
    procedure Currentmaparea1Click(Sender: TObject);
    procedure Linelength1Click(Sender: TObject);
    procedure Linelengthsforonefield1Click(Sender: TObject);
    procedure Recentermaponrecord1Click(Sender: TObject);
    procedure Highlightrecordonmap1Click(Sender: TObject);
    procedure Colorpoint1Click(Sender: TObject);
    procedure Editpointlocation1Click(Sender: TObject);
    procedure WWW1Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Colorallrecords1Click(Sender: TObject);
    procedure Coloruncoloredrecords1Click(Sender: TObject);
    procedure Colorbasedonfield1Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Associateimage1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Highlightcolor1Click(Sender: TObject);
    procedure Linelength2Click(Sender: TObject);
    procedure Fanproperties1Click(Sender: TObject);
    procedure Plotfanlocations1Click(Sender: TObject);
    procedure DBFfile1Click(Sender: TObject);
    procedure Vectordata1Click(Sender: TObject);
    procedure N2Dgraphcolorcoded1Click(Sender: TObject);
    procedure Rosediagram1Click(Sender: TObject);
    procedure Calculatearea1Click(Sender: TObject);
    procedure Recordcoordinates1Click(Sender: TObject);
    procedure Editpointsymbol1Click(Sender: TObject);
    procedure Keyboardnewpointlocation1Click(Sender: TObject);
    procedure RecordDisplay1Click(Sender: TObject);
    procedure Recordscreencoordinates1Click(Sender: TObject);
    procedure Dipandstrikes1Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Redistrict1Click(Sender: TObject);
    procedure Recordedit1Click(Sender: TObject);
    procedure Coordinatestotextfile1Click(Sender: TObject);
    procedure N2Dgraphcolorcodetext1Click(Sender: TObject);
    procedure Focalmechanisms1Click(Sender: TObject);
    procedure Earthquakemechanisms1Click(Sender: TObject);
    procedure Ternarydiagram1Click(Sender: TObject);
    procedure Labelselectedrecords1Click(Sender: TObject);
    procedure Graphicallymovepoints1Click(Sender: TObject);
    procedure Graphicalpickandeditdatabase1Click(Sender: TObject);
    procedure Editlineweightandcolor1Click(Sender: TObject);
    procedure Coloralllinesegments1Click(Sender: TObject);
    procedure Coloralluncoloredlinesegments1Click(Sender: TObject);
    procedure Deleteallrecords1Click(Sender: TObject);
    procedure Forceredrawofmap1Click(Sender: TObject);
    procedure Insertrecord1Click(Sender: TObject);
    procedure Editrecordsingrid1Click(Sender: TObject);
    procedure Beachballs1Click(Sender: TObject);
    procedure Editfillpattern1Click(Sender: TObject);
    procedure Showarearecords1Click(Sender: TObject);
    procedure BitBtn11Click(Sender: TObject);
    procedure Font1Click(Sender: TObject);
    procedure CalculateVolume1Click(Sender: TObject);
    procedure Proportionalsquares1Click(Sender: TObject);
    procedure Distancefrompoint1Click(Sender: TObject);
    procedure Stationtimeseries1Click(Sender: TObject);
    procedure N3series1Click(Sender: TObject);
    procedure N2series1Click(Sender: TObject);
    procedure Stratigraphiccolumn1Click(Sender: TObject);
    procedure Animatefield1Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure Addicons1Click(Sender: TObject);
    procedure AddelevationfromDEM1Click(Sender: TObject);
    procedure Addazimuthtotravelpath1Click(Sender: TObject);
    procedure Thindatabase1Click(Sender: TObject);
    procedure Mutliple1Click(Sender: TObject);
    procedure Getazimuthfrommagneticheading1Click(Sender: TObject);
    procedure Addconstanttofield1Click(Sender: TObject);
    procedure Legend1Click(Sender: TObject);
    procedure Changefieldsused1Click(Sender: TObject);
    procedure NearTIGERroads1Click(Sender: TObject);
    procedure AwayfromTIGERroads1Click(Sender: TObject);
    procedure Medianfilterfield1Click(Sender: TObject);
    procedure Sumtwofields1Click(Sender: TObject);
    procedure Differencetwofields1Click(Sender: TObject);
    procedure Quotienttwofields1Click(Sender: TObject);
    procedure Addlatlongfromlinkeddatabase1Click(Sender: TObject);
    procedure AddDISTANCEAZIMUTHfields1Click(Sender: TObject);
    procedure Cluster1Click(Sender: TObject);
    procedure XML1Click(Sender: TObject);
    procedure Insertcolorfield1Click(Sender: TObject);
    procedure Normalizefield1Click(Sender: TObject);
    procedure MaskDEMfromshapefile1Click(Sender: TObject);
    procedure N2Dgraph2series1Click(Sender: TObject);
    procedure PlotClick(Sender: TObject);
    procedure AddslopefromDEM1Click(Sender: TObject);
    procedure Connectsequentialpoints1Click(Sender: TObject);
    procedure rimblanksinstringfield1Click(Sender: TObject);
    procedure MaskClick(Sender: TObject);
    procedure Labelsensors1Click(Sender: TObject);
    procedure Selectradiusaboutpoint1Click(Sender: TObject);
    procedure Maskdatabasewithgazetteer1Click(Sender: TObject);
    procedure MarkallY1Click(Sender: TObject);
    procedure MarkallN1Click(Sender: TObject);
    procedure Changelatlongfields1Click(Sender: TObject);
    procedure Principalcomponents1Click(Sender: TObject);
    procedure KML1Click(Sender: TObject);
    procedure Centr1Click(Sender: TObject);
    procedure Findneighbors1Click(Sender: TObject);
    procedure Showneighbors1Click(Sender: TObject);
    procedure IgnoreMissingData1Click(Sender: TObject);
    procedure PanoramaView1Click(Sender: TObject);
    procedure Perspectiveview1Click(Sender: TObject);
    procedure Setjoin1Click(Sender: TObject);
    procedure Showjoin1Click(Sender: TObject);
    procedure Clearjoin1Click(Sender: TObject);
    procedure InsertMASKfield1Click(Sender: TObject);
    procedure Insertfield1Click(Sender: TObject);
    procedure Grpahicallyinsertpoint1Click(Sender: TObject);
    procedure Sumofneighbors1Click(Sender: TObject);
    procedure Requiredantennaheight1Click(Sender: TObject);
    procedure Fieldstatistics1Click(Sender: TObject);
    procedure Sum1Click(Sender: TObject);
    procedure Createpointshapefile1Click(Sender: TObject);
    procedure Historgram1Click(Sender: TObject);
    procedure Listuniquevalues2Click(Sender: TObject);
    procedure Longeststring2Click(Sender: TObject);
    procedure Logoffield1Click(Sender: TObject);
    procedure DataDBFonlynogeometry1Click(Sender: TObject);
    procedure Layersymbology1Click(Sender: TObject);
    procedure ArcGISviewshedsensors1Click(Sender: TObject);
    procedure BitBtn6Click(Sender: TObject);
    procedure BitBtn10Click(Sender: TObject);
    procedure BitBtn14Click(Sender: TObject);
    procedure PanoramaSpeedButtonClick(Sender: TObject);
    procedure BitBtn15Click(Sender: TObject);
    procedure LOSButtonClick(Sender: TObject);
    procedure PerspectiveButtonClick(Sender: TObject);
    procedure BitBtn16Click(Sender: TObject);
    procedure Recolorfan1Click(Sender: TObject);
    procedure Recolorallfans1Click(Sender: TObject);
    procedure Legend2Click(Sender: TObject);
    procedure Editfont1Click(Sender: TObject);
    procedure ClearDBsettings1Click(Sender: TObject);
    procedure Createuniticons1Click(Sender: TObject);
    procedure FillField1Click(Sender: TObject);
    procedure BitBtn19Click(Sender: TObject);
    procedure Thindatabase2Click(Sender: TObject);
    procedure ExportXYZtriples1Click(Sender: TObject);
    procedure ComboBox4Change(Sender: TObject);
    procedure ComboBox5Change(Sender: TObject);
    procedure ComboBox6Change(Sender: TObject);
    procedure ComboBox7Change(Sender: TObject);
    procedure CompareshapefileandDEMprofiles1Click(Sender: TObject);
    procedure AddUTMcoordfields1Click(Sender: TObject);
    procedure Centroidofeachrecord1Click(Sender: TObject);
    procedure Lengthofeachrecord1Click(Sender: TObject);
    procedure EvaluateXYProfiles1Click(Sender: TObject);
    procedure Frequencytable1Click(Sender: TObject);
    procedure CopyCoordinatesClick(Sender: TObject);
    procedure InsertNewRecClipboardClick(Sender: TObject);
    procedure Copyfieldtostringfield1Click(Sender: TObject);
    procedure Addleadingzeros1Click(Sender: TObject);
    procedure Pastecoordinatesfromclipboard1Click(Sender: TObject);
    procedure Rangecircles1Click(Sender: TObject);
    procedure Insertdatefield1Click(Sender: TObject);
    procedure Findpointsinareas1Click(Sender: TObject);
    procedure ogglerecords1Click(Sender: TObject);
    procedure Insertlinecolorwidthfields1Click(Sender: TObject);
    procedure Insertareacolorsymbology1Click(Sender: TObject);
    procedure Loadfrommaplibrary1Click(Sender: TObject);
    procedure AllDBs1Click(Sender: TObject);
    procedure Allfields1Click(Sender: TObject);
    procedure Exportlatlongz1Click(Sender: TObject);
    procedure Fillpatternalluncoloredrecords1Click(Sender: TObject);
    procedure Fillpatternallrecords1Click(Sender: TObject);
    procedure Sortfield1Click(Sender: TObject);
    procedure Splitdatefield1Click(Sender: TObject);
    procedure Addfontdefinition1Click(Sender: TObject);
    procedure Insertpointsymbol1Click(Sender: TObject);
    procedure Fillfontfield1Click(Sender: TObject);
    procedure Fieldpercentiles1Click(Sender: TObject);
    procedure EditIcon1Click(Sender: TObject);
    procedure AddXYZfromjoinedtable1Click(Sender: TObject);
    procedure ColorfromRGBfloatfields1Click(Sender: TObject);
    procedure Resetjoin1Click(Sender: TObject);
    procedure Keepbefore1Click(Sender: TObject);
    procedure Keepafte1Click(Sender: TObject);
    procedure BitBtn20Click(Sender: TObject);
    procedure Mergedatabase1Click(Sender: TObject);
    procedure Dividefieldbyconstant1Click(Sender: TObject);
    procedure Copynumericfield1Click(Sender: TObject);
    procedure AddMercator1Click(Sender: TObject);
    procedure AddMGRS1Click(Sender: TObject);
    procedure Fix360longs1Click(Sender: TObject);
    procedure ConvertUTMtolatlong1Click(Sender: TObject);
    procedure BitBtn21Click(Sender: TObject);
    procedure TimefieldHHMMSStohours1Click(Sender: TObject);
    procedure AddelevationdifferencefromDEM1Click(Sender: TObject);
    procedure Findneighborsinseconddatabase1Click(Sender: TObject);
    procedure Insertnewrecordatdistancebearing1Click(Sender: TObject);
    procedure DEMTerrainblowups1Click(Sender: TObject);
    procedure Viewshedfields1Click(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure DechourstoHHMMSS1Click(Sender: TObject);
    procedure Timefieldshourminsectodechours1Click(Sender: TObject);
    procedure Timefieldstodecdays1Click(Sender: TObject);
    procedure SplittimestringHHMMSS1Click(Sender: TObject);
    procedure Renamefield1Click(Sender: TObject);
    procedure BitBtn22Click(Sender: TObject);
    procedure FeaturesClick(Sender: TObject);
    procedure Sigmatee1Click(Sender: TObject);
    procedure Soundvelocity1Click(Sender: TObject);
    procedure Sigmatheta1Click(Sender: TObject);
    procedure Fillviewshedfields1Click(Sender: TObject);
    procedure CurrentflterYrestN1Click(Sender: TObject);
    procedure Highlightfan1Click(Sender: TObject);
    procedure N22Click(Sender: TObject);
    procedure Plotfanlocations2Click(Sender: TObject);
    procedure Labelfans1Click(Sender: TObject);
    procedure Plotfans2Click(Sender: TObject);
    procedure InsertYEARMONTHDATE1Click(Sender: TObject);
    procedure MilitarytimetoHHMMSS1Click(Sender: TObject);
    procedure Enablealloptions1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure DBGrid1DrawColumnCell(Sender: TObject; const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure Quickfilter1Click(Sender: TObject);
    procedure Photolocations1Click(Sender: TObject);
    procedure Outlinecameraview1Click(Sender: TObject);
    procedure BitBtn23Click(Sender: TObject);
    procedure DBGrid1CellClick(Column: TColumn);
    procedure Copyshapefile1Click(Sender: TObject);
    procedure Zoommaptorecord1Click(Sender: TObject);
    procedure Perimeterofeachrecord1Click(Sender: TObject);
    procedure FindrecordsonDEM1Click(Sender: TObject);
    procedure CreateDEM1Click(Sender: TObject);
    procedure Searchandreplace1Click(Sender: TObject);
    procedure Plotcoveragecircles1Click(Sender: TObject);
    procedure AddLatlong1Click(Sender: TObject);
    procedure Normalizedbasinprofile1Click(Sender: TObject);
    procedure ReplaceDialog1Replace(Sender: TObject);
    procedure ReplaceDialog1Find(Sender: TObject);
    procedure Addintegercodefield1Click(Sender: TObject);
    procedure N2DgraphCOLORfield1Click(Sender: TObject);
    procedure Driftmodel1Click(Sender: TObject);
    procedure Extractfiename1Click(Sender: TObject);
    procedure Dividetwofields1Click(Sender: TObject);
    procedure Adddecimalsecondsstring1Click(Sender: TObject);
    procedure Adddecimalminutesstring1Click(Sender: TObject);
    procedure Adddecimaldegreesstring1Click(Sender: TObject);
    procedure HTMLtableperrecord1Click(Sender: TObject);
    procedure Movie1Click(Sender: TObject);
    procedure Zvalues1Click(Sender: TObject);
    procedure Colorallrecords2Click(Sender: TObject);
    procedure Allnormalizedprofiles1Click(Sender: TObject);
    procedure Zstatistics1Click(Sender: TObject);
    procedure Cyclethroughterrainprofiles1Click(Sender: TObject);
    procedure Recordpoints1Click(Sender: TObject);
    procedure Recordboundingbox1Click(Sender: TObject);
    procedure Calculateperimeter1Click(Sender: TObject);
    procedure Multipartrecords1Click(Sender: TObject);
    procedure Pointinrecord1Click(Sender: TObject);
    procedure DayofweekfromYRMonDay1Click(Sender: TObject);
    procedure Dayssincefullmoon1Click(Sender: TObject);
    procedure Markholes1Click(Sender: TObject);
    procedure Recordswithholes1Click(Sender: TObject);
    procedure Find1Click(Sender: TObject);
    procedure FindDialog1Find(Sender: TObject);
    procedure Sinuosity1Click(Sender: TObject);
    procedure Gridfont1Click(Sender: TObject);
    procedure Allprofiles1Click(Sender: TObject);
    procedure Eachprofileseparately1Click(Sender: TObject);
    procedure Elevationchangeofeachrecord1Click(Sender: TObject);
    procedure AddXYZfromshpfile1Click(Sender: TObject);
    procedure CreateXYZpointshapefile1Click(Sender: TObject);
    procedure Labeleverynthrecord1Click(Sender: TObject);
    procedure Treatasregulardatabase1Click(Sender: TObject);
    procedure Meanfilterfilter1Click(Sender: TObject);
    procedure N2Dgraphallopendatabases1Click(Sender: TObject);
    procedure AddrecordIDfield1Click(Sender: TObject);
    procedure Inserticonsfromfilterfile1Click(Sender: TObject);
    procedure TimeFieldAddSnakeyesClick(Sender: TObject);
    procedure Shapefilemetadata1Click(Sender: TObject);
    procedure Quickfilters1Click(Sender: TObject);
    procedure Pointinarea1Click(Sender: TObject);
    procedure Calculatecentroid1Click(Sender: TObject);
    procedure Downhilluphillsegments1Click(Sender: TObject);
    procedure Exportlinetopointdatabase1Click(Sender: TObject);
    procedure AddelevationfromDEMseries1Click(Sender: TObject);
    procedure Movie2Click(Sender: TObject);
    procedure Distancetootherrecords1Click(Sender: TObject);
    procedure InsertJuliandate1Click(Sender: TObject);
    procedure TimefieldstodecJuliandays1Click(Sender: TObject);
    procedure DBGrid1TitleClick(Column: TColumn);
    procedure Columnoperations1Click(Sender: TObject);
    procedure Legendfont1Click(Sender: TObject);
    procedure Tidepredictions1Click(Sender: TObject);
    procedure Splitdatefield2Click(Sender: TObject);
    procedure SplittimefieldHRMN1Click(Sender: TObject);
    procedure Timefieldstodecyears1Click(Sender: TObject);
    procedure Stickstadpoleplot1Click(Sender: TObject);
    procedure Sinoffield1Click(Sender: TObject);
    procedure Cosoffield1Click(Sender: TObject);
    procedure Multiplytwofields1Click(Sender: TObject);
    procedure Fixcompassangles1Click(Sender: TObject);
    procedure ChangeTIGERsymbology1Click(Sender: TObject);
    procedure Editgazetteersymbology1Click(Sender: TObject);
    procedure Gridtitlefont1Click(Sender: TObject);
    procedure Addbeachballicons1Click(Sender: TObject);
    procedure Ridgecountaroundpoint1Click(Sender: TObject);
    procedure Changefieldtype1Click(Sender: TObject);
    procedure Geocodeaddresses1Click(Sender: TObject);
    procedure Maskfieldbystring1Click(Sender: TObject);
    procedure Both1Click(Sender: TObject);
    procedure Addlatlongfromshpfile1Click(Sender: TObject);
    procedure DeliberateKMLGoogleEarthexport1Click(Sender: TObject);
    procedure LOStopoprofile1Click(Sender: TObject);
    procedure ProfileallDEMs1Click(Sender: TObject);
    procedure NewdbfwithspeedpathbyID1Click(Sender: TObject);
    procedure BitBtn28Click(Sender: TObject);
    procedure Removeleadingzeros1Click(Sender: TObject);
    procedure Rosediagram2Click(Sender: TObject);
    procedure Linearinterpolateacrossgaps1Click(Sender: TObject);
    procedure Koppenlatitudestats1Click(Sender: TObject);
    procedure Colorbasedonjoinedtable1Click(Sender: TObject);
    procedure LoadMSTfiles1Click(Sender: TObject);
    procedure ColorfromRGBintegerfields1Click(Sender: TObject);
    procedure AddnearestelevationfromDEM1Click(Sender: TObject);
    procedure SQLliteDB1Click(Sender: TObject);
    procedure Removelowercasecharacters1Click(Sender: TObject);
    procedure AddUTMfromoffsets1Click(Sender: TObject);
    procedure AllDBsmultiplegraphs1Click(Sender: TObject);
    procedure MeanstdallDBs1Click(Sender: TObject);
    procedure Trimblanks1Click(Sender: TObject);
    procedure Multiplegraphmatrix1Click(Sender: TObject);
    procedure LOSterrainprofile1Click(Sender: TObject);
    procedure CDSformat1Click(Sender: TObject);
    procedure Trimblanksallstringfields1Click(Sender: TObject);
    procedure Addfromsubstring1Click(Sender: TObject);
    procedure ColorfromKoppencategory1Click(Sender: TObject);
    procedure Koppenicons1Click(Sender: TObject);
    procedure Removenumericalcharacters1Click(Sender: TObject);
    procedure Addfileextension1Click(Sender: TObject);
    procedure RemovefinalCharacterClick(Sender: TObject);
    procedure Insertimagefield1Click(Sender: TObject);
    procedure InsertWWWfield1Click(Sender: TObject);
    procedure Tincontour1Click(Sender: TObject);
    procedure Clearalldatabasesettings1Click(Sender: TObject);
    procedure Removemissingdata1Click(Sender: TObject);
    procedure Addboundingbox1Click(Sender: TObject);
    procedure MaskDEMgrid1Click(Sender: TObject);
    procedure Restrictbymapscale1Click(Sender: TObject);
    procedure Directionofeachrecord1Click(Sender: TObject);
    procedure Listnonnumericvalues1Click(Sender: TObject);
    procedure Removenonnumericentries1Click(Sender: TObject);
    procedure Datumshift1Click(Sender: TObject);
    procedure QuickKMLexport1Click(Sender: TObject);
    procedure Polestofocalplanes1Click(Sender: TObject);
    procedure Focalplanesonstereonet1Click(Sender: TObject);
    procedure Dipdirectionsforfocalplanes1Click(Sender: TObject);
    procedure Plotforsubsamples1Click(Sender: TObject);
    procedure N3DshapefileprofileLongitude1Click(Sender: TObject);
    procedure N3dshapefileprofileLatitude1Click(Sender: TObject);
    procedure N3Dshapefileprofile1Click(Sender: TObject);
    procedure N2Dgraphsimplelines1Click(Sender: TObject);
    procedure RecordcoordinatesCSV1Click(Sender: TObject);
    procedure Pointreflectancespectra1Click(Sender: TObject);
    procedure Pointreflectancespectranormalized1Click(Sender: TObject);
    procedure AverageStandarddeviation1Click(Sender: TObject);
    procedure InsertCLASSfield1Click(Sender: TObject);
    procedure Compareclassifications1Click(Sender: TObject);
    procedure Pickpointsfortimesequentialseries1Click(Sender: TObject);
    procedure Gradient1Click(Sender: TObject);
    procedure RecordtoKML1Click(Sender: TObject);
    procedure Legned1Click(Sender: TObject);
    procedure Geocodelatlong1Click(Sender: TObject);
    procedure N2Dgraphallopendatabaseslines1Click(Sender: TObject);
    procedure Gridstatistics1Click(Sender: TObject);
    procedure Graphwithranges1Click(Sender: TObject);
    procedure Reflectancespectrasingleclass1Click(Sender: TObject);
    procedure Openallclassfiles1Click(Sender: TObject);
    procedure Wavelengthmeanallclasses1Click(Sender: TObject);
    procedure WavelengthStdDevallclasses1Click(Sender: TObject);
    procedure ClassBanddiscrimination1Click(Sender: TObject);
    procedure Gridcumulativedistributions1Click(Sender: TObject);
    procedure Creategriddistancetoclasscentroid1Click(Sender: TObject);
    procedure Pickgridandaddnearestvalue1Click(Sender: TObject);
    procedure Rosediagram3Click(Sender: TObject);
    procedure Gridmaps1Click(Sender: TObject);
    procedure Creategridnumberofbandswithclassbox1Click(Sender: TObject);
    procedure Distancetonearestneighbor1Click(Sender: TObject);
    procedure Creategridradius1Click(Sender: TObject);
    procedure Creategridbox1Click(Sender: TObject);
    procedure Creategridspecifycode1Click(Sender: TObject);
    procedure Areaofeachrecordm1Click(Sender: TObject);
    procedure KMLexportfilteredbyvalues1Click(Sender: TObject);
    procedure Extractclassfiles1Click(Sender: TObject);
    procedure Computeintervisibilityfrompoint1Click(Sender: TObject);
    procedure Distanceazimuthfromxycomponents1Click(Sender: TObject);
    procedure Monthlywinds1Click(Sender: TObject);
    procedure Integer1Click(Sender: TObject);
    procedure PutpointsonDEMgridlocations1Click(Sender: TObject);
    procedure Trainingclassavailable1Click(Sender: TObject);
    procedure Parametercumulativedistribution1Click(Sender: TObject);
    procedure Clusterfrequency1Click(Sender: TObject);
    procedure Colorarray1Click(Sender: TObject);
    procedure Satelliteaddreflectance1Click(Sender: TObject);
    procedure AddfieldfromopenDB1Click(Sender: TObject);
    procedure ZoomtoDBcoverage1Click(Sender: TObject);
    procedure Firstandlastpoints1Click(Sender: TObject);
    procedure Areaofeachrecord1Click(Sender: TObject);
    procedure FindAddress1Click(Sender: TObject);
    procedure Renamefieldsfromreferencetable1Click(Sender: TObject);
    procedure InsertNAMEfield1Click(Sender: TObject);
    procedure PlacevaluesinDEM1Click(Sender: TObject);
    procedure InsertDATELABEL1Click(Sender: TObject);
    procedure MMDDYYYY1Click(Sender: TObject);
    procedure MDYYYY1Click(Sender: TObject);
    procedure YYYYMMDD1Click(Sender: TObject);
    procedure hreepointproblem1Click(Sender: TObject);
    procedure Removeifsubstringpresent1Click(Sender: TObject);
    procedure Multiplelinearregression1Click(Sender: TObject);
    procedure Extractfilename1Click(Sender: TObject);
    procedure FixExceldates1Click(Sender: TObject);
    procedure Translatefromtable1Click(Sender: TObject);
    procedure Vectoraverageinbox1Click(Sender: TObject);
    procedure Editfilename1Click(Sender: TObject);
    procedure ConvertDDMMSSHstringtoLatLong1Click(Sender: TObject);
    procedure Creategrid2Click(Sender: TObject);
    procedure Creategrid3Click(Sender: TObject);
    procedure Creategrid4Click(Sender: TObject);
    procedure Pointdensitymatchmaparea1Click(Sender: TObject);
    procedure Pointsinbox1Click(Sender: TObject);
    procedure RadiusfromDB1Click(Sender: TObject);
    procedure ValuesfromDB1Click(Sender: TObject);
    procedure N2Dgraph2yaxes1Click(Sender: TObject);
    procedure Featurestatistics1Click(Sender: TObject);
    procedure Beachballscolorandsize1Click(Sender: TObject);
    procedure Recordnumber1Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure Rosediagramstrikes1Click(Sender: TObject);
    procedure Rosediagramdipdirections1Click(Sender: TObject);
    procedure Insertdipdirectionsdipandstrike1Click(Sender: TObject);
    procedure AllDBsaddinterpolatedelevation1Click(Sender: TObject);
    procedure AddDBsfieldrange1Click(Sender: TObject);
    procedure Terrainfabric1Click(Sender: TObject);
    procedure Allfields3Click(Sender: TObject);
    procedure ByLatitude2Click(Sender: TObject);
    procedure byLongitude2Click(Sender: TObject);
    procedure Addspeedfromxyoruvcomponents1Click(Sender: TObject);
    procedure Cumulative1Click(Sender: TObject);
    procedure SplitdatefieldYYYYMMDD1Click(Sender: TObject);
    procedure Horizonblocking1Click(Sender: TObject);
    procedure Hideunusedfields1Click(Sender: TObject);
    procedure Rosediagram01801Click(Sender: TObject);
    procedure Clusterlegend1Click(Sender: TObject);
    procedure Createpointsinpolygons1Click(Sender: TObject);
    procedure Createpointsalonglines1Click(Sender: TObject);
    procedure Createpointsalonglinesbyseparation1Click(Sender: TObject);
    procedure Randomizeduplicatepoints1Click(Sender: TObject);
    procedure InsertNPTSfield1Click(Sender: TObject);
    procedure Updaterecordnumbers1Click(Sender: TObject);
    procedure Limitdecimalplaces1Click(Sender: TObject);
    procedure Zipshapefile1Click(Sender: TObject);
    procedure AllDBsbynumericfield1Click(Sender: TObject);
    procedure rendsurface1Click(Sender: TObject);
    procedure Deadreckoning1Click(Sender: TObject);
    procedure Deadreckoningbackward1Click(Sender: TObject);
    procedure Assignuniquecolors1Click(Sender: TObject);
    procedure Addcountfromsubstring1Click(Sender: TObject);
    procedure Removesubstringandfollowingcharacters1Click(Sender: TObject);
    procedure ranslatefromtable1Click(Sender: TObject);
    procedure Flipbinnames1Click(Sender: TObject);
    procedure Removeinitialcharacter1Click(Sender: TObject);
    procedure XYZ1Click(Sender: TObject);
    procedure AddXYbox1Click(Sender: TObject);
    procedure Dividethreefields1Click(Sender: TObject);
    procedure Adjustzvaluesbasedonx1Click(Sender: TObject);
    procedure Distanceoffmap1Click(Sender: TObject);
    procedure StereoNetPoles1Click(Sender: TObject);
    procedure Stereonetdipdirections1Click(Sender: TObject);
    procedure Stereonetgreatcircles1Click(Sender: TObject);
    procedure Copydatafiles2Click(Sender: TObject);
    procedure raceroute1Click(Sender: TObject);
    procedure Singlepoints2Click(Sender: TObject);
    procedure woorthreefieldRGB1Click(Sender: TObject);
    procedure Positivenegative1Click(Sender: TObject);
    procedure Clearfieldranges1Click(Sender: TObject);
    procedure DeliberateCSVtxt1Click(Sender: TObject);
    procedure ColorfieldinDB1Click(Sender: TObject);
    procedure Iconfield1Click(Sender: TObject);
    procedure PointsymbolsinDB1Click(Sender: TObject);
    procedure Singleicon1Click(Sender: TObject);
    procedure Defaultssymbols1Click(Sender: TObject);
    procedure ColorcodebyDBstringfield1Click(Sender: TObject);
    procedure ShowDBsettings1Click(Sender: TObject);
    procedure Cleargeographicfilter1Click(Sender: TObject);
    procedure ALANDtoALANDKM21Click(Sender: TObject);
    procedure AWATERtoAWATERKM21Click(Sender: TObject);
    procedure N36Click(Sender: TObject);
    procedure RestorebackupversionofDB1Click(Sender: TObject);
    procedure MissingData0Click(Sender: TObject);
    procedure Singlepoints1Click(Sender: TObject);
    procedure BackupDB1Click(Sender: TObject);
    procedure Latlongfields1Click(Sender: TObject);
    procedure Koppenclassification1Click(Sender: TObject);
    procedure Monthlytemperatures1Click(Sender: TObject);
    procedure Monthlyprecipitation1Click(Sender: TObject);
    procedure Iconsfromthumbnails1Click(Sender: TObject);
    procedure SingleZipatone1Click(Sender: TObject);
    procedure ZipATonefield1Click(Sender: TObject);
    procedure Createnormalilzeddatabase1Click(Sender: TObject);
    procedure Geomorphometryatlas1Click(Sender: TObject);
    procedure Plotwithcolorsfromthisfield1Click(Sender: TObject);
    procedure Deletefield1Click(Sender: TObject);
    procedure Trimfield1Click(Sender: TObject);
    procedure ColorfieldinjoinedDB1Click(Sender: TObject);
    procedure Colorallrecordsterrainscale1Click(Sender: TObject);
    procedure Colorallrecordsspectrumscale1Click(Sender: TObject);
    procedure Colorallrecordsrainbowscale1Click(Sender: TObject);
    procedure Increasefieldlength1Click(Sender: TObject);
    procedure Lidarwaveform1Click(Sender: TObject);
    procedure Lineshapefile1Click(Sender: TObject);
    procedure Areashapefile1Click(Sender: TObject);
    procedure Lidarwaveforms1Click(Sender: TObject);
    procedure Distancebetweentwopointsinrecord1Click(Sender: TObject);
    procedure CreatepointSHXindex1Click(Sender: TObject);
    procedure LVISslices1Click(Sender: TObject);
    procedure LVISslicescanopy1Click(Sender: TObject);
    procedure Deleteunusedfields1Click(Sender: TObject);
    procedure Deletecurrentrecord1Click(Sender: TObject);
    procedure Mask2Click(Sender: TObject);
    procedure Addprojectedcoordinates1Click(Sender: TObject);
    procedure Includedebuglog1Click(Sender: TObject);
    procedure rimlengthallstringfields1Click(Sender: TObject);
    procedure Exportshapefilecliptomapextent1Click(Sender: TObject);
    procedure Currentfilter1Click(Sender: TObject);
    procedure Allover1Click(Sender: TObject);
    procedure Allopengridselevationdifference1Click(Sender: TObject);
    procedure Selectfields1Click(Sender: TObject);
    procedure Accumulatedcostsurface1Click(Sender: TObject);
    procedure Exporttextdeliberate1Click(Sender: TObject);
    procedure RedclassifyLoppen1Click(Sender: TObject);
    procedure Quickfiltering2Click(Sender: TObject);
    procedure Noverticalexaggeration1Click(Sender: TObject);
    procedure Verticalexagerration1Click(Sender: TObject);
    procedure Bycategory1Click(Sender: TObject);
    procedure Quickfilters2Click(Sender: TObject);
    procedure Fieldpercentiles2Click(Sender: TObject);
    procedure Reclassifyfocalmechanisms1Click(Sender: TObject);
    procedure Distancealgorithmcomparison1Click(Sender: TObject);
    procedure Sum3Click(Sender: TObject);
    procedure Mean1Click(Sender: TObject);
    procedure Median1Click(Sender: TObject);
    procedure Minimum1Click(Sender: TObject);
    procedure Maximum1Click(Sender: TObject);
    procedure Geomorphometrystats1Click(Sender: TObject);
    procedure Meanwidth1Click(Sender: TObject);
    procedure Compactness1Click(Sender: TObject);
    procedure Shapenumber1Click(Sender: TObject);
    procedure Schwartzberg1Click(Sender: TObject);
    procedure Perimetersquaredoverarea1Click(Sender: TObject);
    procedure Geomrophometrystaseachpointneighborhood1Click(Sender: TObject);
    procedure AllpointsinboxallopenDBs1Click(Sender: TObject);
    procedure Distancetolinepolygon1Click(Sender: TObject);
    procedure AWATERtoWATERKM21Click(Sender: TObject);
    procedure ClosestPointonLinePolygon1Click(Sender: TObject);
    procedure Heatmap1Click(Sender: TObject);
    procedure Verticaldatumshift1Click(Sender: TObject);
    procedure Sunabovethehorizon1Click(Sender: TObject);
    procedure Countofrecordswithsubstring1Click(Sender: TObject);
    procedure Countrecordswithsubstring1Click(Sender: TObject);
    procedure Addmaskfieldfromsubstringinmultiplefields1Click(Sender: TObject);
    procedure Pointcloudstatistics1Click(Sender: TObject);
    procedure Pointcloudpoints1Click(Sender: TObject);
    procedure Clearfield1Click(Sender: TObject);
    procedure GlobalDEMsandpointclouds1Click(Sender: TObject);
    procedure LONGprofile1Click(Sender: TObject);
    procedure LATprofile1Click(Sender: TObject);
    procedure Bothprofiles1Click(Sender: TObject);
    procedure LATprofile2Click(Sender: TObject);
    procedure LONGprofile2Click(Sender: TObject);
    procedure Distributionsummary1Click(Sender: TObject);
    procedure Retainfirstncharacters1Click(Sender: TObject);
    procedure ICESat2Beamsplitter1Click(Sender: TObject);
    procedure DistributionhistogramsthisDB1Click(Sender: TObject);
    procedure DistributionsummarythisDB1Click(Sender: TObject);
    procedure Cloudsummaries1Click(Sender: TObject);
    procedure QuickKMLexport2Click(Sender: TObject);
    procedure hisDBbylandcovercats1Click(Sender: TObject);
    procedure Integer2Click(Sender: TObject);
    procedure hisDBbylandcovercategory1Click(Sender: TObject);
    procedure hisDBslopecats1Click(Sender: TObject);
    procedure ICESat2canopyaddDEMdata1Click(Sender: TObject);
    procedure hisDBcanopyheights1Click(Sender: TObject);
    procedure hisDBbycanopyheight1Click(Sender: TObject);
    procedure hisDBByslope1Click(Sender: TObject);
    procedure hisDBbylandcover1Click(Sender: TObject);
    procedure hisDBall3groups1Click(Sender: TObject);
    procedure hisDBall3groupings1Click(Sender: TObject);
    procedure ICESat2filecleanup1Click(Sender: TObject);
    procedure RewriteinsubdirOGR1Click(Sender: TObject);
    procedure Allpointsinpolygon1Click(Sender: TObject);
    procedure AllpointsinpolygonallopenDBs1Click(Sender: TObject);
    procedure Distancetopointsintable1Click(Sender: TObject);
    procedure Pointslopebyalgorithm1Click(Sender: TObject);
    procedure Fuibn1Click(Sender: TObject);
    procedure AddDEMdata1Click(Sender: TObject);
    procedure Elevationslopeplots1Click(Sender: TObject);
    procedure Landcoversummary1Click(Sender: TObject);
    procedure Create1Click(Sender: TObject);
    procedure CreategeographicPRJfile1Click(Sender: TObject);
    procedure Markanddeleteallrecordspriortothisone1Click(Sender: TObject);
    procedure Selectallrecordsafterthisone1Click(Sender: TObject);
    procedure Colorbarchart1Click(Sender: TObject);
    procedure Percentfield1Click(Sender: TObject);
    procedure GDALsubsettomatchthisrecord1Click(Sender: TObject);
    procedure Removeduplicatepositions1Click(Sender: TObject);
    procedure Filltrackvoids1Click(Sender: TObject);
    procedure AddEGMfields1Click(Sender: TObject);
    procedure RMSE1Click(Sender: TObject);
    procedure AllpointsinlinewithXYZ1Click(Sender: TObject);
    procedure EGM20081Click(Sender: TObject);
    procedure EGM961Click(Sender: TObject);
    procedure AddEGMfieldsfromalgorithms1Click(Sender: TObject);
    procedure N45Click(Sender: TObject);
    procedure Latprofiles1Click(Sender: TObject);
    procedure Latprofiles2Click(Sender: TObject);
    procedure Lattimecolors1Click(Sender: TObject);
    procedure Boxplot1Click(Sender: TObject);
    //procedure ransposeforwinecontest1Click(Sender: TObject);
    //procedure Graphfortransposeddata1Click(Sender: TObject);
    procedure N1degreetilestocoverrecordsintable1Click(Sender: TObject);
    procedure BestDEMbycategory1Click(Sender: TObject);
    procedure RankDEMs1Click(Sender: TObject);
    procedure DEMIXtilesummary1Click(Sender: TObject);
    procedure PickParam1Click(Sender: TObject);
    procedure Filteroutsignedcriteriameanandmedian1Click(Sender: TObject);
    procedure Hide1Click(Sender: TObject);
    procedure Allcriteriavalues1Click(Sender: TObject);
    procedure PercentageofcriteriawhereDEMisbest1Click(Sender: TObject);
    procedure Averageranksbyarea1Click(Sender: TObject);
    procedure COPoALOS1Click(Sender: TObject);
    procedure BestDEMpertilebycriteria1Click(Sender: TObject);
    procedure N7Elevationdifferencecriteria1Click(Sender: TObject);
    procedure Ascending1Click(Sender: TObject);
    procedure Descending1Click(Sender: TObject);
    procedure Alphabetize1Click(Sender: TObject);
    procedure Createshapefilewithboundingboxforeachrecord1Click(Sender: TObject);
    procedure CreateDBwithcornersandcenterofeveryrecord1Click(Sender: TObject);
    procedure GraphsbestDEMpertilebycriteriasortbytilecharacteristics1Click(Sender: TObject);
    procedure Graphfilters1Click(Sender: TObject);
    procedure FilterforDEMIXtiles1Click(Sender: TObject);
    procedure NormalizeddifferencesfromreferenceDEM1Click(Sender: TObject);
    procedure Stackedpercentages1Click(Sender: TObject);
    procedure Deleterecord1Click(Sender: TObject);
    procedure AlphabetizefieldwithCSVsubfields1Click(Sender: TObject);
    procedure iesbyopinions1Click(Sender: TObject);
    procedure Wins1Click(Sender: TObject);
    procedure LoadthisDEM1Click(Sender: TObject);
    procedure Updatestatus1Click(Sender: TObject);
    procedure Averagebylatitude1Click(Sender: TObject);
    procedure Datumshift2Click(Sender: TObject);
    procedure Latlongelevofrecordcorners1Click(Sender: TObject);
    procedure Exporttablewithuniquerecords1Click(Sender: TObject);
    procedure DEMIX1Click(Sender: TObject);
    procedure DEMIXtileinvertory1Click(Sender: TObject);
    procedure Filterforjustsignedcrirteria1Click(Sender: TObject);
    procedure Meanandmedianhistograms1Click(Sender: TObject);
    procedure AddIMAGEfieldfordifferencedistributiongraphs1Click(
      Sender: TObject);
    procedure Modestandarddeviationplots1Click(Sender: TObject);
    procedure AddDEMIXtilecentroid1Click(Sender: TObject);
    procedure Clustermaplocations1Click(Sender: TObject);
    procedure Clusterstatistics1Click(Sender: TObject);
    procedure Addaverageelevationinwindow1Click(Sender: TObject);
    procedure MaskallopenDEMgrids1Click(Sender: TObject);
    procedure Currenttest1Click(Sender: TObject);
    procedure Evaluationrangeforcriterion1Click(Sender: TObject);
    //procedure SSIMtodissimilarity1Click(Sender: TObject);
    procedure Addtilecharacteristics1Click(Sender: TObject);
    procedure Ascending2Click(Sender: TObject);
    procedure Descending2Click(Sender: TObject);
    //procedure GraphSSIMR2foratile1Click(Sender: TObject);
    procedure Clustercomposition1Click(Sender: TObject);
    procedure TransposeSSIMFUVforclusters1Click(Sender: TObject);
    procedure ilecharacteristicsbytileforCopDEM1Click(Sender: TObject);
    procedure ClustersperDEMIXtile1Click(Sender: TObject);
    procedure Clusterdiversity1Click(Sender: TObject);
    procedure Clustersensitivity1Click(Sender: TObject);
    procedure CreateDBwithparametersbyDEM1Click(Sender: TObject);
    procedure LoadtestandreferenceDEMs1Click(Sender: TObject);
    procedure GraphSSIMFUVbycluster1Click(Sender: TObject);
    procedure BitBtn13Click(Sender: TObject);
    procedure SSIMFUVgraphforthistile1Click(Sender: TObject);
    procedure Comparerankingswithdifferentcriteria1Click(Sender: TObject);
    procedure DifferentrankingsbyCriteria1Click(Sender: TObject);
    procedure AssignDEMIXDEMcolors1Click(Sender: TObject);
    procedure estDEMlegend1Click(Sender: TObject);
    procedure Vertical1Click(Sender: TObject);
    procedure Graphbyareawithaveragescoreforselectedcriteria1Click(Sender: TObject);
    procedure Graphbytilewithaveragescoreforselectedcriteria1Click(Sender: TObject);
    procedure Addconstanttofield2Click(Sender: TObject);
    procedure Addconstanttofield3Click(Sender: TObject);
    procedure Dividefieldbyconstant2Click(Sender: TObject);
    procedure Singlefieldarithmetic1Click(Sender: TObject);
    procedure GraphSSIMFUVbyclustermeans1Click(Sender: TObject);
    //procedure AddsloperoughnessrelieftoDB1Click(Sender: TObject);
    procedure Filterfor999valuesinanyevaluation1Click(Sender: TObject);
    procedure CiompareCOPtorivals1Click(Sender: TObject);
    procedure CopHeadtoheadrecord1Click(Sender: TObject);
    procedure InventoryFUVSSIMcriteriainDB1Click(Sender: TObject);
    procedure Copycolumntoclipboard1Click(Sender: TObject);
    procedure QuartilesinCLUSTERfieldbasedonsort1Click(Sender: TObject);
    procedure N53Click(Sender: TObject);
    procedure Addmultiplefields1Click(Sender: TObject);
    procedure BitBtn24Click(Sender: TObject);
    procedure Byclusters1Click(Sender: TObject);
    procedure Alltiles1Click(Sender: TObject);
    procedure TransposeSSIMR2forclusters1Click(Sender: TObject);
    procedure Evaluationrangebycriterion1Click(Sender: TObject);
    procedure Clustermeangraphs1Click(Sender: TObject);
    procedure Areasinclusters1Click(Sender: TObject);
    procedure AddCOPALOSpercentprimarydata1Click(Sender: TObject);
    //procedure AddareafieldtoDB1Click(Sender: TObject);
    procedure CopDEMandLandcoverforthistile1Click(Sender: TObject);
    procedure Loadmapsforthisarea1Click(Sender: TObject);
    procedure LoadCopDEMandLandcoverforarea1Click(Sender: TObject);
    procedure Filterfor0valuesinanyevaluation1Click(Sender: TObject);
    procedure CriteriaforeachDEMIXtile1Click(Sender: TObject);
    procedure PercentilesforCOPbycriterionforeachtile1Click(Sender: TObject);
    procedure Sumforallnumericfields1Click(Sender: TObject);
    procedure SortbyBESTEVAL1Click(Sender: TObject);
    procedure AddlatlongfieldstoDB1Click(Sender: TObject);
    procedure MapsbyclusterandDEM1Click(Sender: TObject);
    procedure Removerowsmissinganyevaluations1Click(Sender: TObject);
    procedure Averagetilecharacteristicsbycluster1Click(Sender: TObject);
    procedure Winpercentagebycriterion1Click(Sender: TObject);
    procedure Winpercentagesbyarea1Click(Sender: TObject);
    procedure GraphwinningpercentagebyDEM1Click(Sender: TObject);
    procedure OpenDEMIXgraphs1Click(Sender: TObject);
    procedure Graphofcriteriaforareaortile1Click(Sender: TObject);
    procedure GraphofPrimaryDataFractionbyClusters1Click(Sender: TObject);
    procedure AddcountryareafieldstoDB1Click(Sender: TObject);
    procedure AddcolorsforFULLU120U80U101Click(Sender: TObject);
    procedure SortandreplaceDB2Click(Sender: TObject);
    procedure Descending3Click(Sender: TObject);
    procedure Winlosstie1Click(Sender: TObject);
    procedure Filterforevaluations11Click(Sender: TObject);
    procedure LATEXtable1Click(Sender: TObject);
    procedure AddKoppenclass1Click(Sender: TObject);
    procedure Numberoftestareasandtilesbycountry1Click(Sender: TObject);
    //procedure Pointfilter2Click(Sender: TObject);
    //procedure Pointfilter1Click(Sender: TObject);
  private
    procedure PlotSingleFile(fName : PathStr; xoff,yoff : float64);
    procedure SetUpLinkGraph;
    function DrawDEMTerrainprofile(Sender: TObject): tThisBaseGraph;
    procedure ColumnOps;
    procedure DrawPointReflectanceGraph(var TheGraph : tThisBaseGraph; theTitle : shortString; Normalize : boolean = false);
    function GetMultipleEntriesFromTableField(WhatFor,aName : shortstring) : tStringList;
    function GetSingleEntryFromTableField(WhatFor,aName : shortstring) : ShortString;
    procedure SearchAndReplace(aField : ShortString; Before,After : ANSIString; var Changed : integer);
    //procedure ThreeDGraph(NoVertExag: boolean);
    procedure SetFonts;
    procedure Distributionsummary(Title : shortstring);
    procedure SingleFieldArithmetic(DBonTable,Operation : integer; CheckField : shortstring);

  public
     DBonTable,
     xdrawspot,ydrawspot : integer;
     Col,Row : integer;
     SelectedColumn: ShortString;
     SelectedColumnType : tFieldType;
     CanCloseIt,
     SimplePlot,
     NoStatsGraph,
     RedGrayGraph,
     AnyHiddenColumns,
     TrackBarringAllowed,
     AllOptionsForFans,
     FormWorking,
     VATEdit,
     TrainingClassAvailable,
     EditSymbologyOnly,
     Closing : boolean;
     LinkGraph : tThisBaseGraph;
     AllGraphBitmap : tMyBitmap;
     GraphOwnerBitmap,
     BaseMapBitmap : tMyBitmap;
     ForceXMax,ForceYMax : float64;
     SavedTheHiddenColumns : Array100Boolean;


     procedure ViewshedTargetCoverage(Target : integer; fName : PathStr = '');
     function Do3Dshapefileprofile1Click(Sender: TObject) : tThisBaseGraph;
     procedure MakeGridBandsInClassBox(aField: shortstring);
     procedure MakeDistanceGridToClassCentroid(aField: shortstring);
     procedure Highlightrecs(JustCurrent : boolean);
     procedure ToggleLayer(LayerOn : boolean);
     procedure ShowFilteredDB(ShowFilter, ShowN: boolean);
     procedure HideColumns;
     procedure UnHideColumns;
     procedure SaveHiddenColumns;
     procedure RestoreHiddenColumns;
     procedure HideHouseKeepingColumns;
     procedure GetReadyForGeologyGeometry;
     procedure ShowStatus;
     procedure MakeFormDockable;
     procedure HighlightFan(inColor : tPlatformColor);
     function CreateGrid(HowCreate : tcgHow; GridSize : float64 = -99) : integer;
  end;



implementation

{$R *.DFM}

uses
   PETdbUtils,

   {$IfDef ExSat}
   {$Else}
      DEMEros,

      {$IfDef ExAdvancedSats}
      {$Else}
         multigrid,
      {$EndIf}

      {$IfDef ExGeoStats}
      {$Else}
         sup_class_aux_grids,
      {$EndIf}
   {$EndIf}

   DEMMapf,DEMMapDraw,
   DEMCoord,
   DEMLOSW,
   DEMDef_routines,
   Make_Tables,
   Text_report_options,
   DEMdbDisplay,
   DataBaseCreate,
   DEMOptions,
   PETMath,
   PETImage,
   GetLatLn,
   demdatabase,
   Petimage_form,
   Petmar_ini_file,

   DEMESRIShapeFile,
   toggle_db_use,
   DEMEditW,
   GIS_scaled_symbols,
   DEM_Manager,
   demstringgrid,
   new_field,

   {$IfDef NoExternalPrograms}
   {$Else}
      MD_use_tools,
   {$EndIf}

   {$IfDef ExTIN}
   {$Else}
      DEM_tin,
   {$EndIf}

   {$IfDef ExKML}
   {$Else}
      kml_creator, kml_opts,
   {$EndIf}

   {$ifDef ExPointCloud}
   {$Else}
      Slicer_3d,
      Point_Cloud_Memory,
      Las_Lidar,
   {$EndIf}

   {$IfDef ExMag}
   {$Else}
      demmagvar,
   {$EndIf}

   {$IfDef NoClustering}
   {$Else}
      MVClusterClientDataSet,
      ClusterOptions, param_graphs,
   {$EndIf}

   {$IfDef ExFourier}
   {$Else}
      PetFouri,
      FitFourier,
      CrossCor,
   {$EndIf}

   {$IfDef ExFMX3D}
   {$Else}
      View3D_main,
   {$EndIf}

   {$IfDef ExGeology}
   {$Else}
      sc_ColMain,
      Beach_ball_options,
   {$EndIf}


   Petmar_geology,

   {$IfDef ExSidescan}
   {$Else}
      SideImg, mst_format,
   {$EndIf}

   {$IfDef ExGeography}
   {$Else}
      KoppenGr,
      sun_position,
      moon_montenbruk_pfleger,
   {$EndIf}

   {$IfDef ExIndexes}
   {$Else}
      DEM_indexes,
   {$EndIf}

   {$IfDef ExViewshed}
   {$Else}
      DEMfanParams,
      DEMweapn,
   {$EndIf}

   {$IfDef ExAdvancedGIS}
   {$Else}
      Map_Masking,
   {$EndIf}

   {$IfDef ExPers}
   {$Else}
      DEMPersw,
   {$EndIf}

   {$IfDef ExGeostats}
   {$Else}
      DEMtrendopt,
      NetMainW, grid_over_map,
      DEMStat, demssocalc,
   {$EndIf}

   {$IfDef ExRedistrict}
   {$Else}
      demredistrict,
   {$EndIf}


    {$IfDef ExMilIcons}
    {$Else}
       dem_milicon,
    {$EndIf}

    {$IfDef ExOceanography}
    {$Else}
       OCEANCAL, drift_model,
    {$EndIf}

    DEMIX_filter,
    demix_definitions,
    DEMIX_Control,
    DEMIX_graphs,
    DEMIX_evals_scores_graphs,

   map_overlays,
   db_display_options,
   gdal_tools,
   Least_cost_path,
   db_join,Insert_Point,Thread_timers,
   add_time_fields, db_field_concatenate, basemap, dem_gaz_opts, DEMShowDbRecord,
   DEMTigerOps,
   Tiger_address,

{Main program MDI window for different programs that use this module}
   Nevadia_Main;
{End of the MDI parent declaration}

var
   HighlightCycle : integer;
   BroadCastingFilterChanges : boolean;

   const
   sfaMult = 1;
   sfaAdd = 2;
   sfaDiv = 3;


procedure ThreeDGraph(DBOnTable : integer; NoVertExag : boolean);
var
  Mult,ThinFactor : integer;
  MinColor,MaxColor : float64;
  StringColorField,NumericColorField : ShortString;
  DataThere : tStringList;

     procedure OpenNew3Dform;
     var
        Min,Max : float64;
        zRange : float64;
        GeometryFName,ColorsFName : PathStr;
         Points : ^tPointXYZIArray;
         MemReq : int64;
         i,Mult : integer;
         Outf : file;
     begin
        {$IfDef RecordOpenGL} WriteLineToDebugFile('OpenNew3Dform in'); {$EndIf}
         ShowHourglassCursor;
         GISdb[DBonTable].EmpSource.Enabled := false;
         GISdb[DBonTable].MyData.FindFieldRange(GISdb[DBonTable].dbOpts.ZField,Min,Max);
         zRange := Max - Min;

        if MDDef.ReverseZFields then begin
           Mult := -1;
           Min := Max;
        end
        else Mult := 1;

        {$IfDef RecordOpenGL} WriteLineToDebugFile('OpenNew3Dform start point extraction'); {$EndIf}
        MemReq := GISdb[DBonTable].MyData.FiltRecsInDB * SizeOf(tPointXYZI);
        GetMem(Points,MemReq);
        StartProgress('Load 3D');
        i := 0;
        GISdb[DBonTable].MyData.First;
        while not GISdb[DBonTable].MyData.eof do begin
            if (i mod 1000 = 0) then begin
               GISdb[DBonTable].EmpSource.Enabled := false;
               UpdateProgressBar(i/GISdb[DBonTable].MyData.FiltRecsInDB);
            end;
            inc(i);
            Points^[i].x := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(GISdb[DBonTable].dbOpts.XField);
            Points^[i].y := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(GISdb[DBonTable].dbOpts.YField);
            Points^[i].z := Mult * GISdb[DBonTable].MyData.GetFieldByNameAsFloat(GISdb[DBonTable].dbOpts.ZField);
            if (NumericColorField <> '') then Points^[i].Int := round(255* ((GISdb[DBonTable].MyData.GetFieldByNameAsFloat(NumericColorField)-MinColor)/(MaxColor-MinColor)))
            else Points^[i].Int := round(255* ((Points^[i].z-Min)/zRange));
            GISdb[DBonTable].MyData.Next;
        end;
        GeometryFName := Petmar.NextFileNumber(MDTempDir, GISdb[DBonTable].DBName + '_','.xyzib');
        ColorsFName := Palette256Bitmap(p256Spectrum);  //p256Terrain);
        AssignFile(Outf,GeometryFName);
        rewrite(Outf,1);
        BlockWrite(OutF,Points^,MemReq);
        CloseFile(outf);
        EndProgress;
        FreeMem(Points,MemReq);
        {$IfDef RecordOpenGL} WriteLineToDebugFile('Export binary over, n=' + IntToStr(i)); {$EndIf}
        FMX3dViewer(True,GeometryfName,'','','','', ColorsFName,'','','','',NoVertExag);
        {$IfDef RecordOpenGL} WriteLineToDebugFile('OpenNew3Dform out'); {$EndIf}
     end;


begin
   {$IfDef RecordOpenGL} WriteLineToDebugFile('Tdbtablef.N3Dgraph1Click in'); {$EndIf}
   DataThere := Nil;

   ThinFactor := 1;
   GISdb[DBonTable].PickNumericFields(dbgtUnspecified,4,'X','Y','Z');

   {$IfDef RecordOpenGL} WriteLineToDebugFile('Tdbtablef.N3Dgraph1Click picked'); {$EndIf}

   ShowHourglassCursor;
   GISdb[DBonTable].EmpSource.Enabled := false;
   if (StringColorField <> '') then GISdb[DBonTable].DBFieldUniqueEntries(StringColorField,DataThere);
   //ShowHourglassCursor;
   GISdb[DBonTable].EmpSource.Enabled := false;
   if (NumericColorField <> '') then GISdb[DBonTable].FieldRange(NumericColorField,MinColor,MaxColor);
   if MDDef.ReverseZFields then Mult := -1 else Mult := 1;
   OpenNew3Dform;
   DataThere.Free;
   GISdb[DBonTable].EmpSource.Enabled := true;
   {$IfDef RecordOpenGL} WriteLineToDebugFile('Tdbtablef.N3Dgraph1Click out'); {$EndIf}
end;


procedure TraceRoute3D(DBonTable : integer);
{$IfDef ExFMX3D}
begin
{$Else}
var
   OutForm : TView3DForm;
   Lat,Long,xutm,yutm : float64;
   z : float32;
begin
   if GISdb[DBonTable].LayerIsOn then GISdb[DBonTable].ToggleLayer(false);
   GISdb[DBonTable].theMapOwner.Image1.Canvas.Pixels[0,0] := clRed;
   OutForm := MapTo3DView(GISdb[DBonTable].TheMapOwner.MapDraw,10000);
   GISdb[DBonTable].EmpSource.Enabled := false;
   GISdb[DBonTable].MyData.First;
   while not GISdb[DBonTable].MyData.eof do begin
      if GISdb[DBonTable].ValidLatLongFromTable(Lat,Long) then begin
         if DEMGlb[GISdb[DBonTable].theMapOwner.MapDraw.DEMonMap].GetElevFromLatLongDegree(Lat,Long,z) then begin
            GISdb[DBonTable].theMapOwner.MapDraw.LatLongDegreeToUTM(Lat,Long,xutm,yutm);
            OutForm.AddPointWithSpecificColor(xutm,yutm,z+1,0,0);
            OutForm.AddPointWithSpecificColor(xutm+1,yutm+1,z+1,0,0);
            OutForm.AddPointWithSpecificColor(xutm+1,yutm-1,z+1,0,0);
            OutForm.AddPointWithSpecificColor(xutm-1,yutm+1,z+1,0,0);
            OutForm.AddPointWithSpecificColor(xutm-1,yutm-1,z+1,0,0);
            Delay(250);
            OutForm.Activate;
            if (GISdb[DBonTable].MyData.GetFieldByNameAsString('HEADING') <> '') then begin
               OutForm.Layout3D1.RotationAngle.Y := GISdb[DBonTable].MyData.GetFieldByNameAsFloat('HEADING') + 90;
            end;
            OutForm.Show;
         end;
      end;
      GISdb[DBonTable].MyData.Next;
   end;
   OutForm.Show;
   GISdb[DBonTable].ShowStatus;
{$EndIf}
end;


procedure Tdbtablef.SingleFieldArithmetic(DBonTable,Operation : integer; CheckField : shortstring);
var
   TStr  : shortString;
   RecCount,i  : integer;
   Mult : float64;
   Value : float32;
   ItsFloatField : boolean;
begin
    if (Operation = sfaMult) then TStr := 'multiply'
    else if (Operation = sfaDiv) then TStr := 'divide'
    else TStr := 'add';
    if (CheckField = '') then CheckField := GISDB[DBonTable].PickField(TStr + ' by constant',NumericFieldTypes);
    if (CheckField = '') then exit;

    Mult := -1;
    ReadDefault('Constant to ' + Tstr,Mult);
    if (Operation = sfaDiv) then Mult := 1 / Mult;
    ItsFloatField := GISdb[DBonTable].MyData.IsFloatField(CheckField);
    GISdb[DBonTable].MyData.First;
    i := 0;
    RecCount := GISdb[DBonTable].MyData.RecordCount;
    StartProgress(TStr);
    while not GISdb[DBonTable].MyData.EOF do begin
       if (i mod 100 = 0) then begin
          GISdb[DBonTable].EmpSource.Enabled := false;
          UpdateProgressBar(i/RecCount);
       end;
       inc(i);
       GISdb[DBonTable].MyData.Edit;
       if GISdb[DBonTable].GetFloat32FromTableLinkPossible(CheckField,Value) then begin
          if (Operation = sfaMult) or (Operation = sfaDiv) then Value := Value * Mult
          else Value := Value + Mult;
          if ItsFloatField then GISdb[DBonTable].MyData.SetFieldByNameAsFloat(CheckField,Value)
          else GISdb[DBonTable].MyData.SetFieldByNameAsInteger(CheckField,round(Value));
       end;
       GISdb[DBonTable].MyData.Next;
    end;
    GISdb[DBonTable].ClearFieldRange(CheckField);
    ShowStatus;
end;


procedure Tdbtablef.Singlefieldarithmetic1Click(Sender: TObject);
begin
   SingleFieldArithmeticPopupMenu.Popup(Mouse.CursorPos.X,Mouse.CursorPos.Y);
end;

type tPointInPolygon = (pipLabels,pipDelete,pipSetMask);

procedure MarkPointInPolygon(PIP : tPointInPolygon; DB,MaskDB : integer; NameField,MaskFieldName : string10);
var
   i,x,y,rc : integer;
   LatCent,LongCent : float64;
   bmp : tMyBitmap;
begin
   StartProgress('Mask');
   if AreaShapeFile(GISdb[DB].ShapeFileType) or GISdb[DB].ItsaPointDB then begin
      GISdb[MaskDB].MyData.First;
      i := 0;
      rc := GISdb[DB].MyData.RecordCount;
      CloneImageToBitmap(GISdb[DB].theMapOwner.Image1,bmp);
      BMP.Canvas.Pen.Color := clBlack;
      BMP.Canvas.Brush.Color := clBlack;
      BMP.Canvas.Brush.Style := bsSolid;
      GISdb[MaskDB].MyData.First;
      GISdb[MaskDB].EmpSource.Enabled := false;
      while not GISdb[MaskDB].MyData.eof do begin
         inc(i);
         UpdateProgressBar(i / rc);
         GISdb[MaskDB].aShapeFile.PlotSingleRecordMap(GISdb[MaskDB].TheMapOwner.MapDraw,BMP,GISdb[MaskDB].MyData.RecNo);
         GISdb[MaskDB].MyData.Next;
      end;

      Bmp.SaveToFile(mdtempdir + 'tester.bmp');

      GISdb[DB].EmpSource.Enabled := false;
      GISdb[DB].MyData.First;
      while not GISdb[DB].MyData.eof do begin
         if AreaShapeFile(GISdb[DB].ShapeFileType) then
            GISdb[DB].aShapeFile.AreaAndCentroid(GISdb[DB].TheMapOwner.MapDraw.PrimMapProj,GISdb[DB].MyData.RecNo,LatCent,LongCent)
         else begin
            LatCent := GISdb[DB].MyData.GetFieldByNameAsFloat(GISdb[DB].LatFieldName);
            LongCent := GISdb[DB].MyData.GetFieldByNameAsFloat(GISdb[DB].LongFieldName);
         end;
         GISdb[MaskDB].TheMapOwner.MapDraw.LatLongDegreeToScreen(LatCent,LongCent,x,y);
         if GISdb[MaskDB].TheMapOwner.MapDraw.OnScreen(x,y) and (BMP.Canvas.Pixels[x,y] = clBlack) then begin
            GISdb[DB].MyData.Edit;
            if (PIP = pipLabels) then begin
               GISdb[DB].MyData.SetFieldByNameAsString(MaskFieldName,GISdb[MaskDB].MyData.GetFieldByNameAsString(NameField));
            end
            else if (PIP = pipSetMask) then begin
               GISdb[DB].MyData.SetFieldByNameAsString(MaskFieldName,'1');
            end
            else if (PIP = pipDelete) then begin
               GISdb[DB].MyData.MarkRecordForDeletion;
            end;
         end;
         GISdb[DB].MyData.Next;
      end;
      FreeAndNil(BMP);
   end;
end;


procedure Tdbtablef.NearTIGERroads1Click(Sender: TObject);
begin
   {$IfDef ExTiger}
   {$Else}
      GISdb[DBonTable].TheMapOwner.TigerRoadMask(-99,true,DBonTable);
   {$EndIf}
end;



procedure Tdbtablef.NewdbfwithspeedpathbyID1Click(Sender: TObject);
var
   Speed,Time,Time2,Lat,Long,Lat2,Long2,Az,Dist,LatMid,LongMid : float64;
   fName : PathStr;
   rTable : tMyData;
   i      : integer;
   ID,LastID : shortstring;
begin
   //with GISdb[DBonTable] do begin
      fName := ExtractFilePath(GISdb[DBonTable].DBFullName) + 'ship_tracks' + DefaultDBExt;
      MakeCliwocTable(fName);
      rTable := tMyData.Create(fName);
      GISdb[DBonTable].EmpSource.Enabled := false;
      GISdb[DBonTable].MyData.First;
      LastID := GISdb[DBonTable].MyData.GetFieldByNameAsString('ID');
      Time := GISdb[DBonTable].MyData.GetFieldByNameAsFloat('DEC_YEAR');
      GISdb[DBonTable].ValidLatLongFromTable(Lat,Long);
      GISdb[DBonTable].MyData.Next;
      StartProgress('Tracks');
      i := 0;
      while not GISdb[DBonTable].MyData.eof do begin
         inc(i);
         if (I mod 500 = 0) then UpdateProgressBar(i/GISdb[DBonTable].MyData.FiltRecsInDB);

         ID := GISdb[DBonTable].MyData.GetFieldByNameAsString('ID');
         Time2 := GISdb[DBonTable].MyData.GetFieldByNameAsFloat('DEC_YEAR');
         GISdb[DBonTable].ValidLatLongFromTable(Lat2,Long2);
         if (ID = LastID) then begin
           if ((Time2 - Time) > 0.001) and ((Time2 - Time) < 0.008) then  begin
              VincentyCalculateDistanceBearing(Lat2,Long2,Lat,Long,Dist,Az);
              VincentyPointAtDistanceBearing(Lat2,Long2,0.5*Dist,Az,LatMid,LongMid);
              rTable.Insert;
              rTable.SetFieldByNameAsFloat('DEC_YEAR',(Time2+Time)/2);
              rTable.SetFieldByNameAsFloat('LAT',LatMid);
              rTable.SetFieldByNameAsFloat('LONG',LongMid);
              Speed := (Dist * 0.001) / (Time2-Time) / 365 / 24;
              rTable.SetFieldByNameAsFloat('SPEED_KPH',Speed);
              rTable.SetFieldByNameAsFloat('KNOTS',Speed*0.539956803);

              rTable.SetFieldByNameAsFloat('HEADING',Az);
              rTable.SetFieldByNameAsString('SHIP',GISdb[DBonTable].MyData.GetFieldByNameAsString('SHIP'));
              rTable.SetFieldByNameAsInteger(GISdb[DBonTable].MonthFieldName,GISdb[DBonTable].MyData.GetFieldByNameAsInteger(GISdb[DBonTable].MonthFieldName));
              rTable.Post;
           end;
         end;
         Lat := Lat2;
         Long := Long2;
         Time := Time2;
         LastID := ID;
         GISdb[DBonTable].MyData.Next;
      end;
      EndProgress;
      ShowStatus;
   //end;
end;

procedure Tdbtablef.AwayfromTIGERroads1Click(Sender: TObject);
begin
   {$IfDef ExTiger}
   {$Else}
      GISdb[DBonTable].TheMapOwner.TigerRoadMask(-99,false,DBonTable);
   {$EndIf}
end;


procedure Tdbtablef.Normalizedbasinprofile1Click(Sender: TObject);
begin
   DEMTerrainprofile1Click(Sender);
end;

procedure Tdbtablef.NormalizeddifferencesfromreferenceDEM1Click(Sender: TObject);
begin
   DEMIXwineContestCriterionGraph(dgNormalizedDiff,DBonTable);
end;

procedure Tdbtablef.Normalizefield1Click(Sender: TObject);
var
   i,j : integer;
   zs : ^bfarray32;
   fName : PathStr;
   Missing : int64;
   Min,Max,z : float64;
   Output,FieldsUsed : tStringList;
   TStr : ANSIString;
   UniqueField,fu : shortString;
   MomentVar : array[0..100] of tMomentVar;
begin
   UniqueField := 'DEMIX_TILE';   //GISdb[DBonTable].PickField('Unique field for linking normalized DB',[ftString,ftInteger]);
   FieldsUsed := GISdb[DBonTable].GetAnalysisFields;
   FieldsUsed.Duplicates := DupIgnore;
   FieldsUsed.Insert(0,RecNoFName);
   if GISdb[DBonTable].MyData.FieldExists(GISdb[DBonTable].LongFieldName) then FieldsUsed.Insert(0,GISdb[DBonTable].LongFieldName);
   if GISdb[DBonTable].MyData.FieldExists(GISdb[DBonTable].LatFieldName) then FieldsUsed.Insert(0,GISdb[DBonTable].LatFieldName);
   FieldsUsed.Insert(0,UniqueField);

   StartProgress('Moments');
   GISdb[DBonTable].EmpSource.Enabled := false;
   for j := 0 to pred(FieldsUsed.Count) do begin
      fu := FieldsUsed.Strings[j];
       if (fu <> GISdb[DBonTable].LatFieldName) and (fu <> GISdb[DBonTable].LongFieldName) and (fu <> RecNoFName) and (fu <> UniqueField) then begin
          New(zs);
          UpdateProgressBar(j/FieldsUsed.Count);
          GISdb[DBonTable].EmpSource.Enabled := false;
          GetFieldValuesInArray(GISdb[DBonTable].MyData,fu,zs^,MomentVar[j].NPts,Missing,Min,Max);
          moment(zs^,MomentVar[j],msAll);
          Dispose(zs);
       end;
   end;

   Output := tStringList.Create;
   TStr := '';
   for j := 0 to pred(FieldsUsed.Count) do begin
      if (j > 0) then TStr := TStr + ',';
      TStr := TStr + FieldsUsed.Strings[j];
   end;
   Output.Add(Tstr);

    StartProgress('Normalize DB fields');
    GISdb[DBonTable].EmpSource.Enabled := false;
    i := 0;
    GISdb[DBonTable].MyData.First;
    while not GISdb[DBonTable].MyData.EOF do begin
       inc(i);
       if (i mod 500 = 0) then  begin
          UpdateProgressBar(i/GISdb[DBonTable].MyData.FiltRecsInDB);
          GISdb[DBonTable].EmpSource.Enabled := false;
       end;

       TStr := '';
       for j := 0 to pred(FieldsUsed.Count) do begin
          fu := FieldsUsed.Strings[j];
          if (j > 0) then TStr := TStr + ',';
          if (fu = GISdb[DBonTable].LatFieldName) or (fu = GISdb[DBonTable].LongFieldName) or (fu = RecNoFName) or  (fu = UniqueField)  then begin
             TStr := TStr + GISdb[DBonTable].MyData.GetFieldByNameAsString(FieldsUsed.Strings[j]);
          end
          else begin
              if GISdb[DBonTable].MyData.CarefullyGetFieldByNameAsFloat64(FieldsUsed.Strings[j],z) then
                 TStr := TStr + RealToString((z - MomentVar[j].mean) / MomentVar[j].std_dev,-12,-5)
              else TStr := TStr + ' ';
          end;
       end;
       Output.Add(Tstr);
       GISdb[DBonTable].MyData.Next;
       if WantOut then break;
    end;
    fName := ExtractFilePath(GISdb[DBonTable].DBFullName) + 'Norm_' +  GISdb[DBonTable].dbName;
    GISdb[DBonTable].theMapOwner.StringListToLoadedDatabase(Output,fName);
   FieldsUsed.Free;
   ShowStatus;
   wmDEM.StatusBar1.Panels[0].Text := '';
end;


procedure Tdbtablef.Noverticalexaggeration1Click(Sender: TObject);
begin
   ThreeDGraph(DBOnTable,true);
end;



procedure Tdbtablef.Numberoftestareasandtilesbycountry1Click(Sender: TObject);
begin
   InventoryAreasAndTilesByCountry(DBonTable);
end;

procedure Tdbtablef.Octree1Click(Sender: TObject);
const
   x_field = 'X';
   y_field = 'Y';
   z_field = 'Z';
   MaxSize = 50000;

     procedure Status(What: shortstring);
     begin
        GISdb[DBonTable].EmpSource.Enabled := false;
        wmdem.StatusBar1.Panels[0].Text := what;
        ShowHourglassCursor;
     end;

     procedure Octree(XMin,XMax,YMin,YMax,ZMin,ZMax : float64; var BaseX,BaseY,BaseZ : shortstring);
     var
        x,y,z : integer;
        xlo,xhi,ylo,yhi,zlo,zhi,
        dx,dy,dz : float64;
        ThX,ThY,ThZ,
        oName : shortstring;
        fName : PathStr;

            procedure SetFilter;
            begin
                oName := 'x=' + ThX +  'y=' + ThY +  'z=' + ThZ;
                GISdb[DBonTable].MyData.ApplyFilter( x_field + '>=' + RealToString(xlo,-18,-8) + ' AND ' + x_field + ' <=' + RealToString(xhi,-18,-8) +  ' AND ' +
                          y_field + '>=' + RealToString(ylo,-18,-8) + ' AND ' + y_field + ' <=' + RealToString(yhi,-18,-8) +  ' AND ' +
                          z_field + '>=' + RealToString(zlo,-18,-8) + ' AND ' + z_field + ' <=' + RealToString(zhi,-18,-8));
            end;

            procedure RecordResults;
            begin
               if (GISdb[DBonTable].MyData.FiltRecsInDB > 0) then begin
                  GISdb[DBonTable].FillFieldWithValue('OCTREE',oName);
                  fName := ExtractFilePath(GISdb[DBonTable].dbFullName) + GISdb[DBonTable].dbName + '-' + oName + DefaultDBExt;
                  GISdb[DBonTable].SaveCurrentDBaseSubset(fName);
               end;
            end;

     begin
         dx := 0.5 * (xmax - xmin);
         dy := 0.5 * (ymax - ymin);
         dz := 0.5 * (zmax - zmin);

         xlo := xmin;
         xhi := xmax;
         ThX := BaseX;

         ylo := ymin;
         yhi := ymax;
         ThY := BaseY;

         zlo := zmin;
         zhi := zmax;
         ThZ := BaseZ;
         if GISdb[DBonTable].MyData.FiltRecsInDB < MaxSize then begin
            RecordResults;
         end
         else for x := 0 to 1 do begin
            xlo := xmin + x * dx;
            xhi := xmin + succ(x) * dx;
            ThX := BaseX + IntToStr(x);

            SetFilter;
            if GISdb[DBonTable].MyData.FiltRecsInDB < MaxSize then begin
               RecordResults;
            end
            else for y := 0 to 1 do begin
               ylo := ymin + y * dy;
               yhi := ymin + succ(y) * dy;
               ThY := BaseY + IntToStr(Y);

               SetFilter;
               if GISdb[DBonTable].MyData.FiltRecsInDB  < MaxSize then begin
                  RecordResults;
               end
               else for z := 0 to 1 do begin
                   zlo := zmin + z * dz;
                   zhi := zmin + succ(z) * dz;
                   ThZ := BaseZ + IntToStr(z);
                   SetFilter;
                   Status(oName);
                   if GISdb[DBonTable].MyData.FiltRecsInDB  < MaxSize then begin
                      RecordResults;
                   end
                   else begin
                      Octree(Xlo,XHi,YLo,YHi,ZLo,ZHi,ThX,ThY,ThZ);
                   end;
               end;
            end;
         end;
     end;

var
   xb,yb,zb : shortstring;
begin
    Status('x range');
    GISdb[DBonTable].FieldRange(x_field,GISdb[DBonTable].dbXMin,GISdb[DBonTable].dbXMax);
    Status('y range');
    GISdb[DBonTable].FieldRange(y_field,GISdb[DBonTable].dbYMin,GISdb[DBonTable].dbYMax);
    Status('z range');
    GISdb[DBonTable].FieldRange(z_field,GISdb[DBonTable].dbZMin,GISdb[DBonTable].dbZMax);
    Status('Add field');
    GISdb[DBonTable].AddFieldToDataBase(ftString,'OCTREE',35,1);
    xb := '';
    yb := '';
    zb := '';
    Octree(GISdb[DBonTable].dbXMin,GISdb[DBonTable].dbXMax,GISdb[DBonTable].dbYMin,GISdb[DBonTable].dbYMax,GISdb[DBonTable].dbZMin,GISdb[DBonTable].dbZMax,xb,yb,zb);
    GISdb[DBonTable].MyData.Filtered := false;
    ShowStatus;
    Status('');
end;


procedure Tdbtablef.ogglerecords1Click(Sender: TObject);
var
   fName : ShortString;
begin
   fName := GISdb[DBonTable].PickField('Field for choice',[ftString]);
   GISdb[DBonTable].SaveFilterStatus(true);
   GISdb[DBonTable].EmpSource.Enabled := false;
   Toggle_DB_Use.VerifyRecordsToUse(GISdb[DBonTable].MyData,fName,'Records to use','USE');
   GISdb[DBonTable].RestoreFilterStatus;
   ShowStatus;
end;


procedure Tdbtablef.Openallclassfiles1Click(Sender: TObject);
{$IfDef ExSat}
begin
{$Else}
var
   fName : PathStr;
   MinV,MaxV : float64;
   WantField : ShortString;
   aClass : integer;
begin
      WantField := 'CLASS';
      GISdb[DBonTable].FieldRange(WantField,MinV,MaxV);
      for aClass := Round(MinV) to round(MaxV) do begin
         GISdb[DBonTable].MyData.ApplyFilter(WantField + '=' + IntToStr(aClass));
         fName := ExtractFilePath(GISdb[DBonTable].dbFullName) + 'class_stats\' + GISdb[DBonTable].MyData.GetFieldByNameAsString('NAME') +  DefaultDBExt;
         if FileExists(fName) then begin
            GISdb[DBonTable].theMapOwner.LoadDataBaseFile(fName);
         end;
      end;
      GISdb[DBonTable].ClearGISFilter;
    Self.BringToFront;
    Wavelengthmeanallclasses1.Enabled := true;
    WavelengthStdDevallclasses1.Enabled := true;
    ClassBanddiscrimination1.Enabled := true;
{$EndIf}
end;


procedure Tdbtablef.OpenDEMIXgraphs1Click(Sender: TObject);
begin
   StartDEMIXgraphs(DBonTable);
end;

procedure Tdbtablef.Outlinecameraview1Click(Sender: TObject);
begin
   GISdb[DBonTable].TheMapOwner.DoFastMapRedraw;
   GISdb[DBonTable].OutlineCurrentViewOnMap;
   RecordDisplay1Click(Sender);
end;


procedure Tdbtablef.Updaterecordnumbers1Click(Sender: TObject);
begin
   GISdb[DBonTable].AddSequentialIndex(SelectedColumn);
end;


procedure Tdbtablef.Updatestatus1Click(Sender: TObject);
begin
   ShowStatus;
end;


procedure Tdbtablef.UnHideColumns;
var
   j : integer;
begin
   if ValidDB(DBonTable) then begin
      for j := 0 to pred(DBGrid1.Columns.Count) do
         if (j <= 100) then DBGrid1.Columns[j].Visible := true;
      AnyHiddenColumns := false;
   end;
   BitBtn13.Enabled := false;
end;

procedure Tdbtablef.HideColumns;
var
   j : integer;
begin
  if ValidDB(DBonTable) then begin
      for j := 0 to pred(DBGrid1.Columns.Count) do
         if (j <= 100) then DBGrid1.Columns[j].Visible := GISdb[DBonTable].dbOpts.VisCols[j];
      AnyHiddenColumns := false;
      for j := 0 to 100 do if (Not GISdb[DBonTable].dbOpts.VisCols[j]) then AnyHiddenColumns := true;
   end;
   if AnyHiddenColumns then BitBtn13.Enabled := true;
end;


procedure Tdbtablef.Hide1Click(Sender: TObject);
var
   j : integer;
begin
   for j := 0 to pred(DBGrid1.Columns.Count) do begin
      if (SelectedColumn = trim(DBGrid1.Columns[j].FieldName)) then begin
         DBGrid1.Columns[j].Visible := false;
         GISdb[DBonTable].dbOpts.VisCols[j] := false;
         exit;
      end;
   end;
end;


procedure Tdbtablef.HideHouseKeepingColumns;
var
   j : integer;
begin
   if ValidDB(DBonTable) then begin
      for j := 0 to pred(DBGrid1.Columns.Count) do begin
         if (j <= 100) then begin
            if (DBGrid1.Columns[j].FieldName = 'COLOR') or (DBGrid1.Columns[j].FieldName = RecNoFName) then begin
               DBGrid1.Columns[j].Visible := false;
               AnyHiddenColumns := true;
            end;
         end;
      end;
   end;
end;


procedure Tdbtablef.Hideunusedfields1Click(Sender: TObject);
var
   j : integer;
   fName : ANSIstring;
begin
   if ValidDB(DBonTable) then begin
      GISdb[DBonTable].EmpSource.Enabled := false;
      for j := 0 to pred(GISdb[DBonTable].MyData.FieldCount) do begin
         if (j <= 100) then begin
            fName := GISdb[DBonTable].MyData.GetFieldName(j);
            if GISdb[DBonTable].MyData.FieldAllBlanks(fName) or GISdb[DBonTable].MyData.FieldAllZeros(fName) then
               GISdb[DBonTable].dbOpts.VisCols[j] := false;
         end;
      end;
   end;
   ShowStatus;
end;


procedure Tdbtablef.SaveHiddenColumns;
begin
   SavedTheHiddenColumns := GISdb[DBonTable].dbOpts.VisCols;
end;

procedure Tdbtablef.RestoreHiddenColumns;
begin
   GISdb[DBonTable].dbOpts.VisCols := SavedTheHiddenColumns;
   GISdb[DBonTable].dbTableF.HideColumns;
   GISdb[DBonTable].dbTableF.ShowStatus;
end;



procedure Tdbtablef.RadiusfromDB1Click(Sender: TObject);
begin
   CreateGrid(cgRadiusDB);
end;

procedure Tdbtablef.Randomizeduplicatepoints1Click(Sender: TObject);
var
   Lat,Long,Dist : float64;
   Results : tStringList;
   I,Found,Moved  : Integer;
begin
   GISdb[DBonTable].MyData.First;
   Results := tStringList.Create;
   Results.Sorted := true;
   Results.Duplicates := dupIgnore;
   GISdb[DBonTable].EmpSource.Enabled := false;
   while not GISdb[DBonTable].MyData.eof do begin
      if GISdb[DBonTable].ValidLatLongFromTable(Lat,Long) then Results.Add(StandardLatLongString(Lat,Long));
      GISdb[DBonTable].MyData.Next;
   end;
   if GISdb[DBonTable].MyData.FiltRecsInDB  = Results.Count then MessageToContinue('No duplicate coordinates')
   else begin
      Dist := 5;
      ReadDefault('Distance to move points (m)',Dist);
      Dist := Dist / 110000;
      Moved := 0;
      for I := 0 to pred(Results.Count) do begin
         GISdb[DBonTable].MyData.First;
         Found := 0;
         while not GISdb[DBonTable].MyData.eof do begin
            if GISdb[DBonTable].ValidLatLongFromTable(Lat,Long) then begin
               if StandardLatLongString(Lat,Long) = Results.Strings[i] then begin
                  Inc(Found);
                  if (Found > 1) then begin
                     inc(Moved);
                     GISdb[DBonTable].MyData.Edit;
                     GISdb[DBonTable].MyData.SetFieldByNameAsFloat(GISdb[DBonTable].LatFieldName,Lat + Random * Dist);
                     GISdb[DBonTable].MyData.SetFieldByNameAsFloat(GISdb[DBonTable].LongFieldName,Long + Random * Dist);
                  end;
               end;
            end;
            GISdb[DBonTable].MyData.Next;
         end;
      end;
     GISdb[DBonTable].RedrawLayerOnMap;
     Results.Free;
     MessageToContinue('Moved records='+ Moved.ToString);
   end;
   ShowStatus;
end;

procedure Tdbtablef.Rangecircles1Click(Sender: TObject);
var
   Lat,Long : float64;
begin
   if GISdb[DBonTable].ValidLatLongFromTable(Lat,Long) then begin
      GISdb[DBonTable].TheMapOwner.Rangecircles1Click(Nil);
      GISdb[DBonTable].TheMapOwner.AddRangeCirclesAtLocation(Lat,Long);
      GISdb[DBonTable].TheMapOwner.BackToWandering;
   end;
end;

procedure Tdbtablef.RankDEMs1Click(Sender: TObject);
begin
   RankDEMS(DBonTable);
end;


procedure Tdbtablef.ranslatefromtable1Click(Sender: TObject);
begin
   Translatefromtable1Click(Nil);
end;



procedure Tdbtablef.Translatefromtable1Click(Sender: TObject);
var
   Before,After : ansistring;
   WantedFieldName : ShortString;
   Changed : integer;
   Table : tMyData;
   tName : PathStr;
   OldText,NewText : ShortString;
begin
   GISdb[DBonTable].MyData.First;
   if (Sender = Nil) then WantedFieldName := SelectedColumn
   else WantedFieldName := GISdb[DBonTable].PickField('Translation',[ftString]);
   if GetFileFromDirectory('file with translations',DefaultDBMask,tName) then begin
      ShowHourglassCursor;
      Table := tMyData.Create(tName);
      OldText := GISdb[DBonTable].PickField('Text to replace',[ftString]);
      NewText := GISdb[DBonTable].PickField('Text to replace with',[ftString]);
      while not Table.eof do begin
         Before := Table.GetFieldByNameAsString(OldText);
         After := Table.GetFieldByNameAsString(NewText);
         if (After <> '') and (Before <> '') then SearchAndReplace(WantedFieldName,Before,After,Changed);
         Table.Next;
      end;
      Table.Destroy;
   end;
   ShowStatus;
end;


procedure Tdbtablef.Treatasregulardatabase1Click(Sender: TObject);
begin
   GISdb[DBonTable].ItsOSMShapeFile := false;
   GISdb[DBonTable].ItsTigerShapeFile := false;
   GISdb[DBonTable].GISProportionalSymbols(dbasDefault);
   //GISdb[DBonTable].RedrawLayerOnMap;
end;

procedure Tdbtablef.Recentermaponrecord1Click(Sender: TObject);
begin
   GISdb[DBonTable].Recentermaponrecord;
end;


procedure Tdbtablef.Reclassifyfocalmechanisms1Click(Sender: TObject);
begin
   GISdb[DBonTable].MyData.First;
   GISdb[DBonTable].EmpSource.Enabled := false;
   ShowHourglassCursor;
   while not GISdb[DBonTable].MyData.eof do begin
      GISdb[DBonTable].MyData.Edit;
      if GISdb[DBonTable].MyData.GetFieldByNameAsInteger('PLUNGE_1') > 50 then GISdb[DBonTable].MyData.SetFieldByNameAsString('MECH','T')
      else if GISdb[DBonTable].MyData.GetFieldByNameAsInteger('PLUNGE_2') > 50 then GISdb[DBonTable].MyData.SetFieldByNameAsString('MECH','S')
      else if GISdb[DBonTable].MyData.GetFieldByNameAsInteger('PLUNGE_3') > 50 then GISdb[DBonTable].MyData.SetFieldByNameAsString('MECH','N')
      else GISdb[DBonTable].MyData.SetFieldByNameAsString('MECH','');
      GISdb[DBonTable].MyData.Next;
   end;
   ShowStatus;
end;

procedure Tdbtablef.Highlightrecordonmap1Click(Sender: TObject);
begin
   Highlightrecs(True);
end;


procedure Tdbtablef.Highlightrecs(JustCurrent : boolean);
var
   Bitmap,Merge : tMyBitmap;
   Transparency : float64;

     procedure ShowOne;
     begin
         if GISdb[DBonTable].LatLongFieldsPresent then  begin
            if GISdb[DBonTable].ValidScreenPositionFromTable(dbxpic,dbypic) then  begin
               ScreenSymbol(Bitmap.Canvas,dbXpic,dbYpic,MDDef.HighlightSymbol);
            end;
         end
         else GISdb[DBonTable].DisplayCurrentRecordOnMap(GISdb[DBonTable].TheMapOwner.MapDraw,Bitmap);
     end;

begin
   if ValidDB(DBonTable) and (GISdb[DBonTable].theMapOwner <> Nil) then begin
      {$IfDef RecordCurrentRecord} WriteLineToDebugFile('Tdbtablef.Highlightrecordonmap1Click. RecNo=' + IntToStr(GISDataBase[DBonTable].MyData.RecNo)); {$EndIf}

      if HLCheckBox.Checked then  begin
         HighlightFan(claRed);
         exit;
      end;

     with GISdb[DBonTable] do begin
        {$IfDef RecordDataBase} WriteLineToDebugFile('Tdbtablef.Highlightrecordonmap1'); {$EndIf}
        if (BaseMapBitmap = Nil) then CopyImageToBitmap(theMapOwner.Image1,BaseMapBitmap)
        else theMapOwner.Image1.Picture.Graphic := BaseMapBitmap;

        CloneImageToBitmap(theMapOwner.Image1,Bitmap);

        Bitmap.Canvas.Pen.Color := ConvertPlatformColorToTColor(MDDef.HighlightColor);
        Bitmap.Canvas.Pen.Width := MDDef.HighlightLineWidth;
        Bitmap.Canvas.Brush.Color := ConvertPlatformColorToTColor(MDDef.HighlightColor);
        Bitmap.Canvas.Brush.Style := bsSolid;
        if JustCurrent then ShowOne
        else begin
           GISdb[DBonTable].MyData.First;
           while not GISdb[DBonTable].MyData.eof do begin
              ShowOne;
              GISdb[DBonTable].MyData.Next;
           end;
        end;

        if AreaShapeFile(ShapeFileType) then Transparency := 0.5
        else Transparency := 1;
        Merge := BlendBitmaps(BaseMapBitmap,Bitmap,Transparency);
        theMapOwner.Image1.Picture.Graphic := Merge;
        Merge.Free;
        Bitmap.Free;
     end;
   end;
end;


procedure Tdbtablef.hisDBall3groupings1Click(Sender: TObject);
begin
   HistogramPointCloudAndGlobalDEMs(DBonTable,'Landcover');
   HistogramPointCloudAndGlobalDEMs(DBonTable,'Canopy');
   HistogramPointCloudAndGlobalDEMs(DBonTable,'Slope');
end;

procedure Tdbtablef.hisDBall3groups1Click(Sender: TObject);
begin
   DirtAndAirShots(DBonTable,'Landcover');
   DirtAndAirShots(DBonTable,'Canopy');
   DirtAndAirShots(DBonTable,'Slope');
end;

procedure Tdbtablef.hisDBbycanopyheight1Click(Sender: TObject);
begin
   HistogramPointCloudAndGlobalDEMs(DBonTable,'Canopy');
end;

procedure Tdbtablef.hisDBbylandcover1Click(Sender: TObject);
begin
   HistogramPointCloudAndGlobalDEMs(DBonTable,'Landcover');
end;

procedure Tdbtablef.hisDBbylandcovercategory1Click(Sender: TObject);
begin
   CloudSummaryGlobalDEMs(DBonTable);
end;

procedure Tdbtablef.hisDBbylandcovercats1Click(Sender: TObject);
begin
   DirtAndAirShots(DBonTable,'Landcover');
end;

procedure Tdbtablef.hisDBByslope1Click(Sender: TObject);
begin
   HistogramPointCloudAndGlobalDEMs(DBonTable,'Slope');
end;

procedure Tdbtablef.hisDBcanopyheights1Click(Sender: TObject);
begin
   DirtAndAirShots(DBonTable,'Canopy');
end;

procedure Tdbtablef.hisDBslopecats1Click(Sender: TObject);
begin
   DirtAndAirShots(DBonTable,'Slope');
end;


procedure Tdbtablef.Thindatabase1Click(Sender: TObject);
var
   ThinFactor,i,j,rc : integer;

   procedure PostIt(ch : char);
   begin
      GISdb[DBonTable].MyData.Edit;
      GISdb[DBonTable].MyData.SetFieldByNameAsString('USE',ch);
      GISdb[DBonTable].MyData.Next;
   end;

begin
   ThinFactor := 2;
   ReadDefault('Thin factor',ThinFactor);
   if (ThinFactor > 1) then {with GISdb[DBonTable] do} begin
      GISdb[DBonTable].AddFieldToDataBase(ftString,'USE',1,0);
      GISdb[DBonTable].EmpSource.Enabled := false;
      GISdb[DBonTable].MyData.First;
      j := 0;
      rc := GISdb[DBonTable].MyData.FiltRecsInDB;
      StartProgress('Thin');
      repeat
         inc(j);
         if (j mod 500 = 0) then UpdateProgressBar(j/rc);
         PostIt('Y');
         for i  := 1 to pred(ThinFactor) do if not GISdb[DBonTable].MyData.EOf then PostIt('N');
      until GISdb[DBonTable].MyData.eof;
      ShowStatus;
   end;
end;


procedure Tdbtablef.Thindatabase2Click(Sender: TObject);
begin
   GISdb[DBonTable].SaveCurrentDBaseSubset('',-1);
end;


procedure Tdbtablef.Colorpoint1Click(Sender: TObject);
begin
   ColorDialog1.Color := GISdb[DBonTable].MyData.TColorFromTable;
   if ColorDialog1.Execute then begin
      GISdb[DBonTable].MyData.Edit;
      GISdb[DBonTable].MyData.SetColorFromTColor(ColorDialog1.Color);
      GISdb[DBonTable].MyData.Post;
   end;
end;

procedure Tdbtablef.Editpointlocation1Click(Sender: TObject);
begin
   Graphicallymovepoints1Click(Sender);
end;

procedure Tdbtablef.TimeFieldAddSnakeyesClick(Sender: TObject);
var
   TStr : shortString;
begin
   GISdb[DBonTable].AddFieldToDataBase(ftString,'TIME_STR',8,0);
   GISdb[DBonTable].EmpSource.Enabled := false;
   GISdb[DBonTable].MyData.First;
   while not GISdb[DBonTable].MyData.EOF do begin
      GISdb[DBonTable].MyData.Edit;
      TStr := GISdb[DBonTable].MyData.GetFieldByNameAsString('TIME_UTC');
      while (Length(TStr) < 9) do TStr := '0' + TStr;
      GISdb[DBonTable].MyData.SetFieldByNameAsString('TIME_STR',Copy(TStr,1,2) + ':' + Copy(TStr,3,2) + ':' + Copy(TStr,5,2));
      GISdb[DBonTable].MyData.Next;
   end;
   ShowStatus;
end;


procedure Tdbtablef.TimefieldHHMMSStohours1Click(Sender: TObject);
var
   TStr : ShortString;
   Hours : float64;
   i,rc : integer;
begin
   {$If Defined(RecordEditsDone) or Defined(RecordEditDBProblems)} WriteLineToDebugFile('Tdbtablef.TimefieldHHMMSStohours1Click in ' + GISdb[DBonTable].dbName + ' Time field to decimal hours'); {$EndIf}
   //with GISdb[DBonTable] do begin
      if GISdb[DBonTable].AddFieldToDataBase(ftFloat,'DEC_HOURS',12,6) then begin
         {$IfDef RecordEditDB} WriteLineToDebugFile('dec_hours added'); {$EndIf}
      end;

      GISdb[DBonTable].ClearGISFilter;
      GISdb[DBonTable].EmpSource.Enabled := false;
      ShowHourglassCursor;
      GISdb[DBonTable].MyData.First;
      rc := ProgressIncrement(GISdb[DBonTable].MyData.FiltRecsInDB);
      i := 0;
      while not GISdb[DBonTable].MyData.EOF do begin
         if (i mod rc = 0) then UpdateProgressBar(i/GISdb[DBonTable].MyData.FiltRecsInDB);
         inc(i);
         GISdb[DBonTable].MyData.Edit;
         if (Sender = TimefieldHHMMSStohours1) then begin
            TStr := UpperCase(GISdb[DBonTable].MyData.GetFieldByNameAsString('TIME'));
            if ANSIContainsText(TStr,':') then begin
               if (TStr[2] = ':') then Tstr := '0' + TStr;
               Hours := 1.0 * StrToInt(Copy(TStr,1,2)) + StrToInt(Copy(TStr,4,2)) / 60 + StrToInt(Copy(TStr,7,2)) / 3600;
               if (Length(TStr) > 8) and ANSIContainsText(TStr,'PM') then Hours := Hours + 12;
            end
            else begin
               TStr := GISdb[DBonTable].MyData.GetFieldByNameAsString('TIME');
               while (length(TStr) < 6) do Tstr := '0' + TStr;
               Hours := 1.0 * StrToInt(Copy(TStr,1,2)) + StrToInt(Copy(TStr,3,2)) / 60 + StrToInt(Copy(TStr,5,2)) / 3600;
            end;
            GISdb[DBonTable].MyData.SetFieldByNameAsFloat('DEC_HOURS',Hours);
         end
         else begin
            GISdb[DBonTable].MyData.SetFieldByNameAsFloat('DEC_HOURS',GISdb[DBonTable].MyData.GetFieldByNameAsInteger('HOUR') + GISdb[DBonTable].MyData.GetFieldByNameAsInteger('MINUTE') /60 + GISdb[DBonTable].MyData.GetFieldByNameAsInteger('SECOND') /3600);
         end;
         GISdb[DBonTable].MyData.Next;
      end;
      ShowStatus;
   //end;
   {$If Defined(RecordEditsDone) or Defined(RecordEditDBProblems)} WriteLineToDebugFile('Tdbtablef.TimefieldHHMMSStohours1Click out'); {$EndIf}
end;


procedure Tdbtablef.Timefieldshourminsectodechours1Click(Sender: TObject);
begin
   TimefieldHHMMSStohours1Click(Sender);
end;


procedure Tdbtablef.Timefieldstodecdays1Click(Sender: TObject);
var
   t : float64;
   MinField,SecField : ShortString;
begin
  //with GISdb[DBonTable] do begin
      if GISdb[DBonTable].MyData.FieldExists('DAY') and GISdb[DBonTable].MyData.FieldExists('HOUR') then begin
         GISdb[DBonTable].AddFieldToDataBase(ftFloat,'DEC_DAYS',12,6);
         if GISdb[DBonTable].MyData.FieldExists('MINUTE') then MinField := 'MINUTE'
         else if GISdb[DBonTable].MyData.FieldExists('MIN') then MinField := 'MIN'
         else MinField := '';
         if GISdb[DBonTable].MyData.FieldExists('SECOND') then MinField := 'SECOND'
         else if GISdb[DBonTable].MyData.FieldExists('SEC') then MinField := 'SEC'
         else SecField := '';

         GISdb[DBonTable].EmpSource.Enabled := false;
         GISdb[DBonTable].MyData.First;
         while not GISdb[DBonTable].MyData.EOF do begin
            GISdb[DBonTable].MyData.Edit;
            t := GISdb[DBonTable].MyData.GetFieldByNameAsInteger('DAY') + GISdb[DBonTable].MyData.GetFieldByNameAsInteger('HOUR')/24;
            if (MinField <> '') then t := t + GISdb[DBonTable].MyData.GetFieldByNameAsInteger(MinField) /60/24;
            if (SecField <> '') then t := t + GISdb[DBonTable].MyData.GetFieldByNameAsInteger(SecField)/3600/24;
            GISdb[DBonTable].MyData.SetFieldByNameAsFloat('DEC_DAYS',t);
            GISdb[DBonTable].MyData.Next;
         end;
         ShowStatus;
      end
      else begin
         MessageToContinue('Required "HOUR" and "DAY" fields');
      end;
   //end;
end;


procedure Tdbtablef.Wavelengthmeanallclasses1Click(Sender: TObject);
begin
   GISdb[DBonTable].dbOpts.XField := 'WAVELENGTH';
   GISdb[DBonTable].dbOpts.YField := 'MEAN';
   N2Dgraphallopendatabases1Click(Sender);
end;

procedure Tdbtablef.WavelengthStdDevallclasses1Click(Sender: TObject);
begin
   GISdb[DBonTable].dbOpts.XField := 'WAVELENGTH';
   GISdb[DBonTable].dbOpts.YField := 'STD_DEV';
   N2Dgraphallopendatabases1Click(Sender);
end;

procedure Tdbtablef.Winlosstie1Click(Sender: TObject);
begin
   MessageToContinue('disabled Tdbtablef.Winlosstie1Click');
   //TileCharateristicsWhiskerPlotsByCluster(DBonTable,false,Nil,true,'COP_ALOS');
end;

procedure Tdbtablef.Winpercentagebycriterion1Click(Sender: TObject);
begin
   MakeWinsDB(DBonTable,'CRITERION');
end;

procedure Tdbtablef.Winpercentagesbyarea1Click(Sender: TObject);
begin
   MakeWinsDB(DBonTable,'AREA');
end;

procedure Tdbtablef.Wins1Click(Sender: TObject);
begin
   WinsAndTies(DBonTable);
end;

procedure Tdbtablef.woorthreefieldRGB1Click(Sender: TObject);
begin
   {$IfDef ExAdvancedGIS}
   {$Else}
      GISdb[DBonTable].GISProportionalSymbols(dbasMultiFieldRGB);
   {$EndIf}
end;

procedure Tdbtablef.WWW1Click(Sender: TObject);
begin
   DisplayWWWFromDataBase(GISdb[DBonTable].MyData);
end;


procedure Tdbtablef.XML1Click(Sender: TObject);
begin
   GISdb[DBonTable].ExportToXML('');
   GISdb[DBonTable].dbIsUnsaved := false;
   ShowStatus;
end;


procedure Tdbtablef.XYZ1Click(Sender: TObject);
var
   i,Num : integer;
begin
   Num := 4;
   ReadDefault('Number of XYZ sets of coordinates',Num);
   for i := 1 to Num do begin
       GISdb[DBonTable].AddFieldToDataBase(ftFloat,'X'+IntToStr(i),12,3);
       GISdb[DBonTable].AddFieldToDataBase(ftFloat,'Y'+IntToStr(i),12,3);
       GISdb[DBonTable].AddFieldToDataBase(ftFloat,'Z'+IntToStr(i),12,3);
   end;
end;


procedure Tdbtablef.YYYYMMDD1Click(Sender: TObject);
begin
   MMDDYYYY1Click(Sender);
end;

procedure Tdbtablef.ZipATonefield1Click(Sender: TObject);
begin
   GISdb[DBonTable].GISProportionalSymbols(dbasZipatoneField);
end;

procedure Tdbtablef.Zipshapefile1Click(Sender: TObject);
begin
   ZipShapefile(DBontable,(Sender = Includedebuglog1),true);
end;


procedure Tdbtablef.Zoommaptorecord1Click(Sender: TObject);
const
   Bitty = 0.05;
var
   Bit : float64;
   bb : sfBoundBox;
begin
   {$IfDef RecordMapSizing} WriteLineToDebugFile('Tdbtablef.Zoommaptorecord1Click in'); {$EndIf}
   if GISdb[DBonTable].ItsAPointDB then begin
      Recentermaponrecord1Click(Sender);
   end
   else begin
      bb := GISdb[DBonTable].MyData.GetRecordBoundingBox;
      Bit := MaxFloat(bb.xmax - bb.xmin,bb.ymax - bb.ymin) * Bitty;
      {$IfDef RecordMapSizing} WriteLineToDebugFile('Tdbtablef.Zoommaptorecord1Click, bit=' + RealToString(Bit,-18,-6)); {$EndIf}
      GISdb[DBonTable].theMapOwner.MapDraw.MaximizeLatLongMapCoverage(bb.ymin - Bit,bb.xmin - Bit, bb.ymax+Bit,bb.xmax+Bit);
      GISdb[DBonTable].theMapOwner.DoCompleteMapRedraw;
   end;
   {$IfDef RecordMapSizing} WriteLineToDebugFile('Tdbtablef.Zoommaptorecord1Click out'); {$EndIf}
end;



procedure Tdbtablef.ZoomtoDBcoverage1Click(Sender: TObject);
begin
   GISdb[DBonTable].ZoomToDBCoverageOnMap;
end;


procedure Tdbtablef.Zstatistics1Click(Sender: TObject);
begin
  GISdb[DBonTable].AddGeometry(agZStats);
end;


procedure Tdbtablef.Zvalues1Click(Sender: TObject);
begin
   GISdb[DBonTable].dbOpts.DBAutoShow := dbasZValues;
   GISdb[DBonTable].dbOpts.ZColorMin := GISdb[DBonTable].Ashapefile.MainFileHeader.BoundBoxZMin;
   GISdb[DBonTable].dbOpts.ZColorMax := GISdb[DBonTable].Ashapefile.MainFileHeader.BoundBoxZMax;
   ReadDefault('Minimum z',GISdb[DBonTable].dbOpts.ZColorMin);
   ReadDefault('Maximum z',GISdb[DBonTable].dbOpts.ZColorMax);
   GISdb[DBonTable].RedrawLayerOnMap;
   ShowStatus;
end;


procedure Tdbtablef.DBGrid1CellClick(Column: TColumn);
var
   ch : AnsiString;
   fName : PathStr;

   procedure ToggleField(fName : ShortString);
   begin
       GISdb[DBonTable].MyData.Edit;
       ch := GISdb[DBonTable].MyData.GetFieldByNameAsString(fName);
       if ch = 'Y' then ch := 'N'
       else ch := 'Y';
       GISdb[DBonTable].MyData.SetFieldByNameAsString(fName,ch);
       GISdb[DBonTable].MyData.Post;
   end;

begin
   {$IfDef RecordDBGrid1DrawColumnCell} WriteLineToDebugFile('Tdbtablef.DBGrid1CellClick'); {$EndIf}
   if (Panel7.Visible and (RadioGroup2.ItemIndex <> 0)) then begin
       case RadioGroup2.ItemIndex of
          1 : EditPointSymbol1Click(EditPointSymbol1);
          2 : begin
                 fName := GISdb[DBonTable].MyData.GetFieldByNameAsString('NAME');
                 Petmar.GetString('Label name',fName,false,ReasonableTextChars);
                 GISdb[DBonTable].MyData.Edit;
                 GISdb[DBonTable].MyData.SetFieldByNameAsString('NAME',fName);
                 GISdb[DBonTable].MyData.Post;
              end;
          3 : begin
                 fName := GISdb[DBonTable].MyData.GetFieldByNameAsString('FILTER');
                 Petmar.GetString('Filter',fName,false,ReasonableTextChars);
                 GISdb[DBonTable].MyData.Edit;
                 GISdb[DBonTable].MyData.SetFieldByNameAsString('FILTER',fName);
                 GISdb[DBonTable].MyData.Post;
              end;
          4 : begin
                 if (Column.FieldName = 'USE') then ToggleField('USE');
                 if (Column.FieldName = 'PLOT') then ToggleField('PLOT');
              end;
       end;
   end
   else if VATEdit or EditSymbologyOnly then begin
        if (Column.FieldName = 'USE') then ToggleField('USE');
        if (Column.FieldName = 'PLOT') then ToggleField('PLOT');
        if (Column.FieldName = 'GRAY') then ToggleField('GRAY');
        if (Column.FieldName = 'NAME') or (Column.FieldName = 'CLASS_NAME') or (Column.FieldName = 'CLASS') or (Column.FieldName = 'COLOR') or (Column.FieldName = 'PALETTE') or (Column.FieldName = 'DEMIX_TILE') then begin
           Colorpoint1Click(Nil);
        end;
  end;
end;


procedure Tdbtablef.DBGrid1DblClick(Sender: TObject);
var
   ThisShapeType : integer;
   fName : ShortString;
begin
   {$IfDef RecordDBGrid1DrawColumnCell} WriteLineToDebugFile('Tdbtablef.DBGrid1DblClick'); {$EndIf}
   with GISdb[DBonTable] do begin
       if (Panel7.Visible and (RadioGroup2.ItemIndex <> 0)) then begin
          case RadioGroup2.ItemIndex of
             1 : EditPointSymbol1Click(EditPointSymbol1);
             2 : begin
                    fName := GISdb[DBonTable].MyData.GetFieldByNameAsString('NAME');
                    Petmar.GetString('Label name',fName,false,ReasonableTextChars);
                    GISdb[DBonTable].MyData.Edit;
                    GISdb[DBonTable].MyData.SetFieldByNameAsString('NAME',fName);
                    GISdb[DBonTable].MyData.Post;
                 end;
             3 : begin
                    fName := GISdb[DBonTable].MyData.GetFieldByNameAsString('FILTER');
                    Petmar.GetString('Filter',fName,false,ReasonableTextChars);
                    GISdb[DBonTable].MyData.Edit;
                    GISdb[DBonTable].MyData.SetFieldByNameAsString('FILTER',fName);
                    GISdb[DBonTable].MyData.Post;
                 end;
          end;
          exit;
       end;

      Highlightrecordonmap1Click(Sender);

      {$IfDef RecordDataBaseImage} if ImagePresent then WriteLineToDebugFile('Image name: ' + GetFieldByNameAsString('IMAGE')) else WriteLineToDebugFile('Images not present'); {$EndIf}
      RecordDisplay1.Visible := Sender = DBGrid1;
      Satelliteaddreflectance1.Visible := (TheMapOwner <> Nil) and ValidSatImage(TheMapOwner.MapDraw.SATonMap);

      ThisShapeType := 0;
      if ItsAGroupingFile then ThisShapeType := GISdb[DBonTable].MyData.GetFieldByNameAsInteger('SHAPE_TYPE');

      Editrecord1.Visible := CheckBox1.Checked;
      EditPointSymbol1.Visible := ItsFanFile or ((ThisShapeType in [0,1,11]) and GISdb[DBonTable].PointSymbolFieldsPresent);
      Editlineweightandcolor1.Visible := ((ThisShapeType in [0,3,13]) and GISdb[DBonTable].LineColorPresent);
      Editfillpattern1.Visible :=  (ThisShapeType in [0,5,15]) and AreaFillPresent;
      Editpointlocation1.Visible := (LatLongFieldsPresent) or ItsFanFile{ or ItsSymbolFile};
      Monthlywinds1.Visible := (DBonTable = WindsDB) or StrUtils.AnsiContainsText(UpperCase(DBName),'WIND') ;

      Map1.Visible := (TheMapOwner <> Nil);
      RecenterMapOnRecord1.Visible := (theMapOwner <> Nil);
      Highlightrecordonmap1.Visible := (theMapOwner <> Nil);
      Horizonblocking1.Visible := (theMapOwner <> Nil) and ValidDEM(TheMapOwner.MapDraw.DEMonMap);
      Movie1.Visible := GISdb[DBonTable].MyData.FieldExists('MOVIE') and (MyData.GetFieldByNameAsString('MOVIE') <> '');
      Movie2.Visible := Movie1.Visible;
      Deleterecord1.Visible := (CheckBox1.Checked) or ItsFanFile;

      StationTimeSeries1.Visible := TimeSeriesPresent;
      Stratigraphiccolumn1.Visible := StratColPresent;
      PlotXYFile1.Visible := GISdb[DBonTable].MyData.FieldExists('XY_FILE');
      Datastatistics1.Visible := StationTimeSeries1.Visible or Stratigraphiccolumn1.Visible or PlotXYFile1.Visible;
      Create1.Visible := (MDDef.ProgramOption = ExpertProgram) and (TotalNumOpenDatabase > 1);

      EditRecord1.Visible := CheckBox1.Checked;
      ColorPoint1.Visible := (ColorPresent or RGBColorPresent);
      EditFont1.Visible := FontFieldExists;
      KeyboardNewPointLocation1.Visible := (LatLongFieldsPresent) or ItsFanFile;

      AssociateImage1.Visible := CheckBox1.Checked and ImagePresent;
      WWW1.Visible := WWWPresent and (MyData.GetFieldByNameAsString(WWWFieldNames[1]) <> '');
      PerspectiveView1.Visible := CameraOrientationExists and ItsaPointDB and (TheMapOwner <> Nil) and ValidDEM(TheMapOwner.MapDraw.DEMonMap);
      PanoramaView1.Visible := ItsAPointDB and (TheMapOwner <> Nil) and (TheMapOwner.MapDraw.DEMonMap <> 0);
      PasteCoordinatesFromClipboard1.Visible := ItsAPointDB and ClipBoard_Coords;

      LineLength2.Visible := ItsAShapeFile and LineShapeFile(ShapeFileType);
      CalculateArea1.Visible := ItsAShapeFile and AreaShapeFile(ShapeFileType);
      Pointinrecord1.Visible := ItsAShapeFile and AreaShapeFile(ShapeFileType);
      Calculateperimeter1.Visible := ItsAShapeFile and AreaShapeFile(ShapeFileType);
      CalculateVolume1.Visible := ItsAShapeFile and AreaShapeFile(ShapeFileType) and ((theMapOwner <> Nil) and ValidDEM(TheMapOwner.MapDraw.DEMonMap));

      RecordCoordinates1.Visible := ItsAPointDB or LatLongFieldsPresent or XYZFile or ItsAShapeFile;
      Recordboundingbox1.Visible := (ItsAShapeFile and LineOrAreaShapeFile(ShapeFileType));
      Recordscreencoordinates1.Visible := ItsAShapeFile and LineOrAreaShapeFile(ShapeFileType);
      RecordEdit1.Visible := CheckBox1.Checked;
      EditFileName1.Visible := CheckBox1.Checked and GISdb[DBonTable].MyData.FieldExists('FILENAME');
      FanProperties1.Visible := ItsFanFile;
      Requiredantennaheight1.Visible := ItsFanFile;
      RecolorFan1.Visible := ItsFanFile;
      Forceredrawofmap1.Visible := ItsFanFile;
      Highlightfan1.Visible := ItsFanFile;
      ShowNeighbors1.Visible := (NeighborTable <> Nil);
      HighLightRecordOnMap1.Visible := ItsAPointDB;
      Loadfrommaplibrary1.Visible := ItsAPointDB;
      N3Dshapefileprofile1.Visible := ShapeFile3D(GISdb[DBonTable].ShapeFileType);
      Pastecoordinatesfromclipboard1.Visible := ItsAPointDB and CheckBox1.Checked;
      LOSterrainprofile1.Visible := GISdb[DBonTable].MyData.FieldExists('LONG') and GISdb[DBonTable].MyData.FieldExists('LONG2');
      DEMTerrainprofile1.Visible := ShapeFile3D(ShapeFileType) or (LineOrAreaShapeFile(ShapeFileType) and ((TheMapOwner <> Nil) and ValidDEM(TheMapOwner.MapDraw.DEMonMap)));
      Normalizedbasinprofile1.Visible := (ShapeFileType in [3,13,23]);
      Exportlinetopointdatabase1.Visible := (ShapeFileType in [1,3,13,23]);
      CompareshapefileandDEMprofiles1.Visible := ShapeFile3D(ShapeFileType) and ((TheMapOwner <> Nil) and (TheMapOwner.MapDraw.DEMonMap <> 0));
      LidarWaveform1.Visible := GISdb[DBonTable].MyData.FieldExists('RH100');
      {$IfDef ExSidescan}
         //Loadsidescanimage1.Visible := false;
      {$Else}
         //Loadsidescanimage1.Visible := SideScanIndex;
      {$EndIf}

      RangeCircles1.Visible:= ItsaPointDB;
      EditIcon1.Visible := IconPresent and CheckBox1.Checked;
      Insertnewrecordatdistancebearing1.Visible := ItsaPointDB and CheckBox1.Checked;

      Outlinecameraview1.Visible := PhotoLocationsPresent;
      DEMTerrainblowups1.Visible := NumDEMDataSetsOPen > 0;
      CreateDEM1.Visible := (LineOrAreaShapeFile(ShapeFileType));
      Markholes1.Visible := (AreaShapeFile(ShapeFileType));
      MaskDEMgrid1.Visible := CheckBox1.Checked and ((TheMapOwner <> Nil) and (TheMapOwner.MapDraw.DEMonMap <> 0));
      AddXYbox1.Visible := GISdb[DBonTable].MyData.FieldExists('X1') and GISdb[DBonTable].MyData.FieldExists('Y2') and GISdb[DBonTable].MyData.FieldExists('X3') and GISdb[DBonTable].MyData.FieldExists('Y4');
      LidarWaveform1.Visible := GISdb[DBonTable].MyData.FieldExists('RH99');
      DEMIX2.Visible := GISdb[DBonTable].MyData.FieldExists('DEMIX_TILE');
      GridCellPopupMenu6.PopUp(Mouse.CursorPos.X,Mouse.CursorPos.Y);
   end;
end;


function PictureInDBField(FieldName : ShortString) : boolean;
begin
  Result := (FieldName = 'NAME') or (FieldName = 'DEM_NAME') or (FieldName = 'EVT_NAME') or (FieldName = 'NAME00') or (FieldName = 'NAME10') or
       (FieldName = 'F_NAME') or (FieldName = 'LONG_NAME') or (FieldName = 'PALETTE') or (FieldName = 'SENSORS') or
       (FieldName = 'PLACE') or (FieldName = 'CLASS_NAME') or (FieldName = 'CLASS') or (FieldName = 'CLUSTER');
end;


procedure Tdbtablef.DBGrid1DrawColumnCell(Sender: TObject; const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
var
   bitmap : tMyBitmap;
   fixRect : TRect;
   bmpWidth : integer;
   Sym : tDrawingSymbol;
   LineWidth : integer;
   SymSize : byte;
   WidePalette : boolean;
   LineColor,
   SymColor : tPlatformColor;
   fName : PathStr;
   ThisShapeType : integer;

   procedure SetUpRect;
   begin
      bmpWidth := (Rect.Bottom - Rect.Top);
      if WidePalette then bmpWidth := 5 * bmpWidth;
      fixRect.Right := Rect.Left + bmpWidth;
      CreateBitmap(bitmap,bmpWidth,bmpWidth);
   end;

begin
   {$IfDef RecordDBGrid1DrawColumnCell} WriteLineToDebugFile('Tdbtablef.DBGrid1DrawColumnCell'); {$Endif}
   if (DBonTable = 0) then exit;
   fixRect := Rect;

   if GISdb[DBonTable].ImageForTable and PictureInDBField(Column.FieldName) then begin
        if GISdb[DBonTable].MyData.FieldExists('USE') and (GISdb[DBonTable].MyData.GetFieldByNameAsString('USE') = 'N') then begin
        end
        else begin
           try
             WidePalette := (Column.FieldName = 'PALETTE') and (not GISdb[DBonTable].ColorPresent) and (not GISdb[DBonTable].RGBColorPresent);
             SetUpRect;

             if GISdb[DBonTable].ItsAGroupingFile then ThisShapeType := GISdb[DBonTable].MyData.GetFieldByNameAsInteger('SHAPE_TYPE')
             else ThisShapeType := 0;
             if GISdb[DBonTable].IconPresent and ExpandIconName(GISdb[DBonTable].MyData,GISdb[DBonTable].dbOpts.IconField,fName) and FileExists(fName) then begin
                Bitmap := PetImage.LoadBitmapFromFile(fName);
             end
             else begin
               if WidePalette then begin
                  GISdb[DBonTable].dbOpts.DBColorPaletteName := GISdb[DBonTable].MyData.GetFieldByNameAsString('PALETTE');
                  DefineColorTableValues(GISdb[DBonTable].dbOpts.DBColorPaletteName,1,100,GISdb[DBonTable].ColorDefTable,GISdb[DBonTable].dbOpts.ReverseColorTable);
                  Bitmap := MakeColorScaleBitmap(200,24,GISdb[DBonTable].dbOpts.DBColorScheme,GISdb[DBonTable].ColorDefTable);
               end
               else if (ThisShapeType in [0,1,11]) and GISdb[DBonTable].PointSymbolFieldsPresent then begin
                  GISdb[DBonTable].MyData.DefinePointSymbol(Sym,SymSize,SymColor);
                  ScreenSymbol(Bitmap.Canvas,bmpwidth div 2, bmpwidth div 2,Sym,SymSize,SymColor);
                end
                else if (ThisShapeType in [0,3,13]) and GISdb[DBonTable].LineColorPresent then begin
                  GISdb[DBonTable].MyData.GetLineColorAndWidth(LineColor,LineWidth);
                  ColorLineWidthBitmap(Bitmap,LineColor,LineWidth);
                end
                else if (ThisShapeType in [0,5,15]) and GISdb[DBonTable].AreaFillPresent then begin
                   GISdb[DBonTable].MyData.GetLineColorAndWidth(LineColor,LineWidth);
                   Bitmap.Canvas.Pen.Color := ConvertPlatformColorToTColor(LineColor);
                   Bitmap.Canvas.Pen.Width := LineWidth;
                   Bitmap.Canvas.Brush.Color := GISdb[DBonTable].MyData.GetFieldByNameAsInteger('FILL_COLOR');
                   Bitmap.Canvas.Brush.Style := tBrushStyle(GISdb[DBonTable].MyData.GetFieldByNameAsInteger('FILL_PAT'));
                   Bitmap.Canvas.Rectangle(0,0,Pred(bmpWidth),pred(BMPWidth));
                end
                else begin
                   Bitmap.Canvas.Pen.Color := GISdb[DBonTable].MyData.TColorFromTable;
                   Bitmap.Canvas.Brush.Color := Bitmap.Canvas.Pen.Color;
                   Bitmap.Canvas.Brush.Style := bsSolid;
                   Bitmap.Canvas.Pen.Width := 1;
                   Bitmap.Canvas.Rectangle(0,0,Pred(bmpWidth),pred(BMPWidth));
                 end;
            end;
            DBGrid1.Canvas.StretchDraw(fixRect,bitmap);
            {$IfDef RecordDBGrid1DrawColumnCell} WriteLineToDebugFile('Drew image'); {$Endif}
          finally
            bitmap.Free;
          end;
       fixRect := Rect;
       fixRect.Left := fixRect.Left + bmpWidth;
     end;
   end;
   DBGrid1.DefaultDrawColumnCell(fixRect,DataCol,Column,State);
end;


procedure Tdbtablef.DBGrid1TitleClick(Column: TColumn);
begin
   SelectedColumn := Column.FieldName;
   SelectedColumnType := Column.Field.DataType;
   ColumnOps;
end;


procedure Tdbtablef.ColumnOps;
var
   ft : tFieldType;
   NumericField : boolean;
begin
   ft := SelectedColumnType;
   NumericField := ftIsNumeric(ft);
   Fieldstatistics1.Visible := NumericField;
   SortField1.Visible := NumericField;
   Fieldpercentiles1.Visible := NumericField;
   Changefieldtype1.Visible := ftIsJoinable(ft) and CheckBox1.Checked;
   Maskfieldbystring1.Visible := ft in [ftString];
   FillField1.Visible := CheckBox1.Checked;
   RenameField1.Visible := CheckBox1.Checked;
   Sum1.Visible := Fieldstatistics1.Visible;
   Historgram1.Visible := Fieldstatistics1.Visible;
   Listuniquevalues2.Visible := true;
   Searchandreplace1.Visible := CheckBox1.Checked and (ft in [ftString,ftInteger,ftSmallInt,ftFloat,ftLargeInt]);
   Historgram1.Visible := NumericField;
   Limitdecimalplaces1.Visible := CheckBox1.Checked and (ft in [ftFloat]);
   Stringmanipulation1.Visible := CheckBox1.Checked and (ft in [ftString]);
   Colorallrecords2.Visible := ((SelectedColumn = 'LINE_COLOR') or (SelectedColumn = 'COLOR')) and CheckBox1.Checked;
   Colorallrecordsterrainscale1.Visible := (SelectedColumn = 'COLOR') and CheckBox1.Checked;
   Colorallrecordsspectrumscale1.Visible := (SelectedColumn = 'COLOR') and CheckBox1.Checked;
   ColorallrecordsRainbowScale1.Visible := (SelectedColumn = 'COLOR') and CheckBox1.Checked;
   Rosediagram3.Visible := (Copy(SelectedColumn,1,3) = 'DIR') or (SelectedColumn = 'FABRIC_DIR') or (SelectedColumn = 'AZIMUTH') or (SelectedColumn = 'HEADING');
   KMLexportfilteredbyvalues1.Visible := NumericField;
   ClusterLegend1.Visible := (SelectedColumn = 'CLUSTER');
   Updaterecordnumbers1.Visible := (SelectedColumn = RecNoFName);
   FlipBinNames1.Visible  := (SelectedColumn = 'BIN_NAME');
   Plotwithcolorsfromthisfield1.Visible := NumericField;
   EditField1.Visible := CheckBox1.Checked;
   FieldTitlePopupMenu7.PopUp(Mouse.CursorPos.X,Mouse.CursorPos.Y);
end;


procedure Tdbtablef.BitBtn1Click(Sender: TObject);
begin
   {$IfDef RecordDataBase} WriteLineToDebugFile('DB symbol selected'); {$EndIf}
   if GISdb[DBonTable].ItsTigerShapeFile then GISdb[DBonTable].TheMapOwner.ModifyTIGERdisplay1Click(nil)
   else GISdb[DBonTable].GISProportionalSymbols(GISdb[DBonTable].dbOpts.dbAutoShow);
end;


procedure Tdbtablef.BitBtn20Click(Sender: TObject);
begin
   if AnswerIsYes('Confirm clear fans') then begin
      GISdb[DBonTable].FillFieldWithValue('IMAGE','');
      ShowStatus;
   end;
end;

procedure Tdbtablef.BitBtn21Click(Sender: TObject);
begin
   Petmar.QueryColor(MDDef.FanColor);
   RecolorBitmap(GISdb[DBonTable].TheMapOwner.MapDraw.AllFansCoverageFName,MDDef.FanColor);
   GISdb[DBonTable].TheMapOwner.DoFastMapRedraw;
end;

procedure Tdbtablef.BitBtn22Click(Sender: TObject);
begin
   DEM_Gaz_opts.SetGazOptions;
   //with GISdb[DBonTable].TheMapOwner do begin
      GISdb[DBonTable].TheMapOwner.MapDraw.DeleteSingleMapLayer(GISdb[DBonTable].TheMapOwner.MapDraw.GazOverlayfName);
      //le].TheMapOwner do begin
      GISdb[DBonTable].TheMapOwner.DoFastMapRedraw;
   //end;
end;

procedure Tdbtablef.BitBtn23Click(Sender: TObject);
begin
   GISdb[DBonTable].TheMapOwner.DoBaseMapRedraw;
end;


procedure Tdbtablef.BitBtn24Click(Sender: TObject);
begin
   Areasinclusters1.Visible := GISdb[DBonTable].MyData.FieldExists('AREA') and GISdb[DBonTable].MyData.FieldExists('CLUSTER');

   DEMIX1Click(Sender);
end;

procedure Tdbtablef.BitBtn28Click(Sender: TObject);
begin
   GISdb[DBonTable].LayerIsOn := not GISdb[DBonTable].LayerIsOn;
   ToggleLayer(GISdb[DBonTable].LayerIsOn);
end;


procedure Tdbtablef.ToggleLayer(LayerOn : boolean);
begin
   if ValidDB(DBonTable) then begin
      GISdb[DBonTable].ToggleLayer(LayerOn);
   end;
end;

procedure Tdbtablef.BitBtn2Click(Sender: TObject);
begin
   SaveBackupDefaults;
   MDDef.KMLExportLayers := true;
   MDDef.KMLExportSeparateFans := true;
   MDDef.AskAboutKMLExport := true;
   GISdb[DBonTable].theMapOwner.ForGoogleearth1Click(nil);
   RestoreBackupDefaults;
   ShowStatus;
end;


procedure Tdbtablef.BitBtn3Click(Sender: TObject);
begin
   GISdb[DBonTable].FillFieldWithValue('IMAGE','');
   Viewshedfields1Click(Sender);
   ShowStatus;
end;


(*
procedure Tdbtablef.SetMultipleFilters;

   procedure DoOne(var ComboBox : tComboBox; aLabel : tlabel; fName : PathStr);
   begin
      aLabel.Caption := fName;
      GISdb[DBonTable].FillComboBoxFromField(ComboBox,fName);
      ComboBox.Enabled := true;
      ComboBox.Visible := fName <> '';
      WriteLineToDebugFile(fname + '=' + IntToStr(ComboBox.Items.Count));
   end;

begin
   GroupBox1.Visible := true;
   DoOne(ComboBox4,Label4,ffName1);
   DoOne(ComboBox5,Label5,ffName2);
   DoOne(ComboBox6,Label6,ffName3);
   DoOne(ComboBox7,Label7,ffName4);
end;
*)

procedure Tdbtablef.FormCreate(Sender: TObject);
begin
   {$IfDef RecordDataBase} WriteLineToDebugFile('Tdbtablef.FormCreate in'); {$EndIf}
   DBonTable := 0;
   AllOptionsForFans := false;
   SimplePlot := false;
   RedGrayGraph := false;
   FormWorking := false;
   VATEdit := false;
   EditSymbologyOnly := false;
   TrainingClassAvailable := false;
   Panel1.Visible := false;
   Panel3.Visible := false;
   Panel7.Visible := false;
   Wavelengthmeanallclasses1.Enabled := false;
   WavelengthStdDevallclasses1.Enabled := false;

   NoStatsGraph := false;
   TrackBarringAllowed := false;
   CheckBox1.Visible := MDdef.CanEditGIS in [egisSometimes,egisAlways];
   CheckBox1.Checked := MDdef.CanEditGIS in [egisAlways];
   Features.Checked := MDDef.LabelGazOnMap;
   SetFonts;

   if MDDef.DefaultEditDBsInGrid then Editrecordsingrid1Click(Sender);
   BaseMapBitmap := Nil;
   GraphOwnerBitmap := Nil;
   LinkGraph := Nil;
   if (Sender <> Nil) then begin
      Closing := false;
      CanCloseIt := true;
   end;
   Width := 600;
   Petmar.CheckFormPlacement(Self);
   Top := 0;
   Left := wmdem.Width - Width - 20;
   TrackBar1.Position := MDDef.SecondGridOpacity;
   {$IfDef RecordDataBase} WriteLineToDebugFile('Tdbtablef.FormCreate out'); {$EndIf}
end;

procedure Tdbtablef.Frequencytable1Click(Sender: TObject);
begin
   Countuniquevalues1Click(Sender);
end;


procedure Tdbtablef.Fuibn1Click(Sender: TObject);
var
   aString : ShortString;
   RecID : integer;
   Findings : tStringList;
begin
   ShowHourglassCursor;
   Findings := tStringList.Create;
   GISdb[DBonTable].EmpSource.Enabled := false;
   GISdb[DBonTable].MyData.First;
   while not GISdb[DBonTable].MyData.eof do begin
      aString := GISdb[DBonTable].MyData.GetFieldByNameAsString(SelectedColumn);
      if not IsNumeric(aString) then begin
          RecID := GISdb[DBonTable].MyData.GetFieldByNameAsInteger('REC_ID');
          Findings.Add(IntegerToString(RecID,12) + '  ' + aString);
      end;
      GISdb[DBonTable].MyData.Next;
   end;
   ShowStatus;
   if (Findings.Count = 0) then begin
      MessageToContinue('All records have numeric values in ' + SelectedColumn);
      Findings.Free;
   end
   else DisplayAndPurgeStringList(Findings,SelectedColumn + ' non numeric values=' + IntToStr(Findings.Count));
end;

procedure Tdbtablef.BitBtn4Click(Sender: TObject);
begin
   {$IfDef RecordDataBase} WriteLineToDebugFile('Tdbtablef.BitBtn4Click--DB filter selected'); {$EndIf}
   GISdb[DBonTable].DisplayTable(GISdb[DBonTable].MyData.Filter);
   ShowStatus;
end;


procedure Tdbtablef.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   {$IfDef RecordCloseDB} WriteLineToDebugFile('Tdbtablef.FormClose in'); {$EndIf}
   inherited;
   Action := caFree;
   {$IfDef RecordCloseDB} WriteLineToDebugFile('Tdbtablef.FormClose out'); {$EndIf}
end;


procedure Tdbtablef.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
   {$IfDef RecordCloseDB} WriteLineToDebugFile('Tdbtablef.FormCloseQuery in'); {$EndIf}
   if (Closing) then begin
      {$IfDef RecordCloseDB} WriteLineToDebugFile('already closing'); {$EndIf}
   end
   else begin
      if ValidDB(DBonTable) then begin
         {$IfDef RecordCloseDB} WriteLineToDebugFile('Tdbtablef.FormCloseQuery has valid DB'); {$EndIf}
         CanClose := CanCloseIt;
         if CanClose then begin
            {$IfDef RecordCloseDB} WriteLineToDebugFile('Tdbtablef.FormCloseQuery call CloseAndNilNumberedDB'); {$EndIf}
            CloseAndNilNumberedDB(DBonTable);
            {$IfDef RecordCloseDB} WriteLineToDebugFile('Tdbtablef.FormCloseQuery closed'); {$EndIf}
         end;
      end;
   end;
   {$IfDef RecordCloseDB} WriteLineToDebugFile('Tdbtablef.FormCloseQuery out'); {$EndIf}
end;

procedure Tdbtablef.Button1Click(Sender: TObject);
begin
{$IfDef RecordDataBase} WriteLineToDebugFile('Tdbtablef.Button1Click--DB all records'); {$EndIf}
   GISdb[DBonTable].ClearGISFilter;
   //ComboBox4.Text := '';
   //ComboBox5.Text := '';
   //ComboBox6.Text := '';
   //ComboBox7.Text := '';
   if (GISdb[DBonTable].TheMapOwner <> Nil)and (Sender <> Quickfilter1) then GISdb[DBonTable].RedrawLayerOnMap;
   ShowStatus;
end;

procedure Tdbtablef.BitBtn5Click(Sender: TObject);
begin
   {$IfDef RecordDataBase} WriteLineToDebugFile('Tdbtablef.BitBtn5Click--Plot'); {$EndIf}
   if not ValidDB(DBonTable) then exit;
   if GISdb[DBonTable].ItsFanFile and (not AllOptionsForFans) then begin
      PopupMenu9.PopUp(Mouse.CursorPos.X,Mouse.CursorPos.Y);
   end
   else with GISdb[DBonTable] do begin
       Plotfanlocations2.Visible := GISdb[DBonTable].ItsFanFile;
       Labelfans1.Visible := GISdb[DBonTable].ItsFanFile;
       if GazDB then begin
         Label1.Visible := true;
         Colorcodebynumericfield1.Visible := false;
         Vectordata1.Visible := false;
      end
      else if SimplePlot then begin
         RedrawLayerOnMap;
         exit;
      end
      else begin
         Label1.Visible := CanPlot and (ItsaPointDB or AreaShapeFile(ShapeFileType) or LatLongCornersPresent);
      end;
      DipAndStrikes1.Visible := DipStrikeFieldExists;
      EarthquakeMechanisms2.Visible := FocalMechsPresent;
      {$IfDef ExGeography}
         Climatestations1.Visible := false;
      {$Else}
         Climatestations1.Visible := KoppenPresent;
      {$EndIf}

      LayerSymbology1.Visible := (LayerTable <> Nil);
      Labeleverynthrecord1.Visible := ItsaPointDB;
      Editgazetteersymbology1.Visible := GazDB;
      ColorfieldinDB1.Visible :=  (GISdb[DBonTable].ColorPresent or GISdb[DBonTable].RGBColorPresent) or GISdb[DBonTable].MyData.FieldExists('LINE_COLOR') ;
      TerrainFabric1.Visible := GISdb[DBonTable].MyData.FieldExists('S1S2') and GISdb[DBonTable].MyData.FieldExists('S2S3')  and GISdb[DBonTable].MyData.FieldExists('FABRIC_DIR');


      PointSymbolsInDB1.Visible := PointSymbolFieldsPresent;
      ColorCodeByNumericField1.Visible := (NumericFields > 0) or (StringFields > 0);
      Proportionalsquares1.Visible := (NumericFields > 0) or IconPresent;
      PointSeparation1.Visible := SecondLatLongFieldsPresent;
      ZipAToneField1.Visible := AreaShapeFile(ShapeFileType);
      SingleZipATone1.Visible := AreaShapeFile(ShapeFileType);

      ZValues1.Visible:= (ShapeFileType in [11,13,15]);
      Plotcoveragecircles1.Visible := GISdb[DBonTable].MyData.FieldExists('RANGE') and ItsaPointDB;
      Connectsequentialpoints1.Visible := CanPlot and ItsAPointDB;
      Showarearecords1.Visible := LatLongCornersPresent or CentroidPresent;
      Creategrid1.Visible := ShapeFile3D(ShapeFileType) and ValidDEM(TheMapOwner.MapDraw.DEMonMap);
      Downhilluphillsegments1.Visible := ShapeFile3D(ShapeFileType);
      Photolocations1.Visible := PhotoLocationsPresent;
      TernaryDiagram1.Visible := MDDef.ShowTernary;
      ChangeTIGERsymbology1.Visible := ItsTigerShapeFile;
      ColorFieldInJoinedDB1.Visible := (GISdb[DBonTable].LinkTable <> Nil);

      Animatefield1.Visible := MDDef.ShowExperimentalOptions;
      Timesequence1.Visible := MDDef.ShowExperimentalOptions;
      Treatasregulardatabase1.Visible := ItsOSMShapeFile or ItsTigerShapeFile;

      ZoomtoDBcoverage1.Visible := (TheMapOwner <> Nil);   // and ((GISdb[DBonTable].aShapeFile <> Nil) or FileExists(ChangeFileExt(GISdb[DBonTable].dbFullName,'.shx')));

      ChangeLatLongFields1.Visible := MDDef.AdvancedDBops;
      TinContour1.Visible := MDDef.AdvancedDBops;

      BoxPlot1.Visible := GISDB[DBonTable].MyData.FieldExists('MIN') and GISDB[DBonTable].MyData.FieldExists('MAX');

      PlotPopupMenu2.PopUp(Mouse.CursorPos.X,Mouse.CursorPos.Y);
   end;
end;


procedure Tdbtablef.FormActivate(Sender: TObject);
begin
   if ValidDB(DBonTable) and (not FormWorking) and (not Closing) then begin
      {$IfDef RecordFormActivate} WriteLineToDebugFile('Tdbtablef.FormActivate in'); {$EndIf}
      Button4.Enabled := CheckBox1.Checked;
      GISdb[DBonTable].ColorButtonForSymbol(BitBtn1);
      if (GISdb[DBonTable].TheMapOwner <> Nil) then begin
         GISdb[DBonTable].TheMapOwner.BringToFront;
         Self.BringToFront;
      end;
      //RecognizeDEMIXVersion(DBonTable);
      ShowStatus;
      {$IfDef RecordFormActivate} WriteLineToDebugFile('Tdbtablef.FormActivate out'); {$EndIf}
   end;
end;


procedure Tdbtablef.BitBtn8Click(Sender: TObject);
begin
   if ValidDB(DBonTable) then begin
        Vectoraverageinbox1.Enabled := GISdb[DBonTable].DEMwithDBsMap and (GISdb[DBonTable].dbOpts.MagField <> '') and (GISdb[DBonTable].dbOpts.DirField<> '');
        N3dGraph1.Visible := (GISdb[DBonTable].NumericFields > 2);
        N2dGraph1.Visible := (GISdb[DBonTable].NumericFields > 1);
        N2dGraph2series1.Visible := (GISdb[DBonTable].NumericFields > 2);
        N2Dgraphcolorcoded1.Visible := GISdb[DBonTable].NumericFields > 1;
        SumForOneField1.Visible := GISdb[DBonTable].NumericFields > 0;
        FieldCorrelations1.Visible := GISdb[DBonTable].NumericFields > 0;
        LineLength1.Visible := LineShapeFile(GISdb[DBonTable].ShapeFileType);

        AllDBs1.Visible := (GISdb[DBonTable].NumericFields > 1) and (TotalNumOpenDatabase > 1);
        TimeSeries1.Visible := MDDef.ShowDBDateTimeSeries;
        if NoStatsGraph then begin
           Histogram1.Visible := false;
           TernaryDiagram1.Visible := false;
           N2DGraph1.Visible := false;
           N2dGraphColorCoded1.Visible := false;
           N2DGraphColorCodeText1.Visible := false;
           N3dGraph1.Visible := false;
           RoseDiagram1.Visible := false;
        end;
        FeatureStatistics1.Visible := (GISdb[DBonTable].TheMapOwner <> Nil) and (GISdb[DBonTable].TheMapOwner.MapDraw.DEMonMap <> 0) and
             (DEMGlb[GISdb[DBonTable].TheMapOwner.MapDraw.DEMonMap].SelectionMap.MapDraw.FeatureGrid <> 0);
        PlotallXYFiles1.Visible := GISdb[DBonTable].MyData.FieldExists('XY_FILE');
        Redistrict1.Visible := (MDDef.ProgramOption = ExpertProgram) and GISdb[DBonTable].MyData.FieldExists('BLACK1') and GISdb[DBonTable].MyData.FieldExists('WHITE1');
        Elevationsatbenchmarks1.Visible := GISdb[DBonTable].MyData.FieldExists('Z');
        ShowJoin1.Visible := GISdb[DBonTable].LinkTable <> Nil;
        ClearJoin1.Visible := GISdb[DBonTable].LinkTable <> Nil;
        ResetJoin1.Visible := FileExists(GISdb[DBonTable].dbOpts.LinkTableName);
        SetJoin1.Visible := GISdb[DBonTable].LinkTable = Nil;
        LOSterrainprofile1.Visible := GISdb[DBonTable].SecondLatLongFieldsPresent and GISdb[DBonTable].DEMwithDBsMap;
        LOStopoprofile1.Visible := LOSterrainprofile1.Visible;
        ProfileallDEMs1 .Visible := LOSterrainprofile1.Visible;
        TerrainProfiles1.Visible := GISdb[DBonTable].ItsAShapeFile and LineShapeFile(GISdb[DBonTable].ShapeFileType);
        Linelength1.Visible := GISdb[DBonTable].ItsAShapeFile and LineShapeFile(GISdb[DBonTable].ShapeFileType);
        Linelengthsforonefield1.Visible := GISdb[DBonTable].ItsAShapeFile and LineShapeFile(GISdb[DBonTable].ShapeFileType);
        Arearecordstatistics1.Visible := GISdb[DBonTable].ItsAShapeFile and LineOrAreaShapeFile(GISdb[DBonTable].ShapeFileType);
        Oceanography2.Visible := MDDef.ShowOceanographyOptions;
        TidePredictions1.Visible := GISdb[DBonTable].MyData.FieldExists('AMPLITUDE') and GISdb[DBonTable].MyData.FieldExists('PHASE') and GISdb[DBonTable].MyData.FieldExists('SPEED');
        Monthlyanalysis1.Visible := GISdb[DBonTable].MyData.FieldExists(GISdb[DBonTable].MonthFieldName);
        Graphwithranges1.Visible := GISdb[DBonTable].MyData.FieldExists('CLASS') and GISdb[DBonTable].MyData.FieldExists('MEAN') and GISdb[DBonTable].MyData.FieldExists('STD_DEV');
        GeomorphometryStats1.Visible := (GISdb[DBonTable].ItsAShapeFile and AreaShapeFile(GISdb[DBonTable].ShapeFileType)) and MDDef.AdvancedDBops and GISdb[DBonTable].DEMwithDBsMap;
        Geomrophometrystaseachpointneighborhood1.Visible := GISdb[DBonTable].ItsAPointDB and MDDef.AdvancedDBops and GISdb[DBonTable].DEMwithDBsMap;
        MultipleLinearRegression1.Visible := MDDef.AdvancedDBops;
        MultipleGraphMatrix1.Visible := MDDef.AdvancedDBops;
        LongestString1.Visible := MDDef.AdvancedDBops;
        Sum1.Visible := MDDef.AdvancedDBops;
        Statisticsgroupedbyonefield1.Visible := MDDef.AdvancedDBops;
        Fieldcorrelations1.Visible := MDDef.AdvancedDBops;
        Centr1.Visible := MDDef.AdvancedDBops;
        FindNeighbors1.Visible := MDDef.AdvancedDBops;
        PrincipalComponents1.Visible := MDDef.AdvancedDBops;
        FieldCooccurrenceMatrix1.Visible := MDDef.AdvancedDBops;
        SticksTadpolePlot1.Visible := MDDef.AdvancedDBops;
        Cluster2.Visible := MDDef.AdvancedDBops;
        LinkDataBase1.Visible := MDDef.AdvancedDBops;
        LVIS1.Visible := GISdb[DBonTable].MyData.FieldExists('RH99');
        ICESat21.Visible := GISdb[DBonTable].IsIcesat;
        PointcloudstoanalyzeglobalDEMs1.Visible := (GISdb[DBonTable].MyData.FieldExists('BEAM') and GISdb[DBonTable].MyData.FieldExists('TRACK_ID')) or
            (GISdb[DBonTable].MyData.FieldExists('CLOUD_0_5') and GISdb[DBonTable].MyData.FieldExists('CLOUD_995'));

        ClusterStatistics1.Enabled := GISdb[DBonTable].MyData.FieldExists('CLUSTER');
        ClusterFrequency1.Enabled := GISdb[DBonTable].MyData.FieldExists('CLUSTER');
        ClusterMapLocations1.Enabled := GISdb[DBonTable].MyData.FieldExists('CLUSTER') and (GISdb[DBonTable].theMapOwner <> nil);
        ilecharacteristicsbytileforCopDEM1.Enabled := GISdb[DBonTable].MyData.FieldExists('CLUSTER') and GISdb[DBonTable].MyData.FieldExists('DEM') ;
        //GraphSSIMR2bycluster1.Enabled := GISdb[DBonTable].MyData.FieldExists('CLUSTER') and GISdb[DBonTable].MyData.FieldExists('METRIC');
        //GraphSSIMR2byDEM1.Enabled := GISdb[DBonTable].MyData.FieldExists('DEM') and GISdb[DBonTable].MyData.FieldExists('METRIC');

        TransposeSSIMR2forclusters1.Enabled := true;  //{GISdb[DBonTable].MyData.FieldExists('CLUSTER') and} GISdb[DBonTable].MyData.FieldExists('METRIC');
   
        {$IfDef ExGeography}
           Koppenlatitudestats1.Visible := false;
        {$Else}
           Koppenlatitudestats1.Visible := GISdb[DBonTable].KoppenPresent;
        {$EndIf}

        {$IfDef ExGeology}
           Earthquakefocalmechanisms2.Visible := false;
           StructuralGeology1.Visible := false;
           Insertdipdirectionsdipandstrike1.Visible := false;
        {$Else}
           Earthquakefocalmechanisms2.Visible := GISdb[DBonTable].FocalMechsPresent;
           StructuralGeology1.Visible := MDDef.ShowGeologyOptions and GISdb[DBonTable].ItsAPointDB;
           Geomorphometryatlas1.Visible := MDDef.AdvancedDBops and GISdb[DBonTable].ItsAPointDB and (MDDef.ProgramOption = ExpertProgram);
           Insertdipdirectionsdipandstrike1.Visible := (not GISdb[DBonTable].MyData.FieldExists('FP1_DIPSTRK')) and (not GISdb[DBonTable].MyData.FieldExists('FP1_DIPDIR'));
        {$EndIf}

        {$IfDef ExSideScan}
           LoadMSTFiles1.Visible := false;
        {$Else}
           LoadMSTFiles1.Visible := GISdb[DBonTable].MyData.FieldExists('MST_FILE');
        {$EndIf}

        {$IfDef ExAdvancedSats}
           SupervisedClassification1.Visible := false;
        {$Else}
           SupervisedClassification1.Visible := ((GISdb[DBonTable].TheMapOwner <> Nil) and (GISdb[DBonTable].TheMapOwner.MapDraw.MultiGridOnMap <> 0) and
                (MultiGridArray[GISdb[DBonTable].TheMapOwner.MapDraw.MultiGridOnMap].TrainingPointsDB = GISdb[DBonTable].DBNumber));
        {$EndIf}

        {$IfDef ExPointCloud}
           N3Dslicer1.Visible := false;
        {$Else}
           N3dSlicer1.Visible := true;
        {$EndIf}

        StatsPopupMenu1.PopUp(Mouse.CursorPos.X,Mouse.CursorPos.Y);
   end;
end;


procedure Tdbtablef.Countofrecordswithsubstring1Click(Sender: TObject);
begin
   GISdb[DBonTable].CountFieldsWithSubstring(SelectedColumn);
end;


procedure Tdbtablef.Countrecordswithsubstring1Click(Sender: TObject);
begin
   GISdb[DBonTable].CountFieldsWithSubstring('');
end;


procedure Tdbtablef.Countuniquevalues1Click(Sender: TObject);
var
  SecondFields,FieldsInDB,Findings : tStringList;
  WantedFieldName,SecondFieldName : shortstring;
  GridForm : tGridForm;
  Sum : float64;
  BaseFilter,OldFilter,LongFilter : Ansistring;
  TStr : shortstring;
  i,Num1,err,rc : integer;
  ft : tFieldType;
  fName : PathStr;


      procedure CloseAndShowResults;
      begin
         FieldsInDB.Free;
         fName := MDTempDir + 'stringgrid.csv';
         Findings.SaveToFile(fName);
         Findings.Free;
         GridForm.ReadCSVFile(fName);
         ShowStatus;
      end;


      procedure SetUpGridForm(Rows : integer);
      begin
         {$IfDef CountUniqueValues} WriteLineToDebugFile('SetUpGridForm in'); {$EndIf}
         GridForm := tGridForm.Create(Application);
         GridForm.HideCorrelationControls;
         GridForm.Caption := GISdb[DBonTable].dbName + '  ' + LongFilter + ' (n=' + IntToStr(GISdb[DBonTable].MyData.FiltRecsInDB) + ')' + '  unique entries=' + IntToStr(pred(Findings.Count));
         GridForm.StringGrid1.FixedCols := 0;
         GridForm.StringGrid1.FixedRows := 1;
         GridForm.ShowSortingControls(true);
         {$IfDef CountUniqueValues} WriteLineToDebugFile('SetUpGridForm out'); {$EndIf}
      end;

     procedure ToCSV;
     begin
        if (Num1 > 0) then begin
           ReplaceCharacter(TStr,',',';');
           TStr := TStr + ',' + IntToStr(Num1);
           if (Sender = Assignuniquecolors1) then TStr := TStr + ',123456789' ;
           Findings.Add(TStr);
        end;
     end;

begin
   {$IfDef CountUniqueValues} WriteLineToDebugFile('Tdbtablef.Countuniquevalues1Click in'); {$EndIf}
   with GISdb[DBonTable] do begin
     GISdb[DBonTable].EmpSource.Enabled := false;
     if (Sender = Listuniquevalues2) or (Sender = Frequencytable1) then begin
        WantedFieldName := SelectedColumn;
     end
     else begin
       if (Sender = Listnonnumericvalues1) or (Sender = Removenonnumericentries1) then begin
          WantedFieldName := PickField('unique values',[ftString]);
       end
       else begin
          WantedFieldName := PickField('unique values',StringOrIntegerField);
       end;
     end;

     if (Sender = Assignuniquecolors1) then begin
        fName := ChangeFileExt(DBAuxDir + 'colors_' + WantedFieldName + '_' + ExtractFileName(dbFullName),DefaultDBExt);
        if FileExists(fName) and (not AnswerIsYes('overwrite existing ' + fname)) then exit;
     end;

     {$IfDef CountUniqueValues} WriteLineToDebugFile('WantedFieldName =' + WantedFieldName); {$EndIf}
     if (WantedFieldName <> '') then begin
        if (Sender = TwoFields1) then begin
           SecondFieldName := PickField('second field',StringOrIntegerField);
           DBFieldUniqueEntries(SecondFieldName,SecondFields);
        end;
        if (Sender = Allrecordsmatchingsinglefield1) then begin
           SecondFieldName := PickField('field for sum',NumericFieldTypes);
        end;

        ShowHourglassCursor;
        EmpSource.Enabled := false;

        if (Sender = Singlefield1) or (Sender = Frequencytable1) or (Sender = Assignuniquecolors1) then begin
           {$IfDef CountUniqueValues} WriteLineToDebugFile('(Sender = Singlefield1) or (Sender = Frequencytable1)'); {$EndIf}
           FieldsInDB := tStringList.Create;
           FieldsInDB.Sorted := true;
           FieldsInDB.Duplicates := dupAccept;
           GISdb[DBonTable].MyData.First;
           i := 0;
           StartProgressAbortOption('Find ' + WantedFieldName);
           while not GISdb[DBonTable].MyData.eof do begin
              inc(i);
              if (i mod 100 = 0) then begin
                 UpdateProgressBar(i/GISdb[DBonTable].MyData.FiltRecsInDB );
                 EmpSource.Enabled := false;
              end;

              TStr := GISdb[DBonTable].MyData.GetFieldByNameAsString(WantedFieldName);
              if (TStr <> '') then FieldsInDB.Add(Tstr);
              GISdb[DBonTable].MyData.Next;
              if WantOut then break;
           end;
           ft := GISdb[DBonTable].MyData.GetFieldType(WantedFieldName);
           if ft in [ftInteger,ftSmallInt,ftFloat] then SortStringListNumerically(FieldsInDB);
           {$IfDef CountUniqueValues} WriteLineToDebugFile('First pass done'); {$EndIf}

           Findings := tStringList.Create;
           if (Sender = Assignuniquecolors1) then Findings.Add('NAME,N,COLOR')
           else Findings.Add(WantedFieldName + ',n');
           i := 0;
           Num1 := 0;
           TStr := '';
           StartProgress('Count ' + WantedFieldName);
           for I := 0 to pred(FieldsInDB.Count) do begin
              if (i mod 100 = 0) then UpdateProgressBar(i/FieldsInDB.Count);
              if (UpperCase(TStr) = UpperCase(FieldsInDB.Strings[i])) then begin
                 inc(Num1);
              end
              else begin
                 ToCSV;
                 TStr := FieldsInDB.Strings[i];
                 Num1 := 1;
              end;
           end;
           {$IfDef CountUniqueValues} WriteLineToDebugFile('Second pass done'); {$EndIf}
           ToCSV;  // for last values

           if (Sender = Assignuniquecolors1) then begin
              fName := ChangeFileExt(DBAuxDir + 'colors_' + WantedFieldName + '_' + ExtractFileName(dbFullName),DefaultDBExt);
              LinkColorTable := PetDBUtils.StringList2CSVtoDB(Findings,fName);
              FieldsInDB.Free;
              GISdb[GISdb[DBonTable].LinkColorTable].MyData.First;
              i := 0;
              rc := GISdb[GISdb[DBonTable].LinkColorTable].MyData.FiltRecsInDB;
              while not GISdb[GISdb[DBonTable].LinkColorTable].MyData.eof do begin
                 GISdb[GISdb[DBonTable].LinkColorTable].MyData.Edit;
                 Color := TerrainTColor(i,0,rc);
                 {$IfDef CountUniqueValues} WriteLineToDebugFile(IntToStr(i) + '   ' + IntToStr(Color)); {$EndIf}
                 GISdb[GISdb[DBonTable].LinkColorTable].MyData.SetFieldByNameAsInteger('COLOR',Color);
                 inc(i);
                 GISdb[GISdb[DBonTable].LinkColorTable].MyData.Next;
              end;
             ShowStatus;
             exit;
           end
           else SetUpGridForm(Findings.Count);
           CloseAndShowResults;
           {$IfDef CountUniqueValues} WriteLineToDebugFile('All done'); {$EndIf}
           exit;
        end;

        GISdb[DBonTable].EmpSource.Enabled := false;
        DBFieldUniqueEntries(WantedFieldName,FieldsInDB);
        if (Sender = TwoFields1) then begin
           for i := 0 to pred(SecondFields.Count) do FieldsInDB.Add(SecondFields.Strings[i]);
        end;
        if (Sender = Countuniquevalues1) then begin
           MessageToContinue('Unique values in ' + WantedFieldName + ': ' + IntToStr(FieldsInDB.Count));
           FieldsInDB.Free;
        end
        else if (Sender = Listnonnumericvalues1) or (Sender = Removenonnumericentries1) then begin
           FieldsInDB.Clear;
           GISdb[DBonTable].MyData.First;
           i := 0;
           while not GISdb[DBonTable].MyData.eof do begin
              TStr := GISdb[DBonTable].MyData.GetFieldByNameAsString(WantedFieldName);
              if (TStr <> '') then begin
                 Val(TStr,Sum,Err);
                 if (err <> 0) then begin
                    if (Sender = Removenonnumericentries1) then begin
                       GISdb[DBonTable].MyData.Edit;
                       GISdb[DBonTable].MyData.SetFieldByNameAsString(WantedFieldName,'');
                    end;
                    FieldsInDB.Add(TStr);
                    inc(i);
                 end;
              end;
              GISdb[DBonTable].MyData.Next;
           end;
           if (FieldsInDB.Count = 0) then MessageToContinue('No non-numeric fields')
           else DisplayAndPurgeStringList(FieldsInDB,WantedFieldName + ' Unique non numeric values=' + IntToStr(FieldsInDB.Count) + '   n=' + IntToStr(i));
        end
        else if (Sender = SingleField1) or (Sender = TwoFields1) or (Sender = Frequencytable1) or (Sender = Allrecordsmatchingsinglefield1) then begin
           if (MyData.Filter = '') then BaseFilter := '' else BaseFilter := GISdb[DBonTable].MyData.Filter + 'AND ';
           OldFilter := GISdb[DBonTable].MyData.Filter;
           LongFilter := WantedFieldName;
           if (Sender = TwoFields1) then LongFilter := LongFilter + ' or ' + SecondFieldName;
           if (FieldsInDB.Count > 0) then begin
              Findings := tStringList.Create;
              if (Sender = Allrecordsmatchingsinglefield1) then Findings.Add(LongFilter + ',n,' + SecondFieldName)
              else Findings.Add(LongFilter + ',n');
              SetUpGridForm(succ(FieldsInDB.Count));
              StartProgress('Counting');
              for i := 0 to pred(FieldsInDB.Count) do begin
                 if (i mod 25 = 0) then UpdateProgressBar(i/FieldsInDB.Count);
                 LongFilter := '(' + WantedFieldName + '=' + QuotedStr(ptTrim(FieldsInDB.Strings[i])) + ')';
                 if (Sender = TwoFields1) then begin
                    LongFilter := LongFilter + ' OR ' + '(' + SecondFieldName + '=' + QuotedStr(ptTrim(FieldsInDB.Strings[i])) + ')';
                 end;
                 dbOpts.MainFilter := BaseFilter + LongFilter;
                 AssembleGISFilter;
                 Num1 := GISdb[DBonTable].MyData.RecordCount;
                 {$IfDef RecordDBfilter} WriteLineToDebugFile('LongFilter=' + LongFilter); {$EndIf}
                 if (Sender = Allrecordsmatchingsinglefield1) then begin
                    GISdb[DBonTable].MyData.First;
                    Sum := 0;
                    while not GISdb[DBonTable].MyData.eof do begin
                       Sum := Sum + GISdb[DBonTable].MyData.GetFieldByNameAsFloat(SecondFieldName);
                       GISdb[DBonTable].MyData.Next;
                    end;
                    Findings.Add(FieldsInDB.Strings[i] + ',' + IntToStr(Num1) + ',' + RealToString(Sum,-18,-6));
                 end
                 else Findings.Add(FieldsInDB.Strings[i] + ',' + IntToStr(Num1));
              end;
              {$IfDef CountUniqueValues} WriteLineToDebugFile('Assembly done'); {$EndIf}
              CloseAndShowResults;
              {$IfDef CountUniqueValues} WriteLineToDebugFile('Grid read'); {$EndIf}
           end;
           dbOpts.MainFilter := OldFilter;
           GISdb[DBonTable].AssembleGISFilter;
        end
        else begin
           DisplayAndPurgeStringList(FieldsInDB,WantedFieldName + ' Unique values: ' + IntToStr(FieldsInDB.Count));
           ShowStatus;
        end;
     end;
   end;
   {$IfDef CountUniqueValues} WriteLineToDebugFile('Tdbtablef.Countuniquevalues1Click out'); {$EndIf}
end;


procedure Tdbtablef.Listnonnumericvalues1Click(Sender: TObject);
begin
   Countuniquevalues1Click(Sender);
end;

procedure Tdbtablef.Listuniquevalues1Click(Sender: TObject);
begin
   Countuniquevalues1Click(Sender);
end;

procedure Tdbtablef.Listuniquevalues2Click(Sender: TObject);
begin
   Countuniquevalues1Click(Sender);
end;


procedure Tdbtablef.Sum1Click(Sender: TObject);
begin
   MessageToContinue(SelectedColumn + ', Sum of values in field: ' + RealToString(GISdb[DBonTable].FieldSum(SelectedColumn),-12,-2),true);
   ShowStatus;
end;

procedure Tdbtablef.Sum3Click(Sender: TObject);
begin
    GISdb[DBonTable].AddMultiFieldStats('Sum',mfsSum);
end;

procedure Tdbtablef.Sumforallnumericfields1Click(Sender: TObject);
var
   TheFields,Findings : tStringList;
   fName : PathStr;
   i : integer;
begin
   GetFields(GISdb[DBonTable].MyData,AllVis,NumericFieldTypes,TheFields);
   Findings := tStringList.Create;
   Findings.Add('FIELD,SUM');
   for i := 0 to pred(TheFields.Count) do begin
      GISdb[DBonTable].EmpSource.Enabled := false;
      Findings.Add(TheFields.Strings[i] + ',' + RealToString(GISdb[DBonTable].MyData.FieldSum(TheFields.Strings[i]),-12,-4));
   end;
   fName := MDtempDir + 'field_sums_' + GISdb[DBonTable].dbName + '.dbf';
   StringList2CSVtoDB(Findings,fName);
   ShowStatus;
end;

procedure Tdbtablef.Sumforonefield1Click(Sender: TObject);
begin
   SelectedColumn := GISdb[DBonTable].PickField('Field for statistics',NumericFieldTypes);
   if (SelectedColumn <> '') then Sum1Click(Sender);
end;

procedure Tdbtablef.Sumofneighbors1Click(Sender: TObject);
begin
   IgnoreMissingData1Click(Sender);
end;

procedure Tdbtablef.Sumtwofields1Click(Sender: TObject);
var
   i,Sign : integer;
   SumName : String[10];
   l1,l2 : shortstring;
   x,y : float32;
begin
   {$IfDef RecordEditsDone} WriteLineToDebugFile('Tdbtablef.Sumtwofields1Click in'); {$EndIf}
    with GISdb[DBonTable] do begin
       i := 2;
       l2 := '';
       if (Sender = QuotientTwoFields1) then begin
          l1 := 'Numerator';
          l2 := 'Denominator';
          SumName := 'RATIO';
       end
       else if (Sender = Multiplytwofields1) then begin
          l1 := 'First field (*)';
          l2 := 'Second field (*)';
          SumName := 'RATIO';
       end
       else if (Sender = LogOfField1) then begin
          i := 1;
          l1 := 'log';
          SumName := 'LOG';
       end
       else if (Sender = DifferenceTwoFields1) then begin
          l1 := 'X field (X-Y)';
          l2 := 'Y field (X-Y)';
          SumName := 'DIFF';
       end
       else if (Sender = SinOfField1) then begin
          l1 := 'sin';
          i := 1;
          SumName := 'SIN';
       end
       else if (Sender = CosOfField1) then begin
          l1 := 'cos';
          i := 1;
          SumName := 'COS';
       end
       else begin
          l1 := 'First field (+)';
          l2 := 'Second field (+)';
          SumName := 'SUM';
       end;
       dbOpts.XField := '';
       dbOpts.YField := '';
       if (Sender = SumTwoFields1) then Sign := 1 else Sign := -1;

       PickNumericFields(dbgtUnspecified,i,l1,l2,'');
       SumName := SumName + '_' + dbOpts.XField;

       SumName := GetFieldNameForDB('New Field',True,SumName);

       {$IfDef RecordEditsDone} WriteLineToDebugFie('New field ' + SumName + ' from ' + dbOpts.XField + '  ' + dbOpts.YField): {$EndIf}

       if (Sender = SinOfField1) or (Sender = CosOfField1) then AddFieldToDataBase(ftFloat,SumName,8,5)
       else AddFieldToDataBase(ftFloat,SumName,18,6);

       i := 0;
       StartProgress('Work');
       GISdb[DBonTable].MyData.First;
       while not GISdb[DBonTable].MyData.EOF do begin
          if (i mod 100 = 0) then begin
             UpdateProgressBar(i/GISdb[DBonTable].MyData.FiltRecsInDB);
             EmpSource.Enabled := false;
          end;
          inc(i);
          GISdb[DBonTable].MyData.Edit;
          if GetFloat32FromTableLinkPossible(dbOpts.XField,x) and
             ((Sender = LogOfField1) or (Sender = SinOfField1) or (Sender = CosOfField1) or GetFloat32FromTableLinkPossible(dbOpts.YField,y)) then begin
                if (Sender = LogOfField1) then begin
                   if (x > 0) then GISdb[DBonTable].MyData.SetFieldByNameAsFloat(SumName,Log10(x));
                end
                else if (Sender = SinOfField1) then begin
                   GISdb[DBonTable].MyData.SetFieldByNameAsFloat(SumName,SinDeg(x));
                end
                else if (Sender = CosOfField1) then begin
                   GISdb[DBonTable].MyData.SetFieldByNameAsFloat(SumName,CosDeg(x));
                end
                else if (Sender = QuotientTwofields1) then begin
                   if (abs(y) > 0.00001) then GISdb[DBonTable].MyData.SetFieldByNameAsFloat(SumName,x/y);
                end
                else if (Sender = Multiplytwofields1) then begin
                   GISdb[DBonTable].MyData.SetFieldByNameAsFloat(SumName,x * y);
                end
                else GISdb[DBonTable].MyData.SetFieldByNameAsFloat(SumName,x + Sign * y);
          end;
          GISdb[DBonTable].MyData.Next;
       end;
       {$IfDef RecordEditsDone} WriteLineToDebugFile('Tdbtablef.Sumtwofields1Click computations over'); {$EndIf}
       ClearFieldRange(SumName);
       if MDDef.ShowNewFieldStats then DisplayFieldStatistics(SumName);
       ShowStatus;
       {$IfDef RecordEditsDone} WriteLineToDebugFile('Tdbtablef.Sumtwofields1Click computations out'); {$EndIf}
    end;
end;


procedure Tdbtablef.Sunabovethehorizon1Click(Sender: TObject);
var
   Lat,Long : float64;
begin
    if GISdb[DBonTable].ValidLatLongFromTable(Lat,Long) and (GISdb[DBonTable].TheMapOwner <> Nil) then begin
       DEMOptions.SetHorizonOptions(GISdb[DBonTable].TheMapOwner,Lat,Long);
    end;
end;

procedure Tdbtablef.Differencetwofields1Click(Sender: TObject);
begin
   Sumtwofields1Click(Sender);
end;


procedure Tdbtablef.DifferentrankingsbyCriteria1Click(Sender: TObject);
begin
    DifferentRankingsByCriteria(DBonTable);
end;

procedure Tdbtablef.Asymmetric1Click(Sender: TObject);
begin
   Symmetric1Click(Sender);
end;


procedure Tdbtablef.Symmetric1Click(Sender: TObject);
const
   MaxSize = 500;
type
   tBigArray = array[1..MaxSize,1..MaxSize] of integer;
var
  SecondFields,FieldsInDB : tStringList;
  WantedFieldName,SecondFieldName : shortstring;
  f1,f2 : ShortString;
  GridForm : tGridForm;
  Min,Max : float64;
  Min1,Max1,Min2,Max2,
  i,j,k,rc : integer;
  BigArray : ^tBigArray;
  Total1,Total2 : array[1..MaxSize] of integer;
begin
   {$IfDef RecordCorrelationMatrix} WriteLineToDebugFile('Tdbtablef.Symmetric1Click in'); {$EndIf}

   with GISdb[DBonTable] do begin
     WantedFieldName := PickField('first field',[ftString,ftInteger,ftSmallInt]);
     SecondFieldName := PickField('second field',[ftString,ftInteger,ftSmallInt]);
     if (WantedFieldName <> '') and (SecondFieldName <> '') then begin
        if (Sender = Compareclassifications1) then begin
           EmpSource.Enabled := false;
           FieldRange(WantedFieldName,Min,Max);
           Min1 := round(Min);
           Max1 := round(max);
           EmpSource.Enabled := false;
           FieldRange(SecondFieldName,Min,Max);
           Min2 := round(Min);
           Max2 := round(max);
        end
        else begin
           EmpSource.Enabled := false;
           DBFieldUniqueEntries(WantedFieldName,FieldsInDB);
           EmpSource.Enabled := false;
           DBFieldUniqueEntries(SecondFieldName,SecondFields);
           if (Sender <> Asymmetric1) then begin
              for i := 0 to pred(SecondFields.Count) do FieldsInDB.Add(SecondFields.Strings[i]);
           end;
        end;

        EmpSource.Enabled := false;
        {$IfDef RecordCorrelationMatrix} WriteLineToDebugFile('Tdbtablef.Symmetric1Click set up done');    {$EndIf}

        if (Sender <> Compareclassifications1) and ((FieldsInDB.Count > MaxSize) or (SecondFields.Count > MaxSize)) then begin
           MessageToContinue('Too many values in database');
        end
        else begin
           GridForm := tGridForm.Create(Application);
           GridForm.HideCorrelationControls;
           GridForm.Caption := GISdb[DBonTable].dbName + '  ' + WantedFieldName + ' by ' + SecondFieldName;

           if (Sender = Compareclassifications1) then begin
              GridForm.StringGrid1.ColCount := round(Max1-Min1) + 3;
              for I := Min1 to Max1 do GridForm.StringGrid1.Cells[succ(i-Min1),0] := WantedFieldName +'=' + IntToStr(i);
              GridForm.StringGrid1.Cells[pred(GridForm.StringGrid1.ColCount),0] := 'Total';

              GridForm.StringGrid1.RowCount := round(Max2-Min2) + 3;
              for I := Min2 to Max2 do GridForm.StringGrid1.Cells[0,succ(i-Min2)] := SecondFieldName +'=' + IntToStr(i);
              GridForm.StringGrid1.Cells[0,pred(GridForm.StringGrid1.RowCount)] := 'Total';
           end
           else begin
              GridForm.StringGrid1.ColCount := succ(FieldsInDB.Count);
              for I := 0 to pred(FieldsInDB.Count) do GridForm.StringGrid1.Cells[succ(i),0] := FieldsInDB.Strings[i];

              if (Sender = Asymmetric1) then begin
                 GridForm.StringGrid1.RowCount := succ(SecondFields.Count);
                 for I := 0 to pred(SecondFields.Count) do GridForm.StringGrid1.Cells[0,succ(i)] := SecondFields.Strings[i];
              end
              else begin
                 GridForm.StringGrid1.RowCount := succ(FieldsInDB.Count);
                 for I := 0 to pred(FieldsInDB.Count) do GridForm.StringGrid1.Cells[0,succ(i)] := FieldsInDB.Strings[i];
              end;
           end;

           {$IfDef RecordCorrelationMatrix} WriteLineToDebugFile('Tdbtablef.Symmetric1Click string grid up'); {$EndIf}

           New(BigArray);
           for i := 1 to MaxSize do begin
              Total1[i] := 0;
              Total2[i] := 0;
              for j := 1 to MaxSize do BigArray^[i,j] := 0;
           end;

           GISdb[DBonTable].MyData.First;
           k := 0;
           rc := GISdb[DBonTable].MyData.RecordCount;
           StartProgress('Matrix');
           while not eof do begin
              if (k mod 100 = 0) then begin
                 UpdateProgressBar(k/rc);
                 EmpSource.Enabled := false;
              end;
              {$IfDef RecordCorrelationMatrix} if (k mod 5000 = 0) then WriteLineToDebugFile('Tdbtablef.Symmetric1Click string,  k=' + IntToStr(k)); {$EndIf}
              inc(k);
              f1 := GISdb[DBonTable].MyData.GetFieldByNameAsString(WantedFieldName);
              f2 := GISdb[DBonTable].MyData.GetFieldByNameAsString(SecondFieldName);
              if (f1 <> '') and (f2 <> '') then begin
                 if (Sender = Compareclassifications1) then begin
                    i := succ(MyData.GetFieldByNameAsInteger(WantedFieldName)-Min1);
                    j := succ(MyData.GetFieldByNameAsInteger(SecondFieldName)-Min2);
                    inc(BigArray^[i,j]);
                    inc(Total1[i]);
                    inc(Total2[j]);
                 end
                 else begin
                    i := succ(FieldsInDB.IndexOf(f1));
                    if (Sender = Asymmetric1) then begin
                       j := succ(SecondFields.IndexOf(f2));
                       inc(BigArray^[i,j]);
                    end
                    else begin
                       j := succ(FieldsInDB.IndexOf(f2));
                       if (i=j) then inc(BigArray^[i,j])
                       else begin
                          inc(BigArray^[i,j]);
                          inc(BigArray^[j,i]);
                       end;
                    end;
                 end;
              end {if (f1 <> '') and (f2 <> '')};
              GISdb[DBonTable].MyData.Next;
           end {while};

           {$IfDef RecordCorrelationMatrix} WriteLineToDebugFile('Tdbtablef.Symmetric1Click loop over'); {$EndIf}

           for i := 1 to MaxSize do begin
              if (Sender = Compareclassifications1) then GridForm.StringGrid1.Cells[succ(i-Min1),pred(GridForm.StringGrid1.RowCount)] := IntToStr(Total1[i]);
              for j := 1 to MaxSize do begin
                if (Sender = Compareclassifications1) then GridForm.StringGrid1.Cells[pred(GridForm.StringGrid1.ColCount),succ(j-Min2)] := IntToStr(Total2[j]);
                if BigArray^[i,j] > 0 then begin
                   if (Sender = Compareclassifications1) then begin
                      GridForm.StringGrid1.Cells[succ(i-Min1),succ(j-Min2)] := IntToStr(BigArray^[i,j]);
                   end
                   else begin
                      GridForm.StringGrid1.Cells[i,j] := IntToStr(BigArray^[i,j]);
                   end;
                end;
              end;
           end;

           {$IfDef RecordCorrelationMatrix} WriteLineToDebugFile('Tdbtablef.Symmetric1Click grid filled in'); {$EndIf}

           Dispose(BigArray);
           GridForm.StringGrid1.FixedCols := 1;
           GridForm.StringGrid1.FixedRows := 1;
        end;
        FieldsInDB.Free;
        SecondFields.Free;
        ShowStatus;
     end;
   end;
   {$IfDef RecordCorrelationMatrix} WriteLineToDebugFile('Tdbtablef.Symmetric1Click out'); {$EndIf}
end;


procedure Tdbtablef.BitBtn9Click(Sender: TObject);
begin
   ManageDBDisplay(DBGrid1,DBonTable);
end;


procedure Tdbtablef.Both1Click(Sender: TObject);
begin
   EditMyFont(MDDef.DBtableGridFont);
   MDDef.DBtableTitleFont := MDDef.DBtableGridFont;
   SetFonts;
end;



procedure Tdbtablef.SetFonts;
begin
   LoadMyFontIntoWindowsFont(MDDef.DBtableGridFont,DBGrid1.Font);
   LoadMyFontIntoWindowsFont(MDDef.DBtableTitleFont,DBGrid1.TitleFont);
end;


procedure Tdbtablef.Fieldcorrelations1Click(Sender: TObject);
label
   Cleanup;
var
   i,j,NumPoints : integer;
   VarCovar,Correlations  : ^tTrendMatrix;
   OldFilter : String;
   UseFields : tStringList;
   GridForm : TGridForm;
begin
   UseFields := GISdb[DBonTable].GetAnalysisFields;
   {$IfDef RecordCorrelationMatrix} WriteLineToDebugFile('Tdbtablef.Fieldcorrelations1Click, n=' + IntToStr(UseFields.Count)); {$EndIf}
   ShowHourglassCursor;
   GISdb[DBonTable].EmpSource.Enabled := false;
   OldFilter := GISdb[DBonTable].MyData.Filter;
   New(VarCovar);
   New(Correlations);
   DataBaseCorrelations(DBonTable,UseFields,VarCovar^,Correlations^,NumPoints);
   {$IfDef RecordCorrelationMatrix} WriteLineToDebugFile('DataBaseCorrelations done'); {$EndIf}

   if (Sender = Fieldcorrelations1) then begin
     if (NumPoints = 0) then MessageToContinue('No complete records in DB')
     else begin
        GridForm := TGridForm.Create(Application);
        GridForm.Caption := 'Correlations, ' + GISdb[DBonTable].dbName + '   n=' + IntToStr(NumPoints);
        GridForm.StringGrid1.RowCount := succ(UseFields.Count);
        GridForm.StringGrid1.ColCount := succ(UseFields.Count);
        GridForm.NumVar := UseFields.Count;
        {$IfDef RecordCorrelationMatrix} WriteLineToDebugFile('GridForm created'); {$EndIf}

         for i := 1 to UseFields.Count do begin
            GISdb[DBonTable].EmpSource.Enabled := false;
            GridForm.StringGrid1.Cells[0,i] := UseFields[pred(i)];
            GridForm.StringGrid1.Cells[i,0] := UseFields[pred(i)];
            GridForm.FieldNames[i] := UseFields[pred(i)];
            for j := 1 to UseFields.Count do begin
               GridForm.r[i,j] := Correlations^[i,j];
               GridForm.StringGrid1.Cells[i,j] := RealToString(GridForm.r[i,j],10,4);
            end {for j};
         end {for i};
      end;
   end;

   if (Sender = Statisticsgroupedbyonefield1) then begin
      GISdb[DBonTable].dbOpts.MainFilter := OldFilter;
      GISdb[DBonTable].AssembleGISFilter;
   end;
  Cleanup:;
   {$IfDef RecordCorrelationMatrix} WriteLineToDebugFile('Cleanup begun'); {$EndIf}
   ShowStatus;
   Dispose(VarCovar);
   Dispose(Correlations);
   UseFields.Free;
   {$IfDef RecordCorrelationMatrix} WriteLineToDebugFile('Tdbtablef.Fieldcorrelations1Click out'); {$EndIf}
end;


procedure Tdbtablef.N36Click(Sender: TObject);
begin
   if GISdb[DBonTable].aShapeFile <> Nil then GISdb[DBonTable].aShapeFile.RepairBoundingBoxInSHPandSHXfiles;
end;



procedure Tdbtablef.N3Dshapefileprofile1Click(Sender: TObject);
begin
   Do3Dshapefileprofile1Click(Sender);
end;

function Tdbtablef.Do3Dshapefileprofile1Click(Sender: TObject) : tThisBaseGraph;
var
   i : integer;
   Dist,Az,
   Lat,Long,LastLat,LastLong : float64;
   rf,rf2 : file;
   TStr : shortstring;
   v : array[1..2] of float32;
begin
   {$IfDef RecordTerrainProfiles} WriteLineToDebugFile('N3Dshapefileprofile1Click'); {$EndIf}
   with GISdb[DBonTable] do begin
      if ShapeFileType in [13,15,23,25] then aShapeFile.GetLineCoords(MyData.RecNo,true)
      else aShapeFile.GetLineCoordsAndZsFromDEM(TheMapOwner.MapDraw.DEMonMap,MyData.RecNo);
      {$IfDef RecordTerrainProfiles} WriteLineToDebugFile('Profile rec start=' + IntToStr(MyData.RecNo) + ' CurrentLinePartSize[1]=' + IntToStr(aShapeFile.CurrentLinePartSize[1])); {$EndIf}

       Result := TThisBaseGraph.Create(Application);
       Result.SetUpGraphForm;
       if (Sender = N3dshapefileprofileLatitude1) then Result.GraphDraw.HorizLabel := 'Latitude'
       else if (Sender = N3DshapefileprofileLongitude1) then Result.GraphDraw.HorizLabel := 'Longitude'
       else Result.GraphDraw.HorizLabel := 'Distance (km)';
       TStr := '';
       if (dbOpts.LabelField <> '') then TStr := GISdb[DBonTable].MyData.GetFieldByNameAsString(dbOpts.LabelField);

       Result.Caption := 'Profile ' + TStr;
       Result.OpenDataFile(rf);

       if (Sender = CompareshapefileandDEMprofiles1) then Result.OpenPointFile(rf2,Result.Symbol);

      for i := 0 to pred(aShapeFile.CurrentPolyLineHeader.NumPoints) do begin
         Lat := aShapeFile.CurrentLineCoords^[i].Lat;
         Long := aShapeFile.CurrentLineCoords^[i].Long;
         if (Sender = N3dshapefileprofileLatitude1) then  v[1] := Lat
         else if (Sender = N3DshapefileprofileLongitude1) then v[1] := Long
         else begin
            if (i=0) then v[1] := 0
            else begin
                VincentyCalculateDistanceBearing(Lat,Long,LastLat,LastLong,Dist,Az);
                v[1] := v[1] + 0.001 * Dist;
            end;
            LastLat := Lat;
            LastLong := Long;
         end;
         v[2] := aShapeFile.CurrentLineZs^[i];
         BlockWrite(rf,v,1);
         if (Sender = CompareshapefileandDEMprofiles1) then begin
            if DEMGlb[TheMapOwner.MapDraw.DEMonMap].GetElevFromLatLongDegree(Lat,Long,v[2]) then begin
               BlockWrite(rf2,v,1);
            end;
         end;
      end;
      CloseFile(rf);
      if (Sender = CompareshapefileandDEMprofiles1) then CloseFile(rf2);
      Result.GraphDraw.ShowLine[1] := true;
      Result.AutoScaleAndRedrawDiagram(true,true,false,false);
      {$IfDef RecordTerrainProfiles} WriteLineToDebugFile('Profile rec  done=' + IntToStr(MyData.RecNo) + ' CurrentLinePartSize[1]=' + IntToStr(aShapeFile.CurrentLinePartSize[1])); {$EndIf}
   end;
end;

procedure Tdbtablef.N3dshapefileprofileLatitude1Click(Sender: TObject);
begin
  Do3Dshapefileprofile1Click(Sender);
end;

procedure Tdbtablef.N3DshapefileprofileLongitude1Click(Sender: TObject);
begin
   Do3Dshapefileprofile1Click(Sender);
end;

procedure Tdbtablef.N3Dslicer1Click(Sender: TObject);
{$ifDef ExPointCloud}
begin
{$Else}
var
   fName : PathStr;
   MPC : tMemoryPointCloud;
begin
  if GISdb[DBonTable].ItsAShapeFile then  begin
    fName := ChangeFileExt(GISdb[DBonTable].dbFullName,'.shp');
     if FileExists(fName) then DEMDefs.VasaProjectFName := fName
     else DEMDefs.VasaProjectFName := GISdb[DBonTable].dbFullName;
     Slicer_3d.DB_3dSlices(GISdb[DBonTable].theMapOwner, Nil,GISdb[DBonTable]);
  end
  else begin
     MPC := tMemoryPointCloud.Create(GISdb[DBonTable].dbFullName, Nil);
     DB_3dSlices(Nil,MPC);
  end;
  {$EndIf}
end;

procedure Tdbtablef.N1degreetilestocoverrecordsintable1Click(Sender: TObject);
var
   Lat,Long : float64;
   SWcorners : tstringlist;
begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('N1degreetilestocoverrecordsintable1Click in'); {$EndIf}
   GISdb[DBonTable].EmpSource.Enabled := false;
   ShowHourglassCursor;
   SWcorners := tstringlist.Create;
   SWcorners.Duplicates := dupIgnore;
   SWcorners.Sorted := true;
   GISdb[DBonTable].MyData.First;
   while not GISdb[DBonTable].MyData.eof do begin
      if GISdb[DBonTable].GetLatLongToRepresentRecord(Lat,Long) then
         SWCorners.Add(SWcornerString(Lat,Long,1));
      GISdb[DBonTable].MyData.Next;
   end;
   ShowInNotepadPlusPlus(SWcorners,'SW_corners_of_1_degree_tiles_needed_to_cover_the_database');
   ShowStatus;
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('N1degreetilestocoverrecordsintable1Click out'); {$EndIf}
end;


procedure Tdbtablef.N22Click(Sender: TObject);
begin
   AllOptionsForFans := true;
end;

procedure Tdbtablef.N2Dgraph1Click(Sender: TObject);
begin
   {$IfDef NoDBGrafs}
   {$Else}
      GISdb[DBonTable].MakeGraph(dbgtN2Dgraph1);
   {$EndIf}
end;


procedure Tdbtablef.N2Dgraph2series1Click(Sender: TObject);
begin
   {$IfDef NoDBGrafs}
   {$Else}
      GISdb[DBonTable].MakeGraph(dbgtN2Dgraph2series1);
   {$EndIf}
end;

procedure Tdbtablef.N2Dgraph2yaxes1Click(Sender: TObject);
begin
   {$IfDef NoDBGrafs}
   {$Else}
      GISdb[DBonTable].MakeGraph(dbgtN2Dgraph2yaxes1);
   {$EndIf}
end;

procedure Tdbtablef.N2Dgraphallopendatabases1Click(Sender: TObject);
var
  i,j,n : integer;
  TStr : ShortString;
  rFile : file;
  v : array[1..3] of float32;
  ThisGraph : TThisbasegraph;
  xf,yf : string16;
   Words : tStringList;
begin
   {$IfDef RecordGraph} WriteLineToDebugFile('Tdbtablef.N2Dgraphallopendatabases1Click in'); {$EndIf}
   //with GISdb[DBonTable] do begin
     if (Sender = N2Dgraphallopendatabases1) or (Sender = N2Dgraphallopendatabaseslines1) then begin
        GISdb[DBonTable].PickNumericFields(dbgtUnspecified,2,'X axis variable','Y axis variable','');
     end;

     ThisGraph := TThisbasegraph.Create(Application);
     xf := GISdb[DBonTable].dbOpts.XField;
     yf := GISdb[DBonTable].dbOpts.yField;
     ThisGraph.GraphDraw.HorizLabel := GISdb[DBonTable].dbOpts.XField;
     ThisGraph.GraphDraw.VertLabel := GISdb[DBonTable].dbOpts.YField;
     ThisGraph.Caption := GISdb[DBonTable].dbOpts.XField + ' vs ' + GISdb[DBonTable].dbOpts.YField;
     ThisGraph.SetUpGraphForm;
     ThisGraph.GraphDraw.LegendList := tStringList.Create;
     if (Sender = N2Dgraphallopendatabaseslines1) then begin
        for i := 1 to 255 do begin
           ThisGraph.GraphDraw.LineSize256[i] := 2;
           if (i < 15) then ThisGraph.GraphDraw.Symbol[i].Size := 0;
        end;
     end;
   //end;

   Words := tStringList.Create;
   Words.Add('NAME,PLOT,GRAY,LINE_WIDTH,LINE_COLOR,FILENAME');

     ShowHourglassCursor;
     for j := 1 to MaxDataBase do if (GISdb[j] <> Nil) then begin
        if GISdb[j].MyData.FieldExists(XF) and GISdb[j].MyData.FieldExists(YF) then begin
            {$IfDef RecordGraph} WriteLineToDebugFile(GISDB[j].DBName); {$EndIf}
            GISdb[j].MyData.First;
            GISdb[j].EmpSource.Enabled := false;
            ThisGraph.GraphDraw.LegendList.Add(GISdb[j].dbName);
            if (Sender = N2Dgraphallopendatabases1) and GISdb[j].ColorPresent then begin
               v[3] := GISdb[j].MyData.tColorFromTable;
               ThisGraph.OpenXYColorFile(rfile);
               TStr := ThisGraph.GraphDraw.XYColorFilesPlotted.Strings[pred(ThisGraph.GraphDraw.XYColorFilesPlotted.Count)];
            end
            else if (Sender = N2Dgraphallopendatabases1) then begin
               ThisGraph.Symbol.Color := GISdb[j].dbOpts.LineColor;
               ThisGraph.Symbol.DrawingSymbol := GISdb[j].dbOpts.Symbol.DrawingSymbol;
               ThisGraph.OpenPointFile(rfile,ThisGraph.Symbol);
            end
            else if (Sender = N2Dgraphallopendatabaseslines1) then begin
               ThisGraph.OpenDataFile(rfile,ConvertPlatformColorToTColor(GISdb[j].dbOpts.LineColor));
            end
            else begin
               ThisGraph.OpenDataFile(rfile);
               TStr := ThisGraph.GraphDraw.DataFilesPlotted.Strings[pred(ThisGraph.GraphDraw.DataFilesPlotted.Count)];
               ThisGraph.GraphDraw.FileColors256[ThisGraph.GraphDraw.DataFilesPlotted.Count] := GISdb[j].MyData.PlatformColorFromTable;
            end;

            n := 0;
            repeat
               if GISdb[j].GetFloat32FromTableLinkPossible(XF,v[1]) and
                    GISdb[j].GetFloat32FromTableLinkPossible(YF,v[2]) then begin
                    BlockWrite(Rfile,v,1);
                    inc(n);
               end;
               GISdb[j].MyData.Next;
            until GISdb[j].MyData.EOF;
            CloseFile(rfile);
            GISdb[j].EmpSource.Enabled := true;
            Words.Add(GISdb[j].DBName + ',Y,N,' + IntToStr(2) + ',' +  IntToStr(GISdb[j].MyData.tColorFromTable) + ',' + TStr);
        end;
     end;
    {$IfDef RecordGraph} WriteLineToDebugFile('looped'); {$EndIf}
    if (ThisGraph.GraphDraw.XYColorFilesPlotted.Count > 0) or (ThisGraph.GraphDraw.DataFilesPlotted.Count > 0) then begin
        ThisGraph.AutoScaleAndRedrawDiagram;
    end
    else begin
       ThisGraph.Close;
    end;
    ShowStatus;
    {$IfDef RecordGraph} WriteLineToDebugFile('out'); {$EndIf}
end;


procedure Tdbtablef.N2Dgraphallopendatabaseslines1Click(Sender: TObject);
begin
   N2Dgraphallopendatabases1Click(Sender);
end;

procedure Tdbtablef.Heatmap1Click(Sender: TObject);
begin
   GISdb[DBonTable].TheMapOwner.MakeHeatMap(dbOnTable);
end;


procedure Tdbtablef.HelpBtnClick(Sender: TObject);
begin
   if VATEdit then DisplayHTMLTopic('html\vat_grid_disp.htm')
   else DisplayHTMLTopic('html\database_table_display.htm');
end;


procedure Tdbtablef.Longeststring1Click(Sender: TObject);
var
   Max : integer;
   Ls,FieldName : ShortString;
begin
   //with GISdb[DBonTable] do begin
      if (Sender = LongestString2) then FieldName := SelectedColumn
      else FieldName := GISdb[DBonTable].PickField('longest string',[ftString]);
      if (FieldName <> '') then begin
         GISdb[DBonTable].LongestString(FieldName,Max,LS);
         ShowStatus;
         MessageToContinue('Max length field ' + FieldName + ': ' + IntToStr(Max) + '   (' + ls + ')');
     end;
  // end;
end;


procedure Tdbtablef.Longeststring2Click(Sender: TObject);
begin
   Longeststring1Click(Sender);
end;


procedure Tdbtablef.LOSButtonClick(Sender: TObject);
var
   Ending,Azimuth,Lat2,Long2 : float64;
   wf : tWeaponsFan;
begin
   wF := WeaponsTableBasicParametersToFan(GISdb[DBonTable].TheMapOwner.MapDraw.PrimMapProj,GISdb[DBonTable].MyData);
   if (wf.StartAngle > wf.EndAngle) then Ending := 360 + wf.EndAngle
   else Ending := wf.EndAngle;
   Azimuth :=  PetMath.FindCompassAngleInRange(0.5 * (Ending + wf.StartAngle));
   VincentyPointAtDistanceBearing(wf.W_Lat,wf.W_Long,wf.W_Range,Azimuth,Lat2,Long2);
   StartLOS(True,JustWandering,1,wf.W_Lat,wf.W_Long,Lat2,Long2,DEMGlb[1].SelectionMap,true);
end;


procedure Tdbtablef.LOSterrainprofile1Click(Sender: TObject);
begin
    LOStopoprofile1Click(Sender);
end;

procedure Tdbtablef.LOStopoprofile1Click(Sender: TObject);
var
   Lat,Long,Lat2,Long2 : float64;
begin
   if GISdb[DBonTable].ValidLatLongFromTable(Lat,Long) and GISdb[DBonTable].ValidLat2Long2FromTable(Lat2,Long2) then begin
      if GISdb[DBonTable].MyData.FieldExists('SENSOR_UP') then
         MDDef.ObsAboveGround := GISdb[DBonTable].MyData.GetFieldByNameAsFloat('SENSOR_UP');
      if GISdb[DBonTable].MyData.FieldExists('TARGET_UP') then
         MDDef.TargetAboveGround := GISdb[DBonTable].MyData.GetFieldByNameAsFloat('TARGET_UP');
      StartLOS(True,JustWandering,GISdb[DBonTable].TheMapOwner.MapDraw.DEMonMap,Lat,Long,Lat2,Long2,GISdb[DBonTable].theMapOwner,true);
   end;
end;

procedure Tdbtablef.LVISslices1Click(Sender: TObject);
begin
    {$IfDef ExPointCloud}
    {$Else}
    DEMDefs.VasaProjectFName := GISdb[DBonTable].DBFullName;
    Slicer_3D.DB_3dSlices(GISdb[DBonTable].theMapOwner,Nil,Nil,'X_UTM','Y_UTM','ZT','ZT');
    SlicerForm.LoadMemoryPointCloud(2,GISdb[DBonTable].DBFullName,'X_UTM','Y_UTM','ZG','ZG');
    {$EndIf}
end;

procedure Tdbtablef.LVISslicescanopy1Click(Sender: TObject);
begin
    {$IfDef ExPointCloud}
    {$Else}
       DEMDefs.VasaProjectFName := GISdb[DBonTable].DBFullName;
       Slicer_3D.DB_3dSlices(GISdb[DBonTable].theMapOwner,Nil,Nil,'X_UTM','Y_UTM','RH100','RH100');
       SlicerForm.LoadMemoryPointCloud(2,GISdb[DBonTable].DBFullName,'X_UTM','Y_UTM','RH75','RH75');
       SlicerForm.LoadMemoryPointCloud(3,GISdb[DBonTable].DBFullName,'X_UTM','Y_UTM','RH50','RH50');
       SlicerForm.LoadMemoryPointCloud(4,GISdb[DBonTable].DBFullName,'X_UTM','Y_UTM','RH10','RH10');
    {$EndIf}
end;

procedure Tdbtablef.ColorcodebyDBstringfield1Click(Sender: TObject);
begin
   GISdb[DBonTable].GISProportionalSymbols(dbasColorByString);
end;

procedure Tdbtablef.Colorcodebynumericfield1Click(Sender: TObject);
begin
   GISdb[DBonTable].GISProportionalSymbols(dbasColorByNumeric);
end;

procedure Tdbtablef.Sigmatee1Click(Sender: TObject);
{$IfDef ExOceanography}
begin
{$Else}
var
   depth,salinity,temp : float64;
begin
   with GISdb[DBonTable] do begin
      if (Sender = Sigmatee1) then AddFieldToDataBase(ftFloat,'SIGMA_TEE',12,6)
      else if (Sender = SigmaTheta1) then AddFieldToDataBase(ftFloat,'SIGMA_THT',12,6)
      else AddFieldToDataBase(ftFloat,'SOUND_VEL',8,3);
      EmpSource.Enabled := false;
      GISdb[DBonTable].MyData.First;
      while not GISdb[DBonTable].MyData.eof do begin
         Depth := GISdb[DBonTable].MyData.GetFieldByNameAsFloat('DEPTH');
         Temp := GISdb[DBonTable].MyData.GetFieldByNameAsFloat('TEMP_C');
         Salinity := GISdb[DBonTable].MyData.GetFieldByNameAsFloat('SALINITY');
         GISdb[DBonTable].MyData.Edit;
         if Sender = Sigmatee1 then GISdb[DBonTable].MyData.SetFieldByNameAsFloat('SIGMA_TEE',OceanCal.CalSigmaT(Temp,Salinity))
         else if Sender = SigmaTheta1 then GISdb[DBonTable].MyData.SetFieldByNameAsFloat('SIGMA_THT',OceanCal.CalSigmaTheta(Temp,Salinity,Depth))
         else GISdb[DBonTable].MyData.SetFieldByNameAsFloat('SOUND_VEL',OceanCal.SoundVelocity(Temp,Salinity,Depth));
         GISdb[DBonTable].MyData.Next;
      end;
      ShowStatus;
   end;
{$EndIf}   
end;

procedure Tdbtablef.Sigmatheta1Click(Sender: TObject);
begin
   Sigmatee1Click(Sender);
end;

procedure Tdbtablef.Singlefield1Click(Sender: TObject);
begin
   Countuniquevalues1Click(Sender);
end;


procedure Tdbtablef.Accumulatedcostsurface1Click(Sender: TObject);
begin
   LeastCostFromCurrentRecord(DBonTable);
end;


procedure Tdbtablef.Addaverageelevationinwindow1Click(Sender: TObject);
begin
   GISdb[DBonTable].AddAndFillFieldFromDEM(adAvgElevInWindow);
end;

procedure Tdbtablef.Addazimuthtotravelpath1Click(Sender: TObject);
begin
   {$IfDef RecordNavigation} WriteLineToDebugFile('Tdbtablef.Addazimuthtotravelpath1Click in'); {$EndIf}
   with GISdb[DBonTable] do begin
      GetSpeedDistOptions;
      GISdb[DBonTable].AddNavFields;
   end;
   {$IfDef RecordNavigation} WriteLineToDebugFile('Tdbtablef.Addazimuthtotravelpath1Click out'); {$EndIf}
end;


procedure Tdbtablef.AddBeachBallIcons1Click(Sender: TObject);
var
   sBitmap : tMyBitmap;
   fName : PathStr;
begin
{$IfDef ExGeology}
{$Else}
   GISdb[DBonTable].EmpSource.Enabled := false;
   if (Sender <> QuickKMLexport1) then Beach_ball_options.SetBeachBallOptions(true);
   //Dipandstrikes1Click(Sender);
   if not GISdb[DBonTable].IconPresent then begin
      GISdb[DBonTable].AddIcon(false);
      GISdb[DBonTable].IconFieldNames[1] := 'ICON';
      GISdb[DBonTable].IconPresent := true;
   end;

   GISdb[DBonTable].MyData.First;
   while not GISdb[DBonTable].MyData.eof do begin
      sBitmap := GISdb[DBonTable].DrawFocalMechanism(GISdb[DBonTable].QuakeRadius);
      fName := NextFileNumber(MainMapData + 'Icons\','t_beachball_','.png');
      Petimage.SaveBitmap(sBitmap,fName);
      //sBitmap.SaveToFile(fName);
      GISdb[DBonTable].MyData.Edit;
      GISdb[DBonTable].MyData.SetFieldByNameAsString('ICON',ExtractFileName(fName));
      GISdb[DBonTable].MyData.Next;
   end;

   GISdb[DBonTable].ExportToKML(false,false);
   ShowStatus;
{$EndIf}
end;


procedure Tdbtablef.Addboundingbox1Click(Sender: TObject);
begin
   GISdb[DBonTable].AddBoundingBox;
end;


procedure Tdbtablef.AddcolorsforFULLU120U80U101Click(Sender: TObject);
begin
   AddColorsForUnderDBs(DBonTable);
end;

procedure Tdbtablef.Addconstanttofield1Click(Sender: TObject);
begin
   SingleFieldArithmetic(DBonTable,sfaAdd,'');
end;


procedure Tdbtablef.Addconstanttofield2Click(Sender: TObject);
begin
   SingleFieldArithmetic(DBonTable,sfaAdd,SelectedColumn);
end;

procedure Tdbtablef.Addconstanttofield3Click(Sender: TObject);
begin
   SingleFieldArithmetic(DBonTable,sfaMult,SelectedColumn);
end;

procedure Tdbtablef.AddCOPALOSpercentprimarydata1Click(Sender: TObject);
begin
   AddPercentPrimaryData(DBonTable);
end;

procedure Tdbtablef.AddDBsfieldrange1Click(Sender: TObject);
var
   i : integer;
   WantedFieldName : ShortString;
   Results : tStringList;
   Min,Max : float64;
   fName : PathStr;
begin
   fName := 'Z';
   WantedFieldName := GISdb[DBonTable].PickField('FFTs',NumericFieldTypes);
   Results := tStringList.Create;
   Results.Add(WantedFieldName + ',MIN,MAX');
   for i := 1 to MaxDataBase do begin
      if (GISdb[i] <> nil) then begin
         GISdb[i].FieldRange(WantedFieldName,Min,Max);
         Results.Add(GISdb[i].dbName + ',' + RealToString(Min,-12,-4) + ',' + RealToString(Max,-12,-4) );
      end;
   end;
   fName := Petmar.NextFileNumber(MDTempDir,'field_range_','.csv');
   Results.SaveToFile(fname);
   Results.Free;
   OpenMultipleDataBases(fname);
end;


procedure Tdbtablef.Adddecimaldegreesstring1Click(Sender: TObject);
begin
   AddUTMcoordfields1Click(Sender);
end;

procedure Tdbtablef.Adddecimalminutesstring1Click(Sender: TObject);
begin
   AddUTMcoordfields1Click(Sender);
end;

procedure Tdbtablef.Adddecimalsecondsstring1Click(Sender: TObject);
begin
   AddUTMcoordfields1Click(Sender);
end;

procedure Tdbtablef.AddDEMdata1Click(Sender: TObject);
begin
    AddGlobalDEMs(dbOnTable);
end;

procedure Tdbtablef.AddDEMIXtilecentroid1Click(Sender: TObject);
var
   WantField,tName : shortstring;
   Lat,Long : float32;
begin
   WantField := GISdb[DBonTable].PickField('DEMIX file names',[ftstring]);
   if (WantField <> '') then begin
      ShowHourglassCursor;
      GISdb[DBonTable].AddLatLong;
      GISdb[DBonTable].EmpSource.Enabled := false;
      GISdb[DBonTable].MyData.First;
      while not GISdb[DBonTable].MyData.eof do begin
         tName := GISdb[DBonTable].MyData.GetFieldByNameAsString(WantField);
         DEMIXtileCentroid(tName,Lat,Long);
         GISdb[DBonTable].MyData.Edit;
         GISdb[DBonTable].MyData.SetFieldByNameAsFloat('LAT',0.001 * round(1000 * Lat));
         GISdb[DBonTable].MyData.SetFieldByNameAsFloat('LONG',0.001 * round(1000 * Long));
         GISdb[DBonTable].MyData.Next;
      end;
      ShowStatus;
   end;
end;

procedure Tdbtablef.AddDISTANCEAZIMUTHfields1Click(Sender: TObject);
var
   Lat,Long,Lat2,Long2,Distance,Bearing : float64;
begin
   with GISdb[DBonTable] do begin
      AddFieldToDataBase(ftFloat,'DIST_KM',12,4);
      AddFieldToDataBase(ftFloat,'AZIMUTH',6,2);
      ShowHourglassCursor;
      EmpSource.Enabled := false;
      GISdb[DBonTable].MyData.First;
      while not GISdb[DBonTable].MyData.EOF do begin
         if ValidLatLongFromTable(Lat,Long) and ValidLat2Long2FromTable(Lat2,Long2) then begin
            VincentyCalculateDistanceBearing(Lat,Long,Lat2,Long2,Distance,Bearing);
            GISdb[DBonTable].MyData.Edit;
            GISdb[DBonTable].MyData.SetFieldByNameAsFloat('AZIMUTH',Bearing);
            GISdb[DBonTable].MyData.SetFieldByNameAsFloat('DIST_KM',0.001 * Distance);
         end;
         GISdb[DBonTable].MyData.Next;
      end;
      ShowStatus;
   end;
end;

procedure Tdbtablef.AddEGMfields1Click(Sender: TObject);
begin
    AddEGMfields(dbOnTable);
end;

procedure Tdbtablef.AddEGMfieldsfromalgorithms1Click(Sender: TObject);
begin
   AddEGMtoDBfromSphHarmonics(DBonTable,true);
   AddEGMtoDBfromSphHarmonics(DBonTable,false);
   ShowStatus;
end;

procedure Tdbtablef.AddelevationdifferencefromDEM1Click(Sender: TObject);
begin
   GISdb[DBonTable].AddAndFillFieldFromDEM(adElevDiff);
end;


procedure Tdbtablef.AddelevationfromDEM1Click(Sender: TObject);
begin
   GISdb[DBonTable].AddAndFillFieldFromDEM(adElevInterp);
end;


procedure Tdbtablef.AddelevationfromDEMseries1Click(Sender: TObject);
var
   z : float32;
   Lat,Long : float64;
   WantedDEM : integer;
   DEMSeries : ShortString;
begin
   with GISdb[DBonTable] do begin
      PickDEMSeries(DEMSeries,'DEM series for elevations');
      ShowHourglassCursor;
      GISdb[DBonTable].ClearGISFilter;
      FillFieldWithValue('ELEV_M','-9999');
      {$IfDef RecordFillDEM} WriteLineToDebugFile('Tdbtablef.AddelevationfromDEMseries1Click  Total Records=' + IntToStr(GISDataBase[DBonTable].MyData.RecordCount)); {$EndIf}
      repeat
         EmpSource.Enabled := false;
         if ValidLatLongFromTable(Lat,Long) then begin
             WantedDEM := LoadMapLibraryPoint(true,Lat,Long,DEMSeries,false);
             if ValidDEM(WantedDEM) then begin
                {$IfDef RecordFillDEM} WriteLineToDebugFile(DEMGlb[WantedDEM].AreaName); {$EndIf}
                //th DEMGlb[WantedDEM] do begin
                    GISdb[DBonTable].MyData.ApplyFilter(MakeGeoFilterFromBoundingBox(DEMGlb[WantedDEM].DEMBoundBoxGeo));

                    //LatFieldName +  '<=' + RealToString(DEMBoundBoxGeo.YMax,-12,-6) + ' AND ' + LatFieldName +  '>=' + RealToString(DEMBoundBoxGeo.YMin,-12,-6) + ' AND ' +
                                      //ngFieldName +  '<=' + RealToString(DEMBoundBoxGeo.XMax,-12,-6) + ' AND ' + LongFieldName +  '>=' + RealToString(DEMBoundBoxGeo.XMin,-12,-6));
                //d;
                {$IfDef RecordFillDEM} WriteLineToDebugFile('Recs check=' + IntToStr(GISDataBase[DBonTable].MyData.RecordCount) + '  Filter=' +  GISDataBase[DBonTable].MyData.Filter); {$EndIf}
                while not GISdb[DBonTable].MyData.eof do begin
                   EmpSource.Enabled := false;
                   GISdb[DBonTable].MyData.Edit;
                   if GetLatLongToRepresentRecord(Lat,Long) and DEMGlb[WantedDEM].GetElevFromLatLongDegree(Lat,Long,z) then begin
                      GISdb[DBonTable].MyData.SetFieldByNameAsFloat('ELEV_M',z);
                   end
                   else GISdb[DBonTable].MyData.SetFieldByNameAsString('ELEV_M','');
                   GISdb[DBonTable].MyData.Next;
                end;
                CloseSingleDEM(WantedDEM);
             end;
         end
         else begin
             GISdb[DBonTable].MyData.Edit;
             GISdb[DBonTable].MyData.SetFieldByNameAsString('ELEV_M','');
             GISdb[DBonTable].MyData.Next;
         end;
         GISdb[DBonTable].MyData.ApplyFilter('ELEV_M = -9999');
         {$IfDef RecordFillDEM} WriteLineToDebugFile('Records left=' + IntToStr(GISDataBase[DBonTable].MyData.RecordCount)); {$EndIf}
     until GISdb[DBonTable].MyData.RecordCount = 0;
     ShowStatus;
   end;
end;

procedure Tdbtablef.AddfieldfromopenDB1Click(Sender: TObject);
var
   OtherDB,i,ClosestRecNo : integer;
   Lat,Long,Distance,MaxSep : float64;
   WantedFieldName : ANSIstring;
begin
   {$IfDef RecordEditDB} WriteLineToDebugFile('Tdbtablef.AddfieldfromopenDB1Click in Recs=' + IntToStr(GISdb[DBonTable].MyData.RecordCount)); {$EndIf}
   OtherDB := PickOpenGISDataBase('Add field from DB',DBonTable,1);
   MaxSep := 1000;
   ReadDefault('max separation (m)',MaxSep);
   if (OtherDB <> 0) then begin
      WantedFieldName := GISdb[OtherDB].PickField('Field add',[ftString,ftInteger,ftFloat]);
      if WantedFieldName <> '' then begin
         GISdb[DBonTable].AddFieldToDataBase(GISdb[OtherDB].MyData.GetFieldType(WantedFieldName),WantedFieldName,GISdb[OtherDB].MyData.GetFieldLength(WantedFieldName),GISdb[OtherDB].MyData.GetFieldPrecision(WantedFieldName));
         StartProgressAbortOption(WantedFieldName);
         i := 0;
         GISdb[DBonTable].EmpSource.Enabled := false;
         GISdb[DBonTable].MyData.First;
         while not GISdb[DBonTable].MyData.eof do begin
            inc(i);
            if (i mod 250 = 0) then begin
               GISdb[DBonTable].EmpSource.Enabled := false;
               UpdateProgressBar(i/GISdb[DBonTable].MyData.FiltRecsInDB);
            end;
            if (GISdb[DBonTable].MyData.GetFieldByNameAsString(WantedFieldName) <> '') and GISdb[DBonTable].ValidLatLongFromTable(Lat,Long) then begin
               GISdb[OtherDB].FindClosestRecord(Lat,Long,ClosestRecNo,Distance);
               if (ClosestRecNo <> 0) and (Distance < MaxSep) then begin
                  GISdb[DBonTable].MyData.Edit;
                  GISdb[DBonTable].MyData.SetFieldByNameAsString(WantedFieldName,GISdb[OtherDB].MyData.GetFieldByNameAsString(WantedFieldName));
               end;
            end;
            GISdb[DBonTable].MyData.Next;
            if WantOut then break;
         end;
      end;
      ShowStatus;
   end;
   {$IfDef RecordEditDB} WriteLineToDebugFile('Tdbtablef.AddfieldfromopenDB1Click out'); {$EndIf}
end;


procedure Tdbtablef.Addfileextension1Click(Sender: TObject);
begin
   Removelowercasecharacters1Click(Sender);
end;

procedure Tdbtablef.Addfontdefinition1Click(Sender: TObject);
begin
   GISdb[DBonTable].AddFieldToDataBase(ftString,'FONT_NAME',36,0);
   GISdb[DBonTable].AddFieldToDataBase(ftInteger,'FONT_SIZE',3,0);
   GISdb[DBonTable].AddFieldToDataBase(ftInteger,'ROT_ANGLE',3,0);
   GISdb[DBonTable].AddFieldToDataBase(ftInteger,'FONT_COLOR',10,0);
   GISdb[DBonTable].AddFieldToDataBase(ftString,'FONT_BOLD',1,0);
   GISdb[DBonTable].AddFieldToDataBase(ftString,'FONT_ITAL',1,0);
   GISdb[DBonTable].AddFieldToDataBase(ftString,'FONT_UNDER',1,0);
   GISdb[DBonTable].AddFieldToDataBase(ftInteger,'PLACE_TEXT',1,0);
   GISdb[DBonTable].FontFieldExists := true;
   Addfontdefinition1.Visible := false;
   if AnswerIsYes('Fill font field') then Fillfontfield1Click(Sender);
   ShowStatus;
end;


{ Returns a count of the number of occurences of SubText in Text }
function CountOccurences( const SubText: ShortString; const Text: Shortstring): Integer;
//from http://stackoverflow.com/questions/15294501/how-to-count-number-of-occurrences-of-a-certain-char-in-string
begin
  Result := Pos(SubText, Text);
  if Result > 0 then Result := (Length(Text) - Length(StringReplace(Text, SubText, '', [rfReplaceAll]))) div  Length(subtext);
end;  { CountOccurences }


procedure Tdbtablef.Addcountfromsubstring1Click(Sender: TObject);
begin
   Addfromsubstring1Click(Sender);
end;


procedure Tdbtablef.AddcountryareafieldstoDB1Click(Sender: TObject);
begin
   //AddCountryAreaToDB(DBonTable);
end;

procedure Tdbtablef.Addfromsubstring1Click(Sender: TObject);
var
   aString,bString,aSubstring : shortstring;
   NewField : shortstring;
   i : integer;
begin
   with GISdb[DBonTable] do begin
      SelectedColumn := PickField('field to search for sub-string',[ftString]);
      if (SelectedColumn <> '') then begin
          NewField := 'FLAG';
          GetString('new flag field',NewField,true,DBaseFieldNameChars);
          asubString := NewField;
          GetString('sub-string to search for',asubString,false,ReasonableTextChars);
          if (aSubString <> '') then begin
             aSubString := UpperCase(asubString);
             if (Sender = Addfromsubstring1) then GISdb[DBonTable].AddFieldToDatabase(ftString,NewField,2)
             else GISdb[DBonTable].AddFieldToDatabase(ftInteger,NewField,4);
              EmpSource.Enabled := false;
              GISdb[DBonTable].MyData.First;
              while not GISdb[DBonTable].MyData.eof do begin
                 aString := UpperCase(MyData.GetFieldByNameAsString(SelectedColumn));
                 if StrUtils.AnsiContainsText(aString,asubstring) then begin
                    GISdb[DBonTable].MyData.Edit;
                    if (Sender = Addfromsubstring1) then begin
                       bString := 'Y';
                       GISdb[DBonTable].MyData.SetFieldByNameAsString(NewField,aString);
                    end
                    else begin
                       i := CountOccurences(asubstring,aString);
                       GISdb[DBonTable].MyData.SetFieldByNameAsInteger(NewField,i);
                    end;
                 end;
                 GISdb[DBonTable].MyData.Next;
              end {while};
           end {if};
        end {if};
        ShowStatus;
   end {with};
end;

procedure Tdbtablef.Addicons1Click(Sender: TObject);
begin
   {$IfDef RecordIcons} WriteLineToDebugFile('Tdbtablef.Addicons1Click in'); {$EndIf}
   GISdb[DBonTable].AddIcon;
   ShowStatus;
   {$IfDef RecordIcons} WriteLineToDebugFile('Tdbtablef.Addicons1Click out'); {$EndIf}
end;


procedure Tdbtablef.AddIMAGEfieldfordifferencedistributiongraphs1Click(Sender: TObject);
begin
   with GISdb[DBonTable] do begin
      AddFieldToDataBase(ftString,'IMAGE',48);
      EmpSource.Enabled := false;
      GISdb[DBonTable].MyData.First;
      while not GISdb[DBonTable].MyData.eof do begin
         GISdb[DBonTable].MyData.Edit;
         GISdb[DBonTable].MyData.SetFieldByNameAsString('IMAGE',MyData.GetFieldByNameAsString('DEMIX_TILE') + '_difference_distrib_graphs_1.png');
         GISdb[DBonTable].MyData.Next;
      end;
      EmpSource.Enabled := true;
   end;
end;

procedure Tdbtablef.Addintegercodefield1Click(Sender: TObject);
var
   fName,NewName : ShortString;
   FieldsInDB : tStringList;
   s : string;
   Index : integer;
begin
   with GISdb[DBonTable] do begin
      fName := PickField('Field for index',[ftString]);
      if (fName = '') then exit;
      NewName := AddNewField(GISdb[DBonTable],'IDX_' + fName,ftInteger,6);
      if (NewName = '') then exit;
      DBFieldUniqueEntries(fName,FieldsInDB);
      EmpSource.Enabled := false;
      GISdb[DBonTable].MyData.First;
      while not GISdb[DBonTable].MyData.eof do begin
         s := GISdb[DBonTable].MyData.GetFieldByNameAsString(fName);
         if FieldsInDB.Find(s,Index) then begin
             GISdb[DBonTable].MyData.Edit;
             GISdb[DBonTable].MyData.SetFieldByNameAsInteger(NewName,succ(Index));
         end;
         Next;
      end;
      FieldsInDB.Free;
      ClearGISFilter;
      ShowStatus;
   end;
end;


procedure Tdbtablef.AddKoppenclass1Click(Sender: TObject);
var
   Lat,Long : float64;
   i : integer;
   aClass : shortstring;
begin
   GISdb[DBonTable].AddFieldToDataBase(ftString,'KOPPEN',4);
   GISdb[DBonTable].ClearGISFilter;
   GISdb[DBonTable].EmpSource.Enabled := false;
   GISdb[DBonTable].MyData.First;
   StartProgress('Add Koppen');
   i := 0;
   while not GISdb[DBonTable].MyData.eof do begin
      GISdb[DBonTable].EmpSource.Enabled := false;
      if (i mod 25 = 0) then UpdateProgressBar(i / GISdb[DBonTable].MyData.FiltRecsinDB);
      inc(i);
      if GISdb[DBonTable].ValidLatLongFromTable(Lat,Long) then begin
         aClass := GetKoppenClass(Lat,Long);
         GISdb[DBonTable].MyData.Edit;
         GISdb[DBonTable].MyData.SetFieldByNameAsString('KOPPEN',aClass);
      end;
      GISdb[DBonTable].MyData.Next;
   end;
   EndProgress;
   GISdb[DBonTable].ShowStatus;
end;

procedure Tdbtablef.AddLatlong1Click(Sender: TObject);
begin
   AddUTMcoordfields1Click(Sender);
end;

procedure Tdbtablef.AddlatlongfieldstoDB1Click(Sender: TObject);
var
   theFields : tStringList;
   i : integer;
begin
   {$IfDef RecordDEMIX} WriteLineToDebugFile('AddTileCharacteristics'); {$EndIf}
   theFields := tStringList.Create;
   theFields.Add('LAT');
   theFields.Add('LONG');
   theFields.Add('AREA');
   theFields.Add('COUNTRY');
   AddFieldsToDEMIXDB(DBonTable,theFields);
end;

procedure Tdbtablef.Addlatlongfromlinkeddatabase1Click(Sender: TObject);
var
   Lat,Long : float64;
   TStr : ShortString;
begin
   with GISdb[DBonTable] do begin
      if SecondLatLongFieldsPresent then begin
         if (not AnswerIsYes('LAT2/LONG2 already present; Overwrite')) then exit;
      end
      else if LatLongFieldsPresent then begin
         Lat2fName := 'LAT2';
         Long2fName := 'LONG2';
         SecondLatLongFieldsPresent := true;
         AddFieldToDataBase(ftFloat,Lat2fName,12,8);
         AddFieldToDataBase(ftFloat,Long2fName,13,8);
      end
      else begin
         AddLatLong;
      end;
      EmpSource.Enabled := false;
      GISdb[DBonTable].MyData.First;
      while not eof do begin
         TStr := GISdb[DBonTable].MyData.GetFieldByNameAsString(dbOpts.LinkFieldThisDB);
         if (TStr <> '') then begin
            WriteLineToDebugFile('Rec: ' + TStr);
            LinkTable.ApplyFilter(dbOpts.LinkFieldOtherDB + '=' + QuotedStr(TStr));
            if LinkTable.RecordCount > 0 then begin
               WriteLineToDebugFile('Located');
               if LinkTable.CarefullyGetFieldByNameAsFloat64('LAT',Lat) and LinkTable.CarefullyGetFieldByNameAsFloat64('LONG',Long) then begin
                  GISdb[DBonTable].MyData.Edit;
                  GISdb[DBonTable].MyData.SetFieldByNameAsFloat(Lat2fName,Lat);
                  GISdb[DBonTable].MyData.SetFieldByNameAsFloat(Long2fName,Long);
               end;
            end;
         end;
         Next;
      end;
      LinkTable.ApplyFilter('');
      ShowStatus;
   end;
end;


procedure Tdbtablef.Addlatlongfromshpfile1Click(Sender: TObject);
begin
   AddXYZfromshpfile1Click(Sender);
end;

procedure Tdbtablef.Addleadingzeros1Click(Sender: TObject);
var
   WantedFieldName : ShortString;
   TStr : shortstring;
   i : Integer;
begin
   with GISdb[DBonTable] do begin
     WantedFieldName := PickField('Field for leading zeros',[ftString]);
     i := succ(MyData.GetFieldLength(WantedFieldName));
     ReadDefault('Desired string length',i);
     EmpSource.Enabled := false;
     ShowHourglassCursor;
     GISdb[DBonTable].MyData.First;
     while not GISdb[DBonTable].MyData.EOF do begin
        TStr := GISdb[DBonTable].MyData.GetFieldByNameAsString(WantedFieldName);
        if Length(TStr) < i then begin
           while (Length(TStr) < i) do TStr := '0' + TStr;
           GISdb[DBonTable].MyData.Edit;
           GISdb[DBonTable].MyData.SetFieldByNameAsString(WantedFieldName,TStr);
        end;
        GISdb[DBonTable].MyData.Next;
     end;
     ShowStatus;
   end;
end;


procedure Tdbtablef.Addmaskfieldfromsubstringinmultiplefields1Click(Sender: TObject);
var
   aString,bString,fName,ch : ShortString;
   i,j : integer;
begin
   Petmar.GetString('Substring to search for',aString,false,ReasonableTextChars);
   fName := aString;
   fName := GISdb[DBonTable].GetFieldNameForDB('New Field for substrings mask',True,fName);
   GISdb[DBonTable].AddFieldToDataBase(ftString,fName,1);

   ShowHourglassCursor;
   j := 0;
   GISdb[DBonTable].MyData.First;
   StartProgress('Mask from substring');
   while not GISdb[DBonTable].MyData.eof do begin
       if (j Mod 100 = 0) then begin
          GISdb[DBonTable].EmpSource.Enabled := false;
          UpDateProgressBar(j/GISdb[DBonTable].MyData.FiltRecsInDB);
       end;
       inc(j);
       ch := 'N';
       for i := 0 to pred(GISdb[DBonTable].MyData.FieldCount) do begin
          if (GISdb[DBonTable].MyData.GetFieldType(i) = ftString) then begin
             bString := GISdb[DBonTable].MyData.GetFieldByNameAsString(GISdb[DBonTable].MyData.GetFieldName(i));
             if StrUtils.AnsiContainsText(bString,aString) then ch := 'Y';
          end;
       end;
      GISdb[DBonTable].MyData.Edit;
      GISdb[DBonTable].MyData.SetFieldByNameAsString(fName,ch);

      GISdb[DBonTable].MyData.Next;
   end;
   ShowStatus;
end;

procedure Tdbtablef.AddMercator1Click(Sender: TObject);
var
   Lat,Long,x,y,h,k : float64;
   Prime : boolean;
   MercMap : BaseMap.tMapProjection;
begin
   with GISdb[DBonTable] do begin
      AddFieldToDataBase(ftFloat,'X_MERC',11,1);
      AddFieldToDataBase(ftFloat,'Y_MERC',12,1);
      AddFieldToDataBase(ftFloat,'MERC_H',6,3);
      MercMap := tMapProjection.Create('dbtable, mercator');;
      MercMap.PName := MercatorEllipsoid;
      SetUpDefaultNewProjection(MercMap);
      MercMap.GetProjectParameters;
      EmpSource.Enabled := false;
      GISdb[DBonTable].MyData.First;
      While not GISdb[DBonTable].MyData.Eof do begin
         if ValidLatLongFromTable(Lat,Long) then begin
            MercMap.ForwardProjectDegrees(Lat,Long,x,y);
            MercMap.GetMapScaleFactor(Lat,Long,h,k,Prime);
            GISdb[DBonTable].MyData.Edit;
            GISdb[DBonTable].MyData.SetFieldByNameAsFloat('X_MERC',x);
            GISdb[DBonTable].MyData.SetFieldByNameAsFloat('Y_MERC',y);
            GISdb[DBonTable].MyData.SetFieldByNameAsFloat('MERC_H',h);
         end;
         GISdb[DBonTable].MyData.Next;
      end;
      MercMap.Destroy;
      ShowStatus;
   end;
end;

procedure Tdbtablef.AddMGRS1Click(Sender: TObject);
begin
   AddUTMcoordfields1Click(Sender);
end;

procedure Tdbtablef.Addmultiplefields1Click(Sender: TObject);
begin
   try
      GetDEMIXPaths(true);
      //Removerowsmissinganyevaluations1Click(Sender);
      RankDEMS(DBonTable);
      EvalRangeAndBestEvalForCriterion(DBonTable);
      //CompareSeriousCompetitors(DBonTable);
      AddTileCharacteristicsToDB(DBonTable);
   finally
      EndDEMIXProcessing;
   end;
end;

procedure Tdbtablef.AddnearestelevationfromDEM1Click(Sender: TObject);
begin
   GISdb[DBonTable].AddAndFillFieldFromDEM(adElevNearest);
end;


procedure Tdbtablef.Addprojectedcoordinates1Click(Sender: TObject);
var
   Lat,Long,x,y : float64;
begin
      GISdb[DBonTable].AddFieldToDataBase(ftFloat,'X_PROJ',11,1);
      GISdb[DBonTable].AddFieldToDataBase(ftFloat,'Y_PROJ',12,1);
      GISdb[DBonTable].EmpSource.Enabled := false;
      GISdb[DBonTable].MyData.First;
      ShowHourglassCursor;
      While not GISdb[DBonTable].MyData.Eof do begin
         if GISdb[DBonTable].ValidLatLongFromTable(Lat,Long) then begin
            GISdb[DBonTable].TheMapOwner.MapDraw.LatLongDegreeToProjectedCoords(Lat,Long,x,y);
            GISdb[DBonTable].MyData.Edit;
            GISdb[DBonTable].MyData.SetFieldByNameAsFloat('X_PROJ',x);
            GISdb[DBonTable].MyData.SetFieldByNameAsFloat('Y_PROJ',y);
         end;
         GISdb[DBonTable].MyData.Next;
      end;
      ShowStatus;
end;

procedure Tdbtablef.AddrecordIDfield1Click(Sender: TObject);
var
   NewField,Leader : shortstring;
   i : integer;
begin
   //with GISdb[DBonTable] do begin
      Leader := '';
      Petmar.GetString('ID string',Leader,false,ReasonableTextChars);
      NewField := RecNoFName;
      NewField := AddNewField(GISdb[DBonTable], NewField,ftString,5 + Length(Leader));

      GISdb[DBonTable].EmpSource.Enabled := false;
      GISdb[DBonTable].MyData.First;
      i := 0;
      while not GISdb[DBonTable].MyData.eof do begin
         inc(i);
         GISdb[DBonTable].MyData.Edit;
         GISdb[DBonTable].MyData.SetFieldByNameAsString(NewField,Leader + IntToStr(i));
         GISdb[DBonTable].MyData.Next;
      end;
      ShowStatus;
   //end;
end;

procedure Tdbtablef.AddslopefromDEM1Click(Sender: TObject);
begin
   GISdb[DBonTable].AddAndFillFieldFromDEM(adSlope);
end;


procedure Tdbtablef.Addspeedfromxyoruvcomponents1Click(Sender: TObject);
var
   x,y : float64;
   i,rc : integer;
begin
   //with GISdb[DBonTable] do begin
      GISdb[DBonTable].PickNumericFields(dbgtUnspecified,2,'x component','y component','');
      GISdb[DBonTable].dbOpts.MagField := GISdb[DBonTable].dbOpts.XField;
      GISdb[DBonTable].dbOpts.DirField := GISdb[DBonTable].dbOpts.YField;
      GISdb[DBonTable].AddFieldToDataBase(ftFloat,'SPEED',8,3);
      GISdb[DBonTable].EmpSource.Enabled := false;
      i := 0;
      rc := GISdb[DBonTable].MyData.RecordCount;
      StartProgress('Speed');
      GISdb[DBonTable].MyData.First;
      while not GISdb[DBonTable].MyData.eof do begin
         inc(i);
         if (i mod 1000 = 0) then UpdateProgressBar(i/rc);

         if GISdb[DBonTable].MyData.CarefullyGetFieldByNameAsFloat64(GISdb[DBonTable].dbOpts.MagField,x) and
             GISdb[DBonTable].MyData.CarefullyGetFieldByNameAsFloat64(GISdb[DBonTable].dbOpts.DirField,y) then begin
             GISdb[DBonTable].MyData.Edit;
             GISdb[DBonTable].MyData.SetFieldByNameAsFloat('SPEED',sqrt(x*x+y*y));
         end;
         GISdb[DBonTable].MyData.Next;
      end;
      ShowStatus;
   //end;
end;

procedure Tdbtablef.Addtilecharacteristics1Click(Sender: TObject);
begin
   AddTileCharacteristicsToDB(DBonTable);
end;


procedure Tdbtablef.AddUTMcoordfields1Click(Sender: TObject);
var
   Lat,Long,xutm,yutm,xbase,ybase : float64;
   SingleUTMZone,AlsoAddLatLong: boolean;
   i,rc : integer;
   PointsHeader : sfPointsWithHeader;
   PointsHeader3D : sfPointsZWithHeader;

       procedure AddUTM;
       begin
          GISdb[DBonTable].AddFieldToDataBase(ftFloat,'X_UTM',12,2);
          GISdb[DBonTable].AddFieldToDataBase(ftFloat,'Y_UTM',13,2);
       end;

       procedure ConvertUTMandAddLatLong;
       begin
          WGS84DatumConstants.UTMtoLatLongDegree(xutm,yutm,Lat,Long);
          GISdb[DBonTable].MyData.SetFieldByNameAsFloat(GISdb[DBonTable].LatFieldName,Lat);
          GISdb[DBonTable].MyData.SetFieldByNameAsFloat(GISdb[DBonTable].LongFieldName,Long);
       end;

       procedure GetNewZone;
       var
          Long,Lat : float64;
          LatCh : AnsiChar;
       begin
          if GISdb[DBonTable].GetLatLongToRepresentRecord(Lat,Long) then begin
             WGS84DatumConstants.projUTMZone := GetUTMZone(Long);
             LatCh := HemiFromLat(Lat);
          end
          else begin
              PickUTMZone(WGS84DatumConstants.projUTMZone);
              LatCh := MDDef.DefaultLatHemi;
          end;
          WGS84DatumConstants.DefineDatumFromUTMZone('WGS84',WGS84DatumConstants.projUTMZone,Latch,'Tdbtablef.AddUTMcoordfields1Click');
       end;

begin
   with GISdb[DBonTable] do begin
      AlsoAddLatLong := false;
      if (Sender = AddMGRS1) then begin
         AddFieldToDataBase(ftString,'MGRS',24,0);
      end
      else if (Sender = AddLatLong1) then begin
         if (aShapeFile = Nil) then exit;
         AddLatLong;
         if (ShapeFileType = 11) then AddFieldToDataBase(ftFloat,'Z',12,4);
      end
      else if (Sender = Adddecimaldegreesstring1) then begin
         AddFieldToDataBase(ftString,'WGS84_DD',28,0);
      end
      else if (Sender = AdddecimalminutesString1) then begin
         AddFieldToDataBase(ftString,'WGS84_DM',28,0);
      end
      else if (Sender = AdddecimalsecondsString1) then begin
         AddFieldToDataBase(ftString,'WGS84_DS',34,0);
      end
      else if (Sender = AddUTMfromoffsets1) then begin
         XBase := 356000;
         YBase := 3966000;
         ReadDefault('X base',XBase);
         ReadDefault('Y Base',YBase);
         AlsoAddLatLong := AnswerIsYes('Also add lat/long');
         if AlsoAddLatLong then begin
            GetNewZone;
            AddLatLong;
         end;
         AddUTM;
      end
      else if (Sender = ConvertUTMtolatlong1) then begin
         GetNewZone;
         dbOpts.xField := 'X_UTM';
         if not GISdb[DBonTable].MyData.FieldExists(dbOpts.xField) then dbOpts.xField := PickField('x UTM',NumericFieldTypes);
         dbOpts.yField := 'Y_UTM';
         if not GISdb[DBonTable].MyData.FieldExists(dbOpts.yField) then dbOpts.yField := PickField('y UTM',NumericFieldTypes);
         PickUTMZone(WGS84DatumConstants.projUTMZone);
         WGS84DatumConstants.DefineDatumFromUTMZone('WGS84',WGS84DatumConstants.projUTMZone,MDDef.DefaultLatHemi,'Tdbtablef.AddUTMcoordfields1Click');
         AddLatLong;
      end
      else begin
         SingleUTMZone := AnswerIsYes('All conversions to single UTM zone');
         if SingleUTMZone then GetNewZone;
         AddUTM;
         AddFieldToDataBase(ftInteger,'UTM_ZONE',2,0);
      end;
      ShowHourglassCursor;

      GISdb[DBonTable].MyData.First;
      StartProgress('Project');
      i := 0;
      rc := GISdb[DBonTable].MyData.RecordCount;
      While not GISdb[DBonTable].MyData.Eof do begin
         if (i mod 1000 = 0) then begin
            UpdateProgressBar(i/rc);
            wmDEM.SetPanelText(0,IntToStr(i));
            EmpSource.Enabled := false;
         end;
         inc(i);
         GISdb[DBonTable].MyData.Edit;
         if (Sender = AddLatLong1) then begin
            if (ShapeFileType = 11) then begin
                if aShapeFile.ReadPointRecord3D(MyData.RecNo,PointsHeader3D) then begin
                   GISdb[DBonTable].MyData.SetFieldByNameAsFloat(GISdb[DBonTable].LatFieldName,PointsHeader3D.y);
                   GISdb[DBonTable].MyData.SetFieldByNameAsFloat(GISdb[DBonTable].LongFieldName,PointsHeader3D.x);
                   GISdb[DBonTable].MyData.SetFieldByNameAsFloat('Z',PointsHeader3D.z);
                end;
            end
            else begin
                if aShapeFile.ReadPointRecord(MyData.RecNo,PointsHeader) then begin
                   GISdb[DBonTable].MyData.SetFieldByNameAsFloat(GISdb[DBonTable].LatFieldName,PointsHeader.y);
                   GISdb[DBonTable].MyData.SetFieldByNameAsFloat(GISdb[DBonTable].LongFieldName,PointsHeader.x);
                end;
            end;
         end
         else if (Sender = ConvertUTMtolatlong1) then begin
            xUTM := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(dbOpts.xField);
            yUTM := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(dbOpts.yField);
            ConvertUTMandAddLatLong;
         end
         else if (Sender = AddUTMfromoffsets1) then begin
            xutm := XBase + GISdb[DBonTable].MyData.GetFieldByNameAsFloat('X_OFF');
            yutm := YBase + GISdb[DBonTable].MyData.GetFieldByNameAsFloat('Y_OFF');
            GISdb[DBonTable].MyData.SetFieldByNameAsFloat('X_UTM',xutm);
            GISdb[DBonTable].MyData.SetFieldByNameAsFloat('Y_UTM',yutm);
            if AlsoAddLatLong then ConvertUTMandAddLatLong;
         end
         else if ValidLatLongFromTable(Lat,Long) then begin
            if (Sender = AddMGRS1) then begin
               RedefineWGS84DatumConstants(Long);
               GISdb[DBonTable].MyData.SetFieldByNameAsString('MGRS',WGS84DatumConstants.LatLongToMGRS(Lat,Long));
            end
            else if (Sender = AdddecimaldegreesString1) then begin
               GISdb[DBonTable].MyData.SetFieldByNameAsString('WGS84_DD',LatLongDegreeToString(Lat,Long));
            end
            else if (Sender = AdddecimalminutesString1) then begin
               GISdb[DBonTable].MyData.SetFieldByNameAsString('WGS84_DM',LatLongDegreeToString(Lat,Long,DecMinutes));
            end
            else if (Sender = AdddecimalsecondsString1) then begin
               GISdb[DBonTable].MyData.SetFieldByNameAsString('WGS84_DS',LatLongDegreeToString(Lat,Long,DecSeconds));
            end
            else begin
               if not SingleUTMZone then RedefineWGS84DatumConstants(Long);
               WGS84DatumConstants.ForwardProjectDegrees(Lat,Long,xutm,yutm);
               GISdb[DBonTable].MyData.SetFieldByNameAsFloat('X_UTM',xutm);
               GISdb[DBonTable].MyData.SetFieldByNameAsFloat('Y_UTM',yutm);
               GISdb[DBonTable].MyData.SetFieldByNameAsInteger('UTM_ZONE',WGS84DatumConstants.projUTMZone);
            end;
         end;
         GISdb[DBonTable].MyData.Next;
      end;
      ShowStatus;
   end;
end;

procedure Tdbtablef.AddUTMfromoffsets1Click(Sender: TObject);
begin
   AddUTMcoordfields1Click(Sender);
end;

procedure Tdbtablef.AddXYbox1Click(Sender: TObject);
begin
   GraphDoing := gdBoxOutlineAdd;
end;

procedure Tdbtablef.AddXYZfromjoinedtable1Click(Sender: TObject);
var
   FieldsInDB,MissingBins : tStringList;
   i,rc : integer;
begin
   {$IfDef ExportCoords} WriteLineToDebugFile('Tdbtablef.AddXYZfromjoinedtable1Click'); {$EndIf}
   with GISdb[DBonTable] do begin
      GISdb[DBonTable].ClearGISFilter;
      MissingBins := tStringList.Create;
      MissingBins.Sorted := true;
      MissingBins.Duplicates := dupIgnore;
      if GISdb[DBonTable].MyData.FieldExists('X') and GISdb[DBonTable].MyData.FieldExists('Y') and GISdb[DBonTable].MyData.FieldExists('Z') then begin
         if AnswerIsYes('Clear XYZ fields') then begin
             GISdb[DBonTable].FillFieldWithValue('X','');
             GISdb[DBonTable].FillFieldWithValue('Y','');
             GISdb[DBonTable].FillFieldWithValue('Z','');
         end;
      end
      else AddXYZfields;
      EmpSource.Enabled := false;
      FieldsInDB := Nil;
      FieldsInDB := GISdb[DBonTable].MyData.ListUniqueEntriesInDB(dbOpts.LinkFieldThisDB);
      ShowHourglassCursor;
      rc := FieldsInDB.Count;
      StartProgress('Add xyz');
      for i := 0 to pred(FieldsInDB.Count) do begin
         UpdateProgressBar(i/rc);
         EmpSource.Enabled := false;
         dbOpts.MainFilter := dbOpts.LinkFieldThisDB + '=' + QuotedStr(trim(FieldsInDB.Strings[i]));
         GISdb[DBonTable].AssembleGISFilter;
         {$IfDef ExportCoords} WriteLineToDebugFile('MyData filter=' + GISdb[DBonTable].MyData.Filter  + '  ' + IntToStr(i) + '/' + IntToStr(pred(FieldsInDB.Count) )); {$EndIf}

         LinkTable.ApplyFilter( dbOpts.LinkFieldOtherDB + '=' + QuotedStr(FieldsInDB.Strings[i]));
         if (LinkTable.RecordCount=0) then begin
            {$IfDef ExportCoords} WriteLineToDebugFile('No bin for ' + FieldsInDB.Strings[i]); {$EndIf}
            MissingBins.Add(FieldsInDB.Strings[i]);
         end
         else if (LinkTable.RecordCount < GISdb[DBonTable].MyData.RecordCount) then begin
            {$IfDef ExportCoords} WriteLineToDebugFile('Insufficent XYZ for ' + FieldsInDB.Strings[i]); {$EndIf}
         end
         else begin
            {$IfDef ExportCoords} WriteLineToDebugFile('MyData recs=' + IntToStr(MyData.RecordCount) + '   LinkData recs=' + IntToStr(LinkTable.RecordCount)); {$EndIf}
            while not GISdb[DBonTable].MyData.eof do begin
               GISdb[DBonTable].MyData.Edit;
               GISdb[DBonTable].MyData.SetFieldByNameAsFloat('X',LinkTable.GetFieldByNameAsFloat('X'));
               GISdb[DBonTable].MyData.SetFieldByNameAsFloat('Y',LinkTable.GetFieldByNameAsFloat('Y'));
               GISdb[DBonTable].MyData.SetFieldByNameAsFloat('Z',LinkTable.GetFieldByNameAsFloat('Z'));
               GISdb[DBonTable].MyData.Next;
               LinkTable.Next;
            end;
         end;
      end;
      FieldsInDB.Free;
      ClearGISFilter;
      LinkTable.ApplyFilter('');
      EndProgress;
      ShowStatus;
      Petmar.DisplayAndPurgeStringList(MissingBins,'Missing bins');
   end;
end;


procedure Tdbtablef.AddXYZfromshpfile1Click(Sender: TObject);
var
   i,rc : integer;
   PointsHeader : sfPointsZWithHeader;
   ThreeD : boolean;

         procedure ItsLatLong;
         begin
             GISdb[DBonTable].AddLatLong;
             GISdb[DBonTable].LatFieldName := 'LAT';
             GISdb[DBonTable].LongFieldName := 'LONG';
         end;

begin
   with GISdb[DBonTable] do begin
      ThreeD := (Sender <> Addlatlongfromshpfile1) and (Sender <> Nil);
      if not ThreeD then begin
         ItsLatLong;
      end
      else begin
         if XYZFile then begin
            AddXYZfields;
            LatFieldName := 'Y';
            LongFieldName := 'X';
         end
         else begin
            ItsLatLong;
            AddFieldToDataBase(ftFloat,'Z',18,6);
         end;
      end;

      i := 0;
      rc := GISdb[DBonTable].MyData.RecordCount;
      GISdb[DBonTable].MyData.First;
      StartProgressAbortOption('Coords');
      while not GISdb[DBonTable].MyData.eof do begin
         if (i mod 250 = 0) then begin
            UpdateProgressBar(i/rc);
            EmpSource.Enabled := false;
         end;
         inc(i);
         aShapeFile.ReadPointRecord3D(MyData.RecNo,PointsHeader);
         GISdb[DBonTable].MyData.Edit;
         GISdb[DBonTable].MyData.SetFieldByNameAsFloat(LongFieldName,PointsHeader.x);
         GISdb[DBonTable].MyData.SetFieldByNameAsFloat(LatFieldName,PointsHeader.y);
         if ThreeD then GISdb[DBonTable].MyData.SetFieldByNameAsFloat('Z',PointsHeader.z);
         GISdb[DBonTable].MyData.Next;
         if WantOut then break;
      end;
      ShowStatus;
   end;
end;


procedure Tdbtablef.Adjustzvaluesbasedonx1Click(Sender: TObject);
var
   LineTable : tMyData;
   fName : PathStr;
   Values : tStringList;
   i,j,nv : integer;
   xv2,zv2 : float64;
   xv,zv : array[0..100] of float64;
begin
   if GetFileFromDirectory('file with Z values',DefaultDBMask,fName) then begin
      LineTable := tMyData.Create(fName);
      fName := 'DECK';
      Values := Nil;
      Values := LineTable.ListUniqueEntriesInDB(fName);
      ShowHourglassCursor;
      for i := 0 to pred(Values.Count) do begin
         LineTable.ApplyFilter(fName + '=' + QuotedStr(Values.Strings[i]));
         {$IfDef Recordzvalueplot} WriteLineToDebugFile('LineTable filter=' + LineTable.Filter); {$EndIf}
         nv := 0;
         while not LineTable.eof do begin
            inc(nv);
            xv[nv] := LineTable.GetFieldByNameAsFloat('X');
            zv[nv] := LineTable.GetFieldByNameAsFloat('Z');
            {$IfDef Recordzvalueplot} WriteLineToDebugFile('nv=' + IntToStr(nv) + ' x=' + RealToString(xv[nv],-8,-2) + '   z=' + RealToString(zv[nv],-8,-2)); {$EndIf}
            LineTable.Next;
         end;
         GISdb[DBonTable].MyData.ApplyFilter(LineTable.Filter);
         GISdb[DBonTable].EmpSource.Enabled := false;
         while not GISdb[DBonTable].MyData.eof do begin
            xv2 := GISdb[DBonTable].MyData.GetFieldByNameAsFloat('X');
            if (xv2 < xv[1]) then zv2 := zv[1]
            else if (xv2 > xv[nv]) then zv2 := zv[nv]
            else begin
               j := nv;
               repeat
                  dec(j);
               until (xv2 > xv[j]);
               {$IfDef Recordzvalueplot} WriteLineToDebugFile('x2=' + RealToString(xv2,-8,-2) + ' between ' + RealToString(xv[j-1],-8,-2) + ' and ' + RealToString(xv[j],-8,-2)); {$EndIf}
               zv2 := zv[j] + (zv[j+1] - zv[j]) * (xv2 - xv[j]) / (xv[j+1] - xv[j]);
               {$IfDef Recordzvalueplot} WriteLineToDebugFile('z2=' + RealToString(zv2,-8,-2) + ' between ' + RealToString(zv[j-1],-8,-2) + ' and ' + RealToString(zv[j],-8,-2),false); {$EndIf}
            end;
            GISdb[DBonTable].MyData.Edit;
            GISdb[DBonTable].MyData.SetFieldByNameAsFloat('Z',zv2);
            GISdb[DBonTable].MyData.Next;
         end;

      end;
      LineTable.Destroy;
      GISdb[DBonTable].ClearGISFilter;
      ShowStatus;
   end;
end;


procedure Tdbtablef.ALANDtoALANDKM21Click(Sender: TObject);
var
   NewField,fName,TStr : shortstring;
   area : float64;
   i : integer;
begin
   if (Sender = AWATERtoWATERKM21) then begin
      fName := 'AWATER';
      tStr := 'water';
      NewField := 'WATER_KM2';
   end
   else begin
      fName := 'ALAND';
      tStr := 'land';
      NewField := 'LAND_KM2';
   end;

   if not GISdb[DBonTable].MyData.FieldExists(fName) then begin
      fName := GISdb[DBonTable].PickField(TStr + ' area (square meters)',NumericFieldTypes);
   end;
   GISdb[DBonTable].AddFieldToDataBase(ftFloat,NewField,14,6);
   GISdb[DBonTable].MyData.First;
   GISdb[DBonTable].EmpSource.Enabled := false;
   StartProgress('Area');
   i := 0;
   while not GISdb[DBonTable].MyData.eof do begin
      inc(i);
      if (i mod 500 = 0) then UpdateProgressBar(i/ GISdb[DBonTable].MyData.TotRecsInDB);
      if GISdb[DBonTable].MyData.CarefullyGetFieldByNameAsFloat64(fName,area) then begin
         GISdb[DBonTable].MyData.Edit;
         GISdb[DBonTable].MyData.SetFieldByNameAsFloat(NewField,0.000001 * Area);
      end;
      GISdb[DBonTable].MyData.Next;
   end;
   ShowStatus;
end;


procedure Tdbtablef.Allcriteriavalues1Click(Sender: TObject);
begin
   {$IfDef ExDEMIXexperimentalOptions}
   {$Else}
      DEMIXwineContestCriterionGraph(dgAllValues,DBonTable);
   {$EndIf}
end;

procedure Tdbtablef.AllDBs1Click(Sender: TObject);
begin
   Histogram1Click(Sender);
end;


procedure Tdbtablef.AllDBsaddinterpolatedelevation1Click(Sender: TObject);
var
   i : integer;
   fName : PathStr;
begin
   fName := 'Z';
   Petmar.GetString('z field name',fName,true,DBaseFieldNameChars);
   for i := 1 to MaxDataBase do begin
      if (GISdb[i] <> nil) then begin
         GISdb[i].AddAndFillFieldFromDEM(adElevInterp,fName);
      end;
   end;
end;

procedure Tdbtablef.AllDBsbynumericfield1Click(Sender: TObject);
var
   WantedField : shortstring;
   i : integer;
begin
   {$IfDef RecordDBPlot} WriteLineRoDebugFile('Tdbtablef.AllDBsbynumericfield1Click in,'); {$EndIf}
   with GISdb[DBonTable] do begin
      WantedField := PickField('coloring',NumericFieldTypes);
      for i := 1 to MaxDataBase do begin
         if (GISdb[i] <> nil) then begin
            {$IfDef RecordDBPlot} WriteLineRoDebugFile('Plot ' + GISdb[i].dbName); {$EndIf}
            GISdb[i].PlotFieldOnMap(WantedField);
         end;
      end;
   end;
end;

procedure Tdbtablef.AllDBsmultiplegraphs1Click(Sender: TObject);
begin
   Histogram1Click(Sender);
end;


procedure Tdbtablef.Allfields1Click(Sender: TObject);
{$IfDef NoDBGrafs}
begin
{$Else}
var
  WantXField : shortstring;
  UseFields : tStringList;
  j : integer;
begin
   ManageDBDisplay(DBGrid1,DBonTable);
   UseFields := tStringList.Create;
   for j := 0 to pred(DBGrid1.Columns.Count) do begin
      if (DBGrid1.Columns[j].Visible) and (DBGrid1.Columns[j].Field.DataType in [ftFloat,ftInteger,ftSmallInt]) then  begin
         UseFields.Add(DBGrid1.Columns[j].Field.FieldName);
      end;
   end;

   for j := 0 to pred(UseFields.Count) do begin
      WantXField := UseFields.Strings[j];
      GISdb[DBonTable].OldCreateHistogramFromDataBase(true,WantXField,'','',false);
   end;
{$EndIf}
end;


procedure Tdbtablef.Allfields2Click(Sender: TObject);
begin
   GISdb[DBonTable].DisplayFieldStatistics('ALL-FIELDS');
end;

procedure Tdbtablef.Allfields3Click(Sender: TObject);
var
   TStr,fName : ShortString;
   i,rc: integer;
begin
   StartProgress('Missing Data');
   rc := GISdb[DBonTable].MyData.FieldCount;
   for i := 0 to pred(rc) do begin
      if (i mod 250 = 0) then begin
         UpdateProgressBar(i/rc);
         GISdb[DBonTable].EmpSource.Enabled := false;
      end;
      GISdb[DBonTable].MyData.First;
      if GISdb[DBonTable].MyData.GetFieldType(i) in [ftFloat,ftInteger,ftSmallInt] then begin
         fName := GISdb[DBonTable].MyData.GetFieldName(i);
         while Not GISdb[DBonTable].MyData.eof do begin
            TStr := GISdb[DBonTable].MyData.GetFieldByNameAsString(fName);
            if MissingNumericString(TStr) then begin
                GISdb[DBonTable].MyData.Edit;
                GISdb[DBonTable].MyData.SetFieldByNameAsString(fName,'');
            end;
            GISdb[DBonTable].MyData.Next;
         end;
      end;
   end;
   ShowStatus;
end;

procedure Tdbtablef.Allnormalizedprofiles1Click(Sender: TObject);
begin
   DrawDEMTerrainprofile(Sender);
end;

procedure Tdbtablef.Allopengridselevationdifference1Click(Sender: TObject);
begin
   GISdb[DBonTable].AddAndFillFieldFromDEM(adDeltaAllGrids);
end;

procedure Tdbtablef.Allover1Click(Sender: TObject);
begin
   GISdb[DBonTable].AddAndFillFieldFromDEM(adElevAllGrids);
end;

procedure Tdbtablef.AllpointsinboxallopenDBs1Click(Sender: TObject);
begin
   ChangeDEMNowDoing(DeleteMultipleRecsAllDBs);
   DBEditting := DBonTable;
   GISdb[DBonTable].theMapOwner.MouseIsDown := false;
end;

procedure Tdbtablef.AllpointsinlinewithXYZ1Click(Sender: TObject);
begin
   GISdb[DBonTable].ExtractPointsFromLineAndAddXYZ;
end;

procedure Tdbtablef.Allpointsinpolygon1Click(Sender: TObject);
var
   MaskDB : integer;
begin
   MaskDB := PickOpenGISDataBase('Mask area database');
   if (MaskDB = -99) then exit;
   MarkPointInPolygon(PipDelete,DBonTable,MaskDB,'','');
   GISdb[DBonTable].DeleteAllSelectedRecords;
   GISdb[DBonTable].theMapOwner.DoFastMapRedraw;
   ShowStatus;
end;

procedure Tdbtablef.AllpointsinpolygonallopenDBs1Click(Sender: TObject);
var
   i,j,MaskDB,OldRecs : integer;
begin
   MaskDB := PickOpenGISDataBase('Mask area database');
   if ValidDB(MaskDB) then begin
      j := 0;
      for i := 1 to MaxDataBase do if (i <> MaskDB) and (GISdb[i] <> Nil) then begin
          inc(j);
          OldRecs := GISdb[i].MyData.TotRecsInDB;
          wmDEM.SetPanelText(0,'Deleting ' + IntToStr(j)+  '/' + IntToStr(NumOpenDB));
          MarkPointInPolygon(PipDelete,i,MaskDB,'','');
          GISdb[i].DeleteAllSelectedRecords;
          GISdb[i].theMapOwner.DoFastMapRedraw;
          WriteLineToDebugFile(GISdb[i].dbName + ' recs deleted = ' + IntToStr(OldRecs - GISdb[i].MyData.TotRecsInDB));
       end;
      wmDEM.SetPanelText(0,'');
      ShowStatus;
   end;
end;

procedure Tdbtablef.Allprofiles1Click(Sender: TObject);
begin
   DrawDEMTerrainprofile(Sender);
end;

procedure Tdbtablef.Allrecordsmatchingsinglefield1Click(Sender: TObject);
begin
   Countuniquevalues1Click(Sender);
end;

procedure Tdbtablef.Alltiles1Click(Sender: TObject);
//Cluster whisker plots for tile characteristics, get all tiles by picking just one criterion
var
   aFilter : shortstring;
   Filters : tStringList;
begin
   aFilter := GISdb[DBonTable].MyData.Filter;
   GISdb[DBonTable].ApplyGISFilter('CRITERION=' + QuotedStr('ELEV_FUV'));
 (*
               for i := 1 to 15 do begin  //loop through clusters, quantiles
                  aFilter := FieldWanted + '=' + IntToStr(i);
                  if (BaseFilter <> '') then aFilter := BaseFilter + ' AND ' + aFilter;
                  DoOneCluster(aFilter,FieldWanted + IntegerToString(i,3));
               end;
 *)
   TileCharateristicsWhiskerPlotsByCluster(DBonTable,true,Nil);
   GISdb[DBonTable].MyData.Filter := aFilter;
end;

procedure Tdbtablef.Alphabetize1Click(Sender: TObject);
var
   aString : ShortString;
   i : integer;
   Sorting : tStringList;
begin
   with GISdb[DBonTable] do begin
      ShowHourglassCursor;
      EmpSource.Enabled := false;
      GISdb[DBonTable].MyData.First;
      while not GISdb[DBonTable].MyData.eof do begin
         aString := GISdb[DBonTable].MyData.GetFieldByNameAsString(SelectedColumn);
         if StrUtils.AnsiContainsText(astring,',') then begin
            Sorting := tStringList.Create;
            Sorting.Sorted := true;
            repeat
                Sorting.Add(BeforeSpecifiedCharacterANSI(aString,',',true,true));
            until aString = '';
            Astring := Sorting.Strings[0];
            for i := 1 to pred(Sorting.Count) do astring := aString + ',' + Sorting.Strings[i];
            Sorting.Destroy;
            GISdb[DBonTable].MyData.Edit;
            GISdb[DBonTable].MyData.SetFieldByNameAsString(SelectedColumn,aString);
         end;
         GISdb[DBonTable].MyData.Next;
      end;
      ShowStatus;
   end;
end;

procedure Tdbtablef.AlphabetizefieldwithCSVsubfields1Click(Sender: TObject);
var
   entry : shortstring;
   entries : tStringList;
   i : integer;
begin
    SelectedColumn := GISdb[DBonTable].PickField('field to alphabetize',[ftString]);
    GISdb[DBonTable].EmpSource.Enabled := false;
    GISdb[DBonTable].MyData.First;
    Entries := tStringList.Create;
    Entries.Sorted := true;
    while not GISdb[DBonTable].MyData.eof do begin
       entry := GISdb[DBonTable].MyData.GetFieldByNameAsString(SelectedColumn);
       entries.clear;
       if ANSIContainsText(entry,',') then begin
         repeat
             Entries.Add(BeforeSpecifiedCharacterANSI(entry,',',true,true));
         until entry = '';
         entry := entries.Strings[0];
         for i := 1 to pred(entries.count) do entry := entry + ',' + entries.Strings[i];
         GISdb[DBonTable].MyData.Edit;
         GISdb[DBonTable].MyData.SetFieldByNameAsString(SelectedColumn,entry);
       end;
       GISdb[DBonTable].MyData.Next;
    end;
    ShowStatus;
end;


procedure Tdbtablef.Singlefield2Click(Sender: TObject);
var
   FieldDesired : ShortString;
begin
   if (Sender = SingleField2) or (Sender = Fieldstatistics1) then begin
      FieldDesired := GISdb[DBonTable].PickField('Field for statistics',NumericFieldTypes);
      if (FieldDesired = '') then exit;
      GISdb[DBonTable].DisplayFieldStatistics(FieldDesired);
   end;
end;


procedure Tdbtablef.Singleicon1Click(Sender: TObject);
begin
   GISdb[DBonTable].GISProportionalSymbols(dbasIconAll);
end;

procedure Tdbtablef.Singlepoints1Click(Sender: TObject);
begin
   ChangeDEMNowDoing(DeleteMultipleDBRecs);
   DBEditting := DBonTable;
   GISdb[DBonTable].theMapOwner.MouseIsDown := false;
end;

procedure Tdbtablef.Singlepoints2Click(Sender: TObject);
begin
   ChangeDEMNowDoing(DeleteSingleDBRecs);
   DBEditting := DBonTable;
end;

procedure Tdbtablef.SingleZipatone1Click(Sender: TObject);
begin
   if GetGraphicsFileName('zipatone for db',GISdb[DBonTable].dbOpts.ZipaToneFName) then begin
      GISdb[DBonTable].GISProportionalSymbols(dbasZipatoneAll);
   end;
end;

procedure Tdbtablef.Sinoffield1Click(Sender: TObject);
begin
   Sumtwofields1Click(Sender);
end;

procedure Tdbtablef.Sinuosity1Click(Sender: TObject);
begin
  GISdb[DBonTable].AddGeometry(asSinuousity);
end;


procedure Tdbtablef.SortandreplaceDB2Click(Sender: TObject);
begin
   SortAndReplaceDataBase(DBOnTable,true);
end;

procedure Tdbtablef.SortbyBESTEVAL1Click(Sender: TObject);
begin
   SortDataBase(DBonTable,true,'BEST_EVAL',ExtractFilePath(GISdb[DBonTable].dbFullName));
end;

procedure Tdbtablef.Sortfield1Click(Sender: TObject);
var
   i,NPts : int64;
   Results : tStringList;
   Values : ^Petmath.bfarray32;
begin
   New(Values);
   GISdb[DBonTable].GetPointArrayForDBField(SelectedColumn,Values^,NPts);
   Petmath.HeapSort(NPts,Values^);
   Results := tStringList.Create;
   for I := 0 to pred(NPts) do Results.Add(RealToString(Values^[i],18,6));
   Petmar.DisplayAndPurgeStringList(Results,'Sort ' + GISdb[DBonTable].dbName + ' for ' + SelectedColumn);
   Dispose(Values);
   ShowStatus;
end;


procedure Tdbtablef.Soundvelocity1Click(Sender: TObject);
begin
   Sigmatee1Click(Sender);
end;

procedure Tdbtablef.Splitdatefield1Click(Sender: TObject);
begin
   GISdb[DBonTable].SplitDateField(dfMDYSlash);
end;


procedure Tdbtablef.Splitdatefield2Click(Sender: TObject);
begin
   GISdb[DBonTable].SplitDateField(dfYMDstraight);
end;

procedure Tdbtablef.SplitdatefieldYYYYMMDD1Click(Sender: TObject);
begin
   GISdb[DBonTable].SplitDateField(dfYMDslash);
end;

procedure Tdbtablef.SplittimefieldHRMN1Click(Sender: TObject);
begin
   SplittimestringHHMMSS1Click(Sender);
end;

procedure Tdbtablef.SplittimestringHHMMSS1Click(Sender: TObject);
var
   TStr : ANSIString;
   i,rc,nc : integer;
   fName : ShortString;
begin
   with GISdb[DBonTable] do begin
      AddFieldToDataBase(ftInteger,'HOUR',2,0);
      AddFieldToDataBase(ftInteger,'MINUTE',2,0);
      if Sender =  SplittimestringHHMMSS1 then begin
         AddFieldToDataBase(ftInteger,'SECOND',5,2);
         fName := 'TIME_STR';
      end
      else FName := 'HRMN';
      GISdb[DBonTable].MyData.First;
      i := 0;
      GISdb[DBonTable].MyData.ProgressVars(rc,nc);
      StartProgress('Time split');
      While not GISdb[DBonTable].MyData.eof do begin
         if (i mod rc = 0) then begin
            EmpSource.Enabled := false;
            UpdateProgressBar(i/nc);
         end;
         inc(i);

         TStr := GISdb[DBonTable].MyData.GetFieldByNameAsString(fName);
         if TStr <> '' then begin
            GISdb[DBonTable].MyData.Edit;
            if Sender = SplittimefieldHRMN1 then begin
               GISdb[DBonTable].MyData.SetFieldByNameAsString('HOUR',Copy(TStr,1,2));
               GISdb[DBonTable].MyData.SetFieldByNameAsString('MINUTE',Copy(TStr,3,2));
            end
            else begin
               GISdb[DBonTable].MyData.SetFieldByNameAsString('HOUR',Petmar_types.BeforeSpecifiedCharacterANSI(TStr,':',true,true));
               GISdb[DBonTable].MyData.SetFieldByNameAsString('MINUTE',Petmar_types.BeforeSpecifiedCharacterANSI(TStr,':',true,true));
               GISdb[DBonTable].MyData.SetFieldByNameAsString('SECOND',TStr);
            end;
         end;
         GISdb[DBonTable].MyData.Next;
      end;
      ShowStatus;
   end;
end;


procedure Tdbtablef.SQLliteDB1Click(Sender: TObject);
begin
   GISdb[DBonTable].ExportToSQLite;
   ShowStatus;
end;


procedure Tdbtablef.SSIMFUVgraphforthistile1Click(Sender: TObject);
begin
   {$IfDef ExDEMIXexperimentalOptions}
   {$Else}
      DEMIX_SSIM_FUV_single_tile_graph(DBonTable,GISdb[DBonTable].MyData.GetFieldByNameAsString('DEMIX_TILE'));
   {$EndIf}
end;


procedure Tdbtablef.Statisticsgroupedbyonefield1Click(Sender: TObject);
begin
   Fieldcorrelations1Click(Sender);
end;


procedure Tdbtablef.Stereonetdipdirections1Click(Sender: TObject);
begin
   StereoNetPoles1Click(Sender);
end;

procedure Tdbtablef.Stereonetgreatcircles1Click(Sender: TObject);
begin
   StereoNetPoles1Click(Sender);
end;

procedure Tdbtablef.StereoNetPoles1Click(Sender: TObject);
{$IfDef ExGeology}
begin
{$Else}
var
   ThreePointNet : TNetForm;
   xd,yd : integer;
begin
    ThreePointNet := TNetForm.Create(Application);
    ThreePointNet.nd.NewNet;
    if (Sender = StereoNetPoles1) then ThreePointNet.Caption := GISdb[DBonTable].dbName + ' Poles to planes'
    else if (Sender=Stereonetgreatcircles1) then ThreePointNet.Caption := GISdb[DBonTable].dbName +  ' Great circles'
    else ThreePointNet.Caption := GISdb[DBonTable].dbName + ' Dip directions';
    GISdb[DBonTable].MyData.First;
    while not GISdb[DBonTable].MyData.eof do begin
       if (Sender = StereoNetPoles1) then
          ThreePointNet.nd.PlotPointOnNet(PolePlot,GISdb[DBonTable].MyData.GetFieldByNameAsInteger('DIP'),GISdb[DBonTable].MyData.GetFieldByNameAsInteger('STRIKE')+90,ASymbol(FilledBox,claRed,2),xd,yd)
       else if (Sender=Stereonetgreatcircles1) then
          ThreePointNet.nd.GreatCircleOnNet(GISdb[DBonTable].MyData.GetFieldByNameAsInteger('DIP'), GISdb[DBonTable].MyData.GetFieldByNameAsInteger('STRIKE')+90,2,claLime)
       else ThreePOintNet.nd.PlotPointOnNet(LinePlot,GISdb[DBonTable].MyData.GetFieldByNameAsInteger('DIP'),GISdb[DBonTable].MyData.GetFieldByNameAsInteger('STRIKE')+90,ASymbol(FilledBox,ClaLime,2),xd,yd);
       GISdb[DBonTable].MyData.Next;
    end;
    ThreePointNet.UpdateDisplay;
{$EndIf}
end;

procedure Tdbtablef.Stickstadpoleplot1Click(Sender: TObject);
var
  MaxRange,MinX : float64;
  rFile : file;
  v : array[1..3] of float64;
  ThisGraph : TThisbasegraph;
begin
   with GISdb[DBonTable] do begin
      PickNumericFields(dbgtUnspecified,3,'Time','Magnitude','Direction');
      dbOpts.TimeField := dbOpts.XField;
      dbOpts.MagField := dbOpts.YField;
      dbOpts.DirField := dbOpts.ZField;
      EmpSource.Enabled := false;

      ThisGraph := TThisbasegraph.Create(Application);
      ThisGraph.OpenXYZFile(rfile);
      ThisGraph.GraphDraw.GraphType := gtTadpole;
      MaxRange := 0;
      GISdb[DBonTable].MyData.First;
      MinX := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(dbOpts.TimeField);
      while not GISdb[DBonTable].MyData.eof do begin
         v[1] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(dbOpts.TimeField);
         v[2] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(dbOpts.DirField);
         v[3] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(dbOpts.MagField);
         if (v[3] > MaxRange) then MaxRange := v[3];
         BlockWrite(rfile,v,1);
         GISdb[DBonTable].MyData.Next;
      end;
      CloseFile(rfile);
      ThisGraph.InitializeTadpole('Sticks/tadpole plot ' + dbName,MinX,v[1],MaxRange);
   end;
   ShowStatus;
end;


procedure Tdbtablef.Histogram1Click(Sender: TObject);
{$IfDef NoDBGrafs}
begin
{$Else}
var
  WantXField : shortstring;
  i : integer;
  MaxY,MaxX : float64;
  Hists : array[1..MaxDataBase] of TThisBaseGraph;
begin
   with GISdb[DBonTable] do begin
      WantXField := PickField('Field for histogram',[ftFloat,ftInteger,ftSmallInt]);
      if (WantXField <> '') then begin
         if (Sender = AllDBsmultiplegraphs1) then begin
            MDDef.NoHistFreqLabels := true;
            CreateSmallGraph := true;
            MaxY := -9999;
            MaxX := -9999;

            for i := 1 to MaxDataBase do begin
               if (GISdb[i] <> nil) then begin
                  Hists[i] := GISdb[i].OldCreateHistogramFromDataBase(true,WantXField,'','',false);
                  if (Hists[i].GraphDraw.MaxVertAxis > MaxY) then MaxY := Hists[i].GraphDraw.MaxVertAxis;
                  if (Hists[i].GraphDraw.MaxHorizAxis > MaxX) then MaxX := Hists[i].GraphDraw.MaxHorizAxis;
               end
               else Hists[i] := Nil;
            end;

            MDDef.NoHistFreqLabels := false;
            CreateSmallGraph := false;
            if AnswerIsYes('Rescale') then begin
              ReadDefault('MaxX',MaxX);
              ReadDefault('MaxY',MaxY);
              for i := 1 to MaxDataBase do begin
                 if (Hists[i] <> Nil) then begin
                    Hists[i].GraphDraw.MaxHorizAxis := MaxX;
                    Hists[i].GraphDraw.MaxVertAxis := MaxY;
                    Hists[i].GraphDraw.BottomMargin := 50;
                    Hists[i].GraphDraw.LeftMargin := 60;
                    Hists[i].RedrawDiagram11Click(Nil);
                 end;
              end;
            end;
         end
         else OldCreateHistogramFromDatabase(true,WantXField,'','',(Sender = AllDBs1));
      end;
   end;
{$EndIf}
end;


procedure Tdbtablef.Historgram1Click(Sender: TObject);
var
   Fields : tStringList;
begin
   {$IfDef NoDBGrafs}
   {$Else}
      Fields := tStringList.Create;
      Fields.Add(SelectedColumn);
      //GISdb[DBonTable].OldCreateHistogramFromDatabase(true,SelectedColumn,'','',false);
      //CreateHistogramFromDataBase(RegHist: boolean; Fields : tStringList; AllDBs: boolean; MinUse : float64 = 1; MaxUse : float64 = -1; BinSize : float64 = -99): TThisBaseGraph;
      GISdb[DBonTable].CreateHistogramFromDataBase(true,Fields,false);
      Fields.Destroy;
   {$EndIf}
end;


procedure Tdbtablef.Horizonblocking1Click(Sender: TObject);
var
   Lat,Long : float64;
begin
    if GISdb[DBonTable].ValidLatLongFromTable(Lat,Long) and (GISdb[DBonTable].TheMapOwner <> Nil) then begin
       DEMOptions.SetHorizonOptions(GISdb[DBonTable].TheMapOwner,Lat,Long);
    end;
end;



procedure Tdbtablef.GetReadyForGeologyGeometry;
begin
   {$IfDef ExGeology}
   {$Else}
      GISdb[DBonTable].dbOpts.XField := 'X_UTM';
      GISdb[DBonTable].dbOpts.YField := 'Y_UTM';
      GISdb[DBonTable].dbOpts.ZField := 'Z';

      if not GISdb[DBonTable].MyData.FieldExists('Z') then begin
         GISdb[DBonTable].AddAndFillFieldFromDEM(adElevInterp,'Z');
      end;
      if not GISdb[DBonTable].MyData.FieldExists('X_UTM') then begin
         AddUTMcoordfields1Click(AddUTMcoordfields1);
      end;
      ShowStatus;
   {$EndIf}
end;


procedure Tdbtablef.hreepointproblem1Click(Sender: TObject);
begin
   {$IfDef ExGeology}
   {$Else}
      GetReadyForGeologyGeometry;
      GISdb[DBonTable].AddFieldToDataBase(ftFloat,'STRIKE',7,2);
      GISdb[DBonTable].AddFieldToDataBase(ftFloat,'DIP',6,2);
      GISdb[DBonTable].AddFieldToDataBase(ftString,'DIPSTRIKE',10);
      GISdb[DBonTable].TheMapOwner.ThreePointProblems(DBonTable);
      GISdb[DBonTable].DipStrikeFieldExists := true;
      GISdb[DBonTable].DipAndStrikeFieldsExist := true;
      ShowStatus;
   {$EndIf}
end;


procedure Tdbtablef.N3series1Click(Sender: TObject);
begin
   {$IfDef NoDBGrafs}
   {$Else}
      GISdb[DBonTable].PickNumericFields(dbgtUnspecified,3,'First histogram','Second Histogram','Third histogram');
      GISdb[DBonTable].OldCreateHistogramFromDataBase(true,GISdb[DBonTable].dbOpts.XField,GISdb[DBonTable].dbOpts.YField,GISdb[DBonTable].dbOpts.ZField,false);
   {$EndIf}
end;



procedure Tdbtablef.N45Click(Sender: TObject);
begin
   {$IfDef RecordIceSat} WriteLineRoDebugFile('ICESat2filecleanup1Click'); {$EndIf}
   IcesatProcessCanopy(DBonTable,false,true);
end;

procedure Tdbtablef.N53Click(Sender: TObject);
begin
   try
      GetDEMIXPaths(true);
      Removerowsmissinganyevaluations1Click(Sender);
      RankDEMS(DBonTable);
      EvalRangeAndBestEvalForCriterion(DBonTable);
      CompareSeriousCompetitors(DBonTable);
      AddTileCharacteristicsToDB(DBonTable);
   finally
      EndDEMIXProcessing;
   end;
end;

procedure Tdbtablef.N7Elevationdifferencecriteria1Click(Sender: TObject);
begin
{$IfDef ExDEMIXexperimentalOptions}
{$Else}
   DEMIXwineContestCriterionGraph(dg7Params,DBonTable);
{$EndIf}
end;


procedure Tdbtablef.N2series1Click(Sender: TObject);
begin
   {$IfDef NoDBGrafs}
   {$Else}
      GISdb[DBonTable].PickNumericFields(dbgtUnspecified,2,'First histogram','Second Histogram','Third histogram');
      GISdb[DBonTable].OldCreateHistogramFromDataBase(true,GISdb[DBonTable].dbOpts.XField,GISdb[DBonTable].dbOpts.YField,'',false);
   {$EndIf}
end;


procedure Tdbtablef.BitBtn12Click(Sender: TObject);
begin
   with GISdb[DBonTable] do begin
      Shiftpointrecords1.Visible := ItsaPointDB;
      Exportsortedtable1.Visible := not LineOrAreaShapeFile(ShapeFileType);
      Exportlatlongz1.Visible := ItsAPointDB;
      Createlineshapefilefrompoints1.Visible := ItsAPointDB;
      DataDBFonlynogeometry1.Visible := LineOrAreaShapeFile(ShapeFileType);
      N11.Visible := LineOrAreaShapeFile(ShapeFileType);
      Coordinatestotextfile1.Visible := LineOrAreaShapeFile(ShapeFileType);
      ShapeFileSubset1.Visible := LineOrAreaShapeFile(ShapeFileType);
      Createpointsinpolygons1.Visible := AreaShapeFile(ShapeFileType);
      Createpointsalonglines1.Visible := LineShapeFile(ShapeFileType);
      Createpointsalonglinesbyseparation1.Visible := LineShapeFile(ShapeFileType);
      Createpointshapefile1.Visible := (ItsAPointDB or XYZFile) and MDDef.AdvancedDBops;
      Createpointshxindex1.Visible := (ItsAPointDB or XYZFile) and MDDef.AdvancedDBops;
      ArcGISviewshedsensors1.Visible := ItsFanFile;
      ExportXYZtriples1.Visible := ShapeFile3D(ShapeFileType);
      CreateXYZpointshapefile1.Visible :=  XYZFile;
      Earthquakefocalmechanisms1.Visible := FocalMechsPresent;
      DBFfile1.Visible := GISdb[DBonTable].ItsAPointDB or GISdb[DBonTable].NoGeometry;
      {$IfDef Judomia}
         KML1.Caption := 'KML export';
      {$EndIf}
      Otherdatabaseformats1.Visible := MDDef.AdvancedDBops;
      dbfStruct.Visible := MDDef.AdvancedDBops or (MDDef.ProgramOption = RemoteSensingProgram);
      Shapefilemetadata1.Visible := MDDef.AdvancedDBops;
      Colorarray1.Visible := MDDef.AdvancedDBops;
      Creategrid3.Visible := MDDef.AdvancedDBops;
      Exportlatlongz1.Visible := MDDef.AdvancedDBops;
      Thindatabase2.Visible := MDDef.AdvancedDBops;
      Copyshapefile1.Visible := MDDef.AdvancedDBops;
      Createpointsfile1.Visible := MDDef.AdvancedDBops and LineOrAreaShapeFile(ShapeFileType);
      Createlineshapefilefrompoints1.Visible := MDDef.AdvancedDBops;
      Movie1.Visible := MDDef.AdvancedDBops;
      ColorBarChart1.Visible := GISdb[DBonTable].MyData.FieldExists('COLOR') and GISdb[DBonTable].MyData.FieldExists('PERCENT');
      ReportPopupMenu4.PopUp(Mouse.CursorPos.X,Mouse.CursorPos.Y);
   end;
end;


procedure Tdbtablef.BitBtn13Click(Sender: TObject);
begin
   UnHideColumns;
end;

procedure Tdbtablef.Text1Click(Sender: TObject);
var
   tf : integer;
   Report : tStringList;
   ReportOptionsForm: TReportOptionsForm;
   ch : AnsiChar;
   fname : PathStr;
begin
   {$IfDef RecordCSVOut} WriteLineRoDebugFile('Tdbtablef.Text1Click in'); {$EndIf}

   //with GISdb[DBonTable] do begin
      GISdb[DBonTable].dbIsUnsaved := false;
      tf := 1;
      ch := ',';
      if (Sender = Exporttextdeliberate1) then begin
         ReportOptionsForm := TReportOptionsForm.Create(Application);
         ReportOptionsForm.ShowModal;
         case ReportOptionsForm.RadioGroup1.ItemIndex of
             0 : ch := ' ';
             1 : ch := ',';
             2 : ch := #9;
             3 : ch := '|';
         end;
         tf := ReportOptionsForm.tf;
         ReportOptionsForm.Free;
      end;

      fName := ChangeFileExt(GISdb[DBonTable].DBFullName,'.csv');
      if Petmar.GetFileNameDefaultExt('Text file export','CSV|*.csv|Text|*.txt',fName) then begin
         Report := GISdb[DBonTable].ExtractDBtoCSV(tf,ch);
         Report.SaveToFile(fName);
         Report.Free;
      end;
      ShowStatus;
   //end;
end;


procedure Tdbtablef.HTML1Click(Sender: TObject);
begin
   {$IfDef RecordHTML} WriteLineRoDebugFile('Tdbtablef.HTML1Click in'); {$EndIf}
   GISdb[DBonTable].EmpSource.Enabled := false;
   HTMLReport(GISdb[DBonTable].MyData,GISdb[DBonTable].EmpSource,GISdb[DBonTable].dbOpts.VisCols);
   ShowStatus;
   {$IfDef RecordHTML} WriteLineRoDebugFile('Tdbtablef.HTML1Click out'); {$EndIf}
end;


procedure Tdbtablef.HTMLtableperrecord1Click(Sender: TObject);
begin
   {$IfDef RecordHTML} WriteLineRoDebugFile('Tdbtablef.HTMLtableperrecord1Click in'); {$EndIf}
   SingleRecordHTMLReport(true,GISdb[DBonTable].MyData,GISdb[DBonTable].dbOpts.VisCols);
   {$IfDef RecordHTML} WriteLineRoDebugFile('Tdbtablef.HTMLtableperrecord1Click out'); {$EndIf}
end;

procedure Tdbtablef.Tincontour1Click(Sender: TObject);
begin
  {$IfDef ExTIN}
  {$Else}
     if GISdb[DBonTable].MyData.FieldExists(GISdb[DBonTable].ZFieldName) then begin
        tTIN.Create(GISdb[DBonTable].theMapOwner,GISdb[DBonTable].dbFullName,false);
     end
     else begin
        MessageToContinue('"Z" field required');
     end;
  {$EndIf}
end;


procedure Tdbtablef.Tidepredictions1Click(Sender: TObject);
const
   MaxComp = 37;
var
   i,j,NComp,interval,NumReadings : integer;
   Results : tStringList;
   Sum,Hours,StartHours : float64;
   Amp,Phase,Speed : array[1..MaxComp] of float64;
begin
  Results := tStringList.Create;
  Results.Add('Hour  Day  Year  Predict');
  NComp := 0;
  GISdb[DBonTable].MyData.First;
  while not GISdb[DBonTable].MyData.EOF do begin
     inc(NComp);
     Amp[NComp] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat('AMPLITUDE');
     Phase[NComp] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat('PHASE');
     Speed[NComp] := GISdb[DBonTable].MyData.GetFieldByNameAsFloat('SPEED');
     GISdb[DBonTable].MyData.Next;
  end;
  i := 0;
  Interval := 2;
  Numreadings := 500000;
  ReadDefault('Hours for record',NumReadings);
  ReadDefault('Interval (hours)',Interval);

  StartHours := 24 * EncodeDate(1900,1,1);
  Hours := 24 * EncodeDate(2011,1,1);

  StartHours := 24 * JulDay(1,1,1900);
  Hours := 24 * JulDay(1,1,2011);

  while i < (NumReadings div interval) do begin
     Sum := 0;
     for j := 1 to NComp do begin
        Sum := Sum + Amp[j] * SinDeg(Phase[j] + (Hours-StartHours) * Speed[j]);
     end;
     Results.Add(IntToStr(i) + RealToString(Hours/24,18,4) + RealToString(Hours/24/365.25,24,8) + RealToString(Sum,12,4));
     Hours := Hours + Interval;
     inc(i,Interval);
  end;
  Results.SaveToFile(ExtractFilePath(GISdb[DBonTable].dbFullName) + 'tides.csv');
end;


procedure Tdbtablef.TimefieldstodecJuliandays1Click(Sender: TObject);
begin
   GISdb[DBonTable].AddFieldToDataBase(ftFloat,'DEC_JULDAY',12,6);
   GISdb[DBonTable].EmpSource.Enabled := false;
   GISdb[DBonTable].MyData.First;
   while not GISdb[DBonTable].MyData.EOF do begin
      GISdb[DBonTable].MyData.Edit;
      GISdb[DBonTable].MyData.SetFieldByNameAsFloat('DEC_JULDAY',GISdb[DBonTable].MyData.GetFieldByNameAsInteger('JULIAN_DAY') +
          GISdb[DBonTable].MyData.GetFieldByNameAsInteger('HOUR')/24 + GISdb[DBonTable].MyData.GetFieldByNameAsInteger('MINUTE')/60/24 +
          GISdb[DBonTable].MyData.GetFieldByNameAsInteger('SECOND')/3600/24);
      GISdb[DBonTable].MyData.Next;
   end;
   ShowStatus;
end;

procedure Tdbtablef.Timefieldstodecyears1Click(Sender: TObject);
begin
   GISdb[DBonTable].TimeFieldsToDecYears;
end;


procedure Tdbtablef.BitBtn7Click(Sender: TObject);
begin
   {$IfDef RecordDataBase} WriteLineRoDebugFile('Tdbtablef.BitBtn7Click--map query'); {$EndIf}
   DBEditting := DBonTable;
   NearTIGERroads1.Visible := MDDef.AdvancedDBops and (GISdb[DBonTable].TheMapOwner <> Nil) and (GISdb[DBonTable].ItsAPointDB);
   AwayfromTIGERroads1.Visible := NearTIGERroads1.Visible;
   FindAddress1.Visible := GISdb[DBonTable].ItsTigerShapeFile;
   ClearGeographicFilter1.Visible := GISdb[DBonTable].dbOpts.GeoFilter <> '';
   QueryPopupMenu5.PopUp(Mouse.CursorPos.X,Mouse.CursorPos.Y);
end;


procedure Tdbtablef.Satelliteaddreflectance1Click(Sender: TObject);
{$ifDef ExSat}
begin
{$Else}
//label
   //OffImage;
var
   xg,yg : float32;
   Lat,Long : float64;
   i,j,Col,Row,DN,rc : integer;
   fName : ShortString;
begin
   {$IfDef RecordSatellite} WriteLineRoDebugFile('Tdbtablef.Satelliteaddreflectance1Click in'); {$EndIf}
   //with GISdb[DBonTable] do begin
      for j := 1 to SatImage[GISdb[DBonTable].TheMapOwner.MapDraw.SATonMap].NumBands do begin
         {$IfDef RecordSatellite} WriteLineRoDebugFile('add ' + 'BAND_' + IntToStr(j)); {$EndIf}
         GISdb[DBonTable].AddFieldToDataBase(ftInteger,'BAND_' + IntToStr(j),6);
      end;
      ShowHourglassCursor;
      i := 0;
      rc := GISdb[DBonTable].MyData.RecordCount;
      StartProgress('Sat DN add');
      GISdb[DBonTable].MyData.First;
      while not GISdb[DBonTable].MyData.Eof do begin
         if (I mod 50 = 0) then begin
            UpdateProgressBar(i/rc);
            GISdb[DBonTable].EmpSource.Enabled := false;
         end;
         inc(i);

         if GISdb[DBonTable].ValidLatLongFromTable(Lat,Long) then begin
             for j :=  1 to SatImage[GISdb[DBonTable].TheMapOwner.MapDraw.SATonMap].NumBands do begin
                SatImage[GISdb[DBonTable].TheMapOwner.MapDraw.SATonMap].LatLongDegreeToSatGrid(j,Lat,Long,xg,yg);
                Col := round(xg);
                Row := round(yg);
                if SatImage[GISdb[DBonTable].TheMapOwner.MapDraw.SATonMap].SatGridInDataSet(j,Col,Row) then begin
                   DN := SatImage[GISdb[DBonTable].TheMapOwner.MapDraw.SATonMap].GetSatPointValue(j,Col,Row);
                   GISdb[DBonTable].MyData.Edit;
                   fName := 'BAND_' + IntToStr(j);
                   GISdb[DBonTable].MyData.SetFieldByNameAsInteger(fName,DN);
                end;
             end;
         end;
         GISdb[DBonTable].MyData.Next;
         //OffImage:;
      end;
      ShowStatus;
   //end;
{$EndIf}
end;

procedure Tdbtablef.SearchAndReplace(aField : ShortString; Before,After : ANSIString; var Changed : integer);
var
   aString,NewString : AnsiString;
   i,nc,rc : integer;
begin
   GISdb[DBonTable].EmpSource.Enabled := false;
   GISdb[DBonTable].MyData.Last;
   Changed := 0;
   GISdb[DBonTable].MyData.ProgressVars(rc,nc);
   i := 0;
   StartProgress('Seach and replace');
   while not GISdb[DBonTable].MyData.bof do begin
     inc(i);
     if (i mod rc = 0) then UpdateProgressBar(i/nc);
     aString := GISdb[DBonTable].MyData.GetFieldByNameAsString(aField);
     NewString := StringReplace(aString,before,after,[rfReplaceAll, rfIgnoreCase]);
     if (aString <> NewString) then begin
        GISdb[DBonTable].MyData.Edit;
        GISdb[DBonTable].MyData.SetFieldByNameAsString(aField,NewString);
        inc(Changed);
     end;
     GISdb[DBonTable].MyData.Prior;
   end;
end;


procedure Tdbtablef.Searchandreplace1Click(Sender: TObject);
begin
   ReplaceDialog1.Execute;
end;


procedure Tdbtablef.Selectallrecordsafterthisone1Click(Sender: TObject);
var
   ThisRec : integer;
begin
   ThisRec := GISdb[DBonTable].MyData.GetFieldByNameAsInteger('REC_ID');
   GISdb[DBonTable].ApplyGISFilter('REC_ID > ' + IntToStr(ThisRec));
   GISdb[DBonTable].RedrawLayerOnMap;
   ShowStatus;
end;

procedure Tdbtablef.Selectfields1Click(Sender: TObject);
begin
   ManageDBDisplay(DBGrid1,DBonTable);
   GISdb[DBonTable].DisplayFieldStatistics('SELECT-FIELDS');
end;

procedure Tdbtablef.SelectionRegion1Click(Sender: TObject);
begin
   {$IfDef RecordDataBase} WriteLineRoDebugFile('Tdbtablef.Selectionregion1Click--DB map query in'); {$EndIf}

   if PointShapeFile(GISdb[DBonTable].ShapeFileType) and (not GISdb[DBonTable].LatLongFieldsPresent) then begin
      AddXYZfromshpfile1Click(Addlatlongfromshpfile1);
   end;
   if LineOrAreaShapeFile(GISdb[DBonTable].ShapeFileType) and (not GISdb[DBonTable].LatLongFieldsPresent) then begin
      GISdb[DBonTable].AddBoundingBox;
   end;

   if (GISdb[DBonTable].theMapOwner <> Nil) then begin
      DBEditting := DBonTable;
      DEMMapf.ChangeDEMNowDoing(GraphFilterDB);
      GISdb[DBonTable].theMapOwner.SetFocus;
   end;
   {$IfDef NoDBGrafs}
   {$Else}
      if (GISdb[DBonTable].theGraphOwner <> Nil) then ChangeGraphDoing(gdGraphDBBoxFilter);
   {$EndIf}
   ShowStatus;
   {$IfDef RecordDataBase} WriteLineRoDebugFile('Tdbtablef.Selectionregion1Click--DB map query out'); {$EndIf}
end;


procedure Tdbtablef.Selectradiusaboutpoint1Click(Sender: TObject);
begin
   {$IfDef RecordDataBase} WriteLineRoDebugFile('Tdbtablef.Selectradiusaboutpoint1Click'); {$EndIf}
   ChangeDEMNowDoing(RadiusDBEdit);
end;


procedure Tdbtablef.Setjoin1Click(Sender: TObject);
begin
   {$IfDef RecordDataBase} WriteLineRoDebugFile(' Tdbtablef.Linkdatabase1Click'); {$EndIf}
   GISdb[DBonTable].ClearLinkTable(true);
   GISdb[DBonTable].LinkSecondaryTable(GISdb[DBonTable].dbOpts.LinkTableName);
end;

procedure Tdbtablef.Selectirregularregion1Click(Sender: TObject);
begin
   {$IfDef RecordDataBase} WriteLineRoDebugFile('Tdbtablef.Selectradiusaboutpoint1Click'); {$EndIf}
   GISdb[DBonTable].dbOpts.GeoFilter := '';
   GISdb[DBonTable].theMapOwner.StartShapeFile(OutlineDBIrregularMask);
end;


procedure Tdbtablef.Selectlinestodisplay1Click(Sender: TObject);
begin
   {$IfDef ExSidescan}
   {$Else}
     GISdb[SideIndexDB].ClearGISFilter;
     Toggle_db_use.VerifyRecordsToUse(GISdb[SideIndexDB].MyData,'LINE_NAME','Sidescan Records to use');
     GISdb[SideIndexDB].MyData.ApplyFilter('USE=' + QuotedStr('Y'));
     GISdb[DBonTable].TheMapOwner.DoCompleteMapRedraw;
   {$EndIf}
end;


procedure Tdbtablef.Cumulative1Click(Sender: TObject);
var
   i,j,Cum : integer;
   Npts,Missing : int64;
   aField,sfield : ShortString;
   Min,Max : float64;
   zs : ^bfarray32;
   ThisGraph   : TThisBaseGraph;
   rFile : file;
   Subs : tStringList;
begin
  aField := GISdb[DBonTable].PickField('time variable',[ftInteger,ftSmallInt,ftFloat]);
  sField := GISdb[DBonTable].PickField('sort',[ftString]);

  Subs := nil;
  Subs := GISdb[DBonTable].MyData.ListUniqueEntriesInDB(sField);

  GISdb[DBonTable].EmpSource.Enabled := false;

  ThisGraph := TThisBaseGraph.Create(Application);
  ThisGraph.GraphDraw.LegendList := tStringList.create;

   for j := 0 to pred(subs.Count) do begin
      GISdb[DBonTable].ApplyGISFilter(sField + '=' + QuotedStr(Subs.Strings[j]));
      New(zs);
      GetFieldValuesInArray(GISdb[DBonTable].MyData,aField,zs^,Npts,Missing,Min,Max);
      PetMath.HeapSort(NPts,zs^);
      ThisGraph.OpenDataFile(rfile);
      ThisGraph.GraphDraw.LegendList.Add(Subs.Strings[j] + '  n=' + IntToStr(NPts));
      ShowHourglassCursor;
      Cum := 0;
      for i := 0 to pred(Npts) do begin
          inc(cum);
          ThisGraph.AddPointToDataBuffer(rfile,zs^[i],100 * Cum / NPts);
      end;
      ThisGraph.ClosePointDataFile(rfile);
      Dispose(zs);
   end;

   ThisGraph.GraphDraw.HorizLabel := aField;
   ThisGraph.GraphDraw.VertLabel := 'Cumulative percentile';
   ThisGraph.AutoScaleAndRedrawDiagram;
   ThisGraph.Caption := aField;
   GISdb[DBonTable].ApplyGISFilter('');
   ShowStatus;
end;

procedure Tdbtablef.Currentfilter1Click(Sender: TObject);
var
   fName : PathStr;
   db : integer;
begin
   fName := ExtractFilePath(LastDataBase);
   if GetFileNameDefaultExt('Shape file',DBNameMask,FName,false) then begin
      CheckFileNameForSpaces(fName);
      GISdb[DBonTable].SubsetShapeFile(fName,1,true);
      HideColumns;
      if AnswerIsYes('Load new file') then begin
         db := GISdb[DBonTable].theMapOwner.OpenDBonMap('',fName);
         GISdb[db].AddSequentialIndex(RecNoFName);
      end;
   end;
end;

procedure Tdbtablef.CurrentflterYrestN1Click(Sender: TObject);
begin
   if GISdb[DBonTable].MyData.FieldExists('USE') then begin
      GISdb[DBonTable].SaveFilterStatus(true);
      GISdb[DBonTable].ClearGISFilter;
      GISdb[DBonTable].FillUseField(false,'N');
      if GISdb[DBonTable].ItsFanFile then begin
         GISdb[DBonTable].TheMapOwner.MapDraw.DeleteSingleMapLayer(GISdb[DBonTable].TheMapOwner.MapDraw.AllFansCoverageFName);
      end;
      GISdb[DBonTable].RestoreFilterStatus;
      GISdb[DBonTable].FillUseField(false,'Y');
   end;
end;

procedure Tdbtablef.Currentmaparea1Click(Sender: TObject);
begin
   {$IfDef RecordDataBase} WriteLineRoDebugFile('Tdbtablef.Currentmaparea1Click'); {$EndIf}
   GISdb[DBonTable].LimitDBtoMapArea;
end;


procedure Tdbtablef.Currenttest1Click(Sender: TObject);
begin
   //DEMIX_evaluations_graph(DBonTable);
end;


procedure Tdbtablef.Cyclethroughterrainprofiles1Click(Sender: TObject);
var
   thisGraph : tThisBaseGraph;
   DelayTime,i,rc : integer;
begin
   DelayTime := 250;
   ReadDefault('Recs on map: ' + IntToStr(GISdb[DBonTable].MyData.RecordCount) + ';  Delay (ms)',DelayTime);
   StartProgressAbortOption('Profiles');
   i := 0;
   rc := GISdb[DBonTable].MyData.RecordCount;
   GISdb[DBonTable].MyData.First;
   repeat
      inc(i);
      UpdateProgressBar(i/rc);
      ThisGraph := DrawDEMTerrainprofile(Normalizedbasinprofile1);
      Delay(DelayTime);
      ThisGraph.Destroy;
      GISdb[DBonTable].MyData.Next;
   until GISdb[DBonTable].MyData.Eof or WantOut;
end;


procedure Tdbtablef.Lidarwaveform1Click(Sender: TObject);
begin
   {$IfDef NoDBGrafs}
   {$Else}
      GISdb[DBonTable].WaveFormGraph(true);
   {$EndIf}
end;

procedure Tdbtablef.Lidarwaveforms1Click(Sender: TObject);
begin
   {$IfDef NoDBGrafs}
   {$Else}
      GISdb[DBonTable].WaveFormGraph(false);
   {$EndIf}
end;



procedure Tdbtablef.Limitdecimalplaces1Click(Sender: TObject);
var
   NumDec : integer;
begin
   NumDec := 3;
   ReadDefault('decimals to retain',NumDec);
   GISdb[DBonTable].LimitFieldDecimals(SelectedColumn,NumDec);
end;


procedure Tdbtablef.Linearinterpolateacrossgaps1Click(Sender: TObject);
var
   YField,ZField : ShortString;
begin
   yField := GISdb[DBonTable].PickField('time variable',[ftInteger,ftSmallInt,ftFloat]);
   zField := GISdb[DBonTable].PickField('Z field',[ftFloat]);
   GISdb[DBonTable].LinearInterpolateAcrossGap(yfield,zField);
end;


procedure Tdbtablef.Linelength1Click(Sender: TObject);
var
   Sum : float64;
   Success : boolean;
begin
   GISdb[DBonTable].aShapeFile := tShapeFile.Create(GISdb[DBonTable].ShapeFileName,Success);
   Sum := 0;
   GISdb[DBonTable].EmpSource.Enabled := false;
   GISdb[DBonTable].MyData.First;
   repeat
     sum := sum + GISdb[DBonTable].aShapeFile.LineLength(GISdb[DBonTable].MyData.RecNo);
     GISdb[DBonTable].MyData.Next;
   until GISdb[DBonTable].MyData.EOF;
   GISdb[DBonTable].aShapeFile.Destroy;
   ShowStatus;
   MessageToContinue('Segments: '+ IntToStr(GISdb[DBonTable].MyData.RecordCount) + MessLineBreak + 'Line Length: ' + RealToString(Sum,-12,0) + ' m',True);
end;

function MakeCell(Text : ShortString) : AnsiString;
begin
   Result := StartColumnString + Text + EndColumnString;
end;


procedure Tdbtablef.Linelengthsforonefield1Click(Sender: TObject);
var
   NumPoints,Run,NumRuns,TotalPoints : integer;
   TotalLength,Sum : float64;
   OldFilter : String;
   Sortings,HTMLResult : tStringList;
   fName : PathStr;
   SortField,SortStr : ShortString;
begin
   ShowHourglassCursor;
   HTMLResult:= tStringList.Create;
   HTMLResult.Add(StartHTMLString + GISdb[DBonTable].dbName + '<br><br>' + 'Filtered with ' + GISdb[DBonTable].MyData.Filter + '<br><br>');
   HTMLResult.Add('<table border="3">');
   HTMLResult.Add('<tr>' + MakeCell('Category') + MakeCell('Number of Lines') + MakeCell('Line length (m)') + '</tr>');

   GISdb[DBonTable].EmpSource.Enabled := false;
   OldFilter := GISdb[DBonTable].MyData.Filter;

   SortField :=  GISdb[DBonTable].PickField('sorting',[ftString]);
   GISdb[DBonTable].DBFieldUniqueEntries(SortField,Sortings);
   NumRuns := Sortings.Count;
   TotalPoints := 0;
   TotalLength := 0;
   for Run := 1 to NumRuns do begin
      NumPoints := 0;

      with GISdb[DBonTable],MyData do begin
           SortStr := SortField + ' = ' + QuotedStr(Sortings.Strings[pred(Run)]);
           if OldFilter  = '' then GISdb[DBonTable].MyData.ApplyFilter( SortStr)
           else GISdb[DBonTable].MyData.ApplyFilter( OldFilter + ' AND ' + SortStr);
           Sum := 0;
           First;
           repeat
              sum := sum + aShapeFile.LineLength(MyData.RecNo);
              inc(NumPoints);
              Next;
           until EOF;
           if (NumPoints > 0) then begin
              HTMLResult.Add('<tr>' + MakeCell(SortStr) + MakeCell(IntToStr(NumPoints)) +  MakeCell(RealToString(Sum,-12,0)) + '</tr>');
              Inc(TotalPoints,NumPoints);
              TotalLength := TotalLength + Sum;
           end;
      end;
   end;

   HTMLResult.Add('<tr>' + MakeCell(' ') + MakeCell(IntToStr(TotalPoints)) + MakeCell(RealToString(TotalLength,-12,0)) + '</tr>');
   HTMLResult.Add(EndTableString);
   HTMLResult.Add(EndHTMLString);
   fName := MDTempDir + 'length_table.htm';
   HTMLResult.SaveToFile(fName);
   ExecuteFile(fName, '', '');
   GISdb[DBonTable].MyData.ApplyFilter(OldFilter);
   ShowStatus;
end;


procedure Tdbtablef.Lineshapefile1Click(Sender: TObject);
begin
   {$IfDef RecordMakeLineArea} WriteLineRoDebugFile('Tdbtablef.Createlineshapefilefrompoints1Click ' + GISDataBase[DBonTable].dbname); {$EndIf}
   MakeLinesFromPoints(GISdb[DBonTable],'',3);
   ShowStatus;
end;

procedure Tdbtablef.Twofields1Click(Sender: TObject);
begin
   Countuniquevalues1Click(Sender);
end;

procedure Tdbtablef.Button4Click(Sender: TObject);
begin
   with GISdb[DBonTable] do begin
      {$IfDef RecordEditDB} WriteLineRoDebugFile('Tdbtablef.Button4Click (edit) in'); {$EndIf}
      AddSequentialIndex(RecNoFName,false);
      Colorallrecords1.Visible := ColorPresent;
      Coloruncoloredrecords1.Visible := ColorPresent;
      ColorBasedOnField1.Visible := ColorPresent;
      Coloralluncoloredlinesegments1.Visible := LineColorPresent and LineShapeFile(ShapeFileType);
      Directionofeachrecord1.Visible := LineShapeFile(ShapeFileType);
      Coloralllinesegments1.Visible := LineColorPresent and LineShapeFile(ShapeFileType);
      ColorfromRGBfloatfields1.Visible := GISdb[DBonTable].MyData.FieldExists('RED') and GISdb[DBonTable].MyData.IsFloatField('RED') and GISdb[DBonTable].MyData.FieldExists('GREEN') and GISdb[DBonTable].MyData.FieldExists('BLUE');
      Colorbasedonjoinedtable1.Visible := GISdb[DBonTable].MyData.FieldExists('COLOR') and (LinkTable <> Nil) and LinkTable.FieldExists('COLOR');

      {$IfDef ExGeography}
         ColorfromKoppencategory1.Visible := false;
         Koppenicons1.Visible := false;
         Dayssincefullmoon1.Enabled := false;
      {$Else}
         ColorfromKoppencategory1.Visible := (not ColorPresent) and KoppenPresent;
         Koppenicons1.Visible := KoppenPresent;
         Dayssincefullmoon1.Enabled := Insertdatefield1.Enabled;
      {$EndIf}

      Insertareacolorsymbology1.Visible := (not AreaFillPresent) and AreaShapeFile(ShapeFileType);
      Fillpatternallrecords1.Visible := AreaFillPresent and AreaShapeFile(ShapeFileType);
      Fillpatternalluncoloredrecords1.Visible := AreaFillPresent and AreaShapeFile(ShapeFileType);

      Insertlinecolorwidthfields1.Visible := (not LineColorPresent) and LineShapeFile(ShapeFileType);
      Date1.Visible := MDDef.ShowDBDateTimeSeries;
      Insertcolorfield1.Visible := (not GISdb[DBonTable].MyData.FieldExists('COLOR'));

      UseField1.Visible := GISdb[DBonTable].MyData.FieldExists('USE');
      Grpahicallyinsertpoint1.Visible := ItsaPointDB;
      Graphicallymovepoints1.Visible := ItsaPointDB;
      Geocodelatlong1.Visible := ItsaPointDB;
      Randomizeduplicatepoints1.Visible := ItsaPointDB;
      Viewshedfields1.Visible := ItsaPointDB and (not ItsFanFile);
      Copydatafiles2.Visible := FileNameFieldExists or DEMIndex or ImagePresent;
      InsertRecord1.Visible := NoGeometry or ItsaPointDB;
      DeleteAllRecords1.Visible := not LineOrAreaShapeFile(ShapeFileType);

      ChangeFieldsUsed1.Visible := GISdb[DBonTable].MyData.FieldExists('USE');
      Distancefrompoint1.Visible := (TheMapOwner <> Nil);
      DEM1.Visible := ((TheMapOwner <> Nil) and ValidDEM(TheMapOwner.MapDraw.DEMonMap)) or (NumDEMDataSetsOpen > 0);
      MaskDEMfromshapefile1.Visible := (NumDEMDataSetsOpen > 0);
      Satelliteaddreflectance1.Visible := (TheMapOwner <> Nil) and ValidSatImage(TheMapOwner.MapDraw.SatonMap);
      Editrecordsingrid1.Visible := MDDef.AllowEditDBInGrid;
      Maskdatabasewithgazetteer1.Visible := GazDB and ValidDEM(TheMapOwner.MapDraw.DEMonMap);
      Networkends1.Visible := MDDef.ShowGeomorphometry;
      Mapcoordinates1.Visible := (GISdb[DBonTable].CanPlot and GISdb[DBonTable].DBhasMapOrGraph) or MDDef.AlwaysShowMapCoordinates;
      AverageOfNeighbors2.Visible := (NeighborTable <> Nil);
      SumOfNeighbors1.Visible := (NeighborTable <> Nil);
      Addlatlongfromlinkeddatabase1.Visible := (LinkTable <> Nil) and LinkTable.FieldExists('LAT') and LinkTable.FieldExists('LONG');
      AddXYZfromjoinedtable1.Visible := (LinkTable <> Nil) and LinkTable.FieldExists('X') and LinkTable.FieldExists('Y') and LinkTable.FieldExists('Z');
      RecolorAllFans1.Visible := ItsFanFile;
      Fillviewshedfields1.Visible := ItsFanFile;
      Areaofeachrecord1.Visible := AreaShapeFile(ShapeFileType);
      Perimeterofeachrecord1.Visible := AreaShapeFile(ShapeFileType);
      Centroidofeachrecord1.Visible := LineOrAreaShapeFile(ShapeFileType);
      LengthOfeachrecord1.Visible := LineShapeFile(ShapeFileType);
      Sinuosity1.Visible := LineShapeFile(ShapeFileType);
      Elevationchangeofeachrecord1.Visible := ShapeFileType in [13,23];
      AddUTMfromoffsets1.Visible := GISdb[DBonTable].MyData.FieldExists('X_OFF') and GISdb[DBonTable].MyData.FieldExists('Y_OFF');

      InsertWWWField1.Visible := (not WWWPresent);
      InsertImageField1.Visible := (not ImagePresent);
      f1.Visible := (LinkTable <> Nil);
      EvaluateXYProfiles1.Visible := GISdb[DBonTable].MyData.FieldExists('XY_FILE');
      InsertNewRecClipboard.Visible := (LineOrAreaShapeFile(ShapeFileType) and (ClipBoard_Line_Coords <> Nil)) or (ClipBoard_Coords and ItsaPointDB);
      MergeDataBases1.Visible := not LineOrAreaShapeFile(ShapeFileType);


      Octree1.Visible := MDDef.ShowExperimentalOptions;
      Multiplefieldstatistics1.Visible := (MDDef.ProgramOption = ExpertProgram) and (NumericFields > 1);
      FieldArithmetic1.Visible := (MDDef.ProgramOption = ExpertProgram) and (NumericFields > 0);

      Formatdatefield1.Enabled := GISdb[DBonTable].MyData.FieldExists('DATE_LABEL');

      Insertdatefield1.Enabled := GISdb[DBonTable].MyData.FieldExists(MonthFieldName) and GISdb[DBonTable].MyData.FieldExists('DAY') and GISdb[DBonTable].MyData.FieldExists('YEAR');
      InsertJuliandate1.Enabled := Insertdatefield1.Enabled;
      DayofweekfromYRMonDay1.Enabled := Insertdatefield1.Enabled;

      TimefieldstodecJuliandays1.Enabled := GISdb[DBonTable].MyData.FieldExists('JULIAN_DAY') and GISdb[DBonTable].MyData.FieldExists('HOUR') and GISdb[DBonTable].MyData.FieldExists('MINUTE') and GISdb[DBonTable].MyData.FieldExists('SECOND');
      Timefieldstodecyears1.Enabled := GISdb[DBonTable].MyData.FieldExists('YEAR') and GISdb[DBonTable].MyData.FieldExists(MonthFieldName);
      Splitdatefield1.Enabled := GISdb[DBonTable].MyData.FieldExists('DATE') or GISdb[DBonTable].MyData.FieldExists('ACQ_DATE') or GISdb[DBonTable].MyData.FieldExists('DATE_LABEL') or GISdb[DBonTable].MyData.FieldExists('DATEOFOCC');
      SplittimestringHHMMSS1.Enabled := GISdb[DBonTable].MyData.FieldExists('TIME_STR');
      TimefieldHHMMSStohours1.Enabled := GISdb[DBonTable].MyData.FieldExists('TIME');
      Timefieldshourminsectodechours1.Enabled := GISdb[DBonTable].MyData.FieldExists('HOUR') and GISdb[DBonTable].MyData.FieldExists('MINUTE') and GISdb[DBonTable].MyData.FieldExists('SECOND');
      Timefieldstodecdays1.Enabled := GISdb[DBonTable].MyData.FieldExists('DAY') and GISdb[DBonTable].MyData.FieldExists('HOUR');
      DechourstoHHMMSS1.Enabled := GISdb[DBonTable].MyData.FieldExists('DEC_HOURS');
      Inserttimeanimationfields1.Enabled := GISdb[DBonTable].MyData.FieldExists('YEAR');
      SplittimefieldHRMN1.Enabled := GISdb[DBonTable].MyData.FieldExists('HRMN');

      Geocodeaddresses1.Visible :=  GISdb[DBonTable].MyData.FieldExists('ADDRESS') and (TheMapOwner <> Nil);
      Oceanography1.Visible := (MDDef.ProgramOption = ExpertProgram) and MDDef.ShowOceanographyOptions and GISdb[DBonTable].MyData.FieldExists('TEMP_C') and GISdb[DBonTable].MyData.FieldExists('SALINITY') and GISdb[DBonTable].MyData.FieldExists('DEPTH');
      AddXYZfromshpfile1.Visible := PointShapeFile(ShapeFileType);
      AllpointsinboxallopenDBs1.Visible := (NumOpenDB > 0);
      AddDISTANCEAZIMUTHfields1.Visible := SecondLatLongFieldsPresent;
      Distancebetweentwopointsinrecord1.Visible := SecondLatLongFieldsPresent;
      EditPopupMenu8.PopUp(Mouse.CursorPos.X,Mouse.CursorPos.Y);
   end;
end;


procedure Tdbtablef.Colorallrecords1Click(Sender: TObject);
var
   fName : ShortString;
   i : integer;
begin
   {$IfDef RecordEditDB} WriteLineRoDebugFile('Tdbtablef.Colorallrecords1Click in'); {$EndIf}
   if GISdb[DBonTable].MyData.FieldExists('COLOR') then fName := 'COLOR'
   else fName := 'LINE_COLOR';
   if (GISdb[DBonTable].MyData.GetFieldByNameAsString(fName) = '') then ColorDialog1.Color := clBlack
   else ColorDialog1.Color := GISdb[DBonTable].MyData.GetFieldByNameAsInteger(fName);
   if ColorDialog1.Execute then begin
      GISdb[DBonTable].EmpSource.Enabled := false;
      StartProgress('Color field');
      i := 0;
      GISdb[DBonTable].MyData.First;
      repeat
         if i mod 100 = 0 then UpdateProgressBar(i/ GISdb[DBonTable].MyData.FiltRecsInDB);

         GISdb[DBonTable].MyData.Edit;
         if (Sender <> Coloruncoloredrecords1) or (GISdb[DBonTable].MyData.GetFieldByNameAsString(fName) = '') then GISdb[DBonTable].MyData.SetFieldByNameAsInteger(fName,ColorDialog1.Color);
         GISdb[DBonTable].MyData.Next;
      until GISdb[DBonTable].MyData.EOF;
   end;
   GISdb[DBonTable].ShowStatus;
   {$IfDef RecordEditDB} WriteLineRoDebugFile('Tdbtablef.Colorallrecords1Click out'); {$EndIf}
end;

procedure Tdbtablef.Colorallrecords2Click(Sender: TObject);
begin
   Colorallrecords1Click(Colorallrecords1);
end;

procedure Tdbtablef.Colorallrecordsrainbowscale1Click(Sender: TObject);
begin
   Colorallrecordsterrainscale1Click(Sender);
end;

procedure Tdbtablef.Colorallrecordsspectrumscale1Click(Sender: TObject);
begin
    Colorallrecordsterrainscale1Click(Sender);
end;

procedure Tdbtablef.ColorAllRecordsTerrainScale1Click(Sender: TObject);
var
   i,rc : integer;
   Color : tColor;
begin
   GISdb[DBonTable].MyData.First;
   i := 0;
   rc := GISdb[DBonTable].MyData.FiltRecsInDB;
   while not GISdb[DBonTable].MyData.eof do begin
      GISdb[DBonTable].MyData.Edit;
      if Sender=Colorallrecordsspectrumscale1 then Color := Petmar.SpectrumColorFunct(i,0,rc)
      else if (Sender=Colorallrecordsrainbowscale1) then Color := RainbowColorFunct(i,0,rc)
      else Color := TerrainTColor(i,0,rc);
      GISdb[DBonTable].MyData.SetFieldByNameAsInteger('COLOR',Color);
      inc(i);
      GISdb[DBonTable].MyData.Next;
   end;
  ShowStatus;
end;

procedure Tdbtablef.Coloruncoloredrecords1Click(Sender: TObject);
begin
   Colorallrecords1Click(Sender);
end;

procedure Tdbtablef.Columnoperations1Click(Sender: TObject);
begin
   SelectedColumn := GISdb[DBonTable].PickField('Column operations',[ftString,ftInteger,ftSmallInt,ftFloat]);
   SelectedColumnType := GISdb[DBonTable].MyData.GetFieldType(SelectedColumn);
   ColumnOps;
end;

procedure Tdbtablef.ComboBox4Change(Sender: TObject);
//var
   //f1,f2,f3,f4,ff : AnsiString;

   function QuoteIfNeeded(ffName : shortstring; ComboBoxText : shortstring) : shortstring;
   begin
      if GISdb[DBonTable].MyData.IsStringField(ffName) then Result := ffName + '=' + QuotedStr(ComboBoxText)
      else Result := ffName + '=' + ComboBoxText
   end;

begin
(*
   if ComboBox4.Text <> '' then f1 := QuoteIfNeeded(ffName1,ComboBox4.Text) else f1 := '';
   if ComboBox5.Text <> '' then f2 := QuoteIfNeeded(ffName2,ComboBox5.Text) else f2 := '';
   if ComboBox6.Text <> '' then f3 := QuoteIfNeeded(ffName3,ComboBox6.Text) else f3 := '';
   if ComboBox7.Text <> '' then f4 := QuoteIfNeeded(ffName4,ComboBox7.Text) else f4 := '';

   if (f1 = '') and (f2 = '') and (f3 = '') and (f4 = '') then begin
      GISdb[DBonTable].ClearGISFilter;
   end
   else begin
      ff := f1;
      if (f2 <> '') then ff := PetDBUtils.AddAndIfNeeded(ff) + f2;
      if (f3 <> '') then ff := PetDBUtils.AddAndIfNeeded(ff) + f3;
      if (f4 <> '') then ff := PetDBUtils.AddAndIfNeeded(ff) + f4;
      GISdb[DBonTable].dbOpts.MainFilter := ff;
      GISdb[DBonTable].AssembleGISFilter;
   end;
   ShowStatus;
   *)
end;

procedure Tdbtablef.ComboBox5Change(Sender: TObject);
begin
   ComboBox4Change(Sender);
end;


procedure Tdbtablef.ComboBox6Change(Sender: TObject);
begin
   ComboBox4Change(Sender);
end;


procedure Tdbtablef.ComboBox7Change(Sender: TObject);
begin
   ComboBox4Change(Sender);
end;


procedure Tdbtablef.ShowFilteredDB(ShowFilter,ShowN : boolean);
var
   ff : shortstring;
begin
  {$IfDef RecordQuickFilter} WriteLineRoDebugFile('Tdbtablef.ShowFilteredDB in'); {$EndIf}
   GISdb[DBonTable].EmpSource.Enabled := false;
   if (GISdb[DBonTable].TheMapOwner <> Nil) then begin
      GISdb[DBonTable].RedrawLayerOnMap;
   end;
   {$IfDef NoDBGrafs}
   {$Else}
      if (GISdb[DBonTable].TheGraphOwner <> Nil) and (GraphOwnerBitmap <> Nil) then begin
         GISdb[DBonTable].TheGraphOwner.Image1.Picture.Graphic := GraphOwnerBitmap;
      end;
   {$EndIf}

   if ShowFilter or ShowN then begin
      ff := '';
      if ShowFilter then ff := GISdb[DBonTable].MyData.Filter + '   ';
      if ShowN then ff := ff + 'n=' + IntToStr(GISdb[DBonTable].MyData.RecordCount);
      if (GISdb[DBonTable].TheMapOwner <> Nil) then begin
         LoadMyFontIntoWindowsFont(GISdb[DBonTable].dbOpts.GisLabelFont1,GISdb[DBonTable].TheMapOwner.Image1.Canvas.Font);
         GISdb[DBonTable].TheMapOwner.Image1.Canvas.TextOut(0,0,ff);
      end;
     {$IfDef NoDBGrafs}
     {$Else}
      if (GISdb[DBonTable].theGraphOwner <> Nil) then begin
         LoadMyFontIntoWindowsFont(GISdb[DBonTable].dbOpts.GisLabelFont1,GISdb[DBonTable].TheGraphOwner.Image1.Canvas.Font);
         GISdb[DBonTable].TheGraphOwner.Image1.Canvas.TextOut(GISdb[DBonTable].TheGraphOwner.GraphDraw.LeftMargin,5,ff);
      end;
      {$EndIf}
   end;
   ShowStatus;
  {$IfDef RecordQuickFilter} WriteLineRoDebugFile('Tdbtablef.ShowFilteredDB out'); {$EndIf}
end;


procedure Tdbtablef.Compactness1Click(Sender: TObject);
begin
   GISdb[DBonTable].AddGeometry(agCompact);
end;

procedure Tdbtablef.Compareclassifications1Click(Sender: TObject);
begin
   Symmetric1Click(Sender);
end;

procedure Tdbtablef.Comparerankingswithdifferentcriteria1Click(Sender: TObject);
begin
   CompareRankings(DBonTable);
end;

procedure Tdbtablef.CompareshapefileandDEMprofiles1Click(Sender: TObject);
begin
   Do3Dshapefileprofile1Click(Sender);
end;


procedure Tdbtablef.Computeintervisibilityfrompoint1Click(Sender: TObject);
var
   Lat,Long,CameraElevation,TowerHeight : float64;
   z : float32;
begin
   GetLatLong(GISdb[DBonTable].TheMapOwner.MapDraw.PrimMapProj,'Point for intervisibility computation',Lat,Long);
   if DEMGlb[GISdb[DBonTable].TheMapOwner.MapDraw.DEMonMap].GetElevFromLatLongDegree(Lat,Long,z) then begin
      TowerHeight := MDDef.ObsAboveGround;
      ReadDefault('Observer height above ground (m)',TowerHeight);
      CameraElevation := z + TowerHeight;
      GISdb[DBonTable].IntervisibilityFromPoint(true,Lat,Long,CameraElevation,TowerHeight);
   end;
end;


procedure Tdbtablef.Concatenatestringfields1Click(Sender: TObject);
var
   db_concatenate : db_field_concatenate.Tdb_concatenate;
   FieldsInDB : tStringList;
begin
   db_concatenate := Tdb_concatenate.Create(Application);
   GetFieldsLinkPossible(GISdb[DBonTable].MyData,GISdb[DBonTable].LinkTable,GISdb[DBonTable].dbOpts.VisCols,[ftString,ftInteger,ftSmallInt],FieldsInDB);
   db_concatenate.ComboBox1.Items := FieldsInDB;
   db_concatenate.ComboBox2.Items := FieldsInDB;
   db_concatenate.ComboBox3.Items := FieldsInDB;
   FieldsInDB.Free;
   db_concatenate.ShowModal;
   if not db_concatenate.Abort then with db_concatenate do begin
      GISdb[DBonTable].AddConcatenatedField(Edit4.Text,ComboBox1.Text,ComboBox2.Text,ComboBox3.Text,Edit1.Text,Edit2.Text,Edit5.Text,Edit3.Text);
   end;
   db_concatenate.Free;
   ShowStatus;
end;


procedure Tdbtablef.Connectsequentialpoints1Click(Sender: TObject);
begin
   GISdb[DBonTable].GISProportionalSymbols(dbasConnectSeqPts);
end;


procedure Tdbtablef.ConvertDDMMSSHstringtoLatLong1Click(Sender: TObject);
var
   aLine : shortstring;
   Lat,Long : float64;
begin
   GISdb[DBonTable].AddLatLong;
   ShowHourglassCursor;
   GISdb[DBonTable].EmpSource.Enabled := false;
   GISdb[DBonTable].MyData.First;
   while not GISdb[DBonTable].MyData.eof do begin
      aline := GISdb[DBonTable].MyData.GetFieldByNameAsString('LOCATION');
      Lat := strToFloat(copy(aline,1,2)) + strToFloat(copy(aline,3,4)) / 60;
      if aline[7] in ['s','S'] then Lat := - Lat;
      Long := strToFloat(copy(aline,9,3)) + strToFloat(copy(aline,12,4)) / 60;
      if aline[16] in ['w','W'] then Long := - Long;
      GISdb[DBonTable].MyData.Edit;
      GISdb[DBonTable].MyData.SetFieldByNameAsFloat('LAT',lat);
      GISdb[DBonTable].MyData.SetFieldByNameAsFloat('LONG',long);
      GISdb[DBonTable].MyData.Next;
   end;
   ShowStatus;
end;

procedure Tdbtablef.ConvertUTMtolatlong1Click(Sender: TObject);
begin
    AddUTMcoordfields1Click(Sender);
end;



procedure Tdbtablef.Colorbarchart1Click(Sender: TObject);
begin
   GISdb[DBonTable].BarGraphLegend;
end;


procedure Tdbtablef.Colorbasedonfield1Click(Sender: TObject);
var
  FieldsInDB : tStringList;
  WantedFieldName : shortstring;
  i : integer;
begin
   {$IfDef RecordEditDB} WriteLineRoDebugFile('Tdbtablef.Colorbasedonfield1Click in'); {$EndIf}
   //with GISdb[DBonTable] do begin
     if (Sender = Colorbasedonfield1) then WantedFieldName := GISdb[DBonTable].PickField('unique values',[ftString,ftInteger,ftSmallInt])
     else WantedFieldName := GISdb[DBonTable].dbOpts.LinkFieldThisDB;
     if (WantedFieldName <> '') then begin
        GISdb[DBonTable].DBFieldUniqueEntries(WantedFieldName,FieldsInDB);
        GISdb[DBonTable].EmpSource.Enabled := true;
        GISdb[DBonTable].ClearGISFilter;
        for i := 0 to pred(FieldsInDB.Count) do begin
           GISdb[DBonTable].MyData.ApplyFilter(WantedFieldName + ' = ' + QuotedStr(FieldsInDB.Strings[i]));
           ShowStatus;
           if Sender = Colorbasedonjoinedtable1 then begin
              GISdb[DBonTable].LinkTable.ApplyFilter( GISdb[DBonTable].dbOpts.LinkFieldOtherDB + ' = ' + QuotedStr(FieldsInDB.Strings[i]));
               GISdb[DBonTable].FillFieldWithValue('COLOR',IntToStr( GISdb[DBonTable].LinkTable.TColorFromTable));
           end
           else begin
              Colorallrecords1Click(Colorallrecords1);
           end;
        end;
        FieldsInDB.Free;
        GISdb[DBonTable].ClearGISFilter;
        ShowStatus;
     end;
   //end;
   {$IfDef RecordEditDB} WriteLineRoDebugFile('Tdbtablef.Colorbasedonfield1Click out'); {$EndIf}
end;

procedure Tdbtablef.Colorbasedonjoinedtable1Click(Sender: TObject);
begin
   Colorbasedonfield1Click(Sender);
end;

procedure Tdbtablef.Button5Click(Sender: TObject);
begin
   {$IfDef RecordDataBase} WriteLineRoDebugFile('Tdbtablef.Button5Click'); {$EndIf}
    ChangeDEMNowDoing(IDDataBaseOne);
    DBEditting := DBonTable;
    if (GISdb[DBonTable].TheMapOwner <> Nil) then GISdb[DBonTable].theMapOwner.SetFocus;
end;


procedure Tdbtablef.Bycategory1Click(Sender: TObject);
begin
   {$IfDef NoDBGrafs}
   {$Else}
      GISdb[DBonTable].HistogramByCategory('','',false);
   {$EndIf}
end;


procedure Tdbtablef.Byclusters1Click(Sender: TObject);
begin
   TileCharateristicsWhiskerPlotsByCluster(DBonTable,false,MakeFiltersForCluster(DBonTable,false));
end;

procedure Tdbtablef.Bylatitude1Click(Sender: TObject);
begin
  {$IfDef NoDBGrafs}
  {$Else}
      GISdb[DBonTable].dbOpts.XField := GISdb[DBonTable].LatFieldName;
      GISdb[DBonTable].MakeGraph(dbgtBylatitude1);
  {$EndIf}
end;

procedure Tdbtablef.ByLatitude2Click(Sender: TObject);
begin
  {$IfDef NoDBGrafs}
  {$Else}
      GISdb[DBonTable].dbOpts.XField := GISdb[DBonTable].LatFieldName;
      GISdb[DBonTable].MakeGraph(dbgtByLatitude2);
   {$EndIf}
end;

procedure Tdbtablef.Bylongitude1Click(Sender: TObject);
begin
   {$IfDef NoDBGrafs}
   {$Else}
       GISdb[DBonTable].dbOpts.XField := GISdb[DBonTable].LongFieldName;
       GISdb[DBonTable].MakeGraph(dbgtBylongitude1);
   {$EndIf}
end;

procedure Tdbtablef.byLongitude2Click(Sender: TObject);
begin
   {$IfDef NoDBGrafs}
   {$Else}
      GISdb[DBonTable].dbOpts.XField := GISdb[DBonTable].LongFieldName;
      GISdb[DBonTable].MakeGraph(dbgtbyLongitude2);
   {$EndIf}
end;

procedure Tdbtablef.Ascending1Click(Sender: TObject);
begin
   SortDataBase(DBOnTable,true);
end;

procedure Tdbtablef.Ascending2Click(Sender: TObject);
begin
   SortDataBase(DBOnTable,true,SelectedColumn);
end;

procedure Tdbtablef.AssignDEMIXDEMcolors1Click(Sender: TObject);
var
   i,j : integer;
begin
    for i := 1 to MaxDataBase do begin
       if ValidDB(i) then begin
          for j := 1 to MaxDEMIXDEM do begin
             if StrUtils.AnsiContainsText(UpperCase(GISdb[i].DBName),UpperCase(DEMIXshort[j])) then begin
                GISdb[i].dbOpts.LineColor := DEMIXDEMcolors[j];
                GISdb[i].RedrawLayerOnMap;
             end;
          end;
       end;
    end;
end;

procedure Tdbtablef.Assignuniquecolors1Click(Sender: TObject);
begin
   Countuniquevalues1Click(Sender);
end;

procedure Tdbtablef.Associateimage1Click(Sender: TObject);
var
   fName : PathStr;
   i     : byte;
begin
   Highlightrecordonmap1Click(Sender);
   fName :=  ExtractFilePath(GISdb[DBonTable].dbFullName);
   if GetGraphicsFileName('Image to associate',fName) then begin
      GISdb[DBonTable].MyData.Edit;
      i := Length(ExtractFilePath(GISdb[DBonTable].dbFullName));
      System.Delete(fname,1,i);
      GISdb[DBonTable].MyData.SetFieldByNameAsString('IMAGE',fName);
      GISdb[DBonTable].MyData.Post;
   end;
end;


procedure Tdbtablef.Timer1Timer(Sender: TObject);
var
   Bitmap : tMyBitmap;
begin
   GISdb[DBonTable].theMapOwner.Image1.Picture.Graphic := BaseMapBitmap;
   if not CopyImageToBitmap(GISdb[DBonTable].theMapOwner.Image1,Bitmap) then exit;
   case HighLightCycle of
      1 : Bitmap.Canvas.Pen.Color := clRed;
      2 : Bitmap.Canvas.Pen.Color := clGreen;
      3 : Bitmap.Canvas.Pen.Color := clBlue;
   end;
   Bitmap.Canvas.Pen.Width := 4;
   Bitmap.Canvas.Brush.Color := Bitmap.Canvas.Pen.Color;
   inc(HighLightCycle);
   if (HighLightCycle = 3) then HighLightCycle := 1;
   GISdb[DBonTable].DisplayCurrentRecordOnMap(GISdb[DBonTable].TheMapOwner.MapDraw,Bitmap);
   GISdb[DBonTable].theMapOwner.Image1.Picture.Graphic := Bitmap;
   Bitmap.Destroy;
end;

procedure Tdbtablef.TrackBar1Change(Sender: TObject);
begin
   if TrackBarringAllowed then begin
      TrackBarringAllowed := false;
      MDDef.SecondGridOpacity := TrackBar1.Position;
      MDDef.FanOpacity := TrackBar1.Position;
      ShowSatProgress := false;
      GISdb[DBonTable].TheMapOwner.DoFastMapRedraw;
      ShowSatProgress := true;
   end;
   TrackBarringAllowed := true;
end;

procedure Tdbtablef.Highlightcolor1Click(Sender: TObject);
begin
   if LineOrAreaShapefile(GISdb[DBonTable].ShapeFileType) then begin
      Petmar.PickLineSizeAndColor('Highlight records',Nil,MDDef.HighLightColor,MDDef.HighlightLineWidth);
   end
   else begin
      Petmar.PickSymbol(Nil,MDDef.HighlightSymbol,'Highlight records');
   end;
end;

procedure Tdbtablef.Linelength2Click(Sender: TObject);
var
  ll : float64;
begin
   ll := GISdb[DBonTable].aShapeFile.LineLength(GISdb[DBonTable].MyData.RecNo);
   MessageToContinue('Length: ' + SmartDistanceMetersFormat(ll),True );
end;


procedure Tdbtablef.Fanproperties1Click(Sender: TObject);
{$IfDef ExViewshed}
begin
{$Else}
var
   wfan : tWeaponsFan;
begin
   {$IfDef RecordDataBase} WriteLineRoDebugFile('Fanproperties1Click'); {$EndIf}
      wFan := WeaponsTableToFan(GISdb[DBonTable].TheMapOwner.MapDraw.PrimMapProj,GISdb[DBonTable].MyData);
      DeleteFileIfExists(wFan.FanFileName);
      wFan.FanFileName := '';
      if GetWeaponParameters(GISdb[DBonTable].TheMapOwner.MapDraw.DEMonMap,wFan) then begin
         GISdb[DBonTable].MyData.Edit;
         GISdb[DBonTable].TheMapOwner.MapDraw.DeleteSingleMapLayer(GISdb[DBonTable].TheMapOwner.MapDraw.AllFansCoverageFName);
         AddFanToWeaponsTable(GISdb[DBonTable].TheMapOwner.MapDraw.PrimMapProj,True,true,GISdb[DBonTable].MyData,Wfan);
         GISdb[DBonTable].ClearImage;
         GISdb[DBonTable].theMapOwner.DoFastMapRedraw;
      end;
   {$EndIf}
end;


procedure Tdbtablef.FeaturesClick(Sender: TObject);
begin
   MDDef.LabelGazOnMap := Features.Checked;
end;

procedure Tdbtablef.Featurestatistics1Click(Sender: TObject);
var
   s1s2,s2s3,Trend,RoughnessFactor : float64;
   i,Npts : integer;
begin
   {$IfDef ExGeology}
   {$Else}
      GISdb[DBonTable].AddFieldToDataBase(ftFloat,'S1S2',8,3);
      GISdb[DBonTable].AddFieldToDataBase(ftFloat,'S2S3',8,3);
      GISdb[DBonTable].AddFieldToDataBase(ftFloat,'TREND',6,1);
      GISdb[DBonTable].EmpSource.Enabled := false;
      GISdb[DBonTable].MyData.First;
      i := 0;
      NPts := GISdb[DBonTable].MyData.RecordCount;
      StartSingleThreadTimer('Features');
      while not GISdb[DBonTable].MyData.eof do begin
         inc(i);
         if (i mod 100 = 0) then begin
            ThreadTimers.UpdateThreadStats(9, round(100 * i / Npts));
            GISdb[DBonTable].EmpSource.Enabled := false;
         end;

         if GISdb[DBonTable].SSOandaspectdiagrams(s1s2,s2s3,Trend,RoughnessFactor) then begin
            GISdb[DBonTable].MyData.Edit;
            GISdb[DBonTable].MyData.SetFieldByNameAsFloat('S1S2',s1s2);
            GISdb[DBonTable].MyData.SetFieldByNameAsFloat('S2S3',s2s3);
            GISdb[DBonTable].MyData.SetFieldByNameAsFloat('TREND',Trend);
         end;
         GISdb[DBonTable].MyData.Next;
      end;
      EndThreadTimers;
      ShowStatus;
   {$EndIf}
end;

procedure Tdbtablef.FFT1Click(Sender: TObject);
{$IfDef ExFourier}
begin
{$Else}
var
   WantedFieldName : ShortString;
   Dfile : file of float64;
   v : float64;
   FFTGraph : TFFTGraph;
begin
   WantedFieldName := GISdb[DBonTable].PickField('FFTs',[ftFloat,ftInteger,ftSmallInt]);
   FFTGraph := TFFTGraph.Create(Application);
   FFTGraph.BinTime := 1;
   FFTGraph.BinUnits := '';
   FFTGraph.fftfilename := NextFileNumber(MDTempDir, WantedFieldName,'');
   FFTGraph.TotalNumberPoints := 0;
   AssignFile(dFile,FFTGraph.fftfilename);
   Rewrite(dFile);
   GISdb[DBonTable].EmpSource.Enabled := false;
   GISdb[DBonTable].MyData.First;
   repeat
      v := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(WantedFieldName);
      BlockWrite(dFile,v,1);
      inc(FFTGraph.TotalNumberPoints);
      GISdb[DBonTable].MyData.Next;
   until GISdb[DBonTable].MyData.EOF;
   ShowStatus;
   CloseFile(DFile);
   FFTGraph.FastFourierTransform;
{$EndIf}
end;

procedure Tdbtablef.Fitfouriercurve1Click(Sender: TObject);
begin
   Autocorrelation1Click(Sender);
end;


procedure Tdbtablef.Fix360longs1Click(Sender: TObject);
var
   i,rc : integer;
   Long : float64;
begin
   with GISdb[DBonTable] do begin
       EmpSource.Enabled := false;
       GISdb[DBonTable].MyData.First;
       i := 0;
       rc := GISdb[DBonTable].MyData.RecordCount;
       StartProgress('Fix longs');
       while not GISdb[DBonTable].MyData.EOF do begin
          inc(i);
          if (i mod 500 = 0) then begin
             UpdateProgressBar(i/rc);
             EmpSource.Enabled := false;
          end;
          Long := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(LongFieldName);
          if (Long > 180) or (Long < -180) then begin
             GISdb[DBonTable].MyData.Edit;
             LongitudeAngleInRange(Long);
             GISdb[DBonTable].MyData.SetFieldByNameAsFloat(LongFieldName,Long);
          end;
          GISdb[DBonTable].MyData.Next;
       end;
       ShowStatus;
    end;
end;

procedure Tdbtablef.Fixcompassangles1Click(Sender: TObject);
var
   i,rc : integer;
   Long : float64;
   WantedFieldName : ShortString;
begin
   with GISdb[DBonTable] do begin
       WantedFieldName := PickField('Compass angles',[ftFloat,ftInteger,ftSmallInt]);
       EmpSource.Enabled := false;
       GISdb[DBonTable].MyData.First;
       i := 0;
       rc := GISdb[DBonTable].MyData.RecordCount;
       StartProgress('Fix compass angles');
       while not EOF do begin
          inc(i);
          if (i mod 500 = 0) then begin
             UpdateProgressBar(i/rc);
             EmpSource.Enabled := false;
          end;
          Long := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(WantedFieldName);
          if (Long > 360) or (Long <0) then begin
             GISdb[DBonTable].MyData.Edit;
             GISdb[DBonTable].MyData.SetFieldByNameAsFloat(WantedFieldName, PetMath.FindCompassAngleInRange(Long));
          end;
         GISdb[DBonTable].MyData. Next;
       end;
       ShowStatus;
    end;
end;

procedure Tdbtablef.FixExceldates1Click(Sender: TObject);
var
   FirstPart,TStr : ansistring;
   WantedFieldName : ShortString;
   I : integer;
begin
   //with GISdb[DBonTable] do begin
      GISdb[DBonTable].MyData.First;
      WantedFieldName := GISdb[DBonTable].PickField(TStr,[ftString]);
      while not GISdb[DBonTable].MyData.eof do begin
         TStr := GISdb[DBonTable].MyData.GetFieldByNameAsString(WantedFieldName);
         if StrUtils.AnsiContainsText(TStr,'-') then begin
            GISdb[DBonTable].EmpSource.Enabled := false;
            FirstPart := Petmar_types.BeforeSpecifiedCharacterANSI(TStr,'-',true,true);
            TStr := UpperCase(TStr);
            for I := 1 to 12 do begin
               if TStr = UpperCase(MonthName[i]) then begin
                  GISdb[DBonTable].MyData.Edit;
                  GISdb[DBonTable].MyData.SetFieldByNameAsString(WantedFieldName,IntToStr(i) + '-' + FirstPart);
               end;
            end;
         end;
         GISdb[DBonTable].MyData.Next;
      end;
//   end;
   ShowStatus;
end;


procedure Tdbtablef.Flipbinnames1Click(Sender: TObject);
var
   all,p1,p2 : ANSIstring;
begin
   GISdb[DBonTable].MyData.First;
   GISdb[DBonTable].EmpSource.Enabled := false;
   ShowHourglassCursor;
   while not GISdb[DBonTable].MyData.eof do begin
      All := GISdb[DBonTable].MyData.GetFieldByNameAsString('BIN_NAME');
      p1 := BeforeSpecifiedCharacterANSI(All,'/',true,true);
      p2 := BeforeSpecifiedCharacterANSI(All,'/',true,true);
      GISdb[DBonTable].MyData.Edit;
      GISdb[DBonTable].MyData.SetFieldByNameAsString('BIN_NAME',p2 + '/' + all + '/' + p1);
      GISdb[DBonTable].MyData.Next;
   end;
   ShowStatus;
end;

procedure Tdbtablef.Autocorrelation1Click(Sender: TObject);
{$IfDef ExFourier}
begin
{$Else}
var
   DataFile,DataFile2 : tStringList;
   WantedFieldName,WantedFieldName2 : ShortString;
   fName,fName2 : PathStr;
   TStr : ShortString;
   DataSpacing : float64;
   DataSpacingUnits : shortstring;
begin
   //with GISdb[DBonTable] do begin
      if (Sender = Fitfouriercurve1) then TStr := 'Fourier curve fitting'
      else if (Sender = Crosscorrelation1) then begin
         WantedFieldName2 := GISdb[DBonTable].PickField('First series cross correlation',NumericFieldTypes);
         fName2 := NextFileNumber(MDTempDir, WantedFieldName2,'');
         DataFile2 := tStringList.Create;
         TStr := 'Second series cross correlation';
      end
      else TStr := 'Autocorrelation';
      WantedFieldName := GISdb[DBonTable].PickField(TStr,[ftFloat,ftInteger,ftSmallInt]);
      fName := NextFileNumber(MDTempDir, WantedFieldName,'');
      DataFile := tStringList.Create;
      GISdb[DBonTable].EmpSource.Enabled := false;
      GISdb[DBonTable].MyData.First;
      repeat
         DataFile.Add(GISdb[DBonTable].MyData.GetFieldByNameAsString(WantedFieldName));
         if (Sender = Crosscorrelation1) then DataFile2.Add(GISdb[DBonTable].MyData.GetFieldByNameAsString(WantedFieldName2));
         GISdb[DBonTable].MyData.Next;
      until GISdb[DBonTable].MyData.EOF;
      DataFile.SaveToFile(fName);
      DataFile.Free;
      if (Sender = Fitfouriercurve1) then FitFourier.FitFourierCurve(fName)
      else begin
         DataSpacing := 1;
         DataSpacingUnits := '';
         if (Sender = Crosscorrelation1) then begin
            DataFile2.SaveToFile(fName2);
            DataFile2.Free;
            CrossCor.CrossCorrelating(false,fName,fName2,true,true,DataSpacing,DataSpacingUnits,false,WantedFieldName,WantedFieldName2);
         end
         else CrossCor.CrossCorrelating(true,fName,'',true,true,DataSpacing,DataSpacingUnits);
      end;
      ShowStatus;
   //end;
{$EndIf}
end;


procedure Tdbtablef.IgnoreMissingData1Click(Sender: TObject);
var
   fv,Words,LinkValue,AvField : shortstring;
   NewField : ShortString;
   NumNeigh,i,n,k,rc : integer;
   x : float32;
   NewGIS : integer;
   Sum : float64;
begin
   if ValidDB(DBonTable) and (GISdb[DBonTable].theMapOwner <> Nil) then begin
      {$IfDef AverageNeighbors} WriteLineRoDebugFile('Tdbtablef.Averageofneighbors1Click in'); {$EndIf}
      with GISdb[DBonTable] do begin
         if (Sender = Sumofneighbors1) then Words := 'sum' else words := 'average';
         AvField := PickField(Words + ' neighbor value' ,NumericFieldTypes);

         if (AvField = '') then exit;

         {$IfDef AverageNeighbors} WriteLineRoDebugFile('Average field: ' + AvField); {$EndIf}

         NewField := AvField;
         LinkedField(NewField);
         if (Sender = Sumofneighbors1) then NewField := 'NS_' + NewField

         else NewField := 'NA_' + NewField;
         NewField := GetFieldNameForDB('New Field',True,NewField);

         {$IfDef AverageNeighbors} WriteLineRoDebugFile('New field: ' + NewField); {$EndIf}

         AddFieldToDataBase(ftFloat,NewField,18,6);

         NewGIS := CopyDatabaseAndOpen(GISdb[DBonTable]);

         GISdb[NewGIS].dbOpts.LinkFieldThisDB := GISdb[DBonTable].dbOpts.LinkFieldThisDB;
         GISdb[NewGIS].dbOpts.LinkFieldOtherDB := GISdb[DBonTable].dbOpts.LinkFieldOtherDB;

         Empsource.Enabled := false;
         GISdb[DBonTable].MyData.First;
         StartProgress('Average');
         k := 0;
         rc := GISdb[DBonTable].MyData.RecordCount;
         while not GISdb[DBonTable].MyData.eof do begin
            inc(k);
            UpdateProgressBar(k/rc);
            LinkValue := GISdb[DBonTable].MyData.GetFieldByNameAsString(NeighborLinkField);
            {$IfDef AverageNeighborsFull} WriteLineRoDebugFile('LinkValue: ' + LinkValue); {$EndIf}

            NeighborTable.ApplyFilter(NeighborLinkField + '=' + QuotedStr(LinkValue));

            fv := NeighborTable.GetFieldByNameAsString('NUM_NEIGH');
            if (Sender = MissingData0) and (fv = '') then fv := '0';

            if (fv <> '') then begin
               NumNeigh := StrToInt(fv);
               Sum := 0;
               n := 0;
               for i := 1 to NumNeigh do begin
                  LinkValue := NeighborTable.GetFieldByNameAsString('NEIGH_' + IntToStr(i));
                  {$IfDef AverageNeighborsFull} WriteLineRoDebugFile('Neighbor: ' + LinkValue); {$EndIf}
                  GISdb[NewGIS].MyData.ApplyFilter(NeighborLinkField + '=' + QuotedStr(LinkValue));

                  if GISdb[NewGIS].GetFloat32FromTableLinkPossible(AvField,x) then begin
                     inc(n);
                     Sum := Sum + x;
                  end;
               end;
               if (N > 0) then begin
                  GISdb[DBonTable].MyData.Edit;
                  if (Sender = Sumofneighbors1) then GISdb[DBonTable].MyData.SetFieldByNameAsFloat(NewField,sum)
                  else GISdb[DBonTable].MyData.SetFieldByNameAsFloat(NewField,sum / n);
               end;
            end;
            GISdb[DBonTable].MyData.Next;
         end;
         CloseAndNilNumberedDB(NewGIS);
         ShowStatus;
      end;
      {$IfDef AverageNeighbors} WriteLineRoDebugFile('Tdbtablef.Averageofneighbors1Click out'); {$EndIf}
   end;
end;



procedure Tdbtablef.ilecharacteristicsbytileforCopDEM1Click(Sender: TObject);
begin
   DEMIX_COP_clusters_tile_stats(DBonTable);
end;

procedure Tdbtablef.Averagebylatitude1Click(Sender: TObject);
var
   aField : shortstring;
   BinSize,Lat,Value : float32;
   GraphData : tStringList;
   i,Bin : integer;
   Sum : array[0..180] of float64;
   Count : array[0..180] of int64;
begin
   //with GISdb[DBonTable] do begin
      aField := GISdb[DBonTable].PickField('Field for rose diagram',NumericFieldTypes);
      BinSize := 5;
      ReadDefault('Lat bin size (degrees)',BinSize);
      if BinSize < 1 then BinSize := 1;
      for i := 0 to 180 do begin
          Sum[i] := 0;
          Count[i] := 0;
      end;

      GraphData := tStringList.Create;
      GraphData.Add(aField + ',LAT');
      GISdb[DBonTable].EmpSource.Enabled := false;
      StartProgress('Average');

      i := 0;
      GISdb[DBonTable].MyData.First;
      while not GISdb[DBonTable].MyData.eof do begin
         if (i mod 100 = 0) then UpdateProgressBar(i / GISdb[DBonTable].MyData.FiltRecsInDB);
         Lat := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(GISdb[DBonTable].LatFieldName);
         Value := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(aField);
         Bin := round((180 - (Lat + 90)) / BinSize);
         Sum[Bin] := Sum[Bin] + value;
         inc(Count[Bin]);
         inc(i);
         GISdb[DBonTable].MyData.Next;
      end;
      for i := 0 to 180 do begin
          if Count[i]  > 0 then begin
            GraphData.Add(RealToString(Sum[i]/Count[i],-9,-2) + ',' + RealToString((90 - (i+0.5) * BinSize),-8,-2) );
          end;
      end;
      GraphFromCSVfile(GraphData,false,true);
      GISdb[DBonTable].ClearGISFilter;
      EndProgress;
      ShowStatus;
   //end;
end;

procedure Tdbtablef.Averageranksbyarea1Click(Sender: TObject);
begin
{$IfDef ExDEMIXexperimentalOptions}
{$Else}
   DEMIXwineContestCriterionGraph(dgArea,DBonTable);
{$EndIf}
end;

procedure Tdbtablef.AverageStandarddeviation1Click(Sender: TObject);
{$IfDef ExAdvancedSats}
begin
{$Else}
var
   Band,aClass,Decs : integer;
   Lat,Long,MinV,MaxV : float64;
   Results : tStringList;
   MomentVar : tMomentVar;
   Values : array[1..5000] of float32;
   fName,BasePath : PathStr;
   WantField : ShortString;
begin
    with GISdb[DBonTable] do begin
      WantField := 'CLASS';
      FieldRange(WantField,MinV,MaxV);
      BasePath := ExtractFilePath(dbFullName) + '\class_stats\';
      SafeMakeDir(BasePath);
      if DEMGlb[MultiGridArray[TheMapOwner.MapDraw.MultiGridOnMap].Grids[1]].DEMheader.DEMPrecision = FloatingPointDEM then Decs := -5
      else Decs := -2;
      StartProgress('Ranges');
      for aClass := Round(MinV) to round(MaxV) do begin
         UpdateProgressBar(aClass/MaxV);
         GISdb[DBonTable].MyData.ApplyFilter(WantField + '=' + IntToStr(aClass));
         fName := BasePath + GISdb[DBonTable].MyData.GetFieldByNameAsString('NAME') +  DefaultDBExt;
         if FileExists(fName) then begin
            SysUtils.DeleteFile(fName);
         end;

          fName := ChangeFileExt(fName,'.csv');
          Results := tStringList.Create;
          Results.Add('BAND,WAVELENGTH,MEAN,STD_DEV,MIN,MAX,MEDIAN,PERC_5,PERC_10,QUANT_25,QUANT_75,PERC_90,PERC_95,COLOR');
          for Band := 1 to MultiGridArray[TheMapOwner.MapDraw.MultiGridOnMap].NumGrids do begin
             GISdb[DBonTable].MyData.First;
             EmpSource.Enabled := false;
             MomentVar.NPts := 0;
             while not GISdb[DBonTable].MyData.eof do begin
                if ValidLatLongFromTable(Lat,Long) then begin
                   inc(MomentVar.NPts);
                   if not DEMGlb[MultiGridArray[TheMapOwner.MapDraw.MultiGridOnMap].Grids[Band]].GetElevFromLatLongDegree(Lat,Long,Values[MomentVar.NPts]) then dec(MomentVar.NPts);
                end;
                GISdb[DBonTable].MyData.Next;
             end;
             HeapSort(MomentVar.NPts,Values);
             Moment(Values,MomentVar,msAll);
             Results.Add(IntToStr(Band) + ',' + RealToString(MultiGridArray[TheMapOwner.MapDraw.MultiGridOnMap].BandWavelengths[Band],-8,-2) + ',' +
                  RealToString(MomentVar.mean,-12,Decs) + ',' + RealToString(MomentVar.std_dev,-12,Decs) + ',' +
                  RealToString(values[1],-12,Decs) + ',' + RealToString(values[MomentVar.NPts],-12,Decs) + ',' +
                  RealToString(MomentVar.Median,-12,Decs) + ',' +
                  RealToString(Quantile(5,Values,MomentVar.NPts,true),-12,Decs) + ',' +
                  RealToString(Quantile(10,Values,MomentVar.NPts,true),-12,Decs) + ',' +
                  RealToString(Quantile(25,Values,MomentVar.NPts,true),-12,Decs) + ',' +
                  RealToString(Quantile(75,Values,MomentVar.NPts,true),-12,Decs) + ',' +
                  RealToString(Quantile(90,Values,MomentVar.NPts,true),-12,Decs) + ',' +
                  RealToString(Quantile(95,Values,MomentVar.NPts,true),-12,Decs) + ',' +
                  IntToStr(MyData.GetFieldByNameAsInteger('COLOR')));
          end;
          theMapOwner.StringListToLoadedDatabase(Results,fName);
       end;
       ClearGISFilter;
       Self.BringToFront;
       ShowStatus;
    end;
{$EndIf}
end;


procedure Tdbtablef.Averagetilecharacteristicsbycluster1Click(Sender: TObject);
begin
   MakeDBForParamStats(opByCluster,DBonTable);
end;

procedure Tdbtablef.AWATERtoAWATERKM21Click(Sender: TObject);
begin
   ALANDtoALANDKM21Click(Sender);
end;

procedure Tdbtablef.AWATERtoWATERKM21Click(Sender: TObject);
begin
   ALANDtoALANDKM21Click(Sender);
end;

procedure Tdbtablef.Crosscorrelation1Click(Sender: TObject);
begin
   Autocorrelation1Click(Sender);
end;


procedure Tdbtablef.Requiredantennaheight1Click(Sender: TObject);
{$IfDef ExAdvancedGIS}
begin
{$Else}
begin
   //with GISdb[DBonTable] do begin
      MDDef.ObsAboveGround := GISdb[DBonTable].MyData.GetFieldByNameAsFloat('SENSOR_UP');
      MDDef.MaskObsRange := GISdb[DBonTable].MyData.GetFieldByNameAsFloat('SENSOR_RNG');
      GISdb[DBonTable].TheMapOwner.RequiredAntennaMap(GISdb[DBonTable].MyData.GetFieldByNameAsFloat('LAT'),GISdb[DBonTable].MyData.GetFieldByNameAsFloat('LONG'));
   //end;
{$EndIf}   
end;

procedure Tdbtablef.Resetjoin1Click(Sender: TObject);
begin
   Clearjoin1Click(Sender);
   Setjoin1Click(Sender);
end;


procedure Tdbtablef.RestorebackupversionofDB1Click(Sender: TObject);
var
   tName : PathStr;
begin
   tName := GISdb[DBonTable].MyData.DBBakDir + GISdb[DBonTable].dbName + '*';
   if GetFileFromDirectory('backup data base',DefaultDBMask,tName) then begin
      GISdb[DBonTable].MyData.Destroy;
      SysUtils.DeleteFile(GISdb[DBonTable].DBFullName);
      repeat
      until (not FileExists(GISdb[DBonTable].DBFullName));
      CopyFile(tName,GISdb[DBonTable].DBFullName);
      repeat
      until FileExists(GISdb[DBonTable].DBFullName);
      GISdb[DBonTable].MyData := tMyData.Create(GISdb[DBonTable].dbFullName);
      GISdb[DBonTable].RespondToChangedDB;
   end;
end;

procedure Tdbtablef.Restrictbymapscale1Click(Sender: TObject);
begin
   Restrictbymapscale1.Checked := not Restrictbymapscale1.Checked;
   MDDef.UsePixelSizeRules := Restrictbymapscale1.Checked;
   GISdb[DBonTable].RedrawLayerOnMap;
end;

procedure Tdbtablef.Retainfirstncharacters1Click(Sender: TObject);
begin
   Removelowercasecharacters1Click(Sender);
end;

procedure Tdbtablef.Trimblanksallstringfields1Click(Sender: TObject);
begin
   GISdb[DBonTable].TrimStringFields;
end;


procedure Tdbtablef.MakeFormDockable;
begin
   DragKind := dkDock;
   DragMode := dmAutomatic;
   UseDockManager := true;
end;


procedure Tdbtablef.rimBlanksInStringField1Click(Sender: TObject);
begin
    SelectedColumn := GISdb[DBonTable].PickField('field to trim',[ftString]);
    GISdb[DBonTable].TrimStringFields(SelectedColumn);
end;

procedure Tdbtablef.Trimfield1Click(Sender: TObject);
begin
   GISdb[DBonTable].TrimOneStringField(SelectedColumn);
end;


procedure Tdbtablef.rimlengthallstringfields1Click(Sender: TObject);
begin
   GISdb[DBonTable].TrimAllStringFields;
end;


procedure Tdbtablef.Plotfans2Click(Sender: TObject);
begin
   Plotfans2Click(Sender);
end;

procedure Tdbtablef.Plotforsubsamples1Click(Sender: TObject);
begin
   {$IfDef NoDBGrafs}
   {$Else}
      GISdb[DBonTable].MakeGraph(dbgtPlotforsubsamples1);
   {$EndIf}
end;

procedure Tdbtablef.Plotfanlocations1Click(Sender: TObject);
begin
   GISdb[DBonTable].RedrawLayerOnMap;
end;


procedure Tdbtablef.Plotfanlocations2Click(Sender: TObject);
begin
   Plotfanlocations1Click(Sender);
end;

procedure Tdbtablef.DataDBFonlynogeometry1Click(Sender: TObject);
begin
   DBFfile1Click(Sender);
end;

procedure Tdbtablef.Datumshift1Click(Sender: TObject);
var
   RecNum,rc : integer;
   oLat,oLong,
   Lat,Long : float64;
   LatHemi : AnsiChar;
   PrimaryMapDatum,NewMapDatum : tMapProjection;
begin
   PrimaryMapDatum := tMapProjection.Create('table, primary');
   NewMapDatum := tMapProjection.Create('table, new map');
   LatHemi := MDdef.DefaultLatHemi;
   PrimaryMapDatum.h_DatumCode := MDdef.PreferPrimaryDatum;
   PickDatum('orginal datum',PrimaryMapDatum.h_DatumCode);
   PrimaryMapDatum.DefineDatumFromUTMZone(PrimaryMapDatum.h_DatumCode,PrimaryMapDatum.projUTMZone,Lathemi,'Tdbtablef.Datumshift1Click');
   NewMapDatum.h_DatumCode := MDdef.PreferPrimaryDatum;
   PickDatum('new shifted datum',NewMapDatum.h_DatumCode);
   NewMapDatum.DefineDatumFromUTMZone(NewMapDatum.h_DatumCode,NewMapDatum.projUTMZone,LatHemi,'Tdbtablef.Datumshift1Click');
   with GISdb[DBonTable],MyData do begin
      ClearGISFilter;
      RecNum := 0;
      rc := GISdb[DBonTable].MyData.RecordCount;
      StartProgress('Reproject');
      while not GISdb[DBonTable].MyData.EOF do begin
         if GISdb[DBonTable].GetLatLongToRepresentRecord(oLat,oLong) then begin
            GISdb[DBonTable].MyData.Edit;
            inc(RecNum);
            if (RecNum mod 250 = 0) then UpdateProgressBar(RecNum/rc);
            MolodenskiyTransformation(oLat,oLong,Lat,Long,PrimaryMapDatum,NewMapDatum);
            GISdb[DBonTable].MyData.SetFieldByNameAsFloat('LAT',Lat);
            GISdb[DBonTable].MyData.SetFieldByNameAsFloat('LONG',Long);
         end;
         GISdb[DBonTable].MyData.Next;
      end;
   end;
   PrimaryMapDatum.Destroy;
   NewMapDatum.Destroy;
   ShowStatus;
end;


procedure Tdbtablef.Datumshift2Click(Sender: TObject);
begin
   ComputeVDatumShift(dbOnTable);
end;

procedure Tdbtablef.DayofweekfromYRMonDay1Click(Sender: TObject);
var
   m,y,d,i,rc : integer;
   Lat,Long : float64;
   ConstantLocation : boolean;
begin
   with GISdb[DBonTable] do begin
      if (Sender = Dayssincefullmoon1) then begin
         ConstantLocation := false;
         if not LatLongFieldsPresent then begin
            GetLatLongDefault(WGS84DatumConstants,'location',Lat,Long);
            ConstantLocation := true;
         end;
         AddFieldToDataBase(ftInteger,'SINCE_FULL',2,0);
      end
      else AddFieldToDataBase(ftString,'DAY_WEEK',12,0);
      EmpSource.Enabled := false;
      GISdb[DBonTable].MyData.First;
      i := 0;
      rc := GISdb[DBonTable].MyData.RecordCount;
      StartProgress('Compute');
      while not GISdb[DBonTable].MyData.Eof do begin
         inc(i);
         if (i mod 1000 = 0) then UpDateProgressBar(i/rc);
         GISdb[DBonTable].MyData.Edit;
         m := GISdb[DBonTable].MyData.GetFieldByNameAsInteger(MonthFieldName);
         d := GISdb[DBonTable].MyData.GetFieldByNameAsInteger('DAY');
         y := GISdb[DBonTable].MyData.GetFieldByNameAsInteger('YEAR');
         if (Sender = Dayssincefullmoon1) then begin
            {$IfDef ExGeography}
            {$Else}
               if ConstantLocation or ValidLatLongFromTable(Lat,Long) then GISdb[DBonTable].MyData.SetFieldByNameAsInteger('SINCE_FULL',DaysSinceFullMoon(m,d,y,Lat,Long));
            {$EndIf}
         end
         else GISdb[DBonTable].MyData.SetFieldByNameAsString('DAY_WEEK',FormatSettings.LongDayNames[DayOfWeek(EncodeDate(y,m,d))]);
         GISdb[DBonTable].MyData.Next;
      end;
      ShowStatus;
   end;
end;


procedure Tdbtablef.Dayssincefullmoon1Click(Sender: TObject);
begin
   DayofweekfromYRMonDay1Click(Sender);
end;


procedure Tdbtablef.DBFfile1Click(Sender: TObject);
var
   j : integer;
   AllVis : boolean;
   fName : PathStr;
begin
   {$IfDef RecordDataBaseSaveFiles} WriteLineRoDebugFile('Tdbtablef.DBFfile1Click in'); {$EndIf}
   //with GISdb[DBonTable] do begin
      if (Sender <> DataDBFonlynogeometry1) then begin
         if GISdb[DBonTable].ItsAShapeFile and LineOrAreaShapeFile(GISdb[DBonTable].ShapeFileType) then exit;
      end;
      AllVis := true;
      for j := 0 to MaxFieldsInDB do if (not GISdb[DBonTable].dbOpts.VisCols[j]) then AllVis := false;
      if (not GISdb[DBonTable].MyData.Filtered) and AllVis then begin
         if (GISdb[DBonTable].dbFullName = '') or GISdb[DBonTable].dbIsUnsaved then fName := Caption
         else fName := ExtractFilePath(GISdb[DBonTable].dbFullName) + 'copy-' + ExtractFileName(GISdb[DBonTable].dbFullName);
         Petmar.GetFileNameDefaultExt('Copy of DB',DefaultDBMask,fName);
         Petmar.CopyFile(GISdb[DBonTable].dbFullName,fName);
      end
      else begin
         if (not AllVis) and GISdb[DBonTable].MyData.Filtered then begin
            if (not AnswerIsYes('Save filtered DB without hidden fields')) then Button1Click(Sender);
         end
         else begin
            if (not AllVis) and (not AnswerIsYes('Save without hidden fields')) then Button1Click(Sender);
            if GISdb[DBonTable].MyData.Filtered and (not AnswerIsYes('Confirm save filtered database')) then Button1Click(Sender);
         end;
         GISdb[DBonTable].SaveCurrentDBaseSubset('');
      end;
      GISdb[DBonTable].dbIsUnsaved := false;
   //end;
      ShowStatus;
   {$IfDef RecordDataBaseSaveFiles} WriteLineRoDebugFile('Tdbtablef.DBFfile1Click out'); {$EndIf}
end;


procedure Tdbtablef.ValuesfromDB1Click(Sender: TObject);
begin
   CreateGrid(cgValuesGrid);
end;


procedure Tdbtablef.Vectoraverageinbox1Click(Sender: TObject);
{$IfDef AllowGeomorphometry}
var
   x,y,n : integer;
   Lat,Long,HiLat,HiLong,LoLat,LoLong,xsum,ysum,Mag,Dir : float64;
   BoxLimits : tGridLimits;
   Results : tStringList;
   fName : PathStr;
begin
   {$IfDef AverageNeighbors} WriteLineRoDebugFile('Tdbtablef.Vectoraverageinbox1Click in'); {$EndIf}
   with GISdb[DBonTable] do begin
     BoxLimits := DEMGlb[TheMapOwner.MapDraw.DEMonMap].FullDEMGridLimits;
     ReadDefault('Half box size (DEM grid postings)',MDDef.HalfBoxSize);
     ReadDefault('Points required to average',MDDef.NAvgReq);

     Results := tStringList.Create;
     Results.Add('LAT,LONG,' + dbOpts.MagField + ',' + dbOpts.DirField + ',N');

     ShowSatProgress := false;
     StartSingleThreadTimer('Averaging');

     x := BoxLimits.XGridLow + MDDef.HalfBoxSize;
     while x <= BoxLimits.XGridHigh - MDDef.LagSearchRadius do begin
        {$IfDef AverageNeighbors} WriteLineRoDebugFile('x=' + IntToStr(x)); {$EndIf}
        ThreadTimers.UpdateThreadStats(9, round(100 * (x - BoxLimits.XGridLow) / (BoxLimits.XGridHigh-BoxLimits.XGridLow)));
        y := BoxLimits.YGridLow + MDDef.HalfBoxSize;
        while y <= BoxLimits.YGridHigh - MDDef.HalfBoxSize do begin
           EmpSource.Enabled := false;
           DEMGlb[TheMapOwner.MapDraw.DEMonMap].DEMGridToLatLongDegree(x,y,Lat,Long);
           VincentyPointAtDistanceBearing(Lat,Long,MDDef.HalfBoxSize*1.414*DEMGlb[TheMapOwner.MapDraw.DEMonMap].AverageSpace,45,HiLat,HiLong);
           VincentyPointAtDistanceBearing(Lat,Long,MDDef.HalfBoxSize*1.414*DEMGlb[TheMapOwner.MapDraw.DEMonMap].AverageSpace,225,LoLat,LoLong);
           GISdb[DBonTable].MyData.ApplyFilter(PetDBUtils.MakePointGeoFilter(LatFieldName,LongFieldName,HiLat,LoLong,LoLat,HiLong));
           if (MyData.RecordCount > 0) then begin
              xsum := 0;
              ysum := 0;
              n := 0;
              while not GISdb[DBonTable].MyData.eof do begin
                 if GISdb[DBonTable].MyData.CarefullyGetFieldByNameAsFloat64(dbOpts.DirField,Dir) and GISdb[DBonTable].MyData.CarefullyGetFieldByNameAsFloat64(dbOpts.MagField, Mag) then begin
                    inc(n);
                    xsum := xsum + Mag * sinDeg(Dir);
                    ysum := ysum + Mag * cosDeg(Dir);
                 end;
                 GISdb[DBonTable].MyData.Next;
              end;
              if (n > MDDef.NAvgReq) then begin
                 Dir := HeadingOfLine(xsum,ysum);
                 Mag := sqrt(sqr(xsum) + sqr(ysum)) / n;
                 Results.Add(RealToString(Lat,-12,-6) + ',' + RealToString(Long,-12,-6) + ',' +  RealToString(Mag,-12,-3) + ',' + RealToString(Dir,-12,-3) + ',' + IntToStr(n));
              end;
           end;
           inc(y,2 * MDDef.HalfBoxSize);
        end;
        inc(x,2 * MDDef.HalfBoxSize);
     end;
     {$IfDef AverageNeighbors} WriteLineRoDebugFile('loop done'); {$EndIf}
     EndThreadTimers;
     ShowSatProgress := true;
     fName := MDTempDir + GISdb[DBonTable].DBName + '_vector_average.csv';
     {$IfDef AverageNeighbors} WriteLineRoDebugFile('fName=' + fName): {$EndIf}
     theMapOwner.StringListToLoadedDatabase(Results,fName);
     GISdb[DBonTable].MyData.ApplyFilter('');
     ShowStatus;
  end;
   {$IfDef AverageNeighbors} WriteLineRoDebugFile('Tdbtablef.Vectoraverageinbox1Click out'); {$EndIf}
{$Else}
begin
{$EndIf}
end;


procedure Tdbtablef.Vectordata1Click(Sender: TObject);
begin
   {$IfDef ExAdvancedGIS}
   {$Else}
      GISdb[DBonTable].GISProportionalSymbols(dbasVector);
   {$EndIf}
end;


procedure Tdbtablef.Vertical1Click(Sender: TObject);
begin
   DisplayBitmap(DEMIXTestDEMLegend(false),'Test DEMs');
end;

procedure Tdbtablef.Verticaldatumshift1Click(Sender: TObject);
var
   i,GeoidGrid : Integer;
   Lat,Long : float64;
   z : float32;
begin
   GeoidGrid := OpenNewDEM(Geoid2008FName,false);
   GISdb[DBonTable].AddFieldToDataBase(ftFloat,'VDATM_SHFT',6,2);
   GISdb[DBonTable].MyData.First;
   GISdb[DBonTable].EmpSource.Enabled := false;
   i := 0;
   while not GISdb[DBonTable].MyData.Eof do begin
      if GISdb[DBonTable].ValidLatLongFromTable(Lat,Long) and DEMGlb[GeoidGrid].GetElevFromLatLongDegree(lat,Long,z) then begin
         GISdb[DBonTable].MyData.Edit;
         GISdb[DBonTable].MyData.SetFieldByNameAsFloat('VDATM_SHFT',z);
      end;
      GISdb[DBonTable].MyData.Next;
   end;
   ShowStatus;
   CloseSingleDEM(GeoidGrid);
end;


procedure Tdbtablef.Verticalexagerration1Click(Sender: TObject);
begin
   ThreeDGraph(DBonTable,false);
end;

procedure Tdbtablef.Viewshedfields1Click(Sender : TObject);
var
   wf : tWeaponsFan;
   i : integer;
begin
   with GISdb[DBonTable] do begin
      AddOrSubtractOverlay(TheMapOwner,ovoFans,false);
      InitializeWeaponsFan(wf);
      if GetWeaponParameters(GISdb[DBonTable].TheMapOwner.MapDraw.DEMonMap,wf,false,false,Nil,true,true) then begin
         if (Sender = ViewshedFields1) then AddFieldGroupToTable('WeaponsFan');
         GISdb[DBonTable].MyData.First;
         GISdb[DBonTable].EmpSource.Enabled := false;
         i := 0;
         while not GISdb[DBonTable].MyData.Eof do begin
            inc(i);
            wf.Fan_Name := GISdb[DBonTable].MyData.GetFieldByNameAsString('NAME');
            if wf.Fan_Name = '' then wf.Fan_Name := 's_' + IntToStr(i);
            if GISdb[DBonTable].ValidLatLongFromTable(wf.W_Lat,wf.W_Long) then AddFanToWeaponsTable(TheMapOwner.MapDraw.PrimMapProj,true,true,MyData,wf);
            GISdb[DBonTable].MyData.Next;
         end;

         ItsFanFile := true;
         dbTablef.Panel1.Visible := true;
         ShowStatus;
         TheMapOwner.MapDraw.CurrentFansTable := DBonTable;
         AddOrSubtractOverlay(TheMapOwner,ovoFans,true);
         TheMapOwner.DoFastMapRedraw;
      end;
   end;
end;


procedure Tdbtablef.N2Dgraphcolorcoded1Click(Sender: TObject);
begin
   {$IfDef NoDBGrafs}
   {$Else}
      GISdb[DBonTable].MakeGraph(dbgtN2Dgraphcolorcoded1);
   {$EndIf}
end;


procedure Tdbtablef.RMSE1Click(Sender: TObject);
begin
   MessageToContinue(SelectedColumn + ' RMSE=' + RealToString(GISdb[DBonTable].FieldRMSE(SelectedColumn),-12,-3));
end;



procedure Tdbtablef.Rosediagram01801Click(Sender: TObject);
begin
   Rosediagram1Click(Sender);
end;


procedure Tdbtablef.Rosediagram1Click(Sender: TObject);
{$IfDef NoDBGrafs}
begin
{$Else}
var
   Month : integer;
begin
   with GISdb[DBonTable] do begin
     if (Sender = Rosediagram3) then dbOpts.XField := SelectedColumn
     else dbOpts.XField := PickField('Field for rose diagram',NumericFieldTypes);
     if (dbOpts.XField = '') then exit;

     MDDef.RoseBothEnds := (Sender = Rosediagram01801);
     if (Sender = Rosediagram1) or (Sender = Rosediagram3) or (Sender = Rosediagram01801) then SingleRose('',dbOpts.Xfield,'')
     else begin
         for Month := 1 to 12 do begin
            GISdb[DBonTable].dbOpts.TimeFilter := 'MONTH=' + IntToStr(Month);
            GISdb[DBonTable].AssembleGISFilter;
            SingleRose('',dbOpts.XField,'');
         end;
     end;
     ShowStatus;
   end;
{$EndIf}
end;


procedure Tdbtablef.Rosediagram2Click(Sender: TObject);
begin
   Rosediagram1Click(Sender);
end;


procedure Tdbtablef.Rosediagram3Click(Sender: TObject);
begin
   Rosediagram1Click(Sender);
end;


procedure Tdbtablef.Rosediagramdipdirections1Click(Sender: TObject);
begin
   {$IfDef ExGeology}
   {$Else}
      GISdb[DBonTable].FocalPlaneStats(fpRoseDipDir);
   {$EndIf}
end;

procedure Tdbtablef.Rosediagramstrikes1Click(Sender: TObject);
begin
   {$IfDef ExGeology}
   {$Else}
      GISdb[DBonTable].FocalPlaneStats(fpRoseStrike);
   {$EndIf}
end;


procedure Tdbtablef.CalculateArea1Click(Sender: TObject);
begin
   GISdb[DBonTable].CalculateAreaorCentroid(false);
end;


procedure Tdbtablef.Calculatecentroid1Click(Sender: TObject);
begin
   //CalculateArea1Click(Sender);
   GISdb[DBonTable].CalculateAreaorCentroid(true);
end;


procedure Tdbtablef.Calculateperimeter1Click(Sender: TObject);
begin
   MessageToContinue('Perimeter: ' + SmartDistanceMetersFormat(GISdb[DBonTable].aShapeFile.LineLength(GISdb[DBonTable].MyData.RecNo)),True);
end;

procedure Tdbtablef.Recolorallfans1Click(Sender: TObject);
begin
   Recolorfan1Click(Sender);
end;

procedure Tdbtablef.Recolorfan1Click(Sender: TObject);
var
   Color : tPlatformColor;
   i,rc : integer;

         procedure RecolorSingleFan;
         var
            Bitmap : tMyBitmap;
            fName : PathStr;
         begin
            with GISdb[DBonTable] do begin
               fName := GISdb[DBonTable].MyData.GetFieldByNameAsString('IMAGE');
               if FileExists(fName) then begin
                  Bitmap := Petimage.LoadBitmapFromFile(fName);
                  RecolorFan(Bitmap,Color);
                  Petimage.SaveBitmap(Bitmap,fName);
                  FreeAndNil(Bitmap);
               end;
               GISdb[DBonTable].MyData.Edit;
               GISdb[DBonTable].MyData.SetFieldByNameAsInteger('VIS_COLOR',ConvertPlatformColorToTColor(Color));
               GISdb[DBonTable].MyData.Post;
            end;
         end;

begin
   Color := claRed;
   Petmar.QueryColor(Color);
   if (Sender = RecolorFan1) then RecolorSingleFan
   else begin
      GISdb[DBonTable].EmpSource.Enabled := false;
      i := 0;
      rc := GISdb[DBonTable].MyData.RecordCount;
      StartProgress('Recolor');
      GISdb[DBonTable].MyData.First;
      while not GISdb[DBonTable].MyData.eof do begin
         inc(i);
         if (I mod 50 = 0) then UpdateProgressBar(i/rc);
         RecolorSingleFan;
         GISdb[DBonTable].MyData.Next;
      end;
      ShowStatus;
   end;
   GISdb[DBonTable].TheMapOwner.DoFastMapRedraw;
end;

procedure Tdbtablef.Recordboundingbox1Click(Sender: TObject);
begin
   RecordCoordinates1Click(Sender);
end;

procedure Tdbtablef.RecordCoordinates1Click(Sender: TObject);
var
   textCoords : tStringList;
   xpic,ypic,i : integer;
   Lat,Long : float64;
   Capt : shortString;
   BoundBox : sfBoundBox;
begin
   with GISdb[DBonTable] do begin
      textCoords := TstringList.Create;
      if LineOrAreaShapefile(ShapeFileType) then begin
         if (Sender = Recordboundingbox1) then begin
            BoundBox := aShapeFile.LineAreaBoundBox(MyData.RecNo);
            MessageToContinue('Lower left corner: ' + LatLongDegreeToString(BoundBox.YMin,BoundBox.XMin,MDDef.OutPutLatLongMethod) + MessLineBreak +
                            'Upper right corner: ' + LatLongDegreeToString(BoundBox.YMax,BoundBox.XMax,MDDef.OutPutLatLongMethod),True);
         end
         else begin
            aShapeFile.GetLineCoords(GISdb[DBonTable].MyData.RecNo,ShapeFile3D(GISdb[DBonTable].ShapeFileType));

            if AreaShapeFile(ShapeFileType) and (aShapeFile.CurrentPolyLineHeader.NumParts > 1) then begin
               aShapeFile.AreasCounterClockwise;
               with aShapeFile.CurrentPolyLineHeader do begin
                  textCoords.Add('Counter clockwise (outline)');
                  for I := 1 to NumParts  do begin
                     if (not aShapeFile.CurrentLinePartsCCW[i]) then textCoords.Add('Part ' + IntToStr(i) + ' pts=' + IntToStr(aShapeFile.CurrentLinePointsPerPart[i]) );
                  end;
                  textCoords.Add('Clockwise (holes)');
                  for I := 1 to NumParts do begin
                     if (aShapeFile.CurrentLinePartsCCW[i]) then textCoords.Add('Part ' + IntToStr(i)  + ' pts=' + IntToStr(aShapeFile.CurrentLinePointsPerPart[i]) );
                  end;
                  textCoords.Add('');
               end;
            end;

             if (Sender = Recordscreencoordinates1) then begin
                for i := 0 to pred(aShapeFile.CurrentPolyLineHeader.NumPoints) do begin
                   TheMapOwner.MapDraw.LatLongDegreeToScreen(aShapeFile.CurrentLineCoords^[i].Lat,aShapeFile.CurrentLineCoords^[i].Long,xpic,ypic);
                   textCoords.Add(integertostring(xpic,10) + integerToString(Ypic,12));
                end;
                DisplayAndPurgeStringList(TextCoords,dbName + ' Rec: ' + IntToStr(MyData.RecNo) + '  Pts: ' + IntToStr(aShapeFile.CurrentPolyLineHeader.NumPoints));
             end
             else begin
                if (Sender = RecordcoordinatesCSV1)  then textCoords.Add('LAT,LONG,Z');
                Capt := '';
                for i := 0 to pred(aShapeFile.CurrentPolyLineHeader.NumPoints) do begin
                   if ShapeFile3D(ShapeFileType) then Capt := RealToString(GISdb[DBonTable].aShapeFile.CurrentLineZs^[i],12,2);
                   Lat := aShapeFile.CurrentLineCoords^[i].Lat;
                   Long := aShapeFile.CurrentLineCoords^[i].Long;
                   if (Sender = RecordcoordinatesCSV1)  then textCoords.Add(RealToString(Lat,-18,-8) + ',' + RealToString(Long,-18,-8) + ',' + RealToString(aShapeFile.CurrentLineZs^[i],-18,-2))
                   else textCoords.Add(LatLongDegreeToString(Lat,Long,MDdef.OutPutLatLongMethod) + Capt);
                end;
             end;
             if GISdb[DBonTable].MyData.FieldExists(dbOpts.LabelField) then Capt := GISdb[DBonTable].MyData.GetFieldByNameAsString(dbOpts.LabelField) + '  '
             else Capt := '';
             Capt := Capt + dbName + ' Rec=' + IntToStr(MyData.RecNo) + '  Pts=' + IntToStr(aShapeFile.CurrentPolyLineHeader.NumPoints);
             if (aShapeFile.CurrentPolyLineHeader.NumParts > 1) then Capt := Capt + '  Parts=' + IntToStr(aShapeFile.CurrentPolyLineHeader.NumParts);
             DisplayAndPurgeStringList(TextCoords,Capt);
         end;
      end
      else begin
         if (ItsAPointDB) then begin
            if ValidLatLongFromTable(Lat,Long) then MessageToContinue(LatLongDegreeToString(Lat,Long) + MessLineBreak +
                               LatLongDegreeToString(Lat,Long,DecMinutes) + MessLineBreak +
                               LatLongDegreeToString(Lat,Long,DecSeconds) + MessLineBreak +
                               'MGRS=' + TheMapOwner.MapDraw.PrimMapProj.LatLongToMGRS(Lat,Long),True);
         end;
      end;
   end;
   ShowStatus;
end;


procedure Tdbtablef.RecordcoordinatesCSV1Click(Sender: TObject);
begin
   RecordCoordinates1Click(Sender);
end;

procedure Tdbtablef.Recordscreencoordinates1Click(Sender: TObject);
begin
   RecordCoordinates1Click(Sender);
end;


procedure Tdbtablef.Recordswithholes1Click(Sender: TObject);
begin
   Multipartrecords1Click(Sender);
end;

procedure Tdbtablef.RecordtoKML1Click(Sender: TObject);
begin
   ConvertToKML(DBonTable,'',nil,true);
end;

procedure Tdbtablef.GDALsubsettomatchthisrecord1Click(Sender: TObject);
const
   Extra = 0.25;
var
   Lat,Long : float64;
   bbox : sfBoundBox;
begin
    if LineOrAreaShapeFile(GISdb[DBonTable].ShapeFileType) then begin
       bbox := GISdb[DBonTable].MyData.GetRecordBoundingBox;
    end
    else begin
        GISdb[DBonTable].MyData.ValidLatLongFromTable(Lat,Long);
        bbox.YMax := Lat + extra;
        bbox.XMin := Long - extra;
        bbox.YMin := Lat - Extra;
        bbox.XMax := Long + extra;
    end;
    GDALsubsetGridAndOpen(bbox,true,'',true);
end;


procedure Tdbtablef.Geocodeaddresses1Click(Sender: TObject);
var
   Lat,Long : float64;
   i,rc : integer;
   TheField : ShortString;
   Address : shortString;
   Asked : boolean;
begin
   with GISdb[DBonTable] do begin
      AddLatLong;
      if GISdb[DBonTable].MyData.FieldExists('ADDRESS') then TheField := 'ADDRESS'
      else TheField := PickField('Addresses',[ftString]);
      StartProgressAbortOption('Geocode');
      GISdb[DBonTable].MyData.First;
      i := 0;
      Asked := false;
      rc := GISdb[DBonTable].MyData.RecordCount;
      while not GISdb[DBonTable].MyData.EOF do begin
         if (i mod 25 = 0) then begin
            EmpSource.Enabled := false;
            UpdateProgressBar(i/rc);
         end;
         inc(i);
         if not ValidLatLongFromTable(Lat,Long) then begin
            Address := GISdb[DBonTable].MyData.GetFieldByNameAsString(TheField);
            TheMapOwner.AddressGeocode(false,false,Address,Lat,Long);
            if (Lat > -89) then begin
               GISdb[DBonTable].MyData.Edit;
               GISdb[DBonTable].MyData.SetFieldByNameAsFloat(LatFieldName,Lat);
               GISdb[DBonTable].MyData.SetFieldByNameAsFloat(LongFieldName,Long);
            end
            else begin
               if not Asked then begin
                  Wantout := AnswerIsYes('Invalid results for ' + Address + '; stop Geocoding');
                  Asked := true;
               end;
            end;
         end;
         GISdb[DBonTable].MyData.Next;
         if WantOut then break;
      end;
      ShowStatus;
   end;
end;


procedure Tdbtablef.Geocodelatlong1Click(Sender: TObject);
var
   Lat,Long : float64;
   i,rc : integer;
   Address : shortString;
begin
   {$IfDef RecordGeoCoding} WriteLineRoDebugFile('Tdbtablef.Geocodelatlong1Click in'); {$EndIf}
   with GISdb[DBonTable] do begin
      AddLatLong;
      EmpSource.Enabled := false;
      AddFieldToDataBase(ftString,'ADDRESS',128);
      {$IfDef RecordGeoCoding} WriteLineRoDebugFile('field added'); {$EndIf}
      EmpSource.Enabled := false;

      StartProgressAbortOption('Geocode');
      GISdb[DBonTable].MyData.First;
      i := 0;
      rc := GISdb[DBonTable].MyData.RecordCount;
      while not GISdb[DBonTable].MyData.EOF do begin
         if (i mod 25 = 0) then begin
            EmpSource.Enabled := false;
            UpdateProgressBar(i/rc);
         end;
         inc(i);
         if ValidLatLongFromTable(Lat,Long) then begin
           {$IfDef RecordGeoCoding} WriteLineRoDebugFile('do record ' + LatLongDegreeToString(Lat,long)); {$EndIf}
           Address := '';
           TheMapOwner.AddressGeocode(true,true,Address,Lat,Long,false);
           if (Address <> '') then begin
              GISdb[DBonTable].MyData.Edit;
              GISdb[DBonTable].MyData.SetFieldByNameAsString('ADDRESS',Address);
           end;
         end;
         GISdb[DBonTable].MyData.Next;
         if WantOut then break;
      end;
      ShowStatus;
   end;
end;


procedure Tdbtablef.Geomorphometryatlas1Click(Sender: TObject);
begin
   {$IfDef ExGeology}
   {$Else}
   Grid_over_map.GridOverlayPointsOnMap(GISDB[DBonTable].TheMapOwner,DBOnTable);
   {$EndIf}
end;


procedure Tdbtablef.Geomorphometrystats1Click(Sender: TObject);
begin
   {$IfDef ExGeoStats}
   {$Else}
   DEMStat.MakeGeomporphDBforPolygons(DBonTable);
   {$EndIf}
end;



procedure Tdbtablef.Geomrophometrystaseachpointneighborhood1Click(Sender: TObject);
begin
   {$IfDef ExGeoStats}
   {$Else}
   DEMStat.MakeGeomporphDBforPoints(DBonTable);
   {$EndIf}
end;

procedure Tdbtablef.Getazimuthfrommagneticheading1Click(Sender: TObject);
{$IfDef ExMag}
begin
{$Else}
var
   i,rc  : integer;
   DEC,DIP,TI,GV : double;
begin
     GISdb[DBonTable].AddFieldToDataBase(ftFloat,'AZIMUTH',6,2);
     GISdb[DBonTable].MyData.First;
     i := 0;
     rc := GISdb[DBonTable].MyData.RecordCount;
     ShowHourglassCursor;
     StartProgress('Mag azimuth');
     while not GISdb[DBonTable].MyData.EOF do begin
        if (i mod 100 = 0) then begin
           GISdb[DBonTable].EmpSource.Enabled := false;
           UpdateProgressBar(i/rc);
        end;
        inc(i);
        GISdb[DBonTable].MyData.Edit;
        MagVr1(GISdb[DBonTable].MyData.GetFieldByNameAsFloat('ALTITUDE'), GISdb[DBonTable].MyData.GetFieldByNameAsFloat(GISdb[DBonTable].LatFieldName),GISdb[DBonTable].MyData.GetFieldByNameAsFloat(GISdb[DBonTable].LongFieldName),
              CurMagYear,DEC,DIP,TI,GV);
        GISdb[DBonTable].MyData.SetFieldByNameAsFloat('AZIMUTH',GISdb[DBonTable].MyData.GetFieldByNameAsFloat('MAG_HEAD') + Dec);
        GISdb[DBonTable].MyData.Next;
     end;
     ShowStatus;
{$EndIf}
end;


procedure Tdbtablef.EditPointSymbol1Click(Sender: TObject);
var
   Sym : tDrawingSymbol;
   SymSize : byte;
   SymColor : tPlatformColor;
   Font : tFont;
begin
   with GISdb[DBonTable] do begin
      GISdb[DBonTable].MyData.Edit;
      if (Sender = EditPointSymbol1) and PointSymbolFieldsPresent then begin
         GISdb[DBonTable].MyData.DefinePointSymbol(Sym,SymSize,SymColor);
         GetSymbol(Sym,SymSize,SymColor);
         GISdb[DBonTable].MyData.PostPointSymbol(Sym,SymSize,SymColor);
      end;
      if (Sender = EditFont1) and FontFieldExists then begin
         Font := tFont.Create;
         GISdb[DBonTable].MyData.DefineFontFromTable(Font);
         EditTFont(Font);
         GISdb[DBonTable].MyData.PostFont(Font);
         Font.Destroy;
      end;
      GISdb[DBonTable].MyData.Post;
   end;
end;


procedure Tdbtablef.Keepafte1Click(Sender: TObject);
begin
   Keepbefore1Click(Sender);
end;


procedure Tdbtablef.Keepbefore1Click(Sender: TObject);
var
   WantedFieldName,NewField,NewField2,NewField3,sep : shortstring;
   i : Integer;
   TStr,TStr1,TStr2 : ANSIstring;
   ch : ANSIChar;
begin
   with GISdb[DBonTable] do begin
     WantedFieldName := PickField('Field fo split',[ftString]);

     NewField := WantedFieldName + '2';
     NewField := GetFieldNameForDB('New Field (1)',True,NewField);
     i := GISdb[DBonTable].MyData.GetFieldLength(WantedFieldName);

     if (Sender = Dividetwofields1) or (Sender = Dividethreefields1) then begin
       NewField2 := WantedFieldName + '3';
       NewField2 := GetFieldNameForDB('New Field (2)',True,NewField2);
       //AddFieldToDataBase(ftString,NewField2,i,0);
       if (Sender = Dividethreefields1) then begin
          NewField3 := WantedFieldName + '3';
          NewField3 := GetFieldNameForDB('New Field (3)',True,NewField3);
          //AddFieldToDataBase(ftString,NewField3,i,0);
       end;
     end;

     sep := ':';
     Petmar.GetString('Separator (1 character)',sep,true,ReasonableTextChars);
     ch := Sep[1];

     AddFieldToDataBase(ftString,NewField,i,0);
     if (Sender = Dividetwofields1) or (Sender = Dividethreefields1) then AddFieldToDataBase(ftString,NewField2,i,0);
     if (Sender = Dividethreefields1) then AddFieldToDataBase(ftString,NewField3,i,0);

     ShowHourglassCursor;
     EmpSource.Enabled := false;
     GISdb[DBonTable].MyData.First;
     while not GISdb[DBonTable].MyData.EOF do begin
        MyData.Edit;
        TStr := ptTrim(MyData.GetFieldByNameAsString(WantedFieldName));
        if (TStr <> '') then begin
          if (Sender = Dividethreefields1) then begin
             TStr1 := Petmar_types.BeforeSpecifiedCharacterANSI(TStr,ch,false,true);
             TStr2 := Petmar_types.BeforeSpecifiedCharacterANSI(TStr,ch,false,true);
             GISdb[DBonTable].MyData.SetFieldByNameAsString(NewField,ptTrim(TStr1));
             GISdb[DBonTable].MyData.SetFieldByNameAsString(NewField2,ptTrim(TStr2));
             GISdb[DBonTable].MyData.SetFieldByNameAsString(NewField3,ptTrim(TStr));
          end
          else begin
             TStr2 := Petmar_types.AfterSpecifiedCharacter(TStr,ch);
             TStr := Petmar_types.BeforeSpecifiedCharacterANSI(TStr,ch,false);
             if Sender = Keepafte1 then TStr := TStr2;
             GISdb[DBonTable].MyData.SetFieldByNameAsString(NewField,ptTrim(TStr));
             if (Sender = Dividetwofields1) then GISdb[DBonTable].MyData.SetFieldByNameAsString(NewField2,ptTrim(TStr2));
          end;
        end;
        GISdb[DBonTable].MyData.Next;
     end;
     ShowStatus;
   end;
end;

procedure Tdbtablef.Keyboardnewpointlocation1Click(Sender: TObject);
var
   Lat,Long : float64;
begin
   with GISdb[DBonTable] do begin
      if (MyData.GetFieldByNameAsString(LatFieldName) = '') or  (MyData.GetFieldByNameAsString(LongFieldName) = '') then begin
         Lat := 0;
         Long := 0;
      end
      else begin
         Lat := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(LatFieldName);
         Long := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(LongFieldName);
      end;
      GetLatLongDefault(TheMapOwner.MapDraw.PrimMapProj,'revised location',Lat,Long);
      GISdb[DBonTable].MyData.Edit;
      ChangeLatLongLocation(Lat,Long);
      GISdb[DBonTable].MyData.Post;
   end;
end;


procedure Tdbtablef.RecordDisplay1Click(Sender: TObject);
begin
   PrevNextButtonsEnabled := true;
   GISdb[DBonTable].DisplayTheRecord(GISdb[DBonTable].MyData.RecNo,MDDef.ModalDBDisplay);
end;


procedure Tdbtablef.RecordEdit1Click(Sender: TObject);
begin
   GISdb[DBonTable].DisplayTheRecord(GISdb[DBonTable].MyData.RecNo,MDDef.ModalEditDBRec,true);
   if GISdb[DBonTable].ItsFanFile then begin
      GISdb[DBonTable].ClearImage;
      GISdb[DBonTable].TheMapOwner.DoFastMapRedraw;
   end;
end;
                  

procedure Tdbtablef.Recordnumber1Click(Sender: TObject);
begin
   GISdb[DBonTable].AddSequentialIndex(RecNoFName);
end;

procedure Tdbtablef.Recordpoints1Click(Sender: TObject);
begin
  GISdb[DBonTable].AddGeometry(agNumPts);
end;

procedure Tdbtablef.Dipandstrikes1Click(Sender: TObject);
{$IfDef ExGeology}
begin
{$Else}
begin
   if (Sender = Dipandstrikes1) then begin
      GISdb[DBonTable].dbOpts.DBAutoShow := dbasDipStrike;
      GISdb[DBonTable].RedrawLayerOnMap;
   end;
{$EndIf}
end;


procedure Tdbtablef.Dipdirectionsforfocalplanes1Click(Sender: TObject);
begin
   Polestofocalplanes1Click(Sender);
end;

procedure Tdbtablef.Directionofeachrecord1Click(Sender: TObject);
begin
  GISdb[DBonTable].AddGeometry(agDirection);
end;

procedure Tdbtablef.Edit1Change(Sender: TObject);
begin
   //with GISdb[DBonTable] do begin
      GISdb[DBonTable].dbOpts.MainFilter := GISdb[DBonTable].dbOpts.LabelField + '=' + QuotedStr(Edit1.Text + '*');
      GISdb[DBonTable].AssembleGISFilter;
   //end;
   ShowStatus;
end;


procedure Tdbtablef.ShowStatus;

      procedure ArrangeButtons;
      var
         glLeftPos : integer;

         procedure CheckBitButton(var SB1 : tBitBtn);
         begin
            //==========================================================
            //Must Check in the same order buttons appear on the toolbar
            //==========================================================
            SB1.Left := glLeftPos;
            if SB1.Visible then begin
               //inc(ButtonsVisible);
               SB1.Top := 1;
               glLeftPos := glLeftPos + SB1.Width+2;
            end;
         end;

         procedure CheckButton(var SB1 : tButton);
         begin
            //==========================================================
            //Must Check in the same order buttons appear on the toolbar
            //==========================================================
            SB1.Left := glLeftPos;
            if SB1.Visible then begin
               //inc(ButtonsVisible);
               SB1.Top := 1;
               glLeftPos := glLeftPos + SB1.Width+2;
            end;
         end;


      begin
         glLeftPos := 1;
         CheckBitButton(BitBtn28);
         CheckBitButton(BitBtn17);
         CheckBitButton(BitBtn1);
         CheckBitButton(BitBtn4);
         CheckButton(Button1);
         CheckBitButton(BitBtn5);
         CheckBitButton(BitBtn8);
         CheckBitButton(BitBtn7);
         CheckButton(Button5);
         CheckBitButton(BitBtn12);
         CheckButton(Button4);
         CheckBitButton(BitBtn9);
         CheckBitButton(BitBtn13);
         CheckBitButton(BitBtn23);
         CheckBitButton(BitBtn24);
         CheckBitButton(HelpBtn);
         CheckBox1.Left := glLeftPos + 5;
      end;

var
   tstr : shortstring;
begin
   {$IfDef RecordShowStatus} WriteLineRoDebugFile('ShowStatus in, db=' + IntToStr(dbOnTable)); {$EndIf}
   if (Closing <> true) and ValidDB(DBonTable) then begin
      if GISdb[DBonTable].dbIsUnsaved then TStr := 'Unsaved--'
      else TStr := '';
      Caption := tstr + GISdb[DBonTable].dbName + ' data base';
      GISdb[DBonTable].EmpSource.Enabled := true;

      BitBtn1.Visible := GISdb[DBonTable].CanPlot and GISdb[DBonTable].DBHasMapOrGraph;
      BitBtn5.Visible := BitBtn1.Visible;
      BitBtn28.Visible := BitBtn1.Visible;
      BitBtn7.Visible := BitBtn1.Visible;
      Button5.Enabled := GISdb[DBonTable].LatLongFieldsPresent or GISdb[DBonTable].LatLongCornersPresent;
      Button1.Enabled := (GISdb[DBonTable].MyData <> Nil) and GISdb[DBonTable].MyData.Filtered;
      BitBtn13.Enabled :=  AnyHiddenColumns;
      {$IfDef ExSidescan} BitBtn17.Visible := false; {$EndIf}

      Zstatistics1.Visible := GISdb[DBonTable].ShapeFileType in [13,23];
      Addfontdefinition1.Visible := not GISdb[DBonTable].FontFieldExists;
      Insertpointsymbol1.Visible := not GISdb[DBonTable].PointSymbolFieldsPresent;
      {$IfDef RecordShowStatus} WriteLineRoDebugFile('check num recs'); {$EndIf}
      if GISdb[DBonTable].MyData.Filtered then begin
         if (GISdb[DBonTable].MyData.FiltRecsInDB = 0) then TStr := 'No Records match filter, from ' + IntToStr(GISdb[DBonTable].MyData.TotRecsInDB)
         else TStr := 'Records displayed: ' + IntToStr(GISdb[DBonTable].MyData.FiltRecsInDB) + '/' + IntToStr(GISdb[DBonTable].MyData.TotRecsInDB);
      end
      else TStr := 'All Records displayed: ' + IntToStr(GISdb[DBonTable].MyData.TotRecsInDB);
      StatusBar1.Panels[0].Text := TStr;
      {$IfDef RecordShowStatus} WriteLineRoDebugFile('call arrange buttons'); {$EndIf}
      ArrangeButtons;
      if GISdb[DBonTable].LayerIsOn then  begin
         BitBtn28.Caption := '√';
         BitBtn28.Font.Color := clGreen;
      end
      else begin
         BitBtn28.Caption := 'X';
         BitBtn28.Font.Color := clRed;
      end;
      {$IfDef RecordShowStatus} WriteLineRoDebugFile('call hide columns'); {$EndIf}
      HideColumns;
      EndProgress;
      ShowDefaultCursor;
      {$IfDef RecordStatus} WriteLineRoDebugFile('UpdateStatus EmpSource.Enabled=' + TrueOrFalse(GISdb[DBonTable].EmpSource.Enabled) + '  DBGrid1.Enabled=' + TrueOrFalse(DBGrid1.Enabled)): {$EndIf}
   end;
   {$IfDef RecordShowStatus} WriteLineRoDebugFile('ShowStatus out'); {$EndIf}
 end;


procedure Tdbtablef.RedclassifyLoppen1Click(Sender: TObject);
{$IfDef ExGeography}
begin
{$Else}
var
   ClimateData : tClimateData;
   Color : tPlatformColor;
begin
   GISdb[DBonTable].EmpSource.Enabled := false;
   GISdb[DBonTable].MyData.First;
   while not GISdb[DBonTable].MyData.eof do begin
       ProcessClimateStationData(ClimateData);
       GISdb[DBonTable].MyData.Edit;
       GISdb[DBonTable].MyData.SetFieldByNameAsString('CLASS',ClimateData.L1 + ClimateData.L2 + ClimateData.L3);
       if GetKoppenColor(ClimateData.L1 + ClimateData.L2 + ClimateData.L3, Color) then GISdb[DBonTable].MyData.SetFieldByNameAsInteger('COLOR',ConvertPlatformColorToTColor(Color));
       GISdb[DBonTable].MyData.Next;
   end;
   ShowStatus;
{$EndIf}
end;

procedure Tdbtablef.Redistrict1Click(Sender: TObject);
begin
   {$IfDef ExRedistrict}
   {$Else}
      DEMRedistrict.StartRedistricting(DBonTable);
   {$EndIf}
end;


procedure Tdbtablef.Reflectancespectrasingleclass1Click(Sender: TObject);
{$IfDef ExSat}
begin
{$Else}
var
   MovieSL,InList : TStringList;
   i,j : integer;
   aGraph : tThisBaseGraph;
   aName : shortstring;
   fName : PathStr;
begin
   {$IfDef RecordMultigrid} WriteLineRoDebugFile('Tdbtablef.Reflectancespectrasingleclass1Click in'); {$EndIf}
   InList := GetMultipleEntriesFromTableField('Class reflectance spectra', 'NAME');
   //with GISdb[DBonTable] do begin
      if (InList.Count > 1) then MovieSL := tStringList.Create;
      for i := 0 to pred(InList.Count) do begin
         aName := InList.Strings[i];
         GISdb[DBonTable].MyData.ApplyFilter('NAME=' + QuotedStr(aName));
         aGraph := tThisBaseGraph.Create(Application);
         for j := 1 to 255 do aGraph.GraphDraw.FileColors256[j] := GISdb[DBonTable].MyData.PlatformColorFromTable;
         DrawPointReflectanceGraph(aGraph,'Reflectances ' + GISdb[DBonTable].MyData.Filter,false);
         if (InList.Count > 1) then begin
            fName := aName + MovieFileExt;
            aGraph.Image1.Picture.SaveToFile(DEMdefs.MovieDir + fName);
            MovieSL.Add(fName);
         end;
      end;
      GISdb[DBonTable].ClearGISFilter;
      {$IfDef ExMovies}
      {$Else}
         if (InList.Count > 1) then begin
            fName := 'training.mov';
            MovieSL.SaveToFile(DEMdefs.MovieDir + fName);
            PetImage.MakeMovie(fName);
         end;
      {$EndIf}
      Inlist.Free;
      MovieSL.Free;
      ShowStatus;
   //end;
{$EndIf}
end;


procedure Tdbtablef.Removeduplicatepositions1Click(Sender: TObject);
begin
   GISdb[DBonTable].RemoveDuplicatePositions;
end;


procedure Tdbtablef.RemovefinalCharacterClick(Sender: TObject);
begin
   Removelowercasecharacters1Click(Sender);
end;

procedure Tdbtablef.Removeifsubstringpresent1Click(Sender: TObject);
begin
   Removelowercasecharacters1Click(Sender);
end;

procedure Tdbtablef.Removeinitialcharacter1Click(Sender: TObject);
begin
   Removelowercasecharacters1Click(Sender);
end;

procedure Tdbtablef.Removeleadingzeros1Click(Sender: TObject);
var
   WantedFieldName : ShortString;
begin
   //with GISdb[DBonTable] do begin
     WantedFieldName := GISdb[DBonTable].PickField('Field to remove leading zeros',[ftString]);
     GISdb[DBonTable].RemoveLeadingZerosInField(GISdb[DBonTable].MyData,WantedFieldName);
   //end;
end;

procedure Tdbtablef.Removelowercasecharacters1Click(Sender: TObject);
var
   aString,ExtToAdd,Ext : ShortString;
   Retain : integer;
begin
   with GISdb[DBonTable] do begin
      ShowHourglassCursor;
      if (Sender = Addfileextension1) then Petmar.GetString('Extension to add',ExtToAdd,false,ValidDosFileNameChars);
      Retain := 8;
      if (Sender = Retainfirstncharacters1) then ReadDefault('Initial characters to retain',Retain);
      if (Sender = Removeinitialcharacter1) then begin
         Petmar.GetString('Initial character to delete',ExtToAdd,false,ValidDosFileNameChars);
      end;
      if (Sender = RemovefinalCharacter) then begin
         Petmar.GetString('Final character to delete',ExtToAdd,false,ValidDosFileNameChars);
      end;
      if (Sender = Removeifsubstringpresent1) then Petmar.GetString('Substring to remove',ExtToAdd,false,ValidDosFileNameChars);
      if (Sender = Removesubstringandfollowingcharacters1) then Petmar.GetString('Substring to remove (and rest of line)',ExtToAdd,false,ReasonableTextChars);

      EmpSource.Enabled := false;
      GISdb[DBonTable].MyData.First;
      while not GISdb[DBonTable].MyData.eof do begin
         aString := GISdb[DBonTable].MyData.GetFieldByNameAsString(SelectedColumn);
         if (Sender = Removenumericalcharacters1) then aString := RemoveNumbers(aString)
         else if (Sender = RemovefinalCharacter) then begin
            if (aString[Length(astring)] = extToAdd[1]) then System.Delete(astring,length(astring),1);
         end
         else  if (Sender = Removeinitialcharacter1) then begin
            if (aString[1] = extToAdd[1]) then System.Delete(astring,1,1);
         end
         else if (Sender = Retainfirstncharacters1) then Astring := Copy(Astring,1,Retain)
         else if (Sender = Addfileextension1) then begin
            if (AString <> '') then begin
               Ext := UpperCase(ExtractFileExt(AString));
               if Ext <> UpperCase(ExtToAdd) then Astring := aString + ExtToAdd;
            end;
         end
         else if (Sender = Removeifsubstringpresent1) then begin
            if StrUtils.AnsiContainsText(aString,ExtToAdd) then aString := '';
         end else if (Sender = Removesubstringandfollowingcharacters1) then begin
            astring := Petmar_Types.BeforeSpecifiedString(aString,ExtToAdd);
         end
         else aString := RemoveLowerCase(aString);
         GISdb[DBonTable].MyData.Edit;
         GISdb[DBonTable].MyData.SetFieldByNameAsString(SelectedColumn,aString);
         GISdb[DBonTable].MyData.Next;
      end;
      ShowStatus;
   end;
end;


procedure Tdbtablef.Removemissingdata1Click(Sender: TObject);
var
   fName : ShortString;
   i : integer;

      procedure OneField(fName : ShortString);
      begin
           GISdb[DBonTable].SaveFilterStatus(true);
           GISdb[DBonTable].MyData.ApplyFilter(fName + '=' + IntToStr(MDDef.ASCIIMissingValue));
           GISdb[DBonTable].FillFieldWithValue(fName,'');
           GISdb[DBonTable].EmpSource.Enabled := false;
           GISdb[DBonTable].RestoreFilterStatus;
      end;


begin
   if AnswerIsYes('All numeric fields') then begin
      fName := '';
   end
   else begin
      fName := GISdb[DBonTable].PickField('Field to remove missing data',[ftInteger,ftSmallInt,ftFloat]);
      if fName = '' then exit;
   end;
   ReadDefault('Missing value',MDDef.ASCIIMissingValue);

   if (fName = '') then begin
      OneField(fName);
   end
   else begin
       for i := 0 to pred(GISdb[DBonTable].MyData.FieldCount) do begin
          if (GISdb[DBonTable].MyData.GetFieldType(i) in [ftInteger,ftSmallInt,ftFloat]) then begin
             fName := GISdb[DBonTable].MyData.GetFieldName(i);
             OneField(fName);
          end;
       end;
   end;
   ShowStatus;
end;


procedure Tdbtablef.Removenonnumericentries1Click(Sender: TObject);
begin
   Countuniquevalues1Click(Sender);
end;

procedure Tdbtablef.Removenumericalcharacters1Click(Sender: TObject);
begin
   Removelowercasecharacters1Click(Sender);
end;

procedure Tdbtablef.Removerowsmissinganyevaluations1Click(Sender: TObject);
begin
   FilterTableForDEMIXevaluation(DBonTable,0);
   if (GISdb[DBonTable].MyData.FiltRecsInDB = 0) then begin
      MessageToContinue('No records with missing data');
   end
   else GISdb[DBonTable].DeleteAllSelectedRecords;
end;

procedure Tdbtablef.Removesubstringandfollowingcharacters1Click(Sender: TObject);
begin
   Removelowercasecharacters1Click(Sender);
end;

procedure Tdbtablef.Trimblanks1Click(Sender: TObject);
begin
   GISdb[DBonTable].EmpSource.Enabled := false;
   GISdb[DBonTable].MyData.TrimAllStringFields(SelectedColumn);
   ShowStatus;
end;


procedure Tdbtablef.Renamefield1Click(Sender: TObject);
var
   WantedFieldName : ShortString;
begin
   WantedFieldName := SelectedColumn;
   WantedFieldName := GISdb[DBonTable].GetFieldNameForDB('Rename Field',true,WantedFieldName);
   if GISdb[DBonTable].MyData.FieldExists(WantedFieldName) then
      MessageToContinue(WantedFieldName + ' already exists')
   else GISdb[DBonTable].RenameField(SelectedColumn,WantedFieldName);
   ShowStatus;
end;

procedure Tdbtablef.Renamefieldsfromreferencetable1Click(Sender: TObject);
var
   tName : PathStr;
   Table : tMyData;
   FieldsRenamed,i : integer;
   OldFieldName,NewFieldName,
   NewName : ShortString;
   FieldsInDB : tStringList;
begin
   {$IfDef RecordFieldRename} WriteLineRoDebugFile('Tdbtablef.Renamefieldsfromreferencetable1Click in',true); {$EndIf}

   tName := ProgramRootDir + 'DP_TableDescriptions' + DefaultDBExt;
   if GetFileFromDirectory('file with name subsitutions',DefaultDBMask,tName) then begin
      PetDBUtils.GetFields(GISdb[DBonTable].MyData,AllVis,[ftString,ftInteger,ftSmallInt,ftFloat],FieldsInDB,true);
      FieldsRenamed := 0;
      Table := tMyData.Create(tName);

      OldFieldName := 'CENS_NAME';
      if (Not Table.FieldExists(OldFieldName)) then OldFieldName := OrigPickField(Table,'Field name to replace',[ftString]);
      NewFieldName := 'DBF_NAME';
      if (Not Table.FieldExists(NewFieldName)) then NewFieldName := OrigPickField(Table,'Field name to replace with',[ftString]);

      for i := 0 to pred(FieldsInDB.Count) do begin
         {$IfDef RecordFieldRename} WriteLineRoDebugFile('check field=' + FieldsInDB.Strings[i], true): {$EndIf}
         GISdb[DBonTable].EmpSource.Enabled := false;
         Table.ApplyFilter(OldFieldName + '=' + QuotedStr(ptTrim(FieldsInDB.Strings[i])));
         {$IfDef RecordFieldRename} WriteLineRoDebugFile('filter=' + Table.Filter); {$EndIf}
         if (Table.RecordCount = 1) then begin
            NewName := ptTrim(Table.GetFieldByNameAsString(NewFieldName));
            if (NewName <> '') then begin
               {$IfDef RecordFieldRename} WriteLineRoDebugFile(FieldsInDB.Strings[i] + ' becomes ' + NewName); {$EndIf}
               GISdb[DBonTable].RenameField(FieldsInDB.Strings[i],NewName);
               inc(FieldsRenamed);
            end;
         end;
      end;
      Table.Destroy;
      MessageToContinue('Fields renamed: ' + IntToStr(FieldsRenamed));
   end;
   ShowStatus;
   {$IfDef RecordFieldRename} WriteLineRoDebugFile('Tdbtablef.Renamefieldsfromreferencetable1Click out'); {$EndIf}
end;


procedure Tdbtablef.rendsurface1Click(Sender: TObject);
{$IfDef ExGeostats}
begin
{$Else}
var
   GridLimits : tGridLimits;
begin
   GetReadyForGeologyGeometry;
   //with GISdb[DBonTable] do begin
       if GISdb[DBonTable].MyData.FieldExists('X_UTM') and GISdb[DBonTable].MyData.FieldExists('Y_UTM') and GISdb[DBonTable].MyData.FieldExists('Z') then begin
          GISdb[DBonTable].dbOpts.XField := 'X_UTM';
          GISdb[DBonTable].dbOpts.YField := 'Y_UTM';
          GISdb[DBonTable].dbOpts.ZField := 'Z';
       end
       else begin
          GISdb[DBonTable].PickNumericFields(dbgtUnspecified,3,'X','Y','Z');
       end;
   //end;
   if ValidDEM(GISdb[DBonTable].theMapOwner.MapDraw.DEMonMap) then begin
      if MDDef.GeomorphMapsFullDEM then begin
         GridLimits := DEMGlb[GISdb[DBonTable].theMapOwner.MapDraw.DEMonMap].FullDEMGridLimits;
      end
      else begin
         GridLimits := DEMGlb[GISdb[DBonTable].theMapOwner.MapDraw.DEMonMap].SelectionMap.MapDraw.MapAreaDEMGridLimits;
      end;
   end;

   GetTrendOptions(DBonTable,GISdb[DBonTable].theMapOwner.MapDraw.DEMonMap,GridLimits,GISdb[DBonTable].theMapOwner);
{$EndIf}
end;

procedure Tdbtablef.ReplaceDialog1Find(Sender: TObject);
var
   aString : AnsiString;
begin
   //with GISdb[DBonTable] do begin
      GISdb[DBonTable].EmpSource.Enabled := false;
      while not GISdb[DBonTable].MyData.eof do begin
         aString := GISdb[DBonTable].MyData.GetFieldByNameAsString(SelectedColumn);
         if AnsiContainsText(aString,ReplaceDialog1.FindText) then break;
         GISdb[DBonTable].MyData.Next;
      end;
      ShowStatus;
   //end;
end;

procedure Tdbtablef.ReplaceDialog1Replace(Sender: TObject);
var
   Before,After : ShortString;
   //TotChanged,
   Changed : integer;
begin
   if ReplaceDialog1.Execute and (ReplaceDialog1.FindText <> '') and (ReplaceDialog1.FindText <> ReplaceDialog1.ReplaceText) then begin
     Before := ReplaceDialog1.FindText;
     After := ReplaceDialog1.ReplaceText;
     try
        ShowHourglassCursor;
        //TotChanged := 0;
        repeat
           SearchAndReplace(SelectedColumn, Before,After,Changed);
           //inc(TotChanged, Changed);
        until (Changed = 0) or (GISdb[DBonTable].MyData.Filter = '');
     finally
        ShowStatus;
     end;
     if (Changed > 0) then begin
        MessageToContinue(IntToStr(Changed) + ' replacements');
     end;
   end;
end;


procedure Tdbtablef.Coordinatestotextfile1Click(Sender: TObject);
var
   textCoords : tStringList;


  procedure Compute(Method : tLatLongMethod);
  var
     i,Num : integer;
  begin
         Num := 0;
         StartProgress('Compute');
         GISdb[DBonTable].MyData.First;
         while not GISdb[DBonTable].MyData.EOF do begin
            inc(Num);
            if (Num mod 500 = 0) then UpdateProgressBar(Num/GISdb[DBonTable].MyData.FiltRecsInDB);
            GISdb[DBonTable].aShapeFile.GetLineCoords(GISdb[DBonTable].MyData.RecNo,false);
            if GISdb[DBonTable].MyData.FieldExists('NAME') then textCoords.Add(GISdb[DBonTable].MyData.GetFieldByNameAsString('NAME'));
            for i := 0 to pred(GISdb[DBonTable].aShapeFile.CurrentPolyLineHeader.NumPoints) do
               textCoords.Add(LatLongDegreeToString(GISdb[DBonTable].aShapeFile.CurrentLineCoords^[i].Lat,GISdb[DBonTable].aShapeFile.CurrentLineCoords^[i].Long,Method));
            textCoords.Add('----------');
            textCoords.Add('');
            textCoords.Add('');
            Next;
         end;
         EndProgress;
  end;

begin
   textCoords := TstringList.Create;
   GISdb[DBonTable].EmpSource.Enabled := false;
   Compute(MDDef.OutPutLatLongMethod);
   if AnswerIsYes('All lat/long formats') then begin
      if (MDDef.OutPutLatLongMethod <> DecDegrees) then Compute(DecDegrees);
      if (MDDef.OutPutLatLongMethod <> DecMinutes) then Compute(DecMinutes);
      if (MDDef.OutPutLatLongMethod <> DecSeconds) then Compute(DecSeconds);
   end;
   ShowStatus;
   Petmar.DisplayAndPurgeStringList(TextCoords,'Database record coordinates');
end;


procedure Tdbtablef.CopDEMandLandcoverforthistile1Click(Sender: TObject);
begin
   LoadCopAndLancoverForDEMIXTile(GISdb[DBonTable].MyData.GetFieldByNameAsString('AREA'),GISdb[DBonTable].MyData.GetFieldByNameAsString('DEMIX_TILE'));
end;


procedure Tdbtablef.CopHeadtoheadrecord1Click(Sender: TObject);
begin
   CreateCopHeadToHeaddb(dbOnTable);
end;

procedure Tdbtablef.COPoALOS1Click(Sender: TObject);
begin
   {$IfDef ExDEMIXexperimentalOptions}
   {$Else}
      DEMIXisCOPorALOSbetter(DBonTable);
   {$EndIf}
end;

procedure Tdbtablef.N2Dgraphcolorcodetext1Click(Sender: TObject);
begin
   {$IfDef NoDBGrafs}
   {$Else}
      GISdb[DBonTable].MakeGraph(dbgtN2Dgraphcolorcodetext1);
   {$EndIf}
end;


procedure Tdbtablef.N2DgraphCOLORfield1Click(Sender: TObject);
begin
   {$IfDef NoDBGrafs}
   {$Else}
      GISdb[DBonTable].MakeGraph(dbgtN2DgraphCOLORfield1);
   {$EndIf}
end;

procedure Tdbtablef.N2Dgraphsimplelines1Click(Sender: TObject);
begin
   {$IfDef NoDBGrafs}
   {$Else}
      GISdb[DBonTable].MakeGraph(dbgtN2Dgraphsimplelines1);
   {$EndIf}
end;

procedure Tdbtablef.Eachprofileseparately1Click(Sender: TObject);
begin
   with GISdb[DBonTable] do begin
      if (MyData.RecordCount > 35) and (Not AnswerIsYes('Proceed with ' + IntToStr(MyData.RecordCount) + ' records')) then exit;
      GISdb[DBonTable].MyData.First;
      while not GISdb[DBonTable].MyData.eof do begin
         N3Dshapefileprofile1Click(Sender);
         GISdb[DBonTable].MyData.Next;
      end;
   end;
end;

procedure Tdbtablef.Earthquakefocalmechanisms3D1Click(Sender: TObject);
begin
  GISdb[DBonTable].dbOpts.XField := GISdb[DBonTable].LongFieldName;
  GISdb[DBonTable].dbOpts.YField := GISdb[DBonTable].LatFieldName;
  GISdb[DBonTable].dbOpts.ZField := 'DEPTH';
  MDDef.ReverseZFields := true;
  ThreeDGraph(DBonTable,false);
end;


procedure Tdbtablef.Focalmechanisms1Click(Sender: TObject);
{$IfDef ExGeology}
begin
{$Else}
var
   TStr : ShortString;
   Num,bbWide,Thin,i,rc : integer;
   BigBitmap,Bitmap : tMyBitmap;
   ID_Present : boolean;
begin
   //with GISdb[DBonTable] do begin
      Thin := 1;
      while GISdb[DBonTable].MyData.RecordCount div Thin > MDDef.NetDef.MaxNumBeachBalls do inc(Thin);
      Num := 0;
      rc := GISdb[DBonTable].MyData.RecordCount;
      bbWide := 6;
      GISdb[DBonTable].MyData.First;
      CreateBitmap(BigBitmap,250 * bbWide,250 * ((pred(bbWide)+GISdb[DBonTable].MyData.RecordCount) div bbWide) + 50);
      BigBitmap.Canvas.Font.Size := 12;
      StartProgress('Compute');
      ID_Present := GISdb[DBonTable].MyData.FieldExists('EVENT_ID');
      repeat
         GISdb[DBonTable].EmpSource.Enabled := false;
         Bitmap := GISdb[DBonTable].DrawFocalMechanism(80);

         BigBitmap.Canvas.Draw(Num mod bbWide * 250,25 + Num div bbWide * 250,Bitmap);
         if ID_Present then begin
            BigBitmap.Canvas.Font.Size := 10;
            BigBitmap.Canvas.TextOut(Num mod bbWide * 250,(Num div bbWide * 250) + 245,
               GISdb[DBonTable].MyData.GetFieldByNameAsString('YEAR') + ' depth=' + GISdb[DBonTable].MyData.GetFieldByNameAsString('DEPTH') + ' MB=' + GISdb[DBonTable].MyData.GetFieldByNameAsString('MB'));
            TStr := GISdb[DBonTable].MyData.GetFieldByNameAsString('EVENT_ID');
         end
         else begin
            if GISdb[DBonTable].MyData.FieldExists('DATE') then TStr := GISdb[DBonTable].MyData.GetFieldByNameAsString('DATE')
            else TStr := GISdb[DBonTable].MyData.GetFieldByNameAsString('MO') + '/' + GISdb[DBonTable].MyData.GetFieldByNameAsString('DA') + '/' + GISdb[DBonTable].MyData.GetFieldByNameAsString('YR');
            TStr := 'Mw=' + GISdb[DBonTable].MyData.GetFieldByNameAsString('MW') + '  ' + TStr;
         end;

         if ID_Present then BigBitmap.Canvas.Font.Size := 12
         else BigBitmap.Canvas.Font.Size := 10;
         BigBitmap.Canvas.TextOut(Num mod bbWide * 250,(Num div bbWide * 250) + 230,TStr + '  ' + LatLongDegreeToString(GISdb[DBonTable].MyData.GetFieldByNameAsFloat(GISdb[DBonTable].LatFieldName),
            GISdb[DBonTable].MyData.GetFieldByNameAsFloat(GISdb[DBonTable].LongFieldName),ShortDegrees));
         Bitmap.Free;
         inc(Num);
         UpdateProgressBar(Num/rc);
         for i := 1 to thin do GISdb[DBonTable].MyData.Next;
      until GISdb[DBonTable].MyData.EOF;
      DisplayBitmap(BigBitmap,'Focal mechanism');
      BigBitmap.Free;
      ShowStatus;
   //end;
{$EndIf}
end;

procedure Tdbtablef.Focalplanesonstereonet1Click(Sender: TObject);
begin
   Polestofocalplanes1Click(Sender);
end;

procedure Tdbtablef.Earthquakemechanisms1Click(Sender: TObject);
begin
   {$IfDef ExGeology}
   {$Else}
      GISdb[DBonTable].dbOpts.DBAutoShow := dbasQuakeMechColor;
      GISdb[DBonTable].RedrawLayerOnMap;
   {$EndIf}
end;

procedure Tdbtablef.Ternarydiagram1Click(Sender: TObject);
type
   tPoints = record
      x,y,z : float64;
   end;
var
   i : integer;
   sum : float64;
   ThisGraph   : TThisBaseGraph;
   rFile : file;
   Points : array[1..1000] of tPoints;
begin
   with GISdb[DBonTable] do begin
      if not AssignField(dbOpts.XField,'CLAY') then AssignField(dbOpts.XField,'CLAY_PCT');
      if not AssignField(dbOpts.YField,'SAND') then AssignField(dbOpts.YField,'SAND_PCT');
      if not AssignField(dbOpts.ZField,'SILT') then AssignField(dbOpts.ZField,'SILT_PCT');

      if (Sender <> Nil) then GISdb[DBonTable].PickNumericFields(dbgtUnspecified,3,'Top corner','Lower left corner','Lower right corner');
      {$IfDef RecordDataBase} WriteLineRoDebugFile('Ternary: ' + dbOpts.XField + '/' + dbOpts.YField + '/' + dbOpts.ZField); {$EndIf}
      TernaryPlotUp := true;
      theMapOwner.DoFastMapRedraw;

      EmpSource.Enabled := false;
      ThisGraph := TThisBaseGraph.Create(Application);

      ThisGraph.GraphDraw.HorizLabel := dbOpts.XField;
      ThisGraph.GraphDraw.VertLabel := dbOpts.YField;
      ThisGraph.GraphDraw.ThirdLabel := dbOpts.ZField;
      ThisGraph.SetDataBaseOnGraph(DBonTable, dbOpts.XField,dbOpts.YField,MyData.Filter);
      ThisGraph.OpenXYZFile(rfile);
      ShowHourglassCursor;

      GISdb[DBonTable].MyData.First;
      i := 0;
      repeat
         inc(i);
         with Points[i] do begin
            x := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(dbOpts.XField);
            y := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(dbOpts.YField);
            z := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(dbOpts.ZField);
            sum := x + y + z;
            if (sum < 0.001) or ((MDDef.TernaryPercentageValues) and ((sum < 98) and (sum > 102))) then dec(i);
         end;
         if (i = 1000) then begin
            BlockWrite(rfile,Points,1000);
            i := 0;
         end;
         GISdb[DBonTable].MyData.Next;
      until GISdb[DBonTable].MyData.EOF;
      BlockWrite(rfile,Points,i);
      CloseFile(rfile);

      ThisGraph.TernarySymSize := dbOpts.Symbol.Size;
      ThisGraph.GraphDraw.GraphType := gtTernary;
      ThisGraph.GraphDraw.GraphDrawn := true;
      ThisGraph.RedrawDiagram11Click(nil);
      ThisGraph.Caption := DBName + ' Ternary Diagram';
      GISdb[DBonTable].dbOpts.DBAutoShow := dbasTernary;
      GISdb[DBonTable].RedrawLayerOnMap;
      ShowStatus;
   end;
end;


procedure Tdbtablef.Labelselectedrecords1Click(Sender: TObject);
begin
    GISdb[DBonTable].dbOpts.LabelField := GISdb[DBonTable].PickField('label based on field', [ftString,ftFloat,ftInteger,ftSmallInt]);
    ChangeDEMNowDoing(LabelIDDataBase);
end;


procedure Tdbtablef.Labelsensors1Click(Sender: TObject);
var
   Bitmap,Bitmap2 : tMyBitmap;
begin
   Bitmap := nil;
   //with GISdb[DBonTable] do begin
      GISdb[DBonTable].LabelRecordsOnMap(Bitmap);
      CopyImageToBitmap(GISdb[DBonTable].TheMapOwner.Image1,Bitmap2);
      DrawAndDeleteOverlay(Bitmap2,Bitmap);
      GISdb[DBonTable].TheMapOwner.Image1.Picture.Graphic := Bitmap2;
      Bitmap2.Free;
   //end;
end;

procedure Tdbtablef.Landcoversummary1Click(Sender: TObject);
begin
   LandCoverSummary;
end;

procedure Tdbtablef.LATEXtable1Click(Sender: TObject);
var
   Latex : tStringList;
   TStr : shortstring;
   i : integer;
begin
   Latex := tStringList.Create;

   Latex.Add('\begin{table}[]');
   Latex.Add('\caption{add caption}');
   Latex.Add('\label{tab: add label}');
   TStr := '';
   for i := 1 to GISdb[DBonTable].MyData.FieldCount do
      if GISdb[DBonTable].dbOpts.VisCols[i] then Tstr := Tstr + 'C';

   Latex.Add('\begin{tabularx}{\textwidth}{' + TStr + '}');
   Latex.Add('\toprule');
   TStr := '\textbf{' + GISdb[DBonTable].MyData.GetFieldName(0) + '}';
   for i := 1 to pred(GISdb[DBonTable].MyData.FieldCount) do
      if GISdb[DBonTable].dbOpts.VisCols[i] then begin
         Tstr := Tstr + ' & \textbf{' + RemoveUnderscores(GISdb[DBonTable].MyData.GetFieldName(i)) + '}';
      end;
   Latex.Add(TStr + ' \\');
   Latex.Add('\midrule');
   GISdb[DBonTable].MyData.First;
   while not GISdb[DBonTable].MyData.eof do begin
      TStr := GISdb[DBonTable].MyData.GetFieldByNameAsString(GISdb[DBonTable].MyData.GetFieldName(0));
      for i := 1 to pred(GISdb[DBonTable].MyData.FieldCount) do
         if GISdb[DBonTable].dbOpts.VisCols[i] then
            Tstr := Tstr + ' & ' + GISdb[DBonTable].MyData.GetFieldByNameAsString(GISdb[DBonTable].MyData.GetFieldName(i));
      TStr := TStr + ' \\';
      Latex.Add(TStr);
      GISdb[DBonTable].MyData.Next;
   end;
   Latex.Add('\bottomrule');
   Latex.Add('\end{tabularx}');
   Latex.Add('\end{table}');
   DisplayAndPurgeStringList(Latex,' LATEX for ' + GISdb[DBonTable].dbName);
end;


procedure Tdbtablef.Latlongelevofrecordcorners1Click(Sender: TObject);
var
   Results : tStringList;
   bb : sfBoundBox;

         procedure DoPoint(Lat,Long : float64);
         var
            z : float32;
         begin
            with GISdb[DBonTable] do begin
               if DEMGlb[theMapOwner.MapDraw.DEMonMap].GetElevFromLatLongDegree(Lat,Long,z) then begin
                  Results.Add(RealToString(Long,-12,-8) + ',' + RealToString(Lat,-12,-8) + ',' + RealToString(z,-12,-2));
               end;
            end;
         end;

begin
  // with GISdb[DBonTable] do begin
      if LineOrAreaShapeFile(GISdb[DBonTable].ShapeFileType) then begin
         bb := GISdb[DBonTable].MyData.GetRecordBoundingBox;
         Results := tStringList.Create;
         Results.Add('LONG,LAT,ELEV');
         DoPoint(bb.ymax,bb.Xmin);
         DoPoint(bb.ymax,bb.Xmax);
         DoPoint(bb.ymin,bb.Xmin);
         DoPoint(bb.ymin,bb.Xmax);
      end;
      GISdb[DBonTable].theMapOwner.StringListToLoadedDatabase(Results,NextFileNumber(MDTempdir,'record_bounding_box_','.dbf'));
  // end;
end;

procedure Tdbtablef.Latlongfields1Click(Sender: TObject);
begin
   GISdb[DBonTable].AddLatLong;
end;

procedure Tdbtablef.LATprofile1Click(Sender: TObject);
begin
   Bothprofiles1Click(Sender);
end;


procedure Tdbtablef.LATprofile2Click(Sender: TObject);
begin
   FiveSeriesGraph(DBonTable,-99,-99,-99,GISdb[DBonTable].LatFieldName);
end;

procedure Tdbtablef.Bothprofiles1Click(Sender: TObject);
const
   Tolerance : float64 = 1;
var
   Lat,Long,Bit : float64;
begin
   if GISdb[DBonTable].ValidLatLongFromTable(Lat,Long) then begin
      ReadDefault('Tolerance (arc sec) each side',Tolerance);
      Bit := Tolerance / 3600;
      if (Sender = LatProfile1) or (Sender = BothProfiles1) then begin
         GISdb[DBonTable].MyData.ApplyFilter(GISdb[DBonTable].LongFieldName + '>=' + RealToString(Long-Bit,-18,-7) + ' AND ' + GISdb[DBonTable].LongFieldName + '<=' + RealToString(Long+Bit,-18,-6) );
         FiveSeriesGraph(DBonTable,Lat,Long,Tolerance,GISdb[DBonTable].LatFieldName);
      end;

      if (Sender = LongProfile1) or (Sender = BothProfiles1) then begin
         GISdb[DBonTable].MyData.ApplyFilter(GISdb[DBonTable].LatFieldName + '>=' + RealToString(Lat-Bit,-18,-7) + ' AND ' + GISdb[DBonTable].LatFieldName + '<=' + RealToString(Lat+Bit,-18,-6) );
         FiveSeriesGraph(DBonTable,Lat,Long,Tolerance,GISdb[DBonTable].LongFieldName);
      end;
      GISdb[DBonTable].MyData.ApplyFilter('');
   end;
end;


procedure Tdbtablef.Boxplot1Click(Sender: TObject);
begin
   StartBoxPlot(DBonTable);
end;


procedure Tdbtablef.LONGprofile1Click(Sender: TObject);
begin
   Bothprofiles1Click(Sender);
end;


procedure Tdbtablef.LONGprofile2Click(Sender: TObject);
begin
   FiveSeriesGraph(DBonTable,-99,-99,-99,GISdb[DBonTable].LongFieldName);
end;

procedure Tdbtablef.Latprofiles1Click(Sender: TObject);
begin
   Quickfiltering2Click(Sender);
   FiveSeriesGraph(DBonTable,-99,-99,-99,GISdb[DBonTable].LatFieldName);
end;

procedure Tdbtablef.Latprofiles2Click(Sender: TObject);
begin
   Quickfiltering2Click(Sender);
   FiveSeriesGraph(DBonTable,-99,-99,-99,GISdb[DBonTable].LongFieldName);
end;


procedure Tdbtablef.Lattimecolors1Click(Sender: TObject);
begin
   GISdb[DBonTable].dbOpts.XField := 'LAT';
   GISdb[DBonTable].dbOpts.YField := 'ICESAT_GRD';
   GISdb[DBonTable].dbOpts.StringColorField := 'DATE';
   GISdb[DBonTable].ActuallyDrawGraph(dbgtN2Dgraphcolorcodetext1);
end;

procedure Tdbtablef.Layersymbology1Click(Sender: TObject);
var
   fName : PathStr;
begin
   fName := GISdb[DBonTable].LayerTableFName;
   GISdb[DBonTable].LayerTable.Destroy;
   OpenDBForModalEdit(fName);
   GISdb[DBonTable].LayerTable := tMyData.Create(GISdb[DBonTable].LayerTableFName);
   GISdb[DBonTable].RedrawLayerOnMap;
end;


procedure Tdbtablef.Legend1Click(Sender: TObject);
begin
   Legend2Click(Sender);
end;


procedure Tdbtablef.Legend2Click(Sender: TObject);
begin
   GISdb[DBonTable].CreatePopupLegend;
end;


procedure Tdbtablef.Legendfont1Click(Sender: TObject);
begin
   EditMyFont(MDDef.DefGISLegendFont);
   ReadDefault('Height per category (pixels)',MDDef.LegendSingleHeight);
   ReadDefault('Symbol width (pixels)',MDDef.LegendGraphWidth);
end;

procedure Tdbtablef.Legned1Click(Sender: TObject);
begin
   Legend2Click(Sender);
end;

procedure Tdbtablef.Lengthofeachrecord1Click(Sender: TObject);
begin
  GISdb[DBonTable].AddGeometry(agLength);
end;

procedure Tdbtablef.Label1Click(Sender: TObject);
begin
   {$IfDef ExAdvancedGIS}
   {$Else}
      GISdb[DBonTable].dbOpts.LabelDBPlots := true;
      GISdb[DBonTable].GISProportionalSymbols(GISdb[DBonTable].dbOpts.dbAutoShow);
   {$EndIf}
end;


procedure Tdbtablef.Labeleverynthrecord1Click(Sender: TObject);
var
   n : integer;
   Bitmap : tMyBitmap;
begin
   //with GISdb[DBonTable] do begin
      N := 1000;
      ReadDefault('label interval',n);
      CopyImageToBitmap(GISdb[DBonTable].theMapOwner.Image1,BitMap);
      ShowHourglassCursor;
      GISdb[DBonTable].aShapeFile.LabelNthPointOnMap(GISdb[DBonTable].theMapOwner,Bitmap,N,FilledBox,2,claRed,true);
      GISdb[DBonTable].theMapOwner.Image1.Picture.Graphic := Bitmap;
      Bitmap.Free;
      ShowHourglassCursor;
   //end;
end;

procedure Tdbtablef.Labelfans1Click(Sender: TObject);
begin
   Labelsensors1Click(Sender);
end;

procedure Tdbtablef.Gradient1Click(Sender: TObject);
var
   Last4Grad,This4Grad,Lat,Long,LastLat,LastLong,Az,Dist : float64;
   f1,f2,TStr : ShortString;
begin
   {$IfDef RecordNavigation} WriteLineRoDebugFile('Tdbtablef.Gradient1Click in'); {$EndIf}
   //with GISdb[DBonTable] do begin
      if (Sender = Gradient1) then begin
         f1 := GISdb[DBonTable].PickField('Field for gradient',NumericFieldTypes);
         f2 := f1 + 'GRAD';
         GetString('new gradient field',f2,true,DBaseFieldNameChars);
         GISdb[DBonTable].AddFieldToDataBase(ftFloat,f2,12,4);
         {$IfDef RecordNavigation} WriteLineRoDebugFile('(Sender = Gradient1), f1=' + f1 + '   and f2=' + f2); {$EndIf}

         GISdb[DBonTable].EmpSource.Enabled := false;
         GISdb[DBonTable].MyData.First;
         GISdb[DBonTable].ValidLatLongFromTable(LastLat,LastLong);
         RedefineWGS84DatumConstants(LastLong);
         TStr := GISdb[DBonTable].MyData.GetFieldByNameAsString(f1);
         Last4grad := StrToFloat(TStr);
         ShowHourglassCursor;
         GISdb[DBonTable].MyData.Next;
         while not GISdb[DBonTable].MyData.eof do begin
            if GISdb[DBonTable].ValidLatLongFromTable(Lat,Long) then begin
               VincentyCalculateDistanceBearing(LastLat,LastLong,Lat,Long,Dist,Az);
               GISdb[DBonTable].MyData.Edit;
               if (Abs(Dist) > 0.0001) and (abs(Az) < 360) then begin
                  This4grad := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(f1);
                  GISdb[DBonTable].MyData.SetFieldByNameAsFloat(f2,(This4Grad - Last4Grad) / Dist);
                  Last4Grad := This4grad;
               end;
               LastLat := Lat;
               LastLong := Long;
            end;
            GISdb[DBonTable].MyData.Next;
         end;
         ShowStatus;
      end;
   //end;
end;

procedure Tdbtablef.Graphbyareawithaveragescoreforselectedcriteria1Click(Sender: TObject);
begin
   MessageToContinue('Disabled; need to add DEM stringlist');
   //DEMIX_AreaAverageScores_graph(DBonTable);
end;

procedure Tdbtablef.Graphbytilewithaveragescoreforselectedcriteria1Click(Sender: TObject);
begin
   MessageToContinue('Disabled; need to add DEM stringlist');
   //GraphAverageScoresByTile(DBonTable,Nil,Nil);
end;

procedure Tdbtablef.Graphfilters1Click(Sender: TObject);
begin
   DoDEMIXFilter(DBonTable);
end;



procedure Tdbtablef.Graphicallymovepoints1Click(Sender: TObject);
var
   Lat,Long : float64;
   x,y : integer;
begin
   with GISdb[DBonTable],MyData do begin
      if ItsAPointDB then begin
         PreEditFilter := GISdb[DBonTable].MyData.Filter;
         if (GetFieldByNameAsString(LatFieldName) = '') or (GetFieldByNameAsString(LongFieldName) = '') then begin
            Lat := 0.5 * (TheMapOwner.MapDraw.MapCorners.BoundBoxGeo.ymax + TheMapOwner.MapDraw.MapCorners.BoundBoxGeo.ymin);
            Long := 0.5 * (TheMapOwner.MapDraw.MapCorners.BoundBoxGeo.xmax + TheMapOwner.MapDraw.MapCorners.BoundBoxGeo.xmin);
            TheMapOwner.MapDraw.LatLongDegreeToScreen(Lat,Long,x,y);
            ScreenSymbol(TheMapOwner.Image1.Canvas,x,y,dbOpts.Symbol);
            Edit;
            ChangeLatLongLocation(Lat,Long);
            Post;
         end;
         ChangeDEMNowDoing(MovePointDBRecs);
      end
      else ChangeDEMNowDoing(PickDBRecsToMove);
      DBEditting := DBonTable;
   end;
end;


procedure Tdbtablef.Graphicalpickandeditdatabase1Click(Sender: TObject);
begin
   ChangeDEMNowDoing(EditDBRecs);
   DBEditting := DBonTable;
end;


procedure Tdbtablef.Graphofcriteriaforareaortile1Click(Sender: TObject);
begin
   LinkedGraphofCriteriaEvaluations(DBonTable,GISdb[dbOnTable].MyData.Filter,false);
end;


procedure Tdbtablef.GraphofPrimaryDataFractionbyClusters1Click(Sender: TObject);
begin
   MultipleScatterPlotsForCluster(dbOnTable);
end;

procedure Tdbtablef.GraphsbestDEMpertilebycriteriasortbytilecharacteristics1Click(Sender: TObject);
begin
   {$IfDef ExDEMIXexperimentalOptions}
   {$Else}
      DEMIX_graph_best_in_Tile(DBonTable,false);
   {$EndIf}
end;

procedure Tdbtablef.GraphSSIMFUVbycluster1Click(Sender: TObject);
begin
   {$IfDef ExDEMIXexperimentalOptions}
   {$Else}
      DEMIX_SSIM_FUV_clusters_graph(DBonTable);
   {$EndIf}
end;


procedure Tdbtablef.GraphSSIMFUVbyclustermeans1Click(Sender: TObject);
begin
   DEMIX_SSIM_FUV_clusters_graph(DBonTable);
end;

procedure Tdbtablef.TransposeSSIMFUVforclusters1Click(Sender: TObject);
begin
    //DEMIX_SSIM_FUV_transpose_kmeans_new_db(DBonTable);
end;

procedure Tdbtablef.TransposeSSIMR2forclusters1Click(Sender: TObject);
begin
    DEMIX_SSIM_FUV_transpose_kmeans_new_db(DBonTable);
end;

procedure Tdbtablef.GraphwinningpercentagebyDEM1Click(Sender: TObject);
begin
   MessageToContinue('Disabled; need to add DEM stringlist');
   //DEMWinningPercentageGraph(DBonTable,'');
end;

procedure Tdbtablef.Graphwithranges1Click(Sender: TObject);
var
   ThisGraph : TThisbasegraph;
   aMin,aMax : float64;
   TStr : shortstring;
begin
   ThisGraph := TThisbasegraph.Create(Application);
   GISdb[DBonTable].FieldRange('MIN',aMin,aMax);
   ThisGraph.GraphDraw.MinHorizAxis := aMin;
   GISdb[DBonTable].FieldRange('MAX',aMin,aMax);
   ThisGraph.GraphDraw.MaxHorizAxis := aMax;
   GISdb[DBonTable].FieldRange('CLASS',aMin,aMax);
   ThisGraph.GraphDraw.MaxVertAxis := aMax + 1;
   ThisGraph.GraphDraw.MinVertAxis := 0;
   ThisGraph.GraphDraw.HorizLabel := GISdb[DBonTable].DBName;
   ThisGraph.GraphDraw.VertLabel := 'Class';
   ThisGraph.SetUpGraphForm;
   ThisGraph.RangeGraphName := GISdb[DBonTable].dbFullName;
   ThisGraph.RedrawDiagram11Click(Sender);
   if MDDef.QuantileRanges then TStr := 'quantiles' else TStr := '1 std dev';
   ThisGraph.Caption := 'Class Ranges (' + TStr + ')';
end;

procedure Tdbtablef.Gridcumulativedistributions1Click(Sender: TObject);
begin
   Gridstatistics1Click(Sender);
end;

procedure Tdbtablef.Gridfont1Click(Sender: TObject);
begin
   EditMyFont(MDDef.DBtableGridFont);
   SetFonts;
end;

procedure Tdbtablef.Gridmaps1Click(Sender: TObject);
{$IfDef ExGeoStats}
begin
{$Else}
var
   aClass : shortstring;
begin
   with GISdb[DBonTable] do begin
      aClass := GetSingleEntryFromTableField('grid maps','NAME');
      Sup_Class_Aux_Grids.ClassWithAuxGrids(theMapOwner,DBonTable,aClass);
   end;
{$EndIf}
end;


procedure Tdbtablef.Gridstatistics1Click(Sender: TObject);
{$IfDef ExAdvancedSats}
begin
{$Else}
var
   i : integer;
   DEMsWanted : tDEMbooleanArray;
   StatOpts : MultiGrid.tStatOpts;
begin
   GetMultipleDEMsFromList('Grid for class statistics',DEMsWanted);
   if (Sender = Gridstatistics1) then StatOpts := soGraph
   else if (Sender = Gridcumulativedistributions1) then StatOpts := soCumDist;
   for i := 1 to MaxDEMDataSets do if DEMsWanted[i] then ClassStatsForGrid(StatOpts,i,DBonTable);
   ShowStatus;
{$EndIf}
end;

procedure Tdbtablef.Gridtitlefont1Click(Sender: TObject);
begin
   EditMyFont(MDDef.DBtableTitleFont);
   SetFonts;
end;

procedure Tdbtablef.Grpahicallyinsertpoint1Click(Sender: TObject);
begin
   ChangeDEMNowDoing(InsertDBPoint);
   DBEditting := DBonTable;
end;

procedure Tdbtablef.Copycolumntoclipboard1Click(Sender: TObject);
var
   sl : tstringlist;
begin
   GISdb[DBonTable].EmpSource.Enabled := false;
   sl := GISdb[DBonTable].MyData.ListUniqueEntriesInDB(SelectedColumn);
   Clipboard.AsText := sl.Text;
   sl.Destroy;
   GISdb[DBonTable].EmpSource.Enabled := true;
end;


procedure Tdbtablef.CopyCoordinatesClick(Sender: TObject);
var
   NParts : integer;
   zs : ^tdElevs;
   PartSize : tPartSize;
begin
   if LineOrAreaShapeFile(GISdb[DBonTable].ShapeFileType) then begin
      if (ClipBoard_Line_Coords = Nil) then new(ClipBoard_Line_Coords);
      GISdb[DBonTable].aShapeFile.OldGetLineCoordsWithZs(GISdb[DBonTable].MyData.RecNo,ClipBoard_NumPts,NParts,ClipBoard_Line_Coords^,PartSize,GISdb[DBonTable].ShapeFileType in [13,15,23,25],zs^);
   end
   else begin
      ClipBoard_Coords := GISdb[DBonTable].ValidLatLongFromTable(Clipboard_Lat,ClipBoard_Long);
      ClipBrd.ClipBoard.AsText := RealToString(Clipboard_Lat,-14,-8) + ',' +  RealToString(Clipboard_Long,-14,-8);
   end;
   if GISdb[DBonTable].MyData.FieldExists('NAME') then
     ClipBoard_Name := GISdb[DBonTable].MyData.GetFieldByNameAsString('NAME')
   else ClipBoard_Name := '';
end;


procedure Tdbtablef.Copydatafiles2Click(Sender: TObject);
var
   DestPath : PathStr;
   fName : ShortString;
begin
   with GISdb[DBonTable] do begin
      DestPath := ProgramRootDir;
      GetDosPath('File destination',DestPath);
      ShowHourglassCursor;
      if FileNameFieldExists or ImagePresent then begin
         GISdb[DBonTable].MyData.First;
         while not GISdb[DBonTable].MyData.EOF do begin
            if FileNameFieldExists and GISdb[DBonTable].MyData.GetFullFileName(fName) then CopyFile(fName,DestPath + ExtractFileName(fName));
            if ImagePresent and GetFullImageName(fName) then begin
               CopyFile(fName,DestPath + ExtractFileName(fName));
            end;
            GISdb[DBonTable].MyData.Next;
         end;
      end;
      ShowHourglassCursor;
   end;
end;

procedure Tdbtablef.Copyfieldtostringfield1Click(Sender: TObject);
var
   WantedFieldName,NewField : ShortString;
   i,rc : Integer;
begin
   with GISdb[DBonTable],MyData do begin
     WantedFieldName := PickField('Field fo copy',[ftString,ftInteger,ftSmallInt,ftFloat]);
     if GISdb[DBonTable].MyData.FieldExists(WantedFieldName) then begin
       NewField := WantedFieldName + '2';
       i := GISdb[DBonTable].MyData.GetFieldLength(WantedFieldName);
       NewField := AddNewField(GISdb[DBonTable], NewField,ftString,i);
       EmpSource.Enabled := false;
       First;
       i := 0;
       rc := GISdb[DBonTable].MyData.RecordCount;
       StartProgress('Copy field');
       while not EOF do begin
          inc(i);
          if (i mod 1000 = 0) then UpDateProgressBar(i/rc);
          Edit;
          SetFieldByNameAsString(NewField,ptTrim(GetFieldByNameAsString(WantedFieldName)));
          Next;
       end;
       ShowStatus;
     end;
   end;
end;

procedure Tdbtablef.Copynumericfield1Click(Sender: TObject);
var
  NewName,SecondFieldName : shortstring;
  ft : tFieldType;
  fl,Decs : integer;
begin
   with GISdb[DBonTable],MyData do begin
      SecondFieldName := PickField('field for sum',NumericFieldTypes);
      ft := GetFieldType(SecondFieldName);
      fl := GISdb[DBonTable].MyData.GetFieldLength(SecondFieldName);
      if (ft = ftFloat) then begin
         inc(fl,6);
         Decs := 6;
      end
      else Decs := 0;
      NewName := SecondFieldName + '2';

      NewName := GetFieldNameForDB('New Field',True,NewName);
      AddFieldToDataBase(ft,NewName,fL,Decs);
      EmpSource.Enabled := false;
      GISdb[DBonTable].MyData.First;
      while not GISdb[DBonTable].MyData.eof do begin
         GISdb[DBonTable].MyData.Edit;
         GISdb[DBonTable].MyData.SetFieldByNameAsString(NewName,GISdb[DBonTable].MyData.GetFieldByNameAsString(SecondFieldName));
         GISdb[DBonTable].MyData.Next;
      end;
      ShowStatus;
   end;
end;

procedure Tdbtablef.Copyshapefile1Click(Sender: TObject);
const
   NewName : PathStr = '';
begin
   with GISdb[DBonTable] do begin
      if NewName = '' then NewName := MainMapData + ExtractFileName(DBFullName)
      else NewName := ExtractFilePath(NewName) + ExtractFileName(DBFullName);
      if GetFileNameDefaultExt('copied shapefile','shapefile|*.shp',NewName,false) then  CopyShapeFile(DBFullName,NewName);
   end;
end;

procedure Tdbtablef.Cosoffield1Click(Sender: TObject);
begin
   Sumtwofields1Click(Sender);
end;

procedure Tdbtablef.Editlineweightandcolor1Click(Sender: TObject);
var
   SymSize : integer;
   SymColor : tPlatformColor;
begin
   with GISdb[DBonTable] do begin
      GISdb[DBonTable].MyData.GetLineColorAndWidth(SymColor,SymSize);
      PickLineSizeAndColor('db record',Nil,SymColor,SymSize);
      GISdb[DBonTable].MyData.Edit;
      GISdb[DBonTable].MyData.SetLineColorAndWidth(SymColor,SymSize);
      GISdb[DBonTable].MyData.Post;
   end;
end;


procedure Tdbtablef.Editfilename1Click(Sender: TObject);
var
   fName : ShortString;
   fName2 : PathStr;
begin
   if GISdb[DBonTable].MyData.GetFullFileName(fName) then begin
       fName2 := fName;
       if Petmar.GetFileFromDirectory('Replace','*.*',fName2) then begin
          GISdb[DBonTable].MyData.Edit;
          GISdb[DBonTable].MyData.SetFieldByNameAsString('FILENAME',fname2);
          GISdb[DBonTable].MyData.Post;
       end;
   end;
end;


procedure Tdbtablef.Editfillpattern1Click(Sender: TObject);

   procedure UpdateThisRecord;
   begin
      with GISdb[DBonTable] do begin
          GISdb[DBonTable].MyData.Edit;
          GISdb[DBonTable].MyData.SetLineColorAndWidth(dbOpts.FillColor,dbOpts.LineWidth);
          GISdb[DBonTable].MyData.SetFieldByNameAsInteger('FILL_PAT',ord(dbOpts.AreaSymbolFill));
          GISdb[DBonTable].MyData.SetFieldByNameAsInteger('FILL_COLOR',ConvertPlatformColorToTColor(dbOpts.FillColor));
          GISdb[DBonTable].MyData.Post;
      end;
   end;


begin
   with GISdb[DBonTable] do begin
       SetColorsFromDB(MyData);
       PickPattern('Area fill',dbOpts.AreaSymbolFill,dbOpts.FillColor,dbOpts.LineColor,dbOpts.LineWidth);
       if (Sender = Editfillpattern1) then UpdateThisRecord
       else if (Sender=Fillpatternallrecords1) or (MyData.GetFieldByNameAsString('FILL_PAT') = '') then begin
          GISdb[DBonTable].MyData.First;
          while not GISdb[DBonTable].MyData.eof do begin
             UpdateThisRecord;
             GISdb[DBonTable].MyData.Next;
          end;
       end;
   end;
end;


procedure Tdbtablef.Editfont1Click(Sender: TObject);
begin
   EditPointSymbol1Click(Sender);
end;

procedure Tdbtablef.Editgazetteersymbology1Click(Sender: TObject);
begin
   OpenDBForModalEdit(GazOptFName);
end;

procedure Tdbtablef.EditIcon1Click(Sender: TObject);
var
   fname : PathStr;
begin
   with GISdb[DBonTable] do begin
   fName :=  MainMapData + 'Icons\';
   if GetGraphicsFileName('',fName) then begin
         GISdb[DBonTable].MyData.Edit;
         GISdb[DBonTable].MyData.SetFieldByNameAsString(dbOpts.IconField,ExtractFileName(fName));
         GISdb[DBonTable].MyData.Post;
      end;
   end;
end;

procedure Tdbtablef.Coloralllinesegments1Click(Sender: TObject);
begin
   {$IfDef RecordEditDB} WriteLineRoDebugFile('Tdbtablef.Coloralllinesegments1Click in'); {$EndIf}
   Insertlinecolorwidthfields1Click(Sender);
   with GISdb[DBonTable] do begin
      PickLineSizeAndColor('db record',Nil,dbOpts.LineColor,dbOpts.LineWidth);
      EmpSource.Enabled := false;
      GISdb[DBonTable].MyData.First;
      repeat
         GISdb[DBonTable].MyData.Edit;
         if ((Sender <> Coloralluncoloredlinesegments1) or (MyData.GetFieldByNameAsString('LINE_COLOR') = '')) then begin
            GISdb[DBonTable].MyData.SetLineColorAndWidth(dbOpts.LineColor,dbOpts.LineWidth);
         end;
         GISdb[DBonTable].MyData.Next;
      until GISdb[DBonTable].MyData.EOF;
     ShowStatus;
   end;
   {$IfDef RecordEditDB} WriteLineRoDebugFile('Tdbtablef.Coloralllinesegments1Click out'); {$EndIf}
end;


procedure Tdbtablef.RewriteinsubdirOGR1Click(Sender: TObject);
begin
   OGRrewriteShapefile(GISdb[DBonTable].dbFullName)
end;


procedure Tdbtablef.Ridgecountaroundpoint1Click(Sender: TObject);
var
   Lat,Long : float64;
   Results : tStringList;
   xgrid,ygrid,SearchRadius,i,Col,Row,Found : integer;
   LastFound : boolean;

            procedure CheckIt(x,y : integer);
            begin
               with GISdb[DBonTable],MyData do begin
                  if DEMGlb[TheMapOwner.MapDraw.DEMonMap].MissingDataInGrid(x,y) then LastFound := false
                  else begin
                     if not LastFound then begin
                        inc(Found);
                        LastFound := true;
                     end;
                  end;
               end;
            end;


begin
   with GISdb[DBonTable] do begin
      if ValidLatLongFromTable(Lat,Long) then begin
         DEMGlb[TheMapOwner.MapDraw.DEMonMap].LatLongDegreeToDEMGridInteger(Lat,Long,xgrid,ygrid);
         SearchRadius := 10;
         Results := tStringList.Create;
         Results.Add('Point: ' + LatLongDegreeToString(Lat,Long,MDDef.OutPutLatLongMethod));
         Results.Add('   DEM grid: ' + IntToStr(Xgrid) + 'x' + IntToStr(YGrid));
         Results.Add('');
         Results.Add('Radius   Ridges');
         for i := 1 to SearchRadius do begin
            Found := 0;
            for Col := (xgrid - i) to (xgrid + i) do begin
               if not DEMGlb[TheMapOwner.MapDraw.DEMonMap].MissingDataInGrid(Col,yGrid-i) then inc(Found);
               if not DEMGlb[TheMapOwner.MapDraw.DEMonMap].MissingDataInGrid(Col,yGrid+i) then inc(Found);
            end;
            for Row := succ(ygrid - i) to pred(ygrid + i) do begin
               if not DEMGlb[TheMapOwner.MapDraw.DEMonMap].MissingDataInGrid(xGrid-i,Row) then inc(Found);
               if not DEMGlb[TheMapOwner.MapDraw.DEMonMap].MissingDataInGrid(xgrid+i,Row) then inc(Found);
            end;
            Results.Add(IntegerToString(i,4) + IntegerToString(Found,6));
         end;
         Results.Add('');

         for i := 1 to SearchRadius do begin
            Found := 0;
            LastFound := false;
            for Col := (xgrid - i) to (xgrid + i) do CheckIt(Col,ygrid-i);
            for Row := succ(ygrid - i) to pred(ygrid + i) do CheckIt(xgrid+i,Row);
            for Col := (xgrid + i) downto (xgrid - i)  do CheckIt(Col,ygrid+i);
            for Row := pred(ygrid + i) downto succ(ygrid - i) do CheckIt(xgrid-i,Row);

            if (not DEMGlb[TheMapOwner.MapDraw.DEMonMap].MissingDataInGrid(xgrid-i,ygrid-i)) and
               (not DEMGlb[TheMapOwner.MapDraw.DEMonMap].MissingDataInGrid(xgrid-i,succ(ygrid-i))) then dec(Found);
            Results.Add(IntegerToString(i,4) + IntegerToString(Found,6));
         end;
         Petmar.DisplayAndPurgeStringList(Results,'Ridges');
      end;
   end;
end;


procedure Tdbtablef.Coloralluncoloredlinesegments1Click(Sender: TObject);
begin
   Coloralllinesegments1Click(Sender);
end;


procedure Tdbtablef.Colorarray1Click(Sender: TObject);
var
   Color : integer;
   r,g,b : byte;
   rs,gs,bs : AnsiString;
   Findings : tStringList;
begin
    rs := '[';
    gs := '[';
    bs := '[';
    GISdb[DBonTable].MyData.First;
     while not GISdb[DBonTable].MyData.Eof do begin
        Color := GISdb[DBonTable].MyData.TColorFromTable;
        Petmar.GetRGBfromTColor(Color,r,g,b);
        rs := rs + IntToStr(r) + ',';
        gs := gs + IntToStr(g) + ',';
        bs := bs + IntToStr(b) + ',';
        GISdb[DBonTable].MyData.Next;
     end;
     Delete(rs,length(rs),1);
     Delete(gs,length(gs),1);
     Delete(bs,length(bs),1);
     Findings := tStringList.Create;
     Findings.Add('_red = ' + rs + ']');
     Findings.Add('_green = ' + gs + ']');
     Findings.Add('_blue = ' + bs + ']');
     Petmar.DisplayAndPurgeStringList(Findings,'Palette');
end;


procedure Tdbtablef.Deleteunusedfields1Click(Sender: TObject);
var
   j : integer;
   fName : ANSIstring;
begin
   if ValidDB(DBonTable) then begin
      GISdb[DBonTable].EmpSource.Enabled := false;
      for j := pred(GISdb[DBonTable].MyData.FieldCount) downto 0 do begin
         fName := GISdb[DBonTable].MyData.GetFieldName(j);
         if GISdb[DBonTable].MyData.FieldAllBlanks(fName) or GISdb[DBonTable].MyData.FieldAllZeros(fName) then begin
            GISdb[DBonTable].MyData.DeleteField(fName);
         end;
      end;
   end;
   ShowStatus;
end;

procedure Tdbtablef.KML1Click(Sender: TObject);
begin
   GISdb[DBonTable].ExportToKML(true,true);
end;

procedure Tdbtablef.KMLexportfilteredbyvalues1Click(Sender: TObject);
begin
   GISdb[DBonTable].ExportToKML(false,false,SelectedColumn);
end;

procedure Tdbtablef.DeliberateCSVtxt1Click(Sender: TObject);
begin
   Text1Click(Sender);
end;

procedure Tdbtablef.DeliberateKMLGoogleEarthexport1Click(Sender: TObject);
begin
   GISdb[DBonTable].ExportToKML(false,false);
end;


procedure Tdbtablef.Koppenclassification1Click(Sender: TObject);
begin
   {$IfDef ExGeography}
   {$Else}
       GISdb[DBonTable].StartClimateDisplay(dbasKoppen);
   {$EndIf}
end;

procedure Tdbtablef.Koppenicons1Click(Sender: TObject);
{$IfDef ExGeography}
begin
{$Else}
var
   Color : tPlatformColor;
   Bitmap : tMyBitmap;
   fName,aClass : PathStr;
begin
   with GISdb[DBonTable] do begin
     AddFieldToDataBase(ftString,'ICON',24);
     ColorPresent := true;
     EmpSource.Enabled := false;
     GISdb[DBonTable].MyData.First;
     while not GISdb[DBonTable].MyData.EOF do begin
        aClass := GISdb[DBonTable].MyData.GetFieldByNameAsString('CLASS');
        if GetKoppenColor(aClass,Color) then begin
           aClass := aclass + '.png';
           fName := MainMapData + 'icons\' + aClass;
           if not FileExists(fName) then begin
              CreateBitmap(Bitmap,12,12);
              Bitmap.Canvas.Pen.Color := ConvertPlatformColorToTColor(Color);
              Bitmap.Canvas.Brush.Color := ConvertPlatformColorToTColor(Color);
              Bitmap.Canvas.Brush.Style := bsSolid;
              Bitmap.Canvas.Rectangle(0,0,pred(Bitmap.Width),pred(Bitmap.Height));
              Bitmap.SaveToFile(fName);
              Bitmap.Free;
           end;
           GISdb[DBonTable].MyData.Edit;
           GISdb[DBonTable].MyData.SetFieldByNameAsString('ICON',aClass);
        end;
        Next;
     end;
     ShowStatus;
   end;
{$EndIf}
end;


procedure Tdbtablef.Koppenlatitudestats1Click(Sender: TObject);
{$IfDef ExGeography}
begin
{$Else}
var
   Lat,LatInc : integer;
   A,BS,BW,C,D,E :  integer;
   KoppenClass : ShortString;
   Results : tStringList;
begin
   LatInc := 2;
   with GISdb[DBonTable],MyData do begin
      Lat := 88;
      ClearGISFilter;
      Results := tStringList.Create;
      Results.Add('Lat       A      BS      BW       C       D       E');
      EmpSource.Enabled := false;
      while Lat >= -88 do begin
         ApplyFilter('LAT <' + IntToStr(Lat+2) + ' AND LAT > ' + IntToStr(Lat-2));
         {$IfDef RecordKoppen} WriteLineRoDebugFile(MyData.Filter,true); {$EndIf}
         A := 0;
         BS := 0;
         BW := 0;
         C := 0;
         D := 0;
         E := 0;
         while not EOF do begin
            KoppenClass := GISdb[DBonTable].MyData.GetFieldByNameAsString('KOPPEN');
            if copy(KoppenClass,1,1) = 'A' then inc(A);
            if copy(KoppenClass,1,2) = 'BS' then inc(BS);
            if copy(KoppenClass,1,2) = 'BW' then inc(BW);
            if copy(KoppenClass,1,1) = 'C' then inc(C);
            if copy(KoppenClass,1,1) = 'D' then inc(D);
            if copy(KoppenClass,1,1) = 'E' then inc(E);
            Next;
         end;
         Results.Add(IntegerToString(Lat,3) + IntegerToString(A,8) + IntegerToString(BS,8)+ IntegerToString(BW,8)+ IntegerToString(C,8)+ IntegerToString(D,8) + IntegerToString(E,8));
         {$IfDef RecordKoppen} WriteLineRoDebugFile(IntegerToString(Lat,3) + IntegerToString(A,8) + IntegerToString(BS,8)+ IntegerToString(BW,8)+ IntegerToString(C,8)+ IntegerToString(D,8) + IntegerToString(E,8)); {$EndIf}
         dec(Lat,2*LatInc);
      end;
      Petmar.DisplayAndPurgeStringList(Results,'Koppen stats');
      ClearGISFilter;
      ShowStatus;
   end;
{$EndIf}
end;


procedure Tdbtablef.DEMIX1Click(Sender: TObject);
begin
   GetDEMIXpaths(false,DBonTable);
   DEMIXPopUpMenu1.PopUp(Mouse.CursorPos.X,Mouse.CursorPos.Y);
end;

procedure Tdbtablef.DEMIXtileinvertory1Click(Sender: TObject);
begin
   DEMIXtile_inventory(DBonTable);
end;


procedure Tdbtablef.DEMIXtilesummary1Click(Sender: TObject);
begin
   DEMIXTileSummary(DBonTable);
end;

procedure Tdbtablef.DEMTerrainblowups1Click(Sender: TObject);
var
   i,xpic,ypic : integer;
   Lat,Long : float64;
begin
   if GISdb[DBonTable].ValidLatLongFromTable(Lat,Long) then begin
      for i := 1 to MaxDEMDataSets do if ValidDEM(i) then begin
         if DEMGlb[i].SelectionMap.MapDraw.LatLongOnScreen(Lat,Long) then begin
            DEMGlb[i].SelectionMap.MapDraw.LatLongDegreeToScreen(Lat,Long,xpic,ypic);
            DEMGlb[i].SelectionMap.LastX := xpic;
            DEMGlb[i].SelectionMap.LastY := Ypic;
            DEMGlb[i].SelectionMap.Terrainblowup1Click(Nil);
         end;
      end;
   end;
end;


function Tdbtablef.DrawDEMTerrainprofile(Sender: TObject) : tThisBaseGraph;
var
  i,j,rc : integer;
  rFile : file;
  MaxDist,Lat,Long,Lat2,Long2,Distance,Bearing,CumDist,MaxDensity : float64;
  v : tFloatPoint;
  v3 : array[1..3] of float64;
  u1,u2 : ShortString;
  PointDensity : array[0..100,0..100] of Integer;

      procedure ProcessFile;
      var
         i,xp,yp : integer;
         MinZ,MaxZ : float64;
         y,z : Array[0..10000] of float64;
         yn,zn : Array[0..100] of float64;
      begin
          with GISdb[DBonTable],MyData do begin
             aShapeFile.ShapeFileDEM := TheMapOwner.MapDraw.DEMonMap;
             MaxDist := 0.001 * aShapeFile.LineLength(MyData.RecNo,True);
             if aShapeFile.CurrentPolyLineHeader.NumPoints < 2 then exit;
             Result.OpenDataFile(rfile);
             MinZ := aShapeFile.CurrentLineZRange[1];
             MaxZ := aShapeFile.CurrentLineZRange[2];
               for i := 0 to pred(aShapeFile.CurrentPolyLineHeader.NumPoints) do begin
                  Lat := aShapeFile.CurrentLineCoords^[i].Lat;
                  Long := aShapeFile.CurrentLineCoords^[i].Long;
                  if (i=0) then CumDist := 0
                  else begin
                     VincentyCalculateDistanceBearing(Lat,Long,Lat2,Long2,Distance,Bearing);
                     Distance := 0.001 * Distance;
                     if (MDDef.EnglishDistanceUnits = disEnglish) then Distance := Distance * 0.621371192;
                     CumDist := CumDist + Distance;
                  end;

                  y[i] := CumDist;
                  z[i] := aShapeFile.CurrentLineZs^[i];
                  if (MDDef.EnglishDistanceUnits = disEnglish) then z[i] := z[i] / FeetToMeters;

                  if (Sender = Normalizedbasinprofile1) or (Sender = Allnormalizedprofiles1) or (Sender = AllProfiles1) then begin
                      y[i] := CumDist / MaxDist;
                      if abs(MaxZ-MinZ) < 0.01 then z[i] := 0
                      else z[i] := (z[i] - MinZ) / (MaxZ - MinZ);
                  end;
                  Lat2 := Lat;
                  Long2 := Long;
               end {for i};

               (*
               if MDDef.SmoothThalwegs then begin
                   if z[0] < z[pred(aShapeFile.CurrentPolyLineHeader.NumPoints)] then begin
                      LastZ := z[pred(aShapeFile.CurrentPolyLineHeader.NumPoints)];
                      for i := pred(aShapeFile.CurrentPolyLineHeader.NumPoints) downto 0 do begin
                         if (I < pred(aShapeFile.CurrentPolyLineHeader.NumPoints)) and (i > 0) then begin
                            if z[i] > LastZ then z[i] := MaxInt
                            else LastZ := z[i];;
                         end;
                      end
                   end
                   else begin
                      LastZ := z[0];
                      for i := 0 to pred(aShapeFile.CurrentPolyLineHeader.NumPoints)  do begin
                         if (I < pred(aShapeFile.CurrentPolyLineHeader.NumPoints)) and (i > 0) then begin
                            if z[i] > LastZ then z[i] := MaxInt
                            else LastZ := z[i];
                         end;
                      end;
                   end;
               end;
               *)

               for i := 0 to pred(aShapeFile.CurrentPolyLineHeader.NumPoints) do begin
                  v[1] := y[i];
                  v[2] := z[i];
                  if z[i] < MaxInt then Result.AddPointToDataBuffer(rfile,v);
               end;

               if (Sender = Normalizedbasinprofile1) or (Sender = Allnormalizedprofiles1) then begin
                    for i := 1 to Result.PointsInDataBuffer do begin
                       y[pred(i)] := Result.PointDataBuffer^[i][1];
                       z[pred(i)] := Result.PointDataBuffer^[i][2];
                    end;
                    for i := 0 to 100 do yn[i] := i * 0.01;
                    InterpolateProfile(Result.PointsInDataBuffer,y,z,101,yn,zn);

                   if (Sender = Allnormalizedprofiles1) then begin
                        zn[100] := z[pred(Result.PointsInDataBuffer)];
                        for i := 0 to 100 do begin
                           xp := round(100*yn[i]);
                           yp := round(100*zn[i]);
                           if (xp < 0) then xp := 0;
                           if (xp > 100) then xp := 100;
                           if (yp < 0) then yp := 0;
                           if (yp > 100) then yp := 100;
                           inc(PointDensity[xp,yp]);
                        end;
                      Result.ClearDataOnGraph;
                      Dispose(Result.PointDataBuffer);
                      Result.PointsInDataBuffer := 0;
                   end;
               end;
              if (Sender <> Allnormalizedprofiles1) then Result.ClosePointDataFile(rfile);
          end {with};
      end;


begin
   with GISdb[DBonTable] do begin
        if (Sender = AllProfiles1) and (MyData.RecordCount > 35) then begin
           if Not AnswerIsYes('Proceed with ' + IntToStr(MyData.RecordCount) + ' records') then exit;
        end;

        Result := tThisBaseGraph.Create(Application);
        u1 := '';
        if (Sender <> Allnormalizedprofiles1) and (dbOpts.LabelField <> '') then u1 := ' ' + GISdb[DBonTable].MyData.GetFieldByNameAsString(dbOpts.LabelField);

        Result.Caption := 'Terrain profile' + u1;

        if (Sender = Allnormalizedprofiles1) or (Sender = AllProfiles1) then begin
           if GISdb[DBonTable].MyData.Filtered then u1 := '  ' + GISdb[DBonTable].MyData.Filter;
           Result.Caption := 'Normalized profiles ' + dbName + u1;
        end;

        if (Sender = Normalizedbasinprofile1) or (Sender = Allnormalizedprofiles1)  or (Sender = AllProfiles1) then begin
          u1 := 'normalized';
          u2 := u1;
        end
        else begin
          if (MDDef.EnglishDistanceUnits = disEnglish) then begin
             u1 := 'miles';
             u2 := 'feet';
          end
          else begin
             u1 := 'km';
             u2 := 'm';
          end;
     end;

     Result.GraphDraw.HorizLabel := 'Distance (' + u1 + ')';
     Result.GraphDraw.VertLabel := 'Elevation (' + u2 + ')';
     Result.GraphDraw.MaxHorizAxis := 1;
     Result.GraphDraw.MaxVertAxis := 1;
     Result.SetUpGraphForm;

     if (Sender = AllProfiles1) or (Sender = Allnormalizedprofiles1) then begin
        if (Sender = Allnormalizedprofiles1) then begin
           for i := 0 to 100 do
              for j := 0 to 100 do
                 PointDensity[i,j] := 0;
           Result.RedrawDiagram11Click(Nil);
        end;

        EmpSource.Enabled := false;
        GISdb[DBonTable].MyData.First;
        StartProgressAbortOption('Profiles');

        i := 0;
        rc := GISdb[DBonTable].MyData.RecordCount;
        while not GISdb[DBonTable].MyData.eof do  begin
           if (i mod 100 = 0) then UpdateProgressBar(i/rc);
           inc(i);
           ProcessFile;
           GISdb[DBonTable].MyData.Next;
           if WantOut then break;
        end;
        EndProgress;

        if (Sender = AllProfiles1) then Result.RedrawDiagram11Click(Nil);

        if (Sender = Allnormalizedprofiles1) then begin
            Result.OpenXYZFile(rfile);
            Result.MinZ := 0;
            Result.MaxZ := 0;
            rc := GISdb[DBonTable].MyData.RecordCount;
            StartProgress('Contour');
            for i := 0 to 100 do begin
               if (i mod 5 = 0) then UpdateProgressBar(i/100);
               for j := 0 to 100 do
                  if (PointDensity[i,j] > 0) then begin
                     v3[1] := 0.01 * i;
                     v3[2] := 0.01 * j;
                     v3[3] := 100 * PointDensity[i,j] / rc;
                     if v3[3] > Result.MaxZ then Result.MaxZ := v3[3];
                     BlockWrite(rfile,v3,1);
                  end;
            end;
            CloseFile(rfile);

           Result.OpenDataFile(rfile);
            for i := 0 to 100 do begin
               v[1] := 0.01 * i;
               v[2] := 0;
               MaxDensity := 0;
               for j := 0 to 100 do begin
                   if PointDensity[i,j] > MaxDensity then begin
                      v[2] := 0.01 * j;
                      MaxDensity := PointDensity[i,j];
                   end;
               end;
               Result.AddPointToDataBuffer(rfile,v);
            end;
            Result.ClosePointDataFile(rfile);

            Result.GraphDraw.DataFilesPlotted.Clear;
            Result.GraphDraw.RainBowColors := true;
            Result.GraphDraw.Symbol[1].Size := 2;
            Result.RedrawDiagram11Click(Nil);
         end
     end
     else begin
        ProcessFile;
        if (Sender = Normalizedbasinprofile1) then Result.RedrawDiagram11Click(Nil)
        else Result.AutoScaleAndRedrawDiagram;
     end;
   end;
   ShowStatus;
end;


procedure Tdbtablef.DEMTerrainprofile1Click(Sender: TObject);
begin
   DrawDEMTerrainprofile(Sender);
end;


procedure Tdbtablef.Descending1Click(Sender: TObject);
begin
   SortDataBase(DBOnTable,false);
end;

procedure Tdbtablef.Descending2Click(Sender: TObject);
begin
   SortDataBase(DBOnTable,false,SelectedColumn);
end;

procedure Tdbtablef.Descending3Click(Sender: TObject);
begin
   SortAndReplaceDataBase(DBOnTable,false);
end;

procedure Tdbtablef.Deadreckoning1Click(Sender: TObject);
var
   Lat,Long,Speed,Heading,Distance,Time : float64;

      procedure GetValues;
      begin
         Speed := GISdb[DBonTable].MyData.GetFieldByNameAsFloat('KNOTS');
         Heading := GISdb[DBonTable].MyData.GetFieldByNameAsFloat('HEADING');
         if GISdb[DBonTable].MyData.FieldExists('DT_HOURS') then Time := GISdb[DBonTable].MyData.GetFieldByNameAsFloat('DT_HOURS')
         else Time := 1;
         Distance := Time * Speed * 1.852 * 1000;   //convert to meters
         VincentyPointAtDistanceBearing(Lat,Long,Distance,Heading,Lat,Long);
      end;


begin
    if GISdb[DBonTable].MyData.FieldExists('KNOTS') and GISdb[DBonTable].MyData.FieldExists('HEADING') then begin
       GISdb[DBonTable].EmpSource.Enabled := false;
       if Sender = Deadreckoning1 then begin
          GISdb[DBonTable].MyData.First;
          if GISdb[DBonTable].ValidLatLongFromTable(Lat,Long) then begin
             while not GISdb[DBonTable].MyData.EOF do begin
                GetValues;
                GISdb[DBonTable].MyData.Next;
                GISdb[DBonTable].MyData.Edit;
                GISdb[DBonTable].MyData.SetFieldByNameAsFloat('LAT',Lat);
                GISdb[DBonTable].MyData.SetFieldByNameAsFloat('LONG',Long);
             end;
             GISdb[DBonTable].MyData.Post;
          end
       end
       else begin
          GISdb[DBonTable].MyData.Last;
          if GISdb[DBonTable].ValidLatLongFromTable(Lat,Long) then begin
             while not GISdb[DBonTable].MyData.BOF do begin
                GISdb[DBonTable].MyData.Prior;
                GetValues;
                GISdb[DBonTable].MyData.Edit;
                GISdb[DBonTable].MyData.SetFieldByNameAsFloat('LAT',Lat);
                GISdb[DBonTable].MyData.SetFieldByNameAsFloat('LONG',Long);
             end;
          end;
       end;
       ShowStatus;
       GISdb[DBonTable].RedrawLayerOnMap;
    end
    else begin
        MessageToContinue('Required fields HEADING and KNOTS');
    end;
end;


procedure Tdbtablef.Deadreckoningbackward1Click(Sender: TObject);
begin
   Deadreckoning1Click(Sender);
end;


procedure Tdbtablef.DechourstoHHMMSS1Click(Sender: TObject);
var
   TStr : ShortString;
   Hours : float64;
   IV : integer;
begin
   GISdb[DBonTable].AddFieldToDataBase(ftString,'TIME_STR',8,0);
   GISdb[DBonTable].EmpSource.Enabled := false;
   GISdb[DBonTable].MyData.First;
   while not  GISdb[DBonTable].MyData.EOF do begin
      GISdb[DBonTable].MyData.Edit;
      Hours := GISdb[DBonTable].MyData.GetFieldByNameAsFloat('DEC_HOURS');
      IV := trunc(Hours);
      TStr := IntegerToString(IV,2);
      Hours := 60 * (Hours - IV);
      iv := trunc(Hours);
      TStr := TSTr + ':' + IntegerToString(IV,2);
      Hours := 60 * (Hours - IV);
      iv := round(Hours);
      TStr := TSTr + ':' + IntegerToString(IV,2);
      for iv := 1 to 8 do if Tstr[iv] = ' ' then TStr[iv] := '0';
      GISdb[DBonTable].MyData.SetFieldByNameAsString('TIME_STR',TStr);
      GISdb[DBonTable].MyData.Next;
   end;
   ShowStatus;
end;

procedure Tdbtablef.Defaultssymbols1Click(Sender: TObject);
begin
   GISdb[DBonTable].GISProportionalSymbols(dbasDefault);
end;

procedure Tdbtablef.Deleteallrecords1Click(Sender: TObject);
begin
   GISdb[DBonTable].DeleteAllSelectedRecords;
end;

procedure Tdbtablef.Deletecurrentrecord1Click(Sender: TObject);
begin
   GISdb[DBonTable].MyData.MarkRecordForDeletion;
   ShowStatus;
end;

procedure Tdbtablef.Deletefield1Click(Sender: TObject);
begin
   GISdb[DBonTable].MyData.DeleteField(SelectedColumn);
   GISdb[DBonTable].RespondToChangedDB;
end;


procedure Tdbtablef.Deleterecord1Click(Sender: TObject);
begin
   GISdb[DBonTable].MyData.MarkRecordForDeletion;
end;

procedure Tdbtablef.Forceredrawofmap1Click(Sender: TObject);
begin
   GISdb[DBonTable].theMapOwner.DoFastMapRedraw;
end;


procedure Tdbtablef.ICESat2Beamsplitter1Click(Sender: TObject);
var
   OldFilter : Ansistring;

   procedure DoOne(Beam : shortString);
   begin
       GISdb[DBonTable].ApplyGISFilter(AddAndIfNeeded(OldFilter) + 'BEAM=' + QuotedStr(Beam));
       Distributionsummary(GISdb[DBonTable].MyData.Filter + 'n=' + IntToStr(GISdb[DBonTable].MyData.FiltRecsInDB));
       FiveSeriesGraph(DBonTable,-99,-99,-99,GISdb[DBonTable].LatFieldName);
   end;

begin
    OldFilter := GISdb[DBonTable].MyData.Filter;
    DoOne('gt1r');
    DoOne('gt1l');
    DoOne('gt2r');
    DoOne('gt2l');
    DoOne('gt3r');
    DoOne('gt3l');
    GISdb[DBonTable].ApplyGISFilter(OldFilter);
end;

procedure Tdbtablef.ICESat2canopyaddDEMdata1Click(Sender: TObject);
begin
   {$IfDef RecordIceSat} WriteLineRoDebugFile('ICESat2canopyaddDEMdata1Click'); {$EndIf}
   IcesatProcessCanopy(DBonTable,true);
end;

procedure Tdbtablef.ICESat2filecleanup1Click(Sender: TObject);
begin
   {$IfDef RecordIceSat} WriteLineRoDebugFile('ICESat2filecleanup1Click'); {$EndIf}
   IcesatProcessCanopy(DBonTable,false);
end;

procedure Tdbtablef.Iconfield1Click(Sender: TObject);
begin
   GISdb[DBonTable].GISProportionalSymbols(dbasIconField);
end;

procedure Tdbtablef.Iconsfromthumbnails1Click(Sender: TObject);
var
   fName : PathStr;
   bmp : tMyBitmap;
   tnName : PathStr;
   i : integer;
begin
   GISdb[DBonTable].AddIcon(false);
   GISdb[DBonTable].MyData.first;
   ReadDefault('Icon height (pixels)',MDDef.tnHeight);
   StartProgress('Thumbnails');
   i := 0;
   while not GISdb[DBonTable].MyData.eof do begin
     GISdb[DBonTable].EmpSource.Enabled := false;
     UpdateProgressBar(i/GISdb[DBonTable].MyData.FiltRecsInDB);
     inc(i);
     if GISdb[DBonTable].GetRotatedImage(bmp,fName) then begin
        MakeBitmapThumbNail(bmp,MDDef.tnHeight);
        tnName := ExtractFilePath(fName) + 'TN-' + extractFileName(fName);
        SaveBitmap(Bmp,tnName);
        bmp.Free;
        tnName := extractFileName(tnName);
        GISdb[DBonTable].MyData.Edit;
        GISdb[DBonTable].MyData.SetFieldByNameAsString('ICON',tnName);
     end;
     GISdb[DBonTable].MyData.Next;
   end;
   ShowStatus;
end;


procedure Tdbtablef.iesbyopinions1Click(Sender: TObject);
var
   entry : shortstring;
   i,nTies,j : integer;

         procedure DoOne(DEM : shortString);
         var
            n : integer;
         begin
            if ANSIcontainsStr(entry,DEM) then n := succ(nTies) else n := 0;
            GISdb[DBonTable].MyData.SetFieldByNameAsInteger(DEM + '_TIE',n);
         end;

begin
   {$IfDef RecordDEMIX} HighlightLineToDebugFile('Tdbtablef.iesbyopinions1Click in'); {$EndIf}
    GISdb[DBonTable].AddFieldToDataBase(ftInteger,'ALOS_TIE',4);
    GISdb[DBonTable].AddFieldToDataBase(ftInteger,'ASTER_TIE',4);
    GISdb[DBonTable].AddFieldToDataBase(ftInteger,'COP_TIE',4);
    GISdb[DBonTable].AddFieldToDataBase(ftInteger,'SRTM_TIE',4);
    GISdb[DBonTable].AddFieldToDataBase(ftInteger,'NASA_TIE',4);
    GISdb[DBonTable].AddFieldToDataBase(ftInteger,'FABDEM_TIE',4);
    GISdb[DBonTable].AddFieldToDataBase(ftInteger,'TANDEM_TIE',4);
    GISdb[DBonTable].AddFieldToDataBase(ftInteger,'TIES',4);
    GISdb[DBonTable].AddFieldToDataBase(ftString,'CRIT_CAT',4);
    GISdb[DBonTable].EmpSource.Enabled := false;
    GISdb[DBonTable].MyData.First;
    StartProgress('Tie numbers');
    j := 0;
    while not GISdb[DBonTable].MyData.eof do begin
       inc(j);
       UpdateProgressBar(j/GISdb[DBonTable].MyData.FiltRecsInDB);
       entry := UpperCase(GISdb[DBonTable].MyData.GetFieldByNameAsString('DEM_LOW_SC'));
       GISdb[DBonTable].MyData.Edit;
       GISdb[DBonTable].MyData.SetFieldByNameAsString('DEM_LOW_SC',entry);
       nTies := 0;
       for i := 1 to Length(Entry) do if (Entry[i] = ',') then inc(NTies);
       GISdb[DBonTable].MyData.SetFieldByNameAsInteger('TIES',succ(nTies));
       GISdb[DBonTable].MyData.SetFieldByNameAsString('CRIT_CAT',Copy(GISdb[DBonTable].MyData.GetFieldByNameAsString('CRITERION'),1,4));
       DoOne('ASTER');
       DoOne('ALOS');
       DoOne('NASA');
       DoOne('SRTM');
       DoOne('COP');
       DoOne('FABDEM');
       DoOne('TANDEM');
       GISdb[DBonTable].MyData.Next;
    end;

   ShowStatus;
   {$IfDef RecordDEMIX} HighlightLineToDebugFile('Tdbtablef.iesbyopinions1Click out'); {$EndIf}
end;

procedure Tdbtablef.Includedebuglog1Click(Sender: TObject);
begin
   Zipshapefile1Click(Sender);
end;

procedure Tdbtablef.Increasefieldlength1Click(Sender: TObject);
var
   Max : integer;
begin
   Max := GISdb[DBonTable].MyData.GetFieldLength(SelectedColumn);
   ReadDefault('New field length',Max);
   if (Max < GISdb[DBonTable].MyData.GetFieldLength(SelectedColumn)) then begin
      if Not AnswerIsYes('OK if some data lost by shortening field') then exit;
   end;
   GISdb[DBonTable].MyData.TrimField(SelectedColumn,Max);
   GISdb[DBonTable].RespondToChangedDB;
end;

procedure Tdbtablef.Indexnewlines1Click(Sender: TObject);
{$IfDef ExSidescan}
begin
{$Else}
var
   fName,fname2 : PathStr;
   Table2 : tMyData;
   TheFiles : tStringList;
   i: integer;
   Lat,Long,Lat2,Long2,Dist,Heading : float64;
begin
   TheFiles := Nil;
   FindMatchingFiles(ExtractFilePath(SidescanIndexFName),'*.xtf',TheFiles,5);

   StartProgress('Index');
   for i := 0 to pred(TheFiles.Count) do begin
      UpdateProgressBar(i/TheFiles.Count);
      fName2 := TheFiles.Strings[i];
      {$IfDef RecordSideScan} WriteLineRoDebugFile(fName2); {$EndIf}
      GISdb[SideIndexDB].dbOpts.MainFilter := 'FILENAME=' + QuotedStr(fName2);
      GISdb[SideIndexDB].AssembleGISFilter;
      if (GISdb[SideIndexDB].MyData.RecordCount = 0) then begin
         {$IfDef RecordSideScan} WriteLineRoDebugFile('  need to add to index'); {$EndIf}
         fName := ChangeFileExt(fName2,DefaultDBExt);
         Table2 := tMyData.Create(fName);
         repeat
            Lat := Table2.GetFieldByNameAsFloat('LAT');
            Long := Table2.GetFieldByNameAsFloat('LONG');
            Table2.Next;
         until (Lat > 1) or (Table2.eof);

         if Table2.eof then begin
            Lat2 := 0;
            Long2 := 0;
         end
         else begin
            Table2.Last;
            repeat
               Lat2 := Table2.GetFieldByNameAsFloat('LAT');
               Long2 := Table2.GetFieldByNameAsFloat('LONG');
               Table2.Prior;
            until (Lat2 > 1);
         end;
         VincentyCalculateDistanceBearing(Lat,Long,Lat2,Long2,Dist,Heading);
         GISdb[SideIndexDB].MyData.Insert;
         GISdb[SideIndexDB].MyData.SetFieldByNameAsString('LINE_NAME',ExtractFileName(fName2));
         GISdb[SideIndexDB].MyData.SetFieldByNameAsString('FILENAME',fName2);
         GISdb[SideIndexDB].MyData.SetFieldByNameAsString('USE','Y');
         GISdb[SideIndexDB].MyData.SetFieldByNameAsFloat('LAT',Lat);
         GISdb[SideIndexDB].MyData.SetFieldByNameAsFloat('LONG',Long);
         GISdb[SideIndexDB].MyData.SetFieldByNameAsFloat('LAT2',Lat2);
         GISdb[SideIndexDB].MyData.SetFieldByNameAsFloat('LONG2',Long2);
         GISdb[SideIndexDB].MyData.SetFieldByNameAsFloat('LENGTH',Dist);
         GISdb[SideIndexDB].MyData.SetFieldByNameAsFloat('HEADING',Heading);
         GISdb[SideIndexDB].MyData.SetFieldByNameAsInteger('PINGS',Table2.RecordCount);
         GISdb[SideIndexDB].MyData.Post;
         Table2.Destroy;
      end;
   end;
   TheFiles.Free;
   EndProgress;
{$EndIf}
end;


procedure Tdbtablef.Insertareacolorsymbology1Click(Sender: TObject);
begin
   GISdb[DBonTable].AddFieldToDataBase(ftInteger,'LINE_COLOR',8);
   GISdb[DBonTable].AddFieldToDataBase(ftInteger,'LINE_WIDTH',2);
   GISdb[DBonTable].AddFieldToDataBase(ftInteger,'FILL_COLOR',8);
   GISdb[DBonTable].AddFieldToDataBase(ftInteger,'FILL_PAT',2);
   GISdb[DBonTable].AreaFillPresent := true;
end;


procedure Tdbtablef.InsertCLASSfield1Click(Sender: TObject);
var
   TStr : shortstring;
begin
   GISdb[DBonTable].AddFieldToDatabase(ftInteger,'CLASS',3);
   TStr := '';
   Petmar.GetString('Class code for all records',TStr,false,['0'..'9']) ;
   if (TStr <> '') then  GISdb[DBonTable].FillFieldWithValue('CLASS',TStr);
   ShowStatus;
end;


procedure Tdbtablef.Insertcolorfield1Click(Sender: TObject);
begin
   GISdb[DBonTable].AddFieldToDataBase(ftInteger,'COLOR',8,0);
   GISdb[DBonTable].ColorPresent := true;
end;


procedure Tdbtablef.Insertdatefield1Click(Sender: TObject);
begin
   with GISdb[DBonTable] do begin
      AddFieldToDataBase(ftString,'DATE_LABEL',12,0);
      EmpSource.Enabled := false;
      GISdb[DBonTable].MyData.First;
      while not GISdb[DBonTable].MyData.Eof do begin
         GISdb[DBonTable].MyData.Edit;
         GISdb[DBonTable].MyData.SetFieldByNameAsString('DATE_LABEL',MyData.GetFieldByNameAsString(MonthFieldName) + '/' +
            GISdb[DBonTable].MyData.GetFieldByNameAsString('DAY') + '/' + GISdb[DBonTable].MyData.GetFieldByNameAsString('YEAR'));
         GISdb[DBonTable].MyData.Next;
      end;
      ShowStatus;
   end;
end;


procedure Tdbtablef.InsertDATELABEL1Click(Sender: TObject);
var
   tStr : shortstring;
begin
   GISdb[DBonTable].AddFieldToDatabase(ftString,'DATE_LABEL',12);
   TStr := '';
   Petmar.GetString('DATE_LABEL',TStr,false,ReasonableTextChars);
   if (TStr <> '') then GISdb[DBonTable].FillFieldWithValue('DATE_LABEL',TStr);
end;


procedure Tdbtablef.Insertdipdirectionsdipandstrike1Click(Sender: TObject);
{$IfDef ExGeology}
begin
{$Else}

      procedure Output(Plane : integer);
      var
         Dip,Strike : integer;
      begin
         Dip := GISdb[DBonTable].MyData.GetFieldByNameAsInteger('FP' + IntToStr(Plane) + '_DIP');
         Strike := GISdb[DBonTable].MyData.GetFieldByNameAsInteger('FP' + IntToStr(Plane) + '_STRIKE');
         GISdb[DBonTable].MyData.SetFieldByNameAsString('FP' + IntToStr(Plane) + '_DIPSTK',CreateStrikeAndDipString(Strike,Dip));
         GISdb[DBonTable].MyData.SetFieldByNameAsInteger('FP' + IntToStr(Plane) + '_DIPDIR',Round(DipDirectionRHR(Strike)));
      end;


begin
   GISdb[DBonTable].AddFieldToDatabase(ftString,'FP1_DIPSTK',12);
   GISdb[DBonTable].AddFieldToDatabase(ftString,'FP2_DIPSTK',12);
   GISdb[DBonTable].AddFieldToDatabase(ftInteger,'FP1_DIPDIR',5);
   GISdb[DBonTable].AddFieldToDatabase(ftInteger,'FP2_DIPDIR',5);
   GISdb[DBonTable].EmpSource.Enabled := false;
   GISdb[DBonTable].MyData.First;
   ShowHourglassCursor;
   while not GISdb[DBonTable].MyData.eof do begin
      GISdb[DBonTable].MyData.Edit;
      Output(1);
      Output(2);
      GISdb[DBonTable].MyData.Next;
   end;
   ShowStatus;
{$EndIf}
end;

procedure Tdbtablef.Insertfield1Click(Sender: TObject);
begin
   New_Field.AddNewField(GISdb[DBonTable]);
   ShowStatus;
end;

procedure Tdbtablef.Inserticonsfromfilterfile1Click(Sender: TObject);
var
   Table : tMyData;
   FileWanted : PathStr;
   Bitmap : tMyBitmap;
   Sym : tDrawingSymbol;
   SymSize : byte;
   SymColor : tPlatformColor;
begin
   FileWanted := '';
   if GetFileFromDirectory('Symbol filter file',DefaultDBMask,FileWanted) then begin
       Table := tMyData.Create(FileWanted);
       GISdb[DBonTable].AddFieldToDatabase(ftFloat,'ICON_SCALE',5,2);
       GISdb[DBonTable].AddFieldToDatabase(ftString,'ICON',24,0);
       while not Table.EOF do begin
           GISdb[DBonTable].dbOpts.MainFilter := Table.GetFieldByNameAsString('FILTER');
           GISdb[DBonTable].AssembleGISFilter;
           CreateBitmap(bitmap,50,50);
           Table.DefinePointSymbol(Sym,SymSize,SymColor);
           ScreenSymbol(Bitmap.Canvas,25,25,Sym,SymSize,SymColor);
           PetImage.GetImagePartOfBitmap(Bitmap);
           Petimage.SaveBitmap(Bitmap,MainMapData + 'icons\' + Table.GetFieldByNameAsString('ICON'));
           Bitmap.Free;
           GISdb[DBonTable].EmpSource.Enabled := false;
           while not GISdb[DBonTable].MyData.eof do begin
              GISdb[DBonTable].MyData.Edit;
              GISdb[DBonTable].MyData.SetFieldByNameAsString('ICON',Table.GetFieldByNameAsString('ICON'));
              GISdb[DBonTable].MyData.SetFieldByNameAsFloat('ICON_SCALE',Table.GetFieldByNameAsFloat('ICON_SCALE'));
              GISdb[DBonTable].MyData.Next;
           end;
           Table.Next;
       end;
       Table.Destroy;
       with GISdb[DBonTable] do if not IconPresent then begin
          IconFieldNames[1] := 'ICON';
          IconPresent := true;
       end;
       ShowStatus;
       GISdb[DBonTable].ClearGISFilter;
   end;
end;

procedure Tdbtablef.Insertimagefield1Click(Sender: TObject);
begin
   GISdb[DBonTable].AddImage;
   ShowStatus;
end;

procedure Tdbtablef.InsertJuliandate1Click(Sender: TObject);
var
   Year,Month,Day : integer;
begin
   with GISdb[DBonTable] do begin
       AddFieldToDataBase(ftInteger,'JULIAN_DAY',3,0);
       EmpSource.Enabled := false;
       GISdb[DBonTable].MyData.First;
       While not GISdb[DBonTable].MyData.eof do begin
          Year := GISdb[DBonTable].MyData.GetFieldByNameAsInteger('YEAR');
          Month := GISdb[DBonTable].MyData.GetFieldByNameAsInteger(MonthFieldName);
          Day := GISdb[DBonTable].MyData.GetFieldByNameAsInteger('DAY');
          GISdb[DBonTable].MyData.Edit;
          GISdb[DBonTable].MyData.SetFieldByNameAsInteger('JULIAN_DAY',AnnualJulianDay(year,month,day));
          GISdb[DBonTable].MyData.Next;
       end;
       ShowStatus;
   end;
end;

procedure Tdbtablef.Insertlinecolorwidthfields1Click(Sender: TObject);
begin
   GISdb[DBonTable].AddFieldToDataBase(ftInteger,'LINE_COLOR',8,0);
   GISdb[DBonTable].AddFieldToDataBase(ftInteger,'LINE_WIDTH',2,0);
   GISdb[DBonTable].LineColorPresent := true;
end;

procedure Tdbtablef.InsertMASKfield1Click(Sender: TObject);
var
   i,j : integer;
begin
   with GISdb[DBonTable] do begin
      if not GISdb[DBonTable].MyData.FieldExists('MASK') then begin
         AddFieldToDataBase(ftString,'MASK',1);
      end
      else begin
         i := 1;
         for j := 2 to 5 do if GISdb[DBonTable].MyData.FieldExists('MASK' + IntToStr(j)) then inc(i);
         if AnswerIsYes('Currently ' + IntToStr(i) + ' mask fields; Add another') then begin
             AddFieldToDataBase(ftString,'MASK' + IntToStr(succ(i)),1);
         end;
      end;
   end;
end;


procedure Tdbtablef.InsertNAMEfield1Click(Sender: TObject);
begin
   GISdb[DBonTable].AddFieldToDatabase(ftString,'NAME',32);
end;

procedure Tdbtablef.InsertNewRecClipboardClick(Sender: TObject);
var
   Parts : array of integer;
   zs : array of float64;
   ShapeFileCreator : tShapeFileCreation;
begin
   with GISdb[DBonTable] do begin
      GISdb[DBonTable].MyData.Last;
      GISdb[DBonTable].MyData.Insert;
      if (ClipBoard_Line_Coords <> Nil) then with AShapeFile do begin
         inc(AShapeFile.NumRecs);
         ShapeFileCreator := tShapeFileCreation.Create(WGS84DatumConstants,'',false,ShapeFileType,true,aShapeFile);
         ShapeFileCreator.RecsInShapeStream := AShapeFile.NumRecs;
         ShapeFileCreator.OLDProcessRecordForShapeFile(ClipBoard_Line_Coords^,1,ClipBoard_NumPts,Parts,zs);
         ShapeFileCreator.CloseShapeFiles;
         PutBoundBoxInTable(MyData,LastRecPolyLineHeader.BoundBox);
         Dispose(ClipBoard_Line_Coords);
         ClipBoard_Line_Coords := Nil;
         ClipBoard_NumPts := 0;
      end
      else begin
         GISdb[DBonTable].MyData.SetFieldByNameAsFloat(LatFieldName,Clipboard_Lat);
         GISdb[DBonTable].MyData.SetFieldByNameAsFloat(LongFieldName,ClipBoard_Long);
      end;

      if (ClipBoard_Name <> '') and GISdb[DBonTable].MyData.FieldExists('NAME') then GISdb[DBonTable].MyData.SetFieldByNameAsString('NAME',Clipboard_Name);
      GISdb[DBonTable].MyData.Post;
      ApplicationProcessMessages;
      DisplayTheRecord(MyData.RecordCount,true,true);

      Clipboard_Name := '';
      ClipBoard_Coords := false;
   end;
end;


procedure Tdbtablef.Insertnewrecordatdistancebearing1Click(Sender: TObject);
var
  InsertPointForm : TInsertPointForm;
  Lat,Long,Dist,Heading : float64;
begin
   GISdb[DBonTable].AddFieldToDataBase(ftString,'NAME',24);
   GISdb[DBonTable].AddFieldToDataBase(ftString,'NOTES',144);
   InsertPointForm := TInsertPointForm.Create(Application);
   InsertPointForm.ShowModal;
   if GISdb[DBonTable].ValidLatLongFromTable(Lat,Long) then begin
      CheckEditString(InsertPointForm.Edit1.Text,Dist);
      Dist := Dist / InsertPointForm.Factor;
      CheckEditString(InsertPointForm.Edit2.Text,Heading);
      VincentyPointAtDistanceBearing(Lat,Long,Dist,Heading,Lat,Long);
      GISdb[DBonTable].MyData.Insert;
      GISdb[DBonTable].MyData.SetFieldByNameAsFloat(GISdb[DBonTable].LatFieldName,Lat);
      GISdb[DBonTable].MyData.SetFieldByNameAsFloat(GISdb[DBonTable].LongFieldName,Long);
      If GISdb[DBonTable].MyData.FieldExists('NAME') then GISdb[DBonTable].MyData.SetFieldByNameAsString('NAME',InsertPointForm.Edit3.Text);
      If GISdb[DBonTable].MyData.FieldExists('NOTES') then GISdb[DBonTable].MyData.SetFieldByNameAsString('NOTES',InsertPointForm.Edit4.Text);
   end;
   InsertPointForm.Destroy;
   Highlightrecordonmap1Click(Sender);
end;


procedure Tdbtablef.InsertNPTSfield1Click(Sender: TObject);
begin
   GISdb[DBonTable].AddFieldToDatabase(ftInteger,'NPTS',8);
end;


procedure Tdbtablef.Insertpointsymbol1Click(Sender: TObject);
begin
   GISdb[DBonTable].AddSymbolToDB;
   GISdb[DBonTable].PointSymbolFieldsPresent := true;
   Insertpointsymbol1.Visible := false;
   if AnswerIsYes('Fill point symbols') then begin
      with GISdb[DBonTable] do begin
          GetSymbol(dbOpts.Symbol.DrawingSymbol,dbOpts.Symbol.Size,dbOpts.Symbol.Color);
          GISdb[DBonTable].MyData.First;
          while not GISdb[DBonTable].MyData.eof do begin
             GISdb[DBonTable].MyData.Edit;
             GISdb[DBonTable].MyData.PostPointSymbol(dbOpts.Symbol);
             GISdb[DBonTable].MyData.Next;
          end;
      end;
   end;
end;


procedure Tdbtablef.Insertrecord1Click(Sender: TObject);
begin
   {$IfDef RecordEditDB} WriteLineRoDebugFile('Tdbtablef.Insertrecord1Click in'); {$EndIf}
   with GISdb[DBonTable] do begin
      GISdb[DBonTable].MyData.CopyRecordToEndOfTable;
      GISdb[DBonTable].MyData.Last;
      ApplicationProcessMessages;
      DisplayTheRecord(MyData.RecordCount,true,true);
   end;
   {$IfDef RecordEditDB} WriteLineRoDebugFile('Tdbtablef.Insertrecord1Click out'); {$EndIf}
end;


procedure Tdbtablef.Inserttimeanimationfields1Click(Sender: TObject);
begin
   Add_Time_Fields.FillDateFields(GISdb[DBonTable]);
end;

procedure Tdbtablef.InsertWWWfield1Click(Sender: TObject);
begin
   GISdb[DBonTable].AddWWW;
   ShowStatus;
end;

procedure Tdbtablef.InsertYEARMONTHDATE1Click(Sender: TObject);
begin
   SplitDateField1Click(Sender);
end;

procedure Tdbtablef.Integer1Click(Sender: TObject);
{$IfDef ExGeoStats}
begin
{$Else}
var
   FieldName1,FieldName2 : ShortString;
   Percentages : boolean;
begin
   {$IfDef RecordCorrelationMatrix} WriteLineRoDebugFile('Tdbtablef.Integer1Click in'); {$EndIf}
   GISdb[DBonTable].EmpSource.Enabled := false;
   FieldName1 := GISdb[DBonTable].PickField('first field',[ftInteger,ftSmallInt]);
   if (FieldName1 = '') then exit;
   FieldName2 := GISdb[DBonTable].PickField('second field',[ftInteger,ftSmallInt]);
   if (FieldName2 = '') then exit;
   Percentages := AnswerIsYes('Show percentages');
   GISdb[DBonTable].EmpSource.Enabled := false;
   DBCoOccurrence(GISdb[DBonTable].MyData,GISdb[DBonTable].EmpSource,FieldName1,FieldName2,Percentages);
   ShowStatus;
{$EndIf}
end;

procedure Tdbtablef.Integer2Click(Sender: TObject);
begin
   GISdb[DBonTable].AddAndFillFieldFromDEM(adElevNearestInt);
end;

procedure Tdbtablef.InventoryFUVSSIMcriteriainDB1Click(Sender: TObject);
begin
   CriteriaInSSIM_FUV_db(dbOnTable);
end;

procedure Tdbtablef.Editrecordsingrid1Click(Sender: TObject);
begin
   DBGrid1.Options := DBGrid1.Options + [dgEditing] - [dgRowSelect];
   Editrecordsingrid1.Checked := true;
end;


procedure Tdbtablef.EGM20081Click(Sender: TObject);
begin
   AddEGMtoDBfromSphHarmonics(DBonTable,(Sender = EGM20081));
   ShowStatus;
end;


procedure Tdbtablef.EGM961Click(Sender: TObject);
begin
   EGM20081Click(Sender);
end;

procedure Tdbtablef.Driftmodel1Click(Sender: TObject);
begin
   {$IfDef ExOceanography}
   {$Else}
      Drift_model.LoadDriftModel(GISdb[DBonTable].theMapOwner,GISdb[DBonTable].DBNumber);
   {$EndIf}
end;

procedure Tdbtablef.BackupDB1Click(Sender: TObject);
begin
   GISdb[DBonTable].BackupDB;
end;


procedure Tdbtablef.Beachballs1Click(Sender: TObject);
var
   Thin,rc : integer;
begin
   {$IfDef ExGeology}
   {$Else}
      SetBeachBallOptions(True,GISdb[DBonTable].TheMapOwner.MapDraw.ScreenPixelSize);
      GISdb[DBonTable].dbOpts.DBAutoShow := dbasBeachBall;

      Thin := 1;
      rc := GISdb[DBonTable].MyData.FiltRecsInDB;
      if (rc > MDDef.NetDef.MaxNumBeachBalls) and (GISdb[DBonTable].MyData.Filter = '') then begin
         GISdb[DBonTable].LimitDBtoMapArea;
         rc := GISdb[DBonTable].MyData.FiltRecsInDB;
      end;

      while (rc div Thin > MDDef.NetDef.MaxNumBeachBalls) do inc(Thin);

       if (rc > MDDef.NetDef.MaxNumBeachBalls) then begin
          {$IfDef RecordBeachBall} WriteLineRoDebugFile('Over MDDef.NetDef.MaxNumBeachBalls records'); {$EndIf}
          if AnswerIsYes('Proceed with ' + IntToStr(rc) + ' records, thin by factor of ' + IntToStr(Thin)) then begin
             //GISdb[DBonTable].RedrawLayerOnMap;
          end
          else begin
             {$IfDef RecordBeachBall} WriteLineRoDebugFile('Bailed because recs=' + IntToStr(rc)); {$EndIf}
             GISdb[DBonTable].dbOpts.DBAutoShow := dbasQuakeMechColor;
          end;
       end;
       GISdb[DBonTable].RedrawLayerOnMap;
   {$EndIf}
end;


procedure Tdbtablef.Beachballscolorandsize1Click(Sender: TObject);
begin
   Dipandstrikes1Click(Sender);
end;


procedure Tdbtablef.BestDEMbycategory1Click(Sender: TObject);
begin
{$IfDef ExDEMIXexperimentalOptions}
{$Else}
   BestDEMSbyCategory(DBonTable);
{$EndIf}
end;


procedure Tdbtablef.BestDEMpertilebycriteria1Click(Sender: TObject);
begin
   {$IfDef ExDEMIXexperimentalOptions}
   {$Else}
      DEMIX_graph_best_in_Tile(DBonTable,true);
   {$EndIf}
end;

procedure Tdbtablef.Showarearecords1Click(Sender: TObject);
var
   Bitmap : tMyBitmap;
   x,y,i,rc : integer;
   Lat,Long : float64;
   Sym : tDrawingSymbol;
begin
   Sym := FilledBox;
   Petmar.GetSymbol(Sym,GISdb[DBonTable].AreaRecordSize,GISdb[DBonTable].dbOpts.FillColor,'Area centroids');
   GISdb[DBonTable].EmpSource.Enabled := false;
   CopyImageToBitmap(GISdb[DBonTable].theMapOwner.Image1,BitMap);
   StartProgress('Centroids');
   i := 0;
   rc := GISdb[DBonTable].MyData.RecordCount;
   GISdb[DBonTable].MyData.First;
   while not GISdb[DBonTable].MyData.EOF do begin
      inc(i);
      if i mod 100 = 0 then begin
         UpdateProgressBar(i/rc);
         GISdb[DBonTable].EmpSource.Enabled := false;
      end;
      GISdb[DBonTable].GetLatLongToRepresentRecord(Lat,Long);
      GISdb[DBonTable].TheMapOwner.MapDraw.LatLongDegreeToScreen(Lat,Long,x,y);
      Petmar.ScreenSymbol(Bitmap.Canvas,x,y,Sym,GISdb[DBonTable].AreaRecordSize,GISdb[DBonTable].dbOpts.FillColor);
      GISdb[DBonTable].MyData.Next;
   end;
   GISdb[DBonTable].theMapOwner.Image1.Picture.Graphic := Bitmap;
   Bitmap.Free;
   ShowStatus;
end;


procedure Tdbtablef.ShowDBsettings1Click(Sender: TObject);
var
   fName : PathStr;
begin
   GISdb[DBonTable].SaveDataBaseStatus;
   fName := GISdb[DBonTable].DBAuxDir + ExtractFileName(GISdb[DBonTable].dbFullName) + '.ini';
   QuickOpenEditWindow(FName,ExtractFileName(fName));
end;

procedure Tdbtablef.Showjoin1Click(Sender: TObject);
begin
   DB_Join.SetUpJoin(GISdb[DBonTable],true);
end;

procedure Tdbtablef.Showneighbors1Click(Sender: TObject);
var
   LinkValue : shortstring;
   Bitmap : tMyBitmap;
   NumNeigh,i : integer;
   OldFilter : string;
begin
   if ValidDB(DBonTable) and (GISdb[DBonTable].theMapOwner <> Nil) then with GISdb[DBonTable] do begin
      LinkValue := GISdb[DBonTable].MyData.GetFieldByNameAsString(NeighborLinkField);
      OldFilter :=  GISdb[DBonTable].MyData.Filter;
      NeighborTable.ApplyFilter( NeighborLinkField + '=' + QuotedStr(LinkValue));

      if (BaseMapBitmap = Nil) then CopyImageToBitmap(GISdb[DBonTable].theMapOwner.Image1,BaseMapBitmap);
      theMapOwner.Image1.Picture.Graphic := BaseMapBitmap;
      if not CopyImageToBitmap(theMapOwner.Image1,Bitmap) then exit;
      Bitmap.Canvas.Pen.Color := ConvertPlatformColorToTcolor(MDDef.HighlightColor);
      Bitmap.Canvas.Pen.Width := 1;
      Bitmap.Canvas.Brush.Color := Bitmap.Canvas.Pen.Color;
      Bitmap.Canvas.Brush.Style := bsSolid;
      DisplayCurrentRecordOnMap(TheMapOwner.MapDraw,Bitmap);

      NumNeigh := NeighborTable.GetFieldByNameAsInteger('NUM_NEIGH');
      for i := 1 to NumNeigh do begin
         LinkValue := NeighborTable.GetFieldByNameAsString('NEIGH_' + IntToStr(i));
         GISdb[DBonTable].MyData.ApplyFilter( NeighborLinkField + '=' + QuotedStr(LinkValue));
         DisplayCurrentRecordOnMap(TheMapOwner.MapDraw,Bitmap);
      end;

      theMapOwner.Image1.Picture.Graphic := Bitmap;
      Bitmap.Destroy;
      GISdb[DBonTable].MyData.ApplyFilter(OldFilter);
   end;
end;

procedure Tdbtablef.BitBtn10Click(Sender: TObject);
var
  FieldsInDB : tStringList;
  i: integer;
  ch : char;
  aName : ShortString;
begin
   {$IfDef RecordFan} WriteLineRoDebugFile('Tdbtablef.BitBtn-10Click in'); {$EndIf}
   with GISdb[DBonTable],TheMapOwner.MapDraw do begin
      DBFieldUniqueEntries('NAME',FieldsInDB);
      if (FieldsInDB.Count <> GISdb[DBonTable].MyData.RecordCount) then begin
         if not AnswerIsYes('NAME field has non-unique entries; problems possible.  Continue anyway') then begin
            FieldsInDB.Free;
            Exit;
         end;
      end;

      TheMapOwner.MapDraw.DeleteSingleMapLayer(TheMapOwner.MapDraw.AllFansCoverageFName);
      SaveFilterStatus(true);
      EmpSource.Enabled := false;
      ClearGISFilter;
      ShowHourglassCursor;
      if (FieldsInDB.Count <> GISdb[DBonTable].MyData.RecordCount) then begin
         for i := 0 to pred(FieldsInDB.Count) do begin
            GISdb[DBonTable].MyData.ApplyFilter('NAME=' + QuotedStr(FieldsInDB.Strings[i]));
            ch := 'A';
            {$IfDef RecordFan} WriteLineRoDebugFile(FieldsInDB.Strings[i] + '  n=' + IntToStr(MyData.RecordCount)); {$EndIf}
            while (MyData.RecordCount > 1) do begin
               aName := GISdb[DBonTable].MyData.GetFieldByNameAsString('NAME');
               if Length(aName)+ 1 < GISdb[DBonTable].MyData.GetFieldLength('NAME') then begin
                  GISdb[DBonTable].MyData.Edit;
                  GISdb[DBonTable].MyData.SetFieldByNameAsString('NAME',aName + ch);
                  GISdb[DBonTable].MyData.Post;
                  inc(ch);
               end
               else break;
            end;
         end;
      end;
      ClearGISFilter;
      FieldsInDB.Free;
      Toggle_DB_Use.VerifyRecordsToUse(MyData,'NAME','Sensors to use','USE','COVERS');
      ShowStatus;
      RestoreFilterStatus;
      TheMapOwner.MapDraw.ClearGISLayer(DBonTable);
      BitBtn6Click(Nil);
      theMapOwner.DoFastMapRedraw;
   end;
   {$IfDef RecordFan} WriteLineRoDebugFile('Tdbtablef.BitBtn-10Click out'); {$EndIf}
end;


procedure Tdbtablef.BitBtn11Click(Sender: TObject);
var
   Bitmap : tMyBitmap;
begin
   Bitmap := Nil;
   CloneImageToBitmap(GISdb[DBonTable].theMapOwner.Image1,Bitmap);
   GISdb[DBonTable].theMapOwner.MapDraw.DeleteSingleMapLayer(GISdb[DBonTable].theMapOwner.MapDraw.GazOverlayfName);
   GISdb[DBonTable].theMapOwner.MapDraw.ClearGISLayer(DBonTable);
   GISdb[DBonTable].TheMapOwner.MapDraw.LabelGazetteerFeatures(Bitmap);
   Bitmap.Free;
   GISdb[DBonTable].TheMapOwner.DoFastMapRedraw;
end;


procedure Tdbtablef.Font1Click(Sender: TObject);
begin
   {$IfDef RecordFont} WriteLineRoDebugFile('Tdbtablef.Font1Click in  Gis font 1: ' + MyFontToString(MDDef.GisLabelFont1)); {$EndIf}
   EditMyFont(GISdb[DBonTable].dbOpts.GisLabelFont1);
   {$IfDef RecordFont} WriteLineRoDebugFile('Tdbtablef.Font1Click out Gis font 1: ' + MyFontToString(MDDef.GisLabelFont1)); {$EndIf}
end;


procedure Tdbtablef.LoadCopDEMandLandcoverforarea1Click(Sender: TObject);
begin
   LoadCopAndLancoverForDEMIXTile(GISdb[dbOnTable].MyData.GetFieldByNameAsString('AREA'));
end;


procedure Tdbtablef.Loadfrommaplibrary1Click(Sender: TObject);
const
   Extra = 0.0001;
var
   WantDEM : integer;
   Lat,Long : float64;
   bbox : sfBoundBox;
begin
   if LineOrAreaShapeFile(GISdb[DBonTable].ShapeFileType) then begin
      bbox := GISdb[DBonTable].MyData.GetRecordBoundingBox;
   end
   else begin
       GISdb[DBonTable].MyData.ValidLatLongFromTable(Lat,Long);
       bbox.YMax := Lat + extra;
       bbox.XMin := Long - extra;
       bbox.YMin := Lat - Extra;
       bbox.XMax := Long + extra;
   end;
   WantDEM := LoadMapLibraryBox(true, bbox);  //bbox.YMax,bbox.XMin,bbox.YMin,bbox.YMax);
end;

procedure Tdbtablef.Loadmapsforthisarea1Click(Sender: TObject);
begin
   LoadThisDEMIXTile(GISdb[dbOnTable].MyData.GetFieldByNameAsString('AREA'),'');
end;

procedure Tdbtablef.LoadMSTfiles1Click(Sender: TObject);
{$IfDef ExSideScan}
begin
{$Else}
var
   TheFiles : tStringList;
   fName : PathStr;
   i : integer;
begin
   with GISdb[DBonTable] do begin
      TheFiles := nil;
      EmpSource.Enabled := false;
      PetDBUtils.FindUniqueEntries(MyData,'MST_FILE',TheFiles);
      if (TheFiles.Count < 10) or AnswerIsYes('Open ' + IntToStr(TheFiles.Count) + ' files') then begin
         for i  := 0 to pred(TheFiles.Count)  do begin
            fName := ExtractFilePath(dbFullName) + TheFiles.Strings[i] + '.mst';
            MST_Format.ImportMSTSidescan(fName);
         end;
      end;
      TheFiles.Free;
      ShowStatus;
   end;
{$EndIf}
end;

procedure Tdbtablef.LoadtestandreferenceDEMs1Click(Sender: TObject);
begin
   LoadThisDEMIXTile(GISdb[DBonTable].MyData.GetFieldByNameAsString('AREA'),GISdb[DBonTable].MyData.GetFieldByNameAsString('DEMIX_TILE'));
end;

procedure Tdbtablef.LoadthisDEM1Click(Sender: TObject);
begin
   OpenNewDEM(GISdb[DBonTable].MyData.GetFieldByNameAsString('FILENAME'));
end;

procedure Tdbtablef.Logoffield1Click(Sender: TObject);
begin
   Sumtwofields1Click(Sender);
end;

procedure Tdbtablef.Downhilluphillsegments1Click(Sender: TObject);
var
   i,y,x : integer;
   LastZ,z,
   Lat,Long : float64;
   Color : tPlatformColor;
begin
   {$IfDef RecordTerrainProfiles} WriteLineRoDebugFile('Tdbtablef.Downhilluphillsegments1Click'); {$EndIf}
   with GISdb[DBonTable] do begin
      if ShapeFileType in [13,15,23,25] then aShapeFile.GetLineCoords(MyData.RecNo,true)
      else aShapeFile.GetLineCoordsAndZsFromDEM(TheMapOwner.MapDraw.DEMonMap,MyData.RecNo);
      for i := 0 to pred(aShapeFile.CurrentPolyLineHeader.NumPoints) do begin
         Lat := aShapeFile.CurrentLineCoords^[i].Lat;
         Long := aShapeFile.CurrentLineCoords^[i].Long;
         z := aShapeFile.CurrentLineZs^[i];

         if i >0 then begin
            if abs(z-LastZ) < 0.01 then Color := claYellow
            else if z > LastZ then Color := claRed
            else Color := claLime;
         end
         else color := claBlack;
         TheMapOwner.MapDraw.LatLongDegreeToScreen(Lat,Long,x,y);
         Petmar.ScreenSymbol(TheMapOwner.Image1.Canvas,x,y,FilledBox,3,Color);
         LastZ := z;
      end;
   end;
end;

procedure Tdbtablef.ClassBanddiscrimination1Click(Sender: TObject);
{$IfDef ExSat}
begin
{$Else}
begin
   MessageToContinue('currently disabled');
   (*
var
   Band,Class1,Class2 : integer;
   Results,Results2 : tStringList;
   TStr : AnsiString;
   Found : boolean;
   BandDiscriminations : array[1..MaxBands] of integer;
begin
   Results := tStringList.Create;
   for Band := 1 to MaxBands do BandDiscriminations[Band] := 0;
   with GISdb[DBonTable],SatImage[TheMapOwner.MapDraw.SatOnMap],SelectionMap do begin
       for Class1 := 1 to NumClass do begin
          for Class2 := 1 to NumClass do begin
               if Class1 <> Class2 then begin
                   TStr := Classes[Class1].ClassName + '  ' + Classes[Class2].ClassName;
                   Found := false;
                   for Band := 1 to NumBands do begin
                       if (Classes[Class1].ClassLowLimit[Band] >  Classes[Class2].ClassHighLimit[Band]) or
                                (Classes[Class2].ClassLowLimit[Band] > Classes[Class1].ClassHighLimit[Band]) then begin
                           Found := true;
                           inc(BandDiscriminations[Band]);
                           TStr := TStr + IntegerToString(Band,4);
                       end;
                   end;
                   if Found then Results.Add(Tstr);
               end;
          end;
       end;
       Results2 := tStringList.Create;
       for Band := 1 to NumBands do
          if BandDiscriminations[Band] > 0 then Results2.Add(IntegerToString(Band,4) + IntegerToString(BandDiscriminations[Band] div 2, 12));
       Petmar.DisplayAndPurgeStringList(Results,'Class discrimination bands');
       Petmar.DisplayAndPurgeStringList(Results2,'Band discriminations');
   end;
*)
{$EndIf}
end;

procedure Tdbtablef.Clearalldatabasesettings1Click(Sender: TObject);
begin
   GISdb[DBonTable].ProcessDBIniFile(iniInit,'',GISdb[DBonTable].IniFileName);
end;

procedure Tdbtablef.ClearDBsettings1Click(Sender: TObject);
begin
   Clearjoin1Click(Sender);
   Clearalldatabasesettings1Click(Sender);
end;

procedure Tdbtablef.Clearfield1Click(Sender: TObject);
begin
   Fillfieldforallrecords1Click(Sender);
end;

procedure Tdbtablef.Clearfieldranges1Click(Sender: TObject);
begin
   if GISdb[DBonTable].RangeTable <> Nil then begin
      GISdb[DBonTable].RangeTable.Destroy;
      GISdb[DBonTable].SetUpRangeTable(GISdb[DBonTable].dbFullName,GISdb[DBonTable].RangeTable,true)
   end;

   if GISdb[DBonTable].LinkRangeTable <> Nil then begin
      GISdb[DBonTable].LinkRangeTable.Destroy;
      GISdb[DBonTable].SetUpRangeTable(GISdb[DBonTable].dbOpts.LinkTableName,GISdb[DBonTable].LinkRangeTable,true)
   end;
end;

procedure Tdbtablef.Cleargeographicfilter1Click(Sender: TObject);
var
   i : integer;
begin
{$IfDef RecordDataBase} WriteLineRoDebugFile('Tdbtablef.Cleargeographicfilter1Click'); {$EndIf}
   for i := 1 to MaxDataBase do begin
     if (GISdb[i] <> Nil) then begin
        GISdb[i].dbOpts.GeoFilter := '';
        GISdb[i].AssembleGISFilter;
        if (GISdb[i].TheMapOwner <> Nil)and (Sender <> Quickfilter1) then
           GISdb[i].RedrawLayerOnMap;
        if GISdb[i].dbTablef <> Nil then GISdb[i].dbTablef.ShowStatus;
     end;
  end;
end;

procedure Tdbtablef.Clearjoin1Click(Sender: TObject);
begin
   with GISdb[DBonTable] do begin
      if (LinkTable <> Nil) then begin
         LinkTable.Destroy;
         LinkTable := Nil;
         dbOpts.LinkFieldThisDB := '';
         dbOpts.LinkFieldOtherDB := '';
         dbOpts.LinkTableName := '';
      end;
   end;
end;

procedure Tdbtablef.ClosestPointonLinePolygon1Click(Sender: TObject);
begin
   GISdb[DBonTable].DistanceToLinePolygon(true);
end;

procedure Tdbtablef.Cloudsummaries1Click(Sender: TObject);
begin
   CloudSummaryGlobalDEMs(0);
end;



procedure Tdbtablef.Cluster1Click(Sender: TObject);
begin
   {$IfDef NoClustering}
   {$Else}
      DoKMeansClustering(DBonTable);
      UnhideColumns;
   {$EndIf}
end;


procedure Tdbtablef.Clustercomposition1Click(Sender: TObject);
begin
   ClusterCompositionByDBfield(DBonTable);
end;



procedure Tdbtablef.Clusterdiversity1Click(Sender: TObject);
begin
    DEMIX_SSIM_FUV_clusters_diversity_graphs(DBonTable);
end;


procedure Tdbtablef.Clusterfrequency1Click(Sender: TObject);
begin
   ClusterFrequencyForSelectedField(DBonTable);
end;


procedure Tdbtablef.Clusterlegend1Click(Sender: TObject);
{$IfDef ExGeostats}
begin
{$Else}
var
   Cluster,TotalRecs,n : integer;
   Legend : tstringList;
   fName : PathStr;
begin
   GISdb[DBonTable].EmpSource.Enabled := false;
   GISdb[DBonTable].MyData.ApplyFilter('');
   TotalRecs := GISdb[DBonTable].MyData.RecordCount;
   Legend := tstringList.Create;
   Legend.Add('CLUSTER,N,PERCENTAGE,COLOR');
   for Cluster := 1 to MaxClusters do  begin
      GISdb[DBonTable].MyData.ApplyFilter('CLUSTER=' + Cluster.ToString);
      N := GISdb[DBonTable].MyData.RecordCount;
      if N > 0 then begin
         Legend.Add(Cluster.ToString+ ',' + n.ToString + ',' + RealToString(100 * n / TotalRecs,-12,-3) + ',' +
            GISdb[DBonTable].MyData.GetFieldByNameAsInteger('COLOR').toString );
      end;
   end;
   GISdb[DBonTable].MyData.ApplyFilter('');
   fName := Petmar.NextFileNumber(MDTempDir, 'Cluster_legend_',DefaultDBExt);
   GISdb[DBonTable].theMapOwner.StringListToLoadedDatabase(Legend,fName);
{$EndIf}
end;


procedure Tdbtablef.Clustermaplocations1Click(Sender: TObject);
begin
   ClusterMapLocation(DBonTable);
end;

procedure Tdbtablef.Clustermeangraphs1Click(Sender: TObject);
begin
   DEMIX_SSIM_FUV_clusters_graph(DBonTable);
end;

procedure Tdbtablef.Clustersensitivity1Click(Sender: TObject);
begin
   DEMIX_SSIM_FUV_cluster_sensitivity_graph(DBonTable);
end;

procedure Tdbtablef.ClustersperDEMIXtile1Click(Sender: TObject);
begin
   DEMIX_clusters_per_tile(DBonTable);
end;


procedure Tdbtablef.Clusterstatistics1Click(Sender: TObject);
begin
    MakeDBForParamStats(opByCluster,DBonTable);
end;


procedure Tdbtablef.CalculateVolume1Click(Sender: TObject);
var
   i : integer;
begin
   GISdb[DBonTable].theMapOwner.SpeedButton5Click(Nil);
   GISdb[DBonTable].aShapeFile.GetLineCoords(GISdb[DBonTable].MyData.RecNo);
   for i := 0 to pred(GISdb[DBonTable].aShapeFile.CurrentPolyLineHeader.NumPoints) do
      GISdb[DBonTable].theMapOwner.AddLatLongPointToStreamProfile(GISdb[DBonTable].aShapeFile.CurrentLineCoords^[i].Lat,GISdb[DBonTable].aShapeFile.CurrentLineCoords^[i].Long);
   GISdb[DBonTable].theMapOwner.DoneWithStreamSelection;
end;


procedure Tdbtablef.CDSformat1Click(Sender: TObject);
begin
   GISdb[DBonTable].ExportToXML(ChangeFileExt(GISdb[DBonTable].dbFullName,'.CDS'));
   GISdb[DBonTable].dbIsUnsaved := false;
   ShowStatus;
end;


procedure Tdbtablef.Centr1Click(Sender: TObject);
label
   OverNow;
var
   NewGIS : integer;
   RecBitmap,TestMyBitmap : tMyBitmap;
   GridForm : tGridForm;
   LatHi,LongHi,LatLow,LongLow,
   Lat,Long,Lat2,Long2,Dist,Az : float64;
   i,j,k,x,y,MaxNeighbors,rc : integer;
   IDField : ShortString;
   pRec,pTest : pRGB;
   fName,fName2 : PathStr;
   NeighList,FieldsInDB : tStringList;
   TStr,TStr2 : shortString;
   ExtentTable : tMyData;
begin
   {$IfDef FindNeighbors} WriteLineRoDebugFile('Tdbtablef.Centr1Click in'); {$EndIf}

   fName := GISdb[DBonTable].DBAuxDir + 'neigh_' + ExtractFileName(GISdb[DBonTable].dbFullName);
   DeleteFileIfExists(fName);
   fName := ChangeFileExt(fName,'.csv');
   DeleteFileIfExists(fName);
   if (Sender <> Centr1) then begin
       NewGIS := CopyDatabaseAndOpen(GISdb[DBonTable],false);
       {$IfDef FindNeighbors} WriteLineRoDebugFile('Copied and opened'); {$EndIf}
        IDField :=  GISdb[DBonTable].PickField('ID Field' ,[ftString,ftInteger,ftSmallInt]);
        GridForm := tGridForm.Create(Application);
        GridForm.HideCorrelationControls;

        GridForm.StringGrid1.ColCount := succ(GISdb[DBonTable].MyData.RecordCount);
        GridForm.StringGrid1.RowCount := succ(GISdb[DBonTable].MyData.RecordCount);
        GISdb[DBonTable].EmpSource.Enabled := false;
        GISdb[DBonTable].ClearGISFilter;
        GISdb[DBonTable].MyData.First;
        rc := GISdb[DBonTable].MyData.RecordCount;
        if (Sender = Centr1) then begin
          {$IfDef FindNeighbors} WriteLineRoDebugFile('Centroids option'); {$EndIf}
           GridForm.Caption := GISdb[DBonTable].dbName + ' centroid separations';
           StartProgress('Centroids');
           j := 0;
           while not GISdb[DBonTable].MyData.eof do begin
               GISdb[DBonTable].aShapeFile.AreaAndCentroid(GISdb[DBonTable].TheMapOwner.MapDraw.PrimMapProj,GISdb[DBonTable].MyData.RecNo,Lat,Long);
               GISdb[DBonTable].EmpSource.Enabled := false;
               inc(j);
               UpdateProgressBar(j/GISdb[DBonTable].MyData.FiltRecsInDB);
               GridForm.StringGrid1.Cells[j,0] := GISdb[DBonTable].MyData.GetFieldByNameAsString(IDField);
               GridForm.StringGrid1.Cells[0,j] := GISdb[DBonTable].MyData.GetFieldByNameAsString(IDField);;
               for I := GISdb[DBonTable].MyData.RecNo to GISdb[DBonTable].MyData.RecordCount do begin
                  GISdb[NewGIS].aShapeFile.AreaAndCentroid(GISdb[DBonTable].TheMapOwner.MapDraw.PrimMapProj,i,Lat2,Long2);
                  VincentyCalculateDistanceBearing(Lat,Long,Lat2,Long2,Dist,Az);
                  GridForm.StringGrid1.Cells[i,j] := RealToString(0.0001 * Dist,-18,2);
                  GridForm.StringGrid1.Cells[j,i] := RealToString(0.0001 * Dist,-18,2);
               end;
               GISdb[DBonTable].MyData.Next;
           end;
        end
        else begin
          {$IfDef FindNeighbors} WriteLineRoDebugFile('Neighbors option'); {$EndIf}
           with GISdb[DBonTable] do DBFieldUniqueEntries(IDField,FieldsInDB);
           GridForm.Caption := GISdb[DBonTable].dbName + ' neighbors';
           StartProgress('Neighbors phase 1');
           MaxNeighbors := 0;
           fName2 := MDTempDir + 'rec_bounds' + DefaultDBExt;
           CreateFileNameIndexTable(fName2);
           ExtentTable := tMyData.Create(fName2);
           for k := 0 to pred(FieldsInDB.Count) do begin
               GISdb[DBonTable].EmpSource.Enabled := false;
               if (k mod 100 = 0) then begin
                  {$IfDef FindNeighbors} WriteLineRoDebugFile('Phase 1, k=' + IntToStr(k)); {$EndIf}
                  UpdateProgressBar(k/FieldsInDB.Count);
               end;
               CloneImageToBitmap(GISdb[DBonTable].TheMapOwner.Image1,RecBitmap);
               RecBitmap.Canvas.Pen.Color := clBlack;
               RecBitmap.Canvas.Pen.Width := 1;
               RecBitmap.Canvas.Brush.Color := clBlack;
               RecBitmap.Canvas.Brush.Style := bsSolid;
               TStr := FieldsInDB.Strings[k];
               if GISdb[DBonTable].MyData.IsStringField(IDField) then GISdb[DBonTable].MyData.ApplyFilter(IDField + '=' + QuotedStr(TStr))
               else GISdb[DBonTable].MyData.ApplyFilter(IDField + '=' + TStr);

               GISdb[DBonTable].MyData.First;

               NeighList := tStringList.Create;
               NeighList.Sorted := true;
               NeighList.Duplicates := dupIgnore;
               InitLatLongBoundBox(LatLow,LongLow,LatHi,LongHi);


               while not GISdb[DBonTable].MyData.eof do begin
                  with GISdb[DBonTable] do aShapeFile.PlotSingleRecordMap(TheMapOwner.MapDraw,RecBitmap,GISdb[DBonTable].MyData.RecNo);
                  GISdb[DBonTable].MyData.Next;
                  Lat := GISdb[DBonTable].MyData.GetFieldByNameAsFloat('LAT_HI');
                  Long := GISdb[DBonTable].MyData.GetFieldByNameAsFloat('LONG_LOW');
                  Petmath.CompareValueToExtremes(Lat,LatLow,LatHi);
                  Petmath.CompareValueToExtremes(Long,LongLow,LongHi);
                  Lat := GISdb[DBonTable].MyData.GetFieldByNameAsFloat('LAT_LOW');
                  Long := GISdb[DBonTable].MyData.GetFieldByNameAsFloat('LONG_HI');
                  Petmath.CompareValueToExtremes(Lat,LatLow,LatHi);
                  Petmath.CompareValueToExtremes(Long,LongLow,LongHi);
               end;
               PetImage.SaveBitmap(RecBitmap,MDTempDir + 'rec_' + TStr + '.gif');
               RecBitmap.Free;
               ExtentTable.Insert;
               ExtentTable.SetFieldByNameAsString('FILENAME',tStr);
               ExtentTable.SetFieldByNameAsFloat('LAT_HI',LatHi);
               ExtentTable.SetFieldByNameAsFloat('LAT_LOW',LatLow);
               ExtentTable.SetFieldByNameAsFloat('LONG_HI',LongHi);
               ExtentTable.SetFieldByNameAsFloat('LONG_LOW',LongLow);
               ExtentTable.Post;
           end;

           StartProgress('Neighbors phase 2');
           for k := 0 to pred(FieldsInDB.Count) do begin
               if (k mod 100 = 0) then begin
                  {$IfDef FindNeighbors} WriteLineRoDebugFile('Phase 2, k=' + IntToStr(k)); {$EndIf}
                  UpdateProgressBar(k/FieldsInDB.Count);
               end;
               TStr := FieldsInDB.Strings[k];
               ExtentTable.ApplyFilter('FILENAME=' + QuotedStr(TStr));
               LatHi := ExtentTable.GetFieldByNameAsFloat('LAT_HI');
               LatLow := ExtentTable.GetFieldByNameAsFloat('LAT_LOW');
               LongHi := ExtentTable.GetFieldByNameAsFloat('LONG_HI');
               LongLow := ExtentTable.GetFieldByNameAsFloat('LONG_LOW');

               RecBitmap := PetImage.LoadBitmapFromFile(MDTempDir + 'rec_' + TStr + '.gif');
               ExtentTable.ApplyFilter( PetDBUtils.MakeGeoFilterFromCorners(LatHi,LongLow,LatLow,LongHi));
               GridForm.StringGrid1.Cells[0,succ(k)] := TStr;
               //i := 0;
               NeighList := tStringList.Create;
               while not ExtentTable.eof do  begin
                  TStr2 := ExtentTable.GetFieldByNameAsString('FILENAME');
                  TestMyBitmap := Nil;
                  if (TStr <> TStr2) then  begin
                     TestMyBitmap := PetImage.LoadBitmapFromFile(MDTempDir + 'rec_' + TStr2 + '.gif');
                     for y := 0 to pred(RecBitmap.Height) do  begin
                        pRec := RecBitmap.ScanLine[y];
                        pTest := TestMyBitmap.ScanLine[y];
                        for x := 0 to pred(RecBitmap.Width) do  begin
                           if SameColor(pRec[x],RGBTripleBlack) and SameColor(pTest[x],RGBTripleBlack) then  begin
                              //inc(i);
                              NeighList.Add(TStr2);
                              goto OverNow;
                           end;
                        end;
                     end;
                  end;
                 OverNow:;
                  if (TestMyBitmap <> Nil) then TestMyBitmap.Free;
                  ExtentTable.Next;
               end;

               GridForm.StringGrid1.Cells[1,succ(k)] := IntToStr(NeighList.Count);
               if (NeighList.Count > 0) then begin
                  for i := 1 to NeighList.Count do GridForm.StringGrid1.Cells[(i+1),succ(k)] := NeighList.Strings[pred(i)];
                  if (NeighList.Count > MaxNeighbors) then MaxNeighbors := NeighList.Count;
               end;
               NeighList.Free;
               RecBitmap.Free;
           end;
           ExtentTable.Destroy;

           GridForm.StringGrid1.ColCount := (MaxNeighbors+2);
           GridForm.StringGrid1.RowCount := succ(FieldsInDB.Count);
           GridForm.StringGrid1.Cells[0,0] := IDField;
           GridForm.StringGrid1.Cells[1,0] := 'Num_Neigh';
           {$IfDef FindNeighbors} WriteLineRoDebugFile('Tdbtablef.Centr1Click MaxNeighbors=' + IntToStr(MaxNeighbors)); {$EndIf}

           for i := 1 to MaxNeighbors do GridForm.StringGrid1.Cells[succ(i),0] := 'Neigh_' + IntToStr(i);
           fName := 'neigh_' + ExtractFileName(GISdb[DBonTable].dbFullName);
           fName := GISdb[DBonTable].DBAuxDir + ChangeFileExt(fName,'.csv');
           {$IfDef FindNeighbors} WriteLineRoDebugFile('Tdbtablef.Centr1Click call StringGridToCSVFile ' + fName); {$EndIf}
           StringGridToCSVFile(fName,GridForm.StringGrid1,Nil);
           CSVFileImportToDB(fName);

           fName := ChangeFileExt(fName,DefaultDBExt);
           GISdb[DBonTable].NeighborTable := tMyData.Create(fName);
           GISdb[DBonTable].NeighborLinkField := GISdb[DBonTable].NeighborTable.GetFieldName(0);
        end;

        ShowStatus;
        GISdb[DBonTable].ClearGISFilter;
        CloseAndNilNumberedDB(NewGIS);
   end;
   {$IfDef FindNeighbors} WriteLineRoDebugFile('Tdbtablef.Centr1Click out'); {$EndIf}
end;


procedure Tdbtablef.Find1Click(Sender: TObject);
begin
   if FindDialog1.Execute then begin
     //MyData.First;
   end;
end;

procedure Tdbtablef.FindAddress1Click(Sender: TObject);
begin
   StartTigerAddress(DBonTable);
end;

procedure Tdbtablef.FindDialog1Find(Sender: TObject);
label
   Found;
var
   aString : AnsiString;
   Done : boolean;
begin
   with GISdb[DBonTable] do begin
      EmpSource.Enabled := false;
      GISdb[DBonTable].MyData.Next;
      repeat
         while not GISdb[DBonTable].MyData.eof do begin
            aString := GISdb[DBonTable].MyData.GetFieldByNameAsString(SelectedColumn);
            if AnsiContainsText(aString,FindDialog1.FindText) then goto Found;
            GISdb[DBonTable].MyData.Next;
         end;
         Done := not AnswerIsYes('Search from beginning of database');
         if not Done then GISdb[DBonTable].MyData.First;
      until Done;
    Found:;
     ShowStatus;
   end;
end;

procedure Tdbtablef.Findneighbors1Click(Sender: TObject);
var
   Lat,Long : float64;
   DistanceBuffer,
   i,j,NumNeighbors,rc  : integer;
   NewField : ShortString;
   Distance,Bearing : float64;
begin
   with GISdb[DBonTable] do begin
      if ItsAPointDB then begin
         DistanceBuffer := 500;
         ReadDefault('Buffer (m)',DistanceBuffer);
         NewField := 'NEIGH_' + IntToStr(DistanceBuffer);
         NewField := GetFieldNameForDB('Number neighbors field name',True,NewField);
         AddFieldToDataBase(ftInteger,NewField,5,0);
         LoadPointCoords;
         EmpSource.Enabled := false;
         i := 0;
         GISdb[DBonTable].MyData.First;
         rc := GISdb[DBonTable].MyData.RecordCount;
         while not GISdb[DBonTable].MyData.Eof do begin
             if ValidLatLongFromTable(Lat,Long) then  begin
                NumNeighbors := 1;   //point itself counts as 1
                for j := 0 to pred(rc) do begin
                   if (i <> j) then begin
                     VincentyCalculateDistanceBearing(Lat,Long,PointsInMemory^[j].Lat,PointsInMemory^[j].Long,Distance,Bearing);
                     if (Distance <= DistanceBuffer) then inc(NumNeighbors);
                   end;
                end;
                GISdb[DBonTable].MyData.Edit;
                GISdb[DBonTable].MyData.SetFieldByNameAsInteger(NewField,NumNeighbors);
                GISdb[DBonTable].MyData.Next;
             end;
             inc(i);
         end;
         DisposePointCoords;
         ShowStatus;
      end
      else begin
         GISdb[DBonTable].AddBoundingBox;
         Centr1Click(Sender);
      end;
   end;
end;


procedure Tdbtablef.Findneighborsinseconddatabase1Click(Sender: TObject);
var
  rc,Neighbors,Count : integer;
  NewField : ShortString;
  BoxSize,Lat,Long,Lat2,Long2,LatSize,LongSize : float64;
begin
   with GISdb[DBonTable] do begin
      Neighbors := PickOpenGISDataBase('Find neighbors',DBonTable);
      if (Neighbors <> 0) then begin
         BoxSize := 25;
         ReadDefault('Box radius (m)',BoxSize);
         NewField := 'NEIGH';
         NewField := GetFieldNameForDB('Number neighbors field name',True,NewField);
         AddFieldToDataBase(ftInteger,NewField,8,0);
         GISdb[DBonTable].EmpSource.Enabled := false;
         GISdb[Neighbors].EmpSource.Enabled := false;

         StartProgress('Find neighbors');
         Count := 0;
         GISdb[DBonTable].MyData.First;
         rc := GISdb[DBonTable].MyData.RecordCount;
         while not GISdb[DBonTable].MyData.eof do begin
            inc(Count);
            if (Count mod 10 = 0) then begin
               UpdateProgressBar(Count/rc);
            end;
            if GISdb[DBonTable].ValidLatLongFromTable(Lat,Long) then begin
               VincentyPointAtDistanceBearing(Lat,Long,BoxSize,0,Lat2,Long2);
               LatSize := abs(Lat2-Lat);
               VincentyPointAtDistanceBearing(Lat,Long,BoxSize,90,Lat2,Long2);
               LongSize := abs(Long2-Long);
               GISdb[Neighbors].MyData.ApplyFilter(PetDBUtils.MakePointGeoFilter(GISdb[Neighbors].LatFieldName,GISdb[Neighbors].LongFieldName,Lat+LatSize,Long-LongSize,Lat-LatSize,Long+LongSize));
               GISdb[DBonTable].MyData.Edit;
               GISdb[DBonTable].MyData.SetFieldByNameAsInteger(NewField,GISdb[Neighbors].MyData.RecordCount);
               GISdb[DBonTable].MyData.Post;
            end;
            GISdb[DBonTable].MyData.Next;
         end;
         GISdb[Neighbors].ClearGISFilter;
         ShowStatus;
      end;
   end;
end;

procedure Tdbtablef.Findpointsinareas1Click(Sender: TObject);
begin
   MaskDatabaseWithShapefile1Click(Sender);
end;


procedure Tdbtablef.FindrecordsonDEM1Click(Sender: TObject);
var
   TheField : shortstring;
begin
   {$IfDef RecordOnDEM} WriteLineRoDebugFile('Tdbtablef.FindrecordsonDEM1Click in'); {$EndIf}
   with GISdb[DBonTable],MyData do begin
      TheField := PickField('Name' ,[ftString]);
      EmpSource.Enabled := false;
      MarkRecordsOnDEM(TheField,TheMapOwner.MapDraw.DEMonMap);
      ShowStatus;
   end;
end;


procedure Tdbtablef.Firstandlastpoints1Click(Sender: TObject);
begin
  GISdb[DBonTable].AddGeometry(agEndPoints);
end;


procedure Tdbtablef.CreateXYZpointshapefile1Click(Sender: TObject);
begin
   GISdb[DBonTable].SavePointShapeFile;
end;

procedure Tdbtablef.CriteriaforeachDEMIXtile1Click(Sender: TObject);
begin
   InventoryCriteriaEachDEMIXtile(DBonTable);
end;

procedure Tdbtablef.Principalcomponents1Click(Sender: TObject);
{$IfDef ExGeostats}
begin
{$Else}
var
   UseFields : tStringList;
begin
   UseFields := GISdb[DBonTable].GetAnalysisFields;
   GISdb[DBonTable].EmpSource.Enabled := false;
   DEMStat.ComputeVarCoVarAndPrincipalComponents(DBonTable,Nil,UseFields);
   UseFields.Free;
   ShowStatus;
   GISdb[DBonTable].TheMapOwner.OpenDBonMap('',MDTempDir + 'pc' + DefaultDBExt);
{$EndIf}
end;


procedure Tdbtablef.ProfileallDEMs1Click(Sender: TObject);
begin
   LOStopoprofile1Click(Sender);
end;


procedure Tdbtablef.Proportionalsquares1Click(Sender: TObject);
begin
   {$IfDef ExAdvancedGIS}
   {$Else}
      GISdb[DBonTable].GISProportionalSymbols(dbasTTFontSymbol);
   {$EndIf}
end;


procedure Tdbtablef.PutpointsonDEMgridlocations1Click(Sender: TObject);
var
   Lat,Long : float64;
   xg,yg : float32;
begin
   GISdb[DBonTable].EmpSource.Enabled := false;
   ShowHourglassCursor;
   GISdb[DBonTable].MyData.First;
   while not GISdb[DBonTable].MyData.eof do begin
      if GISdb[DBonTable].ValidLatLongFromTable(Lat,Long) then begin
         DEMGlb[GISdb[DBonTable].TheMapOwner.MapDraw.DEMonMap].LatLongDegreeToDEMGrid(Lat,Long,xg,yg);
         DEMGlb[GISdb[DBonTable].TheMapOwner.MapDraw.DEMonMap].DEMGridToLatLongDegree(round(xg),round(yg),Lat,Long);
         GISdb[DBonTable].MyData.Edit;
         GISdb[DBonTable].MyData.SetFieldByNameAsFloat(GISdb[DBonTable].LatFieldName,Lat);
         GISdb[DBonTable].MyData.SetFieldByNameAsFloat(GISdb[DBonTable].LongFieldName,Long);
      end;
      GISdb[DBonTable].Mydata.Next;
   end;
   ShowStatus;
end;

procedure Tdbtablef.QuartilesinCLUSTERfieldbasedonsort1Click(Sender: TObject);
begin
   GISdb[DBonTable].PutInQuartilesBasedOnExistingSort;
end;

procedure Tdbtablef.Quickfilter1Click(Sender: TObject);
begin
   GISdb[DBonTable].DisplayTable(SelectedColumn);
end;


procedure Tdbtablef.Quickfiltering2Click(Sender: TObject);
begin
   GISdb[DBonTable].ClearGISFilter;
   GISdb[DBonTable].GISProportionalSymbols(GISdb[DBonTable].dbOpts.dbAutoShow);
   GISdb[DBonTable].gis_scaled_form.CheckBox17.Checked := true;
end;

procedure Tdbtablef.Quickfilters1Click(Sender: TObject);
begin
   Quickfiltering2Click(Sender);
end;

procedure Tdbtablef.Quickfilters2Click(Sender: TObject);
begin
   GISdb[DBonTable].GISProportionalSymbols(GISdb[DBonTable].dbOpts.dbAutoShow);
   GISdb[DBonTable].gis_scaled_form.CheckBox17.Checked := true;
end;

procedure Tdbtablef.QuickKMLexport1Click(Sender: TObject);
begin
   AddBeachBallIcons1Click(Sender);
end;

procedure Tdbtablef.QuickKMLexport2Click(Sender: TObject);
begin
   AddBeachBallIcons1Click(Sender);
end;

procedure Tdbtablef.Quotienttwofields1Click(Sender: TObject);
begin
   {$IfDef RecordEditsDone} WriteLineRoDebugFile('Quotient two fields (ratio)'); {$EndIf}
   Sumtwofields1Click(Sender);
end;


procedure Tdbtablef.raceroute1Click(Sender: TObject);
begin
   TraceRoute3D(DBonTable);
end;


procedure Tdbtablef.Trainingclassavailable1Click(Sender: TObject);
begin
   Trainingclassavailable1.Checked := not Trainingclassavailable1.Checked;
   TrainingClassAvailable := Trainingclassavailable1.Checked;
end;


procedure Tdbtablef.BitBtn6Click(Sender: TObject);
var
   ViewshedSummary : tStringList;
   fName : PathStr;
begin
   {$IfDef RecordFan} WriteLineRoDebugFile('Tdbtablef.BitBtn-6Click in)'); {$EndIf}
   GISdb[DBonTable].FilterDBByUseAndDisable(true);

   if (Sender = BitBtn14) or (Sender = Nil) then begin
      {$IfDef RecordFan} WriteLineRoDebugFile('composite layer, filter=' + GISdb[DBonTable].MyData.Filter); {$EndIf}
      GISdb[DBonTable].TheMapOwner.MapDraw.DeleteSingleMapLayer(GISdb[DBonTable].TheMapOwner.MapDraw.AllFansCoverageFName);
      GISdb[DBonTable].TheMapOwner.MapDraw.AllFansCoverageFName := MDTempDir +  'all_fans' + OverlayFExt;
   end
   else begin
      {$IfDef RecordFan} WriteLineRoDebugFile('number covering, filter=' + GISdb[DBonTable].MyData.Filter); {$EndIf}
      ViewshedSummary := tStringList.Create;
      GISdb[DBonTable].TheMapOwner.MapDraw.ComputeMultiSensorCoverage(GISdb[DBonTable].TheMapOwner.MapDraw.AllFansCoverageFName,GISdb[DBonTable].MyData,ViewShedSummary);
      fName := MDTempDir + 'Viewshed_multiple_coverage.csv';
      StringList2CSVtoDB(ViewshedSummary,fName);
   end;
   AddOrSubtractOverlay(GISdb[DBonTable].TheMapOwner,ovoFans,true);
   GISdb[DBonTable].TheMapOwner.DoFastMapRedraw;
   ShowStatus;
   {$IfDef RecordFan} WriteLineRoDebugFile('Tdbtablef.BitBtn-6Click out,AllFansCoverageFName=' + GISdb[DBonTable].TheMapOwner.MapDraw.AllFansCoverageFName); {$EndIf}
end;


procedure Tdbtablef.BitBtn14Click(Sender: TObject);
begin
   {$IfDef RecordFan} WriteLineRoDebugFile('Tdbtablef.BitBtn-14Click in'); {$EndIf}
   BitBtn6Click(Sender);
end;

procedure Tdbtablef.BitBtn15Click(Sender: TObject);
begin
   {$IfDef RecordFan} WriteLineRoDebugFile('Tdbtablef.BitBtn15Click in'); {$EndIf}
   Requiredantennaheight1Click(Sender);
end;


procedure Tdbtablef.BitBtn16Click(Sender: TObject);
begin
   HighlightFan(claRed);
end;


procedure Tdbtablef.BitBtn17Click(Sender: TObject);
begin
   PopUpMenu10.Popup(Mouse.CursorPos.X,Mouse.CursorPos.Y);
end;


procedure FindMaskCoverage(bm1,bm2 : tMyBitmap; PixelSize : float64; var TotalCoverage,Area : float64);
var
   Total,Covered,
   x,y : integer;
   p1,p2 : pRGB;
begin
   Total := 0;
   Covered := 0;
   for y := 0 to pred(bm1.Height) do begin
      p1 := bm1.ScanLine[y];
      p2 := bm2.ScanLine[y];
      for x := 0 to pred(bm1.Width) do begin
         if not SameColor(p1[x],RGBTripleWhite) then begin
            inc(Total);
            if not SameColor(p2[x],RGBTripleWhite) then inc(Covered);
         end;
      end;
   end;
   TotalCoverage := 100*Covered/Total;
   Area := Total * sqr(PixelSize);
end;


procedure Tdbtablef.BitBtn18Click(Sender: TObject);
var
   Target : integer;
begin
   {$IfDef RecordFan} WriteLineRoDebugFile('Tdbtablef.BitBtn18Click'); {$EndIf}
   Target := PickOpenGISDataBase('Targets for Fan coverage',DBonTable);
   if (Target = -99) then MessageToContinue('Open desired target area shapefile')
   else ViewshedTargetCoverage(Target);
end;



procedure Tdbtablef.ViewshedTargetCoverage(Target : integer; fName : PathStr = '');
var
   VisTargets,db : integer;
   bm1,bm2 : tMyBitmap;
   wf : tWeaponsFan;
   Lat,Long,distance,Bearing,BlockDistance,
   Area,ThisCoverage,TotalCoverage : float64;
   Answer : string;
   MyTable : tMyData;
begin
   {$IfDef RecordFan} WriteLineRoDebugFile('Tdbtablef.ViewshedTargetCoverage, Sensors filter=' + GISDB[Target].MyData.Filter); {$EndIf}
   if (Target <> 0) and (Target <> DBonTable) then with GISdb[DBonTable] do begin
      GISdb[DBonTable].MyData.ApplyFilter('USE=' + QuotedStr('Y'));
      TheMapOwner.DoFastMapRedraw;
      if GISdb[Target].ItsAPointDB then begin
         {$IfDef RecordFan} WriteLineRoDebugFile('Points targets'); {$EndIf}
         GISdb[DBonTable].EmpSource.Enabled := false;
         GISdb[Target].EmpSource.Enabled := false;
         if (fName = '') then fName := NextFileNumber(MDTempDir, 'sensor_target',DefaultDBExt);
         MakeSensorTargetTable(fName,false,36);
         MyTable := tMyData.Create(fName);
         GISdb[DBonTable].MyData.First;
         while not GISdb[DBonTable].MyData.eof do begin
            wF := WeaponsTableBasicParametersToFan(TheMapOwner.MapDraw.PrimMapProj,MyData);
            VisTargets := 0;
            GISdb[Target].MyData.First;
            while not GISdb[Target].MyData.eof  do begin
               if GISdb[Target].ValidLatLongFromTable(Lat,Long) then begin
                  VincentyCalculateDistanceBearing(wf.W_Lat,wf.W_Long,Lat,Long,distance,Bearing);
                  if Distance <= wf.W_Range then begin
                     if DEMGlb[TheMapOwner.MapDraw.DEMonMap].LatLongDegreePointsIntervisible(wf.W_Lat,wf.W_Long,wf.W_Up,Lat,Long,wf.W_TargetUp,Distance,BlockDistance) then begin
                        inc(VisTargets);
                        MyTable.Insert;
                        MyTable.SetFieldByNameAsFloat('LAT',wf.W_Lat);
                        MyTable.SetFieldByNameAsFloat('LONG',wf.W_Long);
                        MyTable.SetFieldByNameAsFloat('LAT2',Lat);
                        MyTable.SetFieldByNameAsFloat('LONG2',Long);
                        MyTable.SetFieldByNameAsString('SENSOR',wf.Fan_Name);
                        MyTable.SetFieldByNameAsString('TARGET',GISdb[Target].MyData.GetFieldByNameAsString('NAME'));
                        MyTable.Post;
                     end;
                  end;
               end;
               GISdb[Target].MyData.Next;
            end;
            GISdb[DBonTable].MyData.Next;
         end;
         GISdb[Target].EmpSource.Enabled := true;
         MyTable.Destroy;
         db := TheMapOwner.LoadDataBaseFile(fName);
         GISdb[db].dbOpts.DBAutoShow := dbasConnectTwoPointsInRec;
         GISdb[db].dbOpts.LineColor := MDDef.ConPtsColor;
         GISdb[db].dbOpts.LineWidth := MDDef.ConPtsWidth;
         GISdb[db].RedrawLayerOnMap;
         ShowStatus;
      end
      else begin
         {$IfDef RecordFan} WriteLineRoDebugFile('Line/area targets'); {$EndIf}
          if (TheMapOwner.MapDraw.DBOverlayfName[Target] <> '') and (TheMapOwner.MapDraw.AllFansCoverageFName <> '') then begin
             bm1 := PetImage.LoadBitmapFromFile(TheMapOwner.MapDraw.DBOverlayfName[Target]);
             bm2 := PetImage.LoadBitmapFromFile(TheMapOwner.MapDraw.AllFansCoverageFName);
             FindMaskCoverage(bm1,bm2,TheMapOwner.MapDraw.ScreenPixelSize,TotalCoverage,Area);
             bm2.Free;
             Answer := 'Sensors: ' + IntToStr(MyData.RecordCount) + MessLineBreak;
             GISdb[DBonTable].MyData.First;
             while not GISdb[DBonTable].MyData.eof do begin
                fName := GISdb[DBonTable].MyData.GetFieldByNameAsString('IMAGE');
                ThisCoverage := 0;
                if FileExists(fName) then begin
                   TheMapOwner.MapDraw.StretchWorldFileMap(Bm2,fName);
                   FindMaskCoverage(bm1,bm2,TheMapOwner.MapDraw.ScreenPixelSize,ThisCoverage,Area);
                   FreeAndNil(Bm2);
                end;
                GISdb[DBonTable].MyData.Edit;
                GISdb[DBonTable].MyData.SetFieldByNameAsFloat('TARGET',ThisCoverage);
                Answer := Answer + GISdb[DBonTable].MyData.GetFieldByNameAsString('NAME') + '  ' + RealToString(ThisCoverage,5,2) + '%  ' +
                    LatLongDegreeToString(MyData.GetFieldByNameAsFloat('LAT'),MyData.GetFieldByNameAsFloat('LONG'),MDDef.OutPutLatLongMethod) +
                    '  Range: ' + RealToString(MyData.GetFieldByNameAsFloat('MIN_RNG'),-6,0) + '--' + RealToString(MyData.GetFieldByNameAsFloat('SENSOR_RNG'),-8,0) + ' m' +
                    '  Ht: ' + RealToString(MyData.GetFieldByNameAsFloat('SENSOR_UP'),-6,0) + ' m' +
                    '  Az: ' + RealToString(MyData.GetFieldByNameAsFloat('LEFT_AZ'),-6,0) + '--' + RealToString(MyData.GetFieldByNameAsFloat('RIGHT_AZ'),-8,0) + DegSym + MessLineBreak;
                GISdb[DBonTable].MyData.Next;
             end;
             MessageToContinue(Answer + MessLineBreak + MessLineBreak + 'Target area: ' + SmartAreaFormat(Area) + MessLineBreak + 'Coverage: ' + RealToString(TotalCoverage,-12,2) + '%',True);
             bm1.Free;
         end
         else MessageToContinue('Map overlays missing; redraw');
      end;
   end;
end;

procedure Tdbtablef.BitBtn19Click(Sender: TObject);
begin
   DBFfile1Click(Sender);
end;

procedure Tdbtablef.HighlightFan(inColor : tPlatformColor);
var
   fName : PathStr;
   Bitmap,bitmap2 : tMyBitmap;
begin
   fName := GISdb[DBonTable].MyData.GetFieldByNameAsString('IMAGE');
   if FileExists(fName) then begin
      GISdb[DBonTable].TheMapOwner.MapDraw.StretchWorldFileMap(Bitmap,fName);
      RecolorFan(Bitmap,inColor);
      GISdb[DBonTable].theMapOwner.DoFastMapRedraw;
      CopyImageToBitmap(GISdb[DBonTable].theMapOwner.Image1,Bitmap2);
      DrawAndDeleteOverlay(Bitmap2,Bitmap,MDDef.FanOpacity);
      GISdb[DBonTable].theMapOwner.Image1.Picture.Graphic := Bitmap2;
      FreeAndNil(Bitmap2);
   end;
end;

procedure Tdbtablef.Highlightfan1Click(Sender: TObject);
begin
   HighlightFan(claRed);
end;

procedure Tdbtablef.Distancealgorithmcomparison1Click(Sender: TObject);
begin
   Distancefrompoint1Click(Sender)
end;

procedure Tdbtablef.Distanceazimuthfromxycomponents1Click(Sender: TObject);
var
   xf,yf : ShortString;
   x,y,dist,az : float64;
begin
   with GISdb[DBonTable] do begin
      xf := PickField('x component',NumericFieldTypes);
      yf := PickField('y component',NumericFieldTypes);
      AddFieldToDataBase(ftFloat,'DIST_M',12,3);
      AddFieldToDataBase(ftFloat,'AZIMUTH',6,1);
      EmpSource.Enabled := false;
      GISdb[DBonTable].MyData.First;
      while not GISdb[DBonTable].MyData.Eof do begin
         if GISdb[DBonTable].MyData.CarefullyGetFieldByNameAsFloat64(xf,x) and GISdb[DBonTable].MyData.CarefullyGetFieldByNameAsFloat64(yf,y) then begin
            GISdb[DBonTable].MyData.Edit;
            Dist := sqrt(x*x+y*y);
            GISdb[DBonTable].MyData.SetFieldByNameAsFloat('DIST_M',Dist);
            if Dist > 0.00001 then begin
               az := HeadingOfLine(x,y);
               GISdb[DBonTable].MyData.SetFieldByNameAsFloat('AZIMUTH',Az);
            end;
         end;
         GISdb[DBonTable].MyData.Next;
      end;
   end;
   ShowStatus;
end;

procedure Tdbtablef.Distancebetweentwopointsinrecord1Click(Sender: TObject);
begin
   AddDISTANCEAZIMUTHfields1Click(Sender);
end;

procedure Tdbtablef.Distancefrompoint1Click(Sender: TObject);
var
   Lat,Long : float64;
   Algs : integer;
   fName : PathStr;
   GISNum : integer;
   Start : shortstring;
begin
   Algs := 1;
   if (Sender = Distancetopointsintable1) then begin
      if Petmar.GetFileFromDirectory('Starting points',DBNameMask,fName) then begin
         if OpenNumberedGISDataBase(GISNum,fName) then begin
            while not GISdb[GISNum].MyData.eof do  begin
                if GISDB[GISNum].ValidLatLongFromTable(Lat,Long) then begin
                    Start := GISdb[GISNum].MyData.GetFieldByNameAsString('NAME');
                    GISdb[DBonTable].DistAllRecsToPoint(Lat,Long,Algs,Start,2);
                end;
                GISdb[GISNum].MyData.Next;
            end;
            CloseAndNilNumberedDB(GISNum);
         end;
      end;
   end
   else begin
      if (Sender = DistanceToOtherrecords1) then GISdb[DBonTable].ValidLatLongFromTable(Lat,Long)
      else GetLatLongNoDatum('Point for distance computations',Lat,Long);
      if (Sender = Distancealgorithmcomparison1) then Algs := 3;
      GISdb[DBonTable].DistAllRecsToPoint(Lat,Long,Algs);
   end;
end;


procedure Tdbtablef.Distanceoffmap1Click(Sender: TObject);
var
   Lat,Long : float64;
begin
   GISdb[DBonTable].AddFieldToDataBase(ftString,'DIST_OFF',35);
   ShowHourglassCursor;
   GISdb[DBonTable].MyData.First;
   while not GISdb[DBonTable].MyData.eof do begin
      if GISdb[DBonTable].ValidLatLongFromTable(Lat,Long) then begin
         GISdb[DBonTable].MyData.Edit;
         GISdb[DBonTable].MyData.SetFieldByNameAsString('DIST_OFF',GISdb[DBonTable].TheMapOwner.MapDraw.DistanceOffMap(Lat,Long));
      end;
      GISdb[DBonTable].MyData.Next;
   end;
   ShowStatus;
end;

procedure Tdbtablef.Distancetolinepolygon1Click(Sender: TObject);
begin
   GISdb[DBonTable].DistanceToLinePolygon(false);
end;

procedure Tdbtablef.Distancetonearestneighbor1Click(Sender: TObject);
var
   Table2 : tMyData;
   fName : PathStr;
   i,rc : integer;
   Lat,Long,Lat2,Long2,MinDist,Dist,Az,Bitty : float64;
begin
   fName := MDTempDir + 'temp_db_dist.dbf';
   Petmar.CopyFile(GISdb[DBonTable].dbFullName,fName);
   Table2 := tMyData.Create(fName);
   GISdb[DBonTable].AddFieldToDataBase(ftFloat,'DIST_NEIGH',12,2);
   GISdb[DBonTable].MyData.First;
   i := 0;
   rc := GISdb[DBonTable].MyData.RecordCount;
   StartProgress('Distances');
   while not GISdb[DBonTable].MyData.eof do begin
      if (i mod 25 = 0) then begin
         GISdb[DBonTable].EmpSource.Enabled := false;
         UpdateProgressBar(i/rc);
      end;
      inc(i);
      if GISdb[DBonTable].ValidLatLongFromTable(Lat,Long) then begin
         MinDist := 23e23;
         Bitty := 0.00001;
         repeat
            Table2.ApplyFilter(PointVeryCloseGeoFilter('LAT','LONG',Lat,Long));
            Bitty := 2 * Bitty;
         until (Table2.RecordCount > 1);
         while not Table2.eof do begin
            Lat2 := Table2.GetFieldByNameAsFloat('LAT');
            Long2 := Table2.GetFieldByNameAsFloat('LONG');
            VincentyCalculateDistanceBearing(Lat,Long,Lat2,Long2,Dist,Az);
            if (Dist > 0.1) and (Dist < MinDist) then MinDist := Dist;
            Table2.Next;
         end;
         GISdb[DBonTable].MyData.Edit;
         GISdb[DBonTable].MyData.SetFieldByNameAsFloat('DIST_NEIGH',MinDist);
      end;
      GISdb[DBonTable].MyData.Next;
   end;
   Table2.Destroy;
   ShowStatus;
end;

procedure Tdbtablef.Distancetootherrecords1Click(Sender: TObject);
begin
   Distancefrompoint1Click(Sender);
end;


procedure Tdbtablef.Distancetopointsintable1Click(Sender: TObject);
begin
   Distancefrompoint1Click(Sender);
end;

procedure Tdbtablef.Distributionsummary1Click(Sender: TObject);
begin
   DirtAndAirShots(0,'');
end;


procedure Tdbtablef.DistributionsummarythisDB1Click(Sender: TObject);
begin
   DirtAndAirShots(DBonTable,'');
end;

procedure Tdbtablef.DistributionhistogramsthisDB1Click(Sender: TObject);
begin
   HistogramPointCloudAndGlobalDEMs(DBonTable);
end;

procedure Tdbtablef.Distributionsummary(Title : shortstring);
begin
end;


procedure Tdbtablef.Dividefieldbyconstant1Click(Sender: TObject);
begin
   SingleFieldArithmetic(DBonTable,sfaDiv,'');
end;

procedure Tdbtablef.Dividefieldbyconstant2Click(Sender: TObject);
begin
   SingleFieldArithmetic(DBonTable,sfaDiv,SelectedColumn);
end;

procedure Tdbtablef.Dividethreefields1Click(Sender: TObject);
begin
    Keepbefore1Click(Sender);
end;

procedure Tdbtablef.Dividetwofields1Click(Sender: TObject);
begin
    Keepbefore1Click(Sender);
end;

procedure Tdbtablef.Shapefilemetadata1Click(Sender: TObject);
begin
   ShapeFileDump(GISdb[DBonTable].dbFullName);
end;

procedure Tdbtablef.Shapenumber1Click(Sender: TObject);
begin
   GISdb[DBonTable].AddGeometry(agShapeNum);
end;

procedure Tdbtablef.Shiftpointrecords1Click(Sender: TObject);
var
   Dist,Azimuth,Lat,Long : float64;
begin
   //with GISdb[DBonTable] do begin
      if (not GISdb[DBonTable].MyData.Filtered) or AnswerIsYes('Apply only to filtered records') then begin
         Dist := 300;
         Azimuth := 270;
         ReadDefault('Distance (m)',Dist);
         ReadDefault('Azimuth ()',Azimuth);
         GISdb[DBonTable].EmpSource.Enabled := false;
         GISdb[DBonTable].MyData.First;
         while not GISdb[DBonTable].MyData.Eof do begin
            if GISdb[DBonTable].ValidLatLongFromTable(Lat,Long) then begin
               Lat := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(GISdb[DBonTable].LatFieldName);
               Long := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(GISdb[DBonTable].LongFieldName);
               VincentyPointAtDistanceBearing(Lat,Long,Dist,Azimuth,Lat,Long);
               GISdb[DBonTable].MyData.Edit;
               GISdb[DBonTable].MyData.SetFieldByNameAsFloat(GISdb[DBonTable].LatFieldName,Lat);
               GISdb[DBonTable].MyData.SetFieldByNameAsFloat(GISdb[DBonTable].LongFieldName,Long);
            end;
            GISdb[DBonTable].MyData.Next;
         end;
         ShowStatus;
      end;
   //end;
end;

procedure Tdbtablef.Elevationchangeofeachrecord1Click(Sender: TObject);
begin
  GISdb[DBonTable].AddGeometry(agElevationDeltas);
end;


procedure Tdbtablef.Elevationslopeplots1Click(Sender: TObject);
begin
   ElevationSlopePlotCompareDEMs;
end;

procedure Tdbtablef.Enablealloptions1Click(Sender: TObject);
begin
   AllOptionsForFans := true;
end;

procedure Tdbtablef.estDEMlegend1Click(Sender: TObject);
begin
   DisplayBitmap(DEMIXTestDEMLegend,'Test DEMs');
end;

procedure Tdbtablef.Terrainfabric1Click(Sender: TObject);
begin
   {$IfDef Exgeology}
   {$Else}
      GISdb[DBonTable].dbOpts.dbAutoShow := dbasTerrainFabric;
      GISdb[DBonTable].RedrawLayerOnMap;
   {$EndIf}
end;

procedure Tdbtablef.Exportlatlongz1Click(Sender: TObject);
var
   Output : tStringList;
   fName : PathStr;
   DefExt : integer;
begin
   GISdb[DBonTable].zFieldName := GISdb[DBonTable].PickField('Z field',NumericFieldTypes);
   DefExt := 1;
   fName := ChangeFileExt(GISdb[DBonTable].dbFullName,'.xyz');
   if GetFileNameDefaultExtSaveExt('output xyz','xyz file|*.xyz',FName,DefExt,false) then begin
      Output := tStringList.Create;
      Output.Add('Lat,Long,z');
      GISdb[DBonTable].EmpSource.Enabled := false;
      GISdb[DBonTable].MyData.First;
      while not GISdb[DBonTable].MyData.eof do begin
         OutPut.Add(RealToString(GISdb[DBonTable].MyData.GetFieldByNameAsFloat(GISdb[DBonTable].LatFieldName),-12,-8) + ',' +
                    RealToString(GISdb[DBonTable].MyData.GetFieldByNameAsFloat(GISdb[DBonTable].LongFieldName),-12,-8) + ',' +
                    RealToString(GISdb[DBonTable].MyData.GetFieldByNameAsFloat(GISdb[DBonTable].ZFieldName),-12,-8) );
         GISdb[DBonTable].MyData.Next;
      end;
      Output.SaveToFile(fName);
      Output.Free;
      ShowStatus;
   end;
end;

procedure Tdbtablef.Exportlinetopointdatabase1Click(Sender: TObject);
var
   i      : integer;
   LastZ,z,Dist,CumDist,Az,LastLat,LastLong,
   Lat,Long : float64;
   fName : PathStr;
   Table : tMyData;
begin
   {$IfDef RecordTerrainProfiles} WriteLineRoDebugFile('Tdbtablef.Exportlinetopointdatabase1Click'); {$EndIf}
   if ShapeFile3D(GISdb[DBonTable].ShapeFileType) then GISdb[DBonTable].aShapeFile.GetLineCoordsAndZsFromDEM(GISdb[DBonTable].TheMapOwner.MapDraw.DEMonMap,GISdb[DBonTable].MyData.RecNo)
   else GISdb[DBonTable].aShapeFile.GetLineCoords(GISdb[DBonTable].MyData.RecNo,true);
   fName := Petmar.NextFileNumber(MDTempDir, 'temp_prof_',DefaultDBExt);
   CreateThalwegFile(fName);
   Table := tMyData.Create(fName);
   for i := 0 to pred(GISdb[DBonTable].aShapeFile.CurrentPolyLineHeader.NumPoints) do begin
      Lat := GISdb[DBonTable].aShapeFile.CurrentLineCoords^[i].Lat;
      Long := GISdb[DBonTable].aShapeFile.CurrentLineCoords^[i].Long;
      z := GISdb[DBonTable].aShapeFile.CurrentLineZs^[i];

      if (i > 0) then begin
         VincentyCalculateDistanceBearing(Lat,Long,LastLat,LastLong,Dist,Az);
         CumDist := CumDist + 0.001 * Dist;
      end
      else CumDist := 0;

      Table.Insert;
      Table.SetFieldByNameAsFloat('LAT',Lat);
      Table.SetFieldByNameAsFloat('LONG',Long);
      Table.SetFieldByNameAsFloat('ELEV_M',z);
      Table.SetFieldByNameAsFloat('DIST_KM',CumDist);
      Table.Post;

      LastLat := Lat;
      LastLong := Long;
      LastZ := z;
   end;
   Table.Destroy;
   GISdb[DBonTable].TheMapOwner.LoadDataBaseFile(fName)
end;


procedure Tdbtablef.Exportshapefilecliptomapextent1Click(Sender: TObject);
begin
   {$IfDef NoExternalPrograms}
   {$Else}
      ExtractMapCoverageToWGS84Shapefile(GISdb[DBonTable].dbFullName, GISdb[DBonTable].TheMapOwner.MapDraw.MapCorners.BoundBoxGeo);
   {$EndIf}
end;


procedure Tdbtablef.Exporttablewithuniquerecords1Click(Sender: TObject);
var
   //tf : integer;
   Report,NewReport : tStringList;
   //ch : AnsiChar;
   fname : PathStr;
   i : integer;
   Header : shortstring;
begin
   {$IfDef RecordCSVOut} WriteLineRoDebugFile('Tdbtablef.Text1Click in'); {$EndIf}

   //with GISdb[DBonTable] do begin
      fName := ChangeFileExt(GISdb[DBonTable].DBFullName,'.csv');
      if Petmar.GetFileNameDefaultExt('Text file export','CSV|*.csv|Text|*.txt',fName) then begin
         Report := GISdb[DBonTable].ExtractDBtoCSV(1,',');
         Header := Report.Strings[0];
         Report.Delete(0);
         NewReport := tStringList.Create;
         NewReport.Sorted := true;
         NewReport.Duplicates := dupIgnore;
         for i := 1 to pred(Report.Count) do begin
            NewReport.Add(Report.Strings[i]);
         end;
         NewReport.Sorted := false;
         NewReport.Insert(0,Header);
         NewReport.SaveToFile(fName);
         Report.Free;
         NewReport.Free;
      end;
      ShowStatus;
   //end;
end;


procedure Tdbtablef.Exporttextdeliberate1Click(Sender: TObject);
begin
   Text1Click(Sender);
end;

procedure Tdbtablef.ExportXYZtriples1Click(Sender: TObject);
var
   fName : PathStr;
   Table : tMyData;
   NumPts,NParts,i : integer;
   Coords : ^tdCoords;
   zs : ^tdElevs;
   PartSize : tPartSize;
begin
   fName := ExtractFilePath(GISdb[DBonTable].dbFullName);
   if GetFileNameDefaultExt('table for XYZ triples',DBNameMask,fName) then begin
      CreateLatLongZTable(fName,true,false,true,false,false);
      Table := tMyData.Create(fName);
      //with GISdb[DBonTable] do begin
         GISdb[DBonTable].MyData.First;
         while not GISdb[DBonTable].MyData.eof do begin
            New(Coords);
            New(zs);
            GISdb[DBonTable].aShapeFile.OldGetLineCoordsWithZs(GISdb[DBonTable].MyData.RecNo,NumPts,NParts,Coords^,PartSize,ShapeFile3D(GISdb[DBonTable].ShapeFileType),zs^);
            for i := 0 to pred(NumPts) do begin
               Table.Insert;
               Table.SetFieldByNameAsFloat('LAT',Coords^[i].Lat);
               Table.SetFieldByNameAsFloat('LONG',Coords^[i].Long);
               Table.SetFieldByNameAsFloat('Z', zs^[i]);
               Table.Post;
            end;
            Dispose(Coords);
            Dispose(zs);
            GISdb[DBonTable].MyData.Next;
         end;
      //end;
      Table.Destroy;
   end;
end;

procedure Tdbtablef.Extractclassfiles1Click(Sender: TObject);
var
  cf : tStringList;
  i  : integer;
begin
  // with GISdb[DBonTable] do begin
      GISdb[DBonTable].DBFieldUniqueEntries('NAME',cf);
      cf.Sorted := false;
      GISdb[DBonTable].EmpSource.Enabled := false;
      for i := 0 to pred(cf.Count) do begin
          GISdb[DBonTable].MyData.ApplyFilter('NAME=' + QuotedStr(cf.Strings[i]));
          cf.Strings[i] := GISdb[DBonTable].MyData.GetFieldByNameAsString('NAME') + ',' +
              GISdb[DBonTable].MyData.GetFieldByNameAsString('CLASS') + ',' + GISdb[DBonTable].MyData.GetFieldByNameAsString('COLOR') + ',' + IntToStr(GISdb[DBonTable].MyData.RecordCount) +  ',Y';
          GISdb[DBonTable].MyData.Next;
      end;
      GISdb[DBonTable].ClearGISFilter;
      cf.Insert(0,'NAME,CLASS,COLOR,NPTS,USE');
      GISdb[DBonTable].theMapOwner.StringListToLoadedDatabase(cf, ExtractFilePath(GISdb[DBonTable].dbFullName) + 'training_definitions.csv');
   //end;
   ShowStatus;
end;

procedure Tdbtablef.Extractfiename1Click(Sender: TObject);
var
   TStr : shortstring;
   i : integer;
begin
   //with GISdb[DBonTable] do begin
     if (Sender = Extractfiename1) then begin
        SelectedColumn := GISdb[DBonTable].PickField('Field for filename extraction (path removal)',[ftString]);
     end;
     GISdb[DBonTable].EmpSource.Enabled := false;
     GISdb[DBonTable].MyData.First;
     while not GISdb[DBonTable].MyData.EOF do begin
        TStr := GISdb[DBonTable].MyData.GetFieldByNameAsString(SelectedColumn);
        for I := 1 to length(TStr) do if (TStr[i] = '/') then TStr[i] := '\';
        TStr := ExtractFileName(TStr);
        GISdb[DBonTable].MyData.Edit;
        GISdb[DBonTable].MyData.SetFieldByNameAsString(SelectedColumn,TStr);
        GISdb[DBonTable].MyData.Next;
     end;
     ShowStatus;
   //end;
end;

procedure Tdbtablef.Extractfilename1Click(Sender: TObject);
begin
   Extractfiename1Click(Sender);
end;


procedure Tdbtablef.f1Click(Sender: TObject);
var
   TheFields : tStringList;
begin
   {$IfDef RecordCopyFieldLinkDB} WriteLineRoDebugFile('Tdbtablef.f1Click enter (Copy fields from linked db)'); {$EndIf}
   if (GISdb[DBonTable].MyData.Filtered) then begin
      if not  AnswerIsYes('Apply only to filtered records') then begin
         GISdb[DBonTable].ClearGISFilter;
      end;
   end;

   TheFields := GISdb[DBonTable].LinkTable.FieldsInDataBase;
   PickSomeFromStringList(TheFields,'joined fields to merge into DB');
   {$IfDef RecordCopyFieldLinkDB} WriteLineRoDebugFile('Fields picked, n=' + IntToStr(TheFields.Count)); {$EndIf}
   GISdb[DBonTable].FillFieldsFromJoinedTable(TheFields,false);
   ShowStatus;
end;


procedure Tdbtablef.Stackedpercentages1Click(Sender: TObject);
begin
    StartStackedHistogram(DBonTable,true);
end;


procedure Tdbtablef.Stationtimeseries1Click(Sender: TObject);
begin
   {$IfDef NoDBGrafs}
   {$Else}
      GISdb[DBonTable].Stationtimeseries;
   {$EndIf}
end;


procedure Tdbtablef.Stratigraphiccolumn1Click(Sender: TObject);
begin
   {$IfDef ExGeology}
   {$Else}
      ColMainF.NewFileToDiagram(GISdb[DBonTable].MyData.GetFieldByNameAsString('COL_NAME'));
   {$EndIf}
end;


procedure Tdbtablef.FillField1Click(Sender: TObject);
begin
   Fillfieldforallrecords1Click(Sender);
   //ShowStatus;
end;

procedure Tdbtablef.Fillfieldforallrecords1Click(Sender: TObject);
var
   WantedFieldName : string10;
   WantedValue : shortString;
begin
   if (Sender = FillField1) and (SelectedColumn <> '') then WantedFieldName := SelectedColumn
   else WantedFieldName := GISDB[DBonTable].PickField('field to fill',[ftString,ftSmallInt,ftFloat,ftInteger]);
   if (WantedFieldName <> '') then begin
      WantedValue := '';
      if (Sender <> Clearfield1) then GetString('value for field ' + WantedFieldName,WantedValue,false,ReasonableTextChars);
      GISdb[DBonTable].EmpSource.Enabled := false;
      GISdb[DBonTable].FillFieldWithValue(WantedFieldName,WantedValue);
   end;
   ShowStatus;
end;


procedure Tdbtablef.Fillfontfield1Click(Sender: TObject);
var
   aFont : tFont;
begin
   aFont := tFont.Create;
   EditTFont(aFont);
   GISdb[DBonTable].MyData.First;
   while not GISdb[DBonTable].MyData.eof do begin
      GISdb[DBonTable].MyData.PostFont(aFont);
      GISdb[DBonTable].MyData.Next;
   end;
end;


procedure Tdbtablef.Fillpatternallrecords1Click(Sender: TObject);
begin
   Editfillpattern1Click(Sender);
end;

procedure Tdbtablef.Fillpatternalluncoloredrecords1Click(Sender: TObject);
begin
   Editfillpattern1Click(Sender);
end;

procedure Tdbtablef.Filltrackvoids1Click(Sender: TObject);
begin
   GISdb[DBonTable].FillTrackVoids;
end;



procedure Tdbtablef.Fillviewshedfields1Click(Sender: TObject);
var
   wf : tWeaponsFan;
begin
   //with GISdb[DBonTable] do begin
      GISdb[DBonTable].EmpSource.Enabled := false;
      GISdb[DBonTable].MyData.First;
      InitializeWeaponsFan(wf);
      while not GISdb[DBonTable].MyData.Eof do begin
         wf.Fan_Name := GISdb[DBonTable].MyData.GetFieldByNameAsString('NAME');
         wf.W_Lat  := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(GISdb[DBonTable].LatFieldName);
         wf.W_Long := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(GISdb[DBonTable].LongFieldName);
         AddFanToWeaponsTable(GISdb[DBonTable].TheMapOwner.MapDraw.PrimMapProj,true,true,GISdb[DBonTable].MyData,wf);
         GISdb[DBonTable].MyData.Next;
      end;
      ShowStatus;
   //end;
end;


procedure Tdbtablef.Filterfor0valuesinanyevaluation1Click(Sender: TObject);
begin
   FilterTableForDEMIXevaluation(DBonTable,0);
end;

procedure Tdbtablef.Filterfor999valuesinanyevaluation1Click(Sender: TObject);
begin
    FilterTableForDEMIXevaluation(DBonTable,-999);
end;


procedure Tdbtablef.FilterforDEMIXtiles1Click(Sender: TObject);
begin
   GISdb[DBonTable].ApplyGISFilter('REF_TYPE=' + QuotedStr('DTM') + ' AND LAND_TYPE=' + QuotedStr('ALL') + ' AND CRITERION=' + QuotedStr('ELVD_AVD'));
end;


procedure Tdbtablef.Filterforevaluations11Click(Sender: TObject);
begin
   FilterTableForDEMIXevaluation(DBonTable,1, '>');
end;

procedure Tdbtablef.Filterforjustsignedcrirteria1Click(Sender: TObject);
begin
   FilterInSignedCriteria(DBonTable);
end;

procedure Tdbtablef.Filteroutsignedcriteriameanandmedian1Click(Sender: TObject);
begin
   FilterOutSignedCriteria(DBonTable);
end;

procedure Tdbtablef.Changefieldsused1Click(Sender: TObject);
begin
   VerifyRecordsToUse(GISdb[DBonTable].MyData,'LINE_NAME','Records to use');
end;


procedure Tdbtablef.Changefieldtype1Click(Sender: TObject);
var
   WantedFieldName : ShortString;
   ft : tFieldType;
begin
   //with GISdb[DBonTable] do begin
      WantedFieldName := SelectedColumn;
      ft := GISdb[DBonTable].MyData.GetFieldType(WantedFieldName);
      if (ft in [ftInteger,ftSmallInt,ftDate]) and AnswerIsYes('Confirm convert ' + WantedFieldName + ' to type string') then begin
         GISdb[DBonTable].ChangeFieldType(WantedFieldName,'C');
      end
      else if (ft = ftString) then begin
          if GISdb[DBonTable].MyData.GetFieldLength(WantedFieldName) > 12 then MessageToContinue('Field ' + WantedFieldName + ' too big to convert')
          else begin
             if AnswerIsYes('Confirm convert ' + WantedFieldName + ' to type integer') then
               GISdb[DBonTable].ChangeFieldType(WantedFieldName,'N');
          end;
      end
      else MessageToContinue('Cannot convert that field type');
   //end;
   ShowStatus;
end;


procedure Tdbtablef.Changelatlongfields1Click(Sender: TObject);
begin
   //with GISdb[DBonTable] do begin
      GISdb[DBonTable].LatFieldName := GISdb[DBonTable].PickField('Latitude',[ftFloat]);
      GISdb[DBonTable].LongFieldName := GISdb[DBonTable].PickField('Longitude',[ftFloat]);
      GISdb[DBonTable].ItsAPointDB := GISdb[DBonTable].LatLongFieldsPresent;
      ShowStatus;
   //end;
end;


procedure Tdbtablef.ChangeTIGERsymbology1Click(Sender: TObject);
begin
   GISdb[DBonTable].TheMapOwner.ModifyTIGERdisplay1Click(nil);
end;


procedure Tdbtablef.CheckBox1Click(Sender: TObject);
begin
   {$IfDef RecordEditDB} WriteLineRoDebugFile('CheckBox1Click (db edit) in'); {$EndIf}
   if CheckBox1.Enabled and ValidDB(DBonTable) { <> 0) and (GISdb[DBonTable] <> Nil)} then begin
      if StrUtils.AnsiContainsText(GISdb[DBonTable].DBFullName,' ') then begin
         {$IfDef RecordEditDB} WriteLineRoDebugFile('Space in file name');{$EndIf}
         MessageToContinue('Cannot edit shapefile with space in file path or name ' + MessLineBreak + GISdb[DBonTable].DBFullName);
         Button4.Visible := false;
         CheckBox1.Enabled := false;
         CheckBox1.Checked := false;
      end
      else begin
         Button4.Enabled := CheckBox1.Checked;
         BackupDB1Click(Sender);
         {$IfDef RecordEditDB} WriteLineRoDebugFile('Backup done');{$EndIf}
         if PointShapeFile(GISdb[DBonTable].ShapeFileType) then begin
            DeleteFileIfExists(ChangeFileExt(GISdb[DBonTable].DBFullName,'.shp'));
            DeleteFileIfExists(ChangeFileExt(GISdb[DBonTable].DBFullName,'.shx'));
            GISdb[DBonTable].DBFullName := ChangeFileExt(GISdb[DBonTable].DBFullName,DefaultDBExt);
            GISdb[DBonTable].ItsAShapeFile := false;
         end;
      end;
      GISdb[DBonTable].AddSequentialIndex(RecNoFName,false);
   end;
   {$IfDef RecordEditDB} WriteLineRoDebugFile('CheckBox1Click (dbedit) out');{$EndIf}
end;


procedure Tdbtablef.CiompareCOPtorivals1Click(Sender: TObject);
begin
   CompareSeriousCompetitors(DBonTable);
end;

procedure Tdbtablef.Animatefield1Click(Sender: TObject);
begin
   {$IfDef ExAdvancedGIS}
   {$Else}
   GISdb[DBonTable].GISProportionalSymbols(dbasAnimate);
   {$EndIf}
end;


procedure Tdbtablef.ArcGISviewshedsensors1Click(Sender: TObject);
var
   fName : PathStr;
   ArcShape : integer;
begin
   with GISdb[DBonTable] do begin
      fName := ExtractFilePath(MyData.TableName);
      if GetFileNameDefaultExt('Sensors for ArcGIS',DBNameMask,fName) then begin
         SaveCurrentDBaseSubset(fName);
         OpenNumberedGISDataBase(ArcShape, fName);
         GISdb[ArcShape].RenameField('SENSOR_UP','OFFSETA');
         GISdb[ArcShape].RenameField('TARGET_UP','OFFSETB');
         GISdb[ArcShape].RenameField('SENSOR_RNG','RADIUS2');
         GISdb[ArcShape].RenameField('MIN_RNG','RADIUS1');
         GISdb[ArcShape].RenameField('LEFT_AZ','AZIMUTH1');
         GISdb[ArcShape].RenameField('RIGHT_AZ','AZIMUTH2');
         GISdb[ArcShape].RenameField('MAX_VERT','VERT1');
         GISdb[ArcShape].RenameField('MIN_VERT','VERT2');
         GISdb[ArcShape].SavePointShapeFile;
         CloseAndNilNumberedDB(ArcShape);
      end;
   end;
end;


procedure Tdbtablef.Areaofeachrecord1Click(Sender: TObject);
begin
   GISdb[DBonTable].AddGeometry(agAreaKM2);
end;

procedure Tdbtablef.Areaofeachrecordm1Click(Sender: TObject);
begin
  GISdb[DBonTable].AddGeometry(agAreaM2);
end;

procedure Tdbtablef.Areashapefile1Click(Sender: TObject);
begin
   {$IfDef RecordMakeLineArea} WriteLineRoDebugFile('Tdbtablef.Createlineshapefilefrompoints1Click ' + GISDataBase[DBonTable].dbname); {$EndIf}
   MakeLinesFromPoints(GISdb[DBonTable],'',5);
   ShowStatus;
end;


procedure Tdbtablef.Areasinclusters1Click(Sender: TObject);
begin
    AreasInClusters(DBonTable);
end;

procedure Tdbtablef.Create1Click(Sender: TObject);
var
   ShapeFileCreator : tShapeFileCreation;
   fName : PathStr;
   j : integer;
begin
   if Petmar.GetFileNameDefaultExt('Database limits','Shapefile|*.shp',fName) then begin
      ShapeFileCreator := tShapeFileCreation.Create(WGS84DatumConstants,fName,true,5);
      for j := 1 to MaxDataBase do if ValidDB(j) then begin
         ShapeFileCreator.RecordName := GISdb[j].DBname;
         ShapeFileCreator.AddPointToShapeStream(GISdb[j].dbBoundBox.yMax,GISdb[j].dbBoundBox.xMax);
         ShapeFileCreator.AddPointToShapeStream(GISdb[j].dbBoundBox.yMax,GISdb[j].dbBoundBox.xMin);
         ShapeFileCreator.AddPointToShapeStream(GISdb[j].dbBoundBox.yMin,GISdb[j].dbBoundBox.xMin);
         ShapeFileCreator.AddPointToShapeStream(GISdb[j].dbBoundBox.yMin,GISdb[j].dbBoundBox.xMax);
         ShapeFileCreator.AddPointToShapeStream(GISdb[j].dbBoundBox.yMax,GISdb[j].dbBoundBox.xMax);
         ShapeFileCreator.ProcessRecordForShapeFile;
      end;
      ShapeFileCreator.CloseShapeFiles;
   end;
end;


procedure Tdbtablef.CreateDBwithcornersandcenterofeveryrecord1Click(Sender: TObject);
var
   sl : tStringList;
   bb : sfBoundBox;
   Name : shortstring;
   fName : PathStr;
begin
   GISdb[DBonTable].MyData.First;
   sl := tStringList.Create;
   sl.add('LONG,LAT,Z,NAME');
   while not GISdb[DBonTable].MyData.eof do begin
      Name := GISdb[DBonTable].MyData.GetFieldByNameAsString(GISdb[DBonTable].dbOpts.LabelField);
      bb := GISdb[DBonTable].MyData.GetRecordBoundingBox;
      sl.add(RealToString(bb.XMin,-12,-6) + ',' + RealToString(bb.YMin,-12,-6) + ',0,' + Name);
      sl.add(RealToString(bb.XMax,-12,-6) + ',' + RealToString(bb.YMin,-12,-6) + ',0,' + Name);
      sl.add(RealToString(bb.XMax,-12,-6) + ',' + RealToString(bb.YMax,-12,-6) + ',0,' + Name);
      sl.add(RealToString(bb.XMin,-12,-6) + ',' + RealToString(bb.YMax,-12,-6) + ',0,' + Name);
      sl.add(RealToString(0.5 * (bb.XMin + bb.XMax),-12,-6) + ',' + RealToString(0.5 * (bb.YMin + bb.YMax),-12,-6) + ',0,' + Name);
      GISdb[DBonTable].MyData.Next;
   end;
   fName := NextFileNumber(MDTempDir,'test_site_limits_','.csv');
   GISdb[DBonTable].theMapOwner.StringListToLoadedDatabase(sl,fName);
end;

procedure Tdbtablef.CreateDBwithparametersbyDEM1Click(Sender: TObject);
begin
   MakeDBForParamStats(opByDEM,DBonTable);
end;

procedure Tdbtablef.CreateDEM1Click(Sender: TObject);
var
   bbox : sfBoundBox;
   WantSeries : ShortString;
   WantDEM : integer;
   Bitmap : tMyBitmap;
   fName : PathStr;
begin
   with GISdb[DBonTable] do begin
      bBox := GISdb[DBonTable].MyData.GetRecordBoundingBox;
      PickDEMSeries(WantSeries,'DEM blowup');
      WantDEM := LoadMapLibraryBox(true,bbox,WantSeries);
      if AnswerIsYes('clip to DEM to record outline') then begin
         DEMDef_Routines.SaveBackupDefaults;
         MDDef.MissingDataColor := claWhite;
         MDDef.MapTicks := tixNone;
         MDDef.ScaleBarLocation.DrawItem := false;
         DEMGlb[WantDEM].SelectionMap.MapDraw.MapType := mtDEMBlank;
         DEMGlb[WantDEM].SelectionMap.MapDraw.MaximizeLatLongMapCoverage(bbox.ymin,bbox.xMin,bbox.yMax,bbox.xMax);

         DEMGlb[WantDEM].SelectionMap.DoCompleteMapRedraw;
         CopyImageToBitmap(DEMGlb[WantDEM].SelectionMap.Image1,Bitmap);
         Bitmap.Canvas.Brush.Color := clBlack;
         Bitmap.Canvas.Brush.Style := bsSolid;

         aShapeFile.PlotSingleRecordMap(DEMGlb[WantDEM].SelectionMap.MapDraw,Bitmap,MyData.RecNo);
         DEMGlb[WantDEM].SelectionMap.Image1.Picture.Graphic := Bitmap;
         Bitmap.Free;
         DEMGlb[WantDEM].SelectionMap.EditGridViaColor(emvcAllbutselectedcolor,clBlack,-MaxSmallInt,true,false);
         DEMDef_Routines.SaveBackupDefaults;

          with DEMGlb[WantDEM],SelectionMap,MapDraw,Mapcorners do begin
             fName := Petmar.NextFileNumber(MDTempDir, 'temp_dem_','.dem');
             RectangleSubsetDEM(SpecifyDEMGridLimits(BoundBoxDataGrid.xmin,BoundBoxDataGrid.ymin,BoundBoxDataGrid.xmax,BoundBoxDataGrid.ymax),fName);
             CloseSingleDEM(WantDEM);
             WantDEM := 0;
             LoadNewDEM(WantDEM,fName);
          end;
      end;
   end;
end;


procedure Tdbtablef.CreategeographicPRJfile1Click(Sender: TObject);
begin
   AddProjectionFile(GISdb[DBonTable].DBfullName);
   //if FileExists(WKT_GCS_Proj_fName) then begin Petmar.CopyFile(WKT_GCS_Proj_fName,ChangeFileExt(GISdb[DBonTable].DBfullName,'.prj') ); end;
end;

procedure Tdbtablef.Creategrid1Click(Sender: TObject);
var
   i,NumPts,NParts,NewDEM,DiffDEM : integer;
   Coords : ^tdCoords;
   zs : ^tdElevs;
   z : float32;
   PartSize : tPartSize;

         procedure MapIt(DEM : integer; Title : ShortString);
         begin
            DEMGlb[DEM].CheckMaxMinElev;
            DEMGlb[DEM].AreaName := Title;
            CreateDEMSelectionMap(DEM);
         end;

begin
   DEMGlb[GISdb[DBonTable].TheMapOwner.MapDraw.DEMonMap].SetNewDEM(NewDEM);
   DEMGlb[GISdb[DBonTable].TheMapOwner.MapDraw.DEMonMap].SetNewDEM(DiffDEM);
   GISdb[DBonTable].MyData.First;
   while not GISdb[DBonTable].MyData.eof do begin
      New(Coords);
      New(zs);
      GISdb[DBonTable].aShapeFile.OldGetLineCoordsWithZs(GISdb[DBonTable].MyData.RecNo,NumPts,NParts,Coords^,PartSize,ShapeFile3D(GISdb[DBonTable].ShapeFileType),zs^);
      for i := 0 to pred(NumPts) do begin
         DEMGlb[NewDEM].SetGridElevationLatLongDegree(Coords^[i].Lat,Coords^[i].Long,zs^[i]);
         if DEMGlb[GISdb[DBonTable].TheMapOwner.MapDraw.DEMonMap].GetElevFromLatLongDegree(Coords^[i].Lat,Coords^[i].Long,z) then begin
            DEMGlb[DiffDEM].SetGridElevationLatLongDegree(Coords^[i].Lat,Coords^[i].Long,zs^[i]-z);
         end;
      end;
      Dispose(Coords);
      Dispose(zs);
      GISdb[DBonTable].MyData.Next;
   end;
   MapIt(NewDEM,'Grid from shape file');
   MapIt(DiffDEM,'Difference Grid');
end;


procedure Tdbtablef.Creategrid2Click(Sender: TObject);
begin
   CreateGrid(cgValuesGrid);
end;

procedure Tdbtablef.Creategrid3Click(Sender: TObject);
begin
   PopupMenu3.PopUp(Mouse.CursorPos.X,Mouse.CursorPos.Y);
end;

procedure Tdbtablef.Creategrid4Click(Sender: TObject);
begin
   PopupMenu3.PopUp(Mouse.CursorPos.X,Mouse.CursorPos.Y);
end;

function Tdbtablef.CreateGrid(HowCreate : tcgHow; GridSize : float64 = -99) : integer;
//type = tcgHow(cgValuesGrid,cgRadiusDB,cgBox,cdCode,cdPointDensity);
var
   //NewHeadRecs : tDEMheader;
   WantedFieldName : ShortString;
   NumPts : int64;
   xg,yg,NewDEM,i,j,xinc,yinc,x,y,Code,rc : integer;
   fName : PathStr;
   Lat,Long,z,rad : float64;
   FieldsUsed : tStringList;
   Num : integer;
begin

//TMapForm.MakeTempGrid(OpenMap : boolean = false; GetParameters : boolean = false) : integer;


   if (HowCreate = cgPointDensity) then begin
      Result := GISdb[DBOnTable].TheMapOwner.CreateGridToMatchMap(cgUTM,true,SmallIntDEM,GridSize);
      DEMGlb[Result].AreaName := GISdb[DBonTable].dbName + '_pt_density';
      DEMGlb[Result].DEMheader.ElevUnits := euUndefined;
      DEMGlb[Result].MissingDataToConstantVelue;

      StartProgress('Density');
      Num := 0;
      rc := GISdb[DBonTable].MyData.RecordCount;
      GISdb[DBonTable].MyData.First;
      GISdb[DBonTable].EmpSource.Enabled := false;
      while not GISdb[DBonTable].MyData.eof do begin
         if (Num mod 500 = 0) then UpdateProgressBar(Num/rc);
         inc(Num);
         if GISdb[DBonTable].ValidLatLongFromTable(Lat,Long) then begin
            DEMGlb[Result].LatLongDegreeToDEMGridInteger(Lat,Long,xg,yg);
            if DEMGlb[Result].GridInDataSet(xg,yg) then begin
               DEMGlb[Result].IncrementGridValue(xg,yg);
            end;
         end;
         GISdb[DBonTable].MyData.Next;
      end;
      DEMGlb[Result].MarkInRangeMissing(0,0,NumPts);
      DEMGlb[Result].SelectionMap.MapDraw.MapType := mtElevSpectrum;
      DEMGlb[Result].SelectionMap.RespondToChangedDEM;
      exit;
   end;

   with GISdb[DBonTable] do begin
      if (HowCreate = cgBox) then begin
         Rad := 5;
         ReadDefault('radius (m)',Rad);
      end;

      //NewHeadRecs := DEMGlb[TheMapOwner.MapDraw.DEMonMap].DEMheader;

      if (HowCreate = cgCode) then begin
         Code := 1;
         ReadDefault('Code for grid',Code);
         FieldsUsed := tStringList.Create;
         FieldsUsed.Add(dbName);
         //NewHeadRecs.DEMPrecision := ByteDEM;
      end
      else FieldsUsed := GISdb[DBonTable].GetAnalysisFields;

      for j := 0 to pred(FieldsUsed.Count) do begin
         WantedFieldName := FieldsUsed.Strings[j];
         //if OpenAndZeroNewDEM(true,NewHeadRecs,NewDEM,FieldsUsed.Strings[j],InitDEMmissing) then begin

         NewDEM := GISdb[DBonTable].theMapOwner.MakeTempGrid(true,true);
         if NewDEM <> 0 then begin
            //AreaName := FieldsUsed.Strings[j];
            DEMGlb[NewDEM].ShortName := FieldsUsed.Strings[j];
            GISdb[DBonTable].MyData.First;
            EmpSource.Enabled := false;

            i := 0;
            rc := GISdb[DBonTable].MyData.RecordCount;
            StartProgress('Extract grid ' + WantedFieldName + '  ' + IntToStr(j +1) + '/' + IntToStr(FieldsUsed.Count));
            while not GISdb[DBonTable].MyData.eof do begin
               inc(i);
               if (i mod 1000 = 0) then begin
                  UpdateProgressBar(i/rc);
                  EmpSource.Enabled := false;
               end;

               if ValidLatLongFromTable(Lat,Long) and ((HowCreate = cgCode) or GISdb[DBonTable].MyData.CarefullyGetFieldByNameAsFloat64(WantedFieldName,z)) then begin
                  DEMGlb[NewDEM].LatLongDegreeToDEMGridInteger(Lat,Long,xg,yg);
                  if (HowCreate = cgCode) then begin
                     DEMGlb[NewDEM].SetGridElevation(xg,yg,Code);
                  end
                  else if (HowCreate = cgBox) or (HowCreate = cgRadiusDB) then begin
                     if (HowCreate = cgRadiusDB) then begin
                        Rad := GISdb[DBonTable].MyData.GetFieldByNameAsFloat('RADIUS_M');
                     end;
                     xinc := round(Rad/DEMGlb[NewDEM].AverageXSpace);
                     yinc := round(Rad/DEMGlb[NewDEM].AverageYSpace);
                     for x := xg-xinc to xg+xinc do
                        for y := yg-yinc to yg+yinc do
                           DEMGlb[NewDEM].SetGridElevation(x,y,z);
                  end
                  else DEMGlb[NewDEM].SetGridElevation(xg,yg,z);
               end;
               GISdb[DBonTable].MyData.Next;
            end;
            EndProgress;
            fName := ExtractFilePath(GISdb[DBonTable].dbFullName) + FieldsUsed.Strings[j] + '.dem';
            DEMGlb[NewDEM].WriteNewFormatDEM(fName);
            DEMGlb[NewDEM].SetUpMap(NewDEM,false,mtElevRainbow);
         end;
      end;
      ShowStatus;
   end;
end;

procedure Tdbtablef.Creategridbox1Click(Sender: TObject);
begin
   CreateGrid(cgBox);
end;

procedure Tdbtablef.MakeDistanceGridToClassCentroid(aField : shortstring);
{$IfDef ExSat}
begin
{$Else}
//var
   //NewDEM,x,y,Band,i,n,ClassNumUsed : integer;
   //Sum,z : float64;
begin
   MessageToContinue('currently disabled');
(*

   with GISdb[DBonTable] do begin
      for i := 1 to theMapOwner.NumClass do if theMapOwner.Classes^[i].ClassName = aField then ClassNumUsed := i;
      NewDEM := MultiGridArray[TheMapOwner.MapDraw.MultiGridOnMap].OpenNewGrid('Dist_' + aField + '_centroid',Undefined,FloatingPointDEM);
      for x := 0 to pred(DEMGlb[MultiGridArray[TheMapOwner.MapDraw.MultiGridOnMap].Grids[1]].HeadRecs.NumCol) do begin
         if (x mod 50 = 0) then UpDateProgressBar(x/DEMGlb[MultiGridArray[TheMapOwner.MapDraw.MultiGridOnMap].Grids[1]].HeadRecs.NumCol);
         for y := 0 to pred(DEMGlb[MultiGridArray[TheMapOwner.MapDraw.MultiGridOnMap].Grids[1]].HeadRecs.NumRow) do begin
             Sum := 0;
             n := 0;
             for Band := 1 to MultiGridArray[TheMapOwner.MapDraw.MultiGridOnMap].NumGrids do begin
                if DEMGlb[MultiGridArray[TheMapOwner.MapDraw.MultiGridOnMap].Grids[Band]].GetElevMeters(x,y,z) then begin
                   Sum := Sum + Math.Power(z - TheMapOwner.Classes^[ClassNumUsed].Mean[Band],MDDef.ClassDistancePower);
                   inc(n);
                end;
             end;
             if n > 0 then begin
                DEMGlb[NewDEM].SetGridElevation(x,y,sqrt(Sum));
             end;
         end;
      end;
     MultiGridArray[TheMapOwner.MapDraw.MultiGridOnMap].DisplayNewGrid(NewDEM);
   end;
   *)
{$EndIf}
end;


procedure Tdbtablef.MakeGridBandsInClassBox(aField : shortstring);
{$IfDef ExSat}
begin
{$Else}
//var
   //NewDEM,x,y,Band,i,n,ClassNumUsed : integer;
   //z : float64;
begin
   MessageToContinue('currently disabled');
(*
   with GISdb[DBonTable],SatImage[TheMapOwner.MapDraw.SatOnMap] do begin
      for i := 1 to TheMapOwner.NumClass do if TheMapOwner.Classes^[i].ClassName = aField then ClassNumUsed := i;
      NewDEM := MultiGridArray[TheMapOwner.MapDraw.MultiGridOnMap].OpenNewGrid('Bands_' + aField + '_box_' + ClassBoxName,Undefined,ByteDEM);
      for x := 0 to pred(DEMGlb[MultiGridArray[TheMapOwner.MapDraw.MultiGridOnMap].Grids[1]].HeadRecs.NumCol) do begin
         if (x mod 50 = 0) then UpDateProgressBar(x/DEMGlb[MultiGridArray[TheMapOwner.MapDraw.MultiGridOnMap].Grids[1]].HeadRecs.NumCol);
         for y := 0 to pred(DEMGlb[MultiGridArray[TheMapOwner.MapDraw.MultiGridOnMap].Grids[1]].HeadRecs.NumRow) do begin
             N := 0;
             for Band := 1 to MultiGridArray[TheMapOwner.MapDraw.MultiGridOnMap].NumGrids do begin
                if DEMGlb[MultiGridArray[TheMapOwner.MapDraw.MultiGridOnMap].Grids[Band]].GetElevMeters(x,y,z) and (z >= TheMapOwner.Classes^[ClassNumUsed].ClassLowLimit[Band]) and
                       (z <= TheMapOwner.Classes^[ClassNumUsed].ClassHighLimit[Band]) then begin
                   inc(n);
                end;
             end;
             if n > 0 then begin
                DEMGlb[NewDEM].SetGridElevation(x,y,n);
             end;
         end;
      end;
     MultiGridArray[TheMapOwner.MapDraw.MultiGridOnMap].DisplayNewGrid(NewDEM);
   end;
   *)
{$EndIf}
end;


procedure Tdbtablef.MapsbyclusterandDEM1Click(Sender: TObject);
begin
   MapsByClusterAndDEM(DBonTable);
end;

function Tdbtablef.GetMultipleEntriesFromTableField(WhatFor,aName : shortstring) : tStringList;
var
   PickedNum : integer;
begin
   GISdb[DBonTable].EmpSource.Enabled := false;
   Result := GISdb[DBonTable].MyData.ListUniqueEntriesInDB(aName);
   PickedNum := 1;
   if not GetMultipleFromList(WhatFor,PickedNum,Result,True) then Result.Clear;
end;


function Tdbtablef.GetSingleEntryFromTableField(WhatFor,aName : shortstring) : ShortString;
var
   Results : tStringList;
begin
   GISdb[DBonTable].EmpSource.Enabled := false;
   Results := GISdb[DBonTable].MyData.ListUniqueEntriesInDB(aName);
   Result := GetFromList(WhatFor,Results,True);
   Results.Free;
end;


procedure Tdbtablef.GlobalDEMsandpointclouds1Click(Sender: TObject);
begin
   HistogramPointCloudAndGlobalDEMs(0);
   ShowStatus;
end;

procedure Tdbtablef.Creategriddistancetoclasscentroid1Click(Sender: TObject);
{$IfDef ExSat}
begin
{$Else}
var
   InList : TStringList;
   i : integer;
   aName : shortstring;
   TStr : shortString;
begin
   {$IfDef RecordMultigrid} WriteLineRoDebugFile('Tdbtablef.Creategriddistancetoclasscentroid1Click in'); {$EndIf}
   if (Sender = Creategridnumberofbandswithclassbox1) then TStr := 'Bands in class box'
   else TStr := 'Distance to grid centroid';
   InList := GetMultipleEntriesFromTableField(TStr, 'NAME');
   with GISdb[DBonTable] do begin
      for i := 0 to pred(InList.Count) do begin
         aName := InList.Strings[i];
         if (Sender = Creategridnumberofbandswithclassbox1) then MakeGridBandsInClassBox(aName)
         else MakeDistanceGridToClassCentroid(aName);
      end;
      ShowStatus;
   end;
{$EndIf}
end;


procedure Tdbtablef.Creategridnumberofbandswithclassbox1Click(Sender: TObject);
begin
   Creategriddistancetoclasscentroid1Click(Sender);
end;

procedure Tdbtablef.Creategridradius1Click(Sender: TObject);
begin
   CreateGrid(cgRadiusDB);
end;

procedure Tdbtablef.Creategridspecifycode1Click(Sender: TObject);
begin
   CreateGrid(cgCode);
end;

procedure Tdbtablef.Createnormalilzeddatabase1Click(Sender: TObject);
begin
   Normalizefield1Click(Sender);
end;

procedure Tdbtablef.Createpointsalonglines1Click(Sender: TObject);
begin
   Createpointsalonglinesbyseparation1Click(Sender);
end;

procedure Tdbtablef.Createpointsalonglinesbyseparation1Click(Sender: TObject);
var
   DistApart : float64;
   AddFirst,AddLast,AddTurns : boolean;
begin
   if (Sender = Createpointsalonglines1) then DistApart := -99
   else begin
      DistApart := 100;
      ReadDefault('Spacing for points (m)',DistApart);
   end;
   AddFirst := AnswerIsYes('Include first point');
   AddLast := AnswerIsYes('Include last point (partial distance)');
   AddTurns := AnswerIsYes('Include turn waypoints (partial distance)');
   GISdb[DBonTable].PointsForLineAreaDB(AddFirst,AddLast,AddTurns,DistApart);
end;

procedure Tdbtablef.Createpointshapefile1Click(Sender: TObject);
begin
   GISdb[DBonTable].SavePointShapeFile;
end;


procedure Tdbtablef.CreatepointSHXindex1Click(Sender: TObject);
begin
   GISdb[DBonTable].SavePointShapeFile(false);
end;

procedure Tdbtablef.Createpointsinpolygons1Click(Sender: TObject);
begin
   GISdb[DBonTable].PointsForLineAreaDB(false,false,false);
end;


procedure Tdbtablef.Createshapefilewithboundingboxforeachrecord1Click(Sender: TObject);
var
   sfc : tShapeFileCreation;
   fName,tile : PathStr;
   db : integer;
   //Lat,Long : float64;
   bb : sfBoundBox;
begin
   GISdb[DBonTable].MyData.First;
   while not GISdb[DBonTable].MyData.eof do begin
      bb := GISdb[DBonTable].MyData.GetRecordBoundingBox;
      bb.XMax := bb.XMax - 0.025;
      bb.XMin := bb.XMin + 0.025;
      bb.YMax := bb.YMax - 0.025;
      bb.YMin := bb.YMin + 0.025;
      Tile := ExtractFileNameNoExt(GISdb[DBonTable].MyData.GetFieldByNameAsString('FILENAME')) ;
      fName := MDTempDir +  Tile + '_bounding_box.dbf';
      sfc := tShapeFileCreation.Create(WGS84DatumConstants,fName,true,5);
      sfc.AddBoundBoxToShapeStream(bb);
      sfc.CloseShapeFiles;
      db := OpenMultipleDataBases('',fName,false);
      SaveBackupDefaults;
      MDDef.ZipKMLFiles := false;

      fName := GISdb[DB].ExportToKML(true,true);
      RestoreBackupDefaults;
      Petmar.CopyFile(fName,MDTempDir + ExtractFileName(fName));
      //ZipShapefile(DB,false,false);     //this won't work on the USGS web sites, which was the whole point of creating it
      CloseAndNilNumberedDB(db);
      GISdb[DBonTable].MyData.Next;
   end;
end;

procedure Tdbtablef.Createuniticons1Click(Sender: TObject);
{$IfDef ExMilIcons}
begin
{$Else}
var
   WantedFieldName : ShortString;
   Tstr : shortstring;
   Units : tStringList;
begin
   with GISdb[DBonTable] do begin
      WantedFieldName := PickField('Unit symbols' ,[ftString]);
      AddFieldToDatabase(ftString,'ICON',24,0);
      Units := tStringList.Create;
      Units.Sorted := true;
      Units.Duplicates := dupIgnore;
      GISdb[DBonTable].MyData.First;
      while not GISdb[DBonTable].MyData.EOF do begin
         TStr := GISdb[DBonTable].MyData.GetFieldByNameAsString(WantedFieldName);
         Units.Add(Tstr);
         GISdb[DBonTable].MyData.Edit;
         GISdb[DBonTable].MyData.SetFieldByNameAsString('ICON',CleanUnitName(TStr) + OverlayFExt);
         GISdb[DBonTable].MyData.Next;
      end;
      Units.SaveToFile(ExtractFilePath(dbFullName) + 'units.txt');
      Units.Free;
      MilIconsForm := TMilIconsForm.Create(Application);
      MilIconsForm.IconList := ExtractFilePath(dbFullName) + 'units.txt';
      MilIconsForm.ShowModal;
   end;
   {$EndIf}
end;


procedure Tdbtablef.dbfStructClick(Sender: TObject);
var
   Output : tStringList;
begin
   Output := GISdb[DBonTable].MyData.GetTableStructure;
   StringList2CSVtoDB(OutPut,MDTempDir + GISdb[DBonTable].dbName + '_structure.csv');
end;


procedure Tdbtablef.PlotSingleFile(fName : PathStr; xoff,yoff : float64);
var
   Values : tStringList;
   xi,yi,j,err : integer;
   TStr : AnsiString;
   x,y : float64;
   FirstPt : boolean;
   SepChar : AnsiChar;
begin
   with GISdb[DBonTable] do begin
      Values := tStringList.Create;
      Values.LoadFromFile(fName);
      TStr := Values.Strings[0];
      GetSeparationCharacter(TStr,SepChar);
      FirstPt := true;
      for j := 0 to pred(Values.Count) do begin
         TStr := UpperCase(ptTrim(Values.Strings[j]));
         if (TStr <> '') and (not (TStr[1] in ['A'..'Z'])) then begin
             val(BeforeSpecifiedCharacterANSI(TStr,SepChar,true,true),x,err);
             if err = 0 then begin
                val(TStr,y,err);
                if err = 0 then begin
                   xi := LinkGraph.GraphDraw.GraphX(x+xoff);
                   yi := LinkGraph.GraphDraw.GraphY(Y+Yoff);
                   if (FirstPt) then LinkGraph.Image1.Canvas.MoveTo(xi,yi)
                   else LinkGraph.Image1.Canvas.LineTo(xi,yi);
                   FirstPt := false;
                end;
             end;
         end;
      end;
      Values.Free;
   end;
end;


procedure Tdbtablef.Plotwithcolorsfromthisfield1Click(Sender: TObject);
begin
   GISdb[DBonTable].PlotFieldOnMap(SelectedColumn);
end;

procedure Tdbtablef.SetUpLinkGraph;
begin
   if (LinkGraph = Nil) then begin
      LinkGraph := tThisBaseGraph.Create(Application);
      LinkGraph.GraphDraw.MaxHorizAxis := 250;
      LinkGraph.GraphDraw.MaxVertAxis := 10;
      LinkGraph.GraphDraw.HorizLabel := 'Distance (m)';
      LinkGraph.GraphDraw.VertLabel := 'Elevation (m)';
      LinkGraph.Caption := 'Profiles';
      LinkGraph.SetUpGraphForm;
   end;
end;


procedure Tdbtablef.PlotXYFile1Click(Sender: TObject);
var
   fName : PathStr;
   xoff,yoff : float64;
   Color : tPlatformColor;
   LineWidth : integer;
begin
   Color := claBlack;
   LineWidth := 2;
   GISdb[DBonTable].MyData.GetLineColorAndWidth(Color,LineWidth);
   Petmar.PickLineSizeAndColor('Profile',Nil,Color,LineWidth);
   SetUpLinkGraph;
   LinkGraph.Image1.Canvas.Pen.Color := ConvertPlatformColorToTColor(Color);
   LinkGraph.Image1.Canvas.Pen.Width := LineWidth;
   with GISdb[DBonTable] do begin
      fName := ExtractFilePath(dbFullName) + GISdb[DBonTable].MyData.GetFieldByNameAsString('XY_FILE');
      if FileExists(fName) then begin
         xoff := GISdb[DBonTable].MyData.GetFieldByNameAsFloat('X_OFFSET');
         yoff := GISdb[DBonTable].MyData.GetFieldByNameAsFloat('Y_OFFSET');
         ReadDefault('x offset',xoff);
         ReadDefault('y offset',yoff);
         GISdb[DBonTable].MyData.Edit;
         GISdb[DBonTable].MyData.SetFieldByNameAsFloat('X_OFFSET',xoff);
         GISdb[DBonTable].MyData.SetFieldByNameAsFloat('Y_OFFSET',yoff);
         GISdb[DBonTable].MyData.SetLineColorAndWidth(Color,LineWidth);
         GISdb[DBonTable].MyData.Post;
         PlotSingleFile(fName,xoff,yoff);
      end;
   end;
end;


procedure Tdbtablef.Pointcloudpoints1Click(Sender: TObject);
{$ifDef ExPointCloud}
begin
{$Else}
var
   Lat,Long : float64;
   LasData : Las_Lidar.tLAS_data;
begin
   if GISdb[DBonTable].ValidLatLongFromTable(Lat,Long) then begin
      LasData := Las_Lidar.tLAS_data.Create(SlicerForm.MemoryPointCloud[1].LASsourceName);
      LasData.ExportIcesat(GISdb[DBonTable].TheMapOwner,Lat,Long);
      LasData.Destroy;
   end;
{$EndIf}
end;


procedure Tdbtablef.Pointcloudstatistics1Click(Sender: TObject);
{$ifDef ExPointCloud}
begin
{$Else}
var
   Lat,Long : float64;
begin
   if GISdb[DBonTable].ValidLatLongFromTable(Lat,Long) then SlicerForm.MemoryPointCloud[1].GetRegionStats(Lat,Long);
{$EndIf}
end;



procedure Tdbtablef.Pointdensitymatchmaparea1Click(Sender: TObject);
begin
   CreateGrid(cgPointDensity);
end;


procedure Tdbtablef.Pointinarea1Click(Sender: TObject);
var
   AreaDB,i,len,Num,rc : integer;
   TheField : ShortString;
   Lat,Long : float64;
   ft: TFieldType;
begin
   AreaDB := PickOpenGISDataBase('points within area', DBonTable);
   if (AreaDB > 0) and AreaShapeFile(GISdb[AreaDB].ShapeFileType) then begin
     TheField := GISdb[AreaDB].PickField('field to insert',[ftString,ftInteger,ftSmallInt],true);
     if (theField = '') then exit;
     ft := GISdb[AreaDB].MyData.GetFieldType(TheField);
     Len := GISdb[AreaDB].MyData.GetFieldLength(TheField);
     GISdb[DBonTable].AddFieldToDataBase(ft,theField,Len,0);
     StartProgress('Points in area');
     Num := 0;
     rc := GISdb[DBonTable].MyData.RecordCount;
     GISdb[DBonTable].MyData.First;
     while not GISdb[DBonTable].MyData.eof do begin
         UpdateProgressBar(Num/rc);
         inc(Num);
         GISdb[DBonTable].EmpSource.Enabled := false;
         GISdb[DBonTable].GetLatLongToRepresentRecord(Lat,Long);
         GISdb[AreaDB].EmpSource.Enabled := false;
         GISdb[AreaDB].MyData.First;
         for i := 1 to GISdb[AreaDB].MyData.RecordCount do begin
            if GISdb[AreaDB].aShapeFile.PointInRecord(GISdb[AreaDB].MyData.RecNo,Lat,Long) then begin
               GISdb[DBonTable].MyData.Edit;
               GISdb[DBonTable].MyData.SetFieldByNameAsString(TheField,GISdb[AreaDB].MyData.GetFieldByNameAsString(TheField));
               GISdb[DBonTable].MyData.Edit;
               break;
            end;
            GISdb[AreaDB].MyData.Next;
         end;
         GISdb[DBonTable].MyData.Next;
      end;

      GISdb[DBonTable].dbtablef.ShowStatus;
      GISdb[AreaDB].dbtablef.ShowStatus;
   end
   else MessageToContinue('Requires selection of open area shapefile');
end;


procedure Tdbtablef.Pointinrecord1Click(Sender: TObject);
var
   Lat,Long : float64;
   TStr : shortString;
begin
   with GISdb[DBonTable] do begin
      GetLatLn.GetLatLongDefault(TheMapOwner.MapDraw.PrimMapProj,'Point in record',Lat,Long);
      if aShapeFile.PointInRecord(MyData.RecNo,Lat,Long) then TStr := ''
      else TStr := ' not ';
      MessageToContinue( LatLongDegreeToString(Lat,Long,MDDef.OutPutLatLongMethod) + TStr + ' in record ' + IntToStr(MyData.RecNo));
   end;
end;



procedure Tdbtablef.DrawPointReflectanceGraph(var TheGraph : tThisBaseGraph; theTitle : shortString; Normalize : boolean = false);
{$IfDef ExAdvancedSats}
begin
{$Else}
var
   Lat,Long : float64;
   i,rc : integer;
begin
   with GISdb[DBonTable] do begin
      MDDef.SatMultiBandNormalize := Normalize;
      GISdb[DBonTable].MyData.First;
      EmpSource.Enabled := false;
      StartProgress('Reflectance ' + theTitle);
      i := 0;
      rc := GISdb[DBonTable].MyData.RecordCount;
      while not GISdb[DBonTable].MyData.eof do begin
         inc(i);
         if (i mod 25 = 0) then UpDateProgressBar(i/rc);

         if ValidLatLongFromTable(Lat,Long) then begin
            MultiGridArray[TheMapOwner.MapDraw.MultiGridOnMap].DrawPointGraph(TheGraph,Lat,Long,false);
         end;
         GISdb[DBonTable].MyData.Next;
      end;
      TheGraph.AutoScaleAndRedrawDiagram;
      TheGraph.Caption := theTitle;
      ShowStatus;
   end;
{$EndIf}
end;


procedure Tdbtablef.Pointreflectancespectra1Click(Sender: TObject);
{$IfDef ExSat}
begin
{$Else}
var
   aGraph : tThisBaseGraph;
   i : integer;
begin
   aGraph := tThisBaseGraph.Create(Application);
   if (Sender = Reflectancespectrasingleclass1) then begin
      for I := 1 to 255 do aGraph.GraphDraw.FileColors256[i] := GISdb[DBonTable].MyData.PlatformColorFromTable;
   end;
   DrawPointReflectanceGraph(aGraph,'Reflectances ' + GISdb[DBonTable].MyData.Filter,(Sender = Pointreflectancespectranormalized1));
{$EndIf}
end;

procedure Tdbtablef.Pointreflectancespectranormalized1Click(Sender: TObject);
begin
   Pointreflectancespectra1Click(Sender);
end;


procedure Tdbtablef.PointSeparation1Click(Sender: TObject);
begin
   {$IfDef ExAdvancedGIS}
   {$Else}
      GISdb[DBonTable].GISProportionalSymbols(dbasConnectTwoPointsInRec);
   {$EndIf}
end;


procedure Tdbtablef.Pointsinbox1Click(Sender: TObject);
begin
   CreateGrid(cgBox);
end;

procedure Tdbtablef.Pointslopebyalgorithm1Click(Sender: TObject);
var
   Lat,Long : float64;
begin
   if GISdb[DBonTable].ValidLatLongFromTable(Lat,Long) then begin
      DEMGlb[GISdb[DBonTable].theMapOwner.MapDraw.DEMonMap].SlopeMethodsReportFromLatLong(lat,Long);
   end;
end;

procedure Tdbtablef.PointsymbolsinDB1Click(Sender: TObject);
begin
    GISdb[DBonTable].GISProportionalSymbols(dbasPointsInDB);
end;

procedure Tdbtablef.Polestofocalplanes1Click(Sender: TObject);
{$IfDef ExGeology}
begin
{$Else}
var
   Num,xd,yd,rc : integer;
   Net : TNetForm;
begin
   with GISdb[DBonTable] do begin
      Net := TNetForm.Create(Application);
      Net.nd.NewNet;
      EmpSource.Enabled := false;
      Num := 0;
      rc := GISdb[DBonTable].MyData.RecordCount;
      if (Sender = Polestofocalplanes1) then Net.Caption := 'Poles to focal planes'
      else if (Sender = Focalplanesonstereonet1) then Net.Caption := 'Focal planes'
      else if (Sender = Dipdirectionsforfocalplanes1) then Net.Caption := 'Dip directions';

      GISdb[DBonTable].MyData.First;
      StartProgress('Poles');
      repeat
         inc(Num);
         UpdateProgressBar(Num/rc);
         if (Sender = Polestofocalplanes1) then begin
            Net.nd.PlotPointOnNet(PolePlot,MyData.GetFieldByNameAsInteger('FP1_DIP'),(MyData.GetFieldByNameAsInteger('FP1_STRIKE')+90),ASymbol(FilledBox,FocalMechanismColor,2),xd,yd);
            Net.nd.PlotPointOnNet(PolePlot,MyData.GetFieldByNameAsInteger('FP2_DIP'),(MyData.GetFieldByNameAsInteger('FP2_STRIKE')+90),ASymbol(FilledBox,FocalMechanismColor,2),xd,yd);
         end
         else if (Sender = Focalplanesonstereonet1) then begin
            Net.nd.GreatCircleOnNet(MyData.GetFieldByNameAsInteger('FP1_DIP'),MyData.GetFieldByNameAsInteger('FP1_STRIKE')+90,2,FocalMechanismColor);
            Net.nd.GreatCircleOnNet(MyData.GetFieldByNameAsInteger('FP2_DIP'),MyData.GetFieldByNameAsInteger('FP2_STRIKE')+90,2,FocalMechanismColor);
         end
         else if (Sender = Dipdirectionsforfocalplanes1) then begin
            Net.nd.PlotPointOnNet(LinePlot,MyData.GetFieldByNameAsInteger('FP1_DIP'),(MyData.GetFieldByNameAsInteger('FP1_STRIKE')+90),ASymbol(FilledBox,FocalMechanismColor,2),xd,yd);
            Net.nd.PlotPointOnNet(LinePlot,MyData.GetFieldByNameAsInteger('FP2_DIP'),(MyData.GetFieldByNameAsInteger('FP2_STRIKE')+90),ASymbol(FilledBox,FocalMechanismColor,2),xd,yd);
         end;
         GISdb[DBonTable].MyData.Next;
      until GISdb[DBonTable].MyData.EOF;
      Net.UpdateDisplay;
      ShowStatus;
   end;
   {$EndIf}
end;

procedure Tdbtablef.Positivenegative1Click(Sender: TObject);
begin
   {$IfDef ExAdvancedGIS}
   {$Else}
      GISdb[DBonTable].GISProportionalSymbols(dbasColorPosNeg);
   {$EndIf}
end;

procedure Tdbtablef.PanoramaSpeedButtonClick(Sender: TObject);
begin
   PanoramaView1Click(Sender);
end;

procedure Tdbtablef.PanoramaView1Click(Sender: TObject);
var
   DEMPersF : TThreeDview;
   wf : tWeaponsFan;
begin
   DEMPersF := Nil;
   wF := WeaponsTableBasicParametersToFan(GISdb[DBonTable].TheMapOwner.MapDraw.PrimMapProj,GISdb[DBonTable].MyData);
   SetUpPanoramaView(DEMPersF,wf.W_Lat,wf.W_Long,wf.W_Up,wf.W_Range,0,400,30,0,GISdb[DBonTable].TheMapOwner.MapDraw.DEMonMap,'Panorama from ' + wf.Fan_Name);
end;

procedure Tdbtablef.Parametercumulativedistribution1Click(Sender: TObject);
begin
   {$IfDef NoClustering}
   {$Else}
      ParamGraphForm(DBonTable);
   {$EndIf}
end;

procedure Tdbtablef.Pastecoordinatesfromclipboard1Click(Sender: TObject);
begin
    GISdb[DBonTable].MyData.Edit;
    GISdb[DBonTable].MyData.SetFieldByNameAsFloat(GISdb[DBonTable].LatFieldName,Clipboard_Lat);
    GISdb[DBonTable].MyData.SetFieldByNameAsFloat(GISdb[DBonTable].LongFieldName,Clipboard_Long);
    GISdb[DBonTable].MyData.Post;
    GISdb[DBonTable].RedrawLayerOnMap;
end;

procedure Tdbtablef.PercentageofcriteriawhereDEMisbest1Click(Sender: TObject);
begin
   {$IfDef ExDEMIXexperimentalOptions}
   {$Else}
      DEMIXwineContestCriterionGraph(dgPercentBest,DBonTable);
   {$EndIf}
end;

procedure Tdbtablef.Percentfield1Click(Sender: TObject);
var
   WantField : shortstring;
   Sum,value : float64;
begin
   GISdb[DBonTable].AddFieldToDataBase(ftFloat,'PERCENT',8,3);
   WantField := GISdb[DBonTable].PickField('for percentages',NumericFieldTypes);
   Sum := GISdb[DBonTable].FieldSum(WantField);
   GISdb[DBonTable].MyData.First;
   while not GISdb[DBonTable].MyData.eof do begin
      if GISdb[DBonTable].MyData.CarefullyGetFieldByNameAsFloat64(WantField,value) then begin
         GISdb[DBonTable].MyData.Edit;
         GISdb[DBonTable].MyData.SetFieldByNameAsFloat('PERCENT',100 * value / sum);
      end;
      GISdb[DBonTable].MyData.Next;
   end;
   ShowStatus;
end;

procedure Tdbtablef.PercentilesforCOPbycriterionforeachtile1Click(Sender: TObject);
begin
   InventoryPercentileByCriterionEachDEMIXtile(DBonTable);
end;

procedure Tdbtablef.Perimeterofeachrecord1Click(Sender: TObject);
begin
  GISdb[DBonTable].AddGeometry(agPerimeter);
end;

procedure Tdbtablef.Perimetersquaredoverarea1Click(Sender: TObject);
begin
   GISdb[DBonTable].AddGeometry(agP2A);
end;

procedure Tdbtablef.PerspectiveButtonClick(Sender: TObject);
var
   DEMPersF : TThreeDview;
   HFOV,Azimuth,Ending : float64;
   wf : tWeaponsFan;
begin
   with GISdb[DBonTable],MyData do begin
      wF := WeaponsTableBasicParametersToFan(TheMapOwner.MapDraw.PrimMapProj,MyData);
      if (wf.StartAngle < wf.EndAngle) then Ending := 360 + wf.EndAngle
      else Ending := wf.EndAngle;
      HFov := Ending - wf.StartAngle;
      Azimuth := 0.5 + (Ending + wf.StartAngle);
      DEMPersF := Nil;
      SetUpPanoramaView(DEMPersF,wf.W_Lat,wf.W_Long,wf.W_Up,wf.W_Range, PetMath.FindCompassAngleInRange(Azimuth),HFOV,30,0,1,'View from ' + wf.Fan_Name);
      DEMPersF.Caption:= 'View from ' + wf.Fan_Name;
      DEMPersF.View3D.MinPitch := wf.DownAngle;
      DEMPersF.View3D.MaxPitch := wf.UpAngle;
      DEMPersF.View3D.MaxRange := wf.W_Range;
      DEMPersF.View3D.MinRange := MDDef.DefWeaponsMinRange;
   end;
end;

procedure Tdbtablef.Perspectiveview1Click(Sender: TObject);
var
   Lat,Long,Height,Azimuth,VFOV,HFOV,Depth,Pitch : float64;
   DEMPersF : TThreeDview;
begin
   with GISdb[DBonTable] do begin
      if ValidLatLongFromTable(Lat,Long) then begin
        Height := GISdb[DBonTable].MyData.GetFieldByNameAsFloat('HEIGHT');
        Azimuth := GISdb[DBonTable].MyData.GetFieldByNameAsFloat('AZIMUTH');
        HFOV := GISdb[DBonTable].MyData.GetFieldByNameAsFloat('HFOV');
        VFOV := GISdb[DBonTable].MyData.GetFieldByNameAsFloat('VFOV');
        Depth := GISdb[DBonTable].MyData.GetFieldByNameAsFloat('DEPTH');
        Pitch := GISdb[DBonTable].MyData.GetFieldByNameAsFloat('PITCH');
        SetUpPanoramaView(DEMPersF,Lat,Long,Height,Depth,Azimuth,HFOV,VFOV,Pitch,TheMapOwner.MapDraw.DEMonMap);
      end;
   end;
end;

procedure Tdbtablef.Photolocations1Click(Sender: TObject);
begin
    GISdb[DBonTable].MyData.First;
    while not GISdb[DBonTable].MyData.eof do begin
       GISdb[DBonTable].OutlineCurrentViewOnMap;
       GISdb[DBonTable].MyData.Next;
    end;
end;

procedure Tdbtablef.Pickgridandaddnearestvalue1Click(Sender: TObject);
begin
   GISdb[DBonTable].AddAndFillFieldFromDEM(adPickNearest);
end;

procedure Tdbtablef.Picklinefromfile1Click(Sender: TObject);
{$IfDef ExSidescan}
begin
{$Else}
var
   SideImage : Tsideimage;
begin
   SideImage := TSideImage.Create(Application);
   SideImage.MapOwner := GISdb[DBonTable].TheMapOwner;
   SideImage.DisplayImage;
{$EndIf}
end;

procedure Tdbtablef.Picklineonmap1Click(Sender: TObject);
begin
   Button5Click(Sender);
end;

procedure Tdbtablef.PickParam1Click(Sender: TObject);
begin
{$IfDef ExDEMIXexperimentalOptions}
{$Else}
   DEMIXwineContestCriterionGraph(dgPick,DBonTable);
{$EndIf}
end;


procedure Tdbtablef.Pickpointsfortimesequentialseries1Click(Sender: TObject);
begin
   ChangeDEMNowDoing(FirstTimeSeries);
   DBEditting := DBonTable;
end;


procedure Tdbtablef.PlacevaluesinDEM1Click(Sender: TObject);
var
   WantField : ShortString;
   TStr : shortstring;
   Lat,Long,z: float64;
   i,rc : integer;
begin
   with GISdb[DBonTable] do begin
      WantField := PickField('unique values',[ftString,ftInteger,ftSmallInt]);
      GISdb[DBonTable].MyData.First;
      EmpSource.Enabled := false;
      StartProgress('Drop in bucket');
      i := 0;
      rc := GISdb[DBonTable].MyData.RecordCount;
      while not GISdb[DBonTable].MyData.EOF do begin
         if (i mod 5000) = 0 then begin
            UpdateProgressBar(i/rc);
            EmpSource.Enabled := false;
         end;
         inc(i);
         if ValidLatLongFromTable(Lat,Long) then begin
           TStr := GISdb[DBonTable].MyData.GetFieldByNameAsString(WantField);
           if (TStr <> '') then begin
              z := StrToFloat(TStr);
              DEMGlb[TheMapOwner.MapDraw.DEMonMap].SetGridElevationLatLongDegree(Lat,Long,z);
           end;
         end;
         GISdb[DBonTable].MyData.Next;
      end;
   end;
   ShowStatus;
   DEMGlb[GISdb[DBonTable].TheMapOwner.MapDraw.DEMonMap].SelectionMap.RespondToChangedDEM;
end;

procedure Tdbtablef.Plot1series1Click(Sender: TObject);
begin
   {$IfDef NoDBGrafs}
   {$Else}
       GISdb[DBonTable].MakeGraph(dbgtPlot1series1);
   {$EndIf}
end;

procedure Tdbtablef.PlotallXYFiles1Click(Sender: TObject);
var
   fName : PathStr;
   xoff,yoff : float64;
   Color,DefColor : tPlatFormColor;
   LineWidth,DefWidth : integer;

   procedure PlotThem(DefaultColors : boolean);
   begin
      with GISdb[DBonTable] do begin
         GISdb[DBonTable].MyData.First;
         while not GISdb[DBonTable].MyData.EOF do begin
           fName := ExtractFilePath(dbFullName) + GISdb[DBonTable].MyData.GetFieldByNameAsString('XY_FILE');
           if FileExists(fName) then begin
              xoff := GISdb[DBonTable].MyData.GetFieldByNameAsFloat('X_OFFSET');
              yoff := GISdb[DBonTable].MyData.GetFieldByNameAsFloat('Y_OFFSET');
              Color := DefColor;
              LineWidth := DefWidth;
              if (Sender = PlotallXYFiles1) then begin
                 GISdb[DBonTable].MyData.GetLineColorAndWidth(Color,LineWidth);
              end;
              LinkGraph.Image1.Canvas.Pen.Color := ConvertPlatformColorToTColor(Color);
              LinkGraph.Image1.Canvas.Pen.Width := LineWidth;
              PlotSingleFile(fName,xoff,yoff);
            end;
            GISdb[DBonTable].MyData.Next;
         end;
      end;
   end;


begin
   SetUpLinkGraph;
   LinkGraph.SetUpGraphForm;
   DefColor := claDarkGrey;
   DefWidth := 1;
   if Sender = PlotallXYFiles1 then Petmar.PickLineSizeAndColor('Profile',Nil,DefColor,DefWidth);
   PlotThem(true);
   if Sender = PlotallXYFiles1 then PlotThem(False);
end;


procedure Tdbtablef.EvaluateXYProfiles1Click(Sender: TObject);
var
   fName : PathStr;
   xoff,yoff : float64;
   ch : char;
   BMP : tMyBitmap;
begin
   PlotallXYFiles1Click(Sender);
   CopyImageToBitmap(LinkGraph.Image1,bmp);
   with GISdb[DBonTable],MyData do begin
      First;
      while not EOF do begin
        LinkGraph.Image1.Picture.Graphic := bmp;
        fName := ExtractFilePath(dbFullName) + GetFieldByNameAsString('XY_FILE');
        if FileExists(fName) then begin
           xoff := GetFieldByNameAsFloat('X_OFFSET');
           yoff := GetFieldByNameAsFloat('Y_OFFSET');
           LinkGraph.Image1.Canvas.Pen.Color := clRed;
           LinkGraph.Image1.Canvas.Pen.Width := 3;
           PlotSingleFile(fName,xoff,yoff);
           Edit;
           if AnswerIsYes('Good profile') then ch := 'Y' else ch := 'N';
           SetFieldByNameAsString('GOOD_PROF',ch);
         end;
         Next;
      end;
   end;
   BMP.Free;
end;


procedure Tdbtablef.Evaluationrangebycriterion1Click(Sender: TObject);
begin
   EvalRangeAndStatsByCriterion(DBonTable);
end;

procedure Tdbtablef.Evaluationrangeforcriterion1Click(Sender: TObject);
begin
   EvalRangeAndBestEvalForCriterion(DBonTable);
end;

procedure Tdbtablef.PlotClick(Sender: TObject);
begin
   {$IfDef NoDBGrafs}
   {$Else}
      GISdb[DBonTable].MakeGraph(dbgtPlot);
   {$EndIf}
end;


procedure Tdbtablef.Plotcoveragecircles1Click(Sender: TObject);
var
   Lat,Long : float64;
   fName : PathStr;
   Table : tMyData;
begin
   with GISdb[DBonTable] do begin
     FName := NextFileNumber(MDTempDir, 'Range_circles', DefaultDBExt);
     Make_Tables.MakeRangeCircleTable(FName);
     Table := tMyData.Create(FName);
     GISdb[DBonTable].MyData.First;
     while not GISdb[DBonTable].MyData.eof do begin
         if ValidLatLongFromTable(Lat,Long) then begin
            Table.Insert;
            Table.SetFieldByNameAsFloat('LAT',Lat);
            Table.SetFieldByNameAsFloat('LONG',Long);
            Table.SetFieldByNameAsFloat('RANGE',MyData.GetFieldByNameAsFloat('RANGE'));
            Table.SetFieldByNameAsInteger('COLOR',clRed);
            Table.SetFieldByNameAsInteger('LINE_WIDTH',3);
            Table.SetFieldByNameAsString('NAME',MyData.GetFieldByNameAsString('NAME'));
            Table.Post;
            TheMapOwner.MapDraw.DrawRangeCircleLatLong(TheMapOwner.Image1.Canvas,Lat,Long,MyData.GetFieldByNameAsFloat('RANGE'),clRed,3,true);
         end;
         GISdb[DBonTable].MyData.Next;
     end;
     TheMapOwner.LoadDataBaseFile(fName);
   end;
end;

procedure Tdbtablef.MarkallN1Click(Sender: TObject);
begin
   GISdb[DBonTable].FillUseField(false,'N');
end;

procedure Tdbtablef.MarkallY1Click(Sender: TObject);
begin
   GISdb[DBonTable].FillUseField(false,'Y');
end;

procedure Tdbtablef.Markanddeleteallrecordspriortothisone1Click( Sender: TObject);
var
   ThisRec : integer;
begin
   ThisRec := GISdb[DBonTable].MyData.GetFieldByNameAsInteger('REC_ID');
   GISdb[DBonTable].ApplyGISFilter('REC_ID < ' + IntToStr(ThisRec));
   GISdb[DBonTable].RedrawLayerOnMap;
   ShowStatus;
end;

procedure Tdbtablef.Markholes1Click(Sender: TObject);
var
   i,j : integer;
   Lat,Long : float64;
begin
   with GISdb[DBonTable] do begin
       StartProgress('Mask');
       for j := 0 to pred(TheMapOwner.MapDraw.MapYSize) do begin
          UpdateProgressBar(j/pred(TheMapOwner.MapDraw.MapYSize));
          for i := 0 to pred(TheMapOwner.MapDraw.MapXSize) do begin
             TheMapOwner.MapDraw.ScreenTolatLongDegree(i,j,Lat,Long);
             if aShapeFile.PointInRecord(MyData.RecNo,Lat,Long) then begin
             end
             else begin
                TheMapOwner.Image1.Canvas.Pixels[i,j] := clBlack;
             end;
          end;
       end;
       EndProgress;
   end;
end;

procedure Tdbtablef.Mask2Click(Sender: TObject);
{$IfDef ExAdvancedGIS}
begin
{$Else}
begin
   if MaskMap(GISdb[DBonTable].TheMapOwner,false,true,1,DBonTable,true) then begin
       if AreaShapeFile(GISdb[DBonTable].ShapeFileType) then begin
          GISdb[DBonTable].MaskIn := MDDef.ShapeMaskNearInsideAreas;
          GISdb[DBonTable].MaskingDistance := MDDef.ShapeAreaBufferDist;
       end
       else if LineShapeFile(GISdb[DBonTable].ShapeFileType) then begin
          GISdb[DBonTable].MaskIn := MDDef.ShapeMaskNearLines;
          GISdb[DBonTable].MaskingDistance := MDDef.ShapeLineBufferDist;
       end
       else begin
          GISdb[DBonTable].MaskIn := MDDef.ShapeMaskNearPoints;
          GISdb[DBonTable].MaskingDistance := MDDef.ShapePointBufferDist;
       end;
       GISdb[DBonTable].dbOpts.ShowBuffers := true;
       GISdb[DBonTable].dbOpts.Opacity := MDDef.MaskOpacity;
       GISdb[DBonTable].RedrawLayerOnMap;
  end;
{$EndIf}
end;

procedure Tdbtablef.MaskallopenDEMgrids1Click(Sender: TObject);
var
   bb : sfBoundBox;
begin
   bb := GISdb[DBonTable].MyData.GetRecordBoundingBox;
   MaskAllDEMsWithGeoBoundingBox(bb);
end;


procedure Tdbtablef.MaskClick(Sender: TObject);
var
   i,WantedDEM,rc : integer;
   Lat,Long : float64;
   z : float32;
   NewField : ShortString;
   ch : AnsiChar;
begin
   with GISdb[DBonTable],MyData do begin
      GetDEM(WantedDEM,false,'DEM for masking');
      if (WantedDEM = 0) then exit;
      
      GISdb[DBonTable].ClearGISFilter;
      NewField := 'MASK_DEM1';
      if GISdb[DBonTable].MyData.FieldExists(NewField) then NewField := 'MASK_DEM2';
      AddFieldToDataBase(ftString,NewField,1,0);
      EmpSource.Enabled := false;
      GISdb[DBonTable].MyData.First;
      StartProgress('Mask');
      i := 0;
      rc := GISdb[DBonTable].MyData.RecordCount;
      while not GISdb[DBonTable].MyData.eof do begin
         inc(i);
         if (i mod 500 = 0) then UpdateProgressBar(i/rc);
         Lat := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(LatFieldName);
         Long := GISdb[DBonTable].MyData.GetFieldByNameAsFloat(LongFieldName);
         if DEMGlb[WantedDEM].GetElevFromLatLongDegree(Lat,Long,z) then ch := 'Y' else ch := 'N';
         GISdb[DBonTable].MyData.Edit;
         GISdb[DBonTable].MyData.SetFieldByNameAsString(NewField,ch);
         GISdb[DBonTable].MyData.Next;
      end;
      EndProgress;
      GISdb[DBonTable].MyData.ApplyFilter(NewField + '=' + QuotedStr('Y'));
      GISdb[DBonTable].RedrawLayerOnMap;
      ShowStatus;
   end;
end;

procedure Tdbtablef.Maskdatabasewithgazetteer1Click(Sender: TObject);
begin
   MaskDEMfromshapefile1Click(Nil);
end;


procedure Tdbtablef.MaskDatabaseWithShapefile1Click(Sender: TObject);
var
   MaskDB{,x,y,rc} : integer;
   PIP : tPointInPolygon;
   NameField,MaskFieldName : string10;
begin
   if Sender = Findpointsinareas1 then PIP := pipLabels
   else PIP := pipSetMask;
   if PIP = pipLabels then begin
      NameField := 'NAME';
      GISdb[DBonTable].AddFieldToDataBase(ftString,'MASK_AREA',GISdb[DBonTable].MyData.GetFieldLength(NameField) ,0);
      MaskFieldName := 'MASK_AREA';
   end
   else MaskFieldName := GISdb[DBonTable].GetMaskFieldName;
   MaskDB := PickOpenGISDataBase('Mask area database');
   if (MaskDB = -99) then exit;
   MarkPointInPolygon(Pip,DBonTable,MaskDB,NameField,MaskFieldName);
   ShowStatus;
end;


procedure Tdbtablef.MaskDEMfromshapefile1Click(Sender: TObject);
{$IfDef ExAdvancedGIS}
begin
{$Else}
var
   SingleRecord,UseDEM : integer;
begin
   {$IfDef RecordMaskDEMShapeFile} WriteLineRoDebugFile('MaskDEMfromshapefile1Click in'); {$EndIf}
   if GetDEM(UseDEM,true,'masking from DB') then begin
     {$IfDef RecordMaskDEMShapeFile} WriteLineRoDebugFile('UseDEM=' + IntToStr(UseDEM) + '  ' + DEMGlb[UseDEM].AreaName); {$EndIf}
      GISdb[DBonTable].TheMapOwner := DEMGlb[UseDEM].SelectionMap;
      if (Sender = MaskDEMgrid1) then begin
         SingleRecord := GISdb[DBonTable].MyData.RecNo;
         Zoommaptorecord1Click(Sender);
         {$IfDef RecordCurrentRecord} WriteLineRoDebugFile('Tdbtablef.MaskDEMfromshapefile1Click, RecNo=' + IntToStr(SingleRecord)); {$EndIf}
      end
      else SingleRecord := 0;
      GetMaskingOptions(false,(Sender <> Maskdatabasewithgazetteer1));
      {$IfDef RecordMaskDEMShapeFile} WriteLineRoDebugFile('Got masking options'); {$EndIf}
      MaskDEMFromShapeFile(UseDEM,DBonTable,(Sender <> Nil),MDDef.MaskShapesIn,SingleRecord,MDDef.MaskDistance);
      if (DEMGlb[UseDEM].SelectionMap <> Nil) and (GISdb[DBonTable].TheMapOwner.MapDraw.ValidDEMonMap and (DEMGlb[UseDEM].LandCoverGrid)) then begin
         DEMGlb[UseDEM].SelectionMap.NLCDLegend1Click(Nil);
      end;
      ShowStatus;
   end
   else begin
      {$IfDef RecordMaskDEMShapeFile} WriteLineRoDebugFile('Failed to get grid'); {$EndIf}
   end;
   {$IfDef RecordMaskDEMShapeFile} WriteLineRoDebugFile('MaskDEMfromshapefile1Click out'); {$EndIf}
{$EndIf}
end;


procedure Tdbtablef.MaskDEMgrid1Click(Sender: TObject);
begin
   MaskDEMfromshapefile1Click(Sender);
end;

procedure Tdbtablef.Maskfieldbystring1Click(Sender: TObject);
var
   TStr,FieldContent : ShortString;
   MatchCase : boolean;
begin
   GISdb[DBonTable].AddFieldToDataBase(ftString,'MASK',1);
   TStr := '';
   Petmar.GetString('sub-string to match',TStr,false,ReasonableTextChars);
   GISdb[DBonTable].EmpSource.Enabled := false;
   MatchCase := AnswerIsYes('Match case');
   if MatchCase then TStr := UpperCase(TStr);

   GISdb[DBonTable].MyData.First;
   while not GISdb[DBonTable].MyData.eof do begin
      FieldContent := GISdb[DBonTable].MyData.GetFieldByNameAsString(SelectedColumn);
      if MatchCase then FieldContent := UpperCase(FieldContent);
      GISdb[DBonTable].MyData.Edit;
      if StrUtils.AnsiContainsStr(FieldContent,TStr) then GISdb[DBonTable].MyData.SetFieldByNameAsString('MASK','Y')
      else GISdb[DBonTable].MyData.SetFieldByNameAsString('MASK','N');
      GISdb[DBonTable].MyData.Next;
   end;
   ShowStatus;
end;

procedure Tdbtablef.Maximum1Click(Sender: TObject);
begin
    GISdb[DBonTable].AddMultiFieldStats('Sum',mfsMax);
end;

procedure Tdbtablef.MDYYYY1Click(Sender: TObject);
begin
   MMDDYYYY1Click(Sender);
end;


procedure Tdbtablef.Mean1Click(Sender: TObject);
begin
    GISdb[DBonTable].AddMultiFieldStats('Sum',mfsMean);
end;

procedure Tdbtablef.Meanandmedianhistograms1Click(Sender: TObject);
begin
{$IfDef ExDEMIXexperimentalOptions}
{$Else}
   //DEMIXMeanMedianHistograms(dbOnTable);
   DEMIXMeanMedianModeHistograms(dbOnTable);
{$EndIf}
end;

procedure Tdbtablef.Meanfilterfilter1Click(Sender: TObject);
begin
   Medianfilterfield1Click(Sender);
end;

procedure Tdbtablef.MeanstdallDBs1Click(Sender: TObject);
var
   FieldDesired : ShortString;
   Missing,i  : int64;
   zs : ^bfarray32;
   fName : PathStr;
   Findings : tStringList;
   MomentVar : tMomentVar;
   Min,Max : float64;
begin
   FieldDesired := GISdb[DBonTable].PickField('Field for mean/std',NumericFieldTypes);
   if (FieldDesired = '') then exit;
   Findings := tStringList.Create;
   Findings.Add('NAME,N_PTS,MEAN,STD_DEV,SKEWNESS,KURTOSIS,MEDIAN,MIN,MAX');
   StartProgress('Stats');
   for i := 1 to MaxDataBase do begin
      UpdateProgressBar(i/MaxDataBase);
      if (GISdb[i] <> nil) then begin
         New(zs);
         GetFieldValuesInArray(GISdb[i].MyData,FieldDesired,zs^,MomentVar.Npts,Missing,Min,Max);
         if (MomentVar.NPts > 1)  then begin
            moment(zs^,MomentVar,msAll);
            Findings.Add(GISdb[i].DBName + ',' + IntToStr(MomentVar.Npts) + ',' + RealToString(MomentVar.mean,-12,-4) + ',' +
                 RealToString(MomentVar.std_dev,-12,-4) + ',' + RealToString(MomentVar.skew,-12,-4) + ',' +
                 RealToString(MomentVar.curt,-12,-4) + ',' + RealToString(MomentVar.Median,-12,-4) + ',' +
                 RealToString(min,-12,-4) + ',' +  RealToString(max,-12,-4) );
         end;
         Dispose(zs);
      end;
   end;
   EndProgress;
   fName := MDTempDir + 'summary_stats.csv';
   GISdb[DBonTable].theMapOwner.StringListToLoadedDatabase(Findings,fName);
end;


procedure Tdbtablef.Meanwidth1Click(Sender: TObject);
begin
   GISdb[DBonTable].AddGeometry(agMeanWidth);
end;

procedure Tdbtablef.Median1Click(Sender: TObject);
begin
    GISdb[DBonTable].AddMultiFieldStats('Sum',mfsMedian);
end;

procedure Tdbtablef.Medianfilterfield1Click(Sender: TObject);
var
   Field,NewField,TStr,TStr2 : ShortString;
   i,FilterSize,WinSize,lNew,lPre : integer;
   Values : ^Petmath.bfarray32;
   NPts : int64;
   z : float32;
begin
   with GISdb[DBonTable],MyData do begin
      if Sender = Meanfilterfilter1 then begin
         TStr := 'Mean';
         TStr2 := 'MEAN';
      end
      else begin
         TStr := 'Median';
         TStr2 := 'MED';
      end;

      Field := GISDB[DBonTable].PickField(Tstr + ' filter',NumericFieldTypes);
      NewField := TStr2 + '_' + Field;
      NewField := GetFieldNameForDB(TStr + ' filtered field name',True,NewField);
      FilterSize := 5;
      ReadDefault('Full filter size (odd number)',FilterSize);
      WinSize := FilterSize div 2;
      lpre := GISdb[DBonTable].MyData.GetFieldPrecision(Field);
      lnew := GISdb[DBonTable].MyData.GetFieldLength(Field);
      GISdb[DBonTable].AddFieldToDataBase(ftFloat,NewField,lNew,lPre);
      EmpSource.Enabled := false;
      New(Values);
      GetPointArrayForDBField(Field,Values^,NPts);
      if (Sender = Meanfilterfilter1) then begin
         MeanFilterArray(Values^,NPts,FilterSize);
      end
      else begin
         MedianFilterArray(Values^,NPts,FilterSize);
      end;

      EmpSource.Enabled := false;
      First;
      StartProgress('Filter');
      for i := 0 to pred(Npts) do begin
         if i mod 25 = 0 then UpdateProgressBar(i/Npts);
         Edit;
         z := Values^[i];
         if round(z) = MaxSmallInt then SetFieldByNameAsString(NewField,'')
         else SetFieldByNameAsFloat(NewField,z);
         Next;
      end;
      Dispose(Values);
      ShowStatus;
   end;
end;


procedure Tdbtablef.Mergedatabase1Click(Sender: TObject);
var
   DefaultFilter : byte;
   ThePath : PathStr;
   FileNames : tStringList;
begin
    ThePath := ExtractFilePath(GISdb[DBonTable].dbFullName);
    FileNames := tStringList.Create;
    FileNames.Add(ThePath);
    DefaultFilter := 0;
    if GetMultipleFiles('Point data bases to merge into this file',DBNameMask,FileNames,DefaultFilter) then begin
       GISdb[DBonTable].MergeDataBases(FileNames);
    end;
    FileNames.Free;
    GISdb[DBonTable].AddSequentialIndex(RecNoFName,true);
    ShowStatus;
end;

procedure Tdbtablef.MilitarytimetoHHMMSS1Click(Sender: TObject);
var
   TStr : ShortString;
begin
   with GISdb[DBonTable] do begin
      AddFieldToDataBase(ftString,'TIME_STR',8,0);
      EmpSource.Enabled := false;
      GISdb[DBonTable].MyData.First;
      while not GISdb[DBonTable].MyData.EOF do begin
         GISdb[DBonTable].MyData.Edit;
         TStr := GISdb[DBonTable].MyData.GetFieldByNameAsString('TIME');
         TStr := Copy(TStr,1,2) + ':' + Copy(TStr,3,2) + ':00';
         GISdb[DBonTable].MyData.SetFieldByNameAsString('TIME_STR',TStr);
         GISdb[DBonTable].MyData.Next;
      end;
      ShowStatus;
   end;
end;

procedure Tdbtablef.Minimum1Click(Sender: TObject);
begin
    GISdb[DBonTable].AddMultiFieldStats('Sum',mfsMin);
end;

procedure Tdbtablef.MissingData0Click(Sender: TObject);
begin
   IgnoreMissingData1Click(Sender);
end;

procedure Tdbtablef.MMDDYYYY1Click(Sender: TObject);
var
   TStr : ANSIString;
   Month,Day,Year : integer;
begin
   with GISdb[DBonTable] do begin
      EmpSource.Enabled := false;
      GISdb[DBonTable].MyData.First;
      while not GISdb[DBonTable].MyData.Eof do begin
         TStr := GISdb[DBonTable].MyData.GetFieldByNameAsString('DATE_LABEL');
         if (TStr <> '') then begin
            GISdb[DBonTable].MyData.Edit;
            Month := StrToInt(Petmar_types.BeforeSpecifiedCharacterANSI(TStr,'/',true,true));
            Day := StrToInt(Petmar_types.BeforeSpecifiedCharacterANSI(TStr,'/',true,true));
            Year := StrToInt(TStr);
            if Year < 100 then begin
               if Year < 20 then Year := Year + 2000
               else Year := Year + 1900;
            end;
            if (Sender = MMDDYYYY1) then
               TStr := AddDayMonthLeadingZero(Month) + '/' + AddDayMonthLeadingZero(Day) + '/' + IntToStr(Year)
            else if (Sender = YYYYMMDD1) then
               TStr := IntToStr(Year) + '/' + AddDayMonthLeadingZero(Month) + '/' + AddDayMonthLeadingZero(Day)
            else TStr := IntToStr(Month) + '/' + IntToStr(Day) + '/' + IntToStr(Year);
            GISdb[DBonTable].MyData.SetFieldByNameAsString('DATE_LABEL',TStr);
         end;
         GISdb[DBonTable].MyData.Next;
      end;
      ShowStatus;
   end;
end;

procedure Tdbtablef.Modestandarddeviationplots1Click(Sender: TObject);
begin
{$IfDef ExDEMIXexperimentalOptions}
{$Else}
   ModeSTDPlot(DBonTable);
{$EndIf}
end;

procedure Tdbtablef.Monthlyprecipitation1Click(Sender: TObject);
begin
{$IfDef ExGeography}
{$Else}
    GISdb[DBonTable].StartClimateDisplay(dbasMonthlyRain);
{$EndIf}
end;

procedure Tdbtablef.Monthlytemperatures1Click(Sender: TObject);
begin
{$IfDef ExGeography}
{$Else}
    GISdb[DBonTable].StartClimateDisplay(dbasMonthlyTemp);
{$EndIf}
end;

procedure Tdbtablef.Monthlywinds1Click(Sender: TObject);
begin
   {$If Defined(NoDBGrafs) or Defined(ExGeograph)}
   {$Else}
      GISdb[DBonTable].MonthlyWindPlotCurrentPoint;
   {$EndIf}
end;


procedure Tdbtablef.Movie1Click(Sender: TObject);
{$IfDef exmovies}
begin
{$Else}
var
   MovieList : tStringList;
   FName     : PathStr;
begin
   with GISdb[DBonTable] do begin
      MovieList := TStringList.Create;
      GISdb[DBonTable].MyData.First;
      while not GISdb[DBonTable].MyData.eof do begin
         fName := GISdb[DBonTable].MyData.GetFieldByNameAsString(ImageFieldNames[1]);
         CopyFile(ExtractFilePath(dbFullName)+fName,MovieDir + fName);
         MovieList.Add(FName);
         GISdb[DBonTable].MyData.Next;
      end;
      FName := MovieDir  + 'DB_Images.MOV';
      MovieList.SaveToFile(FName);
      MovieList.Free;
      PetImage.MakeMovie(ExtractFileName(Fname));
   end;
{$EndIf}
end;

procedure Tdbtablef.Movie2Click(Sender: TObject);
var
   fName : PathStr;
begin
   fName := ExtractFilePath(GISdb[DBonTable].DBFullName) + GISdb[DBonTable].MyData.GetFieldByNameAsString('MOVIE');
   ExecuteFile(fName, '', '');
end;


procedure Tdbtablef.ColorfieldinDB1Click(Sender: TObject);
begin
   GISdb[DBonTable].GISProportionalSymbols(dbasColorField);
end;

procedure Tdbtablef.ColorfieldinjoinedDB1Click(Sender: TObject);
begin
   GISdb[DBonTable].GISProportionalSymbols(dbasColorJoinField);
end;

procedure Tdbtablef.ColorfromKoppencategory1Click(Sender: TObject);
{$IfDef ExGeography}
begin
{$Else}
var
   Color : tPlatformColor;
begin
   with GISdb[DBonTable] do begin
     AddFieldToDataBase(ftInteger,'COLOR',8,0);
     ColorPresent := true;
     EmpSource.Enabled := false;
     GISdb[DBonTable].MyData.First;
     while not GISdb[DBonTable].MyData.EOF do begin
        if GetKoppenColor(MyData.GetFieldByNameAsString('CLASS'),Color) then begin
           GISdb[DBonTable].MyData.Edit;
           GISdb[DBonTable].MyData.SetColorFromPlatformColor(Color);
        end;
        GISdb[DBonTable].MyData.Next;
     end;
     EmpSource.Enabled := true;
   end;
{$EndIf}
end;


procedure Tdbtablef.ColorfromRGBfloatfields1Click(Sender: TObject);
begin
   with GISdb[DBonTable] do begin
      AddFieldToDataBase(ftInteger,'COLOR',8,0);
      ColorPresent := true;
      EmpSource.Enabled := false;
      GISdb[DBonTable].MyData.First;
      While not GISdb[DBonTable].MyData.EOF do begin
          Color := GISdb[DBonTable].MyData.TColorFromTable;
          GISdb[DBonTable].MyData.Edit;
          GISdb[DBonTable].MyData.SetFieldByNameAsInteger('COLOR',Color);
          GISdb[DBonTable].MyData.Next;
      end;
      ShowStatus;
   end;
end;

procedure Tdbtablef.ColorfromRGBintegerfields1Click(Sender: TObject);
begin
    ColorfromRGBfloatfields1Click(Sender);
end;

procedure Tdbtablef.Multipartrecords1Click(Sender: TObject);
var
   Results : tStringList;
   TStr : ShortString;
   CheckField : ShortString;
   i,j,rc,Outs,Holes : integer;
begin
   with GISdb[DBonTable] do begin
       CheckField := GISDB[DBonTable].PickField('Field to name',[ftString,ftInteger]);
       Results := tStringList.Create;
       EmpSource.Enabled := false;
       GISdb[DBonTable].MyData.First;
       StartProgress('Check');
       i := 0;
       rc := GISdb[DBonTable].MyData.RecordCount;
       while not GISdb[DBonTable].MyData.EOF do begin
          inc(i);
          if (i mod 100 = 0) then UpdateProgressBar(i/rc);
          aShapeFile.GetLineCoords(MyData.RecNo);
          TStr := '';
          if (aShapeFile.CurrentPolyLineHeader.NumParts > 1) then begin
             if AreaShapeFile(ShapeFileType) then begin
                 aShapeFile.AreasCounterClockwise;
                 Outs := 0;
                 Holes := 0;
                 with aShapeFile.CurrentPolyLineHeader do begin
                    for j := 1 to NumParts  do begin
                       if aShapeFile.CurrentLinePartsCCW[j] then inc(holes)
                       else inc(Outs);
                    end;
                    if (Holes > 0) then TStr := '   perimeter=' + IntToStr(outs) + '  holes=' + IntToStr(Holes);
                 end;
             end;
             if (Sender = Recordswithholes1) then begin
                if Holes > 0 then Results.Add(MyData.GetFieldByNameAsString(CheckField) + '  holes=' + IntToStr(Holes));
             end
             else Results.Add(MyData.GetFieldByNameAsString(CheckField) + '  Parts=' + IntToStr(aShapeFile.CurrentPolyLineHeader.NumParts) + '  ' + TStr);
          end;
          GISdb[DBonTable].MyData.Next;
       end;
       ShowStatus;
       Petmar.DisplayAndPurgeStringList(Results,'Multi-part records in ' + dbName);
   end;
end;

procedure Tdbtablef.Multiplegraphmatrix1Click(Sender: TObject);
var
   fName : PathStr;
   Table : tMyData;
   xp,yp,Records,i,j : integer;
   Max : float64;
   fN,lN  : array[1..25] of shortstring;
begin
{$IfDef NoDBGrafs}
{$Else}
   with GISdb[DBonTable] do begin
      fName := 'C:\mapdata\0--current_projects\pa_point_cloud\combined_los_definitions.dbf';
      if not Petmar.GetFileFromDirectory('Field groupings',DBNameMask,fName) then exit;
      Table := tMyData.Create(fName);
      Records := Table.RecordCount;
      ForceXMax := 0;
      for i := 1 to Records do begin
         fN[i] := Table.GetFieldByNameAsString('FIELD_NAME');
         lN[i] := Table.GetFieldByNameAsString('LABEL_NAME');
         EmpSource.Enabled := false;
         Max := GISdb[DBonTable].MyData.FindFieldMax(fn[i]);
         if (Max > ForceXMax) then ForceXMax := Max;
         Table.Next;
      end;
      Table.Destroy;
      ForceYMax := ForceXMax;

      SaveBackupDefaults;
      MDDef.DefaultGraphXSize := 400;
      MDDef.DefaultGraphYSize := 260;
      CreateSmallGraph := true;

      EmpSource.Enabled := false;
      PetImage.CreateBitmap(AllGraphBitmap,(Records*MDDef.DefaultGraphXSize div 2){ + 50},(Records*MDDef.DefaultGraphYSize div 2) {+ 45});
      AllGraphBitmap.Canvas.Font.Style := [fsBold];
      AllGraphBitmap.Canvas.Font.Size := 10;
      for i := 1 to records do begin
         dbOpts.XField := fn[i];
         xdrawspot := pred(i) * MDDef.DefaultGraphXSize div 2;
         for j := 1 to records do begin
            ydrawspot := pred(j) * MDDef.DefaultGraphYSize div 2;
            if (i = j) then with AllGraphBitmap.Canvas do begin
               xp := XDrawSpot+(MDDef.DefaultGraphXSize div 4)-(TextWidth(ln[j]) div 2);
               Yp := yDrawSpot+(MDDef.DefaultGraphYSize div 4)-10;
               TextOut(xp,yp,ln[j]);
            end
            else begin
               EmpSource.Enabled := false;
               dbOpts.YField := fn[j];
               GISdb[DBonTable].MakeGraph(dbgtMultiplegraphmatrix1);
            end;
         end;
      end;
      PetImage_form.DisplayBitmap(AllGraphBitmap,ExtractFileNameNoExt(fName));
      AllGraphBitmap.Free;
   end;
   RestoreBackupDefaults;
   ShowStatus;
{$EndIf}
end;


procedure Tdbtablef.Multiplelinearregression1Click(Sender: TObject);
var
   Table : tMyData;
   fName : PathStr;
   NumFields,i,Count,rc : integer;
   Sum : float64;
   Fields : array[1..25] of ShortString;
   Coefs  : array[1..25] of float64;
begin
   FName := ExtractFilePath(GISdb[DBonTable].dbFullName);
   if Petmar.GetFileFromDirectory('multiple linear regression coefficients','*.dbf',fName) then begin
      Table := tMyData.Create(fName);
      NumFields := 0;
      while not Table.eof do begin
         inc(NumFields);
         Fields[NumFields] := Table.GetFieldByNameAsString('FIELD');
         Coefs[NumFields] := Table.GetFieldByNameAsFloat('VALUE');
         Table.Next;
      end;
      Table.Destroy;
     GISdb[DBonTable].AddFieldToDataBase(ftFloat,'PREDICTED',18,6);
     GISdb[DBonTable].EmpSource.Enabled := false;
     GISdb[DBonTable].MyData.First;
     Count := 0;
     rc := GISdb[DBonTable].MyData.RecordCount;
     StartProgress('Multiple linear regression');

     while not GISdb[DBonTable].MyData.eof do begin
        if (Count mod 5000 = 0) then  UpdateProgressBar(Count / rc);
        inc(Count);
        Sum := 0;
        for i := 1 to NumFields do Sum := Sum +  GISdb[DBonTable].MyData.GetFieldByNameAsFloat(Fields[i]) * Coefs[i];
        GISdb[DBonTable].MyData.Edit;
        GISdb[DBonTable].MyData.SetFieldByNameAsFloat('PREDICTED',Sum);
        GISdb[DBonTable].MyData.Next;
     end;
     ShowStatus;
   end;
end;

procedure Tdbtablef.Multiplytwofields1Click(Sender: TObject);
begin
   Sumtwofields1Click(Sender);
end;

procedure Tdbtablef.Mutliple1Click(Sender: TObject);
begin
   SingleFieldArithmetic(DBonTable,sfaMult,'');
end;



procedure Tdbtablef.Fieldstatistics1Click(Sender: TObject);
begin
   GISdb[DBonTable].DisplayFieldStatistics(SelectedColumn);
   ShowStatus;
end;

procedure Tdbtablef.Centroidofeachrecord1Click(Sender: TObject);
begin
  GISdb[DBonTable].AddGeometry(agCentroid);
end;

procedure Tdbtablef.Fieldpercentiles1Click(Sender: TObject);
var
   Results : tStringList;
   //v : float64;
   i : integer;
begin
   Results := tStringList.Create;
   Results.Add('Percent,Value');
   for i :=  0 to 1000 do begin
      Results.Add(RealToString(0.1 * i,8,1) + ',' + RealToString(GISdb[DBonTable].GetFieldPercentile(SelectedColumn,0.1 * i),-18,-6));
   end;
   Petmar.DisplayAndPurgeStringList(Results,'Percentiles for ' + SelectedColumn);
end;


procedure Tdbtablef.Fieldpercentiles2Click(Sender: TObject);
var
   FieldDesired : shortstring;
   pcs : floatarray1000;
   sl : tStringList;
   i  : integer;
begin
   FieldDesired := GISdb[DBonTable].PickField('Field for percentiles',NumericFieldTypes);
   ShowHourglassCursor;
   pcs := GISdb[DBonTable].GetFieldPercentiles(FieldDesired);
   sl := tStringList.Create;
   SL.add('PERCENTILE,'+FieldDesired + ',DENSITY');
   for i := 0 to 1000 do begin
      SL.Add(RealToString(0.1*i,-6,-1) + ',' + RealToString(pcs[i],-12,-8));
   end;
   StringList2CSVtoDB(sl,MDTempDir + GISdb[DBonTable].dbName + '_percentiles.csv');
   ShowStatus;
end;


procedure Tdbtablef.Schwartzberg1Click(Sender: TObject);
begin
   GISdb[DBonTable].AddGeometry(agSchwartz);
end;

initialization
   {$IfDef MessageStartUpUnit}   MessageToContinue('Startup demdbtable'); {$EndIf}
   HighlightCycle := 1;
   ClipBoard_Coords := false;
   BroadCastingFilterChanges := false;
finalization
   {$IfDef RecordClosing} WriteLineToDebugFile('Closing demdbtable in'); {$EndIf}

   {$IfDef RecordSatellite} WriteLineToDebugFile('RecordSatellite active in demdbtable'); {$EndIf}
   {$IfDef RecordKML} WriteLineToDebugFile('RecordKML active in demdbtable'); {$EndIf}
   {$IfDef RecordHTML} WriteLineToDebugFile('RecordHTML active in demdbtable'); {$EndIf}
   {$IfDef RecordStatus} WriteLineToDebugFile('RecordStatus active in demdbtable'); {$EndIf}
   {$IfDef RecordNavigation} WriteLineToDebugFile('RecordNavigation active in demdbtable'); {$EndIf}
   {$IfDef RecordDataBase} WriteLineToDebugFile('RecordDataBaseProblems active in demdbtable'); {$EndIf}
   {$IfDef RecordFieldRename} WriteLineToDebugFile('RecordFieldRename active in demdbtable'); {$EndIf}
   {$IfDef RecordDataBaseImage} WriteLineToDebugFile('RecordDataBaseImageProblems active in demdbtable'); {$EndIf}
   {$IfDef RecordClosing} WriteLineToDebugFile('RecordClosingProblems active in demdbtable');{$EndIf}
   {$IfDef RecordEditDB} WriteLineToDebugFile('RecordEditDBProblems active in demdbtable');{$EndIf}
   {$IfDef RecordGeology} WriteLineToDebugFile('RecordGeologyProblems active in demdbtable');{$EndIf}
   {$IfDef RecordDataBaseSaveFiles} WriteLineToDebugFile('RecordDataBaseSaveFiles active in demdbtable');{$EndIf}
   {$IfDef RecordShapeFileEdits} WriteLineToDebugFile('RecordShapeFileEdits active in demdbtable');{$EndIf}
   {$IfDef ExportCoords} WriteLineToDebugFile('ExportCoords active in demdbtable');{$EndIf}
   {$IfDef RecordBDfilter} WriteLineToDebugFile('RecordDDfileterProblems active in demdbtable');{$EndIf}
   {$IfDef RecordMakeLineArea} WriteLineToDebugFile('RecordMakeLineArea active in demdbtable');{$EndIf}
   {$IfDef RecordOpenGL} WriteLineToDebugFile('RecordOpenGLProblems active in demdbtable');{$EndIf}
   {$IfDef RecordClustering} WriteLineToDebugFile('RecordClustering active in demdbtable'); {$EndIf}
   {$IfDef RecordFont} WriteLineToDebugFile('RecordFontProblems active in demdbtable'); {$EndIf}
   {$IfDef RecordDataSaveStatus} WriteLineToDebugFile('RecordDataSaveStatus active in demdbtable'); {$EndIf}
   {$IfDef FindNeighbors} WriteLineToDebugFile('FindNeighbors active in demdbtable'); {$EndIf}
   {$IfDef RecordDBPlot} WriteLineToDebugFile('RecordDBPlot active in demdbtable'); {$EndIf}
   {$IfDef RecordDistance} WriteLineToDebugFile('RecordDistance active in demdbtable'); {$EndIf}
   {$IfDef RecordCSVOut} WriteLineToDebugFile('RecordCSVOut active in demdbtable'); {$EndIf}
   {$IfDef RecordFan} WriteLineToDebugFile('RecordFan active in demdbtable'); {$EndIf}
   {$IfDef RecordTerrainProfiles} WriteLineToDebugFile('RecordTerrainProfiles active in demdbtable'); {$EndIf}
   {$IfDef RecordSortTable} WriteLineToDebugFile('RecordSortTable active in demdbtable'); {$EndIf}
   {$IfDef RecordCorrelationMatrix} WriteLineToDebugFile('RecordCorrelationMatrix active in demdbtable'); {$EndIf}
   {$IfDef RecordCopyFieldLinkDB} WriteLineToDebugFile('RecordCopyFieldLinkDB active in demdbtable'); {$EndIf}
   {$IfDef RecordFillDEM} WriteLineToDebugFile('RecordFillDEMProblems active in demdbtable'); {$EndIf}
   {$IfDef RecordDBfilter} WriteLineToDebugFile('RecordDBfilterProblems active in demdbtable'); {$EndIf}
   {$IfDef CountUniqueValues} WriteLineToDebugFile('CountUniqueValues active in demdbtable'); {$EndIf}
   {$IfDef RecordTiger} WriteLineToDebugFile('RecordTigerProblems active in demdbtable'); {$EndIf}
   {$IfDef RecordKoppen} WriteLineToDebugFile('RecordKoppen active in demdbtable'); {$EndIf}
   {$IfDef RecordCurrentRecord} WriteLineToDebugFile('RecordCurrentRecord active in demdbtable'); {$EndIf}
   {$IfDef RecordQuickFilter} WriteLineToDebugFile('RecordQuickFilterProblems active in demdbtable'); {$EndIf}
   {$IfDef RecordGraph} WriteLineToDebugFile('RecordGraphProblems active in demdbtable'); {$EndIf}
   {$IfDef RecordIcons} WriteLineToDebugFile('RecordIcons active in demdbtable'); {$EndIf}
   {$IfDef RecordGeoCoding} WriteLineToDebugFile('RecordGeoCoding active in demdbtable'); {$EndIf}
   {$IfDef RecordMultigrid} WriteLineToDebugFile('RecordMultigrid active in demdbtable'); {$EndIf}
   {$IfDef RecordFormActivate} WriteLineToDebugFile('RecordFormActivate active in demdbtable'); {$EndIf}
   {$IfDef Recordzvalueplot} WriteLineToDebugFile('Recordzvalueplot active in demdbtable'); {$EndIf}
   {$IfDef RecordGeostats} WriteLineToDebugFile('RecordGeostats active in demdbtable'); {$EndIf}
   {$IfDef RecordMaskDEMShapeFile} WriteLineToDebugFile('RecordMaskDEMShapeFile active in demdbtable'); {$EndIf}

   {$IfDef RecordClosing} WriteLineToDebugFile('Closing demdbtable out'); {$EndIf}
end.





