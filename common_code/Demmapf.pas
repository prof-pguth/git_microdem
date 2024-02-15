unit Demmapf;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}


{$I nevadia_defines.inc}

{$IFDEF DEBUG}
  //{$Define NoParallelFor} //used to debug only
{$ELSE}
  //{$Define NoParallelFor}
{$ENDIF}


{$IfDef RecordProblems}  //normally only defined for debugging specific problems

//{$Define RecordMapResize}
//{$Define RecordFormResize}

   {$IFDEF DEBUG}
      //{$Define RecordFan}
      //{$Define Record3d}
      //{$Define FanDrawProblems}
      //{$Define RawProjectInverse}  //must also be set in BaseMap
      {$Define RecordDEMIX}
      //{$Define RecordDEMMapProjection}
      //{$Define RecordMatchMaps}
      //{$Define RecordSat}
      //{$Define RecordVAT}
      //{$Define TrackHorizontalDatum}
      //{$Define TrackDEMCorners}
      //{$Define RecordCarto}
      //{$Define RecordNumberOpenMaps}
      //{$Define RecordBigMap}
      //{$Define RecordLegend}
      //{$Define RecordVAT}
      //{$Define RecordCreateSelectionMap}
      //{$Define RecordHyperionReflectanceGraph}
      //{$Define RecordDEMIXtiles}
      //{$Define RecordStreamModeDigitize}
      //{$Define RecordPickRoute}
      //{$Define RecordFocalPlanes}
      //{$Define RecordRangeCircle}
      //{$Define RecordMapResize}
      //{$Define RecordCreateGeomorphMaps}
      //{$Define RecordGeomorphometry}
      //{$Define RecordGeography}
      //{$Define RecordMakeGrid}
      //{$Define RecordGDAL}
      //{$Define TrackTigerOrOS}
      //{$Define Track_f}
      //{$Define RecordSubsetZoom}
      //{$Define RecordMapIndex}
      //{$Define SatCoordsExtra}   //to track sat coords if here are bands with different resolution
      //{$Define RecordOTB}
      //{$Define RecordContour}
      //{$Define RecordCartoFull} //major slowdown for big areas
      //{$Define RecordTissot}
      //{$Define RecordOpenMap}
      //{$Define RecordGeologyMap}
      //{$Define RecordPixelSize}
      //{$Define TrackNLCD}
      //{$Define RecordMapDraw}
      //{$Define ShowProjectedLimits}
      //{$Define Slicer}
      //{$Define RecordHeatMap}
      //{$Define RecordDrainageVectors}
      //{$Define RecordSetup}
      //{$Define RecordZoomWindow}
      //{$Define RecordDoubleClick}
      //{$Define RecordTiming}
      //{$Define RecordLoadClass}
      //{$Define RecordDatumShift}
      //{$Define RecordElevationScaling}
      //{$Define RecordMultiGrids}
      //{$Define RecordMapClosing}
      //{$Define RecordGridToDBFSave}
      //{$Define RecordIndex}
      //{$Define RecordGISDB}
      //{$Define RecordAmbush}
      //{$Define RecordLAS}
      //{$Define RecordGazetteer}
      //{$Define RecordSatClass}
      //{$Define RecordCheckProperTix}
      //{$Define RecordWMS}
      //{$Define RecordNewMaps}
      //{$Define RecordNewWKT}
      //{$Define RecordNewWKTFull}
      //{$Define RecordNewSatMap}
      //{$Define RecordMessages}
      //{$Define RecordGlobeRotation}
      //{$Define RecordThreadCheckPlane}
      //{$Define RecordOpenVectorMap}
      //{$Define RecordPrinter}
      //{$Define RecordKMLexport}
      //{$Define RecordClosing}
      //{$Define RecordCollarMapMargins}
      //{$Define RecordPitsSpires}
      //{$Define RecordFresnel}
      //{$Define MapDisplayLocation}
      //{$Define RecordMapRoam}
      //{$Define RecordAllMapRoam}
      //{$Define RecordGeology}
      //{$Define RecordSatContrast}
      //{$Define RecordGetFabricAtPoint}
      //{$Define RecordFullGetFabricAtPoint}  //slowdown
      //{$Define RecordFormResize}
      //{$Define RecordDefineDatum}
      //{$Define RecordMrSID}
      //{$Define RecordUTMZone}
      //{$Define HiresintervisibilityDEM}
      //{$Define RecordAssociateDEMwithImage}
      //{$Define MeasureDistance}
      //{$Define RecordClick}
      //{$Define RecordCircleAround}
      //{$Define RecordFly}
      //{$Define ShowPickGridLimits}
      //{$Define RecordMGTProfiles}
      //{$Define RecordAmbushDetailed}
      //{$Define RecordOptimalViewshed}     //major slowdown
      //{$Define RecordIHS}
      //{$Define MakeLandCoverGrid}
      //{$Define RecordMergecolorscene}
      //{$Define RecordWeaponsFanTests}
      //{$Define RecordMasking}
      //{$Define RecordShapeFileEdits}
      //{$Define RecordEditsDEM}
      //{$Define RecordFlyRoute}
      //{$Define RecordSatCoords}
      //{$Define RecordDrift}
      //{$Define RecordFullDrift}
      //{$Define RecordFullDriftRate}
      //{$Define RecordEditDB}
      //{$Define RecordSolarPosition}
      //{$Define RecordRadiusDBEdit}
      //{$Define RecordColorMasking}
      //{$Define RecordDrainage}
      //{$Define RecordFlooding}
      //{$Define GPSBroadcast}
      //{$Define RecordSave}
      //{$Define RecordChangeDEMNowDoing}
      //{$Define RecordFullMapDraw}
      //{$Define RecordDrape}
      //{$Define RecordPLSS}
      //{$Define RecordPlateRotations}
      //{$Define RecordMapFormCreation}
      //{$Define RecordReqAnt}
      //{$Define MouseMoving}
      //{$Define RecordButton}
      //{$Define RecordButtonPressed}
      //{$Define RecordConversion}
      //{$Define RecordIntersection}
      //{$Define RecordDigitize}
      //{$Define RecordAltimeter}
      //{$Define RecordTIGER}
      //{$Define RecordOpacity}
      //{$Define RecordMouseDrag}
      //{$Define RecordOblique}
      //{$Define RecordSettingOblique}
      //{$Define RecordActivate}
      //{$Define RecordHorizon}
      //{$Define RecordGeotiff}
      //{$Define RecordCheckPoint}
      //{$Define RecordGeodeticCalculations}
      //{$Define RecordFullReqAnt}                //slowdown
   {$Else}
      //{$Define RecordGeography}
      //{$Define RecordCreateSelectionMap}
      //{$Define RecordSetup}
      //{$Define RecordMapDraw}
      //{$Define RecordCheckProperTix}
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

   SysUtils, Windows, Classes, Graphics, Controls,JPEG,DBCtrls,Math,
   System.Threading,System.SyncObjs,System.UITypes, System.Math.Vectors, System.RTLConsts,
   Forms, Dialogs, ExtCtrls, StdCtrls,{Printers,}ComCtrls,ClipBrd, Menus, Buttons, ToolWin,StrUtils,
   System.Types,

   {$IfDef RecordTime}
      System.Diagnostics,System.TimeSpan,
   {$EndIf}

   {$IfDef MSWindows}
      ShlObj, ActiveX,URLMon,Registry,Messages,ShellAPI,
   {$EndIf}

   {$IfDef ExFMX3D}
   {$Else}
      View3D_Main,
      FMX.Types3D,
   {$EndIf}

   {$IfDef IncludeFMX3DMesh}
      MainForm_3dMeshDrape,
   {$EndIf}

   {$IfDef ExStereoNet}
   {$Else}
      NetMainW,
   {$EndIf}

   DEM_indexes,
   DEMDefs, DEMMapDraw, DEMEditW,
   PetImage,Petmar_types,PETMAR,PETMath,BaseMap;


type
  TMapForm = class(TForm)
    MainMenu1: TMainMenu;
    Modify1: TMenuItem;
    Maparea1: TMenuItem;
    File1: TMenuItem;
    Print1: TMenuItem;
    Close1: TMenuItem;
    Saveimage1: TMenuItem;
    SaveDialog1: TSaveDialog;
    Contourinterval1: TMenuItem;
    Display1: TMenuItem;
    ColorDialog1: TColorDialog;
    Overlay1: TMenuItem;
    Calculate1: TMenuItem;
    Distance1: TMenuItem;
    Slope2: TMenuItem;
    Forceredraw1: TMenuItem;
    Agefromdepth1: TMenuItem;
    Reflectance1: TMenuItem;
    SlopeCategories1: TMenuItem;
    Terraincategories1: TMenuItem;
    Elevation2: TMenuItem;
    Geology1: TMenuItem;
    Bearing1: TMenuItem;
    Offset1: TMenuItem;
    StreamProfile1: TMenuItem;
    VectorOutlines1: TMenuItem;
    Erasersize1: TMenuItem;
    Edit1: TMenuItem;
    Delete1: TMenuItem;
    Replace1: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    Movewindow1: TMenuItem;
    N6: TMenuItem;
    Maskpoints1: TMenuItem;
    Highpoints1: TMenuItem;
    LowPoints1: TMenuItem;
    Specified1: TMenuItem;
    Histogram1: TMenuItem;
    Sourcecontours1: TMenuItem;
    Feet1: TMenuItem;
    Meters1: TMenuItem;
    EditWeaponsFan1: TMenuItem;
    Rangecircles1: TMenuItem;
    Info1: TMenuItem;
    SpeedButton10: TSpeedButton;
    PrintDialog1: TPrintDialog;
    Setgridparameters1: TMenuItem;
    Elevation3: TMenuItem;
    Slope3: TMenuItem;
    Reflectance3: TMenuItem;
    Contour3: TMenuItem;
    Blank2: TMenuItem;
    Pickbandcolors1: TMenuItem;
    Topographicgrain2: TMenuItem;
    N2: TMenuItem;
    Grainbyregionsize1: TMenuItem;
    ScrollBox1: TScrollBox;
    Image1: TImage;
    Band1: TMenuItem;
    Fabricatpoint1: TMenuItem;
    Grainmovie1: TMenuItem;
    DEMcolormerge1: TMenuItem;
    PopupMenu4: TPopupMenu;
    Closepolyline1: TMenuItem;
    Endpolyline1: TMenuItem;
    Mergecolors1: TMenuItem;
    Savemapwithworldfile1: TMenuItem;
    Magneticdeclination1: TMenuItem;
    Loadprojection2: TMenuItem;
    Reflectanceoptions1: TMenuItem;
    Slopecategories2: TMenuItem;
    Elevationcolors1: TMenuItem;
    Displayparameter1: TMenuItem;
    Routeobservation1: TMenuItem;
    Replayflightroute1: TMenuItem;
    Hidden1: TMenuItem;
    DrawColoredMap1: TMenuItem;
    Area1: TMenuItem;
    PopupMenu5: TPopupMenu;
    Display2: TMenuItem;
    Query1: TMenuItem;
    Restrict1: TMenuItem;
    SaveDEM1: TMenuItem;
    Displaymap1: TMenuItem;
    N9: TMenuItem;
    Closedatabase1: TMenuItem;
    Database1: TMenuItem;
    Edit2: TMenuItem;
    Markasmissing1: TMenuItem;
    Outlineregionofinterest1: TMenuItem;
    Markasmissing2: TMenuItem;
    Land1: TMenuItem;
    Water1: TMenuItem;
    N11: TMenuItem;
    Missingdatatosealevel1: TMenuItem;
    PopupMenu8: TPopupMenu;
    Newrouteobservation1: TMenuItem;
    Restorerouteobservation1: TMenuItem;
    Editfan1: TMenuItem;
    SavemapasGEOTIFF1: TMenuItem;
    Help1: TMenuItem;
    Mapdistortion1: TMenuItem;
    Cartography1: TMenuItem;
    Magneticvariation1: TMenuItem;
    Tissotindicatrix1: TMenuItem;
    N13: TMenuItem;
    N14: TMenuItem;
    Abortcurrentoperation1: TMenuItem;
    DEMelevationcolors1: TMenuItem;
    PopupMenu10: TPopupMenu;
    ZoomIn1: TMenuItem;
    NoZoom1: TMenuItem;
    ZoomOut2: TMenuItem;
    Mergecolorscene1: TMenuItem;
    Clearmap1: TMenuItem;
    Copytoclipboard1: TMenuItem;
    PopupMenu12: TPopupMenu;
    Militaryicons1: TMenuItem;
    Createshapefile2: TMenuItem;
    Annotatemap1: TMenuItem;
    Whereiskeyboard1: TMenuItem;
    SetDEMwithmap1: TMenuItem;
    Topographicgrain1: TMenuItem;
    Keyboard1: TMenuItem;
    N16: TMenuItem;
    N17: TMenuItem;
    N18: TMenuItem;
    Loadweaponsfans1: TMenuItem;
    Pointsshapefile1: TMenuItem;
    Lineshapefile1: TMenuItem;
    Areashapefile1: TMenuItem;
    Mapannotation1: TMenuItem;
    PopupMenu9: TPopupMenu;
    LabelLatLongdifference1: TMenuItem;
    LabelUTMdifference1: TMenuItem;
    NoLabels1: TMenuItem;
    Horizonblocking1: TMenuItem;
    MapMarginalia1: TMenuItem;
    Aspect1: TMenuItem;
    ASCII1: TMenuItem;
    MDDEM1: TMenuItem;
    DTED1: TMenuItem;
    Excessiveslopes1: TMenuItem;
    Setscale1: TMenuItem;
    GridPosts2: TMenuItem;
    Viewshed2: TMenuItem;
    Lineofsight1: TMenuItem;
    Database2: TMenuItem;
    ID1: TMenuItem;
    Steepestslope1: TMenuItem;
    Keylatitudes1: TMenuItem;
    Digitizecontours1: TMenuItem;
    Gazetteer1: TMenuItem;
    GeologyPopupMenu15: TPopupMenu;
    RightClickPopupMenu3: TPopupMenu;
    Earthquakes1: TMenuItem;
    Volcanoes1: TMenuItem;
    Magneticanomaly1: TMenuItem;
    Earthquakefocalmechanisms1: TMenuItem;
    Platerotations1: TMenuItem;
    Plateboundaries1: TMenuItem;
    Fracturezones1: TMenuItem;
    Variableopaquemerge1: TMenuItem;
    Overlaysvariableopaque1: TMenuItem;
    Geologysymbols1: TMenuItem;
    PanNWButton: TBitBtn;
    PanNButton: TBitBtn;
    PanWButton: TBitBtn;
    PanNEButton: TBitBtn;
    PanEButton: TBitBtn;
    PanSWButton: TBitBtn;
    PanSButton: TBitBtn;
    PanSEButton: TBitBtn;
    Satellitehistograms1: TMenuItem;
    Usecurrentsubset1: TMenuItem;
    Geodeticbearing1: TMenuItem;
    UTMgridtruenorthdeclination1: TMenuItem;
    Font1: TMenuItem;
    PLSSposition1: TMenuItem;
    NLCDclassification1: TMenuItem;
    NLCDbox1: TMenuItem;
    USGSASCII1: TMenuItem;
    Outlineregions1: TMenuItem;
    Outlineholes1: TMenuItem;
    Outlinelakes1: TMenuItem;
    Horizontalearthcurvature1: TMenuItem;
    N21: TMenuItem;
    Requiredantennaheight1: TMenuItem;
    USGSquadnames1: TMenuItem;
    Intervisibility1: TMenuItem;
    Viewshedalgorithms1: TMenuItem;
    N22: TMenuItem;
    Earthrotation1: TMenuItem;
    BlueMarble1: TMenuItem;
    Predictedseafloorages1: TMenuItem;
    OtherDEM1: TMenuItem;
    Slopes1: TMenuItem;
    N23: TMenuItem;
    Pointselectionalgorithms1: TMenuItem;
    Vectoroutlines2: TMenuItem;
    Editshapefilegroup1: TMenuItem;
    Show1: TMenuItem;
    Label2: TMenuItem;
    Pointinterpolationalgorithms1: TMenuItem;
    N26: TMenuItem;
    CurrentsubsetMDDEM1: TMenuItem;
    USproperties1: TMenuItem;
    Volume1: TMenuItem;
    MapParameterPopUpMenu1 : tPopUpMenu;
    Slope1: TMenuItem;
    MenuItem2: TMenuItem;
    Aspect2: TMenuItem;
    Profileconvexity1: TMenuItem;
    Planconvexity1: TMenuItem;
    Crosssectionalcurvature1: TMenuItem;
    MenuItem3: TMenuItem;
    erraincategory1: TMenuItem;
    Lntransformelevs1: TMenuItem;
    Log10transformelevs1: TMenuItem;
    Derivativegrid1: TMenuItem;
    Slopesin1: TMenuItem;
    Minimumcurvature1: TMenuItem;
    Maximumcurvature1: TMenuItem;
    Pointmodedigitizing1: TMenuItem;
    Streamdigitizing1: TMenuItem;
    N16bitBSQ1: TMenuItem;
    //ReinterpolateUTM1: TMenuItem;
    //ReinterpolateLatLong1: TMenuItem;
    Savemapasimage1: TMenuItem;
    RasterGIS1: TMenuItem;
    Viewshed1: TMenuItem;
    Derivativegrid2: TMenuItem;
    Histogram2: TMenuItem;
    Addgrids1: TMenuItem;
    Terraincategories2: TMenuItem;
    Requiredantennaheight2: TMenuItem;
    Filter1: TMenuItem;
    BIL1: TMenuItem;
    Gridfloat1: TMenuItem;
    Dataheader1: TMenuItem;
    Panel1: TPanel;
    AnnotateSpeedButton1: TSpeedButton;
    PrintSpeedButton: TSpeedButton;
    SaveSpeedButton: TSpeedButton;
    ClipboardSpeedButton: TSpeedButton;
    FullMapSpeedButton: TSpeedButton;
    SubsetSpeedButton: TSpeedButton;
    UnsubsetSpeedButton22: TSpeedButton;
    ZoomInSpeedButton4: TSpeedButton;
    SpeedButton22: TSpeedButton;
    ZoomOutSpeedButton5: TSpeedButton;
    RedrawSpeedButton12: TSpeedButton;
    MeasureDistanceSpeedButton14: TSpeedButton;
    SpeedButton1: TSpeedButton;
    VectorOverlaySpeedButton21: TSpeedButton;
    WeaponFanSpeedButton8: TSpeedButton;
    RangeCirclesSpeedButton9: TSpeedButton;
    GridSpeedButton15: TSpeedButton;
    DupeMapSpeedButton18: TSpeedButton;
    InfoSpeedButton6: TSpeedButton;
    PickBandSpeedButton20: TSpeedButton;
    DataBaseSpeedButton28: TSpeedButton;
    IDSpeedButton: TSpeedButton;
    FocalMechsButton: TSpeedButton;
    GeologySpeedButton1: TSpeedButton;
    EditGridButton: TSpeedButton;
    MapLibSpeedButton: TSpeedButton;
    Editpointelevations1: TMenuItem;
    DRGanaglyph1: TMenuItem;
    //UScounty1: TMenuItem;
    Openimage1: TMenuItem;
    Downhillvectors1: TMenuItem;
    Drainage1: TMenuItem;
    Terrainblowup1: TMenuItem;
    SpeedButton4: TSpeedButton;
    Directional1: TMenuItem;
    Convertcoordinates1: TMenuItem;
    ASCIIArcGrid1: TMenuItem;
    SpeedButton5: TSpeedButton;
    Load1: TMenuItem;
    Terrainorganizationmaps1: TMenuItem;
    Fansensitivity1: TMenuItem;
    N29: TMenuItem;
    Hotspots1: TMenuItem;
    Agedepthcurve1: TMenuItem;
    N30: TMenuItem;
    Structuralgeologycomputations1: TMenuItem;
    Projectfocalmechanismtosurface1: TMenuItem;
    FillholesfromreferenceDEM1: TMenuItem;
    DEMelevationmap1: TMenuItem;
    OBJ1: TMenuItem;
    PGM1: TMenuItem;
    PGM8bit1: TMenuItem;
    PGM2: TMenuItem;
    EditDEMgrid1: TMenuItem;
    KoppenSpeedButton7: TSpeedButton;
    CompareviewshedsonmultipleDEMs1: TMenuItem;
    RenameDEM1: TMenuItem;
    Elevationsextremes1: TMenuItem;
    Elevations1: TMenuItem;
    Datavoids1: TMenuItem;
    Ridges1: TMenuItem;
    Allmapsmatchthiscoveragearea1: TMenuItem;
    Matchothermaps1: TMenuItem;
    Sameelevationcolors1: TMenuItem;
    PopupMenu17: TPopupMenu;
    Shorttable1: TMenuItem;
    Longtable1: TMenuItem;
    NLCD1: TMenuItem;
    Cancelpendingselection1: TMenuItem;
    Ratiooftwogrids1: TMenuItem;
    EditViaSelectedcolor1: TMenuItem;
    Allbutselectedcolor1: TMenuItem;
    SetMaskGrid1: TMenuItem;
    MaskDEM1: TMenuItem;
    ThinDEM1: TMenuItem;
    N33: TMenuItem;
    Floodfill1: TMenuItem;
    StratcolButton: TSpeedButton;
    SavedviewshedsonmultipleDEMs1: TMenuItem;
    DEMHoles1: TMenuItem;
    DEMshadingonvectormap1: TMenuItem;
    DEMreflectance1: TMenuItem;
    Singlegridarithmetic1: TMenuItem;
    Setzrangetoaconstant1: TMenuItem;
    Expandgivenzrange1: TMenuItem;
    Medianfilter1: TMenuItem;
    RGBvalues1: TMenuItem;
    Mapshadingoptions1: TMenuItem;
    Mapshadingoptions2: TMenuItem;
    Integrateddatabasesetup1: TMenuItem;
    ReloadDEM: TMenuItem;
    Pointsnotmeetingcriteria1: TMenuItem;
    Flattenlake1: TMenuItem;
    KoppenSpeedButton: TSpeedButton;
    Duplicatemapwindow1: TMenuItem;
    Pastefromclipboard1: TMenuItem;
    Findpeaks1: TMenuItem;
    SideScanButton: TSpeedButton;
    SubbottomSpeedButton: TSpeedButton;
    Greatcircleroute1: TMenuItem;
    SRTMwaterbodies2: TMenuItem;
    BlendPanel: TPanel;
    TrackBar2: TTrackBar;
    Panel3: TPanel;
    Label4: TLabel;
    SpeedButton12: TSpeedButton;
    SpeedButton13: TSpeedButton;
    CancelBtn: TSpeedButton;
    BitBtn2: TBitBtn;
    MrSidSpeedButton: TSpeedButton;
    Legendsmarginalia1: TMenuItem;
    Geographicquadrangle1: TMenuItem;
    RGBthreeparametermap1: TMenuItem;
    Accumulatedcostsurface1: TMenuItem;
    Leastcostpath1: TMenuItem;
    //DEMSpeedButton25: TSpeedButton;
    MaskGrid1: TMenuItem;
    ReplacevalueswithreferenceDEM1: TMenuItem;
    Labelboth1: TMenuItem;
    Multiplyzvalues1: TMenuItem;
    Raiselowerzvalues1: TMenuItem;
    Interpolateacrossholessmooth1: TMenuItem;
    Selectedpercentilerange1: TMenuItem;
    O1: TMenuItem;
    OutlineregiontoreplacevalueswithreferenceDEM1: TMenuItem;
    Multiplegridarithmetic1: TMenuItem;
    NScomponentofaspect1: TMenuItem;
    Aspect3: TMenuItem;
    EWcomponent1: TMenuItem;
    Slope4: TMenuItem;
    Curvature1: TMenuItem;
    Sunrisesunsettimes1: TMenuItem;
    Redraw1: TMenuItem;
    Reregisterimage1: TMenuItem;
    Matchimage1: TMenuItem;
    AnaglyphSpeedButton: TSpeedButton;
    FennemanProvinces1: TMenuItem;
    Createbitmapmask1: TMenuItem;
    N7: TMenuItem;
    Latituderange1: TMenuItem;
    Landcovercategories1: TMenuItem;
    Missingpointsinmatchinggrid1: TMenuItem;
    Outlinecoveragearea1: TMenuItem;
    Areaofsinglecolor1: TMenuItem;
    Open1: TMenuItem;
    Loadgrouping1: TMenuItem;
    Updategrouping1: TMenuItem;
    Addfiles1: TMenuItem;
    Shapefilegrouping1: TMenuItem;
    Creategroputing1: TMenuItem;
    Overlays1: TMenuItem;
    Manageoverlays2: TMenuItem;
    SpeedButton7: TSpeedButton;
    Loadvegetationgrid1: TMenuItem;
    DSDPODPIODPdrilling1: TMenuItem;
    Loadcartographicoverlay1: TMenuItem;
    Sectoroutlines1: TMenuItem;
    Contoursfromsecondgrid1: TMenuItem;
    RoamingZvaluesfrom2dgrid1: TMenuItem;
    Meanfilter1: TMenuItem;
    Mapprojection1: TMenuItem;
    ForGoogleearth1: TMenuItem;
    CreateimagetomatchDEM1: TMenuItem;
    Supervisedclassification1: TMenuItem;
    Imageanalysis1: TMenuItem;
    Histogramallbands1: TMenuItem;
    Histogrampickband1: TMenuItem;
    Variancecovariance1: TMenuItem;
    Optimumindexfactor1: TMenuItem;
    Newband1: TMenuItem;
    NewBandPopupMenu7: TPopupMenu;
    Addbands1: TMenuItem;
    NDSI1: TMenuItem;
    NDVI1: TMenuItem;
    NDWI1: TMenuItem;
    NormalizedBurnIndex1: TMenuItem;
    Bandratio1: TMenuItem;
    Spectrallibrarygraph1: TMenuItem;
    Createtablefromimage1: TMenuItem;
    Dif: TMenuItem;
    PopupMenu11: TPopupMenu;
    Customimagesize1: TMenuItem;
    ZoomIn2: TMenuItem;
    Zoomout1: TMenuItem;
    Customzoom1: TMenuItem;
    Subsetgraphicalzoom1: TMenuItem;
    Fulldataset1: TMenuItem;
    N11view1: TMenuItem;
    Setmappixelsize1: TMenuItem;
    Key: TMenuItem;
    Modifymaparea1: TMenuItem;
    Googlemaps1: TMenuItem;
    AsGEOTIFFscreenscalegrayscale1: TMenuItem;
    Forcecompleteredraw1: TMenuItem;
    Imagepalette1: TMenuItem;
    Minimumfilter1: TMenuItem;
    Maximumfilter1: TMenuItem;
    Filter2: TMenuItem;
    Mergegridstakelowestvalue1: TMenuItem;
    DBFfile1: TMenuItem;
    Worldfileimages1: TMenuItem;
    Tools1: TMenuItem;
    GeoTIFF1: TMenuItem;
    Forceredrawbasemaplayer1: TMenuItem;
    Mergeallimages1: TMenuItem;
    Batchrequiredantennaheights1: TMenuItem;
    Peakislandarea1: TMenuItem;
    Ridges: TMenuItem;
    Maskmap1: TMenuItem;
    Editgazetteersymbology1: TMenuItem;
    Print2: TMenuItem;
    Floodingsealevelrise1: TMenuItem;
    Draw1: TMenuItem;
    Greatcirclethroughpoint1: TMenuItem;
    Smallcirclethroughpoint1: TMenuItem;
    Mask1: TMenuItem;
    Multipleparameters1: TMenuItem;
    Waverefraction1: TMenuItem;
    Everythingabovecutoff1: TMenuItem;
    Everythingbelowcutoff1: TMenuItem;
    Fresnelzones1: TMenuItem;
    Continentalcrust1: TMenuItem;
    Geomorphatlas1: TMenuItem;
    Outsideselectedrange1: TMenuItem;
    Outsideselectedpercentilerange1: TMenuItem;
    UndoSpeedButton: TSpeedButton;
    LegendPopupMenu14: TPopupMenu;
    TIGERVectorlegend1: TMenuItem;
    NLCDLegend1: TMenuItem;
    Koppenlegend2: TMenuItem;
    Descriptions1: TMenuItem;
    Abbreviations1: TMenuItem;
    Databaselegend1: TMenuItem;
    Shapefilegrouplegend1: TMenuItem;
    Gazetteerlegend1: TMenuItem;
    Legend1: TMenuItem;
    Maplibrary1: TMenuItem;
    Maplibrarycoverage1: TMenuItem;
    Copycoordinates1: TMenuItem;
    Saverangecircles1: TMenuItem;
    Geostationarysatellitevisibility1: TMenuItem;
    Parametricisotropicsmoothing1: TMenuItem;
    Imagenegative1: TMenuItem;
    LatlongofPLSSposition1: TMenuItem;
    PLSS1: TMenuItem;
    Geomorphatlas2: TMenuItem;
    Database3: TMenuItem;
    Colorsfromgridcategories1: TMenuItem;
    Normalizeparameter1: TMenuItem;
    ModifyTIGERdisplay1: TMenuItem;
    Standard1: TMenuItem;
    States1: TMenuItem;
    Counties1: TMenuItem;
    Rivers1: TMenuItem;
    Highways1: TMenuItem;
    Map2: TMenuItem;
    N25: TMenuItem;
    XYZpointsshapefile1: TMenuItem;
    Copyimagecoordinatestoclipboard1: TMenuItem;
    Flat: TMenuItem;
    Highend1: TMenuItem;
    Lowend1: TMenuItem;
    Singlevalue1: TMenuItem;
    N27: TMenuItem;
    Removemissingpointsinmask1: TMenuItem;
    AquastatClimate1: TMenuItem;
    Truenorthlines1: TMenuItem;
    Drawmultiplefans1: TMenuItem;
    LASFile1: TMenuItem;
    Pointcloud1: TMenuItem;
    Parameterpercentiles1: TMenuItem;
    Resoremenus1: TMenuItem;
    Suminbox1: TMenuItem;
    Forceredrawlegendsscalebars1: TMenuItem;
    Clearrangecircles1: TMenuItem;
    Oceanography1: TMenuItem;
    suanamitravel1: TMenuItem;
    Driftmodels1: TMenuItem;
    Elevationmoments1: TMenuItem;
    Slopemoments1: TMenuItem;
    Momentsforregion1: TMenuItem;
    Plancurvature1: TMenuItem;
    Profilecurvature1: TMenuItem;
    N5: TMenuItem;
    Allreliefmeasures1: TMenuItem;
    ransformelevations1: TMenuItem;
    Deletesavedbasemaps1: TMenuItem;
    Gridaveragestddev1: TMenuItem;
    Rugosity1: TMenuItem;
    GridVATlegend1: TMenuItem;
    VATdisplay1: TMenuItem;
    Convergenceindex1: TMenuItem;
    PointCloudSpeedButton: TSpeedButton;
    VARI1: TMenuItem;
    RGBgrayscale1: TMenuItem;
    Erodeimage1: TMenuItem;
    Unsupervisedclassification1: TMenuItem;
    Contraststretch1: TMenuItem;
    AllbandsasGeotiffs1: TMenuItem;
    DEMGridarea1: TMenuItem;
    WMSMaps: TMenuItem;
    Polarorbitersatellitevisibility1: TMenuItem;
    Loadsidescanimagery1: TMenuItem;
    Export1: TMenuItem;
    MaptoGoogleEarth1: TMenuItem;
    GridgraticluetoGoogleEarth1: TMenuItem;
    Globaltopography1: TMenuItem;
    Notrecommended1: TMenuItem;
    hismap1: TMenuItem;
    Allmaps1: TMenuItem;
    Pointdensity1: TMenuItem;
    PickcornersMDDEM1: TMenuItem;
    LibSpeedButton: TSpeedButton;
    Climograph1: TMenuItem;
    Pointsalongsurveylines1: TMenuItem;
    Excessiveslopes2: TMenuItem;
    Copymaptoclipboard1: TMenuItem;
    Cartogrouplegend1: TMenuItem;
    SetsecondDEMonmap1: TMenuItem;
    Saveproject1: TMenuItem;
    N28: TMenuItem;
    ReinterpolateDEM1: TMenuItem;
    CreateDEMtomatchmap1: TMenuItem;
    UTM1: TMenuItem;
    LatLong1: TMenuItem;
    N31: TMenuItem;
    Exportcomputations1: TMenuItem;
    erraincategories2: TMenuItem;
    Streams1: TMenuItem;
    Allterraincategories1: TMenuItem;
    PortionofDEMwithdata1: TMenuItem;
    Bycolumnrow1: TMenuItem;
    ColumnsEastLimit1: TMenuItem;
    Rowssouthoflimit1: TMenuItem;
    ColumnsWestLimit1: TMenuItem;
    Rowsnorthoflimit1: TMenuItem;
    Matchinggrid1: TMenuItem;
    Validpointsinmatchinggrid1: TMenuItem;
    Intervisibilitymap1: TMenuItem;
    DSMfirstreturn1: TMenuItem;
    HAGvegheight1: TMenuItem;
    DTMlastreturn1: TMenuItem;
    Geocodeaddress1: TMenuItem;
    Geocode1: TMenuItem;
    Addresstolatlong1: TMenuItem;
    ViewExifimages1: TMenuItem;
    Multiplevalues1: TMenuItem;
    Abortcurrentoperation2: TMenuItem;
    PLSSfromkeyboardlocation1: TMenuItem;
    QuickbasemaptoGoogleEarth1: TMenuItem;
    Editfans1: TMenuItem;
    Rmove1: TMenuItem;
    Snglepointspires1: TMenuItem;
    Singlepointpits1: TMenuItem;
    Clearoverlays1: TMenuItem;
    DNsatpoint1: TMenuItem;
    Bytedata0tomissing1: TMenuItem;
    Subtractopengrid1: TMenuItem;
    Loadvegetationlayers1: TMenuItem;
    Vegetationlayers1: TMenuItem;
    Vegetationdensitygraph1: TMenuItem;
    Vegetationdensitymovie1: TMenuItem;
    Searchpowerlines1: TMenuItem;
    Buildingedges1: TMenuItem;
    Reclassify1: TMenuItem;
    Rangetosinglevalue1: TMenuItem;
    Missingdatatospecifiedvalue1: TMenuItem;
    CopyVegprofiletoclipboard1: TMenuItem;
    Vegetationdensitylayers1: TMenuItem;
    Vegetationlayeroverlay1: TMenuItem;
    Walls1: TMenuItem;
    N32: TMenuItem;
    Allcurvatures1: TMenuItem;
    QuarterDEM1: TMenuItem;
    //FavoriteDEM11: TMenuItem;
    //FavoriteDEM21: TMenuItem;
    UTMspecifyzone1: TMenuItem;
    Loadsecondvegeationlayers1: TMenuItem;
    Minimumoftwogrids1: TMenuItem;
    Statistics1: TMenuItem;
   // OpenGLpointcloud1: TMenuItem;
    LASexport1: TMenuItem;
    LoadLOStopoprofile1: TMenuItem;
    Loadvegetationgrid21: TMenuItem;
    DeliberatemaptoGoogleEarth1: TMenuItem;
    Coccurrenceoftwogrids1: TMenuItem;
    Percentages1: TMenuItem;
    Counts1: TMenuItem;
    Everythingexceptsinglevalue1: TMenuItem;
    Changemap1: TMenuItem;
    Setzunits1: TMenuItem;
    Undefined1: TMenuItem;
    Integercode1: TMenuItem;
    ClimatePopupMenu18: TPopupMenu;
    KoppenClimateStations1: TMenuItem;
    Globalmonthlywinds1: TMenuItem;
    Recentworldpiracy1: TMenuItem;
    Latitudinalscalebars1: TMenuItem;
    Koppengrid1: TMenuItem;
    GADM1: TMenuItem;
    Level0countries1: TMenuItem;
    Level1stateprovince1: TMenuItem;
    CLIWOCshiplogs1: TMenuItem;
    Weather1: TMenuItem;
    Shipmotion1: TMenuItem;
    ForcereloadWMSmap1: TMenuItem;
    ClearWMSmap1: TMenuItem;
    PopupMenu19: TPopupMenu;
    Quickmap1: TMenuItem;
    Detailedmap1: TMenuItem;
    TIGER1: TMenuItem;
    GE_SpeedButton: TSpeedButton;
    OGL_speedbutton: TSpeedButton;
    Focalplaneanalysis1: TMenuItem;
    CandeandKenttimescale1: TMenuItem;
    Multigrids1: TMenuItem;
    MaxDN1: TMenuItem;
    AverageDN1: TMenuItem;
    Locatehighvalues1: TMenuItem;
    Opengrid1: TMenuItem;
    ailpercentiles1: TMenuItem;
    Weather2: TMenuItem;
    Normalizie1: TMenuItem;
    StdDevinbox1: TMenuItem;
    ShiftDEM1: TMenuItem;
    Leftedge1: TMenuItem;
    Featuremigration1: TMenuItem;
    Missingdatacolor1: TMenuItem;
    Filledneighborhood1: TMenuItem;
    CreateMinDNgrid1: TMenuItem;
    Averagereflectanceinregion1: TMenuItem;
    SpeedButton2: TSpeedButton;
    Matchingopengrid1: TMenuItem;
    Missingpoints1: TMenuItem;
    Validpoints1: TMenuItem;
    Replacefromsecondgrid1: TMenuItem;
    Pickmode1: TMenuItem;
    Bytes1: TMenuItem;
    Integer161: TMenuItem;
    Word1: TMenuItem;
    Floatingpoint1: TMenuItem;
    Pointsfromdatabase1: TMenuItem;
    Roads1: TMenuItem;
    Allvalidtosinglevalue1: TMenuItem;
    Logbase10transform1: TMenuItem;
    Lntransform1: TMenuItem;
    Immediateneighbordropoff1: TMenuItem;
    Rastertovector1: TMenuItem;
    Mask2: TMenuItem;
    Verifyelevationrange1: TMenuItem;
   // GDALresamplethin1: TMenuItem;
    Ransacplane1: TMenuItem;
    Erodedilate1: TMenuItem;
    LinedetectionHoughtransform1: TMenuItem;
    Inf1: TMenuItem;
    NAN1: TMenuItem;
    LinedetectionHoughtransform2: TMenuItem;
    Numberimmediateneighbors1: TMenuItem;
    //Neighborhoodvalues1: TMenuItem;
    Singlevalue2: TMenuItem;
    IDFeatures1: TMenuItem;
    Gridmigration1: TMenuItem;
    Featuremigration2: TMenuItem;
    Monthlywinds1: TMenuItem;
    DEMsatpoint1: TMenuItem;
    Gridmaskcolor1: TMenuItem;
    Featuregeomorphometry1: TMenuItem;
    Loadfeaturegrid1: TMenuItem;
    PNG1: TMenuItem;
    JSON1: TMenuItem;
    idegaugesandsealevelrise1: TMenuItem;
    Globalmonthlytemperatures1: TMenuItem;
    Globalmonthlyrain1: TMenuItem;
    Western1: TMenuItem;
    Southwestern1: TMenuItem;
    Eastwest1: TMenuItem;
    SWNE1: TMenuItem;
    Migrationovertime1: TMenuItem;
    Validpointsinsecondgrid1: TMenuItem;
    Replaceonlyvalidvaluesfromsecondgrid1: TMenuItem;
    ASCII2: TMenuItem;
    Radianstodegrees1: TMenuItem;
    Tangentradians1: TMenuItem;
    Tangentdegrees1: TMenuItem;
    //auDEMtools1: TMenuItem;
    Locationsonly1: TMenuItem;
    Values1: TMenuItem;
    Profiles1: TMenuItem;
    oInternationalDateLine1: TMenuItem;
    oPrimeMeridian1: TMenuItem;
    Referencegridinfile1: TMenuItem;
    Opengrid2: TMenuItem;
    Gridinfile1: TMenuItem;
    N34: TMenuItem;
    SpeedButton3: TSpeedButton;
    //renchslabdips1: TMenuItem;
    renches1: TMenuItem;
    //Slabdepths1: TMenuItem;
    Maxmeanmingridsfrompointdata1: TMenuItem;
    SpeedButton8: TSpeedButton;
    NLCD19921: TMenuItem;
    NLCD20011: TMenuItem;
    LandsatQA1: TMenuItem;
    Cloudmask1: TMenuItem;
    N8: TMenuItem;
    Stats1: TMenuItem;
    Cirrusmask1: TMenuItem;
    Watermask1: TMenuItem;
    Snowicemaksk1: TMenuItem;
    Eastern1: TMenuItem;
    LVIS1: TMenuItem;
    ableofcontents1: TMenuItem;
    ableofcontents2: TMenuItem;
    Classification1: TMenuItem;
    Loadsummarymultipleclassifications1: TMenuItem;
    Lookatpoints1: TMenuItem;
    Ensembleclassification1: TMenuItem;
    Isolatedvalues1: TMenuItem;
    Removeifmissingneighbors1: TMenuItem;
    Removetoofewsimilarneighbors1: TMenuItem;
    Sunabovethehorizon2: TMenuItem;
    Magneticnorthlines1: TMenuItem;
    Northarrow1: TMenuItem;
    Contoursfromsecondgrid2: TMenuItem;
    Singlecontour1: TMenuItem;
    Dividezvalues1: TMenuItem;
    Flowdirections1: TMenuItem;
    Direction01361: TMenuItem;
    ArcGIScodes11281: TMenuItem;
    auDEMcode181: TMenuItem;
    Timemaps1: TMenuItem;
    Randomsamplingpoints1: TMenuItem;
    PointcloudtoGoogleEarth1: TMenuItem;
    PointcloudtoOpenGL1: TMenuItem;
    CreatefilterforUTMsquare1: TMenuItem;
    Coastlines1: TMenuItem;
    ospecifiedlongitude1: TMenuItem;
    Geologicages1: TMenuItem;
    Coloredbynumber1: TMenuItem;
    Epochs1: TMenuItem;
    Periods1: TMenuItem;
    OPEXPoseidonimport1: TMenuItem;
    Import1: TMenuItem;
    Rivers2: TMenuItem;
    Slopelogtangent1: TMenuItem;
    Slopesqrtsin1: TMenuItem;
    Slopplntangent1: TMenuItem;
    Arctangent1: TMenuItem;
    Magneticanomalypicks1: TMenuItem;
    NDSIsnow1: TMenuItem;
    Normalizeddifferencepickbands1: TMenuItem;
    Quickstats1: TMenuItem;
    Histogram3: TMenuItem;
    Outlineothermaps1: TMenuItem;
    Relief2: TMenuItem;
    Reliefavgelevstdelev1: TMenuItem;
    Both1: TMenuItem;
    NDVI2: TMenuItem;
    Percentilestimeseries1: TMenuItem;
    PercentilsofNDVItimeseries1: TMenuItem;
    Pointtimeseries1: TMenuItem;
    NDVIofperenctilesmonthly1: TMenuItem;
    PercentilesofNDVImonthly1: TMenuItem;
    Keypercentiles1: TMenuItem;
    Quickclassfication1: TMenuItem;
    Surveytracklines1: TMenuItem;
    Createsurveylines1: TMenuItem;
    //Algorithms1: TMenuItem;
    Allslopeaspect1: TMenuItem;
    N36: TMenuItem;
    Popuplegends1: TMenuItem;
    DrapecurrentmaptoOpenGL1: TMenuItem;
    N37: TMenuItem;
    Savesatelliteimage1: TMenuItem;
    Validpointssinglecategory1: TMenuItem;
    WMSOpactiy1: TMenuItem;
    Geomorphometry1: TMenuItem;
    Reliefbyregionsize1: TMenuItem;
    Grainbyregionsize2: TMenuItem;
    Openness1: TMenuItem;
    Opennessoptions1: TMenuItem;
    LoadOSMoverlay1: TMenuItem;
    Featuregeomorphometry2: TMenuItem;
    Creategrid1: TMenuItem;
    Createdatabase2: TMenuItem;
    Loaddatabase1: TMenuItem;
    BitmapandXYZBfile1: TMenuItem;
    opographicruggednessindex1: TMenuItem;
    Quadtickpoints1: TMenuItem;
    Verticalswipecompare1: TMenuItem;
    Twomaps1: TMenuItem;
    CreateDBs1: TMenuItem;
    TestMD1: TMenuItem;
    Clearsecondgrid1: TMenuItem;
    Blankmapcolor1: TMenuItem;
    Pointsbelow1: TMenuItem;
    Pointsabove1: TMenuItem;
    ID2: TMenuItem;
    Contourinterval2: TMenuItem;
    Seismicviewing1: TMenuItem;
    N39: TMenuItem;
    Openmultigridsforrasteranalysis1: TMenuItem;
    All2: TMenuItem;
    VISandNIRsurfacebands1: TMenuItem;
    N2bandscattergram1: TMenuItem;
    Pickfilter1: TMenuItem;
    Basicstatistics1: TMenuItem;
    ZigDistButton: TSpeedButton;
    Endlengthmeasurement1: TMenuItem;
    Trackpointfile1: TMenuItem;
    Endtrack1: TMenuItem;
    ConvertUKOSDEMtoUTM1: TMenuItem;
    ConvertUKOSDEM1: TMenuItem;
    oGeographic1: TMenuItem;
    Offcurrentmap1: TMenuItem;
    PLSSlocation1: TMenuItem;
    CompareDNconversions1: TMenuItem;
    //Nightlights20161: TMenuItem;
    IwashishiandPikeclassification1: TMenuItem;
    GDALtranslatesubset1: TMenuItem;
    Orfeotoolbox1: TMenuItem;
    Concatenateimages1: TMenuItem;
    Kmeansclustering1: TMenuItem;
    Segmentation1: TMenuItem;
    LCCstandardparallels1: TMenuItem;
    Mapprojectionzones1: TMenuItem;
    //USSPCS1: TMenuItem;
    //MGRSUSNG6x8zones1: TMenuItem;
    //UTM100Kzones1: TMenuItem;
    Maximizeforscreen1: TMenuItem;
    N1: TMenuItem;
    Streammodetolerance1: TMenuItem;
    MergemultipleCSVTXTfiles1: TMenuItem;
    GDALwarpsubset1: TMenuItem;
    N12: TMenuItem;
    Bandcorrelation1: TMenuItem;
    //Falsecolorpansharpen1: TMenuItem;
    RVTgridcreation1: TMenuItem;
    CurrentsubsetGeotiff1: TMenuItem;
    Whiteboxfillholes1: TMenuItem;
    Gaussianpyramiddownsample1: TMenuItem;
    All11scale1: TMenuItem;
    //OpenGLdrapeonanotherDEM1: TMenuItem;
    Reinterpolatealltosameresolution1: TMenuItem;
    Allsamepixelsizeasthismap1: TMenuItem;
    Allsamedisplay1: TMenuItem;
    Physicalgeography1: TMenuItem;
    Sunpositionequinoxsolstice1: TMenuItem;
    Annualsunrisesunset1: TMenuItem;
    Monthlygraphatpoint1: TMenuItem;
    Multiplyallgridsbyconstantsandresave1: TMenuItem;
    Evapotranspirationprecipitationwaterbudget1: TMenuItem;
    AddRGBwindows1: TMenuItem;
    Saveallmapsasimage1: TMenuItem;
    Quickrotatemap1: TMenuItem;
    Set0tomissingandresave1: TMenuItem;
    Koppenclimograph1: TMenuItem;
    Hurricanes1: TMenuItem;
    UStornadoes1: TMenuItem;
    Slopemm1: TMenuItem;
    Annualsunrisesunset2: TMenuItem;
    CombinedMGRSpolygons1: TMenuItem;
    Equinoxsolstice1: TMenuItem;
    oday1: TMenuItem;
    All3: TMenuItem;
    Pointcoordinates2: TMenuItem;
    Physicalgeographylabs1: TMenuItem;
    Fromllokuptable1: TMenuItem;
    Monthlytemperatureranges1: TMenuItem;
    N35: TMenuItem;
    Allmultigridbasicstatistics1: TMenuItem;
    Koppenclimographfromclimategrids1: TMenuItem;
    Verticaldatumshift1: TMenuItem;
    WGS84elllipsoidtoEGM200081: TMenuItem;
    EGM2008toWGS84ellipsoid1: TMenuItem;
    GDALinfo1: TMenuItem;
    MICRODEMGeotiffinfo1: TMenuItem;
    Monthlyclimatologies1: TMenuItem;
    CCAP1: TMenuItem;
    ComboBox1: TComboBox;
    ZoomBitBtn: TBitBtn;
    Geology3: TMenuItem;
    Pointslopebyregionsize1: TMenuItem;
    Sedimentthicknessversuscrustalage1: TMenuItem;
    Openleastcostpathgrids1: TMenuItem;
    issot1: TMenuItem;
    Setspecifiedvaluetomissingandresave1: TMenuItem;
    Keylatitudes2: TMenuItem;
    LAScurrentmaparea1: TMenuItem;
    Averagebylatitude1: TMenuItem;
    Pointcloudtodatabase1: TMenuItem;
    Oceancurrents1: TMenuItem;
    N40: TMenuItem;
    Plateoutlines1: TMenuItem;
    DEMgridhistogram1: TMenuItem;
    errainsunblocking1: TMenuItem;
    N0codes1: TMenuItem;
    Vectoraverage1: TMenuItem;
    Aspectrosediagram1: TMenuItem;
    ModeFilterPopupMenu: TPopupMenu;
    Filterjustforvoids1: TMenuItem;
    N3x3neighborhood1: TMenuItem;
    N5x5neighborhood1: TMenuItem;
    Filterentiregrid1: TMenuItem;
    N3x3neighborhood2: TMenuItem;
    N5x5neighborhhhod1: TMenuItem;
    Fillwithmodefilter1: TMenuItem;
    Modefilter1: TMenuItem;
    LASclassificationlegend1: TMenuItem;
    LAScategoriestoshow1: TMenuItem;
    RGBgridfillholes1: TMenuItem;
    Roundtobylerange1: TMenuItem;
    Loadvegetation1: TMenuItem;
    LoadCHMgrid1: TMenuItem;
    Loadchangegrid1: TMenuItem;
    Gridgraticule1: TMenuItem;
    Gridgraticule2: TMenuItem;
    Heatmap1: TMenuItem;
    Shapefileaftersubsettomatchmapextent1: TMenuItem;
    Differencemapsallotheropengrids1: TMenuItem;
    Rasteraftersubsettomatchthismapextent1: TMenuItem;
    Globaltectonicsmap1: TMenuItem;
    Rangecircles2: TMenuItem;
    Restorerangecircles1: TMenuItem;
    Annualsolarelevation1: TMenuItem;
    NDBIbuiltup1: TMenuItem;
    //Definedcategories1: TMenuItem;
    Fixedpalettestats1: TMenuItem;
    N19: TMenuItem;
    N24: TMenuItem;
    Allgraphs1: TMenuItem;
    N41: TMenuItem;
    Multiplegraphs1: TMenuItem;
    ClimographandETO1: TMenuItem;
    Solarparameters1: TMenuItem;
    Imagery1: TMenuItem;
    Landsatmetadata1: TMenuItem;
    Sentinel2metadata1: TMenuItem;
    OArelectancewithsunpositioncorrection1: TMenuItem;
    RemoteSensingPopupOptions: TMenuItem;
    OAreflectance1: TMenuItem;
    Surfaceradiance1: TMenuItem;
    Surfaceradiance2: TMenuItem;
    LandsatTIR1: TMenuItem;
    Brightnesstemperature1: TMenuItem;
    OAradiance1: TMenuItem;
    SatelliteDNsatpoint2: TMenuItem;
    Sunposition1: TMenuItem;
    N3DviewwithtwoDEMs1: TMenuItem;
    LSTfromemissivity1: TMenuItem;
    Fixedpalettecategories1: TMenuItem;
    Createmask1: TMenuItem;
    Sentinel2bandreflectance1: TMenuItem;
    Experimentaloptions1: TMenuItem;
    Landsatmetadata2: TMenuItem;
    Sentinel2metadata2: TMenuItem;
    Differenceelevationslopeaspectmaps1: TMenuItem;
    Checkprojectionresults1: TMenuItem;
    Whiteboxslopemape1: TMenuItem;
    Externaltools1: TMenuItem;
    Pointslopealgorithms1: TMenuItem;
    N42: TMenuItem;
    Extractpointcloudreturns1: TMenuItem;
    Slopedegreestopercent1: TMenuItem;
    GDALslopemap1: TMenuItem;
    GDALslopemapHorn1: TMenuItem;
    Whiteboxaspectmap1: TMenuItem;
    GDALaspectmapZevenbergenThorne1: TMenuItem;
    GDALaspectmapHorn1: TMenuItem;
    Whiteboxfillholes2: TMenuItem;
    Gridoutlines1: TMenuItem;
    N1pixel1: TMenuItem;
    N1pixel2: TMenuItem;
    Directionalslopes1: TMenuItem;
    N43: TMenuItem;
    GDALfillholes1: TMenuItem;
    Fillholeswithexternalprogram1: TMenuItem;
    Allpoints1: TMenuItem;
    Isplandperimeter1: TMenuItem;
    N44: TMenuItem;
    ForceAllRedraw1: TMenuItem;
    Pointinterpolationalgorithms2: TMenuItem;
    GDALupsample1: TMenuItem;
    CreateMedianDNgrid1: TMenuItem;
    NASADEMtomatchthismap1: TMenuItem;
    DEMsfrommaplibrary1: TMenuItem;
    ASTERGDEMtomatchthismap1: TMenuItem;
    CenterpointsofallDBs1: TMenuItem;
    Bigimagewithallmaps1: TMenuItem;
    Monthlyinsolation1: TMenuItem;
    Gridspacinganddeclination1: TMenuItem;
    MatchWKTprojection1: TMenuItem;
    Slopeerrorestimateexperimental1: TMenuItem;
    Allsamemapsize1: TMenuItem;
    Exportvaluesforallopengrids1: TMenuItem;
    GDALbilinearbicubictoUTM1: TMenuItem;
    GDALhillshadeHorn1: TMenuItem;
    Slopeandcomponents1: TMenuItem;
    Scribbleonmap1: TMenuItem;
    Absolutevalue1: TMenuItem;
    RMSE1: TMenuItem;
    DEMIX10ktiles1: TMenuItem;
    Loadlandcover1: TMenuItem;
    Averagetopographicprofile1: TMenuItem;
    DEMsfrommaplibrarymaparea1: TMenuItem;
    Maskallothergridstomatchthisone1: TMenuItem;
    N10: TMenuItem;
    ResampleDEMgridbyaveraging1: TMenuItem;
    Downsampling1: TMenuItem;
    hinaveragingcomparison1: TMenuItem;
    N45: TMenuItem;
    N46: TMenuItem;
    Mapdirectsolarillumination1: TMenuItem;
    Sunandsatellitevisibilityandblocking1: TMenuItem;
    Geoid1: TMenuItem;
    Exportmaplibrary1: TMenuItem;
    Pixelsize1: TMenuItem;
    Cartographu1: TMenuItem;
    EGM1996toEGM20081: TMenuItem;
    Horizontalshift1: TMenuItem;
    HorizontalDEMshifts1: TMenuItem;
    HorizontalDEMshifts2: TMenuItem;
    Allopengrids1: TMenuItem;
    Createnewgridforthismaparea1: TMenuItem;
    Flickermovie1: TMenuItem;
    racks1: TMenuItem;
    GNDVIvegetation1: TMenuItem;
    WhiteBoxmultiscaleroughness1: TMenuItem;
    N48: TMenuItem;
    Roughnessstddevofslope1: TMenuItem;
    DerivativeGridPopupMenu16: TPopupMenu;
    RoughnessfromSSO1: TMenuItem;
    GDALTRIWilsonbathymetric1: TMenuItem;
    GDALTRIRileyterrestrial1: TMenuItem;
    GDALTPI1: TMenuItem;
    GDALroughness1: TMenuItem;
    N49: TMenuItem;
    Moonriseset1: TMenuItem;
    N50: TMenuItem;
    GRASSslopeHorn1: TMenuItem;
    GRASSvectorruggedness1: TMenuItem;
    Missingdata1: TMenuItem;
    Pointparameters1: TMenuItem;
    Pointzvaluesallgrids1: TMenuItem;
    Curvaturecategories1: TMenuItem;
    Geomorphometryanalysis1: TMenuItem;
    Grassprofilecurvature1: TMenuItem;
    Grasstangentialcurvature1: TMenuItem;
    Differencebetweentwogrids2: TMenuItem;
    Scatterplotoftwogrids2: TMenuItem;
    Gridcorrelations2: TMenuItem;
    Gammagrids1: TMenuItem;
    ACOLITEprocessing1: TMenuItem;
    GRASSTRI1: TMenuItem;
    Externaltools2: TMenuItem;
    Metadata1: TMenuItem;
    Landsurfacetemperature1: TMenuItem;
    GRASSaspect1: TMenuItem;
    N15: TMenuItem;
    Slopeerrorestimatorexperimental1: TMenuItem;
    Aspectslopemerge1: TMenuItem;
    Aspectoptions1: TMenuItem;
    Outlinemaparea1: TMenuItem;
    DEMIX10Ktile1: TMenuItem;
    Averagetonewprojection1: TMenuItem;
    LAZ1: TMenuItem;
    PixelSizeEdit: TEdit;
    WhiteboxPennockClassification1: TMenuItem;
    Fatfingers1: TMenuItem;
    Recenter2: TMenuItem;
    Generaloptions1: TMenuItem;
    N38: TMenuItem;
    Reinterpolatepickprojection1: TMenuItem;
    Saveimage2: TMenuItem;
    AddanopenDEM1: TMenuItem;
    SubtractanopenDEM1: TMenuItem;
    Moviewithallmaps1: TMenuItem;
    DEMIX1secresamplebyaveraging1: TMenuItem;
    Samehorizontaldatum1: TMenuItem;
    Samehorizontaldatum2: TMenuItem;
    OpenGLwithallmaps1: TMenuItem;
    MatchMapsPopupMenu: TPopupMenu;
    Matchothermaps2: TMenuItem;
    Matchothermaps3: TMenuItem;
    WhiteboxGeomorphons1: TMenuItem;
    Whiteboxcurvature1: TMenuItem;
    Profile1: TMenuItem;
    angential1: TMenuItem;
    Minimal1: TMenuItem;
    Maximal1: TMenuItem;
    Mean1: TMenuItem;
    Gaussian1: TMenuItem;
    Maskothergridstomatchthisone1: TMenuItem;
    GRASSTPI1: TMenuItem;
    StdDevinregion2: TMenuItem;
    GetGRASSextensions1: TMenuItem;
    Roundtobyterangepercentiles1: TMenuItem;
    Unitconversions1: TMenuItem;
    Rangeshifts1: TMenuItem;
    Mathoperatioins1: TMenuItem;
    Mathfunctions2: TMenuItem;
    N3x3region1: TMenuItem;
    N5x5region1: TMenuItem;
    StdDevinregion1: TMenuItem;
    DetrendDEMgrid1: TMenuItem;
    WhiteboxTRI1: TMenuItem;
    N47: TMenuItem;
    N51: TMenuItem;
    SAGATPImap1: TMenuItem;
    SaveasPixelispoint1: TMenuItem;
    SaveasPixelisarea1: TMenuItem;
    N7x7region1: TMenuItem;
    N7x7region2: TMenuItem;
    GDALcontourshapefile1: TMenuItem;
    DEMIXevaluatehalfsecondgrids1: TMenuItem;
    MICRODEMupsamplebilinearbicubic1: TMenuItem;
    Moonposition1: TMenuItem;
    Experimental1: TMenuItem;
    BestglobalDEM1: TMenuItem;
    DEMIX1secresamplewithGDAL1: TMenuItem;
    Pixelextentandhighresaverage1: TMenuItem;
    Coordinates1: TMenuItem;
    oday2: TMenuItem;
    oday3: TMenuItem;
    N3Drotatingglobe1: TMenuItem;
    SAGAVRMmapvectorruggedness1: TMenuItem;
    Whitebox1: TMenuItem;
    WhiteboxCircularVarianceOfAspect1: TMenuItem;
    INForNAN1: TMenuItem;
    CreategeoatlasKMZ1: TMenuItem;
    Filtersizes1: TMenuItem;
    Mapboundingbox1: TMenuItem;
    N20: TMenuItem;
    Airballdirtball1: TMenuItem;
    ClipDEMtoregionwithdata1: TMenuItem;
    Elevationpercentiles1: TMenuItem;
    Differencemap1: TMenuItem;
    Dataheader2: TMenuItem;
    NumericgridwithVATtobytegridwithcodes1: TMenuItem;
    Geomorphometrybycategories1: TMenuItem;
    Area2: TMenuItem;
    Volume2: TMenuItem;
    Allvalidpixels1: TMenuItem;
    Openbandforrasteranalysis1: TMenuItem;
    Changemap2: TMenuItem;
    PickseriesandloadDEMsfromlibrary1: TMenuItem;
    N52: TMenuItem;
    ComapreUTMvsgeographic1: TMenuItem;
    NDVI3: TMenuItem;
    NBR1: TMenuItem;
    Elevationdifference1: TMenuItem;
    Genericdifference1: TMenuItem;
    Entiregrid1: TMenuItem;
    CurrentMapArea2: TMenuItem;
    N53: TMenuItem;
    AspectDifference1: TMenuItem;
    COPALOSbestlocations1: TMenuItem;
    COPALOScategories1: TMenuItem;
    COPALOS9categories1: TMenuItem;
    Normalizeeastwest1: TMenuItem;
    Normalizenorthsouth1: TMenuItem;
    Nonormalization1: TMenuItem;
    Alolthreenormalizations1: TMenuItem;
    InsertpostingsfromreferenceDEM1: TMenuItem;
    Normalizeto30m1: TMenuItem;
    RIK1: TMenuItem;
    RICK1: TMenuItem;
    Winwcontestmaps1: TMenuItem;
    Likelymissingdatacodes1: TMenuItem;
    Experimental2: TMenuItem;
    Allmissingtosinglevaluevalidsettomissing1: TMenuItem;
    Interactiveadjusment1: TMenuItem;
    N54: TMenuItem;
    N55: TMenuItem;
    COPandALOS1: TMenuItem;
    All61: TMenuItem;
    Geoidgrids1: TMenuItem;
    SummaryallopenDEMsGrids1: TMenuItem;
    N56: TMenuItem;
    Slopeandroughness1: TMenuItem;
    MergeanotherDEMhillshade1: TMenuItem;
    Zoomfullresolution1: TMenuItem;
    EditVATDBFcolorscategorynames1: TMenuItem;
    N57: TMenuItem;
    Putshadingfromthismapunderselectedmaps1: TMenuItem;
    PI1: TMenuItem;
    LocaddatumtoEGM20081: TMenuItem;
    Assignverticaldatum1: TMenuItem;
    NAVD881: TMenuItem;
    EGM2008: TMenuItem;
    Other1: TMenuItem;
    DEMIX1: TMenuItem;
    Datumshiftcomparison1: TMenuItem;
    Shiftfilecomparison1: TMenuItem;
    CSVforVDATUM1: TMenuItem;
    DEMIXhalfsecto2onesec1: TMenuItem;
    Removequickoverlayhillshade1: TMenuItem;
    N58: TMenuItem;
    PerpProfiles1: TMenuItem;
    NearestpeakoneachDEM1: TMenuItem;
    Specifyxyzshifts1: TMenuItem;
    UsingVDATUMoutput1: TMenuItem;
    UsingVDATUM1: TMenuItem;
    WGS84elllipsoid1: TMenuItem;
    Pickmapsforbigimage1: TMenuItem;
    dNBRNBRbeforeandafterfire1: TMenuItem;
    NBR21: TMenuItem;
    Percent1: TMenuItem;
    GDALgridsubsettomatchthismap1: TMenuItem;
    ClipDEMtofullDEMIXtiles1: TMenuItem;
    N61: TMenuItem;
    N62: TMenuItem;
    Roundtointegers1: TMenuItem;
    PicksingleDEMseriesthisarea1: TMenuItem;
    DEMIXrangescales1: TMenuItem;
    SSIM1: TMenuItem;
    SAGAremovesinksallopenDEMs1: TMenuItem;
    SAGAchannelnetworkallopenDEMs1: TMenuItem;
    SAGAChannelNetworkandBasins1: TMenuItem;
    GDALrasterizehapfiels1: TMenuItem;
    Pureplatecareeprojectiondistorted1: TMenuItem;
    Rasterizedatabases1: TMenuItem;
    Comparechannelnetworks1: TMenuItem;
    Landcover1: TMenuItem;
    SAGAremovesinks1: TMenuItem;
    N63: TMenuItem;
    LC100landcoverwaterbodies1: TMenuItem;
    SAGAedgecontaminationmap1: TMenuItem;
    SAGADrainagebasins1: TMenuItem;
    Whiteboxdrainagebasins1: TMenuItem;
    SAGAwatershedbasinsWangLiu1: TMenuItem;
    SAGAStrahlerordergrid1: TMenuItem;
    SAGAflowaccumulationParallelizable1: TMenuItem;
    Whieboxflowaccumulationlog1: TMenuItem;
    Whiteboxflowaccumulation1: TMenuItem;
    Numbercells1: TMenuItem;
    FD8Lognumbercells1: TMenuItem;
    FD8Lognumbercells2: TMenuItem;
    Whiteboxwetnessindex1: TMenuItem;
    SAGALSfactor1: TMenuItem;
    //procedure HiresintervisibilityDEM1Click(Sender: TObject);
    procedure Waverefraction1Click(Sender: TObject);
    procedure Multipleparameters1Click(Sender: TObject);
    procedure Mask1Click(Sender: TObject);
    procedure Smallcirclethroughpoint1Click(Sender: TObject);
    procedure Greatcirclethroughpoint1Click(Sender: TObject);
    procedure DataBaseSpeedButton28Click(Sender: TObject);
    procedure Greatcircleroute1Click(Sender: TObject);
    procedure SubbottomSpeedButtonClick(Sender: TObject);
    procedure SideScanButtonClick(Sender: TObject);
    procedure Findpeaks1Click(Sender: TObject);
    procedure Pastefromclipboard1Click(Sender: TObject);
    procedure Duplicatemapwindow1Click(Sender: TObject);
    procedure KoppenSpeedButtonClick(Sender: TObject);
    procedure Flattenlake1Click(Sender: TObject);
    procedure Pointsnotmeetingcriteria1Click(Sender: TObject);
    procedure ReloadDEMClick(Sender: TObject);
    procedure Integrateddatabasesetup1Click(Sender: TObject);
    procedure Mapshadingoptions2Click(Sender: TObject);
    procedure Mapshadingoptions1Click(Sender: TObject);
    procedure RGBvalues1Click(Sender: TObject);
    procedure Medianfilter1Click(Sender: TObject);
    procedure Expandgivenzrange1Click(Sender: TObject);
    procedure Setzrangetoaconstant1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
    procedure FormCloseQuery(Sender : TObject; var CanClose: Boolean);
    procedure Image1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Image1DblClick(Sender: TObject);
    procedure Print1Click(Sender: TObject);
    procedure Saveimage1Click(Sender: TObject);
    procedure Contour1Click(Sender: TObject);
    procedure Contour2Click(Sender: TObject);
    procedure Distance1Click(Sender: TObject);
    procedure Slope2Click(Sender: TObject);
    procedure Forceredraw1Click(Sender: TObject);
    procedure Agefromdepth1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Terraincategories1Click(Sender: TObject);
    procedure Reflectance2Click(Sender: TObject);
    procedure ContourBands1Click(Sender: TObject);
    procedure Bearing1Click(Sender: TObject);
    procedure Subset1Click(Sender: TObject);
    procedure Forcesize1Click(Sender: TObject);
    procedure Offset1Click(Sender: TObject);
    procedure Contourinterval1Click(Sender: TObject);
    procedure Reflectance1Click(Sender: TObject);
    procedure Blank1Click(Sender: TObject);
    procedure StreamProfile1Click(Sender: TObject);
    procedure VectorOutlines1Click(Sender: TObject);
    procedure Delete1Click(Sender: TObject);
    procedure Replace1Click(Sender: TObject);
    procedure Movewindow1Click(Sender: TObject);
    procedure Highpoints1Click(Sender: TObject);
    procedure LowPoints1Click(Sender: TObject);
    procedure Specified1Click(Sender: TObject);
    procedure Histogram1Click(Sender: TObject);
    procedure SaveDEM2Click(Sender: TObject);
    procedure FullDEM1Click(Sender: TObject);
    procedure Feet1Click(Sender: TObject);
    procedure Magneticanomaly1Click(Sender: TObject);
    procedure Blowup2Click(Sender: TObject);
    procedure Zoomout1Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditWeaponsFan1Click(Sender: TObject);
    procedure Rangecircles1Click(Sender: TObject);
    procedure PreparePrinterImage1Click(Sender: TObject);
    procedure InfoSpeedButton6Click(Sender: TObject);
    procedure PrintSpeedButtonClick(Sender: TObject);
    procedure SaveSpeedButtonClick(Sender: TObject);
    procedure ZoomInSpeedButton4Click(Sender: TObject);
    procedure ZoomOutSpeedButton5Click(Sender: TObject);
    procedure SubsetSpeedButtonClick(Sender: TObject);
    procedure WeaponFanSpeedButton8Click(Sender: TObject);
    procedure RangeCirclesSpeedButton9Click(Sender: TObject);
    procedure FullMapSpeedButtonClick(Sender: TObject);
    procedure RedrawSpeedButton12Click(Sender: TObject);
    procedure MeasureDistanceSpeedButton14Click(Sender: TObject);
    procedure GridSpeedButton15Click(Sender: TObject);
    procedure Setgridparameters1Click(Sender: TObject);
    procedure ClipboardSpeedButtonClick(Sender: TObject);
    procedure Elevation3Click(Sender: TObject);
    procedure Reflectance3Click(Sender: TObject);
    procedure Contour3Click(Sender: TObject);
    procedure Blank2Click(Sender: TObject);
    procedure Display1Click(Sender: TObject);
    procedure SlopeCategories1Click(Sender: TObject);
    procedure DupeMapSpeedButton18Click(Sender: TObject);
    procedure PickBandSpeedButton20Click(Sender: TObject);
    procedure BandColor1Click(Sender: TObject);
    procedure Pickbandcolors1Click(Sender: TObject);
    procedure Contrast2Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure Topographicgrain2Click(Sender: TObject);
    procedure Grainbyregionsize1Click(Sender: TObject);
    procedure Band1Click(Sender: TObject);
    procedure Fabricatpoint1Click(Sender: TObject);
    procedure Colors2Click(Sender: TObject);
    procedure Erasersize1Click(Sender: TObject);
    procedure GDALinfo1Click(Sender: TObject);
    //procedure Rasteraftersubsettomatchthismapextent1Click(Sender: TObject);
    procedure GDALtranslatesubset1Click(Sender: TObject);
    procedure ContoursShapefile1Click(Sender: TObject);
    procedure GDALslopemap1Click(Sender: TObject);
    procedure GDALslopemapHorn1Click(Sender: TObject);
    procedure Whiteboxaspectmap1Click(Sender: TObject);
    procedure GDALaspectmapZevenbergenThorne1Click(Sender: TObject);
    procedure GDALaspectmapHorn1Click(Sender: TObject);
    procedure Whiteboxfillholes2Click(Sender: TObject);
    procedure Gridoutlines1Click(Sender: TObject);
    procedure N1pixel2Click(Sender: TObject);
    procedure N1pixel1Click(Sender: TObject);
    procedure Directionalslopes1Click(Sender: TObject);
    procedure GDALfillholes1Click(Sender: TObject);
    procedure Allpoints1Click(Sender: TObject);
    procedure Isplandperimeter1Click(Sender: TObject);
    procedure ForceAllRedraw1Click(Sender: TObject);
    procedure Pointinterpolationalgorithms2Click(Sender: TObject);
    procedure GDALupsample1Click(Sender: TObject);
    procedure GDALbilinearbicubictoUTM1Click(Sender: TObject);
    procedure GDALhillshadeHorn1Click(Sender: TObject);
    procedure GDALroughness1Click(Sender: TObject);


procedure GDALTPI1Click(Sender: TObject);
procedure GDALTRIRileyterrestrial1Click(Sender: TObject);
procedure GDALTRIWilsonbathymetric1Click(Sender: TObject);
procedure GDALwarpsubset1Click(Sender: TObject);

procedure CreateMedianDNgrid1Click(Sender: TObject);


    //procedure Datumshift1Click(Sender: TObject);
    procedure Grainmovie1Click(Sender: TObject);
    procedure DEMcolormerge1Click(Sender: TObject);
    procedure Closepolyline1Click(Sender: TObject);
    procedure Endpolyline1Click(Sender: TObject);
    procedure VectorOverlaySpeedButton21Click(Sender: TObject);
    procedure UnsubsetSpeedButton22Click(Sender: TObject);
    procedure Mergecolors1Click(Sender: TObject);
    procedure Savemapwithworldfile1Click(Sender: TObject);
    procedure TIGERoptions1Click(Sender: TObject);
    procedure Loadprojection2Click(Sender: TObject);
    //procedure Saveprojection2Click(Sender: TObject);
    //procedure Modifyprojection1Click(Sender: TObject);
    procedure Reflectanceoptions1Click(Sender: TObject);
    procedure Slopecategories2Click(Sender: TObject);
    procedure RightClickContourIntervalClick(Sender: TObject);
    procedure Elevationcolors1Click(Sender: TObject);
    //procedure Grid1Click(Sender: TObject);
    procedure Displayparameter1Click(Sender: TObject);
    //procedure Loadroute1Click(Sender: TObject);
    //procedure DEMSpeedButton25Click(Sender: TObject);
    procedure TIGERVectorlegend1Click(Sender: TObject);
    //procedure Editroute1Click(Sender: TObject);
    procedure Routeobservation1Click(Sender: TObject);
    procedure Replayflightroute1Click(Sender: TObject);
    //procedure Mapcorners1Click(Sender: TObject);
    procedure Removemerge1Click(Sender: TObject);
    procedure DrawColoredMap1Click(Sender: TObject);
    procedure Area1Click(Sender: TObject);
    procedure Meters1Click(Sender: TObject);
    procedure Database1Click(Sender: TObject);
    procedure Markasmissing1Click(Sender: TObject);
    procedure Outlineregionofinterest1Click(Sender: TObject);
    procedure Land1Click(Sender: TObject);
    procedure Water1Click(Sender: TObject);
    procedure Saveimage2Click(Sender: TObject);
    procedure Newrouteobservation1Click(Sender: TObject);
    procedure Restorerouteobservation1Click(Sender: TObject);
    procedure Editfan1Click(Sender: TObject);
    procedure Floodbasin1Click(Sender: TObject);
    procedure SavemapasGEOTIFF1Click(Sender: TObject);
    procedure Help1Click(Sender: TObject);
    procedure Lineshapefile1Click(Sender: TObject);
    procedure Mapdistortion1Click(Sender: TObject);
    procedure Magneticvariation1Click(Sender: TObject);
    procedure Tissotindicatrix1Click(Sender: TObject);
    procedure Abortcurrentoperation1Click(Sender: TObject);
    procedure Areashapefile1Click(Sender: TObject);
    procedure DEMelevationcolors1Click(Sender: TObject);
    procedure ZoomIn1Click(Sender: TObject);
    procedure NoZoom1Click(Sender: TObject);
    procedure ZoomOut2Click(Sender: TObject);
    procedure Plateboundaries1Click(Sender: TObject);
    //procedure Interpolatesatellitecolors1Click(Sender: TObject);
    procedure N11view1Click(Sender: TObject);
    procedure Mergecolorscene1Click(Sender: TObject);
    procedure Pointsshapefile1Click(Sender: TObject);
    procedure Clearmap1Click(Sender: TObject);
    //procedure AllTIGER1Click(Sender: TObject);
    procedure Copytoclipboard1Click(Sender: TObject);
    procedure AnnotateSpeedButton1Click(Sender: TObject);
    procedure Militaryicons1Click(Sender: TObject);
    procedure Annotatemap1Click(Sender: TObject);
    procedure Blowupmapoverviewdetail1Click(Sender: TObject);
    procedure Whereiskeyboard1Click(Sender: TObject);
    procedure Earthquakes1Click(Sender: TObject);
    procedure Volcanoes1Click(Sender: TObject);
    procedure SetDEMwithmap1Click(Sender: TObject);
    //procedure Pointslopealgorithm1Click(Sender: TObject);
    procedure IDSpeedButtonClick(Sender: TObject);
    procedure MapLibSpeedButtonClick(Sender: TObject);
    procedure Keyboard1Click(Sender: TObject);
    procedure Loadweaponsfans1Click(Sender: TObject);
    procedure EditGridButtonClick(Sender: TObject);
    procedure Mapannotation1Click(Sender: TObject);
    procedure LabelLatLongdifference1Click(Sender: TObject);
    procedure LabelUTMdifference1Click(Sender: TObject);
    procedure NoLabels1Click(Sender: TObject);
    procedure Horizonblocking1Click(Sender: TObject);
    procedure Mapmarginalia1Click(Sender: TObject);
    procedure SpeedButton22Click(Sender: TObject);
    procedure Aspect1Click(Sender: TObject);
    procedure MDDEM1Click(Sender: TObject);
    procedure DTED1Click(Sender: TObject);
    procedure ASCII1Click(Sender: TObject);
    procedure Excessiveslopes1Click(Sender: TObject);
    procedure FocalMechsButtonClick(Sender: TObject);
    procedure Earthquakefocalmechanisms1Click(Sender: TObject);
    procedure Setscale1Click(Sender: TObject);
    procedure GridPosts2Click(Sender: TObject);
    procedure Elevation2Click(Sender: TObject);
    procedure Expandcolorrange1Click(Sender: TObject);
    procedure Platerotations1Click(Sender: TObject);
    procedure Viewshed2Click(Sender: TObject);
    procedure Lineofsight1Click(Sender: TObject);
    procedure ID1Click(Sender: TObject);
    procedure Steepestslope1Click(Sender: TObject);
    procedure Keylatitudes1Click(Sender: TObject);
    procedure Digitizecontours1Click(Sender: TObject);
    procedure Gazetteer1Click(Sender: TObject);
    procedure Geology1Click(Sender: TObject);
    procedure GeologySpeedButton1Click(Sender: TObject);
    procedure Fracturezones1Click(Sender: TObject);
    procedure Variableopaquemerge1Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure Overlaysvariableopaque1Click(Sender: TObject);
    procedure Geologysymbols1Click(Sender: TObject);
    procedure Usecurrentsubset1Click(Sender: TObject);
    //procedure Combinecurrentimages1Click(Sender: TObject);
    procedure Geodeticbearing1Click(Sender: TObject);
    procedure UTMgridtruenorthdeclination1Click(Sender: TObject);
    procedure Font1Click(Sender: TObject);
    procedure PLSSposition1Click(Sender: TObject);
    procedure USGSASCII1Click(Sender: TObject);
    procedure Outlineholes1Click(Sender: TObject);
    procedure Outlinelakes1Click(Sender: TObject);
    procedure Horizontalearthcurvature1Click(Sender: TObject);
    procedure Requiredantennaheight1Click(Sender: TObject);
    procedure USGSquadnames1Click(Sender: TObject);
    procedure Viewshedalgorithms1Click(Sender: TObject);
    procedure BlueMarble1Click(Sender: TObject);
    procedure Predictedseafloorages1Click(Sender: TObject);
    procedure OtherDEM1Click(Sender: TObject);
    procedure Pointselectionalgorithms1Click(Sender: TObject);
    procedure Vectoroutlines2Click(Sender: TObject);
    procedure Editshapefilegroup1Click(Sender: TObject);
    procedure Show1Click(Sender: TObject);
    procedure Label2Click(Sender: TObject);
    procedure CurrentsubsetMDDEM1Click(Sender: TObject);
    procedure Volume1Click(Sender: TObject);
    procedure Slope1Click(Sender: TObject);
    procedure Aspect2Click(Sender: TObject);
    procedure erraincategory1Click(Sender: TObject);
    procedure Lntransformelevs1Click(Sender: TObject);
    procedure Log10transformelevs1Click(Sender: TObject);
    procedure Crosssectionalcurvature1Click(Sender: TObject);
    procedure Profileconvexity1Click(Sender: TObject);
    procedure Planconvexity1Click(Sender: TObject);
    procedure Derivativegrid1Click(Sender: TObject);
    procedure Slope3Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure Filtered1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure Slopesin1Click(Sender: TObject);
    procedure Minimumcurvature1Click(Sender: TObject);
    procedure Maximumcurvature1Click(Sender: TObject);
    procedure Pointmodedigitizing1Click(Sender: TObject);
    procedure Streamdigitizing1Click(Sender: TObject);
    procedure N16bitBSQ1Click(Sender: TObject);
    procedure Viewshed1Click(Sender: TObject);
    procedure Derivativegrid2Click(Sender: TObject);
    procedure Addgrids1Click(Sender: TObject);
    procedure Terraincategories2Click(Sender: TObject);
    procedure Requiredantennaheight2Click(Sender: TObject);
    procedure BIL1Click(Sender: TObject);
    procedure Gridfloat1Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure FennemanProvinces1Click(Sender: TObject);
    //procedure StatePlaneCoordinateSystem1Click(Sender: TObject);
    procedure Dataheader1Click(Sender: TObject);
    procedure Editpointelevations1Click(Sender: TObject);
    procedure DRGanaglyph1Click(Sender: TObject);
    //procedure UScounty1Click(Sender: TObject);
    procedure Openimage1Click(Sender: TObject);
    //procedure Scatterplotoftwogrids2Click(Sender: TObject);
    procedure Downhillvectors1Click(Sender: TObject);
    procedure Terrainblowup1Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure Convertcoordinates1Click(Sender: TObject);
    procedure ASCIIArcGrid1Click(Sender: TObject);
    procedure SpeedButton5Click(Sender: TObject);
    procedure Contributingarea1Click(Sender: TObject);
    procedure Fansensitivity1Click(Sender: TObject);
    //procedure LabelTIGERroads1Click(Sender: TObject);
    //procedure LocateTIGERRoad1Click(Sender: TObject);
    procedure Hotspots1Click(Sender: TObject);
    procedure Agedepthcurve1Click(Sender: TObject);
   // procedure SpeedButton6Click(Sender: TObject);
    procedure Structuralgeologycomputations1Click(Sender: TObject);
    procedure Projectfocalmechanismtosurface1Click(Sender: TObject);
    procedure DEMelevationmap1Click(Sender: TObject);
    procedure OBJ1Click(Sender: TObject);
    procedure PGM1Click(Sender: TObject);
    procedure PGM8bit1Click(Sender: TObject);
    procedure EditDEMgrid1Click(Sender: TObject);
    procedure Databaselegend1Click(Sender: TObject);
    procedure KoppenSpeedButton7Click(Sender: TObject);
    procedure CompareviewshedsonmultipleDEMs1Click(Sender: TObject);
    procedure RenameDEM1Click(Sender: TObject);
    procedure Elevationsextremes1Click(Sender: TObject);
    procedure Datavoids1Click(Sender: TObject);
    procedure Ridges1Click(Sender: TObject);
    procedure Allmapsmatchthiscoveragearea1Click(Sender: TObject);
    procedure Sameelevationcolors1Click(Sender: TObject);
    procedure Cancelpendingselection1Click(Sender: TObject);
    procedure Ratiooftwogrids1Click(Sender: TObject);
    procedure EditViaSelectedcolor1Click(Sender: TObject);
    procedure Allbutselectedcolor1Click(Sender: TObject);
    procedure SetMaskGrid1Click(Sender: TObject);
    procedure MaskDEM1Click(Sender: TObject);
    procedure ThinDEM1Click(Sender: TObject);
    procedure Floodfill1Click(Sender: TObject);
    procedure Setmappixelsize1Click(Sender: TObject);
    procedure StratcolButtonClick(Sender: TObject);
    procedure DEMreflectance1Click(Sender: TObject);
    procedure SRTMwaterbodies2Click(Sender: TObject);
    procedure TrackBar2Change(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure SpeedButton12Click(Sender: TObject);
    procedure SpeedButton13Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure MrSidSpeedButtonClick(Sender: TObject);
    procedure Legendsmarginalia1Click(Sender: TObject);
    procedure Geographicquadrangle1Click(Sender: TObject);
    procedure RGBthreeparametermap1Click(Sender: TObject);
    procedure Accumulatedcostsurface1Click(Sender: TObject);
    procedure Leastcostpath1Click(Sender: TObject);
    procedure Labelboth1Click(Sender: TObject);
    procedure Multiplyzvalues1Click(Sender: TObject);
    procedure Raiselowerzvalues1Click(Sender: TObject);
    procedure Interpolateacrossholessmooth1Click(Sender: TObject);
    procedure Koppenlegend1Click(Sender: TObject);
    procedure Selectedpercentilerange1Click(Sender: TObject);
    procedure O1Click(Sender: TObject);
    procedure OutlineregiontoreplacevalueswithreferenceDEM1Click(Sender: TObject);
    procedure NScomponentofaspect1Click(Sender: TObject);
    procedure EWcomponent1Click(Sender: TObject);
    procedure Filter1Click(Sender: TObject);
    procedure Descriptions1Click(Sender: TObject);
    procedure Sunrisesunsettimes1Click(Sender: TObject);
    procedure Redraw1Click(Sender: TObject);
    procedure Reregisterimage1Click(Sender: TObject);
    procedure Matchimage1Click(Sender: TObject);
    procedure AnaglyphSpeedButtonClick(Sender: TObject);
    procedure Createbitmapmask1Click(Sender: TObject);
    procedure Latituderange1Click(Sender: TObject);
    procedure Gazetteerlegend1Click(Sender: TObject);
    //procedure Landcovercategories1Click(Sender: TObject);
    procedure Missingpointsinmatchinggrid1Click(Sender: TObject);
    procedure Outlinecoveragearea1Click(Sender: TObject);
    procedure Areaofsinglecolor1Click(Sender: TObject);
    procedure Customzoom1Click(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure Shapefilegrouplegend1Click(Sender: TObject);
    procedure Loadgrouping1Click(Sender: TObject);
    procedure Updategrouping1Click(Sender: TObject);
    procedure Addfiles1Click(Sender: TObject);
    procedure Creategroputing1Click(Sender: TObject);
    procedure KeyClick(Sender: TObject);
    procedure Overlays1Click(Sender: TObject);
    procedure Manageoverlays2Click(Sender: TObject);
    procedure SpeedButton7Click(Sender: TObject);
    procedure Loadvegetationgrid1Click(Sender: TObject);
    procedure DSDPODPIODPdrilling1Click(Sender: TObject);
    procedure Loadcartographicoverlay1Click(Sender: TObject);
    procedure Sectoroutlines1Click(Sender: TObject);
    procedure Contoursfromsecondgrid1Click(Sender: TObject);
    procedure RoamingZvaluesfrom2dgrid1Click(Sender: TObject);
    procedure Meanfilter1Click(Sender: TObject);
    procedure ForGoogleearth1Click(Sender: TObject);
    procedure CreateimagetomatchDEM1Click(Sender: TObject);
    procedure Supervisedclassification1Click(Sender: TObject);
    procedure Histogramallbands1Click(Sender: TObject);
    procedure Histogrampickband1Click(Sender: TObject);
    procedure Variancecovariance1Click(Sender: TObject);
    procedure Optimumindexfactor1Click(Sender: TObject);
    procedure NDVI1Click(Sender: TObject);
    procedure NDSI1Click(Sender: TObject);
    procedure NDWI1Click(Sender: TObject);
    procedure Addbands1Click(Sender: TObject);
    procedure NormalizedBurnIndex1Click(Sender: TObject);
    procedure Bandratio1Click(Sender: TObject);
    procedure Newband1Click(Sender: TObject);
    procedure Spectrallibrarygraph1Click(Sender: TObject);
    procedure Createtablefromimage1Click(Sender: TObject);
    procedure DifClick(Sender: TObject);
    procedure Maparea1Click(Sender: TObject);
    procedure Modifymaparea1Click(Sender: TObject);
    procedure ZoomIn2Click(Sender: TObject);
    procedure Customimagesize1Click(Sender: TObject);
    procedure Subsetgraphicalzoom1Click(Sender: TObject);
    procedure Fulldataset1Click(Sender: TObject);
    procedure Googlemaps1Click(Sender: TObject);
    procedure Quicksealevelrise1Click(Sender: TObject);
    procedure AsGEOTIFFscreenscalegrayscale1Click(Sender: TObject);
    procedure PanSEButtonClick(Sender: TObject);
    procedure PanWButtonClick(Sender: TObject);
    procedure PanEButtonClick(Sender: TObject);
    procedure PanNButtonClick(Sender: TObject);
    procedure PanSButtonClick(Sender: TObject);
    procedure PanSWButtonClick(Sender: TObject);
    procedure PanNWButtonClick(Sender: TObject);
    procedure PanNEButtonClick(Sender: TObject);
    procedure Forcecompleteredraw1Click(Sender: TObject);
    procedure Imagepalette1Click(Sender: TObject);
    procedure Minimumfilter1Click(Sender: TObject);
    procedure Maximumfilter1Click(Sender: TObject);
    procedure Mergegridstakelowestvalue1Click(Sender: TObject);
    procedure DBFfile1Click(Sender: TObject);
    procedure Worldfileimages1Click(Sender: TObject);
    procedure Tools1Click(Sender: TObject);
    //procedure VegetationobstaclegridfromNBCD20001Click(Sender: TObject);
    procedure GeoTIFF1Click(Sender: TObject);
    procedure Forceredrawbasemaplayer1Click(Sender: TObject);
    procedure Mergeallimages1Click(Sender: TObject);
    procedure Batchrequiredantennaheights1Click(Sender: TObject);
    procedure Peakislandarea1Click(Sender: TObject);
    //procedure LandFiregrid1Click(Sender: TObject);
    procedure RidgesClick(Sender: TObject);
    procedure Maskmap1Click(Sender: TObject);
    procedure Editgazetteersymbology1Click(Sender: TObject);
    procedure Print2Click(Sender: TObject);
    procedure Vegetationmap1Click(Sender: TObject);
    procedure Everythingabovecutoff1Click(Sender: TObject);
    procedure Everythingbelowcutoff1Click(Sender: TObject);
    procedure Fresnelzones1Click(Sender: TObject);
    procedure Continentalcrust1Click(Sender: TObject);
    procedure Geomorphatlas1Click(Sender: TObject);
    procedure Outsideselectedrange1Click(Sender: TObject);
    procedure Outsideselectedpercentilerange1Click(Sender: TObject);
    procedure UndoSpeedButtonClick(Sender: TObject);
    procedure Legend1Click(Sender: TObject);
    procedure Maplibrary1Click(Sender: TObject);
    procedure Maplibrarycoverage1Click(Sender: TObject);
    procedure Copycoordinates1Click(Sender: TObject);
    procedure Saverangecircles1Click(Sender: TObject);
    procedure Geostationarysatellitevisibility1Click(Sender: TObject);
    procedure Parametricisotropicsmoothing1Click(Sender: TObject);
    procedure Imagenegative1Click(Sender: TObject);
    procedure LatlongofPLSSposition1Click(Sender: TObject);
    //procedure Addtoshapefile1Click(Sender: TObject);
    procedure Colorsfromgridcategories1Click(Sender: TObject);
    procedure Normalizeparameter1Click(Sender: TObject);
    procedure ModifyTIGERdisplay1Click(Sender: TObject);
    procedure States1Click(Sender: TObject);
    procedure Counties1Click(Sender: TObject);
    procedure Rivers1Click(Sender: TObject);
    procedure Highways1Click(Sender: TObject);
    procedure Map2Click(Sender: TObject);
    procedure XYZpointsshapefile1Click(Sender: TObject);
    procedure Copyimagecoordinatestoclipboard1Click(Sender: TObject);
    procedure Panel1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Highend1Click(Sender: TObject);
    procedure Lowend1Click(Sender: TObject);
    //procedure NLCD1992grid1Click(Sender: TObject);
    procedure Singlevalue1Click(Sender: TObject);
    procedure Removemissingpointsinmask1Click(Sender: TObject);
    procedure AquastatClimate1Click(Sender: TObject);
    procedure Truenorthlines1Click(Sender: TObject);
    procedure Drawmultiplefans1Click(Sender: TObject);
    procedure LASFile1Click(Sender: TObject);
    procedure Parameterpercentiles1Click(Sender: TObject);
    procedure Resoremenus1Click(Sender: TObject);
    procedure Pointcloud1Click(Sender: TObject);
    procedure Suminbox1Click(Sender: TObject);
    procedure Forceredrawlegendsscalebars1Click(Sender: TObject);
    procedure Clearrangecircles1Click(Sender: TObject);
    procedure suanamitravel1Click(Sender: TObject);
    procedure Driftmodels1Click(Sender: TObject);
    procedure Elevationmoments1Click(Sender: TObject);
    procedure Slopemoments1Click(Sender: TObject);
    procedure Plancurvature1Click(Sender: TObject);
    procedure Profilecurvature1Click(Sender: TObject);
    procedure Allreliefmeasures1Click(Sender: TObject);
    procedure Gridaveragestddev1Click(Sender: TObject);
    procedure Rugosity1Click(Sender: TObject);
    procedure GridVATlegend1Click(Sender: TObject);
    procedure VATdisplay1Click(Sender: TObject);
    procedure Convergenceindex1Click(Sender: TObject);
    procedure PointCloudSpeedButtonClick(Sender: TObject);
    procedure VARI1Click(Sender: TObject);
    procedure RGBgrayscale1Click(Sender: TObject);
    procedure Erodeimage1Click(Sender: TObject);
    procedure Unsupervisedclassification1Click(Sender: TObject);
    procedure AllbandsasGeotiffs1Click(Sender: TObject);
    procedure DEMGridarea1Click(Sender: TObject);
    procedure WMSMapsClick(Sender: TObject);
    procedure Polarorbitersatellitevisibility1Click(Sender: TObject);
    procedure Loadsidescanimagery1Click(Sender: TObject);
    procedure MaptoGoogleEarth1Click(Sender: TObject);
    procedure GridgraticluetoGoogleEarth1Click(Sender: TObject);
    procedure GlobalTopoOverlay1Click(Sender: TObject);
    procedure Globaltopography1Click(Sender: TObject);
    procedure hismap1Click(Sender: TObject);
    procedure Allmaps1Click(Sender: TObject);
    procedure Pointdensity1Click(Sender: TObject);
    procedure PickcornersMDDEM1Click(Sender: TObject);
    procedure LibSpeedButtonClick(Sender: TObject);
    procedure Abbreviations1Click(Sender: TObject);
    procedure Climograph1Click(Sender: TObject);
    procedure Pointsalongsurveylines1Click(Sender: TObject);
    procedure Excessiveslopes2Click(Sender: TObject);
    procedure DEMfromseries1Click(Sender: TObject);
    procedure Copymaptoclipboard1Click(Sender: TObject);
    procedure DEMfromseriespoint1Click(Sender: TObject);
    procedure Cartogrouplegend1Click(Sender: TObject);
    procedure SetsecondDEMonmap1Click(Sender: TObject);
    procedure Saveproject1Click(Sender: TObject);
    procedure UTM1Click(Sender: TObject);
    procedure LatLong1Click(Sender: TObject);
    procedure Exportcomputations1Click(Sender: TObject);
    //procedure NLCD2006grid1Click(Sender: TObject);
    procedure Streams1Click(Sender: TObject);
    procedure Allterraincategories1Click(Sender: TObject);
    procedure PortionofDEMwithdata1Click(Sender: TObject);
    procedure ColumnsEastLimit1Click(Sender: TObject);
    procedure Rowssouthoflimit1Click(Sender: TObject);
    procedure ColumnsWestLimit1Click(Sender: TObject);
    procedure Rowsnorthoflimit1Click(Sender: TObject);
    procedure Validpointsinmatchinggrid1Click(Sender: TObject);
    procedure DSMfirstreturn1Click(Sender: TObject);
    procedure HAGvegheight1Click(Sender: TObject);
    procedure DTMlastreturn1Click(Sender: TObject);
    procedure Geocodeaddress1Click(Sender: TObject);
    procedure Addresstolatlong1Click(Sender: TObject);
    procedure ViewExifimages1Click(Sender: TObject);
    procedure Multiplevalues1Click(Sender: TObject);
    procedure Abortcurrentoperation2Click(Sender: TObject);
    procedure PLSSfromkeyboardlocation1Click(Sender: TObject);
    procedure QuickbasemaptoGoogleEarth1Click(Sender: TObject);
    procedure Editfans1Click(Sender: TObject);
    procedure Snglepointspires1Click(Sender: TObject);
    procedure Singlepointpits1Click(Sender: TObject);
    procedure Clearoverlays1Click(Sender: TObject);
    procedure Bytedata0tomissing1Click(Sender: TObject);
    procedure Subtractopengrid1Click(Sender: TObject);
    procedure Loadvegetationlayers1Click(Sender: TObject);
    procedure Vegetationlayers1Click(Sender: TObject);
    procedure Vegetationdensitygraph1Click(Sender: TObject);
    procedure Vegetationdensitymovie1Click(Sender: TObject);
    procedure Searchpowerlines1Click(Sender: TObject);
    procedure Buildingedges1Click(Sender: TObject);
    procedure Missingdatatospecifiedvalue1Click(Sender: TObject);
    procedure Rangetosinglevalue1Click(Sender: TObject);
    procedure CopyVegprofiletoclipboard1Click(Sender: TObject);
    procedure Vegetationlayeroverlay1Click(Sender: TObject);
    procedure Walls1Click(Sender: TObject);
    procedure UTMspecifyzone1Click(Sender: TObject);
    procedure Loadsecondvegeationlayers1Click(Sender: TObject);
    procedure Minimumoftwogrids1Click(Sender: TObject);
    procedure Statistics1Click(Sender: TObject);
    procedure LASexport1Click(Sender: TObject);
    procedure LoadLOStopoprofile1Click(Sender: TObject);
    procedure Loadvegetationgrid21Click(Sender: TObject);
    procedure DeliberatemaptoGoogleEarth1Click(Sender: TObject);
    procedure Magneticdeclination1Click(Sender: TObject);
    procedure Percentages1Click(Sender: TObject);
    procedure Counts1Click(Sender: TObject);
    procedure Everythingexceptsinglevalue1Click(Sender: TObject);
    procedure Changemap1Click(Sender: TObject);
    procedure Undefined1Click(Sender: TObject);
    procedure Integercode1Click(Sender: TObject);
    procedure KoppenClimateStations1Click(Sender: TObject);
    procedure Globalmonthlywinds1Click(Sender: TObject);
    //procedure Recentworldpiracy1Click(Sender: TObject);
    procedure Latitudinalscalebars1Click(Sender: TObject);
    procedure Koppengrid1Click(Sender: TObject);
    procedure Level0countries1Click(Sender: TObject);
    procedure Level1stateprovince1Click(Sender: TObject);
    procedure Weather1Click(Sender: TObject);
    procedure Shipmotion1Click(Sender: TObject);
    procedure ForcereloadWMSmap1Click(Sender: TObject);
    procedure ClearWMSmap1Click(Sender: TObject);
    procedure Quickmap1Click(Sender: TObject);
    procedure Detailedmap1Click(Sender: TObject);
    procedure NLCDLegend1Click(Sender: TObject);
    procedure GE_SpeedButtonClick(Sender: TObject);
    procedure OGL_speedbuttonClick(Sender: TObject);
    procedure Focalplaneanalysis1Click(Sender: TObject);
    procedure CandeandKenttimescale1Click(Sender: TObject);
    procedure AverageDN1Click(Sender: TObject);
    procedure MaxDN1Click(Sender: TObject);
    procedure Locatehighvalues1Click(Sender: TObject);
    procedure Opengrid1Click(Sender: TObject);
    procedure ailpercentiles1Click(Sender: TObject);
    procedure Weather2Click(Sender: TObject);
    procedure Normalizie1Click(Sender: TObject);
    procedure StdDevinbox1Click(Sender: TObject);
    procedure Missingdatacolor1Click(Sender: TObject);
    procedure Filledneighborhood1Click(Sender: TObject);
    procedure CreateMinDNgrid1Click(Sender: TObject);
    procedure Averagereflectanceinregion1Click(Sender: TObject);
    procedure Cloudbrightening1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure Missingpoints1Click(Sender: TObject);
    procedure Validpoints1Click(Sender: TObject);
    procedure Replacefromsecondgrid1Click(Sender: TObject);
    procedure Bytes1Click(Sender: TObject);
    procedure Integer161Click(Sender: TObject);
    procedure Word1Click(Sender: TObject);
    procedure Floatingpoint1Click(Sender: TObject);
    procedure Pointsfromdatabase1Click(Sender: TObject);
    procedure Roads1Click(Sender: TObject);
    procedure Allvalidtosinglevalue1Click(Sender: TObject);
    //procedure Classbydistance1Click(Sender: TObject);
    procedure Logbase10transform1Click(Sender: TObject);
    procedure Lntransform1Click(Sender: TObject);
    procedure Immediateneighbordropoff1Click(Sender: TObject);
    procedure Rastertovector1Click(Sender: TObject);
    procedure Mask2Click(Sender: TObject);
    procedure Verifyelevationrange1Click(Sender: TObject);
    //procedure GDALresamplethin1Click(Sender: TObject);
    procedure Ransacplane1Click(Sender: TObject);
    procedure Erodedilate1Click(Sender: TObject);
    procedure LinedetectionHoughtransform1Click(Sender: TObject);
    procedure Inf1Click(Sender: TObject);
    procedure NAN1Click(Sender: TObject);
    procedure LinedetectionHoughtransform2Click(Sender: TObject);
    procedure Numberimmediateneighbors1Click(Sender: TObject);
    procedure Singlevalue2Click(Sender: TObject);
    procedure Gridmigration1Click(Sender: TObject);
    procedure Featuremigration2Click(Sender: TObject);
    procedure Monthlywinds1Click(Sender: TObject);
    procedure DEMsatpoint1Click(Sender: TObject);
    procedure Gridmaskcolor1Click(Sender: TObject);
    procedure Featuregeomorphometry1Click(Sender: TObject);
    procedure Loadfeaturegrid1Click(Sender: TObject);
    procedure PNG1Click(Sender: TObject);
    procedure JSON1Click(Sender: TObject);
   // procedure idegaugesandsealevelrise1Click(Sender: TObject);
    procedure Globalmonthlytemperatures1Click(Sender: TObject);
    procedure Globalmonthlyrain1Click(Sender: TObject);
    procedure Western1Click(Sender: TObject);
    procedure Southwestern1Click(Sender: TObject);
    procedure Eastwest1Click(Sender: TObject);
    procedure SWNE1Click(Sender: TObject);
    procedure Replaceonlyvalidvaluesfromsecondgrid1Click(Sender: TObject);
    procedure Radianstodegrees1Click(Sender: TObject);
    procedure Tangentradians1Click(Sender: TObject);
    procedure Tangentdegrees1Click(Sender: TObject);
    procedure Locationsonly1Click(Sender: TObject);
    procedure Values1Click(Sender: TObject);
    procedure oPrimeMeridian1Click(Sender: TObject);
    procedure oInternationalDateLine1Click(Sender: TObject);
    procedure Referencegridinfile1Click(Sender: TObject);
    procedure Opengrid2Click(Sender: TObject);
    procedure Gridinfile1Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    //procedure renchslabdips1Click(Sender: TObject);
    //procedure Slabdepths1Click(Sender: TObject);
    procedure Maxmeanmingridsfrompointdata1Click(Sender: TObject);
    procedure SpeedButton8Click(Sender: TObject);
    procedure NLCD19921Click(Sender: TObject);
    procedure NLCD20011Click(Sender: TObject);
    procedure Cloudmask1Click(Sender: TObject);
    procedure Stats1Click(Sender: TObject);
    procedure Cirrusmask1Click(Sender: TObject);
    procedure Watermask1Click(Sender: TObject);
    procedure Snowicemaksk1Click(Sender: TObject);
    procedure Eastern1Click(Sender: TObject);
    procedure LVIS1Click(Sender: TObject);
    procedure ableofcontents1Click(Sender: TObject);
    procedure ableofcontents2Click(Sender: TObject);
    procedure Loadsummarymultipleclassifications1Click(Sender: TObject);
    procedure Lookatpoints1Click(Sender: TObject);
    procedure Ensembleclassification1Click(Sender: TObject);
    procedure Removeifmissingneighbors1Click(Sender: TObject);
    procedure Removetoofewsimilarneighbors1Click(Sender: TObject);
    procedure Sunabovethehorizon2Click(Sender: TObject);
    procedure Magneticnorthlines1Click(Sender: TObject);
    procedure Northarrow1Click(Sender: TObject);
    procedure Singlecontour1Click(Sender: TObject);
    procedure Dividezvalues1Click(Sender: TObject);
    procedure Direction01361Click(Sender: TObject);
    procedure ArcGIScodes11281Click(Sender: TObject);
    procedure auDEMcode181Click(Sender: TObject);
    //procedure Timemaps1Click(Sender: TObject);
    procedure Randomsamplingpoints1Click(Sender: TObject);
    procedure PointcloudtoGoogleEarth1Click(Sender: TObject);
    procedure PointcloudtoOpenGL1Click(Sender: TObject);
    procedure CreatefilterforUTMsquare1Click(Sender: TObject);
    procedure Coastlines1Click(Sender: TObject);
    //procedure Areahistogram1Click(Sender: TObject);
    procedure ospecifiedlongitude1Click(Sender: TObject);
    procedure Periods1Click(Sender: TObject);
    procedure Epochs1Click(Sender: TObject);
    procedure Coloredbynumber1Click(Sender: TObject);
    procedure OPEXPoseidonimport1Click(Sender: TObject);
    procedure Rivers2Click(Sender: TObject);
    procedure Slopelogtangent1Click(Sender: TObject);
    procedure Slopesqrtsin1Click(Sender: TObject);
    procedure Slopplntangent1Click(Sender: TObject);
    procedure Arctangent1Click(Sender: TObject);
    procedure Magneticanomalypicks1Click(Sender: TObject);
    procedure NDSIsnow1Click(Sender: TObject);
    procedure Normalizeddifferencepickbands1Click(Sender: TObject);
    procedure Histogram3Click(Sender: TObject);
    procedure Gridcorrelations1Click(Sender: TObject);
    procedure Outlineothermaps1Click(Sender: TObject);
    procedure Reliefavgelevstdelev1Click(Sender: TObject);
    procedure Both1Click(Sender: TObject);
    procedure Percentilestimeseries1Click(Sender: TObject);
    procedure PercentilsofNDVItimeseries1Click(Sender: TObject);
    procedure Pointtimeseries1Click(Sender: TObject);
    procedure NDVIofperenctilesmonthly1Click(Sender: TObject);
    procedure PercentilesofNDVImonthly1Click(Sender: TObject);
    procedure Keypercentiles1Click(Sender: TObject);
    procedure Quickclassfication1Click(Sender: TObject);
    procedure Surveytracklines1Click(Sender: TObject);
    procedure Createsurveylines1Click(Sender: TObject);
    //procedure Copyregistrationfiles1Click(Sender: TObject);
    procedure Algorithms1Click(Sender: TObject);
    procedure Allslopeaspect1Click(Sender: TObject);
    procedure Popuplegends1Click(Sender: TObject);
    procedure DrapecurrentmaptoOpenGL1Click(Sender: TObject);
    procedure Validpointssinglecategory1Click(Sender: TObject);
    procedure WMSOpactiy1Click(Sender: TObject);
    procedure Reliefbyregionsize1Click(Sender: TObject);
    procedure Grainbyregionsize2Click(Sender: TObject);
    procedure Openness1Click(Sender: TObject);
    procedure Opennessoptions1Click(Sender: TObject);
    procedure LoadOSMoverlay1Click(Sender: TObject);
    procedure Creategrid1Click(Sender: TObject);
    procedure Createdatabase2Click(Sender: TObject);
    procedure Loaddatabase1Click(Sender: TObject);
    procedure BitmapandXYZBfile1Click(Sender: TObject);
    procedure Quadtickpoints1Click(Sender: TObject);
    procedure Verticalswipecompare1Click(Sender: TObject);
    procedure TestMD1Click(Sender: TObject);
    procedure Clearsecondgrid1Click(Sender: TObject);
    procedure Blankmapcolor1Click(Sender: TObject);
    procedure Pointsabove1Click(Sender: TObject);
    procedure Pointsbelow1Click(Sender: TObject);
    procedure ID2Click(Sender: TObject);
    procedure Seismicviewing1Click(Sender: TObject);
    procedure All2Click(Sender: TObject);
    procedure VISandNIRsurfacebands1Click(Sender: TObject);
    procedure N2bandscattergram1Click(Sender: TObject);
    procedure Pickfilter1Click(Sender: TObject);
    procedure Basicstatistics1Click(Sender: TObject);
    procedure ZigDistButtonClick(Sender: TObject);
    procedure Endlengthmeasurement1Click(Sender: TObject);
    procedure Trackpointfile1Click(Sender: TObject);
    procedure Endtrack1Click(Sender: TObject);
    procedure ConvertUKOSDEMtoUTM1Click(Sender: TObject);
    procedure oGeographic1Click(Sender: TObject);
    //procedure Zio1Click(Sender: TObject);
    procedure Offcurrentmap1Click(Sender: TObject);
    procedure PLSSlocation1Click(Sender: TObject);
    procedure CompareDNconversions1Click(Sender: TObject);
    //procedure Nightlights20161Click(Sender: TObject);
    procedure Concatenateimages1Click(Sender: TObject);
    procedure Kmeansclustering1Click(Sender: TObject);
    procedure Segmentation1Click(Sender: TObject);
    procedure LCCstandardparallels1Click(Sender: TObject);
    //procedure USSPCS1Click(Sender: TObject);
    //procedure MGRSUSNG6x8zones1Click(Sender: TObject);
    //procedure UTM100Kzones1Click(Sender: TObject);
    procedure Maximizeforscreen1Click(Sender: TObject);
    procedure Streammodetolerance1Click(Sender: TObject);
    procedure MergemultipleCSVTXTfiles1Click(Sender: TObject);
   // procedure GDALwarpsubset1Click(Sender: TObject);
   // procedure Bandcorrelation1Click(Sender: TObject);
    //procedure Falsecolorpansharpen1Click(Sender: TObject);
    //procedure auDEMtools1Click(Sender: TObject);
    procedure RVTgridcreation1Click(Sender: TObject);
    procedure Aspectdifference1Click(Sender: TObject);
    procedure CurrentsubsetGeotiff1Click(Sender: TObject);
    procedure Whiteboxfillholes1Click(Sender: TObject);
    procedure Gaussianpyramiddownsample1Click(Sender: TObject);
    procedure All11scale1Click(Sender: TObject);
    //procedure OpenGLdrapeonanotherDEM1Click(Sender: TObject);
    procedure Reinterpolatealltosameresolution1Click(Sender: TObject);
    procedure Allsamepixelsizeasthismap1Click(Sender: TObject);
    procedure Allsamedisplay1Click(Sender: TObject);
    procedure Monthlygraphatpoint1Click(Sender: TObject);
    procedure Multiplyallgridsbyconstantsandresave1Click(Sender: TObject);
    procedure Evapotranspirationprecipitationwaterbudget1Click(Sender: TObject);
    procedure Close2Click(Sender: TObject);
    procedure AddRGBwindows1Click(Sender: TObject);
    procedure Saveallmapsasimage1Click(Sender: TObject);
    procedure Quickrotatemap1Click(Sender: TObject);
    procedure Set0tomissingandresave1Click(Sender: TObject);
    procedure Koppenclimograph1Click(Sender: TObject);
    procedure Hurricanes1Click(Sender: TObject);
    procedure UStornadoes1Click(Sender: TObject);
    procedure Slopemm1Click(Sender: TObject);
    procedure Annualsunrisesunset2Click(Sender: TObject);
    procedure CombinedMGRSpolygons1Click(Sender: TObject);
    procedure All3Click(Sender: TObject);
    procedure oday1Click(Sender: TObject);
    procedure Equinoxsolstice1Click(Sender: TObject);
    procedure Pointcoordinates2Click(Sender: TObject);
    procedure Physicalgeographylabs1Click(Sender: TObject);
    procedure Fromllokuptable1Click(Sender: TObject);
    procedure Monthlytemperatureranges1Click(Sender: TObject);
    procedure Allmultigridbasicstatistics1Click(Sender: TObject);
    procedure Koppenclimographfromclimategrids1Click(Sender: TObject);
    procedure WGS84elllipsoidtoEGM200081Click(Sender: TObject);
    procedure EGM2008toWGS84ellipsoid1Click(Sender: TObject);
    procedure MICRODEMGeotiffinfo1Click(Sender: TObject);
    procedure Monthlyclimatologies1Click(Sender: TObject);
    procedure Close1Click(Sender: TObject);
    procedure CCAP1Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure ZoomBitBtnClick(Sender: TObject);
    procedure Geomorphometryanalysis1Click(Sender: TObject);
    procedure Geology3Click(Sender: TObject);
    procedure Pointslopebyregionsize1Click(Sender: TObject);
    procedure Sedimentthicknessversuscrustalage1Click(Sender: TObject);
    procedure Openleastcostpathgrids1Click(Sender: TObject);
    procedure issot1Click(Sender: TObject);
    procedure Setspecifiedvaluetomissingandresave1Click(Sender: TObject);
    procedure Keylatitudes2Click(Sender: TObject);
    procedure LAScurrentmaparea1Click(Sender: TObject);
    procedure Averagebylatitude1Click(Sender: TObject);
    procedure Pointcloudtodatabase1Click(Sender: TObject);
    procedure Oceancurrents1Click(Sender: TObject);
    procedure Zoomin3Click(Sender: TObject);
    procedure Zoomout3Click(Sender: TObject);
    procedure Plateoutlines1Click(Sender: TObject);
    procedure DEMgridhistogram1Click(Sender: TObject);
    procedure errainsunblocking1Click(Sender: TObject);
    procedure N0codes1Click(Sender: TObject);
    procedure Vectoraverage1Click(Sender: TObject);
    procedure Aspectrosediagram1Click(Sender: TObject);
    procedure N3x3neighborhood1Click(Sender: TObject);
    procedure N5x5neighborhood1Click(Sender: TObject);
    procedure N3x3neighborhood2Click(Sender: TObject);
    procedure N5x5neighborhhhod1Click(Sender: TObject);
    procedure Fillwithmodefilter1Click(Sender: TObject);
    procedure Modefilter1Click(Sender: TObject);
    procedure LASclassificationlegend1Click(Sender: TObject);
    procedure Terrainorganizationmaps1Click(Sender: TObject);
    procedure LAScategoriestoshow1Click(Sender: TObject);
    procedure RGBgridfillholes1Click(Sender: TObject);
    procedure Roundtobylerange1Click(Sender: TObject);
    procedure LoadCHMgrid1Click(Sender: TObject);
    procedure Loadchangegrid1Click(Sender: TObject);
    procedure Gridgraticule1Click(Sender: TObject);
    procedure Gridgraticule2Click(Sender: TObject);
    procedure Heatmap1Click(Sender: TObject);
    procedure Shapefileaftersubsettomatchmapextent1Click(Sender: TObject);
    procedure Simpleelevationcheck1Click(Sender: TObject);
    procedure Differencemapsallotheropengrids1Click(Sender: TObject);
    procedure Globaltectonicsmap1Click(Sender: TObject);
    procedure Restorerangecircles1Click(Sender: TObject);
    procedure Annualsolarelevation1Click(Sender: TObject);
    procedure NDBIbuiltup1Click(Sender: TObject);
    //procedure Definedcategories1Click(Sender: TObject);
    procedure Fixedpalettestats1Click(Sender: TObject);
    procedure Allgraphs1Click(Sender: TObject);
    procedure ClimographandETO1Click(Sender: TObject);
    procedure Solarparameters1Click(Sender: TObject);
    procedure Imagery1Click(Sender: TObject);
    procedure Landsatmetadata1Click(Sender: TObject);
    procedure Sentinel2metadata1Click(Sender: TObject);
    procedure OArelectancewithsunpositioncorrection1Click(Sender: TObject);
    procedure OAreflectance1Click(Sender: TObject);
    procedure Surfaceradiance1Click(Sender: TObject);
    procedure Brightnesstemperature1Click(Sender: TObject);
    procedure OAradiance1Click(Sender: TObject);
    procedure SatelliteDNsatpoint2Click(Sender: TObject);
    procedure Sunposition1Click(Sender: TObject);
    procedure N3DviewwithtwoDEMs1Click(Sender: TObject);
    procedure LSTfromemissivity1Click(Sender: TObject);
    procedure Fixedpalettecategories1Click(Sender: TObject);
    procedure Createmask1Click(Sender: TObject);
    procedure Sentinel2bandreflectance1Click(Sender: TObject);
    procedure Landsatmetadata2Click(Sender: TObject);
    procedure Sentinel2metadata2Click(Sender: TObject);
    procedure Differenceelevationslopeaspectmaps1Click(Sender: TObject);
    procedure Checkprojectionresults1Click(Sender: TObject);
    procedure ScrollBox1Click(Sender: TObject);
    procedure Whiteboxslopemape1Click(Sender: TObject);
    procedure Pointslopealgorithms1Click(Sender: TObject);
    procedure Slopedegreestopercent1Click(Sender: TObject);
    procedure NASADEMtomatchthismap1Click(Sender: TObject);
    procedure DEMsfrommaplibrary1Click(Sender: TObject);
    procedure ASTERGDEMtomatchthismap1Click(Sender: TObject);
    procedure CenterpointsofallDBs1Click(Sender: TObject);
    procedure Bigimagewithallmaps1Click(Sender: TObject);
    procedure Monthlyinsolation1Click(Sender: TObject);
    procedure Gridspacinganddeclination1Click(Sender: TObject);
    procedure MatchWKTprojection1Click(Sender: TObject);
    procedure Slopeerrorestimateexperimental1Click(Sender: TObject);
    procedure Allsamemapsize1Click(Sender: TObject);
    procedure Exportvaluesforallopengrids1Click(Sender: TObject);
    procedure Slopeandcomponents1Click(Sender: TObject);
    procedure Scribbleonmap1Click(Sender: TObject);
    procedure Absolutevalue1Click(Sender: TObject);
    procedure RMSE1Click(Sender: TObject);
    procedure Loadlandcover1Click(Sender: TObject);
    procedure Averagetopographicprofile1Click(Sender: TObject);
    procedure DEMsfrommaplibrarymaparea1Click(Sender: TObject);
    procedure Maskallothergridstomatchthisone1Click(Sender: TObject);
    procedure ResampleDEMgridbyaveraging1Click(Sender: TObject);
    procedure Downsampling2Click(Sender: TObject);
    procedure hinaveragingcomparison1Click(Sender: TObject);
    procedure Annualsolarillumination1Click(Sender: TObject);
    procedure Sunandsatellitevisibilityandblocking1Click(Sender: TObject);
    procedure Geoid1Click(Sender: TObject);
    procedure Mapdirectsolarillumination1Click(Sender: TObject);
    procedure Exportmaplibrary1Click(Sender: TObject);
    procedure Pixelsize1Click(Sender: TObject);
    procedure EGM1996toEGM20081Click(Sender: TObject);
    procedure Horizontalshift1Click(Sender: TObject);
    procedure HorizontalDEMshifts2Click(Sender: TObject);
    procedure Allopengrids1Click(Sender: TObject);
    procedure Createnewgridforthismaparea1Click(Sender: TObject);
    procedure Flickermovie1Click(Sender: TObject);
    procedure racks1Click(Sender: TObject);
    procedure GNDVIvegetation1Click(Sender: TObject);
    procedure WhiteBoxmultiscaleroughness1Click(Sender: TObject);
    procedure RoughnessfromSSO1Click(Sender: TObject);
    procedure Moonriseset1Click(Sender: TObject);
    procedure GRASSslopeHorn1Click(Sender: TObject);
    procedure GRASSvectorruggedness1Click(Sender: TObject);
    procedure Missingdata1Click(Sender: TObject);
    procedure Pointparameters1Click(Sender: TObject);
    procedure Pointzvaluesallgrids1Click(Sender: TObject);
    procedure Curvaturecategories1Click(Sender: TObject);
    procedure IwashishiandPikeclassification1Click(Sender: TObject);
    procedure Grassprofilecurvature1Click(Sender: TObject);
    procedure Grasstangentialcurvature1Click(Sender: TObject);
    procedure Allcurvatures1Click(Sender: TObject);
    procedure Differencebetweentwogrids2Click(Sender: TObject);
    procedure Scatterplotoftwogrids2Click(Sender: TObject);
    procedure Gridcorrelations2Click(Sender: TObject);
    procedure Gammagrids1Click(Sender: TObject);
    procedure ACOLITEprocessing1Click(Sender: TObject);
    procedure GRASSTRI1Click(Sender: TObject);
    procedure Landsurfacetemperature1Click(Sender: TObject);
    procedure GRASSaspect1Click(Sender: TObject);
    procedure Slopeerrorestimatorexperimental1Click(Sender: TObject);
    procedure Aspectslopemerge1Click(Sender: TObject);
    procedure Aspectoptions1Click(Sender: TObject);
    procedure Outlinemaparea1Click(Sender: TObject);
    procedure DEMIX10Ktile1Click(Sender: TObject);
    procedure Averagetonewprojection1Click(Sender: TObject);
    procedure Floodingsealevelrise1Click(Sender: TObject);
    procedure WhiteboxPennockClassification1Click(Sender: TObject);
    procedure Fatfingers1Click(Sender: TObject);
    procedure Recenter2Click(Sender: TObject);
    procedure Generaloptions1Click(Sender: TObject);
    procedure Reinterpolatepickprojection1Click(Sender: TObject);
    procedure Falsecolorpansharpen1Click(Sender: TObject);
    procedure Annualsunrisesunset1Click(Sender: TObject);
    procedure AddanopenDEM1Click(Sender: TObject);
    procedure SubtractanopenDEM1Click(Sender: TObject);
    procedure Moviewithallmaps1Click(Sender: TObject);
    procedure DEMIX1secresamplebyaveraging1Click(Sender: TObject);
    procedure Samehorizontaldatum1Click(Sender: TObject);
    procedure Samehorizontaldatum2Click(Sender: TObject);
    procedure OpenGLwithallmaps1Click(Sender: TObject);
    procedure Matchothermaps2Click(Sender: TObject);
    procedure Matchothermaps3Click(Sender: TObject);
    procedure WhiteboxGeomorphons1Click(Sender: TObject);
    procedure Profile1Click(Sender: TObject);
    procedure angential1Click(Sender: TObject);
    procedure Minimal1Click(Sender: TObject);
    procedure Maximal1Click(Sender: TObject);
    procedure Mean1Click(Sender: TObject);
    procedure Gaussian1Click(Sender: TObject);
    procedure Maskothergridstomatchthisone1Click(Sender: TObject);
    procedure GRASSTPI1Click(Sender: TObject);
    procedure GetGRASSextensions1Click(Sender: TObject);
    procedure StdDevinregion2Click(Sender: TObject);
    procedure Roundtobyterangepercentiles1Click(Sender: TObject);
    procedure N3x3region1Click(Sender: TObject);
    procedure N5x5region1Click(Sender: TObject);
    procedure StdDevinregion1Click(Sender: TObject);
    procedure DetrendDEMgrid1Click(Sender: TObject);
    procedure WhiteboxTRI1Click(Sender: TObject);
    procedure N51Click(Sender: TObject);
    procedure SAGATPImap1Click(Sender: TObject);
    procedure SaveasPixelispoint1Click(Sender: TObject);
    procedure SaveasPixelisarea1Click(Sender: TObject);
    procedure N7x7region1Click(Sender: TObject);
    procedure N7x7region2Click(Sender: TObject);
    procedure GDALcontourshapefile1Click(Sender: TObject);
    //procedure DEMIXevaluatehalfsecondgrids1Click(Sender: TObject);
    procedure MICRODEMupsamplebilinearbicubic1Click(Sender: TObject);
    procedure BestglobalDEM1Click(Sender: TObject);
    procedure DEMIX1secresamplewithGDAL1Click(Sender: TObject);
    procedure Pixelextentandhighresaverage1Click(Sender: TObject);
    procedure oday2Click(Sender: TObject);
    procedure oday3Click(Sender: TObject);
    procedure N3Drotatingglobe1Click(Sender: TObject);
    procedure SAGAVRMmapvectorruggedness1Click(Sender: TObject);
    procedure Whitebox1Click(Sender: TObject);
    procedure WhiteboxCircularVarianceOfAspect1Click(Sender: TObject);
    procedure INForNAN1Click(Sender: TObject);
    procedure CreategeoatlasKMZ1Click(Sender: TObject);
    procedure Filtersizes1Click(Sender: TObject);
    procedure Mapboundingbox1Click(Sender: TObject);
    procedure DEMIX10ktiles1Click(Sender: TObject);
    procedure Airballdirtball1Click(Sender: TObject);
    procedure ClipDEMtoregionwithdata1Click(Sender: TObject);
    procedure Elevationpercentiles1Click(Sender: TObject);
    procedure Differencemap1Click(Sender: TObject);
    procedure Dataheader2Click(Sender: TObject);
    procedure NumericgridwithVATtobytegridwithcodes1Click(Sender: TObject);
    procedure Geomorphometrybycategories1Click(Sender: TObject);
    procedure Allvalidpixels1Click(Sender: TObject);
    procedure Openbandforrasteranalysis1Click(Sender: TObject);
    procedure Changemap2Click(Sender: TObject);
    procedure PickseriesandloadDEMsfromlibrary1Click(Sender: TObject);
    procedure ComapreUTMvsgeographic1Click(Sender: TObject);
    procedure MatchThiscoverageareaandsamepixelsize1Click(Sender: TObject);
    procedure NDVI3Click(Sender: TObject);
    procedure NBR1Click(Sender: TObject);
    procedure Elevationdifference1Click(Sender: TObject);
    procedure Genericdifference1Click(Sender: TObject);
    procedure Entiregrid1Click(Sender: TObject);
    procedure CurrentMapArea2Click(Sender: TObject);
    procedure COPALOSbestlocations1Click(Sender: TObject);
    procedure COPALOScategories1Click(Sender: TObject);
    procedure COPALOS9categories1Click(Sender: TObject);
    procedure Normalizeeastwest1Click(Sender: TObject);
    procedure Normalizenorthsouth1Click(Sender: TObject);
    procedure Nonormalization1Click(Sender: TObject);
    procedure Alolthreenormalizations1Click(Sender: TObject);
    procedure InsertpostingsfromreferenceDEM1Click(Sender: TObject);
    procedure Normalizeto30m1Click(Sender: TObject);
    procedure RIK1Click(Sender: TObject);
    procedure RICK1Click(Sender: TObject);
    procedure Winwcontestmaps1Click(Sender: TObject);
    procedure Likelymissingdatacodes1Click(Sender: TObject);
    procedure Allmissingtosinglevaluevalidsettomissing1Click(Sender: TObject);
    procedure Interactiveadjusment1Click(Sender: TObject);
    procedure OpenGLdrapeonanotherDEM1Click(Sender: TObject);
    procedure COPandALOS1Click(Sender: TObject);
    procedure All61Click(Sender: TObject);
    procedure Geoidgrids1Click(Sender: TObject);
    procedure SummaryallopenDEMsGrids1Click(Sender: TObject);
    procedure Slopeandroughness1Click(Sender: TObject);
    procedure MergeanotherDEMhillshade1Click(Sender: TObject);
    procedure Zoomfullresolution1Click(Sender: TObject);
    procedure EditVATDBFcolorscategorynames1Click(Sender: TObject);
    procedure Putshadingfromthismapunderselectedmaps1Click(Sender: TObject);
    procedure LocaddatumtoEGM20081Click(Sender: TObject);
    procedure NAVD881Click(Sender: TObject);
    procedure EGM2008Click(Sender: TObject);
    procedure Other1Click(Sender: TObject);
    //procedure Datumshiftcomparison1Click(Sender: TObject);
    procedure Shiftfilecomparison1Click(Sender: TObject);
    procedure CSVforVDATUM1Click(Sender: TObject);
    procedure DEMIXhalfsecto2onesec1Click(Sender: TObject);
    procedure Removequickoverlayhillshade1Click(Sender: TObject);
    procedure PerpProfiles1Click(Sender: TObject);
    procedure NearestpeakoneachDEM1Click(Sender: TObject);
    procedure Specifyxyzshifts1Click(Sender: TObject);
    procedure UsingVDATUMoutput1Click(Sender: TObject);
    procedure UsingVDATUM1Click(Sender: TObject);
    procedure WGS84elllipsoid1Click(Sender: TObject);
    procedure Pickmapsforbigimage1Click(Sender: TObject);
    procedure dNBRNBRbeforeandafterfire1Click(Sender: TObject);
    procedure NBR21Click(Sender: TObject);
    procedure N60Click(Sender: TObject);
    procedure Percent1Click(Sender: TObject);
    procedure Rasteraftersubsettomatchthismapextent1Click(Sender: TObject);
    procedure GDALgridsubsettomatchthismap1Click(Sender: TObject);
    procedure PI1Click(Sender: TObject);
    procedure ClipDEMtofullDEMIXtiles1Click(Sender: TObject);
    procedure N62Click(Sender: TObject);
    procedure Roundtointegers1Click(Sender: TObject);
    procedure PicksingleDEMseriesthisarea1Click(Sender: TObject);
    procedure DEMIXrangescales1Click(Sender: TObject);
    procedure SSIM1Click(Sender: TObject);
    procedure SAGAchannelnetwork1Click(Sender: TObject);
    procedure SAGAremovesinksallopenDEMs1Click(Sender: TObject);
    procedure SAGAchannelnetworkallopenDEMs1Click(Sender: TObject);
    procedure SAGAChannelNetworkandBasins1Click(Sender: TObject);
    procedure GDALrasterizehapfiels1Click(Sender: TObject);
    procedure Pureplatecareeprojectiondistorted1Click(Sender: TObject);
    procedure Rasterizedatabases1Click(Sender: TObject);
    procedure Comparechannelnetworks1Click(Sender: TObject);
    procedure Landcover1Click(Sender: TObject);
    procedure SAGAremovesinks1Click(Sender: TObject);
    procedure LC100landcoverwaterbodies1Click(Sender: TObject);
    procedure SAGAedgecontaminationmap1Click(Sender: TObject);
    procedure SAGADrainagebasins1Click(Sender: TObject);
    procedure Whiteboxdrainagebasins1Click(Sender: TObject);
    procedure SAGAwatershedbasinsWangLiu1Click(Sender: TObject);
    procedure SAGAStrahlerordergrid1Click(Sender: TObject);
    procedure SAGAflowaccumulationParallelizable1Click(Sender: TObject);
    procedure Whieboxflowaccumulationlog1Click(Sender: TObject);
    procedure Numbercells1Click(Sender: TObject);
    procedure FD8Lognumbercells1Click(Sender: TObject);
    procedure FD8Lognumbercells2Click(Sender: TObject);
    procedure Whiteboxwetnessindex1Click(Sender: TObject);
    procedure SAGALSfactor1Click(Sender: TObject);
    //procedure RescaleallDEMsforSSIM1Click(Sender: TObject);
 private
    MouseUpLat,MouseUpLong,
    MouseDownLat,MouseDownLong,
    RightClickLat,RightClickLong : float64;
    RespondingToMouseMove,RecMoving : boolean;
    sx,sy       : integer;     //for mouse panning
    procedure WMBroadcastLatLongMessage(var Msg : TMessage); message WM_BroadcastLatLongMessage;

    procedure ArrangeButtons;
    procedure SetClientHeight(DefaultSize : boolean = false);
    procedure SetPanButtons;
    procedure CheckAndDrawNewCorners;
    function GetSecondDEM(MustBeCompatible : boolean = true) : boolean;
    function ValidDifferentDEM(i : integer) : boolean; inline;

    procedure SetMapOverlays;
    procedure MakePanButtonsVisible(ButtonsVisible : boolean);
    function FindDBsOnMap : integer;
    procedure FeatureMigration(Dir : tCompassDirection);

    procedure GetPositionData(var Length,Heading : float64; var Tstr2 : ShortString; DontVerify : boolean = false);
    procedure MapDisplayLocation(var x,y : integer; var Lat,Long : float64; var Elev : float32);
    procedure CutPoints(MinGood,MaxGood : SmallInt);

    procedure RespondToRightMouseButton;
    procedure QuadSizeMap(LastX,LastY : integer);
    procedure ChangePixelIsFormat;

    {$IfDef IncludePeakIslandArea}
         function FindIslandArea(Lat,Long,z : float64; ShowOnMap : boolean) : float64;
    {$EndIf}

    function ExpertDEMVersion: boolean;
    procedure HideUndesiredOptions;
    procedure PointInterpolationReport(Lat,Long : float64);

    procedure Loadvegetationlayer(i: integer; mItem : tMenuItem);
    procedure FillHoles(ModeWanted: tHoleFill);
    procedure DropinthebucketfromGrids;

    procedure PlotNorthArrowLegend(x, y : integer);
    procedure PostPointCoordinates(Lat,Long : float64);

    procedure SetUpTopForm;
    procedure ChangeElevUnits(Units : tElevUnit);

    procedure LoadSecondGrid(which : tSecondGrid);
    function ComputeRMSE : float64;

    procedure AddPointSpectralReflectance(LastX,LastY : integer);
    function OtherMapSameSize(om : tMapForm) : boolean;
    function OtherMapSameCoverage(om : tMapForm) : boolean;


    {$IfDef RecordMapResize}
       procedure DebugMapSize;
    {$EndIf}

    {$IfDef ExGeology}
    {$Else}
       procedure WMBroadcastDrawPlane(var Msg : TMessage); message WM_BroadcastDrawPlane;
    {$EndIf}

    {$IfDef ExDrainage}
    {$Else}
       procedure DrawDownhillVectors;
    {$EndIf}

    {$IfDef Ex3D}
    {$Else}
       procedure ThreeDCheckDblClick(NotSamePoint : boolean);
       procedure DrawPerspective(x,y : integer; TheMode : TPenMode);
    {$EndIf}

   {$IfDef ExAdvancedGIS}
   {$Else}
       procedure CheckAdvancedGISDoubleClick(Lat,Long : float64);
   {$EndIf}


   {$IfDef ExViewshed}
   {$Else}
       procedure SetUpAmbush;
       procedure DrawMultipleIntervisible;
       procedure GetHorizonBlockingOptions;
       procedure CheckViewShedMapDblClick(NotSamePoint : boolean);
       procedure DrawMultipleFanMethods(WeaponsFan : tWeaponsFan);
       procedure CompareFanSensitivityAnalysis(WeaponsFan : tWeaponsFan);
    {$EndIf}

   {$IfDef ExGeology}
   {$Else}
    function CheckGeologyOptions(Lat,Long : float64; LastX,LastY : integer; NotSamePoint : boolean) : boolean;
    procedure UpdateThreePointer;
   {$EndIf}


    {$IfDef ExDrainage}
    {$Else}
       procedure CheckDrainageDblClick;
       procedure FindDrainageAreaContributing(x,y : integer);
    {$EndIf}

    {$IfDef RecordFormResize}
       procedure RecordFormSize(Title : shortString);
    {$EndIf}

 public
    { Public declarations }
     MapDraw    : tMapDraw;
     ShowAMenu,    //allows not showing menus on just this map
     PanButtonsOnMap,
     FormOperational,
     SizingWindow,
     Closable,
     PointCloudBase,
     FullDraw,
     MouseIsDown,
     NoMovingForm,
     BroadcastMapChanges,
     AutoAnaglyphRedraw,
     IgnoreDblClick,
     AllowMapLibraryLoads,
     MapRedrawsAllowed,
     Blending,
     MapSubsetAllowed,
     UseMapForMultipleMapOperations,
     ClosingMapNotAllowed,
     ShowKeyLocation : boolean;
     MapTOCIndex,
     LegendOptionsAvailable : byte;

     KeyLocationLat,KeyLocationLong,
     HighZ,LowZ: float64;
     xDEMg2,yDEMg2,xDEMg1,yDEMg1 : float32;

     NewX1,NewY1,Newx2,NewY2,
     LastBroadcastX,LastBroadcastY,
     RightClickX,RightClickY,LastX,LastY,LastGPSX,LastGPSY,
     FeaturesDB,
     SavedMergeReflectanceDEM,
     ExtremeZDEM      : integer;
     SavedMapImage,
     OverlayOpaqueBMP,MapBaseBMP  : tMyBitmap;
     VariableOpaqueOverlays: boolean;
     DrSymbol   : tFullSymbolDeclaration;

     DEMeditForm : TDEMeditForm;
     SliderDrapeMap,
     ZoomWindow  : tMapForm;

     {$IfDef ExGeology}
     {$Else}
        LineTable : integer;
     {$EndIf}

    function MapInformationString : tStringList;
    procedure SaveMapasGEOTIFFFile(fName : PathStr; Grayscale : boolean);

    function DuplicateMap(CopyOverlays : boolean; DrawIt : boolean = true; Invisible : boolean = false) : tMapForm;
    procedure CopyMap(var DrapeForm : tMapForm; HiddenDrape,CopyOverlays : boolean; Invisible : boolean = false);
    procedure SetUpNewDEMMapWindow(CurDEM : integer; mt : tMapType; MapCaption : ShortString; Selection,DrawIt,UsePC : boolean);
    procedure CreateZoomWindow(UseToVerify : boolean; ContourInterval,DEMGridSize : integer; var xDEMg,yDEMg,xSATg,ySATg : float32; ShowPanButtons : boolean = true; ShowMenu : boolean = true; LabelGridPts : boolean = true);

    function CreateMapAndLegendSideBySide : tMyBitmap;

    function CurrentMapImage : tImage;
    procedure DrawCollaredMap(var TickSize,LabelSize : float64; var Bitmap2 : tMyBitmap);
    procedure EditGridViaColor(emvc : temvc; Color : tColor; LakeZ : float64 = -MaxSmallInt; SureOfColor : boolean = false; ShowResults : boolean = true);

    procedure DoFastMapRedraw;
    procedure DoBaseMapRedraw;
    procedure DoCompleteMapRedraw;
    procedure RestoreFullMap;
    procedure RespondToChangedDEM;
    procedure InsureGrayScaleReflectanceMap;
    procedure CheckThatLegendsAreOnTop;
    procedure SetMapDisplayType(dt : tMapType);
    procedure MakeHeatMap(db : integer);
    procedure RecolorMapWithElevationRange(Min,Max : float32);

    procedure BackToWandering;
    procedure BlowUpTheMap(BlowUp : float64);
    procedure SetMapPixelSize(PixelSize : float64);
    procedure ResizeByPercentage(Percent : integer);
    procedure SubsetAndZoomMap(NewX1,Newy1,NewX2,Newy2 : integer);
    procedure SubsetAndZoomMapFromGeographicBounds(GeoBox : sfBoundBox; Redraw : boolean = true);
    procedure SubsetAndZoomMapFromProjectedBounds(Redraw : boolean = true);
    procedure ClipDEMtoregion(Limits : sfBoundBox);

    procedure OutlineGridOutlines;
    procedure OutlineGeoBox(bb : sfBoundBox; Color : tColor; LineSize : integer);
    procedure OutlineUTMBox(bb : sfBoundBox; Color : tColor; LineSize : integer);
    procedure OutlineMap;

    procedure RedrawMapDefaultsSize;

    function LoadDEMIXtileOutlines(WantBoundBoxGeo : sfBoundBox; AddGridFull : boolean = false; AddTileSize : boolean = false; OpenTable : boolean = true) : integer;
    function DEMIXtilesOnMap(RecordFill : tStringList = Nil) : tStringList;
    procedure ClipDEMtoFullDEMIXTiles(NewName : PathStr = '');

    procedure MoveADBRecord(lat,Long : float64);

    function MakeNLCDLegend(theLabel : shortstring = ''; Stats : tstringlist = nil) : integer;

    function StringListToLoadedDatabase(var Findings : tStringList; fName : PathStr; DisplayNow : boolean = true; RestrictToMapOwner : boolean = false; ShowTable : boolean = true) : integer;
    function LoadDataBaseFile(fName : PathStr; OpenTable : boolean = true; DrawNow : boolean = true; RestrictToMapOwner : boolean = false) : integer;
    function OpenDBonMap(WhatFor : shortstring; DefaultFile : PathStr; DisplayNow : boolean = true; OpenTable : boolean = true; ThisMapOnly : boolean = false;
         ForceColor : tColor = -99; ForceLineWidth : byte = 0; HideFields : ShortString = '') : integer;

    procedure MatchMapToThisOne(var DrapeMap : tMapForm);
    function DrawSlopeMask(Color : tColor; maxSlope : float64; DisplayAndPurge : boolean) : tMyBitmap;
    function NewSatWindow(nsb : tNewSatBand) : integer;

    function DisplayAndPurgeStringListDB(var TheList : tStringList; fname : PathStr; OpenTable : boolean = true) : integer;

    procedure LoadCartoDBoverlay(fName : PathStr);
    function ToolAndPanelHeights : integer;
    procedure OpenGazetterOnMap(fName : PathStr; RedrawNow : boolean = true);

    function CenterMapOnLatLong(Lat,Long : float64; HowZoom : tHowZoom) : boolean;
    procedure MaxUpperLeftDrawMap(XSize : integer = 0; ysize : integer = 0);
    procedure RedrawMapForDataGrid(LeftGrid,TopGrid,RightGrid,BottomGrid : float64; xsize,ysize : integer);
    procedure DrapeMapFromTwoGridCorners(xu1,yu1,xu4,yu4 : float64);
    function SiteContourMapBlowUp(Lat,Long : float64; SiteBlowupMapSize,ZoomSize,ContourInterval : integer; Title : shortString) : tMapForm;
    procedure OverlayContourFromSecondDEM(DEM : integer; z : float64; color : tPlatformcolor);

    procedure AddLatLongPointToStreamProfile(Lat2,Long2 : float64);

    function GeotiffDEMNameOfMap : PathStr;

    procedure FindSlopePoints(Memo1 : tMemo; SlopeLimit : float64; IHSMerge : boolean = false);
    procedure PlotExtremeZValues(Memo1 : tMemo; sl : tstringlist = nil);
    procedure CheckProperTix;
    procedure MakeLastPointFirst;
    procedure DoneWithStreamSelection;
    procedure ShowSubsetOnMap(x1,y1,x2,y2 : integer);
    procedure CutOutCenterOfMap(var BaseBMP : tMyBitmap; x,y : integer);
    procedure DEMGridToImageGrid(xg,yg : float32; var xg1,yg1 : float32);
    procedure VerifyPointOnMap(WhatFor : string; var xg,yg : float32; var xutm,yutm : float64);
    procedure DrawStreamProfile(FinalRun : boolean = false; AskToSave : boolean = true);
    procedure PlotGridPoint(xgrid,ygrid : float64; PlotColor : tPlatformColor);
    procedure ClipLatLongToMap(var Lat,Long : float64);
    procedure AddressGeocode(AskUser,SupplyLatLong : boolean; var Address : shortString; var Lat,Long : float64; ShowResults : boolean = false);
    procedure Savemapwithworldfile(fName : PathStr; BaseMapOnly : boolean = false);
    procedure GridpointsfromsecondDEMAssignAndDraw(SecondDEM : integer);
    procedure MaskFromSecondGrid(SecondGrid: integer; HowMask : tMaskGrid ; ShowResults : boolean = false);
    function SaveDEMtoDBF(fName : PathStr; zName : shortString = 'Z'; ThinFactor : integer = -99;  FilterZ : boolean = false; ReportOut : boolean = false) : PathStr;

    function DrawRidgeMask(Color : tColor; RidgeMask,DisplayAndPurge : boolean) : tMyBitmap;
    function CreateGridToMatchMap(What : tCreateGrid;  OpenMap : boolean = true; Resolution : tDEMprecision = FloatingPointDEM; SpacingX : float64 = -99; SpacingY : float64 = -99; DesiredUTMZone : integer = -99; RasterPixelIs : byte = 1) : integer;
    function MakeTempGrid(OpenMap : boolean = false; GetParameters : boolean = false) : integer;

    function DrawStreamProfileOnMap(db : integer; WhatWanted : tDEMDoingWhat; Color : tPlatformColor; Width : byte) : tMyBitmap;
    procedure DrawLineFromPointDBOnMap(Table : tMyData; Color : tPlatformColor; Width : byte);

    procedure CheckThisPoint(var X,Y : integer; var xDEMg,yDEMg,xSATg,ySATg : float32; NeedCheck : tCheckPoint);
    function Intersection(Lat1,Long1,Az1,Lat2,Long2,Az2 : float64; var Lat,Long : float64) : boolean;

    procedure IHSmergeOntoMap(var Bitmap2 : tMyBitmap; IHSValues : boolean = true; Opacity : byte = 100);
    procedure MergeAnotherDEMreflectance(DEM : integer; MakeSticky : boolean = false; Opacity : byte = 40);

    procedure AddPointToDB(Lat,Long : float64; PointLabel : string = '');
    procedure DrawRecordBeingEditted(Table : tMyData);
    procedure HideToolbar;
    procedure NoScrollbars;

    procedure SaveBitmapForMovie(BaseName: PathStr; var MovieList: tStringList);

    procedure OutlinePerspectiveView(HFOV,Lat1,Long1,SectLen,Azimuth : float64; TheMode : tPenMode);
    procedure StartShapeFile(DEMNowDoing : tDEMDoingWhat);

    function OverlayUp(Layer : tOverlayOrder) : boolean;
    procedure ExportMapToGoogleEarth(MergeAll : boolean);

    procedure SatDNsatPoint(LastX,LastY : integer);
    procedure SatLSTatPoint(LastX,LastY : integer);
    procedure OverlayTissot;


    {$IfDef ExViewshed}
    {$Else}
       procedure DrawHorizon(Lat,Long : float64);
       procedure AddTableWeaponsFan(fName : PathStr);
       procedure EditAWeaponsFan(Sender : tObject);
       procedure AddRangeCirclesAtLocation(Lat,Long : float64);
    {$EndIf}

    {$IfDef ExTiger}
    {$Else}
    procedure TigerRoadMask(RoadProximity : integer; NearRoadsMask : boolean; dbNum : integer = 0; PC : integer = 100);
    procedure RedrawTiger;
    {$EndIf}

    {$IfDef ExTopoGrain}
    {$Else}
       procedure GetFabricAtPoint(x,y : integer);
       procedure FigureGrainByRegionSize(Col,Row : integer);
       procedure FindMostOrganizedRegion;
    {$EndIf}

    {$IfDef ExIndexes}
    {$Else}
      procedure CheckIndexMouseUp(NWLat,NWLong,SELat,SELong : float64);
      procedure SetUpIndexMap(DEMtoUse : integer);
    {$EndIf}

    {$IfDef ExAdvancedGIS}
    {$Else}
      procedure RequiredAntennaMap(Lat,Long : float64);
      procedure SetMultibandToShowOnMap(Band : integer);
      procedure SetRGBMultibandToShowOnMap(What : shortstring; RedBand,GreenBand,BlueBand : integer);
      function GridLimitsForGeomorphOps : tGridLimits;
    {$EndIf}

    {$IfDef Ex3D}
    {$Else}
       procedure StartTheFlyThrough(WhatStarting : tDEMDoingWhat; Replay : boolean = false; SaveName : PathStr = '');
    {$EndIf}

    {$IfDef ExGeology}
    {$Else}
       procedure PlateRotationSetup;
       procedure FigureThreePointProblem(DB : integer);
       procedure BroadCastDrawPlane;
       procedure ThreePointProblems(DB : integer;DoThreePoints : boolean =  true);
       procedure ThreadCheckPlane(GetOrientation: boolean; xg1,yg1 : float64; Dip,Strike,DipDir  : float32);
    {$EndIf}
end;

{$IfDef ExVegDensity}
{$Else}
const
   LoadVegGridString = 'Load vegetation grid';
   LoadVegLayersStr = 'Load vegetation layer';
{$EndIf}

const
   MaxVectorMap = 25;
type
   GetElevColProcType = procedure(x : integer; var z : tElevColPointer);
   ColorFunctType = function(z : integer) : TColor;
const
   LockMaps : boolean = false;
var
   EnsembleClassDB : integer;
   ClosingIsHappening : boolean;
   Sub,SteepestSlopeCol,SteepestSlopeRow,
   WantToEdit,
   StartX,StartY,MapScreenX1,MapScreenY1,
   AD8DEM,
   NumPlateMotionMaps,
   FocalRad,
   DatumShiftDB,
   tCol,tRow,EditPoint : integer;
   DEMDoingWhatBefore,
   DEMDoingNext,
   DEMNowDoing         : tDEMDoingWhat;
   StreamProfileResults,
   FlyRoute    : tStringList;

   ClipBoard_Image_Coords,
   ClipBoard_Coords : boolean;
   ClipBoard_NumPts,
   Clipboard_ImageX,Clipboard_ImageY : integer;
   Clipboard_Lat,ClipBoard_Long : float64;
   ClipBoard_Name : shortstring;
   ClipBoard_Line_Coords : ^tdCoords;

   LocalEdit,
   TargetFlyThrough,
   FirstFan,
   CreateTrainingSet : boolean;
   VolumeRelativeToZ,
   xutm1,yutm1,
   xutm2,yutm2,
   PanSize,PanInc,
   gbLatStart,gbLongStart,
   LastRoamLat,LastRoamLong    : float64;
   ZoomWindowxDEMg1,ZoomWindowyDEMg1,
   xSATg2,ySATg2,
   xSATg1,ySATg1 : float32;
   FocalMechBMP,
   RoadMaskBMP,
   DragBitmap : tMyBitmap;

   VectorMap : array[1..MaxVectorMap] of tMapForm;
   LabelDatumShift : tMarkShift;
   GeosymbolTable : tMyData;

   {$IfDef ExGeography}
   {$Else}
      SunriseOptions : tSunriseOptions;
   {$EndIf}

   {$IfDef ExGeology}
   {$Else}
      BroadCastPlaneData : tBroadCastPlaneData;
   {$EndIf}

procedure BroadcastLatLong(Handle : tHandle; Lat,Long : float64);
procedure ChangeDEMNowDoing(WhatTo : tDEMDoingWhat; WhatNext : tDEMDoingWhat = JustWandering; ThisCapt : shortstring = '');
function SetUpVectorMap(DrawIt,WorldOutline : boolean; WantProjection : tProjectType = undefinedProj; ProjName : PathStr = '') : integer;
function SetUpANewVectorMap(i : integer; DrawIt,WorldOutline : boolean; WantProjection : tProjectType = undefinedProj; ProjName : PathStr = '') : tMapForm;

function MakeRequiredAntennaMap(ProgTitle: shortString; CurDEM : integer;  W_Lat,W_Long : float64; ObserverTotalElevation : float64 = 0; MaxRange : float64 = 75000;
     DrawMap : boolean = true; StartAngle : float64 = 0; EndAngle : float64 = 360) : integer;

function PolygonDigitizing(DEMNowDoing :  tDEMDoingWhat) : boolean;
function PolyLineDigitizing(DEMNowDoing :  tDEMDoingWhat) : boolean;
function ShapeFileDigitizingUnderway(DEMNowDoing :  tDEMDoingWhat) : boolean;
procedure UpdateMenusForAllMaps;

function LoadBlankVectorMapAndOverlay(ItsTiger,ItsGazetteer : boolean; fName : Pathstr = '') : integer;

{$IfDef ExGeology}
{$Else}
   procedure GetSpreadingRateAndDirection(Lat,Long : float64; var v,Azimuth : float64);
{$EndIf}


{$IfDef ExSat}
   const
      NumSatImageOpen = 0;
{$Else}
   procedure CreateNewSatSubsetWindow(inSatView : tSatView; var ResultMap : tMapForm; CurSat : integer; mt : tMapType; MapCaption : ShortString; Roamable,Selection,ReallyDraw : boolean; UseDEM : integer = 0);
   procedure CreateNewSatWindow(inSatView : tSatView; var ResultMap : tMapForm; CurSat : integer; mt : tMapType; MapCaption : ShortString; Selection : boolean; UseDEM : integer = 0);
{$EndIf}

function CreateDEMIXTileShapefile(WantBoundBoxGeo : sfBoundBox; AddGridFull : boolean = false; AddTileSize : boolean = false) : shortstring;
function DEMIXtileFill(DEM : integer; AreaBox : sfBoundBox; OpenTable : boolean = true) : integer;
function DEMIXtileBoundingBox(tName : shortString; PixelIsAreaSafe : boolean = false) : sfBoundBox;
procedure DEMIXtileCentroid(tName : shortString; var Lat,Long : float32);
function LoadDEMIXtileOutlinesNoMap(WantBoundBoxGeo : sfBoundBox; AddGridFull : boolean = false; AddTileSize : boolean = false; OpenTable : boolean = true) : integer;
function DEMIXtilesOnDEM(DEM : integer; RecordFill : tStringList = Nil) : tStringList;


procedure CreateDEMSelectionMap(DEM : integer; DrawIt : boolean = true; usePC : boolean = true; inMapType : tMapType = mtElevRainbow);
function CreateANewDEMMapWindow(CurDEM : integer; DrawIt : boolean; mt : tMapType; MapCaption : ShortString) : tMapForm;
procedure MatchAnotherDEMMap(DEM,CurDEM : integer);

procedure NakedMapOptions;
function NumOpenMaps : integer;

procedure SetMapsForBigBitmaps(Setting : boolean);
procedure Bigimagewithallmaps(NumCols : integer = 3; FileName : PathStr = ''; MapsToUse : tStringList = Nil);


implementation

{$R *.DFM}

uses
   Nevadia_Main,

   {$IfDef ExDrainage}
   {$Else}
      Basin_flooding,
      drainage_opts,
   {$EndIf}

   {$IfDef RegisterPhoto}   //unclear if all the code for this is still available, and if it would run
      Register_Photo,
   {$EndIf}

   {$IfDef ExGetOptions}
   {$Else}
      DEMOptions,
   {$EndIf}

   {$IfDef ExCartography}
   {$Else}
      tissot,
      UK_OS_Converter,
      cart_movie_options,
   {$EndIf}

   {$IfDef ExImages}
   {$Else}
      DEM_Legend,
   {$EndIf}

   {$IfDef ExExoticMaps}
   {$Else}
      DRG_Anaglyph,
   {$EndIf}

   {$IfDef ExPLSS}
   {$Else}
      DEM_PLSS_op,
      DEM_PLSS,
   {$EndIf}

   {$IfDef ExSidescan}
   {$Else}
      SideImg,
      chirpopt,
      ChirpGrf,
   {$EndIf}

   {$IfDef ExTrackSat}
   {$Else}
      trackstarmain,
   {$EndIf}

   {$IfDef ExMilicons}
   {$Else}
      dem_milicon,
   {$EndIf}

   {$IfDef ExDigitize}
   {$Else}
      DEMXYZdisplay,
   {$EndIf}

   {$IfDef ExGeography}
   {$Else}
      KoppenGr,
      Sun_Position,
      get_sunrise,
      moon_montenbruk_pfleger,
   {$EndIf}

   {$IfDef ExMag}
   {$Else}
      DEMMagVar,
   {$EndIf}

   {$IfDef ExAltimeter}
   {$Else}
      AltCommR,
   {$EndIf}

   {$IfDef ExEXIF}
   {$Else}
      JpegDumpForm,
   {$EndIf}

   {$IfDef ExSat}
   {$Else}
      DEMSatMerge,
      DEM_NLCD,
      demsatcontrast,
      DEMEROS,
      //MrSidImagery,
      rgb_colors_three_params,
   {$EndIf}

   {$IfDef ExAdvancedSats}
   {$Else}
      image_erode_dilate,
   {$EndIf}


   {$IfDef ExAdvanced3D}
   {$Else}
      Stereo_Viewer,
      Map_Splitter,
   {$EndIf}

   {$IfDef ExPers}
   {$Else}
      DEMPersW,
      DEM_3D_view,
      dempanorama,
      DEMPerOp,
   {$EndIf}

   {$IfDef ExFly}
   {$Else}
      demflycontrols,
   {$EndIf}

   {$IfDef ExTIGER}
   {$Else}
      DEMTiger,
      DEMTigerOps,
   {$EndIf}

   {$IfDef ExVectorOverlay}
   {$Else}
      PickFillPattern,
      DEMTerrC,
      DEMSlpEd,
   {$EndIf}

   {$IfDef ExGIS}
   {$Else}
      demdatabase,
      DataBaseCreate,
      DataBaseAddRec,
   {$EndIf}

   {$IfDef ExGazetteer}
   {$Else}
      DEM_Gaz_opts,
   {$EndIf}

   {$IfDef ExRedistrict}
   {$Else}
      demredistrict,
   {$EndIf}

   {$IfDef ExPNG}
   {$Else}
      PNGImage,
   {$EndIf}

   {$IfDef ExGeoStats}
   {$Else}
      Make_grid,
      DEMStat,
      Pick_geostats,
      Check_8_Dirs,
      DEM_optimal_lag,
      Feature_Migration,
      geomorph_point_class, curv_tend_map,
      ant_hts_ops,
      Sup_Class_Aux_Grids,
      DEMTrendOpt,
      geomorph_region_size_graph,
   {$EndIf}

   {$IfDef ExGridOverMap}
   {$Else}
      Grid_over_map,
   {$EndIf}

   {$IfDef ExGeology}
   {$Else}
      edit_dip,
      Plate_Rotate,
      DEMDips,
      Three_Point_Problem,
      Beach_Ball_Options,
      sc_ColMain,
   {$EndIf}

   Petmar_geology,

   {$IfDef ExTopoGrain}
   {$Else}
      demfabricregion,
      DEMssocalc,
   {$EndIf}

   {$IfDef ExViewshed}
   {$Else}
      DEM_Fan_Compare,
      DEMLOSop,
      DEMWeapn,
      Fan_sens_opts,
   {$EndIf}

   {$IfDef ExMarginalia}
   {$Else}
      DEMMarginalia,
   {$EndIf}


   {$IfDef RadarAltimeter}
   AltMain,
   {$EndIf}


   {$IfDef ExWaveRefraction}
   {$Else}
      //refraction_model,
   {$EndIf}


   {$IfDef ExFresnel}
   {$Else}
      fresnel_block_form,
   {$EndIf}

   {$IfDef MICRODEM}
      Least_cost_path,
   {$EndIf}

   {$IfDef ExPointCloud}
   {$Else}
      point_cloud_options,
      Las_Lidar,
      LVIS,LVIS_Form,
   {$EndIf}

   {$IfDef ExDriftModel}
   {$Else}
      drift_model,
   {$EndIf}

   {$IfDef ExImageClassify}
   {$Else}
      sup_class,
   {$EndIf}

   {$IfDef NoClustering}
   {$Else}
      clusterOptions, MVClusterClientDataSet,
   {$EndIf}

   {$IfDef ExSlicer3D}
   {$Else}
      slicer_3d,
   {$EndIf}

   {$IfDef ExKML}
   {$Else}
      kml_creator,
   {$EndIf}

   {$IfDef ExAdvancedGIS}
   {$Else}
      Mask_multiple,
      Map_algebra,
      Map_Masking,
      Raster_2_vector, survey_lines,
   {$EndIf}

   {$IfDef ExAdvancedSats}
   {$Else}
      MultiGrid,
   {$EndIf}

   {$IfDef ExVegDensity}
   {$Else}
      Veg_density,
   {$EndIf}

   {$IfDef NoExternalPrograms}
   {$Else}
      MD_use_tools,
   {$EndIf}

   DEMIX_control,
   DEMcoord,
   DEMIX_cop_alos,
   pick_several_dems ,
   Monitor_Change_Form,
   DEM_sat_Header,
   ufrmMain,
   aspect_colors,
   DEMLOSW, DEMLOS_Draw,
   BaseGraf,
   PetImage_form,
   db_display_options,
   DEMESRIShapeFile,
   US_Properties,
   DEMRange,
   Grayscale_shift,
   //SystemCriticalU,
   DEM_computations,
   DEM_Manager,
   PETGraphColors,
   Make_Tables,
   PETdbUtils,
   Grid_Postings_Form,
   GetLatLn,
   Thread_timers,
   DEMElevOps,
   GEOTIFF,
   DEMPrintPreview,
   DEM_Map_scale,
   DEMGrPik,
   DEMConOp,
   DEMSlopeOpts,
   Map_Options,
   DEMRefOp,
   DEMDef_routines,
   toggle_db_use, usoutlines, pick_limits, map_overlays,
   Get_PLSS, map_route,
   csv_export,
   gdal_tools,
   New_petmar_movie,
   Elev_color_range,
   GIS_Scaled_symbols, demstringgrid, fat_fingers;

var
   EraserSize  : integer;
   PersViewDepth,
   CumLength,SectLen : float64;
   xDEMg3,yDEMg3 : float32;
   MouseDragging,
   CreateHiddenMap : boolean;
   OldRad,LastClickX,LastClickY : integer;
   PointSatReflectanceGraph,
   VegGraph,StreamGraph : tThisBaseGraph;
   SpectLibGraph : tThisBaseGraph;
   MovieList : tStringList;

{$I demmapf_gdal.inc}
{$I demmapf_geostats.inc}
{$I demmapf_veg_obstacles.inc}
{$I demmapf_set_up_new_windows.inc}
{$I demmapf_resize_map.inc}
{$I demmapf_demix_tiles.inc}

{$IfDef ExGeology}
{$Else}
    {$I demmapf_geology.inc}
{$EndIf}

procedure MatchAnotherMapThisPixelSize(ThisMap,OtherMap : tMapForm);
begin
   {$IfDef RecordMatchMaps} WriteLineToDebugFile('MatchAnotherMapThisPixelSize, thisMap=' + ThisMap.Caption + ' other map=' + OtherMap.Caption); {$EndIf}

   if (not ThisMap.OtherMapSameSize(OtherMap)) {and (not ThisMap.OtherMapSameCoverage(OtherMap))} then begin
      OtherMap.SetMapPixelSize(ThisMap.MapDraw.ScreenPixelSize);
   end
   else begin
      {$IfDef RecordMatchMaps} WriteLineToDebugFile('MatchAnotherMapThisPixelSize already same pixel size'); {$EndIf}
   end;
end;

procedure MatchAnotherMapThisCoverageArea(ThisMap,OtherMap : tMapForm);
begin
    if (not ThisMap.OtherMapSameSize(OtherMap)) and (not ThisMap.OtherMapSameCoverage(OtherMap)) then begin
       if SameProjection(ThisMap.MapDraw,OtherMap.MapDraw) then begin
          OtherMap.MapDraw.MapCorners.BoundBoxProj := ThisMap.MapDraw.MapCorners.BoundBoxProj;
          OtherMap.SubsetAndZoomMapFromProjectedBounds;
       end
       else OtherMap.SubsetAndZoomMapFromGeographicBounds(ThisMap.MapDraw.MapCorners.BoundBoxGeo, true);
       OtherMap.FullMapSpeedButton.Enabled := true;
    end;
   UpdateMenusForAllMaps;
end;



procedure Bigimagewithallmaps(NumCols : integer = 3; FileName : PathStr = ''; MapsToUse : tStringList = Nil);
var
   BottomMargin,
   i,DEM,StartFont : integer;
   Findings : tStringlist;
   fName : PathStr;
   Bitmap : tMyBitmap;
   TStr : shortstring;

   function UseThisMap(MapCaption : shortstring) : boolean;
   var
      i : integer;
   begin
      for i := 0 to pred(MapsToUse.Count) do begin
         if (MapCaption = MapsToUse[i]) then begin
            Result := true;
            exit;
         end;
      end;
      Result := false;
   end;

begin
   {$IfDef RecordBigMap} WriteLineToDebugFile('Bigimagewithallmaps in'); {$EndIf}
   Findings := tStringList.Create;
   if MDDef.MapNameBelowComposite then begin
      BottomMargin := 55;
      StartFont := 34;
      for i := pred(WMDEM.MDIChildCount) downto 0 do begin
         if (WMDEM.MDIChildren[i] is tMapForm) and (WMDEM.MDIChildren[i] as TMapForm).UseMapForMultipleMapOperations then begin
            if (MapsToUse = Nil) or UseThisMap((WMDEM.MDIChildren[i] as TMapForm).Caption) then begin
               CopyImageToBitmap((WMDEM.MDIChildren[i] as TMapForm).Image1,Bitmap);
               Bitmap.Canvas.Font.Size := StartFont;
               Bitmap.Canvas.Font.Style := [fsBold];
               DEM := (WMDEM.MDIChildren[i] as TMapForm).MapDraw.DEMonMap;
               if ValidDEM(DEM) then begin
                  TStr := RemoveUnderScores(DEMGLB[DEM].AreaName);
                  while Bitmap.Canvas.TextWidth(TStr) > Bitmap.Width - 10 do Bitmap.Canvas.Font.Size := Bitmap.Canvas.Font.Size - 1;
                  StartFont := Bitmap.Canvas.Font.Size;
               end;
               Bitmap.Free;
            end;
         end;
      end;
   end
   else BottomMargin := 25;

   for i := pred(WMDEM.MDIChildCount) downto 0 do begin
      if (WMDEM.MDIChildren[i] is tMapForm) and (WMDEM.MDIChildren[i] as TMapForm).UseMapForMultipleMapOperations then begin
         if (MapsToUse = Nil) or UseThisMap((WMDEM.MDIChildren[i] as TMapForm).Caption) then begin
            (WMDEM.MDIChildren[i] as TMapForm).DoFastMapRedraw;
            CopyImageToBitmap((WMDEM.MDIChildren[i] as TMapForm).Image1,Bitmap);
            Bitmap.Canvas.Brush.Style := bsClear;
            Bitmap.Canvas.Pen.Width := 2;
            Bitmap.Canvas.Pen.Color := clBlack;
            Bitmap.Canvas.Rectangle(0,0,pred(Bitmap.Width),pred(Bitmap.Height));
            Bitmap.Height := Bitmap.Height + BottomMargin;
            Bitmap.Canvas.Pen.Width := 2;
            Bitmap.Canvas.Pen.Color := clWhite;
            Bitmap.Canvas.Brush.Style := bsSolid;
            Bitmap.Canvas.Brush.Color := clWhite;
            Bitmap.Canvas.Rectangle(0,Bitmap.Height-BottomMargin,Bitmap.Width,Bitmap.Height);
            Bitmap.Canvas.Brush.Style := bsClear;
            if MDDef.MapNameBelowComposite then begin
               Bitmap.Canvas.Font.Size := StartFont;
               Bitmap.Canvas.Font.Style := [fsBold];
               DEM := (WMDEM.MDIChildren[i] as TMapForm).MapDraw.DEMonMap;
               if ValidDEM(DEM) then begin
                  TStr := RemoveUnderScores(DEMGLB[DEM].AreaName);
                  while Bitmap.Canvas.TextWidth(TStr) > Bitmap.Width - 10 do Bitmap.Canvas.Font.Size := Bitmap.Canvas.Font.Size - 1;
                  Bitmap.Canvas.TextOut(5,(Bitmap.Height - Bitmap.Canvas.TextHeight(TStr) - 5), TStr);
               end;
            end;
            fName := NextFileNumber(MDtempDir,(WMDEM.MDIChildren[i] as TMapForm).Caption + '_','.bmp');
            Bitmap.SaveToFile(fName);
            Bitmap.Free;
            Findings.Add(fName);
         end;
      end;
   end;
   if (Findings.Count > 0) then begin
      {$IfDef  RecordBigMap} WriteLineToDebugFile('Bigimagewithallmaps in, maps=' + IntToStr(Findings.Count)); {$EndIf}
      MakeBigBitmap(Findings,'',FileName,NumCols);
   end;
end;


procedure SetMapsForBigBitmaps(Setting : boolean);
var
   i,n : integer;
begin
   n := 0;
   for i := pred(WMDEM.MDIChildCount) downto 0 do begin
      if (WMDEM.MDIChildren[i] is tMapForm) then begin
         (WMDEM.MDIChildren[i] as TMapForm).UseMapForMultipleMapOperations := setting;
         inc(n);
      end;
   end;
   {$IfDef RecordBigMap} WriteLineToDebugFile('SetMapsForBigBitmaps out, set maps=' + IntToStr(n)); {$EndIf}
end;

procedure TMapForm.Bigimagewithallmaps1Click(Sender: TObject);
begin
    Bigimagewithallmaps;
end;


function MonthlyInsolationGraph(DEM : integer; Lat,Long : float32) : TThisBaseGraph;
var
   TStr : shortstring;
   z : float32;
begin
   Result := nil;
   {$IfDef ExMultigrid}
   {$Else}
      OpenSolarRad(false);
      if ValidMultiGrid(SolarRad) then begin
         if (DEM <> 0) and DEMGlb[DEM].GetElevFromLatLongDegree(Lat,Long,z) then TStr := RealToString(z,-8,0) + ' m' else TStr := '';
         Result := MultiGridArray[SolarRad].AnnualParameterGraph(Lat,Long,TStr);
         if (Result <> Nil) then begin
            Result.GraphDraw.MaxVertAxis := 30000;
            Result.GraphDraw.MinVertAxis := 0;
            Result.GraphDraw.MaxHorizAxis := 12;
            Result.GraphDraw.MinHorizAxis := 1;
            Result.GraphDraw.VertLabel := 'Insolation';
            Result.GraphDraw.LineSize256[1] := 4;
            Result.Width := 500;
            Result.Height := 400;
            Result.RedrawDiagram11Click(Nil);
         end;
      end;
   {$EndIf}
end;


function MakePOTETgraph(DEM : integer; Lat,Long : float32) : TThisBaseGraph;
{$IfDef ExMultigrid}
begin
{$Else}
var
   TStr : shortstring;
   z : float32;
begin
   {$IfDef RecordGeography} WriteLineToDebugFile('TMapForm.Evapotranspirationprecipitationwaterbudget1Click'); {$EndIf}
   OpenTempPrecipEvap(false);
   if ValidMultiGrid(ETOMG) and ValidMultiGrid(PrecipMG) then begin
      TStr := '';
      if (DEM <> 0) and DEMGlb[DEM].GetElevFromLatLongDegree(Lat,Long,z) then TStr := RealToString(z,-8,0) + ' m';
      Result := MultipleMultigridsAnnualParamrGraph(ETOMG,PrecipMG,0,Lat,Long,TStr);
      if (Result <> Nil) then begin
         Result.GraphDraw.MaxVertAxis := 10 * MDDef.KoppenOpts.MaxPrecip;
         Result.GraphDraw.MaxHorizAxis := 12;
         Result.GraphDraw.MinHorizAxis := 1;
         Result.GraphDraw.VertLabel := 'POTET/Precip (mm)';
         Result.GraphDraw.LegendList := tStringList.Create;
         Result.GraphDraw.LegendList.Add('Evapotranspiration');
         Result.GraphDraw.LegendList.Add('Precipitation');
         Result.Caption := 'Water budget: ' + Result.Caption + ' ' + TStr;
         Result.Width := 500;
         Result.Height := 400;
         Result.RedrawDiagram11Click(Nil);
      end;
   end;
{$EndIf}
end;

function MakeKoppenClimograph(DEM : integer; Lat,Long : float32) : TKoppenGraph;
var
   ClimateData : tClimateData;
   z : float32;
begin
   {$IfDef RecordGeography} WriteLineToDebugFile('TMapForm.Koppenclimographfromclimategrids1Click'); {$EndIf}
   OpenTempPrecipEvap(false);
   if ValidMultiGrid(TempMG) and ValidMultiGrid(PrecipMG) then begin
      ClimateData.Lat := Lat;
      ClimateData.Long := Long;
      ClimateData.Elevation := -9999;
      if (DEM <> 0) then begin
         if (DEMGlb[DEM].DEMheader.ElevUnits in [euMeters]) and DEMGlb[DEM].GetElevFromLatLongDegree(ClimateData.Lat,ClimateData.Long,z) then
            ClimateData.Elevation := round(z);
      end;
      ClimateData.Location := 'Global Grids ';
      LoadClimateData(ClimateData);
      if ClassifyClimate(ClimateData) then Result := OpenKoppenGraph(500,400,ClimateData)
      else Result := Nil;
   end;
end;


procedure NakedMapOptions;
begin
   SaveBackupDefaults;
   MDDef.ScaleBarLocation.DrawItem := false;
   MDDef.GridLegendLocation.DrawItem := false;
   MDDef.TerrainCatLegend.DrawItem := false;
   MDDef.NorthArrowLocation.DrawItem := false;
   MDDef.MapNameLocation.DrawItem := false;
   MDDef.MapTicks := tixNone;
end;


function NumOpenMaps : integer;
var
   i : integer;
begin
   Result := 0;
   for i := pred(WMDEM.MDIChildCount) downto 0 do begin
      if (WMDEM.MDIChildren[i] is TMapForm) then begin
         Inc(result);
      end;
   end;
end;


procedure UpdateMenusForAllMaps;
var
   i : integer;
begin
   if (WMDEM.MDIChildCount > 0) then begin
      for i := pred(WMDEM.MDIChildCount) downto 0 do begin
         if WMDEM.MDIChildren[i] is TMapForm then begin
            (WMDEM.MDIChildren[i] as TMapForm).CheckProperTix;
         end;
      end;
   end;
end;


procedure TMapForm.OutlineGridOutlines;
var
   //DEMWanted,xi,yi ,
   i,x,y,xp,yp,n,SymSize : integer;
   bb : sfBoundBox;
   Sum : float64;
   z,xg,yg : float32;
   Lat,Long : float64;
   Sym : tDrawingSymbol;
   DEMsWanted : tDEMbooleanArray;
begin
   if (MapDraw.DEMonMap = 0) then exit;
   for i := 1 to MaxDEMDataSets do begin
      DEMsWanted[i] := ValidDEM(i) and (not DEMGlb[i].HiddenGrid) and (i <> MapDraw.DEMonMap);
   end;

   GetMultipleDEMsFromList('Outline pixel/cell outlines',DEMsWanted);
   Sym := FilledDiamond;
   SymSize := 5;
   for i := 1 to MaxDEMDataSets do if DEMsWanted[i] then begin
      bb := DEMGlb[i].bbDEMGridPartOfDEMonMap(Self);
      xp := round((bb.xmax + bb.xmin) / 2);
      yp := round((bb.ymax + bb.ymin) / 2);

      MapDraw.ScreenToLatLongDegree(MapDraw.MapXSize div 2, MapDraw.MapYSize div 2, Lat,Long);
      DEMGlb[i].LatLongDegreeToDEMGrid(Lat,Long,xg,yg);
      xp := round(xg);
      yp := round(yg);

      {$IfDef RecordCarto}
         DEMGlb[i].DEMGridToLatLongDegree(xp,yp,Lat,Long);
         WriteLineToDebugFile('TMapForm.OutlineGridOutlines, DEM=' + IntToStr(i) + '  ' + DEMGlb[i].AreaName +
            '  x=' + IntToStr(xp) + '  y=' + IntToStr(yp) + '  ' + LatLongDegreeToString(Lat,Long));
      {$EndIf}
      if (DEMGlb[i].DEMHeader.DEMUsed = ArcSecDEM) then OutlineGeoBox(DEMGlb[i].PixelBoundBoxGeo(xp,yp),WinGraphColors[i],2)
      else OutlineUTMBox(DEMGlb[i].PixelBoundBoxUTM(xp,yp),WinGraphColors[i],2);

      n := 0;
      Sum := 0;
      bb := DEMGlb[MapDraw.DEMonMap].bbDEMGridPartOfDEMonMap(Self);
      for x := round(bb.xmin) to round(bb.xmax) do begin
         for y := round(bb.ymin) to round(bb.ymax) do begin
            DEMGlb[MapDraw.DEMonMap].DEMGridToLatLongDegree(x,y,Lat,Long);
            if PointInBoundingBox(Lat,Long,DEMGlb[i].PixelBoundBoxGeo(xp,yp)) then begin

               //DEMGlb[i].LatLongDegreeToDEMGridInteger(Lat,Long,xi,yi);
               //WriteLineToDebugFile(RealToString(Lat,-12,-8) + ',' + RealToString(Long,-12,-8) + ',' + IntToStr(xi)  + ',' + IntToStr(yi));

               MapDraw.MapSymbolAtLatLongDegree(Image1.Canvas,Lat,Long,Sym,SymSize,ConvertTColorToPlatformColor(WinGraphColors[i]));
               inc(n);
               DEMGlb[MapDraw.DEMonMap].GetElevMeters(x,y,z);
               Sum := Sum + z;
            end;
         end;
      end;
      Sym := FilledBox;
      SymSize := 3;
      DEMGlb[i].GetElevMeters(xp,yp,z);
      {$IfDef RecordCarto}
         WriteLineToDebugFile(DEMGlb[i].AreaName + '  z=' + RealToString(z,-12,2) + '  aggregate z=' + RealToString(Sum/n,-12,2) +
            '  n=' + IntToStr(n) + '  ' + ColorString(WinGraphColors[i]));
      {$EndIf}


      (*
         for i := 1 to MaxDEMDataSets do if DEMsWanted[i] then begin
            bb := DEMGlb[i].PartOfDEMonMap(Self);
            for x := round(bb.xmin) to round(bb.xmax) do begin
               for y := round(bb.ymin) to round(bb.ymax) do begin
                  if (DEMGlb[i].DEMHeader.DEMUsed = ArcSecDEM) then OutlineGeoBox(DEMGlb[i].PixelBoundBoxGeo(x,y),WinGraphColors[i],2)
                  else OutlineUTMBox(DEMGlb[i].PixelBoundBoxUTM(x,y),WinGraphColors[i],2);
               end;
            end;
         end;
      *)
   end;

         for i := 1 to MaxDEMDataSets do if DEMsWanted[i] then begin
            bb := DEMGlb[i].bbDEMGridPartOfDEMonMap(Self);
            for x := round(bb.xmin) to round(bb.xmax) do begin
               for y := round(bb.ymin) to round(bb.ymax) do begin
                  DEMGlb[i].DEMGridToLatLongDegree(x,y,Lat,Long);
                  MapDraw.MapSymbolAtLatLongDegree(Image1.Canvas,Lat,Long,Sym,SymSize,ConvertTColorToPlatformColor(WinGraphColors[i]));
               end;
            end;
         end;

   CheckThatLegendsAreOnTop;
end;


procedure TMapForm.Brightnesstemperature1Click(Sender: TObject);
begin
   DEMDef_Routines.SaveBackupDefaults;
   MDDef.dnConvert := dncBrightness;
   NewSatWindow(nsbBrightness);
   DEMDef_Routines.RestoreBackupDefaults;
end;


procedure SummarizeEnsemble(Lat,Long : float64);
var
   i,c,BestClass : integer;
   z,prob,MaxProb : float32;
   Results : tStringList;
   aFilter : ANSIString;
   MostLikely : array[1..15] of integer;
   ClassCat : array[1..15] of float32;
begin
   Results := tStringList.Create;
   Results.Add(LatLongDegreeToString(Lat,Long));
   for c := 1 to 7 do begin
      MostLikely[c] := 0;
      ClassCat[c] := 0;
   end;

   for i := 1 to MaxDEMDataSets do begin
      if ValidDEM(i) then begin
         if DEMGlb[i].GetElevFromLatLongDegree(Lat,Long,z) then begin
           aFilter := 'GRID_NAME=' + QuotedStr(ptTrim(DEMGlb[i].AreaName)) + ' AND CLASS =' + IntToStr(round(z));
           GISDB[EnsembleClassDB].MyData.ApplyFilter(aFilter);
           if GISDB[EnsembleClassDB].MyData.FiltRecsInDB > 0 then begin
               Results.Add(DEMGlb[i].AreaName  +   '   cat=' + IntToStr(round(z)));
               MaxProb := 0;
               for c := 1 to 7 do begin
                   prob := GISDB[EnsembleClassDB].MyData.GetFieldByNameAsFloat('CAT_' + IntToStr(c));
                   ClassCat[c] := ClassCat[c] + prob;
                   if prob > MaxProb then begin
                      MaxProb := prob;
                      BestClass := c;
                   end;
                   if (prob > 0.05) then Results.Add('   ' + IntToStr(c) + RealToString(prob,8,4));
               end;
               inc(MostLikely[BestClass]);
           end;
           Results.Add('');
         end;
      end;
   end;

   MaxProb := 0;
   for c := 1 to 7 do begin
      Results.Add('Class ' + intToStr(c) + RealToString(ClassCat[c],8,4));
      if ClassCat[c] > MaxProb then begin
         MaxProb := ClassCat[c];
         BestClass := c;
      end;
   end;
   Results.Add('Most likely class=' + IntToStr(BestClass) +  '  with prob=' + RealToString(MaxProb/8,8,4));

   MaxProb := 0;
   for c := 1 to 7 do begin
      if MostLikely[c] > MaxProb then begin
         MaxProb := MostLikely[c];
         BestClass := c;
      end;
   end;
   Results.Add('Most likely class=' + IntToStr(BestClass) +  '  in models=' + IntToStr(round(MaxProb)));
   Results.Add('');
   Petmar.DisplayAndPurgeStringList(Results,'Point');
end;


type
   tPrediction = array[1..10,  //for the grids
                       0..30,  //class in ensemble members
                       1..10] of float32;  //class in final classification
var
   Prediction : tPrediction;


procedure EnsemblePrediction(Lat,Long : float64; var BestClass : Integer; var Agreement : float64);
var
   i,c,NumClass,cat{,BestHighClass} : integer;
   z,prob,MaxProb{,MaxHighProb} : float32;
   MostLikelyHigh,
   MostLikely : array[1..15] of integer;
   ClassCat : array[1..15] of float32;
begin
   for c := 1 to 7 do begin
      MostLikely[c] := 0;
      MostLikelyHigh[c] := 0;
      ClassCat[c] := 0;
   end;
   NumClass := 0;
   for i := 2 to MaxDEMDataSets do begin
      if ValidDEM(i) then begin
         inc(NumClass);
         if DEMGlb[i].GetElevFromLatLongDegree(Lat,Long,z) then begin
           c := round(z);
          //MaxProb := 0;
           //MaxHighProb := 0;
           //BestHighClass := 0;
           for cat := 1 to 7 do begin
              prob := Prediction[i,c,cat];
              ClassCat[cat] := ClassCat[cat] + prob;
           end {for cat loop};
         end {if good z};
      end {if ValidDEM(i)};
   end {for};
   MaxProb := 0;
   for c := 1 to 7 do begin
       if ClassCat[c] > MaxProb then begin
          MaxProb := ClassCat[c];
          BestClass := c;
       end;
   end;
   Agreement := MaxProb/NumClass;
end;

procedure LoadProbabilityMatrix;
var
   i,c,cat : integer;
   prob : float64;
   aFilter : ANSIString;
begin
   for i := 1 to MaxDEMDataSets do begin
      if ValidDEM(i) then begin
         aFilter := 'GRID_NAME=' + QuotedStr(ptTrim(DEMGlb[i].AreaName));
         GISDB[EnsembleClassDB].MyData.ApplyFilter(aFilter);
         if GISDB[EnsembleClassDB].MyData.FiltRecsInDB > 0 then begin
            while not GISDB[EnsembleClassDB].MyData.eof do begin
               c := GISDB[EnsembleClassDB].MyData.GetFieldByNameAsInteger('CLASS');
               for cat := 1 to 7 do begin
                  prob := GISDB[EnsembleClassDB].MyData.GetFieldByNameAsFloat('CAT_' + IntToStr(cat));
                  Prediction[i,c,cat] := prob;
               end;
               GISDB[EnsembleClassDB].MyData.Next;
            end;
          end;
      end;
   end;
end;

procedure EnsembleClassify;
var
   BestClass,x,y,ProbDEM,ClassDEM : integer;
   z : float32;
   Agreement,Lat,Long : float64;
begin
   ProbDEM := DEMGlb[1].CloneAndOpenGridSetMissing(ByteDEM,'Ensemble probability',Undefined);
   ClassDEM := DEMGlb[1].CloneAndOpenGridSetMissing(ByteDEM,'Ensemble classification',Undefined);
   StartProgress('Ensemble classify');
   for x := 0 to pred(DEMGlb[1].DEMheader.NumCol) do begin
     UpdateProgressBar(x/DEMGlb[1].DEMheader.NumCol);
     for y := 0 to pred(DEMGlb[1].DEMheader.NumRow) do begin
        if DEMGlb[1].GetElevMetersOnGrid(x,y,z) then begin
           DEMGlb[1].DEMGridToLatLongDegree(x,y,Lat,Long);
           EnsemblePrediction(Lat,Long,BestClass,Agreement);
           if (round(z) = 99) then DEMGlb[ClassDEM].SetGridElevation(x,y,BestClass)
           else DEMGlb[ClassDEM].SetGridElevation(x,y,z);
           DEMGlb[ProbDEM].SetGridElevation(x,y,100 * Agreement);
        end;
     end;
   end;
   EndProgress;
   DEMGlb[ProbDEM].SetUpMap(ClassDEM,true,mtElevSpectrum);
   DEMGlb[ProbDEM].SetUpMap(ProbDEM,true,mtElevSpectrum);
end;

(*
procedure TMapForm.WMBroadcastMapRedraw(var Msg : TMessage);
var
   Bitmap : tMyBitmap;
   x,y : array[1..4] of float64;
   xc,yc,xp,yp,i : integer;
   LatLow,LongLow,LatHigh,LongHigh : float64;
begin
   {$IfDef RecordMessages} WriteLineToDebugFile('TMapForm.WMBroadcastMapRedraw in ' + Self.Caption); {$EndIf}

   if OutlineCoverageOtherMaps then begin
       CopyImageToBitmap(Image1,Bitmap);
       Bitmap.Canvas.Pen.Color := OutlineCoverageColor;
       Bitmap.Canvas.Pen.Width := OutlineCoverageWidth;
       MapDraw.LatLongDegreeToScreen(BroadcastMapBoundBoxGeo.ymax,BroadcastMapBoundBoxGeo.xmin,xp,yp); Bitmap.Canvas.MoveTo(xp,yp);
       MapDraw.LatLongDegreeToScreen(BroadcastMapBoundBoxGeo.ymax,BroadcastMapBoundBoxGeo.xmax,xc,yc); Bitmap.Canvas.LineTo(xc,yc);
       MapDraw.LatLongDegreeToScreen(BroadcastMapBoundBoxGeo.ymin,BroadcastMapBoundBoxGeo.xmax,xc,yc); Bitmap.Canvas.LineTo(xc,yc);
       MapDraw.LatLongDegreeToScreen(BroadcastMapBoundBoxGeo.ymin,BroadcastMapBoundBoxGeo.xmin,xc,yc); Bitmap.Canvas.LineTo(xc,yc);
       Bitmap.Canvas.LineTo(xp,yp);
       Image1.Picture.Graphic := Bitmap;
       FreeAndNil(Bitmap);
   end
   else begin
      {$IfDef RecordMessages} WriteLineToDebugFile('TMapForm.WMBroadcastMapRedraw MaxCoverage'); {$EndIf}
      if (BroadCastXSize > 0) then begin
         MapDraw.MapXSize := BroadcastXSize;
         MapDraw.MapYSize := BroadcastYSize;
      end;
      if (abs(BroadCastMinElev) < 32000) and MapDraw.ValidDEMonMap and ((MapDraw.MapType in [mtElevRainbow,mtDEMReflectElevMerge,mtElevRainbow,mtElevSpectrum]) or (MapDraw.MapMerge = mmElevation)) then begin
         {$IfDef RecordMessages} WriteLineToDebugFile('TMapForm.WMBroadcastMapRedraw ElevRedraw, ' + IntToStr(round(BroadCastMinElev)) + ' to ' + IntToStr(round(BroadCastMaxElev))); {$EndIf}
         MapDraw.MinMapElev := BroadCastMinElev;
         MapDraw.MaxMapElev := BroadCastMaxElev;
      end;

      if BroadcastMapBoundBoxGeo.YMax > -99 then begin
         SubsetAndZoomMapFromGeographicBounds(BroadcastMapBoundBoxGeo);
         FullMapSpeedButton.Enabled := true;
         CheckProperTix;
      end
      else DoBaseMapRedraw;
   end;
   {$IfDef RecordMessages} WriteLineToDebugFile('TMapForm.WMBroadcastMapRedraw out'); {$EndIf}
end;
*)


function TMapForm.OtherMapSameSize(om : tMapForm) : boolean;
begin
   Result := (MapDraw.MapXSize = Om.MapDraw.MapXSize) and (MapDraw.MapYSize = Om.MapDraw.MapYSize);
end;

function TMapForm.OtherMapSameCoverage(om : tMapForm) : boolean;
begin
   Result := (abs(MapDraw.MapCorners.BoundBoxGeo.XMin - OM.MapDraw.MapCorners.BoundBoxGeo.XMin) < 0.01) and
             (abs(MapDraw.MapCorners.BoundBoxGeo.XMax - OM.MapDraw.MapCorners.BoundBoxGeo.XMax) < 0.01) and
             (abs(MapDraw.MapCorners.BoundBoxGeo.yMin - OM.MapDraw.MapCorners.BoundBoxGeo.yMin) < 0.01) and
             (abs(MapDraw.MapCorners.BoundBoxGeo.yMax - OM.MapDraw.MapCorners.BoundBoxGeo.yMax) < 0.01);
end;


procedure TMapForm.WMBroadcastLatLongMessage(var Msg : TMessage);
var
    x,y : integer;
begin
   {$IfDef GPSBroadcast} WriteLineToDebugFile('TMapForm.WMBroadcastLatLongMessage in, ' + Self.Caption); {$EndIf}
   if ClosingIsHappening or (MapDraw = Nil) or (DEMNowDoing in [Calculating]) or (not MapDraw.MapDrawValid)or MapDraw.ClosingMapNow then exit;

   if (not (MapDraw.MapOwner in [moDrapeMap,moPointVerificationMap])) then begin
       MapDraw.LatLongDegreeToScreen(LastRoamLat,LastRoamLong,x,y);

       {$IfDef Chirps}
       if LastRoamZ < pred(MaxSmallInt) then begin
          Image1.Canvas.Pen.Mode := pmNotXor;
          Image1.Canvas.Pen.Width := 3;
          if OnScreen(LastBroadcastX,LastBroadcastY) then
             CrossWithHole(Image1.Canvas,LastBroadcastX,LastBroadcastY);

          Image1.Canvas.Pen.Mode := pmCopy;
          Image1.Canvas.Pen.Color := clBlack;
          Image1.Canvas.Pen.Width := 1;
          if OnScreen(x,y) then begin
            CrossWithHole(Image1.Canvas,x,y);
            Image1.Canvas.TextOut(x+5,Y+5,RealToString(LastRoamZ,-8,1));
          end;

          Image1.Canvas.Pen.Mode := pmNotXor;
          Image1.Canvas.Pen.Width := 3;
          if OnScreen(LastBroadcastX,LastBroadcastY) then CrossWithHole(Image1.Canvas,LastBroadcastX,LastBroadcastY);
       end
       else
      {$EndIf}
       begin
          Image1.Canvas.Pen.Color := ConvertPlatformColorToTColor(MDDef.RoamSecondMapColor);
          Image1.Canvas.Pen.Mode := pmNotXor;
          Image1.Canvas.Pen.Width := 3;
          if MapDraw.OnScreen(LastBroadcastX,LastBroadcastY) then CrossWithHole(Image1.Canvas,LastBroadcastX,LastBroadcastY);
          if MapDraw.OnScreen(x,y) then begin
            CrossWithHole(Image1.Canvas,x,y);
            LastBroadcastX := x;
            LastBroadcastY := y;
          end
          else begin
            LastBroadcastX := -1;
            LastBroadcastY := -1;
          end;
       end;
   end;
   {$IfDef GPSBroadcast} WriteLineToDebugFile('TMapForm.WMBroadcastLatLongMessage out, ' + Self.Caption); {$EndIf}
end;


{=============================================== end Windows Message routines ============================================}


procedure tMapForm.RedrawMapDefaultsSize;
begin
   MapDraw.MapXSize := 0;
   MapDraw.MapYSize := 0;
   DoCompleteMapRedraw;
end;


procedure tMapForm.DoFastMapRedraw;
var
   BitMap : tMyBitmap;
   x,y,i  : integer;
begin
  if (MapDraw <> Nil) and MapRedrawsAllowed and (not MapDraw.ClosingMapNow) then begin
     {$If Defined(RecordMapDraw) or Defined(RecordPixelSize) or Defined(RecordTimingProblems) or Defined(RecordUTMZone)}
        Stopwatch := TStopwatch.StartNew;
        WriteLineToDebugFile('TMapForm.DoFastMapRedraw in, maptype=' + IntToStr(MapDraw.MapType) + '  ' + MapDraw.MapSizeString + ' Map UTM zone=' + IntToStr(MapDraw.PrimMapProj.projUTMZone));
     {$EndIf}

     if (SavedMapImage <> Nil) then FreeAndNil(SavedMapImage);
     AssignMapOwnerToPLSS(self);
     if VariableOpaqueOverlays then begin
        MapDraw.DrawMapOnBMP(Bitmap,true,false);
        FormResize(Nil);
        Self.Image1.Picture.Graphic := Bitmap;
        FreeAndNil(Bitmap);
        SetUpTopForm;
        MapDraw.DrawMapOnBMP(OverlayOpaqueBMP,false,true);
     end
     else begin
        {$If Defined(RecordPixelSize) or Defined(RecordTimingProblems)} WriteLineToDebugFile('TMapForm.DoFastMapRedraw, all MapDraw.DrawMapOnBMP: ' + MapDraw.MapSizeString); {$EndIf}

        try
           if MDdef.DBsOnAllMaps then begin
              for i := 1 to MaxDataBase do begin
                 if ValidDB(i) then GISDB[i].AssociateMap(self);
              end;
           end;

           Bitmap := Nil;
           MapDraw.DrawMapOnBMP(Bitmap);
           {$If Defined(RecordPixelSize) or Defined(RecordTimingProblems)} WriteLineToDebugFile('TMapForm.DoFastMapRedraw, point 2 ' + MapDraw.MapSizeString); {$EndIf}

           if (Not MapDraw.FastMapDraw) and (MapDraw.SectorOutlines <> '') then begin
              MapDraw.OutlineSectors(Bitmap.Canvas);
              {$IfDef RecordTiming} WriteLineToDebugFile('tMapForm.DoFastMapRedraw 3'); {$EndIf}
           end;

           if FullDraw then FormResize(Nil);
        finally
           if (Bitmap <> Nil) then begin
              Self.Image1.Picture.Graphic := Bitmap;
              Bitmap.Free;
           end;
        end;
        if (not MapDraw.NoDrawingNow) and FullDraw and (Not MapDraw.FastMapDraw) then begin
           {$IfDef ExDrainage}
           {$Else}
              if MapDraw.RedrawDrainageVectors then DrawDownhillVectors;
           {$EndIf}

           PlotExtremeZValues(Nil);

           if MDDef.NorthArrowLocation.DrawItem then begin
              PlotNorthArrowLegend(15,MapDraw.MapYSize - 65);
           end;

           if ShowKeyLocation then begin
              MapDraw.MapSymbolAtLatLongDegree(Image1.Canvas,KeyLocationLat,KeyLocationLong,MDDef.KeyLocationSymbol);
              MapDraw.LatLongDegreeToScreen(KeyLocationLat,KeyLocationLong,x,y);
              ScrollBox1.HorzScrollBar.Position := X - ClientWidth div 2;
              ScrollBox1.VertScrollBar.Position := Y - ClientHeight div 2;
           end;

           {$IfDef ExPointCloud}
           {$Else}
              if (pt_cloud_opts_fm <> Nil) and MDDef.ShowCloudOutlines then begin
                 pt_cloud_opts_fm.MapCallOutlineClouds;
              end;
           {$EndIf}

           {$IfDef ExExoticMaps}
           {$Else}
              if AutoAnaglyphRedraw then Draw3Dmap(Self);
           {$EndIf}
        end;
        {$IfDef ExRedistrict}
        {$Else}
           if (RedistrictForm <> Nil) and (not MapDraw.FastMapDraw) then RedistrictForm.BitBtn4Click(Nil);
        {$EndIf}
     end;
     if ValidDEM(SavedMergeReflectanceDEM) then MergeAnotherDEMreflectance(SavedMergeReflectanceDEM);

     MapDraw.MapDrawValid := true;
     LastGPSX := MaxInt;
     LastGPSY := MaxInt;
     ChangeDEMNowDoing(DEMNowDoing);
     if (MapDraw <> Nil) and (MapDraw.BaseTitle <> '') then Caption := MapDraw.BaseTitle;
     ComboBox1.Text := RealToString(100.0*MapDraw.MapZoomFactor,-8,1) + '%';

     {$IfDef ExMultigrid}
     {$Else}
        if (MapDraw.MultiGridOnMap <> 0) then begin
           if MultiGridArray[MapDraw.MultiGridOnMap].MonthlyData then begin
              Image1.Canvas.Font.Size := 18;
              Image1.Canvas.TextOut(5,5,MonthName[MultiGridArray[MapDraw.MultiGridOnMap].IndexBand]);
           end;
        end;
        if (MapDraw.MonthlyDBArrayOnMap <> 0) then begin
           Image1.Canvas.Font.Size := 18;
           Image1.Canvas.TextOut(5,5,MonthName[MonthlyDBArray[MapDraw.MonthlyDBArrayOnMap].CurrentMonth]);
        end;
     {$EndIf}

     {$IfDef ExSat}
     {$Else}
        if (MapDraw.ValidSatOnMap) and (not (MapDraw.MapOwner = moIndexMap)) and (not (MapDraw.MapOwner = moMapDatabase)) then begin
           if (MapDraw.BaseTitle = '') then MapDraw.BaseTitle := SatImage[MapDraw.SATonMap].SceneBaseName;
           if SatImage[MapDraw.SatOnMap].CanEnhance then begin
              Caption := RemoveUnderscores(MapDraw.BaseTitle)  + '  ' + MapDraw.CurrentSatelliteColors;
           end
           else Caption := RemoveUnderscores(MapDraw.BaseTitle);
        end;
     {$EndIf}

     {$If Defined(RecordTimingProblems) or Defined(RecordMapDraw) or Defined(RecordUTMZone)}
        Elapsed := Stopwatch.Elapsed;
        WriteLineToDebugFile('TMapForm.DoFastMapRedraw out, Maptype= ' + MapTypeName(MapDraw.MapType) + '  ' + MapDraw.MapSizeString + '   ' + RealToString(Elapsed.TotalSeconds,-12,-4) + ' sec');
     {$EndIf}
     PixelSizeEdit.Text := RealToString(MapDraw.ScreenPixelSize,-12,-2) + ' m';
     CheckProperTix;
     WmDem.SetMenusForVersion;
     {$If Defined(RecordTimingProblems) or Defined(RecordMapDraw)} WriteLineToDebugFile('TMapForm.DoFastMapRedraw out'); {$EndIf}
  end;
  {$IfDef RecordMapResize} DebugMapSize; {$EndIf}
end;


procedure tMapForm.DoBaseMapRedraw;
begin
   if MapRedrawsAllowed then begin
      {$If Defined(RecordMapDraw) or Defined(RecordPixelSize)} WriteLineToDebugFile('tMapForm.DoBaseMapRedraw,  pix size =' + RealToString(MapDraw.ScreenPixelSize,-12,-2)); {$EndIf}
      MapDraw.DeleteSingleMapLayer(MapDraw.BaseMapFName);
      MapDraw.DeleteSingleMapLayer(MapDraw.LegendOverlayfName);
      DoFastMapRedraw;
   end;
end;

procedure tMapForm.DoCompleteMapRedraw;
begin
   if MapRedrawsAllowed then begin
      {$If Defined(RecordMapDraw) or Defined(RecordMapResize)} WriteLineToDebugFile('tMapForm.DoCompleteMapRedraw in'); {$EndIf}
      MapDraw.DeleteMapSavedLayers;
      {$If Defined(RecordMapDraw) or Defined(RecordMapResize)} WriteLineToDebugFile('tMapForm.DoCompleteMapRedraw DeleteMapSavedLayers'); {$EndIf}
      DoFastMapRedraw;
      {$If Defined(RecordMapDraw) or Defined(RecordMapResize)} WriteLineToDebugFile('tMapForm.DoCompleteMapRedraw out'); {$EndIf}
   end;
end;


function tMapForm.OverlayUp(Layer : tOverlayOrder) : boolean;
var
   i : integer;
begin
   for i := 1 to MaxOverlays do begin
      if MapDraw.OverLayOrder[i] = Layer then begin
         Result := true;
         exit;
      end;
   end;
   Result := false;
end;


procedure TMapForm.Simpleelevationcheck1Click(Sender: TObject);
begin
   {$IfDef ExDrainage}
   {$Else}
      BasinFlooding(Self,-99,-99);
   {$EndIf}
end;

function TMapForm.SiteContourMapBlowUp(Lat,Long : float64; SiteBlowupMapSize,ZoomSize,ContourInterval : integer; Title : shortString) : tMapForm;
var
   uMinLat,uMinLong,uMaxLat,uMaxLong : float64;
begin
   Result := Nil;
   Result := DuplicateMap(false);
   Result.MapDraw.BaseTitle := Title;
   Result.MapDraw.MapType := mtDEMBlank;
   Result.MapDraw.FirstMapDrawing := false;
   VincentyPointAtDistanceBearing(Lat,Long,SiteBlowupMapSize,225,uMinLat,uMinLong);
   VincentyPointAtDistanceBearing(Lat,Long,SiteBlowupMapSize,45,uMaxLat,uMaxLong);
   Result.MapDraw.MaximizeLatLongMapCoverage(uMinLat,uMinLong,uMaxLat,uMaxLong);
   Result.MapDraw.MapType := mtSlopeTrafficCats;
   Result.MapDraw.MapMerge := mmNone;
   MDDef.LabelContours := false;
   Result.MapDraw.MapOverlays.ConInt := ContourInterval;
   Map_Overlays.AddOverlay(Result,ovoContours);
   Result.SetMapOverlays;
   Result.ShowKeyLocation := true;
   Result.KeyLocationLat := Lat;
   Result.KeyLocationLong := Long;
   Result.ResizeByPercentage(ZoomSize{,true,true});
end;


{$IfDef ExDrainage}
{$Else}
      procedure TMapForm.FindDrainageAreaContributing(x,y : integer);
      const
         Tol = 3;
      var
         LowLat,LowLong,HiLat,HiLong : float64;
         TheMinElev,TheMaxElev : float32;
      begin
         MapDraw.ScreenToLatLongDegree(X-Tol,Y-Tol,HiLat,LowLong);
         MapDraw.ScreenToLatLongDegree(X+Tol,Y+Tol,LowLat,HiLong);
         DEMGlb[AD8DEM].AreaExtremeElevationsFromLatLong(LowLat,LowLong,HiLat,HiLong, TheMinElev,TheMaxElev);
         WMDEM.SetPanelText(3, SmartAreaFormat(TheMaxElev * DEMGlb[AD8DEM].AverageXSpace * DEMGlb[AD8DEM].AverageYSpace));
      end;

      procedure TMapForm.CheckDrainageDblClick;
      begin
         if (DEMNowDoing = FloodBasin) then begin
            CheckThisPoint(LastX,LastY,xDEMg1,yDEMg1,xsatg1,ysatg1,CheckAll);
            Basin_flooding.BasinFlooding(Self,round(xDEMg1),round(yDEMg1));
            BackToWandering;
         end;
      end;
{$EndIf}



procedure TMapForm.Fixedpalettecategories1Click(Sender: TObject);
begin
   Fixedpalettestats1Click(Sender);
end;

procedure TMapForm.Fixedpalettestats1Click(Sender: TObject);
var
   i,j,n,total   : integer;
   sl : tStringList;
   fName : PathStr;
   zj : float32;
   Color : tPlatFormColor;
begin
   StartProgress('Stats');
   for n := 1 to 256 do MapDraw.zColorTable.ZTableCount[n] := 0;
   Total := 0;
   for j := MapDraw.MapAreaDEMGridLimits.YGridLow to MapDraw.MapAreaDEMGridLimits.YGridHigh do begin
      if (j mod 100 = 0) then UpdateProgressBar(j / MapDraw.MapAreaDEMGridLimits.YGridHigh);
      for i := MapDraw.MapAreaDEMGridLimits.XGridLow to MapDraw.MapAreaDEMGridLimits.XGridHigh do begin
         if DEMGlb[MapDraw.DEMonMap].GetElevMeters(i,j,zj) then begin
            Color := ColorFromZColorTable(MapDraw.ZColorTable,zj,n);
            inc(MapDraw.zColorTable.ZTableCount[n]);
         end;
      end;
   end {for j};
   for n := 1 to 256 do Total := Total + MapDraw.zColorTable.zTableCount[n];
   sl := tStringList.Create;
   sl.Add('COLOR,NAME,COUNT,PERCENT');
   for n := MapDraw.zColorTable.ZTableEntries downto 1 do begin
      sl.add(IntToStr(ConvertPlatformColorToTColor(MapDraw.zColorTable.zTableColors[n])) + ',' + MapDraw.zColorTable.zTableLabels[n] + ',' + IntToStr(MapDraw.zColorTable.zTableCount[n]) + ',' +
          RealToString(100 * MapDraw.zColorTable.zTableCount[n] / Total,-12,3));
   end;
   fName := Petmar.NextFileNumber(MDTempDir, 'Category_count_', DefaultDBExt);
   StringListToLoadedDatabase(sl,fName);
   EndProgress;
end;


{$IfDef ExAdvancedGIS}
{$Else}

procedure tmapForm.CheckAdvancedGISDoubleClick(Lat,Long : float64);
const
   PointSpacing : float64 = 50;
var
   i,RecsFound,npts : integer;
   FeatName : ShortString;
   fName : ShortString;
   TStr : ShortString;
   xg,yg : float32;
   OneRec,OtherRec,PathDistance,PathCost,Lat2,Long2,Distance,Azimuth : float64;
begin
   if (DEMNowDoing in [StartAccumulatedCostSurface,StartLeastCostPath]) then begin
      if (DEMNowDoing in [StartLeastCostPath]) then Least_Cost_Path.FigureLeastCostPath(Self,Lat,Long,PathDistance,PathCost)
      else ;  //Least_Cost_Path.AccumulatedCostSurface(Lat,Long,'','');
      BackToWandering;
   end;


   if (DBEditting <> 0) and (DEMNowDoing in [InsertDBPoint]) then begin
      GISdb[DBEditting].MyData.CopyRecordToEndOfTable;
      GISdb[DBEditting].MyData.Last;
      GISdb[DBEditting].MyData.Edit;
      GISdb[DBEditting].MyData.SetFieldByNameAsFloat(GISdb[DBEditting].LatFieldName,Lat);
      GISdb[DBEditting].MyData.SetFieldByNameAsFloat(GISdb[DBEditting].LongFieldName,Long);
      GISdb[DBEditting].MyData.Post;
      GISdb[DBEditting].DisplayTheRecord(GISdb[DBEditting].MyData.FiltRecsInDB,true,true);
   end;

   if (DBEditting <> 0) and (DEMNowDoing in [EditPointDBRecs {$IfDef ExMilicons}{$Else},EditMilIcons{$EndIf}]) then  begin
      GISdb[DBEditting].IdentifyRecord(LastX,LastY,Lat,Long,RecsFound,false,false,FeatName,false);
      if (RecsFound = 1) then begin
         if (DEMNowDoing = EditPointDBRecs) then begin
            GISdb[DBEditting].ValidLatLongFromTable(Lat,Long);
            MDDef.aSym.DrawingSymbol := tDrawingSymbol(GISdb[DBEditting].MyData.GetFieldByNameAsInteger('SYM_TYPE'));
            MDDef.aSym.Color := ConverttColorToPlatformColor(GISdb[DBEditting].MyData.GetFieldByNameAsInteger('SYM_COLOR'));
            MDDef.aSym.Size := GISdb[DBEditting].MyData.GetFieldByNameAsInteger('SYM_SIZE');
         end;
      end;
    end;

   {$IfDef ExRedistrict}
   {$Else}
      if (DEMNowDoing = RecolorRedistrict) then begin
         CheckThisPoint(LastX,LastY,xDEMg1,yDEMg1,xSATg1,ySATg1,CheckNothing);
         MapDraw.ScreenToLatLongDegree(LastX,LastY,Lat,Long);
         GISdb[RedistrictForm.DBonTable].IdentifyRecord(LastX,LastY,Lat,Long,RecsFound,false,false,FeatName);
         GISdb[RedistrictForm.DBonTable].MyData.ApplyFilter('');
         if not RedistrictForm.CheckBox1.Checked then  RedistrictForm.DistrictsChanged;
      end;
   {$EndIf}

   if DEMNowDoing in [GetPointSymbols,PlottingPointElevations{$IfDef ExGeology}{$Else},GetGeologySymbols{$EndIf}] then begin
      CheckThisPoint(LastX,LastY,xDEMg1,yDEMg1,xsatg1,ysatg1,CheckReasonable);
      if (MapDraw.VectorIndex <> 0) then begin
         Lat := yDEMg1;
         Long := xDEMg1;
      end
      else begin
         if MapDraw.ValidDEMonMap then DEMGlb[MapDraw.DEMonMap].DEMGridToLatLongDegree(xDEMg1,yDEMg1,Lat,Long)
         else MapDraw.DataGridToLatLongDegree(xsatg1,ysatg1,Lat,Long);
      end;
      AddPointToDB(Lat,Long);
   end;

   if ShapeFileDigitizingUnderway(DEMNowDoing) then begin
      dbAddRecForm.ShapeFileCreator.ProcessShapeDigitization(Self);
      if (DEMNowDoing = ShapeFirstLine) or (DEMNowDoing = ShapeFirstPolygon) then begin
         MapDraw.ScreenToLatLongDegree(LastX,LastY,gbLatStart,gbLongStart);
      end;
   end;

   if (DEMNowDoing in [ShapePoint,ShapeXYZPoint,ShapeTrack]) then begin
      ScreenSymbol(Image1.Canvas,LastX,LastY,MDDef.DefGISSymbol);
      if CreateTrainingSet and ValidSatImage(MapDraw.SATonMap) then begin
         MapDraw.ScreenToDataGrid(LastX,LastY,xg,yg) ;
         SatImage[MapDraw.SATonMap].SatGridToLatLongDegree(SatImage[MapDraw.SatOnMap].BandForSize,round(xg),round(yg),Lat,Long);
      end
      else MapDraw.ScreenToLatLongDegree(LastX,LastY,lat,long);
      dbAddRecForm.AddPointRecord(Lat,Long,(DEMNowDoing in [ShapeTrack]));
   end;

   if (DEMNowDoing in [FirstTimeSeries]) then begin
      MapScreenX1 := LastX;
      MapScreenY1 := LastY;
      ChangeDEMNowDoing(SecondTimeSeries);
   end
   else if (DEMNowDoing in [SecondTimeSeries]) then  begin
       if (DBEditting <> 0) and (GISdb[DBEditting] <> Nil) then begin
          BackToWandering;
          fName := GISdb[DBEditting].PickField('time series' ,[ftString,ftInteger,ftSmallInt]);

          MapDraw.ScreenToLatLongDegree(LastX,LastY,lat,long);
          GISdb[DBEditting].IdentifyRecord(LastX,LastY,Lat,Long,RecsFound,false,false,TStr,false,true);
          if RecsFound> 0 then begin
            OneRec := StrToFloat(GISdb[DBEditting].MyData.GetFieldByNameAsString(fName));
            GISdb[DBEditting].ClearGISFilter;
            MapDraw.ScreenToLatLongDegree(MapScreenX1,MapScreenY1,lat2,long2);
            GISdb[DBEditting].IdentifyRecord(MapScreenX1,MapScreenY1,lat2,long2,RecsFound,false,false,TStr,false,true);
            if RecsFound> 0 then begin
               OtherRec := StrToFloat(GISdb[DBEditting].MyData.GetFieldByNameAsString(fName));
               PetMath.MinOfPairFirst(OneRec,OtherRec);
               GISdb[DBEditting].MyData.ApplyFilter(fName + '>=' + RealToString(OneRec,-18,-6) + ' AND ' + fName + '<=' + RealToString(OtherRec,-18,-6));
               GISdb[DBEditting].dbOpts.MainFilter := GISdb[DBEditting].MyData.Filter;
               GISdb[DBEditting].dbTablef.ShowStatus;
            end;
          end;
       end;
   end;

   if (DEMNowDoing in [ShapePointsAlongFirstLine]) then begin
      MapScreenX1 := LastX;
      MapScreenY1 := LastY;
      ChangeDEMNowDoing(ShapePointsAlongSecondLine);
   end
   else if (DEMNowDoing in [ShapePointsAlongSecondLine]) then begin
      MapDraw.ScreenToLatLongDegree(LastX,LastY,lat,long);
      MapDraw.ScreenToLatLongDegree(MapScreenX1,MapScreenY1,lat2,long2);
      VincentyCalculateDistanceBearing(Lat2,Long2,Lat,Long,Distance,Azimuth);
      ReadDefault('Point spacing (m)',PointSpacing);

      Npts := round(Distance/PointSpacing);
      Distance := Distance / Npts;
      for i := 0 to Npts do begin
         VincentyPointAtDistanceBearing(Lat2,Long2,i*Distance,Azimuth,Lat,Long);
         dbAddRecForm.AddPointRecord(Lat,Long,true);
      end;

     if AnswerIsYes('Another survey line') then ChangeDEMNowDoing(ShapePointsAlongFirstLine)
     else begin
        DbaddRecForm.CompleteAddingPoints;
        BackToWandering;
     end;
   end;

   if (DEMNowDoing in [ShapePointsAlongFirstLine]) then begin
      MapScreenX1 := LastX;
      MapScreenY1 := LastY;
      ChangeDEMNowDoing(ShapePointsAlongSecondLine);
   end
   else if (DEMNowDoing in [ShapePointsAlongSecondLine]) then begin
      MapDraw.ScreenToLatLongDegree(LastX,LastY,lat,long);
      MapDraw.ScreenToLatLongDegree(MapScreenX1,MapScreenY1,lat2,long2);
      VincentyCalculateDistanceBearing(Lat2,Long2,Lat,Long,Distance,Azimuth);
      ReadDefault('Point spacing (m)',PointSpacing);

      Npts := round(Distance/PointSpacing);
      Distance := Distance / Npts;
      for i := 0 to Npts do begin
         VincentyPointAtDistanceBearing(Lat2,Long2,i*Distance,Azimuth,Lat,Long);
         dbAddRecForm.AddPointRecord(Lat,Long,true);
      end;

     if AnswerIsYes('Another survey line') then ChangeDEMNowDoing(ShapePointsAlongFirstLine)
     else begin
        DbaddRecForm.CompleteAddingPoints;
        BackToWandering;
     end;
   end;

   if (DEMNowDoing in [DragEdit]) then begin
      Image1.Canvas.Pen.Color := clWhite;
      Image1.Canvas.Brush.Color := clWhite;
      Image1.Canvas.Brush.Style := bsSolid;
      Image1.Canvas.Rectangle(LastX,LastY,LastX+DragBitmap.Width,LastY+DragBitmap.Height);
      Image1.Canvas.Draw(LastX,LastY,DragBitmap);
      FreeAndNil(DragBitmap);
      FreeAndNil(SavedMapImage);
      BackToWandering;
   end;
end;

{$EndIf}

{$IfDef ExSatAdvanced}
{$Else}

   procedure AddTMRGBWindows(MapOwner : tMapForm);
   var
     TMTable : tMyData;
     NewMap  : tMapForm;
     r,g,b,rName,gName,bName : integer;
   begin
      {$IfDef RecordSat} WriteLineToDebugFile('AddTMRGBWindows'); {$EndIf}
      TMTable := tMyData.Create(TM_RGB_fname);
      if (TMTable <> Nil) then begin
         TMTable.ApplyFilter( 'SATELLITE = ' + QuotedStr(SatImage[MapOwner.MapDraw.SatOnMap].SatelliteName));
         if VerifyRecordsToUse(TMTable,'NAME','Windows to display') then begin
            TMTable.ApplyFilter('SATELLITE = ' + QuotedStr(SatImage[MapOwner.MapDraw.SatOnMap].SatelliteName) + ' AND USE = ' + QuotedStr('Y'));
            TMTable.First;
            while not TMTable.Eof do begin
               rName := TMTable.GetFieldByNameAsInteger('RED_BAND');
               gName := TMTable.GetFieldByNameAsInteger('GREEN_BAND');
               bName := TMTable.GetFieldByNameAsInteger('BLUE_BAND');
               if (SatImage[MapOwner.MapDraw.SatOnMap].FindTMBand(RName,r) and SatImage[MapOwner.MapDraw.SatOnMap].FindTMBand(gName,g) and SatImage[MapOwner.MapDraw.SatOnMap].FindTMBand(bName,b)) then begin
                  NewMap := nil;
                  MapOwner.CopyMap(NewMap,False,true);
                  NewMap.MapDraw.BaseTitle := SatImage[MapOwner.MapDraw.SatOnMap].SceneTitle + '  ' + TMTable.GetFieldByNameAsString('NAME');
                  NewMap.MapDraw.MapType:= mtSatPickColor;
                  NewMap.MapDraw.SatView.RedBand := r;
                  NewMap.MapDraw.SatView.GreenBand := g;
                  NewMap.MapDraw.SatView.BlueBand := b;
                  NewMap.Closable := true;
                  {$IfDef RecordSat} WriteLineToDebugFile('Drawing ' + NewMap.Caption); {$EndIf}
                  if (NewMap <> Nil) then NewMap.DrawColoredMap1Click(nil);
               end;
               TMTable.Next;
            end;
         end;
         TMTable.Destroy;
      end;
      UpdateMenusForAllMaps;
   end;

{$EndIf}


{$IfDef Ex3D}
{$Else}

procedure TMapForm.Bytedata0tomissing1Click(Sender: TObject);
begin
   Markasmissing1Click(Sender);
end;

procedure TMapForm.Bytes1Click(Sender: TObject);
begin
   ThinDEM1Click(Sender);
end;

procedure TMapForm.DrawPerspective(x,y : integer; TheMode : tPenMode);
var
   Lat,Long,Lat1,Long1,
   Azimuth : float64;
begin
   DEMGlb[MapDraw.DEMonMap].DEMGridToLatLongDegree(xDEMg1,yDEMg1,Lat1,Long1);
   MapDraw.ScreenToLatLongDegree(x,y,Lat,Long);
   VincentyCalculateDistanceBearing(Lat1,Long1,Lat,Long,SectLen,Azimuth);
   if (SectLen < MDdef.PerspOpts.PersFirstProfile) then exit;
   OutlinePerspectiveView(MDdef.PersHFOV,Lat1,Long1,SectLen,Azimuth,TheMode);
end;


procedure TMapForm.OutlinePerspectiveView(HFOV,Lat1,Long1,SectLen,Azimuth : float64; TheMode : tPenMode);
var
   Lat,Long,EndAz  : float64;
   xl,yl,xr,yr      : integer;
begin
   Image1.Canvas.Pen.Mode := TheMode;
   Image1.Canvas.Pen.Color := clRed;
   Image1.Canvas.Pen.Width := 3;
   VincentyPointAtDistanceBearing(Lat1,Long1,MDdef.PerspOpts.PersFirstProfile,Azimuth+HFOV*0.5,Lat,Long);
   MapDraw.LatLongDegreeToScreen(Lat,Long,xr,yr);
   Image1.Canvas.MoveTo(xr,yr);

   VincentyPointAtDistanceBearing(Lat1,Long1,MDdef.PerspOpts.PersFirstProfile,Azimuth-HFOV*0.5,Lat,Long);
   MapDraw.LatLongDegreeToScreen(Lat,Long,xl,yl);
   Image1.Canvas.LineTo(xl,yl);

   Azimuth := Azimuth-HFOV*0.5;
   VincentyPointAtDistanceBearing(Lat1,Long1,SectLen,Azimuth,Lat,Long);
   MapDraw.LatLongDegreeToScreen(Lat,Long,xl,yl);
   Image1.Canvas.LineTo(xl,yl);

   EndAz := Azimuth+HFOV;
   repeat
      Azimuth := Azimuth + 2.5;
      if (Azimuth > EndAz) then Azimuth := EndAz;
      VincentyPointAtDistanceBearing(Lat1,Long1,SectLen,Azimuth,Lat,Long);
      MapDraw.LatLongDegreeToScreen(Lat,Long,xl,yl);
      Image1.Canvas.LineTo(xl,yl);
    until (Azimuth > EndAz-0.005);
   Image1.Canvas.LineTo(xr,yr);
   Image1.Canvas.Pen.Width := 1;
end;


procedure TMapForm.StartTheFlyThrough(WhatStarting : tDEMDoingWhat; Replay : boolean = false; SaveName : PathStr = '');
{$IfDef ExFly}
begin
{$Else}
var
   DEMPersF : TThreeDview;
   z,TargetZ : float32;
   Lat1,Long1,Lat2,Long2,LastLat,LastLong,
   xutm,yutm,Dist,SectLen, xobs,yobs,StartAzimuth,
   Distance,Lat,Long, Azimuth,Pitch   : float64;
   i,j,k,err   : integer;
   LocationMap,DrapeForm : tMapForm;
   Fname        : PathStr;
   FlightLocations, OpenData : tStringList;
   TStr : ShortString;


      procedure SetUpFlightControlIfLiveFlying;
      var
         ObsElevation : float32;
      begin
         FlightControlForm := TFlightControlForm.Create(Application);
         FlightControlForm.Visible := true;
         FlightControlForm.Button4.Enabled := MDDef.PerspOpts.WhichPerspective in [ReflectancePerspective];
         if MDdef.PerspOpts.NapEarth  then begin
            DEMGlb[MapDraw.DEMonMap].GetElevMeters(xDEMg1,yDEMg1,ObsElevation);
            ObsElevation := ObsElevation + MDdef.FlyOptions.FlyHeight;
         end
         else ObsElevation := MDdef.PerspOpts.PerspAbsElev;
         DEMPersF.View3D.ObsElev := ObsElevation;
         FlightControlForm.Edit1.Text := RealToString(MDdef.FlyOptions.FlySceneSeparation,-8,0);
         FlightControlForm.Edit2.Text := RealToString(Azimuth,8,2);
         FlightControlForm.Edit3.Text := RealToString(ObsElevation,8,2);
         FlightControlForm.CheckBox1.Checked := MDdef.PerspOpts.NapEarth;
         DEMPersF.LiveFlying := true;
      end;

         procedure GetFlyThroughExpandedRoute;
         var
            j : integer;
         begin
             {$IfDef RecordFlyRoute} WriteLineToDebugFile('Enter GetFlyThroughExpandedRoute'); WriteStringListToDebugFile(StreamProfileResults); {$EndIf}
             ExpandRoute(MapDraw.DEMonMap,StreamProfileResults,MDdef.FlyOptions.FlySceneSeparation,true,true,false);
             FlightLocations.Clear;
             ReadCoordsLatLongFromStreamProfileResults(StreamProfileResults,0,Lat1,Long1);
             for j := 1 to pred(StreamProfileResults.Count) do begin
                 ReadCoordsLatLongFromStreamProfileResults(StreamProfileResults,j,Lat2,Long2);
                 VincentyCalculateDistanceBearing(Lat1,Long1,Lat2,Long2,Distance,Azimuth);
                 FlightLocations.Add(RealToString(Lat2,12,8) + RealToString(Long2,14,8) + RealToString(Azimuth,8,2));
                 Lat1 := Lat2;
                 Long1 := Long2;
             end;
             {$IfDef RecordFlyRoute} WriteLineToDebugFile('Now lat/long'); WriteStringListToDebugFile(DEMPersF.View3D.FlightLocations); {$EndIf}
         end;

      procedure SetUpDrapeMap;
      var
         i,j : integer;
      begin
         {$IfDef RecordDrape} WriteLineToDebugFile('Drape Perspective underway'); {$EndIf}
         if MDdef.DrapeExactly then with DEMPersF.View3D do begin
            {$IfDef RecordDrape} WriteLineToDebugFile('MDdef.DrapeExactly'); {$EndIf}
            Self.DoFastMapRedraw;
            DrapingMaps[1] := Self;
            DrapeMapUsing := 1;
            CopyImageToBitmap(Image1,DrapedBMP[DrapeMapUsing]);
            for i := 0 to pred(DrapedBMP[DrapeMapUsing].Height) do
               P0[DrapeMapUsing][i] := DrapedBMP[DrapeMapUsing].ScanLine[i];
        end
        else if (MDdef.FlyOptions.NumFlyDrapes = 2) then begin
            {$IfDef RecordDrape} WriteLineToDebugFile('MDdef.NumFlyDrapes = 2'); {$EndIf}
            OpenData := tStringList.Create;
            for i := 1 to MaxDEMDataSets do
               if ValidDEM(i) then OpenData.Add('DEM' + IntegerToString(i,5) + '  ' + DEMGlb[i].AreaName);
            {$IfDef ExSat}
            {$Else}
            for i := 1 to MaxSatAllowed do
               if (SatImage[i] <> Nil) then OpenData.Add('Image' + IntegerToString(i,3) + '  ' + SatImage[i].SceneBaseName);
            {$EndIf}
            {$IfDef RecordFly} WriteLineToDebugFile('Pt 2.3 Target coord:' + RealToString(DEMPersF.View3D.TargetXUTM,10,0) + RealToString(DEMPersF.View3D.TargetYUTM,12,0)); {$EndIf}
            if (OpenData.Count = 1) then begin
               MDdef.FlyOptions.NumFlyDrapes := 1;
               DEMPersF.View3D.SetUpDrapeMap(WhatStarting,self,Distance);
            end
            else for j := 1 to 2 do begin
               k := j;
               GetFromList('Drape' + IntegerToString(j,2),k,OpenData);
               DrapeForm := tMapForm.create(Application);
               TStr := OpenData[pred(k)];
               Val(Copy(TStr,6,3),i,err);
               if (Copy(TStr,1,3) = 'DEM') then DEMGlb[i].SelectionMap.CopyMap(DrapeForm,true,true)
               else begin
                  {$IfDef ExSat}
                  {$Else}
                  SatImage[i].SelectionMap.CopyMap(DrapeForm,true,true);
                  {$EndIf}
               end;
               DEMPersF.View3D.DrapingMaps[j] := DrapeForm;
            end;
         end
         else begin
            {$IfDef RecordDrape} WriteLineToDebugFile('Drop through case'); {$EndIf}
            DEMPersF.View3D.SetUpDrapeMap(WhatStarting,self,Distance);
         end;
      end;

      procedure SetUpLocationMap;
      begin
         LocationMap := Nil;
         CopyMap(LocationMap,false,true);
         LocationMap.Panel1.Visible := false;
         LocationMap.Panel1.Height := 0;
         DEMPersF.ToolBar1.Visible := false;
         DEMPersF.ToolBar1.Height := 0;

         LocationMap.MapDraw.ShowMapMargins := false;
         LocationMap.Caption := 'Fly through route';
         if MDdef.FlyOptions.FlySideBySide then LocationMap.MaxUpperLeftDrawMap(1500,DEMPersF.View3D.FlyOpts.FlyThroughHeight)
         else LocationMap.MaxUpperLeftDrawMap(DEMPersF.View3D.FlyOpts.FlyThroughWidth,1500);

         if MDdef.FlyOptions.ShowFlyThroughRoute then begin
            if (DEMPersf.View3D.FlightRouteDB <> Nil) then begin
               LocationMap.DrawLineFromPointDBOnMap(DEMPersf.View3D.FlightRouteDB,MDDef.FightLineColor,MDDef.FlightLineSize)
            end;
         end;

         {$IfDef RecordFly} WriteLineToDebugFile('Flight map:' + IntToStr(LocationMap.MapDraw.MapXSize) + ' x' + IntToStr(LocationMap.MapDraw.MapYSize));     {$EndIf}
         DEMPersF.PositionMap := LocationMap;
      end;

      procedure ComputeFrameCoordinates;
      begin
         xobs := DEMPersF.View3D.TargetXUTM - distance * sinDeg(Azimuth);
         yobs := DEMPersF.View3D.TargetYUTM - distance * cosDeg(Azimuth);
         DEMGlb[MapDraw.DEMonMap].UTMtoLatLongDegree(xobs,yobs,Lat,Long);
         FlightLocations.Add(StandardLatLongString(Lat,Long) + RealToString(Azimuth,8,2));
      end;

      procedure SetUpCircleFly;
      begin
         {$IfDef RecordCircleAround} WriteLineToDebugFile('SetUpCircleFly in'); {$EndIf}
         StartAzimuth := Azimuth;

         PanoramaOps := tPanoramaOps.Create(Application);
         PanInc := 10;
         PanSize := 360;

         with PanoramaOps do begin
            ComboBox1.Text := ComboBox1.Items[ord(MDdef.PerspOpts.WhichPerspective)];
            Edit1.Text := RealToString(StartAzimuth,-6,0);
            Edit2.Text := RealToString(PanSize,-4,0);
            Edit3.Text := RealToString(PanInc,-4,0);
            ShowModal;
            CheckEditString(Edit1.Text,StartAzimuth);
            CheckEditString(Edit2.Text,PanSize);
            CheckEditString(Edit3.Text,PanInc);
         end;

         FlyRoute := tStringList.Create;
         Azimuth := StartAzimuth;
         if (WhatStarting in [SeekingThirdCircleFly]) then begin
            {$IfDef RecordCircleAround} WriteLineToDebugFile('SeekingThirdCircleFly processing'); {$EndIf}
            DEMGlb[MapDraw.DEMonMap].DEMGridtoUTM(xDEMg1,yDEMg1,XUTM,YUTM);
            Distance := sqrt( sqr(DEMPersF.View3D.TargetXUTM-xutm) + sqr(DEMPersF.View3D.TargetYUTM-yutm));
            if (PanoramaOps.RadioGroup1.ItemIndex = 0) then begin
               while Azimuth  < StartAzimuth + PanSize do begin
                  ComputeFrameCoordinates;
                  Azimuth := Azimuth + PanInc;
               end;
            end
            else begin
               while (Azimuth  > StartAzimuth - PanSize) do begin
                  ComputeFrameCoordinates;
                  Azimuth := Azimuth - PanInc;
               end;
            end;
            if (Distance < PersViewDepth / 2) then Distance := PersViewDepth - Distance;
            if (Distance < PersViewDepth - Distance) then Distance := PersViewDepth - Distance;
            DEMPersF.View3D.ViewAzimuth := 0;
            if (PanoramaOps.ComboBox1.ItemIndex = 3) then DEMPersF.View3D.SetUpDrapeMap(WhatStarting,self,Distance);
         end;
         PanoramaOps.Free;
         {$IfDef RecordCircleAround} WriteLineToDebugFile('SetUpCircleFly out'); {$EndIf}
      end;

begin
   {$IfDef RecordFly} WriteLineToDebugFile('Start StartTheFlyThrough with flydepth=' + IntToStr(MDDef.PerspOpts.FlyDepth)); {$EndIf}
   DEMPersF := Nil;

   DEMNowDoing := Calculating;
   PersViewDepth := MDdef.PerspOpts.FlyDepth;

   DEMPersF := TThreeDview.Create(Application);
   DEMPersF.View3D := tView3D.Create;
   DEMPersF.View3D.InitializeViewParameters;

   DEMPersF.OwningMap := self;
   DEMPersF.View3D.DEMonView := MapDraw.DEMonMap;

   if (WhatStarting = JustWandering) then begin
      FName := DEMdefs.MovieDir;
      if not GetFileFromDirectory('Flight route to replay',DefaultDBMask,FName) then begin
         DEMPersF.Close;
         DEMPersF := Nil;
         exit;
      end;
      StreamProfileResults := tStringList.Create;
      DEMPersf.View3D.FlightDBName := fName;
      DEMPersf.FlightPathGraphsPossible := true;
      DEMPersf.View3D.FlightRouteDB := tMyData.Create(fName);

      DEMPersf.View3D.FlightRouteDB.ApplyFilter( 'USE <> ' + QuotedStr('N'));
      DEMPersf.View3D.ViewerLat := DEMPersf.View3D.FlightRouteDB.GetFieldByNameAsFloat('LAT');
      DEMPersf.View3D.ViewerLong := DEMPersf.View3D.FlightRouteDB.GetFieldByNameAsFloat('LONG');
      for i := 0 to 10 do StreamProfileResults.Add('');
   end
   else begin
      DEMGlb[MapDraw.DEMonMap].DEMGridToLatLongDegree(xDEMg1,yDEMg1,DEMPersF.View3D.ViewerLat,DEMPersF.View3D.ViewerLong);
      DEMGlb[MapDraw.DEMonMap].DEMGridToLatLongDegree(xDEMg2,yDEMg2,DEMPersF.View3D.ViewedLat,DEMPersF.View3D.ViewedLong);
      if WhatStarting in [SeekingSecondPerspective,LiveFly2,SeekingSecondNewPanorama,SeekingThirdCircleFly] then begin
         VincentyCalculateDistanceBearing(DEMPersF.View3D.ViewerLat,DEMPersF.View3D.ViewerLong,DEMPersF.View3D.ViewedLat,DEMPersF.View3D.ViewedLong,PersViewDepth,Azimuth);
         MDdef.PerspOpts.FlyDepth := round(PersViewDepth);
         DEMPersF.View3D.ViewAzimuth := Azimuth;
      end;
   end;

   FlightLocations := tStringList.Create;

   {$IfDef RecordFly}
      WriteLineToDebugFile('Viewer: ' + LatLongDegreeToString(DEMPersF.View3D.ViewerLat,DEMPersF.View3D.ViewerLong,MDDef.OutputLatLongMethod));
      WriteLineToDebugFile('Viewed: ' + LatLongDegreeToString(DEMPersF.View3D.ViewedLat,DEMPersF.View3D.ViewedLong,MDDef.OutputLatLongMethod));
      WriteLineToDebugFile('View toward: ' + RealToString(DEMPersF.View3D.ViewAzimuth,-8,2));
   {$EndIf}

   if TargetFlyThrough or (WhatStarting in [SeekingThirdCircleFly]) then begin
      DEMGlb[MapDraw.DEMonMap].DEMGridtoUTM(xDEMg3,yDEMg3,DEMPersF.View3D.TargetXUTM,DEMPersF.View3D.TargetYUTM);
      DEMGlb[MapDraw.DEMonMap].GetElevMeters(xDEMg3,yDEMg3,TargetZ);
   end;

   if (WhatStarting in [JustWandering,SeekingSecondPerspective,LiveFly2,SeekingFlyThroughRoute,SeekingThirdCircleFly]) then begin
      if WhatStarting in [SeekingSecondPerspective,LiveFly2] then begin
         FlightLocations.Add(RealToString(DEMPersF.View3D.ViewerLat,12,8) +
              RealToString(DEMPersF.View3D.ViewerLong,14,8) + RealToString(Azimuth,8,2));
      end;
      DEMPersF.View3D.MaxPitch := MDdef.PersVFOV * 0.5;
      DEMPersF.View3D.MinPitch := MDdef.PersVFOV * -0.5;
      DEMPersF.View3D.MaxRange := PersViewDepth;
      DEMPersF.View3D.MinRange := 0;
      DEMPersF.View3D.ForceElevAngle := false;
      DEMPersf.FlightPathGraphsPossible := (WhatStarting in [SeekingFlyThroughRoute,JustWandering]);
  end;

  DEMPersF.View3D.ViewDepth := PersViewDepth;

   case WhatStarting of
      SeekingSecondNewPanorama         : DEMPersF.View3D.PersSize := psPanorama;
      SeekingThirdCircleFly,
      JustWandering,
      LiveFly2,SeekingFlyThroughRoute  : DEMPersF.View3D.PersSize := psFlying;
      SeekingSecondPerspective         : DEMPersF.View3D.PersSize := psPerpsective;
   end;
   DEMPersF.View3D.SetSize;
   SetPerspectiveOptions(DEMPersF.View3D,DEMPersF.View3D.PersSize);
   DEMPersF.View3D.SetSize;

   if (WhatStarting = LiveFly2) then SetUpFlightControlIfLiveFlying;
   if (WhatStarting in [JustWandering]) then begin
      for j := 2 to pred(StreamProfileResults.Count) do FlightLocations.Add(StreamProfileResults[j]);
   end;
   if (WhatStarting = SeekingThirdCircleFly) then SetUpCircleFly;

   if (WhatStarting in [SeekingFlyThroughRoute]) then begin
      GetFlyThroughExpandedRoute;
      if (WhatStarting in [LiveFly2,SeekingFlyThroughRoute]) and (DEMPersf.View3D.FlightRouteDB = Nil) then begin
         DEMPersF.View3D.FlightDBName := Petmar.NextFileNumber(MDTempDir, 'fly',DefaultDBExt);
         CreateLatLongZTable(DEMPersF.View3D.FlightDBName,true,false,true,true,true,false,true);
         DEMPersf.View3D.FlightRouteDB := tMyData.Create(DEMPersF.View3D.FlightDBName);
         Dist := 0;
         for i := 0 to pred(FlightLocations.Count) do begin
            DEMPersf.View3D.FlightRouteDB.Insert;
            ReadCoordsLatLongFromStreamProfileResults(FlightLocations,i,Lat,Long);
            DEMPersf.View3D.FlightRouteDB.SetFieldByNameAsFloat('LAT',Lat);
            DEMPersf.View3D.FlightRouteDB.SetFieldByNameAsFloat('LONG',Long);
            DEMPersf.View3D.FlightRouteDB.SetFieldByNameAsFloat('AZIMUTH',StrToFloat(Copy(FlightLocations.Strings[i],27,8)));
            if DEMGlb[MapDraw.DEMonMap].GetElevFromLatLongDegree(Lat,Long,z) then begin
               DEMPersf.View3D.FlightRouteDB.SetFieldByNameAsFloat('Z',z);
               if MDDef.PerspOpts.NapEarth then z := z + MDDef.PerspOpts.PersObsUp
               else z := MDDef.PerspOpts.PerspAbsElev;
               DEMPersf.View3D.FlightRouteDB.SetFieldByNameAsFloat('ALTITUDE',z);
            end;
            if (i=0) then begin
               DEMPersf.View3D.ViewerLat := Lat;
               DEMPersf.View3D.ViewerLong := Long;
            end else begin;
               VincentyCalculateDistanceBearing(Lat,Long,LastLat,LastLong,SectLen,Azimuth);
               Dist := Dist + 0.001 * SectLen;
            end;
            DEMPersf.View3D.FlightRouteDB.SetFieldByNameAsFloat('DISTANCE',Dist);
            DEMPersf.View3D.FlightRouteDB.Post;
            LastLat := Lat;
            LastLong := Long;
         end;
      end;
   end;

   DEMPersF.CanModifyParameters := (not TargetFlyThrough);

   {$IfDef RecordFly}
     WriteLineToDebugFile('mid StartTheFlyThrough with ' + IntToStr(MDDef.PerspOpts.FlyDepth) + ' Target:' + RealToString(DEMPersF.View3D.TargetXUTM,10,0) + RealToString(DEMPersF.View3D.TargetYUTM,12,0));
     WriteLineToDebugFile('Selected points');
     WriteStringListToDebugFile(StreamProfileResults);
     WriteLineToDebugFile('Flight route');
   {$EndIf}

   if (WhatStarting in [SeekingSecondNewPanorama]) then begin
      Pitch := 0.5*(DEMPersF.View3D.MaxPitch + DEMPersF.View3D.MinPitch);
      SetUpPanoramaView(DEMPersF,DEMPersF.View3D.ViewerLat,DEMPersF.View3D.ViewerLong,MDDef.PerspOpts.PersObsUp,
           PersViewDepth,180,DEMPersF.View3D.ViewHFOV,DEMPersF.View3D.ViewVFOV,Pitch,MapDraw.DEMOnMap,'',
           DEMPersF.View3D.PersOpts.WhichPerspective = BMPPerspective,WhatStarting,Self);
      if (SaveName <> '') then begin
         PetImage.SaveImageAsBMP(DEMPersF.Image1,SaveName);
         DemPersf.Close;
         DEMPersf := Nil;
      end;
   end
   else begin
      DEMPersF.View3D.TargetRun := TargetFlyThrough;
      if ((WhatStarting = LiveFly2) or (FlightLocations.Count > 1))and MDdef.FlyOptions.ShowFlyThroughRoute then SetUpLocationMap;
      if (StreamProfileResults <> Nil) then FreeAndNil(StreamProfileResults);
      FlightLocations.Free;
      DEMPersF.SetFocus;
      TargetFlyThrough := false;
      BackToWandering;
      ShowSatProgress := false;
      DEMPersF.SetPerspectiveWindowSize;
      if (DEMPersF.View3D.PersOpts.WhichPerspective = BMPPerspective) then SetUpDrapeMap;
      DEMPersF.PerspectiveView;
   end;
   {$IfDef RecordFly} WriteLineToDebugFile('end StartTheFlyThrough with flydepth=' + IntToStr(MDDef.PerspOpts.FlyDepth)); {$EndIf}
{$EndIf}
end;


function ValidElevationRequired : boolean;
begin
   Result := (DEMNowDoing in [SeekingLOS,SeekingSecondLOS,SeekingSecondPerspective,SeekingSecondNewPanorama,SeekingSecondCircleFly,SeekingThirdCircleFly,LiveFly2,
        SeekingPerspective,SeekingFirstNewPanorama,LiveFly, SeekingFirstCircleFly,FirstPointSelectionAlgorithm,SeekingAverageProfile,SeekingSecondAverageProfile,
        PlottingPointElevations,SeekingSecondPerspective,
        FirstSlopePoint,SecondSlopePoint,CompareFanAlgorithms,FanSensitivity,
        FirstRequiredAntenna,
        SeekingFlyThroughRoute,SeekingSecondLOS,
        SeekingAverageProfile,SeekingSecondAverageProfile,
        SeekingPerspective,SeekingLOS,SeekingStreamProfile,
        SeekingFirstNewPanorama,SeekingSecondNewPanorama,
        QuickWeaponsFan,EditWeaponsFans,GrainByRegionSize,GetPointFabric,
        Calculating,CalculateVolume,
        EditFlightPathOnMap,SeekingFirstCircleFly,SeekingSecondCircleFly,SeekingThirdCircleFly,
        RouteObservation,LiveFly,LiveFly2,
        {$IfDef ExGeology}{$Else} SeekingLeftSideMagModels,SeekingRightSideMagModels,ProjectFocalMechToSurface,SeekingFirstThreePoint,SeekingSecondThreePoint,SeekingThirdThreePoint,SeekingPlaneContact,{$EndIf}
        {$IfDef ExDrainage}{$Else} FloodBasin,{$EndIf}
        {$IfDef ExFresnel} {$Else} FirstFresnelPoint,SecondFresnelPoint, {$EndIf}
        FindBlockHorizon,
        MultipleLOS,MultipleTopoProfileRight,SeekingTopoProfile,SimpleTopoProfileRight]);
end;


procedure tMapForm.ThreeDCheckDblClick(NotSamePoint : boolean);
var
   xSatg3,ysatg3,z : float32;
   Lat1,Long1,Lat2,Long2,Heading : float64;
   Finalx,Finaly : integer;
   Checking      : tCheckPoint;
begin
   if (DEMNowDoing in [SeekingSecondLOS,SeekingSecondPerspective,SeekingSecondNewPanorama,MultipleTopoProfileRight,SimpleTopoProfileRight,SeekingSecondAverageProfile,
            {$IfDef ExGeology} {$Else} SeekingRightSideMagModels, {$EndIf}
            SeekingSecondCircleFly,SeekingThirdCircleFly,LiveFly2]) and NotSamePoint then begin
      if (DEMNowDoing in [SeekingThirdCircleFly]) then begin
         CheckThisPoint(LastX,LastY,xDEMg3,yDEMg3,xSatg3,ysatg3,CheckAll);
         if ValidElevationRequired and (not DEMGlb[MapDraw.DEMonMap].GridInDataSet(xDEMg3,yDEMg3)) then begin
            MessageToContinue(NoDEMCovers);
            exit;
         end;
      end
      else begin
         if DEMNowDoing in [SeekingSecondPerspective,SeekingSecondNewPanorama,SeekingSecondCircleFly,LiveFly2] then Checking := CheckAll
         else Checking := CheckReasonable;

         CheckThisPoint(LastX,LastY,xDEMg2,yDEMg2,xsatg2,ysatg2,Checking);
         MapDraw.DEMGridToScreen(xDEMg2,yDEMg2,Finalx,Finaly);
         if ValidElevationRequired and (not DEMGlb[MapDraw.DEMonMap].GridInDataSet(xDEMg2,yDEMg2)) then begin
            MessageToContinue(NoDEMCovers);
            exit;
         end;
         if DEMNowDoing in [SeekingSecondCircleFly] then begin
            MDdef.PerspOpts.FlyDepth := round(DEMGlb[MapDraw.DEMonMap].DistanceMetersBetweenPoints(xDEMg1,yDEMg1,xDEMg2,yDEMg2,Heading));
         end;
      end;

      {$IfDef ExViewshed}
      {$Else}
      if (DEMNowDoing in [SeekingSecondLOS,MultipleTopoProfileRight,{$IfDef ExGeology}{$Else} SeekingRightSideMagModels,{$EndIf} SimpleTopoProfileRight,SeekingSecondAverageProfile]) then begin
         Image1.Canvas.Pen.Mode := pmCopy;
         Image1.Canvas.Pen.Color := clRed;
         Image1.Canvas.MoveTo(MapScreenX1,MapScreenY1);
         Image1.Canvas.LineTo(Finalx,Finaly);

         DEMGlb[MapDraw.DEMonMap].DEMGridToLatLongDegree(xDEMg1,yDEMg1,Lat1,Long1);
         DEMGlb[MapDraw.DEMonMap].DEMGridToLatLongDegree(xDEMg2,yDEMg2,Lat2,Long2);
         StartLOS(True,DEMNowDoing,MapDraw.DEMonMap,Lat1,Long1,Lat2,Long2,Self);
         if (DEMNowDoing in [SeekingSecondAverageProfile]) then begin
            ChangeDEMNowDoing(SeekingAverageProfile);
            exit;
         end;
      end;
      {$EndIf}

      if DEMNowDoing in [SeekingSecondCircleFly] then begin
         ChangeDEMNowDoing(SeekingThirdCircleFly);
         exit;
      end;

      if (DEMNowDoing in [SeekingSecondPerspective,SeekingSecondNewPanorama,SeekingThirdCircleFly,LiveFly2]) then StartTheFlyThrough(DEMNowDoing);
      if (DEMNowDoing in [SeekingSecondPerspective]) then DrawPerspective(LastX,LastY,pmCopy);
      BackToWandering;
   end;

   if DEMNowDoing in [SeekingLOS,SeekingPerspective,SeekingFirstNewPanorama,LiveFly,MultipleLOS,SeekingTopoProfile,SimpleTopoProfileRight,SeekingAverageProfile,
           SeekingFirstCircleFly,FirstPointSelectionAlgorithm] then begin
      if DEMNowDoing in [{SeekingPerspective,}LiveFly,SeekingFirstCircleFly] then Checking := CheckAll
      else Checking := CheckReasonable;
      CheckThisPoint(LastX,LastY,xDEMg1,yDEMg1,xsatg1,ysatg1,Checking);

      if ValidElevationRequired and (not DEMGlb[MapDraw.DEMonMap].GetElevMeters(xDEMg1,yDEMg1,z)) then begin
         MessageToContinue(NoDEMCovers);
         exit;
      end;

      MapScreenX1 := LastX;
      MapScreenY1 := LastY;
      newx1 := LastX;
      newy1 := LastY;

      case DEMNowDoing of
         LiveFly : ChangeDEMNowDoing(LiveFly2);
         SeekingLos : ChangeDEMNowDoing(SeekingSecondLOS);
         MultipleLOS : ChangeDEMNowDoing(MultipleTopoProfileRight);
         SeekingTopoProfile : ChangeDEMNowDoing(SimpleTopoProfileRight);
         FirstPointSelectionAlgorithm : ChangeDEMNowDoing(SecondPointSelectionAlgorithm);
         SeekingFirstNewPanorama : ChangeDEMNowDoing(SeekingSecondNewPanorama);
         SeekingFirstCircleFly : ChangeDEMNowDoing(SeekingSecondCircleFly);
         SeekingAverageProfile : ChangeDEMNowDoing(SeekingSecondAverageProfile);
         else ChangeDEMNowDoing(SeekingSecondPerspective);
      end;
   end;

end;


{$EndIf}


{$IfDef ExViewshed}
{$Else}

procedure TMapForm.SetUpAmbush;
var
   Bitmap     : tMyBitmap;
   WeaponsFan : tWeaponsFan;
   i          : integer;
   AmbushFansTable : tMyData;
   fName : PathStr;
   Seg,TotalLength : float64;
begin
   {$IfDef RecordAmbush} WriteLineToDebugFile('TMapForm.SetUpAmbush enter'); WriteStringListToDebugFile(StreamProfileResults); {$EndIf}
   SafeMakeDir(MainMapData + 'images\fans\');
   InitializeWeaponsFan(WeaponsFan);
   with MapDraw do begin
       TotalLength := 0;
       for i := 3 to pred(StreamProfileResults.Count) do begin
          Seg := StrToFloat(Copy(StreamProfileResults.Strings[i],36,10));
          TotalLength := TotalLength + Seg;
       end;

       if not GetWeaponParameters(DEMonMap,WeaponsFan,false,true,Nil,false,true,TotalLength) then exit;
       DoFastMapRedraw;

       CloneImageToBitmap(Image1,Bitmap);
       ExpandRoute(MapDraw.DEMonMap,StreamProfileResults,MDDef.WeaponRouteSeparation,true,true,false);
       {$IfDef RecordAmbush} WriteLineToDebugFile('TMapForm.SetUpAmbush after expand route'); WriteStringListToDebugFile(StreamProfileResults); {$EndIf}
       fName := NextFileNumber(SaveViewshedDir, 'ambush',DefaultDBExt);
       CreateTableOfType(fName,'WeaponsFan');
       AmbushFansTable := tMyData.Create(fName);
       for i := 0 to pred(StreamProfileResults.Count) do with WeaponsFan do begin
          ReadCoordsLatLongFromStreamProfileResults(StreamProfileResults,i,w_Lat,w_Long);
          AmbushFansTable.Insert;
          AmbushFansTable.SetFieldByNameAsString('NAME','Ambush_position_' + IntToStr(succ(I)));
          AmbushFansTable.SetFieldByNameAsFloat('LAT',w_lat);
          AmbushFansTable.SetFieldByNameAsFloat('LONG',w_long);
          AmbushFansTable.SetFieldByNameAsFloat('SENSOR_UP',W_Up);
          AmbushFansTable.SetFieldByNameAsFloat('TARGET_UP',W_TargetUp);
          AmbushFansTable.SetFieldByNameAsFloat('SENSOR_RNG',W_Range);
          AmbushFansTable.SetFieldByNameAsFloat('MIN_RNG',MDDef.DefWeaponsMinRange);
          AmbushFansTable.SetFieldByNameAsFloat('LEFT_AZ',StartAngle);
          AmbushFansTable.SetFieldByNameAsFloat('RIGHT_AZ',EndAngle);
          AmbushFansTable.SetFieldByNameAsFloat('MAX_VERT',UpAngle);
          AmbushFansTable.SetFieldByNameAsFloat('MIN_VERT',DownAngle);
          AmbushFansTable.SetFieldByNameAsString('USE','Y');
          AmbushFansTable.Post;
       end;

      {$IfDef RecordAmbush} WriteLineToDebugFile('Call InsureAllFansDrawn'); {$EndIf}

      SaveBackupDefaults;
      MDDef.ShowViewshedMixedPixels := false;
      InsureAllFansDrawn(AmbushFansTable,Bitmap);
      AmbushFansTable.Destroy;

      {$IfDef RecordAmbush} WriteLineToDebugFile('Load DB ' + fName): {$EndIf}
      CurrentFansTable := LoadDataBaseFile(fName);
      fName := SaveViewShedDir + 'route_composite' + OverlayFExt;
      PetImage.SaveBitmap(Bitmap,fName);
      MapDraw.WriteMapsWorldFile(fName);

      FName := SaveViewShedDir + 'num_fans' + OverlayFExt;
      MapDraw.ComputeMultiSensorCoverage(FName,AmbushFansTable);
      MapDraw.AllFansCoverageFName := fName;
      Bitmap := PetImage.LoadBitmapFromFile(fName);

      RestoreBackupDefaults;
      FreeAndNil(StreamProfileResults);
      IHSmergeOntoMap(Bitmap);
      if CurrentFansTable <> 0 then begin
         GISdb[CurrentFansTable].dbTableF.Panel1.Visible := true;
         GISdb[CurrentFansTable].ItsFanFile := true;
      end;
   end;
   {$IfDef RecordAmbush} WriteLineToDebugFile('TMapForm.SetUpAmbush exit'); {$EndIf}
end;


procedure tMapForm.EditAWeaponsFan(Sender : tObject);
begin
    if (MapDraw.CurrentFansTable <> 0) and (GISdb[MapDraw.CurrentFansTable].dbTablef = Nil) then begin
        GISdb[MapDraw.CurrentFansTable].DisplayTable;
        GISdb[MapDraw.CurrentFansTable].ItsFanFile := true;
        GISdb[MapDraw.CurrentFansTable].dbTablef.CheckBox1.Checked := true;
        GISdb[MapDraw.CurrentFansTable].dbTablef.Panel1.Visible := true;
        GISdb[MapDraw.CurrentFansTable].dbTablef.Height := 250;
        GISdb[MapDraw.CurrentFansTable].dbTablef.Width := 400;
        GISdb[MapDraw.CurrentFansTable].dbTablef.FormStyle := fsStayOnTop;
        GISdb[MapDraw.CurrentFansTable].dbTablef.Visible := true;
        GISdb[MapDraw.CurrentFansTable].dbTablef.Caption := 'Edit Weapons fans';
        GISdb[MapDraw.CurrentFansTable].dbTablef.BitBtn8.Visible := false;
    end;
end;


{$IfDef ExAdvancedGIS}
{$Else}
procedure TMapForm.RequiredAntennaMap(Lat,Long : float64);
var
   Lat2,Long2 : float64;
begin
   Ant_hts_ops.GetRequiredAntennaOptions;
   if MDDef.DoReqAntHigh or MDDef.DoReqFlyHigh or MDDef.DoGrazingAngle or MDDef.DoEarthCurvature then begin
      MakeRequiredAntennaMap('Antennas',MapDraw.DEMonMap,Lat,Long,MDDef.ObsAboveGround,MDdef.MaskObsRange,true,0,360);
      if MDDef.DoLOSProfile then begin
         VincentyPointAtDistanceBearing(Lat,Long,MDdef.MaskObsRange,90,Lat2,Long2);
         StartLOS(True,JustWandering,MapDraw.DEMonMap,Lat,Long,Lat2,Long2,Self,true);
      end;
   end;
end;
{$EndIf}


procedure TMapForm.Restorerangecircles1Click(Sender: TObject);
begin
   {$IfDef ExAdvancedGIS}
   {$Else}
      if GetFileFromDirectory('File with saved range circles',DBNameMask,MapDraw.RangeCirclesFName) then DoFastMapRedraw;
   {$EndIf}
end;


procedure TMapForm.RGBgridfillholes1Click(Sender: TObject);
begin
   {$IfDef ExAdvancedGIS}
   {$Else}
      DEMGlb[MapDraw.DEMonMap].RGBfilterDEM(1,true);
   {$EndIf}
end;


procedure TMapForm.CheckViewShedMapDblClick(NotSamePoint : boolean);
var
   Lat,Long : float64;
   z1 : float32;
   WeaponsFan  : tWeaponsFan;
   fName : PathStr;
begin
   {$IfDef ExAdvancedGIS}
   {$Else}
   if (DEMNowDoing = FirstRequiredAntenna) then begin
      CheckThisPoint(LastX,LastY,xDEMg1,yDEMg1,xSATg1,ySATg1,CheckReasonable);
      DEMGlb[MapDraw.DEMonMap].DEMGridToLatLongDegree(xDEMg1,yDEMg1,Lat,Long);
      RequiredAntennaMap(Lat,Long);
      ChangeDEMNowDoing(JustWandering);
      exit;
   end;
   {$EndIf}

   {$IfDef ExFresnel}
   {$Else}
      if (DEMNowDoing = FirstFresnelPoint) then begin
         CheckThisPoint(LastX,LastY,xDEMg1,yDEMg1,xSATg1,ySATg1,CheckReasonable);
         DEMGlb[MapDraw.DEMonMap].DEMGridToLatLongDegree(xDEMg1,yDEMg1,Lat,Long);
         Fresnel_Block_Form.FresnelBlockage(Self,True,Lat,Long);
         ChangeDEMNowDoing(SecondFresnelPoint);
         exit;
      end;

      if (DEMNowDoing = SecondFresnelPoint) then begin
         {$IfDef RecordFresnel} WriteLineToDebugFile('Starting (DEMNowDoing = SecondFresnelPoint'); {$EndIf}
         CheckThisPoint(LastX,LastY,xDEMg1,yDEMg1,xSATg1,ySATg1,CheckReasonable);
         DEMGlb[MapDraw.DEMonMap].DEMGridToLatLongDegree(xDEMg1,yDEMg1,Lat,Long);
         Fresnel_Block_Form.FresnelBlockage(Self,False,Lat,Long);
         {$IfDef RecordFresnel} WriteLineToDebugFile('done (DEMNowDoing = SecondFresnelPoint'); {$EndIf}
         exit;
      end;
   {$EndIf}

   if DEMNowDoing in [QuickWeaponsFan,CompareFanAlgorithms,FanSensitivity] then begin
      {$IfDef RecordFan} WriteLineToDebugFile('Starting Weapons Fan'); {$EndIf}
      CheckThisPoint(LastX,LastY,xDEMg1,yDEMg1,xsatg1,ysatg1,CheckReasonable);
      DEMGlb[MapDraw.DEMonMap].DEMGridToLatLongDegree(xDEMg1,yDEMg1,Lat,Long);

      if (MDDef.FanPickMode in [fpMultipleSame,fpMultipleAsk]) then begin
         ScreenSymbol(Image1.Canvas,LastX,LastY,MDDef.DefGISSymbol);
      end;

      if not DEMGlb[MapDraw.DEMonMap].GetElevMeters(xDEMg1,yDEMg1,z1) then begin
         MessageToContinue(NoDEMCovers);
         exit;
      end;
      {$IfDef RecordFan} WriteLineToDebugFile('Fan computed at ' + LatLongDegreeToString(Lat,Long) ); {$EndIf}

      InitializeWeaponsFan(WeaponsFan);
      WeaponsFan.W_Lat := Lat;
      WeaponsFan.W_Long := Long;

      if FirstFan or (MDDef.FanPickMode in [fpSingle,fpMultipleAsk]) then begin
         if not GetWeaponParameters(MapDraw.DEMonMap,WeaponsFan,false,false,Nil,true,true) then begin
            {$IfDef RecordFan} WriteLineToDebugFile('Failed getting parameters'); {$EndIf}
            exit;
         end;
         FirstFan := false;
      end;

      {$IfDef FanDrawProblems}
          MDdef.BlankMapColor := claWhite;
          MDDef.FanOpacity := 100;
          MDDef.wf.MaskRaySpacingDeg := 2;
          WeaponsFan.W_Up := 25;
          if MapDraw.DEMMap then MapDraw.MapType := mtDEMBlank else MapDraw.MapType := mtSatBlank;
          DoCompleteMapRedraw;
      {$EndIf}

      if DEMNowDoing in [CompareFanAlgorithms] then DrawMultipleFanMethods(WeaponsFan)
      else if DEMNowDoing in [FanSensitivity] then CompareFanSensitivityAnalysis(WeaponsFan)
      else begin
         if (MapDraw.CurrentFansTable = 0) then begin
            fName := NextFileNumber(CurrentProject, 'Fans',DefaultDBExt);
            CreateTableOfType(fName,'WeaponsFan');
            ZeroRecordsAllowed := true;
            OpenNumberedGISDataBase(MapDraw.CurrentFansTable,fName,false,false,Self);
            {$IfDef RecordFan} WriteLineToDebugFile('CheckViewShedMapDblClick Opened new fan table=' + fName);      {$EndIf}
         end;
         MapDraw.AddFanToMap(WeaponsFan);
         {$IfDef RecordFan} WriteLineToDebugFile('CheckViewShedMapDblClick added to map at ' + LatLongDegreeToString(WeaponsFan.W_Lat,WeaponsFan.W_Long)); {$EndIf}
         if MDDef.DrawRangeCircles then begin
            {$IfDef RecordFan} WriteLineToDebugFile('Drawing range circles'); {$EndIf}
            AddRangeCirclesAtLocation(WeaponsFan.W_Lat,WeaponsFan.W_Long);
         end;
         AddOrSubtractOverlay(Self,ovoFans,true);
         DoFastMapRedraw;
         {$IfDef RecordFan} WriteLineToDebugFile('CheckViewShedMapDblClick Map redrawn, at ' + LatLongDegreeToString(WeaponsFan.W_Lat,WeaponsFan.W_Long)); {$EndIf}
      end;
      CheckProperTix;
      exit;
   end;

   if (DEMNowDoing = SecondPointSelectionAlgorithm) then begin
      CheckThisPoint(LastX,LastY,xDEMg2,yDEMg2,xSATg2,ySATg2,CheckReasonable);
      DrawMultipleIntervisible;
   end;

   if (DEMNowDoing = FindBlockHorizon) then begin
      CheckThisPoint(LastX,LastY,xDEMg1,yDEMg1,xSATg1,ySATg1,CheckReasonable);
      ScreenSymbol(Image1.Canvas,LastX,LastY,Box,3,claRed);
      GetHorizonBlockingOptions;
      ChangeDEMNowDoing(JustWandering);
   end;
end;


procedure TMapForm.Cirrusmask1Click(Sender: TObject);
begin
   {$IfDef ExLandsatQA}
      MessageToContinue('Disabled while rewriting for collection 2');
   {$Else}
      SatImage[Mapdraw.SATonMap].GetLandsatQAMap('Cirrus', SatImage[Mapdraw.SATonMap].OriginalFileName,12,13);
   {$EndIf}
end;

procedure TMapForm.DrawMultipleIntervisible;
const
   MaxPts = 500;
var
   Lats,Longs,Dists : array[1..3] of Petmath.bfarray32;
   I,j,NPts : integer;
   Lat1,Long1,Lat2,Long2,
   Lat5,Long5,
   Dist1,Dist2,Dist3,
   FullDistance,Bearing : float64;
   Results : tStringList;
   Table : array[1..3] of tMyData;

   procedure CreateRouteTable(fName : PathStr; var Table : tMyData);
   begin
      CreateLatLongZTable(fName,false);
      Table := tMyData.Create(fName);
   end;

   procedure AddToTable(var Table : tMyData; Lat,Long : float64);
   begin
      Table.Insert;
      Table.SetFieldByNameAsFloat('LAT',Lat);
      Table.SetFieldByNameAsFloat('LONG',Long);
      Table.Post;
   end;

begin
   ChangeDEMNowDoing(JustWandering);
   for j := 1 to 3 do CreateRouteTable(DBDir + 'route' + IntToStr(j) + DefaultDBExt,Table[j]);
   DEMGlb[MapDraw.DEMonMap].DEMGridToLatLongDegree(xDEMg1,yDEMg1,Lat1,Long1);
   DEMGlb[MapDraw.DEMonMap].DEMGridToLatLongDegree(xDEMg2,yDEMg2,Lat2,Long2);

   VincentyCalculateDistanceBearing(Lat1,Long1,Lat2,Long2,FullDistance,Bearing);
   VincentyPointAtDistanceBearing(Lat1,Long1,FullDistance,Bearing,Lat5,Long5);

   {$IfDef RecordGeodeticCalculations}
      WriteLineToDebugFile('TMapForm.DrawMultipleIntervisible');
      WriteLineToDebugFile('Start point: ' + LatLongDegreeToString(Lat1,long1) + '  End point: ' + LatLongDegreeToString(Lat2,long2));
      WriteLineToDebugFile('Distance: ' + RealToString(FullDistance,18,2) + '   Bearing: ' + RealToString(Bearing,18,2));
      WriteLineToDebugFile('Calc end point: ' + LatLongDegreeToString(Lat5,long5));
   {$EndIf}

   NPts := 25;
   ReadDefault('Points to use',NPts);
   if NPts > MaxPts then NPts := MaxPts;

   for i := 1 to 3 do with DEMGLB[MapDraw.DEMonMap] do begin
      GetStraightRouteLatLongDegree(Lat1,Long1,Lat2,Long2,tStraightAlgorithm(i),NPts,Lats[i],Longs[i],dists[i]);
      for j := 0 to NPts do AddToTable(Table[i],Lats[i,j],Longs[i,j]);
   end;

   Results := tStringList.Create;
   Results.Add('Distance    UTM/Lat-Long   UTM/Vincenty   Vincenty/LatLong');
   for j := 0 to NPts do begin
      VincentyCalculateDistanceBearing(Lats[1,j],Longs[1,j],Lats[2,j],Longs[2,j],Dist1,Bearing);
      VincentyCalculateDistanceBearing(Lats[1,j],Longs[1,j],Lats[3,j],Longs[3,j],Dist2,Bearing);
      VincentyCalculateDistanceBearing(Lats[3,j],Longs[3,j],Lats[2,j],Longs[2,j],Dist3,Bearing);
      Results.Add(RealToString(j/NPts * FullDistance,8,1) + RealToString(Dist1,15,1) + RealToString(Dist2,12,1) + RealToString(Dist3,12,1));
   end;
   Petmar.DisplayAndPurgeStringList(Results,'Profile separation');
   for j := 1 to 3 do Table[j].Destroy;
   for j := 1 to 3 do LoadDataBaseFile(DBDir + 'route' + IntToStr(j) + DefaultDBExt);
end;


procedure TMapForm.ComapreUTMvsgeographic1Click(Sender: TObject);
begin
   {$If Defined(RecordCreateGeomorphMaps) or Defined(RecordDEMIX)} WriteLineToDebugFile('TMapForm.ComapreUTMvsgeographic1Clic in, ' + DEMGlb[MapDraw.DEMonMap].DEMFileName); {$EndIf}
   if (DEMGlb[MapDraw.DEMonMap].DEMFileName = '') then begin
      DEMGlb[MapDraw.DEMonMap].WriteNewFormatDEM(DEMGlb[MapDraw.DEMonMap].DEMFileName,' save DEM before resampling');
   end;
   ResampleForUTM_GeoComparison(MapDraw.DEMonMap);
   {$If Defined(RecordCreateGeomorphMaps) or Defined(RecordDEMIX)} WriteLineToDebugFile('TMapForm.ComapreUTMvsgeographic1Click out'); {$EndIf}
end;


procedure TMapForm.CombinedMGRSpolygons1Click(Sender: TObject);
begin
   //UTM100Kzones1Click(Sender);
   //MGRSUSNG6x8zones1Click(Sender);
end;

procedure TMapForm.ComboBox1Change(Sender: TObject);
var
   TStr : shortstring;
   BlowUp : int32;
begin
    TStr := ComboBox1.Text;
    Delete(Tstr,length(TStr),1);
    BlowUp := StrToInt(TStr);
    ResizeByPercentage(Blowup);
end;


procedure TMapForm.Comparechannelnetworks1Click(Sender: TObject);
begin
   //CompareChannelNetworks;
end;

procedure TMapForm.CompareDNconversions1Click(Sender: TObject);

   procedure DoOne(What : shortstring; HowConvert : tDNconvert);
   var
      dem : integer;
   begin
      MDDef.dnConvert := HowConvert;
      dem := NewSatWindow(nsbNDVI);
      DEMGlb[DEM].SelectionMap.Caption := What;
      DEMGlb[DEM].AreaName := What;
   end;

begin
   DEMDef_Routines.SaveBackupDefaults;
   DoOne('NDVI with DN',dncDN);
   DoOne('NDVI with TOA radiance',dncRadiance);
   DoOne('NDVI with TOA reflectance',dncReflectance);
   DoOne('NDVI with TOA ref and solar elevation correction',dncReflectSun);
   DEMDef_Routines.RestoreBackupDefaults;
end;


procedure TMapForm.CompareFanSensitivityAnalysis(WeaponsFan : tWeaponsFan);
var
   i,j,err : integer;
   x,y : float64;
   TStr : ShortString;
   Report : tStringList;
   fan_sens_form: Tfan_sens_form;


      procedure DoAVersion(Rays,Interval : float64);
      var
         NewMap : tMapForm;
         Bitmap : tMyBitmap;
         ShowName : PathStr;
      begin
         MDDef.wf.FanMethod := fmFanRadials;
         MDDef.wf.MaskRaySpacingDeg := Rays;
         MDDef.wf.MaskAreaInterval := Interval;
         NewMap := Self.DuplicateMap(false);
         NewMap.Caption := IntervisiblityAlgorithmName(MDDef.wf);
         CopyImageToBitmap(NewMap.Image1,Bitmap);
         NewMap.MapDraw.DrawFanOnMap(WeaponsFan,ShowName,'');
         NewMap.Caption := TStr;
         NewMap.Close;
         Bitmap.Free;
      end;

begin
   SaveBackupDefaults;
   Report := tStringList.Create;

   fan_sens_form := Tfan_sens_form.Create(Application);
   fan_sens_form.ShowModal;

   for i := 0 to pred(fan_sens_form.Memo1.Lines.Count) do begin
      Val(fan_sens_form.Memo1.Lines.Strings[i],x,err);
      if (err = 0) then begin
         for j := 0 to pred(fan_sens_form.Memo2.Lines.Count) do begin
            Val(fan_sens_form.Memo2.Lines.Strings[j],y,err);
            if (err = 0) then DoAVersion(y,x);
         end;
      end;
   end;
   fan_sens_form.free;
   Petmar.DisplayAndPurgeStringList(Report,'Results');
   RestoreBackupDefaults;
end;


procedure TMapForm.Createmask1Click(Sender: TObject);
var
   TerrainCategory : tTerrainCatDefinition;
begin
   DEMGlb[MapDraw.DEMonMap].InitializeTerrainCategory(TerrainCategory);
   GetTerrainCategory(tcNormal,Self,MapDraw.DEMonMap,TerrainCategory,false);
end;


procedure TMapForm.DrawMultipleFanMethods(WeaponsFan : tWeaponsFan);
var
   i,x,y : integer;
   DiffMap : tMapForm;
   DiffBMP : tMyBitmap;
   Red,Green,Blue : byte;
   P0 : pRGB;
   ShowName : PathStr;
   NewMap : array[1..6] of tMapForm;
   Bitmap : array[1..6] of tMyBitmap;
   aBitmap : tMyBitmap;
   PotentialCoverage : float64;
   TStr : ShortString;
   ThreeColorMap : boolean;
begin
   DEM_Fan_Compare.GetFanCompareOptions;
   NakedMapOptions;    //which calls SaveBackupDefaults;
   MapDraw.MapType := mtDEMBlank;

   CopyImageToBitmap(Image1,aBitmap);
   MapDraw.DrawFanOnMap(WeaponsFan,ShowName,'');
   Image1.Picture.Graphic := aBitmap;
   FreeAndNil(aBitmap);

   for i := 1 to MDDef.MultipleFanAlgorithmsToUse do begin
      MDDef.wf := MDDef.CompareIVA[i];
      NewMap[i] := Self.DuplicateMap(false);
      NewMap[i].Caption := IntervisiblityAlgorithmName(MDDef.wf);
      CopyImageToBitmap(NewMap[i].Image1,Bitmap[i]);
      NewMap[i].MapDraw.DrawFanOnMap(WeaponsFan,ShowName,'');
      TStr := IntervisiblityAlgorithmName(MDDef.wf) + '    ' +  RealToString(PotentialCoverage,-8,2) + '%';
      NewMap[i].Caption := TStr;
      {$IfDef RecordProblems} WriteLineToDebugFile(TStr); {$EndIf}
      NewMap[i].Image1.Picture.Graphic := Bitmap[i];
   end;

   if ThreeColorMap then begin
      DiffMap := Self.DuplicateMap(false);
      CopyImageToBitmap(DiffMap.Image1,DiffBMP);
      StartProgress('Coloring');
      for y := 0 to DiffMap.MapDraw.MapYSize do begin
         P0 := DiffBMP.ScanLine[y];
         UpdateProgressBar(y/DiffMap.MapDraw.MapYSize);
         for x := 0 to DiffMap.MapDraw.MapXSize do begin
            if Bitmap[1].Canvas.Pixels[x,y] = clWhite then Red := 0 else Red := 255;
            if Bitmap[2].Canvas.Pixels[x,y] = clWhite then Green := 0 else Green := 255;
            if Bitmap[3].Canvas.Pixels[x,y] = clWhite then Blue := 0 else Blue := 255;
            P0[x].rgbtRed := Red;
            P0[x].rgbtGreen := Green;
            P0[x].rgbtBlue := Blue;
         end;
      end;
      DiffMap.Image1.Picture.Graphic := DiffBMP;
      DiffBMP.Free;
   end;
   for i := 1 to 3 do Bitmap[i].Free;
   EndProgress;
   RestoreBackupDefaults;
end;


procedure TMapForm.Drawmultiplefans1Click(Sender: TObject);
begin
   EditAWeaponsFan(Nil);
end;


procedure TMapForm.GetHorizonBlockingOptions;
{$IfDef ExAdvancedGIS}
begin
{$Else}
var
   Lat,Long : float64;
begin
    CheckThisPoint(LastX,LastY,xDEMg1,yDEMg1,xSATg1,ySATg1,CheckReasonable);
    ScreenSymbol(Image1.Canvas,LastX,LastY,Box,3,claRed);
    DEMGlb[MapDraw.DEMonMap].DEMGridToLatLongDegree(xDEMg1,yDEMg1,Lat,Long);
    DEMOptions.SetHorizonOptions(Self,Lat,Long);
{$EndIf}
end;


procedure TMapForm.Globaltectonicsmap1Click(Sender: TObject);
begin
   {$If Defined(ExGeology) or Defined(ExGeologyDownload)}
   {$Else}
      {$IfDef RecordGeologyMap} WriteLineToDebugFile('TMapForm.Globaltectonicsmap1Click in'); {$EndIf}
      GetETOPO1;
      GeologyGetData(false);
      Plateboundaries1Click(Sender);
      {$IfDef RecordGeologyMap} WriteLineToDebugFile('TMapForm.Globaltectonicsmap1Click plate boundaries done'); {$EndIf}
      Earthquakefocalmechanisms1Click(Sender);
      {$IfDef RecordGeologyMap} WriteLineToDebugFile('TMapForm.Globaltectonicsmap1Click focal mech done'); {$EndIf}
      Volcanoes1Click(Sender);
      {$IfDef RecordGeologyMap} WriteLineToDebugFile('TMapForm.Globaltectonicsmap1Click volcanoes done'); {$EndIf}
      Hotspots1Click(Sender);
      {$IfDef RecordGeologyMap} WriteLineToDebugFile('TMapForm.Globaltectonicsmap1Click hot spots done'); {$EndIf}
      db_display_options.OpenMapTableOfContents(Self,true);
      {$IfDef RecordGeologyMap} WriteLineToDebugFile('TMapForm.Globaltectonicsmap1Click in'); {$EndIf}
   {$EndIf}
end;


procedure TMapForm.Gridgraticule1Click(Sender: TObject);
begin
   GridSpeedButton15Click(Sender);
end;


procedure TMapForm.Gridgraticule2Click(Sender: TObject);
begin
   GridSpeedButton15Click(Sender);
end;


procedure TMapForm.DrawHorizon(Lat,Long : float64);
begin
   {$IfDef RecordCircleAround} WriteLineToDebugFile('TMapForm.DrawHorizon in ' + LatLongDegreeToString(Lat,Long)); {$EndIf}
      HorizonBlockingGraph(Self,Lat,Long,false,false);   //MDDef.HorizonVertAngleGraph,MDDef.HorizonDistanceGraph);
   {$IfDef RecordCircleAround} WriteLineToDebugFile('TMapForm.DrawHorizon out'); {$EndIf}
end;

{$EndIf}


{$IfDef ExIndexes}
{$Else}

      procedure TMapForm.SetUpIndexMap(DEMtoUse : integer);
      var
      MinLat,MaxLat,MinLong,MaxLong : float64;
      begin
      {$IfDef RecordMapIndex}    WriteLineToDebugFile('TMapForm.SetUpIndexMap in');      {$EndIf}
      if (DEMtoUse = 0) and (MapDraw.SatOnMap = 0) then begin
         MapDraw.MapOwner := moMapDatabase;
         MapDraw.MapXSize := MDdef.DefaultMapXSize;
         MapDraw.MapYSize := MDdef.DefaultMapYSize;
         GetMapLibraryDataLimits(MinLat,MaxLat,MinLong,MaxLong);
         if (MinLat < -70) then MinLat := -70;
         if (MaxLat > 70) then MaxLat := 70;
         MapDraw.MaximizeLatLongMapCoverage(MinLat-0.05,MinLong-0.05,MaxLat+0.05,MaxLong+0.05);
         AddOverlay(Self,ovoWorldOutlines);
      end;
      DoFastMapRedraw;
      {$IfDef RecordMapIndex} WriteLineToDebugFile('TMapForm.SetUpIndexMap out'); {$EndIf}
      end;



      procedure TMapForm.Shapefileaftersubsettomatchmapextent1Click(Sender: TObject);
      {$IfDef NoExternalPrograms}
      begin
      {$Else}
      var
      fName : PathStr;
      begin
      fName := ExtractFilePath(LastDataBase);
      if Petmar.GetExistingFileName('shape file to subset and import','Shape file|*.shp',fName) then begin
         fName := ExtractMapCoverageToWGS84Shapefile(fName, MapDraw.MapCorners.BoundBoxGeo);
         LoadDataBaseFile(fName);
      end;
      {$EndIf}
      end;


      procedure TMapForm.CheckIndexMouseUp(NWLat,NWLong,SELat,SELong : float64);
      var
      WantedDEM,WantImage : integer;
      begin
      LoadMapLibraryBox(WantedDEM,WantImage,DEMNowDoing in [OpenMapsFromLibrary],NWLat,NWLong,SELat,SELong);
      if (WantedDEM <> 0) then begin
         CreateDEMSelectionMap(WantedDEM);
      end;
      if MDdef.AutoCloseIndexMaps then Close;
      wmdem.SetMenusForVersion;
      end;

{$EndIf}


procedure TMapForm.Checkprojectionresults1Click(Sender: TObject);
var
   Lat,Long,x,y,xproj,yproj : float64;
   sl : tStringList;
begin
    PointConvertDebug := true;
    if MapDraw.PrimMapProj.PName = LambertConformalConicEllipse then begin
       Lat := 35;
       Long := -75;
       x := 1894410.9;
       y := 1564649.5;
       sl := MapDraw.PrimMapProj.ProjectionParametersList(true);
       sl.add('');
       sl.add('Snyder test points, on a different ellipsoid');

       sl.Add('Starting points:  ' + LatLongDegreeToString(Lat,Long) +  ' LCC projected, x=' + RealToString(x,-12,-1) + '  y=' + RealToString(y,-12,-1) );
       MapDraw.PrimMapProj.ForwardProjectDegrees(Lat,Long,xproj,yproj);
       MapDraw.PrimMapProj.InverseProjectDegrees(x,y,Lat,Long);
       sl.add('Projected points: ' + LatLongDegreeToString(Lat,Long) +  ' LCC projected, x=' + RealToString(xproj,-12,-1) + '  y=' + RealToString(yproj,-12,-1) );
       DisplayAndPurgeStringList(sl,'LCC conversion');
    end;
    PointConvertDebug := false;
end;


procedure TMapForm.DrapecurrentmaptoOpenGL1Click(Sender: TObject);
begin
   {$IfDef RecordDrape} WriteLineToDebugFile('TMapForm.DrapecurrentmaptoOpenGL1Click enter'); {$EndIf}
   BitmapandXYZBfile1Click(Sender);
end;


var
   AntDEM,FlyDEM,GrazeDEM,CurveDEM : integer;


function MakeRequiredAntennaMap(ProgTitle: shortString; CurDEM : integer; W_Lat,W_Long : float64; ObserverTotalElevation : float64 = 0;
    MaxRange : float64 = 75000; DrawMap : boolean = true; StartAngle : float64 = 0; EndAngle : float64 = 360) : integer;
var
   Col,Row,StartCol,StartRow,EndCol,EndRow,xg,yg : integer;
   NeedZ,xgrids,ygrids,dists : ^Petmath.bfarray32;
   wx,wy : float32;

         procedure DoRadial(x,y : integer);
         var
            PixLong,PointOnRay : integer;
            PointElev,LastPointElev : float32;
            Lat2,Long2,Slope2,Pitch, Heading : float64;
         begin
            {$IfDef RecordFullReqAnt} WriteLineToDebugFile('Do radial in'); {$EndIf}
            with DEMGlb[CurDEM] do begin
               PixLong := round(2 * DistanceMetersBetweenPoints(wx,wy,x,y,Heading) / AverageXSpace);
               if (Heading < StartAngle) or (Heading > EndAngle) then exit;
               {$IfDef RecordFullReqAnt} WriteLineToDebugFile('DoRadial PixLong done');{$EndIf}
               DEMGridToLatLongDegree(x,y,Lat2,Long2);

               LatLongDegreePointsRequiredAntenna(PixLong,W_Lat,W_Long,ObserverTotalElevation,Lat2,Long2,xgrids^,ygrids^,dists^,NeedZ^);
               {$IfDef RecordFullReqAnt} WriteLineToDebugFile('DoRadial GetStraightRoute done'); {$EndIf}
               GetElevMeters(xgrids^[0],ygrids^[0],LastPointElev);
               for PointOnRay := 1 to PixLong do begin
                  if GetElevMeters(xgrids^[PointOnRay],ygrids^[PointOnRay],PointElev) then begin
                     xg := round(xgrids^[PointOnRay]);
                     yg := round(ygrids^[PointOnRay]);
                     if MDDef.DoReqAntHigh then begin
                        DEMGlb[AntDEM].SetGridElevation(xg,yg,NeedZ^[PointOnRay]);
                     end;
                     if MDDef.DoEarthCurvature then begin
                        DEMGlb[CurveDEM].SetGridElevation(xg,yg,DropEarthCurve(Dists^[PointOnRay]) );
                     end;
                     if MDDef.DoGrazingAngle then begin
                        if NeedZ^[PointOnRay] < 1 then  begin  //leave undefined wherer masked
                           Slope2 := ArcTan((PointElev - LastPointElev) / (Dists^[PointOnRay] - Dists^[pred(PointOnRay)])) / DegToRad;
                           Pitch := -ArcTan((ObserverTotalElevation - PointElev - DropEarthCurve(Dists^[PointOnRay]) ) /Dists^[PointOnRay]) / DegToRad;
                           DEMGlb[GrazeDEM].SetGridElevation(xg,yg,Slope2+Pitch);
                           LastPointElev := PointElev;
                        end;
                     end;
                     if MDDef.DoReqFlyHigh then begin  //must be last since redefine NeedZ value
                        if (NeedZ^[PointOnRay] < MDdef.MinTerrainFlyAbove) then NeedZ^[PointOnRay] := MDdef.MinTerrainFlyAbove;
                        NeedZ^[PointOnRay] := PointElev + NeedZ^[PointOnRay];
                        DEMGlb[FlyDEM].SetGridElevation(xg,yg,NeedZ^[PointOnRay]);
                     end;
                  end;
                  if (Dists^[PointOnRay] > MaxRange) then break;
               end;
            end {with};
         end;


         procedure SetUpMap(LogDEM : integer);
         begin
            {$IfDef RecordReqAnt} WriteLineToDebugFile('Coverage area  ' + DEMGlb[LogDEM].KeyDEMParams); {$EndIf}
            DEMGlb[LogDEM].SetUpMap(LogDEM,true,mtElevSpectrum);
            DEMGlb[LogDEM].SelectionMap.SaveDEM1.Visible := false;
         end;


begin
   {$IfDef RecordReqAnt} WriteLineToDebugFile('MakeRequiredAntennaMap'); {$EndIf}
   New(xgrids);
   New(ygrids);
   New(dists);
   New(NeedZ);
   DEMGlb[CurDEM].LatLongDegreeToDEMGrid(W_Lat,W_Long,wx,wy);

   StartCol := trunc(wx - MaxRange / DEMGlb[CurDEM].AverageXSpace);
   EndCol := succ(round(wx + MaxRange / DEMGlb[CurDEM].AverageXSpace));
   StartRow := trunc(wy - MaxRange / DEMGlb[CurDEM].AverageYSpace);
   EndRow := succ(round(wy + MaxRange / DEMGlb[CurDEM].AverageYSpace));

   DEMGlb[CurDEM].ClipDEMGrid(StartCol,StartRow);
   DEMGlb[CurDEM].ClipDEMGrid(EndCol,EndRow);

   {$IfDef RecordReqAnt} WriteLineToDebugFile('Setup done'); {$EndIf}
   AntDEM := 0;
   FlyDEM := 0;

   if MDDef.DoEarthCurvature then  begin
      CurveDEM := DEMGlb[CurDEM].CloneAndOpenGridSetMissing(FloatingPointDEM,'Earth curvature (m)',Undefined);
   end;

   if MDDef.DoReqAntHigh then begin
      AntDEM := DEMGlb[CurDEM].CloneAndOpenGridSetMissing(FloatingPointDEM,'Antenna required (m)',euMeters);
      Result := AntDEM;
   end;

   if MDDef.DoReqFlyHigh then FlyDEM  := DEMGlb[CurDEM].CloneAndOpenGridSetMissing(FloatingPointDEM,'Flying height (m)',euMeters);

   if MDDef.DoGrazingAngle then begin
      GrazeDEM := DEMGlb[CurDEM].CloneAndOpenGridSetMissing(FloatingPointDEM, 'Grazing angle (' + DegSym + ')',zDegrees);
   end;

    StartProgress(ProgTitle + ' Cols (1/2)');
    for Col := StartCol to EndCol do begin
       if (Col mod 100 = 0) then UpdateProgressBar((Col-StartCol)/(EndCol-StartCol));
       DoRadial(Col,StartRow);
       DoRadial(Col,EndRow);
    end;

    StartProgress(ProgTitle + ' Rows (2/2)');
    for Row := StartRow to EndRow do begin
       if (Row mod 100 = 0) then UpdateProgressBar((Row-StartRow)/(EndRow-StartRow));
       DoRadial(StartCol,Row);
       DoRadial(EndCol,Row);
    end;
    EndProgress;

    if DrawMap then begin
       if MDDef.DoReqAntHigh then SetUpMap(AntDEM);
       if MDDef.DoReqFlyHigh then SetUpMap(FlyDEM);
       if MDDef.DoGrazingAngle then SetUpMap(GrazeDEM);
       if MDDef.DoEarthCurvature then SetUpMap(CurveDEM);
    end;

   Dispose(xgrids);
   Dispose(ygrids);
   Dispose(dists);
   Dispose(NeedZ);

   {$IfDef RecordReqAnt} WriteLineToDebugFile(' MakeRequiredAntennaMap out'); {$EndIf}
end;



procedure TMapForm.Derivativegrid1Click(Sender: TObject);
begin
   if (MapDraw.ValidDEMonMap) or (MapDraw.ValidSatOnMap) then begin
      RIK1.Visible := TrilobiteComputer;
      RICK1.Visible := TrilobiteComputer;
      DerivativeGridPopupMenu16.Popup(Mouse.CursorPos.X,Mouse.CursorPos.Y);
   end;
end;

procedure TMapForm.Differenceelevationslopeaspectmaps1Click(Sender: TObject);
begin
   MakeElevSlopeAspectDifferenceMap;
end;


function TMapForm.ValidDifferentDEM(i : integer) : boolean;
begin
   Result := (i <> MapDraw.DEMonMap) and ValidDEM(i);
end;

procedure TMapForm.Differencemap1Click(Sender: TObject);
begin
   MapDraw.MapType := mtGGRReflect;
   MDDef.TopCutLevel := abs(MDDef.TopCutLevel);
   MDDef.BottomCutLevel := -MDDef.TopCutLevel;
   DoBaseMapRedraw;
   ModifyChangeMapSettings(Self);
end;


procedure TMapForm.Differencemapsallotheropengrids1Click(Sender: TObject);
{$IfDef ExComplexGeoStats}
begin
{$Else}
var
   i,j,NewDEM : integer;
   Results : tStringList;
   fName : PathStr;
   DoDEM : array [1..MaxDEMDataSets] of boolean;
begin
   Results := tStringList.Create;
   Results.Add('DEM,RMSE');
   for i := 1 to MaxDEMDataSets do begin
      DoDEM[i] := ValidDifferentDEM(i);
   end;
   try
      HeavyDutyProcessing := true;
      for i := 1 to MaxDEMDataSets do if DoDEM[i] then begin
         NewDEM := MakeDifferenceMapOfBoxRegion(MapDraw.DEMonMap,i,MapDraw.DEMonMap,0,DEMGlb[MapDraw.DEMonMap].FullDEMGridLimits,true,false,false,'Delta ' + DEMGlb[i].AreaName + ' minus ' + DEMGlb[MapDraw.DEMonMap].AreaName);
         Results.Add(DEMGlb[i].AreaName + ',' + RealToString(DEMGlb[NewDEM].SelectionMap.ComputeRMSE,-12,2));
      end;
   finally
      HeavyDutyProcessing := true;
   end;
   fName := Petmar.NextFileNumber(MDTempDir, 'DEMGlb[MapDraw.DEMonMap].AreaName_RMSE_', DefaultDBExt);
   StringListToLoadedDatabase(Results,fName);
   if AnswerIsYes('Close original DEMs/grids') then begin
      for i := 1 to MaxDEMDataSets do if DoDEM[i] then begin
         j := i;
         CloseSingleDEM(j);
      end;
      CloseSingleDEM(MapDraw.DEMonMap);
   end;
{$EndIf}
end;

(*
procedure TMapForm.Slabdepths1Click(Sender: TObject);
{$IfDef ExGeology}
begin
{$Else}
var
   fName,fName2 : PathStr;
   db : integer;
begin
   fName2 := 'slab_depth' + DefaultDBExt;
   fName := DBDir + fName2;
   if not FileExists(fName) then begin
      DownloadFileFromWeb(WebDataDownLoadDir + fName2,fName);
   end;


   if FileExists(fName) then  begin
      MapDraw.AllowDataBaseDrawing := false;
      db := LoadDataBaseFile(fName);
      if ValidDB(db) then begin
         MapDraw.AllowDataBaseDrawing := true;
         GISdb[db].PlotFieldOnMap('SLAB_DEPTH',-740,0);
      end;
   end;
{$EndIf}
end;
*)

procedure TMapForm.Solarparameters1Click(Sender: TObject);
begin
   {$IfDef RecordGeography} WriteLineToDebugFile('TMapForm.Solarparameters1Click'); {$EndIf}
   Equinoxsolstice1Click(Sender);
   Annualsunrisesunset2Click(Sender);
   Annualsolarelevation1Click(Sender);
end;

procedure TMapForm.auDEMcode181Click(Sender: TObject);
begin
  {$IfDef RecordMapDraw} WriteLineToDebugFile('TMapForm.auDEMcode181Click in'); {$EndIf}
   MapDraw.MapType := mtFlowDirTau;
   DrawColoredMap1Click(Nil);
  {$IfDef RecordMapDraw} WriteLineToDebugFile('TMapForm.auDEMcode181Click out'); {$EndIf}
end;


procedure TMapForm.Buildingedges1Click(Sender: TObject);
begin
   {$IfDef ExGeostats}
   {$Else}
      PitSpireDefaults(Self,2);
   {$EndIf}
end;

procedure TMapForm.Slope1Click(Sender: TObject);
begin
   {$IfDef ExGeoStats}
   {$Else}
      CreateSlopeMap(MapDraw.DEMonMap);
   {$EndIf}
end;

procedure TMapForm.Aspect2Click(Sender: TObject);
begin
   {$IfDef ExGeoStats}
   {$Else}
      MakeAspectMap(MapDraw.DEMonMap);
   {$EndIf}
end;


procedure TMapForm.Aspectdifference1Click(Sender: TObject);
{$IfDef ExGeoStats}
begin
{$Else}
var
   AspectDEM : integer;
begin
   AspectDEM := MakeAspectMap(MapDraw.DEMonMap);
   AspectDifferenceMap(AspectDEM,1,DEMGlb[AspectDEM].FullDEMGridLimits);
   AspectDifferenceMap(AspectDEM,2,DEMGlb[AspectDEM].FullDEMGridLimits);
   AspectDifferenceMap(AspectDEM,3,DEMGlb[AspectDEM].FullDEMGridLimits);
{$EndIf}
end;


procedure TMapForm.Aspectoptions1Click(Sender: TObject);
begin
   ChangeAspectMap(Self);
end;

procedure TMapForm.Aspectrosediagram1Click(Sender: TObject);
begin
   CreateAspectRose(MapDraw.DEMonMap);
end;


procedure TMapForm.Aspectslopemerge1Click(Sender: TObject);
begin
   MapDraw.MapType := mtDEMaspectSlope;
   DrawColoredMap1Click(Nil);
end;

procedure TMapForm.erraincategory1Click(Sender: TObject);
begin
   {$IfDef ExGeoStats}
   {$Else}
      MakeSingleNewDerivativeMap('T',MapDraw.DEMonMap);
   {$EndIf}
end;

procedure TMapForm.Lntransformelevs1Click(Sender: TObject);
begin
   {$IfDef ExGeoStats}
   {$Else}
      MakeSingleNewDerivativeMap('E',MapDraw.DEMonMap);
   {$EndIf}
end;

procedure TMapForm.Log10transformelevs1Click(Sender: TObject);
begin
   {$IfDef ExGeoStats}
   {$Else}
       MakeSingleNewDerivativeMap('L',MapDraw.DEMonMap);
   {$EndIf}
end;


procedure TMapForm.Reliefbyregionsize1Click(Sender: TObject);
begin
   {$IfDef ExGeoStats}
   {$Else}
      MakeGraphsByRegionSize(MapDraw.DEMonMap,RightClickLat,RightClickLong);
   {$EndIf}
end;

procedure TMapForm.Crosssectionalcurvature1Click(Sender: TObject);
begin
   {$IfDef ExGeoStats}
   {$Else}
      SetAllCurvatures(false);
      MDDef.DoCrossCurve := true;
      MakeMomentsGrid(MapDraw.DEMonMap,'C');
   {$EndIf}
end;

procedure TMapForm.CSVforVDATUM1Click(Sender: TObject);
begin
   DEMGlb[MapDraw.DEMonMap].CSVforVDatum;
end;


procedure TMapForm.Profile1Click(Sender: TObject);
var
   NewGrid : integer;
begin
   {$IfDef NoExternalPrograms}
   {$Else}
       NewGrid := WBT_ProfileCurvature(GeotiffDEMNameOfMap);
       MatchAnotherDEMMap(NewGrid,MapDraw.DEMonMap);
   {$EndIf}
end;

procedure TMapForm.Profileconvexity1Click(Sender: TObject);
begin
   {$IfDef ExGeoStats}
   {$Else}
       CreateProfileConvexityMap(MapDraw.DEMonMap);
   {$EndIf}
end;

procedure TMapForm.Profilecurvature1Click(Sender: TObject);
begin
   {$IfDef ExGeostats}
   {$Else}
      MakeMomentsGrid(MapDraw.DEMonMap,'r',MDDef.MomentsBoxSizeMeters);
   {$EndIf}
end;

procedure TMapForm.Planconvexity1Click(Sender: TObject);
begin
   {$IfDef ExGeoStats}
   {$Else}
      SetAllCurvatures(false);
      MDDef.DoPlanCurve := true;
      MakeMomentsGrid(MapDraw.DEMonMap,'C');
   {$EndIf}
end;

procedure TMapForm.Plancurvature1Click(Sender: TObject);
begin
   {$IfDef ExGeostats}
   {$Else}
      MakeMomentsGrid(MapDraw.DEMonMap,'l',MDDef.MomentsBoxSizeMeters);
   {$EndIf}
end;


procedure TMapForm.MenuItem3Click(Sender: TObject);
begin
   {$IfDef ExGeoStats}
   {$Else}
      MakeSingleNewDerivativeMap('R',MapDraw.DEMonMap);
   {$EndIf}
end;

procedure TMapForm.MenuItem2Click(Sender: TObject);
begin
   {$IfDef ExGeoStats}
   {$Else}
      SetAllSlopes(false);
      MDDef.DoSlopeDeg := true;
      MakeMomentsGrid(MapDraw.DEMonMap,'S');
   {$EndIf}
end;

procedure TMapForm.SlopeSin1Click(Sender: TObject);
begin
   {$IfDef ExGeoStats}
   {$Else}
      SetAllSlopes(false);
      MDDef.DoSlopeSin := true;
      MakeMomentsGrid(MapDraw.DEMonMap,'S');
   {$EndIf}
end;

procedure TMapForm.Slopesqrtsin1Click(Sender: TObject);
begin
   {$IfDef ExGeoStats}
   {$Else}
      SetAllSlopes(false);
      MDDef.DoSlopeSqrtSin := true;
      MakeMomentsGrid(MapDraw.DEMonMap,'S');
   {$EndIf}
end;

procedure TMapForm.Slopplntangent1Click(Sender: TObject);
begin
   {$IfDef ExGeoStats}
   {$Else}
      SetAllSlopes(false);
      MDDef.DoSlopeLnTan := true;
      MakeMomentsGrid(MapDraw.DEMonMap,'S');
   {$EndIf}
end;

procedure TMapForm.Minimal1Click(Sender: TObject);
var
   NewGrid : integer;
begin
   {$IfDef NoExternalPrograms}
   {$Else}
       NewGrid := WBT_MinimalCurvature(GeotiffDEMNameOfMap);
       MatchAnotherDEMMap(NewGrid,MapDraw.DEMonMap);
   {$EndIf}
end;

procedure TMapForm.Minimumcurvature1Click(Sender: TObject);
begin
   {$IfDef ExGeoStats}
   {$Else}
      SetAllCurvatures(false);
      MDDef.DoMinCurve := true;
      MakeMomentsGrid(MapDraw.DEMonMap,'C');
   {$EndIf}
end;


procedure TMapForm.Maximumcurvature1Click(Sender: TObject);
begin
   {$IfDef ExGeoStats}
   {$Else}
      SetAllCurvatures(false);
      MDDef.DoMinCurve := true;
      MakeMomentsGrid(MapDraw.DEMonMap,'C');
   {$EndIf}
end;


procedure TMapForm.EWcomponent1Click(Sender: TObject);
begin
   {$IfDef ExGeoStats}
   {$Else}
      SetAllSlopes(false);
      MDDef.DoAspectEW := true;
      MakeMomentsGrid(MapDraw.DEMonMap,'S');
   {$EndIf}
end;

procedure TMapForm.NScomponentofaspect1Click(Sender: TObject);
begin
   {$IfDef ExGeoStats}
   {$Else}
      SetAllSlopes(false);
      MDDef.DoAspectNS := true;
      MakeMomentsGrid(MapDraw.DEMonMap,'S');
   {$EndIf}
end;


procedure TMapForm.Ridges1Click(Sender: TObject);
begin
   {$IfDef ExExoticMaps}
   {$Else}
      CreateRidgeMap(MapDraw.DEMOnMap,DEMGlb[MapDraw.DEMonMap].FullDEMGridLimits, rtmRidge);
   {$EndIf}
end;


function TMapForm.DrawRidgeMask(Color : tColor; RidgeMask,DisplayAndPurge : boolean) : tMyBitmap;
{$IfDef ExGeology}
begin
{$Else}
var
   PointType : tPointType;
   Rad,x1,y1,i,j : integer;
begin
   DoFastMapRedraw;
   PetImage.CloneImageToBitmap(Image1,Result);
   Rad := round(MDDef.RidgeMaskRadius / MapDraw.ScreenPixelSize);
   if (Rad < 1) then Rad := 1;
   Result.Canvas.Brush.Style := bsSolid;
   Result.Canvas.Brush.Color := Color;
   Result.Canvas.Pen.Color := Color;

   StartProgress('Ridge mask');
   with DEMGLB[MapDraw.DEMonMap],MapDraw.MapCorners do begin
      for i := trunc(BoundBoxDataGrid.xmin) to round(BoundBoxDataGrid.xmax) do begin
         if ShowSatProgress then UpdateProgressBar(i/DEMheader.NumCol);
         for j := trunc(BoundBoxDataGrid.ymin) to round(BoundBoxDataGrid.ymax) do begin
             if (MDDef.RidgePointClassify = raWood) then WoodPointClassify(i,j,PointType)
             else PointType := ClassifyAPoint(i,j);
             if (RidgeMask and (PointType in [PeakPoint,RidgePoint])) or ((not RidgeMask) and (PointType in [PitPoint,ValleyPoint]))  then begin
                MapDraw.DEMGridToScreen(i,j,x1,y1);
                if (MDDef.RidgePointClassify = raWood) then Result.Canvas.Pixels[x1,y1] := Color
                else Result.Canvas.Ellipse(x1-rad,y1-rad,x1+rad,y1+rad);
             end;
         end;
      end;
   end;

   if MDDef.InvertRidgeMask then MakeBitmapNegative(Result,ConvertTColorToPlatformColor(Color));
   if DisplayAndPurge then IHSmergeOntoMap(Result);
   EndProgress;
{$EndIf}
end;

procedure TMapForm.RidgesClick(Sender: TObject);
begin
   {$IfDef ExGeostats}
   {$Else}
      Geomorph_point_class.PointClassification(MapDraw.DEMonMap);
   {$EndIf}
end;



procedure TMapForm.CreateGeoAtlasKMZ1Click(Sender: TObject);
var
   Results : tStringList;
   fName : PathStr;
   Table : tMyData;


         procedure DoPoint(Lat,Long : float32);
         var
            Graph : TNetForm;
         begin
            MDDef.SunlightSingleDay := 2;
            Graph := SunAndHorizon(Nil,0,Lat,Long,false,true);
            fName := Petmar.NextFileNumber(MDTempDir,'atlas_image_','.png');
            SaveImageAsBMP(Graph.Image1,fName);
            Results.Add(RealToString(Lat,-8,-2) + ',' + RealToString(Long,-8,-2) + ',' + fName);
            Graph.Destroy;
         end;

         Procedure DoLatRow(Lat : float32);
         begin
            DoPoint(Lat,-100);
            DoPoint(Lat,0);
            DoPoint(Lat,100);
         end;

         procedure DoSunPosition;
         begin
            DoLatRow(75);
            DoLatRow(66.5);
            DoLatRow(50);
            DoLatRow(40);
            DoLatRow(23.5);
            DoLatRow(10);
            DoLatRow(0);
            DoLatRow(-23.5);
            DoLatRow(-40);
            DoLatRow(-66.5);
            DoLatRow(-75);
         end;

         procedure MakeGraph(Which : integer; Lat,Long : float32);
         var
            Graph : TThisBaseGraph;
            kg  : TKoppenGraph;
            Net : tNetForm;
            aName : shortstring;
         begin
            fName := Petmar.NextFileNumber(MDTempDir,'atlas_image_','.png');
            aName := Table.GetFieldByNameAsString('NAME');
            if (Which = 1) then begin
               kg := MakeKoppenClimograph(MapDraw.DEMonMap,Lat,Long);
               if (kg <> Nil) then begin
                  SaveImageAsBMP(kg.Image1,fName);
                  kg.Destroy;
                  Results.Add(RealToString(Lat,-12,-6) + ',' + RealToString(Long,-12,-6) + ',' + aName + ',' + fName);
                  wmdem.Closeallgraphs1Click(Sender);
                  CloseAllDataBases;
               end;
            end;
            if (Which in [2,3]) then begin
               if Which = 2 then Graph := MakePOTETgraph(MapDraw.DEMonMap,Lat,Long);
               if Which = 3 then Graph := MonthlyInsolationGraph(MapDraw.DEMonMap,Lat,Long);
               if (Graph <> Nil) then begin
                  SaveImageAsBMP(Graph.Image1,fName);
                  Graph.Destroy;
                  Results.Add(RealToString(Lat,-12,-6) + ',' + RealToString(Long,-12,-6) + ',' + aName + ',' + fName);
                  wmdem.Closeallgraphs1Click(Sender);
                  CloseAllDataBases;
               end;
            end;

            if Which = 4 then begin
               MDDef.SunlightSingleDay := 2;
               Net := SunAndHorizon(Nil,0,Lat,Long,false,true);
               if (Net <> Nil) then begin
                  SaveImageAsBMP(Net.Image1,fName);
                  Results.Add(RealToString(Lat,-12,-6) + ',' + RealToString(Long,-12,-6) + ',' + aName + ',' + fName);
                  Net.Destroy;
               end;
            end;
         end;

          procedure MakeAtlasPage(Which : integer; aName : shortstring);
          var
             db : integer;
             Lat,Long : float64;
          begin
             Results := tStringList.Create;
             Results.Add('Lat,Long,NAME,IMAGE');

             Table.First;
             while not Table.eof do begin
                if Table.ValidLatLongFromTable(Lat,Long) then MakeGraph(Which,Lat,Long);
                Table.Next;
             end;
             (*
             iLat := 70;
             while iLat > -80 do begin
                iLong := -160;

                if abs(ilat) in [0..40] then dLong := 3
                else if abs(ilat) in [41..50] then dLong := 4
                else if abs(ilat) in [50..61] then dlong := 6
                else dlong := 10;
                while iLong < 170 do begin
                   MakeGraph(1,iLat,iLong);
                   iLong := iLong + dlong;
                end;
                iLat := iLat - 5;
             end;
             *)
             fName := Petmar.NextFileNumber(MDTempDir,aName + '_','.dbf');
             db := StringListToLoadedDatabase(Results, fName);
             GISdb[db].ExportToKML(true,true);
          end;

begin
   fName := 'C:\mydocs\website\so262\google_earth_exercises\las_vegas\vegas.dbf';
   if GetFileFromDirectory('Points for atlas figures',DBNameMask,fName) then begin
      Table := tMyData.Create(fName);
      MakeAtlasPage(1,'Koppen_climograph');
      MakeAtlasPage(2,'POTET_precipitation');
      MakeAtlasPage(3,'Insolation');
      MakeAtlasPage(4,'Sun_position');
      Table.Destroy;
   end;
end;


procedure TMapForm.Creategrid1Click(Sender: TObject);
{$IfDef ExGeostats}
begin
{$Else}
var
   fLoX,fHiX,fLoY,fHiY, UseDEM,x2,y2,i,ID,NewDEM: integer;
   z,z2 : float32;
   WantedField : ShortString;
begin
   {$IfDef RecordCreateGeomorphMaps} WriteLineToDebugFile('TMapForm.Creategrid1Click in FeaturesDB=' + FeaturesDB.ToString); {$EndIf}
   if (FeaturesDB = 0) or (GISdb[FeaturesDB] = Nil) then exit;
   WantedField := GISdb[FeaturesDB].PickField('Field for grid',NumericFieldTypes);
   if WantedField = '' then exit;

   {$IfDef RecordCreateGeomorphMaps} WriteLineToDebugFile('  WantedField=' + WantedField): {$EndIf}

   if (DEMGlb[MapDraw.DEMonMap].SelectionMap.MapDraw.FeatureGrid = 0) then UseDEM := MapDraw.DEMonMap
   else UseDEM := DEMGlb[MapDraw.DEMonMap].SelectionMap.MapDraw.FeatureGrid;

   NewDEM := DEMGlb[UseDEM].CloneAndOpenGridSetMissing(FloatingPointDEM,WantedField,Undefined);
   i := 0;
   StartProgress('Features');
   GISdb[FeaturesDB].MyData.First;
   GISdb[FeaturesDB].EmpSource.Enabled := false;
   while not GISdb[FeaturesDB].MyData.eof do begin
      inc(i);
      if (i mod 25 = 0) then UpdateProgressBar(i/GISdb[FeaturesDB].MyData.FiltRecsInDB);
      z2 := GISdb[FeaturesDB].MyData.GetFieldByNameAsFloat(WantedField);
      fLoX := GISdb[FeaturesDB].MyData.GetFieldByNameAsInteger('BOUND_XMIN');
      fHiX := GISdb[FeaturesDB].MyData.GetFieldByNameAsInteger('BOUND_XMAX');
      fLoY := GISdb[FeaturesDB].MyData.GetFieldByNameAsInteger('BOUND_YMIN');
      fHiY := GISdb[FeaturesDB].MyData.GetFieldByNameAsInteger('BOUND_YMAX');
      ID :=GISdb[FeaturesDB].MyData.GetFieldByNameAsInteger(RecNoFName);
      for x2 := fLoX to fHiX do begin
        for y2 := fLoY to fHiY do begin
             if DEMGLB[UseDEM].GetElevMetersOnGrid(x2,y2,z) and (round(z) = ID) then begin
                DEMGlb[NewDEM].SetGridElevation(x2,y2,z2);
             end;
        end;
      end;
      GISdb[FeaturesDB].MyData.Next;
   end;
   EndProgress;
   GISdb[FeaturesDB].dbTablef.ShowStatus;
   {$IfDef RecordCreateGeomorphMaps} WriteLineToDebugFile('Computations done, FeaturesDB=' + FeaturesDB.ToString); {$EndIf}
   DEMGlb[NewDEM].SetUpMap(NewDEM,true,mtElevSpectrum);
   {$IfDef RecordCreateGeomorphMaps} WriteLineToDebugFile('TMapForm.Creategrid1Click out FeaturesDB=' + FeaturesDB.ToString); {$EndIf}
{$EndIf}
end;


procedure TMapForm.Immediateneighbordropoff1Click(Sender: TObject);
begin
   {$IfDef ExGeoStats}
   {$Else}
      MakeSingleNewDerivativeMap('i',MapDraw.DEMonMap);
   {$EndIf}
end;


procedure TMapForm.Terrainorganizationmaps1Click(Sender: TObject);
begin
   {$IfDef ExGeoStats}
   {$Else}
      SetAllOrganization(true);
      CreateAnOrganizationMap(MapDraw.DEMonMap);
   {$EndIf}
end;


procedure TMapForm.Multipleparameters1Click(Sender: TObject);
begin
   {$IfDef ExGeoStats}
   {$Else}
      {$IfDef RecordCreateGeomorphMaps} WriteLineToDebugFile('TMapForm.Multipleparameters1Click in'); {$EndIf}
         GetGeomorphBlockOpts(gbGrid,Self.MapDraw.DEMonMap,DEMGlb[Self.MapDraw.DEMonMap].FullDEMGridLimits);
         MatchThiscoverageareaandsamepixelsize1Click(Sender);
      {$IfDef RecordCreateGeomorphMaps}  WriteLineToDebugFile('TMapForm.Multipleparameters1Click out'); {$EndIf}
   {$EndIf}
end;

procedure TMapForm.Histogram1Click(Sender: TObject);
begin
   {$IfDef ExGeoStats}
   {$Else}
      SingleDEMHistogram(MapDraw.DEMonMap);
   {$EndIf}
end;


procedure TMapForm.Grainmovie1Click(Sender: TObject);
{$IfDef ExGeology}
begin
{$Else}
var
   SSOCalcDlg : TSSOCalcDlg;
   xsize,ysize,
   ColInc,RowInc,
   tBoxSize,
   i,j,Err,Len,x,y,Col,Row  : integer;
   BoxSizes,MovieList : tStringList;
   FName     : PathStr;
   Bitmap : tMyBitmap;
   s1s2,s2s3,Trend,rf : float64;
begin
   if MapDraw.DEMMap then begin
      MovieList := TStringList.Create;
      with MapDraw do with DEMGlb[DEMOnMap] do begin
         SSOCalcDlg := TSSOCalcDlg.Create(Application);
         with SSOCalcDlg do begin
            Edit2.Enabled := false;
            if (ShowModal = idCancel) then begin
               SSOCalcDlg.Free;
               exit;
            end;
            SSOCalcDlg.CheckNewValues;
         end;
         ColInc := round(MDDef.PointSeparation / AverageXSpace);
         RowInc := round(MDDef.PointSeparation / AverageYSpace);
         i := 1;
         BoxSizes := tStringList.Create;
         FName := ProgramRootDir + 'box size.txt';
         if FileExists(FName) then BoxSizes.LoadFromFile(FName)
         else begin
            BoxSizes.Add('Edit file to have region sizes you want in movie (m)');
            BoxSizes.Add('One value per line.');
            BoxSizes.Add('Save and close when done.');
            BoxSizes.SaveToFile(FName);
            ModalEditWindow(FName,'Region size for movie');
            BoxSizes.LoadFromFile(FName);
         end;

         for j := 0 to pred(BoxSizes.Count) do begin
            CopyImageToBitmap(Image1,Bitmap);
            Val(BoxSizes.Strings[j],tBoxSize,err);
            xsize := round(0.5 * tBoxSize / AverageXSpace);
            ysize := round(0.5 * tBoxSize / AverageYSpace);

            StartProgressAbortOption('Grain overlay ' + IntToStr(j) + '/' + IntToStr(pred(BoxSizes.Count)));
            Col := xSize;
            while Col < pred(DEMheader.NumCol)-xsize do begin
               if (Col >= MapCorners.BoundBoxDataGrid.xmin) and (Col <= MapCorners.BoundBoxDataGrid.xmax) then begin
                  Row := ysize;
                  while Row < pred(DEMheader.NumRow)-ysize do begin
                     if (Row >= MapCorners.BoundBoxDataGrid.ymin) and (Row <= MapCorners.BoundBoxDataGrid.ymax) then begin
                         if SimplePointSSOComputations(false,Col,Row,tBoxSize,s1s2,s2s3,Trend,rf) then begin
                            DataGridToScreen(Col,Row,x,y);
                            Len := round(s2s3*MDDef.GrainLengthMultiple);
                            PlotOrientedLine(Bitmap,x,y,Len,Trend,MDDef.GrainColor,MDDef.GrainLineWidth);
                         end;
                     end;
                     inc(Row,RowInc);
                  end;
               end;
               inc(Col,ColInc);
               if ShowSatProgress then UpdateProgressBar((Col - MapCorners.BoundBoxDataGrid.xmin) / (MapCorners.BoundBoxDataGrid.xmax- MapCorners.BoundBoxDataGrid.xmin));
               if WantOut then break;
            end;

            Bitmap.Canvas.Font.Size := 18;
            Bitmap.Canvas.TextOut(Image1.Width-120,Image1.Height-25,RealToString(tBoxSize,8,0) + ' m');

            if ShowSatProgress then EndProgress;
            FName := ExtractFilePath(LastDEMName) + 'TREND Overlay' + IntegerToString(i,-2) + MovieFileExt;
            inc(i);
            Bitmap.SaveToFile(FName);
            Bitmap.Free;
            MovieList.Add(FName);
         end;
         BoxSizes.Free;
         FName := ExtractFilepath(LastDEMName) + 'TREND Overlay' + '.MOV';
         MovieList.SaveToFile(FName);
         MovieList.Free;
         PetImage.MakeMovie(Fname);
         SSOCalcDlg.Free;
      end;
   end;
{$EndIf}
end;

procedure TMapForm.GRASSaspect1Click(Sender: TObject);
begin
   {$IfDef NoExternalPrograms}
   {$Else}
       MatchAnotherDEMMap(GRASSAspectMap(GeotiffDEMNameOfMap),MapDraw.DEMonMap);
   {$EndIf}
end;

procedure TMapForm.Parameterpercentiles1Click(Sender: TObject);
begin
   {$IfDef ExGeoStats}
   {$Else}
      MakeSingleNewDerivativeMap('P',MapDraw.DEMonMap);
   {$EndIf}
end;

procedure TMapForm.Parametricisotropicsmoothing1Click(Sender: TObject);
begin
   ThinDEM1Click(Sender);
end;


procedure TMapForm.FeatureGeomorphometry1Click(Sender: TObject);
{$IfDef ExGeostats}
begin
{$Else}
var
   fLoX,fHiX,fLoY,fHiY, ElevDEM,x2,y2,i,ID : integer;
   s1s2,s2s3,Trend,RoughnessFactor,DownDip : float64;
   z,MinZ,MaxZ : float32;
begin
  if FileExists(DEMGlb[MapDraw.DEMonMap].VATFileName) and ValidDB(FeaturesDB) then begin
      GetDEM(ElevDEM,false,'elevation DEM');
      if (ElevDEM <> 0) then begin
         GISdb[FeaturesDB].AddFieldToDataBase(ftFloat,'RELIEF',10,2);
         GISdb[FeaturesDB].AddFieldToDataBase(ftFloat,'S2S3',8,3);
         GISdb[FeaturesDB].AddFieldToDataBase(ftFloat,'TREND',5,1);
         GISdb[FeaturesDB].AddFieldToDataBase(ftFloat,'ROUGHNESS',8,3);
         GISdb[FeaturesDB].AddFieldToDataBase(ftFloat,'DOWN_DIP',5,1);
         i := 0;
         StartProgress('Features');
         while not GISdb[FeaturesDB].MyData.eof do begin
            inc(i);
            if (i mod 25 = 0) then UpdateProgressBar(i/GISdb[FeaturesDB].MyData.FiltRecsInDB);
            fLoX := GISdb[FeaturesDB].MyData.GetFieldByNameAsInteger('BOUND_XMIN');
            fHiX := GISdb[FeaturesDB].MyData.GetFieldByNameAsInteger('BOUND_XMAX');
            fLoY := GISdb[FeaturesDB].MyData.GetFieldByNameAsInteger('BOUND_YMIN');
            fHiY := GISdb[FeaturesDB].MyData.GetFieldByNameAsInteger('BOUND_YMAX');
            ID := GISdb[FeaturesDB].MyData.GetFieldByNameAsInteger(RecNoFName);
            MinZ := 99999;
            MaxZ := -99999;
            for x2 := fLoX to fHiX do begin
              for y2 := fLoY to fHiY do begin
                   if DEMGLB[MapDraw.DEMonMap].GetElevMetersOnGrid(x2,y2,z) and (round(z) = ID) then begin
                      if DEMGLB[ElevDEM].GetElevMetersOnGrid(x2,y2,z) then PetMath.CompareValueToExtremes(z,MinZ,MaxZ);
                   end;
              end;
            end;
            z := MaxZ - MinZ;
            if (z > 0) then begin
               GISdb[FeaturesDB].MyData.Edit;
               GISdb[FeaturesDB].MyData.SetFieldByNameAsFloat('RELIEF',z);
            end;
            if DEMGLB[ElevDEM].FeatureSSOComputations(MapDraw.DEMonMap,ID,fLoX,fLoY,fHiX,fHiY, s1s2,s2s3,Trend,RoughnessFactor,DownDip) then begin
               GISdb[FeaturesDB].MyData.Edit;
               GISdb[FeaturesDB].MyData.SetFieldByNameAsFloat('S2S3',s2s3);
               GISdb[FeaturesDB].MyData.SetFieldByNameAsFloat('TREND',Trend);
               GISdb[FeaturesDB].MyData.SetFieldByNameAsFloat('DOWN_DIP',DownDip);
               GISdb[FeaturesDB].MyData.SetFieldByNameAsFloat('ROUGHNESS',RoughnessFactor);
            end;
            GISdb[FeaturesDB].MyData.Next;
         end;
         EndProgress;
      end;
  end;
{$EndIf}
end;


procedure TMapForm.FeatureMigration(Dir : tCompassDirection);
label
   FoundOne;
var
   NewDEM,Map1,Map2,Col,Row,
   i,x,y,dx,dy : integer;
   Lat,Long,Spacing : float64;
   mName : PathStr;
   TStr : shortstring;
begin
   GetTwoCompatibleGrids('feature migration',true,Map1,Map2,false,true);
   ReadDefault('Max migration to check (pixels)',MDDef.MaxMigration);

   dx := 0;
   dy := 0;
   Spacing := DEMGlb[Map1].AverageSpace;
   if Dir in [cdN,cdS] then begin
     dy := 1;
     TStr := 'N-S';
   end;
   if Dir in [cdE,cdW] then begin
     dx := 1;
     TStr := 'E-W';
   end;
   if Dir in [cdSW,cdNE] then begin
      dx := 1;
      dy := 1;
      TStr := 'NE-SW';
      Spacing := DEMGlb[Map1].AverageDiaSpace;
   end;
   if Dir in [cdSE,cdNW] then begin
      dx := 1;
      dy := -1;
      TStr := 'NW-SE';
      Spacing := DEMGlb[Map1].AverageDiaSpace;
   end;

   mName := ptTrim(DEMGlb[Map1].AreaName) + '_to_' + ptTrim(DEMGlb[Map2].AreaName) + '_' + TStr + '_migration';
   NewDEM := DEMGlb[Map1].CloneAndOpenGridSetMissing(FloatingPointDEM,mName,euMeters);

   if ShowSatProgress then StartProgress('Migration');
   for Col := 0 to pred(DEMGlb[Map1].DEMheader.NumCol) do begin
      UpdateProgressBar(Col/DEMGlb[Map1].DEMheader.NumCol);
      for Row := 0 to pred(DEMGlb[Map1].DEMheader.NumRow) do begin
         x := 0;
         y := 0;
         if not DEMGlb[Map1].MissingDataInGrid(Col,Row) then begin
            for i := 0 to MDDef.MaxMigration do begin
               if DEMGlb[Map2].GridInDataSet(Col+x,Row+y) and (not DEMGlb[Map2].MissingDataInGrid(Col+x,Row+y)) then begin
                  DEMGlb[Map2].DEMGridToLatLongDegree(Col+x,Row+y,Lat,Long);
                  DEMGlb[NewDEM].SetGridElevation(Col,Row,i * spacing);
                  goto FoundOne;
               end;
               if DEMGlb[Map2].GridInDataSet(Col-x,Row-y) and (not DEMGlb[Map2].MissingDataInGrid(Col-x,Row-y)) then begin
                  DEMGlb[Map2].DEMGridToLatLongDegree(Col-x,Row-y,Lat,Long);
                  DEMGlb[NewDEM].SetGridElevation(Col,Row,-i * spacing);
                  goto FoundOne;
               end;
               x := x + dx;
               y := y + dy;
            end;
            FoundOne:;
         end;
      end;
   end;
   EndProgress;
   DEMGlb[NewDEM].SetUpMap(NewDEM,true);
end;


procedure TMapForm.Grainbyregionsize2Click(Sender: TObject);
{$IfDef ExGeoStats}
begin
{$Else}
var
   xDEMg1,yDEMg1 : float64;
begin
   DEMGlb[MapDraw.DEMonMap].LatLongDegreeToDEMGrid(RightClickLat,RightClickLong,xDEMg1,yDEMg1);
   Grainbyregionsize1Click(Sender);
   FabricOptions.xDEMg1 := xdemg1;
   FabricOptions.yDEMg1 := ydemg1;
   FigureGrainByRegionSize(round(xDEMg1),round(yDEMg1));
{$EndIf}
end;


procedure TMapForm.FabricAtPoint1Click(Sender: TObject);
begin
{$IfDef ExTopoGrain}
{$Else}
   ChangeDEMNowDoing(GetPointFabric);
   DrawTopoGrainOverlay(Self,Nil,true);
{$EndIf}
end;


procedure TMapForm.Falsecolorpansharpen1Click(Sender: TObject);
begin

end;

{$IfDef ExTopoGrain}
{$Else}

procedure TMapForm.GetFabricAtPoint(x,y : integer);
var
   xp,yp,i  : integer;
   Lat,Long : float64;
   Results : tStringList;


   procedure ResultsForDEM(DEM,x,y : integer);
   var
      {$IfDef ExWaveLengthHeight}
      {$Else}
         WavelengthMean,WavelengthMedian,WavelengthStdDev,HeightMean,HeightMedian,HeightStd,//,Distance,Relief,
      {$EndIf}
      s1s2,s2s3,Trend,rf{,lat1,long1,lat2,long2} : float64;
      z : float32;
      xp,yp : integer;
      //ProfileData : tMyData;
      Bitmap : tMyBitmap;

         {$IfDef ExWaveLengthHeight}
         {$Else}
            procedure PlotFilteredTable(Table : tMyData; Filter : ANSIString; Sym : tDrawingSymbol; size : byte; Color : tPlatformColor);
            var
               Lat, Long : float64;
            begin
               Table.ApplyFilter(Filter);
               while not Table.Eof do begin
                   Lat := Table.GetFieldByNameAsFloat('LAT');
                   Long := Table.GetFieldByNameAsFloat('LONG');
                   MapDraw.MapSymbolAtLatLongDegree(Image1.Canvas,Lat,Long,Sym,Size,Color);
                   Table.Next;
               end;
            end;
         {$EndIf}

   begin
      {$IfDef RecordGetFabricAtPoint}  WriteLineToDebugFile('DEM=' + DEMGlb[DEM].AreaName): {$EndIf}
      if DEMGlb[DEM].GetElevMetersOnGrid(x,y,z) and DEMGlb[DEM].SimplePointSSOComputations(true,x,y,MDDef.SSOBoxSizeMeters,s1s2,s2s3,Trend,rf) then begin
         if (DEM = MapDraw.DEMonMap) then begin
            MapDraw.DEMGridToScreen(x,y,xp,yp);
            CopyImageToBitmap(Image1,Bitmap);
            PlotOrientedLine(Bitmap,xp,yp,round(s2s3*MDDef.GrainLengthMultiple),Trend,ClaRed,MDDef.GrainLineWidth);
            Image1.Picture.Graphic := bitmap;
            Bitmap.Free;
         end;

         Results.Add('');
         Results.Add(DEMGlb[DEM].AreaName);
         Results.Add('s1s2=' + RealToString(s1s2,-6,-2));
         Results.Add('s2s3=' + RealToString(s2s3,-6,-2));
         Results.Add('Trend=' + RealToString(Trend,-6,-1) + DegSym);

         (*
         if DEMGlb[DEM].FindReliefInflectionGraph(x,y,Distance,Relief) then begin
            Results.Add('');
            Results.Add('Relief inflection');
            Results.Add(' wavelength=' + RealToString(Distance,-18,-1) + 'm');
            Results.Add(' relief=' + RealToString(Relief,-18,-2) + 'm');
         end;
         *)
         {$IfDef ExWaveLengthHeight}
         {$Else}
             if MDDef.FindWavelength then begin
                DEMGlb[MapDraw.DEMonMap].DEMGridToLatLongDegree(x,y,Lat,Long);
                if DEMGlb[MapDraw.DEMonMap].GetPerpendicularLineEnd(Lat,Long,MDdef.WavelengthCompDist,Trend,Lat1,Long1,Lat2,Long2) then begin
                   MDDef.ForceCrestComputations := true;
                   LOSComputeOnly(DEMGlb[MapDraw.DEMonMap],0,lat1,long1,lat2,long2,0,0,0);
                   if MDDef.PlotCrest then begin
                      PlotFilteredTable(ProfileData, 'PEAK=' + QuotedStr('Y'),FilledBox,3,claLime);
                      PlotFilteredTable(ProfileData, 'PIT=' + QuotedStr('Y'),FilledBox,3,claRed);
                   end;
                   FindWavelengthStats(0,WavelengthMean,WavelengthMedian,WavelengthStdDev,HeightMean,HeightMedian,HeightStd);
                   //ProfileData.Destroy;
                    //GetWaveSpacingHeightAtPoint(True,Lat,Long,Trend,WavelengthMean,WavelengthMedian,WavelengthStdDev,HeightMean,HeightMedian,HeightStd) then begin
                   Results.Add('');
                   AddToWaveSpacingHeightResults(results,WavelengthMean,WavelengthMedian,WavelengthStdDev,HeightMean,HeightMedian,HeightStd);
                end;
            end;
         {$EndIf{}
      end;
   end;


begin
   {$IfDef RecordGetFabricAtPoint} WriteLineToDebugFile('GetFabricAtPoint in'): {$EndIf}
   if MapDraw.ValidDEMonMap then begin
      ShowHourglassCursor;
      MapDraw.ScreenToLatLongDegree(x,y,Lat,Long);
      Results := tStringList.Create;
      Results.Add(DEMGlb[MapDraw.DEMonMap].DEMLocationString(x,y));
      ResultsForDEM(MapDraw.DEMOnMap,x,y);
      for I := 1 to NumDEMDataSetsOpen do begin
         if ValidDEM(i) and (I <> MapDraw.DEMonMap) then begin
            DEMGLB[i].LatLongDegreeToDEMGridInteger(Lat,Long,xp,yp);
            ResultsForDEM(i,xp,yp);
         end;
      end;
      if (SSOCalcDlg = Nil) then Petmar.DisplayAndPurgeStringList(Results,'Point fabric ' + DEMGlb[MapDraw.DEMonMap].DEMLocationString(x,y))
      else SSOCalcDlg.Memo1.Lines := Results;
      if MDDef.PointFabricTerrBlowup then Terrainblowup1Click(Nil);
      ShowDefaultCursor;
   end;
   {$IfDef RecordGetFabricAtPoint} WriteLineToDebugFile('GetFabricAtPoint out'); {$EndIf}
end;
procedure TMapForm.GetGRASSextensions1Click(Sender: TObject);
begin
   GetGrassExtensionsNow(GeotiffDEMNameOfMap);
end;

{$EndIf}

procedure TMapForm.Slopelogtangent1Click(Sender: TObject);
begin
   {$IfDef ExGeostats}
   {$Else}
      SetAllSlopes(false);
      MDDef.DoSlopeLogTan := true;
      MakeMomentsGrid(MapDraw.DEMonMap,'S');
   {$EndIf}
end;

procedure TMapForm.Slopemm1Click(Sender: TObject);
begin
   {$IfDef ExGeoStats}
   {$Else}
      SetAllSlopes(false);
      MDDef.DoSlopeMperM := true;
      MakeMomentsGrid(MapDraw.DEMonMap,'S');
   {$EndIf}
end;

procedure TMapForm.Slopemoments1Click(Sender: TObject);
begin
   {$IfDef ExGeostats}
   {$Else}
      MakeMomentsGrid(MapDraw.DEMonMap,'s',MDDef.MomentsBoxSizeMeters);
   {$EndIf}
end;

procedure TMapForm.Elevationmoments1Click(Sender: TObject);
begin
   {$IfDef ExGeostats}
   {$Else}
      MakeMomentsGrid(MapDraw.DEMonMap,'e',MDDef.MomentsBoxSizeMeters);
   {$EndIf}
end;


procedure TMapForm.Elevationpercentiles1Click(Sender: TObject);
var
   i,db : integer;
   fName : PathStr;
   TStr : shortstring;
   results : tStringList;
   Graph : tThisBaseGraph;
begin
   if (DEMGlb[MapDraw.DEMonMap].ZPercens = Nil) then begin
      DEMGlb[MapDraw.DEMonMap].GetElevPercentiles(DEMGlb[MapDraw.DEMonMap].FullDEMGridLimits);
   end;
   results := tStringList.Create;
   Results.Add('PERCENTILE,VALUE');
   for I := 1 to 999 do begin
      Tstr := RealToString(0.1 * i,-12,-3) + ',' + RealToString(DEMGlb[MapDraw.DEMonMap].zPercens^[i],-12,-3);   //temp variable for debugging
      Results.Add(TStr );
   end;
   Fname := Petmar.NextFileNumber(MDTempDir,DEMGlb[MapDraw.DEMonMap].AreaName + '_percentiles_',DefaultDBExt);
   db := StringListToLoadedDatabase(Results,fName);
   Graph := GISdb[db].CreateScatterGram('PERCENTILE','VALUE',true);
   Graph.GraphDraw.LLcornerText := DEMGlb[MapDraw.DEMonMap].AreaName;
   Graph.RedrawDiagram11Click(Nil);
end;


procedure TMapForm.Walls1Click(Sender: TObject);
begin
   {$IfDef ExGeostats}
   {$Else}
      PitSpireDefaults(Self,4);
   {$EndIf}
end;


procedure TMapForm.Rugosity1Click(Sender: TObject);
begin
   {$IfDef ExGeoStats}
   {$Else}
      MakeSingleNewDerivativeMap('g',MapDraw.DEMonMap);
   {$EndIf}
end;

procedure TMapForm.RVTgridcreation1Click(Sender: TObject);
begin
   {$IfDef ExGeoStats}
   {$Else}
      RVTgrids(MapDraw.DEMonMap);
   {$EndIf}
end;


procedure TMapForm.Counts1Click(Sender: TObject);
begin
    {$IfDef ExGeostats}
    {$Else}
       GridCoOccurrence(true,0,0,false);
    {$EndIf}
end;


procedure TMapForm.Normalizeparameter1Click(Sender: TObject);
begin
   {$IfDef ExGeoStats}
   {$Else}
      MakeSingleNewDerivativeMap('n',MapDraw.DEMonMap);
   {$EndIf}
end;


procedure TMapForm.Normalizeto30m1Click(Sender: TObject);
begin
   MakeTRIGrid(MapDraw.DEMonMap,nm30m,true);
end;

procedure TMapForm.Excessiveslopes2Click(Sender: TObject);
{$IfDef ExGeoStats}
begin
{$Else}
var
   Col,Row,xp,yp,RemoveBuffer,NewDEM : integer;
   ExcessSlope,z : float32;
begin
   ExcessSlope := 5;
   ReadDefault('Excess slope',ExcessSlope);
   RemoveBuffer := 0;
   ReadDefault('Buffer (pixels)',RemoveBuffer);

   NewDEM := MakeSingleNewDerivativeMap('S',MapDraw.DEMonMap);

   with MapDraw,DEMGlb[DEMonMap] do begin
      if ShowSatProgress then Startprogress('Slope removal');
      for Col := 1 to (DEMheader.NumCol-2) do begin
         if ShowSatProgress and (col mod 25 = 0) then UpdateProgressBar(Col/DEMheader.NumCol);
         for Row := 1 to (DEMheader.NumRow-2) do begin
            if DEMGlb[NewDEM].GetElevMetersOnGrid(Col,Row,z) then begin
               if (z > ExcessSlope) then begin
                  for yp := 0-RemoveBuffer to 0+RemoveBuffer do begin
                     for xp := 0-RemoveBuffer to 0+RemoveBuffer do begin
                        DEMGlb[MapDraw.DEMonMap].SetGridMissing(Col,Row);
                     end;
                  end;
               end;
            end;
         end;
      end;
      if ShowSatProgress then EndProgress;
   end;
   RespondToChangedDEM;
{$EndIf}
end;


procedure TMapForm.FindSlopePoints(Memo1 : tMemo; SlopeLimit : float64; IHSMerge : boolean = false);
var
    Col,Row,xp,yp,Bad,Total : integer;
    BitMap : tMyBitmap;
    SlopeAsp : tSlopeAspectRec;
begin
    if MapDraw.ValidDEMonMap then with DEMGlb[MapDraw.DEMonMap] do begin
       if ShowSatProgress then StartprogressAbortOption('Slopes');
       Bad := 0;
       Total := 0;
       if IHSMerge then CloneImageToBitmap(Image1,Bitmap);

       for Col := MapDraw.MapAreaDEMGridLimits.XGridLow to (MapDraw.MapAreaDEMGridLimits.XGridHigh) do begin
          if ShowSatProgress and (col mod 25 = 0) then UpdateProgressBar(Col/DEMheader.NumCol);
          for Row := MapDraw.MapAreaDEMGridLimits.YGridLow to (MapDraw.MapAreaDEMGridLimits.YGridHigh) do begin
             if GetSlopeAndAspect(Col,Row,SlopeAsp) then begin
                inc(Total);
                if (SlopeAsp.SlopePercent > SlopeLimit) then begin
                   inc(Bad);
                   MapDraw.DEMGridToScreen(Col,Row,xp,yp);
                   if MapDraw.OnScreen(xp,yp) then begin
                      if IHSMerge then ScreenSymbol(Bitmap.Canvas,xp,yp,DrSymbol)
                      else ScreenSymbol(Image1.Canvas,xp,yp,DrSymbol);
                   end;
                end;
             end;
          end;
          if WantOut then Break;
       end;
       if ShowSatProgress then EndProgress;
       if IHSMerge then IHSmergeOntoMap(Bitmap);
       Memo1.Lines.Add('Slopes > ' + RealToString(SlopeLimit,-8,-2) + '%: ' + RealToString(100 * Bad/ Total,-12,2) + '% of DEM');
    end;
end;

procedure TMapForm.Both1Click(Sender: TObject);
begin
   {$IfDef ExGeostats}
   {$Else}
      CreateOpennessMap(MapDraw.DEMonMap,true,true);
   {$EndIf}
end;


procedure TMapForm.Variancecovariance1Click(Sender: TObject);
{$IfDef ExGeoStats}
begin
{$Else}
begin
   {$IfDef RecordSat} WriteLineToDebugFile('TMapForm.Variancecovariance1Click in'); {$EndIf}
   if (MapDraw.MultiGridOnMap = 0) then All2Click(Sender);
   ComputeVarCoVarAndPrincipalComponents(0,MultiGridArray[MapDraw.MultiGridOnMap],Nil);
   {$IfDef RecordSat} WriteLineToDebugFile('TMapForm.Variancecovariance1Click out'); {$EndIf}
{$EndIf}
end;


procedure TMapForm.Pointdensity1Click(Sender: TObject);
begin
   {$IfDef ExGeoStats}
   {$Else}
      MakeSingleNewDerivativeMap('p',MapDraw.DEMonMap);
   {$EndIf}
end;

procedure TMapForm.Mask2Click(Sender: TObject);
begin
   {$IfDef ExGeoStats}
   {$Else}
      MakeSingleNewDerivativeMap('B',MapDraw.DEMonMap);
   {$EndIf}
end;


procedure TMapForm.N2bandscattergram1Click(Sender: TObject);
var
   GridLimits : tGridLimits;
begin
    {$IfDef ExGeoStats}
    {$Else}
       DEMStat.GridScatterGram(GridLimits);
    {$EndIf}
end;


procedure TMapForm.Histogram3Click(Sender: TObject);
begin
   {$IfDef ExGeoStats}
   {$Else}
      if (MapDraw.ValidDEMonMap) then SingleDEMHistogram(MapDraw.DEMonMap,true);
   {$EndIf}
end;


procedure TMapForm.Findpeaks1Click(Sender: TObject);
begin
   {$IfDef ExGeostats}
   {$Else}
      DEMOptions.PitSpireDefaults(Self,1);
   {$EndIf}
end;

procedure TMapForm.Percent1Click(Sender: TObject);
begin
   DEMGlb[MapDraw.DEMonMap].SelectionMap.MapDraw.MapType := mtElevRainbow;
   ChangeElevUnits(euPercent);
end;

procedure TMapForm.Percentages1Click(Sender: TObject);
begin
   {$IfDef ExGeostats}
   {$Else}
      GridCoOccurrence(true,0,0,true);
   {$EndIf}
end;


procedure TMapForm.Convergenceindex1Click(Sender: TObject);
begin
   {$IfDef ExGeoStats}
   {$Else}
      MakeSingleNewDerivativeMap('c',MapDraw.DEMonMap);
   {$EndIf}
end;

procedure TMapForm.Allreliefmeasures1Click(Sender: TObject);
begin
   {$IfDef ExGeostats}
   {$Else}
       MakeMomentsGrid(MapDraw.DEMonMap,'G');
   {$EndIf}
end;


procedure TMapForm.Allslopeaspect1Click(Sender: TObject);
begin
   {$IfDef ExGeostats}
   {$Else}
      SetAllSlopes(true);
      MakeMomentsGrid(MapDraw.DEMonMap,'S');
   {$EndIf}
end;

procedure TMapForm.Roads1Click(Sender: TObject);
begin
   {$IfDef ExGeostats}
   {$Else}
      DEMOptions.PitSpireDefaults(Self,5);
   {$EndIf}
end;


procedure TMapForm.Searchpowerlines1Click(Sender: TObject);
begin
   {$IfDef ExGeostats}
   {$Else}
      PitSpireDefaults(Self,3);
   {$EndIf}
end;

procedure TMapForm.Airballdirtball1Click(Sender: TObject);
begin
   AirBallDirtBallMap(MapDraw.DEMonMap,0,0);
end;


procedure TMapForm.Algorithms1Click(Sender: TObject);
begin
   {$IfDef MultipleCurvatureMethods}
       ReadDefault('Curvature box size (pixels)',MDDef.CurvRegionSize);
       MDDef.CurvatureMethod := cmShary;
       MakeMomentsGrid(MapDraw.DEMonMap,'A');
       MDDef.CurvatureMethod := cmHeerdegenAndBeran;
       MakeMomentsGrid(MapDraw.DEMonMap,'A');
       MDDef.CurvatureMethod := cmZevenbergenAndThorne;
       MakeMomentsGrid(MapDraw.DEMonMap,'A');
       MDDef.CurvatureMethod := cmEvans;
       MakeMomentsGrid(MapDraw.DEMonMap,'A');
    {$EndIf}
end;


procedure TMapForm.SetMaskGrid1Click(Sender: TObject);
begin
   {$IfDef ExGeoStats}
   {$Else}
      GetDEM(GridMaskDEM,true,'Grid to use as mask');
      if Sender = Removemissingpointsinmask1 then begin
         MaskGrid(Self,MapDraw.DEMonMap,true);
      end
      else begin
         ReadDefault('Max valid value for mask',MaskMaxVal);
         ReadDefault('Min valid value for mask',MaskMinVal);
         SetMaskGrid1.Caption := 'Mask grid, ' + DEMGlb[GridMaskDEM].AreaName + ' ' +  RealToString(MaskMinVal,-18,-2) +  ' to ' + RealToString(MaskMaxVal,-18,-2);
      end;
      CheckProperTix;
   {$EndIf}
end;

procedure TMapForm.MaskDEM1Click(Sender: TObject);
begin
   {$IfDef ExGeoStats}
   {$Else}
      MaskGrid(Self,MapDraw.DEMonMap,true);
   {$EndIf}
end;

procedure TMapForm.Pointsnotmeetingcriteria1Click(Sender: TObject);
begin
   {$IfDef ExGeoStats}
   {$Else}
      MaskGrid(Self,MapDraw.DEMonMap,false);
   {$EndIf}
end;


procedure TMapForm.Featuremigration2Click(Sender: TObject);
begin
   {$IfDef ExGeostats}
   {$Else}
      CalculateFeatureMigration(self);
   {$EndIf}
end;

procedure TMapForm.Gridcorrelations1Click(Sender: TObject);
begin
   {$IfDef ExGeostats}
   {$Else}
      GridCorrelationMatrix;
   {$EndIf}
end;


procedure TMapForm.errainsunblocking1Click(Sender: TObject);
begin
   {$IfDef ExGeography}
   {$Else}
      ScreenSymbol(Image1.Canvas,LastX,LastY,Box,3,claRed);
      DEMOptions.SetHorizonOptions(Self,RightClickLat,RightClickLong);;
      Sun_Position.SunAndHorizon(Self,MapDraw.DEMOnMap,RightClickLat,RightClickLong);
   {$EndIf}
end;

procedure TMapForm.TestMD1Click(Sender: TObject);
begin
   {$IfDef RecordDrape} WriteLineToDebugFile('TMapForm.TestMD1Click in'); {$EndIf}

   {$IfDef IncludeFMX3DMesh}
      LoadMDMeshMap(Self.MapDraw);
   {$EndIf}
end;

procedure TMapForm.Lntransform1Click(Sender: TObject);
begin
    Multiplyzvalues1Click(Sender);
end;


procedure TMapForm.Logbase10transform1Click(Sender: TObject);
begin
   Multiplyzvalues1Click(Sender);
end;

procedure TMapForm.LSTfromemissivity1Click(Sender: TObject);
begin
   SatLSTatPoint(LastX,LastY);
end;

procedure TMapForm.Reliefavgelevstdelev1Click(Sender: TObject);
begin
   {$IfDef ExGeology}
   {$Else}
      MakeMomentsGrid(MapDraw.DEMonMap,'R',MDDef.ReliefBoxSizeMeters);
   {$EndIf}
end;


procedure TMapForm.OAradiance1Click(Sender: TObject);
begin
   Surfaceradiance1Click(Sender);
end;

procedure TMapForm.OAreflectance1Click(Sender: TObject);
begin
   DEMDef_Routines.SaveBackupDefaults;
   MDDef.dnConvert := dncReflectance;
   NewSatWindow(nsbTOARef);
   DEMDef_Routines.RestoreBackupDefaults;
end;

procedure TMapForm.OArelectancewithsunpositioncorrection1Click(Sender: TObject);
begin
   DEMDef_Routines.SaveBackupDefaults;
   MDDef.dnConvert := dncReflectSun;
   NewSatWindow(nsbTOARefSolar);
   DEMDef_Routines.RestoreBackupDefaults;
end;


procedure TMapForm.OPEXPoseidonimport1Click(Sender: TObject);
begin
   {$IfDef ExAltimeter}
   {$Else}
      ImportTopexCD(Self);
   {$EndIf}
end;


procedure TMapForm.Minimumfilter1Click(Sender: TObject);
begin
   ThinDEM1Click(Sender);
end;

procedure TMapForm.Minimumoftwogrids1Click(Sender: TObject);
begin
   {$IfDef ExAdvancedGIS}
   {$Else}
      GridMinimum;
   {$EndIf}
end;

procedure TMapForm.MaxDN1Click(Sender: TObject);
begin
   {$IfDef ExAdvancedSats}
   {$Else}
      MultiGridArray[MapDraw.MultiGridOnMap].CreateMaxValueGrid;
   {$EndIf}
end;

procedure TMapForm.Maximal1Click(Sender: TObject);
var
   NewGrid : integer;
begin
   {$IfDef NoExternalPrograms}
   {$Else}
       NewGrid := WBT_MaximalCurvature(GeotiffDEMNameOfMap);
       MatchAnotherDEMMap(NewGrid,MapDraw.DEMonMap);
   {$EndIf}
end;

procedure TMapForm.Maximizeforscreen1Click(Sender: TObject);
begin
   SetDefaultMapSizeToScreen;
   RedrawMapDefaultsSize;
end;


procedure TMapForm.Maximumfilter1Click(Sender: TObject);
begin
   ThinDEM1Click(Sender);
end;


procedure TMapForm.Changemap1Click(Sender: TObject);
begin
   {$IfDef ExGeostats}
   {$Else}
      MakeChangeMap(0,0,DEMGlb[MapDraw.DEMonMap].FullDEMGridLimits);
   {$EndIf}
end;


procedure TMapForm.Changemap2Click(Sender: TObject);
begin
   ModifyChangeMapSettings(Self);
end;

procedure TMapForm.N3Drotatingglobe1Click(Sender: TObject);
begin
   StartEarthRotation(MapDraw.FullMapfName);
end;

procedure TMapForm.N3DviewwithtwoDEMs1Click(Sender: TObject);
var
   DEM1,DEM2 : integer;
begin
   GetTwoCompatibleGrids('Two OpenGL 3D views',false,DEM1,DEM2, false);
   if (DEM1 <> 0) and (DEM2 <> 0) then begin
      Map3d := MapTo3DView(DEMGlb[DEM1].SelectionMap.MapDraw);
      Map3D.DoMap(DEMGlb[DEM2].SelectionMap.MapDraw);
   end;
end;


procedure TMapForm.NDBIbuiltup1Click(Sender: TObject);
begin
   NewSatWindow(nsbNDBIbuilding);
end;


procedure TMapForm.Numbercells1Click(Sender: TObject);
begin
   WBT_FlowAccumulation(True,False,True,GeotiffDEMNameOfMap);
end;

procedure TMapForm.Numberimmediateneighbors1Click(Sender: TObject);
begin
   ThinDEM1Click(Sender);
end;


procedure TMapForm.NumericgridwithVATtobytegridwithcodes1Click(Sender: TObject);
var
   fName2 : PathStr;
   x,y,Code,NewDEM : integer;
   z : float32;
   Color : tRGBTriple;
   Vat : tStringList;
   Hist : array[1..50] of int64;
begin
   for Code := 1 to 50 do Hist[Code] := 0;

   StartProgress('Reclassify');
   for x := 0 to pred(DEMGlb[MapDraw.DEMonMap].DEMheader.NumCol) do begin
      if (x mod 50 = 0) then UpDateProgressBar(x/DEMGlb[MapDraw.DEMonMap].DEMheader.NumCol);
      for y := 0 to pred(DEMGlb[MapDraw.DEMonMap].DEMheader.NumRow) do begin
         if DEMGlb[MapDraw.DEMonMap].GetElevMetersOnGrid(x,y,z) then begin
            Color := ColorFromZColorTable(MapDraw.ZColorTable,z, Code);
            DEMGlb[MapDraw.DEMonMap].SetGridElevation(x,y,Code);
            inc(Hist[Code]);
         end;
      end;
   end;

   NewDEM := DEMGlb[MapDraw.DEMonMap].ResaveNewResolution(fcSaveByte);
   fName2 := MDTempDir + 'reclasify_' + DEMGlb[MapDraw.DEMonMap].AreaName + '.dem';
   DEMGlb[NewDEM].WriteNewFormatDEM(fName2);

   Vat := tStringList.Create;
   Vat.add('VALUE,NAME,N,USE,COLOR');
   for Code := 1 to 50 do
      if Hist[Code] > 0 then begin
         Vat.add(IntToStr(Code) + ',' + MapDraw.ZColorTable.zTableLabels[Code] + ',' + IntToStr(Hist[Code]) + ',Y,' +
            IntToStr(ConvertPlatformColorToTColor(MapDraw.ZColorTable.zTableColors[Code])) );
      end;

   fName2 := ChangeFileExt(fName2,'.vat.dbf');
   StringList2CSVtoDB(vat,fName2,true);
   DEMGlb[NewDEM].VATFileName := fName2;
   DEMglb[NewDEM].CheckMaxMinElev;
   DEMglb[NewDEM].SetUpMap(NewDEM,true,mtDEMVATTable);

   ReloadDEMClick(Sender);
end;

procedure TMapForm.Geostationarysatellitevisibility1Click(Sender: TObject);
begin
   SetHorizonOptions;
end;


procedure TMapForm.GeoTIFF1Click(Sender: TObject);
begin
   DEMGlb[MapDraw.DEMonMap].SaveAsGeotiff('');
end;



function TMapForm.DrawSlopeMask(Color : tColor; maxSlope : float64; DisplayAndPurge : boolean) : tMyBitmap;
var
  Col,Row,x,y : integer;
  SlopeAspectRec : tSlopeAspectRec;
  xg,yg : float32;
begin
   DoFastMapRedraw;
   PetImage.CloneImageToBitmap(Image1,Result);
   StartProgress('Slope mask');
   with DEMGLB[MapDraw.DEMonMap],MapDraw.MapCorners do begin

      if (MapDraw.MapZoomFactor > 0.95) then begin
         {$IfDef RecordColorMasking} WriteLineToDebugFile('use screen resolution'); {$EndIf}
         for y := 0 to pred(Result.Height) do begin
            if (y mod 25 = 0) then UpdateProgressBar(y/Result.Height);
            for x := 0 to pred(Result.Width) do begin
               MapDraw.ScreenToDEMGrid(x,y,xg,yg);
               if GetSlopeAndAspect(round(xg),round(yg),SlopeAspectRec) and (SlopeAspectRec.SlopePercent <= MaxSlope) then begin
                  Result.Canvas.Pixels[x,y] := Color;
               end;
            end;
         end;
      end
      else begin
         for Col := trunc(BoundBoxDataGrid.xmin) to round(BoundBoxDataGrid.xmax) do begin
            if ShowSatProgress then UpdateProgressBar(Col/DEMheader.NumCol);
            for Row := trunc(BoundBoxDataGrid.ymin) to round(BoundBoxDataGrid.ymax) do begin
                if GetSlopeAndAspect(Col,Row,SlopeAspectRec) and (SlopeAspectRec.SlopePercent <= MaxSlope) then begin
                   MapDraw.DEMGridToScreen(Col,Row,x,y);
                   Result.Canvas.Pixels[x,y] := Color;
                end;
            end;
         end;
      end;
   end;
   if DisplayAndPurge then IHSmergeOntoMap(Result);
   EndProgress;
end;



procedure TMapForm.RoughnessfromSSO1Click(Sender: TObject);
begin
   CreateRoughnessMap(MapDraw.DEMonMap);
end;

procedure TMapForm.Roundtobylerange1Click(Sender: TObject);
begin
   DEMGlb[Mapdraw.DEMonMap].RoundToByteRange;
   RespondToChangedDEM;
end;

procedure TMapForm.Roundtobyterangepercentiles1Click(Sender: TObject);
begin
   DEMGlb[MapDraw.DEMonMap].RoundZRangeByPercentileToByte;
   RespondToChangedDEM;
end;

procedure TMapForm.Roundtointegers1Click(Sender: TObject);
begin
   Multiplyzvalues1Click(Sender);
end;

procedure TMapForm.Suminbox1Click(Sender: TObject);
begin
   ThinDEM1Click(Sender);
end;

procedure TMapForm.SummaryallopenDEMsGrids1Click(Sender: TObject);
begin
   MakeDEMsummaryTable;
end;

procedure TMapForm.Sunposition1Click(Sender: TObject);
begin
   All3Click(Sender);
end;

procedure TMapForm.Surfaceradiance1Click(Sender: TObject);
begin
   DEMDef_Routines.SaveBackupDefaults;
   MDDef.dnConvert := dncRadiance;
   NewSatWindow(nsbTOARadiance);
   DEMDef_Routines.RestoreBackupDefaults;
end;


procedure TMapForm.Batchrequiredantennaheights1Click(Sender: TObject);
var
   Table : tMyData;
   fName : PathStr;
   GridLimits : tGridLimits;
   i,DecFactor,ThinDEM : integer;
   sName : shortstring;
begin
   fName := MainMapData;
   if GetFileFromDirectory('Sensors for batch intervisibility',DBNameMask,fName) then begin
       Table := tMyData.Create(fName);
       Table.ApplyFilter( 'USE=' + QuotedStr('Y'));
       ShowSatProgress := false;
       MDDef.DoReqAntHigh := true;
       MDDef.DoReqFlyHigh := false;
       if (Table.FiltRecsInDB = 0) then begin
          MessageToContinue('No sensors marked to use');
       end
       else begin
          DecFactor := 1;
          ReadDefault('Decimation factor',DecFactor);
          i := 0;
          while Not Table.eof do begin
             inc(i);
             wmDEM.SetPanelText(0,IntToStr(i) + '/' + IntToStr(Table.FiltRecsInDB));
             sName := Table.GetFieldByNameAsString('NAME');
             if sName = '' then begin
                Table.Edit;
                sName := 'sensor_'+ IntToStr(i);
                Petmar.GetString('Sensor name',sName,false,ReasonableTextChars);
                Table.SetFieldByNameAsString('NAME',sName);
             end;

             fName := ExtractFilePath(DEMGLB[MapDraw.DEMonMap].DEMFileName) + 'Ant_need_' + sName + '.flt';
             if not FileExists(fName) then begin
                MakeRequiredAntennaMap('Antennas',MapDraw.DEMonMap,
                    Table.GetFieldByNameAsFloat('LAT'), Table.GetFieldByNameAsFloat('LONG'),
                    Table.GetFieldByNameAsFloat('SENSOR_UP'), Table.GetFieldByNameAsFloat('SENSOR_RNG'),
                    false,0,360);
                if (AntDEM <> 0) then begin
                   if (DecFactor > 1) then begin
                      ThinDEM := DEMGlb[AntDEM].ThinThisDEM('',DecFactor);
                      CloseSingleDEM(AntDEM);
                      AntDEM := ThinDEM;
                   end;
                   GridLimits := DEMGlb[AntDEM].FullDEMGridLimits;
                   DEMGlb[AntDEM].FilledGridBox(GridLimits);
                   DEMGlb[AntDEM].RectangleSubsetDEM(GridLimits,fName);
                   CloseSingleDEM(AntDEM);
                end;
             end;
             Table.Next;
          end;
       end;
       Table.Destroy;
       ShowSatProgress := true;
   end;
end;


procedure TMapForm.Optimumindexfactor1Click(Sender: TObject);
begin
   {$IfDef ExSat}
   {$Else}
   SatImage[MapDraw.SATonMap].OptimumIndexFactor;
   {$EndIf}
end;

procedure TMapForm.ospecifiedlongitude1Click(Sender: TObject);
var
   Long : float64;
begin
   Long := -130;
   ReadDefault('New west edge longitude',Long);
   DEMGlb[MapDraw.DEMonMap].ShiftGlobalDEM(Long);
   MapDraw.DefineNewDEMMap(MapDraw.DEMonMap,MapDraw.MapType);
   DoCompleteMapRedraw;
end;


function ShapeFileDigitizingUnderway(DEMNowDoing :  tDEMDoingWhat) : boolean;
begin
   Result := (DEMNowDoing in [CalculateArea,ShapeLine,ShapePolygon,ShapeFirstLine,ShapeFirstPolygon,OutlineDBIrregularMask]);
end;

function StartMapRoamOps(WhatTo : tDEMDoingWhat) : boolean;
begin
   Result := ShapeFileDigitizingUnderway(WhatTo) or  (DEMNowDoing in [CalculateVolume,SubsetByOutline,
            SubsetHole,SubsetLake,SubsetByOutline,ReplaceValuesByOutline,FillHolesByOutline,
            StreamDistance,NewTrack,RouteObservation,FirstZigDistance,
            FirstDistancePoint,MultipleLOS, SeekingTopoProfile,DigitizeContourPoint,DigitizeContourStream,SeekingFirstCircleFly,
            SeekingFlyThroughRoute,SeekingStreamProfile]);
end;

function PolygonDigitizing(DEMNowDoing :  tDEMDoingWhat) : boolean;
begin
   PolygonDigitizing := DEMNowDoing in [OutlineDBIrregularMask,CalculateArea,CalculateVolume,SubsetByOutline,
      ShapePolygon,ShapeFirstPolygon,SubsetHole,SubsetLake,SubsetByOutline,ReplaceValuesByOutline,FillHolesByOutline];
end;

function PolyLineDigitizing(DEMNowDoing :  tDEMDoingWhat) : boolean;
begin
   PolyLineDigitizing := DEMNowDoing in [StreamDistance,NewTrack,RouteObservation,ShapeLine,ShapeFirstLine,SeekingFlyThroughRoute,SeekingStreamProfile,Scribble];
end;

function NowStreamDigitizing(DEMNowDoing :  tDEMDoingWhat) : boolean;
begin
   Result := PolygonDigitizing(DEMNowDoing) or PolyLineDigitizing(DEMNowDoing);
end;

function DraggingOperation(DEMNowDoing :  tDEMDoingWhat) : boolean;
begin
   Result := (DEMNowDoing in [{$IfDef ExMilicons}{$Else}AddMilIcon,{$EndIf}  DragEdit]) ;
end;


function BoxOutlining(DEMNowDoing :  tDEMDoingWhat) : boolean;
begin
   Result := (DEMNowDoing in [NewCoverage,OpenMapsFromLibrary,CornerEditBox,MoveMapBox,GraphFilterDB,DeleteMultipleDBRecs,DeleteMultipleRecsAllDBs,
              {$IfDef ExRedistrict}{$Else} RecolorRedistrictBox,{$EndIf}
              {$IfDef ExSat}{$Else} RegionDNs,OpenMrSid,PickTrainingBox,{$EndIf}
              NLCDBox]);
end;


function DEMRequiredForOperation(var DEM : integer) : boolean;
begin
   Result := ValidElevationRequired;
   if Result then begin
      if (DEM = 0) then GetDEM(DEM);
      if (DEM = 0) then MessageToContinue('Cannot proceed; ' +  NoDEMCovers);
   end;
end;


function TMapForm.FindDBsOnMap : integer;
var
   i : integer;
begin
   result := 0;
   for i := 1 to MaxDataBase do begin
      if (GISdb[i] <> Nil) and (GISdb[i].TheMapOwner <> Nil) and (GISdb[i].TheMapOwner.Handle = Self.Handle) then begin
         inc(Result);
      end;
   end;
end;


procedure TMapForm.DrawLineFromPointDBOnMap(Table : tMyData; Color : tPlatformColor; Width : byte);
var
   PolyLinePoints  : ^tPolyLinePts;
   PointsInPolyLine,xp,yp : integer;
   Lat,Long : float64;
begin
   with Image1.Canvas do begin
      PointsInPolyLine := 0;
      New(PolyLinePoints);
      Table.First;
      while not Table.eof do begin
         if Table.ValidLatLongFromTable(Lat,Long) then begin
            //Lat := Table.GetFieldByNameAsFloat('LAT');
            //Long := Table.GetFieldByNameAsFloat('LONG');
            MapDraw.LatLongDegreeToScreen(Lat,Long,xp,yp);
            PolyLinePoints^[PointsInPolyLine].x := xp;
            PolyLinePoints^[PointsInPolyLine].y := yp;
            inc(PointsInPolyLine);
         end;
         Table.Next;
      end;

      if (PointsInPolyLine > 0) then begin
         ShowHourglassCursor;
         Image1.Canvas.Pen.Color := ConvertPlatformColorToTColor(Color);
         Image1.Canvas.Pen.Width := Width;
         Image1.Canvas.PolyLine(slice(PolyLinePoints^,PointsInPolyLine));
      end;
      Dispose(PolyLinePoints);
   end;
end;


function TMapForm.DrawStreamProfileOnMap(db : integer; WhatWanted : tDEMDoingWhat; Color : tPlatformColor; Width : byte) : tMyBitmap;

         procedure SetCanvasParameters(Canvas : tCanvas);
         begin
            Canvas.Brush.Style := bsSolid;
            Canvas.Brush.Color := ConvertPlatformColorToTColor(Color);
            Canvas.Pen.Color := ConvertPlatformColorToTColor(Color);
            Canvas.Pen.Width := Width;
         end;

begin
   ShowHourglassCursor;
   CloneImageToBitmap(Image1,result);
   SetCanvasParameters(Result.Canvas);
   GISdb[db].PlotDefaultSymbols(MapDraw,Result);

   if WhatWanted in [OutlineDBIrregularMask] then begin
      Result.SaveToFile(MDTempDir + 'target-mask.bmp');
      if (MapDraw.MultiFanMasks = Nil) then MapDraw.MultiFanMasks := tStringList.Create;
      MapDraw.MultiFanMasks.Clear;
      MapDraw.MultiFanMasks.Add(MDTempDir + 'target-mask.bmp');
   end;

   {$IfDef RecordDigitize} Result.SaveToFile(MDTempDir + 'digitize.bmp'); {$EndIf}
end;


procedure TMapForm.DoneWithStreamSelection;
var
   x,y,db : integer;
   z : float32;
   SumPositive,SumNegative,
   Lat,Long,LakeLevel : float64;
   WhatWanted : tDEMDoingWhat;
   Bitmap : tMyBitmap;
   TStr : shortString;
   Dist3DOK : boolean;
   fName : PathStr;


        procedure ProcessHoleLakeOutlineVolume;
        var
            Col1,Col2,Row1,Row2,Col,Row,ReferenceDEM : integer;
            fName : PathStr;
            SameGrid : boolean;
         begin
             ReferenceDEM := 0;
             ShowHourglassCursor;
             if (MapDraw.ValidDEMonMap) then with MapDraw,MapCorners do begin
                DEMGlb[DEMonMap].LatLongDegreeToDEMGridInteger(BoundBoxGeo.ymax,BoundBoxGeo.xmax,Col2,Row2);
                DEMGlb[DEMonMap].LatLongDegreeToDEMGridInteger(BoundBoxGeo.ymin,BoundBoxGeo.xmin,Col1,Row1);
                DEMGlb[MapDraw.DEMonMap].ClipDEMGrid(Col1,Row1);
                DEMGlb[MapDraw.DEMonMap].ClipDEMGrid(Col2,Row2);
             end;

             if (Col1 = Col2) and (Row1 = Row2) then exit;
             if (WhatWanted = SubsetLake) then begin
                LakeLevel := 0;
                ReadDefault('Lake level',LakeLevel);
             end;
             if WhatWanted in [ReplaceValuesByOutline,FillHolesByOutline] then begin
                fName := '';
                if not LoadNewDEM(ReferenceDEM,fName,false,'Reference for filling holes') then exit;
                SameGrid := DEMGlb[MapDraw.DEMonMap].SecondGridIdentical(ReferenceDEM);
             end;
             SumPositive := 0;
             SumNegative := 0;

             StartProgress('Perimeter');
             if MapDraw.DEMMap and (WhatWanted in [SubsetByOutline,SubsetHole,SubsetLake,ReplaceValuesByOutline,FillHolesByOutline,CalculateVolume]) then begin
                DEMGlb[MapDraw.DEMonMap].SetGridMissingOutsideBox(Col1,Row1,Col2,Row2);
             end;

             for Col := Col1 to Col2 do begin
                if ShowSatProgress and (Col mod 10 = 0) Then UpdateProgressBar((Col-Col1)/(Col2-Col1));
                for Row := Row1 to Row2 do begin
                   MapDraw.DEMGridToScreen(Col,Row,x,y);
                   if MapDraw.OnScreen(x,y) then begin
                      if WhatWanted in [CalculateVolume] then begin
                         if (Bitmap.Canvas.Pixels[x,y] = clBlack) then begin
                           if DEMGlb[MapDraw.DEMonMap].GetElevMetersOnGrid(Col,Row,z) then begin
                              z := z - VolumeRelativeToZ;
                              if (z > 0) then SumPositive := SumPositive + z
                              else SumNegative := SumNegative + z;
                            end;
                         end;
                      end
                      else if (WhatWanted in [SubsetByOutline]) then begin
                         if (Bitmap.Canvas.Pixels[x,y] <> clBlack) then DEMGlb[MapDraw.DEMonMap].SetGridMissing(Col,Row);
                      end
                      else if (WhatWanted in [ReplaceValuesByOutline,FillHolesByOutline]) then begin
                         if (Bitmap.Canvas.Pixels[x,y] = clBlack) then begin
                            if SameGrid then DEMGlb[ReferenceDEM].GetElevMetersOnGrid(Col,Row,z)
                            else begin
                               DEMGlb[MapDraw.DEMonMap].DEMGridToLatLongDegree(Col,Row,Lat,Long);
                               DEMGlb[ReferenceDEM].GetElevFromLatLongDegree(Lat,Long,z);
                            end;
                            if (z < 32000) then begin
                              if (WhatWanted in [ReplaceValuesByOutline]) or DEMGlb[MapDraw.DEMonMap].MissingDataInGrid(Col,Row) then DEMGlb[MapDraw.DEMonMap].SetGridElevation(Col,Row,z)
                           end;
                         end;
                      end
                      else begin
                         if (Bitmap.Canvas.Pixels[x,y] = clBlack) then begin
                            if (WhatWanted = SubsetLake) then DEMGlb[MapDraw.DEMonMap].SetGridElevation(Col,Row,LakeLevel)
                            else DEMGlb[MapDraw.DEMonMap].SetGridMissing(Col,Row);
                         end;
                      end;
                   end;
                end;
             end;
             EndProgress;
             CloseSingleDEM(ReferenceDEM);
             if (WhatWanted in [SubsetByOutline,SubsetHole,SubsetLake,ReplaceValuesByOutline,FillHolesByOutline]) then begin
                DEMGlb[MapDraw.DEMonMap].CheckMaxMinElev;
                if not MapDraw.UsePercentiles then MapDraw.ScaleMapElevationsToDEM;
                DoBaseMapRedraw;
                if (not MapDraw.DEMMap) then DEMGlb[MapDraw.DEMonMap].SelectionMap.DoBaseMapRedraw;
             end {if};

            {$IfDef ExMapReports}
            {$Else}
                if WhatWanted in [CalculateVolume] then with DEMGlb[MapDraw.DEMonMap] do begin
                   SumPositive := SumPositive * AverageXSpace * AverageYSpace;
                   SumNegative := SumNegative * AverageXSpace * AverageYSpace;
                   MessageToContinue('Volume:' + MessLineBreak + '   Fill: ' + SmartVolumeFormat(SumPositive) + MessLineBreak +
                      '   Cut:  ' + SmartVolumeFormat(SumNegative) + MessLineBreak +
                      '   Net:   ' + SmartVolumeFormat(SumPositive+SumNegative) +  MessLineBreak +
                      'Volume base level, z=' + RealToString(VolumeRelativeToZ,-12,-2),True);
                end {if WhatWanted};
            {$EndIf}
         end;


         procedure ProcessStreamDistance(ShowResults : boolean = true);
         begin
             if (WhatWanted in [StreamDistance]) then begin
               {$IfDef MeasureDistance} WriteLineToDebugFile('Start ProcessStreamDistance'); {$EndIf}
                SaveBackupDefaults;
                MDdef.AddSpeed := false;
                MDdef.AddAzimuth := true;
                MDdef.AddDist := true;
                MDdef.AddCumDist := true;
                MDdef.Add3DDist := true;
                MDDef.UseMeters := true;

                GISDB[db].AddNavFields;
                Dist3DOK := MapDraw.ValidDEMonMap;
                if ShowResults then begin
                   GISDB[db].MyData.Last;
                   if Dist3DOK then TStr := MessLineBreak + '3D Dist:  ' + SmartDistanceMetersFormat(GISDB[db].MyData.GetFieldByNameAsFloat('CUM_3D_M'))
                   else TStr := '';
                   TStr := 'Distance: ' + SmartDistanceMetersFormat(GISDB[db].MyData.GetFieldByNameAsFloat('CUM_M')) + MessLineBreak + TStr;
                   MessageToContinue(TStr);
                end;
                RestoreBackupDefaults;
             end;
         end;

         procedure FinishShapeFileRecord;
         var
            i,db : integer;
            Values : tStringList;
         begin
            {$If Defined(RecordShapeFileEdits)} WriteLineToDebugFile('tmapForm.FinishShapeFileRecord in'); {$EndIf}
              MouseIsDown := false;
              dbAddRecForm.ShapeFileCreator.ProcessShapeFileRecord;
              if (dbAddRecForm.CheckBox1.Checked) and (dbAddRecForm.ShapeFileCreator.RecsInShapeStream > 0) then begin
                 Values := tStringList.Create;
                 for i := 0 to pred(DbaddRecForm.Table1.FieldCount) do begin
                    Values.Add(DbaddRecForm.Table1.GetFieldByNameAsString(DbaddRecForm.Table1.GetFieldName(i) ));
                 end;
              end;

              DbaddRecForm.Table1.Last;
              DbaddRecForm.Table1.Insert;

              if DbaddRecForm.Table1.FieldExists('LAT_HI') then begin
                 PutBoundBoxInTable(DbaddRecForm.Table1,dbAddRecForm.ShapeFileCreator.glMainFileHeader.BoundBox);
              end;

              if (dbAddRecForm.CheckBox1.Checked) and (dbAddRecForm.ShapeFileCreator.RecsInShapeStream > 0) then begin
                 for i := 0 to pred(DbaddRecForm.Table1.FieldCount) do
                    DbaddRecForm.Table1.SetFieldByNameAsString(DbaddRecForm.Table1.GetFieldName(i), Values.Strings[i]);
                 Values.Free;
              end;

              DbaddRecForm.Table1.SetFieldByNameAsInteger(RecNoFName,DbaddRecForm.Table1.FiltRecsInDB);

              if (DEMNowDoing in [CalculateArea,OutlineDBIrregularMask]) then begin
                 DbaddRecForm.Table1.SetFieldByNameAsInteger(RecNoFName,DbaddRecForm.Table1.FiltRecsInDB);
                 DbaddRecForm.Table1.Post;
                 db := dbAddRecForm.ShapeFileCreator.CloseShapeFiles;

                 if (DEMNowDoing in [CalculateArea]) then begin
                    GISdb[db].DBTablef.CalculateArea1Click(Nil);
                    DoFastMapRedraw;
                    BackToWandering;
                 end;
              end
              else begin
                 DbaddRecForm.Button1.Enabled := false;
                 DbaddRecForm.Button2.Enabled := false;
                 DbaddRecForm.Table1.Post;
                 DbaddRecForm.ShowModal;
              end;
            {$IfDef RecordShapeFileEdits} WriteLineToDebugFile('tmapForm.FinishShapeFileRecord out'); {$EndIf}
         end;


begin
   {$IfDef RecordDigitize} WriteLineToDebugFile('TMapForm.DoneWithStreamSelection in'); WriteStringListToDebugFile(StreamProfileResults); {$EndIf}
   MouseIsDown := false;
   db := 0;
   WhatWanted := DEMNowDoing;
   if ShapeFileDigitizingUnderway(DEMNowDoing) then begin
      if (dbAddRecForm.ShapeFileCreator.PtsInShapeStream < 2) then begin
         MessageToContinue('Need at least 2 points');
         exit;
      end;
      FinishShapeFileRecord;
      {$If Defined(RecordDigitize) or Defined(RecordShapeFileEdits)} WriteLineToDebugFile('back from FinishShapeFileRecord'); {$EndIf}
      if WhatWanted in [OutlineDBIrregularMask] then begin
         GISdb[DBEditting].IrregularFilterDB;
      end;
      if (StreamProfileResults <> Nil) then FreeAndNil(StreamProfileResults);
   end
   else begin
      if (StreamProfileResults = Nil) then exit
      else if (DEMNowDoing in [SeekingStreamProfile]) then begin
         ExpandRoute(MapDraw.DEMonMap,StreamProfileResults,DEMGlb[MapDraw.DEMonMap].AverageSpace * 0.5,true,true,false);
         {$IfDef RecordDigitize} WriteLineToDebugFile('Expanded'); WriteStringListToDebugFile(StreamProfileResults); {$EndIf}
         db := StringListToLoadedDatabase(StreamProfileResults, Petmar.NextFileNumber(MDTempDir, 'Stream_digitize_', DefaultDBExt),true,false,true);
         StreamProfileResults := Nil;
         ProcessStreamDistance(false);
         GISdb[db].dbOpts.XField := 'CUM_M';
         GISdb[db].dbOpts.YField := 'Z';
         GISdb[db].dbOpts.LineWidth := 2;
         GISdb[db].MakeGraph(dbgtN2Dgraphsimplelines1,false);
      end
      else begin
         if (WhatWanted in [NewTrack]) then fName := Petmar.NextFileNumber(dbDir, 'Track_', DefaultDBExt)
         else if (WhatWanted in [SeekingFlyThroughRoute]) then fName := Petmar.NextFileNumber(MDTempDir, 'Fly_trough_', DefaultDBExt)
         else if (WhatWanted in [RouteObservation]) then fName := Petmar.NextFileNumber(MDTempDir, 'Route_observation_', DefaultDBExt)
         else fName := Petmar.NextFileNumber(MDTempDir, 'Stream_digitize_', DefaultDBExt);
         db := StringListToLoadedDatabase(StreamProfileResults, fName,true,false,true);
         GISdb[db].dbOpts.ConnectUnderPoints := true;
         GISdb[db].RedrawLayerOnMap;
         StreamProfileResults := Nil;
         ShowHourglassCursor;
         if (WhatWanted in [StreamDistance,NewTrack]) then ProcessStreamDistance
         else if WhatWanted in [SubsetByOutline,SubsetHole,SubsetLake,CalculateVolume,ReplaceValuesByOutline,FillHolesByOutline] then ProcessHoleLakeOutlineVolume
         else if NowStreamDigitizing(DEMNowDoing) then begin

            {$IfDef ExFly}
            {$Else}
               if (WhatWanted in [SeekingFlyThroughRoute]) then begin
                  StartTheFlyThrough(WhatWanted);
               end;
            {$EndIf}

            {$IfDef ExViewshed}
            {$Else}
               if (WhatWanted in [RouteObservation]) then begin
                  SetupAmbush;
               end;
            {$EndIf}
         end {if};
      end;
      if (WhatWanted <> Scribble) then BackToWandering;
   end;
   ShowDefaultCursor;
   MapScreenX1 := -9999;
   MapScreenY1 := -9999;
   {$IfDef RecordDigitize} WriteLineToDebugFile('TMapForm.DoneWithStreamSelection out'); {$EndIf}
end;


procedure TMapForm.ClimographandETO1Click(Sender: TObject);
begin
   OpenTempPrecipEvap(false);
   KoppenClimographFromClimateGrids1Click(Sender);
   Evapotranspirationprecipitationwaterbudget1Click(Sender);
end;

procedure TMapForm.ClipLatLongToMap(var Lat,Long : float64);
begin
   with MapDraw.MapCorners do begin
      if (Lat > BoundBoxGeo.ymax) then Lat := BoundBoxGeo.ymax;
      if (Lat < BoundBoxGeo.ymin) then Lat := BoundBoxGeo.ymin;
      if (Long > BoundBoxGeo.xmax) then Long := BoundBoxGeo.xmax;
      if (Long < BoundBoxGeo.xmin) then Long := BoundBoxGeo.xmin;
   end;
end;


function TMapForm.ExpertDEMVersion : boolean;
begin
   Result := (MDDef.ProgramOption in [ExpertProgram,RemoteSensingProgram]) and MapDraw.ValidDEMonMap;
end;


procedure TMapForm.Allgraphs1Click(Sender: TObject);
begin
   {$IfDef RecordGeography} WriteLineToDebugFile('TMapForm.Allgraphs1Click'); {$EndIf}
   Monthlytemperatureranges1Click(Sender);
   ClimographandETO1Click(Sender);
   Monthlyinsolation1Click(Sender);
   Solarparameters1Click(Sender);
end;

procedure TMapForm.Annualsolarelevation1Click(Sender: TObject);
begin
   {$IfDef ExGeography}
   {$Else}
      SunAtLocalNoon(Self,RightClickLat,RightClickLong);
   {$EndIf}
end;

procedure TMapForm.Annualsolarillumination1Click(Sender: TObject);
begin
   HoursSolarIlluminationGraph(MapDraw.DEMonMap,RightClickLat,RightClickLong);
end;

procedure TMapForm.ArrangeButtons;
var
   ButtonsVisible,glLeftPos : integer;

   procedure CheckButton(var SB1 : tSpeedButton; Which : shortString = '');  //Must Check in same order buttons appear on toolbar
   begin
      SB1.Left := glLeftPos;
      if SB1.Visible then begin
         inc(ButtonsVisible);
         SB1.Width := 25;
         SB1.Top := 1;
         inc(glLeftPos,SB1.Width);
         {$IfDef RecordButton} if (Which <> '') then WriteLineToDebugFile(Which + '.Left=' + IntToStr(SB1.Left)); {$EndIf}
      end;
      {$IfDef RecordButton} WriteLineToDebugFile(' Button: ' + IntToStr(ButtonsVisible) + '   Left: ' + IntToStr(SB1.Left) + '  right : ' + IntToStr(glLeftPos)); {$EndIf}
   end;

begin
      {$IfDef RecordButton} WriteLineToDebugFile('ClientWidth=' + IntToStr(ClientWidth) + '  panel1.Width=' + IntToStr(panel1.Width)); {$EndIf}
      glLeftPos := 0;

      ComboBox1.Visible := (MapDraw <> Nil) and MapDraw.DEMMap or ValidSatImage(MapDraw.SatOnMap);
      ZoomBitBtn.Visible := ComboBox1.Visible;
      if ComboBox1.Visible then begin
         ComboBox1.Left := 0;
         ComboBox1.Top := 1;
         glLeftPos := ComboBox1.Width;
         ZoomBitbtn.Left := glLeftPos;
         glLeftPos := glLeftPos + ZoomBitBtn.Width;
      end;

      ButtonsVisible := 0;
      CheckButton(UndoSpeedButton);
      CheckButton(PrintSpeedButton,'print');
      CheckButton(SaveSpeedButton,'save');
      CheckButton(ClipboardSpeedButton,'clip');
      CheckButton(AnnotateSpeedButton1,'annotate');
      CheckButton(SpeedButton7);
      CheckButton(DataBaseSpeedButton28);
      CheckButton(FullMapSpeedButton,'full');
      CheckButton(SubsetSpeedButton,'subset');
      CheckButton(UnsubsetSpeedButton22,'unsubset');
      if (MDdef.ProgramOption = GeologyProgram) then CheckButton(GeologySpeedButton1,'geology');
      if (MDdef.ProgramOption = GeographyProgram) then CheckButton(KoppenSpeedButton);
      if (MDdef.ProgramOption = GeographyProgram) then CheckButton(KoppenSpeedButton7);
      CheckButton(ZoomInSpeedButton4,'zoom in');
      CheckButton(SpeedButton22);
      CheckButton(ZoomOutSpeedButton5,'zoom out');
      CheckButton(RedrawSpeedButton12);
      CheckButton(MrSidspeedbutton,'mrsid');
      CheckButton(MeasureDistanceSpeedButton14);
      CheckButton(ZigDistButton);
      CheckButton(SpeedButton1);
      CheckButton(SpeedButton4);
      CheckButton(SpeedButton2);
      CheckButton(SideScanButton);
      CheckButton(SubbottomSpeedButton);
      CheckButton(GridSpeedButton15);
      CheckButton(PickBandSpeedButton20);
      CheckButton(GE_SpeedButton);
      CheckButton(OGL_SpeedButton);
      CheckButton(InfoSpeedButton6);
      CheckButton(PointCloudSpeedButton,'point cloud');
      CheckButton(IDSpeedButton);
      if (MDdef.ProgramOption <> GeologyProgram) then CheckButton(GeologySpeedButton1,'geology');
      CheckButton(FocalMechsButton,'beach ball');
      if (MDdef.ProgramOption <> GeographyProgram) then CheckButton(KoppenSpeedButton);
      if (MDdef.ProgramOption <> GeographyProgram) then CheckButton(KoppenSpeedButton7);
      CheckButton(StratcolButton);
      CheckButton(WeaponFanSpeedButton8);
      CheckButton(RangeCirclesSpeedButton9);
      CheckButton(SpeedButton5,'vol');
      CheckButton(VectorOverlaySpeedButton21);
      CheckButton(DupeMapSpeedButton18);
      CheckButton(AnaglyphSpeedButton);
      CheckButton(EditGridButton);
      //CheckButton(DEMSpeedButton25);
      CheckButton(MapLibSpeedButton);
      CheckButton(LibSpeedButton);
      CheckButton(SpeedButton3);
      CheckButton(SpeedButton8);
      PixelSizeEdit.Left := glLeftPos;
      {$IfDef RecordButton} WriteLineToDebugFile('Buttons visible=' + IntToStr(ButtonsVisible)); {$EndIf}
end;


procedure TMapForm.CheckProperTix;

      procedure ProcessLegendOptions;
      begin
         {$IfDef ExTiger}
            TigerVectorLegend1.Visible := false;
         {$Else}
            TigerVectorLegend1.Visible := OverlayUp(ovoTiger);
            if OverlayUp(ovoTiger) then VectorOutlines1.Visible := true;
         {$EndIf}

         GridVATLegend1.Visible := MapDraw.MapType = mtDEMVATTable;
         NumericgridwithVATtobytegridwithcodes1.Visible := (MapDraw.ZColorTable.ZTableEntries > 0);
         Fixedpalettecategories1.Visible := MapDraw.MapType = mtElevFromTable;
         DataBaseLegend1.Visible := (NumOpenDatabaseThisMap(Self) > 0);
         GazetteerLegend1.Visible := OverlayUp(ovoGazetteer);
         Shapefilegrouplegend1.Visible := (MapDraw.MapOverlays.ovShapeFileGroup <> '');
         Cartogrouplegend1.Visible := (MapDraw.CartoGroupShapesUp <> '');
         LegendOptionsAvailable := 0;

         if Fixedpalettecategories1.Visible then inc(LegendOptionsAvailable);
         if TigerVectorLegend1.Visible then inc(LegendOptionsAvailable);
         if KoppenLegend2.Visible then inc(LegendOptionsAvailable);
         if NLCDLegend1.Visible then inc(LegendOptionsAvailable);
         if LASclassificationlegend1.Visible then inc(LegendOptionsAvailable);
         if DatabaseLegend1.Visible then inc(LegendOptionsAvailable);
         if Shapefilegrouplegend1.Visible then inc(LegendOptionsAvailable);
         if GazetteerLegend1.Visible then inc(LegendOptionsAvailable);
         if GridVATLegend1.Visible then inc(LegendOptionsAvailable);
         if Cartogrouplegend1.Visible then inc(LegendOptionsAvailable);
         Legend1.Visible := LegendOptionsAvailable > 0;
         Popuplegends1.Visible := LegendOptionsAvailable > 0;
      end;


var
   i : integer;
begin
   {$If Defined(RecordCheckProperTix)} WriteLineToDebugFile(Caption + ' CheckProperTix in'); {$EndIf}

   if SkipMenuUpdating or Help1.Visible or (MapDraw = Nil) then exit;

   if MDDef.ShowMapToolbar then Panel1.Height := 27
   else Panel1.Height := 0;

      {$IfDef IncludeFMX3DMesh}
         TestMD1.Visible := TrilobiteComputer;
      {$Else}
         TestMD1.Visible := false;
      {$EndIf}

   //these are turned on here, but might be turned off later for small maps where they would not fit on visible toolbar
      ClipboardSpeedButton.Visible := true;
      GridSpeedButton15.Visible := true;
      SaveSpeedButton.Visible := true;

   //end of turned on here, but might be turned off later for small maps

      N11view1.Visible := (MDDef.ProgramOption = ExpertProgram) and (MapDraw.VectorIndex = 0);
      Speedbutton22.Enabled := (MapDraw.MapZoomFactor < 0.98) or (MapDraw.MapZoomFactor > 1.02);
      N11view1.Enabled := Speedbutton22.Enabled;
      Averagebylatitude1.Visible := (MDDef.ProgramOption = ExpertProgram);
      UnsubsetSpeedButton22.Enabled := FullMapSpeedButton.Enabled and (MapDraw.DEMMap or (MapDraw.VectorIndex <> 0));
      CurrentsubsetMDDEM1.Visible := FullMapSpeedButton.Enabled;
      CurrentsubsetGeotiff1.Visible := FullMapSpeedButton.Enabled;
      FixedPaletteStats1.Visible := (MapDraw.MapType = mtElevFromTable);
      GeologicAges1.Visible := (MapDraw.ValidDEMonMap) and (DEMGlb[MapDraw.DEMonMap].DEMheader.ElevUnits = HundredthMa);
      Saveallmapsasimage1.Visible := (MDDef.ProgramOption in [ExpertProgram]) and (NumOpenMaps > 1);

      Dataheader2.Visible := ((MapDraw.DEMonMap > 0) or (MapDraw.SatOnMap > 0)) and (MDDef.ProgramOption in [ExpertProgram,RemoteSensingProgram]);

      ElevationsExtremes1.Checked := (ExtremeZDEM <> 0);
      Missingdatacolor1.Visible := (MapDraw.ValidDEMonMap) or ValidSatImage(MapDraw.SatonMap);
      Lntransform1.Visible := ExpertDEMVersion and (DEMGlb[MapDraw.DEMonMap].DEMheader.MaxElev > 0);
      Vectoraverage1.Visible := ExpertDEMVersion and (DEMGlb[MapDraw.DEMonMap].DEMheader.ElevUnits = zDegrees);

      Aspectrosediagram1.Visible := ExpertDEMVersion and ( (DEMGlb[MapDraw.DEMonMap].DEMheader.ElevUnits in [zDegrees,AspectDeg]) or ((DEMGlb[MapDraw.DEMonMap].DEMheader.MaxElev <= 360) and (DEMGlb[MapDraw.DEMonMap].DEMheader.MaxElev >= 0) ));

      RGBgridfillholes1.Visible := ExpertDEMVersion and (DEMGlb[MapDraw.DEMonMap].DEMheader.ElevUnits = euRGB);
      Logbase10transform1.Visible := Lntransform1.Visible;
      Weather2.Visible := (MDDef.ProgramOption in [ExpertProgram,GeographyProgram]);

      Integercode1.Visible := ExpertDEMVersion and (DEMGlb[MapDraw.DEMonMap].DEMheader.DEMPrecision in [byteDEM,SmallIntDEM,WordDEM]);

     N3Drotatingglobe1.Visible := (MapDraw.PrimMapProj.PName in [PlateCaree]) and MapDraw.PrimMapProj.FullWorld and (not FullMapSpeedButton.Enabled);

     GetGRASSextensions1.Visible := ValidDEM(MapDraw.DEMonMap) and (MDDef.ProgramOption in [ExpertProgram]);

    {$If Defined(RecordCheckProperTix)} WriteLineToDebugFile('CheckProperTix start overlay menu'); {$EndIf}

    Removequickoverlayhillshade1.Visible := ValidDEM(SavedMergeReflectanceDEM);
    //Overlay menu
      ValidPointsInSecondGrid1.Visible := (MDDef.ProgramOption in [ExpertProgram,RemoteSensingProgram]);
      Contoursfromsecondgrid2.Visible := (MDDef.ProgramOption in [ExpertProgram,RemoteSensingProgram]);
      RangeCircles2.Visible := (MDDef.ProgramOption in [ExpertProgram,RemoteSensingProgram]);
      Sectoroutlines1.Visible := (MDDef.ProgramOption in [ExpertProgram,RemoteSensingProgram]);
      Surveytracklines1.Visible := (MDDef.ProgramOption in [ExpertProgram,RemoteSensingProgram]);
      Worldfileimages1.Visible := (MDDef.ProgramOption in [ExpertProgram,RemoteSensingProgram,GeologyProgram]);
      Overlaysvariableopaque1.Visible := (MDDef.ProgramOption in [ExpertProgram,RemoteSensingProgram]);
      Clearsecondgrid1.Visible := ValidDEM(MapDraw.DEM2onMap);

      Pureplatecareeprojectiondistorted1.Visible := ValidDEM(MapDraw.DEMonMap) and (MDDef.ProgramOption in [ExpertProgram]);
      Pureplatecareeprojectiondistorted1.Checked := MapDraw.UseDistortedRawLatLongScaling;

    //Edit Menu and options
      CreateDEMtomatchmap1.Visible := (MDDef.ProgramOption in [ExpertProgram,RemoteSensingProgram]);
      Edit2.Visible := (MDDef.ProgramOption in [ExpertProgram,RemoteSensingProgram]);
      Geocode1.Visible := (MDDef.ProgramOption in [ExpertProgram]);
      Flattenlake1.Visible := ExpertDEMVersion;
      MarkAsMissing2.Visible := (MDDef.ProgramOption in [ExpertProgram,RemoteSensingProgram]) and MapDraw.DEMMap;
      DEMHoles1.Visible := (MDDef.ProgramOption in [ExpertProgram,RemoteSensingProgram]) and MapDraw.DEMMap;
      EditVATDBFcolorscategorynames1.Visible := ValidDEM(MapDraw.DEMonMap) and (DEMGlb[MapDraw.DEMonMap].VATFileName <> '');

    //Cartography menu
      Gridspacinganddeclination1.Visible := ExpertDEMversion;

      Singlegridarithmetic1.Visible := (MDDef.ProgramOption in [ExpertProgram,RemoteSensingProgram]) and MapDraw.DEMMap;
      EditPointElevations1.Visible := ExpertDEMVersion;
      ReloadDEM.Visible := (MDDef.ProgramOption in [ExpertProgram,RemoteSensingProgram]) and ValidDEM(MapDraw.DEMonMap) and (DemGLB[MapDraw.DEMonMap].DEMFileName <> '');
      Flat.Visible := ExpertDEMVersion;
      Walls1.Visible := ExpertDEMVersion;
      BuildingEdges1.Visible := ExpertDEMVersion;
      ShiftDEM1.Visible := (MDDef.ProgramOption in [ExpertProgram,GeologyProgram]) and MapDraw.ValidDEMonMap and (DEMGlb[MapDraw.DEMonMap].LongSizeMap > 350);
      oInternationalDateLine1.Enabled := ExpertDEMVersion and (DEMGlb[MapDraw.DEMonMap].DEMheader.DEMSWCornerX < 179.95) and (DEMGlb[MapDraw.DEMonMap].DEMheader.DEMSWCornerX > -179.95);
      oPrimeMeridian1.Enabled := ExpertDEMVersion and (abs(DEMGlb[MapDraw.DEMonMap].DEMheader.DEMSWCornerX) > 0.05);
      Loadfeaturegrid1.Visible := (MDDef.ProgramOption = ExpertProgram);
      LoadCHMgrid1.Visible := (MDDef.ProgramOption = ExpertProgram);
      LoadLOStopoprofile1.Visible := (MDDef.ProgramOption = ExpertProgram);
      LoadChangeGrid1.Visible := (MDDef.ProgramOption in [ExpertProgram,RemoteSensingProgram]);
      LoadLandCover1.Visible := (MDDef.ProgramOption in [ExpertProgram,RemoteSensingProgram,GeographyProgram]);

      LASfile1.Visible := (MDDef.ProgramOption in [ExpertProgram,RemoteSensingProgram]);

      Coccurrenceoftwogrids1.Visible := (NumDEMDataSetsOpen > 1);

      Topographicgrain1.Visible := MDDef.ShowGeomorphometry and (MapDraw <> Nil) and (MapDraw.VectorIndex = 0) and ValidDEM(MapDraw.DEMonMap);
      ConvertUKOSDEM1.Visible := ExpertDEMVersion and (MapDraw.ValidDEMonMap) and (DEMGlb[MapDraw.DEMonMap].DEMheader.DigitizeDatum = UK_OS_grid);
      DEMgridhistogram1.Visible := MapDraw.ValidDEMonMap;

      {$IfDef ExDrainage}
         Quicksealevelrise1.Visible := false;
         Floodbasin1.Visible := false;
      {$Endif}

      Clearoverlays1.Visible := MapDraw.MapOverlays.ovVectorFiles.Count > 0;
      LCCstandardParallels1.Visible := (MapDraw.VectorIndex <> 0) and (MapDraw.PrimMapProj.PName in [AlbersEqAreaConicalEllipsoid,LambertConformalConicEllipse]);

      Gridoutlines1.Visible := ExpertDEMVersion;
      Fatfingers1.Visible := (MDDef.ProgramOption in [ExpertProgram,RemoteSensingProgram]);

      SubsetSpeedButton.Visible := MapSubsetAllowed;

     {$If Defined(RecordCheckProperTix)} WriteLineToDebugFile('CheckProperTix start VectorIndex <> 0'); {$EndIf}

      if (MapDraw.VectorIndex <> 0) then begin
         SpeedButton22.Visible := false;
         Display1.Visible := false;
         SubsetSpeedButton.Visible := MapDraw.PrimMapProj.SeriousWorkingProjection and MapSubsetAllowed;
         MeasureDistanceSpeedButton14.Visible := SubsetSpeedButton.Visible;
         Distance1.Visible := SubsetSpeedButton.Visible;
         Bearing1.Visible := SubsetSpeedButton.Visible;
         ZoomInSpeedButton4.Visible := SubsetSpeedButton.Visible;
         ZoomOutSpeedButton5.Visible := SubsetSpeedButton.Visible;
      end
      else if MapDraw.DEMMap then begin
         Display1.Visible := true;
      end;

      UnsubsetSpeedButton22.Visible := SubsetSpeedButton.Visible;
      FullMapSpeedButton.Visible := SubsetSpeedButton.Visible;

      {$IfDef ExSat}
         NLCD1.Visible := false;
         NLCDLegend1.Visible := false;
         ContrastSpeedButton19.Visible := false;
         MrSidSpeedButton.Visible := false;
         GeotifffromMrSid1.Visible := false;
         SubsetSpeedButton.Enabled := MapDraw.DEMMap or (MapDraw.VectorIndex <> 0);
         AllbandsasGeotiffs1.Visible := false;
         Satellitehistograms1.Visible := false;
         Imagepalette1.Visible := false;
         Reregisterimage1.Visible := false;
         Mergeallimages1.Visible := false;
         MultiGrids1.Visible := false;
         Savesatelliteimage1.Visible := false;
         Band1.Visible := false;
         Landsatmetadata1.Visible := false;
         Sentinel2Metadata.Visible := false;
      {$Else}
         MergeColorScene1.Visible := MapDraw.ValidSatOnMap;
         SubsetSpeedButton.Enabled := MapDraw.DEMMap or (MapDraw.VectorIndex <> 0 ) or (MapDraw.ValidSatOnMap);
         UnsubsetSpeedButton22.Visible := MapDraw.ValidSatOnMap and MapSubsetAllowed;
         Histogramallbands1.Visible := MapDraw.ValidSatOnMap and (SatImage[MapDraw.SatOnMap].NumBands > 1);
         PickBandSpeedButton20.Visible := MapDraw.ValidSatOnMap and ((SatImage[MapDraw.SatOnMap].NumBands > 1) or SatImage[MapDraw.SatOnMap].CanEnhance or (MapDraw.MapType = mtSatBlank));
         Band1.Visible := PickBandSpeedButton20.Visible;
         Display1.Visible := MapDraw.ValidSatOnMap and SatImage[MapDraw.SatOnMap].CanEnhance;
         AddRGBWindows1.Visible := MapDraw.ValidSatOnMap and (SatImage[MapDraw.SatOnMap].NumBands > 5) and FileExists(TM_RGB_fname);

         Imageanalysis1.Visible := (MDDef.ProgramOption in [ExpertProgram,RemoteSensingProgram]) and (MapDraw.ValidSatOnMap or (MapDraw.MultiGridOnMap <> 0));
         MultiGrids1.Enabled := (MapDraw.MultiGridOnMap <> 0);
         Unsupervisedclassification1.Enabled := (MapDraw.MultiGridOnMap <> 0);
         NDVI1.Visible := (MapDraw.MultiGridOnMap <> 0);
         Mergeallimages1.Visible := (NumSatImageOpen > 1);
         Savesatelliteimage1.Visible := MapDraw.ValidSatOnMap;
         Imagepalette1.Visible := MapDraw.ValidSatOnMap;
         Reregisterimage1.Visible := MapDraw.ValidSatOnMap and (MDDef.ProgramOption = ExpertProgram);
         Newband1.Visible := MapDraw.ValidSatOnMap and (SatImage[MapDraw.SATonMap] <> Nil);
         NLCD1.Visible := false;
         MrSidSpeedButton.Visible := MapDraw.ValidSatOnMap and (SatImage[MapDraw.SatOnMap].CurrentSidName <> '');
         Satellitehistograms1.Visible := MapDraw.ValidSatOnMap and  SatImage[MapDraw.SatOnMap].CanEnhance;
         Allmapsmatchthiscoveragearea1.Visible := (MDDef.ProgramOption in [ExpertProgram,RemoteSensingProgram]) and ((NumOpenMaps > 1) or (WMDEM.MDIChildCount > 1));
         NLCDLegend1.Visible := (MDDef.ProgramOption in [ExpertProgram,GeographyProgram,RemoteSensingProgram]) and (MapDraw.ValidDEMonMap and (DEMGlb[MapDraw.DEMonMap].LandCoverGrid));
         Landsatmetadata1.Visible := MapDraw.ValidSatOnMap and (SatImage[MapDraw.SatOnMap].LandsatNumber <> 0);
         Sentinel2Metadata1.Visible := MapDraw.ValidSatOnMap and SatImage[MapDraw.SatOnMap].IsSentinel2;
         LandsatQA1.Visible := MapDraw.ValidSatOnMap and (SatImage[MapDraw.SatOnMap].LandsatNumber <> 0);
         Landsatmetadata2.Visible := MapDraw.ValidSatOnMap and (SatImage[MapDraw.SatOnMap].LandsatNumber <> 0);
         VISandNIRsurfacebands1.Visible := MapDraw.ValidSatOnMap and (SatImage[MapDraw.SatOnMap].LandsatNumber <> 0);
         Sentinel2Metadata2.Visible := MapDraw.ValidSatOnMap and SatImage[MapDraw.SatOnMap].IsSentinel2;
      {$EndIf}

      {$IfDef ExDTED}
         DTED1.Visible := false;
      {$Else}
         DTED1.Visible := (MDDef.ProgramOption in [ExpertProgram]);
      {$EndIf}

      {$IfDef IncludePeakIslandArea}
         Peakislandarea1.Visible := ExpertDEMVersion;
      {$Else}
         Peakislandarea1.Visible := false;
      {$EndIf}



{$If Defined(RecordCheckProperTix)} WriteLineToDebugFile('CheckProperTix finish sats'); {$EndIf}

      LASclassificationlegend1.Visible := ValidDEM(MapDraw.DEMonMap) and (DEMGlb[MapDraw.DEMonMap].DEMHeader.ElevUnits in [euLASclass13]);
      Editshapefilegroup1.Visible := MapDraw.MapOverlays.ovShapeFileGroup <> '';

      Bearing1.Visible := (MDdef.ProgramOption in [ExpertProgram,GeologyProgram]);
      Geodeticbearing1.visible := (MDDef.ProgramOption = ExpertProgram);
      Greatcircleroute1.visible := (MDDef.ProgramOption = ExpertProgram);
      RGBvalues1.visible := (MDDef.ProgramOption = ExpertProgram);
      AreaOfSingleColor1.visible := (MDDef.ProgramOption = ExpertProgram);
      Maplibrarycoverage1.visible := (MDDef.ProgramOption = ExpertProgram) and AllowMapLibraryLoads;
      Blankmapcolor1.Visible := MapDraw.MapType in [mtDEMBlank,mtSatBlank];

      PLSSfromkeyboardlocation1.Visible := MDDef.ShowPLSS and (MDDef.ProgramOption in [ExpertProgram]);

      Modefilter1.Visible := ExpertDEMVersion  and (DEMGlb[MapDraw.DEMonMap].DEMHeader.DEMPrecision = ByteDEM);
      Fillwithmodefilter1.Visible := ExpertDEMVersion  and (DEMGlb[MapDraw.DEMonMap].DEMHeader.DEMPrecision = ByteDEM);

      {$IfDef ExGeostats}
         Findpeaks1.visible := false;
         Roads1.visible := false;
         Buildingedges1.visible := false;
         Walls1.visible := false;
         Pointsnotmeetingcriteria1.Visible := false;
         MaskDEM1.Visible := false;
         GridMigration1.Visible := false;
      {$Else}
         Findpeaks1.visible := ExpertDEMVersion;
         Roads1.visible := ExpertDEMVersion;
         Buildingedges1.visible := ExpertDEMVersion;
         Walls1.visible := ExpertDEMVersion;
         Pointsnotmeetingcriteria1.Visible := (GridMaskDEM > 0);
         MaskDEM1.Visible := (GridMaskDEM > 0);
         GridMigration1.Visible := ExpertDEMVersion and MDDef.ShowGeomorphometry;  // and (NumDEMDataSetsOpen >= 2);
      {$EndIf}

      ForGoogleearth1.visible := true;
      CreateimagetomatchDEM1.visible := (NumSatImageOpen > 0);
      Clearrangecircles1.Visible := (MapDraw.RangeCirclesFName <> '');

      Colorsfromgridcategories1.visible := ExpertDEMVersion and (DEMGlb[MapDraw.DEMonMap].DEMheader.DEMPrecision = SmallIntDEM);
      Bytedata0tomissing1.visible := ExpertDEMVersion and (DEMGlb[MapDraw.DEMonMap].DEMheader.DEMPrecision = ByteDEM);

      ID1.Visible := (MapDraw.MapOwner in [moMapDatabase]);

      Database1.Visible := true;

      ID1.Visible := NumOpenDatabaseThisMap(Self) > 0;

      Rivers1.Visible := (RiversGISFileName <> '');
      Highways1.Visible := (HighwayGISFileName <> '');
      States1.Visible := (StateGISFileName <> '');
      Counties1.Visible := (CountyGISFileName <> '');

      SpeedButton2.Visible := (MDDef.ProgramOption = ExpertProgram);

{$If Defined(RecordCheckProperTix)} WriteLineToDebugFile('CheckProperTix start vegies'); {$EndIf}

      {$IfDef ExVegDensity}
          LoadVegetation1.Visible := false;
      {$Else}
          LoadVegetation1.Visible := ExpertDEMVersion;
          Loadvegetationlayers1.Visible := ExpertDEMVersion;
          Loadvegetationlayers1.Checked := ExpertDEMVersion and (DEMGlb[MapDraw.DEMonMap].VegDensityLayers[1] <> Nil);
          Loadsecondvegeationlayers1.Visible := ExpertDEMVersion and (DEMGlb[MapDraw.DEMonMap].VegDensityLayers[1] <> Nil);
          Loadsecondvegeationlayers1.Checked := ExpertDEMVersion and (DEMGlb[MapDraw.DEMonMap].VegDensityLayers[2] <> Nil);
          Vegetationdensitylayers1.Visible := ExpertDEMVersion and (DEMGlb[MapDraw.DEMonMap].VegDensityLayers[1] <> Nil);
          Loadvegetationgrid1.Visible := ExpertDEMVersion;
          Loadvegetationgrid1.Checked := ExpertDEMVersion and (DEMGlb[MapDraw.DEMonMap].VegGrid[1] <> 0);
          Loadvegetationgrid21.Visible := ExpertDEMVersion and (DEMGlb[MapDraw.DEMonMap].VegGrid[1] <> 0);
          Loadvegetationgrid21.Checked := ExpertDEMVersion and (DEMGlb[MapDraw.DEMonMap].VegGrid[2] <> 0);
      {$EndIf}

      Close1.Visible := true;

      Deletesavedbasemaps1.Visible := ExpertDEMVersion;
      Matchimage1.Visible := (MDDef.ProgramOption = ExpertProgram);

      Convertcoordinates1.Visible := MDDef.ShowCartography;

      Level0countries1.Visible := PathIsValid(GADMDir + 'ADM0\') and (MDDef.ProgramOption in [ExpertProgram,GeographyProgram]);
      Level1stateprovince1.Visible := PathIsValid(GADMDir + 'ADM1\') and (MDDef.ProgramOption in [ExpertProgram,GeographyProgram]);
      GADM1.Visible := Level0countries1.Visible or Level1stateprovince1.Visible;
      SpeedButton3.Visible := Level1stateprovince1.Visible;

      Exportmaplibrary1.Visible := (MDDef.ProgramOption = ExpertProgram);

      {$IfDef ExAdvancedGIS}
         Colorsfromgridcategories1.Visible := false;
      {$EndIf}

{$If Defined(RecordCheckProperTix)} WriteLineToDebugFile('CheckProperTix vegie dems'); {$EndIf}
      if (MapDraw.ValidDEMonMap) then begin
         {$IfDef ExVegGrid}
            Loadvegetationgrid21.Visible := false;
            Loadvegetationgrid1.Visible := false;
         {$Else}
            if (DEMGlb[MapDraw.DEMonMap].VegGrid[1] <> 0) then begin
               Loadvegetationgrid1.Caption := LoadVegGridString + ' (' + DEMGlb[DEMGlb[MapDraw.DEMonMap].VegGrid[1]].AreaName + ')';
            end
            else Loadvegetationgrid1.Caption := LoadVegGridString;
            Loadvegetationgrid21.Visible := Loadvegetationgrid1.Checked;
            Loadvegetationgrid21.Checked := (DEMGlb[MapDraw.DEMonMap].VegGrid[2] <> 0);
            if (DEMGlb[MapDraw.DEMonMap].VegGrid[2] <> 0) then begin
               Loadvegetationgrid21.Caption := LoadVegGridString + ' (' + DEMGlb[DEMGlb[MapDraw.DEMonMap].VegGrid[2]].AreaName + ')';
            end
            else Loadvegetationgrid21.Caption := LoadVegGridString;
         {$EndIf}

         {$IfDef ExVegDensity}
            Loadvegetationlayers1.Visible := false;
            Loadsecondvegeationlayers1.Visible := false;
         {$Else}
            if DEMGlb[MapDraw.DEMonMap].VegDensityLayers[1] = Nil then begin
               Loadvegetationlayers1.Caption := LoadVegLayersStr;
            end
            else begin
              Loadvegetationlayers1.Caption := LoadVegLayersStr +  ' (' + DEMGlb[MapDraw.DEMonMap].VegDensityLayers[1].VegDensityName + ')';
            end;
            if DEMGlb[MapDraw.DEMonMap].VegDensityLayers[2] = Nil then begin
               Loadsecondvegeationlayers1.Caption := LoadVegLayersStr + ' 2';
            end
            else begin
              Loadsecondvegeationlayers1.Caption := LoadVegLayersStr + ' 2' +  ' (' + DEMGlb[MapDraw.DEMonMap].VegDensityLayers[2].VegDensityName + ')';
            end;
         {$EndIf}
      end;

      {$IfDef ExExif}
         ViewExifimages1.Visible := false;
      {$Else}
         ViewExifimages1.Visible := (MDdef.ProgramOption = ExpertProgram);
      {$EndIf}

      {$If Defined(RecordCheckProperTix)} WriteLineToDebugFile('CheckProperTix start point clouds'); {$EndIf}

      {$IfDef ExPointCloud}
         PointCloud1.Visible := false;
         LASFile1.Visible := false;
         LVIS1.Visible := false;
         PointCloudSpeedButton.Visible := false;
         LAScategoriestoshow1.Visible := false;
      {$Else}
         LAScategoriestoshow1.Visible := (pt_cloud_opts_fm <> Nil);
         PointCloudSpeedButton.Visible := MDDef.ShowPointClouds;
         LasFile1.Visible := MDDef.ShowPointClouds;
         Pointcloud1.Visible := MDDef.ShowPointClouds;
         LVIS1.Visible := MDDef.ShowPointClouds;
      {$EndIf}

     {$IfDef IncludeBILWrite}
        BIL1.Visible := MapDraw.ValidDEMonMap and (DEMGlb[MapDraw.DEMonMap].DEMheader.DEMUsed = ArcSecDEM);
        Gridfloat1.Visible := BIL1.Visible;
     {$Else}
        BIL1.Visible := false;
        Gridfloat1.Visible := false;
     {$EndIf}

      {$IfDef ExViewshed}
         FieldData1.Visible := false;
      {$EndIf}

     {$IfDef ExExoticMaps}
         AnaglyphSpeedButton.Visible := false;
         GE_SpeedButton.Visible := false;
      {$Else}
         AnaglyphSpeedButton.Visible := MapDraw.ValidDEMonMap and (MDdef.ProgramOption in [ExpertProgram,GeologyProgram]);
         GE_SpeedButton.Visible := (MDdef.ProgramOption in [ExpertProgram,GeologyProgram]);
      {$EndIf}

      {$IfDef ExFMX3D}
         OGL_SpeedButton.Visible := false;
      {$Else}
         OGL_SpeedButton.Visible := (MapDraw.ValidDEMonMap);
      {$EndIf}

      Twomaps1.visible := (MDDef.ProgramOption = ExpertProgram) and (NumOpenMaps > 1);
      DEMelevationmap1.Visible := ExpertDEMVersion and (not MapDraw.DEMMap);

      RoamingZvaluesfrom2dgrid1.Visible := ExpertDEMVersion and (NumDEMDataSetsOpen > 1);
      Contoursfromsecondgrid1.Visible := RoamingZvaluesfrom2dgrid1.Visible;

      if (MapDraw.ValidDEMonMap) then begin
         Bytes1.Enabled := not (DEMGlb[MapDraw.DEMonMap].DEMheader.DEMPrecision = ByteDEM);
         Integer161.Enabled := not (DEMGlb[MapDraw.DEMonMap].DEMheader.DEMPrecision = SmallIntDEM);
         Word1.Enabled := not (DEMGlb[MapDraw.DEMonMap].DEMheader.DEMPrecision = WordDEM);
         FloatingPoint1.Enabled := not (DEMGlb[MapDraw.DEMonMap].DEMheader.DEMPrecision = FloatingPointDEM);
      end;

      ChangeMap2.Visible := (MapDraw.MapType = mtGGRReflect) and (MDDef.ProgramOption in [ExpertProgram,RemoteSensingProgram]);
      DifferenceMap1.Visible := (MapDraw.DEMmap) and (DEMGlb[MapDraw.DEMonMap].DEMHeader.ElevUnits in [euDifference,euElevDiff]) and (MDDef.ProgramOption in [ExpertProgram,RemoteSensingProgram]);

      Drainage1.Visible := ExpertDEMVersion;

      DerivativeGrid1.Visible := ExpertDEMVersion;
      Slopes1.visible := (MDDef.ProgramOption in [ExpertProgram,GeologyProgram]) and (MapDraw.ValidDEMonMap);
      Steepestslope1.Visible := ExpertDEMVersion;
      Directional1.Visible := ExpertDEMVersion;
      Dataheader1.Visible := ExpertDEMVersion;
      Elevations1.visible := ExpertDEMVersion;
      ViewShed2.Visible := ExpertDEMVersion;
      LineOfSight1.Visible := ExpertDEMVersion;
      Horizontalearthcurvature1.Visible := ExpertDEMVersion;
      Missingdatatosealevel1.Visible := ExpertDEMVersion;
      ThinDEM1.Visible := ExpertDEMVersion;
      Volume1.Visible := ExpertDEMVersion;
      Mapshadingoptions1.Visible := ExpertDEMVersion;
      SameElevationColors1.Visible := ExpertDEMVersion;
      Intervisibility1.visible := (MDDef.ProgramOption in [ExpertProgram]) and (MapDraw.ValidDEMonMap) and MDDef.ShowIntervisibility;

      Ridges1.Visible := ExpertDEMVersion;
      Ridges.Visible := ExpertDEMVersion;
      RenameDEM1.Visible := ExpertDEMVersion and MDDef.ShowDataProperties;

      SaveDEM1.Visible := (MDDef.ProgramOption in [ExpertProgram,GeographyProgram,GeologyProgram,RemoteSensingProgram]) and MapDraw.DEMMap;

      SaveasPixelispoint1.Visible := (MDDef.ProgramOption in [ExpertProgram,RemoteSensingProgram]) and MapDraw.DEMMap and (DEMGlb[MapDraw.DEMonMap].DEMHeader.RasterPixelIsGeoKey1025 in [1]);
      SaveasPixelisArea1.Visible := (MDDef.ProgramOption in [ExpertProgram,RemoteSensingProgram]) and MapDraw.DEMMap and (DEMGlb[MapDraw.DEMonMap].DEMHeader.RasterPixelIsGeoKey1025 in [2]);

      PickcornersMDDEM1.Visible := (MDDef.ProgramOption in [ExpertProgram]);
      PortionofDEMwithdata1.Visible := (MDDef.ProgramOption in [ExpertProgram,GeographyProgram,GeologyProgram,RemoteSensingProgram]);
      GeoTIFF1.Visible := (MDDef.ProgramOption in [ExpertProgram,RemoteSensingProgram]);
      CurrentsubsetGeotiff1.Visible := (MDDef.ProgramOption in [ExpertProgram,RemoteSensingProgram]);
      BIL1.Visible := (MDDef.ProgramOption in [ExpertProgram]);
      Gridfloat1.Visible := (MDDef.ProgramOption in [ExpertProgram]);
      DBFfile1.Visible := (MDDef.ProgramOption in [ExpertProgram]);
      JSON1.Visible := (MDDef.ProgramOption in [ExpertProgram]);
      ASCII2.Visible := (MDDef.ProgramOption in [ExpertProgram]);
      Notrecommended1.Visible := (MDDef.ProgramOption in [ExpertProgram]);
      ReinterpolateDEM1.Visible := (MDDef.ProgramOption in [ExpertProgram,RemoteSensingProgram]);
      Pickmode1.Visible := (MDDef.ProgramOption in [ExpertProgram,RemoteSensingProgram]);
      CreateimagetomatchDEM1.Visible := (MDDef.ProgramOption in [ExpertProgram]);
      SourceContours1.Visible := ExpertDEMVersion and MDDef.ShowGeomorphometry;
      Oceanography1.Visible := MDDef.ShowOceanographyOptions;

      StreamProfile1.Visible := (MDdef.ProgramOption in [ExpertProgram]) and MapDraw.ValidDEMonMap;

      USProperties1.Visible := (MDDef.ProgramOption = ExpertProgram);
      Requiredantennaheight1.Visible := MDDef.ProgramOption = ExpertProgram;
      Offset1.Visible := MDdef.ProgramOption = ExpertProgram;
      ReplayFlightRoute1.Visible := ExpertDEMVersion;
      MatchOtherMaps1.Visible := (MDdef.ProgramOption in [ExpertProgram,RemoteSensingProgram]) and (NumOpenMaps > 1);
      MatchOtherMaps2.Visible := (MDdef.ProgramOption in [ExpertProgram,RemoteSensingProgram]) and (NumOpenMaps > 1);
      MatchOtherMaps3.Visible := (MDdef.ProgramOption in [ExpertProgram,RemoteSensingProgram]) and (NumOpenMaps > 1);
      USGSquadnames1.Visible := (MDDef.ProgramOption = ExpertProgram) and MDDef.ShowUSGSQuadNames;
      BlueMarble1.visible := (MDDef.ProgramOption in [ExpertProgram]) and FileExists(MainMapData + 'nasa\world20min.jpg');

      ShapeFileGrouping1.Visible := (MDDef.ProgramOption = ExpertProgram);
      Updategrouping1.Visible := Shapefilegrouplegend1.Visible;

      InfoSpeedButton6.Visible := true;
      Info1.Visible := InfoSpeedButton6.Visible and ShowAMenu;

      DupeMapSpeedButton18.Visible := (MapDraw.PrimMapProj = Nil) and (MDDef.ProgramOption = ExpertProgram);
      Duplicatemapwindow1.Visible := DupeMapSpeedButton18.Visible;

      DEMshadingonvectormap1.Visible := (MDDef.ProgramOption = ExpertProgram) and ((MapDraw.VectorIndex <> 0) or MapDraw.ValidSatOnMap);
      Earthrotation1.Visible := (MapDraw.VectorIndex <> 0) and (MapDraw.PrimMapProj.PName in [OldStereographic,LamAzEqArea,OrthoProj]);
      Quickrotatemap1.Visible := (MapDraw.VectorIndex <> 0) and (MapDraw.PrimMapProj.PName in [OldStereographic,LamAzEqArea,OrthoProj]);
      MDDEM1.Visible := MapDraw.ValidDEMonMap;

      Geomorphometrybycategories1.Visible := MapDraw.ValidDEMonMap and (DEMGlb[MapDraw.DEMonMap].VATfileName <> '');
      {$If Defined(RecordCheckProperTix)} WriteLineToDebugFile('CheckProperTix tissot'); {$EndIf}

      TerrainCategories1.Visible := MapDraw.ValidDEMonMap and ((MDDef.ProgramOption in [ExpertProgram,GeologyProgram]) or (MDDef.ShowConversionAndAnalyze));
      TerrainCategories2.Visible := TerrainCategories1.Visible;

      RouteObservation1.Visible := ExpertDEMVersion and (MDDef.ShowViews);

      Saverangecircles1.Visible := (MapDraw.RangeCirclesFName <> '');
      RangeCircles1.Visible := (MDDef.ShowViews) and (not (MDDef.ProgramOption in [GeologyProgram]));
      RangeCirclesSpeedButton9.Visible := RangeCircles1.Visible;

      AnnotateSpeedButton1.Visible := (not (MDDef.ProgramOption in [GeologyProgram]));

      VectorOutlines1.Visible := (MDDef.ShowViews) and (not (MDDef.ProgramOption in [GeologyProgram]));
      VectorOverlaySpeedButton21.Visible := VectorOutlines1.Visible;
      DataBase2.Visible := DataBaseSpeedButton28.Visible;

      SlopeCategories1.Visible := isSlopeMap(MapDraw.MapType);
      Elevation2.Visible := IsElevationMap(MapDraw.MapType);
      ContourInterval1.Visible := (MapDraw.MapType = mtDEMContour) or OverlayUp(ovoContours);
      ContourInterval2.Visible := (MapDraw.MapType = mtDEMContour) or OverlayUp(ovoContours);

      Reflectance1.Visible := IsReflectanceMap(MapDraw.MapType);

      SaveMapAsImage1.Visible := (MDdef.ProgramOption in [ExpertProgram,RemoteSensingProgram]) and (MapDraw.PrimMapProj.PName in [UTMEllipsoidal,PlateCaree]);

      {$If Defined(RecordCheckProperTix)} WriteLineToDebugFile('CheckProperTix weapons fans'); {$EndIf}

      Loadweaponsfans1.Visible := ExpertDEMVersion and MDDef.ShowIntervisibility;
      EditWeaponsFan1.Visible := (MDdef.ProgramOption = ExpertProgram) and (MapDraw.CurrentFansTable <> 0) and MDDef.ShowIntervisibility;
      WeaponFanSpeedButton8.Visible := MDDef.ShowIntervisibility and MapDraw.ValidDEMonMap and  (not (MDDef.ProgramOption in [GeologyProgram]));;
      ViewShed2.Visible := MDDef.ShowIntervisibility and MapDraw.ValidDEMonMap and (MDDef.ShowViews);
      Viewshed1.Visible := MDDef.ShowIntervisibility and MapDraw.ValidDEMonMap and DEMGLB[MapDraw.DEMonMap].ElevationDEM;

      ViewShed2.Caption := 'Viewshed';

      SourceContours1.Visible := (MDdef.ProgramOption = ExpertProgram) and (MapDraw.PrimMapProj = Nil)and (MDDef.ShowDEMCompare);

      RequiredAntennaHeight2.Visible := MapDraw.ValidDEMonMap and DEMGLB[MapDraw.DEMonMap].ElevationDEM;
      Addgrids1.Visible := (NumDEMDataSetsOpen > 1);

      Clearmap1.Visible := (MDdef.ProgramOption = ExpertProgram);
      GridSpeedButton15.Visible := (MDdef.ProgramOption in [ExpertProgram,GeographyProgram,GeologyProgram,RemoteSensingProgram]);
      SpeedButton4.Visible := (MDdef.ProgramOption in [ExpertProgram,GeologyProgram,DragonPlotProgram]);
      Area1.Visible := SpeedButton4.Visible;
      SpeedButton8.Visible := NumOpenDB > 0;

      SpeedButton5.Visible := ExpertDEMVersion;

      {$If Defined(RecordCheckProperTix)} WriteLineToDebugFile('CheckProperTix geography'); {$EndIf}

      Convertcoordinates1.Visible := (MapDraw.VectorIndex <> 0) and (MDdef.ProgramOption = ExpertProgram) or (MapDraw.DEMMap and (DEMGlb[MapDraw.DEMonMap].DEMMapProjection <> Nil));

      {$If Defined(RecordCheckProperTix)} WriteLineToDebugFile('CheckProperTix coords vis'); {$EndIf}

      SetDEMwithmap1.Visible := (NumDEMDataSetsOpen > 0) and (not MapDraw.DEMMap);
      SetsecondDEMonmap1.Visible := (NumDEMDataSetsOpen > 1) and (not MapDraw.DEMMap) and (MapDraw.DEMonMap <> 1);

      {$If Defined(RecordCheckProperTix)} WriteLineToDebugFile('CheckProperTix plss start'); {$EndIf}

      PLSSposition1.Enabled := PathIsValid(PLSSFile[1]);
      PLSSposition1.Visible := (MDDef.ShowPLSS) and (MDdef.ProgramOption = ExpertProgram);
      PLSS1.Visible := (MDDef.ShowPLSS) and (MDdef.ProgramOption = ExpertProgram);
      PLSSlocation1.Visible := (MDDef.ShowPLSS) and (MDdef.ProgramOption = ExpertProgram);

      {$If Defined(RecordCheckProperTix)} WriteLineToDebugFile('CheckProperTix done plss'); {$EndIf}

      IDSpeedButton.Visible := (MapDraw.MapOwner in [moMapDatabase]) or AllowMapLibraryLoads;
      FennemanProvinces1.Visible := FileExists(FennemanGISFileName);

      KoppenSpeedButton7.Visible := false;
      KoppenLegend2.Visible := false;
      Koppenclimograph1.Visible := false;

      {$If Defined(RecordCheckProperTix)} WriteLineToDebugFile('CheckProperTix start dbs'); {$EndIf}

      for i := 1 to MaxDataBase do begin
         if ValidDB(i) and ((GISdb[i].theMapOwner = Self) or (MDDef.DBsOnAllMaps)) then begin
            IDSpeedButton.Visible := true;
            if GISdb[i].FocalMechsPresent then begin
               FocalMechsButton.Visible := true;
               FocalPlaneAnalysis1.Visible := true;
             end;
            {$IfDef ExGeography}
            {$Else}
               if GISdb[i].KoppenPresent then begin
                  KoppenSpeedButton7.Visible := true;
                  KoppenLegend2.Visible := true;
                  Koppenclimograph1.Visible := true;
               end;
            {$EndIf}
            if GISdb[i].StratColPresent then StratcolButton.Visible := true;
         end;
      end;

      if (MapDraw.CartoGroupShapesUp <> '') then IDSpeedButton.Visible := true;

      Agefromdepth1.Visible := (MDdef.ProgramOption in [GeologyProgram,GeographyProgram]);

      GridPosts2.Visible := (MDdef.ProgramOption = ExpertProgram) and (MDDef.ShowDataProperties) and MapDraw.ValidDEMonMap;

      ProcessLegendOptions;

      PrintSpeedButton.Visible := (MapDraw.BasicProjection in [bpUTM,bpLatLong]) or (MapDraw.ZoomableVectorMap);
      Import1.Visible := (MDdef.ProgramOption = ExpertProgram);

      if (MDdef.ProgramOption = DragonPlotProgram) then begin
      end
      else begin
         QuickStats1.Visible := (MDdef.ProgramOption = ExpertProgram);
         Geomorphometry1.Visible := (MDdef.ProgramOption = ExpertProgram);
         TimeMaps1.Visible := (MDdef.ProgramOption = ExpertProgram);
      end;

      if MapDraw.MapOwner in [moPointVerificationMap] then begin
         ImageAnalysis1.Visible := false;
         Edit2.Visible := false;
         Info1.Visible := false;
         DataBase1.Visible := false;
         File1.Visible := false;
         Modify1.Visible := false;
         Overlay1.Visible := false;
         DataBase2.Visible := false;
         Calculate1.Visible := false;
         Cartography1.Visible := false;
         RasterGIS1.Visible := false;
      end;

     {$If Defined(RecordCheckProperTix)} WriteLineToDebugFile('CheckProperTix start map owner types'); {$EndIf}

      if MapDraw.MapOwner in [moIndexMap,moMapDatabase,moEditMap] then begin
         GeologySpeedButton1.Visible := false;
         PrintSpeedButton.Visible := false;
         if not (MapDraw.MapOwner in [moMapDatabase]) then IDSpeedButton.Visible := false;
         SaveSpeedButton.Visible := false;
         ClipboardSpeedButton.Visible := false;
         WeaponFanSpeedButton8.Visible := false;
         MeasureDistanceSpeedButton14.Visible := false;
         SpeedButton1.Visible := false;
         GridSpeedButton15.Visible := false;
         RangeCirclesSpeedButton9.Visible := false;
         DupeMapSpeedButton18.Visible := false;
         InfoSpeedButton6.Visible := false;
         PickBandSpeedButton20.Visible := false;
         SpeedButton4.Visible:= false;
         KoppenSpeedButton7.Visible:= false;
         KoppenSpeedButton.Visible := false;
         SideScanButton.Visible := false;
         SubbottomSpeedButton.Visible := false;
         AnnotateSpeedButton1.Visible := false;
         Savemapwithworldfile1.Visible := false;
         Replayflightroute1.Visible := false;
         DataBaseSpeedbutton28.Visible := false;
         VectorOverlaySpeedButton21.Visible := (MapDraw.MapOwner = moEditMap);
         Cartography1.Visible := false;
         Calculate1.Visible := false;
         Overlay1.Visible := (MapDraw.MapOwner = moEditMap);
         LoadWeaponsFans1.Visible := false;
     end;

      MapLibSpeedButton.Visible := (MapDraw.MapOwner in [moMapDatabase]) or AllowMapLibraryLoads;
      LibSpeedButton.Visible := MapLibSpeedButton.Visible;

      {$IfDef ExGDAL}
         GDALinfo1.Visible := false;
      {$Else}
         if MapDraw.DEMMap then GDALinfo1.Visible := GDALGridFormat(ExtractFileExt(DEMGlb[MapDraw.DEMOnMap].DEMFileName))
         else if ValidSatImage(MapDraw.SatOnMap) then GDALinfo1.Visible := GDALImageFormat(ExtractFileExt(SatImage[MapDraw.SatOnMap].IndexFileName))
         else GDALinfo1.Visible := false;
      {$EndIf}

      if (not MDDef.ShowMenus) or (not ShowAMenu) then begin
         File1.Visible := false;
         Edit1.Visible := false;
         Edit2.Visible := false;
         Modify1.Visible := false;
         Overlay1.Visible := false;
         Calculate1.Visible := false;
         RasterGis1.Visible := false;
         Info1.Visible := false;
         Database2.Visible := false;
         Hidden1.Visible := false;
         Keyboard1.Visible := false;
         Help1.Visible := false;
         Cartography1.Visible := false;
         ImageAnalysis1.Visible := false;
      end;

      HideUndesiredOptions;

      if (ClientWidth < 700) then begin
         ClipboardSpeedButton.Visible := false;
         GridSpeedButton15.Visible := false;
      end;
      if (ClientWidth < 650) then begin
         PickBandSpeedButton20.Visible := false;
      end;
      if (ClientWidth < 600) then begin
         SaveSpeedButton.Visible := false;
      end;

      {$If Defined(RecordCheckProperTix)} WriteLineToDebugFile('CheckProperTix start arrange buttons'); {$EndIf}

      ArrangeButtons;
      SetPanButtons;

   ShowDefaultCursor;
   if (FormStyle = fsNormal) and (not CreateHiddenMap) then begin
      FormStyle := fsMDIChild;
      if MapDraw <> Nil then begin
         ClientWidth := MapDraw.MapXSize;
         ClientHeight := MapDraw.MapYSize;
      end;
   end;
   {$IfDef RecordCheckProperTix} WriteLineToDebugFile(Caption + ' CheckProperTix out'); {$EndIf}
end;



procedure TMapForm.MakeHeatMap(db : integer);
var
   HeatGrid,i : integer;
   FileNames : tStringList;
   GridSize : float64;
   bmp : tMyBitmap;
   bmpMem : tBMPMemory;

      procedure ProcessDB(HeatGrid,db : integer); inline;
      var
         xg,yg,x,y : integer;
         Lat,Long : float64;
      begin
        while not GISdb[db].MyData.Eof do begin
           if GISdb[db].ValidLatLongFromTable(Lat,Long) {and DEMGlb[HeatGrid].LatLongDegreeInDEM(Lat,Long)} then begin
              DEMGlb[HeatGrid].LatLongDegreeToDEMGridInteger(Lat,Long,xg,yg);
              for x := -1 to 1 do
                 for y := -1 to 1 do
                    DEMGlb[HeatGrid].IncrementGridValue(xg+x,yg+y);
           end;
           GISdb[db].MyData.Next;
        end;
      end;

      procedure DrawOnBitmap;
      var
         xg,yg,lastx,lasty : integer;
         Lat,Long : float64;
         First : boolean;
      begin
        GISdb[db].MyData.First;
        Bmp.Canvas.Pen.Color := clRed;
        First := true;
        while not GISdb[db].MyData.Eof do begin
           if GISdb[db].ValidLatLongFromTable(Lat,Long) then begin
              MapDraw.LatLongDegreeToScreen(Lat,Long,xg,yg);
              if MapDraw.OnScreen(xg,yg) then begin
                 if First then Bmp.Canvas.MoveTo(xg,yg)
                 else Bmp.Canvas.LineTo(LastX,LastY);
                 LastX := xg;
                 LastY := yg;
                 First := false;
              end
              else First := true;
           end;
           GISdb[db].MyData.Next;
        end;
      end;

      procedure DrawOnBitmap2;
      var
         xg,yg : integer;
         Lat,Long : float64;
      begin
        GISdb[db].MyData.First;
        while not GISdb[db].MyData.Eof do begin
           if GISdb[db].ValidLatLongFromTable(Lat,Long) then begin
              MapDraw.LatLongDegreeToScreen(Lat,Long,xg,yg);
              if MapDraw.OnScreen(xg,yg) then BMPMem.SetPixelColorSize(xg,yg,3,claRed);
           end;
           GISdb[db].MyData.Next;
        end;
      end;

      procedure MakeHeatMap;
      begin
         if AnswerIsYes('UTM grid') then begin
            ReadDefault('Heat map grid size (m)',GridSize);
            HeatGrid := CreateGridToMatchMap(cgUTM,true,WordDEM,GridSize,GridSize,MapDraw.PrimMapProj.projUTMZone);
         end
         else begin
            ReadDefault('Heat map grid size (arc sec)',GridSize);
            HeatGrid := CreateGridToMatchMap(cgLatLong,true,WordDEM,GridSize/3600,GridSize/3600,MapDraw.PrimMapProj.projUTMZone);
         end;
         DEMGlb[HeatGrid].DEMHeader.ElevUnits := Undefined;
         DEMGlb[HeatGrid].AreaName := 'Heat_map';
      end;


begin
   {$IfDef RecordHeatMap} WriteLineToDebugFile('TMapForm.MakeHeatMap, db=' + IntToStr(db)); {$EndIf}
   GridSize := 5;

   if (db = 0) then begin
     FileNames := tStringList.Create;
     FileNames.Add(LastDataBase);
     if GetMultipleFiles('Data bases for heat map',DBMaskString,FileNames,MDDef.DefDBFilter) then begin
        MakeHeatMap;
        //CopyImageToBitmap(Image1,bmp);
        //BMPMem := tBMPMemory.Create(bmp);
        StartProgress('Heat map');
        for i := 0 to pred(FileNames.Count) do begin
           UpdateProgressBar(i/FileNames.Count);
           OpenNumberedGISDataBase(db,FileNames.Strings[i]);
           ProcessDB(HeatGrid,db);
           //DrawOnBitmap2;
           CloseAndNilNumberedDB(db);
        end;
        //BMPMem.Destroy;
        //Image1.Picture.Graphic := bmp;
        //bmp.savetoFile(mdtempdir + 'map.bmp');
        //bmp.free;
        EndProgress;
        {$IfDef RecordHeatMap} WriteLineToDebugFile('TMapForm.MakeHeatMap, done files=' + IntToStr(FileNames.Count)); {$EndIf}
        FileNames.Destroy;
     end
     else exit;
   end
   else begin
     GISdb[db].LatFieldName := GISdb[db].PickField('Lat field for heat map',[ftFloat]);
     GISdb[db].LongFieldName := GISdb[db].PickField('Long field for heat map',[ftFloat]);
     GISdb[db].ItsAPointDB := true;
     MakeHeatMap;
     GISdb[db].MyData.First;
     GISdb[db].EmpSource.Enabled := false;
     ProcessDB(HeatGrid,db);
     GISdb[db].ShowStatus;
   end;
   DEMGlb[HeatGrid].CheckMaxMinElev;
   DEMGlb[HeatGrid].SelectionMap.SetUpNewDEMMapWindow(HeatGrid,mtElevSpectrum,DEMGlb[HeatGrid].AreaName,true,true,false);
   {$IfDef RecordHeatMap} WriteLineToDebugFile('TMapForm.MakeHeatMap out'); {$EndIf}
end;



procedure TMapForm.Heatmap1Click(Sender: TObject);
begin
   MakeHeatMap(0);
end;


procedure TMapForm.HideUndesiredOptions;
begin
   if (MDdef.ProgramOption = DragonPlotProgram) then begin
       Loadfeaturegrid1.Visible := false;
       Draw1.Visible := false;
       Load1.Visible := false;
       DEMsatPoint1.Visible := false;
       UndoSpeedButton.Visible := false;
       SpeedButton5.Visible := false;
       AnaglyphSpeedButton.Visible := false;
       GridSpeedButton15.Visible := false;
       DupeMapSpeedButton18.Visible := false;
       DriftModels1.Visible := false;
       UndoSpeedButton.Visible := false;
       DuplicateMapWindow1.Visible := false;
       TerrainCategories1.Visible := false;
       WorldFileImages1.Visible := false;
       Volume1.Visible := false;
       Slopes1.Visible := false;
       Elevations1.Visible := false;
       Ridges.Visible := false;
       Offset1.Visible := false;
       Areaofsinglecolor1.Visible := false;
       Greatcircleroute1.Visible := false;
       Findpeaks1.Visible := false;
       Peakislandarea1.Visible := false;
       StreamProfile1.Visible := false;
       Drainage1.Visible := false;
       Intervisibility1.Visible := false;
       GridPosts2.Visible := false;
       USproperties1.Visible := false;
       Derivativegrid1.Visible := false;
       RGBvalues1.Visible := false;
       SaveDEM1.Visible := false;
       Dataheader1.Visible := false;
       Replayflightroute1.Visible := false;
       Matchothermaps1.Visible := false;
       Matchimage1.Visible := false;
       Loadvegetationgrid1.Visible := false;
       Loadweaponsfans1.Visible := false;
       Legend1.Visible := false;
       Savemapasimage1.Visible := false;
       RenameDEM1.Visible := false;
       Tools1.Visible := false;
       Open1.Visible := false;
       Pointcloud1.Visible := false;
       PointCloudSpeedButton.Visible := false;
       LVIS1.Visible := false;
       UndoSpeedButton.Visible := false;
       LoadLOStopoprofile1.Visible := false;
       ShiftDEM1.Visible := false;
       GADM1.Visible := false;
       SaveProject1.Visible := false;
       Profiles1.Visible := false;
       DEMGridarea1.Visible := false;
       Maxmeanmingridsfrompointdata1.Visible := false;
       SpeedButton3.Visible := false;
       KeyLatitudes1.Visible := false;
       Twomaps1.Visible := false;
       LoadOSMoverlay1.Visible := false;
       MergemultipleCSVTXTfiles1.Visible := false;
       QuickStats1.Visible := false;
       Geomorphometry1.Visible := false;
       TimeMaps1.Visible := false;
       Cartography1.Visible := false;
       GeologySpeedButton1.Visible := false;
       Calculate1.Visible := false;
       Cartography1.Visible := false;
    end;

    {$IfDef ExWaveRefraction}
       Waverefraction1.Visible := false;
    {$EndIf}

    {$IfDef ExRasterGIS}
       RasterGIS1.Visible := false;
       RansacPlane1.Visible := false;
    {$Else}
       RasterGIS1.Visible := (MDdef.ProgramOption in [ExpertProgram,RemoteSensingProgram]) and (MapDraw <> Nil) and (MapDraw.DEMMap);
       RansacPlane1.Visible := (MDdef.ProgramOption = ExpertProgram);
       AddGrids1.Visible := (NumDEMDataSetsOpen > 1);
       DifferenceBetweenTwoGrids2.Visible := (NumDEMDataSetsOpen > 1);
       RatioOfTwoGrids1.Visible := (NumDEMDataSetsOpen > 1);
       ScatterPlotOfTwoGrids2.Visible := (NumDEMDataSetsOpen > 1);
       Multiplegridarithmetic1.Visible := (NumDEMDataSetsOpen > 1);
       Gridcorrelations2.Visible := (NumDEMDataSetsOpen > 1);
       RGBthreeparametermap1.Visible := (NumDEMDataSetsOpen > 2);
    {$EndIf}

    {$IfDef ExGeology}
       GeologySpeedButton1.Visible := false;
       Geology1.Visible := false;
       SourceContours1.Visible := false;
       DEMGrid1.Visible := false;
       Fabricatpoint1.Visible := false;
       Geology1.Visible := false;
       StratcolButton.Visible := false;
       FocalMechsButton.Visible := false;
       FocalPlaneAnalysis1.Visible := false;
    {$Else}
       Geology1.Visible := MDDef.ShowGeologyOptions or (MDDef.ProgramOption = GeographyProgram);
       GeologySpeedButton1.Visible := Geology1.Visible;
       Predictedseafloorages1.Visible := (MDDef.ProgramOption = ExpertProgram) and MDDef.ShowMarineGeology and FileExists(PredAgesFile);
    {$EndIf}

     {$IfDef ExSidescan}
        SideScanButton.Visible := false;
        SubbottomSpeedButton.Visible := false;
        LoadSidescanImagery1.Visible := false;
     {$Else}
        SideScanButton.Visible := MDDef.ShowSidescan;
        LoadSidescanImagery1.Visible := MDDef.ShowSidescan;
        SubbottomSpeedButton.Visible := MDDef.ShowSubBottom;
     {$EndIf}


   {$IfDef ExCartography}
      Cartography1.Visible := false;
   {$Else}
      Cartography1.Visible := MDDef.ShowCartography;
   {$EndIf}

      {$IfDef ExTissot}
         Mapdistortion1.Visible := false;
         TissotIndicatrix1.Visible := false;
      {$Else}
         Mapdistortion1.Visible := (MDDef.ProgramOption = ExpertProgram) and MapDraw.PrimMapProj.TissotEnabled;
         TissotIndicatrix1.Visible := Mapdistortion1.Visible;
      {$EndIf}

      {$IfDef ExGeography}
         KoppenSpeedButton.Visible := False;
         KoppenLegend2.Visible := false;
         Climograph1.Visible := false;
         Weather2.Visible := false;
         Physicalgeographylabs1.Visible := false;
         Evapotranspirationprecipitationwaterbudget1.Visible := false;
         Koppenclimographfromclimategrids1.Visible := false;
      {$Else}
         Physicalgeographylabs1.Visible := (MDDef.ProgramOption = GeographyProgram) or MDDef.ShowLabs or MDDef.ShowClimateAndLight;
         KoppenSpeedButton.Visible := (MDDef.ProgramOption = GeographyProgram) or MDDef.ShowLabs or MDDef.ShowClimateAndLight;
         Weather2.Visible := (MDDef.ProgramOption = GeographyProgram) or MDDef.ShowLabs or MDDef.ShowClimateAndLight;
         Climograph1.Visible := ((MDDef.ProgramOption = GeographyProgram) or MDDef.ShowLabs or MDDef.ShowClimateAndLight) and (ClimateStationDB <> 0);
      {$EndIf}

      {$IfDef ExPLSS}
         MDDef.ShowPLSS := false;
      {$EndIf}



end;

procedure TMapForm.Vegetationdensitymovie1Click(Sender: TObject);
begin
  {$IfDef ExMovies}
  {$Else}
     DEMGlb[MapDraw.DEMonMap].VegDensityLayers[1].MakeMovie(self);
  {$EndIf}
end;

procedure TMapForm.Vegetationlayeroverlay1Click(Sender: TObject);
begin
   Vegetationlayers1Click(Sender);
end;


procedure TMapForm.Vegetationlayers1Click(Sender: TObject);
begin
  {$IfDef ExVegDensity}
  {$Else}
     ReadDefault('Vegetation layer',MapDraw.ShowVegLayer);
     DoFastMapRedraw;
     DEMGlb[MapDraw.DEMonMap].VegDensityLayers[1].OverlayVegLayerOnMap(Self,MapDraw.ShowVegLayer);
   {$EndIf}
end;


procedure TMapForm.StartShapeFile(DEMNowDoing : tDEMDoingWhat);
var
   FName : PathStr;
   ShapeTypeWanted : integer;
   NoChoiceOnFields : boolean;
begin
   {$IfDef RecordShapeFileEdits} WriteLineToDebugFile('TMapForm.StartShapeFile in'); {$EndIf}
   FName := ExtractFilePath(LastDataBase);
   NoChoiceOnFields := DEMNowDoing in [OutlineDBIrregularMask,CalculateArea];
   if (NoChoiceOnFields) or GetFileNameDefaultExt('New digitized shapefile',DBNameMask,FName,false) then begin
      if (DEMNowDoing in [OutlineDBIrregularMask,CalculateArea,ShapeFirstPolygon]) then ShapeTypeWanted := 5
      else if (DEMNowDoing in [ShapePoint,ShapePointsAlongFirstLine]) then ShapeTypeWanted := 1
      else if (DEMNowDoing in [ShapeXYZPoint,ShapeTrack]) then ShapeTypeWanted := 11
      else if (DEMNowDoing in [ShapeFirstLine]) then begin
         if MDDef.Create3DShapefiles and (MapDraw.ValidDEMonMap) then ShapeTypeWanted := 13
         else ShapeTypeWanted := 3;
      end;

      if (DEMNowDoing in [CalculateArea]) then fName := Petmar.NextFileNumber(MDTempDir, 'area_',DefaultDBExt);
      if (DEMNowDoing in [OutlineDBIrregularMask]) then fName := Petmar.NextFileNumber(MDTempDir, 'target_',DefaultDBExt);

      Make_tables.DefineAndCreateANewTable(FName,(DEMNowDoing in [ShapeXYZPoint]),(ShapeTypeWanted in [1,11]),(ShapeTypeWanted in [3,5]),NoChoiceOnFields or MDDef.QuickShapeFileCoding,MDDef.QuickShapeFileCoding);
      EditTable(fName,ShapeTypeWanted,MapDraw.DEMonMap);

      MapScreenX1 := -9999;
      MapScreenY1 := -9999;
      ChangeDEMNowDoing(DEMNowDoing);
      CheckProperTix;
   end;
   {$IfDef RecordShapeFileEdits} WriteLineToDebugFile('TMapForm.StartShapeFile out'); {$EndIf}
end;


procedure TMapForm.Landsatmetadata1Click(Sender: TObject);
var
   fName : PathStr;
begin
   if MapDraw.ValidSatOnMap then begin
      fName := SatImage[MapDraw.SatOnMap].GetLandsatMetadataName;
      if (fName <> '') then  ShowInNotepadPlusPlus(fName,'Landsat metadata');
   end;
end;

procedure TMapForm.Landsatmetadata2Click(Sender: TObject);
begin
   Landsatmetadata1Click(Sender);
end;

procedure TMapForm.Landsurfacetemperature1Click(Sender: TObject);
begin
   DEMDef_Routines.SaveBackupDefaults;
   MDDef.dnConvert := dncBrightness;
   NewSatWindow(nsbLST);
   DEMDef_Routines.RestoreBackupDefaults;
end;

procedure TMapForm.Lineshapefile1Click(Sender: TObject);
begin
   {$IfDef RecordShapeFileEdits} WriteLineToDebugFile('TMapForm.Lineshapefile1Click'); {$EndIf}
   MapForShapeDigitizing := Self;
   if (Sender = Area1) or (Sender = SpeedButton4) then StartShapeFile(CalculateArea)
   else if (Sender = XYZpointsshapefile1) then StartShapeFile(ShapeXYZPoint)
   else if (Sender = PointsShapeFile1) then StartShapeFile(ShapePoint)
   else if (Sender = TrackPointFile1) then StartShapeFile(ShapeTrack)
   else if (Sender = LineShapeFile1) then StartShapeFile(ShapeFirstLine)
   else if (Sender = PointsAlongSurveyLines1) then StartShapeFile(ShapePointsAlongFirstLine)
   else StartShapeFile(ShapeFirstPolygon);
end;


procedure BroadcastLatLong(Handle : tHandle; Lat,Long : float64);
var
   k : integer;
begin
   {$IfDef GPSBroadcast} WriteLineToDebugFile('BroadcastLatLong in; # children=' + IntToStr(WmDEM.MDIChildCount)); {$EndIf}
   LastRoamLat := Lat;
   LastRoamLong := Long;
   if (WmDEM.MDIChildCount > 0) then
     for k := WmDEM.MDIChildCount-1 downto 0 do begin
         {$IfDef GPSBroadcast} WriteLineToDebugFile('Child ' + IntToStr(k) + '  ' + WmDEM.MDIChildren[k].Caption); {$EndIf}
         if (WmDEM.MDIChildren[k].Handle <> Handle) then begin
            {$IfDef GPSBroadcast} WriteLineToDebugFile('Transmit'); {$EndIf}
            PostMessage(WmDEM.MDIChildren[k].Handle,WM_BroadcastLatLongMessage,0,0);
         end
         else begin
            {$IfDef GPSBroadcast} WriteLineToDebugFile('Window that sent request'); {$EndIf}
         end;
     end;
   {$IfDef GPSBroadcast} WriteLineToDebugFile('BroadcastLatLong out');{$EndIf}
end;



procedure TMapForm.LoadSecondGrid(which : tSecondGrid);
var
   fName : PathStr;
begin
   DEMNowDoing := Calculating;
   fName := ExtractFilePath(LastDEMName);
   MapDraw.DEM2onMap := 0;
   if which = g2CHM then begin
      if (MapDraw.CHMGrid <> 0) then CloseSingleDEM(MapDraw.CHMGrid);
      if not LoadNewDEM(MapDraw.CHMGrid,fName,true,'CHM grid') then exit;
   end;
   if which = g2Change then begin
      if (Mapdraw.ChangeGrid <> 0) then CloseSingleDEM(MapDraw.ChangeGrid);
      if not LoadNewDEM(MapDraw.ChangeGrid,fName,true,'Change grid') then exit;
   end;
   if which = g2Feature then begin
      if (MapDraw.FeatureGrid <> 0) then CloseSingleDEM(MapDraw.FeatureGrid);
      if not LoadNewDEM(MapDraw.FeatureGrid,fName,true,'CHM grid') then exit;
      fName := ChangeFileExt(DEMGlb[DEMGlb[MapDraw.DEMonMap].SelectionMap.MapDraw.FeatureGrid].DEMFileName,'.vat.dbf');
      if FileExists(fName) then FeaturesDB := LoadDataBaseFile(fName);
   end;
   if MapDraw.DEMMap then MapDraw.MapType := mtGrayReflect;
   Self.BringToFront;
   DoBaseMapRedraw;
end;

procedure TMapForm.LoadFeaturegrid1Click(Sender: TObject);
begin
   LoadSecondGrid(g2Feature);
end;

procedure TMapForm.Loadchangegrid1Click(Sender: TObject);
begin
   LoadSecondGrid(g2Change);
end;

procedure TMapForm.LoadCHMgrid1Click(Sender: TObject);
begin
   LoadSecondGrid(g2CHM);
end;


function TMapForm.LoadDataBaseFile(fName : PathStr; OpenTable : boolean = true; DrawNow : boolean = true; RestrictToMapOwner : boolean = false) : integer;
{$IfDef ExGIS}
begin
{$Else}
var
   i : integer;
begin
   {$IfDef RecordGISDB} WriteLineToDebugFile('TMapForm.LoadDataBaseFile in ' + fName); {$EndIf}
   Result := OpenDBonMap('',fName,DrawNow,OpenTable,RestrictToMapOwner);
   if (Result <> 0) and GISdb[Result].CanPlot and DrawNow then begin
      if MapDraw.AllowDataBaseDrawing then begin
          if MDDef.DBsOnAllMaps and (not RestrictToMapOwner) then begin
             {$IfDef RecordGISDB} WriteLineToDebugFile('TMapForm.LoadDataBaseFile show on all maps'); {$EndIf}
             for i := 0 to pred(WMDEM.MDIChildCount) do begin
                if WMDEM.MDIChildren[i] is tMapForm then begin
                   (WMDEM.MDIChildren[i] as TMapForm).DoFastMapRedraw;
                end;
             end;
          end
          else begin
             {$IfDef RecordGISDB} WriteLineToDebugFile('TMapForm.LoadDataBaseFile show just on owner'); {$EndIf}
             DoFastMapRedraw;
          end;
      end;
      CheckProperTix;
   end;
   {$IfDef RecordGISDB} WriteLineToDebugFile('TMapForm.LoadDataBaseFile out'); {$EndIf}
{$EndIf}
end;


procedure TMapForm.Loadgrouping1Click(Sender: TObject);
begin
   LoadShapeFileGroup(self);
   CheckProperTix;
end;


procedure TMapForm.Loadlandcover1Click(Sender: TObject);
var
   NewGrid : integer;
begin
   {$IfDef ExGDAL}
   {$Else}
      NewGrid := GDALsubsetGridAndOpen(MapDraw.MapCorners.BoundBoxGeo,true,'',true);
      if DEMGlb[NewGrid].LandCoverGrid then DEMGlb[NewGrid].SelectionMap.MakeNLCDLegend(DEMGlb[NewGrid].AreaName);
   {$EndIf}
end;

procedure TMapForm.LoadLOStopoprofile1Click(Sender: TObject);
var
   i : integer;
begin
   if (LastSavedLOSfName = '') then LastSavedLOSfName := ProjectDir + 'FANS\';
   if ((Sender = Nil) and FileExists(LastSavedLOSfName)) or GetFileFromDirectory('saved LOS/topo profile',DefaultDBMask,LastSavedLOSfName) then begin
      i := LoadDataBaseFile(LastSavedLOSfName);
      if ValidDB(i) then GISdb[i].dbTablef.LOStopoprofile1Click(Nil);
   end;
end;

procedure TMapForm.LoadOSMoverlay1Click(Sender: TObject);
begin
   if (MapDraw.OSMShapesUp <> Nil) then FreeAndNil(MapDraw.OSMShapesUp);
   MapDraw.OSMShapesUp := tStringList.Create;
   MapDraw.OSMShapesUp.Add(MainMapData);
   if GetMultipleDirectories('OSM shape files',MapDraw.OSMShapesUp) then begin
      SubtractOverlay(Self,ovoWorldOutlines);
      MapDraw.DeleteSingleMapLayer(MapDraw.OSMOverlayfName);
      AddOverlay(self,ovoOSM);
      DoFastMapRedraw;
   end;
end;

procedure TMapForm.Imagery1Click(Sender: TObject);
begin
   DEMGlb[MapDraw.DEMonMap].SelectionMap.MapDraw.MapType := mtElevGray;
   ChangeElevUnits(euImagery);
end;

function TMapForm.Intersection(Lat1,Long1,Az1,Lat2,Long2,Az2 : float64; var Lat,Long : float64) : boolean;
var
   FullDistance,b1,b2,Dist : float64;

      function ZoomIn(FirstMult,LastMult,Increment : integer; var Dist,Lat,Long : float64) : boolean;
      var
         i : integer;
         Bearing,
         dAZ,LastDAz : float64;
      begin
         {$IfDef RecordIntersection} WriteLineToDebugFile('ZoomIn ' + IntToStr(Increment) ); {$EndIf}
         LastdAz := 500;
         for i := FirstMult to LastMult do begin
            Dist := i * Increment;
            VincentyPointAtDistanceBearing(Lat1,Long1,Dist,Az1,Lat,Long);
            VincentyCalculateDistanceBearing(Lat2,Long2,Lat,Long,FullDistance,Bearing);
            if (Bearing > 270) and (Az2 < 90) then dAz := (360-Bearing) + Az2
            else if (Bearing < 90) and (Az2 > 270) then dAZ := (360-Az2) + Bearing
            else dAZ := abs(Bearing - Az2);

            {$IfDef RecordIntersection} WriteLineToDebugFile('Dist=' + RealToString(Dist,-12,0) + ' Bearing=' + RealToString(Bearing,-6,1) + '  & dAz=' + RealToString(dAz,-8,2)); {$EndIf}
            if (DAz > lastDAz) and (dAz < 45) then begin
               {$IfDef RecordIntersection} WriteLineToDebugFile('Intersection ' + IntToStr(Increment) + '   ' + LatLongToString(Lat,Long)); {$EndIf}
               Dist := Dist - Increment;
               Result := true;
               exit;
            end;
            LastDAz := dAz;
         end;
         Result := false;
      end;

begin
   if ZoomIn(1,250,1000,Dist,Lat,Long) and ZoomIn((round(Dist/100)-10),(round(Dist/100)+10),100,Dist,Lat,Long)  and ZoomIn((round(Dist/10)-10),(round(Dist/10)+10),10,Dist,Lat,Long) and ZoomIn((round(Dist)-10),(round(Dist)+10),1,Dist,Lat,Long) then begin
      Result := true;
   end
   else begin
      Result := false;
      exit;
   end;

   VincentyCalculateDistanceBearing(Lat2,Long2,Lat,Long,FullDistance,B2);
   VincentyCalculateDistanceBearing(Lat1,Long1,Lat,Long,FullDistance,B1);
   if (abs(B2 - Az2) > 5) or (abs(B1 - Az1) > 5) then begin
      Result := false;
      exit;
   end;
end;


procedure TMapForm.Isplandperimeter1Click(Sender: TObject);
const
   slb = 0.1;
var
   x,y : integer;
   z : float32;
begin
//start at DEM edge, and proceed inward
//if the cell is missing data, or it is 0 because it was already marked from another direction, make it 0
//break when there is an elevation above 0
//does not change voids on land
   for y := 0 to pred(DEMGlb[MapDraw.DEMonMap].DEMheader.NumRow) do begin
      x := 0;
      while (x < DEMGlb[MapDraw.DEMonMap].DEMheader.NumCol) do begin
         z := -99;
         if DEMGlb[MapDraw.DEMonMap].MissingDataInGrid(x,y) or (not (DEMGlb[MapDraw.DEMonMap].GetElevMetersOnGrid(x,y,z)) or (abs(z) < slb)) then begin
            DEMGlb[MapDraw.DEMonMap].SetGridElevation(x,y,0);
         end;
         inc(x);
         if z > slb then break;
      end;
      x := pred(DEMGlb[MapDraw.DEMonMap].DEMheader.NumCol);
      while (x >= 0) do begin
         z := -99;
         if DEMGlb[MapDraw.DEMonMap].MissingDataInGrid(x,y) or (not (DEMGlb[MapDraw.DEMonMap].GetElevMetersOnGrid(x,y,z)) or (abs(z) < slb)) then begin
            DEMGlb[MapDraw.DEMonMap].SetGridElevation(x,y,0);
         end;
         dec(x);
         if z > slb then break;
      end;
   end;

   for x := 0 to pred(DEMGlb[MapDraw.DEMonMap].DEMheader.NumCol) do begin
      y := 0;
      while (y < DEMGlb[MapDraw.DEMonMap].DEMheader.NumRow) do begin
         z := -99;
         if DEMGlb[MapDraw.DEMonMap].MissingDataInGrid(x,y) or (not (DEMGlb[MapDraw.DEMonMap].GetElevMetersOnGrid(x,y,z)) or (abs(z) < slb)) then begin
            DEMGlb[MapDraw.DEMonMap].SetGridElevation(x,y,0);
         end;
         inc(y);
         if z > slb then break;
      end;
      y := pred(DEMGlb[MapDraw.DEMonMap].DEMheader.NumRow);
      while (y >= 0) do begin
         z := -99;
         if DEMGlb[MapDraw.DEMonMap].MissingDataInGrid(x,y) or (not (DEMGlb[MapDraw.DEMonMap].GetElevMetersOnGrid(x,y,z)) or (abs(z) < slb)) then begin
            DEMGlb[MapDraw.DEMonMap].SetGridElevation(x,y,0);
         end;
         dec(y);
         if z > slb then break;
      end;
   end;
   RespondToChangedDEM;
end;


procedure TMapForm.issot1Click(Sender: TObject);
begin
   {$IfDef ExTissot}
   {$Else}
      Mapdraw.DrawTissotIndicatrix(Image1.Canvas,RightClickLat,RightClickLong);
   {$EndIf}
end;

procedure TMapForm.IwashishiandPikeclassification1Click(Sender: TObject);
begin
    IwashishiandPikeclassificationMap(MapDraw.DEMonMap);
end;

function TMapForm.ToolAndPanelHeights : integer;
begin
   Result := Panel1.Height + BlendPanel.Height;
end;

procedure TMapForm.SatelliteDNsatpoint2Click(Sender: TObject);
var
   xg1,yg1 : float32;
   graph : tThisBaseGraph;
begin
   if (MapDraw.MultiGridOnMap <> 0) then begin
      {$IfDef RecordHyperionReflectanceGraph} WriteLineToDebugFile('TMapForm.SatelliteDNsatpoint2Click in, current maps=' + IntToStr(NumOpenMaps)); {$EndIf}
      DEMGlb[MultiGridArray[MapDraw.MultiGridOnMap].IndexBand].LatLongDegreeToDEMGrid(RightClickLat,RightClickLong,xg1,yg1);
      graph := MultiGridArray[MapDraw.MultiGridOnMap].CreateAverageReflectanceGraph(round(xg1),round(yg1),round(xg1),round(yg1));
      {$IfDef RecordHyperionReflectanceGraph} WriteLineToDebugFile('TMapForm.SatelliteDNsatpoint2Click graph set, current maps=' + IntToStr(NumOpenMaps)); {$EndIf}
      graph.GraphDraw.LLcornerText := LatLongDegreeToString(RightClickLat,RightClickLong);
      Graph.GraphDraw.MaxVertAxis := 4000;
      Graph.RedrawDiagram11Click(Nil);
      {$IfDef RecordHyperionReflectanceGraph} WriteLineToDebugFile('TMapForm.SatelliteDNsatpoint2Click done, current maps=' + IntToStr(NumOpenMaps)); {$EndIf}
   end
   else SatDNsatPoint(LastX,LastY);
end;

procedure TMapForm.Sentinel2bandreflectance1Click(Sender: TObject);
begin
   NewSatWindow(nsbSentinelReflectance);
end;

procedure TMapForm.Sentinel2metadata1Click(Sender: TObject);
var
   theFiles : tStringList;
   i : integer;
   fName : PathStr;
begin
   if MapDraw.ValidSatOnMap then begin
      TheFiles := Nil;
      FindMatchingFiles(SatImage[MapDraw.SATonMap].LandsatDir,'*.xml',TheFiles,6);
      if TheFiles.Count > 0 then begin
         for I := 1 to TheFiles.Count do begin
            fName := TheFiles.Strings[pred(i)];
            if StrUtils.AnsiContainsText(fName,'MTD_MS') or StrUtils.AnsiContainsText(fName,'MTD_TL') then ShowInNotepadPlusPlus(fName);
         end;
      end;
      TheFiles.Free;
   end;
end;

procedure TMapForm.Sentinel2metadata2Click(Sender: TObject);
begin
   Sentinel2metadata1Click(Sender);
end;

procedure TMapForm.Set0tomissingandresave1Click(Sender: TObject);
begin
   {$IfDef ExMultigrid}
   {$Else}
      MultiGridArray[MapDraw.MultiGridOnMap].ZeroAsMissing(0);
   {$EndIf}
end;

procedure TMapForm.SetClientHeight;
begin
   if DefaultSize then begin
      MapDraw.MapXSize := MDdef.DefaultMapXSize;
      MapDraw.MapYSize := MDdef.DefaultMapYSize;
   end;
   ClientWidth := MapDraw.MapXSize + GetSystemMetrics(SM_CXVSCROLL);  //ScrollBox1.HorzScrollBar.Size;
   ClientHeight := MapDraw.MapYSize + ToolAndPanelHeights + GetSystemMetrics(SM_CXVSCROLL);  //ScrollBox1.HorzScrollBar.Size;
end;


procedure TMapForm.Word1Click(Sender: TObject);
begin
   ThinDEM1Click(Sender);
end;


procedure ConvertToPNGwithWorldFile(var fName : PathStr; var ItsUTM : boolean);
{$IfDef ExSat}
begin
{$Else}
var
   fName2 : PathStr;
   NewSatImage : integer;
begin
   {$IfDef RecordDrape} WriteLineToDebugFile('ConvertToPNGwithWorldFile in'); {$EndIf}
   fName2 := ChangeFileExt(fName,'.png');
   if FileExists(fName2) then begin
      NewSatImage := OpenAndDisplaySatelliteScene(Nil,fName,true,true,true);
      ItsUTM := SatImage[NewSatImage].SelectionMap.MapDraw.IsThisMapUTM;
      CloseSingleSatelliteImage(NewSatImage);
   end
   else begin
      NakedMapOptions;   //which calls SaveBackupDefaults;
      OpenAndDisplaySatelliteScene(Nil,fName,true,true,true);
      {$IfDef RecordDrape} WriteLineToDebugFile('Scene opened'); {$EndIf}
      SatImage[NewSatImage].SelectionMap.N11view1Click(Nil);
      ItsUTM := SatImage[NewSatImage].SelectionMap.MapDraw.IsThisMapUTM;
      {$IfDef RecordDrape} WriteLineToDebugFile('1:1 view'); {$EndIf}
      SatImage[NewSatImage].SelectionMap.SaveMapWithWorldFile(fName2);
      {$IfDef RecordDrape} WriteLineToDebugFile('world map saved'); {$EndIf}
      CloseSingleSatelliteImage(NewSatImage);
      ShowDefaultCursor;
      RestoreBackupDefaults;
   end;
   fName := fName2;
   {$IfDef RecordDrape} WriteLineToDebugFile('ConvertToPNGwithWorldFile out'); {$EndIf}
{$EndIf}
end;


procedure TMapForm.Worldfileimages1Click(Sender: TObject);
var
   FileNames,
   FilesWanted : TStringList;
   i,j: integer;
   DefFilt : byte;
   ItsUTM : boolean;
   fName,Dir : PathStr;
begin
   {$IfDef RecordDrape} WriteLineToDebugFile('TMapForm.Worldfileimages1Click in'); {$EndIf}
   FilesWanted := tStringList.Create;
   FilesWanted.Add(LastWorldFileOverlay);
   DefFilt := 1;
   if GetMultipleFiles('Images to overlay','Any image format|*.png;*.jpg;*.jp2;*.gif;*.bmp;*.tif;*.tiff;*.kml;*.kmz|PNG images|*.png|TIFF images|*.tif;*.tiff',FilesWanted,DefFilt) then begin
       StartProgress('overlays');
       for i := 0 to pred(FilesWanted.Count) do begin
          UpdateProgressBar(i/FilesWanted.Count);
          fName := FilesWanted[i];
          {$IfDef RecordDrape} WriteLineToDebugFile(IntToStr(i) + '/' + IntToStr(pred(FilesWanted.Count)) + '  ' + fName); {$EndIf}
          if FileExtEquals(fName,'.KML') then begin
             if MapDraw.IsThisMapUTM  then MessageToContinue('KML can only go on geographic map')
             else MapDraw.MapOverlays.ovVectorFiles.Add(fName);
          end
          else if FileExtEquals(fName,'.KMZ') then begin
             if MapDraw.IsThisMapUTM  then MessageToContinue('KMZ can only go on geographic map')
             else begin
                UnzipKMLtoKMLDir(Dir,fName);
                FileNames := Nil;
                Petmar.FindMatchingFiles(Dir,'*.KML',FileNames);
                for j := 0 to pred(FileNames.Count) do begin
                   fName := FileNames.Strings[j];
                   {$IfDef RecordDrape} WriteLineToDebugFile('KML file: ' + fName); {$EndIf}
                   MapDraw.MapOverlays.ovVectorFiles.Add(fName);
                end;
                FileNames.Free;
             end;
          end
          else if FileExtEquals(fName,'.tif') or FileExtEquals(fName,'.jp2') then begin
             ConvertToPNGwithWorldFile(fName,ItsUTM);
             if MapDraw.IsThisMapUTM  and (not ItsUTM) then begin
                MessageToContinue('Map is UTM, and ' + ExtractFileName(fname) + ' is not');
             end
             else if (not MapDraw.IsThisMapUTM ) and (ItsUTM) then begin
                MessageToContinue('Map is not UTM, and ' + ExtractFileName(fname) + ' is UTM');
             end
             else begin
                {$IfDef RecordDrape} WriteLineToDebugFile('TIF/JP2 is now: ' + fName); {$EndIf}
                MapDraw.MapOverlays.ovVectorFiles.Add(fName);
             end;
          end
          else begin
             MapDraw.MapOverlays.ovVectorFiles.Add(fName);
          end;
       end;
       EndProgress;
       AddOverlay(Self,ovoVectors);
       DoFastMapRedraw;
       LastWorldFileOverlay := FilesWanted[0];
   end;
   FilesWanted.Free;
end;


procedure TMapForm.XYZpointsshapefile1Click(Sender: TObject);
begin
   Lineshapefile1Click(Sender);
end;


procedure TMapForm.FormCreate(Sender: TObject);
begin
   {$IfDef RecordMapFormCreation} WriteLineToDebugFile('TMapForm.FormCreate enter'); {$EndIf}
   FormOperational := false;

   //only show map where drawing is complete
   FormStyle := fsNormal;
   Visible := false;

   BlendPanel.Height := 0;
   Self.Width := 500;
   Self.Height := 250;
   RespondingToMouseMove := false;

   if (MDIChildCount = 0) then begin
      Left := 0;
      Top := 0;
   end;
   ScrollBox1.DoubleBuffered := True;  //for image panning
   Image1.Stretch := true;             //Required for Delphi 6 "feature"
   PointCloudBase := false;
   Blending := false;
   MapSubsetAllowed := true;
   UseMapForMultipleMapOperations := true;
   MapDraw := tMapDraw.Create;
   MapDraw.ClosingMapNow := false;

   {$IfDef ExWMS}
      WMSMaps.Visible := false;
      ForcereloadWMSmap1.Visible := false;
      WMSOpactiy1.Visible := false;
      ClearWMSmap1.Visible := false;
   {$Else}
      ForcereloadWMSmap1.Visible := MapDraw.WMSLayerfName <> '';
      ClearWMSmap1.Visible := MapDraw.WMSLayerfName <> '';
   {$EndIf}

   {$IfDef ExGeology}
   {$Else}
      LineTable := 0;
   {$EndIf}

   RecMoving := false;
   ShowKeyLocation := false;
   IgnoreDblClick := false;
   ClosingMapNotAllowed := false;

   AllowMapLibraryLoads := false;

   if MDDef.ShowNativeGrid or (MDDef.MapTicks in [tixLatLong,tixBoth,tixUTM]) then AddOverlay(Self,ovoGrids);

   FullDraw := true;
   ShowAMenu := true;
   MapRedrawsAllowed := true;
   SavedMergeReflectanceDEM := 0;
   FeaturesDB := 0;
   MapTOCIndex := 0;

   DrSymbol.DrawingSymbol := FilledBox;
   DrSymbol.Size  := 3;
   DrSymbol.Color := claRed;

   SavedMapImage := Nil;
   DEMEditForm := Nil;

   NoMovingForm := false;
   VariableOpaqueOverlays := false;
   OverlayOpaqueBMP := nil;
   MapBaseBMP := nil;
   SizingWindow := false;

   EraserSize := 3;
   MouseIsDown := false;
   Closable := true;
   Edit1.Visible := false;
   Edit2.Visible := false;
   Image1.Left := 0;
   Image1.Top := 0;
   ExtremeZDEM := 0;
   HighZ := -1;
   LowZ := 1;
   LastBroadcastx := -1;
   LastBroadcastY := -1;
   PanButtonsOnMap := MDDef.UseMapPanButtons;
   FullMapSpeedButton.Enabled := false;
   ZoomWindow := Nil;
   FormOperational := true;
   {$If Defined(RecordMapFormCreation) or Defined(RecordFullMapDrawProblems)} WriteLineToDebugFile('out of TMapForm.FormCreate'); {$EndIf}
end;


procedure CloseVectorMap(TheMap : integer);
begin
   if (TheMap <> 0) then begin
      VectorMap[TheMap].MapDraw.ClosingMapNow := true;
      VectorMap[TheMap].Close;
      VectorMap[TheMap] := Nil;
   end;
end;


procedure TMapForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
label
   CleanUp;
begin
   {$If Defined(RecordClosing) or Defined(RecordMapClosing)} WriteLineToDebugFile('TMapForm FormCloseQuery in, map=' + Caption); {$EndIf}
   ApplicationProcessMessages;
   if ClosingMapNotAllowed then begin
      CanClose := false;
      exit;
   end;

   if (MapDraw = Nil) or MapDraw.ClosingMapNow then begin
      CanClose := true;
      exit;
   end;

   {$IfDef ExPointCloud}
   {$Else}
      if (PointCloudBase) then begin
          pt_cloud_opts_fm.Close;
      end;
   {$EndIf}

    if (MapDraw.CurrentFansTable <> 0) and (GISdb[MapDraw.CurrentFansTable] <> Nil) then begin
       GISdb[MapDraw.CurrentFansTable].FanCanClose := true;
    end;

   if (MapDraw.MapOwner in [moHiResIntervis,moDrapeMap]) then exit;

   if (MapDraw.MapOwner in [moNone,moPointVerificationMap]) then begin
      if (MapDraw.MapOwner in [moPointVerificationMap]) then Closable := true;
      CanClose := Closable;
      {$IfDef RecordClosing} WriteLineToDebugFile('TMapForm close without checking'); if Closable then WriteLineToDebugFile('   closable'); {$EndIf}
   end
   else if (MapDraw.MapOwner in [moEditMap,moDEMSelectionMap]) and MapDraw.ValidDEMonMap then begin
       {$If Defined(RecordClosing) or Defined(RecordMapClosing)} WriteLineToDebugFile('MapOwner in [moDEMSelectionMap,moEditMap'); {$EndIf}
       CanClose := false;
       {$IfDef ExMultiGrid}
          CloseSingleDEM(MapDraw.DEMonMap);
       {$Else}
          if (MapDraw.MonthlyDBArrayOnMap <> 0) then CloseSingleMonthlyDBArray(MapDraw.MonthlyDBArrayOnMap)
          else if (MapDraw.MultiGridOnMap <> 0) then CloseSingleMultigrid(MapDraw.MultiGridOnMap)
          else begin
             CloseSingleDEM(MapDraw.DEMonMap,false,false);
             CanClose := true;
          end;
       {$EndIf}
    end
    else if MapDraw.MapOwner in [moImageSelectionMap] then begin
       {$IfDef ExSat}
       {$Else}
          if (MapDraw.MapOwner = moImageSelectionMap) and MapDraw.ValidSatOnMap then begin
             {$If Defined(RecordClosing) or Defined(RecordMapClosing)} WriteLineToDebugFile('MapOwner = moImageSelectionMap'); {$EndIf}
             CanClose := false;
             CloseSingleSatelliteImage(MapDraw.SatOnMap);
          end;
       {$EndIf}
    end
   else CanClose := Closable;
   {$If Defined(RecordClosing) or Defined(RecordMapClosing)} WriteLineToDebugFile('Exit TMapForm.FormCloseQuery fallthrough'); {$EndIf}
end;


procedure TMapForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
   i : integer;

   procedure CloseIt(j : integer);
   begin
      DEMDataBase.CloseAndNilNumberedDB(j);
   end;

begin
   {$If Defined(RecordClosing) or Defined(RecordMapClosing)}  WriteLineToDebugFile('Enter TMapForm.formClose for ' + Caption); {$EndIf}
   CloseMapTableOfContents(Self);

   if (MapDraw <> Nil) then MapDraw.ClosingMapNow := true;
   //Action := caFree;
   ApplicationProcessMessages;

   if (BlendPanel.Height > 0) then BitBtn1Click(Nil);

   If (NumOpenMaps = 1) then begin
      {$IfDef RecordClosing} WriteLineToDebugFile('TMapForm.formClose CloseAllDataBases'); {$EndIf}
      CloseAllDataBases;
   end
   else begin
      for i := 1 to MaxDataBase do begin
         if (ValidDB(i)) then begin
            {$If Defined(RecordClosing) or Defined(RecordMapClosing)}  WriteLineToDebugFile('TMapForm.formClose Close db=' + IntToStr(i)); {$EndIf}
            if (GISdb[i].theMapOwner <> Nil) and (not GISdb[i].PLSSFile) then begin
               if (GISdb[i].theMapOwner = Self) and GISdb[i].ShowLocalMapOnly then begin
                  CloseIt(i);
               end
               else if MDDef.DBsOnAllMaps then begin
                  if (NumOpenMaps = 1) then CloseIt(i);
               end
               else if (GISdb[i].theMapOwner = Self) then begin
                  CloseIt(i);
               end;
            end;
         end;
      end;
   end;
   BackToWandering;

   {$If Defined(RecordClosing) or Defined(RecordMapClosing)}  WriteLineToDebugFile('TMapForm.formClose Tdb_display_opts'); {$EndIf}
   for i := 0 to pred(WMDEM.MDIChildCount) do begin
      if (WMDEM.MDIChildren[i] is Tdb_display_opts) then begin
         if ((WMDEM.MDIChildren[i] as Tdb_display_opts).TheMapOwner = Self) then begin
            (WMDEM.MDIChildren[i] as Tdb_display_opts).Close;
         end;
      end;
   end;

   {$If Defined(RecordClosing) or Defined(RecordMapClosing)}  WriteLineToDebugFile('TMapForm.formClose MapDraw'); {$EndIf}
   if (MapDraw <> Nil) then begin
      MapDraw.MapDrawValid := false;
      MapDraw.Destroy;
      MapDraw := Nil;
   end;

   if (DEMEditForm <> nil) then begin
      DEMEditForm.Close;
      DEMEditForm := nil;
   end;

   if (VegGraph <> Nil) then begin
       VegGraph.CanCloseGraph := true;
       VegGraph.Close;
       VegGraph := Nil;
   end;

   {$If Defined(RecordClosing) or Defined(RecordMapClosing)}  WriteLineToDebugFile('TMapForm.formClose WMDEM'); {$EndIf}
   if (WMDEM <> Nil) then begin
      if (not LockStatusBar) then wmDEM.ClearStatusBarPanelText;
      WmDem.SetMenusForVersion;
   end;

   if (ZoomWindow <> Nil) and (ZoomWindow.Handle = Self.Handle) then ZoomWindow := Nil;
   Self.Destroy;
   Self := Nil;
   {$If Defined(RecordClosing) or Defined(RecordMapClosing)}  WriteLineToDebugFile('Closed map window ' + Caption); {$EndIf}
end;


procedure TMapForm.SavemapasGEOTIFF1Click(Sender: TObject);
begin
   SaveMapasGEOTIFFFile('',false);
end;


procedure TMapForm.SaveMapasGEOTIFFFile(fName : PathStr; Grayscale : boolean);
begin
   if (fName = '') then begin
      fName := WriteSatDir;
      if not GetFileNameDefaultExt('GEOTIFF name','GEOTIFF|*.TIF',FName) then exit;
      WriteSatDir := ExtractFilePath(fName);
   end;
   with MapDraw do begin
      ShowHourglassCursor;
      CaptureBMPInGeoTIFF(MapDraw,fName,Image1,Grayscale);
      ShowDefaultCursor;
   end;
end;


procedure TMapForm.Scatterplotoftwogrids2Click(Sender: TObject);
begin
    {$IfDef ExGeoStats}
    {$Else}
      N2bandscattergram1Click(Sender);
    {$EndIf}
end;

procedure TMapForm.Scribbleonmap1Click(Sender: TObject);
begin
   ChangeDEMnowDoing(Scribble);
end;

procedure TMapForm.ScrollBox1Click(Sender: TObject);
begin

end;

{$IfDef ExTopoGrain}
{$Else}

      procedure OrganizedRegionStrip(DEM : integer; var Findings : tStringList; GridLimits  : tGridLimits);
      var
         Col,Row,ColInc,RowInc : integer;
         MaxOrg,MaxDir,MaxBox,Relief : float32;
         Lat,Long : float64;
         Results : tstringList;
         TStr : shortstring;
      begin
         Results := Nil;
         ColInc := round(MDDef.PointSeparation / DEMGlb[DEM].AverageXSpace);
         RowInc := round(MDDef.PointSeparation / DEMGlb[DEM].AverageYSpace);
         Row := GridLimits.YGridLow + RowInc div 2;
         while Row <= GridLimits.YGridHigh do begin
            {$IfDef VCL}
               if (ThreadsNumDone mod 4 = 0) then UpdateProgressBar(ThreadsNumDone/ThreadsToDo);
               TInterlocked.Increment(ThreadsNumDone);
            {$EndIf}
            Col := GridLimits.XGridLow + ColInc div 2;
            while Col <= GridLimits.XGridHigh do begin
               if DEMGlb[DEM].SSOByRegionSize(Col,Row,MaxOrg,MaxBox,MaxDir,Relief,Results) then begin
                  DEMGlb[DEM].DEMGridToLatLongDegree(Col,Row,Lat,Long);
                  TStr := RealToString(Lat,-18,-8) + ',' + RealToString(Long,-18,-8) + ',' + RealToString(MaxBox,-8,-2) + ',' + RealToString(MaxOrg,-8,-3) + ',' + RealToString(MaxDir,-8,-3) + ',' + RealToString(Relief,-8,-2);
                  Findings.Add(TStr);
                  {$IfDef RecordFullGetFabricAtPoint} WriteLineToDebugFile('OrganizedRegionStrip ' + TStr); {$EndIf}
               end
               else begin
                  {$IfDef RecordFullGetFabricAtPoint} WriteLineToDebugFile('No result, col=' + IntToStr(Col) + '  row=' + IntToStr(row)); {$EndIf}
               end;
               inc(Col,ColInc);
            end;
            inc(Row,RowInc);
         end;
      end;

      function TMapForm.GridLimitsForGeomorphOps : tGridLimits;
      begin
         if (MapDraw.ValidDEMonMap) then begin
            if MDDef.GeomorphMapsFullDEM then Result := DEMGlb[MapDraw.DEMonMap].FullDEMGridLimits
            else Result := MapDraw.MapAreaDEMGridLimits;
         end;
      end;

      procedure TMapForm.FindMostOrganizedRegion;
      var
         i,it,RowInc : integer;
         Findings : array[1..MaxThreadsAllowed] of tStringList;
         fName : PathStr;
         PartLimits :  tGridLimitsArray;
      begin
         {$IfDef RecordGetFabricAtPoint} WriteLineToDebugFile('TMapForm.FindMostOrganizedRegion in'); {$EndIf}
         fName := Petmar.NextFileNumber(MDTempDir, 'fabric_region_size_','.dbf');
         for i := 1 to MDdef.MaxThreadsForPC do Findings[i] := tStringList.Create;
         Findings[1].Add('LAT,LONG,REGION_M,S2S3,FABRIC_DIR,RELIEF');

         ThreadsNumDone := 0;
         ThreadsToDo := DEMGlb[MapDraw.DEMonMap].DEMheader.NumRow div round(MDDef.PointSeparation / DEMGlb[MapDraw.DEMonMap].AverageYSpace);
         {$IfDef RecordGetFabricAtPoint} WriteLineToDebugFile('ThreadsToDo :=' + IntToStr(ThreadsToDo)); {$EndIf}

         StartProgress('Fabric region size');
         {$IfDef NoParallelFor}
            OrganizedRegionStrip(MapDraw.DEMonMap,Findings[1],DEMGlb[MapDraw.DEMonMap].FullDEMGridLimits);
         {$Else}
            RowInc := round(MDDef.PointSeparation / DEMGlb[MapDraw.DEMonMap].AverageYSpace);
            PartLimits := GetLimitsForParallelLoops(DEMGlb[MapDraw.DEMOnMap].FullDEMGridLimits,RowInc);
            TParallel.For(1,MDdef.MaxThreadsForPC,
               procedure (Value: Integer)
               begin
                  OrganizedRegionStrip(MapDraw.DEMonMap,Findings[Value],PartLimits[Value]);
               end);
               ThreadsWorking := false;

               for it := 2 to MDdef.MaxThreadsForPC do begin
                 for I := 0 to pred(Findings[it].Count) do
                    Findings[1].Add(Findings[it].Strings[i]);
                 Findings[it].Free;
               end;
         {$EndIf}
         EndProgress;
         StringListToLoadedDatabase(Findings[1],fName);
         {$IfDef RecordGetFabricAtPoint} WriteLineToDebugFile('TMapForm.FindMostOrganizedRegion out'); {$EndIf}
      end;


      procedure TMapForm.FigureGrainByRegionSize(Col,Row : integer);
      var
         GISNum : integer;
         Maxs2s3,BoxSizeForMax,DirForMax,Relief : float32;
         Lat,Long : float64;
         Findings : tStringList;
         fName : PathStr;

            procedure DoAGraph(var theGraph : tThisBaseGraph; f1,f2 : shortstring);
            var
               aTop,aLeft : integer;
            begin
                aTop := 200;
                aLeft := 500;
                if (theGraph <> Nil) then begin
                   aTop := theGraph.Top;
                   aLeft := theGraph.Left;
                   theGraph.Destroy;
                end;
                theGraph := GISdb[GISNum].CreateScatterGram(f1,f2);
                theGraph.Top := aTop;
                theGraph.Left := aLeft;
                theGraph.CanCloseGraph := false;
            end;

      begin
         {$IfDef RecordGetFabricAtPoint} WriteLineToDebugFile('TMapForm.FigureGrainByRegionSize in'); {$EndIf}
         if DEMGlb[MapDraw.DEMonMap].MissingDataInGrid(Col,Row) then exit;
         DEMGlb[MapDraw.DEMonMap].DEMGridToLatLongDegree(Col,Row,Lat,Long);

         Findings := tStringList.Create;
         DEMGlb[MapDraw.DEMonMap].SSOByRegionSize(Col,Row,Maxs2s3,BoxSizeForMax,DirForMax,Relief,Findings);

         fName := Petmar.NextFileNumber(MDTempDir, 'fabric_region_','.dbf');
         GISNum := StringListToLoadedDatabase(Findings,fName);
         if MDDef.GrainFlatGraph then DoAGraph(FabricOptions.GrainFlatGraph,'REGION_M','FLATNESS');
         if MDDef.GrainOrgGraph then DoAGraph(FabricOptions.GrainOrgGraph,'REGION_M','S2S3');
         if MDDef.GrainDir then DoAGraph(FabricOptions.GrainDirGraph,'REGION_M','FABRIC_DIR');
         if MDDef.GrainReliefGraph then DoAGraph(FabricOptions.GrainReliefGraph,'REGION_M','RELIEF');
         {$IfDef RecordGetFabricAtPoint} WriteLineToDebugFile('TMapForm.FigureGrainByRegionSize out'); {$EndIf}
      end;


{$EndIf}


procedure TMapForm.AddPointToDB(Lat,Long : float64; PointLabel : string = '');
{$IfDef ExGeology}
begin
{$Else}
var
   GetDipStrike : TGetDipStrike;
   Dip,Strike,DipDir : float32;
   OK : boolean;
   Bitmap : tMyBitmap;
begin
   if (DEMNowDoing = GetGeologySymbols) then begin
      GetDipStrike := TGetDipStrike.Create(Application);
      repeat
         GetDipStrike.ShowModal;
         StripDipAndStrike(GetDipStrike.Edit1.Text, Dip,Strike,DipDir,OK);
      until OK;
      CopyImageToBitmap(Image1,Bitmap);
      PlotDipSymbol(Bitmap,Lastx,Lasty,15,round(Dip),round(Strike),round(DipDir),tStructureType(GetDipStrike.RadioGroup1.ItemIndex),True,MDDef.MapGeoSymColor,2);
      Image1.Picture.Graphic := bitmap;
      Bitmap.Free;
      GeosymbolTable.Insert;
      GeosymbolTable.SetFieldByNameAsFloat('LAT',Lat);
      GeosymbolTable.SetFieldByNameAsFloat('LONG',Long);
      GeosymbolTable.SetFieldByNameAsString('PLOT','Y');
      GeosymbolTable.SetFieldByNameAsInteger('FEATURE',GetDipStrike.RadioGroup1.ItemIndex);
      GeosymbolTable.SetFieldByNameAsString('DIPSTRIKE',GetDipStrike.Edit1.Text);
      GeosymbolTable.SetFieldByNameAsString('NOTE',GetDipStrike.Edit2.Text);
      GeosymbolTable.Post;
      GetDipStrike.Destroy;
      exit;
   end;
   CheckProperTix;
   {$IfDef RecordEditDB} WriteLineToDebugFile('done TMapForm.AddPointToDB ' + IntToStr(DBEditting)); {$EndIf}
{$EndIf}
end;

procedure TMapForm.CurrentMapArea2Click(Sender: TObject);
begin
   DEMGlb[MapDraw.DEMonMap].CreatePartDEMHistogram(MapDraw.MapAreaDEMGridLimits);
end;

function TMapForm.CurrentMapImage : tImage;
begin
   Result := Image1;
end;


procedure TMapForm.AddLatLongPointToStreamProfile({Lat1,Long1,}Lat2,Long2 : float64);
var
   z2 : float32;
   TStr : shortstring;
begin
   z2 := 0;
   TStr := '';
   if MapDraw.ValidDEMonMap and DEMGlb[MapDraw.DEMonMap].GetElevFromLatLongDegree(Lat2,Long2,z2) then TStr := RealToString(z2,9,1);
   if (StreamProfileResults = Nil) then begin
      StreamProfileResults := tStringList.Create;
      StreamProfileResults.Add('Lat,Long,z');
   end;
   StreamProfileResults.Add(RealToString(Lat2,12,8) + ',' + RealToString(Long2,14,8) + ',' + TStr );  // + RealToString(Distance,10,2) );
   {$IfDef RecordPickRoute} WriteLineToDebugFile('point  ' + IntToStr(StreamProfileResults.Count) + '  ' + LatLongDegreeToString(Lat2,Long2) + RealToString(z2,9,1)); {$EndIf}
end;


procedure TMapForm.QuadSizeMap(LastX,LastY : integer);
var
   Lat,Long,QuadSize : float64;
begin
   MapDraw.ScreenToLatLongDegree(LastX,LastY,Lat,Long);
   ReadDefault('Quad size (minutes)',MDDef.DefaultQuadSize);
   QuadSize := MDDef.DefaultQuadSize / 60;
   Lat := trunc(Lat / Quadsize) * QuadSize;
   Long := trunc(Long / Quadsize) * QuadSize;
   MapDraw.MaximizeLatLongMapCoverage(Lat,Long,Lat+QuadSize,Long+QuadSize);
end;


procedure TMapForm.Quadtickpoints1Click(Sender: TObject);
var
   QuadTickSize,QuadSize,LatLow,LongLow,Lat,Long,NewLat,NewLong,xutm,yutm : float64;
   Results : tStringList;
   TStr : shortstring;
   OrigMapDatum,NewMapDatum : tMapProjection;
   IsNAD27 : boolean;
begin
   QuadTickSize := 2.5;
   QuadSize := 7.5;
   OrigMapDatum := tMapProjection.Create;
   NewMapDatum := tMapProjection.Create;

   ReadDefault('Quad size (minutes)',QuadSize);
   ReadDefault('Tick size (minutes)',QuadTickSize);
   IsNAD27 := AnswerIsYes('Ticks for NAD27');

   OrigMapDatum.DefineDatumFromUTMZone('NAD27',MapDraw.PrimMapProj.projUTMZone,MDDef.DefaultLatHemi,'TMapForm.Quadtickpoints1');
   NewMapDatum.DefineDatumFromUTMZone('NAD83',MapDraw.PrimMapProj.projUTMZone,MDDef.DefaultLatHemi,'TMapForm.Quadtickpoints1');

   QuadTickSize := QuadTickSize / 60;
   QuadSize := QuadSize / 60;

   LatLow := QuadSize * trunc(RightClickLat / QuadSize);
   if RightClickLong < 0 then LongLow := QuadSize * (trunc(RightClickLong / QuadSize)-1)
   else LongLow := QuadSize * trunc(RightClickLong / QuadSize);
   Results := tStringList.Create;
   if IsNAD27 then TStr := ',LAT_NAD27,LONG_NAD27' else TStr := '';

   Results.Add('LAT,LONG,X_UTM,Y_UTM' + TStr);
   Lat := LatLow;
   while Lat < LatLow + QuadSize + 0.001 do begin
      Long := LongLow;
      while Long < LongLow + QuadSize + 0.001 do begin
         if IsNAD27 then begin
            MolodenskiyTransformation(Lat,Long,NewLat,NewLong,OrigMapDatum,NewMapDatum);
            NewMapDatum.ForwardProjectDegrees(NewLat,NewLong,XUTM,YUTM);
            TStr := ',' + RealToString(Lat,-12,-4) + ',' + RealToString(Long,-12,-4);
         end
         else begin
            NewLat := Lat;
            NewLong := Long;
            NewMapDatum.ForwardProjectDegrees(NewLat,NewLong,XUTM,YUTM);
         end;
         Results.Add(RealToString(NewLat,-12,-6) + ',' + RealToString(NewLong,-12,-6) + ',' + RealToString(XUTM,-12,-2) + ',' + RealToString(YUTM,-12,-2) + TStr);
         Long := Long + QuadTickSize;
      end;
      Lat := Lat + QuadTickSize;
   end;
   OrigMapDatum.Destroy;
   NewMapDatum.Destroy;
   StringListToLoadedDatabase(Results,MDTempDir + 'quadlimits.dbf');
end;


{$IfDef IncludePeakIslandArea}

   function TMapForm.FindIslandArea(Lat,Long,z : float64; ShowOnMap : boolean) : float64;
   //removed Jan 2023; it did not work, was too specialized, and there are probably other ways to do this
   var
      Bitmap : tMyBitmap;
      x,y,Pts : Integer;
      p0 : prgb;
      zt : float32;
   begin
      if ShowOnMap then DoFastMapRedraw;
      CloneImageToBitmap(Image1,Bitmap);
      for y := 0 to pred(Bitmap.Height) do begin
         p0 := Bitmap.ScanLine[y];
         for x := 0 to pred(Bitmap.Width) do begin
            if MapDraw.ScreenToElev(x,y,zt) and (zt >= z) then begin
               p0[x] := RGBTripleBlack;
            end;
         end;
      end;
      MapDraw.LatLongDegreeToScreen(Lat,Long,x,y);
      Bitmap.Canvas.Brush.Style := bsSolid;
      Bitmap.Canvas.Brush.Color := clRed;
      Bitmap.Canvas.FloodFill(x,y,clWhite,fsBorder);
      Pts := 0;
      for y := 0 to pred(Bitmap.Height) do begin
         p0 := Bitmap.ScanLine[y];
         for x := 0 to pred(Bitmap.Width) do begin
            if SameColor(p0[x],RGBTripleRed) then inc(Pts);
         end;
      end;

      if ShowOnMap then begin
         for y := 0 to pred(Bitmap.Height) do begin
            p0 := Bitmap.ScanLine[y];
            for x := 0 to pred(Bitmap.Width) do begin
               if SameColor(p0[x],RGBTripleBlack) then p0[x] := RGBTripleWhite;
            end;
         end;
         IHSmergeOntoMap(Bitmap);
      end;
      Bitmap.Free;
      Result := Pts * sqr(MapDraw.ScreenPixelSize);
   end;
{$EndIf}

procedure TMapForm.AddRangeCirclesAtLocation(Lat,Long : float64);
var
   MapRangeCircleTable,Table : tMyData;
   Range : float64;
   Color,LineWidth : integer;
   cName : ShortString;
begin
   {$IfDef RecordRangeCircle} WriteLineToDebugFile('TMapForm.AddRangeCirclesAtLocation in'); {$EndIf}
   if (MapDraw.RangeCirclesFName = '') then begin
      MapDraw.RangeCirclesFName := NextFileNumber(MDTempDir, 'Range_circles', DefaultDBExt);
      Make_Tables.MakeRangeCircleTable(MapDraw.RangeCirclesFName);
   end;

   MapRangeCircleTable := tMyData.Create(MapDraw.RangeCirclesFName);
   if not FileExists(RangeCircleSizesfName) then EditRangeCircles;

   Table := tMyData.Create(RangeCircleSizesfName);
   Table.ApplyFilter('USE=' + QuotedStr('Y'));
   if (Table.FiltRecsInDB = 0) then begin
      {$IfDef RecordRangeCircle} WriteLineToDebugFile('TMapForm.AddRangeCirclesAtLocation but no ranges selected'); {$EndIf}
   end
   else begin
      {$IfDef RecordRangeCircle} WriteLineToDebugFile('add range circles=' + IntToStr(Table.FiltRecsInDB)); {$EndIf}
      while (not Table.eof) do begin
         {$IfDef RecordRangeCircle} WriteLineToDebugFile('Add another range circle'); {$EndIf}
         Range := Table.GetFieldByNameAsFloat('RANGE');
         Color := Table.GetFieldByNameAsInteger('COLOR');
         LineWidth := Table.GetFieldByNameAsInteger('LINE_WIDTH');
         cName := Table.GetFieldByNameAsString('NAME');
         {$IfDef RecordRangeCircle} WriteLineToDebugFile('Params read'); {$EndIf}
         MapRangeCircleTable.Insert;
         MapRangeCircleTable.SetFieldByNameAsFloat('LAT',Lat);
         MapRangeCircleTable.SetFieldByNameAsFloat('LONG',Long);
         MapRangeCircleTable.SetFieldByNameAsFloat('RANGE',Range);
         MapRangeCircleTable.SetFieldByNameAsInteger('COLOR',Color);
         MapRangeCircleTable.SetFieldByNameAsInteger('LINE_WIDTH',LineWidth);
         MapRangeCircleTable.SetFieldByNameAsString('NAME',cName);
         MapRangeCircleTable.Post;
         Table.Next;
         {$IfDef RecordRangeCircle} WriteLineToDebugFile('Params set'); {$EndIf}
      end;
      DoFastMapRedraw;
      {$IfDef RecordRangeCircle} WriteLineToDebugFile('work over (range circles)'); {$EndIf}
   end;
   Table.Destroy;
   MapRangeCircleTable.Destroy;
   {$IfDef RecordRangeCircle} WriteLineToDebugFile('TMapForm.AddRangeCirclesAtLocation out'); {$EndIf}
end;




procedure TMapForm.AddressGeocode(AskUser,SupplyLatLong : boolean; var Address : shortString; var Lat,Long : float64; ShowResults : boolean = false);

         function ExtractFloatFromString(TStr : ShortString) : float64;
         var
            i : integer;
         begin
            for i := length(TStr) downto 1 do begin
               if not (TStr[i] in ['-','.','0'..'9']) then Delete(TStr,i,1);
            end;
            Result := StrToFloat(TStr);
         end;

var
   cmd : ANSIstring;
   Results : tStringList;
   i: Integer;
   TStr : ANSIString;
   fName : PathStr;
   UseAddress,OK : boolean;
begin

//http://api.positionstack.com/v1/forward?access_key=168e84e164469bbb5f23d818aee2a8cb&query=40.7638435,-73.9729691


   if (MDDef.GoogleAPIkey = '') then begin
      MessageToContinue('No free lunch from google. Requires valid API key.  Enter now.');
      GetString('Google API key',MDDef.GoogleAPIkey,false,ReasonableTextChars);
   end;

   Lat := -99;
   Long := -999;
   if (MapDraw <> Nil) and (not SupplyLatLong) or (not UseAddress) then begin
      MapDraw.ScreenToLatLongDegree(RightClickX,RightClickY,Lat,Long);
   end;

   UseAddress := (Address <> '');

   cmd := GoogleAPIsURL + 'key=' + MDDef.GoogleAPIkey;
   if (UseAddress) then begin
      if AskUser then begin
         Petmar.GetString('address',Address,false,ReasonableTextChars);
      end;
      cmd := '&address=' + Address;
   end
   else begin
      cmd := '&latlng=' + RealToString(Lat,-18,8) + ',' +  RealToString(Long,-18,8);
   end;
   cmd := GoogleAPIsURL + 'key=' + MDDef.GoogleAPIkey + cmd + '&sensor=true';

   fName := Petmar.NextFileNumber(MDTempDir,'geocode_','.xml');
   DownloadFileFromWeb(cmd,fName);

   OK := false;
   Results := tStringList.Create;
   Results.LoadFromFile(fName);
   for i := 0 to pred(Results.Count) do begin
      if StrUtils.AnsiContainsText(Results.Strings[i],'<status>OK</status>') then begin
         OK := true;
         break
      end;
   end;

   if OK then begin
       if (UseAddress) then begin
         for i := 0 to pred(Results.Count) do begin
            if StrUtils.AnsiContainsText(Results.Strings[i],'<location>') then begin
               Lat := ExtractFloatFromString(Results.Strings[succ(i)]);
               Long := ExtractFloatFromString(Results.Strings[i+2]);
               TStr := Address + MessLineBreak + LatLongDegreeToString(Lat,Long,MDDef.OutPutLatLongMethod);
               Break;
            end;
         end;
      end
      else begin
         for i := 0 to pred(Results.Count) do begin
            if StrUtils.AnsiContainsText(Results.Strings[i],'<formatted_address>') then begin
               TStr := Results.Strings[i];
               TStr := AfterSpecifiedCharacter(TStr,'>');
               Address := BeforeSpecifiedCharacterANSI(TStr,'<');
               TStr := LatLongDegreeToString(Lat,Long,MDDef.OutPutLatLongMethod) + MessLineBreak + Address;
               Break;
            end;
         end;
      end;
      ClipBoard_Coords := true;
      Clipboard_Lat := Lat;
      ClipBoard_Long := Long;
      if ShowResults then begin
         MapDraw.MapSymbolAtLatLongDegree(Image1.Canvas,Lat,Long,MDDef.KeyLocationSymbol);
         MessageToContinue(TStr,True);
      end;
      Results.Free;
   end
   else begin
      Lat := -99;
      Long := -999;
      Clipboard_Lat := -999;
      ClipBoard_Long := -999;
      ShowInNotepadPlusPlus(Results);
   end;
end;


procedure TMapForm.Addresstolatlong1Click(Sender: TObject);
var
   Lat,Long : float64;
begin
   if (MDDEF.GeocodeAddress = '') then MDDEF.GeocodeAddress := 'enter address';
   AddressGeocode(True,false,MDDEF.GeocodeAddress,Lat,Long,true);
end;

procedure TMapForm.AddRGBwindows1Click(Sender: TObject);
begin
   {$IfDef ExSatAdvanced}
   {$Else}
      AddTMRGBWindows(Self);
   {$EndIf}
end;

procedure TMapForm.Generaloptions1Click(Sender: TObject);
begin
   MakeTempGrid(true,true);
end;

procedure TMapForm.Genericdifference1Click(Sender: TObject);
begin
   ChangeElevUnits(euDifference);
end;

procedure TMapForm.Geocodeaddress1Click(Sender: TObject);
var
   Lat,Long : float64;
   Address : shortString;
begin
   Address := '';
   AddressGeocode(false,false,Address,Lat,Long,true);
end;


procedure TMapForm.AddPointSpectralReflectance(LastX,LastY : integer);
var
   refl_sun : array[1..MaxBands] of float64;
   i,x,y,DN : integer;
   color : tPlatformColor;
begin
    for i := 1 to SatImage[MapDraw.SATonMap].NumBands do begin
       if MapDraw.ScreenToSatelliteDataGrid(i,LastX,LastY,X,Y) then begin
          DN := SatImage[MapDraw.SATonMap].GetSatPointValue(i,x,y);
          if SatImage[MapDraw.SATonMap].IsSentinel2 then begin
             refl_sun[i] := 0.0001 * DN;
          end
          else if SatImage[MapDraw.SATonMap].LandsatNumber <> 0 then begin
             refl_sun[i] := SatImage[MapDraw.SATonMap].ConvertDN(DN,i,dncReflectSun);
          end
          else refl_sun[i] := DN;
          x := SpectLibGraph.GraphDraw.Graphx(1000 * SatImage[MapDraw.SATonMap].BandWavelengths[i]);
          y := SpectLibGraph.GraphDraw.GraphY(100 * refl_sun[i]);
          color := ConvertTColorToPlatformColor(WinGraphColors[SpectLibGraph.CurrentOverlay]);
          ScreenSymbol(SpectLibGraph.Image1.Canvas,x,y,FilledBox,5,color);
       end;
    end;
    inc(SpectLibGraph.currentOverlay);
    if (SpectLibGraph.currentOverlay > 14) then SpectLibGraph.currentOverlay := 0;
end;


 procedure TMapForm.SatDNsatPoint(LastX,LastY : integer);
 var
    I, x,y,DN : integer;
    Findings : tStringList;
    Capt,TStr : shortstring;
    radiance,refl_sun : array[1..MaxBands] of float64;
 begin
    if ValidSatImage(MapDraw.SATonMap)then begin
       Capt := MapDraw.ScreenLocStr(LastX,LastY);
       Findings := tStringList.Create;
       Findings.Add(Capt);
       Findings.Add('');
       for i := 1 to SatImage[MapDraw.SATonMap].NumBands do begin
          if MapDraw.ScreenToSatelliteDataGrid(i,LastX,LastY,X,Y) then begin
             DN := SatImage[MapDraw.SATonMap].GetSatPointValue(i,x,y);
             if SatImage[MapDraw.SATonMap].IsSentinel2 then begin
                refl_sun[i] := 0.0001 * DN;
                TStr := ' TOA refl w/sun=' + RealToString(refl_sun[i],-6,3);
             end
             else if SatImage[MapDraw.SATonMap].LandsatNumber = 0 then begin
                TStr := IntegerToString(DN,6);
             end
             else begin
                Radiance[i] := SatImage[MapDraw.SATonMap].ConvertDN(DN,i,dncRadiance);
                refl_sun[i] := SatImage[MapDraw.SATonMap].ConvertDN(DN,i,dncReflectSun);
                TStr := 'DN=' + IntegerToString(DN,6) + '  radiance=' + RealToString(Radiance[i],8,2);
                if i = 10  then Findings.Add('---------------------------------------------------------------------------------------------');
                if i < 10 then begin
                   TStr := TStr + '  refl=' + RealToString(SatImage[MapDraw.SATonMap].ConvertDN(DN,i,dncReflectance),6,3) +  '  refl w/sun=' + RealToString(refl_sun[i],6,3);
                end
                else begin
                    TStr := TStr + '  TB=' + RealToString(SatImage[MapDraw.SATonMap].ConvertDN(DN,i,dncBrightness),8,2) + ' K';
                end;
             end;
             Findings.Add(TStr + '  ' + SatImage[MapDraw.SATonMap].BandLongName[i] {$IfDef SatCoordsExtra}+ '  ' + IntToStr(x) + '/' + IntToStr(y) {$EndIf});
          end;
       end;
       Findings.Add('');
       PETMAR.DisplayAndPurgeStringList(Findings,'Satellite DNs at ' + Capt);
    end;
 end;


procedure TMapForm.SatLSTatPoint(LastX,LastY : integer);

var
    I, x,y,DN : integer;
    Findings : tStringList;
    Capt : shortstring;
    tb,lambda,TLS : float64;

       procedure AssumeEmissivity(Emissivity : float64);
       begin
          TLS := TB / ( 1 + lambda * 1e-6 * TB / 1.4388e-2 * system.ln(Emissivity));
          Findings.Add('   if e=' + RealToString(Emissivity,-5,2) + '  then TLS =' + RealToString(TLs,8,2) + ' K');
       end;

 begin
    if (SatImage[MapDraw.SATonMap].LandsatNumber = 0) then begin
       MessageToContinue('Needs thermal bands (Landsat)');
    end
    else begin
    Capt := MapDraw.ScreenLocStr(LastX,LastY);
    Findings := tStringList.Create;
    Findings.Add(Capt);
    Findings.Add('');
    for i := 10 to 11 do begin
       if MapDraw.ScreenToSatelliteDataGrid(i,LastX,LastY,X,Y) then begin
          DN := SatImage[MapDraw.SATonMap].GetSatPointValue(i,x,y);
          Findings.Add(SatImage[MapDraw.SATonMap].BandLongName[i]);
          Findings.Add('DN=' + IntegerToString(DN,6) + '  radiance=' + RealToString(SatImage[MapDraw.SATonMap].ConvertDN(DN,i,dncRadiance),8,2) + '  TB=' + RealToString(SatImage[MapDraw.SATonMap].ConvertDN(DN,i,dncBrightness),8,2) + ' K');
          TB := SatImage[MapDraw.SATonMap].ConvertDN(DN,i,dncBrightness);
          lambda := SatImage[MapDraw.SATonMap].BandWavelengths[i];
          AssumeEmissivity(0.99);
          AssumeEmissivity(0.98);
          AssumeEmissivity(0.97);
          AssumeEmissivity(0.96);
          AssumeEmissivity(0.95);
          AssumeEmissivity(0.94);
          AssumeEmissivity(0.93);
          AssumeEmissivity(0.92);
          AssumeEmissivity(0.91);
          AssumeEmissivity(0.90);
          AssumeEmissivity(0.80);
          AssumeEmissivity(0.50);
          AssumeEmissivity(0.25);
          Findings.Add('---------------------------------------------------------------------------------------------');
       end;
    end;
    Findings.Add('');
    PETMAR.DisplayAndPurgeStringList(Findings,'Satellite DNs at ' + Capt);
    end;
 end;


function TMapForm.StringListToLoadedDatabase(var Findings : tStringList; fName : PathStr; DisplayNow : boolean = true; RestrictToMapOwner : boolean = false; ShowTable : boolean = true) : integer;
begin
   {$IfDef RecordGISDB} WriteLineToDebugFile('TMapForm.CSVtoLoadedDatabase in, fname=' + fName); {$EndIf}
   if (fName = '') then Fname := Petmar.NextFileNumber(MDTempDir, 'temporary_db_',DefaultDBExt);
   StringList2CSVtoDB(Findings,fName,true);
   {$IfDef RecordGISDB} WriteLineToDebugFile('TMapForm.CSVtoLoadedDatabase load db, fname=' + fName); {$EndIf}
   Result := LoadDataBaseFile(fName,ShowTable,DisplayNow,RestrictToMapOwner);
end;


procedure TMapForm.PlotNorthArrowLegend(x,y : integer);
var
   Lat,Long,Lat1,Lat2,Long1,Long2,Length : float64;
   x1,y1,x2,y2 : integer;
   Bitmap : tBitmap;
begin
   if (MapDraw.ScreenPixelSize < 2500) then begin
      MapDraw.ScreenToLatLongDegree(x,y,Lat,Long);
      Length := 50 * MapDraw.ScreenPixelSize;
      VincentyPointAtDistanceBearing(Lat,Long,Length,180,Lat1,Long1);
      MapDraw.LatLongDegreeToScreen(Lat1,Long1,x1,y1);
      VincentyPointAtDistanceBearing(Lat,Long,Length,0,Lat2,Long2);
      MapDraw.LatLongDegreeToScreen(Lat2,Long2,x2,y2);
      CopyImageToBitmap(Image1,Bitmap);
      PlotVector(Bitmap,x1,y1,x2,y2,claRed,2,true,'N');
      Image1.Picture.Graphic := Bitmap;
      Bitmap.Free;
   end;
end;


 procedure TMapForm.PostPointCoordinates(Lat,Long : float64);
 var
    TStr,SecondaryLocation,Datumshiftstr,CheckString : shortString;
    xp,yp,
    NewLat,NewLong : float64;
    z : float32;
 begin
    if (MapDraw.DEMonMap = 0) then TStr := ''
    else begin
       DEMGlb[MapDraw.DEMonMap].GetElevFromLatLongDegree(Lat,Long,z);
       TStr := '  z=' + RealToString(z,-8,1);
    end;

   DatumShiftStr := '';
   SecondaryLocation := '';
   MapDraw.PrimMapProj.ForwardProjectDegrees(Lat,Long,xp,yp);
   MapDraw.PrimMapProj.InverseProjectDegrees(xp,yp,NewLat,NewLong);
   CheckString := 'Inverse project: ' + LatLongDegreeToString(NewLat,NewLong);

   MessageToContinue(MapDraw.PrimMapProj.h_DatumCode + MessLineBreak + LatLongDegreeToString(Lat,Long,MDDef.OutPutLatLongMethod) + TStr + MessLineBreak +
      'MGRS: ' + WGS84DatumConstants.LatLongToMGRS(Lat,Long) + MessLineBreak + '--------' + MessLineBreak +
      WGS84DatumConstants.UTMStringFromLatLongDegree(Lat,Long) + MessLineBreak +
      LatLongDegreeToString(Lat,Long) + MessLineBreak +
      LatLongDegreeToString(Lat,Long,DecMinutes) + MessLineBreak +
      LatLongDegreeToString(Lat,Long,DecSeconds) + MessLineBreak + '--------' + MessLineBreak +
      SecondaryLocation +  DatumShiftStr + MapDraw.ProjectedCoordinatesStringFromLatLong(lat,long) + MessLineBreak + CheckString, true);
 end;


procedure TMapForm.PointInterpolationReport(Lat,Long : float64);
var
   xDEMg2,yDEMg2 : float32;
   Findings : tStringlist;
   fName : PathStr;
   ElevInt : tElevInterpolation;
begin
   if MapDraw.ValidDEMonMap then begin
     DEMNowDoing := Calculating;
     SaveBackupDefaults;
     DEMGlb[MapDraw.DEMonMap].LatLongDegreeToDEMGrid(Lat,Long,xDEMg2,yDEMg2);
     MDDef.DefDEMMap := mtElevIHS;
     Findings := tStringList.Create;
     for ElevInt := low(tElevInterpolation) to high(tElevInterpolation) do begin
        MDDef.wf.ElevInterpolation := ElevInt;
        CreateZoomWindow(false,5,5,xDEMg2,yDEMg2,xSATg2,ySATg2,false,false);
        ZoomWindow.Image1.Canvas.Font.Size := 14;
        ZoomWindow.Image1.Canvas.Font.Name := 'Verdana';
        ZoomWindow.Image1.Canvas.TextOut(5,5,ElevInterpolationName[ElevInt]);
        fName := NextFileNumber(MDtempDir,'pt_int_','.bmp');
        ZoomWindow.Image1.Picture.SaveToFile(fName);
        Findings.Add(fName);
        ZoomWindow.Close;
        ZoomWindow := Nil;
     end;
     MakeBigBitmap(Findings,'Point interpolation algorithms');
     RestoreBackupDefaults;
     BackToWandering;
  end;
end;


procedure TMapForm.Curvaturecategories1Click(Sender: TObject);
begin
    {$IfDef ExGeostats}
    {$Else}
       Curv_tend_map.DrawCurveCatMap(self);
    {$EndIf}
end;

procedure TMapForm.Image1DblClick(Sender: TObject);
var
   xgrid,ygrid,elev,elev2 : float32;
   Length,
   xutm1,yutm1,xutm2,yutm2,Slope,
   NextDec,
   Lat1,Long1,Lat2,Long2,Heading,Lat,Long,xf,yf,
   xe,ye,DEC,DIP,TI,GV   : float64;
   z : float32;
   RecsFound,
   i,j,x,y     : integer;
   Bitmap      : tMyBitmap;
   Checking    : tCheckPoint;
   MenuStr     : string;
   First       : boolean;
   DistStr     : AnsiString;
   TStr,TStr2,
   PrimaryLocation,SecondaryLocation : ShortString;
   Refs : tAllRefs;
   rfile : file;
   {$IfDef RegisterPhoto}
      Distance,Pitch : float64;
   {$EndIf}

         {$IfDef ExSat}
         {$Else}
            procedure MakeGraphFile(Col,Row : integer; Color : tColor);
            var
               x : integer;
            begin
              with SatImage[MapDraw.SATonMap] do begin
                  PointSatReflectanceGraph.OpenDataFile(rfile,true,Color);
                  TiffImage[1].GetPointReflectances(Col,Row,Refs);
                  for x := 1 to NumBands do begin
                     PointSatReflectanceGraph.AddPointToDataBuffer(rfile,BandAxisValue(x),Refs[x]);
                  end;
                  PointSatReflectanceGraph.ClosePointDataFile(rfile);
              end;
           end;
        {$EndIf}

         function NotSamePoint : boolean;
         begin
            NotSamePoint := (LastClickX <> LastX) or (LastClickY <> LastY);
         end;

begin
   if (DEMRequiredForOperation(MapDraw.DEMonMap) and (MapDraw.DEMonMap = 0)) then exit;
   if IgnoreDblClick then begin
      IgnoreDblClick := false;
      exit;
   end;

   if (fat_fingers_form <> Nil) then begin
      fat_fingers_form.RateFatFingers(LastX,LastY);
      exit;
   end;

   MapDraw.ScreenToLatLongDegree(Lastx,Lasty,Lat,Long);

   {$IfDef RecordDoubleClick} WriteLineToDebugFile('Image1DblClick in OK, x=' + IntToStr(lastX) + '  y=' + IntToStr(lastY) + '  ' + LatLongDegreeToString(Lat,Long)); {$EndIf}


   if (DEMNowDoing = SeekingPerpendicularProfiles) then begin
      DrawProfilesThroughPeak(MapDraw.DEMonMap,Lat,Long);
      exit;
   end;

   if (DEMNowDoing = PlotNorthArrow) then begin
      PlotNorthArrowLegend(LastX,LastY);
      exit;
   end;

   if (DEMNowDoing = SpectralReflectance) then begin
      AddPointSpectralReflectance(LastX,LastY);
      exit;
   end;

   if (DEMNowDoing = EnsembleClassSummary) then begin
      SummarizeEnsemble(Lat,Long);
      exit;
   end;

   if RecMoving and (DEMNowDoing in [MovePointDBRecs]) and (GISdb[DBEditting].MyData.FiltRecsInDB > 1) then begin
      MoveADBRecord(Lat,Long);
      exit;
   end;

   {$IfDef ExGeology}
   {$Else}
      if CheckGeologyOptions(Lat,Long,LastX,LastY,NotSamePoint) then exit;
   {$EndIf}

   {$IfDef ExAdvancedGIS}
   {$Else}
      CheckAdvancedGISDoubleClick(Lat,Long);
   {$EndIf}

   {$IfDef ExGeoStats}
   {$Else}
        if (DEMNowDoing in [GeomorphPointGraph]) and (RegionSizeForm <> Nil) then begin
           RegionSizeForm.GeomorphParameterVersusRegion(Lat,Long);
           exit;
        end;
        if (DEMNowDoing in [LagSizeSensitivity]) and (LagOptionsForm <> Nil) then begin
           LagOptionsForm.Sensitivty(Lat,Long);
           exit;
        end;
   {$EndIf}

   {$IfDef ExPointCloud}
   {$Else}
      if (DEMNowDoing = PtCloudExtractPoints) then begin
         SlicerForm.PointsAtLatLong(Lat,Long);
         exit;
      end;
   {$EndIf}

   MapDraw.ScreenToDataGrid(Lastx,Lasty,xgrid,ygrid);
   MapDraw.ScreenToUTM(Lastx,Lasty,xutm1,yutm1);

   {$IfDef ExSlicer3D}
   {$Else}
      if (GraphDoing = gdGraphDigitize) then begin
         ScreenSymbol(Image1.Canvas,LastX,LastY,FilledBox,3,claRed);
         SatImage[MapDraw.SATonMap].SatGridToUTM(SatImage[MapDraw.SatOnMap].BandForSize,xgrid,ygrid,XUTM1,YUTM1);
         SlicerForm.DigitizePoint(XUTM1,YUTM1);
         exit;
      end;

       if (DEMNowDoing = PickSliceLocation) then begin
          SlicerForm.SliceAtUTM(XUTM1,YUTM1);
          exit;
       end;

       if (DEMNowDoing = PickSlicePanorama) then begin
          {$IfDef Slicer} WriteLinetoDebugFile('PickSlicePanorama at ' + LatLongDegreeToString(Lat,Long)); {$EndIf}
          DEMGlb[MapDraw.DEMonMap].GetElevFromLatLongDegree(Lat,Long,z);
          MapDraw.DrawRangeCircleLatLong(Image1.Canvas,Lat,Long,MDDef.LidarPanClose,clRed,2);
          MapDraw.DrawRangeCircleLatLong(Image1.Canvas,Lat,Long,MDDef.LidarPanFar,clRed,2);
          {$IfDef Slicer} WriteLinetoDebugFile('PickSlicePanorama call SlicerPanorama'); {$EndIf}
          SlicerForm.SlicerPanorama(Lat,Long,z);
          exit;
       end;

       if (DEMNowDoing = PickPointCloudStats) then begin
          SlicerForm.MemoryPointCloud[1].GetRegionStats(Lat,Long);
          exit;
       end;
   {$EndIf}

   if (Veggraph <> Nil) and (DEMNowDoing = JustWandering) then begin
      ScreenSymbol(Image1.Canvas,LastX,LastY,FilledBox,3,claRed);
      exit;
   end;

   if (not MouseIsDown) and (DEMNowDoing in [{$IfDef ExMilicons}{$Else}AddMilIcon,{$EndIf} DragEdit]) and (SavedMapImage <> Nil) then begin
      CheckProperTix;
   end;

   if abs(MapDraw.PrimMapProj.projUTMZone - GetUTMZone(Long)) > 1 then begin
      MapDraw.PrimMapProj.DefineDatumFromUTMZone(MapDraw.PrimMapProj.h_DatumCode,GetUTMZone(Long),HemiFromLat(Lat),'TMapForm.Image1DblClick');
   end;

    {$IfDef ExTrackSat}
    {$Else}
       if (DEMNowDoing = VisPolarOrbiter) then begin
          DEMGlb[MapDraw.DEMonMap].GetElevFromLatLongDegree(Lat,Long,z);
          MDDef.HorizonSkyMap := true;
          SatTractForm := TSatTractForm.Create(Application);
          SatTractForm.MapOwner := Self;
          SatTractForm.ObsLat := Lat;
          SatTractForm.ObsLong := Long;
          SatTractForm.ObsElev := z;
          SatTractForm.BitBtn5Click(Nil);
          SatTractForm.BitBtn4Click(Nil);
       end;
    {$EndIf}

    {$IfDef ExMultiGrid}
    {$Else}
       if (DEMNowDoing = NDVIPointTimeSeries) then begin
          MultiGridArray[MapDraw.MultiGridOnMap].NDVIPointTimeSeries(Lat,Long);
       end;

       if (DEMNowDoing = PickTrainingPoints) then begin
          if (MapDraw.MultiGridOnMap <> 0) then begin
             MultiGridArray[MapDraw.MultiGridOnMap].AddTrainingPoint(Lat,Long);
          end;
       end;
    {$EndIf}


    {$IfDef IncludePeakIslandArea}
       if (DEMNowDoing = GetIslandArea) then begin
          if DEMGlb[MapDraw.DEMonMap].GetElevFromLatLongDegree(Lat,Long,z) then begin
              Findings := tStringList.Create;
              Findings.Add('Sea level,area');
              z := 5;
              while (z >= -121) do begin
                 Findings.Add(RealToString(z,-5,0) + ',' + SmartAreaFormat(FindIslandArea(Lat,Long,z,false)));
                 z := z - 5;
              end;
              StringListToLoadedDatabase(Findings,MDTempDir + 'Island_area_over_time.csv');
          end
          else MessageToContinue('No elevation there');
       end;
    {$EndIf}


    if (DEMNowDoing = RadiusDBEdit) then begin
       x := LastX;
       y := LastY;
       ReadDefault('Radius (m)',MDDef.FilterRadius);
       {$IfDef RecordRadiusDBEdit} WriteLineToDebugFile('RadiusDBedit, radius='+IntToStr(round(MDDef.FilterRadius))); {$EndIf}

       CloneImageToBitmap(Image1,Bitmap);
       i := Round(MDDef.FilterRadius / MapDraw.ScreenPixelSize);
       Bitmap.Canvas.Brush.Style := bsSolid;
       Bitmap.Canvas.Brush.Color := clBlack;
       Bitmap.Canvas.Ellipse(x-i,Y-i,X+i,Y+i);
       Bitmap.SaveToFile(MDTempDir + 'target-mask.bmp');
       Bitmap.Free;
       Image1.Canvas.Pen.Color := clRed;
       Image1.Canvas.Pen.Width := 2;
       Image1.Canvas.Brush.Style := bsClear;
       Image1.Canvas.Ellipse(x-i,Y-i,X+i,Y+i);
       GISdb[DBEditting].IrregularFilterDB;
       BackToWandering;
    end;

    if (DEMNowDoing = GetGreatCircleRoute) then begin
       Greatcirclethroughpoint1Click(Sender);
       Smallcirclethroughpoint1Click(Sender);
    end;

   if (DEMNowDoing = GetRGBValues) then begin
      Color := Image1.Canvas.Pixels[Lastx,LastY];
      MessageToContinue('Color = ' + IntToStr(Color) + MessLineBreak +  ColorString(Color),True);
   end;

   if (DEMNowDoing = FloodFill) then Image1.Canvas.FloodFill(LastX,LastY,clBlack,fsBorder);
   if (DEMNowDoing = TerrainBlowup) then Terrainblowup1Click(nil);

   if (DEMNowDoing = EditPointElevs) and MapDraw.ValidDEMonMap then begin
      CheckThisPoint(LastX,LastY,xDEMg1,yDEMg1,xSATg1,ySATg1,CheckNothing);
      if DEMGlb[MapDraw.DEMonMap].GetElevMetersOnGrid(round(xDEMg1),round(yDEMg1),z) then begin
         ReadDefault('Elev',z);
         DEMGlb[MapDraw.DEMonMap].SetGridElevation(round(xDEMg1),round(yDEMg1),z);
      end;
   end;

   if (DBEditting <> 0) and (DEMNowDoing in [DeletePointDBRecs,EditZDBRecs]) then begin
      GISdb[DBEditting].IdentifyRecord(LastX,LastY,Lat,Long,RecsFound,false,false,TStr,false);
      if (RecsFound = 1) then begin
         GISdb[DBEditting].MyData.Edit;
         if DEMNowDoing in [DeletePointDBRecs] then GISdb[DBEditting].MyData.Delete
         else begin
            z := GISdb[DBEditting].MyData.GetFieldByNameAsFloat('Z');
            ReadDefault('Elev',z);
            GISdb[DBEditting].MyData.SetFieldByNameAsFloat('Z',z);
            GISdb[DBEditting].MyData.Post;
         end;
         DoBaseMapRedraw;
      end;
    end;

   if DEMNowDoing in [PickDBRecsToMove,DeleteSingleDBRecs] then begin
      GISdb[DBEditting].IdentifyRecord(LastX,LastY,Lat,Long,RecsFound,False,false,TStr);
      exit;
   end;

   {$IfDef RegisterPhoto}
      if PersPitchAzimuthDigitize then with RegPhotoForm.RegPers.View3D  do begin
         DEMGlb[MapDraw.DEMonMap].GetElevFromLatLongDegree(Lat,Long,z);
         CalculateDistanceBearing(ViewerLat,ViewerLong,Lat,Long,Distance,Heading);
         Pitch := ArcTan((Z - ObserverElevation - DropEarthCurve(Distance)) / Distance) / DegToRad;
         RegPhotoForm.PostPoint(Lat,Long,Distance,Heading,Pitch);
      end;
   {$EndIf}

    {$IfDef ExGeography}
    {$Else}
       if (DEMNowDoing = SunriseClicking) then begin
          Petmar.ScreenSymbol(Image1.Canvas,Lastx,Lasty,FilledBox,2,clared);
          Image1.Canvas.TextOut(Lastx+2,Lasty+2,SunTime(Lat,Long,SunriseOptions.DiffUTC,SunriseOptions.Year,SunriseOptions.Month,SunriseOptions.Day,SunriseOptions.Morning,true,SunriseOptions.SunAngle));
       end;
   {$EndIf}


   if (DEMNowDoing in [IDDataBaseOne,IDDataBaseAll,IDDBforAction,LabelIDDataBase,EditDBRecs]) and (MapDraw.MapOwner <> moMapDatabase) then begin
      {$IfDef RecordClick} WriteLineToDebugFile('IDDataBase in record click'); {$EndIf}
      if (DBEditting = 0) then begin
         for i := 1 to MaxDataBase do begin
            if (GISdb[i] <> nil) and (GISdb[i].theMapOwner = Self) then begin
               GISdb[i].IdentifyRecord(LastX,LastY,Lat,Long,RecsFound, DEMNowDoing in [IDDataBaseOne,IDDataBaseAll],DEMNowDoing = LabelIDDataBase,TStr,false,false {DEMNowDoing = IDFilterDataBase});
            end;
         end;
      end
      else begin
         GISdb[DBEditting].IdentifyRecord(LastX,LastY,Lat,Long,RecsFound,DEMNowDoing in [IDDataBaseOne,IDDataBaseAll,EditDBRecs],(DEMNowDoing = LabelIDDataBase),TStr,false,false);
      end;
   end;

   if (DEMNowDoing = PickToBroadcast) then begin
      CheckThisPoint(LastX,LastY,xDEMg1,yDEMg1,xSATg1,ySATg1,CheckReasonable);
      DEMGlb[MapDraw.DEMonMap].DEMGridToLatLongDegree(xDEMg1,yDEMg1,Lat,Long);
      BroadcastLatLong(Self.Handle,Lat,Long);
      exit;
   end;

   (*
   if (DEMNowDoing in [USCounty]) then begin
      US_properties.GetCounty(Lat,Long,TStr);
      MessageToContinue(tStr);
      exit;
   end;
   *)

   if (DEMNowDoing = UTMTrueDeviation) then begin
      Dec := MapDraw.UTMGridToTrueNorthAngle(Lat,Long);
      ScreenSymbol(Image1.Canvas,LastX,LastY,Box,3,claRed);
      Image1.Canvas.TextOut(lastX+5,LastY+5, RealToString(Dec,-8,2) + '°');
   end;

   if (DEMNowDoing = RangeCircles) then begin
      if (MapDraw.PrimMapProj = Nil) then begin
         CheckThisPoint(LastX,LastY,xDEMg1,yDEMg1,xSATg1,ySATg1,CheckAll);
         if MapDraw.DEMMap then DEMGlb[MapDraw.DEMonMap].DEMGridToLatLongDegree(xDEMg1,yDEMg1,Lat,Long)
         else begin
            {$IfDef ExSat}
            {$Else}
                SatImage[MapDraw.SATonMap].SatGridToLatLongDegree(SatImage[MapDraw.SatOnMap].BandForSize,xSATg1,ySATg1,Lat,Long);
            {$EndIf}
         end;
      end;
      AddRangeCirclesAtLocation(Lat,Long);
   end;

   if (DEMNowDoing = VerifyingPoint) then begin
      MapDraw.ScreenToDataGrid(Lastx,Lasty,ZoomWindowxDEMg1,ZoomWindowyDEMg1);
      SizeIsCorrectThankYou := true;
      Self.Close;
      Exit;
   end;

   if (DEMNowDoing = PickCenterAndScale) then begin
      Setscale1Click(Sender);
      BackToWandering;
      exit;
   end;

   (*
   if (DEMNowDoing = DoingDatumShift) then begin
      {$IfDef RecordDatumShift} WriteLineToDebugFile('DEMNowDoing = DoingDatumShift click ' + LatLongDegreeToString(Lat,Long)): {$EndIf}
      MapDraw.ComputeDatumShifts(Image1.Canvas,Lat,Long,shift1,shift2,LabelDatumShift);
      exit;
   end;
    *)

   {$IfDef ExMag}
   {$Else}
      if (DEMNowDoing = ShowMagneticVariation) then with MDDef do begin
         MagVr1(0,lat,Long,CurMagYear, DEC,DIP,TI,GV);
         ScreenSymbol(Image1.Canvas,LastX,LastY,DefGISSymbol);
         LoadMyFontIntoWindowsFont(MDDef.DefGisLabelFont1,Image1.Canvas.Font);
         Image1.Canvas.TextOut(lastX+5,LastY+5, RealToString(Dec,-8,2) + '°');
         InitializeMagneticVariation(true);
         MagVr1(0,lat,Long,CurMagYear, NextDec,DIP,TI,GV);
         MessageToContinue(LatLongDegreeToString(Lat,Long,MDDef.OutPutLatLongMethod) + MessLineBreak + 'Magnetic declination:' + RealToString(Dec,8,2) + DegSYM + MessLineBreak +
             'Annual change:' + RealToString(NextDec -Dec,8,2) + DegSYM + '/yr',True);
         InitializeMagneticVariation(false);
      end;
   {$EndIf}


   {$IfDef ExViewshed}
   {$Else}
      CheckViewShedMapDblClick(NotSamePoint);
   {$EndIf}

   {$IfDef ExDigitize}
   {$Else}
        if (DEMNowDoing = DigitizeContourPoint) and (NotSamePoint) then XYZDisplayForm.AddPoint(Lastx,Lasty);
   {$EndIf}

   {$IfDef ExTissot}
   {$Else}
       if (DEMNowDoing = MapTissotIndicatrix) then begin
          Mapdraw.DrawTissotIndicatrix(Image1.Canvas,lat,long);
          exit;
       end;
   {$EndIf}

   {$IfDef ExPLSS}
   {$Else}
      if (DEMNowDoing = PLSSposition) then begin
         ScreenSymbol(Image1.Canvas,LastX,LastY,Box,3,claRed);
         MenuStr := LatLongDegreeToString(Lat,Long,MDDef.OutPutLatLongMethod) + MessLineBreak;
         if (PLSS[1] <> Nil) then begin
            PrimaryLocation := PLSS[1].SimplePLSSLocation(Lat,Long);
            if PrimaryLocation <> '' then MenuStr := MenuStr + 'First PLSS: ' + PrimaryLocation + MessLineBreak;
         end;
         if (PLSS[2] <> Nil) then begin
            SecondaryLocation := PLSS[2].SimplePLSSLocation(Lat,Long);
            if SecondaryLocation <> '' then MenuStr := MenuStr + 'Second PLSS: ' + SecondaryLocation + MessLineBreak;
         end;
         if (PLSS[3] <> Nil) then begin
            SecondaryLocation := PLSS[3].SimplePLSSLocation(Lat,Long);
            if SecondaryLocation <> '' then MenuStr := MenuStr + 'Third PLSS: ' + SecondaryLocation + MessLineBreak;
         end;
         {$IfDef RecordPLSS} WriteLineToDebugFile(MenuStr); {$EndIf}
         MessageToContinue(MenuStr,True);
         ChangeDEMNowDoing(PLSSposition);
      end;
   {$EndIf}


   {$IfDef ExVectorOverlay}
   {$Else}
      if NowStreamDigitizing(DEMNowDoing) or (DEMNowDoing in [SeekingStreamProfile,SeekingFlyThroughRoute]) then with MapDraw do begin
         {$IfDef RecordPickRoute} WriteLineToDebugFile('Dbl Click with route selection active'); {$EndIf}
         if DEMNowDoing in [SeekingFlyThroughRoute,OutlineDBIrregularMask,CalculateArea,CalculateVolume,SubsetByOutline,DeleteMultipleDBRecs,DeleteMultipleRecsAllDBs,
           ShapePolygon,ShapeFirstPolygon,SubsetHole,SubsetLake,SubsetByOutline,ReplaceValuesByOutline,FillHolesByOutline,StreamDistance,NewTrack,RouteObservation,ShapeLine,ShapeFirstLine,
           SeekingFlyThroughRoute,SeekingStreamProfile] then Checking := CheckAll
         else Checking := CheckReasonable;
         CheckThisPoint(LastX,LastY,xDEMg2,yDEMg2,xsatg2,ysatg2,Checking);
         First := (StreamProfileResults = Nil);
         if ValidDEMonMap then begin
            DEMGlb[DEMonMap].DEMGridToLatLongDegree(xDEMg2,yDEMg2,Lat2,Long2);
            DEMGlb[DEMonMap].DEMGridToLatLongDegree(xDEMg1,yDEMg1,Lat1,Long1);
         end
         else if MapDraw.ValidSatOnMap then begin
            {$IfDef ExSat}
            {$Else}
               SatImage[SatonMap].SatGridToLatLongDegree(SatImage[MapDraw.SatOnMap].BandForSize,xsatg2,ysatg2,Lat2,Long2);
               SatImage[SatonMap].SatGridToLatLongDegree(SatImage[MapDraw.SatOnMap].BandForSize,xsatg1,ysatg1,Lat1,Long1);
            {$EndIf}
         end
         else begin
            MapDraw.ScreenToLatLongDegree(LastX,LastY,Lat2,Long2);
            MapDraw.ScreenToLatLongDegree(MapScreenX1,MapScreenY1,Lat1,Long1);
         end;

         AddLatLongPointToStreamProfile({Lat1,Long1,}Lat2,Long2);

         if not First then begin
            if (DEMNowDoing in [SeekingStreamProfile]) then begin
               if (wmDEM <> Nil) and (WmDEM.MDIChildCount > 0) then
                  for j := WmDEM.MDIChildCount-1 downto 0 do
                      PostMessage(WmDEM.MDIChildren[j].Handle,WM_ClearListeningWindow,0,0);
            end;
            if (DEMNowDoing in [SeekingStreamProfile]) then with Image1.Canvas do begin
               Pen.Color := ConvertPlatformColorToTColor(MDDef.DigitizeColor);
               Pen.Width := 2;
               MoveTo(MapScreenX1,MapScreenY1);
               LineTo(Lastx,Lasty);
            end;
            if NowStreamDigitizing(DEMNowDoing) then begin
               Image1.Canvas.Pen.Color := ConvertPlatformColorToTColor(MDDef.DigitizeColor);
               Image1.Canvas.Pen.Width := MDDef.DigitizeWidth;
               Image1.Canvas.MoveTo(MapScreenX1,MapScreenY1);
               Image1.Canvas.LineTo(Lastx,Lasty);
            end;
         end;
         MakeLastPointFirst;
      end;
   {$EndIf}

   {$IfDef ExDrainage}
   {$Else}
      CheckDrainageDblClick;
   {$EndIf}

   {$IfDef Ex3D}
   {$Else}
      ThreeDCheckDblClick(NotSamePoint);
   {$EndIf}

   if (DEMNowDoing = PlottingOffset) then with MapDraw do begin
      CheckThisPoint(LastX,LastY,xDEMg1,yDEMg1,xSATg1,ySATg1,CheckReasonable);
      DEMGlb[DEMonMap].DEMGridToLatLongDegree(xDEMg1,yDEMg1,Lat,Long);
      for i := 1 to MDDef.NumOffsets do begin
         VincentyPointAtDistanceBearing(Lat,Long,MDDef.OffsetDistance,MDDef.OffsetBearings[i],Lat2,Long2);
         LatLongDegreeToScreen(Lat2,Long2,x,y);
         with Image1.Canvas do begin
            Pen.Color := ConvertPlatformColorToTColor(MDDef.OffsetColor);
            Pen.Width := MDDef.OffsetLineWidth;
            MoveTo(LastX,LastY);
            LineTo(x,y);
         end;
      end;
      ApplicationProcessMessages;
      if not AnswerIsYes('Another offset, same parameters') then BackToWandering;
      exit;
   end;

   if (DEMNowDoing = SecondDistancePoint) and NotSamePoint then with MapDraw do begin
      {$IfDef MeasureDistance} WriteLineToDebugFile('DEMNowDoing = SecondDistancePoint start'); {$EndIf}
      GetPositionData(Length,Heading,Tstr2,MDdef.CheckPoint = CheckNothing);
      CumLength := CumLength + Length;
      if (ScreenPixelSize < 500) and (MapDraw.PrimMapProj = Nil) then begin
         ScreenToUTM(MapScreenX1,MapScreenY1,xf,yf);
         ScreenToUTM(LastX,LastY,xe,ye);
         TStr := ' dx=' + SmartDistanceMetersFormat(xe - xf) + '   dy =' +  SmartDistanceMetersFormat(ye - yf) +  MessLineBreak;
      end
      else TStr := '';
      if (CumLength > Length + 0.0001) then MenuStr := 'Cumulative: ' + SmartDistanceMetersFormat(CumLength) + MessLineBreak + MessLineBreak
      else MenuStr := '';
      DistStr := SmartDistanceMetersFormat(Length);
      MenuStr := 'Distance:  ' + DistStr + MessLineBreak + TStr + 'Bearing:' + RealToString(Heading,8,1) + '°   ' + TStr2 + MessLineBreak + MessLineBreak + MenuStr;
      {$IfDef MeasureDistance} WriteLineToDebugFile('DEMNowDoing = SecondDistancePoint end ' + MenuStr); {$EndIf}
      if AnswerIsYes(MenuStr + 'Add another segment') then MakeLastPointFirst
      else BackToWandering;
   end;


   if (DEMNowDoing = LaterZigDistance) and NotSamePoint then with MapDraw do begin
      GetPositionData(Length,Heading,Tstr2,MDdef.CheckPoint = CheckNothing);
      CumLength := CumLength + Length;
      MakeLastPointFirst;
   end;


   if (DEMNowDoing = FirstDistancePoint) then begin
      {$IfDef MeasureDistance} WriteLineToDebugFile('DEMNowDoing = FirstDistancePoint start'); {$EndIf}
      MakeLastPointFirst;
      CheckThisPoint(MapScreenX1,MapScreenY1,xDEMg1,yDEMg1,xSATg1,ySATg1,CheckReasonable);
      ChangeDEMNowDoing(SecondDistancePoint);
      CumLength := 0;
      {$IfDef MeasureDistance} WriteLineToDebugFile('DEMNowDoing = FirstDistancePoint end'); {$EndIf}
   end;

   if (DEMNowDoing = FirstZigDistance) then begin
      MakeLastPointFirst;
      CheckThisPoint(MapScreenX1,MapScreenY1,xDEMg1,yDEMg1,xSATg1,ySATg1,CheckReasonable);
      ChangeDEMNowDoing(LaterZigDistance);
      CumLength := 0;
      {$IfDef MeasureDistance} WriteLineToDebugFile('DEMNowDoing = FirstDistancePoint end'); {$EndIf}
   end;


   if (DEMNowDoing = SecondBearingPoint) and NotSamePoint then begin
      GetPositionData(Length,Heading,Tstr2);
      {$IfDef ExGeology}
      {$Else}
      if (ThreePointer <> Nil) then  begin
         if (Heading > 90) and (Heading < 270) then Heading := Heading + 180;
         if (Heading > 360) then Heading := Heading - 360;
         ThreePointer.Edit1.Text := RealToString(Heading,-8,2);
         BackToWandering;
      end
      else {$EndIf}
      begin
         with Image1.Canvas do begin
            Pen.Mode := pmCopy;
            Pen.Color := clRed;
            MoveTo(MapScreenX1,MapScreenY1);
            LineTo(LastX,LastY);
            TextOut(LastX+5,LastY+5,TStr2);
            StrikeIntoString(Round(Heading+270),TStr,false);
            TextOut(MapScreenX1+5,MapScreenY1+5,TStr);
         end;
         if AnswerIsYes('Bearing (True):' + RealToString(Heading,8,1) + '°' + MessLineBreak + TStr2 + MessLineBreak + MessLineBreak + 'Another') then begin
             ChangeDEMNowDoing(FirstBearingPoint);
             exit;
         end
         else BackToWandering;
      end;
   end;

   if (DEMNowDoing = FirstBearingPoint) then  begin
      MakeLastPointFirst;
      CheckThisPoint(MapScreenX1,MapScreenY1,xDEMg1,yDEMg1,xSATg1,ySATg1,CheckReasonable);
      ChangeDEMNowDoing(SecondBearingPoint);
   end;

   if (DEMNowDoing = FirstSlopePoint) then begin
      MakeLastPointFirst;
      CheckThisPoint(MapScreenX1,MapScreenY1,xDEMg1,yDEMg1,xSATg1,ySATg1,CheckReasonable);
      ChangeDEMNowDoing(SecondSlopePoint);
      exit;
   end;

   if (DEMNowDoing = SecondSlopePoint) and NotSamePoint then with MapDraw do begin
      CheckThisPoint(LastX,LastY,xgrid,ygrid,xSATg1,ySATg1,CheckReasonable);
      DEMGlb[DEMonMap].DEMGridtoUTM(xDEMg1,yDEMg1,xutm1,yutm1);
      DEMGlb[DEMonMap].DEMGridtoUTM(xgrid,ygrid,xutm2,yutm2);
      Length := sqrt(sqr(XUTM2 - XUTM1) + sqr(YUTM2 - YUTM1));
      DEMGlb[DEMonMap].GetElevMeters(xDEMg1,yDEMg1,elev);
      DEMGlb[DEMonMap].GetElevMeters(xgrid,ygrid,elev2);
      Slope := 100 * abs(elev - elev2) / Length;
      with Image1.Canvas do begin
         Pen.Mode := pmNotXor;
         Pen.Color := clBlack;
         Pen.Width := 3;
         MoveTo(MapScreenX1,MapScreenY1);
         LineTo(LastX,LastY);
      end;
      if AnswerIsYes('Average Slope: '+ RealToString(Slope,-8,2) + '%  or ' + RealToString(ArcTan(0.01* Slope)/DegToRad,-6,1) + '°  over: '+ RealToString((0.001*Length),-8,2)+' km' + MessLineBreak + 'Another') then begin
         ChangeDEMNowDoing(FirstSlopePoint);
      end
      else BackToWandering;
   end;

   {$IfDef ExGeology}
   {$Else}
      if DEMNowDoing in [SeekingLeftSideMagModels] then begin
         CheckThisPoint(LastX,LastY,xDEMg2,yDEMg2,xsatg2,ysatg2,CheckReasonable);
         ChangeDEMNowDoing(SeekingRightSideMagModels);
         MakeLastPointFirst;
      end;
   {$EndIf}

   {$IfDef ExTopoGrain}
   {$Else}
      if (DEMNowDoing = GrainByRegionSize) then begin
         CheckThisPoint(LastX,LastY,xDEMg1,yDEMg1,xsatg1,ysatg1,CheckNothing);
         FigureGrainByRegionSize(round(xDEMg1),Round(yDEMg1));
      end;

      if (DEMNowDoing = GetPointFabric) then begin
         CheckThisPoint(LastX,LastY,xDEMg1,yDEMg1,xSATg1,ySATg1,CheckAll);
         GetFabricAtPoint(round(xDEMg1),round(yDEMg1));
      end;
   {$EndIf}

   LastClickX := LastX;
   LastClickY := LastY;
end;


procedure TMapForm.MakeLastPointFirst;
begin
   MapScreenX1 := Lastx;
   MapScreenY1 := LastY;
   xDEMg1 := xDEMg2;
   yDEMg1 := yDEMg2;
   xSATg1 := xSATg2;
   ySATg1 := ySATg2;
   xutm1 := xutm2;
   yutm1 := yutm2;
end;


procedure TMapForm.HideToolbar;
begin
   Panel1.Visible := false;
   Panel1.Height := 0;
   FormResize(Nil);
end;


procedure TMapForm.Open1Click(Sender: TObject);
begin
   {$IfDef Nevadia}
   {$Else}
      wmdem.Open1Click(nil);
   {$EndIf}
end;


procedure TMapForm.Openbandforrasteranalysis1Click(Sender: TObject);
var
   WantedBand,DEM : integer;
begin
   SatImage[MapDraw.SatOnMap].PickBand('Band for raster analysis', WantedBand);
   DEM := OpenNewDEM(SatImage[MapDraw.SatOnMap].BFileName[WantedBand]);
   DEMGlb[DEM].DEMHeader.ElevUnits := euImagery;
end;


function TMapForm.OpenDBonMap(WhatFor : shortstring; DefaultFile : PathStr; DisplayNow : boolean = true; OpenTable : boolean = true; ThisMapOnly : boolean = false;
    ForceColor : tColor = -99; ForceLineWidth : byte = 0; HideFields : ShortString = '') : integer;
var
   FileNames : tStringList;
   i : integer;

       function OpenSingleDataBase : integer;
       var
          i : integer;
       begin
          {$IfDef RecordGISDB} WriteLineToDebugFile('Enter OpenSingleDataBase ' + DefaultFile); {$EndIf}
          if OpenNumberedGISDataBase(Result,DefaultFile,OpenTable,false,self) then begin
             if (ForceColor > -99) then begin
                GISdb[Result].dbOpts.LineColor := ConvertTColorToPlatformColor(ForceColor);
             end;
             if (ForceLineWidth > 0) then begin
                GISdb[Result].dbOpts.LineWidth := ForceLineWidth;
             end;
             if (HideFields <> '') then with GISdb[Result] do begin
                for I := 0 to pred(MyData.FieldCount) do begin
                   dbOpts.VisCols[i] := MyData.FieldsInDataBase[i] = HideFields;
                end;
             end;
             if OpenTable then GISdb[Result].dbTablef.HideColumns;
             if GISdb[Result].CanPlot then begin
                AddOverlay(Self,ovoDatabases);
                if DisplayNow then DoFastMapRedraw;
             end;
          end;
          {$IfDef RecordGISDB} WriteLineToDebugFile('Exit OpenSingleDataBase ' + DefaultFile); {$EndIf}
       end;

begin
   {$IfDef RecordGISDB} WriteLineToDebugFile('TMapForm.OpenDBonMap in ' + DefaultFile + '  Map owner=' + Self.Caption); {$EndIf}
   if (DefaultFile <> '') and (FileExists(DefaultFile) or FileExists(ChangeFileExt(DefaultFile,'.dbf'))) then begin
      Result := OpenSingleDataBase;
   end
   else begin  //no index file found or default file selected
      FileNames := tStringList.Create;
      if (DefaultFile = '') then FileNames.Add(LastDataBase)
      else FileNames.Add(DefaultFile);
      if not GetMultipleFiles('Data bases ' + WhatFor,DBMaskString ,FileNames,MDDef.DefDBFilter) then Exit;
      try
         for i := 0 to pred(FileNames.Count) do begin
            DefaultFile := FileNames.Strings[i];
            wmdem.SetPanelText(1,IntToStr(succ(i)) + '/' + IntToStr(FileNames.Count) + '  ' + DefaultFile);
            ShlObj.SHAddToRecentDocs(SHARD_PATH, PChar(FileNames.Strings[i]));
            LastDataBase := DefaultFile;
            Result := OpenSingleDataBase;
         end;
      finally
         wmdem.SetPanelText(1,'');
      end;
      FileNames.Free;
      DefaultFile := '';
   end;
   if (Result <> 0) and GISdb[Result].CanPlot then begin
      CheckProperTix;
      CheckThatLegendsAreOnTop;
      if (MapTOCIndex <> 0) then MapTOC[MapTOCIndex].LabelTheButtons;
   end;
   {$IfDef RecordGISDB} WriteLineToDebugFile('TMapForm.OpenADataBase out'); {$EndIf}
end;


procedure TMapForm.OpenGLdrapeonanotherDEM1Click(Sender: TObject);
begin
   if GetSecondDEM(false) then begin
       OGL_speedbuttonClick(Sender);
   end;
end;

procedure TMapForm.OpenGLwithallmaps1Click(Sender: TObject);
var
   Viewer : TView3DForm;
   Maps : tStringList;
   i,j,Num : integer;
begin
   PickMaps(Maps,'Maps for 3D view (max ' + IntToStr(MaxClouds) + ')');
   if (Maps.Count > 0) then begin
      MatchThiscoverageareaandsamepixelsize1Click(Sender);
      Viewer := MapTo3DView(Self.MapDraw);
      Num := 1;
      for i := 0 to pred(WMDEM.MDIChildCount) do begin
         if WMDEM.MDIChildren[i] is tMapForm and (WmDEM.MDIChildren[i].Handle <> Handle) then begin
            for j := 0 to pred(Maps.Count) do begin
               if (Num < MaxClouds) and (Maps.Strings[j] = (WMDEM.MDIChildren[i] as TMapForm).Caption) then begin
                  inc(Num);
                  Viewer.DoMap((WMDEM.MDIChildren[i] as TMapForm).MapDraw);
               end;
            end;
         end;
      end;
   end;
   Maps.Free;
end;


{$IfDef ExMultiGrid}
{$Else}
      procedure TMapForm.SetMultibandToShowOnMap(Band : integer);
      begin
         {$IfDef RecordMultiGrids} WriteLineToDebugFile('TMapForm.SetMultibandToShowOnMap in, Band=' + IntToStr(Band) + '  ' + MapDraw.MapSizeString); {$EndIf}
          if ValidDEM(MultiGridArray[MapDraw.MultiGridOnMap].Grids[Band]) then begin
             MapDraw.DEMonMap := MultiGridArray[MapDraw.MultiGridOnMap].Grids[Band];
             MultiGridArray[MapDraw.MultiGridOnMap].IndexBand := Band;
             MapDraw.ScaleMapElevationsToDEM;
             MapDraw.BaseTitle := MultiGridArray[MapDraw.MultiGridOnMap].MG_short_name + ' ' + DEMGlb[MapDraw.DEMonMap].AreaName;
             MapDraw.MapType := mtElevGray;
             DoBaseMapRedraw;
          end;
         {$IfDef RecordMultiGrids} WriteLineToDebugFile('TMapForm.SetMultibandToShowOnMap out, ' + MapDraw.MapSizeString + '  title=' + MapDraw.BaseTitle); {$EndIf}
      end;


      procedure TMapForm.SetRGBMultibandToShowOnMap(What : shortstring; RedBand,GreenBand,BlueBand : integer);
      var
         x,y : integer;
         xgrid,ygrid : float32;
         finalBmp : tMyBitmap;
         finalmem : tBmpMemory;
         bmin,bmax,z : array[1..3] of float32;

            procedure GetRange(band,ThisDEM : integer);
            begin
               bmin[Band] := DEMGlb[ThisDEM].FindPercentileElevation(MDDef.MinImagePercentile);
               bmax[Band] := DEMGlb[ThisDEM].FindPercentileElevation(MDDef.MaxImagePercentile);
            end;

            function GetShade(Band : integer) : integer;
            begin
               if z[Band] > bMax[Band] then Result := 254
               else if z[Band] <  bMin[Band] then Result := 0
               else Result := round(254 * (z[Band] - bmin[Band]) / (bmax[Band] - bmin[Band]));
            end;

      begin
         {$IfDef RecordMultiGrids} WriteLineToDebugFile('TMapForm.SetRGBMultibandToShowOnMap in, title=' + MapDraw.BaseTitle); {$EndIf}
         MDDef.GridLegendLocation.DrawItem := false;
         MapDraw.DeleteSingleMapLayer(MapDraw.LegendOverlayfName);

         GetRange(1,RedBand);
         GetRange(2,GreenBand);
         GetRange(3,BlueBand);
         CloneImageToBitmap(Image1,finalBMP);
         FinalMem := tBmpMemory.Create(finalbmp);

         for y := 0 to pred(finalbmp.Height) do begin
            for x := 0 to pred(finalbmp.Width) do begin
               MapDraw.ScreenToDataGrid(x,y,xgrid,ygrid);
               if DEMGlb[RedBand].GetElevMeters(xgrid,ygrid,z[1]) and DEMGlb[GreenBand].GetElevMeters(xgrid,ygrid,z[2]) and DEMGlb[BlueBand].GetElevMeters(xgrid,ygrid,z[3]) then begin
                  finalMem.SetRedChannel(x,y,GetShade(1));
                  finalMem.SetGreenChannel(x,y,GetShade(2));
                  finalMem.SetBlueChannel(x,y,GetShade(3));
               end;
            end;
         end;
         FinalMem.Destroy;
         FinalBMP.SaveToFile(MapDraw.BaseMapFName);
         FinalBmp.Destroy;
         {$IfDef RecordMultiGrids} WriteLineToDebugFile('TMapForm.SetRGBMultibandToShowOnMap redraw now, title=' + MapDraw.BaseTitle); {$EndIf}

         DoFastMapRedraw;
         Self.BringToFront;
         {$IfDef RecordMultiGrids} WriteLineToDebugFile('TMapForm.SetRGBMultibandToShowOnMap out, title=' + MapDraw.BaseTitle); {$EndIf}
      end;
{$EndIf}


procedure TMapForm.Opengrid1Click(Sender: TObject);
{$IfDef ExMultiGrid}
begin
{$Else}
const
   Band : integer = -99;
var
   TStr : shortstring;
begin
    if (Band < 0) then Band := MapDraw.SATView.BandInWindow;
    if MultiGridArray[MapDraw.MultiGridOnMap].MonthlyData then TStr := 'Month' else TStr := 'Band';
    ReadDefault(TStr,Band);
    SetMultibandToShowOnMap(Band);
{$EndIf}
end;

procedure TMapForm.Opengrid2Click(Sender: TObject);
var
   MergeDEM : integer;
begin
   if GetDEM(MergeDEM,true,'to replace all holes in ' + DEMGlb[MapDraw.DEMonMap].AreaName) then begin
      DEMGlb[MapDraw.DEMonMap].FillHolesSelectedBoxFromReferenceDEM(DEMGlb[MapDraw.DEMonMap].FullDEMGridLimits,MergeDEM,hfOnlyHole);
      RespondToChangedDEM;
   end;
end;

procedure TMapForm.SetUpTopForm;
begin
   Self.Top := 0;
   Self.Left := 0;
   CopyImageToBitmap(Image1,MapBaseBMP);
   BlendPanel.Height := 45;
   TrackBar2.Position := MDDef.MapOverlayOpacity;
   SetClientHeight;
end;


procedure TMapForm.DrawColoredMap1Click(Sender: TObject);
begin
   DoFastMapRedraw;
end;


{$IfDef RecordMapResize}
      procedure TMapForm.DebugMapSize;
      var
        Results : tStringList;
        x1,y1,x2,y2 : float64;
        Decs : integer;
      begin
         WriteLineToDebugFile('');
         WriteLineToDebugFile(Caption);
         x1 := MapDraw.MapCorners.BoundBoxUTM.xmax - MapDraw.MapCorners.BoundBoxUTM.xmin;
         y1 := MapDraw.MapCorners.BoundBoxUTM.ymax - MapDraw.MapCorners.BoundBoxUTM.ymin;
         WriteLineToDebugFile('UTM Size:  ' + RealToString(x1,-12,-2) + ' x ' + RealToString(x1,-12,-2) + ' m');
         WriteLineToDebugFile('Pixel Size:  ' + RealToString(x1/MapDraw.MapXSize,-12,-2) + ' x ' + RealToString(y1/MapDraw.MapySize,-12,-2) + ' m');
         WriteLineToDebugFile('');
      end;
{$EndIf}


procedure TMapForm.Verifyelevationrange1Click(Sender: TObject);
begin
   DEMGlb[MapDraw.DEMonMap].CheckMaxMinElev;
   RespondToChangedDEM;
end;


procedure TMapForm.VerifyPointOnMap(WhatFor : string; var xg,yg : float32; var xutm,yutm : float64);
var
   lat,Long : float64;
begin
   //with MapDraw do begin
      if (WhatFor = '') then WhatFor := WMDEM.StatusBar1.Panels[0].Text;
      if (MapDraw.VectorIndex <> 0) then begin
      end
      else if MapDraw.DEMMap then DEMGlb[MapDraw.DEMonMap].VerifyDefaultPosition(WhatFor,xg,yg,xutm,yutm)
      {$IfDef ExSat}
              ;
      {$Else}
      else begin
          SatImage[MapDraw.SatOnMap].SatGridToLatLongDegree(SatImage[MapDraw.SatOnMap].BandForSize,xg,yg,Lat,Long);
          GetLatLongDefault(SatImage[MapDraw.SatOnMap].ImageMapProjection,WhatFor,Lat,Long);
          SatImage[MapDraw.SatOnMap].LatLongDegreeToSatGrid(SatImage[MapDraw.SatOnMap].BandForSize,Lat,Long,xg,yg);
          SatImage[MapDraw.SatOnMap].SatGridToUTM(SatImage[MapDraw.SatOnMap].BandForSize,xg,yg,xutm,yutm);
      end;
      {$EndIf}
   //end;
end;


procedure TMapForm.Verticalswipecompare1Click(Sender: TObject);
{$IfDef ExAdvanced3D}
begin
{$Else}
var
   Map2 : tMapForm;
   n1,n2 : integer;
   fName1,fName2 : PathStr;
begin
   n2 := PickMap('right map');
   Map2 := WMDEM.MDIChildren[n2] as tMapForm;

   if (Self.MapDraw.BasicProjection <> Map2.MapDraw.BasicProjection) then begin
      MessageToContinue(IncMapTypes);
      exit;
   end;

   Self.MatchMapToThisOne(Map2);
   fName1 := MDtempDir + WMDEM.MDIChildren[n1].Caption + '.bmp';
   SaveImageAsBMP(Self.Image1,fName1);
   fName2 := MDtempDir + WMDEM.MDIChildren[n2].Caption + '.bmp';
   SaveImageAsBMP(Map2.Image1,fName2);
   ImageSplitHorizontal(fName1,fName2);
{$EndIf}
end;

procedure TMapForm.JSON1Click(Sender: TObject);
begin
   DEMGlb[MapDraw.DEMonMap].SaveAsGeoJSON;
end;


procedure TMapForm.SetMapOverlays;
begin
   if MDDef.DBsOnAllMaps and (NumOpenDB > 0) then begin
      AddOverlay(Self,ovoDataBases);
   end;

   {$IfDef ExTiger}
   {$Else}
      if (not MapDraw.aDRGmap) and (not (MapDraw.MapOwner in [moIndexMap,moMapDatabase, moEditMap,moPointVerificationMap])) then begin
          if (MDdef.TigrDef.AutoTigerOnDEMs and MapDraw.DEMMap) or
             (MDdef.TigrDef.AutoTigerOnImages and ValidSatImage(MapDraw.SatOnMap)) then begin
             AddOverlay(Self,ovoTiger);
          end;
      end;
   {$EndIf}

   {$IfDef ExPLSS}
   {$Else}
      if (not MapDraw.aDRGmap) and (MapDraw.MapOwner in [moDEMSelectionMap,moNone,moDrapeMap]) and (MDDef.PLSSDef.AutoDrawPLSS) and (MDDef.ShowPLSS) then begin
         if PLSSOpen or TryToOpenPLSS(Self) then AddOverlay(Self,ovoPLSS);
      end;
   {$EndIf}
end;


procedure TMapForm.DEMGridToImageGrid(xg,yg : float32; var xg1,yg1 : float32);
var
   Lat,Long : float64;
begin
   if MapDraw.DEMMap then begin
      xg1 := xg;
      yg1 := yg;
   end;
   {$IfDef ExSat}
   {$Else}
      if MapDraw.ValidDEMonMap then begin
         DEMGlb[MapDraw.DEMonMap].DEMGridToLatLongDegree(xg,yg,Lat,Long);
         SatImage[MapDraw.SatOnMap].LatLongDegreetoSatGrid(SatImage[MapDraw.SatOnMap].BandForSize,Lat,Long,xg1,yg1);
      end;
   {$EndIf}
end;


procedure TMapForm.DEMIX10Ktile1Click(Sender: TObject);
begin
   LoadDEMIXtileOutlines(PointBoundBoxGeo(RightClickLat,RightClickLong),false,true);
end;


procedure TMapForm.DEMIX10ktiles1Click(Sender: TObject);
begin
   if ValidDEM(MapDraw.DEMonMap) then DEMIXtileFill(MapDraw.DEMonMap, MapDraw.MapCorners.BoundBoxGeo)
   else LoadDEMIXtileOutlines(MapDraw.MapCorners.BoundBoxGeo);
end;

procedure TMapForm.DEMIX1secresamplebyaveraging1Click(Sender: TObject);
begin
   {$If Defined(RecordCreateGeomorphMaps) or Defined(RecordDEMIX)} WriteLineToDebugFile('TMapForm.DEMIX1secresamplebyaveraging1Click in, ' + DEMGlb[MapDraw.DEMonMap].DEMFileName); {$EndIf}
   ResampleForDEMIXOneSecDEMs(false,MapDraw.DEMonMap,true);
   {$If Defined(RecordCreateGeomorphMaps) or Defined(RecordDEMIX)} WriteLineToDebugFile('TMapForm.DEMIX1secresamplebyaveraging1Click grids out'); {$EndIf}
end;


procedure TMapForm.DEMIX1secresamplewithGDAL1Click(Sender: TObject);
begin
    GDAL_downsample_DEM_1sec(MapDraw.DEMonMap,MDTempDir + 'gdal_downsample.dem');
end;


procedure TMapForm.DEMIXhalfsecto2onesec1Click(Sender: TObject);
var
  dem1,dem2 : integer;
begin
   DEM1 := DEMGlb[MapDraw.DEMonMap].ThinThisDEM('1sec_pixel-is-point',2,true,0);
   DEM2 := DEMGlb[MapDraw.DEMonMap].ThinThisDEM('1sec_pixel-is-area',2,true,1);
end;


procedure TMapForm.DEMIXrangescales1Click(Sender: TObject);
begin
   ResampleForDEMIXOneSecDEMs(false,MapDraw.DEMonMap,false,MDTempDir,ResampleModeRange);
end;

procedure TMapForm.PlotGridPoint(xgrid,ygrid : float64; PlotColor : tPlatformColor);
var
   xpic,ypic : integer;
begin
   if MapDraw.DEMMap then with DemGLB[MapDraw.DEMonMap] do begin
      MapDraw.DataGridToScreen(XGrid,YGrid,XPic,YPic);
      ScreenSymbol(Image1.Canvas,xpic,ypic,Splat,3,Plotcolor);
   end;
end;


procedure TMapForm.MapDisplayLocation(var x,y : integer; var Lat,Long : float64; var Elev : float32);
var
   ClimateData : tClimateData;
   XGrid,YGrid,veg_ht : float32;
   sl,len : float64;
   SlopeStr,CapStr : ShortString;
   SlopeAsp : tSlopeAspectRec;
   Panel1String,Panel2String,Panel3String,TStr : ShortString;

         function ElevationString(DEM : integer; Elev : float64) : ShortString;
         var
            Dec : integer;
         begin
             if (MapDraw.ValidDEMonMap) and (not DEMGlb[DEM].MissingElevation(Elev)) then begin

                if (DEMGlb[DEM].VatLegendStrings <> Nil) then begin
                   Result := ' ';  // +
                   Panel3String := DEMGlb[DEM].VatLegendStrings.Strings[round(Elev)];
                end
                else case DEMGlb[DEM].DEMheader.ElevUnits of
                   Undefined,euImagery  : Result := ' z=' + RealToString(Elev,6,2);
                   euMM : Result := ' z=' + RealToString(Elev,6,2) + ' mm';
                   HundredthMa  : Result := ' z=' + RealToString(Elev,6,2) + ' Ma';
                   PercentSlope : Result := ' z=' + RealToString(Elev,6,1) + '%';
                   euPercent : Result := ' z=' + RealToString(Elev,6,2) + '%';
                   zDegrees : Result := ' z=' + RealToString(Elev,6,2) + '°';
                   lnElev : Result := 'ln z=' + RealToString(Elev,5,3) + '  z=' + RealToString(Math.Power(e,Elev),-18,-2);
                   LogElev : Result := 'log z=' + RealToString(Elev,5,3) + '  z=' + RealToString(Math.Power(10,Elev),-18,-2);
                   else begin
                      if (DEMGlb[DEM].AverageSpace > 250) then Dec := 0
                      else if (DEMGlb[DEM].AverageSpace < 7) then Dec := 2 else Dec := 1;
                      if MDdef.ElevDisplayMeters then begin
                         Result := ' z=' + RealToString(Elev,-8,Dec) + ' m';
                         if MDdef.DualElevs then Result := Result + '  (' + RealToString(Elev/Petmar_types.FeetToMeters,-8,Dec) + ' ft)';
                      end
                      else begin
                         Result := ' z=' + RealToString(Elev / FeetToMeters,-8,Dec) + ' ft';
                         if MDdef.DualElevs then Result := Result + '  (' + RealToString(Elev,-8,Dec) + ' m)';
                      end;
                   end;
                end;
             end
             else Result := '';
         end;

begin
   if HeavyDutyProcessing or ClosingIsHappening or LockStatusBar or MapDraw.ClosingMapNow or (WMDEM = Nil) or (MapDraw = Nil) or (Not MapDraw.MapDrawValid) or (DEMNowDoing in [Calculating]) then exit;

   MapDraw.ScreenToLatLongDegree(x,Y,Lat,Long);
   if IsNAN(Long) or (abs(Long) > 360) or (abs(Lat) > 90) then exit;
   {$IfDef MapDisplayLocation} WriteLineToDebugFile('MapDisplayLocation 1'); {$EndIf}
   Panel2String := '';
   Panel3String := '';
   LastRoamLat := Lat;
   LastRoamLong := Long;

   if MDDef.ShowProjectedCoordinates then begin
      Panel3String := MapDraw.ProjectedCoordinatesStringFromLatLong(Lat,Long);
      {$IfDef MapDisplayLocation} WriteLineToDebugFile('MapDisplayLocation 1.2 ' + Panel3String); {$EndIf}
   end;

   Panel1String := MapDraw.PrimMapProj.PreferLocationString(Lat,Long) +  ' ' +  MapDraw.PrimMapProj.h_DatumCode;
   {$IfDef ExSat}
   {$Else}
      if MapDraw.ValidSatOnMap and MDDef.SatImageCoords then begin
         MapDraw.ScreenToDataGrid(x,y,xgrid,ygrid);
         TStr := '   ref=' + IntToStr(SatImage[MapDraw.SATonMap].GetSatPointValue(MapDraw.SatView.BandInWindow,round(XGrid),round(YGrid)));
         Panel3String := 'Image x=' + RealToString(xgrid,-12,2) + '  & y=' +  RealToString(ygrid,-12,2) + TStr + ' ' + Panel3String;
      end;
   {$EndIf}

   if (MapDraw.DEM2onMap <> 0) and (DEMGlb[MapDraw.DEM2onMap] <> Nil) then begin
      MapDraw.ScreenToDataGrid(x,y,xgrid,ygrid);
      if DEMGlb[MapDraw.DEM2onMap].GetElevMeters(XGrid,YGrid,Elev) then WmDEM.SetPanelText(3, DEMGlb[MapDraw.DEM2onMap].AreaName + ElevationString(MapDraw.DEM2onMap,Elev));
   end;


   if (MDdef.MGRSandLatLongWhileRoam) then begin
      if MDdef.CoordUse = coordLatLong then TStr := 'MGRS: ' + WGS84DatumConstants.LatLongToMGRS(Lat,Long)
      else TStr := LatLongDegreeToString(Lat,Long,MDdef.OutPutLatLongMethod);
      Panel3String := TStr +  '   ' +   MapDraw.PrimMapProj.h_DatumCode + ' ' + Panel3String;
   end;

   {$IfDef RecordAllMapRoam} WriteLineToDebugFile('MapDisplayLocation 2'); {$EndIf}

   if (DEMNowDoing in [SecondBearingPoint,SecondDistancePoint,SeekingSecondPerspective,SeekingSecondLOS,SimpleTopoProfileRight,SeekingSecondAverageProfile]) and ((x <> MapScreenX1) or (y <> MapScreenY1)) then begin
      GetPositionData(Len,sl,CapStr,true);
      Panel3String := RealToString(sl,8,1) + '°  ' + SmartDistanceMetersFormat(Len);
   end;

   if ValidDEM(MapDraw.DEMonMap) and (not LockStatusBar) then begin
      {$IfDef RecordAllMapRoam} WriteLineToDebugFile('Point 2.1'); {$EndIf}

      if MDDef.ShowDEMGridCoords and (Panel3String = '') then begin
         DEMGlb[MapDraw.DEMonMap].LatLongDegreeToDEMGrid(Lat,Long,XGrid,YGrid);
         Panel3String := DEMGridString(xgrid,ygrid) + ' ' + Panel3String;
      end;

      {$IfDef RecordAllMapRoam} WriteLineToDebugFile('Point 2.11'); {$EndIf}

      if DEMGlb[MapDraw.DEMonMap].GetElevFromLatLongDegree(Lat,Long,Elev) then begin
         {$IfDef RecordAllMapRoam} WriteLineToDebugFile('Point 2.111'); {$EndIf}
         CapStr := '';
         if (isSlopeMap(MapDraw.Maptype) or (MapDraw.MapType in [mtDEMaspect])) or (DEMNowDoing in [FirstSlopePoint,SecondSlopePoint]) then begin
            DEMGlb[MapDraw.DEMonMap].LatLongDegreeToDEMGrid(Lat,Long,XGrid,YGrid);
            if DEMGlb[MapDraw.DEMonMap].GetSlopeAndAspect(round(xgrid),round(ygrid),SlopeAsp) then begin
               if (MapDraw.MapType = mtDEMaspect) then Panel2String := '  downhill' + RealToString(SlopeAsp.AspectDir,4,0) + '°'
               else Panel2String := RealToString(SlopeAsp.SlopePercent,8,0) + '% slope' + ' (' + RealToString(SlopeAsp.SlopeDegree,-4,-1) + '°)  ';
            end;
         end;

         {$IfDef RecordAllMapRoam} WriteLineToDebugFile('Point 2.113');{$EndIf}

         if (MapDraw.MapOwner = moPointVerificationMap) then CapStr := ElevationString(MapDraw.DEMonMap,Elev)
         else CapStr := CapStr + '  ' + ElevationString(MapDraw.DEMonMap,Elev);
         if DEMNowDoing in [FirstSlopePoint,SecondSlopePoint] then Panel3String := SlopeStr;
         Panel1String := Panel1String + CapStr;

         {$IfDef RecordAllMapRoam} WriteLineToDebugFile('Point 2.115'); {$EndIf}

         {$IfDef ExSat}
         {$Else}
            if (DEMGlb[MapDraw.DEMonMap].LandCoverGrid) and (DEMGlb[MapDraw.DEMonMap].NLCDCats <> Nil) then begin
               if DEMGlb[MapDraw.DEMonMap].GetElevMeters(round(XGrid),round(YGrid),Elev) and (Elev > 0) and (Elev < MaxLandCoverCategories) then
                  Panel2String := DEMGlb[MapDraw.DEMonMap].NLCDCats^[round(Elev)].LongName + '  z=' + RealToString( DEMGlb[MapDraw.DEMonMap].NLCDCats^[round(Elev)].Height,-7,-1);
            end;
         {$EndIf}
      end;

      {$IfDef ExVegDensity}
      {$Else}
         if (DEMGlb[MapDraw.DEMonMap].VegGrid[1] > 0) and (DEMGlb[DEMGlb[MapDraw.DEMonMap].VegGrid[1]] <> Nil) then begin
            DEMGlb[DEMGlb[MapDraw.DEMonMap].VegGrid[1]].GetJustVegHeight(xgrid,ygrid,veg_ht);
            Panel3String := 'Veg=' + RealToString(veg_ht,-8,1) + ' m';
         end;
      {$EndIf}
      {$IfDef RecordAllMapRoam} WriteLineToDebugFile('Point 2.3'); {$EndIf}
   end;

      if ValidMultiGrid(TempMG) and ValidMultiGrid(PrecipMG) then begin
         ClimateData.Lat := Lat;
         ClimateData.Long := Long;
         LoadClimateData(ClimateData);
         if ClassifyClimate(ClimateData) then Panel2String := 'Koppen: ' + ClimateData.L1 + ClimateData.L2 + ClimateData.L3
         else Panel2String := '';
      end;

   WmDEM.SetPanelText(1, Panel1String);
   WmDEM.SetPanelText(2, Panel2String);
   WmDEM.SetPanelText(3, Panel3String);
   {$IfDef RecordAllMapRoam} WriteLineToDebugFile('MapDisplayLocation 2.5'); {$EndIf}

   {$IfDef ExVegDensity}
   {$Else}
      if (VegGraph <> Nil) and (DEMGlb[MapDraw.DEMonMap].VegDensityLayers[1] <> Nil) then begin
         DEMGlb[MapDraw.DEMonMap].VegDensityLayers[1].ShowGraphAtPoint(VegGraph,Lat,Long,xgrid,ygrid);
      end;
   {$EndIf}

   if (MapDraw <> Nil) and  (MapDraw.MapDrawValid) then begin
      if (MDdef.ShowRoamOnAllMaps and (DEMNowDoing = JustWandering)) or (DEMNowDoing = RoamBroadcast) or (MapDraw.MapOwner = moPointVerificationMap) then begin
         {$IfDef RecordAllMapRoam} WriteLineToDebugFile('MapDisplayLocation 2.6'); {$EndIf}
         BroadcastLatLong(Self.Handle,Lat,Long);
         {$IfDef RecordAllMapRoam} WriteLineToDebugFile('MapDisplayLocation 2.7'); {$EndIf}
      end;
   end;
   {$IfDef MapDisplayLocation} WriteLineToDebugFile('MapDisplayLocation out'); {$EndIf}
end {proc MapDisplayLocation};



procedure TMapForm.Image1MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
var
   TStr : ShortString;
   dx,i,j,x1,y1,x2,y2,Rad     : integer;
   NewBMP      : tMyBitmap;
   Prime       : boolean;
   FullDistance,Bearing,FullDistance2,
   Lat2,Long2,Lat3,Long3,
   BearingAngle,Distance, Sum,
   Maph,Mapk,Omega,DEC,DIP,TI,GV,lat,long : float64;
   xgrid,ygrid,ZElev,z1 : float32;
   GeoName : ShortString;
   Count   : LongInt;


      function DrawLineMode : boolean;
      begin
         Result := DEMNowDoing in [SeekingSecondLOS,ShapeLine,ShapePolygon,LaterZigDistance,SeekingSecondAverageProfile,NewTrack,
              {$IfDef ExGeology} {$Else} SeekingRightSideMagModels,GetStratcolColumn, {$EndIf}
              SecondBearingPoint,MultipleTopoProfileRight,SimpleTopoProfileRight,SecondPointSelectionAlgorithm,SecondDistancePoint,SecondSlopePoint,RouteObservation,SubsetByOutline,SecondTimeSeries,
              SeekingFlyThroughRoute,ShapePointsAlongSecondLine,OutlineDBIrregularMask,SeekingStreamProfile,SubsetHole,SubsetLake,ReplaceValuesByOutline,FillHolesByOutline];
      end;

      procedure ShowBoxSize;
      begin
         MapDraw.ScreenToLatLongDegree(Newx1,newy2,Lat2,Long2); //SW corner
         MapDraw.ScreenToLatLongDegree(Newx2,newy1,Lat3,Long3); //NE corner
         VincentyCalculateDistanceBearing(Lat,Long,Lat2,Long2,FullDistance,Bearing);
         VincentyCalculateDistanceBearing(Lat,Long,Lat3,Long3,FullDistance2,Bearing);
         wmDEM.SetPanelText(2, SmartDistanceMetersFormat(FullDistance) + ' x ' + SmartDistanceMetersFormat(FullDistance2) + '  (' + IntToStr(succ(NewX2-NewX1)) + 'x' + IntToStr(succ(NewY2-NewY1)) + ' pixels)');
      end;


begin
    if DEMMergeInProgress or OpeningNewGrid or HeavyDutyProcessing or DEMIXProcessing or LoadingFromMapLibrary or ClosingIsHappening or ClosingEverything or (wmDEM = Nil) or (MapDraw = Nil) or WMDEM.ProgramClosing or MapDraw.ClosingMapNow then begin
       {$IfDef RecordStreamModeDigitize} WriteLineToDebugFile('TMapForm.Image1MouseMove out fast 1'); {$EndIf}
       exit;
    end;

    if (DEMNowDoing in [Calculating]) or (not MapDraw.MapDrawValid) or RespondingToMouseMove then begin
       {$IfDef RecordStreamModeDigitize} WriteLineToDebugFile('TMapForm.Image1MouseMove out fast 2'); {$EndIf}
       exit;
    end;

   {$IfDef RecordMapRoam} WriteLineToDebugFile('enter TMapDraw.Image1MouseMove ' + Caption); {$EndIf}

   RespondingToMouseMove := true;

   if (LastBroadcastX > -1) and (LastBroadcastY > -1) then begin
       CrossWithHole(Image1.Canvas,LastBroadcastX,LastBroadcastY);
       LastBroadcastX := -1;
       LastBroadcastY := -1;
   end;

   MapDisplayLocation(x,y,Lat,Long,z1);
   {$IfDef RecordMapRoam} WriteLineToDebugFile('TMapDraw.Image1MouseMove did display location'); {$EndIf}

   {$IfDef ExGeoStats}
   {$Else}
      if (SupClassAuxGrids <> Nil) then SupClassAuxGrids.GridValuesAtPoint(Lat,Long);
   {$EndIf}

   if MouseIsDown and (DEMNowDoing = JustWandering) then begin
      ScrollBox1.HorzScrollBar.Position := ScrollBox1.HorzScrollBar.Position + SX - X;
      ScrollBox1.VertScrollBar.Position := ScrollBox1.VertScrollBar.Position + SY - Y;
      RespondingToMouseMove := false;
      exit;
   end;

   {$IfDef RecordMapRoam} WriteLineToDebugFile('Image1MouseMove done scrollbox move'); {$EndIf}

   if (DEMNowDoing in [SeekingSecondNewPanorama]) then begin
      SetRedrawMode(Image1);
      Rad := round(sqrt(sqr(newx1-x) + sqr(newy1-y)));
      if (OldRad > -90) then Image1.Canvas.Ellipse(newx1-oldrad,newy1-oldrad,newx1+oldrad,newy1+oldrad);
      OldRad := rad;
      Image1.Canvas.Ellipse(newx1-rad,newy1-rad,newx1+rad,newy1+rad);
   end;

   {$IfDef ExDigitize}
   {$Else}
      if MouseIsDown and NowStreamDigitizing(DEMNowDoing) then begin
         if (abs(x - Newx1) < MDDef.ContDigitizeSeparation) and (abs(Y - NewY1) < MDDef.ContDigitizeSeparation) then begin
            {$IfDef RecordStreamModeDigitize} WriteLineToDebugFile('not far enough, TMapForm.Image1MouseMove from ' + IntToStr(NewX1) + '/' + IntToStr(NewY1) + ' to ' + IntToStr(x) + '/' + IntToStr(y)); {$EndIf}
            RespondingToMouseMove := false;
            exit;
         end;
         LastX := x;
         LastY := y;
         if DEMNowDoing in [NewTrack] then AddLatLongPointToStreamProfile(Lat,Long)
         else dbAddRecForm.ShapeFileCreator.ProcessShapeDigitization(Self);
         {$IfDef RecordStreamModeDigitize} WriteLineToDebugFile('far enough, TMapForm.Image1MouseMove from ' + IntToStr(NewX1) + '/' + IntToStr(NewY1) + ' to ' + IntToStr(x) + '/' + IntToStr(y) + '  ' + LatLongDegreeToString(Lat,Long) ): {$EndIf}
         if (StreamProfileResults = Nil) then begin
            MapScreenX1 := x;
            MapScreenY1 := y;
         end;
         NewX1 := x;
         NewY1 := y;
         RespondingToMouseMove := false;
         Petmar.ScreenSymbol(Image1.Canvas,x,y,Box,2,claRed);
         exit;
      end;

      if (XYZDisplayForm <> Nil) then begin
         if (XYZDisplayForm.DEM1.Checked or XYZDisplayForm.DEMConstant1.Checked) then begin
            if XYZDisplayForm.DEMConstant1.Checked then z1 := z1 + XYZDisplayForm.AboveDEM;
            XYZDisplayForm.Edit1.Text := RealToString(z1,-18,-2);
         end;

         if MouseIsDown and (DEMNowDoing = DigitizeContourStream) then begin
             if (abs(x - Newx1) > MDDef.ContDigitizeSeparation) or (abs(Y - NewY1) > MDDef.ContDigitizeSeparation) then begin
                XYZDisplayForm.AddPoint(x,y);
                NewX1 := x;
                NewY1 := y;
             end;
         end;
      end;
   {$EndIf}

   if MapDraw.OnScreen(MapScreenX1,MapScreenY1) and (DrawLineMode or (ShapeFileDigitizingUnderway(DEMNowDoing) and (StreamProfileResults <> Nil))) then begin
       {$If Defined(RecordMapRoam) or Defined(RecordStreamModeDigitize)} WriteLineToDebugFile('Image1MouseMove draw lines'); {$EndIf}
       SetRedrawMode(Image1);
       Image1.Canvas.MoveTo(MapScreenX1,MapScreenY1);
       Image1.Canvas.LineTo(Lastx,Lasty);
       Image1.Canvas.MoveTo(MapScreenX1,MapScreenY1);
       Image1.Canvas.LineTo(x,y);
   end;

   if DraggingOperation(DEMNowDoing) and (SavedMapImage <> Nil) then begin
      {$IfDef RecordEditDB} WriteLineToDebugFile('Drag icon'); {$EndIf}
       CreateBitmap(newbmp,savedmapImage.width,savedmapImage.height);
       newbmp.Canvas.Draw(0,0,SavedMapImage);
       newbmp.Canvas.CopyMode := cmSrcAnd;

       if (DEMNowDoing in [DragEdit]) then newbmp.Canvas.Draw(x,y,DragBitmap);
       {$IfDef ExMilicons}
       {$Else}
          if (DEMNowDoing = AddMilIcon) then begin
             if (MilIconsForm.StampBitmap <> nil) then
                newbmp.Canvas.Draw(x-MilIconsForm.StampBitmap.Width div 2,y-MilIconsForm.StampBitmap.Height div 2,MilIconsForm.StampBitmap);
          end;
       {$EndIf}

       Image1.Picture.Graphic := newbmp;
       newbmp.free;
      {$IfDef RecordEditDB} WriteLineToDebugFile('Drag over'); {$EndIf}
   end;


   if MouseDragging then begin
      {$IfDef RecordMapRoam} WriteLineToDebugFile('Image1MouseMove MouseDragging'); {$EndIf}
      SetRedrawMode(Image1);
      if (Newx1 < MaxInt) then begin
         Image1.Canvas.MoveTo(newx1,newy1);
         Image1.Canvas.LineTo(lastx,lasty);
         Image1.Canvas.MoveTo(newx1,newy1);
         Image1.Canvas.LineTo(x,y);
         {$IfDef RecordMouseDrag} WriteLineToDebugFile('1: ' + IntToStr(newx1) + '/' + IntToStr(newy1) + ' ' + IntToStr(LastX) + '/' + IntToStr(LastY) + ' ' + IntToStr(X) + '/' + IntToStr(Y) ); {$EndIf};
      end;
      if (newx2 < MaxInt) then begin
         Image1.Canvas.MoveTo(newx2,newy2);
         Image1.Canvas.LineTo(lastx,lasty);
         Image1.Canvas.MoveTo(newx2,newy2);
         Image1.Canvas.LineTo(x,y);
         {$IfDef RecordMouseDrag} WriteLineToDebugFile('2: ' + IntToStr(newx2) + '/' + IntToStr(newy2) + ' ' + IntToStr(LastX) + '/' + IntToStr(LastY) + ' ' + IntToStr(X) + '/' + IntToStr(Y) ); {$EndIf};
         ShowBoxSize;
      end;
   end;

   if MouseIsDown and (DEMNowDoing = ErasingPoints) then begin
      MapDraw.ScreenToDataGrid(X,Y,XGrid,YGrid);
      Image1.Canvas.Pen.Color := ConvertPlatformColorToTColor(MDdef.MissingDataColor);
      Image1.Canvas.Brush.Color := ConvertPlatformColorToTColor(MDdef.MissingDataColor);
      Image1.Canvas.Pen.Mode := pmCopy;
      MapDraw.DEMGridToScreen(XGrid-EraserSize,YGrid-EraserSize,x1,y1);
      MapDraw.DEMGridToScreen(XGrid+EraserSize,YGrid+EraserSize,x2,y2);
      Image1.Canvas.Rectangle(x1,y1,x2,y2);
      for i := (round(XGrid)-EraserSize) to (round(XGrid)+EraserSize) do begin
         if (i >= 0) and (i < DEMGlb[MapDraw.DEMonMap].DEMheader.NumCol) then begin
            for j := (round(YGrid)-EraserSize) to (round(YGrid)+EraserSize) do begin
               if (j >= 0) and (j < DEMGlb[MapDraw.DEMonMap].DEMheader.NumRow) then DEMGlb[MapDraw.DEMonMap].SetGridMissing(i,j);
            end;
         end;
      end;
      DEMGlb[MapDraw.DEMonMap].DEMstatus := dsUnsaved;
   end;

   if MouseIsDown and BoxOutlining(DEMNowDoing) then begin
      {$IfDef RecordMapRoam} WriteLineToDebugFile('ImageMoueMove Box outlining'); {$EndIf}
      SetRedrawMode(Image1);
      Image1.Canvas.Rectangle(newx1,newy1,newx2,newy2);
      if MDDef.AspectBoxRegion and (DEMNowDoing in [NewCoverage]) then begin
         dx := (NewX1 - x);
         y := NewY1 - round(dx * MDDef.YAspect / MDDef.XAspect);
      end;

      if (DEMNowDoing = MoveMapBox) then begin
         Newx2 := x + (Newx2-Newx1);
         Newy2 := y + (Newy2-Newy1);
         NewX1 := x;
         NewY1 := y;
      end
      else begin
         Newx2 := x;
         Newy2 := y;
      end;
      Image1.Canvas.Rectangle(newx1,newy1,newx2,newy2);
      ShowBoxSize;
   end;

   if AgeFromDepth1.Checked and (MDdef.ProgramOption in [GeologyProgram,GeographyProgram]) and (MapDraw.DEMMap) then begin
      DEMGlb[MapDraw.DEMonMap].LatLongDegreeToDEMGrid(Lat,Long,XGrid,YGrid);
      Sum := 0;
      Count := 0;
      for i := pred(Round(XGrid)) to succ(round(Xgrid)) do
         if (i >= 0) and (i < DEMGlb[MapDraw.DEMonMap].DEMheader.NumCol) then
            for j := pred(Round(YGrid)) to succ(round(Ygrid)) do
               if (j >= 0) and (j < DEMGlb[1].DEMheader.NumRow) then
                  if not DEMGlb[MapDraw.DEMonMap].MissingDataInGrid(i,j) then begin
                     if DEMGlb[MapDraw.DEMonMap].GetElevMetersOnGrid(i,j,z1) then begin
                        inc(Count);
                        Sum := Sum + z1;
                     end;
                  end;
      if (Count > 0) then ZElev := Sum / Count;
      if (Count = 0) then TStr := ''
      else if (zElev > 0) then Tstr := 'Continent'
      else if (zElev > -2500) then Tstr := 'Continental Margin'
      else if (zElev < -5500) then TStr := 'T > 100 Ma'
      else if (zElev < -6500) then TStr := 'Trench'
      else TStr := 'Age:' + RealToString( sqr((zElev + 2500) / -300),6,1) + ' Ma';
      WmDEM.SetPanelText(2, TStr);
   end;

   if MDDef.RoamAllZ and (NumDEMDataSetsOpen > 1) then begin
      TStr := '';
      for i := 1 to MaxDEMDataSets do begin
         if ValidDEM(i) then begin
            if DEMGlb[i].GetElevFromLatLongDegree(Lat,Long,zElev) then TStr := TStr + '  ' + DEMGlb[i].AreaName + '=' + RealToString(ZElev,-12,2);
         end;
      end;
      WmDEM.SetPanelText(3, TStr);
   end;

   {$IfDef Ex3D}
   {$Else}
      if (DEMNowDoing in [SeekingSecondPerspective,SeekingSecondCircleFly,LiveFly2]) and ( (LastX <> x) or (LastY <> Y)) then begin
         {$IfDef RecordMapRoam} WriteLineToDebugFile('Image1MouseMove draw perspective'); {$EndIf}
         DrawPerspective(Lastx,Lasty,pmNotXor);
         DrawPerspective(x,y,pmNotXor);
      end;
   {$EndIf}

   {$IfDef ExTissot}
   {$Else}
      if (DEMNowDoing = MapTissotIndicatrix) then begin
         MapDraw.GetMapScaleFactor(Lat,Long,Maph,Mapk,Prime);
         if Prime then TStr := '''=' else TStr := '=';
         Omega := 2 * arcsin(abs(maph-mapk)/(Maph+Mapk)) / degtorad;
         wmDEM.SetPanelText(3, 'h' + TStr +  RealToString(MapH,-12,4) + '  & k' + TStr + RealToString(MapK,-12,4) + '   w=' + RealToString(Omega,2,3) + '°');
      end;
   {$EndIf}

   if (DEMNowDoing = GeodeticBearing) or (DEMNowDoing = ShapeFirstLine) or (DEMNowDoing = ShapeFirstPolygon) then begin
      MapDraw.ScreenToLatLongDegree(Lastx,Lasty,Lat,Long);
      VincentyCalculateDistanceBearing(gbLatStart,gbLongStart,Lat,Long,Distance,BearingAngle);
      WmDEM.SetPanelText(3, 'Bearing angle: ' + RealToString(BearingAngle,-8,2) + '    ' +  SmartDistanceMetersFormat(Distance));
   end;

   if (DEMNowDoing = UTMTrueDeviation) then begin
      WmDEM.SetPanelText(3, 'Grid-true declination: ' + RealToString(MapDraw.UTMGridToTrueNorthAngle(Lat,Long),-8,2));
   end;

   {$IfDef ExMag}
   {$Else}
      if (DEMNowDoing = ShowMagneticVariation) then begin
         MagVr1(0,lat,Long,CurMagYear, DEC,DIP,TI,GV);
         WmDEM.SetPanelText(3, 'Magnetic declination: ' + RealToString(Dec,-8,2) + '°');
      end;
   {$EndIf}

   {$IfDef ExDrainage}
   {$Else}
      if (DEMNowDoing = DrainageArea) then FindDrainageAreaContributing(x,y);
   {$EndIf}

   if USGSQuadNames1.Checked then begin
      if (not (DEMNowDoing in [ShowMagneticVariation,UTMTrueDeviation,GeodeticBearing,NLCDClassification,MapTissotIndicatrix])) and
         MDDef.ShowUSGSQuadNames and GetUSGSQuadName(Lat,Long,GeoName) then
            WmDEM.SetPanelText(3,'24K quad: ' + GeoName);
   end;

   LastX := x;
   LastY := y;
   Image1.Canvas.Pen.Mode := pmCopy;
   RespondingToMouseMove := false;
   {$IfDef RecordMapRoam} WriteLineToDebugFile('leaving Image1MouseMove'); {$EndIf}
end;


procedure TMapForm.Createbitmapmask1Click(Sender: TObject);
var
   Bitmap : tMyBitmap;
begin
   CloneImageToBitmap(Image1,Bitmap);
   Image1.Picture.Graphic := Bitmap;
   FreeAndNil(Bitmap);
   OpenDBonMap('Bitmap mask','');
   Saveimage1Click(Nil);
   DoBaseMapRedraw;
end;


procedure TMapForm.Createdatabase2Click(Sender: TObject);
const
   MaxColor = 255;
var
   Bitmap : tMyBitmap;
   MinX,MinY,MaxX,MaxY,xsum,ysum,Npts : array[1..MaxColor] of integer;
   StartRegion,RegionID,NewDEM,x,y,Culled : integer;
   OnColor : tColor;
   Lat,Long,Area : float64;
   z : float32;
   P0    : tScreenPRGB;
   Histy : tStringList;
   FindCentroid : boolean;
   TStr,TStr2,TStr3 : shortstring;
   fName : PathStr;


            procedure ProcessMap;
            var
               i,x2,y2 : integer;
            begin
               {$IfDef RecordNewMaps} WriteLineToDebugFile('Process map in RegionID=' + IntToStr(RegionID) +' Startregion=' + IntToStr(StartRegion) + '  OnColor=' + IntToStr(OnColor)); {$EndIf}

                  for i := 1 to MaxColor do begin
                     NPts[i] := 0;
                     xsum[i] := 0;
                     ysum[i] := 0;
                     MinX[i] := 2560000;
                     MinY[i] := 2560000;
                     MaxX[i] := -99;
                     MaxY[i] := -99;
                  end;

                  {$IfDef RecordNewMaps} WriteLineToDebugFile('Start map check'); {$EndIf}
                  for y2 := y to pred(DEMGlb[NewDEM].DEMheader.NumRow) do begin
                     ThreadTimers.UpdateThreadStats(1,round(100 * y2/DEMGlb[NewDEM].DEMheader.NumRow));
                     for x2 := 0 to pred(DEMGlb[NewDEM].DEMheader.NumCol) do begin
                        if SameColor(p0[y2][x2], RGBTripleWhite) or SameColor(p0[y2][x2], RGBTripleBlack) then begin
                        end
                        else begin
                           for I := 1 to OnColor do begin
                              if SameColor(p0[y2][x2], ConvertTColorToPlatformColor(RGB(i,0,0))) then begin
                                 CompareValueToExtremes(x2,MinX[i],MaxX[i]);
                                 CompareValueToExtremes(y2,MinY[i],MaxY[i]);
                                 DEMGlb[NewDEM].SetGridElevation(x2,y2,StartRegion + i);
                                 p0[y2][x2] := RGBTripleBlack;
                                 inc(Npts[i]);
                                 if FindCentroid then begin
                                    xsum[i] := xsum[i] + x2;
                                    ysum[i] := ysum[i] + y2;
                                 end;
                              end;
                           end;
                        end;
                     end;
                  end;

                  {$IfDef RecordNewMaps} WriteLineToDebugFile('Start color check'); {$EndIf}
                  for I := 1 to OnColor do begin
                     if (NPts[i] > MDDef.MinRegionPoints) then begin
                         inc(RegionID);
                         if FindCentroid then begin
                            DEMGlb[NewDEM].DEMGridtoLatLongDegree(xsum[i] / Npts[i],ysum[i] / Npts[i],Lat,Long);
                            TStr := ',' + RealToString(Lat,-14,-8) + ',' + RealToString(Long,-14,-8);
                         end
                         else TStr := '';
                         TStr2 := ',' + IntToStr(MinX[i])  + ',' + IntToStr(MinY[i]) + ',' + IntToStr(MaxX[i])  + ',' + IntToStr(MaxY[i]);
                         Area := DEMGlb[NewDEM].AverageXSpace * DEMGlb[NewDEM].AverageYSpace * NPts[i];
                         if (DEMGlb[NewDEM].AverageSpace < 10) then TStr3 := RealToString(Area,-18,-2)
                         else TStr3 := RealToString(Area * 0.001 * 0.001,-18,-6);

                         Histy.Add(IntToStr(RegionID) + ',' + IntToStr(Npts[i]) + ',' + TStr3 + TStr + TStr2);
                     end
                     else inc(Culled);

                     for x2 := MinX[i] to MaxX[i] do begin
                       for y2 := MinY[i] to MaxY[i] do begin
                          if DEMGlb[NewDEM].GetElevMetersOnGrid(x2,y2,z) and (Round(z) = (StartRegion+i)) then begin
                             if (NPts[i] < MDDef.MinRegionPoints) then begin
                                DEMGlb[NewDEM].SetGridMissing(x2,y2);
                             end
                             else begin
                                DEMGlb[NewDEM].SetGridElevation(x2,y2,RegionID);
                             end;
                          end;
                       end;
                     end;
                  end;
                  StartRegion := RegionID;
                  OnColor := 0;
                  Histy.SaveToFile(ChangeFileExt(fName,'.vat.csv'));
                  ThreadTimers.UpdateThreadStats(1,0,'');
                  ThreadTimers.UpdateThreadStats(2,0,'');
            end;



begin
   {$IfDef RecordNewMaps} WriteLineToDebugFile('TMapForm.IDFeatures1Click in'); {$EndIf}
  try
    FindCentroid := AnswerIsYes('Find centroid');
    MDDef.MinRegionPoints := 250;
    ReadDefault('Minimum points for region',MDDef.MinRegionPoints);

   NewDem := DEMGlb[MapDraw.DEMonMap].CloneAndOpenGridSetMissing(WordDEM,DEMGlb[MapDraw.DEMonMap].AreaName  + '_features',euIntCode);
   CreateBitmap(Bitmap,DEMGlb[NewDEM].DEMheader.NumCol,DEMGlb[NewDEM].DEMheader.NumRow);

   StartProgress('Zero');
   for y := 0 to pred(Bitmap.Height) do P0[y] := Bitmap.ScanLine[y];
   for y := 0 to pred(DEMGlb[NewDEM].DEMheader.NumRow) do begin
      if (y mod 25 = 0) then UpDateProgressBar(y/DEMGlb[NewDEM].DEMheader.NumRow);
      for x := 0 to pred(DEMGlb[NewDEM].DEMheader.NumCol) do begin
         if DEMGlb[MapDraw.DEMonMap].MissingDataInGrid(x,y) then p0[y][x] := RGBTripleBlack;
      end;
   end;
   EndProgress;

   fName := ExtractFilePath(DEMGlb[MapDraw.DEMonMap].DEMFileName) + ExtractFileNameNoExt(DEMGlb[MapDraw.DEMonMap].DEMFileName) + '_features.dem';
   Histy := tStringList.Create;
   if FindCentroid then TStr := ',LAT,LONG' else TStr := '';
   if (DEMGlb[NewDEM].AverageSpace < 10) then TStr3 := 'M2' else TStr3 := 'KM2';
   TStr2 := ',BOUND_XMIN,BOUND_YMIN,BOUND_XMAX,BOUND_YMAX';
   Histy.Add(RecNoFName + ',NPTS,AREA_' + TStr3 + TStr + TStr2);
   RegionID := 0;
   Culled := 0;
   StartRegion := 0;

   ShowSatProgress := false;
   StartSingleThreadTimer('Locate');
   ThreadTimers.EnableGauge(1,true,'Map');
   OnColor := 0;
   for y := 0 to pred(DEMGlb[NewDEM].DEMheader.NumRow) do begin
      if (y mod 25 = 0) then begin
         TStr := 'Region=' + IntToStr(RegionID) + ' cull=' + IntToStr(Culled) + ' '  + TimeToStr(Now);
         ThreadTimers.UpdateThreadStats(9,round(100 * y/DEMGlb[NewDEM].DEMheader.NumRow),TStr);
      end;

      for x := 0 to pred(DEMGlb[NewDEM].DEMheader.NumCol) do begin
         if (Not SameColor(p0[y][x],RGBTripleBlack)) then begin
            inc(OnColor);
            Bitmap.Canvas.Brush.Color := RGB(Oncolor,0,0);
            Bitmap.Canvas.FloodFill(x,y,clBlack,fsBorder);
            if (OnColor = MaxColor) then begin
               ProcessMap;
            end;
         end {for x};
      end;
   end;
   if OnColor > 0 then ProcessMap;

   DEMGlb[NewDEM].SetUpMap(NewDEM,true,mtElevSpectrum);
   DEMGlb[NewDEM].WriteNewFormatDEM(fName);
   DEMGlb[NewDEM].VATFileName := ChangeFileExt(fName,'.vat.dbf');
   FeaturesDB := DEMGlb[NewDEM].SelectionMap.StringListToLoadedDatabase(Histy, DEMGlb[NewDEM].VATFileName);

   if (Culled > 0) then MessageToContinue('Small regions culled: ' + IntToStr(Culled));

   Bitmap.Free;
  finally
    EndThreadTimers;
    ShowSatProgress := true;
  end;
end;

function UTMgridBoxFilter(GridSize : integer; utmx,utmy : float64) : shortstring;
var
   Lowx,lowy,hix,hiy : integer;
begin
   LowX := GridSize * (trunc(utmx) div GridSize);
   LowY := GridSize * (trunc(utmy) div GridSize);
   HiX := LowX + GridSize;
   HiY := LowY + GridSize;
   Result := '(X_UTM>=' + IntToStr(lowX) + ') AND (X_UTM<=' + IntToStr(hix)  + ') AND (Y_UTM>=' + IntToStr(lowY) + ') AND (Y_UTM<=' + IntToStr(hiy)  + ')';
end;

procedure TMapForm.CreatefilterforUTMsquare1Click(Sender: TObject);
var
   utmx,utmy : float64;
   GridSize : integer;
begin
   MapDraw.ScreenToUTM(RightClickX,RightClickY,utmx,utmy);
   GridSize := 100;
   ReadDefault('Grid size (m)',GridSize);
   MessageToContinue(UTMgridBoxFilter(GridSize,utmx,utmy),true);
end;


procedure TMapForm.Creategroputing1Click(Sender: TObject);
begin
   DEM_Indexes.DefineShapeFileGrouping('');
end;


procedure TMapForm.CreateimagetomatchDEM1Click(Sender: TObject);
var
   Bitmap,Bitmap2 : tMyBitmap;
   ImageWanted,x,y,xp,yp : integer;
   Lat,Long : float64;
   NewMap : tMapForm;
   p0 : pRGB;
   p1 : tScreenPRGB;
begin
   if GetImage(ImageWanted) then begin
      CloneImageToBitmap(Image1,Bitmap);
      StartProgress('Image');
      CopyImageToBitmap(SatImage[ImageWanted].SelectionMap.Image1,Bitmap2);

      FillScanlineAddresses(Bitmap2,p1);
      for y := 0 to pred(Bitmap.Height) do begin
         UpdateProgressBar(y/Bitmap.Height);
         P0 := Bitmap.ScanLine[y];
         for x := 0 to pred(Bitmap.Width) do begin
            MapDraw.ScreenToLatLongDegree(x,y,Lat,Long);
            SatImage[ImageWanted].SelectionMap.MapDraw.LatLongDegreeToScreen(Lat,Long,xp,yp);
            if SatImage[ImageWanted].SelectionMap.MapDraw.OnScreen(xp,yp) then
              p0^[x] := p1[yp]^[xp]
         end;
      end;
      NewMap := DuplicateMap(false);
      NewMap.Image1.Picture.Graphic := Bitmap;
      NewMap.Caption := 'Reprojected ' + SatImage[ImageWanted].SceneBaseName;
      Bitmap.Free;
      Bitmap2.Free;
      EndProgress;
   end;
end;


procedure TMapForm.CreateMedianDNgrid1Click(Sender: TObject);
begin
   {$IfDef ExMultigrid}
   {$Else}
       MultiGridArray[MapDraw.MultiGridOnMap].MedianGrids(true);
   {$EndIf}
end;

procedure TMapForm.CreateMinDNgrid1Click(Sender: TObject);
begin
   {$IfDef ExMultiGrid}
   {$Else}
      MultiGridArray[MapDraw.MultiGridOnMap].CreateMinValueGrid;
   {$EndIf}
end;

procedure TMapForm.Createnewgridforthismaparea1Click(Sender: TObject);
begin
   MakeTempGrid(true,true);
end;

procedure TMapForm.Createsurveylines1Click(Sender: TObject);
begin
   {$IfDef ExAdvancedGIS}
   {$Else}
      MapDraw.ScreenToLatLongDegree(RightClickX,RightClickY,Clipboard_Lat,Clipboard_Long);
      ClipBoard_Coords  := true;
      GetSurveyTracks(Self);
   {$EndIf}
end;

procedure TMapForm.Createtablefromimage1Click(Sender: TObject);
var
   fName : PathStr;
   GISNum : integer;
begin
   {$IfDef ExSat}
   {$Else}
      SatImage[MapDraw.SATonMap].CreateDBfromSatImage(fName,MapDraw.MapCorners.BoundBoxDataGrid);
      DEMDataBase.OpenNumberedGISDataBase(GISNum,fName,true,false,Self);
   {$EndIf}
end;


procedure TMapForm.CreateZoomWindow(UseToVerify : boolean; ContourInterval,DEMGridSize : integer; var xDEMg,yDEMg,xSATg,ySATg : float32; ShowPanButtons : boolean = true; ShowMenu : boolean = true; LabelGridPts : boolean = true);
var
   Pt : tPoint;
begin
   {$IfDef RecordZoomWindow} WriteLineToDebugFile('TMapForm.CreateZoomWindow in'); {$EndIf}
   if (MapDraw.DEMonMap = 0) then exit;
   SaveBackupDefaults;
   GetCursorPos(Pt);
   if (ZoomWindow = Nil) then begin
      ZoomWindow := CreateANewDEMMapWindow(MapDraw.DEMonMap,false,MDDef.DefDEMMap,'Zoom map');
      ZoomWindow.Left := Mouse.CursorPos.X;
      ZoomWindow.Top := Mouse.CursorPos.Y;
      ZoomWindow.PanButtonsOnMap := ShowPanButtons;
      ZoomWindow.ShowAMenu := ShowMenu;
      ZoomWindow.HideToolbar;
      ZoomWindow.MapDraw.MapOwner := moPointVerificationMap;
      ZoomWindow.MapDraw.DrawLegendsThisMap := false;
      {$IfDef ExDrainage}
      {$Else}
         if false and (not UseToVerify) then begin
            Drainage_opts.SetDrainageDelineationOptions(Self);
            ZoomWindow.MapDraw.RedrawDrainageVectors := true;
         end;
      {$EndIf}
   end
   else begin
      ZoomWindow.MapDraw.DeleteMapSavedLayers;
   end;
   {$IfDef RecordZoomWindow} WriteLineToDebugFile('TMapForm.CreateZoomWindow created'); {$EndIf}

   ZoomWindow.Closable := false;
   ZoomWindow.MapDraw.FirstMapDrawing := false;
   if UseToVerify then begin
      DEMNowDoing := VerifyingPoint;
      ZoomWindow.FormStyle := fsNormal;
      ZoomWindow.Visible := false;
   end
   else begin
      ZoomWindow.FormStyle := fsStayOnTop;
   end;
   {$IfDef RecordZoomWindow} WriteLineToDebugFile('TMapForm.CreateZoomWindow set up done'); {$EndIf}

   if MapDraw.DEMMap then begin
      ZoomWindow.MapDraw.MapType := mtIHSReflect;
      MDDef.MapTicks := tixNone;
      {$IfDef RecordZoomWindow} WriteLineToDebugFile('TMapForm.CreateZoomWindow center, x=' + RealToString(xDEMg,-12,-2) + '  y=' + RealToString(yDEMg,-12,-2)); {$EndIf}
      ZoomWindow.RedrawMapForDataGrid(xDEMg - DEMGridSize,yDEMg + DEMGridSize,xDEMg + DEMGridSize,yDEMg - DEMGridSize,MDDef.DefaultTerrainXSize,MDDef.DefaultTerrainYSize);
      {$IfDef RecordZoomWindow} WriteLineToDebugFile('TMapForm.CreateZoomWindow limits set,  data grid=' + sfBoundBoxToString(ZoomWindow.MapDraw.MapCorners.BoundBoxDataGrid,2)); {$EndIf}
   end
   else begin
      {$IfDef ExSat}
      {$Else}
         ZoomWindow.MapDraw.MapType := mtSatImageGray;
         ZoomWindow.MapDraw.SatOnMap := MapDraw.SatOnMap;
         MapDraw.DEMonMap := MapDraw.DEMonMap;
         ZoomWindow.RedrawMapForDataGrid(xSatg-250,ySatg2 + 250,xSatg + 250,YSatG-250,MDDef.DefaultTerrainXSize,MDDef.DefaultTerrainYSize);
         ZoomWindow.CheckAndDrawNewCorners;
      {$EndIf}
   end;

   ZoomWindow.MapDraw.FirstMapDrawing := false;
   AddOverlay(ZoomWindow,ovoContours);
   ZoomWindow.Keyboard1.Visible := ShowMenu;
   ZoomWindow.Help1.Visible := ShowMenu;
   ZoomWindow.Info1.Visible := ShowMenu;

   if UseToVerify then begin
      ZoomWindow.ShowModal;
      xDEMg := ZoomWindowxDEMg1;
      yDEMg := ZoomWindowyDEMg1;
      SetCursorPos(Pt.x,Pt.y);
      ZoomWindow := Nil;
   end
   else begin
     {$IfDef RecordZoomWindow} WriteLineToDebugFile('TMapForm.CreateZoomWindow not UseToVerify'); {$EndIf}
      ZoomWindow.Show;
   end;
   RestoreBackupDefaults;
   {$IfDef RecordZoomWindow} WriteLineToDebugFile('TMapForm.CreateZoomWindow out'); {$EndIf}
end;


procedure TMapForm.CheckThisPoint(var X,Y : integer; var xDEMg,yDEMg,xSATg,ySATg : float32; NeedCheck : tCheckPoint);
var
   Lat,Long,xutm,yutm : float64;
   WhatsUp : tDEMDoingWhat;
begin
   {$IfDef RecordCheckPoint} WriteLineToDebugFile('Checking point'); {$EndIf}
   with MapDraw do begin
      if (VectorIndex <> 0) then begin
         ScreenToLatLongDegree(X,Y,Lat,Long);
         if ValidDEMonMap then DEMGlb[DEMOnMap].LatLongDegreeToDEMGrid(Lat,Long,xDEMg,yDEMg);
         exit;
      end;
      if DEMMap then begin
         ScreenToDataGrid(X,Y,xDEMg,yDEMg);
         xSATg := -1;
         ySATg := -1;
      end
      else begin
         ScreenToDataGrid(X,Y,xSatg,ySATg);
         if ValidDEMonMap then MapGridToDEMGrid(xSATg,ySATg,xDEMg,yDEMg);
      end;

      WhatsUp := DEMNowDoing;
      DEMNowDoing := Calculating;
      {$IfDef RecordCheckPoint}
         if ValidDEMonMap then begin
            ScreenToLatLong(X,Y,Lat,Long);
            WriteLineToDebugFile('Point selected on map ' + DEMGlb[DEMOnMap].DEMLocationString(xDEMg,yDEMg) + '   ' + LatLongDegreeToString(Lat,Long));
         end;
      {$EndIf}
      if (NeedCheck <> CheckNothing) and ((MDdef.CheckPoint >= NeedCheck) or (MDdef.CheckPoint = CheckAll)) then begin
         if (WhatsUp in [SeekingSecondLOS,SecondDistancePoint,MultipleTopoProfileRight,SimpleTopoProfileRight,SeekingSecondAverageProfile]) then with Image1.Canvas do begin
            Pen.Mode := pmNotXor;
            Pen.Color := clBlack;
            Pen.Width := 3;
            MoveTo(MapScreenX1,MapScreenY1);
            LineTo(x,y);
         end;

         if MDdef.GraphicalCoordVerify and ValidDEMonMap then begin
            CreateZoomWindow(true,10,20,xDEMg,yDEMg,xSATg,ySATg,true,false);
         end
         else begin
            if DEMMap then VerifyPointOnMap('',xDEMg,yDEMg,XUTM,YUTM)
            else begin
               VerifyPointOnMap('',xSATg,ySATg,XUTM,YUTM);
               MapGridToDEMGrid(xSATg,ySATg,xDEMg,yDEMg);
            end;
         end;

         if DEMMap then DataGridToScreen(xDEMg,yDEMg,x,y)
         else DataGridToScreen(xSATg,ySATg,x,y);
         if (WhatsUp in [SeekingSecondLOS,SecondDistancePoint,MultipleTopoProfileRight,SimpleTopoProfileRight,SeekingSecondAverageProfile]) then with Image1.Canvas do begin
            MoveTo(MapScreenX1,MapScreenY1);
            LineTo(x,y);
         end;
      {$IfDef RecordCheckPoint}
         if (DEMonMap > 0) then begin
            DEMGlb[DEMOnMap].DEMGridToLatLong(xDEMg,yDEMg,Lat,Long);
            WriteLineToDebugFile('Verified ' + DEMGlb[DEMOnMap].DEMLocationString(xDEMg,yDEMg) +  '   ' + LatLongDegreeToString(Lat,Long,DecDegrees));
         end;
      {$EndIf}
      end;
      DEMNowDoing := WhatsUp;
   end;
end;


procedure TMapForm.DrawRecordBeingEditted(Table : tMyData);
var
   x,y : integer;

   procedure GetCoords;
   begin
      MapDraw.LatLongDegreeToScreen(Table.GetFieldByNameAsFloat('LAT'), Table.GetFieldByNameAsFloat('LONG'),x,y);
   end;

begin
   {$IfDef RecordShapeFileEdits} WriteLineToDebugFile('TMapForm.DrawRecordBeingEditted, recCount=' + IntToStr(Table.FiltRecsInDB)); {$EndIf}
   Table.First;
   Image1.Canvas.Pen.Color := clLime;
   Image1.Canvas.Pen.Mode := pmCopy;
   GetCoords;

   Image1.Canvas.MoveTo(x,y);
   while not Table.eof do begin
      GetCoords;
      Image1.Canvas.LineTo(x,y);
      Table.next;
   end;

   Table.First;
   while not Table.eof do begin
      GetCoords;
      Petmar.ScreenSymbol(Image1.Canvas,x,y,Box,2,claRed);
      Table.next;
   end;
 end;

procedure PostNewLatLongInEditDataBase(Lat,Long : float64);
begin
   {$IfDef RecordShapeFileEdits} WriteLineToDebugFile('add point to edit db ' + LatLongDegreeToString(Lat,Long)); {$EndIf}
   GISdb[DBEditting].MyData.SetFieldByNameAsFloat('LAT',Lat);
   GISdb[DBEditting].MyData.SetFieldByNameAsFloat('LONG',Long);
   GISdb[DBEditting].MyData.Post;
end;


procedure TMapForm.MoveADBRecord(lat,Long : float64);
begin
   GISDB[DBEditting].MyData.Edit;
   GISDB[DBEditting].ChangeLatLongLocation(Lat,Long);
   GISDB[DBEditting].MyData.Post;
   GISDB[DBEditting].MyData.ApplyFilter(PreEditFilter);
   GISDB[DBEditting].theMapOwner.MapDraw.ClearGISLayer(DBEditting);
   DoFastMapRedraw;
   RecMoving := false;
end;


procedure TMapForm.Image1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
   i : integer;
   xg1,yg1,xg2,yg2,xg,yg : float32;
   xu,yu,yl,
   LatLow,LongLow,LatHigh,LongHigh : float64;
   TStr : ShortString;
begin
   if ClosingIsHappening or (MapDraw = Nil) or (MapDraw.MapOwner = moPointVerificationMap) or (DEMNowDoing = Calculating) or (DEMRequiredForOperation(MapDraw.DEMonMap) and (MapDraw.DEMonMap = 0)) then exit;
   MapDraw.ScreenToLatLongDegree(LastX,LastY,MouseUpLat,MouseUpLong);
   {$IfDef RecordMapDraw} WriteLineToDebugFile('Mouse up at: ' + LatLongDegreeToString(MouseUpLat,MouseUpLong,DecDegrees)); {$EndIf}

   if NowStreamDigitizing(DemNowDoing) and MouseIsDown and (MDDef.DigitizeMode = dmStream) then begin
      DoneWithStreamSelection;
   end;

   if (DEMNowDoing in [MovePointDBRecs]) then begin
      if (GISdb[DBEditting].MyData.FiltRecsInDB = 1) then begin
         {$IfDef RecordEditDB} WriteLineToDebugFile('Mouse up, MovePointDBRecs'); {$EndIf}
         MoveADBRecord(MouseUpLat,MouseUpLong);
      end
      else exit;
   end;

   {$IfDef Ex3D}
   {$Else}
      if MouseDragging then begin
          MouseDragging := false;
          CheckThisPoint(X,Y,xg1,yg1,xg2,yg2,CheckReasonable);
          DEMGlb[MapDraw.DEMonMap].DEMGridToUTM(xg1,yg1,xu,yu);
          TStr := (RealToString(xu,9,0) + RealToString(yu,12,0));
          if (EditPoint > 0) then StreamProfileResults.Strings[EditPoint] := TStr
          else StreamProfileResults.Insert(abs(EditPoint),Tstr);
          DrawColoredMap1Click(Nil);
          if not AnswerIsYes('Continue edit') then begin
             if (DEMNowDoing = EditFlightPathOnMap) then begin
                ChangeDEMNowDoing(SeekingFlyThroughRoute);
                StartTheFlyThrough(DEMNowDoing);
             end
             else DrawStreamProfile(true);
          end;
      end;
   {$EndIf}

    if MouseIsDown then begin
       MouseIsDown := false;
       ShowHourglassCursor;

       MinOfPairFirst(NewX1,NewX2);
       MinOfPairFirst(NewY1,NewY2);

       if DEMNowDoing in [NewCoverage] then begin
          if (NewX1 = NewX2) or (NewY1 = NewY2) then exit;
          {$If Defined(RecordClick) or Defined(RecordMapResize) or Defined(RecordSubsetZoom)} WriteLineToDebugFile('Mouse up pick box, point 1: ' + IntToStr(NewX1) + '/' + IntToStr(NewY1) + ' point2: ' + IntToStr(NewX2) + '/' + IntToStr(NewY2)); {$EndIf}
          SubsetAndZoomMap(NewX1,Newy1,NewX2,Newy2);
          CheckProperTix;
          BackToWandering;
          {$If Defined(RecordClick) or Defined(RecordMapResize)} WriteLineToDebugFile('Mouse up to pick new box done'); {$EndIf}
          Exit;
       end;

       if BoxOutlining(DEMNowDoing) then begin
          if (NewX1 = x) or (NewY1 = y) then exit;
          if (not MDDef.AspectBoxRegion) then begin
             Newx2 := x;
             NewY2 := y;
          end;
          {$IfDef RecordMapResize} WriteLineToDebugFile('First point: ' + IntToStr(NewX1) + '/' + IntToStr(NewY1) + ' Last  point: ' + IntToStr(NewX2) + '/' + IntToStr(NewY2)); {$EndIf}
          {$IfDef RecordMapResize} WriteLineToDebugFile('NW point: x=' + IntegerToString(NewX1,8) + IntegerToString(NewY1,8) + ' SE point: x=' + IntegerToString(NewX2,8) + IntegerToString(NewY2,8)); {$EndIf}

          MapDraw.ScreenBoxToLatLongMinMax(newx1,Newy1,NewX2,NewY2,LatLow,LongLow,LatHigh,LongHigh);
          MapDraw.ScreenToDataGrid(NewX1,NewY1,xg1,yg1);
          MapDraw.ScreenToDataGrid(NewX2,NewY2,xg2,yg2);
          {$IfDef ExRedistrict}
          {$Else}
             if (DEMNowDoing = RecolorRedistrictBox) then begin
                GISdb[RedistrictForm.DBonTable].QueryBox(Self.MapDraw,NewX1,NewY1,NewX2,NewY2,false);
                RedistrictForm.DistrictsChanged;
                ChangeDEMNowDoing(RecolorRedistrictBox);
                exit;
             end;
          {$EndIf}

          if (DEMNowDoing in [DeleteMultipleDBRecs,DeleteMultipleRecsAllDBs]) then begin
             Image1.Canvas.CopyMode := cmSrcAnd;
             Image1.Canvas.Pen.Color := ConvertPlatformColorToTColor(MDDef.DBOutlineColor);
             Image1.Canvas.Pen.Width := MDDef.DBOutlineWidth;
             Image1.Canvas.Rectangle(NewX1,NewY1,NewX2,NewY2);
             for i := 1 to MaxDataBase do if ValidDB(i) then begin
                 if (DEMNowDoing in [DeleteMultipleRecsAllDBs]) and GISdb[i].ItsAPointDB then begin
                    GISdb[i].QueryGeoBox(LatHigh,LongLow,LatLow,LongHigh,true);
                    GISdb[i].DeleteAllSelectedRecords;
                 end
                 else if(GISdb[i].theMapOwner = Self) then begin
                    if (DEMNowDoing in [DeleteMultipleDBRecs]) then begin
                       GISdb[i].QueryGeoBox(LatHigh,LongLow,LatLow,LongHigh,true);
                       GISdb[i].DeleteAllSelectedRecords;
                    end;
                end;
                DoFastMapRedraw;
             end;
             if not AnswerIsYes('Delete another region') then BackToWandering;
             exit;
          end;

          {$IfDef ExMultiGrid}
          {$Else}
             if (DEMNowDoing = PickTrainingBox) then begin
                 if (MapDraw.MultiGridOnMap <> 0) then begin
                    MultiGridArray[MapDraw.MultiGridOnMap].AddTrainingBox(LatLow,LongLow,LatHigh,LongHigh);
                 end;
             end;
             if (DEMNowDoing = RegionDNs) then begin
                DEMGlb[MultiGridArray[MapDraw.MultiGridOnMap].IndexBand].LatLongDegreeToDEMGrid(LatLow,LongLow,xg1,yg1);
                DEMGlb[MultiGridArray[MapDraw.MultiGridOnMap].IndexBand].LatLongDegreeToDEMGrid(LatHigh,LongHigh,xg2,yg2);
                MultiGridArray[MapDraw.MultiGridOnMap].CreateAverageReflectanceGraph(round(xg1),round(yg1),round(xg2),round(yg2));
             end;
          {$EndIf}

         {$IfDef ExMrSID}
         {$Else}
          if (DEMNowDoing = OpenMrSid) and MapDraw.ValidSatOnMap then begin
             {$IfDef RecordMrSID} WriteLineToDebugFile('Image1MouseUp (DEMNowDoing = OpenMrSid)'); {$EndIf}
             if (SatImage[MapDraw.SatOnMap].RegVars.Registration = RegProjection) then begin
                {$IfDef RecordMrSID} WriteLineToDebugFile('Projected MrSID'); {$EndIf}
                SatImage[MapDraw.SatOnMap].ImageMapProjection.ForwardProjectRadians(LatHigh*DegToRad,LongLow*DegToRad,xg1,yg1);
                SatImage[MapDraw.SatOnMap].ImageMapProjection.ForwardProjectRadians(LatLow*DegToRad,LongHigh*DegToRad,xg2,yg2);
                fName := SubsetTiffFromSID(SatImage[MapDraw.SatOnMap].CurrentSidName,true,yg1,xg1,yg2,xg2,true);
             end
             else begin
                {$IfDef RecordMrSID} WriteLineToDebugFile('Lat/Long MrSID'); {$EndIf}
                fName := SubsetTiffFromSID(SatImage[MapDraw.SatOnMap].CurrentSidName,true,LatHigh,LongLow,LatLow,LongHigh,false);
             end;
             NewSatImage := OpenAndDisplayNewScene(nil,fName,true,true,true );
             SatImage[NewSatImage].CurrentSidName := SatImage[MapDraw.SatOnMap].CurrentSidName;
             SatImage[NewSatImage].SelectionMap.CheckProperTix;
             BackToWandering;
             exit;
          end;
          {$EndIf}

          if DEMNowDoing in [GraphFilterDB] then begin
             if ValidDB(DBEditting) then GISDB[DBEditting].QueryGeoBox(LatHigh,LongLow,LatLow,LongHigh,true);
             DBEditting := 0;
             BackToWandering;
             Exit;
          end;

          {$IfDef ExDataManip}
          {$Else}
             if (DEMNowDoing = CornerEditBox) then begin
                for i := 1 to MaxDEMDataSets do begin
                   if ValidDEM(i) and (DEMGlb[i].SelectionMap <> Nil) and (DEMGlb[i].SelectionMap.DEMEditForm <> Nil) then begin
                      DEMGlb[i].LatLongDegreeToDEMGrid(LatHigh,LongLow,xu,yu);
                      DEMGlb[i].SelectionMap.DEMEditForm.SubSetLeftCol := trunc(xu);
                      DEMGlb[i].LatLongDegreeToDEMGrid(LatLow,LongHigh,xu,yl);
                      DEMGlb[i].SelectionMap.DEMEditForm.SubsetBottRow := trunc((yl));
                      DEMGlb[i].SelectionMap.DEMEditForm.StringGrid1.ColCount := succ(round(xu) - DEMGlb[i].SelectionMap.DEMEditForm.SubSetLeftCol);
                      DEMGlb[i].SelectionMap.DEMEditForm.StringGrid1.RowCount := succ(round(yu-yl));
                      DEMGlb[i].SelectionMap.DEMEditForm.FillInGrid;
                   end;
                end {if};
                exit;
             end;
          {$EndIf}

          {$IfDef ExIndexes}
          {$Else}
             CheckIndexMouseUp(LatHigh,LongLow,LatLow,LongHigh);
          {$EndIf}

          if (DEMNowDoing = OpenMapsFromLibrary) and MDdef.AutoCloseIndexMaps then begin
             Self.Closable := true;
             Self.Close;
             Self := Nil;
          end;
       end;

       {$IfDef ExVectorOverlay}
       {$Else}
           if (DEMNowDoing = MoveMapBox) then begin
              NewX1 := NewX1 + (x - LastX);
              if (Newx1 < 0) then begin
                 NewX1 := 0;
                 NewX2 := DEMEditForm.StringGrid1.ColCount;
              end;
              Newx2 := NewX2 + (x - LastX);
              if (Newx2 > Image1.Width)  then NewX2 := Image1.Width;
              NewY1 := NewY1 + (Y - LastY);
              if (NewY1 < 0) then begin
                 NewY1 := 0;
                 NewY2 := DEMEditForm.StringGrid1.RowCount;
              end;
              NewY2 := NewY2 + (Y - LastY);
              if (Newy2 > Image1.Height) then NewY2 := Image1.Height;
              with DEMEditForm do begin
                 if (NewX2 - NewX1) < StringGrid1.ColCount then NewX1 := NewX2 - StringGrid1.ColCount;
                 if (NewY2 - Newy1) < StringGrid1.RowCount then Newy1 := Newy2 - StringGrid1.RowCount;
                 MapDraw.ScreenToDataGrid(NewX1,NewY1,xg,yg);
                 SubSetLeftCol := round(xg);
                 MapDraw.ScreenToDataGrid(NewX2,NewY2,xg,yg);
                 SubsetBottRow := round(yg);
                 if SubSetLeftCol + pred(StringGrid1.ColCount) > pred(DEMGlb[MapDraw.DEMonMap].DEMheader.NumCol) then
                    SubsetLeftCol := Pred(DEMGlb[MapDraw.DEMonMap].DEMheader.NumCol) - pred(StringGrid1.ColCount);
                 if SubSetBottRow + pred(StringGrid1.RowCount) > pred(DEMGlb[MapDraw.DEMonMap].DEMheader.NumRow) then
                    SubsetBottRow := Pred(DEMGlb[MapDraw.DEMonMap].DEMheader.NumRow) - pred(StringGrid1.RowCount);
                 FillInGrid;
              end;
           end;
       {$EndIf}
      ShowDefaultCursor;
   end;
end;


procedure TMapForm.Imagenegative1Click(Sender: TObject);
begin
   Imagenegative1.Checked := not Imagenegative1.Checked;
   MakeGraphicsFileNegative(MapDraw.BaseMapFName);
   DoFastMapRedraw;
end;

procedure TMapForm.Imagepalette1Click(Sender: TObject);
{$IfDef ExSat}
begin
{$Else}
var
   Palette : tStringList;
   i : integer;
begin
   Palette := tStringList.Create;
   Palette.Add('Position  Red  Green Blue');
   for i := 0 to 255 do with SatImage[MapDraw.SATonMap].DefinedImageColors[i] do
      Palette.Add(IntegerToString(i,3) + IntegerToString(rgbtRed,5) + IntegerToString(rgbtGreen,5) + IntegerToString(rgbtBlue,5));
   Petmar.DisplayAndPurgeStringList(Palette,SatImage[MapDraw.SATonMap].SceneBaseName + ' Palette');
{$EndIf}
end;


(*
procedure TMapForm.Timemaps1Click(Sender: TObject);

      procedure DrawMap(mt : tMapType);
      var
         MyBitmap : tMyBitmap;
         RightImage,LeftImage : PathStr;
      begin
          {$IfDef RecordTime} Stopwatch := TStopwatch.StartNew; {$EndIf}
          MapDraw.DeleteMapSavedLayers;
          if (mt = mtAnaglyph) then begin
             MapDraw.MapType := mtGrayReflect;
             MapDraw.DrawMapOnBMP(MyBitmap);
             MapDraw.AnaglyphBitmap(RightImage,LeftImage,MyBitmap);
          end
          else begin
             MapDraw.MapType := mt;
             MapDraw.DrawMapOnBMP(MyBitmap);
          end;
          Image1.Picture.Graphic := MyBitmap;
          MyBitmap.Free;
          {$IfDef RecordTime}
          Elapsed := Stopwatch.Elapsed;
          WriteLineToDebugFile(RealToString(Elapsed.TotalSeconds,-12,-4) + ' sec to draw ' + MapTypeName(mt));
          {$EndIf}
      end;

begin
   MDDef.MapTicks := tixNone;
   MDDef.DefaultMapXSize := 1024;
   MDDef.DefaultMapYSize := 1024;
   MDDef.FanSaveExt := '.png';

   DrawMap(mtGrayReflect);
   DrawMap(mtElevSpectrum);
   DrawMap(mtGrayReflect);
   DrawMap(mtIHSReflect);

   DrawMap(mtSlopeTrafficCats);
   DrawMap(mtDEMAspect);
   DrawMap(mtAnaglyph);
end;
*)

procedure TMapForm.Image1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
   i,FirstPoint,xpic,ypic,recsFound : integer;
   Lat,Long   : float64;
   TStr : ShortString;
begin
   if ClosingIsHappening then exit;
   if (DEMNowDoing = JustWandering) and (Button = mbLeft) then begin
      SX := X;  // X start co-ordinate, image panning
      SY := Y;  // Y start co-ordinate, image panning
      MouseIsDown := true;
      Forms.Screen.Cursor := crHandPoint;
      exit;
   end;

   MapDraw.ScreenToLatLongDegree(LastX,LastY,MouseDownLat,MouseDownLong);
   {$IfDef RecordMapDraw} WriteLineToDebugFile('Mouse down at: ' + LatLongDegreeToString(MouseDownLat,MouseDownLong)); {$EndIf}

   if (DEMNowDoing = Calculating) or (DEMRequiredForOperation(MapDraw.DEMonMap) and (MapDraw.DEMonMap = 0)) then exit;

   if (DEMNowDoing in [ErasingPoints,MoveMapBox,DigitizeContourStream]) then MouseIsDown := true;

   if (MDDef.DigitizeMode = dmStream) and (NowStreamDigitizing(DEMNowDoing)) then begin
      {$IfDef RecordStreamModeDigitize} WriteLineToDebugFile('TMapForm.Image1MouseDown  x=' + IntToStr(NewX1) + '  y=' + IntToStr(NewY1) + ' to x=' + IntToStr(x) + '  y=' + IntToStr(y)): {$EndIf}
      newx1 := x;
      Newy1 := y;
      Image1.Canvas.MoveTo(x,y);
      MouseIsDown := true;
   end;

   if (DBEditting <> 0) and (DEMNowDoing in [MovePointDBRecs]) then begin
      MapDraw.ScreenToLatLongDegree(LastX,LastY,Lat,Long);
      GISdb[DBEditting].IdentifyRecord(LastX,LastY,Lat,Long,RecsFound,false,false,TStr,false,true);
      if (RecsFound > 0) then begin
         {$IfDef RecordEditDB} WriteLineToDebugFile('MovePointDBRecs rec ID at ' + LatLongDegreeToString(Lat,Long)); {$EndIf}
         Forms.Screen.Cursor := crDrag;
         RecMoving := true;
         if (RecsFound > 1) then wmdem.SetPanelText(0,'Double click at new rec location');
      end
   end;

   if (DEMNowDoing in [EditFlightPathOnMap]) then begin
      //check to see if user wants to move a node
      i := 0;
      FirstPoint := i;
      for i := FirstPoint to pred(StreamProfileResults.Count) do begin
         ReadCoordsLatLongFromStreamProfileResults(StreamProfileResults,i,Lat,Long);
         MapDraw.LatLongDegreeToScreen(Lat,Long,xpic,ypic);
         if (abs(xpic - x) < 5) and  (abs(ypic - y) < 5) then begin
            if (i > FirstPoint) then begin
               ReadCoordsLatLongFromStreamProfileResults(StreamProfileResults,pred(i),Lat,Long);
               MapDraw.LatLongDegreeToScreen(Lat,Long,newx1,newy1);
            end
            else begin
               newx1 := MaxInt;
               Newy1 := MaxInt;
            end;
            if i < pred(StreamProfileResults.Count) then begin
               ReadCoordsLatLongFromStreamProfileResults(StreamProfileResults,succ(i),Lat,Long);
               MapDraw.LatLongDegreeToScreen(Lat,Long,newx2,newy2);
            end
            else begin
               newx2 := MaxInt;
               Newy2 := MaxInt;
            end;
            MouseDragging := true;
            EditPoint := i;
            exit;
         end;
      end;
      for i := succ(FirstPoint) to pred(StreamProfileResults.Count) do begin
         //check to see if user wants to insert a new node
         ReadCoordsLatLongFromStreamProfileResults(StreamProfileResults,pred(i),Lat,Long);
         MapDraw.LatLongDegreeToScreen(Lat,Long,Newx2,newy2);
         ReadCoordsLatLongFromStreamProfileResults(StreamProfileResults,i,Lat,Long);
         MapDraw.LatLongDegreeToScreen(Lat,Long,Newx1,newy1);
         if NearLine(Point(X,Y),Point(NewX1,NewY1),Point(NewX2,NewY2)) then begin
            MouseDragging := true;
            EditPoint := -(i);
            exit;
         end;
      end;
   end;

    if BoxOutlining(DEMNowDoing)  then begin
      MouseIsDown := true;
      Newx1 := x;
      Newy1 := y;
      Newx2 := x;
      Newy2 := y;
      if (DEMNowDoing = DigitizeContourStream) then begin
         Newx1 := 9999;
         Newy1 := 9999;
      end;
      if (WMDEM <> Nil) then WmDEM.SetPanelText(0, 'Drag to SE corner');
   end;
   if (Button = mbRight) then begin
      {$IfDef RawProjectInverse} DebugRawProjectInverse := true; {$EndIf}
      LastX := x;
      LastY := Y;
      MapDraw.ScreenToLatLongDegree(x,y,RightClickLat,RightClickLong);
      RespondToRightMouseButton;
   end;
end;


procedure TMapForm.CheckThatLegendsAreOnTop;
var
   Bitmap : tMyBitmap;
begin
   CopyImageToBitmap(Image1,Bitmap);
   MapDraw.DrawLegendsOnMap(Bitmap);
   Image1.Picture.Graphic := Bitmap;
   Bitmap.Free;
end;


procedure TMapForm.RespondToRightMouseButton;
var
   TStr : shortString;
begin
   if ClosingIsHappening then exit;
   RightClickX := LastX;
   RightClickY := LastY;

   {$IfDef RecordClick} WriteLineToDebugFile('Right mouse click'); {$EndIf}
   {$IfDef ExVectorOverlay}
   {$Else}
      if NowStreamDigitizing(DEMNowDoing) or (DEMNowDoing in [ShapeTrack,LaterZigDistance,SeekingStreamProfile,SeekingFlyThroughRoute]) then begin
         if (NowStreamDigitizing(DEMNowDoing) or (DEMNowDoing in [SeekingStreamProfile,SeekingFlyThroughRoute])) then
            EndPolyLine1.Caption := 'End selection';
         EndPolyLine1.Visible := PolyLineDigitizing(DEMNowDoing) or (DEMNowDoing in [SeekingStreamProfile,SeekingFlyThroughRoute]);
         ClosePolyLine1.Visible := PolygonDigitizing(DEMNowDoing);
         PointModeDigitizing1.Visible := NowStreamDigitizing(DEMNowDoing);
         PointModeDigitizing1.Checked := (MDDef.DigitizeMode = dmPoint);
         StreamDigitizing1.Visible := NowStreamDigitizing(DEMNowDoing);
         StreamDigitizing1.Checked := (MDDef.DigitizeMode = dmStream);
         EndLengthMeasurement1.Visible := DemNowDoing in [LaterZigDistance];
         EndTrack1.Visible := DemNowDoing in [ShapeTrack];
         {$IfDef ExGeoStats}
            Geomorphometry1.Visible := ShowGeomorphometry;
         {$Else}
            Geomorphometry1.Visible := false;
         {$EndIf}

         if MDdef.ClipboardExports = 0 then TStr := 'un' else TStr := '';
         Copytoclipboard1.Caption := 'Copy map to clipboard (' + TStr + 'collared)';
         StreamModeTolerance1.Caption := 'Stream mode tolerance, ' + IntToStr(MDDef.ContDigitizeSeparation);

         PopupMenu4.PopUp(Mouse.CursorPos.X,Mouse.CursorPos.Y);
         exit;
      end;
   {$EndIf}

   EditFan1.Visible := (MapDraw.CurrentFansTable <> 0);
   EditFans1.Visible := EditFan1.Visible;
   MapProjection1.Visible := (MapDraw.VectorIndex <> 0);
   DisplayParameter1.Visible := (MapDraw.VectorIndex = 0);
   Reflectanceoptions1.Visible := isReflectanceMap(MapDraw.MapType);

   MapShadingOptions1.Visible := ExpertDEMVersion;
   Terrainblowup1.Visible := ExpertDEMVersion and (MapDraw.MapOwner <> moPointVerificationMap);
   CreateDBs1.Visible := (MDdef.ProgramOption in [ExpertProgram]);
   GridGraticule1.Visible := (MDdef.ProgramOption in [ExpertProgram,GeographyProgram,RemoteSensingProgram]);
   Geomorphometry1.Visible := (MDdef.ProgramOption in [ExpertProgram]) and MDDef.ShowGeomorphometry;
   DEMsatpoint1.Visible := (MDdef.ProgramOption in [ExpertProgram,GeographyProgram]);
   Slopecategories2.Visible := isSlopeMap(MapDraw.MapType);
   ID2.Visible := IDSpeedButton.Visible;
   RemoteSensingPopupOptions.Visible := (MDdef.ProgramOption in [ExpertProgram,RemoteSensingProgram]);
   LSTfromemissivity1.Visible := ValidSatImage(MapDraw.SatOnMap) and (SatImage[MapDraw.SatOnmap].LandsatNumber <> 0);
   RemoteSensingPopupOptions.Visible := ValidSatImage(MapDraw.SatOnMap) or (MapDraw.MultiGridOnMap <> 0);

   Gridmaskcolor1.Visible := MapDraw.MapType = mtDEMMask;
   Opennessoptions1.Visible := MapDraw.MapType = mtOpenness;
   MonthlyWinds1.Visible := ValidDB(WindsDB);
   DrapecurrentmaptoOpenGL1.Visible := MapDraw.ValidDEMonMap;
   GridgraticluetoGoogleEarth1.Visible := MDDef.ShowCartography;
   Maplibrary1.Visible := (MDDef.ProgramOption = ExpertProgram) and MDDef.ShowIntDB;
   AnnotateMap1.Visible := (Not (MDdef.ProgramOption in [GeologyProgram])) and (not (MapDraw.MapOwner in [moIndexMap,moMapDatabase]));
   Elevationcolors1.Visible := IsElevationMap(MapDraw.MapType);
   Extractpointcloudreturns1.Visible := (pt_cloud_opts_fm <> Nil);

   if (MapDraw.PrimMapProj = Nil) then begin
      MergeColors1.Visible := (MapDraw.MapType in [mtMergeTwoDEMs]);
      {$IfDef ExSat}
      {$Else}
         if (not MapDraw.DEMMap) and MapDraw.ValidSatOnMap then begin
            Band1.Visible := ((SatImage[MapDraw.SatOnMap].NumBands > 1) and SatImage[MapDraw.SatOnMap].CanEnhance) or (MapDraw.MapType = mtSatBlank);
         end;
      {$EndIf}
   end;

   Abortcurrentoperation1.Visible := (DEMNowDoing <> JustWandering);
   Abortcurrentoperation2.Visible := (DEMNowDoing <> JustWandering);
   DrawMultipleFans1.Visible := (DEMNowDoing = QuickWeaponsFan) and (MDDef.FanPickMode in [fpMultipleSame,fpMultipleAsk]);
   Resoremenus1.Visible := not MDDef.ShowMenus;
   Copyimagecoordinatestoclipboard1.Visible := MapDraw.ValidSatOnMap;
   GridgraticluetoGoogleEarth1.Visible := MDDef.MapTicks <> tixNone;
   CopyVegprofiletoclipboard1.Visible := (VegGraph <> Nil);
   Aspectoptions1.Visible := ValidDEM(MapDraw.DEMonMap) and ((MapDraw.MapType in [mtDEMAspect]) or (DEMGlb[MapDraw.DEMonMap].DEMheader.ElevUnits in [AspectDeg]));

   PLSSlocation1.Visible := MDDef.ShowPLSS;
   errainsunblocking1.Visible := ValidDEM(MapDraw.DEMonMap);

   {$IfDef ExDriftModel}
      Driftmodels1.Visible := false;
   {$Else}
      Driftmodels1.Visible := MDDef.ShowOceanModels;
   {$EndIf}

   {$IfDef ExExoticMaps}
      DRGanaglyph1.Visible := false;
   {$Else}
      DRGanaglyph1.Visible := (MapDraw.ValidDEMonMap) and (MDdef.ProgramOption in [ExpertProgram,GeologyProgram]);
   {$EndIf}

   {$IfDef ExVegDensity}
      Vegetationlayers1.Visible := false;
   {$Else}
      Intervisibilitymap1.Visible := (MapDraw.ValidDEMonMap) and (DEMGlb[MapDraw.DEMonMap].VegGrid[1] <> 0);
      DSMfirstreturn1.Checked := MDDef.VegOptionMap = voDSM;
      HAGvegheight1.Checked := MDDef.VegOptionMap = voVEG;
      DTMlastreturn1.Checked := MDDef.VegOptionMap = voDTM;
      Vegetationlayers1.Visible := (MapDraw.ValidDEMonMap) and (DEMGlb[MapDraw.DEMonMap].VegDensityLayers[1]<> Nil);
   {$EndIf}

   {$IfDef ExTiger}
      Tiger1.Visible := false;
      ModifyTIGERdisplay1.Visible := false;
   {$Else}
      Tiger1.Visible := MapDraw.TigerSingleOverlayUp or OverlayUp(ovoTiger);
      ModifyTIGERdisplay1.Visible := MapDraw.TigerSingleOverlayUp or OverlayUp(ovoTiger);
   {$EndIf}

   {$IfDef ExGeography}
      AquastatClimate1.Visible := false;
      PhysicalGeography1.Visible := false;
   {$Else}
      AquastatClimate1.Visible := MDDef.ShowClimateAndLight;
      PhysicalGeography1.Visible :=(MDDef.ProgramOption in [ExpertProgram,GeographyProgram]);
   {$EndIf}

   {$IfDef ExGazetteer}
      Gazetteer1.Visible := false;
   {$Else}
      Gazetteer1.Visible := MDDef.UseGazetteer and (MDDef.ProgramOption in [ExpertProgram]);
   {$EndIf}

   {$IfDef ExKML}
      Googlemaps1.Visible := false;
   {$EndIf}

   {$IfDef ExIndexes}
      Integrateddatabasesetup1.Visible := false;
   {$Else}
      Integrateddatabasesetup1.Visible := (MapDraw.MapOwner in [moMapDatabase,moIndexMap]) or AllowMapLibraryLoads;
   {$EndIf}

   if (MDdef.ProgramOption = DragonPlotProgram) then begin
      MaptoGoogleEarth1.Caption := 'KML file export';
   end;

   {$IfDef ExPointCloud}
      PointcloudtoGoogleEarth1.Visible := false;
      PointcloudtoOpenGL1.Visible := false;
   {$Else}
      PointcloudtoGoogleEarth1.Visible := (pt_cloud_opts_fm <> nil);
      PointcloudtoOpenGL1.Visible := (pt_cloud_opts_fm <> nil);
      Pointcloudtodatabase1.Visible := (pt_cloud_opts_fm <> nil) and ExpertDEMVersion;
   {$EndIf}

   {$IfDef RecordProblems} WriteLineToDebugFile('SatOnMap=' + IntToStr(MapDraw.SatOnmap) + '   RS options=' + TrueOrFalse(RemoteSensingPopupOptions.Visible)); {$EndIf}

   {$IfDef ExWMS}
       WMSOpactiy1.Visible := false;
   {$Else}
       WMSOpactiy1.Visible := MapDraw.WMSLayerfName <> '';
    {$EndIf}

   BackToWandering;
   RightClickPopupMenu3.PopUp(Mouse.CursorPos.X,Mouse.CursorPos.Y);
end;


procedure TMapForm.GetPositionData(var Length,Heading : float64; var Tstr2 : ShortString; DontVerify : boolean = false);
var
   Check : tCheckPoint;
   Lat1,Long1,Lat2,Long2 : float64;
begin
   {$IfDef MeasureDistance} WriteLineToDebugFile('TMapForm.GetPositionData in'); {$EndIf}
   if (DontVerify or (MDdef.CheckPoint = CheckNothing)) then Check := CheckNothing else Check := CheckReasonable;
   CheckThisPoint(LastX,LastY,xDEMg2,yDEMg2,xSATg2,ySATg2,Check);
   if (MapDraw.VectorIndex <> 0) then begin
      Lat1 := yDEMg1;
      Long1 := xDEMg1;
      Lat2 := yDEMg2;
      Long2 := xDEMg2;
   end
   else begin
      if MapDraw.ValidDEMonMap then begin
         DEMGlb[MapDraw.DEMonMap].DEMGridToLatLongDegree(xDEMg1,yDEMg1,Lat1,Long1);
         DEMGlb[MapDraw.DEMonMap].DEMGridToLatLongDegree(xDEMg2,yDEMg2,Lat2,Long2);
      end
      else begin
         {$IfDef ExSat}
         {$Else}
         SatImage[MapDraw.SATonMap].SatGridToLatLongDegree(SatImage[MapDraw.SatOnMap].BandForSize,xSatg1,ySatg1,Lat1,Long1);
         SatImage[MapDraw.SATonMap].SatGridToLatLongDegree(SatImage[MapDraw.SatOnMap].BandForSize,xSatg2,ySatg2,Lat2,Long2);
         {$EndIf}
      end;
   end;
   VincentyCalculateDistanceBearing(Lat1,Long1,Lat2,Long2,Length,Heading);

   if MapDraw.DEMMap and (DEMGlb[MapDraw.DEMonMap].DEMheader.DEMUsed = ArcSecDEM) and (DEMGlb[MapDraw.DEMonMap].LongSizeMap > 2.5) then TStr2 := ''
   else StrikeIntoString(Round(Heading+90),TStr2,false);
   {$IfDef MeasureDistance} WriteLineToDebugFile('TMapForm.GetPositionData out ' + TStr2); {$EndIf}
end;

procedure TMapForm.BackToWandering;
begin
   MouseIsDown := false;
   ChangeDEMNowDoing(JustWandering);
end;

procedure TMapForm.Print1Click(Sender: TObject);
begin
   PreparePrinterImage1Click(Sender);
end;


procedure TMapForm.Print2Click(Sender: TObject);
begin
   PreparePrinterImage1Click(Sender);
end;

procedure TMapForm.Saveimage1Click(Sender: TObject);
begin
   PetImage.SaveImageAsBMP(Image1);
end;


procedure TMapForm.Concatenateimages1Click(Sender: TObject);
{$IfDef ExOTB}
begin
{$Else}
var
   InNames: tStringList;
   OutName : PathStr;
   i : integer;
begin
   if ValidSatImage(MapDraw.SatOnMap) then begin
      InNames := tStringList.Create;
      for i := 1 to SatImage[MapDraw.SatOnMap].NumBands do
         if SatImage[MapDraw.SatOnMap].IsLandsatImageAnalysisBand(i) then
            InNames.Add(SatImage[MapDraw.SatOnMap].TiffImage[i].TiffFileName);
      OutName := ExtractFilePath(SatImage[MapDraw.SatOnMap].TiffImage[1].TiffFileName) + 'merge.tif';
      OTB_ConcatenateImages(InNames,OutName);
   end;
{$EndIf}
end;

procedure TMapForm.Kmeansclustering1Click(Sender: TObject);
{$IfDef ExOTB}
begin
{$Else}
var
   InName,OutName : PathStr;
begin
   {$If Defined(RecordOTB)} WriteLineToDebugFile('TMapForm.Kmeansclustering1Click in');{$EndIf}
   InName := ExtractFilePath(SatImage[MapDraw.SatOnMap].TiffImage[1].TiffFileName) + 'merge.tif';
   if not FileExists(InName) then begin
      {$If Defined(RecordOTB)} WriteLineToDebugFile('Concatenate to ' + InName);{$EndIf}
      Concatenateimages1Click(Sender);
   end;
   OutName := ExtractFilePath(SatImage[MapDraw.SatOnMap].TiffImage[1].TiffFileName) + 'merge_cluster.tif';
   {$If Defined(RecordOTB)} WriteLineToDebugFile('try to k-means to ' + OutName);{$EndIf}
   OTB_KMeansClassification(InName, OutName);
   if FileExists(OutName) then begin
      OpenNewDEM(OutName);
   end
   else begin
      {$If Defined(RecordOTB)} WriteLineToDebugFile('failed creation, ' + OutName);{$EndIf}
   end;
{$EndIf}
end;

procedure TMapForm.Segmentation1Click(Sender: TObject);
{$IfDef ExOTB}
begin
{$Else}
var
   InName,OutName : PathStr;
begin
   InName := ExtractFilePath(SatImage[MapDraw.SatOnMap].TiffImage[1].TiffFileName) + 'merge.tif';
   OutName := ExtractFilePath(SatImage[MapDraw.SatOnMap].TiffImage[1].TiffFileName) + 'merge_segment.tif';
   OTB_Segmentation(InName, OutName);
   if FileExists(OutName) then OpenNewDEM(OutName);
   {$EndIf}
end;


procedure TMapForm.Continentalcrust1Click(Sender: TObject);
var
   db : integer;
begin
   {$IfDef ExGeology}
   {$Else}
      db := LoadDataBaseFile(ContCrustOutlineFile);
      if ValidDB(db) then begin
         GISdb[db].dbOpts.LineColor := ConvertTColorToPlatformColor(clBrown);
         GISdb[db].RedrawLayerOnMap;
      end;
   {$EndIf}
end;


procedure TMapForm.Contour1Click(Sender: TObject);
begin
   MapDraw.MapType := mtDEMContour;
   MapDraw.SingleContourColor := false;
   Contourinterval1Click(Sender);
end;

procedure TMapForm.Contour2Click(Sender: TObject);
var
   BitMap  : tMyBitmap;
begin
   {$If Defined(RecordMapDraw) or Defined(RecordContour)} WriteLineToDebugFile('TMapForm.Contour2Click Overlay contours in, interval=' + IntToStr(MapDraw.MapOverlays.ConInt));{$EndIf}
   MDdef.DefaultContourInterval := MapDraw.MapOverlays.ConInt;
   ContourOptions := TContourOptions.Create(Application);
   ContourOptions.MapOwner := Self;
   ContourOptions.ExpressContourOptions;
   (*
   if (ContourOptions.ShowModal <> idCancel) then begin
      CheckEditString(ContourOptions.Edit1.Text, MDdef.DefaultContourInterval);
      MapDraw.MapOverlays.ConInt := MDdef.DefaultContourInterval;
      MDDef.LabelContours := ContourOptions.CheckBox1.Checked;
   end;
   *)
   ContourOptions.Free;
   MapDraw.MapOverlays.ConInt := MDdef.DefaultContourInterval;
   {$If Defined(RecordMapDraw) or Defined(RecordContour)} WriteLineToDebugFile('TMapForm.Contour2Click new interval=' + IntToStr(MapDraw.MapOverlays.ConInt));{$EndIf}
   if (Sender <> Nil) then begin
      CopyImageToBitmap(Image1,Bitmap);
      if (Sender = Contoursfromsecondgrid1) then begin
         MapDraw.DrawContoursInArea(BitMap,MapDraw.MapOverlays.ConInt,MapDraw.DEM2onMap);
      end
      else begin
         AddOrSubtractOverlay(Self,ovoContours,True);
         MapDraw.OverlayContours(Bitmap);
      end;
      Image1.Picture.Graphic := Bitmap;
      BitMap.Free;
   end;
   MapDraw.DeleteSingleMapLayer(MapDraw.ContourOverlayfName);
   MapDraw.DeleteSingleMapLayer(MapDraw.ContourOverlayfName2);
   DoFastMapRedraw;
end;

procedure TMapForm.Distance1Click(Sender: TObject);
begin
   {$IfDef MeasureDistance} WriteLineToDebugFile('Distance1Click'); {$EndIf}
   ChangeDEMNowDoing(FirstDistancePoint);
end;

procedure TMapForm.Dividezvalues1Click(Sender: TObject);
begin
   Multiplyzvalues1Click(Sender);
end;

procedure TMapForm.dNBRNBRbeforeandafterfire1Click(Sender: TObject);
begin
   MakeDNBRMap(0,0);
end;


procedure TMapForm.Slope2Click(Sender: TObject);
begin
   ChangeDEMNowDoing(FirstSlopePoint);
end;

procedure TMapForm.Forcecompleteredraw1Click(Sender: TObject);
begin
   DoCompleteMapRedraw;
end;


procedure TMapForm.Forceredraw1Click(Sender: TObject);
begin
   DoFastMapRedraw;
end;

procedure TMapForm.Forceredrawbasemaplayer1Click(Sender: TObject);
begin
   DoBaseMapRedraw;
end;

procedure TMapForm.Forceredrawlegendsscalebars1Click(Sender: TObject);
begin
   MapDraw.DeleteSingleMapLayer(MapDraw.LegendOverlayfName);
   if (Sender <> Nil) then DoFastMapRedraw;
end;


procedure TMapForm.AgeFromDepth1Click(Sender: TObject);
begin
   {$IfDef ExGeology}
   {$Else}
      AgeFromDepth1.Checked := not AgeFromDepth1.Checked;
      if (wmDEM <> Nil) and (not LockStatusBar) then WmDEM.StatusBar1.Panels[2].Text := '';
   {$EndIf}
end;

procedure TMapForm.ailpercentiles1Click(Sender: TObject);
begin
   {$IfDef ExMultigrid}
   {$Else}
      MultiGridArray[MapDraw.MultiGridOnMap].KeyPercentiles;
   {$EndIf}
end;


procedure TMapForm.Terraincategories1Click(Sender: TObject);
{$IfDef ExVectorOverlay}
begin
{$Else}
var
   TerrainCategory : tTerrainCatDefinition;
begin
   with MapDraw do begin
      if (MapOverlays.ovTerrainCat.Count = 0) then begin
         DEMGlb[DEMonMap].InitializeTerrainCategory(TerrainCategory);
         MapOverlays.ovVectorFiles.Add('Terrain.Cat');
         AddOverlay(Self,ovoVectors);
      end
      else TerrainCategory := StringToTerrainCategory(MapOverlays.ovTerrainCat.Strings[pred(MapOverlays.ovTerrainCat.Count)]);
      GetTerrainCategory(tcNormal,Self,DEMonMap,TerrainCategory,DEMGlb[DEMonMap].ElevationDEM);
   end;
{$EndIf}
end;


procedure TMapForm.Reflectance2Click(Sender: TObject);
begin
   ChangeReflectanceOptions(Self)
end;

procedure TMapForm.ContourBands1Click(Sender: TObject);
begin
   if MapDraw.DEMMap then MapDraw.MapType := mtDEMBlank
   else MapDraw.MapType := mtSatBlank;
   ReadDefault('Contour interval',MapDraw.MapOverlays.ConInt);
   DrawColoredMap1Click(Nil);
end;

procedure TMapForm.Bearing1Click(Sender: TObject);
begin
   ChangeDEMNowDoing(FirstBearingPoint);
end;


procedure TMapForm.BestglobalDEM1Click(Sender: TObject);
var
   Col,Row,DEM,BestDEM,NewDEM,db,Code : integer;
   Lat,Long : float64;
   z,z2,BestDiff : float32;
   NewHeadRecs : tDEMHeader;
   fName : PathStr;
begin
   LoadDEMsCoveringBox(MapDraw.MapCorners.BoundBoxGeo,true);
   NewHeadRecs := DEMGlb[MapDraw.DEMonMap].DEMheader;
   NewHeadRecs.DEMPrecision := ByteDEM;
   fName := MDtempDir + 'best_dem.dem';
   OpenAndZeroNewDEM(true,NewHeadRecs,NewDEM,fName,InitDEMMissing);

   StartProgress('Best DEM');
   for Col := 0 to pred(DEMGlb[MapDraw.DEMonMap].DEMheader.NumCol) do begin
      UpdateProgressBar(Col/DEMGlb[MapDraw.DEMonMap].DEMheader.NumCol);
      for Row := 0 to pred(DEMGlb[MapDraw.DEMonMap].DEMheader.NumRow) do begin
         if DEMGlb[MapDraw.DEMonMap].GetElevMetersOnGrid(Col,Row,z) then begin
            DEMGlb[MapDraw.DEMonMap].DEMGridToLatLongDegree(Col,Row,Lat,Long);
            BestDiff := 99999;
            BestDEM := 0;
            for DEM := 1 to MaxDEMDataSets do begin
               if ValidDEM(DEM) and (DEM <> MapDraw.DEMonMap) and (DEM <> NewDEM) then begin
                  DEMGlb[DEM].GetElevFromLatLongDegree(Lat,Long,z2);
                  if abs(z-z2) < BestDiff then begin
                     BestDiff := abs(z-z2);
                     BestDEM := DEM;
                  end
                  else if abs(abs(z-z2) - BestDiff) < 0.01 then begin
                     if BestDEM >= 12 then inc(BestDEM)
                     else BestDEM := 12;
                  end;
               end;
            end;
            DEMGlb[NewDEM].SetGridElevation(Col,Row,BestDEM);
         end;
      end;
   end;
   DEMGlb[NewDEM].AreaName := 'best_dem';
   DEMGlb[NewDEM].WriteNewFormatDEM(fName);
   DEMGlb[NewDEM].CreateVATforDEM(false);

   db := DEMGlb[MapDraw.DEMonmap].SelectionMap.LoadDataBaseFile(DEMGlb[NewDEM].VATFileName);
      GISdb[db].MyData.First;
      while Not GISdb[db].MyData.eof do begin
         Code := GISdb[db].MyData.GetFieldByNameAsInteger('VALUE');
         GISdb[db].MyData.Edit;
         if ValidDEM(Code) then begin
            GISdb[db].MyData.SetFieldByNameAsString('NAME', DEMGlb[Code].AreaName);
         end
         else begin
            GISdb[db].MyData.SetFieldByNameAsString('NAME','Ties=' + IntToStr(Code-10));
         end;
         GISdb[db].MyData.SetFieldByNameAsInteger('COLOR',WinGraphColors[Code mod 15]) ;
         GISdb[db].MyData.Next;
      end;
   CloseAndNilNumberedDB(db);

   DEMGlb[NewDEM].SetUpMap(NewDEM,true);
   EndProgress;
end;



procedure TMapForm.ShowSubsetOnMap(x1,y1,x2,y2: integer);
begin
   MapDraw.DataGridToScreen(x1,y1,Newx1,Newy1);
   MapDraw.DataGridToScreen(x2,y2,Newx2,Newy2);
   Image1.Canvas.Pen.Mode := pmNotXor;
   Image1.Canvas.Pen.Color := clRed;
   Image1.Canvas.Pen.Width := 4;
   Image1.Canvas.Rectangle(newx1,newy1,newx2,newy2);
end;


procedure TMapForm.SideScanButtonClick(Sender: TObject);
{$IfDef ExSidescan}
begin
{$Else}
begin
   if (SidescanIndexFName = '') then begin
      Loadsidescanimagery1Click(Sender);
   end
   else begin
     if ValidDB(SideIndexDB) then begin
        GISdb[SideIndexDB].DBTablef.Picklinefromfile1Click(Sender);
     end
     else begin
        SideIndexDB := LoadDataBaseFile(SidescanIndexFName);
        if (SideIndexDB = 0) then begin
           SidescanIndexFName := '';
        end
        else begin
           GISdb[SideIndexDB].MyData.ApplyFilter('USE=' + QuotedStr('Y'));
           GISdb[SideIndexDB].dbTablef.ShowStatus;
           DoFastMapRedraw;
           //SideIndexOpen := true;
        end;
     end;
     ChangeDEMNowDoing(JustWandering);
   end;
{$EndIf}
end;


procedure TMapForm.SubbottomSpeedButtonClick(Sender: TObject);
begin
   {$IfDef ExSidescan}
   {$Else}
      StartChirps(Self);
   {$EndIf}
end;


procedure TMapForm.Subset1Click(Sender: TObject);
begin
   if LockMaps then begin
      MessageToContinue('Maps locked for digitizing');
   end
   else begin
      {$IfDef RecordSubsetZoom} WriteLineToDebugFile('TMapForm.Subset1Click'); {$EndIf}
      DEMDoingWhatBefore := DEMNowDoing;
      ChangeDEMNowDoing(NewCoverage);
   end;
end;


procedure TMapForm.Subsetgraphicalzoom1Click(Sender: TObject);
begin
   Subset1Click(Sender);
end;


procedure TMapForm.Forcesize1Click(Sender: TObject);
begin
   if LockMaps then begin
      MessageToContinue('Maps locked for digitizing');
   end
   else if GetNewBMPSize(MDdef.DefaultMapXSize, MDdef.DefaultMapYSize,'' ) then begin
      Top := 0;
      Left := 0;
      MapDraw.MapDrawValid := false;
      MapDraw.MapXSize := MDdef.DefaultMapXSize;
      MapDraw.MapYSize := MDdef.DefaultMapYSize;
      FormResize(Nil);
      DrawColoredMap1Click(Nil);
   end;
end;


procedure TMapForm.ForGoogleEarth1Click(Sender: TObject);
begin
   ExportMapToGoogleEarth(false);
end;


procedure TMapForm.ExportMapToGoogleEarth(MergeAll : boolean);
const
   KMLReproj = ', KML Geo reprojection';
var
   Bitmap,Bitmap2 : tMyBitmap;
   x,y,xp,yp,zf,x1,y1,x2,y2 : integer;
   DesiredPixelSize,
   Dist,Dist2,Bear,Lat,Long : float64;
   NewMap,i : integer;
   LatHi,LongHi,LatLow,LongLow : float64;

         (*
         procedure InitRange;
         begin
            LatHi := -999;
            LongHi := -999;
            LatLow := 9999;
            LongLow := 9999;
         end;
         *)

         procedure RangeLimits(i : integer);
         begin
             {$IfDef ExSat}
             {$Else}
                if (i > 0) and (SatImage[i] <> Nil) then begin
                   SatImage[i].SatGridToLatLongDegree(SatImage[MapDraw.SatOnMap].BandForSize,0,0,Lat,Long);
                   if Lat > LatHi Then LatHi := Lat;
                   if Long < LongLow then LongLow := Long;
                   SatImage[i].SatGridToLatLongDegree(SatImage[MapDraw.SatOnMap].BandForSize,SatImage[i].NumSatCol,SatImage[i].NumSatRow,Lat,Long);
                   if Lat < LatLow Then LatLow := Lat;
                   if Long > LongHi then LongHi := Long;
                end;
             {$EndIf}
         end;

begin
   {$IfDef RecordKMLexport} WriteLineToDebugFile('ExportMapToGoogleEarth in, ' + Caption + ' MergeAll=' + TrueOrFalse(MergeAll) + ' KMLCompat=' + TrueOrFalse(MapDraw.KMLcompatibleMap)); {$EndIf}
   if MergeAll and MapDraw.KMLcompatibleMap then begin
      {$IfDef RecordKMLexport} WriteLineToDebugFile('(Sender <> Mergeallimages1) and MapDraw.KMLcompatibleMap'); {$EndIf}
      LoadMapInGoogleEarth(Caption,Self);
   end
   else begin
      if (MergeAll) then begin
         {$IfDef ExSat}
         {$Else}
            InitLatLongBoundBox(LatLow,LongLow,LatHi,LongHi);
            for i := 1 to MaxSatAllowed do if (SatImage[i] <> Nil) then RangeLimits(i);
            VincentyCalculateDistanceBearing(LatHi,LongLow,LatHi,LongHi,Dist,Bear);
            VincentyCalculateDistanceBearing(LatHi,LongLow,LatHi,LongHi,Dist2,Bear);
            DesiredPixelSize := MapDraw.ScreenPixelSize;
            zf := 100;
            if MDDef.AskAboutKMLExport then begin
               ReadDefault('Output pixel size',DesiredPixelSize);
               ReadDefault('Zoom factor for resampling',zf);
            end;

            NewMap := MakeNewBlankDEMMap(LatLow,LongLow,LatHi-LatLow,LongHi-LongLow,round(Dist/DesiredPixelSize),round(Dist2/DesiredPixelSize));
            CloneImageToBitmap(DEMGlb[NewMap].SelectionMap.Image1,Bitmap);
            NakedMapOptions;  //which calls SaveBackupDefaults;
            for i := 1 to MaxSatAllowed do if (SatImage[i] <> Nil) then begin
               //InitRange;
               InitLatLongBoundBox(LatLow,LongLow,LatHi,LongHi);

               RangeLimits(i);
               SatImage[i].SelectionMap.ResizeByPercentage(ZF{,true});
               CopyImageToBitmap(SatImage[i].SelectionMap.Image1,Bitmap2);
               StartProgress('Image ' + IntToStr(i));
               DEMGlb[NewMap].SelectionMap.MapDraw.LatLongDegreeToScreen(LatLow,LongLow,x1,y2);
               DEMGlb[NewMap].SelectionMap.MapDraw.LatLongDegreeToScreen(LatHi,LongHi,x2,y1);
               for y := y1 to y2 do begin
                  UpdateProgressBar(y/Bitmap.Height);
                  for x := x1 to x2 do begin
                     if Bitmap.Canvas.Pixels[x,y] = clWhite then begin
                        DEMGlb[NewMap].SelectionMap.MapDraw.ScreenToLatLongDegree(x,y,Lat,Long);
                        SatImage[i].SelectionMap.MapDraw.LatLongDegreeToScreen(Lat,Long,xp,yp);
                        if SatImage[i].SelectionMap.MapDraw.OnScreen(xp,yp) then Bitmap.Canvas.Pixels[x,y] := Bitmap2.Canvas.Pixels[xp,yp];
                     end;
                  end;
               end;
               SatImage[i].SelectionMap.RedrawMapDefaultsSize;
               Bitmap2.Free;
            end;
            RestoreBackupDefaults;
            SaveMapasGEOTIFFFile('',false);
         {$EndIf}
      end
      else begin
         if MapDraw.KMLcompatibleMap then begin
            {$IfDef ExKML}
            {$Else}
               LoadMapInGoogleEarth(Self.Caption,Self);
            {$EndIf}
         end
         else begin
             NewMap := MakeNewBlankDEMMap(MapDraw.MapCorners.BoundBoxGeo.ymin,MapDraw.MapCorners.BoundBoxGeo.xmin,
                MapDraw.MapCorners.BoundBoxGeo.ymax-MapDraw.MapCorners.BoundBoxGeo.ymin,MapDraw.MapCorners.BoundBoxGeo.xmax-MapDraw.MapCorners.BoundBoxGeo.xmin,MapDraw.MapXSize,MapDraw.MapYSize);

           CloneImageToBitmap(DEMGlb[NewMap].SelectionMap.Image1,Bitmap);
           CopyImageToBitmap(Image1,Bitmap2);

           {$IfDef RecordKMLexport}
              WriteLineToDebugFile('TMapForm.ForGoogleearth1Click start reproject');
              if (DEMGlb[NewMap].DEMMapProjection = Nil) then WriteLineToDebugFile('Nil vector dem map projection') else WriteLineToDebugFile('Defined vector dem map projection (PROBLEM)');
           {$EndIf}

           StartProgress('Reproject');
           for y := 0 to pred(Bitmap.Height) do begin
              UpdateProgressBar(y/Bitmap.Height);
              for x := 0 to pred(Bitmap.Width) do begin
                 DEMGlb[NewMap].SelectionMap.MapDraw.ScreenToLatLongDegree(x,y,Lat,Long);
                 MapDraw.LatLongDegreeToScreen(Lat,Long,xp,yp);
                 if MapDraw.OnScreen(xp,yp) then Bitmap.Canvas.Pixels[x,y] := Bitmap2.Canvas.Pixels[xp,yp];
              end;
           end;
           {$IfDef RecordKMLexport} WriteLineToDebugFile('TMapForm.ForGoogleearth1Click end reproject'); {$EndIf}
           Bitmap2.Free;

          DEMGlb[NewMap].SelectionMap.Image1.Picture.Graphic := Bitmap;
          DEMGlb[NewMap].SelectionMap.Caption := Self.Caption;
          Bitmap.Free;
          EndProgress;
          {$IfDef ExKML}
          {$Else}
             LoadMapInGoogleEarth(DEMGlb[NewMap].SelectionMap.Caption,DEMGlb[NewMap].SelectionMap);
          {$EndIf}
          DEMGlb[NewMap].SelectionMap.Close;
        end;
      end;
   end;
   {$IfDef RecordKMLexport} WriteLineToDebugFile('ExportMapToGoogleEarth out'); {$EndIf}
end;


procedure TMapForm.Exportvaluesforallopengrids1Click(Sender: TObject);
label
   MissingData;
var
   x,y,i : integer;
   Lat,Long : float64;
   z : float32;
   Results : tStringList;
   aLine : ANSIstring;
begin
   ShowHourglassCursor;
   Results := tStringList.Create;
   aline := 'LAT,LONG';
   for i := 1 to MaxDEMDataSets do begin
      if ValidDEM(i) then begin
         aline := aline + ',' + DEMGlb[i].AreaName;
      end;
   end;
   Results.Add(aline);

   for x := round(MapDraw.MapCorners.BoundBoxDataGrid.xmin) to round(MapDraw.MapCorners.BoundBoxDataGrid.xmax) do begin
      for y := round(MapDraw.MapCorners.BoundBoxDataGrid.ymin) to round(MapDraw.MapCorners.BoundBoxDataGrid.ymax) do begin
         DEMGlb[MapDraw.DEMonMap].DEMGridToLatLongDegree(x,y,Lat,Long);
         aLine := RealToString(Lat,-12,-7) + ',' + RealToString(Long,-12,-7);
         for i := 1 to MaxDEMDataSets do begin
            if ValidDEM(i) then begin
               if DEMGlb[i].GetElevFromLatLongDegree(Lat,Long,z) then begin
                  aLine := aLine + ',' + RealToString(z,-12,-2);
               end
               else GoTo MissingData;
            end;
         end;
         Results.Add(aline);
        MissingData:;
      end;
   end;
   ShowDefaultCursor;
   StringListToLoadedDatabase(Results,  Petmar.NextFileNumber(MDTempDir, 'dem_export_',DefaultDBExt));
end;

procedure TMapForm.Offcurrentmap1Click(Sender: TObject);
begin
    ColumnsEastLimit1Click(Sender);
end;

procedure TMapForm.Offset1Click(Sender: TObject);
{$IfDef ExVectorOverlay}
begin
{$Else}
var
   i : integer;
begin
   ReadDefault('Number of offsets',MDDef.NumOffsets);
   if (MDDef.NumOffsets > 5) then MDDef.NumOffsets := 5;
   ReadDefault('Offset distance (m)',MDDef.OffsetDistance);
   for i := 1 to MDDef.NumOffsets do ReadDefault('Bearing (' + DegSym + ') #' + IntToStr(i),MDDef.OffsetBearings[i]);
   Petmar.PickLineSizeAndColor('Offsets',Nil,MDDef.OffsetColor,MDDef.OffsetLineWidth);
   ChangeDEMNowDoing(PlottingOffset);
{$EndIf}
end;


procedure TMapForm.oGeographic1Click(Sender: TObject);
begin
   ConvertUKOSDEMtoUTM(MapDraw.DEMonMap,false);
end;

procedure TMapForm.Tools1Click(Sender: TObject);
begin
   {$IfDef MICRODEM}
   wmdem.Tools1Click(Sender);
   {$EndIf}
end;

{$IfDef ExTiger}
{$Else}
procedure TMapForm.TigerRoadMask(RoadProximity : integer; NearRoadsMask : boolean; DBNum : integer = 0; PC : integer = 100);
var
   RoadMaskBMP : tMyBitmap;
   i,x,y : integer;
   Caption : shortString;
   Col : tColor;
   ch : AnsiChar;
begin
   if (RoadProximity < 0) then begin
      RoadProximity := 100;
      ReadDefault('Buffer distance from road (m)',RoadProximity);
   end;

   Caption := Self.Caption;

   if (PC > -999) then ResizeByPercentage(PC {,true,true});

   CloneImageToBitmap(Image1,RoadMaskBMP);
   MapDraw.DrawFullTigerCoverage(RoadMaskBMP,true,true,false,RoadProximity);

   StartProgress('Mask road');
   if (dbNum = 0) then begin
      if NearRoadsMask then MakeBitmapNegative(RoadMaskBMP,RGBTripleBlack);

      Image1.Picture.Graphic := RoadMaskBMP;

      if MDDef.ShowMasks then Petimage_form.DisplayBitmap(RoadMaskBMP,'Mask');

      EditGridViaColor(emvcBasicMask,clBlack);
      DoBaseMapRedraw;
   end
   else with GISdb[DBNum] do begin
      EmpSource.Enabled := false;
      if NearRoadsMask then Col := clBlack
      else Col := clWhite;
      AddFieldToDataBase(ftString,'USE',1,0);
      i := 0;
      MyData.First;
      while not MyData.EOF do begin
         inc(i);
         if (i Mod 50 = 0) then UpDateProgressBar(i/MyData.FiltRecsInDB);
         MapDraw.LatLongDegreeToScreen(MyData.GetFieldByNameAsFloat(LatFieldName),MyData.GetFieldByNameAsFloat(LongFieldName),x,y);
         if (RoadMaskBMP.Canvas.Pixels[x,y] = Col) then ch := 'Y' else ch := 'N';
         MyData.Edit;
         MyData.SetFieldByNameAsString('USE',ch);
         MyData.Next;
      end;
      EmpSource.Enabled := true;
   end;
   FreeAndNil(RoadMaskBMP);
   EndProgress;
end;
{$EndIf}



procedure TMapForm.Contourinterval1Click(Sender: TObject);
begin
   MDdef.DefaultContourInterval := MapDraw.MapOverlays.ConInt;
   ContourOptions := TContourOptions.Create(Application);
   ContourOptions.MapOwner := Self;
   ContourOptions.Label4.Caption := ElevUnitsAre(DEMGlb[MapDraw.DEMonMap].DEMheader.ElevUnits);

   if (ContourOptions.ShowModal <> idCancel) then begin
      case DEMGLB[MapDraw.DEMonMap].DEMheader.ElevUnits of
         Decimeters : MDdef.DefaultContourInterval := (MDdef.DefaultContourInterval * 10);
         Centimeters : MDdef.DefaultContourInterval := (MDdef.DefaultContourInterval * 100);
      end;
      {$IfDef RecordMapDraw} WriteLineToDebugFile('TMapForm.Contourinterval1Click contour interval=' + IntToStr(MDdef.DefaultContourInterval)): {$EndIf}
      MapDraw.MapOverlays.ConInt := MDdef.DefaultContourInterval;
      DoCompleteMapRedraw;
   end;
   ContourOptions.Free;
end;


procedure TMapForm.Referencegridinfile1Click(Sender: TObject);
begin
   FillHoles(hfOnlyHole);
end;


procedure TMapForm.FillHoles(ModeWanted : tHoleFill);
var
   i,MergeDEM : integer;
   DefFilter : byte;
   FilesWanted : tStringList;
   fName : PathStr;
begin
   DefFilter := 1;
   FilesWanted := tStringList.Create;
   FilesWanted.Add(ExtractFilePath(LastDEMName));
   if Petmar.GetMultipleFiles('DEMs for hole filling','DEMs|*.*',FilesWanted,DefFilter) then begin
      for i  := 0 to pred(FilesWanted.Count) do begin
         fName := FilesWanted.Strings[i];
         if LoadNewDEM(MergeDEM,fName,(FilesWanted.Count = 1),'','',false) then begin
            DEMGlb[MapDraw.DEMonMap].FillHolesSelectedBoxFromReferenceDEM(DEMGlb[MapDraw.DEMonMap].FullDEMGridLimits,MergeDEM,ModeWanted);
            CloseSingleDEM(MergeDEM);
         end;
      end;
      if (FilesWanted.Count > 1) and AnswerIsYes('Save new DEM in all input DEMs') then begin
         for i  := 0 to pred(FilesWanted.Count) do begin
            fName := FilesWanted.Strings[i];
            DEMGlb[MapDraw.DEMonMap].WriteNewFormatDEM(fName);
         end;
      end;
   end;
   FilesWanted.Free;
   RespondToChangedDEM;
end;


procedure TMapForm.Fillwithmodefilter1Click(Sender: TObject);
begin
   ModeFilterPopupMenu.PopUp(Mouse.CursorPos.X,Mouse.CursorPos.Y);
end;

procedure TMapForm.Reflectance1Click(Sender: TObject);
begin
   ChangeReflectanceOptions(Self);
end;


procedure TMapForm.Blank1Click(Sender: TObject);
begin
   if MapDraw.DEMMap then MapDraw.MapType := mtDEMBlank else MapDraw.MapType := mtSatBlank;
   DrawColoredMap1Click(Nil);
end;


procedure TMapForm.DrawStreamProfile(FinalRun : boolean = false; AskToSave : boolean = true);
var
   xg,yg,StartLat,StartLong,Lat,Long,
   SegLen,CumDist          : float64;
   rfile     : file;
   xpic,ypic,FirstPoint,NumPoints,i,j,db           : integer;
   v                        : array[1..2] of float32;
   PointSeparation, Bearing,EndLat,EndLong : float64;
   Lats,Longs,dists : ^bfarray32;
begin
   with MapDraw do begin
      if (StreamProfileResults = Nil) then exit;
      ChangeDEMNowDoing(Calculating);
      PointSeparation := DEMGlb[DEMonMap].AverageXSpace * 0.5;
      ExpandRoute(MapDraw.DEMonMap,StreamProfileResults,DEMGlb[DEMonMap].AverageSpace * 0.5,true,true,false);
      db := StringListToLoadedDatabase(StreamProfileResults, Petmar.NextFileNumber(MDTempDir, 'Stream_digitize_', DefaultDBExt),true,false,true);
      StreamProfileResults := Nil;

      StreamGraph := TThisBaseGraph.Create(Application);
      StreamGraph.BaseCaption := 'Stream Profile';
      StreamGraph.VertCompare := 0.001;
      StreamGraph.GraphDraw.TopMargin := 150;
      StreamGraph.OpenDataFile(rfile);
      i := 0;
      FirstPoint := i;
      v[1] := 0;
      ReadCoordsLatLongFromStreamProfileResults(StreamProfileResults,FirstPoint,EndLat,EndLong);
      DEMGlb[DEMonMap].GetElevFromLatLongDegree(EndLat,EndLong,v[2]);
      StreamGraph.GraphDraw.MaxVertAxis := v[2];
      StreamGraph.GraphDraw.MinVertAxis := v[2];
      LatLongDegreeToScreen(EndLat,EndLong,xpic,ypic);
      Self.Image1.Canvas.MoveTo(xpic,ypic);
      Self.Image1.Canvas.Pen.Color := ConvertPlatformColorToTColor(MDDef.DigitizeColor);
      Self.Image1.Canvas.Pen.Width := 3;
      CumDist := 0;
      //TotalPoints := 0;
      New(Lats);
      New(Longs);
      New(dists);

      for j := FirstPoint to pred(StreamProfileResults.Count) do begin
         ReadCoordsLatLongFromStreamProfileResults(StreamProfileResults,j,Lat,Long);
         VincentyCalculateDistanceBearing(EndLat,EndLong,Lat,Long,SegLen,Bearing);
         if (SegLen > 0.01) then begin
            NumPoints := round(SegLen / PointSeparation);
            if (NumPoints < 2) then NumPoints := 2;
            DEMGlb[DEMonMap].GetStraightRouteLatLongDegree(EndLat,EndLong,Lat,Long,saSmart,NumPoints,Lats^,Longs^,dists^);
            for i := 0 to NumPoints do begin
               if DEMGlb[DEMonMap].GetElevFromLatLongDegree(Lats^[i],Longs^[i],v[2]) then begin
                  LatLongDegreeToScreen(Lats^[i],Longs^[i],xpic,ypic);
                  v[1] := CumDist + 0.001 * Dists[i];
                  if (v[2] > StreamGraph.GraphDraw.MaxVertAxis) then StreamGraph.GraphDraw.MaxVertAxis := v[2];
                  if (v[2] < StreamGraph.GraphDraw.MinVertAxis) then StreamGraph.GraphDraw.MinVertAxis := v[2];
                  //inc(TotalPoints);
                  BlockWrite(rfile,v,1);
               end;
            end;
            CumDist := CumDist + 0.001 * SegLen;
            EndLat := Lat;
            EndLong := Long;
         end;
      end;
      Dispose(Lats);
      Dispose(Longs);
      Dispose(dists);

      StreamGraph.GraphDraw.MaxHorizAxis := CumDist;
      CloseFile(rfile);
      StreamGraph.GraphDraw.HorizLabel := 'Distance along Profile (km)';
      StreamGraph.GraphDraw.VertLabel := 'Elevation (m)';
      PadAxis(StreamGraph.GraphDraw.MinVertAxis,StreamGraph.GraphDraw.MaxVertAxis);
      {$IfDef RecordClick} WriteLineToDebugFile('Profile drawing points: ' + IntegerToString(TotalPoints)); {$EndIf}

      CumDist := 0;
      if MDDef.LabelRouteTurningPoints then StreamGraph.GraphDraw.GraphTopLabels := tStringList.Create;
      for i := FirstPoint to pred(StreamProfileResults.Count) do begin
         ReadCoordsLatLongFromStreamProfileResults(StreamProfileResults,i,Lat,Long);
         if (i = FirstPoint) then SegLen := 0
         else VincentyCalculateDistanceBearing(StartLat,StartLong,Lat,Long,SegLen,Bearing);
         SegLen := 0.001 * SegLen;
         CumDist := CumDist + SegLen;
         DEMGlb[DEMonMap].LatLongDegreetoDEMGrid(Lat,Long,xg,yg);
         if MDDef.LabelRouteTurningPoints then StreamGraph.GraphDraw.GraphTopLabels.Add(RealToString(CumDist,12,4) + '  ' + DEMGlb[DEMonMap].DEMLocationString(xg,yg));
         StartLat := Lat;
         StartLong := Long;
      end;
      StreamGraph.GraphDraw.LabelPointsAtop := MDDef.LabelRouteTurningPoints and (StreamGraph.GraphDraw.GraphTopLabels.Count < 25);

      (*
      if ExtractXYPointsAlongProfile then begin
         fName := Petmar.NextFileNumber(MDTempDir, 'profile_',DefaultDBExt);
         StringListToLoadedDatabase(Results,fName);
         ExtractXYPointsAlongProfile := false;
      end;
      *)

      StreamGraph.GraphDraw.GraphDrawn := true;
      StreamGraph.SetUpGraphForm;
      StreamGraph.RedrawDiagram11Click(Nil);
      ChangeDEMNowDoing(JustWandering);
   end;
end;


procedure TMapForm.StreamProfile1Click(Sender: TObject);
begin
   ChangeDEMNowDoing(SeekingStreamProfile);
   if (StreamProfileResults <> Nil) then FreeAndNil(StreamProfileResults);
   //MapDraw.MapOverlays.ovVectorFiles.Add('Pipeline-stream.plr');
end;

procedure TMapForm.Streams1Click(Sender: TObject);
begin
{$IfDef ExExoticMaps}
{$Else}
   CreateRidgeMap(MapDraw.DEMOnMap,DEMGlb[MapDraw.DEMonMap].FullDEMGridLimits,rtmStream);
{$EndIf}
end;

procedure TMapForm.Vectoraverage1Click(Sender: TObject);
begin
   ThinDEM1Click(Sender);
end;

procedure TMapForm.VectorOutlines1Click(Sender: TObject);
var
   Masks : string;
   FileNames    : tStringList;
   i : integer;
begin
   Masks := 'Any database|*.shp;*.dbf;*.db|ESRI Shapefile|*.shp|' + DBNameMask + '|' + 'All files|*.*|';
   FileNames := tStringList.Create;
   FileNames.Add(LastOverlayName);
   if GetMultipleFiles('Vector file to overlay',Masks,FileNames,MDdef.DefaultVectorFilter) then begin
      AddOrSubtractOverlay(Self,ovoVectors,true);
      for i := 0 to pred(FileNames.Count) do begin
         MapDraw.MapOverlays.ovVectorFiles.Add(FileNames.Strings[i]);
      end;
      FileNames.Free;
      DoFastMapRedraw;
   end;
end;


procedure TMapForm.Delete1Click(Sender: TObject);
begin
{$IfDef ExVectorOverlay}
{$Else}
   if (DEMEditForm <> Nil) then DEMeditForm.Delete1Click(Sender);
{$EndIf}
end;


procedure TMapForm.DeliberatemaptoGoogleEarth1Click(Sender: TObject);
begin
   MaptoGoogleEarth1Click(Sender);
end;

procedure TMapForm.Replace1Click(Sender: TObject);
begin
{$IfDef ExVectorOverlay}
{$Else}
   if (DEMEditForm <> Nil) then DEMeditForm.Replace1Click(Sender);
{$EndIf}
end;


procedure TMapForm.Replacefromsecondgrid1Click(Sender: TObject);
var
   MergeDEM : integer;
begin
   if GetDEM(MergeDEM,true,'replace all values in ' + DEMGlb[MapDraw.DEMonMap].AreaName) then begin
      DEMGlb[MapDraw.DEMonMap].FillHolesSelectedBoxFromReferenceDEM(DEMGlb[MapDraw.DEMonMap].FullDEMGridLimits,MergeDEM,hfEverything);
      RespondToChangedDEM;
   end;
end;

procedure TMapForm.Replaceonlyvalidvaluesfromsecondgrid1Click(Sender: TObject);
var
   MergeDEM : integer;
begin
   if GetDEM(MergeDEM,true,'replace only valid values in ' + DEMGlb[MapDraw.DEMonMap].AreaName) then begin
      DEMGlb[MapDraw.DEMonMap].FillHolesSelectedBoxFromReferenceDEM(DEMGlb[MapDraw.DEMonMap].FullDEMGridLimits,MergeDEM,hfOnlyValid);
      RespondToChangedDEM;
   end;
end;

procedure TMapForm.Movewindow1Click(Sender: TObject);
begin
   ChangeDEMNowDoing(MoveMapBox);
end;

procedure TMapForm.Moviewithallmaps1Click(Sender: TObject);
var
   i  : integer;
   Findings : tStringlist;
   fName : PathStr;
   Bitmap : tMyBitmap;
begin
   Findings := tStringList.Create;
   AllMapsMatchThisCoverageArea1Click(Nil);
   Allsamepixelsizeasthismap1Click(Sender);
   for i := 0 to pred(WMDEM.MDIChildCount) do begin
      if WMDEM.MDIChildren[i] is tMapForm then begin
          CopyImageToBitmap((WMDEM.MDIChildren[i] as TMapForm).Image1,Bitmap);
          fName := (WMDEM.MDIChildren[i] as TMapForm).MapDraw.BaseTitle + '.bmp';
          Petmar.ReplaceCharacter(fName,'/','-');
          Bitmap.SaveToFile(MDtempDir + fName);
          Findings.Add(fName);
      end;
   end;
   if (Findings.Count > 1) then begin
      Findings.Sort;
      fName := NextFileNumber(MDtempDir,'map_4_movie_','.mov');
      Findings.SaveToFile(fName);
      CreateNewMovie(fName);
   end;
   Findings.Free;
end;


procedure TMapForm.MrSidSpeedButtonClick(Sender: TObject);
begin
   {$IfDef RecordMrSID} WriteLineToDebugFile('TMapForm.MrSidSpeedButtonClick'); {$EndIf}
   ChangeDEMNowDoing(OpenMrSid);
end;


procedure TMapForm.ForcereloadWMSmap1Click(Sender: TObject);
begin
   {$IfDef ExWMS}
   {$Else}
      WMSMapsClick(Sender);
   {$EndIf}
end;


procedure TMapForm.SpeedButton2Click(Sender: TObject);
begin
   {$IfDef ExWMS}
   {$Else}
      WMSMapsClick(Sender);
   {$EndIf}
end;


procedure TMapForm.WMSMapsClick(Sender: TObject);
begin
   {$IfDef ExWMS}
   {$Else}
      {$IfDef RecordWMS} WriteLineToDebugFile('TMapForm.WMSMapsClick in'); {$EndIf}
      MapDraw.WMSOverlayOnMap := true;
      if MDDef.WMSOpacityValue < 50 then ReadDefault('WMS opacity (0-100)',MDDef.WMSOpacityValue);
      DoFastMapRedraw;
      {$IfDef RecordWMS} WriteLineToDebugFile('TMapForm.WMSMapsClick out'); {$EndIf}
   {$EndIf}
end;


procedure TMapForm.WMSOpactiy1Click(Sender: TObject);
begin
   {$IfDef ExWMS}
   {$Else}
      ReadDefault('WMS opacity (0-100)',MDDef.WMSOpacityValue);
      DoFastMapRedraw;
   {$EndIf}
end;


procedure TMapForm.Multiplevalues1Click(Sender: TObject);
begin
   {$IfDef ExAdvancedGIS}
   {$Else}
      MultMaskDEM(MapDraw.DEMonMap);
   {$EndIf}
end;

procedure TMapForm.Multiplyallgridsbyconstantsandresave1Click(Sender: TObject);
begin
   {$IfDef ExMultigrid}
   {$Else}
   MultiGridArray[MapDraw.MultiGridOnMap].MultiplyGrids;
   {$EndIf}
end;

procedure TMapForm.Multiplyzvalues1Click(Sender: TObject);
label
   CompletedEarly;
var
   x,y,Invalid  : integer;
   Mult,Add,z : float32;
begin
    with DEMGlb[MapDraw.DEMonMap] do begin
       if (Sender = Multiplyzvalues1) then begin
          Mult := 1 / FeetToMeters;
          ReadDefault('z multiplier',Mult);
          DEMGlb[MapDraw.DEMonMap].MultiplyGridByConstant(Mult);
          goto CompletedEarly;
       end
       else if (Sender = Dividezvalues1) then begin
          Mult := 1 / FeetToMeters;
          ReadDefault('z divisor',Mult);
          DEMGlb[MapDraw.DEMonMap].MultiplyGridByConstant(1/Mult);
          goto CompletedEarly;
       end
       else if (Sender = Raiselowerzvalues1) then begin
          Add := 32;
          ReadDefault('z add to',Add);
          DEMGlb[MapDraw.DEMonMap].AddConstantToGrid(Add);
          goto CompletedEarly;
       end
       else if (Sender = Arctangent1) then begin
          Mult := 2;
          ReadDefault('z multiplier with arctangent',Mult);
       end;
       Invalid := 0;
       StartProgress('DEM math');
       for x := 0 to pred(DEMheader.NumCol) do begin
          if (x mod 50 = 0) then UpDateProgressBar(x/pred(DEMheader.NumCol));
          for y := 0 to pred(DEMheader.NumRow) do
             if GetElevMetersOnGrid(x,y,z) then begin
                //if (Sender = Multiplyzvalues1) or (Sender = Nil) then SetGridElevation(x,y,Mult*z);
                //if (Sender = Dividezvalues1) then SetGridElevation(x,y,z/Mult);
                if (Sender = Radianstodegrees1) then SetGridElevation(x,y,z/DegToRad);
                //if (Sender = Raiselowerzvalues1) then SetGridElevation(x,y, Add + z);
                if (Sender = Absolutevalue1) then SetGridElevation(x,y, abs(z));
                if (Sender = Roundtointegers1) then SetGridElevation(x,y, round(z));
                if (Sender = Lntransform1) or (Sender = Logbase10transform1) then begin
                    if (z > 0) then begin
                       if (Sender = Lntransform1) then SetGridElevation(x,y,ln(z));
                       if (Sender = Logbase10transform1) then SetGridElevation(x,y,log10(z));
                    end
                    else begin
                       Inc(Invalid);
                       SetGridMissing(x,y);
                    end;
                end;
                if (Sender = TangentRadians1) then SetGridElevation(x,y,Math.Tan(z));
                if (Sender = Tangentdegrees1) then SetGridElevation(x,y,Math.Tan(z*DegToRad));
                if (Sender = Slopedegreestopercent1) then SetGridElevation(x,y,100 * Math.Tan(z*DegToRad));
                if (Sender = Arctangent1) then SetGridElevation(x,y,arctan(Mult*z));
             end
             else SetGridMissing(x,y);
       end;
       EndProgress;
       if (Invalid > 0) then begin
          MessageToContinue('Invalid points set to missing: ' + IntToStr(Invalid));
       end;
    end {with};
   CompletedEarly:;
    RespondToChangedDEM;
end;


procedure TMapForm.Radianstodegrees1Click(Sender: TObject);
begin
   Multiplyzvalues1Click(Sender);
end;

procedure TMapForm.Raiselowerzvalues1Click(Sender: TObject);
begin
    Multiplyzvalues1Click(Sender);
end;


procedure TMapForm.CutPoints(MinGood,MaxGood : SmallInt);
var
   Col,Row : integer;
   z : float32;
  // Changed : boolean;
begin
  // Changed := false;
   with DEMGlb[MapDraw.DEMonMap]  do begin
      if ShowSatProgress and (DEMheader.NumCol > 1500) then StartProgress('Mask');
      for Col := 0 to pred(DEMheader.NumCol) do begin
         if (col mod 100 = 0) and ShowSatProgress  and (DEMheader.NumCol > 1500) then UpdateProgressBar( Col / DEMheader.NumCol);
         for Row := 0 to pred(DEMheader.NumRow) do begin
           if GetElevMetersOnGrid(Col,Row,z) then begin
              if ( (MinGood = MaxGood) and (round(z) = MinGood)) or
               ((MinGood <> MaxGood) and ((round(z) < MinGood) or (round(z) > MaxGood))) then begin
                 SetGridMissing(Col,Row);
                 //Changed := true;
              end;
            end;
         end;
      end;
      if ShowSatProgress and (DEMheader.NumCol > 1500) then EndProgress;
      if ShowSatProgress then RespondToChangedDEM;
   end;
end;

procedure TMapForm.Highend1Click(Sender: TObject);
var
   x,y  : integer;
   zRep,zHi,zLo,z : float32;
begin
    zHi := 99e99;
    zLo := -99e99;
    with DEMGlb[MapDraw.DEMonMap] do begin
       if (Sender = HighEnd1) then begin
          zHi := DEMheader.MaxElev;
          ReadDefault('Cutoff (clip higher points to this z)',zHi);
          zRep := zHi;
       end
       else if (Sender = LowEnd1) then begin
          zLo := DEMheader.MinElev;
          ReadDefault('Cutoff (clip lower points to this z)',zLo);
          zRep := zLo;
       end;

       ShowHourglassCursor;
       if ShowSatProgress and (DEMheader.NumCol > 1500) then StartProgress('Mask');
       for x := 0 to pred(DEMheader.NumCol) do begin
          if (x mod 100 = 0) and ShowSatProgress  and (DEMheader.NumCol > 1500) then UpdateProgressBar(x / DEMheader.NumCol);
          for y := 0 to pred(DEMheader.NumRow) do begin
             if GetElevMetersOnGrid(x,y,z) then begin
                if (z < zLo) or (z > zHi) then SetGridElevation(x,y,ZRep);
             end;
          end;
       end;
       CheckMaxMinElev;

       MapDraw.MinMapElev := DEMheader.MinElev;
       MapDraw.MaxMapElev := DEMheader.MaxElev;
       DEMGlb[MapDraw.DEMonMap].DEMstatus := dsUnsaved;

       {$IfDef RecordElevationScaling} WriteLineToDebugFile('TMapForm.Markasmissing1Click, min=' + RealToString(MapDraw.MinMapElev,-12,-2) + '  max=' + RealToString(MapDraw.MaxMapElev,-12,-2)); {$EndIf}
       if ShowSatProgress and (DEMheader.NumCol > 1500) then EndProgress;
    end {with};
    DoBaseMapRedraw;
end;

procedure TMapForm.Highpoints1Click(Sender: TObject);
var
   CutElev : SmallInt;
begin
   CutElev := MaxSmallInt;
   ReadDefault('Remove values >',CutElev);
   CutPoints(-MaxSmallInt,CutElev);
end;

procedure TMapForm.Highways1Click(Sender: TObject);
begin
   LoadDataBaseFile(HighwayGISFileName);
end;


procedure TMapForm.hinaveragingcomparison1Click(Sender: TObject);
var
   Levels,i,NewDEM,Spacings,BaseSpacing : integer;
   Linear,SaveDEMs : boolean;
   fName : PathStr;
begin
   Linear := AnswerIsYes('Linear (not power)');
   BaseSpacing := 1;
   if Linear then ReadDefault('Spacing between DEMs',BaseSpacing);
   ReadDefault('levels',Levels);
   SaveDEMs := AnswerIsYes('Save DEMs');
   if SaveDEMs then GetDOSPath('Location to save DEMs',WriteDEMDir);
   for i := 2 to Levels do begin
      if Linear then Spacings := BaseSpacing + pred(i) * BaseSpacing
      else Spacings := round(Math.IntPower(2, pred(i)));
      NewDEM := DEMGlb[MapDraw.DEMonMap].ThinThisDEM('',Spacings);
      if (NewDEM <> 0) and SaveDEMs then begin
         //DEMGlb[NewDEM].SetUpMap(NewDEM,true,MapDraw.MapType);
         fName := WriteDEMDir + DEMGlb[NewDEM].AreaName + '.dem';
         DEMGlb[NewDEM].WriteNewFormatDEM(fName);
      end;
      NewDEM := DEMGlb[MapDraw.DEMonMap].ThinThisDEM('',Spacings,true);
      if (NewDEM <> 0) and SaveDEMs then begin
         //DEMGlb[NewDEM].SetUpMap(NewDEM,true,MapDraw.MapType);
         fName := WriteDEMDir + DEMGlb[NewDEM].AreaName + '.dem';
         DEMGlb[NewDEM].WriteNewFormatDEM(fName);
      end;
   end;
end;

procedure TMapForm.MatchThiscoverageareaandsamepixelsize1Click(Sender: TObject);
begin
   AllMapsMatchThisCoverageArea1Click(Sender);
   Allsamepixelsizeasthismap1Click(Sender);
end;

procedure TMapForm.hismap1Click(Sender: TObject);
begin
   if (MapDraw.ValidDEMonMap) then begin
      DeleteFileIfExists(ProjectDir +  'Map_Layers\' + DEMGlb[MapDraw.DEMonMap].AreaName + '_tiger.bmp');
      DeleteFileIfExists(ProjectDir +  'Map_Layers\' + DEMGlb[MapDraw.DEMonMap].AreaName + '_base.bmp');
   end;
end;

procedure TMapForm.Lowend1Click(Sender: TObject);
begin
   HighEnd1Click(Sender);
end;

procedure TMapForm.LowPoints1Click(Sender: TObject);
var
   CutElev : SmallInt;
begin
   CutElev := -MaxSmallInt;
   ReadDefault('Remove values <',CutElev);
   CutPoints(CutElev,MaxSmallInt);
end;


procedure TMapForm.LVIS1Click(Sender: TObject);
{$IfDef ExPointCloud}
begin
{$Else}
var
   lvis_form1 : Tlvis_form1;
begin
   lvis_form1 := Tlvis_form1.Create(Application);
   lvis_form1.Open(Self);
{$EndIf}
end;

procedure TMapForm.Specified1Click(Sender: TObject);
var
   CutElev : SmallInt;
begin
   CutElev := MaxSmallInt;
   ReadDefault('Remove values =',CutElev);
   CutPoints(CutElev,CutElev);
end;

procedure TMapForm.Specifyxyzshifts1Click(Sender: TObject);
var
   //fName : PathStr;
   dx,dy,dz : float32;
   //i,
   NewVD : integer;
begin
   dx := 0.43;
   ReadDefault('dx',dx);
   dy := -1.24;
   ReadDefault('dy',dy);
   dz := -0.58;
   ReadDefault('dz',dz);
   NewVD := VertCSEGM2008;
   ReadDefault('new vertical datum code',NewVD);
   DEMGlb[MapDraw.DEMonMap].DEMHeader.VerticalCSTypeGeoKey := NewVD;
   DEMGlb[MapDraw.DEMonMap].DEMHeader.DEMSWCornerX := DEMGlb[MapDraw.DEMonMap].DEMHeader.DEMSWCornerX + dx;
   DEMGlb[MapDraw.DEMonMap].DEMHeader.DEMSWCornerY := DEMGlb[MapDraw.DEMonMap].DEMHeader.DEMSWCornerY + dy;
   DEMGlb[MapDraw.DEMonMap].AddConstantToGrid(dz);
end;

procedure TMapForm.Spectrallibrarygraph1Click(Sender: TObject);
begin
   {$IfDef ExSat}
   {$Else}
      SpectLibGraph := SpectralLibraryGraph;
      ChangeDEMNowDoing(SpectralReflectance);
   {$EndIf}
end;


procedure TMapForm.SaveDEM2Click(Sender: TObject);
var
   FileName : PathStr;
begin
   Filename := DEMGlb[MapDraw.DEMonMap].DEMFileName;
   GetFileNameDefaultExt('modified DEM','DEM|*.dem',FileName);
   DEMGlb[MapDraw.DEMonMap].WriteNewFormatDEM(FileName);
   DEMGlb[MapDraw.DEMonMap].DEMstatus := dsSaved;
   DoFastMapRedraw;
end;



procedure TMapForm.PanSEButtonClick(Sender: TObject);
begin
   LastX := (8 - MDDEF.PanOverlap) * MapDraw.MapXSize div 8;
   LastY := (8 - MDDEF.PanOverlap) * MapDraw.MapYSize div 8;
   ZoomIn1Click(NoZoom1);
end;


procedure TMapForm.PanSButtonClick(Sender: TObject);
begin
   LastX := MapDraw.MapXSize div 2;
   LastY := (8 - MDDEF.PanOverlap) * MapDraw.MapYSize div 8;
   ZoomIn1Click(NoZoom1);
end;

procedure TMapForm.PanWButtonClick(Sender: TObject);
begin
   LastX := MDDEF.PanOverlap * MapDraw.MapXSize div 8;
   LastY := MapDraw.MapYSize div 2;
   ZoomIn1Click(NoZoom1);
end;


procedure TMapForm.PanSWButtonClick(Sender: TObject);
begin
   LastX := MDDEF.PanOverlap * MapDraw.MapXSize div 8;
   LastY := (8 - MDDEF.PanOverlap) * MapDraw.MapYSize div 8;
   ZoomIn1Click(NoZoom1);
end;

procedure TMapForm.PanEButtonClick(Sender: TObject);
begin
   LastX := (8 - MDDEF.PanOverlap) * MapDraw.MapXSize div 8;
   LastY := MapDraw.MapYSize div 2;
   ZoomIn1Click(NoZoom1);
end;


procedure TMapForm.Panel1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   if Button = mbRight then begin
      DisplayHTMLTopic('html\maptool.htm');
   end;
end;

procedure TMapForm.PanNWButtonClick(Sender: TObject);
begin
   LastX := MDDEF.PanOverlap * MapDraw.MapXSize div 8;
   LastY := MDDEF.PanOverlap * MapDraw.MapYSize div 8;
   ZoomIn1Click(NoZoom1);
end;

procedure TMapForm.PanNButtonClick(Sender: TObject);
begin
   LastX := MapDraw.MapXSize div 2;
   LastY := MDDEF.PanOverlap * MapDraw.MapYSize div 8;
   ZoomIn1Click(NoZoom1);
end;


procedure TMapForm.PanNEButtonClick(Sender: TObject);
begin
   LastX := (8 - MDDEF.PanOverlap) * MapDraw.MapXSize div 8;
   LastY := MDDEF.PanOverlap * MapDraw.MapYSize div 8;
   ZoomIn1Click(NoZoom1);
end;


procedure TMapForm.SetPanButtons;
begin
   if PanButtonsOnMap and SubsetSpeedButton.Visible then {with MapDraw,MapCorners do} begin
      if MapDraw.DEMMap and MapDraw.ValidDEMonMap then {with DEMGlb[MapDraw.DEMonMap] do} begin
         PanNButton.Enabled := MapDraw.MapCorners.BoundBoxDataGrid.ymax < pred(DEMGlb[MapDraw.DEMonMap].DEMheader.NumRow) - 0.5;
         PanSButton.Enabled := MapDraw.MapCorners.BoundBoxDataGrid.ymin > 0.5;
         PanWButton.Enabled := MapDraw.MapCorners.BoundBoxDataGrid.xmin > 0.5;
         PanEButton.Enabled := MapDraw.MapCorners.BoundBoxDataGrid.xmax < pred(DEMGlb[MapDraw.DEMonMap].DEMheader.NumCol) - 0.5;
      {$IfDef ExSat}
      {$Else}
      end
      else if (MapDraw.SatOnMap > 0) and (SatImage[MapDraw.SatOnMap] <> Nil) then with MapDraw.SatView do begin
         PanNButton.Enabled := MapDraw.MapCorners.BoundBoxDataGrid.ymin > 0;
         PanSButton.Enabled := MapDraw.MapCorners.BoundBoxDataGrid.ymax < pred(SatImage[MapDraw.SatOnMap].NumSatRow) - 0.5;
         PanWButton.Enabled := MapDraw.MapCorners.BoundBoxDataGrid.xmin > 0;
         PanEButton.Enabled := MapDraw.MapCorners.BoundBoxDataGrid.xmax < pred(SatImage[MapDraw.SatOnMap].NumSatCol) - 0.5;
      {$EndIf}
      end;

      PanNWButton.Enabled := PanNButton.Enabled and PanWButton.Enabled;
      PanNEButton.Enabled := PanNButton.Enabled and PanEButton.Enabled;
      PanSWButton.Enabled := PanSButton.Enabled and PanWButton.Enabled;
      PanSEButton.Enabled := PanSButton.Enabled and PanEButton.Enabled;

      PanNButton.Left := (MapDraw.MapXSize {+ LeftMargin}) div 2 - 8;
      PanSButton.Left := (MapDraw.MapXSize {+ LeftMargin}) div 2 - 8;

      PanWButton.Top := (MapDraw.MapYSize {+ BottomMargin}) div 2 - 8;
      PanEButton.Top := (MapDraw.MapYSize {+ BottomMargin}) div 2 - 8;

      PanSWButton.Top := (MapDraw.MapYSize {+ BottomMargin}) - 16;
      PanSButton.Top := (MapDraw.MapYSize {+ BottomMargin}) - 16;
      PanSEButton.Top := (MapDraw.MapYSize {+ BottomMargin}) - 16;

      PanNEButton.Left := (MapDraw.MapXSize {+ LeftMargin}) - 16;
      PanEButton.Left := (MapDraw.MapXSize {+ LeftMargin}) - 16;
      PanSEButton.Left := (MapDraw.MapXSize {+ LeftMargin}) - 16;
      MakePanButtonsVisible(PanNButton.Enabled or PanEButton.Enabled or PanSButton.Enabled or PanWButton.Enabled);
   end
   else begin
      MakePanButtonsVisible(false);
   end;
end;

procedure TMapForm.MakePanButtonsVisible(ButtonsVisible : boolean);
begin
   PanNButton.Visible := ButtonsVisible;
   PanNEButton.Visible := ButtonsVisible;
   PanEButton.Visible := ButtonsVisible;
   PanSEButton.Visible := ButtonsVisible;
   PanSButton.Visible := ButtonsVisible;
   PanSWButton.Visible := ButtonsVisible;
   PanWButton.Visible := ButtonsVisible;
   PanNWButton.Visible := ButtonsVisible;
end;



procedure TMapForm.NoScrollbars;
begin
   ScrollBox1.HorzScrollBar.Visible := false;
   ScrollBox1.VertScrollBar.Visible := false;
   ClientWidth := MapDraw.MapXSize;
   ClientHeight := MapDraw.MapYSize;
   ZoomInSpeedButton4.Visible := false;
   ZoomOutSpeedButton5.Visible := false;
   SpeedButton22.Visible := false;
   MapArea1.Visible := false;
   ModifyMapArea1.Visible := false;
   ArrangeButtons;
end;


{$IfDef RecordFormResize}
   procedure TMapForm.RecordFormSize(Title : shortString);
   begin
      if FormOperational then  WriteLineToDebugFile(Title + ' client: ' + IntToStr(Self.ClientWidth) + 'x' + IntToStr(Self.ClientHeight) +
          '  scrollbox: ' + IntToStr(ScrollBox1.ClientWidth) + 'x' + IntToStr(ScrollBox1.ClientHeight)  +
          '  image: ' + IntToStr(Image1.ClientWidth) + 'x' + IntToStr(Image1.ClientHeight)  +  '  ' + MapDraw.MapSizeString {+  '  Toolbar: ' + IntToStr(ToolAndPanelHeights)});
   end;
{$EndIf}


procedure TMapForm.FormResize(Sender: TObject);


      procedure SetSize(wantx,wanty,extra : integer);
      begin
         if (ClientWidth > WantX + Extra) then ClientWidth := WantX + Extra;
         if (ClientHeight > WantY + Panel1.Height + Extra) then ClientHeight := WantY + Panel1.Height + Extra;
     end;

     function WantedWidth : integer;
     begin
        WantedWidth := MapDraw.MapXSize + GetSystemMetrics(SM_CXVSCROLL);  //ScrollBox1.HorzScrollBar.Size;
     end;

     function WantedHeight : integer;
     begin
        WantedHeight := MapDraw.MapYSize + ToolAndPanelHeights + GetSystemMetrics(SM_CXVSCROLL);  //ScrollBox1.HorzScrollBar.Size;
     end;

begin
   if SizingWindow or (MapDraw = Nil) or MapDraw.ClosingMapNow then exit;
   SizingWindow := true;
   if (Sender = Nil) then begin
      SetClientHeight;
   end;
   {$IfDef RecordFormResize} RecordFormSize(Self.Caption + ' FormResize in, ' + MapDraw.MapSizeString); {$EndIf}

   (*
   if (ClientWidth > WantedWidth) then ClientWidth := WantedWidth;
   if (ClientHeight > WantedHeight) then ClientHeight := WantedHeight;
   *)

   if (MapDraw.MapOwner = moPointVerificationMap) then begin
      NoScrollBars;
   end
   else begin
      if (WantedWidth < 8 * wmdem.ClientWidth div 10) and (WantedHeight < 8 * wmdem.ClientHeight div 10) then begin
         //SetSize(WantedWidth,WantedHeight,0);
         ClientWidth := WantedWidth;
         ClientHeight := WantedHeight + Panel1.Height;
         ScrollBox1.HorzScrollBar.Visible := false;
         ScrollBox1.VertScrollBar.Visible := false;
      end
      else begin
         //SetSize(WantedWidth,WantedHeight,20);
         ClientWidth := 8 * wmdem.ClientWidth div 10;
         ClientHeight := 8 * wmdem.ClientHeight div 10;
         ScrollBox1.HorzScrollBar.Visible := true;
         ScrollBox1.VertScrollBar.Visible := true;
         if (ScrollBox1.Width > WantedWidth) then ScrollBox1.ClientWidth := WantedWidth;
         if (ScrollBox1.Height > WantedHeight) then ScrollBox1.ClientHeight := WantedHeight;
         ScrollBox1.HorzScrollBar.Range := MapDraw.MapXSize;
         ScrollBox1.VertScrollBar.Range := MapDraw.MapYSize;
      end;
      //ClientWidth := ScrollBox1.Width;
      //ClientHeight := ScrollBox1.Height;

   end;
   SetPanButtons;

   TrackBar2.Width := Panel1.Width - Panel3.Width;
   SizingWindow := false;

   {$IfDef RecordFormResize} RecordFormSize(Self.Caption + ' FormResize out, '); {$EndIf}
end;

procedure TMapForm.DEMGridarea1Click(Sender: TObject);
var
   x,y, Pixels : integer;
   RowArea,TotalArea : float64;
begin
   with DEMGlb[MapDraw.DEMonMap] do begin
      TotalArea := 0;
      Pixels := 0;
      for x := 0 to pred(DEMheader.NumCol) do begin
         RowArea := 0;
         for y := 0 to pred(DEMheader.NumRow) do begin
            if not MissingDataInGrid(x,y) then begin
                RowArea := RowArea + AverageYSpace * XSpaceByDEMrow^[y];
                Inc(Pixels);
            end;
         end;
         TotalArea := TotalArea + RowArea * 0.001 * 0.001;
      end;
   end;
   MessageToContinue('Area: ' + RealToString(TotalArea,-18,-4) + ' km²' + MessLineBreak + 'Pixels: ' + IntToStr(Pixels));
end;

procedure TMapForm.DEMgridhistogram1Click(Sender: TObject);
begin
   DEMGlb[MapDraw.DEMonMap].CreateWholeDEMHistogram;
end;

procedure TMapForm.Fulldataset1Click(Sender: TObject);
begin
   FullDEM1Click(Sender);
end;


procedure TMapForm.FullDEM1Click(Sender: TObject);
begin
   RestoreFullMap;
end;


procedure TMapForm.Feet1Click(Sender: TObject);
var
   CInt,i,j : integer;
   Factor,z      : float32;
   TStr : ShortString;
begin
   with MapDraw do begin
      if (Sender = Meters1) then begin
         CInt := 10;
         TStr := 'meters';
         Factor := 1;
      end
      else begin
         CInt := 40;
         TStr := 'feet';
         Factor := Petmar_types.FeetToMeters;
      end;
      ReadDefault('souce map contour interval (' + TStr + ')',CInt);
      if not ColorDialog1.Execute then exit;
      if ShowSatProgress then StartProgress('Ghost contours');
      with DEMGlb[DEMOnMap] do begin
         for i := 0 to pred(MapXSize) do begin
            for j := 0 to pred(MapYSize) do begin
               ScreenToElev(i,j,z);
               if round(z) mod round(CInt * Factor) = 0 then Image1.Canvas.Pixels[i,j] := ColorDialog1.Color;
            end;
            if (i mod 50 = 0) and ShowSatProgress then begin
               UpdateProgressBar(i/MapXSize);
               if WantOut then exit;
            end;
         end {for i};
      end {with};
      if ShowSatProgress then EndProgress;
   end;
end;


procedure TMapForm.SetMapPixelSize(PixelSize : float64);
var
   NewBlowUp : float64;
begin
   NewBlowUp := MapDraw.ScreenPixelSize / PixelSize;
   BlowUpTheMap(NewBlowUp);
end;


procedure TMapForm.Blowup2Click(Sender: TObject);
var
   BlowUp : float64;
   MemNeed : int64;
   OK : boolean;
begin
   if LockMaps then begin
      MessageToContinue('Maps locked for digitizing');
      exit;
   end;
   BlowUp := 2;
   repeat
      ReadDefault('zoom in factor',BlowUp);
      MemNeed := round( 4.0 * MapDraw.MapXSize * BlowUp * MapDraw.MapYSize * BlowUp);
      if AskUserAboutMemory(MemNeed) then begin
         OK := AnswerIsYes('Image requires  ' + SmartMemorySizeBytes(MemNeed) + MessLineBreak +
             '  displays slowly, will not fit on screen.' + MessLineBreak +
             'Subset before using this zoom.'+ MessLineBreak + MessLineBreak +
             'Continue anyway');
         if not OK then BlowUp := 1.0;
      end
      else OK := true;
   until OK;
   {$IfDef RecordMapDraw} WriteLineToDebugFile('Zoom in map by: ' + RealToString(BlowUp,8,2)); {$EndIf}
   if (abs(BlowUp-1) < 0.0001) then exit;
   BlowUpTheMap(BlowUp);
   ChangeDEMNowDoing(DEMDoingWhatBefore);
end;


procedure TMapForm.Zoomout1Click(Sender: TObject);
var
   BlowUp : float64;
begin
   if LockMaps then begin
      MessageToContinue('Maps locked for digitizing');
      exit;
   end;
   BlowUp := 2;
   ReadDefault('zoom out factor',BlowUp);
   {$IfDef RecordMapDraw} WriteLineToDebugFile('Zoom out map by: ' + RealToString(BlowUp,8,2)); {$EndIf}
   BlowUpTheMap(1/BlowUp);
   ChangeDEMNowDoing(DEMDoingWhatBefore);
end;


procedure TMapForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
   Pt : tPoint;
begin
   GetCursorPos(Pt);
   if key in [33..39] then begin
      if Key in [33,34,39] then inc(Pt.x);
      if Key in [35,36,37] then dec(Pt.x);
      if Key in [33,36,38] then dec(Pt.y);
      if Key in [34,35,40] then inc(Pt.y);
      SetCursorPos(Pt.x,Pt.y);
      Image1MouseMove(Sender,Shift,Pt.X,Pt.Y);
   end;
end;


procedure TMapForm.Randomsamplingpoints1Click(Sender: TObject);
var
   NPts,found : integer;
   Lat, Long : float64;
   z : float32;
   Results : tStringList;
   fName : PathStr;
begin
   NPts := 25;
   ReadDefault('Random points',NPts);
   Found := 0;
   Randomize;
   Results := tStringList.Create;
   Results.Add('LAT,LONG');
   repeat
      Lat := MapDraw.MapCorners.BoundBoxGeo.ymin + Random * (MapDraw.MapCorners.BoundBoxGeo.ymax - MapDraw.MapCorners.BoundBoxGeo.ymin);
      Long := MapDraw.MapCorners.BoundBoxGeo.xmin + Random * (MapDraw.MapCorners.BoundBoxGeo.xmax - MapDraw.MapCorners.BoundBoxGeo.xmin);
      if (MapDraw.DEMonMap = 0) or DEMGlb[MapDraw.DEMonMap].GetElevFromLatLongDegree(Lat,Long,z) then begin
         inc(Found);
         Results.Add(RealToString(Lat,-12,8) + ',' + RealToString(Long,-12,8));
      end;
   until Found >= NPts;
   fName := Petmar.NextFileNumber(MDtempDir,'random_','.dbf');
   StringListToLoadedDatabase(Results,fName);
end;

procedure TMapForm.Rangecircles1Click(Sender: TObject);
begin
   EditRangeCircles;
   ChangeDEMNowDoing(RangeCircles);
end;


function TMapForm.MapInformationString : tStringList;
const
   DashLine = '--------------------------------';
var
   TStr  : ShortString;
   TempList : tStringList;
   i,Decs : integer;
   ib : byte;

          procedure AddProjectionToResult(Projection : tMapProjection );
          var
             i : integer;
          begin
               for I := 0 to pred(Projection.ProjectionParametersList.Count) do
                  Result.Add(Projection.ProjectionParametersList.Strings[i]);
          end;

          function BBlimits(bb : sfBoundBox) : shortstring;
          begin
             Result := '   ' + RealToString(bb.xmax-bb.xmin,-12,Decs) + 'x' + RealToString(bb.ymax-bb.ymin,-12,Decs);
          end;

begin
   with MapDraw do begin
      Result := tStringList.Create;
      Result.Add('Screen Pixels: ' + IntToStr(MapXSize) + ' x ' + IntToStr(MapYSize));
      Result.Add('Screen pixel size: ' + SmartDistanceMetersFormat(ScreenPixelSize));
      Result.Add('Display Datum: ' + DatumName(MapDraw.PrimMapProj.h_DatumCode));
      AddProjectionToResult(PrimMapProj);
      if (MapDraw.VectorIndex = 0) then begin
         if (MapDraw.MapZoomFactor > 1) then TStr := 'Every point appears ' + RealToString(MapZoomFactor,-8,2) + ' times'
         else TStr := RealToString(100.0*MapZoomFactor,-8,1) + '% of points on map';
         Result.Add(TStr + MessLineBreak + DashLine);
         if DEMMap then begin
            Result.Add('DEM=' + DEMGlb[DEMonMap].AreaName + MessLineBreak +  DEMGlb[DEMonMap].KeyDEMParams);
            if (DEMGlb[DEMonMap].DEMheader.DEMUsed = UTMbasedDEM) then TStr := 'UTM grid (zone ' + IntToStr(DEMGlb[DEMonMap].DEMheader.UTMZone) + ')' else TStr := 'True';
            if (DEMGlb[DEMonMap].DEMMapProjection = Nil) then TStr := TStr +  ' north at top of map' + MessLineBreak
            else TStr := '';
            Result.Add(TStr + MessLineBreak + DashLine);
         end
         else if ValidSatImage(MapDraw.SatOnMap) then begin
            {$IfDef ExSat}
            {$Else}
               Result.Add('Image: ' + SatImage[MapDraw.SatOnMap].SceneBaseName + MessLineBreak);
               Result.Add('Image points on map: ' + IntToStr(succ(round(MapCorners.BoundBoxDataGrid.xmax - MapCorners.BoundBoxDataGrid.xmin))) +
                   ' x ' + IntToStr(succ(round(abs(MapCorners.BoundBoxDataGrid.ymax - MapCorners.BoundBoxDataGrid.ymin)))) + MessLineBreak);
               Result.Add(SatImage[SATonMap].ImageKeyInfo + DashLine);
               Result.Add(SatImage[SATonMap].SatelliteImageLimits + DashLine);

               TempList := LandsatSceneMetadata(ExtractFilePath(SatImage[SATonMap].OriginalFileName) + SatImage[SATonMap].SceneBaseName,ib,SatImage[SATonMap].SceneBaseName);
               if (TempList <> Nil) then begin
                   for I := 0 to pred(TempList.Count) do Result.Add(TempList.Strings[i] + MessLineBreak);
                   Result.Add(DashLine);
               end;
               AddProjectionToResult(SatImage[SatOnMap].ImageMapProjection);
            {$EndIf}
         end;
      end;
      Decs := 8;
      Result.Add('Map Window Geographic corner coordinates' + bbLimits(MapDraw.MapCorners.BoundBoxGeo) +  MessLineBreak +
                '   NW: ' + LatLongDegreeToString(MapDraw.MapCorners.BoundBoxGeo.ymax,MapDraw.MapCorners.BoundBoxGeo.xmin,MDDef.OutPutLatLongMethod) +
                '----------NE: ' + LatLongDegreeToString(MapDraw.MapCorners.BoundBoxGeo.ymax,MapDraw.MapCorners.BoundBoxGeo.xmax, MDDef.OutPutLatLongMethod) + MessLineBreak +
                '   SW: ' + LatLongDegreeToString(MapDraw.MapCorners.BoundBoxGeo.ymin,MapDraw.MapCorners.BoundBoxGeo.xmin,MDDef.OutPutLatLongMethod)   +
                '----------SE: ' + LatLongDegreeToString(MapDraw.MapCorners.BoundBoxGeo.ymin,MapDraw.MapCorners.BoundBoxGeo.xmax, MDDef.OutPutLatLongMethod) + MessLineBreak + DashLine);

         if (MapDraw.BasicProjection = bpLatLong) then Decs := 8 else Decs := 1;
         Result.Add('Map Window Projected coordinates: ' + bbLimits(MapDraw.MapCorners.BoundBoxProj) + MessLineBreak +
                    RealToString(MapDraw.MapCorners.BoundBoxProj.ymax,20,Decs) + MessLineBreak +
                    RealToString(MapDraw.MapCorners.BoundBoxProj.xmin,12,Decs) + RealToString(MapDraw.MapCorners.BoundBoxProj.xmax,16,Decs) + MessLineBreak +
                    RealToString(MapDraw.MapCorners.BoundBoxProj.ymin,20,Decs) + MessLineBreak + DashLine);

         Decs := 1;
         Result.Add('Map Window UTM coordinates, zone ' + IntToStr(MapDraw.PrimMapProj.projUTMZone) + ': ' + bbLimits(MapDraw.MapCorners.BoundBoxUTM) + MessLineBreak +
                    RealToString(MapDraw.MapCorners.BoundBoxUTM.ymax,20,Decs) + MessLineBreak +
                    RealToString(MapDraw.MapCorners.BoundBoxUTM.xmin,12,Decs) + RealToString(MapDraw.MapCorners.BoundBoxUTM.xmax,16,Decs) + MessLineBreak +
                    RealToString(MapDraw.MapCorners.BoundBoxUTM.ymin,20,Decs) + MessLineBreak + DashLine);

      if (MapDraw.VectorIndex = 0) then begin
         Decs := 0;
         Result.Add('Map Window Data Grid coordinates: ' + bbLimits(MapDraw.MapCorners.BoundBoxDataGrid) +  MessLineBreak +
                    RealToString(MapDraw.MapCorners.BoundBoxDataGrid.ymax,14,0) + MessLineBreak +
                    RealToString(MapDraw.MapCorners.BoundBoxDataGrid.xmin,8,0) + RealToString(MapDraw.MapCorners.BoundBoxDataGrid.xmax,10,0) + MessLineBreak +
                    RealToString(MapDraw.MapCorners.BoundBoxDataGrid.ymin,14,0) + MessLineBreak + DashLine);
      end;

      {$IfDef ExMag}
      {$Else}
         if (MagModel <> Nil) and (MapDraw.MapCorners.BoundBoxGeo.xmax - MapDraw.MapCorners.BoundBoxGeo.xmin < 6.01) then begin
            Result.Add(MessLineBreak + DashLine + MessLineBreak);
            if (GridTrueAngle > -99) then  Result.Add('Magnetic declination:' + RealToString(MapMagDec,6,1) + '°');
            Result.Add('UTM Grid declination:' + RealToString(GridTrueAngle,6,1) + '°');
         end;
      {$EndIf}
      if ValidDEMonMap and (not DEMMap) then Result.Add('Elevations from ' + 'DEM=' + DEMGlb[DEMonMap].AreaName + MessLineBreak + DEMGlb[DEMonMap].KeyDEMParams + MessLineBreak);
   end;
end;


procedure TMapForm.Maplibrary1Click(Sender: TObject);
begin
   {$IfDef RecordIndex} WriteLineToDebugFile('TMapForm.Maplibrary1Click in'); {$EndIf}
   AllowMapLibraryLoads := true;
   DEM_Indexes.SetUpDataBaseForOperations(true);
   Maplibrarycoverage1Click(Sender);
   CheckProperTix;
   ChangeDEMNowDoing(OpenMapsFromLibrary);
end;

procedure TMapForm.Maplibrarycoverage1Click(Sender: TObject);
begin
   {$IfDef RecordIndex} WriteLineToDebugFile('TMapForm.Maplibrarycoverage1Click in'); {$EndIf}
   Maplibrarycoverage1.Checked := not Maplibrarycoverage1.Checked;
   MapDraw.ShowMapLibraryCoverage := Maplibrarycoverage1.Checked;
   DoFastMapRedraw;
end;


procedure TMapForm.DrawCollaredMap(var TickSize,LabelSize : float64; var Bitmap2 : tMyBitmap);
var
   tc,bc,lc,rc,xp,yp,{i,}x1,y1,x2,y2,LastX,xim,yim,ylab  : integer;
   LatRange,x,y,Lat,Long : float64;
   OutMethod : tLatLongMethod;
   TStr: ShortString;
   ch : char;

           procedure DrawTheLatLongGraticule;
           var
              i,startX : integer;
            begin
             {$IfDef RecordPrinter} WriteLineToDebugFile('TMapForm.DrawCollaredMap lat/long start'); {$EndIf}

            {draw left side lat}
                Lat := round( MapDraw.MapCorners.BoundBoxGeo.ymin / TickSize) * TickSize;
                while (Lat < MapDraw.MapCorners.BoundBoxGeo.ymax) do begin
                   {$IfDef RecordCollarMapMargins} WriteLineToDebugFile('left side lat=' + RealToString(Lat,-18,-3)); {$EndIf}
                   MapDraw.LatLongDegreeToScreen(Lat, MapDraw.MapCorners.BoundBoxGeo.xmin,x1,y1{,true});
                   MapDraw.LatLongDegreeToScreen(Lat+TickSize,MapDraw.MapCorners.BoundBoxGeo.xmin,x2,y2{,true});
                   if y2 < 0 then y2 := 0;
                   if y1 > yim then y1 := Yim;
                   for i := -2 to 2 do begin
                      Bitmap2.Canvas.MoveTo(lc-3+i,tc + y1);
                      Bitmap2.Canvas.LineTo(lc-3+i,tc + y2);
                   end;
                   Lat := Lat + 2 * TickSize;
                end;

            {label left side lat}
                Lat := round( MapDraw.MapCorners.BoundBoxGeo.ymin / LabelSize) * LabelSize;
                while Lat < MapDraw.MapCorners.BoundBoxGeo.ymax do begin
                   MapDraw.LatLongDegreeToScreen(Lat,MapDraw.MapCorners.BoundBoxGeo.xmin,x1,y1);
                   {$IfDef RecordCollarMapMargins} WriteLineToDebugFile('check lat=' + RealToString(Lat,-18,-3)); {$EndIf}
                   if (y1 > yim) then y1 := Yim;
                   if (Lat >= MapDraw.MapCorners.BoundBoxGeo.ymin) then begin
                      {$IfDef RecordCollarMapMargins} WriteLineToDebugFile('label lat=' + RealToString(Lat,-18,-3)); {$EndIf}
                      Bitmap2.Canvas.MoveTo(lc-5,tc+y1);
                      Bitmap2.Canvas.LineTo(lc-10,tc+y1);
                      TStr := LatToString(Lat,OutMethod);
                      if MDDef.HorizGratText then begin
                          y1 := tc+y1-Bitmap2.Canvas.TextHeight(TStr) div 2;
                          if y1 < 2 then y1 := 2;
                          Bitmap2.Canvas.TextOut(lc-Bitmap2.Canvas.TextWidth(TStr)-15,y1,TStr);
                      end
                      else TextOutVertical(Bitmap2.Canvas,lc-10-Bitmap2.Canvas.TextHeight(TStr),tc+y1+Bitmap2.Canvas.TextWidth(TStr) div 2   ,TStr);
                   end;
                   Lat := Lat + LabelSize;
                end;

             {$IfDef RecordPrinter}WriteLineToDebugFile('TMapForm.DrawCollaredMap 3');{$EndIf}

            {draw right side lat}
                Lat := round( MapDraw.MapCorners.BoundBoxGeo.ymin / TickSize) * TickSize;
                while Lat < MapDraw.MapCorners.BoundBoxGeo.ymax do begin
                   MapDraw.LatLongDegreeToScreen(Lat,MapDraw.MapCorners.BoundBoxGeo.xmax,x1,y1{,true});
                   MapDraw.LatLongDegreeToScreen(Lat+TickSize,MapDraw.MapCorners.BoundBoxGeo.xmax,x2,y2{,true});
                   if (y2 < 0) then y2 := 0;
                   if (y1 > yim) then y1 := Yim;
                   for i := -2 to 2 do begin
                      Bitmap2.Canvas.MoveTo(lc+2+xim+i,tc+y1);
                      Bitmap2.Canvas.LineTo(lc+2+xim+i,tc+y2);
                   end;
                   Lat := Lat + 2 * TickSize;
                end;
                Lastx := 0;

           {draw bottom long}
                Long := round(MapDraw.MapCorners.BoundBoxGeo.xmin / TickSize) * TickSize;
                while Long < MapDraw.MapCorners.BoundBoxGeo.xmax  do begin
                   {$IfDef RecordCollarMapMargins} WriteLineToDebugFile('check long=' + RealToString(Long,-18,-3));{$EndIf}
                   MapDraw.LatLongDegreeToScreen(MapDraw.MapCorners.BoundBoxGeo.ymin,Long,x1,y1{,true});
                   MapDraw.LatLongDegreeToScreen(MapDraw.MapCorners.BoundBoxGeo.ymin,Long+TickSize,x2,y2{,true});
                   if (x1 < 0) then x1 := 0;
                   if (x2 > xim) then x2 := xim;
                   if (x2 > x1) then begin
                      for i := -2 to 2 do begin
                         Bitmap2.Canvas.MoveTo(lc+x1,tc+3+yim+i);
                         Bitmap2.Canvas.LineTo(lc+x2,tc+3+Yim+i);
                      end;
                   end
                   else Long := Long - TickSize;
                   Long := Long + 2 * TickSize;
                end;

             {$IfDef RecordPrinter}WriteLineToDebugFile('TMapForm.DrawCollaredMap 4'); {$EndIf}

            {label bottom long}
                Long := round(MapDraw.MapCorners.BoundBoxGeo.xmin / LabelSize) * LabelSize;
                LastX := 0;
                while Long < MapDraw.MapCorners.BoundBoxGeo.xmax do begin
                   {$IfDef RecordCollarMapMargins} WriteLineToDebugFile('check long=' + RealToString(Long,-18,-3)); {$EndIf}
                   MapDraw.LatLongDegreeToScreen(MapDraw.MapCorners.BoundBoxGeo.ymin,Long,x1,y1);
                   TStr := LongToString(Long,OutMethod);
                   {$IfDef RecordCollarMapMargins} WriteLineToDebugFile('x1=' + IntToStr(x1)); {$EndIf}
                   Bitmap2.Canvas.MoveTo(lc+x1,tc+yim+6);
                   Bitmap2.Canvas.LineTo(lc+x1,tc+yim+10);
                   if lc + x1-(Bitmap2.Canvas.TextWidth(TStr) div 2) > LastX then begin
                      {$IfDef RecordCollarMapMargins} WriteLineToDebugFile('label long=' + RealToString(Long,-18,-3)); {$EndIf}
                      StartX := lc + x1 - (Bitmap2.Canvas.TextWidth(TStr) div 2);
                      if (StartX > LastX) and ( (StartX + Bitmap2.Canvas.TextWidth(TStr)) < (lc + MapDraw.MapXSize)) then begin
                         Bitmap2.Canvas.TextOut(StartX,tc+yim+15,TStr);
                         Bitmap2.Canvas.MoveTo(lc+x1,tc+yim+6);
                         Bitmap2.Canvas.LineTo(lc+x1,tc+yim+13);
                         LastX := StartX + Bitmap2.Canvas.TextWidth(TStr + '  ');
                      end;
                   end;
                   Long := Long + LabelSize;
                end;

            {Draw top long}
                Long := round(MapDraw.MapCorners.BoundBoxGeo.xmin / TickSize) * TickSize;
                while Long < MapDraw.MapCorners.BoundBoxGeo.xmax do begin
                   MapDraw.LatLongDegreeToScreen(MapDraw.MapCorners.BoundBoxGeo.ymax,Long,x1,y1{,true});
                   MapDraw.LatLongDegreeToScreen(MapDraw.MapCorners.BoundBoxGeo.ymax,Long+TickSize,x2,y2{,true});
                   if (x1 < 0) then x1 := 0;
                   if (x2 > xim) then x2 := xim;
                   if x2 > x1 then begin
                      for i := -2 to 2 do begin
                         Bitmap2.Canvas.MoveTo(lc+x1,tc-3+i);
                         Bitmap2.Canvas.LineTo(lc+x2,tc-3+i);
                      end;
                   end
                   else Long := Long - TickSize;
                   Long := Long + 2 * TickSize;
                end;
               {$IfDef RecordPrinter} WriteLineToDebugFile('TMapForm.DrawCollaredMap lat/long done'); {$EndIf}
             end;


         procedure DrawUTM;
         begin
            {$IfDef RecordPrinter} WriteLineToDebugFile('TMapForm.DrawCollaredMap UTM start'); {$EndIf}
            MapDraw.SetUTMTickInt(MapDraw.UTMTickInt);
            {******* label Horizontal grid lines ********}
            y := MapDraw.UTMTickInt * trunc(MapDraw.MapCorners.BoundBoxUTM.ymin / MapDraw.UTMTickInt);
            yLab := 1;
            while y < MapDraw.MapCorners.BoundBoxUTM.ymax + MapDraw.UTMTickInt do begin
               y := y + MapDraw.UTMTickInt;
               if MDDef.FullUTMGridLabels or (abs(frac(0.001 * y + 0.0005)) < 0.001) then begin
                  MapDraw.UTMtoScreen(MapDraw.MapCorners.BoundBoxUTM.xmax,y,xp,yp);
                  MapDraw.UTMtoScreen(MapDraw.MapCorners.BoundBoxUTM.xmax,y + MapDraw.UTMTickInt,x2,y2);
                  if (YP >0) and (yp < MapDraw.MapYSize) then begin
                     {$IfDef RecordPrinter}   WriteLineToDebugFile('UTM ' + RealToString(y,12,0) + ' at y=' + IntToStr(yp)); {$EndIf}
                     if MDDef.JustSimpleGrid then begin
                        if y2 < MapDraw.MapYSize then begin
                           TStr := IntToStr(yLab);
                           Bitmap2.Canvas.TextOut(lc+xim+20,(yp+y2) div 2 + Bitmap2.Canvas.TextHeight(TStr),TStr);
                           inc(YLab);
                        end;
                     end
                     else begin
                        Bitmap2.Canvas.MoveTo(lc + 6 + xim,tc+yp);
                        Bitmap2.Canvas.LineTo(lc + 10 + xim,tc+yp);
                        if MDDef.FullUTMGridLabels then begin
                           TStr := RealToString(y,-12,0);
                           TextOutVertical(Bitmap2.Canvas,lc+xim+20,tc+yp+Bitmap2.Canvas.TextWidth(TStr) div 2,TStr,True);
                        end
                        else begin
                           TStr := UTMGridLabel(y);
                           Bitmap2.Canvas.TextOut(lc+xim+20,tc+yp-Bitmap2.Canvas.TextHeight(TStr) div 2,TStr);
                           if (MDDef.MapTicks in [tixUTM]) then begin
                              MapDraw.UTMtoScreen(MapDraw.MapCorners.BoundBoxUTM.xmin,y,xp,yp);
                              Bitmap2.Canvas.TextOut(lc-10-Bitmap2.Canvas.TextWidth(TStr),tc+yp-Bitmap2.Canvas.TextHeight(TStr) div 2,TStr);
                           end;
                        end;
                     end;
                  end;
               end;
            end {while};

            {$IfDef RecordPrinter} WriteLineToDebugFile('TMapForm.PreparePrinterImage1Click 6'); {$EndIf}

            {******* label vertical grid lines ********}
            ch := 'A';
            x := MapDraw.UTMTickInt * trunc(MapDraw.MapCorners.BoundBoxUTM.xmin / MapDraw.UTMTickInt);
            while x < MapDraw.MapCorners.BoundBoxUTM.xmax + MapDraw.UTMTickInt do begin
               x := x + MapDraw.UTMTickInt;
               if MDDef.FullUTMGridLabels or (abs(frac(0.001 * x + 0.0005)) < 0.001) then begin
                  MapDraw.UTMtoScreen(x,MapDraw.MapCorners.BoundBoxUTM.ymax,xp,yp);
                  MapDraw.UTMtoScreen(x + MapDraw.UTMTickInt,MapDraw.MapCorners.BoundBoxUTM.ymax,x2,y2);
                  if (xp > 0) and (xp < MapDraw.MapXSize) then begin
                     {$IfDef RecordPrinter} WriteLineToDebugFile('UTM ' + RealToString(x,12,0) + ' at x=' + IntToStr(xp)); {$EndIf}
                     Bitmap2.Canvas.MoveTo(lc+xp,tc-5);
                     Bitmap2.Canvas.LineTo(lc+xp,tc-10);
                     if MDDef.JustSimpleGrid then begin
                        if x2 < MapDraw.MapXSize then begin
                           TStr := ch;
                           Bitmap2.Canvas.TextOut(lc+(xp + x2) div 2 - Bitmap2.Canvas.TextWidth(TStr) div 2,tc-2-Bitmap2.Canvas.TextHeight(TStr),TStr);
                           inc(ch);
                        end;
                     end
                     else begin
                        if MDDef.FullUTMGridLabels then TStr := RealToString(x,-12,0)
                        else TStr := UTMGridLabel(x);
                        Bitmap2.Canvas.TextOut(lc+xp-Bitmap2.Canvas.TextWidth(TStr) div 2,tc-2-Bitmap2.Canvas.TextHeight(TStr),TStr);
                     end;
                  end;
               end;
            end {while};
            {$IfDef RecordPrinter} WriteLineToDebugFile('TMapForm.PreparePrinterImage1Click UTM done'); {$EndIf}
         end;

         procedure FindWhiteMarginAndMakeBitmap;
         begin
            CreateBitmap(Bitmap2,MapDraw.MapXSize,MapDraw.MapYSize);
            LoadMyFontIntoWindowsFont(MDDef.CollarUnitsFont,Bitmap2.Canvas.Font);
            if (MDDef.MapTicks in [tixUTM,tixBoth]) then begin
               tc := 35;
               rc := 60;
            end
            else begin
               tc := 6;
               rc := 6;
            end;
            TStr := LatToString(MapDraw.MapCorners.BoundBoxGeo.ymax,OutMethod);
            lc := 35 + Bitmap2.Canvas.TextWidth(TStr);
            bc := 20 + Bitmap2.Canvas.TextHeight(TStr);
            FreeAndNil(Bitmap2);
            CreateBitmap(Bitmap2,MapDraw.MapXSize + lc + rc,MapDraw.MapYSize + tc + bc);
            LoadMyFontIntoWindowsFont(MDDef.CollarUnitsFont,Bitmap2.Canvas.Font);
         end;


begin
   {$IfDef RecordCollarMapMargins}   WriteLineToDebugFile('TMapForm.DrawCollaredMap in, font=' + MyFontToString(MDDef.CollarUnitsFont) + ' ' + MapSizeString); {$EndIf}

   xim := MapDraw.MapXSize;
   yim := MapDraw.MapYSize;

   LatRange := (MapDraw.MapCorners.BoundBoxGeo.ymax - MapDraw.MapCorners.BoundBoxGeo.ymin);
   if (TickSize < 0) then BaseMap.SetLatLongTickInterval(LatRange,TickSize,LabelSize,OutMethod)
   else begin
      if (LabelSize > 0.999) then OutMethod := NearestDegree
      else OutMethod := NearestMinute;
   end;
   FindWhiteMarginAndMakeBitmap;

   if (MapDraw.BasicProjection in [bpUTM,bpLatLong]) or (MapDraw.ZoomableVectorMap) then begin
      {$IfDef RecordCollarMapMargins} WriteLineToDebugFile('Bitmap2 size: ' + IntToStr(Bitmap2.Width) + 'x' + IntToStr(Bitmap2.Height)); {$EndIf}
       Bitmap2.Canvas.Pen.Width := 1;
       Bitmap2.Canvas.Brush.Color := clBlack;
       Bitmap2.Canvas.Pen.Color := clBlack;
       Bitmap2.Canvas.Brush.Style := bsClear;
       Bitmap2.Canvas.Rectangle(lc-5,tc-5,lc + 6 + xim,tc+6+yim);  //large outer collar box
       Bitmap2.Canvas.Pen.Color := clBlack;
       Bitmap2.Canvas.Brush.Style := bsClear;
       Bitmap2.Canvas.Rectangle(lc-1,tc-1,lc+1+xim,tc+1+yim);       //small inner collar box

      if (MDDef.MapTicks in [tixLatLong,tixBoth]) then DrawTheLatLongGraticule;
      if (MDDef.MapTicks in [tixUTM,tixBoth]) then DrawUTM;

   //corners of collars
      Bitmap2.Canvas.Brush.Color := clWhite;
      Bitmap2.Canvas.Brush.Style := bsSolid;
      Bitmap2.Canvas.Rectangle(lc-5,tc-5,lc-1,tc-1);
      Bitmap2.Canvas.Rectangle(lc-5,tc+1+yim,lc-1,tc+6+yim);
      Bitmap2.Canvas.Rectangle(lc+1+xim,tc-5,lc + 6 + xim   ,tc-1);
      Bitmap2.Canvas.Rectangle(lc+1+xim,tc+1+yim,lc + 6 + xim   ,tc+6+yim);
      Bitmap2.Canvas.CopyRect(Rect(lc,tc,lc + xim,tc+yim),Image1.Canvas,Rect(0,0, xim,yim));
      Bitmap2.SaveToFile(MDTempDir + 'collaredmap.bmp');
   end;
   {$IfDef RecordCollarMapMargins} WriteLineToDebugFile('TMapForm.DrawCollaredMap out' + '   Bitmap2 final size: ' + IntToStr(Bitmap2.Width) + 'x' + IntToStr(Bitmap2.Height)); {$EndIf}
end;


procedure TMapForm.PreparePrinterImage1Click(Sender: TObject);
var
   SetUpLegendForm : DEM_Legend.TSetUpLegendForm;
   Bitmap2,Bitmap  : tMyBitmap;
begin
   {$IfDef RecordPrinter} WriteLineToDebugFile('TMapForm.PreparePrinterImage1Click 1'); {$EndIf}
   if (MapDraw.BasicProjection in [bpUTM,bpLatLong]) or MapDraw.ZoomableVectorMap then begin
      DEMDef_routines.SaveBackupDefaults;
      CheckThatLegendsAreOnTop;
      SetUpLegendForm := DEM_Legend.TSetUpLegendForm.Create(application);

      with SetUpLegendForm do begin
         MapOwner := Self;
         DrawCollaredMap(TickSize,LabelSize,Bitmap2);
         SetUpLegendForm.BitBtn10.Caption := 'Ticks: ' + AngleFormat(TickSize,MDDef.GraticuleUnits);
         SetUpLegendForm.BitBtn11.Caption := 'Labels: ' + AngleFormat(LabelSize,MDDef.GraticuleUnits);

         If MDDef.MapTopTitle then SetUpLegendForm.TopHeight := 50
         else SetUpLegendForm.TopHeight := 0;
         SetUpLegendForm.Edit4.Text := IntToStr(Bitmap2.Width + 20);
         SetUpLegendForm.Edit3.Text := IntToStr(Bitmap2.Height + 190 + SetUpLegendForm.TopHeight);

         PrinterImage := PetImage_form.TImageDisplayForm.Create(Application);
         PrinterImage.StatusBar1.Height := 20;
         PrinterImage.CreateOverlays;
         PrinterImage.CanCloseItself := false;
         PrinterImage.AddImageOverlay(0,SetUpLegendForm.TopHeight,Bitmap2.Width,Bitmap2.Height,'collaredmap.bmp',true);

         Bitmap := Nil;
         Bitmap := MapDraw.DrawScaleBarOnBitmap;
         if (Bitmap <> Nil)  then begin
            Bitmap.SaveToFile(MDTempDir + 'scalebar.bmp');
            PrinterImage.AddImageOverlay(150,Bitmap2.Height + 90,Bitmap.Width,Bitmap.HEIGHT,'scalebar.bmp',MDDef.PrinterLegend.ShowScaleBar);
            Bitmap.Free;
         end;

         MapDraw.DrawDeclinationDiagramOnBitmap(Bitmap);
         Bitmap.SaveToFile(MDTempDir + 'decdiag.bmp');
         PrinterImage.AddImageOverlay(0,Bitmap2.Height + 90,Bitmap.Width,Bitmap.HEIGHT,'decdiag.bmp',MDDef.PrinterLegend.ShowDeclinationDiagram);
         Bitmap.Free;

         Bitmap := MapDraw.DrawLegendOnBitmap;
         if (Bitmap <> Nil) then begin
            Bitmap.SaveToFile(MDTempDir + 'elevleg.bmp');
            PrinterImage.AddImageOverlay(0,Bitmap2.Height + 190,Bitmap.Width,Bitmap.HEIGHT,'elevleg.bmp',MDDef.PrinterLegend.ShowColorLegend);
            Bitmap.Free;
         end
         else SetUpLegendForm.CheckBox1.Enabled := false;

         {$IfDef RecordPrinter} WriteLineToDebugFile('TMapForm.PreparePrinterImage1Click 8'); {$EndIf}

         SetUpLegendForm.BitBtn1Click(Nil);
         Bitmap2.Free;
         DEMDef_routines.SaveBackupDefaults;
         if (Sender = Nil) then begin
            SetUpLegendForm.SpeedButton1Click(Sender);
            SetUpLegendForm.BitBtn5Click(Sender);
         end;
      end;
   end;
end;


procedure TMapForm.Inf1Click(Sender: TObject);
begin
    Markasmissing1Click(Sender);
end;

procedure TMapForm.INForNAN1Click(Sender: TObject);
begin
   Markasmissing1Click(Sender);
end;

procedure TMapForm.InfoSpeedButton6Click(Sender: TObject);
begin
   Map2Click(Sender);
end;


procedure TMapForm.InsertpostingsfromreferenceDEM1Click(Sender: TObject);
var
   MergeDEM : integer;
begin
   if GetDEM(MergeDEM,true,'to insert elevations into ' + DEMGlb[MapDraw.DEMonMap].AreaName) then begin
      DEMGlb[MapDraw.DEMonMap].FillHolesSelectedBoxFromReferenceDEM(DEMGlb[MapDraw.DEMonMap].FullDEMGridLimits,MergeDEM,hfJustReferencePostings);
      RespondToChangedDEM;
   end;
end;


procedure TMapForm.InsureGrayScaleReflectanceMap;
begin
  if MDDef.AutoGrayScaleReflectance and IsReflectanceMap(MapDraw.MapType) and (MapDraw.MapType <> mtGrayReflect) then begin
     MapDraw.MapType := mtGrayReflect;
     DoBaseMapRedraw;
  end;
end;

procedure TMapForm.SaveSpeedButtonClick(Sender: TObject);
begin
   Saveimage1Click(Sender);
end;


procedure TMapForm.ZoomInSpeedButton4Click(Sender: TObject);
begin
   Blowup2Click(Sender);
end;


procedure TMapForm.ZoomOutSpeedButton5Click(Sender: TObject);
begin
   Zoomout1Click(Sender);
end;

procedure TMapForm.SubsetSpeedButtonClick(Sender: TObject);
begin
   {$IfDef RecordClick} WriteLineToDebugFile('SubsetSpeedButtonClick'); {$EndIf}
   Subset1Click(Sender);
end;

procedure TMapForm.SubtractanopenDEM1Click(Sender: TObject);
var
   MergeDEM : integer;
begin
   if GetDEM(MergeDEM,true,'subract from ' + DEMGlb[MapDraw.DEMonMap].AreaName) then begin
      AddaDEM(MapDraw.DEMonMap,MergeDEM,-1);
      RespondToChangedDEM;
   end;
end;

procedure TMapForm.Subtractopengrid1Click(Sender: TObject);
var
   fName : PathStr;
   Col,Row : integer;
   z,z2 : float32;
begin
   fName := ExtractFilePath(LastDEMName);
   if LoadNewDEM(GridMaskDEM,fName,false) then begin
      if DEMGlb[MapDraw.DEMonMap].SecondGridIdentical(GridMaskDEM) then begin
         StartProgress('Subtract');
         for Col := 0 to DEMGlb[MapDraw.DEMonMap].DEMheader.NumCol do begin
            if (Col Mod 100 = 0) then UpdateProgressBar(Col/DEMGlb[MapDraw.DEMonMap].DEMheader.NumCol);
            for Row := 0 to DEMGlb[MapDraw.DEMonMap].DEMheader.NumRow do begin
               if DEMGlb[MapDraw.DEMonMap].GetElevMeters(Col,Row,z) and DEMGlb[GridMaskDEM].GetElevMeters(Col,Row,z2) then begin
                  DEMGlb[MapDraw.DEMonMap].SetGridElevation(Col,Row,z-z2);
               end;
            end;
         end;
         EndProgress;
         RespondToChangedDEM;
      end
      else MessageToContinue('Not compatible DEM');
      CloseSingleDEM(GridMaskDEM);
   end;
end;

procedure TMapForm.WeaponFanSpeedButton8Click(Sender: TObject);
begin
   {$IfDef RecordFan} WriteLineToDebugFile('WeaponFanSpeedButton8Click'): {$EndIf}
   FirstFan := true;
   ChangeDEMNowDoing(QuickWeaponsFan);
end;

procedure TMapForm.Weather2Click(Sender: TObject);
begin
   {$IfDef ExGeography}
   {$Else}
       ClimatePopupMenu18.Popup(Mouse.CursorPos.X,Mouse.CursorPos.Y);
   {$EndIf}
end;

procedure TMapForm.Western1Click(Sender: TObject);
begin
   ThinDEM1Click(Sender);
end;


procedure TMapForm.WGS84elllipsoid1Click(Sender: TObject);
begin
   DEMGlb[MapDraw.DEMonMap].DEMheader.VerticalCSTypeGeoKey := VertCSWGS84;
   DEMGlb[MapDraw.DEMonMap].DEMMapProjection.h_DatumCode := 'WGS84';
end;

procedure TMapForm.WGS84elllipsoidtoEGM200081Click(Sender: TObject);
begin
   VerticalDatumShift(MapDraw.DEMonMap,vdWGS84toEGM2008);
end;


procedure TMapForm.RangeCirclesSpeedButton9Click(Sender: TObject);
begin
   Rangecircles1Click(Sender);
end;


procedure TMapForm.Rangetosinglevalue1Click(Sender: TObject);
var
   MinRange,MaxRange,NewZ : float64;
begin
    MinRange := DEMGlb[MapDraw.DEMOnMap].DEMheader.MinElev;
    MaxRange := DEMGlb[MapDraw.DEMOnMap].DEMheader.MaxElev;
    NewZ := 0;
    if (Sender = Rangetosinglevalue1) then begin
       ReadDefault('Min value to reclassify',MinRange);
       ReadDefault('Max value to reclassify',MaxRange);
    end;
    if (Sender = Singlevalue2) then begin
       ReadDefault('Value to reclassify',MinRange);
       MaxRange := MinRange;
    end;
    ReadDefault('Reclassified z value',NewZ);
    DEMGlb[MapDraw.DEMOnMap].ReclassifyRange(MinRange,MaxRange,NewZ);
    RespondToChangedDEM;
end;


procedure TMapForm.Ransacplane1Click(Sender: TObject);
var
   InFiles,Sl1,sl2 : tStringList;
   x,y,z,Lat,Long,
   xoff,yoff,zoff : float64;
   fName,fName2 : pathStr;
   i : Integer;
   DefaultFilter : byte;
   aLine : ANSIstring;
   j: Integer;
begin
    fName := MainMapData;

    Infiles := tstringList.Create;
    Infiles.Add(fName);
    DefaultFilter := 1;
    Petmar.GetMultipleFiles('Ransac plane','planes|*.xyz',Infiles,DefaultFilter);

     fName := Infiles.Strings[0];
     fName2 := ExtractFilePath(fName) + 'ransac_offsets.txt';
     if FileExists(fName2) then begin
        sl1 := tStringList.Create;
        sl1.LoadFromFile(fName2);
        xoff := StrToFloat(sl1.strings[0]);
        yoff := StrToFloat(sl1.strings[1]);
        zoff := StrToFloat(sl1.strings[2]);
        sl1.Destroy;
     end
     else begin
        xoff := 0;
        yoff := 0;
        zoff := 0;
     end;

    for j := 0 to pred(inFiles.Count) do begin
       fName := Infiles.Strings[j];
       sl1 := tStringList.Create;
       sl1.LoadFromFile(fName);
       sl2 := tStringList.Create;
       sl2.Add('lat,long,x_utm,y_utm,z');
       for i := 1 to pred(sl1.Count) do begin
          aLine := sl1.Strings[i];
          x := StrToFloat(Petmar_types.BeforeSpecifiedCharacterANSI(aLine,',',true,true));
          y := StrToFloat(Petmar_types.BeforeSpecifiedCharacterANSI(aLine,',',true,true));
          z := StrToFloat(Petmar_types.BeforeSpecifiedCharacterANSI(aLine,',',true,true));
          x := x - xoff;
          y := y - yoff;
          MapDraw.PrimMapProj.UTMtoLatLongDegree(x,y,Lat,Long);
          sl2.Add(RealToString(Lat,-14,-7) + ',' +  RealToString(Long,-14,-7) + ','  + RealToString(x,-12,-3) + ',' + RealToString(y,-12,-3) + ',' + RealToString(z-zoff,-12,-3));
       end;
       sl1.Destroy;
       StringListToLoadedDatabase(sl2,ChangeFileExt(fName,'.csv'));
   end;
   Infiles.Destroy
end;


procedure TMapForm.Geomorphometryanalysis1Click(Sender: TObject);
begin
   {$IfDef ExComplexGeoStats}
   {$Else}
       {$IfDef RecordGeomorphometry} WriteLineToDebugFile('TMapForm.Geomorphometryanalysis1Click in'); {$EndIf}
       Pick_Geostats.DoGeoStatAnalysis;
       {$IfDef RecordGeomorphometry} WriteLineToDebugFile('TMapForm.Geomorphometryanalysis1Click out'); {$EndIf}
   {$EndIf}
end;



procedure TMapForm.Geomorphometrybycategories1Click(Sender: TObject);
var
   ElevMap,SlopeMap,RuffMap,AspMap : integer;
   Graph1,Graph2,Graph3,Graph4 : tThisBaseGraph;

begin
   GetDEM(ElevMap,true,'elevation histogram');
   SlopeMap := 0;
   RuffMap := 0;
   SlopeMap := -1;   //forces creation of slope and roughness maps
   RuffMap := CreateSlopeRoughnessSlopeStandardDeviationMap(ElevMap,3,SlopeMap);
   AspMap := MakeAspectMap(ElevMap);
   HistogramsFromVATDEM(MapDraw.DEMonMap,ElevMap,SlopeMap,RuffMap,AspMap,Graph1,Graph2,Graph3,Graph4);
end;

procedure TMapForm.Rasteraftersubsettomatchthismapextent1Click(Sender: TObject);
begin
   {$IfDef ExGDAL}
   {$Else}
      GDALsubsetGridAndOpen(MapDraw.MapCorners.BoundBoxGeo,true,'',true);
   {$EndIf}
end;

procedure TMapForm.Rasterizedatabases1Click(Sender: TObject);
begin
   //ChannelsSHPtoGrid(MapDraw.DEMonMap);
end;


procedure TMapForm.Rastertovector1Click(Sender: TObject);
begin
   {$IfDef ExAdvancedGIS}
   {$Else}
      RasterToVector(self);
   {$EndIf}
end;


procedure TMapForm.FullMapSpeedButtonClick(Sender: TObject);
begin
   RestoreFullMap;
end;


procedure TMapForm.Redraw1Click(Sender: TObject);
begin
   DoFastMapRedraw;
end;


procedure TMapForm.RedrawSpeedButton12Click(Sender: TObject);
begin
   DoFastMapRedraw;
end;

procedure TMapForm.Mean1Click(Sender: TObject);
var
   NewGrid : integer;
begin
   {$IfDef NoExternalPrograms}
   {$Else}
       NewGrid := WBT_MeanCurvature(GeotiffDEMNameOfMap);
       MatchAnotherDEMMap(NewGrid,MapDraw.DEMonMap);
   {$EndIf}
end;

procedure TMapForm.Meanfilter1Click(Sender: TObject);
begin
   ThinDEM1Click(Sender);
end;

procedure TMapForm.MeasureDistanceSpeedButton14Click(Sender: TObject);
begin
   Distance1Click(Sender);
end;


procedure TMapForm.GridSpeedButton15Click(Sender: TObject);
begin
   ChangeGridOptions(Self);
end;


procedure TMapForm.GridVATlegend1Click(Sender: TObject);
begin
   Petimage_form.DisplayBitmap(MapDraw.MakeVATLegend,DEMGlb[MapDraw.DEMOnMap].AreaName + ' Legend');
end;


procedure TMapForm.Setgridparameters1Click(Sender: TObject);
begin
   GridSpeedButton15Click(Sender);
end;


procedure TMapForm.Climograph1Click(Sender: TObject);
begin
   KoppenSpeedButton7Click(Sender);
end;

procedure TMapForm.ClipboardSpeedButtonClick(Sender: TObject);
begin
   Quickmap1Click(Sender);
   if (MDDef.ClipboardExports = 1) then Detailedmap1Click(Sender)
   else begin
       Detailedmap1.Visible := (MapDraw.BasicProjection in [bpUTM,bpLatLong] ) or (MapDraw.ZoomableVectorMap);
       PopUpMenu19.PopUp(Mouse.CursorPos.X,Mouse.CursorPos.Y);
   end;
end;


procedure TMapForm.ClipDEMtofullDEMIXtiles1Click(Sender: TObject);
begin
   ClipDEMtoFullDEMIXTiles;
end;

procedure TMapForm.ClipDEMtoregion(Limits : sfBoundBox);
begin
   if (DEMGlb[MapDraw.DEMonMap].DEMFileName = '') then DEMGlb[MapDraw.DEMonMap].DEMFileName := MDTempDir + DEMGlb[MapDraw.DEMonMap].AreaName + '.dem';
   SubsetAndZoomMapFromGeographicBounds(Limits,false);
   DEMGlb[MapDraw.DEMonMap].SaveSpecifiedPartOfDEM(DEMGlb[MapDraw.DEMonMap].DEMFileName, MapDraw.MapAreaDEMGridLimits);
   ReloadDEMClick(Nil);
end;


procedure TMapForm.ClipDEMtoregionwithdata1Click(Sender: TObject);
begin
   if (DEMGlb[MapDraw.DEMonMap].DEMFileName = '') then DEMGlb[MapDraw.DEMonMap].DEMFileName := MDTempDir + DEMGlb[MapDraw.DEMonMap].AreaName + '.dem';
   DEMGlb[MapDraw.DEMonMap].SavePartOfDEMWithData(DEMGlb[MapDraw.DEMonMap].DEMFileName);
   ReloadDEMClick(Sender);
end;

procedure TMapForm.Detailedmap1Click(Sender: TObject);
begin
   if (MapDraw.BasicProjection in [bpUTM,bpLatLong]) or (MapDraw.ZoomableVectorMap) then PreparePrinterImage1Click(Nil)
   else Quickmap1Click(Sender);
end;


procedure TMapForm.DetrendDEMgrid1Click(Sender: TObject);
begin
   ThinDEM1Click(Sender);
end;

procedure TMapForm.OutlineMap;
begin
    Image1.Canvas.Brush.Style := bsClear;
    Image1.Canvas.Pen.Width := 3;
    Image1.Canvas.Pen.Color := clBlack;
    Image1.Canvas.Rectangle(0,0,pred(Image1.Width),pred(Image1.Height));
end;


procedure TMapForm.Quickmap1Click(Sender: TObject);
var
   bmp : Graphics.TBitmap;
begin
    if MDDef.BoxAroundQuickMaps then begin
       CopyImageToBitmap(Image1,bmp);
       OutlineMap;
    end;
    AssignImageToClipBoard(Image1);
    if MDDef.BoxAroundQuickMaps then begin
       Image1.Picture.Graphic := bmp;
       bmp.Free;
    end;
end;


procedure TMapForm.Quickrotatemap1Click(Sender: TObject);
begin
    DEMelevationcolors1Click(Sender);
end;

procedure TMapForm.Elevation3Click(Sender: TObject);
begin
   {$IfDef RecordMaDraw} WriteLineToDebugFile('Picked elevation map'); {$EndIf}
   MapDraw.MapType := mtElevRainbow;
   MapDraw.MapMerge := mmNone;
   MapDraw.NeedToRedraw := true;
   Elevationcolors1Click(Sender);
end;


procedure TMapForm.Slope3Click(Sender: TObject);
begin
   MapDraw.MapType := MDDef.DefSlopeMap;
   MapDraw.MapMerge := mmNone;
   SlopeCategories1Click(Sender);
end;

procedure TMapForm.Slopeandcomponents1Click(Sender: TObject);
begin
    CreateSlopeMap(MapDraw.DEMonMap,true,true);
end;

procedure TMapForm.Slopeandroughness1Click(Sender: TObject);
var
   RuffMap,SlopeMap : integer;
begin
   SlopeMap := 0;
   RuffMap := CreateSlopeRoughnessSlopeStandardDeviationMap(MapDraw.DEMonMap,5,SlopeMap);
end;

procedure TMapForm.Reflectance3Click(Sender: TObject);
begin
   Reflectance2Click(Sender);
end;

procedure TMapForm.Contour3Click(Sender: TObject);
begin
   MapDraw.MapMerge := mmNone;
   Contour1Click(Sender);
end;

procedure TMapForm.Blank2Click(Sender: TObject);
begin
   Blank1Click(Sender);
end;

procedure TMapForm.Blankmapcolor1Click(Sender: TObject);
begin
   QueryColor(MDdef.BlankMapColor);
   DoBaseMapRedraw;
end;


procedure TMapForm.Display1Click(Sender: TObject);
begin
   with MapDraw do begin
      BaseTitle := '';
      DeleteSingleMapLayer(BaseMapFName);
      DeleteSingleMapLayer(LegendOverlayfName);
      Elevation3.Visible := ValidDEM(MapDraw.DEMonMap);
      Slope3.Visible := DEMMap;
      Reflectance3.Visible := DEMMap;
      Contour3.Visible := DEMMap and (MDDef.ProgramOption = ExpertProgram);
      Mask1.Visible := DEMMap and (MDDef.ProgramOption = ExpertProgram);
      FlowDirections1.Visible := DEMMap and (MDDef.ProgramOption = ExpertProgram);
      DEMColorMerge1.Visible := DEMMap and (NumDEMDataSetsOpen > 1) and MDDef.ShowDEMCompare;
      VATdisplay1.Visible := DEMMap and (DEMGlb[DEMonMap].DEMheader.DEMPrecision in [SmallIntDEM,ByteDEM,WordDEM]);
      DEMelevationmap1.Visible := (MapDraw.ValidDEMonMap) and (MapDraw.VectorIndex <> 0);
      Openness1.Visible := DEMMap;
      {$IfDef ExExoticMaps}
         Aspect1.Visible := false;
      {$Else}
         Aspect1.Visible := DEMMap and (MDDef.ProgramOption in [ExpertProgram,GeologyProgram]);
      {$EndIf}

      {$IfDef ExSat}
         PickBandColors1.Visible := false;
      {$Else}
         PickBandColors1.Visible := ((not DEMMap) and MapDraw.ValidSatOnMap and (SatImage[SatOnMap].NumBands > 1)) or (MapType = mtSatBlank);
      {$EndIf}
      MapParameterPopUpMenu1.PopUp(Mouse.CursorPos.X,Mouse.CursorPos.Y);
   end;
end;


function TMapForm.DisplayAndPurgeStringListDB(var TheList: tStringList; fname: PathStr; OpenTable : boolean = true) : integer;
begin
   while FileExists(fName) do fName := NextFileNumber(ExtractFilePath(fName), ExtractFileNameNoExt(fname) + '_','.csv');
   TheList.SaveToFile(fName);
   TheList.Free;
   Result := LoadDataBaseFile(fName,OpenTable);
   if (Result > 0) then begin
      GISdb[Result].dbIsUnsaved := true;
      if (GISdb[Result].dbTablef <> Nil) then GISdb[Result].dbTablef.ShowStatus;
   end;
end;


procedure TMapForm.SlopeCategories1Click(Sender: TObject);
begin
   if ChangeSlopeMapOptions(Self) then begin
      MapDraw.DeleteSingleMapLayer(MapDraw.BaseMapFName);
      MapDraw.DeleteSingleMapLayer(MapDraw.LegendOverlayfName);
      DrawColoredMap1Click(Nil);
   end;
end;

procedure TMapForm.Duplicatemapwindow1Click(Sender: TObject);
begin
   DupeMapSpeedButton18Click(Sender);
end;


procedure TMapForm.DupeMapSpeedButton18Click(Sender: TObject);
begin
   DuplicateMap(false);
end;


procedure TMapForm.Pastefromclipboard1Click(Sender: TObject);
begin
   DragBitmap := tMyBitmap.Create;
   DragBitmap.Assign(ClipBoard);
   if (DragBitmap.Width = Image1.Width) and (DragBitmap.Height = Image1.Height)  then begin
      Image1.Picture.Graphic := DragBitmap;
      FreeAndNil(DragBitmap);
   end
   else begin
      CopyImageToBitmap(Image1,SavedMapImage);
      ChangeDEMNowDoing(DragEdit);
   end;
end;


procedure TMapForm.PickBandSpeedButton20Click(Sender: TObject);
begin
   if ValidSatImage(MapDraw.SatOnMap) then GetContrast(self);
end;

procedure TMapForm.PickcornersMDDEM1Click(Sender: TObject);
var
   GridLimits : tGridLimits;
begin
   GridLimits := MapDraw.MapAreaDEMGridLimits;;
   PickLimits(DEMGlb[MapDraw.DEMonMap],Self,GridLimits);
   DEMGlb[MapDraw.DEMonMap].RectangleSubsetDEM(GridLimits);
end;

procedure TMapForm.Pickfilter1Click(Sender: TObject);
begin
   ThinDEM1Click(Sender);
end;

procedure TMapForm.Pickmapsforbigimage1Click(Sender: TObject);
var
   Maps : tStringList;
begin
   PickMaps(Maps,'Maps for to combine on big figure');
   Bigimagewithallmaps(3,'',Maps);
   Maps.Free;
end;

procedure TMapForm.PickseriesandloadDEMsfromlibrary1Click(Sender: TObject);
begin
   AdjustMapLibraryDataBaseSeries;
   LoadDEMsCoveringBox(MapDraw.MapCorners.BoundBoxGeo,true);
end;

procedure TMapForm.PicksingleDEMseriesthisarea1Click(Sender: TObject);
//var
   //sName : ShortString;
begin
   //PickDEMSeries(sName,'DEM series to load for this map area');
   DEMfromseries1Click(Sender);
end;

procedure TMapForm.Pixelextentandhighresaverage1Click(Sender: TObject);
var
   xg, yg : double;
   xi,yi : integer;
   bb : sfBoundBox;
   i : integer;
   z : float32;
   Lat,Long : float64;
   Results : tStringList;
   gl : tGridLimits;
   mv : tMomentVar;
begin
    DEMGlb[MapDraw.DEMonMap].LatLongDegreeToDEMGrid(RightClickLat,RightClickLong,xg,yg);
    xi := round(xg);
    yi := round(yg);
    DEMGlb[MapDraw.DEMonMap].DEMGridToLatLongDegree(xi,yi,Lat,Long);
    bb := DEMGlb[MapDraw.DEMonMap].PixelBoundBoxGeo(xi,yi);
    if DEMGlb[MapDraw.DEMonMap].GetElevMetersOnGrid(xi,yi,z) then begin
       OutlineGeoBox(bb,clRed,3);
       Results := tStringList.Create;
       Results.Add('Click at:      ' + LatLongDegreeToString(RightClickLat,RightClickLong));
       Results.Add('Nearest pixel: ' + LatLongDegreeToString(Lat,Long) + '  ' + DEMGridString(xi,yi));
       Results.Add(DEMGlb[MapDraw.DEMonMap].AreaName + '   z=' + RealToString(z,-8,-2));
       Results.Add('');
       Results.Add('Contributing area elevations');
       for i := 1 to MaxDEMDataSets do begin
         if ValidDEM(i) and (DEMGlb[i].SelectionMap <> Nil) then begin
            DEMGlb[i].SelectionMap.OutlineGeoBox(bb,clRed,3);
            DEMGlb[i].LatLongDegreeToDEMGridInteger(bb.ymin,bb.xmin,gl.XGridLow,gl.YGridLow);
            DEMGlb[i].LatLongDegreeToDEMGridInteger(bb.ymax,bb.xmax,gl.XGridHigh,gl.YGridHigh);
            mv := DEMGlb[i].ElevationMoments(gl);
            DEMGlb[i].GetElevFromLatLongDegree(Lat,Long,z);
            if mv.NPts > 0 then begin
               Results.Add(DEMGlb[i].AreaName + '  nearest z=' + RealToString(z,-8,-2) + '     pt in pixel=' + IntToStr(mv.Npts) + '  mean z=' + RealToString(mv.Mean,-8,-2));
            end;
         end;
      end;
      WriteStringListToDebugFile(Results);
      DisplayAndPurgeStringList(Results,'All grids at location');
    end;
    ShowDefaultCursor;
end;

procedure TMapForm.Pixelsize1Click(Sender: TObject);
var
   xg, yg : double;
begin
   if MapDraw.ValidDEMonMap then begin
      DEMGlb[MapDraw.DEMonMap].LatLongDegreeToDEMGrid(RightClickLat,RightClickLong,xg,yg);
      MessageToContinue(DEMGlb[MapDraw.DEMonMap].PixelSize(round(xg),round(yg)));
   end;
end;

procedure TMapForm.KeyClick(Sender: TObject);
var
   aMaxLat,aMinLat,aMaxLong,aMinLong : float64;
   GridLimits: tGridLimits;
begin
   if LockMaps then begin
      MessageToContinue('Maps locked for digitizing');
      exit;
   end;
    if (MapDraw.ValidDEMonMap) then begin
      {$IfDef ShowPickGridLimits}  MapDraw.ShowMapGridLimits('TMapForm.Pickgridlimits1Click in');      {$EndIf}
      GridLimits := MapDraw.MapAreaDEMGridLimits;
      Pick_Limits.PickLimits(DEMGlb[MapDraw.DEMonMap],Self,GridLimits);
      DEMGlb[MapDraw.DEMonMap].DEMGridToLatLongDegree(GridLimits.XGridLow,GridLimits.YGridLow,aMinLat,aMinLong);
      DEMGlb[MapDraw.DEMonMap].DEMGridToLatLongDegree(GridLimits.XGridHigh,GridLimits.YGridHigh,aMaxLat,aMaxLong);
      {$IfDef ShowPickGridLimits} MapDraw.ShowMapGridLimits('TMapForm.Pickgridlimits1Click pt 2');      {$EndIf}
      FullMapSpeedButton.Enabled := true;
    end
    else begin
       GetLatLongNoDatum('NW corner',aMaxLat,aMinLong);
       GetLatLongNoDatum('SE corner',aMinLat,aMaxLong);
    end;

    MapDraw.DeleteMapSavedLayers;
    MapDraw.MapXSize := MDdef.DefaultMapXSize;
    MapDraw.MapYSize := MDdef.DefaultMapYSize;
    MapDraw.MaximizeLatLongMapCoverage(aMinLat,aMinLong,aMaxLat,aMaxLong);
    DrawColoredMap1Click(Nil);
end;

procedure TMapForm.BandColor1Click(Sender: TObject);
begin
   PickBandSpeedButton20Click(Nil);
end;


procedure TMapForm.Bandratio1Click(Sender: TObject);
begin
   NewSatWindow(nsbRatio);
end;

procedure TMapForm.Basicstatistics1Click(Sender: TObject);
begin
   {$IfDef ExAdvancedSats}
   {$Else}
      MultiGridArray[MapDraw.MultiGridOnMap].BasicStats;
   {$EndIf}
end;

procedure TMapForm.Accumulatedcostsurface1Click(Sender: TObject);
begin
   ChangeDEMNowDoing(StartAccumulatedCostSurface);
end;


procedure TMapForm.ACOLITEprocessing1Click(Sender: TObject);
begin
    if SatImage[MapDraw.SATonMap].IsSentinel2 and (not  StrUtils.AnsiContainsText(SatImage[MapDraw.SatOnMap].OriginalFileName,'SAFE')) then begin
       MessageToContinue('Must pick the SAFE directory for Sentinel-2 imagery to use ACOLITE');
    end
    else begin
       if FullMapSpeedButton.Enabled or AnswerIsYes('Process full scene (very slow)') then ACOLITEprocessing(Self);
    end;
end;


procedure TMapForm.AddanopenDEM1Click(Sender: TObject);
var
   MergeDEM : integer;
begin
   if GetDEM(MergeDEM,true,'add to ' + DEMGlb[MapDraw.DEMonMap].AreaName) then begin
      AddaDEM(MapDraw.DEMonMap,MergeDEM);
      RespondToChangedDEM;
   end;
end;

procedure TMapForm.Addbands1Click(Sender: TObject);
begin
   NewSatWindow(nsbAddImages);
end;

procedure TMapForm.Addfiles1Click(Sender: TObject);
var
   fName : PathStr;
begin
   FName := DBDir + 'groups\';
   if GetFileFromDirectory('Shape file group',DefaultDBMask,FName) then DEM_Indexes.DefineShapeFileGrouping(fName);
end;

procedure TMapForm.Pickbandcolors1Click(Sender: TObject);
begin
   PickBandSpeedButton20Click(Nil);
end;

procedure TMapForm.Contrast2Click(Sender: TObject);
begin
   PickBandSpeedButton20Click(Nil);
end;

procedure TMapForm.FormActivate(Sender: TObject);
begin
   {$IfDef RecordActivate} WriteLineToDebugFile('Activate map, DEM=' + IntToStr(MapDraw.DEMonMap) + '  & Image=' + IntToStr(MapDraw.SatonMap) ); {$EndIf}
   if (MapDraw = Nil) or MapDraw.ClosingMapNow or (not MapDraw.MapDrawValid) or (DEMNowDoing in [Calculating]) then exit;
   MouseIsDown := false;
   if MapDraw.DEMMap and ValidDEM(MapDraw.DEMonMap) then exit
   else if {not} ValidSatImage(MapDraw.SatOnMap) then exit
   else if (MapDraw.VectorIndex <> 0) then exit;
   Closable := true;
   Close;
end;

procedure TMapForm.Topographicgrain2Click(Sender: TObject);
begin
   {$IfDef ExTopoGrain}
   {$Else}
       DrawTopoGrainOverlay(Self,Nil);
   {$EndIf}
end;

procedure TMapForm.Grainbyregionsize1Click(Sender: TObject);
begin
   {$IfDef ExTopoGrain}
   {$Else}
      ChangeDEMNowDoing(GrainByRegionSize);
      DEMOptions.GetFabricOptions(MapDraw.DEMonMap);
   {$EndIf}
end;


procedure TMapForm.Grassprofilecurvature1Click(Sender: TObject);
var
   NewGrid : integer;
begin
   {$IfDef NoExternalPrograms}
   {$Else}
       NewGrid := GRASSProfileCurvatureMap(GeotiffDEMNameOfMap);
       MatchAnotherDEMMap(NewGrid,MapDraw.DEMonMap);
   {$EndIf}
end;

procedure TMapForm.GRASSslopeHorn1Click(Sender: TObject);
var
   NewGrid : integer;
begin
   {$IfDef NoExternalPrograms}
   {$Else}
       NewGrid := GRASSSlopeMap(GeotiffDEMNameOfMap);
       MatchAnotherDEMMap(NewGrid,MapDraw.DEMonMap);
   {$EndIf}
end;

procedure TMapForm.Grasstangentialcurvature1Click(Sender: TObject);
var
   NewGrid : integer;
begin
   {$IfDef NoExternalPrograms}
   {$Else}
       NewGrid := GRASSTangentialCurvatureMap(GeotiffDEMNameOfMap);
       MatchAnotherDEMMap(NewGrid,MapDraw.DEMonMap);
   {$EndIf}
end;

procedure TMapForm.GRASSTPI1Click(Sender: TObject);
var
   NewGrid : integer;
begin
   {$IfDef NoExternalPrograms}
   {$Else}
       NewGrid := GRASSTPIMap(GeotiffDEMNameOfMap);
       MatchAnotherDEMMap(NewGrid,MapDraw.DEMonMap);
   {$EndIf}
end;

procedure TMapForm.GRASSTRI1Click(Sender: TObject);
var
   NewGrid : integer;
begin
   {$IfDef NoExternalPrograms}
   {$Else}
       NewGrid := GRASSTRIMap(GeotiffDEMNameOfMap);
       MatchAnotherDEMMap(NewGrid,MapDraw.DEMonMap);
   {$EndIf}
end;

procedure TMapForm.GRASSvectorruggedness1Click(Sender: TObject);
var
   WindowSize : integer;
begin
   WindowSize := 5;
   ReadDefault('window size (pixels)',WindowSize);
   GrassVectorRuggedness(GeotiffDEMNameOfMap,WindowSize);
end;

procedure TMapForm.Band1Click(Sender: TObject);
begin
   {$IfDef ExSat}
   {$Else}
      PickBandSpeedButton20Click(Sender);
   {$EndIf}
end;


procedure TMapForm.GE_SpeedButtonClick(Sender: TObject);
begin
   MDDef.AskAboutKMLExport := false;
   MDDef.KMLOutputOption := 1;
   ExportMapToGoogleEarth(false);
end;



procedure TMapForm.Colors2Click(Sender: TObject);
begin
   Elevationcolors1Click(Sender);
end;


procedure TMapForm.Colorsfromgridcategories1Click(Sender: TObject);
{$IfDef ExAdvancedGIS}
begin
{$Else}
var
   ImpCat,x,y,z,ThisCat,Cats,Total : integer;
   bmp : tMyBitmap;
   zf : float32;
   xg,yg : float32;
   p0 : pRGB;
   CatPCforLegend : float64;
   aColor : tColor;
   Hist : array[0..MaxVatCats] of integer;
   Colors : array[0..MaxVatCats] of Windows.TRGBTriple;
begin
   CatPCforLegend := 2;
   ReadDefault('Category percentage for legend',CatPCforLegend);
   ShowHourglassCursor;
   for x := 0 to 4800 do Hist[x] := 0;

   Total := 0;
   for y := round(MapDraw.MapCorners.BoundBoxDataGrid.ymin) to round(MapDraw.MapCorners.BoundBoxDataGrid.ymax) do begin
      for x := round(MapDraw.MapCorners.BoundBoxDataGrid.xmin) to round(MapDraw.MapCorners.BoundBoxDataGrid.xmax) do begin
         if DEMGlb[MapDraw.DEMonMap].GetElevMetersOnGrid(x,y,zf) then begin
            z := round(zf);
            if (z >= 0) and (z <= MaxVatCats) then begin
               Inc(Hist[z]);
               Inc(Total);
            end;
         end;
      end;
   end;

   Cats := 0;
   for x := 0 to MaxVatCats do if (Hist[x] > 0) then inc(cats);

   ThisCat := 0;
   for x := 0 to MaxVatCats do if (Hist[x] > 0) then begin
      inc(ThisCat);
      aColor := RainbowColorFunct(ThisCat,1,Cats);
      Colors[x] := ConvertTColorToPlatformColor(aColor);
   end;

   CloneImageToBitmap(Image1,bmp);
   for y := 0 to pred(MapDraw.MapYSize) do begin
      p0 := bmp.ScanLine[y];
      for x := 0 to pred(MapDraw.MapXSize) do begin
         MapDraw.ScreenToDEMGrid(x,y,xg,yg);
         if DEMGlb[MapDraw.DEMonMap].GetElevMetersOnGrid(round(xg),round(yg),zf) then begin
            p0[x] := Colors[round(zf)];
         end;
      end;
   end;
   Image1.Picture.Graphic := bmp;
   bmp.free;

   ThisCat := 0;
   ImpCat := 0;
   CreateBitmap(bmp,200,25*succ(Cats) + 4);
   bmp.Canvas.Font.Size := 12;
   bmp.Canvas.Font.Style := [fsBold];

   bmp.Canvas.TextOut(5,1,'Category   % Area');
   for x := 0 to MaxVatCats do if (Hist[x] > 0) then begin
      xg := 100.0*Hist[x]/Total;
      inc(ThisCat);
      aColor := RainbowColorFunct(ThisCat,1,Cats);
      if (xg > CatPCforLegend) then begin
         inc(ImpCat);
         bmp.Canvas.Pen.Color := aColor;
         bmp.Canvas.Brush.Color := aColor;
         bmp.Canvas.Brush.Style := bsSolid;
         bmp.Canvas.Rectangle(5,(ImpCat)*25,25,succ(ImpCat)*25);
         bmp.Canvas.Brush.Style := bsClear;
         bmp.Canvas.TextOut(30,ImpCat*25 + 4,IntegerToString(x,4) + RealToString(xg,12,4));
         bmp.savetofile('c:\temp\number' + IntToStr(x) + OverlayFExt);
      end;
   end;
   bmp.Height := succ(ImpCat)*25 + 4;
   Petimage_form.DisplayBitmap(bmp,'Legend');
   ShowDefaultCursor;
{$EndIf}
end;


procedure TMapForm.ColumnsEastLimit1Click(Sender: TObject);
var
   GridLimits : tGridLimits;
   //x,y : integer;
begin
   if (Sender = Offcurrentmap1) then begin
      GridLimits := MapDraw.MapAreaDEMGridLimits;
   end
   else begin
      GridLimits := DEMGlb[MapDraw.DEMonMap].FullDEMGridLimits;
      if Sender = ColumnsEastLimit1 then ReadDefault('Delete to east of column',GridLimits.XGridHigh);
      if Sender = ColumnsWestLimit1 then ReadDefault('Delete to west of column',GridLimits.XGridLow);
      if Sender = Rowssouthoflimit1 then ReadDefault('Delete to south of row',GridLimits.YGridLow);
      if Sender = Rowsnorthoflimit1 then ReadDefault('Delete to north of row',GridLimits.YGridHigh);
   end;

   DEMGlb[MapDraw.DEMonMap].SetGridMissingOutsideBox(GridLimits.XGridLow,GridLimits.yGridLow,GridLimits.XGridHigh,GridLimits.yGridHigh);
   RespondToChangedDEM;
(*
    with DEMGlb[MapDraw.DEMonMap] do begin
       StartProgress('Check DEM');
       for x := 0 to pred(DEMheader.NumCol) do begin
          if (x mod 50 = 0) then UpDateProgressBar(x/DEMheader.NumCol);
          for y := 0 to pred(DEMheader.NumRow) do begin
             if (x > GridLimits.XGridHigh) or (x < GridLimits.XGridLow) or (y > GridLimits.YGridHigh) or (y < GridLimits.YGridLow) then begin
                SetGridMissing(x,y);
             end;
          end;
       end;
       EndProgress;
    end {with};
    DoBaseMapRedraw;
 *)
end;


procedure TMapForm.ColumnsWestLimit1Click(Sender: TObject);
begin
   ColumnsEastLimit1Click(Sender);
end;

procedure TMapForm.Erasersize1Click(Sender: TObject);
begin
   ReadDefault('Eraser size',EraserSize);
   if (EraserSize > 0) then ChangeDEMNowDoing(ErasingPoints)
   else ChangeDEMNowDoing(JustWandering);
end;


procedure TMapForm.Erodedilate1Click(Sender: TObject);
begin
   Erodeimage1Click(Sender);
end;

procedure TMapForm.Erodeimage1Click(Sender: TObject);
{$IfDef ExAdvancedSats}
begin
{$Else}
var
   DilateErodeForm: TDilateErodeForm;
begin
   DilateErodeForm := TDilateErodeForm.Create(Application);
   DilateErodeForm.BaseMap := self;
   if ValidSatImage(MapDraw.SatOnMap) then begin
     DilateErodeForm.Edit1.Text := IntToStr(MDDef.MaxSatRange);
     DilateErodeForm.Edit2.Text := IntToStr(MDDef.MinSatRange);
     if (MapDraw.Satview.WindowContrast <> MaskRange) then begin
        MapDraw.Satview.WindowContrast := MaskRange;
        DoBaseMapRedraw;
     end;
   end;
   DilateErodeForm.ShowModal;
{$EndIf}
end;

(*
procedure TMapForm.Datumshift1Click(Sender: TObject);
var
   fName : PathStr;
   ShiftDatum : tMapProjection;
begin
    {$IfDef RecordDatumShift} WriteLineToDebugFile('TMapForm.Datumshift1Click in'); {$EndIf}
    GetSecondaryDatum;
    ShiftDatum := tMapProjection.Create('TMapForm.Datumshift1Click');
    ShiftDatum.DefineDatumFromUTMZone(MDDef.PreferSecondaryDatum,MapDraw.SecondaryMapProjection.projUTMZone,MDDef.DefaultLatHemi,'TMapForm.Datumshift1Click');

    if MapDraw.PrimMapProj.H_datumcode = ShiftDatum.H_datumcode then begin
       MessageToContinue('Both datums the same, ' + MapDraw.PrimMapProj.H_datumcode);
       {$IfDef RecordDatumShift} WriteLineToDebugFile('TMapForm.Datumshift1Click fails, both datums=' + MapDraw.PrimMapProj.h_Datumcode); {$EndIf}
    end
    else begin
       ChangeDEMNowDoing(DoingDatumShift);
       {$IfDef NoUTMDiffsDatumShift} LabelDatumShift := msLatLong; {$Else}
       PopupMenu9.PopUp(Mouse.CursorPos.X,Mouse.CursorPos.Y);
       {$EndIf}
       fName := Petmar.NextFileNumber(MDTempDir, 'datum_shift_',DefaultDBExt);
       DefineAndCreateDatumShiftTable(FName);
       DatumShiftDB := LoadDataBaseFile(fName);
       {$IfDef RecordDatumShift} WriteLineToDebugFile('TMapForm.Datumshift1Click out, two datums ' + MapDraw.PrimMapProj.h_Datumcode + '   ' + ShiftDatum.h_Datumcode); {$EndIf}
    end;
end;
*)

function TMapForm.SaveDEMtoDBF(fName : PathStr; zName : shortString = 'Z'; ThinFactor : integer = -99;  FilterZ : boolean = false; ReportOut : boolean = false) : PathStr;
var
   x,y,NPts,MissingPoints,InRange : integer;
   Lat,Long : float64;
   z,Min,Max : float32;
   OutF : tStringList;
   TStr : ANSIString;
begin
   Result := '';
   if (fName = '') then begin
      if (DEMGlb[MapDraw.DEMOnMap].DEMFileName = '') then fName := MDTempDir + DEMGlb[MapDraw.DEMOnMap].AreaName + DefaultDBExt
      else fName := ChangeFileExt(DEMGlb[MapDraw.DEMOnMap].DEMFileName,DefaultDBExt);
      if not Petmar.GetFileNameDefaultExt('DBF export',DBNameMask,fName) then exit;
   end;
   fName := ChangeFileExt(fName,'.dbf');
   if (ThinFactor < 1) then begin
      ThinFactor := 1;
      TStr := IntToStr(round(MapDraw.MapCorners.BoundBoxDataGrid.xmax - MapDraw.MapCorners.BoundBoxDataGrid.xmin)) + 'x' +
              IntToStr(round(MapDraw.MapCorners.BoundBoxDataGrid.ymax - MapDraw.MapCorners.BoundBoxDataGrid.ymin));
      ReadDefault('Thin factor, map has grid points=' + TStr,ThinFactor);
   end;
   Min := DEMGlb[MapDraw.DEMOnMap].DEMheader.MinElev;
   Max := DEMGlb[MapDraw.DEMOnMap].DEMheader.MaxElev;
   if FilterZ then begin
      ReadDefault('Min z to export',Min);
      ReadDefault('Max z to export',Max);
   end;
   Result := fName;
   OutF := tStringList.Create;
   OutF.Add('LAT,LONG,' + zName);

   NPts := 0;
   MissingPoints := 0;
   InRange := 0;
   StartProgress('Grid DBF save');
   x := round(MapDraw.MapCorners.BoundBoxDataGrid.xmin);
   while x <= round(MapDraw.MapCorners.BoundBoxDataGrid.xmax) do begin
      if (x mod 10 = 0) then UpdateProgressBar((x-MapDraw.MapCorners.BoundBoxDataGrid.xmin) / (MapDraw.MapCorners.BoundBoxDataGrid.xmax - MapDraw.MapCorners.BoundBoxDataGrid.xmin));
      y := round(MapDraw.MapCorners.BoundBoxDataGrid.ymin);
      while y <= round(MapDraw.MapCorners.BoundBoxDataGrid.ymax) do begin
         inc(NPts);
         if DEMGlb[MapDraw.DEMOnMap].GetElevMetersOnGrid(x,y,z) then begin
            if (z >= Min) and (Z <= Max) then begin
              DEMGlb[MapDraw.DEMOnMap].DEMGridToLatLongDegree(x,y,Lat,Long);
              OutF.Add(RealToString(Lat,-12,-6) + ',' + RealToString(Long,-12,-6) + ',' + RealToString(z,-8,-2));
              inc(InRange);
            end;
         end
         else inc(MissingPoints);
         inc(y,ThinFactor);
      end;
      inc(x,ThinFactor);
   end;
   EndProgress;
   {$IfDef RecordGridToDBFSave} OutF.SaveToFile(MDTempDir + 'dem.csv'); {$EndIf}
   StringList2CSVtoDB(OutF,fName,true);
   if ReportOut then begin
      if (ThinFactor = 1) then TStr := 'Points on map: '
      else TStr := 'Points on thinned map: ';
      TStr := TStr + IntToStr(NPts) +  MessLineBreak;
      if (MissingPoints > 0) then TStr := TStr + 'Missing data points: ' + IntToStr(MissingPoints) +  MessLineBreak;
      TStr := TStr + 'In range points exported: ' + IntToStr(InRange);
      MessageToContinue(TStr,True);
   end;
end;


procedure TMapForm.DBFfile1Click(Sender: TObject);
begin
   SaveDEMtoDBF('','Z',-99,true,true);
end;


procedure TMapForm.DEMcolormerge1Click(Sender: TObject);
begin
   MapDraw.MapType := mtMergeTwoDEMs;
   GetDEM(MapDraw.MergeMap,false,' tint overlay');
   DrawColoredMap1Click(Nil);
end;


procedure TMapForm.Close1Click(Sender: TObject);
begin
   wmdem.ClosePopUpMenu.Popup(Mouse.CursorPos.X,Mouse.CursorPos.Y);
end;

procedure TMapForm.Close2Click(Sender: TObject);
begin
   wmdem.ClosePopUpMenu.Popup(Mouse.CursorPos.X,Mouse.CursorPos.Y);
end;

procedure TMapForm.Closepolyline1Click(Sender: TObject);
begin
   DoneWithStreamSelection;
end;


procedure TMapForm.Cloudbrightening1Click(Sender: TObject);
begin
   {$IfDef ExMultigrid}
   {$Else}
      //MultiGridArray[MapDraw.MultiGridOnMap].CloudBrighten;
   {$EndIf}
end;

procedure TMapForm.Cloudmask1Click(Sender: TObject);
begin
   {$IfDef ExLandsatQA}
      MessageToContinue('Disabled while rewriting for collection 2');
   {$Else}
      SatImage[Mapdraw.SATonMap].GetLandsatQAMap('Cloud', SatImage[Mapdraw.SATonMap].OriginalFileName,14,15);
   {$EndIf}
end;

procedure TMapForm.Endlengthmeasurement1Click(Sender: TObject);
var
   MenuStr : shortstring;
begin
   if (DEMNowDoing = LaterZigDistance) then begin
      {$IfDef MeasureDistance} WriteLineToDebugFile('DEMNowDoing = SecondDistancePoint start'); {$EndIf}
      MenuStr := 'Cumulative length: ' + SmartDistanceMetersFormat(CumLength);
      MessageToContinue(MenuStr);
      BackToWandering;
   end;
end;

procedure TMapForm.Endpolyline1Click(Sender: TObject);
begin
   DoneWithStreamSelection;
end;


procedure TMapForm.Endtrack1Click(Sender: TObject);
begin
   DBAddRecForm.Button2Click(Nil);
end;

procedure TMapForm.Ensembleclassification1Click(Sender: TObject);
begin
   Loadsummarymultipleclassifications1Click(Sender);
   EnsembleClassify;
end;

procedure TMapForm.Entiregrid1Click(Sender: TObject);
begin
    DEMGlb[MapDraw.DEMonMap].CreateWholeDEMHistogram;
end;

procedure TMapForm.Epochs1Click(Sender: TObject);
begin
   {$IfDef ExGeology}
   {$Else}
      MapDraw.MapType := mtElevFromTable;
      ElevationFixedPalette := EpochsTimeScale;
      DoBaseMapRedraw;
   {$EndIf}
end;

procedure TMapForm.Equinoxsolstice1Click(Sender: TObject);
begin
  {$IfDef ExGeography}
  {$Else}
      MDDef.SunlightSingleDay := 2;
      SunAndHorizon(Nil,0,RightClickLat,RightClickLong,false,true);
  {$EndIf}
end;

procedure ChangeDEMNowDoing(WhatTo : tDEMDoingWhat; WhatNext : tDEMDoingWhat = JustWandering; ThisCapt : shortstring = '');
const
  OLAF = 'Outline area for ';
  FPF = 'First point for ';
  SPF = 'Second point for ';
  NWCornerStr = 'NW corner, ';
  ForShapefile = ' for ShapeFile';
  R2 = 'Record to ';
  LocationFor = 'Location for ';
  V2 = 'Vertex to ';
  PointFor = 'Point for ';
  PointsFor = 'Points for ';
  LPF =  'Locate paste location';
  SaveCaption : shortstring = '';
  LS = 'Left side ';
  RS = 'Right side ';
var
   NewTitle : ShortString;
begin
   if LocalEdit and (DEMNowDoing in [PickDBRecsToMove]) and (WhatTo <> PickDBRecsToMove) then begin
      CloseAndNilNumberedDB(DBEditting);
      LocalEdit := false;
   end;
   {$IfDef ExGeology}
   {$Else}
      if (GeosymbolTable <> Nil) and (WhatTo <> GetGeologySymbols) then GeosymbolTable.Destroy;
   {$EndIf}

   DEMNowDoing := WhatTo;
   DEMDoingNext := WhatNext;

   if StartMapRoamOps(WhatTo) then begin
      MapScreenX1 := -99;
      MapScreenY1 := -99;
   end;
   if WhatTo in [SeekingSecondNewPanorama] then OldRad := -99;

   if ThisCapt <> '' then SaveCaption := ThisCapt;
   if SaveCaption  <> '' then NewTitle := SaveCaption
   else case DEMNowDoing of
        Calculating,
        VerifyingPoint,
        RoamBroadCast,
        PickToBroadcast                : NewTitle := '';
        InteractiveLOS                 : NewTitle := 'Interactive LOS';
        FirstPointSelectionAlgorithm   : NewTitle := FPF + 'selection algorithm';
        SecondPointSelectionAlgorithm  : NewTitle := SPF + 'selection algorithm';
        FirstRequiredAntenna           : NewTitle := 'Fixed antenna';
        FanSensitivity                 : NewTitle := PointFor + 'fan sensitivity';
        CompareFanAlgorithms           : NewTitle := PointFor + 'fan algorithms';
        InsertDBPoint                  : NewTitle := PointFor + ' insert new DB point';
        VisPolarOrbiter                : NewTitle := PointFor + ' satellite visibility';
        EditPointDBRecs                : NewTitle := R2 + 'edit';
        DeletePointDBRecs              : NewTitle := R2 + 'delete';
        EditZDBRecs                    : NewTitle := R2 + 'edit z value';
        PtCloudExtractPoints           : NewTitle := 'Extract pt cloud pts';
        NDVIPointTimeSeries            : NewTitle := 'Point time series';
        EditDBRecs                     : NewTitle := R2 + 'edit';
        MovePointDBRecs                : NewTitle := R2 + 'move';
        StartAccumulatedCostSurface    : NewTitle := 'Start of accumulated cost surface';
        StartLeastCostPath             : NewTitle := 'Start of least cost path';
        RecolorShapeRecord             : NewTitle := 'Line to recolor';
        RadiusDBEdit                   : NewTitle := 'Center for radius restrictin of DB';
        NLCDBox                        : NewTitle := 'NLCD box';
        NLCDClassification             : NewTitle := 'NLCD classification';
        UTMTrueDeviation               : NewTitle := 'UTM true north declination';
        GeodeticBearing                : NewTitle := 'Geodetic bearing';
        PlotNorthArrow                 : NewTitle := 'North arrow';
        PickDBRecsToMove               : NewTitle := 'Pick line/area to edit';
        PickCenterAndScale             : NewTitle := 'Center new map';
        DigitizeContourPoint           : NewTitle := 'Digitize single point';
        DigitizeContourStream          : NewTitle := 'Stream points on contour line';
        FindBlockHorizon               : NewTitle := 'Point, horizon blocking';
        PlottingPointElevations        : NewTitle := 'Point elevations';
        PickTrainingBox                : NewTitle := 'Training box';
        PickTrainingPoints             : NewTitle := 'Training points';
        JustWandering                  : NewTitle := '';
        IDDBforAction,
        IDDataBaseOne,IDDataBaseAll    : NewTitle := 'Data base record to ID';
        DeleteSingleDBRecs             : NewTitle := 'Delete single DB recs';
        FirstZigDistance,
        FirstDistancePoint             : NewTitle := FPF + 'distance';
        LaterZigDistance,
        SecondDistancePoint            : NewTitle := SPF + 'distance';
        FirstBearingPoint              : NewTitle := FPF + 'bearing';
        SeekingSecondPerspective       : NewTitle := 'Depth for perspective view';
        SecondBearingPoint             : NewTitle := SPF + 'bearing';
        FirstSlopePoint                : NewTitle := FPF + 'average slope';
        SecondSlopePoint               : NewTitle := SPF + 'average slope';
        SeekingLOS                     : NewTitle := 'Left side profile (observer)';
        SeekingAverageProfile,
        MultipleLOS,SeekingTopoProfile          : NewTitle := 'Profile left side';
        SeekingSecondLOS,
        SeekingSecondAverageProfile,
        MultipleTopoProfileRight,SimpleTopoProfileRight : NewTitle := 'Profile right side';
        SeekingFlyThroughRoute         : NewTitle := 'Route for fly through';
        ShowMagneticVariation          : NewTitle := PointFor + 'magnetic variation';
        TerrainBlowup                  : NewTitle := PointFor + 'Terrain Blowup';
        GetPointSymbols                : NewTitle := PointsFor + 'symbol overlay';
        RegionDNs                      : NewTitle := NWCornerStr + ' average reflectance';
        OpenMrSid                      : NewTitle := NWCornerStr + 'MrSid blowup';
        NewCoverage,
        CornerEditBox,
        GraphFilterDB                        : NewTitle := NWCornerStr;
        NewTrack                       : NewTitle := 'Track';
        StreamDistance                 : NewTitle := 'Route for distance';
        GetRGBValues                   : NewTitle := PointFor + 'RGB values';
        GetGreatCircleRoute            : NewTitle := PointFor + 'great circle route';
        SeekingPerspective             : NewTitle := LocationFor + 'Viewer perspective';
        PlottingOffset                 : NewTitle := PointFor + 'offset';
        MoveMapBox                     : NewTitle := 'Move map box';
        ErasingPoints                  : NewTitle := 'Erasing points';
        SeekingFirstNewPanorama        : NewTitle := PointFor + 'panorama';
        SeekingSecondNewPanorama       : NewTitle := 'Depth panorama view';
        QuickWeaponsFan                : NewTitle := 'Weapons location';
        EditWeaponsFans                : NewTitle := 'Weapons fan to edit';
        RangeCircles                   : NewTitle := 'Range circles';
        GrainByRegionSize              : NewTitle := PointFor + 'grain by region size';
        GetPointFabric                 : NewTitle := PointFor + 'grain statistics';
        //DoingDatumShift                : NewTitle := 'Datum shift';
        ShapeTrack,
        ShapePoint                     : NewTitle := 'Digitize point' + ForShapefile;
        ShapeLine,ShapeFirstLine       : NewTitle := 'Digitize line' + ForShapefile;
        ShapePolygon,ShapeFirstPolygon : NewTitle := 'Digitize polygon' + ForShapefile;
        ShapeXYZPoint                  : NewTitle := 'Digitize xyz' + ForShapefile;
        EditFlightPathOnMap            : NewTitle := 'Route segment to shift';
        OpenMapsFromLibrary            : NewTitle := 'Select area of operations';
        SeekingFirstCircleFly          : NewTitle := 'First viewer location';
        SunriseClicking                : NewTitle := 'Points for sunrise/sunset';
        SeekingSecondCircleFly         : NewTitle := 'Depth of view';
        SeekingThirdCircleFly          : NewTitle := 'Viewpoint center';
        SeekingStreamProfile           : NewTitle := 'Points for stream profile';
        RouteObservation               : NewTitle := 'Route';
        LiveFly                        : NewTitle := 'Start of live flight';
        LiveFly2                       : NewTitle := 'End first view';
        CalculateArea                  : NewTitle := OLAF + 'area calculation';
        CalculateVolume                : NewTitle := OLAF + 'volume calculation';
        SubsetByOutline                : NewTitle := OLAF + 'subset';
        OutlineDBIrregularMask         : NewTitle := OLAF + 'DB filter region';
        SubsetHole                     : NewTitle := OLAF + 'hole';
        SubsetLake                     : NewTitle := OLAF + 'lake';
        ReplaceValuesByOutline         : NewTitle := OLAF + 'replace values with reference DEM';
        FillHolesByOutline             : NewTitle := OLAF + 'fill holes with reference DEM';
        DeleteMultipleRecsAllDBs,
        DeleteMultipleDBRecs           : NewTitle := OLAF + 'delete records';
        MapTissotIndicatrix            : NewTitle := 'Point for Tissot indicatrix';
        LabelIDDataBase                : NewTitle := 'DB record to label';
        //USCounty                       : NewTitle := 'US county';
        EditPointElevs                 : NewTitle := 'Edit point elevations';
        FloodFill                      : NewTitle := 'Pt to flood fill';
        DragEdit                       : NewTitle := LPF;
        SeekingPerpendicularProfiles   : NewTitle := 'Point for perpendicular profiles';
        GetIslandArea                  : NewTitle := 'Point on island/peak';
        ShapePointsAlongFirstLine      : NewTitle := 'Start of survey line';
        ShapePointsAlongSecondLine     : NewTitle := 'End of survey line';
        FirstTimeSeries                : NewTitle := 'First point of time series';
        SecondTimeSeries               : NewTitle := 'Second point of time series';
        SpectralReflectance            : NewTitle := 'Point for spectral reflectance';
        Scribble                       : NewTitle := 'Scribble';

        {$IfDef ExFresnel}
        {$Else}
           FirstFresnelPoint              : NewTitle := 'First Fresnel point';
           SecondFresnelPoint             : NewTitle := 'Second Fresnel point';
        {$EndIf}
        {$IfDef ExSlicer3D}
        {$Else}
           PickSliceLocation              : NewTitle := 'Slice location';
           PickSlicePanorama              : NewTitle := 'Slice panorama';
           PickPointCloudStats            : NewTitle := 'Point cloud stats';
        {$EndIf}

        {$IfDef ExRedistrict}{$Else}
           RecolorRedistrictBox        : NewTitle := NWCornerStr + 'recs to redistrict';
           RecolorRedistrict           : NewTitle := 'Single record to redistrict';
       {$EndIf}

        {$IfDef ExGeostats}
        {$Else}
           LagSizeSensitivity             : NewTitle := 'Lag region sensitivity';
           GeomorphPointGraph             : NewTitle := 'Geomorph pt graph';
        {$EndIf}

        {$IfDef ExDrainage}
        {$Else}
           FloodBasin                     : NewTitle := 'Start flood basin from...';
           DrainageArea                   : NewTitle := PointFor + 'contributing basin area';
        {$EndIf}

        {$IfDef ExPLSS}
        {$Else}
           PLSSposition                   : NewTitle := 'PLSS location';
        {$EndIf}

        {$IfDef ExMilicons}
        {$Else}
           AddMilIcon                     : NewTitle := 'Place military icon on map';
           EditMilIcons                   : NewTitle := 'Military icon to edit';
        {$EndIf}

        {$IfDef ExGeology}
        {$Else}
           GetSpreadingRate               : NewTitle := PointFor + 'spreading rate';
           GetDriftVectors                : NewTitle := 'Pick drift speed';
           EarthquakeFocalMech            : NewTitle := 'Earthquake focal mechanism';
           SeekingLeftSideMagModels       : NewTitle := LS + 'magnetic models';
           SeekingRightSideMagModels      : NewTitle := RS + 'magnetic models';
           PickEulerPole                  : NewTitle := 'Euler pole';
           SeekingFirstThreePoint         : NewTitle := FPF + 'plane';
           SeekingSecondThreePoint        : NewTitle := SPF + 'plane';
           SeekingThirdThreePoint         : NewTitle := 'Final point on plane';
           SeekingPlaneContact            : NewTitle := 'Point on plane';
           GetGeologySymbols              : NewTitle := 'Geology symbols';
           SeekingThickness               : NewTitle := 'Bed thickness';
           TraceContact                   : NewTitle := 'Trace contact (key points)';
           ProjectFocalMechToSurface      : NewTitle := 'Focal mechanism to project';
           GetStratcolColumn              : NewTitle := 'Stratigraphic column';
        {$EndIf}
   end;
   if (NewTitle <> SaveCaption) then SaveCaption := '';

   if (DEMNowDoing in [SubsetByOutline]) and (StreamProfileResults <> Nil) then FreeAndNil(StreamProfileResults);

   if (WmDEM <> Nil) and (not LockStatusBar) then begin
      wmDEM.SetPanelText(0,NewTitle);
      WmDEM.SetPanelText(3, '');
   end;
   if  not (DEMNowDoing in [GeodeticBearing]) then gbLatStart := -999;

   DEMDoingNext := WhatNext;
   ApplicationProcessMessages;

   {$IfDef RecordChangeDEMNowDoing} if NewTitle <> '' then WriteLineToDebugFile('Change to ' + NewTitle); {$EndIf}
end;


procedure TMapForm.VectorOverlaySpeedButton21Click(Sender: TObject);
begin
   VectorOutlines1Click(Sender);
end;

procedure TMapForm.Vegetationdensitygraph1Click(Sender: TObject);
begin
   {$IfDef ExVegDensity}
   {$Else}
    if (Veggraph = Nil) then begin
       VegGraph := TThisBaseGraph.Create(Application);
       VegGraph.Caption := 'Vegetation density';
       VegGraph.GraphDraw.VertLabel := 'Height above ground (m)';
       VegGraph.GraphDraw.HorizLabel := 'Vegetation Density';
       VegGraph.GraphDraw.MaxVertAxis := DEMGlb[MapDraw.DEMonMap].VegDensityLayers[1].LayersPresent;
       VegGraph.GraphDraw.MaxHorizAxis := MDDef.VegDensityGraphMaxDensity;
       VegGraph.CanCloseGraph := false;
       VegGraph.RedrawDiagram11Click(Nil);
       Vegetationdensitygraph1.Checked := true;
    end
    else begin
       DEMNowDoing := Calculating;
       ApplicationProcessMessages;
       Vegetationdensitygraph1.Checked := false;
       VegGraph.CanCloseGraph := true;
       VegGraph.Close;
       VegGraph := Nil;
       ChangeDEMNowDoing(JustWandering);
    end;
    {$EndIf}
end;


procedure TMapForm.Vegetationmap1Click(Sender: TObject);
begin
   {$IfDef ExVegDensity}
   {$Else}
      if MapDraw.ValidDEMonMap and (DEMGlb[MapDraw.DEMonMap].VegGrid[1] <> 0) then CloseSingleDEM(DEMGlb[MapDraw.DEMonMap].VegGrid[1]);
   {$EndIf}
end;


procedure TMapForm.Undefined1Click(Sender: TObject);
begin
   ChangeElevUnits(Undefined);
end;

procedure TMapForm.UndoSpeedButtonClick(Sender: TObject);
begin
   if (SavedMapImage <> Nil) then begin
      Image1.Picture.Graphic := SavedMapImage;  //UndoBMP;
   end;
end;

procedure TMapForm.UnsubsetSpeedButton22Click(Sender: TObject);
begin
   with MapDraw do begin
      MapDraw.ZeroTickInt;
      MapDraw.DeleteMapSavedLayers;
      if (VectorIndex <> 0) then begin
         //PrimMapProj.ReadProjection(MDTempDir + 'last vector proj.prj');
         //DrawColoredMap1Click(Nil);
      end
      else begin
         //MapCorners := OldMapCorners;
         if MapDraw.DEMMap then begin
            SizeIsCorrectThankYou := false;
            CheckAndDrawNewCorners;
         end;
         DrawColoredMap1Click(Nil);
      end;
      UnsubsetSpeedButton22.Enabled := false;
   end;
end;


procedure TMapForm.Unsupervisedclassification1Click(Sender: TObject);
//var
  // i : integer;
begin
   {$IfDef NoClustering}
   {$Else}
      {$IfDef RecordSat} WriteLineToDebugFile('TMapForm.Unsupervisedclassification1Click in'); {$EndIf}
      if (MapDraw.MultiGridOnMap = 0) then All2Click(Sender);
      UnsupervisedClassification(MapDraw.MultiGridOnMap, Self);
      {$IfDef RecordSat} WriteLineToDebugFile('TMapForm.Unsupervisedclassification1Click out'); {$EndIf}
   {$EndIf}
end;


procedure TMapForm.Updategrouping1Click(Sender: TObject);
begin
   UpdateShapeFileGroup;
end;



procedure TMapForm.Mergeallimages1Click(Sender: TObject);
begin
   ExportMapToGoogleEarth(True);
end;

procedure TMapForm.MergeanotherDEMhillshade1Click(Sender: TObject);
begin
   MergeAnotherDEMreflectance(0,true);
end;

procedure TMapForm.Mergecolors1Click(Sender: TObject);
begin
   {$IfDef ExSat}
   {$Else}
      if GetIHSparameters(MDdef.MergeInt,MDdef.MergeHue,MDdef.MergeSat) then DrawColoredMap1Click(Nil);
   {$EndIf}
end;


procedure TMapForm.Savemapwithworldfile(fName : PathStr; BaseMapOnly : boolean = false);
begin
    if (fName = '') then begin
       fName := WriteSatDir;
       if not GetFileNameDefaultExt('save map image',PetImage.GraphicsFilters,fName) then exit;
    end;

    {$IfDef RecordMapDraw} WriteLineToDebugFile('Saving BMP with world file to ' + fName+ '  Map size: ' + ImageSize(Image1)); {$EndIf}
    if BaseMapOnly then Image1.Picture.LoadFromFile(MapDraw.BaseMapfName);
    SaveImageAsBMP(Image1,fName);
    MapDraw.WriteMapsWorldFile(fName);
end;

procedure TMapForm.Savemapwithworldfile1Click(Sender: TObject);
begin
   SaveMapWithWorldFile('');
end;


procedure TMapForm.TIGERoptions1Click(Sender: TObject);
begin
   ModifyTIGERdisplay1Click(Sender);
end;


procedure TMapForm.Loadprojection2Click(Sender: TObject);
begin
   {$IfDef RecordOpenVectorMap} WriteLineToDebugFile('TMapForm.Loadprojection2Click in'); {$EndIf}
   if (Sender = Nil) or GetFileFromDirectory('Map Projection','*.PRJ;*.WKT',VectorMapName) then begin
      {$IfDef RecordOpenVectorMap} WriteLineToDebugFile(VectorMapName); {$EndIf}
      MapDraw.PrimMapProj.ProjectionfName := VectorMapName;
      {$IfDef RecordOpenVectorMap} WriteLineToDebugFile('WKT projection'); {$EndIf}
      MapDraw.PrimMapProj.InitializeProjectionFromWKT(VectorMapName);
      MapDraw.BaseTitle := MapDraw.PrimMapProj.GetProjectionName;
      {$IfDef RecordOpenVectorMap} WriteLineToDebugFile('TMapForm.Loadprojection2Click, geo:  ' + sfBoundBoxToString(MapDraw.MapCorners.BoundBoxGeo)); {$EndIf}
      MapDraw.BoundingBoxGeoToProjected;
      MapDraw.MapCorners.BoundBoxGeo := MapDraw.GetBoundBoxGeo;
      {$IfDef RecordOpenVectorMap} WriteLineToDebugFile('TMapForm.Loadprojection2Click, want projected:  ' + sfBoundBoxToString(MapDraw.MapCorners.BoundBoxProj)); {$EndIf}
      DoCompleteMapRedraw;
   end;
end;


procedure TMapForm.Saveproject1Click(Sender: TObject);
begin
   dem_manager.SaveMicrodemDesktop;
end;



procedure TMapForm.Saverangecircles1Click(Sender: TObject);
var
   fName : PathStr;
begin
   fName := ExtractFilePath(LastDataBase);
   if Petmar.GetFileNameDefaultExt('shape file for range circles','Shape file|*.shp',fName) then CopyShapeFile(MapDraw.RangeCirclesFName,fName);
end;

procedure TMapForm.Modefilter1Click(Sender: TObject);
begin
   ModeFilterPopupMenu.PopUp(Mouse.CursorPos.X,Mouse.CursorPos.Y);
end;

procedure TMapForm.Modifymaparea1Click(Sender: TObject);
begin
   PopUpMenu11.Popup(Mouse.CursorPos.X,Mouse.CursorPos.Y);
end;


procedure TMapForm.ModifyTIGERdisplay1Click(Sender: TObject);
begin
   {$IfDef RecordTIGER} WriteLineToDebugFile('TMapForm.ModifyTIGERdisplay1Click in'); {$EndIf}
   try
      DEMTigerOps.SetTigerOptions(Self,MapDraw.ScreenPixelSize);
   except
      on Exception do ;
   end;
end;


procedure TMapForm.RedrawTiger;
begin
   {$IfDef RecordTIGER} WriteLineToDebugFile('TMapForm.RedrawTiger'); {$EndIf}
   if (MapDraw.TigerOverlayFName = '') then begin
      {$IfDef RecordTIGER} WriteLineToDebugFile('TMapForm.ModifyTIGERdisplay1Click call MapDraw.DeleteDatabaseSavedLayers'); {$EndIf}
      MapDraw.DeleteDatabaseSavedLayers;
   end
   else begin
      {$IfDef RecordTIGER} WriteLineToDebugFile('TMapForm.ModifyTIGERdisplay1Click call delete ' + MapDraw.TigerOverlayFName); {$EndIf}
      MapDraw.DeleteSingleMapLayer(MapDraw.TigerOverlayFName);
   end;
   DoFastMapRedraw;
   {$IfDef RecordTIGER} WriteLineToDebugFile('TMapForm.ModifyTIGERdisplay1Click out'); {$EndIf}
end;


procedure TMapForm.Reflectanceoptions1Click(Sender: TObject);
begin
   Reflectance2Click(Sender);
end;


procedure TMapForm.Slopecategories2Click(Sender: TObject);
begin
   SlopeCategories1Click(Sender);
end;


procedure TMapForm.Slopedegreestopercent1Click(Sender: TObject);
begin
    Multiplyzvalues1Click(Sender);
end;


procedure TMapForm.Slopeerrorestimateexperimental1Click(Sender: TObject);
begin
   if ValidDEM(MapDraw.DEMonMap) then DEMGlb[MapDraw.DEMonMap].RichardsonExtrapolationSlopeMaps;
end;


procedure TMapForm.Slopeerrorestimatorexperimental1Click(Sender: TObject);
begin
   Slopeerrorestimateexperimental1Click(Sender);
end;

procedure TMapForm.RightClickContourIntervalClick(Sender: TObject);
begin
  Contourinterval1Click(Sender);
end;


procedure TMapForm.RIK1Click(Sender: TObject);
begin
   MakeTRIGrid(MapDraw.DEMonMap,nmTRIK,true);
end;

function TMapForm.GetSecondDEM(MustBeCompatible : boolean = true) : boolean;
begin
   GetDEM(MapDraw.DEM2onMap);
   if (not MustBeCompatible) or DEMGlb[MapDraw.DEMonMap].SecondGridIdentical(MapDraw.DEM2onMap) then begin
      Result := true;
      MapDraw.AssignSecondDEM(MapDraw.DEM2onMap);
   end
   else begin
       MessageToContinue('Incompatible grids; currently not working');
       MapDraw.DEM2onMap := 0;
       Result := false;
   end;
end;


procedure TMapForm.Googlemaps1Click(Sender: TObject);
begin
   {$IfDef ExKML}
   {$Else}
      SimpleLoadGoogleMaps(RightClickLat,RightClickLong);
   {$EndIf}
end;


procedure TMapForm.Contoursfromsecondgrid1Click(Sender: TObject);
begin
   if GetSecondDEM then begin
      AddOrSubtractOverlay(Self,ovoContoursDEM2,true);
      Contour2Click(Sender);
   end;
end;



procedure TMapForm.Elevationcolors1Click(Sender: TObject);
begin
   ChangeElevationMap(MDdef.MergeInt,MDdef.MergeHue,MDdef.MergeSat,Self);
   if MapDraw.NeedToRedraw then begin
      if (MapDraw.MapOwner = moPointVerificationMap) then MDdef.ZoomWindowMapType := MapDraw.MapType;
      MapDraw.DeleteSingleMapLayer(MapDraw.BaseMapFName);
      MapDraw.DeleteSingleMapLayer(MapDraw.LegendOverlayfName);
      DrawColoredMap1Click(Nil);
   end;
end;


procedure TMapForm.Elevationdifference1Click(Sender: TObject);
begin
   ChangeElevUnits(euElevDiff);
end;

procedure TMapForm.Elevation2Click(Sender: TObject);
begin
   Elevationcolors1Click(Sender);
end;


procedure TMapForm.Gridaveragestddev1Click(Sender: TObject);
label
   NoDataSkip;
var
   NewHeadRecs : tDEMheader;
   MomentVar : tMomentVar;
   LargestAssignedDEM,i,x,y,MeanDEM,NPtsDEM,STDDEM,MedDEM,FloorDEM,CeilingDEM,EnvDEM,MaxDEM,MinDEM  : integer;
   Lat,Long : float64;
   z : float32;
   Data : array[1..MaxDEMDataSets] of float32;


      procedure DoDEM(DEM : integer; TheName : shortString);
      begin
         if (DEM <> 0) then  begin
            DEMGlb[DEM].AreaName := TheName;
            DEMGlb[DEM].DEMheader.ElevUnits := Undefined;
            DEMGlb[DEM].SetUpMap(DEM,true,mtElevSpectrum);
         end;
      end;

      function ComputingDEM(i : integer) : boolean;
      begin
         Result := ValidDEM(i) and (i <> MeanDEM) and (i <> NptsDEM) and (i <> STDDEM) and (I <> MedDEM) and (i <> FloorDEM) and (i <> CeilingDEM) and (i <> EnvDEM) and (I <> MaxDEM) and (I <> MinDEM);
      end;


begin
   try
      ChangeOptions(17);
      NewHeadRecs := DEMGlb[MapDraw.DEMonMap].DEMheader;
      MeanDEM := 0;
      NPtsDEM := 0;
      STDDEM := 0;
      MedDEM := 0;
      FloorDEM := 0;
      CeilingDEM := 0;
      EnvDEM := 0;
      MaxDEM := 0;
      MinDEM  := 0;

      with DEMGlb[MapDraw.DEMonMap] do begin
         if MDDef.doMeanDEM then OpenAndZeroNewDEM(true,NewHeadRecs,MeanDEM,'',InitDEMmissing);
         if MDDef.doEnvDEM then OpenAndZeroNewDEM(true,NewHeadRecs,EnvDEM,'',InitDEMmissing);
         if MDDef.doSTDDEM then OpenAndZeroNewDEM(true,NewHeadRecs,STDDEM,'',InitDEMmissing);
         if MDDef.doMedDEM then OpenAndZeroNewDEM(true,NewHeadRecs,MedDEM,'',InitDEMmissing);
         if MDDef.doFloorDEM then OpenAndZeroNewDEM(true,NewHeadRecs,FloorDEM,'',InitDEMmissing);
         if MDDef.doCeilingDEM then OpenAndZeroNewDEM(true,NewHeadRecs,CeilingDEM,'',InitDEMmissing);

         NewHeadRecs.DEMPrecision := byteDEM;
         NewHeadRecs.ElevUnits := euIntCode;
         if MDDef.doDEMwithMax then OpenAndZeroNewDEM(true,NewHeadRecs,MaxDEM,'',InitDEMmissing);
         if MDDef.doDEMwithMin then OpenAndZeroNewDEM(true,NewHeadRecs,MinDEM,'',InitDEMmissing);
         if MDDef.doNPtsDEM then OpenAndZeroNewDEM(true,NewHeadRecs,NPTsDEM,'',InitDEMmissing);

         for I := 1 to MaxDEMDataSets do begin
            if ValidDEM(i) then LargestAssignedDEM := i;
         end;

         StartProgress('Multigrid stats');
         for x := 0 to pred(DEMheader.NumCol) do begin
            if (x Mod 100 = 0) then UpdateProgressBar(x/DEMheader.NumCol);

            for y := 0 to pred(DEMheader.NumRow) do begin
               DEMGlb[MapDraw.DEMonMap].DEMGridToLatLongDegree(x,y,Lat,Long);
                MomentVar.NPts := 0;
               for I := 1 to LargestAssignedDEM do begin
                  if ComputingDEM(i) then begin
                     if DEMGlb[i].GetElevFromLatLongDegree(Lat,Long,z) then begin
                        inc(MomentVar.NPts);
                        Data[MomentVar.NPts] := z;
                     end
                     else begin
                        if MDDef.AnyNoDataMeansNoData then goto NoDataSkip;
                     end;
                  end;
               end;
               if (MomentVar.Npts > 0) then begin
                  PetMath.moment(data,MomentVar,msAll);
                  if MDDef.doMeanDEM then DEMGlb[MeanDEM].SetGridElevation(x,y,MomentVar.mean);
                  if MDDef.doNPtsDEM then DEMGlb[NPtsDEM].SetGridElevation(x,y,MomentVar.Npts);
                  if MDDef.doSTDDEM and (MomentVar.NPts > 1) then DEMGlb[STDDEM].SetGridElevation(x,y,MomentVar.std_dev);
                  if MDDef.doMedDEM then DEMGlb[MedDEM].SetGridElevation(x,y,MomentVar.Median);
                  if MDDef.doFloorDEM then DEMGlb[FloorDEM].SetGridElevation(x,y,MomentVar.MinZ);
                  if MDDef.doCeilingDEM then DEMGlb[CeilingDEM].SetGridElevation(x,y,MomentVar.MaxZ);
                  if MDDef.doEnvDEM then DEMGlb[EnvDEM].SetGridElevation(x,y,MomentVar.MaxZ-MomentVar.MinZ);

                  if (MDDef.doDEMwithMax or MDDef.doDEMwithMin) and (MomentVar.Npts > 1) then begin
                     HeapSort(MomentVar.NPts,Data);
                     if MDDef.doDEMwithMax then begin
                        if Data[MomentVar.NPts] > Data[pred(MomentVar.NPts)] + MDDef.MinMaxGridTolerance then begin
                           for I := 1 to MaxDEMDataSets do begin
                              if ComputingDEM(i) then begin
                                 if DEMGlb[i].GetElevMeters(x,y,z) then begin
                                    if abs(z - Data[MomentVar.Npts]) < 0.001 then begin
                                        DEMGlb[MaxDEM].SetGridElevation(x,y,i);
                                    end;
                                 end;
                              end;
                           end;
                        end;
                     end;

                     if MDDef.doDEMwithMin then begin
                        if Data[1] < Data[2] - MDDef.MinMaxGridTolerance then begin
                           for I := 1 to MaxDEMDataSets do begin
                              if ComputingDEM(i) then begin
                                 if DEMGlb[i].GetElevMeters(x,y,z) then begin
                                    if abs(z - Data[1]) < 0.001 then begin
                                        DEMGlb[MinDEM].SetGridElevation(x,y,i);
                                    end;
                                 end;
                              end;
                           end;
                        end;
                     end;
                   end;
                end;
                NoDataSkip:;
            end {for y};
         end {for x};
         DoDEM(MeanDEM,'Mean elevation');
         DoDEM(NPtsDEM, 'Points averaged');
         DoDEM(STDDEM, 'Elevation std dev');
         DoDEM(MedDEM, 'Median elevation');
         DoDEM(FloorDEM,'Floor elevation');
         DoDEM(CeilingDEM, 'Ceiling elevation');
         DoDEM(envDEM, 'Envelope thickness');
         DoDEM(MaxDEM, 'Highest DEM');
         DoDEM(MinDEM, 'Lowest DEM');
      end;
   finally
    EndProgress;
   end;
end;

procedure TMapForm.Displayparameter1Click(Sender: tObject);
begin
   Display1Click(Sender);
end;


procedure TMapForm.Loadsecondvegeationlayers1Click(Sender: TObject);
begin
   Loadvegetationlayer(2,Loadsecondvegeationlayers1);
end;


procedure TMapForm.Loadsidescanimagery1Click(Sender: TObject);
{$IfDef ExSidescan}
begin
{$Else}
var
   Infiles : tstringList;
   i,DefaultFilter : byte;
begin
    Infiles := tstringList.Create;
    Infiles.Add(InputSideScanFileName);
    DefaultFilter := 1;
    Petmar.GetMultipleFiles('sidescan data','XTF files|*.xtf',Infiles,DefaultFilter);
    for i := 0 to pred(Infiles.Count) do begin
        LoadSideScanFile(Self,InFiles.Strings[i]);
    end;
    Infiles.Free;
{$EndIf}
end;

procedure TMapForm.Loadsummarymultipleclassifications1Click(Sender: TObject);
var
   fName : PathStr;
begin
   if EnsembleClassDB = 0 then begin
     fName := 'C:\mapdata\0--current_projects\00-IEEE_GRSS_IADFTC_Contest2014_full_data_set\clustering_results.dbf';
     EnsembleClassDB := LoadDataBaseFile(fName);
     //LastDBLoaded;
     LoadProbabilityMatrix;
   end;
end;


procedure TMapForm.TIGERVectorlegend1Click(Sender: TObject);
{$IfDef ExVectorOverLay}
begin
{$Else}
var
   Bitmap : tMyBitmap;


         procedure DrawOne(y,LineWidth : integer; LineColor : tPlatformColor; Name : shortstring);
         begin
            with Bitmap.Canvas do begin
               Pen.Width := LineWidth;
               Pen.Color := ConvertPlatformColorToTColor(LineColor);
               MoveTo(10,y);
               LineTo(50,y);
               TextOut(60,y-10,Name);
            end;
         end;

begin
   CreateBitmap(Bitmap,300,295);
   Bitmap.Canvas.Font.Name := 'Times New Roman';
   Bitmap.Canvas.Font.Size := 16;
   with MDdef.TigrDef do begin
      DrawOne(10,MajorRoadWidth,MajorRoadColor,'Interstate highway');
      DrawOne(35,RoadCat2Width,RoadCat2Color,'Primary road');
      DrawOne(60,RoadCat3Width,RoadCat3Color,'Secondary road');
      DrawOne(85,RoadCat4Width,RoadCat4Color,'Local road');
      DrawOne(110,RoadCat5Width,RoadCat5Color,'Vehicular trail');
      DrawOne(135,RoadCat6Width,RoadCat6Color,'Special road');
      DrawOne(160,RoadCat7Width,RoadCat7Color,'Driveway');
      DrawOne(185,WaterWidth1,WaterColor1,'Permanent stream');
      DrawOne(210,RailroadWidth,RailroadColor,'Railroad');
      DrawOne(235,PowerLineWidth,PowerlineColor,'Power Line');
      DrawOne(260,BoundaryWidth,BoundaryColor,'Boundary');
   end;
   DisplayBitmap(Bitmap);
   Bitmap.Free;
{$EndIf}
end;


procedure TMapForm.Manageoverlays2Click(Sender: TObject);
begin
   Overlays1Click(Sender);
end;

procedure TMapForm.CutOutCenterOfMap(var BaseBMP : tMyBitmap; x,y : integer);
var
   xp,yp : integer;
begin
    CreateBitmap(BaseBMP,x,y);
    xp := MapDraw.MapXSize div 2;
    yp := MapDraw.MapYSize div 2;
    x := x div 2;
    y := y div 2;
    BaseBMP.Canvas.CopyRect(Rect(0,0,2*x,2*y),Image1.Canvas,Rect(xp-x,yp-y,xp+x,yp+y));
end;


procedure TMapForm.Replayflightroute1Click(Sender: TObject);
begin
   {$IfDef Ex3d}
   {$Else}
      StartTheFlyThrough(DEMNowDoing);
   {$EndIf}
end;


procedure TMapForm.Removeifmissingneighbors1Click(Sender: TObject);
begin
   ThinDEM1Click(Sender);
end;

procedure TMapForm.Removemerge1Click(Sender: TObject);
begin
   MapDraw.MapMerge := mmNone;
   DrawColoredMap1Click(Nil)
end;


procedure TMapForm.Removemissingpointsinmask1Click(Sender: TObject);
begin
   SetMaskGrid1Click(Sender);
end;

procedure TMapForm.Removequickoverlayhillshade1Click(Sender: TObject);
begin
   SavedMergeReflectanceDEM := 0;
   DoFastMapRedraw;
end;

procedure TMapForm.Removetoofewsimilarneighbors1Click(Sender: TObject);
begin
   ThinDEM1Click(Sender);
end;

procedure TMapForm.ArcGIScodes11281Click(Sender: TObject);
begin
   MapDraw.MapType := mtFlowDirArc;
   DrawColoredMap1Click(Nil);
end;

procedure TMapForm.Arctangent1Click(Sender: TObject);
begin
   Multiplyzvalues1Click(Sender);
end;

procedure TMapForm.Area1Click(Sender: TObject);
begin
   Lineshapefile1Click(Sender);
end;

procedure TMapForm.Areaofsinglecolor1Click(Sender: TObject);
begin
   EditGridViaColor(emvcAreaofsinglecolor,clBlack);
end;

procedure TMapForm.Meters1Click(Sender: TObject);
begin
   Feet1Click(Meters1);
end;

(*
procedure TMapForm.MGRSUSNG6x8zones1Click(Sender: TObject);
var
   fName : PathStr;
begin
   DownloadandUnzipDataFileIfNotPresent('mgrs6x8_100k');
   fName := MainMapData + 'mgrs6x8_100k\mgrs6x8_east.shp';
   LoadDataBaseFile(fName);
   fName := MainMapData + 'mgrs6x8_100k\mgrs6x8_west.shp';
   LoadDataBaseFile(fName);
end;
*)

procedure TMapForm.MICRODEMGeotiffinfo1Click(Sender: TObject);
begin
    if MapDraw.DEMMap then GeotiffMetadata(MDMicrodem,DEMglb[MapDraw.DEMonMap].DEMfileName)
    else GeotiffMetadata(mdMicrodem,SatImage[MapDraw.SatOnMap].IndexFileName);
end;

procedure TMapForm.MICRODEMupsamplebilinearbicubic1Click(Sender: TObject);
var
   Spacing : float32;
   NewDEM1,NewDEM2,NewDEM3 : integer;
begin
   //Spacing := 0.1 * DEMGlb[MapDraw.DEMonMap].DEMheader.DEMxSpacing;
   if (DEMGlb[MapDraw.DEMonMap].DEMheader.DEMUsed = ArcSecDEM)  then begin
      //Spacing := 3600 * 0.1 * DEMGlb[MapDraw.DEMonMap].DEMheader.DEMxSpacing;  //since it has to be in arc seconds for the reinterpolation routine
      Spacing := -99;
      MDDef.wf.ElevInterpolation := piBicubicVT;
      NewDEM1 := DEMGlb[MapDraw.DEMonMap].ReinterpolateLatLongDEM(Spacing,MDTempDir + DEMGlb[MapDraw.DEMonMap].AreaName + '_bicubicVT.dem');
      MDDef.wf.ElevInterpolation := piBicubicVT;
      NewDEM3 := DEMGlb[MapDraw.DEMonMap].ReinterpolateLatLongDEM(Spacing,MDTempDir + DEMGlb[MapDraw.DEMonMap].AreaName + '_bicubicNR.dem');
      MDDef.wf.ElevInterpolation := piBilinear;
      NewDEM2 := DEMGlb[MapDraw.DEMonMap].ReinterpolateLatLongDEM(Spacing,MDTempDir + DEMGlb[MapDraw.DEMonMap].AreaName + '_bilinear.dem');

      DEMGlb[NewDEM1].SetUpMap(NewDEM1,true,MapDraw.MapType);
      DEMGlb[NewDEM2].SetUpMap(NewDEM2,true,MapDraw.MapType);
      DEMGlb[NewDEM3].SetUpMap(NewDEM3,true,MapDraw.MapType);
   end;
end;

procedure TMapForm.Database1Click(Sender: TObject);
begin
   DataBaseSpeedButton28Click(Sender);
end;


procedure TMapForm.GeomorphAtlas1Click(Sender: TObject);
begin
   {$IfDef ExGeology}
   {$Else}
      Grid_over_map.GridOverlayPointsOnMap(Self);
   {$EndIf}
end;


procedure TMapForm.Markasmissing1Click(Sender: TObject);
const
   PC = ' (percentiles)';
   zHi : float32 = 0;
   zLo : float32 = 0;
var
   x,y,zi,
   PN,NPts  : integer;
   Original,Fixed : int64;
   znw,zw,zsw,zn,z,zs,zne,ze,zse : float32;
   HiMissing,LowMissing : ShortString;
begin
   {$IfDef RecordEditsDEM} WritelineToDebugFile('TMapForm.Markasmissing1Click in'); {$EndIf}
    LowMissing := ' z to mark missing (' + ptTrim(ElevUnitsAre(DEMGlb[MapDraw.DEMonMap].DEMheader.ElevUnits))  + ')';
    HiMissing := 'Hi' + LowMissing;
    LowMissing := 'Lo' + LowMissing;
    PN := 0;
    if (Sender = Bytedata0tomissing1) or (Sender = INForNAN1) or  (Sender = Likelymissingdatacodes1) then begin
       //Nothing to set
    end
    else if (Sender = Inf1) then begin
       ReadDefault('Replace +Inf with',zn);
    end
    else if (Sender = NAN1) then begin
       ReadDefault('Replace NAN with',zn);
    end
    else if (Sender = Snglepointspires1) then begin
       ReadDefault('Remove spires over this z (relative to neighbors)',zHi);
    end
    else if (Sender = Singlepointpits1) then begin
       ReadDefault('Remove pits under this z  (relative to neighbors)',zHi);
       ReadDefault('Pit neighbors allows',PN);
    end
    else if (Sender = Land1) then begin
       zHi := 32767;
       zLo := 0.1;
    end
    else if (Sender = Singlevalue1) or (Sender = Everythingexceptsinglevalue1) then begin
       zi := 0;
       if (Sender = Singlevalue1) then ReadDefault('Remove this z',zi)
       else ReadDefault('Retain just this z',zi);
       zHi := zi + 0.0001;
       zLo := zi - 0.0001;
    end
    else if (Sender = Water1) then begin
       zLo := -32767;
       zHi := 0.01;
    end
    else if (Sender = Selectedpercentilerange1) or (Sender = Outsideselectedpercentilerange1) then begin
       repeat
          zHi := 99;
          ReadDefault(HiMissing + PC,zHi);
          zLo := 1;
          ReadDefault(LowMissing + PC,zLo);
       until (zHi > zLo);
       if (zHi > 99.99) then zHi := 32767
       else zHi := DEMGLb[MapDraw.DEMonMap].FindPercentileElevation(zHi);
       if (zLo < 0.0001) then zLo := -32767
       else zLo := DEMGLb[MapDraw.DEMonMap].FindPercentileElevation(zLo);
    end
    else if (Sender = EverythingAboveCutoff1) then begin
       ReadDefault('Cutoff (remove this z and above)',zLo);
    end
    else if (Sender = EverythingBelowCutoff1) then begin
       {$IfDef RecordEditsDEM} WritelineToDebugFile('(Sender = EverythingBelowCutoff1)'); {$EndIf}
       ReadDefault('Cutoff (remove this z and below)',zHi);
    end
    else if (Sender = Outsideselectedrange1) then begin
       repeat
          ReadDefault('High z to retain',zHi);
          ReadDefault('Low z to retain',zLo);
       until (zHi >= zLo);
    end
    else begin
       repeat
          ReadDefault(HiMissing,zHi);
          ReadDefault(LowMissing,zLo);
       until (zHi >= zLo);
    end;

    {$IfDef RecordEditsDEM} WritelineToDebugFile('start loops'); {$EndIf}
    ShowHourglassCursor;
    Fixed := 0;
    Original := 0;
    if (Sender = Snglepointspires1) or (Sender = Singlepointpits1) then begin
       for x := 0 to pred(DEMGLb[MapDraw.DEMonMap].DEMheader.NumCol) do begin
          if (x mod 100 = 0) and ShowSatProgress and (DEMGLb[MapDraw.DEMonMap].DEMheader.NumCol > 1500) then StartProgress('Isolated points');
          for y := 0 to pred(DEMGLb[MapDraw.DEMonMap].DEMheader.NumRow) do begin
             if DEMGLB[MapDraw.DEMonMap].SurroundedPointElevs(x,y,znw,zw,zsw,zn,z,zs,zne,ze,zse) then begin
                inc(original);
                if (Sender = Snglepointspires1) then begin
                   if (z > znw + zHi) and (z > zn + zHi) and (z > zne + zHi)  and
                      (z > zw + zHi)                     and (z > ze + zHi) and
                      (z > zsw + zHi) and (z > zs + zHi) and (z > zse + zHi) then begin
                          DEMGLb[MapDraw.DEMonMap].SetGridElevation(x,y, (znw+zw+zsw+zn+zs+zne+ze+zse)/8);
                          inc(Fixed)
                   end;
                end
                else begin
                   NPts := 0;
                   if (z < znw - zHi) then inc(NPts);
                   if (z < zn - zHi)  then inc(NPts);
                   if (z < zne - zHi) then inc(NPts);
                   if (z < zw - zHi)  then inc(NPts);
                   if (z < ze - zHi)  then inc(NPts);
                   if (z < zsw - zHi) then inc(NPts);
                   if (z < zs - zHi)  then inc(NPts);
                   if (z < zse - zHi) then inc(NPts);
                   if NPTs >= 8 - PN then begin
                      DEMGLb[MapDraw.DEMonMap].SetGridElevation(x,y, (znw+zw+zsw+zn+zs+zne+ze+zse)/8);
                      inc(fixed);
                   end;
                end;
             end;
          end;
       end;
    end
    else if (Sender = Outsideselectedpercentilerange1) or (Sender = Outsideselectedrange1) or (Sender = Everythingexceptsinglevalue1) then begin
       for x := 0 to pred(DEMGLb[MapDraw.DEMonMap].DEMheader.NumCol) do begin
          if (x mod 100 = 0) and ShowSatProgress and (DEMGLb[MapDraw.DEMonMap].DEMheader.NumCol > 1500) then StartProgress('Outside percentile ' + DEMGLb[MapDraw.DEMonMap].AreaName);
          for y := 0 to pred(DEMGLb[MapDraw.DEMonMap].DEMheader.NumRow) do begin
             if DEMGLb[MapDraw.DEMonMap].GetElevMeters(x,y,z) then begin
                inc(Original);
                if (z < zLo) or (z > zHi) then begin
                   DEMGLb[MapDraw.DEMonMap].SetGridMissing(x,y);
                   inc(Fixed);
                end;
             end;
          end;
       end;
    end
    else if (Sender = Bytedata0tomissing1) then begin
       for x := 0 to pred(DEMGLb[MapDraw.DEMonMap].DEMheader.NumCol) do begin
          if (x mod 100 = 0) and ShowSatProgress and (DEMGLb[MapDraw.DEMonMap].DEMheader.NumCol > 1500) then StartProgress('Mark missing ' + DEMGLb[MapDraw.DEMonMap].AreaName);
          for y := 0 to pred(DEMGLb[MapDraw.DEMonMap].DEMheader.NumRow) do begin
             if DEMGLb[MapDraw.DEMonMap].GetElevMeters(x,y,z) then begin
                inc(Original);
                if (z < 0.001) then DEMGLb[MapDraw.DEMonMap].SetGridMissing(x,y)
                else DEMGLb[MapDraw.DEMonMap].SetGridElevation(x,y,z-1);
             end
             else DEMGLb[MapDraw.DEMonMap].SetGridElevation(x,y,254);
          end;
       end;
    end
    else if (Sender = Inf1) or (Sender = NAN1) then begin
       for x := 0 to pred(DEMGLb[MapDraw.DEMonMap].DEMheader.NumCol) do begin
          if (x mod 100 = 0) and ShowSatProgress and (DEMGLb[MapDraw.DEMonMap].DEMheader.NumCol > 1500) then
             StartProgress('Reclassify ' + DEMGLb[MapDraw.DEMonMap].AreaName);
          for y := 0 to pred(DEMGLb[MapDraw.DEMonMap].DEMheader.NumRow) do begin
             if DEMGLb[MapDraw.DEMonMap].GetElevMeters(x,y,z) then begin
                inc(Original);
                if ((Sender = Inf1) and IsInfinity(z)) or ((Sender = NAN1) and Math.IsNAN(z))  then begin
                   DEMGLb[MapDraw.DEMonMap].SetGridElevation(x,y,zn);
                   inc(Fixed);
                end;
             end;
          end;
       end;
    end
    else if (Sender = INForNAN1) then begin
       for x := 0 to pred(DEMGLb[MapDraw.DEMonMap].DEMheader.NumCol) do begin
          if (x mod 100 = 0) and ShowSatProgress and (DEMGLb[MapDraw.DEMonMap].DEMheader.NumCol > 1500) then
             StartProgress('NAN or infinity removal ' + DEMGLb[MapDraw.DEMonMap].AreaName);
          for y := 0 to pred(DEMGLb[MapDraw.DEMonMap].DEMheader.NumRow) do begin
             if DEMGLb[MapDraw.DEMonMap].GetElevMeters(x,y,z) then begin
                inc(Original);
                if IsInfinity(z) or Math.IsNAN(z) then begin
                   DEMGLb[MapDraw.DEMonMap].SetGridMissing(x,y);
                   inc(Fixed);
                end;
             end;
          end;
       end;
    end
    else if (Sender = EverythingAboveCutoff1) then begin
       DEMGLb[MapDraw.DEMonMap].MarkAboveMissing(zlo,Fixed,false);
    end
    else if (Sender = EverythingBelowCutoff1) then begin
       DEMGLb[MapDraw.DEMonMap].MarkBelowMissing(zHi,Fixed,false);
    end
    else if (Sender = Likelymissingdatacodes1) then begin
       DEMGLb[MapDraw.DEMonMap].DeleteMissingDataPoints;
    end
    else begin
       DEMGLb[MapDraw.DEMonMap].MarkInRangeMissing(zlo,zhi,Fixed,false);
    end;

    if (Original = 0) then begin
       Original := 1;
       Original := Original * DEMGLb[MapDraw.DEMonMap].DEMheader.NumCol * DEMGLb[MapDraw.DEMonMap].DEMheader.NumRow;
    end;

    RespondToChangedDEM;

    EndProgress;
    if (Sender = Snglepointspires1) then MessageToContinue('Removed spires: ' + IntToStr(Fixed));
    if (Sender = Singlepointpits1) then MessageToContinue('Removed pits: ' + IntToStr(Fixed));
    if (Sender = Inf1) then MessageToContinue('Removed +Inf: ' + IntToStr(Fixed));
    if (Sender = NAN1) then MessageToContinue('Removed NAN: ' + IntToStr(Fixed));
    if (Sender = Outsideselectedpercentilerange1) or (Sender = Outsideselectedrange1)  or (Sender = Everythingexceptsinglevalue1) or (Sender = EverythingAboveCutoff1) or (Sender = EverythingBelowCutoff1) then begin
       MessageToContinue('Removed: ' + IntToStr(Fixed) + ' (' + RealToString(100.0 * Fixed / Original,-12,-3) + '% of points)');
    end;

    {$IfDef RecordElevationScaling} WriteLineToDebugFile('TMapForm.Markasmissing1Click,  ' + MapDraw.MapZRangeString); {$EndIf}
end;


procedure TMapForm.Land1Click(Sender: TObject);
begin
   Markasmissing1Click(Sender);
end;


procedure TMapForm.Landcover1Click(Sender: TObject);
(*
var
   x,y,Code : integer;
   z : float32;
begin
   for x := 0 to pred(DEMGlb[1].DEMheader.NumCol) do begin
      for y := 0 to pred(DEMGlb[1].DEMheader.NumRow) do begin
         if DEMGlb[MapDraw.DEMOnMap].GetElevMetersOnGrid(x,y,z) then begin
            Code := ReclassifyLandCover(MapDraw.DEMOnMap,round(z));
            DEMGlb[MapDraw.DEMOnMap].SetGridElevation(x,y,Code);
         end;
      end;
   end;
   DEMGlb[MapDraw.DEMOnMap].DEMHeader.ElevUnits := euSimpleLandCover;
   Dispose(DEMGlb[MapDraw.DEMOnMap].NLCDCats);
   DEMGlb[MapDraw.DEMOnMap].CheckForLandCover;
   //SetUpNLCDCategories(false,TStr,NLCDCats^);
*)
begin
   SimplifyLandCoverGrid(MapDraw.DEMOnMap);

end;


procedure TMapForm.LAScategoriestoshow1Click(Sender: TObject);
{$IfDef ExPointCloud}
begin
{$Else}
var
   fName : PathStr;
   Use : shortString;
   aClass,db : integer;
begin
    fName := ChangeFileExt(DEMGlb[MapDraw.DEMonMap].DEMfileName,'.dbf');
    if FileExists(fName) then begin
       db := LoadDataBaseFile(fName);
       GISdb[db].MyData.First;
       while not GISdb[db].MyData.eof do begin
          Use := GISdb[db].MyData.GetFieldByNameAsString('USE');
          aClass := GISdb[db].MyData.GetFieldByNameAsInteger('Code');
          LasCatMapped[aClass] := Use = 'Y';
          GISdb[db].MyData.Next;
       end;
    end
    else begin
       LASclassificationlegend1Click(Sender);
    end;
    DoBaseMapRedraw;
{$EndIf}
end;


procedure TMapForm.LASclassificationlegend1Click(Sender: TObject);
{$IfDef ExPointCloud}
begin
{$Else}
var
   x,y,zi : integer;
   z : float32;
   Title : shortstring;
   fName : PathStr;
   Results : tStringList;
   Total : int64;
   Count :  array[0..MaxLasCat] of int64;
begin
    fName := ChangeFileExt(DEMGlb[MapDraw.DEMonMap].DEMfileName,'.dbf');//   Petmar.NextFileNumber(MDTempDir, Title + '_','.csv');
    if FileExists(fName) then begin
       LoadDataBaseFile(fName);
    end
    else begin
      for x := 0 to MaxLasCat do Count[x] := 0;
      Total := 0;
      //Missing := 0;
      StartProgress('Land cover');
      for x := round(MapDraw.Mapcorners.BoundBoxDataGrid.xmin) to round(MapDraw.Mapcorners.BoundBoxDataGrid.xmax) do begin
         if (x mod 400 = 0) then UpDateProgressBar( (x-MapDraw.Mapcorners.BoundBoxDataGrid.xmin) / (MapDraw.Mapcorners.BoundBoxDataGrid.xmax - MapDraw.Mapcorners.BoundBoxDataGrid.xmin));
         for y := round(MapDraw.Mapcorners.BoundBoxDataGrid.ymin) to round(MapDraw.Mapcorners.BoundBoxDataGrid.ymax) do begin
            if DEMGlb[MapDraw.DEMonMap].GetElevMetersOnGrid(x,y,z) then  begin
               zi := round(z);
               if (zi > 0) and (zi <= MaxLasCat) then begin
                  inc(Count[zi]);
                  inc(Total);
               end;
               //else inc(Missing);
            end;
            //else inc(Missing);
         end;
      end;
      EndProgress;

      Results := tStringList.Create;
      Title := 'PERCENT,NAME,CODE,COLOR,USE';
      if MDDef.LongLandCoverResults then Title := Title +  ',CATEGORY,NUMBER';
      Results.Add(Title);
      for x := 0 to MaxLasCat do if Count[x] > 0 then begin
         Title := RealToString(100 * Count[x]/Total,8,2) + ',' + LasCatName[x] + ',' + IntToStr(x) + ',' + IntToStr(ConvertPlatformColorToTColor(Las_rgb_colors[x])) + ',Y';
         Results.Add(Title);
      end;
      fName := ChangeFileExt(DEMGlb[MapDraw.DEMonMap].DEMfileName,'.csv');
      StringList2CSVtoDB(Results,fName);
    end;
{$EndIf}
end;


procedure TMapForm.LAScurrentmaparea1Click(Sender: TObject);
{$IfDef ExPointCloud}
begin
{$Else}
var
   FileName : PathStr;
   GL : tGridLimits;
begin
   FileName := '';
   GL := MapDraw.MapAreaDEMGridLimits;
   FileName := DEMGlb[MapDraw.DEMonMap].ExportASCII(GL.xGridLow,GL.yGridLow,GL.xGridHigh,gl.YGridHigh);
   WinExecAndWait32(lastools_txt2las_cmd(FileName,IntToStr(MapDraw.PrimMapProj.projUTMZone) + 'U'));
{$EndIf}
end;


procedure TMapForm.LASexport1Click(Sender: TObject);
{$IfDef ExVegDensity}
begin
{$Else}
var
   FName : PathStr;
begin
   with MapDraw,MapCorners do begin
      fName := MainMapData;
      Petmar.GetFileNameDefaultExt('LAS file','*.las',fName,false);
      DEMGlb[MapDraw.DEMonMap].VegDensityLayers[1].ExportToLAS(round(BoundBoxDataGrid.xmin),round(BoundBoxDataGrid.ymin),round(BoundBoxDataGrid.xmax),round(BoundBoxDataGrid.ymax),fName);
   end;
   {$EndIf}
end;

procedure TMapForm.LASFile1Click(Sender: TObject);
begin
   {$IfDef ExPointCloud}
   {$Else}
      {$If Defined(RecordLAS) or Defined(RecordUTMZone)} WriteLineToDebugFile('TMapForm.LASFile1Click in, Map UTM zone=' + IntToStr(MapDraw.PrimMapProj.projUTMZone)); {$EndIf}
      if (pt_cloud_opts_fm = Nil) then Point_cloud_options.OvelayPointClouds(Self);
      {$IfDef RecordUTMZone} WriteLineToDebugFile('TMapForm.LASFile1Click out, Map UTM zone=' + IntToStr(MapDraw.PrimMapProj.projUTMZone)); {$EndIf}
   {$EndIf}
end;


procedure TMapForm.LatitudeRange1Click(Sender: TObject);
var
   bb : sfBoundBox;
begin
   bb := MapDraw.MapCorners.BoundBoxGeo;
   GetLatLongDefault(MapDraw.PrimMapProj,'NW corner of valid region',bb.YMax,bb.XMin);
   GetLatLongDefault(MapDraw.PrimMapProj,'SE corner of valid region',bb.YMin,bb.XMax);
   DEMGlb[MapDraw.DEMonMap].CutOutGeoBox(bb);
   RespondToChangedDEM;
end;



procedure TMapForm.Latitudinalscalebars1Click(Sender: TObject);


      procedure DoAScalebar(y : integer);
      var
         Lat1,Long1,Lat2,Long2,Dist,Bearing : float64;
         Bitmap : tMyBitmap;
      begin
         MapDraw.ScreenToLatLongDegree(MapDraw.MapXSize div 2,y, Lat1,Long1);
         MapDraw.ScreenToLatLongDegree(0,y, Lat2,Long2);
         VincentyCalculateDistanceBearing(Lat1,Long1,Lat2,Long2,Dist,Bearing);
         Dist := Dist / (MapDraw.MapXSize div 2);
         Bitmap := MapDraw.DrawScaleBarOnBitmap(Dist);
         if (y > 0) then begin
            if y = Pred(Image1.Height) then y := pred(Image1.Height) - Bitmap.Height
            else y := y - Bitmap.Height div 2;
         end;
         Image1.Canvas.Draw(MapDraw.MapXSize - Bitmap.Width,y,Bitmap);
         Bitmap.Free;
      end;

begin
   DoAScalebar(0);
   DoAscaleBar(MapDraw.MapYSize div 2);
   DoAScalebar(pred(Image1.Height));
end;

procedure TMapForm.LatLong1Click(Sender: TObject);
begin
   CreateGridToMatchMap(cgLatLong);
end;

procedure TMapForm.LatlongofPLSSposition1Click(Sender: TObject);
var
   PLSSString : ShortString;
   Lat,Long : float64;
begin
   Get_PLSS.GetPLSSLocation(PLSSString,Lat,Long,Self);
end;


procedure TMapForm.LC100landcoverwaterbodies1Click(Sender: TObject);
begin
   MarkWaterMissingInThisDEM(MapDraw.DEMonMap);
end;

procedure TMapForm.LCCstandardparallels1Click(Sender: TObject);
var
   LineColor : tPlatformColor;
   LineSize :  byte;
begin
   LineColor := claLime;
   LineSize := 3;
   PickLineSizeAndColor('Key latitudes',Nil,LineColor,LineSize);
   Image1.Canvas.Pen.Color := ConvertPlatformColorToTColor(LineColor);
   Image1.Canvas.Pen.Width := LineSize;
   MapDraw.DrawLatLine(Image1.Canvas,MapDraw.PrimMapProj.Phi1/DegToRad);
   MapDraw.DrawLatLine(Image1.Canvas,MapDraw.PrimMapProj.Phi2/DegToRad);
end;


procedure TMapForm.Leastcostpath1Click(Sender: TObject);
begin
   ChangeDEMNowDoing(StartLeastCostPath);
end;

procedure TMapForm.Legend1Click(Sender: TObject);
begin
    {$IfDef RecordLegend} WriteLineToDebugFile('TMapForm.Legend1Click in, LegendOptions=' + IntToStr(LegendOptionsAvailable) + '  MapType=' + IntToStr(MapDraw.MapType)); {$EndIf}
    if (LegendOptionsAvailable = 1) then begin
       if DatabaseLegend1.Visible then Databaselegend1Click(Sender);
       if GridVATLegend1.Visible then GridVATLegend1Click(Sender);
       if NLCDLegend1.Visible then NLCDLegend1Click(Sender);
       if LASclassificationlegend1.Visible then LASclassificationlegend1Click(Sender);
       if Shapefilegrouplegend1.Visible then Shapefilegrouplegend1Click(Sender);
       if Cartogrouplegend1.Visible then Cartogrouplegend1Click(Sender);
       if Fixedpalettecategories1.Visible then Fixedpalettecategories1Click(Sender);
    end
    else LegendPopUpMenu14.Popup(Mouse.CursorPos.X,Mouse.CursorPos.Y);
end;

procedure TMapForm.Legendsmarginalia1Click(Sender: TObject);
begin
   {$IfDef ExMarginalia}
   {$Else}
      DEMMarginalia.MapMarginalia(Self);
   {$EndIf}
end;

procedure TMapForm.Level0countries1Click(Sender: TObject);
var
   SaveLastDB : PathStr;
   ForceColor : tColor;
   ForceWidth : byte;
   ShowField  : ShortString;
   db : integer;
begin
   SaveBackupDefaults;
   SaveLastDB := LastDataBase;
   if (Sender = Level0countries1) then begin
      LastDataBase := GADMDir + 'ADM0\';
      ForceColor := ConvertPlatformColorToTColor(MDDef.CountryOutline_Color);
      ForceWidth := MDDef.CountryOutline_Width;
      ShowField := 'NAME_ENGLI';
   end
   else begin
      LastDataBase := GADMDir + 'ADM1\';
      ForceColor := ConvertPlatformColorToTColor(MDDef.ProvinceOutline_Color);
      ForceWidth := MDDef.ProvinceOutline_Width;
      ShowField := 'NAME_1';
   end;
   MDDef.DefDBFilter := 2;
   db := OpenDBonMap('','',true,true,true,ForceColor,ForceWidth,ShowField);
   if db <> 0 then GISdb[db].DBTablef.HideColumns;
   LastDataBase := SaveLastDB;
   RestoreBackupDefaults;
end;

procedure TMapForm.Level1stateprovince1Click(Sender: TObject);
begin
   Level0countries1Click(Sender);
end;

procedure TMapForm.Water1Click(Sender: TObject);
begin
   Markasmissing1Click(Sender);
end;


procedure TMapForm.Watermask1Click(Sender: TObject);
begin
   {$IfDef ExLandsatQA}
      MessageToContinue('Disabled while rewriting for collection 2');
   {$Else}
      SatImage[Mapdraw.SATonMap].GetLandsatQAMap('Water', SatImage[Mapdraw.SATonMap].OriginalFileName,4,5);
   {$EndIf}
end;

procedure TMapForm.Waverefraction1Click(Sender: TObject);
begin
   {$IfDef ExWaveRefraction}
   {$Else}
      //Refraction_model.CreateWaveRefraction(self);
   {$EndIf}
end;

procedure TMapForm.QuickbasemaptoGoogleEarth1Click(Sender: TObject);
begin
   MDDef.AskAboutKMLExport := false;
   MDDef.KMLOutputOption := 1;
   ExportMapToGoogleEarth(false);
end;


procedure TMapForm.Quickclassfication1Click(Sender: TObject);
var
   VegGrid,SoilGrid,SnowGrid,WaterGrid,x,y,xf,yf : integer;
   Color : tColor;
   Lat,Long : float64;
   z,z2 : float32;
begin
    VegGrid := NewSatWindow(nsbNDVI);
    SoilGrid := NewSatWindow(nsbNDSIsoil);
    SnowGrid := NewSatWindow(nsbNDSIsnow);
    WaterGrid := NewSatWindow(nsbNDWI);
    MapDraw.DEMonMap := VegGrid;
    for x := 0 to pred(MapDraw.MapXSize) do begin
       for y := 0 to pred(MapDraw.MapYSize) do begin
          MapDraw.ScreenToLatLongDegree(x,y,Lat,Long);
          DEMGLB[VegGrid].LatLongDegreeToDEMGridInteger(Lat,Long,xf,yf);
           if DEMGLB[VegGrid].GetElevMetersOnGrid(xf,yf,z) then begin
              Color := clLime;
              if DEMGLB[SoilGrid].GetElevMetersOnGrid(xf,yf,z2) and (z2 > z) then begin
                 Color := clBrown;
                 z := z2;
              end;
              if DEMGLB[SnowGrid].GetElevMetersOnGrid(xf,yf,z2) and (z2 > z) then begin
                 Color := clWhite;
                 z := z2;
              end;
              if DEMGLB[WaterGrid].GetElevMetersOnGrid(xf,yf,z2) and (z2 > z) then begin
                 Color := clBlue;
              end;
              Image1.Canvas.Pixels[x,y] := Color;
           end;
       end;
    end;
end;


procedure TMapForm.Quicksealevelrise1Click(Sender: TObject);
begin
   {$IfDef ExDrainage}
   {$Else}
      BasinFlooding(Self,-99,-99);
   {$EndIf}
end;


procedure TMapForm.racks1Click(Sender: TObject);
var
  TheDir : PathStr;
  FileNames : tStringList;
  I{,db} : Integer;
begin
   GetDosPath('tracks',TheDir);
   FileNames := Nil;
   Petmar.FindMatchingFiles(TheDir,'*.DBF',FileNames);
   for I := 0 to pred(FileNames.Count) do begin
      OpenDBonMap('',FileNames.Strings[i],true,false);
   end;
   FileNames.Free;
end;

procedure TMapForm.PrintSpeedButtonClick(Sender: TObject);
begin
   PreparePrinterImage1Click(Sender);
end;


procedure TMapForm.Saveimage2Click(Sender: TObject);
begin
   Saveimage1Click(Sender);
end;


procedure TMapForm.Missingdata1Click(Sender: TObject);
begin
   MissingPointsInGrids;
end;

procedure TMapForm.Missingdatacolor1Click(Sender: TObject);
begin
   QueryColor(MDdef.MissingDataColor);
   DoBaseMapRedraw;
end;

procedure TMapForm.Missingdatatospecifiedvalue1Click(Sender: TObject);
var
   z : float32;
begin
   z := 0;
   ReadDefault('Replace missing data with',z);
   DEMGlb[MapDraw.DEMOnMap].MissingDataToConstantVelue(z);
   RespondToChangedDEM;
end;


procedure TMapForm.Missingpoints1Click(Sender: TObject);
begin
   MaskFromSecondGrid(0, msSecondMissing);
end;


procedure TMapForm.Validpoints1Click(Sender: TObject);
begin
   MaskFromSecondGrid(0, msSecondValid);
end;


procedure TMapForm.MaskFromSecondGrid(SecondGrid : integer; HowMask : tMaskGrid; ShowResults : boolean = false);
var
   fName : PathStr;
begin
   {$IfDef RecordEditsDEM} WriteLineToDebugFile('TMapForm.MaskFromSecondGrid, mask mode=' + IntToStr(ord(HowMask))); {$EndIf}
   fName := '';
   if (SecondGrid = 0) and (not GetDEM(SecondGrid,true,'masking')) then exit;
   if (SecondGrid < 0) then begin
      fName := ExtractFilePath(LastDEMName);
      if not LoadNewDEM(SecondGrid,fName,false) then exit;
   end;

   DEMGlb[MapDraw.DEMonMap].SecondGridIdentical(SecondGrid);

   {$IfDef RecordEditsDEM} WriteLineToDebugFile('Grid being masked: ' + DEMGlb[MapDraw.DEMonMap].AreaName); WriteLineToDebugFile('  Masking grid: ' + DEMGlb[SecondGrid].AreaName); {$EndIf}

   StartProgress('Mask');
   EditsDone := 0;
   ParallelRowsDone := 0;

   MaskStripFromSecondGrid(MapDraw.DEMonMap,SecondGrid,HowMask);

   EndProgress;
   ThreadsWorking := false;
   if (fName <> '') then CloseSingleDEM(SecondGrid);

   RespondToChangedDEM;
   if ShowResults then MessageToContinue('Points removed=' + IntToStr(EditsDone));
   {$IfDef RecordEditsDEM} WriteLineToDebugFile('TMapForm.MaskFromSecondGrid, Points removed=' + IntToStr(EditsDone) ); {$EndIf}

end;


procedure TMapForm.MissingPointsInMatchingGrid1Click(Sender: TObject);
begin
   MaskFromSecondGrid(-1,msSecondMissing);
end;


procedure TMapForm.Routeobservation1Click(Sender: TObject);
begin
   {$IfDef RecordAmbush} WriteLineToDebugFile('Ambush started (TMapForm.Routeobservation1Click)'); {$EndIf}
   ChangeDEMNowDoing(RouteObservation);
end;


procedure TMapForm.Truenorthlines1Click(Sender: TObject);
{$IfDef ExMag}
begin
{$Else}
const
   TrueNorthLineWidth : integer = 3;
var
   TrueNorthLineColor : tPlatformColor;
   lat,long,Dist : float64;
   DEC,DIP,TI,GV : double;
   MagNorthLineInc,x,xf,yf : integer;
   TStr : shortString;
begin
   MagNorthLineInc := 200;  //pixels
   TrueNorthLineColor := claRed;
   if (Sender = Truenorthlines1) then TStr := 'True' else TStr := 'Magnetic';

   ReadDefault(TStr + ' north line spacing (pixels)',MagNorthLineInc);
   Petmar.PickLineSizeAndColor(TStr + ' north',Nil,TrueNorthLineColor,TrueNorthLineWidth);
   Dist := 1.414 * MapDraw.MapYSize * MapDraw.ScreenPixelSize;
   x := -3 * MagNorthLineInc div 2;
   Image1.Canvas.Pen.Color := ConvertPlatformColorToTColor(TrueNorthLineColor);
   Image1.Canvas.Pen.Width := TrueNorthLineWidth;
   while (x < MapDraw.MapXSize + MagNorthLineInc) do begin
      MapDraw.ScreenToLatLongDegree(x,MapDraw.MapYSize,Lat,Long);
      if Sender = Truenorthlines1 then Dec := 0
      else MagVr1(0,lat,Long,CurMagYear, DEC,DIP,TI,GV);
      VincentyPointAtDistanceBearing(Lat,Long,Dist,Dec,Lat,Long);
      MapDraw.LatLongDegreeToScreen(Lat,Long,xf,yf);
      Image1.Canvas.MoveTo(x,MapDraw.MapYSize);
      Image1.Canvas.LineTo(xf,yf);
      inc(x,MagNorthLineInc);
   end;
{$EndIf}
end;

procedure TMapForm.NearestpeakoneachDEM1Click(Sender: TObject);
var
   DEM,xloc,yloc : integer;
   xg,yg,LocMax,zref : float32;
   Distance,Bearing,
   Lat,Long : float64;
   bb : tGridLimits;
   Results : tStringList;
   fName : PathStr;

   procedure OneDEM(DEM : integer);
   var
      TStr : shortString;
   begin
      DEMglb[DEM].LatLongDegreeToDEMGrid(RightClickLat,RightClickLong,xg,yg);
      bb.XGridLow := round(xg) - 10;
      bb.XGridHigh := round(xg) + 10;
      bb.YGridLow := round(yg) - 10;
      bb.YGridHigh := round(yg) + 10;

      DEMglb[DEM].FindLocationOfMaximum(bb,xloc,yloc,LocMax);
      DEMglb[DEM].DEMGridToLatLongDegree(xloc,yloc,lat,long);
      if (DEM <> MapDraw.DEMonMap) then begin
         VincentyCalculateDistanceBearing(RightClickLat,RightClickLong,Lat,Long,Distance,Bearing);
         TStr := ',' + RealToString(Distance,-12,-2) + ',' + RealToString(Bearing,-12,1) + ',' + RealToString(LocMax-zref,-12,-2);
      end
      else begin
         RightClickLat := Lat;
         RightClickLong := Long;
         TStr := ',-9999,-9999,-9999';
         Zref := LocMax;
      end;
      Results.Add(DEMGlb[DEM].AreaName + ',' + RealToString(Lat,-12,-8) + ',' + RealToString(Long,-12,-8) + ',' + RealToString(LocMax,-12,2) + TStr );
   end;

begin
   Results := tStringList.Create;
   Results.Add('DEM,LAT,LONG,ELEV_M,DISPLACE,DIRECTION,DELTA_Z');
   OneDEM(MapDraw.DEMonMap);
   for DEM := 1 to MaxDEMDataSets do begin
      if ValidDEM(DEM) and (DEM <> MapDraw.DEMonMap) then begin
         OneDEM(DEM);
      end;
   end;
   fName := Petmar.NextFileNumber(MDTempDir, 'Peaks_in_vicinity_','.dbf');
   StringList2CSVtoDB(Results,fName);
end;

procedure TMapForm.Newband1Click(Sender: TObject);
begin
   {$IfDef ExSat}
   {$Else}
   if MapDraw.ValidSatOnMap and (SatImage[MapDraw.SATonMap] <> Nil) then  begin
      RGBgrayscale1.Visible := (SatImage[MapDraw.SATonMap].NumBands >= 3);
      VARI1.Visible := (SatImage[MapDraw.SATonMap].NumBands >= 3);
      AddBands1.Visible := (SatImage[MapDraw.SATonMap].NumBands >= 2);
      NDVI1.Visible := (SatImage[MapDraw.SATonMap].NumBands >= 3);
      NDSI1.Visible := (SatImage[MapDraw.SATonMap].NumBands >= 3);
      NDWI1.Visible := (SatImage[MapDraw.SATonMap].NumBands >= 3);
      NormalizedBurnIndex1.Visible := (SatImage[MapDraw.SATonMap].NumBands >= 3);
      BandRatio1.Visible := (SatImage[MapDraw.SATonMap].NumBands >= 2);
      Sentinel2bandreflectance1.Visible := SatImage[MapDraw.SATonMap].IsSentinel2;
      LandsatTIR1.Visible := SatImage[MapDraw.SATonMap].LandsatNumber <> 0;
      Surfaceradiance2.Visible := SatImage[MapDraw.SATonMap].LandsatNumber <> 0;
      NewBandPopupMenu7.Popup(Mouse.CursorPos.X,Mouse.CursorPos.Y);
   end;
   {$EndIf}
end;

procedure TMapForm.Newrouteobservation1Click(Sender: TObject);
begin
   ChangeDEMNowDoing(RouteObservation);
end;


procedure TMapForm.Restorerouteobservation1Click(Sender: TObject);
{$IfDef ExViewshed}
begin
{$Else}
var
   fName : PathStr;
begin
   fName := DEMdefs.MovieDir;
   if GetFileFromDirectory('Route Observation','*.FLT',FName) then begin
      StreamProfileResults := tStringList.Create;
      StreamProfileResults.LoadFromFile(FName);
      SetUpAmbush;
   end;
   {$EndIf}
end;



procedure TMapForm.Monthlywinds1Click(Sender: TObject);
begin
   {$IfDef ExGeography}
   {$Else}
      GISdb[WindsDB].QueryGeoBox(RightClickLat + 2.495,RightClickLong-2.495,RightClickLat-2.495,RightClickLong+2.495,true);
      if (GISdb[WindsDB].MyData.FiltRecsInDB > 0) then begin
         GISdb[WindsDB].MonthlyWindPlotCurrentPoint;
      end;
      GISdb[WindsDB].MyData.ApplyFilter('');
      GISdb[WindsDB].DBTablef.ShowStatus;
   {$EndIf}
end;


procedure TMapForm.Moonriseset1Click(Sender: TObject);
{$IfDef ExGeography}
begin
{$Else}
var
   Month,Day,Year,Duration : integer;
begin
   Month := -99;
   Duration := 365;
   GetDateAndDuration(Month,Day,Year,Duration);
   MoonRise(Month,Day,Year,Duration,RightClickLat,RightClickLong);
{$EndIf}
end;

procedure TMapForm.Flattenlake1Click(Sender: TObject);
begin
   EditGridViaColor(emvcFlattenLake,clBlack);
end;


procedure TMapForm.Flickermovie1Click(Sender: TObject);
var
   i : integer;
   MovieList : tStringList;
   fName : PathStr;
begin
   MovieList := tStringList.Create;
   for i := 1 to MaxDEMDataSets do begin
      if ValidDEM(i) then begin
         if (i <> MapDraw.DEMonMap) then DEMGlb[i].SelectionMap.MatchMapToThisOne(Self);
         fName := DEMGlb[i].AreaName + '.bmp';
         CopyFile(DEMGlb[i].SelectionMap.MapDraw.FullMapfName, MDtempDir + fName);
         MovieList.Add(MDtempDir + fName);
      end;
   end;
   fName := NextFileNumber(MDtempDir,'map_4_movie_','.mov');
   MovieList.SaveToFile(fName);
   CreateNewMovie(fName);
   MovieList.Free;
end;


procedure TMapForm.Floatingpoint1Click(Sender: TObject);
begin
   ThinDEM1Click(Sender);
end;

procedure TMapForm.Floodbasin1Click(Sender: TObject);
begin
   {$IfDef ExDrainage}
   {$Else}
      ChangeDEMNowDoing(FloodBasin);
   {$EndIf}
end;


procedure TMapForm.HAGvegheight1Click(Sender: TObject);
begin
   MDDef.VegOptionMap := voVeg;
   DoBaseMapRedraw;
end;

procedure TMapForm.Help1Click(Sender: TObject);
begin
   DisplayHTMLTopic('html\demb6i0e.htm');
end;


procedure TMapForm.Pointsshapefile1Click(Sender: TObject);
begin
   Lineshapefile1Click(Sender);
end;

procedure TMapForm.Differencebetweentwogrids2Click(Sender: TObject);
begin
   {$IfDef ExGeoStats}
   {$Else}
      DEMStat.GridDiffernces(true);
   {$EndIf}
end;

procedure TMapForm.DifClick(Sender: TObject);
begin
   {$IfDef ExGeoStats}
   {$Else}
      DEMStat.GridDiffernces(false);
   {$EndIf}
end;


procedure TMapForm.Pointslopebyregionsize1Click(Sender: TObject);
   begin
   {$IfDef ExGeoStats}
   {$Else}
      PointSlopesByRegionSize(MapDraw.DEMonMap,RightClickLat,RightClickLong);
   {$EndIf}
end;


procedure TMapForm.Pointtimeseries1Click(Sender: TObject);
begin
   ChangeDEMNowDoing(NDVIPointTimeSeries);
end;


procedure TMapForm.Pointzvaluesallgrids1Click(Sender: TObject);
var
   sl : tStringList;

   procedure Figure(What : integer);
   var
      z : float32;
      xg,yg : integer;
      i : integer;
      xgf,ygf : float32;
   begin
      if What = 1 then begin
         DEMGlb[MapDraw.DEMonMap].LatLongDegreeToDEMGrid(RightClickLat,RightClickLong,xgf,ygf);
         if ValidDEM(MapDraw.DEMonMap) and DEMGlb[MapDraw.DEMonMap].GetElevFromLatLongDegree(RightClickLat,RightClickLong,z) then begin
            sl.Add(RealToString(z,12,4) + ' ' + ElevUnitsAre(DEMGlb[MapDraw.DEMonMap].DEMHeader.ElevUnits) + '   ' + DEMGlb[MapDraw.DEMonMap].AreaName + '  ' + DEMGridString(xgf,ygf));
         end;

         for i := 1 to MaxDEMDataSets do begin
            if ValidDEM(i) and (i <> MapDraw.DEMonMap) then begin
               DEMGlb[i].LatLongDegreeToDEMGrid(RightClickLat,RightClickLong,xgf,ygf);
               if DEMGlb[i].GetElevFromLatLongDegree(RightClickLat,RightClickLong,z) then begin
                  sl.Add(RealToString(z,12,4)+ ' ' + ElevUnitsAre(DEMGlb[i].DEMHeader.ElevUnits) + '   ' + DEMGlb[i].AreaName + '  ' + DEMGridString(xgf,ygf));
               end;
            end;
         end;
      end
      else begin
         DEMGlb[MapDraw.DEMonMap].LatLongDegreeToDEMGridInteger(RightClickLat,RightClickLong,xg,yg);
         if ValidDEM(MapDraw.DEMonMap) and DEMGlb[MapDraw.DEMonMap].GetElevMetersOnGrid(xg,yg,z) then begin
            sl.Add(RealToString(z,12,4) + ' ' + ElevUnitsAre(DEMGlb[MapDraw.DEMonMap].DEMHeader.ElevUnits) + '   ' + DEMGlb[MapDraw.DEMonMap].AreaName + '  ' + DEMGridString(xg,yg));
         end;

         for i := 1 to MaxDEMDataSets do begin
            if ValidDEM(i) and (i <> MapDraw.DEMonMap) then begin
               DEMGlb[i].LatLongDegreeToDEMGridInteger(RightClickLat,RightClickLong,xg,yg);
               if DEMGlb[i].GetElevMetersOnGrid(xg,yg,z) then begin
                  sl.Add(RealToString(z,12,4)+ ' ' + ElevUnitsAre(DEMGlb[i].DEMHeader.ElevUnits)+ '   ' + DEMGlb[i].AreaName + '  ' + DEMGridString(xg,yg));
               end;
            end;
         end;
      end;
   end;

begin
   sl := tStringList.Create;
   sl.Add('Point clicked  ' + LatLongDegreeToString(RightClickLat,RightClickLong,MDDef.OutPutLatLongMethod));
   Figure(1);
   sl.Add('');
   sl.Add('Nearest grid point');
   Figure(2);
   DisplayAndPurgeStringList(sl,'All grids at location');
end;

procedure TMapForm.Polarorbitersatellitevisibility1Click(Sender: TObject);
begin
   ChangeDEMNowDoing(VisPolarOrbiter);
end;

procedure TMapForm.Popuplegends1Click(Sender: TObject);
begin
   Legend1Click(Sender);
end;

procedure TMapForm.PortionofDEMwithdata1Click(Sender: TObject);
var
   FileName : PathStr;
begin
   FileName := '';
   DEMGlb[MapDraw.DEMonMap].SavePartOfDEMWithData(FileName);
   DoFastMapRedraw;
end;

procedure TMapForm.Mapdistortion1Click(Sender: TObject);
begin
   {$IfDef ExCartography}
   {$Else}
      GetTissotOptions(Nil,false);
      ChangeDEMNowDoing(MapTissotIndicatrix);
   {$EndIf}
end;


procedure TMapForm.Magneticvariation1Click(Sender: TObject);
begin
   Petmar.PickSymbol(Nil,MDDef.DefGISSymbol,'Magnetic variation');
   EditMyFont(MDDef.DefGisLabelFont1);
   ChangeDEMNowDoing(ShowMagneticVariation);
end;


procedure TMapForm.Tissotindicatrix1Click(Sender: TObject);
begin
   Tissot.GetTissotOptions(Self);
end;

procedure TMapForm.OverlayTissot;
{$IfDef ExCartography}
begin
{$Else}
var
   bmp : tMyBitmap;
begin
   {$IfDef RecordTissot} WriteLineToDebugFile('TMapForm.OverlayTissot in'); {$EndIf}
   DoFastMapRedraw;
   CopyImageToBitmap(Image1,bmp);
   ShowHourglassCursor;
   MapDraw.DrawTissotIndicatrixOverlay(bmp);
   Image1.Picture.Graphic := bmp;
   bmp.free;
   CheckThatLegendsAreOnTop;
   ShowDefaultCursor;
{$EndIf}
end;


procedure TMapForm.Abbreviations1Click(Sender: TObject);
begin
   {$IfDef ExGeography}
   {$Else}
      CreateKoppenLegend(true);
   {$EndIf}
end;

procedure TMapForm.ableofcontents1Click(Sender: TObject);
begin
   SpeedButton8Click(Sender);
end;

procedure TMapForm.ableofcontents2Click(Sender: TObject);
begin
   SpeedButton8Click(Sender);
end;

procedure TMapForm.Abortcurrentoperation1Click(Sender: TObject);
begin
   ChangeDEMNowDoing(JustWandering);
   RecMoving := false;
   CheckProperTix;
end;


procedure TMapForm.Abortcurrentoperation2Click(Sender: TObject);
begin
   Abortcurrentoperation1Click(Sender);
end;

procedure TMapForm.Absolutevalue1Click(Sender: TObject);
begin
   Multiplyzvalues1Click(Sender);
end;

procedure TMapForm.Areashapefile1Click(Sender: TObject);
begin
   Lineshapefile1Click(Sender);
end;

procedure TMapForm.DEMreflectance1Click(Sender: TObject);
begin
   DEMelevationcolors1Click(Sender);
end;


procedure TMapForm.DEMsatpoint1Click(Sender: TObject);
var
   Lat,Long : float64;
   TStr : ANSIString;
begin
   MapDraw.ScreenToLatLongDegree(LastX,LastY,Lat,Long);
   TStr := LatLongDegreeToString(Lat,Long,NearestMinute) + MessLineBreak;
   TStr := TStr + 'SW corner 1 degree tile:  ' + SWcornerString(Lat,Long,1) + MessLineBreak;
   TStr := TStr + 'SW corner 5 degree tile:  ' + SWcornerString(Lat,Long,5) + MessLineBreak;
   TStr := TStr + 'SW corner 20 degree tile: ' + SWcornerString(Lat,Long,20) + MessLineBreak + MessLineBreak;

   TStr := TStr + 'NW corner 1 degree tile:  ' + NWcornerString(Lat,Long,1) + MessLineBreak;
   TStr := TStr + 'NW corner 5 degree tile:  ' + NWcornerString(Lat,Long,5) + MessLineBreak;
   TStr := TStr + 'NW corner 20 degree tile: ' + NWcornerString(Lat,Long,20) + MessLineBreak;
   MessageToContinue(TStr,True);
end;


procedure TMapForm.DEMsfrommaplibrary1Click(Sender: TObject);
begin
   LoadDEMsCoveringPoint(RightClickLat,RightClickLong,true);
end;


procedure TMapForm.DEMsfrommaplibrarymaparea1Click(Sender: TObject);
begin
   LoadDEMsCoveringBox(MapDraw.MapCorners.BoundBoxGeo,true);
end;


procedure TMapForm.DEMelevationcolors1Click(Sender: TObject);
{$IfDef ExCartography}
begin
{$Else}
type
   tScreenLocations = array[0..5000,0..5000] of tSingleCoords;
var
   CartMovieOptsForm: TCartMovieOptsForm;
   tFile,fName : PathStr;
   NewSatImage,x,y,i,xi,yi,AngleShift,Steps,SatJustLoaded,xgrid,Ygrid : integer;
   Lat,Long : float64;
   P0,P1 : pRGB;
   MovieList : tStringList;
   MaskBitmap,Bitmap,SatBMP : tMyBitmap;
   ScreenLocations : ^tScreenLocations;


         procedure DesiredSteps;
         begin
            case MDDef.CartMovieSteps of
               0 : Steps := 36;
               1 : Steps := 24;
               2 : Steps := 18;
               3 : Steps := 12;
            end;
         end;

begin
   {$IfDef RecordGlobeRotation} WriteLineToDebugFile(' Enter  TMapForm.DEMelevationcolors1Click'); {$EndIf}
   if (MapDraw.VectorIndex <> 0) then begin
      SubtractOverlay(Self,ovoTissot);
      if (MapDraw.PrimMapProj.PName in [MercatorEllipsoid,OldStereographic,UTMEllipsoidal,LamAzEqArea,OrthoProj,AlbersEqAreaConicalEllipsoid,LambertConformalConicEllipse]) then begin
         MapDraw.MapType := MDDef.DefaultElevationColors;
         SatBMP := Nil;
         if (Sender = Predictedseafloorages1) or (Sender = DEMreflectance1) or (Sender = Globaltopography1) then begin
            if (Sender = Globaltopography1) or (Sender = Predictedseafloorages1) then begin
               if (Sender = Globaltopography1) then fName := ETOPODEMName;
               if (Sender = Predictedseafloorages1) then fName := PredAgesFile;
               LoadNewDEM(MapDraw.DEMonMap,fName,false);
               MapDraw.MapType := mtElevRainbow;
            end;
            MapDraw.ScaleMapElevationsToDEM;
         end
         {$IfDef ExSat}
         {$Else}
            else if (Sender = BlueMarble1) or (Sender = OpenImage1) then begin
                if (Sender = BlueMarble1) then begin
                   NewSatImage := OpenAndDisplaySatelliteScene(nil,MainMapData + 'nasa\world20min.jpg',true,true,true);
                   if NewSatImage = 0 then exit;
                   SatJustLoaded := NewSatImage;
                end
                else SatJustLoaded := 1;
                CopyImageToBitmap(SatImage[SatJustLoaded].SelectionMap.Image1,SatBMP);
            end
         {$EndIf}
         else if (Sender <> Vectoroutlines2) and (Sender <> Quickrotatemap1) then begin
            TFile := '';
            if (MapDraw.DEMonMap = 0) and (not NewArea(True,MapDraw.DEMonMap,'to overlay on vector map',TFile)) then exit;
            MapDraw.MapType := mtElevRainbow;
            MapDraw.ScaleMapElevationsToDEM;
         end;

         CloneImageToBitmap(Image1,MaskBitMap);

         if (Sender = Predictedseafloorages1) or (Sender = Globaltopography1) or (Sender = BlueMarble1)  or (Sender = OpenImage1) or (Sender = Vectoroutlines2) then begin
            CartMovieOptsForm := TCartMovieOptsForm.Create(Application);
            CartMovieOptsForm.CheckBox1.Checked := MDDef.RotatingEarthOutlines;
            CartMovieOptsForm.RadioGroup1.ItemIndex := MDDef.CartMovieSteps;
            CartMovieOptsForm.ShowModal;
            MDDef.RotatingEarthOutlines := CartMovieOptsForm.CheckBox1.Checked;
            MDDef.CartMovieSteps := CartMovieOptsForm.RadioGroup1.ItemIndex;
            DesiredSteps;
            if CartMovieOptsForm.CheckBox2.Checked then begin
               if (Sender = DEMreflectance1) then ChangeReflectanceOptions(Self)
               else ChangeElevationMap(MDDef.MergeInt,MDDef.MergeHue,MDDef.MergeSat,Self);
            end;
            CartMovieOptsForm.Free;
         end
         else if (Sender = Quickrotatemap1) then begin
            DesiredSteps;
         end
         else Steps := 0;


         if (Steps <> 0) then begin
            AngleShift := 360 div Steps;
            MovieList := tStringList.Create;
         end;

         {$IfDef RecordGlobeRotation} WriteLineToDebugFileWitheTime('Create ScreenLocations'); {$EndIf}

         New(ScreenLocations);
         ShowSatProgress := false;
         StartProgress('Setup');
         for x := 0 to pred(MapDraw.MapXSize) do begin
            if (x mod 50 = 0) then UpdateProgressBar(x/MapDraw.MapXSize);
            for y := 0 to pred(MapDraw.MapYSize) do begin
               MapDraw.ScreenToLatLongDegree(x,y,Lat,Long);
               if (abs(lat) < 90.000001) and (abs(Long) < 180.00001) then begin
                  ScreenLocations^[x,y].Lat := Lat;
                  ScreenLocations^[x,y].Long := Long;
               end
               else begin
                  ScreenLocations^[x,y].Lat := -9999;
                  ScreenLocations^[x,y].Long := -9999;
               end;
            end;
         end;

         StartProgress('Rotate');
         for i := 0 to Steps do begin
            UpdateProgressBar(i/Steps);
            {$IfDef RecordGlobeRotation} WriteLineToDebugFile('Step ' + IntToStr(i)); {$EndIf}
            CloneImageToBitmap(Image1,BitMap);
            if (Steps > 0) then begin
               MapDraw.PrimMapProj.Long0 := (360 - i * AngleShift) * DegToRad;
               MapDraw.PrimMapProj.GetProjectParameters;
               MapDraw.SetFullMapCoverage;
               DoCompleteMapRedraw;
            end;

            if (Sender = Vectoroutlines2) or (Sender = Quickrotatemap1) then begin
               Bitmap.Free;
               CopyImageToBitmap(Image1,BitMap);
            end
            else begin
               if MapDraw.ValidDEMonMap then MapDraw.SetUpElevationColorTable;
               for y := 0 to pred(MapDraw.MapYSize) do begin
                  P0 := BitMap.ScanLine[y];
                  for x := 0 to pred(MapDraw.MapXSize) do begin
                     if ScreenLocations^[x,y].Lat > -99 then begin
                        Lat := ScreenLocations^[x,y].Lat;
                        Long := ScreenLocations^[x,y].Long - i * AngleShift;
                        if Long < -180 then Long := Long + 360;

                        if (MapDraw.ValidDEMonMap) then begin
                           DEMGlb[MapDraw.DEMonMap].LatLongDegreeToDEMGridInteger(Lat,Long,xgrid,Ygrid);
                           if (Sender = DEMreflectance1) then P0[x] := DEMGlb[MapDraw.DEMonMap].RGBReflectanceColor(MapDraw.MapType,XGrid,YGrid)
                           else P0[x] := MapDraw.QuickElevColor(XGrid,YGrid);
                        end
                        else begin
                           {$IfDef ExSat}
                           {$Else}
                              SatImage[SatJustLoaded].SelectionMap.MapDraw.LatLongDegreeToScreen(Lat,Long,xi,yi);
                              if (yi < SatBMP.Height) then begin
                                 P1 := SatBMP.ScanLine[yi];
                                 P0[x] := P1[xi];
                              end;
                          {$EndIf}
                         end;
                     end;
                  end;
               end;
               if (Steps > 0) and MDDef.RotatingEarthOutlines then MapDraw.SmartWorldOutline(Bitmap);
            end;

            if (Steps = 0) then MapDraw.SaveLayerBitmap(Bitmap,MapDraw.BaseMapFName);

            MapDraw.DrawMapOverlays(Bitmap);
            Image1.Picture.Graphic := Bitmap;

            if (Steps > 0) then begin
               FName := 'earth_rotation_' + intToStr(360 - i * AngleShift) + MovieFileExt;
               PetImage.SaveBitmap(Bitmap,DEMdefs.MovieDir + fName);
               MovieList.Add(fName);
            end;
            Bitmap.Free;
         end;
         EndProgress;
         ShowSatProgress := true;

         Dispose(ScreenLocations);
         MaskBitmap.Free;
         if (SatBMP <> Nil) then FreeAndNil(SatBMP);
         if (Sender = Predictedseafloorages1) or (Sender = Globaltopography1) then begin
            CloseSingleDEM(MapDraw.DEMonMap);
            MapDraw.DEMonMap := 0;
            MapDraw.DEMMap
         end;

         {$IfDef ExMovies}
         {$Else}
         if (Steps > 0) then begin
            {$IfDef RecordGlobeRotation} WriteLineToDebugFile('Start Movie'); {$EndIf}
            fName := DEMdefs.MovieDir + 'earth_rotation.mov';
            MovieList.SaveToFile(fName);

            //PetImage.MakeMovie(fName);
            CreateNewMovie(fName);
            MovieList.Free;
         end;
         {$EndIf}
      end
      else begin
         MessageToContinue('Invalid projection (use orthographic, stereographic, Lambert azimuthal equal area, or conical');
      end;
   end;
{$EndIf}
end;


procedure TMapForm.ZigDistButtonClick(Sender: TObject);
begin
    ChangeDEMNowDoing(FirstZigDistance);
end;


procedure TMapForm.ZoomBitBtnClick(Sender: TObject);
var
   BlowUp : int32;
begin
    BlowUp := round(100 * MapDraw.MapZoomFactor);
    ReadDefault('Blowup factor',BlowUp);
    ResizeByPercentage(Blowup);
end;


procedure TMapForm.Zoomfullresolution1Click(Sender: TObject);
begin
   ZoomIn1Click(Sender);
end;

procedure TMapForm.ZoomIn1Click(Sender: TObject);
var
   lat,Long : float64;
begin
   if LockMaps then begin
      MessageToContinue('Maps locked for digitizing');
   end
   else begin
      MapDraw.ScreenToLatLongDegree(lastx,LastY,Lat,Long);
      if (Sender = NoZoom1) then CenterMapOnLatLong(Lat,Long,hzNoZoom);
      if (Sender = ZoomIn1) then CenterMapOnLatLong(Lat,Long,hzZoomIn);
      if (Sender = ZoomOut2) then CenterMapOnLatLong(Lat,Long,hzZoomOut);
      if (Sender = Zoomfullresolution1) then CenterMapOnLatLong(Lat,Long,hzFullZoom);
      FullMapSpeedButton.Enabled := true;
   end;
end;


procedure TMapForm.ZoomIn2Click(Sender: TObject);
begin
   Blowup2Click(Sender);
end;

procedure TMapForm.Zoomin3Click(Sender: TObject);
begin
   CenterMapOnLatLong(RightClickLat,RightClickLong,hzZoomIn);
end;

procedure TMapForm.NoZoom1Click(Sender: TObject);
begin
   ZoomIn1Click(Sender);
end;

procedure TMapForm.ZoomOut2Click(Sender: TObject);
begin
   ZoomIn1Click(Sender);
end;


procedure TMapForm.Zoomout3Click(Sender: TObject);
begin
   CenterMapOnLatLong(RightClickLat,RightClickLong,hzZoomOut);
end;

procedure TMapForm.Recenter2Click(Sender: TObject);
begin
   MapDraw.DeleteMapSavedLayers;
   if (MapDraw.VectorIndex <> 0) and (MapDraw.PrimMapProj.PName in [OldStereographic,OrthoProj]) then begin
      MapDraw.PrimMapProj.Lat0 := RightClickLat * DegToRad;
      MapDraw.PrimMapProj.Long0 := RightClickLong * DegToRad;
      MapDraw.PrimMapProj.GetProjectParameters;
      DrawColoredMap1Click(Nil);
   end
   else PopupMenu10.PopUp(Mouse.CursorPos.X,Mouse.CursorPos.Y);
end;


procedure TMapForm.N0codes1Click(Sender: TObject);
var
   x,y : integer;
   z,ZeroTolerance : float32;
begin
   ZeroTolerance := 0.01;
   ReadDefault('Zero tolerance',ZeroTolerance);
   ZeroTolerance := abs(ZeroTolerance);
   ShowHourglassCursor;
   for x := 0 to pred(DEMGlb[MapDraw.DEMonMap].DEMheader.NumCol) do begin
      for y := 0 to pred(DEMGlb[MapDraw.DEMonMap].DEMheader.NumRow) do begin
         if DEMGlb[MapDraw.DEMonMap].GetElevMetersOnGrid(x,y,z) then begin
            if (z >= ZeroTolerance) then DEMGlb[MapDraw.DEMonMap].SetGridElevation(x,y,1)
            else if (z <= -ZeroTolerance) then DEMGlb[MapDraw.DEMonMap].SetGridElevation(x,y,-1)
            else DEMGlb[MapDraw.DEMonMap].SetGridElevation(x,y,0);
         end;
      end;
   end;
   ThinDEM1Click(Integer161);
   Integercode1Click(Sender);
end;


procedure TMapForm.N11view1Click(Sender: TObject);
begin
   ResizeByPercentage(100);
end;

procedure TMapForm.ResampleDEMgridbyaveraging1Click(Sender: TObject);
begin
   DEMGlb[MapDraw.DEMonMap].ThinThisDEM('',0,true);
   //AverageResampleThisDEM(NewDEM,0);
end;

(*
procedure TMapForm.RescaleallDEMsforSSIM1Click(Sender: TObject);
var
   DEM : integer;
   Min,Max : float32;
   fName : PathStr;
begin
   Min := 99e39;
   Max := -99e39;
   for DEM := 1 to MaxDEMDataSets do begin
      //get elevation range
      if ValidDEM(DEM) then begin
         if DEMGlb[DEM].DEMheader.MaxElev > Max then Max := DEMGlb[DEM].DEMheader.MaxElev;
         if DEMGlb[DEM].DEMheader.MinElev < Min then Min := DEMGlb[DEM].DEMheader.MinElev;
      end;
   end;
   for DEM := 1 to MaxDEMDataSets do begin
      //rescale to 0-1 range
      if ValidDEM(DEM) then begin
         DEMGlb[DEM].AddConstantToGrid(-Min);
         DEMGlb[DEM].MultiplyGridByConstant(1/(Max-Min));
         fName := 'c:\temp\' + DEMGlb[DEM].AreaName + '_ssim.tif';
         DEMGlb[DEM].SaveGridSubsetGeotiff(MapDraw.MapAreaDEMGridLimits,fName);
      end;
   end;
end;
*)

procedure TMapForm.Resoremenus1Click(Sender: TObject);
begin
   MDDef.ShowMenus := true;
   CheckProperTix;
end;

procedure TMapForm.Interactiveadjusment1Click(Sender: TObject);
begin
   PickMapElevationRangeForColoring(self);
end;

procedure TMapForm.Interpolateacrossholessmooth1Click(Sender: TObject);
begin
   DEMGlb[MapDraw.DEMonMap].InterpolateAcrossHoles(true);
   RespondToChangedDEM;
end;

procedure TMapForm.Matchimage1Click(Sender: TObject);
var
   fName : PathStr;
   Bitmap,Bitmap2 : tMyBitmap;
begin
   fName := '';
   PetImage.GetGraphicsFileName('Matching to map',fName);
   if (fName <> '') then begin
      CopyImageToBitmap(Image1,Bitmap);
      Bitmap2 := Petimage.LoadBitmapFromFile(fName);
      AlphaMatchBitmaps(Bitmap,Bitmap2);
      Bitmap2.Free;
      Bitmap.Free;
   end;
end;

procedure TMapForm.MatchMapToThisOne(var DrapeMap : tMapForm);
(*
//changed 10/22/2023
var
   Lat1,Long1,Lat2,Long2 : float64;
   xg1,yg1,xg2,yg2 : float32;
*)
begin
   MatchAnotherMapThisCoverageArea(Self,DrapeMap);
(*
   MapDraw.ScreenToLatLongDegree(0,0,Lat1,Long1);
   MapDraw.ScreenToLatLongDegree(MapDraw.MapXSize,MapDraw.MapYSize,Lat2,Long2);
   if (DrapeMap.MapDraw.DEMMap ) then begin
      DEMGlb[DrapeMap.MapDraw.DEMOnMap].LatLongDegreetoDEMGrid(Lat1,Long1,xg1,yg1);
      DEMGlb[DrapeMap.MapDraw.DEMOnMap].LatLongDegreetoDEMGrid(Lat2,Long2,xg2,yg2);
      DrapeMap.FullMapSpeedButton.Enabled := true;
   end
   else begin
      {$IfDef ExSat}
      {$Else}
        SatImage[DrapeMap.MapDraw.SatOnMap].LatLongDegreeToSatGrid(SatImage[MapDraw.SatOnMap].BandForSize,Lat1,Long1,xg1,yg1);
        SatImage[DrapeMap.MapDraw.SatOnMap].LatLongDegreeToSatGrid(SatImage[MapDraw.SatOnMap].BandForSize,Lat2,Long2,xg2,yg2);
        DrapeMap.MapDraw.MapCorners.BoundBoxDataGrid.xmin := round(xg1);
        DrapeMap.MapDraw.MapCorners.BoundBoxDataGrid.ymin := round(yg1);
        //DrapeMap.MapDraw.SatView.ColsDisplayed := succ(round(xg2-xg1));
        //DrapeMap.MapDraw.SatView.RowsDisplayed := succ(round(yg2-yg1));
      {$EndIf}
   end;

   DrapeMap.MapDraw.MapXSize := Self.MapDraw.MapXSize;
   DrapeMap.MapDraw.MapYSize := Self.MapDraw.MapYSize;
   SizeIsCorrectThankYou := true;
   DrapeMap.ClientWidth := Self.ClientWidth;
   DrapeMap.ClientHeight := Self.ClientHeight;
   SizeIsCorrectThankYou := false;
   DrapeMap.DoCompleteMapRedraw;
*)
end;


procedure TMapForm.Matchothermaps2Click(Sender: TObject);
begin
   MatchMapsPopupMenu.PopUp(Mouse.CursorPos.X,Mouse.CursorPos.Y);
end;

procedure TMapForm.Matchothermaps3Click(Sender: TObject);
begin
   MatchMapsPopupMenu.PopUp(Mouse.CursorPos.X,Mouse.CursorPos.Y);
end;

procedure TMapForm.MatchWKTprojection1Click(Sender: TObject);
begin
   CreateGridToMatchMap(cgWKT);
end;


procedure TMapForm.Mergecolorscene1Click(Sender: TObject);
{$IfDef ExSat}
begin
{$Else}
var
   n1 : integer;
begin
   {$IfDef RecordMergecolorscene} WriteLineToDebugFile('TMapForm.Mergecolorscene1Click in'); {$EndIf}
   n1 := PickADifferentMap('map to overlay on ' + Self.Caption,Self.Caption);
   SliderDrapeMap := WMDEM.MDIChildren[n1] as tMapForm;
   {$IfDef RecordMergecolorscene} WriteLineToDebugFile('merge=' + DrapeMap.Caption); {$EndIf}

   if (Self.MapDraw.BasicProjection <> SliderDrapeMap.MapDraw.BasicProjection) then begin
      MessageToContinue(IncMapTypes);
      exit;
   end;

   MatchMapToThisOne(SliderDrapeMap);

   {$IfDef RecordMergecolorscene} WriteLineToDebugFile('TMapForm.Mergecolorscene1Click set up'); {$EndIf}

   if (Sender = Variableopaquemerge1) then begin
      {$IfDef RecordMergecolorscene} WriteLineToDebugFile('Sender = Variableopaquemerge1'); {$EndIf}
      CopyImageToBitmap(SliderDrapeMap.Image1,OverlayOpaqueBMP);
      SetUpTopForm;
      Caption := 'Opacity 0= ' + RemoveUnderscores(Self.MapDraw.BaseTitle) + ' ----- Opacity 100= ' + RemoveUnderscores(SliderDrapeMap.MapDraw.BaseTitle);
   end;
{$EndIf}
end;


procedure TMapForm.Mergegridstakelowestvalue1Click(Sender: TObject);
var
   i,MergeDEM,Col,Row : integer;
   DefFilter : byte;
   FilesWanted : tStringList;
   fName : PathStr;
   z,z2 : float32;
begin
   DefFilter := 1;
   FilesWanted := tStringList.Create;
   FilesWanted.Add(ExtractFilePath(LastDEMName));
   if Petmar.GetMultipleFiles('DEMs to merge, keep low value','DEMs|*.*',FilesWanted,DefFilter) then begin
      for i  := 0 to pred(FilesWanted.Count) do begin
         fName := FilesWanted.Strings[i];
         if LoadNewDEM(MergeDEM,fName,(FilesWanted.Count = 1),'','',false) and ((FilesWanted.Count > 1) or AnswerIsYes('Use this DEM')) then begin
            if DEMGlb[MapDraw.DEMonMap].SecondGridIdentical(MergeDEM) then begin
               for Col := 0 to pred(DEMGlb[MapDraw.DEMonMap].DEMheader.NumCol) do begin
                   for Row := 0 to pred(DEMGlb[MapDraw.DEMonMap].DEMheader.NumCol) do begin
                       if DEMGlb[MergeDEM].GetElevMetersOnGrid(Col,Row,z) then begin
                          if (not DEMGlb[MapDraw.DEMonMap].GetElevMetersOnGrid(Col,Row,z2)) or (z < z2) then
                            DEMGlb[MapDraw.DEMonMap].SetGridElevation(Col,Row,z);
                       end;
                   end;
               end;
            end
            else MessageToContinue(DEMGlb[MergeDEM].AreaName + ' not compatible');
            CloseSingleDEM(MergeDEM);
         end;
      end;
   end;
   FilesWanted.Free;
   RespondToChangedDEM;
end;


procedure TMapForm.MergemultipleCSVTXTfiles1Click(Sender: TObject);
begin
    MergeMultipleCSVorTextFiles(Self);
end;



procedure tMapForm.MergeAnotherDEMreflectance(DEM : integer; MakeSticky : boolean = false; Opacity : byte = 40);
var
   Bitmap1,Bitmap2 : tMyBitmap;
begin
   if (not ValidDEM(DEM)) then begin
      if GetDEM(DEM,true, 'DEM map to merge on ' + Self.Caption) then begin
         if not (DEMGlb[DEM].SelectionMap <> Nil) then begin
            MessageToContinue('DEM must have open map');
            exit;
         end;
         if (Self.MapDraw.BasicProjection <> DEMGlb[DEM].SelectionMap.MapDraw.BasicProjection) then begin
            MessageToContinue(IncMapTypes);
            exit;
         end;
      end
      else exit;
   end;
   if MakeSticky then SavedMergeReflectanceDEM := DEM;

    if (DEMGlb[DEM].SelectionMap.MapDraw.MapType <> mtGrayReflect) then begin
       DEMGlb[DEM].SelectionMap.MapDraw.MapType := mtGrayReflect;
       DEMGlb[DEM].SelectionMap.DoBaseMapRedraw;
    end;

    MatchAnotherMapThisCoverageArea(Self,DEMGlb[DEM].SelectionMap);
    MatchAnotherMapThisPixelSize(Self,DEMGlb[DEM].SelectionMap);

    Bitmap1 := tMyBitmap.Create;
    Bitmap1.LoadFromFile(DEMGlb[DEM].SelectionMap.MapDraw.BaseMapFName);
    Bitmap2 := tMyBitmap.Create;
    Bitmap2.LoadFromFile(MapDraw.FullMapfName);

    DrawAndDeleteOverlay(Bitmap1,Bitmap2,Opacity);
    Image1.Picture.Graphic := Bitmap1;
    Bitmap1.SaveToFile(MapDraw.FullMapfName);
    Bitmap1.Free;
    CheckThatLegendsAreOnTop;
end;



procedure tMapForm.IHSmergeOntoMap(var Bitmap2 : tMyBitmap; IHSValues : boolean = true; Opacity : byte = 100);
var
   Bitmap1 : tMyBitmap;
begin
   {$IfDef RecordGeomorphFilter} Bitmap.SaveToFile(MDTempDir + 'mask_it.bmp'); {$EndIf}
    CopyImageToBitmap(Self.Image1,Bitmap1);
    if IHSValues then begin
       IHSMergePurgeBitmaps(Bitmap1,Bitmap2);
    end
    else begin
       DrawAndDeleteOverlay(Bitmap1,Bitmap2,Opacity);
    end;
    Image1.Picture.Graphic := Bitmap1;
    Bitmap1.Free;
    Forceredrawlegendsscalebars1Click(Nil);
end;


procedure TMapForm.Clearmap1Click(Sender: TObject);
var
   Bitmap : tMyBitmap;
begin
   CloneImageToBitmap(Image1,Bitmap);
   Image1.Picture.Graphic := Bitmap;
   Bitmap.Free;
end;


procedure TMapForm.Clearoverlays1Click(Sender: TObject);
begin
    MapDraw.MapOverlays.ovVectorFiles.Clear;
    MapDraw.DeleteSingleMapLayer(MapDraw.VectorOverlayfName);
    DoFastMapRedraw;
end;

procedure TMapForm.Clearrangecircles1Click(Sender: TObject);
begin
   MapDraw.DeleteSingleMapLayer(MapDraw.RangeCirclesFName);
   DoFastMapRedraw;
end;

procedure TMapForm.Clearsecondgrid1Click(Sender: TObject);
begin
   MapDraw.DEM2onMap := 0;
   AddOrSubtractOverlay(Self,ovoSecondGrid, false);
   MapDraw.DeleteSingleMapLayer(MapDraw.SecondGridfName);
   DoFastMapRedraw;
   CheckProperTix;
end;

procedure TMapForm.ClearWMSmap1Click(Sender: TObject);
begin
   {$IfDef ExWMS}
   {$Else}
      MapDraw.LastWMSService := '';
      MapDraw.DeleteSingleMapLayer(MapDraw.WMSLayerfName);
      DoFastMapRedraw;
   {$EndIf}
end;



procedure TMapForm.Allvalidpixels1Click(Sender: TObject);
var
   x,y, Pixels,PixelsAbove,PixelsBelow : integer;
   RowVolumeAbove,TotalVolumeAbove,
   RowVolumeBelow,TotalVolumeBelow,
   //TotalVolumeBelow,TotalVolumeAbove,
   RowArea,TotalArea,PixelArea : float64;
   z,BaseZ : float32;
begin
   BaseZ := 0;
   ReadDefault('Base elevation (m)',BaseZ);
   with DEMGlb[MapDraw.DEMonMap] do begin
      TotalArea := 0;
      Pixels := 0;
      PixelsAbove := 0;
      PixelsBelow := 0;
      TotalVolumeBelow := 0;
      TotalVolumeAbove := 0;
      for y := 0 to pred(DEMheader.NumRow) do begin
         RowArea := 0;
         RowVolumeAbove := 0;
         RowVolumeBelow := 0;
         PixelArea := AverageYSpace * XSpaceByDEMrow^[y];
         for x := 0 to pred(DEMheader.NumCol) do begin
            if GetElevMeters(x,y,z) then begin
               RowArea := RowArea + PixelArea;
               Inc(Pixels);
               z := z - BaseZ;
               if Z >= 0 then begin
                  inc(PixelsAbove);
                  RowVolumeAbove := RowVolumeAbove + z * PixelArea;
               end
               else begin
                  inc(PixelsBelow);
                  RowVolumeBelow := RowVolumeBelow + z * PixelArea;
               end;
            end;
         end;
         TotalArea := TotalArea + RowArea * 0.001 * 0.001;
         TotalVolumeBelow := TotalVolumeBelow + RowVolumeBelow * 0.001 * 0.001 * 0.001;
         TotalVolumeAbove := TotalVolumeAbove + RowVolumeAbove * 0.001 * 0.001 * 0.001;
      end;
   end;
   MessageToContinue('Total Area: ' + RealToString(TotalArea,-18,-4) + ' km²' + '   Pixels: ' + IntToStr(Pixels)  + MessLineBreak +
       'Volume above ' + RealToString(BaseZ,-12,-4) + ' m  ' + RealToString(TotalVolumeAbove,-18,-6) + ' km³' + '   Pixels: ' + IntToStr(PixelsAbove)  + MessLineBreak   +
       'Volume below ' + RealToString(BaseZ,-12,-4) + ' m  ' + RealToString(TotalVolumeBelow,-18,-6) + ' km³' + '   Pixels: ' + IntToStr(PixelsBelow)   + MessLineBreak  );
end;


procedure TMapForm.Allvalidtosinglevalue1Click(Sender: TObject);
begin
   Rangetosinglevalue1Click(Sender);
end;

procedure TMapForm.Alolthreenormalizations1Click(Sender: TObject);
var
   TRI_30m,TRI_ew,TRI_ns,TRI_none,TRI_interpolate : integer;
begin
   {$IfDef ExGeostats}
   {$Else}
      TRI_ew := MakeTRIGrid(MapDraw.DEMonMap,nmEastWest,true);
      TRI_ns := MakeTRIGrid(MapDraw.DEMonMap,nmNorthSouth,true);
      TRI_none := MakeTRIGrid(MapDraw.DEMonMap,nmNone,true);
      TRI_interpolate := MakeTRIGrid(MapDraw.DEMonMap,nmInterpolate,true);
      TRI_30m := MakeTRIGrid(MapDraw.DEMonMap,nm30m,true);
      MakeTRIGrid(MapDraw.DEMonMap,nmTRIK,true);
      MakeTRIGrid(MapDraw.DEMonMap,nmRRI,true);
   {$EndIf}
end;

procedure TMapForm.Copytoclipboard1Click(Sender: TObject);
begin
   ClipboardSpeedButtonClick(Sender);
end;

procedure TMapForm.CopyVegprofiletoclipboard1Click(Sender: TObject);
begin
   AssignImageToClipBoard(VegGraph.Image1);
end;

procedure TMapForm.Counties1Click(Sender: TObject);
begin
   LoadDataBaseFile(CountyGISFileName);
end;

procedure TMapForm.AnnotateSpeedButton1Click(Sender: TObject);
begin
   GeologySymbols1.Visible := MDDef.ShowGeologyOptions;
   Digitizecontours1.Visible := MapDraw.ValidDEMonMap;
   PopupMenu12.PopUp(Mouse.CursorPos.X,Mouse.CursorPos.Y);
end;

procedure TMapForm.Annualsunrisesunset1Click(Sender: TObject);
begin
    Sun_position.AnnualSunRiseGeometry(Self,RightClickLat,RightClickLong);
end;

procedure TMapForm.Annualsunrisesunset2Click(Sender: TObject);
begin
    {$IfDef ExGeography}
    {$Else}
       SunRiseSunSet(RightClickLat,RightClickLong,false,true);
       Sun_position.AnnualSunRiseGeometry(Self,RightClickLat,RightClickLong,true);
    {$EndIf}
end;

procedure TMapForm.AquastatClimate1Click(Sender: TObject);
begin
   {$IfDef ExGeography}
   {$Else}
      ClimatePopupMenu18.Popup(Mouse.CursorPos.X,Mouse.CursorPos.Y);
   {$EndIf}
end;


procedure TMapForm.Militaryicons1Click(Sender: TObject);
{$IfDef ExMilIcons}
begin
{$Else}
var
  fName : PathStr;
begin
   FName := ProjectDir + 'MILICONS\';
   if GetFileNameDefaultExt('military icons',DBNameMask,FName,true) then begin
      MapDraw.MapOverlays.ovVectorFiles.Add(FName);
      if FileExists(fName) then DoFastMapRedraw
      else CreateMilIconsTable(fName);
      if (SavedMapImage <> Nil) then FreeAndNil(SavedMapImage);
      CopyImageToBitmap(Image1,SavedMapImage);
      MilIconsForm := TMilIconsForm.Create(Application);
      MilIconsForm.BaseMap := Self;
      MilIconsForm.SavingName := fName;
      MilIconsForm.Show;
      ChangeDEMNowDoing(AddMilIcon);
   end;
{$EndIf}
end;

procedure TMapForm.Annotatemap1Click(Sender: TObject);
begin
   AnnotateSpeedButton1Click(Sender);
end;

procedure TMapForm.Blowupmapoverviewdetail1Click(Sender: TObject);
begin
   Subset1Click(Sender);
end;



procedure TMapForm.Whereiskeyboard1Click(Sender: TObject);
var
   lat,Long : float64;
   xp,yp   : integer;
   TStr,TStr2 : shortstring;
begin
   MapDraw.ScreenToLatLongDegree(MapDraw.MapXSize div 2, MapDraw.MapYSize div 2, Lat,Long);
   repeat
       GetLatLongDefault(MapDraw.PrimMapProj,'Location to plot',Lat,Long);
       MapDraw.LatLongDegreeToScreen(Lat,Long,xp,yp);
       ScreenSymbol(Image1.Canvas,xp,yp,Box,3,ClaRed);
       if not MapDraw.OnScreen(xp,yp) then begin
          TStr := '';
          TStr2 := '';
          if (XP < 0) then TStr := 'W';
          if (XP > ClientWidth) then TStr := 'E';
          if (YP < 0) then TStr2 := 'N';
          if (YP > ClientHeight) then TStr2 := 'S';
          MessageToContinue('Point offscreen to the ' + TStr2 + TStr);
       end;
    until not AnswerIsYes('Another location');
end;

procedure TMapForm.Whiteboxfillholes2Click(Sender: TObject);
begin
   {$IfDef NoExternalPrograms}
   {$Else}
      WBT_GridFillMissingData(GeotiffDEMNameOfMap,DEMGlb[MapDraw.DEMonMap].DEMheader.ElevUnits);
   {$EndIf}
end;

procedure TMapForm.WhiteboxGeomorphons1Click(Sender: TObject);
begin
   WBT_Geomorphons(GeotiffDEMNameOfMap);
end;

procedure TMapForm.WhiteBoxmultiscaleroughness1Click(Sender: TObject);
begin
   {$IfDef NoExternalPrograms}
   {$Else}
      WBT_MultiscaleRoughness(GeotiffDEMNameOfMap);
   {$EndIf}
end;

procedure TMapForm.Whieboxflowaccumulationlog1Click(Sender: TObject);
begin
   WBT_FlowAccumulation(True,True,True,GeotiffDEMNameOfMap);
end;

procedure TMapForm.Whitebox1Click(Sender: TObject);
const
   FilterSize : integer = 5;
begin
   ReadDefault('Filter Size (pixels)',FilterSize);
   WBT_AvgNormVectAngDev(GeotiffDEMNameOfMap,FilterSize);
end;


procedure TMapForm.Whiteboxaspectmap1Click(Sender: TObject);
begin
   {$IfDef NoExternalPrograms}
   {$Else}
      WBT_AspectMap(GeotiffDEMNameOfMap);
   {$EndIf}
end;

procedure TMapForm.WhiteboxCircularVarianceOfAspect1Click(Sender: TObject);
const
   FilterSize : integer = 5;
begin
   ReadDefault('Filter Size (pixels)',FilterSize);
   WBT_CircularVarianceOfAspect(GeotiffDEMNameOfMap,FilterSize);
end;

procedure TMapForm.Whiteboxdrainagebasins1Click(Sender: TObject);
begin
   WBT_DrainageBasins(GeotiffDEMNameOfMap);
end;

procedure TMapForm.Whiteboxfillholes1Click(Sender: TObject);
begin
   {$IfDef NoExternalPrograms}
   {$Else}
      WBT_GridFillMissingData(GeotiffDEMNameOfMap,DEMGlb[MapDraw.DEMonMap].DEMheader.ElevUnits);
   {$EndIf}
end;

procedure TMapForm.WhiteboxPennockClassification1Click(Sender: TObject);
begin
   WBT_PennockLandformClass(GeotiffDEMNameOfMap,true);
end;

procedure TMapForm.Whiteboxslopemape1Click(Sender: TObject);
var
   NewGrid : integer;
begin
   {$IfDef NoExternalPrograms}
   {$Else}
       NewGrid := WBT_SlopeMap(GeotiffDEMNameOfMap);
       MatchAnotherDEMMap(NewGrid,MapDraw.DEMonMap);
   {$EndIf}
end;

procedure TMapForm.WhiteboxTRI1Click(Sender: TObject);
begin
   WBT_TRI(GeotiffDEMNameOfMap);
end;

procedure TMapForm.Whiteboxwetnessindex1Click(Sender: TObject);
begin
   WBT_WetnessIndex(True,True,GeotiffDEMNameOfMap);
end;

procedure TMapForm.Winwcontestmaps1Click(Sender: TObject);
var
   TheList,Findings : tStringList;
   ch : ANSIchar;
   i : integer;
   Output,fName : PathStr;

   procedure DoAMap(DEM : integer);
   begin
     if ValidDEM(DEM) then begin
         if (DEM = 1) then DEMGlb[1].SelectionMap.MapDraw.BaseTitle := 'Reference DEM'
         else DEMGlb[DEM].SelectionMap.MapDraw.BaseTitle := 'DEM ' + ch;
         DEMGlb[DEM].SelectionMap.DoCompleteMapRedraw;
         with DEMGlb[DEM].SelectionMap do begin
             Image1.Canvas.Brush.Style := bsClear;
             Image1.Canvas.Pen.Width := 3;
             Image1.Canvas.Pen.Color := clBlack;
             Image1.Canvas.Rectangle(0,0,pred(Image1.Width),pred(Image1.Height));
         end;
         fName := Output + DEMGlb[DEM].SelectionMap.MapDraw.BaseTitle + '--' + DEMGlb[DEM].AreaName + '.bmp';
         PetImage.SaveImageAsBMP(DEMGlb[DEM].SelectionMap.Image1,fName);
         Findings.Add(fName);
     end;
   end;


begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('TMapForm.Winwcontestmaps1Click in'); {$EndIf}
   DEMGlb[1].SelectionMap.MatchThiscoverageareaandsamepixelsize1Click(Sender);
   DEMGlb[1].SelectionMap.SameElevationColors1Click(Sender);
   Findings := tStringList.Create;
   Output := 'c:\temp\';
   TheList := tStringList.Create;
   for ch := 'A' to 'F' do TheList.Add('DEM ' + ch);
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('In order:'); writeStringListToDebugFile(TheList); {$EndIf}
   RandomizeStringList(TheList);
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Randomized:'); writeStringListToDebugFile(TheList); {$EndIf}
   for ch := 'A' to 'F' do begin
       i := TheList.IndexOf('DEM ' + ch);
       {$If Defined(RecordDEMIX)} WriteLineToDebugFile(ch + IntegerToString(i)); {$EndIf}
       DoAMap(i+2);
       if (ch = 'D') then DoAMap(1);
   end;
   Findings.SaveToFile(Output + 'hillshade_wine_contest.txt');
   {$If Defined(RecordDEMIX)} writeStringListToDebugFile(Findings); {$EndIf}
   MakeBigBitmap(Findings,'',Output + 'hillshade.png',3);
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('TMapForm.Winwcontestmaps1Click out'); {$EndIf}
end;



procedure TMapForm.FocalMechsButtonClick(Sender: TObject);
begin
   {$IfDef ExGeology}
   {$Else}
      SetBeachBallOptions(false);
      ChangeDEMNowDoing(EarthquakeFocalMech);
   {$EndIf}
end;

procedure TMapForm.Focalplaneanalysis1Click(Sender: TObject);
{$IfDef ExAdvancedGIS}
begin
{$Else}
var
   TerrainCategory : tTerrainCatDefinition;
begin
   {$IfDef RecordFocalPlanes} WriteLineToDebugFile('TMapForm.Focalplaneanalysis1Click in'); {$EndIf}
   DEMGlb[MapDraw.DEMonMap].InitializeTerrainCategory(TerrainCategory);
   {$IfDef RecordFocalPlanes} WriteLineToDebugFile('TMapForm.Focalplaneanalysis1Click mid'); {$EndIf}
   GetTerrainCategory(tcTwoAspects,Self,MapDraw.DEMonMap,TerrainCategory,false);
   {$IfDef RecordFocalPlanes} WriteLineToDebugFile('TMapForm.Focalplaneanalysis1Click out'); {$EndIf}
{$EndIf}
end;

procedure TMapForm.SetDEMwithmap1Click(Sender: TObject);
begin
  GetDEM(MapDraw.DEMonMap);
  CheckProperTix;
end;


procedure TMapForm.Pointslopealgorithms1Click(Sender: TObject);
var
   xDEMg1,yDEMg1,xSATg1,ySATg1 : float32;
begin
   CheckThisPoint(LastX,LastY,xDEMg1,yDEMg1,xSATg1,ySATg1,CheckReasonable);
   DEMGlb[MapDraw.DEMonMap].SlopeMethodsReport(round(xDEMg1), round(yDEMg1));
end;

procedure TMapForm.IDSpeedButtonClick(Sender: TObject);
begin
   ChangeDEMNowDoing(IDDataBaseAll);
   DBEditting := 0;
end;

procedure TMapForm.MapLibSpeedButtonClick(Sender: TObject);
begin
   ChangeDEMNowDoing(OpenMapsFromLibrary);
end;


procedure TMapForm.Keyboard1Click(Sender: TObject);
var
   xutm,yutm : float64;
begin
   DEMGlb[MapDraw.DEMonMap].VerifyDefaultPosition('',xdemg1,ydemg1,xutm,yutm);
   Close;
end;

procedure TMapForm.EditWeaponsFan1Click(Sender: TObject);
begin
   {$IfDef ExViewshed}
   {$Else}
      EditAWeaponsFan(Sender);
   {$EndIf}
end;

procedure TMapForm.EGM1996toEGM20081Click(Sender: TObject);
begin
   VerticalDatumShift(MapDraw.DEMonMap,vdEGM96toEGM2008);
end;

procedure TMapForm.EGM2008toWGS84ellipsoid1Click(Sender: TObject);
begin
    VerticalDatumShift(MapDraw.DEMonMap,vdEGM2008toWGS84);
end;

procedure TMapForm.Editfan1Click(Sender: TObject);
begin
   {$IfDef ExViewshed}
   {$Else}
      EditAWeaponsFan(Sender);
   {$EndIf}
end;

procedure TMapForm.Editfans1Click(Sender: TObject);
begin
    EditFan1Click(Sender);
end;

procedure TMapForm.Editgazetteersymbology1Click(Sender: TObject);
var
   db : integer;
begin
   OpenNumberedGISDataBase(db,GazOptFName,true);
   GISdb[db].dbtablef.CheckBox1.Checked := true;
end;

procedure TMapForm.Loadweaponsfans1Click(Sender: TObject);
var
   fName : PathStr;
begin
    {$IfDef ExViewshed}
    {$Else}
       if (LastFanTable = '') then fName := ProjectDir + 'FANS\'
       else fName := LastFanTable;
       if not GetFileFromDirectory('saved fans',DefaultDBMask,fName) then exit;
       LastFanTable := fName;
       AddTableWeaponsFan(fName);
       EditAWeaponsFan(Sender);
       CheckProperTix;
    {$EndIf}
end;

{$IfDef ExViewshed}
{$Else}
procedure TMapForm.AddTableWeaponsFan(fName : PathStr);
begin
   if (FName = '') or (not FileExists(fName)) then begin
      if (fName = '') then fName := ProjectDir + 'FANS\';
      if not GetFileFromDirectory('saved fans',DefaultDBMask,fName) then exit;
      LastFanTable := fName;
   end;
   {$IfDef RecordFan} WriteLineToDebugFile('TMapForm.Loadweaponsfans1Click ' + fName); {$EndIf}
   CloseAndNilNumberedDB(MapDraw.CurrentFansTable);

   OpenNumberedGISDataBase(MapDraw.CurrentFansTable,fName,false,false,Self);

   if GISdb[MapDraw.CurrentFansTable].MyData.FieldExists('OFFSETA') then begin
      {$IfDef RecordFan} WriteLineToDebugFile('Rename ArcGIS verion'); {$EndIf}
      GISdb[MapDraw.CurrentFansTable].RenameField('OFFSETA','SENSOR_UP');
      GISdb[MapDraw.CurrentFansTable].RenameField('OFFSETB','TARGET_UP');
      GISdb[MapDraw.CurrentFansTable].RenameField('RADIUS1','MIN_RNG');
      GISdb[MapDraw.CurrentFansTable].RenameField('RADIUS2','SENSOR_RNG');
      GISdb[MapDraw.CurrentFansTable].RenameField('AZIMUTH1','LEFT_AZ');
      GISdb[MapDraw.CurrentFansTable].RenameField('AZIMUTH2','RIGHT_AZ');
      GISdb[MapDraw.CurrentFansTable].RenameField('VERT1','MAX_VERT');
      GISdb[MapDraw.CurrentFansTable].RenameField('VERT2','MIN_VERT');
   end;
   GISdb[MapDraw.CurrentFansTable].MyData.ApplyFilter('USE=' + QuotedStr('Y'));

   AddOrSubtractOverlay(Self,ovoFans,true);
   {$IfDef RecordFan} WriteLineToDebugFile('Calling Force redraw'); {$EndIf}
   DoFastMapRedraw;
   {$IfDef RecordFan} WriteLineToDebugFile('Done'); {$EndIf}
end;

{$EndIf}

procedure TMapForm.EditGridButtonClick(Sender: TObject);
begin
   DoFastMapRedraw;
   ChangeDEMNowDoing(CornerEditBox);
end;


procedure TMapForm.Map2Click(Sender: TObject);
var
   sl : tStringList;
   t1 : shortstring;
begin
   sl := MapInformationString;
   if MapDraw.DEMMap then sl.Insert(0,DEMGlb[MapDraw.DEMonMap].AreaName);
   if ValidSatImage(MapDraw.SatOnMap) then sl.Insert(0,SatImage[MapDraw.SatonMap].SceneBaseName);

   t1 := MapDraw.PrimMapProj.GetProjectionName;
   sl.add('Primary: ' +  t1);
   if MapDraw.DEMMap then begin
      sl.add('------------------------------------');
      sl.Add('DEM bounding box geographic: ' + sfBoundBoxToString(DEMGlb[MapDraw.DEMonMap].DEMBoundBoxGeo,6));
      sl.Add('DEM bounding box Proj: ' + sfBoundBoxToString(DEMGlb[MapDraw.DEMonMap].DEMBoundBoxProjected,1));
   end;
   if ValidSatImage(MapDraw.SatOnMap) then begin
      sl.add('------------------------------------');
      sl.Add('Image bounding box geographic: ' + sfBoundBoxToString(SatImage[MapDraw.SatonMap].SatelliteBoundBoxGeo(1),6));
      sl.Add('Image bounding box Proj: ' + sfBoundBoxToString(SatImage[MapDraw.SatonMap].SatelliteBoundBoxProj(1),1));
   end;
   DisplayAndPurgeStringList(sl,'Map window corners and center');
end;


procedure TMapForm.Mapannotation1Click(Sender: TObject);
begin
   AnnotateSpeedButton1Click(Sender);
end;

procedure TMapForm.LabelLatLongdifference1Click(Sender: TObject);
begin
   LabelDatumShift := msLatLong;
end;

procedure TMapForm.LabelUTMdifference1Click(Sender: TObject);
begin
  LabelDatumShift := msUTM;
end;


procedure TMapForm.NoLabels1Click(Sender: TObject);
begin
   LabelDatumShift := msNone;
end;

procedure TMapForm.Nonormalization1Click(Sender: TObject);
begin
   {$IfDef ExGeostats}
   {$Else}
      MakeTRIGrid(MapDraw.DEMonMap,nmNone,true);
   {$EndIf}
end;

procedure TMapForm.Labelboth1Click(Sender: TObject);
begin
   LabelDatumShift := msBoth;
end;


procedure TMapForm.NormalizedBurnIndex1Click(Sender: TObject);
begin
   NewSatWindow(nsbNBRNormalizedBurnIndex);
end;


procedure TMapForm.Normalizeddifferencepickbands1Click(Sender: TObject);
begin
   NewSatWindow(nsbPickEm);
end;


procedure TMapForm.Normalizeeastwest1Click(Sender: TObject);
begin
   {$IfDef ExGeostats}
   {$Else}
      MakeTRIGrid(MapDraw.DEMonMap,nmEastWest,true);
   {$EndIf}
end;

procedure TMapForm.Normalizenorthsouth1Click(Sender: TObject);
begin
   {$IfDef ExGeostats}
   {$Else}
      MakeTPIGrid(MapDraw.DEMonMap,nmNorthSouth,true);
   {$EndIf}
end;

procedure TMapForm.Normalizie1Click(Sender: TObject);
begin
   {$IfDef ExMultigrid}
   {$Else}
   MultiGridArray[MapDraw.MultiGridOnMap].NormalizeGrids;
   {$EndIf}
end;

procedure TMapForm.Northarrow1Click(Sender: TObject);
begin
   ChangeDEMNowDoing(PlotNorthArrow);
end;

procedure TMapForm.Horizonblocking1Click(Sender: TObject);
begin
   ChangeDEMNowDoing(FindBlockHorizon);
end;

procedure TMapForm.Mapmarginalia1Click(Sender: TObject);
begin
   {$IfDef ExMarginalia}
   {$Else}
      DEMMarginalia.MapMarginalia(Self);
   {$EndIf}
end;

procedure TMapForm.SpeedButton22Click(Sender: TObject);
begin
   N11view1Click(Sender);
end;


procedure TMapForm.SpeedButton3Click(Sender: TObject);
begin
   Level1stateprovince1Click(Sender);
end;

procedure TMapForm.OGL_speedbuttonClick(Sender: TObject);
begin
   {$IfDef ExFMX3D}
   {$Else}
      {$IfDef Record3d} WriteLineToDebugFile('TMapForm.OGL_speedbuttonClick in'); {$EndIf}
      MapDraw.MapCorners.BoundBoxUTM := MapDraw.GetBoundBoxUTM;
      {$IfDef Record3d} WriteLineToDebugFile('TMapForm.OGL_speedbuttonClick got bound box'); {$EndIf}
      if MDDef.OpenGLCleanOverlays then begin
         SaveBackupDefaults;
         NakedMapOptions;
         DoFastMapRedraw;
      end;
      {$IfDef Record3d} WriteLineToDebugFile('TMapForm.OGL_speedbuttonClick call Map3D'); {$EndIf}
      Map3d := MapTo3DView(Self.MapDraw);
      {$IfDef Record3d} WriteLineToDebugFile('TMapForm.OGL_speedbuttonClick back Map3D'); {$EndIf}
      if MDDef.OpenGLCleanOverlays then begin
         RestoreBackupDefaults;
         DoFastMapRedraw;
      end;
      {$IfDef Record3d} WriteLineToDebugFile('TMapForm.OGL_speedbuttonClick out'); {$EndIf}
   {$EndIf}
end;

procedure TMapForm.oInternationalDateLine1Click(Sender: TObject);
begin
   DEMGlb[MapDraw.DEMonMap].ShiftGlobalDEM(180);
   MapDraw.DefineNewDEMMap(MapDraw.DEMonMap,MapDraw.MapType);
   DoCompleteMapRedraw;
end;

procedure TMapForm.AsGEOTIFFscreenscalegrayscale1Click(Sender: TObject);
begin
   SaveMapasGEOTIFFFile('',true);
end;

procedure TMapForm.Aspect1Click(Sender: TObject);
begin
   MapDraw.MapType := mtDEMAspect;
   DrawColoredMap1Click(Nil);
end;

procedure TMapForm.ASTERGDEMtomatchthismap1Click(Sender: TObject);
begin
    NASADEMtomatchthismap1Click(Sender);
end;

procedure TMapForm.MDDEM1Click(Sender: TObject);
var
   FileName : PathStr;
begin
   {$IfDef RecordSave} WriteLineToDebugFile('TMapForm.MDDEM1Click'); {$EndIf}
   FileName := '';
   DEMGlb[MapDraw.DEMonMap].WriteNewFormatDEM(FileName);
   DoFastMapRedraw;
end;

procedure TMapForm.DTED1Click(Sender: TObject);
begin
   {$IfDef ExDTED}
   {$Else}
      DEMGlb[MapDraw.DEMonMap].SaveAsDTED(0,0);
      DoFastMapRedraw;
   {$EndIf}
end;

procedure TMapForm.DTMlastreturn1Click(Sender: TObject);
begin
   MDDef.VegOptionMap := voDTM;
   DoBaseMapRedraw;
end;

procedure TMapForm.ASCII1Click(Sender: TObject);
begin
   DEMGlb[MapDraw.DEMonMap].ExportASCII(0,0,Pred(DEMGlb[MapDraw.DEMonMap].DEMheader.NumCol),Pred(DEMGlb[MapDraw.DEMonMap].DEMheader.NumRow));
end;

procedure TMapForm.Excessiveslopes1Click(Sender: TObject);
{$IfDef ExVectorOverlay}
begin
{$Else}
var
    TerrainCategory : tTerrainCatDefinition;
begin
    DEMGlb[MapDraw.DEMonMap].InitializeTerrainCategory(TerrainCategory);
    GetTerrainCategory(tcSlopeOnly,Self,MapDraw.DEMonMap,TerrainCategory,DEMGlb[MapDraw.DEMonMap].ElevationDEM);
{$EndIf}
end;

procedure TMapForm.Setscale1Click(Sender: TObject);
var
  MapScaledForm : DEM_Map_scale.TMapScaledForm;
  OK,Prime : boolean;
  xg,yg : float32;
  xg1,yg1,xg2,yg2,xs,ys, lat,Long,xc,yc,LatC,LongC,Lat1,Long1,h,k : float64;
begin
  //with mapDraw do begin
     MapDraw.ScreenToLatLongDegree(lastx,LastY,Lat,Long);
      {$IfDef RecordMapDraw} WriteLineToDebugFile('TMapForm.Setscale1Click enter'); {$EndIf}
     MapScaledForm := DEM_Map_scale.TMapScaledForm.Create(Application);
     with MapScaledForm,MapDraw do begin
        WantLat := Lat;
        WantLong := Long;
        if  (VectorIndex <> 0) then with PrimMapProj do begin
           OldCenterLat := MapDraw.MapLatCent;
           OldCenterLong := MapDraw.MapLongCent;
        end;
        MapDatumConstants := MapDraw.PrimMapProj;
        ShowMapSize;
        ShowModal;
       Lat := WantLat;
       Long := WantLong;
       MapDraw.MapLatCent := Lat;
       MapDraw.MapLongCent := Long;
       MapXSize := WantMapXSize;
       MapYSize := WantMapYSize;
       ClientWidth := MapXSize;
       ClientHeight := MapYSize;
       MapDraw.ZeroTickInt;

       if (VectorIndex <> 0) then begin
         PrimMapProj.GetMapScaleFactor(Lat,Long,h,k ,Prime);
         PrimMapProj.ForwardProjectDegrees(Lat,Long,Xc,yc);
         PrimMapProj.InverseProjectDegrees(xc-0.5*xmeters*h,yc+0.5*ymeters*h,LatC,LongC);
         PrimMapProj.InverseProjectDegrees(xc+0.5*xmeters*h,yc-0.5*ymeters*h,Lat1,Long1);

         {$IfDef RecordMapDraw} WriteLineToDebugFile('TMapForm.Setscale1Click 4' + '  ' + LatLongDegreeToString(Lat1,LongC) +'  ' + LatLongDegreeToString(LatC,Long1)): {$EndIf}

          MapDraw.MaximizeLatLongMapCoverage(Lat1,LongC,LatC,Long1);
          {$IfDef RecordMapDraw} WriteLineToDebugFile('Resizing vector map'); {$EndIf}
          DrawColoredMap1Click(Nil);
       end
       else begin
          if DEMMap then with DEMGLB[DEMonMap] do begin
             LatLongDegreeToDEMGrid(Lat,Long,xg,yg);
             OK := GridInDataSet(xg,yg);
             xs := AverageXSpace;
             ys := AverageYSpace;
             yg1 := yg + MapScaledForm.YMeters / 2 / ys;
             yg2 := yg - MapScaledForm.YMeters / 2 / ys;
          {$IfDef ExSat}
          {$Else}
          end
          else with SatImage[SatOnMap] do begin
             LatLongDegreeToSatGrid(SatImage[MapDraw.SatOnMap].BandForSize,Lat,Long,xg,yg);
             OK := SatGridInDataSet(SatImage[MapDraw.SatOnMap].BandForSize,XG,YG);
             xs := xMetersPerPixel;
             ys := yMetersPerPixel;
             yg2 := yg + MapScaledForm.YMeters / 2 / ys;
             yg1 := yg - MapScaledForm.YMeters / 2 / ys;
          {$EndIf}
          end;
          if OK then begin
             xg1 := xg - MapScaledForm.XMeters / 2 / xs;
             xg2 := xg + MapScaledForm.XMeters / 2 / xs;
             RedrawMapForDataGrid(xg1,yg1,xg2,yg2,MapXSize,MapYSize);
          end
          else MessageToContinue('Requested location outside current map coverage');
       end;

     end;
 // end;
  MapScaledForm.Free;
 {$IfDef RecordMapDraw} WriteLineToDebugFile('TMapForm.Setscale1Click done'); {$EndIf}
end;

procedure TMapForm.SetsecondDEMonmap1Click(Sender: TObject);
begin
   MapDraw.AssignSecondDEM(0);
   CheckProperTix;
end;

procedure TMapForm.Setspecifiedvaluetomissingandresave1Click(Sender: TObject);
var
   mv : float64;
begin
   {$IfDef ExMultigrid}
   {$Else}
   Mv := -9999;
   ReadDefault('Value to set missing',mv);
   MultiGridArray[MapDraw.MultiGridOnMap].ZeroAsMissing(mv);
   {$EndIf}
end;

procedure TMapForm.Smallcirclethroughpoint1Click(Sender: TObject);
var
Lat,Long,Lat2,Long2,Azimuth,Distance : float64;
x,y : integer;
begin
 MapDraw.ScreenToLatLongDegree(LastX,LastY,Lat,Long);
 Distance := 10;
 repeat
    ReadDefault('Radius (' + DegSym +') for small circle',Distance);
    Azimuth := 0;
    while (Azimuth < 360.001) do begin
       VincentyPointAtDistanceBearing(Lat,Long,Distance*111200,Azimuth,Lat2,Long2);
       MapDraw.LatLongDegreeToScreen(Lat2,Long2,x,y);
       Petmar.ScreenSymbol(Image1.Canvas,x,y,FilledBox,2,clared);
       Azimuth := Azimuth + 1;
    end;
until not AnswerIsYes('Another small circle about point');
end;


procedure TMapForm.Snglepointspires1Click(Sender: TObject);
begin
   Markasmissing1Click(Sender);
end;

procedure TMapForm.Snowicemaksk1Click(Sender: TObject);
begin
   {$IfDef ExLandsatQA}
      MessageToContinue('Disabled while rewriting for collection 2');
   {$Else}
      SatImage[Mapdraw.SATonMap].GetLandsatQAMap('Snow-ice', SatImage[Mapdraw.SATonMap].OriginalFileName,10,11);
   {$EndIf}
end;

procedure TMapForm.Southwestern1Click(Sender: TObject);
begin
   ThinDEM1Click(Sender);
end;

procedure TMapForm.GridPosts2Click(Sender: TObject);
begin
   Show1Click(Sender);
end;


procedure TMapForm.Gridspacinganddeclination1Click(Sender: TObject);
begin
   DEM_computations.DoGridSpacingAndDeclination(MapDraw.DEMonMap);
end;


procedure TMapForm.Expandcolorrange1Click(Sender: TObject);
var
   eRange : integer;
   avgelev : float32;
begin
    if not MapDraw.LockZcolors then begin
       DEMGlb[MapDraw.DEMonMap].BoxAreaExtremeElevations(MapDraw.MapAreaDEMGridLimits,MapDraw.MinMapElev,MapDraw.MaxMapElev,avgelev);
    end;

    {$IfDef RecordElevationScaling} WriteLineToDebugFile('Expand color range to ' + RealToString(MapDraw.MinMapElev,-18,0) + '--' + RealToString(MapDraw.MaxMapElev,-18,0)); {$EndIf}
    with MapDraw do if MapOwner = moPointVerificationMap then begin
       eRange := round(MaxMapElev - MinMapElev);
       if eRange < 25 then MapOverlays.ConInt := 1
       else if eRange < 100 then MapOverlays.ConInt := 5
       else if eRange < 250 then MapOverlays.ConInt := 10
       else if eRange < 500 then MapOverlays.ConInt := 20
       else if eRange < 1000 then MapOverlays.ConInt := 30
       else MapOverlays.ConInt := 50;
    end;
    DrawColoredMap1Click(Nil);
end;

procedure TMapForm.Platerotations1Click(Sender: TObject);
begin
  {$IfDef ExGeology}
  {$Else}
     PlateRotationSetup;
  {$EndIf}
end;



procedure TMapForm.LoadCartoDBoverlay(fName : PathStr);
begin
   if (FName = '') then begin
      fName := DBDir + 'groups\';
      if not Petmar.GetFileFromDirectory('shape file grouping',DefaultDBMask,fName) then exit;
   end;
   AddOrSubtractOverlay(Self,ovoWorldOutlines,false);
   MapDraw.DeleteSingleMapLayer(MapDraw.CartoDBfName);
   MapDraw.CartoGroupShapesUp := fName;
   AddOrSubtractOverlay(self,ovoCartoDB,True);
   DoFastMapRedraw;
end;


procedure TMapForm.Loadcartographicoverlay1Click(Sender: TObject);
begin
   LoadCartoDBoverlay('');
end;


procedure TMapForm.Loaddatabase1Click(Sender: TObject);
var
   fName : PathStr;
begin
   fName := ChangeFileExt(DEMGlb[MapDraw.DEMonMap].DEMFileName,'.vat.dbf');
   FeaturesDB := LoadDataBaseFile(fName);
   DoFastMapRedraw;
end;

procedure TMapForm.Viewshed2Click(Sender: TObject);
begin
   ChangeDEMNowDoing(QuickWeaponsFan);
end;

procedure TMapForm.LibSpeedButtonClick(Sender: TObject);
begin
   Integrateddatabasesetup1Click(Sender);
end;

procedure TMapForm.Likelymissingdatacodes1Click(Sender: TObject);
begin
   Markasmissing1Click(Sender);
end;

procedure TMapForm.LinedetectionHoughtransform1Click(Sender: TObject);
var
   fName,fName2,fName3 : PathStr;
   bmp : tMyBitmap;
   cmd : string;
   TStr : tStringList;
begin
   fName := Petmar.NextFileNumber(MDTempDir, 'todeskew_','.png');
   bmp := PetImage.LoadBitmapFromFile(MapDraw.BaseMapFName);
   Petimage.SaveBitmap(Bmp,fName);
   bmp.Destroy;
   fName2 := Petmar.NextFileNumber(MDTempDir, 'deskewed_','.png');
   fName3 := Petmar.NextFileNumber(MDTempDir, 'deskewed_','.txt');
   cmd := ProgramRootDir + 'deskew\deskew64.exe -a 45 -o ' + fname2 + ' ' + fName + ' >' + fName3;
   WinExecAndWait32(cmd);
   TStr := tStringList.Create;
   TStr.LoadFromFile(fName3);
   Petimage_form.DisplayBitmap(fName2,TStr.Strings[4]);
   TStr.Destroy;
end;

procedure TMapForm.LinedetectionHoughtransform2Click(Sender: TObject);
begin
   LinedetectionHoughtransform1Click(Sender);
end;

procedure TMapForm.Lineofsight1Click(Sender: TObject);
begin
   {$IfDef Microdem}
      WMDEM.LOS1Click(Sender);
   {$EndIf}
end;

procedure TMapForm.ID1Click(Sender: TObject);
begin
   IDSpeedButtonClick(Sender);
end;

procedure TMapForm.ID2Click(Sender: TObject);
var
   i,RecsFound : integer;
   TStr : ShortString;
begin
   for i := 1 to MaxDataBase do begin
      if (GISdb[i] <> nil) and (GISdb[i].theMapOwner = Self) then begin
         GISdb[i].IdentifyRecord(LastX,LastY,RightClickLat,RightClickLong,RecsFound, true, false,TStr,false,false);
      end;
   end;
end;



procedure TMapForm.Steepestslope1Click(Sender: TObject);
{$IfDef ExMapReports}
begin
{$Else}
var
  Col,Row,x,y : integer;
  SlopeAsp : tSlopeAspectRec;
  MaxSlope : float64;
begin
  if (MapDraw.ValidDEMonMap) then {with MapDraw do} begin
     if ShowSatProgress then Startprogress('Slopes');
     MaxSlope := -99;
     for Col := round(MapDraw.MapCorners.BoundBoxDataGrid.xmin) to round(MapDraw.MapCorners.BoundBoxDataGrid.xmax) do begin
        if ShowSatProgress and (col mod 25 = 0) then UpdateProgressBar(Col/DEMGlb[MapDraw.DEMonMap].DEMheader.NumCol);
        for Row := round(MapDraw.MapCorners.BoundBoxDataGrid.ymin) to round(MapDraw.MapCorners.BoundBoxDataGrid.ymax) do begin
           if DEMGlb[MapDraw.DEMonMap].GetSlopeAndAspect(Col,Row,SlopeAsp) and (SlopeAsp.SlopePercent > MaxSlope) then begin
              SteepestSlopeCol := Col;
              SteepestSlopeRow := Row;
              MaxSlope := SlopeAsp.SlopePercent;
           end;
        end;
     end;
     if ShowSatProgress then EndProgress;
     MapDraw.DEMGridToScreen(SteepestSlopeCol,SteepestSlopeRow,x,y);
     ScreenSymbol(Image1.Canvas,x,y,FilledBox,3,claRed);
     DEMGlb[MapDraw.DEMonMap].SlopeMethodsReport(SteepestSlopeCol,SteepestSlopeRow,'Steepest point in DEM');
  end;
{$EndIf}
end;


procedure TMapForm.Keylatitudes1Click(Sender: TObject);
const
   TropicLat = 23.5;
var
   LineColor : tPlatformColor;
   LineSize :  byte;
begin
   LineColor := claLime;
   LineSize := 3;
   if (Sender <> Nil) then PickLineSizeAndColor('Key latitudes',Nil,LineColor,LineSize);
   Image1.Canvas.Pen.Color := ConvertPlatformColorToTColor(LineColor);
   Image1.Canvas.Pen.Width := LineSize;
   MapDraw.DrawLatLine(Image1.Canvas,90-TropicLat);
   MapDraw.DrawLatLine(Image1.Canvas,TropicLat);
   MapDraw.DrawLatLine(Image1.Canvas,0);
   MapDraw.DrawLatLine(Image1.Canvas,-(90-TropicLat));
   MapDraw.DrawLatLine(Image1.Canvas,-TropicLat);
end;


procedure TMapForm.Keylatitudes2Click(Sender: TObject);
begin
    Keylatitudes1Click(Sender);
end;

procedure TMapForm.Keypercentiles1Click(Sender: TObject);
begin
   {$IfDef ExMultigrid}
   {$Else}
      MultiGridArray[MapDraw.MultiGridOnMap].KeyPercentiles(false);
   {$EndIf}
end;


procedure TMapForm.Digitizecontours1Click(Sender: TObject);
begin
    {$IfDef ExDigitize}
    {$Else}
       XYZDisplayForm := TXYZDisplayForm.Create(self);
       XYZDisplayForm.MapOwner := Self;
    {$EndIf}
end;


procedure TMapForm.Gammagrids1Click(Sender: TObject);
var
   BoxSize : integer;
begin
   BoxSize := 10;
   ReadDefault('Box size (pixels)',BoxSize);
   MakeGammaGrids(MapDraw.DEMonMap,BoxSize);
end;

procedure TMapForm.Gaussian1Click(Sender: TObject);
var
   NewGrid : integer;
begin
   {$IfDef NoExternalPrograms}
   {$Else}
       NewGrid := WBT_GaussianCurvature(GeotiffDEMNameOfMap);
       MatchAnotherDEMMap(NewGrid,MapDraw.DEMonMap);
   {$EndIf}
end;

procedure TMapForm.Gaussianpyramiddownsample1Click(Sender: TObject);
//DEM does not have to be square, and can have holes
//DEM can be geographic or UTM
var
   i,FilDEM,StartDEM,x,y,tf : integer;
   fName,SaveDir : PathStr;
   Spacing : float32;
   //FileList : tStringList;
   MeanDEM,ThinDEM,DownDEM,UpDEM,UpFillDEM : array[1..10] of integer;

   procedure MapHouseKeeping(DEM : integer);
   begin
      //DEMGlb[DEM].SetUpMap(DEM,true,MapDraw.MapType);
      fName := SaveDir + DEMGlb[DEM].AreaName + '.dem';
      DEMGlb[DEM].WriteNewFormatDEM(fName);
   end;

begin
   {$IfDef RecordGeomorphometry} WritelineToDebugFile('TMapForm.Gaussianpyramiddownsample1Click in'); {$EndIf}

   SaveDir := GetMultiGridPath;
   ReadDefault('levels (factor 2)',MDDef.PyramidLevels);
   fName := SaveDir + 'z_original.dem';
   DEMGlb[MapDraw.DEMonMap].WriteNewFormatDEM(fName);
   StartDEM := MapDraw.DEMonMap;

   for i := 1 to MDDef.PyramidLevels do begin
      //thin the DEM
      tf := round(Math.Power(2,i));
      ThinDEM[i] := DEMGlb[MapDraw.DEMonMap].ThinThisDEM('thin_' + IntToStr(i),tf);
      MapHouseKeeping(ThinDEM[i]);

      MeanDEM[i] := DEMGlb[MapDraw.DEMonMap].ThinThisDEM('mean_' + IntToStr(i),tf,true);
      MapHouseKeeping(MeanDEM[i]);
   end;

   {$IfDef RecordGeomorphometry} WritelineToDebugFile('TMapForm.Gaussianpyramiddownsample1Click thin over'); {$EndIf}

   for i := 1 to MDDef.PyramidLevels do begin
      //filter and then thin the DEM
      DEMGlb[StartDEM].FilterThisDEM(fcFilFile,FilDEM,2,ProgramRootDir + 'filters\gauss-5x5.fil');
      DownDEM[i] := DEMGlb[FilDEM].ThinThisDEM('down_' + IntToStr(i),2);
      CloseSingleDEM(FilDEM);
      MapHouseKeeping(DownDEM[i]);

      //upsample DEM by reinterpolation
      if (DEMGlb[DownDEM[i]].DEMheader.DEMUsed = UTMbasedDEM) then UpDEM[i] := DEMGlb[DownDEM[i]].ReinterpolateUTMDEM( 0.5 * DEMGlb[DownDEM[i]].DEMheader.DEMySpacing)
      else begin
         Spacing := 0.5 * 3600 * DEMGlb[DownDEM[i]].DEMheader.DEMySpacing;
         UpDEM[i] := DEMGlb[DownDEM[i]].ReinterpolateLatLongDEM(Spacing);
      end;

      DEMGlb[UpDEM[i]].AreaName := 'up_' + IntToStr(pred(i));
      MapHouseKeeping(UpDEM[i]);

      //mark as missing even columns and rows, added by interpolation
      x := 1;
      while x < DEMGlb[UpDEM[i]].DEMheader.NumCol do begin
         y := 1;
         while y < DEMGlb[UpDEM[i]].DEMheader.NumRow do begin
            DEMGlb[UpDEM[i]].SetGridMissing(x,y);
            inc(y,2);
         end;
         inc(x,2);
      end;

      //filter algorithm correctly deals with missing data to smooth upsampled DEM
      DEMGlb[UpDEM[i]].FilterThisDEM(fcFilFile,UpFillDEM[i],2,ProgramRootDir + 'filters\gauss-5x5.fil');
      DEMGlb[UpDEM[i]].ReloadDEM(true);  //in case user wants to continue to manipulate
      DEMGlb[UpFillDEM[i]].AreaName := 'fil_up_' + IntToStr(pred(i));
      MapHouseKeeping(UpFillDEM[i]);
      StartDEM := DownDEM[i];
   end;
   {$IfDef RecordGeomorphometry} WritelineToDebugFile('TMapForm.Gaussianpyramiddownsample1Click out'); {$EndIf}
end;


procedure tMapForm.Gazetteer1Click(Sender: TObject);
{$IfDef ExGazetteer}
begin
{$Else}
var
  fName : PathStr;
begin
   {$IfDef RecordGazetteer} WritelineToDebugFile('tMapForm.Gazetteer1Click'); {$EndIf}
  fName := LastGazFile;
  if not GetGazFileName(fName) then exit;
  OpenGazetterOnMap(fName);
{$EndIf}
end;


procedure tMapForm.OpenGazetterOnMap(fName : PathStr; RedrawNow : boolean = true);
begin
{$IfDef ExGazetteer}
{$Else}
   {$IfDef RecordGazetteer} WritelineToDebugFile('tMapForm.OpenGazetterOnMap' + fName); {$EndIf}
   if (fName <> '') and FileExists(fName) then begin
     MapDraw.MapGazDB := OpenDBonMap('',fName,true,false);
     GISdb[MapDraw.MapGazDB].SetGazTableOptions;
     GISdb[MapDraw.MapGazDB].dbOpts.DBAutoShow := dbasGaz;
     GISdb[MapDraw.MapGazDB].RedrawLayerOnMap;
     AddOrSubtractOverlay(Self,ovoGazetteer,true);
     if RedrawNow then DoFastMapRedraw;
  end;
  {$EndIf}
end;

procedure TMapForm.Geology1Click(Sender: TObject);
begin
   {$IfDef ExGeology}
   {$Else}
      PlateRotations1.Visible := (MDDef.ProgramOption in [GeologyProgram,ExpertProgram]);
      CandeAndKentTimeScale1.Visible := (MDDef.ProgramOption in [GeologyProgram,ExpertProgram]) and FileExists(MagneticAnomalyTimeScale);
      MagneticAnomaly1.Visible := (MDDef.ProgramOption in [GeologyProgram,ExpertProgram]);
      Hotspots1.Visible := (MDDef.ProgramOption in [GeologyProgram,ExpertProgram]) and FileExists(DBDir + 'hotspot' + DefaultDBExt);
      FractureZones1.Visible := (MDDef.ProgramOption in [GeologyProgram,ExpertProgram]) and  FileExists(DBDir + 'wfz' + DefaultDBExt);
      Structuralgeologycomputations1.Visible := (MDDef.ProgramOption in [GeologyProgram,ExpertProgram]) and (MapDraw.ValidDEMonMap);
      Projectfocalmechanismtosurface1.Visible := (MDDef.ProgramOption = ExpertProgram) and (MapDraw.ValidDEMonMap);
      GeologyPopupMenu15.Popup(Mouse.CursorPos.X,Mouse.CursorPos.Y);
   {$EndIf}
end;


procedure TMapForm.Geology3Click(Sender: TObject);
begin
   Geology1Click(Sender);
end;

procedure TMapForm.GeologySpeedButton1Click(Sender: TObject);
begin
   Geology1Click(Sender);
end;


procedure TMapForm.Validpointsinmatchinggrid1Click(Sender: TObject);
begin
   MaskFromSecondGrid(-1,msSecondValid);
end;

procedure TMapForm.Validpointssinglecategory1Click(Sender: TObject);
begin
  Locationsonly1Click(Sender);
end;

procedure TMapForm.Values1Click(Sender: TObject);
begin
   Locationsonly1Click(Sender);
end;

procedure TMapForm.VARI1Click(Sender: TObject);
begin
   NewSatWindow(nsbVARI);
end;

procedure TMapForm.Variableopaquemerge1Click(Sender: TObject);
begin
   //MatchThiscoverageareaandsamepixelsize1Click(Sender);
   MergeColorScene1Click(Sender);
end;


procedure TMapForm.VATdisplay1Click(Sender: TObject);
begin
   {$IfDef RecordVAT} WriteLineToDebugFile('TMapForm.VATdisplay1Click'); {$EndIf}
   MapDraw.MapType := mtDEMVATTable;
   DEMGlb[MapDraw.DEMonMap].CreateVATforDEM;
   DoBaseMapRedraw;
end;

procedure TMapForm.TrackBar2Change(Sender: TObject);
var
   Bitmap : tMyBitmap;
begin
    Bitmap := BlendBitmaps(MapBaseBMP,OverlayOpaqueBMP,0.01 * TrackBar2.Position);

    if (SliderDrapeMap <> Nil) and (SliderDrapeMap.MapDraw.LegendOverlayfName <> '') then begin
       SliderDrapeMap.MapDraw.DrawLegendsOnMap(Bitmap);
    end;

    Image1.Picture.Graphic := Bitmap;
    Bitmap.Free;
    MDDef.MapOverlayOpacity := TrackBar2.Position;
end;


procedure TMapForm.BitBtn1Click(Sender: TObject);
begin
   NoMovingForm := false;
   FreeAndNil(OverlayOpaqueBMP);
   FreeAndNil(MapBaseBMP);
   SliderDrapeMap := Nil;
   BlendPanel.Height := 0;
   Caption := Self.MapDraw.BaseTitle;

   //TrackBar2.Enabled := false;
   SetClientHeight;
   ZoomInSpeedButton4.Enabled := true;
   Self.ZoomOutSpeedButton5.Enabled := true;
   if (not MapDraw.ClosingMapNow) then DoFastMapRedraw;
   VariableOpaqueOverlays := false;
end;

procedure TMapForm.BitBtn2Click(Sender: TObject);
begin
   {$IfDef ExMovies}
   {$Else}
      BlendMovie(OverlayOpaqueBMP,MapBaseBMP);
   {$EndIf}
end;


procedure TMapForm.BitmapandXYZBfile1Click(Sender: TObject);
begin
   {$IfDef ExFMX3D}
   {$Else}
     Map3D := MapTo3DView(Self.MapDraw);
   {$EndIf}
end;


procedure TMapForm.Overlaysvariableopaque1Click(Sender: TObject);
begin
   VariableOpaqueOverlays := true;
   DoFastMapRedraw;
end;

procedure TMapForm.Plateboundaries1Click(Sender: TObject);
var
   db : integer;
begin
   {$IfDef ExGeology}
   {$Else}
      //if MDDef.MoveGeologyDBMemory then DesiredDBMode := dbmCDS;
      db := LoadDataBaseFile(PlateBoundaryFile);
      GISdb[db].dbOpts.dbAutoShow := dbasColorField;
      GISdb[db].RedrawLayerOnMap;
   {$EndIf}
end;

procedure TMapForm.Plateoutlines1Click(Sender: TObject);
var
   db : integer;
begin
   {$IfDef ExGeology}
   {$Else}
      //if MDDef.MoveGeologyDBMemory then DesiredDBMode := dbmCDS;
      db := LoadDataBaseFile(PlateOutlinesfName);
      GISdb[db].dbOpts.dbAutoShow := dbasColorField;
      GISdb[db].RedrawLayerOnMap;
   {$EndIf}
end;

procedure TMapForm.Earthquakes1Click(Sender: TObject);
begin
   {$IfDef RecordGISDB} WriteLineToDebugFile('TMapForm.Earthquakes1Click'); {$EndIf}
   {$IfDef ExGeology}
   {$Else}
      LoadDataBaseFile(DBDir + 'quakes' + DefaultDBExt);
   {$EndIf}
end;

procedure TMapForm.Eastern1Click(Sender: TObject);
begin
   ThinDEM1Click(Sender);
end;

procedure TMapForm.Eastwest1Click(Sender: TObject);
begin
   FeatureMigration(cdE);
end;

procedure TMapForm.Volcanoes1Click(Sender: TObject);
begin
   {$IfDef ExGeology}
   {$Else}
      {$IfDef RecordGISDB} WriteLineToDebugFile('TMapForm.Volcanoes1Click'); {$EndIf}
      //if MDDef.MoveGeologyDBMemory then DesiredDBMode := dbmCDS;
      LoadDataBaseFile(VolcanoesDB);
   {$EndIf}
end;

procedure TMapForm.Magneticanomaly1Click(Sender: TObject);
begin
   {$IfDef ExGeology}
   {$Else}
   //if MDDef.MoveGeologyDBMemory then DesiredDBMode := dbmCDS;
   LoadDataBaseFile(DBDir + 'wmag' + DefaultDBExt);
   {$EndIf}
end;

procedure TMapForm.Magneticanomalypicks1Click(Sender: TObject);
begin
   {$IfDef ExGeology}
   {$Else}
   //if MDDef.MoveGeologyDBMemory then DesiredDBMode := dbmCDS;
   LoadDataBaseFile(GSFML_global_picks);
   {$EndIf}
end;

procedure TMapForm.Magneticdeclination1Click(Sender: TObject);
{$IfDef ExMag}
begin
{$Else}
var
   lat,long : float64;
   dz,DEC,DIP,TI,GV : double;
   GridInc,x1,y1         : integer;
   Bitmap : tMyBitmap;
begin
   dz := MapDraw.MapCorners.BoundBoxGeo.xmax - MapDraw.MapCorners.BoundBoxGeo.xmin;
   if MapDraw.AFullWorldMap or (dz > 300) then GridInc := 10
   else if (dz < 200) then GridInc := 5
   else if (dz < 100) then GridInc := 2
   else if (dz < 50) then GridInc := 1;
   lat := -75;
   CopyIMageToBitmap(Image1,Bitmap);
   while (lat < 80) do with MapDraw do begin
      long := -180;
      while (long < 180) do begin
         LatLongDegreeToScreen(lat,long,x1,y1);
         if OnScreen(x1,y1) then begin
            MagVr1(0,lat,Long,CurMagYear,DEC,DIP,TI,GV);
            PlotOrientedLine(Bitmap,x1,y1,12,round(dec),claRed,2);
         end;
         long := long + GridInc;
      end;
      lat := lat + GridInc;
   end;
   Image1.Picture.Graphic := Bitmap;
   Bitmap.Free;
{$EndIf}
end;

procedure TMapForm.Magneticnorthlines1Click(Sender: TObject);
begin
   Truenorthlines1Click(Sender);
end;

procedure TMapForm.Fracturezones1Click(Sender: TObject);
begin
    {$IfDef ExGeology}
    {$Else}
    //if MDDef.MoveGeologyDBMemory then DesiredDBMode := dbmCDS;
    LoadDataBaseFile(DBDir + 'wfz' + DefaultDBExt)
    {$EndIf}
end;

procedure TMapForm.Fresnelzones1Click(Sender: TObject);
begin
   {$IfDef ExFresnel}
   {$Else}
   ChangeDEMNowDoing(FirstFresnelPoint);
   {$EndIf}
end;

procedure TMapForm.Fromllokuptable1Click(Sender: TObject);
var
   fName : PathStr;
   LUT : array[1..255] of byte;
   i,x,y,NPts,tn : integer;
   z : float32;
begin
   fName := ProgramRootDir;
   for i := 0 to 255 do LUT[i] := i;
   tn := OpenDBonMap('Reclassify lookup',fName,false,false);
   if tn <> 0  then begin
      while not GISdb[tn].MyData.eof do begin
         LUT[GISdb[tn].MyData.GetFieldByNameAsInteger('OLD_CODE')] := GISdb[tn].MyData.GetFieldByNameAsInteger('NEW_CODE');
         GISdb[tn].MyData.Next;
      end;
      NPts := 0;
      StartProgress('Reclassify');
      for x := 0 to pred(DEMGlb[MapDraw.DEMonMap].DEMheader.NumCol) do begin
         if (x mod 50 = 0) then UpDateProgressBar(x/DEMGlb[MapDraw.DEMonMap].DEMheader.NumCol);
         for y := 0 to pred(DEMGlb[MapDraw.DEMonMap].DEMheader.NumRow) do      begin
            if DEMGlb[MapDraw.DEMonMap].GetElevMetersOnGrid(x,y,z) then begin
                DEMGlb[MapDraw.DEMonMap].SetGridElevation(x,y,LUT[Round(z)]);
               inc(NPts);
            end;
         end;
      end;
      CloseAndNilNumberedDB(tn);
      RespondToChangedDEM;
      MessageToContinue(IntToStr(NPts) + ' points reclassified');
   end;
end;

procedure TMapForm.Earthquakefocalmechanisms1Click(Sender: TObject);
var
   db : integer;
begin
   {$IfDef ExGeology}
   {$Else}
      {$IfDef RecordGISDB} WriteLineToDebugFile('TMapForm.Earthquakefocalmechanisms1Click'); {$EndIf}
      //if MDDef.MoveGeologyDBMemory then DesiredDBMode := dbmCDS;
      db := LoadDataBaseFile(CMT_fault_cent_fName,true,false);
      if db <> 0 then begin
         GISdb[db].dbOpts.DBAutoShow := dbasQuakeMechColor;
         AddOverlay(Self,ovoDatabases);
         GISdb[db].RedrawLayerOnMap;
      end;
   {$EndIf}
end;

procedure TMapForm.Pointcloud1Click(Sender: TObject);
begin
   {$IfDef ExPointCloud}
   {$Else}
      OvelayPointClouds(Self);
   {$EndIf}
end;


procedure TMapForm.PointCloudSpeedButtonClick(Sender: TObject);
begin
   LASFile1Click(Sender);
end;


procedure TMapForm.Pointcloudtodatabase1Click(Sender: TObject);
{$IfDef ExPointCloud}
begin
{$Else}
var
   Cloud,xg,yg : integer;
   fName : PathStr;
   BoundBox : sfBoundBox;
begin
   Cloud := 1;
   if ( pt_cloud_opts_fm.LasFiles[Cloud].LAS_fnames.Count > 0) then begin
      fName := NextFileNumber(MDtempDir, 'point_export_','.csv');
      DEMGlb[MapDraw.DEMonMap].LatLongDegreeToDEMGridInteger(RightClickLat,RightClickLong,xg,yg);
      DEMGlb[MapDraw.DEMonMap].DEMGridToLatLongDegree(xg,yg,RightClickLat,RightClickLong);
      BoundBox.YMin := RightClickLat - DEMGlb[MapDraw.DEMonMap].DEMheader.DEMySpacing / 2;
      BoundBox.XMin := RightClickLong - DEMGlb[MapDraw.DEMonMap].DEMheader.DEMxSpacing / 2;
      BoundBox.YMax := RightClickLat + DEMGlb[MapDraw.DEMonMap].DEMheader.DEMySpacing / 2;
      BoundBox.XMax := RightClickLong + DEMGlb[MapDraw.DEMonMap].DEMheader.DEMxSpacing / 2;
      pt_cloud_opts_fm.LasFiles[Cloud].ShowLASProgress := true;
      pt_cloud_opts_fm.LasFiles[Cloud].KMLLasPoints(BoundBox,exmDBF,fName);
      LoadDataBaseFile(fName);
   end;
{$EndIf}
end;

procedure TMapForm.PointcloudtoGoogleEarth1Click(Sender: TObject);
{$IfDef ExPointCloud}
begin
{$Else}
var
   Cloud : integer;
   BoundBox : sfBoundBox;
begin
   Cloud := 1;
   if ( pt_cloud_opts_fm.LasFiles[Cloud].LAS_fnames.Count > 0) then begin
      VincentyPointAtDistanceBearing(RightClickLat,RightClickLong,25,235,BoundBox.YMin,BoundBox.XMin);
      VincentyPointAtDistanceBearing(RightClickLat,RightClickLong,25,45,BoundBox.YMax,BoundBox.XMax);
      pt_cloud_opts_fm.LasFiles[Cloud].ShowLASProgress := true;
      pt_cloud_opts_fm.LasFiles[Cloud].KMLLasPoints(BoundBox,exmKML);
   end;
{$EndIf}
end;


procedure TMapForm.PointcloudtoOpenGL1Click(Sender: TObject);
{$If Defined(ExPointCloud) or Defined(ExFMX3D)}
begin
{$Else}
var
   Lat,Long : float64;
   Cloud : integer;
   BoundBox : sfBoundBox;
   GeometryFName,ColorsFName,
   fName : PathStr;
   LasData : tLAS_data;
begin
   MapDraw.ScreenToLatLongDegree(RightClickX,RightClickY,Lat,Long);
   Cloud := 1;
   if ( pt_cloud_opts_fm.LasFiles[Cloud].LAS_fnames.Count > 0) then begin
      VincentyPointAtDistanceBearing(Lat,Long,250,235,BoundBox.YMin,BoundBox.XMin);
      VincentyPointAtDistanceBearing(Lat,Long,250,45,BoundBox.YMax,BoundBox.XMax);
      pt_cloud_opts_fm.LasFiles[Cloud].ShowLASProgress := true;
      fName := Petmar.NextFileNumber(MDTempDir, 'clound_for_ogl_','.las');
      pt_cloud_opts_fm.LasFiles[Cloud].CutBBox := BoundBox;
      if pt_cloud_opts_fm.LasFiles[Cloud].MergeLasPoints(mlInBox,Self,fName) then begin
         LasData := Las_Lidar.tLAS_data.Create(FName);
         LasData.OldExportBinary(Cloud,GeometryFName,ColorsFName);
         LasData.Destroy;
         FMX3dViewer(True,GeometryfName,'','','','', ColorsFName,'','','','');
      end
      else MessageToContinue('No point cloud coverage');
   end;
{$EndIf}
end;

procedure TMapForm.Pointcoordinates2Click(Sender: TObject);
begin
   PostPointCoordinates(RightClickLat,RightClickLong);
end;


procedure TMapForm.Pointinterpolationalgorithms2Click(Sender: TObject);
begin
   PointInterpolationReport(RightClickLat,RightClickLong);
end;


procedure TMapForm.Mask1Click(Sender: TObject);
begin
   QueryColor(MapDraw.MaskColor);
   MapDraw.MapType := mtDEMMask;
   DoBaseMapRedraw;
end;



procedure TMapForm.Maskallothergridstomatchthisone1Click(Sender: TObject);
var
   i : integer;
begin
   for i := 1 to MaxDEMDataSets do begin
      if ValidDEM(i) and (I <> MapDraw.DEMonMap) and Assigned(DEMGlb[i].SelectionMap) then begin
         DEMGlb[i].SelectionMap.MaskFromSecondGrid(MapDraw.DEMonMap, msSecondMissing);
         DEMGlb[i].SavePartOfDEMWithData(DEMGlb[i].DEMFileName);
         DEMGlb[i].DEMAlreadyDefined := false;
         DEMGlb[i].SelectionMap.ReloadDEMClick(Nil);
      end;
   end;
   UpdateMenusForAllMaps;
end;

procedure TMapForm.Geologysymbols1Click(Sender: TObject);
{$IfDef ExGeology}
begin
{$Else}
var
   fName : PathStr;
begin
   FName := DBDir;
   if GetFileNameDefaultExt('geology symbols',DBNameMask,FName) then begin
      CreateGeologySymbolFile(fName);
   end;
   ChangeDEMNowDoing(GetGeologySymbols);
   GeosymbolTable := tMyData.Create(fName);
{$EndIf}
end;


procedure TMapForm.Loadvegetationgrid1Click(Sender: TObject);
{$IfDef ExVegGrid}
begin
{$Else}
var
   fName,LName : PathStr;
begin
   fName := LastDEMName;
   if (DEMGlb[MapDraw.DEMonMap].VegGrid[1] <> 0) then begin
      DEMGlb[MapDraw.DEMonMap].CloseVegGrid(1);
      LName := '';
   end
   else lName := LastVegGrid;
    DEMNowDoing := Calculating;
    DEMGlb[MapDraw.DEMonMap].OpenVegGrid(lName,1);
    DEMNowDoing := Calculating;
    ApplicationProcessMessages;
    ChangeDEMNowDoing(JustWandering);
    if (DEMGlb[MapDraw.DEMonMap].VegGrid[1] <> 0) and (MDDef.VegOptionMap <> voDTM) then DoBaseMapRedraw;
    LastVegGrid := LastDEMName;
    LastDEMName := fName;
    CheckProperTix;
{$EndIf}
end;


procedure TMapForm.Loadvegetationlayers1Click(Sender: TObject);
begin
   Loadvegetationlayer(1,Loadvegetationlayers1);
end;


procedure TMapForm.Loadvegetationgrid21Click(Sender: TObject);
{$IfDef ExVegDensity}
begin
{$Else}
var
   fName : PathStr;
begin
   fName := ExtractFilePath(LastVegGrid);
   DEMGlb[MapDraw.DEMonMap].OpenVegGrid(fName,2);
   CheckProperTix;
{$EndIf}
end;

procedure TMapForm.Loadvegetationlayer(i : integer; mItem : tMenuItem);
{$IfDef ExVegDensity}
begin
{$Else}
begin
    if (i=1) then DEMGlb[MapDraw.DEMonMap].VegDensityLayers[i] := tVegDensity.Create(LastVegDensity1fName,MapDraw.DEMonMap)
    else DEMGlb[MapDraw.DEMonMap].VegDensityLayers[i] := tVegDensity.Create(LastVegDensity2fName,MapDraw.DEMonMap);
    if (DEMGlb[MapDraw.DEMonMap].VegDensityLayers[i].LayersPresent = 0) then begin
        MessageToContinue('No vegetation layers found');
        DEMGlb[MapDraw.DEMonMap].VegDensityLayers[i].Destroy;
        DEMGlb[MapDraw.DEMonMap].VegDensityLayers[i] := Nil;
        MapDraw.VegDensityLayerInUse := 0;
    end
    else begin
       MapDraw.VegDensityLayerInUse := i;
    end;
    CheckProperTix;
    LastDEMName := DEMGlb[MapDraw.DEMonMap].DEMFileName;
{$EndIf}
end;


procedure TMapForm.Usecurrentsubset1Click(Sender: TObject);
begin
   {$IfDef NoDBGrafs}
   {$Else}
      SatImage[MapDraw.SatonMap].SatHistograms(round(MapDraw.MapCorners.BoundBoxDataGrid.xmin),round(MapDraw.MapCorners.BoundBoxDataGrid.ymin),round(MapDraw.MapCorners.BoundBoxDataGrid.xmax),
         round(MapDraw.MapCorners.BoundBoxDataGrid.ymax));
   {$EndIf}
end;

procedure TMapForm.Geodeticbearing1Click(Sender: TObject);
begin
   ChangeDEMNowDoing(GeodeticBearing);
   if (gbLatStart < -99) then GetLatLong(MapDraw.PrimMapProj,'Start point',gbLatStart,gbLongStart);
end;


procedure TMapForm.Geographicquadrangle1Click(Sender: TObject);
begin
   QuadSizeMap(LastX,LastY);
end;


procedure TMapForm.Geoid1Click(Sender: TObject);
var
   Results : tStringList;
   aLine : shortstring;
   Lat,Long : float32;

         procedure DoOne(DEM : integer);
         var
            z : float32;
         begin
            if ValidDEM(DEM) then begin
               DEMGlb[DEM].GetElevFromLatLongDegree(Lat,Long,z);
               aLine := aline + ',' + RealToString(z,-9,3);
            end
            else aLine := ',-999';
         end;

begin
   //ExecuteFile('https://geographiclib.sourceforge.io/cgi-bin/GeoidEval?input=' + RealToString(RightClickLat,-12,-4) + '+' + RealToString(RightClickLong,-12,-4)  + '&option=Submit', '', '');
   Lat := RightClickLat;
   Long := RightClickLong;
   OpenEGMgrids;
   Results := tStringList.Create;
   Results.Add('LAT,LONG,EGM96_GD,EGM2008_GD,DIFF_96_08');
   aLine := RealToString(Lat,-12,-6) + ',' +  RealToString(Long,-12,-6);
   DoOne(EGM2008_grid);
   DoOne(EGM96_grid);
   DoOne(EGMdiff_grid);
   Results.Add(aline);
   StringListToLoadedDatabase(Results,Petmar.NextFileNumber(MDTempDir,'egm_point_','.dbf'));
end;


procedure TMapForm.Geoidgrids1Click(Sender: TObject);
begin
   OpenNewDEM(GeoidWGS84ellipsoidToLocalVDatum);
   OpenNewDEM(Geoid2008FName);
end;

(*
procedure TMapForm.UTM100Kzones1Click(Sender: TObject);
var
   fName : PathStr;
begin
   DownloadandUnzipDataFileIfNotPresent('mgrs6x8_100k');
   fName := MainMapData + 'mgrs6x8_100k\mgrs_region.shp';
   LoadDataBaseFile(fName);
end;
*)

procedure TMapForm.UTM1Click(Sender: TObject);
begin
   CreateGridToMatchMap(cgUTM);
end;


function TMapForm.CreateGridToMatchMap(What : tCreateGrid;  OpenMap : boolean = true; Resolution : tDEMprecision = FloatingPointDEM; SpacingX : float64 = -99; SpacingY : float64 = -99; DesiredUTMZone : integer = -99; RasterPixelIs : byte = 1) : integer;
var
   Lat,Long : float64;
   NewHeader : tDEMheader;
   NewName : shortstring;
   fName : PathStr;
   Perimeter : integer;
   Projection : tMapProjection;
   xmin,xmax,ymin,ymax,
   x,y : float64;
begin
   MapDraw.MapCorners.BoundBoxGeo := MapDraw.GetBoundBoxGeo;
   {$IfDef RecordNewMaps} WriteLineToDebugFile('TMapForm.CreateGridToMatchMap in, Map geo limits: ' + sfBoundBoxToString(MapDraw.MapCorners.BoundBoxGeo,4)); {$EndIf}
   Result := 0;
   ZeroDEMHeader(NewHeader, (What = cgUTM) or (What = cgSpecifyUTM) );
   NewHeader.RasterPixelIsGeoKey1025 := RasterPixelIs;
   NewHeader.DEMPrecision := Resolution;
   NewHeader.LatHemi := MapDraw.PrimMapProj.LatHemi;

    if (DesiredUTMZone > 0) then begin
       NewHeader.UTMZone := DesiredUTMZone;
    end
    else if MapDraw.IsThisMapUTM and ValidDEM(MapDraw.DEMonMap) then begin
       NewHeader.UTMZone := DEMGlb[MapDraw.DEMonMap].DEMheader.UTMZone;
       {$IfDef RecordNewMaps} WriteLineToDebugFile('TMapForm.CreateGridToMatchMap UTM=' + IntToStr(NewHeader.UTMZone) + NewHeader.LatHemi); {$EndIf}
    end
    else begin
       MapDraw.ScreenToLatLongDegree(MapDraw.MapXSize div 2, MapDraw.MapYSize div 2,Lat,Long);
       NewHeader.UTMZone := GetUTMZone(Long);
    end;

    if (What = cgUTM) or (What = cgSpecifyUTM) then begin
       WGS84DatumConstants.DefineDatumFromUTMZone('WGS84',NewHeader.UTMZone,NewHeader.LatHemi,'CreateGridToMatchMap');
       MapDraw.MapCorners.BoundBoxUTM := MapDraw.GetBoundBoxUTM;
       {$IfDef RecordNewMaps} WriteLineToDebugFile('Make UTM for  limits: ' +  sfBoundBoxToString(MapDraw.MapCorners.BoundBoxUTM,4)); {$EndIf}
       if (What = cgSpecifyUTM) then begin
          PickUTMZone(NewHeader.UTMzone);
       end;
        if (SpacingX < 0) then begin
           NewHeader.DEMxSpacing := MDDef.PreferUTMSpace;
           ReadDefault('Data spacing (m) for new grid',NewHeader.DEMxSpacing);
        end
        else NewHeader.DEMxSpacing := SpacingX;
        if SpacingY < 0 then NewHeader.DEMySpacing := NewHeader.DEMxSpacing
        else NewHeader.DEMySpacing := SpacingY;
        NewHeader.DEMSWCornerX := trunc(MapDraw.MapCorners.BoundBoxUTM.XMin / NewHeader.DEMxSpacing) * NewHeader.DEMxSpacing;
        NewHeader.DEMSWCornerY := trunc(MapDraw.MapCorners.BoundBoxUTM.YMin / NewHeader.DEMySpacing) * NewHeader.DEMySpacing;
        NewHeader.NumCol := round((MapDraw.MapCorners.BoundBoxUTM.XMax - MapDraw.MapCorners.BoundBoxUTM.XMin) / NewHeader.DEMxSpacing);
        NewHeader.NumRow := round((MapDraw.MapCorners.BoundBoxUTM.YMax - MapDraw.MapCorners.BoundBoxUTM.YMin) / NewHeader.DEMySpacing);
        if (not (NewHeader.NumRow <= MaxElevArraySize) and (NewHeader.NumCol <= DEMDefs.MaxColsInRAM)) then begin
           MessageToContinue('Grid spacing would require ' + IntToStr(NewHeader.NumCol) + 'x' + IntToStr(NewHeader.NumRow) + '  increase grid spacing or decrease grid extent');
           exit;
        end;
        NewName := 'UTM projection zone ' + IntToStr(NewHeader.UTMzone) + NewHeader.LatHemi;
    end
    else if (What = cgLatLong) then begin
        if (SpacingX < 0) then begin
           NewHeader.DEMxSpacing := MDDef.PreferArcSecSpace;
           ReadDefault('Data spacing (arc sec)',NewHeader.DEMxSpacing);
        end
        else NewHeader.DEMxSpacing := SpacingX;
        if SpacingY < 0 then NewHeader.DEMySpacing := NewHeader.DEMxSpacing
        else NewHeader.DEMySpacing := SpacingY;

        NewHeader.DEMySpacing := NewHeader.DEMySpacing / 3600;
        NewHeader.DEMxSpacing := NewHeader.DEMxSpacing / 3600;
        NewHeader.DEMSWCornerX := trunc(MapDraw.MapCorners.BoundBoxGeo.xmin / NewHeader.DEMxSpacing) * NewHeader.DEMxSpacing;
        NewHeader.DEMSWCornerY := trunc(MapDraw.MapCorners.BoundBoxGeo.ymin / NewHeader.DEMySpacing) * NewHeader.DEMySpacing;
        NewHeader.NumCol := round((MapDraw.MapCorners.BoundBoxGeo.xmax - NewHeader.DEMSWCornerX) / NewHeader.DEMxSpacing);
        NewHeader.NumRow := round((MapDraw.MapCorners.BoundBoxGeo.ymax - NewHeader.DEMSWCornerY) / NewHeader.DEMySpacing);
        if not (NewHeader.NumRow <= MaxElevArraySize) and (NewHeader.NumCol <= DEMDefs.MaxColsInRAM) then begin
           MessageToContinue(ImpGridIncSpace + ' would require ' + IntToStr(NewHeader.NumCol) + 'x' + IntToStr(NewHeader.NumRow));
           exit;
        end;
       NewName := 'geographic coordinates';
    end
    else if (What = cgWKT) then begin
       fName := MDDef.WKTLidarProj;
       Projection := tMapProjection.Create;
       Projection.InitializeProjectionFromWKT(fName);
       NewHeader.WKTString := Projection.WKTString;

       {$IfDef RecordNewWKT} WriteLineToDebugFile(''); Projection.ProjectionParamsToDebugFile('Projection initialized',true); {$EndIf}

       if (SpacingX < 0) then begin
          ReadDefault('Data spacing (projected meters) for new grid', MDDef.PreferWKTSpace);
          NewHeader.DEMxSpacing := MDDef.PreferWKTSpace;
       end
       else NewHeader.DEMxSpacing := SpacingX;
       if (SpacingY < 0) then NewHeader.DEMySpacing := NewHeader.DEMxSpacing
       else NewHeader.DEMySpacing := SpacingY;

       Perimeter := MapDraw.MapPerimeter;
       xmin := 99e39;
       xmax := -99e39;
       ymin := 99e39;
       ymax := -99e39;
       while not GISDB[Perimeter].MyData.eof do begin
           if GISDB[Perimeter].ValidLatLongFromTable(Lat,Long) then begin
              Projection.ForwardProjectDegrees(Lat,Long,x,y);
              {$IfDef RecordNewWKTFull} WriteLineToDebugFile(LatLongDegreeToString(Lat,Long) + RealToString(x,15,2)  + RealToString(y,15,2)); {$EndIf}
              Petmath.CompareValueToExtremes(x,xmin,xmax);
              Petmath.CompareValueToExtremes(y,ymin,ymax);
           end;
           GISDB[Perimeter].MyData.Next;
       end;
       CloseAndNilNumberedDB(Perimeter);

       {$IfDef RecordNewWKT}
          Projection.InverseProjectDegrees(xmin,ymin,Lat,Long);
          Projection.ForwardProjectDegrees(Lat,Long,xmin,ymin);
          WriteLineToDebugFile('SW corner:  ' + LatLongDegreeToString(Lat,Long) + RealToString(xmin,15,2)  + RealToString(ymin,15,2));
          WriteLineToDebugFile('x range=:  ' + RealToString(xmin,-15,2) + ' to ' + RealToString(xmax,-15,2) + '  yrange=' + RealToString(ymin,-15,2) + ' to '  + RealToString(ymax,-15,2));
          WriteLineToDebugFile('NewHeader.WKTString=' + NewHeader.WKTString);
       {$EndIf}

       NewHeader.DEMUsed := WKTDEM;
       NewHeader.DEMSWCornerX := trunc(xmin / NewHeader.DEMxSpacing) * NewHeader.DEMxSpacing;
       NewHeader.DEMSWCornerY := trunc(ymin / NewHeader.DEMySpacing) * NewHeader.DEMySpacing;
       NewHeader.NumCol := round((xmax-xmin) / NewHeader.DEMxSpacing);
       NewHeader.NumRow := round((ymax-ymin) / NewHeader.DEMxSpacing);
       Projection.Destroy;
       NewName := ExtractFileName(fName);
    end;

    {$IfDef RecordNewMaps} WriteLineToDebugFile('Variables set DEM size: ' + IntToStr(NewHeader.NumCol) + 'x' + IntToStr(NewHeader.NumRow) + '  ' + NewHeader.WKTString); {$EndIf}
    OpenAndZeroNewDEM(false,NewHeader,Result,'Grid with ' + NewName, InitDEMmissing);
    {$IfDef RecordNewMaps} WriteLineToDebugFile('OpenAndZeroNewDEM done, DEM=' + IntToStr(Result) + '  Projection ' + DEMGlb[Result].DEMMapProjection.GetProjectionName); {$EndIf}

    if OpenMap then begin
       DEMGlb[Result].SetUpMap(Result,false,MDDef.DefDEMMap);
       {$IfDef RecordNewMaps} WriteLineToDebugFile('TMapForm.CreateGridToMatchMap out ' + DEMGlb[Result].SelectionMap.MapDraw.PrimMapProj.KeyDatumParams); {$EndIf}
    end;
    BackToWandering;
   {$IfDef RecordNewMaps} WriteLineToDebugFile('TMapForm.CreateGridToMatchMap out ' + DEMGlb[Result].FullDEMParams); {$EndIf}
end;



procedure TMapForm.UTMgridtruenorthdeclination1Click(Sender: TObject);
begin
   ChangeDEMNowDoing(UTMTrueDeviation);
end;


procedure TMapForm.UTMspecifyzone1Click(Sender: TObject);
begin
   CreateGridToMatchMap(cgSpecifyUTM);
end;

procedure TMapForm.Font1Click(Sender: TObject);
begin
   EditTFont(Image1.Canvas.Font);
end;

procedure TMapForm.PLSSfromkeyboardlocation1Click(Sender: TObject);
var
   Lat,Long : float64;
   MenuStr : shortString;
begin
   GetLatLong(MapDraw.PrimMapProj,'PLSS location',Lat,Long);
   MapDraw.MapSymbolAtLatLongDegree(Image1.Canvas,Lat,Long,MDDef.KeyLocationSymbol);
   MenuStr := LatLongDegreeToString(Lat,Long,MDDef.OutPutLatLongMethod) + MessLineBreak + PLSSLocation(Lat,Long);
   {$IfDef RecordPLSS} WriteLineToDebugFile(' TMapForm.PLSSfromkeyboardlocation1Click  ' + MenuStr); {$EndIf}
   MessageToContinue(MenuStr,True);
end;

procedure TMapForm.PLSSlocation1Click(Sender: TObject);
begin
   MessageToContinue(LatLongDegreeToString(RightClickLat,RightClickLong,MDDef.OutPutLatLongMethod) + MessLineBreak + PLSSLocation(RightClickLat,RightClickLong),true);
end;

procedure TMapForm.PLSSposition1Click(Sender: TObject);
begin
   {$IfDef ExPLSS}
   {$Else}
      if TryToOpenPLSS(Self) then ChangeDEMNowDoing(PLSSposition);
   {$EndIf}
end;


procedure TMapForm.PNG1Click(Sender: TObject);
var
   MyPNG : PNGImage.TPNGImage;
   i,j,zi : integer;
   z : float32;
   Bitmap : tMyBitmap;
   P0 : pRGB;
begin
   CreateBitmap(Bitmap,DEMGlb[MapDraw.DEMonMap].DEMheader.NumCol,DEMGlb[MapDraw.DEMonMap].DEMheader.NumRow);
   for j := 0 to pred(Bitmap.Height) do begin
      P0 := Bitmap.Scanline[pred(Bitmap.Height)-j];
      for i := 0 to pred(Bitmap.Width) do begin
          if DEMGlb[MapDraw.DEMonMap].GetElevMetersOnGrid(i,j,z) then begin
              zi := round(100 * z);
              p0[i].rgbtRed := zi mod 256;
              zi := zi div 256;
              p0[i].rgbtGreen := zi mod 256;
              p0[i].rgbtBlue := zi div 256;
              if (z < 0) then p0[i].rgbtBlue := p0[i].rgbtBlue + 128;
          end;
      end;
   end;
   MyPNG := TPNGImage.Create;
   MyPNG.Assign(Bitmap);
   Bitmap.Free;
   MyPNG.CompressionLevel := 7;
   MyPNG.SaveToFile(ChangeFileExt(DEMGlb[MapDraw.DEMonMap].DEMFileName,'.png'));
   MyPNG.Free;
end;

procedure TMapForm.USGSASCII1Click(Sender: TObject);
begin
   {$IfDef ExOddballDEMexports}
   {$Else}
      DEMGlb[MapDraw.DEMonMap].SaveAsUSGSASCII;
   {$EndIf}
end;

procedure TMapForm.Outlineregionofinterest1Click(Sender: TObject);
begin
   ChangeDEMNowDoing(SubsetByOutline);
end;

procedure TMapForm.OutlineregiontoreplacevalueswithreferenceDEM1Click(Sender: TObject);
begin
   ChangeDEMNowDoing(ReplaceValuesByOutline);
end;

procedure TMapForm.Outsideselectedpercentilerange1Click(Sender: TObject);
begin
   Markasmissing1Click(Sender);
end;

procedure TMapForm.Outsideselectedrange1Click(Sender: TObject);
begin
   Markasmissing1Click(Sender);
end;

procedure TMapForm.Outlineholes1Click(Sender: TObject);
begin
   ChangeDEMNowDoing(SubsetHole);
end;

procedure TMapForm.Outlinelakes1Click(Sender: TObject);
begin
   ChangeDEMNowDoing(SubsetLake);
end;

procedure TMapForm.Outlinemaparea1Click(Sender: TObject);
var
   sfc : tShapeFileCreation;
   fName : PathStr;
   x,y : integer;
   Lat,Long : float64;
begin
   FName := ExtractFilePath(LastDataBase);
   if GetFileNameDefaultExt('Shape file for map outline',DBNameMask,FName,false) then begin
      sfc := tShapeFileCreation.Create(WGS84DatumConstants,fName,true,5);
      if Sender = MapBoundingBox1 then begin
         sfc.AddBoundBoxToShapeStream(MapDraw.GetBoundBoxGeo);
      end
      else begin
         x := 0;
         while x <= pred(MapDraw.MapXSize) do begin
             MapDraw.ScreenToLatLongDegree(x,Pred(MapDraw.MapYSize),Lat,Long);
             sfc.AddPointToShapeStream(Lat,Long);
             inc(x,25);
         end;
         y := pred(MapDraw.MapYSize);
         while y >= 0 do begin
             MapDraw.ScreenToLatLongDegree(Pred(MapDraw.MapXSize),y,Lat,Long);
             sfc.AddPointToShapeStream(Lat,Long);
             dec(y,25);
         end;
         x := pred(MapDraw.MapXSize);
         while x >= 0 do begin
             MapDraw.ScreenToLatLongDegree(x,0,Lat,Long);
             sfc.AddPointToShapeStream(Lat,Long);
             dec(x,25);
         end;
         y := 0;
         while y <= pred(MapDraw.MapYSize) do begin
             MapDraw.ScreenToLatLongDegree(0,y,Lat,Long);
             sfc.AddPointToShapeStream(Lat,Long);
             inc(y,25);
         end;
      end;
      sfc.ProcessRecordForShapeFile;
      sfc.CloseShapeFiles;
   end;
end;


procedure TMapForm.Outlineothermaps1Click(Sender: TObject);
var
   Bitmap : tMyBitmap;
   xc,yc,i : integer;

   procedure Outline(MapCorners : CurScreenMapType);
   begin
       MapDraw.LatLongBox(Bitmap,MapCorners.BoundBoxGeo.ymin, MapCorners.BoundBoxGeo.xmin, MapCorners.BoundBoxGeo.ymax, MapCorners.BoundBoxGeo.xmax, 10,xc,yc);
   end;

begin
   CopyImageToBitmap(Image1,Bitmap);
   Bitmap.Canvas.Pen.Color := clRed;
   Bitmap.Canvas.Pen.Width := 3;
   for i := 0 to pred(WMDEM.MDIChildCount) do
      if WMDEM.MDIChildren[i] is tMapForm then begin
         if (WmDEM.MDIChildren[i].Handle <> Handle) then begin
            outline((WMDEM.MDIChildren[i] as TMapForm).MapDraw.MapCorners);
         end;
      end;
    Image1.Picture.Graphic := Bitmap;
    FreeAndNil(Bitmap);
end;


procedure TMapForm.HorizontalDEMshifts2Click(Sender: TObject);
var
   SubDEM : integer;
begin
   if GetDEM(SubDEM,false,'shift over ' + DEMGlb[MapDraw.DEMonMap].AreaName) then begin
      if (SubDEM = MapDraw.DEMonMap) then begin
         MessageToContinue('Requires a different DEM');
         exit;
      end;
   end;
   GetOptimalLagParameters(Self,SubDEM);
end;


procedure TMapForm.Horizontalearthcurvature1Click(Sender: TObject);
{$IfDef ExMapReports}
begin
{$Else}
var
   xutm,yutm,lat,long,Azimuth,BearingAngle,Distance,OutDist,
   xutm2,yutm2,lat2,long2 : float64;
   xp,yp : integer;
   Results : tStringList;
begin
   Azimuth := 45;
   ReadDefault('Azimuth',Azimuth);
   DEMGlb[MapDraw.DEMonMap].DEMGridToUTM(0,0,xutm,yutm);
   DEMGlb[MapDraw.DEMonMap].DEMGridToLatLongDegree(0,0,Lat,Long);
   Results := tStringList.Create;
   Results.Add('Point: ' + LatLongDegreeToString(Lat,Long,MDDef.OutPutLatLongMethod));
   Results.Add('Grid true angle: ' + RealToString(MapDraw.UTMGridToTrueNorthAngle(Lat,Long),6,2));
   Results.Add('');
   Results.Add('UTM Distance  UTM Azimuth  True Distance  True Azimuth');
   Results.Add('=======================================================');
   OutDist := 5000;
   repeat
      xutm2 := xutm + OutDist * SinDeg(Azimuth);
      yutm2 := yutm + OutDist * CosDeg(Azimuth);
      DEMGlb[MapDraw.DEMonMap].UTMtoLatLongDegree(xutm2,yutm2,Lat2,Long2);
      VincentyCalculateDistanceBearing(Lat,Long,Lat2,Long2,Distance,BearingAngle);
      Results.Add(RealToString(OutDist,12,1) + RealToString(Azimuth,12,2) + RealToString(Distance,12,1) + RealToString(BearingAngle,12,2) );
      MapDraw.LatLongDegreeToScreen(Lat2,Long2,xp,yp);
      Petmar.ScreenSymbol(Image1.Canvas,xp,yp,Box,3,claRed);
      OutDist := OutDist + 5000;
   until not (DEMGlb[MapDraw.DEMonMap].LatLongDegreeInDEM(Lat2,Long2));
   PETMAR.DisplayAndPurgeStringList(Results,'Horizontal earth curvature');
{$EndIf}
end;


procedure TMapForm.Horizontalshift1Click(Sender: TObject);
var
   xpost,ypost : integer;
   fName : PathStr;
begin
   {$IfDef RecordEditsDEM} WriteLineToDebugFile('TMapForm.Horizontalshift1Click in, ' + DEMGlb[MapDraw.DEMonMap].SWcornerString); {$EndIf}
   xpost := 0;
   ReadDefault('Posts to shift east-west',xpost);
   ypost := 0;
   ReadDefault('Posts to shift north-south',ypost);
   DEMGlb[MapDraw.DEMonMap].DEMheader.DEMSWCornerX := DEMGlb[MapDraw.DEMonMap].DEMheader.DEMSWCornerX + xpost * DEMGlb[MapDraw.DEMonMap].DEMheader.DEMxSpacing;
   DEMGlb[MapDraw.DEMonMap].DEMheader.DEMSWCornerY := DEMGlb[MapDraw.DEMonMap].DEMheader.DEMSWCornerY + ypost * DEMGlb[MapDraw.DEMonMap].DEMheader.DEMySpacing;
   {$IfDef RecordEditsDEM}  WriteLineToDebugFile('x shift=' + IntToStr(xpost) + '  y shift=' + IntToStr(ypost) + '   ' + DEMGlb[MapDraw.DEMonMap].SWcornerString): {$EndIf}
   fName := MDTempDir + 'horiz_shift_x_' + IntToStr(xpost) + '_y_' + IntToStr(ypost) + DEMGlb[MapDraw.DEMonMap].AreaName + '.dem';
   DEMGlb[MapDraw.DEMonMap].WriteNewFormatDEM(fname);
   DEMGlb[MapDraw.DEMonMap].DEMAlreadyDefined := false;
   ReloadDEMClick(Nil);
   {$IfDef RecordEditsDEM}  WriteLineToDebugFile('TMapForm.Horizontalshift1Click out, ' + DEMGlb[MapDraw.DEMonMap].SWcornerString); {$EndIf}
end;

procedure TMapForm.Maparea1Click(Sender: TObject);
begin
   PopUpMenu11.Popup(Mouse.CursorPos.X,Mouse.CursorPos.Y);
end;

procedure TMapForm.Mapboundingbox1Click(Sender: TObject);
begin
   Outlinemaparea1Click(Sender);
end;

procedure TMapForm.Mapdirectsolarillumination1Click(Sender: TObject);
begin
   DEMOptions.SetHorizonOptions(Self,RightClickLat,RightClickLong);;
end;

procedure TMapForm.USGSquadnames1Click(Sender: TObject);
begin
   USGSquadnames1.Checked := not USGSquadnames1.Checked;
end;

procedure TMapForm.UsingVDATUM1Click(Sender: TObject);
var
   SaveName : PathStr;
begin
   SaveName := '';
   VerticalDatumShiftWithGDAL(MapDraw.DEMonMap,SaveName);
end;

procedure TMapForm.UsingVDATUMoutput1Click(Sender: TObject);
begin
   VerticalDatumShiftWithVDATUM('DEM edit',MapDraw.DEMonMap,0,'');
end;


(*
procedure TMapForm.LoadDataBaseFileWithMissingMessage(fName : PathStr);
begin
   if FileExists(fName) then begin
      LoadDataBaseFile(fName);
   end
   else begin
      MessageToContinue('File missing: ' + fName);
   end;
end;
*)

procedure TMapForm.UStornadoes1Click(Sender: TObject);
begin
   {$IfDef ExGeography}
   {$Else}
      LoadDataBaseFile(ClimateDir + 'tornado_2017\torn.dbf');
  {$EndIf}
end;

procedure TMapForm.Viewshedalgorithms1Click(Sender: TObject);
begin
  ChangeDEMNowDoing(CompareFanAlgorithms);
end;

procedure TMapForm.VISandNIRsurfacebands1Click(Sender: TObject);
var
   ThisOne : integer;
begin
   {$IfDef ExAdvancedSats}
   {$Else}
      if FindOpenMultigrid(ThisOne) then begin
         OpenLandsatOrSentinel2Multigrid(ThisOne,MapDraw.SatOnMap,false);
      end;
   {$EndIf}
end;

procedure TMapForm.Gazetteerlegend1Click(Sender: TObject);
var
   Bitmap : tMyBitmap;
begin
   MapDraw.GazetteerLegend(Bitmap);
   Bitmap.Free;
end;


procedure TMapForm.GDALcontourshapefile1Click(Sender: TObject);
begin
   ContoursShapefile1Click(Sender);
end;

procedure TMapForm.GDALgridsubsettomatchthismap1Click(Sender: TObject);
begin
   GDALsubsetGridAndOpen(MapDraw.MapCorners.BoundBoxGeo,true,'',true);
end;


procedure GDAL_rasterize_shapefile(Outfile,Shapefile: PathStr; bb: sfBoundBox; x,y : float32);
var
   cmd : shortstring;
begin
   if IsGDALFilePresent(GDAL_rasterize) then begin
      cmd :=  GDAL_rasterize + ' -burn 1'   + ' -te ' +  GDALextentBoxGeo(bb)  + ' -ts ' + RealToString(x,-8,-2) + ' ' + RealToString(y,-8,-2) + ' ' +
         ShapeFile + ' ' + OutFile;
      WinExecAndWait32(cmd);
   end;
end;


procedure TMapForm.GDALrasterizehapfiels1Click(Sender: TObject);
var
   i : integer;
   OutFile : shortString;
begin
   for i := 1 to MaxDataBase do begin
      if ValidDB(i) then begin
         OutFile := ChangeFileExt(GISdb[i].dbFullName,'.tif');
         GDAL_rasterize_shapefile(Outfile,GISdb[i].dbFullName,MapDraw.MapCorners.BoundBoxGeo,1,1);
      end;
   end;
end;

procedure TMapForm.BlueMarble1Click(Sender: TObject);
begin
   DEMelevationcolors1Click(Sender);
end;

procedure TMapForm.Globalmonthlyrain1Click(Sender: TObject);
begin
   KoppenClimateStations1Click(Globalmonthlyrain1);
end;

procedure TMapForm.Globalmonthlytemperatures1Click(Sender: TObject);
begin
   KoppenClimateStations1Click(Sender);
end;

procedure TMapForm.Globalmonthlywinds1Click(Sender: TObject);
{$IfDef ExGeography}
begin
{$Else}
begin
   {$IfDef RecordGISDB} WriteLineToDebugFile('TMapForm.Globalmonthlywinds1Click in'); {$EndIf}
   //if MDDef.MoveGeographyDBMemory then DesiredDBMode := dbmCDS;
   WindsDB := OpenDBonMap('',GlobalWindsFName,true,(Sender <> Nil));
   {$IfDef RecordGISDB} WriteLineToDebugFile('WindsDB opened'); {$EndIf}

   GISdb[WindsDB].ClearGISFilter;
   GISdb[WindsDB].dbOpts.DirField := 'JAN_V_MS';
   GISdb[WindsDB].dbOpts.MagField := 'JAN_U_MS';
   GISdb[WindsDB].dbOpts.WindAutoSpace := true;
   GISdb[WindsDB].dbOpts.WindPixelSpace := 30;
   GISdb[WindsDB].dbOpts.VectorLineMult := 4;
   GISdb[WindsDB].dbOpts.LineWidth := 1;
   GISdb[WindsDB].dbOpts.GISVectorsMaxSpeed := 10;
   GISdb[WindsDB].dbOpts.GISVectorsByMaxSpeed := true;
   GISdb[WindsDB].dbOpts.DBLegendLocation.DrawItem := true;
   GISdb[WindsDB].dbOpts.DBLegendLocation.MapPosition := lpNEMap;
   GISdb[WindsDB].LegendCaption := 'Wind speed (m/s)';
   MDDef.PlotArrowHead := true;
   {$IfDef RecordGISDB} WriteLineToDebugFile('TMapForm.Globalmonthlywinds1Click call GISProportionalSymbols'); {$EndIf}
   GISdb[WindsDB].dbOpts.dbAutoShow := dbasVector;
   if Sender = Nil then GISdb[WindsDB].RedrawLayerOnMap
   else GISdb[WindsDB].GISProportionalSymbols(dbasVector);
   {$IfDef RecordGISDB} WriteLineToDebugFile('TMapForm.Globalmonthlywinds1Click out'); {$EndIf}
{$EndIf}
end;

procedure TMapForm.Monthlyclimatologies1Click(Sender: TObject);
begin
   wmDEM.PopUpMenu8.Popup(Mouse.CursorPos.X,Mouse.CursorPos.Y);
end;

procedure TMapForm.Monthlygraphatpoint1Click(Sender: TObject);
var
   i : integer;
begin
   {$IfDef ExMultigrid}
   {$Else}
      for i := 1 to MaxMultiGrid  do begin
         if (MultiGridArray[i] <> Nil) and (MultiGridArray[i].NumGrids = 12) then begin
            MultiGridArray[i].AnnualParameterGraph(RightClickLat,RightClickLong);
         end;
      end;
   {$EndIf}
end;


procedure TMapForm.Monthlytemperatureranges1Click(Sender: TObject);
var
   Graph : TThisBaseGraph;
begin
   {$IfDef ExMultigrid}
   {$Else}
      {$IfDef RecordGeography} WriteLineToDebugFile('TMapForm.Monthlytemperatureranges1Click'); {$EndIf}
      OpenDailyTemps(false);
      if (MaxTempMG <> 0) and (TempMG <> 0) and (MinTempMG <> 0) then begin
         Graph := MultipleMultigridsAnnualParamrGraph(TempMG,MinTempMG,MaxTempMG,RightClickLat,RightClickLong);
         Graph.GraphDraw.MaxVertAxis := 45;
         Graph.GraphDraw.MinVertAxis := -20;
         Graph.GraphDraw.MaxHorizAxis := 12;
         Graph.GraphDraw.MinHorizAxis := 1;
         Graph.GraphDraw.VertLabel := 'Min/mean/max Temp (' + DegSym + 'C)';
         Graph.RedrawDiagram11Click(Nil);
      end;
   {$EndIf}
end;



procedure TMapForm.Monthlyinsolation1Click(Sender: TObject);
begin
   MonthlyInsolationGraph(MapDraw.DEMonMap,RightClickLat,RightClickLong);
end;

procedure TMapForm.Shiftfilecomparison1Click(Sender: TObject);
var
   LocalToWGS84,WGS84toEGM2008 : integer;

   procedure DoOne(fName : PathStr);
   begin
      Geoid2008FName := fName;
      LoadDatumShiftGrids(LocalToWGS84,WGS84toEGM2008);
      //need to add code here to do the datum shift for the grid in question
      CloseSingleDEM(LocalToWGS84);
      CloseSingleDEM(WGS84toEGM2008);
   end;


begin
   SaveBackupDefaults;
   TemporaryNewGeotiff := false; //so transformation grid will be untiled, uncompressed
   GeoidWGS84ellipsoidToLocalVDatum := 'd:\gis_software\OSGeo4W\share\proj\us_noaa_g2012bu0.tif';
   DoOne('D:\geoid\egm2008-1.tif');
   LoadDatumShiftGrids(LocalToWGS84,WGS84toEGM2008);
   DoOne('D:\geoid\egm2008-2.5.tif');
   RestoreBackupDefaults;
end;

procedure TMapForm.Shipmotion1Click(Sender: TObject);
{$IfDef ExLabs}
begin
{$Else}
var
   fName,pName : PathStr;
   db : integer;
begin
   {$IfDef RecordGISDB} WriteLineToDebugFile('TMapForm.Shipmotion1Click in'); {$EndIf}
   pName := 'cliwoc_ships_all_v6';
   fName := DBDir + pName + DefaultDBExt;
   //if not FileExists(fName) then DownloadFileFromWeb(WebDataDownLoadDir + pName + '.dbf',fName);
   //if MDDef.MoveGeographyDBMemory then DesiredDBMode := dbmCDS;
   db := OpenDBonMap('',FName);
   GISdb[db].ClearGISFilter;
   GISdb[db].dbOpts.DirField := 'HEADING';
   GISdb[db].dbOpts.MagField := 'KNOTS';
   GISdb[db].dbOpts.DBAutoShow := dbasVector;
   GISdb[db].dbOpts.VectorLineMult := 2;
   GISdb[db].dbOpts.LineWidth := 1;
   MDDef.PlotArrowHead := true;
   GISdb[db].dbOpts.GISVectorsByMaxSpeed := true;
   GISdb[db].dbOpts.GISVectorsMaxSpeed := 8;
   AddOverlay(Self,ovoDatabases);
   GISdb[db].RedrawLayerOnMap;
   {$IfDef RecordGISDB} WriteLineToDebugFile('TMapForm.Shipmotion1Click out'); {$EndIf}
{$EndIf}
end;


procedure TMapForm.Weather1Click(Sender: TObject);
{$IfDef ExLabs}
begin
{$Else}
var
   fName,pname : PathStr;
begin
   pname := 'cliwoc_weather_all_v3';
   fName := DBDir + pName + DefaultDBExt;
   //if not FileExists(fName) then DownloadFileFromWeb( WebDataDownLoadDir + pName + '.dbf',fName);
   //if MDDef.MoveGeographyDBMemory then DesiredDBMode := dbmCDS;
   LoadDataBaseFile(fName);
{$EndIf}
end;


procedure TMapForm.Globaltopography1Click(Sender: TObject);
begin
   DEMelevationcolors1Click(Sender);
end;


procedure TMapForm.GlobalTopoOverlay1Click(Sender: TObject);
begin
   DEMelevationcolors1Click(Sender);
end;


procedure TMapForm.GNDVIvegetation1Click(Sender: TObject);
begin
   NewSatWindow(nsbGDVI);
end;

procedure TMapForm.Evapotranspirationprecipitationwaterbudget1Click(Sender: TObject);
begin
   MakePOTETgraph(MapDraw.DEMonMap,RightClickLat,RightClickLong);
end;

procedure TMapForm.Everythingabovecutoff1Click(Sender: TObject);
begin
   Markasmissing1Click(Sender);
end;

procedure TMapForm.Everythingbelowcutoff1Click(Sender: TObject);
begin
   Markasmissing1Click(Sender);
end;

procedure TMapForm.Everythingexceptsinglevalue1Click(Sender: TObject);
begin
   Markasmissing1Click(Sender);
end;

procedure TMapForm.Predictedseafloorages1Click(Sender: TObject);
begin
   DEMelevationcolors1Click(Sender);
end;

procedure TMapForm.Other1Click(Sender: TObject);
begin
    DEMGlb[MapDraw.DEMonMap].DEMheader.VerticalCSTypeGeoKey := 0;
end;

procedure TMapForm.OtherDEM1Click(Sender: TObject);
begin
   DEMelevationcolors1Click(Sender);
end;

procedure TMapForm.Vectoroutlines2Click(Sender: TObject);
begin
   DEMelevationcolors1Click(Sender);
end;

procedure TMapForm.NLCD19921Click(Sender: TObject);
begin
   ChangeElevUnits(NLCD1992);
end;


procedure TMapForm.NLCD20011Click(Sender: TObject);
begin
   ChangeElevUnits(NLCD2001up);
end;

procedure TMapForm.NLCDLegend1Click(Sender: TObject);
begin
    MakeNLCDLegend;
end;


function TMapForm.MakeNLCDLegend(theLabel : shortstring = ''; Stats : tstringlist = nil) : integer;
begin
   Result := MakeAnNLCDLegend(MapDraw.DEMonMap,theLabel,Stats);
end;



procedure TMapForm.Rivers1Click(Sender: TObject);
begin
   LoadDataBaseFile(RiversGISFileName);
end;


procedure TMapForm.Rivers2Click(Sender: TObject);
begin
   {$IfDef AllowUSNAdataDownloads}
      if not FileExists(RiversFile) then GetNaturalEarthData;
   {$EndIf}
   LoadDataBaseFile(RiversFile);
end;


function TMapForm.ComputeRMSE : float64;
var
   x,y,n : integer;
   z : float32;
   Sum : float64;
begin
   with DEMGlb[MapDraw.DEMonMap] do begin
      StartProgress('RMSE');
      Sum := 0;
      n := 0;
      for x := 0 to pred(DEMheader.NumCol) do begin
         if (x Mod 100 = 0) then UpdateProgressBar(x/DEMheader.NumCol);
         for y := 0 to pred(DEMheader.NumRow) do begin
            if GetElevMeters(x,y,z) then begin
               inc(n);
               Sum := Sum + sqr(z);
            end;
         end {for y};
      end {for x};
      Result := sqrt(Sum/n);
   end;
end;

procedure TMapForm.RMSE1Click(Sender: TObject);
begin
   MessageToContinue('RMSE=' +RealToString(ComputeRMSE,-12,-2));
end;


procedure TMapForm.OverlayContourFromSecondDEM(DEM : integer; z : float64; color : tPlatformcolor);
{$IfDef ExGeostats}
begin
{$Else}
var
  Col,Row,
  x,y : integer;
  Lat,Long : float64;
begin
   for x := 0 to pred(MapDraw.MapXSize) do begin
      for y := 0 to pred(MapDraw.MapYSize) do begin
          MapDraw.ScreenToLatLongDegree(x,y,Lat,Long);
          DEMGlb[DEM].LatLongDegreeToDEMGridInteger(Lat,long,col,row);
          if DEMGlb[DEM].ContourLineCrossing(Col,Row,z) then begin
             Petmar.ScreenSymbol(Image1.Canvas,x,y,FilledBox,1,color);
          end;
      end;
   end;
{$EndIf}
end;


procedure TMapForm.Singlecontour1Click(Sender: TObject);
var
  z : float64;
begin
   if GetSecondDEM(false) then begin
      z := 0;
      ReadDefault('Contour line to locate',z);
      OverlayContourFromSecondDEM(MapDraw.DEM2onMap,z,claRed);
   end;
end;


procedure TMapForm.Singlepointpits1Click(Sender: TObject);
begin
   Markasmissing1Click(Sender);
end;


procedure TMapForm.Singlevalue1Click(Sender: TObject);
begin
   Markasmissing1Click(Sender);
end;


procedure TMapForm.Singlevalue2Click(Sender: TObject);
begin
   Rangetosinglevalue1Click(Sender);
end;

procedure TMapForm.Pointsabove1Click(Sender: TObject);
begin
   MaskFromSecondGrid(0,msAboveSecond);
end;

procedure TMapForm.Pointsalongsurveylines1Click(Sender: TObject);
begin
   Lineshapefile1Click(Sender);
end;

procedure TMapForm.Pointsbelow1Click(Sender: TObject);
begin
   MaskFromSecondGrid(0,msBelowSecond);
end;

procedure TMapForm.Pointselectionalgorithms1Click(Sender: TObject);
begin
   ChangeDEMNowDoing(FirstPointSelectionAlgorithm);
end;

procedure TMapForm.Pointsfromdatabase1Click(Sender: TObject);
var
   DB,Col,Row,i,NPts : Integer;
   Lat,Long : float64;
   z : float32;
begin
   DB := PickOpenGISDataBase('Mark points missing',0,1);
   if (DB > 0) then begin
      GISdb[DB].MyData.First;
      StartProgress('Missing');
      i := 0;
      NPts := 0;
      while not GISdb[DB].MyData.eof do begin
         inc(i);
         if (i mod 25 = 0) then UpDateProgressBar(i/GISdb[DB].MyData.FiltRecsInDB);
         if GISdb[DB].ValidLatLongFromTable(Lat,Long) then begin
            DEMGlb[MapDraw.DEMonMap].LatLongDegreeToDEMGridInteger(Lat,Long,Col,Row);
            if DEMGlb[MapDraw.DEMonMap].GetElevMetersOnGrid(Col,Row,z) then begin
               DEMGlb[MapDraw.DEMonMap].SetGridMissing(Col,Row);
               inc(NPts);
            end;
         end;
         GISdb[DB].MyData.Next;
      end;
      RespondToChangedDEM;
      MessageToContinue('Points removed: ' + IntToStr(NPts));
   end;
end;

procedure TMapForm.Editshapefilegroup1Click(Sender: TObject);
begin
   LoadDataBaseFile(MapDraw.MapOverlays.ovShapeFileGroup);
end;

procedure TMapForm.Show1Click(Sender: TObject);
var
   Bitmap : tMyBitmap;
   grid_posting_options : Grid_Postings_Form.Tgrid_posting_options;
begin
   CopyImageToBitmap(Image1,Bitmap);
   MapDraw.DEMGridLabel := (Sender = Nil);
   if (Sender = GridPosts2) then begin
      grid_posting_options := Tgrid_posting_options.Create(Application);
      grid_posting_options.CheckBox1.Checked := MapDraw.DEMGridLabel;
      grid_posting_options.MapOwner := Self;
      Petmar.SymbolOnButton(grid_posting_options.BitBtn1,MapDraw.DEMGridSym,MapDraw.DEMGridSymSize,MapDraw.DEMGridSymColor);
      grid_posting_options.ShowModal;
      MapDraw.DEMGridRedraw := (grid_posting_options.RadioGroup1.ItemIndex = 1);
      MapDraw.DEMGridLabel := grid_posting_options.CheckBox1.Checked;
      if (grid_posting_options.RadioGroup1.ItemIndex = 2) then begin
         DoFastMapRedraw;
         exit;
      end;
      grid_posting_options.Free;
   end;
   MapDraw.ShowDEMGrid(Bitmap);
   Image1.Picture.Graphic := Bitmap;
   Bitmap.Free;
end;

procedure TMapForm.Label2Click(Sender: TObject);
begin
   Show1Click(Nil);
end;


procedure TMapForm.CurrentsubsetGeotiff1Click(Sender: TObject);
var
   fName : PathStr;
begin
   {$IfDef RecordSave} WriteLineToDebugFile('TMapForm.CurrentsubsetGeotiff1Click'); {$EndIf}
   fName := WriteDEMDir + 'sub-' + DEMGlb[MapDraw.DEMonMap].AreaName;
   GetFileNameDefaultExt('subset DEM','DEM Tiff|*.tif',fName);
   DEMGlb[MapDraw.DEMonMap].SaveGridSubsetGeotiff(MapDraw.MapAreaDEMGridLimits,fName);
   if AnswerIsYes('Load subset ' + ExtractFilename(fName)) then OpenNewDEM(fName);
end;


procedure TMapForm.CurrentsubsetMDDEM1Click(Sender: TObject);
var
   fName : PathStr;
   NewGrid : integer;
   What : tCreateGrid;
begin
   {$IfDef RecordSave} WriteLineToDebugFile('TMapForm.CurrentsubsetMDDEM1Click'); {$EndIf}
   if (DEMGlb[MapDraw.DEMonMap].SelectionMap.MapDraw.BasicProjection in [bpUTM,bpLatLong]) then begin
     fName := WriteDEMDir + 'sub-' + DEMGlb[MapDraw.DEMonMap].AreaName;
     GetFileNameDefaultExt('subset DEM','DEM|*.dem',fName);
     DEMGlb[MapDraw.DEMonMap].WriteNewFormatDEM(MapDraw.MapAreaDEMGridLimits,fName);
     if AnswerIsYes('Load subset ' + ExtractFilename(fName)) then OpenNewDEM(fName);
   end
   else begin
      if AnswerIsYes('Lat/long grid (vice UTM)') then What := cgLatLong else What := cgUTM;
      NewGrid := CreateGridToMatchMap(What);
      DEMGlb[NewGrid].FillHolesSelectedBoxFromReferenceDEM(DEMGlb[NewGrid].FullDEMGridLimits,MapDraw.DEMonMap,hfOnlyHole);
      DEMGlb[NewGrid].SelectionMap.RespondToChangedDEM;
   end;
end;

procedure TMapForm.Volume1Click(Sender: TObject);
begin
   SpeedButton5Click(Sender);
end;


procedure TMapForm.Pointmodedigitizing1Click(Sender: TObject);
begin
   MDDef.DigitizeMode := dmPoint;
   MouseIsDown := false;
end;

procedure TMapForm.Pointparameters1Click(Sender: TObject);
var
   Col,Row : integer;
begin
   if DEMGlb[MapDraw.DEMonMap].LatLongDegreeToDEMGridInteger(RightClickLat,RightClickLong,Col,Row) then DEMGlb[MapDraw.DEMonMap].PointParameters(Col,Row);
end;

procedure TMapForm.Streamdigitizing1Click(Sender: TObject);
begin
   MDDef.DigitizeMode := dmStream;
   MouseIsDown := false;
end;


procedure TMapForm.Streammodetolerance1Click(Sender: TObject);
begin
  ReadDefault('Stream mode tolerance, pixels',MDDef.ContDigitizeSeparation);
  StreamModeTolerance1.Caption := 'Stream mode tolerance, ' + IntToStr(MDDef.ContDigitizeSeparation);
end;


procedure TMapForm.N16bitBSQ1Click(Sender: TObject);
begin
   {$IfDef ExOddballDEMexports}
   {$Else}
      DEMGlb[MapDraw.DEMonMap].Save16BitBSQ;
   {$EndIf}
end;


procedure TMapForm.N1pixel1Click(Sender: TObject);
var
   Col,Row : integer;
begin
   DEMGlb[MapDraw.DEMonMap].LatLongDegreeToDEMGridInteger(RightClickLat,RightClickLong,col,row);
   pt_cloud_opts_fm.ExtractPointsToDataBaseForPixel(MapDraw.DEMonMap,Col,Row,1);
end;


procedure TMapForm.N1pixel2Click(Sender: TObject);
var
   Col,Row : integer;
begin
   LoadDEMsCoveringPoint(RightClickLat,RightClickLong);
   DEMGlb[MapDraw.DEMonMap].LatLongDegreeToDEMGridInteger(RightClickLat,RightClickLong,col,row);
   pt_cloud_opts_fm.ExtractPointsToDataBaseForPixel(MapDraw.DEMonMap,Col,Row,0);
end;


procedure TMapForm.N3x3neighborhood1Click(Sender: TObject);
begin
   {$IfDef ExGeoStats}
   {$Else}
      ModeFilterDEM(MapDraw.DEMonMap,1,true);
   {$EndIf}
end;

procedure TMapForm.N3x3neighborhood2Click(Sender: TObject);
begin
   {$IfDef ExGeoStats}
   {$Else}
      ModeFilterDEM(MapDraw.DEMonMap,1,false);
   {$EndIf}
end;


procedure TMapForm.N3x3region1Click(Sender: TObject);
begin
   CreateRoughnessSlopeStandardDeviationMap(MapDraw.DEMonMap,3);
end;

procedure TMapForm.ForceAllRedraw1Click(Sender: TObject);
var
   i : integer;
begin
   for i := 0 to pred(WMDEM.MDIChildCount) do
      if WMDEM.MDIChildren[i] is tMapForm and (WmDEM.MDIChildren[i].Handle <> Handle) then begin
         (WMDEM.MDIChildren[i] as TMapForm).DoCompleteMapRedraw;
      end;
end;

procedure TMapForm.N51Click(Sender: TObject);
begin
   SagaTRIMap(DEMGlb[MapDraw.DEMonMap].SelectionMap.GeotiffDEMNameOfMap);
end;

procedure TMapForm.PerpProfiles1Click(Sender: TObject);
begin
   DrawProfilesThroughPeak(MapDraw.DEMonMap,RightClickLat,RightClickLong);
end;

procedure TMapForm.N5x5neighborhhhod1Click(Sender: TObject);
begin
   {$IfDef ExGeoStats}
   {$Else}
      ModeFilterDEM(MapDraw.DEMonMap,2,false);
   {$EndIf}
end;

procedure TMapForm.N5x5neighborhood1Click(Sender: TObject);
begin
   {$IfDef ExGeoStats}
   {$Else}
      ModeFilterDEM(MapDraw.DEMonMap,2,true);
   {$EndIf}
end;

procedure TMapForm.N5x5region1Click(Sender: TObject);
begin
   CreateRoughnessSlopeStandardDeviationMap(MapDraw.DEMonMap,5);
end;


function TMapForm.CreateMapAndLegendSideBySide : tMyBitmap;
var
   bmp2 : tMyBitmap;
begin
   SaveBackupDefaults;
   MDDef.GridLegendLocation.DrawItem := false;
   DoCompleteMapRedraw;
   CreateBitmap(Result,1,1);
   Result.LoadFromFile(MapDraw.FullMapfName);
   bmp2 := MapDraw.DrawLegendOnBitmap;
   Result.Width := Result.Width + 25 + Bmp2.Width;
   if (Result.Height < bmp2.Height) then Result.Height := bmp2.Height;
   Result.Canvas.Draw(Result.Width - bmp2.width,0,bmp2);
   bmp2.Free;
   RestoreBackupDefaults;
   DoCompleteMapRedraw;
end;

procedure TMapForm.N60Click(Sender: TObject);
begin
   DisplayBitmap(CreateMapAndLegendSideBySide,'Map with legend',true);
end;


procedure TMapForm.N62Click(Sender: TObject);
var
   ErrorMessage : shortstring;
begin
   LoadLC100LandCover('',MapDraw.MapCorners.BoundBoxGeo,ErrorMessage,true);
end;

procedure TMapForm.N7x7region1Click(Sender: TObject);
begin
   CreateRoughnessSlopeStandardDeviationMap(MapDraw.DEMonMap,7);
end;

procedure TMapForm.N7x7region2Click(Sender: TObject);
begin
   CreateRoughnessSlopeStandardDeviationMap(MapDraw.DEMonMap,9);
end;

procedure TMapForm.NAN1Click(Sender: TObject);
begin
   Markasmissing1Click(Sender);
end;

procedure TMapForm.NASADEMtomatchthismap1Click(Sender: TObject);
//login will be required for the downloads
const
   Buffer = 0;
var
   Lat,Long : integer;
   cmd : ANSIstring;
   ch1,ch2 : char;
begin
   for Long := round(MapDraw.MapCorners.BoundBoxGeo.XMin-Buffer) to round(MapDraw.MapCorners.BoundBoxGeo.XMax+Buffer) do begin
      if Long >= 0 then ch2 := 'e' else ch2 := 'w';
      for Lat := round(MapDraw.MapCorners.BoundBoxGeo.YMin-Buffer) to round(MapDraw.MapCorners.BoundBoxGeo.YMax+Buffer) do begin
         if Lat >= 0 then ch1 := 'n' else ch1 := 's';
         cmd := ch1 + IntegerToString(abs(Lat),2) + ch2 + IntegerToString(abs(Long),3);
         ReplaceCharacter(cmd,' ','0');
         if (Sender =  NASADEMtomatchthismap1) then begin
            cmd := 'https://e4ftl01.cr.usgs.gov//DP132/MEASURES/NASADEM_HGT.001/2000.02.11/NASADEM_HGT_' + cmd + '.zip';
         end
{
https://prism-dem-open.copernicus.eu/pd-desk-open-access/prismDownload/COP-DEM_GLO-30-DGED__2022_1/Copernicus_DSM_10_N29_00_E014_00.tar
}

         else begin
            cmd := 'https://e4ftl01.cr.usgs.gov/ASTT/ASTGTM.003/2000.03.01/ASTGTMV003_' + Uppercase(cmd) + '.zip';
         end;
         ExecuteFile('chrome',cmd,'');
      end;
   end;
end;

procedure TMapForm.NAVD881Click(Sender: TObject);
begin
   DEMGlb[MapDraw.DEMonMap].DEMheader.VerticalCSTypeGeoKey := VertCSNAVD88;
   DEMGlb[MapDraw.DEMonMap].DEMMapProjection.h_DatumCode := 'NAD83';
end;

procedure TMapForm.EGM2008Click(Sender: TObject);
begin
   DEMGlb[MapDraw.DEMonMap].DEMheader.VerticalCSTypeGeoKey := VertCSEGM2008;
   DEMGlb[MapDraw.DEMonMap].DEMMapProjection.h_DatumCode := 'EGM2008';
end;

procedure TMapForm.NBR1Click(Sender: TObject);
begin
   ChangeElevUnits(euNBR);
end;

procedure TMapForm.NBR21Click(Sender: TObject);
begin
   NewSatWindow(nsbNBRNormalizedBurnIndex2);
end;

function TMapForm.NewSatWindow(nsb : tNewSatBand) : integer;
begin
   {$IfDef ExSat}
   {$Else}
      Result := SatImage[MapDraw.SATonMap].MakeNewBand(nsb);
      if FullMapSpeedButton.Enabled then DEMGlb[Result].SelectionMap.SubsetAndZoomMapFromGeographicBounds(MapDraw.MapCorners.BoundBoxGeo);
   {$EndIf}
end;


procedure TMapForm.NDSI1Click(Sender: TObject);
begin
   NewSatWindow(nsbNDSIsoil);
end;

procedure TMapForm.NDSIsnow1Click(Sender: TObject);
begin
   NewSatWindow(nsbNDSIsnow);
end;

procedure TMapForm.NDVI1Click(Sender: TObject);
begin
   NewSatWindow(nsbNDVI);
end;

procedure TMapForm.NDVI3Click(Sender: TObject);
begin
   ChangeElevUnits(euNDVI);
end;

procedure TMapForm.NDVIofperenctilesmonthly1Click(Sender: TObject);
begin
   {$IfDef ExMultigrid}
   {$Else}
      MultiGridArray[MapDraw.MultiGridOnMap].NDVIPercentileTimeSeries(false,true);
   {$EndIf}
end;

procedure TMapForm.NDWI1Click(Sender: TObject);
begin
   NewSatWindow(nsbNDWI);
end;


procedure TMapForm.ViewExifimages1Click(Sender: TObject);
begin
   {$IfDef ExExif}
   {$Else}
      StartExif;
   {$EndIf}
end;

procedure TMapForm.Viewshed1Click(Sender: TObject);
begin
   Viewshed2Click(Sender);
end;


procedure TMapForm.Derivativegrid2Click(Sender: TObject);
begin
   Derivativegrid1Click(Sender);
end;


procedure TMapForm.Descriptions1Click(Sender: TObject);
begin
   {$IfDef ExGeography}
   {$Else}
      CreateKoppenLegend(false);
   {$EndIf}
end;


procedure TMapForm.Histogramallbands1Click(Sender: TObject);
begin
   {$IfDef ExAdvancedSats}
   {$Else}
      SatImage[MapDraw.SATonMap].GraphHistogram(shAllBands,1);
   {$EndIf}
end;


procedure TMapForm.Histogrampickband1Click(Sender: TObject);
{$IfDef ExAdvancedSats}
begin
{$Else}
var
   Band : integer;
begin
   Band := MapDraw.SatView.BandInWindow;
   SatImage[MapDraw.SATonMap].PickBand('Band for histogram',Band);
   SatImage[MapDraw.SATonMap].GraphHistogram(shOneBand,band);
{$EndIf}
end;


procedure TMapForm.Addgrids1Click(Sender: TObject);
begin
   {$IfDef ExAdvancedGIS}
   {$Else}
      Map_Algebra.DoMapAlgebra;
   {$EndIf}
end;

procedure TMapForm.Ratiooftwogrids1Click(Sender: TObject);
begin
   {$IfDef ExGeoStats}
   {$Else}
      DEMStat.GridRatio;
   {$EndIf}
end;


procedure TMapForm.Terraincategories2Click(Sender: TObject);
begin
   Terraincategories1Click(Sender);
end;

procedure TMapForm.Requiredantennaheight2Click(Sender: TObject);
begin
   ChangeDEMNowDoing(FirstRequiredAntenna);
end;

procedure TMapForm.Reregisterimage1Click(Sender: TObject);
{$IfDef RegisterSatelliteImage}
var
   SatelliteForm : TSatelliteForm;
begin
   SatelliteForm := CreateSatelliteImage(MapDraw.SatOnMap);
   SatelliteForm.StartImageRegistration;
   Self.WindowState := wsMinimized;
{$Else}
begin
{$EndIf}
end;


procedure TMapForm.Peakislandarea1Click(Sender: TObject);
begin
   {$IfDef IncludePeakIslandArea} ChangeDEMNowDoing(GetIslandArea); {$EndIf}
end;

function TMapForm.GeotiffDEMNameOfMap : PathStr;
begin
   Result := '';
   if ValidDEM(MapDraw.DEMonMap) then begin
      Result := DEMGLB[MapDraw.DEMonMap].GeotiffDEMName;
   end;
end;


procedure TMapForm.PercentilesofNDVImonthly1Click(Sender: TObject);
begin
   {$IfDef ExMultigrid}
   {$Else}
      MultiGridArray[MapDraw.MultiGridOnMap].NDVIPercentileTimeSeries(true,true);
   {$EndIf}
end;

procedure TMapForm.Percentilestimeseries1Click(Sender: TObject);
begin
   {$IfDef ExMultigrid}
   {$Else}
      MultiGridArray[MapDraw.MultiGridOnMap].NDVIPercentileTimeSeries(true,false);
   {$EndIf}
end;

procedure TMapForm.PercentilsofNDVItimeseries1Click(Sender: TObject);
begin
   {$IfDef ExMultigrid}
   {$Else}
      MultiGridArray[MapDraw.MultiGridOnMap].NDVIPercentileTimeSeries(false,false);
   {$EndIf}
end;

procedure TMapForm.Periods1Click(Sender: TObject);
begin
   {$IfDef ExGeology}
   {$Else}
      MapDraw.MapType := mtElevFromTable;
      ElevationFixedPalette := PeriodsTimeScale;
      DoBaseMapRedraw;
   {$EndIf}
end;

procedure TMapForm.BIL1Click(Sender: TObject);
{$IfDef IncludeBILWrite}
var
   fName : PathStr;
begin
   fName := '';
   DEMGlb[MapDraw.DEMonMap].WriteBILFormatDEM(fName);
{$Else}
begin
{$EndIf}
end;


procedure TMapForm.Gridfloat1Click(Sender: TObject);
{$IfDef IncludeBILWrite}
var
   fName : PathStr;
begin
   fName := '';
   DEMGlb[MapDraw.DEMonMap].WriteGridFloatFormatDEM(fName);
{$Else}
begin
{$EndIf}
end;


procedure TMapForm.GridgraticluetoGoogleEarth1Click(Sender: TObject);
var
   inBitmap : tMyBitmap;
begin
   CloneImageToBitmap(Image1,inBitmap);
   MapDraw.DrawGridLines(inBitmap);
   inBitmap.Free;
   ConvertShapeFileToKML(MapDraw.GraticuleFName,self);
   if MDdef.KMLLabelGraticule then ConvertShapeFileToKML(MapDraw.GraticuleEndsFName,self);
   ConvertShapeFileToKML(MapDraw.UTMgridFName,self);
   if MDdef.KMLLabelGraticule then ConvertShapeFileToKML(MapDraw.UTMgridEndsFName,self);
end;


procedure TMapForm.Gridinfile1Click(Sender: TObject);
begin
   FillHoles(hfEverything);
end;

procedure TMapForm.Gridmaskcolor1Click(Sender: TObject);
begin
   Mask1Click(Sender);
end;

procedure TMapForm.Gridmigration1Click(Sender: TObject);
begin
   HorizontalDEMshifts2Click(Sender);
end;

procedure TMapForm.Gridoutlines1Click(Sender: TObject);
begin
   OutlineGridOutlines;
end;

procedure TMapForm.GridpointsfromsecondDEMAssignAndDraw(SecondDEM : integer);
begin
   MapDraw.AssignSecondDEM(SecondDEM);
   MapDraw.DeleteSingleMapLayer(MapDraw.SecondGridfName);
   DoFastMapRedraw;
   Locationsonly1Click(Nil);
end;


procedure TMapForm.SWNE1Click(Sender: TObject);
begin
   FeatureMigration(cdNE);
end;

procedure TMapForm.SpeedButton12Click(Sender: TObject);
begin
   Saveimage1Click(Sender);
end;

procedure TMapForm.SpeedButton13Click(Sender: TObject);
begin
    ClipboardSpeedButtonClick(Sender);
end;

procedure TMapForm.SpeedButton1Click(Sender: TObject);
begin
  ChangeDEMNowDoing(StreamDistance);
  MDDef.DigitizeMode := dmStream;
end;

procedure TMapForm.FennemanProvinces1Click(Sender: TObject);
var
   Bitmap : tMyBitmap;
begin
   CopyImageToBitmap(Image1,Bitmap);
   US_properties.FennemanOutlines(Self,Bitmap);
   Image1.Picture.Graphic := Bitmap;
   Bitmap.Free;
end;


procedure TMapForm.States1Click(Sender: TObject);
begin
   LoadDataBaseFile(StateGISFileName);
end;

procedure TMapForm.Statistics1Click(Sender: TObject);
begin
   {$IfDef ExVegDensity}
   {$Else}
      with DEMGlb[MapDraw.DEMonMap] do begin
         if (VegDensityLayers[1] <> Nil) then VegDensityLayers[1].GetStats;
         if (VegDensityLayers[2] <> Nil) then VegDensityLayers[2].GetStats;
      end;
   {$EndIf}
end;

procedure TMapForm.Stats1Click(Sender: TObject);
{$IfDef ExLandsatQA}
begin
   MessageToContinue('Disabled while rewriting for collection 2');
{$Else}
var
   sl : tStringList;
begin
   sl := SatImage[Mapdraw.SATonMap].GetLandsatQA(SatImage[Mapdraw.SATonMap].OriginalFileName);
   if (sl <> Nil) then Petmar.DisplayAndPurgeStringList(sl, ExtractFileNameNoExt(SatImage[Mapdraw.SATonMap].OriginalFileName));
   {$EndIf}
end;


procedure TMapForm.StdDevinbox1Click(Sender: TObject);
begin
   ThinDEM1Click(Sender);
end;


procedure TMapForm.StdDevinregion1Click(Sender: TObject);
begin
   StdDevinregion2Click(Sender);
end;

procedure TMapForm.StdDevinregion2Click(Sender: TObject);
var
   Radius : integer;
begin
   Radius := 1;
   ReadDefault('Region size (pixels)',Radius);
   CreateStandardDeviationMap(MapDraw.DEMonMap,Radius);
end;

procedure TMapForm.Dataheader1Click(Sender: TObject);
begin
   {$IfDef ExViewDEMHeader}
   {$Else}
      DEM_Manager.ViewHeaderRecord(MapDraw.DEMonMap);
   {$EndIf}
end;

procedure TMapForm.Dataheader2Click(Sender: TObject);
begin
   if (MapDraw.DEMonMap > 0) then ViewHeaderRecord(MapDraw.DEMonMap);
   {$IfDef ExSat}
   {$Else}
      if (MapDraw.SatOnMap > 0) then DEM_sat_Header.ViewSatHeader(MapDraw.SatOnMap);
   {$EndIf}
end;

procedure TMapForm.Coastlines1Click(Sender: TObject);
var
   db : integer;
begin
   //if not FileExists(CoastLineFile) then GetNaturalEarthData;
   db := LoadDataBaseFile(CoastLineFile);
   if ValidDB(db) then begin
      GISdb[db].dbOpts.LineColor := ConvertTColorToPlatformColor(clBlue);
      GISdb[db].RedrawLayerOnMap;
   end;
end;

procedure TMapForm.Coloredbynumber1Click(Sender: TObject);
begin
   MapDraw.MapType := mtElevRainbow;
   ElevationFixedPalette := '';
   DoBaseMapRedraw;
end;


procedure TMapForm.Editpointelevations1Click(Sender: TObject);
begin
   ChangeDEMNowDoing(EditPointElevs);
end;


procedure TMapForm.DRGanaglyph1Click(Sender: TObject);
begin
   {$IfDef ExExoticMaps}
   {$Else}
      Set3DmapOptions(Self);
   {$EndIf}
end;

procedure TMapForm.Driftmodels1Click(Sender: TObject);
begin
   {$IfDef ExDriftModel}
   {$Else}
      Drift_Model.LoadDriftModel(Self);
   {$EndIf}
end;

procedure TMapForm.Maxmeanmingridsfrompointdata1Click(Sender: TObject);
begin
   DropinthebucketfromGrids;
end;



procedure TMapForm.DropinthebucketfromGrids;
const
   FName : PathStr = '';
var
   MinGrid,MaxGrid,MeanGrid,DensityGrid : integer;
   z,OldZ,Spacing,sum : float32;
   GridChoice : tCreateGrid;
   infile : tStringList;
   inf : system.textfile;
   Line : ANSIString;
   m,i,fSize,x,y     : integer;
   DefaultFilter : byte;
   FilesWanted : TStringList;
   lat,long : float64;
   Table : tMyData;
   zField : ShortString;


          procedure ProcessLine;
          begin
              lat := StrToFloat(Petmar_types.BeforeSpecifiedCharacterANSI(line,',',true,true));
              long := StrToFloat(Petmar_types.BeforeSpecifiedCharacterANSI(line,',',true,true));
              z := StrToFloat(line);
          end;

         procedure FinishDEM(WhichDEM : integer);
         begin
             DEMGlb[WhichDEM].SelectionMap.MapDraw.MapType := mtElevSpectrum;
             DEMGlb[WhichDEM].SelectionMap.RespondToChangedDEM;
         end;


begin
   {$IfDef RecordDropInBucket} WriteLineToDebugFile('TMapForm.DropinthebucketfromGrids in'); {$EndIf}
   if (fName = '') then FName := MainMapData;
   FilesWanted := TStringList.Create;
   FilesWanted.Add(fName);
   DefaultFilter := 1;

   if AnswerIsYes('UTM grid') then begin
      ReadDefault('UTM grid spacing (m)',MDDef.PreferUTMSpace);
      Spacing := MDDef.PreferUTMSpace;
      GridChoice := cgUTM;
   end
   else begin
      ReadDefault('Geographic grid spacing (arc sec)',MDDef.PreferArcSecSpace);
      Spacing := MDDef.PreferArcSecSpace;
      GridChoice := cgLatLong;
   end;

   if GetMultipleFiles('Files to drop in the bucket','Shapefiles|*.shp|Database files|*.db;*.dbf|All files|*.*',FilesWanted,DefaultFilter) then begin
      DensityGrid := CreateGridToMatchMap(GridChoice,true,SmallIntDEM,Spacing);
      DEMGlb[DensityGrid].AreaName := 'pt density';
      DEMGlb[DensityGrid].DEMheader.ElevUnits := Undefined;
      DEMGlb[DensityGrid].SetEntireGridToConstant(0);

      MinGrid := CreateGridToMatchMap(GridChoice,true,FloatingPointDEM,Spacing);
      DEMGlb[MinGrid].AreaName := 'Min';
      DEMGlb[MinGrid].DEMheader.ElevUnits := euMeters;
      DEMGlb[MinGrid].SetEntireGridToConstant(999999);

      MaxGrid := CreateGridToMatchMap(GridChoice,true,FloatingPointDEM,Spacing);
      DEMGlb[MaxGrid].AreaName := 'Max';
      DEMGlb[MaxGrid].DEMheader.ElevUnits := euMeters;
      DEMGlb[MaxGrid].SetEntireGridToConstant(-99999);

      MeanGrid := CreateGridToMatchMap(GridChoice,true,FloatingPointDEM,Spacing);
      DEMGlb[MeanGrid].AreaName := 'Mean';
      DEMGlb[MeanGrid].DEMheader.ElevUnits := euMeters;
      DEMGlb[MeanGrid].SetEntireGridToConstant(0);

      ShowHourglassCursor;
      StartProgress('Drop in bucket');
      for m := 0 to pred(FilesWanted.Count) do begin
         UpdateProgressBar(m/FilesWanted.Count);
         fName := FilesWanted.Strings[m];
         {$IfDef RecordDropInBucket} WriteLineToDebugFile(fname); {$EndIf}
         if FileExtEquals(fName,DefaultDBExt) then begin
            Table := TmyData.Create(fName);
            if (m=0) then zField := PetDBUtils.OrigPickField(Table,'z value field',NumericFieldTypes)
            else begin
              if Not Table.FieldExists(zField) then zField := PetDBUtils.OrigPickField(Table,'z value field',NumericFieldTypes);
            end;
            while not Table.eof do  begin
               Lat := Table.GetFieldByNameAsFloat('LAT');
               Long := Table.GetFieldByNameAsFloat('LONG');
               z := Table.GetFieldByNameAsFloat(zField);
               if (MaxGrid <> 0) or (MinGrid <> 0) or (MeanGrid <> 0) or (DensityGrid <> 0)  then begin
                  DEMGlb[DensityGrid].LatLongDegreeToDEMGridInteger(Lat,Long,x,y);
                  if DEMGlb[DensityGrid].GridInDataSet(x,y) then begin
                     if (MaxGrid <> 0) then begin
                        DEMGlb[MaxGrid].GetElevMeters(x,y,OldZ);
                        if Z > OldZ then DEMGlb[MaxGrid].SetGridElevation(x,y,Z);
                     end;
                     if (MinGrid <> 0) then begin
                        DEMGlb[MinGrid].GetElevMeters(x,y,OldZ);
                        if Z < OldZ then DEMGlb[MinGrid].SetGridElevation(x,y,Z);
                     end;
                     if (MeanGrid <> 0) then begin
                        DEMGlb[MeanGrid].GetElevMeters(x,y,OldZ);
                        DEMGlb[MeanGrid].SetGridElevation(x,y,Z + OldZ);
                     end;
                     if (DensityGrid <> 0) then DEMGlb[DensityGrid].IncrementGridValue(x,y);
                  end;
               end;
               Table.Next;
            end;
            Table.Destroy;
         end
         else begin  //ASCII lat/long z
             fSize := GetFileSize(fName);
             if (fSize <  InMemoryStringSizeLimit) then begin
                infile := tStringList.Create;
                inFile.LoadFromFile(fName);
                for i := 1 to pred(infile.Count) do begin
                   line := infile.Strings[i];
                   ProcessLine;
                end {for i};
                infile.Free;
              end
              else begin
                 AssignFile(inf,fname);
                 reset(inf);
                 readln(inf);
                 while not eof(inf) do  begin
                    readln(inf,line);
                    ProcessLine;
                 end;
                 CloseFile(inf);
              end;
          end;
      end {for m};
      FilesWanted.Free;

      if (DensityGrid <> 0) and (MeanGrid <> 0) then begin
          for x := 0 to pred(DEMGlb[DensityGrid].DEMheader.NumCol) do begin
             for y := 0 to pred(DEMGlb[DensityGrid].DEMheader.NumRow) do begin
                DEMGlb[DensityGrid].GetElevMetersOnGrid(x,y,z);
                if (z < 0.5) then begin
                   DEMGlb[DensityGrid].SetGridMissing(x,y);
                   DEMGlb[MeanGrid].SetGridMissing(x,y);
                   DEMGlb[MinGrid].SetGridMissing(x,y);
                   DEMGlb[MaxGrid].SetGridMissing(x,y);
                end
                else begin
                   DEMGlb[MeanGrid].GetElevMetersOnGrid(x,y,Sum);
                   DEMGlb[MeanGrid].SetGridElevation(x,y,Sum/z);
                end;
             end;
          end;
          FinishDEM(DensityGrid);
          FinishDEM(MeanGrid);
      end;
      if (MaxGrid <> 0) then FinishDEM(MaxGrid);
      if (MinGrid <> 0) then FinishDEM(MinGrid);
   end;
 {$IfDef RecordDropInBucket} WriteLineToDebugFile('TMapForm.DropinthebucketfromGrids out'); {$EndIf}
end;


procedure TMapForm.DSDPODPIODPdrilling1Click(Sender: TObject);
begin
   {$IfDef ExGeology}
   {$Else}
      //if not FileExists(DSDP_db) then DownloadFileFromWeb(WebDataDownLoadDir + ExtractFileName(DSDP_db),DSDP_db);
      LoadDataBaseFile(DSDP_db);
   {$EndIf}
end;

procedure TMapForm.DSMfirstreturn1Click(Sender: TObject);
begin
   MDDef.VegOptionMap := voDSM;
   DoBaseMapRedraw;
end;

(*
procedure TMapForm.UScounty1Click(Sender: TObject);
begin
   ChangeDEMNowDoing(USCounty);
end;
*)

procedure TMapForm.Openimage1Click(Sender: TObject);
begin
   DEMelevationcolors1Click(Sender);
end;

procedure TMapForm.Openleastcostpathgrids1Click(Sender: TObject);
begin
   {$IfDef MICRODEM}
      OpenLeastCostGrids;
   {$EndIf}
end;

procedure TMapForm.Openness1Click(Sender: TObject);
begin
   MapDraw.MapType := mtOpenness;
   DoBaseMapRedraw;
end;

procedure TMapForm.Opennessoptions1Click(Sender: TObject);
begin
   SetGrayscale(OpennessGray,Self);
end;

procedure TMapForm.Downhillvectors1Click(Sender : TObject);
begin
   {$IfDef ExDrainage}
   {$Else}
      if (Sender <> Nil) then begin
          Downhillvectors1.Checked := not Downhillvectors1.Checked;
          MapDraw.RedrawDrainageVectors := Downhillvectors1.Checked;
          if MapDraw.RedrawDrainageVectors then Drainage_opts.SetDrainageDelineationOptions(Self);
      end;
      DoFastMapRedraw;
   {$EndIf}
end;

procedure TMapForm.Downsampling2Click(Sender: TObject);
begin

end;

{$IfDef ExDrainage}
{$Else}
procedure TMapForm.DrawDownhillVectors;
var
    Bitmap : tMyBitmap;
    AspToAverage,
    DrainIncr,i,j,x,y,Col,Row,VecLen : integer;
    AvgAspectDir,
    Mag : float64;
    Readings : farray;
    SlopeAspectRec : tSlopeAspectRec;
begin
   {$IfDef RecordDrainageVectors} WriteLineToDebugFile('TMapForm.DrawDownhillVector in'); {$EndIf}
   if MapDraw.ValidDEMonMap then begin
      SaveBackupDefaults;
      MDDef.SlopeAlg := MDdef.DrainageSlopeAlgorithm;
      CopyImageToBitmap(Image1,Bitmap);
      if ShowSatProgress then StartProgressAbortOption('Grids');

      DrainIncr := 0;
      repeat
         inc(DrainIncr);
      until (MapDraw.MapXSize div(round(MapDraw.MapCorners.BoundBoxDataGrid.xmax - MapDraw.MapCorners.BoundBoxDataGrid.xmin) div DrainIncr)) >= MDdef.DrainageArrowSeparation;

      Col := round(MapDraw.MapCorners.BoundBoxDataGrid.xmin);
      while (Col <= MapDraw.MapCorners.BoundBoxDataGrid.xmax) do begin
         if ShowSatProgress then UpdateProgressBar(Col/DEMGlb[MapDraw.DEMonMap].DEMheader.NumCol);
         Row := round(MapDraw.MapCorners.BoundBoxDataGrid.ymin);
         while (Row <= MapDraw.MapCorners.BoundBoxDataGrid.ymax) do begin
            MapDraw.DEMGridToScreen(Col,Row,x,y);

            if MDDef.DrainagePointSlope then begin
               if DEMGlb[MapDraw.DEMonMap].GetSlopeAndAspect(Col,Row,SlopeAspectRec) then begin
                  VecLen := 4 + round(MDdef.DrainageArrowLength * 2 * SlopeAspectRec.Slope);
                  PlotOrientedLine(Bitmap,x,y,VecLen,SlopeAspectRec.AspectDir,MDdef.DrainageArrowColor,MDdef.DrainageArrowWidth,true);
                  {$IfDef RecordDrainageVectors} WriteLineToDebugFile('Point vector at x=' + IntToStr(x) + '  y=' + IntToStr(y)); {$EndIf}
               end;
            end;

            if MDDef.DrainageVectorAverage then begin
               AspToAverage := 0;
               for i := -MDDef.AspectRegionSize to MDDef.AspectRegionSize do begin
                  for j := -MDDef.AspectRegionSize to MDDef.AspectRegionSize do begin
                     if DEMGlb[MapDraw.DEMonMap].GetSlopeAndAspect(Col+i,Row+j,SlopeAspectRec) then begin
                        Readings[AspToAverage] := SlopeAspectRec.AspectDir;
                        inc(AspToAverage);
                     end;
                  end;
               end;
               if (AspToAverage > 0) then begin
                  AvgAspectDir := Petmath.VectorAverage(AspToAverage,Readings,Mag);
                  VecLen := 4 + round(MDdef.DrainageVectAvgArrowLength * 2 * Mag);
                  PlotOrientedLine(Bitmap,x,y,VecLen,AvgAspectDir,MDdef.DrainageVectAvgArrowColor,MDdef.DrainageVectAvgArrowWidth,true);
                  {$IfDef RecordDrainageVectors} WriteLineToDebugFile('Average vector at x=' + IntToStr(x) + '  y=' + IntToStr(y)); {$EndIf}
               end;
            end;
            inc(Row,DrainIncr);
         end;
         inc(Col,DrainIncr);
         if WantOut then break;
      end;
      if ShowSatProgress then EndProgress;
      MapDraw.DrawLegendsOnMap(Bitmap);
      RestoreBackupDefaults;
      Image1.Picture.Graphic := Bitmap;
      Bitmap.Free;
   end;
end;
{$EndIf}


procedure TMapForm.Terrainblowup1Click(Sender: TObject);
var
   xDEMg2,yDEMg2,xSATg2,ySATg2 : float32;
begin
  if (MapDraw.ValidDEMonMap) and (MapDraw.MapOwner <> moPointVerificationMap) then begin
     MapDraw.ScreenToDEMGrid(LastX,LastY,xDEMg2,yDEMg2);
     CreateZoomWindow(false,5,20,xDEMg2,yDEMg2,xSATg2,ySATg2,false,false);
     ZoomWindow.Closable := false;
     ZoomWindow.Caption := 'Blowup ' + DEMGlb[MapDraw.DEMonMap].DEMLocationString(xDEMg2,yDEMg2);
     if (Sender <> Nil) then ChangeDEMNowDoing(TerrainBlowup);
  end;
end;


procedure TMapForm.SpeedButton4Click(Sender: TObject);
begin
   Lineshapefile1Click(Sender);
end;

procedure TMapForm.Direction01361Click(Sender: TObject);
begin
   MapDraw.MapType := mtFlowDir360;
   DrawColoredMap1Click(Nil);
end;

procedure TMapForm.Directionalslopes1Click(Sender: TObject);
begin
   DEMGlb[MapDraw.DEMonMap].DirectionalSlopesReport(RightClickLat,RightClickLong);
end;

procedure TMapForm.Convertcoordinates1Click(Sender: TObject);
{$IfDef ExCartography}
begin
{$Else}
var
   ConvertForm : TUKOSConvertForm;
begin
   ConvertForm := TUKOSConvertForm.Create(Application);
   ConvertForm.Caption := 'Coordinate converter';
   if MapDraw.DEMMap then ConvertForm.This_projection := DEMGlb[MapDraw.DEMonMap].DEMMapProjection
   else ConvertForm.This_projection := MapDraw.PrimMapProj;
   ConvertForm.Parameters.TabVisible := false;
   ConvertForm.ShowParams;
   ConvertForm.ShowModal;
   {$EndIf}
end;

procedure TMapForm.ConvertUKOSDEMtoUTM1Click(Sender: TObject);
begin
   ConvertUKOSDEMtoUTM(MapDraw.DEMonMap,true);
end;


procedure TMapForm.COPALOS9categories1Click(Sender: TObject);
begin
   COP_ALOS_compare(ca9Cat);
end;

procedure TMapForm.COPALOSbestlocations1Click(Sender: TObject);
begin
   COP_ALOS_compare(caBest);
end;

procedure TMapForm.COPALOScategories1Click(Sender: TObject);
begin
   COP_ALOS_compare(ca4Cat);
end;


procedure TMapForm.COPandALOS1Click(Sender: TObject);
begin
   LoadDEMIXCandidateDEMs('',{MapDraw.DEMonMap,}true,false);
end;

procedure TMapForm.ASCIIArcGrid1Click(Sender: TObject);
begin
   {$IfDef ExOddballDEMexports}
   {$Else}
      DEMGlb[MapDraw.DEMonMap].SaveAsArcGridASCII;
   {$EndIf}
end;

procedure TMapForm.SpeedButton5Click(Sender: TObject);
begin
   ReadDefault('Elevation base level',VolumeRelativeToZ);
   ChangeDEMNowDoing(CalculateVolume);
end;


procedure TMapForm.Contributingarea1Click(Sender: TObject);
{$IfDef ExDrainage}
begin
{$Else}
var
   fName : PathStr;
begin
  if (AD8DEM = 0) then begin
     fName := MainMapData + 'drainage\';
     LoadNewDEM(AD8DEM,fName);
  end;
  ChangeDEMNowDoing(DrainageArea);
{$EndIf}
end;

procedure TMapForm.Sunabovethehorizon2Click(Sender: TObject);
begin
  {$IfDef ExGeography}
  {$Else}
     ChangeDEMNowDoing(FindBlockHorizon);
  {$EndIf}
end;

procedure TMapForm.Sunandsatellitevisibilityandblocking1Click(Sender: TObject);
begin
   SetHorizonOptions(Self);
end;

procedure TMapForm.Sunrisesunsettimes1Click(Sender: TObject);
{$IfDef ExGeography}
begin
{$Else}
var
    sunrisepicker : Get_Sunrise.Tsunrisepicker;
begin
    sunrisepicker := Tsunrisepicker.Create(Application);
    sunrisepicker.ShowModal;
    CheckEditString(sunrisepicker.Edit1.Text,SunriseOptions.Day);
    CheckEditString(sunrisepicker.Edit2.Text,SunriseOptions.Month);
    CheckEditString(sunrisepicker.Edit3.Text,SunriseOptions.Year);
    CheckEditString(sunrisepicker.Edit4.Text,SunriseOptions.DiffUTC);
    SunriseOptions.Morning := (Sunrisepicker.RadioGroup2.ItemIndex = 0);
    SunriseOptions.SunAngle := tSunriseSunsetAngle(sunrisepicker.RadioGroup1.ItemIndex);
    SunRisePicker.Free;
    ChangeDEMNowDoing(SunriseClicking);
{$EndIf}
end;

procedure TMapForm.Supervisedclassification1Click(Sender: TObject);
begin
   {$IfDef ExImageClassify}
   {$Else}
      {$IfDef RecordSatClass} WriteLineToDebugFile('TMapForm.Supervisedclassification1Click ' + Self.Caption); {$EndIf}
      if (MapDraw.MultiGridOnMap = 0) then VISandNIRsurfacebands1Click(Sender);
      Sup_class.StartSupervisedClassification(Self);
   {$EndIf}
end;

procedure TMapForm.Surveytracklines1Click(Sender: TObject);
begin
   {$IfDef ExAdvancedGIS}
   {$Else}
      Survey_lines.GetSurveyTracks(self);
   {$EndIf}
end;

procedure TMapForm.Averagebylatitude1Click(Sender: TObject);
{$IfDef ExAdvancedGIS}
begin
{$Else}
var
   x,y,n,db : integer;
   z,zt : float32;
   Lat,Long : float64;
   fName : PathStr;
   Findings : tStringList;
   sg :  TThisbasegraph;
   GridLimits : tGridLimits;
begin
   Findings := tStringList.Create;
   Findings.Add('PARAMETER,LAT');
   StartProgress('Average by lat');
   GridLimits := MapDraw.MapAreaDEMGridLimits;

   for y := GridLimits.YGridLow to GridLimits.YGridHigh do begin
     UpdateProgressBar(y/DEMGlb[MapDraw.DEMonMap].DEMheader.NumRow);
     zt := 0;
     n := 0;
     for x := GridLimits.XGridLow to GridLimits.XGridHigh do begin
        if DEMGlb[MapDraw.DEMonMap].GetElevMeters(x,y,z) then begin
           zt := zt + z;
           inc(n);
        end;
     end;
     if (n > 0) then begin
         DEMGlb[MapDraw.DEMonMap].DEMGridToLatLongDegree(0,y,Lat,Long);
         Findings.Add(RealToString(zt/n,-18,-6) + ',' + RealToString(Lat,-18,-6));
     end;
   end;
   EndProgress;
   fName := Petmar.NextFileNumber(MDTempDir,'param_by_lat_','.csv');
   db := StringListToLoadedDatabase(Findings, fName);
   sg := GISdb[db].CreateScatterGram('PARAMETER','LAT',true);
   sg.Caption := DEMGlb[MapDraw.DEMonMap].AreaName;
{$EndIf}
end;

procedure TMapForm.AverageDN1Click(Sender: TObject);
begin
   {$IfDef ExMultigrid}
   {$Else}
       MultiGridArray[MapDraw.MultiGridOnMap].CreateAverageValueGrid;
   {$EndIf}
end;

procedure TMapForm.Averagereflectanceinregion1Click(Sender: TObject);
begin
   ChangeDEMNowDoing(RegionDNs);
end;


procedure TMapForm.Averagetonewprojection1Click(Sender: TObject);
begin
   {$IfDef RecordCreateGeomorphMaps} WriteLineToDebugFile('TMapForm.Averagetonewprojection1Click in'); {$EndIf}
   GetGridParameters;
   DEMGlb[MapDraw.DEMonMap].ResampleByAveraging(true);
   {$IfDef RecordCreateGeomorphMaps} WriteLineToDebugFile('TMapForm.Averagetonewprojection1Click grids out'); {$EndIf}
end;


procedure TMapForm.Averagetopographicprofile1Click(Sender: TObject);
begin
   ChangeDEMNowDoing(SeekingAverageProfile);
end;


procedure TMapForm.Fansensitivity1Click(Sender: TObject);
begin
   ChangeDEMNowDoing(FanSensitivity);
end;


procedure TMapForm.Fatfingers1Click(Sender: TObject);
begin
   StartFatFingers(Self);
end;

procedure TMapForm.FD8Lognumbercells1Click(Sender: TObject);
begin
   WBT_FlowAccumulation(True,True,False,GeotiffDEMNameOfMap);
end;

procedure TMapForm.FD8Lognumbercells2Click(Sender: TObject);
begin
   WBT_FlowAccumulation(True,False,False,GeotiffDEMNameOfMap);
end;

procedure TMapForm.LocaddatumtoEGM20081Click(Sender: TObject);
var
   LocalToWGS84,WGS84toEGM2008 : integer;
begin
   if GetExistingFileName('local vertical datum to WGS84 ellipsoid','*.dem,*.tif',GeoidWGS84ellipsoidToLocalVDatum) and GetExistingFileName('WGS84 ellipsoid to EGM2008','*.dem,*.tif',Geoid2008FName) then begin
      LocalToWGS84 := OpenNewDEM(GeoidWGS84ellipsoidToLocalVDatum,false);
      WGS84toEGM2008 := OpenNewDEM(Geoid2008FName,false);
      DEMGlb[MapDraw.DEMonMap].MoveToEGM2008(WGS84toEGM2008,LocalToWGS84);
      DEMGlb[MapDraw.DEMonMap].DEMheader.VerticalCSTypeGeoKey := VertCSEGM2008;
      CloseSingleDEM(LocalToWGS84);
      CloseSingleDEM(WGS84toEGM2008);
      RespondToChangedDEM;
   end;
end;

procedure TMapForm.LocateHighValues1Click(Sender: TObject);
{$IfDef ExMultigrid}
begin
{$Else}
const
   TheMax : integer = 64000;
   TheMin : integer = 16000;
begin
   ReadDefault('Min values to flag',TheMin);
   ReadDefault('Max values to flag',TheMax);
   MultiGridArray[MapDraw.MultiGridOnMap].FindHighValues(TheMin,TheMax,Self);
{$EndIf}
end;


procedure TMapForm.Locationsonly1Click(Sender: TObject);
begin
   MapDraw.AssignSecondDEM(MapDraw.DEM2onMap);
   MDDef.FuzzyMatches := true;
   MDDef.SinglePixel := true;
   if (Sender = Validpointssinglecategory1) then begin
   end
   else if (Sender = Values1) or (Sender = Nil) then begin
   end
   else if (Sender = LocationsOnly1) then begin
      MDDef.FuzzyMatches := false;
      MDDef.SinglePixel := false;
   end;
   AddOverlay(Self,ovoSecondGrid);
   DoFastMapRedraw;
end;



procedure TMapForm.Hotspots1Click(Sender: TObject);
begin
   {$IfDef ExGeology}
   {$Else}
      //if MDDef.MoveGeologyDBMemory then DesiredDBMode := dbmCDS;
      LoadDataBaseFile(Hotspot_db);
   {$EndIf}
end;


procedure TMapForm.Hurricanes1Click(Sender: TObject);
begin
   {$IfDef ExGeography}
   {$Else}
      //if MDDef.MoveGeographyDBMemory then DesiredDBMode := dbmCDS;
      LoadDataBaseFile(ClimateDir + 'hurricane_2017\Allstorms.ibtracs_wmo.v03r10.dbf');
   {$EndIf}
end;


procedure TMapForm.Agedepthcurve1Click(Sender: TObject);
var
   DepthDEM : integer;
   ThisGraph : TThisBaseGraph;
begin
   if (MapDraw.DEMonMap = PredAgesDEM) then begin
      wmdem.PredBathySpeedButtonClick(Nil);
      DepthDEM := LastDEMloaded;
   end
   else begin
      if (PredAgesDEM = 0) then wmdem.SeaFloorAgeSpeedButtonClick(nil);
      DepthDEM := MapDraw.DEMOnMap;
      AllMapsMatchThisCoverageArea1Click(Nil);
   end;

   ThisGraph := DEMStat.GridScatterGram(DEMGlb[PredAgesDEM].SelectionMap.MapDraw.MapAreaDEMGridLimits,PredAgesDEM,DepthDEM);
   ThisGraph.Caption := 'Age-depth Curve';
   ThisGraph.GraphDraw.HorizLabel := 'Age (Ma)';
   ThisGraph.GraphDraw.VertLabel := 'Depth (m)';
   ThisGraph.RedrawDiagram11Click(Nil);
end;


procedure TMapForm.SpeedButton7Click(Sender: TObject);
begin
   Overlays1Click(Sender);
end;

procedure TMapForm.SpeedButton8Click(Sender: TObject);
begin
   db_display_options.OpenMapTableOfContents(Self,true);
end;

procedure TMapForm.AnaglyphSpeedButtonClick(Sender: TObject);
begin
   {$IfDef ExExoticMaps}
   {$Else}
       MdDef.StereoMode := smAnaglyph;
       AutoAnaglyphRedraw := not AutoAnaglyphRedraw;
       if AutoAnaglyphRedraw then Draw3Dmap(Self)
       else DoBaseMapRedraw;
   {$EndIf}
end;

procedure TMapForm.angential1Click(Sender: TObject);
var
   NewGrid : integer;
begin
   {$IfDef NoExternalPrograms}
   {$Else}
       NewGrid := WBT_TangentialCurvature(GeotiffDEMNameOfMap);
       MatchAnotherDEMMap(NewGrid,MapDraw.DEMonMap);
   {$EndIf}
end;

procedure TMapForm.Tangentdegrees1Click(Sender: TObject);
begin
   Multiplyzvalues1Click(Sender);
end;

procedure TMapForm.Tangentradians1Click(Sender: TObject);
begin
   Multiplyzvalues1Click(Sender);
end;

procedure TMapForm.KoppenSpeedButtonClick(Sender: TObject);
begin
   {$IfDef ExGeography}
   {$Else}
       KoppenClimateStations1.Visible := MDdef.ShowClimateStationDB;
       Globalmonthlytemperatures1.Visible := MDdef.ShowClimateStationDB;
       Globalmonthlyrain1.Visible := MDdef.ShowClimateStationDB;
       ClimatePopupMenu18.Popup(Mouse.CursorPos.X,Mouse.CursorPos.Y);
   {$EndIf}
end;


procedure TMapForm.Structuralgeologycomputations1Click(Sender: TObject);
begin
   {$IfDef ExGeology}
   {$Else}
       OpenStructuralGeologyForm(self);
   {$EndIf}
end;

procedure TMapForm.suanamitravel1Click(Sender: TObject);
begin
   Map_route.OverlayMapRoutes(Self,true);
end;


procedure TMapForm.ProjectFocalMechanismtoSurface1Click(Sender: TObject);
begin
   {$IfDef ExGeology}
   {$Else}
      ChangeDEMNowDoing(ProjectFocalMechToSurface);
   {$EndIf}
end;

procedure TMapForm.Pureplatecareeprojectiondistorted1Click(Sender: TObject);
begin
   MapDraw.UseDistortedRawLatLongScaling := not MapDraw.UseDistortedRawLatLongScaling;
   MapDraw.ResizeMapByPercentage(MapDraw.CurrentZoomLevel);
   DoCompleteMapRedraw;
end;

procedure TMapForm.Putshadingfromthismapunderselectedmaps1Click(Sender: TObject);
var
   Maps : tStringList;
   i,j : integer;
begin
   PickMaps(Maps,'Maps for shading from this map');
   if (Maps.Count > 0) then begin
      //MatchThiscoverageareaandsamepixelsize1Click(Sender);
      for i := 0 to pred(WMDEM.MDIChildCount) do begin
         if WMDEM.MDIChildren[i] is tMapForm and (WmDEM.MDIChildren[i].Handle <> Handle) then begin
            for j := 0 to pred(Maps.Count) do begin
               if (Maps.Strings[j] = (WMDEM.MDIChildren[i] as TMapForm).Caption) then begin
                  (WMDEM.MDIChildren[i] as TMapForm).MergeAnotherDEMreflectance(MapDraw.DEMonMap);
               end;
            end;
         end;
      end;
   end;
   Maps.Free;
end;

procedure TMapForm.Filledneighborhood1Click(Sender: TObject);
begin
   ThinDEM1Click(Sender);
end;

procedure TMapForm.RespondToChangedDEM;
begin
   DEMGlb[MapDraw.DEMonMap].CheckMaxMinElev;
   DEMGlb[MapDraw.DEMonMap].CloseElevPercentiles;
   MapDraw.ScaleMapElevationsToDEM;
   DEMGlb[MapDraw.DEMonMap].DEMStatus := dsUnsavedEdits;
   CheckProperTix;
   DoBaseMapRedraw;
end;


procedure InitializeDEMMapf;
var
   i : integer;
begin
   {$IfDef MessageStartUpUnit} MessageToContinue('DEMMapf initialization'); {$EndIf}
   for i := 1 to MaxVectorMap do VectorMap[i] := Nil;
   VegGraph := Nil;
   EnsembleClassDB := 0;
   NumPlateMotionMaps := 0;
   DEMNowDoing := JustWandering;
   LocalEdit := false;
   CreateTrainingSet := false;
   MouseDragging := false;
   CreateHiddenMap := false;
   SizeIsCorrectThankYou := false;
   DEMNowDoing := JustWandering;
   DEMDoingNext := JustWandering;
   DEMDoingWhatBefore := JustWandering;
   RoadMaskBMP := Nil;
   MovieList := Nil;
   ClipBoard_Line_Coords := Nil;
   ClipBoard_NumPts := 0;
   DatumShiftDB := 0;
   LocationLabel := true;
   LocationColor := clBlack;
   LocationSymSize := 2;
   LocationSymbol := Box;
   TargetFlyThrough  := false;
   gbLatStart := -999;
   VolumeRelativeToZ := 0;
   AmbushCountMyBitmap := Nil;
   StreamProfileResults := Nil;
   GeosymbolTable := nil;
   AD8DEM := 0;
   ClosingIsHappening := false;

   {$IfDef ExGeology}
   {$Else}
      BroadCastPlaneData.DipAndStrike := '';
   {$EndIf}

   {$IfDef MessageStartUpUnit} MessageToContinue('DEMMapf initialization done'); {$EndIf}
end;

procedure TMapForm.DEMelevationmap1Click(Sender: TObject);
begin
   DEMelevationcolors1Click(Sender);
end;


procedure TMapForm.DEMfromseries1Click(Sender: TObject);
var
    WantedDEM,WantImage : integer;
    LatHigh,LatLow,LongHigh,LongLow : float64;
    DEMSeries : ShortString;
begin
    PickDEMSeries(DEMSeries,'DEM to match display area');
    LatHigh := MapDraw.MapCorners.BoundBoxGeo.ymax;
    LatLow := MapDraw.MapCorners.BoundBoxGeo.ymin;
    LongHigh := MapDraw.MapCorners.BoundBoxGeo.xmax;
    LongLow := MapDraw.MapCorners.BoundBoxGeo.xmin;
    LoadMapLibraryBox(WantedDEM,WantImage,true,LatHigh,LongLow,LatLow,LongHigh,DEMSeries,True);
    MatchMapToThisOne(DEMGlb[WantedDEM].SelectionMap);
end;


procedure TMapForm.DEMfromseriespoint1Click(Sender: TObject);
begin
   DEMfromseries1Click(Sender);
end;


procedure Export3DDEM(DEMonMap : integer);
var
   xm,x,y,NumVert     : integer;
   z1,z2,z3,z4,zc : float32;
   x1,y1,x2,y2,x3,y3,x4,y4,xc,yc : float64;
   ObjList : tStringList;
   ThisColVertex,LastColVertex : array[0..MaxElevArraySize] of integer;
   SaveName : PathStr;

   procedure AddVertex(x,y,z : float64);
   begin
      OBJList.Add('v ' + RealToString(x-xc,-18,1) + ' ' + RealToString(y-yc,-18,1) + ' ' + RealToString(z-zc,-18,1));
   end;

begin
   {$IfDef RecordSave} WriteLineToDebugFile('Start Export3DDEM'); {$EndIf}
   with DEMGlb[DEMonMap] do begin
      x := pred(DEMheader.NumCol) * Pred(DEMheader.NumRow);
      xm := x * 139;
      if not AnswerIsYes('Export with ' + IntToStr(x) + ' polygons' + MessLineBreak + 'File: ' + SmartMemorySizeBytes(xm) + MessLineBreak + 'Proceed') then exit;

      SaveName := ProgramRootDir;
      GetFileNameDefaultExt('OBJ file','OBJ file|*.OBJ',SaveName);

      if AnswerIsYes('Center output') then begin
         x := DEMheader.NumCol div 2;
         y := DEMheader.NumRow div 2;
         DEMGridtoUTM(x,y,xc,yc);
         GetElevMetersOnGrid(x,y,zc);
      end
      else begin
         xc := 0;
         yc := 0;
         zc := 0;
      end;

      ObjList := tStringList.Create;
      ObjList.Add('# Created with PETMAR Trilobite Breeding Ranch MICRODEM');
      ObjList.Add('#  DEM: ' + AreaName);
      NumVert := 0;
      if ShowSatProgress then StartProgress('Export');

      for x := 0 to (DEMheader.NumCol-2) do begin
         if ShowSatProgress then UpdateProgressBar(x/(DEMheader.NumCol-2));
         for y := 0 to (DEMheader.NumRow-2) do begin
            if (x > 0) and GetElevMetersOnGrid(x,y,z1) and GetElevMetersOnGrid(succ(x),y,z2) and GetElevMetersOnGrid(succ(x),succ(y),z3) and  GetElevMetersOnGrid(x,succ(y),z4) then begin
               DEMGridtoUTM(X,y,x1,y1);
               DEMGridtoUTM(succ(X),y,x2,y2);
               DEMGridtoUTM(succ(X),succ(y),x3,y3);
               DEMGridtoUTM(X,succ(y),x4,y4);
               AddVertex(x1,y1,z1);
               AddVertex(x2,y2,z2);
               AddVertex(x3,y3,z3);
               AddVertex(x4,y4,z4);
               OBJList.Add('f ' + IntToStr(NumVert+1) + ' ' + IntToStr(NumVert+2) + ' ' + IntToStr(NumVert+3) + ' ' + IntToStr(NumVert+4)  );
               inc(NumVert,4);
            end;
            LastColvertex := ThisColVertex;
         end;
      end;
      if ShowSatProgress then EndProgress;

      ObjList.SaveToFile(SaveName);
      ObjList.Free;
   end;
   {$IfDef RecordSave} WriteLineToDebugFile('Ending Export3DDEM'); {$EndIf}
end;

procedure TMapForm.O1Click(Sender: TObject);
begin
   ChangeDEMNowDoing(FillHolesByOutline);
end;

procedure TMapForm.OBJ1Click(Sender: TObject);
begin
   Export3DDEM(MapDraw.DEMonMap);
end;

procedure TMapForm.Oceancurrents1Click(Sender: TObject);
begin
  {$IfDef ExGeography}
  {$Else}
      //if MDDef.MoveGeographyDBMemory then DesiredDBMode := dbmCDS;
      LoadDataBaseFile(GlobalCurrentsFName);
  {$EndIf}
end;

procedure TMapForm.oday1Click(Sender: TObject);
begin
   {$IfDef ExGeography}
   {$Else}
      MDDef.SunlightSingleDay := 2;
      SunAndHorizon(Self,0,RightClickLat,RightClickLong,true,false);
   {$EndIf}
end;

procedure TMapForm.oday2Click(Sender: TObject);
var
   iMonth,iDay,iYear : word;
begin
   DecodeDate(Now,iYear,iMonth,iDay);
   MoonPositionStereoNet(RightClickLat,RightClickLong,iyear,imonth,iday);
end;

procedure TMapForm.oday3Click(Sender: TObject);
begin
   MoonPositionStereoNet(RightClickLat,RightClickLong,-99,-99,-99);
end;

procedure TMapForm.PGM1Click(Sender: TObject);
begin
   {$IfDef ExOddballDEMexports}
   {$Else}
      DEMGlb[MapDraw.DEMonMap].SavePGM(true);
   {$EndIf}
end;

procedure TMapForm.PGM8bit1Click(Sender: TObject);
begin
   {$IfDef ExOddballDEMexports}
   {$Else}
      DEMGlb[MapDraw.DEMonMap].SavePGM(false);
   {$EndIf}
end;

procedure TMapForm.Physicalgeographylabs1Click(Sender: TObject);
begin
  {$IfDef ExGeography}
  {$Else}
      wmdem.GeographyPopUpMenu.Popup(Mouse.CursorPos.X,Mouse.CursorPos.Y);
  {$EndIf}
end;

procedure TMapForm.PI1Click(Sender: TObject);
begin
   MakeTPIGrid(MapDraw.DEMonMap,nmRRI,true);
end;

procedure TMapForm.EditDEMgrid1Click(Sender: TObject);
begin
   DEMEditW.ShowAndEditDEMGrid(DEMGlb[MAPDraw.DEMonMap].SelectionMap.DEMeditForm,MapDraw.DEMonMap);
end;


procedure TMapForm.Databaselegend1Click(Sender: TObject);
var
   Bitmap,sBitmap : tMyBitmap;
   TStr : ANSIString;
   i,dbsonMap,y,MaxLen,Len,BoxSize : integer;
begin
   dbsOnMap := FindDBsOnMap;
   {$If Defined(RecordLegend) or Defined(RecordGISDB)} WriteLineToDebugFile('TMapForm.Databaselegend1Click in,  dbs on map=' + IntToStr(dbsOnMap)); {$EndIf}
   CreateBitmap(Bitmap,1200,25*dbsOnMap);
   EditMyFont(MDDef.DefGisLabelFont1);
   LoadMyFontIntoWindowsFont(MDDef.DefGisLabelFont1,Bitmap.Canvas.Font);

   BoxSize := Bitmap.Canvas.TextHeight('M') + 5;
   Bitmap.Height := dbsOnMap * BoxSize + 15;

   y := 0;
   MaxLen := 0;
   for i := 1 to MaxDataBase do begin
      if (GISdb[i] <> Nil) and (GISdb[i].TheMapOwner <> Nil) and (GISdb[i].TheMapOwner.Handle = Self.Handle) then begin
         {$IfDef RecordGISDB} WriteLineToDebugFile('y=' + IntToStr(y) + '   ' + GISdb[i].dbName); {$EndIf}
         CreateBitmap(Sbitmap,25,20);
         GISdb[i].BitmapForDatabase(sBitmap,20);
         Bitmap.Canvas.Draw(5,y,sBitmap);
         sBitmap.Free;
         TStr := RemoveUnderScores(GISdb[i].dbName) + '   n=' + IntToStr(GISdb[i].MyData.FiltRecsInDB);
         Len := Bitmap.Canvas.TextWidth(TStr);
         if (Len > maxlen) then MaxLen := Len;
         Bitmap.Canvas.Brush.Style := bsClear;
         Bitmap.Canvas.TextOut(35,y,TStr);
         inc(y,BoxSize);
      end;
   end;
   Bitmap.Width := 40 + MaxLen;
   Bitmap.Canvas.Pen.Color := clBlack;
   Bitmap.Canvas.Rectangle(0,0,pred(Bitmap.Width),Pred(Bitmap.Height));
   PetImage_form.DisplayBitmap(Bitmap,'Database legend');
   FreeAndNil(Bitmap);
end;


procedure TMapForm.DataBaseSpeedButton28Click(Sender: TObject);
begin
   OpenDBonMap('','');
end;


procedure TMapForm.KoppenClimateStations1Click(Sender: TObject);
{$IfDef ExGeography}
begin
{$Else}
begin
   if not ValidDB(ClimateStationDB) then begin
      ClimateStationDB := LoadDataBaseFile(ClimateStationFName);
   end;
   if ValidDB(ClimateStationDB) then begin
      if (Sender = Globalmonthlytemperatures1) then GISdb[ClimateStationDB].StartClimateDisplay(dbasMonthlyTemp)
      else if (Sender = Globalmonthlyrain1) then GISdb[ClimateStationDB].StartClimateDisplay(dbasMonthlyRain)
      else GISdb[ClimateStationDB].StartClimateDisplay(dbasKoppen);
      CheckProperTix;
   end;
{$EndIf}
end;

procedure TMapForm.Koppenclimograph1Click(Sender: TObject);
var
   RecsFound : integer;
   TStr : shortstring;
begin
  {$IfDef ExGeography}
  {$Else}
      if ValidDB(ClimateStationDB) and (GISdb[ClimateStationDB].theMapOwner = Self) and (GISdb[ClimateStationDB].KoppenPresent) then begin
         GISdb[ClimateStationDB].IdentifyRecord(-999,-999,RightClickLat,RightClickLong,RecsFound,true,false,TStr,false);
      end;
   {$EndIf}
end;


procedure TMapForm.Koppenclimographfromclimategrids1Click(Sender: TObject);

      (*
      function MakeKoppenClimograph(DEM : integer; Lat,Long : float32) : TKoppenGraph;
      var
         ClimateData : tClimateData;
         z : float32;
      begin
         {$IfDef RecordGeography} WriteLineToDebugFile('TMapForm.Koppenclimographfromclimategrids1Click'); {$EndIf}
         OpenTempPrecipEvap(false);
         if ValidMultiGrid(TempMG) and ValidMultiGrid(PrecipMG) then begin
            ClimateData.Lat := Lat;
            ClimateData.Long := Long;
            ClimateData.Elevation := -9999;
            if (DEM <> 0) then begin
               if (DEMGlb[DEM].DEMheader.ElevUnits in [euMeters]) and DEMGlb[DEM].GetElevFromLatLongDegree(ClimateData.Lat,ClimateData.Long,z) then
                  ClimateData.Elevation := round(z);
            end;
            ClimateData.Location := 'Global Grids ';
            LoadClimateData(ClimateData);
            ClassifyClimate(ClimateData);
            Result := OpenKoppenGraph(500,400,ClimateData);
         end;
      end;
      *)
begin
   MakeKoppenClimograph(MapDraw.DEMonMap,RightClickLat,RightClickLong);
end;


procedure TMapForm.Koppengrid1Click(Sender: TObject);
begin
   {$IfDef ExGeography}
   {$Else}
      LoadDataBaseFile(ClimateDir + 'koppen_grid' + DefaultDBExt);
      CreateKoppenLegend(false);
   {$EndIf}
end;

procedure TMapForm.Koppenlegend1Click(Sender: TObject);
begin
   {$IfDef ExGeography}
   {$Else}
      CreateKoppenLegend(false);
   {$EndIf}
end;

procedure TMapForm.KoppenSpeedButton7Click(Sender: TObject);
begin
{$IfDef ExGeography}
{$Else}
   if (ClimateStationDB = 0) or (GISdb[ClimateStationDB] = Nil) then begin
      ClimateStationDB := 0;
      BackToWandering;
   end
   else begin
      ChangeDEMNowDoing(IDDataBaseOne);
      DBEditting := ClimateStationDB;
   end;
{$EndIf}
end;


procedure TMapForm.CompareviewshedsonmultipleDEMs1Click(Sender: TObject);
{$IfDef ExViewshed}
begin
{$Else}
label
   Done;
var
    Xstart,YStart,XEnd,YEnd,NumFansDone,SymSize,
    SampleIncr,EdgeBuffer,x,y : integer;
    SavedBitmap : tMyBitmap;
    WeaponsFan : tWeaponsFan;
begin
    ShowSatProgress := false;
    if (Sender = SavedviewshedsonmultipleDEMs1) then begin
    end
    else begin
       DeleteMultipleFiles(MDTempDir,OverlayFExt);
       SampleIncr := 25;
       if (Sender = Nil) then begin
          XStart := NewX1;
          YStart := NewY1;
          XEnd := NewX2;
          YEnd := NewY2;
       end
       else EdgeBuffer := 100;
       CopyImageToBitmap(Image1,SavedBitmap);
       repeat
           Image1.Picture.Graphic := SavedBitmap;
           ReadDefault('Sample increment (map pixels)',SampleIncr);
           if (Sender <> Nil) then begin
              ReadDefault('Edge buffer (map pixels)',EdgeBuffer);
              XStart := EdgeBuffer;
              XEnd := MapDraw.MapXSize-EdgeBuffer;
              YStart := EdgeBuffer;
              YEnd := MapDraw.MapYSize-EdgeBuffer;
           end;
           if (SampleIncr > 5) then SymSize := 4
           else if (SampleIncr > 2) then SymSize := 2
           else SymSize := 1;
           NumFansDone := 0;
           x := Xstart;
           while x < XEnd do begin
              y := YStart;
              while y < YEnd do begin
                 Inc(NumFansDone);
                 MapDraw.ScreenToLatLongDegree(x,y,WeaponsFan.W_Lat,WeaponsFan.W_Long);
                 WeaponsFan.Fan_Name := 'S_' + IntToStr(NumFansDone);
                 Petmar.ScreenSymbol(Image1.Canvas,x,y,FilledBox,SymSize,claRed);
                 inc(y,SampleIncr);
              end;
              inc(x,SampleIncr);
           end;
        until AnswerIsYes('OK' + MessLineBreak + 'Fans: ' + IntToStr(NumFansDone) + MessLineBreak + 'Spacing: ' + RealToString(MapDraw.ScreenPixelSize * SampleIncr,-12,-1) + ' m');
       Image1.Picture.Graphic := SavedBitmap;
       SavedBitmap.Free;
    end;
    {$EndIf}
end;


procedure TMapForm.RenameDEM1Click(Sender: TObject);
begin
   Petmar.GetString('New name for DEM',DEMGlb[MapDraw.DEMonMap].AreaName,false,ReasonableTextChars);
   DoFastMapRedraw;
end;

(*
procedure TMapForm.renchslabdips1Click(Sender: TObject);
{$IfDef ExGeology}
begin
{$Else}
var
   fName,fName2 : PathStr;
   db : integer;
begin
   fName2 := 'trench_slab_dips' + DefaultDBExt;
   fName := DBDir + 'trench_slab_dips' + DefaultDBExt;
   if not FileExists(fName) then DownloadFileFromWeb(WebDataDownLoadDir + fName2,fName);

   if FileExists(fName) then begin
      MapDraw.AllowDataBaseDrawing := false;
      db := LoadDataBaseFile(fName);
      if db <> 0 then begin
         MapDraw.AllowDataBaseDrawing := true;
         GISdb[db].PlotFieldOnMap('DIP',0,90);
      end;
   end;
   {$EndIf}
end;
*)

procedure TMapForm.ElevationsExtremes1Click(Sender: TObject);
{$IfDef ExVectorOverlay}
begin
{$Else}
var
   TerrainCategory : tTerrainCatDefinition;
begin
    GetDEM(ExtremeZDEM);
    DEMGlb[ExtremeZDEM].InitializeTerrainCategory(TerrainCategory);
    GetTerrainCategory(tcElevOnly,Self,ExtremeZDEM,TerrainCategory,DEMGlb[ExtremeZDEM].ElevationDEM);
{$EndIf}
end;


procedure TMapForm.PlotExtremeZValues(Memo1 : tMemo; sl : tstringlist = nil);
var
  Col,Row,xp,yp,Bad,Shown : integer;
  Lat,Long   : float64;
  z : float32;
  Bitmap : tMyBitmap;
begin
   if ValidDEM(ExtremeZDEM) then with DEMGlb[ExtremeZDEM] do begin
      if ShowSatProgress then StartprogressAbortOption('Points');
      Bad := 0;
      Shown := 0;
      CopyImageToBitmap(Image1,Bitmap);

      for Col := 0 to (DEMheader.NumCol-1) do begin
        if ShowSatProgress and (col mod 25 = 0) then UpdateProgressBar(Col/DEMheader.NumCol);
         for Row := 0 to (DEMheader.NumRow-1) do begin
            if GetElevMetersOnGrid(Col,Row,z) and (z >= LowZ) and (z <= HighZ) then begin
               inc(Bad);
               DEMGridToLatLongDegree(Col,Row,Lat,Long);
               MapDraw.LatLongDegreeToScreen(Lat,Long,xp,yp);
               if (sl <> Nil) then sl.Add(RealToString(Lat,-12,-8) + ',' + RealToString(Long,-12,-8) + ',' + RealToString(z,-12,-4));
               if MapDraw.OnScreen(xp,yp) then begin
                  inc(Shown);
                  ScreenSymbol(Bitmap.Canvas,xp,yp,DrSymbol);
               end;
            end;
         end;
         if WantOut then break;
      end;
      if ShowSatProgress then EndProgress;
      Image1.Picture.Graphic := Bitmap;
      FreeAndNil(Bitmap);
      if (Memo1 <> Nil) then begin
         Memo1.Lines.Add('Points with z >= ' + RealToString(LowZ,-12,-2) + ' and z <= ' +  RealToString(HighZ,-12,-2));
         Memo1.Lines.Add('   In DEM:  ' + IntToStr(Bad));
         Memo1.Lines.Add('   On Map:  ' + IntToStr(Shown));
      end;
   end;
end;


procedure TMapForm.Datavoids1Click(Sender: TObject);
var
   x,y, NumHoles,Col,Row,numHolesOnmap : integer;
   Bitmap : tMyBitmap;
begin
    CopyImageToBitmap(Image1,Bitmap);
    if ShowSatProgress then Startprogress('Holes');
    NumHoles := 0;
    numHolesOnmap := 0;
    For Col := 0 to pred(DEMGlb[MapDraw.DEMOnMap].DEMheader.NumCol) do begin
       if ShowSatProgress and (col mod 25 = 0) then UpdateProgressBar(Col/DEMGlb[MapDraw.DEMOnMap].DEMheader.NumCol);
       for Row := 0 to pred(DEMGlb[MapDraw.DEMOnMap].DEMheader.NumRow) do begin
          if  DEMGlb[MapDraw.DEMOnMap].MissingDataInGrid(Col,Row) then begin
             inc(numHoles);
             MapDraw.DEMGridToScreen(Col,Row,x,y);
             if MapDraw.OnScreen(x,y) then begin
                inc(numHolesOnmap);
                Petmar.ScreenSymbol(Bitmap.Canvas,x,y,DrSymbol);
             end;
          end;
       end;
    end;
    if ShowSatProgress then EndProgress;
    Image1.Picture.Graphic := Bitmap;
    FreeAndNil(Bitmap);
    MessageToContinue('DEM: ' +  DEMGlb[MapDraw.DEMOnMap].AreaName + MessLineBreak + MessLineBreak +
         'Holes on map: ' + IntToStr(NumHolesOnMap) + '  (' +RealToString(100* NumHoles /  DEMGlb[MapDraw.DEMOnMap].DEMheader.NumRow /  DEMGlb[MapDraw.DEMOnMap].DEMheader.NumCol,-12,2) + '%)' + MessLineBreak +
         'Holes in DEM: ' + IntToStr(NumHoles) + MessLineBreak,True);
end;



procedure TMapForm.Allmaps1Click(Sender: TObject);
begin
   DeleteMultipleFiles(ProjectDir +  'Map_Layers\','*.bmp');
end;


procedure TMapForm.Saveallmapsasimage1Click(Sender: TObject);
var
   i : integer;
   MyBMP : tMyBitmap;
   fName : PathStr;
begin
   for i := pred(WMDEM.MDIChildCount) downto 0 do begin
      if (WMDEM.MDIChildren[i] is TMapForm) then begin
         (WMDEM.MDIChildren[i] as TMapForm).DoFastMapRedraw;
         CopyImageToBitmap((WMDEM.MDIChildren[i] as TMapForm).Image1,MyBMP);
         fName := (WMDEM.MDIChildren[i] as TMapForm).MapDraw.BaseTitle;
         fName := SpacesToUnderScores(fName);
         StripInvalidPathNameChars(fName);
         fName := ImageDir + fName + '.png';
         SaveBitmap(MyBMP,fName);
         MyBMP.Free;
      end;
   end;
end;

procedure TMapForm.ChangePixelIsFormat;
var
   fName : PathStr;
begin
   {$If Defined(TrackDEMCorners)} DEMGlb[MapDraw.DEMonMap].WriteDEMCornersToDebugFile('TMapForm.ChangePixelIsFormat, original DEM'); {$EndIf}

   OpenNewDEM(DEMGlb[MapDraw.DEMonMap].DEMfileName);
   if (DEMGlb[MapDraw.DEMonMap].DEMHeader.RasterPixelIsGeoKey1025 in [PixelIsPoint]) then begin
      DEMGlb[MapDraw.DEMonMap].DEMHeader.DEMSWCornerX := DEMGlb[MapDraw.DEMonMap].DEMHeader.DEMSWCornerX - 0.5 * DEMGlb[MapDraw.DEMonMap].DEMHeader.DEMxSpacing;
      DEMGlb[MapDraw.DEMonMap].DEMHeader.DEMSWCornerY := DEMGlb[MapDraw.DEMonMap].DEMHeader.DEMSWCornerY - 0.5 * DEMGlb[MapDraw.DEMonMap].DEMHeader.DEMySpacing;
      DEMGlb[MapDraw.DEMonMap].DEMHeader.RasterPixelIsGeoKey1025 := PixelIsArea;
      fName := ChangeFileExt(DEMGlb[MapDraw.DEMonMap].DEMfileName,'_pixel_is_area.dem');
   end
   else if (DEMGlb[MapDraw.DEMonMap].DEMHeader.RasterPixelIsGeoKey1025 in [PixelIsArea]) then begin
      DEMGlb[MapDraw.DEMonMap].DEMHeader.DEMSWCornerX := DEMGlb[MapDraw.DEMonMap].DEMHeader.DEMSWCornerX + 0.5 * DEMGlb[MapDraw.DEMonMap].DEMHeader.DEMxSpacing;
      DEMGlb[MapDraw.DEMonMap].DEMHeader.DEMSWCornerY := DEMGlb[MapDraw.DEMonMap].DEMHeader.DEMSWCornerY + 0.5 * DEMGlb[MapDraw.DEMonMap].DEMHeader.DEMySpacing;
      DEMGlb[MapDraw.DEMonMap].DEMHeader.RasterPixelIsGeoKey1025 := PixelIsPoint;
      fName := ChangeFileExt(DEMGlb[MapDraw.DEMonMap].DEMfileName,'_pixel_is_point.dem');
   end;

   DEMGlb[MapDraw.DEMonMap].WriteNewFormatDEM(fName);
   CloseSingleDEM(MapDraw.DEMonMap);
   OpenNewDEM(fName);
end;


procedure TMapForm.SaveasPixelisarea1Click(Sender: TObject);
begin
   ChangePixelIsFormat;
end;

procedure TMapForm.SaveasPixelispoint1Click(Sender: TObject);
begin
   ChangePixelIsFormat;
end;

procedure TMapForm.SaveBitmapForMovie(BaseName : PathStr; var MovieList : tStringList);
var
   fName : PathStr;
begin
   if (MDDef.MovieFormat = mfGeotiff) then begin
      fName := BaseName + '.TIF';
      CaptureBMPInGeoTIFF(MapDraw,DEMdefs.MovieDir + fName,Image1);
   end
   else begin
      if MDdef.MovieFormat in [mfJPEG,mfJPEGworld] then fName := BaseName + '.JPG'
      else fName := BaseName + OverlayFExt;
      SaveImageAsBMP(Image1,fName);
      if MDdef.MovieFormat in [mfBMPWorld,mfJPEGworld] then MapDraw.WriteMapsWorldFile(fName);
   end;
   MovieList.Add(ExtractFileName(fName));
end;





procedure TMapForm.AllMapsMatchThisCoverageArea1Click(Sender: TObject);
var
   i : integer;
begin
   for i := 0 to pred(WMDEM.MDIChildCount) do
      if (WMDEM.MDIChildren[i] is tMapForm) and (WmDEM.MDIChildren[i].Handle <> Handle) then begin
         MatchAnotherMapThisCoverageArea(Self,WMDEM.MDIChildren[i] as TMapForm);
      end;
   UpdateMenusForAllMaps;
end;


procedure TMapForm.Allmissingtosinglevaluevalidsettomissing1Click(Sender: TObject);
var
   NewZ : float64;
   x,y : integer;
begin
   ReadDefault('Reclassified z value',NewZ);
   for x := 0 to pred(DEMGlb[MapDraw.DEMonMap].DEMheader.NumCol) do begin
      for y := 0 to pred(DEMGlb[MapDraw.DEMonMap].DEMheader.NumRow) do begin
         if DEMGlb[MapDraw.DEMonMap].MissingDataInGrid(x,y) then DEMGlb[MapDraw.DEMonMap].SetGridElevation(x,y,NewZ)
         else DEMGlb[MapDraw.DEMonMap].SetGridMissing(x,y);
      end;
   end;
   RespondToChangedDEM;
end;


procedure TMapForm.Allmultigridbasicstatistics1Click(Sender: TObject);
var
   i : integer;
begin
   {$IfDef ExAdvancedSats}
   {$Else}
      for i := 1 to MaxMultiGrid do begin
         if (MultiGridArray[i] <> Nil) then begin
            MultiGridArray[i].BasicStats;
         end;
      end;
   {$EndIf}
end;


procedure TMapForm.Allopengrids1Click(Sender: TObject);
begin
   GetOptimalLagParameters(Self,0);
end;

procedure TMapForm.Allpoints1Click(Sender: TObject);
begin
   DEMGlb[MapDraw.DEMOnMap].MissingDataToConstantVelue;
   RespondToChangedDEM;
end;

procedure TMapForm.Allsamedisplay1Click(Sender: TObject);
var
   i : integer;
begin
   for i := 0 to pred(WMDEM.MDIChildCount) do begin
      if (WMDEM.MDIChildren[i] is tMapForm) and (WmDEM.MDIChildren[i].Handle <> Handle) then begin
         (WMDEM.MDIChildren[i] as TMapForm).SetMapDisplayType(MapDraw.MapType);
      end;
   end;
end;


procedure TMapForm.Allsamemapsize1Click(Sender: TObject);
var
   i : integer;
   Aspect : float64;
begin
   {$IfDef RecordMatchMaps} WriteLineToDebugFile('TMapForm.Allsamemapsize1Click'); {$EndIf}
   if GetNewBMPSize(MapDraw.MapXSize, MapDraw.MapYSize,'' ) then begin
      for i := 0 to pred(WMDEM.MDIChildCount) do begin
         if (WMDEM.MDIChildren[i] is tMapForm) then begin
            (WMDEM.MDIChildren[i] as TMapForm).MapDraw.MapDrawValid := false;
            Aspect := (WMDEM.MDIChildren[i] as TMapForm).MapDraw.GetMapAspectRatio;
            (WMDEM.MDIChildren[i] as TMapForm).MapDraw.MapXSize := MapDraw.MapXSize;
            (WMDEM.MDIChildren[i] as TMapForm).MapDraw.MapYSize := round(MapDraw.MapXSize / Aspect);
            (WMDEM.MDIChildren[i] as TMapForm).MapDraw.LatTickInt := MapDraw.LatTickInt;
            (WMDEM.MDIChildren[i] as TMapForm).FormResize(Nil);
            (WMDEM.MDIChildren[i] as TMapForm).DrawColoredMap1Click(Nil);
            {$IfDef RecordMatchMaps}
               WriteLineToDebugFile((WMDEM.MDIChildren[i] as TMapForm).Caption + ' redrawn, ' + IntToStr((WMDEM.MDIChildren[i] as TMapForm).MapDraw.MapXSize) +
                    'x' + IntToStr((WMDEM.MDIChildren[i] as TMapForm).MapDraw.MapYSize));
            {$EndIf}
         end;
      end;
   end;
end;


procedure TMapForm.SetMapDisplayType(dt : tMapType);
begin
   MapDraw.MapType := dt;
   if isSlopeMap(dt) then SetSlopedefaultColors(MDDef.NumSlopeBands,MapDraw.SlopeCut,MapDraw.SlopeColors);
   DoBaseMapRedraw;
end;



procedure TMapForm.Allsamepixelsizeasthismap1Click(Sender: TObject);
var
   i : integer;
begin
   for i := 0 to pred(WMDEM.MDIChildCount) do begin
      if (WMDEM.MDIChildren[i] is tMapForm) and (WmDEM.MDIChildren[i].Handle <> Handle) then begin
         MatchAnotherMapThisPixelSize(Self,WMDEM.MDIChildren[i] as TMapForm);
         (*
         if (not OtherMapSameSize(WMDEM.MDIChildren[i] as TMapForm)) and (not OtherMapSameCoverage(WMDEM.MDIChildren[i] as TMapForm)) then begin
            (WMDEM.MDIChildren[i] as TMapForm).SetMapPixelSize(MapDraw.ScreenPixelSize);
         end;
         *)
      end;
   end;
end;


procedure TMapForm.Allterraincategories1Click(Sender: TObject);
begin
   {$IfDef ExExoticMaps}
   {$Else}
      CreateRidgeMap(MapDraw.DEMOnMap,DEMGlb[MapDraw.DEMonMap].FullDEMGridLimits,rtmAllPoints);
   {$EndIf}
end;


procedure TMapForm.SAGAchannelnetwork1Click(Sender: TObject);
begin
   SagaChannelNetwork(DEMGlb[MapDraw.DEMonMap].SelectionMap.GeotiffDEMNameOfMap);
end;


procedure TMapForm.SAGAchannelnetworkallopenDEMs1Click(Sender: TObject);
var
   i : integer;
   InName,OutName,SHPName : PathStr;
begin
    for i := 1 to MaxDEMDataSets do begin
      if ValidDEM(i) then begin
         InName := DEMGlb[i].SelectionMap.GeotiffDEMNameOfMap;
         ShpName := ChangeFileExt(InName,'_channels.shp');
         SagaChannelShapefile(InName,ShpName);
      end;
   end;
end;

procedure TMapForm.SAGAChannelNetworkandBasins1Click(Sender: TObject);
var
   InName,ChannelName : PathStr;
   db : integer;
begin
   InName := DEMGlb[MapDraw.DEMonMap].SelectionMap.GeotiffDEMNameOfMap;
   ChannelName := MDTempDir + ExtractFileNameNoExt(InName) + '_channels.shp';
   db := SagaChannelShapefile(InName,ChannelName);
   OpenDBonMap('',ChannelName);
end;

procedure TMapForm.SAGADrainagebasins1Click(Sender: TObject);
begin
   SagaWatershedBasins(DEMGlb[MapDraw.DEMonMap].SelectionMap.GeotiffDEMNameOfMap);
end;

procedure TMapForm.SAGAedgecontaminationmap1Click(Sender: TObject);
begin
   SAGAedgeContaminationMap(DEMGlb[MapDraw.DEMonMap].SelectionMap.GeotiffDEMNameOfMap);
end;

procedure TMapForm.SAGAflowaccumulationParallelizable1Click(Sender: TObject);
begin
   SAGA_FlowAccumulationParallizeable(DEMGlb[MapDraw.DEMonMap].SelectionMap.GeotiffDEMNameOfMap);
end;

procedure TMapForm.SAGALSfactor1Click(Sender: TObject);
begin
   SAGA_LSFactor(true,DEMGlb[MapDraw.DEMonMap].SelectionMap.GeotiffDEMNameOfMap);
end;

procedure TMapForm.SAGAremovesinks1Click(Sender: TObject);
begin
   SagaSinkRemoval(DEMGlb[MapDraw.DEMonMap].SelectionMap.GeotiffDEMNameOfMap);
end;

procedure TMapForm.SAGAremovesinksallopenDEMs1Click(Sender: TObject);
begin
   SAGA_all_DEMs_remove_sinks;
end;

procedure TMapForm.SAGAStrahlerordergrid1Click(Sender: TObject);
begin
   SAGA_StrahlerOrderGrid(DEMGlb[MapDraw.DEMonMap].SelectionMap.GeotiffDEMNameOfMap);
end;

procedure TMapForm.SAGATPImap1Click(Sender: TObject);
begin
   SagaTPIMap(DEMGlb[MapDraw.DEMonMap].SelectionMap.GeotiffDEMNameOfMap);
end;

procedure TMapForm.SAGAVRMmapvectorruggedness1Click(Sender: TObject);
const
   Radius : integer = 5;
begin
   ReadDefault('Radius (pixels)',Radius);
   SagaVectorRuggednessMap(DEMGlb[MapDraw.DEMonMap].SelectionMap.GeotiffDEMNameOfMap,Radius);
end;

procedure TMapForm.SAGAwatershedbasinsWangLiu1Click(Sender: TObject);
begin
   SAGA_WatershedBasinsWangLiu(DEMGlb[MapDraw.DEMonMap].SelectionMap.GeotiffDEMNameOfMap);
end;

procedure TMapForm.RecolorMapWithElevationRange(Min,Max : float32);
begin
   MapDraw.MinMapElev := Min;
   MapDraw.MaxMapElev := Max;
   DoBaseMapRedraw;
end;

procedure TMapForm.SameElevationColors1Click(Sender: TObject);
var
   i : integer;
begin
   for i := 0 to pred(WMDEM.MDIChildCount) do
      if WMDEM.MDIChildren[i] is tMapForm and (WmDEM.MDIChildren[i].Handle <> Handle) then begin
         if (WMDEM.MDIChildren[i] as TMapForm).MapDraw.DEMMap then begin
           (WMDEM.MDIChildren[i] as TMapForm).RecolorMapWithElevationRange(MapDraw.MinMapElev,MapDraw.MaxMapElev);
         end;
      end;
end;

procedure TMapForm.Samehorizontaldatum1Click(Sender: TObject);
var
   InName,OutName : PathStr;
   Settings : shortstring;
begin
   InName := DEMGlb[MapDraw.DEMonMap].SelectionMap.GeotiffDEMNameOfMap;
   OutName := ChangeFileExt(InName,'.laz');
   Petmar.GetFileNameDefaultExt('laz compressed DEM name','.laz',OutName);
   if (Sender = Samehorizontaldatum1) then Settings := ''
   else Settings := ' -longlat -wgs84';
   Lastools_DEMToLAZ(InName,OutName,Settings);
end;


procedure TMapForm.Samehorizontaldatum2Click(Sender: TObject);
begin
   //this is not reprojected, but just reassigned (so failure for what we wanted)
   Samehorizontaldatum1Click(Sender);
end;

procedure TMapForm.Outlinecoveragearea1Click(Sender: TObject);
var
   i : integer;
begin
   for i := 0 to pred(WMDEM.MDIChildCount) do
      if (WMDEM.MDIChildren[i] is tMapForm) and (WmDEM.MDIChildren[i].Handle <> Handle) then begin
         (WMDEM.MDIChildren[i] as TMapForm).OutlineGeoBox(MapDraw.MapCorners.BoundBoxGeo,clRed,3);
      end;
end;


procedure TMapForm.OutlineGeoBox(bb : sfBoundBox; Color : tColor; LineSize : integer);
var
   xp,yp,xc,yc : integer;
begin
    Image1.Canvas.Pen.Color := Color;
    Image1.Canvas.Pen.Width := LineSize;
    Image1.Canvas.Pen.Mode := pmCopy;
    MapDraw.LatLongDegreeToScreen(bb.ymax,bb.xmin,xp,yp); Image1.Canvas.MoveTo(xp,yp);
    MapDraw.LatLongDegreeToScreen(bb.ymax,bb.xmax,xc,yc); Image1.Canvas.LineTo(xc,yc);
    MapDraw.LatLongDegreeToScreen(bb.ymin,bb.xmax,xc,yc); Image1.Canvas.LineTo(xc,yc);
    MapDraw.LatLongDegreeToScreen(bb.ymin,bb.xmin,xc,yc); Image1.Canvas.LineTo(xc,yc);
    Image1.Canvas.LineTo(xp,yp);
end;


procedure TMapForm.OutlineUTMBox(bb : sfBoundBox; Color : tColor; LineSize : integer);
var
   xp,yp,xc,yc : integer;
begin
    Image1.Canvas.Pen.Color := Color;
    Image1.Canvas.Pen.Width := LineSize;
    Image1.Canvas.Pen.Mode := pmCopy;
    MapDraw.UTMToScreen(bb.xmax,bb.ymin,xp,yp); Image1.Canvas.MoveTo(xp,yp);
    MapDraw.UTMToScreen(bb.xmax,bb.ymax,xc,yc); Image1.Canvas.LineTo(xc,yc);
    MapDraw.UTMToScreen(bb.xmin,bb.ymax,xc,yc); Image1.Canvas.LineTo(xc,yc);
    MapDraw.UTMToScreen(bb.xmin,bb.ymin,xc,yc); Image1.Canvas.LineTo(xc,yc);
    Image1.Canvas.LineTo(xp,yp);
end;



procedure TMapForm.Shapefilegrouplegend1Click(Sender: TObject);
begin
   LegendFromShapeFileGroup(Self,MapDraw.MapOverlays.ovShapeFileGroup);
end;


procedure TMapForm.Lookatpoints1Click(Sender: TObject);
begin
   Loadsummarymultipleclassifications1Click(Sender);
   ChangeDEMNowDoing(EnsembleClassSummary);
end;

procedure TMapForm.Greatcircleroute1Click(Sender: TObject);
begin
   Map_route.OverlayMapRoutes(Self,false);
end;


procedure TMapForm.Greatcirclethroughpoint1Click(Sender: TObject);
var
   Lat,Long,Lat2,Long2,Azimuth,Distance,dz : float64;
   fName : PathStr;
   Table : tMyData;


      procedure InsertPoint(Lat,Long,dz : float64);
      var
         z : float32;
      begin
          Table.Insert;
          Table.SetFieldByNameAsFloat('LAT',lat);
          PetMath.LongitudeAngleInRange(Long);
          Table.SetFieldByNameAsFloat('LONG',long);
          Table.SetFieldByNameAsFloat('DISTANCE',Dz);
          if (MapDraw.ValidDEMonMap) and (DEMGLB[MapDraw.DEMonMap].GetElevFromLatLongDegree(Lat,Long,z)) then
             Table.SetFieldByNameAsFloat('Z',z);
          Table.Post;
      end;


begin
    MapDraw.ScreenToLatLongDegree(LastX,LastY,Lat,Long);
    Azimuth := 90;
    repeat
       fName := Petmar.NextFileNumber(MDTempDir, 'rhumb_line',DefaultDBExt);
       Make_Tables.CreateLatLongZTable(fName,true,false,false,false,true,false);
       Table := tMyData.Create(fName);
       ReadDefault('Azimuth for rhumb line (' + DegSym + ')',Azimuth);
       dz := 100000;
       Distance := dz;
       InsertPoint(Lat,Long,0);
       while (Distance < 40075000) do begin
          VincentyPointAtDistanceBearing(Lat,Long,Distance,Azimuth,Lat2,Long2);
          InsertPoint(Lat2,Long2,dz);
          Distance := Distance + dz;
       end;
       Table.Destroy;
       LoadDataBaseFile(fName);
    until not AnswerIsYes('Another great circle through point');
end;

procedure TMapForm.RoamingZvaluesfrom2dgrid1Click(Sender: TObject);
begin
   GetSecondDEM(false);
   CheckProperTix;
end;


procedure TMapForm.Cancelpendingselection1Click(Sender: TObject);
begin
   ChangeDEMNowDoing(JustWandering);
end;


procedure TMapForm.CandeandKenttimescale1Click(Sender: TObject);
begin
   {$IfDef ExGeology}
   {$Else}
      LoadDataBaseFile(MagneticAnomalyTimeScale);
   {$EndIf}
end;

procedure TMapForm.Cartogrouplegend1Click(Sender: TObject);
begin
   LegendFromShapeFileGroup(Self,MapDraw.CartoGroupShapesUp);
end;

procedure TMapForm.ChangeElevUnits(Units : tElevUnit);
begin
   {$IfDef ExSat}
   {$Else}
      DEMGlb[MapDraw.DEMonMap].DEMheader.ElevUnits := Units;
      //DEMGlb[MapDraw.DEMonMap].DEMStatus := dsUnsavedEdits;
      DEMGlb[MapDraw.DEMonMap].WriteNewFormatDEM(DEMGlb[MapDraw.DEMonMap].DEMFileName);
      DEMGlb[MapDraw.DEMonMap].CheckForLandCover;
      DoBaseMapRedraw;
   {$EndIf}
end;


procedure TMapForm.CCAP1Click(Sender: TObject);
begin
   ChangeElevUnits(CCAP);
end;




procedure TMapForm.CenterpointsofallDBs1Click(Sender: TObject);
var
   i : integer;
   Lat,Long : float64;
   Centers : tStringList;
begin
    Centers := tStringList.Create;
    Centers.Add('DATABASE,LAT,LONG,LOCATION');
    for i := 1 to MaxDataBase do if ValidDB(i) then begin
       Long := 0.5 * (GISDB[i].dbBoundBox.xmax + GISDB[i].dbBoundBox.xmin);
       Lat := 0.5 * (GISDB[i].dbBoundBox.ymax + GISDB[i].dbBoundBox.ymin);
       Centers.Add(GISDB[i].dbName + ','+  RealToString(Lat,-12,-6) + ',' + RealToString(Long,-12,-6) + ','+ LatLongDegreeToString(Lat,Long,VeryShortDegrees))
   end;
   DisplayAndPurgeStringList(Centers,'Center point of open data bases');
end;


procedure TMapForm.EditGridViaColor(emvc : temvc; Color : tColor; LakeZ : float64 = -MaxSmallInt; SureOfColor : boolean = false; ShowResults : boolean = true);
var
   xg,yg,x,y,Masked,Pts : integer;
   z : float32;
   Bitmap : tMyBitmap;
   TStr : ShortString;
   MaskColor : tRGBTriple;
   p0 : pRGB;
   FastScreen : tScreenPRGB;    //this will have to be changed to make it thread safe
begin
    {$IfDef RecordColorMasking} WriteLineToDebugFile('TMapForm.Selectedcolor1Click in'); {$EndIf}
    ColorDialog1.Color := Color;
    SureOfColor := SureOfColor or (Color >= 0);
    if SureOfColor or (emvc in [emvcBasicMask,emvcSetElevation]) or ColorDialog1.Execute then begin
       if (emvc = emvcFlattenlake) then begin
          LakeZ := 0;
          ReadDefault('Lake level (m)',LakeZ);
       end;
       CopyImageToBitmap(Image1,Bitmap);
       {$IfDef RecordColorMasking} WriteLineToDebugFile('copied mask to temp directory'); Bitmap.SaveToFile(MDTempDir + 'Selectedcolor1-map.bmp'); {$EndIf}

       Masked := 0;
       Pts := 0;
       MaskColor := ConvertTColorToPlatformColor(ColorDialog1.Color);

       if ShowSatProgress then StartProgress('Color mask');
       if (MapDraw.MapZoomFactor > 1.01) then begin
          {$IfDef RecordColorMasking} WriteLineToDebugFile('use screen resolution'); {$EndIf}
          for y := 0 to pred(Bitmap.Height) do begin
             p0 := BitMap.ScanLine[y];
             if ShowSatProgress and (y mod 25 = 0) then UpdateProgressBar(y/Bitmap.Height);
             for x := 0 to pred(Bitmap.Width) do begin
                inc(Pts);
                if (emvc = emvcAllButSelectedColor) then begin
                   if not SameColor(p0[x],MaskColor) then begin
                      MapDraw.ScreenToDEMGrid(x,y,xg,yg);
                      DEMGlb[MapDraw.DEMonMap].SetGridMissing(xg,yg);
                      inc(Masked);
                   end;
                end
                else if SameColor(p0[x],MaskColor) then begin
                   MapDraw.ScreenToDEMGrid(x,y,xg,yg);
                   if (emvc = emvcAreaOfSingleColor) then begin
                      inc(Masked);
                   end
                   else if (emvc in [emvcFlattenlake,emvcSetElevation]) then begin
                      DEMGlb[MapDraw.DEMonMap].SetGridElevation(xg,yg,LakeZ);
                      inc(Masked);
                   end
                   else if ((emvc = emvcSelectedcolor) or (emvc = emvcBasicMask)) and DEMGlb[MapDraw.DEMonMap].GetElevMetersOnGrid(xg,yg,z) then begin
                      DEMGlb[MapDraw.DEMonMap].SetGridMissing(xg,yg);
                      inc(Masked);
                   end;
                end;
             end;
          end;
       end
       else begin
          {$IfDef RecordColorMasking} WriteLineToDebugFile('use DEM resolution'); {$EndIf}
          FillScanlineAddresses(Bitmap,FastScreen);
          for yg := 0 to pred(DEMGlb[MapDraw.DEMonMap].DEMheader.NumRow) do begin
             if ShowSatProgress and (yg mod 25 = 0) then UpdateProgressBar(yg/DEMGlb[MapDraw.DEMonMap].DEMheader.NumRow);
             for xg := 0 to pred(DEMGlb[MapDraw.DEMonMap].DEMheader.NumCol) do begin
                MapDraw.DEMGridToScreen(xg,yg,x,y);
                if MapDraw.OnScreen(x,y) then begin
                   inc(Pts);
                   if (emvc in [emvcFlattenlake,emvcSetElevation]) then begin
                      if (SameColor(FastScreen[y][x],MaskColor)) then begin
                         DEMGlb[MapDraw.DEMonMap].SetGridElevation(xg,yg,LakeZ);
                         inc(Masked);
                      end;
                   end
                   else if (emvc = emvcAllButSelectedColor) then begin
                      if (not SameColor(FastScreen[y][x],MaskColor)) then begin
                         DEMGlb[MapDraw.DEMonMap].SetGridMissing(xg,yg);
                         inc(Masked);
                      end;
                   end
                   else begin
                      if (SameColor(FastScreen[y][x],MaskColor)) then begin
                         DEMGlb[MapDraw.DEMonMap].SetGridMissing(xg,yg);
                         inc(Masked);
                      end;
                   end;
                end;
             end;
          end;
       end;
       Bitmap.Free;

       if ShowResults then begin
         if (emvc = emvcAreaOfSingleColor) then begin
            TStr := 'Area: ' +  SmartAreaFormat(Masked * sqr(DEMGlb[1].SelectionMap.MapDraw.ScreenPixelSize)) + MessLineBreak +
                   'Map pixel size: ' + RealToString(DEMGlb[1].SelectionMap.MapDraw.ScreenPixelSize,8,2) + ' m' + MessLineBreak +
                   'Points: ' + IntToStr(Masked);
         end
         else if (emvc = emvcFlattenlake) then TStr := 'Lake points set: '
         else TStr := 'Points masked: ';
         TStr := TStr + IntToStr(Masked) + '  (' + RealToString(100.0 * Masked /  Pts,-12,2) + '% of grid)';
         if (not (emvc in [emvcBasicMask,emvcSetElevation])) then MessageToContinue(TStr,(emvc = emvcAreaOfSingleColor));
       end;
       RespondToChangedDEM;
       {$If Defined(RecordEditsDEM) or Defined(RecordColorMasking)} WriteLineToDebugFile('TMapForm.Selectedcolor1Click out ' + TStr); {$EndIf}
    end;
end;


procedure TMapForm.EditVATDBFcolorscategorynames1Click(Sender: TObject);
begin
   OpenDBForModalEdit(DEMGlb[MapDraw.DEMonMap].VATFileName);
   DoBaseMapRedraw;
end;

procedure TMapForm.EditViaSelectedcolor1Click(Sender: TObject);
begin
   EditGridViaColor(emvcSelectedColor,-99);
end;


procedure TMapForm.Sectoroutlines1Click(Sender: TObject);
begin
   if Petmar.GetFileFromDirectory('Sector outlines',DefaultDBMask,MapDraw.SectorOutlines) then begin
      MapDraw.OutlineSectors(Image1.Canvas);
   end;
end;


procedure TMapForm.Sedimentthicknessversuscrustalage1Click(Sender: TObject);
{$IfDef ExGeology}
begin
{$Else}
var
   ThickDEM : integer;
   ThisGraph : TThisBaseGraph;
begin
   wmdem.SeaFloorAgeSpeedButtonClick(nil);
   wmdem.SedThickButtonClick(Nil);
   AllMapsMatchThisCoverageArea1Click(Nil);
   ThisGraph := DEMStat.GridScatterGram(DEMglb[PredAgesDEM].SelectionMap.MapDraw.MapAreaDEMGridLimits,PredAgesDEM,ThickDEM);
   ThisGraph.Caption := 'Age versus sediment thickness';
   ThisGraph.GraphDraw.HorizLabel := 'Age (Ma)';
   ThisGraph.GraphDraw.VertLabel := 'Sediment thickness (m)';
   ThisGraph.GraphDraw.NormalCartesianY := false;
   ThisGraph.RedrawDiagram11Click(Nil);
{$EndIf}
end;

procedure TMapForm.Seismicviewing1Click(Sender: TObject);
begin
{$IfDef ExFMX3D}
{$Else}
    StartSeismicViewing;
{$EndIf}
end;


procedure TMapForm.Selectedpercentilerange1Click(Sender: TObject);
begin
   Markasmissing1Click(Sender);
end;


procedure TMapForm.All11scale1Click(Sender: TObject);
var
   i : integer;
begin
   for i := 0 to pred(WMDEM.MDIChildCount) do
      if WMDEM.MDIChildren[i] is tMapForm then begin
         {$IfDef RecordMatchMaps} WriteLineToDebugFile('TMapForm.All11scale1Click for 100%, Map=' + (WMDEM.MDIChildren[i] as TMapForm).Caption); {$EndIf}
         (WMDEM.MDIChildren[i] as TMapForm).ResizeByPercentage(100);
      end;
end;


procedure TMapForm.All2Click(Sender: TObject);
var
   ThisOne : integer;
begin
   {$IfDef ExAdvancedSats}
   {$Else}
   if FindOpenMultigrid(ThisOne) then begin
      if StrUtils.AnsiContainsText(SatImage[MapDraw.SatOnMap].OriginalFileName,'EO1H') then begin
         OpenHyperionMultigrid(ThisOne,SatImage[MapDraw.SatOnMap].OriginalFileName);
      end
      else begin
         OpenLandsatOrSentinel2Multigrid(ThisOne,MapDraw.SatOnMap,true);
      end;
      MapDraw.MultiGridOnMap := ThisOne;
      if SatImage[MapDraw.SatOnMap].IsSentinel2 then Reinterpolatealltosameresolution1Click(nil);
   end;
   {$EndIf}
end;

procedure TMapForm.All3Click(Sender: TObject);
begin
  {$IfDef ExGeography}
  {$Else}
   MDDef.SunlightSingleDay := 2;
   SunAndHorizon(Self,0,RightClickLat,RightClickLong);
   {$EndIf}
end;

procedure TMapForm.All61Click(Sender: TObject);
begin
   LoadDEMIXCandidateDEMs('',{MapDraw.DEMonMap,}true,true);
end;

procedure TMapForm.AllbandsasGeotiffs1Click(Sender: TObject);
{$IfDef ExSat}
begin
{$Else}
var
   I : integer;
   fName,BaseDir : PathStr;
   bmp : tBitmap;
begin
   BaseDir := WriteSatDir;
   GetDOSPath('write Geotiffs',BaseDir);
   with SatImage[MapDraw.SATonMap],MapDraw,SatView do begin
       N11view1Click(Sender);
       WindowContrast := NoEnhancement;
       for I := 1 to NumBands do begin
          BandInWindow := i;
          DoBaseMapRedraw;
          bmp := PetImage.LoadBitmapFromFile(MapDraw.BaseMapFName);
          Image1.Picture.Graphic := bmp;
          bmp.Free;
          fName := BaseDir + SceneBaseName + '_' + BandLongName[i] + '.tif';
          SaveMapasGEOTIFFFile(fName,true);
       end;
   end;
   {$EndIf}
end;

procedure TMapForm.Allbutselectedcolor1Click(Sender: TObject);
begin
   EditGridViaColor(emvcAllbutselectedcolor,-99);
end;

procedure TMapForm.Allcurvatures1Click(Sender: TObject);
begin
   {$IfDef ExGeoStats}
   {$Else}
      SetAllCurvatures(true);
      MakeMomentsGrid(MapDraw.DEMonMap,'C');
   {$EndIf}
end;

procedure TMapForm.Maskmap1Click(Sender: TObject);
begin
   {$IfDef ExAdvancedGIS}
   {$Else}
      Map_Masking.MaskMap(Self,true,false,100);
   {$EndIf}
end;


procedure TMapForm.Maskothergridstomatchthisone1Click(Sender: TObject);
begin
   Maskallothergridstomatchthisone1Click(Sender);
end;


procedure TMapForm.ThinDEM1Click(Sender: TObject);
const
   Radius : integer = 2;
var
   NewDEM : integer;
   fName : PathStr;
begin
   if (Sender = ThinDEM1) then begin
      DEMGlb[MapDraw.DEMonMap].ThinThisDEM('',NewDEM);
   end
   else begin
      NakedMapOptions;   //which calls SaveBackupDefaults;
      DEMNowDoing := Calculating;
      NewDEM := 0;
      if (Sender = Filter1) or (Sender = PickFilter1) then DEMGlb[MapDraw.DEMonMap].FilterThisDEM(fcFilFile,NewDEM);
      if (Sender = Numberimmediateneighbors1) then DEMGlb[MapDraw.DEMonMap].FilterThisDEM(fcNumNeigh,NewDEM);
      if (Sender = Medianfilter1) then DEMGlb[MapDraw.DEMonMap].FilterThisDEM(fcMedian,NewDEM);
      if (Sender = Vectoraverage1) then DEMGlb[MapDraw.DEMonMap].FilterThisDEM(fcVectAvg,NewDEM);
      if (Sender = Meanfilter1) then DEMGlb[MapDraw.DEMonMap].FilterThisDEM(fcMean,NewDEM);
      if (Sender = MinimumFilter1) then DEMGlb[MapDraw.DEMonMap].FilterThisDEM(fcMin,NewDEM);
      if (Sender = MaximumFilter1) then DEMGlb[MapDraw.DEMonMap].FilterThisDEM(fcMax,NewDEM);
      if (Sender = Filledneighborhood1) then begin
         ReadDefault('Min neighbors required to retain',MDDef.ExpandNeighborsRequired);
         DEMGlb[MapDraw.DEMonMap].FilterThisDEM(fcNeighbors,NewDEM);
      end;
      if (Sender = Suminbox1) then DEMGlb[MapDraw.DEMonMap].FilterThisDEM(fcSum,NewDEM);
      if (Sender = StdDevinbox1) then DEMGlb[MapDraw.DEMonMap].FilterThisDEM(fcSTD,NewDEM);
      if (Sender = Parametricisotropicsmoothing1) then DEMGlb[MapDraw.DEMonMap].FilterThisDEM(fcParamIsotrop,NewDEM);
      if (Sender = RemoveTooFewSimilarNeighbors1) then DEMGlb[MapDraw.DEMonMap].FilterThisDEM(fcDissimilarNeighbors,NewDEM);
      if (Sender = Bytes1) then NewDEM := DEMGlb[MapDraw.DEMonMap].ResaveNewResolution(fcSaveByte);
      if (Sender = Integer161) then NewDEM := DEMGlb[MapDraw.DEMonMap].ResaveNewResolution(fcSaveSmallInt);
      if (Sender = Word1) then NewDEM := DEMGlb[MapDraw.DEMonMap].ResaveNewResolution(fcSaveWord);
      if (Sender = FloatingPoint1) then NewDEM := DEMGlb[MapDraw.DEMonMap].ResaveNewResolution(fcSaveFloatingPoint);
      if (Sender = Western1) then DEMGlb[MapDraw.DEMonMap].FindEdgeThisDEM(NewDEM,cdW);
      if (Sender = Eastern1) then DEMGlb[MapDraw.DEMonMap].FindEdgeThisDEM(NewDEM,cdE);
      if (Sender = SouthWestern1) then DEMGlb[MapDraw.DEMonMap].FindEdgeThisDEM(NewDEM,cdSW);
      //if (Sender = ResampleDEMgridbyaveraging1) then
      if (Sender = DetrendDEMgrid1) then begin
         Radius := 3;
         ReadDefault('radius to filter (pixels)',Radius);
         NewDEM := DEMGlb[MapDraw.DEMonMap].BoxcarDetrendDEM(true,DEMGlb[MapDraw.DEMonMap].FullDEMGridLimits, Radius);
         exit;
         //NewDEM := DEMGlb[MapDraw.DEMonMap].DetrendDEM(true,Radius);
      end;

      if (NewDEM <> 0) then begin
         DEMGlb[NewDEM].SetUpMap(NewDEM,true,MapDraw.MapType);
         fName := '';
         if MDdef.PromptToSaveNewDEMs then DEMGlb[NewDEM].WriteNewFormatDEM(fName);
      end;
      ChangeDEMNowDoing(JustWandering);
      RestoreBackupDefaults;
   end;
end;

procedure TMapForm.Medianfilter1Click(Sender: TObject);
begin
   ThinDEM1Click(Sender);
end;

procedure TMapForm.Filter1Click(Sender: TObject);
begin
   ThinDEM1Click(Sender);
end;

procedure TMapForm.Filtered1Click(Sender: TObject);
begin
   ThinDEM1Click(Sender);
end;

procedure TMapForm.Filtersizes1Click(Sender: TObject);
var
   Findings : tStringList;
   FilterSize : integer;
   x,y : integer;
   aline : shortstring;
   radius : float32;
begin
   FilterSize := 7;
   Findings := tStringList.Create;
   Findings.Add('FILTER,MINUS_7,MINUS_6,MINUS_5,MINUS_4,MINUS_3,MINUS_2,MINUS_1,ZERO,PLUS_1,PLUS_2,PLUS_3,PLUS_4,PLUS_5,PLUS_6,PLUS_7');
   for y := FilterSize downto -FilterSize do begin
      aline := '';
      for x := -FilterSize to FilterSize do begin
         radius := sqrt(sqr(x * DEMGlb[MapDraw.DEMonMap].AverageXSpace) + sqr(y * DEMGlb[MapDraw.DEMonMap].AverageYSpace));
         aline := aline + ',' + RealToString(Radius,-8,-3);
      end;
      Findings.Add(IntToStr(y) + aline);
   end;
   StringListToLoadedDatabase(Findings,Petmar.NextFileNumber(MDTempDir, 'filter_size_',DefaultDBExt));
end;


procedure TMapForm.Reinterpolatealltosameresolution1Click(Sender: TObject);
begin
   {$IfDef ExAdvancedSats}
   {$Else}
      MultiGridArray[MapDraw.MultiGridOnMap].ReinterpolateAllToSameResolution;
   {$EndIf}
end;

procedure TMapForm.Reinterpolatepickprojection1Click(Sender: TObject);
var
   NewDEM : integer;
begin
    NewDEM := MakeTempGrid(true,true);
    //DEMGlb[NewDEM].DEMHeader.RasterPixelIsGeoKey1025  := DEMGlb[MapDraw.DEMonMap].DEMHeader.RasterPixelIsGeoKey1025;
    //DEMGlb[NewDEM].DEMHeader.ElevUnits := DEMGlb[MapDraw.DEMonMap].DEMHeader.ElevUnits;

    DEMGlb[NewDEM].FillHolesSelectedBoxFromReferenceDEM(DEMGlb[NewDEM].FullDEMGridLimits,MapDraw.DEMonMap,hfOnlyHole);
    DEMGlb[NewDEM].SelectionMap.RespondToChangedDEM;
    DEMGlb[NewDEM].AreaName := DEMGlb[MapDraw.DEMonMap].AreaName + '_reinterpolated';
    DEMGlb[NewDEM].SelectionMap.SetUpNewDEMMapWindow(NewDEM,MapDraw.MapType,'',true,true,MapDraw.UsePercentiles);
end;

procedure TMapForm.CancelBtnClick(Sender: TObject);
begin
   BitBtn1Click(Sender);
end;

procedure TMapForm.Floodfill1Click(Sender: TObject);
begin
   ColorDialog1.Color := clRed;
   if ColorDialog1.Execute then begin
      Image1.Canvas.Brush.Color := ColorDialog1.Color;
      Image1.Canvas.Brush.Style := bsSolid;
      ChangeDEMNowDoing(FloodFill);
   end;
end;


procedure TMapForm.Floodingsealevelrise1Click(Sender: TObject);
begin
   {$IfDef ExDrainage}
   {$Else}
       Basin_flooding.BasinFlooding(Self,-99,-99);
   {$EndIf}
end;

procedure TMapForm.Setmappixelsize1Click(Sender: TObject);
var
   NewPixelSize : float64;
begin
   if LockMaps then begin
      MessageToContinue('Maps locked for digitizing');
      exit;
   end;
   NewPixelSize := MapDraw.ScreenPixelSize;
   ReadDefault('Map pixel size',NewPixelSize);
   SetMapPixelSize(NewPixelSize);
end;

procedure TMapForm.StratcolButtonClick(Sender: TObject);
begin
   {$IfDef ExGeology}
   {$Else}
      MapScreenX1 := -9999;
      ChangeDEMNowDoing(GetStratcolColumn);
   {$EndIf}
end;

procedure TMapForm.Requiredantennaheight1Click(Sender: TObject);
begin
   ChangeDEMNowDoing(FirstRequiredAntenna);
end;

procedure TMapForm.Setzrangetoaconstant1Click(Sender: TObject);
var
   NumPts : integer;
begin
   DEMGlb[MapDraw.DEMonMap].MarkElevationRangeAsConstant(NumPts);
   RespondToChangedDEM;
end;

procedure TMapForm.Expandgivenzrange1Click(Sender: TObject);
begin
   DEMGlb[MapDraw.DEMonMap].ExpandSpecifiedZRange;
   RespondToChangedDEM;
end;

procedure TMapForm.Exportcomputations1Click(Sender: TObject);
begin
   CSV_Export.ExportCSVLatLongGrid(Self);
end;

procedure TMapForm.Exportmaplibrary1Click(Sender: TObject);
begin
   CopyMapLibraryBox(MapDraw.Mapcorners.BoundBoxGeo);
end;

procedure TMapForm.SRTMwaterbodies2Click(Sender: TObject);
var
   xg,yg,Col,Row : integer;
   Bitmap : tMyBitmap;
   p0 : pRGB;
   waterrgb : TPlatformColor;
   OldMapType : tMapType;
begin
   OldMapType := MapDraw.MapType;
   MapDraw.MapType := mtDEMBlank;
   N11view1Click(Sender);
   CloneImageToBitmap(Image1,Bitmap);
   MapDraw.OverlaySRTMWaterBodies(Bitmap,true);
   WaterRGB := MDDef.WaterColor;
   for row := 0 to pred(Bitmap.Height) do begin
      P0 := BitMap.ScanLine[Row];
      for Col := 0 to pred(Bitmap.Width) do begin
         if SameColor(p0[Col],WaterRGB) then begin
            MapDraw.ScreenToDEMGrid(Col,Row,xg,yg);
            DEMGlb[MapDraw.DEMonMap].SetGridMissing(xg,yg);
         end;
      end;
   end;
   FreeAndNil(Bitmap);
   MapDraw.MapType := OldMapType;
   RespondToChangedDEM;
end;


procedure TMapForm.SSIM1Click(Sender: TObject);
var
   DEM1,DEM2 : integer;
   //SSIM,Luminance,Contrast,Structure : float64;
begin
   GetTwoCompatibleGrids('SSIM',false, DEM1,DEM2,false);   // then begin
   MakeSSIMMaps(DEM1,DEM2);
      //ComputeSSIM(DEM1,DEM2,DEMGlb[DEM1].FullDEMGridLimits,DEMGlb[DEM2].FullDEMGridLimits,SSIM,Luminance,Contrast,Structure);
   //end;
end;

procedure TMapForm.Overlays1Click(Sender: TObject);
begin
   Map_overlays.ManageMapOverlays(self);
   DoFastMapRedraw;
end;


procedure TMapForm.Customimagesize1Click(Sender: TObject);
begin
   Forcesize1Click(Sender);
end;

procedure TMapForm.Customzoom1Click(Sender: TObject);
begin
   if LockMaps then begin
      MessageToContinue('Maps locked for digitizing');
      exit;
   end;
   ReadDefault('Zoom level',MapDraw.CurrentZoomLevel);
   ResizeByPercentage(MapDraw.CurrentZoomLevel);
end;

procedure TMapForm.RGBgrayscale1Click(Sender: TObject);
begin
   NewSatWindow(nsbGrayscale);
end;

procedure TMapForm.RGBthreeparametermap1Click(Sender: TObject);
{$IfDef ExSat}
begin
{$Else}
var
    NewMap : tMapForm;
begin
    NewMap := DuplicateMap(false);
    ThreeGridRGBMap(NewMap);
{$EndIf}
end;

procedure TMapForm.RGBvalues1Click(Sender: TObject);
begin
   ChangeDEMNowDoing(GetRGBValues);
end;

procedure TMapForm.RICK1Click(Sender: TObject);
begin
   MakeTRIGrid(MapDraw.DEMonMap,nmRRI,true);
end;

procedure TMapForm.Mapshadingoptions1Click(Sender: TObject);
begin
   Map_options.SetMapOptions(Self);
end;

procedure TMapForm.Mapshadingoptions2Click(Sender: TObject);
begin
   Map_options.SetMapOptions(Self);
end;

procedure TMapForm.MaptoGoogleEarth1Click(Sender: TObject);
begin
   {$IfDef RecordKMLexport} WriteLineToDebugFile('TMapForm.MaptoGoogleEarth1Click in ' + Caption); {$EndIf}
   MDDef.AskAboutKMLExport := true;
   ExportMapToGoogleEarth(false);
end;

procedure TMapForm.Integer161Click(Sender: TObject);
begin
   ThinDEM1Click(Sender);
end;

procedure TMapForm.Integercode1Click(Sender: TObject);
begin
   DEMGlb[MapDraw.DEMonMap].SelectionMap.MapDraw.MapType := mtElevRainbow;
   ChangeElevUnits(euIntCode);
end;

procedure TMapForm.Integrateddatabasesetup1Click(Sender: TObject);
begin
   {$IfDef ExIndexes}
   {$Else}
      AdjustMapLibraryDataBaseSeries;
      DrawColoredMap1Click(Nil);
   {$EndIf}
end;

procedure TMapForm.ReloadDEMClick(Sender: TObject);
begin
    MapDraw.ClosingMapNow := true;
    DEMGlb[MapDraw.DEMonMap].DEMAlreadyDefined := false;
    DEMGlb[MapDraw.DEMonMap].ReloadDEM(true);
    FullDEM1Click(Sender);
    MapDraw.ClosingMapNow := false;
    DoBaseMapRedraw;
    CheckProperTix;
end;


procedure TMapForm.Copycoordinates1Click(Sender: TObject);
begin
   ClipBoard_Coords := true;
   MapDraw.ScreenToLatLongDegree(RightClickX,RightClickY,Clipboard_Lat,ClipBoard_Long);
   ClipBrd.ClipBoard.AsText := RealToString(Clipboard_Lat,-14,-8) + ',' +  RealToString(Clipboard_Long,-14,-8);
end;


procedure TMapForm.Copyimagecoordinatestoclipboard1Click(Sender: TObject);
{$IfDef ExSat}
begin
{$Else}
var
   xgrid,ygrid : float32;
begin
   if MapDraw.ValidSatOnMap then begin
      MapDraw.ScreenToDataGrid(Lastx,LastY,xgrid,ygrid);
      ClipBoard_Image_Coords := true;
      Clipboard_ImageX := round(xgrid);
      Clipboard_ImageY := round(ygrid);
   end;
{$EndIf}
end;

procedure TMapForm.CopyMap(var DrapeForm : tMapForm; HiddenDrape,CopyOverlays : boolean; Invisible : boolean = false);
begin
   {$IfDef RecordDrape} WriteLineToDebugFile('TMapForm.CopyMap, ' + Self.Caption + '  DEM on map=' + IntToStr(MapDraw.DEMonMap )+ '  Sat on map=' + IntToStr(MapDraw.SATonMap)); {$EndIf}
   if Invisible then CreateHiddenMap := true;
   if (DrapeForm = Nil) then DrapeForm := tMapForm.Create(Application);
   MapDraw.CopyMapParams(DrapeForm.MapDraw,CopyOverlays);
   DrapeForm.Closable := false;
   if Invisible or HiddenDrape then begin
      {$IfDef RecordDrape} WriteLineToDebugFile('Hidden drape'); {$EndIf}
      DrapeForm.MapDraw.BaseTitle := 'Draped map for 3D view';
      DrapeForm.FormStyle := fsNormal;
      DrapeForm.Visible := false;
      DrapeForm.MapDraw.MapOwner := moDrapeMap;
   end
   else DrapeForm.MapDraw.BaseTitle := 'Copy--' + MapDraw.BaseTitle;

   {$IfDef RecordDrape} WriteLineToDebugFile('TMapForm.CopyMap done'); {$EndIf}
end;


procedure TMapForm.Copymaptoclipboard1Click(Sender: TObject);
begin
   ClipboardSpeedButtonClick(Sender);
end;



function TMapForm.DuplicateMap(CopyOverlays : boolean; DrawIt : boolean = true; Invisible : boolean = false) : tMapForm;
begin
   Result := Nil;
   CopyMap(Result,False,CopyOverlays,Invisible);
   if MapDraw.DEMMap then Result.MapDraw.MapOwner := moNone
   else if (MapDraw.VectorIndex <> 0) then
   else begin
     {$IfDef ExSat}
     {$Else}
        Result.MapDraw.SatView := Self.MapDraw.SatView;
     {$EndIf}
   end;
   //Result.ADuplicateMap := true;
   if DrawIt then  begin
      Result.DoCompleteMapRedraw;
      Result.FullMapSpeedButton.Enabled := FullMapSpeedButton.Enabled;
   end
   else CheckProperTix;
end;

procedure TMapForm.oPrimeMeridian1Click(Sender: TObject);
begin
   DEMGlb[MapDraw.DEMonMap].ShiftGlobalDEM(0);
   MapDraw.DefineNewDEMMap(MapDraw.DEMonMap,MapDraw.MapType);
   DoCompleteMapRedraw;
end;

procedure TMapForm.Rowsnorthoflimit1Click(Sender: TObject);
begin
   ColumnsEastLimit1Click(Sender);
end;

procedure TMapForm.Rowssouthoflimit1Click(Sender: TObject);
begin
   ColumnsEastLimit1Click(Sender);
end;


procedure TMapForm.Trackpointfile1Click(Sender: TObject);
begin
   //Lineshapefile1Click(Sender);
   StreamProfileResults := Nil;
   ChangeDEMNowDoing(NewTrack);
end;


procedure TMapForm.Gridcorrelations2Click(Sender: TObject);
begin
   GridCorrelationMatrix;
end;

initialization
   InitializeDEMMapf;
finalization
   {$IfDef RecordClosing} WriteLineToDebugFile('Closing demmapf in'); {$EndIf}
   {$IfDef RecordPrinter} WriteLineToDebugFile('RecordPrinter active in DEMMapF'); {$EndIf}
   {$IfDef RecordAmbush} WriteLineToDebugFile('RecordAmbush active in DEMMapF'); {$EndIf}
   {$IfDef TrackNLCD} WriteLineToDebugFile('TrackNLCD active in DEMMapF'); {$EndIf}
   {$IfDef RecordContour} WriteLineToDebugFile('RecordContour active in DEMMapF'); {$EndIf}
   {$IfDef RecordAmbushDetailed} WriteLineToDebugFile('RecordAmbushDetailed active in DEMMapF'); {$EndIf}
   {$IfDef RecordMapDraw} WriteLineToDebugFile('RecordMapDraw active in DEMMapF'); {$EndIf}
   {$IfDef RecordMapRoam} WriteLineToDebugFile('RecordMapRoam active in DEMMapF (degrades performance)'); {$EndIf}
   {$IfDef RecordClosing} WriteLineToDebugFile('RecordClosing active in DEMMapF'); {$EndIf}
   {$IfDef RecordFly} WriteLineToDebugFile('RecordFly active in DEMMapF'); {$EndIf}
   {$IfDef Recordgeotiff} WriteLineToDebugFile('Recordgeotiff active in DEMMapF'); {$EndIf}
   {$IfDef RecordOpacity} WriteLineToDebugFile('RecordOpacity active in DEMMapF'); {$EndIf}
   {$IfDef RecordSettingOblique} WriteLineToDebugFile('RecordSettingOblique active in DEMMapF'); {$EndIf}
   {$IfDef RecordDrape} WriteLineToDebugFile('RecordDrape active in DEMMapF'); {$EndIf}
   {$IfDef RecordOblique} WriteLineToDebugFile('RecordOblique active in DEMMapF'); {$EndIf}
   {$IfDef RecordGISDB} WriteLineToDebugFile('RecordGISDB active in DEMMapF'); {$EndIf}
   {$IfDef RecordEditDB} WriteLineToDebugFile('RecordEditDB active in DEMMapF'); {$EndIf}
   {$IfDef RecordHorizon} WriteLineToDebugFile('RecordHorizon active in DEMMapF'); {$EndIf}
   {$IfDef RecordPLSS} WriteLineToDebugFile('RecordPLSS active in DEMMapF'); {$EndIf}
   {$IfDef RecordTiming} WriteLineToDebugFile('RecordTiming active in DEMMapF'); {$EndIf}
   {$IfDef RecordGeodeticCalculations} WriteLineToDebugFile('RecordGeodeticCalculations active in DEMMapF'); {$EndIf}
   {$IfDef RecordIntersection} WriteLineToDebugFile('RecordIntersection active in DEMMapF'); {$EndIf}
   {$IfDef RecordDigitize} WriteLineToDebugFile('RecordDigitize active in DEMMapF'); {$EndIf}
   {$IfDef RecordStreamModeDigitize} WriteLineToDebugFile('RecordStreamModeDigitize active in DEMMapF'); {$EndIf}
   {$IfDef RecordButton} WriteLineToDebugFile('RecordButton active in DEMMapF (very verbose)'); {$EndIf}
   {$IfDef RecordButtonPressed} WriteLineToDebugFile('RecordButtonPressed active in DEMMapF'); {$EndIf}
   {$IfDef MeasureDistance} WriteLineToDebugFile('MeasureDistance active in DEMMapF'); {$EndIf}
   {$IfDef RecordDrift} WriteLineToDebugFile('RecordDrift active in DEMMapF  (slowdown)'); {$EndIf}
   {$IfDef RecordFullDrift} WriteLineToDebugFile('RecordFullDrift active in DEMMapF  (slowdown)'); {$EndIf}
   {$IfDef RecordSolarPosition} WriteLineToDebugFile('RecordSolarPosition active in DEMMapF'); {$EndIf}
   {$IfDef RecordGeology} WriteLineToDebugFile('RecordGeology active in DEMMapF'); {$EndIf}
   {$IfDef RecordWeaponsFanTests} WriteLineToDebugFile('RecordWeaponsFanTests active in DEMMapF'); {$EndIf}
   {$IfDef RecordFlyRoute} WriteLineToDebugFile('RecordFlyRoute active in DEMMapF'); {$EndIf}
   {$IfDef RecordDoubleClick} WriteLineToDebugFile('RecordDoubleClick active in DEMMapF'); {$EndIf}
   {$IfDef MouseMoving} WriteLineToDebugFile('MouseMoving active in DEMMapF'); {$EndIf}
   {$IfDef RecordReqAnt} WriteLineToDebugFile('RecordReqAnt active in DEMMapF'); {$EndIf}
   {$IfDef RecordFullReqAnt} WriteLineToDebugFile('RecordFullReqAnt active in DEMMapF    //slowdown'); {$EndIf}
   {$IfDef RecordSat} WriteLineToDebugFile('RecordSat active in DEMMapF'); {$EndIf}
   {$IfDef RecordPlateRotations} WriteLineToDebugFile('RecordPlateRotations active in DEMMapF'); {$EndIf}
   {$IfDef RecordMergecolorscene} WriteLineToDebugFile('RecordMergecolorscene active in DEMMapF'); {$EndIf}
   {$IfDef RecordShapeFileEdits} WriteLineToDebugFile('RecordShapeFileEdits active in demMapF'); {$EndIf}
   {$IfDef RecordChangeDEMNowDoing} WriteLineToDebugFile('RecordChangeDEMNowDoing active in demMapF'); {$EndIf}
   {$IfDef RecordAssociateDEMwithImage} WriteLineToDebugFile('RecordAssociateDEMwithImage active in demMapF'); {$EndIf}
   {$IfDef RecordZoomWindow} WriteLineToDebugFile('RecordZoomWindow active in demMapF'); {$EndIf}
   {$IfDef RecordOpenVectorMap} WriteLineToDebugFile('RecordOpenVectorMap active in demMapF'); {$EndIf}
   {$IfDef RecordMapIndex} WriteLineToDebugFile('RecordMapIndex active in demMapF'); {$EndIf}
   {$IfDef RecordVAT} WriteLineToDebugFile('RecordVAT active in demMapF'); {$EndIf}
   {$IfDef RecordLoadClass} WriteLineToDebugFile('RecordLoadClass active in demMapF'); {$EndIf}
   {$IfDef RecordEditsDEM} WriteLineToDebugFile('RecordEditsDEM active in demMapF'); {$EndIf}
   {$IfDef RecordRadiusDBEdit} WriteLineToDebugFile('RecordRadiusDBEdit active in demMapF'); {$EndIf}
   {$IfDef RecordColorMasking} WriteLineToDebugFile('RecordColorMasking active in DEMMapF'); {$EndIf}
   {$IfDef RecordFullMapDraw} WriteLineToDebugFile('RecordFullMapDraw active in DEMMapF'); {$EndIf}
   {$IfDef RecordSatCoords} WriteLineToDebugFile('RecordSatCoords active in DEMMapF'); {$EndIf}
   {$IfDef RecordMaximizeMapCoverage} WriteLineToDebugFile('RecordMaximizeMapCoverage active in DEMMapF'); {$EndIf}
   {$IfDef RecordIHS} WriteLineToDebugFile('RecordIHS active in DEMMapF'); {$EndIf}
   {$IfDef MakeLandCoverGrid} WriteLineToDebugFile('MakeLandCoverGrid active in DEMMapF'); {$EndIf}
   {$IfDef RecordCircleAround} WriteLineToDebugFile('RecordCircleAround active in DEMMapF'); {$EndIf}
   {$IfDef ShowPickGridLimits} WriteLineToDebugFile('ShowPickGridLimits active in DEMMapF'); {$EndIf}
   {$IfDef RecordDrainage} WriteLineToDebugFile('RecordDrainage active in DEMMapF'); {$EndIf}
   {$IfDef RecordDrainageVectors} WriteLineToDebugFile('RecordDrainageVectors active in DEMMapF'); {$EndIf}
   {$IfDef RecordLAS} WriteLineToDebugFile('RecordLAS active in DEMMapF'); {$EndIf}
   {$IfDef RecordMrSID} WriteLineToDebugFile('RecordMrSID active in DEMMapF'); {$EndIf}
   {$IfDef HiresintervisibilityDEM} WriteLineToDebugFile('HiresintervisibilityDEM active in DEMMapF'); {$EndIf}
   {$IfDef RecordGetFabricAtPoint} WriteLineToDebugFile('RecordGetFabricAtPoint active in DEMMapF'); {$EndIf}
   {$IfDef RecordFormResize} WriteLineToDebugFile('RecordFormResize active in DEMMapF'); {$EndIf}
   {$IfDef RecordKMLexport} WriteLineToDebugFile('RecordKMLexport active in DEMMapF'); {$EndIf}
   {$IfDef RecordCollarMapMargins} WriteLineToDebugFile('RecordCollarMapMargins active in DEMMapF'); {$EndIf}
   {$IfDef RecordPitsSpires} WriteLineToDebugFile('RecordPitsSpires active in DEMMapF'); {$EndIf}
   {$IfDef RecordFresnel} WriteLineToDebugFile('RecordFresnel active in DEMMapF'); {$EndIf}
   {$IfDef RecordSatContrast} WriteLineToDebugFile('RecordSatContrast active in DEMMapF'); {$EndIf}
   {$IfDef RecordFan} WriteLineToDebugFile('RecordFan active in DEMMapF'); {$EndIf}
   {$IfDef RecordGlobeRotation} WriteLineToDebugFile('RecordGlobeRotation active in DEMMapF'); {$EndIf}
   {$IfDef RecordThreadCheckPlane} WriteLineToDebugFile('RecordThreadCheckPlane active in DEMMapF'); {$EndIf}
   {$IfDef RecordMapResize} WriteLineToDebugFile('RecordMapResize active in DEMMapF'); {$EndIf}
   {$IfDef RecordGeomorphometry} WriteLineToDebugFile('RecordGeomorphometry active in DEMMapF'); {$EndIf}
   {$IfDef RecordSetup} WriteLineToDebugFile('RecordSetup active in DEMMapF'); {$EndIf}
   {$IfDef RecordNewMaps} WriteLineToDebugFile('RecordNewMaps active in DEMMapF'); {$EndIf}
   {$IfDef NoParallelFor} WriteLineToDebugFile('NoParallelFor active in DEMMapF'); {$EndIf}
   {$IfDef RecordWMS} WriteLineToDebugFile('RecordWMS active in DEMMapF'); {$EndIf}
   {$IfDef NoParallelFor} WriteLineToDebugFile('NoParallelFor active in demmapf'); {$EndIf}
   {$IfDef RecordMultiGrids} WriteLineToDebugFile('RecordMultiGrids active in demmapf'); {$EndIf}
   {$IfDef Slicer} WriteLinetoDebugFile('Slicer active in demmapf'); {$EndIf}
   {$IfDef RecordHeatMap} WriteLinetoDebugFile('RecordHeadMap active in demmapf'); {$EndIf}
   {$IfDef RecordGDAL} WriteLinetoDebugFile('RecordGDAL active in demmapf'); {$EndIf}

   {$IfDef RecordClosing} WriteLineToDebugFile('Closing demmapf out'); {$EndIf}
end.

