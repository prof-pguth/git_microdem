unit BaseGraf;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch }
{   file verified 6/22/2011       }
{_________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems} //normally only defined for debugging specific problems
   {$IFDEF DEBUG}
       {$Define NoInLine}
       //{$Define RecordGraphMemoryAlocations}
       //{$Define RecordGrafProblems}
       {$Define RecordScaling}
       //{$Define RecordFormResize}
       //{$Define RecordHistogram}
       //{$Define RecordSaveSeries}
       //{$Define RecordGrafAxis}
       //{$Define RecordHistogram}
       //{$Define RecordGraphColors}
       //{$Define RecordGrafDensity}
       //{$Define RecordPlotFiles}
       //{$Define Closing}
       //{$Define ReverseFit}
       //{$Define TimeGraphing}
       //{$Define RecordTIN}
       //{$Define RecordDetailedTIN}
       //{$Define RecordFullFit}
       //{$Define RecordFit}
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

   Windows,
   SysUtils, Messages, Classes, Graphics, ClipBrd,Grids,
   System.UITypes,
   Forms, Dialogs, Math,
   Petmar_types,
   PETMar,     {common defintions and operation}
   PETMath,    {math library}
   DEMMapF,
   {$IfDef TIDES}
      TideCons,
   {$EndIf}
   Buttons, Vcl.ToolWin, Vcl.ComCtrls, Vcl.Menus, Vcl.ExtCtrls, Vcl.Controls;
const
   MaxContoursPerGrid = 25;
   MaxGridSize  = 100;
   TickSize     = 10;
   MaxCycles    = 100;
   MaxArraySize = 400;
   MaxPointsInBuffer = 64000;
type
   tGraphPlane = (gpXY,gpXZ,gpYZ);
   tDensityShow = (dsAll,dsAxis,dsYAxis);

   tFloatPoint = array[1..2] of float32;
   tPointDataBuffer = array[1..MaxPointsInBuffer] of tFloatPoint;
   tGraphDoing = (gdDragEdit,gdDoingNothing,gdResize,gdGraphDBFilter,gdGraphDBBoxFilter,gdGraphDigitize,gdIDDataBase,gdFirstDistance,gdSecondDistance,
           gdFirstSlope,gdSecondSlope,gdDoZShift,gdDigitYAxis,gdBoxOutline,gdBoxOutlineAdd,gdFreeDrag,gdXDrag,gdYDrag);
   tGraphType = (gtTernary,gtRose,gtNormal,gtTadpole,gtTwoVertAxes,gtBoxPlot);
   tTernaryGrid = (tgNone,tgRegular,tgShepSed);
   RealToRealFunction = function(x : float32) : float32;
   AxesType = (FullGrid,PartGrid,NoGrid,XFullGridOnly,XPartGridOnly,XTimeYFullGrid,XTimeYPartGrid);
   tAxisFunction = (LinearAxis,Log10Axis,ShortCumNormalAxis,CumulativeNormalAxis,LongCumulativeNormalAxis,LongerCumulativeNormalAxis);
   ColorFunctionType = Function(Z : float32) : TColor;
   tCycleCut = array[1..MaxCycles] of VectorType;   {with each, the order is min, max, increment}

   tGraphDraw = class
       protected
       private
       public
         HorizAxisFunct,VertAxisFunct               : RealToRealFunction;
         HorizAxisFunctionType,VertAxisFunctionType : tAxisFunction;
         ForceVertCycleSize,
         ForceVertTickIncr,
         ForceHorizCycleSize,
         ForceHorizTickIncr,
         ScrVertRange,ScrVertRange2,ScrHorizRange,

         ScrMinHorizAxis,ScrMaxHorizAxis,
         ScrMinVertAxis,ScrMaxVertAxis,
         ScrMinVertAxis2,ScrMaxVertAxis2,

         RegionXLo,RegionXHi,
         RegionYLo,RegionYHi,
         XPixelSize,YPixelSize,VertExag,
         MinHorizAxis,MaxHorizAxis,MinVertAxis,MaxVertAxis,MinVertAxis2,MaxVertAxis2 : float32;
         XWindowSize,YWindowSize,
         TopMargin,
         BottomMargin,
         LeftMargin,
         RightMargin,
         MarginFreeboard,
         FullLineFraction,
         NumHorizCycles,NumVertCycles,
         Day1,Month1,Year1,Day2,Month2,Year2 : int32;
         HorizCycleCuts,VertCycleCuts : tCycleCut;
         HorizLabel,VertLabel,ThirdLabel,VertLabel2,UpperLeftText : ShortString;
         AnnualCycle,
         Draw1to1Line,
         DrawRegion,
         PointCloudPanorama,
         PointCloudSlice,
         NormalCartesianX,
         NormalCartesianY : boolean;
         AxisColor : TColor;
         GraphAxes : AxesType;

         GraphType : tGraphType;
         GraphPlane : integer;   //0=xy;  1=xz;  2=yz;
         c1,c2,c3 : integer;     //xyz coords to use for graph planes

         ThirdPlaneConstant,ThirdPlaneThickness : float32;
         TernaryGrid : tTernaryGrid;
         UserSetVertCycles,
         UserSetHorizCycles,
         MonochromeBitmap,
         CorrectScaling,
         ForceNewSize,
         ForcePrinterDPI,
         LabelPointsAtop,
         RainBowColors,
         RedGray,
         HardColors,
         ZColorLegend,
         AutoPointDensity,
         SkipDrawing,
         ShowYears,
         LabelXFromLog,
         ShowHorizAxis0,
         GraphDrawn     : boolean;
         Symbol         : tSymbols15;
         ShowLine,
         ShowPoints     : array[1..50] of boolean;

         FileColors256  : tPlatformColors256;
         LineSize256    : tbytes256;

         SatBands       : shortstring;

         DataFilesPlottedTable : PathStr;
         LegendList,
         GraphTopLabels,
         GraphLeftLabels,
         RawFilesPlotted,
         XYZFilesPlotted,
         XYColorFilesPlotted,
         DataFilesPlotted,
         DBFPointFilesPlotted,
         DBFLineFilesPlotted      : tStringList;    {list of temporary files with data plotted on graph.  Files deleted when window closes.}

          constructor Create;
          destructor Destroy;
          function GraphX(x : float32) : integer; {$IfDef NoInLine} {$Else} inline; {$EndIf}
          function GraphY(y : float32) : integer; inline;
          function GraphY2(y : float32) : integer; inline;
          function AxisRange : shortstring;
          procedure SetMargins(Bitmap : tMyBitmap);

          function InvGraphY(y : integer) : float32;
          function InvGraphX(x : integer) : float32;
          function PtOnGraph(x,y : float32) : boolean; overload;  inline;
          function PtOnGraph(x,y : integer) : boolean; overload;  inline;
          procedure DefaultAxisFit(AxisFunctionType : tAxisFunction; NumPoints : integer; var x : array of float32; var CycleCuts : tCycleCut; var NumCycles : integer; var Min,Max : float32; PixelsHigh,TickSpacing : integer);
          procedure ManualAxisFit(var CycleCuts : tCycleCut; var NumCycles : integer; var Min,Max : float32; Message : shortstring; PixelsHigh,TickSpacing : integer);
          procedure ForceAxisFit(AxisFunctionType : tAxisFunction; var CycleCuts : tCycleCut; var NumCycles : integer; var Min,Max : float32; PixelsHigh,TickSpacing : integer);
          procedure SetShowAllLines(setting : boolean);
          procedure SetShowAllPoints(setting : boolean);
   end;

  TThisBaseGraph = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Rescale1: TMenuItem;
    Close1: TMenuItem;
    Saveimage1: TMenuItem;
    Option1: TMenuItem;
    ColorDialog1: TColorDialog;
    Graphparameters1: TMenuItem;
    Graphsettings2: TMenuItem;
    Font1: TMenuItem;
    FontDialog1: TFontDialog;
    Filter1: TMenuItem;
    None1: TMenuItem;
    N3term1: TMenuItem;
    N5term1: TMenuItem;
    n7term1: TMenuItem;
    N9term1: TMenuItem;
    Analyze1: TMenuItem;
    FFT1: TMenuItem;
    AlongXaxis1: TMenuItem;
    AlongYaxis1: TMenuItem;
    Scale1: TMenuItem;
    ScrollBox1: TScrollBox;
    Image1: TImage;
    Viewdata1: TMenuItem;
    Panel1: TPanel;
    Dragresize1: TMenuItem;
    Custom1: TMenuItem;
    ToolBar1: TToolBar;
    SpeedButton2: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton7: TSpeedButton;
    Hidden1: TMenuItem;
    ChangeGraphSettings: TMenuItem;
    Zcolorscalelegend1: TMenuItem;
    ContourLineWidth1: TMenuItem;
    LineandPointMarkers2: TMenuItem;
    Linearfit1: TMenuItem;
    Monthlyaverage1: TMenuItem;
    SpeedButton8: TSpeedButton;
    RedrawDiagram11: TMenuItem;
    Pointdensity1: TMenuItem;
    Labelpointsatopprofile1: TMenuItem;
    Linearfit2: TMenuItem;
    Ongraphdataonly1: TMenuItem;
    Blackandwhitegraph1: TMenuItem;
    PointDensity2: TMenuItem;
    Xaxis1: TMenuItem;
    Copytoclipboard1: TMenuItem;
    Showtoolbar1: TMenuItem;
    N1: TMenuItem;
    Medianalongx1: TMenuItem;
    Legend1: TMenuItem;
    Autocorrelation1: TMenuItem;
    Alongxaxis2: TMenuItem;
    Fitfouriercurve1: TMenuItem;
    Aongxaxis1: TMenuItem;
    Crosscorrelation1: TMenuItem;
    Alongxaxis3: TMenuItem;
    N11scaling1: TMenuItem;
    PopupMenu1: TPopupMenu;
    Rescalegraph1: TMenuItem;
    Lineandpointmarkers1: TMenuItem;
    N2: TMenuItem;
    Abortcurrentoperation1: TMenuItem;
    Copytoclipboard2: TMenuItem;
    Bestfitlinecolor1: TMenuItem;
    LegendSpeedButton: TSpeedButton;
    SpeedButton6: TSpeedButton;
    Alongyaxis2: TMenuItem;
    Blowupmap1: TMenuItem;
    Pastefromclipboard1: TMenuItem;
    Legend2: TMenuItem;
    SpeedButton9: TSpeedButton;
    SpeedButton10: TSpeedButton;
    IDSpeedButton: TSpeedButton;
    Zcolorrange1: TMenuItem;
    SpeedButton11: TSpeedButton;
    SpeedButton12: TSpeedButton;
    SpeedButton13: TSpeedButton;
    Keyboardresize1: TMenuItem;
    Separatehistograms1: TMenuItem;
    SpeedButton14: TSpeedButton;
    SpeedButton15: TSpeedButton;
    Yaxis1: TMenuItem;
    Saveimage2: TMenuItem;
    Edit1: TMenuItem;
    Freedrag1: TMenuItem;
    DragInXDirection1: TMenuItem;
    DraginYdirection1: TMenuItem;
    Grayscalegraph1: TMenuItem;
    extinlowerleftcorner1: TMenuItem;
    Blowupgraph1: TMenuItem;
    Lidarpanoramalimits1: TMenuItem;
    extinlowerrightcorner1: TMenuItem;
    N3: TMenuItem;
    DifferentSatellite1: TMenuItem;
    Landsat1: TMenuItem;
    Landsat2: TMenuItem;
    Sentinel21: TMenuItem;
    Sentinel22: TMenuItem;
    Spot51: TMenuItem;
    procedure IDSpeedButtonClick(Sender: TObject);
    procedure LegendSpeedButtonClick(Sender: TObject);
    procedure Bestfitlinecolor1Click(Sender: TObject);
    procedure Abortcurrentoperation1Click(Sender: TObject);
    procedure Lineandpointmarkers1Click(Sender: TObject);
    procedure Copytoclipboard2Click(Sender: TObject);
    procedure Rescalegraph1Click(Sender: TObject);
    procedure N11scaling1Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
    procedure Image1DblClick(Sender: TObject);
    procedure Close1Click(Sender: TObject);
    procedure Saveimage1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Graphparameters1Click(Sender: TObject);
    procedure Graphsettings2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Font1Click(Sender: TObject);
    procedure None1Click(Sender: TObject);
    procedure N3term1Click(Sender: TObject);
    procedure N5term1Click(Sender: TObject);
    procedure n7term1Click(Sender: TObject);
    procedure N9term1Click(Sender: TObject);
    procedure AlongYaxis1Click(Sender: TObject);
    procedure AlongXaxis1Click(Sender: TObject);
    procedure Scale1Click(Sender: TObject);
    procedure Viewdata1Click(Sender: TObject);
    procedure Dragresize1Click(Sender: TObject);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Image1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Custom1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure SpeedButton5Click(Sender: TObject);
    procedure SpeedButton7Click(Sender: TObject);
    procedure ChangeGraphSettingsClick(Sender: TObject);
    procedure Zcolorscalelegend1Click(Sender: TObject);
    procedure ContourLineWidth1Click(Sender: TObject);
    procedure LineandPointMarkers2Click(Sender: TObject);
    procedure Linearfit1Click(Sender: TObject);
    procedure Monthlyaverage1Click(Sender: TObject);
    procedure SpeedButton8Click(Sender: TObject);
    procedure RedrawDiagram11Click(Sender: TObject);
    procedure Pointdensity1Click(Sender: TObject);
    procedure Labelpointsatopprofile1Click(Sender: TObject);
    procedure Ongraphdataonly1Click(Sender: TObject);
    procedure Blackandwhitegraph1Click(Sender: TObject);
    procedure Xaxis1Click(Sender: TObject);
    procedure Copytoclipboard1Click(Sender: TObject);
    procedure Showtoolbar1Click(Sender: TObject);
    procedure Medianalongx1Click(Sender: TObject);
    procedure Legend1Click(Sender: TObject);
    procedure Alongxaxis2Click(Sender: TObject);
    procedure Aongxaxis1Click(Sender: TObject);
    procedure Alongxaxis3Click(Sender: TObject);
    procedure SpeedButton6Click(Sender: TObject);
    procedure Alongyaxis2Click(Sender: TObject);
    procedure Blowupmap1Click(Sender: TObject);
    procedure Pastefromclipboard1Click(Sender: TObject);
    procedure Legend2Click(Sender: TObject);
    procedure SpeedButton9Click(Sender: TObject);
    procedure SpeedButton10Click(Sender: TObject);
    procedure Zcolorrange1Click(Sender: TObject);
    procedure SpeedButton11Click(Sender: TObject);
    procedure SpeedButton12Click(Sender: TObject);
    procedure SpeedButton13Click(Sender: TObject);
    procedure Keyboardresize1Click(Sender: TObject);
    procedure Separatehistograms1Click(Sender: TObject);
    procedure SpeedButton14Click(Sender: TObject);
    procedure SpeedButton15Click(Sender: TObject);
    procedure Yaxis1Click(Sender: TObject);
    procedure Saveimage2Click(Sender: TObject);
    procedure Freedrag1Click(Sender: TObject);
    procedure DragInXDirection1Click(Sender: TObject);
    procedure DraginYdirection1Click(Sender: TObject);
    procedure Grayscalegraph1Click(Sender: TObject);
    procedure extinlowerleftcorner1Click(Sender: TObject);
    procedure Blowupgraph1Click(Sender: TObject);
    procedure Lidarpanoramalimits1Click(Sender: TObject);
    procedure extinlowerrightcorner1Click(Sender: TObject);
    procedure Landsat1Click(Sender: TObject);
    procedure Landsat2Click(Sender: TObject);
    procedure Sentinel21Click(Sender: TObject);
    procedure Sentinel22Click(Sender: TObject);
    procedure Spot51Click(Sender: TObject);
  private
    { Private declarations }
     procedure WindowACCORD(Canvas : TCanvas; ContInterval,NumContourLines,NumDataPoints : integer; Pnt : tPointerPnt; XMin,YMin,ZMin,DataX : float32; ColorFunction : ColorFunctionType; SaveTIN : PathStr = '');
     procedure SetMyBitmapColors(var Bitmap : tMyBitmap; i : integer);
     procedure SetMenus;
     procedure MakePointDensityGraph(DensityShow : tDensityShow);
     procedure FilterDBatCurrentPoint;
     procedure ShowSatelliteBands(Bitmap : tMyBitmap);
     procedure DrawBoxPlot(Bitmap : tMyBitmap);
  public
    { Public declarations }
     GraphDraw : tGraphDraw;
     MapOwner : tMapForm;
     DataBaseOnGraph : integer;
     ScrollGraph,
     SettingsChanged,
     CanCloseGraph,
     ShowProgress,
     RedrawingNow,
     RedrawNow,
     SizingWindow,
     HighlightBox,
     MouseIsDown : boolean;
     Symbol : tFullSymbolDeclaration;
     VertCompare,UserContourInterval,MaxZ,MinZ,MinZShow   : float32;
     BinSize,sx,sy,
     TernarySymSize,
     BoxLineWidth,
     PointsInDataBuffer,
     CurrentOverlay,
     FirstX,FirstY,
     LastX,LastY : integer;
     RoseColor  : tColor;
     RoseData         : ^CircleFreqType;
     SaveGraphName,
     RangeGraphName,
     ASCIIXYZFile        : PathStr;
     LLcornerText,LRcornerText,
     BaseCaption,
     RoseLegend,
     DBFXFieldName,DBFYFieldName,
     GraphFilter : shortstring;
     DataBaseFilter : string;
     XField,YField : string[35];
     PointDataBuffer : ^tPointDataBuffer;

    {$IfDef TIDES}
       PhaseAmplitude,PhaseShift  : PhaseArray;
       SimpleTides,StaticGraph    : boolean;
       TidalRecord                : ^tTidalRecord;

        procedure PlotOneCurve(Initialize : boolean);
        procedure PlotComplexTide(Initialize : boolean);
        procedure LatitudeTides;
        procedure SetUpTideGraphForm;
        procedure RedrawTideGraph;
    {$EndIf}

     procedure OpenDataFile(var rfile : file; ActuallyOpen : boolean = true; Color : tColor = -1);
     procedure OpenPointFile(var rfile : file; Symbol : tFullSymbolDeclaration);
     procedure OpenXYZFile(var rfile : file);
     procedure OpenXYColorFile(var rfile: file);
     procedure AddPointToDataBuffer(var rfile : file; v : tFloatPoint); overload;
     procedure AddPointToDataBuffer(var rfile : file; x,y : float32); overload;
     procedure ClosePointDataFile(var rfile : file);
     procedure ClearDataOnGraph;

     procedure PlotAFile(Bitmap : tMyBitmap; inf : PathStr; Count : integer);
     procedure PlotPointFile(Bitmap : tMyBitmap; inf : PathStr; Symbol : tFullSymbolDeclaration);
     procedure PlotXYZFile(Bitmap : tMyBitmap; inf : PathStr; ASCII : boolean = false);
     procedure PlotXYColorFile(Bitmap : tMyBitmap; inf : PathStr);
     procedure PlotLineDBFFile(Bitmap : tMyBitmap; inf : PathStr);
     procedure PlotPointDBFFile(Bitmap : tMyBitmap; inf : PathStr);

     procedure PlotPointOnGraph(x,y : float32; Symbol : tFullSymbolDeclaration);

     procedure SetUpGraphForm;
     procedure AutoScaleAndRedrawDiagram(DoVert : boolean = true; DoHoriz : boolean = true; PadX : boolean = true; PadY : boolean = true);
     procedure DrawGraph(Bitmap : tMyBitmap; DrawInside : boolean = true);
     procedure Filter(Terms : integer; Median : boolean = false);
     procedure DoFFT(XAxis,SeekPeaks  : boolean);
     procedure DrawBestFitLineOnGraph(a,b : float32);
     procedure BestSegmentFitGraph(nt : integer; fName : PathStr; var FittedInt,FittedSlope,FittedR : float32; var NPtsUsed : integer; PtsRequired : integer; SlopeDifferenceAllowed : float32);

     function MakeLegend(Flist : tstringList; Extract : boolean) : tMyBitmap;

     procedure InverseProjectTernaryComposition(xp,yp : integer;  var comp1, comp2, comp3 : float32);

     procedure ACCORD(Bitmap : tMyBitmap; ContInterval,NumContourLines,NumDataPoints : integer; Pnt : tPointerPnt; XMin,YMin,ZMin,DataX : float32; ColorFunction : ColorFunctionType; SaveTIN : PathStr);
     procedure ShowNormalDistribution;
     procedure SetDataBaseOnGraph(DBNum : integer; XField,YField : shortstring; Filter : string);

     procedure DrawRangesGraph;
     procedure DrawAspectRose(AspectFreqVals : CircleFreqType; Legend : ShortString = ''; BottomLegend : ShortString = '');
     procedure DrawTernaryDiagram(var Bitmap : tMyBitmap);
     procedure ProjectTernaryComposition(comp1, comp2, comp3 : float32; var xp,yp : integer);
     procedure PlotOnTernary(Bitmap : tMyBitmap; comp1, comp2, comp3 : float32);
     procedure DrawTernaryAxis(Bitmap : tMyBitmap);
     procedure InitializeTadpole(Title : shortstring; MinX,MaxX,MaxRange : float32);

     procedure HideToolbar(HideIt : boolean);
     procedure HideMenus;
     procedure SetBackgroundRegion(xlo,ylo,xhigh,yhigh : float32);
     procedure FitGraph(AllData : boolean; nt : integer; fName : PathStr; var a,b,r : float32; var n  : integer; LowLimitPoint : integer = 0; HighLimitPoint : integer = -1);
     procedure WriteDataSeriesASCII(XAxis,ASCII : boolean; var fftfilename : PathStr; var TotalNumberPoints : integer; DefSeries : integer = -1);
     procedure WindowGraphAxes(Bitmap : tMyBitmap; DrawInsideLines : boolean = true);
  end;


var
   ForceCycleSize,ForceTickIncr : float32;
   DefaultClientHeight,
   DefaultClientWidth : Int32;
   FittedSlope : float32;
   GraphDoing : tGraphDoing;
   CreateSmallGraph,
   CreateGraphHidden : boolean;
const
   AddDelauneyZ : boolean = true;
   AddDelauneyImage : boolean = false;


procedure DefaultGraphSettings(var GraphDraw : tGraphDraw);

procedure ComplicatedLocatePointOnGraph(Canvas : TCanvas;  GraphDraw : tGraphDraw;  x,y,sx,sy : integer);
procedure LocatePointOnGraph(Canvas : TCanvas; GraphDraw : tGraphDraw; x,y : integer);

function GraphAxesName(GraphAxes : AxesType) : shortstring;
function Linear(x : float32) : float32;

function SaveSingleValueSeries(NumVals : integer; var zs : Petmath.bfarray32; fName : PathStr = '' {; First : integer = 0; Last : integer = -999}) : PathStr;

function CreateMultipleHistograms(GraphNumbers : boolean; FileList,LegendList : tStringList; ParamName,TitleBar : ShortString;
    NumBins : integer = 100; Min : float32 = 1; Max : float32 = -1; BinSize : float32 =  -99) : TThisBaseGraph;

function DeprecatedCreateHistogram(GraphNumbers : boolean; NumVals : integer; var values : Petmath.bfarray32;
    ParamName,TitleBar : ShortString; NumBins : integer = 100; Min : float32 = 1; Max : float32 = -1; BinSize : float32 =  -99; Color1 : tColor = 0) : TThisBaseGraph;

procedure CreateQuantileQuantilePlot(var ThisGraph : TThisBaseGraph; NumVals : integer; var values : array of float32; Mean,Std : float32; ParamName,TitleBar : ShortString);
function CreateCumProbabilityFromFile(fNames : tStringList; ParamName,TitleBar : ShortString) : TThisBaseGraph;

procedure PadAxis(var Min,Max : float32);

procedure ChangeGraphDoing(ToWhat : tGraphDoing);
function NumOpenGraphs : integer;

procedure StartBoxPlot(DBonTable : integer);



implementation

{$R *.DFM}

uses
   DEMDefs,
   PetImage,
   PetGraphColors,
   Petimage_form,
   DEMdef_Routines,

   {$IfDef ExFourier}
   {$Else}
      CrossCor,
      FitFourier,
      PETFouri,   {form for display of FFT analysis}
      FourOpF,    {form to change FFT options}
   {$EndIf}

   {$IfDef ExGraphs}
   {$Else}
      GraphSet,   {form to modify graph settings and scaling}
   {$EndIf}

   {$IfDef ExTernary}
   {$Else}
      TernOptions,
   {$EndIf}

   {$IfDef ExSlicer3D}
   {$Else}
      Slicer_3D,
   {$EndIf}

   {$IfDef ExGIS}
   {$Else}
      PetDBUtils,
      DEMDataBase,
      BaseMap,
      DataBaseCreate,
   {$EndIf}

   {$IfDef ExTIN}
   {$Else}
      DEM_TIN,
   {$EndIf}

   Nevadia_Main, Make_tables;

const
   ShortProbCycleCuts : array[1..6] of VectorType = ( (5,10,1),(10,25,5), (25,50,5),(50,75,5),(75,90,5),(90,95,1) );
   ProbCycleCuts : array[1..8] of VectorType = ( (1,5,1),(5,10,1),(10,25,5), (25,50,5),(50,75,5),(75,90,5),(90,95,1),(95,99,1) );
   LongProbCycleCuts :  array[1..10] of VectorType = ( (0.1,1,0.1),(1,5,1),(5,10,1),(10,25,5), (25,50,5),(50,75,5),(75,90,5),(90,95,1),(95,99,1),(99,99.9,0.1) );
   LongerProbCycleCuts : array[1..10] of VectorType = ( (0.01,0.1,0.01),(0.1,1,0.1),(1,10,1),(10,25,5),(25,50,5),(50,75,5),(75,90,5),(90,99,1),(99,99.9,0.1),(99.9,99.99,0.01) );
const
   //MaxVariables = 3;     {number of possible end members}
   ASize = 2048;
type
   CoordArray = array[1..(2*ASize)] of float32;
   Coord3Array = array[1..(3*ASize)] of float32;
   CoordArray32 = array[1..(2*ASize)] of float32;
   Coord3Array32 = array[1..(3*ASize)] of float32;
var
   TernaryScreenMult : integer;
   CompNames     : array[1..3] of shortstring;
   BestFitLineColor : tPlatformColor;
   SavedGraphImage,
   LegendBitmap : tMyBitmap;
   BestFitLineWidth : integer;
   FilterTerms : integer;


{$IfDef Tides}
   {$I ..\tides\tidegraf.pas}
{$EndIf}


procedure TThisBaseGraph.DrawBoxPlot(Bitmap : tMyBitmap);
var
   y : float32;
   xi,yi,x1,x2 : integer;
   TStr : shortstring;
begin
   GISDB[DataBaseOnGraph].MyData.First;
   y := 1;
   while not GISDB[DataBaseOnGraph].MyData.eof do begin
      yi := GraphDraw.GraphY(y);
      TStr := RemoveUnderscores(GISdb[DataBaseOnGraph].MyData.GetFieldByNameAsString('NAME'));
      Bitmap.Canvas.TextOut(2,yi - Bitmap.Canvas.TextHeight(TStr) div 2,TStr);

      ScreenSymbol(Bitmap.Canvas,GraphDraw.GraphX(GISdb[DataBaseOnGraph].MyData.GetFieldByNameAsFloat('MIN') ),Yi,FilledBox,3,claRed);
      ScreenSymbol(Bitmap.Canvas,GraphDraw.GraphX(GISdb[DataBaseOnGraph].MyData.GetFieldByNameAsFloat('MAX') ),Yi,FilledBox,3,claRed);
      ScreenSymbol(Bitmap.Canvas,GraphDraw.GraphX(GISdb[DataBaseOnGraph].MyData.GetFieldByNameAsFloat('PC99') ),Yi,FilledBox,3,claRed);
      ScreenSymbol(Bitmap.Canvas,GraphDraw.GraphX(GISdb[DataBaseOnGraph].MyData.GetFieldByNameAsFloat('PC98') ),Yi,FilledBox,3,claRed);

      x1 := GraphDraw.GraphX(GISdb[DataBaseOnGraph].MyData.GetFieldByNameAsFloat('PC5'));
      x2 := GraphDraw.GraphX(GISdb[DataBaseOnGraph].MyData.GetFieldByNameAsFloat('PC95'));
      Bitmap.Canvas.MoveTo(x1,yi-5); Bitmap.Canvas.LineTo(x1,yi+5);
      Bitmap.Canvas.MoveTo(x2,yi-5); Bitmap.Canvas.LineTo(x2,yi+5);
      Bitmap.Canvas.MoveTo(x1,yi); Bitmap.Canvas.LineTo(x2,yi);

      x1 := GraphDraw.GraphX(GISdb[DataBaseOnGraph].MyData.GetFieldByNameAsFloat('Q1'));
      x2 := GraphDraw.GraphX(GISdb[DataBaseOnGraph].MyData.GetFieldByNameAsFloat('Q3'));
      Bitmap.Canvas.Rectangle(x1,yi-5,x2,yi+5);
      xi := GraphDraw.GraphX(GISdb[DataBaseOnGraph].MyData.GetFieldByNameAsFloat('MEAN'));
      Bitmap.Canvas.MoveTo(xi,yi-5); Bitmap.Canvas.LineTo(xi,yi+5);

      y := y + 1;
      GISDB[DataBaseOnGraph].MyData.Next;
   end;
end;


procedure StartBoxPlot(DBonTable : integer);
var
   Graph : tThisBaseGraph;
  begin
   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('OneField in, ' + aField); {$EndIf}
   GISdb[DBonTable].EmpSource.Enabled := false;

   Graph := tThisBaseGraph.Create(Application);
   Graph.GraphDraw.LegendList := tStringList.Create;
   Graph.GraphDraw.HorizLabel := RemoveUnderscores(GISdb[DBonTable].dbName);
   Graph.Caption := RemoveUnderscores(GISdb[DBonTable].DBName);
   Graph.DataBaseOnGraph := DBonTable;
   Graph.GraphDraw.GraphType := gtBoxPlot;

   Graph.GraphDraw.GraphAxes := XPartGridOnly;

   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Call Graph.AutoScaleAndRedrawDiagram'); {$EndIf}
   Graph.GraphDraw.MinVertAxis := 0;
   Graph.GraphDraw.MaxVertAxis := succ(GISDB[Graph.DataBaseOnGraph].MyData.FiltRecsInDB);
   Graph.GraphDraw.ShowHorizAxis0 := true;

   Graph.GraphDraw.MinHorizAxis := GISDB[Graph.DataBaseOnGraph].MyData.FindFieldMin('MIN');
   Graph.GraphDraw.MaxHorizAxis := GISDB[Graph.DataBaseOnGraph].MyData.FindFieldMin('MAX');

   Graph.AutoScaleAndRedrawDiagram(false,false);
   Graph.GraphDraw.LeftMargin := 450;

   Graph.Width := 1000;
   Graph.Height := 130 + 30 * GISDB[Graph.DataBaseOnGraph].MyData.FiltRecsInDB;
   Graph.RedrawDiagram11Click(Nil);

   {$If Defined(RecordDEMIX)} WriteLineToDebugFile('Graphsforwinecontest1Click out'); {$EndIf}

   GISdb[DBonTable].EmpSource.Enabled := true;
end;



      function GetAxisDecimals(inc : float32) : integer;
      begin
         if inc > 1 then Result := 0
         else if inc > 0.1 then Result := 1
         else if inc > 0.01 then Result := 2
         else if inc > 0.001 then Result := 3
         else if inc > 0.0001 then Result := 4
         else if inc > 0.00001 then Result := 5
         else if inc > 0.000001 then Result := 6;
      end;



procedure TThisBaseGraph.PlotPointOnGraph(x,y : float32; Symbol : tFullSymbolDeclaration);
begin
   if GraphDraw.PtOnGraph(x,y) then ScreenSymbol(Image1.Canvas, GraphDraw.GraphX(x),GraphDraw.Graphy(y),Symbol);
end;


function tGraphDraw.AxisRange : shortstring;
begin
   Result := ' vert axis: ' + RealToString(MinVertAxis,-18,-6) + '--' + RealToString(MaxVertAxis,-18,-6) + ' horiz axis:' + RealToString(MinHorizAxis,-18,-6) + '--' + RealToString(MaxHorizAxis,-18,-6);
end;

function NumOpenGraphs : integer;
var
   i : integer;
begin
   Result := 0;
   for i := pred(WMDEM.MDIChildCount) downto 0 do
      if WMDEM.MDIChildren[i] is TThisBaseGraph then begin
         inc(Result);
      end;
end;


procedure ChangeGraphDoing(ToWhat : tGraphDoing);
begin
   GraphDoing := ToWhat;
   case GraphDoing of
      gdGraphDBFilter  : WMDEM.StatusBar1.Panels[0].Text := 'Box for DBfilter';
      gdBoxOutlineAdd,
      gdBoxOutline     : WMDEM.StatusBar1.Panels[0].Text := 'Box to digitize';
      gdDoZShift       : WMDem.StatusBar1.Panels[0].Text := 'Pick point for z-shifting';
      gdFirstDistance  : WMDem.StatusBar1.Panels[0].Text := 'First point for distance';
      gdSecondDistance : WMDem.StatusBar1.Panels[0].Text := 'Second point for distance';
      gdDoingNothing   : WMDEM.StatusBar1.Panels[0].Text := '';
      gdXDrag          : WMDEM.StatusBar1.Panels[0].Text := 'Drag point in X direction';
      gdYDrag          : WMDEM.StatusBar1.Panels[0].Text := 'Drag point in Y direction';
      gdFreeDrag       : WMDEM.StatusBar1.Panels[0].Text := 'Drag point freely';
   end;
end;


function SaveSingleValueSeries(NumVals : integer; var zs : Petmath.bfarray32; fName : PathStr = '' {; First : integer = 0; Last : integer = -999}) : PathStr;
var
   inf : file;
   First,Last : integer;
begin
   //if (Last = -999) then Last := NumVals
   //else Last := Last - First;             \
   Last := NumVals;
   First := 0;
   if (fName = '') then Result := NextFileNumber(MDTempDir,'short_float_','.z')
   else Result := fName;

   AssignFile(inf,Result);
   rewrite(inf,sizeOf(float32));
   BlockWrite(inf,zs[First],Last);
   closeFile(inf);
   {$IfDef RecordSaveSeries} writeLineToDebugFile('SaveSingleValueSeries, n=' +IntToStr(NumVals) + '  ' + Result ); {$EndIf}
end;


function DigitsFromSize(Yis : float32) : integer;
begin
   if (yis > 0.1) then DigitsFromSize := 2
   else if (yis > 0.01) then DigitsFromSize := 3
   else if (yis > 0.001) then DigitsFromSize:= 4
   else if (Yis > 0.0001) then DigitsFromSize := 5
   else DigitsFromSize := 6;
end;


procedure TThisBaseGraph.MakePointDensityGraph(DensityShow : tDensityShow);
type
   tCounter = array[0..1920,0..1200] of SmallInt;
var
   NumRead,Max,
   nsx,nsy,i,j,x,y : integer;
   xf,yf : float32;
   tf    : file;
   TotNum,NumDone : LongInt;
   Coords : ^CoordArray;
   inf : PathStr;
   P0  : tScreenPRGB;
   Bitmap : tMyBitmap;
   Counter,Neighborhood : ^tCounter;
   Color : TRGBTriple;
begin
   {$IfDef RecordGrafDensity} WriteLineToDebugFile('TThisBaseGraph.Pointdensity1Click in'); {$EndIf}
   ShowHourglassCursor;
   CreateBitmap(Bitmap,succ(GraphDraw.XWindowSize),succ(GraphDraw.YWindowSize));
   Bitmap.Canvas.Font := FontDialog1.Font;

   DrawGraph(BitMap);
   New(Counter);
   for x := 0 to pred(Bitmap.Width) do
      for y := 0 to pred(Bitmap.Height) do Counter^[x,y] := 0;

   new(Coords);
   for i := 1 to GraphDraw.DataFilesPlotted.Count do begin
      inf := GraphDraw.DataFilesPlotted.Strings[pred(i)];
      {$IfDef RecordGrafDensity} WriteLineToDebugFile(inf); {$EndIf}
      TotNum := GetFileSize(inf) div (2*SizeOf(float32));
      assignFile(tf,inf);
      reset(tf,2*SizeOf(float32));
      if ShowProgress then StartProgressAbortOption('Points');
      NumDone := 0;
      while not EOF(tf) do begin
         BlockRead(tf,Coords^,ASize,Numread);
         inc(NumDone,NumRead);
         if ShowProgress then UpdateProgressBar(NumDone/TotNum);
         for x := 1 to NumRead do begin
            xf := Coords^[pred(2*x)];
            yf := Coords^[2*x];
            if GraphDraw.PtOnGraph(xf,yf) then inc(Counter^[GraphDraw.GraphX(xf),GraphDraw.GraphY(yf)]);
         end;
      end;
      EndProgress;
      closeFile(tf);
   end;
   Dispose(Coords);

   {$IfDef RecordGrafDensity} WriteLineToDebugFile('TThisBaseGraph.Pointdensity1Click point 2'); {$EndIf}

   for y := 0 to pred(Bitmap.Height) do P0[y] := Bitmap.ScanLine[y];
   if (DensityShow = dsAxis) then begin
      for x := 0 to pred(Bitmap.Width) do begin
         Max := 0;
         for y := 0 to pred(Bitmap.Height) do begin
            if Counter^[x,y] > Max then Max := Counter^[x,y];
         end;
         {$IfDef RecordGrafDensity} WriteLineToDebugFile('x=' + IntToStr(x) + '     Max=' + IntToStr(Max)); {$EndIf}
         if Max > 0 then begin
            for y := 0 to pred(Bitmap.Height) do begin
               if Counter^[x,y] > 0 then begin
                  p0[y][x] := RainbowRGBFunct(100 * Counter^[x,y]/Max,0,100);
               end;
            end;
         end;
      end;
   end
   else if (DensityShow = dsYAxis) then begin
      {$IfDef RecordGrafDensity} WriteLineToDebugFile('DensityShow = dsYAxis'); {$EndIf}
      for y := 0 to pred(Bitmap.Height) do begin
         Max := 0;
         for x := 0 to pred(Bitmap.Width) do begin
            if Counter^[x,y] > Max then Max := Counter^[x,y];
         end;
         {$IfDef RecordGrafDensity} WriteLineToDebugFile('y=' + IntToStr(y) + '  Max=' + IntToStr(Max)); {$EndIf}
         if Max > 0 then begin
            for x := 0 to pred(Bitmap.Width) do begin
               if Counter^[x,y] > 0 then begin
                  p0[y][x] := RainbowRGBFunct(100 * Counter^[x,y]/Max,0,100);
               end;
            end;
         end;
      end;
   end
   else begin
      nsx := 0;
      while succ(2*nsx) * GraphDraw.XPixelSize < MDDef.GraphDensityXBlock do inc(nsx);
      nsy := 0;
      while succ(2*nsy) * GraphDraw.YPixelSize < MDDef.GraphDensityXBlock do inc(nsy);
      New(Neighborhood);
      Max := 0;
      for x := 0 to pred(Bitmap.Width) do begin
         for y := 0 to pred(Bitmap.Height) do begin
            Neighborhood^[x,y] := 0;
            for i := -nsx to nsx do begin
               if (x+i >= 0) and (x+i < pred(Bitmap.Width)) then begin
                   for j := -nsy to nsy do begin
                      if (y+j >= 0) and (y+j < pred(Bitmap.Height)) then begin
                          Neighborhood^[x,y] := Neighborhood^[x,y] + Counter^[x+i,y+j];
                          if (Neighborhood^[x,y] > Max) then Max := Neighborhood^[x,y];
                      end;
                   end;
               end;
            end;
         end;
      end;

      {$IfDef RecordGrafDensity} WriteLineToDebugFile('Max=' + IntToStr(Max)); {$EndIf}

      for y := 0 to pred(Bitmap.Height) do begin
         for x := 0 to pred(Bitmap.Width) do begin
            if Neighborhood^[x,y] > 0 then begin
               Color := RainbowRGBFunct(Neighborhood^[x,y],0,Max);
               p0[y][x] := Color;
            end;
         end;
      end;
      Dispose(NeighborHood);
   end;

   if (not GraphDraw.SkipDrawing) then Image1.Picture.Graphic := Bitmap;
   Bitmap.Free;
   Dispose(Counter);
   {$IfDef RecordGrafDensity} WriteLineToDebugFile('TThisBaseGraph.Pointdensity1Click out'); {$EndIf}
end;


procedure TThisBaseGraph.DrawRangesGraph;
var
   Table : tMyData;
   y : integer;
   Std,Min,Max,High,Low,Middle : float32;
begin
   Table := tMyData.Create(RangeGraphName);
   while not Table.eof do begin
      y := GraphDraw.GraphY(Table.GetFieldByNameAsFloat('CLASS'));
      if MDDef.QuantileRanges then begin
        Middle := Table.GetFieldByNameAsFloat('MEDIAN');
        Low := Table.GetFieldByNameAsFloat('QUANT_25');
        High := Table.GetFieldByNameAsFloat('QUANT_75');
      end
      else begin
        Middle := Table.GetFieldByNameAsFloat('MEAN');
        Std := Table.GetFieldByNameAsFloat('STD_DEV');
        Low := Middle - Std;
        High := Middle + Std;
      end;
      Min := Table.GetFieldByNameAsFloat('MIN');
      Max := Table.GetFieldByNameAsFloat('MAX');

      Image1.Canvas.Pen.Color := 2;
      Image1.Canvas.Pen.Width := 1;
      Image1.Canvas.MoveTo( GraphDraw.GraphX(Min),y);
      Image1.Canvas.LineTo( GraphDraw.GraphX(Max),y);

      if (Low < Min) then Low := Min;

      Image1.Canvas.Pen.Width := 3;
      Image1.Canvas.MoveTo( GraphDraw.GraphX(Low),y);
      if (High > Max) then High := Max;
      Image1.Canvas.LineTo( GraphDraw.GraphX(High),y);

      Petmar.ScreenSymbol(Image1.Canvas,GraphDraw.GraphX(Min),Y,FilledBox,3,Table.PlatformColorFromTable);
      Petmar.ScreenSymbol(Image1.Canvas,GraphDraw.GraphX(Middle),Y,FilledBox,3,Table.PlatformColorFromTable);
      Petmar.ScreenSymbol(Image1.Canvas,GraphDraw.GraphX(Max),Y,FilledBox,3,Table.PlatformColorFromTable);
      Table.Next;
   end;
   Table.Destroy;
end;

procedure TThisBaseGraph.SetMyBitmapColors(var Bitmap : tMyBitmap; i : integer);
begin
   if (i > 255) then i := succ(255 mod 15);
   Bitmap.Canvas.Pen.Width := GraphDraw.LineSize256[i];
   Bitmap.Canvas.Pen.Color := ConvertPlatformColorToTColor(GraphDraw.FileColors256[i]);
   Bitmap.Canvas.Font.Color := Bitmap.Canvas.Pen.Color;
   {$IfDef RecordGraphColors} WritelineToDebugFile('TThisBaseGraph.SetMyBitmapColors, fn='+ IntToStr(I) + '  color=' +IntToStr(Bitmap.Canvas.Pen.Color)); {$EndIf}
end;


procedure TThisBaseGraph.SetDataBaseOnGraph(DBNum: integer; XField,YField : shortstring; Filter: string);
begin
   DataBaseOnGraph := DBNum;
   DataBaseFilter := Filter;
   Self.XField := XField;
   Self.YField := YField;
   IDSpeedButton.Visible := true;
end;


function CreateCumProbabilityFromFile(fNames : tStringList; ParamName,TitleBar : ShortString) : TThisBaseGraph;
var
   Coords : ^CoordArray;
   tf,uf : file;
   freq : float32;
   v : array[1..2] of float32;
   i,j,Numread,sum : integer;
begin
   Result := tThisBaseGraph.Create(Application);
   ShowHourglassCursor;
   Result.GraphDraw.HorizLabel := RemoveUnderscores(ParamName);
   Result.GraphDraw.VertLabel := 'Cumulative probability';
   Result.GraphDraw.VertAxisFunctionType  := LongCumulativeNormalAxis;
   Result.Caption := TitleBar;
   for j := 0 to pred(fNames.Count) do begin
      Result.OpenDataFile(uf);
      assignFile(tf,fNames[j]);
      reset(tf,2*SizeOf(float32));
      new(Coords);
      while not EOF(tf) do begin
         BlockRead(tf,Coords^,ASize,Numread);
         Sum := 0;
         for i := 1 to NumRead do sum := sum + round(Coords^[2*i]);
         freq := 0;
         for i := 1 to NumRead do begin
            v[1] := Coords^[pred(2*i)];
            freq := Freq + Coords^[2*i];
            v[2] := 100 * freq / sum;
            BlockWrite(uf,v,1);
         end;
         CloseFile(uf);
      end;
      Dispose(Coords);
      CloseFile(tf);
   end;
   ShowDefaultCursor;
   Result.AutoScaleAndRedrawDiagram(false,true);
end;


function CreateMultipleHistograms;
Label
   CleanUp;
const
   MaxBins = 5000;
var
  i,NumVals : integer;
  values : ^Petmath.bfarray32;
  Value,Range,Incr : float32;
  rFile : file;
  l1,l2 : shortstring;
  v : array[1..2] of float32;
  Bins : array[0..MaxBins] of integer;

          procedure LoadSeries(fName : PathStr);
          var
             inf : file;
             i : integer;
          begin
             NumVals := GetFileSize(fName) div SizeOf(float32);
             AssignFile(inf,fName);
             reset(inf,sizeOf(float32));
             BlockRead(inf,values^[0],NumVals);
             closeFile(inf);
             for i := 0 to pred(NumVals) do begin
                CompareValueToExtremes(Values[i],Min,Max);
             end;
          end;


         function ProcessSeries(fName : PathStr) : boolean;
         var
            inf : file;
            i,j,ax,ay : integer;
         begin
            LoadSeries(fName);
            NumVals := GetFileSize(fName) div SizeOf(float32);
            AssignFile(inf,fName);
            reset(inf,sizeOf(float32));
            BlockRead(inf,values^[0],NumVals);
            closeFile(inf);
            Result := (NumVals > 1) and ((Max - Min) > 0.00001);
            if Result then begin
               for j := 0 to NumBins do Bins[j] := 0;
               StartProgress('Histogram');
               for i := 0 to pred(NumVals) do begin
                  if (i mod 250 = 0) then UpdateProgressBar(i/NumVals);
                  Value := Values[i];
                  j := round((Value - Min) / BinSize);
                  if (j < 0) then j := 0;
                  if (j > NumBins) then j := NumBins;
                  inc(Bins[j]);
               end;
               EndProgress;
               CreateMultipleHistograms.OpenDataFile(rfile);
               if MDDef.FlipHistogram then begin
                  ax := 2;
                  ay := 1;
               end
               else begin
                  ax := 1;
                  ay := 2;
               end;
               for j := 0 to NumBins do begin
                  if Bins[j] > 0 then begin
                     v[ax] := Min + (j+0.5) * BinSize;
                     if GraphNumbers then v[ay] := Bins[j]
                     else v[ay] := Bins[j] / NumVals * NumBins;
                     BlockWrite(rfile,v,1);
                  end;
               end;
               CloseFile(rfile);
            end;
         end;


begin
   {$IfDef RecordHistogram} WriteLineToDebugFile('CreateMultipleHistograms in '); {$EndIf}
   New(Values);

   Result := TThisBaseGraph.Create(Application);
   if (LegendList <> Nil) then begin
      Result.GraphDraw.LegendList := tStringList.Create;
      for I := 0 to pred(LegendList.Count) do begin
         Result.GraphDraw.LegendList.Add(LegendList.Strings[i]);
         {$IfDef RecordHistogram} WriteLineToDebugFile(IntToStr(i) + ' ' + LegendList.Strings[i]); {$EndIf}
      end;
   end;
   ShowHourglassCursor;
   Result.Caption := TitleBar;
   Result.GraphDraw.SetShowAllPoints(false);
   Result.GraphDraw.ShowHorizAxis0 := true;

   l1 := RemoveUnderscores(ParamName);
   if MDDef.NoHistFreqLabels then l2 := ''
   else begin
      if GraphNumbers then l2 := 'Number of values'
      else l2 := 'Concentration';
   end;

   if MDDef.FlipHistogram then begin
      Result.GraphDraw.HorizLabel := l2;
      Result.GraphDraw.VertLabel := l1;
   end
   else begin
      Result.GraphDraw.HorizLabel := l1;
      Result.GraphDraw.VertLabel := l2;
   end;

   if (Min > Max) then begin
      Min := 99e39;
      Max := -99e39;
      for I := 0 to pred(FileList.Count) do begin
         {$IfDef RecordHistogram} writeLineToDebugFile('Load series ' + IntToStr(i) + ' ' + FileList.Strings[i]); {$EndIf}
         LoadSeries(FileList.Strings[i]);
      end;
      Range := Max-Min;
      if abs(Range) < 0.00001 then begin
         MessageToContinue('Only one value in data set');
         Result.Close;
         goto Cleanup;
      end;

      if (Range > 0.001) and (Range < 100000) then begin
         if Range < 0.01 then Incr :=  0.005
      end;

      if MDDef.AskHistogramBins then begin
         ReadDefault('Minimum value',Min);
         ReadDefault('Maximum value',Max);
         ReadDefault('Number of bins',NumBins);
      end;
   end
   else begin
      NumBins := succ(Round((Max-Min)/BinSize));
      Result.GraphDraw.MinVertAxis := Min;
      Result.GraphDraw.MaxVertAxis := Max;
   end;
   if (NumBins > MaxBins) then NumBins := MaxBins;
   BinSize := (Max-Min) / NumBins;
   {$IfDef RecordHistogram} writeLineToDebugFile('CreateMultipleHistograms NumBins=' + IntToStr(NumBins) + '  Max=' + RealToString(Max,-18,-6) + '  Min=' + RealToString(Min,-18,-6)); {$EndIf}
   for I := 0 to pred(FileList.Count) do begin
      if ProcessSeries(FileList.Strings[i]) then begin
         {$IfDef RecordHistogram} writeLineToDebugFile('Process series ' + IntToStr(i) + ' ' + FileList.Strings[i]); {$EndIf}
         Result.GraphDraw.ShowLine[succ(i)] := true;
      end;
   end;
   Result.AutoScaleAndRedrawDiagram(true,true,false,false);
CleanUp:;
   Dispose(Values);
   ShowDefaultCursor;
   MDDef.FlipHistogram := false;
end;


function DeprecatedCreateHistogram(GraphNumbers : boolean; NumVals : integer; var values : Petmath.bfarray32; ParamName,TitleBar : ShortString;
    NumBins : integer = 100; Min : float32 = 1; Max : float32 = -1; BinSize : float32 =  -99; Color1 : tColor = 0) : TThisBaseGraph;
var
   FileList : tStringList;
begin
   {$IfDef RecordGraphColors} WritelineToDebugFile('Create histogram, color1 = '+ IntToStr(Color1)); {$EndIf}
   FileList := tStringList.Create;
   FileList.Add(SaveSingleValueSeries(NumVals,Values));
   Result := CreateMultipleHistograms(GraphNumbers,FileList,Nil,ParamName,TitleBar);
end;


procedure CreateQuantileQuantilePlot(var ThisGraph : TThisBaseGraph; NumVals : integer; var values : array of float32; Mean,Std : float32; ParamName,TitleBar : ShortString);
var
   dx : float32;
   i : integer;
   rFile : file;
   v : array[1..2] of float32;
begin
   ThisGraph := TThisBaseGraph.Create(Application);
   ShowHourglassCursor;
   ThisGraph.GraphDraw.HorizLabel := 'Gaussian';
   ThisGraph.GraphDraw.VertLabel := RemoveUnderscores(ParamName);
   ThisGraph.GraphDraw.Draw1to1Line := true;
   ThisGraph.Caption := TitleBar;
   HeapSort(NumVals,values);
   dx := 100 / succ(NumVals);

   ThisGraph.OpenDataFile(rfile);
   for i := 1 to NumVals do begin
      v[1] := Mean + std * Petmath.ninv(i*dx);
      v[2] := Values[pred(i)];
      BlockWrite(rfile,v,1);
   end;
   CloseFile(Rfile);

   ThisGraph.AutoScaleAndRedrawDiagram(true,true);
   ThisGraph.Image1.Canvas.TextOut(0,ThisGraph.Image1.Height - 20, 'Mean=' + RealToString(Mean,-12,2) +  '  StdDev=' + RealToString(std,-12,2));
   ShowDefaultCursor;
end;

procedure TThisBaseGraph.SetMenus;
begin
   Legend1.Visible := (GraphDraw.DBFLineFilesPlotted.Count > 0) or (GraphDraw.LegendList <> Nil) and (GraphDraw.GraphType <> gtRose);
   SpeedButton6.Visible := (GraphDraw.GraphType <> gtRose);
   SpeedButton9.Visible := (GraphDraw.GraphType <> gtRose);
   SpeedButton10.Visible := (GraphDraw.GraphType <> gtRose);
   SpeedButton11.Visible := (GraphDraw.GraphType <> gtRose);
   SpeedButton12.Visible := (GraphDraw.GraphType <> gtRose);
   SpeedButton13.Visible := (GraphDraw.GraphType <> gtRose);

   Zcolorrange1.Visible := (GraphDraw.XYZFilesPlotted <> Nil) and (GraphDraw.XYZFilesPlotted.Count > 0);
   Legend2.Visible := Legend1.Visible or Zcolorrange1.Visible;
   LegendSpeedButton.Visible := Legend1.Visible;
   CrossCorrelation1.Visible := (GraphDraw.DataFilesPlotted.Count > 1);
   Labelpointsatopprofile1.Visible := (GraphDraw.GraphTopLabels <> Nil);
   MonthlyAverage1.Visible := GraphDraw.GraphAxes in [XTimeYFullGrid,XTimeYPartGrid];
end;


procedure TThisBaseGraph.HideMenus;
begin
   File1.Visible := false;
   Rescale1.Visible := false;
   Analyze1.Visible := false;
   Option1.Visible := false;
end;


procedure TThisBaseGraph.HideToolbar(HideIt : boolean);
begin
   Toolbar1.Visible := not HideIt;
   if HideIt then ToolBar1.Height := 0
   else ToolBar1.Height := 28;
   FormResize(nil);
end;


procedure TThisBaseGraph.Sentinel21Click(Sender: TObject);
begin
   GraphDraw.SatBands := 'Sentinel-2';
   RedrawDiagram11Click(Nil);
end;

procedure TThisBaseGraph.Sentinel22Click(Sender: TObject);
begin
   GraphDraw.SatBands := 'WV2';
   RedrawDiagram11Click(Nil);
end;

procedure TThisBaseGraph.Separatehistograms1Click(Sender: TObject);
var
   i,db : integer;
   Words : tStringList;
   TStr : ANSIString;
   fName : PathStr;
begin
   Words := tStringList.Create;
   Words.Add('NAME,PLOT,GRAY,LINE_WIDTH,LINE_COLOR,FILENAME');
   with GraphDraw do begin
       for i := 1 to DataFilesPlotted.Count do begin
          if (LegendList <> Nil) then begin
             TStr := LegendList.Strings[pred(i)];
             ReplaceCharacter(TStr,',',' ');
          end
          else TStr := 'Series ' + IntToStr(i);
          Words.Add(TStr + ',Y,N,' + IntToStr(LineSize256[i]) + ',' +  IntToStr(ConvertPlatformColorToTColor(FileColors256[i])) + ',' + DataFilesPlotted.Strings[pred(i)]);
       end;
   end;
   fName := NextFileNumber(MDTempDir, 'graph_file_list', '.csv');
   Words.SaveToFile(fName);
   db := OpenDataBase('',fName);
   GraphDraw.DataFilesPlottedTable := ChangeFileExt(fName,DefaultDBExt);
   GISdb[db].dbtablef.EditSymbologyOnly := true;
end;


procedure TThisBaseGraph.SetBackgroundRegion(xlo,ylo,xhigh,yhigh : float32);
begin
   GraphDraw.RegionXLo := xlo;
   GraphDraw.RegionXHi := xhigh;
   GraphDraw.RegionYLo := ylo;
   GraphDraw.RegionYHi := yhigh;
   GraphDraw.DrawRegion := true;
end;

procedure TThisBaseGraph.ProjectTernaryComposition(comp1, comp2, comp3 : float32; var xp,yp : integer);
var
   Total,pcv1,pcv2 : float32;
begin
   Total := comp1 + comp2 + comp3;
   if (Total < 0.0001) then begin
      xp := -1;
      yp := -1;
   end
   else begin
      pcv1 := comp1 / Total * 100;
      pcv2 := comp2 / Total * 100;
      {pcv3 := comp3 / Total * 100;} {redundant-- does not have to calculated}
      xp := TernaryScreenMult * (275 - trunc(( (230 * (199 * pcv2) / 100) / 198)) - trunc(( (199 * pcv1) / 100) * 0.5774));
      yp :=  TernaryScreenMult * (199 - trunc(( (199 * pcv1) / 100)));
   end;
end;

procedure TThisBaseGraph.InverseProjectTernaryComposition(xp,yp : integer;  var comp1, comp2, comp3 : float32);
begin
   Comp1 := (199 - (yp / TernaryScreenMult)) / 1.99;
   Comp2 := 100 - (198 * (275 - (xp / TernaryScreenMult) +  (( (199 * Comp1) / 100) * 0.5774)) / 2.3 / 199);
   Comp3 := 100 - Comp1 - Comp2;
end;

procedure TThisBaseGraph.PlotOnTernary(Bitmap : tMyBitmap; comp1, comp2, comp3 : float32);
var
   xp,yp : integer;
   sum : float32;
   color : tPlatFormColor;
begin
   ProjectTernaryComposition(comp1, comp2, comp3,xp,yp);
   sum := comp1 + comp2 + comp3;
   Color := ConvertTColorToPlatformColor(RGB(round(255*comp1/sum),round(255*comp2/sum),round(255*comp3/sum)));
   ScreenSymbol(Bitmap.Canvas,xp,yp,FilledBox,TernarySymSize,color);
end;


procedure TThisBaseGraph.DrawTernaryAxis(Bitmap : tMyBitmap);
const
   GridInc = 10;
var
   i,x1,y1,x2,y2,x3,y3,x4,y4 : integer;

         procedure AppropriateDraw(x1,y1,x2,y2,Width : integer);
         begin
            Bitmap.Canvas.Pen.Width := Width;
            Bitmap.Canvas.MoveTo(x1,y1);
            Bitmap.Canvas.LineTo(x2,y2);
         end;

         procedure ThreeValues(v1,v2,v3 : integer);
         begin
            ProjectTernaryComposition(v1,v2,v3,x1,y1);
            ProjectTernaryComposition(v1,v3,v2,x2,y2);
            AppropriateDraw(x1,y1,x2,y2,2);
            ProjectTernaryComposition(v3,v1,v2,x1,y1);
            ProjectTernaryComposition(v2,v1,v3,x2,y2);
            AppropriateDraw(x1,y1,x2,y2,2);
            ProjectTernaryComposition(v3,v2,v1,x1,y1);
            ProjectTernaryComposition(v2,v3,v1,x2,y2);
            AppropriateDraw(x1,y1,x2,y2,2);
          end;

          procedure OneTheSame(v1,v2,v3,v4 : float32);
          begin
            ProjectTernaryComposition(v1,v2,v2,x1,y1);
            ProjectTernaryComposition(v3,v4,v4,x2,y2);
            AppropriateDraw(x1,y1,x2,y2,2);

            ProjectTernaryComposition(v2,v1,v2,x1,y1);
            ProjectTernaryComposition(v4,v3,v4,x2,y2);
            AppropriateDraw(x1,y1,x2,y2,2);

            ProjectTernaryComposition(v2,v2,v1,x1,y1);
            ProjectTernaryComposition(v4,v4,v3,x2,y2);
            AppropriateDraw(x1,y1,x2,y2,2);
          end;


begin
   ProjectTernaryComposition(100,0,0,x1,y1);
   ProjectTernaryComposition(0,100,0,x2,y2);
   ProjectTernaryComposition(0,0,100,x3,y3);
   AppropriateDraw(x1,y1,x2,y2,3);
   AppropriateDraw(x1,y1,x3,y3,3);
   AppropriateDraw(x2,y2,x3,y3,3);

   if GraphDraw.TernaryGrid <> tgNone then begin
      Bitmap.Canvas.Pen.Color := clSilver;
      if GraphDraw.TernaryGrid = tgRegular then begin
         i := GridInc;
         while (i < 100) do begin
            ProjectTernaryComposition(i,100-i,0,x1,y1);
            ProjectTernaryComposition(i,0,100-i,x2,y2);
            ProjectTernaryComposition(0,i,100-i,x3,y3);
            ProjectTernaryComposition(100-i,i,0,x4,y4);
            AppropriateDraw(x1,y1,x2,y2,1);
            AppropriateDraw(x4,y4,x3,y3,1);
            AppropriateDraw(x2,y2,x3,y3,1);
            i := i + GridInc;
         end;
      end
      else begin
         ThreeValues(75,25,0);
         OneTheSame(75,12.5,60,20);
         OneTheSame(0,50,20,40);
         ProjectTernaryComposition(20,20,60,x1,y1);
         ProjectTernaryComposition(20,60,20,x2,y2);
         ProjectTernaryComposition(60,20,20,x3,y3);
         AppropriateDraw(x1,y1,x2,y2,3);
         AppropriateDraw(x1,y1,x3,y3,3);
         AppropriateDraw(x2,y2,x3,y3,3);
      end;
   end;

   CompNames[1] := 'Clay';
   CompNames[2] := 'Sand';
   CompNames[3] := 'Silt';

   with GraphDraw do begin
      Bitmap.Canvas.TextOut(10+160*TernaryScreenMult,0,HorizLabel);
      Bitmap.Canvas.TextOut(45*TernaryScreenMult-8*Length(CompNames[2]),200*TernaryScreenMult,VertLabel);
      Bitmap.Canvas.TextOut(275*TernaryScreenMult,200*TernaryScreenMult,ThirdLabel);
   end;
end;


procedure TThisBaseGraph.DrawTernaryDiagram(var Bitmap : tMyBitmap);
begin
   ClientWidth := 275 * TernaryScreenMult + 150;
   ClientHeight := 200 * TernaryScreenMult + 50;
   CreateBitmap(Bitmap,ClientWidth,ClientHeight);
   DrawTernaryAxis(Bitmap);
   Caption := 'Ternary diagram';
   SpeedButton4.Visible := false;
   SpeedButton5.Visible := false;
   Rescale1.Visible := false;
   Analyze1.Visible := false;
end;


procedure TThisBaseGraph.extinlowerleftcorner1Click(Sender: TObject);
begin
   GetString('Text in lower left corner',LLcornerText,false,ReasonableTextChars);
   RedrawDiagram11Click(Nil);
end;

procedure TThisBaseGraph.extinlowerrightcorner1Click(Sender: TObject);
begin
   GetString('Text in lower right corner',LRcornerText,false,ReasonableTextChars);
   RedrawDiagram11Click(Nil);
end;

function tGraphDraw.PtOnGraph(x,y : float32) : boolean;
begin
    Result := (x <= MaxHorizAxis) and (x >= MinHorizAxis) and (y <= MaxVertAxis) and (y >= MinVertAxis);
end;


function tGraphDraw.PtOnGraph(x,y : integer) : boolean;
begin
   Result := (x <= XWindowSize - RightMargin) and (x >= LeftMargin) and (y <= YWindowSize-BottomMargin) and (y >= TopMargin);
end;


procedure TThisBaseGraph.ShowNormalDistribution;

   procedure DrawLine(y : float32);
   var
      yp : integer;
   begin
      if (y < GraphDraw.MaxVertAxis) and (y > GraphDraw.MinVertAxis) then begin
         yp := GraphDraw.GraphY(y);
         Image1.Canvas.MoveTo(GraphDraw.GraphX(GraphDraw.MinHorizAxis),yp);
         Image1.Canvas.LineTo(GraphDraw.GraphX(GraphDraw.MaxHorizAxis),yp);
      end;
   end;

begin
   Image1.Canvas.Pen.Color := clBlack;
   Image1.Canvas.Pen.Width := 1;
   Image1.Canvas.Pen.Style := psSolid;
   DrawLine(16);
   DrawLine(50);
   DrawLine(84);
   DrawLine(2.28);
   DrawLine(97.73);
   DrawLine(0.14);
   DrawLine(99.87);
end;


procedure TThisBaseGraph.WindowACCORD(Canvas : TCanvas; ContInterval,NumContourLines,NumDataPoints : integer; Pnt : tPointerPnt; XMin,YMin,ZMin,DataX : float32; ColorFunction : ColorFunctionType; SaveTIN : PathStr);
{from Fortran program in D.F. Watson, Accord: Automatic contouring of raw data, Computers & Geosciences, vol.8, no.1, p.97-101  }
{ read data points, and form all 3-tuples such that no other point lies within that 3-tuples circumcircle }
{ Pnt holds data points }
{ Tetr carries circumcircle center and radius squared for each 3-tuple}

{  ITetr holds data point indices in input order of each 3-tuple }
{  IStack is a last-in-first-out stack of incidces of vacant 3-tuples }
{  KTetr is a temporary list of edges of deleted 3-tuples }
{  ID is pointer to IStack and JT is pointer to Tetr and ITetr }

{ Algorithm does not like to have a line of data points sticking out beyond the rest of the data points, and will crash with error 205 or 207.  The calling program must avoid this case by adding some extra control points.}
label
   NoLineDrawn,NoLineDrawn2;
type
   DoubleMaxType = array[1..succ(DoubleContourMax)] of float32;
   DoubleMaxInteger = array[1..succ(DoubleContourMax)] of integer;
   ITetrType = array[1..3] of ^DoubleMaxInteger;

   NTetrType  = array[1..3] of ^DoubleMaxType;
   XPntType  = array[1..3,1..3] of float32;
   ITempType = array[1..3,1..2] of integer;
   IStackType = array[0..succ(DoubleContourMax)] of integer;
   ContType  = array[1..MaxContours] of float32;
const
   PointTol = 0.000001;
   ConXPnt  : XPntType  = ((-1,-1,2), (5,-1,2), (-1,5,18));
   ConITemp : ITempType = ((1,2), (1,3), (2,3));
var
   XPnt  : XPntType;
   ITemp : ITempType;
   xs,ys,zs : array[1..3] of float32;
   Det   : array[1..2,1..3] of float32;
   KTetr : array[1..5000,1..2] of integer;
   I2,KT,KMT,k1,L1,L2,
   ximHigh,xImLow,yimHigh,yImLow,
   KM,ISP,ID  : integer;
   Top,Bot,ABit,CZ,DD,DSQ,X1,Y1,X2,Y2 : float32;
   Tetr   : NTetrType;
   ITetr  : ITetrType;
   IStack : ^IStackType;
   Cont   : ^ContType;
   TriTable : tMyData;


   procedure MakeTriangles;
   label
      Label28;
   var
      i,j,k,l,Nuc,Jt,JZ : integer;
   begin
      for i := 1 to 3 do begin
         New(Tetr[i]);
         New(ITetr[i]);
      end;
      New(IStack);
      XPnt := ConXPnt;
      ITemp := ConITemp;
      ISP   := 1;
      ID    := 2;
      for i := 1 to 3 do begin
         ITetr[i]^[1] := i;
         Tetr[i]^[1] := XPnt[i,3];
         for j := 1 to 2 do Pnt[j]^[i] := XPnt[i,j];
      end {for i};

      for i := 2 to succ(DoubleContourMax) do IStack^[i] := i;

      {normalize data}
      for i := 4 to NumDataPoints + 3 do begin
         {$IfDef RecordDetailedTIN}   WriteLineToDebugFile(IntToStr(i) + RealToString(Pnt[1]^[i],12,3) + RealToString(Pnt[2]^[i],12,3)); {$EndIf}
         Pnt[1]^[i] := (Pnt[1]^[i] - XMin) / DataX;
         Pnt[2]^[i] := (Pnt[2]^[i] - YMin) / DataX;
         {$IfDef RecordDetailedTIN} WriteLineToDebugFile(IntToStr(i) + RealToString(Pnt[1]^[i],12,3) + RealToString(Pnt[2]^[i],12,3));   {$EndIf}
      end {for i};

      {$IfDef RecordTIN} WriteLineToDebugFile('start Delauney triangles'); {$EndIf}
      StartProgress('Delaunay triangles');

      for Nuc := 4 to (NumDataPoints + 3) do begin
         if (Nuc mod 10 = 0) then UpdateProgressBar( (Nuc-3) / (NumDataPoints + 3));
         KM := 0;
         {loop through estabished 3-tuples}
         for JT := 1 to Isp do begin
            {check if new data point is within JT circumcircle}
            DSQ := Tetr[3]^[JT] - Sqr(Pnt[1]^[Nuc] - Tetr[1]^[JT]);
            if (DSQ >= 0) then begin
               DSQ := DSQ - Sqr(Pnt[2]^[Nuc] - Tetr[2]^[JT]);
               if (DSQ >= 0) then begin
                  {delete this 3-tuple but save its edge}
                  dec(ID);
                  IStack^[ID] := JT;
                  {add edges to KTetr but delete if already listed}
                  for i := 1 to 3 do begin
                     L1 := ITemp[i,1];
                     L2 := ITemp[i,2];
                     if KM > 0 then begin
                        KMT := KM;
                        for j := 1 to KMT do begin
                           if (ITetr[L1]^[JT] = KTetr[j,1]) and
                              (ITetr[L2]^[JT] = KTetr[j,2]) then begin
                                 dec(KM);
                                 if (j > KM) then goto Label28;
                                 for k := j to KM do begin
                                    K1 := succ(k);
                                    for l := 1 to 2 do KTetr[k,l] := KTetr[K1,l];
                                 end {for k};
                                 goto label28;
                           end {if};
                        end {for j};
                     end {if KM > 0};
                     inc(KM);
                     KTetr[KM,1] := ITetr[L1]^[JT];
                     KTetr[KM,2] := ITetr[L2]^[JT];
                     label28:;
                  end {for i};
               end {if};
            end {if};
         end {for JT};

         {form new 3-tuples}
         for i := 1 to KM do begin
            KT := IStack^[ID];
            inc(ID);
            {calculate circumcircle center and radius squared of points KTert[i,*] and place in Tetr^[KT,*]  }
            for JZ := 1 to 2 do begin
               I2 := KTetr[i,JZ];
               Det[JZ,1] := Pnt[1]^[I2] - Pnt[1]^[Nuc];
               Det[JZ,2] := Pnt[2]^[I2] - Pnt[2]^[Nuc];
               Det[JZ,3] := Det[JZ,1] * (Pnt[1]^[I2] + Pnt[1]^[Nuc]) / 2 + Det[JZ,2] * (Pnt[2]^[I2] + Pnt[2]^[Nuc]) / 2;
            end {for JZ};
            DD := Det[1,1] * Det[2,2] - Det[1,2] * Det[2,1];
            //if abs(dd) < 0.001 then DD := 0.001;

            Tetr[1]^[KT] := (Det[1,3] * Det[2,2] - Det[2,3] * Det[1,2]) / DD;
            Tetr[2]^[KT] := (Det[1,1] * Det[2,3] - Det[2,1] * Det[1,3]) / DD;
            Tetr[3]^[KT] := Sqr(Pnt[1]^[Nuc] - Tetr[1]^[KT]) + Sqr(Pnt[2]^[Nuc] - Tetr[2]^[KT]);

            Tetr[3]^[KT] := Sqr(Pnt[1]^[Nuc] - Tetr[1]^[KT]) + Sqr(Pnt[2]^[Nuc] - Tetr[2]^[KT]);
            ITetr[1]^[KT] := KTetr[i,1];
            ITetr[2]^[KT] := KTetr[i,2];
            ITetr[3]^[KT] := NUC;
         end {for i};
         inc(ISP,2);
      end {for Nuc};
      EndProgress;
      for i := 4 to NumDataPoints + 3 do begin  {return data to original scaling}
         Pnt[1]^[i] := DataX * Pnt[1]^[i] + XMin;
         Pnt[2]^[i] := DataX * Pnt[2]^[i] + YMin;
      end {for i};
   end;

   procedure SaveTriangles;
   var
      i,JT,jc : integer;
   begin
      {$IfDef RecordTIN} WriteLineToDebugFile('Created SaveTIN file: '+ SaveTIN);   {$EndIf}
      Make_tables.MakeDelauneyTable(SaveTIN,AddDelauneyZ,AddDelauneyImage);
      ApplicationProcessMessages;

       TriTable := tMyData.Create(SaveTin);
       StartProgress('Create TIN table');
       for JT := 1 to ISP do begin
         UpdateProgressBar(jt / isp);
         if (ITetr[1]^[JT] >= 4) and (Tetr[3]^[JT] <= 1) then begin
             TriTable.Insert;
             ximHigh := -9999;
             xImLow := 999999;
             yimHigh := -9999;
             yImLow := 999999;
             for jc := 1 to 3 do begin
                xs[jc] := Pnt[1]^[ITetr[jc]^[JT]];
                ys[jc] := Pnt[2]^[ITetr[jc]^[JT]];
                TriTable.SetFieldByNameAsFloat('X' + IntToStr(jc),Pnt[1]^[ITetr[jc]^[JT]]);
                TriTable.SetFieldByNameAsFloat('Y' + IntToStr(jc),Pnt[2]^[ITetr[jc]^[JT]]);
                if AddDelauneyImage then begin
                   TriTable.SetFieldByNameAsInteger('XIM' + IntToStr(jc),round(Pnt[4]^[ITetr[jc]^[JT]]));
                   TriTable.SetFieldByNameAsInteger('YIM' + IntToStr(jc),round(Pnt[5]^[ITetr[jc]^[JT]]));
                   Petmath.CompareValueToExtremes(round(Pnt[4]^[ITetr[jc]^[JT]]),xImLow,xImHigh);
                   Petmath.CompareValueToExtremes(round(Pnt[5]^[ITetr[jc]^[JT]]),yImLow,yImHigh);
                end;
                if AddDelauneyZ then TriTable.SetFieldByNameAsFloat('Z' + IntToStr(jc),Pnt[3]^[ITetr[jc]^[JT]] * UserContourInterval);
             end;
             HeapSort(3,xs);
             TriTable.SetFieldByNameAsFloat('X_LOW',xs[1]);
             TriTable.SetFieldByNameAsFloat('X_HI',xs[3]);
             HeapSort(3,ys);
             TriTable.SetFieldByNameAsFloat('Y_LOW',ys[1]);
             TriTable.SetFieldByNameAsFloat('Y_HI',ys[3]);
             if AddDelauneyImage then begin
                TriTable.SetFieldByNameAsInteger('XIM_LOW',xImLow);
                TriTable.SetFieldByNameAsInteger('XIM_HI',xImHigh);
                TriTable.SetFieldByNameAsInteger('YIM_LOW',yImLow);
                TriTable.SetFieldByNameAsInteger('YIM_HI',yImHigh);
             end;
             TriTable.Post;
         end {if};
      end {for JT};
      for i := 1 to 3 do  begin
         Dispose(Tetr[i]);
         Dispose(ITetr[i]);
      end;
      Dispose(IStack);
   end;


   procedure DrawTriangles;
   {$IfDef ExTIN}
   begin
   {$ELSE}
   label
      NoLineDrawn2;
   var
      i,jc,NumTris,nc,rc : integer;
   begin
      New(Cont);
      Cont^[1] := (trunc(ZMin / ContInterval)) * ContInterval * UserContourInterval;
      for i := 2 to NumContourLines do Cont^[i] := Cont^[1] + pred(i) * ContInterval * UserContourInterval;
      {$IfDef RecordTIN}  for i := 1 to NumContourLines do WriteLineToDebugFile(IntToStr(i)+ '=' + RealToString(Cont^[i],-12,-2)); {$EndIf}
      StartProgress('Triangles');
      TriTable.ProgressVars(rc,nc);
      NumTris := 0;
      TriTable.First;
      while Not TriTable.EOF do with GraphDraw do begin
          inc(NumTris);
          if NumTris mod rc = 0 then UpDateProgressBar(NumTris / nc);

          for jc := 1 to 3 do begin
             xs[jc] := TriTable.GetFieldByNameAsFloat('X' + IntToStr(jc));
             ys[jc] := TriTable.GetFieldByNameAsFloat('Y' + IntToStr(jc));
             if AddDelauneyZ then zs[jc] := TriTable.GetFieldByNameAsFloat('Z' + IntToStr(jc))
             else zs[jc] := 1;
          end;
          if SmallEnoughTriangle(xs[1],xs[2],xs[3],ys[1],ys[2],ys[3]) then begin
                if MDDef.ShowDelauneyTriangles then begin
                   Canvas.Pen.Color := ConvertPlatformColorToTColor(MDDef.DelaunayLineColor);
                   Canvas.Pen.Width := MDDef.DelaunayLineThick;
                   Canvas.MoveTo(GraphX(xs[1]),GraphY(ys[1]));
                   Canvas.LineTo(GraphX(xs[2]),GraphY(ys[2]));
                   Canvas.LineTo(GraphX(xs[3]),GraphY(ys[3]));
                   Canvas.LineTo(GraphX(xs[1]),GraphY(ys[1]));
                end {if};

               {find contour intersections}
               if (zs[1] = zs[2]) or (zs[1] = zs[3]) or (zs[2] = zs[3]) then Abit := 1e-10
               else ABit := 0;
               Top := zs[1];
               Bot := zs[1];
               if zs[2] > Top then Top := zs[2]
               else if zs[2] < Bot then Bot := zs[2];
               if zs[3] > Top then Top := zs[3]
               else if zs[3] < Bot then Bot := zs[3];

               for JC := 1 to NumContourLines do begin
                  if (Cont^[JC] > Bot) and (Cont^[JC] < Top) then begin
                     CZ := (Cont^[JC] - zs[1]) / (zs[2]- zs[1] + ABit);
                     if (CZ <= 0) or (CZ >= 1) then begin
                        CZ := (Cont^[JC] - zs[1]) / (zs[3] - zs[1] + ABit);
                        if (CZ < 0) or (CZ > 1) then Goto NoLineDrawn2;
                        X1 := (xs[1] + (xs[3] - xs[1]) * CZ);
                        Y1 := (ys[1] + (ys[3] - ys[1]) * CZ);
                        CZ := (Cont^[JC] - zs[2]) / (zs[3] - zs[2] + ABit);
                        if (CZ < 0) or (CZ > 1) then Goto NoLineDrawn2;
                        X2 := (xs[2] + (xs[3] - xs[2]) * CZ);
                        Y2 := (ys[2] + (ys[3] - ys[2]) * CZ);
                     end
                     else begin
                        X1 := (xs[1] + (xs[2] - xs[1]) * CZ);
                        Y1 := (ys[1] + (ys[2] - ys[1]) * CZ);
                        CZ := (Cont^[JC] - zs[1]) / (zs[3] - zs[1] + ABit);
                        if (CZ < 0) or (CZ > 1) then begin
                           CZ := (Cont^[JC]-zs[2])/(zs[3] - zs[2] + ABit);
                           if (CZ < 0) or (CZ > 1) then Goto NoLineDrawn2;
                           X2 := (xs[2] + (xs[3] - xs[2]) * CZ);
                           Y2 := (ys[2] + (ys[3] - ys[2]) * CZ);
                        end
                        else begin
                           X2 := (xs[1] + (xs[3] - xs[1]) * CZ);
                           Y2 := (ys[1] + (ys[3] - ys[1]) * CZ);
                        end;
                     end;
                     Canvas.Pen.Color := ConvertPlatformColorToTColor(MDDef.ContourLineColor);
                     Canvas.Pen.Width := MDDef.ContourLineWidth;
                     Canvas.MoveTo(GraphX(X1),GraphY(Y1));
                     Canvas.LineTo(GraphX(X2),GraphY(Y2));
                  end {if};
                  NoLineDrawn2:;
               end {for JC};
          end;
          TriTable.Next;
      end;
      EndProgress;
   {$EndIf}
   end;


begin
   {$IfDef RecordTIN} WriteLineToDebugFile('tThisBaseGraph.WindowACCORD'); {$EndIf}
      if (NumDataPoints > MaxContourPoints) then begin
         MessageToContinue('Too many points for ACCORD');
         exit;
      end;
      if (FileExists(SaveTIN)) then begin
        TriTable := tMyData.Create(SaveTin);
      end
      else begin
         MakeTriangles;
         SaveTriangles;
      end;
      DrawTriangles;

      {$IfDef RecordTIN}
         WriteLineToDebugFile('TIN records:' + IntToStr(TriTable.RecordCount) + '   ContInterval:' + IntToStr(ContInterval));
         WriteLineToDebugFile('UserContourInterval:' + RealToString(UserContourInterval,-18,-4) + '  NumContourLines:' + IntToStr(NumContourLines));
      {$EndIf}

   if (SaveTIN <> '') then TriTable.Destroy;
   EndProgress;
   Dispose(Cont);
end {proc Accord};


      procedure TThisBaseGraph.ShowSatelliteBands(Bitmap : tMyBitmap);
      var
         SatTable : tMyData;
         Lower,higher : float32;
         x1,y1,x2,y2 : integer;
      begin
         SatTable := tMyData.Create(SatBandNames);
         SatTable.ApplyFilter('SATELLITE=' + QuotedStr(GraphDraw.SatBands));
         while not SatTable.eof do begin
            Lower := 1000 * SatTable.GetFieldByNameAsFloat('UM_LOW');
            Higher := 1000 * SatTable.GetFieldByNameAsFloat('UM_HIGH');
            x1 := GraphDraw.GraphX(Lower);
            y1 := GraphDraw.TopMargin;
            x2 := GraphDraw.GraphX(Higher);
            y2 := succ(GraphDraw.YWindowSize - GraphDraw.BottomMargin);
            if SatTable.GetFieldByNameAsString('TOP_ONLY') = 'Y' then begin
              y2 :=  GraphDraw.TopMargin + GraphDraw.YWindowSize div 3;
              Bitmap.Canvas.Pen.Color := RGB(208,255,235);
              Bitmap.Canvas.Brush.Color := RGB(208,200,235);
            end
            else begin
                Bitmap.Canvas.Pen.Color := RGB(235,255,208);
                Bitmap.Canvas.Brush.Color := RGB(235,200,208);
            end;
            Bitmap.Canvas.Rectangle(x1,y1,x2,y2);
            SatTable.Next;
         end;
         SatTable.Destroy;
         //Bitmap.SaveToFile(MDtempDir + 'coverage.bmp')
      end;



procedure TThisBaseGraph.WindowGraphAxes(Bitmap : tMyBitmap; DrawInsideLines : boolean = true);
const
   MinTickValue = 0.000001;
var
   LastLabelEnd : integer;

      procedure SetNormalAxis(AxisFunctionType : tAxisFunction; var AxisFunct : RealToRealFunction;  var CycleCuts : tCycleCut; var NumCycles : integer;  var MinAxis,MaxAxis : float32);
      var
         i : integer;
      begin
         if AxisFunctionType in [ShortCumNormalAxis,LongCumulativeNormalAxis,LongerCumulativeNormalAxis,CumulativeNormalAxis] then begin
            AxisFunct := NInv;
            if AxisFunctionType = CumulativeNormalAxis then begin
               NumCycles := 8;
               for I  := 1 to NumCycles do CycleCuts[i] := ProbCycleCuts[i];
            end
            else if AxisFunctionType = ShortCumNormalAxis then begin
               NumCycles := 6;
               for I  := 1 to NumCycles do CycleCuts[i] := ShortProbCycleCuts[i];
            end
            else if AxisFunctionType = LongerCumulativeNormalAxis then begin
               NumCycles := 10;
               for I  := 1 to NumCycles do CycleCuts[i] := LongerProbCycleCuts[i];
            end
            else begin
               NumCycles := 10;
               for I  := 1 to NumCycles do CycleCuts[i] := LongProbCycleCuts[i];
            end;
            MinAxis := CycleCuts[1,1];
            MaxAxis := CycleCuts[NumCycles,2];
         end;
      end;


      procedure TimeAxis;
      var
         Range,Time     : float32;
         LongX,YearInc,Day,Month,Year,i,x,y,ms,Modder : integer;

         procedure WriteMonth;
         begin
            with GraphDraw do begin
               Time := JulDay(Month,1,Year);
               X := GraphX(Time);
               if (x < XWindowSize) then begin
                  BitmapTextOut(Bitmap,x+2,YWindowSize-BottomMargin + 4,Copy(MonthName[Month],1,ms));
                  if ((Month = 1) or (Month = Month1)) and (not AnnualCycle) and GraphDraw.ShowYears then
                     BitmapTextOut(Bitmap,x+2,YWindowSize-BottomMargin + 12 + Bitmap.Canvas.TextHeight('1'),IntegerToString(Year,4));
               end;
            end {with};
         end;

         function YearString(Year : integer) : shortstring;
         begin
            Result := IntToStr(Year);
            if (Range > 5000) then Result := Copy(Result,3,2);
         end;


      begin {proc TimeAxis}
         with Bitmap.Canvas do begin
            if GraphDraw.AnnualCycle then begin
               GraphDraw.MinHorizAxis := JulDay(1,1,1996);
               GraphDraw.MaxHorizAxis := JulDay(12,31,1996);
            end
            else begin
               GraphDraw.MinHorizAxis := JulDay(GraphDraw.Month1,GraphDraw.Day1,GraphDraw.Year1);
               GraphDraw.MaxHorizAxis := JulDay(GraphDraw.Month2,GraphDraw.Day2,GraphDraw.Year2);
            end;

            GraphDraw.ScrMaxHorizAxis := GraphDraw.MaxHorizAxis;
            GraphDraw.ScrMinHorizAxis := GraphDraw.MinHorizAxis;
            Bitmap.Canvas.Font.Color := GraphDraw.AxisColor;

            Range := GraphDraw.MaxHorizAxis - GraphDraw.MinHorizAxis;   {range in days}
            case (GraphDraw.Year2-GraphDraw.Year1) of
               0..5 : YearInc := 1;
               6..20 : YearInc := 2;
               21..50 : YearInc := 5;
               else YearInc := 10;
            end;

            if (Range < 1000) then begin
               Month := GraphDraw.Month1;
               Year  := GraphDraw.Year1;
               if GraphDraw.GraphX(JulDay(succ(Month),1,Year)) - GraphDraw.GraphX(JulDay(Month,1,Year)) < 50  then ms := 1 else ms := 3;
               WriteMonth;
               repeat
                  inc(Month);
                  if (Month > 12) then begin
                     Month := 1;
                     inc(Year);
                  end;
                  WriteMonth;
                  if DrawInsideLines then begin
                     if GraphDraw.GraphAxes in [FullGrid,XFullGridOnly] then begin
                        if (x <= GraphDraw.XWindowSize) then for i:= 0 to GraphDraw.YWindowSize-GraphDraw.BottomMargin do
                           if (i mod GraphDraw.FullLineFraction = 0) then Pixels[x,i] := GraphDraw.AxisColor;
                     end
                     else begin
                        DrawLine(Bitmap,x,GraphDraw.YWindowSize-GraphDraw.BottomMargin+TickSize,x,GraphDraw.YWindowSize-GraphDraw.BottomMargin-TickSize);
                        DrawLine(Bitmap,x,0,x,2*TickSize);
                     end;
                  end;
               until (Year > GraphDraw.Year2);
            end
            else if (Range < 25000) then begin
               Year := GraphDraw.Year1;
               if (GraphDraw.Month1 = 1) and (GraphDraw.Day1 = 1) then begin
                  Time := JulDay(1,1,Year);
                  X := GraphDraw.GraphX(Time);
                  if (x < GraphDraw.XWindowSize) then BitmapTextOut(Bitmap,x+2,GraphDraw.YWindowSize-GraphDraw.BottomMargin + 4,YearString(Year));
               end;
               repeat
                  inc(Year,YearInc);
                  Time := JulDay(1,1,Year);
                  X := GraphDraw.GraphX(Time);
                  if (x < GraphDraw.XWindowSize) then begin
                     BitmapTextOut(Bitmap,x+2,GraphDraw.YWindowSize-GraphDraw.BottomMargin + 4,YearString(Year));
                  end;
                  if DrawInsideLines then begin
                     if GraphDraw.GraphAxes in [FullGrid,XFullGridOnly] then begin
                        if (x <= GraphDraw.XWindowSize) then for i:= 0 to GraphDraw.YWindowSize-GraphDraw.BottomMargin do
                           if (i mod GraphDraw.FullLineFraction = 0) then Pixels[x,i] := GraphDraw.AxisColor;
                     end
                     else begin
                        DrawLine(Bitmap,x,GraphDraw.YWindowSize-GraphDraw.BottomMargin+TickSize,x,GraphDraw.YWindowSize-GraphDraw.BottomMargin-TickSize);
                        DrawLine(Bitmap,x,0,x,TickSize);
                     end;
                  end;
               until (Year >= GraphDraw.Year2);
            end
            else begin
               Year := 5 * (GraphDraw.Year1 div 5);
               repeat
                  Time := JulDay(1,1,Year);
                  X := GraphDraw.GraphX(Time);
                  if (x < GraphDraw.XWindowSize) then Bitmap.Canvas.TextOut(x+2,GraphDraw.YWindowSize-GraphDraw.BottomMargin + 4,YearString(Year));
                  if DrawInsideLines then begin
                     if GraphDraw.GraphAxes in [FullGrid,XFullGridOnly] then begin
                        if (x <= GraphDraw.XWindowSize) then for i:= 0 to GraphDraw.YWindowSize-GraphDraw.BottomMargin do
                           if (i mod GraphDraw.FullLineFraction = 0) then Pixels[x,i] := GraphDraw.AxisColor;
                     end
                     else begin
                        DrawLine(Bitmap,x,GraphDraw.YWindowSize-GraphDraw.BottomMargin+TickSize,x,GraphDraw.YWindowSize-GraphDraw.BottomMargin-TickSize);
                        DrawLine(Bitmap,x,0,x,TickSize);
                     end;
                  end;
                  inc(Year,YearInc);
               until (Year >= GraphDraw.Year2);
            end;

            Pen.Color := GraphDraw.AxisColor;
            if (Range < 100) then begin
               for Longx := Round(GraphDraw.MinHorizAxis) to round(GraphDraw.MaxHorizAxis) do begin
                  x := GraphDraw.GraphX(LongX);
                  DrawLine(Bitmap,x,GraphDraw.YWindowSize-GraphDraw.BottomMargin,x,GraphDraw.YWindowSize-GraphDraw.BottomMargin-TickSize);
                  DrawLine(Bitmap,x,0,x,TickSize);
                  CalDat(LongX,Month,Day,Year);

                  Modder := 0;
                  if (Range < 10) then Modder := 1
                  else if (Range < 20) then Modder := 2
                  else if (Range < 50) then Modder := 5
                  else if (Range < 100) then Modder := 10;
                  if (Modder > 0) and (x > GraphDraw.LeftMargin + 40) and (x < GraphDraw.XWindowSize) and (Day < 30) then
                     Bitmap.Canvas.TextOut(x,GraphDraw.YWindowSize-GraphDraw.BottomMargin + 2,IntegerToString(Month) + '/' + IntegerToString(Day));
               end;
            end;
            if (Range < 3) then begin
               for Longx := trunc(GraphDraw.MinHorizAxis) to succ(round(GraphDraw.MaxHorizAxis)) do begin
                  for i := 1 to 23 do begin
                     x := GraphDraw.GraphX(LongX + i /24);
                     if i in [6,12,18] then begin
                        BitmapTextOut(Bitmap,x,GraphDraw.YWindowSize-GraphDraw.BottomMargin + 2,IntToStr(i));
                        y := 2*TickSize;
                     end
                     else y := TickSize;
                     DrawLine(Bitmap,x,GraphDraw.YWindowSize-GraphDraw.BottomMargin,x,GraphDraw.YWindowSize-GraphDraw.BottomMargin-y);
                     DrawLine(Bitmap,x,0,x,y);
                  end;
               end;
            end;
         end {with};
      end {proc TimeAxis};


      procedure HorizPartOfGraph(Min,Max,Inc : float32; First : boolean);   {x axis ticks and lines}
      var
         PerCen : float32;
         x,LabelStart,LabelYStart,LabelWidth    : integer;

            procedure DashLine(Max : float32);
            var
               i : integer;
            begin
                X := GraphDraw.GraphX(Max);
                if x <= GraphDraw.XWindowSize then for i:= GraphDraw.TopMargin to GraphDraw.YWindowSize-GraphDraw.BottomMargin do
                    if (i mod GraphDraw.FullLineFraction = 0) then Bitmap.Canvas.Pixels[x,i] := GraphDraw.AxisColor;
            end;

            procedure GraphTick(Max : float32; var x : integer);
            var
               TStr : ShortString;
               AxisDecimals : integer;
            begin
               x := GraphDraw.GraphX(Max);
               if GraphDraw.LabelXFromLog then begin
                  TStr := '';
                  if (Frac(Max) < 0.001) then begin
                     DashLine(Max);
                     Max := Math.Power(10,Max);
                  end
                  else exit;
               end;
               AxisDecimals := GetAxisDecimals(Max - Min);   //inc);

               TStr := RealToString(Max,-12,AxisDecimals);
               DrawLine(Bitmap,x,GraphDraw.YWindowSize - GraphDraw.BottomMargin + 5,x,GraphDraw.YWindowSize - GraphDraw.BottomMargin);
               LabelWidth := Bitmap.Canvas.TextWidth(Tstr);
               LabelStart := x-LabelWidth div 2;
               LabelYStart := GraphDraw.YWindowSize - GraphDraw.BottomMargin + 6;
               if GraphDraw.NormalCartesianX then begin
                  if (LabelStart > LastLabelEnd) and (LabelStart +LabeLWidth < GraphDraw.XWindowSize) then begin
                     Bitmap.Canvas.TextOut(LabelStart,LabelYStart,TStr);
                     LastLabelEnd := LabelStart + LabelWidth + 10;
                  end;
               end
               else begin
                  if (LabelStart < LastLabelEnd - LabelWidth) then begin
                     Bitmap.Canvas.TextOut(LabelStart,LabelYStart,TStr);
                     LastLabelEnd := LabelStart - 10;
                  end;
               end;
            end;


      begin
         {$IfDef RecordGrafAxis} writeLineToDebugFile('HorizPartOfGraph: ' + RealToString(Min,-18,-6) + ' to ' + RealToString(Max,-18,-6) + ' inc=' + RealToString(Inc,-18,-6)); {$EndIf}
         if (GraphDraw.GraphAxes in [XTimeYFullGrid,XTimeYPartGrid]) or (abs(Max-Min) < MinTickValue) then exit;
         if GraphDraw.LabelXFromLog then begin
         end
         else begin
             if DrawInsideLines then begin
                Min := trunc(Min / inc) * inc;
                PerCen := Min;
                while (PerCen <= Max + MinTickValue) do begin
                   X := GraphDraw.GraphX(PerCen);
                   if (x >= GraphDraw.LeftMargin) and (x <= GraphDraw.LeftMargin + GraphDraw.XWindowSize) then begin
                      if GraphDraw.GraphAxes in [FullGrid,XFullGridOnly] then DashLine(PerCen)
                      else begin
                         DrawLine(Bitmap,x,GraphDraw.YWindowSize-GraphDraw.BottomMargin,x,GraphDraw.YWindowSize-GraphDraw.BottomMargin-TickSize);
                         DrawLine(Bitmap,x,GraphDraw.TopMargin,x,GraphDraw.TopMargin + TickSize);
                      end;
                   end;
                   PerCen := PerCen + Inc;
                end {while};
             end;
         end;
         if GraphDraw.ShowHorizAxis0 then begin
            DrawLine(Bitmap,GraphDraw.GraphX(0),GraphDraw.TopMargin,GraphDraw.GraphX(0),GraphDraw.YWindowSize-GraphDraw.BottomMargin);
         end;

         //label, but only the first and last in the cycle
         if GraphDraw.NormalCartesianX then begin
            GraphTick(Max,x);
            if (First) then GraphTick(Min,x);
         end
         else begin
            GraphTick(Min,x);
            if (First) then GraphTick(Max,x);
         end;
      end;


      procedure VertPartOfGraph(Min,Max,Inc : float32; First : boolean; RightSide : boolean = false);   {y axis ticks and lines}
      var
         PerCen : float32;
         i,y,y2,AxisDecimals : integer;
         TStr : ShortString;
      begin
         if GraphDraw.GraphAxes in [XFullGridOnly,XPartGridOnly] then exit;
         AxisDecimals := GetAxisDecimals(Max-Min);    //inc);

         if DrawInsideLines then begin
            if (GraphDraw.GraphType <> gtTwoVertAxes) then begin
               PerCen := Min;
               while (PerCen <= Max + Inc - MinTickValue) and (PerCen <= GraphDraw.MaxVertAxis) do begin
               //draws horizontal lines across the graph area for the minor divisions
                  y := GraphDraw.GraphY(PerCen);
                  if GraphDraw.GraphAxes in [FullGrid,XTimeYFullGrid] then begin
                     for i := GraphDraw.LeftMargin to GraphDraw.XWindowSize-GraphDraw.RightMargin do
                        if (i mod GraphDraw.FullLineFraction = 0) then Bitmap.Canvas.Pixels[i,y] := GraphDraw.AxisColor;
                  end
                  else begin
                     Bitmap.Canvas.MoveTo(GraphDraw.LeftMargin,y);  Bitmap.Canvas.LineTo(GraphDraw.LeftMargin+TickSize,y);
                     Bitmap.Canvas.MoveTo(GraphDraw.XWindowSize,y);  Bitmap.Canvas.LineTo(GraphDraw.XWindowSize-TickSize,y);
                  end {if};
                  PerCen := PerCen + Inc;
               end {while};

               if  (GraphDraw.GraphAxes in [PartGrid,XTimeYPartGrid]) then begin
               //draws horizontal lines across the graph area for the major divisions
                  y := GraphDraw.GraphY(Min);
                  for i := GraphDraw.LeftMargin to GraphDraw.XWindowSize do if (i mod GraphDraw.FullLineFraction = 0) then Bitmap.Canvas.Pixels[i,y] := GraphDraw.AxisColor;
                  y := GraphDraw.GraphY(Max);
                  for i := GraphDraw.LeftMargin to GraphDraw.XWindowSize do if (i mod GraphDraw.FullLineFraction = 0) then Bitmap.Canvas.Pixels[i,y] := GraphDraw.AxisColor;
               end;
            end;
         end;

            if (not First) then begin
               TStr := RealToString(Min,-12,AxisDecimals);
               if RightSide then begin
                  y := GraphDraw.GraphY2(Min);
                  Bitmap.Canvas.MoveTo(GraphDraw.XWindowSize - GraphDraw.RightMargin,y);
                  Bitmap.Canvas.LineTo(GraphDraw.XWindowSize - GraphDraw.RightMargin + 5,y);
                  y := y - Bitmap.Canvas.TextHeight(TStr) div 2;
                  if (y >= 0) then Bitmap.Canvas.TextOut(GraphDraw.XWindowSize - GraphDraw.RightMargin + 8,y,TStr);
               end
               else begin
                  y := GraphDraw.GraphY(Min);
                  Bitmap.Canvas.MoveTo(GraphDraw.LeftMargin - 5,y);
                  Bitmap.Canvas.LineTo(GraphDraw.LeftMargin,y);
                  y := y - Bitmap.Canvas.TextHeight(TStr) div 2;
                  if (y >= 0) then Bitmap.Canvas.TextOut(GraphDraw.LeftMargin - 5 - Bitmap.Canvas.TextWidth(TStr),y,TStr);
               end;
            end;

         TStr := RealToString(Max,-12,AxisDecimals);
         if RightSide then begin
            y2 := GraphDraw.GraphY2(Max);
            Bitmap.Canvas.MoveTo(GraphDraw.XWindowSize - GraphDraw.RightMargin,y2);
            Bitmap.Canvas.LineTo(GraphDraw.XWindowSize - GraphDraw.RightMargin + 5,y2);
            y2 := y2-Bitmap.Canvas.TextHeight(TStr) div 2;
            if (y2 < y - Bitmap.Canvas.TextHeight(TStr) - 5) and (y2 > 0) then Bitmap.Canvas.TextOut(GraphDraw.XWindowSize - GraphDraw.RightMargin + 8,y2,Tstr);
         end
         else begin
            y2 := GraphDraw.GraphY(Max);
            Bitmap.Canvas.MoveTo(GraphDraw.LeftMargin - 5,y2);
            Bitmap.Canvas.LineTo(GraphDraw.LeftMargin,y2);
            y2 := y2-Bitmap.Canvas.TextHeight(TStr) div 2;
            if (y2 < y - Bitmap.Canvas.TextHeight(TStr) - 5) and (y2 > 0) then Bitmap.Canvas.TextOut(GraphDraw.LeftMargin - 5 - Bitmap.Canvas.TextWidth(TStr),y2,Tstr);
         end;
      end {proc VertPartOfGraph};

      procedure DefaultColors;
      begin
         Bitmap.Canvas.Pen.Color := GraphDraw.AxisColor;
         Bitmap.Canvas.Font.Color := GraphDraw. AxisColor;
         Bitmap.Canvas.Pen.Width := MDDef.FrameLineWidth;
         Bitmap.Canvas.Brush.Style := bsClear;
      end;

var
   x1,y1,i : integer;
begin {proc CreateGraphAxes}
   with GraphDraw do begin
      if NormalCartesianX then LastLabelEnd := 0
      else LastLabelEnd := Bitmap.Width;

      SetNormalAxis(HorizAxisFunctionType,HorizAxisFunct,HorizCycleCuts,NumHorizCycles,MinHorizAxis,MaxHorizAxis);
      ScrMaxHorizAxis := HorizAxisFunct(MaxHorizAxis);
      ScrMinHorizAxis := HorizAxisFunct(MinHorizAxis);
      ScrHorizRange := (ScrMaxHorizAxis - ScrMinHorizAxis);
      XPixelSize :=  ScrHorizRange /(XWindowSize - LeftMargin - RightMargin);
      if GraphDraw.NormalCartesianX then LastLabelEnd := 0
      else LastLabelEnd := GraphDraw.XWindowSize;

      Bitmap.Canvas.Pen.Color := RGB(235,255,208);
      Bitmap.Canvas.Brush.Color := RGB(235,255,208);
      Bitmap.Canvas.Brush.Style := bsSolid;
      Bitmap.Canvas.Pen.Width := 1;
      if HighlightBox then begin
         Bitmap.Canvas.Rectangle(GraphDraw.LeftMargin, GraphY(1), GraphDraw.XWindowSize,GraphY(0));
      end;

      DefaultColors;
      Bitmap.Canvas.Brush.Style := bsClear;

      Bitmap.Canvas.Rectangle(LeftMargin,TopMargin,XWindowSize-RightMargin,succ(YWindowSize - BottomMargin));

      for i := 1 to NumHorizCycles do HorizPartOfGraph(HorizCycleCuts[i,1],HorizCycleCuts[i,2],HorizCycleCuts[i,3],i=1);

      if ((XWindowSize - LeftMargin - RightMargin) > Bitmap.Canvas.TextWidth(HorizLabel)) then x1 := LeftMargin + (XWindowSize - LeftMargin - RightMargin - Bitmap.Canvas.TextWidth(HorizLabel)) div 2
      else x1 := (XWindowSize - Bitmap.Canvas.TextWidth(HorizLabel)) div 2;
      Bitmap.Canvas.TextOut(x1,GraphDraw.YWindowSize - GraphDraw.BottomMargin + 6 + Bitmap.Canvas.TextHeight('8'),GraphDraw.HorizLabel);

      if (GraphType = gtTwoVertAxes) then begin
         ScrMaxVertAxis2 := VertAxisFunct(MaxVertAxis2);
         ScrMinVertAxis2 := VertAxisFunct(MinVertAxis2);
         ScrVertRange2 := (ScrMaxVertAxis2 - ScrMinVertAxis2);

         Bitmap.Canvas.Font.Color := ConvertPlatformColorToTColor(GraphDraw.Symbol[2].Color);
         GraphDraw.ForceAxisFit(VertAxisFunctionType,VertCycleCuts,NumVertCycles,MinVertAxis2,MaxVertAxis2,YWindowSize-BottomMargin,25);
         for i := 1 to NumVertCycles do VertPartOfGraph(VertCycleCuts[i,1],VertCycleCuts[i,2],VertCycleCuts[i,3],i=1,true);
         if YWindowSize - BottomMargin > Bitmap.Canvas.TextWidth(VertLabel2) then y1 := YWindowSize - BottomMargin - ((YWindowSize - BottomMargin - Bitmap.Canvas.TextWidth(VertLabel2)) div 2)
         else y1 := YWindowSize - ((YWindowSize - Bitmap.Canvas.TextWidth(VertLabel2)) div 2);
         TextOutVertical(Bitmap.Canvas,XWindowSize - 2 - Bitmap.Canvas.TextHeight(VertLabel2),y1,VertLabel2);

         Bitmap.Canvas.Font.Color := ConvertPlatformColorToTColor(GraphDraw.Symbol[1].Color);
         GraphDraw.ForceAxisFit(VertAxisFunctionType,VertCycleCuts,NumVertCycles,MinVertAxis,MaxVertAxis,YWindowSize-BottomMargin,25);
      end;

      SetNormalAxis(VertAxisFunctionType,VertAxisFunct,VertCycleCuts,NumVertCycles,MinVertAxis,MaxVertAxis);
      ScrMaxVertAxis := VertAxisFunct(MaxVertAxis);
      ScrMinVertAxis := VertAxisFunct(MinVertAxis);
      ScrVertRange := (ScrMaxVertAxis - ScrMinVertAxis);
      YPixelSize :=  ScrVertRange / (YWindowSize - TopMargin - BottomMargin);
      for i := 1 to GraphDraw.NumVertCycles do VertPartOfGraph(GraphDraw.VertCycleCuts[i,1],GraphDraw.VertCycleCuts[i,2],GraphDraw.VertCycleCuts[i,3],i=1);
      if YWindowSize - BottomMargin > Bitmap.Canvas.TextWidth(VertLabel) then y1 := YWindowSize - BottomMargin - ((YWindowSize - BottomMargin - Bitmap.Canvas.TextWidth(VertLabel)) div 2)
      else y1 := YWindowSize - ((YWindowSize - Bitmap.Canvas.TextWidth(VertLabel)) div 2);
      TextOutVertical(Bitmap.Canvas,3,y1,VertLabel);

      {$IfDef RecordGraf} WriteLineToDebugFile('tThisBaseGraph.WindowGraphAxes in,  xsize=' + IntToStr(xsize)); {$EndIf}

      if GraphAxes in [XTimeYFullGrid,XTimeYPartGrid] then TimeAxis;

      if GraphDraw.GraphLeftLabels <> Nil then begin
         for I := 1 to GraphDraw.GraphLeftLabels.Count do begin
            Bitmap.Canvas.TextOut(5,GraphY(i)-Bitmap.Canvas.TextHeight(GraphDraw.GraphLeftLabels.Strings[pred(I)]) div 2,GraphDraw.GraphLeftLabels.Strings[pred(I)]);
         end;
      end;


      if Draw1to1Line then begin
         Bitmap.Canvas.Pen.Color := clSilver;
         Bitmap.Canvas.Pen.Width := 1 + MDDef.FrameLineWidth;
         if (MinHorizAxis > MinVertAxis) then Bitmap.Canvas.MoveTo(GraphX(MinHorizAxis),GraphY(MinHorizAxis))
         else Bitmap.Canvas.MoveTo(GraphX(MinVertAxis),GraphY(MinVertAxis));
         if MaxHorizAxis > MaxVertAxis then Bitmap.Canvas.LineTo(GraphX(MaxVertAxis),GraphY(MaxVertAxis))
         else Bitmap.Canvas.LineTo(GraphX(MaxHorizAxis),GraphY(MaxHorizAxis));
      end;

      if DrawRegion then begin
         Bitmap.Canvas.Brush.Color := clSilver;
         Bitmap.Canvas.Brush.Style := bsSolid;
         Bitmap.Canvas.Pen.Color := clSilver;
         Bitmap.Canvas.Pen.Width := 3;
         Bitmap.Canvas.Rectangle(GraphX(RegionXLo),GraphY(RegionYLo),GraphX(RegionXHi),GraphY(RegionYHi));
      end;
      Bitmap.Canvas.Pen.Width := 1;
   end;
   {$IfDef RecordGraf} WriteLineToDebugFile('tThisBaseGraph.WindowGraphAxes out'); {$EndIf}
end {proc CreateGraphAxes};


{$F+}
function tGraphDraw.GraphY(y : float32) : integer;
var
   ty : integer;
begin
   y := VertAxisFunct(y);
   tY := round( (y - ScrMinVertAxis) /(ScrVertRange) * (YWindowSize - BottomMargin - TopMargin));
   if NormalCartesianY then ty := YWindowSize - TopMargin - BottomMargin - ty;
   GraphY := TopMargin + ty;
end;



function tGraphDraw.GraphY2(y : float32) : integer;
var
   ty : integer;
begin
   y := VertAxisFunct(y);
   tY := round( (y - ScrMinVertAxis2) /(ScrVertRange2) * (YWindowSize - BottomMargin - TopMargin));
   if NormalCartesianY then ty := YWindowSize - TopMargin - BottomMargin - ty;
   GraphY2 := TopMargin + ty;
end;


{$F+}
function tGraphDraw.GraphX(x : float32) : integer;
var
   Month,Day,Year : integer;
begin
   x := HorizAxisFunct(x);
   if AnnualCycle then begin
      CalDat(Trunc(x),Month,Day,Year);
      x := JulDay(Month,Day,1996);
   end;
   if NormalCartesianX then GraphX := LeftMargin + round((x - ScrMinHorizAxis) / (ScrHorizRange) * (XWindowSize - LeftMargin- RightMargin))
   else GraphX := LeftMargin + round((ScrMaxHorizAxis - x) / (ScrHorizRange) * (XWindowSize - LeftMargin-RightMargin))
end;

procedure tGraphDraw.SetMargins(Bitmap : tMyBitmap);
var
   ad,w1,w2 : integer;
begin
   BottomMargin := MarginFreeboard + 2 * Bitmap.Canvas.TextHeight(HorizLabel);

   ad := GetAxisDecimals((MaxVertAxis-MinVertAxis) / NumVertCycles);    //inc);
   w1 := Bitmap.Canvas.TextWidth(RealToString(MaxVertAxis,-12,ad));
   w2 := Bitmap.Canvas.TextWidth(RealToString(MinVertAxis,-12,ad));

   if w2 > w1 then w1 := w2;
   LeftMargin := MarginFreeboard + Bitmap.Canvas.TextHeight(VertLabel) + w1;

   ad := GetAxisDecimals(MaxVertAxis2-MinVertAxis2);    //inc);
   if (GraphType = gtTwoVertAxes) then begin
      w1 := Bitmap.Canvas.TextWidth(RealToString(MaxVertAxis2,-12,ad));
      w2 := Bitmap.Canvas.TextWidth(RealToString(MinVertAxis2,-12,ad));
      if w2 > w1 then w1 := w2;
      RightMargin := MarginFreeboard + Bitmap.Canvas.TextHeight(VertLabel2) + w1;
   end;
end;

procedure tGraphDraw.SetShowAllLines(setting: boolean);
var
   i : integer;
begin
   for i := 1 to 15 do ShowLine[i] := setting;
end;

procedure tGraphDraw.SetShowAllPoints(setting: boolean);
var
   i : integer;
begin
   for i := 1 to 15 do ShowPoints[i] := setting;
end;

procedure TThisBaseGraph.DrawGraph(Bitmap : tMyBitmap; DrawInside : boolean = true);
begin
   {$IfDef RecordGraf} WriteLineToDebugFile('TThisBaseGraph.DrawGraph in ' + FormClientSize(Self)); {$EndIf}
   ForceCycleSize := GraphDraw.ForceHorizCycleSize;
   ForceTickIncr  := GraphDraw.ForceHorizTickIncr;
   if not GraphDraw.UserSetHorizCycles then GraphDraw.ForceAxisFit(GraphDraw.HorizAxisFunctionType,GraphDraw.HorizCycleCuts,GraphDraw.NumHorizCycles,GraphDraw.MinHorizAxis,GraphDraw.MaxHorizAxis,GraphDraw.XWindowSize-GraphDraw.LeftMargin,50);
   ForceCycleSize := GraphDraw.ForceVertCycleSize;
   ForceTickIncr  := GraphDraw.ForceVertTickIncr;
   if not GraphDraw.UserSetVertCycles then GraphDraw.ForceAxisFit(GraphDraw.VertAxisFunctionType,GraphDraw.VertCycleCuts,GraphDraw.NumVertCycles,GraphDraw.MinVertAxis,GraphDraw.MaxVertAxis,GraphDraw.YWindowSize-GraphDraw.BottomMargin,25);
   if (Bitmap <> Nil) then WindowGraphAxes(Bitmap);
   {$IfDef RecordGraf} writeLineToDebugFile('TThisBaseGraph.DrawGraph out ' + FormClientSize(Self)); {$EndIf}
end;


procedure TThisBaseGraph.SetUpGraphForm;
var
   BitMap : tMyBitmap;
   w,h : integer;
begin
   {$IfDef RecordGraf} writeLineToDebugFile('TThisBaseGraph.SetUpGraphForm ' + intToStr(ClientWidth) + 'x' + intToStr(ClientHeight)); {$EndIf}
   ScrollBox1.Enabled := true;
   if ScrollGraph then begin
      ScrollBox1.AutoScroll := true;
      w := Image1.Width;
      h := Image1.Height;
   end
   else begin
      ScrollBox1.AutoScroll := false;
      w := ClientWidth;
      h := ClientHeight - Panel1.Height - ToolBar1.Height;
   end;
   CreateBitmap(Bitmap,w,h);
   Bitmap.Monochrome := GraphDraw.MonochromeBitMap;
   {$IfDef RecordGraf} WriteLineToDebugFile('Bitmap created, ' + IntToStr(w) + 'x' + IntToStr(h) + ' ' +  GraphDraw.AxisRange); {$EndIf}
   GraphDraw.XWindowSize := pred(Width);
   GraphDraw.YWindowSize := pred(Height);
   Bitmap.Canvas.Font := FontDialog1.Font;
   GetMyFontFromWindowsFont(MDDef.DefaultGraphFont,FontDialog1.Font);
   GraphDraw.SetMargins(Bitmap);
   {$IfDef RecordGraf} WriteLineToDebugFile('Move to drawgraph'); {$EndIf}
   DrawGraph(BitMap);
   if GraphDraw.GraphDrawn and (not GraphDraw.SkipDrawing) then begin
      Image1.Picture.Graphic := Bitmap;
   end;
   Bitmap.Free;
   GraphDraw.GraphDrawn := true;
   {$IfDef RecordGraf} WriteLineToDebugFile('TThisBaseGraph.SetUpGraphForm done'); {$EndIf}
end;


procedure TThisBaseGraph.AutoScaleAndRedrawDiagram(DoVert : boolean = true; DoHoriz : boolean = true; PadX : boolean = true; PadY : boolean = true);
var
   NumRead,
   i,j : integer;
   x,y : float32;
   tf    : file;
   fName : PathStr;
   MyTable : tMyData;
   Coords : ^CoordArray;
   Coords3 : ^Coord3Array;
begin
   {$If Defined(RecordScaling)} WriteLineToDebugFile('TThisBaseGraph.AutoScaleAndRedrawDiagram in'); {$EndIf}
   with GraphDraw do begin
      if DoHoriz or DoVert then begin
         if DoHoriz then begin
            MinHorizAxis := 99e99;
            MaxHorizAxis := -99e99;
         end;
         if DoVert then begin
            MinVertAxis := 99e99;
            MaxVertAxis := -99e99;
            MinVertAxis2 := 99e99;
            MaxVertAxis2 := -99e99;
         end;
        MinZ := 99e39;
        MaxZ := -99e39;
        if (GraphType = gtTwoVertAxes) then begin
            assignFile(tf,XYZFilesPlotted[0]);
            reset(tf,3*SizeOf(float32));
            new(Coords3);
            while not EOF(tf) do begin
               BlockRead(tf,Coords3^,ASize,Numread);
               for i := 1 to NumRead do begin
                  if DoHoriz then Petmath.CompareValueToExtremes(Coords3^[(3*i)-2],MinHorizAxis,MaxHorizAxis);
                  if DoVert then begin
                     Petmath.CompareValueToExtremes(Coords3^[pred(3*i)],MinVertAxis,MaxVertAxis);
                     Petmath.CompareValueToExtremes(Coords3^[3*i],MinVertAxis2,MaxVertAxis2);
                  end;
               end;
            end;
            Dispose(Coords3);
            CloseFile(tf);
        end
        else begin
            for j := 0 to pred(GraphDraw.DBFPointFilesPlotted.Count) do if FileExists(DBFPointFilesPlotted[j]) then begin
               fName := DBFPointFilesPlotted[j];
               MyTable := tMyData.Create(fname);
               while not MyTable.Eof do begin
                  x := MyTable.GetFieldByNameAsFloat('X_COORD');
                  y := MyTable.GetFieldByNameAsFloat('Y_COORD');
                  if DoHoriz then begin
                     Petmath.CompareValueToExtremes(x,MinHorizAxis,MaxHorizAxis);
                  end;
                  if DoVert then begin
                     Petmath.CompareValueToExtremes(y,MinVertAxis,MaxVertAxis);
                  end;
                  MyTable.Next;
               end;
               MyTable.Destroy;
            end;
            for j := 0 to pred(GraphDraw.DataFilesPlotted.Count) do if FileExists(DataFilesPlotted[j]) then begin
               assignFile(tf,DataFilesPlotted[j]);
               reset(tf,2*SizeOf(float32));
               new(Coords);
               while not EOF(tf) do begin
                  BlockRead(tf,Coords^,ASize,Numread);
                  for i := 1 to NumRead do begin
                     if DoHoriz then begin
                        Petmath.CompareValueToExtremes(Coords^[pred(2*i)],MinHorizAxis,MaxHorizAxis);
                     end;
                     if DoVert then begin
                        Petmath.CompareValueToExtremes(Coords^[2*i],MinVertAxis,MaxVertAxis);
                     end;
                  end;
               end;
               Dispose(Coords);
               CloseFile(tf);
               {$If Defined(RecordScaling)} WriteLineToDebugFile(DataFilesPlotted[j] + '  ' + GraphDraw.AxisRange); {$EndIf}
            end;

            for j := 0 to pred(XYColorFilesPlotted.Count) do  begin
               assignFile(tf,XYColorFilesPlotted[j]);
               reset(tf,3*SizeOf(float32));
               new(Coords3);
               while not EOF(tf) do begin
                  BlockRead(tf,Coords3^,ASize,Numread);
                  for i := 1 to NumRead do begin
                     if DoHoriz then Petmath.CompareValueToExtremes(Coords3^[(3*i)-2],MinHorizAxis,MaxHorizAxis);
                     if DoVert then Petmath.CompareValueToExtremes(Coords3^[pred(3*i)],MinVertAxis,MaxVertAxis);
                     Petmath.CompareValueToExtremes(Coords3^[(3*i)],MinZ,MaxZ);
                  end;
               end;
               Dispose(Coords3);
               CloseFile(tf);
            end;
            {$If Defined(RecordGraf) or Defined(RecordScaling)} WriteLineToDebugFile('TThisBaseGraph.AutoScaleAndRedrawDiagram  ' + GraphDraw.AxisRange); {$EndIf}
         end;
      end;

      if PadX or PadY then  begin
         {$If Defined(RecordScaling)} WriteLineToDebugFile('TThisBaseGraph.AutoScaleAndRedrawDiagram padding'); {$EndIf}
         for j := 0 to pred(XYZFilesPlotted.Count) do begin
            assignFile(tf,XYZFilesPlotted[j]);
            reset(tf,3*SizeOf(float32));
            new(Coords3);
            while not EOF(tf) do  begin
               BlockRead(tf,Coords3^,ASize,Numread);
               for i := 1 to NumRead do begin
                  Petmath.CompareValueToExtremes(Coords3^[(3*i)-2],MinHorizAxis,MaxHorizAxis);
                  Petmath.CompareValueToExtremes(Coords3^[pred(3*i)],MinVertAxis,MaxVertAxis);
                  Petmath.CompareValueToExtremes(Coords3^[(3*i)],MinZ,MaxZ);
               end;
            end;
            Dispose(Coords3);
            CloseFile(tf);
         end;
         MinZShow := MinZ;
         if PadX then PadAxis(MinHorizAxis,MaxHorizAxis);
         if PadY then PadAxis(MinVertAxis,MaxVertAxis);
      end;
      {$If Defined(RecordScaling)} WriteLineToDebugFile('TThisBaseGraph.AutoScaleAndRedrawDiagram call SetUpGraphForm'); {$EndIf}
      SetUpGraphForm;
      {$If Defined(RecordScaling)} WriteLineToDebugFile('TThisBaseGraph.AutoScaleAndRedrawDiagram call RedrawDiagram11Click'); {$EndIf}
      RedrawDiagram11Click(Nil);
   end;
   {$If Defined(RecordScaling)} WriteLineToDebugFile('TThisBaseGraph.AutoScaleAndRedrawDiagram in'); {$EndIf}
end;


procedure TThisBaseGraph.FormResize(Sender: TObject);
var
   xs,ys : float32;
begin
   if (not MouseIsDown) and (GraphDraw.ForceNewSize or ((not ScrollGraph) and (not SizingWindow))) then begin
      {$IfDef RecordFormResize} WriteLinetoDebugFile('TThisBaseGraph.FormResize start ' + FormClientWidth(Self)); {$EndIf}
      if GraphDraw.ForceNewSize then begin
         ClientWidth := DefaultClientWidth;
         ClientHeight := DefaultClientHeight;
         GraphDraw.ForceNewSize := false;
         {$IfDef RecordFormResize} writeLineToDebugFile('TThisBaseGraph.FormResize forced new size ' + FormClientWidth(Self)); {$EndIf}
      end;
      if GraphDraw.CorrectScaling then begin
         SizingWindow := true;
         xs := DefaultClientWidth - GraphDraw.LeftMargin;
         ys := DefaultClientHeight - GraphDraw.BottomMargin - Panel1.Height;
         {$IfDef RecordFormResize} writeLineToDebugFile('GraphDraw.CorrectScaling,  Pixels on graph area: ' + intToStr(round(xs)) + 'x' + intToStr(round(ys))); {$EndIf}
         xs := ( GraphDraw.MaxHorizAxis -  GraphDraw.MinHorizAxis) / xs;
         ys := ( GraphDraw.MaxVertAxis -  GraphDraw.MinVertAxis) / ys;
         {$IfDef RecordFormResize} writeLineToDebugFile('Pixel dimensions: ' + RealToString(xs,-16,-4) + 'x' + RealToString(ys,-16,-4)); {$EndIf}
         if (xs < ys) then ClientWidth := round( (GraphDraw.MaxHorizAxis -  GraphDraw.MinHorizAxis) / ys) +  GraphDraw.LeftMargin
         else ClientHeight := round( (GraphDraw.MaxVertAxis -  GraphDraw.MinVertAxis) / xs * GraphDraw.VertExag) +  GraphDraw.BottomMargin;
      end;
      if GraphDraw.GraphDrawn then RedrawDiagram11Click(Nil);
      SizingWindow := false;
      {$IfDef RecordFormResize} WriteLinetoDebugFile('TThisBaseGraph.FormResize out ' + intToStr(ClientWidth) + 'x' + intToStr(ClientHeight)); {$EndIf}
   end;
end;


procedure TThisBaseGraph.Freedrag1Click(Sender: TObject);
begin
   ChangeGraphDoing(gdFreeDrag);
end;

procedure TThisBaseGraph.Pastefromclipboard1Click(Sender: TObject);
begin
   LegendBitmap := tMyBitmap.Create;
   LegendBitmap.Assign(ClipBoard);
   if (LegendBitmap.Width = Image1.Width) and (LegendBitmap.Height = Image1.Height)  then begin
      Image1.Picture.Graphic := LegendBitmap;
      FreeAndNil(LegendBitmap);
   end
   else begin
      CopyImageToBitmap(Image1,SavedGraphImage);
      GraphDoing := gdDragEdit;
   end;
end;

procedure TThisBaseGraph.PlotAFile(Bitmap : tMyBitmap; inf : PathStr; Count : integer);
var
   NumRead,Year,Month,Day,NumYears,
   i,x,y : integer;
   tf    : file;
   First : boolean;
   TotNum,NumDone : LongInt;
   Coords : ^CoordArray32;
   Color : tColor;
begin
   {$IfDef RecordPlotFiles} WriteLineToDebugFile('TThisBaseGraph.PlotAFile in, ' + inf); {$EndIf}
   TotNum := GetFileSize(inf) div (2*SizeOf(float32));
   if TotNum = 0 then exit;
   assignFile(tf,inf);
   reset(tf,2*SizeOf(float32));
   new(Coords);
   First := true;
   if ShowProgress then StartProgressAbortOption('Series ' + IntToStr(Count));
   NumDone := 0;
   Numyears := 0;
   while not EOF(tf) do begin
      {$IfDef RecordPlotFiles} writeLineToDebugFile('Try to read=' + IntToStr(ASize));   {$EndIf}
      BlockRead(tf,Coords^,ASize,Numread);
      {$IfDef RecordPlotFiles} WriteLineToDebugFile('Did read=' + IntToStr(NumRead));   {$EndIf}
      inc(NumDone,NumRead);
      if ShowProgress then UpdateProgressBar(NumDone/TotNum);
      for i := 1 to NumRead do  begin
         if GraphDraw.VertAxisFunctionType in [ShortCumNormalAxis,LongCumulativeNormalAxis,LongerCumulativeNormalAxis,CumulativeNormalAxis] then begin
            if Coords^[2*i] < GraphDraw.MinVertAxis then Coords^[2*i] := GraphDraw.MinVertAxis;
            if Coords^[2*i] > GraphDraw.MaxVertAxis then Coords^[2*i] := GraphDraw.MaxVertAxis;
         end;
         x := GraphDraw.GraphX(Coords^[pred(2*i)]);
         y := GraphDraw.GraphY(Coords^[2*i]);
         if GraphDraw.AnnualCycle then begin
            if First or (x < LastX) then with GraphDraw do begin
               CalDat(Trunc(Coords^[pred(2*i)]),Month,Day,Year);
               Color := RainbowColorFunct(Year,Year1,Year2);
               Bitmap.Canvas.Pen.Color := color;
               Bitmap.Canvas.Font.Color := color;
               Bitmap.Canvas.TextOut(50 * NumYears,GraphDraw.YWindowSize -20,IntegerToString(Year,4));
               inc(NumYears);
               First := false;
               Bitmap.Canvas.MoveTo(x,y);
            end
            else Bitmap.Canvas.LineTo(x,y);
            LastX := x;
         end;
         if (Coords^[2*i] > pred(MaxInt)) or (x < GraphDraw.LeftMargin) then First := true
         else begin
            if First then begin
                First := false;
                Bitmap.Canvas.MoveTo(x,y);
            end
            else Bitmap.Canvas.LineTo(x,y);
         end;
      end;
      if Coords^[pred(2*NumRead)] > GraphDraw.MaxHorizAxis then break;
      ApplicationProcessMessages;
      if WantOut then Break;
   end;
   Bitmap.Canvas.Pen.Color := clBlack;
   Bitmap.Canvas.Pen.Width := 1;
   EndProgress;
   Dispose(Coords);
   closeFile(tf);
   {$IfDef RecordPlotFiles} writeLineToDebugFile('TThisBaseGraph.PlotAFile out'); {$EndIf}
end;


procedure TThisBaseGraph.PlotPointFile(Bitmap : tMyBitmap; inf : PathStr;  Symbol : tFullSymbolDeclaration);
var
   TotNum,NumDone,
   NumRead, i : integer;
   xf,yf : float32;
   tf    : file;
   Coords : ^tPointDataBuffer;
begin
   {$IfDef TimeGraphing} WriteLinetoDebugFile('TThisBaseGraph.PlotPointFile in ' + inf); {$EndIf}

   TotNum := GetFileSize(inf) div (2*SizeOf(float32));
   assignFile(tf,inf);
   reset(tf,2*SizeOf(float32));
   new(Coords);
   if ShowProgress then StartProgressAbortOption('Points');
   NumDone := 0;
   while not EOF(tf) do begin
      BlockRead(tf,Coords^,MaxPointsInBuffer,Numread);
      inc(NumDone,NumRead);
      if ShowProgress then UpdateProgressBar(NumDone/TotNum);
      for i := 1 to NumRead do begin
         xf := Coords^[i][1];
         yf := Coords^[i][2];
         if GraphDraw.PtOnGraph(xf,yf) then ScreenSymbol(Bitmap.Canvas, GraphDraw.GraphX(xf), GraphDraw.GraphY(yf),Symbol);
      end;
   end;
   Bitmap.Canvas.Pen.Width := 1;
   EndProgress;
   Dispose(Coords);
   closeFile(tf);
   {$IfDef TimeGraphing} WriteLinetoDebugFile('TThisBaseGraph.PlotPointFile out'); {$EndIf}
end;


procedure TThisBaseGraph.PlotXYColorFile(Bitmap: tMyBitmap; inf: PathStr);
var
   NumRead,i,xp,yp : integer;
   xf,yf,zf : float32;
   tf    : file;
   NumDone,TotNum : LongInt;
   Coords : ^Coord3Array;
   Plot : boolean;
begin
   {$IfDef RecordPlotFiles} writeLineToDebugFile('TThisBaseGraph.PlotXYColorFile in, ' + inf); {$EndIf}
   try
       TotNum := GetFileSize(inf) div (3*SizeOf(float32));
       assignFile(tf,inf);
       reset(tf,3*SizeOf(float32));
       new(Coords);
       NumDone := 0;
       while not EOF(tf) do begin
          BlockRead(tf,Coords^,ASize,Numread);
          inc(NumDone,NumRead);
          if ShowProgress then UpdateProgressBar(NumDone/TotNum);
          for i := 1 to NumRead do begin
             xf := Coords^[(3*i)-2];
             yf := Coords^[pred(3*i)];
             if GraphDraw.PtOnGraph(xf,yf) then  begin
                zf := Coords^[(3*i)];
                Plot := true;
                if GraphDraw.RainBowColors and (zf >= MinZShow) then begin
                   GraphDraw.Symbol[1].Color := PlatformRainbowColorFunct(zf,Minz,MaxZ);
                end
                else GraphDraw.Symbol[1].Color := ConvertTColorToPlatformColor(round(zf));
                if Plot then begin
                   xp := GraphDraw.GraphX(xf);
                   yp := GraphDraw.GraphY(yf);
                   if GraphDraw.Symbol[1].Size = 0 then begin
                      Bitmap.Canvas.Pen.Color := ConvertPlatformColorToTColor(GraphDraw.Symbol[1].Color);
                      Bitmap.Canvas.Pen.Width := GraphDraw.Symbol[1].Size;
                      if i=1 then Bitmap.Canvas.MoveTo(xp,yp)
                      else Bitmap.Canvas.LineTo(xp,yp);
                   end
                   else ScreenSymbol(Bitmap.Canvas, xp,yp,GraphDraw.Symbol[1]);
                end;
             end;
          end;
       end;
       closeFile(tf);
       Dispose(Coords);
   finally
      Canvas.Pen.Width := 1;
      EndProgress;
   end;
   {$IfDef RecordPlotFiles} writeLineToDebugFile('TThisBaseGraph.PlotXYColorFile out'); {$EndIf}
end;

procedure TThisBaseGraph.InitializeTadpole(Title : shortstring; MinX,MaxX,MaxRange : float32);
begin
    Caption := Title;
    GraphDraw.MinHorizAxis := MinX;
    GraphDraw.MaxHorizAxis := MaxX;
    GraphDraw.MaxVertAxis := 2* MaxRange;
    GraphDraw.MinVertAxis := -2 * MaxRange;
    SetUpGraphForm;
    RedrawDiagram11Click(Nil);
end;


procedure TThisBaseGraph.PlotXYZFile(Bitmap : tMyBitmap; inf : PathStr; ASCII : boolean = false);
var
   NumRead,Run,NumRuns,
   i,yi,y2 : integer;
   xf,yf,zf : float32;
   tf    : file;
   afile : TextFile;
   NumDone,TotNum : LongInt;
   Coords : ^Coord3Array;
   Color  : TColor;
   Plot : boolean;
   sBitmap : tMyBitmap;
begin
   {$IfDef RecordPlotFiles} writeLineToDebugFile('TThisBaseGraph.PlotXYZFile in, ' +  inf); {$EndIf}
   try
      if (GraphDraw.GraphType = gtTernary) or (GraphDraw.GraphType = gtTadpole) then begin
         if (GraphDraw.GraphType = gtTadpole) then begin
            Bitmap.Canvas.Pen.Color := ConvertPlatformColorToTColor(GraphDraw.FileColors256[1]);
            Bitmap.Canvas.Pen.Width := GraphDraw.LineSize256[1];
         end;

         TotNum := GetFileSize(inf) div (3*SizeOf(float32));
         assignFile(tf,inf);
         reset(tf,3*SizeOf(float32));
         new(Coords);
         NumDone := 0;
         while not EOF(tf) do begin
            BlockRead(tf,Coords^,ASize,Numread);
            inc(NumDone,NumRead);
            UpdateProgressBar(NumDone/TotNum);
            for i := 1 to NumRead do  begin
               xf := Coords^[(3*i)-2];
               yf := Coords^[pred(3*i)];
               zf := Coords^[(3*i)];
               if (GraphDraw.GraphType = gtTernary) then begin
                  PlotOnTernary(Bitmap,xf,yf,zf);
               end
               else  begin
                  yi := GraphDraw.GraphY(0);
                  y2 := yi - GraphDraw.GraphY(zf);
                  Bitmap.Canvas.MoveTo(GraphDraw.GraphX(xf),GraphDraw.GraphY(0));
                  Bitmap.Canvas.LineTo(GraphDraw.GraphX(xf)+round(SinDeg(yf)*y2),GraphDraw.GraphY(0)-round(cosDeg(yf)*y2));
               end;
            end;
         end;
         closeFile(tf);
         Dispose(Coords);
      end
      else begin
         if GraphDraw.ZColorLegend then begin
            sBitmap := DefaultHorizontalLegendOnBitmap(MinZ,MaxZ,'','',LegRainbows);
            DisplayBitmap(sBitmap,'Color legend');
            sBitmap.Free;
         end;

         if ShowProgress then StartProgressAbortOption('Points');
         if ASCII then begin
            assignFile(afile,inf);
            reset(afile);
            while not EOF(afile) do begin
               readln(afile,xf,yf,zf);
               while Eoln(afile) and (Not EOF(afile)) do readln(afile);
               if  GraphDraw.PtOnGraph(xf,yf) then begin
                  if GraphDraw.HardColors then GraphDraw.Symbol[1].Color := ConvertTColorToPlatformColor(round(zf))
                  else GraphDraw.Symbol[1].Color := PlatformRainbowColorFunct(zf,MinZ,MaxZ);
                  ScreenSymbol(Canvas, GraphDraw.GraphX(xf), GraphDraw.GraphY(yf),GraphDraw.Symbol[1]);
               end;
            end;
            CloseFile(afile);
         end
         else  begin
            if GraphDraw.RedGray then NumRuns := 2 else NumRuns := 1;
            for Run := 1 to NumRuns do begin
               if (Run = 1) then Color := clGray else Color := clRed;
               TotNum := GetFileSize(inf) div (3*SizeOf(float32));
               assignFile(tf,inf);
               reset(tf,3*SizeOf(float32));
               new(Coords);
               NumDone := 0;
               while not EOF(tf) do  begin
                  BlockRead(tf,Coords^,ASize,Numread);
                  inc(NumDone,NumRead);
                  if ShowProgress then UpdateProgressBar(NumDone/TotNum);
                  for i := 1 to NumRead do begin
                     xf := Coords^[(3*i)-2];
                     yf := Coords^[pred(3*i)];
                     if GraphDraw.PtOnGraph(xf,yf) then begin
                        zf := Coords^[(3*i)];
                        if GraphDraw.RainBowColors then begin
                           if GraphDraw.RedGray then begin
                              Plot := (run = 1) and (round(zf) = 0) or (run = 2) and (round(zf) = 1);
                           end
                           else begin
                              if ZF >= MinZShow then GraphDraw.Symbol[1].Color := PlatformRainbowColorFunct(zf,Minz,MaxZ);
                           end;
                        end
                        else GraphDraw.Symbol[1].Color := ConvertTColorToPlatformColor(round(zf));
                        if (not GraphDraw.RainBowColors) or (ZF >= MinZShow) then begin
                           ScreenSymbol(Bitmap.Canvas, GraphDraw.GraphX(xf), GraphDraw.GraphY(yf),GraphDraw.Symbol[1]);
                        end;
                     end;
                  end;
               end;
               closeFile(tf);
               Dispose(Coords);
            end;
         end;
      end;
   finally
      Canvas.Pen.Width := 1;
      EndProgress;
   end;
   {$IfDef RecordPlotFiles} writeLineToDebugFile('TThisBaseGraph.PlotXYZFile out'); {$EndIf}
end;


procedure TThisBaseGraph.PlotLineDBFFile(Bitmap : tMyBitmap; inf : PathStr);
var
   i,x,y : integer;
   MyTable : tMyData;
begin
   {$IfDef RecordPlotFiles} writeLineToDebugFile('TThisBaseGraph.PlotDBFFile in ' + inf); {$EndIf}
   try
      MyTable := tMyData.Create(Inf);
      i := 0;
      while not MyTable.Eof do begin
         x := GraphDraw.Graphx(MyTable.GetFieldByNameAsFloat(DBFXFieldName));
         y := GraphDraw.GraphY(MyTable.GetFieldByNameAsFloat(DBFYFieldName));
         if (i = 0) then Bitmap.Canvas.MoveTo(x,y)
         else Bitmap.Canvas.LineTo(x,y);
         MyTable.Next;
         inc(i);
      end;
      MyTable.Destroy;
   finally
      Canvas.Pen.Width := 1;
      EndProgress;
   end;
   {$IfDef RecordPlotFiles} writeLineToDebugFile('TThisBaseGraph.PlotDBFFile in'); {$EndIf}
end;


procedure TThisBaseGraph.PlotPointDBFFile(Bitmap : tMyBitmap; inf : PathStr);
var
   x,y : integer;
   MyTable : tMyData;
   Symbol : tDrawingSymbol;
   SymbolSize : byte;
   SymbolColor : tPlatformColor;
   TStr : shortstring;
begin
   {$IfDef RecordPlotFiles} writeLineToDebugFile('TThisBaseGraph.PlotDBFFile in ' + inf); {$EndIf}
   try
      MyTable := tMyData.Create(Inf);
      while not MyTable.Eof do  begin
         x := GraphDraw.Graphx(MyTable.GetFieldByNameAsFloat('X_COORD'));
         y := GraphDraw.GraphY(MyTable.GetFieldByNameAsFloat('Y_COORD'));
         MyTable.DefinePointSymbol(Symbol,SymbolSize,SymbolColor);
         ScreenSymbol(Bitmap.Canvas,X,Y,Symbol,SymbolSize,SymbolColor);
         if MyTable.FieldExists('NAME') then begin
            TStr := MyTable.GetFieldByNameAsString('NAME');
            Bitmap.Canvas.TextOut(x+5,y,TStr);
         end;
         MyTable.Next;
      end;
      MyTable.Destroy;
   finally
      EndProgress;
   end;
   {$IfDef RecordPlotFiles} writeLineToDebugFile('TThisBaseGraph.PlotDBFFile in'); {$EndIf}
end;



function TGraphDraw.InvGraphY(y : integer) : float32;
begin
   Result := (Y - TopMargin) / (YWindowSize - BottomMargin - TopMargin) * (ScrMaxVertAxis - ScrMinVertAxis);
   if NormalCartesianY then Result := ScrMaxVertAxis - Result
   else Result := ScrMinVertAxis + Result;
   if VertAxisFunctionType = Log10Axis then Result := Math.Power(10,Result);
   if VertAxisFunctionType in [ShortCumNormalAxis,CumulativeNormalAxis,LongCumulativeNormalAxis,LongerCumulativeNormalAxis] then begin
       Result := 0.01;
       while (Graphy(Result) > y) and (Result < 100) do Result := Result + 0.01;
   end;
end;


procedure  TThisBaseGraph.Keyboardresize1Click(Sender: TObject);
begin
   if GetNewBMPSize(DefaultClientWidth,DefaultClientHeight,'Graph size') then begin
      GraphDraw.ForceNewSize := true;
      FormResize(sender);
   end;
end;

function TGraphDraw.InvGraphX(x : integer) : float32;
begin
   if NormalCartesianX then Result := ScrMinHorizAxis + (x - LeftMargin) / (XWindowSize - LeftMargin - RightMargin) * (ScrMaxHorizAxis - ScrMinHorizAxis)
   else Result := ScrMaxHorizAxis - (x - LeftMargin) / (XWindowSize - LeftMargin - RightMargin) * (ScrMaxHorizAxis - ScrMinHorizAxis);
   if HorizAxisFunctionType = Log10Axis then Result := Math.Power(10,result);
end;

procedure TThisBaseGraph.IDSpeedButtonClick(Sender: TObject);
begin
   {$IfDef ExGIS}
   {$Else}
      GraphDoing := gdIDDataBase;
      wmDEM.SetPanelText(0,'Pick record to ID');
   {$EndIf}
end;

procedure TThisBaseGraph.FilterDBatCurrentPoint;
var
   x1,y1,x2,y2,z1,z2 : float32;
   k : integer;
begin
   with GISdb[DataBaseOnGraph] do repeat
      EmpSource.Enabled := false;
      if (GraphDraw.GraphType = gtTernary) then begin
         Self.InverseProjectTernaryComposition(LastX+k,lastY+k,x1,y1,z1);
         Self.InverseProjectTernaryComposition(LastX-k,lastY-k,x2,y2,z2);
         x1 := round(x1);
         x2 := round(x2);
         y1 := round(z1);
         y2 := round(z2);
      end
      else begin
         x1 := Self.GraphDraw.InvGraphX(LastX-k);
         x2 := Self.GraphDraw.InvGraphX(LastX+k);
         if GraphDraw.NormalCartesianY then begin
            y1 := Self.GraphDraw.InvGraphY(LastY+k);
            y2 := Self.GraphDraw.InvGraphY(LastY-k);
         end
         else begin
            y1 := Self.GraphDraw.InvGraphY(LastY-k);
            y2 := Self.GraphDraw.InvGraphY(LastY+k);
         end;
      end;
      GISdb[DataBaseOnGraph].dbOpts.GeoFilter := XField + ' >= ' + RealToString(x1,-12,-2) + ' AND ' + XField + ' <= ' + RealToString(x2,-12,-2) + ' AND ' + YField + ' >= ' + RealToString(y1,-12,-2) +
          ' AND ' + YField + ' <= ' + RealToString(y2,-12,-2);
      GISdb[DataBaseOnGraph].AssembleGISFilter;
      inc(k);
   until (MyData.RecordCount >= 1) or (k > 5);
end;

procedure TThisBaseGraph.Image1DblClick(Sender: TObject);
var
   Comp1,Comp2,Comp3,xis,yis,
   dx,dy : float32;
   j,{k,}Month,Day,Year : integer;
   MenuStr,TStr : ShortString;
begin
   MouseIsDown := false;
   {$IfDef ExGIS}
   {$Else}
      if (GraphDoing = gdIDDataBase) then begin
         MouseIsDown := false;
         if (DataBaseOnGraph = 0) or (GISdb[DataBaseOnGraph] = Nil) then begin
            wmdem.StatusBar1.Panels[0].Text := '';
            exit;
         end;
        FilterDBatCurrentPoint;
        if (GISdb[DataBaseOnGraph].MyData.RecordCount > 0) then begin
            GISdb[DataBaseOnGraph].MyData.First;
            for j := 0 to pred(GISdb[DataBaseOnGraph].MyData.RecordCount) do begin
               Image1.Canvas.Pen.Mode := pmNotXor;
               ScreenSymbol(Image1.Canvas,LastX,LastY,Splat,5,claBlack);
               if (GISdb[DataBaseOnGraph].theMapOwner <> Nil) then GISdb[DataBaseOnGraph].dbtablef.Highlightrecordonmap1Click(Nil);
               GISdb[DataBaseOnGraph].DisplayTheRecord(j);
               if (GISdb[DataBaseOnGraph].theMapOwner <> Nil) then GISdb[DataBaseOnGraph].theMapOwner.Image1.Picture.Graphic := GISdb[DataBaseOnGraph].dbTablef.BaseMapBitmap;
               ScreenSymbol(Image1.Canvas,Lastx,Lasty,Splat,5,claBlack);
               Image1.Canvas.Pen.Mode := pmCopy;
               if not GISdb[DataBaseOnGraph].MyData.EOF then Next;
            end;
         end;
         GISdb[DataBaseOnGraph].MyData.ApplyFilter(DataBaseFilter);
         GISdb[DataBaseOnGraph].dbtablef.Button1.Enabled := true;
         GISdb[DataBaseOnGraph].EmpSource.Enabled := true;
         exit;
      end;
   {$EndIf}

   with GraphDraw,Image1.Canvas do begin
      {$IfDef ExSlicer3D}
      {$Else}
         if (GraphDoing = gdGraphDBFilter) then begin
            SlicerForm.MaskPoints(InvGraphX(LastX),InvGraphY(LastY));
            exit;
         end;
         if (GraphDoing = gdDoZShift) then  begin
            SlicerForm.ZShift(InvGraphX(LastX),InvGraphY(LastY));
            exit;
         end;
         if (GraphDoing = gdDigitYAxis) then begin
            SlicerForm.DigitizeYAxis(InvGraphX(LastX),InvGraphY(LastY));
            ScreenSymbol(Image1.Canvas,LastX,LastY,FilledBox,3,claRed);
            exit;
         end;
         if GraphDoing = gdSecondDistance then begin
            ChangeGraphDoing(gdDoingNothing);
            dx := InvGraphX(LastX)-InvGraphX(FirstX);
            dy := InvGraphY(LastY)-InvGraphY(FirstY);
            MessageToContinue('Distance: ' + RealToString(sqrt(sqr(dx) + sqr(dy)),-18,-3)+ MessLineBreak + MessLineBreak +
                'dx=' + RealToString(dx,-12,-2) +  '   dy=' + RealToString(dy,-12,-2) + MessLineBreak + MessLineBreak + 'Heading=' + RealToString(HeadingOfLine(dx,dy),-12,-2) + DegSym,True);
            exit;
         end;
         if GraphDoing in [gdFirstDistance,gdFirstSlope] then begin
            if GraphDoing = gdFirstDistance then ChangeGraphDoing(gdSecondDistance)
            else GraphDoing := gdSecondSlope;
            FirstX := LastX;
            FirstY := LastY;
            exit;
         end;
         if GraphDoing = gdSecondSlope then begin
            ChangeGraphDoing(gdDoingNothing);
            MessageToContinue('Slope between points: ' + RealToString((InvGraphY(LastY)-InvGraphY(FirstY))/(InvGraphX(LastX)-InvGraphX(FirstX)),-18,-3) +
                '  ' + GraphDraw.VertLabel + ' / ' + GraphDraw.HorizLabel + MessLineBreak + MessLineBreak +
                GraphDraw.HorizLabel + ': ' + RealToString(InvGraphX(FirstX),-18,-3) + ' to ' + RealToString(InvGraphX(LastX),-18,-3) + MessLineBreak +
                GraphDraw.VertLabel + ': ' + RealToString(InvGraphY(FirstY),-18,-3) + ' to ' + RealToString(InvGraphY(LastY),-18,-3),True);
            exit;
         end;
         if (GraphDoing = gdGraphDigitize) then begin
            ScreenSymbol(Image1.Canvas,LastX,LastY,MDDef.SlicerDigPtSym);
            SlicerForm.DigitizePoint(InvGraphX(LastX),InvGraphY(LastY));
            exit;
         end;
      {$EndIf}

      if (GraphDoing = gdDragEdit) then begin
         FreeAndNil(savedGraphImage);
         FreeAndNil(LegendBitmap);
         GraphDoing := gdDoingNothing;
         exit;
      end
      else if (RoseData <> Nil) then begin
         exit;
      end
      else begin
         if (GraphDraw.GraphType = gtTernary) then begin
             InverseProjectTernaryComposition(Lastx,Lasty,comp1, comp2, comp3);
             if (Comp1 > 0) and (Comp2 > 0) and (Comp3 > 0) then
                MenuStr := HorizLabel + '=' +  RealToString(Comp1,5,1) + MessLineBreak + VertLabel + '=' + RealToString(Comp3,5,1) + MessLineBreak + ThirdLabel + '=' + RealToString(Comp2,5,1)
             else exit;
         end
         else  begin
            xis := InvGraphX(LastX);
            yis := InvGraphY(LastY);
            if GraphAxes in [ XTimeYFullGrid, XTimeYPartGrid] then begin
               CalDat(Trunc(xis),Month,Day,Year);
               if (ScrMaxHorizAxis - ScrMinHorizAxis) < 15.001 then
                  TStr := RealToString(Frac(xis)*24,6,2) + ' hrs '
               else TStr := '';
               MenuStr := IntegerToString(Day,2) + ' ' + MonthName[Month] +
                     IntegerToString(Year,5) + TStr;
            end
            else MenuStr := 'x=' + RealToString(xis,-12,2);
            MenuStr := MenuStr + MessLineBreak + 'y=' + RealToString(yis,-12,DigitsFromSize(Yis));
         end;
      end;
      Pen.Mode := pmNotXor;
      ScreenSymbol(Image1.Canvas,LastX,LastY,Splat,3,claRed);
      MessageToContinue(MenuStr,True);
      ScreenSymbol(Image1.Canvas,Lastx,Lasty,Splat,3,claRed);
      Pen.Mode := pmCopy;
   end {with};
end;


procedure TThisBaseGraph.Close1Click(Sender: TObject);
begin
   Close;
end;


procedure TThisBaseGraph.Saveimage1Click(Sender: TObject);
begin
   {$IfDef ExImages}
   {$Else}
      SaveImageAsBMP(Image1);
   {$EndIf}
end;


procedure TThisBaseGraph.Saveimage2Click(Sender: TObject);
begin
   Saveimage1Click(Sender);
end;

procedure TThisBaseGraph.ACCORD(Bitmap : tMyBitmap; ContInterval,NumContourLines,NumDataPoints : integer;
   Pnt : tPointerPnt; XMin,YMin,ZMin,DataX : float32; ColorFunction : ColorFunctionType; SaveTIN : PathStr);
begin
   {$IfDef RecordTIN} WriteLineToDebugFile('TThisBaseGraph.ACCORD, saveTin='+SaveTin); {$EndIf}
   WindowACCORD(Bitmap.Canvas,ContInterval,NumContourLines,NumDataPoints,Pnt,XMin,YMin,ZMin,DataX, ColorFunction, SaveTIN);
end;


procedure TThisBaseGraph.ClearDataOnGraph;

   procedure HandleGroup(SL : tStringList);
   var
      i : integer;
   begin
      if (sl <> Nil) then begin
         for i := 1 to sl.Count do
            if (ExtractFilePath(sl.Strings[pred(i)]) = MDTempDir) then DeleteFileIfExists(sl.Strings[pred(i)]);
         sl.Clear;
      end;
   end;

begin
    HandleGroup(GraphDraw.DataFilesPlotted);
    HandleGroup(GraphDraw.XYZFilesPlotted);
    HandleGroup(GraphDraw.XYColorFilesPlotted);
    if (GraphDraw.LegendList <> Nil) then GraphDraw.LegendList.Clear;
    if (GraphDraw.GraphTopLabels <> Nil) then GraphDraw.GraphTopLabels.Clear;
    if (GraphDraw.RawFilesPlotted <> Nil) then GraphDraw.RawFilesPlotted.Clear;
    if (GraphDraw.DBFLineFilesPlotted <> Nil) then GraphDraw.DBFLineFilesPlotted.Clear;
    if (GraphDraw.DBFPointFilesPlotted <> Nil) then GraphDraw.DBFPointFilesPlotted.Clear;
end;


procedure TThisBaseGraph.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   {$IfDef Closing} WriteLinetoDebugFile('TThisBaseGraph.FormClose in, ' + Caption); {$EndIf}
    ClearDataOnGraph;
    if (GraphDraw.LegendList <> Nil) then GraphDraw.LegendList.Free;
    if (GraphDraw.GraphTopLabels <> Nil) then GraphDraw.GraphTopLabels.Free;
    if (GraphDraw.RawFilesPlotted <> Nil) then GraphDraw.RawFilesPlotted.Free;
    if (GraphDraw.DBFLineFilesPlotted <> Nil) then GraphDraw.DBFLineFilesPlotted.Free;
    if (GraphDraw.DBFPointFilesPlotted <> Nil) then GraphDraw.DBFPointFilesPlotted.Free;
    GraphDraw.Destroy;
    Action := caFree;
end;


procedure TThisBaseGraph.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
   {$IfDef Closing} WriteLinetoDebugFile('TThisBaseGraph.FormCloseQuery in, ' + Caption); {$EndIf}
   CanClose := CanCloseGraph;
end;


procedure TThisBaseGraph.Graphparameters1Click(Sender: TObject);
begin
   {$IfDef VCL}
      if GetNewBMPSize(DefaultClientWidth,DefaultClientHeight,'') then begin
         GraphDraw.ForceNewSize := true;
         FormResize(Sender);
      end;
   {$EndIf}
end;

procedure TThisBaseGraph.Graphsettings2Click(Sender: TObject);
begin
   if (GraphDraw.GraphType = gtRose) and (RoseData <> Nil) then begin
      ReadDefault('Rose bin size',BinSize);
      MDDef.RoseBothEnds := AnswerIsYes('180 degree data');
      DrawAspectRose(RoseData^);
   end
   else if GraphDraw.GraphType in [gtTernary,gtTadpole,gtTwoVertAxes,gtNormal,gtBoxPlot] then begin
         ChangeGraphSettingsClick(Nil);
         if SettingsChanged then begin
            RedrawDiagram11Click(Nil);
            if GraphDraw.CorrectScaling then FormResize(nil);
         end;
   end;
end;


procedure TThisBaseGraph.Grayscalegraph1Click(Sender: TObject);
begin
   GrayScaleImage(Image1);
end;

procedure TThisBaseGraph.FormCreate(Sender: TObject);
var
   Bitmap : tMyBitmap;
   i      : integer;
begin
   {$IfDef RecordGraf} WriteLineToDebugFile('TThisBaseGraph.FormCreate in'); {$EndIf}
   GraphDraw := tGraphDraw.Create;

   if MDDef.CreateGraphHidden then begin
      FormStyle := fsNormal;
      Hide;
   end;
   MDDef.CreateGraphHidden := false;

   GraphDraw.GraphDrawn := false;
   GraphDraw.LabelXFromLog := false;
   GraphDraw.ShowHorizAxis0 := false;

     MapOwner := nil;
     RedrawNow := false;
     HighlightBox := false;
     BoxLineWidth := 2;
     GraphDraw.RedGray := false;
     GraphDraw.ShowYears := true;
     SaveGraphName := '';
     RangeGraphName := '';
     GraphFilter := '';
     LLcornerText := '';
     LRcornerText := '';
     MainMenu1.AutoMerge := Not SkipMenuUpdating;

     if CreateGraphHidden then begin
       HideToolbar(true);
       HideMenus;
     end;

     LoadMyFontIntoWindowsFont(MDDef.DefaultGraphFont,FontDialog1.Font);
     DefaultClientWidth := MDDef.DefaultGraphXSize;
     DefaultClientHeight := MDDef.DefaultGraphYSize;

     GraphDraw.MarginFreeboard := 25;

     if CreateSmallGraph then begin
        DefaultClientWidth := DefaultClientWidth div 2;
        DefaultClientHeight := DefaultClientHeight div 2;
        FontDialog1.Font.Size := 8;
        HideToolbar(true);
     end;

     ClientWidth := DefaultClientWidth;
     ClientHeight := DefaultClientHeight;
     GraphDoing := gdDoingNothing;
     Symbol.DrawingSymbol := FilledBox;
     Symbol.Color := claRed;
     Symbol.Size := 2;
     CurrentOverlay := 0;


     {$IfDef RecordGraf} writeLineToDebugFile('TThisBaseGraph.FormCreate 1 ' + intToStr(ClientWidth) + 'x' + intToStr(ClientHeight)); {$EndIf}

     GraphDraw.GraphType := gtNormal;
     Image1.Stretch := true;             //Required for Delphi 6 "feature"
     ScrollBox1.AutoScroll := false;
     ASCIIXYZFile := '';
     BaseCaption := '';
     VertCompare := 1;
     MouseIsDown := false;
     RedrawingNow := false;
     Panel1.Height := 0;
     SizingWindow := false;
     ScrollGraph := false;
     GraphDraw.HardColors := false;
     GraphDraw.RainBowColors := false;
     GraphDraw.LabelPointsAtop := true;
     GraphDraw.ZColorLegend := false;
     GraphDraw.SkipDrawing := false;
     GraphDraw.TernaryGrid := tgRegular;

     RoseData := Nil;
     for i := 1 to 255 do begin
        GraphDraw.FileColors256[i] := ConvertTColorToPlatformColor(WinGraphColors[i mod 15]);
        GraphDraw.LineSize256[i] := 3;
     end;
     for i := 1 to 15 do begin
        GraphDraw.Symbol[i].DrawingSymbol := Box;
        GraphDraw.Symbol[i].Color := ConvertTColorToPlatformColor(WinGraphColors[i]);
        GraphDraw.Symbol[i].Size := 2;
        GraphDraw.ShowPoints[i] := true;
        GraphDraw.ShowLine[i] := false;
     end;
     GraphDraw.Symbol[1].DrawingSymbol := FilledBox;
     GraphDraw.Symbol[2].DrawingSymbol := FilledUpTri;
     GraphDraw.Symbol[3].DrawingSymbol := FilledDiamond;
     RoseColor := clLime;
     DefaultGraphSettings(GraphDraw);
     {$IfDef RecordGraf} writeLineToDebugFile('TThisBaseGraph.FormCreate 2 ' + intToStr(ClientWidth) + 'x' + intToStr(ClientHeight)); {$EndIf}
     GraphDraw.XWindowSize := pred(DefaultClientWidth);
     GraphDraw.YWindowSize := pred(DefaultClientHeight - Panel1.Height - ToolBar1.Height);
     CreateBitmap(Bitmap,succ(GraphDraw.XWindowSize),Succ(GraphDraw.YWindowSize));
     if (not GraphDraw.SkipDrawing) then Image1.Picture.Graphic := Bitmap;
     Bitmap.Free;
     MinZ := 9e39;
     MaxZ := -9e39;
     CanCloseGraph := true;
     GraphDraw.CorrectScaling := false;

     GraphDraw.DataFilesPlotted := tStringList.Create;
     GraphDraw.XYZFilesPlotted := tStringList.Create;
     GraphDraw.DBFLineFilesPlotted := tStringList.Create;
     GraphDraw.DBFPointFilesPlotted := tStringList.Create;
     GraphDraw.XYColorFilesPlotted := tStringList.Create;
     GraphDraw.RawFilesPlotted := Nil;
     GraphDraw.LegendList := Nil;
     GraphDraw.GraphTopLabels := Nil;
     GraphDraw.GraphLeftLabels := Nil;
     GraphDraw.DataFilesPlottedTable := '';

     FontDialog1.Font.Style := [fsBold];
     GraphDraw.ForcePrinterDPI := false;
     GraphDraw.UserSetVertCycles := false;
     GraphDraw.UserSetHorizCycles := false;
     GraphDraw.AnnualCycle := false;
     GraphDraw.Draw1to1Line := false;
     GraphDraw.DrawRegion := false;

     {$IfDef TIDES}
        StaticGraph := true;
        TidalRecord := Nil;
     {$EndIf}

     Petmar.CheckFormPlacement(Self);
     Toolbar1.Visible := true;   //ShowToolBars;
     SetUpGraphForm;
   {$IfDef RecordGraf} WriteLineToDebugFile('TThisBaseGraph.FormCreate 3 ' + intToStr(ClientWidth) + 'x' + intToStr(ClientHeight)); {$EndIf}
end;


procedure TThisBaseGraph.Font1Click(Sender: TObject);
begin
   if FontDialog1.Execute then begin
      RedrawDiagram11Click(Nil);
      {$IfDef MICRODEM}
         GetMyFontFromWindowsFont(MDDef.DefaultGraphFont,FontDialog1.Font);
      {$EndIf}
   end;
end;


procedure tThisBaseGraph.AddPointToDataBuffer(var rfile : file; v : tFloatPoint);
begin
   inc(PointsInDataBuffer);
   PointDataBuffer^[PointsInDataBuffer] := v;
   if PointsInDataBuffer = MaxPointsInBuffer then begin
      BlockWrite(rfile,PointDataBuffer^,PointsInDataBuffer);
      PointsInDataBuffer := 0;
   end;
end;


procedure tThisBaseGraph.AddPointToDataBuffer(var rfile : file; x,y : float32);
var
   v : tFloatPoint;
begin
   v[1] := x;
   v[2] := y;
   AddPointToDataBuffer(rfile,v);
end;


procedure tThisBaseGraph.ClosePointDataFile(var rfile : file);
begin
   if (PointDataBuffer <> Nil) then begin
       if (PointsInDataBuffer > 0) then begin
          BlockWrite(rfile,PointDataBuffer^,PointsInDataBuffer);
          CloseFile(rFile);
       end;
       {$IfDef RecordGraphMemoryAlocations} WriteLineToDebugFile('Free PointDataBuffer in tThisBaseGraph.OpenDataFile ' + IntToStr(SizeOf(PointDataBuffer^))); {$EndIf}
       Dispose(PointDataBuffer);
       PointDataBuffer := Nil;
   end
   else CloseFile(rfile);
end;


procedure tThisBaseGraph.OpenDataFile(var rfile : file; ActuallyOpen : boolean = true; Color : tColor = -1);
var
   TStr : ShortString;
begin
   if ActuallyOpen then begin
       TStr := NextFileNumber(MDTempDir, 'bf-pet', '.tmp');
       {$IfDef RecordGraf} WriteLineToDebugFile('created graph file: ' + TStr); {$EndIf}
       assignFile(rfile,Tstr);
       rewrite(rfile,2*SizeOf(float32));
       GraphDraw.DataFilesPlotted.Add(Tstr);
       if (Color <> -1) and (GraphDraw.DataFilesPlotted.Count <= 255) then begin
          if GraphDraw.DataFilesPlotted.Count <= 15 then GraphDraw.Symbol[GraphDraw.DataFilesPlotted.Count].Color := ConvertTColorToPlatformColor(color);
          GraphDraw.FileColors256[GraphDraw.DataFilesPlotted.Count] := ConvertTColorToPlatformColor(color);
       end;
   end;

   New(PointDataBuffer);
   {$IfDef RecordGraphMemoryAlocations} WriteLineToDebugFile('Allocate PointDataBuffer in tThisBaseGraph.OpenDataFile ' + IntToStr(SizeOf(PointDataBuffer^))); {$EndIf}
   PointsInDataBuffer := 0;
end;


procedure tThisBaseGraph.OpenPointFile(var rfile : file; Symbol : tFullSymbolDeclaration);
begin
   OpenDataFile(rfile);
   GraphDraw.LineSize256[GraphDraw.DataFilesPlotted.Count] := 0;
   GraphDraw.FileColors256[GraphDraw.DataFilesPlotted.Count] := Symbol.color;
   GraphDraw.Symbol[(GraphDraw.DataFilesPlotted.Count)] := Symbol;
end;


procedure tThisBaseGraph.OpenXYZFile(var rfile : file);
var
   TStr : ShortString;
begin
   TStr := NextFileNumber(MDTempDir, 'bf-pet','.tmp');
   assignFile(rfile,Tstr);
   rewrite(rfile,3*SizeOf(float32));
   GraphDraw.XYZFilesPlotted.Add(Tstr);
end;

procedure tThisBaseGraph.OpenXYColorFile(var rfile : file);
var
   TStr : ShortString;
   i : integer;
begin
   TStr := NextFileNumber(MDTempDir, 'bf-pet','.tmp');
   assignFile(rfile,Tstr);
   rewrite(rfile,3*SizeOf(float32));
   GraphDraw.XYColorFilesPlotted.Add(Tstr);
   for i := 1 to 14 do GraphDraw.Symbol[i].size := 4;
end;


procedure TThisBaseGraph.Filter(Terms : integer; Median : boolean = false);
const
   MaxFilter = 2500;
type
   tvals = array[1..2] of float32;
   ValArray = array[1..MaxFilter] of tvals;
var
   infile,rfile : file;
   i     : integer;
   Count  : LongInt;
   Meds : array[1..MaxFilter] of single;
   Values : ^ValArray;
   Sum,v      : tvals;
begin
   GetMem(Values,Terms*Sizeof(tvals));
   if (GraphDraw.RawFilesPlotted = Nil) then begin
      GraphDraw.RawFilesPlotted := tStringList.Create;
      for i := 1 to GraphDraw.DataFilesPlotted.Count do GraphDraw.RawFilesPlotted.Add(GraphDraw.DataFilesPlotted.Strings[pred(i)]);
   end;
   GraphDraw.DataFilesPlotted.Free;
   GraphDraw.DataFilesPlotted := tStringList.Create;
   assignFile(infile,GraphDraw.RawFilesPlotted.Strings[0]);
   reset(infile,2*SizeOf(float32));
   OpenDataFile(rFile);
   Sum[1] := 0;
   Sum[2] := 0;
   for i := 1 to pred(Terms) do  begin
      BlockRead(Infile,v,1);
      Values^[i] := v;
      Sum[1] := Sum[1] + v[1];
      Sum[2] := Sum[2] + v[2];
   end;
   StartCount('Filtering, terms done: ');
   Count := 0;
   while not EOF(infile) do  begin
      BlockRead(Infile,values^[Terms],1);
      inc(Count);
      if (Count mod 100 = 0) then UpDateCount(Count);
      Sum[1] := Sum[1] + values^[Terms][1];
      v[1] := Sum[1] / Terms;
      Sum[2] := Sum[2] + values^[Terms][2];
      if Median then begin
         for i := 1 to Terms do Meds[i] := values^[i][2];
         v[2] := Petmath.Median(Meds,Terms);
      end
      else v[2] := Sum[2] / Terms;

      BlockWrite(rfile,v,1);
      Sum[1] := Sum[1] - values^[1][1];
      Sum[2] := Sum[2] - values^[1][2];
      for i := 1 to pred(Terms) do begin
         values^[i][1] := values^[succ(i)][1];
         values^[i][2] := values^[succ(i)][2];
      end;
   end;
   FreeMem(Values,Terms*Sizeof(tvals));
   EndCount;
   CloseFile(Rfile);
   CloseFile(Infile);
   RedrawDiagram11Click(Nil);
end;


procedure TThisBaseGraph.None1Click(Sender: TObject);
begin
   Filter(1);
end;

procedure TThisBaseGraph.N3term1Click(Sender: TObject);
begin
   Filter(3);
end;

procedure TThisBaseGraph.N5term1Click(Sender: TObject);
begin
   Filter(5);
end;


procedure TThisBaseGraph.n7term1Click(Sender: TObject);
begin
   Filter(7);
end;

procedure TThisBaseGraph.N9term1Click(Sender: TObject);
begin
   Filter(9);
end;


procedure TThisBaseGraph.Custom1Click(Sender: TObject);
begin
   ReadDefault('Averaging filter length',FilterTerms);
   if not Odd(FilterTerms) then dec(FilterTerms);
   if (FilterTerms < 1) then FilterTerms := 1;
end;


procedure TThisBaseGraph.DoFFT(XAxis,SeekPeaks : boolean);
{$IfDef ExFourier}
begin
{$Else}
begin
   if (GraphDraw.DataFilesPlotted.Count = 0) then exit;
   FFTGraph := TFFTGraph.Create(Application);
   FFTGraph.BinTime := 1;
   FFTGraph.BinUnits := '';
   if XAxis then FFTGraph.fftfilename := NextFileNumber(MDTempDir,GraphDraw.VertLabel,'')
   else FFTGraph.fftfilename := NextFileNumber(MDTempDir, GraphDraw.HorizLabel,'');
   WriteDataSeriesASCII(XAxis,false,FFTGraph.fftfilename,FFTGraph.TotalNumberPoints);
   FFTGraph.FastFourierTransform;
   if SeekPeaks then FFTGraph.SeekPeakButtonClick(Nil);
{$EndIf}
end;


procedure TThisBaseGraph.WriteDataSeriesASCII(XAxis,ASCII : boolean; var fftfilename : PathStr; var TotalNumberPoints : integer; DefSeries : integer = -1);
var
   Infile : file;
   aFile  : TextFile;
   tfile  : file of float32;
   v      : array[1..2] of float32;
   Series : integer;
begin
   if (FFTFileName = '') then FFTfileName := NextFileNumber(MDTempDir, 'temp','.fft');
   if ASCII then begin
      assignFile(afile,fftfilename);
      rewrite(afile);
   end
   else begin
      assignFile(tfile,fftfilename);
      rewrite(tfile);
   end;
   if (GraphDraw.DataFilesPlotted.Count > 1) then begin
      if (DefSeries < 0) then begin
         Series := 1;
         Dec(Series);
         ReadDefault('There are ' + IntToStr(GraphDraw.DataFilesPlotted.Count) + ' series plotted; which do you want?',Series);
      end
      else Series := DefSeries;
   end
   else Series := 0;
   assignFile(infile,GraphDraw.DataFilesPlotted.Strings[Series]);
   reset(infile,2*SizeOf(float32));
   TotalNumberPoints := 0;
   while not EOF(infile) do begin
      BlockRead(infile,v,1);
      inc(TotalNumberPoints);
      if ASCII then begin
         if XAxis then writeln(afile,RealToString(v[2],-24,-8)) else writeln(afile,RealToString(v[1],-24,-8));
      end
      else  begin
         if XAxis then write(Tfile,v[2]) else write(Tfile,v[1]);
      end;
   end;
   CloseFile(InFile);
   if ASCII then closeFile(aFile) else closeFile(Tfile);
end;


procedure TThisBaseGraph.AlongYaxis1Click(Sender: TObject);
begin
   DOFFT(false,false);
end;


procedure TThisBaseGraph.Alongyaxis2Click(Sender: TObject);
begin
   Alongxaxis2Click(Sender);
end;


procedure TThisBaseGraph.AlongXaxis1Click(Sender: TObject);
begin
   DOFFT(true,false);
end;


procedure TThisBaseGraph.Scale1Click(Sender: TObject);
type
   tvals = array[1..2] of float32;
var
   infile,rfile : file;
   i     : integer;
   v     : tvals;
   XMult,YMult : float32;
begin
   XMult := 1;
   YMult := 1 / PetMar_Types.FeetToMeters;
   ReadDefault('x axis multiplier',XMult);
   ReadDefault('y axis multiplier',YMult);

   if (GraphDraw.RawFilesPlotted = Nil) then begin
      GraphDraw.RawFilesPlotted := tStringList.Create;
      for i := 1 to GraphDraw.DataFilesPlotted.Count do GraphDraw.RawFilesPlotted.Add(GraphDraw.DataFilesPlotted.Strings[pred(i)]);
   end;
   GraphDraw.DataFilesPlotted.Free;
   GraphDraw.DataFilesPlotted := tStringList.Create;
   assignFile(infile,GraphDraw.RawFilesPlotted.Strings[0]);
   reset(infile,2*SizeOf(float32));
   OpenDataFile(rFile);
   while not EOF(infile) do  begin
      BlockRead(Infile,v,1);
      v[1] := v[1] * XMult;
      v[2] := v[2] * YMult;
      BlockWrite(rfile,v,1);
   end;
   with GraphDraw do begin
      MinHorizAxis := MinHorizAxis * XMult;
      MaxHorizAxis := MaxHorizAxis * XMult;
      MinVertAxis := MinVertAxis * YMult;
      MaxVertAxis := MaxVertAxis * YMult;
   end;
   CloseFile(Rfile);
   CloseFile(Infile);
   RedrawDiagram11Click(Nil);
end;


procedure ForceLogAxisFit(var CycleCuts : tCycleCut; var NumCycles : integer; var Min,Max : float32);
var
   i : integer;
begin
   if (abs(Min) < e-10) then Min := 0.01;
   Min := trunc(log10(Min));
   Max := log10(Max);
   if Frac(Max) > 0.1 then Max := succ(trunc(Max))
   else Max := round(Max);
   NumCycles := round(Max - Min);
   If (NumCycles = 0) then NumCycles := 1;
   if (NumCycles > MaxCycles) then NumCycles := MaxCycles;
   CycleCuts[1,1] := Math.Power(10.0,Min);
   CycleCuts[1,2] := 10 * CycleCuts[1,1];
   CycleCuts[1,3] := CycleCuts[1,1];

   for i := 2 to NumCycles do begin
      CycleCuts[i,1] := 10 * CycleCuts[pred(i),1];
      CycleCuts[i,2] := 10 * CycleCuts[i,1];
      CycleCuts[i,3] := CycleCuts[i,1]
   end;
   Min := CycleCuts[1,1];
   Max := CycleCuts[NumCycles,2];
end {proc ForceLogAxisFit};

procedure ForceLinearAxisFit(var CycleCuts : tCycleCut; var NumCycles : integer; Min,Max : float32; PixelsHigh,TickSpacing : integer);
var
  i  : integer;
  CycleSize,TickIncr,Range : float32;
begin
   Range := Max - Min;
   if (abs(ForceCycleSize) > 0.000001) and (abs(ForceTickIncr) > 0.000001) then begin
      CycleSize := ForceCycleSize;
      while (Range > 10 * CycleSize) do CycleSize := 2 * CycleSize;
      TickIncr := ForceTickIncr;
   end
   else begin
      CycleSize := GetTickInt(PixelsHigh,TickSpacing,Range);
      TickIncr := CycleSize / 5;
   end;

   i := 1;
   CycleCuts[i,1] := Min;
   if (Min < 0) then CycleCuts[i,2] := CycleSize*trunc(Min / CycleSize)
   else CycleCuts[i,2] := CycleSize*trunc(Min / CycleSize)  + CycleSize;
   CycleCuts[i,3] := TickIncr;
   while (CycleCuts[i,2]  < Max) and (i < MaxCycles) do begin
      if (CycleCuts[i,1] < -0.000001) and (CycleCuts[i,2] > 0.00001) then begin
          CycleCuts[i,2] := 0;
          CycleCuts[succ(i),2] := CycleCuts[i,2];
          CycleCuts[succ(i),1] := 0;
          CycleCuts[succ(i),3] := TickIncr;
          inc(i);
      end {if};
      inc(i);
      CycleCuts[i,1] := CycleCuts[pred(i),2];
      CycleCuts[i,2] := CycleCuts[i,1] + CycleSize;
      CycleCuts[i,3] := TickIncr;
   end {while};
   CycleCuts[i,2] := Max;
   NumCycles := i;
end;


procedure tGraphDraw.ForceAxisFit(AxisFunctionType : tAxisFunction; var CycleCuts : tCycleCut; var NumCycles : integer;  var Min,Max : float32; PixelsHigh,TickSpacing : integer);
begin
   {$IfDef RecordGrafAxis} WriteLineToDebugFile('ForceAxisFit: ' + RealToString(Min,18,6) + ' to ' + RealToString(Max,18,6)); {$EndIf}
   if (AxisFunctionType = Log10Axis) then ForceLogAxisFit(CycleCuts,NumCycles,Min,Max)
   else if (AxisFunctionType = LinearAxis) then ForceLinearAxisFit(CycleCuts,NumCycles,Min,Max,PixelsHigh,TickSpacing);
end;


procedure TThisBaseGraph.DrawAspectRose(AspectFreqVals : CircleFreqType; Legend : ShortString = ''; BottomLegend : ShortString = '');
var
   rad,n,i,j,XCent,YCent,Incr,MaxCircle,MaxAspectFreq,Radius,
   x3,x4,y3,y4,Count : integer;
   ang    : float32;
   Bitmap : tMyBitmap;


      procedure IncrementCount(i : integer);
      begin
         if (i > -0.5 * BinSize + ang) and (i < +0.5 * BinSize + ang) then begin
            inc(Count,RoseData^[i]);
            if MDDef.RoseBothEnds then begin
               if (i < 180) then inc(Count,RoseData^[i+180])
               else inc(Count,RoseData^[i-180]);
            end;
         end;
      end;

begin
   SpeedButton4.Visible := false;
   SpeedButton5.Visible := false;
   Rescale1.Visible := false;
   Analyze1.Visible := false;
   ShowHourglassCursor;
   if (RoseData = Nil) then begin
      New(RoseData);
      RoseData^ := AspectFreqVals;
      BinSize := 0;
      Caption := Legend;
   end;
   if (Legend <> '') then RoseLegend := RemoveUnderscores(Legend);
   CreateBitmap(Bitmap,ClientWidth-2,ClientHeight-Toolbar1.Height-2);
   Bitmap.Canvas.Font := FontDialog1.Font;
   Bitmap.Canvas.Font.Size := 14;

   Radius := (ClientHeight - Toolbar1.Height - 45) div 2;
   Xcent := ClientWidth div 2;
   YCent := BitMap.Height div 2;

   MaxAspectFreq := 0;
   n := 0;
   RoseData^[0] := RoseData^[0] + RoseData^[360];
   RoseData^[360] := 0;
   for i := 0 to 359 do  begin
      if RoseData^[i] > MaxAspectFreq then MaxAspectFreq := RoseData^[i];
      inc(n, RoseData^[i]);
   end;

   if (BinSize = 0) then begin
      BinSize := 0;
      repeat
         inc(BinSize);
         while (BinSize in [7,8,11,13,14,16,17,18,19]) do inc(BinSize);
      until (n div (360 div Binsize) > 5) or (BinSize >= 20);
   end;

  MaxAspectFreq := 0;
   ang := 0;
   while ang <= 359 do begin
      Count := 0;
      for i := 0 to 359 do begin
         IncrementCount(i);
      end;
      if (Count > MaxAspectFreq) then MaxAspectFreq := Count;
      ang := ang + 0.5 * BinSize;
   end;

   while (Bitmap.Canvas.TextWidth(RoseLegend) > ClientWidth) do Bitmap.Canvas.Font.Size := Bitmap.Canvas.Font.Size - 1;

   Bitmap.Canvas.TextOut(0,0,RoseLegend);
   if MDDef.DetailRoseLegend then begin
      Bitmap.Canvas.Font.Size := 13;
      Bitmap.Canvas.TextOut(5,Bitmap.Canvas.TextHeight('n=1')+8,'n=' + n.ToString);
      Bitmap.Canvas.Font.Size := 10;
   end;

   with Bitmap.Canvas do  begin
      if MaxAspectFreq < 10 then Incr := 1
      else if MaxAspectFreq < 20 then Incr := 2
      else if MaxAspectFreq < 50 then Incr := 5
      else if MaxAspectFreq < 100 then Incr := 10
      else if MaxAspectFreq < 500 then Incr := 50
      else if MaxAspectFreq < 1000 then Incr := 100
      else if MaxAspectFreq < 5000 then Incr := 500
      else if MaxAspectFreq < 10000 then Incr := 1000
      else if MaxAspectFreq < 25000 then Incr := 2500
      else if MaxAspectFreq < 50000 then Incr := 5000
      else if MaxAspectFreq < 100000 then Incr := 10000
      else if MaxAspectFreq < 250000 then Incr := 25000
      else if MaxAspectFreq < 750000 then Incr := 50000
      else if MaxAspectFreq < 1500000 then Incr := 100000
      else if MaxAspectFreq < 5000000 then Incr := 250000
      else if MaxAspectFreq < 10000000 then Incr := 500000
      else Incr := 1000000;
      MaxCircle := succ(trunc(MaxAspectFreq / Incr)) * Incr;

      if (GraphFilter <> '') then begin
         Bitmap.Canvas.TextOut(1,Bitmap.Height - 20,'Filter: ' + GraphFilter);
      end
      else if (BottomLegend <> '') then begin
         Bitmap.Canvas.TextOut(1,Bitmap.Height - 20,BottomLegend);
      end
      else begin
         if MDDef.DetailRoseLegend then begin
            Bitmap.Canvas.TextOut(1,Bitmap.Height - 20,'Circle spacing=' + IntToStr(Incr) + '  Bin size=' + BinSize.ToString + DegSym);
         end;
      end;

      Pen.Color := clSilver;
      Pen.Width := 2;
      i := MaxCircle;
      while i >= 0 do begin
         j := round(i / MaxCircle * Radius);
         Ellipse(XCent-j,YCent-j,XCent+j,YCent+j);
         Canvas.Font.Color := clSilver;
         if i = MaxCircle then Canvas.TextOut(XCent + 5, YCent + j +5,IntToStr(i));
         dec(i,Incr);
      end;

      ang := 0;
      while ang < 360 do begin
         x3 := round(XCent + cosDeg(ang) * radius);
         y3 := round(YCent - sinDeg(ang) * radius);
         MoveTo(xcent,ycent);
         LineTo(x3,y3);
         ang := ang + 30;
      end;

      Pen.Color := RoseColor;
      Brush.Color := RoseColor;
      Brush.Style := bsSolid;

      ang := 0;
      while (ang <= 359) do  begin
         Count := 0;
         for i := 0 to 359 do begin
            IncrementCount(i);
         end;
         Rad := round(Radius * Count / MaxCircle);
         x3 := round(XCent + cos(CompassAngleToRadians(+0.5 * BinSize + ang)) * Rad);
         y3 := round(YCent - sin(CompassAngleToRadians(+0.5 * BinSize + ang)) * Rad);
         x4 := round(XCent + cos(CompassAngleToRadians(-0.5 * BinSize + ang)) * Rad);
         y4 := round(YCent - sin(CompassAngleToRadians(-0.5 * BinSize + ang)) * Rad);
         if (x3 = x4) and (y4 = y3) then begin
            MoveTo(xcent,ycent);
            LineTo(x3,y3);
         end
         else Pie(xcent-rad,ycent-rad,xcent+rad,ycent+rad,x3,y3,x4,y4);
         ang := ang + BinSize;
      end;
   end;
   if (not GraphDraw.SkipDrawing) then Image1.Picture.Graphic := Bitmap;
   Bitmap.Free;
   ShowDefaultCursor;
end;

function GraphAxesName(GraphAxes : AxesType) : shortstring;
begin
   case GraphAxes of
      FullGrid : GraphAxesName :=  'full';
      PartGrid : GraphAxesName :=  'partial';
      NoGrid   : GraphAxesName :=  'none';
   end;
end;

function Linear(x : float32) : float32;
begin
   Linear := x;
end;

procedure ComplicatedLocatePointOnGraph(Canvas : TCanvas; GraphDraw : tGraphDraw;  x,y,sx,sy : integer);
var
   xis,yis : float32;
   Month,Day,Year : integer;
   MenuStr,TStr : ShortString;
begin
   with GraphDraw do begin
      Canvas.Pen.Mode := pmNotXor;
      ScreenSymbol(Canvas,sx,sy,Splat,3,claRed);
      xis := (x - LeftMargin) / (XWindowSize - LeftMargin) * (ScrMaxHorizAxis - ScrMinHorizAxis);
      yis := (y - TopMargin) / (YWindowSize - BottomMargin - TopMargin) * (ScrMaxVertAxis - ScrMinVertAxis);
      if NormalCartesianY then yis := ScrMaxVertAxis - yis
      else yis := ScrMinVertAxis + yis;
      if NormalCartesianX then xis := ScrMinHorizAxis + xis
      else xis := ScrMaxVertAxis - xis;
      if HorizAxisFunctionType = Log10Axis then xis := Math.Power(10,xis);
      if VertAxisFunctionType = Log10Axis then yis := Math.Power(10,yis);
      if GraphAxes in [ XTimeYFullGrid, XTimeYPartGrid] then begin
         CalDat(Trunc(xis),Month,Day,Year);
         if (ScrMaxHorizAxis - ScrMinHorizAxis) < 15.001 then
            TStr := RealToString(Frac(xis)*24,6,2) + ' hrs '
         else TStr := '';
         MenuStr := IntegerToString(Day,2) + ' ' + MonthName[Month] +
               IntegerToString(Year,5) + TStr;
      end
      else MenuStr := 'x=' + RealToString(xis,-12,2);

      MessageToContinue(MenuStr + MessLineBreak + 'y=' + RealToString(yis,-12,DigitsFromSize(Yis)));
      ScreenSymbol(Canvas,sx,sy,Splat,3,claRed);
      Canvas.Pen.Mode := pmCopy;
   end {with};
end;

procedure LocatePointOnGraph(Canvas : TCanvas; GraphDraw : tGraphDraw; x,y : integer);
begin
   ComplicatedLocatePointOnGraph(Canvas,GraphDraw,x,y,x,y);
end;

procedure PadAxis(var Min,Max : float32);
var
   Range,Padding : float32;
begin
   Range := (Max - Min);
   if abs(Range) < 0.00001 then Max := Max + 0.0025;
   if Range <= 0.05 then Padding := 0.0025
   else if Range <= 0.1 then Padding := 0.025
   else if Range <= 0.5 then Padding := 0.05
   else if Range <= 1 then Padding := 0.1
   else if Range <= 10 then Padding := 1
   else if Range <= 25 then Padding := 2
   else if Range <= 100 then Padding := 10
   else if Range <= 1000 then Padding := 100
   else if Range <= 5000 then Padding := 500
   else if Range <= 10000 then Padding := 1000
   else Padding := 10000;
   if (Max > 0) then Max := Padding + Padding * trunc(Max / Padding)
   else Max := Padding * trunc(Max / Padding);
   if abs(Min) < 0.0001 then Min := 0
   else if Min > 0 then Min := Padding * trunc(Min / Padding)
   else Min := Padding * trunc(Min / Padding) - Padding;
end;

procedure tGraphDraw.DefaultAxisFit(AxisFunctionType : tAxisFunction; NumPoints : integer; var x : array of float32;
   var CycleCuts : tCycleCut; var NumCycles : integer; var Min,Max : float32; PixelsHigh,TickSpacing : integer);
var
  i  : integer;
begin
   Min := x[0];
   Max := x[0];
   for i := 1 to pred(NumPoints) do begin
      if x[i] < Min then Min := x[i];
      if x[i] > Max then Max := x[i];
   end {for i};
   PadAxis(Min,Max);
   if AxisFunctionType = LinearAxis then ForceLinearAxisFit(CycleCuts,NumCycles,Min,Max,PixelsHigh,TickSpacing);
   if AxisFunctionType = Log10Axis then ForceLogAxisFit(CycleCuts,NumCycles,Min,Max);
end {proc DefaultLinearAxisFit};

procedure tGraphDraw.ManualAxisFit(var CycleCuts : tCycleCut;
   var NumCycles : integer; var Min,Max : float32; Message : shortstring; PixelsHigh,TickSpacing : integer);
begin
   repeat
      ReadDefault('Min on ' + Message + ' axis',Min);
      ReadDefault('Max on ' + Message + ' axis',Max);
   until Min < Max;
   ForceLinearAxisFit(CycleCuts,NumCycles,Min,Max,PixelsHigh,TickSpacing);
end;

function DateString(Month,Day,Year : integer) : string10;
begin
   DateString := IntToStr(Month) + '/' + IntToStr(Day) + '/' + IntegerToString(Year,4);
end;


procedure DefaultGraphSettings(var GraphDraw : tGraphDraw);
begin
   with GraphDraw do begin
      AxisColor := clBlack;
      TopMargin    := 0;
      LeftMargin   := 65;
      BottomMargin := 45;
      RightMargin := 0;
      FullLineFraction := 3;
      NormalCartesianY := true;
      NormalCartesianX := true;
      GraphAxes := PartGrid;
      HorizAxisFunct := Linear;
      VertAxisFunct  := Linear;
      HorizAxisFunctionType := LinearAxis;
      VertAxisFunctionType := LinearAxis;
      HorizLabel     := '';
      VertLabel      := '';
      MinHorizAxis := 0;
      MaxHorizAxis := 100;
      MinVertAxis := 0;
      MaxVertAxis := 100;
      MinVertAxis2 := 0;
      MaxVertAxis2 := 100;
      ForceVertCycleSize := 0;
      ForceVertTickIncr := 0;
      ForceHorizCycleSize := 0;
      ForceHorizTickIncr := 0;
   end;
end;

procedure TThisBaseGraph.Viewdata1Click(Sender: TObject);
{$IfDef VCL}
var
   infile : file;
   Month,Day,Year,
   i      : integer;
   TStr : shortString;
   Results : tStringList;
   v       : array[1..3] of float32;

      procedure DoFile(fName : PathStr);
      begin
         Results := tStringList.Create;
         assignFile(infile,fName);
         reset(infile,3*SizeOf(float32));
         while not EOF(infile) do begin
            BlockRead(infile,v,1);
            Results.Add( RealToString(v[1],18,8) + ' ' + RealToString(v[2],18,8) + ' ' + RealToString(v[3],18,8));
         end;
         CloseFile(InFile);
         DisplayAndPurgeStringList(Results,'Data set ' + IntToStr(i) + ',   n=' + IntToStr(Results.Count));
      end;

begin
   if (RoseData <> Nil) then begin
      Results := tStringList.Create;
      for i := 0 to 360 do Results.Add( IntegerToString(i,4) +  IntegerToString(RoseData^[i],8));
      DisplayAndPurgeStringList(Results,'Rose diagram data');
   end;
   for i := 1 to GraphDraw.DataFilesPlotted.Count do begin
       Results := tStringList.Create;
       assignFile(infile,GraphDraw.DataFilesPlotted.Strings[pred(i)]);
       reset(infile,2*SizeOf(float32));
       while not EOF(infile) do begin
          BlockRead(infile,v,1);
          if GraphDraw.GraphAxes in [XTimeYFullGrid,XTimeYPartGrid] then begin
             CalDat(round(v[1]),Month,Day,Year);
             Results.Add( IntegerToString(Month,2) + '/' + IntegerToString(Day,2) + '/' + IntegerToString(Year,4) + RealToString(v[2],18,8));
          end
          else Results.Add( RealToString(v[1],18,8) + ' ' + RealToString(v[2],18,8));
       end;
       CloseFile(InFile);
       if (GraphDraw.LegendList <> Nil) then TStr := ' (' + GraphDraw.LegendList.Strings[pred(i)] + ')'
       else TStr := '';
       DisplayAndPurgeStringList(Results,'Data set ' + IntToStr(i) + TStr + ',   n=' + IntToStr(Results.Count));
   end;
   if (GraphDraw.XYZFilesPlotted <> Nil) then for i := 1 to GraphDraw. XYZFilesPlotted.Count do DoFile(GraphDraw.XYZFilesPlotted.Strings[pred(i)]);
   for i := 1 to GraphDraw.XYColorFilesPlotted.Count do DoFile(GraphDraw.XYColorFilesPlotted.Strings[pred(i)]);
{$EndIf}
end;

procedure TThisBaseGraph.DragInXDirection1Click(Sender: TObject);
begin
   ChangeGraphDoing(gdXDrag);
end;

procedure TThisBaseGraph.DraginYdirection1Click(Sender: TObject);
begin
   ChangeGraphDoing(gdYDrag);
end;

procedure TThisBaseGraph.Dragresize1Click(Sender: TObject);
begin
   DragResize1.Checked := not DragResize1.Checked;
end;


procedure TThisBaseGraph.Image1MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
var
   newbmp : tMyBitmap;
   j : integer;
   xutm,yutm : float32;
begin
    wmdem.StatusBar1.Panels[1].Text :=  'graph x=' + RealToString(GraphDraw.InvGraphX(X),-12,2) + '  y=' + RealToString(GraphDraw.InvGraphY(Y),-12,2);

   if MouseIsDown and ScrollGraph and (GraphDoing = gdDoingNothing) and (not DragResize1.Checked) then begin
      ScrollBox1.HorzScrollBar.Position := ScrollBox1.HorzScrollBar.Position + SX - X;
      ScrollBox1.VertScrollBar.Position := ScrollBox1.VertScrollBar.Position + SY - Y;
   end
   else if (GraphDoing = gdDragEdit) then begin
      CloneBitmap(savedGraphImage,NewBMP);
      newbmp.Canvas.Draw(0,0,savedGraphImage);
      newbmp.Canvas.Draw(x,y,LegendBitmap);
      Image1.Picture.Graphic := NewBMP;
      NewBMP.Free;
   end
   else if (GraphDoing in [gdSecondDistance,gdSecondSlope]) then begin
      Image1.Canvas.Pen.Mode := pmNotXor;
      Image1.Canvas.Pen.Color := clRed;
      Image1.Canvas.Pen.Width := BoxLineWidth;
      Image1.Canvas.MoveTo(FirstX,FirstY);  Image1.Canvas.LineTo(LastX,LastY);
      Image1.Canvas.MoveTo(FirstX,FirstY);  Image1.Canvas.LineTo(X,Y);
   end
   else if MouseIsDown and (not (GraphDoing in [gdFreeDrag,gdXDrag,gdYDrag,gdFirstDistance])) then begin
      Image1.Canvas.Pen.Mode := pmNotXor;
      Image1.Canvas.Pen.Color := clRed;
      Image1.Canvas.Pen.Width := BoxLineWidth;
      Image1.Canvas.Rectangle(FirstX,FirstY,LastX,LastY);
      Image1.Canvas.Rectangle(FirstX,FirstY,x,y);
   end;
   LastX := x;
   LastY := y;

   if (MapOwner <> Nil) then begin
      if GraphDraw.GraphPlane in [0,1] then xutm := GraphDraw.InvGraphX(LastX)
      else xutm := GraphDraw.ThirdPlaneConstant;

      if GraphDraw.GraphPlane = 0 then Yutm := GraphDraw.InvGraphY(LastY)
      else if GraphDraw.GraphPlane = 2 then Yutm := GraphDraw.InvGraphX(LastX)
      else yutm := GraphDraw.ThirdPlaneConstant;

      MapOwner.MapDraw.PrimMapProj.UTMtoLatLongDegree(xutm,yutm,LastRoamLat,LastRoamLong);
      if (WmDEM.MDIChildCount > 0) then
        for j := WmDEM.MDIChildCount-1 downto 0 do
           if WmDEM.MDIChildren[j].Handle <> Self.Handle then
              PostMessage(WmDEM.MDIChildren[j].Handle,WM_BroadcastLatLongMessage,0,0);
      ApplicationProcessMessages;
   end;
end;


procedure TThisBaseGraph.Image1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   MouseIsDown := true;
   if (GraphDoing = gdDoingNothing) and ScrollGraph and (Button = mbLeft) and (not DragResize1.Checked) then begin
      SX := LastX;  // X start co-ordinate, image panning
      SY := LastY;  // Y start co-ordinate, image panning
      Forms.Screen.Cursor := crHandPoint;
   end
   else if (GraphDoing = gdXDrag) or (GraphDoing = gdYDrag) or (GraphDoing = gdFreeDrag) then begin
      FilterDBatCurrentPoint;
      if (GISdb[DataBaseOnGraph].MyData.RecordCount = 1) then begin
         ScreenSymbol(Image1.Canvas,x,y,GISdb[DataBaseOnGraph].dbOpts.Symbol);  //. GISdb[DataBaseOnGraph].dbOpts.Symbol.Size, clGray);
         Forms.Screen.Cursor := crDrag;
      end
      else begin
         GISdb[DataBaseOnGraph].dbOpts.GeoFilter :=  '';
         GISdb[DataBaseOnGraph].AssembleGISFilter;
      end;
   end
   else if (Button = mbRight) then begin
      LidarPanoramaLimits1.Visible := GraphDraw.PointCloudPanorama;
      DifferentSatellite1.Visible := GraphDraw.SatBands <> '';
      MouseIsDown := false;
      PopupMenu1.Popup(Mouse.CursorPos.X,Mouse.CursorPos.Y)
   end
   else begin
      FirstX := Lastx;
      FirstY := Lasty;
  end;
end;

procedure TThisBaseGraph.Image1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
const
   BinName : shortString = '';
   BinZ : float32 = 0;

   {$IfDef ExSlicer3D}
   {$Else}
      procedure DoIt;
      begin
         PetMath.MinOfPairFirst(FirstX,LastX);
         PetMath.MinOfPairFirst(LastY,FirstY);
         GISdb[SlicerForm.BoxOutlineGIS].MyData.SetFieldByNameAsFloat('X1',GraphDraw.InvGraphX(FirstX));  //NW
         GISdb[SlicerForm.BoxOutlineGIS].MyData.SetFieldByNameAsFloat('Y1',GraphDraw.InvGraphY(FirstY));
         GISdb[SlicerForm.BoxOutlineGIS].MyData.SetFieldByNameAsFloat('Z1',BinZ);
         GISdb[SlicerForm.BoxOutlineGIS].MyData.SetFieldByNameAsFloat('X4',GraphDraw.InvGraphX(FirstX));  //SW
         GISdb[SlicerForm.BoxOutlineGIS].MyData.SetFieldByNameAsFloat('Y4',GraphDraw.InvGraphY(LastY));
         GISdb[SlicerForm.BoxOutlineGIS].MyData.SetFieldByNameAsFloat('Z4',BinZ);
         GISdb[SlicerForm.BoxOutlineGIS].MyData.SetFieldByNameAsFloat('X2',GraphDraw.InvGraphX(LastX));    //NE
         GISdb[SlicerForm.BoxOutlineGIS].MyData.SetFieldByNameAsFloat('Y2',GraphDraw.InvGraphY(FirstY));
         GISdb[SlicerForm.BoxOutlineGIS].MyData.SetFieldByNameAsFloat('Z2',BinZ);
         GISdb[SlicerForm.BoxOutlineGIS].MyData.SetFieldByNameAsFloat('X3',GraphDraw.InvGraphX(LastX));     //SE
         GISdb[SlicerForm.BoxOutlineGIS].MyData.SetFieldByNameAsFloat('Y3',GraphDraw.InvGraphY(LastY));
         GISdb[SlicerForm.BoxOutlineGIS].MyData.SetFieldByNameAsFloat('Z3',BinZ);
         GISdb[SlicerForm.BoxOutlineGIS].MyData.Post;
      end;
   {$EndIf}

var
   xv,yv : float32;
begin
   {$IfDef ExSlicer3D}
   {$Else}
      if (GraphDoing = gdGraphDBBoxFilter) and MouseIsDown then begin
         SlicerForm.MaskBoxPoints(GraphDraw.InvGraphX(FirstX),GraphDraw.InvGraphX(LastX),GraphDraw.InvGraphY(LastY),GraphDraw.InvGraphY(FirstY));
      end;
      if (GraphDoing = gdBoxOutlineAdd) and MouseIsDown then begin
         ReadDefault('Bin z',BinZ);
         GISdb[SlicerForm.BoxOutlineGIS].MyData.Edit;
         DoIt;
      end;
      if (GraphDoing = gdBoxOutline) and MouseIsDown then begin
         Petmar.GetString('Bin name',BinName,false,ReasonableTextChars);
         ReadDefault('Bin z',BinZ);
         GISdb[SlicerForm.BoxOutlineGIS].MyData.Insert;
         GISdb[SlicerForm.BoxOutlineGIS].MyData.SetFieldByNameAsString('BIN_NAME',BinName);
         DoIt;
      end;
   {$EndIf}

   if ((GraphDoing = gdXDrag) or (GraphDoing = gdYDrag) or (GraphDoing = gdFreeDrag)) and MouseIsDown then begin
       GISdb[DataBaseOnGraph].MyData.Edit;
       xv := GraphDraw.InvGraphX(X);
       yv := GraphDraw.InvGraphY(Y);
       if ((GraphDoing = gdXDrag) or (GraphDoing = gdFreeDrag)) then GISdb[DataBaseOnGraph].MyData.SetFieldByNameAsFloat(XField,xv);
       if ((GraphDoing = gdYDrag) or (GraphDoing = gdFreeDrag)) then GISdb[DataBaseOnGraph].MyData.SetFieldByNameAsFloat(YField,yv);
       ScreenSymbol(Image1.Canvas,x,y,GISdb[DataBaseOnGraph].dbOpts.Symbol);
       GISdb[DataBaseOnGraph].MyData.Post;
       GISdb[DataBaseOnGraph].dbOpts.GeoFilter :=  '';
       GISdb[DataBaseOnGraph].AssembleGISFilter;
   end;

    if DragResize1.Checked and MouseIsDown then begin
       if (x <= FirstX) or (y <= FirstY) then DragDirections
       else begin
          MinOfPairFirst(x,FirstX);
          MinOfPairFirst(y,FirstY);

          {$IfDef RecordScaling} WriteLineToDebugFile('TThisBaseGraph.Image1MouseUp,  x range: ' + IntToStr(FirstX) + 'x' + IntToStr(x) + '  y range: ' + IntToStr(FirstY) + 'x' + IntToStr(y)); {$EndIf}
          GraphDraw.MinHorizAxis := GraphDraw.InvGraphX(FirstX);
          GraphDraw.MaxHorizAxis :=  GraphDraw.InvGraphX(x);
          if GraphDraw.GraphAxes in [XTimeYFullGrid,XTimeYPartGrid] then begin
             CalDat(Trunc(GraphDraw.MinHorizAxis),GraphDraw.Month1,GraphDraw.Day1,GraphDraw.Year1);
             CalDat(Trunc(GraphDraw.MaxHorizAxis),GraphDraw.Month2,GraphDraw.Day2,GraphDraw.Year2);
          end;

          if GraphDraw.NormalCartesianY then begin
             GraphDraw.MaxVertAxis := GraphDraw.InvGraphY(FirstY);
             GraphDraw.MinVertAxis := GraphDraw.InvGraphY(Y);
          end
          else begin
             GraphDraw.MinVertAxis := GraphDraw.InvGraphY(FirstY);
             GraphDraw.MaxVertAxis := GraphDraw.InvGraphY(Y);
          end;
          RedrawDiagram11Click(Nil);
          DragResize1.Checked := false;
       end;
    end;
    MouseIsDown := false;
    ShowDefaultCursor;
end;

procedure TThisBaseGraph.SpeedButton10Click(Sender: TObject);
begin
   {$IfDef MICRODEM}
   FontDialog1.Font.Size := FontDialog1.Font.Size - 1;
   MDDef.DefaultGraphFont.Size := FontDialog1.Font.Size;
   {$EndIf}
   RedrawDiagram11Click(Sender);
end;

procedure TThisBaseGraph.SpeedButton11Click(Sender: TObject);
begin
   ChangeGraphDoing(gdFirstDistance);
end;

procedure TThisBaseGraph.SpeedButton12Click(Sender: TObject);
begin
   if (SaveGraphName = '') then SaveGraphName := Petmar.NextFileNumber(MDTempDir, 'graph_',OverlayFExt);
   SaveImageAsBMP(Image1,SaveGraphName);
   SpeedButton13.Enabled := true;
end;

procedure TThisBaseGraph.SpeedButton13Click(Sender: TObject);
begin
   PetImage_form.PutMyBitmapIntoImage(SaveGraphName,Image1);
end;

procedure TThisBaseGraph.SpeedButton14Click(Sender: TObject);
begin
   ClearDataOnGraph;
   RedrawDiagram11Click(Nil);
end;

procedure TThisBaseGraph.SpeedButton15Click(Sender: TObject);
begin
   GraphDoing := gdFirstSlope;
end;

procedure TThisBaseGraph.SpeedButton2Click(Sender: TObject);
begin
   Saveimage1Click(Sender);
end;

procedure TThisBaseGraph.SpeedButton4Click(Sender: TObject);
begin
   Graphsettings2Click(Sender);
end;

procedure TThisBaseGraph.SpeedButton5Click(Sender: TObject);
begin
   Dragresize1Click(Sender);
end;

procedure TThisBaseGraph.SpeedButton6Click(Sender: TObject);
begin
   Linearfit1Click(LinearFit1);
end;

procedure TThisBaseGraph.LegendSpeedButtonClick(Sender: TObject);
begin
   Legend1Click(Sender);
end;

procedure TThisBaseGraph.SpeedButton7Click(Sender: TObject);
begin
   AssignImageToClipBoard(Image1);
end;

procedure TThisBaseGraph.ChangeGraphSettingsClick(Sender: TObject);
var
{$IfDef ExTernary}
{$Else}
  TernOptForm : TTernOptForm;
{$EndIf}
{$IfDef ExGraphs}
{$Else}
  GraphSettingsForm  : TGraphSettingsForm;
{$EndIf}
  i       : integer;
  FieldsInDB : tStringList;
  MyTable : tMyData;
  fName : PathStr;
  VisCols : Array100Boolean;
begin
   with GraphDraw do begin
   {$IfDef ExTernary}
   {$Else}
      if (GraphDraw.GraphType = gtTernary) then begin
         TernOptForm := TTernOptForm.Create(Application);
         SettingsChanged := true;
         TernOptForm.RadioGroup1.ItemIndex := ord(TernaryGrid);
         TernOptForm.RadioGroup2.ItemIndex := pred(TernaryScreenMult);
         TernOptForm.ShowModal;
         TernaryGrid := tTernaryGrid(TernOptForm.RadioGroup1.ItemIndex);
         TernaryScreenMult := succ(TernOptForm.RadioGroup2.ItemIndex);
         TernOptForm.Free;
         exit;
      end;
   {$EndIf}

      GraphSettingsForm := TGraphSettingsForm.Create(Application);
      GraphSettingsForm.OwningGraph := Self;
      SettingsChanged := false;
      with GraphSettingsForm do begin
         XLabelEdit.Text := HorizLabel;
         YLabelEdit.Text := VertLabel;
         ComboBox1.ItemIndex := ord(GraphAxes);
         ComboBox2.Visible := VertAxisFunctionType in [ShortCumNormalAxis,CumulativeNormalAxis,LongCumulativeNormalAxis,LongerCumulativeNormalAxis];
         CheckBox1.Visible := GraphAxes in [XTimeYFullGrid,XTimeYPartGrid];
         CheckBox1.Checked := AnnualCycle;
         CheckBox2.Checked := not NormalCartesianY;
         CheckBox3.Checked := Draw1to1Line;
         CheckBox5.Checked := not NormalCartesianX;
         CheckBox6.Checked := CorrectScaling;

         Edit7.Text := IntToStr(LeftMargin);
         Edit8.Text := IntToStr(TopMargin);
         Edit9.Text := IntToStr(BottomMargin);
         Edit10.Text := LLcornerText;

         if (DBFLineFilesPlotted.Count > 0) then begin
            ComboBox3.Visible := true;
            fName := DBFLineFilesPlotted.Strings[0];
            MyTable := tMyData.Create(fName);
            PetdbUtils.GetFields(MyTable,VisCols,NumericFieldTypes,FieldsInDB,true);
            for i := 0 to pred(FieldsInDB.Count) do ComboBox3.Items.Add(FieldsInDB.Strings[i]);
            ComboBox3.Text := '';
            MyTable.Destroy;
         end;
         if (XYZFilesPlotted.Count > 0) then begin
            Colors.Visible := true;
            Label1.Visible := true;
            Label4.Visible := true;
            Edit5.Visible := true;
            Edit6.Visible := true;
            Edit5.Text := RealToString(MinZ,-12,-6);
            Edit6.Text := RealToString(MaxZ,-12,-6);
         end;
         if ComboBox2.Visible then begin
            case VertAxisFunctionType of
               ShortCumNormalAxis : ComboBox2.ItemIndex := 0;
               CumulativeNormalAxis: ComboBox2.ItemIndex := 1;
               LongCumulativeNormalAxis : ComboBox2.ItemIndex := 2;
               LongerCumulativeNormalAxis : ComboBox2.ItemIndex := 3;
            end;
         end;
         if GraphDraw.GraphType in [gtTwoVertAxes] then begin
            YLabelEdit.Visible := false;
            YMaxEdit.Visible := false;
            YMinEdit.Visible := false;
            YMaxLabel.Visible := false;
            YMinLabel.Visible := false;
         end;
         if GraphAxes in [XTimeYFullGrid,XTimeYPartGrid] then begin
            XMinLabel.Caption := 'Ending';
            XMaxLabel.Caption := 'Starting';
            XLabelEdit.Visible := false;
            XMinEdit.Text := IntToStr(Day2);
            XMaxEdit.Text := IntToStr(Day1);
            Edit1.Text := IntToStr(Month1);
            Edit2.Text := IntToStr(Month2);
            Edit3.Text := IntToStr(Year1);
            Edit4.Text := IntToStr(Year2);
            Label2.Visible := true;
            Label3.Visible := true;
            Edit1.Visible := true;
            Edit2.Visible := true;
            Edit3.Visible := true;
            Edit4.Visible := true;
            Day.Visible := true;
         end
         else begin
            Edit1.Visible := false;
            Edit2.Visible := false;
            Edit3.Visible := false;
            Edit4.Visible := false;
            Label2.Visible := false;
            Label3.Visible := false;
            Day.Visible := false;
            XMinLabel.Caption := 'Min x';
            XMaxLabel.Caption := 'Max x';
            XLabelEdit.Visible := true;
            XMinEdit.Text := RealToString(MinHorizAxis,-18,-6);
            XMaxEdit.Text := RealToString(MaxHorizAxis,-18,-6);
         end;

         YMinEdit.Text := RealToString(MinVertAxis,-18,-6);
         YMaxEdit.Text := RealToString(MaxVertAxis,-18,-6);

          if (ShowModal = mrCancel) then SettingsChanged := false
          else begin
            SettingsChanged := true;
            CheckSettings;
          end;
      end;
      GraphSettingsForm.Destroy;
   end;
end;

procedure TThisBaseGraph.Zcolorrange1Click(Sender: TObject);
begin
   ReadDefault('Min z to show on graph',MinZShow);
   ReadDefault('Min z for color scale',MinZ);
   ReadDefault('Max z for color scale',MaxZ);
   RedrawDiagram11Click(Nil);
end;

procedure TThisBaseGraph.Zcolorscalelegend1Click(Sender: TObject);
begin
    GraphDraw.ZColorLegend := not GraphDraw.ZColorLegend;
    Zcolorscalelegend1.Checked := GraphDraw.ZColorLegend;
    GraphDraw.ForceNewSize := true;
    FormResize(Nil);
end;

procedure TThisBaseGraph.ContourLineWidth1Click(Sender: TObject);
begin
   ReadDefault('Contour line width',MDDef.ContourLineWidth);
end;

procedure TThisBaseGraph.LineandPointMarkers2Click(Sender: TObject);
begin
   {$IfDef VCL}
      SetGraphColors(Self);
   {$EndIf}
end;

procedure TThisBaseGraph.Monthlyaverage1Click(Sender: TObject);
{$IfDef VCL}
var
   infile : file;
   LastMonth,LastYear,
   Month,Day,Year,Gaps,
   i,n      : integer;
   Sum      : float32;
   v       : array[1..2] of float32;
   Results : tStringList;

   function NextMonth : boolean;
   begin
      if Month  = 1 then
         NextMonth := (LastMonth = 12) and (Year = succ(LastYear))
      else NextMonth := (Month = succ(LastMonth)) and (Year = LastYear);
   end;

begin
   if (GraphDraw.DataFilesPlotted.Count = 0) then exit;
   for i := 1 to GraphDraw.DataFilesPlotted.Count do begin
      assignFile(infile,GraphDraw.DataFilesPlotted.Strings[pred(i)]);
      reset(infile,2*SizeOf(float32));
      n := 0;
      Sum := 0;
      Gaps := 0;
      LastMonth := 100;
      LastYear  := 0;
      Results := tStringList.Create;
      ShowHourglassCursor;
      while not EOF(infile) do begin
         BlockRead(infile,v,1);
         CalDat(round(v[1]),Month,Day,Year);
         if (Month <> LastMonth) or (Year <> LastYear) then begin
            if n > 0 then begin
               Results.Add( IntegerToString(LastMonth,2) + '/' +IntegerToString(LastYear,4) +
                    RealToString(Sum/n,18,4));
            end;
            if (Results.Count > 1) and (not NextMonth) then begin
               inc(Gaps);
               Results.Add('Gap');
            end;
            n := 0;
            Sum := 0;
         end;
         inc(n);
         Sum := Sum + v[2];
         LastMonth := Month;
         LastYear := Year;
      end;
      ShowDefaultCursor;
      CloseFile(InFile);
      MessageToContinue('Months missing:' + IntegerToString(Gaps,8));
      DisplayAndPurgeStringList(Results,'Monthly Averages, with '+ IntegerToString(Gaps) + ' gaps');
   end;
{$EndIf}
end;


procedure TThisBaseGraph.SpeedButton8Click(Sender: TObject);
begin
   RedrawDiagram11Click(Nil);
end;

procedure TThisBaseGraph.SpeedButton9Click(Sender: TObject);
begin
   {$IfDef MICRODEM}
   FontDialog1.Font.Size := FontDialog1.Font.Size + 1;
   MDDef.DefaultGraphFont.Size := FontDialog1.Font.Size;
   {$EndIf}
   RedrawDiagram11Click(Sender);
end;


procedure TThisBaseGraph.Spot51Click(Sender: TObject);
begin
   GraphDraw.SatBands := 'SPOT5';
   RedrawDiagram11Click(Nil);
end;


procedure TThisBaseGraph.RedrawDiagram11Click(Sender: TObject);
var
   BitMap : tMyBitmap;
   LastXi,LastYi,LastYi2,
   xi,yi,yi2,i,j,err,NumRead  : integer;
   x      : float32;
   MenuStr : ShortString;
   aTable : tMyData;
   fName : PathStr;
   tf : file;
   Coords3 : ^Coord3Array;
begin
    if RedrawingNow then exit;
    RedrawingNow := true;
    {$IfDef RecordGraphColors} writelineToDebugFile('TThisBaseGraph.RedrawDiagram11Click'); {$EndIf}
    SetMenus;
    try
       if (RoseData <> Nil) then begin
          DrawAspectRose(RoseData^);
          RedrawingNow := false;
          exit;
       end;

       if GraphDraw.AutoPointDensity then begin
          MakePointDensityGraph(dsAll);
          RedrawingNow := false;
          exit;
       end;

       ShowHourglassCursor;
       CreateBitmap(Bitmap,ClientWidth,ClientHeight - Panel1.Height - ToolBar1.Height);
       GraphDraw.XWindowSize := Bitmap.Width;
       GraphDraw.YWindowSize := Bitmap.Height;
       Bitmap.Canvas.Font := FontDialog1.Font;

       if GraphDraw.GraphDrawn then begin
          if (GraphDraw.GraphType = gtTadpole) then begin
             SetMyBitmapColors(Bitmap,1);
             DrawGraph(BitMap);
             PlotXYZFile(BitMap,GraphDraw.XYZFilesPlotted.Strings[0]);
          end
          else if (GraphDraw.GraphType = gtTernary) then begin
             DrawTernaryDiagram(Bitmap);
             for i := 1 to GraphDraw.XYZFilesPlotted.Count do PlotXYZFile(BitMap,GraphDraw.XYZFilesPlotted.Strings[pred(i)]);
          end
          else {if GraphDraw.GraphDrawn then} begin
             DrawGraph(BitMap,true);
             if (GraphDraw.SatBands <> '') then begin
                ShowSatelliteBands(Bitmap);
             end;
             if (GraphDraw.GraphType = gtBoxPlot) then begin
                DrawBoxPlot(Bitmap);
                Image1.Picture.Graphic := Bitmap;
                Bitmap.Free;
                RedrawingNow := false;
                exit;
             end;

             if (GraphDraw.GraphType = gtTwoVertAxes) then begin
                  LastXI := -9999;
                  assignFile(tf,GraphDraw.XYZFilesPlotted[0]);
                  reset(tf,3*SizeOf(float32));
                  new(Coords3);
                  while not EOF(tf) do begin
                     BlockRead(tf,Coords3^,ASize,Numread);
                     for i := 1 to NumRead do begin
                        xi := GraphDraw.GraphX(Coords3^[(3*i)-2]);
                        yi := GraphDraw.GraphY(Coords3^[pred(3*i)]);
                        yi2 := GraphDraw.GraphY2(Coords3^[(3*i)]);
                        if GraphDraw.ShowPoints[1] and GraphDraw.PtOnGraph(xi,yi) then Petmar.ScreenSymbol(Bitmap.Canvas,xi,yi,GraphDraw.Symbol[1]);  //FilledBox,2,claRed);
                        if GraphDraw.ShowPoints[2] and GraphDraw.PtOnGraph(xi,yi2) then Petmar.ScreenSymbol(Bitmap.Canvas,xi,yi2,GraphDraw.Symbol[2]);  //FilledBox,2,claLime);
                        if LastXI > -9998 then begin
                           Bitmap.Canvas.Pen.Color := ConvertPlatformColorToTColor(LineColors10[1]);
                           if GraphDraw.ShowLine[1] then DrawLine(Bitmap,xi,yi,lastxi,lastyi);
                           Bitmap.Canvas.Pen.Color :=  ConvertPlatformColorToTColor(LineColors10[2]);
                           if GraphDraw.ShowLine[2] then DrawLine(Bitmap,xi,yi2,lastxi,lastyi2);
                         end;
                         LastXI := xi;
                         LastYI := yi;
                         LastYI2 := yi2;
                     end;
                  end;
                  Dispose(Coords3);
                  CloseFile(tf);
                  Image1.Picture.Graphic := Bitmap;
                  Bitmap.Free;
                  RedrawingNow := false;
                 exit;
             end;

             if (RangeGraphName <> '') then begin
                Image1.Picture.Graphic := Bitmap;
                Bitmap.Free;
                DrawRangesGraph;
                RedrawingNow := false;
                exit;
             end;

             for i := 1 to GraphDraw.XYZFilesPlotted.Count do begin
                PlotXYZFile(BitMap,GraphDraw.XYZFilesPlotted.Strings[pred(i)]);
             end;

             for i := 1 to GraphDraw.XYColorFilesPlotted.Count do begin
                PlotXYColorFile(BitMap,GraphDraw.XYColorFilesPlotted.Strings[pred(i)]);
             end;

             if FileExists(ASCIIXYZFile) then begin
                PlotXYZFile(BitMap,ASCIIXYZFile,true);
             end;

             if (GraphDraw.DataFilesPlottedTable <> '') then begin
               {$IfDef DataFilesPlottedTable} WritelineToDebugFile('DataFilesPlottedTable plotting start'); {$EndIf}
                aTable := tMyData.Create(GraphDraw.DataFilesPlottedTable);
                if aTable.FieldExists('GRAY') then begin
                    aTable.ApplyFilter('GRAY=' + QuotedStr('Y'));
                    i := 0;

                   {$IfDef DataFilesPlottedTable} WritelineToDebugFile('DataFilesPlottedTable plot grays'); {$EndIf}
                    while (not aTable.EOF) do begin
                       inc(i);
                       if aTable.GetFieldByNameAsString('PLOT') = 'Y' then begin
                          Bitmap.Canvas.Pen.Color := clGray;
                          Bitmap.Canvas.Pen.Width := aTable.GetFieldByNameAsInteger('LINE_WIDTH');
                          PlotAFile(BitMap,aTable.GetFieldByNameAsString('FILENAME'),i);
                       end;
                       aTable.Next;
                    end;

                   {$IfDef DataFilesPlottedTable} writelineToDebugFile('DataFilesPlottedTable plot colors'); {$EndIf}
                    aTable.ApplyFilter('GRAY=' + QuotedStr('N'));
                end;

                i := 0;
                while (not aTable.EOF) do begin
                   inc(i);
                   if (not aTable.FieldExists('PLOT')) or (aTable.GetFieldByNameAsString('PLOT') = 'Y') then begin
                      Bitmap.Canvas.Pen.Color := aTable.GetFieldByNameAsInteger('LINE_COLOR');
                      Bitmap.Canvas.Pen.Width := aTable.GetFieldByNameAsInteger('LINE_WIDTH');
                      PlotAFile(BitMap,aTable.GetFieldByNameAsString('FILENAME'),i);
                   end;
                   aTable.Next;
                end;

                aTable.Destroy;
               {$IfDef DataFilesPlottedTable} writelineToDebugFile('DataFilesPlottedTable done'); {$EndIf}
             end
             else begin
                 for i := 1 to GraphDraw.DataFilesPlotted.Count do begin
                     SetMyBitmapColors(Bitmap,i);
                     fName := GraphDraw.DataFilesPlotted.Strings[pred(i)];
                     if GraphDraw.ShowLine[i] then begin
                        {$IfDef RecordGraphColors} writelineToDebugFile(IntToStr(i) + '   ' + GraphDraw.DataFilesPlotted.Strings[pred(i)] + '  = '+ IntToStr(Bitmap.Canvas.Font.Color)); {$EndIf}
                        PlotAFile(BitMap,fName,i);
                     end;
                     j := i;
                     if (j > 15) then j := succ(j mod 15);
                     if GraphDraw.ShowPoints[i] then begin
                        {$IfDef RecordGraphColors} writelineToDebugFile(IntToStr(i) + '   ' + GraphDraw.DataFilesPlotted.Strings[pred(i)] + '  = '+ IntToStr(Bitmap.Canvas.Font.Color)); {$EndIf}
                        PlotPointFile(BitMap,fName,GraphDraw.Symbol[j]);
                     end;
                 end;
             end;
             for i := 1 to GraphDraw.DBFPointFilesPlotted.Count do begin
                PlotPointDBFFile(BitMap,GraphDraw.DBFPointFilesPlotted.Strings[pred(i)]);
             end;
             for i := 1 to GraphDraw.DBFLineFilesPlotted.Count do begin
                 SetMyBitmapColors(Bitmap,i);
                 PlotLineDBFFile(BitMap,GraphDraw.DBFLineFilesPlotted.Strings[pred(i)]);
             end;
             if (GraphDraw.GraphTopLabels <> Nil) and GraphDraw.LabelPointsAtop then begin
                Bitmap.Canvas.Pen.Color := clRed;
                Bitmap.Canvas.Pen.Width := 3;
                for i := 0 to pred(GraphDraw.GraphTopLabels.Count) do begin
                    MenuStr := GraphDraw.GraphTopLabels[i];
                    Val(copy(MenuStr,1,12),x,err);
                    Delete(MenuStr,1,14);
                    xi := GraphDraw.GraphX(x);
                    Bitmap.Canvas.MoveTo(xi,GraphDraw.TopMargin);
                    Bitmap.Canvas.LineTo(xi,GraphDraw.YWindowSize-GraphDraw.BottomMargin);
                    while (Bitmap.Canvas.TextWidth(MenuStr) > GraphDraw.TopMargin -15) and (Bitmap.Canvas.Font.Size > 4) do
                       Bitmap.Canvas.Font.Size := Bitmap.Canvas.Font.Size - 1;
                    xi := xi - Bitmap.Canvas.TextHeight(MenuStr) div 2;
                    if xi > Bitmap.Width - Bitmap.Canvas.TextHeight(MenuStr) then
                       xi := Bitmap.Width - Bitmap.Canvas.TextHeight(MenuStr);
                    TextOutVertical(Bitmap.Canvas,xi,(GraphDraw.TopMargin-5),MenuStr);
                end;
             end;

             {$IfDef ExSlicer3D}
             {$Else}
                if GraphDraw.PointCloudPanorama then begin
                   SlicerForm.RedrawSlicerPanorama(Bitmap);
                end;
            {$EndIf}


   //this is where we clean up outside the graph margins
             Bitmap.Canvas.Pen.Color := clWhite;
             Bitmap.Canvas.Brush.Color := clWhite;
             Bitmap.Canvas.Brush.Style := bsSolid;
             Bitmap.Canvas.Rectangle(0,0,GraphDraw.LeftMargin,Bitmap.Height);

             Bitmap.Canvas.Rectangle(0,Bitmap.Height-GraphDraw.BottomMargin,Bitmap.Width,Bitmap.Height);
             DrawGraph(BitMap,false);

             ReDrawNow := true;
          end;
       end;

       if (GraphDraw.UpperLeftText <> '') then begin
          Bitmap.Canvas.TextOut(GraphDraw.LeftMargin + 12, GraphDraw.TopMargin + 8, GraphDraw.UpperLeftText);
       end;
       if (LLcornerText <> '') then begin
          Bitmap.Canvas.Font.Color := clBlack;
          Bitmap.Canvas.TextOut(1, Bitmap.Height - Bitmap.Canvas.TextHeight(LLcornerText), RemoveUnderScores(LLcornerText));
       end;
       if (LRcornerText <> '') then begin
          Bitmap.Canvas.Font.Color := clBlack;
          Bitmap.Canvas.TextOut(Bitmap.Width - 3 - Bitmap.Canvas.TextWidth(LRcornerText), Bitmap.Height - Bitmap.Canvas.TextHeight(LRcornerText), RemoveUnderScores(LRcornerText));
       end;

       if (not GraphDraw.SkipDrawing) then Image1.Picture.Graphic := Bitmap;
       BitMap.Free;
       if (BaseCaption <> '') then with GraphDraw do begin
          Caption := BaseCaption + '   V.E.= ' + RealToString( ( YWindowSize / (MaxVertAxis - MinVertAxis)  / ( XWindowSize / (MaxHorizAxis - MinHorizAxis) * VertCompare)  ),-12,-2) ;
       end;
      {$IfDef TIDES}
      RedrawTideGraph;
      {$EndIf}
    finally
      ShowDefaultCursor;
      RedrawingNow := false;
    end;
end;


procedure TThisBaseGraph.Pointdensity1Click(Sender: TObject);
begin
   MakePointDensityGraph(dsAll);
end;


procedure TThisBaseGraph.Labelpointsatopprofile1Click(Sender: TObject);
begin
   GraphDraw.LabelPointsAtop := not GraphDraw.LabelPointsAtop;
   RedrawDiagram11Click(Nil);
end;


procedure TThisBaseGraph.Landsat1Click(Sender: TObject);
begin
   GraphDraw.SatBands := 'TM8';
   RedrawDiagram11Click(Nil);
end;

procedure TThisBaseGraph.Landsat2Click(Sender: TObject);
begin
   GraphDraw.SatBands := 'TM';
   RedrawDiagram11Click(Nil);
end;

procedure TThisBaseGraph.FitGraph(AllData : boolean; nt : integer; fName : PathStr; var a,b,r : float32; var n  : integer; LowLimitPoint : integer = 0; HighLimitPoint : integer = -1);
var
   infile : file;
   v      : array[1..3] of float32;
   x,y    : ^bfarray32;
   siga,sigb : float32;
   PtInSeries,NumRead,i : integer;
   Coords : ^tPointDataBuffer;

               procedure ProcessPoint;
               begin
                 if (AllData) or ( (v[1] <= GraphDraw.MaxHorizAxis) and (v[1] >= GraphDraw.MinHorizAxis) ) then begin
                    if (PtInSeries >= LowLimitPoint) then begin
                       x^[n] := v[1];
                       y^[n] := v[2];
                       inc(n);
                    end;
                    inc(PtInSeries);
                 end;
               end;


begin
   {$IfDef TimeGraphing} WriteLinetoDebugFile('TThisBaseGraph.FitGraph in ' + fname); {$EndIf}
   assignFile(infile, fName);
   reset(infile,nt*SizeOf(float32));
   n := 0;
   New(x);
   New(y);
   v[3] := 0;
   PtInSeries := 1;
   if (nt = 2) then begin
     New(Coords);
     while not EOF(InFile) do begin
        BlockRead(Infile,Coords^,MaxPointsInBuffer,Numread);
        for i := 1 to NumRead do begin
           v[1] := Coords^[i][1];
           v[2] := Coords^[i][2];
           ProcessPoint;
           if (HighLimitPoint > 0) and (PtInSeries > HighLimitPoint) then break;
        end;
     end;
     Dispose(Coords);
   end
   else begin
       while not EOF(infile) do with GraphDraw do begin
          BlockRead(infile,v,1);
          ProcessPoint;
          if (HighLimitPoint > 0) and (PtInSeries > HighLimitPoint) then break;
       end;
   end;
   CloseFile(InFile);

   {$IfDef ReverseFit}
   fit(y^,x^,n,a,B,siga,sigb,r);
   WriteLineToDebugFile('FitGraph, y,x:  ' + RealToString(a,18,4) +  RealToString(b,18,4) +   RealToString(r,18,4) + '   ' + IntToStr(n));
   {$EndIf}

   fit(x^,y^,n,a,B,siga,sigb,r);

   {$IfDef ReverseFit} WriteLineToDebugFile('FitGraph, x,y:  ' + RealToString(a,18,4) +  RealToString(b,18,4) +   RealToString(r,18,4) + '   ' + IntToStr(n)); {$EndIf}
   Dispose(x);
   Dispose(y);
end;

procedure TThisBaseGraph.BestSegmentFitGraph(nt : integer; fName : PathStr; var FittedInt,FittedSlope,FittedR : float32; var NPtsUsed : integer; PtsRequired : integer; SlopeDifferenceAllowed : float32);
var
   nPts,FirstPoint,LastPoint  : integer;
   siga,sigb,a,r,StartSlope,EndSlope,Difference  : float32;
   x,y    : ^bfarray32;
   infile : file;
   v      : array[1..2] of float32;
begin
   assignFile(infile, fName);
   reset(infile,nt*SizeOf(float32));
   nPts := 0;
   New(x);
   New(y);
   while not EOF(infile) do with GraphDraw do begin
      BlockRead(infile,v,1);
      x^[nPts] := v[1];
      y^[nPts] := v[2];
      inc(nPts);
   end;
   CloseFile(InFile);

   FirstPoint := 1;
   LastPoint := nPts;

   {$IfDef RecordFit}
      WriteLineToDebugFile('Guth père algorithm');
      WriteLineToDebugFile('---------------------');
      WriteLineToDebugFile('First Last  n    Slope    r²');
   {$EndIf}
   repeat
      FitOnRange(x^,y^,FirstPoint,LastPoint,nPts,FittedInt,FittedSlope,siga,sigb,FittedR);

      {$IfDef RecordFit} WriteLineToDebugFile(IntegerToString(FirstPoint,5) + IntegerToString(LastPoint,5) + IntegerToString(nPts,5) + RealToString(FittedSlope,9,3) + RealToString(sqr(FittedR),8,4)); {$EndIf}

      FitOnRange(x^,y^,FirstPoint,pred(LastPoint),npts,a,StartSlope,siga,sigb,R);

      {$IfDef RecordFit} WriteLineToDebugFile(IntegerToString(FirstPoint,5) + IntegerToString(pred(LastPoint),5) + IntegerToString(nPts,5) + RealToString(StartSlope,9,3) + RealToString(sqr(r),8,4)); {$EndIf}

      FitOnRange(x^,y^,succ(FirstPoint),LastPoint,npts,a,EndSlope,siga,sigb,R);
      Difference := (StartSlope-EndSlope) / FittedSlope;
      {$IfDef RecordFit}
      WriteLineToDebugFile(IntegerToString(succ(FirstPoint),5) + IntegerToString(LastPoint,5) + IntegerToString(nPts,5) + RealToString(EndSlope,9,3) + RealToString(sqr(r),8,4));
      WriteLineToDebugFile('Difference: ' + RealToString(Difference,8,4));
      {$EndIf}
      if abs(StartSlope-FittedSlope) > abs(EndSlope-FittedSlope) then inc(FirstPoint) else Dec(LastPoint);
      NPtsUsed := succ(LastPoint - FirstPoint );
   until (NPtsUsed <= PtsRequired) or (Difference <= SlopeDifferenceAllowed);
   WriteLineToDebugFile('');
   WriteLineToDebugFile('');
   Dispose(x);
   Dispose(y);
end;


procedure TThisBaseGraph.DrawBestFitLineOnGraph(a,b : float32);
var
   x1,y1 : integer;
begin
   with Image1.Canvas,GraphDraw do begin
      Pen.Color := ConvertPlatformColorToTColor(BestFitLineColor);
      Pen.Width := BestFitLineWidth;

      if (b > 0) then begin
         x1 := GraphX(MinHorizAxis);
         y1 := GraphY(a + b * (MinHorizAxis));
         if PtOnGraph(x1,y1) then MoveTo(x1,y1)
         else begin
            x1 := GraphX((MinVertAxis - a)/b);
            y1 := GraphY(MinVertAxis);
            MoveTo(x1,y1);
         end;

         x1 := GraphX(MaxHorizAxis);
         y1 := GraphY(a + b * (MaxHorizAxis));
         if PtOnGraph(x1,y1) then LineTo(x1,y1)
         else begin
            x1 := GraphX((MaxVertAxis - a)/b);
            y1 := GraphY(MaxVertAxis);
            LineTo(x1,y1);
         end;
      end
      else begin
         x1 := GraphX(MinHorizAxis);
         y1 := GraphY(a + b * (MinHorizAxis));
         if PtOnGraph(x1,y1) and (y1 <> 0) then MoveTo(x1,y1)
         else begin
            x1 := GraphX((MaxVertAxis - a)/b);
            y1 := GraphY(MaxVertAxis);
            MoveTo(x1,y1);
         end;

         x1 := GraphX(MaxHorizAxis);
         y1 := GraphY(a + b * (MaxHorizAxis));
         if PtOnGraph(x1,y1) then LineTo(x1,y1)
         else begin
            x1 := GraphX((MinVertAxis - a)/b);
            y1 := GraphY(MinVertAxis);
            LineTo(x1,y1);
         end;
      end;
   end;
end;


procedure TThisBaseGraph.Linearfit1Click(Sender: TObject);
var
   Results : tStringList;

   procedure ProcessFile(i : integer; Fname : PathStr; Nt : integer);
   var
      n  : integer;
      a,b,r,MinX,MaxX : float32;
   begin
      {$IfDef RecordFit} WriteLineToDebugFile('TThisBaseGraph.Linearfit1Click ' + fName); {$EndIf}

      MinX := GraphDraw.MinHorizAxis;
      MaxX := GraphDraw.MaxHorizAxis;

      FitGraph((Sender = Linearfit1),nt,fName, a,b,r,n);

      DrawBestFitLineOnGraph(a,b);

      if (Sender = Nil) then begin
         {$IfDef Record} WriteLineToDebugFile( RealToString(a,12,4) + realToString(b,12,4) + RealToString(r,12,4)); {$EndIf}
      end
      else begin
         if (GraphDraw.LegendList <> Nil) then Results.Add(GraphDraw.LegendList.Strings[i]);

         if GraphDraw.GraphAxes in [XTimeYFullGrid,XTimeYPartGrid] then begin
            Results.Add('Slope: ' + RealToString(b,-18,-8)  + ' /day');
            Results.Add('Slope: ' + RealToString(b*365.25,-18,-8)  + ' /yr');
         end
         else begin
            Results.Add('Intercept: ' + RealToString(a,-18,-8));
            Results.Add('Slope: ' + RealToString(b,-18,-8));
         end;
         Results.Add('r:' + RealToString(r,8,4));
         Results.Add('r²:' + RealToString(sqr(r),8,4));
         Results.Add('n=' + IntToStr(n));
         Results.Add('');
         FittedSlope := b;

         GraphDraw.MinHorizAxis := MinX;
         GraphDraw.MaxHorizAxis := MaxX;
      end;
   end;

var
   i : integer;
begin
   if (Sender <> Nil) then Results := tStringList.Create;
   for i := 0 to pred(GraphDraw.DataFilesPlotted.Count) do ProcessFile(i,GraphDraw.DataFilesPlotted.Strings[i],2);
   for i := 0 to pred(GraphDraw.XYZFilesPlotted.Count) do ProcessFile(i,GraphDraw.XYZFilesPlotted.Strings[i],3);
   if (Sender <> Nil) then Petmar.DisplayAndPurgeStringList(Results,'Linear fit');
end;


procedure TThisBaseGraph.Ongraphdataonly1Click(Sender: TObject);
begin
   Linearfit1Click(Sender);
end;

procedure TThisBaseGraph.Blackandwhitegraph1Click(Sender: TObject);
begin
   GrayScaleImage(Image1);
end;


procedure TThisBaseGraph.Blowupgraph1Click(Sender: TObject);
begin
   Blowupmap1Click(Sender);
end;

procedure TThisBaseGraph.Blowupmap1Click(Sender: TObject);
var
   BlowUp : float32;
begin
   BlowUp := 2;
   ReadDefault('Graph blow up',BlowUp);
   SizingWindow := true;
   ScrollGraph := true;
   ClientWidth := Round(BlowUp * ClientWidth);
   ClientHeight := Round(BlowUp * ClientHeight);
   RedrawDiagram11Click(Nil);
   SizingWindow := false;
end;

procedure TThisBaseGraph.Xaxis1Click(Sender: TObject);
begin
   MakePointDensityGraph(dsAxis);
end;

procedure TThisBaseGraph.Yaxis1Click(Sender: TObject);
begin
   MakePointDensityGraph(dsYAxis);
end;

procedure TThisBaseGraph.Copytoclipboard1Click(Sender: TObject);
begin
   AssignImageToClipBoard(Image1);
end;

procedure TThisBaseGraph.Showtoolbar1Click(Sender: TObject);
begin
   HideToolbar(Showtoolbar1.Checked);
end;

procedure TThisBaseGraph.Medianalongx1Click(Sender: TObject);
var
   Terms : integer;
begin
   Terms := 5;
   ReadDefault('Terms for median filter',Terms);
   Filter(Terms,true);
end;


function TThisBaseGraph.MakeLegend(Flist : tstringList; Extract : boolean) : tMyBitmap;
var
   MaxLen,Len,i : integer;
   TStr : shortString;
begin
   {$IfDef RecordGraphColors} WritelineToDebugFile('TThisBaseGraph.MakeLegend'); {$EndIf}
   CreateBitmap(Result,1000,25*Flist.Count+5);
   MaxLen := 0;

   for i := 1 to fList.Count do begin
       SetMyBitmapColors(Result,i);
      {$IfDef RecordGraphColors} WritelineToDebugFile(IntToStr(i) + '   ' + fList.Strings[pred(i)]+ '  = '+ IntToStr(Result.Canvas.Font.Color)); {$EndIf}
       Result.Canvas.Font.Style := [fsBold];
       Result.Canvas.Font.Name := FontDialog1.Font.Name;
       Result.Canvas.Font.Size := FontDialog1.Font.Size;
       if GraphDraw.ShowLine[i] then begin
          Result.Canvas.MoveTo(5,pred(i) * 25 + 12);
          Result.Canvas.LineTo(45,pred(i) * 25 + 12);
       end;

       if GraphDraw.ShowPoints[i] then begin
          GraphDraw.Symbol[i].Color := ConvertTColorToPlatformColor(Result.Canvas.Pen.Color);
          Petmar.ScreenSymbol(Result.Canvas,30,pred(i) * 25 + 12,GraphDraw.Symbol[i]);
       end;

       if Extract then TStr := ExtractFileName(fList.Strings[pred(i)])
       else TStr := fList.Strings[pred(i)];
       TStr := RemoveUnderScores(TStr);
       Len := Result.Canvas.TextWidth(TStr);
       if (Len > MaxLen) then MaxLen := Len;

       Result.Canvas.TextOut(50,5+ pred(i) * 25, TStr);
   end;
   Result.Width := 60 + MaxLen;
   Result.Canvas.Pen.Width := 1;
   Result.Canvas.Pen.Color := clBlack;
   Result.Canvas.Brush.Style := bsClear;
   Result.Canvas.Rectangle(0,0,pred(Result.Width),pred(Result.Height));
end;


procedure TThisBaseGraph.Legend1Click(Sender: TObject);
{$IfDef ExImages}
begin
{$Else}

   procedure MakeThisLegend(Flist : tstringList; Extract : boolean);
   var
      bmp : tMyBitmap;
   begin
      bmp := MakeLegend(Flist,Extract);
      AssignBitmapToClipBoard(bmp);
      bmp.Free;
   end;

begin
   if (GraphDraw.DBFLineFilesPlotted.Count > 0) then MakeThisLegend(GraphDraw.DBFLineFilesPlotted,true);
   if (GraphDraw.LegendList <> Nil) then MakeThisLegend(GraphDraw.LegendList,false);
   Pastefromclipboard1Click(Sender);
{$EndIf}
end;


procedure TThisBaseGraph.Legend2Click(Sender: TObject);
begin
   Legend1Click(Sender);
end;

procedure TThisBaseGraph.Alongxaxis2Click(Sender: TObject);
{$IfDef ExFourier}
begin
{$Else}
var
   fName : PathStr;
   NPts : integer;
begin
   fName := NextFileNumber(MDTempDir, GraphDraw.VertLabel,'');
   WriteDataSeriesASCII(Sender=AlongxAxis2,true,fname,NPts);
   CrossCor.CrossCorrelating(true,fName);
{$EndIf}
end;


procedure TThisBaseGraph.Aongxaxis1Click(Sender: TObject);
{$IfDef ExFourier}
begin
{$Else}
var
   fName : PathStr;
   NPts : integer;
begin
   fName := NextFileNumber(MDTempDir, GraphDraw.VertLabel,'');
   WriteDataSeriesASCII(true,true,fname,NPts);
   FitFourier.FitFourierCurve(fName);
{$EndIf}
end;

procedure TThisBaseGraph.Alongxaxis3Click(Sender: TObject);
{$IfDef ExFourier}
begin
{$Else}
var
   fName,fName2 : PathStr;
   NPts : integer;
begin
   fName := NextFileNumber(MDTempDir, GraphDraw.VertLabel,'');
   WriteDataSeriesASCII(true,true,fname,NPts,0);
   fName := NextFileNumber(MDTempDir, GraphDraw.VertLabel,'');
   WriteDataSeriesASCII(true,true,fname2,NPts,1);
   CrossCor.CrossCorrelating(false,fName,fName2);
{$EndIf}
end;

procedure TThisBaseGraph.N11scaling1Click(Sender: TObject);
begin
   GraphDraw.CorrectScaling := true;
   FormResize(Nil);
end;

procedure TThisBaseGraph.Rescalegraph1Click(Sender: TObject);
begin
   Graphsettings2Click(Sender);
end;

procedure TThisBaseGraph.Copytoclipboard2Click(Sender: TObject);
begin
   Copytoclipboard1Click(Sender);
end;

procedure TThisBaseGraph.Lidarpanoramalimits1Click(Sender: TObject);
begin
   {$IfDef ExPointCloud}
   {$Else}
      ReadDefault('Minimum range to show (m)',MDDef.LidarPanClose);
      ReadDefault('Maximum range to show (m)',MDDef.LidarPanFar);
      RedrawDiagram11Click(Nil);
   {$EndIf}
end;

procedure TThisBaseGraph.Lineandpointmarkers1Click(Sender: TObject);
begin
   LineandPointMarkers2Click(Sender);
end;

procedure TThisBaseGraph.Abortcurrentoperation1Click(Sender: TObject);
begin
   MouseIsDown := false;
   RedrawDiagram11Click(Nil);
end;

procedure TThisBaseGraph.Bestfitlinecolor1Click(Sender: TObject);
begin
   Petmar.PickLineSizeAndColor('Best fit line',Nil,BestFitLineColor,BestFitLineWidth);
end;

procedure  InitializeBaseGraf;
begin
   {$IfDef MessageStartupUnit}  MessageToContinue('Startup BaseGraf'); {$EndIf}
   FilterTerms := 11;
   DefaultClientWidth := 600;
   DefaultClientHeight := 400;
   TernaryScreenMult := 2;
   BestFitLineColor := claBlack;
   BestFitLineWidth := 3;
   CreateGraphHidden := false;
   CreateSmallGraph := false;
end;


{ tGraphDraw }

constructor tGraphDraw.Create;
begin
   AutoPointDensity := false;
   PointCloudPanorama := false;
   PointCloudSlice := false;
   SatBands := '';
   VertExag := 1;
end;

destructor tGraphDraw.Destroy;
begin
   inherited;
   UpperLeftText := '';
end;

initialization
   InitializeBaseGraf;
finalization
   {$IfDef RecordClosing} WriteLineToDebugFile('Closing basegraf in'); {$EndIf}
   {$IfDef RecordGraphMemoryAlocations} WiteLineToDebugFile('RecordGraphMemoryAlocations active in BaseGraf'); {$EndIf}
   {$IfDef RecordGraf} WriteLineToDebugFile('RecordGrafProblems active in BaseGraf'); {$EndIf}
   {$IfDef RecordTIN} writeLineToDebugFile('RecordTINProblems active in BaseGraf'); {$EndIf}
   {$IfDef RecordDetailedTIN} writeLineToDebugFile('RecordDetailedTINProblems active in BaseGraf'); {$EndIf}
   {$IfDef RecordFit} writeLineToDebugFile('RecordFitProblems active in BaseGraf'); {$EndIf}
   {$IfDef RecordFullFit} writeLineToDebugFile('RecordFullFitProblems active in BaseGraf'); {$EndIf}
   {$IfDef RecordHistogram} writeLineToDebugFile('RecordHistogramProblems active in BaseGraf'); {$EndIf}
   {$IfDef RecordGrafDensity} writeLineToDebugFile('RecordGrafDensityProblems active in BaseGraf'); {$EndIf}
   {$IfDef ReverseFit} writeLineToDebugFile('ReverseFit active in BaseGraf'); {$EndIf}
   {$IfDef RecordGrafAxis} writeLineToDebugFile('RecordGrafAxisProblems active in BaseGraf'); {$EndIf}
   {$IfDef TimeGraphing} writeLineToDebugFile('TimeGraphing active in BaseGraf'); {$EndIf}
   {$IfDef RecordHistogram} writeLineToDebugFile('RecordHistogram active in BaseGraf'); {$EndIf}
   {$IfDef RecordPlotFiles} writeLineToDebugFile('RecordPlotFiles active in BaseGraf'); {$EndIf}
   {$IfDef RecordScaling} writeLineToDebugFile('RecordScalingProblems active in BaseGraf'); {$EndIf}
   {$IfDef RecordGraphColors} writeLineToDebugFile('RecordGraphColors active in BaseGraf'); {$EndIf}
   {$IfDef RecordFormResize} writeLineToDebugFile('RecordFormResize active in BaseGraf'); {$EndIf}
   {$IfDef Closing} writeLineToDebugFile('ClosingProblems active in BaseGraf'); {$EndIf}
   {$IfDef RecordSaveSeries} WriteLineToDebugFile('RecordSaveSeries active in BaseGraf'); {$EndIf}
   {$IfDef RecordClosing} WriteLineToDebugFile('Closing basegraf out'); {$EndIf}
end.


