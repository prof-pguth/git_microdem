unit point_cloud_options;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems} //normally only defined for debugging specific problems
   {$IFDEF DEBUG}
      //{$Define RecordMakeGrid}
      {$Define BasicOpens}
      //{$Define RecordMakeBaseMap}
      //{$Define TrackPointCloud}
      //{$Define RecordExtractPoints}
      //{$Define Slicer}
      //{$Define BulkGrids}
      //{$Define RecordFirstPulse}
      //{$Define RecordLASfiles}
      //{$Define RecordMakeGridFull}
      //{$Define NoInline}
      //{$Define TimePointCloud}
      //{$Define RecordGridFileNames}
      //{$Define OGLexport}
      //{$Define PointCloudMap}
      //{$Define PointCloudOutlines}
      //{$Define RecordPointCloudViewing}
      //{$Define RecordLASfilesRedraw}
      {$Define RecordLASOpen}
      //{$Define RecordPointCloudOptionsForm}
      //{$Define RecordPointCloudViewing}
      //{$Define RecordNewGrids}
      //{$Define RecordBlocks}
      //{$Define ShowNewGridsMap}   //debug option to find voids not being done with Polar Stereographic
   {$ELSE}
      //{$Define RecordNewGrids}
   {$ENDIF}
{$EndIf}

interface

uses
//needed for inline of the core DB functions
   Petmar_db, Data.DB,
   {$IfDef UseFireDacSQLlite}
      FireDAC.Comp.Client, FireDAC.Comp.Dataset,FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteWrapper,
   {$EndIf}

   {$IfDef UseTDBF}
      dbf,
   {$EndIf}

   {$IfDef UseTCLientDataSet}
      DBClient,
   {$EndIf}
//end needed for inline of the core DB functions

  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.Menus,
  System.UITypes,
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs,  Buttons,Grids, DBGrids,

  DEMMapf, DEMDefs,Petmar_types,las_lidar,
  las_files_grouping,
  petimage_form,
  Point_cloud_Memory;


type
   tPCGridMaker = (pcgmAllIntensity,pcgmMaxIntensity,pcgmPointCount,pcgmCeilFloor,pcgmAboveBelow,pcgmClass,pcgmAirNonLastReturn,pcgmGround,pcgmFirstRet,pcgmSecondRet,pcgmThreeKeyDensities,pcgmMeanFirst,
       pcgmSingleRet,pcgmMeanStd,pcgmVegVox,pcgmDensityVox,pcgmScanAngle,pcgmGrndPtDTM,pcgmGroundLowXYZ,pcgmLowXYZ,pcgmClassification,pcgmRGB,pcgmOverlap,pcgmPointSourceID,pcgmUserData,pcgmBlank,pcgmMinIntensity);
   tWhatProcess = (wpWBClass,wpFusionClass,wpMCCclass,wpLasToolsClass,wpWBSegClass,wpPDALsmrf,wpPDALpmf,wpAllClass,wpWBDenoise);
   tdbpcPointCloud = (dbpcDataBase,dbpcDSM,dbpcMidCloud,dbpcDTM);

type
  Tpt_cloud_opts_fm = class(TForm)
    Panel1: TPanel;
    CancelBtn: TBitBtn;
    HelpBtn: TBitBtn;
    PageControl1: TPageControl;
    General: TTabSheet;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Panel2: TPanel;
    BitBtn5: TBitBtn;
    BitBtn8: TBitBtn;
    BitBtn13: TBitBtn;
    Edit2: TEdit;
    RadioGroup3: TRadioGroup;
    GroupBox1: TGroupBox;
    BitBtn4: TBitBtn;
    BitBtn15: TBitBtn;
    BitBtn14: TBitBtn;
    TabSheet3: TTabSheet;
    BitBtn17: TBitBtn;
    ZcritLabel: TLabel;
    BitBtn18: TBitBtn;
    BitBtn19: TBitBtn;
    Label6: TLabel;
    Edit8: TEdit;
    Label7: TLabel;
    Edit9: TEdit;
    BitBtn11: TBitBtn;
    BitBtn39: TBitBtn;
    BitBtn35: TBitBtn;
    BitBtn42: TBitBtn;
    CheckBox7: TCheckBox;
    BitBtn47: TBitBtn;
    BitBtn48: TBitBtn;
    BitBtn49: TBitBtn;
    Label10: TLabel;
    Edit10: TEdit;
    TabSheet5: TTabSheet;
    BitBtn44: TBitBtn;
    BitBtn43: TBitBtn;
    BitBtn29: TBitBtn;
    Label8: TLabel;
    Label9: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label5: TLabel;
    Edit6: TEdit;
    Edit5: TEdit;
    Label4: TLabel;
    Label3: TLabel;
    Edit3: TEdit;
    CheckBox8: TCheckBox;
    Edit11: TEdit;
    BitBtn31: TBitBtn;
    TabSheet6: TTabSheet;
    Thin: TLabel;
    CheckBox3: TCheckBox;
    Filters: TTabSheet;
    CheckBox11: TCheckBox;
    GroupBox6: TGroupBox;
    Edit1: TEdit;
    Edit7: TEdit;
    Edit15: TEdit;
    CheckBox13: TCheckBox;
    CheckBox10: TCheckBox;
    CheckBox12: TCheckBox;
    CheckBox95: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox1: TCheckBox;
    CheckBox4: TCheckBox;
    RadioGroup6: TRadioGroup;
    CheckBox14: TCheckBox;
    CheckBox15: TCheckBox;
    Edit16: TEdit;
    BitBtn40: TBitBtn;
    BitBtn52: TBitBtn;
    BitBtn53: TBitBtn;
    BitBtn54: TBitBtn;
    BitBtn55: TBitBtn;
    BitBtn56: TBitBtn;
    CheckBox17: TCheckBox;
    CheckBox19: TCheckBox;
    Label17: TLabel;
    Edit18: TEdit;
    CheckBox20: TCheckBox;
    Edit20: TEdit;
    BitBtn34: TBitBtn;
    BitBtn30: TBitBtn;
    BitBtn51: TBitBtn;
    BitBtn57: TBitBtn;
    PopupMenu2: TPopupMenu;
    PopupMenu3: TPopupMenu;
    PopupMenu4: TPopupMenu;
    Ceilingfloor1: TMenuItem;
    Ceiling1: TMenuItem;
    Floor1: TMenuItem;
    DTMfromgroundclassifiedpoints1: TMenuItem;
    Meanstandarddeviation1: TMenuItem;
    DistanceabovebelowDEM1: TMenuItem;
    Pointcount1: TMenuItem;
    Byclass1: TMenuItem;
    Firstreturns1: TMenuItem;
    Singlereturns1: TMenuItem;
    Nonlastreturns1: TMenuItem;
    GroundoverDTM1: TMenuItem;
    Returnintensity1: TMenuItem;
    VegetationVoxcels1: TMenuItem;
    Scanangle1: TMenuItem;
    BitBtn6: TBitBtn;
    GroupBox3: TGroupBox;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Edit13: TEdit;
    Edit12: TEdit;
    Edit14: TEdit;
    CheckBox18: TCheckBox;
    BitBtn21: TBitBtn;
    Lowpointsxyz1: TMenuItem;
    Lowpointxyz1: TMenuItem;
    Edit4: TEdit;
    Edit23: TEdit;
    Label2: TLabel;
    Label21: TLabel;
    Secondreturns1: TMenuItem;
    CheckBox22: TCheckBox;
    TabSheet4: TTabSheet;
    BitBtn37: TBitBtn;
    BitBtn46: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn9: TBitBtn;
    BitBtn12: TBitBtn;
    BitBtn10: TBitBtn;
    BitBtn16: TBitBtn;
    BitBtn22: TBitBtn;
    BitBtn23: TBitBtn;
    BitBtn28: TBitBtn;
    Label23: TLabel;
    DTMrangescalesfromgroundpoints1: TMenuItem;
    RadioGroup4: TRadioGroup;
    CheckBox23: TCheckBox;
    BitBtn33: TBitBtn;
    BitBtn36: TBitBtn;
    CheckBox25: TCheckBox;
    Las: TBitBtn;
    PopupMenu1: TPopupMenu;
    FusionTIN1: TMenuItem;
    WBIDW1: TMenuItem;
    WBnearestneighbor1: TMenuItem;
    blast2dem1: TMenuItem;
    MICRODEM1: TMenuItem;
    BitBtn1: TBitBtn;
    DSMNVSDTM1: TMenuItem;
    Classification1: TMenuItem;
    RadioGroup2: TRadioGroup;
    RGB1: TMenuItem;
    BitBtn25: TBitBtn;
    Returndensityvoxcels1: TMenuItem;
    Maxintenstiy1: TMenuItem;
    N1: TMenuItem;
    Overlappoints1: TMenuItem;
    CheckBox27: TCheckBox;
    CheckBox28: TCheckBox;
    N2: TMenuItem;
    Everything1: TMenuItem;
    PointsourceID1: TMenuItem;
    Userdata1: TMenuItem;
    CheckBox24: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox16: TCheckBox;
    CheckBox30: TCheckBox;
    CheckBox31: TCheckBox;
    BitBtn7: TBitBtn;
    BitBtn24: TBitBtn;
    Blankgrid1: TMenuItem;
    CheckBox21: TCheckBox;
    Label25: TLabel;
    Panel3: TPanel;
    RadioGroup1: TRadioGroup;
    GroupBox7: TGroupBox;
    CheckBoxPC1: TCheckBox;
    CheckBoxPC2: TCheckBox;
    CheckBoxPC3: TCheckBox;
    CheckBoxPC4: TCheckBox;
    CheckBoxPC5: TCheckBox;
    SymbolPC4: TBitBtn;
    SymbolPC3: TBitBtn;
    SymbolPC2: TBitBtn;
    SymbolPC1: TBitBtn;
    SymbolPC5: TBitBtn;
    BitBtn27: TBitBtn;
    BitBtn20: TBitBtn;
    BitBtn32: TBitBtn;
    TabSheet7: TTabSheet;
    GroupBox2: TGroupBox;
    Label28: TLabel;
    Label27: TLabel;
    Label29: TLabel;
    Label26: TLabel;
    Edit29: TEdit;
    Edit28: TEdit;
    Edit27: TEdit;
    BitBtn41: TBitBtn;
    BitBtn38: TBitBtn;
    Edit26: TEdit;
    Edit22: TEdit;
    BitBtn26: TBitBtn;
    CheckBox2: TCheckBox;
    CheckBox26: TCheckBox;
    TabSheet8: TTabSheet;
    RadioGroup5: TRadioGroup;
    Edit24: TEdit;
    Label22: TLabel;
    CheckBox9: TCheckBox;
    Edit30: TEdit;
    Label31: TLabel;
    BitBtn45: TBitBtn;
    Label24: TLabel;
    Edit25: TEdit;
    Edit21: TEdit;
    Label20: TLabel;
    Label18: TLabel;
    Edit17: TEdit;
    RadioGroup7: TRadioGroup;
    Label16: TLabel;
    BitBtn50: TBitBtn;
    Edit19: TEdit;
    Label19: TLabel;
    Label32: TLabel;
    Edit31: TEdit;
    CheckBox32: TCheckBox;
    BitBtn58: TBitBtn;
    CheckBox33: TCheckBox;
    N3: TMenuItem;
    hreekeydensities1: TMenuItem;
    N4: TMenuItem;
    Edit32: TEdit;
    Label33: TLabel;
    BitBtn59: TBitBtn;
    Meanfirstreturns1: TMenuItem;
    Label34: TLabel;
    Label35: TLabel;
    Edit33: TEdit;
    Edit34: TEdit;
    BitBtn60: TBitBtn;
    CheckBox99: TCheckBox;
    Minintensity1: TMenuItem;
    BitBtn61: TBitBtn;
    BitBtn62: TBitBtn;
    BitBtn64: TBitBtn;
    CheckBox34: TCheckBox;
    CheckBox29: TCheckBox;
    DTMfromlowestreturn1: TMenuItem;
    RadioGroup8: TRadioGroup;
    procedure BitBtn14Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure Edit3Change(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure HelpBtnClick(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure BitBtn8Click(Sender: TObject);
    procedure BitBtn12Click(Sender: TObject);
    procedure BitBtn13Click(Sender: TObject);
    procedure BitBtn16Click(Sender: TObject);
    procedure BitBtn17Click(Sender: TObject);
    procedure BitBtn19Click(Sender: TObject);
    procedure BitBtn21Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure BitBtn22Click(Sender: TObject);
    procedure BitBtn23Click(Sender: TObject);
    //procedure BitBtn24Click(Sender: TObject);
    procedure Edit8Change(Sender: TObject);
    procedure Edit9Change(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn11Click(Sender: TObject);
    procedure BitBtn29Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Edit7Change(Sender: TObject);
    procedure CheckBox6Click(Sender: TObject);
    procedure BitBtn35Click(Sender: TObject);
    procedure BitBtn37Click(Sender: TObject);
    procedure BitBtn39Click(Sender: TObject);
    procedure BitBtn42Click(Sender: TObject);
    procedure CheckBox7Click(Sender: TObject);
    procedure BitBtn43Click(Sender: TObject);
    procedure BitBtn44Click(Sender: TObject);
    procedure BitBtn45Click(Sender: TObject);
    procedure BitBtn46Click(Sender: TObject);
    procedure BitBtn47Click(Sender: TObject);
    procedure BitBtn49Click(Sender: TObject);
    procedure Edit10Change(Sender: TObject);
    procedure BitBtn18Click(Sender: TObject);
    procedure BitBtn6Click(Sender: TObject);
    procedure BitBtn50Click(Sender: TObject);
    procedure CheckBox8Click(Sender: TObject);
    procedure Edit11Change(Sender: TObject);
    procedure BitBtn31Click(Sender: TObject);
    procedure BitBtn33Click(Sender: TObject);
    procedure BitBtn32Click(Sender: TObject);
    procedure CheckBox9Click(Sender: TObject);
    procedure Edit12Change(Sender: TObject);
    procedure Edit13Change(Sender: TObject);
    procedure Edit14Change(Sender: TObject);
    procedure CheckBox5Click(Sender: TObject);
    procedure CheckBox95Click(Sender: TObject);
    procedure CheckBox10Click(Sender: TObject);
    procedure CheckBox11Click(Sender: TObject);
    procedure CheckBox12Click(Sender: TObject);
    procedure CheckBox13Click(Sender: TObject);
    procedure Edit15Change(Sender: TObject);
    procedure RadioGroup6Click(Sender: TObject);
    procedure CheckBox14Click(Sender: TObject);
    procedure Edit16Change(Sender: TObject);
    procedure CheckBox15Click(Sender: TObject);
    procedure BitBtn40Click(Sender: TObject);
    procedure BitBtn52Click(Sender: TObject);
    procedure BitBtn53Click(Sender: TObject);
    procedure BitBtn54Click(Sender: TObject);
    procedure BitBtn55Click(Sender: TObject);
    procedure BitBtn56Click(Sender: TObject);
    procedure CheckBoxPC1Click(Sender: TObject);
    procedure CheckBoxPC3Click(Sender: TObject);
    procedure CheckBoxPC2Click(Sender: TObject);
    procedure CheckBoxPC4Click(Sender: TObject);
    procedure CheckBox16Click(Sender: TObject);
    procedure Edit17Change(Sender: TObject);
    procedure CheckBox17Click(Sender: TObject);
    procedure BitBtn20Click(Sender: TObject);
    procedure BitBtn27Click(Sender: TObject);
    procedure CheckBox18Click(Sender: TObject);
    procedure CheckBox19Click(Sender: TObject);
    procedure Edit18Change(Sender: TObject);
    procedure CheckBox20Click(Sender: TObject);
    procedure Edit20Change(Sender: TObject);
    procedure BitBtn34Click(Sender: TObject);
    procedure Ceilingfloor1Click(Sender: TObject);
    procedure Ceiling1Click(Sender: TObject);
    procedure Floor1Click(Sender: TObject);
    procedure DTMfromgroundclassifiedpoints1Click(Sender: TObject);
    procedure Meanstandarddeviation1Click(Sender: TObject);
    procedure DistanceabovebelowDEM1Click(Sender: TObject);
    procedure BitBtn30Click(Sender: TObject);
    procedure BitBtn51Click(Sender: TObject);
    procedure BitBtn57Click(Sender: TObject);
    procedure Pointcount1Click(Sender: TObject);
    procedure Byclass1Click(Sender: TObject);
    procedure Firstreturns1Click(Sender: TObject);
    procedure Singlereturns1Click(Sender: TObject);
    procedure Nonlastreturns1Click(Sender: TObject);
    procedure GroundoverDTM1Click(Sender: TObject);
    procedure Returnintensity1Click(Sender: TObject);
    procedure VegetationVoxcels1Click(Sender: TObject);
    procedure Scanangle1Click(Sender: TObject);
    procedure Lowpointsxyz1Click(Sender: TObject);
    procedure Lowpointxyz1Click(Sender: TObject);
    procedure BitBtn9Click(Sender: TObject);
    procedure CheckBox21Click(Sender: TObject);
    procedure Edit23Change(Sender: TObject);
    procedure Edit4Change(Sender: TObject);
    procedure Secondreturns1Click(Sender: TObject);
    procedure BitBtn10Click(Sender: TObject);
    procedure CheckBox22Click(Sender: TObject);
    procedure BitBtn25Click(Sender: TObject);
    procedure Edit24Change(Sender: TObject);
    procedure BitBtn28Click(Sender: TObject);
    procedure CheckBoxPC5Click(Sender: TObject);
    procedure DTMrangescalesfromgroundpoints1Click(Sender: TObject);
    procedure RadioGroup4Click(Sender: TObject);
    procedure CheckBox23Click(Sender: TObject);
    procedure CheckBox24Click(Sender: TObject);
    procedure BitBtn36Click(Sender: TObject);
    procedure CheckBox25Click(Sender: TObject);
    procedure LasClick(Sender: TObject);
    procedure FusionTIN1Click(Sender: TObject);
    procedure WBIDW1Click(Sender: TObject);
    procedure blast2dem1Click(Sender: TObject);
    procedure WBnearestneighbor1Click(Sender: TObject);
    procedure MICRODEM1Click(Sender: TObject);
    procedure DSMNVSDTM1Click(Sender: TObject);
    procedure Edit25Change(Sender: TObject);
    procedure SymbolPC1Click(Sender: TObject);
    procedure SymbolPC2Click(Sender: TObject);
    procedure SymbolPC3Click(Sender: TObject);
    procedure SymbolPC4Click(Sender: TObject);
    procedure SymbolPC5Click(Sender: TObject);
    procedure Classification1Click(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure RGB1Click(Sender: TObject);
    procedure Returndensityvoxcels1Click(Sender: TObject);
    procedure Maxintenstiy1Click(Sender: TObject);
    procedure Overlappoints1Click(Sender: TObject);
    procedure CheckBox27Click(Sender: TObject);
    procedure CheckBox28Click(Sender: TObject);
    procedure Everything1Click(Sender: TObject);
    procedure PointsourceID1Click(Sender: TObject);
    procedure Userdata1Click(Sender: TObject);
    procedure CheckBox29Click(Sender: TObject);
    procedure Edit19Change(Sender: TObject);
    procedure CheckBox30Click(Sender: TObject);
    procedure RadioGroup5Click(Sender: TObject);
    procedure CheckBox31Click(Sender: TObject);
    procedure Edit21Change(Sender: TObject);
    procedure BitBtn7Click(Sender: TObject);
    procedure BitBtn24Click(Sender: TObject);
    procedure Blankgrid1Click(Sender: TObject);
    procedure BitBtn26Click(Sender: TObject);
    procedure BitBtn38Click(Sender: TObject);
    procedure BitBtn41Click(Sender: TObject);
    procedure Edit22Change(Sender: TObject);
    procedure Edit26Change(Sender: TObject);
    procedure Edit27Change(Sender: TObject);
    procedure Edit28Change(Sender: TObject);
    procedure Edit29Change(Sender: TObject);
    procedure RadioGroup7Click(Sender: TObject);
    procedure Edit30Change(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox26Click(Sender: TObject);
    procedure Edit31Change(Sender: TObject);
    procedure CheckBox32Click(Sender: TObject);
    procedure BitBtn58Click(Sender: TObject);
    procedure hreekeydensities1Click(Sender: TObject);
    procedure Edit32Change(Sender: TObject);
    procedure BitBtn59Click(Sender: TObject);
    procedure Meanfirstreturns1Click(Sender: TObject);
    procedure Edit33Change(Sender: TObject);
    procedure Edit34Change(Sender: TObject);
    procedure BitBtn60Click(Sender: TObject);
    procedure CheckBox99Click(Sender: TObject);
    procedure Minintensity1Click(Sender: TObject);
    procedure BitBtn61Click(Sender: TObject);
    procedure BitBtn62Click(Sender: TObject);
    procedure BitBtn64Click(Sender: TObject);
    procedure CheckBox34Click(Sender: TObject);
    procedure BitBtn63Click(Sender: TObject);
    procedure DTMfromlowestreturn1Click(Sender: TObject);
    procedure RadioGroup8Click(Sender: TObject);
  private
    { Private declarations }
    MinAreaZ,MaxAreaZ : float64;
    NeedNewElevationLimits,
    HasIntensity,
    HasClassification,
    ShowMeanDensityGrid,
    HasPointID,
    HasTime,
    HasRGB,
    HasUserData,
    HasScanAngle,
    HasReturnNumbers,
    CanAutoZoom,
    InitialCloudDisplay,
    FirstLoad : boolean;
    MemPtCloud : tMemoryPointCloud;
    StatusBarShortMess : ShortString;
    procedure ResyncFileList;
    procedure OutLineClouds;
    procedure ZoomToCloudCoverage(Layer : integer);
    function MakeGrid(PCGridMaker : tPCGridMaker) : integer;
    function TheCloudName : shortstring;
    procedure SetCloudOptions;
    procedure LasSubset(MergeLasWhat : tMergeLas; SubsetName,SubsetWhat : shortstring);
    procedure RedrawAllLayers;
    procedure UpdateColorOptions;
    procedure RunProcess(WhatProcess : tWhatProcess);
    procedure UncheckAll(setting : boolean = false);
    procedure Hideoptions;
    procedure LabelMemoryRequired;
    function FindDEMGridCellOfShot(LasData : Las_Lidar.tLAS_data; DEM,J : integer; var xgrid,ygrid : integer) : boolean;  {$If Defined(NoInline)} {$Else} inline; {$EndIf}
  public
    { Public declarations }
     BaseMap  : tMapForm;
     UsePC    : tUsePC;
     LasFiles : tLasFiles;
     AutoSaveDir,DEMrulesFName : PathStr;
     function GetFilesForPointCloud(CloudNum: integer; var BaseDir: PathStr; AutoLoad : boolean = false) : boolean;
     procedure ExtractPointsToDataBaseForPixel(DEM,Col,Row : integer; Buffer : integer = 0);
     function CreateGridOrDBfromStats(Option : tdbpcPointCloud; Percentile : float64 = 50; SaveName : PathStr = ''; OpenMap : boolean = true) : integer;
     function LegendForPointCloudLayers : tMyBitmap;
     procedure MapCallOutlineClouds;
     function  NumOpenClouds : integer;
     procedure HideForGridPick;
  end;


var
  pt_cloud_opts_fm : Tpt_cloud_opts_fm;


procedure OvelayPointClouds(inBaseMap : tMapForm; DirOpen : PathStr = '');
procedure GetGridParameters;
procedure pt_cloud_opts_fm_Close;


implementation

uses
   {$IfDef ExGIS}
   {$Else}
      demdatabase,
   {$EndIf}

   Icesat_filter_Form,
   gdal_tools,


   Petmar,PetDBUtils,PetImage,
   View3d_Main,
   PetMath, DEM_tin,
   DEMCoord,
   Thread_timers,
   Color_filter,
   Make_Grid,
   DEMDef_routines,
   DEM_Manager,
   MultiGrid,
   BaseGraf,
   MD_Use_tools,
   demmapdraw,
   Nevadia_Main, Make_tables, DEMESRIShapeFile, DEM_indexes, slicer_3d,
   demdbfilter, basemap, DEMStat, demstringgrid;

{$R *.dfm}



procedure OvelayPointClouds(inBaseMap : tMapForm; DirOpen : PathStr = '');
var
   Paths : tStringList;
   i : integer;
   fName : PathStr;
begin
   {$If Defined(RecordLASfiles) or Defined(RecordLASopen) or Defined(TrackPointCloud)} WriteLineToDebugFile('OvelayPointCloud in'); {$EndIf}
   MDDef.ShiftMercToUTM := true;
   pt_cloud_opts_fm := Tpt_cloud_opts_fm.Create(Application);
   pt_cloud_opts_fm.InitialCloudDisplay := true;
   pt_cloud_opts_fm.BaseMap := InBaseMap;
   if (inBaseMap <> Nil) then begin
      {$If Defined(RecordLASfiles) or Defined(RecordLASopen)} WriteLineToDebugFile('OvelayPointCloud start map creation'); {$EndIf}
      inBaseMap.MapDraw.DrawLegendsThisMap := false;
      pt_cloud_opts_fm.BaseMap.MapDraw.LasLayerOnMap := true;
      pt_cloud_opts_fm.Caption := 'Point clouds on ' + InBaseMap.Caption;
      InBaseMap.PointCloudBase := true;
      pt_cloud_opts_fm.BitBtn56.Visible := (InBaseMap.MapDraw.DEMonMap <> 0);
      pt_cloud_opts_fm.CanAutoZoom := false;
   end
   else pt_cloud_opts_fm.CanAutoZoom := true;
   pt_cloud_opts_fm.Show;
   if (DirOpen = '') then begin
      Paths := tStringList.Create;
      Paths.Add(LastLidarDirectory);
      if MDDef.PickLASDirs and GetMultipleDirectories('Lidar point clouds',Paths) then begin
         for i := 0 to pred(Paths.Count) do begin
            if (i=0) then LastLidarDirectory := Paths.Strings[0];
            if (succ(i) < MaxClouds) then begin
               if (i>1) then pt_cloud_opts_fm.InitialCloudDisplay := false;
               fName := Paths.Strings[i];
               pt_cloud_opts_fm.GetFilesForPointCloud(succ(i),fName,true);
            end;
         end;
      end
      else begin
         pt_cloud_opts_fm.GetFilesForPointCloud(1,LastLidarDirectory);
      end;
      Paths.Destroy;
   end
   else pt_cloud_opts_fm.GetFilesForPointCloud(1,DirOpen,true);
   pt_cloud_opts_fm.UpdateColorOptions;
   pt_cloud_opts_fm.InitialCloudDisplay := true;
   {$If Defined(RecordLASfiles) or Defined(RecordLASopen) or Defined(TrackPointCloud)} WriteLineToDebugFile('OvelayPointCloud out'); {$EndIf}
end;



procedure pt_cloud_opts_fm_Close;
begin
   if (pt_cloud_opts_fm <> Nil) then pt_cloud_opts_fm.Close;
   pt_cloud_opts_fm := Nil;
end;


procedure GetGridParameters;
begin
   {$IfDef RecordMakeGrid} WriteLineToDebugFile('GetGridParameters in'); {$EndIf}
   pt_cloud_opts_fm := Tpt_cloud_opts_fm.Create(Application);
   pt_cloud_opts_fm.HideForGridPick;
   pt_cloud_opts_fm.Caption := 'Set grid parameters';
   pt_cloud_opts_fm.BitBtn59.Visible := true;
   pt_cloud_opts_fm.ShowModal;
   {$IfDef RecordMakeGrid} WriteLineToDebugFile('GetGridParameters out'); {$EndIf}
end;



function Tpt_cloud_opts_fm.NumOpenClouds : integer;
var
   i : integer;
begin
   Result := 0;
   for i := 1 to MaxClouds do if (LasFiles[i] <> Nil) and (LasFiles[i].LAS_fnames.Count > 0) then inc(Result);
end;


function Tpt_cloud_opts_fm.TheCloudName : shortstring;

         procedure CheckCloud(Cloud : integer);
         begin
            if (Result = 'Merge') then exit;
            if UsePC[Cloud] then begin
               if (Result <> '') then begin
                  Result := 'Merge';
               end
               else Result := LasFiles[Cloud].CloudName;
            end;
         end;

var
   Cloud : integer;
begin
   Result := '';
   if UsePC[1] then Result := LasFiles[1].CloudName;
   for Cloud := 2 to MaxClouds do CheckCloud(Cloud);
end;



procedure Tpt_cloud_opts_fm.ExtractPointsToDataBaseForPixel(DEM,Col,Row : integer; Buffer : integer = 0);
var
   NoFilter : boolean;
   Findings : tStringList;
   CellElevs    : array[-100..5000] of Word;
   DoHistogram, DoDB : boolean;

           procedure OneLasLayer(Cloud,Col,Row : integer);
           var
              Bin, i,j,k,RecsRead,xgrid,ygrid : integer;
              Lat,Long,z : float64;
              fName : PathStr;
              NeedTile : boolean;
              LasData : Las_Lidar.tLAS_data;
           begin
             {$IfDef RecordExtractPoints} WriteLineToDebugFile('One layer in, cloud=' + IntToStr(Cloud) + '   points=' + IntToStr(LasFiles[Cloud].TotalCloudPts)); {$EndIf}
              if DoHistogram then for Bin := -100 to 5000 do begin
                 CellElevs[Bin] := 0;
              end;
              for k := 0 to Pred(LasFiles[Cloud].LAS_fnames.Count) do begin
                 ThreadTimers.OverallGauge9.Progress := round(100 * k / LasFiles[Cloud].LAS_fnames.Count);
                 fName := LasFiles[Cloud].LAS_fnames.Strings[k];
                 LasData := Las_Lidar.tLAS_data.Create(fName);
                 if DEMGlb[DEM].DEMHeader.DEMUsed = UTMBasedDEM then begin
                    NeedTile := LasData.InBoundBoxUTM(DEMGlb[DEM].PixelBoundBoxUTM(Col,Row));
                 end
                 else if DEMGlb[DEM].DEMHeader.DEMUsed = ArcSecDEM then begin
                    NeedTile := LasData.InBoundBoxGeo(DEMGlb[DEM].PixelBoundBoxGeo(Col,Row));
                 end
                 else begin
                 end;
                 if NeedTile then begin
                     {$IfDef RecordExtractPoints} WriteLineToDebugFile('Cloud=' + IntToStr(Cloud) + '   using=' + fName); {$EndIf}
                     LasData.PrepDataRead;
                     for i := 0 to LasData.ReadsRequired do begin
                         ThreadTimers.Gauge1.Progress := round(100 * i/LasData.ReadsRequired);
                         LasData.ReadPoints(RecsRead);
                         for j := 1 to RecsRead do begin
                            if NoFilter or LasData.MeetsFilter(j) then begin
                                if FindDEMGridCellOfShot(LasData,DEM,J,xgrid,ygrid) then begin
                                   if (xgrid = Col) and (ygrid = Row) then begin
                                      z := LasData.ExpandLAS_Z(j);
                                      if DoHistogram then inc(CellElevs[round(z)]);
                                      if DoDB then begin
                                         lasData.GetShotCoordinatesLatLong(j,Lat,Long);
                                         Findings.Add(RealToString(Lat,-12,-7) + ',' + RealToString(Long,-12,-7) + ',' + RealToString(z,-8,-2)  + ',' + IntToStr(LasData.LASClassification(j))) ;
                                      end;
                                   end;
                                end;
                            end;
                         end {for j};
                     end {for i};
                     LasData.FreeLASRecordMemory;
                 end;
                 LasData.Destroy;
              end {for k};
              {$IfDef RecordExtractPoints} WriteLineToDebugFile('One layer out, Cloud=' + IntToStr(Cloud)); {$EndIf}
           end;

var
   x,y,j,db,Bin,Cloud,Top : integer;
   Lat,Long : float64;
   z : float32;
   fName : PathStr;
   Color : tColor;
   Graph : TThisBaseGraph;
   Leg : tMyBitmap;
   rfile : file;
   v     : array[1..2] of float32;
begin
   {$IfDef RecordExtractPoints} WriteLineToDebugFile('ExtractPointsToDataBase in, DEM=' + DEMGlb[DEM].AreaName +  '   ' + sfBoundBoxToString(DEMGlb[DEM].PixelBoundBoxGeo(Col,Row))+ '  Buffer=' + IntToStr(Buffer)); {$EndIf}
   NoFilter := NoFilterWanted;

   DoHistogram := true;
   DoDB := true;

   for x := (Col - Buffer) to (Col + Buffer) do begin
      for y := (Row - Buffer) to (Row + Buffer) do begin
         {$IfDef RecordExtractPoints} WriteLineToDebugFile('Cell x=' + IntToStr(x) + '  y=' + IntToStr(y)); {$EndIf}
         StartThreadTimers('Point cloud ',1,true);
         if DoDB then begin
            Findings := tStringList.Create;
            Findings.Add('LAT,LONG,ELEV,CLASS');
         end;

         for Cloud := 1 to MaxClouds do if UsePC[Cloud] and (LasFiles[Cloud] <> Nil) then begin
            {$IfDef RecordExtractPoints} WriteLineToDebugFile('Start cloud=' + IntToStr(Cloud)); {$EndIf}
            OneLasLayer(Cloud,x,y);
            if DoDB then begin
               {$IfDef RecordExtractPoints} WriteLineToDebugFile('Do DB for cloud=' + IntToStr(Cloud)); {$EndIf}
               if (Findings.Count > 1) then begin
                  fName := Petmar.NextFileNumber(MDtempDir,'las_pts_col_' + IntToStr(Col) + '_row_' + IntToStr(Row),'.dbf');
                  fName := ChangeFileExt(fName,'.csv');
                  db := DEMGlb[DEM].SelectionMap.StringListToLoadedDatabase(Findings,fName,true,true);
                  GISDB[db].DisplayFieldStatistics('ELEV');
                  Color := clLime;
               end
               else begin
                  Findings.Destroy;
                  Color := clRed;
               end;
               if (DEMGlb[DEM].DEMHeader.DEMUsed = UTMBasedDEM) then begin
                  DEMGlb[DEM].SelectionMap.OutlineUTMBox(DEMGlb[DEM].PixelBoundBoxUTM(x,y),Color,2);
               end
               else begin
                  DEMGlb[DEM].SelectionMap.OutlineGeoBox(DEMGlb[DEM].PixelBoundBoxGeo(x,y),Color,2);
               end;
            end;

            if DoHistogram then begin
                {$IfDef RecordExtractPoints} WriteLineToDebugFile('Histogram Cell for cloud=' + IntToStr(Cloud)); {$EndIf}
                DEMGlb[DEM].DEMGridToLatLongDegree(Col,Row,Lat,Long);
                Graph := TThisBaseGraph.Create(Application);
                Graph.GraphDraw.VertLabel := 'Elev (m)';
                Graph.GraphDraw.HorizLabel := 'Returns ' + LatLongDegreeToString(Lat,Long);

                Graph.GraphDraw.MinHorizAxis := 0.0;
                Graph.GraphDraw.MaxHorizAxis := 10;
                Graph.GraphDraw.MinVertAxis := 6000;
                Graph.GraphDraw.MaxVertAxis := -500;
                Graph.GraphDraw.XWindowSize := 1000;
                Graph.GraphDraw.YWindowSize := 1000;
                Graph.SetUpGraphForm;

               Graph.OpenDataFile(rfile);
               Graph.GraphDraw.LineSize256[1] := 1;
               Graph.GraphDraw.ShowLine[1] := true;
               Graph.GraphDraw.FileColors256[1] := ConvertTColorToPlatformColor(clBrown);   //ConvertTColorToPlatformColor(WinGraphColors[1]);
               for Bin := -100 to 5000 do begin
                   if (CellElevs[Bin] > 0) then begin
                      if Bin > Graph.GraphDraw.MaxVertAxis then Graph.GraphDraw.MaxVertAxis := Bin + 10;
                      if Bin < Graph.GraphDraw.MinVertAxis then Graph.GraphDraw.MinVertAxis := Bin - 10;
                      if CellElevs[Bin] > Graph.GraphDraw.MaxHorizAxis then Graph.GraphDraw.MaxHorizAxis := CellElevs[Bin];
                      v[1] := CellElevs[Bin];
                      v[2] := Bin;
                      BlockWrite(rfile,v,1);
                   end;
               end;
               Graph.GraphDraw.MaxHorizAxis := Graph.GraphDraw.MaxHorizAxis + 0.2 * (Graph.GraphDraw.MaxHorizAxis - Graph.GraphDraw.MinHorizAxis);
               CloseFile(Rfile);

               for j := 1 to MaxCompare do begin
                  if (DEM_Manager.CompareDEMNames[j] <> '') then begin
                     if DEMGlb[DEM_Manager.CompareDEMIndexes[j]].GetElevFromLatLongDegree(Lat,Long,z) then begin
                        if z > Graph.GraphDraw.MaxVertAxis then Graph.GraphDraw.MaxVertAxis := z + 10;
                        if z < Graph.GraphDraw.MinVertAxis then Graph.GraphDraw.MinVertAxis := z - 10;
                     end;
                  end;
               end;

               Graph.RedrawDiagram11Click(Nil);
               CreateBitmap(Leg,200,200);
               Leg.Canvas.Font.Size := 14;
               Top := 5;

               for j := 1 to MaxCompare do begin
                  if (DEM_Manager.CompareDEMNames[j] <> '') then begin
                     if DEMGlb[DEM_Manager.CompareDEMIndexes[j]].GetElevFromLatLongDegree(Lat,Long,z) then begin
                        Petmar.ScreenSymbol(Graph.Image1.Canvas,(Graph.ClientWidth - 25),Graph.GraphDraw.Graphy(z),FilledBox,5,ConvertTColorToPlatformColor(WinGraphColors[j]));
                        Leg.Canvas.Brush.Style := bsSolid;
                        Leg.Canvas.Brush.Color := WinGraphColors[j];
                        Leg.Canvas.Rectangle(5,Top,25,Top+20);
                        Leg.Canvas.Brush.Style := bsClear;
                        Leg.Canvas.TextOut(30,Top,CompareDEMNames[j]);
                        Top := Top + 22;
                     end;
                  end;
               end;
               GetImagePartOfBitmap(Leg);
               DisplayBitmap(Leg,'DEMs');
            end;
            {$IfDef RecordExtractPoints} WriteLineToDebugFile('End cloud=' + IntToStr(Cloud)); {$EndIf}
         end {for i};
         EndThreadTimers;
      end {for y};
   end {for x};
   {$IfDef RecordExtract} WriteLineToDebugFile('ExtractPointsToDataBase out'); {$EndIf}
end;


procedure Tpt_cloud_opts_fm.VegetationVoxcels1Click(Sender: TObject);
begin
   MakeGrid(pcgmVegVox);
end;

procedure Tpt_cloud_opts_fm.WBIDW1Click(Sender: TObject);
var
   i : integer;
   LasName,DTMName : PathStr;
begin
    for i := 1 to MaxClouds do if UsePC[i] and (LasFiles[i] <> Nil) then begin
       LasName := NextFileNumber(MDtempDir, 'wbidw_dtm_','.las');
       LasFiles[i].ExtractGroundPoints(LasName);
       DTMname := NextFileNumber(MdtempDir, LasFiles[i].CloudName + '_wbidw_dtm_','.tif');
       WBT_IDWCreate(true,LasName,DTMName,MDdef.DefLidarXGridSize);
    end;
end;

procedure Tpt_cloud_opts_fm.WBnearestneighbor1Click(Sender: TObject);
var
   i : integer;
   LasName,DTMName : PathStr;
begin
    for i := 1 to MaxClouds do if UsePC[i] and (LasFiles[i] <> Nil) then begin
       LasName := NextFileNumber(MDtempDir, 'wb_near_neigh_','.las');
       LasFiles[i].ExtractGroundPoints(LasName);
       DTMname := NextFileNumber(MdtempDir, LasFiles[i].CloudName + '_wb_near_neigh_dtm_','.tif');
       WBT_BNearNeighCreate(true,LasName,DTMName,MDdef.DefLidarXGridSize);
    end;
end;


function Tpt_cloud_opts_fm.FindDEMGridCellOfShot(LasData : Las_Lidar.tLAS_data; DEM,J : integer; var xgrid,ygrid : integer) : boolean;
var
   xApp,yApp, xf,yf : float64;
begin
    if (MDDef.LidarGridProjection = UTMBasedDEM) then begin
        LasData.GetShotCoordinatesUTM(j,xApp,yApp);
        DEMGlb[DEM].UTMToDEMGrid(xApp,yApp,xf,yf,Result);
    end
    else if (MDDef.LidarGridProjection in [ArcSecDEM,WKTDEM]) then begin
       LasData.GetShotCoordinatesLatLong(j,yApp,xApp);
       DEMGlb[DEM].LatLongDegreeToDEMGrid(yApp,xApp,xf,yf);
       Result := DEMGlb[DEM].GridInDataSetFloat(xf,yf);
    end;
    xgrid := round(xf);
    ygrid := round(yf);
end;


function Tpt_cloud_opts_fm.MakeGrid(PCGridMaker : tPCGridMaker) : integer;


    function MakeGridFromLidarCloud(TheCloudName : shortString; PCGridMaker : tPCGridMaker; BaseMap : tMapForm;  UsePC : tUsePC; LasFiles : tLasFiles) : integer;
    var
       NewHeadRecs : tDEMheader;
         AirReturnDEM,
         BlankDEM,
         ClassDEM,
         DSMDEM,
         MaxIntensityDEM,
         MaxScanAngleDEM,
         MeanIntensityDEM,
         MeanReturnHeightDEM,
         MeanReturnStdDEM,
         MinIntensityDEM,
         MinScanAngleDEM,
         NewBuildingDEM,
         NewDensity,
         NewDistance,
         NewFirstReturnDEM,
         NewGroundDensity,
         NewGroundMax,
         NewGroundMean,
         NewGroundMin,
         NewGroundNearest,
         NewGroundXYZ,
         NewGroundXYZDEM,
         NewLowXYZ,
         NewLowXYZDEM,
         NewOtherDEM,
         NewPointIDdem,
         NewRGBGrid,
         NewUnclassDEM,
         NewUserDataDEM,
         NewVegDEM,
         NVSDEM,
         OverlapDEM,
         SecondReturnDEM,
         SingleReturnDEM,
         TempDEM,
       CheckDEM,
       Intensity,
       i,RecsRead : integer;
       zShot,zCrit: float64;
       fName,OutDir : PathStr;
       Count : int64;
       Ext : ExtStr;
       LasData : Las_Lidar.tLAS_data;
       VegDensity : array[1..MaxVegLayers] of integer;



         procedure CreateNewGrids;

              function NewGridName(What : shortString) : ShortString;
              begin
                 Result := What + TheCloudName;
              end;

              procedure MissingDataGrid(var DEM : integer; GridName : ShortString; InitialValueMode : integer = InitDEMMissing);
              begin
                 OpenAndZeroNewDEM(true,NewHeadRecs,DEM,GridName,InitialValueMode);
                 CheckDEM := DEM;
                 {$IfDef RecordMakeGrid} WriteLineToDebugFile('Missing data grid out, ' + DEMGlb[DEM].FullDEMParams); {$EndIf}
              end;

         var
            i : integer;
         begin
             NewHeadRecs := DEMGlb[TempDEM].DEMheader;
             NewHeadRecs.RasterPixelIsGeoKey1025 := MDDef.LasDEMPixelIs;
             NewHeadRecs.DEMPrecision := LongWordDEM;
             NewHeadRecs.ElevUnits := euUndefined;

             if (PCGridMaker in [pcgmAllIntensity,pcgmClass,pcgmPointCount,pcgmThreeKeyDensities,pcgmMeanStd,pcgmMeanFirst]) then MissingDataGrid(NewDensity,NewGridname('Total_pt_density_'),InitDEMzero);
             if (PCGridMaker = pcgmSecondRet) then MissingDataGrid(SecondReturnDEM,NewGridname('Second_return_density_'),InitDEMzero);
             if (PCGridMaker = pcgmSingleRet) then MissingDataGrid(SingleReturnDEM,NewGridname('Single_return_density_'),InitDEMzero);
             if (PCGridMaker = pcgmOverlap) then MissingDataGrid(OverlapDEM,NewGridname('Overlap_density_'),InitDEMzero);
             if (PCGridMaker in [pcgmFirstRet,pcgmThreeKeyDensities]) then MissingDataGrid(NewFirstReturnDEM,NewGridname('First_return_density_'),InitDEMzero);
             if (PCGridMaker in [pcgmThreeKeyDensities,pcgmGround,pcgmGrndPtDTM]) then MissingDataGrid(NewGroundDensity,NewGridname('Ground_return_density_'),InitDEMzero);
             if (PCGridMaker in [pcgmAirNonLastReturn]) then MissingDataGrid(AirReturnDEM,NewGridname('Non_last_return_density_'),InitDEMzero);
             if (PCGridMaker = pcgmBlank) then MissingDataGrid(BlankDEM,NewGridname('Blank_grid_'));

             if (PCGridMaker in [pcgmRGB]) then begin
                NewHeadRecs.ElevUnits := euRGB;
                MissingDataGrid(NewRGBGrid,NewGridname('rgb_'));
             end;

             NewHeadRecs.DEMPrecision := FloatingPointDEM;
             NewHeadRecs.ElevUnits := euMeters;
             if (PCGridMaker in [pcgmMeanStd]) then begin
                MissingDataGrid(MeanReturnHeightDEM,NewGridname('Mean_return_ht_'));
                OpenAndZeroNewDEM(true,NewHeadRecs,MeanReturnStdDEM,NewGridname('Mean_return_std_'),InitDEMvalue,0);
             end
             else if (PCGridMaker in [pcgmMeanFirst]) then begin
                MissingDataGrid(MeanReturnHeightDEM,NewGridname('Mean_first_return_ht_'));
                OpenAndZeroNewDEM(true,NewHeadRecs,MeanReturnStdDEM,NewGridname('Mean_first_return_std_'),InitDEMvalue,0);
             end
             else if (PCGridMaker in [pcgmLowXYZ]) then MissingDataGrid(NewLowXYZDEM,NewGridname('low_min_elev_'))
             else if (PCGridMaker in [pcgmGroundLowXYZ]) then MissingDataGrid(NewGroundXYZDEM,NewGridname('low_ground_min_elev_'))
             else if (PCGridMaker in [pcgmCeilFloor]) then begin
                if (MDDef.MakePCFloor) then MissingDataGrid(NVSDEM,NewGridname('NVS_'));
                if (MDDef.MakePCCeiling) then MissingDataGrid(DSMdem,NewGridname('DSM_'));
             end;

             if (PCGridMaker in [pcgmGrndPtDTM]) then begin
                {$IfDef RecordMakeGrid} WriteLineToDebugFile('start pcgmGrndPtDTM'); {$EndIf}
                NewHeadRecs.DEMPrecision := FloatingPointDEM;
                NewHeadRecs.ElevUnits := euMeters;
                if (MDDef.DTMoption in [dtmAll,dtmMean]) then MissingDataGrid(NewGroundMean,NewGridname('DTM_ground_pts_Mean_'));
                if (MDDef.DTMoption in [dtmAll,dtmMax]) then MissingDataGrid(NewGroundMax,NewGridname('DTM_ground_pts_Max_'));
                if (MDDef.DTMoption in [dtmAll,dtmMin]) then MissingDataGrid(NewGroundMin,NewGridname('DTM_ground_pts_Min_'));
                if (MDDef.DTMoption in [dtmAll,dtmNearest]) then begin
                   MissingDataGrid(NewGroundNearest,NewGridname('DTM_ground_pts_Nearest_'));
                   MissingDataGrid(NewDistance,NewGridname('DTM_ground_pts_Dist_'));
                end;
                {$IfDef RecordMakeGrid} WriteLineToDebugFile('done prep for pcgmGrndPtDTM'); {$EndIf}
             end
             else if (PCGridMaker = pcgmVegVox) then begin
                OutDir := ExtractFilePath(DEMGlb[BaseMap.MapDraw.DEMonMap].DEMFileName);
                GetDosPath('Vegetation density grids',OutDir);
                if (OutDir = '') then exit;
                ReadDefault('Layers',MDDef.VegDensityHeights);
                if (MDDef.VegDensityHeights > MaxVegLayers) then MDDef.VegDensityHeights := MaxVegLayers;
                MDDef.DiscardHighPointsVegDensity := AnswerIsYes('Discard points above ' + IntToStr(MDDef.VegDensityHeights) + 'm');

                StartProgress('open');
                for i := 1 to MDDef.VegDensityHeights do begin
                   NewHeadRecs.DEMPrecision := ByteDEM;
                   NewHeadRecs.ElevUnits := euUndefined;
                   UpdateProgressBar(i/MaxVegLayers);
                   OpenAndZeroNewDEM(true,NewHeadRecs,VegDensity[i],'Voxel up to ' + IntToStr(i),InitDEMvalue,0);
                end;
                EndProgress;
             end
             else if (PCGridMaker = pcgmDensityVox) then begin
                {$IfDef RecordMakeGrid} WriteLineToDebugFile('Tpt_cloud_opts_fm.BitBtn9Click set up PCGridMaker = pcgmDensityVox'); {$EndIf}
                GetDosPath('Point cloud density voxels',OutDir);
                if (OutDir = '') then exit;
                MDDef.VoxBinHeight := 1;
                while (round(MaxAreaZ-MinAreaZ) div MDDef.VoxBinHeight) > MDDef.VegDensityHeights  do inc(MDDef.VoxBinHeight);
                StartProgress('open');
                for i := 1 to MaxVegLayers do begin
                   NewHeadRecs.DEMPrecision := SmallIntDEM;
                   NewHeadRecs.ElevUnits := euUndefined;
                   UpdateProgressBar(i/MaxVegLayers);
                   OpenAndZeroNewDEM(true,NewHeadRecs,VegDensity[i],'Voxel height ' + IntToStr(i * MDDef.VoxBinHeight),InitDEMvalue,0);
                end;
                EndProgress;
                {$IfDef RecordMakeGrid} WriteLineToDebugFile('Tpt_cloud_opts_fm.BitBtn9Click done set up PCGridMaker = pcgmDensityVox'); {$EndIf}
             end
             else if (PCGridMaker in [pcgmScanAngle]) then begin
                NewHeadRecs.DEMPrecision := SmallIntDEM;  //byte would work for scan angle, but we need negatives; and intensities go over 255
                NewHeadRecs.ElevUnits := euUndefined;
                OpenAndZeroNewDEM(true,NewHeadRecs,MinScanAngleDEM,NewGridname('Min_scan_angle_'),InitDEMvalue,-9999);
                OpenAndZeroNewDEM(true,NewHeadRecs,MaxScanAngleDEM,NewGridname('Max_scan_angle_'),InitDEMvalue,9999);
             end
             else if (PCGridMaker in [pcgmPointSourceID,pcgmUserData]) then begin
                NewHeadRecs.ElevUnits := euUndefined;
                NewHeadRecs.DEMPrecision := WordDEM;
                MissingDataGrid(NewPointIDDEM,NewGridname('Point_ID_'));
                NewHeadRecs.DEMPrecision := ByteDEM;
                MissingDataGrid(NewUserDataDEM,NewGridname('User_data_'));
             end
             else if (PCGridMaker in [pcgmAllIntensity,pcGMMaxIntensity,pcgmMinIntensity]) then begin
                NewHeadRecs.DEMPrecision := WordDEM;
                NewHeadRecs.ElevUnits := euImagery;
                if (PCGridMaker in [pcgmAllIntensity,pcGMMaxIntensity]) then MissingDataGrid(MaxIntensityDEM,NewGridname('Lidar_max_intensity_'));
               if (PCGridMaker in [pcgmAllIntensity,pcgmMinIntensity]) then MissingDataGrid(MaxIntensityDEM,NewGridname('Lidar_min_intensity_'));
                 if (PCGridMaker in [pcgmAllIntensity]) then begin
                     OpenAndZeroNewDEM(true,NewHeadRecs,NewDensity,NewGridname('Num_ground_returns_'),InitDEMvalue,0);
                     NewHeadRecs.DEMPrecision := FloatingPointDEM;
                     OpenAndZeroNewDEM(true,NewHeadRecs,MeanIntensityDEM,NewGridname('Lidar_mean_intensity_'),InitDEMvalue,0);
                 end;
             end
             else if (PCGridMaker in [pcgmClass]) then begin
                NewHeadRecs.DEMPrecision := WordDEM;
                NewHeadRecs.ElevUnits := euUndefined;
                OpenAndZeroNewDEM(true,NewHeadRecs,NewDensity,NewGridname('total_density'),InitDEMvalue,0);
                if (PCGridMaker = pcgmClass) then begin
                   OpenAndZeroNewDEM(true,NewHeadRecs,NewVegDEM,NewGridname('Vegetation_density_'),InitDEMvalue,0);
                   OpenAndZeroNewDEM(true,NewHeadRecs,NewUnclassDEM,NewGridname('Unclassified_density_'),InitDEMvalue,0);
                   OpenAndZeroNewDEM(true,NewHeadRecs,NewGroundDensity,NewGridname('Ground_density_'),InitDEMvalue,0);
                   OpenAndZeroNewDEM(true,NewHeadRecs,NewBuildingDEM,NewGridname('Building_density_'),InitDEMvalue,0);
                   OpenAndZeroNewDEM(true,NewHeadRecs,NewOtherDEM,NewGridname('Other_density_'),InitDEMvalue,0);
                end;
             end
             else if (PCGridMaker = pcgmClassification) then begin
                NewHeadRecs.DEMPrecision := ByteDEM;
                //need to deal with LAS 1.4
                NewHeadRecs.ElevUnits := euLASClass13;
                OpenAndZeroNewDEM(true,NewHeadRecs,ClassDEM,NewGridname('Classification_'),InitDEMmissing);
                CheckDEM := ClassDEM;
             end;
         end;


           procedure OneLasLayer(Layer : integer);
           var
              xgrid,ygrid,k,i,j,LasClass : integer;
              //r,g,b : byte;
              zGrid,Dist,xf,yf : float32;
              NoFilter,OK : boolean;
              //TStr : shortstring;
           begin
              NoFilter := NoFilterWanted;  //not checking for noise, or a user filter
             {$IfDef RecordMakeGrid} WriteLineToDebugFile('One layer, layer=' + IntToStr(Layer) + '   points=' + IntToStr(LasFiles[Layer].TotalCloudPts) + ' filter=' + TrueOrFalse(not NoFilter)); {$EndIf}
              for k := 0 to Pred(LasFiles[Layer].LAS_fnames.Count) do begin
                 ThreadTimers.OverallGauge9.Progress := round(100 * k / LasFiles[Layer].LAS_fnames.Count);
                 fName := LasFiles[Layer].LAS_fnames.Strings[k];
                 LasData := Las_Lidar.tLAS_data.Create(fName);
                  {$If Defined(RecordMakeGridFull) or Defined(RecordGridFileNames)} WriteLineToDebugFile('One layer using=' + fName); {$EndIf}
                  LasData.PrepDataRead;
                  for i := 0 to LasData.ReadsRequired do begin
                      if (i mod 5 = 0) then ThreadTimers.Gauge1.Progress := round(100 * i/LasData.ReadsRequired);
                      LasData.ReadPoints(RecsRead);
                      for j := 1 to RecsRead do begin
                          if NoFilter or LasData.MeetsFilter(j) then begin
                             if FindDEMGridCellOfShot(LasData,CheckDEM,J,xgrid,ygrid) then begin
                                LASclass := LasData.LASClassification(j);
                                zShot := Lasdata.ExpandLAS_Z(j);

                                if (PCGridMaker in [pcgmMeanStd,pcgmMeanFirst]) then begin
                                   if (PCGridMaker in [pcgmMeanStd]) or LasData.FirstReturn(j) then begin
                                      DEMGlb[NewDensity].IncrementGridValue(xgrid,ygrid);
                                      if (not DEMGlb[MeanReturnHeightDEM].GetElevMetersOnGrid(xgrid,ygrid,zGrid)) then ZGrid := 0;
                                      DEMGlb[MeanReturnHeightDEM].SetGridElevation(xgrid,ygrid,zShot+Zgrid);
                                   end;
                                end
                                else begin
                                   if (NewDensity <> 0) then DEMGlb[NewDensity].IncrementGridValue(xgrid,ygrid);
                                   if (NewFirstReturnDEM <> 0) and LasData.FirstReturn(j) then DEMGlb[NewFirstReturnDEM].IncrementGridValue(xgrid,ygrid);
                                   if (SingleReturnDEM <> 0) and (LasData.ReturnsInPulse(j) = 1) then DEMGlb[SingleReturnDEM].IncrementGridValue(xgrid,ygrid);
                                   if (OverlapDEM <> 0) and LASData.OverlapPoint(j) then DEMGlb[OverlapDEM].IncrementGridValue(xgrid,ygrid);
                                   if (SecondReturnDEM <> 0) and (LASData.ReturnNumber(j) = 2) then DEMGlb[SecondReturnDEM].IncrementGridValue(xgrid,ygrid);
                                   if (AirReturnDEM <> 0) and LasData.AirReturn(j) then DEMGlb[AirReturnDEM].IncrementGridValue(xgrid,ygrid);

                                   if (MinScanAngleDEM <> 0) and (MaxScanAngleDEM <> 0) then begin
                                     zShot := LasData.GetScanAngle(j);
                                     if DEMGlb[MinScanAngleDEM].GetElevMeters(xgrid,ygrid,zGrid) and (zshot < zGrid) then DEMGlb[MinScanAngleDEM].SetGridElevation(xgrid,ygrid,zShot);
                                     if DEMGlb[MaxScanAngleDEM].GetElevMeters(xgrid,ygrid,zGrid) and (zshot > zGrid) then DEMGlb[MaxScanAngleDEM].SetGridElevation(xgrid,ygrid,zShot);
                                   end;

                                   if (PCGridMaker in [pcgmMaxIntensity,pcgmMinIntensity,pcgmAllIntensity]) then begin
                                      Intensity := LasData.GetShotMeasuredIntensity(j);
                                      if (not DEMGlb[MaxIntensityDEM].GetElevMeters(xgrid,ygrid,zGrid)) or (Intensity > zGrid) then DEMGlb[MaxIntensityDEM].SetGridElevation(xgrid,ygrid,Intensity);
                                      if (MinIntensityDEM <> 0) then if (not DEMGlb[MinIntensityDEM].GetElevMeters(xgrid,ygrid,zGrid) or (Intensity < zGrid)) then DEMGlb[MinIntensityDEM].SetGridElevation(xgrid,ygrid,Intensity);
                                      if (MeanIntensityDEM <> 0) then begin
                                         if (not DEMGlb[MeanIntensityDEM].GetElevMeters(xgrid,ygrid,zGrid)) then zgrid := 0;
                                         DEMGlb[MeanIntensityDEM].SetGridElevation(xgrid,ygrid,Intensity + zGrid);
                                      end;
                                   end;
                                    if (PCGridMaker in [pcgmGrndPtDTM]) then begin
                                        if LasData.GroundReturn(j) then begin
                                           if (NewGroundDensity <> 0) then DEMGlb[NewGroundDensity].IncrementGridValue(xgrid,ygrid);
                                           if (NewGroundMean <> 0) then begin
                                              if (not DEMGlb[NewGroundMean].GetElevMetersOnGrid(xgrid,ygrid,zGrid)) then ZGrid := 0;
                                              DEMGlb[NewGroundMean].SetGridElevation(xgrid,ygrid,zShot+Zgrid);
                                           end;
                                           if (NewGroundNearest <> 0) then begin
                                              Dist := sqr(xf-xgrid) + sqr(yf-ygrid);
                                              if DEMGlb[NewDistance].GetElevMetersOnGrid(xgrid,ygrid,zGrid) and (Dist < zgrid) then begin
                                                 DEMGlb[NewGroundNearest].SetGridElevation(xgrid,ygrid,zShot);
                                                 DEMGlb[NewDistance].SetGridElevation(xgrid,ygrid,Dist);
                                              end;
                                           end;
                                           if (NewGroundMax <> 0) then begin
                                              if (not DEMGlb[NewGroundMax].GetElevMetersOnGrid(xgrid,ygrid,zGrid)) or (zShot > zGrid) then DEMGlb[NewGroundMax].SetGridElevation(xgrid,ygrid,zShot);
                                           end;
                                           if (NewGroundMin <> 0) then begin
                                              if (not DEMGlb[NewGroundMin].GetElevMetersOnGrid(xgrid,ygrid,zGrid)) or (zShot < zGrid) then DEMGlb[NewGroundMin].SetGridElevation(xgrid,ygrid,zShot);
                                           end;
                                        end;
                                    end
                                    else if (PCGridMaker in [pcgmCeilFloor]) then begin
                                        zShot := Lasdata.ExpandLAS_Z(j);
                                        OK := true;
                                        if (MDDef.LasElevChecks) and (BaseMap.MapDraw.DEMonMap <> 0) and DEMGlb[BaseMap.MapDraw.DEMonMap].GetElevMetersOnGrid(xgrid,ygrid,zGrid) then begin
                                           if (zshot < MDDef.LowValidZinPointCloud) or (zshot > MDDef.MaxValidZinPointCloud) or ((zshot - zGrid) > MDDef.MaxValidHAGinPointCloud) then OK := false;
                                        end;
                                        if OK then begin
                                           if (DSMDEM <> 0) then begin
                                              if (not DEMGlb[DSMDEM].GetElevMetersOnGrid(xgrid,ygrid,zGrid)) or (zshot > zGrid) then DEMGlb[DSMDEM].SetGridElevation(xgrid,ygrid,zShot);
                                           end;
                                           if (NVSDEM <> 0) then begin
                                              if (not DEMGlb[NVSDEM].GetElevMetersOnGrid(xgrid,ygrid,zGrid)) or (zshot < zGrid) then DEMGlb[NVSDEM].SetGridElevation(xgrid,ygrid,zShot);
                                           end;
                                        end;
                                   end
                                end{if};
                             end;
                          end;
                      end {for j};
                  end {for i};
                 FreeAndNil(LasData);
              end {for k};
           end;


           procedure CheckMap(DEM : integer; FillHoles : boolean = true; MapType : tMapType = mtElevRainbow);
           var
              fName : PathStr;
           begin
              if ValidDEM(DEM) then begin
                 if FillHoles and MDDef.PCAutoFillHoles then begin
                    DEMGlb[DEM].InterpolateAcrossHoles(false);
                 end;
                 {$IfDef RecordMakeGrid} WriteLineToDebugFile('CheckMap, DEM=' + IntToStr(DEM) + '  ' + DEMGlb[DEM].AreaName); {$EndIf}
                 if (AutoSaveDir <> '') then begin
                    fName := AutoSaveDir + DEMGlb[DEM].AreaName + '.dem';
                    {$IfDef RecordMakeGrid} WriteLineToDebugFile('Save DEM, ' + fName); {$EndIf}
                    DEMGlb[DEM].WriteNewFormatDEM(fName);
                 end;
                 DEMGlb[DEM].SetUpMap(true,MapType);
                 {$IfDef RecordMakeGrid} WriteLineToDebugFile(DEMGlb[DEM].zRange + ' geo range=' + sfBoundBoxToString( DEMGlb[DEM].SelectionMap.MapDraw.MapCorners.BoundBoxGeo,6)); {$EndIf}
                 if (PCGridMaker <> pcgmBlank) and (DEMGlb[DEM].DEMheader.MaxElev = 0) then begin
                    {$IfDef RecordMakeGrid} WriteLineToDebugFile('Closing, empty DEM=' + IntToStr(DEM) + ' ' + DEMGlb[DEM].AreaName); {$EndIf}
                    CloseSingleDEM(DEM);
                 end;
              end;
           end;

              procedure FinishMeanStdDevProcessing(MeanDEM,STDDEM,CountDEM : integer);
              var
                 Col,Row : integer;
                 std,sumsq,sum,z2 : float32;
              begin
                for Col := 0 to pred(DEMGlb[CountDEM].DEMheader.NumCol) do begin
                   for Row := 0 to pred(DEMGlb[CountDEM].DEMheader.NumRow) do begin
                      if DEMGlb[CountDEM].GetElevMeters(Col,Row,z2) then begin
                         if (z2 < 0.01) then begin
                            DEMGlb[CountDEM].SetGridMissing(Col,Row);
                            DEMGlb[MeanDEM].SetGridMissing(Col,Row);
                            if (StdDEM <> 0) then DEMGlb[StdDEM].SetGridMissing(Col,Row);
                         end
                         else begin
                            DEMGlb[MeanDEM].GetElevMeters(Col,Row,sum);
                            DEMGlb[MeanDEM].SetGridElevation(Col,Row,sum/z2);
                            if (StdDEM <> 0) then begin
                               if z2 > 1.001 then begin
                                  DEMGlb[StdDEM].GetElevMeters(Col,Row,sumsq);
                                  std := sqrt( (z2 * sumsq - sqr(sum)) / z2 / (z2 - 1));
                                  DEMGlb[StdDEM].SetGridElevation(Col,Row,std);
                               end
                               else DEMGlb[StdDEM].SetGridMissing(Col,Row);
                            end;
                         end;
                      end;
                   end;
                 end;
              end;


        function DTMOptionName : shortstring;
        begin
           case MDDef.DTMoption of
              dtmMax : Result := 'max';
              dtmMean : Result := 'mean';
              dtmMin : Result := 'min';
              dtmNearest : Result := 'nearest';
           end;
        end;

        procedure MakeMapDisplays;
        var
           i : integer;
        begin
           ShowHourglassCursor;
           if (PCGridMaker = pcgmVegVox) then begin
              for i := 1 to MDDef.VegDensityHeights do begin
                 DEMGlb[VegDensity[i]].MarkInRangeMissing(0,0,Count);
                 fName := OutDir + 'las_density_'+ IntToStr(i) + '.dem';
                 DEMGlb[VegDensity[i]].WriteNewFormatDEM(fName);
                 CloseSingleDEM(VegDensity[i]);
              end;
           end
           else if (PCGridMaker = pcgmDensityVox) then begin
              for i := 1 to MDDef.VegDensityHeights do begin
                 DEMGlb[VegDensity[i]].MarkInRangeMissing(0,0,Count);
                 fName := OutDir + 'las_density_'+ IntToStr(i) + '_' + IntToStr(round(MinAreaZ + Pred(i))) + '.dem';
                 DEMGlb[VegDensity[i]].WriteNewFormatDEM(fName);
                 CloseSingleDEM(VegDensity[i]);
              end;
           end
           else if (PCGridMaker in [pcgmGrndPtDTM]) then begin
               if (NewGroundMean <> 0) then begin
                  FinishMeanStdDevProcessing(NewGroundMean,0,NewGroundDensity);
                  if ShowMeanDensityGrid then CheckMap(NewGroundDensity)
                  else CloseSingleDEM(NewGroundDensity);
                  Result := NewGroundMean;
               end;
               CloseSingleDEM(NewDistance);
           end
           else if (PCGridMaker in [pcgmMeanStd,pcgmMeanFirst]) then begin
              FinishMeanStdDevProcessing(MeanReturnHeightDEM,MeanReturnStdDEM,NewDensity);
           end
           else if (PCGridMaker in [pcgmAllIntensity]) then begin
               FinishMeanStdDevProcessing(MeanIntensityDEM,0,NewDensity);
           end
           else if (PCGridMaker in [pcgmScanAngle]) then begin
               //we start with way out of range elevations, and now mark missing the cells with no returns
               if (MinScanAngleDEM <> 0) then DEMGlb[MinScanAngleDEM].MarkInRangeMissing(9998,10000,Count);
               if (MaxScanAngleDEM <> 0) then DEMGlb[MaxScanAngleDEM].MarkInRangeMissing(-10000,-9998,Count);
           end
           else if (PCGridMaker = pcgmClassification) then begin
              if (ClassDEM <> 0) then begin
                 CheckMap(ClassDEM,false,mtLASclass);
                 if MDDef.PCAutoFillHoles then begin
                    ModeFilterDEM(ClassDEM,1,true);
                    CloseSingleDEM(ClassDEM);
                    CheckDEM := LastDEMLoaded;
                 end;
              end;
           end;

           if (NewRGBGrid <> 0) then begin
              CheckMap(NewRGBGrid,false,mtRGBimagery);
              if MDDef.PCAutoFillHoles then begin
                 DEMGlb[NewRGBGrid].RGBFilterDEM(1,true);
                 CloseSingleDEM(NewRGBGrid);
                 NewRGBGrid := LastDEMLoaded;
              end;
           end;
           CheckMap(AirReturnDEM,false);
           CheckMap(BlankDEM,false);
           CheckMap(DSMDEM,true,mtIHSReflect);
           CheckMap(MaxIntensityDEM,true,mtElevGray);
           CheckMap(MinIntensityDEM,true,mtElevGray);
           CheckMap(MaxScanAngleDEM);
           CheckMap(MeanIntensityDEM,true,mtElevGray);
           CheckMap(MeanReturnHeightDEM,true,mtIHSReflect);
           CheckMap(MeanReturnStdDEM);
           CheckMap(MinIntensityDEM,true,mtElevGray);
           CheckMap(MinScanAngleDEM);
           CheckMap(NewBuildingDEM,false);
           CheckMap(NewDensity,false);

           CheckMap(NewFirstReturnDEM,false);
           CheckMap(NewGroundDensity,false);
           CheckMap(NewGroundMax,true,mtIHSReflect);
           CheckMap(NewGroundMean,true,mtIHSReflect);
           CheckMap(NewGroundMin,true,mtIHSReflect);
           CheckMap(NewGroundNearest,true,mtIHSReflect);

           CheckMap(NewOtherDEM,false);
           CheckMap(NewPointIDDEM,false);
           CheckMap(NewUnclassDEM,false);
           CheckMap(NewUserDataDEM,false);
           CheckMap(NewVegDEM,false);
           CheckMap(NVSDEM,true,mtIHSReflect);
           CheckMap(OverlapDEM,false);
           CheckMap(SecondReturnDEM,false);
           CheckMap(SingleReturnDEM,false);
        end;


    begin
       {$If Defined(RecordMakeGrid) or Defined(TrackPointCloud)} WriteLineToDebugFile(''); WriteLineToDebugFile('Tpt_cloud_opts_fm.BitBtn9Click (Make grid) in, ' + IntToStr(ord(PCGridMaker))); {$EndIf}

       if (PCGridMaker in [pcgmAboveBelow,pcgmLowXYZ, pcgmGroundLowXYZ]) then begin
          MessageToContinue('Option currentely disabled');
          exit;
       end;

       ShowHourglassCursor;
       //BaseMap.MapDraw.MapCorners.BoundBoxUTM := BaseMap.MapDraw.GetBoundBoxUTM;
       TempDEM := BaseMap.MakeTempGrid;
      {$IfDef RecordMakeGrid} WriteLineToDebugFile('CreateGridToMatchMap done, TempDEM=' + IntToStr(TempDEM) + ' ' + DEMGlb[TempDEM].GridDefinition + ' ' + DEMGlb[TempDEM].DEMSizeString + ' ' + DEMGlb[TempDEM].ColsRowsString); {$EndIf}
      AirReturnDEM := 0;
      BlankDEM := 0;
      ClassDEM := 0;
      DSMDEM := 0;
      MaxIntensityDEM := 0;
      MaxScanAngleDEM := 0;
      MeanIntensityDEM := 0;
      MeanReturnHeightDEM := 0;
      MeanReturnStdDEM := 0;
      MinIntensityDEM := 0;
      MinScanAngleDEM := 0;
      NewBuildingDEM := 0;
      NewDensity := 0;
      NewDistance := 0;
      NewFirstReturnDEM := 0;
      NewGroundDensity := 0;
      NewGroundMax := 0;
      NewGroundMean := 0;
      NewGroundMin := 0;
      NewGroundNearest := 0;
      NewGroundXYZ := 0;
      NewGroundXYZDEM := 0;
      NewLowXYZ := 0;
      NewLowXYZDEM := 0;
      NewOtherDEM := 0;
      NewPointIDdem := 0;
      NewRGBGrid := 0;
      NewUnclassDEM := 0;
      NewUserDataDEM := 0;
      NewVegDEM := 0;
      NVSDEM := 0;
      OverlapDEM := 0;
      SecondReturnDEM := 0;
      SingleReturnDEM := 0;

       CheckEditString(Edit2.Text,zcrit);

       CreateNewGrids;

       CloseSingleDEM(TempDEM);

        if (PCGridMaker <> pcgmBlank) then begin
           StartThreadTimers('Point cloud',1,true);
           //Ext := ExtractFileExt(LasFiles[1].LAS_fnames.Strings[0]);
           for i := 1 to MaxClouds do if UsePC[i] and (LasFiles[i] <> Nil) then OneLasLayer(i);
           EndThreadTimers;
        end;

       {$If Defined(RecordMakeGrid) or Defined(TrackPointCloud)} WriteLineToDebugFile('Grid creation over'); {$EndIf}
       MakeMapDisplays;
       Result := CheckDEM;
       {$If Defined(RecordMakeGrid) or Defined(TrackPointCloud)} WriteLineToDebugFile('MakeGridFromLidarCloud out'); {$EndIf}
    end;


begin
   if (MDDef.LidarGridProjection = UTMBasedDEM) or (PCGridMaker in [pcgmLowXYZ, pcgmGroundLowXYZ]) then begin
       if (Edit19.Text = '') then begin
          PickUTMZone(MDdef.DefaultUTMZone);
          Edit19.Text := IntToStr(MDdef.DefaultUTMZone);
       end;
    end;
   Result := MakeGridFromLidarCloud(TheCloudName,PCGridMaker,BaseMap,UsePC,LasFiles);
end;



function Tpt_cloud_opts_fm.CreateGridOrDBfromStats(Option : tdbpcPointCloud; Percentile : float64 = 50; SaveName : PathStr = ''; OpenMap : boolean = true) : integer;
label
   BreakOut;
const
   MaxBlockSize = 255;
   MaxPossPtsInCell = 128000;
type
   tCellValues = array[0..MaxPossPtsInCell] of float32;
   tBlockArray = array[0..MaxBlockSize,0..MaxBlockSize] of ^tCellValues;
   tCellCount = array[0..MaxBlockSize,0..MaxBlockSize] of word;
var
   pBlockArray : ^tBlockArray;
   pCellCount : ^tCellCount;
   Findings : tStringList;
   MomentVar : tMomentVar;
   TooManyPoints : int64;
   DEMLimits : tGridLimits;
   WarningIssued : boolean;

           procedure OneLasLayer(aLabel : shortstring; Layer,Col,Row : integer);
           label
              RestartThinned;
           var
              NumNoiseOverlap,OutsideBlock,NumWrongReturn,OutsideNewGrid,GoodInBlock : int64;
              TilesUsed,ThinFactor,xf,yf,ff,
              buf,k,i,j,RecsRead,xgrid,ygrid,xg,yg,ColHi,RowHi : integer;
              fName : PathStr;
              NeedTile{,NoFilter} : boolean;
              LasData : Las_Lidar.tLAS_data;
              bb : sfBoundBox;
              Proceed{,p1,p2} : boolean;
              xutm,yutm : float64;
           begin
              {$IfDef RecordNewGrids} WriteLineToDebugFile(aLabel + '  layer=' + IntToStr(Layer) + ' col=' + IntToStr(Col) + ' row=' + IntToStr(Row) ); {$EndIf}
              {$IfDef ShowNewGridsMap} BaseMap.DoFastMapRedraw; {$EndIf}
              ColHi := Col + MDDef.BlockSize;
              RowHi := Row + MDDef.BlockSize;
              ThinFactor := 1;

              InitializeBoundBox(bb);
              ff := 10;
              buf := 0;
              for xg := 0 to ff do begin
                 xf := Col + xg * (ColHi - Col) div ff;
                 for yg := 0 to ff do begin
                    yf := Row + yg * (RowHi - Row) div ff;
                    if DEMGlb[Result].GridInDataSetInteger(xf,yf) then begin
                       DEMGlb[Result].DEMGridToUTM(xf,yf,xutm,yutm);
                       {$IfDef ShowNewGridsMap} BaseMap.MapDraw.UTMtoScreen(xutm,yutm,xp,yp); ScreenSymbol(BaseMap.Image1.Canvas,xp,yp,FilledBox,3,claRed); {$EndIf}
                       CompareValueToExtremes(yutm,bb.ymin,bb.ymax);
                       CompareValueToExtremes(xutm,bb.xmin,bb.xmax);
                    end;
                 end;
              end;
              {$IfDef RecordNewGrids} WriteLineToDebugFile('UTM bounding box: ' + sfBoundBoxToString(bb) ); {$EndIf}

             RestartThinned:;
              NumNoiseOverlap := 0;
              NumWrongReturn := 0;
              OutsideBlock := 0;
              OutsideNewGrid := 0;
              GoodInBlock := 0;
              TilesUsed := 0;
              for xg := 0 to MDDef.BlockSize do
                 for yg := 0 to MDDef.BlockSize do
                    pCellCount^[xg,yg] := 0;

              for k := 0 to Pred(LasFiles[Layer].LAS_fnames.Count) do begin
                 ThreadTimers.Gauge1.Progress := round(100 * k/LasFiles[Layer].LAS_fnames.Count);
                 fName := LasFiles[Layer].LAS_fnames.Strings[k];
                 LasData := Las_Lidar.tLAS_data.Create(fName);
                 NeedTile := LasData.InBoundBoxUTM(bb) or AtLeastPartOfBoxInAnotherBox(bb,LasData.LAS_UTM_Box);
                 {$IfDef ShowNewGridsMap}
                    if NeedTile then BaseMap.Image1.Canvas.Pen.Color := clLime
                    else BaseMap.Image1.Canvas.Pen.Color := clRed;
                    BaseMap.Image1.Canvas.Pen.Width := 3;
                    BaseMap.MapDraw.UTMtoScreen(LasData.LAS_UTM_Box.xmin,LasData.LAS_UTM_Box.ymin,xp,yp);
                    BaseMap.Image1.Canvas.MoveTo(xp,yp);
                    BaseMap.MapDraw.UTMtoScreen(LasData.LAS_UTM_Box.xmin,LasData.LAS_UTM_Box.ymax,xp,yp);
                    BaseMap.Image1.Canvas.LineTo(xp,yp);
                    BaseMap.MapDraw.UTMtoScreen(LasData.LAS_UTM_Box.xmax,LasData.LAS_UTM_Box.ymax,xp,yp);
                    BaseMap.Image1.Canvas.LineTo(xp,yp);
                    BaseMap.MapDraw.UTMtoScreen(LasData.LAS_UTM_Box.xmax,LasData.LAS_UTM_Box.ymin,xp,yp);
                    BaseMap.Image1.Canvas.LineTo(xp,yp);
                    BaseMap.MapDraw.UTMtoScreen(LasData.LAS_UTM_Box.xmin,LasData.LAS_UTM_Box.ymin,xp,yp);
                    BaseMap.Image1.Canvas.LineTo(xp,yp);
                 {$EndIf}

                 if NeedTile then begin
                     LasData.PrepDataRead;
                     inc(TilesUsed);
                     if (Option = dbpcMidCloud) then Proceed := true;
                     for i := 0 to LasData.ReadsRequired do begin
                         LasData.ReadPoints(RecsRead);
                         j := 1;
                         while j <= RecsRead do begin
                            if (MDDef.UseOverlap or (not LasData.OverlapPoint(j)) ) and (MDDef.UseNoise or (not LasData.NoiseReturn(j))) then begin
                                if (Option = dbpcDSM) then Proceed := LasData.FirstReturn(j)
                                else if (Option = dbpcDTM) then Proceed := LasData.GroundReturn(j)
                                else if (Option = dbpcDataBase) then Proceed := LasData.MeetsFilter(j);
                                if Proceed then begin
                                   if FindDEMGridCellOfShot(LasData,Result,J,xgrid,ygrid) then begin
                                      if (xgrid >= Col) and (xgrid <= ColHi) and (ygrid >= Row) and (ygrid <= RowHi) then begin
                                         xg := xgrid - Col;
                                         yg := ygrid - Row;
                                         if pCellCount^[xg,yg] < MDDef.MaxPtsInCell then begin
                                            pBlockArray^[xg,yg]^[pCellCount^[xg,yg]] := LasData.ExpandLAS_Z(j);
                                            inc(pCellCount^[xg,yg]);
                                            inc(GoodInBlock);
                                         end
                                         else begin
                                            if MDDef.AutoThinByBlock then begin
                                               inc(ThinFactor);
                                               {$IfDef RecordNewGrids} WriteLineToDebugFile('Block thinning increased=' + IntToStr(ThinFactor)); {$EndIf}
                                               goto RestartThinned;
                                            end
                                            else inc(TooManyPoints);
                                         end;
                                      end
                                      else begin
                                         inc(OutsideBlock);
                                      end;
                                   end
                                   else begin
                                      inc(OutsideNewGrid);
                                   end;
                                end
                                else begin
                                   inc(NumWrongReturn);
                                end;
                            end
                            else begin
                               inc(NumNoiseOverlap);
                            end;
                            inc(j,ThinFactor);
                         end {while};
                     end {for i};
                     LasData.FreeLASRecordMemory;
                 end;
                 LasData.Destroy;
              end {for k};
             {$IfDef RecordBlocks} WriteLineToDebugFile('tiles read=' + IntToStr(TilesUsed) + '  noise, overlap=' + IntToStr(NumNoiseOverlap) + '  outside block=' + IntToStr(OutsideBlock) + '  wrong return=' + IntToStr(NumWrongReturn) +
                        '  outside new grid=' +  IntToStr(OutsideNewGrid) + '  in the block=' + IntToStr(GoodInBlock)); {$EndIf}
              {$IfDef ShowNewGridsMap} MessageToContinue('see green missing'); {$EndIf}
           end;

var
   i,x,y,Col,Row,theRound,Rounds,Base,PtDensity : integer;
   Lat,Long,{fr1,fr2,fr3,CellDZ,z,}PC_99,PC_95,PC_5,PC_1,PC_995,PC_05 : float64;
   TStr : shortstring;

begin
   Result := BaseMap.MakeTempGrid;
   if (Result = 0) then exit;
   PtDensity := 0;
   if CheckBox33.Checked then PtDensity := BaseMap.MakeTempGrid;
   DEMLimits := DEMGlb[Result].FullDEMGridLimits;

   {$IfDef RecordNewGrids} WriteLineToDebugFile('CreateDataBaseOfPixelCloudStats, DEM=' + DEMGlb[Result].AreaName + '  ' + DEMGlb[Result].KeyDEMParams(true)); {$EndIf}

   {$IfDef RecordNewGrids} WriteLineToDebugFile('UTM limits lidar: ' +  sfBoundBoxToString(LasFiles[1].UTMBBox,1) + '  Zone:' + IntToStr(LasFiles[1].UTMZone)); {$EndIf}
   {$IfDef RecordNewGrids} WriteLineToDebugFile('Geo limits lidar: ' +  sfBoundBoxToString(LasFiles[1].GeoBBox,5)); {$EndIf}

   {$IfDef RecordNewGrids} WriteLineToDebugFile('Proj limits DEM: ' +  sfBoundBoxToString(DEMGlb[Result].DEMBoundBoxProjected,1)); {$EndIf}
   {$IfDef RecordNewGrids} WriteLineToDebugFile('Geo limits DEM: ' +  sfBoundBoxToString(DEMGlb[Result].DEMBoundBoxGeo,5)); {$EndIf}

   //NoFilter := NoFilterWanted;
   if Option = dbpcDataBase then begin
      Findings := tStringList.Create;
      Findings.Add('LAT,LONG,CLOUD_PTS,CLOUD_MAX,DOWN_1,CLOUD_995,CLOUD_99,CLOUD_95,CLOUD_MEAN,CLOUD_MED,CLOUD_5,CLOUD_1,CLOUD_0_5,UP_1,CLOUD_MIN,CLOUD_HT,HT_0_5PC,HT_1PC,HT5PC');
   end;
   {$IfDef RecordNewGrids} WriteLineToDebugFile(Label28.Caption); {$EndIf}

   StartThreadTimers('Grid ' + StatusBarShortMess,1,true);
   ThreadTimers.StatusBar1.Panels[1].Text := StatusBarShortMess;
   new(pBlockArray);
   if MDDef.BlockSize > 255 then MDDef.BlockSize := 255;

   for x := 0 to MDDef.BlockSize do
      for y := 0 to MDDef.BlockSize do
         GetMem(pBlockArray^[x,y],MDDef.MaxPtsInCell * SizeOf(float32) );
   new(pCellCount);
   TooManyPoints := 0;
   Rounds := 0;
   WarningIssued := false;
   {$IfDef RecordNewGrids} WriteLineToDebugFile('Memory allocated'); {$EndIf}

   //find out how many rounds (passes through the data) will be needed
   Col := DEMLimits.xgridlow;
   while Col <= DEMLimits.xgridhigh do begin
      Row := DEMLimits.ygridlow;
      while Row <= DEMLimits.ygridhigh do begin
         inc(rounds);
         inc(Row,succ(MDDef.BlockSize));
      end;
      inc(Col,succ(MDDef.BlockSize));
   end;

   theRound := 0;
   ThreadTimers.OverallGauge9.Visible := true;
   Col := DEMLimits.xgridlow;
   while Col <= DEMLimits.xgridhigh do begin
      Row := DEMLimits.ygridlow;
      while Row <= DEMLimits.ygridhigh do begin
         ThreadTimers.OverallGauge9.Progress := round(100 * theround/rounds);
         inc(theRound);
         {$IfDef RecordNewGrids} TStr := 'start: ' + IntToStr(theRound) + '/' + IntToStr(rounds); {$EndIf}

         for i := 1 to MaxClouds do if UsePC[i] and (LasFiles[i] <> Nil) then begin
            OneLasLayer(TStr,i,Col,Row);
         end;

         for x := 0 to MDDef.BlockSize do begin
            for y := 0 to MDDef.BlockSize do begin
               if (pCellCount^[x,y] >= MDDef.MinPtsRequiredPercentile) then begin
                  MomentVar.NPts := pCellCount^[x,y];
                  Moment(pBlockArray^[x,y]^,MomentVar,msAll);

                  if (Option in [dbpcDSM,dbpcMidCloud,dbpcDTM]) then begin
                     DEMGlb[Result].SetGridElevation(Col+x,Row+y,pBlockArray^[x,y]^[round(0.01 * Percentile * pred(MomentVar.Npts))]);
                  end;

                  if (Option = dbpcDataBase) then begin
                     DEMGlb[Result].DEMGridToLatLongDegree(Col + x, Row + y,Lat,Long);
                     PC_995 := pBlockArray^[x,y]^[round(0.995 * pred(MomentVar.Npts))];
                     PC_99 := pBlockArray^[x,y]^[round(0.99 * pred(MomentVar.Npts))];
                     PC_95 := pBlockArray^[x,y]^[round(0.95 * pred(MomentVar.Npts))];
                     PC_5 := pBlockArray^[x,y]^[round(0.05 * pred(MomentVar.Npts))];
                     PC_1 := pBlockArray^[x,y]^[round(0.01 * pred(MomentVar.Npts))];
                     PC_05 := pBlockArray^[x,y]^[round(0.005 * pred(MomentVar.Npts))];
                     Findings.Add(RealToString(Lat,-12,-6) + ',' +  RealToString(Long,-12,-6) + ',' + IntToStr(MomentVar.NPts) + ',' + RealToString(MomentVar.MaxZ,-8,-2) + ',' +
                         RealToString(pBlockArray^[x,y]^[pred(MomentVar.Npts)],-8,-2) + ',' + RealToString(PC_995,-8,-2) + ',' + RealToString(PC_99,-8,-2) + ',' +  RealToString(PC_95,-8,-2) + ',' +
                         RealToString(MomentVar.mean,-8,-2) + ',' +  RealToString(MomentVar.Median,-8,-2) + ',' + RealToString(PC_5,-8,-2) + ',' +  RealToString(PC_1,-8,-2) + ',' +
                         RealToString(PC_05,-8,-2) + ',' + RealToString(pBlockArray^[x,y]^[1],-8,-2) + ',' + RealToString(MomentVar.MinZ,-8,-2) + ',' +
                         RealToString(MomentVar.MaxZ-MomentVar.MinZ,-8,-2) + ',' + RealToString(PC_995-PC_05,-8,-2)+ ',' + RealToString(PC_99-PC_1,-8,-2) + ',' + RealToString(PC_95-PC_5,-8,-2));
                  end;
               end;
               if (Option in [dbpcDSM,dbpcMidCloud,dbpcDTM]) then begin
                  if (PtDensity <> 0) then DEMGlb[PtDensity].SetGridElevation(Col+x,Row+y,pCellCount^[x,y]);
               end;
            end;
         end;
         if (TooManyPoints > 0) and (Not WarningIssued) then begin
            WarningIssued := true;
            if not AnswerIsYes('There were ' + IntToStr(TooManyPoints) + ' cells with more than ' + IntToStr(MDDef.MaxPtsInCell) + ' points; Continue') then Goto BreakOut;
         end;
         inc(Row,succ(MDDef.BlockSize));
      end;
      inc(Col,succ(MDDef.BlockSize));
   end;
   {$IfDef RecordNewGrids} WriteLineToDebugFile('Passes complete'); {$EndIf}
   if (TooManyPoints > 0) then MessageToContinue('There were ' + IntToStr(TooManyPoints) + ' cells with more than ' + IntToStr(MDDef.MaxPtsInCell) + ' points');

   BreakOut:;
   EndThreadTimers;

   if (Option = dbpcDataBase) then begin
      if (SaveName = '') then SaveName := Petmar.NextFileNumber(MDTempDir,'point_cloud_','.csv');
      DEMGlb[Result].SelectionMap.DisplayAndPurgeStringListDB(Findings,SaveName);
      CloseSingleDEM(Base);
   end;
   if Option in [dbpcDSM,dbpcMidCloud,dbpcDTM] then begin
      {$IfDef RecordNewGrids} WriteLineToDebugFile('Set up DEM call'); {$EndIf}
       if (SaveName <> '') then begin
          DEMGlb[Result].AreaName := ExtractFileNameNoExt(SaveName);
          DEMGlb[Result].WriteNewFormatDEM(SaveName);
       end
       else begin
          if Option in [dbpcDSM] then DEMGlb[Result].AreaName := 'DSM';
          if Option in [dbpcMidCloud] then DEMGlb[Result].AreaName := 'Mid_Cloud';
          if Option in [dbpcDTM] then DEMGlb[Result].AreaName := 'DTM';
          DEMGlb[Result].AreaName := DEMGlb[Result].AreaName + '_' + RealToString(Percentile,-8,-2) + '_' + TheCloudName;
       end;
       if OpenMap then begin
          DEMGlb[Result].SetUpMap(true,mtIHSReflect);
          if (PtDensity <> 0) then begin
             DEMGlb[PtDensity].AreaName := 'Point_density';
             DEMGlb[PtDensity].SetUpMap(true,mtElevSpectrum);

            //Outline the Blocks
             Col := DEMLimits.xgridlow;
             while Col <= DEMLimits.xgridhigh do begin
                 Row := DEMLimits.ygridlow;
                 while Row <= DEMLimits.ygridhigh do begin
                    DEMGlb[PtDensity].SelectionMap.Image1.Canvas.Pen.Width := 3;
                    DEMGlb[PtDensity].SelectionMap.MapDraw.DataGridToScreen(Col,Row,x,y);
                    DEMGlb[PtDensity].SelectionMap.Image1.Canvas.MoveTo(x,y);
                    DEMGlb[PtDensity].SelectionMap.MapDraw.DataGridToScreen(Col,Row+MDDef.BlockSize,x,y);
                    DEMGlb[PtDensity].SelectionMap.Image1.Canvas.LineTo(x,y);
                    DEMGlb[PtDensity].SelectionMap.MapDraw.DataGridToScreen(Col+MDDef.BlockSize,Row+MDDef.BlockSize,x,y);
                    DEMGlb[PtDensity].SelectionMap.Image1.Canvas.LineTo(x,y);
                    DEMGlb[PtDensity].SelectionMap.MapDraw.DataGridToScreen(Col+MDDef.BlockSize,Row,x,y);
                    DEMGlb[PtDensity].SelectionMap.Image1.Canvas.LineTo(x,y);
                    DEMGlb[PtDensity].SelectionMap.MapDraw.DataGridToScreen(Col,Row,x,y);
                    DEMGlb[PtDensity].SelectionMap.Image1.Canvas.LineTo(x,y);
                    inc(Row,succ(MDDef.BlockSize));
                 end;
                 inc(Col,succ(MDDef.BlockSize));
             end;
          end;
       end
       else begin
          CloseSingleDEM(Result);
          CloseSingleDEM(PtDensity);
          Result := 0;
       end;
   end;
   {$IfDef RecordNewGrids} WriteLineToDebugFile('Maps/DB created'); {$EndIf}

   for x := 0 to MDDef.BlockSize do
      for y := 0 to MDDef.BlockSize do
         FreeMem(pBlockArray^[x,y],MDDef.MaxPtsInCell * SizeOf(float32)   );
   Dispose(pCellCount);

   {$IfDef RecordNewGrids} WriteLineToDebugFile('CreateDataBaseOfPixelCloudStats out'); {$EndIf}
end;


procedure CloseOverlayPointClouds;
begin
    pt_cloud_opts_fm := nil;
end;


procedure Tpt_cloud_opts_fm.BitBtn10Click(Sender: TObject);
begin
   RunProcess(wpPDALsmrf);
end;


procedure Tpt_cloud_opts_fm.BitBtn11Click(Sender: TObject);
var
   fName : PathStr;
   j : integer;
begin
   for j := 1 to MaxClouds do if UsePC[j] and (LasFiles[j] <> Nil) then begin
      fName := '';
      LasFiles[j].SetClassificationToZero(fName);
   end;
end;


procedure Tpt_cloud_opts_fm.BitBtn12Click(Sender: TObject);
begin
   RunProcess(wpPDALpmf);
end;


procedure Tpt_cloud_opts_fm.MapCallOutlineClouds;
var
   i,Cloud : integer;
   fName : PathStr;
   LasData : Las_Lidar.tLAS_data;
   sf      : tShapeFile;
   success : boolean;
begin
   {$IfDef PointCloudOutlines} WriteLineToDebugFile('Tpt_cloud_opts_fm.MapCallOutlineClouds in'); {$EndIf}
   for Cloud := 1 to MaxClouds do if UsePC[Cloud] and (LasFiles[Cloud] <> Nil) then begin
      if (LasFiles[Cloud].LAS_fnames.Count > 0) then  begin
         BaseMap.Image1.Canvas.Pen.Width := 3;
         BaseMap.Image1.Canvas.Brush.Style := bsClear;
         {$IfDef PointCloudOutlines} WriteLineToDebugFile('Cloud=' + IntToStr(Cloud) + '  files=' + IntToStr(LasFiles[Cloud].LAS_fnames.Count) + '  color=' + IntToStr(BaseMap.Image1.Canvas.Pen.Color)); {$EndIf}
         for i := 0 to pred(LasFiles[Cloud].LAS_fnames.Count) do begin
            fName := LasFiles[Cloud].LAS_fnames.Strings[i];
            if FileExtEquals(fName, '.LAS') then begin
               LasData := Las_Lidar.tLAS_data.Create(fName);
               LasData.OutlineOnMap(BaseMap,ConvertPlatformColorToTColor(MDDef.CloudSymbol[Cloud].Color));
               FreeAndNil(LasData);
            end
            else if FileExtEquals(fName,'.shp') then  begin
               sf := tShapeFile.Create(fName,Success);
               sf.PlotBoundingBox(BaseMap);
               FreeAndNil(sf);
            end;
         end;
      end;
   end;
end;

procedure Tpt_cloud_opts_fm.Maxintenstiy1Click(Sender: TObject);
begin
    MakeGrid(pcgmMaxIntensity);
end;

procedure Tpt_cloud_opts_fm.Meanfirstreturns1Click(Sender: TObject);
begin
   MakeGrid(pcgmMeanFirst);
end;

procedure Tpt_cloud_opts_fm.Meanstandarddeviation1Click(Sender: TObject);
begin
   MakeGrid(pcgmMeanStd);
end;

procedure Tpt_cloud_opts_fm.MICRODEM1Click(Sender: TObject);
begin
   MakeGrid(pcgmGrndPtDTM);
end;

procedure Tpt_cloud_opts_fm.Minintensity1Click(Sender: TObject);
begin
   MakeGrid(pcgmMinIntensity);
end;

procedure Tpt_cloud_opts_fm.Nonlastreturns1Click(Sender: TObject);
begin
    MakeGrid(pcgmAirNonLastReturn);
end;

procedure Tpt_cloud_opts_fm.OutLineClouds;
begin
   {$IfDef RecordLASfiles} WriteLineToDebugFile('Tpt_cloud_opts_fm.BitBtn13Click in'); {$EndIf}
   if MDdef.AutoReDrawMapLAS then begin
      if (BaseMap <> Nil) then BaseMap.DoFastMapRedraw;
      if (not MDDef.ShowCloudOutlines) then MapCallOutlineClouds;
   end
   else begin
      MapCallOutlineClouds;
   end;
end;


procedure Tpt_cloud_opts_fm.Overlappoints1Click(Sender: TObject);
begin
   MakeGrid(pcgmOverlap);
end;

procedure Tpt_cloud_opts_fm.Pointcount1Click(Sender: TObject);
begin
   MakeGrid(pcgmPointCount);
end;

procedure Tpt_cloud_opts_fm.PointsourceID1Click(Sender: TObject);
begin
   MakeGrid(pcgmPointSourceID);
end;

procedure Tpt_cloud_opts_fm.BitBtn13Click(Sender: TObject);
var
   i,Cloud,NumShow : integer;
   fName : PathStr;
   Results : tStringList;
   LasData : Las_Lidar.tLAS_data;
   sf : tShapeFile;
   success : boolean;
begin
   {$IfDef RecordLASfiles} WriteLineToDebugFile('Tpt_cloud_opts_fm.BitBtn13Click in'); {$EndIf}
   if (BaseMap = Nil) then exit;
   ShowHourglassCursor;
   if (Sender = BitBtn18) or (Sender = Nil) then BaseMap.DoFastMapRedraw;
   for Cloud := 1 to MaxClouds do if  UsePC[Cloud] and (LasFiles[Cloud] <> Nil) then begin
      if (LasFiles[Cloud].LAS_fnames.Count  > 0) then  begin
         //BaseMap.Image1.Canvas.Pen.Color := clRed;
         //BaseMap.Image1.Canvas.Pen.Width := 3;
         //BaseMap.Image1.Canvas.Brush.Style := bsClear;
         NumShow := LasFiles[Cloud].LAS_fnames.Count;
         if LasFiles[Cloud].LAS_fnames.Count > 5 then begin
            ReadDefault('Metadata files to show',NumShow);
         end;
          for i := 0 to pred(NumShow) do begin
             if (Sender = BitBtn13) then BaseMap.DoFastMapRedraw;
             fName := LasFiles[Cloud].LAS_fnames.Strings[i];
             if FileExtEquals(fName,'.LAS') then  begin
                LasData := Las_Lidar.tLAS_data.Create(fName);
                if (Sender = BitBtn18) or (Sender = Nil) then begin
                   {$If Defined(PointCloudOutlines) or Defined(RecordLASfiles)} WriteLineToDebugFile('Tpt_cloud_opts_fm.BitBtn13Click, call LasData.OutlineOnMap'); {$EndIf}
                   LasData.OutlineOnMap(BaseMap,clRed);
                end
                else begin
                   {$IfDef RecordLASfiles} WriteLineToDebugFile('LASdata.GetMetadata for ' + ExtractFileName(fName)); {$EndIf}
                   Results := LASdata.GetMetadata;
                   Petmar.DisplayAndPurgeStringList(Results,ExtractFileName(fName));
                end;
                FreeAndNil(LasData);
             end
             else if FileExtEquals(fName,'.shp') then begin
                sf := tShapeFile.Create(fName,Success);
                sf.PlotBoundingBox(BaseMap);
                FreeAndNil(sf);
             end;
          end;
      end;
   end;
   ShowDefaultCursor;
end;


procedure Tpt_cloud_opts_fm.BitBtn14Click(Sender: TObject);
var
   GISNum,
   i,Count,xgrid,ygrid,cat,Cloud : integer;
   z : float32;
   Lat,Long,zpt,zcrit : float64;
   fName : PathStr;
   Table : tMyData;
   TheFilter : AnsiString;
   Valid,ChangeUse : boolean;
   Ext : ExtStr;
   Results : tStringList;
begin
   if (BaseMap.MapDraw.DEMonMap = 0) then exit;
   CheckEditString(Edit2.Text,zcrit);
   for Cloud := 1 to MaxClouds do if  UsePC[Cloud] and (LasFiles[Cloud] <> Nil) then begin
      Ext := UpperCase(ExtractFileExt(LasFiles[Cloud].LAS_fnames.Strings[0]));
      if ExtEquals(Ext,DefaultDBExt) or ExtEquals(Ext, '.shp') then begin
         if CheckBox1.Checked then begin
            fName := ChangeFileExt(LasFiles[Cloud].LAS_fnames.Strings[0],DefaultDBExt);
            if OpenNumberedGISDataBase(GISNum,fName) then  begin
               DEMDBFilter.GetFilterString(GISNum,TheFilter,ChangeUse);
               CloseAndNilNumberedDB(GISNum);
            end;
         end;
         Results := tStringList.Create;
         StartProgress('Map');
         for i := 0 to Pred(LasFiles[Cloud].LAS_fnames.Count) do  begin
            fName := ChangeFileExt(LasFiles[Cloud].LAS_fnames.Strings[i],DefaultDBExt);
            Table := tMyData.Create(fName);
            if CheckBox1.Checked then begin
               Table.ApplyFilter(TheFilter);
            end;
            Results.Add('DZ,CLASS,LAT,LONG');
            Count := 0;
            while not Table.eof do begin
               inc(Count);
               if (Count mod 1000 = 0) then UpdateProgressBar(Count/Table.RecordCount);
               Lat := Table.GetFieldByNameAsFloat('LAT');
               Long := Table.GetFieldByNameAsFloat('LONG');
               zpt := Table.GetFieldByNameAsFloat('ELEV');
               DEMGlb[BaseMap.MapDraw.DEMonMap].LatLongDegreeToDEMGridInteger(Lat,Long,xgrid,ygrid);
               if (RadioGroup3.ItemIndex = 0) then  begin
                  Valid := DEMGlb[BaseMap.MapDraw.DEMonMap].GetElevMeters(xgrid,ygrid,z);
               end;
               if Sender = BitBtn15 then begin
                  if RadioGroup3.ItemIndex = 1 then begin
                     Valid := DEMGlb[BaseMap.MapDraw.DEMonMap].MaxElevAroundPoint(xgrid,ygrid,z);
                  end;
                  Valid := Valid and (zpt > z + zcrit);
               end
               else begin
                  if (RadioGroup3.ItemIndex = 1) then begin
                     Valid := DEMGlb[BaseMap.MapDraw.DEMonMap].MinElevAroundPoint(xgrid,ygrid,z);
                  end;
                  Valid := Valid and (zpt < z - zcrit);
               end;
               if Valid then begin
                  cat := Table.GetFieldByNameAsInteger('CLASS');
                  Results.Add(RealToString(z-zpt,-12,-2) + ',' + IntToStr(cat) + ',' + RealToString(Lat,-18,-8) + ',' + RealToString(Long,-18,-8));
               end;
               Table.Next;
            end;
            FreeAndNil(Table);
         end;
      end;
      fName := PetMar.NextFileNumber(MDTempDir,'las_points_','.csv');
      BaseMap.StringListtoLoadedDatabase(Results,fName);
      EndProgress;
   end;
end;


procedure Tpt_cloud_opts_fm.BitBtn16Click(Sender: TObject);
begin
   RunProcess(wpWBClass);
   RunProcess(wpFusionClass);
   RunProcess(wpMCCclass);
   RunProcess(wpLasToolsClass);
   RunProcess(wpWBSegClass);
   RunProcess(wpPDALsmrf);
   RunProcess(wpPDALpmf);
end;

procedure Tpt_cloud_opts_fm.BitBtn17Click(Sender: TObject);
var
   i : integer;
begin
   for i := 1 to MaxClouds do if UsePC[i] and (LasFiles[i] <> Nil) then begin
      if (LasFiles[i].LAS_fnames.Count  > 0) and FileExtEquals(LasFiles[i].LAS_fnames[0], '.LAS') then begin
         LasFiles[i].CloudStatistics(false,csReportStats);
      end;
   end;
   UpdateColorOptions;
end;


procedure Tpt_cloud_opts_fm.BitBtn18Click(Sender: TObject);
begin
   OutLineClouds;
end;


procedure Tpt_cloud_opts_fm.BitBtn19Click(Sender: TObject);
var
   Bitmap : tMyBitmap;
   OutName : PathStr;
   Cloud   : integer;
begin
   for Cloud := 1 to MaxClouds do if UsePC[Cloud] and (LasFiles[Cloud] <> Nil) then begin
       if FileExtEquals(LasFiles[Cloud].LAS_fnames.Strings[0],'.LAS') then begin
          CopyImageToBitmap(BaseMap.Image1,Bitmap);
          OutName := '';
          LasFiles[Cloud].MergeLasPoints(mlOnMask,BaseMap,OutName,Bitmap);
          Bitmap.Free;
       end;
   end;
end;


procedure Tpt_cloud_opts_fm.UpdateColorOptions;
var
   Layer,ActiveClouds,i : integer;
   OldChosenOption : shortstring;
begin
   HasIntensity := false;
   HasClassification := false;
   HasTime := false;
   HasPointID := false;
   HasUserData := false;
   HasRGB := false;
   HasScanAngle:= false;
   HasReturnNumbers := false;
   ActiveClouds := 0;
   if RadioGroup1.ItemIndex > -1 then OldChosenOption := RadioGroup1.Items[RadioGroup1.ItemIndex];

   RadioGroup1.Items.Clear;
   RadioGroup1.Items.Add('Elevation');
   for Layer := 1 to MaxClouds do if (LasFiles[Layer] <> Nil) and (UsePC[Layer]) then begin
      if LasFiles[Layer].HasClassification then HasClassification := true;
      if LasFiles[Layer].HasIntensity then HasIntensity := true;
      if LasFiles[Layer].HasRGB then HasRGB := true;
      if LasFiles[Layer].HasScanAngle then HasScanAngle := true;
      if LasFiles[Layer].HasReturnNumbers then HasReturnNumbers := true;
      if LasFiles[Layer].HasTime then HasTime := true;
      if LasFiles[Layer].HasPointID then HasPointID := true;
      if LasFiles[Layer].HasUserData then HasUserData := true;
      inc(ActiveClouds);
   end;

   if HasClassification then RadioGroup1.Items.Add('Classification');
   if HasIntensity then RadioGroup1.Items.Add('Intensity');
   if HasRGB then RadioGroup1.Items.Add('RGB');
   if HasScanAngle then RadioGroup1.Items.Add('Scan angle');
   if HasReturnNumbers then begin
      RadioGroup1.Items.Add('Return numbers');
      RadioGroup1.Items.Add('Returns in pulse');
   end;
   if HasTime then RadioGroup1.Items.Add('GPS time');
   if HasPointID then RadioGroup1.Items.Add('Point ID');
   if HasUserData then RadioGroup1.Items.Add('User data');
   if (ActiveClouds > 1) then RadioGroup1.Items.Add('Cloud ID');

   Edit4.Enabled := HasRGB;
   Edit23.Enabled := HasRGB;
   Label2.Enabled := HasRGB;
   Label21.Enabled := HasRGB;
   for I := 0 to pred(RadioGroup1.Items.Count) do begin
      if OldChosenOption = RadioGroup1.Items[i] then begin
         RadioGroup1.ItemIndex := i;
         exit;
      end;
   end;
   RadioGroup1.ItemIndex := 0;
end;


procedure Tpt_cloud_opts_fm.Userdata1Click(Sender: TObject);
begin
    MakeGrid(pcgmUserData);
end;


function Tpt_cloud_opts_fm.LegendForPointCloudLayers : tMyBitmap;
var
   Layer,i,j : integer;
begin
   Result := nil;
   for Layer := 1 to Maxclouds do begin
     if (LasFiles[Layer] <> Nil) and (LasFiles[Layer].LAS_fnames.Count > 0) then begin
          if (MDDef.ls.ColorCoding = lasccElevation) then begin
             Result := DefaultHorizontalLegendOnBitmap(BaseMap.MapDraw.LasMinAreaZ,BaseMap.MapDraw.LasMaxAreaZ,'','',LegTerrain);
          end
          else if (MDDef.ls.ColorCoding = lasccGPSTime) then begin
             Result := DefaultHorizontalLegendOnBitmap(LasFiles[Layer].MinTime,LasFiles[Layer].MaxTime,'','',LegSpectrum);
          end
          else if (MDDef.ls.ColorCoding = lasccScanAngle) then begin
             Result := DefaultHorizontalLegendOnBitmap(-25,25,'','',LegSpectrum);
          end
          else if (MDDef.ls.ColorCoding in [lasccReturnNumber,lasccReturnsPulse]) then begin
             CreateBitmap(Result,50,MaxReturns * 25);
             ClearBitmap(Result,clNearWhite);
             for I := 1 to MaxReturns do begin
                Result.Canvas.Brush.Color := ConvertPlatformColorToTColor(Las_ret_colors[i]);
                Result.Canvas.Brush.Style := bsSolid;
                BitmapRectangle(Result,2, 2 + pred(i) * 25,22,2 + pred(i) * 25 + 22);
                Result.Canvas.Brush.Style := bsClear;
                Result.Canvas.Font.Size := 14;
                BitmapTextOut(Result,28, 2 + pred(i) * 25 + 2,IntToStr(i));
             end;
             PetImage.GetImagePartOfBitmap(Result);
             Result.Canvas.Brush.Style := bsClear;
             Result.Canvas.Pen.Color := clBlack;
             Result.Canvas.Pen.Width := 2;
             BitmapRectangle(Result,0,0,Pred(Result.Width),pred(Result.Height));
          end
          else if (MDDef.ls.ColorCoding = lasccCloudID) then begin
             CreateBitmap(Result,500,MaxLasCat * 25);
             ClearBitmap(Result,clNearWhite);
             j := 0;
             for I := 1 to MaxClouds do begin
                if UsePC[i] and (LasFiles[i] <> Nil) then begin
                   Result.Canvas.Brush.Color := ConvertPlatformColorToTColor(MDDef.CloudSymbol[i].Color);
                   Result.Canvas.Brush.Style := bsSolid;
                   BitmapRectangle(Result,2, 2 + j * 25,22,2 + j * 25 + 22);
                   Result.Canvas.Brush.Style := bsClear;
                   Result.Canvas.Font.Size := 14;
                   BitmapTextOut(Result,28, 2 + j * 25 + 2,LasFiles[i].CloudName);
                   inc(j);
                end;
             end;
             if (j = 0) then begin
                FreeAndNil(Result);
             end
             else begin
                PetImage.GetImagePartOfBitmap(Result);
                Result.Canvas.Brush.Style := bsClear;
                Result.Canvas.Pen.Color := clBlack;
                Result.Canvas.Pen.Width := 2;
                BitmapRectangle(Result,0,0,Pred(Result.Width),pred(Result.Height));
             end;
          end
          else if (MDDef.ls.ColorCoding = lasccClass) then begin
             CreateBitmap(Result,500,MaxLasCat * 25);
             ClearBitmap(Result,clNearWhite);
             j := 0;
             for I := 0 to MaxLasCat do begin
                if LasCatUsed[i] then begin
                   Result.Canvas.Brush.Color := ConvertPlatformColorToTColor(LAS_RGB_colors[i]);
                   Result.Canvas.Brush.Style := bsSolid;
                   BitmapRectangle(Result,2, 2 + j * 25,22,2 + j * 25 + 22);
                   Result.Canvas.Brush.Style := bsClear;
                   Result.Canvas.Font.Size := 14;
                   BitmapTextOut(Result,28, 2 + j * 25 + 2,LasCatName[i]);
                   inc(j);
                end;
             end;
             if (j = 0) then begin
                FreeAndNil(Result);
             end
             else begin
                PetImage.GetImagePartOfBitmap(Result);
                Result.Canvas.Brush.Style := bsClear;
                Result.Canvas.Pen.Color := clBlack;
                Result.Canvas.Pen.Width := 2;
                BitmapRectangle(Result,0,0,Pred(Result.Width),pred(Result.Height));
             end;
          end;
       end;
       {$IfDef RecordLASfilesRedraw} WriteLineToDebugFile('Tpt_cloud_opts_fm.RedrawPointCloudLayer out ' +  IntToStr(Layer)); {$EndIf}
      exit;
   end;
end;



procedure Tpt_cloud_opts_fm.RedrawAllLayers;


      procedure RedrawPointCloudLayer(Layer : integer);
      var
         i : integer;

            procedure GetElevationLimits(LasLayer : integer);
            var
               sf : tShapeFile;
               success : boolean;
               i : integer;
               fName : PathStr;
               MinAreaZ,MaxAreaZ : float64;
               LasData : Las_Lidar.tLAS_data;
            begin
                {$IfDef RecordLAS} WriteLineToDebugFile('TMapForm.PlotLASFilesOnMap in layer =' + IntToStr(LasLayer)); {$EndIf}
                if (MDDef.ls.ColorCoding = lasccElevation) and (MinAreaZ > MaxAreaZ) then begin
                   for i := 0 to pred(pt_cloud_opts_fm.LASfiles[LasLayer].LAS_fnames.count) do begin
                      fName := pt_cloud_opts_fm.LASfiles[LasLayer].LAS_fnames.Strings[i];
                      if FileExtEquals(fName, '.shp') then begin
                         sf := tShapeFile.Create(fName,Success);
                         if (i=0) then begin
                            MinAreaZ := sf.MainFileHeader.BoundBoxZMin;
                            MaxAreaZ := sf.MainFileHeader.BoundBoxZMax;
                         end
                         else begin
                            if (sf.MainFileHeader.BoundBoxZMin < MinAreaZ) Then MinAreaZ := sf.MainFileHeader.BoundBoxZMin;
                            if (sf.MainFileHeader.BoundBoxZMax > MaxAreaZ) Then MaxAreaZ := sf.MainFileHeader.BoundBoxZMax;
                         end;
                         sf.Destroy;
                      end
                      else begin
                         LasData := Las_Lidar.tLAS_data.Create(fName);
                         if (i = 0) then begin
                            MinAreaZ := LasData.LasHeader.MinZ;
                            MaxAreaZ := LasData.LasHeader.MaxZ;
                         end
                         else begin
                            if (MinAreaZ > LasData.LasHeader.MinZ) Then MinAreaZ := LasData.LasHeader.MinZ;
                            if (MaxAreaZ < LasData.LasHeader.MaxZ) Then MaxAreaZ := LasData.LasHeader.MaxZ;
                         end;
                         LasData.Destroy;
                      end;
                      {$IfDef RecordLAS} WriteLineToDebugFile('range for ' + ExtractFilename(fname)  + '  ' + RealToString(MinAreaZ,9,1) + ' to' +RealToString(MaxAreaZ,9,1)); {$EndIf}
                   end;
                   BaseMap.MapDraw.LasMinAreaZ := MinAreaZ;
                   BaseMap.MapDraw.LasMaxAreaZ := MaxAreaZ;
                   NeedNewElevationLimits := false;
                end;
            end;


      begin
         {$If Defined(RecordPointCloudViewing) or Defined(PointCloudMap)} WriteLineToDebugFile('Tpt_cloud_opts_fm.RedrawPointCloudLayer in ' + IntToStr(Layer)); {$EndIf}
         if (Layer in [1..MaxClouds]) and (LasFiles[Layer] <> Nil) and (LasFiles[Layer].LAS_fnames.Count > 0) then begin
            for i := 0 to MaxLASCat do LasCatUsed[i] := false;
            {$IfDef RecordLASfilesRedraw} WriteLineToDebugFile('call BaseMap.PlotLASFilesOnMap'); {$EndIf}
            if NeedNewElevationLimits then GetElevationLimits(Layer);
            {$IfDef RecordLASfilesRedraw} WriteLineToDebugFile('Tpt_cloud_opts_fm.RedrawPointCloudLayer out ' +  IntToStr(Layer)); {$EndIf}
         end
         else begin
            {$IfDef RecordLASfilesRedraw} WriteLineToDebugFile('Tpt_cloud_opts_fm.RedrawPointCloudLayer, did not draw ' +  IntToStr(Layer)); {$EndIf}
         end;
      end;


var
   Cloud : integer;
   TStr : shortstring;
begin {Tpt_cloud_opts_fm.RedrawAllLayers}
   if (NumOpenClouds > 0) then begin
      if (MDdef.ls.ColorCoding = lasccElevation) then begin
         CheckEditString(Edit5.Text, BaseMap.MapDraw.LasMaxAreaZ);
         CheckEditString(Edit6.Text, BaseMap.MapDraw.LasMinAreaZ);
      end;
       {$If Defined(RecordPointCloudViewing) or Defined(PointCloudMap)} WriteLineToDebugFile('Tpt_cloud_opts_fm.RedrawAllLayers in'); {$EndIf}
       for Cloud := MaxClouds downto 1 do begin
          if UsePC[Cloud] and (LasFiles[Cloud] <> Nil) then begin
             {$If Defined(RecordPointCloudViewing) or Defined(TrackPointCloud)} WriteLineToDebugFile('Tpt_cloud_opts_fm.BitBtn1Click (draw) Cloud=' + IntToStr(Cloud)); {$EndIf}
             RedrawPointCloudLayer(Cloud);
          end
          else if (LasFiles[Cloud] <> Nil) then begin
             {$If Defined(RecordPointCloudViewing) or Defined(PointCloudMap) or Defined(TrackPointCloud)} WriteLineToDebugFile('Not drawing cloud=' + IntToStr(Cloud)); {$EndIf}
          end;
       end;
       {$IfDef RecordPointCloudViewing} WriteLineToDebugFile('Tpt_cloud_opts_fm.RedrawAllLayers DoFastMapRedraw'); {$EndIf}
       BaseMap.MapDraw.DeleteSingleMapLayer(BaseMap.MapDraw.LegendOverlayfName);
       BaseMap.DoFastMapRedraw;
       TStr := 'Point cloud display:';
       for Cloud := 1 to MaxClouds do if UsePC[Cloud] and (LasFiles[Cloud] <> Nil) then TStr := TStr + ' ' + LasFiles[Cloud].CloudName;
       BaseMap.Caption := TStr;
       BaseMap.BringToFront;
       EndProgress;
       {$If Defined(RecordPointCloudViewing) or Defined(TrackPointCloud)} WriteLineToDebugFile('Tpt_cloud_opts_fm.RedrawAllLayers out'); {$EndIf}
   end;
end {Tpt_cloud_opts_fm.RedrawAllLayers};


procedure Tpt_cloud_opts_fm.BitBtn1Click(Sender: TObject);
begin
    PopUpMenu1.Popup(Mouse.CursorPos.X,Mouse.CursorPos.Y);
end;


procedure Tpt_cloud_opts_fm.BitBtn25Click(Sender: TObject);
var
   i : integer;
begin
   for i := 1 to MaxClouds do if UsePC[i] and (LasFiles[i] <> Nil) then begin
      if (LasFiles[i].LAS_fnames.Count  > 0) and FileExtEquals(LasFiles[i].LAS_fnames[0], '.LAS') then begin
         LasFiles[i].ElevationHistogram(BaseMap);
      end;
   end;
end;


procedure Tpt_cloud_opts_fm.BitBtn26Click(Sender: TObject);
begin
   CreateGridOrDBfromStats(dbpcDSM,MDDef.DSMpercentile);
end;

procedure Tpt_cloud_opts_fm.BitBtn20Click(Sender: TObject);
begin
   BitBtn8Click(Sender);
end;


procedure Tpt_cloud_opts_fm.BitBtn21Click(Sender: TObject);
var
   NewName : PathStr;
   i,LayerExport : integer;
   FirstFile : boolean;
begin
   {$If Defined(RecordPointCloudViewing) or Defined(TimePointCloud) or Defined(Slicer)} WriteLineToDebugFile('Tpt_cloud_opts_fm.BitBtn21Click (slice) in'); {$EndIf}
   if (MDDef.ProgramOption = ExpertProgram) and MDDef.ShowIcesat then GetIcesatFilter;
   SaveBackupDefaults;
   if (MemPtCloud = Nil) then begin
       {$IfDef RecordPointCloudViewing} WriteLineToDebugFile('Tpt_cloud_opts_fm.BitBtn21Click (MemPtCloud = Nil)'); {$EndIf}
       FirstFile := true;
       NewName := '';
       for i := MaxClouds downto 1 do begin
          if UsePC[i] and (LasFiles[i] <> Nil) then begin
             {$IfDef RecordPointCloudViewing} WriteLineToDebugFile('Tpt_cloud_opts_fm.BitBtn21Click cloud=' + IntToStr(i)); {$EndIf}
             wmdem.SetPanelText(0,'Slice out=' + IntToStr(i) + ' ' + LasFiles[i].CloudName);
             LasFiles[i].ShowLASProgress := true;
             if (LasFiles[i].LAS_fnames.Count = 1) and BaseMap.MapDraw.LatLongOnScreen(LasFiles[i].GeoBBox.ymin,LasFiles[i].GeoBBox.xmin) and BaseMap.MapDraw.LatLongOnScreen(LasFiles[i].GeoBBox.ymax,LasFiles[i].GeoBBox.xmax) then begin
                NewName := LasFiles[i].LAS_fnames.Strings[0];
             end
             else begin
                NewName := Petmar.NextFileNumber(MDTempDir, LasFiles[i].CloudName + '_subset_','.las');
                LasFiles[i].MergeLasPoints(mlOnMap,BaseMap,NewName,Nil);
             end;
             if FileExists(NewName) then begin
                wmdem.SetPanelText(0,'Slice draw=' + IntToStr(i) + ' ' + LasFiles[i].CloudName);
                if FirstFile then begin
                   FirstFile := false;
                   LayerExport := 1;
                   DEMDefs.VasaProjectFName := NewName;
                   {$If Defined(RecordPointCloudViewing) or Defined(TimePointCloud) or Defined(Slicer)} WriteLineToDebugFile('Tpt_cloud_opts_fm.BitBtn21Click (slice) call Slicer_3D.DB_3dSlice'); {$EndIf}
                   Slicer_3D.DB_3dSlices(BaseMap,Nil,Nil,'','','','',MDDef.LatLongSlices);
                end
                else begin
                   Inc(LayerExport);
                   SlicerForm.LoadMemoryPointCloud(LayerExport,NewName);
                end;
             end;
          end;
       end;
   end
   else begin
      {$If Defined(RecordPointCloudViewing) or Defined(TimePointCloud) or Defined(Slicer)}  WriteLineToDebugFile('Tpt_cloud_opts_fm.BitBtn21Click (MemPtCloud <> Nil)'); {$EndIf}
      wmdem.SetPanelText(0,'Slice start');
      Slicer_3D.DB_3dSlices(BaseMap,MemPtCloud);
   end;
   wmdem.SetPanelText(0,'');
   RestoreBackupDefaults;
   {$If Defined(RecordPointCloudViewing) or Defined(TimePointCloud) or Defined(Slicer)} WriteLineToDebugFile('Tpt_cloud_opts_fm.BitBtn21Click (slice) out'); {$EndIf}
end;


procedure Tpt_cloud_opts_fm.BitBtn22Click(Sender: TObject);
begin
   GetString('lasground parameters',MDDef.LastoolsGroundParams,false,ReasonableTextChars);
   GetString('lasclassify parameters',MDDef.lastoolsClassifyParams,false,ReasonableTextChars);
end;

procedure Tpt_cloud_opts_fm.BitBtn23Click(Sender: TObject);
begin
   ReadDefault('scale parameter',MDDef.ls.MCC_scale);
   ReadDefault('threshhold',MDDef.ls.MCC_thresh);
end;


procedure Tpt_cloud_opts_fm.BitBtn24Click(Sender: TObject);
begin
   CreateGridOrDBfromStats(dbpcDataBase);
end;


procedure Tpt_cloud_opts_fm.BitBtn27Click(Sender: TObject);
begin
   BitBtn8Click(Sender);
end;

procedure Tpt_cloud_opts_fm.BitBtn28Click(Sender: TObject);
begin
   GetFilesForPointCloud(5,LastLidar5Directory);
end;

procedure Tpt_cloud_opts_fm.BitBtn29Click(Sender: TObject);
begin
   GetFilesForPointCloud(2,LastLidar2Directory);
end;

procedure Tpt_cloud_opts_fm.BitBtn2Click(Sender: TObject);
begin
   RunProcess(wpWBClass);
end;


procedure Tpt_cloud_opts_fm.RunProcess(WhatProcess : tWhatProcess);
var
   fName,fName2,fName3,FinalName,
   NewDir : PathStr;
   i,lt : integer;
   bFile : tStringList;
   pdalMethod : shortstring;
   NewString, Params,
   cmd : ANSIstring;
begin
   {$IfDef RecordPointCloudOptionsForm} WriteLineToDebugFile('Tpt_cloud_opts_fm.BitBtn2Click in'); {$EndIf}
   bfile := Nil;
   if (WhatProcess in [wpWBClass,wpWBSegClass,wpWBDeNoise]) and (not WhiteBoxPresent) then exit;
   if (WhatProcess in [wpWBClass]) then begin
      NewString := 'WB_class';
   end;
   if (WhatProcess in [wpWBSegClass]) then begin
      NewString := 'WB_seg_class';
   end;
   if (WhatProcess in [wpWBDeNoise]) then begin
      NewString := 'WB_denoise';
   end;
   if (WhatProcess in [wpFusionClass]) then begin
      NewString := 'fusion_class';
   end;
   if (WhatProcess in [wpMCCclass]) then begin
      if not MCC_lidarPresent then exit;
      NewString := 'mcc_class';
   end;
   if (WhatProcess in [wpLasToolsClass]) then begin
      NewString := 'lastools_class';
   end;
   if WhatProcess in [wpPDALsmrf] then begin
      StartGDALbatchFile(bFile);
      NewString := 'pdal_smrf_class';
      pdalMethod := 'smrf';
   end;
   if WhatProcess in [wpPDALpmf] then begin
      StartGDALbatchFile(bFile);
      NewString := 'pdal_pmf_class';
      pdalMethod := 'pmf';
   end;

   {$IfDef RecordPointCloudOptionsForm} WriteLineToDebugFile('Tpt_cloud_opts_fm.BitBtn2Click options set, ' + NewString); {$EndIf}
   NewDir := ExtractFilePath(LasFiles[1].LAS_fnames.Strings[0]) + NewString + '\';
   SafeMakeDir(NewDir);

   lt := succ(LasFiles[1].LAS_fnames.Count div MDDef.MaxThreadsForPC);
   if (bfile = Nil) then bFile := tStringList.Create;

   for i := 0 to pred(LasFiles[1].LAS_fnames.Count) do begin
      fName := LasFiles[1].LAS_fnames.strings[i];
      FinalName := NewDir + NewString + '_' + ExtractFilename(fName);

      if (WhatProcess = wpWBClass) then cmd := WBT_GroundClassify(fName,FinalName);
      if (WhatProcess = wpWBSegClass) then cmd := WBT_LidarSegmentationBasedFilter(fName,FinalName);
      if (WhatProcess = wpWBDenoise) then cmd := WBT_DeNoise(fName,FinalName);

      if (WhatProcess = wpFusionClass) then cmd :=  ProgramRootDir + 'fusion\groundfilter ' + FinalName + ' ' + RealToString(MDdef.DefLidarXGridSize,-12,-29) + ' ' + fName;

      if (WhatProcess = wpMCCclass) then cmd := mcc_lidarFName + ' -s ' + RealToString(MDDef.ls.MCC_scale,-12,-2) + ' -t ' + RealToString(MDDef.ls.MCC_thresh,-12,-2) + ' ' + fName + ' ' + FinalName;

      if (WhatProcess = wpLasToolsClass) then begin
         fName2 := MDTempDir + 'las_ground_outputInt_' + IntToStr(i) + '.las';
         cmd := ProgramRootDir + 'lastools\bin\lasground.exe -i ' + fName + ' -o ' + fName2 + ' ' + MDDef.LastoolsGroundParams;
         bFile.Add(cmd);
         fName3 := MDTempDir + 'las_height_output.las_' + IntToStr(i) + '.las';
         cmd := ProgramRootDir + 'lastools\bin\lasheight.exe -i ' + fName2 + ' -o ' + fName3;
         bFile.Add(cmd);
         cmd := ProgramRootDir + 'lastools\bin\lasclassify.exe -i ' + fName3 + ' -o ' + FinalName + ' ' + MDDef.lastoolsClassifyParams;
      end;

      if WhatProcess in [wpPDALsmrf,wpPDALpmf] then begin
         fName2 := MDTempDir + 'pdal_output_' + IntToStr(i) + '.las';
         cmd := 'pdal translate ' + fName +  ' -o ' + fName2 + ' ' + pdalMethod + ' -v 4';
         bFile.Add(cmd);
         cmd := 'pdal translate ' + fName2 + ' -o ' + FinalName + ' -v 4 -f filters.reprojection --filters.reprojection.out_srs="EPSG:269' + Edit19.Text + '" --filters.reprojection.in_srs="EPSG:269' + Edit19.Text + '"';
      end;

      bFile.Add(Cmd);
      if (Succ(i) mod lt = 0) or (i=pred(LasFiles[1].LAS_fnames.Count)) then begin
         fName := MDtempdir + NewString + IntToStr(succ(i)) + '.bat';
         bFile.SaveToFile(fName);
         bFile.Clear;
         if MDDef.LogDosOutput then begin
            Params := '>' + MDTempDir + Newstring + '_log_' + IntToStr(succ(i)) + '.txt' + ' 2>&1';
         end
         else Params := '';
         if WhatProcess in [wpPDALsmrf,wpPDALpmf] then begin
            EndBatchFile(MDTempDir + NewString + '.bat',bfile);
            StartGDALbatchFile(bFile);
         end
         else begin
            ExecuteFile(fName, Params, '');
         end;
      end;
   end;
   bFile.Destroy;
   {$IfDef RecordPointCloudOptionsForm} WriteLineToDebugFile('Tpt_cloud_opts_fm.BitBtn2Click out'); {$EndIf}
end;

procedure Tpt_cloud_opts_fm.BitBtn30Click(Sender: TObject);
begin
   PopUpMenu2.Popup(Mouse.CursorPos.X,Mouse.CursorPos.Y);
end;

procedure Tpt_cloud_opts_fm.BitBtn31Click(Sender: TObject);
begin
   LasFiles[1].ElevRangeOnMap(BaseMap.MapDraw,MinAreaZ,MaxAreaZ);
   Edit5.Text := realToString(MaxAreaZ,-12,-1);
   Edit6.Text := realToString(MinAreaZ,-12,-1);
end;

procedure Tpt_cloud_opts_fm.BitBtn32Click(Sender: TObject);
begin
   UncheckAll;
end;

procedure Tpt_cloud_opts_fm.BitBtn33Click(Sender: TObject);
begin
   RunProcess(wpWBDenoise);
end;

procedure Tpt_cloud_opts_fm.BitBtn34Click(Sender: TObject);
var
   fName : PathStr;
   lf : tLas_Data;
begin
    fName := MDTempDir + 'map_coverage.las';
    LasFiles[1].ShowLASProgress := true;
    LasFiles[1].MergeLasPoints(mlOnMap,BaseMap,fName,nil);
    lf := tLas_Data.Create(fName);
    fName := MDTempDir + 'points.dbf';
    lf.ExportXYZ_DB(false,fName,BiggestTriangulation);
    lf.Destroy;
    tTIN.Create(BaseMap,fName,false);
end;

procedure Tpt_cloud_opts_fm.BitBtn35Click(Sender: TObject);
begin
   SubsetLASfiles(ExtractFilePath(LasFiles[1].LAS_fnames.Strings[0]));
   BitBtn5Click(Sender);
   GetFilesForPointCloud(1,LastLidarDirectory);
end;


procedure Tpt_cloud_opts_fm.BitBtn36Click(Sender: TObject);
begin
   ReadDefault('WBGroundClassRadius',MDDef.WBGroundClassRadius);
   ReadDefault('WBSegFilterRadius',MDDef.WBSegFilterRadius);
   ReadDefault('WBDeNoiseRadius',MDDef.WBDeNoiseRadius);
   ReadDefault('WBDenoiseElevDiff',MDDef.WBDenoiseElevDiff);
end;


procedure Tpt_cloud_opts_fm.BitBtn37Click(Sender: TObject);
begin
  //The following LAStools require licensing for all commercial & government use. They can however be used freely for non-profit
  //personal, non-profit educational, or non-profit humanitarian purposes assuming that they are strictly non-military. Imagine
  //you would have to personally convince me that you will better the world (according to my definition) ... (-:
  //
  //this restriction covers the three programs used here
   if AnswerIsYes('I meet licensing restrictions for lasclassify, lasground, and lasheight at http://www.cs.unc.edu/~isenburg/lastools/download/LICENSE.txt') then begin
      RunProcess(wpLasToolsClass);
   end;
end;

procedure Tpt_cloud_opts_fm.BitBtn38Click(Sender: TObject);
begin
   CreateGridOrDBfromStats(dbpcMidCloud,MDDef.Midpercentile);
end;

procedure Tpt_cloud_opts_fm.BitBtn39Click(Sender: TObject);
begin
   LasFiles[1].SubdivideLasPointsByClassification;
end;

procedure Tpt_cloud_opts_fm.BitBtn3Click(Sender: TObject);
begin
   {$IfDef RecordPointCloudOptionsForm} WriteLineToDebugFile('Tpt_cloud_opts_fm.BitBtn2Click in'); {$EndIf}
   RunProcess(wpWBSegClass);
end;


procedure Tpt_cloud_opts_fm.BitBtn40Click(Sender: TObject);
var
   i : integer;
begin
   for i := 1 to MaxClouds do begin
      if (LasFiles[i] <> Nil) and (LasFiles[i].LAS_fnames.Count  > 0) and FileExtEquals(LasFiles[i].LAS_fnames[0], '.LAS') then begin
         LasFiles[i].RGBPalette;
      end;
   end;
end;

procedure Tpt_cloud_opts_fm.BitBtn41Click(Sender: TObject);
begin
   CreateGridOrDBfromStats(dbpcDTM,MDDef.DTMpercentile);
end;

procedure Tpt_cloud_opts_fm.BitBtn42Click(Sender: TObject);
begin
   LasFiles[1].ZeroLessUsefulFields;
end;

procedure Tpt_cloud_opts_fm.BitBtn43Click(Sender: TObject);
begin
   GetFilesForPointCloud(3,LastLidar3Directory);
end;

procedure Tpt_cloud_opts_fm.BitBtn44Click(Sender: TObject);
begin
   GetFilesForPointCloud(4,LastLidar4Directory);
end;

procedure Tpt_cloud_opts_fm.BitBtn45Click(Sender: TObject);
begin
   Petmar.GetExistingFileName('wkt projection','*.prj;*.wkt',MDDef.WKTLidarProj);
   BitBtn45.Caption := ExtractFileNameNoExt(MDDef.WKTLidarProj);
end;

procedure Tpt_cloud_opts_fm.BitBtn46Click(Sender: TObject);
begin
   {$IfDef RecordPointCloudOptionsForm} WriteLineToDebugFile('Tpt_cloud_opts_fm.BitBtn46Click in'); {$EndIf}
   RunProcess(wpMCCclass);
end;


procedure Tpt_cloud_opts_fm.BitBtn47Click(Sender: TObject);
begin
   ZoomToCloudCoverage(1);
end;


procedure Tpt_cloud_opts_fm.ZoomToCloudCoverage(Layer : integer);
var
   Extra : float64;
begin
   {$IfDef RecordPointCloudOptionsForm} WriteLineToDebugFile('BitBtn47Click (Max lat/long) in'); {$EndIf}
   Extra := 0.025 * (LasFiles[Layer].GeoBBox.xmax - LasFiles[Layer].GeoBBox.xmin);
   Extra := 0;
   BaseMap.MapDraw.MaximizeLatLongMapCoverage(LasFiles[Layer].GeoBBox.ymin - Extra,LasFiles[Layer].GeoBBox.xmin  - Extra,LasFiles[Layer].GeoBBox.ymax  + Extra,LasFiles[Layer].GeoBBox.xmax  + Extra);
   {$IfDef RecordPointCloudOptionsForm} WriteLineToDebugFile('MaximizeLatLongMapCoverage over'); {$EndIf}
   BaseMap.DoCompleteMapRedraw;
   BaseMap.FullMapSpeedButton.Enabled := true;
   OutlineClouds;
   {$IfDef RecordPointCloudOptionsForm} WriteLineToDebugFile('BitBtn47Click (Max lat/long) out'); {$EndIf}
end;


procedure Tpt_cloud_opts_fm.BitBtn49Click(Sender: TObject);
var
    OutName : PathStr;
begin
   {$IfDef RecordPointCloudOptionsForm} WriteLineToDebugFile('Tpt_cloud_opts_fm.BitBtn49Click (valid grid out)'); {$EndIf}
    if FileExtEquals(LasFiles[1].LAS_fnames.Strings[0],'.LAS') then begin
       OutName := '';
       LasFiles[1].ShowLASProgress := true;
       LasFiles[1].MergeLasPoints(mlDEMCovered,BaseMap,OutName);
    end;
end;


procedure Tpt_cloud_opts_fm.BitBtn4Click(Sender: TObject);
var
   i,db  : integer;
   OutName,fName : PathStr;
   LasNames,FileNames : tStringList;
begin
    if FileExtEquals(LasFiles[1].LAS_fnames.Strings[0],'.LAS') then begin
       for i := 1 to MaxClouds do if UsePC[i] and (LasFiles[i] <> Nil) then begin
          OutName := '';
          LasFiles[i].ShowLASProgress := true;
          LasFiles[i].MergeLasPoints(mlOnMap,BaseMap,OutName,nil);
          if (Sender = BitBtn48) then begin
            LidarASCIIout(OutName,lasascClassInt);
            LAS2GeoJSON(OutName);
          end;
       end;
    end
    else begin
       OutName := MainMapData + 'subset.shp';
       Petmar.GetFileNameDefaultExt('exported shape file','*.shp',OutName);
       FileNames := tStringList.Create;
       LasNames := tStringList.Create;
       for i := 0 to pred(LasFiles[1].LAS_fnames.Count) do begin
          wmdem.SetPanelText(0,IntToStr(i) + '/' + IntToStr(pred(LasFiles[1].LAS_fnames.Count)));
          BaseMap.DoFastMapRedraw;
          fName := LasFiles[1].LAS_fnames.Strings[i];
          if FileExtEquals(fName,'.shp') then begin
             db := BaseMap.LoadDataBaseFile(fName);
             GISdb[db].LimitDBtoMapArea;
             if (GISdb[db].MyData.RecordCount > 0) then begin
                fName := MDTempDir + 'subset-pt-'+IntToStr(i) + DefaultDBExt;
                GISdb[db].SaveCurrentDBaseSubset(fName,1,true);
                Filenames.Add(FName);
             end;
             CloseAndNilNumberedDB(db);
          end;
       end;

       if (FileNames.Count > 0) then begin
          fName := FileNames.Strings[0];
          db := BaseMap.LoadDataBaseFile(fName,false,false);
          FileNames.Delete(0);
          GISdb[db].MergeDataBases(FileNames);
          GISdb[db].MyData.ApplyFilter('');
          GISdb[db].SavePointShapeFile;
          fName := GISdb[db].dbFullName;
          CloseAndNilNumberedDB(db);
          DEMESRIShapeFile.CopyShapeFile(Name,OutName);
       end;
       FileNames.Free;
    end;
    EndProgress;
    wmdem.SetPanelText(0,'');
end;


procedure SetGridFromTable(atable:tMyData);
begin
   if Uppercase(aTable.GetFieldByNameAsString('UNITS')) = 'M' then begin
      MDDef.LidarGridProjection := UTMBasedDEM;
      MDdef.DefLidarXGridSize := aTable.GetFieldByNameAsFloat('X_RES');
      MDdef.DefLidarYGridSize := aTable.GetFieldByNameAsFloat('Y_RES');
   end;
   if Uppercase(aTable.GetFieldByNameAsString('UNITS')) = 'SEC' then begin
      MDDef.LidarGridProjection := ArcSecDEM;
      MDdef.DefLidarGeoGridSizeY := aTable.GetFieldByNameAsFloat('Y_RES');
      MDdef.DefLidarGeoGridSizeX := aTable.GetFieldByNameAsFloat('X_RES');
   end;
   if Uppercase(aTable.GetFieldByNameAsString('UNITS')) = 'WKT' then begin
       MDDef.LidarGridProjection := WKTDEM;
       MDdef.DefWKTGridSize := aTable.GetFieldByNameAsFloat('X_RES');
       MDdef.WKTLidarProj := aTable.GetFieldByNameAsString('WKT');
   end;
   if Uppercase(aTable.GetFieldByNameAsString('PIXEL_IS')) = 'AREA' then MDDef.LasDEMPixelIs := 1
   else MDDef.LasDEMPixelIs := 2;

   if Uppercase(aTable.GetFieldByNameAsString('DEM')) = 'DSM' then begin
      MDDef.DSMpercentile := aTable.GetFieldByNameAsFloat('PERCENTILE');
   end;
   if Uppercase(aTable.GetFieldByNameAsString('DEM')) = 'MID' then begin
      MDDef.MIDpercentile := aTable.GetFieldByNameAsFloat('PERCENTILE');
   end;
   if Uppercase(aTable.GetFieldByNameAsString('DEM')) = 'DTM' then begin
      MDDef.DTMpercentile := aTable.GetFieldByNameAsFloat('PERCENTILE');
   end;
end;


procedure Tpt_cloud_opts_fm.BitBtn50Click(Sender: TObject);
var
   i,NewDEM : integer;
   NewName,StartTime : shortstring;
   aTable : Petmar_db.tMyData;
begin
   {$If Defined(RecordNewGrids) or Defined(BulkGrids)} WriteLineToDebugFile(''); WriteLineToDebugFile('Make multiple pass grids,  start'); {$EndIf}
   if Not FileExists(DEMrulesFName) then begin
      DEMrulesFName := ProgramRootDir;
      Petmar.GetExistingFileName('DEM creation parameters','*.dbf',DEMrulesFName);
   end;
   if FileExists(DEMrulesFName) then begin
      If (AutoSaveDir = '') then GetDosPath('saved grids',AutoSaveDir);
      aTable := tMyData.Create(DEMrulesFName);
      StartTime := TimeToStr(Now);
      i := 0;
      while not aTable.eof do begin
         inc(i);
         StatusBarShortMess := StartTime + '  ' + IntToStr(i) + '/' + IntToStr(aTable.RecordCount);
         SetGridFromTable(atable);
         NewName := TheCloudName + '_' + aTable.GetFieldByNameAsString('FILE_NAME') + '.tif';
         wmDEM.SetPanelText(0,NewName);
         NewName := AutoSaveDir + NewName;
         if FileExists(NewName) then begin
            {$If Defined(RecordNewGrids) or Defined(BulkGrids)} WriteLineToDebugFile('Already exists, ' + NewName); {$EndIf}
         end
         else begin
           {$If Defined(RecordNewGrids) or Defined(BulkGrids)} WriteLineToDebugFile('Start ' + NewName); {$EndIf}
            if Uppercase(aTable.GetFieldByNameAsString('DEM')) = 'DSM' then begin
               NewDEM := CreateGridOrDBfromStats(dbpcDSM,MDDef.DSMpercentile,NewName,false);
            end;
            if Uppercase(aTable.GetFieldByNameAsString('DEM')) = 'MID' then begin
               NewDEM := CreateGridOrDBfromStats(dbpcMidCloud,MDDef.MIDpercentile,NewName,false);
            end;
            if Uppercase(aTable.GetFieldByNameAsString('DEM')) = 'DTM' then begin
               NewDEM := CreateGridOrDBfromStats(dbpcDTM,MDDef.DTMpercentile,NewName,false);
            end;
         end;
         aTable.Next;
      end;
      aTable.Destroy;
   end;
   wmDEM.SetPanelText(0,'');
   {$If Defined(RecordNewGrids) or Defined(BulkGrids)} WriteLineToDebugFile('Make multiple pass grids,  done'); {$EndIf}
end;



procedure Tpt_cloud_opts_fm.BitBtn51Click(Sender: TObject);
begin
   PopUpMenu3.Popup(Mouse.CursorPos.X,Mouse.CursorPos.Y);
end;

procedure Tpt_cloud_opts_fm.LasClick(Sender: TObject);
var
   cmd : ShortString;
   fName,pname : PathStr;
begin
   fName := LasFiles[1].LAS_fnames.Strings[0];
   pname := 'lasview.exe';
   if GetLASToolsFileName(pName) then begin
      ExecuteFile(pName + ' ' + fName);
   end;
end;

procedure Tpt_cloud_opts_fm.LasSubset(MergeLasWhat : tMergeLas; SubsetName,SubsetWhat : shortstring);
var
   fName : PathStr;
begin
   if FileExtEquals(LasFiles[1].LAS_fnames.Strings[0], '.LAS') then begin
      fName := LasFiles[1].LAS_fnames.Strings[0];
      fName := ExtractFilePath(fname) + ExtractFileNameNoExt(fName) + SubsetName + '.las';
      Petmar.GetFileNameDefaultExt(SubsetWhat,'*.las',fName,false);
      LasFiles[1].ShowLASProgress := true;
      LasFiles[1].MergeLasPoints(MergeLasWhat,BaseMap,fName,nil);
   end;
end;


procedure Tpt_cloud_opts_fm.Lowpointsxyz1Click(Sender: TObject);
begin
   MakeGrid(pcgmGroundLowXYZ);
end;

procedure Tpt_cloud_opts_fm.Lowpointxyz1Click(Sender: TObject);
begin
   MakeGrid(pcgmLowXYZ);
end;

procedure Tpt_cloud_opts_fm.BitBtn52Click(Sender: TObject);
begin
   ReadDefault ('Thinning factor',LasFiles[1].LasExportThinFactor);
   LasSubset(mlThin, '_thin_' + IntToStr(LasFiles[1].LasExportThinFactor),'thinned LAS file');
end;

procedure Tpt_cloud_opts_fm.BitBtn53Click(Sender: TObject);
begin
   ReadDefault('TranslateX',LasFiles[1].TranslateX);
   ReadDefault('TranslateY',LasFiles[1].TranslateY);
   ReadDefault('TranslateZ',LasFiles[1].TranslateZ);
   LasSubset(mlTranslate, '_translated','translated LAS file');
end;

procedure Tpt_cloud_opts_fm.BitBtn54Click(Sender: TObject);
begin
   ReadDefault('Scaling factor',LasFiles[1].ScaleUp);
   LasSubset(mlScaleUp, '_scaled_up','scaled up LAS file');
end;

procedure Tpt_cloud_opts_fm.BitBtn55Click(Sender: TObject);
begin
   Color_filter.GetColorFilter;
   LasSubset(mlRGBFilter, '_rgb_filtered','LAS file with RGB filte');
end;

procedure Tpt_cloud_opts_fm.BitBtn56Click(Sender: TObject);
begin
   if (BaseMap.MapDraw.DEMonMap <> 0) then begin
      Edit5.Text := realToString(BaseMap.MapDraw.MaxMapElev,-12,-2);
      Edit6.Text := realToString(BaseMap.MapDraw.MinMapElev,-12,-2);
   end;
end;

procedure Tpt_cloud_opts_fm.BitBtn57Click(Sender: TObject);
begin
   RGB1.Visible := HasRGB;
   PopUpMenu4.Popup(Mouse.CursorPos.X,Mouse.CursorPos.Y);
end;

procedure Tpt_cloud_opts_fm.BitBtn58Click(Sender: TObject);
begin
   BaseMap.MakeTempGrid(true);
end;

procedure Tpt_cloud_opts_fm.BitBtn59Click(Sender: TObject);
begin
   pt_cloud_opts_fm_Close;
end;

procedure Tpt_cloud_opts_fm.BitBtn5Click(Sender: TObject);
begin
   GetFilesForPointCloud(1,LastLidarDirectory);
end;

procedure Tpt_cloud_opts_fm.BitBtn60Click(Sender: TObject);
var
   Cloud : integer;
begin
   for Cloud := MaxClouds downto 1 do begin
      if UsePC[Cloud] and (LasFiles[Cloud] <> Nil) then begin
        SysUtils.DeleteFile(LasFiles[Cloud].IndexTableName);
        LasFiles[Cloud].CloudStatistics(true,csLoadStats,true,false);
      end;
   end;
end;

procedure Tpt_cloud_opts_fm.BitBtn61Click(Sender: TObject);
var
   Cloud,Tile : integer;
   NewDir : PathStr;
begin
   StartProgress('Copy files');
   for Cloud := 1 to MaxClouds do if UsePC[Cloud] and (LasFiles[Cloud] <> Nil) then begin
      UpdateProgressBar(Cloud/MaxClouds);
      GetDOSPath('path for cloud ' + LasFiles[Cloud].CloudName,NewDir);

      for Tile := 0 to pred(LasFiles[Cloud].LAS_fnames.Count) do begin
         CopyFile(LasFiles[Cloud].LAS_fnames.Strings[Tile],NewDir + ExtractFileName(LasFiles[Cloud].LAS_fnames.Strings[Tile]));
      end;
   end;
   EndProgress;
end;

procedure Tpt_cloud_opts_fm.BitBtn62Click(Sender: TObject);
var
   Intensities : tstringlist;
   cloud : integer;
   fName : PathStr;
begin
   Intensities := tstringlist.Create;
   Intensities.Add('CLOUD,MIN_INTENS,INTENS_1,INTENS_99,MAX_INTENS');
   for Cloud := 1 to MaxClouds do if (LasFiles[Cloud] <> Nil) then begin
      Intensities.Add(LasFiles[Cloud].CloudName + ',' + IntToStr(LasFiles[Cloud].MIN_INTEN) + ',' + IntToStr(LasFiles[Cloud].INTEN_1) + ',' + IntToStr(LasFiles[Cloud].INTEN_99) + ',' + IntToStr(LasFiles[Cloud].MAX_INTEN));
   end;
   fName := NextFileNumber(MDTempDir,'Lidar_intensity_','.dbf');
   StringList2CSVtoDB(Intensities,fName);
end;

procedure Tpt_cloud_opts_fm.BitBtn63Click(Sender: TObject);
begin
   //if (not UsePC[1]) then CheckBoxPC1Click(Sender);
   (*
   if (not CheckBoxPC2.Checked) then CheckBoxPC2Click(Sender);
   if (not CheckBoxPC3.Checked) then CheckBoxPC3Click(Sender);
   if (not CheckBoxPC4.Checked) then CheckBoxPC4Click(Sender);
   if (not CheckBoxPC5.Checked) then CheckBoxPC5Click(Sender);
   *)
end;

procedure Tpt_cloud_opts_fm.BitBtn64Click(Sender: TObject);
begin
   if (not CheckBoxPC1.Checked) then CheckBoxPC1.Checked := true;
   if (not CheckBoxPC2.Checked) then CheckBoxPC2.Checked := true;
   if (not CheckBoxPC3.Checked) then CheckBoxPC3.Checked := true;
   if (not CheckBoxPC4.Checked) then CheckBoxPC4.Checked := true;
   if (not CheckBoxPC5.Checked) then CheckBoxPC5.Checked := true;
end;

procedure Tpt_cloud_opts_fm.BitBtn6Click(Sender: TObject);
begin
   {$If Defined(PointCloudMap) or Defined(TrackPointCloud)} WriteLineToDebugFile('Tpt_cloud_opts_fm.BitBtn6Click in'); {$EndIf}
   SaveBackupDefaults;
   MDdef.AutoRedrawMapLAS := true;
   BaseMap.MapDraw.DeleteMapSavedLasLayers;
   RedrawAllLayers;
   RestoreBackupDefaults;
   {$If Defined(PointCloudMap) or Defined(TrackPointCloud)} WriteLineToDebugFile('Tpt_cloud_opts_fm.BitBtn6Click out'); {$EndIf}
end;


procedure Tpt_cloud_opts_fm.BitBtn7Click(Sender: TObject);
begin
   BaseMap.OutlineGridOutlines(-999,-999);
end;


function CreateNewUTMGrid(DEMName : shortstring; bb : sfBoundBox; Spacing : float64) : integer;
var
   NewHeader : tDEMheader;
begin
   {$IfDef RecordMakeBaseMap} WriteLineToDebugFile('CreateNewUTMGrid in, UTM=' + IntToStr(MDDef.DefaultUTMZone) + MDDef.DefaultLatHemi + '  bb: ' + sfBoundBoxToString(bb,4)); {$EndIf}
   Result := 0;
   ZeroDEMHeader(NewHeader, true);
   NewHeader.DEMxSpacing := Spacing;
   NewHeader.DEMySpacing := Spacing;
   NewHeader.DEMSWCornerX := bb.XMin;
   NewHeader.DEMSWCornerY := bb.YMin;
   NewHeader.NumCol := round((bb.xmax - bb.xmin) / Spacing);
   NewHeader.NumRow := round((bb.ymax - bb.ymin) / Spacing);

   OpenAndZeroNewDEM(false,NewHeader,Result,DEMName,InitDEMmissing);
   {$IfDef RecordMakeBaseMap} WriteLineToDebugFile('CreateNewUTMGrid created, OpenAndZero ' + sfBoundBoxToString(DEMGlb[Result].DEMBoundBoxGeo,4)); {$EndIf}
   {$IfDef RRecordMakeBaseMap} WriteLineToDebugFile('CreateNewUTMGrid ' + DEMGlb[Result].FullDEMParams); {$EndIf}

   CreateDEMSelectionMap(Result,false,MDDef.DefElevsPercentile,mtDEMBlank);
   {$IfDef RecordMakeBaseMap} WriteLineToDebugFile('CreateNewUTMGrid out, map grid box:' + sfBoundBoxToString(DEMGlb[Result].SelectionMap.MapDraw.MapCorners.BoundBoxDataGrid,2)); {$EndIf}
   {$IfDef RecordMakeBaseMap} WriteLineToDebugFile('CreateNewUTMGrid out, map geo box:' + sfBoundBoxToString(DEMGlb[Result].SelectionMap.MapDraw.MapCorners.BoundBoxGeo,4)); {$EndIf}
end;



function Tpt_cloud_opts_fm.GetFilesForPointCloud(CloudNum : integer; var BaseDir : PathStr; AutoLoad : boolean = false) : boolean;
var
   DefFilter : byte;
   width,height,Aspect,DesiredAspect,NewHeight,NewWidth : float32;
   DEMbase,i : integer;
   Files : tStringList;
   MakeBox : sfBoundBox;
   fName : PathStr;
begin
   {$If Defined(BasicOpens) or Defined(RecordPointCloudOptionsForm) or Defined(RecordLASOpen)  or Defined(TrackPointCloud)} WriteLineToDebugFile('Tpt_cloud_opts_fm.GetFilesForPointCloud in, cloud=' + IntToStr(CloudNum)); {$EndIf}
   if (LasFiles[CloudNum] = Nil) then LasFiles[CloudNum] := tLas_files.Create
   else LasFiles[CloudNum].LAS_fnames.Clear;

   Files := Nil;
   Petmar.FindMatchingFiles(MDTempDir,'cloud_' + IntToStr(CloudNum) + '*.bmp',Files);
   for i := 0 to pred(Files.Count) do DeleteFile(Files[i]);
   Files.Free;

   if AutoLoad and ValidPath(BaseDir) then begin
      Petmar.FindMatchingFiles(BaseDir,'*.las',LasFiles[CloudNum].LAS_fnames,6);
   end
   else begin
      if MDDef.PickLASDirs then begin
         Petmar.GetDOSPath('LAS files',BaseDir);
         {$If Defined(RecordPointCloudOptionsForm) or Defined(RecordLASOpen)} WriteLineToDebugFile('Human delay, Finding files in ' + BaseDir); {$EndIf}
         Petmar.FindMatchingFiles(BaseDir,'*.las',LasFiles[CloudNum].LAS_fnames,6);
      end
      else begin
         DefFilter := 1;
         LasFiles[CloudNum].LAS_fnames.Add(BaseDir);
         if not Petmar.GetMultipleFiles('Lidar files','Reasonable files|*.LAS;*.shp|LAS files|*.las|3d shapefiles|*.shp',LasFiles[CloudNum].LAS_fnames,DefFilter) then exit;
         {$If Defined(RecordPointCloudOptionsForm) or Defined(RecordLASOpen)} WriteLineToDebugFile('Human delay, Finding files='+ IntToStr(LasFiles[CloudNum].LAS_fnames.Count)); {$EndIf}
      end;
      LasFiles[CloudNum].CloudDir := BaseDir;
   end;

   for i := pred(Files.Count) downto 0 do begin
       fName := LasFiles[CloudNum].LAS_fnames.Strings[i];
       if (UpperCase(ExtractFileExt(fName)) = '.LAZ') then begin
           MessageToContinue('Decompress LAZ file before use, '+ ExtractFileName(fName));
           LasFiles[CloudNum].LAS_fnames.Delete(i);
       end;
   end;


   {$If Defined(BasicOpens) or Defined(RecordPointCloudOptionsForm) or Defined(RecordLASOpen)} WriteLineToDebugFile('Files found ' + IntToStr(LasFiles[CloudNum].LAS_fnames.Count)); {$EndIf}
   Result := (LasFiles[CloudNum].LAS_fnames.Count > 0);
   if Result then begin
      ShowHourglassCursor;
      BaseDir := ExtractFilePath(LasFiles[CloudNum].LAS_fnames.Strings[0]);
      LasFiles[CloudNum].CloudName := LastSubDir(BaseDir);
      LasFiles[CloudNum].FindCloudExtent;
      {$If Defined(RecordPointCloudOptionsForm) or Defined(RecordLASOpen)} WriteLineToDebugFile('call CloudStatistics'); {$EndIf}
      LasFiles[CloudNum].CloudStatistics(true,csLoadStats,true,false);

      if (BaseMap = Nil) then begin
         MDDef.DefaultUTMZone := LasFiles[CloudNum].UTMZone;
         MDDef.DefaultLatHemi := LasFiles[CloudNum].LatHemi;
         {$If Defined(RecordPointCloudOptionsForm) or Defined(RecordLASOpen)}
            WriteLineToDebugFile('GetFiles Create cloud basemap, UTM=' + IntToStr(MDDef.DefaultUTMZone) + MDDef.DefaultLatHemi);
            WriteLineToDebugFile('GetFile CreateNewGrid, cloud utm box:  ' + sfBoundBoxToString(LasFiles[CloudNum].UTMBBox,1));
            WriteLineToDebugFile('GetFile CreateNewGrid, cloud geo box:  ' + sfBoundBoxToString(LasFiles[CloudNum].GeoBBox,6));
         {$EndIf}

         MakeBox := LasFiles[CloudNum].UTMBBox;

         Width := MakeBox.XMax - MakeBox.XMin;
         Height := MakeBox.yMax - MakeBox.yMin;
         Aspect := Height / Width;
         DesiredAspect := MDDef.DefaultMapYSize / MDDef.DefaultMapXSize;
         if (Aspect < DesiredAspect) then begin
            NewWidth := DesiredAspect * Height;
            NewHeight := Height;
            MakeBox.XMax := MakeBox.XMax + 0.5 * (NewWidth-Width);
            MakeBox.XMin := MakeBox.XMin - 0.5 * (NewWidth-Width);
         end
         else begin
            NewHeight := Width / DesiredAspect;
            NewWidth := Width;
            MakeBox.YMax := MakeBox.YMax + 0.5 * (NewHeight-Height);
            MakeBox.YMin := MakeBox.YMin - 0.5 * (NewHeight-Height);
         end;

         {$If Defined(RecordPointCloudOptionsForm) or Defined(RecordLASOpen)}
            WriteLineToDebugFile('GetFile CreateNewGrid, utm Make box:  ' + sfBoundBoxToString(MakeBox,1));
            WriteLineToDebugFile('utm resizing, ' + RealToString(Width,-12,-1) + 'x' + RealToString(Height,-12,-1) + ' to ' + RealToString(NewWidth,-12,-1) + 'x' + RealToString(NewHeight,-12,-1));
         {$EndIf}

         DEMBase := CreateNewUTMGrid(LasFiles[CloudNum].CloudName + '_cloud',{cgUTM,}MakeBox,{FloatingPointDEM,}2);
         BaseMap := DEMGlb[DemBase].SelectionMap;

         {$If Defined(RecordMakeBaseMap)}
            WriteLineToDebugFile('Cloud Basemap UTM box:' + sfBoundBoxToString(BaseMap.MapDraw.MapCorners.BoundBoxUTM,1) );
            WriteLineToDebugFile('Cloud Basemap geo box:' + sfBoundBoxToString(BaseMap.MapDraw.MapCorners.BoundBoxGeo,6) );
            WriteLineToDebugFile('Cloud Basemap projected box:' + sfBoundBoxToString(BaseMap.MapDraw.MapCorners.BoundBoxProj,6) +
               ' dx=' + RealToString(BaseMap.MapDraw.MapCorners.ProjDX,-12,-6) + ' dy=' + RealToString(BaseMap.MapDraw.MapCorners.ProjDY,-12,-6));
         {$EndIf}

         BaseMap.MapDraw.DrawLegendsThisMap := false;
         BaseMap.MapDraw.LasLayerOnMap := true;
         BaseMap.Closable := true;
         BaseMap.PointCloudBase := true;
         BitBtn56.Visible := true;
      end
      else if MDDef.CheckLasOnMap then begin
         {$If Defined(RecordPointCloudOptionsForm) or Defined(RecordLASOpen)} WriteLineToDebugFile('Call Remove tiles not on map'); {$EndIf}
         LasFiles[CloudNum].RemoveTilesNotOnMap(BaseMap.MapDraw);
         if (BaseMap.MapDraw.ScreenPixelSize > 100) then ZoomToCloudCoverage(1);
         {$If Defined(RecordPointCloudOptionsForm) or Defined(RecordLASOpen)} WriteLineToDebugFile('Removed tiles not on map, files=' + IntToStr(LasFiles[CloudNum].Las_fnames.count)); {$EndIf}
         Result := (LasFiles[CloudNum].Las_fnames.count > 0);
      end;

      if Result then begin
         ResyncFileList;
         if FirstLoad and MDdef.AutoZoomOpenLAS and CanAutoZoom then begin
            {$If Defined(RecordPointCloudOptionsForm) or Defined(RecordLASOpen)} WriteLineToDebugFile('GetFile call zoomCloudCoverage'); {$EndIf}
            ZoomToCloudCoverage(1);
            FirstLoad := false;
         end
         else begin
            {$If Defined(RecordPointCloudOptionsForm) or Defined(RecordLASOpen)} WriteLineToDebugFile('GetFile call outline clouds'); {$EndIf}
            OutLineClouds;
         end;

         if (CloudNum = 1) then begin
            {$If Defined(RecordPointCloudOptionsForm) or Defined(RecordLASOpen)} WriteLineToDebugFile('cloudnum=1'); {$EndIf}
            CheckBoxPC1.Caption := LasFiles[CloudNum].CloudName;
            {$If Defined(RecordPointCloudOptionsForm) or Defined(RecordLASOpen)} WriteLineToDebugFile('got stats'); {$EndIf}
             if (LasFiles[CloudNum].UTMZone > 0) then begin
                MDdef.DefaultUTMZone := LasFiles[CloudNum].UTMZone;
                Edit19.Text := IntToStr(LasFiles[CloudNum].UTMZone);
             end;
             if LasFiles[CloudNum].HasRGB then begin
                Edit23.Text := IntToStr(LasFiles[CloudNum].Max_Red);
                Edit4.Text := IntToStr(LasFiles[CloudNum].Min_Red);
             end;
             Edit5.Text := realToString(LasFiles[CloudNum].MaxZ,-12,-1);
             Edit6.Text := realToString(LasFiles[CloudNum].MinZ,-12,-1);

             if MDDef.LASPC99 then begin
                Edit33.Text := IntToStr(LasFiles[CloudNum].Inten_99);
                Edit34.Text := IntToStr(LasFiles[CloudNum].Inten_1);
             end
             else begin
                Edit33.Text := IntToStr(LasFiles[CloudNum].Max_Inten);
                Edit34.Text := IntToStr(LasFiles[CloudNum].Min_Inten);
             end;

             CheckBoxPC1.Checked := InitialCloudDisplay;
             CheckBoxPC1.Visible := true;
             SymbolPC1.Visible := true;
             SymbolOnbutton(SymbolPC1,MDDef.CloudMapSymbol[1]);
             BaseMap.MapDraw.BaseTitle := 'Cloud: ' + LasFiles[CloudNum].CloudName;
             BaseMap.Caption := BaseMap.MapDraw.BaseTitle;
             Caption := 'Point clouds on ' + BaseMap.Caption;
         end
         else if (CloudNum = 2) then begin
            CheckBoxPC2.Caption := LasFiles[CloudNum].CloudName;
            CheckBoxPC2.Checked := InitialCloudDisplay;
            CheckBoxPC2.Visible := true;
            SymbolPC2.Visible := true;
            SymbolOnbutton(SymbolPC2,MDDef.CloudMapSymbol[2]);
         end
         else if (CloudNum = 3) then  begin
            CheckBoxPC3.Caption := LasFiles[CloudNum].CloudName;
            CheckBoxPC3.Checked := InitialCloudDisplay;
            CheckBoxPC3.Visible := true;
            SymbolPC3.Visible := true;
            SymbolOnbutton(SymbolPC3,MDDef.CloudMapSymbol[3]);
         end
         else if (CloudNum = 4) then  begin
            CheckBoxPC4.Caption := LasFiles[CloudNum].CloudName;
            CheckBoxPC4.Checked := InitialCloudDisplay;
            CheckBoxPC4.Visible := true;
            SymbolPC4.Visible := true;
            SymbolOnbutton(SymbolPC4,MDDef.CloudMapSymbol[4]);
         end
         else if (CloudNum = 5) then  begin
            CheckBoxPC5.Caption := LasFiles[CloudNum].CloudName;
            CheckBoxPC5.Checked := InitialCloudDisplay;
            CheckBoxPC5.Visible := true;
            SymbolPC5.Visible := true;
            SymbolOnbutton(SymbolPC5,MDDef.CloudMapSymbol[5]);
         end;
         UsePC[CloudNum] := InitialCloudDisplay;

         Result := true;
         NeedNewElevationLimits := true;
         UpdateColorOptions;
      end;
   end;

   ShowDefaultCursor;
   {$If Defined(BasicOpens) or Defined(RecordPointCloudOptionsForm) or Defined(RecordLASOpen) or Defined(TrackPointCloud)} WriteLineToDebugFile('Tpt_cloud_opts_fm.GetFilesForPointCloud out, ' + LasFiles[CloudNum].CloudStats); {$EndIf}
end;


procedure Tpt_cloud_opts_fm.GroundoverDTM1Click(Sender: TObject);
begin
   MakeGrid(pcgmGround);
end;


procedure Tpt_cloud_opts_fm.ResyncFileList;

      function TheCloudStats(WhichCloud : integer) : shortstring;
      begin
          if (LasFiles[WhichCloud] <> Nil) and (LasFiles[WhichCloud].LAS_fnames.Count > 0) then Result := LasFiles[WhichCloud].CloudStats
          else Result := 'Cloud ' + IntToStr(WhichCloud) + ': not loaded';
      end;

begin
   {$IfDef RecordPointCloudOptionsForm} WriteLineToDebugFile('Tpt_cloud_opts_fm.ResyncFileList in'); {$EndIf}
   Label8.Caption := TheCloudStats(1);
   Label9.Caption := TheCloudStats(2);
   Label11.Caption := TheCloudStats(3);
   Label12.Caption := TheCloudStats(4);
   Label23.Caption := TheCloudStats(5);

   BitBtn4.Enabled := (LasFiles[1] <> Nil) and (LasFiles[1].LAS_fnames.Count > 0);
   BitBtn8.Enabled := BitBtn4.Enabled;
   BitBtn14.Enabled := BitBtn4.Enabled;
   BitBtn15.Enabled := BitBtn4.Enabled;
   BitBtn17.Enabled := BitBtn4.Enabled;
   BitBtn19.Enabled := BitBtn4.Enabled;
   {$IfDef RecordPointCloudOptionsForm} WriteLineToDebugFile('Tpt_cloud_opts_fm.ResyncFileList out'); {$EndIf}
end;


procedure Tpt_cloud_opts_fm.Returndensityvoxcels1Click(Sender: TObject);
begin
   MakeGrid(pcgmDensityVox);
end;

procedure Tpt_cloud_opts_fm.Returnintensity1Click(Sender: TObject);
begin
    MakeGrid(pcgmAllIntensity);
end;

procedure Tpt_cloud_opts_fm.RGB1Click(Sender: TObject);
begin
   MakeGrid(pcgmRGB);
end;

procedure Tpt_cloud_opts_fm.BitBtn9Click(Sender: TObject);
begin
   RunProcess(wpFusionClass);
end;

procedure Tpt_cloud_opts_fm.Blankgrid1Click(Sender: TObject);
begin
   MakeGrid(pcgmBlank);
end;

procedure Tpt_cloud_opts_fm.blast2dem1Click(Sender: TObject);
var
   i : integer;
   LasName,DTMName : PathStr;
begin
    for i := 1 to MaxClouds do if UsePC[i] and (LasFiles[i] <> Nil) then begin
       LasName := NextFileNumber(MDtempDir, 'wbidw_dtm_','.las');
       LasFiles[i].ExtractGroundPoints(LasName);
       DTMname := NextFileNumber(MdtempDir, LasFiles[i].CloudName + '_wbidw_dtm_','.tif');
       WBT_IDWCreate(true,LasName,DTMName,MDdef.DefLidarXGridSize);
    end;
end;

procedure Tpt_cloud_opts_fm.Byclass1Click(Sender: TObject);
begin
   MakeGrid(pcgmClass);
end;

procedure Tpt_cloud_opts_fm.CancelBtnClick(Sender: TObject);
begin
   pt_cloud_opts_fm_Close;
end;


procedure Tpt_cloud_opts_fm.Ceiling1Click(Sender: TObject);
begin
   MDDef.MakePCFloor := false;
   MDDef.MakePCCeiling := true;
   MakeGrid(pcgmCeilFloor);
end;

procedure Tpt_cloud_opts_fm.Ceilingfloor1Click(Sender: TObject);
begin
   MDDef.MakePCFloor := true;
   MDDef.MakePCCeiling := true;
   MakeGrid(pcgmCeilFloor);
end;

procedure Tpt_cloud_opts_fm.CheckBox10Click(Sender: TObject);
begin
   MDDef.ls.AirReturnsOnly := CheckBox10.Checked;
end;

procedure Tpt_cloud_opts_fm.CheckBox11Click(Sender: TObject);
begin
   MDDef.ls.filters := CheckBox11.Checked;
   GroupBox6.Enabled := MDDef.ls.Filters;
end;

procedure Tpt_cloud_opts_fm.CheckBox12Click(Sender: TObject);
begin
   MDDef.ls.FirstReturnsOnly := CheckBox12.Checked;
end;

procedure Tpt_cloud_opts_fm.CheckBox13Click(Sender: TObject);
begin
   MDDef.ls.LastReturnsOnly := CheckBox13.Checked;
end;

procedure Tpt_cloud_opts_fm.CheckBox14Click(Sender: TObject);
begin
   MDDef.ShowCloudOutlines := CheckBox14.Checked;
end;

procedure Tpt_cloud_opts_fm.CheckBox15Click(Sender: TObject);
begin
   MDDef.ls.PointIDFiltered := CheckBox15.Checked;
   Edit16.Enabled := CheckBox15.Checked;
end;

procedure Tpt_cloud_opts_fm.CheckBox16Click(Sender: TObject);
begin
   MDdef.AutoZoomOpenLAS := CheckBox16.Checked;
end;

procedure Tpt_cloud_opts_fm.CheckBox17Click(Sender: TObject);
begin
   MDDef.ls.DiscardOverlap := CheckBox17.Checked;
end;

procedure Tpt_cloud_opts_fm.CheckBox18Click(Sender: TObject);
begin
   MDDef.LasElevChecks := CheckBox18.Checked;
end;

procedure Tpt_cloud_opts_fm.CheckBox19Click(Sender: TObject);
begin
   MDDef.LasLegend := CheckBox19.Checked;
end;

procedure Tpt_cloud_opts_fm.CheckBox1Click(Sender: TObject);
begin
   Edit1.Enabled := CheckBox1.Checked;
end;

procedure Tpt_cloud_opts_fm.CheckBox20Click(Sender: TObject);
begin
   MDDef.ls.UserDataRecordFiltered := CheckBox20.Checked;
   Edit20.Enabled := CheckBox20.Checked;
end;

procedure Tpt_cloud_opts_fm.CheckBox21Click(Sender: TObject);
begin
   MDDef.LogDosOutput := CheckBox21.Checked;
end;

procedure Tpt_cloud_opts_fm.CheckBox22Click(Sender: TObject);
begin
   MDDef.ls.GroundClassOnly := CheckBox22.Checked;
end;

procedure Tpt_cloud_opts_fm.CheckBox23Click(Sender: TObject);
begin
   MDDef.ls.SimpleBuildingFilter := CheckBox23.Checked;
end;

procedure Tpt_cloud_opts_fm.CheckBox24Click(Sender: TObject);
begin
   MDdef.AutoDrawMapLAS := CheckBox24.Checked;
end;

procedure Tpt_cloud_opts_fm.CheckBox25Click(Sender: TObject);
begin
   MDDef.LASPC99  := Checkbox25.Checked;
end;

procedure Tpt_cloud_opts_fm.CheckBox26Click(Sender: TObject);
begin
   MDDef.UseOverlap := CheckBox26.Checked;
end;

procedure Tpt_cloud_opts_fm.CheckBox27Click(Sender: TObject);
begin
   MDDef.ls.DiscardHighNoise := CheckBox27.Checked;
end;

procedure Tpt_cloud_opts_fm.CheckBox28Click(Sender: TObject);
begin
    MDDef.ls.AssumeLAS14classes := CheckBox28.Checked;
end;

procedure Tpt_cloud_opts_fm.CheckBox29Click(Sender: TObject);
begin
   MDDef.LatLongSlices := CheckBox29.Checked;
end;

procedure Tpt_cloud_opts_fm.CheckBox2Click(Sender: TObject);
begin
   MDDef.UseNoise := CheckBox2.Checked;
end;

procedure Tpt_cloud_opts_fm.UncheckAll(setting : boolean = false);
var
   i : integer;
begin
   CheckBoxPC1.Checked := Setting;
   CheckBoxPC2.Checked := Setting;
   CheckBoxPC3.Checked := Setting;
   CheckBoxPC4.Checked := Setting;
   CheckBoxPC5.Checked := Setting;
   SymbolPC1.Visible := Setting;
   SymbolPC2.Visible := Setting;
   SymbolPC3.Visible := Setting;
   SymbolPC4.Visible := Setting;
   SymbolPC5.Visible := Setting;
   for i := 1 to MaxClouds do UsePC[i] := Setting;
   if MDdef.AutoReDrawMapLAS then RedrawAllLayers;
end;


procedure Tpt_cloud_opts_fm.CheckBox30Click(Sender: TObject);
begin
   MDdef.AutoRedrawMapLAS := CheckBox30.Checked;
end;

procedure Tpt_cloud_opts_fm.CheckBox31Click(Sender: TObject);
begin
   ShowMeanDensityGrid := CheckBox31.Checked;
end;

procedure Tpt_cloud_opts_fm.CheckBox32Click(Sender: TObject);
begin
   MDDef.AutoThinByBlock := CheckBox32.Checked;
end;

procedure Tpt_cloud_opts_fm.CheckBox34Click(Sender: TObject);
begin
   MdDef.ForceSquarePixels := CheckBox34.Checked;
   Edit25.Enabled := not MdDef.ForceSquarePixels;
   Edit32.Enabled := not MdDef.ForceSquarePixels;
   Label20.Enabled := not MdDef.ForceSquarePixels;
   Label33.Enabled := not MdDef.ForceSquarePixels;
end;

procedure Tpt_cloud_opts_fm.CheckBox3Click(Sender: TObject);
begin
   MDDef.SlicerIHSMerge := CheckBox3.Checked;
   Label17.Enabled := MDDef.SlicerIHSMerge;
   Edit18.Enabled := MDDef.SlicerIHSMerge;
   if MDdef.AutoReDrawMapLAS then RedrawAllLayers;
end;

procedure Tpt_cloud_opts_fm.CheckBox4Click(Sender: TObject);
begin
   Edit7.Enabled := CheckBox4.Checked;
end;

procedure Tpt_cloud_opts_fm.CheckBox5Click(Sender: TObject);
begin
   MDDef.ls.ScanAngleFiltered := CheckBox5.Checked;
end;

procedure Tpt_cloud_opts_fm.CheckBox6Click(Sender: TObject);
begin
   MDDef.CheckLasOnMap := CheckBox6.Checked;
end;

procedure Tpt_cloud_opts_fm.CheckBox7Click(Sender: TObject);
begin
   MDDef.LabelLAStiles := CheckBox7.Checked;
end;

procedure Tpt_cloud_opts_fm.CheckBox8Click(Sender: TObject);
begin
   MDDef.LasAutoThin := CheckBox8.Checked;
end;

procedure Tpt_cloud_opts_fm.CheckBox95Click(Sender: TObject);
begin
   MDDef.ls.DiscardLowPointsNoise := CheckBox95.Checked;
end;

procedure Tpt_cloud_opts_fm.CheckBox99Click(Sender: TObject);
begin
   MDDef.ls.SingleReturnsOnly := CheckBox99.Checked;
end;

procedure Tpt_cloud_opts_fm.CheckBox9Click(Sender: TObject);
begin
   MDDef.PCAutoFillHoles := CheckBox9.Checked;
   Label22.Enabled := MDDef.PCAutoFillHoles;
   Edit24.Enabled := MDDef.PCAutoFillHoles;
end;

procedure Tpt_cloud_opts_fm.CheckBoxPC1Click(Sender: TObject);
begin
   UsePC[1] := CheckBoxPC1.Checked;
   UpdateColorOptions;
   if MDdef.AutoReDrawMapLAS then RedrawAllLayers;
end;

procedure Tpt_cloud_opts_fm.CheckBoxPC2Click(Sender: TObject);
begin
   UsePC[2] := CheckBoxPC2.Checked;
   UpdateColorOptions;
   if MDdef.AutoReDrawMapLAS then RedrawAllLayers;
end;

procedure Tpt_cloud_opts_fm.CheckBoxPC3Click(Sender: TObject);
begin
   UsePC[3] := CheckBoxPC3.Checked;
   UpdateColorOptions;
   if MDdef.AutoReDrawMapLAS then RedrawAllLayers;
end;

procedure Tpt_cloud_opts_fm.CheckBoxPC4Click(Sender: TObject);
begin
   UsePC[4] := CheckBoxPC4.Checked;
   UpdateColorOptions;
   if MDdef.AutoReDrawMapLAS then RedrawAllLayers;
end;

procedure Tpt_cloud_opts_fm.CheckBoxPC5Click(Sender: TObject);
begin
   UsePC[5] := CheckBoxPC5.Checked;
   UpdateColorOptions;
   if MDdef.AutoReDrawMapLAS then RedrawAllLayers;
end;

procedure Tpt_cloud_opts_fm.Classification1Click(Sender: TObject);
begin
   MakeGrid(pcgmClassification);
end;

procedure Tpt_cloud_opts_fm.DistanceabovebelowDEM1Click(Sender: TObject);
begin
   MakeGrid(pcgmAboveBelow);
end;

procedure Tpt_cloud_opts_fm.DSMNVSDTM1Click(Sender: TObject);
begin
   MDDef.MakePCFloor := true;
   MDDef.MakePCCeiling := true;
   MakeGrid(pcgmCeilFloor);
   MakeGrid(pcgmGrndPtDTM);
end;

procedure Tpt_cloud_opts_fm.DTMfromgroundclassifiedpoints1Click(Sender: TObject);
begin
   MakeGrid(pcgmGrndPtDTM);
end;


procedure Tpt_cloud_opts_fm.DTMfromlowestreturn1Click(Sender: TObject);
begin
   MDDef.MakePCFloor := true;
   MDDef.MakePCCeiling := false;
   MakeGrid(pcgmCeilFloor);
end;

procedure Tpt_cloud_opts_fm.DTMrangescalesfromgroundpoints1Click(Sender: TObject);
var
   //i,
   NewDEM : integer;
   SaveDir,fName : PathStr;
   NewName : shortstring;
   aTable : Petmar_db.tMyData;
begin
   //WriteLineToDebugFile(''); WriteLineToDebugFile('Tpt_cloud_opts_fm.DTMrangescalesfromgroundpoints1Click start');
   fName := ProgramRootDir;
   if Petmar.GetExistingFileName('DTM creation parameters','*.dbf',fName) then begin
      GetDosPath('saved grids',SaveDir);
      aTable := tMyData.Create(fName);
      while not aTable.eof do begin
         SetGridFromTable(atable);
         NewName := TheCloudName + '_' + aTable.GetFieldByNameAsString('RESOLUTION')  + '_' + aTable.GetFieldByNameAsString('UNITS') + '_' +
              aTable.GetFieldByNameAsString('PIXEL_IS') + '_' + aTable.GetFieldByNameAsString('CALC');
         wmDEM.SetPanelText(0,NewName);

         WriteLineToDebugFile('Start ' + fName);

         if Uppercase(aTable.GetFieldByNameAsString('DEM')) = 'DTM' then NewDEM := MakeGrid(pcgmGrndPtDTM);
         if ValidDEM(NewDEM) then begin
            DEMGlb[NewDEM].AreaName := NewName;
            fName := SaveDir + DEMGlb[NewDEM].AreaName + '.dem';
            DEMGlb[NewDEM].WriteNewFormatDEM(fName);
            DEMGlb[NewDEM].SelectionMap.Caption := fName;
            WriteLineToDebugFile('Saved: ' + fName);
         end;
         aTable.Next;
      end;
      aTable.Destroy;
   end;
   wmDEM.SetPanelText(0,'');
   //WriteLineToDebugFile('Tpt_cloud_opts_fm.DTMrangescalesfromgroundpoints1Click done');
end;


procedure Tpt_cloud_opts_fm.Edit10Change(Sender: TObject);
begin
   CheckEditString(Edit10.Text,MDDef.KML_Las_offset);
end;

procedure Tpt_cloud_opts_fm.Edit11Change(Sender: TObject);
begin
   CheckEditString(Edit11.Text,MDDef.LasThinFactor);
end;

procedure Tpt_cloud_opts_fm.Edit12Change(Sender: TObject);
begin
   CheckEditString(Edit12.Text,MDDef.MaxValidZinPointCloud);
end;

procedure Tpt_cloud_opts_fm.Edit13Change(Sender: TObject);
begin
   CheckEditString(Edit13.Text,MDDef.LowValidZinPointCloud);
end;

procedure Tpt_cloud_opts_fm.Edit14Change(Sender: TObject);
begin
   CheckEditString(Edit14.Text,MDDef.MaxValidHAGinPointCloud);
end;

procedure Tpt_cloud_opts_fm.Edit15Change(Sender: TObject);
begin
   CheckEditString(Edit15.Text,MDDef.ls.ScanAngleFilter);
end;

procedure Tpt_cloud_opts_fm.Edit16Change(Sender: TObject);
begin
   CheckEditString(Edit16.Text,MDDef.ls.PointIDFilter);
end;

procedure Tpt_cloud_opts_fm.Edit17Change(Sender: TObject);
begin
   CheckEditString(Edit17.Text, MDdef.DefLidarXGridSize);
   if MdDef.ForceSquarePixels then Edit32.Text := Edit17.Text;
end;

procedure Tpt_cloud_opts_fm.Edit18Change(Sender: TObject);
begin
   CheckEditString(Edit18.Text,MDDEF.LasOpacity);
end;

procedure Tpt_cloud_opts_fm.Edit19Change(Sender: TObject);
begin
   CheckEditString(Edit19.Text,MDdef.DefaultUTMZone);
   Label19.Caption := UTMZoneExtent(MDdef.DefaultUTMZone);
end;

procedure Tpt_cloud_opts_fm.Edit1Change(Sender: TObject);
begin
  if CheckBox1.Checked then CheckEditString(Edit1.Text,MDDef.ls.CatFilter) else MDDef.ls.CatFilter := 0;
end;

procedure Tpt_cloud_opts_fm.Edit20Change(Sender: TObject);
begin
  CheckEditString(Edit20.Text, MDDef.ls.UserDataRecordFilter);
end;

procedure Tpt_cloud_opts_fm.Edit21Change(Sender: TObject);
begin
   CheckEditString(Edit21.Text, MDdef.DefLidarGeoGridSizeY);
   if MdDef.ForceSquarePixels then Edit23.Text := Edit21.Text;
end;

procedure Tpt_cloud_opts_fm.Edit22Change(Sender: TObject);
begin
    CheckEditString(Edit22.Text,MDDef.DSMpercentile);
end;

procedure Tpt_cloud_opts_fm.Edit23Change(Sender: TObject);
begin
   CheckEditString(Edit23.Text,MDDef.MaxRGB);
end;

procedure Tpt_cloud_opts_fm.Edit24Change(Sender: TObject);
begin
   CheckEditString(Edit24.Text,MDdef.FillHoleRadius);
end;

procedure Tpt_cloud_opts_fm.Edit25Change(Sender: TObject);
begin
   CheckEditString(Edit25.Text, MDdef.DefLidarGeoGridSizeX);
end;

procedure Tpt_cloud_opts_fm.Edit26Change(Sender: TObject);
begin
   CheckEditString(Edit26.Text,MDDef.Midpercentile);
end;

procedure Tpt_cloud_opts_fm.Edit27Change(Sender: TObject);
begin
   CheckEditString(Edit27.Text,MDDef.DTMpercentile);
end;

procedure Tpt_cloud_opts_fm.Edit28Change(Sender: TObject);
begin
   CheckEditString(Edit28.Text,MDDef.BlockSize);
   LabelMemoryRequired;
end;

procedure Tpt_cloud_opts_fm.Edit29Change(Sender: TObject);
begin
   CheckEditString(Edit29.Text,MDDef.MaxPtsInCell);
   LabelMemoryRequired;
end;

procedure Tpt_cloud_opts_fm.LabelMemoryRequired;
var
   MemReq : int64;
begin
   MemReq := MDDef.MaxPtsInCell * sqr(succ(MDDef.BlockSize)) * SizeOf(float32) + sqr(succ(MDDef.BlockSize)) * sizeOf(word);
   Label28.Caption := 'Requires ' + SmartMemorySizeBytes(MemReq)  + '; you have ' + SmartMemorySizeBytes(SystemMemorySize)
end;

procedure Tpt_cloud_opts_fm.Edit30Change(Sender: TObject);
begin
   CheckEditString(Edit30.Text, MDdef.DefWKTGridSize);
end;

procedure Tpt_cloud_opts_fm.Edit31Change(Sender: TObject);
begin
   CheckEditString(Edit31.Text,MDDef.MinPtsRequiredPercentile);
end;

procedure Tpt_cloud_opts_fm.Edit32Change(Sender: TObject);
begin
   CheckEditString(Edit32.Text, MDdef.DefLidarYGridSize);
end;

procedure Tpt_cloud_opts_fm.Edit33Change(Sender: TObject);
begin
   CheckEditString(Edit33.Text,MDDef.MaxIntensity)
end;

procedure Tpt_cloud_opts_fm.Edit34Change(Sender: TObject);
begin
   CheckEditString(Edit34.Text,MDDef.MinIntensity)
end;

procedure Tpt_cloud_opts_fm.Edit3Change(Sender: TObject);
begin
   CheckEditString(Edit3.Text,MDDef.CloudMapThinFactor);
end;

procedure Tpt_cloud_opts_fm.Edit4Change(Sender: TObject);
begin
   CheckEditString(Edit4.Text,MDDef.MinRGB);
end;

procedure Tpt_cloud_opts_fm.Edit7Change(Sender: TObject);
begin
  if CheckBox4.Checked then CheckEditString(Edit7.Text,MDDef.ls.RetFilter) else MDDef.ls.RetFilter := 0;
end;


procedure Tpt_cloud_opts_fm.Edit8Change(Sender: TObject);
begin
   CheckEditString(Edit8.Text,MDDef.CloudOpenGLThinFactor);
end;


procedure Tpt_cloud_opts_fm.Edit9Change(Sender: TObject);
begin
   CheckEditString(Edit8.Text,MDDef.CloudSliceThinFactor);
end;


procedure Tpt_cloud_opts_fm.Everything1Click(Sender: TObject);
begin
   AutoSaveDir := MainMapData + 'lidar_scene\' + LasFiles[1].CloudName + '\';
   SafeMakeDir(AutoSaveDir);
   MakeGrid(pcgmMaxIntensity);
   MakeGrid(pcgmClassification);
   MakeGrid(pcgmGrndPtDTM);
   MDDef.MakePCFloor := true;
   MDDef.MakePCCeiling := true;
   MakeGrid(pcgmCeilFloor);
   if LasFiles[1].HasRGB then MakeGrid(pcgmRGB);
end;

procedure Tpt_cloud_opts_fm.Firstreturns1Click(Sender: TObject);
begin
   MakeGrid(pcgmFirstRet);
end;

procedure Tpt_cloud_opts_fm.Floor1Click(Sender: TObject);
begin
   MDDef.MakePCFloor := true;
   MDDef.MakePCCeiling := false;
   MakeGrid(pcgmCeilFloor);
end;

procedure Tpt_cloud_opts_fm.FormClose(Sender: TObject; var Action: TCloseAction);
var
   i : integer;
begin
   for i := 1 to MaxClouds do if (LasFiles[i] <> Nil) then FreeAndNil(LasFiles[i]);
   if (BaseMap <> Nil) then begin
      BaseMap.PointCloudBase := false;
      BaseMap.Closable := true;
      BaseMap.MapDraw.DeleteMapSavedLasLayers;
   end;
   CloseOverlayPointClouds;
   Action := caFree;
end;


procedure Tpt_cloud_opts_fm.Scanangle1Click(Sender: TObject);
begin
   MakeGrid(pcgmScanAngle);
end;

procedure Tpt_cloud_opts_fm.Secondreturns1Click(Sender: TObject);
begin
   MakeGrid(pcgmSecondRet);
end;

procedure Tpt_cloud_opts_fm.SetCloudOptions;
begin
   Edit7.Text := IntToStr(MDDef.ls.RetFilter);
   Edit1.Text := IntToStr(MDDef.ls.CatFilter);
   Edit15.Text := IntToStr(MDDef.ls.ScanAngleFilter);
   Edit16.Text := IntToStr(MDDef.ls.PointIDFilter);
   Edit16.Enabled := MDDef.ls.PointIDFiltered;
   Edit20.Text := IntToStr(MDDef.ls.UserDataRecordFilter);
   CheckBox1.Checked := MDDef.ls.CatFilter > 0;
   CheckBox3.Checked := MDDef.SlicerIHSMerge;
   CheckBox4.Checked := MDDef.ls.RetFilter > 0;
   CheckBox5.Checked := MDDef.ls.ScanAngleFilter > 0;
   GroupBox6.Enabled := MDDef.ls.Filters;
   CheckBox95.Checked := MDDef.ls.DiscardLowPointsNoise;
   CheckBox10.Checked := MDDef.ls.AirReturnsOnly;
   CheckBox11.Checked := MDDef.ls.Filters;
   CheckBox12.Checked := MDDef.ls.FirstReturnsOnly;
   CheckBox13.Checked := MDDef.ls.LastReturnsOnly;
   CheckBox20.Checked := MDDef.ls.UserDataRecordFiltered;
   RadioGroup1.ItemIndex := ord(MDdef.ls.ColorCoding);
end;


procedure Tpt_cloud_opts_fm.Singlereturns1Click(Sender: TObject);
begin
   MakeGrid(pcgmSingleRet);
end;


procedure Tpt_cloud_opts_fm.SymbolPC1Click(Sender: TObject);
begin
   PickSymbol(SymbolPC1,MDDef.CloudMapSymbol[1],'cloud symbolization');
end;

procedure Tpt_cloud_opts_fm.SymbolPC2Click(Sender: TObject);
begin
   PickSymbol(SymbolPC2,MDDef.CloudMapSymbol[2],'cloud symbolization');
end;

procedure Tpt_cloud_opts_fm.SymbolPC3Click(Sender: TObject);
begin
   PickSymbol(SymbolPC3,MDDef.CloudMapSymbol[3],'cloud symbolization');
end;

procedure Tpt_cloud_opts_fm.SymbolPC4Click(Sender: TObject);
begin
   PickSymbol(SymbolPC4,MDDef.CloudMapSymbol[4],'cloud symbolization');
end;

procedure Tpt_cloud_opts_fm.SymbolPC5Click(Sender: TObject);
begin
   PickSymbol(SymbolPC5,MDDef.CloudMapSymbol[5],'cloud symbolization');
end;

procedure Tpt_cloud_opts_fm.HideForGridPick;
begin
   BitBtn8.Visible := false;
   BitBtn21.Visible := false;
   BitBtn58.Visible := false;
   BitBtn59.Visible := false;
   TabSheet1.TabVisible := false;
   TabSheet2.TabVisible := false;
   TabSheet3.TabVisible := false;
   TabSheet4.TabVisible := false;
   TabSheet5.TabVisible := false;
   TabSheet6.TabVisible := false;
   TabSheet7.TabVisible := false;
   Filters.TabVisible := false;
   General.TabVisible := false;
   Panel1.Visible := false;
   Panel3.Visible := false;
   PageControl1.ActivePage := TabSheet8;
end;


procedure Tpt_cloud_opts_fm.HideOptions;
begin
   Edit17.Enabled := MDDef.LidarGridProjection = UTMBasedDEM;
   Edit32.Enabled := MDDef.LidarGridProjection = UTMBasedDEM;
   Edit19.Enabled := MDDef.LidarGridProjection = UTMBasedDEM;
   Label16.Enabled := MDDef.LidarGridProjection = UTMBasedDEM;
   Label18.Enabled := MDDef.LidarGridProjection = UTMBasedDEM;
   Label19.Enabled := MDDef.LidarGridProjection = UTMBasedDEM;

   Edit21.Enabled := MDDef.LidarGridProjection = ArcSecDEM;
   Edit25.Enabled := MDDef.LidarGridProjection = ArcSecDEM;
   Label24.Enabled := MDDef.LidarGridProjection = ArcSecDEM;
   Label20.Enabled := MDDef.LidarGridProjection = ArcSecDEM;

   BitBtn45.Enabled := MDDef.LidarGridProjection = WKTDEM;
   Edit30.Enabled := MDDef.LidarGridProjection = WKTDEM;
   Label31.Enabled := MDDef.LidarGridProjection = WKTDEM;
end;


procedure Tpt_cloud_opts_fm.hreekeydensities1Click(Sender: TObject);
begin
   MakeGrid(pcgmThreeKeyDensities);
end;

procedure Tpt_cloud_opts_fm.FormCreate(Sender: TObject);
var
   i : integer;
begin
   {$If Defined(BasicOpens) or Defined(RecordPointCloudOptionsForm) or Defined(RecordLASOpen) or Defined(RecordPointCloudViewing)} WriteLineToDebugFile('Tpt_cloud_opts_fm.FormCreate in'); {$EndIf}
   for i := 1 to MaxClouds do LasFiles[i] := Nil;
   for i := 1 to MaxClouds do UsePC[i] := false;

   if MDDef.LasThinFactor < 1 then MDDef.LasThinFactor := 1;
   MDDef.LasAutoThin := true;
   (*
   MDDef.MinIntensity := 0;
   MDDef.MaxIntensity := MaxWord;
   MDDef.MinRGB := 0;
   MDDef.MaxRGB := MaxWord;
   *)
   ShowMeanDensityGrid := false;

   FirstLoad := true;
   SetCloudOptions;
   Edit3.Text := IntToStr(MDDef.CloudMapThinFactor);
   Edit8.Text := IntToStr(MDDef.CloudOpenGLThinFactor);
   Edit9.Text := IntToStr(MDDef.CloudSliceThinFactor);
   Edit10.Text := RealToString(MDDef.KML_Las_offset,-12,-2);
   Edit11.Text := IntToStr(MDDef.LasThinFactor);
   Edit14.Text := RealToString(MDDef.MaxValidHAGinPointCloud,-12,-1);
   Edit17.Text := RealToString(MDdef.DefLidarXGridSize,-12,-2);
   Edit32.Text := RealToString(MDdef.DefLidarYGridSize,-12,-2);
   Edit18.Text := IntToStr(MDDEF.LasOpacity);
   Edit24.Text := IntToStr(MDdef.FillHoleRadius);
   Edit25.Text := RealToString(MDdef.DefLidarGeoGridSizeX,-12,-4);
   Edit21.Text := RealToString(MDdef.DefLidarGeoGridSizeY,-12,-4);

   Edit22.Text := RealToString(MDDef.DSMpercentile,-8,-3);
   Edit30.Text := RealToString(MDdef.DefWKTGridSize,-8,-2);

   Edit26.Text := RealToString(MDDef.Midpercentile,-8,-3);
   Edit27.Text := RealToString(MDDef.DTMpercentile,-8,-3);

   Edit28.Text := IntToStr(MDDef.BlockSize);
   Edit29.Text := IntToStr(MDDef.MaxPtsInCell);
   Edit31.Text := IntToStr(MDDef.MinPtsRequiredPercentile);

   LabelMemoryRequired;


   CheckBox26.Checked := MDDef.UseOverlap;
   CheckBox2.Checked := MDDef.UseNoise;

   CheckBox7.Checked := MDDef.LabelLAStiles;
   CheckBox8.Checked := MDDef.LasAutoThin;
   CheckBox9.Checked := MDDef.PCAutoFillHoles;
   CheckBox14.Checked := MDDef.ShowCloudOutlines;
   CheckBox16.Checked := MDDef.AutoZoomOpenLAS;
   CheckBox18.Checked := MDDef.LasElevChecks;
   CheckBox19.Checked := MDDef.LasLegend;
   CheckBox21.Checked := MDDef.LogDosOutput;
   CheckBox22.Checked := MDDef.ls.GroundClassOnly;
   CheckBox24.Checked := MDdef.AutoDrawMapLAS;
   Checkbox25.Checked := MDDef.LASPC99;
   CheckBox27.Checked := MDDef.ls.DiscardHighNoise;
   CheckBox28.Checked := MDDef.ls.AssumeLAS14classes;
   CheckBox29.Checked := MDDef.LatLongSlices;
   CheckBox30.Checked := MDdef.AutoReDrawMapLAS;
   CheckBox32.Checked := MDDef.AutoThinByBlock;

   RadioGroup4.ItemIndex := MDDef.DTMoption;
   if MDDef.PickLASDirs then RadioGroup6.ItemIndex := 0 else RadioGroup6.ItemIndex := 1;
   RadioGroup2.ItemIndex := MDDef.ls.BadPointFilter;
   RadioGroup5.ItemIndex := pred(MDDef.LasDEMPixelIs);

   RadioGroup7.ItemIndex := MDDef.LidarGridProjection;
   if (MDdef.DefaultLatHemi = 'N') then RadioGroup8.ItemIndex := 0 else RadioGroup8.ItemIndex := 1;

   if (MDDef.WKTLidarProj <> '') then BitBtn45.Caption := ExtractFileNameNoExt(MDDef.WKTLidarProj);
   Label22.Enabled := MDDef.PCAutoFillHoles;
   Edit24.Enabled := MDDef.PCAutoFillHoles;

   MemPtCloud := Nil;
   AutoSaveDir := '';
   DEMrulesFName := '';
   InitialCloudDisplay := true;
   UncheckAll;
   HideOptions;
   ResyncFileList;
   PageControl1.ActivePage := TabSheet1;
   wmDEM.FormPlacementInCorner(self);
   {$If Defined(BasicOpens) or Defined(RecordPointCloudOptionsForm) or Defined(RecordLASOpen) or Defined(RecordPointCloudViewing)} WriteLineToDebugFile('Tpt_cloud_opts_fm.FormCreate out'); {$EndIf}
end;


procedure Tpt_cloud_opts_fm.FusionTIN1Click(Sender: TObject);
var
   i : integer;
   LasName,DTMName : PathStr;
begin
    {$IfDef RecordMakeGrid} WriteLineToDebugFile('Tpt_cloud_opts_fm.BitBtn1Click'); {$EndIf}
    if (Edit19.Text = '') then begin
       PickUTMZone(MDdef.DefaultUTMZone);
       Edit19.Text := IntToStr(MDdef.DefaultUTMZone);
    end;

    for i := 1 to MaxClouds do if UsePC[i] and (LasFiles[i] <> Nil) then begin
       LasName := NextFileNumber(MDtempDir, 'fusion_tin_dtm_','.las');
       LasFiles[i].ExtractGroundPoints(LasName);
       {$IfDef RecordMakeGrid} WriteLineToDebugFile('Tpt_cloud_opts_fm.BitBtn1Click, LAS=' + LasName); {$EndIf}
       DTMname := NextFileNumber(MdtempDir, LasFiles[i].CloudName + '_fusion_tin_dtm_','.dem');
       FusionTinCreate(LasName,DTMName,MDdef.DefLidarXGridSize,StrToInt(Edit19.Text),'N');
    end;
end;

procedure Tpt_cloud_opts_fm.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\pt_cld_opts.htm');
end;

procedure Tpt_cloud_opts_fm.RadioGroup1Click(Sender: TObject);
begin
   {$IfDef RecordPointCloudOptionsForm} WriteLineToDebugFile('Tpt_cloud_opts_fm.RadioGroup1Click in, ' + RadioGroup1.Items.Strings[RadioGroup1.ItemIndex]); {$EndIf}
   if RadioGroup1.Items.Strings[RadioGroup1.ItemIndex] = 'Elevation' then MDdef.ls.ColorCoding := lasccElevation;
   if RadioGroup1.Items.Strings[RadioGroup1.ItemIndex] = 'Classification' then MDdef.ls.ColorCoding := lasccClass;
   if RadioGroup1.Items.Strings[RadioGroup1.ItemIndex] = 'Intensity' then MDdef.ls.ColorCoding := lasccIntensity;
   if RadioGroup1.Items.Strings[RadioGroup1.ItemIndex] = 'RGB' then MDdef.ls.ColorCoding := lasccRGB;
   if RadioGroup1.Items.Strings[RadioGroup1.ItemIndex] = 'Scan angle' then MDdef.ls.ColorCoding := lasccScanAngle;
   if RadioGroup1.Items.Strings[RadioGroup1.ItemIndex] = 'Return numbers' then MDdef.ls.ColorCoding := lasccReturnNumber;
   if RadioGroup1.Items.Strings[RadioGroup1.ItemIndex] = 'Returns in pulse' then MDdef.ls.ColorCoding := lasccReturnsPulse;
   if RadioGroup1.Items.Strings[RadioGroup1.ItemIndex] = 'GPS time' then MDdef.ls.ColorCoding := lasccGPSTime;
   if RadioGroup1.Items.Strings[RadioGroup1.ItemIndex] = 'Point ID' then MDdef.ls.ColorCoding := lasccPointSourceID;
   if RadioGroup1.Items.Strings[RadioGroup1.ItemIndex] = 'User data' then MDdef.ls.ColorCoding := lasccUserData;
   if RadioGroup1.Items.Strings[RadioGroup1.ItemIndex] = 'Cloud ID' then MDdef.ls.ColorCoding := lasccCloudID;
   if MDdef.AutoReDrawMapLAS then RedrawAllLayers;
end;


procedure Tpt_cloud_opts_fm.RadioGroup2Click(Sender: TObject);
begin
   MDDef.ls.BadPointFilter := RadioGroup2.ItemIndex;
end;

procedure Tpt_cloud_opts_fm.RadioGroup4Click(Sender: TObject);
begin
  MDDef.DTMoption := RadioGroup4.ItemIndex;
end;

procedure Tpt_cloud_opts_fm.RadioGroup5Click(Sender: TObject);
begin
   MDDef.LasDEMPixelIs := succ(RadioGroup5.ItemIndex);
end;

procedure Tpt_cloud_opts_fm.RadioGroup6Click(Sender: TObject);
begin
   MDDef.PickLASDirs := RadioGroup6.ItemIndex = 0;
end;

procedure Tpt_cloud_opts_fm.RadioGroup7Click(Sender: TObject);
begin
   MDDef.LidarGridProjection := RadioGroup7.ItemIndex;
   HideOptions;
end;


procedure Tpt_cloud_opts_fm.RadioGroup8Click(Sender: TObject);
begin
   if RadioGroup8.ItemIndex = 0 then MDdef.DefaultLatHemi := 'N' else MDdef.DefaultLatHemi := 'S';
end;

procedure Tpt_cloud_opts_fm.BitBtn8Click(Sender: TObject);
var
   Cloud : integer;
   OK,ViewMultiple,ExportDone,AlreadyLoaded,LASDestroy : Boolean;
   ColorsFName,fName : array[1..5] of PathStr;
   i : Integer;

      procedure ExportCloud(Cloud : integer; var fName,ColorName : PathStr; BaseName : PathStr; ExportFilter : tLASClassificationCategory = lccAll);
      begin
         if (LasFiles[Cloud] <> Nil) and (LasFiles[Cloud].LAS_fnames.Count > 0) then begin
            if AlreadyLoaded then exit;
            FName := Petmar.NextFileNumber(MDTempDir, BaseName + '_','.xyzib');
            if LasFiles[Cloud].ExportBinary(mlOnMask,BaseMap,Cloud,FName,ColorName,ExportFilter) then OK := true;
         end;
      end;

begin
   {$If Defined(RecordPointCloudViewing) or Defined(OGLexport)} WriteLineToDebugFile('Tpt_cloud_opts_fm.BitBtn8Click (OGL)'); {$EndIf}
   ok := False;
   AlreadyLoaded := false;
   LASdestroy := true;
   for i := 1 to MaxClouds do begin
      fName[i] := '';
      ColorsFName[i] := '';
   end;
   if (Sender = BitBtn20) or (Sender = BitBtn27) or (Sender = BitBtn8) then begin
      ExportDone := false;
      ViewMultiple := (Sender = BitBtn27) or (Sender = BitBtn8);

      for Cloud := 1 to MaxClouds do begin
         if UsePC[Cloud] and (LasFiles[Cloud] <> Nil) then begin
           if (Sender = BitBtn8) then begin
              ExportCloud(Cloud,fName[Cloud],ColorsFName[Cloud],LasFiles[Cloud].CloudName);
           end
           else begin
               LASdestroy := false;
               if (Sender = BitBtn20) then begin
                  MDdef.ls.ColorCoding := lasccClass;
                  ExportCloud(Cloud,fName[1],ColorsFName[1],'Lidar_classification');
                  MDdef.ls.ColorCoding := lasccIntensity;
                  ExportCloud(Cloud,fName[2],ColorsFName[2],'Lidar_intensity');
                  MDdef.ls.ColorCoding := lasccElevation;
                  ExportCloud(Cloud,fName[3],ColorsFName[3],'Elevation');
                  if LasFiles[Cloud].HasRGB then begin
                     MDdef.ls.ColorCoding := lasccRGB;
                     ExportCloud(Cloud,fName[4],ColorsFName[4],'RGB');
                  end;
               end;
               if (Sender = BitBtn27) then begin
                  MDdef.ls.ColorCoding := lasccClass;
                  ExportCloud(1,fName[1],ColorsFName[1],'Ground',lccGround);
                  ExportCloud(1,fName[2],ColorsFName[2],'Vegetation',lccVeg);
                  ExportCloud(1,fName[3],ColorsFName[3],'Buildings',lccBuilding);
                  ExportCloud(1,fName[4],ColorsFName[4],'Water',lccWater);
                  ExportCloud(1,fName[5],ColorsFName[5],'Unclassified',lccUnclass);
               end;
               ExportDone := true;
           end;
         end;
      end;
   end
   else begin
       if MDdef.ls.ColorCoding in [lasccRGB,lasccClass,lasccIntensity,lasccElevation,lasccCloudID,lasccPointSourceID,lasccUserData] then begin
         for Cloud := 1 to MaxClouds do begin
            if UsePC[Cloud] and (LasFiles[Cloud] <> Nil) then begin
               {$IfDef RecordPointCloudViewing} WriteLineToDebugFile('Export=' + IntToStr(Cloud) + '  ' + fName[Cloud]); {$EndIf}
               ExportCloud(Cloud,fName[Cloud],ColorsFName[Cloud],LasFiles[Cloud].CloudName);
               AlreadyLoaded := false;
            end;
         end;
         ViewMultiple := true;
       end
       else begin
          MessageToContinue('Export color mode not yet supported');
          exit;
       end;
   end;

   {$If Defined(RecordPointCloudViewing) or Defined(OGLexport)} WriteLineToDebugFile('done Tpt_cloud_opts_fm.BitBtn8Click (OGL)'); {$EndIf}
   if OK then begin
      wmdem.SetPanelText(0,'Start FMX3dViewer');
      FMX3dViewer(ViewMultiple,fName[1],fname[2],fname[3],fname[4],fName[5],ColorsFName[1],ColorsFName[2],ColorsFName[3],ColorsFName[4],ColorsfName[5])
   end
   else MessageToContinue('No points exported');
   wmdem.SetPanelText(0,'');
end;


initialization
   pt_cloud_opts_fm := Nil;
finalization
end.
