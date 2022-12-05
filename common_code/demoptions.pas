unit demoptions;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of ianMICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2022 Peter L. Guth   }
{____________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   //{$Define OptionsProblems}
   //{$Define ShowLocalOptima}
   //{$Define ShowDirectories}
   //{$Define ShowDatumOptions}
   //{$Define RecordPitsSpires}
{$EndIf}

interface

uses
   Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls,
   SysUtils, Windows,Classes, Graphics, Forms, Controls,
   Buttons,  {OkCancl2,}  Tabnotbk, Dialogs,
   DEMMapf, DEMDefs,Grids,Petmar_types, OKCANCL2;

type
  TOptionsForm = class(TOKRightDlg)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    RadioGroup7: TRadioGroup;
    CheckBox4: TCheckBox;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    Label8: TLabel;
    Label34: TLabel;
    Label17: TLabel;
    RadioGroup6: TRadioGroup;
    BitBtn1: TBitBtn;
    Button9: TButton;
    Button5: TButton;
    Edit15: TEdit;
    Edit18: TEdit;
    CheckBox59: TCheckBox;
    CheckBox51: TCheckBox;
    TabSheet5: TTabSheet;
    Button2: TButton;
    Button15: TButton;
    Button21: TButton;
    TabSheet6: TTabSheet;
    TabSheet7: TTabSheet;
    BitBtn8: TBitBtn;
    BitBtn7: TBitBtn;
    TabSheet8: TTabSheet;
    CheckBox26: TCheckBox;
    Button22: TButton;
    TabSheet9: TTabSheet;
    Label3: TLabel;
    Button12: TButton;
    BitBtn6: TBitBtn;
    Edit17: TEdit;
    Button18: TButton;
    TabSheet10: TTabSheet;
    TabSheet11: TTabSheet;
    Label36: TLabel;
    Label7: TLabel;
    UpDown1: TUpDown;
    CheckBox16: TCheckBox;
    TabSheet13: TTabSheet;
    RadioGroup3: TRadioGroup;
    RadioGroup2: TRadioGroup;
    RadioGroup11: TRadioGroup;
    TabSheet14: TTabSheet;
    RadioGroup5: TRadioGroup;
    RadioGroup4: TRadioGroup;
    CheckBox30: TCheckBox;
    //CheckBox27: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox1: TCheckBox;
    TabSheet15: TTabSheet;
    CheckBox69: TCheckBox;
    CheckBox61: TCheckBox;
    CheckBox62: TCheckBox;
    TabSheet16: TTabSheet;
    CheckBox40: TCheckBox;
    CheckBox45: TCheckBox;
    Label38: TLabel;
    Label30: TLabel;
    CheckBox31: TCheckBox;
    BitBtn14: TBitBtn;
    RadioGroup14: TRadioGroup;
    RadioGroup22: TRadioGroup;
    GroupBox7: TGroupBox;
    Label31: TLabel;
    Label32: TLabel;
    Edit13: TEdit;
    Edit14: TEdit;
    CheckBox88: TCheckBox;
    Label33: TLabel;
    Edit23: TEdit;
    RadioGroup23: TRadioGroup;
    RadioGroup24: TRadioGroup;
    BitBtn25: TBitBtn;
    BitBtn26: TBitBtn;
    BitBtn27: TBitBtn;
    TabSheet12: TTabSheet;
    CheckBox34: TCheckBox;
    //BitBtn29: TBitBtn;
    //Label10: TLabel;
    RadioGroup18: TRadioGroup;
    CheckBox20: TCheckBox;
    CheckBox56: TCheckBox;
    CheckBox96: TCheckBox;
    CheckBox98: TCheckBox;
    CheckBox99: TCheckBox;
    CheckBox100: TCheckBox;
    CheckBox101: TCheckBox;
    TabSheet18: TTabSheet;
    Button1: TButton;
    Label1: TLabel;
    RadioGroup16: TRadioGroup;
    RadioGroup17: TRadioGroup;
    CheckBox49: TCheckBox;
    BitBtn13: TBitBtn;
    Label11: TLabel;
    CheckBox107: TCheckBox;
    CheckBox123: TCheckBox;
    Koppen: TBitBtn;
    CheckBox91: TCheckBox;
    CheckBox17: TCheckBox;
    GroupBox8: TGroupBox;
    CheckBox66: TCheckBox;
    Label13: TLabel;
    Edit6: TEdit;
    CheckBox108: TCheckBox;
    Label15: TLabel;
    Edit10: TEdit;
    Label21: TLabel;
    Edit11: TEdit;
    OutlineButton: TBitBtn;
    CheckBox120: TCheckBox;
    CheckBox29: TCheckBox;
    BitBtn15: TBitBtn;
    CheckBox130: TCheckBox;
    CheckBox122: TCheckBox;
    RadioGroup10: TRadioGroup;
    BitBtn16: TBitBtn;
    CheckBox11: TCheckBox;
    CheckBox73: TCheckBox;
    StringGrid1: TStringGrid;
    RadioGroup12: TRadioGroup;
    CheckBox124: TCheckBox;
    Graph: TBitBtn;
    GroupBox11: TGroupBox;
    Label39: TLabel;
    Edit21: TEdit;
    Edit22: TEdit;
    Label40: TLabel;
    BitBtn2: TBitBtn;
    CheckBox52: TCheckBox;
    CheckBox55: TCheckBox;
    CheckBox64: TCheckBox;
    CheckBox76: TCheckBox;
    CheckBox83: TCheckBox;
    CheckBox84: TCheckBox;
    CheckBox125: TCheckBox;
    Label4: TLabel;
    Edit2: TEdit;
    CheckBox129: TCheckBox;
    CheckBox133: TCheckBox;
    BitBtn9: TBitBtn;
    CheckBox3: TCheckBox;
    Edit5: TEdit;
    Label2: TLabel;
    GroupBox2: TGroupBox;
    RadioGroup9: TRadioGroup;
    Label26: TLabel;
    Edit26: TEdit;
    CheckBox12: TCheckBox;
    Panel1: TPanel;
    BitBtn3: TBitBtn;
    BitBtn32: TBitBtn;
    ColorDialog1: TColorDialog;
    BitBtn10: TBitBtn;
    BitBtn17: TBitBtn;
    BitBtn18: TBitBtn;
    CheckBox22: TCheckBox;
    BitBtn19: TBitBtn;
    BitBtn20: TBitBtn;
    RadioGroup19: TRadioGroup;
    CheckBox39: TCheckBox;
    CheckBox41: TCheckBox;
    CheckBox42: TCheckBox;
    RadioGroup25: TRadioGroup;
    CheckBox43: TCheckBox;
    RadioGroup26: TRadioGroup;
    TabSheet2: TTabSheet;
    CheckBox48: TCheckBox;
    Label9: TLabel;
    Edit19: TEdit;
    BitBtn12: TBitBtn;
    BitBtn31: TBitBtn;
    TabSheet17: TTabSheet;
    CheckBox77: TCheckBox;
    GroupBox1: TGroupBox;
    CheckBox63: TCheckBox;
    CheckBox75: TCheckBox;
    CheckBox74: TCheckBox;
    CheckBox71: TCheckBox;
    CheckBox67: TCheckBox;
    CheckBox65: TCheckBox;
    CheckBox58: TCheckBox;
    RadioGroup20: TRadioGroup;
    CheckBox82: TCheckBox;
    CheckBox86: TCheckBox;
    Label16: TLabel;
    Edit20: TEdit;
    Label22: TLabel;
    Edit24: TEdit;
    Button3: TButton;
    Label23: TLabel;
    TabSheet20: TTabSheet;
    CheckBox24: TCheckBox;
    CheckBox78: TCheckBox;
    CheckBox87: TCheckBox;
    Label25: TLabel;
    Edit25: TEdit;
    CheckBox97: TCheckBox;
    GroupBox3: TGroupBox;
    Label29: TLabel;
    CheckBox90: TCheckBox;
    CheckBox94: TCheckBox;
    Edit28: TEdit;
    CheckBox104: TCheckBox;
    Label37: TLabel;
    Edit29: TEdit;
    TabSheet21: TTabSheet;
    GroupBox4: TGroupBox;
    CheckBox105: TCheckBox;
    CheckBox111: TCheckBox;
    CheckBox112: TCheckBox;
    GroupBox5: TGroupBox;
    CheckBox113: TCheckBox;
    CheckBox114: TCheckBox;
    CheckBox115: TCheckBox;
    GroupBox6: TGroupBox;
    BitBtn34: TBitBtn;
    BitBtn35: TBitBtn;
    CheckBox117: TCheckBox;
    GroupBox9: TGroupBox;
    CheckBox119: TCheckBox;
    CheckBox126: TCheckBox;
    CheckBox132: TCheckBox;
    CheckBox134: TCheckBox;
    CheckBox135: TCheckBox;
    CheckBox136: TCheckBox;
    RadioGroup32: TRadioGroup;
    CheckBox15: TCheckBox;
    CheckBox137: TCheckBox;
    CheckBox138: TCheckBox;
    CheckBox140: TCheckBox;
    CheckBox141: TCheckBox;
    CheckBox142: TCheckBox;
    CheckBox143: TCheckBox;
    GroupBox10: TGroupBox;
    Label19: TLabel;
    //Edit7: TEdit;
    CheckBox131: TCheckBox;
    CheckBox85: TCheckBox;
    Label42: TLabel;
    //Edit31: TEdit;
    Label43: TLabel;
    Edit32: TEdit;
    CheckBox147: TCheckBox;
    CheckBox148: TCheckBox;
    Label44: TLabel;
    Edit33: TEdit;
    CheckBox149: TCheckBox;
    CheckBox150: TCheckBox;
    CheckBox151: TCheckBox;
    RadioGroup30: TRadioGroup;
    CheckBox152: TCheckBox;
    CheckBox153: TCheckBox;
    BitBtn4: TBitBtn;
    CheckBox155: TCheckBox;
    GroupBox12: TGroupBox;
    Label28: TLabel;
    Label45: TLabel;
    Label46: TLabel;
    Edit27: TEdit;
    Edit34: TEdit;
    Edit35: TEdit;
    CheckBox23: TCheckBox;
    RadioGroup33: TRadioGroup;
    CheckBox89: TCheckBox;
    Label48: TLabel;
    Edit37: TEdit;
    CheckBox156: TCheckBox;
    CheckBox18: TCheckBox;
    CheckBox118: TCheckBox;
    BitBtn36: TBitBtn;
    CheckBox157: TCheckBox;
    CheckBox9: TCheckBox;
    CheckBox32: TCheckBox;
    CheckBox72: TCheckBox;
    CheckBox159: TCheckBox;
    CheckBox160: TCheckBox;
    RadioGroup34: TRadioGroup;
    RadioGroup35: TRadioGroup;
    BitBtn11: TBitBtn;
    GDAL: TBitBtn;
    CheckBox38: TCheckBox;
    CheckBox95: TCheckBox;
    Button4: TButton;
    Label5: TLabel;
    CheckBox164: TCheckBox;
    GroupBox14: TGroupBox;
    CheckBox163: TCheckBox;
    CheckBox47: TCheckBox;
    CheckBox14: TCheckBox;
    CheckBox165: TCheckBox;
    BitBtn5: TBitBtn;
    CheckBox166: TCheckBox;
    GroupBox13: TGroupBox;
    CheckBox37: TCheckBox;
    checkbox161: TCheckBox;
    CheckBox167: TCheckBox;
    CheckBox121: TCheckBox;
    BitBtn37: TBitBtn;
    CheckBox110: TCheckBox;
    GroupBox15: TGroupBox;
    CheckBox168: TCheckBox;
    CheckBox170: TCheckBox;
    CheckBox171: TCheckBox;
    CheckBox172: TCheckBox;
    CheckBox173: TCheckBox;
    CheckBox174: TCheckBox;
    CheckBox175: TCheckBox;
    CheckBox176: TCheckBox;
    CheckBox177: TCheckBox;
    CheckBox178: TCheckBox;
    CheckBox179: TCheckBox;
    RadioGroup8: TRadioGroup;
    CheckBox7: TCheckBox;
    BitBtn38: TBitBtn;
    Label6: TLabel;
    RadioGroup1: TRadioGroup;
    CheckBox13: TCheckBox;
    //CheckBox80: TCheckBox;
    CheckBox28: TCheckBox;
    BitBtn21: TBitBtn;
    BitBtn28: TBitBtn;
    BitBtn30: TBitBtn;
    Label14: TLabel;
    Edit1: TEdit;
    BitBtn33: TBitBtn;
    CheckBox6: TCheckBox;
    CheckBox35: TCheckBox;
    CheckBox53: TCheckBox;
    CheckBox57: TCheckBox;
    CheckBox60: TCheckBox;
    Button6: TButton;
    CheckBox36: TCheckBox;
    CheckBox79: TCheckBox;
    CheckBox81: TCheckBox;
    CheckBox70: TCheckBox;
    CheckBox102: TCheckBox;
    CheckBox103: TCheckBox;
    CheckBox46: TCheckBox;
    BitBtn22: TBitBtn;
    Label18: TLabel;
    Edit4: TEdit;
    CheckBox54: TCheckBox;
    BitBtn29: TBitBtn;
    CheckBox300: TCheckBox;
    GroupBox16: TGroupBox;
    Edit8: TEdit;
    Label24: TLabel;
    CheckBox33: TCheckBox;
    CheckBox127: TCheckBox;
    CheckBox93: TCheckBox;
    CheckBox21: TCheckBox;
    GroupBox17: TGroupBox;
    RadioGroup15: TRadioGroup;
    Edit9: TEdit;
    Label10: TLabel;
    CheckBox144: TCheckBox;
    CheckBox128: TCheckBox;
    CheckBox92: TCheckBox;
    CheckBox116: TCheckBox;
    CheckBox145: TCheckBox;
    CheckBox146: TCheckBox;
    BitBtn39: TBitBtn;
    BitBtn40: TBitBtn;
    Label20: TLabel;
    BitBtn41: TBitBtn;
    BitBtn42: TBitBtn;
    GroupBox18: TGroupBox;
    Edit12: TEdit;
    DTfilllabel: TLabel;
    CheckBox5: TCheckBox;
    CheckBox154: TCheckBox;
    CheckBox158: TCheckBox;
    CheckBox162: TCheckBox;
    CheckBox169: TCheckBox;
    CheckBox180: TCheckBox;
    procedure BitBtn32Click(Sender: TObject);
    procedure BitBtn13Click(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure UpDown1Click(Sender: TObject; Button: TUDBtnType);
    procedure BitBtn1Click(Sender: TObject);
    procedure Button18Click(Sender: TObject);
    procedure Button14Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure BitBtn6Click(Sender: TObject);
    procedure BitBtn8Click(Sender: TObject);
    procedure BitBtn7Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button15Click(Sender: TObject);
    procedure Button21Click(Sender: TObject);
    procedure Button22Click(Sender: TObject);
    procedure BitBtn14Click(Sender: TObject);
    procedure BitBtn18Click(Sender: TObject);
    procedure CheckBox88Click(Sender: TObject);
    procedure BitBtn25Click(Sender: TObject);
    procedure BitBtn26Click(Sender: TObject);
    procedure BitBtn27Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
   //procedure BitBtn29Click(Sender: TObject);
    procedure KoppenClick(Sender: TObject);
    procedure OutlineButtonClick(Sender: TObject);
    procedure BitBtn15Click(Sender: TObject);
    procedure BitBtn16Click(Sender: TObject);
    procedure StringGrid1SelectCell(Sender: TObject; ACol, ARow: Integer;  var CanSelect: Boolean);
    procedure GraphClick(Sender: TObject);
    procedure BitBtn9Click(Sender: TObject);
    procedure BitBtn17Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure BitBtn19Click(Sender: TObject);
    procedure BitBtn20Click(Sender: TObject);
    procedure BitBtn12Click(Sender: TObject);
    procedure BitBtn31Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Edit25Change(Sender: TObject);
    procedure BitBtn34Click(Sender: TObject);
    procedure BitBtn35Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure RadioGroup33Click(Sender: TObject);
    procedure BitBtn36Click(Sender: TObject);
    procedure RadioGroup7Click(Sender: TObject);
    procedure BitBtn11Click(Sender: TObject);
    procedure GDALClick(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure BitBtn37Click(Sender: TObject);
    procedure BitBtn38Click(Sender: TObject);
    procedure BitBtn21Click(Sender: TObject);
    procedure BitBtn28Click(Sender: TObject);
    procedure BitBtn30Click(Sender: TObject);
    procedure BitBtn33Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure BitBtn22Click(Sender: TObject);
    procedure BitBtn29Click(Sender: TObject);
    procedure BitBtn39Click(Sender: TObject);
    procedure BitBtn40Click(Sender: TObject);
    procedure BitBtn41Click(Sender: TObject);
    procedure BitBtn42Click(Sender: TObject);
  private
    { Private declarations }
      procedure labelDirectories;
    procedure ShowDRGBoxes;
  public
    { Public declarations }
     UserWorking,Saved,ProgramModeChanged : boolean;
  end;

procedure ChangeOptions(DesiredPage : integer = 0);
procedure GetSpeedDistOptions;

{$IfDef VCL}
   procedure SetHorizonOptions(TheMap : tMapForm = nil; Lat : float64 = -99; Long : float64 = -99);
{$EndIf}

{$IfDef ExGeostats}
{$Else}
   procedure GetFabricOptions(DEMonMap : integer);
   procedure PitSpireDefaults(var inMapOwner : tMapForm; PC : integer);
   procedure OptimaRegionsDefaults(DEM : integer);
   procedure GetGeomorphBlockOpts(WhatFor : tGeomporphBlock; DEM : integer; GridLimits : tGridLimits);
{$EndIf}


implementation

{$R *.DFM}

uses
   {$IfDef ExStereoNet}
   {$Else}
      NetOpts,
   {$EndIf}
   {$IfDef ExGeology}
   {$Else}
      sc_ColOpts, Beach_Ball_Options,
   {$EndIf}

   {$IfDef ExGeostats}
   {$Else}
      DEMFabricRegion,
      optima_reg,
      Pit_and_Spire,
      DEMCurvature,
      demslopeopts,
   {$EndIf}

{$IfDef ExSat}
{$Else}
   DEMEROS,//DEMEROSM,demsatcontrast,
{$EndIf}

{$IfDef ExVectorOverlay}
{$Else}
   DEMConOp,DEMRange,
{$EndIf}

{$IfDef ExTiger}
{$Else}
   DEMTigerOps,
{$EndIf}


{$IfDef ExPLSS}
{$Else}
   DEM_PLSS,DEM_PLSS_Op,
{$EndIf}

{$IfDef ExAmbush}
{$Else}
   DEMAmbushParams,
{$EndIf}

{$IfDef ExViewshed}
{$Else}
   DEMFanParams,
   DEM_Fan_Algorithm,
{$EndIf}

   {$IfDef ExDrainage}
   {$Else}
      Drainage_opts,
   {$EndIf}

   {$IfDef ExPers}
   {$Else}
      DEMPersW,DEMPerOp,
   {$EndIf}

   {$IfDef ExGeography}
   {$Else}
      koppen_opts,
   {$EndIf}

   {$IfDef ExKML}
   {$Else}
      kml_overlay,
   {$EndIf}

   {$IfDef ExComplexGeoStats}
   {$Else}
      Block_Opts,
      DEMSSOcalc,
   {$EndIf}

   {$IfDef ExGIS}
   {$Else}
      demdatabase,
   {$EndIf}

   {$IfDef VCL}
      Horizon_Opts,
   {$EndIf}

   {$IFDef ExPointCloud}
   {$Else}
      Point_Cloud_Memory, las_lidar,
   {$EndIf}

   {$IfDef NoExternalPrograms}
   {$Else}
      MD_use_tools,
   {$EndIf}

   {$IFDef ExLCP}
   {$Else}
      lcp_options,
   {$EndIf}

   {$IfDef ExDP}
   {$Else}
      Dragon_plot_init,
  {$EndIf}

   gdal_tools,
   PetEd32,
   DEM_Digit_opts,
   DEMGrPik,DEMRefOp,BaseMap,
   DEM_Gaz_opts,
   DEM_Manager,
   Speed_dist_form,
   DEMCoord,Demdef_routines,
   point_cloud_options,
   NE_Outlines,
   USOutlines,
   PETMAR,PETMath,PETGraphColors, Petmar_ini_file,
   DEM_indexes,
   Nevadia_Main;

var
   MainMap,
   EtopoRow,
   ViewSheds,
   VectorMap,
   BlowUpDEM,
   BlowUpVeg,
   FWT,
   DEMIXbase,
   TauDEM : integer;



procedure GetSpeedDistOptions;
var
  SpeedDistanceForm: TSpeedDistanceForm;
begin
  SpeedDistanceForm := TSpeedDistanceForm.Create(Application);
  SpeedDistanceForm.ShowModal;
  SpeedDistanceForm.Free;
end;


procedure ChangeOptions;
var
  OptionsForm : TOptionsForm;
begin
   OptionsForm := TOptionsForm.Create(Application);
   if (DesiredPage <> 0) then begin
      case DesiredPage of
          2 : OptionsForm.PageControl1.ActivePage := OptionsForm.TabSheet2;  //LAS
          9 : OptionsForm.PageControl1.ActivePage := OptionsForm.TabSheet9;
         10 : OptionsForm.PageControl1.ActivePage := OptionsForm.TabSheet10;
         17 : OptionsForm.PageControl1.ActivePage := OptionsForm.TabSheet17;
      end;
   end;
   OptionsForm.ShowModal;
end;


{$IfDef ShowLocalOptima}
   procedure ShowLocaOptimaOptions(Where : shortString);
   begin
      WriteLineToDebugFile('LocalOptimaOpts ' + Where);
      WriteLineToDebugFile('  grid incrs:  ' + IntToStr(MDDef.LocalOptimaOpts.ColInc) + ' and ' + IntToStr(MDDef.LocalOptimaOpts.RowInc));
      WriteLineToDebugFile('  Npts:  ' + IntToStr(MDDef.LocalOptimaOpts.Npts));
   end;
{$EndIf}


{$IfDef VCL}

      procedure SetHorizonOptions;
      var
         HorizonOptions : THorizonOptions;
      begin
         HorizonOptions := THorizonOptions.Create(Application);
         HorizonOptions.MapOwner := TheMap;
         HorizonOptions.Lat := Lat;
         HorizonOptions.Long := Long;
         if (Lat < -90) or (Long < -200) then begin
             HorizonOptions.BitBtn2.Enabled := false;
             HorizonOptions.BitBtn4.Enabled := false;
             HorizonOptions.BitBtn7.Enabled := false;
             HorizonOptions.BitBtn8.Enabled := false;
         end;
         //if (not DatesAllowed) then HorizonOptions.ClientWidth := 267;

         if (theMap = Nil) then begin
            HorizonOptions.ShowModal;
            HorizonOptions.Free;
         end
         else begin
            HorizonOptions.FormStyle := fsStayOnTop;
            HorizonOptions.Show;
         end;
      end;
{$EndIf}


{$IfDef ExGeostats}
{$Else}

      procedure GetGeomorphBlockOpts(WhatFor : tGeomporphBlock; DEM : integer; GridLimits : tGridLimits);
      var
        BlockOpsForm : TBlockOpsForm;
      begin
         {$IfDef OptionsProblems} WriteLineToDebugFile('GetGeomorphBlockOpts in'); {$EndIf}
         BlockOpsForm := TBlockOpsForm.Create(Application);
         if (WhatFor = gbGrid) then BlockOpsForm.PageControl1.ActivePage := BlockOpsForm.TabSheet2
         else BlockOpsForm.PageControl1.ActivePage := BlockOpsForm.TabSheet1;
         BlockOpsForm.SetForMode(WhatFor);
         BlockOpsForm.DEM := DEM;
         BlockOpsForm.GridLimits := GridLimits;
         BlockOpsForm.ShowNumbers;
         BlockOpsForm.Show;
         {$IfDef OptionsProblems} WriteLineToDebugFile('GetGeomorphBlockOpts out'); {$EndIf}
      end;

      procedure GetFabricOptions(DEMonMap : integer);
      begin
         FabricOptions := TFabricOptions.Create(Application);
         FabricOptions.BaseMap := DEMGlb[DEMonMap].SelectionMap;
         FabricOptions.Show;
      end;


      procedure PitSpireDefaults(var inMapOwner : tMapForm; PC : integer);
      var
         PitSpireForm: TPitSpireForm;
      begin
         {$IfDef RecordPitsSpires} WriteLineToDebugFile('PitSpireDefaults in'); {$EndIf}
         PitSpireForm := TPitSpireForm.Create(Application);
         PitSpireForm.Caption := 'Feature detection ' +  DEMGlb[inMapOwner.MapDraw.DEMonMap].AreaName;
         PitSpireForm.UseDEM := inMapOwner.MapDraw.DEMonMap;
         with PitSpireForm do begin
            if pc=1 then PageControl1.ActivePage := TabSheet1;
            if pc=2 then PageControl1.ActivePage := TabSheet2;
            if pc=3 then PageControl1.ActivePage := TabSheet3;
            if pc=4 then PageControl1.ActivePage := TabSheet4;
            if pc=5 then PageControl1.ActivePage := TabSheet5;
            PitSpireForm.MapOwner := inMapOwner;
            PitSpireForm.Show;
         end;
      end;


      procedure OptimaRegionsDefaults(DEM : integer);
      var
         RegOptsForm : Optima_reg.TRegOptsForm;
      begin
         {$IfDef ShowLocalOptima}  ShowLocaOptimaOptions('Start OptimaRegionsDefaults');   {$EndIf}

         RegOptsForm := Optima_reg.TRegOptsForm.Create(Application);
         with RegOptsForm,MDDef.LocalOptimaOpts do begin
            if (DEM <> 0) then with DEMGlb[DEM],DEMheader,SelectionMap,MapDraw.MapCorners do begin
               Label6.Caption := IntToStr(NumCol) + 'x' + IntToStr(NumRow) + ' points';
               Label7.Caption := IntToStr(succ(round(BoundBoxDataGrid.xmax-BoundBoxDataGrid.xmin))) +
                  'x' + IntToStr(succ(round(BoundBoxDataGrid.ymax-BoundBoxDataGrid.ymin))) + ' points';
               XSize := AverageXSpace;
               YSize := AverageYSpace;
               RegOptsForm.DEMUsed := DEM;
            end
            else begin
               XSize := -99;
               YSize := -99;
               DEMUsed := 0;
            end;

            Edit1.Text := IntToStr(ColInc);
            Edit2.Text := IntToStr(RowInc);
            Edit3.Text := IntToStr(NPts);
            Edit6.Text := IntToStr(MDDef.OptimaBoxSize);

            RadioGroup1.ItemIndex := ord(MDDef.LocalOptimaOpts_DEMRegion);
            EstimateResults;
            ShowModal;

            CheckEditString(Edit1.Text,ColInc);
            CheckEditString(Edit2.Text,RowInc);
            CheckEditString(Edit3.Text,NPts);
            CheckEditString(Edit6.Text,MDDef.OptimaBoxSize);

            MDDef.LocalOptimaOpts_DEMRegion := tDEMRegion(RadioGroup1.ItemIndex);
         end;
         {$IfDef ShowLocalOptima}  ShowLocaOptimaOptions('End OptimaRegionsDefaults'); {$EndIf}
      end;

{$EndIf}



procedure TOptionsForm.HelpBtnClick(Sender: TObject);
begin
  DisplayHTMLTopic('html\tbme1njp.htm');
end;


procedure TOptionsForm.KoppenClick(Sender: TObject);
begin
   {$IfDef ExGeography}
   {$Else}
      Koppen_opts.KoppenOptions;
   {$EndIf}
end;


procedure TOptionsForm.LabelDirectories;
var
   OnRow : integer;

   procedure DrawRow(var i : integer; First,Second : shortString);
   begin
      inc(OnRow);
      i := OnRow;
      StringGrid1.Cells[0,i] := First;
      StringGrid1.Cells[1,i] := Second;
   end;

begin
   StringGrid1.Cells[0,0] := 'Purpose';
   StringGrid1.Cells[1,0] := 'File/Path';

   OnRow := 0;
   DrawRow(MainMap,'Main map directory',MainMapData);
   DrawRow(EtopoRow,'ETOPO DEM',ETOPODEMName);
   DrawRow(Viewsheds,'Viewshed dir',SaveViewShedDir);
   DrawRow(VectorMap,'Vector map name',VectorMapName);

   {$IfDef ExGDAL}
   {$Else}
      DrawRow(FWT,'GDAL',GDALtools_Dir);
   {$EndIf}

   DrawRow(DEMIXbase,'DEMIX',MDDef.Demix_base_dir);
   //DrawRow(TauDEM,'TauDEM',TauDEMDir);

   StringGrid1.RowCount := OnRow + 1;
   StringGrid1.ColWidths[0] := 120;

   GroupBox7.Enabled := CheckBox88.Checked;
   Edit13.Enabled := CheckBox88.Checked;
   Edit14.Enabled := CheckBox88.Checked;
   {$IfDef ShowDirectories} WriteLineToDebugFile('TOptionsForm.LabelDirectories MDdef.MainMapData= ' + MainMapData); {$EndIf}
end;



procedure TOptionsForm.StringGrid1SelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
begin
   if (Arow > 0) then begin
      {$IfDef ShowDirectories} WriteLineToDebugFile('TOptionsForm.StringGrid1SelectCell, arow=' + IntToStr(ARow));{$EndIf}
      if Arow = MainMap then GetMapDataDirectory;
      if Arow = EtopoRow then GetFileFromDirectory('ETOPO DEM','*.dem',ETOPODEMName);
      if Arow = ViewSheds then GetDOSPath('Viewshed dir',SaveViewShedDir);
      if Arow = VectorMap then GetFileFromDirectory('Vector map name','*.prj',VectorMapName);
      {$IfDef ExGDAL}
      {$Else}
         if (Arow = FWT) then begin
            GDALtools_Dir := '';
            GetGDALFileNames;
         end;
      {$EndIf}
      if Arow = DEMIXbase then SetDEMIXdirs(true);
      //if Arow = TauDEM then GetDOSPath('TauDEM',TauDEMDir);
   end;
   LabelDirectories;
end;


procedure TOptionsForm.ShowDRGBoxes;
begin
   {$If Defined(ExDRGimport) or Defined(ExGeoPDF)}
      GroupBox15.Visible := false;
   {$Else}
      CheckBox7.Checked := MDdef.DRGQuadClip;
      CheckBox168.Checked := MDDef.DRGCollar;
      CheckBox170.Checked:= MDDef.DRGStructures;
      CheckBox171.Checked := MDDef.DRGTransport;
      CheckBox172.Checked := MDDef.DRGHydrography;
      CheckBox173.Checked := MDDef.DRGShadedRelief;
      CheckBox174.Checked := MDDef.DRGBoundaries;
      CheckBox175.Checked := MDDef.DRGOrthos;
      CheckBox176.Checked := MDDef.DRGGrid;
      CheckBox178.Checked := MDDef.DRGContours;
      CheckBox177.Checked := MDDef.DRGWoodland;
      CheckBox179.Checked := MDDef.DRGPLSS;
   {$EndIf}
end;



procedure TOptionsForm.FormCreate(Sender: TObject);
begin
   Caption := WMDEM.Caption + ' Options';
   UserWorking := false;
   {$IfDef HideHelpButtons}
      BitBtn3.Visible := false;
   {$EndIf}

   {$IfDef ExPLSS}
      Button21.Visible := false;
   {$EndIf}

   {$IfDef ExTiger}
      Button2.Visible := false;
   {$EndIf}

   {$IfDef ExPointCloud}
      RadioGroup1.Visible := false;
      RadioGroup35.Visible := false;
      CheckBox13.Visible := false;
      CheckBox14.Visible := false;
      CheckBox46.Visible := false;
      CheckBox47.Visible := false;
      CheckBox164.Visible := false;
   {$Else}
      CheckBox164.Checked := MDDef.PointCloudColorFilter;
      RadioGroup35.ItemIndex := MDDef.LasPointDensityBasedOn;
      CheckBox14.Checked := MDdef.LoadLASclassificationToMemory;
      CheckBox47.Checked := MDdef.LoadLASreturnNumberToMemory;
      CheckBox163.Checked := MDdef.LoadLASRGBToMemory;
   {$EndIf}

   RadioGroup1.ItemIndex := MDdef.LVISGabonGraphs;
   RadioGroup2.ItemIndex := ord(MDdef.CoordUse);
   RadioGroup3.ItemIndex := ord(MDdef.OutPutLatLongMethod);
   RadioGroup4.ItemIndex := ord(MDdef.CheckPoint);
   RadioGroup8.ItemIndex := ord(MDDef.dnConvert);

   RadioGroup10.ItemIndex := ord(MDDef.EnglishDistanceUnits);

   if MDdef.GraphicalCoordVerify then RadioGroup5.ItemIndex := 0
   else RadioGroup5.ItemIndex := 1;
   RadioGroup6.ItemIndex := MDdef.DefDEMMap;

   RadioGroup7.ItemIndex := ord(MDdef.ProgramOption);
   RadioGroup9.ItemIndex := ord(MDdef.OpennessHt);

   if MDdef.ElevDisplayMeters then RadioGroup11.ItemIndex := 0 else RadioGroup11.ItemIndex := 1;
   RadioGroup12.ItemIndex := ord(MDdef.MovieFormat);
   RadioGroup14.ItemIndex := ord(MDdef.OutputAzimuthMethod);
   if MDdef.DefaultLatHemi = 'N' then RadioGroup16.ItemIndex := 0
   else RadioGroup16.ItemIndex := 1;
   if MDdef.DefaultLongHemi = 'W' then RadioGroup17.ItemIndex := 0
   else RadioGroup17.ItemIndex := 1;
   RadioGroup18.ItemIndex := ord(MDDef.CanEditGIS);
   RadioGroup19.ItemIndex := ord(MDdef.DefaultVectorMapProjection);
   RadioGroup20.ItemIndex := ord(MDdef.VegOptionMap);
   RadioGroup22.ItemIndex := ord(MDdef.OutputPitchMethod);
   RadioGroup23.ItemIndex := ord(MDDef.SpeedUnit);
   RadioGroup24.ItemIndex := pred(MDDEF.PanOverlap);
   if MDDef.DoubleEtopoImport then RadioGroup25.ItemIndex := 1 else RadioGroup25.ItemIndex := 0;
   RadioGroup26.ItemIndex := ord(MDdef.AutoOpen);
   RadioGroup32.ItemIndex := MDDef.ClipboardExports;

   if MDDef.LeftLatGlobalDEM = 0 then RadioGroup33.ItemIndex := 1
   else RadioGroup33.ItemIndex := 0;

   RadioGroup34.ItemIndex := pred(MDDef.FrameLineWidth);

   Edit1.Text := MDDef.GoogleAPIkey;

   Edit19.Text := IntToStr(MDDef.LOSSliceBuffer);

   Edit10.Text := IntToStr(MDDef.NumMasksToAdd);
   Edit2.Text := IntToStr(MDDef.BlowUpExtraMargin);
   //Edit3.Text := IntToStr(MDdef.MaxMrSidImageSize);
   Edit4.Text := RealToString(MDdef.ScalebarDistortionTolerable,-8,-2);
   Edit5.Text := RealToString(MDDef.HistogramTailClipSize,-18,-4);
   Edit6.Text := RealToString(MDdef.MercShiftLongLimit,-12,-4);
   //Edit7.Text := IntToStr(MDdef.OGLDefs.MaxInitOpenGLTriangles);
   Edit8.Text := IntToStr(MDdef.FillHoleRadius);
   Edit11.Text := IntToStr(MDDef.JPEGQuality);
   Edit12.Text := IntToStr(MDDef.DEMIX_full);

   CheckBox28.Checked := MDDef.ShowPLSS;

   Edit13.Text := MDDef.DefaultServerIP;
   Edit14.Text := IntToStr(MDdef.DefaultServerPort);
   Edit15.Text := IntToStr(MDdef.MaxMapSize);

   {$IfDef ExFly}
   {$Else}
      Edit17.Text := IntToStr(MDdef.FlyOptions.LiveFlyDelay);
   {$EndIf}

   Edit18.Text := IntToStr(MDDef.MapSizeToVerify);
   Edit20.Text := IntToStr(MDDef.MemoryPointCloudMaxPts);

   Edit21.Text := RealToString(MDDef.BlowUpLatSize,-12,-8);
   Edit22.Text := RealToString(MDDef.BlowUpLongSize,-12,-8);
   Edit23.Text := IntToStr(MDdef.MaxThreadsForPC);
   Edit24.Text := IntToStr(MDDef.GridLabelDecimals);
   Edit26.Text := RealToString(MDDef.OpennessHowHigh,-12,-2);
   Edit28.Text := RealToString(MDDef.VegGridRandomizationDistance,-12,2);
   Edit29.Text := IntToStr(MDDef.DbMinIntFieldSize);
  //Edit31.Text := IntToStr(MDdef.OGLDefs.MaxOpenGLPoints);
   Edit32.Text := IntToStr(MDDef.CloudOpenGLThinFactor);
   Edit33.Text := RealToString(MDDef.MinMaxGridTolerance,-12,-2);
   Edit37.Text := IntToStr(MDDef.UpdateDelay);

   Edit27.Text := IntToStr(MDDef.GeoJSONG_zdec);
   Edit34.Text := IntToStr(MDDef.GeoJSONP_xydec);
   Edit35.Text := IntToStr(MDDef.GeoJSONP_zdec);

   CheckBox6.Checked := MDDef.RoamAllZ;
   Label36.Caption := IntToStr(MDDef.DefaultGraphXSize) + 'x' + IntToStr(MDDef.DefaultGraphYSize);
   Label23.Caption := IntToStr(MDDef.DefaultTerrainXSize) + 'x' + IntToStr(MDDef.DefaultTerrainYSize);

   CheckBox58.Checked := MDdef.doMeanDEM;
   CheckBox75.Checked := MDdef.doEnvDEM;
   CheckBox67.Checked := MDdef.doNPTsDEM;
   CheckBox65.Checked := MDdef.doSTDDEM;
   CheckBox63.Checked := MDdef.doMedDEM;
   CheckBox71.Checked := MDdef.doFloorDEM;
   CheckBox81.Checked := MDdef.ConfirmDBEdits;
   CheckBox74.Checked := MDdef.doCeilingDEM;
   CheckBox77.Checked := MDdef.AnyNoDataMeansNoData;
   CheckBox147.Checked := MDdef.doDEMwithMin;
   CheckBox148.Checked := MDdef.doDEMwithMax;
   CheckBox1.Checked := MDdef.DualElevs;
   CheckBox2.Checked := MDdef.SatImageCoords;
   CheckBox3.Checked := MDdef.ShowMapToolbar;
   CheckBox4.Checked := MDdef.ShowMainToolbar;
   CheckBox5.Checked := MDDef.AskAboutSIDLevel;
   CheckBox9.Checked := MDdef.ShowSHPButton;
   UpDown1.Position := MDdef.StatSampleIncr;
   CheckBox12.Checked := MDdef.StatGrafReverseYAxis;
   CheckBox13.Checked := MDdef.FilterGridsToEdge;
   CheckBox16.Checked := MDdef.UseSealevel;
   CheckBox17.Checked := MDdef.DBsOnAllMaps;
   CheckBox18.Checked := MDdef.IsUSNAcomputer;
   CheckBox20.Checked := MDdef.MissingToSeaLevel;
   CheckBox21.Checked := MDdef.DeleteAuxTiffFiles;
   CheckBox22.Checked := MDdef.DefaultEditDBsInGrid;

   ShowDRGBoxes;

   {$IfDef ExGDAL}
   {$Else}
      CheckBox23.Checked := MDDef.RouteGeotiffExportGDAL;
   {$EndIf}

   //CheckBox25.Checked := Mddef.HydrologyEnforceProfile;
   CheckBox26.Checked := MDdef.WrapETOPO;
   //CheckBox27.Checked := MDdef.BothDatumsWhileRoam;
   CheckBox29.Checked := MDdef.AutoAssignNameField;
   CheckBox30.Checked := MDdef.MGRSandLatLongWhileRoam;

   {$IfDef RecordProblems}
      CheckBox31.Checked := MDdef.MDRecordDebugLog;
   {$Else}
      CheckBox31.Visible := false;
   {$EndIf}

   CheckBox32.Checked := MDdef.ShowDBOnly;
   CheckBox33.Checked := MDdef.AutoFillHoles;
   CheckBox34.Checked := MDdef.AutoCloseIndexMaps;
   CheckBox35.Checked := MDdef.AllowDBsToRAM;
   CheckBox36.Checked := MDdef.AddFitNav;


   CheckBox37.Checked := MDdef.AllowMemoryLinkDB;
   CheckBox38.Checked := MDdef.DupeImportsAllowed;

   CheckBox39.Checked := MDdef.WorldOutlinesOnGlobalDEM;
   CheckBox40.Checked := MDdef.AverageImageReadings;
   CheckBox41.Checked := MDdef.WorldOutlinesOnGlobalBlueMarble;
   CheckBox42.Checked := MDDef.ModalDBDisplay;
   CheckBox43.Checked := MDDef.TransparentIcons;
   //CheckBox44.Checked := MDDef.SmoothThalwegs;

   CheckBox45.Checked := MDdef.IgnoreHistogramZero;
   CheckBox46.Checked := MDdef.ShowClimateStationDB;
   CheckBox48.Checked := MDDef.SkipWebUpdates;
   CheckBox49.Checked := MDDef.AspectBoxRegion;

   CheckBox51.Checked := MDDef.UseMapPanButtons;
   CheckBox52.Checked := MDDef.NoDEMInterpolations;

   CheckBox56.Checked := MDdef.PromptToSaveNewDEMs;
   CheckBox59.Checked := MDdef.GrayscaleMerges;
   CheckBox60.Checked := MDDef.DBRecShowToolbarTop;
   CheckBox61.Checked := MDDef.GISSecondToolbar;
   CheckBox62.Checked := MDDef.ShowRegionBoundaries;
   CheckBox64.Checked := MDDef.AssumeMinus999999Missing;

   CheckBox66.Checked := MDDef.ShiftMercToUTM;
   CheckBox69.Checked := MDDef.SaveDBStatus;
   CheckBox165.Checked := MDDef.SaveDBFilter;
   CheckBox166.Checked := MDDef.StayAwake;
   CheckBox70.Checked := MDDef.ImageryIconDirs;
   CheckBox72.Checked := MDDef.ShowIntDB;
   CheckBox73.Checked := MDDef.TransparentGIF;
   CheckBox76.Checked := MDDef.TernaryPercentageValues;
   CheckBox79.Checked := MDDef.DeleteFIT;

   CheckBox82.Checked := MDDef.DBMinimizeOnOpen;
   CheckBox83.Checked := MDDef.CleanKMLDirOnClosing;
   CheckBox84.Checked := MDDef.QuickShapeFileCoding;
   CheckBox85.Checked := MDDef.MakeOGLMovie;
   CheckBox86.Checked := MDDef.BackupEXEbeforeUpdate;

   CheckBox88.Checked := MDDef.EnableGridNetworkComputing;

   CheckBox89.Checked := MDDef.DeleteTarGZ;

   CheckBox90.Checked := MDDef.VegDensityGroundPoints;

   CheckBox91.Checked := MDDef.AssumeNegativeValuesMissing;

  {$IfDef ExSidescan}
  {$Else}
   CheckBox93.Checked := MDDef.SonarMapDef.CustomPalette;
   {$EndIf}

   CheckBox94.Checked := MDDef.VegDensityRandomizePoints;
   CheckBox95.Checked := MDDef.AlwaysShowMapCoordinates;

   CheckBox96.Checked := MDDef.AvoidTextOverprints;
   CheckBox97.Checked := MDDef.BoxAroundQuickMaps;
   CheckBox98.Checked:= MDDef.AssumeMinus99Missing;
   CheckBox99.Checked := MDDef.AssumeMinus999Missing;
   CheckBox100.Checked := MDDef.AssumeMinus9999Missing;
   CheckBox55.Checked := MDDef.AssumeMinus99999Missing;
   CheckBox54.Checked := MDDef.AssumeMinus32767Missing;

   CheckBox101.Checked := MDDef.RotatingEarthOutlines;
   CheckBox102.Checked := MDDef.DeleteJP2;
   CheckBox103.Checked := MDDef.ShowPointClouds;

   CheckBox104.Checked := MDDef.VegDensityBuildingPoints;
   CheckBox107.Checked := MDDef.TerrainCatPercentages;

   CheckBox108.Checked := MDDef.MercShiftSingleZone;

   CheckBox110.Checked := MDDef.ShowRoamOnAllMaps;

   CheckBox113.Checked := MDDef.ShowGlobalDEM;
   CheckBox114.Checked := MDDef.ShowBlueMarble;
   CheckBox115.Checked := MDDef.Create3DShapefiles;
   CheckBox117.Checked := MDDef.SaveIntermediateDBs;

   CheckBox118.Checked := MDDef.ShowNewFieldStats;
   CheckBox120.Checked := MDDef.ShowMenus;
   CheckBox121.Checked := MDDef.TrackDatabaseRanges;
   CheckBox122.Checked := MDDef.OpenVegGridMap;

   CheckBox123.Checked := MDDef.AutoElevationReset;
   CheckBox125.Checked := MDDef.ConfirmOpennesDirections;
   CheckBox127.Checked := MDDef.LooseFillHoles;
   CheckBox129.Checked := MDDef.MultipleDBFiltersForGraphing;
   CheckBox130.Checked := MDDef.VerifyAllWorldFiles;
   CheckBox131.Checked := MDDef.OpenGL_VE;

   CheckBox133.Checked := MDDef.ShowDEMGridCoords;

   CheckBox136.Checked := MDDef.UsePixelSizeRules;

   CheckBox140.Checked := MDDef.SatMultiBandTrueColor;
   CheckBox141.Checked := MDDef.SatMultiBandNormalize;
   CheckBox142.Checked := MDDef.ShowLabs;
   CheckBox143.Checked := MDDef.BandsByWavelength;
   CheckBox145.Checked := MDDef.OpenGLCleanOverlays;
   CheckBox146.Checked := MDDef.ShowProjectedCoordinates;

   {$IfDef AllowGeomorphometry}
   CheckBox149.Checked := MDDef.CumFreqNormAxis;
   {$EndIf}

   CheckBox150.Checked := MDDef.QuantileRanges;
   CheckBox151.Checked := MDDef.ShowViews;
   CheckBox152.Checked := MDDef.GeomorphMapsFullDEM;
   CheckBox153.Checked := MDDef.DB_ID_grids;
   CheckBox155.Checked := MDDef.AskHistogramBins;
   CheckBox156.Checked := MDDef.AdvancedDBops;

   CheckBox157.Checked := MDDef.ShowWinExec;
   //CheckBox158.Checked := MDDef.AspectContinuous;
   CheckBox159.Checked := MDDef.MapLimitDB;
   CheckBox161.Checked := MDDef.TigertoCDS;
   CheckBox300.Checked := MDDef.SeaLevelToMissing;

   {$IfDef ExOSM}
      CheckBox117.Visible := false;
   {$Else}
      CheckBox167.Checked := MDDef.OSMtoCDS;
   {$EndIf}

   CheckBox24.Checked := MDDef.AutoLoadVegGrid;
   CheckBox78.Checked := MDDef.AutoLoadVegDensityGrids;
   CheckBox87.Checked := MDDef.VegDensityGraphAverage;
   Edit25.Text := IntToStr(MDDef.VegDensityGraphMaxDensity);

   {$IfDef ExOceanography}
      PageControl1.Pages[18].TabVisible := false;
      GroupBox4.Visible := false;
   {$Else}
      CheckBox105.Checked := MDDef.ShowOceanographyOptions;
      CheckBox111.Checked := MDDef.ShowSidescan;
      CheckBox112.Checked := MDDef.ShowSubbottom;
   {$EndIf}

   {$IfDef ExPointCloud}
      PageControl1.Pages[16].TabVisible := false;
   {$EndIf}

   {$IfDef ExGeology}
      GroupBox9.Visible := false;
      PageControl1.Pages[5].TabVisible := false;
   {$Else}
      CheckBox15.Checked := MDdef.ShowPlateRotation;
      CheckBox119.Checked := MDDef.ShowGeologyOptions;
      CheckBox126.Checked := MDDef.ShowStereoNet;
      CheckBox132.Checked := MDDef.ShowStratCol;
      CheckBox134.Checked := MDDef.ShowTernary;
      CheckBox135.Checked := MDDef.ShowMarineGeology;
      CheckBox137.Checked := MDDef.ShowSieve;
      CheckBox138.Checked := MDDef.ShowGeomorphometry;
      CheckBox53.Checked := MDDef.MoveGeographyDBMemory;
      CheckBox57.Checked := MDDef.MoveGeologyDBMemory;
   {$EndIf}

   {$IfDef ExDP}
   {$Else}
      if (MDdef.ProgramOption = DragonPlotProgram) then begin
         RadioGroup7.Visible := false;
         RadioGroup19.Visible := false;
         RadioGroup26.Visible := false;
         GroupBox6.Visible := false;
         GroupBox8.Visible := false;
         BitBtn14.Visible := false;
         BitBtn31.Visible := false;
         CheckBox39.Visible := false;
         CheckBox41.Visible := false;
         CheckBox101.Visible := false;
      end;
   {$EndIf}

   CheckBox5.Checked := MDDef.DEMIX_DoCHM;
   CheckBox154.Checked := MDDef.DEMIX_DoAirOrDirt;
   CheckBox158.Checked := MDDef.DEMIX_DoElevDiff;
   CheckBox162.Checked := MDDef.DEMIX_DoSlopeDiff;
   CheckBox169.Checked := MDDef.DEMIX_DoRuffDiff;
   CheckBox180.Checked := MDDef.DEMIX_DoHalfSecDEMs;

   if MdDEF.DefaultUTMZone in [1..60] then Label1.Caption := IntToStr(MdDEF.DefaultUTMZone) + '  (' + UTMZoneExtent(MdDEF.DefaultUTMZone) + ')';
   Label6.Caption := 'Windows reports processors=' + IntToStr(GetNumberProcessors);
   Label5.Caption := DatumName(MDDef.PreferPrimaryDatum);
   Label7.Caption := IntegerToString(MDdef.StatSampleIncr,3) + ': Statistical sampling interval';
   Label11.Caption := IntToStr(MDdef.xAspect) + ':' + IntToStr(MDdef.YAspect);
   Label8.Caption := IntToStr(MDdef.DefaultMapXSize) + ' by ' + IntToStr(MDdef.DefaultMapYSize);
   Label30.Caption := DatumName(MDdef.DefaultDigitizeDatum);
   ColorBitBtn(BitBtn1,MDdef.MissingDataColor);
   ColorBitBtn(BitBtn36,MDdef.BlankMapColor);

   ColorBitBtn(BitBtn25,MDDef.RoamSecondMapColor);
   Petmar.SymbolOnButton(BitBtn15,MDDef.DefGISSymbol);
   Petmar.SymbolOnButton(BitBtn16,MDDef.KeyLocationSymbol);

   Petmar.ColorLineWidthBitBtn(OutlineButton,MDDef.DBOutlineColor,MDDef.DBOutlineWidth);
   Petmar.ColorLineWidthBitBtn(BitBtn34,MDDef.CountryOutline_Color,MDDef.CountryOUtline_Width);
   Petmar.ColorLineWidthBitBtn(BitBtn35,MDDef.ProvinceOutline_Color,MDDef.ProvinceOUtline_Width);

    {$IfDef ExACOLITE}
       RadioGroup13.Visible := false;
    {$Else}
       Edit9.Text := MDDef.l2w_Params;
       CheckBox128.Checked := MDDef.acolite_delete_nc;
       CheckBox144.Checked := MDDef.acolite_delete_misc;
       CheckBox92.Checked := MDDef.acolite_delete_rhos;
       CheckBox116.Checked := MDDef.acolite_delete_rhot;
       RadioGroup15.ItemIndex := MDDef.acoliteS2res;
    {$EndIf}

   Label20.Caption := MapLibDir;
   LabelDirectories;
   if (Sender <> Nil) then begin
      if Width > WMDEM.ClientWidth then Width := WMDEM.ClientWidth - 50;
      Petmar.PlaceFormAtMousePosition(Self);
      PageControl1.ActivePage := TabSheet1;
   end;
   UserWorking := true;
   ProgramModeChanged := false;
end;


procedure TOptionsForm.FormResize(Sender: TObject);
begin
  inherited;
   StringGrid1.ColWidths[0] := 120;
   StringGrid1.ColWidths[1] := PageControl1.ClientWidth - 120;
end;


procedure TOptionsForm.GDALClick(Sender: TObject);
begin
   {$IfDef ExGDAL}
   {$Else}
      MDDef.DontBugMeAboutGDAL := false;
      GetGDALFileNames;
   {$EndIf}
end;

procedure TOptionsForm.GraphClick(Sender: TObject);
begin
    GetNewBMPSize(MDDef.DefaultGraphXSize,MDDef.DefaultGraphYSize,'Graph size');
    Label36.Caption := IntToStr(MDDef.DefaultGraphXSize) + 'x' + IntToStr(MDDef.DefaultGraphYSize);
end;


procedure TOptionsForm.OKBtnClick(Sender: TObject);
var
   fName : PathStr;
   SaveLabs : boolean;
begin
   {$IfDef OptionsProblems} WriteLineToDebugFile('TOptionsForm.OKBtnClick in'); {$EndIf}

   {$IFDef ExPointCloud}
   {$Else}
      MDDef.LasPointDensityBasedOn := RadioGroup35.ItemIndex;
      MDdef.LoadLASclassificationToMemory := CheckBox14.Checked;
      MDdef.LoadLASreturnNumberToMemory := CheckBox47.Checked;
      MDdef.LoadLASRGBToMemory := CheckBox163.Checked;
      MDDef.PointCloudColorFilter := CheckBox164.Checked;
   {$EndIf}

   {$IfDef ExGDAL}
   {$Else}
      MDDef.RouteGeotiffExportGDAL := CheckBox23.Checked;
   {$EndIf}

   {$IfDef RecordProblems}
      MDdef.MDRecordDebugLog := CheckBox31.Checked;
   {$EndIf}

   {$IfDef ExSidescan}
   {$Else}
      MDDef.SonarMapDef.CustomPalette := CheckBox93.Checked;
   {$EndIf}

   {$IfDef ExGeology}
   {$Else}
      MDDef.ShowGeologyOptions := CheckBox119.Checked;
      MDDef.ShowStereoNet := CheckBox126.Checked;
      MDDef.ShowStratCol := CheckBox132.Checked;
      MDDef.ShowTernary := CheckBox134.Checked;
      MDDef.ShowMarineGeology := CheckBox135.Checked;
      MDDef.ShowGeomorphometry := CheckBox138.Checked;
      MDDef.MoveGeologyDBMemory := CheckBox57.Checked;
   {$EndIf}

   {$IfDef ExGeography}
   {$Else}
      MDDef.MoveGeographyDBMemory := CheckBox53.Checked;
   {$EndIf}

   {$IFDef ExPointCloud}
   {$Else}
   if (MDDef.MemoryPointCloudMaxPts > TheMaxPointsMemPtCloud) then MDDef.MemoryPointCloudMaxPts := TheMaxPointsMemPtCloud;
   {$EndIf}

   {$IfDef ExOSM}
   {$Else}
   MDDef.OSMtoCDS := CheckBox167.Checked;
   {$EndIf}

   {$IfDef AllowGeomorphometry}
   MDDef.CumFreqNormAxis := CheckBox149.Checked;
   {$EndIf}

   {$IfDef ExDRGimport}
   {$Else}
      MDDef.DRGCollar := CheckBox168.Checked;
      MDDef.DRGStructures := CheckBox170.Checked;
      MDDef.DRGTransport := CheckBox171.Checked;
      MDDef.DRGHydrography := CheckBox172.Checked;
      MDDef.DRGShadedRelief := CheckBox173.Checked;
      MDDef.DRGBoundaries := CheckBox174.Checked;
      MDDef.DRGOrthos := CheckBox175.Checked;
      MDDef.DRGGrid := CheckBox176.Checked;
      MDDef.DRGContours := CheckBox178.Checked;
      MDDef.DRGWoodland := CheckBox177.Checked;
      MDDef.DeleteFIT := CheckBox79.Checked;
      MDDef.DRGPLSS := CheckBox179.Checked;
      MDdef.DRGQuadClip := CheckBox7.Checked;
   {$EndIf}

   {$IfDef ExFly}
   {$Else}
   CheckEditString(Edit17.Text,MDdef.FlyOptions.LiveFlyDelay);
   {$EndIf}

    {$IfDef ExACOLITE}
    {$Else}
       MDDef.l2w_Params := Edit9.Text;
       MDDef.acolite_delete_nc := CheckBox128.Checked;
       MDDef.acolite_delete_misc := CheckBox144.Checked;
       MDDef.acolite_delete_rhos := CheckBox92.Checked;
       MDDef.acolite_delete_rhot := CheckBox116.Checked;
       MDDef.acoliteS2res := RadioGroup15.ItemIndex;
    {$EndIf}

   MDdef.LVISGabonGraphs:= RadioGroup1.ItemIndex;

   MDdef.coordUse := tCoordUse(RadioGroup2.ItemIndex);
   MDdef.OutPutLatLongMethod := tLatLongMethod(RadioGroup3.ItemIndex);
   MDdef.CheckPoint := tCheckPoint(RadioGroup4.ItemIndex);
   MDdef.GraphicalCoordVerify := RadioGroup5.ItemIndex = 0;
   MDdef.DefDEMMap := RadioGroup6.ItemIndex;
   MDDef.dnConvert := tdnConvert(RadioGroup8.ItemIndex);

   MDdef.OpennessHt := RadioGroup9.ItemIndex;
   MDDef.EnglishDistanceUnits := tDistanceUnits(RadioGroup10.ItemIndex);

   MDdef.ElevDisplayMeters := RadioGroup11.ItemIndex = 0;
   MDdef.MovieFormat := tMovieFormat(RadioGroup12.ItemIndex);
   MDdef.OutputAzimuthMethod := tLatLongMethod(RadioGroup14.ItemIndex);

   if (RadioGroup16.ItemIndex = 0) then MDdef.DefaultLatHemi := 'N' else MDdef.DefaultLatHemi := 'S';
   if (RadioGroup17.ItemIndex = 0) then MDdef.DefaultLongHemi := 'W' else MDdef.DefaultLongHemi := 'E';

   MDDef.CanEditGIS := tCanEditGIS(RadioGroup18.ItemIndex);
   MDdef.DefaultVectorMapProjection := tDefaultVectorMapProject(RadioGroup19.ItemIndex);
   MDdef.VegOptionMap := tVegOptionMap(RadioGroup20.ItemIndex);
   MDdef.OutputPitchMethod := tLatLongMethod(RadioGroup22.ItemIndex);
   MDDef.SpeedUnit := tSpeedUnit(RadioGroup23.ItemIndex);
   MDDEF.PanOverlap := succ(RadioGroup24.ItemIndex);
   MDDef.DoubleEtopoImport := RadioGroup25.ItemIndex = 1;
   MDDef.AutoOpen := tAutoOpen(RadioGroup26.ItemIndex);

   MDDef.ClipboardExports := RadioGroup32.ItemIndex;
   MDDef.FrameLineWidth := succ(RadioGroup34.ItemIndex);
   MDdef.DualElevs := CheckBox1.Checked;
   MDdef.SatImageCoords := CheckBox2.Checked;
   MDdef.ShowMapToolbar := CheckBox3.Checked;
   MDdef.ShowMainToolbar := CheckBox4.Checked;
   MDDef.AskAboutSIDLevel := CheckBox5.Checked;
   MDDef.RoamAllZ := CheckBox6.Checked;
   MDdef.ShowSHPButton:= CheckBox9.Checked;
   //MDdef.AutoEnhanceShapeFiles := CheckBox10.Checked;
   MDdef.StatGrafReverseYAxis := CheckBox12.Checked;
   MDdef.FilterGridsToEdge := CheckBox13.Checked;

   MDdef.ShowPlateRotation := CheckBox15.Checked;
   MDdef.UseSealevel := CheckBox16.Checked;
   MDdef.DBsOnAllMaps := CheckBox17.Checked;
   MDdef.IsUSNAcomputer := CheckBox18.Checked;

   MDdef.MissingToSeaLevel := CheckBox20.Checked;
   MDdef.DeleteAuxTiffFiles := CheckBox21.Checked;

   MDdef.DefaultEditDBsInGrid := CheckBox22.Checked;

   //Mddef.HydrologyEnforceProfile := CheckBox25.Checked;
   MDdef.WrapETOPO := CheckBox26.Checked;
   //MDdef.BothDatumsWhileRoam := CheckBox27.Checked;

   MDDef.ShowPLSS := CheckBox28.Checked;

   MDdef.AutoAssignNameField := CheckBox29.Checked;
   MDdef.MGRSandLatLongWhileRoam := CheckBox30.Checked;
   MDdef.ShowDBOnly := CheckBox32.Checked;
   MDdef.AutoFillHoles := CheckBox33.Checked;
   MDdef.AutoCloseIndexMaps := CheckBox34.Checked;
   MDdef.AllowDBsToRAM := CheckBox35.Checked;
   MDdef.AddFitNav := CheckBox36.Checked;

   MDdef.AllowMemoryLinkDB := CheckBox37.Checked;
   MDdef.DupeImportsAllowed := CheckBox38.Checked;

   MDdef.WorldOutlinesOnGlobalDEM := CheckBox39.Checked;
   MDdef.WorldOutlinesOnGlobalBlueMarble := CheckBox41.Checked;
   MDDef.ModalDBDisplay := CheckBox42.Checked;
   MDDef.TransparentIcons := CheckBox43.Checked;
   //MDDef.SmoothThalwegs := CheckBox44.Checked;

   MDdef.AverageImageReadings := CheckBox40.Checked;
   MDDef.AspectBoxRegion := CheckBox49.Checked;
   MDDef.UseMapPanButtons := CheckBox51.Checked;
   MDdef.IgnoreHistogramZero := CheckBox45.Checked;
   MDdef.ShowClimateStationDB := CheckBox46.Checked;

   MDDef.SkipWebUpdates := CheckBox48.Checked;

   MDdef.NoDEMInterpolations := CheckBox52.Checked;

   MDDef.AssumeMinus32767Missing := CheckBox54.Checked;
   MDDef.AssumeMinus99999Missing := CheckBox55.Checked;

   MDdef.PromptToSaveNewDEMs := CheckBox56.Checked;

   MDdef.GrayscaleMerges := CheckBox59.Checked;
   MDDef.DBRecShowToolbarTop := CheckBox60.Checked;
   MDDef.GISSecondToolbar := CheckBox61.Checked;
   MDDef.ShowRegionBoundaries := CheckBox62.Checked;
   MDDef.AssumeMinus999999Missing := CheckBox64.Checked;

   MDDef.ShiftMercToUTM := CheckBox66.Checked;
   MDDef.SaveDBStatus := CheckBox69.Checked;
   MDDef.SaveDBFilter := CheckBox165.Checked;
   MDDef.StayAwake := CheckBox166.Checked;
   MDDef.ImageryIconDirs := CheckBox70.Checked;

   MDDef.ShowIntDB := CheckBox72.Checked;

   MDDef.TransparentGIF := CheckBox73.Checked;
   MDDef.TernaryPercentageValues := CheckBox76.Checked;
   MDdef.ConfirmDBEdits := CheckBox81.Checked;

   MDDef.DBMinimizeOnOpen := CheckBox82.Checked;

   MDDef.CleanKMLDirOnClosing := CheckBox83.Checked;
   MDDef.QuickShapeFileCoding := CheckBox84.Checked;
   MDDef.MakeOGLMovie := CheckBox85.Checked;
   MDDef.BackupEXEbeforeUpdate := CheckBox86.Checked;

   MDDef.EnableGridNetworkComputing := CheckBox88.Checked;

   MDDef.DeleteTarGZ := CheckBox89.Checked;

   MDDef.VegDensityGroundPoints := CheckBox90.Checked;
   MDDef.AssumeNegativeValuesMissing := CheckBox91.Checked;
   MDDef.VegDensityRandomizePoints := CheckBox94.Checked;
   MDDef.AlwaysShowMapCoordinates := CheckBox95.Checked;

   MDDef.AvoidTextOverprints := CheckBox96.Checked;
   MDDef.BoxAroundQuickMaps := CheckBox97.Checked;

   MDDef.AssumeMinus99Missing := CheckBox98.Checked;
   MDDef.AssumeMinus999Missing := CheckBox99.Checked;
   MDDef.AssumeMinus9999Missing := CheckBox100.Checked;
   MDDef.RotatingEarthOutlines := CheckBox101.Checked;
   MDDef.DeleteJP2 := CheckBox102.Checked;
   MDDef.ShowPointClouds := CheckBox103.Checked;
   MDDef.VegDensityBuildingPoints := CheckBox104.Checked;

   MDDef.TerrainCatPercentages := CheckBox107.Checked;

   MDDef.MercShiftSingleZone := CheckBox108.Checked;
   MDDef.ShowRoamOnAllMaps := CheckBox110.Checked;
   MDDef.Create3DShapefiles := CheckBox115.Checked;
   MDDef.SaveIntermediateDBs := CheckBox117.Checked;
   MDDef.ShowNewFieldStats := CheckBox118.Checked;

   MDDef.ShowMenus := CheckBox120.Checked;
   MDDef.TrackDatabaseRanges := CheckBox121.Checked;
   MDDef.OpenVegGridMap := CheckBox122.Checked;
   MDDef.AutoElevationReset := CheckBox123.Checked;
   MDDef.ConfirmOpennesDirections := CheckBox125.Checked;
   MDDef.LooseFillHoles := CheckBox127.Checked;
   MDDef.MultipleDBFiltersForGraphing := CheckBox129.Checked;

   MDDef.VerifyAllWorldFiles := CheckBox130.Checked;

   MDDef.OpenGL_VE := CheckBox131.Checked;
   MDDef.ShowDEMGridCoords := CheckBox133.Checked;

   MDDef.UsePixelSizeRules := CheckBox136.Checked;
   MDDef.ShowSieve := CheckBox137.Checked;

   MDDef.OpenGLCleanOverlays := CheckBox145.Checked;

   MDDef.TigerToCDS := CheckBox161.Checked;
   MDDef.SeaLevelToMissing := CheckBox300.Checked;

   MDdef.doMeanDEM :=  CheckBox58.Checked;
   MDdef.doEnvDEM := CheckBox75.Checked;
   MDdef.doNPTsDEM := CheckBox67.Checked;
   MDdef.doSTDDEM := CheckBox65.Checked;
   MDdef.doMedDEM := CheckBox63.Checked;
   MDdef.doFloorDEM := CheckBox71.Checked;
   MDdef.doCeilingDEM := CheckBox74.Checked;

   MDdef.doDEMwithMin := CheckBox147.Checked;
   MDdef.doDEMwithMax := CheckBox148.Checked;

   MDdef.AnyNoDataMeansNoData := CheckBox77.Checked;
   //MDDef.RecordWMSrequests :=  CheckBox80.Checked;

   MDDef.AutoLoadVegGrid := CheckBox24.Checked;
   MDDef.AutoLoadVegDensityGrids := CheckBox78.Checked;
   MDDef.VegDensityGraphAverage := CheckBox87.Checked;

   MDDef.ShowOceanographyOptions := CheckBox105.Checked;
   MDDef.ShowSidescan := CheckBox111.Checked;
   MDDef.ShowSubbottom := CheckBox112.Checked;
   MDDef.ShowGlobalDEM := CheckBox113.Checked;
   MDDef.ShowBlueMarble := CheckBox114.Checked;

   MDDef.SatMultiBandTrueColor := CheckBox140.Checked;
   MDDef.SatMultiBandNormalize := CheckBox141.Checked;
   MDDef.ShowLabs := CheckBox142.Checked;
   MDDef.BandsByWavelength := CheckBox143.Checked;
   MDDef.ShowProjectedCoordinates := CheckBox146.Checked;

   MDDef.QuantileRanges := CheckBox150.Checked;
   MDDef.ShowViews := CheckBox151.Checked;
   MDDef.GeomorphMapsFullDEM := CheckBox152.Checked;
   MDDef.DB_ID_grids := CheckBox153.Checked;
   MDDef.AskHistogramBins := CheckBox155.Checked;
   MDDef.AdvancedDBops := CheckBox156.Checked;
   MDDef.ShowWinExec := CheckBox157.Checked;
   //MDDef.AspectContinuous := CheckBox158.Checked;
   MDDef.MapLimitDB := CheckBox159.Checked;
   MDDef.AutoGrayScaleReflectance := CheckBox160.Checked;


   MDDef.DEMIX_DoCHM := CheckBox5.Checked;
   MDDef.DEMIX_DoAirOrDirt := CheckBox154.Checked;
   MDDef.DEMIX_DoElevDiff  := CheckBox158.Checked;
   MDDef.DEMIX_DoSlopeDiff := CheckBox162.Checked;
   MDDef.DEMIX_DoRuffDiff := CheckBox169.Checked;
   MDDef.DEMIX_DoHalfSecDEMs := CheckBox180.Checked;

   MDdef.StatSampleIncr := UpDown1.Position;

   CheckEditString(Edit19.Text,MDDef.LOSSliceBuffer);
   MDDef.GoogleAPIkey := Edit1.Text;

   CheckEditString(Edit2.Text,MDDef.BlowUpExtraMargin);
   //CheckEditString(Edit3.Text,MDdef.MaxMrSidImageSize);
   CheckEditString(Edit4.Text,MDdef.ScalebarDistortionTolerable);
   CheckEditString(Edit5.Text,MDDef.HistogramTailClipSize);
   CheckEditString(Edit6.Text,MDdef.MercShiftLongLimit);

   //CheckEditString(Edit7.Text,MDdef.OGLDefs.MaxInitOpenGLTriangles);
   CheckEditString(Edit8.Text,MDdef.FillHoleRadius);
   CheckEditString(Edit11.Text,MDDef.JPEGQuality);
   CheckEditString(Edit12.Text,MDDef.DEMIX_full);


   MDDef.DefaultServerIP := Edit13.Text;
   MDDef.DefaultServerPort := StrToInt(Edit14.Text);

   CheckEditString(Edit15.Text,MDdef.MaxMapSize);
   CheckEditString(Edit18.Text,MDDef.MapSizeToVerify);
   CheckEditString(Edit20.Text,MDDef.MemoryPointCloudMaxPts);

   CheckEditString(Edit21.Text,MDDef.BlowUpLatSize);
   CheckEditString(Edit22.Text,MDDef.BlowUpLongSize);
   CheckEditString(Edit23.Text,MDdef.MaxThreadsForPC);
   CheckEditString(Edit24.Text,MDDef.GridLabelDecimals);
   CheckEditString(Edit26.Text,MDDef.OpennessHowHigh);
   CheckEditString(Edit28.Text,MDDef.VegGridRandomizationDistance);
   CheckEditString(Edit29.Text,MDDef.DbMinIntFieldSize);
   //CheckEditString(Edit31.Text,MDdef.OGLDefs.MaxOpenGLPoints);
   CheckEditString(Edit32.Text,MDDef.CloudOpenGLThinFactor);
   CheckEditString(Edit33.Text,MDDef.MinMaxGridTolerance);
   CheckEditString(Edit27.Text,MDDef.GeoJSONG_zdec);
   CheckEditString(Edit34.Text,MDDef.GeoJSONP_xydec);
   CheckEditString(Edit35.Text,MDDef.GeoJSONP_zdec);
   CheckEditString(Edit37.Text,MDDef.UpdateDelay);

   if (MDdef.MaxMapSize > MaxScreenXMax) then MDdef.MaxMapSize := MaxScreenXMax;

   CheckEditString(Edit10.Text,MDDef.NumMasksToAdd);

   Edit10.Text := IntToStr(MDDef.NumMasksToAdd);

   //SetGlobalDefaults;

   Saved := true;

   {$IfDef NoModeChanges}
   {$Else}
   if ProgramModeChanged then begin
      {$IfDef RecordNaturalEarthFileNames}
         WriteLineToDebugFile('Start mode change');
         WriteLineToDebugFile('   small: ' + SmallScaleWorldOutlines);
         WriteLineToDebugFile('  medium: ' + MedScaleWorldOutlines);
         WriteLineToDebugFile('   large: ' + LargeScaleWorldOutlines);
      {$EndIf}
      {$IfDef OptionsProblems} WriteLineToDebugFile('ChangeOptions, old ProgramMode=' + IntToStr(ord(MDdef.ProgramOption)) + '  new ProgramMode=' + IntToStr(RadioGroup7.ItemIndex));    {$EndIf}
      MDdef.ProgramOption := tProgramOption(RadioGroup7.ItemIndex);
      if (MDdef.ProgramOption = GeologyProgram) then begin
         SetStructuralGeologyDefaults;
         GetNaturalEarthData;
         GeologyGetData;
      end;
      if (MDdef.ProgramOption = GeographyProgram) then begin
         SetPhysicalGeographyDefaults;
         ClimateGetData;
         GetNaturalEarthData;
      end;
      //if (MDdef.ProgramOption = ShipwrecksProgram) then SetShipwrecksDefaults;
      //if (MDdef.ProgramOption = EconProgram) then SetEconDefaults;
      if (MDdef.ProgramOption = RemoteSensingProgram) then SetRemoteSensingDefaults;
      if (MDdef.ProgramOption = ExpertProgram) then begin
         SaveLabs := MDDef.ShowLabs;
         ProcessIniFile(iniInit,'Menus');
         SetExpertOptions(true);
         MDDef.ShowLabs := SaveLabs;
      end;
      {$IfDef ExDP}
      {$Else}
         if (MDdef.ProgramOption = DragonPlotProgram) then begin
             WMDEM.CloseAlldataandwindows1Click(Sender);
             StartDragonPlot;
             exit;
         end;
      {$EndIf}

      (*
      if (WmDEM.MDIChildCount > 0) then begin
         if AnswerIsYes('Close open data') then begin
            WMDEM.CloseAlldataandwindows1Click(Sender);
         end
         else begin
            UpdateMenusForAllMaps;
         end;
      end;
      *)
      UpdateMenusForAllMaps;
      WMDEM.FormActivate(Nil);
      {$IfDef OptionsProblems} WriteLineToDebugFile('Done version change');   {$EndIf}
      {$IfDef RecordNaturalEarthFileNames}
         WriteLineToDebugFile('End mode change ');
         WriteLineToDebugFile('   small: ' + SmallScaleWorldOutlines);
         WriteLineToDebugFile('  medium: ' + MedScaleWorldOutlines);
         WriteLineToDebugFile('   large: ' + LargeScaleWorldOutlines);
      {$EndIf}
   end;
   {$EndIf}

   if (Sender = BitBtn32) then begin
      fName := ProgramRootDir;
      if Petmar.GetFileNameDefaultExt('MICRODEM settings INI file','*.ini',fName) then begin
         {$IfDef OptionsProblems} WriteLineToDebugFile('TOptionsForm.OKBtnClick, write to ' + fName); {$EndIf}
         ProcessIniFile(iniWrite,'',fName);
      end;
   end
   else begin
      SaveMDdefaults;
   end;
   WMDEM.SetMenusForVersion;
   Close;
   {$IfDef OptionsProblems} WriteLineToDebugFile('TOptionsForm.OKBtnClick out'); {$EndIf}
end;


procedure TOptionsForm.OutlineButtonClick(Sender: TObject);
begin
   Petmar.PickLineSizeAndColor('DB region outlines',OutlineButton,MDDef.DBOutlineColor,MDDef.DBOutlineWidth);
end;


procedure TOptionsForm.RadioGroup33Click(Sender: TObject);
begin
   if (RadioGroup3.ItemIndex = 0) then MDDef.LeftLatGlobalDEM := 0 else MDDef.LeftLatGlobalDEM := -180;
end;

procedure TOptionsForm.RadioGroup7Click(Sender: TObject);
begin
  ProgramModeChanged := true;
end;

procedure TOptionsForm.Button2Click(Sender: TObject);
begin
   {$IfDef ExTiger}
   {$Else}
      SetTigerOptions(Nil,0);
   {$EndIf}
end;


procedure TOptionsForm.Button3Click(Sender: TObject);
begin
   with MDdef do GetNewBMPSize(DefaultTerrainXSize,DefaultTerrainYSize,'Default map size');
   Label23.Caption := IntToStr(MDDef.DefaultTerrainXSize) + 'x' + IntToStr(MDDef.DefaultTerrainYSize);
end;

procedure TOptionsForm.Button4Click(Sender: TObject);
begin
   PickDatum('primary datum', MDdef.PreferPrimaryDatum);
   Label5.Caption := DatumName(MDDef.PreferPrimaryDatum);
end;

procedure TOptionsForm.Button5Click(Sender: TObject);
begin
   GetNewBMPSize(MDdef.DefaultMapXSize,MDdef.DefaultMapYSize,'Default map size');
   Label8.Caption := IntToStr(MDDef.DefaultMapXSize) + 'x' + IntToStr(MDDef.DefaultMapYSize);
end;


procedure TOptionsForm.Button6Click(Sender: TObject);
begin
   GetSpeedDistOptions;
end;

procedure TOptionsForm.Button9Click(Sender: TObject);
begin
   {$IfDef ExVectorOverlay}
   {$Else}
      ContourOptions := TContourOptions.Create(Application);
      ContourOptions.ShowModal;
      ContourOptions.Free;
   {$EndIf}
end;

procedure TOptionsForm.UpDown1Click(Sender: TObject; Button: TUDBtnType);
begin
   Label7.Caption := IntegerToString(UpDown1.Position,3) + ':  Statistical sampling interval';
end;

procedure TOptionsForm.BitBtn1Click(Sender: TObject);
begin
   QueryColor(BitBtn1,MDdef.MissingDataColor);
end;


procedure TOptionsForm.Button18Click(Sender: TObject);
begin
   {$IfDef ExGeostats}
   {$Else}
      GetCurvAlg;
   {$EndIf}
end;


procedure TOptionsForm.Edit25Change(Sender: TObject);
begin
   CheckEditString(Edit25.Text,MDDef.VegDensityGraphMaxDensity);
   InitializeVegColors;
end;

procedure TOptionsForm.Button14Click(Sender: TObject);
begin
   SetBaseDirectory;
   LabelDirectories;
   SetDefaultDirectories;
end;


procedure TOptionsForm.Button12Click(Sender: TObject);
begin
   {$IfDef ExViewshed}
   {$Else}
      GetFanParameters;
   {$EndIf}
end;


procedure TOptionsForm.BitBtn6Click(Sender: TObject);
begin
   {$IfDef ExAmbush}
   {$Else}
      GetAmbushParameters;
   {$EndIf}
end;

procedure TOptionsForm.BitBtn8Click(Sender: TObject);
begin
   {$IfDef ExStereoNet}
   {$Else}
      NetOpts.MicronetOptions(nil);
   {$EndIf}
end;

procedure TOptionsForm.BitBtn9Click(Sender: TObject);
var
   i : integer;
   TStr : shortString;
begin
   for i := 0 to 3 do begin
      if WMDEM.StatusBar1.Panels[i].Text <> '' then Tstr := ' (' +  WMDEM.StatusBar1.Panels[i].Text + ')'
      else TStr := '';
      ReadDefault('Panel ' + IntToStr(i) + TStr + ' width',MDDef.SB1PanelWidths[i]);
      WMDEM.StatusBar1.Panels[i].Width := MDDef.SB1PanelWidths[i];
   end;
end;


procedure TOptionsForm.BitBtn7Click(Sender: TObject);
begin
   {$IfDef ExGeology}
   {$Else}
      sc_Colopts.SetStratcolOptions;
   {$EndIf}
end;

procedure TOptionsForm.Button1Click(Sender: TObject);
begin
   {$IfDef ShowDatumOptions} WriteLineToDebugFile('TOptionsForm.Button1Click in'); {$EndIf}
   PickUTMZone(MdDEF.DefaultUTMZone);
   Label1.Caption := IntToStr(MdDEF.DefaultUTMZone) + '  (' + UTMZoneExtent(MdDEF.DefaultUTMZone) + ')';
   {$IfDef ShowDatumOptions} WriteLineToDebugFile('TOptionsForm.Button1Click out'); {$EndIf}
end;


procedure TOptionsForm.Button15Click(Sender: TObject);
begin
   SetGazOptions;
end;


procedure TOptionsForm.Button21Click(Sender: TObject);
begin
   {$IfDef ExPLSS}
   {$Else}
      SetPLSSPlot(Nil);
   {$EndIf}
end;


procedure TOptionsForm.Button22Click(Sender: TObject);
begin
   PickDatum('Default digitizing datum',MDdef.DefaultDigitizeDatum);
   Label30.Caption := DatumName(MDdef.DefaultDigitizeDatum);
end;


procedure TOptionsForm.BitBtn14Click(Sender: TObject);
begin
   SetUSOutlines;
end;

procedure TOptionsForm.BitBtn15Click(Sender: TObject);
begin
   Petmar.PickSymbol(BitBtn15,MDDef.DefGISSymbol,'First point database');
end;

procedure TOptionsForm.BitBtn16Click(Sender: TObject);
begin
   PickSymbol(BitBtn16,MDDef.KeyLocationSymbol,'Key Locations');
end;

procedure TOptionsForm.BitBtn17Click(Sender: TObject);
begin
   Close;
end;

procedure TOptionsForm.BitBtn18Click(Sender: TObject);
{$IfDef ExKML}
begin
{$Else}
var
   KML_over_opts : KML_Overlay.TKML_over_opts;
begin
   KML_over_opts := TKML_over_opts.Create(Application);
   KML_over_opts.ShowModal;
   KML_over_opts.Free;
{$EndIf}
end;

procedure TOptionsForm.BitBtn19Click(Sender: TObject);
begin
   {$IfDef ExComplexGeoStats}
   {$Else}
      DrawTopoGrainOverlay(Nil,NIL);
   {$EndIf}
end;

procedure TOptionsForm.BitBtn2Click(Sender: TObject);
begin
   {$IfDef ExGeology}
   {$Else}
      Beach_ball_options.SetBeachBallOptions(true);
   {$EndIf}
end;


procedure TOptionsForm.BitBtn20Click(Sender: TObject);
begin
  {$IfDef ExGeostats}
  {$Else}
     ChangeSlopeMapOptions(Nil);
  {$EndIf}
end;

procedure TOptionsForm.BitBtn21Click(Sender: TObject);
begin
  {$IfDef ExGeology}
  {$Else}
     GeologyGetData(true);
  {$EndIf}
end;

procedure TOptionsForm.BitBtn22Click(Sender: TObject);
begin
   GetETOPO1(true);
   GetBlueMarble(true);
end;

procedure TOptionsForm.CheckBox88Click(Sender: TObject);
begin
   LabelDirectories;
end;


procedure TOptionsForm.BitBtn25Click(Sender: TObject);
begin
   Petmar.QueryColor(BitBtn25,MDDef.RoamSecondMapColor);
end;


procedure TOptionsForm.BitBtn26Click(Sender: TObject);
begin
   {$IfDef ExViewshed}
   {$Else}
      GetFanAlgorithmParameters(MDDef.wf);
   {$EndIf}
end;

procedure TOptionsForm.BitBtn27Click(Sender: TObject);
begin
   SetDigitizingOptions;
end;

procedure TOptionsForm.BitBtn28Click(Sender: TObject);
begin
   Petmar.GetFileFromDirectory('Geoid','*.tif',Geoid2008FName);
end;

procedure TOptionsForm.BitBtn29Click(Sender: TObject);
begin
   GetGridParameters;
end;

procedure TOptionsForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   Action := caFree;
   {$IfDef OptionsProblems} WriteLineToDebugFile('TOptionsForm.Button13Click'); {$EndIf}
end;


procedure TOptionsForm.BitBtn30Click(Sender: TObject);
begin
   {$IfDef ExLCP}
   {$Else}
      LeastCostPathOptions;
   {$EndIf}
end;

procedure TOptionsForm.BitBtn31Click(Sender: TObject);
begin
   SetNEOutlines(Nil);
end;


procedure TOptionsForm.BitBtn32Click(Sender: TObject);
begin
   OKBtnClick(Sender);
end;


procedure TOptionsForm.BitBtn33Click(Sender: TObject);
begin
   {$IfDef ExGeography}
   {$Else}
      ClimateGetData(true);
   {$EndIf}
end;

procedure TOptionsForm.BitBtn34Click(Sender: TObject);
begin
   Petmar.PickLineSizeAndColor('Country outlines',BitBtn34,MDDef.CountryOUtline_Color,MDDef.CountryOUtline_Width);
end;

procedure TOptionsForm.BitBtn35Click(Sender: TObject);
begin
   Petmar.PickLineSizeAndColor('Province outlines',BitBtn35,MDDef.ProvinceOutline_Color,MDDef.ProvinceOutline_Width);
end;

procedure TOptionsForm.BitBtn36Click(Sender: TObject);
begin
   QueryColor(BitBtn36,MDdef.BlankMapColor);
end;

procedure TOptionsForm.BitBtn37Click(Sender: TObject);
begin
   //QuickOpenEditWindow(IniFileName,ShortEXEName + '.ini file');
   ShowInNotepadPlusPlus(IniFileName,ShortEXEName + '.ini file');
end;

procedure TOptionsForm.BitBtn38Click(Sender: TObject);
begin
   {$IfDef ExDRGimport}
   {$Else}
      SetDRGDefaults;
   {$EndIf}
end;

procedure TOptionsForm.BitBtn39Click(Sender: TObject);
begin
   GetGeoid;
end;

procedure TOptionsForm.BitBtn40Click(Sender: TObject);
begin
   PickMapIndexLocation;
   Label20.Caption := MapLibDir;
end;

procedure TOptionsForm.BitBtn41Click(Sender: TObject);
begin
   AdjustIntegratedDataBaseSeries;
end;

procedure TOptionsForm.BitBtn42Click(Sender: TObject);
begin
    GetNaturalEarthData(True);
end;

procedure TOptionsForm.BitBtn4Click(Sender: TObject);
var
   fName : PathStr;
begin
   {$IfDef OptionsProblems} WriteLineToDebugFile('TOptionsForm.BitBtn4Click in'); {$EndIf}
   fName := ProgramRootDir;
   if Petmar.GetFileFromDirectory('INI file','*.ini',fName) then begin
      {$IfDef OptionsProblems} WriteLineToDebugFile('Picked ini=' + fName); {$EndIf}
      ProcessIniFile(iniRead,'',fName);
      FormCreate(nil);    //to renter all the restored settings from the INI file on this form
   end;
   {$IfDef OptionsProblems} WriteLineToDebugFile('TOptionsForm.BitBtn4Click out'); {$EndIf}
end;

procedure TOptionsForm.BitBtn5Click(Sender: TObject);
begin
  RecreateINIfile;
  FormCreate(nil); //to renter all the restored settings from the INI file on this form
end;

procedure TOptionsForm.BitBtn11Click(Sender: TObject);
begin
  ProcessIniFile(iniInit);
end;

procedure TOptionsForm.BitBtn12Click(Sender: TObject);
begin
  {$IFDef ExPointCloud}
  {$Else}
  inherited;
   OpenDBForModalEdit(LasRulesName);
   LAS_Lidar.InitializeLASColors;
   {$EndIf}
end;


procedure TOptionsForm.BitBtn13Click(Sender: TObject);
begin
   if GetNewBMPSize(MDDef.XAspect,MDDef.YAspect,'Aspect ratio') then begin
      Label11.Caption := IntToStr(MDDef.xAspect) + ':' + IntToStr(MDDef.YAspect);
   end;
end;


initialization
   {$IfDef MessageStartUpUnitProblems}   MessageToContinue('Startup demoptions'); {$EndIf}
finalization
   {$IfDef RecordClosingProblems} WriteLineToDebugFile('Closing demoptions in');   {$EndIf}
   {$IfDef ShowDirectories} WriteLineToDebugFile('ShowDirectories active in demoptions');   {$EndIf}
   {$IfDef OptionsProblems} WriteLineToDebugFile('OptionsProblems active in DEMOptions');   {$EndIf}
   {$IfDef ShowLocalOptima} WriteLineToDebugFile('ShowLocalOptima active in DEMOptions');   {$EndIf}
   {$IfDef ShowDatumOptions} WriteLineToDebugFile('ShowDatumOptions active in DEMOptions');   {$EndIf}
   {$IfDef RecordClosingProblems} WriteLineToDebugFile('Closing demoptions out');   {$EndIf}
end.


