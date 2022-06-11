unit block_opts;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch }
{   file verified 6/22/2011       }
{_________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   //{$Define RecordBlockOptions}
{$EndIf}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons,
  DEMDefs, ExtCtrls, Vcl.ComCtrls;

type
  TBlockOpsForm = class(TForm)
    HelpBtn: TBitBtn;
    OKBtn: TBitBtn;
    CheckBox9: TCheckBox;
    Label11: TLabel;
    Edit7: TEdit;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    GroupBox1: TGroupBox;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox10: TCheckBox;
    CheckBox11: TCheckBox;
    CheckBox12: TCheckBox;
    CheckBox13: TCheckBox;
    tabsheet2: TTabSheet;
    GroupBox4: TGroupBox;
    CheckBox21: TCheckBox;
    CheckBox22: TCheckBox;
    CheckBox23: TCheckBox;
    CheckBox24: TCheckBox;
    CheckBox51: TCheckBox;
    GroupBox7: TGroupBox;
    CheckBox33: TCheckBox;
    CheckBox34: TCheckBox;
    CheckBox35: TCheckBox;
    CheckBox36: TCheckBox;
    CheckBox37: TCheckBox;
    CheckBox38: TCheckBox;
    CheckBox39: TCheckBox;
    CheckBox40: TCheckBox;
    CheckBox42: TCheckBox;
    CheckBox43: TCheckBox;
    GroupBox6: TGroupBox;
    CheckBox29: TCheckBox;
    CheckBox30: TCheckBox;
    CheckBox31: TCheckBox;
    CheckBox32: TCheckBox;
    CheckBox44: TCheckBox;
    CheckBox45: TCheckBox;
    GroupBox8: TGroupBox;
    CheckBox47: TCheckBox;
    CheckBox48: TCheckBox;
    CheckBox49: TCheckBox;
    CheckBox50: TCheckBox;
    GroupBox3: TGroupBox;
    CheckBox17: TCheckBox;
    CheckBox18: TCheckBox;
    CheckBox19: TCheckBox;
    CheckBox20: TCheckBox;
    CheckBox16: TCheckBox;
    GroupBox2: TGroupBox;
    CheckBox14: TCheckBox;
    CheckBox15: TCheckBox;
    CheckBox46: TCheckBox;
    CheckBox52: TCheckBox;
    Label15: TLabel;
    GroupBox5: TGroupBox;
    CheckBox25: TCheckBox;
    CheckBox26: TCheckBox;
    CheckBox27: TCheckBox;
    CheckBox28: TCheckBox;
    CheckBox41: TCheckBox;
    GroupBox9: TGroupBox;
    Edit12: TEdit;
    Edit5: TEdit;
    Label6: TLabel;
    Label16: TLabel;
    GroupBox12: TGroupBox;
    Edit6: TEdit;
    Label3: TLabel;
    GroupBox13: TGroupBox;
    Label7: TLabel;
    BitBtn2: TBitBtn;
    Label2: TLabel;
    BitBtn1: TBitBtn;
    CheckBox53: TCheckBox;
    Edit3: TEdit;
    Label10: TLabel;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    Edit8: TEdit;
    Label8: TLabel;
    Edit2: TEdit;
    Label9: TLabel;
    Label1: TLabel;
    Edit1: TEdit;
    BitBtn5: TBitBtn;
    CheckBox54: TCheckBox;
    Edit9: TEdit;
    Label12: TLabel;
    Label13: TLabel;
    Edit10: TEdit;
    BitBtn6: TBitBtn;
    CheckBox8: TCheckBox;
    CheckBox55: TCheckBox;
    CheckBox56: TCheckBox;
    CheckBox57: TCheckBox;
    Edit4: TEdit;
    Label5: TLabel;
    GroupBox10: TGroupBox;
    Label18: TLabel;
    Edit13: TEdit;
    Label4: TLabel;
    Edit11: TEdit;
    BitBtn7: TBitBtn;
    CheckBox58: TCheckBox;
    procedure OKBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Edit4Change(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure ThreadsClick(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure CheckBox9Click(Sender: TObject);
    procedure CheckBox46Click(Sender: TObject);
    procedure CheckBox52Click(Sender: TObject);
    procedure Edit12Change(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Edit6Change(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure BitBtn6Click(Sender: TObject);
    procedure BitBtn7Click(Sender: TObject);
  private
    procedure SetAll(setting: boolean);
    procedure CheckSettings;
    { Private declarations }
  public
    { Public declarations }
    DEM : integer;
    GridLimits : tGridLimits;
    procedure ShowNumbers;
    procedure SetForMode(WhatFor : tGeomporphBlock);
  end;


implementation

{$R *.dfm}

uses
   DEMCoord,Petmar,Petmar_types,Nevadia_Main, demoptions,DEMstat,   Make_grid,
   DEM_indexes,PetDBUtils;


procedure TBlockOpsForm.SetForMode(WhatFor : tGeomporphBlock);
var
   aTop : integer;

   procedure PositionGroupBox(Box : tGroupBox);
   begin
      if Box.Visible then begin
         Box.Top := atop;
         atop := atop + Box.Height + 5
      end;
   end;


begin
   GroupBox12.Visible := WhatFor in [gbPolygon];
   GroupBox13.Visible := false;
   aTop := 50;

   //PositionGroupBox(GroupBox10);
   //PositionGroupBox(GroupBox9);
   //PositionGroupBox(GroupBox12);
   //PositionGroupBox(GroupBox13);

   //OKBtn.Top := atop;
   //HelpBtn.Top := atop;
   //atop := atop + HelpBtn.Height + 25;

   //if atop < 350 then ClientHeight := 350
   //else ClientHeight := atop;
end;


procedure TBlockOpsForm.BitBtn1Click(Sender: TObject);
begin
   GetDOSPath('Input directory',BlockInputDir);
   Label2.Caption := BlockInputDir;
end;

procedure TBlockOpsForm.BitBtn2Click(Sender: TObject);
begin
   GetDOSPath('Output directory',BlockOutputDir);
   Label7.Caption := BlockOutputDir;
end;

procedure TBlockOpsForm.BitBtn3Click(Sender: TObject);
begin
   CheckSettings;
   GridsByRegionSize(DEM,'F');
end;


procedure TBlockOpsForm.BitBtn4Click(Sender: TObject);
begin
    CheckSettings;
    if MDDef.DoUpOpen or MDDef.DoDownOpen then MakeMomentsGrid(DEM, 'O',MDDef.OpenBoxSizeMeters);
    if MDDef.DoRelief1 or MDDef.DoAvgElev or MDDef.DoElevStd or MDDef.DoREL or MDDef.DoTPI then MakeMomentsGrid(DEM, 'R',MDDef.ReliefBoxSizeMeters);
    if MDDef.DoRelief2 or MDDef.DoSummit or MDDef.DoBaseLevel or MDDef.DoGeophysical or MDDef.DoDropoff or MDDef.DoElevRelief then MakeMomentsGrid(DEM, 'G',MDDef.ReliefBoxSizeMeters);
    if MDDef.DoS1S2 or MDDef.DoS2S3 or MDDef.DoFabDir90 or MDDef.DoFabDir180 or MDDef.DoFabDir360 or MDDef.DoRoughness then MakeMomentsGrid(DEM, 'F',MDDef.SSOBoxSizeMeters);

    if MDDef.ElevMoments then MakeMomentsGrid(DEM, 'e',MDDef.MomentsBoxSizeMeters);
    if MDDef.SlopeMoments then MakeMomentsGrid(DEM, 's',MDDef.MomentsBoxSizeMeters);
    if MDDef.PlanCurvMoments then MakeMomentsGrid(DEM, 'l',MDDef.MomentsBoxSizeMeters);
    if MDDef.SlopeCurvMoments then MakeMomentsGrid(DEM, 'r',MDDef.MomentsBoxSizeMeters);

    if MDDef.DoCrossCurve or MDDef.DoMaxCurve or MDDef.DoMinCurve or MDDef.DoSlopeCurve or MDDef.DoPlanCurve then MakeMomentsGrid(DEM, 'C');
    if MDDef.DoSlopePC or MDDef.DoSlopeDeg or MDDef.DoSlopeSin or MDDef.DoSlopeLogTan or MDDef.DoSlopeLnTan or MDDef.DoSlopeSqrtSin or
        MDDef.DoAspect or MDDef.DoAspectNS or MDDef.DoAspectEW or MDDef.DoNSSlope or MDDef.DoEWSlope then MakeMomentsGrid(DEM, 'S');
end;

procedure TBlockOpsForm.BitBtn5Click(Sender: TObject);
begin
   CheckSettings;
   GridsByRegionSize(DEM,'O');
end;

procedure TBlockOpsForm.BitBtn6Click(Sender: TObject);
begin
   CheckSettings;
   if MDDef.ElevMoments then GridsByRegionSize(DEM,'e');  //MakeMomentsGrid(DEM, 'e');
   if MDDef.SlopeMoments then GridsByRegionSize(DEM,'s');   //MakeMomentsGrid(DEM, 's');
   if MDDef.PlanCurvMoments then GridsByRegionSize(DEM,'L');   //MakeMomentsGrid(DEM, 'l');
   if MDDef.SlopeCurvMoments then GridsByRegionSize(DEM,'r');   //MakeMomentsGrid(DEM, 'r');
end;

procedure TBlockOpsForm.BitBtn7Click(Sender: TObject);
begin
   CheckSettings;
   if MDDef.DoRelief1 or MDDef.DoAvgElev or MDDef.DoElevStd or MDDef.DoREL or MDDef.DoTPI then GridsByRegionSize(DEM, 'R');
   if MDDef.DoRelief2 or MDDef.DoSummit or MDDef.DoBaseLevel or MDDef.DoGeophysical or MDDef.DoDropoff or MDDef.DoElevRelief then GridsByRegionSize(DEM,'G');
end;

procedure TBlockOpsForm.CheckBox46Click(Sender: TObject);
begin
   SetAll(CheckBox46.Checked);
end;

procedure TBlockOpsForm.CheckBox52Click(Sender: TObject);
begin
   MDDef.AutoSaveGeomorphGrids := CheckBox52.Checked;
end;


procedure TBlockOpsForm.CheckBox9Click(Sender: TObject);
begin
   MDDef.EntireDEMGeostats := CheckBox9.Checked;
   Edit1.Enabled := not MDDef.EntireDEMGeostats;
   Edit3.Enabled := not MDDef.EntireDEMGeostats;
   Edit4.Enabled := not MDDef.EntireDEMGeostats;
   Edit5.Enabled := not MDDef.EntireDEMGeostats;
end;

procedure TBlockOpsForm.Edit12Change(Sender: TObject);
begin
   CheckEditString(Edit12.Text,MDDef.MinPointsForSSO);
end;

procedure TBlockOpsForm.Edit1Change(Sender: TObject);
begin
   ShowNumbers;
end;

procedure TBlockOpsForm.Edit4Change(Sender: TObject);
begin
   ShowNumbers;
end;


procedure TBlockOpsForm.Edit6Change(Sender: TObject);
begin
   CheckEditString(Edit6.Text,MDDef.MaskDistance);
end;


procedure TBlockOpsForm.SetAll(setting : boolean);
begin
    //curvature
      CheckBox20.Checked := Setting;
      CheckBox19.Checked := Setting;
      CheckBox18.Checked := Setting;
      CheckBox16.Checked := Setting;
      CheckBox17.Checked := Setting;
    //openness
      CheckBox14.Checked := Setting;
      CheckBox15.Checked := Setting;
      CheckBox54.Checked := Setting;
    //relief1
      CheckBox21.Checked := Setting;
      CheckBox22.Checked := Setting;
      CheckBox23.Checked := Setting;
      CheckBox24.Checked := Setting;
      CheckBox51.Checked := Setting;  // MDDef.DoTPI;
   //relief2
      CheckBox25.Checked := Setting;
      CheckBox26.Checked := Setting;
      CheckBox27.Checked := Setting;
      CheckBox28.Checked := Setting;
      CheckBox41.Checked := Setting;
      CheckBox58.Checked := Setting;
    //Fabric
      CheckBox29.Checked := Setting;
      CheckBox30.Checked := Setting;
      CheckBox31.Checked := Setting;
      CheckBox32.Checked := Setting;
      CheckBox44.Checked := Setting;  //MDDef.DoFabDir180;
      CheckBox45.Checked := Setting;  //MDDef.DoFabDir360;
    //slope/aspect
      CheckBox33.Checked := Setting;  //MDDef.DoSlopePC;
      CheckBox34.Checked := Setting;  //MDDef.DoSlopeDeg;
      CheckBox35.Checked := Setting;  //MDDef.DoSlopeSin;
      CheckBox36.Checked := Setting;  //MDDef.DoSlopeLogTan;
      CheckBox43.Checked := Setting;  //MDDef.DoSlopeLnTan;
      CheckBox37.Checked := Setting;  //MDDef.DoSlopeSqrtSin;
      CheckBox38.Checked := Setting;  //MDDef.DoAspect;
      CheckBox39.Checked := Setting;  //MDDef.DoAspectNS;
      CheckBox40.Checked := Setting;  //MDDef.DoAspectEW;
      CheckBox42.Checked := Setting;  //MDDef.DoNSSlope;
      CheckBox53.Checked := Setting;  //MDDef.DoEWSlope;

    //Moments
      CheckBox47.Checked := Setting;  // MDDef.DoElevMoments;
      CheckBox48.Checked := Setting;  // MDDef.DoSlopeMoments;
      CheckBox49.Checked := Setting;  // MDDef.DoPlanCurvMoments;
      CheckBox50.Checked := Setting;  // MDDef.DoProfCurvMoments;
end;


procedure TBlockOpsForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   Action := caFree;
end;

procedure TBlockOpsForm.FormCreate(Sender: TObject);
begin
   Label2.Caption := BlockInputDir;
   Label7.Caption := BlockOutputDir;
   CheckBox7.Checked := MDDef.IncludeWaveLength;
   CheckBox6.Checked := MDDef.IncludeFractalMeasures;
   CheckBox5.Checked := MDDef.IncludeGammaMeasures;
   CheckBox3.Checked := MDDef.IncludeProfCMeasures;
   CheckBox4.Checked := MDDef.IncludePlanCMeasures;
   CheckBox2.Checked := MDDef.IncludeFabricMeasures;
   CheckBox1.Checked := MDDef.IncludeSlopeMeasures;
   CheckBox10.Checked := MDDef.IncludeBasicElevation;
   CheckBox11.Checked := MDDef.IncludeAdvancedElevation;
   CheckBox12.Checked := MDDef.IncludeMissingHoles;
   CheckBox13.Checked := MDDef.IncludeOpenness;
   CheckBox9.Checked := MDDef.EntireDEMGeostats;
   CheckBox9Click(nil);

   CheckBox20.Checked := MDDef.DoCrossCurve;
   CheckBox19.Checked := MDDef.DoMaxCurve;
   CheckBox18.Checked := MDDef.DoMinCurve;
   CheckBox16.Checked := MDDef.DoSlopeCurve;
   CheckBox17.Checked := MDDef.DoPlanCurve;

   CheckBox14.Checked := MDDef.DoUpOpen;
   CheckBox15.Checked := MDDef.DoDownOpen;
   CheckBox54.Checked := MDDef.DoDiffOpen;

   CheckBox21.Checked := MDDef.DoRelief1;
   CheckBox22.Checked := MDDef.DoAvgElev;
   CheckBox23.Checked := MDDef.DoElevStd;
   CheckBox24.Checked := MDDef.DoREL;
   CheckBox51.Checked := MDDef.DoTPI;

   CheckBox25.Checked := MDDef.DoRelief2;
   CheckBox26.Checked := MDDef.DoSummit;
   CheckBox27.Checked := MDDef.DoBaseLevel;
   CheckBox28.Checked := MDDef.DoGeophysical;
   CheckBox41.Checked := MDDef.DoDropoff;
   CheckBox58.Checked := MDDef.DoElevRelief;

   CheckBox29.Checked := MDDef.DoS1S2;
   CheckBox30.Checked := MDDef.DoS2S3;
   CheckBox31.Checked := MDDef.DoFabDir90;
   CheckBox32.Checked := MDDef.DoRoughness;
   CheckBox44.Checked := MDDef.DoFabDir180;
   CheckBox45.Checked := MDDef.DoFabDir360;

   CheckBox33.Checked := MDDef.DoSlopePC;
   CheckBox34.Checked := MDDef.DoSlopeDeg;
   CheckBox35.Checked := MDDef.DoSlopeSin;
   CheckBox36.Checked := MDDef.DoSlopeLogTan;
   CheckBox43.Checked := MDDef.DoSlopeLnTan;
   CheckBox37.Checked := MDDef.DoSlopeSqrtSin;
   CheckBox38.Checked := MDDef.DoAspect;
   CheckBox39.Checked := MDDef.DoAspectNS;
   CheckBox40.Checked := MDDef.DoAspectEW;
   CheckBox42.Checked := MDDef.DoNSSlope;
   CheckBox53.Checked := MDDef.DoEWSlope;

   CheckBox47.Checked :=  MDDef.ElevMoments;
   CheckBox48.Checked :=  MDDef.SlopeMoments;
   CheckBox49.Checked :=  MDDef.PlanCurvMoments;
   CheckBox50.Checked :=  MDDef.SlopeCurvMoments;

   CheckBox8.Checked :=  MDDef.DoMean;
   CheckBox55.Checked :=  MDDef.DoSTD;
   CheckBox56.Checked :=  MDDef.DoSkew;
   CheckBox57.Checked :=  MDDef.DoKurt;

   CheckBox52.Checked := MDDef.AutoSaveGeomorphGrids;

   Edit1.Text := IntToStr(MDDef.OpennessCalcThin);
   Edit8.Text := MDDef.FabricCalcThin.ToString;
   Edit10.Text := MDDef.MomentCalcThin.ToString;
   Edit11.Text := MDDef.ReliefCalcThin.ToString;

   Edit2.Text := IntToStr(MDDef.OpenBoxSizeMeters);
   Edit4.Text := IntToStr(MDDef.GeomorphBoxSizeMeters);
   Edit3.Text := IntToStr(MDDef.SSOBoxSizeMeters);
   Edit9.Text := MDDef.MomentsBoxSizeMeters.ToString;
   Edit13.Text := MDDef.ReliefBoxSizeMeters.ToString;

   Edit5.Text := IntToStr(MDDef.GeomorphElevsNeeded);
   Edit6.Text := IntToStr(MDDef.MaskDistance);
   Edit7.Text := MDDef.StatSampleIncr.ToString;
   Edit12.Text := MDDef.MinPointsForSSO.ToString;
   wmdem.FormPlacementInCorner(Self);
end;

procedure TBlockOpsForm.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\geomorph_block_settings.htm');
end;

procedure TBlockOpsForm.CheckSettings;
begin
   MDDef.IncludeWaveLength := CheckBox7.Checked;
   MDDef.IncludeFractalMeasures := CheckBox6.Checked;
   MDDef.IncludeGammaMeasures := CheckBox5.Checked;
   MDDef.IncludeProfCMeasures := CheckBox3.Checked;
   MDDef.IncludePlanCMeasures := CheckBox4.Checked;
   MDDef.IncludeFabricMeasures := CheckBox2.Checked;
   MDDef.IncludeSlopeMeasures := CheckBox1.Checked;
   MDDef.IncludeBasicElevation := CheckBox10.Checked;
   MDDef.IncludeAdvancedElevation := CheckBox11.Checked;
   MDDef.IncludeMissingHoles := CheckBox12.Checked;
   MDDef.IncludeOpenness := CheckBox13.Checked;

   MDDef.DoCrossCurve := CheckBox20.Checked;
   MDDef.DoMaxCurve := CheckBox19.Checked;
   MDDef.DoMinCurve := CheckBox18.Checked;
   MDDef.DoSlopeCurve := CheckBox16.Checked;
   MDDef.DoPlanCurve := CheckBox17.Checked;

   MDDef.DoUpOpen := CheckBox14.Checked;
   MDDef.DoDownOpen := CheckBox15.Checked;
   MDDef.DoDiffOpen := CheckBox54.Checked;

   MDDef.DoRelief1 := CheckBox21.Checked;
   MDDef.DoAvgElev := CheckBox22.Checked;
   MDDef.DoElevStd := CheckBox23.Checked;
   MDDef.DoREL := CheckBox24.Checked;
   MDDef.DoTPI := CheckBox51.Checked;

   MDDef.DoRelief2 := CheckBox25.Checked;
   MDDef.DoSummit := CheckBox26.Checked;
   MDDef.DoBaseLevel := CheckBox27.Checked;
   MDDef.DoGeophysical := CheckBox28.Checked;
   MDDef.DoDropoff := CheckBox41.Checked;
   MDDef.DoElevRelief := CheckBox58.Checked;

   MDDef.DoS1S2 := CheckBox29.Checked;
   MDDef.DoS2S3 := CheckBox30.Checked;
   MDDef.DoFabDir90 := CheckBox31.Checked;
   MDDef.DoFabDir180 := CheckBox44.Checked;
   MDDef.DoFabDir360 := CheckBox45.Checked;
   MDDef.DoRoughness := CheckBox32.Checked;

   MDDef.DoSlopePC := CheckBox33.Checked;
   MDDef.DoSlopeDeg := CheckBox34.Checked;
   MDDef.DoSlopeSin := CheckBox35.Checked;
   MDDef.DoSlopeLogTan := CheckBox36.Checked;
   MDDef.DoSlopeSqrtSin := CheckBox37.Checked;
   MDDef.DoAspect := CheckBox38.Checked;
   MDDef.DoAspectNS := CheckBox39.Checked;
   MDDef.DoAspectEW := CheckBox40.Checked;
   MDDef.DoSlopeLnTan := CheckBox43.Checked;
   MDDef.DoNSSlope := CheckBox42.Checked;
   MDDef.DoEWSlope := CheckBox53.Checked;

   MDDef.ElevMoments := CheckBox47.Checked;
   MDDef.SlopeMoments := CheckBox48.Checked;
   MDDef.PlanCurvMoments := CheckBox49.Checked;
   MDDef.SlopeCurvMoments := CheckBox50.Checked;

   MDDef.DoMean := CheckBox8.Checked;
   MDDef.DoSTD := CheckBox55.Checked;
   MDDef.DoSkew := CheckBox56.Checked;
   MDDef.DoKurt := CheckBox57.Checked;

   CheckEditString(Edit1.Text,MDDef.OpennessCalcThin);
   CheckEditString(Edit8.Text,MDDef.FabricCalcThin);
   CheckEditString(Edit9.Text,MDDef.MomentsBoxSizeMeters);
   CheckEditString(Edit10.Text,MDDef.MomentCalcThin);
   CheckEditString(Edit11.Text,MDDef.ReliefCalcThin);
   CheckEditString(Edit13.Text,MDDef.ReliefBoxSizeMeters);
   CheckEditString(Edit3.Text,MDDef.SSOBoxSizeMeters);
   CheckEditString(Edit2.Text,MDDef.OpenBoxSizeMeters);
   CheckEditString(Edit4.Text,MDDef.GeomorphBoxSizeMeters);
   CheckEditString(Edit5.Text,MDDef.GeomorphElevsNeeded);
   CheckEditString(Edit7.Text,MDDef.StatSampleIncr);
end;


procedure TBlockOpsForm.OKBtnClick(Sender: TObject);
begin
   Close;
end;

procedure TBlockOpsForm.ShowNumbers;
begin
end;

procedure TBlockOpsForm.ThreadsClick(Sender: TObject);
begin
   DEMOptions.ChangeOptions(10);
end;


initialization
finalization
   {$IfDef RecordBlockOptions}  WriteLineToDebugFile('RecordBlockOptions active in block_opts');  {$EndIf}
   {$IfDef RecordClosingProblems} WriteLineToDebugFile('Closing block_opts out');  {$EndIf}
end.
