unit dem_fan_algorithm;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2023 Peter L. Guth  }
{___________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   //{$Define RecordFanProblems}
{$EndIf}

interface

uses
   Windows, Classes, Graphics, Forms, Controls, Buttons,
   StdCtrls, ExtCtrls,ColorGrd, Dialogs,SysUtils,
   PETMAR,DEMDefs, DEMCoord,PETMath;

type
  TFanAlgParams = class(TForm)
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    HelpBtn: TBitBtn;
    Label6: TLabel;
    Edit6: TEdit;
    RadioGroup2: TRadioGroup;
    Label5: TLabel;
    Button2: TButton;
    RadioGroup3: TRadioGroup;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    Panel1: TPanel;
    RadioGroup4: TRadioGroup;
    RadioGroup5: TRadioGroup;
    Panel2: TPanel;
    Label2: TLabel;
    Label1: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Label7: TLabel;
    Edit7: TEdit;
    Label3: TLabel;
    Edit3: TEdit;
    BitBtn1: TBitBtn;
    Label4: TLabel;
    Edit4: TEdit;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    Label8: TLabel;
    Edit5: TEdit;
    procedure HelpBtnClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RadioGroup3Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
  private
    { Private declarations }
     procedure Hide;
     procedure LabelForm;
  public
    { Public declarations }
     iva : tIntervisibilityAlgorithm;
  end;


function GetFanAlgorithmParameters(var in_iva : tIntervisibilityAlgorithm) : boolean;


implementation

{$R *.DFM}

uses
   DEMcurvature,Petmar_types,Demdef_routines;


procedure TFanAlgParams.LabelForm;
begin
   with iva do begin
      Label5.Caption := CurvAlgName(FanCurvAlg);
      Edit1.Text := RealToString(FanDEMSpaceMultiple,-18,-4);
      Edit2.Text := RealToString(FanMapSpaceMultiple,-18,-4);
      Edit6.Text := RealToString(MaskRaySpacingDeg,-18,-4);
      Edit7.Text := RealToString(MaskAreaInterval,-18,-4);
      Edit3.Text := RealToString(SmartSwitchOver,-12,-2);
      Edit4.Text := IntToStr(ClosestBlockingDistance);
      RadioGroup2.ItemIndex := ord(FanMethod);
      RadioGroup3.ItemIndex := ord(LOSAlgorithm);
      RadioGroup4.ItemIndex := ord(ElevInterpolation);
      RadioGroup5.ItemIndex := ord(StraightAlgorithm);
   end;
end;


function GetFanAlgorithmParameters(var in_iva : tIntervisibilityAlgorithm) : boolean;
var
  aElevInterpolation : tElevInterpolation;
  aStraightAlgorithm : tStraightAlgorithm;
  FanAlgParams : TFanAlgParams;
begin
   {$IfDef RecordFanProblems} WriteLineToDebugFile('GetFanAlgorithmParameters in,  method=' + FanMethodName[in_iva.FanMethod] + ' algorithm=' + LOSAlgorithmDescription(MDDef.wf));  {$EndIf}

   FanAlgParams := TFanAlgParams.Create(Application);
   with FanAlgParams,iva do begin
      Top := Mouse.CursorPos.Y;
      Left := Mouse.CursorPos.X;
      CheckBox3.Checked := MDDef.MissingDataBlocksLOS;
      Edit5.Text := IntToStr(MDDef.FanMapZoom);
      iva := in_iva;
      for aElevInterpolation := piBilinear to piSWGrid do RadioGroup4.Items.Add(ElevInterpolationName[aElevInterpolation]);
      for aStraightAlgorithm := saDEMGrid to saSmart do RadioGroup5.Items.Add(StraightAlgorithmName[aStraightAlgorithm]);
      LabelForm;
      Hide;

      Result := (ShowModal = mrCancel);
      if Result then begin
         CheckEditString(Edit1.Text,FanDEMSpaceMultiple);
         CheckEditString(Edit2.Text,FanMapSpaceMultiple);
         CheckEditString(Edit6.Text,MaskRaySpacingDeg);
         CheckEditString(Edit7.Text,MaskAreaInterval);
         CheckEditString(Edit3.Text,SmartSwitchOver);
         CheckEditString(Edit4.Text,ClosestBlockingDistance);
         CheckEditString(Edit5.Text,MDDef.FanMapZoom);
         FanMethod := tFanMethod(RadioGroup2.ItemIndex);
         LOSAlgorithm := tLOSAlgorithm(RadioGroup3.ItemIndex);
         FanCurvAlg := MDdef.CurvAlg;
         ElevInterpolation := tElevInterpolation(RadioGroup4.ItemIndex);
         StraightAlgorithm := tStraightAlgorithm(RadioGroup5.ItemIndex);
         in_iva := iva;
         MDDef.MissingDataBlocksLOS := CheckBox3.Checked;
      end;
      FanAlgParams.Free;
   end;
   {$IfDef RecordFanProblems}  WriteLineToDebugFile('GetFanAlgorithmParameters out,  algorithm=' + LOSAlgorithmDescription(MDDef.wf));  {$EndIf}
end;


procedure TFanAlgParams.Hide;
begin
  Edit6.Enabled := RadioGroup2.ItemIndex = 0;
  Label6.Enabled := RadioGroup2.ItemIndex = 0;

  Edit7.Enabled := RadioGroup3.ItemIndex = 1;
  Label7.Enabled := RadioGroup3.ItemIndex = 1;

  Edit1.Enabled := RadioGroup3.ItemIndex = 0;
  Edit2.Enabled := RadioGroup3.ItemIndex = 0;
  Label1.Enabled := RadioGroup3.ItemIndex = 0;
  Label2.Enabled := RadioGroup3.ItemIndex = 0;

  RadioGroup4.Enabled := RadioGroup3.ItemIndex in [0,1];
  if RadioGroup3.ItemIndex in [2,3] then RadioGroup4.ItemIndex := 0;
end;


procedure TFanAlgParams.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\los_fan_alg.htm');
end;


procedure TFanAlgParams.Button2Click(Sender: TObject);
begin
   GetCurvAlg;
   Label5.Caption := CurvAlgName(MDdef.CurvAlg);
end;


procedure TFanAlgParams.RadioGroup2Click(Sender: TObject);
begin
   Hide;
end;

procedure TFanAlgParams.RadioGroup3Click(Sender: TObject);
begin
   Hide;
end;


procedure TFanAlgParams.FormCreate(Sender: TObject);
begin
   Petmar.PlaceFormAtMousePosition(Self);
end;


procedure TFanAlgParams.BitBtn1Click(Sender: TObject);
begin
   ResetDefaultFanAlgorithm;
   LabelForm;
end;


initialization
finalization
   {$IfDef RecordFanProblems} WriteLineToDebugFile('RecordFanProblems active in dem_fan_algorithm'); {$EndIf}
   {$IfDef RecordClosingProblems} WriteLineToDebugFile('Closing dem_fan_algorithm');  {$EndIf}
end.
