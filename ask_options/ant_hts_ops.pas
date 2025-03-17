unit ant_hts_ops;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{________________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
{$EndIf}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons;

type
  TReqAntOptsForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    Label3: TLabel;
    Edit3: TEdit;
    BitBtn4: TBitBtn;
    BitBtn5: TBitBtn;
    Label4: TLabel;
    BitBtn1: TBitBtn;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


procedure GetRequiredAntennaOptions;


implementation

{$R *.dfm}

uses
   Petmar,Petmar_types,DEMDefs,DEMDef_routines,demcurvature;


procedure GetRequiredAntennaOptions;
var
  ReqAntOptsForm : TReqAntOptsForm;
begin
   ReqAntOptsForm := TReqAntOptsForm.Create(Application);
   ReqAntOptsForm.BitBtn1Click(nil);
   ReqAntOptsForm.ShowModal;
end;


procedure TReqAntOptsForm.BitBtn1Click(Sender: TObject);
begin
   if (Sender <> Nil) then DemCurvature.GetCurvAlg;
   Label4.Caption := EarthCurvAlgName(MDDef.EarthVertCurvAlg);
end;


procedure TReqAntOptsForm.BitBtn4Click(Sender: TObject);
begin
   DisplayHTMLTopic('html\calculate_antenna_height.htm');
end;


procedure TReqAntOptsForm.BitBtn5Click(Sender: TObject);
begin
   CheckEditString(Edit1.Text,MDDef.ObsAboveGround);
   CheckEditString(Edit2.Text,MDdef.MaskObsRange);
   CheckEditString(Edit3.Text,MDDef.MinTerrainFlyAbove);
   MDDef.DoReqAntHigh := CheckBox1.Checked;
   MDDef.DoReqFlyHigh := CheckBox2.Checked;
   MDDef.DoLOSProfile := CheckBox3.Checked;
   MDDef.DoGrazingAngle := CheckBox4.Checked;
   MDDef.DoEarthCurvature := CheckBox5.Checked;
   Close;
end;


procedure TReqAntOptsForm.FormClose(Sender: TObject;  var Action: TCloseAction);
begin
   Action := caFree;
end;

procedure TReqAntOptsForm.FormCreate(Sender: TObject);
begin
   Edit1.Text := RealToString(MDDef.ObsAboveGround,-12,-2);
   Edit2.Text := RealToString(MDdef.MaskObsRange,-12,-2);
   Edit3.Text := IntToStr(MDDef.MinTerrainFlyAbove);
   CheckBox1.Checked := MDDef.DoReqAntHigh;
   CheckBox2.Checked := MDDef.DoReqFlyHigh;
   CheckBox3.Checked := MDDef.DoLOSProfile;
   CheckBox4.Checked := MDDef.DoGrazingAngle;
   CheckBox5.Checked := MDDef.DoEarthCurvature;
   Petmar.PlaceFormAtMousePosition(Self);
end;

initialization
finalization
end.
