unit moment_opts;
 
{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2026 Peter L. Guth  }
{________________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}  //normally only defined for debugging specific problems
{$EndIf}


interface

uses
  Windows, Messages, SysUtils,  Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls;

type
  TMomentOptsForm = class(TForm)
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    OKBtn: TBitBtn;
    HelpBtn: TBitBtn;
    CheckBox6: TCheckBox;
    RadioGroup1: TRadioGroup;
    CheckBox1: TCheckBox;
    CheckBox7: TCheckBox;
    procedure HelpBtnClick(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MomentOptsForm: TMomentOptsForm;

procedure SetMomentOptions;


implementation

{$R *.dfm}

uses
   DEMDefs,Petmar,Nevadia_Main;

procedure SetMomentOptions;
var
   MomentOptsForm: TMomentOptsForm;
begin
   MomentOptsForm := TMomentOptsForm.Create(Application);
   InsureFormOnScreenCurrentLocation(MomentOptsForm,Mouse.CursorPos.X,Mouse.CursorPos.Y);
   MomentOptsForm.ShowModal;
   MomentOptsForm.Free;
end;

 
procedure TMomentOptsForm.CheckBox1Click(Sender: TObject);
begin
   MDDef.RoughnessMoments := CheckBox1.Checked;
end;

procedure TMomentOptsForm.FormCreate(Sender: TObject);
begin
   CheckBox1.Checked := MDDef.RoughnessMoments;
   CheckBox2.Checked := MDDef.ElevMoments;
   CheckBox3.Checked := MDDef.SlopeMoments;
   {$IfDef AllowCurvatureStatistics}
      CheckBox4.Checked := MDDef.PlanCurvMoments;
      CheckBox5.Checked := MDDef.SlopeCurvMoments;
   {$Else}
      CheckBox4.Visible := false;
      CheckBox5.Visible := false;
   {$EndIf}
   CheckBox6.Checked := MDDef.GraphsOfMoments;
   CheckBox7.Checked := MDDef.LongMoments;
   if MDDef.CountHistograms then RadioGroup1.ItemIndex := 0
   else RadioGroup1.ItemIndex := 1;
end;

procedure TMomentOptsForm.HelpBtnClick(Sender: TObject);
begin
   Petmar.DisplayHTMLTopic('html\moment_stats.htm');
end;

procedure TMomentOptsForm.OKBtnClick(Sender: TObject);
begin
   MDDef.RoughnessMoments := CheckBox1.Checked;
   MDDef.ElevMoments := CheckBox2.Checked;
   MDDef.SlopeMoments := CheckBox3.Checked;
   {$IfDef AllowCurvatureStatistics}
      MDDef.PlanCurvMoments := CheckBox4.Checked;
      MDDef.SlopeCurvMoments := CheckBox5.Checked;
   {$EndIf}
   MDDef.GraphsOfMoments := CheckBox6.Checked;
   MDDef.LongMoments := CheckBox7.Checked;
   MDDef.CountHistograms := RadioGroup1.ItemIndex = 0;
   Close;
end;


end.
