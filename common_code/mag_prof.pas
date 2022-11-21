unit mag_prof;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2022 Peter L. Guth  }
{___________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   //{$Define RecordMGTProblems}
{$EndIf}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons,
  DEMLOSW, Vcl.ExtCtrls;

type
  TPickMagProfVars = class(TForm)
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    BitBtn1: TBitBtn;
    CheckBox1: TCheckBox;
    Edit4: TEdit;
    Label4: TLabel;
    BitBtn3: TBitBtn;
    Label5: TLabel;
    Label6: TLabel;
    Edit5: TEdit;
    Edit6: TEdit;
    Edit7: TEdit;
    Label7: TLabel;
    CheckBox2: TCheckBox;
    Label8: TLabel;
    RadioGroup1: TRadioGroup;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure Edit5Change(Sender: TObject);
    procedure Edit6Change(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    LOSForm : TDEMLOSF;
    procedure ShowValues;
  end;


implementation

{$R *.dfm}


uses
   Petmar_types,PETMAR,DEMdefs;


procedure TPickMagProfVars.ShowValues;
begin
   Edit1.Text := RealToString(DEMLOSW.MagI,-8,-2);
   Edit2.Text := RealToString(DEMLOSW.MagAlpha,-8,-2);
   Edit3.Text := IntToStr(DEMLOSW.BlockIntensity);
   Edit5.Text := RealToString(DEMLOSW.TimeLeft,-8,-2);
   Edit6.Text := RealToString(DEMLOSW.TimeRight,-8,-2);
   Edit7.Text := RealToString(0.0005 * LOSForm.LOSdraw.FormSectLenMeters,-8,-2);
   CheckBox1.Checked := DEMLOSW.MagTimeScale;
   CheckBox2.Checked := DEMLOSW.MagAnomNames;
end;


procedure TPickMagProfVars.BitBtn1Click(Sender: TObject);
begin
   {$IfDef RecordMGTProblems} WriteLineToDebugFile('TPickMagProfVars.BitBtn1Click');  {$EndIf}
   CheckEditString(Edit1.Text,DEMLOSW.MagI);
   CheckEditString(Edit2.Text,DEMLOSW.MagAlpha);
   CheckEditString(Edit3.Text,DEMLOSW.BlockIntensity);
   DEMLOSW.MagTimeScale := CheckBox1.Checked;
   DEMLOSW.MagAnomNames := CheckBox2.Checked;
   Edit4.Text := RealToString(SpreadRidge,6,1);
   LOSForm.MarineMagneticAnomalies;
end;


procedure TPickMagProfVars.BitBtn3Click(Sender: TObject);
begin
   DisplayHTMLTopic('html\model_mag_anom_prof.htm');
end;


procedure TPickMagProfVars.Edit5Change(Sender: TObject);
begin
   CheckEditString(Edit5.Text,DEMLOSW.TimeLeft);
   CheckEditString(Edit6.Text,DEMLOSW.TimeRight);
   DEMLOSW.SpreadRidge := (0.001 * LOSForm.LOSdraw.FormSectLenMeters) / abs(DEMLOSW.TimeRight - DEMLOSW.TimeLeft);
end;

procedure TPickMagProfVars.Edit6Change(Sender: TObject);
begin
   Edit5Change(Sender);
end;


procedure TPickMagProfVars.RadioGroup1Click(Sender: TObject);
begin
   MDdef.MagLineWidth := succ(RadioGroup1.ItemIndex);
end;

initialization
finalization
   {$IfDef RecordMGTProblems} WriteLineToDebugFile('RecordMGTProblems active in mag_prof');{$EndIf}
end.
