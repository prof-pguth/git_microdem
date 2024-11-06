unit Beach_Ball_Options;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
{$EndIf}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls,
  Petmar_types;

type
  TBeachBallForm = class(TForm)
    RadioGroup1: TRadioGroup;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    OKBtn: TBitBtn;
    BitBtn3: TBitBtn;
    RadioGroup2: TRadioGroup;
    Label4: TLabel;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Label5: TLabel;
    RadioGroup3: TRadioGroup;
    Edit7: TEdit;
    Edit8: TEdit;
    Edit11: TEdit;
    Edit12: TEdit;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Edit9: TEdit;
    procedure OKBtnClick(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure RadioGroup3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


Procedure SetBeachBallOptions(AllowMapChange : boolean; MapPixelSize : float64 = -1);

implementation

{$R *.dfm}


uses
   Petmar,DEMDefs;


Procedure SetBeachBallOptions(AllowMapChange : boolean; MapPixelSize : float64 = -1);
var
   BeachBallForm : TBeachBallForm;
begin
    BeachBallForm := TBeachBallForm.Create(Application);
    BeachBallForm.RadioGroup2.Enabled := AllowMapChange;
    BeachBallForm.Label4.Enabled := AllowMapChange;
    BeachBallForm.Edit4.Enabled := AllowMapChange;
    if (MapPixelSize > 0) then BeachBallForm.Label11.Caption := 'Current map: ' + RealToString(MapPixelSize,-12,-2)
    else BeachBallForm.Label11.Caption := '';
    BeachBallForm.ShowModal;
    BeachBallForm.Free;
end;

procedure TBeachBallForm.BitBtn3Click(Sender: TObject);
begin
   DisplayHTMLTopic('html\beach_ball_opts.htm');
end;

procedure TBeachBallForm.FormCreate(Sender: TObject);
begin
    RadioGroup1.ItemIndex := ord(MDDef.Netdef.BeachBallSize);
    Edit1.Text := IntToStr(MDDef.Netdef.AllBeachBallSize);
    Edit2.Text := IntToStr(MDDef.Netdef.M3BeachBallSize);
    Edit3.Text := IntToStr(MDDef.Netdef.M9BeachBallSize);
    Edit4.Text := IntToStr(MDDef.BeachBallSwitchPixelSize);
    Edit5.Text := RealToString(MDDef.Netdef.MinScaleMagnitude,-12,-2);
    Edit6.Text := RealToString(MDDef.Netdef.MaxScaleMagnitude,-12,-2);
    RadioGroup3.ItemIndex := ord(MDDef.Netdef.BeachBallColor);
    Edit7.Text := RealToString(MDDef.Netdef.MinColorMagnitude,-12,-2);
    Edit8.Text := RealToString(MDDef.Netdef.MaxColorMagnitude,-12,-2);
    Edit9.Text := IntToStr(MDDef.NetDef.MaxNumBeachBalls);
    Edit11.Text := RealToString(MDDef.Netdef.MinColorDepth,-12,-2);
    Edit12.Text := RealToString(MDDef.Netdef.MaxColorDepth,-12,-2);
    RadioGroup2.ItemIndex := ord(MDDef.BeachBallMap);
end;

procedure TBeachBallForm.OKBtnClick(Sender: TObject);
begin
    MDDef.Netdef.BeachBallSize := tBeachBallSize(RadioGroup1.ItemIndex);
    CheckEditString(Edit1.Text,MDDef.Netdef.AllBeachBallSize);
    CheckEditString(Edit2.Text,MDDef.Netdef.M3BeachBallSize);
    CheckEditString(Edit3.Text,MDDef.Netdef.M9BeachBallSize);
    CheckEditString(Edit4.Text,MDDef.BeachBallSwitchPixelSize);
    CheckEditString(Edit5.Text,MDDef.Netdef.MinScaleMagnitude);
    CheckEditString(Edit6.Text,MDDef.Netdef.MaxScaleMagnitude);
    CheckEditString(Edit7.Text,MDDef.Netdef.MinColorMagnitude);
    CheckEditString(Edit8.Text,MDDef.Netdef.MaxColorMagnitude);
    CheckEditString(Edit9.Text,MDDef.NetDef.MaxNumBeachBalls);
    CheckEditString(Edit11.Text,MDDef.Netdef.MinColorDepth);
    CheckEditString(Edit12.Text,MDDef.Netdef.MaxColorDepth);
    MDDef.BeachBallMap := tBeachBallMap(RadioGroup2.ItemIndex);
    MDDef.Netdef.BeachBallColor := tBeachBallColors(RadioGroup3.ItemIndex);
    Close;
end;


procedure TBeachBallForm.RadioGroup1Click(Sender: TObject);
begin
   Edit1.Enabled := RadioGroup1.ItemIndex = 0;
   Edit2.Enabled := RadioGroup1.ItemIndex <> 0;
   Edit3.Enabled := RadioGroup1.ItemIndex <> 0;
   Edit5.Enabled := RadioGroup1.ItemIndex <> 0;
   Edit6.Enabled := RadioGroup1.ItemIndex <> 0;
end;


procedure TBeachBallForm.RadioGroup2Click(Sender: TObject);
begin
   Edit4.Enabled := RadioGroup2.ItemIndex = 2;
   Label4.Enabled := RadioGroup2.ItemIndex = 2;
end;


procedure TBeachBallForm.RadioGroup3Click(Sender: TObject);
begin
    Edit7.Enabled := RadioGroup3.ItemIndex in [1,2];
    Edit8.Enabled := RadioGroup3.ItemIndex in [1,2];
    Edit11.Enabled := RadioGroup3.ItemIndex in [3];
    Edit12.Enabled := RadioGroup3.ItemIndex in [3];
end;


initialization
finalization
end.
