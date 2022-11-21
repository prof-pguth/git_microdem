unit legend_placement;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2022 Peter L. Guth  }
{___________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
{$EndIf}


interface

uses
  Windows, Messages, SysUtils,  Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,  Buttons,
  Petmar_types,DEMDefs, Vcl.ExtCtrls;

type
  Tleg_opts_form = class(TForm)
    RadioGroup1: TRadioGroup;
    OKBtn: TBitBtn;
    HelpBtn: TBitBtn;
    CheckBox1: TCheckBox;
    RadioGroup2: TRadioGroup;
    BitBtn1: TBitBtn;
    RadioGroup3: TRadioGroup;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    procedure OKBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CheckBox1Click(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
  private
    { Private declarations }
     aFont : Petmar_types.tMyFont;
     theLegendLocation : tLegendLocation;
  public
    { Public declarations }
  end;


procedure LegendOptions(WhatFor : string35;var TheFont : Petmar_types.tMyFont; var LegendLocation : tLegendLocation; OrientationOption : boolean = true);


implementation

{$R *.dfm}

uses
   Petmar;


procedure LegendOptions(WhatFor : string35; var TheFont : Petmar_types.tMyFont; var LegendLocation : tLegendLocation; OrientationOption : boolean = true);
var
  leg_opts_form: Tleg_opts_form;
begin
   leg_opts_form := Tleg_opts_form.Create(Application);
   with leg_opts_form do begin
      Caption := WhatFor;
      theLegendLocation := LegendLocation;
      CheckBox1.Checked := LegendLocation.DrawItem;
      RadioGroup1.Enabled := LegendLocation.DrawItem;
      RadioGroup1.ItemIndex := pred(LegendLocation.MapPosition);
      RadioGroup2.ItemIndex := ord(LegendLocation.HorizontalLegend);
      RadioGroup2.Visible := OrientationOption;
      RadioGroup3.ItemIndex := pred(LegendLocation.LegendSize);
      Edit1.Text := IntToStr(MDDef.LegendBarWidth);
      Edit2.Text := IntToStr(MDDef.LegendTickSize);
      Edit3.Text := IntToStr(MDDef.SpecifyLegendX);
      aFont := TheFont;

      ShowModal;
      TheFont := aFont;
      LegendLocation.DrawItem := CheckBox1.Checked;
      LegendLocation.MapPosition := succ(RadioGroup1.ItemIndex);
      LegendLocation.HorizontalLegend := RadioGroup2.ItemIndex = 1;
      LegendLocation.LegendSize := succ(RadioGroup3.ItemIndex);
      CheckEditString(Edit1.Text,MDDef.LegendBarWidth);
      CheckEditString(Edit2.Text,MDDef.LegendTickSize);
      CheckEditString(Edit3.Text,MDDef.SpecifyLegendX);
   end;
end;


procedure Tleg_opts_form.BitBtn1Click(Sender: TObject);
begin
   EditMyFont(aFont);
end;


procedure Tleg_opts_form.CheckBox1Click(Sender: TObject);
begin
   theLegendLocation.DrawItem := CheckBox1.Checked;
   RadioGroup1.Enabled := theLegendLocation.DrawItem;
end;

procedure Tleg_opts_form.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   Action := caFree;
end;

procedure Tleg_opts_form.FormCreate(Sender: TObject);
begin
   Petmar.PlaceFormAtMousePosition(Self);
end;

procedure Tleg_opts_form.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\leg_place.htm');
end;

procedure Tleg_opts_form.OKBtnClick(Sender: TObject);
begin
   Close;
end;


initialization
finalization
   {$IfDef RecordClosingProblems} WriteLineToDebugFile('Closing legend_placement out'); {$EndIf}
end.
