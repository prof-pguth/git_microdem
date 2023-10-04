unit legend_placement;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2023 Peter L. Guth  }
{___________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
{$EndIf}


interface

uses
  Windows, Messages, SysUtils,  Classes, Graphics, Controls, Forms,
  Vcl.ExtCtrls,
  Dialogs, StdCtrls,  Buttons,
  Petmar_types,DEMDefs,DEMmapf;

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
    Edit1: TEdit;
    Edit2: TEdit;
    BitBtn2: TBitBtn;
    procedure OKBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CheckBox1Click(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure RadioGroup3Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
  private
    { Private declarations }
     aFont : Petmar_types.tMyFont;
     theLegendLocation : tLegendLocation;
     MapForm : tMapForm;
  public
    { Public declarations }
  end;

const
   loScaleBar = 0;
   loMapName = 1;
   loElevLegend = 2;
   loTerrainCat = 3;
   loNorthArrow = 4;
   loDBlegend = 5;

procedure LegendOptions(inMapForm : tMapForm; WhatFor : string35;var TheFont : Petmar_types.tMyFont; var LegendLocation : tLegendLocation; LegendOption : byte);


implementation

{$R *.dfm}

uses
   Petmar;


procedure LegendOptions(inMapForm : tMapForm; WhatFor : string35; var TheFont : Petmar_types.tMyFont; var LegendLocation : tLegendLocation; LegendOption : byte);
var
  leg_opts_form: Tleg_opts_form;
begin
//LegendOption in [loScaleBar,loMapName,loElevLegend,loTerrainCat,loNorthArrow,loDBLegend];
   leg_opts_form := Tleg_opts_form.Create(Application);
   with leg_opts_form do begin
      Caption := WhatFor;
      theLegendLocation := LegendLocation;
      CheckBox1.Checked := LegendLocation.DrawItem;
      RadioGroup1.Enabled := LegendLocation.DrawItem;
      RadioGroup1.ItemIndex := pred(LegendLocation.MapPosition);
      RadioGroup2.ItemIndex := ord(LegendLocation.HorizontalLegend);
      RadioGroup3.ItemIndex := pred(LegendLocation.LegendSize);

      RadioGroup2.Visible := LegendOption in [loMapName,loElevLegend,loTerrainCat,loNorthArrow,loDBLegend];
      Label1.Visible := LegendOption in [loMapName,loElevLegend,loTerrainCat,loNorthArrow,loDBLegend];
      Label2.Visible := LegendOption in [loMapName,loElevLegend,loTerrainCat,loNorthArrow,loDBLegend];
      Edit1.Visible := LegendOption in [loMapName,loElevLegend,loTerrainCat,loNorthArrow,loDBLegend];
      Edit2.Visible := LegendOption in [loMapName,loElevLegend,loTerrainCat,loNorthArrow,loDBLegend];
      BitBtn1.Visible := LegendOption in [loMapName,loElevLegend,loTerrainCat,loNorthArrow,loDBLegend];

      Edit1.Text := IntToStr(MDDef.LegendBarWidth);
      Edit2.Text := IntToStr(MDDef.LegendTickSize);
      aFont := TheFont;

      MapForm := inMapForm;
      ShowModal;

      TheFont := aFont;
      LegendLocation.DrawItem := CheckBox1.Checked;
      LegendLocation.MapPosition := succ(RadioGroup1.ItemIndex);
      LegendLocation.HorizontalLegend := RadioGroup2.ItemIndex = 1;
      LegendLocation.LegendSize := succ(RadioGroup3.ItemIndex);
      CheckEditString(Edit1.Text,MDDef.LegendBarWidth);
      CheckEditString(Edit2.Text,MDDef.LegendTickSize);
   end;
end;


procedure Tleg_opts_form.BitBtn1Click(Sender: TObject);
begin
   EditMyFont(aFont);
end;


procedure Tleg_opts_form.BitBtn2Click(Sender: TObject);
begin
   MapForm.DoCompleteMapRedraw;
end;

procedure Tleg_opts_form.CheckBox1Click(Sender: TObject);
begin
   theLegendLocation.DrawItem := CheckBox1.Checked;
   RadioGroup1.Enabled := theLegendLocation.DrawItem;
end;

procedure Tleg_opts_form.Edit1Change(Sender: TObject);
begin
   CheckEditString(Edit1.Text,MDDef.LegendBarWidth);
end;

procedure Tleg_opts_form.Edit2Change(Sender: TObject);
begin
   CheckEditString(Edit2.Text,MDDef.LegendTickSize);
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


procedure Tleg_opts_form.RadioGroup3Click(Sender: TObject);
begin
   theLegendLocation.LegendSize := succ(RadioGroup3.ItemIndex);
end;


initialization
finalization
   {$IfDef RecordClosingProblems} WriteLineToDebugFile('Closing legend_placement out'); {$EndIf}
end.
