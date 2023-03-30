unit slope_graph_opts;

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
  Dialogs, StdCtrls, Buttons;

type
  Tslopegraphopts = class(TForm)
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    CheckBox9: TCheckBox;
    HelpBtn: TBitBtn;
    OKBtn: TBitBtn;
    Label1: TLabel;
    Edit1: TEdit;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  slopegraphopts: Tslopegraphopts;

implementation

{$R *.dfm}

uses
   DEMDefs,Petmar;

procedure Tslopegraphopts.FormClose(Sender: TObject;  var Action: TCloseAction);
begin
   action := caFree;
end;

procedure Tslopegraphopts.FormCreate(Sender: TObject);
begin
   CheckBox1.Checked := MDDef.ShowElevFreq;
   CheckBox2.Checked := MDDef.ShowSlopeFreq;
   CheckBox3.Checked := MDDef.ShowElevSlope;
   CheckBox4.Checked := MDDef.ShowCumSlope;
   CheckBox5.Checked := MDDef.ShowAspectRose;
   CheckBox6.Checked := MDDef.ShowElevSlopeDeg;
   CheckBox8.Checked := MDDef.ShowColorLegend;
   CheckBox9.Checked := MDDef.ShowSDonElevSlope;
   Edit1.Text := IntToStr(MDDef.ElevBinSize);
end;


procedure Tslopegraphopts.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\elev_slope_graph.htm');
end;

procedure Tslopegraphopts.OKBtnClick(Sender: TObject);
begin
   MDDef.ShowElevFreq:= CheckBox1.Checked;
   MDDef.ShowSlopeFreq := CheckBox2.Checked;
   MDDef.ShowElevSlope := CheckBox3.Checked;
   MDDef.ShowCumSlope := CheckBox4.Checked;
   MDDef.ShowAspectRose := CheckBox5.Checked;
   MDDef.ShowElevSlopeDeg := CheckBox6.Checked;
   MDDef.ShowColorLegend := CheckBox8.Checked;
   MDDef.ShowSDonElevSlope := CheckBox9.Checked;
   CheckEditString(Edit1.Text,MDDef.ElevBinSize);
   Close;
end;


end.

