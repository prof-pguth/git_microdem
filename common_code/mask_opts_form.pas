unit mask_opts_form;

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
  Dialogs, StdCtrls, Buttons, Vcl.ExtCtrls;  // ExtCtrls;

type
  TMaskOptsForm = class(TForm)
    Label1: TLabel;
    Edit1: TEdit;
    RadioGroup1: TRadioGroup;
    OKBtn: TBitBtn;
    HelpBtn: TBitBtn;
    CheckBox1: TCheckBox;
    BitBtn1: TBitBtn;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    procedure OKBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BitBtn1Click(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


implementation

{$R *.dfm}

uses
   Petmar,DEMTigerOps,DEMdefs;

procedure TMaskOptsForm.BitBtn1Click(Sender: TObject);
begin
   SetTigerOptions(Nil,-99);
end;

procedure TMaskOptsForm.CheckBox3Click(Sender: TObject);
begin
   MDDef.TreatLineAsPolygon := CheckBox3.Checked;
end;

procedure TMaskOptsForm.CheckBox4Click(Sender: TObject);
begin
   MDDef.TreatPolygonAsLine := CheckBox4.Checked;
end;

procedure TMaskOptsForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   Action := caFree;
end;


procedure TMaskOptsForm.FormCreate(Sender: TObject);
begin
   CheckBox3.Checked := MDDef.TreatLineAsPolygon;
   CheckBox4.Checked := MDDef.TreatPolygonAsLine;
end;

procedure TMaskOptsForm.HelpBtnClick(Sender: TObject);
begin
    DisplayHTMLTopic('html\mask_opts.htm');
end;

procedure TMaskOptsForm.OKBtnClick(Sender: TObject);
begin
   CheckEditString(Edit1.Text,MDDef.MaskDistance);
   MDDef.FilterAllTigerRoads := CheckBox1.Checked;
   MDDef.MaskShapesIn := RadioGroup1.ItemIndex = 1;
   MDDef.ShowMasks := CheckBox2.Checked;
   Close;
end;




end.




