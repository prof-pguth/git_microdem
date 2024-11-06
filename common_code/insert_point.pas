unit insert_point;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program       }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2024 Peter L. Guth   }
{____________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
{$EndIf}

interface

uses
  Windows, Messages, SysUtils,  Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls,
  Petmar_types;

type
  TInsertPointForm = class(TForm)
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    RadioGroup1: TRadioGroup;
    HelpBtn: TBitBtn;
    CancelBtn: TBitBtn;
    OKBtn: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
     Factor : float64;
  end;

implementation

{$R *.dfm}

uses
   DEMDefs,DEMDef_Routines,Petmar;

procedure TInsertPointForm.FormCreate(Sender: TObject);
begin
   RadioGroup1.ItemIndex := ord(MDDef.RangeCircleUnit);
   PlaceFormAtMousePosition(self);
end;

procedure TInsertPointForm.HelpBtnClick(Sender: TObject);
begin
    DisplayHTMLTopic('html\insert_pt.htm');
end;

procedure TInsertPointForm.OKBtnClick(Sender: TObject);
begin
   Close;
end;

procedure TInsertPointForm.RadioGroup1Click(Sender: TObject);
begin
   MDDef.RangeCircleUnit := tRangeCircleUnit(RadioGroup1.ItemIndex);
   DEMDef_Routines.GetRangeFactor(Factor);
end;

end.
