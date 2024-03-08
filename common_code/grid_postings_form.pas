unit grid_postings_form;

interface

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program       }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2024 Peter L. Guth   }
{____________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
{$EndIf}


uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons,
  DEMMapf, Vcl.ExtCtrls;

type
  Tgrid_posting_options = class(TForm)
    RadioGroup1: TRadioGroup;
    CheckBox1: TCheckBox;
    BitBtn1: TBitBtn;
    OKBtn: TBitBtn;
    BitBtn2: TBitBtn;
    procedure BitBtn1Click(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BitBtn2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    MapOwner : tMapForm;
  end;


implementation

{$R *.dfm}


uses
   Petmar,
   DEMDefs;

procedure Tgrid_posting_options.BitBtn1Click(Sender: TObject);
begin
   with MapOwner.MapDraw do
      PetMar.PickSymbol(BitBtn1,DEMGridSym,DEMGridSymSize,DEMGridSymColor,'DEM grid postings');
end;


procedure Tgrid_posting_options.OKBtnClick(Sender: TObject);
begin
   Close;
end;

procedure Tgrid_posting_options.BitBtn2Click(Sender: TObject);
begin
   ReadDefault('Decimal places',MDDef.GridLabelDecimals);
end;

procedure Tgrid_posting_options.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   Action := caFree;
end;


end.
