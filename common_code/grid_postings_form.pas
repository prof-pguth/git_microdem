unit grid_postings_form;

interface

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{________________________________________}


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
    RedrawSpeedButton12: TSpeedButton;
    BitBtn3: TBitBtn;
    Label1: TLabel;
    Edit1: TEdit;
    Label2: TLabel;
    Edit2: TEdit;
    CheckBox2: TCheckBox;
    procedure BitBtn1Click(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BitBtn2Click(Sender: TObject);
    procedure RedrawSpeedButton12Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
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
   MapOwner.DoFastMapRedraw;
end;


procedure Tgrid_posting_options.OKBtnClick(Sender: TObject);
begin
   Close;
end;

procedure Tgrid_posting_options.RadioGroup1Click(Sender: TObject);
begin
   MapOwner.MapDraw.DEMGridRedraw := RadioGroup1.ItemIndex = 1;
end;

procedure Tgrid_posting_options.RedrawSpeedButton12Click(Sender: TObject);
begin
   MapOwner.DoFastMapRedraw;
end;

procedure Tgrid_posting_options.BitBtn2Click(Sender: TObject);
begin
   ReadDefault('Decimal places',MDDef.GridLabelDecimals);
   MapOwner.DoFastMapRedraw;
end;

procedure Tgrid_posting_options.BitBtn3Click(Sender: TObject);
begin
   EditMyFont(MDDef.GridPointLabelFont);
   MapOwner.DoFastMapRedraw;
end;

procedure Tgrid_posting_options.CheckBox1Click(Sender: TObject);
begin
   if (MapOwner <> Nil) then begin
      MapOwner.MapDraw.DEMGridLabel := CheckBox1.Checked;
      MapOwner.DoFastMapRedraw;
   end;
end;

procedure Tgrid_posting_options.CheckBox2Click(Sender: TObject);
begin
   MapOwner.MapDraw.HalfPixelOffsets := CheckBox2.Checked;
   if MapOwner <> Nil then MapOwner.DoFastMapRedraw;
end;

procedure Tgrid_posting_options.Edit1Change(Sender: TObject);
begin
   CheckEditString(Edit1.Text,MDDef.GridLabelXoff);
   if MapOwner <> Nil then MapOwner.DoFastMapRedraw;
end;

procedure Tgrid_posting_options.Edit2Change(Sender: TObject);
begin
   CheckEditString(Edit2.Text,MDDef.GridLabelYoff);
   if MapOwner <> Nil then MapOwner.DoFastMapRedraw;
end;

procedure Tgrid_posting_options.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   Action := caFree;
end;


procedure Tgrid_posting_options.FormCreate(Sender: TObject);
begin
   MapOwner := nil;
   Edit1.Text := IntToStr(MDDef.GridLabelXoff);
   Edit2.Text := IntToStr(MDDef.GridLabelYoff);
end;


end.
