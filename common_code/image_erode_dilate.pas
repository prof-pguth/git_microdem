unit image_erode_dilate;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2026 Peter L. Guth  }
{________________________________________}


{$I nevadia_defines.inc}


{$IfDef RecordProblems}   //normally only defined for debugging specific problems
{$EndIf}


interface

uses
  Windows, SysUtils,  Classes, Graphics, Controls,Forms,
  DEMMapf, StdCtrls, ExtCtrls, Buttons;

type
  TDilateErodeForm = class(TForm)
    BitBtn1: TBitBtn;
    DilateButton: TBitBtn;
    ErodeButton: TBitBtn;
    RadioGroup1: TRadioGroup;
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    HelpBtn: TBitBtn;
    RedrawSpeedButton12: TSpeedButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    SpeedButton22: TSpeedButton;
    SaveSpeedButton: TSpeedButton;
    procedure DilateButtonClick(Sender: TObject);
    procedure ErodeButtonClick(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure RedrawSpeedButton12Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure SpeedButton22Click(Sender: TObject);
    procedure SaveSpeedButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    BaseMap : tMapForm;
  end;


implementation

{$R *.dfm}

uses
  Petimage,Petmar,Petmar_types,DEMDefs;

procedure TDilateErodeForm.BitBtn1Click(Sender: TObject);
begin
   BaseMap.Imagenegative1Click(Nil);
end;


procedure TDilateErodeForm.DilateButtonClick(Sender: TObject);
var
   Bitmap : tMyBitmap;
   BackColor,ImageColor : tColor;
begin
   Bitmap := PetImage.LoadBitmapFromFile(BaseMap.MapDraw.BaseMapFName);
   if (RadioGroup1.ItemIndex = 0) then begin
      BackColor := clBlack;
      ImageColor := clWhite;
   end
   else begin
      BackColor := clWhite;
      ImageColor := clBlack;
   end;

   if (Sender = ErodeButton) then ErodeTheImage(Bitmap,BackColor)
   else DilateTheImage(Bitmap,ImageColor);
   Bitmap.SaveToFile(BaseMap.MapDraw.BaseMapFName);
   Bitmap.Free;
   BaseMap.DoFastMapRedraw;
end;


procedure TDilateErodeForm.Edit1Change(Sender: TObject);
begin
   CheckEditString(Edit1.Text,MDDef.MaxSatRange);
end;

procedure TDilateErodeForm.Edit2Change(Sender: TObject);
begin
   CheckEditString(Edit2.Text,MDDef.MinSatRange);
end;

procedure TDilateErodeForm.ErodeButtonClick(Sender: TObject);
begin
   DilateButtonClick(Sender);
end;

procedure TDilateErodeForm.RedrawSpeedButton12Click(Sender: TObject);
begin
   BaseMap.DoBaseMapRedraw;
end;


procedure TDilateErodeForm.SaveSpeedButtonClick(Sender: TObject);
begin
   BaseMap.Saveimage1Click(Sender);
end;


procedure TDilateErodeForm.SpeedButton22Click(Sender: TObject);
begin
   BaseMap.N11view1Click(Nil);
end;


end.
