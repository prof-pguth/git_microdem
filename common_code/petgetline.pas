unit petgetline;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2026 Peter L. Guth  }
{________________________________________}

{$I nevadia_defines.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Buttons, Spin,
  PETMAR,Petmar_types;

type
  Tlineparamsform = class(TForm)
    Image1: TImage;
    ColorDialog1: TColorDialog;
    Button1: TButton;
    SpinButton1: TSpinButton;
    Width: TLabel;
    BitBtn1: TBitBtn;
    procedure Button1Click(Sender: TObject);
    procedure SpinButton1DownClick(Sender: TObject);
    procedure SpinButton1UpClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure ShowSymbols;
  public
    { Public declarations }
    TheColor : tPlatformColor;
    TheSize  : integer;
  end;


implementation

{$R *.DFM}

uses
   Nevadia_Main,
   PetImage;

procedure Tlineparamsform.ShowSymbols;
var
   Bitmap : tMyBitmap;
begin
   CreateBitmap(Bitmap,120,65);
   Bitmap.Canvas.Pen.Color := ConvertPlatformColorToTColor(TheColor);
   Bitmap.Canvas.Pen.Width := TheSize;
   Bitmap.Canvas.MoveTo(10,30);
   Bitmap.Canvas.LineTo(110,30);
   Image1.Picture.Graphic := Bitmap;
   Bitmap.Free;
end;


procedure Tlineparamsform.Button1Click(Sender: TObject);
begin
   Close;
end;

procedure Tlineparamsform.SpinButton1DownClick(Sender: TObject);
begin
   if (TheSize > 1) then dec(TheSize);
   ShowSymbols;
end;

procedure Tlineparamsform.SpinButton1UpClick(Sender: TObject);
begin
   inc(TheSize);
   ShowSymbols;
end;

procedure Tlineparamsform.FormActivate(Sender: TObject);
begin
   ShowSymbols;
   ColorBitBtn(BitBtn1,TheColor);
   PlaceFormAtMousePosition(Self);
end;

procedure Tlineparamsform.BitBtn1Click(Sender: TObject);
begin
   QueryColor(BitBtn1,TheColor);
   ShowSymbols;
end;

procedure Tlineparamsform.FormCreate(Sender: TObject);
begin
   ClientWidth := 254;
   ClientHeight := 133;
   PlaceFormAtMousePosition(Self);
end;

end.
