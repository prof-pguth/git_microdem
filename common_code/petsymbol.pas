unit petsymbol;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{________________________________________}

interface

uses
  PETMAR,
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, StdCtrls, ExtCtrls, ComCtrls, Buttons,
  Petmar_types;

type
  TPickSymbolForm = class(TForm)
    Image1: TImage;
    Button1: TButton;
    Symbol: TLabel;
    UpDown2: TUpDown;
    Size: TLabel;
    OKBtn: TBitBtn;
    HelpBtn: TBitBtn;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    procedure Button1Click(Sender: TObject);
    procedure UpDown2Click(Sender: TObject; Button: TUDBtnType);
    procedure HelpBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Bitmap : tMyBitmap;
    SymCount : integer;
    GetPointSymbol : tDrawingSymbol;
    GetPointSize   : integer;
    GetPointColor  : tPlatformColor;
    procedure SampleSymbol;
  end;

 procedure GetSymbol(var DrSymbol : tDrawingSymbol; var SymSize   : byte; var WantColor  : tPlatformColor; WhatFor : shortstring = '');


implementation

uses
   petimage,
   nevadia_Main;

{$R *.DFM}


 procedure GetSymbol(var DrSymbol : tDrawingSymbol; var SymSize   : byte; var WantColor  : tPlatformColor; WhatFor : shortstring = '');
var
  PickSymbolForm : TPickSymbolForm;
 begin
    PickSymbolForm := TPickSymbolForm.Create(Application);
    with PickSymbolForm do begin
       if (WhatFor <> '') then Caption := WhatFor;
       GetPointColor := WantColor;
       GetPointSymbol := DrSymbol;
       GetPointSize := SymSize;
       UpDown2.Position := GetPointSize;
       SymCount := ord(GetPointSymbol);
       SampleSymbol;
       if (ShowModal <> idCancel) then begin
          WantColor := GetPointColor;
          DrSymbol := GetPointSymbol;
          SymSize := GetPointSize;
       end;
       Free;
    end;
 end;


procedure TPickSymbolForm.Button1Click(Sender: TObject);
begin
   QueryColor(GetPointColor);
   SampleSymbol;
end;

procedure TPickSymbolForm.SampleSymbol;
var
   Bitmap : tMyBitmap;
begin
   GetPointSymbol := tDrawingSymbol(SymCount);
   PetImage.CreateBitmap(Bitmap,40,40);
   ScreenSymbol(Bitmap.Canvas,20,20,GetPointSymbol,GetPointSize,GetPointColor);
   Image1.Picture.Graphic := Bitmap;
   Bitmap.Free;
end;


procedure TPickSymbolForm.SpeedButton1Click(Sender: TObject);
begin
   inc(SymCount);
   if (SymCount > ord(LastSymbol)) then SymCount := 0;
   SampleSymbol;
end;

procedure TPickSymbolForm.SpeedButton2Click(Sender: TObject);
begin
   dec(SymCount);
   if (SymCount < 0) then SymCount := ord(LastSymbol);
   SampleSymbol;
end;

procedure TPickSymbolForm.UpDown2Click(Sender: TObject; Button: TUDBtnType);
begin
   if (Button = btNext) then inc(GetPointSize)
   else begin
      if (GetPointSize > 1) then dec(GetPointSize);
   end;
   SampleSymbol;
end;


procedure TPickSymbolForm.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\symbol_selection.htm');
end;

procedure TPickSymbolForm.FormCreate(Sender: TObject);
begin
   PlaceFormAtMousePosition(Self);
end;

initialization
finalization
end.



