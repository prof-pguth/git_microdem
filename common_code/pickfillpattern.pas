unit pickfillpattern;

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
  Windows, SysUtils, Classes, Graphics,  Forms, Buttons,
  Vcl.Samples.Spin, Vcl.ExtCtrls, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Controls,
  Petmar_types;

type
  TPickFillForm = class(TForm)
    Image1: TImage;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    OKBtn: TBitBtn;
    ColorDialog1: TColorDialog;
    Image2: TImage;
    SpinButton1: TSpinButton;
    Label1: TLabel;
    Label2: TLabel;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure Image1DblClick(Sender: TObject);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
    procedure SpinButton1DownClick(Sender: TObject);
    procedure SpinButton1UpClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    PickStyle :tBrushStyle;
    PickFillColor : tPlatformColor;
    PickBorderColor : tPlatformColor;
    PickBorderWidth : integer;
    LastX,LastY : integer;
    BaseBitmap : tMyBitmap;
    procedure ShowPickedPattern;
  end;


implementation

{$R *.DFM}

uses
   Petmar,PetImage;


procedure TPickFillForm.SpinButton1DownClick(Sender: TObject);
begin
   if (PickBorderWidth > 0) then begin
      dec(PickBorderWidth);
      ShowPickedPattern;
   end;
end;


procedure TPickFillForm.SpinButton1UpClick(Sender: TObject);
begin
    inc(PickBorderWidth);
    ShowPickedPattern;
end;


procedure TPickFillForm.Image1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
   LastX := x;
   LastY := y;
end;


procedure TPickFillForm.FormCreate(Sender: TObject);
var
   Bitmap : tMyBitmap;

   procedure Pattern(aStyle : tBrushStyle; y :integer);
   begin
      if (aStyle = bsClear) then exit;
      Bitmap.Canvas.Brush.Style := aStyle;
      Bitmap.Canvas.Brush.Color := clSilver;
      Bitmap.Canvas.Rectangle(10,y,90,Y+20);
   end;

begin
   BaseBitMap := Nil;
   CreateBitmap(Bitmap,100,200);
   Bitmap.Canvas.Rectangle(10,34,90,54);
   Pattern(bsBDiagonal,56);
   Pattern(bsFDiagonal,78);
   Pattern(bsCross,100);
   Pattern(bsDiagCross,122);
   Pattern(bsHorizontal,144);
   Pattern(bsVertical,166);
   Pattern(bsSolid,12);
   Image1.Picture.Graphic := Bitmap;
   Bitmap.Free;
   Petmar.PlaceFormAtMousePosition(Self);
end;


procedure TPickFillForm.ShowPickedPattern;
var
   Bitmap : tMyBitmap;
   PolyLinePoints  : array[1..5] of tPoint;
   i : integer;
begin
   CreateBitmap(Bitmap,100,100);
   if (BaseBitmap <> Nil) then Bitmap.Canvas.Draw(0,0,BaseBitmap);
   Bitmap.Canvas.Brush.Style := PickStyle;
   Bitmap.Canvas.Brush.Color := ConvertPlatformColorTotColor(PickFillColor);
   Bitmap.Canvas.Pen.Color := ConvertPlatformColorTotColor(PickBorderColor);
   Bitmap.Canvas.Pen.Width := PickBorderWidth;

   BitBtn2.Enabled := PickStyle <> bsClear;

   PolyLinePoints[1].x := 15;
   PolyLinePoints[1].y := 15;
   PolyLinePoints[2].x := 15;
   PolyLinePoints[2].y := 85;
   PolyLinePoints[3].x := 85;
   PolyLinePoints[3].y := 85;
   PolyLinePoints[4].x := 85;
   PolyLinePoints[4].y := 15;
   PolyLinePoints[5].x := 15;
   PolyLinePoints[5].y := 15;

   if (PickStyle = bsClear) then begin
      Bitmap.Canvas.MoveTo(PolyLinePoints[1].x,PolyLinePoints[1].y);
      for i := 2 to 5 do Bitmap.Canvas.LineTo(PolyLinePoints[i].x,PolyLinePoints[i].y);
   end
   else Bitmap.Canvas.Polygon(slice(PolyLinePoints,5));
   Image2.Picture.Graphic := Bitmap;
   Bitmap.Free;
   Label2.Caption := IntegerToString(PickBorderWidth,-2);
end;


procedure TPickFillForm.OKBtnClick(Sender: TObject);
begin
   Close;
end;

procedure TPickFillForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   Action := caFree;
end;

procedure TPickFillForm.BitBtn1Click(Sender: TObject);
begin
   QueryColor(BitBtn1,PickBorderColor);
   ShowPickedPattern;
end;

procedure TPickFillForm.BitBtn2Click(Sender: TObject);
begin
  QueryColor(BitBtn2,PickFillColor);
  if SameColor(PickFillColor,claWhite) then PickFillColor := RGBtrip(255,255,254);
  ShowPickedPattern;
end;

procedure TPickFillForm.Image1DblClick(Sender: TObject);
begin
   case LastY of
      12..32 : PickStyle := bsSolid;
      34..54 : PickStyle := bsClear;
      56..76 : PickStyle := bsBDiagonal;
      78..98 : PickStyle := bsFDiagonal;
      100..120 : PickStyle := bsCross;
      122..142 : PickStyle := bsDiagCross;
      144..164 : PickStyle := bsHorizontal;
      166..186 : PickStyle := bsVertical;
   end;
   ShowPickedPattern;
end;

procedure TPickFillForm.Button1Click(Sender: TObject);
begin
   QueryColor(BitBtn1,PickBorderColor);
   PickFillColor := PickBorderColor;
   ColorBitBtn(BitBtn2,PickBorderColor);
   ShowPickedPattern;
end;


end.

