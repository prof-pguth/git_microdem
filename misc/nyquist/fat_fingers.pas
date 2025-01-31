unit fat_fingers;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{________________________________________}

{$I nevadia_defines.inc}


{$IfDef RecordProblems} //normally only defined for debugging specific problems
   {$IfDef Debug}
   {$EndIf}
{$EndIf}


interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.Buttons, Vcl.Grids,
  DEMMapf,Petmar,petmar_types;

type
  Tfat_fingers_form = class(TForm)
    Memo1: TMemo;
    BitBtn1: TBitBtn;
    StringGrid1: TStringGrid;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    SaveSpeedButton: TSpeedButton;
    Label4: TLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BitBtn1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure SaveSpeedButtonClick(Sender: TObject);
  private
    { Private declarations }
    N : integer;
    SumErrors : float64;
    procedure SetTarget;
    procedure ResetGame;
  public
    { Public declarations }
    BaseMap : tMapForm;
    xtarget,ytarget : integer;
    TargetColor : tPlatformColor;
    ScoreCard : array[-5..5,-5..5] of integer;
    procedure RateFatFingers(x,y : integer);
  end;

var
  fat_fingers_form : Tfat_fingers_form;

procedure StartFatFingers(BaseMap : tMapForm);

implementation

{$R *.dfm}

uses PETImage;


procedure StartFatFingers(BaseMap : tMapForm);
begin
   fat_fingers_form := Tfat_fingers_form.create(Application);
   fat_fingers_form.BaseMap := BaseMap;
   fat_fingers_form.Caption := 'Fat fingers for ' +  GetComputerNetName;
   fat_fingers_form.Label4.Caption := fat_fingers_form.Caption;
   fat_fingers_form.ResetGame;
   fat_fingers_form.Show;
end;


procedure CloseFatFingers;
begin
   fat_fingers_form.Close;
   fat_fingers_form := Nil;
end;


{ Tfat_fingers_form }

procedure Tfat_fingers_form.BitBtn1Click(Sender: TObject);
begin
   CloseFatFingers;
end;

procedure Tfat_fingers_form.BitBtn2Click(Sender: TObject);
begin
   QueryColor(BitBtn2,TargetColor);
end;

procedure Tfat_fingers_form.FormClose(Sender: TObject;  var Action: TCloseAction);
begin
   Action := caFree;
end;


procedure Tfat_fingers_form.ResetGame;
var
   i,x,y : integer;
begin
   for i := 1 to 11 do begin
      StringGrid1.Cells[0,i] := IntToStr(i-6);
      StringGrid1.Cells[i,0] := IntToStr(i-6);
   end;
   for x := -5 to 5 do
      for y := -5 to 5 do
         ScoreCard[x,y] := 0;
   N := 0;
   SumErrors := 0;
   SetTarget;
end;

procedure Tfat_fingers_form.FormCreate(Sender: TObject);
begin
   TargetColor := claRed;
   ColorBitBtn(BitBtn2,TargetColor);
end;


procedure Tfat_fingers_form.RateFatFingers(x, y: integer);
var
   xerr,yerr : integer;
begin
   xerr := x-xtarget;
   yerr := ytarget - y;
   SumErrors := SumErrors + sqrt(sqr(xerr) + sqr(yerr));
   Memo1.Lines.Add('xerr=' + IntToStr(xerr) + '  yerr=' + IntToStr(yerr));
   if xerr < -5 then xerr := -5;
   if xerr > 5 then xerr := 5;
   if yerr < -5 then yerr := -5;
   if yerr > 5 then yerr := 5;
   inc(ScoreCard[xerr,yerr]);
   inc(n);
   Label1.Caption := 'Score: ' + RealToString(100 * Scorecard[0,0] / N,-8,2) + '%    n=' + IntToStr(n);
   Label2.Caption := 'Average error (pixels)=' + RealToString(SumErrors/N,-8,2);
   Label3.Caption := 'Point pick error: ' + RealToString(SumErrors/N + BaseMap.MapDraw.ScreenPixelSize,-8,2) + 'm';

   for x := -5 to 5 do
      for y := -5 to 5 do
         if ScoreCard[x,y] > 0 then begin
            StringGrid1.Cells[x+6,6-y] := IntToStr(ScoreCard[x,y]);
         end;
   BaseMap.DoFastMapRedraw;
   SetTarget;
end;

procedure Tfat_fingers_form.SaveSpeedButtonClick(Sender: TObject);
var
   fName : PathStr;
begin
   fName := '';
   PetImage.SaveScreenCapture(0,fName,true,true);
end;


procedure Tfat_fingers_form.SetTarget;
begin
   xtarget := BaseMap.ClientWidth div 2 - 50 + Random(150);
   ytarget := BaseMap.ClientHeight div 2 - 50 + Random(150);
   ScreenSymbol(BaseMap.Image1.Canvas,xtarget,yTarget,Cross,10,TargetColor);
end;



initialization
   fat_fingers_form := Nil;
finalization
end.
