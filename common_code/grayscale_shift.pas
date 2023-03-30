unit grayscale_shift;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2023 Peter L. Guth  }
{___________________________________}


{$I nevadia_defines.inc}


interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Vcl.ComCtrls,
  DEMMapf, Vcl.ExtCtrls;

type
  TGrayscaleForm = class(TForm)
    TrackBar1: TTrackBar;
    TrackBar2: TTrackBar;
    BitBtn1: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    TrackBar3: TTrackBar;
    TrackBar4: TTrackBar;
    Image2: TImage;
    Label3: TLabel;
    Label4: TLabel;
    BitBtn2: TBitBtn;
    procedure BitBtn1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure TrackBar1Change(Sender: TObject);
    procedure TrackBar2Change(Sender: TObject);
    procedure TrackBar3Change(Sender: TObject);
    procedure TrackBar4Change(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
  private
    { Private declarations }
     procedure ColorScaleBar;
     procedure LabelTrackBars;
     procedure SetUpForm;
  public
    { Public declarations }
    TheMap : tMapForm;
    ColorScale : integer;
    SetupOver : boolean;
  end;

const
  SlopeGray = 1;
  OpennessGray = 2;
  ChangeRed = 3;
  ChangeGreen = 4;

procedure SetGrayscale(ColorScale : integer; Map : tMapForm);

implementation

{$R *.dfm}

uses
   DEMDefs,DemDef_routines,Petmar_types,PetImage,Petmar_ini_file,
   Nevadia_main;



procedure SetGrayscale(ColorScale : integer;  Map : tMapForm);
var
  Form4 : TGrayscaleForm;
begin
   Form4 := TGrayscaleForm.Create(Application);
   Form4.TheMap := Map;
   Form4.ColorScale := ColorScale;
   Form4.SetupForm;
   InsureFormOnScreen(Form4,Mouse.CursorPos.X,Mouse.CursorPos.Y);
   Form4.ShowModal;
end;


procedure TGrayscaleForm.SetUpForm;
begin
   SetupOver := false;
   if ColorScale = SlopeGray then begin
      TrackBar1.Position := MDDef.MinSlopeForGray;
      TrackBar2.Position := MDDef.MaxSlopeForGray;
      TrackBar3.Position := MDDef.SlopeMinGray;
      TrackBar4.Position := MDDef.SlopeMaxGray;
      Caption := 'Grayscale for reverse slope map';
   end
   else if ColorScale = OpennessGray then begin
      TrackBar1.Position := MDDef.MinUpward;
      TrackBar2.Position := MDDef.MaxUpward;
      TrackBar3.Position := MDDef.OpenMinGray;
      TrackBar4.Position := MDDef.OpenMaxGray;
      Caption := 'Grayscale for openness map';
   end
   else if ColorScale = ChangeRed then begin
      TrackBar1.Position := round(MDDef.ChangeMinRedValue);
      TrackBar2.Position := round(MDDef.ChangeMaxRedValue);
      TrackBar3.Position := MDDef.ChangeMinRedColor;
      TrackBar4.Position := MDDef.ChangeMaxRedColor;
      Caption := 'Red scale for change map';
   end
   else if ColorScale = ChangeGreen then begin
      TrackBar1.Position := round(MDDef.ChangeMinGreenValue);
      TrackBar2.Position := round(MDDef.ChangeMaxGreenValue);
      TrackBar3.Position := MDDef.ChangeMinGreenColor;
      TrackBar4.Position := MDDef.ChangeMaxGreenColor;
      Caption := 'Green scale for change map';
   end;
   ColorScaleBar;
   SetupOver := true;
end;


procedure TGrayscaleForm.BitBtn2Click(Sender: TObject);
begin
   if ColorScale = SlopeGray then ProcessIniFile(iniInit,'SlopeGray')
   else if ColorScale = OpennessGray then ProcessIniFile(iniInit,'OpenGray')
   else if ColorScale = ChangeRed then ProcessIniFile(iniInit,'RedChangeSettings')
   else if ColorScale = ChangeGreen then ProcessIniFile(iniInit,'GreenChangeSettings');
   SetupForm;
end;

procedure TGrayscaleForm.ColorScaleBar;
var
   bitmap : tMyBitmap;
   Color : tColor;
   MinGray,MaxGray,i : integer;
begin
   LabelTrackBars;
   MinGray := TrackBar3.Position;
   MaxGray := TrackBar4.Position;
   CreateBitmap(Bitmap,512,20);
   for I := 0 to 255 do begin
      if ColorScale in [SlopeGray,OpennessGray] then Color := rgb(i,i,i)
      else if ColorScale = ChangeRed then Color := rgb(i,0,0)
      else if ColorScale = ChangeGreen then Color := rgb(0,i,0);
      Bitmap.Canvas.Pen.Color := Color;
      DrawLine(Bitmap,2*i,0,2*i,19);
      DrawLine(Bitmap,succ(2*i),0,succ(2*i),19);
   end;
   Bitmap.Canvas.Pen.Color := clRed;
   Bitmap.Canvas.Brush.Style := bsClear;
   Bitmap.Canvas.Rectangle(2*MinGray,5,2*MaxGray,15);
   Image2.Picture.Graphic := Bitmap;
   Bitmap.Free;
end;

procedure TGrayscaleForm.LabelTrackBars;
begin
   if SetupOver then begin
      if ColorScale = SlopeGray then begin
         MDDef.MinSlopeForGray := TrackBar1.Position;
         MDDef.MaxSlopeForGray := TrackBar2.Position;
         MDDef.SlopeMinGray := TrackBar3.Position;
         MDDef.SlopeMaxGray := TrackBar4.Position;
      end
      else if ColorScale = OpennessGray then begin
         MDDef.MinUpward := TrackBar1.Position;
         MDDef.MaxUpward := TrackBar2.Position;
         MDDef.OpenMinGray := TrackBar3.Position;
         MDDef.OpenMaxGray := TrackBar4.Position;
      end
      else if ColorScale = ChangeRed then begin
         MDDef.ChangeMinRedValue := TrackBar1.Position;
         MDDef.ChangeMaxRedValue := TrackBar2.Position;
         MDDef.ChangeMinRedColor := TrackBar3.Position;
         MDDef.ChangeMaxRedColor := TrackBar4.Position;
      end
      else if ColorScale = ChangeGreen then begin
         MDDef.ChangeMinGreenValue := TrackBar1.Position;
         MDDef.ChangeMaxGreenValue := TrackBar2.Position;
         MDDef.ChangeMinGreenColor := TrackBar3.Position;
         MDDef.ChangeMaxGreenColor := TrackBar4.Position;
      end;
   end;
   Label1.Caption := 'Min Value for color scale: ' + IntToStr(TrackBar1.Position);
   Label2.Caption := 'Max Value for color scale: ' + IntToStr(TrackBar2.Position);
   Label3.Caption := 'Darkest color to use: ' + IntToStr(TrackBar3.Position);
   Label4.Caption := 'Lightest color to use: ' + IntToStr(TrackBar4.Position);
end;


procedure TGrayscaleForm.BitBtn1Click(Sender: TObject);
begin
   ColorScaleBar;
   TheMap.DoBaseMapRedraw;
end;

procedure TGrayscaleForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TGrayscaleForm.TrackBar1Change(Sender: TObject);
begin
   LabelTrackBars;
end;

procedure TGrayscaleForm.TrackBar2Change(Sender: TObject);
begin
   LabelTrackBars;
end;


procedure TGrayscaleForm.TrackBar3Change(Sender: TObject);
begin
   ColorScaleBar;
end;

procedure TGrayscaleForm.TrackBar4Change(Sender: TObject);
begin
   ColorScaleBar;
end;


initialization
finalization
end.
