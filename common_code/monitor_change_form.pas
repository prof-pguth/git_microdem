unit monitor_change_form;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2023 Peter L. Guth  }
{___________________________________}


{$I nevadia_defines.inc}


{$IFDEF DEBUG}
{$ELSE}
{$ENDIF}


interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Buttons, Vcl.StdCtrls,
  DEMMapf, Vcl.ComCtrls, Vcl.ExtCtrls;

type
  TChangeMapForm = class(TForm)
    OKBtn: TButton;
    HelpBtn: TButton;
    RedrawSpeedButton12: TSpeedButton;
    PageControl1: TPageControl;
    Single: TTabSheet;
    Label1: TLabel;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Edit4: TEdit;
    Edit5: TEdit;
    Label7: TLabel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Image1: TImage;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    procedure OKBtnClick(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure RedrawSpeedButton12Click(Sender: TObject);
    procedure Edit4Change(Sender: TObject);
    procedure Edit5Change(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure Edit3Change(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
  private
    { Private declarations }
     procedure Captions;
  public
    { Public declarations }
       MapOwner : tMapForm;
       Changed : boolean;
  end;


procedure ModifyChangeMapSettings(aMapOwner : tMapForm);

implementation

{$R *.dfm}


uses
   Nevadia_Main, Make_grid,
   PetImage,Petmar,PETMar_types,DEMDefs,DEMCoord;


procedure ModifyChangeMapSettings(aMapOwner : tMapForm);
var
   ChangeMapForm: TChangeMapForm;
begin
   ChangeMapForm := TChangeMapForm.Create(Application);
   ChangeMapForm.MapOwner := aMapOwner;
   ChangeMapForm.Edit1.Text := RealToString(abs(MDDef.TopCutLevel),-12,-3);
   ChangeMapForm.Edit2.Text := RealToString(MDDef.TopCutLevel,-12,-3);
   ChangeMapForm.Edit3.Text := RealToString(MDDef.BottomCutLevel,-12,-3);
   ChangeMapForm.Changed := false;
   InsureFormOnScreen(ChangeMapForm,Mouse.CursorPos.X,Mouse.CursorPos.Y);
   ChangeMapForm.Captions;
   ChangeMapForm.Show;
end;


procedure TChangeMapForm.Edit1Change(Sender: TObject);
begin
   CheckEditString(Edit1.Text, MDDef.TopCutLevel);
   MDDef.TopCutLevel := abs(MDDef.TopCutLevel);
   MDDef.BottomCutLevel := -MDDef.TopCutLevel;
   Changed := true;
end;


procedure TChangeMapForm.Edit2Change(Sender: TObject);
begin
   CheckEditString(Edit2.Text,MDDef.TopCutLevel);
   Changed := true;
end;

procedure TChangeMapForm.Edit3Change(Sender: TObject);
begin
   CheckEditString(Edit3.Text,MDDef.BottomCutLevel);
   Changed := true;
end;

procedure TChangeMapForm.Edit4Change(Sender: TObject);
begin
   CheckEditString(Edit4.Text,MDDef.TopCutLevel);
   MDDef.TopCutLevel := DEMGlb[MapOwner.MapDraw.DEMonMap].FindPercentileElevation(MDDef.TopCutLevel);
   Changed := true;
end;

procedure TChangeMapForm.Edit5Change(Sender: TObject);
begin
   CheckEditString(Edit5.Text,MDDef.BottomCutLevel);
   MDDef.BottomCutLevel := DEMGlb[MapOwner.MapDraw.DEMonMap].FindPercentileElevation(MDDef.BottomCutLevel);
   Changed := true;
end;

procedure TChangeMapForm.HelpBtnClick(Sender: TObject);
begin
    DisplayHTMLTopic('html\change_map_display.html');
end;


procedure TChangeMapForm.OKBtnClick(Sender: TObject);
begin
   if Changed then RedrawSpeedButton12Click(Sender);
   close;
end;

procedure TChangeMapForm.BitBtn1Click(Sender: TObject);
var
   fName : PathStr;
begin
   fName := DEMGlb[MapOwner.MapDraw.DEMonMap].AreaName + '_categories';
   DifferenceCategoryMap(MapOwner.MapDraw.DEMonMap,fName);
end;


procedure TChangeMapForm.BitBtn2Click(Sender: TObject);
var
   Vat : tStringList;
   fName : PathStr;
   bmp : tMyBitmap;
   color : tColor;
   CatHeight,Cat : integer;
begin
   CreateBitmap(bmp,1200,800);
   bmp.Canvas.Font.Size := MDDef.LegendFont.Size;
   CatHeight := 6 * bmp.Canvas.TextHeight('Wy') div 5;
   Cat := 0;

   ClearBitmap(Bmp,clNearWhite);
   bmp.Canvas.Font.Style := [fsBold];

   Bmp.Canvas.TextOut(5,1,'    % Area     Category');
   inc(Cat);
   bmp.Canvas.Pen.Color := clLime;
   bmp.Canvas.Brush.Color := clLime;
   bmp.Canvas.Brush.Style := bsSolid;
   bmp.Canvas.Rectangle(5,Cat*CatHeight,40,succ(Cat)*CatHeight);
   bmp.Canvas.Brush.Style := bsClear;
   bmp.Canvas.TextOut(45,Cat*CatHeight + 4, RealToString(DEMGlb[MapOwner.MapDraw.DEMonMap].PercentileOfElevation(MDDef.TopCutLevel),9,2) + '%   Positive Change > ' + RealToString(MDDef.TopCutLevel,-8,-2));

   inc(Cat);
   bmp.Canvas.Pen.Color := clRed;
   bmp.Canvas.Brush.Color := clRed;
   bmp.Canvas.Brush.Style := bsSolid;
   bmp.Canvas.Rectangle(5,Cat*CatHeight,40,succ(Cat)*CatHeight);
   bmp.Canvas.Brush.Style := bsClear;
   bmp.Canvas.TextOut(45,Cat*CatHeight + 4,RealToString(DEMGlb[MapOwner.MapDraw.DEMonMap].PercentileOfElevation(MDDef.BottomCutLevel),9,2) + '%    Negative Change < ' + RealToString(MDDef.BottomCutLevel,-8,-2));
   PutBitmapInBox(Bmp);

   Image1.Picture.Graphic := bmp;
   bmp.Free;
end;


procedure TChangeMapForm.Captions;
begin
   BitBtn2Click(Nil);
end;


procedure TChangeMapForm.RedrawSpeedButton12Click(Sender: TObject);
begin
   MapOwner.DoBaseMapRedraw;
   Captions;
   Changed := false;
end;


initialization
finalization
end.
