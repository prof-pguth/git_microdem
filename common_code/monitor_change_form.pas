unit monitor_change_form;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
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
    Image1: TImage;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Panel1: TPanel;
    BitBtn1: TBitBtn;
    RedrawSpeedButton12: TSpeedButton;
    HelpBtn: TButton;
    BitBtn2: TBitBtn;
    OKBtn: TButton;
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
   Nevadia_Main, Make_grid,DEMmapDraw,
   PetImage,Petmar,PETMar_types,DEMDefs,DEMCoord;


procedure ModifyChangeMapSettings(aMapOwner : tMapForm);
var
   ChangeMapForm : TChangeMapForm;
begin
   ChangeMapForm := TChangeMapForm.Create(Application);
   ChangeMapForm.MapOwner := aMapOwner;
   ChangeMapForm.Edit1.Text := RealToString(abs(MDDef.TopCutLevel),-12,-3);
   ChangeMapForm.Edit2.Text := RealToString(MDDef.TopCutLevel,-12,-3);
   ChangeMapForm.Edit3.Text := RealToString(MDDef.BottomCutLevel,-12,-3);
   ChangeMapForm.Changed := false;
   InsureFormOnScreenCurrentLocation(ChangeMapForm,Mouse.CursorPos.X,Mouse.CursorPos.Y);
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
   bmp : tMyBitmap;
begin
   bmp := MakeChangeMapLegend(MapOwner.MapDraw.DEMonMap);
   image1.Height := bmp.Height;
   Image1.Width := bmp.Width;
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
