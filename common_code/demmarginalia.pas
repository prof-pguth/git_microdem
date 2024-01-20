unit demmarginalia;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   //{$Define RecordMarginalia}
{$EndIf}

interface

uses
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls,
  Windows,  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons,
  DEMMapf, PETMAR;

type
  TDemMarginaliaForm = class(TForm)
    Image2: TImage;
    Label1: TLabel;
    StatusBar1: TStatusBar;
    Panel1: TPanel;
    El: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn5: TBitBtn;
    Label4: TLabel;
    Label5: TLabel;
    Edit7: TEdit;
    Edit8: TEdit;
    BitBtn1: TBitBtn;
    HelpBtn: TBitBtn;
    BitBtn4: TBitBtn;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    BitBtn2: TBitBtn;
    BitBtn6: TBitBtn;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    BitBtn7: TBitBtn;
    Label2: TLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure HelpBtnClick(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure Image2MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
    procedure CheckBox1Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure ElClick(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn6Click(Sender: TObject);
    procedure CheckBox5Click(Sender: TObject);
    procedure BitBtn7Click(Sender: TObject);
    procedure CheckBox6Click(Sender: TObject);
  private
    { Private declarations }
      procedure DrawPreview;
  public
    { Public declarations }
     MapOwner : tMapForm;
     MultFact : integer;
  end;


procedure MapMarginalia(inMapOwner : tMapForm);


implementation

{$R *.DFM}

uses
   DEMDefs,DEMGrPik, legend_placement, PETImage,Petmar_types;


procedure MapMarginalia(inMapOwner : tMapForm);
var
  DemMarginaliaForm : TDemMarginaliaForm;
begin
   {$IfDef RecordMarginalia} WriteLineToDebugFile('MapMarginalia in'); {$EndIf}

   DemMarginaliaForm := TDemMarginaliaForm.Create(Application);
   with DemMarginaliaForm do begin
      CheckBox1.Checked := MDDef.GridLegendLocation.DrawItem;
      CheckBox2.Checked := MDDef.ScaleBarLocation.DrawItem;
      CheckBox3.Checked := MDDef.TerrainCatLegend.DrawItem;
      CheckBox5.Checked := MDDef.NorthArrowLocation.DrawItem;
      CheckBox6.Checked := MDDef.MapNameLocation.DrawItem;
      Label2.Caption := 'Max distance distortion: ' + RealToString(inMapOwner.MapDraw.MaxScaleDistortionOnMap,-12,-2) + '%';

      Edit7.Enabled := inMapOwner.MapDraw.DEMMap;
      Edit8.Enabled := inMapOwner.MapDraw.DEMMap;
      Label4.Enabled := inMapOwner.MapDraw.DEMMap;
      Label5.Enabled := inMapOwner.MapDraw.DEMMap;
      MapOwner := inMapOwner;
      DrawPreview;
      ShowModal;
      inMapOwner.MapDraw.DeleteSingleMapLayer(inMapOwner.MapDraw.LegendOverlayfName);
      inMapOwner.DoFastMapRedraw;
   end;
   {$IfDef RecordMarginalia} WriteLineToDebugFile('MapMarginalia out'); {$EndIf}
end;


procedure TDemMarginaliaForm.DrawPreview;
var
   Bitmap : tMyBitmap;
begin
   if PetImage.CopyImageToBitmap(MapOwner.Image1,Bitmap) then begin
      MultFact := 1;
      while (Bitmap.Height div MultFact > 400) or (Bitmap.Width div MultFact > 400) do inc(MultFact);
      Image2.Stretch := true;
      Image2.Height := Bitmap.Height div MultFact;
      Image2.Width := Bitmap.Width div MultFact;
      Image2.Picture.Graphic := Bitmap;
      Bitmap.Free;
      if (Image2.Height > 375) then ClientHeight := Image2.Height + 25
      else ClientHeight := 400;
   end;
end;


procedure TDemMarginaliaForm.ElClick(Sender: TObject);
begin
   Legend_Placement.LegendOptions(MapOwner,'Elevation legend',MDDef.LegendFont,MDDef.GridLegendLocation,loElevLegend);
   CheckBox1.Checked := MDDef.GridLegendLocation.DrawItem;
   MapOwner.MapDraw.DeleteSingleMapLayer(MapOwner.MapDraw.LegendOverlayfName);
   MapOwner.DoFastMapRedraw;
   DrawPreview;
end;


procedure TDemMarginaliaForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   Action := caFree;
end;

procedure TDemMarginaliaForm.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\marginalia.htm');
end;

procedure TDemMarginaliaForm.BitBtn1Click(Sender: TObject);
begin
   with MapOwner.MapDraw do begin
      DeleteSingleMapLayer(BaseMapFName);
      DeleteSingleMapLayer(LegendOverlayfName);
   end;
   MapOwner.DoFastMapRedraw;
   DrawPreview;
end;


procedure TDemMarginaliaForm.Image2MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
   StatusBar1.SimpleText := 'x=' + IntToStr(x*MultFact) +   '  & y=' + IntToStr(y*MultFact);
end;


procedure TDemMarginaliaForm.CheckBox1Click(Sender: TObject);
begin
   MDDef.GridLegendLocation.DrawItem := CheckBox1.Checked;
end;

procedure TDemMarginaliaForm.CheckBox2Click(Sender: TObject);
begin
   MDDef.ScaleBarLocation.DrawItem := CheckBox2.Checked;
end;

procedure TDemMarginaliaForm.CheckBox3Click(Sender: TObject);
begin
   MDDef.TerrainCatLegend.DrawItem := CheckBox3.Checked;
end;

procedure TDemMarginaliaForm.CheckBox5Click(Sender: TObject);
begin
   MDDef.NorthArrowLocation.DrawItem := CheckBox5.Checked;
end;

procedure TDemMarginaliaForm.CheckBox6Click(Sender: TObject);
begin
   MDDef.MapNameLocation.DrawItem := CheckBox6.Checked;
end;

procedure TDemMarginaliaForm.BitBtn2Click(Sender: TObject);
begin
   ChangeGridOptions(MapOwner);
end;

procedure TDemMarginaliaForm.BitBtn3Click(Sender: TObject);
begin
   Legend_Placement.LegendOptions(MapOwner,'Scale bar',MDDef.LegendFont,MDDef.ScaleBarLocation,loScaleBar);
   CheckBox2.Checked := MDDef.ScaleBarLocation.DrawItem;
   MapOwner.MapDraw.DeleteSingleMapLayer(MapOwner.MapDraw.LegendOverlayfName);
   MapOwner.DoFastMapRedraw;
   DrawPreview;
end;

procedure TDemMarginaliaForm.BitBtn4Click(Sender: TObject);
begin
   Close;
end;

procedure TDemMarginaliaForm.BitBtn5Click(Sender: TObject);
begin
   Legend_Placement.LegendOptions(MapOwner,'Terrain category legend',MDDef.LegendFont,MDDef.TerrainCatLegend,loTerrainCat);
   CheckBox3.Checked := MDDef.TerrainCatLegend.DrawItem;
end;


procedure TDemMarginaliaForm.BitBtn6Click(Sender: TObject);
begin
   Legend_Placement.LegendOptions(MapOwner,'North arrow',MDDef.LegendFont,MDDef.NorthArrowLocation,loNorthArrow);
   CheckBox5.Checked := MDDef.NorthArrowLocation.DrawItem;
end;


procedure TDemMarginaliaForm.BitBtn7Click(Sender: TObject);
begin
   Legend_Placement.LegendOptions(MapOwner,'Map name',MDDef.TitleLabelFont,MDDef.MapNameLocation,loMapName);
   GetString('Map title',MapOwner.MapDraw.BaseTitle,false,ReasonableTextChars);
   CheckBox6.Checked := MDDef.MapNameLocation.DrawItem;
end;

initialization
finalization
   {$IfDef RecordMarginalia} WriteLineToDebugFile('RecordMarginalia active in DEMmarginalia'); {$EndIf}
end.
