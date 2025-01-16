unit drg_anaglyph;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{________________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}  //normally only defined for debugging specific problems
   //{$Define RecordAnaglyphProblems}
{$EndIf}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls,
  DEMDefs,DEMMapF;

type
  TDRGAnaglyphForm = class(TForm)
    OKBtn: TBitBtn;
    BitBtn1: TBitBtn;
    HelpBtn: TBitBtn;
    CheckBox5: TCheckBox;
    RadioGroup1: TRadioGroup;
    Edit2: TEdit;
    Label3: TLabel;
    RadioGroup2: TRadioGroup;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    RedrawSpeedButton12: TSpeedButton;
    procedure BitBtn1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure RedrawSpeedButton12Click(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure CheckBox5Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure OKBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
     MapOwner : tMapForm;
     NeedRedraw : boolean;
  end;


procedure Set3DmapOptions(var MapForm : tMapForm);
procedure Draw3Dmap(MapForm : tMapForm);


implementation

{$R *.dfm}

uses
   {$IfDef ExSat}
   {$Else}
      DEMEros,
   {$EndIf}
   {$IfDef ExStereo}
   {$Else}
      stereo_viewer,
   {$EndIf}
   DEMCoord,DEMRefOp,Petmar_types,PetMar,
   DEM_Manager,DEMMapDraw,
   PetDBUtils, PETImage,BaseMap;


procedure Set3DmapOptions(var MapForm : tMapForm);
var
   DRGAnaglyphForm : DRG_Anaglyph.TDRGAnaglyphForm;
begin
   {$IfDef RecordAnaglyph} WriteLineToDebugFile('DrawAnaglyphMap in'); {$EndIf}
   if (MapForm.MapDraw.DEMonMap = 0) then MapForm.SetDEMwithmap1Click(Nil);
   DRGAnaglyphForm := TDRGAnaglyphForm.Create(Application);
   DRGAnaglyphForm.MapOwner := MapForm;
   DRGAnaglyphForm.RadioGroup1.Enabled := ValidDEM(MapForm.MapDraw.DEMonMap);
   if (MapForm.MapDraw.DEMonMap = 0) then DRGAnaglyphForm.RadioGroup1.ItemIndex := 0;
   DRGAnaglyphForm.Edit2.Text := IntToStr(MDDef.MaxAnaglyphShift);
   {$IfDef RecordAnaglyph} WriteLineToDebugFile(' TMapForm.DRGanaglyph1Click set up done'); {$EndIf}
   MapForm.AutoAnaglyphRedraw := false;
   DRGAnaglyphForm.Show;
   {$IfDef RecordAnaglyph} WriteLineToDebugFile(' TMapForm.DRGanaglyph1Click out'); {$EndIf}
end;


procedure Draw3Dmap(MapForm : tMapForm);
var
   Bitmap : tMyBitmap;
   LeftImage,RightImage : PathStr;
begin
    if MDDef.ShadeOpts in [soReflectance,soIntensity] then MapForm.MapDraw.TerrainShadowsDEM := MapForm.MapDraw.DEMonMap
    else MapForm.MapDraw.TerrainShadowsDEM := 0;
    MapForm.MapDraw.DeleteSingleMapLayer(MapForm.MapDraw.BaseMapFName);

    if ValidDEM(MapForm.MapDraw.DEMonMap) and (MapForm.MapDraw.MinMapElev = 0) and (MapForm.MapDraw.MaxMapElev = 0) then begin
       MapForm.MapDraw.ScaleMapElevationsToDEM;
    end;

    MapForm.DoBaseMapRedraw;

    if (MdDef.StereoMode in [smAnaglyph,smPair]) then begin
       CopyImageToBitmap(MapForm.Image1,Bitmap);
       Mapform.MapDraw.AnaglyphBitmap(RightImage,LeftImage,Bitmap);
       {$IfDef ExStereo}
          MapForm.Image1.Picture.Graphic := Bitmap;
       {$Else}
          if (MdDef.StereoMode in [smPair]) then Stereo_Viewer.ShowStereoPair(LeftImage,RightImage)
          else MapForm.Image1.Picture.Graphic := Bitmap;
       {$EndIf}
       FreeAndNil(Bitmap);
    end;
   {$IfDef RecordAnaglyph} WriteLineToDebugFile('add done map drawing'); {$EndIf}
end;

procedure TDRGAnaglyphForm.BitBtn1Click(Sender: TObject);
begin
   ChangeReflectanceOptions(MapOwner);
end;


procedure TDRGAnaglyphForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if NeedRedraw then Draw3Dmap(MapOwner);
  Free;
end;

procedure TDRGAnaglyphForm.FormCreate(Sender: TObject);
begin
   ClientWidth := 262;
   ClientHeight := 308;
   Petmar.PlaceFormAtMousePosition(Self);
   RadioGroup1.ItemIndex := ord(MdDef.StereoMode);
   RadioGroup2.ItemIndex := ord(MDDef.ShadeOpts);
   CheckBox3.Checked := MDDef.EWAnaglyphShift;
end;

procedure TDRGAnaglyphForm.CheckBox2Click(Sender: TObject);
begin
   If CheckBox2.Checked then begin
      GetDEM(MapOwner.MapDraw.TerrainShadowsDEM,false,'Elevations for 3D effect');
      CheckBox2.Caption := 'Elevations from ' + DEMGlb[MapOwner.MapDraw.TerrainShadowsDEM].AreaName;
   end
   else CheckBox2.Caption := 'Elevations from another DEM';
   NeedRedraw := true;
end;

procedure TDRGAnaglyphForm.CheckBox3Click(Sender: TObject);
begin
   MDDef.EWAnaglyphShift := CheckBox3.Checked;
   NeedRedraw := true;
end;

procedure TDRGAnaglyphForm.CheckBox5Click(Sender: TObject);
begin
   MapOwner.MapDraw.NoMapGrids := CheckBox5.Checked;
   NeedRedraw := true;
end;

procedure TDRGAnaglyphForm.Edit2Change(Sender: TObject);
begin
   CheckEditString(Edit2.Text,MDDef.MaxAnaglyphShift);
   NeedRedraw := true;
end;

procedure TDRGAnaglyphForm.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\drg_anaglyph.htm');
end;


procedure TDRGAnaglyphForm.OKBtnClick(Sender: TObject);
begin
   Close;
end;

procedure TDRGAnaglyphForm.RadioGroup1Click(Sender: TObject);
begin
   Edit2.Enabled := RadioGroup1.ItemIndex = 2;
   Label3.Enabled := RadioGroup1.ItemIndex = 2;
   CheckBox3.Enabled := RadioGroup1.ItemIndex = 2;
   MdDef.StereoMode := tStereoMode(RadioGroup1.ItemIndex);
   {$IfDef RecordAnaglyph} WriteLineToDebugFile('TDRGAnaglyphForm.RadioGroup1Click, stereo mode = ' + RadioGroup1.ItemIndex.ToString); {$EndIf}
   NeedRedraw := true;
end;


procedure TDRGAnaglyphForm.RadioGroup2Click(Sender: TObject);
begin
   MDDef.ShadeOpts := tShadeOpts(RadioGroup2.ItemIndex);
   BitBtn1.Enabled := RadioGroup2.ItemIndex in [1,2];
   NeedRedraw := true;
end;

procedure TDRGAnaglyphForm.RedrawSpeedButton12Click(Sender: TObject);
begin
   Draw3Dmap(MapOwner);
   NeedRedraw := false;
end;

initialization
finalization
   {$IfDef RecordAnaglyph} WriteLineToDebugFile('RecordAnaglyph active in drg_analgyph'); {$EndIf}
end.
