unit lidar_multiple_grid_display;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2023 Peter L. Guth  }
{___________________________________}


{$I nevadia_defines.inc}


{$IfDef RecordProblems}   //normally only defined for debugging specific problems
{$EndIf}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Buttons;

type
  TLidarMultipleDisplayForm = class(TForm)
    RadioGroup1: TRadioGroup;
    RadioGroup2: TRadioGroup;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    RadioGroup3: TRadioGroup;
    RadioGroup4: TRadioGroup;
    Label1: TLabel;
    Edit1: TEdit;
    CheckBox3: TCheckBox;
    RedrawSpeedButton12: TSpeedButton;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    BitBtn5: TBitBtn;
    BitBtn6: TBitBtn;
    BitBtn7: TBitBtn;
    BitBtn8: TBitBtn;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    procedure RadioGroup1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure RadioGroup4Click(Sender: TObject);
    procedure RadioGroup3Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure RedrawSpeedButton12Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure BitBtn6Click(Sender: TObject);
    procedure BitBtn7Click(Sender: TObject);
    procedure BitBtn8Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CheckBox5Click(Sender: TObject);
    procedure CheckBox6Click(Sender: TObject);
  private
    { Private declarations }
     OpenForBusiness : boolean;
     procedure RedrawMap;
     procedure EnableControls;
     procedure SetShadows;
     procedure SetElevColoring;
     procedure IterateRadioGroup(Horizontal : boolean; rg : tRadioGroup);
  public
    { Public declarations }
     lmgonmap : integer;
     procedure SetGrids;
  end;


implementation

{$R *.dfm}

uses
   Petmar,
   Petmar_types,
   PetImage,
   DEMDefs,
   DEMmapf,
   DEMcoord,
   las_lidar,
   Grayscale_shift,
   Nevadia_Main,
   MultiGrid;


procedure TLidarMultipleDisplayForm.RedrawMap;
begin
   if ValidLMG(lmgonmap) and OpenForBusiness then begin
      EnableControls;
      lmg[lmgonmap].MapOwner.MapDraw.MakeMapGrayscale := CheckBox1.Checked and (lmg[lmgonmap].Mapowner.MapDraw.DEMonMap in [lmg[lmgonmap].ClassGrid,lmg[lmgonmap].RGBgrid,lmg[lmgonmap].NIRgrid]);
      lmg[lmgonmap].MapOwner.SetMultibandToShowOnMap(0);
   end;
end;


procedure TLidarMultipleDisplayForm.RedrawSpeedButton12Click(Sender: TObject);
begin
   if ValidLMG(lmgonmap) then begin
      RedrawMap;
   end;
end;

procedure TLidarMultipleDisplayForm.BitBtn1Click(Sender: TObject);
begin
   if ValidLMG(lmgonmap) then begin
      SetGrayscale(OpennessGray,lmg[lmgonmap].Mapowner);
   end;
end;

procedure TLidarMultipleDisplayForm.BitBtn2Click(Sender: TObject);
begin
   if ValidLMG(lmgonmap) then begin
      SetGrayscale(SlopeGray,lmg[lmgonmap].Mapowner);
   end;
end;

procedure TLidarMultipleDisplayForm.BitBtn3Click(Sender: TObject);
begin
   if ValidLMG(lmgonmap) then begin
      SetGrayscale(ChangeGreen,lmg[lmgonmap].Mapowner);
   end;
end;

procedure TLidarMultipleDisplayForm.BitBtn4Click(Sender: TObject);
begin
   if ValidLMG(lmgonmap) then begin
      SetGrayscale(ChangeRed,lmg[lmgonmap].Mapowner);
   end;
end;


procedure TLidarMultipleDisplayForm.IterateRadioGroup(Horizontal : boolean; rg : tRadioGroup);
var
   i,BandSize : integer;
   bmp,composite : tMyBitmap;
   arect : tRect;
begin
   CopyImageToBitmap(lmg[lmgonmap].MapOwner.Image1,Composite);
   if Horizontal then BandSize := Composite.Height div rg.Items.Count
   else BandSize := Composite.Width div rg.Items.Count;
   for i := 0 to Pred(rg.Items.Count) do begin
      rg.ItemIndex := i;
      CopyImageToBitmap(lmg[lmgonmap].MapOwner.Image1,bmp);
      if Horizontal then aRect := Rect(0,i * BandSize,pred(bmp.width),succ(i) * BandSize)
      else aRect := Rect(i * BandSize,0,succ(i) * BandSize,pred(bmp.Height));
      Composite.Canvas.CopyRect(arect,bmp.Canvas,aRect);
      Bmp.Free;
   end;
   lmg[lmgonmap].MapOwner.Image1.Picture.Graphic := Composite;
   Composite.Free;
end;

procedure TLidarMultipleDisplayForm.BitBtn5Click(Sender: TObject);
begin
   IterateRadioGroup(true,RadioGroup1);
end;

procedure TLidarMultipleDisplayForm.BitBtn6Click(Sender: TObject);
begin
   IterateRadioGroup(false,RadioGroup1);
end;

procedure TLidarMultipleDisplayForm.BitBtn7Click(Sender: TObject);
begin
   IterateRadioGroup(true,RadioGroup2);
end;

procedure TLidarMultipleDisplayForm.BitBtn8Click(Sender: TObject);
begin
   IterateRadioGroup(false,RadioGroup2);
end;

procedure TLidarMultipleDisplayForm.CheckBox1Click(Sender: TObject);
begin
   if ValidLMG(lmgonmap) then begin
      RedrawMap;
   end;
end;

procedure TLidarMultipleDisplayForm.CheckBox2Click(Sender: TObject);
begin
   if ValidLMG(lmgonmap) then begin
      lmg[lmgonmap].MapOwner.MapDraw.SubdueBase := CheckBox2.Checked;
      RedrawMap;
   end;
end;

procedure TLidarMultipleDisplayForm.CheckBox3Click(Sender: TObject);
begin
   MDdef.ColorizeInPlace := CheckBox3.Checked;
   if ValidLMG(lmgonmap) then begin
      RedrawMap;
   end;
end;

procedure TLidarMultipleDisplayForm.CheckBox4Click(Sender: TObject);
begin
   SetGrids;
end;

procedure TLidarMultipleDisplayForm.CheckBox5Click(Sender: TObject);
begin
   SetElevColoring;
end;

procedure TLidarMultipleDisplayForm.CheckBox6Click(Sender: TObject);
begin
   SetGrids;
end;

procedure TLidarMultipleDisplayForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   if ValidLMG(lmgonmap) then begin
      lmg[lmgonmap].MapOwner.Close;
      lmg[lmgonmap].Destroy;
   end;
end;

procedure TLidarMultipleDisplayForm.FormCreate(Sender: TObject);
begin
   OpenForBusiness := false;
   lmgonmap := 0;
   if MDDef.DefLidarElevMap = mtIHSReflect then RadioGroup2.ItemIndex := 0;
   if MDDef.DefLidarElevMap = mtGrayReflect then RadioGroup2.ItemIndex := 1;
   if MDDef.DefLidarElevMap = mtSlopeGrayScaleReversed then RadioGroup2.ItemIndex := 2;
   if MDDef.DefLidarElevMap = mtOpenness then RadioGroup2.ItemIndex := 3;
   if (MDDef.SecondGridOpacity > 80) then MDDef.SecondGridOpacity := 50;
   Edit1.Text := IntToStr(MDDef.SecondGridOpacity);
   CheckBox3.Checked := MDdef.ColorizeInPlace;
   wmDEM.FormPlacementInCorner(self);
   InitializeLASColors;
   Show;
   OpenForBusiness := true;
end;


procedure TLidarMultipleDisplayForm.Edit1Change(Sender: TObject);
begin
   if ValidLMG(lmgonmap) then begin
      lmg[lmgonmap].MapOwner.MapDraw.DeleteSingleMapLayer(lmg[lmgonmap].MapOwner.MapDraw.SecondGridfName);
   end;
   CheckEditString(Edit1.Text, MDDef.SecondGridOpacity);
end;


procedure TLidarMultipleDisplayForm.SetElevColoring;
begin
   RadioGroup2.Items.Clear;
   RadioGroup2.Items.Add('Grayscale reflectance');
   RadioGroup2.Items.Add('Slope reverse grayscale');
   RadioGroup2.Items.Add('Openness');
   if (RadioGroup3.ItemIndex = 0) and CheckBox5.Checked then RadioGroup2.Items.Add('IHS reflectance');
end;

procedure TLidarMultipleDisplayForm.SetShadows;
begin
   RadioGroup4.Items.Clear;
   RadioGroup4.Items.Add('DSM');
   if CheckBox4.Checked then begin
      RadioGroup4.Items.Add('DTM');
      RadioGroup4.Items.Add('NVS');
   end;
end;


procedure TLidarMultipleDisplayForm.SetGrids;
begin
   RadioGroup1.Items.Clear;
   if lmg[lmgonmap].DSMgrid > 0 then RadioGroup1.Items.Add('DSM');
   if CheckBox4.Checked then begin
      if lmg[lmgonmap].DTMgrid > 0 then RadioGroup1.Items.Add('DTM');
      if lmg[lmgonmap].NVSgrid > 0 then RadioGroup1.Items.Add('NVS');
   end;
   if lmg[lmgonmap].IntensityGrid > 0 then RadioGroup1.Items.Add('LIDAR_MAX_INTENSITY');
   if lmg[lmgonmap].RGBgrid > 0 then RadioGroup1.Items.Add('RGB');
   if lmg[lmgonmap].NIRgrid > 0 then RadioGroup1.Items.Add('NIR');
   if (RadioGroup3.ItemIndex <> 1) and CheckBox6.Checked and (lmg[lmgonmap].ClassGrid > 0) then RadioGroup1.Items.Add('CLASSIFICATION');
   if (lmg[lmgonmap].CHMGrid > 0) and CheckBox6.Checked then RadioGroup1.Items.Add('CHM');
   if (lmg[lmgonmap].ChangeGrid > 0) and CheckBox6.Checked then RadioGroup1.Items.Add('CHANGE');

   RadioGroup3.Items.Clear;
   RadioGroup3.Items.Add('None');
   if (lmg[lmgonmap].ClassGrid > 0) then begin
      RadioGroup3.Items.Add('CLASSIFICATION');
      RadioGroup3.Items.Add('Ground Only');
   end;
   if (lmg[lmgonmap].CHMGrid > 0) then RadioGroup3.Items.Add('CHM');
   if (lmg[lmgonmap].ChangeGrid > 0) then RadioGroup3.Items.Add('CHANGE');
end;


procedure TLidarMultipleDisplayForm.EnableControls;
begin
   RadioGroup2.Enabled := lmg[lmgonmap].Mapowner.MapDraw.DEMonMap in [lmg[lmgonmap].DSMgrid,lmg[lmgonmap].DTMgrid,lmg[lmgonmap].NVSgrid];
   RadioGroup4.Enabled := lmg[lmgonmap].Mapowner.MapDraw.DEMonMap in [lmg[lmgonmap].ClassGrid,{lmg[lmgonmap].IntensityGrid,}lmg[lmgonmap].RGBGrid,lmg[lmgonmap].NIRGrid];
   BitBtn1.Enabled := RadioGroup2.Enabled and (MDDef.DefLidarElevMap = mtOpenness);
   BitBtn2.Enabled := RadioGroup2.Enabled and (MDDef.DefLidarElevMap = mtSlopeGrayScaleReversed);
   BitBtn7.Enabled := RadioGroup2.Enabled;
   BitBtn8.Enabled := RadioGroup2.Enabled;
end;

procedure TLidarMultipleDisplayForm.RadioGroup1Click(Sender: TObject);
var
  Code : integer;
begin
   if ValidLMG(lmgonmap) then begin
      if RadioGroup1.Items[RadioGroup1.ItemIndex] = 'DSM' then Code := 0;
      if RadioGroup1.Items[RadioGroup1.ItemIndex] = 'DTM' then Code := 1;
      if RadioGroup1.Items[RadioGroup1.ItemIndex] = 'NVS' then Code := 2;
      if RadioGroup1.Items[RadioGroup1.ItemIndex] = 'LIDAR_MAX_INTENSITY' then Code := 3;
      if RadioGroup1.Items[RadioGroup1.ItemIndex] = 'CLASSIFICATION' then Code := 4;
      if RadioGroup1.Items[RadioGroup1.ItemIndex] = 'RGB' then Code := 5;
      if RadioGroup1.Items[RadioGroup1.ItemIndex] = 'NIR' then Code := 6;
      if RadioGroup1.Items[RadioGroup1.ItemIndex] = 'CHM' then Code := 7;
      if RadioGroup1.Items[RadioGroup1.ItemIndex] = 'CHANGE' then Code := 8;

      case Code of
         0 : lmg[lmgonmap].Mapowner.MapDraw.DEMonMap := lmg[lmgonmap].DSMgrid;
         1 : lmg[lmgonmap].Mapowner.MapDraw.DEMonMap := lmg[lmgonmap].DTMgrid;
         2 : lmg[lmgonmap].Mapowner.MapDraw.DEMonMap := lmg[lmgonmap].NVSgrid;
         3 : lmg[lmgonmap].Mapowner.MapDraw.DEMonMap := lmg[lmgonmap].IntensityGrid;
         4 : lmg[lmgonmap].Mapowner.MapDraw.DEMonMap := lmg[lmgonmap].ClassGrid;
         5 : if (lmg[lmgonmap].RGBgrid = 0) then exit else lmg[lmgonmap].Mapowner.MapDraw.DEMonMap := lmg[lmgonmap].RGBgrid;
         6 : if (lmg[lmgonmap].NIRgrid = 0) then exit else lmg[lmgonmap].Mapowner.MapDraw.DEMonMap := lmg[lmgonmap].NIRgrid;
         7 : lmg[lmgonmap].Mapowner.MapDraw.DEMonMap := lmg[lmgonmap].CHMGrid;
         8 : lmg[lmgonmap].Mapowner.MapDraw.DEMonMap := lmg[lmgonmap].ChangeGrid;
      end;

      case Code of
         0,1,2 : lmg[lmgonmap].Mapowner.MapDraw.MapType := MDDef.DefLidarElevMap;
         3 : lmg[lmgonmap].Mapowner.MapDraw.MapType := mtElevGray;
         4 : lmg[lmgonmap].Mapowner.MapDraw.MapType := mtLASclass;
         5,6 : lmg[lmgonmap].Mapowner.MapDraw.MapType := mtRGBimagery;
         7 : lmg[lmgonmap].Mapowner.MapDraw.MapType := mtGrayReflect;
         8 : lmg[lmgonmap].Mapowner.MapDraw.MapType := mtGGRReflect;
      end;
      EnableControls;
      RadioGroup3Click(Nil);   //which also redraws map
   end;

end;


procedure TLidarMultipleDisplayForm.RadioGroup2Click(Sender: TObject);
begin
   if ValidLMG(lmgonmap) then begin
      case RadioGroup2.ItemIndex of
         0 : MDDef.DefLidarElevMap := mtGrayReflect;
         1 : MDDef.DefLidarElevMap := mtSlopeGrayScaleReversed;
         2 : MDDef.DefLidarElevMap := mtOpenness;
         3 : MDDef.DefLidarElevMap := mtIHSReflect;
      end;
      lmg[lmgonmap].Mapowner.MapDraw.MapType := MDDef.DefLidarElevMap;
      RedrawMap;
   end;
end;


procedure TLidarMultipleDisplayForm.RadioGroup3Click(Sender: TObject);
var
   Option : shortstring;
begin
   if ValidLMG(lmgonmap) then begin
      if RadioGroup3.ItemIndex < 0 then RadioGroup3.ItemIndex := 0;

      lmg[lmgonmap].MapOwner.MapDraw.DeleteSingleMapLayer(lmg[lmgonmap].MapOwner.MapDraw.SecondGridfName);
      lmg[lmgonmap].Mapowner.MapDraw.DEM2onMap := 0;
      lmg[lmgonmap].Mapowner.MapDraw.CHMGrid := 0;
      lmg[lmgonmap].Mapowner.MapDraw.ChangeGrid := 0;
      lmg[lmgonmap].Mapowner.MapDraw.LASclassGrid := 0;
      Option := UpperCase(RadioGroup3.Items[RadioGroup3.ItemIndex]);
      if (Option = 'CLASSIFICATION') or (Option = 'GROUND ONLY') then begin
         InitializeLASColors;
         lmg[lmgonmap].Mapowner.MapDraw.DEM2onMap := lmg[lmgonmap].classgrid;
         lmg[lmgonmap].Mapowner.MapDraw.GroundOnly := (Option = 'GROUND ONLY') ;
         lmg[lmgonmap].Mapowner.MapDraw.LASclassGrid := lmg[lmgonmap].Classgrid;
      end
      else if (Option = 'CHM') then begin
         lmg[lmgonmap].Mapowner.MapDraw.DEM2onMap := lmg[lmgonmap].CHMgrid;
         lmg[lmgonmap].Mapowner.MapDraw.CHMGrid := lmg[lmgonmap].CHMgrid;
      end
      else if (Option = 'CHANGE') then begin
         lmg[lmgonmap].Mapowner.MapDraw.DEM2onMap := lmg[lmgonmap].ChangeGrid;
         lmg[lmgonmap].Mapowner.MapDraw.ChangeGrid := lmg[lmgonmap].ChangeGrid;
      end;
      SetElevColoring;
      RedrawMap;
   end;
end;

procedure TLidarMultipleDisplayForm.RadioGroup4Click(Sender: TObject);
begin
   if ValidLMG(lmgonmap) then begin
       MDDef.ShadeOpts := soReflectance;
       case RadioGroup4.ItemIndex of
          0 : lmg[lmgonmap].MapOwner.MapDraw.TerrainShadowsDEM := 0;
          1 : lmg[lmgonmap].MapOwner.MapDraw.TerrainShadowsDEM := lmg[lmgonmap].DSMgrid;
          2 : lmg[lmgonmap].Mapowner.MapDraw.TerrainShadowsDEM := lmg[lmgonmap].DTMgrid;
          3 : lmg[lmgonmap].Mapowner.MapDraw.TerrainShadowsDEM := lmg[lmgonmap].NVSgrid;
       end;
      RedrawMap;
   end;
end;


initialization
finalization
end.
