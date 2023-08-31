unit demelevops;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of ianMICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2023 Peter L. Guth   }
{____________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   //{$Define RecordElevColorMapChanges}
{$EndIf}


interface

uses
  Windows, SysUtils, Classes, Graphics, Forms,
  Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Controls, Vcl.Buttons, OKCANCL2,
  Petmar_types,Petmar_db,PETMAR,DEMDEFs,DEMMapf, DB;

type
  TElevOptionsForm = class(TOKRightDlg)
    HelpBtn: TButton;
    RadioGroup1: TRadioGroup;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    Button1: TButton;
    BitBtn1: TBitBtn;
    Image1: TImage;
    ComboBox1: TComboBox;
    Button2: TButton;
    RadioGroup3: TRadioGroup;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    RedrawSpeedButton12: TSpeedButton;
    CheckBox6: TCheckBox;
    ComboBox2: TComboBox;
    CheckBox1: TCheckBox;
    procedure Button2Click(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Edit1Change(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure RadioGroup3Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure CheckBox5Click(Sender: TObject);
    procedure RedrawSpeedButton12Click(Sender: TObject);
    procedure CheckBox6Click(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
  private
    { Private declarations }
    procedure DrawPreview;
    function SetMapType : tMapType;
  public
    { Public declarations }
    ihsHue,
    ihsSat,
    ihsInt : SmallInt;
    SettingUp,
    ihsUseRef : boolean;
    MapOwner : tMapForm;
    Table : tMyData;
  end;



procedure ChangeElevationMap(var Int,Hue,Sat : SmallInt; aMapOwner : tMapForm);


implementation

{$R *.DFM}

uses
   {$IfDef ExSats}
   {$Else}
   DEMSatMerge,
   {$EndIf}
   PetImage,
   Make_Tables,
   DEMCoord,
   Petmath,
   Elev_color_range,
   PetDButils;



procedure ChangeElevationMap;
var
  ElevOptionsForm: TElevOptionsForm;
begin
   {$IfDef RecordElevColorMapChanges} WriteLineToDebugFile('ChangeElevationMap in'); {$EndIf}
   ElevOptionsForm := TElevOptionsForm.Create(Application);
   ElevOptionsForm.SettingUp := true;
   ElevOptionsForm.MapOwner := aMapOwner;
   ElevOptionsForm.Caption := 'Elevation options: ' + DEMGlb[ElevOptionsForm.MapOwner.MapDraw.DEMonMap].AreaName;
   ColorBitBtn(ElevOptionsForm.BitBtn1,MDdef.MissingDataColor);
   ElevOptionsForm.RadioGroup3.ItemIndex := MDDef.MonochromeColor;
   FillComboBoxWithColorPalettes(ColorBrewerName,ElevOptionsForm.ComboBox1);
   if (MDDef.ElevPalName = '') then ElevOptionsForm.ComboBox1.Text := ElevOptionsForm.ComboBox1.Items[0]
   else ElevOptionsForm.ComboBox1.Text := MDDef.ElevPalName;
   FillComboBoxWithColorPalettes(HardLimitColorPaletteFName,ElevOptionsForm.ComboBox2);
   if (ElevationFixedPalette = '') then ElevOptionsForm.ComboBox2.Text := ElevOptionsForm.ComboBox2.Items[0]
   else ElevOptionsForm.ComboBox2.Text := ElevationFixedPalette;
   ElevOptionsForm.Button2.Visible := (ElevOptionsForm.MapOwner <> Nil);

   ElevOptionsForm.CheckBox1.Checked := MDDef.NoDEMInterpolations;
   ElevOptionsForm.CheckBox2.Checked := MDdef.WaterCheck;
   ElevOptionsForm.CheckBox3.Checked := MDdef.LakeCheck;
   ElevOptionsForm.CheckBox4.Checked := MDdef.InvertGrayScale;
   ElevOptionsForm.CheckBox5.Checked := ElevOptionsForm.MapOwner.MapDraw.Log10Elev;
   ElevOptionsForm.ihsHue := Hue;
   ElevOptionsForm.ihsSat := Sat;
   ElevOptionsForm.ihsInt := Int;
   ElevOptionsForm.SettingUp := false;

   if ElevOptionsForm.MapOwner.MapDraw.MapType = mtElevTerrain then ElevOptionsForm.RadioGroup1.ItemIndex := 0
   else if ElevOptionsForm.MapOwner.MapDraw.MapType = mtElevGray    then ElevOptionsForm.RadioGroup1.ItemIndex := 1
   else if ElevOptionsForm.MapOwner.MapDraw.MapType = mtElevBands   then ElevOptionsForm.RadioGroup1.ItemIndex := 2
   else if ElevOptionsForm.MapOwner.MapDraw.MapType = mtElevIHS     then ElevOptionsForm.RadioGroup1.ItemIndex := 3
   else if ElevOptionsForm.MapOwner.MapDraw.MapType = mtElevLandSea then ElevOptionsForm.RadioGroup1.ItemIndex := 4
   else if ElevOptionsForm.MapOwner.MapDraw.MapType = mtElevSpectrum then ElevOptionsForm.RadioGroup1.ItemIndex := 5
   else if ElevOptionsForm.MapOwner.MapDraw.MapType = mtElevRainbow  then ElevOptionsForm.RadioGroup1.ItemIndex := 6
   else if ElevOptionsForm.MapOwner.MapDraw.MapType = mtElevFromTable then ElevOptionsForm.RadioGroup1.ItemIndex := 7
   else if ElevOptionsForm.MapOwner.MapDraw.MapType = mtElevDefinedPalette then ElevOptionsForm.RadioGroup1.ItemIndex := 8
   else if ElevOptionsForm.MapOwner.MapDraw.MapType = mtElevContrast then ElevOptionsForm.RadioGroup1.ItemIndex := 9
   else if ElevOptionsForm.MapOwner.MapDraw.MapType = mtElevGrayReversed then ElevOptionsForm.RadioGroup1.ItemIndex := 10
   else begin
      ElevOptionsForm.RadioGroup1.ItemIndex := 1;
      ElevOptionsForm.MapOwner.DoFastMapRedraw;
   end;
   ElevOptionsForm.Show;
   {$IfDef RecordElevColorMapChanges} WriteLineToDebugFile('ChangeElevationMap out'); {$EndIf}
end;



procedure TElevOptionsForm.DrawPreview;
var
   xc,yc,x,y : integer;
   Bitmap : tMyBitmap;
   BMPmem : tBMPMemory;
begin
   {$IfDef RecordElevColorMapChanges} WriteLineToDebugFile('TElevOptionsForm.DrawPreview in'); {$EndIf}
   if (MapOwner <> Nil) then begin
       XC := round(MapOwner.MapDraw.MapCorners.BoundBoxDataGrid.xmin + MapOwner.MapDraw.MapCorners.BoundBoxDataGrid.xmax) div 2 - 50;
       YC := round(MapOwner.MapDraw.MapCorners.BoundBoxDataGrid.ymax + MapOwner.MapDraw.MapCorners.BoundBoxDataGrid.ymin) div 2 + 5;

       PetImage.CreateBitmap(Bitmap,100,100);
       BMPmem := tBMPMemory.Create(Bitmap);
       MapOwner.MapDraw.SetUpElevationColorTable;
       for y := 0 to 99 do begin
          for x := 0 to 99 do begin
             BMPmem.SetPixelColorSize(x,99-y,1,MapOwner.MapDraw.GetElevColor(xc+x,yc+y));
          end;
       end;
       BMPMem.Destroy;
       Self.Image1.Picture.Graphic := Bitmap;
       Bitmap.Free;

       if MDdef.QuickMapRedraw then RedrawSpeedButton12Click(nil)
       else MapOwner.MapDraw.NeedToRedraw := true;
   end;
   {$IfDef RecordElevColorMapChanges} WriteLineToDebugFile('TElevOptionsForm.DrawPreview out'); {$EndIf}
end;


procedure TElevOptionsForm.Edit1Change(Sender: TObject);
begin
   SetMapType;
end;

function TElevOptionsForm.SetMapType : tMapType;
begin
   {$IfDef RecordElevColorMapChanges} WriteLineToDebugFile('TElevOptionsForm.SetMapType in'); {$EndIf}
   if not SettingUp then begin
      case RadioGroup1.ItemIndex of
         0 : Result := mtElevTerrain;
         1 : Result := mtElevGray;
         2 : Result := mtElevBands;
         3 : Result := mtElevIHS;
         4 : Result := mtElevLandSea;
         5 : Result := mtElevSpectrum;
         6 : Result := mtElevRainbow;
         7 : Result := mtElevFromTable;
         8 : Result := mtElevDefinedPalette;
         9 : Result := mtElevContrast;
         10 : Result := mtElevGrayReversed;
      end;
      CheckBox2.Enabled := RadioGroup1.ItemIndex <> 0;
      CheckBox3.Enabled := RadioGroup1.ItemIndex <> 0;
      CheckBox4.Enabled := RadioGroup1.ItemIndex = 2;
      RadioGroup3.Enabled := RadioGroup1.ItemIndex = 1;
      Button1.Enabled := RadioGroup1.ItemIndex = 5;
      MapOwner.MapDraw.MapType := result;
      DrawPreview;
   end;
  {$IfDef RecordElevColorMapChanges} WriteLineToDebugFile('TElevOptionsForm.SetMapType out'); {$EndIf}
end;



procedure TElevOptionsForm.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\tbme78s3.htm');
end;


procedure TElevOptionsForm.OKBtnClick(Sender: TObject);
begin
  //inherited;
   {$IfDef RecordElevColorMapChanges} WriteLineToDebugFile('TElevOptionsForm.OKBtnClick'); {$EndIf}
   Close;
end;

procedure TElevOptionsForm.RadioGroup1Click(Sender: TObject);
begin
   CheckBox2.Enabled := RadioGroup1.ItemIndex <> 0;
   CheckBox3.Enabled := RadioGroup1.ItemIndex <> 0;
   Button1.Enabled := RadioGroup1.ItemIndex = 5;
   ComboBox2.Enabled := RadioGroup1.ItemIndex in [7];
   ComboBox1.Enabled := RadioGroup1.ItemIndex in [8];
   RadioGroup3.Enabled := RadioGroup1.ItemIndex = 1;
   if (RadioGroup1.ItemIndex = 2) then ReadDefault('Contour interval (integer)',MapOwner.MapDraw.MapOverlays.ConInt);
   MapOwner.MapDraw.MapType := SetMapType;
end;


procedure TElevOptionsForm.RadioGroup3Click(Sender: TObject);
begin
   MDDef.MonochromeColor := RadioGroup3.ItemIndex;
   SetMapType;
   if MDdef.QuickMapRedraw then RedrawSpeedButton12Click(nil);
end;

procedure TElevOptionsForm.RedrawSpeedButton12Click(Sender: TObject);
begin
   if (not SettingUp) then begin
      MapOwner.DoBaseMapRedraw;
      MapOwner.MapDraw.NeedToRedraw := false;
   end;
end;

procedure TElevOptionsForm.Button1Click(Sender: TObject);
begin
   {$IfDef ExSats}
   {$Else}
      GetIHSparameters(ihsInt,ihsHue,ihsSat,ihsUseRef);
   {$EndIf}
end;


procedure TElevOptionsForm.BitBtn1Click(Sender: TObject);
begin
   QueryColor(BitBtn1,MDdef.MissingDataColor);
end;


procedure TElevOptionsForm.BitBtn4Click(Sender: TObject);
begin
   SetMapType;
end;

procedure TElevOptionsForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   if (Table <> Nil) then Table.Destroy;
end;


procedure TElevOptionsForm.FormCreate(Sender: TObject);
begin
  //Width := 320;
   Petmar.PlaceFormAtMousePosition(Self);
   //CheckBox1.Checked := MDDef.UseBigElevationColorTables;
   CheckBox6.Checked := MDDef.QuickMapRedraw;
   {$IfDef HideHelpButtons} HelpBtn.Visible := false; {$EndIf}
end;


procedure TElevOptionsForm.CancelBtnClick(Sender: TObject);
begin
   MapOwner.MapDraw.NeedToRedraw := false;
   Close;
end;

procedure TElevOptionsForm.CheckBox1Click(Sender: TObject);
begin
   MDDef.NoDEMInterpolations := CheckBox1.Checked;
   SetMapType;
end;

procedure TElevOptionsForm.CheckBox4Click(Sender: TObject);
begin
   MDdef.InvertGrayScale := CheckBox4.Checked;
   SetMapType;
end;

procedure TElevOptionsForm.CheckBox5Click(Sender: TObject);
begin
   MapOwner.MapDraw.Log10Elev := CheckBox5.Checked;
   if MapOwner.MapDraw.Log10Elev then begin
     if (DEMGlb[MapOwner.MapDraw.DEMonMap].DEMheader.MinElev <= 0) then MapOwner.MapDraw.MinMapElev := PetMath.Log10(0.01)
     else MapOwner.MapDraw.MinMapElev := PetMath.Log10(DEMGlb[MapOwner.MapDraw.DEMonMap].DEMheader.MinElev);
     MapOwner.MapDraw.MaxMapElev := PetMath.Log10(DEMGlb[MapOwner.MapDraw.DEMonMap].DEMheader.MaxElev);
   end
   else begin
      MapOwner.MapDraw.MinMapElev := DEMGlb[MapOwner.MapDraw.DEMonMap].DEMheader.MinElev;
      MapOwner.MapDraw.MaxMapElev := DEMGlb[MapOwner.MapDraw.DEMonMap].DEMheader.MaxElev;
   end;
   SetMapType;
end;

procedure TElevOptionsForm.CheckBox6Click(Sender: TObject);
begin
  MDdef.QuickMapRedraw := CheckBox6.Checked;
end;

procedure TElevOptionsForm.ComboBox1Change(Sender: TObject);
begin
   MDDef.ElevPalName := ComboBox1.Text;
   SetMapType;
end;

procedure TElevOptionsForm.ComboBox2Change(Sender: TObject);
begin
   ElevationFixedPalette := ComboBox2.Text;
   SetMapType;
end;

procedure TElevOptionsForm.Button2Click(Sender: TObject);
begin
   PickMapElevationRangeForColoring(MapOwner);
   SetMapType;
end;


initialization
finalization
   {$IfDef RecordElevColorMapChanges} WriteLineToDebugFile('RecordElevColorMapChanges active in demelevops'); {$EndIf}
end.

