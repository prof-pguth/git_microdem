unit DEMGrPik;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch }
{   file verified 6/22/2011       }
{_________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}  //normally only defined for debugging specific problems
  //{$Define RecordGetGridOpts}
{$EndIf}


interface

uses
   Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
   Buttons, ExtCtrls, OkCancl2,
   PETMAR, Petmar_types,DEMMapf,Dialogs, Vcl.ComCtrls;

type
  TPickGrid = class(TOKRightDlg)
    RadioGroup1: TRadioGroup;
    CheckBox4: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox1: TCheckBox;
    Label7: TLabel;
    CheckBox7: TCheckBox;
    BitBtn7: TBitBtn;
    BitBtn8: TBitBtn;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    BitBtn4: TBitBtn;
    BitBtn1: TBitBtn;
    TabSheet2: TTabSheet;
    Button1: TButton;
    Label4: TLabel;
    BitBtn2: TBitBtn;
    BitBtn6: TBitBtn;
    TabSheet3: TTabSheet;
    Native: TBitBtn;
    Edit3: TEdit;
    Label5: TLabel;
    CheckBox6: TCheckBox;
    Label3: TLabel;
    GroupBox1: TGroupBox;
    Label2: TLabel;
    Edit2: TEdit;
    Edit1: TEdit;
    Label1: TLabel;
    GroupBox2: TGroupBox;
    BitBtn5: TBitBtn;
    RadioGroup2: TRadioGroup;
    BitBtn3: TBitBtn;
    HelpBtn: TButton;
    CheckBox2: TCheckBox;
    CheckBox5: TCheckBox;
    Label6: TLabel;
    Edit4: TEdit;
    procedure HelpBtnClick(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure NativeClick(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure CheckBox5Click(Sender: TObject);
    procedure BitBtn6Click(Sender: TObject);
    procedure Edit3Change(Sender: TObject);
    procedure CheckBox6Click(Sender: TObject);
    procedure CheckBox7Click(Sender: TObject);
    procedure BitBtn7Click(Sender: TObject);
    procedure BitBtn8Click(Sender: TObject);
    procedure Edit4Change(Sender: TObject);
  private
    { Private declarations }
     CreatingForm : boolean;
     procedure ColorButton;
     procedure RedrawGrid;
  public
    { Public declarations }
     MapForm : tMapForm;
  end;

function ChangeGridOptions(inMapForm : tMapForm) : boolean;


implementation

{$R *.DFM}


uses
  DEMdefs,
  DEMCoord,
  Map_Overlays,BaseMap;


function ChangeGridOptions(inMapForm : tMapForm) : boolean;
var
   PickGrid : TPickGrid;
begin
   PickGrid := tPickGrid.Create(Application);
   PickGrid.MapForm := inMapForm;
   if (PickGrid.MapForm <> Nil) then begin
       PickGrid.Edit1.Text := RealToString(PickGrid.MapForm.MapDraw.UTMTickInt,-8,0);
       PickGrid.Edit3.Text := RealToString(PickGrid.MapForm.MapDraw.NativeTickInt,-8,0);
       PickGrid.Edit4.Text := IntToStr(PickGrid.MapForm.MapDraw.PrimMapProj.projUTMZone);
       if abs(PickGrid.MapForm.MapDraw.LatTickInt) > 0.00001 then PickGrid.BitBtn5.Caption := 'Graticule, ' + AngleFormat(PickGrid.MapForm.MapDraw.LatTickInt,MDDef.GraticuleUnits);
       PickGrid.CheckBox3.Enabled := PickGrid.MapForm.MapDraw.NativeGridAllowedOnMap;
       PickGrid.Label5.Enabled := PickGrid.MapForm.MapDraw.NativeGridAllowedOnMap;
       PickGrid.Edit3.Enabled := PickGrid.MapForm.MapDraw.NativeGridAllowedOnMap;
       PickGrid.Native.Enabled := PickGrid.MapForm.MapDraw.NativeGridAllowedOnMap;
       PickGrid.Label7.Caption := 'Map pixel size: ' + SmartDistanceMetersFormat(PickGrid.MapForm.MapDraw.ScreenPixelSize);
   end;
   PickGrid.ColorButton;
   PickGrid.Native.Enabled := PickGrid.CheckBox3.Enabled;
   Result := (PickGrid.ShowModal <> idCancel);
   PickGrid.Free;
end;

procedure TPickGrid.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\tbme8cv8.htm');
end;


procedure TPickGrid.NativeClick(Sender: TObject);
begin
  PickLineSizeAndColor('Native grid',Native,MDdef.NativeGridColor,MDDef.NativeGridLineWidth);
  RedrawGrid;
end;

procedure TPickGrid.ColorButton;
begin
   if CreatingForm then exit;

   ColorLineWidthBitBtn(BitBtn1,MDdef.MapGridColor,MDdef.UTMGridLineWidth);
   ColorLineWidthBitBtn(BitBtn2,MDdef.SecondaryUTMColor,MDdef.UTMSecGridLineWidth);
   ColorLineWidthBitBtn(BitBtn6,MDdef.SecondaryGeoColor,MDdef.GeoSecGridLineWidth);
   ColorLineWidthBitBtn(BitBtn4,MDdef.MapLatLongGridColor,MDdef.MapLatLongLineWidth);
   ColorLineWidthBitBtn(Native,MDdef.NativeGridColor,MDDef.NativeGridLineWidth);
   MDdef.ShowPrimaryGrid := CheckBox1.Checked;
   MDdef.ShowSecondaryGrid := CheckBox4.Checked;

   Label1.Enabled := (MDDef.MapTicks in [tixBoth,tixUTM]);
   Label2.Enabled := (MDDef.MapTicks in [tixBoth,tixUTM]);
   Edit1.Enabled := (MDDef.MapTicks in [tixBoth,tixUTM]);
   Edit2.Enabled := (MDDef.MapTicks in [tixBoth,tixUTM]);

   BitBtn5.Enabled := (MDDef.MapTicks in [tixLatLong,tixBoth]);

   BitBtn1.Enabled := MDdef.ShowPrimaryGrid and (MDDef.MapTicks in [tixBoth,tixUTM]);
   BitBtn4.Enabled := MDdef.ShowPrimaryGrid and (MDDef.MapTicks in [tixLatLong,tixBoth]);
   Label3.Enabled := MDdef.ShowPrimaryGrid;

   Button1.Enabled := MDdef.ShowSecondaryGrid and (MDDef.MapTicks in [tixLatLong,tixUTM,tixBoth]);
   BitBtn2.Enabled := MDdef.ShowSecondaryGrid and (MDDef.MapTicks in [tixBoth,tixUTM]);
   BitBtn6.Enabled := MDdef.ShowSecondaryGrid and (MDDef.MapTicks in [tixLatLong,tixBoth]);
   Label4.Enabled := MDdef.ShowSecondaryGrid;
   RadioGroup2.Enabled := (MDDef.MapTicks in [tixLatLong,tixBoth]);
end;

procedure TPickGrid.Edit1Change(Sender: TObject);
begin
   CheckEditString(Edit1.Text,MapForm.MapDraw.UTMTickInt);
   MapForm.MapDraw.DeleteSingleMapLayer(MapForm.MapDraw.GridOverlayFName);
   if (MapForm <> Nil) and (MapForm.MapDraw.UTMTickInt > 10 * MapForm.MapDraw.ScreenPixelSize) then RedrawGrid;
end;

procedure TPickGrid.Edit2Change(Sender: TObject);
begin
   CheckEditString(Edit2.Text,MDDef.UTMGridMaxPixelSize);
   if (MapForm <> Nil) then MapForm.MapDraw.DeleteSingleMapLayer(MapForm.MapDraw.GridOverlayFName);
end;

procedure TPickGrid.Edit3Change(Sender: TObject);
begin
   CheckEditString(Edit3.Text,MapForm.MapDraw.NativeTickInt);
   MapForm.MapDraw.DeleteSingleMapLayer(MapForm.MapDraw.GridOverlayFName);
   if (MapForm <> Nil) and (MapForm.MapDraw.NativeTickInt > 10 * MapForm.MapDraw.ScreenPixelSize) then RedrawGrid;
end;

procedure TPickGrid.Edit4Change(Sender: TObject);
begin
   CheckEditString(Edit4.Text,MapForm.MapDraw.PrimMapProj.projUTMZone);
end;

procedure TPickGrid.BitBtn1Click(Sender: TObject);
begin
   {$IfDef ExPETMARUserInput}
   {$Else}
     PickLineSizeAndColor('Primary UTM grid',BitBtn1,MDdef.MapGridColor,MDdef.UTMGridLineWidth);
     RedrawGrid;
   {$EndIf}
end;

procedure TPickGrid.BitBtn2Click(Sender: TObject);
begin
   {$IfDef ExPETMARUserInput}
   {$Else}
      PickLineSizeAndColor('Secondary grid',BitBtn2,MDdef.SecondaryUTMColor,MDdef.UTMSecGridLineWidth);
      RedrawGrid;
   {$EndIf}
end;

procedure TPickGrid.BitBtn3Click(Sender: TObject);
begin
   MapForm.GridgraticluetoGoogleEarth1Click(Sender);
end;

procedure TPickGrid.BitBtn4Click(Sender: TObject);
begin
   {$IfDef ExPETMARUserInput}
   {$Else}
       PickLineSizeAndColor('Primary lat/long grid',BitBtn4,MDdef.MapLatLongGridColor,MDdef.MapLatLongLineWidth);
   {$EndIf}
   RedrawGrid;
end;

procedure TPickGrid.BitBtn5Click(Sender: TObject);
begin
  {$IfDef RecordGetGridOpts} WriteLineToDebugFile('TPickGrid.BitBtn5Click in, Grat units=' + RealToString(MapForm.MapDraw.LatTickInt,-12,-6)); {$EndIf}
  if (MapForm <> Nil) then begin
     GetAngle('Graticule spacing',MapForm.MapDraw.LatTickInt,MDDef.GraticuleUnits);
     if abs(MapForm.MapDraw.LatTickInt) > 0.00001 then
             BitBtn5.Caption := 'Graticule, ' + AngleFormat(MapForm.MapDraw.LatTickInt,MDDef.GraticuleUnits);
     {$IfDef RecordGetGridOpts} WriteLineToDebugFile('TPickGrid.BitBtn5Click out, Grat units=' + RealToString(MapForm.MapDraw.LatTickInt,-12,-6)); {$EndIf}
     RedrawGrid;
  end;
end;


procedure TPickGrid.BitBtn6Click(Sender: TObject);
begin
   {$IfDef ExPETMARUserInput}
   {$Else}
      PickLineSizeAndColor('Secondary grid',BitBtn6,MDdef.SecondaryGeoColor,MDdef.GeoSecGridLineWidth);
      RedrawGrid;
   {$EndIf}
end;

procedure TPickGrid.BitBtn7Click(Sender: TObject);
begin
   EditMyFont(MDDef.InsideGridFont);
   RedrawGrid;
end;

procedure TPickGrid.BitBtn8Click(Sender: TObject);
begin
   QueryColor(BitBtn8,MDDef.InsideMapGridColor);
   RedrawGrid;
end;

procedure TPickGrid.CheckBox2Click(Sender: TObject);
begin
   MDdef.KMLLabelGraticule := CheckBox2.Checked;
   if MDdef.KMLLabelGraticule then RedrawGrid;
end;

procedure TPickGrid.CheckBox3Click(Sender: TObject);
begin
   MDDef.ShowNativeGrid := CheckBox3.Checked;
   CheckBox6.Enabled := MDDef.ShowNativeGrid;
   Label5.Enabled := MDDef.ShowNativeGrid;
   Edit3.Enabled := MDDef.ShowNativeGrid;
   Native.Enabled := MDDef.ShowNativeGrid;
   RedrawGrid;
end;

procedure TPickGrid.CheckBox4Click(Sender: TObject);
begin
   ColorButton;
   RedrawGrid;
end;


procedure TPickGrid.CheckBox5Click(Sender: TObject);
begin
  MDDef.ShortKML_UTM := CheckBox5.Checked;
end;


procedure TPickGrid.CheckBox6Click(Sender: TObject);
begin
  MDDef.LabelNativeGrid := CheckBox6.Checked;
  RedrawGrid;

end;

procedure TPickGrid.CheckBox7Click(Sender: TObject);
begin
   MDDef.GridLabelsInsideMap := CheckBox7.Checked;
   RedrawGrid;
end;

procedure TPickGrid.Button1Click(Sender: TObject);
begin
   PickDatum('Secondary datum',MDdef.PreferSecondaryDatum);
   Label4.Caption := DatumName(MDdef.PreferSecondaryDatum);
   if (MapForm.MapDraw.SecondaryMapProj = Nil) then MapForm.MapDraw.SecondaryMapProj := tMapProjection.Create('Secondary projection in TPickGrid.RedrawGrid;');
   MapForm.MapDraw.SecondaryMapProj.DefineDatumFromUTMZone(MapForm.MapDraw.SecondaryMapProj.h_DatumCode,MapForm.MapDraw.SecondaryMapProj.projUTMZone,MapForm.MapDraw.SecondaryMapProj.LatHemi,'TPickGrid.RedrawGrid');
   RedrawGrid;
end;


procedure TPickGrid.FormCreate(Sender: TObject);
begin
    Petmar.PlaceFormAtMousePosition(Self);
    CreatingForm := true;
    RadioGroup1.ItemIndex := ord(MDDef.MapTicks);
    CheckBox1.Checked := MDDef.ShowPrimaryGrid;
    CheckBox3.Checked := MDDef.ShowNativeGrid;
    CheckBox2.Checked := MDdef.KMLLabelGraticule;
    CheckBox4.Checked := MDdef.ShowSecondaryGrid;
    CheckBox5.Checked := MDDef.ShortKML_UTM;
    CheckBox6.Checked := MDDef.LabelNativeGrid;
    CheckBox7.Checked := MDDef.GridLabelsInsideMap;

    Edit2.Text := IntToStr(MDDef.UTMGridMaxPixelSize);
    Label3.Caption := DatumName(MDDef.PreferPrimaryDatum);
    Label4.Caption := DatumName(MDDef.PreferSecondaryDatum);
    ColorBitBtn(BitBtn8,MDDef.InsideMapGridColor);

    if MDDef.LatLongGridTicks then RadioGroup2.ItemIndex := 1 else RadioGroup2.ItemIndex := 0;
    CreatingForm := false;
    ColorButton;
   {$IfDef HideHelpButtons} HelpBtn.Visible := false;{$EndIf}
end;


procedure TPickGrid.RadioGroup1Click(Sender: TObject);
begin
  MDDef.MapTicks := tMapTick(RadioGroup1.ItemIndex);
  ColorButton;
  RedrawGrid;
end;


procedure TPickGrid.RadioGroup2Click(Sender: TObject);
begin
   MDDef.LatLongGridTicks := RadioGroup2.ItemIndex = 1;
   RedrawGrid;
end;

procedure TPickGrid.RedrawGrid;
begin
   if (MapForm <> Nil) then begin
      AddOrSubtractOverlay(MapForm,ovoGrids,(MDDef.MapTicks <> tixNone) or (MDDef.ShowNativeGrid));
      MapForm.MapDraw.GridOverlayfName := '';
      MapForm.MapDraw.ResetMarginalia;
      MapForm.DoFastMapRedraw;
   end;
end;

initialization
finalization
   {$IfDef RecordGetGridOpts} WriteLineToDebugFile('RecordGetGridOpts active in DEMGrPik'); {$EndIf}
   {$IfDef RecordClosingProblems} WriteLineToDebugFile('Closing DEMgrPik'); {$EndIf}
end.

