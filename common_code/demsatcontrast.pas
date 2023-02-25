unit demsatcontrast;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2023 Peter L. Guth  }
{___________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
    //{$Define RecordContrast}
    //{$Define RecordPickBand}
    //{$Define RecordPixelSize}
{$EndIf}


interface

uses
   Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls, Vcl.ComCtrls,
   Buttons, ExtCtrls, OkCancl1,
   DEMMapf,DEMDefs,BaseGraf,PETMAR;

type
  TEROSContrastForm = class(TOKBottomDlg)
    HelpBtn: TButton;
    Image1: TImage;
    CheckBox3: TCheckBox;
    CheckBox1: TCheckBox;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Contrast: TTabSheet;
    Label1: TLabel;
    Label4: TLabel;
    RadioGroup1: TRadioGroup;
    Edit1: TEdit;
    Edit4: TEdit;
    GroupBox2: TGroupBox;
    ComboBox4: TComboBox;
    UpDown1: TUpDown;
    GroupBox1: TGroupBox;
    Label5: TLabel;
    Label6: TLabel;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    ComboBox3: TComboBox;
    RadioGroup2: TRadioGroup;
    RedrawSpeedButton12: TSpeedButton;
    CheckBox2: TCheckBox;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    DupeMapSpeedButton18: TSpeedButton;
    Edit2: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    Edit3: TEdit;
    procedure HelpBtnClick(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure Edit3Change(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure ComboBox4Change(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure ComboBox3Change(Sender: TObject);
    procedure UpDown1Click(Sender: TObject; Button: TUDBtnType);
    procedure RadioGroup2Click(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure RedrawSpeedButton12Click(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure Edit4Change(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BitBtn2Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure DupeMapSpeedButton18Click(Sender: TObject);
  private
    { Private declarations }
    procedure ColorPreview;
    procedure ToggleBands;
    procedure ShowImageWithContrast;
  public
    { Public declarations }
    Sat : integer;
    WorkingNow,ReDrawNeeded : boolean;
    BaseMap : tMapForm;
    g1,g2 : tThisBaseGraph;
  end;


function GetContrast(inBaseMap : tMapForm) : TEROSContrastForm;


implementation

{$R *.DFM}

uses
   DEMEROS,
   DEMDef_routines,
   Petmar_types,
   PetImage,
   Nevadia_Main, basemap;


function GetContrast(inBaseMap : tMapForm) : TEROSContrastForm;
var
   i : integer;
begin
   {$IfDef RecordContrast} WriteLineToDebugFile('Enter GetContrast with contrast ' + IntToStr(ord(InBaseMap.MapDraw.Satview.WindowContrast)) + '  Bands=' + IntToStr(SatImage[InBaseMap.MapDraw.SatOnMap].NumBands)); {$EndIf}
   {$If Defined(RecordPickBand) or Defined(RecordPixelSize)}
      WriteLineToDebugFile('   projected limits,  ' + sfBoundBoxToString(InBaseMap.MapDraw.MapCorners.BoundBoxProj,1));
      WriteLineToDebugFile('   data grid limits,  ' + sfBoundBoxToString(InBaseMap.MapDraw.MapCorners.BoundBoxDataGrid,1));
      WriteLineToDebugFile('   geographic limits, ' + sfBoundBoxToString(InBaseMap.MapDraw.MapCorners.BoundBoxGeo,6));
   {$EndIf}

   Result := TEROSContrastForm.Create(Application);
   Result.Caption := 'Display for ' + SatImage[InBaseMap.MapDraw.SatOnMap].SceneBaseName;
   Result.BaseMap := inBaseMap;
   Result.Sat := InBaseMap.MapDraw.SatOnMap;

   Result.Edit1.Text := RealToString(Result.BaseMap.MapDraw.SatView.WindowContrastLowTailSize,-12,-2);
   Result.Edit4.Text := RealToString(Result.BaseMap.MapDraw.SatView.WindowContrastHighTailSize,-12,-2);
   Result.Edit3.Text := IntToStr(MDdef.MaxSatRange);
   Result.Edit2.Text := IntToStr(MDdef.MinSatRange);

   Result.CheckBox1.Checked := MDdef.IgnoreHistogramZero;
   Result.CheckBox3.Checked := MDDef.QuickMapRedraw;
   Result.RadioGroup1.ItemIndex := ord(Result.BaseMap.MapDraw.Satview.WindowContrast);
   Result.CheckBox2.Enabled := SatImage[Result.Sat].LandsatNumber in [7,8,9];

   if (SatImage[Result.Sat].NumBands > 1) then begin
      Result.PageControl1.ActivePage := Result.TabSheet1;
      with Result do begin
         UpDown1.Max := succ(SatImage[Result.Sat].NumBands);
         UpDown1.Position := BaseMap.MapDraw.SatView.BandInWindow;
         for i := 1 to SatImage[Result.Sat].NumBands do begin
            ComboBox1.Items.Add(SatImage[Result.Sat].BandLongName[i]);
            ComboBox2.Items.Add(SatImage[Result.Sat].BandLongName[i]);
            ComboBox3.Items.Add(SatImage[Result.Sat].BandLongName[i]);
            ComboBox4.Items.Add(SatImage[Result.Sat].BandLongName[i]);
         end;
         if IsSatelliteColorImage(Result.BaseMap.MapDraw.MapType) then begin
            ComboBox1.Text := SatImage[Result.Sat].BandLongName[BaseMap.MapDraw.SatView.RedBand];
            ComboBox2.Text := SatImage[Result.Sat].BandLongName[BaseMap.MapDraw.SatView.GreenBand];
            ComboBox3.Text := SatImage[Result.Sat].BandLongName[BaseMap.MapDraw.SatView.BlueBand];
            case Result.BaseMap.MapDraw.MapType of
               mtSatTrueColor  : RadioGroup2.ItemIndex := 1;
               mtSatFalseColor : RadioGroup2.ItemIndex := 2;
               mtSatPickColor  : RadioGroup2.ItemIndex := 3;
            end;
         end
         else RadioGroup2.ItemIndex := 0;
         ComboBox4.Text := SatImage[Result.Sat].BandLongName[BaseMap.MapDraw.SatView.BandInWindow];
      end;
      {$IfDef RecordContrast} WriteLineToDebugFile('GetContrast setup done'); {$EndIf}
   end
   else begin
      Result.TabSheet1.Visible := false;
      Result.PageControl1.ActivePage := Result.Contrast;
   end;
   Result.WorkingNow := true;
   Result.Show;
   {$IfDef RecordContrast} WriteLineToDebugFile('Exit GetContrast'); {$EndIf}
end;


procedure tEROSContrastForm.ToggleBands;
begin
   GroupBox2.Enabled := BaseMap.MapDraw.MapType in [mtSatImageGray];
   ComboBox4.Enabled := BaseMap.MapDraw.MapType in [mtSatImageGray];
   GroupBox1.Enabled := IsSatelliteColorImage(BaseMap.MapDraw.MapType);
   ComboBox1.Enabled := GroupBox1.Enabled;
   ComboBox2.Enabled := GroupBox1.Enabled;
   ComboBox3.Enabled := GroupBox1.Enabled;
   if BaseMap.MapDraw.MapType in [mtSatTrueColor] then begin
      ComboBox1.Text := SatImage[Sat].BandLongName[SatImage[Sat].DefRedTrue];
      ComboBox2.Text := SatImage[Sat].BandLongName[SatImage[Sat].DefGreenTrue];
      ComboBox3.Text := SatImage[Sat].BandLongName[SatImage[Sat].DefBlueTrue];
   end
   else if BaseMap.MapDraw.MapType in [mtSatFalseColor] then begin
      ComboBox1.Text := SatImage[Sat].BandLongName[SatImage[Sat].DefRedFalse];
      ComboBox2.Text := SatImage[Sat].BandLongName[SatImage[Sat].DefGreenFalse];
      ComboBox3.Text := SatImage[Sat].BandLongName[SatImage[Sat].DefBlueFalse];
   end;
   CheckBox2.Enabled := (BaseMap.MapDraw.MapType in [mtSatTrueColor,mtSatFalseColor,mtSatPickColor]) and (SatImage[BaseMap.MapDraw.SATonMap].LandsatNumber in [7,8,9]);
end;


procedure tEROSContrastForm.ColorPreview;
var
   i : integer;
begin
   if WorkingNow then begin
      {$If Defined(RecordPickBand) or Defined(RecordPixelSize)} WriteLineToDebugFile('TPickThreeBandForm.ColorPreview in pix ' + BaseMap.MapDraw.MapSizeString); {$EndIf}
      for i := 1 to SatImage[Sat].NumBands do begin
         if ComboBox1.Text = SatImage[Sat].BandLongName[i] then BaseMap.MapDraw.SatView.RedBand := i;
         if ComboBox2.Text = SatImage[Sat].BandLongName[i] then BaseMap.MapDraw.SatView.GreenBand := i;
         if ComboBox3.Text = SatImage[Sat].BandLongName[i] then BaseMap.MapDraw.SatView.BlueBand := i;
         if ComboBox4.Text = SatImage[Sat].BandLongName[i] then BaseMap.MapDraw.SatView.BandInWindow := i;
      end;
      ShowImageWithContrast;
      {$If Defined(RecordPickBand) or Defined(RecordPixelSize)} WriteLineToDebugFile('ColorPreview out ' + BaseMap.MapDraw.CurrentSatelliteColors + ' pixels=' + RealToString(BaseMap.MapDraw.ScreenPixelSize,-12,-2) + ' m'); {$EndIf}
   end;
end;


procedure tEROSContrastForm.ShowImageWithContrast;
begin
   if WorkingNow then begin
      {$If Defined(RecordContrast) or Defined(RecordPickBand) or Defined(RecordPixelSize)} WriteLineToDebugFile('tEROSContrastForm.ShowImageWithContrast in'); {$EndIf}
      BaseMap.MapDraw.SatView.WindowContrast := tContrastEnhancement(RadioGroup1.ItemIndex);
      MDdef.IgnoreHistogramZero := CheckBox1.Checked;
      CheckEditString(Edit1.Text,BaseMap.MapDraw.SatView.WindowContrastLowTailSize);
      CheckEditString(Edit4.Text,BaseMap.MapDraw.SatView.WindowContrastHighTailSize);

      //CheckEditString(Edit2.Text,MDdef.MaxSatRange);
      //CheckEditString(Edit3.Text,MDdef.MinSatRange);

      Edit1.Enabled := RadioGroup1.ItemIndex in [3];
      Edit4.Enabled := RadioGroup1.ItemIndex in [3,4];
      Label1.Enabled := RadioGroup1.ItemIndex in [3];
      Label4.Enabled := RadioGroup1.ItemIndex in [3,4];

      Edit2.Enabled := RadioGroup1.ItemIndex in [5];
      Edit3.Enabled := RadioGroup1.ItemIndex in [5];
      Label2.Enabled := RadioGroup1.ItemIndex in [5];
      Label3.Enabled := RadioGroup1.ItemIndex in [5];
      //GroupBox3.Visible := RadioGroup1.ItemIndex in [5];
      if (MDDef.QuickMapRedraw) then begin
         {$If Defined(RecordContrast) or Defined(RecordPickBand) or Defined(RecordPixelSize)} WriteLineToDebugFile('tEROSContrastForm.ShowImageWithContrast start redraw'); {$EndIf}
         BaseMap.DoBaseMapRedraw;
         RedrawNeeded := false;
      end;
      {$If Defined(RecordContrast) or Defined(RecordPickBand) or Defined(RecordPixelSize)} WriteLineToDebugFile('tEROSContrastForm.ShowImageWithContrast out'); {$EndIf}
   end;
end;


procedure TEROSContrastForm.UpDown1Click(Sender: TObject; Button: TUDBtnType);
begin
   {$IfDef RecordPickBand} WriteLineToDebugFile('TPickThreeBandForm.UpDown1Click in UpDown1.Position=' + IntToStr(UpDown1.Position) + '  UpDown1.Min=' + IntToStr(UpDown1.Min)+ '  UpDown1.Max=' + IntToStr(UpDown1.Max)); {$EndIf}
   if ComboBox4.Items[pred(UpDown1.Position)] <> '' then ComboBox4.Text := ComboBox4.Items[pred(UpDown1.Position)];
   ColorPreview;
end;

procedure TEROSContrastForm.HelpBtnClick(Sender: TObject);
begin
   if PageControl1.ActivePage = Contrast then DisplayHTMLTopic('html\set_contrast.htm')
   else DisplayHTMLTopic('html\tbme9dpq.htm');
end;


procedure TEROSContrastForm.OKBtnClick(Sender: TObject);
begin
   if RedrawNeeded then BaseMap.DoBaseMapRedraw;
   Close;
end;

procedure TEROSContrastForm.RadioGroup1Click(Sender: TObject);
begin
   BaseMap.MapDraw.SatView.WindowContrast := tContrastEnhancement(RadioGroup1.ItemIndex);
   ShowImageWithContrast;
end;


procedure TEROSContrastForm.RadioGroup2Click(Sender: TObject);
begin
   {$IfDef RecordPixelSize} WriteLineToDebugFile('TEROSContrastForm.RadioGroup2Click in pix size=' +  RealToString(BaseMap.MapDraw.ScreenPixelSize,-12,-2)); {$EndIf}
   case RadioGroup2.ItemIndex of
       0 : BaseMap.MapDraw.MapType := mtSatImageGray;
       1 : BaseMap.MapDraw.MapType := mtSatTrueColor;
       2 : BaseMap.MapDraw.MapType := mtSatFalseColor;
       3 : BaseMap.MapDraw.MapType := mtSatPickColor;
   end;
   ToggleBands;
   ColorPreview;
   {$IfDef RecordPixelSize} WriteLineToDebugFile('TEROSContrastForm.RadioGroup2Click out'); {$EndIf}
end;


procedure TEROSContrastForm.RedrawSpeedButton12Click(Sender: TObject);
begin
   ShowImageWithContrast;
   RedrawNeeded := false;
   if not (MDDef.QuickMapRedraw) then BaseMap.DoBaseMapRedraw;
end;


procedure TEROSContrastForm.BitBtn1Click(Sender: TObject);
begin
   {$IfDef NoDBGrafs}
   {$Else}
      if g1 <> Nil then g1.destroy;
      if g2 <> nil then g2.Destroy;
      g1 := SatImage[Sat].EnhancementGraph;
      if BaseMap.MapDraw.MapType = mtSatImageGray then g2 := SatImage[Sat].GraphHistogram(shOneBand,BaseMap.MapDraw.SatView.BandInWindow)
      else g2 := SatImage[Sat].GraphHistogram(shRGBBands,0);
   {$EndIf}
end;

procedure TEROSContrastForm.BitBtn2Click(Sender: TObject);
begin
   MDdef.ContrastEnhancement := BaseMap.MapDraw.Satview.WindowContrast;
   MDdef.LowTailSize := BaseMap.MapDraw.Satview.WindowContrastLowTailSize;
   MDdef.HighTailSize := BaseMap.MapDraw.Satview.WindowContrastHighTailSize;
end;

procedure TEROSContrastForm.CancelBtnClick(Sender: TObject);
begin
   Close;
end;

procedure TEROSContrastForm.CheckBox2Click(Sender: TObject);
begin
   {$If Defined(RecordPickBand) or Defined(RecordPixelSize)} WriteLineToDebugFile('TEROSContrastForm.CheckBox2Click in'); {$EndIf}
   BaseMap.MapDraw.SatView.PanSharpenImage := CheckBox2.Checked;
   RedrawNeeded := true;
   RadioGroup2Click(Sender);
   {$If Defined(RecordPickBand) or Defined(RecordPixelSize)} WriteLineToDebugFile('TEROSContrastForm.CheckBox2Click out'); {$EndIf}
end;

procedure TEROSContrastForm.CheckBox3Click(Sender: TObject);
begin
   MDDef.QuickMapRedraw := CheckBox3.Checked;
end;

procedure TEROSContrastForm.ComboBox1Change(Sender: TObject);
begin
  ColorPreview;
end;

procedure TEROSContrastForm.ComboBox2Change(Sender: TObject);
begin
  ColorPreview;
end;

procedure TEROSContrastForm.ComboBox3Change(Sender: TObject);
begin
  ColorPreview;
end;

procedure TEROSContrastForm.ComboBox4Change(Sender: TObject);
begin
  ColorPreview;
end;

procedure TEROSContrastForm.DupeMapSpeedButton18Click(Sender: TObject);
begin
   BaseMap.DuplicateMap(false);
end;

procedure TEROSContrastForm.Edit1Change(Sender: TObject);
begin
   ShowImageWithContrast;
end;


procedure TEROSContrastForm.Edit2Change(Sender: TObject);
begin
   CheckEditString(Edit2.Text,MDdef.MinSatRange);
end;

procedure TEROSContrastForm.Edit3Change(Sender: TObject);
begin
   CheckEditString(Edit3.Text,MDdef.MaxSatRange);
end;


procedure TEROSContrastForm.Edit4Change(Sender: TObject);
begin
   ShowImageWithContrast;
end;

procedure TEROSContrastForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   FreeAndNil(g1);
   FreeAndNil(g2);
end;

procedure TEROSContrastForm.FormCreate(Sender: TObject);
begin
   wmDEM.FormPlacementInCorner(self,lpCenterMap);
   WorkingNow := false;
   RedrawNeeded := false;
   g1 := Nil;
   g2 := nil;
end;


initialization
finalization
   {$IfDef RecordContrast} WriteLineToDebugFile('RecordContrast active in DemSatContrast'); {$EndIf}
   {$IfDef RecordPickBand} WriteLineToDebugFile('RecordPickBand active in DemSatContrast'); {$EndIf}
end.

