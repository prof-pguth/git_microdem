unit demrefop;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch }
{   file verified 6/22/2011       }
{_________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   {$Define RecordRefOp}
{$EndIf}


interface

uses
   DEMDefs,PETMar,PETMath,
   Windows, Classes, Graphics, Forms, Controls, Buttons,  StrUtils,
   StdCtrls, ExtCtrls, Dialogs,
   Petmar_types,DEMMapf, ComCtrls;

type
  TRefOptFM = class(TForm)
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    HelpBtn: TBitBtn;
    Bevel1: TBevel;
    Label1: TLabel;
    Label2: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    Edit3: TEdit;
    Button1: TButton;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    BitBtn1: TBitBtn;
    RadioGroup1: TRadioGroup;
    Image1: TImage;
    Image2: TImage;
    UpDown1: TUpDown;
    TrackBar1: TTrackBar;
    Label5: TLabel;
    BitBtn2: TBitBtn;
    RedrawSpeedButton12: TSpeedButton;
    RadioGroup2: TRadioGroup;
    CheckBox5: TCheckBox;
    Label6: TLabel;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    procedure Edit3Change(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure Image2DblClick(Sender: TObject);
    procedure Image2MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
    procedure UpDown1Click(Sender: TObject; Button: TUDBtnType);
    procedure TrackBar1Change(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure RedrawSpeedButton12Click(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure CheckBox5Click(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure Edit4Change(Sender: TObject);
    procedure Edit5Change(Sender: TObject);
    procedure Edit6Change(Sender: TObject);
  private
    { Private declarations }
      procedure SetNewOptions;
  public
    { Public declarations }
      MapOwner : tMapForm;
      LastX,LastY : integer;
      Changed,
      SetupOver,
      CanChange : boolean;
      procedure SetUpForm;
      procedure DrawPreview;
      procedure DrawSunOrientation;
  end;

procedure ChangeReflectanceOptions(aMapOwner : tMapForm);


implementation

{$R *.DFM}

uses
   {$IfDef ExGeology}
   {$Else}
      Petmar_geology,
   {$EndIf}
   DEMCoord,Demdef_routines,Elev_color_range, PETImage;



procedure ChangeReflectanceOptions(aMapOwner : tMapForm);   // : boolean;
var
   RefOptFm : TRefOptFm;
begin
   aMapOwner.MapDraw.MapMerge := mmNone;
   if (not IsReflectanceMap(aMapOwner.MapDraw.MapType)) then begin
      aMapOwner.MapDraw.MapType := MDDef.DefRefMap;
      aMapOwner.DoBaseMapRedraw;
   end;

   RefOptFm := TRefOptFm.Create(Application);
   DEMDef_routines.SaveBackupDefaults;
   RefOptFm.MapOwner := aMapOwner;
   RefOptFm.UpDown1.Position := round(DEMGlb[RefOptFm.MapOwner.MapDraw.DEMonMap].RefVertExag);
   RefOptFm.Edit3.Text := RealToString(DEMGlb[RefOptFm.MapOwner.MapDraw.DEMonMap].RefVertExag,-8,-2);
   RefOptFm.SetUpForm;
   RefOptFm.SetupOver := true;
   RefOptFm.ShowModal;
   RefOptFm.SetNewOptions;
   if RefOptFm.Changed and (not MDdef.QuickMapRedraw) then RefOptFm.MapOwner.DoBaseMapRedraw;
   RefOptFm.Free;
end;


procedure TRefOptFM.DrawPreview;
var
   xc,yc,xp,yp : integer;
   Bitmap : tMyBitmap;
   p0 : prgb;
begin
   {$IfDef RecordRefOp} WriteLineToDebugFile('TRefOptFM.DrawPreview in'); {$EndIf}
   SetNewOptions;
   if (MapOwner <> Nil) then begin
       XC := round(MapOwner.MapDraw.MapCorners.BoundBoxDataGrid.xmin + MapOwner.MapDraw.MapCorners.BoundBoxDataGrid.xmax) div 2 - 50;
       YC := round(MapOwner.MapDraw.MapCorners.BoundBoxDataGrid.ymax + MapOwner.MapDraw.MapCorners.BoundBoxDataGrid.ymin) div 2 + 5;
       PetImage.CreateBitmap(Bitmap,100,100);
       DEMGlb[MapOwner.MapDraw.DEMonMap].ReflectanceParams(MapOwner.MapDraw.MinMapElev,MapOwner.MapDraw.MaxMapElev);
       MapOwner.MapDraw.SetUpElevationColorTable;
       for yp := 0 to 99 do begin
          p0 := Bitmap.ScanLine[yp];
          for xp := 0 to 99 do begin
             p0[xp]:= DEMGlb[MapOwner.MapDraw.DEMonMap].RGBReflectanceColor(MapOwner.MapDraw.MapType,xc+xp,yc-yp);
          end;
       end;
       Self.Image1.Picture.Graphic := Bitmap;
       Bitmap.Free;
       if MDDef.QuickMapRedraw then begin
          if SetUpOver then MapOwner.DoBaseMapRedraw;
          Changed := false;
       end
       else begin
          if CanChange then Changed := true;
       end;
   end;
   DrawSunOrientation;
   CheckBox2.Enabled := (RadioGroup1.ItemIndex = 0);
   CheckBox3.Enabled := (RadioGroup1.ItemIndex = 0);
   TrackBar1.Enabled := (RadioGroup1.ItemIndex = 3);
   Label5.Enabled := (RadioGroup1.ItemIndex = 3);
   BitBtn2.Enabled := (RadioGroup1.ItemIndex > 0);
   BitBtn1.Enabled := CheckBox2.Checked or CheckBox3.Checked;
   {$IfDef RecordRefOp} WriteLineToDebugFile('TRefOptFM.DrawPreview out'); {$EndIf}
end;


procedure TRefOptFM.SetNewOptions;
begin
   if (MapOwner <> Nil) and (MapOwner.MapDraw.DEMonMap <> 0) then begin
      CheckEditString(Edit3.Text,MDDef.RefVertExagLargeScaleDEM);
      DEMGlb[MapOwner.MapDraw.DEMonMap].RefVertExag := MDDef.RefVertExagLargeScaleDEM;
   end;
end;


procedure TRefOptFM.DrawSunOrientation;
var
   Bitmap : tMyBitmap;
   rad : float64;
   x,y : integer;
begin
    PetImage.CreateBitmap(Bitmap,100,100);
    Bitmap.Canvas.Ellipse(0,0,99,99);
    Rad := ((90-MDDef.RefTheta)/90)*50;
    x := 50 + round(Rad * sinDeg(MDDef.RefPhi));
    y := 50 - round(Rad * cosDeg(MDDef.RefPhi));
    Petmar.ScreenSymbol(Bitmap.Canvas,x,y,FilledCircle,3,ConvertTColorToPlatformColor(clRed));
    Image2.Picture.Graphic := Bitmap;
    Bitmap.Free;
end;

procedure TRefOptFM.Image2DblClick(Sender: TObject);
var
   Dist : float64;
begin
   {$IfDef RecordRefOp} WriteLineToDebugFile('TRefOptFM.Image2DblClick'); {$EndIf}
   MDdef.RefPhi := PetMath.HeadingOfLine(LastX-49,49-LastY);
   Dist := sqrt(sqr(LastX-49) + sqr(LastY-49));
   if (Dist > 50) then Dist := 50;
   MDDef.RefTheta := 90 - Dist * 90/50;
   CanChange := false;
   Edit1.Text := RealToString(MDdef.RefPhi,-8,-2);
   Edit2.Text := RealToString(MDdef.RefTheta,-8,-2);
   CanChange := true;
   DrawPreview;
end;


procedure TRefOptFM.SetUpForm;
begin
   {$IfDef RecordRefOp} WriteLineToDebugFile('TRefOptFM.SetUpForm in'); {$EndIf}
   CheckBox2.Checked := MDdef.WaterCheck;
   CheckBox3.Checked := MDdef.LakeCheck;
   BitBtn1.Enabled := CheckBox2.Checked or CheckBox3.Checked;
   Label3.Caption := AzimuthToDirection(round(MDdef.RefPhi));

   case MDDef.DefRefMap of
      mtGrayReflect : RadioGroup1.ItemIndex := 0;
      mtIHSReflect: RadioGroup1.ItemIndex := 1;
      mt6ColorsReflect : RadioGroup1.ItemIndex := 2;
      mtBlueGreenReflect : RadioGroup1.ItemIndex := 3;
      mtRefGrayColor : RadioGroup1.ItemIndex := 4;
      mtRefColorGray : RadioGroup1.ItemIndex := 5;
      mtRefGrayBlue : RadioGroup1.ItemIndex := 6;
      mtGYRReflect : RadioGroup1.ItemIndex := 7;
      mtGGRReflect : RadioGroup1.ItemIndex := 8;
      mtGrCyBlReflect : RadioGroup1.ItemIndex := 9;
   end;
   ColorBitBtn(BitBtn1,MDdef.WaterColor);
   DrawPreview;
   {$IfDef RecordRefOp} WriteLineToDebugFile('TRefOptFM.SetUpForm out'); {$EndIf}
end;

procedure TRefOptFM.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\tbme9u5v.htm');
end;

procedure TRefOptFM.Edit1Change(Sender: TObject);
begin
   if CanChange then begin
      CheckEditString(Edit1.Text,MDdef.RefPhi);
      {$IfDef RecordRefOp} WriteLineToDebugFile('TRefOptFM.Edit1Change'); {$EndIf}
      DrawPreview;
   end;
end;

procedure TRefOptFM.Edit2Change(Sender: TObject);
begin
   if CanChange then begin
      CheckEditString(Edit2.Text,MDdef.RefTheta);
      {$IfDef RecordRefOp} WriteLineToDebugFile('TRefOptFM.Edit2Change'); {$EndIf}
      DrawPreview;
   end;
end;

procedure TRefOptFM.Button1Click(Sender: TObject);
begin
   SetReflectanceDefaults;
   FormCreate(nil);
end;

procedure TRefOptFM.BitBtn1Click(Sender: TObject);
begin
   QueryColor(BitBtn1,MDdef.WaterColor);
end;

procedure TRefOptFM.FormCreate(Sender: TObject);
begin
   {$IfDef HideHelpButtons} HelpBtn.Visible := false; {$EndIf}
   Petmar.PlaceFormAtMousePosition(Self);
   CanChange := false;
   Changed := false;
   TrackBar1.Position := MDDEF.MergeSat;
   Edit1.Text := RealToString(MDdef.RefPhi,-8,-2);
   Edit2.Text := RealToString(MDdef.RefTheta,-8,-2);
   Edit4.Text := RealToString(MDDef.CurrentSeaLevel,-8,-2);
   Edit5.Text := RealToString(MDDef.BottomCutLevel,-8,-2);
   Edit6.Text := RealToString(MDDef.TopCutLevel,-8,-2);
   RadioGroup2.ItemIndex := pred(MDdef.UseRefDirs);
   CheckBox5.Checked := MDdef.QuickMapRedraw;
   CanChange := true;
   SetUpForm;
   SetupOver := false;
end;

procedure TRefOptFM.RadioGroup1Click(Sender: TObject);
begin
   {$IfDef RecordRefOp} WriteLineToDebugFile('TRefOptFM.RadioGroup1Click'); {$EndIf}
   case RadioGroup1.ItemIndex of
      0 : MDDef.DefRefMap := mtGrayReflect;
      1 : MDDef.DefRefMap := mtIHSReflect;
      2 : MDDef.DefRefMap := mt6ColorsReflect;
      3 : MDDef.DefRefMap := mtBlueGreenReflect;
      4 : MDDef.DefRefMap := mtRefGrayColor;
      5 : MDDef.DefRefMap := mtRefColorGray;
      6 : MDDef.DefRefMap := mtRefGrayBlue;
      7 : MDDef.DefRefMap := mtGYRReflect;
      8 : MDDef.DefRefMap := mtGGRReflect;
      9 : MDDef.DefRefMap := mtGrCyBlReflect;
   end;

   if (MapOwner <> Nil) then begin
      MapOwner.MapDraw.MapType := MDDef.DefRefMap;
      DrawPreview;
   end;
end;


procedure TRefOptFM.RadioGroup2Click(Sender: TObject);
begin
   MDdef.UseRefDirs := succ(RadioGroup2.ItemIndex);
   DrawPreview;
end;

procedure TRefOptFM.RedrawSpeedButton12Click(Sender: TObject);
begin
   MapOwner.DoBaseMapRedraw;
end;

procedure TRefOptFM.Image2MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
begin
   LastX := x;
   LastY := y;
end;

procedure TRefOptFM.OKBtnClick(Sender: TObject);
begin
  close;
end;

procedure TRefOptFM.UpDown1Click(Sender: TObject; Button: TUDBtnType);
begin
   {$IfDef RecordRefOp} WriteLineToDebugFile('TRefOptFM.UpDown1Click'); {$EndIf}
   if (Button = btNext) then DEMGlb[MapOwner.MapDraw.DEMonMap].RefVertExag := DEMGlb[MapOwner.MapDraw.DEMonMap].RefVertExag + 1;
   if (Button = btPrev) and (DEMGlb[MapOwner.MapDraw.DEMonMap].RefVertExag > 1.5) then DEMGlb[MapOwner.MapDraw.DEMonMap].RefVertExag := DEMGlb[MapOwner.MapDraw.DEMonMap].RefVertExag - 1;
   Edit3.Text := RealToString(DEMGlb[MapOwner.MapDraw.DEMonMap].RefVertExag,-8,-2);
end;

procedure TRefOptFM.TrackBar1Change(Sender: TObject);
begin
   MDDEF.MergeSat := TrackBar1.Position;
   SetUpForm;
end;

procedure TRefOptFM.BitBtn2Click(Sender: TObject);
begin
   Elev_color_range.PickMapElevationRangeForColoring(MapOwner);
   SetUpForm;
end;

procedure TRefOptFM.CancelBtnClick(Sender: TObject);
begin
   DEMDef_routines.RestoreBackupDefaults;
end;

procedure TRefOptFM.CheckBox2Click(Sender: TObject);
begin
   MDdef.WaterCheck := CheckBox2.Checked;
   DrawPreview;
end;

procedure TRefOptFM.CheckBox3Click(Sender: TObject);
begin
   MDdef.LakeCheck := CheckBox3.Checked;
   DrawPreview;
end;


procedure TRefOptFM.CheckBox5Click(Sender: TObject);
begin
   MDdef.QuickMapRedraw := CheckBox5.Checked;
end;

procedure TRefOptFM.Edit3Change(Sender: TObject);
begin
   if (Sender <> UpDown1) then begin
      CheckEditString(Edit3.Text,MDDef.RefVertExagLargeScaleDEM);
      DEMGlb[MapOwner.MapDraw.DEMonMap].RefVertExag := MDDef.RefVertExagLargeScaleDEM;
   end;
   DrawPreview;
end;

procedure TRefOptFM.Edit4Change(Sender: TObject);
begin
   CheckEditString(Edit4.Text,MDDef.CurrentSeaLevel);
end;

procedure TRefOptFM.Edit5Change(Sender: TObject);
begin
   CheckEditString(Edit5.Text,MDDef.TopCutLevel);
end;

procedure TRefOptFM.Edit6Change(Sender: TObject);
begin
   CheckEditString(Edit6.Text,MDDef.BottomCutLevel);
end;

initialization
finalization
   {$IfDef RecordRefOp} WriteLineToDebugFile('RecordRefOp active in demrefop'); {$EndIf}
   {$IfDef RecordClosingProblems} WriteLineToDebugFile('Closing demrefop'); {$EndIf}
end.

