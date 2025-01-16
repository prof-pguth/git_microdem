unit demslopeopts;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{________________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   {$Define RecordDrawMap}
{$EndIf}

interface

uses
   System.Diagnostics,
   Windows, Classes, Graphics, Forms, Controls, Buttons,
   StdCtrls, ExtCtrls, Dialogs,SysUtils,ComCtrls,
   DEMDefs,PETMar,PETMath,
   DEMMapf, DEMDef_routines;

type
  TSlopeOptForm = class(TForm)
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    HelpBtn: TBitBtn;
    Label3: TLabel;
    Image1: TImage;
    Button11: TButton;
    Label6: TLabel;
    Edit1: TEdit;
    Label25: TLabel;
    Label1: TLabel;
    Edit2: TEdit;
    CheckBox1: TCheckBox;
    RedrawSpeedButton12: TSpeedButton;
    RadioGroup2: TRadioGroup;
    RadioGroup3: TRadioGroup;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    RadioGroup1: TRadioGroup;
    TabSheet2: TTabSheet;
    RadioGroup4: TRadioGroup;
    BitBtn1: TBitBtn;
    ClipboardSpeedButton: TSpeedButton;
    BitBtn2: TBitBtn;
    procedure HelpBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure RedrawSpeedButton12Click(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure RadioGroup3Click(Sender: TObject);
    procedure RadioGroup4Click(Sender: TObject);
    procedure ClipboardSpeedButtonClick(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
      MapOwner : tMapForm;
      LastX,LastY : integer;
      SetupDone,NeedRedraw : boolean;
      procedure SetUpForm;
      procedure DrawPreview;
  end;


function ChangeSlopeMapOptions(aMapOwner : tMapForm) : boolean;

implementation

{$R *.DFM}

uses
   {$IfDef ExAdvancedGIS}
   {$Else}
      DEMSlpEd,
   {$EndIf}
   DEMCoord,Petmar_types,
   DEMMapDraw,
   Make_Grid,
   Grayscale_shift,
   PETImage;


function ChangeSlopeMapOptions(aMapOwner : tMapForm) : boolean;
var
   SlopeOptForm : TSlopeOptForm;
begin
   SlopeOptForm := TSlopeOptForm.Create(Application);
   SlopeOptForm.MapOwner := aMapOwner;
   SlopeOptForm.SetUpForm;
   Result := not (SlopeOptForm.ShowModal = idCancel);
   if (aMapOwner <> Nil) then begin
      if SlopeOptForm.NeedRedraw or (not MDdef.QuickMapRedraw) then SlopeOptForm.MapOwner.DoBaseMapRedraw;
   end;
   SlopeOptForm.Free;
end;


procedure TSlopeOptForm.SetUpForm;
begin
   if MapOwner.MapDraw.MapType = mtDEMaspect then PageControl1.ActivePage := TabSheet2
   else PageControl1.ActivePage := TabSheet2;
   case MDDef.DefSlopeMap of
      mtSlopeStandardCats : RadioGroup1.ItemIndex := 0;
      mtSlopeTrafficCats  : RadioGroup1.ItemIndex := 1;
      mtSlopeGrayScale    : RadioGroup1.ItemIndex := 2;
      mtSlopeGrayScaleReversed  : RadioGroup1.ItemIndex := 3;
      mtSlopeRainbow      : RadioGroup1.ItemIndex := 4;
      mtSlopePastel       : RadioGroup1.ItemIndex := 5;
      mtSlopeGoNoGo       : RadioGroup1.ItemIndex := 6;
   end;
   Label6.Caption := SlopeMethodName(MDDef.SlopeAlgorithm);
   Edit1.Text := IntToStr(MDdef.SlopeRegionRadius);
   Edit2.Text := IntToStr(MDdef.MaxSlopeOnMaps);
   Button11.Enabled := (MDDef.ProgramOption = ExpertProgram) or MDDef.ShowGeomorphometry;
   Edit1.Enabled := Button11.Enabled;
   CheckBox1.Checked := MDDef.QuickMapRedraw;
   RadioGroup2.ItemIndex := pred(MDDef.SlopeLSQorder);
   RadioGroup3.ItemIndex := pred(MDDef.SlopeLSQradius);
   if (MapOwner = nil) then begin
      CheckBox1.Visible := false;
      BitBtn1.Visible := false;
   end;
   DrawPreview;
   SetUpdone := true;
end;



procedure TSlopeOptForm.BitBtn2Click(Sender: TObject);
var
   DEM : integer;
begin
   DEM := CreateSlopeMapPercent(false,MapOwner.MapDraw.DEMonMap);
   DEMglb[DEM].AreaName := DEMglb[MapOwner.MapDraw.DEMonMap].AreaName + '_' + SlopeMethodName(MDDef.SlopeAlgorithm);
   CreateDEMSelectionMap(DEM,true,true,MDDef.DefSlopeMap);
end;

procedure TSlopeOptForm.Button11Click(Sender: TObject);
begin
   PickSlopeAspectMethod('',MDDef.SlopeAlgorithm);
   Label6.Caption := SlopeMethodName(MDDef.SlopeAlgorithm);
   RadioGroup2.Enabled := MDDef.SlopeAlgorithm = smLSQ;
   RadioGroup3.Enabled := MDDef.SlopeAlgorithm = smLSQ;
   DrawPreview;
end;

procedure TSlopeOptForm.CheckBox1Click(Sender: TObject);
begin
    MDdef.QuickMapRedraw := CheckBox1.Checked;
end;

procedure TSlopeOptForm.ClipboardSpeedButtonClick(Sender: TObject);
begin
   MapOwner.Quickmap1Click(Sender);
end;

procedure TSlopeOptForm.DrawPreview;
var
   xc,yc,xp,yp,SlopeVal : integer;
   Bitmap : tMyBitmap;
   p0 : prgb;
   SlopeAspectRec : tSlopeAspectRec;
   {$If Defined(RecordDrawMap)} MapStopwatch : TStopwatch; {$EndIf}
begin
   if (MapOwner <> Nil) and SetupDone then begin
       XC := round(MapOwner.MapDraw.MapCorners.BoundBoxDataGrid.xmin + MapOwner.MapDraw.MapCorners.BoundBoxDataGrid.xmax) div 2 - 50;
       YC := round(MapOwner.MapDraw.MapCorners.BoundBoxDataGrid.ymax + MapOwner.MapDraw.MapCorners.BoundBoxDataGrid.ymin) div 2 + 5;
       PetImage.CreateBitmap(Bitmap,100,100);
       MapOwner.MapDraw.DefineSlopeColors;
       for yp := 0 to 99 do begin
          p0 := Bitmap.ScanLine[yp];
          for xp := 0 to 99 do begin
             if DEMGlb[MapOwner.MapDraw.DEMonMap].GetSlopeAndAspect(xc+xp,yc-yp,SlopeAspectRec,false,false) then begin
                SlopeVal := round(SlopeAspectRec.SlopePercent);
                if (SlopeVal > 100) then SlopeVal := 100;
                p0[xp] := SlopeColorChoices[SlopeVal];
             end;
          end;
       end;
       Self.Image1.Picture.Graphic := Bitmap;
       Bitmap.Free;
       if MDdef.QuickMapRedraw then begin
          {$If Defined(RecordDrawMap)} MapStopwatch := TStopwatch.StartNew; {$EndIf}
          MapOwner.DoBaseMapRedraw;
          {$If Defined(RecordDrawMap)} WriteLineToDebugFile('Draw ' + SlopeMethodName(MDDef.SlopeAlgorithm)  + '  ' + RealToString(MapStopwatch.Elapsed.TotalSeconds,-12,-4) + ' sec'); {$EndIf}
       end;
       NeedRedraw := false;
   end;
end;


procedure TSlopeOptForm.Edit1Change(Sender: TObject);
begin
   CheckEditString(Edit1.Text,MDdef.SlopeRegionRadius);
   NeedRedraw := true;
end;


procedure TSlopeOptForm.Edit2Change(Sender: TObject);
begin
   CheckEditString(Edit2.Text,MDdef.MaxSlopeOnMaps);
   NeedRedraw := true;
end;


procedure TSlopeOptForm.FormCreate(Sender: TObject);
begin
   Petmar.PlaceFormAtMousePosition(Self);
   SetUpDone := false;
   {$IfDef ExAdvancedGIS} BitBtn1.Visible := false; {$Endif}
   {$IfDef HideHelpButtons} HelpBtn.Visible := false; {$EndIf}
end;


procedure TSlopeOptForm.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('images\slopmap_instructions.htm');
end;

procedure TSlopeOptForm.RadioGroup1Click(Sender: TObject);
begin
   case RadioGroup1.ItemIndex of
     0 : MDDef.DefSlopeMap := mtSlopeStandardCats;
     1 : MDDef.DefSlopeMap := mtSlopeTrafficCats;
     2 : MDDef.DefSlopeMap := mtSlopeGrayScale;
     3 : MDDef.DefSlopeMap := mtSlopeGrayScaleReversed;
     4 : MDDef.DefSlopeMap := mtSlopeRainbow;
     5 : MDDef.DefSlopeMap := mtSlopePastel;
     6 : MDDef.DefSlopeMap := mtSlopeGoNoGo;
   end;
   MapOwner.MapDraw.MapType := MDDef.DefSlopeMap;
   if (MapOwner <> Nil) then begin
     SetSlopedefaultColors(MDDef.NumSlopeBands,MapOwner.MapDraw.SlopeCut,MapOwner.MapDraw.SlopeColors);
     DrawPreview;
   end;
   BitBtn1.Enabled := RadioGroup1.ItemIndex in [0,3];
end;


procedure TSlopeOptForm.RadioGroup2Click(Sender: TObject);
begin
    MDDef.SlopeLSQorder := succ(RadioGroup2.ItemIndex);
    //if user wants 3rd or 4th order, it has to be 5x5 window
    if (MDDef.SlopeLSQorder in [3,4]) and (MDDef.SlopeLSQradius < 2) then RadioGroup3.ItemIndex := 1
    else DrawPreview;
end;

procedure TSlopeOptForm.RadioGroup3Click(Sender: TObject);
begin
   MDDef.SlopeLSQradius := succ(RadioGroup3.ItemIndex);
   //if user wants the 3x3 window, it has to be a 2d order
   if  (MDDef.SlopeLSQradius in [1]) and (MDDef.SlopeLSQorder in [3,4]) then RadioGroup2.ItemIndex := 0
   else DrawPreview;
end;

procedure TSlopeOptForm.RadioGroup4Click(Sender: TObject);
begin
   MDDef.AspectMapMode := RadioGroup4.ItemIndex;
   MapOwner.MapDraw.MapType := mtDEMAspect;
   DrawPreview;
end;

procedure TSlopeOptForm.RedrawSpeedButton12Click(Sender: TObject);
begin
   DrawPreview;
end;

procedure TSlopeOptForm.BitBtn1Click(Sender: TObject);
begin
   {$IfDef ExAdvancedGIS}
   {$Else}
   if (MDDef.DefSlopeMap = mtSlopeGrayScaleReversed) then begin
      SetGrayscale(SlopeGray,MapOwner);
   end
   else begin
      EditSlopeCategories(MapOwner.MapDraw.SlopeCut,MapOwner.MapDraw.SlopeColors);
      DrawPreview;
   end;
   {$EndIf}
end;




end.

