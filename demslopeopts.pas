unit demslopeopts;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch }
{   file verified 6/22/2011       }
{_________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
{$EndIf}

interface

uses
   DEMDefs,PETMar,PETMath,
   Windows, Classes, Graphics, Forms, Controls, Buttons,
   StdCtrls, ExtCtrls, Dialogs,SysUtils,
   DEMMapf, DEMDef_routines,ComCtrls;

type
  TSlopeOptForm = class(TForm)
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    HelpBtn: TBitBtn;
    Label3: TLabel;
    RadioGroup1: TRadioGroup;
    Image1: TImage;
    BitBtn1: TBitBtn;
    Button11: TButton;
    Label6: TLabel;
    Edit1: TEdit;
    Label25: TLabel;
    Label1: TLabel;
    Edit2: TEdit;
    CheckBox1: TCheckBox;
    procedure HelpBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
      MapOwner : tMapForm;
      LastX,LastY : integer;
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
   Grayscale_shift,
   PETImage;


function ChangeSlopeMapOptions(aMapOwner : tMapForm) : boolean;
var
   SlopeOptForm : TSlopeOptForm;
begin
   SlopeOptForm := TSlopeOptForm.Create(Application);
   SlopeOptForm.MapOwner := aMapOwner;
   SlopeOptForm.SetUpForm;
   if (SlopeOptForm.ShowModal = idCancel) then
       Result := false
   else begin
      Result := true;
   end;
   SlopeOptForm.Free;
end;


procedure TSlopeOptForm.Button11Click(Sender: TObject);
begin
   PickSlopeAspectMethod('',MDdef.SlopeAlg);
   Label6.Caption := SlopeMethodName(MDdef.SlopeAlg);
end;

procedure TSlopeOptForm.CheckBox1Click(Sender: TObject);
begin
    MDdef.QuickMapRedraw := CheckBox1.Checked;
end;

procedure TSlopeOptForm.DrawPreview;
var
   xc,yc,xp,yp,SlopeVal : integer;
   Bitmap : tMyBitmap;
   p0 : prgb;
   SlopeAspectRec : tSlopeAspectRec;
begin
   if (MapOwner <> Nil) then begin
       MapOwner.MapDraw.MapType := MDDef.DefSlopeMap;
       XC := round(MapOwner.MapDraw.MapCorners.BoundBoxDataGrid.xmin + MapOwner.MapDraw.MapCorners.BoundBoxDataGrid.xmax) div 2 - 50;
       YC := round(MapOwner.MapDraw.MapCorners.BoundBoxDataGrid.ymax + MapOwner.MapDraw.MapCorners.BoundBoxDataGrid.ymin) div 2 + 5;
       PetImage.CreateBitmap(Bitmap,100,100);
       MapOwner.MapDraw.DefineSlopeColors;
       for yp := 0 to 99 do begin
          p0 := Bitmap.ScanLine[yp];
          for xp := 0 to 99 do begin
             if DEMGlb[MapOwner.MapDraw.DEMonMap].GetSlopeAndAspect(xc+xp,yc-yp,SlopeAspectRec) then begin
                SlopeVal := round(SlopeAspectRec.SlopePercent);
                if (SlopeVal > 100) then SlopeVal := 100;
                p0[xp] := SlopeColorChoices[SlopeVal];
             end;
          end;
       end;
       Self.Image1.Picture.Graphic := Bitmap;
       Bitmap.Free;
       if MDdef.QuickMapRedraw then MapOwner.DoBaseMapRedraw;
   end;
end;


procedure TSlopeOptForm.Edit1Change(Sender: TObject);
begin
   CheckEditString(Edit1.Text,MDdef.SlopeRegionRadius);
end;


procedure TSlopeOptForm.Edit2Change(Sender: TObject);
begin
   CheckEditString(Edit2.Text,MDdef.MaxSlopeOnMaps);
end;


procedure TSlopeOptForm.SetUpForm;
begin
   case MDDef.DefSlopeMap of
      mtSlopeStandardCats : RadioGroup1.ItemIndex := 0;
      mtSlopeTrafficCats  : RadioGroup1.ItemIndex := 1;
      mtSlopeGrayScale    : RadioGroup1.ItemIndex := 2;
      mtSlopeGrayScaleReversed    : RadioGroup1.ItemIndex := 3;
      mtSlopeRainbow      : RadioGroup1.ItemIndex := 4;
      mtSlopePastel       : RadioGroup1.ItemIndex := 5;
      mtSlopeGoNoGo       : RadioGroup1.ItemIndex := 6;
   end;
   Label6.Caption := SlopeMethodName(MDdef.SlopeAlg);
   Edit1.Text := IntToStr(MDdef.SlopeRegionRadius);
   Edit2.Text := IntToStr(MDdef.MaxSlopeOnMaps);
   Button11.Enabled := (MDDef.ProgramOption = ExpertProgram) or MDDef.ShowGeomorphometry;
   Edit1.Enabled := Button11.Enabled;
end;


procedure TSlopeOptForm.FormCreate(Sender: TObject);
begin
   Petmar.PlaceFormAtMousePosition(Self);
   CheckBox1.Checked := MDDef.QuickMapRedraw;
   {$IfDef ExAdvancedGIS}  BitBtn1.Visible := false; {$Endif}
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

   if MapOwner <> Nil then begin
     SetSlopedefaultColors(MDDef.NumSlopeBands,MapOwner.MapDraw.SlopeCut,MapOwner.MapDraw.SlopeColors);
     DrawPreview;
   end;
   BitBtn1.Enabled := RadioGroup1.ItemIndex in [0,3];
end;


procedure TSlopeOptForm.BitBtn1Click(Sender: TObject);
begin
   {$IfDef ExAdvancedGIS}
   {$Else}
   if MDDef.DefSlopeMap = mtSlopeGrayScaleReversed then begin
      SetGrayscale(SlopeGray,MapOwner);
   end
   else begin
      EditSlopeCategories(MapOwner.MapDraw.SlopeCut,MapOwner.MapDraw.SlopeColors);
      DrawPreview;
   end;
   {$EndIf}
end;




end.

