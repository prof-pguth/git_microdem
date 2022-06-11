unit map_options;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch }
{_________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
{$EndIf}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,  Buttons,  //ExtCtrls,
  DEMMapF,DEMMapDraw, Vcl.ExtCtrls;

type
  TTMapOptsForm = class(TForm)
    RadioGroup1: TRadioGroup;
    HelpBtn: TBitBtn;
    OKBtn: TBitBtn;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    BitBtn5: TBitBtn;
    GroupBox1: TGroupBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    GroupBox2: TGroupBox;
    CheckBox3: TCheckBox;
    Checkbox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox6: TCheckBox;
    GroupBox3: TGroupBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    GroupBox4: TGroupBox;
    CheckBox9: TCheckBox;
    CheckBox10: TCheckBox;
    RedrawSpeedButton12: TSpeedButton;
    CheckBox11: TCheckBox;
    procedure HelpBtnClick(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RedrawSpeedButton12Click(Sender: TObject);
    procedure CheckBox11Click(Sender: TObject);
  private
    { Private declarations }
    theMapowner : tMapForm;
  public
    { Public declarations }
     procedure RedrawTheMap;
  end;


function SetMapOptions(var MapForm : tMapForm) : boolean;


implementation

{$R *.dfm}

uses
   {$IfDef ExSat}
   {$Else}
   DEMSatMerge,
   {$EndIf}

   Elev_color_range,DEMDefs,
   DEMRefOp,DEMSlopeOpts,DEMElevOps,
   DEMCoord,DEM_Manager,
   Petmar,Petmar_types;


function SetMapOptions(var MapForm : tMapForm) : boolean;
var
  MapOptsForm : TTMapOptsForm;
begin
    MapOptsForm := TTMapOptsForm.Create(Application);
    with MapForm,MapOptsForm do begin
       theMapOwner := MapForm;
       if (MapDraw.TerrainShadowsDEM <> 0) then RadioGroup1.ItemIndex := 4
       else RadioGroup1.ItemIndex := ord(MapDraw.MapMerge);
       RadioGroup1.Enabled := MapDraw.DemOnMap <> 0;
       if (MapDraw.DemOnMap = 0) then RadioGroup1.ItemIndex := 0;
       CheckBox1.Checked := MapDraw.MakeMapGrayscale;
       CheckBox2.Checked := MapDraw.GrayscaleSubdueOverlays;
       CheckBox3.Checked := MapDraw.SubdueBase;
       CheckBox4.Checked := MapDraw.GrayscaleWorldOutline;
       CheckBox5.Checked := MapDraw.SubdueWorldOutline;
       CheckBox6.Checked := MapDraw.SubdueGrids;
       CheckBox7.Checked := MapDraw.GrayscaleOSM;
       CheckBox8.Checked := MapDraw.SubdueOSM;
       CheckBox9.Checked := MDDef.MakeTigerMapGrayscale;
       CheckBox10.Checked := MDDef.SubdueTigerBase;
       ShowModal;
       RedrawTheMap;
       Free;
    end;
end;

procedure TTMapOptsForm.RedrawSpeedButton12Click(Sender: TObject);
begin
   RedrawTheMap;
end;

procedure TTMapOptsForm.RedrawTheMap;
begin
   with TheMapOwner do begin
       MapDraw.TerrainShadowsDEM := 0;
       if (RadioGroup1.ItemIndex = 4) then begin
          MapDraw.TerrainShadowsDEM := MapDraw.DEMonMap;
       end
       else MapDraw.MapMerge := tMapMerge(RadioGroup1.ItemIndex);
       MapDraw.MakeMapGrayscale := CheckBox1.Checked;
       MapDraw.GrayscaleSubdueOverlays := CheckBox2.Checked;
       MapDraw.SubdueBase := CheckBox3.Checked;
       MapDraw.GrayscaleWorldOutline := CheckBox4.Checked;
       MapDraw.SubdueWorldOutline := CheckBox5.Checked;
       MapDraw.SubdueGrids := CheckBox6.Checked;
       MapDraw.GrayscaleOSM := CheckBox7.Checked;
       MapDraw.SubdueOSM := CheckBox8.Checked;
       MDDef.MakeTigerMapGrayscale := CheckBox9.Checked;
       MDDef.SubdueTigerBase := CheckBox10.Checked;
       DoBaseMapRedraw;
    end;
end;

procedure TTMapOptsForm.BitBtn1Click(Sender: TObject);
begin
   DEMRefOp.ChangeReflectanceOptions(DEMGlb[TheMapOwner.MapDraw.DEMonMap].SelectionMap);
end;

procedure TTMapOptsForm.BitBtn2Click(Sender: TObject);
begin
   ChangeSlopeMapOptions(theMapOwner);
end;

procedure TTMapOptsForm.BitBtn3Click(Sender: TObject);
begin
   PickMapElevationRangeForColoring(theMapOwner);
end;

procedure TTMapOptsForm.RadioGroup1Click(Sender: TObject);
begin
   BitBtn1.Enabled := RadioGroup1.ItemIndex in [3,4];
   BitBtn2.Enabled := RadioGroup1.ItemIndex in [2];
   BitBtn3.Enabled := RadioGroup1.ItemIndex in [1];
   BitBtn4.Enabled := RadioGroup1.ItemIndex in [2];
   BitBtn5.Enabled := RadioGroup1.ItemIndex in [1];
end;


procedure TTMapOptsForm.BitBtn4Click(Sender: TObject);
begin
   MDdef.MaxMergeSlope := 100 * MDdef.MaxMergeSlope;
   ReadDefault('Max slope on merge (%)',MDdef.MaxMergeSlope);
   MDdef.MaxMergeSlope := 0.01 * MDdef.MaxMergeSlope;
end;

procedure TTMapOptsForm.BitBtn5Click(Sender: TObject);
begin
   {$IfDef ExSat}
   {$Else}
   GetIHSparameters(MDdef.MergeInt,MDdef.MergeHue,MDdef.MergeSat{,MergeUseReflectance});
   {$EndIf}
end;

procedure TTMapOptsForm.CheckBox11Click(Sender: TObject);
begin
   If CheckBox11.Checked then begin
      GetDEM(theMapOwner.MapDraw.TerrainShadowsDEM,false,'Elevations for 3D effect');
      CheckBox11.Caption := 'Elevations from ' + DEMGlb[theMapOwner.MapDraw.TerrainShadowsDEM].AreaName;
   end
   else CheckBox11.Caption := 'Elevations from another DEM';
end;


procedure TTMapOptsForm.FormCreate(Sender: TObject);
begin
   Petmar.PlaceFormAtMousePosition(Self);
end;

procedure TTMapOptsForm.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\map_options.htm');
end;


initialization
finalization
end.
