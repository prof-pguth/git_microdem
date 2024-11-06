unit elev_color_range;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   {$IfDef Debug}
      //{$Define ElevColorChange}
   {$EndIf}
{$EndIf}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons,
  DEMMapf,DEMMapDraw;

type
  TElevationRangeForm = class(TForm)
    OKBtn: TBitBtn;
    HelpBtn: TBitBtn;
    RadioGroup1: TRadioGroup;
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Edit3: TEdit;
    Edit4: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    RedrawSpeedButton12: TSpeedButton;
    CheckBox1: TCheckBox;
    RadioGroup2: TRadioGroup;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    procedure HelpBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure Edit3Change(Sender: TObject);
    procedure Edit4Change(Sender: TObject);
    procedure RedrawSpeedButton12Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
  private
    { Private declarations }
    procedure DisplayValues;
  public
    { Public declarations }
    MapOwner : tMapForm;
  end;


procedure PickMapElevationRangeForColoring(var MapForm : tMapForm);


implementation

{$R *.dfm}

uses
   Petmar,Petmar_types,
   DEMDefs,
   DEMCoord;


procedure PickMapElevationRangeForColoring(var MapForm : tMapForm);
var
  ElevationRangeForm : TElevationRangeForm;
begin
   {$IfDef ElevColorChange} WriteLineToDebugFile(MapOwner.MapDraw.MapColorRange('TPickMapElevationRangeForColoring in')); {$EndIf}
   ElevationRangeForm := TElevationRangeForm.Create(Application);
   ElevationRangeForm.MapOwner := MapForm;
   ElevationRangeForm.RadioGroup2.ItemIndex := ord(ElevationRangeForm.MapOwner.MapDraw.ElevStretch);
   if ElevationRangeForm.MapOwner.MapDraw.UsePercentiles then ElevationRangeForm.RadioGroup1.ItemIndex := 5
   else ElevationRangeForm.RadioGroup1.ItemIndex := 4;

   //Turn on clip colorsSet contrasting colors for the highs and lowsSpecified for the z RangeMode the Max and Min
   ElevationRangeForm.DisplayValues;
   ElevationRangeForm.ShowModal;
   MapForm.MapDraw.LockZColors := false;
   ElevationRangeForm.Free;
   {$IfDef ElevColorChange} WriteLineToDebugFile(MapOwner.MapDraw.MapColorRange('TPickMapElevationRangeForColoring out')); {$EndIf}
end;


procedure TElevationRangeForm.BitBtn1Click(Sender: TObject);
begin
   QueryColor(BitBtn1,MDdef.HighOffscaleColor);
end;

procedure TElevationRangeForm.BitBtn2Click(Sender: TObject);
begin
   QueryColor(BitBtn2,MDdef.LowOffscaleColor);
end;

procedure TElevationRangeForm.BitBtn3Click(Sender: TObject);
var
   NumPts : int64;
begin
   DEMGlb[MapOwner.MapDraw.DEMonMap].MarkOutsideRangeMissing(MapOwner.MapDraw.MinMapElev,MapOwner.MapDraw.MaxMapElev,NumPts);
   if (NumPts > 0) then RedrawSpeedButton12Click(Sender);
end;

procedure TElevationRangeForm.BitBtn4Click(Sender: TObject);
begin
   QueryColor(BitBtn4,MDdef.MissingDataColor);
end;

procedure TElevationRangeForm.CheckBox1Click(Sender: TObject);
begin
   MDDef.ClipZColors := CheckBox1.Checked;
   BitBtn1.Enabled := MDDef.ClipZColors;
   BitBtn2.Enabled := MDDef.ClipZColors;
end;

procedure TElevationRangeForm.DisplayValues;
begin
   Edit1.Enabled := RadioGroup1.ItemIndex = 4;
   Edit2.Enabled := RadioGroup1.ItemIndex = 4;
   Edit3.Enabled := RadioGroup1.ItemIndex = 5;
   Edit4.Enabled := RadioGroup1.ItemIndex = 5;
   Label3.Enabled := RadioGroup1.ItemIndex = 4;
   Label4.Enabled := RadioGroup1.ItemIndex = 5;
   Edit1.Text := RealToString(MapOwner.MapDraw.MaxMapElev,-18,-6);
   Edit2.Text := RealToString(MapOwner.MapDraw.MinMapElev,-18,-6);
   Edit4.Text := RealToString(MDDef.MinElevPercentile,-5,1);
   Edit3.Text := RealToString(MDDef.MaxElevPercentile,-5,-1);
end;


procedure TElevationRangeForm.OKBtnClick(Sender: TObject);
begin
   Close;
end;

procedure TElevationRangeForm.RadioGroup1Click(Sender: TObject);
var
   AvgElev : float32;
begin
   if (RadioGroup1.ItemIndex = 0) then begin
      MapOwner.MapDraw.MinMapElev := DEMGlb[MapOwner.MapDraw.DEMonMap].DEMheader.MinElev;
      MapOwner.MapDraw.MaxMapElev := DEMGlb[MapOwner.MapDraw.DEMonMap].DEMheader.MaxElev;
     {$IfDef ElevColorChange} WriteLineToDebugFile(MapOwner.MapDraw.MapColorRange('TElevationRangeForm.RadioGroup1Click, index=0')); {$EndIf}
   end
   else if (RadioGroup1.ItemIndex = 1) then begin
      DEMGlb[MapOwner.MapDraw.DEMonMap].BoxAreaExtremeElevations(DEMGlb[MapOwner.MapDraw.DEMonMap].SelectionMap.MapDraw.MapAreaDEMGridLimits,MapOwner.MapDraw.MinMapElev,MapOwner.MapDraw.MaxMapElev,AvgElev);
     {$IfDef ElevColorChange} WriteLineToDebugFile(MapOwner.MapDraw.MapColorRange('TElevationRangeForm.RadioGroup1Click, index=1')); {$EndIf}
   end
   else if (RadioGroup1.ItemIndex = 2) then begin   //topo
      MapOwner.MapDraw.MinMapElev := 0;
   end
   else if (RadioGroup1.ItemIndex = 3) then begin   //bathy
      MapOwner.MapDraw.MaxMapElev := 0;
   end
   else if RadioGroup1.ItemIndex = 4 then begin   //specified
   end
   else if (RadioGroup1.ItemIndex = 5) then begin   //percentiles
      MapOwner.MapDraw.UsePercentiles := true;
   end;
   if MDdef.QuickMapRedraw then RedrawSpeedButton12Click(Sender)
   else MapOwner.MapDraw.NeedToRedraw := true;
   DisplayValues;
end;


procedure TElevationRangeForm.RadioGroup2Click(Sender: TObject);
var
   NPts : int64;
begin
   MapOwner.MapDraw.ElevStretch := tElevStretch(RadioGroup2.ItemIndex);
   case MapOwner.MapDraw.ElevStretch of
      esNone : begin
                  MapOwner.MapDraw.MinMapElev := DEMGlb[MapOwner.MapDraw.DEMonMap].DEMheader.MinElev;
                  MapOwner.MapDraw.MaxMapElev := DEMGlb[MapOwner.MapDraw.DEMonMap].DEMheader.MaxElev;
               end;
      esSD : begin
                DEMGlb[MapOwner.MapDraw.DEMOnMap].ElevationStatistics(DEMGlb[MapOwner.MapDraw.DEMonMap].FullDEMGridLimits,MapOwner.MapDraw.Z_Mean,MapOwner.MapDraw.Z_Std,NPts);
                MapOwner.MapDraw.MinMapElev := (DEMGlb[MapOwner.MapDraw.DEMonMap].DEMheader.MinElev - MapOwner.MapDraw.Z_Mean) / MapOwner.MapDraw.Z_std;
                MapOwner.MapDraw.MaxMapElev := (DEMGlb[MapOwner.MapDraw.DEMonMap].DEMheader.MaxElev - MapOwner.MapDraw.Z_Mean) / MapOwner.MapDraw.Z_std;
             end;
      esPercentile : begin
                        DEMGlb[MapOwner.MapDraw.DEMOnMap].GetElevPercentiles(DEMGlb[MapOwner.MapDraw.DEMOnMap].FullDEMGridLimits);
                        MapOwner.MapDraw.MinMapElev := MDDef.MinElevPercentile;
                        MapOwner.MapDraw.MaxMapElev := MDDef.MaxElevPercentile;
                     end;
   end;
   if MDdef.QuickMapRedraw then RedrawSpeedButton12Click(nil)
   else MapOwner.MapDraw.NeedToRedraw := true;
   {$IfDef ElevColorChange} WriteLineToDebugFile(MapOwner.MapDraw.MapColorRange('TElevationRangeForm.RadioGroup2, stretch=' + IntToStr(RadioGroup2.ItemIndex))); {$EndIf}
end;

procedure TElevationRangeForm.RedrawSpeedButton12Click(Sender: TObject);
begin
   {$IfDef ElevColorChange} WriteLineToDebugFile(MapOwner.MapDraw.MapColorRange('TElevationRangeForm.RedrawSpeedButton12')); {$EndIf}
   MapOwner.DoBaseMapRedraw;
   MapOwner.MapDraw.NeedToRedraw := false;
end;

procedure TElevationRangeForm.Edit1Change(Sender: TObject);
begin
   if (Edit1.Enabled) then CheckEditString(Edit1.Text,MapOwner.MapDraw.MaxMapElev);
   MapOwner.MapDraw.NeedToRedraw := true;
end;

procedure TElevationRangeForm.Edit2Change(Sender: TObject);
begin
   if (Edit2.Enabled) then CheckEditString(Edit2.Text,MapOwner.MapDraw.MinMapElev);
   MapOwner.MapDraw.NeedToRedraw := true;
end;

procedure TElevationRangeForm.Edit3Change(Sender: TObject);
begin
   if Edit3.Enabled then CheckEditString(Edit3.Text,MDDef.MaxElevPercentile);
   MapOwner.MapDraw.MaxMapElev := DEMGlb[MapOwner.MapDraw.DEMonMap].FindPercentileElevation(MDDef.MaxElevPercentile);
   if MDdef.QuickMapRedraw then RedrawSpeedButton12Click(Sender)
   else MapOwner.MapDraw.NeedToRedraw := true;
end;

procedure TElevationRangeForm.Edit4Change(Sender: TObject);
begin
   if (Edit4.Enabled) then  CheckEditString(Edit4.Text,MDDef.MinElevPercentile);
   MapOwner.MapDraw.MinMapElev := DEMGlb[MapOwner.MapDraw.DEMonMap].FindPercentileElevation(MDDef.MinElevPercentile);
   MapOwner.MapDraw.NeedToRedraw := true;
end;


procedure TElevationRangeForm.FormCreate(Sender: TObject);
begin
   ColorBitBtn(BitBtn1,MDdef.HighOffscaleColor);
   ColorBitBtn(BitBtn2,MDdef.LowOffscaleColor);
   ColorBitBtn(BitBtn4,MDdef.MissingDataColor);
   CheckBox1.Checked := MDDef.ClipZColors;
   Petmar.PlaceFormAtMousePosition(Self);
end;


procedure TElevationRangeForm.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\elevation_range_for_coloring.htm');
end;


initialization
finalization
   {$IfDef ElevColorChange} WriteLineToDebugFile('ElevColorChange in elev_color_range'); {$EndIf}
end.
