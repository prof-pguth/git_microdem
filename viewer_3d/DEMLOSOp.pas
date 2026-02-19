unit Demlosop;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2026 Peter L. Guth  }
{________________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   //{$Define RecordLOSOptions}
{$EndIf}

interface

uses
  Windows, Classes, Graphics, Forms, Controls, Buttons, SysUtils,
  Petmar_types,PETMAR,DEMLOSW,DEMMapf, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TLOSOption = class(TForm)
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    HelpBtn: TBitBtn;
    BitBtn11: TBitBtn;
    BitBtn17: TBitBtn;
    BitBtn18: TBitBtn;
    BitBtn22: TBitBtn;
    SaveSpeedButton: TSpeedButton;
    CheckBox3: TCheckBox;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Label13: TLabel;
    BitBtn12: TBitBtn;
    Label19: TLabel;
    BitBtn13: TBitBtn;
    Label14: TLabel;
    Label20: TLabel;
    Label2: TLabel;
    Edit2: TEdit;
    Label3: TLabel;
    Edit3: TEdit;
    Edit12: TEdit;
    Label15: TLabel;
    Label16: TLabel;
    Edit13: TEdit;
    GroupBox7: TGroupBox;
    Label6: TLabel;
    Label9: TLabel;
    Label5: TLabel;
    Edit8: TEdit;
    Edit1: TEdit;
    Edit5: TEdit;
    GroupBox5: TGroupBox;
    CheckBox7: TCheckBox;
    BitBtn3: TBitBtn;
    CheckBox23: TCheckBox;
    GroupBox10: TGroupBox;
    CheckBox19: TCheckBox;
    Edit15: TEdit;
    CheckBox20: TCheckBox;
    CheckBox21: TCheckBox;
    GroupBox11: TGroupBox;
    Label17: TLabel;
    Label18: TLabel;
    CheckBox24: TCheckBox;
    Edit16: TEdit;
    Edit17: TEdit;
    CheckBox10: TCheckBox;
    TabSheet2: TTabSheet;
    GroupBox8: TGroupBox;
    CheckBox1: TCheckBox;
    RadioGroup1: TRadioGroup;
    BitBtn9: TBitBtn;
    BitBtn10: TBitBtn;
    GroupBox6: TGroupBox;
    Label10: TLabel;
    Label11: TLabel;
    CheckBox8: TCheckBox;
    BitBtn8: TBitBtn;
    Edit9: TEdit;
    Edit10: TEdit;
    GroupBox4: TGroupBox;
    Label4: TLabel;
    Edit4: TEdit;
    CheckBox4: TCheckBox;
    BitBtn6: TBitBtn;
    BitBtn7: TBitBtn;
    GroupBox3: TGroupBox;
    BitBtn5: TBitBtn;
    CheckBox9: TCheckBox;
    GroupBox2: TGroupBox;
    BitBtn4: TBitBtn;
    CheckBox2: TCheckBox;
    Edit6: TEdit;
    Label7: TLabel;
    CheckBox6: TCheckBox;
    CheckBox11: TCheckBox;
    CheckBox12: TCheckBox;
    CheckBox17: TCheckBox;
    Label1: TLabel;
    Button1: TButton;
    TabSheet3: TTabSheet;
    CheckBox26: TCheckBox;
    CheckBox18: TCheckBox;
    GroupBox1: TGroupBox;
    Label8: TLabel;
    Label12: TLabel;
    CheckBox13: TCheckBox;
    CheckBox14: TCheckBox;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Edit7: TEdit;
    Edit11: TEdit;
    CheckBox5: TCheckBox;
    Edit14: TEdit;
    CheckBox15: TCheckBox;
    BitBtn15: TBitBtn;
    BitBtn16: TBitBtn;
    RadioGroup3: TRadioGroup;
    RadioGroup4: TRadioGroup;
    RadioGroup2: TRadioGroup;
    RadioGroup7: TRadioGroup;
    GroupBox9: TGroupBox;
    CheckBox16: TCheckBox;
    BitBtn14: TBitBtn;
    BitBtn19: TBitBtn;
    RadioGroup6: TRadioGroup;
    RadioGroup5: TRadioGroup;
    Font: TBitBtn;
    CheckBox22: TCheckBox;
    CheckBox25: TCheckBox;
    CheckBox27: TCheckBox;
    procedure HelpBtnClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CheckBox7Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure BitBtn6Click(Sender: TObject);
    procedure BitBtn7Click(Sender: TObject);
    procedure BitBtn8Click(Sender: TObject);
    procedure BitBtn9Click(Sender: TObject);
    procedure BitBtn10Click(Sender: TObject);
    procedure BitBtn11Click(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure Edit3Change(Sender: TObject);
    procedure Edit6Change(Sender: TObject);
    procedure CheckBox6Click(Sender: TObject);
    procedure Edit11Change(Sender: TObject);
    procedure CheckBox5Click(Sender: TObject);
    procedure BitBtn14Click(Sender: TObject);
    procedure BitBtn15Click(Sender: TObject);
    procedure BitBtn16Click(Sender: TObject);
    procedure BitBtn17Click(Sender: TObject);
    procedure CheckBox18Click(Sender: TObject);
    procedure Edit7Change(Sender: TObject);
    procedure Edit12Change(Sender: TObject);
    procedure Edit13Change(Sender: TObject);
    procedure Edit4Change(Sender: TObject);
    procedure Edit15Change(Sender: TObject);
    procedure CheckBox21Click(Sender: TObject);
    procedure CheckBox19Click(Sender: TObject);
    procedure CheckBox23Click(Sender: TObject);
    procedure CheckBox24Click(Sender: TObject);
    procedure Edit16Change(Sender: TObject);
    procedure Edit17Change(Sender: TObject);
    procedure BitBtn18Click(Sender: TObject);
    procedure CheckBox26Click(Sender: TObject);
    procedure BitBtn22Click(Sender: TObject);
    procedure CheckBox13Click(Sender: TObject);
    procedure CheckBox14Click(Sender: TObject);
    procedure CheckBox15Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox9Click(Sender: TObject);
    procedure CheckBox8Click(Sender: TObject);
    procedure SaveSpeedButtonClick(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure RadioGroup3Click(Sender: TObject);
    procedure RadioGroup4Click(Sender: TObject);
    procedure BitBtn19Click(Sender: TObject);
    procedure RadioGroup5Click(Sender: TObject);
    procedure RadioGroup6Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure RadioGroup7Click(Sender: TObject);
    procedure FontClick(Sender: TObject);
    procedure CheckBox25Click(Sender: TObject);
    procedure CheckBox22Click(Sender: TObject);
    procedure CheckBox27Click(Sender: TObject);
  private
    { Private declarations }
    LOSFormMustRecalculate,
    SetupDone,
    PointCloudMustReload : boolean;
    theBaseMap : tMapForm;
    theLOSForm : TDEMLOSF;
    procedure SetPointCloudOptions;
    procedure GetOptions;
  public
    { Public declarations }
     procedure ShowSetting;
  end;


function SetLOSOptions(var LOSForm : TDEMLOSF) : boolean;


implementation

{$R *.DFM}

uses
   {$IFDef ExPointCloud}
   {$Else}
      Point_Cloud_Options,
      Point_Cloud_Memory,
   {$EndIf}
   {$IfDef ExGIS}
   {$Else}
      demdatabase,
   {$EndIf}

   Nevadia_Main,
   BaseGraf, PetImage,
   BaseMap,DEMOptions,DEMCoord,
   DEMDefs,DEMDef_routines,DEMCurvature, PETMath;


function SetLOSOptions(var LOSForm : TDEMLOSF) : boolean;

     function ElevStr(z : float64; DEMName : string3) : shortString;
     begin
        if z > -9998 then Result := '  z(' + DEMName + ')=' + RealToString(z,-12,-2)
        else Result := '';
     end;

var
  LOSOption: TLOSOption;
begin
   LOSOption := TLOSOption.Create(Application);
   with LOSOption do begin
      {$IfDef RecordLOSOptions} WriteLineToDebugFile('Enter set losoptions'); {$EndIf}
      theBaseMap := LOSform.BaseMap;
      theLOSForm := LosForm;
      Label13.Caption := LOSform.BaseMap.MapDraw.PrimMapProj.PreferLocationString(LOSform.LOSdraw.LatLeft,LOSform.LOSdraw.LongLeft);
      Label19.Caption := ElevStr(LOSform.LOSdraw.ObsGroundElev,'DEM') +  ElevStr(LOSform.LOSdraw.ObsGroundElevLAS,'LAS');
      Label14.Caption := LOSform.BaseMap.MapDraw.PrimMapProj.PreferLocationString(LOSform.LOSDraw.LatRight,LOSform.LOSDraw.LongRight);
      Label20.Caption := ElevStr(LOSform.LOSDraw.TargetGroundElev,'DEM') +  ElevStr(LOSform.LOSDraw.TargetGroundElevLAS,'LAS');

      LOSoption.Edit1.Text := RealToString(LOSForm.LOSdraw.MaxAreaZ,-12,-2);
      Edit5.Text := RealToString(LOSForm.LOSdraw.FormVertExag,-18,-4);
      Edit8.Text := RealToString(LOSForm.LOSdraw.MinAreaZ,-12,-2);
      Edit12.Text := RealToString(LOSForm.LOSdraw.FormSectLenMeters,-12,-2);
      Edit13.Text := RealToString(LOSForm.LOSdraw.LOSAzimuth,-12,-2);
      Edit16.Text := RealToString(LOSForm.LOSdraw.ZoomLeft,-12,-3);
      Edit17.Text := RealToString(LOSForm.LOSdraw.ZoomRight,-12,-3);
      TabSheet2.Visible := theLOSForm.LOSdraw.LOSVariety = losVanilla;

      LOSOption.ShowSetting;
      LOSFormMustRecalculate := false;
      SetupDone := true;

      {$IfDef RecordLOSOptions} WriteLineToDebugFile('do ShowModal'); {$EndIf}
      if (LOSOption.ShowModal = mrCancel) then begin
         Result := false;
      end
      else begin
         Result := true;
         GetOptions;
      end;
   end;
{$IfDef RecordLOSOptions} WriteLineToDebugFile('Exit set losoptions'); {$EndIf}
end;


procedure TLOSOption.GetOptions;
begin
   MDDef.LOSVisible := CheckBox1.Checked;
   MDDef.DrawLOS := CheckBox2.Checked;
   MDDef.DifferentFloorSliceThickness := CheckBox5.Checked;
   MDDef.MissingDataBlocksLOS := CheckBox6.Checked;
   MDDef.ShowDEMTerrainProfile := CheckBox7.Checked;
   MDDef.LOSShowPitch := CheckBox8.Checked;
   MDDef.ShowMaskedAirspace := CheckBox9.Checked;
   MDDef.DrawLOSProtractor := CheckBox10.Checked;
   MDDef.ShowObserverMaskingCircle:= CheckBox11.Checked;
   MDDef.ShowLOSDataBase := CheckBox12.Checked;
   MDDef.ShowOpenGLLOSline := CheckBox16.Checked;
   MDDef.DoGrazingFields := CheckBox17.Checked;

   MDDef.LOSVisShowLine:= RadioGroup1.ItemIndex = 1;

   CheckEditString(Edit1.Text,theLOSForm.LOSdraw.MaxAreaZ);
   CheckEditString(Edit8.Text,theLOSForm.LOSdraw.MinAreaZ);
   CheckEditString(Edit2.Text,MDDef.ObsAboveGround);
   CheckEditString(Edit3.Text,MDDef.TargetAboveGround);
   CheckEditString(Edit5.Text,theLOSForm.LOSdraw.FormVertExag);
   CheckEditString(Edit6.Text,MDDef.wf.ClosestBlockingDistance);
   CheckEditString(Edit7.Text,MDDef.LOSSliceBuffer);
   CheckEditString(Edit9.Text,MDDef.LOSMinPitch);
   CheckEditString(Edit10.Text,MDDef.LOSMaxPitch);
   CheckEditString(Edit11.Text,MDDef.CloudSliceThick);
   CheckEditString(Edit14.Text,MDDef.FloorSliceThick);

   {$IfDef ExFresnel}
   {$Else}
      MDDef.DrawFresnel := CheckBox4.Checked;
      CheckEditString(Edit4.Text,MDDef.FresnelFreq);
   {$EndIf}
   {$IfDef ExPointCloud}
   {$Else}
      MDDef.LOSshowIntervisibilityFromLAS := CheckBox15.Checked;
      MDDef.LOSshowPointCloudRangeLines := CheckBox14.Checked;
   {$EndIf}

   if CheckBox4.Checked and (MDdef.EarthVertCurvAlg <> vcRadioLineOfSight) then begin
      MDdef.EarthVertCurvAlg := vcRadioLineOfSight;
      Label1.Caption := EarthCurvAlgName(MDdef.EarthVertCurvAlg);
      MessageToContinue('Fresnel zones requires Radio LOS');
   end;

   if LOSFormMustRecalculate and (theLOSForm.LOSdraw.LOSProfileDB <> 0) then begin
      CloseSingleDB(theLOSForm.LOSdraw.LOSProfileDB);
   end;
end;


procedure TLOSOption.SaveSpeedButtonClick(Sender: TObject);
begin
   theLosForm.Saveimage1Click(Sender);
end;

procedure TLOSOption.SetPointCloudOptions;
begin
   {$IFDef ExPointCloud}
      GroupBox1.Enabled := false;
      BitBtn16.Enabled := false;
   {$Else}
      GroupBox1.Enabled := true;
      BitBtn16.Enabled := (Point_Cloud_Options.pt_cloud_opts_fm = Nil);
      Edit7.Enabled := (Point_Cloud_Options.pt_cloud_opts_fm <> Nil);
      Edit11.Enabled := (Point_Cloud_Options.pt_cloud_opts_fm <> Nil);
      Label8.Enabled := (Point_Cloud_Options.pt_cloud_opts_fm <> Nil);
      Label12.Enabled := (Point_Cloud_Options.pt_cloud_opts_fm <> Nil);
   {$Endif}
end;


procedure TLOSOption.ShowSetting;
begin
   {$IfDef RecordLOSOptions} WriteLineToDebugFile('TLOSOption.ShowSetting'); {$EndIf}
   Edit2.Text := RealToString(MDDef.ObsAboveGround,-18,-4);
   Edit3.Text := RealToString(MDDef.TargetAboveGround,-18,-4);

   Edit6.Text := IntToStr(MDDef.wf.ClosestBlockingDistance);
   Edit7.Text := IntToStr(MDDef.LOSSliceBuffer);
   Edit9.Text := RealToString(MDDef.LOSMinPitch,-8,-2);
   Edit10.Text := RealToString(MDDef.LOSMaxPitch,-8,-2);
   Edit11.Text := RealToString(MDDef.CloudSliceThick,-8,-2);
   Edit14.Text := RealToString(MDDef.FloorSliceThick,-12,-2);

   CheckBox1.Checked := MDDef.LOSVisible;
   CheckBox2.Checked := MDDef.DrawLOS;
   CheckBox5.Checked := MDDef.DifferentFloorSliceThickness;
   CheckBox6.Checked := MDDef.MissingDataBlocksLOS;
   CheckBox7.Checked := MDDef.ShowDEMTerrainProfile;
   CheckBox8.Checked := MDDef.LOSShowPitch;
   CheckBox9.Checked := MDDef.ShowMaskedAirspace;
   CheckBox10.Checked := MDDef.DrawLOSProtractor;
   CheckBox11.Checked := MDDef.ShowObserverMaskingCircle;
   CheckBox12.Checked := MDDef.ShowLOSDataBase;
   CheckBox16.Checked := MDDef.ShowOpenGLLOSline;
   CheckBox17.Checked := MDDef.DoGrazingFields;
   CheckBox18.Checked := MDDef.ShowGridVegEffects;
   CheckBox19.Checked := MDDef.ForceLOSHorizTickSpacing;
   CheckBox21.Checked := MDDef.LOSHorizTickSpacing_KM;
   CheckBox23.Checked := MDDef.LabelMultipleProf;
   Edit4.Enabled := CheckBox4.Checked;
   Label4.Enabled := CheckBox4.Checked;
   Label1.Caption := EarthCurvAlgName(MDdef.EarthVertCurvAlg);
   SetPointCloudOptions;
   if MDDef.LOSVisShowLine then RadioGroup1.ItemIndex := 1 else RadioGroup1.ItemIndex := 0;

   {$IfDef ExPointCloud}
      CheckBox13.Visible := false;
      CheckBox14.Visible := false;
      CheckBox15.Visible := false;
      RadioGroup3.Visible := false;
   {$Else}
      CheckBox15.Checked := MDDef.LOSshowIntervisibilityFromLAS;
      CheckBox13.Checked := MDDef.LOSshowPointCloudRangePoints;
      CheckBox14.Checked := MDDef.LOSshowPointCloudRangeLines;
      RadioGroup3.Enabled := theLOSForm.LOSdraw.LOSMemoryPointCloud[2] <> Nil;
   {$EndIf}
   {$IfDef ExFresnel}
   {$Else}
      Edit4.Text := RealToString(MDDef.FresnelFreq,-18,-4);
      CheckBox4.Checked := MDDef.DrawFresnel;
   {$EndIf}
   {$IfDef ExVegDensity}
      CheckBox26.Enabled := false;
      RadioGroup2.Enabled := false;
      CheckBox18.Enabled := false;
   {$Else}
      CheckBox18.Enabled := (DEMGlb[theLosForm.LOSdraw.DEMonView].VegGrid[1] <> 0);
      CheckBox26.Enabled := (DEMGlb[theLosForm.LOSdraw.DEMonView].VegDensityLayers[1] <> Nil);
      RadioGroup2.Enabled := DEMGlb[theLOSForm.LOSdraw.DEMonView].VegDensityLayers[2] <> Nil;
   {$EndIf}
end;

procedure TLOSOption.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\tbme1wc3.htm');
end;


procedure TLOSOption.RadioGroup2Click(Sender: TObject);
begin
   theBaseMap.MapDraw.VegDensityLayerInUse := succ(RadioGroup2.ItemIndex);
   BitBtn17Click(Sender);
end;

procedure TLOSOption.RadioGroup3Click(Sender: TObject);
begin
   {$IFDef ExPointCloud}
   {$Else}
      theLOSForm.LOSdraw.PtCldInUse := succ(RadioGroup3.ItemIndex);
      BitBtn17Click(Sender);
   {$EndIf}
end;

procedure TLOSOption.RadioGroup4Click(Sender: TObject);
begin
   MDDef.ShowPointCloundOnProfile := tShowPointCloundOnProfile(RadioGroup4.ItemIndex);
   BitBtn17Click(Sender);
end;

procedure TLOSOption.RadioGroup5Click(Sender: TObject);
begin
   MDDef.ShowCloudDensity := RadioGroup5.ItemIndex = 1;
   MDDef.LOSShowVoxelDensity := RadioGroup5.ItemIndex = 2;
   BitBtn17Click(Sender);
end;

procedure TLOSOption.RadioGroup6Click(Sender: TObject);
begin
   MDDef.VegEffectsVoxels := RadioGroup6.ItemIndex = 2;
   if RadioGroup6.ItemIndex = 1 then MDDef.ShowPointCloundOnProfile := spcDensity
   else MDDef.ShowPointCloundOnProfile := spcNone;
   BitBtn17Click(Sender);
end;


procedure TLOSOption.RadioGroup7Click(Sender: TObject);
begin
   {$IFDef ExVegDensity}
   {$Else}
      theLOSForm.LOSdraw.VegGridUsed := succ(RadioGroup7.ItemIndex);
      BitBtn17Click(Sender);
   {$EndIf}
end;

procedure TLOSOption.BitBtn10Click(Sender: TObject);
begin
   QueryColor(BitBtn10,MDdef.MaskColor);
end;

procedure TLOSOption.BitBtn11Click(Sender: TObject);
begin
   SetLOSDefaults;
   ShowSetting;
end;

procedure TLOSOption.BitBtn14Click(Sender: TObject);
begin
   PickLineSizeAndColor('LOS',BitBtn14,MDdef.OpenGLLosLineColor, MDDef.OpenGLLosLineWidth);
end;

procedure TLOSOption.BitBtn15Click(Sender: TObject);
begin
   DEMoptions.ChangeOptions(2);
end;

procedure TLOSOption.BitBtn16Click(Sender: TObject);
begin
   LOSFormMustRecalculate := true;
   theBaseMap.PointCloudSpeedButtonClick(nil);
   SetPointCloudOptions;
end;

procedure TLOSOption.BitBtn17Click(Sender: TObject);
begin
   {$IfDef RecordLOSOptions} WriteLineToDebugFile('TLOSOption.BitBtn17Click in'); {$EndIf}
   if SetupDone then begin
      GetOptions;
      theLOSForm.FormResize(Nil);
      ShowSetting;
   end;
   {$IfDef RecordLOSOptions} WriteLineToDebugFile('TLOSOption.BitBtn17Click out'); {$EndIf}
end;

procedure TLOSOption.BitBtn18Click(Sender: TObject);
begin
   LOSFormMustRecalculate := true;
end;

procedure TLOSOption.BitBtn19Click(Sender: TObject);
begin
   theLosForm.OpenGLofPointCloud1Click(Sender);
end;

procedure TLOSOption.BitBtn1Click(Sender: TObject);
begin
   {$IfDef ExPointCloud}
   {$Else}
      PickLineSizeAndColor('Ceiling',BitBtn1,MDdef.PointCloudCeilingColor,MDdef.PointCloudCeilingWidth);
   {$EndIf}
end;


procedure TLOSOption.BitBtn22Click(Sender: TObject);
begin
   theLosForm.Copytoclipboard1Click(Sender);
end;

procedure TLOSOption.BitBtn2Click(Sender: TObject);
begin
   {$IfDef ExPointCloud}
   {$Else}
       PickLineSizeAndColor('Ceiling',BitBtn2,MDdef.PointCloudFloorColor,MDdef.PointCloudFloorWidth);
   {$EndIf}
end;

procedure TLOSOption.BitBtn3Click(Sender: TObject);
begin
   PickLineSizeAndColor('Ceiling',BitBtn3,MDdef.TerrainProfileColor,MDdef.TerrainProfileWidth);
end;

procedure TLOSOption.BitBtn4Click(Sender: TObject);
begin
   PickLineSizeAndColor('LOS',BitBtn4,MDdef.LOSConnectionColor,MDdef.LOSConnectionWidth);
end;

procedure TLOSOption.BitBtn5Click(Sender: TObject);
begin
   PickLineSizeAndColor('LOS',BitBtn5,MDdef.MaskedAirspaceColor,MDdef.MaskedAirspaceWidth);
end;

procedure TLOSOption.BitBtn6Click(Sender: TObject);
begin
   {$IfDef ExFresnel}
   {$Else}
      PickLineSizeAndColor('Fresnel zone 1',BitBtn6,MDdef.FresnelZone1Color,MDdef.FresnelZone1Width);
   {$EndIf}
end;

procedure TLOSOption.BitBtn7Click(Sender: TObject);
begin
   {$IfDef ExFresnel}
   {$Else}
      PickLineSizeAndColor('Fresnel zone 2',BitBtn7,MDdef.FresnelZone2Color,MDdef.FresnelZone2Width);
   {$EndIf}
end;

procedure TLOSOption.BitBtn8Click(Sender: TObject);
begin
   PickLineSizeAndColor('Pitch line',BitBtn8,MDdef.PitchLineColor,MDdef.PitchLineWidth);
end;

procedure TLOSOption.BitBtn9Click(Sender: TObject);
begin
   QueryColor(BitBtn9,MDdef.FanColor);
end;

procedure TLOSOption.Button1Click(Sender: TObject);
begin
   demcurvature.GetCurvAlg;
   Label1.Caption := EarthCurvAlgName(MDdef.EarthVertCurvAlg);
   if (MDdef.EarthVertCurvAlg <> vcRadioLineOfSight) then CheckBox4.Checked := false;
   LOSFormMustRecalculate := true;
end;

procedure TLOSOption.CheckBox13Click(Sender: TObject);
begin
   BitBtn17Click(Sender);
end;

procedure TLOSOption.CheckBox14Click(Sender: TObject);
begin
   BitBtn17Click(Sender);
end;

procedure TLOSOption.CheckBox15Click(Sender: TObject);
begin
   BitBtn17Click(Sender);
end;

procedure TLOSOption.CheckBox18Click(Sender: TObject);
begin
   MDDef.ShowGridVegEffects := CheckBox18.Checked;
   BitBtn17Click(Sender);
end;

procedure TLOSOption.CheckBox19Click(Sender: TObject);
begin
   MDDef.ForceLOSHorizTickSpacing := CheckBox19.Checked;
end;

procedure TLOSOption.CheckBox1Click(Sender: TObject);
begin
   BitBtn17Click(Sender);
end;

procedure TLOSOption.CheckBox21Click(Sender: TObject);
begin
   MDDef.LOSHorizTickSpacing_KM := CheckBox21.Checked;
end;

procedure TLOSOption.CheckBox22Click(Sender: TObject);
begin
   MDDef.ProfileShowZLevel := CheckBox25.Checked;
   MDDef.ProfileShowSeaLevel := CheckBox22.Checked;
end;

procedure TLOSOption.CheckBox23Click(Sender: TObject);
begin
   MDDef.LabelMultipleProf := CheckBox23.Checked;
   BitBtn17Click(Sender);
end;

procedure TLOSOption.CheckBox24Click(Sender: TObject);
begin
   theLOSForm.LOSdraw.ZoomedDiagram := CheckBox24.Checked;
   BitBtn17Click(Sender);
end;

procedure TLOSOption.CheckBox25Click(Sender: TObject);
begin
   MDDef.ProfileShowZLevel := CheckBox25.Checked;
end;

procedure TLOSOption.CheckBox26Click(Sender: TObject);
begin
   MDDef.VegEffectsVoxels := CheckBox26.Checked;
   BitBtn17Click(Sender);
end;

procedure TLOSOption.CheckBox27Click(Sender: TObject);
begin
   MDDef.LOSLeftTixLabels := CheckBox27.Checked;
end;

procedure TLOSOption.CheckBox2Click(Sender: TObject);
begin
   BitBtn17Click(Sender);
end;

procedure TLOSOption.CheckBox3Click(Sender: TObject);
begin
   SetupDone := false;
   RadioGroup5.ItemIndex := 0;
   RadioGroup6.ItemIndex := 0;
   SetupDone := true;
   theLosForm.LOSdraw.CompareDensityAlongProfile := CheckBox3.Checked;
   BitBtn17Click(Sender);
end;

procedure TLOSOption.CheckBox4Click(Sender: TObject);
begin
   Edit4.Enabled := CheckBox4.Checked;
   Label4.Enabled := CheckBox4.Checked;
   LOSFormMustRecalculate := true;
   BitBtn17Click(Sender);
end;

procedure TLOSOption.CheckBox5Click(Sender: TObject);
begin
   Edit14.Enabled := CheckBox5.Checked;
end;

procedure TLOSOption.CheckBox6Click(Sender: TObject);
begin
   LOSFormMustRecalculate := true;
end;

procedure TLOSOption.CheckBox7Click(Sender: TObject);
begin
   BitBtn17Click(Sender);
end;


procedure TLOSOption.CheckBox8Click(Sender: TObject);
begin
   BitBtn17Click(Sender);
end;

procedure TLOSOption.CheckBox9Click(Sender: TObject);
begin
   BitBtn17Click(Sender);
end;

procedure TLOSOption.Edit11Change(Sender: TObject);
begin
   LOSFormMustRecalculate := true;
end;


procedure TLOSOption.Edit12Change(Sender: TObject);
begin
   LOSFormMustRecalculate := true;
end;

procedure TLOSOption.Edit13Change(Sender: TObject);
begin
   LOSFormMustRecalculate := true;
end;

procedure TLOSOption.Edit15Change(Sender: TObject);
begin
   CheckEditString(Edit15.Text,MDDef.LOSHorizTick);
end;

procedure TLOSOption.Edit16Change(Sender: TObject);
begin
   CheckEditString(Edit16.Text,theLOSForm.LOSdraw.ZoomLeft);
end;

procedure TLOSOption.Edit17Change(Sender: TObject);
begin
   CheckEditString(Edit17.Text,theLOSForm.LOSdraw.ZoomRight);
end;

procedure TLOSOption.Edit2Change(Sender: TObject);
begin
   LOSFormMustRecalculate := true;
end;

procedure TLOSOption.Edit3Change(Sender: TObject);
begin
   LOSFormMustRecalculate := true;
end;

procedure TLOSOption.Edit4Change(Sender: TObject);
begin
   LOSFormMustRecalculate := true;
end;

procedure TLOSOption.Edit6Change(Sender: TObject);
begin
   LOSFormMustRecalculate := true;
end;

procedure TLOSOption.Edit7Change(Sender: TObject);
begin
   LOSFormMustRecalculate := true;
end;

procedure TLOSOption.FontClick(Sender: TObject);
begin
   EditMyFont(MDDef.LOSfont);
end;

procedure TLOSOption.FormCreate(Sender: TObject);
begin
   {$IfDef RecordLOSOptions} WriteLineToDebugFile('TLOSOption.FormCreate in'); {$EndIf}
   SetupDone := false;
   PointCloudMustReload := false;

   if MDDef.SimpleTopoProfilesOnly then begin
      BitBtn18.Visible := false;
      CheckBox7.Enabled := false;
      CheckBox3.Visible := false;
      PageControl1.ActivePage := PageControl1.Pages[0];
      PageControl1.Pages[1].TabVisible := false;  //pages are 0 based
      PageControl1.Pages[2].TabVisible := false;  //pages are 0 based
   end;
   ColorLineWidthBitBtn(BitBtn3,MDdef.TerrainProfileColor,MDdef.TerrainProfileWidth);
   ColorLineWidthBitBtn(BitBtn4,MDdef.LOSConnectionColor,MDdef.LOSConnectionWidth);
   ColorLineWidthBitBtn(BitBtn5,MDdef.MaskedAirspaceColor,MDdef.MaskedAirspaceWidth);
   ColorLineWidthBitBtn(BitBtn8,MDdef.PitchLineColor,MDdef.PitchLineWidth);
   ColorLineWidthBitBtn(BitBtn14,MDdef.OpenGLLosLineColor, round(MDDef.OpenGLLosLineWidth));
   ColorBitBtn(BitBtn9,MDdef.FanColor);
   ColorBitBtn(BitBtn10,MDdef.MaskColor);
   Edit15.Text := RealToString(MDDef.LOSHorizTick,-8,-4);
   RadioGroup4.ItemIndex := ord(MDDef.ShowPointCloundOnProfile);
   if MDDef.ShowCloudDensity then RadioGroup5.ItemIndex := 1
   else if MDDef.LOSShowVoxelDensity then RadioGroup5.ItemIndex := 2
   else RadioGroup5.ItemIndex := 0;
   CheckBox26.Checked := MDDef.VegEffectsVoxels;

   CheckBox25.Checked := MDDef.ProfileShowZLevel;
   CheckBox22.Checked := MDDef.ProfileShowSeaLevel;
   CheckBox27.Checked := MDDef.LOSLeftTixLabels;

   if MDdef.ProgramOption = DragonPlotProgram then begin
      PageControl1.Pages[2].TabVisible := false;  //pages are 0 based
   end;

   {$IfDef ExPointCloud}
      BitBtn1.Visible := false;
      BitBtn2.Visible := false;
   {$Else}
      ColorLineWidthBitBtn(BitBtn1,MDdef.PointCloudCeilingColor,MDdef.PointCloudCeilingWidth);
      ColorLineWidthBitBtn(BitBtn2,MDdef.PointCloudFloorColor,MDdef.PointCloudFloorWidth);
   {$EndIf}

   {$IfDef ExFresnel}
      BitBtn6.Visible := false;
      BitBtn7.Visible := false;
   {$Else}
      ColorLineWidthBitBtn(BitBtn6,MDdef.FresnelZone1Color,MDdef.FresnelZone1Width);
      ColorLineWidthBitBtn(BitBtn7,MDdef.FresnelZone2Color,MDdef.FresnelZone2Width);
   {$EndIf}

   PlaceFormAtMousePosition(Self);
   {$IfDef RecordLOSOptions} WriteLineToDebugFile('TLOSOption.FormCreate out'); {$EndIf}
end;


initialization
finalization
   {$IfDef RecordLOSOptions} WriteLineToDebugFile('RecordLOSOptions action in demlosop'); {$EndIf}
   {$IfDef RecordClosingProblems} WriteLineToDebugFile('Closing demlsoop out'); {$EndIf}
end.
