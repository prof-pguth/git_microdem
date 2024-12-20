unit Demperop;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program       }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2024 Peter L. Guth   }
{____________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   //{$Define RecordPerspectiveProblems}
{$EndIf}


interface

uses
  Windows, Classes, Graphics, Forms, Controls, Buttons,
  StdCtrls, ExtCtrls, ComCtrls,SysUtils,
  DEMDefs,Petmar_types,DEMPersW,DEM_3d_view, Grids, Vcl.Dialogs;

type
  TPerspOptions = class(TForm)
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    HelpBtn: TBitBtn;
    Button2: TButton;
    CheckBox20: TCheckBox;
    ComboBox1: TComboBox;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Label1: TLabel;
    Label8: TLabel;
    Label3: TLabel;
    Label2: TLabel;
    Label24: TLabel;
    Edit2: TEdit;
    Edit1: TEdit;
    Edit3: TEdit;
    Edit8: TEdit;
    ComboBox2: TComboBox;
    Button1: TButton;
    CheckBox3: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox16: TCheckBox;
    TabSheet2: TTabSheet;
    Label15: TLabel;
    Label17: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label6: TLabel;
    Label18: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Edit15: TEdit;
    Edit17: TEdit;
    Edit19: TEdit;
    Edit20: TEdit;
    Edit21: TEdit;
    Edit6: TEdit;
    Edit13: TEdit;
    TabSheet3: TTabSheet;
    Label12: TLabel;
    Label13: TLabel;
    Edit11: TEdit;
    Edit10: TEdit;
    RadioGroup2: TRadioGroup;
    TabSheet5: TTabSheet;
    Label11: TLabel;
    Label16: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Edit16: TEdit;
    Edit18: TEdit;
    CheckBox11: TCheckBox;
    CheckBox12: TCheckBox;
    CheckBox13: TCheckBox;
    CheckBox14: TCheckBox;
    CheckBox15: TCheckBox;
    Edit22: TEdit;
    Edit23: TEdit;
    TabSheet6: TTabSheet;
    CheckBox17: TCheckBox;
    StringGrid1: TStringGrid;
    Edit26: TEdit;
    Edit27: TEdit;
    Edit28: TEdit;
    Edit29: TEdit;
    GroupBox1: TGroupBox;
    CheckBox6: TCheckBox;
    BitBtn1: TBitBtn;
    Edit14: TEdit;
    Label14: TLabel;
    GroupBox2: TGroupBox;
    CheckBox18: TCheckBox;
    BitBtn2: TBitBtn;
    BitBtn4: TBitBtn;
    CheckBox52: TCheckBox;
    BitBtn3: TBitBtn;
    Label29: TLabel;
    Label7: TLabel;
    Label10: TLabel;
    Edit12: TEdit;
    Edit7: TEdit;
    Edit9: TEdit;
    Label9: TLabel;
    RadioGroup1: TRadioGroup;
    Edit25: TEdit;
    Edit24: TEdit;
    Edit30: TEdit;
    BitBtn5: TBitBtn;
    TabSheet7: TTabSheet;
    Label31: TLabel;
    Panel1: TPanel;
    Label32: TLabel;
    Label33: TLabel;
    Edit33: TEdit;
    Edit34: TEdit;
    BitBtn6: TBitBtn;
    BitBtn7: TBitBtn;
    BitBtn8: TBitBtn;
    BitBtn9: TBitBtn;
    BitBtn10: TBitBtn;
    CheckBox19: TCheckBox;
    CheckBox21: TCheckBox;
    BitBtn11: TBitBtn;
    M: TLabel;
    Edit35: TEdit;
    Edit37: TEdit;
    Label34: TLabel;
    Edit36: TEdit;
    Edit38: TEdit;
    GroupBox3: TGroupBox;
    Label4: TLabel;
    Label5: TLabel;
    Edit4: TEdit;
    Edit5: TEdit;
    CheckBox7: TCheckBox;
    GroupBox4: TGroupBox;
    Label30: TLabel;
    Label35: TLabel;
    Edit39: TEdit;
    Edit40: TEdit;
    CheckBox22: TCheckBox;
    TabSheet8: TTabSheet;
    CheckBox23: TCheckBox;
    CheckBox24: TCheckBox;
    Label36: TLabel;
    Edit41: TEdit;
    Label37: TLabel;
    Edit42: TEdit;
    CheckBox25: TCheckBox;
    CheckBox26: TCheckBox;
    checkBox99: TCheckBox;
    Senso: TLabel;
    Label38: TLabel;
    Label39: TLabel;
    Edit43: TEdit;
    Edit44: TEdit;
    CheckBox27: TCheckBox;
    Edit45: TEdit;
    CheckBox28: TCheckBox;
    BitBtn12: TBitBtn;
    CheckBox2: TCheckBox;
    GroupBox5: TGroupBox;
    BitBtn13: TBitBtn;
    CheckBox1: TCheckBox;
    CheckBox4: TCheckBox;
    Label40: TLabel;
    Label41: TLabel;
    Edit46: TEdit;
    Edit47: TEdit;
    Edit31: TEdit;
    Edit32: TEdit;
    Label26: TLabel;
    Label25: TLabel;
    CheckBox10: TCheckBox;
    CheckBox9: TCheckBox;
    CheckBox8: TCheckBox;
    RedrawSpeedButton12: TSpeedButton;
    procedure OKBtnClick(Sender: TObject);
    procedure BitBtn10Click(Sender: TObject);
    procedure BitBtn9Click(Sender: TObject);
    procedure BitBtn8Click(Sender: TObject);
    procedure Edit34Change(Sender: TObject);
    procedure Edit33Change(Sender: TObject);
    procedure BitBtn6Click(Sender: TObject);
    procedure BitBtn7Click(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure Edit9Change(Sender: TObject);
    procedure Edit7Change(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure CheckBox5Click(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure CheckBox6Click(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure CheckBox52Click(Sender: TObject);
    procedure Edit15Change(Sender: TObject);
    procedure Edit17Change(Sender: TObject);
    procedure Edit13Change(Sender: TObject);
    procedure Edit20Change(Sender: TObject);
    procedure Edit21Change(Sender: TObject);
    procedure Edit6Change(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure CheckBox18Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure Edit24Change(Sender: TObject);
    procedure Edit25Change(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure BitBtn11Click(Sender: TObject);
    procedure Edit28Change(Sender: TObject);
    procedure Edit37Change(Sender: TObject);
    procedure CheckBox22Click(Sender: TObject);
    procedure CheckBox7Click(Sender: TObject);
    procedure Edit12Change(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure BitBtn13Click(Sender: TObject);
    procedure RedrawSpeedButton12Click(Sender: TObject);
  private
    { Private declarations }
     procedure ShowOptions;
     procedure CheckViewSize;
     procedure ChangeSettings;
  public
    { Public declarations }
     ElevationsChanged,StillSettingUp : boolean;
     View3D : tView3D;
     Pitch : float64;
     DEMUsed : integer;
     CurrentMode : tPersSize;
     PerfForm : TThreeDview;
     procedure Update;
  end;


function SetPerspectiveOptions(RedrawAllowed : boolean; inPerfForm : TThreeDview; var inView3D : tView3D; WhichSet : tPersSize) : boolean;


implementation

{$R *.DFM}

uses
   {$IfDef ExSat}
   {$Else}
      DEMEROS,
   {$EndIf}
   DEMRefOp,Demdef_routines,BaseMap,DEMCoord,demfanparams,
   GetLatLn,
   DEM_Manager,
   PETMAR, DEM_indexes;

const
   SPVF = 'saved perpsective view file';


function SetPerspectiveOptions(RedrawAllowed : boolean; inPerfForm : TThreeDview; var inView3D : tView3D; WhichSet : tPersSize) : boolean;
var
   PerspOptionsForm : TPerspOptions;
   i : integer;
begin
   PerspOptionsForm := TPerspOptions.Create(Application);
   with PerspOptionsForm,View3D,PersOpts do begin
      View3D := inView3D;
      PerfForm := inPerfForm;
      StillSettingUp := true;
      RedrawSpeedButton12.Enabled := RedrawAllowed;
      Pitch := View3D.MaxPitch - 0.5*(View3D.MaxPitch-View3D.MinPitch);

      {$IfDef RecordPerspectiveProblems} WriteLineToDebugFile('SetPerspectiveOptions in, Pitch=' + RealToString(Pitch,-12,2) + '  ' + View3D.VertAngleRange + '  ' + View3D.FieldOfView); {$EndIf}
      CurrentMode := WhichSet;
      DEMUsed := View3D.DEMOnView;

      TabSheet7.TabVisible := (DEMUsed <> 0);
      TabSheet3.TabVisible := View3D.DrapeMapUsing <> 0;
      TabSheet5.TabVisible := WhichSet in [psFlying];
      TabSheet6.TabVisible := MDDef.ProgramOption = ExpertProgram;

      Edit6.Enabled := WhichSet in [psPerpsective];
      Edit13.Enabled := WhichSet in [psPerpsective];
      Edit20.Enabled := WhichSet in [psPerpsective];
      Edit21.Enabled := WhichSet in [psPerpsective];
      Label27.Enabled := WhichSet in [psPerpsective];
      Edit19.Enabled := false;

      Edit26.Enabled := WhichSet in [psPanorama];
      Edit27.Enabled := WhichSet in [psPanorama];
      Edit28.Enabled := WhichSet in [psPanorama];
      Edit37.Enabled := WhichSet in [psPanorama];
      Label28.Enabled := WhichSet in [psPanorama];
      Edit29.Enabled := false;

      Edit24.Enabled := WhichSet in [psFlying];
      Edit36.Enabled := WhichSet in [psFlying];
      Edit38.Enabled := WhichSet in [psFlying];
      Edit25.Enabled := WhichSet in [psFlying];
      Label34.Enabled := WhichSet in [psPanorama];
      Edit30.Enabled := false;
      Label3.Visible := true;

      ShowOptions;

      CheckBox3.Enabled := not View3D.ASingleView;
      if View3D.ASingleView then Edit9.Enabled := false;
      Edit8.Text := RealToString(View3D.RadialPointSpacing[1],-8,-2);
      Edit9.Text := RealToString(View3D.PersOpts.PerspAbsElev,-8,-2);
      Edit12.Text := RealToString(View3D.ViewDepth,-12,-2);
      Edit17.Text := RealToString(View3D.ViewAzimuth,-12,-2);
      Edit18.Text := View3D.SaveName;
      Edit35.Text := RealToString(MDDef.PerspectiveSkyAngle,-8,1);
      Edit46.Text := IntToStr(MDDef.PerspOpts.HorizLabelInc);
      Edit47.Text := IntToStr(MDDef.PerspOpts.VertLabelInc);

      CheckBox7.Checked := MDDef.ShowCrossTrackProfiles;
      CheckBox2.Checked := not MDDef.PerspOpts.NoVE;
      CheckBox22.Checked := MDDef.ShowAlongTrackProfiles;
      Edit4.Text := IntToStr(MDDef.FlyCrossTrackheight);
      Edit5.Text := IntToStr(MDDef.FlyCrossTrackDistance);
      Edit39.Text := IntToStr(MDDef.NumSideProfiles);
      Edit40.Text := IntToStr(MDDef.SideProfileSpacing);

      Edit30.Text := IntToStr(MDDef.NumSideProfiles);
      Edit41.Text := IntToStr(MDDef.ShiftGazPeaksDist);
      Edit42.Text := IntToStr(MDDef.GazPeakObsUp);
      Edit15.Text := RealToString(Pitch,-12,2);

      {$IfDef ExFly}
      {$Else}
         Edit45.Text := RealToString(MDDef.FlyOptions.FlyHeight,-12,-2);
         CheckBox27.Checked := MDDef.FlyOptions.LiveFlyMovie;
         CheckBox28.Checked := MDDef.SaveLiveFlying;
      {$EndIf}

      CheckBox23.Checked := MDDef.AutoLabelGaz;
      CheckBox24.Checked := MDDef.ShiftGazPeaks;
      CheckBox25.Checked := MDDef.GazMarkPeaksPerspective;
      CheckBox26.Checked := MDDef.GazMarkPeaksPerpsOnMap;

      CheckBox99.Checked := MDDef.ConvergingViews;
      StillSettingUp := false;

      Update;
      CheckViewSize;
      PageControl1.ActivePage := Tabsheet2;
      Result := (ShowModal <> idCancel);
   end;

   if Result then begin
      PerspOptionsForm.ChangeSettings;
   end;
end;



procedure TPerspOptions.ChangeSettings;
var
   i : integer;
begin
   with PerfForm do begin
      //View3D := PerspOptionsForm.View3D;
      View3D.PersOpts.PerpsectiveStereo := tPerpsectiveStereo(RadioGroup2.ItemIndex);
      View3D.PersOpts.WhichPerspective := tPerspectiveType(ComboBox1.ItemIndex);
      View3D.PersOpts.LabelPerspectiveViewPort := CheckBox1.Checked;
      View3D.PersOpts.TitlePerspectiveViewPort := CheckBox4.Checked;
      MDdef.AviationDangerColors := CheckBox5.Checked;
      View3D.PersOpts.OutlineCrests := CheckBox6.Checked;
      MDDef.DrapeExactly := CheckBox10.Checked;

        {$IfDef ExFly}
        {$Else}
           if CheckBox14.Checked then View3D.FlyOpts.NumTargetViews := 2
           else View3D.FlyOpts.NumTargetViews := 1;
           View3D.FlyOpts.ShowFlyThroughRoute := CheckBox11.Checked;
           View3D.FlyOpts.FlySideBySide := CheckBox12.Checked;
           if CheckBox15.Checked then View3D.FlyOpts.NumFlyDrapes := 2 else View3D.FlyOpts.NumFlyDrapes := 1;
           View3D.FlyOpts.LiveFlyMovie := CheckBox27.Checked;
           CheckEditString(Edit45.Text,View3D.FlyOpts.FlyHeight);
           CheckEditString(Edit24.Text,View3D.FlyOpts.FlyThroughWidth);
           CheckEditString(Edit25.Text,View3D.FlyOpts.FlyThroughHeight);
           if (View3D.FlyOpts.FlyThroughWidth mod 16 <> 0) or (View3D.FlyOpts.FlyThroughHeight mod 16 <> 0) then begin
               View3D.FlyOpts.FlyThroughWidth := 16 * (succ(View3D.FlyOpts.FlyThroughWidth div 16));
               View3D.FlyOpts.FlyThroughHeight := 16 * (succ(View3D.FlyOpts.FlyThroughHeight div 16));
           end;
           CheckEditString(Edit45.Text,MDDef.FlyOptions.FlyHeight);
           CheckEditString(Edit16.Text,View3D.FlyOpts.FlySceneSeparation);
           CheckEditString(Edit22.Text,View3D.FlyOpts.TargetFOV1);
           CheckEditString(Edit23.Text,View3D.FlyOpts.TargetFOV2);
        {$EndIf}

        MDdef.AutomaticNewMovieNames := CheckBox13.Checked;
        View3D.PersOpts.ViewShedFanWithPerspective := CheckBox16.Checked;
        View3D.PersOpts.PersVaryResolutionAlongRadial := CheckBox17.Checked;
        View3D.PersOpts.CloudBackground := CheckBox18.Checked;
        View3D.PersOpts.SaveAsProgramDefaults := CheckBox20.Checked;
        MDDef.ShowLocationSensitivity := CheckBox21.Checked;

        MDDef.SaveLiveFlying := CheckBox28.Checked;

        MDDef.PerspOpts.UsersSky := CheckBox52.Checked;
        MDDef.ConvergingViews := CheckBox99.Checked;
        View3D.PersOpts.NapEarth := (RadioGroup1.ItemIndex = 0);
        CheckEditString(Edit1.Text,View3D.PersOpts.PersDistBetweenProf);
        CheckEditString(Edit2.Text,View3D.PersOpts.PersMeshSpace);
        CheckEditString(Edit3.Text,View3D.PersOpts.PersFirstProfile);

        CheckEditString(Edit4.Text,MDDef.FlyCrossTrackheight);
        CheckEditString(Edit5.Text,MDDef.FlyCrossTrackDistance);

        CheckEditString(Edit30.Text,MDDef.NumSideProfiles);
        CheckEditString(Edit39.Text,MDDef.NumSideProfiles);
        CheckEditString(Edit40.Text,MDDef.SideProfileSpacing);

        CheckEditString(Edit46.Text,MDDef.PerspOpts.HorizLabelInc);
        CheckEditString(Edit47.Text,MDDef.PerspOpts.VertLabelInc);

        MDDef.ShowCrossTrackProfiles := CheckBox7.Checked;
        MDDef.ShowAlongTrackProfiles := CheckBox22.Checked;

        CheckEditString(Edit7.Text,View3D.PersOpts.PersObsUp);
        CheckEditString(Edit8.Text,View3D.RadialPointSpacing[1]);
        CheckEditString(Edit9.Text,View3D.PersOpts.PerspAbsElev);
        CheckEditString(Edit10.Text,View3D.PersOpts.PerspAnaglyphShift);
        CheckEditString(Edit11.Text,View3D.PersOpts.PerspAnaglyphSeperate);
        CheckEditString(Edit12.Text,View3D.ViewDepth);

        CheckEditString(Edit14.Text,View3D.PersOpts.CrestSeparator);

        CheckEditString(Edit15.Text,Pitch);
        View3D.MinPitch := Pitch - View3D.ViewVFOV * 0.5;
        View3D.MaxPitch := Pitch + View3D.ViewVFOV * 0.5;

        CheckEditString(Edit17.Text,View3D.ViewAzimuth);
        View3D.SaveName := Edit18.Text;

        {$IfDef RecordPerspectiveProblems}
           WriteLineToDebugFile('Before checkviewsize HFOV=' + RealToString(View3D.ViewHFOV,-12,2 )+  '  VFOV=' + RealToString(View3D.ViewVFOV,-12,2));
        {$EndIf}

        {$IfDef RecordPerspectiveProblems}
           WriteLineToDebugFile(' HFOV=' + RealToString(View3D.ViewHFOV,-12,2));
           WriteLineToDebugFile(' VFOV=' + RealToString(View3D.ViewVFOV,-12,2));
           WriteLineToDebugFile(' Pitch=' + RealToString(Pitch,-12,2),true);
           WriteLineToDebugFile(' Pers vert angle range: ' + RealToString(View3D.MinPitch,8,2) + RealToString(View3D.MaxPitch,8,2));
        {$EndIf}

        CheckEditString(Edit31.Text,MDdef.MaxDrapeXSize);
        CheckEditString(Edit32.Text,MDdef.MaxDrapeYSize);
        CheckEditString(Edit35.Text,MDDef.PerspectiveSkyAngle);

        if (MDdef.MaxDrapeXSize > 6000) then MDdef.MaxDrapeXSize := 6000;
        if (MDdef.MaxDrapeYSize > 6000) then MDdef.MaxDrapeYSize := 6000;

        for i := 1 to 4 do begin
           View3D.PersOpts.PersVaryResRanges[i] := StrToInt(StringGrid1.Cells[0,i]);
           View3D.PersOpts.PersVaryResFactors[i] := StrToInt(StringGrid1.Cells[1,i]);
        end;

        CheckEditString(Edit41.Text,MDDef.ShiftGazPeaksDist);
        CheckEditString(Edit42.Text,MDDef.GazPeakObsUp);

        MDDef.AutoLabelGaz := CheckBox23.Checked;
        MDDef.ShiftGazPeaks := CheckBox24.Checked;
        MDDef.GazMarkPeaksPerspective := CheckBox25.Checked;
        MDDef.GazMarkPeaksPerpsOnMap := CheckBox26.Checked;


        CheckViewSize;
        View3D.DEMOnView := DEMUsed;

        View3D.SetSize;

        {$IfDef RecordPerspectiveProblems}
            WriteLineToDebugFile('SetPerspectiveOptions out, width=' + IntToStr(View3D.PersOpts.PersWidth) + '    & height=' + IntToStr(View3D.PersOpts.PersHeight),true);
            WriteLineToDebugFile(' HFOV=' + RealToString(View3D.ViewHFOV,-12,2));
            WriteLineToDebugFile(' VFOV=' + RealToString(View3D.ViewVFOV,-12,2));
            WriteLineToDebugFile(' Pitch=' + RealToString(Pitch,-12,2));
            WriteLineToDebugFile(' Pers vert angle range: ' + RealToString(View3D.MinPitch,8,2) + RealToString(View3D.MaxPitch,8,2));
            WriteLineToDebugFile('');
        {$EndIf}
   end;
end;



procedure TPerspOptions.CheckViewSize;
begin
   if StillSettingUp then exit;

   with View3d.PersOpts do begin
     if (View3D.PersSize = psPerpsective) then begin
        CheckEditString(Edit13.Text,MDDef.PersVFOV);
        CheckEditString(Edit6.Text,MDDef.PersHFOV);
        View3D.ViewVFOV := MDDef.PersVFOV;
        View3D.ViewHFOV := MDDef.PersHFov;
        CheckEditString(Edit20.Text,PersWidth);
        CheckEditString(Edit21.Text,PersHeight);
        Edit19.Text := RealToString(MDDef.PersHFOV/MDDef.PersVFOV * PersHeight/PersWidth,-8,2);
     end;

     if (View3D.PersSize = psPanorama) then begin
        CheckEditString(Edit28.Text,MDDef.PanHFOV);
        CheckEditString(Edit37.Text,MDDef.PanVFOV);
        View3D.ViewVFOV := MDDef.PanVFOV;
        View3D.ViewHFOV := MDDef.PanHFov;
        CheckEditString(Edit27.Text,PanWidth);
        CheckEditString(Edit26.Text,PanHeight);
        Edit29.Text := RealToString(MDDef.PanHFOV/MDDef.PanVFOV * PanHeight/PanWidth,-8,2);
     end;

     {$IfDef ExFly}
     {$Else}
        if (View3D.PersSize = psFlying) then begin
           CheckEditString(Edit36.Text,View3D.FlyOpts.FlyHFOV);
           CheckEditString(Edit38.Text,View3D.FlyOpts.FlyVFOV);
           CheckEditString(Edit24.Text,View3D.FlyOpts.FlyThroughWidth);
           CheckEditString(Edit25.Text,View3D.FlyOpts.FlyThroughHeight);
           Edit30.Text := RealToString(View3D.FlyOpts.FlyHFOV/View3D.FlyOpts.FlyVFOV * View3D.FlyOpts.FlyThroughHeight/View3D.FlyOpts.FlyThroughWidth,-8,2);
           View3D.ViewVFOV := View3D.FlyOpts.FlyVFOV;
           View3D.ViewHFOV := View3D.FlyOpts.FlyHFOV;
        end;
     {$EndiF}

     CheckEditString(Edit15.Text,Pitch);
     View3D.MinPitch := Pitch - View3D.ViewVFOV * 0.5;
     View3D.MaxPitch := Pitch + View3D.ViewVFOV * 0.5;
     Edit43.Text := RealToString(View3D.MinPitch,-12,2);
     Edit44.Text := RealToString(View3D.MaxPitch,-12,2);
   end;
end;


procedure TPerspOptions.ShowOptions;
var
   i : integer;
begin
   with View3D.PersOpts do begin
      ComboBox1.Text := ComboBox1.Items[ord(WhichPerspective)];
      ComboBox1.ItemIndex := ord(WhichPerspective);
      ComboBox2.Text := ComboBox2.Items[ord(MDdef.CurvAlg)];
      CheckBox1.Checked := LabelPerspectiveViewPort;
      if NapEarth then RadioGroup1.ItemIndex := 0 else RadioGroup1.ItemIndex := 1;
      RadioGroup2.ItemIndex := ord(PerpsectiveStereo);

      {$IfDef ExFly}
      {$Else}
         CheckBox3.Checked := View3D.FlyOpts.ShowFlyScene;
         CheckBox11.Checked := View3D.FlyOpts.ShowFlyThroughRoute;
         CheckBox12.Checked := View3D.FlyOpts.FlySideBySide;
         CheckBox14.Checked := View3D.FlyOpts.NumTargetViews = 2;
         CheckBox15.Checked := View3D.FlyOpts.NumFlyDrapes = 2;
         Edit16.Text := RealToString(View3D.FlyOpts.FlySceneSeparation,-12,0);
         Edit22.Text := RealToString(View3D.FlyOpts.TargetFOV1,-12,-2);
         Edit23.Text := RealToString(View3D.FlyOpts.TargetFOV2,-12,-2);

         Edit24.Text := IntToStr(View3D.FlyOpts.FlyThroughWidth);
         Edit25.Text := IntToStr(View3D.FlyOpts.FlyThroughHeight);
         Edit36.Text := RealToString(View3D.FlyOpts.FlyHFOV,-8,-2);
         Edit38.Text := RealToString(View3D.FlyOpts.FlyVFOV,-12,-2);
      {$EndIf}

      CheckBox4.Checked := TitlePerspectiveViewPort;
      CheckBox5.Checked := MDdef.AviationDangerColors;
      CheckBox6.Checked := OutlineCrests;
      CheckBox10.Checked := MDDef.DrapeExactly;
      CheckBox13.Checked := MDdef.AutomaticNewMovieNames;
      CheckBox16.Checked := ViewShedFanWithPerspective;
      CheckBox17.Checked := PersVaryResolutionAlongRadial;
      CheckBox18.Checked := CloudBackground;
      CheckBox20.Checked := SaveAsProgramDefaults;
      CheckBox21.Checked := MDDef.ShowLocationSensitivity;
      CheckBox52.Checked := MDDef.PerspOpts.UsersSky;

      ColorLineWidthBitBtn(BitBtn1,CrestColor,CrestLineWidth);
      ColorBitBtn(BitBtn4,MDDef.PerspOpts.rgbtSky);
      ColorLineWidthBitBtn(BitBtn11,MDDef.FightLineColor,MDDef.FlightLineSize);

      StringGrid1.Cells[0,0] := 'Range';
      StringGrid1.Cells[1,0] := 'Factor';
      for i := 1 to 4 do begin
         StringGrid1.Cells[0,i] := IntToStr(PersVaryResRanges[i]);
         StringGrid1.Cells[1,i] := IntToStr(PersVaryResFactors[i]);
      end;
      Label24.Caption := ExtractFileName(CloudBitmapName);

      ElevationsChanged := false;

      Edit1.Text := IntToStr(PersDistBetweenProf);
      Edit2.Text := IntToStr(PersMeshSpace);
      Edit3.Text := IntToStr(PersFirstProfile);
      Edit6.Text := RealToString(MDDef.PersHFOV,-8,-2);
      Edit7.Text := RealToString(PersObsUp,-8,-2);
      Edit10.Text := IntToStr(PerspAnaglyphShift);
      Edit11.Text := IntToStr(PerspAnaglyphSeperate);
      Edit13.Text := RealToString(MDDef.PersVFOV,-12,-2);
      Edit14.Text := IntToStr(CrestSeparator);
      Edit15.Text := RealToString(0.5*(View3D.MaxPitch + View3D.MinPitch),-8,-2);
      Edit20.Text := IntToStr(PersWidth);
      Edit21.Text := IntToStr(PersHeight);

      Edit26.Text := IntToStr(PanHeight);
      Edit27.Text := IntToStr(PanWidth);
      Edit28.Text := RealToString(MDDef.PanHFOV,-8,-2);;
      Edit31.Text := IntToStr(MDdef.MaxDrapeXSize);
      Edit32.Text := IntToStr(MDdef.MaxDrapeYSize);
      Edit37.Text := RealToString(MDDef.PanVFOV,-12,-2);

      Edit15Change(Nil);
      Label31.Caption := 'View from ' + LatLongDegreeToString(View3D.ViewerLat,View3D.ViewerLong,MDDef.OutPutLatLongMethod);
   end;
end;


procedure TPerspOptions.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\tbme8izy.htm');
end;


procedure TPerspOptions.OKBtnClick(Sender: TObject);
begin
   {$IfDef PerspectiveProblems} WriteLineToDebugFile('TPerspOptions.OKBtnClick');  {$EndIf}
   MDDef.PerspOpts.PersWidth := View3D.PersOpts.PersWidth;
   MDDef.PerspOpts.PersHeight := View3D.PersOpts.PersHeight;
end;

procedure TPerspOptions.Button1Click(Sender: TObject);
begin
   if View3D.PersOpts.WhichPerspective = BMPPerspective then ChangeReflectanceOptions(View3D.DrapingMaps[View3D.DrapeMapUsing])
   else ChangeReflectanceOptions(DEMGlb[View3d.DEMonView].SelectionMap);
end;


procedure TPerspOptions.ComboBox1Change(Sender: TObject);
var
   i : integer;
begin
   for i := 0 to pred(ComboBox1.Items.Count) do
      if ComboBox1.Text = ComboBox1.Items[i] then
         View3D.PersOpts.WhichPerspective := tPerspectiveType(i);
   Update;
end;


procedure TPerspOptions.CheckBox22Click(Sender: TObject);
begin
   Update;
end;

procedure TPerspOptions.CheckBox2Click(Sender: TObject);
begin
   MDDef.PerspOpts.NoVE := not CheckBox2.Checked;
end;


procedure TPerspOptions.Update;
begin
   Button1.Enabled := ComboBox1.ItemIndex in [2,3];
   Edit1.Enabled := ComboBox1.ItemIndex in [0,1];
   Edit2.Enabled := ComboBox1.ItemIndex in [0,1];

   CheckBox99.Enabled := RadioGroup2.ItemIndex in [2];
   Edit7.Enabled := RadioGroup1.ItemIndex = 0;
   Label7.Enabled := RadioGroup1.ItemIndex = 0;
   Edit9.Enabled := RadioGroup1.ItemIndex = 1;
   Label9.Enabled := RadioGroup1.ItemIndex = 1;

   Edit14.Enabled := CheckBox6.Checked;
   BitBtn1.Enabled := CheckBox6.Checked;
   BitBtn4.Enabled := CheckBox52.Checked;
   BitBtn2.Enabled := CheckBox18.Checked;
   Label24.Enabled := CheckBox18.Checked;
   BitBtn3.Enabled := NumDEMDataSetsOpen > 2;

   Edit10.Enabled := RadioGroup2.ItemIndex = 1;
   Label2.Enabled := RadioGroup2.ItemIndex = 1;

   Edit11.Enabled := RadioGroup2.ItemIndex in [2,3];
   Label3.Enabled := RadioGroup2.ItemIndex in [2,3];

   Edit4.Enabled := CheckBox7.Checked;
   Edit5.Enabled := CheckBox7.Checked;
   Edit39.Enabled := CheckBox22.Checked;
   Edit40.Enabled := CheckBox22.Checked;
   Label4.Enabled := CheckBox7.Checked;
   Label5.Enabled := CheckBox7.Checked;
   Label30.Enabled := CheckBox22.Checked;
   Label35.Enabled := CheckBox22.Checked;
end;


procedure TPerspOptions.Edit9Change(Sender: TObject);
begin
   ElevationsChanged := true;
end;

procedure TPerspOptions.Edit7Change(Sender: TObject);
begin
   ElevationsChanged := true;
end;

procedure TPerspOptions.Button2Click(Sender: TObject);
begin
   SetPerspectiveDefaults(View3D.PersOpts);
   {$IfDef ExFly}
   {$Else}
      SetFlyDefaults;
      View3D.FlyOpts := MDDef.FlyOptions;
   {$EndIf}
   ShowOptions;
end;

procedure TPerspOptions.CheckBox4Click(Sender: TObject);
begin
   Update;
end;

procedure TPerspOptions.CheckBox5Click(Sender: TObject);
begin
   Update;
end;

procedure TPerspOptions.ComboBox2Change(Sender: TObject);
var
   i : integer;
begin
   for i := 0 to pred(ComboBox2.Items.Count) do
      if ComboBox2.Text = ComboBox2.Items[i] then
         MDDef.CurvAlg := tVerticalCurvAlg(i);
end;

procedure TPerspOptions.CheckBox6Click(Sender: TObject);
begin
   Update;
end;

procedure TPerspOptions.CheckBox7Click(Sender: TObject);
begin
   Update;
end;

procedure TPerspOptions.RadioGroup1Click(Sender: TObject);
begin
   Update;
end;

procedure TPerspOptions.RadioGroup2Click(Sender: TObject);
begin
   Update;
end;

procedure TPerspOptions.RedrawSpeedButton12Click(Sender: TObject);
begin
   ChangeSettings;
   PerfForm.DrawPerspective(false);
end;

procedure TPerspOptions.BitBtn4Click(Sender: TObject);
begin
   QueryColor(BitBtn4,MDDef.PerspOpts.rgbtSky);
end;


procedure TPerspOptions.FormCreate(Sender: TObject);
begin
   PlaceFormAtMousePosition(Self);
end;

procedure TPerspOptions.BitBtn11Click(Sender: TObject);
begin
   PickLineSizeAndColor('flight route',BitBtn11,MDDef.FightLineColor,MDDef.FlightLineSize);
end;

procedure TPerspOptions.BitBtn13Click(Sender: TObject);
begin
   EditMyFont(MDDef.PerspFont);
end;

procedure TPerspOptions.BitBtn1Click(Sender: TObject);
begin
   PickLineSizeAndColor('Ridge lines',BitBtn1,View3D.PersOpts.CrestColor,View3D.PersOpts.CrestLineWidth);
end;

procedure TPerspOptions.CheckBox52Click(Sender: TObject);
begin
   Update;
end;

procedure TPerspOptions.Edit15Change(Sender: TObject);
begin
   if (Edit13.Text = '') or (Edit20.Text = '') then exit;
   CheckViewSize;
end;

procedure TPerspOptions.Edit17Change(Sender: TObject);
begin
   Edit15Change(Sender);
end;

procedure TPerspOptions.Edit12Change(Sender: TObject);
begin
   if (CurrentMode in [psFlying]) then CheckEditString(Edit12.Text,MDdef.PerspOpts.FlyDepth);
end;

procedure TPerspOptions.Edit13Change(Sender: TObject);
begin
   Edit15Change(Sender);
end;

procedure TPerspOptions.Edit20Change(Sender: TObject);
begin
   Edit15Change(Sender);
end;

procedure TPerspOptions.Edit21Change(Sender: TObject);
begin
   Edit15Change(Sender);
end;

procedure TPerspOptions.Edit6Change(Sender: TObject);
begin
   Edit15Change(Sender);
end;

procedure TPerspOptions.BitBtn2Click(Sender: TObject);
begin
   if GetFileFromDirectory('clouds','*.jpg',CloudBitmapName) then
      Label24.Caption := ExtractFileName(CloudBitmapName);
end;

procedure TPerspOptions.CheckBox18Click(Sender: TObject);
begin
   Update;
end;

procedure TPerspOptions.BitBtn3Click(Sender: TObject);
begin
   GetDEM(DEMUsed);
end;

procedure TPerspOptions.Edit24Change(Sender: TObject);
begin
   Edit15Change(Sender);
end;


procedure TPerspOptions.Edit25Change(Sender: TObject);
begin
   Edit15Change(Sender);
end;


procedure TPerspOptions.Edit28Change(Sender: TObject);
begin
   Edit15Change(Sender);
end;

procedure TPerspOptions.BitBtn5Click(Sender: TObject);
begin
   demfanparams.GetFanParameters;
end;


procedure TPerspOptions.BitBtn7Click(Sender: TObject);
begin
   GetLatLn.GetLatLongDefault(DEMGlb[DEMused].SelectionMap.MapDraw.PrimMapProj,'Viewer location',View3D.ViewerLat,View3D.ViewerLong);
   DEMGlb[DEMused].SelectionMap.OutlinePerspectiveView( View3D.ViewHFOV,View3D.ViewerLat,View3D.ViewerLong,View3D.ViewDepth,View3D.ViewAzimuth,pmCopy);
end;


procedure TPerspOptions.BitBtn6Click(Sender: TObject);
var
   Distance,Azimuth : float64;
begin
   CheckEditString(Edit33.Text,Azimuth);
   CheckEditString(Edit34.Text,Distance);
   VincentyPointAtDistanceBearing(View3D.ViewerLat,View3D.ViewerLong,Distance,Azimuth,View3D.ViewerLat,View3D.ViewerLong);
   DEMGlb[DEMused].GetElevFromLatLongDegree(View3D.ViewerLat,View3D.ViewerLong,View3D.ElevObs);
   Label31.Caption := 'View from ' + LatLongDegreeToString(View3D.ViewerLat,View3D.ViewerLong,MDDef.OutPutLatLongMethod);
   Edit33.Text := '';
   Edit34.Text := '';
end;


procedure TPerspOptions.Edit33Change(Sender: TObject);
begin
   BitBtn6.Enabled := (Edit33.Text <> '') and (Edit34.Text <> '');
end;


procedure TPerspOptions.Edit34Change(Sender: TObject);
begin
   Edit33Change(Sender);
end;


procedure TPerspOptions.Edit37Change(Sender: TObject);
begin
   Edit15Change(Sender);
end;

procedure TPerspOptions.BitBtn8Click(Sender: TObject);
var
   fName : PathStr;
   f     : file of tView3D;
begin
   FName := ProjectDir;
   if GetFileNameDefaultExt(SPVF,'Perspective view|*.PER',FName,false) then begin
      assignFile(f,fName);
      rewrite(f);
      Write(f,View3D);
      closeFile(f);
   end;
end;


procedure TPerspOptions.BitBtn9Click(Sender: TObject);
var
   fName : PathStr;
   f     : file of tView3D;
begin
   FName := ProjectDir;
   if GetFileFromDirectory(SPVF,'*.PER',FName) then begin
      assignFile(f,fName);
      reset(f);
      read(f,View3D);
      closeFile(f);
      ShowOptions;
   end;
end;


procedure TPerspOptions.BitBtn10Click(Sender: TObject);
var
  HFOV,Azimuth : float64;
begin
    if CheckBox19.Checked then DEMGlb[DEMUsed].SelectionMap.DoFastMapRedraw;
    CheckEditString(Edit6.Text,HFOV);
    CheckEditString(Edit12.Text,View3D.SectLen);
    CheckEditString(Edit17.Text,Azimuth);
    DEMGlb[DEMUsed].SelectionMap.OutlinePerspectiveView(HFOV,View3D.ViewerLat,View3D.ViewerLong,View3D.SectLen,Azimuth,pmCopy);
    DEMGlb[DEMUsed].SelectionMap.BringToFront;
end;


initialization
finalization
  {$IfDef PerspectiveProblems} WriteLineToDebugFile('PerspectiveProblems active in demperop'); {$EndIf}
  {$IfDef RecordClosingProblems} WriteLineToDebugFile('Closing demperop'); {$EndIf}
end.


