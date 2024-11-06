unit Demweapn;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   //{$Define RecordFanOptions}
{$EndIf}


interface

uses
   Windows, Classes, Graphics, Forms, Controls, Buttons, StdCtrls, ExtCtrls, SysUtils,
   Petmar_types,PETMAR,DEMDefs, ColorGrd, Dialogs, ToolWin, ComCtrls;


type
  TPickWeapon = class(TForm)
    Panel1: TPanel;
    Label11: TLabel;
    Edit9: TEdit;
    Panel2: TPanel;
    CancelBtn: TBitBtn;
    OKBtn: TBitBtn;
    HelpBtn: TBitBtn;
    Panel3: TPanel;
    Label1: TLabel;
    Label5: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    BitBtn1: TBitBtn;
    Edit6: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit1: TEdit;
    Edit5: TEdit;
    CheckBox4: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox1: TCheckBox;
    RadioGroup3: TRadioGroup;
    Edit10: TEdit;
    Edit7: TEdit;
    Label6: TLabel;
    Label7: TLabel;
    RadioGroup2: TRadioGroup;
    RadioGroup1: TRadioGroup;
    Label12: TLabel;
    Edit11: TEdit;
    CheckBox3: TCheckBox;
    BitBtn2: TBitBtn;
    BitBtn4: TBitBtn;
    Label13: TLabel;
    Edit12: TEdit;
    BitBtn5: TBitBtn;
    CheckBox2: TCheckBox;
    RadioGroup4: TRadioGroup;
    Label10: TLabel;
    CheckBox5: TCheckBox;
    BitBtn3: TBitBtn;
    procedure HelpBtnClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure RadioGroup3Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure Edit9Change(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    JustAFan : boolean;
    RangeFactor,theLength : float64;
    WeaponsFan : tWeaponsFan;
  end;


function GetWeaponParameters(DEMonMap : integer; var inWeaponsFan : tWeaponsFan; LimitOptions : boolean = false;  AmbushMovie : boolean = false; BackBitmap : tMyBitmap = Nil; ItsJustAFan : boolean = true;  NameEdit : boolean = true; RouteLength : float64 = 0) : boolean;

implementation

{$R *.DFM}

uses
   {$IfDef ExAmbush}
   {$Else}
   DEMAmbushParams,
   {$EndIf}
   DEMCoord,DEMRange,DEMCurvature,
   Demdef_routines,
   PETMath,petdbutils,
   DEMFanParams, get_db_coloring;


function GetWeaponParameters(DEMonMap : integer; var inWeaponsFan : tWeaponsFan; LimitOptions : boolean = false; AmbushMovie : boolean = false; BackBitmap : tMyBitmap = Nil; ItsJustAFan : boolean = true;  NameEdit : boolean = true; RouteLength : float64 = 0) : boolean;
var
   xg,yg  : float64;
   PickWeapon: TPickWeapon;
begin
   {$If Defined(RecordLOSProblems) or Defined(RecordFanOptions)} WriteLineToDebugFile('GetWeaponParameters in, fan at ' + LatLongDegreeToString(inWeaponsFan.W_Lat,inWeaponsFan.W_Long)); {$EndIf}

   GetWeaponParameters := false;
   if (DEMonMap = 0) then exit;
   PickWeapon := tPickWeapon.Create(Application);
   with PickWeapon,WeaponsFan do  begin
      WeaponsFan := inWeaponsFan;
      JustAFan := ItsJustAFan;
      if (W_Lat < 100) then begin
         DEMGlb[DEMonMap].LatLongDegreetoDEMGrid(W_Lat,W_Long,xg,yg);
         Label1.Caption := 'Observer: ' +  DEMGlb[DEMonMap].DEMLocationString(xg,yg);
      end
      else Label1.Caption := '';

      if ObserverTerrainHug then RadioGroup1.ItemIndex := 1 else RadioGroup1.ItemIndex := 0;
      RadioGroup1Click(Nil);

      if TargetTerrainHug then RadioGroup2.ItemIndex := 1 else RadioGroup2.ItemIndex := 0;
      RadioGroup2Click(Nil);

      RadioGroup4.ItemIndex := ord(MDDef.FanPickMode);

      Edit2.Text := RealToString(W_Range*RangeFactor,-18,-4);
      Edit3.Text := RealToString(W_Up,-18,-4);
      Edit4.Text := RealToString(W_TargetUp,-18,-4);
      Edit6.Text := Fan_Name;
      Edit1.Text := RealToString(MDDef.StartFanAngle,-18,-4);
      Edit5.Text := RealToString(MDDef.EndFanAngle,-18,-4);
      Edit9.Text := IntToStr(MDDef.WeaponRouteSeparation);
      Edit7.Text := RealToString(UpAngle,-18,-4);
      Edit10.Text := RealToString(DownAngle,-18,-4);
      Edit11.Text := RealToString(MDDef.DefWeaponsMinRange,-18,-4);
      Edit12.Text := IntToStr(MDdef.FanOpacity);
      CheckBox3.Checked := MDDef.OutlineFans;
      ColorLineWidthBitBtn(BitBtn2,MDDef.FanOutLineColor,MDDef.FanOutLineWidth);
      BitBtn2.Enabled := CheckBox3.Checked;

      CheckBox2.Checked := MDDef.UseVegInFans;
      {$IFDef ExVegDensity}
         CheckBox2.Enabled := false;
      {$Else}
         CheckBox2.Enabled := (DEMonMap <> 0) and (DEMGlb[DEMonMap].VegGrid[1] <> 0);
      {$EndIf}
      CheckBox4.Checked := MDDef.ReportFanCoverage;
      CheckBox5.Checked := MDDef.DrawRangeCircles;
      CheckBox6.Checked := MDDef.GraphFanCoverage;
      Panel1.Visible := AmbushMovie;
      Label8.Visible := not AmbushMovie;
      Label9.Visible := not AmbushMovie;
      Edit1.Visible := not AmbushMovie;
      Edit5.Visible := not AmbushMovie;
      Label5.Visible := not AmbushMovie;
      Edit6.Visible := not AmbushMovie;

      if AmbushMovie then begin
         ClientHeight := Panel1.Height + Panel2.Height + Panel3.Height;
         Panel2.Top := Panel1.Height + Panel3.Height;
         theLength := RouteLength;
         Label10.Caption := RealToString(0.001 * TheLength,-12,-3) + ' km and ' + IntToStr(Round(RouteLength / MDDef.WeaponRouteSeparation)) + ' fans';
      end
      else begin
         CheckBox6.Enabled := false;
         ClientHeight := Panel2.Height + Panel3.Height;
         RadioGroup1.Enabled := false;
         ClientHeight := Panel2.Height + Panel3.Height;
         Panel2.Top := Panel3.Height;
      end;

      CheckBox1.Checked := MDDef.SaveFanRadiaProfiles;
      CheckBox1.Enabled := MDDef.wf.FanMethod in [fmFanRadials,fmRadialIHS];
      if LimitOptions then begin
         Label1.Visible := false;
         Label3.Enabled := false;
         Label4.Enabled := false;
         Edit3.Enabled := false;
         Edit4.Enabled := false;
         Edit7.Enabled := false;
         Edit10.Enabled := false;
         CheckBox1.Visible := false;
      end;
      Edit5.Enabled := NameEdit;

      if (PickWeapon.ShowModal <> mrCancel) then begin
         GetWeaponParameters := true;
         CheckEditString(Edit2.Text,W_Range);
         W_Range := W_Range / RangeFactor;
         CheckEditString(Edit3.Text,W_Up);
         CheckEditString(Edit4.Text,W_TargetUp);
         CheckEditString(Edit1.Text,WeaponsFan.StartAngle);
         CheckEditString(Edit5.Text,WeaponsFan.EndAngle);
         CheckEditString(Edit7.Text,UpAngle);
         CheckEditString(Edit10.Text,DownAngle);
         CheckEditString(Edit11.Text,MDDef.DefWeaponsMinRange);
         CheckEditString(Edit12.Text,MDdef.FanOpacity);

         MDdef.StartFanAngle := round(WeaponsFan.StartAngle);
         MDdef.EndFanAngle := round(WeaponsFan.EndAngle);
         MDDef.FanUpAngle := UpAngle;
         MDDef.FanDownAngle := DownAngle;

         Fan_Name := Edit6.Text;
         MDDef.MaskObsRange := W_Range;
         MDDef.SaveFanRadiaProfiles := CheckBox1.Checked;
         MDDef.UseVegInFans := CheckBox2.Checked;
         MDDef.OutlineFans := CheckBox3.Checked;

         MDDef.FanPickMode := tFanPickMode(RadioGroup4.ItemIndex);

         MDDef.ReportFanCoverage := CheckBox4.Checked;
         MDDef.DrawRangeCircles := CheckBox5.Checked;

         MDDef.GraphFanCoverage := CheckBox6.Checked;
         TargetTerrainHug := (RadioGroup2.ItemIndex = 1);
         ObserverTerrainHug := (RadioGroup1.ItemIndex = 1);
         if MDDef.DefaultTargetTerrainHug then MDDef.TargetAboveGround := W_TargetUp
         else MDDef.DefaultTargetASL := round(W_TargetUp);

         if MDDef.DefaultObserverTerrainHug then MDdef.ObsAboveGround := W_Up
         else MDdef.ObsAboveGround := round(W_Up);
         inWeaponsFan := WeaponsFan;
      end;
   end;
   {$If Defined(RecordLOSProblems) or Defined(RecordFanOptions)} WriteLineToDebugFile('GetWeaponParameters out, fan at ' + LatLongDegreeToString(inWeaponsFan.W_Lat,inWeaponsFan.W_Long)); {$EndIf}
end;


procedure TPickWeapon.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\tbme5lrn.htm');
end;


procedure TPickWeapon.Button1Click(Sender: TObject);
begin
   EditRangeCircles;
end;

procedure TPickWeapon.CancelBtnClick(Sender: TObject);
begin
   Close;
end;

procedure TPickWeapon.CheckBox3Click(Sender: TObject);
begin
   BitBtn2.Enabled := CheckBox3.Checked;
end;

procedure TPickWeapon.Edit9Change(Sender: TObject);
begin
   CheckEditString(Edit9.Text,MDDef.WeaponRouteSeparation);
    Label10.Caption := RealToString(0.001 * TheLength,-12,-3) + ' km and ' + IntToStr(Round(TheLength /MDDef.WeaponRouteSeparation)) + ' fans';
end;

procedure TPickWeapon.BitBtn1Click(Sender: TObject);
begin
   if JustAFan then begin
      GetFanParameters;
      InitializeWeaponsFanColors(WeaponsFan);
   end
   else {$IfDef ExAmbush} {$Else} GetAmbushParameters {$EndIf};
   CheckBox1.Enabled := MDDef.wf.FanMethod in [fmRadialIHS, fmFanRadials];
end;


procedure TPickWeapon.BitBtn2Click(Sender: TObject);
begin
   PickLineSizeAndColor('Fan outlines',BitBtn2,MDDef.FanOutLineColor,MDDef.FanOutLineWidth);
end;

procedure TPickWeapon.BitBtn3Click(Sender: TObject);
begin
   Edit1.Text := '0';
   Edit5.Text := '360';
   Edit7.Text := '89';
   Edit10.Text := '-89';
end;

procedure TPickWeapon.BitBtn4Click(Sender: TObject);
begin
   GetDOSPath('Stored fans',SaveViewshedDir);
end;

procedure TPickWeapon.FormCreate(Sender: TObject);
var
   ColorDefTable : tColorTableDefinitions;
begin
   if (Sender <> Nil) then begin
      PlaceFormAtMousePosition(Self);
      RadioGroup3.ItemIndex := ord(MDDef.RangeCircleUnit);
   end;
   DefineColorTableValues(MDDef.ViewshedPaletteName,0,60,ColorDefTable);
   BitBtn5.Glyph := MakeColorScaleBitmap(60,14,MDDef.ViewshedColorScheme,ColorDefTable);
   Petmar.PlaceFormAtMousePosition(Self);
end;

procedure TPickWeapon.RadioGroup2Click(Sender: TObject);
var
   TStr : string3;
begin
   MDDef.DefaultTargetTerrainHug := RadioGroup2.ItemIndex = 1;
   if MDDef.DefaultTargetTerrainHug then TStr := 'AGL'
   else TStr := 'ASL';
   Label4.Caption := 'Target ' + TStr + ' (m)';
end;

procedure TPickWeapon.RadioGroup1Click(Sender: TObject);
var
   TStr : string3;
begin
   MDDef.DefaultObserverTerrainHug := RadioGroup1.ItemIndex = 1;
   if MDDef.DefaultObserverTerrainHug then TStr := 'AGL'
   else TStr := 'ASL';
   Label3.Caption := 'Observer ' + TStr + ' (m)';
end;


procedure TPickWeapon.RadioGroup3Click(Sender: TObject);
begin
   MDDef.RangeCircleUnit := tRangeCircleUnit(RadioGroup3.ItemIndex);
   GetRangeFactor(RangeFactor);
end;


initialization
finalization
   {$IfDef RecordFanOptions} WriteLineToDebugFile('RecordFanOptions active in demweapn'); {$EndIf}
   {$IfDef RecordClosingProblems} WriteLineToDebugFile('Closing demweapn out'); {$EndIf}
end.
