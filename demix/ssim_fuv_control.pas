unit ssim_fuv_control;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{________________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems} //normally only defined for debugging specific problems
   {$IFDEF DEBUG}
      {$Define RecordFUV_SSIM}
   {$ELSE}
   {$ENDIF}
{$EndIf}



interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons;

type
  Tfuv_ssim_control = class(TForm)
    GroupBox1: TGroupBox;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox25: TCheckBox;
    CheckBox24: TCheckBox;
    CheckBox22: TCheckBox;
    BitBtn1: TBitBtn;
    BitBtn38: TBitBtn;
    CheckBox9: TCheckBox;
    CheckBox157: TCheckBox;
    BitBtn2: TBitBtn;
    BitBtn5: TBitBtn;
    CheckBox11: TCheckBox;
    BitBtn3: TBitBtn;
    Edit1: TEdit;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn38Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure HillshadeClick(Sender: TObject);
    procedure HANDClick(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
  private
    { Private declarations }
    procedure CheckParameters;
  public
    { Public declarations }
  end;


procedure FUV_SSIM_work;
procedure FUV_SSIM_Processing(Mode : Byte; Overwrite,AreasInsteadOfTiles : boolean);


implementation

{$R *.dfm}

uses
  Petmar_types,DEMDefs,demdef_routines,demix_definitions,Demix_control,DEMstat,
  toggle_db_use,
  petmar;


procedure FUV_SSIM_work;
var
  fuv_ssim_control : Tfuv_ssim_control;
begin
   fuv_ssim_control := Tfuv_ssim_control.Create(Application);
   fuv_ssim_control.Show;
end;


procedure FUV_SSIM_Processing(Mode : Byte; Overwrite,AreasInsteadOfTiles : boolean);
var
   Areas : tStringList;
begin
   GetDEMIXpaths;
   DEMIX_initialized := true;
   MDDef.DEMIX_mode := Mode;
   SetParamsForDEMIXmode;
   Areas := DEMIX_AreasWanted(not MDDef.DEMIX_all_areas);
   AreaSSIMandFUVComputations(Overwrite,AreasInsteadOfTiles,Areas);
   Areas.Destroy;
end;


procedure Spawn_FUV_SSIMProcessing;
var
   NewName : PathStr;
   i,j,k : integer;
begin
   j := 3;
   ReadDefault('Number to spawn',j);
   for k := 1 to j do begin
      i := 0;
      repeat
         inc(i);
         NewName := ExtractFilePath(application.exename) + 'md_' + IntToStr(i) + '.exe';
      until not FileExists(NewName);
      CopyFile(application.exename,NewName);
      Petmar.ExecuteFile(NewName,'-fuvssim -mode=' + IntToStr(dmFull),'');
   end;
end;


procedure Tfuv_ssim_control.FormCreate(Sender: TObject);
begin
   (*
   CheckBox12.Checked := MDDef.SSIM_elev;
   CheckBox13.Checked := MDDef.SSIM_slope;
   CheckBox14.Checked := MDDef.SSIM_ruff;
   CheckBox15.Checked := MDDef.SSIM_rri;
   Hillshade.Checked := MDDef.SSIM_hill;
   HAND.Checked := MDDef.SSIM_HAND;
   CheckBox17.Checked := MDDef.SSIM_tpi;
   CheckBox19.Checked := MDDef.SSIM_flow;
   CheckBox20.Checked := MDDef.SSIM_wet;
   CheckBox21.Checked := MDdef.SSIM_ls;
   CheckBox22.Checked := MDDef.DoSSIM;
   CheckBox24.Checked := MDDef.DoFUV;
   CheckBox25.Checked := MDDef.OpenSavedMapsFUVSSIM;
   CheckBox26.Checked := MDdef.SSIM_ProfC;
   CheckBox27.Checked := MDdef.SSIM_PlanC;
   CheckBox28.Checked := MDDef.SSIM_TangC;
   CheckBox7.Checked := MDDef.SSIM_rotor;
   CheckBox8.Checked := MDDef.SSIM_Openness;
   CheckBox10.Checked := MDDef.SSIM_ConvergeIndex;
   *)
   CheckBox5.Checked := MDDef.DEMIX_overwrite_enabled;
   CheckBox6.Checked := MDDef.DEMIX_all_areas;
   CheckBox1.Checked := true;
   CheckBox9.Checked := MDDef.ProcessLoopsForward;
   CheckBox157.Checked := MDDef.ShowWinExec;

   Edit1.Text := IntToStr(MDDef.DEMIX_Tile_Full);
   CheckBox22.Checked := MDDef.DoSSIM;
   CheckBox24.Checked := MDDef.DoFUV;
   GroupBox1.Visible := MDdef.DEMIX_AllowCoastal;
end;


procedure Tfuv_ssim_control.HANDClick(Sender: TObject);
begin
   //MDDef.SSIM_HAND := HAND.Checked;
end;

procedure Tfuv_ssim_control.HillshadeClick(Sender: TObject);
begin
  // MDDef.SSIM_hill := Hillshade.Checked;
end;


procedure Tfuv_ssim_control.BitBtn1Click(Sender: TObject);


   procedure DoOne(Mode : byte);
   begin
      if (Sender = BitBtn1) then begin
         {$IfDef RecordFUV_SSIM} WriteLineToDebugFile('DoOne Processing, mode=' + IntToStr(Mode)); {$EndIf}
         FUV_SSIM_Processing(Mode,MDDef.DEMIX_overwrite_enabled,CheckBox11.Checked);
      end
      else begin
         {$IfDef RecordFUV_SSIM} WriteLineToDebugFile('DoOne Merge, mode=' + IntToStr(Mode)); {$EndIf}
         MDDef.DEMIX_mode := Mode;
         if MDDef.DoFUV then MergeCSV(1);
         if MDDef.DoSSIM then MergeCSV(2);
      end;
   end;


begin
   {$IfDef RecordFUV_SSIM} WriteLineToDebugFile('Tfuv_ssim_control.BitBtn1Click in'); {$EndIf}
   CheckParameters;
   MDDef.DEMIX_mode := dmFull;
   PickWineContestDBLocation;
   ToggleShowProgress(false);
   Self.Visible := false;
   if CheckBox1.Checked then DoOne(dmFull);
   if MDdef.DEMIX_AllowCoastal then begin
       if CheckBox2.Checked then DoOne(dmU120);
       if CheckBox3.Checked then DoOne(dmU80);
       if CheckBox4.Checked then DoOne(dmU10);
   end;
   ToggleShowProgress(true);
   Self.Visible := true;
   {$IfDef RecordFUV_SSIM} WriteLineToDebugFile('Tfuv_ssim_control.BitBtn1Click out'); {$EndIf}
end;


procedure Tfuv_ssim_control.BitBtn2Click(Sender: TObject);
begin
   BitBtn1Click(Sender);
end;

procedure Tfuv_ssim_control.BitBtn38Click(Sender: TObject);
begin
   CheckParameters;
   SaveMDdefaults;
end;

procedure Tfuv_ssim_control.BitBtn3Click(Sender: TObject);
begin
   GetDEMIXpaths;
   VerifyRecordsToUse(DemixSettingsDir + 'demix_fuv_parameters.dbf','PARAMETER');
end;

procedure Tfuv_ssim_control.BitBtn5Click(Sender: TObject);
begin
   Spawn_FUV_SSIMProcessing;
end;

procedure Tfuv_ssim_control.CheckParameters;
begin
   CheckEditString(Edit1.Text,MDDef.DEMIX_Tile_Full);
   MDDef.DoSSIM := CheckBox22.Checked;
   MDDef.DoFUV := CheckBox24.Checked;

   MDDef.OpenSavedMapsFUVSSIM := CheckBox25.Checked;
   MDDef.DEMIX_overwrite_enabled := CheckBox5.Checked;
   MDDef.DEMIX_all_areas := CheckBox6.Checked;
   MDDef.OpenSavedMapsFUVSSIM := CheckBox25.Checked;
   MDDef.ShowWinExec := CheckBox157.Checked;
   MDDef.ProcessLoopsForward := CheckBox9.Checked;
end;


end.
