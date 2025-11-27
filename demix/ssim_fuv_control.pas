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
      //{$Define RecordFUV_SSIM}
   {$ELSE}
   {$ENDIF}
{$EndIf}



interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls;

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
    RadioGroup1: TRadioGroup;
    CheckBox7: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn38Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
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
    if MDDef.DoFUV then RadioGroup1.ItemIndex := 0
    else if MDDef.DoSSIM then RadioGroup1.ItemIndex := 1
    else if MDDef.DoPartials then RadioGroup1.ItemIndex := 2
    else if MDDef.DoCurvatures then RadioGroup1.ItemIndex := 3;

   CheckBox5.Checked := MDDef.DEMIX_overwrite_enabled;
   CheckBox6.Checked := MDDef.DEMIX_all_areas;

   CheckBox7.Checked := MDDef.DEMIX_Geo_Tiles;
   CheckBox1.Checked := true;
   CheckBox9.Checked := MDDef.ProcessLoopsForward;
   CheckBox157.Checked := MDDef.ShowWinExec;

   Edit1.Text := IntToStr(MDDef.DEMIX_Tile_Full);
   CheckBox22.Checked := MDDef.DoSSIM;
   CheckBox24.Checked := MDDef.DoFUV;
   {$IfDef IncludeCoastalDEMs}
      GroupBox1.Visible := MDdef.DEMIX_AllowCoastal;
   {$Else}
      GroupBox1.Visible := false;
   {$EndIf}
end;


procedure Tfuv_ssim_control.RadioGroup1Click(Sender: TObject);
begin
    MDDef.DoFUV := RadioGroup1.ItemIndex = 0;
    MDDef.DoSSIM := RadioGroup1.ItemIndex = 1;
    MDDef.DoPartials := RadioGroup1.ItemIndex = 2;
    MDDef.DoCurvatures := RadioGroup1.ItemIndex = 3;
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
         if MDDef.DoPartials then MergeCSV(6);
         if MDDef.DoCurvatures then MergeCSV(7);
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
   {$IfDef IncludeCoastalDEMs}
       if MDdef.DEMIX_AllowCoastal then begin
           if CheckBox2.Checked then DoOne(dmU120);
           if CheckBox3.Checked then DoOne(dmU80);
           if CheckBox4.Checked then DoOne(dmU10);
       end;
   {$EndIf}
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
   VerifyRecordsToUse(DEMIX_criteria_tolerance_fName,'CRITERION');
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
   MDDef.DEMIX_Geo_Tiles := CheckBox7.Checked;

   MDDef.OpenSavedMapsFUVSSIM := CheckBox25.Checked;
   MDDef.ShowWinExec := CheckBox157.Checked;
   MDDef.ProcessLoopsForward := CheckBox9.Checked;
end;





end.
