unit ssim_fuv_control;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}

{$I nevadia_defines.inc}


interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons;

type
  Tfuv_ssim_control = class(TForm)
    GroupBox9: TGroupBox;
    CheckBox12: TCheckBox;
    CheckBox13: TCheckBox;
    CheckBox14: TCheckBox;
    CheckBox15: TCheckBox;
    Hillshade: TCheckBox;
    CheckBox17: TCheckBox;
    CheckBox19: TCheckBox;
    CheckBox20: TCheckBox;
    CheckBox21: TCheckBox;
    HAND: TCheckBox;
    CheckBox26: TCheckBox;
    CheckBox27: TCheckBox;
    CheckBox28: TCheckBox;
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
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    CheckBox9: TCheckBox;
    CheckBox10: TCheckBox;
    CheckBox157: TCheckBox;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    BitBtn5: TBitBtn;
    CheckBox11: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn38Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure HillshadeClick(Sender: TObject);
    procedure HANDClick(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
  private
    { Private declarations }
    procedure CheckParameters;
    procedure CheckAll(whichway : boolean);
  public
    { Public declarations }
  end;


procedure FUV_SSIM_work;
procedure FUV_SSIM_Processing(Mode : Byte; Overwrite,AreasInsteadOfTiles : boolean);


implementation

{$R *.dfm}

uses
  Petmar_types,DEMDefs,demdef_routines,demix_definitions,Demix_control,DEMstat,
  petmar;


procedure FUV_SSIM_work;
var
  fuv_ssim_control : Tfuv_ssim_control;
begin
   fuv_ssim_control := Tfuv_ssim_control.Create(Application);
   fuv_ssim_control.Show;
   //fuv_ssim_control.Destroy;
end;

procedure Tfuv_ssim_control.CheckAll(whichway : boolean);
begin
   CheckBox12.Checked := WhichWay;
   CheckBox13.Checked := WhichWay;
   CheckBox14.Checked := WhichWay;
   CheckBox15.Checked := WhichWay;
   Hillshade.Checked := WhichWay;
   HAND.Checked := WhichWay;
   CheckBox17.Checked := WhichWay;
   CheckBox19.Checked := WhichWay;
   CheckBox20.Checked := WhichWay;
   CheckBox21.Checked := WhichWay;
   CheckBox26.Checked := WhichWay;
   CheckBox27.Checked := WhichWay;
   CheckBox28.Checked := WhichWay;
   CheckBox7.Checked := WhichWay;
   CheckBox8.Checked := WhichWay;
   CheckBox10.Checked := WhichWay;
end;


procedure Tfuv_ssim_control.FormCreate(Sender: TObject);
begin
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
   CheckBox25.Checked := MDDef.OpenMapsFUVSSIM;
   CheckBox26.Checked := MDdef.SSIM_ProfC;
   CheckBox27.Checked := MDdef.SSIM_PlanC;
   CheckBox28.Checked := MDDef.SSIM_TangC;
   CheckBox7.Checked := MDDef.SSIM_rotor;
   CheckBox8.Checked := MDDef.SSIM_Openness;
   CheckBox10.Checked := MDDef.SSIM_ConvergeIndex;

   CheckBox5.Checked := MDDef.DEMIX_overwrite_enabled;
   CheckBox6.Checked := MDDef.DEMIX_all_areas;
   CheckBox1.Checked := true;
   CheckBox9.Checked := MDDef.ProcessLoopsForward;
   CheckBox157.Checked := MDDef.ShowWinExec;
end;


procedure Tfuv_ssim_control.HANDClick(Sender: TObject);
begin
   MDDef.SSIM_HAND := HAND.Checked;
end;

procedure Tfuv_ssim_control.HillshadeClick(Sender: TObject);
begin
   MDDef.SSIM_hill := Hillshade.Checked;
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


procedure Tfuv_ssim_control.BitBtn1Click(Sender: TObject);


   procedure DoOne(Mode : byte);
   begin
      if (Sender = BitBtn1) then begin
         FUV_SSIM_Processing(Mode,MDDef.DEMIX_overwrite_enabled,CheckBox11.Checked);
      end
      else begin
         MDDef.DEMIX_mode := Mode;
         if MDDef.DoFUV then MergeCSV(1);   //MergeCSVtoCreateFinalDB(FUVresultsDir,'_fuv_results.csv','_fuv_demix_db_');
         if MDDef.DoSSIM then MergeCSV(2); //MergeCSVtoCreateFinalDB(SSIMresultsDir,'_ssim_results.csv','_ssim_demix_db_');
      end;
   end;


begin
   CheckParameters;
   MDDef.DEMIX_mode := dmFull;
   PickWineContestDBLocation;
   ToggleShowProgress(false);
   if CheckBox1.Checked then DoOne(dmFull);
   if CheckBox2.Checked then DoOne(dmU120);
   if CheckBox3.Checked then DoOne(dmU80);
   if CheckBox4.Checked then DoOne(dmU10);
   ToggleShowProgress(true);
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
   CheckAll(true);
end;

procedure Tfuv_ssim_control.BitBtn4Click(Sender: TObject);
begin
   CheckAll(false);
end;

procedure Tfuv_ssim_control.BitBtn5Click(Sender: TObject);
begin
   Spawn_FUV_SSIMProcessing;
end;

procedure Tfuv_ssim_control.CheckParameters;
begin
   MDDef.SSIM_elev := CheckBox12.Checked;
   MDDef.SSIM_slope := CheckBox13.Checked;
   MDDef.SSIM_ruff := CheckBox14.Checked;
   MDDef.SSIM_rri := CheckBox15.Checked;
   MDDef.SSIM_tpi := CheckBox17.Checked;
   MDDef.SSIM_flow := CheckBox19.Checked;
   MDDef.SSIM_wet := CheckBox20.Checked;
   MDdef.SSIM_ls := CheckBox21.Checked;
   MDDef.DoSSIM := CheckBox22.Checked;
   MDDef.DoFUV := CheckBox24.Checked;
   MDDef.SSIM_ProfC := CheckBox26.Checked;
   MDDef.SSIM_PlanC := CheckBox27.Checked;
   MDDef.SSIM_TangC := CheckBox28.Checked;
   MDDef.SSIM_rotor := CheckBox7.Checked;
   MDDef.SSIM_Openness := CheckBox8.Checked;
   MDDef.SSIM_ConvergeIndex := CheckBox10.Checked;

   MDDef.OpenMapsFUVSSIM := CheckBox25.Checked;
   MDDef.DEMIX_overwrite_enabled := CheckBox5.Checked;
   MDDef.DEMIX_all_areas := CheckBox6.Checked;
   MDDef.OpenMapsFUVSSIM := CheckBox25.Checked;
   MDDef.ShowWinExec := CheckBox157.Checked;
   MDDef.ProcessLoopsForward := CheckBox9.Checked;
end;


end.
