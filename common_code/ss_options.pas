unit ss_options;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2023 Peter L. Guth  }
{___________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}  //normally only defined for debugging specific problems
{$EndIf}


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons,
  SideImg;

type
  TSS_opts_form = class(TForm)
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit7: TEdit;
    Label7: TLabel;
    Label3: TLabel;
    Label2: TLabel;
    RadioGroup1: TRadioGroup;
    Edit4: TEdit;
    Label5: TLabel;
    Label6: TLabel;
    Edit5: TEdit;
    Label14: TLabel;
    Label15: TLabel;
    Edit8: TEdit;
    Edit9: TEdit;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    BitBtn1: TBitBtn;
    Edit12: TEdit;
    Label10: TLabel;
    CheckBox3: TCheckBox;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Edit3: TEdit;
    Edit6: TEdit;
    Label4: TLabel;
    CheckBox4: TCheckBox;
    Memo1: TMemo;
    Label8: TLabel;
    Edit10: TEdit;
    Label9: TLabel;
    Edit11: TEdit;
    BitBtn5: TBitBtn;
    Edit13: TEdit;
    Edit15: TEdit;
    Label11: TLabel;
    Edit14: TEdit;
    RedrawSpeedButton12: TSpeedButton;
    Label12: TLabel;
    Label13: TLabel;
    procedure BitBtn2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure Edit13Change(Sender: TObject);
    procedure Edit15Change(Sender: TObject);
    procedure RedrawSpeedButton12Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
  private
    { Private declarations }
    procedure ProcessChanges;
  public
    { Public declarations }
     PCChanged : boolean;
     SSImage   : Tsideimage;
     procedure UpdateStats;
  end;

var
  SS_opts_form: TSS_opts_form;

implementation

{$R *.dfm}

uses
   DEMDefs,
   PETMAR,petmar_types;
   //Nevadia_Main;


procedure TSS_opts_form.BitBtn1Click(Sender: TObject);
begin
   Edit8.Text := '1';
   Edit9.Text := '1';
end;


procedure TSS_opts_form.UpdateStats;
var
  Results : tStringList;
  i : integer;
begin
   Memo1.Lines.Clear;
   Results := SSImage.SideScanImageStats;
   for I := 0 to pred(results.Count) do Memo1.Lines.Add(Results.Strings[i]);
   Results.Free;
end;

procedure TSS_opts_form.BitBtn2Click(Sender: TObject);
begin
   ProcessChanges;
   Close;
end;

procedure TSS_opts_form.BitBtn3Click(Sender: TObject);
begin
   Close;
end;

procedure TSS_opts_form.BitBtn4Click(Sender: TObject);
begin
   DisplayHTMLTopic('html\sidescan_options.htm');
end;


procedure TSS_opts_form.BitBtn5Click(Sender: TObject);
begin
    PickLineSizeAndColor('grid',BitBtn5,MDDef.SonarMapDef.SSGridColor,MDDef.SonarMapDef.SSGridWidth);
end;


procedure TSS_opts_form.Edit13Change(Sender: TObject);
begin
   PCChanged := true;
end;

procedure TSS_opts_form.Edit15Change(Sender: TObject);
begin
   PCChanged := true;
end;

procedure TSS_opts_form.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   Action := caFree;
   Self := Nil;
end;

procedure TSS_opts_form.FormCreate(Sender: TObject);
begin
   ColorLineWidthBitBtn(BitBtn5,MDDef.SonarMapDef.SSGridColor,MDDef.SonarMapDef.SSGridWidth);
   Petmar.PlaceFormAtMousePosition(Self);
   PCChanged := false;
end;


procedure TSS_opts_form.ProcessChanges;
begin
   with SSImage do begin
      case RadioGroup1.ItemIndex  of
         0 : FreqUp := LowSideScanFreq;
         1 : FreqUp := HighSideScanFreq;
         2 : FreqUp := MergeFreq;
      end;

      CheckEditString(Edit1.Text,LowGain);
      CheckEditString(Edit2.Text,HighGain);
      CheckEditString(Edit3.Text,MDDef.SonarMapDef.GridSpaceAcross);
      CheckEditString(Edit4.Text,FirstPingDisplayed);
      CheckEditString(Edit5.Text,LastPingDisplayed);
      CheckEditString(Edit6.Text,MDDef.SonarMapDef.GridSpaceAlong);
      CheckEditString(Edit7.Text,FishHeight);
      CheckEditString(Edit8.Text,AcrossTrackThinning);
      CheckEditString(Edit9.Text,PingThinning);
      if PingThinning = 1 then CheckEditString(Edit14.Text,PingRepeats)
      else PingRepeats := 1;

      if PCChanged then begin
         CheckEditString(Edit13.Text,MDDef.SonarMapDef.MinPC);
         CheckEditString(Edit15.Text,MDDef.SonarMapDef.MaxPC);
         RangeFromPercentiles;
         Edit10.Text := IntToStr(LowDisplayValue);
         Edit11.Text := IntToStr(HighDisplayValue);
      end
      else begin
         CheckEditString(Edit10.Text,LowDisplayValue);
         CheckEditString(Edit11.Text,HighDisplayValue);
      end;
      CheckEditString(Edit12.Text,MDDef.SonarMapDef.SidescanLayback);
      ClientWidth := MDDef.SonarMapDef.DefaultPixelsWide;
      ReverseImageDirection := CheckBox1.Checked;
      ReverseGrayscale := CheckBox2.Checked;
      MDDef.SonarMapDef.CustomPalette := CheckBox3.Checked;
      MDDef.SonarMapDef.OverlayGrid := CheckBox4.Checked;
      SetSideScanColorTable;
   end;
end;


procedure TSS_opts_form.RedrawSpeedButton12Click(Sender: TObject);
begin
   ProcessChanges;
   UpdateStats;
   SSImage.DisplayImage;
end;

end.
