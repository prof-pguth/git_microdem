unit drainage_opts;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2022 Peter L. Guth  }
{___________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
{$EndIf}


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons,
  DEMMapf;

type
  Tdrain_opt_form = class(TForm)
    BitBtn1: TBitBtn;
    Label2: TLabel;
    Edit2: TEdit;
    HelpBtn: TBitBtn;
    OKBtn: TBitBtn;
    Label3: TLabel;
    RedrawSpeedButton12: TSpeedButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    GroupBox1: TGroupBox;
    BitBtn2: TBitBtn;
    Label1: TLabel;
    Edit1: TEdit;
    Label4: TLabel;
    Edit3: TEdit;
    GroupBox2: TGroupBox;
    BitBtn3: TBitBtn;
    Label5: TLabel;
    Edit4: TEdit;
    Edit5: TEdit;
    Label6: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure HelpBtnClick(Sender: TObject);
    procedure RedrawSpeedButton12Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure Edit3Change(Sender: TObject);
    procedure Edit4Change(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure Edit5Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    MapOwner : tMapForm;
  end;

procedure SetDrainageDelineationOptions(MapOwner : tMapForm);

implementation

{$R *.dfm}


uses
   DEMDefs,Demdef_routines,
   DemCoord,
   Petmar;

procedure SetDrainageDelineationOptions;
var
   drain_opt_form : Tdrain_opt_form;
begin
   drain_opt_form := Tdrain_opt_form.Create(Application);
   drain_opt_form.MapOwner := MapOwner;
   drain_opt_form.Show;
end;

procedure Tdrain_opt_form.FormCreate(Sender: TObject);
begin
    Label3.Caption := SlopeMethodName(MDdef.DrainageSlopeAlgorithm);
    Edit1.Text := IntToStr(MDDef.DrainageArrowSeparation);
    Edit2.Text := IntToStr(MDDef.DrainageArrowLength);
    Edit3.Text := IntToStr(MDDef.SlopeRegionRadius);
    Edit4.Text := IntToStr(MDDef.AspectRegionSize);
    Edit5.Text := IntToStr(MDDef.DrainageVectAvgArrowLength);
    CheckBox1.Checked := MDDef.DrainagePointSlope;
    CheckBox2.Checked := MDDef.DrainageVectorAverage;
    Petmar.ColorLineWidthBitBtn(BitBtn2,MDDef.DrainageArrowColor,MDDef.DrainageArrowWidth);
    Petmar.ColorLineWidthBitBtn(BitBtn3,MDDef.DrainageVectAvgArrowColor,MDDef.DrainageVectAvgArrowWidth);
end;


procedure Tdrain_opt_form.BitBtn2Click(Sender: TObject);
begin
   Petmar.PickLineSizeAndColor('drainage vectors',BitBtn2,MDDef.DrainageArrowColor,MDDef.DrainageArrowWidth);
end;

procedure Tdrain_opt_form.BitBtn3Click(Sender: TObject);
begin
   Petmar.PickLineSizeAndColor('drainage vectors',BitBtn3,MDDef.DrainageVectAvgArrowColor,MDDef.DrainageVectAvgArrowWidth);
end;

procedure Tdrain_opt_form.CheckBox1Click(Sender: TObject);
begin
    MDDef.DrainagePointSlope := CheckBox1.Checked;
end;

procedure Tdrain_opt_form.CheckBox2Click(Sender: TObject);
begin
    MDDef.DrainageVectorAverage := CheckBox2.Checked;
end;

procedure Tdrain_opt_form.Edit1Change(Sender: TObject);
begin
   Petmar.CheckEditString(Edit1.Text,MDDef.DrainageArrowLength);
end;

procedure Tdrain_opt_form.Edit2Change(Sender: TObject);
begin
   Petmar.CheckEditString(Edit2.Text,MDDef.DrainageArrowSeparation);
end;

procedure Tdrain_opt_form.Edit3Change(Sender: TObject);
begin
   CheckEditString(Edit3.Text,MDDef.SlopeRegionRadius);
end;

procedure Tdrain_opt_form.Edit4Change(Sender: TObject);
begin
   CheckEditString(Edit4.Text,MDDef.AspectRegionSize);
end;

procedure Tdrain_opt_form.Edit5Change(Sender: TObject);
begin
   CheckEditString(Edit5.Text,MDDef.DrainageVectAvgArrowLength);
end;


procedure Tdrain_opt_form.BitBtn1Click(Sender: TObject);
begin
   DemCoord.PickSlopeAspectMethod('',MDdef.DrainageSlopeAlgorithm);
   Label3.Caption := SlopeMethodName(MDdef.DrainageSlopeAlgorithm);
end;

procedure Tdrain_opt_form.OKBtnClick(Sender: TObject);
begin
   Close;
end;


procedure Tdrain_opt_form.RedrawSpeedButton12Click(Sender: TObject);
begin
   MapOwner.DoFastMapRedraw;
end;

procedure Tdrain_opt_form.FormClose(Sender: TObject;  var Action: TCloseAction);
begin
   Action := caFree;
end;


procedure Tdrain_opt_form.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\drain_ops.htm');
end;


initialization
finalization
end.
