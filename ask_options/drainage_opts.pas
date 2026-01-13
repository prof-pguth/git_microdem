unit drainage_opts;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2026 Peter L. Guth  }
{________________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
{$EndIf}


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons,
  DEMMapf, Vcl.ExtCtrls;

type
  Tdrain_opt_form = class(TForm)
    Label2: TLabel;
    Edit2: TEdit;
    HelpBtn: TBitBtn;
    OKBtn: TBitBtn;
    RedrawSpeedButton12: TSpeedButton;
    CheckBox2: TCheckBox;
    GroupBox1: TGroupBox;
    BitBtn2: TBitBtn;
    Label1: TLabel;
    Edit1: TEdit;
    GroupBox2: TGroupBox;
    BitBtn3: TBitBtn;
    Label5: TLabel;
    Edit4: TEdit;
    RadioGroup1: TRadioGroup;
    BitBtn1: TBitBtn;
    BitBtn4: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure HelpBtnClick(Sender: TObject);
    procedure RedrawSpeedButton12Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure Edit4Change(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
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
   Petmar,Petmar_types, PETImage;

procedure SetDrainageDelineationOptions;
var
   drain_opt_form : Tdrain_opt_form;
begin
   drain_opt_form := Tdrain_opt_form.Create(Application);
   drain_opt_form.MapOwner := MapOwner;
   drain_opt_form.Show;
   MapOwner.MapDraw.RedrawDrainageVectors := true;
end;


procedure Tdrain_opt_form.FormCreate(Sender: TObject);
begin
    MapOwner := nil;
    Edit1.Text := IntToStr(MDDef.DrainageArrowSeparation);
    Edit2.Text := IntToStr(MDDef.DrainageArrowLength);
    Edit4.Text := IntToStr(MDDef.AspectRegionSize);
    CheckBox2.Checked := MDDef.DrainageVectorAverage;
    Petmar.ColorLineWidthBitBtn(BitBtn2,MDDef.DrainageArrowColor1,MDDef.DrainageArrowWidth);
    Petmar.ColorLineWidthBitBtn(BitBtn1,MDDef.DrainageArrowColor2,MDDef.DrainageArrowWidth);
    RadioGroup1.ItemIndex := MDDef.DrainageMethod;
end;


procedure Tdrain_opt_form.BitBtn2Click(Sender: TObject);
begin
   Petmar.PickLineSizeAndColor('drainage vectors 2d order',BitBtn2,MDDef.DrainageArrowColor1,MDDef.DrainageArrowWidth);
end;

procedure Tdrain_opt_form.BitBtn3Click(Sender: TObject);
begin
   Petmar.PickLineSizeAndColor('drainage vectors',BitBtn3,MDDef.DrainageVectAvgArrowColor,MDDef.DrainageArrowWidth);
end;

procedure Tdrain_opt_form.CheckBox2Click(Sender: TObject);
begin
   MDDef.DrainageVectorAverage := CheckBox2.Checked;
   RedrawSpeedButton12Click(Sender);
end;

procedure Tdrain_opt_form.Edit1Change(Sender: TObject);
begin
   Petmar.CheckEditString(Edit1.Text,MDDef.DrainageArrowLength);
end;

procedure Tdrain_opt_form.Edit2Change(Sender: TObject);
begin
   Petmar.CheckEditString(Edit2.Text,MDDef.DrainageArrowSeparation);
end;

procedure Tdrain_opt_form.Edit4Change(Sender: TObject);
begin
   CheckEditString(Edit4.Text,MDDef.AspectRegionSize);
end;

procedure Tdrain_opt_form.BitBtn1Click(Sender: TObject);
begin
   Petmar.PickLineSizeAndColor('drainage vectors 3d order',BitBtn1,MDDef.DrainageArrowColor1,MDDef.DrainageArrowWidth);
end;

procedure Tdrain_opt_form.OKBtnClick(Sender: TObject);
begin
   Close;
end;


procedure Tdrain_opt_form.RadioGroup1Click(Sender: TObject);
begin
   MDDef.DrainageMethod := RadioGroup1.ItemIndex;
   RedrawSpeedButton12Click(Sender);
end;

procedure Tdrain_opt_form.RedrawSpeedButton12Click(Sender: TObject);
begin
   if MapOwner <> Nil then MapOwner.DoFastMapRedraw;
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
