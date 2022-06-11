unit dem_hist_opts;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch }
{   file verified 6/22/2011       }
{_________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}  //normally only defined for debugging specific problems
{$EndIf}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls;

type
  THistOptForm = class(TForm)
    HelpBtn: TBitBtn;
    BitBtn1: TBitBtn;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    RadioGroup1: TRadioGroup;
    RadioGroup2: TRadioGroup;
    RadioGroup3: TRadioGroup;
    Edit1: TEdit;
    Label1: TLabel;
    procedure HelpBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BitBtn1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RadioGroup3Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


procedure SetHistogramOptions;


implementation

{$R *.dfm}

uses
   DEMDefs,Petmar,Petmar_types,Nevadia_Main;



procedure SetHistogramOptions;
var
  HistOptForm : THistOptForm;
begin
   HistOptForm := THistOptForm.Create(Application);
   InsureFormOnScreen(HistOptForm,Mouse.CursorPos.X,Mouse.CursorPos.Y);
   HistOptForm.ShowModal;
   HistOptForm.Free;
end;


procedure THistOptForm.BitBtn1Click(Sender: TObject);
begin
   Close;
end;


procedure THistOptForm.Edit1Change(Sender: TObject);
begin
   CheckEditString(Edit1.Text,MDDef.HistBinSize);
end;

procedure THistOptForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   MDDef.ShowRegularHistogram := CheckBox1.Checked;
   MDDef.ShowCumulativeHistogram := CheckBox2.Checked;
   MDDef.ShowNormalHistogram := CheckBox3.Checked;
   MDDef.ShowStrahlerHistogram := CheckBox4.Checked;
   MDDef.ShowHistogramText := CheckBox5.Checked;
   MDDef.CountHistograms := RadioGroup1.ItemIndex = 1;
   MDDef.GeomorphAllDEMs := RadioGroup2.ItemIndex = 1;
   Action := caFree;
end;

procedure THistOptForm.FormCreate(Sender: TObject);
begin
   CheckBox1.Checked := MDDef.ShowRegularHistogram;
   CheckBox2.Checked := MDDef.ShowCumulativeHistogram;
   CheckBox3.Checked := MDDef.ShowNormalHistogram;
   CheckBox4.Checked := MDDef.ShowStrahlerHistogram;
   CheckBox5.Checked := MDDef.ShowHistogramText;
   Edit1.Text := RealToString(MDDef.HistBinSize,-12,-2);
   if MDDef.GeomorphAllDEMs then RadioGroup2.ItemIndex := 1 else  RadioGroup2.ItemIndex := 0;
   if MDDef.CountHistograms then RadioGroup1.ItemIndex := 1 else  RadioGroup1.ItemIndex := 0;
   if MDDef.GeomorphMapsFullDEM then RadioGroup3.ItemIndex := 0 else RadioGroup3.ItemIndex := 1;
end;

procedure THistOptForm.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\tbme046r.htm');
end;

procedure THistOptForm.RadioGroup3Click(Sender: TObject);
begin
   MDDef.GeomorphMapsFullDEM := (RadioGroup1.ItemIndex = 0);
end;


end.
