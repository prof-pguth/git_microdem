unit mask_opts2;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch }
{   file created 6/26/2013        }
{_________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
{$EndIf}


interface

uses
  Winapi.Windows, System.SysUtils,  System.Classes, Vcl.Graphics,  //System.Variants,Winapi.Messages, Vcl.ExtCtrls,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls;

type
  TGridMaskOptForm = class(TForm)
    BitBtn1: TBitBtn;
    CheckBox1: TCheckBox;
    Edit1: TEdit;
    Label3: TLabel;
    Edit4: TEdit;
    Label4: TLabel;
    RadioGroup3: TRadioGroup;
    OKBtn: TBitBtn;
    HelpBtn: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure RadioGroup3Click(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

procedure GridMaskOptions;


implementation


uses
   Petmar,Petmar_types,DEMDefs;

{$R *.dfm}


procedure GridMaskOptions;
var
  GridMaskOptForm : TGridMaskOptForm;
begin
   GridMaskOptForm := TGridMaskOptForm.Create(Application);
   GridMaskOptForm.ShowModal;
   GridMaskOptForm.Destroy;
end;

procedure TGridMaskOptForm.BitBtn1Click(Sender: TObject);
begin
   Petmar.QueryColor(BitBtn1,MDDef.MapMaskColor);
end;

procedure TGridMaskOptForm.CheckBox1Click(Sender: TObject);
begin
   MDDef.FuzzyMatches := CheckBox1.Checked;
end;


procedure TGridMaskOptForm.FormCreate(Sender: TObject);
begin
   CheckBox1.Checked := MDDef.FuzzyMatches;
   ColorBitBtn(BitBtn1,MDDef.MapMaskColor);
   Edit1.Text := IntToStr(MDDef.SecondGridOpacity);
   RadioGroup3.ItemIndex := MDDef.MaskMapShow;
end;


procedure TGridMaskOptForm.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\mask_opts2.htm');
end;

procedure TGridMaskOptForm.OKBtnClick(Sender: TObject);
begin
   Close;
end;

procedure TGridMaskOptForm.RadioGroup3Click(Sender: TObject);
begin
   MDDef.MaskMapShow := RadioGroup3.ItemIndex;
end;



end.
