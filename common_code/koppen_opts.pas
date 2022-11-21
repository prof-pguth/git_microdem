unit koppen_opts;






{$I nevadia_defines.inc}

{$IfDef RecordProblems}
{$EndIf}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, 
  DEMDefs;

type
  TKoppen_opt_f = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    OKBtn: TBitBtn;
    HelpBtn: TBitBtn;
    GroupBox1: TGroupBox;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox5: TCheckBox;
    Label3: TLabel;
    Edit3: TEdit;
    CheckBox7: TCheckBox;
    Ft: TLabel;
    Edit4: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    Edit5: TEdit;
    Edit6: TEdit;
    procedure OKBtnClick(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


procedure KoppenOptions;


implementation

{$R *.dfm}

uses
   Petmar;

procedure KoppenOptions;
var
  Koppen_opt_f : TKoppen_opt_f;
begin
   Koppen_opt_f  := TKoppen_opt_f.Create(Application);
   with Koppen_opt_f do begin
       CheckBox1.Checked :=  MDDef.KoppenOpts.ShowLatLong;
       CheckBox2.Checked :=  MDDef.KoppenOpts.ShowTempAndRain;
       CheckBox5.Checked :=  MDDef.KoppenOpts.ShowElevation;
       CheckBox7.Checked := MDDef.TZFromLong;
       Edit1.Text := IntToStr(Height);
       Edit2.Text := IntToStr(Width);
       Edit3.Text := IntToStr(MDDef.UTCOffset);
       Edit4.Text := IntToStr(MDDef.KoppenOpts.KoppenFontSize);
       Edit5.Text := IntToStr(MDDef.KoppenOpts.MaxTemp);
       Edit6.Text := IntToStr(MDDef.KoppenOpts.MaxPrecip);
       ShowModal;
       MDDef.KoppenOpts.ShowLatLong := CheckBox1.Checked;
       MDDef.KoppenOpts.ShowTempAndRain := CheckBox2.Checked;
       MDDef.KoppenOpts.ShowElevation := CheckBox5.Checked;
       MDDef.TZFromLong := CheckBox7.Checked;
       CheckEditString(Edit1.Text,MDDef.KoppenOpts.KopHeight);
       CheckEditString(Edit2.Text,MDDef.KoppenOpts.KopWidth);
       CheckEditString(Edit3.Text,MDDef.UTCOffset);
       CheckEditString(Edit4.Text,MDDef.KoppenOpts.KoppenFontSize);
       CheckEditString(Edit5.Text,MDDef.KoppenOpts.MaxTemp);
       CheckEditString(Edit6.Text,MDDef.KoppenOpts.MaxPrecip);
   end;
end;


procedure TKoppen_opt_f.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\geography_options.htm');
end;


procedure TKoppen_opt_f.OKBtnClick(Sender: TObject);
begin
   Close;
end;



end.
