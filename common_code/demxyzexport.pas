unit demxyzexport;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2023 Peter L. Guth  }
{___________________________________}


{$IfDef RecordProblems}   //normally only defined for debugging specific problems
{$EndIf}


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls, Buttons, ExtCtrls;

type
  TXYZformatform = class(TForm)
    RadioGroup1: TRadioGroup;
    OKBtn: TBitBtn;
    RadioGroup2: TRadioGroup;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    HelpBtn: TBitBtn;
    CheckBox1: TCheckBox;
    Thin: TLabel;
    Edit4: TEdit;
    Label5: TLabel;
    Edit5: TEdit;
    Label6: TLabel;
    procedure RadioGroup2Click(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Edit5Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


implementation

{$R *.dfm}

uses
   Petmar,DEMDefs,BaseMap;

procedure TXYZformatform.Edit5Change(Sender: TObject);
begin
   CheckEditString(Edit5.Text,MDDef.DefaultUTMZone);
   Label6.Caption := UTMZoneExtent(MDDef.DefaultUTMZone);
end;

procedure TXYZformatform.FormCreate(Sender: TObject);
begin
   Edit5.Text := IntToStr(MDDef.DefaultUTMZone);
   Label6.Caption := UTMZoneExtent(MDDef.DefaultUTMZone);
end;

procedure TXYZformatform.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\xyz_export.htm');
end;

procedure TXYZformatform.RadioGroup2Click(Sender: TObject);
begin
   Edit1.Enabled := RadioGroup2.ItemIndex in [0,4];
   Edit2.Enabled := RadioGroup2.ItemIndex in [1,2];
   Edit3.Enabled := RadioGroup1.ItemIndex in [2];
end;

end.
