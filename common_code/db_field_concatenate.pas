unit db_field_concatenate;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM                }
{ PETMAR Trilobite Breeding Ranch }
{_________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
{$EndIf}


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons;

type
  Tdb_concatenate = class(TForm)
    Edit1: TEdit;
    ComboBox1: TComboBox;
    Edit2: TEdit;
    ComboBox2: TComboBox;
    Edit3: TEdit;
    BitBtn2: TBitBtn;
    HelpBtn: TBitBtn;
    Edit4: TEdit;
    Label1: TLabel;
    ComboBox3: TComboBox;
    Edit5: TEdit;
    procedure BitBtn2Click(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Abort : boolean;
  end;

var
  db_concatenate : Tdb_concatenate;

implementation

{$R *.dfm}

uses
   Petmar;

procedure Tdb_concatenate.BitBtn2Click(Sender: TObject);
begin
   Abort := false;
   Close;
end;

procedure Tdb_concatenate.FormCreate(Sender: TObject);
begin
   Abort := true;
end;

procedure Tdb_concatenate.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\db_concatenate.htm');
end;


end.
