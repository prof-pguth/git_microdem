unit sc_COLLABEL;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch }
{   file verified 5/13/2018       }
{_________________________________}

{$I nevadia_defines.inc}

interface

uses
   Windows, Classes, Graphics, Forms, Controls, Buttons, StdCtrls, ExtCtrls;

type
  TCollabf = class(TForm)
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    HelpBtn: TBitBtn;
    Bevel1: TBevel;
    TopEdit: TEdit;
    TextEdit: TEdit;
    BaseEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure HelpBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Collabf: TCollabf;

implementation

{$R *.DFM}

procedure TCollabf.HelpBtnClick(Sender: TObject);
begin
   Application.HelpJump('Time_Edit_Window');
end;



end.
