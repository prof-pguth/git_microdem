unit OKCNHLP2;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls, 
  Buttons, ExtCtrls, OKCANCL2;

type
  TOKHelpRightDlg = class(TOKRightDlg)
    HelpBtn: TButton;
    procedure HelpBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  OKHelpRightDlg: TOKHelpRightDlg;

implementation

{$R *.dfm}

procedure TOKHelpRightDlg.HelpBtnClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

end.
 
