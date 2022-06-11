unit OKCNHLP1;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls, 
  Buttons, ExtCtrls, OKCANCL1;

type
  TOKHelpBottomDlg = class(TOKBottomDlg)
    HelpBtn: TButton;
    procedure HelpBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  OKHelpBottomDlg: TOKHelpBottomDlg;

implementation

{$R *.dfm}

procedure TOKHelpBottomDlg.HelpBtnClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

end.
 
