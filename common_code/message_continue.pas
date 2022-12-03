unit message_continue;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2022 Peter L. Guth  }
{___________________________________}


{$I nevadia_defines.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls;

type
  Tmess_cont_form = class(TForm)
    Panel1: TPanel;
    Memo1: TMemo;
    OKBtn: TBitBtn;
    procedure OKBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  mess_cont_form : Tmess_cont_form;

implementation

{$R *.dfm}

procedure Tmess_cont_form.OKBtnClick(Sender: TObject);
begin
   Close;
end;


end.
