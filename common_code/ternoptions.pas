unit ternoptions;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch }
{   file verified 6/22/2011       }
{_________________________________}

{$I nevadia_defines.inc}


interface

uses
  Windows, Messages, SysUtils,  Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons;

type
  TTernOptForm = class(TForm)
    RadioGroup1: TRadioGroup;
    BitBtn1: TBitBtn;
    RadioGroup2: TRadioGroup;
    BitBtn5: TBitBtn;
    procedure BitBtn5Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


implementation

uses petmar;

{$R *.dfm}

procedure TTernOptForm.BitBtn1Click(Sender: TObject);
begin
   Close;
end;

procedure TTernOptForm.BitBtn5Click(Sender: TObject);
begin
   Petmar.DisplayHTMLTopic('html\tern_opts.htm');
end;

end.
