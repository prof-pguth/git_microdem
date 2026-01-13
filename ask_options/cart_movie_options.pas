unit cart_movie_options;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2026 Peter L. Guth  }
{________________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
{$EndIf}


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls;

type
  TCartMovieOptsForm = class(TForm)
    RadioGroup1: TRadioGroup;
    RadioGroup2: TRadioGroup;
    CheckBox1: TCheckBox;
    OKBtn: TBitBtn;
    HelpBtn: TBitBtn;
    CheckBox2: TCheckBox;
    procedure OKBtnClick(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  CartMovieOptsForm : TCartMovieOptsForm;

implementation

uses petmar;

{$R *.dfm}

procedure TCartMovieOptsForm.HelpBtnClick(Sender: TObject);
begin
   Petmar.DisplayHTMLTopic('html\earth_rotation_movie.htm');
end;

procedure TCartMovieOptsForm.OKBtnClick(Sender: TObject);
begin
   Close;
end;


end.
