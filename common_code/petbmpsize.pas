unit petbmpsize;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2022 Peter L. Guth  }
{___________________________________}


interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, Forms, StdCtrls, Buttons;

type
  TNewBMPForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    WidthEdit: TEdit;
    HeightEdit: TEdit;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.DFM}

initialization
finalization
end.
