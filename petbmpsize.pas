unit petbmpsize;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch }
{   file verified 5/21/2013       }
{_________________________________}

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
