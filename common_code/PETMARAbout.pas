unit PETMARAbout;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}


{$I nevadia_defines.inc}

interface

uses
   Windows, Classes, Graphics, Forms, Controls, StdCtrls,
   ExtCtrls,Buttons;

type
  TPetmarAboutBox = class(TForm)
    Panel1: TPanel;
    OKButton: TBitBtn;
    image1: TImage;
    ProductName: TLabel;
    Version: TLabel;
    Copyright: TLabel;
    Label1: TLabel;
    ProgramIcon: TImage;
    Label2: TLabel;
    Label3: TLabel;
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

