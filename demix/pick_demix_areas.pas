unit pick_demix_areas;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}




interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons;

type
  TPickAreasForm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    CancelBtn: TBitBtn;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Mode : integer;
  end;


function ModeForAreaSelection : integer;


implementation

{$R *.dfm}


function ModeForAreaSelection : integer;
var
  PickAreasForm: TPickAreasForm;
begin
   PickAreasForm := TPickAreasForm.Create(Application);
   PickAreasForm.Mode := 0;
   PickAreasForm.ShowModal;
   Result := PickAreasForm.Mode;
   PickAreasForm.Close;
end;


procedure TPickAreasForm.BitBtn1Click(Sender: TObject);
begin
   Mode := 1;
   Close;
end;

procedure TPickAreasForm.BitBtn2Click(Sender: TObject);
begin
   Mode := 2;
   Close;
end;


procedure TPickAreasForm.BitBtn3Click(Sender: TObject);
begin
   Mode := 3;
   Close;
end;

end.
