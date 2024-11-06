unit color_filter;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2024 Peter L. Guth  }
{___________________________________}


interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TColorFilterForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

procedure GetColorFilter;

implementation

{$R *.dfm}

uses
   Petmar, Petmar_types,demdefs;


procedure GetColorFilter;
var
  ColorFilterForm: TColorFilterForm;
begin
   ColorFilterForm := TColorFilterForm.Create(Application);
   ColorFilterForm.ShowModal;
   CheckEditString(ColorFilterForm.Edit1.Text,MDDef.RedLow);
   CheckEditString(ColorFilterForm.Edit2.Text,MDDef.RedHigh);
   CheckEditString(ColorFilterForm.Edit3.Text,MDDef.GreenLow);
   CheckEditString(ColorFilterForm.Edit4.Text,MDDef.GreenHigh);
   CheckEditString(ColorFilterForm.Edit5.Text,MDDef.BlueLow);
   CheckEditString(ColorFilterForm.Edit6.Text,MDDef.BlueHigh);
end;

procedure TColorFilterForm.FormCreate(Sender: TObject);
begin
   Edit1.Text := IntToStr(MDDef.RedLow);
   Edit2.Text := IntToStr(MDDef.RedHigh);
   Edit3.Text := IntToStr(MDDef.GreenLow);
   Edit4.Text := IntToStr(MDDef.GreenHigh);
   Edit5.Text := IntToStr(MDDef.BlueLow);
   Edit6.Text := IntToStr(MDDef.BlueHigh);
end;

end.
