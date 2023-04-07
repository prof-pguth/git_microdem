unit demdbfieldpicker;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2023 Peter L. Guth  }
{___________________________________}

interface

{$I nevadia_defines.inc}

{$IfDef RecordProblems} //normally only defined for debugging specific problems
{$EndIf}


uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, Vcl.ComCtrls,
  Petmar_types;

type
  TFieldPicker = class(TForm)
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    ComboBox3: TComboBox;
    BitBtn4: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    CheckBox1: TCheckBox;
    ComboBox4: TComboBox;
    ComboBox5: TComboBox;
    ComboBox6: TComboBox;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    CheckBox2: TCheckBox;
    procedure BitBtn4Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    tf : integer;
    col : tPlatformColor;
    procedure HideAdvancedOptions;
  end;


implementation

uses
   petmar,DEMDefs;

{$R *.DFM}


procedure TFieldPicker.BitBtn4Click(Sender: TObject);
begin
   Close;
end;

procedure TFieldPicker.CheckBox2Click(Sender: TObject);
begin
   MDDef.FlipHistogram := CheckBox2.Checked;
end;

procedure TFieldPicker.HideAdvancedOptions;
begin
   ComboBox4.Visible := false;
   ComboBox5.Visible := false;
   ComboBox6.Visible := false;
   Label5.Visible := false;
   Label6.Visible := false;
   Label7.Visible := false;
end;

initialization
finalization
end.
