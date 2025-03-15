unit dem_save_dted;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{________________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
{$EndIf}


interface

uses
  Windows, Messages, SysUtils,  Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, Grids, ExtCtrls;

type
  TDted_save_form = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    FullCellCheckBox: TCheckBox;
    Label3: TLabel;
    Edit3: TEdit;
    Label4: TLabel;
    Edit4: TEdit;
    OKBtn: TBitBtn;
    Label5: TLabel;
    Edit5: TEdit;
    CheckBox2: TCheckBox;
    GroupBox1: TGroupBox;
    StringGrid1: TStringGrid;
    RadioGroup1: TRadioGroup;
    procedure FullCellCheckBoxClick(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CheckBox2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


implementation

{$R *.dfm}

uses
   Petmar,DEMDefs; 

procedure TDted_save_form.FullCellCheckBoxClick(Sender: TObject);
begin
   Edit3.Enabled := FullCellCheckBox.Checked;
   Edit4.Enabled := FullCellCheckBox.Checked;
end;


procedure TDted_save_form.CheckBox2Click(Sender: TObject);
begin
    Edit2.Enabled := not CheckBox2.Checked;
end;

procedure TDted_save_form.Edit1Change(Sender: TObject);
begin
   if CheckBox2.Checked then Edit2.Text := Edit1.Text;
end;

procedure TDted_save_form.FormCreate(Sender: TObject);
var
   wYear,wMonth,Wday : word;
   MaintDate : string[4];
begin
   DecodeDate(now,wYear,wmonth,wDay);
   MaintDate := IntToStr(wMonth);
   if (length(MaintDate) = 1) then MaintDate := '0' + MaintDate;
   MaintDate := copy(IntToStr(wYear),3,2) + MaintDate;

   with StringGrid1 do begin
      Cells[0,0] := 'Edition';          Cells[1,0] := '01';
      Cells[0,1] := 'Producer';         Cells[1,1] := 'US';
      Cells[0,2] := 'Classification';   Cells[1,2] := 'Unclassified';
      Cells[0,3] := '90% Vert Acc';     Cells[1,3] := 'NA';
      Cells[0,4] := 'Source';           Cells[1,4] := 'NA';
      Cells[0,5] := '90% Vert Acc';     Cells[1,5] := 'NA';
      Cells[0,6] := 'Prod Date';        Cells[1,6] := MaintDate;
      Cells[0,7] := 'Maint Date';       Cells[1,7] := MaintDate;
      Cells[0,8] := 'Comment';          Cells[1,8] := 'NA';
      Cells[0,9] := 'Source';           Cells[1,9] := '';
   end;
   Petmar.PlaceFormAtMousePosition(Self);
end;


procedure TDted_save_form.RadioGroup1Click(Sender: TObject);
begin
   Case RadioGroup1.ItemIndex of
      0 : Edit1.Text := '30';
      1 : Edit1.Text := '3';
      2 : Edit1.Text := '1';
      3 : Edit1.Text := '0.4';
   end;
   Edit1.Enabled := RadioGroup1.ItemIndex = 4;
end;


procedure TDted_save_form.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   //Action := caFree;
end;

initialization
finalization
end.
