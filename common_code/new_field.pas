unit new_field;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{________________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}  //normally only defined for debugging specific problems
   //{$Define RecordNewField}
{$EndIf}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Db,
  StdCtrls,   Grids, Buttons,
  Petmar_types,PETMAR,
  DEMDataBase, Vcl.ExtCtrls;

type
  TNewFieldForm = class(TForm)
    Label2: TLabel;
    Label1: TLabel;
    Label3: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    RadioGroup1: TRadioGroup;
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    HelpBtn: TBitBtn;
    procedure HelpBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
  private
    { Private declarations }
     Abort : boolean;
  public
  end;


function AddNewField(var GISDB : TGISdataBaseModule; TheFieldName : ShortString = ''; aft : tFieldType = ftString; Length : integer = 24) : ShortString;


implementation

{$R *.DFM}

uses
   DEMDefs,
   PetDBUtils;


function AddNewField(var GISDB : TGISdataBaseModule; TheFieldName : ShortString = ''; aft : tFieldType = ftString; Length : integer = 24) : ShortString;
var
  NewFieldForm: TNewFieldForm;
  Decimals : integer;
begin
   NewFieldForm := TNewFieldForm.Create(Application);
   with NewFieldForm do begin
      if (TheFieldName <> '') then begin
         if (System.Length(TheFieldName) > 10) then TheFieldName := Copy(TheFieldName,1,10);
         Edit1.Text := TheFieldName;
      end;
      Edit2.Text := IntToStr(Length);
      case aft of
        ftString  : RadioGroup1.ItemIndex := 0;
        ftInteger : RadioGroup1.ItemIndex := 1;
        ftFloat   : RadioGroup1.ItemIndex := 2;
      end;

      NewFieldForm.ShowModal;

      if (not Abort) then begin
        case RadioGroup1.ItemIndex of
           0 : aft := ftString;
           1 : aft := ftInteger;
           2 : aft := ftFloat;
        end;
        CheckEditString(Edit2.Text,Length);
        CheckEditString(Edit3.Text,Decimals);
        result := UpperCase(Edit1.Text);
        {$IfDef RecordNewField} WriteLineToDebugFile('AddNewField calling GISDB.AddFieldToDataBase for ' + Result);   {$EndIf}
        GISDB.AddFieldToDataBase(aft,Result,Length,Decimals);
        NewFieldForm.Destroy;
      end;
   end;
end;


{ TNewFieldForm }

procedure TNewFieldForm.CancelBtnClick(Sender: TObject);
begin
   Abort := true;
end;

procedure TNewFieldForm.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\new_field.htm');
end;

procedure TNewFieldForm.OKBtnClick(Sender: TObject);
begin
   Abort := false;
end;


initialization
finalization
   {$IfDef RecordNewField} WriteLineToDebugFile('RecordNewField active in new_field'); {$EndIf}
   {$IfDef RecordClosing} WriteLineToDebugFile('Closing new_field out');  {$EndIf}
end.


