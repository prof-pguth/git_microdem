unit getgridunits;

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
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls,
  Petmar_types,PETMAR;

type
  TGetGridForm = class(TForm)
    Label1: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Label2: TLabel;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    RadioGroup1: TRadioGroup;
    BitBtn1: TBitBtn;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BitBtn1Click(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure Edit3Change(Sender: TObject);
    procedure Edit4Change(Sender: TObject);
    procedure Edit5Change(Sender: TObject);
    procedure Edit6Change(Sender: TObject);
  private
    { Private declarations }
    procedure WriteValues(First : boolean = false);
  public
    { Public declarations }
    Lat,Long : float32;
  end;


procedure GetGridSpacing(var Latspace,LongSpace : float32);


implementation

{$R *.dfm}


procedure TGetGridForm.WriteValues(First : boolean = false);
begin
   if First or (RadioGroup1.ItemIndex <> 0) then begin
      Edit1.Text := RealToString(Lat,-18,-12);
      Edit4.Text := RealToString(Long,-18,-12);
   end;

   if First or (RadioGroup1.ItemIndex <> 1) then begin
      Edit2.Text := RealToString(Lat*60,-18,-12);
      Edit5.Text := RealToString(Long*60,-18,-12);
   end;

   if First or (RadioGroup1.ItemIndex <> 2) then begin
      Edit3.Text := RealToString(Lat*3600,-18,-12);
      Edit6.Text := RealToString(Long*3600,-18,-12);
   end;

   Edit1.Enabled := RadioGroup1.ItemIndex = 0;
   Edit2.Enabled := RadioGroup1.ItemIndex = 1;
   Edit3.Enabled := RadioGroup1.ItemIndex = 2;
   Edit4.Enabled := RadioGroup1.ItemIndex = 0;
   Edit5.Enabled := RadioGroup1.ItemIndex = 1;
   Edit6.Enabled := RadioGroup1.ItemIndex = 2;
end;


procedure GetGridSpacing(var Latspace,LongSpace : float32);
var
  GetGridForm : TGetGridForm;
begin
   GetGridForm := TGetGridForm.Create(Application);
   GetGridForm.Lat := LatSpace;
   GetGridForm.Long := LongSpace;
   GetGridForm.WriteValues(True);
   GetGridForm.ShowModal;
   LatSpace := GetGridForm.Long;
   LongSpace := GetGridForm.Long;
   GetGridForm.Free;
end;


procedure TGetGridForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
   Action := caFree;
end;

procedure TGetGridForm.BitBtn1Click(Sender: TObject);
begin
   Close;
end;

procedure TGetGridForm.RadioGroup1Click(Sender: TObject);
begin
   WriteValues;
end;


procedure TGetGridForm.Edit1Change(Sender: TObject);
begin
   CheckEditString(Edit1.Text,Lat);
   WriteValues;
end;

procedure TGetGridForm.Edit2Change(Sender: TObject);
begin
   CheckEditString(Edit2.Text,Lat);
   Lat := Lat / 60;
   WriteValues;
end;

procedure TGetGridForm.Edit3Change(Sender: TObject);
begin
   CheckEditString(Edit3.Text,Lat);
   Lat := Lat / 3600;
   WriteValues;
end;

procedure TGetGridForm.Edit4Change(Sender: TObject);
begin
   CheckEditString(Edit4.Text,Long);
   WriteValues;
end;

procedure TGetGridForm.Edit5Change(Sender: TObject);
begin
   CheckEditString(Edit5.Text,Long);
   Long := Long / 60;
   WriteValues;
end;


procedure TGetGridForm.Edit6Change(Sender: TObject);
begin
   CheckEditString(Edit6.Text,Long);
   Long := Long / 3600;
   WriteValues;
end;


end.
