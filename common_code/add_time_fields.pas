unit add_time_fields;

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
  Dialogs,DB,
  Petmath,Petmar_types,DEMDataBase, StdCtrls, ExtCtrls, Buttons;

type
  TTimeFieldsForm = class(TForm)
    OKBtn: TBitBtn;
    HelpBtn: TBitBtn;
    RadioGroup1: TRadioGroup;
    RadioGroup2: TRadioGroup;
    Edit1: TEdit;
    procedure OKBtnClick(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


procedure FillDateFields(GIS : TGISdataBaseModule);

implementation

{$R *.dfm}

uses
   Petmar;


procedure FillDateFields(GIS : TGISdataBaseModule);
const
  MonthLength : array[1..12] of byte = (31,28,31,30,31,30,31,31,30,31,30,31);
var
  TimeFieldsForm : TTimeFieldsForm;
  Month,Year,i,tz : integer;
  TimeZone,YearAndMonth : string35;
  Values : TStringList;
begin
   TimeFieldsForm := TTimeFieldsForm.Create(Application);
   TimeFieldsForm.ShowModal;
   if TimeFieldsForm.RadioGroup1.ItemIndex = 4 then Values := TStringList.Create;
   with GIS do begin
      AddFieldToDataBase(ftString,'START_DATE',26,0);
      AddFieldToDataBase(ftString,'END_DATE',26,0);
      GIS.EmpSource.Enabled := false;
      if TimeFieldsForm.RadioGroup2.ItemIndex = 0 then TimeZone := 'Z'
      else begin
         CheckEditString(TimeFieldsForm.Edit1.Text,tz);
         TimeZone := AddDayMonthLeadingZero(abs(tz)) + ':00';
         if tz < 0 then TimeZone := '-' + TimeZone;
      end;

      MyData.First;
      while not MyData.EOF do begin
         if TimeFieldsForm.RadioGroup1.ItemIndex = 1 then Year := 2000
         else Year := MyData.GetFieldByNameAsInteger('YEAR');
         MyData.Edit;

         if TimeFieldsForm.RadioGroup1.ItemIndex = 2 then begin
            MyData.SetFieldByNameAsString('START_DATE',IntToStr(Year) + '-01-01' + JustAfterMidnight + TimeZone);
            MyData.SetFieldByNameAsString('END_DATE',IntToStr(Year) + '-12-31' + JustBeforeMidnight + TimeZone);
         end
         else begin
            Month := MyData.GetFieldByNameAsInteger(MonthFieldName);
            YearAndMonth := IntToStr(Year) + '-' + AddDayMonthLeadingZero(Month);
            if TimeFieldsForm.RadioGroup1.ItemIndex = 3 then begin
               YearAndMonth := YearAndMonth + '-' + AddDayMonthLeadingZero(MyData.GetFieldByNameAsInteger('DAY'));
               MyData.SetFieldByNameAsString('START_DATE',YearAndMonth + JustAfterMidnight + TimeZone);
               MyData.SetFieldByNameAsString('END_DATE',YearAndMonth + JustBeforeMidnight + TimeZone);
            end
            else if TimeFieldsForm.RadioGroup1.ItemIndex = 0 then begin
               MyData.SetFieldByNameAsString('START_DATE',YearAndMonth + '-01' + JustAfterMidnight + TimeZone);
               MyData.SetFieldByNameAsString('END_DATE',YearAndMonth + '-' + AddDayMonthLeadingZero(MonthLength[Month]) + JustBeforeMidnight + TimeZone);
            end
            else if TimeFieldsForm.RadioGroup1.ItemIndex = 4 then begin
               YearAndMonth := YearAndMonth + '-' + AddDayMonthLeadingZero(MyData.GetFieldByNameAsInteger('DAY')) + 'T' + MyData.GetFieldByNameAsString('TIME_STR') + TimeZone;
               Values.Add(YearAndMonth);
               MyData.SetFieldByNameAsString('START_DATE',YearAndMonth);
            end;
         end;
         MyData.Next;
      end;
      if TimeFieldsForm.RadioGroup1.ItemIndex = 4 then begin
         i := 0;
         MyData.First;
         while not eof do begin
            MyData.Edit;
            inc(i);
            if i > pred(Values.Count) then i := pred(Values.Count);
            MyData.SetFieldByNameAsString('END_DATE',Values.Strings[i]);
            MyData.Next;
         end;
      end;
   end;
   GIS.EmpSource.Enabled := true;
   TimeFieldsForm.Free;
end;

procedure TTimeFieldsForm.OKBtnClick(Sender: TObject);
begin
   Close;
end;

procedure TTimeFieldsForm.RadioGroup2Click(Sender: TObject);
begin
   Edit1.Enabled := RadioGroup2.ItemIndex = 1;
end;

end.
