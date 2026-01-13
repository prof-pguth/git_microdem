unit pick_demix_areas;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2026 Peter L. Guth  }
{________________________________________}


{$I nevadia_defines.inc}

   {$IfDef RecordProblems}
      //{$Define RecordPickAreas}
   {$EndIf}


interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls;

type
  TPickAreasForm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    CancelBtn: TBitBtn;
    Memo1: TMemo;
    BitBtn4: TBitBtn;
    RadioGroup1: TRadioGroup;
    BitBtn5: TBitBtn;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
  private
    { Private declarations }
    procedure LoadDEMs;
  public
    { Public declarations }
    Mode : integer;
  end;


function ModeForAreaSelection : tStringList;


implementation

{$R *.dfm}

uses
   Petmar,Petmar_types,
   nevadia_main,
   DEMdefs,
   Toggle_DB_use,
   DEMIX_control,DEMIX_definitions;


function ModeForAreaSelection : tStringList;
var
  PickAreasForm : TPickAreasForm;
  fName : PathStr;
  i,PickedNum,aThird,First,Last : integer;
begin
    PickAreasForm := TPickAreasForm.Create(Application);
    PickAreasForm.LoadDEMs;

   if ClonedEXE then PickAreasForm.Mode := 1
   else begin
      PickAreasForm.Mode := 0;
      PickAreasForm.ShowModal;
   end;
   case PickAreasForm.Mode of
         0 : Result := tStringList.Create;
         1 : begin
                Result := DEMIX_GetListOfAreas;
             end;
         2 : begin
                Result := DEMIX_GetListOfAreas;
                MultiSelectSingleColumnStringList(' Areas to process',PickedNum,Result,true,true);
             end;
         3 : begin
               fName := DEMIXSettingsDir;
               if GetExistingFileName('DEMIX areas','area*.txt',fName) then begin
                  Result := tStringList.Create;
                  Result.LoadFromFile(fName);
               end;
             end;
         4 : begin
                 Result := tStringList.Create;
                 for i := 0 to pred(PickAreasForm.Memo1.Lines.Count) do
                   Result.Add(PickAreasForm.Memo1.Lines[i]);
             end;
         5..7 : begin
                   Result := DEMIX_GetListOfAreas;
                   aThird := Result.Count div 3;
                   if PickAreasForm.Mode in [5,6] then begin //for first and mid thirds, delete last third
                      First := pred(Result.Count);
                      Last := (Result.Count - aThird);
                      for i := First downto Last do Result.Delete(i);
                   end;
                   if PickAreasForm.Mode in [5] then begin //for first third, delete middle third
                      First := pred(Result.Count);
                      Last := (Result.Count - aThird);
                      for i := First downto Last do Result.Delete(i);
                   end;
                   if PickAreasForm.Mode in [6] then begin //for middle third, delete first third
                      First := pred(Result.Count) - aThird;
                      Last := 0;
                      for i := First downto Last do Result.Delete(i);
                   end;
                   if PickAreasForm.Mode in [7] then begin //for last third, delete first and mid thirds
                      First := pred(Result.Count) - aThird;
                      Last := 0;
                      for i := First downto Last do Result.Delete(i);
                   end;
                end;
   end;
   {$IfDef RecordPickAreas} if Result.Count > 0 then WriteLineToDebugFile('Areas=' + IntToStr(Result.Count) + '  from ' + Result.Strings[0] + ' to ' + Result.Strings[pred(Result.Count)]); {$EndIf}
   Result.Sort;
   PickAreasForm.Close;
end;

procedure TPickAreasForm.LoadDEMs;
var
  theDEMs : tStringList;
  i : integer;
begin
    theDEMs := GetListOfTestDEMsinUse;
    Memo1.Clear;
    for I := 0 to pred(theDEMs.Count) do begin
       Memo1.Lines.Add(theDEMs.Strings[i]);
    end;
    theDEMs.Destroy;
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

procedure TPickAreasForm.BitBtn4Click(Sender: TObject);
begin
   Mode := 4;
   Close;
end;

procedure TPickAreasForm.BitBtn5Click(Sender: TObject);
begin
   VerifyRecordsToUse(DemixSettingsDir + 'demix_dems.dbf','SHORT_NAME');
   LoadDEMs;
end;

procedure TPickAreasForm.CancelBtnClick(Sender: TObject);
begin
  Mode := 0;
  Close;
end;

procedure TPickAreasForm.RadioGroup1Click(Sender: TObject);
begin
   Mode := 5 + RadioGroup1.ItemIndex;
   Close;
end;


end.
