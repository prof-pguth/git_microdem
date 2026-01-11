unit demrange;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2026 Peter L. Guth  }
{________________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   //{$Define RecordRangeCircles}
{$EndIf}


interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls, 
  Buttons, ExtCtrls, OkCancl2, Grids, Dialogs,Petmar_types,Petmar_db,PETMAR;

type
  TRangeCircleForm = class(TOKRightDlg)
    HelpBtn: TButton;
    StringGrid1: TStringGrid;
    RadioGroup1: TRadioGroup;
    Label1: TLabel;
    Label2: TLabel;
    procedure HelpBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Factor : float64;
  end;


procedure EditRangeCircles;
procedure GetDistance(var Dist : float64;  theCapt : ShortString = '');


implementation

uses
   Nevadia_Main,
   DEMDefs,DEMDef_Routines,PETMath, PetDBUtils, toggle_db_use,Make_tables;

{$R *.DFM}

procedure GetDistance(var Dist : float64;  theCapt : ShortString = '');
var
   RangeCircleForm : TRangeCircleForm;
   error           : integer;
   tval            : float64;
begin
   RangeCircleForm := TRangeCircleForm.Create(Application);
   RangeCircleForm.StringGrid1.RowCount := 1;
   if (theCapt <> '') then RangeCircleForm.Caption := theCapt;

   RangeCircleForm.StringGrid1.Cells[0,0] := RealToString(Dist * RangeCircleForm.Factor,-12,-4);
   if (RangeCircleForm.ShowModal <> mrCancel) then begin
      val(RangeCircleForm.StringGrid1.Cells[0,0],tval,error);
      if (error = 0) then Dist := tval/RangeCircleForm.Factor;
   end;
end;


procedure EditRangeCircles;
var
   RangeCircleForm : TRangeCircleForm;
   i,error         : integer;
   tval            : float64;
   Table : tMyData;
   TStr : shortString;
begin
   {$IfDef RecordRangeCircles} WriteLineToDebugFile('EditRangeCircles in'); {$EndIf}
   if FileExists(RangeCircleSizesfName) and Toggle_db_use.VerifyRecordsToUse(RangeCircleSizesfName,'NAME','Ranges to use (m)','USE','RANGE') then begin
      {$IfDef RecordRangeCircles} WriteLineToDebugFile('Picked range circles to use'); {$EndIf}
   end
   else begin
      {$IfDef RecordRangeCircles}   WriteLineToDebugFile('Create range circles'); {$EndIf}
      if (not FileExists(RangeCircleSizesfName)) then begin
         {$IfDef RecordRangeCircles} WriteLineToDebugFile('File did not exist: ' + RangeCircleSizesfName); {$EndIf}
         Make_Tables.MakeRangeCircleTable(RangeCircleSizesfName);
      end;

      RangeCircleForm := TRangeCircleForm.Create(Application);
      Table := tMyData.Create(RangeCircleSizesfName);
      if (RangeCircleForm.ShowModal <> mrCancel) then begin
         for i := 0 to 4 do begin
            val(RangeCircleForm.StringGrid1.Cells[0,i],tval,error);
            if (error = 0) then begin
               Table.Insert;
               Table.SetFieldByNameAsFloat('RANGE',tval / RangeCircleForm.Factor);
               TStr := RangeCircleForm.StringGrid1.Cells[1,i];
               if (TStr = '') then TStr := RangeCircleForm.StringGrid1.Cells[0,i] + ' ' + RangeCircleForm.RadioGroup1.Items[RangeCircleForm.RadioGroup1.ItemIndex];
               Table.SetFieldByNameAsString('NAME',TStr);
               Table.SetFieldByNameAsString('USE','Y');
               Table.SetFieldByNameAsInteger('COLOR',0);
               Table.SetFieldByNameAsINteger('LINE_WIDTH',1);
               Table.Post;
            end;
         end;
         Table.Destroy;
      end;
      RangeCircleForm.Free;
   end;
   {$IfDef RecordRangeCircles} WriteLineToDebugFile('EditRangeCircles out'); {$EndIf}
end;


procedure TRangeCircleForm.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\tbme1jub.htm');
end;


procedure TRangeCircleForm.FormCreate(Sender: TObject);
begin
   PlaceFormAtMousePosition(Self);
   RadioGroup1.ItemIndex := ord(MDDef.RangeCircleUnit);
end;


procedure TRangeCircleForm.RadioGroup1Click(Sender: TObject);
begin
   MDDef.RangeCircleUnit := tRangeCircleUnit(RadioGroup1.ItemIndex);
   DEMDef_Routines.GetRangeFactor(Factor);
end;


initialization
finalization
   {$IfDef RecordRangeCircles} WriteLineToDebugFile('RecordRangeCircles active in DEMrange'); {$EndIf}
end.

