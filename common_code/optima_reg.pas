unit optima_reg;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2026 Peter L. Guth  }
{________________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   //{$Define OptimaRegsProblems}
{$EndIf}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls,
  Petmar_Types;

type
  TRegOptsForm = class(TForm)
    Grid: TLabel;
    Label2: TLabel;
    Edit2: TEdit;
    Label3: TLabel;
    Edit3: TEdit;
    OKBtn: TBitBtn;
    HelpBtn: TBitBtn;
    Edit1: TEdit;
    RadioGroup1: TRadioGroup;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Edit6: TEdit;
    Label9: TLabel;
    Label10: TLabel;
    CheckBox5: TCheckBox;
    Label1: TLabel;
    procedure RadioGroup1Click(Sender: TObject);
    procedure CheckBox5Click(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure HelpBtnClick(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure Edit3Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    XSize,YSize : float64;
    DEMUsed : integer;
    procedure EstimateResults;
  end;


implementation

{$R *.dfm}

uses
   Petmar,DEMCoord,DEMDefs;


procedure TRegOptsForm.CheckBox5Click(Sender: TObject);
begin
   Edit2.Enabled := not CheckBox5.Checked;
   Label2.Enabled := not CheckBox5.Checked;
end;

procedure TRegOptsForm.EstimateResults;
var
   ColInc,RowInc,NPts,Cols,Rows : integer;
begin
   CheckEditString(Edit1.Text,ColInc);
   CheckEditString(Edit2.Text,RowInc);
   CheckEditString(Edit3.Text,NPts);
   if NPts > 25 then Npts := 25;
   
   if CheckBox5.Checked then begin
      Edit2.Text := Edit1.Text;
      RowInc := ColInc;
   end;
   if (DEMUsed <> 0) then with DEMGlb[DEMused],SelectionMap,MapDraw,MapCorners do begin

      if (XSize > 0) then Label9.Caption := RealToString(ColInc * XSize,-12,-2) + ' m';
      if (YSize > 0) then Label10.Caption := RealToString(RowInc * YSize,-12,-2) + ' m';

      if (RadioGroup1.ItemIndex = 2) then begin
         Label1.Caption := '';
      end
      else begin
         if (RadioGroup1.ItemIndex = 0) then begin
            Cols := succ(DEMheader.NumCol div ColInc);
            Rows := succ(DEMheader.NumRow div ColInc);
         end
         else if (RadioGroup1.ItemIndex = 1) then begin
            Cols := succ(round(BoundBoxDataGrid.xmax-BoundBoxDataGrid.xmin) div ColInc);
            Rows := succ(succ(round(BoundBoxDataGrid.ymax-BoundBoxDataGrid.ymin)) div ColInc);
         end
         else begin
            Cols := 1;
            Rows := 1;
         end;
         {$IfDef OptimaRegsProblems} WriteLineToDebugFile('TRegOptsForm.EstimateResults  Cols: ' + IntToStr(Cols) +  '  Rows: ' + IntToStr(Rows) + '  Npts: ' + IntToStr(Npts));  {$EndIf}
         Label1.Caption := 'Regions: ' + IntToStr(Cols*Rows) + '   Candidate Points: ' +  IntToStr(Cols*Rows*NPts);
      end;
   end;
end;

procedure TRegOptsForm.Edit1Change(Sender: TObject);
begin
   EstimateResults;
end;

procedure TRegOptsForm.Edit2Change(Sender: TObject);
begin
   if not CheckBox5.Checked then EstimateResults;
end;

procedure TRegOptsForm.Edit3Change(Sender: TObject);
begin
   EstimateResults;
end;

procedure TRegOptsForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   Action := caFree;
end;

procedure TRegOptsForm.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html\regional_optima_parameters.htm');
end;

procedure TRegOptsForm.OKBtnClick(Sender: TObject);
begin
   Close;
end;

procedure TRegOptsForm.RadioGroup1Click(Sender: TObject);
begin
   EstimateResults;
end;


initialization
finalization
   {$IfDef OptimaRegsProblems} WriteLineToDebugFile('OptimaRegsProblems active in optima_reg'); {$EndIf}
end.



