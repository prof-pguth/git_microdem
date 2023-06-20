unit map_algebra;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2023 Peter L. Guth  }
{___________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   //{$Define RecordMapAlgebra}
{$EndIf}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons;

type
  TMapAlgebraForm = class(TForm)
    ListBox1: TListBox;
    HelpBtn: TBitBtn;
    OKBtn: TBitBtn;
    BitBtn1: TBitBtn;
    Edit1: TEdit;
    CheckBox1: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    NumDemsUsed : integer;
    DEMsUsed : array[1..25] of shortString;
    OpsUsed  : array[1..25] of char;
  end;

procedure DoMapAlgebra;


implementation

{$R *.dfm}

uses
   DEMDefs,DEMCoord,DEMStat,
   Petmar_types,Petmar;


procedure DoMapAlgebra;
var
  MapAlgebraForm : TMapAlgebraForm;
begin
   MapAlgebraForm := TMapAlgebraForm.Create(Application);
   MapAlgebraForm.ShowModal;
   MapAlgebraForm.Close;
end;


procedure TMapAlgebraForm.BitBtn1Click(Sender: TObject);
begin
   Edit1.Text := Edit1.Text + '+ ';
end;

procedure TMapAlgebraForm.FormCreate(Sender: TObject);
var
   i : integer;
begin
   NumDemsUsed := 0;
   for i := 1 to MaxDEMDataSets do begin
      if ValidDEM(i) then begin
         ListBox1.Items.Add(DEMGlb[i].AreaName);
      end {if};
   end {for i};
end;


procedure TMapAlgebraForm.HelpBtnClick(Sender: TObject);
begin
    DisplayHTMLTopic('html/map_algebra.htm');
end;

procedure TMapAlgebraForm.ListBox1Click(Sender: TObject);
begin
   Edit1.Text := Edit1.Text + ListBox1.Items[ListBox1.ItemIndex] + ' ';
   inc(NumDemsUsed);
   DEMsUsed[NumDEMsUsed] := ListBox1.Items[ListBox1.ItemIndex];
end;


procedure TMapAlgebraForm.OKBtnClick(Sender: TObject);
var
   FirstDEM,i,j : integer;
   Merge : tDEMBooleans;
begin
   {$IfDef RecordMapAlgebra} WriteLineToDebugFile('TMapAlgebraForm.OKBtnClick in'); {$EndIf}
   for i := 1 to MaxDEMDataSets do Merge[i] := false;
   FirstDEM := 0;
   for I := 1 to MaxDEMDataSets do if ValidDEM(i) then begin
      for j := 1 to NumDEMsUsed do begin
         if (DEMGlb[i].AreaName = DEMsUsed[j]) then begin
            Merge[i] := true;
            if (FirstDEM = 0) then FirstDEM := i;
         end;
      end;
   end;
   if (NumDEMsUsed > 1) then begin
      SumDEMs(FirstDEM,Merge,'DEM_sum',true,CheckBox1.Checked);
   end;
   Close;
   {$IfDef RecordMapAlgebra} WriteLineToDebugFile('TMapAlgebraForm.OKBtnClick out'); {$EndIf}
end;


initialization
finalization
end.
