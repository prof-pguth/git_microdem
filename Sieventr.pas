unit Sieventr;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch }
{   file verified 6/22/2011       }
{_________________________________}

{$I nevadia_defines.inc}



interface

uses
   Windows, Classes, Graphics, Forms, Controls, Buttons,
   StdCtrls, ExtCtrls, Grids;

type
  TSieveEntryForm = class(TForm)
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    HelpBtn: TBitBtn;
    Bevel1: TBevel;
    StringGrid1: TStringGrid;
    CheckBox1: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure StringGrid1MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
    procedure StringGrid1DblClick(Sender: TObject);
    procedure StringGrid1Click(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  SieveEntryForm: TSieveEntryForm;

implementation

{$R *.DFM}

uses
   PETMAR,Petmar_types;

var
   LastX,LastY : integer;

procedure TSieveEntryForm.FormCreate(Sender: TObject);
var
   i : integer;
begin
   with StringGrid1 do  begin
      for i := -2 to 4 do begin
         Cells[0,i+3] := IntegerToString(i,4);
         Cells[1,i+3] := '0';
         Cells[2,i+3] := '0';
         Cells[3,i+3] := '0';
      end;
      Cells[0,8] := 'Pan';
      Cells[1,8] := '0';
      Cells[2,8] := '0';
      Cells[3,8] := '0';
      Cells[0,0] := 'Sieve (Phi)';
      Cells[1,0] := 'Sand + Sieve';
      Cells[2,0] := 'Sieve';
      Cells[3,0] := 'Sand';
   end;
end;

procedure TSieveEntryForm.StringGrid1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
   LastX := x;
   LastY := y;
end;


procedure TSieveEntryForm.StringGrid1DblClick(Sender: TObject);
var
   Col,Row : longInt;
   x       : float64;
   err     : integer;

   procedure CheckSand;
   var
      y : float64;
   begin
      Val(StringGrid1.Cells[1,Row],x,err);
      if (err = 0) then begin
         Val(StringGrid1.Cells[2,Row],y,err);
         if err = 0 then begin
            x := x - y;
            StringGrid1.Cells[3,Row] := RealToString(x,-8,-4);
            if x < -0.0001 then begin
               MessageToContinue('Negative weight for sand highly unlikely.');
            end;
         end;
      end;
   end;


begin
   StringGrid1.MouseToCell(LastX,Lasty,Col,Row);
   if (Col = 0) and (CheckBox1.Checked)then   begin
      x := 0;
      CheckEditString(StringGrid1.Cells[Col,Row],x);
      ReadDefault('New phi size for this sieve',x);
      StringGrid1.Cells[Col,Row] := RealToString(x,-8,-4);
   end;

   if (Col = 1) then begin
      Val(StringGrid1.Cells[Col,Row],x,err);
      ReadDefault('Sand + sieve for phi size' + StringGrid1.Cells[0,Row],x);
      StringGrid1.Cells[Col,Row] := RealToString(x,-8,-4);
      CheckSand;
   end;
   if Col = 2 then begin
      Val(StringGrid1.Cells[Col,Row],x,err);
      ReadDefault('Empty sieve size for phi size' + StringGrid1.Cells[0,Row],x);
      StringGrid1.Cells[Col,Row] := RealToString(x,-8,-4);
      CheckSand;
   end;
   if Col = 3 then  begin
      Val(StringGrid1.Cells[Col,Row],x,err);
      ReadDefault('Sand for phi size' + StringGrid1.Cells[0,Row],x);
      StringGrid1.Cells[Col,Row] := RealToString(x,-8,-4);
      StringGrid1.Cells[1,Row] := '0';
      StringGrid1.Cells[2,Row] := '0';
   end;
end;

procedure TSieveEntryForm.StringGrid1Click(Sender: TObject);
begin
   StringGrid1DblClick(Sender);
end;

procedure TSieveEntryForm.HelpBtnClick(Sender: TObject);
begin
   Application.HelpJump('Sieve_entry');
end;


initialization
   LastX := 0;
   LastY := 0;
end.
