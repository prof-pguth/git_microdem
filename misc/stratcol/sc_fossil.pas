unit sc_fossil;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{________________________________________}

{$I nevadia_defines.inc}


interface

uses
   Windows, Classes, Graphics, Forms, Controls, Buttons,StdCtrls, ExtCtrls;

type
  TFossilRangeOptions = class(TForm)
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    HelpBtn: TBitBtn;
    Bevel1: TBevel;
    Label1: TLabel;
    Label2: TLabel;
    Edit1: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    Edit2: TEdit;
    Edit3: TEdit;
    procedure Edit1Change(Sender: TObject);
    procedure Edit3Change(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FossilRangeOptions: TFossilRangeOptions;

procedure SetFossilRangeOptions(NumRanges,RangeWidth,ImageWidth : integer; var RangesStart,RangesEnd : SmallInt);

implementation

{$R *.DFM}

uses
   PETMAR,Petmar_types;

var
   gbRangeWidth,RangesAcross,gbImageWidth : integer;

procedure SetFossilRangeOptions(NumRanges,RangeWidth,ImageWidth : integer; var RangesStart,RangesEnd : SmallInt);
begin
   FossilRangeOptions := TFossilRangeOptions.Create(Application);
   gbRangeWidth := RangeWidth;
   gbImageWidth := ImageWidth;
   with FossilRangeOptions do begin
      Label1.Caption := 'Fossil ranges in file:' + IntegerToString(NumRanges,8);
      RangesAcross := (ImageWidth - RangesStart) div RangeWidth;
      if RangesAcross > NumRanges then RangesAcross := NumRanges;
      RangesEnd := RangesStart + RangesAcross * RangeWidth;
      Edit1.Text := IntegerToString(RangesAcross,-8);
      Edit2.Text := IntegerToString(RangesStart,-8);
      Edit3.Text := IntegerToString(RangesEnd,-8);
      if ShowModal <> idCancel then begin
         CheckEditString(Edit3.Text,RangesEnd);
      end;
      Close;
   end;
end;


procedure TFossilRangeOptions.Edit1Change(Sender: TObject);
var
   RangesStart,RangesEnd : integer;
begin
   CheckEditString(Edit1.Text,RangesAcross);
   CheckEditString(Edit2.Text,RangesStart);
   RangesEnd := RangesStart + RangesAcross * gbRangeWidth;
   if RangesEnd > gbImageWidth then RangesEnd := gbimageWidth;
   Edit3.Text := IntegerToString(RangesEnd,-8);
end;

procedure TFossilRangeOptions.Edit3Change(Sender: TObject);
var
   RangesEnd,RangesStart : integer;
begin
   CheckEditString(Edit3.Text,RangesEnd);
   if RangesEnd > gbImageWidth then RangesEnd := gbimageWidth;
   CheckEditString(Edit2.Text,RangesStart);
   RangesAcross := (RangesEnd - RangesStart) div gbRangeWidth;
end;


procedure TFossilRangeOptions.HelpBtnClick(Sender: TObject);
begin
   Application.HelpJump('Fossil_ranges');
end;



end.
