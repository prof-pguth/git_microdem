unit dem_sat_header;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program       }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2024 Peter L. Guth   }
{____________________________________}

{$I nevadia_defines.inc}


{$IfDef RecordProblems}   //normally only defined for debugging specific problems
{$EndIf}


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, StdCtrls, Buttons, Vcl.ExtCtrls;

type
  TSatHeaderForm = class(TForm)
    StringGrid1: TStringGrid;
    Panel1: TPanel;
    OKBtn: TBitBtn;
    Panel2: TPanel;
    Label5: TLabel;
    Edit5: TEdit;
    Label6: TLabel;
    Edit6: TEdit;
    Label7: TLabel;
    Edit7: TEdit;
    Edit8: TEdit;
    Edit4: TEdit;
    Label4: TLabel;
    Edit3: TEdit;
    Label3: TLabel;
    Edit2: TEdit;
    Label2: TLabel;
    Edit1: TEdit;
    Label1: TLabel;
    Label8: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

procedure ViewSatHeader(SatToLoad : integer);


implementation

{$R *.dfm}

uses
   DEMEros;

procedure ViewSatHeader(SatToLoad : integer);
var
   SatHeaderForm: TSatHeaderForm;
   i : integer;
begin
   SatHeaderForm := TSatHeaderForm.Create(Application);
   with SatHeaderForm,SatImage[SatToLoad] do begin
      Caption := 'Header, ' + SceneBaseName;
      Edit1.Text := SceneTitle;
      Edit2.Text := IntToStr(NumBands);
      Edit3.Text := IntToStr(NumSatCol);
      Edit4.Text := IntToStr(NumSatRow);
      Edit5.Text := IntToStr(SelectionMap.MapDraw.SatView.BandInWindow);
      if NumBands = 1 then begin
         Edit6.Visible := false;
         Edit7.Visible := false;
         Edit8.Visible := false;
      end
      else begin
         Edit6.Text := IntToStr(SelectionMap.MapDraw.SatView.RedBand);
         Edit7.Text := IntToStr(SelectionMap.MapDraw.SatView.GreenBand);
         Edit8.Text := IntToStr(SelectionMap.MapDraw.SatView.BlueBand);
      end;
      StringGrid1.RowCount := succ(NumBands);
      for i := 1 to NumBands do begin
         StringGrid1.Cells[0,i] := IntToStr(i);
         StringGrid1.Cells[1,i] := BandLongName[i];
         StringGrid1.Cells[2,i] := BFileName[i];
      end;
      ShowModal;
      SatHeaderForm.Destroy;
   end;
end;


procedure TSatHeaderForm.FormCreate(Sender: TObject);
begin
   with StringGrid1 do begin
      ColWidths[0] := 50;
      ColWidths[1] := 200;
      ColWidths[2] := 400;
      Cells[0,0] := 'Band';
      Cells[1,0] := 'Name';
      Cells[2,0] := 'FileName';
   end;
end;


procedure TSatHeaderForm.OKBtnClick(Sender: TObject);
begin
   Close;
end;

procedure TSatHeaderForm.FormClose(Sender: TObject;  var Action: TCloseAction);
begin
   Action := caFree;
end;


end.
