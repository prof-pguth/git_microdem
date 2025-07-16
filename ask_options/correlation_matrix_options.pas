unit correlation_matrix_options;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{________________________________________}


{$I nevadia_defines.inc}


{$IFDEF DEBUG}
{$ELSE}
{$ENDIF}


interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  DEMstringgrid, Vcl.Buttons;

type
  TCorrelationMatrixOptionsForm = class(TForm)
    RadioGroup1: TRadioGroup;
    RadioGroup2: TRadioGroup;
    RedrawSpeedButton12: TSpeedButton;
    RadioGroup3: TRadioGroup;
    Label1: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Label2: TLabel;
    procedure RedrawSpeedButton12Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure RadioGroup3Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    DisplayGrid : tGridForm;
  end;


procedure InteractiveCorrelativeMatrix(GridForm : tGridForm);

implementation

{$R *.dfm}

uses
   Petmar,DEMdefs,DEMdef_routines;


procedure InteractiveCorrelativeMatrix(GridForm : tGridForm);
var
   CorrelationMatrixOptionsForm : TCorrelationMatrixOptionsForm;
begin
   CorrelationMatrixOptionsForm := TCorrelationMatrixOptionsForm.Create(Application);
   CorrelationMatrixOptionsForm.DisplayGrid := GridForm;
   CorrelationMatrixOptionsForm.Edit1.Text := GridForm.ULstring;
   CorrelationMatrixOptionsForm.Caption := 'Options for ' + GridForm.Caption;
   CorrelationMatrixOptionsForm.Show;
end;


procedure TCorrelationMatrixOptionsForm.Edit1Change(Sender: TObject);
begin
   DisplayGrid.ULstring := Edit1.Text;
end;

procedure TCorrelationMatrixOptionsForm.Edit2Change(Sender: TObject);
begin
   CheckEditString(Edit2.Text,MDDef.PerfectR);
end;

procedure TCorrelationMatrixOptionsForm.FormCreate(Sender: TObject);
begin
   RadioGroup1.ItemIndex := pred(MDDef.CR_ColorPalette);
   if MDDef.CR_MatrixEqualization then RadioGroup2.ItemIndex := 0 else RadioGroup2.ItemIndex := 1;
   RadioGroup3.ItemIndex := pred(MDDef.CR_Decimals);
end;

procedure TCorrelationMatrixOptionsForm.RadioGroup1Click(Sender: TObject);
begin
   MDDef.CR_ColorPalette := succ(RadioGroup1.ItemIndex);
end;

procedure TCorrelationMatrixOptionsForm.RadioGroup2Click(Sender: TObject);
begin
   MDDef.CR_MatrixEqualization := (RadioGroup2.ItemIndex = 0);
end;

procedure TCorrelationMatrixOptionsForm.RadioGroup3Click(Sender: TObject);
begin
   MDDef.CR_Decimals := succ(RadioGroup3.ItemIndex);
end;

procedure TCorrelationMatrixOptionsForm.RedrawSpeedButton12Click(Sender: TObject);
begin
   DrawCorrelationDiagram(DisplayGrid);
end;



end.
