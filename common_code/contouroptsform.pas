unit contouroptsform;

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
  Dialogs, StdCtrls, Buttons,


  BaseGraf;

type
  TSimpleContourOptions = class(TForm)
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    Label1: TLabel;
    Edit1: TEdit;
    Label2: TLabel;
    Edit2: TEdit;
    procedure BitBtn1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


implementation

{$R *.dfm}

uses
   Petmar,Petmath,DEMdefs,Petmar_types;

procedure TSimpleContourOptions.BitBtn1Click(Sender: TObject);
begin
   Close;
end;

procedure TSimpleContourOptions.BitBtn2Click(Sender: TObject);
begin
    Petmar.PickLineSizeAndColor('Contour lines',BitBtn2,MDDef.ContourLineColor,MDDef.ContourLineThick);
end;

procedure TSimpleContourOptions.BitBtn3Click(Sender: TObject);
begin
    Petmar.PickLineSizeAndColor('Delaunay triangles',BitBtn3,MDDef.DelaunayLineColor,MDDef.DelaunayLineThick);
end;


procedure TSimpleContourOptions.FormCreate(Sender: TObject);
begin
   ColorLineWidthBitBtn(BitBtn2,MDDef.ContourLineColor,MDDef.ContourLineThick);
   ColorLineWidthBitBtn(BitBtn3,MDDef.DelaunayLineColor,MDDef.DelaunayLineThick);
   Edit2.Text := RealToString(MDDef.MaxTriSide,-12,-6);
   Petmar.PlaceFormAtMousePosition(Self);
end;


initialization
finalization
end.
