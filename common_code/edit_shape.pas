unit edit_shape;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
{ Part of MICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch }
{_________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
{$EndIf}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls;

type
  TShapeEditForm = class(TForm)
    RadioGroup1: TRadioGroup;
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    HelpBtn: TBitBtn;
    procedure CancelBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

procedure StartShapeEdit;

implementation

{$R *.dfm}

uses
   DEMdefs,DEMMapf;

var
  ShapeEditForm: TShapeEditForm;

procedure StartShapeEdit;
begin
   If (ShapeEditForm = Nil) then ShapeEditForm := TShapeEditForm.Create(Application);
   ShapeEditForm.Show;
end;

procedure TShapeEditForm.CancelBtnClick(Sender: TObject);
begin
   ChangeDEMNowDoing(JustWandering);
   Close;
end;

procedure TShapeEditForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   Action := caFree;
end;

initialization
   ShapeEditForm := Nil;
end.
