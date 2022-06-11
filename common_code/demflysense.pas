unit demflysense;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch }
{   file verified 6/22/2011       }
{_________________________________}


{$IfDef RecordProblems}   //normally only defined for debugging specific problems
{$EndIf}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons;

type
  TFlightControlSensitivity = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    OKBtn: TBitBtn;
  private
    { Private declarations }
  public
    { Public declarations }
  end;


procedure ChangeFlightControls;

implementation

{$R *.DFM}

uses
   PETMAR,Petmar_types,DEMdefs;


procedure ChangeFlightControls;
var
  FlightControlSensitivity : TFlightControlSensitivity;
begin
   FlightControlSensitivity := TFlightControlSensitivity.Create(Application);
   with MDdef.FlyOptions,FlightControlSensitivity do begin
      Edit1.Text := IntToStr(DeltaSpeed);
      Edit2.Text := RealToString(DeltaHeading,-8,-2);
      Edit3.Text := IntToStr(Deltaz);
      ShowModal;
      CheckEditString(Edit1.Text,DeltaSpeed);
      CheckEditString(Edit2.Text,DeltaHeading);
      CheckEditString(Edit3.Text,DeltaZ);
      Free;
   end;
end;

end.
