unit demflycontrols;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program       }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2024 Peter L. Guth   }
{____________________________________}


{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   //{$Define RecordChangeFlyControls}
{$EndIf}


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Spin,
  ComCtrls, Buttons;

type
  TFlightControlForm = class(TForm)
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Speed: TLabel;
    Heading: TLabel;
    Elevation: TLabel;
    SpinButton1: TSpinButton;
    SpinButton2: TSpinButton;
    SpinButton3: TSpinButton;
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Button2: TButton;
    CheckBox1: TCheckBox;
    Button4: TButton;
    Button5: TButton;
    Edit4: TEdit;
    Label3: TLabel;
    SpinButton4: TSpinButton;
    TrackBar1: TTrackBar;
    Label4: TLabel;
    BitBtn1: TBitBtn;
    Label5: TLabel;
    SpinButton5: TSpinButton;
    procedure SpinButton2DownClick(Sender: TObject);
    procedure SpinButton2UpClick(Sender: TObject);
    procedure SpinButton1DownClick(Sender: TObject);
    procedure SpinButton1UpClick(Sender: TObject);
    procedure SpinButton3DownClick(Sender: TObject);
    procedure SpinButton3UpClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure SpinButton4DownClick(Sender: TObject);
    procedure SpinButton4UpClick(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure SpinButton5DownClick(Sender: TObject);
    procedure SpinButton5UpClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
     PitchUp,PitchDown,SaveFlight,
     WantToEndFlying,EditColors,EditControls : boolean;
  end;

var
  FlightControlForm: TFlightControlForm;

implementation

{$R *.DFM}


uses
   Petmath,PETMar,DEMdefs,Petmar_types;


procedure TFlightControlForm.SpinButton2DownClick(Sender: TObject);
var
   Heading : float64;
begin
   CheckEditString(Edit2.Text,Heading);
   Heading := FindCompassAngleInRange(Heading - MDdef.FlyOptions.DeltaHeading);
   Edit2.Text := RealToString(Heading,-8,0);
end;


procedure TFlightControlForm.SpinButton2UpClick(Sender: TObject);
var
   Heading : float64;
begin
   CheckEditString(Edit2.Text,Heading);
   Heading := FindCompassAngleInRange(Heading + MDdef.FlyOptions.DeltaHeading);
   Edit2.Text := RealToString(Heading,-8,0);
end;


procedure TFlightControlForm.SpinButton1DownClick(Sender: TObject);
var
   Speed : float64;
begin
   CheckEditString(Edit1.Text,Speed);
   Speed := Speed - MDdef.FlyOptions.DeltaSpeed;
   if (Speed < 0) then Speed := 0;
   Edit1.Text := RealToString(Speed,-8,0);
end;

procedure TFlightControlForm.SpinButton1UpClick(Sender: TObject);
var
   Speed : float64;
begin
   CheckEditString(Edit1.Text,Speed);
   Speed := Speed + MDdef.FlyOptions.DeltaSpeed;
   Edit1.Text := RealToString(Speed,-8,0);
end;


procedure TFlightControlForm.SpinButton3DownClick(Sender: TObject);
var
   z : float64;
begin
   if MDdef.FlyOptions.LiveFlyAutoNap then MessageToContinue('Auto NAP flight is on')
   else begin
      CheckEditString(Edit3.Text,z);
      z := z - MDdef.FlyOptions.DeltaZ;
      if (z < 0) then z := 0;
      Edit3.Text := RealToString(z,-8,0);
   end;
end;


procedure TFlightControlForm.SpinButton3UpClick(Sender: TObject);
var
   z : float64;
begin
   if MDdef.FlyOptions.LiveFlyAutoNap then MessageToContinue('Auto NAP flight on')
   else begin
      CheckEditString(Edit3.Text,z);
      z := z + MDdef.FlyOptions.DeltaZ;
      Edit3.Text := RealToString(z,-8,0);
   end;
end;



procedure TFlightControlForm.Button1Click(Sender: TObject);
begin
   WantToEndFlying := true;
   {$IfDef RecordProblems} WriteLineToDebugFile('Try to close flying window'); {$EndIf}
end;


procedure TFlightControlForm.FormCreate(Sender: TObject);
begin
   {$IfDef RecordChangeFlyControls}   WriteLineToDebugFile('TFlightControlForm.FormCreate 1'); {$EndIf}
   WantToEndFlying := false;
   CheckBox1.Checked := MDdef.FlyOptions.LiveFlyAutoNap;
   EditColors := false;
   EditControls := false;
   PitchUp := false;
   PitchDown := false;
   {$IfDef RecordChangeFlyControls}   WriteLineToDebugFile('TFlightControlForm.FormCreate 2'); {$EndIf}

   Edit4.Text := IntToStr(MDdef.FlyOptions.LiveFlyDelay);
   {$IfDef RecordChangeFlyControls}    WriteLineToDebugFile('TFlightControlForm.FormCreate 3'); {$EndIf}
end;

procedure TFlightControlForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
   case ord(Key) of
      VK_Left  : SpinButton2DownClick(Sender);
      VK_Right : SpinButton2UpClick(Sender);
      VK_Up    : SpinButton3UpClick(Sender);
      VK_Down  : SpinButton3DownClick(Sender);
   end;
   case key of
      'P' : Button2Click(Sender);
      'S' : if AnswerIsYes('Confirm stop') then Button1Click(Sender);
   end;
end;


procedure TFlightControlForm.FormKeyDown(Sender: TObject; var Key: Word;  Shift: TShiftState);
var
   aKey : Char;
begin
   aKey := Chr(key);
   FormKeyPress(Sender,aKey);
end;


procedure TFlightControlForm.Button2Click(Sender: TObject);
begin
   MessageToContinue('Fly through paused.' + MessLineBreak + 'Resume when ready.');
end;


procedure TFlightControlForm.Button4Click(Sender: TObject);
begin
   EditColors := true;
end;


procedure TFlightControlForm.Button5Click(Sender: TObject);
begin
   EditControls := true;
end;


procedure TFlightControlForm.SpinButton4DownClick(Sender: TObject);
begin
   if (MDDef.FlyOptions.LiveFlyDelay > 100) then  MDDef.FlyOptions.LiveFlyDelay := MDdef.FlyOptions.LiveFlyDelay - 100;
   Edit4.Text := IntToStr(MDdef.FlyOptions.LiveFlyDelay);
end;

procedure TFlightControlForm.SpinButton4UpClick(Sender: TObject);
begin
   MDdef.FlyOptions.LiveFlyDelay := MDdef.FlyOptions.LiveFlyDelay + 100;
   Edit4.Text := IntToStr(MDdef.FlyOptions.LiveFlyDelay);
end;

procedure TFlightControlForm.BitBtn1Click(Sender: TObject);
begin
  TrackBar1.Position := 0;
end;

procedure TFlightControlForm.TrackBar1Change(Sender: TObject);
begin
   {$IfDef RecordChangeFlyControls} WriteLineToDebugFile('Changed fly look direction'); {$EndIf}
end;


procedure TFlightControlForm.SpinButton5DownClick(Sender: TObject);
begin
   PitchDown := true;
end;


procedure TFlightControlForm.SpinButton5UpClick(Sender: TObject);
begin
   PitchUp := true;
end;

initialization
finalization
   {$IfDef RecordChangeFlyControls}   WriteLineToDebugFile('RecordChangeFlyControls active in demflycontrols'); {$EndIf}
end.
