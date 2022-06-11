unit thread_timers;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch }
{   file verified 6/22/2011       }
{_________________________________}


{$I nevadia_defines.inc}


{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   //{$Define ThreadTimerCreation}
{$EndIf}


interface

uses
  Windows, Messages, SysUtils,  Classes, Graphics, Controls, Forms,
  Dialogs, Gauges, StdCtrls, Buttons,
  Petmar_types, Vcl.ComCtrls;

type
  TThreadTimerForm = class(TForm)
    Gauge1: TGauge;
    Gauge2: TGauge;
    Gauge3: TGauge;
    Gauge4: TGauge;
    Gauge5: TGauge;
    Gauge6: TGauge;
    Gauge7: TGauge;
    Gauge8: TGauge;
    OverallGauge9: TGauge;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    StatusBar1: TStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure UpdateThreadStats(i,Done : integer; ShortMess : ShortString = '');
    procedure EnableGauge(ThreadNum : integer; Setting : boolean; Capt : ShortString = '');
  end;

var
   ThreadTimers  : TThreadTimerForm;

implementation

{$R *.dfm}

uses
   Nevadia_Main,Petmar,DEMDefs;


procedure TThreadTimerForm.EnableGauge(ThreadNum : integer; Setting : boolean; Capt : ShortString = '');
begin
   case ThreadNum of
      1 : Gauge1.Visible := Setting;
      2 : Gauge2.Visible := Setting;
      3 : Gauge3.Visible := Setting;
      4 : Gauge4.Visible := Setting;
      5 : Gauge5.Visible := Setting;
      6 : Gauge6.Visible := Setting;
      7 : Gauge7.Visible := Setting;
      8 : Gauge8.Visible := Setting;
   end;
   UpdateThreadStats(ThreadNum,0);
   case ThreadNum of
      1 : Label1.Caption := Capt;
      2 : Label2.Caption := Capt;
      3 : Label3.Caption := Capt;
      4 : Label4.Caption := Capt;
      5 : Label5.Caption := Capt;
   end;
   //ApplicationProcessMessages;
end;


procedure TThreadTimerForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   Action := caFree;
end;

procedure TThreadTimerForm.FormCreate(Sender: TObject);
begin
   wmdem.FormPlacementInCorner(self);
   UpdateThreadStats(9,0);
end;

procedure TThreadTimerForm.UpdateThreadStats(i,Done : integer; ShortMess : ShortString = '');
begin
   case i of
      1 : Gauge1.Progress := Done;
      2 : Gauge2.Progress := Done;
      3 : Gauge3.Progress := Done;
      4 : Gauge4.Progress := Done;
      5 : Gauge5.Progress := Done;
      6 : Gauge6.Progress := Done;
      7 : Gauge7.Progress := Done;
      8 : Gauge8.Progress := Done;
      9 : OverallGauge9.Progress := Done;
   end;
   if (ShortMess <> '') then StatusBar1.Panels[1].Text := ShortMess;
   //ApplicationProcessMessages;
end;


initialization
finalization
end.

