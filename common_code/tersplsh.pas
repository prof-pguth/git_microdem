unit Tersplsh;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch }
{   file verified 6/22/2011       }
{_________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   //{$Define RecordSplashProblems}
{$EndIf}


interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ExtCtrls, Buttons, Vcl.Imaging.pngimage;

type
  TTerBaseSplashForm = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    Label6: TLabel;
    Panel2: TPanel;
    Image2: TImage;
    BitBtn3: TBitBtn;
    SpeedButton1: TSpeedButton;
    Options: TBitBtn;
    BitBtn1: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BitBtn3Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure OptionsClick(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
  end;


procedure MDStopSplashing;
procedure MDStartSplashing;


var
  MDSplashForm : TTerBaseSplashForm;
                                                   
implementation

{$R *.DFM}

uses
   PETMAR,Petmar_types,DEMOptions, Nevadia_main;


procedure MDStartSplashing;
begin
   {$IfDef RecordSplashProblems} WriteLineToDebugFile('Start splash screen');  {$EndIf}

   if (MDSplashForm = nil) then MDSplashForm := TTerBaseSplashForm.Create(Application);
   MDSplashForm.Show;
   MDSplashForm.Update;
   CheckFormPlacement(MDSplashForm);
end;


procedure MDStopSplashing;
begin
   if (MDSplashForm <> nil) then begin
      {$IfDef RecordSplashProblems}  WriteLineToDebugFile('Close splash screen');   {$EndIf}
      MDSplashForm.Close;
      MDSplashForm := nil;
   end;
end;


procedure TTerBaseSplashForm.FormCreate(Sender: TObject);
begin
   Label6.Caption := 'Build ' + BuildString;
   Label1.Caption := ShortEXEName;
   {$IfDef Win32}
   Panel1.Color := clBlue;
   Label1.Caption := 'Luddite 32 bit ' + ShortEXEName;
   Label1.Font.Color := clYellow;
   Label6.Font.Color := clYellow;
   {$EndIf}
   CheckFormPlacement(Self);
end;


procedure TTerBaseSplashForm.OptionsClick(Sender: TObject);
begin
{$IfDef ExSetOptions}
{$Else}
   ChangeOptions;
{$EndIf}
end;

procedure TTerBaseSplashForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   Action := caFree;
end;


procedure TTerBaseSplashForm.BitBtn1Click(Sender: TObject);
begin
   Petmar.DisplayHTMLTopic('html\splash_screen_messages.htm');
end;

procedure TTerBaseSplashForm.BitBtn3Click(Sender: TObject);
begin
   Petmar.DisplayHTMLTopic('html\micr2zsn.htm');
end;


procedure TTerBaseSplashForm.SpeedButton1Click(Sender: TObject);
begin
   StopSplashing;
end;


initialization
   {$IfDef MessageStartUpUnitProblems} MessageToContinue('Startup tersplash'); {$EndIf}
   MDSplashForm := nil;
finalization
end.



