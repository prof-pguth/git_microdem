unit Petsplsh;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Copyright  1987--2013 by        }
{ PETMAR Trilobite Breeding Ranch }
{   file verified 5/21/2013       }
{_________________________________}


{$I nevadia_defines.inc}

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ExtCtrls,
  PETMAR;

type
  TSplashForm = class(TForm)
    Panel1: TPanel;
    Label2: TLabel;
    Label3: TLabel;
    Label1: TLabel;
    Image1: TImage;
    Label4: TLabel;
    ProgramIcon: TImage;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ProgramIconDblClick(Sender: TObject);
    procedure Label4Click(Sender: TObject);
    procedure Label4DblClick(Sender: TObject);
  end;


procedure StopSplashing;
procedure StartSplashing(ProgramName : String; TheIcon : TIcon);


var
  SplashForm: TSplashForm;
  StillSplashing : boolean;

implementation

{$R *.DFM}

uses
   MMSystem,Petmar_types;

var
   ShowIcon : TIcon;


procedure StartSplashing(ProgramName : String; TheIcon : TIcon);
begin
{$IfDef NoSplash}
   StillSplashing := false;
{$Else}
  ShowIcon := TheIcon;
  if not StillSplashing then SplashForm := TSplashForm.Create(Application);
  while length(ProgramName) < 16 do ProgramName := ' ' + ProgramName;
  SplashForm.Label1.Caption := ProgramName;
  SplashForm.Show;
  SplashForm.Update;
{$EndIf}
end;


procedure StopSplashing;
begin
   if StillSplashing then begin
      SplashForm.Hide;
      SplashForm.Free;
      StillSplashing := false;
   end;
end;



procedure TSplashForm.FormShow(Sender: TObject);
var
   BitMap : tMyBitmap;
begin
   Bitmap := tMyBitmap.Create;
   Bitmap.Height := 160;
   BitMap.Width := 110;
   with Bitmap.Canvas do begin
      Pen.Color := clYellow;
      Rectangle(0,0,110,160);
      Brush.Color := clYellow;
      FloodFill(5,5,clYellow,fsBorder);
   end;
   Image1.Picture.Graphic := Bitmap;
   DisplayNevadella(Image1.Canvas,4,-45,1,clBlue);
   Bitmap.Free;
end;

procedure PlaySound;
begin
end;

procedure TSplashForm.FormCreate(Sender: TObject);
begin
   Petmar.CheckFormPlacement(Self);
   FormShow(Sender);
   StillSplashing := true;
end;

procedure TSplashForm.ProgramIconDblClick(Sender: TObject);
begin
   SplashForm.Visible := false;
end;

procedure TSplashForm.Label4Click(Sender: TObject);
begin
   Application.HelpJump('PETMAR_story');
end;

procedure TSplashForm.Label4DblClick(Sender: TObject);
begin
   Application.HelpJump('PETMAR_story');
end;


initialization
   StillSplashing := false;
finalization
end.
