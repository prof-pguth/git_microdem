unit sc_COLHEAD;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{________________________________________}

{$I nevadia_defines.inc}

interface

uses
   Windows, Classes, Graphics, Forms, Controls, Buttons,
   StdCtrls, Dialogs, ExtCtrls,
   PETMAR,Petmar_types,sc_ColMain;

type
  TColumnHeaderEntry = class(TForm)
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    HelpBtn: TBitBtn;
    Bevel1: TBevel;
    FontButton: TButton;
    LocationButton: TButton;
    DirectionButton: TButton;
    LabelButton: TButton;
    FontDialog1: TFontDialog;
    FontLabel: TLabel;
    LocationLabel: TLabel;
    LabelLabel: TLabel;
    procedure FontButtonClick(Sender: TObject);
    procedure DirectionButtonClick(Sender: TObject);
    procedure LocationButtonClick(Sender: TObject);
    procedure LabelButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function ChangeHeaderInfo(TimeLabelFile : boolean; var Lat,Long : float64; var InFont : TFont; var CorrelationDir : AnsiChar;  var ColumnLabel : ShortString) : boolean;

implementation

{$R *.DFM}

uses
   PETMath,GetLatLn;

var
   Dir : AnsiChar;
   ColLat,ColLong : float64;
   ColLabel       : ShortString;
   SaveFont       : TFont;

function ChangeHeaderInfo(TimeLabelFile : boolean; var Lat,Long : float64; var InFont : TFont; var CorrelationDir : AnsiChar;  var ColumnLabel : ShortString) : boolean;
var
   HeadDlg : TColumnHeaderEntry;
begin
   HeadDlg := TColumnHeaderEntry.Create(Application);
   with HeadDlg do begin
      if TimeLabelFile then begin
         LocationButton.Visible := false;
         LabelButton.Visible := false;
         LocationLabel.Caption := '';  
         LabelLabel.Caption := '';
      end
      else begin
         LocationButton.Visible := true;
         LabelButton.Visible := true;
         LocationLabel.Caption := LatLongDegreeToString(Lat,Long,DecMinutes);
         LabelLabel.Caption := ColumnLabel;
      end;
      FontDialog1.Font := InFont;
      FontLabel.Font := InFont;
      SaveFont := InFont;
      Dir := CorrelationDir;
      ColLat := Lat;
      ColLong := Long;
      ColLabel := ColumnLabel;
      if ShowModal = mrCancel then
         ChangeHeaderInfo := false
      else begin
         ChangeHeaderInfo := true;
         InFont := SaveFont;
         CorrelationDir := Dir;
         Lat := ColLat;
         Long := ColLong;
         ColumnLabel := ColLabel;
      end;
   end;
   HeadDlg.Destroy;
end;


procedure TColumnHeaderEntry.FontButtonClick(Sender: TObject);
begin
   FontDialog1.Execute;
   FontLabel.Font := FontDialog1.Font;
   SaveFont := FontDialog1.Font;
end;

procedure TColumnHeaderEntry.DirectionButtonClick(Sender: TObject);
begin
   if Dir = 'H' then Dir := 'V' else Dir := 'H';
end;


procedure TColumnHeaderEntry.LocationButtonClick(Sender: TObject);
begin
   GetLatLn.GetLatLongNoDatum('Column location',ColLat,ColLong);
   LocationLabel.Caption := LatLongDegreeToString(ColLat,ColLong,DecMinutes);
end;

procedure TColumnHeaderEntry.LabelButtonClick(Sender: TObject);
var
   TStr : ShortString;
begin
   TStr := ColLabel;
   GetString('Column header',TStr,false,ReasonableTextChars);
   ColLabel := TStr;
   LabelLabel.Caption := ColLabel;
end;


end.
