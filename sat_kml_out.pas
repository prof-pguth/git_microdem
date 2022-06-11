unit sat_kml_out;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
{ Part of MICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch }
{_________________________________}

{$I nevadia_defines.inc}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   //{$Define KMLoutput}
{$EndIf}

interface

uses
  Windows, Messages, SysUtils,  Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls,
  Petmar_Types;

type
  TKML_overlay_setup = class(TForm)
    Edit1: TEdit;
    Edit2: TEdit;
    Image1: TImage;
    HelpBtn: TBitBtn;
    OKBtn: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CancelBtn: TBitBtn;
    procedure Edit1Change(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
     MapImage : tImage;
     ThisSatImage,TileSize,XTiles,YTiles,XSize,YSize : integer;
     ForgetThis : boolean;
     procedure FigureImage;
  end;


implementation

{$R *.dfm}

uses
   Petmar,DEMEros,DEMDefs, PETImage;

{ TKML_overlay_setup }

procedure TKML_overlay_setup.CancelBtnClick(Sender: TObject);
begin
   ForgetThis := true;
   Close;
end;

procedure TKML_overlay_setup.Edit1Change(Sender: TObject);
begin
   FigureImage;
end;

procedure TKML_overlay_setup.Edit2Change(Sender: TObject);
begin
   FigureImage;
end;

procedure TKML_overlay_setup.FigureImage;
var
   Bitmap : tMyBitmap;
   Size : integer;
   BlowUp : float64;
begin
   if (ThisSatImage <> 0) and (SatImage[ThisSatImage] <> Nil) then with SatImage[ThisSatImage] do begin
      CheckEditString(Edit1.Text,MDDef.KMLZoomSize);
      Blowup := 0.01 * MDDef.KMLZoomSize;
      CheckEditString(Edit2.Text,TileSize);
      {$IfDef KMLoutput} WriteLineToDebugFile('TKML_overlay_setup.FigureImage Blowup=' + RealToString(Blowup,-12,-2)  '  ize=' + IntToStr(TileSize)); {$EndIf}
      TileSize := round(TileSize / Blowup);
      Xtiles := 1;
      While (NumSatCol div XTiles > TileSize) do inc(XTiles);
      XSize := round(BlowUp * (NumSatCol div XTiles));
      Ytiles := 1;
      While (NumSatRow div YTiles > TileSize) do inc(YTiles);
      YSize := round(BlowUp * (NumSatRow div YTiles));
      Label1.Caption := 'Tiles: ' + IntToStr(XTiles) + 'x' + IntToStr(YTiles);
      Label2.Caption := 'Tile size: ' + IntToStr(XSize) + 'x' + IntToStr(YSize);
      Size := round(150 / MDDef.KMLZoomSize);
      PetImage.CreateBitmap(Bitmap,150,150);
      Bitmap.Canvas.CopyRect(Rect(0,0,149,149),MapImage.Canvas, Rect(MapImage.Width div 2-size,MapImage.Height div 2-size,MapImage.Width div 2+size,MapImage.Height div 2+size));
      Image1.Picture.Graphic := Bitmap;
      Bitmap.Free;
   end;
end;


procedure TKML_overlay_setup.FormCreate(Sender: TObject);
begin
   ForgetThis := false;
end;

procedure TKML_overlay_setup.HelpBtnClick(Sender: TObject);
begin
   DisplayHTMLTopic('html/sat_kml_setup.htm');
end;

initialization
finalization
  {$IfDef KMLoutput} WriteLineToDebugFile('KMLoutput active in sat_kml_out'); {$Endif}
end.
