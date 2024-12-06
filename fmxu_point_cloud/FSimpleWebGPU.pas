unit FSimpleWebGPU;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms3D, FMX.Types3D, FMX.Forms, FMX.Graphics, 
  FMX.Dialogs, System.Math.Vectors, FMX.Controls3D, FMX.Objects3D,
  FMX.MaterialSources,
  FMXU.Viewport3D;

type
  TForm27 = class(TForm3D)
    Cube1: TCube;
    CMSBlue: TColorMaterialSource;
    Cube2: TCube;
    TMSBrick: TTextureMaterialSource;
    Timer1: TTimer;
    procedure Form3DCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure DoOnIdle(Sender: TObject; var Done: Boolean);
  end;

var
  Form27: TForm27;

implementation

{$R *.fmx}

procedure TForm27.Form3DCreate(Sender: TObject);
begin
   Application.OnIdle := DoOnIdle;
end;

procedure TForm27.Timer1Timer(Sender: TObject);
begin
   Caption := Format('%.3f ms', [ LastPaintSeconds * 1000 ]);
end;

// DoOnIdle
//
procedure TForm27.DoOnIdle(Sender: TObject; var Done: Boolean);
begin
   var angle := Frac(Now) * 86400 * 20;
   Cube1.RotationAngle.Y := Frac(Now) * 86400 * 20;
   Cube2.Position.Y := -3 + Cos(angle * 0.1);
   Invalidate;
end;

end.
