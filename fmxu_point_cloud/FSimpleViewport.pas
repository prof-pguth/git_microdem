unit FSimpleViewport;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  System.Math.Vectors, FMX.Controls3D, FMX.Objects3D, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Viewport3D, FMX.Types3D, FMX.MaterialSources,
  FMXU.WebGPU.Materials, FMXU.Viewport3D;

type
  TForm27 = class(TForm)
    Viewport3D: TViewport3D;
    LabelFPS: TLabel;
    Panel1: TPanel;
    RoundCube1: TRoundCube;
    LightMaterialSource1: TLightMaterialSource;
    Light1: TLight;
    Sphere1: TSphere;
    Plane1: TPlane;
    Dummy1: TDummy;
    Light2: TLight;
    Timer1: TTimer;
    Light3: TLight;
    LightMaterialSource2: TLightMaterialSource;
    StrokeCube1: TStrokeCube;
    StrokeCube2: TStrokeCube;
    StrokeCube3: TStrokeCube;
    procedure FormCreate(Sender: TObject);
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

procedure TForm27.FormCreate(Sender: TObject);
begin
   Application.OnIdle := DoOnIdle;
end;

procedure TForm27.Timer1Timer(Sender: TObject);
begin
   LabelFPS.Text := Format('%.1f ms', [ Viewport3D.LastPaintSeconds*1000 ]);
end;

// DoOnIdle
//
procedure TForm27.DoOnIdle(Sender: TObject; var Done: Boolean);
begin
   Dummy1.RotationAngle.Y := Frac(Now) * 86400 * 30;
   Viewport3D.Repaint;
end;

end.
