program PointCloud;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.Types,
  FPointCloud in 'FPointCloud.pas' {PointCloudForm},
  FMXU.Context.DX11,
  petmar in '..\microdem_open_source\petmar.pas',
  petmar_types in '..\microdem_open_source\petmar_types.pas',
  demdefs in '..\microdem_open_source\demdefs.pas',
  PETMath in '..\microdem_open_source\PETMath.pas';

{$R *.res}

begin
   RegisterDX11ContextU;  //has to be done before form creation

   Application.Initialize;
   Application.CreateForm(TPointCloudForm, PointCloudForm);
  Application.Run;
end.
