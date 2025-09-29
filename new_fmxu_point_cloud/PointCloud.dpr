program PointCloud;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.Types,
  FPointCloud in 'FPointCloud.pas' {PointCloudForm},
  FMXU.Context.DX11,
  petmar in '..\microdem_open_source\petmar.pas',
  petmar_types in '..\microdem_open_source\petmar_types.pas';

{$R *.res}

begin
   RegisterDX11ContextU;  //has to be done before the form creation

   Application.Initialize;
   AApplication.CreateForm(TPointCloudForm, PointCloudForm);
  pplication.Run;
end.
