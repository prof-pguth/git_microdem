program PointCloud;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.Types,
  FPointCloud in 'FPointCloud.pas' {PointCloudForm},
  FMXU.Context.DX11;

{$R *.res}

begin
   RegisterDX11ContextU;  //has to be done before the form creation

   Application.Initialize;
   Application.CreateForm(TPointCloudForm, PointCloudForm);
   Application.Run;
end.
