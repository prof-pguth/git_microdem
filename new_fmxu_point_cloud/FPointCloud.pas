{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2026 Peter L. Guth  }
{________________________________________}

//this has to be compiled in Delphi 12, version 23.0

{based on:       }
{https://www.delphitools.info/2024/10/29/supporting-gpu-buffers-for-fmx/}
{https://www.delphitools.info/2024/09/05/faster-3d-point-cloud-with-fmx/}
{https://www.delphitools.info/2024/09/01/exploring-3d-point-clouds-for-fmx/}
{https://github.com/EricGrange}


//{$I nevadia_defines.inc}

{$Define Recordfmxu_pointcloud}
{$Define Recordfmxu_import}

unit FPointCloud;

interface

{$i fmxu.inc}

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.UIConsts, System.Math, System.Diagnostics, System.Math.Vectors,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Viewport3D,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Controls3D,
  FMX.Objects3D, FMX.MaterialSources, FMX.Types3D, System.RTLConsts,
  FMX.Edit, FMX.EditBox, FMX.ComboTrackBar, FMX.ListBox,
  FMXU.D3DShaderCompiler, FMXU.PointCloud, FMXU.Buffers, FMXU.Material.PointColor,
  FMXU.Viewport3D, FMXU.Scene,FMXU.Context.DX11;

type
  TPointCloudForm = class(TForm)
    Viewport3D1: TViewport3D;
    Camera: TCamera;
    DummyTarget: TDummy;
    Panel1: TPanel;
    CBShape: TComboBox;
    CTBPointSize: TComboTrackBar;
    Label1: TLabel;
    Label2: TLabel;
    TimerFPS: TTimer;
    OpenDialog: TOpenDialog;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
    procedure CBShapeChange(Sender: TObject);
    procedure CTBPointSizeChangeTracking(Sender: TObject);
    procedure TimerFPSTimer(Sender: TObject);
    procedure Viewport3D1Painting(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure CheckBox1Change(Sender: TObject);
    procedure CheckBox2Change(Sender: TObject);
    procedure CheckBox3Change(Sender: TObject);
  private
    { Private declarations }
    FMouseDownPos : TPoint3D;
    FPointCloud   : array[1..5] of tPointCloud3D;
    FRenderCount : Integer;
    FRenderTicks : TStopwatch;
    FRunningFPS : Double;

    procedure OnApplicationIdle(Sender: TObject; var Done: Boolean);
    procedure AutoCenterAndScale(Cloud : TPointCloud3D);
    procedure LoadClouds(FileList : tStringList);
  public
    { Public declarations }
  end;


procedure Startfmxu_point_cloud_viewer;

var
   PointCloudForm : TPointCloudForm;

implementation

{$R *.fmx}

uses
  Petmar,Petmar_types,PetMath,DEMDefs;


procedure Startfmxu_point_cloud_viewer;
var
   FileList : tStringList;
   fName : shortstring;
begin
   //RegisterDX11ContextU;  //has to be done before form creation
   //PointCloudForm := TPointCloudForm.Create(Application);
   DebugFilename := 'c:\microdem\logs\md_PointCloudViewer.txt';
   Petmar_types.OpenDebugFile;
   fName := ParamStr(1);
   if FileExists(fName) then begin
       {$IfDef Recordfmxu_pointcloud} WriteLineToDebugFile('Startfmxu_point_cloud_viewer in ' + fName); {$EndIf}
       FileList := tStringList.Create;
       FileList.LoadFromFile(fName);
       PointCloudForm.LoadClouds(FileList);
       PointCloudForm.Show;
       FileList.Destroy;
       {$IfDef Recordfmxu_pointcloud} WriteLineToDebugFile('Startfmxu_point_cloud_viewer out'); {$EndIf}
   end
   else begin
       {$IfDef Recordfmxu_pointcloud} WriteLineToDebugFile('Startfmxu_point_cloud_viewer missing ' + fName); {$EndIf}
   end;
end;


procedure TPointCloudForm.FormCreate(Sender: TObject);
var
   FileList : tStringList;
begin
   {$IfDef Recordfmxu_pointcloud} WriteLineToDebugFile('TPointCloudForm.FormCreate in'); {$EndIf}
   Application.OnIdle := OnApplicationIdle;
   FRenderTicks.Start;
   CheckBox1.Enabled := false;
   CheckBox2.Enabled := false;
   CheckBox3.Enabled := false;
   CheckBox3.IsChecked := false;
   CheckBox2.IsChecked := false;
   CheckBox1.IsChecked := false;
   Startfmxu_point_cloud_viewer;
   {$IfDef Recordfmxu_pointcloud} WriteLineToDebugFile('TPointCloudForm.FormCreate out'); {$EndIf}
end;


procedure TPointCloudForm.AutoCenterAndScale(Cloud : TPointCloud3D);
begin
   var bary := BufferBarycenter(Cloud.Points);
   var factor := 20 / BufferAverageDistance(Cloud.Points, bary);
   {$IfDef Recordfmxu_pointcloud} WriteLineToDebugFile('TPointCloudForm.AutoCenterAndScale, 1st guess factor=' + RealToString(factor,-12,-8)); {$EndIf}
   while Factor < 0.1 do Factor := 5 * factor;
   {$IfDef Recordfmxu_pointcloud} WriteLineToDebugFile('TPointCloudForm.AutoCenterAndScale, 2nd guess factor=' + RealToString(factor,-12,-8)); {$EndIf}
   BufferOffsetAndScale(Cloud.Points, -bary, factor);
   Cloud.UpdatePoints;
end;


procedure TPointCloudForm.LoadClouds(FileList : tStringList);

      procedure LoadFromMICRODEMxyzi(fName : String; var cloud : TPointCloud3D);
      var
         Points : pPointXYZIArray;
         tfile : File;
         Pts : integer;
         c : TAlphaColorRec;
         {$IfDef Recordfmxu_import} xsmin,xsmax,ysmin,ysmax,zsmin,zsmax : float32; {$Endif}

      begin
           {$IfDef Recordfmxu_pointcloud} WriteLineToDebugFile('LoadFromMICRODEMxyzi in ' + fName); {$EndIf}
           if FileExists(fName) then begin
              {$IfDef Recordfmxu_import}
                 xsmin := 99e38;
                 xsmax := -99e38;
                 ysmin := 99e38;
                 ysmax := -99e38;
                 zsmin := 99e38;
                 zsmax := -99e38;
              {$Endif}
              Cloud := TPointCloud3D.Create(Self);
              Cloud.Parent := Viewport3D1;
              Cloud.PointShape := TPointColorShape(1);
              {$IfDef Record3d} WriteLineToDebugFile('LoadMapWithColorByCodes in ' + PointsFile); {$EndIf}
              assignFile(tfile,fName);
              reset(tFile,sizeOf(tPointXYZI));
              new(Points);
              BlockRead(tfile,Points^,MaxPts,Pts);
              CloseFile(tFile);
              {$IfDef Recordfmxu_pointcloud} WriteLineToDebugFile('Read, pts=' + IntToStr(Pts)); {$EndIf}
              cloud.Points.Length := Pts;
              for var i := 1 to Pts do begin
                 if (abs(Points^[i].y) > 0.01) and (abs(Points^[i].x) > 0.01) then begin
                      {$IfDef Recordfmxu_import}
                         CompareValueToExtremes(Points^[i].y,xsmin,xsmax);
                         CompareValueToExtremes(Points^[i].x,ysmin,ysmax);
                         CompareValueToExtremes(-Points^[i].z,zsmin,zsmax);
                      {$Endif}

                      cloud.Points.Vertices[pred(i)] := Point3D(Points^[i].y,Points^[i].x,-Points^[i].z);   //reorder from GIS to screen axesx
                      c.B := Points^[i].int1;
                      c.G := Points^[i].int2;
                      c.R := Points^[i].int3;
                      c.A := 1;
                      cloud.Points.Color0[pred(i)] := c.Color;
                 end;
              end;
              Dispose(Points);
              {$IfDef Recordfmxu_import}
                 WriteLineToDebugFile('  x:' + RealToString(xsmin,-12,-2) + '--' + RealToString(xsmax,-12,-2) +
                                      '  y:' + RealToString(ysmin,-12,-2) + '--' + RealToString(ysmax,-12,-2) +
                                      '  z:' + RealToString(zsmin,-12,-2) + '--' + RealToString(zsmax,-12,-2));
              {$Endif}
              {$IfDef Recordfmxu_pointcloud} WriteLineToDebugFile('LoadFromMICRODEMxyzi out, Pts=' + IntToStr(Pts)); {$EndIf}
           end
           else begin
              {$IfDef Recordfmxu_pointcloud} WriteLineToDebugFile('LoadFromMICRODEMxyzi fail for ' + fName); {$EndIf}
           end;
      end;

var
   i : integer;
begin
   {$IfDef Recordfmxu_pointcloud} WriteLineToDebugFile('TPointCloudForm.LoadClouds in'); {$EndIf}
   for i := 1 to 3 do FPointCloud[i] := nil;

   for i := 0 to pred(FileList.Count) do begin
      if FileExists(FileList.Strings[i]) then begin
          LoadFromMICRODEMxyzi(FileList.Strings[i],FPointCloud[succ(i)]);
          case i of
             0 : CheckBox1.Enabled := true;
             1 : CheckBox2.Enabled := true;
             2 : CheckBox3.Enabled := true;
          end;
          case i of
             0 : CheckBox1.Text := ExtractFileNameNoExt(FileList.Strings[i]);
             1 : CheckBox2.Text := ExtractFileNameNoExt(FileList.Strings[i]);
             2 : CheckBox3.Text := ExtractFileNameNoExt(FileList.Strings[i]);
          end;
      end;
   end;
   if (FPointCloud[1] <> nil) then begin
      AutoCenterAndScale(FPointCloud[1]);
      CBShape.ItemIndex := Ord(pcsPoint);
      CTBPointSizeChangeTracking(nil);
      CBShapeChange(nil);
   end;
   {$IfDef Recordfmxu_pointcloud} WriteLineToDebugFile('TPointCloudForm.LoadClouds out'); {$EndIf}
end;


procedure TPointCloudForm.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
   MouseCapture;
   if Button = TMouseButton.mbLeft then begin
      FMouseDownPos.X := X;
      FMouseDownPos.Y := Y;
   end;
end;

procedure TPointCloudForm.FormMouseMove(Sender: TObject; Shift: TShiftState; X,Y: Single);
begin
   if Shift = [ssLeft ] then begin
      MoveControl3DAroundTarget(Camera, DummyTarget, (Y - FMouseDownPos.Y) * 0.2, (X - FMouseDownPos.X) * 0.2);
      FMouseDownPos.X := X;
      FMouseDownPos.Y := Y;
   end;
end;

procedure TPointCloudForm.FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
   ReleaseCapture;
end;

procedure TPointCloudForm.FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
begin
   Camera.AngleOfView := Camera.AngleOfView * Power(1.15, WheelDelta / 120);
end;

procedure TPointCloudForm.CBShapeChange(Sender: TObject);
begin
   FPointCloud[1].PointShape := TPointColorShape(CBShape.ItemIndex);
end;

procedure TPointCloudForm.CheckBox1Change(Sender: TObject);
begin
   FPointCloud[1].Visible := CheckBox1.IsChecked;
   CheckBox2.IsChecked := false;
   CheckBox3.IsChecked := false;
   //AutoCenterAndScale(FPointCloud[2]);
end;

procedure TPointCloudForm.CheckBox2Change(Sender: TObject);
begin
   FPointCloud[2].Visible := CheckBox2.IsChecked;
   CheckBox1.IsChecked := false;
   CheckBox3.IsChecked := false;
   AutoCenterAndScale(FPointCloud[2]);
end;

procedure TPointCloudForm.CheckBox3Change(Sender: TObject);
begin
   FPointCloud[3].Visible := CheckBox3.IsChecked;
   CheckBox2.IsChecked := false;
   CheckBox1.IsChecked := false;

   AutoCenterAndScale(FPointCloud[3]);
end;

procedure TPointCloudForm.CTBPointSizeChangeTracking(Sender: TObject);
begin
   FPointCloud[1].PointSize := CTBPointSize.Value / 100;
end;

procedure TPointCloudForm.OnApplicationIdle(Sender: TObject; var Done: Boolean);
begin
   Viewport3D1.Repaint;
   Done := False;
end;

procedure TPointCloudForm.TimerFPSTimer(Sender: TObject);
begin
   var count := FRenderCount;
   var elapsed := FRenderTicks.ElapsedMilliseconds;

   var fps : Double := 0;
   if count > 0 then
      fps := count / elapsed * 1000;

   FRenderTicks.Reset;
   FRenderTicks.Start;
   FRenderCount := 0;

   var paintFPS := 0.0;
   if Viewport3D1.LastPaintSeconds > 0 then
      paintFPS := 1 / Viewport3D1.LastPaintSeconds;
   FRunningFPS := FRunningFPS * 0.5  + fps * 0.5;

   Caption := Format('%.1f ms paint (%.1f FPS) / %.1f actual FPS',
      [ Viewport3D1.LastPaintSeconds*1000, paintFPS, FRunningFPS, 0 {SmartNumberPoints(FPointCloud[1].Points.Length)} ]  );
end;


procedure TPointCloudForm.Viewport3D1Painting(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
begin
   Inc(FRenderCount);
end;


end.
