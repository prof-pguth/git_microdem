unit simple_ogl;

//not yet completed, because it has a form inside it


// based on http://escargot.la.coocan.jp/SimpleSamples/SimpleSampleOpenGL_FMX.pas
//     could not find the form file (so I recreated it), or any documentation
//     removed comments in a font that was not displaying

interface

uses
  FMX.Forms3D, FMX.Types3D,

  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Platform, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Menus

  {$IFDEF MSWINDOWS}
  ,Winapi.Windows, Winapi.OpenGL, FMX.Platform.Win,
  FMX.TabControl
  {$ENDIF}

  {$IFDEF MACOS}
  ,Macapi.ObjectiveC, Macapi.OpenGL, Macapi.AppKit, Macapi.CocoaTypes, Macapi.Foundation, FMX.Platform.Mac, System.Rtti
  {$ENDIF}
  ;

type
  TSimpleOGL = class
  private
    FForm             : TForm3D;
    FHandle           : TWindowHandle;   //TFmxHandle;
    FDoubleBuffered   : Boolean;
    FInitialized      : Boolean;
  {$IFDEF MSWINDOWS}
    FRenderingContext : HGLRC;
    FDeviceContext    : HDC;
    FWND              : HWND;
  {$ENDIF}
  {$IFDEF MACOS}
    FLibraryHandle    : HMODULE;
    FRenderingContext : NSOpenGLContext;
    function GetNSView: NSView;
  {$ENDIF}
  protected

  public
    constructor Create;
    destructor  Destroy; override;

    procedure   Init(Form : TForm3D; DoubleBuffered : Boolean; DepthSize : integer; StencilSize : integer);
    procedure   Close;
    function    Activate : Boolean;
    procedure   Deactivate;
    procedure   Swap;
    procedure   Resized;
    procedure   FitViewport;

  {$IFDEF MSWINDOWS}
    property    DeviceContext : HDC read FDeviceContext;
  {$ENDIF}
  end;

implementation


{ TSimpleOGL }

constructor TSimpleOGL.Create;
begin
  inherited;
  FInitialized := false;
  FDoubleBuffered := false;
end;

destructor TSimpleOGL.Destroy;
begin
  Close;

  inherited;
end;

procedure TSimpleOGL.Init(Form : TForm3D; DoubleBuffered : Boolean; DepthSize : integer; StencilSize : integer);
{$IFDEF MSWINDOWS}
var
  PixelFormat : Integer;
  PixelFormatDescriptor : TPixelFormatDescriptor;
  option : DWORD;
{$ENDIF}

{$IFDEF MACOS}
var
  PixelFormat: NSOpenGLPixelFormat;
  OpenGLContext : NSOpenGLContext;
  option : array of NSOpenGLPixelFormatAttribute;

  procedure addOption( opt : NSOpenGLPixelFormatAttribute );
  var
    len : integer;
  begin
    len := Length(option);
    SetLength(option, len+1);
    option[len] := opt;
  end;
{$ENDIF}

begin
  FForm := Form2;
  FHandle := Form.Handle;
  FDoubleBuffered := DoubleBuffered;

{$IFDEF MSWINDOWS}
  FWND := FmxHandleToHWND(FHandle);
  FDeviceContext := GetDC( FWND );

  option := PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL;

  if DoubleBuffered then option := option or PFD_DOUBLEBUFFER;

  FillChar( PixelFormatDescriptor, sizeof(PixelFormatDescriptor), 0);
  PixelFormatDescriptor.nSize        := sizeof(PixelFormatDescriptor);
  PixelFormatDescriptor.nVersion     := 1;
  PixelFormatDescriptor.dwFlags      := option;
  PixelFormatDescriptor.iPixelType   := PFD_TYPE_RGBA;
  PixelFormatDescriptor.cColorBits   := 24;
  PixelFormatDescriptor.cDepthBits   := DepthSize;
  PixelFormatDescriptor.cStencilBits := StencilSize;
  PixelFormatDescriptor.iLayerType   := PFD_MAIN_PLANE;

  PixelFormat := ChoosePixelFormat(FDeviceContext, @PixelFormatDescriptor);
  SetPixelFormat(FDeviceContext, PixelFormat, @PixelFormatDescriptor);
  DescribePixelFormat(FDeviceContext, PixelFormat, sizeof(PixelFormatDescriptor), PixelFormatDescriptor);

  FRenderingContext := wglCreateContext(FDeviceContext);
{$ENDIF}

{$IFDEF MACOS}
  PixelFormat := TNSOpenGLPixelFormat.Create;
  try
    Initialize(option);
    addOption( NSOpenGLPFAAccelerated );
    if FDoubleBuffered then addOption( NSOpenGLPFADoubleBuffer );
    addOption( NSOpenGLPFADepthSize );
    addOption( DepthSize );
    addOption( NSOpenGLPFAStencilSize );
    addOption( StencilSize );
    addOption( 0 );

    PixelFormat := TNSOpenGLPixelFormat.Wrap(PixelFormat.initWithAttributes(@option[0]));

    FRenderingContext := TNSOpenGLContext.Wrap(TNSOpenGLContext.Create.initWithFormat(PixelFormat, nil));
  finally
    PixelFormat.release;
  end;

  FLibraryHandle := InitOpenGL;
{$ENDIF}

  FInitialized := true;
end;

procedure TSimpleOGL.Close;
begin
  if FInitialized then
  begin
    {$IFDEF MSWINDOWS}
      wglMakeCurrent(FWND, 0);
      wglDeleteContext(FRenderingContext);
    {$ENDIF}

    {$IFDEF MACOS}
      FRenderingContext.release;
    {$ENDIF}

    FInitialized := false;
  end;
end;

{$IFDEF MACOS}
function TSimpleOGL.GetNSView : NSView;
var
  ctx: TRttiContext;
  obj: TObject;
  prop: TRttiProperty;
  view : NSView;
  rect : NSRect;
begin
  result := nil;
  obj := TObject(FmxHandleToObjC(FHandle));
  ctx := TRttiContext.Create;
  try
    // special thanks! lynatan.
    prop := ctx.GetType(obj.ClassType).GetProperty('View');
    view := prop.GetValue(obj).AsInterface as NSView;
    result := view;
  finally
    ctx.Free;
  end;
end;
{$ENDIF}

function TSimpleOGL.Activate : Boolean;
{$IFDEF MACOS}
var
  view : NSView;
  rect : NSRect;
{$ENDIF}
begin
  result := false;
  if FInitialized then
  begin
  {$IFDEF MSWINDOWS}
    wglMakeCurrent(FDeviceContext, FRenderingContext);
    result := true;
  {$ENDIF}

  {$IFDEF MACOS}
    view := GetNSView;
    result := view <> nil;
    if result then
    begin
      FRenderingContext.setView(view);
      FRenderingContext.makeCurrentContext;
    end;
  {$ENDIF}
  end;
end;

procedure TSimpleOGL.Deactivate;
begin
  if FInitialized then
  begin
  {$IFDEF MSWINDOWS}
    wglMakeCurrent(0, 0);
  {$ENDIF}

  {$IFDEF MACOS}

  {$ENDIF}
  end;
end;



procedure TSimpleOGL.Resized;
{$IFDEF MACOS}
var
  view : NSView;
  rect : NSRect;
{$ENDIF}
begin
  if FInitialized then
  begin
{$IFDEF MACOS}
    FRenderingContext.update;
{$ENDIF}
  end;
end;

procedure TSimpleOGL.FitViewport;
begin
  if FInitialized then
  begin
    if Activate then
    begin
      glViewPort(0, 0, FForm.ClientWidth, FForm.ClientHeight);
      Deactivate;
    end;
  end;
end;


procedure TSimpleOGL.Swap;
begin
  if FInitialized then
  begin
    if FDoubleBuffered then
    begin
    {$ifdef MSWINDOWS}
      SwapBuffers(FDeviceContext);
    {$endif}

    {$IFDEF MACOS}
      glSwapAPPLE();
    {$ENDIF}
    end;
  end;
end;

procedure TForm2.Form3DClose(Sender: TObject; var Action: TCloseAction);
begin
  FOGL.Free;
end;

procedure TForm2.Form3DCreate(Sender: TObject);
begin
  FOGL := TSimpleOGL.Create;
  FOGL.Init(self, true, 24, 0);
  FAngle := 0;
end;


var
  DEMTriangleList : integer;

procedure LoadGISdrape;
type
   tPointXYZColor = record
      x,y,z,r,g,b,Size : single;
   end;
   tPointXYZColorArray = array[1..1000] of tPointXYZColor;
var
   PointXYZColor : tPointXYZColorArray;
   tfile : File;
   Total,Pts,i : integer;
begin
  //glNewList(DEMTriangleList,GL_COMPILE);
  inc(DEMTriangleList);
  Application.ProcessMessages;
  glPointSize(1);
  glBegin(GL_POINTS);
     assignFile(tfile,'c:\temp\test.xyz');
     i := sizeOf(tPointXYZColor);
     reset(tFile,i);
     Total := 0;
     while not eof(Tfile) do begin
        BlockRead(tfile,PointXYZColor[1],1000,Pts);
        for i := 1 to Pts do begin
           inc(total);
           glColor3f(PointXYZColor[i].r,PointXYZColor[i].g,PointXYZColor[i].b );
           glVertex3f(0.075*PointXYZColor[i].x,0.01*PointXYZColor[i].z,0.075*PointXYZColor[i].y);
        end;
     end;
     CloseFile(tFile);
   glEnd;
   //glEndList;
end;


procedure TForm2.Form3DRender(Sender: TObject; Context: TContext3D);
const
  vcolors : array[0..11] of Single = (
    0.0, 1.0, 0.0, 1.0,
    1.0, 0.0, 0.0, 1.0,
    0.0, 0.0, 1.0, 1.0
  );
var
  x,y : single;
  i: Integer;
begin
  if FOGL.Activate then
  begin
    glClearColor(0 , 0.5, 0.6, 0.8);
    glClear(GL_COLOR_BUFFER_BIT);


  glLoadIdentity();
  //glTranslatef(0, -0.5, -0.5);
  glRotatef(5*DemTriangleList, 0.20, 1, 0);


    LoadGISDrape;
    Caption := IntToStr(DEMtrianglelist);

    (*

    glBegin(GL_TRIANGLES);
    for i := 0 to 2 do
    begin
      x := cos((FAngle+120*i)*PI/180);
      y := sin((FAngle+120*i)*PI/180);
      glVertex2f(x , y);
      glColor4f(vcolors[i*4+0], vcolors[i*4+1], vcolors[i*4+2], vcolors[i*4+3]);
    end;
    glEnd();
    glFlush();
    *)

    FAngle := FAngle + 1;

    //---------------------------------------------------------------------------

    FOGL.Swap;
    FOGL.Deactivate;
  end;
end;



procedure TForm2.Form3DResize(Sender: TObject);
begin

  FOGL.Resized;
  FOGL.FitViewport;

end;

procedure TForm2.Timer1Timer(Sender: TObject);
begin
   Form3DRender(Nil,Nil);
end;

initialization
{$IfDef MSWINDOWS}
  GlobalUseDirect2D := false;
{$endif}
DEMTriangleList := 0;

end.


