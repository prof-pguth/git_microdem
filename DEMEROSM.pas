unit Demerosm;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch }
{   file verified 10/4/2015       }
{_________________________________}


{$I nevadia_defines.inc}


{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   {$Define RecordSatLoad}
   //{$Define TimeSatLoad}
   //{$Define BasicOpens}
   //{$Define RecordSatForm}
   //{$Define RecordBandModProblems}
   //{$Define RecordSatCoords}
   //{$Define RecordClosingProblems}
   //{$Define RecordKMLProblems}
{$EndIf}


interface

uses
  SysUtils, Windows, Classes, Graphics,  ClipBrd, System.UItypes, ShlObj,Forms,
  PETMAR,Petmar_types,DEMDefs,
  Vcl.Buttons, Vcl.Controls, Vcl.StdCtrls, Vcl.ToolWin, Vcl.ComCtrls, Vcl.ExtCtrls,
  Vcl.Menus, Vcl.Dialogs;

type
  TSatelliteForm = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    NewImage1: TMenuItem;
    Exit1: TMenuItem;
    Modify1: TMenuItem;
    Blowup1: TMenuItem;
    Zoomout1: TMenuItem;
    Saveimage1: TMenuItem;
    ScrollBox1: TScrollBox;
    Image1: TImage;
    Fullsize1: TMenuItem;
    Forceredraw1: TMenuItem;
    Register1: TMenuItem;
    Showregistrationpoints1: TMenuItem;
    ColorDialog1: TColorDialog;
    Graphicalsubset1: TMenuItem;
    FullImage1: TMenuItem;
    N1: TMenuItem;
    ToolBar1: TToolBar;
    SpeedButton2: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton10: TSpeedButton;
    SpeedButton13: TSpeedButton;
    SpeedButton21: TSpeedButton;
    SpeedButton22: TSpeedButton;
    FullImageSpeedButton: TSpeedButton;
    PopupMenu1: TPopupMenu;
    ComboBox1: TComboBox;
    Recentermap1: TMenuItem;
    Copytoclipboard1: TMenuItem;
    Saveimage2: TMenuItem;
    Copyimagecoordinatestoclipboard1: TMenuItem;
    Copylatlongcoordstoclipboard1: TMenuItem;
    procedure DisplayScene;
    procedure NewImage1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
    procedure Saveimage1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Blowup1Click(Sender: TObject);
    procedure Zoomout1Click(Sender: TObject);
    procedure Fullsize1Click(Sender: TObject);
    //procedure Image1DblClick(Sender: TObject);
    procedure Subset1Click(Sender: TObject);
    procedure Image1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure Forceredraw1Click(Sender: TObject);
    procedure Showregistrationpoints1Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Graphicalsubset1Click(Sender: TObject);
    procedure FullImage1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure SpeedButton5Click(Sender: TObject);
    procedure SpeedButton10Click(Sender: TObject);
    procedure SpeedButton13Click(Sender: TObject);
    procedure SpeedButton21Click(Sender: TObject);
    procedure SpeedButton22Click(Sender: TObject);
    procedure FullImageSpeedButtonClick(Sender: TObject);
    procedure Display2Click(Sender: TObject);
    procedure SpeedButton9Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure Recentermap1Click(Sender: TObject);
    procedure Copytoclipboard1Click(Sender: TObject);
    procedure Saveimage2Click(Sender: TObject);
    procedure Copyimagecoordinatestoclipboard1Click(Sender: TObject);
    procedure Copylatlongcoordstoclipboard1Click(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  private
    { Private declarations }
     sx,sy    : integer;     //for mouse panning
     ImageXSize,ImageYSize,
     BlowupInWindow : integer;
     procedure SetMenuChoices;
  public
    { Public declarations }
     SatInWindow : integer;
     SatView : tSatView;
     function XSatFromScreen(x1 : word) : word;
     function YSatFromScreen(y1 : word) : word;
     function XSatToScreen(x1 : integer) : integer;
     function YSatToScreen(y1 : integer) : integer;
     function ScreenGeoLocation(x,y : integer) : ShortString;
     function OnScreen(x,y : integer) : boolean;

     //procedure KMLReprojectImage(DoKML,DoGeotiff : boolean);
     //procedure UTMToSatelliteScreen(X,Y : float64;  var XPic,YPic : integer);
     //procedure LatLongToSatelliteScreen(Lat,Long: float64;  var XPic,YPic : integer);
     //procedure SatelliteScreenToUTM(x,y : word; var XUTM,YUTM : float64);
     procedure SatGridToScreen(xu,yu : float64; var XPic,YPic : integer);
     procedure MaximizeBlowUp;
     procedure StartImageRegistration;
  end;

type
   tEROSDoingWhat = (ErosJustWandering,SubsetCoverage,PickTINTriangle,PickTINEdgePoint);
var
   EROSDoingWhat   : tEROSDoingWhat;
var
  DefaultSceneBaseName : ShortString;
  {$IfDef NoSatelliteForms}
  {$Else}
     SatelliteForm : TSatelliteForm;
  {$EndIf}

function CreateSatelliteImage(NewSatImage : integer) : TSatelliteForm;


implementation

{$R *.DFM}

uses
   {$IfDef ExDataManip}
   {$Else}
      DEMHandW,
   {$EndIf}

   {$IfDef ExGeostats}
   {$Else}
      DEMStat,
   {$EndIf}

   {$IfDef ExImageClassify}
   {$Else}
      sup_class,
   {$EndIf}

   PETMath,
   PETImage,
   BaseGraf,
   BaseMap,
   DEMEROS,
   Make_Tables,
   PETForm,
   PetDBUtils,
   Map_overlays,
   DEM_Manager,
   DEMCoord,DEMRefOp,DEMMapf,
   DEMGrPik,
   KML_Creator,
   sat_kml_out,
   demsatcontrast,Demdef_routines,
   MrSidImagery,
   compress_form,

{$IfDef ExSatStats}
{$Else}
   demstringgrid,
{$EndIf}

{Main program MDI window for different programs that use this module}
   Nevadia_Main;
{End of the MDI parent declaration}


var
   lastx,lasty : integer;
   NewX1,NewY1,Newx2,NewY2 : integer;
   MouseIsDown : boolean;


function CreateSatelliteImage(NewSatImage : integer) : TSatelliteForm;
begin
   {$IfDef RecordSatLoad} WriteLineToDebugFile('CreateSatelliteImage NewSatImage=' + IntToStr(NewSatImage)); {$EndIf}
   Result := TSatelliteForm.Create(Application);
   Result.SatInWindow := NewSatImage;
   Result.SatView.ColsDisplayed := SatImage[Result.SatInWindow].NumSatCol;
   Result.SatView.RowsDisplayed := SatImage[Result.SatInWindow].NumSatRow;
   Result.MaximizeBlowUp;
   Result.DisplayScene;
   {$IfDef RecordSatLoad} WriteLineToDebugFile('CreateSatelliteImage out'); {$EndIf}
end;



procedure TSatelliteForm.StartImageRegistration;
begin
   {$IfDef ExRegisterImage}
   {$Else}
      //ImageRegistrationStartup(Self,SatInWindow);
   {$EndIf}
end;


function TSatelliteForm.ScreenGeoLocation(x,y : integer) : ShortString;
var
   Lat,Long : float64;
   TStr     : ShortString;
begin
   if (SatImage[SatInWindow] <> Nil) then with SatImage[SatInWindow]{,RegVars} do begin
      TStr := 'Image x=' + IntToStr(XSatFromScreen(x)) + '    y='+IntToStr(YSatFromScreen(y));
      if RegVars.Registration in [RegProjection] then begin
         SatGridToLatLongDegree(1,XSatFromScreen(x),YSatFromScreen(y),Lat,Long);
         Result := ImageMapProjection.PreferLocationString(Lat,Long);
         if MDdef.SatImageCoords then Result := Result + '    ' + TStr;
         Result := Result + '  ' +  ImageMapProjection.h_DatumCode;;
      end
      else if (RegVars.Registration in [RegNone]) then begin
         Result := TStr;
      end;
   end;
end;


procedure TSatelliteForm.SatGridToScreen(xu,yu : float64; var XPic,YPic : integer);
begin
   with SatImage[SatInWindow] do begin
      if (BlowUpInWindow > 0) then begin
         XPic := (round(XU) - SatView.XOffsetInWindow) * BlowUpInWindow;
         YPic := (round(YU) - SatView.YOffsetInWindow) * BlowUpInWindow;
      end
      else begin
         XPic := round((XU - SatView.XOffsetInWindow) / -BlowUpInWindow);
         YPic := round((YU - SatView.YOffsetInWindow) / -BlowUpInWindow);
      end;
   end {with};
end;

//{$F+}

(*
procedure TSatelliteForm.UTMToSatelliteScreen(X,Y : float64;  var XPic,YPic : integer);
var
   xu,yu : float64;
begin
   SatImage[SatInWindow].UTMtoSatGrid(1,X,Y,Xu,Yu);
   SatGridToScreen(xu,yu,xpic,ypic);
end;


procedure TSatelliteForm.LatLongToSatelliteScreen(Lat,Long: float64;  var XPic,YPic : integer);
var
   xu,yu : float64;
begin
   SatImage[SatInWindow].LatLongDegreeToSatGrid(1,Lat,Long,xu,yu);
   SatGridToScreen(xu,yu,xpic,ypic);
end;

procedure TSatelliteForm.SatelliteScreenToUTM(x,y : word; var XUTM,YUTM : float64);
begin
   SatImage[SatInWindow].SatGridToUTM(1,1.0*XSatFromScreen(x),1.0*YSatFromScreen(y),XUTM,YUTM);
end;

*)


function TSatelliteForm.XSatFromScreen(x1 : word) : word;
begin
   if BlowUpInWindow > 0 then x1 := SatView.XOffsetinWindow + x1 div BlowUpInWindow
   else X1 := SatView.XOffsetInWindow + x1 * -BlowUpInWindow;
   XSatFromScreen := x1;
end;

function TSatelliteForm.YSatFromScreen(y1 : word) : word;
begin
   if BlowUpInWindow > 0 then Y1 := SatView.YOffsetInWindow + y1 div BlowUpInWindow
   else Y1 := SatView.YOffsetInWindow + y1 * -BlowUpInWindow;
   YSatFromScreen := y1;
end;

function TSatelliteForm.XSatToScreen(x1 : integer) : integer;
begin
   if BlowUpInWindow > 0 then XSatToScreen := (x1 - SatView.XOffsetInWindow) * BlowUpInWindow
   else XSatToScreen := (x1 - SatView.XOffsetInWindow) div - BlowUpInWindow;
end;

function TSatelliteForm.YSatToScreen(y1 : integer) : integer;
begin
   if (BlowUpInWindow > 0) then YSatToScreen := (y1 - SatView.YOffsetInWindow) * BlowUpInWindow
   else YSatToScreen := (y1 - SatView.YOffsetInWindow) div - BlowUpInWindow;
end;


procedure TSatelliteForm.SetMenuChoices;
begin
   if (SatInWindow > 0) and (SatImage[SatInWindow] <> Nil) then with SatImage[SatInWindow] do begin
      if (BlowUpInWindow = 1) then ComboBox1.Text := '100%'
      else if (BlowUpInWindow > 1) then ComboBox1.Text := IntToStr(BlowUpInWindow *100) + '%'
      else ComboBox1.Text := IntToStr(round(100 div -BlowUpInWindow)) + '%';
      Register1.Enabled := RegVars.Registration in [RegNone];
      NewImage1.Visible := (NumDEMDataSetsOpen = 0);
   end;
end;


procedure TSatelliteForm.MaximizeBlowUp;
begin
   ImageYSize := Forms.Screen.Height - 95;
   ImageXSize := Forms.Screen.Width - 45;
   with SatImage[SatInWindow] do begin
      BlowUpInWindow := 1;
      if (SatView.ColsDisplayed > ImageXSize) or (SatView.RowsDisplayed > ImageYSize) then begin
         BlowUpInWindow := -2;
         while (SatView.ColsDisplayed div -BlowUpInWindow > ImageXSize) or (SatView.RowsDisplayed div -BlowUpInWindow > ImageYSize) do dec(BlowUpInWindow);
         ImageXSize := SatView.ColsDisplayed div -BlowUpInWindow;
         ImageYSize := SatView.RowsDisplayed div -BlowUpInWindow;
      end
      else begin;
         while (SatView.ColsDisplayed * succ(BlowUpInWindow) <= ImageXSize) and (SatView.RowsDisplayed * succ(BlowUpInWindow) <= ImageYSize) do inc(BlowUpInWindow);
         ImageXSize := SatView.ColsDisplayed * BlowUpInWindow;
         ImageYSize := SatView.RowsDisplayed * BlowUpInWindow;
      end;
   end;
   FormResize(Nil);
   SetMenuChoices;
end;


procedure TSatelliteForm.NewImage1Click(Sender: TObject);
begin
   OpenAndDisplayNewScene(Nil,'',true,true,true);
end;


procedure TSatelliteForm.DisplayScene;
var
   Bitmap      : tMyBitmap;
   MemNeed,
   Mult,Divide : integer;
   OK          : boolean;
begin
   with SatImage[SatInWindow] do begin
      repeat
         OK := true;
         Mult := 1;
         Divide := 1;
         if (BlowUpInWindow >= 1) then Mult := BlowUpInWindow
         else Divide := -BlowUpInWindow;
         MemNeed := round(4.0 * (SatView.ColsDisplayed / Divide  * Mult) * (SatView.RowsDisplayed / Divide * Mult));
         if AskUserAboutMemory(MemNeed) then begin
            if OK then OK := AnswerIsYes(ComboBox1.Text + ' Image requires' + SmartMemorySizeBytes(MemNeed) + MessLineBreak +  '  will display slowly and not fit on screen.' + MessLineBreak +
                'Subset before using this zoom.'+ MessLineBreak + MessLineBreak + 'Are you sure you want to continue')
         end
         else OK := true;
         if not OK then exit;
      until OK;
      ImageXSize := succ(SatView.ColsDisplayed div Divide * Mult);
      ImageYSize := succ(SatView.RowsDisplayed div Divide * Mult);
      CreateBitmap(Bitmap,ImageXSize,ImageYSize);
      {$IfDef RecordProblems} WriteLineToDebugFile('TSatelliteForm.DisplayScene, with size: ' + IntToStr(ImageXSize) + ' by ' + IntToStr(ImageYSize)); {$EndIf}
      FormResize(Nil);

      Caption := SceneBaseName;
      DisplayImage(SatView,Nil,Bitmap);
      Image1.Picture.Graphic := Bitmap;
      Bitmap.Free;
      SetMenuChoices;
      FormResize(Nil);
      SpeedButton22.Enabled := BlowUpInWindow <> 1;
   end;
end;


procedure TSatelliteForm.Exit1Click(Sender: TObject);
begin
   CloseAllImagery;
   ApplicationProcessMessages;
   Close;
end;


procedure TSatelliteForm.Saveimage1Click(Sender: TObject);
begin
   SaveImageAsBMP(Image1);
end;

procedure TSatelliteForm.Saveimage2Click(Sender: TObject);
begin
   Saveimage1Click(Sender);
end;

procedure TSatelliteForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   Action := caFree;
   Self := Nil;
end;


procedure TSatelliteForm.Blowup1Click(Sender: TObject);
begin
   if (BlowUpInWindow < 1) then BlowUpInWindow := 2
   else inc(BlowUpInWindow);
   repeat
      ReadDefault('Blow up factor',BlowUpInWindow);
   until (BlowUpInWindow in [1..10]);
   DisplayScene;
end;

procedure TSatelliteForm.Zoomout1Click(Sender: TObject);
begin
   BlowUpInWindow := -BlowUpInWindow;
   if (BlowUpInWindow < 1) then BlowUpInWindow := 2 else inc(BlowUpInWindow);
   repeat
      ReadDefault('Zoom out factor',BlowUpInWindow);
   until (BlowUpInWindow in [1..20]);
   BlowUpInWindow := -BlowUpInWindow;
   DisplayScene;
end;

procedure TSatelliteForm.Fullsize1Click(Sender: TObject);
begin
   BlowUpInWindow := 1;
   DisplayScene;
end;

procedure TSatelliteForm.Image1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
   Lat,Long : float64;
begin
   if SatInWindow = 0 then exit;
   WmDEM.StatusBar1.Panels[1].Text := ScreenGeoLocation(x,y);

   {$IfDef ExTIN}
   {$Else}
   if (SatImage[SatInWindow].SatTINRegistration <> Nil) then begin
      if SatImage[SatInWindow].SatTINRegistration.InterpolateXY(XSatFromScreen(x),YSatFromScreen(y),Long,Lat) then
         WmDEM.StatusBar1.Panels[2].Text := LatLongDegreeToString(Lat,Long,MDDef.OutPutLatLongMethod)
      else WmDEM.StatusBar1.Panels[2].Text := '';
   end;
   {$EndIf}

   if MouseIsDown and (EROSDoingWhat = EROSJustWandering) then begin
      ScrollBox1.HorzScrollBar.Position := ScrollBox1.HorzScrollBar.Position + SX - X;
      ScrollBox1.VertScrollBar.Position := ScrollBox1.VertScrollBar.Position + SY - Y;
      exit;
   end;

   if (EROSDoingWhat in [SubsetCoverage]) and MouseIsDown then begin
      Newx2 := X;
      NewY2 := Y;
      Image1.Canvas.Pen.Mode := pmNotXor;
      Image1.Canvas.Pen.Color := clRed;
      Image1.Canvas.Pen.Width := 4;
      Image1.Canvas.Rectangle(newx1,newy1,LastX,LastY);
      Image1.Canvas.Rectangle(newx1,newy1,newx2,newy2);
   end;

   Lastx := x;
   lasty := y;
end;


procedure TSatelliteForm.Subset1Click(Sender: TObject);
begin
   EROSDoingWhat := SubsetCoverage;
   {$IfDef MicroDEM} wmDEM.SetPanelText(0,'Pick NW corner'); {$EndIf}
   MouseIsDown := false;
end;


procedure TSatelliteForm.Image1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   MouseIsDown := false;
   ShowHourglassCursor;
   if EROSDoingWhat in [SubsetCoverage] then begin
      if (Newx1 >= Newx2) or (Newy1 >= newY2) then begin
         DragDirections;
         Subset1Click(Sender);
         exit;
      end;
      wmDEM.SetPanelText(0,'');
      ShowHourglassCursor;
      with SatImage[SatInWindow] do begin
         SatView.ColsDisplayed := succ(XSatFromScreen(Newx2) - XSatFromScreen(Newx1));
         SatView.RowsDisplayed := succ(YSatFromScreen(NewY2) - YSatFromScreen(NewY1));
         SatView.XOffsetInWindow := XSatFromScreen(Newx1);
         SatView.YOffsetInWindow := YSatFromScreen(NewY1);
         BlowUpInWindow := 1;
         MaximizeBlowUp;
         DisplayScene;
         FullImageSpeedButton.Enabled := true;
      end;
      EROSDoingWhat := EROSJustWandering;
    end;
end;

procedure TSatelliteForm.Image1MouseDown(Sender: TObject;  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   MouseIsDown := true;
   if (EROSDoingWhat = EROSJustWandering) then begin
      SX := X;  // X start co-ordinate, image panning
      SY := Y;  // Y start co-ordinate, image panning
      Forms.Screen.Cursor := crHandPoint;
   end;

   if EROSDoingWhat in [SubsetCoverage] then begin
      {$IfDef MicroDEM} WmDEM.StatusBar1.Panels[0].Text := 'Drag to SE corner'; {$EndIf}
      NewX1 := x;
      NewY1 := y;
      Newx2 := x;
      NewY2 := y;
   end;

   if (Button = mbRight) then begin
      {$IfDef RecordProblems} WriteLineToDebugFile('Right mouse click on image analysis window'); {$EndIf}
      RecenterMap1.Visible := FullImageSpeedButton.Visible;

      {$IfDef ExTIN}
         Copylatlongcoordstoclipboard1.Visible := false;
      {$Else}
         Copylatlongcoordstoclipboard1.Visible := (SatImage[SatInWindow].SatTINRegistration <> Nil);
      {$EndIf}

      PopupMenu1.PopUp(Mouse.CursorPos.X,Mouse.CursorPos.Y);
   end;
end;


procedure TSatelliteForm.FormCreate(Sender: TObject);
begin
   {$IfDef RecordSatForm} WriteLineToDebugFile('TSatelliteForm.FormCreate in'); {$EndIf}
   ScrollBox1.DoubleBuffered := True;  //for image panning
   Image1.Stretch := true;             //Required for Delphi 6 "feature"
   Toolbar1.Visible := MDDef.ShowMapToolbar;
   InitializeSatView(SatView);
   FullImageSpeedButton.Enabled := false;
   SatInWindow := 0;
   BlowUpInWindow := 1;
   ImageXSize := GetDeviceCaps(GetDC(0),HorzRes) - 100;
   ImageYSize := GetDeviceCaps(GetDC(0),VertRes) - 100;
   ClientHeight := ImageYSize;
   ClientWidth := ImageXSize;
   CheckFormPlacement(Self);
   {$IfDef RecordSatForm} WriteLineToDebugFile('TSatelliteForm.FormCreate out'); {$EndIf}
end;


procedure TSatelliteForm.FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if Screen.ActiveControl = ComboBox1 then Handled := True;
end;

procedure TSatelliteForm.Copyimagecoordinatestoclipboard1Click(Sender: TObject);
begin
   ClipBoard_Image_Coords := true;
   Clipboard_ImageX := round(XSatFromScreen(Lastx));
   Clipboard_ImageY := round(YSatFromScreen(Lasty));
end;

procedure TSatelliteForm.Copylatlongcoordstoclipboard1Click(Sender: TObject);
begin
   {$IfDef ExTIN}
   {$Else}
      ClipBoard_Coords := SatImage[SatInWindow].SatTINRegistration.InterpolateXY(XSatFromScreen(Lastx),YSatFromScreen(Lasty),ClipBoard_Long,ClipBoard_Lat);
      ClipBrd.ClipBoard.AsText := RealToString(Clipboard_Lat,-14,-8) + ',' +  RealToString(Clipboard_Long,-14,-8);
   {$EndIf}
end;

procedure TSatelliteForm.Copytoclipboard1Click(Sender: TObject);
begin
   SpeedButton21Click(Sender);
end;

procedure TSatelliteForm.Forceredraw1Click(Sender: TObject);
begin
   DisplayScene;
end;


procedure TSatelliteForm.Showregistrationpoints1Click(Sender: TObject);
begin
   StartImageRegistration;
end;


procedure TSatelliteForm.FormActivate(Sender: TObject);
begin
   if (SatInWindow <> 0) and (SatImage[SatInWindow] = nil) then begin
      Close;
      exit;
   end;
   if (SatInWindow > 0) then begin
      SetMenuChoices;
      {$IfDef MicroDEM} WmDEM.StatusBar1.Panels[1].Text := ''; {$EndIf}
   end;
end;


procedure TSatelliteForm.Recentermap1Click(Sender: TObject);
begin
   SatView.XOffsetInWindow := XSatFromScreen(LastX) - SatView.ColsDisplayed div 2;
   SatView.YOffsetInWindow := YSatFromScreen(LastY) - SatView.RowsDisplayed div 2;
   BlowUpInWindow := 1;
   MaximizeBlowUp;
   DisplayScene;
end;


procedure TSatelliteForm.FormResize(Sender: TObject);

     function WantedWidth : integer;
     begin
        WantedWidth := ImageXSize + GetSystemMetrics(SM_CXVSCROLL);
     end;

     function WantedHeight : integer;
     begin
        WantedHeight := ImageYSize + GetSystemMetrics(SM_CXVSCROLL);
     end;

begin
   {$IfDef RecordFormResizeProblems} RecordFormSize(Self.Caption + ' FormResize 0:'); {$EndIf}
   if (ClientWidth > WantedWidth) then ClientWidth := WantedWidth;
   if (ClientHeight > WantedHeight) then ClientHeight := WantedHeight;
   if (ScrollBox1.Width > WantedWidth) then ScrollBox1.ClientWidth := WantedWidth;
   if (ScrollBox1.Height > WantedHeight) then ScrollBox1.ClientHeight := WantedHeight;
end;

procedure TSatelliteForm.Graphicalsubset1Click(Sender: TObject);
begin
   EROSDoingWhat := SubsetCoverage;
   wmDEM.SetPanelText(0,'Pick NW corner');
   MouseIsDown := false;
end;

procedure TSatelliteForm.FullImage1Click(Sender: TObject);
begin
    SatView.ColsDisplayed := SatImage[SatInWindow].NumSatCol;
    SatView.RowsDisplayed := SatImage[SatInWindow].NumSatRow;
    SatView.XOffsetInWindow := 0;
    SatView.YOffsetInWindow := 0;
    MaximizeBlowUp;
    DisplayScene;
    FullImageSpeedButton.Enabled := false;
end;


procedure TSatelliteForm.SpeedButton2Click(Sender: TObject);
begin
   Saveimage1Click(Sender);
end;


procedure TSatelliteForm.SpeedButton4Click(Sender: TObject);
begin
   Blowup1Click(Sender);
end;

procedure TSatelliteForm.SpeedButton5Click(Sender: TObject);
begin
   Zoomout1Click(Sender);
end;

procedure TSatelliteForm.SpeedButton10Click(Sender: TObject);
begin
   Graphicalsubset1Click(Sender);
end;

procedure TSatelliteForm.SpeedButton13Click(Sender: TObject);
begin
   Forceredraw1Click(Sender);
end;

procedure TSatelliteForm.SpeedButton21Click(Sender: TObject);
begin
   AssignImageToClipBoard(Image1);
end;

procedure TSatelliteForm.SpeedButton22Click(Sender: TObject);
begin
   Fullsize1Click(Sender);
end;

procedure TSatelliteForm.FullImageSpeedButtonClick(Sender: TObject);
begin
   FullImage1Click(Sender);
end;

procedure TSatelliteForm.Display2Click(Sender: TObject);
begin
   DisplayScene;
end;

procedure TSatelliteForm.SpeedButton9Click(Sender: TObject);
begin
   Display2Click(Sender);
end;


procedure TSatelliteForm.ComboBox1Change(Sender: TObject);
var
   BUp,Err : integer;
   TStr  : ShortString;
begin
   TStr := ComboBox1.Text;
   if TStr[length(Tstr)] = '%' then Delete(TStr,Length(TStr),1);
   Val(Tstr,BUp,Err);
   if (BUp > 500) then begin
      MessageToContinue('Blowup beyond 500% not allowed');
      BUp := 500;
   end;
   if (BUp = 100) then BlowUpInWindow := 1
   else if (BUp > 100) then BlowUpInWindow := Bup div 100
   else BlowUpInWindow := -round(100 / Bup);
   DisplayScene;
end;


function TSatelliteForm.OnScreen(x, y: integer): boolean;
begin
   Result := (x >= 0) and (x < ImageXSize) and (y >= 0) and (y < ImageYSize);
end;


initialization
  {$IfDef MessageStartUpUnitProblems} MessageToContinue('Startup demerosM'); {$EndIf}
   EROSDoingWhat := EROSJustWandering;
   MouseIsDown := false;
   DefaultSceneBaseName := '';
finalization
   {$IfDef RecordClosingProblems} WriteLineToDebugFile('RecordClosingProblems active in demerosm'); {$EndIf}
   {$IfDef RecordSatLoad} WriteLineToDebugFile('RecordSatLoad active in demerosm'); {$EndIf}
   {$IfDef RecordBandModProblems} WriteLineToDebugFile('RecordBandModProblems active in demerosm  (major slowdown)'); {$EndIf}
   {$IfDef RecordSatCoords} WriteLineToDebugFile('RecordSatCoords active in demerosm'); {$EndIf}
   {$IfDef RecordKMLProblems} WriteLineToDebugFile('RecordKMLProblems active in demerosm'); {$EndIf}
end.

