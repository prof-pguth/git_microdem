unit dempersw;


{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2026 Peter L. Guth  }
{________________________________________}




{$I nevadia_defines.inc}


{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   //{$Define RecordFindLatLong}
   //{$Define RecordClosing}
   //{$Define RecordSunPath}
   //{$Define RecordSetUpPanoramaView}
   //{$Define RecordPerspLabel}
   //{$Define RecordPerspLabelWrite}
   //{$Define RecordPerspLabelShift}
   //{$Define RecordFindLatLongError}
   //{$Define RecordDrape}
   //{$Define RecordFullDrape}
   //{$Define RecordHorizon}
   //{$Define RecordLocatePointOnPersp}
   //{$Define ShowDrapeExtent}
   //{$Define RecordTrackObserverElevation}
   //{$Define RecordPerspective}
   //{$Define RecordSatTimeSeries}
   //{$Define RecordPerspectiveSize}
   //{$Define RecordPerspectiveTime}
   //{$Define RecordFlySequence}
   //{$Define RecordShortFlySequence}
   //{$Define RecordElevAndCompare}         //major slowdow
   //{$Define RecordPerspectiveHorizon}     //detailed options
   //{$Define RecordMapDraw}
   //{$Define RecordInnerLoopPerspective}
   //{$Define PerspectiveQuickDebug}
   //{$Define RecordPerspectivePancake}
   //{$Define RecordDefineDatum}
   //{$Define RecordAzimuthLimits}
   //{$Define RecordView3dCreate}
{$EndIf}


interface

uses
//needed for inline of the core DB functions
   Petmar_db,
   Data.DB,
   {$IfDef UseFireDacSQLlite}
      FireDAC.Comp.Client, FireDAC.Comp.Dataset,FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteWrapper,
   {$EndIf}

   {$IfDef UseTDBF}
      dbf,
   {$EndIf}

   {$IfDef UseTCLientDataSet}
      DBClient,
   {$EndIf}
//end inline core DB functions


  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  System.Threading,System.SyncObjs,System.Math, System.UITypes,
  Vcl.ComCtrls,Vcl.ToolWin, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Buttons,
  Forms, Dialogs,  Menus,
  Petmar_types,PETMAR,PETImage,PETMath,DEMMapF,DEMdefs, BaseGraf,DEM_3d_view;

type
  TThreeDview = class(TForm)
    ScrollBox1: TScrollBox;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Saveimage1: TMenuItem;
    Close1: TMenuItem;
    Modify1: TMenuItem;
    ToolBar1: TToolBar;
    SpeedButton2: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    Showthedrapemap1: TMenuItem;
    Imagescaling1: TMenuItem;
    SpeedButton8: TSpeedButton;
    SpeedButton9: TSpeedButton;
    SpeedButton10: TSpeedButton;
    SpeedButton11: TSpeedButton;
    InfoSpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    Panel1: TPanel;
    TrackBar1: TTrackBar;
    Panel2: TPanel;
    Label1: TLabel;
    SpeedButton12: TSpeedButton;
    BitBtn1: TBitBtn;
    Alphablendbitmap1: TMenuItem;
    SpeedButton13: TSpeedButton;
    CancelBtn: TSpeedButton;
    SpeedButton14: TSpeedButton;
    SpeedButton15: TSpeedButton;
    SpeedButton16: TSpeedButton;
    SpeedButton17: TSpeedButton;
    SpeedButton18: TSpeedButton;
    SpeedButton19: TSpeedButton;
    SpeedButton20: TSpeedButton;
    SpeedButton21: TSpeedButton;
    SpeedButton22: TSpeedButton;
    Image1: TImage;
    BitBtn2: TBitBtn;
    Sunposition1: TMenuItem;
    oday1: TMenuItem;
    Pickday1: TMenuItem;
    Moonposition1: TMenuItem;
    Moonposition2: TMenuItem;
    Pickday2: TMenuItem;
    PopupMenu1: TPopupMenu;
    procedure SpeedButton13Click(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure Alphablendbitmap1Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure SpeedButton7Click(Sender: TObject);
    procedure Saveimage1Click(Sender: TObject);
    procedure Close1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure Modify1Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Image1DblClick(Sender: TObject);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure SpeedButton5Click(Sender: TObject);
    procedure SpeedButton6Click(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure Showthedrapemap1Click(Sender: TObject);
    procedure Imagescaling1Click(Sender: TObject);
    procedure SpeedButton8Click(Sender: TObject);
    procedure SpeedButton9Click(Sender: TObject);
    procedure SpeedButton10Click(Sender: TObject);
    procedure SpeedButton11Click(Sender: TObject);
    procedure InfoSpeedButton6Click(Sender: TObject);
    procedure SpeedButton14Click(Sender: TObject);
    procedure SpeedButton19Click(Sender: TObject);
    procedure SpeedButton20Click(Sender: TObject);
    procedure SpeedButton21Click(Sender: TObject);
    procedure SpeedButton22Click(Sender: TObject);
    procedure Image1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure BitBtn2Click(Sender: TObject);
    procedure oday1Click(Sender: TObject);
    procedure Pickday1Click(Sender: TObject);
    procedure Moonposition2Click(Sender: TObject);
    procedure Pickday2Click(Sender: TObject);
  private
    { Private declarations }
    procedure ModifyWithoutRedraw(var Redraw : boolean);
    procedure RadialStatistics;

    {$IfDef RecordTrackObserverElevation}
       procedure TrackObserverElevation(Where : string35);
    {$EndIf}
  public
    { Public declarations }
     PersViewBaseBMP,OverlayOpaqueBMP : tMyBitmap;
     UseDefaultResize,LiveFlying,
     WaterCheck,ImageDrawn,
     ScrollingView,
     NoViewResizing,
     CanModifyParameters,
     Closable,
     MouseIsDown,
     FlightPathGraphsPossible,
     DrawPointOnRedraw : boolean;
     View3D : tView3D;
     SaveTop,SaveLeft,SaveWidth,SaveHeight,
     LastX,LastY,
     SX,SY : integer;  // Y start co-ordinate, image panning
     OwningMap,PositionMap: TMapForm;

     BaseCaption : shortstring;
     AlongTrackGlide, CrossTrackProfile : BaseGraf.TThisBaseGraph;
     dporPitch,dporAzimuth : float64;
     dporSym : tFullSymbolDeclaration;

     procedure SymbolAtPitchAzimith(Pitch,Azimuth : float64; Sym : tFullSymbolDeclaration; Remeber : boolean = false);
     {$IfDef ExGeography}
     {$Else}
        procedure ShowSunPath(Day : integer);
        procedure ShowMoonPath(Today : boolean);
     {$EndIf}
     function FindLatLong( var Lat,Long : float64; var Pitch : float32; var DistOut : float64; var Azimuth : float32;
        {function FindLatLong(var Lat,Long,Pitch,DistOut,Azimuth : float64;} var Mess1,Mess2,Mess3,Mess4 : shortString; DoPLSS : boolean = false) : boolean;
     procedure SetPerspectiveWindowSize;
     procedure PerspectiveView(SkipDraw : boolean = false);
     function DrawPerspective(SkipDraw : boolean)  : boolean;
     procedure DrawFanForPerspective(OnMap : tMapForm);
     function LocatePointOnPerspective(Azimuth,Pitch : float64; var Lat,Long,DistOut : float64; var PointMess1,PLSSMess2  : shortstring; DoPLSS : boolean = false; DeleteRadial : boolean = true) : boolean;
     procedure ShowHorizonOnView(SecondImage : tImage);
     procedure EnableAlphaBlending(fName : PathStr);
     procedure DistanceOverlay;

     {$IfDef RecordPerspectiveSize}
        procedure RecordFormSize(Where : ShortString);
     {$EndIf}
  end;


procedure SetUpPanoramaView(var DEMPersF : TThreeDview; Lat,Long,CamerasElevAboveGround,Depth,inAzimuth,HFOV,VFOV,Depress : float64; DEMUsed : integer;
    ViewCaption : shortstring = ''; DoItDraped : boolean = false; WhatStarting : tDEMDoingWhat = JustWandering; MapForm : tMapForm = Nil; Distance : float64 = 0);

var
  PersPitchAzimuthDigitize : boolean;

implementation

{$R *.DFM}

uses
   {$IfDef ExGIS}
   {$Else}
      demdatabase,
   {$EndIf}
   {$IfDef ExPLSS}
   {$Else}
      DEM_PLSS,
   {$EndIf}

   {$IfDef ExSat}
   {$Else}
      DEMEros,
   {$EndIf}

   {$IfDef ExFly}
   {$Else}
      demflycontrols,DEMFlySense,
   {$EndIf}

   {$IfDef ExStereo}
   {$Else}
      Stereo_viewer,
   {$EndIf}

   {$IfDef RegisterPhoto}   //unclear if all the code for this is still available, and if it would run
      Register_photo,
   {$EndIf}

   {$IfDef ExGeography}
   {$Else}
      Sun_Position,
   {$EndIf}

   Petimage_form,
   DEMPerOp,
   DEM_Indexes,
   Petdbutils,
   DEMCoord,DEMRefOp, dem_gaz_opts, Make_tables,
   Demdef_routines,
   BaseMap,
   Nevadia_Main;


var
   YMin, YMinNew  : ^HideArray;
   ResizingNow,
   NeedErrorString,
   GetRadial,
   AnyPointSet   : boolean;
   PersNumStr    : string[15];
   MovieList     : tStringList;


procedure SetUpPanoramaView(var DEMPersF : TThreeDview; Lat,Long,CamerasElevAboveGround,Depth,inAzimuth,HFOV,VFOV,Depress : float64; DEMUsed : integer;
    ViewCaption : shortstring = ''; DoItDraped : boolean = false; WhatStarting : tDEMDoingWhat = JustWandering; MapForm : tMapForm = Nil; Distance : float64 = 0);
var
   zobs : float32;
begin
   {$If Defined(RecordPerspectiveTimeProblems) or Defined(RecordTrackObserverElevation)} WriteLineToDebugFile('SetUpPanoramaView' + '  Location:  ' + LatLongDegreeToString(lat,long)); {$EndIf}

    if not DEMGlb[DEMused].GetElevFromLatLongDegree(Lat,Long,zobs) then begin
      MessageToContinue(NoDEMCovers);
      exit;
   end;

   {$IfDef RecordSetUpPanoramaView}
      WriteLineToDebugFile('  zobs=' + RealToString(zobs,-8,1) + '  Pitch/Depress=' + RealToString(Depress,-8,2));
      WriteLineToDebugFile('  Depth=' + RealToString(Depth,-8,0) + '  Azimuth=' + RealToString(inAzimuth,-8,2 )+ '  HFOV=' + RealToString(HFOV,-8,1) + '  VFOV=' + RealToString(VFOV,-8,1));
   {$EndIf}

   if (DEMPersF = Nil) then begin
      {$IfDef RecordSetUpPanoramaView} WriteLineToDebugFile('Need to create DEMPersF'); {$EndIf}
      DEMPersF := TThreeDview.Create(Application);
      DEMPersF.ScrollBox1.HorzScrollBar.Position := Round(0.5 * DEMPersF.Image1.Width);
   end;
   OpenPanoramaView(DEMPersF.View3D,Lat,Long,CamerasElevAboveGround,Depth,inAzimuth,HFOV,VFOV,Depress, DEMUsed ,ViewCaption,DoItDraped, WhatStarting,Distance);
   DEMPersF.ScrollingView := (HFOV > 75);

   {$IfDef RecordSetUpPanoramaView} WriteLineToDebugFile('SetUpPanoramaView after SetSize, ' + DEMPersF.View3D.VertAngleRange + DEMPersF.View3D.FieldOfView) ; {$EndIf}

   if (DEMGlb[DEMUsed].SelectionMap <> Nil) then begin
      {$IfDef RecordDefineDatum} WriteLineToDebugFile('SetUpPanoramaView call define datum'); {$EndIf}
      DEMGlb[DEMUsed].SelectionMap.MapDraw.PrimMapProj.DefineDatumFromUTMZone(MDdef.PreferPrimaryDatum,GetUTMZone(Long),HemiFromLat(Lat),'SetUpPanoramaView');
   end;

   DEMPersF.SetFocus;

   {$IfDef RecordSetUpPanoramaView} WriteLineToDebugFile('focus set, ViewAzimuth=' + RealToString(DEMPersf.View3D.ViewAzimuth,-8,2) + '  ' + DEMPersF.View3D.VertAngleRange + DEMPersF.View3D.FieldOfView);  {$EndIf}

   if DoItDraped then begin
      {$IfDef RecordPerspective} WriteLineToDebugFile('DoItDraped'); {$EndIf}
      DEMPersF.View3D.SetUpDrapeMap(WhatStarting,MapForm,Distance);
      DEMPersF.View3D.PersOpts.WhichPerspective := BMPPerspective;
      {$IfDef RecordPerspective} WriteLineToDebugFile('DoItDraped done'); {$EndIf}
   end;

   DEMPersF.PerspectiveView(false);

   {$If Defined(RecordPerspectiveTimeProblems) or Defined(RecordTrackObserverElevation) or Defined(RecordSetUpPanoramaVie)} WriteLineToDebugFile('Out SetUpPanoramaView ' + DEMPersF.View3D.FieldOfView); {$EndIf}
end;


{$IfDef RecordTrackObserverElevation}
   procedure TThreeDview.TrackObserverElevation(Where : string35);
   begin
      WriteLineToDebugFile(Where + ' Obs elev: ' + RealToString(View3D.ObserverElevation,-8,1) + '  Obs up: ' + RealToString(View3D.PersOpts.PersObsUp,-8,1));
   end;
{$EndIf}


{$IfDef RecordPerspectiveSize}
   procedure TThreeDview.RecordFormSize;
   begin
      WriteLineToDebugFile(Where + ' Form size:   ' + IntToStr(Width) + ' x ' + IntToStr(Height) + '  Client size: ' + IntToStr(ClientWidth) + ' x ' + IntToStr(ClientHeight) + '  Image size:  ' + IntToStr(Image1.Width) + ' x ' + IntToStr(Image1.Height));
   end;
{$EndIf}

procedure TThreeDview.SymbolAtPitchAzimith(Pitch,Azimuth : float64; Sym : tFullSymbolDeclaration; Remeber : boolean = false);
var
   x,y : integer;
begin
   View3D.AzimuthToScreen(Azimuth,x);
   View3D.PitchToScreen(Pitch,y);
   Petmar.ScreenSymbol(Image1.Canvas,x,y,Sym);
   if Remeber then begin
      DrawPointOnRedraw := true;
      dporPitch := Pitch;
      dporAzimuth := Azimuth;
      dporSym := sym;
   end;
   {$IfDef RecordFindLatLong} WriteLineToDebugFile('TThreeDview.SymbolAtPitchAzimith, plot at '+ IntToStr(x) + '/' + IntToStr(y)); {$EndIf}
end;


procedure TThreeDview.ShowHorizonOnView(SecondImage : tImage);
var
   Azimuth : float32;
   BlockAngle,BlockDist,BlockLat,BlockLong : float64;
   xp,yp : integer;
begin
   {$IfDef RecordHorizon} WriteLineToDebugFile('TThreeDview.ShowHorizonOnView in'); {$EndIf}
   Image1.Canvas.CopyMode := cmSrcAnd;
   if (SecondImage <> Nil) then SecondImage.Canvas.CopyMode := cmSrcAnd;
   with View3D do begin
      StartProgressAbortOption('Horizon');
      for xp := 0 to pred(Image1.Width) do begin
         UpdateProgressBar(xp / Image1.Width);
         ScreenToAzimuth(xp,Azimuth);
         DEMGlb[DEMonView].HorizonBlocking(ViewerLat,ViewerLong,Azimuth,ViewDepth,View3D.ObsElev-View3D.ElevObs,BlockAngle,BlockDist,BlockLat,BlockLong,View3D.StraightLineAlgorithm);
         PitchToScreen(BlockAngle,yp);
         {$IfDef RecordHorizon} WriteLineToDebugFile('x=' + IntToStr(xp) + '  Az=' + RealToString(Azimuth,-8,1) + '   Pitch=' + RealToString(BlockAngle,-8,2)); {$EndIf}
         if (SecondImage <> Nil) then SecondImage.Canvas.Pixels[xp,yp] := clRed;
         Image1.Canvas.Pixels[xp,yp] := clRed;
         If WantOut then break;
      end;
      EndProgress;
   end;
end;


procedure TThreeDview.EnableAlphaBlending(fName : PathStr);
begin
   {$IfDef ExAlphaBlending}
   {$Else}
      Panel1.Height := 41;
      UseDefaultResize := false;
      FormResize(Nil);
      UseDefaultResize := true;
      CopyImageToBitmap(Image1,PersViewBaseBMP);
      OverlayOpaqueBMP := tMyBitmap.Create;
      OverlayOpaqueBMP.LoadFromFile(fName);
   {$EndIf}
end;


procedure TThreeDview.SetPerspectiveWindowSize;
begin
   ResizingNow := true;
   {$IfDef RecordPerspectiveSize}  RecordFormSize('TThreeDview.SetPerspectiveWindowSize in'); {$EndIf}

   Image1.Height := View3D.ViewPortHeight;
   Image1.Width := View3D.ViewPortWidth;

   {$IfDef RecordPerspectiveSize} RecordFormSize('TThreeDview.SetPerspectiveWindowSize 1'); {$EndIf}

    if (ClientWidth < Image1.Width) and (Image1.Width < WMDEM.ClientWidth - 25) then ClientWidth := Image1.Width;
    if (ClientHeight < Image1.Height + Toolbar1.height + Panel1.Height) and (Image1.Height < WMDEM.ClientHeight - 80) then ClientHeight := Image1.Height + Toolbar1.height + Panel1.Height;

    if (Width > WMDEM.ClientWidth - 25) then Width := WMDEM.ClientWidth - 25;
    if (Left + Width > WMDEM.ClientWidth) then Left := 5;

    if (ClientWidth > Image1.Width) then ClientWidth := Image1.Width;
    if (ClientHeight > Image1.Height + Toolbar1.height + Panel1.Height) then ClientHeight := Image1.Height + Toolbar1.height + Panel1.Height;

    ScrollBox1.HorzScrollBar.Visible := Image1.Width > ClientWidth;
    if ScrollBox1.HorzScrollBar.Visible then ClientHeight := Image1.Height + Toolbar1.height  + Panel1.Height + 15;
    ScrollBox1.VertScrollBar.Visible := Image1.Height + Toolbar1.height  + Panel1.Height > ClientHeight;
   ResizingNow := false;
   {$IfDef RecordPerspectiveSize} RecordFormSize('TThreeDview.SetPerspectiveWindowSize out'); {$EndIf}
end;


procedure TThreeDview.DistanceOverlay;
var
   az, pit : float32;
   Dist,Lat2,Long2 : float64;
   Mes1,Mes2 : shortstring;
   xp,yp : integer;
   Bitmap : tBitmap;
   p3 : tScreenPRGB;
begin
   CopyImageToBitmap(Image1,Bitmap);
   FillScanlineAddresses(Bitmap,P3);
   StartProgress('Distances');
   xp := 0;
   while (xp <= View3D.ViewportWidth) do begin
      if (xp mod 100 = 0) then UpdateProgressBar(xp/View3D.ViewportWidth);
      yp := pred(View3D.ViewportHeight);
      while (yp >=0) do begin
         View3D.ScreenToAzimuthPitch(Xp,Yp,Az,Pit);
         if LocatePointOnPerspective(Az,Pit,Lat2,Long2,Dist,Mes1,Mes2,false) then begin
            p3[yp][xp] := RainbowRGBFunct(Dist,0,View3D.ViewDepth);
         end
         else break;
         dec(yp);
      end;
      inc(xp);
   end;
   Image1.Picture.Graphic := bitmap;
   Bitmap.Free;
   EndProgress;
end;


procedure TThreeDview.DrawFanForPerspective(Onmap : tMapForm);
{$IfDef ExViewshed}
begin
{$Else}
var
   Bitmap : tMyBitmap;
   WeaponsFan : tWeaponsFan;
   fName : PathStr;
begin
   InitializeWeaponsFan(WeaponsFan);
   WeaponsFan.W_Lat := View3d.ViewerLat;
   WeaponsFan.W_Long := View3d.ViewerLong;
   WeaponsFan.W_Range := View3d.ViewDepth;
   if View3d.PersOpts.NapEarth then WeaponsFan.W_Up := View3d.PersOpts.PersObsUp
   else WeaponsFan.W_Up := View3d.ObsElev - View3d.ElevObs;
   WeaponsFan.W_TargetUp := 0;
   WeaponsFan.StartAngle := round(View3D.LeftAzimuth);
   WeaponsFan.EndAngle := round(View3D.RightAzimuth);
   PETImage.CopyImageToBitmap(OnMap.Image1,Bitmap);
   OnMap.MapDraw.DrawFanOnMap(WeaponsFan,fName,'');
   OnMap.MapDraw.DrawWorldFileImageOnMap(Bitmap,fName);
   OnMap.Image1.Picture.Graphic := Bitmap;
   Bitmap.Free;
{$EndIf}
end;


{$F+}
procedure HiddenSetPoint(BitMap : tMyBitmap; x,y : integer; color : TColor);
begin
  if (y < YMin^[x]) then begin
     Bitmap.Canvas.Pixels[x,y] := Color;
     AnyPointSet := True;
  end {if};
  if y < YMinNew^[x] then YMinNew^[x] := y;
end;


function TThreeDview.DrawPerspective(SkipDraw : boolean) : boolean;
var
   i,j    : integer;
   Bitmap : tMyBitmap;
begin {proc DrawPerspective}
   Result := false;
   SpeedButton15.Visible := false;
   SpeedButton16.Visible := false;
   SpeedButton17.Visible := false;
   SpeedButton18.Visible := false;
   FormResize(Nil);
   {$IfDef RecordTrackObserverElevation}  TrackObserverElevation('TThreeDview.DrawPerspective in'); {$EndIf}
    with View3D do begin
       View3D.SetAzimuthLimits;
       View3D.SetSize;
       NumPointsOnRay[1] := round(ViewDepth / RadialPointSpacing[1]);
       SetPerspectiveWindowSize;
    end;
    if SkipDraw then exit;

   {$IfDef RecordPerspective} WriteLineToDebugFile('TThreeDview.DrawPerspective enter  size = ' + IntToStr(View3D.ViewportWidth) + 'x' + IntToStr(View3D.ViewportHeight)); {$EndIf}
   {$IfDef RecordMapDraw} WriteLineToDebugFile('TThreeDview.DrawPerspective Target coordinates:' + RealToString(View3D.TargetXUTM,10,0) + RealToString(View3D.TargetYUTM,12,0) + '  ' + View3D.VertAngleRange); {$EndIf}

   {$IfDef ExFly}
   {$Else}
      if LiveFlying then FlightControlForm.SetFocus;
   {$EndIf}

   StartProgressAbortOption('Perspective ' + PersNumStr);
   if View3D.PerspectiveDraw(Bitmap,(not LiveFlying)) then begin
      if (PositionMap <> Nil) then with PositionMap.Image1.Canvas do begin
         {$IfDef RecordMapDraw} WriteLineToDebugFile('Loading temp map.bmp'); {$EndIf}
         PositionMap.Image1.Picture.LoadFromFile(MDTempDir + 'temp map.bmp');
         if (View3D.TargetXUTM > 1) then begin
            PositionMap.MapDraw.UTMtoScreen(View3D.TargetXUTM,View3D.TargetYUTM,i,j);
            ScreenSymbol(PositionMap.Image1.Canvas,i,j,Box,2,ConvertTColorToPlatformColor(clRed));
         end;
         PositionMap.OutlinePerspectiveView( View3D.ViewHFOV,View3D.ViewerLat,View3D.ViewerLong,View3D.ViewDepth,View3D.ViewAzimuth,pmCopy);
         if abs(View3D.ViewAzimuth - View3D.FlightAzimuth) > 0.5 then begin
            PositionMap.MapDraw.LatLongDegreetoScreen(View3D.ViewerLat,View3D.ViewerLong,i,j);
            PositionMap.Image1.Canvas.Pen.Color := clYellow;
            PositionMap.Image1.Canvas.MoveTo(i,j);
            PositionMap.Image1.Canvas.LineTo(round(i+150*SinDeg(View3D.FlightAzimuth)),round(j-150*CosDeg(View3D.FlightAzimuth)));
         end;
      end;
      if MDDef.ShowCrossTrackProfiles  and (CrossTrackProfile <> Nil) then begin
         DEMGlb[View3d.DEMonView].DrawCrossTrackProfile(CrossTrackProfile,View3D.ViewerLat,View3D.ViewerLong,View3D.FlightAzimuth,MDDef.FlyCrossTrackDistance,0.5*DEMGlb[View3D.DEMonView].AverageSpace);
         with CrossTrackProfile do petmar.ScreenSymbol(Image1.Canvas,GraphDraw.GraphX(0),GraphDraw.GraphY(View3D.ObsElev),FilledBox,3,ConvertTColorToPlatformColor(clRed));
      end;

      Image1.Picture.Graphic := Bitmap;
      Bitmap.Free;
      if (View3D.PersOpts.ViewShedFanWithPerspective) then begin
         if (OwningMap <> Nil) then begin
            OwningMap.DoFastMapRedraw;
            DrawFanForPerspective(OwningMap);
         end
         else begin
            DEMGlb[View3D.DEMonView].SelectionMap.DoFastMapRedraw;
            DrawFanForPerspective(DEMGlb[View3D.DEMonView].SelectionMap);
         end;
      end;
      if not LiveFlying then EndProgress;
      SpeedButton8.Visible := (View3d <> Nil) and ((View3d.MainInCh = 'P') and (View3D.PersOpts.WhichPerspective = BMPPerspective));
      Result := true;
      EndProgress;
   end
   else begin
      EndProgress;
      close;
   end;

   {$IfDef RecordTrackObserverElevation}  TrackObserverElevation('TThreeDview.DrawPerspective out'); {$EndIf}
end {proc DrawPerspective};



procedure TThreeDview.PerspectiveView;
label
   OffImage,WantedOut;
var
   NumViews,DiffView,MilliSecs,LastMillisecs,
   PicNum,CPTHeight,NImages,i  : integer;
   rfile,rfile2,rfile3 : file;
   v : array[1..2] of float32;
   MFile,FName : PathStr;
   Single    : boolean;
   Lat,Long,Lat2,Long2,MaxR,OffR,MaxL,OffL,
   ViewHeadingRelative,az,az2,az3 : float64;
   zt : float32;
   BigBitMap : tMyBitmap;
   Redraw : boolean;
   FlyThroughFiles : tStringList;
   OutF            : textFile;
   Dir : DirStr;
   bName : NameStr;
   Ext : ExtStr;
   TStr : ShortString;


      function GetMillisecs : integer;
      var
         SysTime : _SystemTime;
      begin
         GetSystemTime(SysTime);
         with SysTime do begin
            Result := wHour * 60 * 60 * 1000 + wMinute * 60 * 1000 + wSecond * 1000 + wMilliSeconds;
         end;
      end;

          {$IfDef ExFly}
          {$Else}
          function CheckLiveFlyingControl : boolean;
          var
             i,j : integer;
          begin
             Result := true;
             if LiveFlying then with FlightControlForm,View3D,DEMGlb[DEMonView] do begin
                if (PositionMap <> Nil) then with PositionMap.Image1.Canvas do begin
                   {$IfDef RecordMapDraw} WriteLineToDebugFile('Loading temp map.bmp'); {$EndIf}
                   PositionMap.Image1.Picture.LoadFromFile(MDTempDir + 'temp map.bmp');
                   if View3D.TargetXUTM > 1 then begin
                      PositionMap.MapDraw.UTMtoScreen(TargetXUTM,TargetYUTM,i,j);
                      ScreenSymbol(PositionMap.Image1.Canvas,i,j,Box,2,ConvertTColorToPlatformColor(clRed));
                   end;
                   PositionMap.Image1.Canvas.Pen.Color := clRed;
                   PositionMap.Image1.Canvas.Pen.Width := 2;
                   for i := 0 to 4 do begin
                      if (i = 0) then j := 4 else j := i;
                      PositionMap.MapDraw.DEMGridToScreen(xu[j],yu[j],x,y);
                      if (i = 0) then PositionMap.Image1.Canvas.MoveTo(x,y)
                      else PositionMap.Image1.Canvas.LineTo(x,y);
                      {$IfDef RecordMapDraw} if (i > 0) then WriteLineToDebugFile('Draw Corner ' + IntToStr(i) + ': ' + RealToString(xu[i],8,0) + RealToString(yu[i],8,0)); {$EndIf}
                   end;
                   if (abs(ViewAzimuth - FlightAzimuth) > 0.5) then begin
                      PositionMap.MapDraw.LatLongDegreetoScreen(ViewerLat,ViewerLong,i,j);
                      PositionMap.Image1.Canvas.Pen.Color := clYellow;
                      PositionMap.Image1.Canvas.MoveTo(i,j);
                      PositionMap.Image1.Canvas.LineTo(round(i+150*SinDeg(FlightAzimuth)),round(j-150*CosDeg(FlightAzimuth)));
                   end;
                end;

                 MilliSecs := GetMillisecs;
                 Self.Caption := DEMGlb[View3D.DEMonView].AreaName + RealToString( 1000 / (MilliSecs - LastMillisecs),12,2) + ' f.p.s.';
                 if (MilliSecs - LastMillisecs) < MDDef.FlyOptions.LiveFlyDelay then Delay(MDDef.FlyOptions.LiveFlyDelay-(MilliSecs - LastMillisecs));
                 LastMilliSecs := MilliSecs;

                 FlightControlForm.SetFocus;
                 if (Not View3D.PointOnView) then with View3D do begin
                    if AnswerIsYes('Off map; reverse course') then begin
                       CheckEditString(Edit2.Text,FlightAzimuth);
                       FlightAzimuth := FlightAzimuth + 180;
                       FlightAzimuth := CompassAngleInRangeFloat32(FlightAzimuth);
                       Edit2.Text := RealToString(FlightAzimuth,-6,1);
                    end
                    else WantToEndFlying := true;
                 end;

                 if PitchUp then begin
                    View3D.MinPitch := View3D.MinPitch + 1;
                    View3D.MaxPitch := View3D.MaxPitch + 1;
                    PitchUp := false;
                 end;

                 if PitchDown then begin
                    View3D.MinPitch := View3D.MinPitch - 1;
                    View3D.MaxPitch := View3D.MaxPitch - 1;
                    PitchDown := false;
                 end;

                 if EditColors then ChangeReflectanceOptions(Nil);
                 EditColors := false;

                 if EditControls then ChangeFlightControls;
                 EditControls := false;

                 CheckEditString(Edit1.Text,View3D.FlyOpts.FlySceneSeparation);
                 CheckEditString(Edit2.Text,View3D.FlightAzimuth);
                 CheckEditString(Edit3.Text,View3D.ObsElev);
                 ViewHeadingRelative := TrackBar1.Position;

               FlightControlForm.Label4.Caption := 'View: ' + IntToStr(FlightControlForm.TrackBar1.Position) + '° (relative)';

               ViewAzimuth := FlightAzimuth + ViewHeadingRelative;
               ViewAzimuth := CompassAngleInRangeFloat32(ViewAzimuth);

               Az2 := FlightAzimuth - ViewHeadingRelative + DEMGlb[DEMonView].SelectionMap.MapDraw.MapGridTrueAngle;
               Az2 := CompassAngleInRangeFloat32(Az2);
               Az3 := CompassAngleInRangeFloat32(Az2 + DEMGlb[DEMonView].SelectionMap.MapDraw.MapMagDec);
               FlightControlForm.Label2.Caption := RealToString(Az2,8,1) + '° True    ' + RealToString(Az3,8,1) + '° Mag';

               SetAzimuthLimits;

               VincentyPointAtDistanceBearing(ViewerLat,ViewerLong,FlyOpts.FlySceneSeparation,FlightAzimuth,ViewerLat,ViewerLong);
               DEMGlb[DEMOnView].LatLongDegreeToDEMGrid(ViewerLat,ViewerLong,XGridRight,YGridRight);
               VincentyPointAtDistanceBearing(ViewerLat,ViewerLong,ViewDepth,ViewAzimuth,ViewedLat,ViewedLong);

               DEMGlb[DEMonView].GetElevMeters(XGridRight,YGridRight,Zt);

               FlyOpts.LiveFlyAutoNap := FlightControlForm.CheckBox1.Checked;
               MDdef.FlyOptions.LiveFlyAutoNap := FlightControlForm.CheckBox1.Checked;
               PersOpts.NapEarth := FlightControlForm.CheckBox1.Checked;

               if FlyOpts.LiveFlyAutoNap then begin
                  View3D.ObsElev := zt + FlyOpts.FlyHeight;
                  FlightControlForm.Edit3.Text := RealToString(View3D.ObsElev,-12,0);
               end
               else begin
                   if (View3D.ObsElev < zt) then View3D.ObsElev := zt + 1;
               end;

               FlightControlForm.Label1.Caption := RealToString(View3D.ObsElev -zt,-12,1) + ' m above ground';

               View3D.FlightRouteDB.Insert;
               View3D.FlightRouteDB.SetFieldByNameAsFloat('LAT',ViewerLat);
               View3D.FlightRouteDB.SetFieldByNameAsFloat('LONG',ViewerLong);
               View3D.FlightRouteDB.SetFieldByNameAsFloat('AZIMUTH',ViewAzimuth);
               View3D.FlightRouteDB.SetFieldByNameAsFloat('PITCH',0.5 * (View3D.MinPitch + View3D.MaxPitch));
               View3D.FlightRouteDB.SetFieldByNameAsFloat('Z',zt);
               View3D.FlightRouteDB.SetFieldByNameAsFloat('ALTITUDE',View3D.ObsElev);
               View3D.FlightRouteDB.Post;

               if WantToEndFlying then begin
                  if SaveFlight then begin
                     FName := MovieDir;
                  end;
                  FlightControlForm.Free;
                  {$IfDef RecordPerspective} writeLineToDebugFile('Closed flight control window');       {$EndIf}
                  Result := false;
               end;
            end {if};
         end {proc};
      {$EndIf}


begin {proc PerspectiveView}
   {$If Defined(RecordPerspectiveProblems) or Defined(RecordTrackObserverElevation)} WriteLineToDebugFileWithTime('TThreeDview.PerspectiveView enter, flydepth=' + IntToStr(MDdef.PerspOpts.FlyDepth) + ' DrapeMapUsing=' + IntToStr(View3D.DrapeMapUsing)); {$EndIf}
   {$IfDef RecordMapDraw} WriteLineToDebugFile('Start PerspectiveView Target coordinates:' + RealToString(View3D.TargetXUTM,10,0) + RealToString(View3D.TargetYUTM,12,0) + View3D.FieldOfView); {$EndIf}
   ViewHeadingRelative := 0;
  //with DEMGlb[View3D.DEMOnView] do begin
      Single := (View3D.FlightRouteDB = Nil) or ((View3D.FlightRouteDB.RecordCount in [0,1]) and (not LiveFlying));
      View3D.MainInCh := 'P';
      View3D.ASingleView := Single;
      Caption := DEMGlb[View3D.DEMOnView].AreaName + ' Perspective View';

      NumViews := 1;

      if (View3D.FlightRouteDB <> Nil) then begin
         View3D.FlightRouteDB.First;
         if MDDef.ShowAlongTrackProfiles then begin
            View3D.FlightRouteDB.InsureFieldPresentAndAdded(ftFloat,'CLEAR_R',8,2);
            View3D.FlightRouteDB.InsureFieldPresentAndAdded(ftFloat,'CLEAR_OFFR',8,2);
            View3D.FlightRouteDB.InsureFieldPresentAndAdded(ftFloat,'CLEAR_L',8,2);
            View3D.FlightRouteDB.InsureFieldPresentAndAdded(ftFloat,'CLEAR_OFFL',8,2);
         end;
      end;

      {$IfDef RecordPerspective} WriteLineToDebugFile('Point 2' + View3D.FieldOfView); {$EndIf}

      for i := 0 to 2 do begin
         View3D.RadialPointSpacing[i] := (DEMGlb[View3D.DEMonView].AverageXSpace);
         {$IfDef ExSat}
         {$Else}
            if (i > 0) and (View3D.DrapingMaps[i] <> Nil) and IsSatelliteMap(View3D.DrapingMaps[i].MapDraw.MapType) then View3D.RadialPointSpacing[i] := (SatImage[View3D.DrapingMaps[i].MapDraw.SatOnMap].MetersPerPixel);
         {$EndIf}
         View3D.NumPointsOnRay[i] := round(View3D.ViewDepth / View3D.RadialPointSpacing[i]);
         if (View3D.NumPointsOnRay[i] < 150) then View3D.NumPointsOnRay[i] := 150;
      end {for i};

      {$IfDef ExFly}
      {$Else}
         if not Single then begin
            Left := 0;
            Top := 0;
            if (PositionMap <> Nil) then begin
               PositionMap.Left := Width + 5;
               PositionMap.Top := 0;
            end;
         end;
         if (PositionMap <> Nil) then PositionMap.Image1.Picture.SaveToFile(MDTempDir + 'temp map.bmp');
      {$EndIf}

      {$IfDef RecordPerspective} WriteLineToDebugFile('Point 3' + View3D.FieldOfView); {$EndIf}

      ImageDrawn := true;
      if Single then begin
         {$IfDef RecordPerspective} WriteLineToDebugFile('Point 4' + View3D.FieldOfView); {$EndIf}
         if View3D.ViewWithInData(false,true,(View3D.MaxPitch < -99)) then View3D.MainInCh := 'P';
         PersNumStr := '';
         {$IfDef RecordPerspective} WriteLineToDebugFile('Point 5' + View3D.FieldOfView); {$EndIf}
         DrawPerspective(SkipDraw);
         View3D.RedrapeEachFrame := false;
         {$IfDef RecordPerspective} WriteLineToDebugFile('Point 6' + View3D.FieldOfView); {$EndIf}
      end
      else begin
         Closable := false;

         {$IfDef RecordAzimuthLimits} WriteLineToDebugFile('>>>>>>>>>>> Get view parameters ' + View3d.VertAngleRange) ; {$EndIf}

         if (View3D.FlightRouteDB <> Nil) then begin
            BaseGraf.CreateSmallGraph := true;
            NImages := View3D.FlightRouteDB.RecordCount;
            if FlightPathGraphsPossible and MDDef.ShowCrossTrackProfiles then begin
               CrossTrackProfile := BaseGraf.TThisBaseGraph.Create(Application);
               CrossTrackProfile.GraphDraw.HorizLabel := 'Distance from track (m)';
               CrossTrackProfile.GraphDraw.VertLabel := 'Altitude (m)';
               CrossTrackProfile.GraphDraw.MaxVertAxis := MDDef.FlyCrossTrackheight;
               CrossTrackProfile.GraphDraw.MinHorizAxis := -MDDef.FlyCrossTrackDistance;
               CrossTrackProfile.GraphDraw.MaxHorizAxis := MDDef.FlyCrossTrackDistance;
               CrossTrackProfile.SetUpGraphForm;
               CrossTrackProfile.Top := Self.Height + 5;
               CrossTrackProfile.Left := 0;
            end;

            if FlightPathGraphsPossible and MDDef.ShowAlongTrackProfiles then begin
               AlongTrackGlide := BaseGraf.TThisBaseGraph.Create(Application);
               AlongTrackGlide.GraphDraw.HorizLabel := 'Distance out (km)';
               AlongTrackGlide.GraphDraw.VertLabel := 'Altitude (m)';
               AlongTrackGlide.Top := Self.Height + 5;
               AlongTrackGlide.Left := Self.Width + 5;
               AlongTrackGlide.MainSymbol.Color := ConverttColorToPlatformColor(clGreen);
               AlongTrackGlide.OpenPointSymbolFile(rfile,'flight',AlongTrackGlide.MainSymbol);
               AlongTrackGlide.MainSymbol.Color := ConverttColorToPlatformColor(clGray);
               AlongTrackGlide.OpenPointSymbolFile(rfile2,'flight',AlongTrackGlide.MainSymbol);
               AlongTrackGlide.MainSymbol.Color := ConverttColorToPlatformColor(clSilver);
               AlongTrackGlide.OpenPointSymbolFile(rfile3,'flight',AlongTrackGlide.MainSymbol);
               View3D.FlightRouteDB.First;
               while not View3D.FlightRouteDB.eof do begin
                  v[1] := View3D.FlightRouteDB.GetFieldByNameAsFloat('DISTANCE');
                  v[2] := View3D.FlightRouteDB.GetFieldByNameAsFloat('Z');
                  Blockwrite(rfile,v,1);
                  v[2] := View3D.FlightRouteDB.GetFieldByNameAsFloat('ALTITUDE');
                  Blockwrite(rfile2,v,1);
                  Lat := View3D.FlightRouteDB.GetFieldByNameAsFloat('LAT');
                  Long := View3D.FlightRouteDB.GetFieldByNameAsFloat('LONG');
                  az := View3D.FlightRouteDB.GetFieldByNameAsFloat('AZIMUTH');
                  MaxR := -9999;
                  MaxL := -9999;
                  for I := 1 to MDDef.NumSideProfiles do begin
                     VincentyPointAtDistanceBearing(Lat,Long,i*MDDef.SideProfileSpacing,az+90,Lat2,Long2);
                     if DEMGlb[View3D.DEMonView].GetElevFromLatLongDegree(Lat2,Long2,v[2]) then begin
                        Blockwrite(rfile3,v,1);
                        if v[2] > MaxR then begin
                           MaxR := v[2];
                           OffR := i*MDDef.SideProfileSpacing;
                        end;
                     end;
                     VincentyPointAtDistanceBearing(Lat,Long,i*MDDef.SideProfileSpacing,az-90,Lat2,Long2);
                     if DEMGlb[View3D.DEMonView].GetElevFromLatLongDegree(Lat2,Long2,v[2]) then  begin
                        Blockwrite(rfile3,v,1);
                        if v[2] > MaxL then begin
                           MaxL := v[2];
                           OffL := i*MDDef.SideProfileSpacing;
                        end;
                     end;
                  end;
                  View3D.FlightRouteDB.Edit;
                  View3D.FlightRouteDB.SetFieldByNameAsFloat('CLEAR_R',MaxR);
                  View3D.FlightRouteDB.SetFieldByNameAsFloat('CLEAR_OFFR',OffR);
                  View3D.FlightRouteDB.SetFieldByNameAsFloat('CLEAR_L',MaxL);
                  View3D.FlightRouteDB.SetFieldByNameAsFloat('CLEAR_OFFL',OffL);
                  View3D.FlightRouteDB.Next;
               end;
               CloseFile(rfile);
               CloseFile(rfile2);
               AlongTrackGlide.AutoScaleAndRedrawDiagram;
               View3D.FlightRouteDB.First;
            end;
         end;

      {$IfDef RecordAzimuthLimits} WriteLineToDebugFile('>>>> Done view parameters ' + View3D.VertAngleRange); {$EndIf}

      {$IfDef ExMovies}
      {$Else}
         MFile := DEMdefs.MovieDir + View3D.SaveName + '.MOV';
      {$EndIf}

      {$IfDef ExFly}
      {$Else}
         if LiveFlying then begin
            ToolBar1.Visible := false;
            Toolbar1.Height := 0;
            DrawPerspective(false);
            FlightControlForm.Left := 100;
            FlightControlForm.Top := Height + 5;
            LastMillisecs := GetMillisecs;
            View3D.StraightLineAlgorithm := saDEMGrid;
         end;
      {$EndIf}

         FlyThroughFiles := tStringList.Create;
         if LiveFlying then NImages := MaxInt;
         for PicNum := 0 to pred(NImages) do begin
            for DiffView := 1 to NumViews do begin
               {$IfDef ExFly}
               {$Else}
                  if LiveFlying then FlightControlForm.SetFocus;
               {$EndIf}
               if (PositionMap <> Nil) then PositionMap.Image1.Picture.LoadFromFile(MDTempDir + 'temp map.bmp');

               if View3D.TargetRun then View3D.RedrapeEachFrame := (PicNum mod 5) = 0;
               PersNumStr := '  ' + IntegerToString(PicNum,-4) + '/' + IntegerToString(NImages,-4);

               {$IfDef ExFly}
               {$Else}
               if (View3D.FlyOpts.NumTargetViews = 2) then begin
                  if (DiffView = 1) then View3D.ViewHFOV := View3D.FlyOpts.TargetFOV2;
               end
               else {$EndIf} View3D.DrapeMapUsing:= DiffView;

               if (not LiveFlying) then begin
                  View3D.ObsElev := 0;
                  View3D.SeriesNumberCoord(PicNum);
               end {if};

               View3D.NumPointsOnRay[DiffView] := round(View3D.ViewDepth / View3D.RadialPointSpacing[DiffView]);
               if (View3D.NumPointsOnRay[DiffView] < 150) then View3D.NumPointsOnRay[DiffView] := 150;

               DrawPerspective(false);
               ApplicationProcessMessages;

               {$IfDef ExFly}
               {$Else}
                  {$IfDef ExMovies}
                  {$Else}
                     if (not LiveFlying) or View3D.FlyOpts.LiveFlyMovie then try
                        if (NumViews = 1) then TStr := ''
                        else if (DiffView = 1) then TStr := 'a' else TStr := 'b';
                        FName := View3D.SaveName + IntegerToString(PicNum,-4) + TStr + MovieFileExt;

                        if (PositionMap = Nil) then Image1.Picture.SaveToFile(DEMdefs.MovieDir + FName)
                        else begin
                            BigBitMap := tMyBitmap.Create;
                            if (CrossTrackProfile <> Nil) then CPTHeight := CrossTrackProfile.Height + 5
                            else CPTHeight := 0;
                            if View3D.FlyOpts.FlySideBySide then begin
                               BigBitMap.Height := Image1.Height+1 + CPTHeight;
                               BigBitMap.width := Image1.Width + PositionMap.Image1.Width + 5;
                               BigBitMap.Canvas.CopyRect(Rect(0,0,pred(Image1.Width),pred(Image1.Height)),Image1.Canvas,Rect(0,0,pred(Image1.Width),pred(Image1.Height)));
                               BigBitMap.Canvas.CopyRect(Rect(Image1.Width+4,0,pred(BigBitmap.Width),pred(BigBitmap.Height-CPTHeight)),
                                   PositionMap.Image1.Canvas,Rect(0,0,pred(PositionMap.Image1.Width),pred(PositionMap.Image1.Height)));
                            end
                            else begin
                               BigBitMap.Height := Image1.Height + PositionMap.Image1.Height + 5 + CPTHeight;
                               BigBitMap.width := Image1.Width;
                               BigBitMap.Canvas.CopyRect(Rect(0,0,Image1.Width,Image1.Height),Image1.Canvas,Rect(0,0,Image1.Width,Image1.Height));
                               BigBitMap.Canvas.CopyRect(Rect(0,succ(Image1.Height)+5,Image1.Width,pred(BigBitmap.Height)),PositionMap.Image1.Canvas,Rect(0,0,PositionMap.Image1.Width,PositionMap.Image1.Height));
                            end {if FlyOpts.FlySideBySide};

                            if (CrossTrackProfile <> Nil) then begin
                              BigBitMap.Canvas.CopyRect(Rect(0,BigBitMap.Height - CrossTrackProfile.Image1.Height,CrossTrackProfile.Image1.Width,BigBitMap.Height),
                                   CrossTrackProfile.Image1.Canvas,Rect(0,0,CrossTrackProfile.Image1.Width,CrossTrackProfile.Image1.Height));
                            end;
                            if (AlongTrackGlide <> Nil) then begin
                              with AlongTrackGlide do petmar.ScreenSymbol(Image1.Canvas,GraphDraw.GraphX(View3D.DistanceAlongRoute ),GraphDraw.GraphY(View3D.ObsElev),FilledBox,3,ConvertTColorToPlatformColor(clRed));
                              BigBitMap.Canvas.CopyRect(Rect(CrossTrackProfile.Image1.Width,BigBitMap.Height - CrossTrackProfile.Image1.Height,CrossTrackProfile.Image1.Width+AlongTrackGlide.Image1.Width,BigBitMap.Height),
                                   AlongTrackGlide.Image1.Canvas,Rect(0,0,AlongTrackGlide.Image1.Width,AlongTrackGlide.Image1.Height));
                            end;

                            BigBitMap.SaveToFile(DEMdefs.MovieDir + FName);
                            BigBitMap.Free;
                        end {if (PositionMap = Nil)};

                        if (MDDef.MovieFormat in [mfJPEG,mfJPEGworld]) then begin
                           fname := DEMdefs.MovieDir + FName;
                           PETImage.ConvertMyBitmapToJPEG(fName);
                           FSplit(fName,Dir,bName,Ext);
                           FName := bName + Ext;
                        end;

                        FlyThroughFiles.Add(FName);
                     except
                        on Exception do begin
                           MessageToContinue('Out of disk space' + MessLineBreak + IntToStr(PicNum) + '/' + IntToStr(NImages) + ' images generated.');
                           WantOut := true;
                           {$IfDef Record} WriteLineToDebugFile('Perspective Disk space problem');{$EndIf}
                        end;
                     end;
                  {$EndIf}

                  ApplicationProcessMessages;

                  if (not LiveFlying) and (PicNum = 0) and (NImages > 1) and (Not Wantout) and CanModifyParameters then WantOut := not AnswerIsYes('Continue w/ these parameters');

                  if Wantout then begin
                     if CanModifyParameters and AnswerIsYes('Modify flight parameters') then begin
                        ModifyWithoutRedraw(Redraw);
                        {$IfDef RecordPerspective} WriteLineToDebugFile('Modified flight parameters'); {$EndIf}
                        FlyThroughFiles.Free;
                     end;
                     GoTo WantedOut;
                  end;
                  if not CheckLiveFlyingControl then goto WantedOut;
               {$EndIf}
            end {for DiffView};
         end {for PicNum};

        WantedOut:;
         Closable := true;

        {$IfDef ExFly}
        {$Else}
         if (not LiveFlying) or View3D.FlyOpts.LiveFlyMovie then begin
            {$IfDef RecordPersective} WriteLineToDebugFile('Save ' + MFile + ',  frames=' + IntToStr(FlyThroughFiles.Count));            {$EndIf}
            FlyThroughFiles.SaveToFile(MFile);
            {$IfDef ExMovies}
            {$Else}
               if (Not Wantout) or (WantOut and AnswerIsYes('See frames generated')) then begin
                  if View3D.TargetRun then begin
                     assignFile(outf,Mfile + '.TGT');
                     rewrite(outf);
                     writeln(outf,(Image1.Width div 2):8,(Image1.Height div 2):8);
                     CloseFile(outf);
                  end {if};
                  {$IfDef RecordPersective} WriteLineToDebugFile('Opening movie after fly through ' + MFile);          {$EndIf}
                  PetImage.MakeMovie(MFile);
               end {if};
            {$EndIf}
         end {if};
         {$EndIf}
         FlyThroughFiles.Free;
         Close;
      end {if single / else};
   //end {with};
   ShowSatProgress := true;
   if MDDef.AutoLabelGaz and (View3D.PersSize in [psPerpsective,psPanorama]) then begin
      {$IfDef RecordPerspLabel} WriteLineToDebugFile('off to auto label with gaz'); {$EndIf}
      SpeedButton9Click(nil);
   end;
   {$If Defined(RecordTrackObserverElevation) or Defined(RecordPerspective)} WriteLineToDebugFile('TThreeDview.PerspectiveView out'); {$EndIf}
end {proc PerspectiveView};


procedure TThreeDview.FormResize(Sender: TObject);
begin
   TrackBar1.Width := Panel1.Width - Panel2.Width;
   if UseDefaultResize then exit;
   {$IfDef RecordPerspectiveSize} RecordFormSize('TThreeDview.FormResize in'); {$EndIf}
   Toolbar1.Repaint;
   if (View3D <> Nil) and (not ResizingNow) then begin
      if (View3D.MainInCh = 'P') then begin
         SetPerspectiveWindowSize;
         {$IfDef RecordPerspectiveSize} RecordFormSize('TThreeDview.FormResize out, "P" option'); {$EndIf}
      end;
   end;
end;

procedure TThreeDview.Saveimage1Click(Sender: TObject);
begin
   PetImage.SaveImageAsBMP(Image1);
end;

procedure TThreeDview.Alphablendbitmap1Click(Sender: TObject);
var
   fName : PathStr;
begin
   fname := '';
   if GetGraphicsFileName('',fName) then EnableAlphaBlending(fName);
end;


procedure TThreeDview.BitBtn1Click(Sender: TObject);
begin
   {$IfDef ExMovies}
   {$Else}
      PetImage.BlendMovie(OverlayOpaqueBMP,PersViewBaseBMP);
   {$EndIf}
end;


procedure TThreeDview.BitBtn2Click(Sender: TObject);
begin
   DistanceOverlay;
end;

procedure TThreeDview.CancelBtnClick(Sender: TObject);
begin
   if (Panel1.Height > 0) then begin
      Image1.Picture.Graphic := PersViewBaseBMP;
      FreeAndNil(PersViewBaseBMP);
      FreeAndNil(OverlayOpaqueBMP);
      Panel1.Height := 0;
      ClientHeight := Image1.Height;
   end;
end;

procedure TThreeDview.Close1Click(Sender: TObject);
begin
   Close;
end;

procedure TThreeDview.FormCreate(Sender: TObject);
begin
   {$IfDef RecordPerspective} WriteLineToDebugFile('TThreeDview.FormCreate in'); {$EndIf}
   MouseIsDown := false;
   UseDefaultResize := false;
   FlightPathGraphsPossible := false;
   ImageDrawn := false;
   LiveFlying := false;
   ScrollingView := false;
   NoViewResizing := false;
   WaterCheck := true;
   Closable := true;
   View3D := Nil;
   CanModifyParameters := true;
   OwningMap := Nil;
   PositionMap := Nil;
   Panel1.Height := 0;
   BaseCaption := '';
   PersViewBaseBMP := Nil;
   OverlayOpaqueBMP := Nil;
   CrossTrackProfile := Nil;
   AlongTrackGlide := Nil;

   ClientWidth := 800;    //size after all other initializations
   ClientHeight := 600;   //size after all other initializations

   Width := 800;          //size after all other initializations
   Height := 600;         //size after all other initializations

   if MDdef.ProgramOption = DragonPlotProgram then begin
      SpeedButton14.Visible := false;
      SpeedButton11.Visible := false;
      SpeedButton10.Visible := false;
      SpeedButton6.Visible := false;
      SpeedButton7.Visible := false;
   end;
   {$IfDef ExGeography}
       oday1.Visible := false;
       Pickday1.Visible := false;
   {$EndIf}

   PlaceFormAtMousePosition(Self);
   {$IfDef RecordPerspective} WriteLineToDebugFile('TThreeDview.FormCreate out'); {$EndIf}
end;


procedure TThreeDview.ModifyWithoutRedraw(var Redraw : boolean);
begin
   if (View3D.MainInCh = 'P') and DEMPerOp.SetPerspectiveOptions(true,Self,View3D,View3D.PersSize) then begin
      DrawPerspective(false);
   end;
end;


procedure TThreeDview.Moonposition2Click(Sender: TObject);
begin
   {$IfDef ExGeography}
   {$Else}
      ShowMoonPath(true);
   {$EndIf}
end;

procedure TThreeDview.oday1Click(Sender: TObject);
begin
   {$IfDef ExGeography}
   {$Else}
      ShowSunPath(MDDef.SingleJulianDay);
   {$EndIf}
end;


{$IfDef ExGeography}
{$Else}
   procedure TThreeDview.ShowMoonPath(Today : boolean);
   var
      x,y,db : integer;
      az,alt : float64;
      iYear,iMonth,iDay : word;
      Sym : tFullSymbolDeclaration;
      Time : shortstring;
   begin
      Sym.Color := claDarkGrey;
      Sym.Size := 5;
      Sym.DrawingSymbol := FilledCircle;
      (*
      if Today then begin
         imonth := 0;
      end
      else begin
         iMonth := -99;
      end;
      *)

      if Today then DecodeDate(Now,iYear,iMonth,iDay)
      else begin
         GetDate(UsersFavoriteMonth,UsersFavoriteDay,UsersFavoriteYear);
         iMonth := UsersFavoriteMonth;
         iDay := UsersFavoriteDay;
         iYear := UsersFavoriteYear;
      end;

      db := MoonPositionDB(View3D.ViewerLat,View3D.ViewerLong, iyear,imonth,iday);

      GISdb[db].MyData.First;
      while not GISdb[db].MyData.eof do begin
         Alt := GISdb[db].MyData.GetFieldByNameAsFloat('ALTITUDE');
         if (Alt > 0) then begin
            Az := GISdb[db].MyData.GetFieldByNameAsFloat('AZIMUTH');
            Time := GISdb[db].MyData.GetFieldByNameAsString('TIME');
            SymbolAtPitchAzimith(alt,az,Sym,true);
            View3D.AzimuthToScreen(Az,x);
            View3D.PitchToScreen(Alt,y);
            Image1.Canvas.TextOut(x,y,Time);
         end;
         GISdb[db].MyData.Next;
      end;
   end;


   procedure TThreeDview.ShowSunPath(Day : integer);
   var
      hrtime,tz : float64;
      x,y : integer;
      az,alt,sunrise,sunset : float64;
      Sym : tFullSymbolDeclaration;
      TStr : shortstring;
   begin
      {$IfDef RecordSunPath} WriteLineToDebugFile('TThreeDview.ShowSunPath, day=' + IntToStr(Day)); {$EndIf}
      tz := round(View3D.ViewerLong / 15);
      if MDDef.VerifyTimeZone then ReadDefault('Time zone', tz);
      Sym.Color := claYellow;
      Sym.Size := 8;
      Sym.DrawingSymbol := FilledCircle;
      Image1.Canvas.Font.Size := 15;
      hrtime := 0;
      repeat
         Sun_Position.ComputeSunPosition(View3D.ViewerLat,View3D.ViewerLong, hrtime,tz,Day,az,alt,sunrise,sunset);
         hrtime := hrtime + 30 / 60;
         if (Alt > 0) then begin
             SymbolAtPitchAzimith(alt,az,Sym,true);
             View3D.AzimuthToScreen(Az,x);
             View3D.PitchToScreen(Alt,y);
             TStr := HoursMinutesString(hrtime);
             Image1.Canvas.TextOut(x,y,TStr);
             {$IfDef RecordSunPath} WriteLineToDebugFile('hour=' + RealToString(hrtime,-12,-2) + ' az=' + RealToString(az,-12,-2) + 'alt=' + RealToString(alt,-12,-2) ); {$EndIf}
         end;
      until (hrTime > 24);
   end;
{$EndIf}


procedure TThreeDview.Modify1Click(Sender: TObject);
var
   Redraw : boolean;
   i      : integer;
begin
   {$IfDef RecordTrackObserverElevation} TrackObserverElevation('Modify 3D view in'); {$EndIf}
   Redraw := false;
   ImageDrawn := false;

   if (Sender = SpeedButton21) or (Sender = SpeedButton22) then begin
      with View3D.PersOpts do begin
         if (Sender = SpeedButton21) then begin
            if (View3D.PersSize = psPanorama) then begin
              PanWidth := 2 * PanWidth;
              PanHeight := 2 * PanHeight;
            end
            else begin
               PersWidth := 2 * PersWidth;
               PersHeight := 2 * PersHeight;
            end;
         end
         else begin
            if View3D.PersSize = psPanorama then begin
              PanWidth := PanWidth div 2;
              PanHeight := PanHeight div 2;
            end
            else begin
               PersWidth := PersWidth div 2;
               PersHeight := PersHeight div 2;
            end;
         end;
         Position := poMainFormCenter;
         DrawPerspective(false);
      end;
   end
   else begin
      ModifyWithoutRedraw(Redraw);
      if (View3D.MainInCh = 'P') and Redraw then begin
         if (View3D.DrapeMapUsing <> 0) and (View3D.DrapedBMP[View3D.DrapeMapUsing] <> Nil) then begin
            ImageDrawn := true;
            CopyImageToBitmap(View3D.DrapingMaps[View3D.DrapeMapUsing].Image1,View3D.DrapedBMP[View3D.DrapeMapUsing]);
            for i := 0 to pred(View3D.DrapedBMP[View3D.DrapeMapUsing].Height) do View3D.P0[View3D.DrapeMapUsing][i] := View3D.DrapedBMP[View3D.DrapeMapUsing].ScanLine[i];
         end;
         FormResize(Sender);
      end;
   end;

   if DrawPointOnRedraw  then begin
      SymbolAtPitchAzimith(dporPitch,dporAzimuth,dporSym);
   end;

   if MDDef.AutoLabelGaz and (View3D.PersSize in [psPerpsective,psPanorama]) then SpeedButton9Click(nil);
   ImageDrawn := true;
   {$IfDef RecordTrackObserverElevation} TrackObserverElevation('Modify 3D view out'); {$EndIf}
end;


procedure TThreeDview.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
   if not Closable then begin
      {$IfDef ExFly}
      {$Else}
         if (FlightControlForm <> Nil) then FlightControlForm.Button1Click(Nil);
      {$EndIf}
      CanClose := false;
   end
   else CanClose := true;
end;


function TThreeDview.LocatePointOnPerspective(Azimuth,Pitch : float64; var Lat,Long,DistOut : float64; var PointMess1,PLSSMess2 : shortstring; DoPLSS : boolean = false; DeleteRadial : boolean = true) : boolean;
label
   FoundIt;
var
   ThisPitch,Elev1,xr,yr : float32;
   FirstPt,Pt : integer;
   elevs,fxgrids,fygrids,fdists : ^bfarray32;

begin
   if (DEMGlb[View3D.DEMOnView] = nil) or (not ImageDrawn) then exit;
   {$If Defined(RecordLocatePointOnPersp) or Defined(RecordFindLatLong)}
      WriteLineToDebugFile('TThreeDview.LocatePointOnPerspective in');
      WriteLineToDebugFile('LocatePointOnPerspective from ' + LatLongDegreeToString(ViewerLat,ViewerLong) + ' obs at ' + RealToString(View3D.ObsElev,8,1) + ' az=' + RealToString(Azimuth,8,2) + ' pitch=' + RealToString(Pitch,8,2));
   {$EndIf}
   Lat := 0;
   Long := 0;
   PointMess1 := '';
   PLSSMess2 := '';
   Result := false;
   VincentyPointAtDistanceBearing(View3D.ViewerLat,View3D.ViewerLong,View3D.ViewDepth,Azimuth,View3D.ViewedLat,View3D.ViewedLong);

   new(fxgrids);
   new(fygrids);
   new(fdists);
   new(elevs);
   DEMGlb[View3D.DEMonView].GetStraightRouteLatLongWithElevs(View3D.ViewerLat,View3D.ViewerLong,View3D.ViewedLat,View3D.ViewedLong,View3D.NumRadialPts,fxgrids^,fygrids^,fdists^,elevs^);
   Pitch := TanDeg(Pitch);
   FirstPt := 0;
   while (fDists^[FirstPt] < View3D.PersOpts.PersFirstProfile) do inc(FirstPt);
   for Pt := FirstPt to View3D.NumRadialPts do begin
      DistOut := fDists^[Pt];
      xr := fXGrids^[Pt];
      yr := fYGrids^[Pt];
      if DEMGlb[View3D.DEMonView].GetElevMeters(xr,yr,Elev1) then begin
         ThisPitch := ( -(View3D.ObsElev - (Elev1 - DropEarthCurve(DistOut))) / DistOut);
         if (ThisPitch >= Pitch) then begin
            DEMGlb[View3D.DEMOnView].DEMGridToLatLongDegree(xr,yr,Lat,Long);
            PointMess1 := DEMGlb[View3D.DEMOnView].DEMLocationString(xr,yr) + '   z=' + RealToString(Elev1,-8,1) + ' m';
            if MDDef.DualElevs and DEMGlb[View3D.DEMOnView].ElevationDEM then PointMess1 := PointMess1 + '  (' + RealToString(Elev1/Petmar_types.FeetToMeters,-5,0) + ' ft)';
            {$If Defined(RecordLocatePointOnPersp) or Defined(RecordFindLatLong)} WriteLineToDebugFile(PointMess1); {$EndIf}
            {$IfDef ExPLSS} {$Else} if DoPLSS and (MDDef.ShowPLSS) then PLSSMess2 := PLSSLocation(Lat,Long); {$EndIf}
            Result := true;
            goto FoundIt;
         end {if ThisPitch};
      end {if elev1};
   end {for pt};
FoundIt:;
   Dispose(fxgrids);
   Dispose(fygrids);
   Dispose(fdists);
   Dispose(elevs);
   //if DeleteRadial then View3D.ClearGrids;
   {$IfDef RecordLocatePointOnPersp} WriteLineToDebugFile('TThreeDview.LocatePointOnPerspective out'); {$EndIf}
end;


var
   ErrorString : string;


function TThreeDview.FindLatLong(var Lat,Long : float64; var Pitch : float32; var DistOut : float64; var Azimuth : float32;
   var Mess1,Mess2,Mess3,Mess4 : shortString; DoPLSS : boolean = false) : boolean;
var
   lx,ly,i : integer;


   function NeighborPixel(xp,yp : integer) : shortString;
   var
      az, pit : float32;
      Dist,Heading,Lat2,Long2 : float64;
      Mes1,Mes2 : shortstring;
   begin
      with View3D,DEMGlb[DEMOnView] do begin
         ScreenToAzimuthPitch(Xp,Yp,Az,Pit);
         {$IfDef RecordFindLatLongError} WriteLineToDebugFile('x=' + intToStr(xp) + '  y=' + IntToStr(yp) + ' az=' + RealToString(Az,-12,-2) + '  pitch=' + RealToString(Pit,-12,-2)); {$EndIf}
         if LocatePointOnPerspective(Az,Pit,Lat2,Long2,Dist,Mes1,Mes2,false) then begin
            VincentyCalculateDistanceBearing(Lat,Long,Lat2,Long2,Dist,Heading);
            Result := SmartDistanceMetersFormat(Dist);
         end
         else Result := '';
         Result := FormatString(Result,12,RightJustify);
      end;
   end;

begin
   {$IfDef RecordFindLatLong} WriteLineToDebugFile('TThreeDview.FindLatLong in'); {$EndIf}
   Result := false;
   Mess1 := '';
   Mess2 := '';
   Mess3 := '';
   Mess4 := '';
   for i := 1 to 3 do wmdem.StatusBar1.Panels[i].Text := '';

   with View3D,DEMGlb[DEMOnView] do begin
      LX := LastX;
      LY := LastY;
      ScreenToAzimuthPitch(LastX,LastY,Azimuth,Pitch);
      {$IfDef RecordFindLatLon} WriteLineToDebugFile('x=' + intToStr(Lastx) + '  y=' + IntToStr(Lasty) + '   az=' + RealToString(Azimuth,-12,-2) + '  pitch=' + RealToString(Pitch,-12,-2),true); {$EndIf}
      Mess3 := 'Az: ' + ConvertToDegreesString(Azimuth,MDDef.OutputAzimuthMethod,false) + '  Pitch: ' + ConvertToDegreesString(Pitch,MDDef.OutputPitchMethod,false);
      if LocatePointOnPerspective(Azimuth,Pitch,Lat,Long,DistOut,Mess1,Mess4,DoPLSS) then begin
         Mess2 := 'Range: ' + SmartDistanceMetersFormat(DistOut);
         if (MDDef.ShowRoamOnAllMaps) {$IfDef RegisterPhoto} and (ImageDoingWhat <> DigitizeOnPhoto) {$EndIf} then
               BroadcastLatLong(Self.Handle,Lat,Long);
         {$IfDef RecordFindLatLong} WriteLineToDebugFile(TimeToStr(Now) + '  FindLatLong from ' + '  x=' + intToStr(Lastx) + '  & y=' + IntToStr(LastY) + '  ' + Mess1 + '   ' + Mess3); {$EndIf}
         wmdem.StatusBar1.Panels[1].Text := Mess1;
         wmdem.StatusBar1.Panels[2].Text := Mess2 + '   ' + Mess3;
         Result := true;
         if NeedErrorString then begin
            ErrorString := MessLineBreak + 'Point selection sensitivity' + MessLineBreak +
                 NeighborPixel(pred(LX),Pred(LY)) + NeighborPixel(LX,Pred(LY)) + NeighborPixel(succ(LX),Pred(LY)) +  MessLineBreak +
                 NeighborPixel(pred(LX),LY) + '            '  + NeighborPixel(succ(LX),LY) +  MessLineBreak +
                 NeighborPixel(pred(LX),succ(LY))  + NeighborPixel(LX,succ(LY)) + NeighborPixel(succ(LX),succ(LY)) +  MessLineBreak ;
         end
         else ErrorString := '';
      end
      else wmdem.StatusBar1.Panels[2].Text := Mess3;
   end;
end;


procedure TThreeDview.RadialStatistics;
var
   Mess2,Mess3,Mess4,MessZ : ShortString;
   Lat,Long,DistOut: float64;
   Pitch,Azimuth : float32;
   SaveLastY,i : integer;
   Results : tStringList;
begin
   SaveLastY := LastY;
   Results := tStringList.Create;
   Results.Add('Row    Pitch     Range');
   Results.Add('=========================');
   for i := 0 to pred(Image1.Height) do begin
     LastY := i;
     if FindLatLong(Lat,Long,Pitch,DistOut,Azimuth,MessZ,Mess2,Mess3,Mess4,(not PersPitchAzimuthDigitize)) then with Image1.Canvas do begin
        Results.Add(IntegerToString(i,4) + RealToString(Pitch,8,2) + RealToString(DistOut,12,2));
     end;
   end;
   Petmar.DisplayAndPurgeStringList(Results,'Radial at aziumth ' + RealToString(Azimuth,-12,1));
   LastY := SaveLastY;
end;


procedure TThreeDview.Image1DblClick(Sender: TObject);
var
   Mess1,Mess2,Mess3,Mess4,MessZ : ShortString;
   Lat,Long,DistOut: float64;
   Pitch,Azimuth : float32;
begin
   with View3D,DEMGlb[DEMOnView],SelectionMap.MapDraw do if (MainInCh in ['P']) then begin
      if GetRadial then begin
         RadialStatistics;
         GetRadial := false;
         exit;
      end;

      NeedErrorString := true;
      if FindLatLong(Lat,Long,Pitch,DistOut,Azimuth,MessZ,Mess2,Mess3,Mess4,(not PersPitchAzimuthDigitize)) then {with Image1.Canvas do} begin
         {$IfDef RegisterPhoto}
            if PersPitchAzimuthDigitize then begin
               RegPhotoForm.PostPoint(Lat,Long,DistOut,Azimuth,Pitch);
               Petmar.ScreenSymbol(Image1.Canvas,LastX,LastY,Cross,5,ConvertTColorToPlatformColor(clRed));
               Image1.Canvas.Font.Color := clRed;
               Image1.Canvas.TextOut(LastX+5,LastY+5,IntToStr(RegPhotoForm.PointDoing));
               exit;
            end;
         {$EndIf}
         Image1.Canvas.Pen.Mode := pmNotXor;
         ScreenSymbol(Image1.Canvas,LastX,LastY,Splat,3,ConvertTColorToPlatformColor(clRed));
         SelectionMap.Image1.Canvas.Pen.Mode := pmNotXor;
         Mess1 := PrimMapProj.h_DatumCode + '   ' + LatLongDegreeToString(Lat,Long,MDDef.OutPutLatLongMethod) + MessLineBreak +
               PrimMapProj.UTMStringFromLatLongDegree(Lat,Long) + MessLineBreak+ '  ' + MessLineBreak;
         MessageToContinue(Mess1 + MessLineBreak + MessZ + MessLineBreak + MessLineBreak + Mess2 + '  ' + Mess3 + MessLineBreak + '  ' + MessLineBreak + Mess4 + ErrorString,true);
         ScreenSymbol(Image1.Canvas,Lastx,Lasty,Splat,3,ConvertTColorToPlatformColor(clRed));
         Image1.Canvas.Pen.Mode := pmCopy;
         SelectionMap.Image1.Canvas.Pen.Mode := pmCopy;
         {$IfDef RegisterPhoto}
         Nevadia_Main.PhotoForm.ImageMouseIsDown := false;
         {$EndIf}
      end;
      NeedErrorString := false;
   end;
end;


procedure TThreeDview.Image1MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
var
   Mess1,Mess2,Mess3,Mess4 : ShortString;
   Lat,Long,DistOut: float64;
   Pitch,Azimuth : float32;
begin
   if (View3D <> Nil) and (View3d.MainInCh in ['P']) then begin
      {$IfDef RecordFindLatLong} WriteLineToDebugFile('TThreeDview.Image1MouseMove in'); {$EndIf}
      LastX := x;
      LastY := y;
      FindLatLong(Lat,Long,Pitch,DistOut,Azimuth,Mess1,Mess2,Mess3,Mess4);

      {$IfDef RegisterPhoto}
         if (PhotoRegForm <> Nil) and (PhotoForm <> Nil) then begin
            PhotoForm.Image1.Canvas.Pen.Color := clRed;
            PhotoForm.Image1.Canvas.Pen.Mode := pmNotXor;
            PhotoForm.Image1.Canvas.Pen.Width := 3;
            if (PetImage.LastPhotoRoamX > -1) then CrossWithHole(PhotoForm.Image1.Canvas,PetImage.LastPhotoRoamX,PetImage.LastPhotoRoamY);
            CrossWithHole(PhotoForm.Image1.Canvas,x,y);
            PetImage.LastPhotoRoamX := x;
            PetImage.LastPhotoRoamY := y;
        end;
     {$EndIf}

      {$IfDef RecordFindLatLong} WriteLineToDebugFile('TThreeDview.Image1MouseMove out'); {$EndIf}
   end;
   if MouseIsDown and (DEMNowDoing = JustWandering) then begin
      ScrollBox1.HorzScrollBar.Position := ScrollBox1.HorzScrollBar.Position + SX - X;
      ScrollBox1.VertScrollBar.Position := ScrollBox1.VertScrollBar.Position + SY - Y;
      exit;
   end;
end;


procedure TThreeDview.Image1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   MouseIsDown := false;
end;

procedure TThreeDview.FormClose(Sender: TObject; var Action: TCloseAction);
var
   fName,fName2 : PathStr;
begin
   {$IfDef RecordClosing} WriteLineToDebugFile('TThreeDview.FormClose in');{$EndIf}
   Action := caFree;
   //with View3D do begin
      View3d.CloseDrapingMaps;
      if (View3d.FlightRouteDB <> Nil) then begin
         fName := '';
         if LiveFlying and MDDef.SaveLiveFlying and AnswerIsYes('Save flight route') then begin
            fName2 := View3d.FlightRouteDB.TableName;
            fName := fName2;
            Petmar.GetFileNameDefaultExt('Saved flight route',DefaultDBMask,fName);
         end;
         View3d.FlightRouteDB.Destroy;
         if (fName <> '') then Petmar.CopyFile(fName2,fName);
      end;
      if (CrossTrackProfile <> Nil) then CrossTrackProfile.Close;
      if (AlongTrackGlide <> Nil) then AlongTrackGlide.Close;
      if (PositionMap <> Nil) then begin
         {$IfDef RecordClosing} WriteLineToDebugFile('TThreeDview.FormClose, position map'); {$EndIf}
         PositionMap.Closable := true;
         PositionMap.Close;
         PositionMap := Nil;
      end;
      View3d.Destroy;
   //end;
   BaseGraf.CreateSmallGraph := false;
   {$IfDef RecordClosing} WriteLineToDebugFile('TThreeDview.FormClose out'); {$EndIf}
end;


procedure TThreeDview.Image1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   if (Button = mbRight) then begin
      PopupMenu1.Popup(Mouse.CursorPos.X,Mouse.CursorPos.Y);
   end;

   if (Button = mbLeft) then begin
      SX := X;  // X start co-ordinate, image panning
      SY := Y;  // Y start co-ordinate, image panning
      MouseIsDown := true;
      Forms.Screen.Cursor := crHandPoint;
      exit;
   end;
end;


procedure TThreeDview.SpeedButton20Click(Sender: TObject);
var
   y : integer;  //this really is used despite what compiler says
begin
   Image1.Canvas.Pen.Color := clGreen;
   Image1.Canvas.Pen.Width := 1;
   View3D.PitchToScreen(View3D.MinPitch,y);
   Image1.Canvas.MoveTo(0,y);
   Image1.Canvas.LineTo(pred(Image1.Width),y);
   View3D.PitchToScreen(View3D.MaxPitch,y);
   Image1.Canvas.MoveTo(0,y);
   Image1.Canvas.LineTo(pred(Image1.Width),y);
end;

procedure TThreeDview.SpeedButton21Click(Sender: TObject);
begin
   Modify1Click(Sender);
end;

procedure TThreeDview.SpeedButton22Click(Sender: TObject);
begin
   Modify1Click(Sender);
end;

procedure TThreeDview.SpeedButton2Click(Sender: TObject);
var
   Bitmap : tMyBitmap;
begin
   if (Panel1.Height > 0) then begin
      Bitmap := BlendBitmaps(OverlayOpaqueBMP,PersViewBaseBMP,TrackBar1.Position/255);
      SaveBitmap(Bitmap);
      Bitmap.Free;
   end
   else Saveimage1Click(Sender);
end;


procedure TThreeDview.SpeedButton4Click(Sender: TObject);
begin
   Modify1Click(Sender);
end;

procedure TThreeDview.SpeedButton5Click(Sender: TObject);
begin
   AssignImageToClipBoard(Image1);
end;

procedure TThreeDview.SpeedButton6Click(Sender: TObject);
var
   Ratio : float64;
begin
   SaveTop := Top;
   SaveLeft := Left;
   SaveWidth := View3D.ViewportWidth;
   SaveHeight := View3D.ViewportHeight;
   Top := 5;
   Left := 5;
   ResizingNow := false;
   Ratio := View3D.ViewportWidth / View3D.ViewportHeight;

   View3D.ViewportWidth := wmDEM.ClientWidth - 80;
   if round(View3D.ViewportWidth / Ratio) < wmDEM.ClientHeight - 120 then begin
      View3D.ViewportHeight := round(View3D.ViewportWidth / Ratio);
   end
   else begin
      View3D.ViewportHeight := wmDEM.ClientHeight - 120;
      View3D.ViewportWidth := round(Ratio * View3D.ViewportHeight);
   end;
   SetPerspectiveWindowSize;
   PerspectiveView;
   SpeedButton6.Enabled := false;
   SpeedButton7.Enabled := true;
end;

procedure TThreeDview.SpeedButton7Click(Sender: TObject);
begin
   View3D.ViewportWidth := SaveWidth;
   View3D.ViewportHeight := SaveHeight;
   SetPerspectiveWindowSize;
   PerspectiveView;
   Top:= SaveTop;
   Left := SaveLeft;
   SpeedButton7.Enabled := false;
   SpeedButton6.Enabled := true;
end;

procedure TThreeDview.FormKeyPress(Sender: TObject; var Key: Char);
begin
   {$IfDef ExFly}
   {$Else}
      if LiveFlying then FlightControlForm.FormKeyPress(Sender,Key);
   {$EndIf}
end;


procedure TThreeDview.Showthedrapemap1Click(Sender: TObject);
begin
   View3D.DrapingMaps[View3D.DrapeMapUsing].Visible := true;
   View3D.DrapingMaps[View3D.DrapeMapUsing].SetFocus;
end;


procedure TThreeDview.Imagescaling1Click(Sender: TObject);
var
   MenuStr : ShortString;
   s1,s2,s3 : string35;
   j : integer;
   xgl,ygl,xgr,ygr,xg1,yg1,xg2,yg2,dxg1,dyg1,dxg2,dyg2,p,dist : float64;
   Results : tStringList;
begin
   Results := tStringList.Create;
    if (View3d.DrapeMapUsing > 0) and (View3d.DrapingMaps[View3d.DrapeMapUsing] <> Nil) then begin
      Results.Add('Spacing along the radial: ' + RealToString(View3d.RadialPointSpacing[View3d.DrapeMapUsing],-12,1) + ' m');
      Results.Add(' DEM points per calculation: ' + RealToString(View3d.RadialPointSpacing[View3d.DrapeMapUsing] /
          DEMGlb[View3d.DrapingMaps[View3d.DrapeMapUsing].MapDraw.DEMonMap].AverageYSpace,-8,2) );

      {$IfDef ExSat}
      {$Else}
         if (View3d.DrapingMaps[View3d.DrapeMapUsing].MapDraw.ValidSatOnMap) then
             with SatImage[View3d.DrapingMaps[View3d.DrapeMapUsing].MapDraw.SatOnMap] do
            Results.Add(' Image points per calculation: ' + RealToString(View3d.RadialPointSpacing[View3d.DrapeMapUsing]/MetersPerPixel,-8,2) );
      {$EndIf}
   end;

   Results.Add('');
   Results.Add('Tangential Spacing:');
   Results.Add('');


   if (View3d.DrapeMapUsing <> 0) and (View3d.DrapingMaps[View3d.DrapeMapUsing] <> Nil) then begin
      s1 := '     Image    TimesImage';
      s2 := '   Pixels   Pixels Shown';
      s3 := '========================';
   end;
   Results.Add('  Profile   Distance   Viewport      DEM      Times DEM' + s1);
   Results.Add('              (m)        (m)        pixels   Pixels shown' + s2);
   Results.Add('=========================================================' + s3);

   dxg1 := (View3d.LeftRearGridX - View3d.XGridRight) / View3d.NumPointsOnRay[View3d.DrapeMapUsing];
   dyg1 := (View3d.LeftRearGridY - View3d.YGridRight) / View3d.NumPointsOnRay[View3d.DrapeMapUsing];
   dxg2 := (View3d.RightRearGridX - View3d.XGridRight) / View3d.NumPointsOnRay[View3d.DrapeMapUsing];
   dyg2 := (View3d.RightRearGridY - View3d.YGridRight) / View3d.NumPointsOnRay[View3d.DrapeMapUsing];

   p:= View3d.PersOpts.PersFirstProfile;
   while p <= View3d.ViewDepth do begin
      j := round(p/View3d.ViewDepth * View3d.NumPointsOnRay[View3d.DrapeMapUsing]);
      xg1 := View3d.XGridRight + j * dxg1;
      yg1 := View3d.YGridRight + j * dyg1;
      xg2 := View3d.XGridRight + j * dxg2;
      yg2 := View3d.YGridRight + j * dyg2;
      dist := sqrt(sqr(xg1-xg2) + sqr(yg1-yg2));
      MenuStr := IntegerToString(j,5) + RealToString(p,12,1) + RealToString(dist*DEMGlb[View3d.DEMOnView].AverageSpace,12,1) + RealToString(dist,12,1) +
          RealToString(ClientWidth/Dist,12,1) ;

      {$IfDef ExSat}
      {$Else}
         if (View3d.DrapeMapUsing > 0) and (View3d.DrapingMaps[View3d.DrapeMapUsing] <> Nil) and (View3d.DrapingMaps[View3d.DrapeMapUsing].MapDraw.ValidSatOnMap) then begin
            with SatImage[View3d.DrapingMaps[View3d.DrapeMapUsing].MapDraw.SatOnMap] do begin
               View3d.DrapingMaps[View3d.DrapeMapUsing].DEMGridToImageGrid(xg1,yg1,xgl,ygl);
               View3d.DrapingMaps[View3d.DrapeMapUsing].DEMGridToImageGrid(xg2,yg2,xgr,ygr);
               dist := sqrt(sqr(xgr-xgl) + sqr(ygr-ygl));
               MenuStr := MenuStr + RealToString(dist,12,1) + RealToString(ClientWidth/Dist,12,1) ;
            end;
         end;
      {$EndIf}
      Results.Add(MenuStr);
      if (p < 1000) then p := p + 200
      else if (p < 5000) then p := p + 500
      else if (p < 15000) then p := p + 1000
      else if (p < 50000) then p := p + 5000
      else p := p + 10000;
   end;
   DisplayAndPurgeStringList(Results,'Perspective scaling');
end;

procedure TThreeDview.SpeedButton8Click(Sender: TObject);
var
  i : integer;
  Bitmap : tMyBitmap;
begin
   for i := 1 to 2 do begin
      if (View3D.DrapingMaps[i] <> Nil) then begin
         CopyImageToBitmap(View3D.DrapingMaps[i].Image1,Bitmap);
         PetImage_form.DisplayBitmap(Bitmap,'Image on draping map ' + IntToStr(i));
         FreeAndNil(Bitmap);
      end;
      if View3D.DrapedBMP[i] <> Nil then PetImage_form.DisplayBitmap(View3D.DrapedBMP[i],'Drape BMP ' + IntToStr(i));
   end;
end;


procedure TThreeDview.SpeedButton9Click(Sender: TObject);
label
   NewLine,NoSpace,Restart;
const
   MaxRow = 8;
var
   Table : tMyData;
   xi,yi,len,iRow,i,NumClip : integer;
   Dist,Heading,BlockDist,Pitch,
   Lat,Long,
   Lat1,Long1,Lat2,Long2 : float64;
   z : float32;
   Used : array[1..MaxRow,0..6400] of boolean;
   TStr2,NameStr : ShortString;
   Resorted,Features : tStringList;
   PointsIntervis, USGSGaz : boolean;
   fName,fName2 : PathStr;
   Bitmap,bm2 : tMyBitmap;
   Color : tColor;
   TStr, FeatureName : shortString;
begin
   {$IfDef RecordPerspLabel} WriteLineToDebugFile('TThreeDview.SpeedButton9Click (label peaks)'); {$EndIf}
   Restart:;
   if (not FileExists(LastGazFile)) and (not GetGazFileName(LastGazFile)) then begin
      LastGazFile := '';
      MDDef.AutoLabelGaz := false;
      exit;
   end;

   Table := tMyData.Create(LastGazFile);
   USGSGaz := USGSGazeeteerFile(LastGazFile);

   {$IfDef RecordPerspLabel} WriteLineToDebugFile('Opened: ' + LastGazFile); {$EndIf}

   NumClip := 0;
   ShowHourglassCursor;

   if USGSGaz then NameStr := 'FEATURE' else NameStr := 'NAME';

   with View3D do begin
      for xi := 1 to 4 do for yi := 0 to 6400 do Used[xi,yi] := false;
      VincentyPointAtDistanceBearing(ViewerLat,ViewerLong,ViewDepth,0,Lat2,Long);
      VincentyPointAtDistanceBearing(ViewerLat,ViewerLong,ViewDepth,90,Lat,Long2);
      VincentyPointAtDistanceBearing(ViewerLat,ViewerLong,ViewDepth,180,Lat1,Long);
      VincentyPointAtDistanceBearing(ViewerLat,ViewerLong,ViewDepth,270,Lat,Long1);

      TStr := ') ) and (' + MakePointGeoFilter('LAT','LONG',Lat2,Long1,Lat1,Long2) + ')';

      if USGSGaz then Table.ApplyFilter( '( (CLASS=' + QuotedStr('Summit') + TStr)
      else Table.ApplyFilter( '( (DSG=' + QuotedStr('MT') + TStr);

      if (Table.RecordCount = 0) then begin
         Table.Destroy;
         if AnswerIsYes('No peaks in ' + ExtractFileName(LastGazFile) + '; Try another gazetteer') then begin
            LastGazFile := GazetteerDir;
            goto Restart;
         end
         else begin
            LastGazFile := '';
            MDDef.AutoLabelGaz := false;
            exit;
         end;
      end;
      {$IfDef RecordPerspLabel} WriteLineToDebugFile('LeftAz=' + RealToString(LeftAzimuth,-5,0) + '  View angle=' + RealToString(PersOpts.PersHFOV,-5,0) + ' Peaks checked: ' + IntToStr(Table.RecordCount)); {$EndIf}

      Features := tStringList.Create;
      Features.Sorted := true;

      while not Table.Eof do begin
         Lat := Table.GetFieldByNameAsFloat('LAT');
         Long := Table.GetFieldByNameAsFloat('LONG');

         if MDDef.ShiftGazPeaks then begin
            DEMGlb[DEMonView].LocationOfMaximumZAroundPoint(Lat,Long,Z,MDDef.ShiftGazPeaksDist);
            {$IfDef RecordPerspLabelShift}
               DEMGlb[DEMonView].GetElevFromLatLongDegree(Table.GetFieldByNameAsFloat('LAT'),Table.GetFieldByNameAsFloat('LONG'),z2);
               WriteLineToDebugFile(Table.GetFieldByNameAsString(NameStr),true);
               WriteLineToDebugFile(LatLongDegreeToString(Table.GetFieldByNameAsFloat('LAT'),Table.GetFieldByNameAsFloat('LONG')) + RealToString(z2,12,2));
               WriteLineToDebugFile(LatLongDegreeToString(Lat,Long) + RealToString(z,12,2));
               CalculateDistanceBearing(Table.GetFieldByNameAsFloat('LAT'),Table.GetFieldByNameAsFloat('LONG'),Lat,Long,DEMGlb[DEMOnView].SelectionMap.MapDraw.PrimMapProj,Dist,Heading);
               WriteLineToDebugFile('Moved:' + RealToString(Dist,12,2));
            {$EndIf}
         end;

         PointsIntervis := DEMGlb[DEMonView].LatLongDegreePointsIntervisible(View3D.ViewerLat,View3D.ViewerLong,PersOpts.PersObsUp,Lat,Long,MDDef.GazPeakObsUp,Dist,BlockDist);
         VincentyCalculateDistanceBearing(View3D.ViewerLat,View3D.ViewerLong,Lat,Long,Dist,Heading);

         if (Dist <= ViewDepth) then begin
            FeatureName := Table.GetFieldByNameAsString(NameStr);
            DEMdatabase.AdjustGazFeatureName(FeatureName);
            AzimuthToScreen(Heading,x);
            if (x > 0) and (x < Image1.Width) then begin
                if PointsIntervis then TStr2 := 'Visible' else TStr2 := 'Masked';
                Features.Add(RealToString(0.001*Dist,-12,2) + ',' + RealToString(Heading,-6,1) + ',' + RealToString(Lat,-12,-6) + ',' +  RealToString(Long,-12,-6) + ',' + FeatureName+ ',' + TStr2);
            end;
         end;
         Table.Next;
      end;
      Table.Destroy;

      Resorted := tStringList.Create;
      Resorted.Add('DIST,AZIMUTH,LAT,LONG,NAME,STATUS');

      for i := pred(Features.Count) downto 0 do begin
         TStr := Features.Strings[i];
         Resorted.Add(TStr);
      end;

      fName := MDTempDir + 'gaz_feat.csv';
      Resorted.SaveToFile(fName);
      Resorted.Free;
      Features.Free;
      CSVFileImportToDB(fName);
      CopyImageToBitmap(Image1,Bitmap);
      CopyImageToBitmap(DEMGlb[DEMonView].SelectionMap.Image1,bm2);

      fName2 := ChangeFileExt(fName,DefaultDBExt);
      Table := tMyData.Create(fName2);
      Table.First;
      while not Table.Eof do begin
         FeatureName := Table.GetFieldByNameAsString('NAME');
         Heading := Table.GetFieldByNameAsFloat('AZIMUTH');
         Dist := 1000 * Table.GetFieldByNameAsFloat('DIST');
         Lat := Table.GetFieldByNameAsFloat('LAT');
         Long := Table.GetFieldByNameAsFloat('LONG');
         DEMGlb[DEMonView].GetElevFromLatLongDegree(Lat,Long,z);
         Pitch := arctan((z - ObsElev - DropEarthCurve(Dist)) / Dist) / DegToRad;

         DEMdatabase.AdjustGazFeatureName(FeatureName);
         AzimuthToScreen(Heading,x);
         PitchToScreen(Pitch,y);

         if Table.GetFieldByNameAsString('STATUS') = 'Visible' then Color := clLime else Color := clRed;
         if MDDef.GazMarkPeaksPerspective then Petmar.ScreenSymbol(Bitmap.Canvas,x,y,FilledBox,3,ConvertTColorToPlatformColor(Color));
         if MDDef.GazMarkPeaksPerpsOnMap  then begin
            DEMGlb[DEMonView].SelectionMap.MapDraw.LatLongDegreeToScreen(Lat,Long,xi,yi);
            Petmar.ScreenSymbol(bm2.Canvas,xi,yi,FilledBox,3,ConvertTColorToPlatformColor(Color));
         end;

         if (x > 0) and (x < Image1.Width) then begin
            FeatureName := ' ' + FeatureName + ' ';
            Len := Image1.Canvas.TextWidth(FeatureName);

            x := x - Len div 2;
            if (x < 5) then x := 5;
            if (x > Image1.Width - Len - 5) then x := Image1.Width - Len - 5;
            iRow := 1;
            NewLine:;
            if (iRow > MaxRow) then begin
               inc(NumClip);
               {$IfDef RecordPerspLabelWrite} WriteLineToDebugFile('No room to write ' + FeatureName); {$EndIf}
               goto NoSpace;
            end;
            for i := x to x + Len do begin
               if Used[iRow,i] then begin
                  inc(iRow);
                  {$IfDef RecordPerspLabelWrite} WriteLineToDebugFile('New line'); {$EndIf}
                  goto NewLine;
               end;
            end;
            for i := (x-2) to (x + Len+2) do if (I >=0) and (i <= 6400) then Used[iRow,i] := true;
            Bitmap.Canvas.Brush.Style := bsClear;
            Bitmap.Canvas.Font.Color := Color;
            y := 10 + 15 * Irow;
            Bitmap.Canvas.TextOut(x,y,FeatureName);
            {$IfDef RecordPerspLabelWrite} WriteLineToDebugFile('Written at x=' +IntToStr(x) + '   y=' + IntToStr(y) + ' ' + FeatureName); {$EndIf}
         end;
         NoSpace:;
         Table.Next;
      end;
      Table.Destroy;
      if (NumClip > 0) then MessageToContinue('Could not label ' + IntToStr(NumClip) + ' features');
   end;
   ShowDefaultCursor;
   Image1.Picture.Graphic := Bitmap;
   Bitmap.Free;
   DEMGlb[View3D.DEMonView].SelectionMap.Image1.Picture.Graphic := bm2;
   bm2.Free;
end;


procedure TThreeDview.SpeedButton10Click(Sender: TObject);
begin
   Imagescaling1Click(Sender);
end;


procedure TThreeDview.SpeedButton11Click(Sender: TObject);
begin
   ShowHorizonOnView(Nil);
end;

procedure TThreeDview.SpeedButton13Click(Sender: TObject);
begin
   AssignBitmapToClipBoard(BlendBitmaps(OverlayOpaqueBMP,PersViewBaseBMP,TrackBar1.Position/255));
end;

procedure TThreeDview.SpeedButton14Click(Sender: TObject);
begin
   GetRadial := true;
end;

procedure TThreeDview.SpeedButton19Click(Sender: TObject);
var
   x,y : integer;
   Azimuth,Pitch : float32;
   Distout,Lat,Long : float64;
   Mess1,Mess2 : shortString;
   Bitmap : tMyBitmap;
begin
   StartProgressAbortOption('Locating');
   //View3D.Findfxgrids := Nil;
   //View3D.Findfygrids := Nil;
   //View3D.Findfdists := Nil;
   CopyImageToBitmap(DEMGlb[View3D.DEMonView].SelectionMap.Image1,Bitmap);
   for x := 0 to pred(Image1.Width) do begin
      UpdateProgressBar(x / Image1.Width);
      View3D.ScreenToAzimuth(x,Azimuth);
      for y := pred(Image1.Height) downto 0 do begin
         View3D.ScreenToPitch(y,Pitch);
         if (Pitch >= View3D.MinPitch) and (Pitch <= View3D.MaxPitch) then begin
            if LocatePointOnPerspective(Azimuth,Pitch,Lat,Long,DistOut,Mess1,Mess2,false,false) then begin
               if (DistOut >= View3D.MinRange) and (DistOut <= View3D.MaxRange) then begin
                  DEMGlb[View3D.DEMonView].SelectionMap.MapDraw.MapSymbolAtLatLongDegree(Bitmap.Canvas,Lat,Long,FilledBox,2,claRed);
               end;
            end
            else break;
          end;
       end;
       //View3D.ClearGrids;
       if WantOut then break;
   end;
   DEMGlb[View3D.DEMonView].SelectionMap.Image1.Picture.Graphic := Bitmap;
   Bitmap.Free;
   EndProgress;
end;


procedure TThreeDview.InfoSpeedButton6Click(Sender: TObject);
begin
   if (View3D.MainInCh = 'P') then MessageToContinue(
       'View From: ' + LatLongDegreeToString(View3D.ViewerLat,View3D.ViewerLong,MDDef.OutPutLatLongMethod) + MessLineBreak +
       'Image size:  ' + ImageSize(Image1) + MessLineBreak +
       'HFOV=' + RealToString(View3D.ViewHFOV,-8,2) + '  VFOV=' + RealToString(View3D.ViewVFOV,-8,2) + MessLineBreak +
       'Aspect=' + RealToString(Image1.Width/Image1.Height,-8,4) + ' or ' + RealToString(View3D.ViewHFOV/View3D.ViewVFOV,-8,4) + MessLineBreak +
       'Azimuth: ' + RealToString(View3D.ViewAzimuth,-12,2) + MessLineBreak +
       'Azimuth range: ' +  RealToString(View3D.LeftAzimuth,-12,2) + ' to ' + RealToString(View3D.RightAzimuth,-12,2) + MessLineBreak +
       'Pitch: ' + RealToString(0.5 * (View3D.MinPitch + View3D.MaxPitch),-12,2) + MessLineBreak +
       'Vertical range: ' +  RealToString(View3D.MinPitch,-12,2) + ' to ' + RealToString(View3D.MaxPitch,-12,2),true);
end;


procedure TThreeDview.TrackBar1Change(Sender: TObject);
begin
   if (Panel1.Height > 0) then begin
      Label1.Caption := 'Alpha blend: ' + IntToStr(TrackBar1.Position);
      Image1.Picture.Graphic := BlendBitmaps(OverlayOpaqueBMP,PersViewBaseBMP,TrackBar1.Position/255);
      ApplicationProcessMessages;
   end;
end;

procedure TThreeDview.Pickday1Click(Sender: TObject);
{$IfDef ExGeography}
begin
{$Else}
begin
   GetDate(UsersFavoriteYear,UsersFavoriteMonth,UsersFavoriteDay);
   ShowSunPath(AnnualJulianDay(UsersFavoriteYear,UsersFavoriteMonth,UsersFavoriteDay));
{$EndIf}
end;


procedure TThreeDview.Pickday2Click(Sender: TObject);
begin
   {$IfDef ExGeography}
   {$Else}
      ShowMoonPath(false);
   {$EndIf}
end;

initialization
   {$IfDef MessageStartUpUnit} MessageToContinue('Startup dempersw'); {$EndIf}
   PersPitchAzimuthDigitize := false;
   ResizingNow := false;
   NeedErrorString := false;
   GetRadial := false;
   MovieList := Nil;
finalization
   {$IfDef RecordClosing} WriteLineToDebugFile('Closing DEMPERSW in'); {$EndIf}
end.


