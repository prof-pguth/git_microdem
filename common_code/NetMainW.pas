unit NetMainW;


{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{________________________________________}


{$I nevadia_defines.inc}

{$IfDef RecordProblems}  //normally only defined for debugging specific problems
   //{$Define NetDraw}
   //{$Define TrackVisual}
   //{$Define ContourNet}
   //{$Define SaveNetDrawingSteps}
{$EndIf}


interface

uses
  Windows, SysUtils, Messages, Classes, Graphics, Controls, Forms, Dialogs,Menus,  // ExtCtrls,
  Math,
  Petmar_types,PETMAR,PETMath,DEMDefs, Vcl.ExtCtrls;

const
   HemisphereName : array[tHemisphere] of ShortString = ('Upper','Lower');
   NetName        : array[tNetType] of ShortString    = ('Schmidt','Wulff');
   NetEquality    : array[tNetType] of shortstring    = ('area','angle');

type {used to keep track of what has been plotted}
   tPoleCount = array[0..200,0..200] of LongInt;

type
   tNetDraw = class
   private
    { Private declarations }
      procedure DrawEquatorialGrid;
      procedure DrawVerticalPlane(RadStrike : float32; color : TPlatformColor);
      procedure WhereIsPointOnNet(What : tPoleOrLine; Dip,DipDirect : float32; var xd,yd : integer);
      procedure AdjustForHemisphere(x,y : float64; var xcoord,ycoord : float64);
   public
    Sum : VectorType;
    PlotRad,
    XPlotCent,YPlotCent,
    NumDataPoints : integer;
    NetColor      : tColor;
    ContourBreaks  : array[0..16] of float64;
    NumPlottedPoints    : LongInt;
    PoleCount           : ^tPoleCount;
    Closable,ReallyPlot : Boolean;
    LLcornerText,NetTitle : ShortString;
    NetOffset  : integer;
    MainDiagramSize : integer;
    WorkingBitmap : tMyBitmap;
      function XPlotCoord(xd : integer) : integer; overload;
      function YPlotCoord(yd : integer) : integer; overload;
      function XPlotCoord(xd : float64) : integer; overload;
      function YPlotCoord(yd : float64) : integer; overload;
    procedure NetOutline(Fill : boolean = false);
    procedure DrawNetGrid;
    procedure EraseOutsideNet;
    procedure ZeroPoleCount;
    procedure PlotPointOnNet(What : tPoleOrLine; Dip,DipDirect : float64; Sym : tFullSymbolDeclaration; var xd,yd : integer);
    procedure LabelPointOnNet(Dip,DipDirect : float64; hrtime : integer; Size : integer = 2);
    procedure GreatCircleOnNet(Dip,DipDirect : float64; Width : integer; Color : tPlatformColor);
    procedure NewNet;
    procedure ContourPoles(AutoScale : boolean = true);
    procedure Betadiagram;
    constructor Create;
    destructor Destroy;
   end;


  TNetForm = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    SaveImage1: TMenuItem;
    Zeronet1: TMenuItem;
    Close1: TMenuItem;
    Modify1: TMenuItem;
    Net1: TMenuItem;
    Statistics1: TMenuItem;
    AverageOrientation1: TMenuItem;
    Title1: TMenuItem;
    Adddata1: TMenuItem;
    SaveDialog1: TSaveDialog;
    Contour1: TMenuItem;
    Contourinterval1: TMenuItem;
    Panel1: TPanel;
    Image1: TImage;
    Betadiagram1: TMenuItem;
    PopupMenu1: TPopupMenu;
    Netopitons1: TMenuItem;
    Saveimage2: TMenuItem;
    Plot1: TMenuItem;
    Plane1: TMenuItem;
    Copytoclipboard1: TMenuItem;
    Poletoplane1: TMenuItem;
    Planepolegreatcircle1: TMenuItem;
    Line1: TMenuItem;
    Intersectiontwoplanes1: TMenuItem;
    Enterandplot1: TMenuItem;
    Countourlimits1: TMenuItem;
    N1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Close1Click(Sender: TObject);
    procedure Zeronet1Click(Sender: TObject);
    procedure Net1Click(Sender: TObject);
    procedure AverageOrientation1Click(Sender: TObject);
    procedure Title1Click(Sender: TObject);
    procedure SaveImage1Click(Sender: TObject);
    procedure Contour1Click(Sender: TObject);
    procedure Contourinterval1Click(Sender: TObject);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Betadiagram1Click(Sender: TObject);
    procedure Netopitons1Click(Sender: TObject);
    procedure Saveimage2Click(Sender: TObject);
    procedure Copytoclipboard1Click(Sender: TObject);
    procedure Line1Click(Sender: TObject);
    procedure Intersectiontwoplanes1Click(Sender: TObject);
    procedure Enterandplot1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Countourlimits1Click(Sender: TObject);
  public
    { Public declarations }
    nd : tNetDraw;
    procedure UpDateDisplay;
  end;

var
  NetForm  : TNetForm;

implementation

{$R *.DFM}

uses
   PetImage,
   Petmar_geology,
   NetOpts,
   Net_entry,
   NetConBr,
   Nevadia_Main;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^ Size variables^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{    This affects how memory is allocated.  The large values are primarily    }
{    for large beta diagrams.                                                 }
{    In addition to these major uses of memory, the printer image requires    }
{         48-64 kb, and another 48-64 kb if you want to figure the contour    }
{         lines rather than patterns or bands.                                }
{    The program requires close to 640 kb with no RAM resident junkware.      }
{    Menus in graphics mode will require memory.                              }

const {used to define number of points in RAM; requires 8 bytes per}
   MaximumDataPoints = 5000;
   BasePlotRad =  99;
type
   PlaneDataType    = record
      PlaneDip        : byte;
      PlaneStrike     : int32;
      PlaneDipDir     : int32;
      PlaneSymbol     : tDrawingSymbol;
      PlaneColor      : byte;
      PlaneRepresents : PlotTypes;
   end {record};
   PlaneDataArray   = array[1..MaximumDataPoints] of PlaneDataType;
const
   InputIs        : PlotTypes = aDipAndStrike;
   //DipAndStrike : string16 = 'N45E 23NW';
var
   LabelSubset    : array[PETMAR_types.tDrawingSymbol] of string35 = ('','','','','','','','','','','','','','','');
   AllPlaneData   : ^PlaneDataArray;
   NumSubset      : array[tDrawingSymbol] of integer;



procedure IntersectionTwoPlanes(Strike1,Strike2,Dip1,Dip2,DipDir1,DipDir2 : float32; var DipDirect,IntDip  : float32);
{calculates direction of dip and dip value for the line of intersection between two planes       }
var
   e,x,y,rho,phi : float64;

   function AngularDistance(Angle1,Angle2 : float64) : float64;
   begin
      Result := (Angle1 - Angle2);
      if (Result > 180) then
         if (Angle1 > Angle2)  then Result := 360 + Angle2 - Angle1
         else Result := 360 + Angle1 - Angle2;
   end;


      procedure DoMath;
      begin
         e := SinDeg(DipDir1 - DipDir2);
         x := (CotDeg(Dip1) * SinDeg(DipDir2) - CotDeg(Dip2)*SinDeg(DipDir1)) / e;
         y := (CotDeg(Dip1) * CosDeg(DipDir2) - CotDeg(Dip2)*CosDeg(DipDir1)) / e;
         rho := ArcTan( 1 / sqrt(sqr(x) + sqr(y)) );
         IntDip := rho / DegToRad;
      end;


begin {proc IntersectionTwoPlanes}
   if (abs(Strike1 - Strike2) < 0.001) or (abs(Strike1 - Strike2 -180) < 0.0001) then begin
      {two planes share same strike}
      DipDirect := Strike1;
      IntDip := 0;
   end
   else if (abs(Dip1 - 90) < 0.001) and (abs(Dip2 - 90) < 0.001) then begin  { two planes are vertical }
      DipDirect := 0;
      IntDip := 90;
   end {if}
   else if (abs(Dip1) < 0.001) or (abs(Dip2) < 0.001) then begin    { one plane is horizontal }
      if (abs(Dip1) < 0.001) then DipDirect := DipDir2 + 90
      else DipDirect := DipDir1 + 90;
      DipDirect := Petmath.CompassAngleInRangeFloat32(DipDirect);
      IntDip := 0;
   end
   else if (abs(Dip1 - 90) < 0.001) or (abs(Dip2 - 90) < 0.001)then begin  { one planes is vertical }
      if (abs(Dip1 - 90) < 0.001) then  begin
         DipDirect := Strike1;
         if AngularDistance(DipDirect,DipDir2) > 90 then DipDirect := DipDirect + 180;
      end
      else begin
         DipDirect := Strike2;
         if AngularDistance(DipDirect,DipDir1) > 90 then DipDirect := DipDirect + 180;
      end;
      DipDirect := Petmath.CompassAngleInRangeFloat32(DipDirect);
      DoMath;
   end {if}
   else begin
      DoMath;
      phi := Math.ArcCos(x / sqrt(sqr(x) + sqr(y)) );
      if y < 0 then DipDirect := (phi / Petmar_types.DegToRad) + 180
      else DipDirect := 180 - (phi / Petmar_types.DegToRad);
   end;
end {proc IntersectionTwoPlanes};


procedure GeodataToValues(Geodata : ShortString; var Dip,Strike,DipDir : float32);
var
   Len : integer;
   OK : boolean;
begin
   Len := Length(Geodata);
   if (Len > 10) then Len := 10;
   GeoData := copy(Geodata,1,Len);
   StripDipAndStrike(GeoData,Dip,Strike,DipDir,OK);
end;


procedure TNetForm.Betadiagram1Click(Sender: TObject);
begin
   nd.BetaDiagram;
end;


procedure Rotate(DipBed,DipDirBed,DipAxis,DipDirAxis : float64; var RotatedDip,RotatedDipDir : float64);
{from Fortran program in Parks, J.M., 1970, Geological Society of America Bulletin, vol.81, p.537-540}
{rotates Axis back to horizontal, and returns rotated dip and dip direction of Bed}
var
   DiffDipDir,Doffs,CosDX,CosDN,SinDX,SinDN,CosDF,SinRod,BB    : float64;
begin {proc Rotate}
   DiffDipDir := abs(DipDirBed - DipDirAxis);
   if DiffDipDir = 0 then
      if DipAxis >= DipBed then  begin
         RotatedDipDir := DipDirBed + 180;
         RotatedDip    := DipAxis - DipBed;
      end
      else begin
         RotatedDipDir := DipDirBed;
         RotatedDip    := DipBed - DipAxis;
      end
   else {DiffDipDir > 0}
        if DiffDipDir = 180 then begin
           RotatedDipDir := DipDirAxis;
           RotatedDip := DipBed + DipAxis;
        end
        else begin
           if (DiffDipDir < 180) then Doffs := DiffDipDir
           else Doffs := 360 - DiffDipDir;
           CosDX := cos(DegToRad * DipBed);
           CosDN := cos(DegToRad * DipAxis);
           SinDX := sin(DegToRad * DipBed);
           SinDN := sin(DegToRad * DipAxis);
           CosDF := cos(DegToRad * Doffs);
           RotatedDip := CosDX * CosDN + SinDX * SinDN * CosDF;
           if RotatedDip < 0 then RotatedDip := - RotatedDip;
           SinRod := sqrt(1.0 - sqr(RotatedDip));
           BB := (CosDX - RotatedDip * CosDN) / (SinRod * SinDN);
           if BB > 1.0 then BB := 1.0;
           BB := ArcCos(BB) / DegToRad;
           if RotatedDip < 0.0000001 then RotatedDip := 90
           else RotatedDip := ArcTan(SinRod / RotatedDip) / DegToRad;
           if DiffDipDir > 180 then
              if DipDirAxis > DipDirBed then  RotatedDipDir := DipDirAxis - BB + 180
              else                            RotatedDipDir := DipDirAxis + BB - 180
           else {DiffDipDir < 180}
              if DipDirAxis > DipDirBed then  RotatedDipDir := DipDirAxis + BB - 180
              else                            RotatedDipDir := DipDirAxis - BB + 180;
        end;
   RotatedDipDir := PetMath.CompassAngleInRangeFloat64(RotatedDipDir);
end {proc Rotate};


procedure TNetForm.FormCreate(Sender: TObject);
begin
   nd := tNetDraw.Create;
   Width := 500;
   Height := 500;

   if MDDef.CreateNetHidden then begin
      FormStyle := fsNormal;
      Hide;
   end;
   nd.DrawNetGrid;
   wmDEM.FormPlacementInCorner(self);
   MDDef.CreateNetHidden := false;
   Caption := NetName[MDDef.NetDef.NetUsed] + ' Net   ' +  HemisphereName[MDDef.NetDef.HemisphereUsed] + ' Hemisphere';
end;

procedure TNetForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   nd.Destroy;
   Action := caFree;
   NetForm := Nil;
end;

procedure TNetForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
   CanClose := nd.Closable;
end;

procedure TNetForm.Enterandplot1Click(Sender: TObject);
begin
   MicronetPlot(Self);
end;

procedure TNetForm.Close1Click(Sender: TObject);
begin
   Close;
end;

procedure TNetForm.Zeronet1Click(Sender: TObject);
begin
   nd.NewNet;
end;

procedure TNetForm.Net1Click(Sender: TObject);
begin
   Netopts.MicronetOptions(Self);
   Self.Caption := NetName[MDDef.NetDef.NetUsed] + ' Net   ' + HemisphereName[MDDef.NetDef.HemisphereUsed] + ' Hemisphere';
   nd.NewNet;
   nd.ContourPoles;
end;

procedure TNetForm.AverageOrientation1Click(Sender: TObject);
var
   DipDirect,Dip,
   R,K,alpha95   : float64;
   i,xd,yd             : integer;
   Captions      : array[1..3] of ShortString;
   TStr : ShortString;
begin
   if nd.NumDataPoints = 0 then exit;
   R := sqrt( sqr(nd.Sum[1]) + sqr(nd.Sum[2]) + sqr(nd.Sum[3]) );
   Dip := ArcTan( nd.Sum[3] / sqrt(sqr(nd.Sum[1]) + sqr(nd.Sum[2]) ) ) / DegToRad;
   DipDirect := HeadingOfLine(nd.Sum[2],nd.Sum[1]);
   K := (nd.NumDataPoints - 1) / (nd.NumDataPoints - R);
   Alpha95 := 140 / sqrt(K * nd.NumDataPoints);
   R := R / nd.NumDataPoints;
   nd.PlotPointOnNet(PolePlot,Round(Dip),round(DipDirect),ASymbol(Splat,claLime,2),xd,yd);
   //AzimuthToDirection(round(DipDirect),Dir);
   Captions[3] := 'n=' + IntegerToString(nd.NumDataPoints,8);
   Captions[1] := 'Vector Avg (*):' + RealToString(Dip,5,1) + DegSym + ' toward' + RealToString(DipDirect,6,1) + DegSym + ' ' + AzimuthToDirection(round(DipDirect));
   Captions[2] := 'Alpha95:'+ RealToString(Alpha95,6,1) + DegSym + '  Strength:' + RealToString(R,5,3);
   TStr := '';
   for i := 1 to 3 do TStr := TStr + Captions[i] + MessLineBreak;
   if AnswerIsYes(TStr + ' Label on diagram') then begin
      for i := 1 to 3 do with Image1.Canvas do TextOut(0,15*Pred(i),Captions[i]);
   end;
end;

procedure TNetForm.Title1Click(Sender: TObject);
begin
   with Image1.Canvas do begin
      Font.Color := clWhite;
      TextOut(10,Image1.Height - nd.NetOffset,nd.NetTitle);
   end;
   GetString('Net title',nd.NetTitle,false,ReasonableTextChars);
   Font.Color := clBlack;
   nd.WorkingBitmap.Canvas.TextOut(10,nd.WorkingBitmap.Height - nd.NetOffset,nd.NetTitle);
   UpdateDisplay;
end;

procedure TNetForm.UpDateDisplay;
begin
    ClientWidth := nd.WorkingBitmap.Width;
    ClientHeight := nd.WorkingBitmap.Height + Panel1.Height;
    Image1.Width := nd.WorkingBitmap.Width;
    Image1.Height := nd.WorkingBitmap.Height;
    Image1.Picture.Graphic := nd.WorkingBitmap;
end;

procedure TNetForm.SaveImage1Click(Sender: TObject);
begin
   SaveImageAsBMP(Image1);
end;

procedure TNetForm.Contour1Click(Sender: TObject);
begin
   Nd.ContourPoles;
end;


procedure TNetForm.Contourinterval1Click(Sender: TObject);
//var
   //i,err : integer;
begin
   if MDDef.NetDef.NetContourColors in [GrayScale,Spectrum,Terrain,Rainbow] then begin
      ReadDefault('max concentration',MDDef.NetDef.MaxContourConcentration);
      ReadDefault('min concentration',MDDef.NetDef.MinContourConcentration);
      nd.ContourPoles;
   end;
end;


procedure TNetForm.Image1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   if (Button = mbRight) then PopupMenu1.PopUp(Mouse.CursorPos.X,Mouse.CursorPos.Y);
end;

procedure TNetForm.Intersectiontwoplanes1Click(Sender: TObject);
var
   Dip1,Strike1,DipDir1,
   Dip2,Strike2,DipDir2,
   DipDirect,IntDip : float32;
   DipAndStrike,
   DipAndStrike2 : Shortstring;
begin
   GetStrikeAndDip('Plane 1',DipAndStrike,Dip1,Strike1,DipDir1);
   GetStrikeAndDip('Plane 2',DipAndStrike2,Dip2,Strike2,DipDir2);
   IntersectionTwoPlanes(Strike1,Strike2,Dip1,Dip2,DipDir1,DipDir2,DipDirect,IntDip);
   MessageToContinue('Plane 1: ' + DipAndStrike + MessLineBreak +
                     'Plane 2: ' + DipAndStrike2 + MessLineBreak +
                     'Intersection dips: ' + RealToString(IntDip,-8,-1) + ' toward ' + RealToString(DipDirect,-8,1),true);
end;

procedure TNetForm.Netopitons1Click(Sender: TObject);
begin
   Net1Click(Nil);
end;

procedure TNetForm.Saveimage2Click(Sender: TObject);
begin
   SaveImageAsBMP(Image1);
end;

procedure TNetForm.Copytoclipboard1Click(Sender: TObject);
begin
   AssignImageToClipBoard(Image1);
end;

procedure TNetForm.Countourlimits1Click(Sender: TObject);
begin
   Contourinterval1Click(Sender);
end;

procedure TNetForm.Line1Click(Sender: TObject);
var
   Dip,DipDirect : float64;
   xd,yd : integer;
begin
   DipDirect := 45;
   Dip := 12;
   ReadDefault('Bearing of line',DipDirect);
   ReadDefault('Dip of line',Dip);
   nd.PlotPointOnNet(LinePlot,Dip,DipDirect,ASymbol(FilledBox,claRed,3),xd,yd);
end;




{ tNetDraw }

constructor tNetDraw.Create;
begin
   PlotRad := BasePlotRad;
   NetOffset := 20;
   NetTitle := '';
   WorkingBitmap := Nil;
   ReallyPlot := true;
   Closable := true;
   New(PoleCount);
   New(AllPlaneData);
   NumDataPoints := 0;
   ZeroPoleCount;
   NetColor := clBlack;
   LLcornerText := '';
   NewNet;
end;


destructor tNetDraw.Destroy;
begin
   if (AllPlaneData <> Nil) then begin
      dispose(AllPlaneData);
      AllPlaneData := Nil;
   end;
   Dispose(PoleCount);
   WorkingBitmap.Destroy;
end;


function TNetDraw.XPlotCoord(xd : integer) : integer;
begin
   Result := NetOffset + (MDDef.NetDef.NetScreenMult * xd);
end;

function TNetDraw.YPlotCoord(yd : integer) : integer;
begin
   Result := NetOffset + (MDDef.NetDef.NetScreenMult * yd);
end;

function TNetDraw.XPlotCoord(xd : float64) : integer;
begin
   Result := NetOffset + round(MDDef.NetDef.NetScreenMult * xd);
end;

function TNetDraw.YPlotCoord(yd : float64) : integer;
begin
   Result := NetOffset + round(MDDef.NetDef.NetScreenMult * yd);
end;


procedure TNetDraw.DrawEquatorialGrid;
label
   Bored,Bored2;
var
   Long,Lat,x,y,
   GridInc,
   LastX,LastY     : float64;
   First           : boolean;

         procedure Project(Lat,Long : float64; var x,y : float64);
         var
            K,SinPhi,CosPhi,
            SinL,CosL           : float64;
         begin
            {simplified for Equatorial Aspect}
            CosPhi := cos(Lat);
            SinPhi := sin(Lat);
            CosL := cos(Long);
            SinL := sin(Long);
            case MDDef.NetDef.NetUsed of
               Schmidt : begin {Lambert Azimuthal equal area projection}
                            K := CosPhi * CosL;
                            K := sqrt(2 / (1 + K)) * 0.5 * Sqrt_2;
                            x := K * CosPhi * SinL;
                            y := K * SinPhi;
                         end;
               Wulff   : begin {Stereographic, Snyder, 1987, p.157-158}
                            K := 1.0 + CosPhi * CosL;
                            K := 2.0 * 0.5 / K;
                            x := K * CosPhi * SinL;
                            y := K * SinPhi;
                         end;
            end {case};
         end;


begin
   LastX := -999;
   LastY := -999;
   GridInc := DegToRad * MDDef.NetDef.CircleGridIncrement;

   Long := -HalfPi;
   while (Long <= HalfPi) do begin
      Lat := -HalfPi;
      First := true;
      while (Lat <= HalfPi) do begin
         Project(Lat,Long,x,y);
         if First then First := false
         else begin
            WorkingBitmap.Canvas.MoveTo(XPlotCoord(XPlotCent + PlotRad * x),YPlotCoord(YPlotCent + PlotRad * y));
            WorkingBitmap.Canvas.LineTo(XPlotCoord(XPlotCent + PlotRad * LastX),YPlotCoord(YPlotCent + PlotRad * LastY));
         end;
         LastX := x;
         LastY := y;
         Lat := Lat + DegToRad;
      end;
      Long := Long + GridInc;
      if WantOut then goto Bored;
   end {while};
   Bored:
   Lat := -HalfPi;
   while (Lat <= HalfPi) do begin
      Long := -HalfPi;
      First := true;
      while (Long <= HalfPi) do begin
         Project(Lat,Long,x,y);
         if First then First := false
         else begin
            WorkingBitmap.Canvas.MoveTo(XPlotCoord(XPlotCent + PlotRad * x),YPlotCoord(YPlotCent + PlotRad * y));
            WorkingBitmap.Canvas.LineTo(XPlotCoord(XPlotCent + PlotRad * LastX),YPlotCoord(YPlotCent + PlotRad * LastY));
         end;
         LastX := x;
         LastY := y;
         Long := Long + DegToRad;
      end;
      Lat := Lat + GridInc;
      if WantOut then Exit;
   end {while};
   Bored2:
end;


procedure TNetDraw.DrawVerticalPlane(RadStrike : float32; color : TPlatformColor);
{input angle in radians}
begin
   WorkingBitmap.Canvas.Pen.Color := ConvertPlatformColorToTColor(Color);
   WorkingBitmap.Canvas.MoveTo(XPlotCoord(XPlotCent + PlotRad * cos(RadStrike)),YPlotCoord(YPlotCent - PlotRad * sin(RadStrike)));
   WorkingBitmap.Canvas.LineTo(XPlotCoord(XPlotCent - PlotRad * cos(RadStrike)),YPlotCoord(YPlotCent + PlotRad * sin(RadStrike)));
end;

procedure TNetDraw.AdjustForHemisphere(x,y : float64; var xcoord,ycoord : float64);
begin
   if (MDDef.NetDef.HemisphereUsed = Upper) then begin
      XCoord := XPlotCent - x;
      YCoord := YPlotCent - y;
   end
   else begin
      XCoord := XPlotCent + x;
      YCoord := YPlotCent + y;
   end;
end;


procedure TNetDraw.GreatCircleOnNet(Dip,DipDirect : float64; Width : integer; Color : tPlatformColor);
var
   RadStrike,StrikeDirect : float32;
   r,XCoord,YCoord : float64;

         procedure GreatCircleOnSchmidtNet(Dip,DipDirect: float64);
         var
            LastX,LastY,RadDip,RadStrike,ApparentDip : float32;
            x,y,r,dd  : float64;
            First : boolean;
         begin
            if (Dip > 89.99) then begin
               StrikeDirection(DipDirect,RadStrike);
               RadStrike := CompassAngleToRadians(RadStrike);
               DrawVerticalPlane(RadStrike,color);
            end
            else with MDDef.NetDef do begin
               First := true;
               dd := -90;
               while dd <= 90 do begin
                  StrikeDirection(DipDirect + dd,RadStrike);
                  RadStrike := CompassAngleToRadians(RadStrike);
                  ApparentDip := abs(arctan( tanDeg(Dip) * cosDeg(dd)));
                  RadDip := 90 * DegToRad - ApparentDip;

                  x := NetScreenMult*PlotRad * sin(RadDip) * sin(RadStrike);
                  y := NetScreenMult*PlotRad * sin(RadDip) * cos(RadStrike);
                  if (x=0) and (y=0) then r := 0
                  else r := abs(sqrt_2 * PlotRad * sin(RadDip / 2)) / sqrt(sqr(x)+sqr(y));
                  AdjustForHemisphere(x*r,y*r,XCoord,YCoord);
                  if First then with MDDef.NetDef do begin
                     First := false;
                  end
                  else begin
                     WorkingBitmap.Canvas.MoveTo(XPlotCoord(xcoord), YPlotCoord(ycoord));
                     WorkingBitmap.Canvas.LineTo(XPlotCoord(LastX), YPlotCoord(LastY));
                  end;
                  LastX := XCoord;
                  LastY := YCoord;
                  dd := dd + 0.5;
               end {while i};
            end;
         end {proc GreatCircleOnSchmidtNet};


         procedure GreatCircleOnWulffNet(Dip,DipDirect : float32);
         begin
            StrikeDirection(DipDirect,StrikeDirect);
            RadStrike := StrikeDirect;
            CompassAngleToRadians(RadStrike);
            if (Dip = 90) then DrawVerticalPlane(RadStrike,color)
            else with MDDef.NetDef do begin
               r := PlotRad * tanDeg(Dip);
               AdjustForHemisphere(r * sin(RadStrike),r * cos(RadStrike),Xcoord,Ycoord);
               r := NetScreenMult * (sqrt(sqr(r) + sqr(PlotRad)));
               WorkingBitmap.Canvas.Brush.Style := bsClear;
               WorkingBitmap.Canvas.Ellipse(XPlotCoord(xcoord-r/ 2),YPlotCoord(ycoord-r/ 2),XPlotCoord(xcoord+r/ 2),YPlotCoord(ycoord+r/ 2));
               WorkingBitmap.Canvas.FloodFill(1,1,clBlack,fsBorder);
            end;
         end {proc GreatCircleOnWulffNet};


begin
   WorkingBitmap.Canvas.Pen.Color := ConvertPlatformColorToTColor(Color);
   WorkingBitmap.Canvas.Pen.Width := Width;
   if (MDDef.NetDef.NetUsed = Schmidt) then GreatCircleOnSchmidtNet(Dip,DipDirect)
   else GreatCircleOnWulffNet(Dip,DipDirect);
end {proc GreatCircleOnNet};


procedure tNetDraw.LabelPointOnNet(Dip,DipDirect : float64; hrtime : integer; Size : integer = 2);
var
   xd,yd,xoff   : integer;
   tstr : shortstring;
   Sym : tFullSymbolDeclaration;
begin
   WhereIsPointOnNet(LinePlot,Dip,DipDirect,Xd,yd);
   Sym.Color := claBlack;
   Sym.Size := Size;
   Sym.DrawingSymbol := FilledBox;
   {if rounding, round just the center coordiate and then draw around it}

   //xd := XPlotCoord(xc);
   //yd := NetOffset + round(MDDef.NetDef.NetScreenMult * yc);

   tstr := AddDayMonthLeadingZero(HrTime);
   if (hrtime < 12) then xoff := 5
   else if (Hrtime = 2) then xoff := 0
   else xoff := -5 - WorkingBitmap.Canvas.TextWidth(TStr);

   ScreenSymbol(WorkingBitmap.Canvas,xd,yd,Sym);
   WorkingBitmap.Canvas.TextOut(xd+xoff,yd+5,TStr);
end;


procedure TNetDraw.WhereIsPointOnNet(What : tPoleOrLine; Dip,DipDirect : float32; var xd,yd : integer);
var
   StrikeDirect,RadDip,RadStrike : float32;
   x,y,r,xc,yc  : float64;
begin
   StrikeDirection(DipDirect,StrikeDirect);
   RadStrike := CompassAngleToRadians(StrikeDirect);
   if (What = LinePlot) then RadDip  := (90 - Dip) * DegToRad
   else RadDip := Dip * DegToRad;
   x := PlotRad * sin(RadDip) * sin(RadStrike);
   y := PlotRad * sin(RadDip) * cos(RadStrike);
   r := 0;
   if (x<>0) or (y<>0) then begin
      case MDDef.NetDef.NetUsed of
         Wulff   : r := abs(PlotRad * Math.Tan(0.5 * RadDip)) / sqrt(sqr(x)+sqr(y));
         Schmidt : r := abs(sqrt_2 * PlotRad * sin(0.5 * RadDip)) / sqrt(sqr(x)+sqr(y));
      end {case};
   end;
   XC := x * r;
   YC := y * r;
   if (What = LinePlot) then begin
      AdjustForHemisphere(-xc,-yc,xc,yc);
   end
   else begin
      AdjustForHemisphere(xc,yc,xc,yc);
   end {if};
   xd := round(xc);
   yd := round(yc);
   if (PoleCount <> Nil) and (Xd >= 0) and (xd <= 200) and (Yd >= 0) and (Yd <= 200) then inc(PoleCount^[Xd,Yd]);
   xd := XPlotCoord(xc);
   yd := XPlotCoord(yc);
end;


procedure TNetDraw.PlotPointOnNet(What : tPoleOrLine; Dip,DipDirect : float64; Sym : tFullSymbolDeclaration; var xd,yd : integer);
begin
   WhereIsPointOnNet(What,Dip,DipDirect,Xd,yd);
   if ReallyPlot then ScreenSymbol(WorkingBitmap.Canvas,xd,yd,Sym);
end {proc PlotPointOnNet};


procedure TNetDraw.EraseOutsideNet;
var
   i : integer;
begin
    WorkingBitmap.Canvas.Brush.Color := clWhite;
    WorkingBitmap.Canvas.Pen.Color := clWhite;
    WorkingBitmap.Canvas.Brush.Style := bsClear;
    for i := 2 to 100 do begin
       if (I > 2) then WorkingBitmap.Canvas.Pen.Width := 3 else WorkingBitmap.Canvas.Pen.Width := 1;
       WorkingBitmap.Canvas.Ellipse(XPlotCoord(XPlotCent-PlotRad)-i,YPlotCoord(YPlotCent-PlotRad)-i,XPlotCoord(XPlotCent+PlotRad)+i,YPlotCoord(YPlotCent+PlotRad)+i);
    end;
end;


//{$Define RecordPoints}
//{$Define RecordMakeFilterTime}

type
   tCenteredCircularFilter =  array[-50..50,-50..50] of byte;
   tCircularFilter =  array[0..200,0..200] of byte;


function MakeCircularFilter(Size : integer; var NPts : integer) : tCircularFilter;
//radius 100, with a range 0..200
//this took 0.001 sec to compute
var
   x,y,xc,yc : integer;
begin
   if (Size <= 100) then begin
      {$IfDef RecordMakeFilterTime} WriteLineToDebugFile('Start circular filter');  {$EndIf}
      NPts := 0;
      xc := 100;
      yc := 100;
      for x := 0 to 200 do begin
         for y := 0 to 200 do begin
            if (x < (xc - Size)) or (x > (xc + Size))  or (y < (yc - Size)) or (y > (yc + Size)) then begin
               Result[x,y] := 0;  //outside the corresponding box filter
            end
            else if sqrt(sqr(xc-x) + sqr(yc-y)) > Size then begin
               Result[x,y] := 0;  //in the rounded corners of the corresponding box filter
            end
            else begin
               Result[x,y] := 1;
               inc(NPts);
            end;
         end;
      end;
      {$IfDef RecordMakeFilterTime} WriteLineToDebugFile('End circular filter');  {$EndIf}
   end;
end;


function MakeCenteredCircularFilter(Size : integer; var NPts : integer) : tCenteredCircularFilter;
//max radius=50, in a structure from -50..50
var
   x,y : integer;
begin
   if (Size <= 50) then begin
      NPts := 0;
      for x := -50 to 50 do begin
         for y := -50 to 50 do begin
            if (x < -Size) or (x > Size)  or (y < -Size) or (y > Size) then begin
               Result[x,y] := 0;  //outside the corresponding box filter
            end
            else if sqrt(sqr(x) + sqr(y)) > Size then begin
               Result[x,y] := 0;  //in the rounded corners of the corresponding box filter
            end
            else begin
               Result[x,y] := 1;
               inc(NPts);
            end;
         end;
      end;
      {$IfDef RecordPoints} WriteLineToDebugFile(IntToStr(Size) + ',' + IntToStr(NPts)); {$EndIf}
   end;
end;


{$Define RecordConcentrations}


procedure TNetDraw.ContourPoles(AutoScale : boolean = true);
{contours distribution of points on the net, stored in PoleCount^}
var
   i,x,y,xc,yc,//NumCats,YStart,
   CountPoints,NetPoints,
   xo,yo{,xs,ys}         : integer;
   NetPart            : ^tPoleCount;
   Values : array[0..255] of float64;
   MaxNeighbors       : LongInt;
   Rad2,Alpha,MaxConcentration  : float64;
   NearEdge                : boolean;
   LegBitmap        : tMyBitmap;
   {$IfDef RecordConcentrations} Concentrations : tStringList; {$EndIf}
   //fName : PathStr;
   PCStr : shortstring;
   ContourColorArray : tColors256;
   NetAreaFilter : tCircularFilter;
   NearPointFilter : tCenteredCircularFilter;


     procedure WideEGAPlot(x,y : integer; Num : LongInt);
     {plot 4 adjoining pixels, making spot easier to see}
     var
        i{xs,ys,Colr} : integer;
        Value : float64;
     begin
        with MDDef.NetDef do begin
           Value := 100.0 * Num / NumPlottedPoints;
           if (Value >= MinContourConcentration) {and (Value <= MaxContourConcentration)} then begin
              i := ValidByteRange(round((Value - MinContourConcentration) / (MaxContourConcentration - MinContourConcentration) * 255));
              WorkingBitmap.Canvas.Pixels[NetOffset + round(NetScreenMult * x),NetOffset + round(NetScreenMult * y)] := ContourColorArray[i];
              WorkingBitmap.Canvas.Pixels[NetOffset + pred(round(NetScreenMult * x)),NetOffset + round(NetScreenMult * y)] := ContourColorArray[i];
              WorkingBitmap.Canvas.Pixels[NetOffset + round(NetScreenMult * x),NetOffset + pred(round(NetScreenMult * y))] := ContourColorArray[i];
              WorkingBitmap.Canvas.Pixels[NetOffset + pred(round(NetScreenMult * x)),NetOffset + pred(round(NetScreenMult * y))] := ContourColorArray[i];
           end;
        end;
     end;


     procedure UpDateCounters(x,y,xc,yc : integer);
     begin
         if (xc in [0..200]) and (yc in [0..200]) then begin
            if (NetAreaFilter[xc,yc] = 1) then begin
               {point this side of projection}
               inc(NetPart^[xc,yc], PoleCount^[x,y]);
            end
            else if NearEdge then begin
               inc(NetPart^[xo+xc-x,yo+yc-y],PoleCount^[x,y]);
               {point opposite side of projection}
            end;
         end;
      end {proc UpDateCounters};


begin {proc ContourPoles}
   {$IfDef ContourNet} WriteLineToDebugFile('ContourPoles in'); {$EndIf}
   {$IfDef RecordConcentrations} for i := 1 to 50 do NearPointFilter := MakeCenteredCircularFilter(i,NetPoints); {$EndIf}
   NetAreaFilter := MakeCircularFilter(100,NetPoints);
   NearPointFilter := MakeCenteredCircularFilter(MDDef.NetDef.CountRadius,CountPoints);  //10);
   PCstr := '%/' + RealToString(100 * CountPoints / NetPoints,-8,-1) + '% area';
   New(NetPart);
   FillChar(NetPart^,SizeOf(NetPart^),0);

   for i := 0 to 255 do begin
      if MDDef.NetDef.NetContourColors = GrayScale then ContourColorArray[i] := RGB(i,i,i)
      else if MDDef.NetDef.NetContourColors = Rainbow then ContourColorArray[i] := RainbowColorFunct(i,0,255)
      else if MDDef.NetDef.NetContourColors = Spectrum then ContourColorArray[i] := SpectrumColorFunct(i,0,255)
      else if MDDef.NetDef.NetContourColors = Terrain then ContourColorArray[i] := TerrainTColor(i,0,255);
   end;

   for x := 0 to 200 do begin
      for y := 0 to 200 do
         if (PoleCount^[x,y] > 0) then begin
            if sqrt(sqr(x-XPlotCent)+sqr(y-YPlotCent)) >= PlotRad-10 then begin
               NearEdge := true;
               Rad2 := 2 * PlotRad - sqrt(sqr(x-XPlotCent)+sqr(y-YPlotCent));
               if y = YPlotCent then Alpha := Pi / 2
               else Alpha := ArcTan((x-XPlotCent)/(y-YPlotCent));
               {xo,yo are projection of point on opposite side of plot}
               xo := abs(round(sin(Alpha) * Rad2));
               if x > XPlotCent then xo := XPlotCent - xo
                                else xo := XPlotCent + xo;
               yo := abs(round(cos(Alpha) * Rad2));
               if y > YPlotCent then yo := YPlotCent - yo
                                else yo := YPlotCent + yo;
            end
            else NearEdge := false;
            for xc := -9 to 9 do begin
               for yc := -9 to 9 do begin
                  if (NearPointFilter[xc,yc] = 1) then begin
                     UpdateCounters(x,y,x+xc,y+yc);
                  end;
               end;
            end;
        end {if};
   end {for x};
   {$IfDef ContourNet} WriteLineToDebugFile('Counters finished'); {$EndIf}

   {$IfDef RecordConcentrations} Concentrations := tStringList.Create; {$EndIf}
   MaxNeighbors := 0;
   NumPlottedPoints := 0;
   for x := 0 to 200 do begin
     for y := 0 to 200 do begin
        if PoleCount^[x,y] > 0 then begin
           inc(NumPlottedPoints,PoleCount^[x,y]);
           if NetPart^[x,y] > MaxNeighbors then MaxNeighbors := NetPart^[x,y];
           {$IfDef RecordConcentrations} Concentrations.Add(IntToStr(PoleCount^[x,y])); {$EndIf}
        end;
     end {for x};
   end;
   {$IfDef RecordConcentrations} Concentrations.SaveToFile('c:\temp\concentrations.csv'); {$EndIf}
   {$IfDef ContourNet} WriteLineToDebugFile('Max neighbors=' + IntToStr(MaxNeighbors) + ' total plotted=' + IntToStr(NumPlottedPoints)); {$EndIf}

   if (NumPlottedPoints > 0) then begin;
      MaxConcentration := (100.0 * MaxNeighbors / NumPlottedPoints);
      {$IfDef ContourNet} WriteLineToDebugFile('Max concentration=' + RealToString(MaxConcentration,-12,-4)); {$EndIf}
      x := 8;
      MDDef.NetDef.MaxContourConcentration := MaxConcentration;
      for i := 1 to x do ContourBreaks[i] := MDDef.NetDef.MinContourConcentration + (MDDef.NetDef.MaxContourConcentration - MDDef.NetDef.MinContourConcentration) * i / x;
      for i := succ(x) to 16 do ContourBreaks[i] := -99;

      MDDef.NetDef.NetScreenMult := 2;

      NewNet;
      WorkingBitMap := tMyBitmap.Create;
      MainDiagramSize := 200 * round(MDDef.NetDef.NetScreenMult) + 2 * NetOffset;
      WorkingBitmap.Height := MainDiagramSize;
      WorkingBitmap.Width := MainDiagramSize;

      for i := 0 to 255 do Values[i] := MDDef.NetDef.MinContourConcentration + (i/255) * (MDDef.NetDef.MaxContourConcentration - MDDef.NetDef.MinContourConcentration);
      if false then begin
         LegBitmap := VerticalLegendOnBitmap(ContourColorArray,Values,PCstr);
         WorkingBitmap.Width := WorkingBitmap.Width + 10 + LegBitmap.Width;
      end
      else begin
         LegBitmap := HorizontalLegendOnBitmap(ContourColorArray,Values,PCstr,'');
         WorkingBitmap.Height := WorkingBitmap.Height + LegBitmap.Height;
      end;

      DrawNetGrid;

      {$IfDef ContourNet} WriteLineToDebugFile('Net created:' + BitmapSizeString(WorkingBitmap)); {$EndIf}
      {$IfDef SaveNetDrawingSteps} fName := NextFileNumber(MDtempDir,'before_contour_drawing_','.bmp'); WorkingBitmap.SaveToFile(fName); {$EndIf}
      for x := 0 to 200 do begin
         for y := 0 to 200 do begin
            if (NetPart^[x,y] > 0) then begin
               WideEGAPlot(x,y,NetPart^[x,y]);
            end;
         end {for y};
      end;
      {$IfDef SaveNetDrawingSteps} fName := NextFileNumber(MDtempDir,'after_contour_drawing_','.bmp'); WorkingBitmap.SaveToFile(fName); {$EndIf}
      {$IfDef ContourNet} WriteLineToDebugFile('Contour drawing done'); {$EndIf}

      if false then begin
         WorkingBitmap.Canvas.Draw(WorkingBitmap.Width - LegBitmap.Width,WorkingBitmap.Height - LegBitmap.Height - 10,LegBitmap);
      end
      else begin
         WorkingBitmap.Canvas.Draw(WorkingBitmap.Width - LegBitmap.Width,WorkingBitmap.Height - LegBitmap.Height,LegBitmap);
      end;
      {$IfDef SaveNetDrawingSteps} fName := NextFileNumber(MDtempDir,'after_legend_','.bmp'); WorkingBitmap.SaveToFile(fName); {$EndIf}
      LegBitmap.SaveToFile(MDtempDir + 'net_legend.bmp');
      LegBitmap.Free;
   end;
   Dispose(NetPart);
end {proc ContourPoles};



procedure TNetDraw.NetOutline(Fill : boolean = false);
begin
    WorkingBitmap.Canvas.Pen.Color := clblack;    //NetColor;
    WorkingBitmap.Canvas.Pen.Width := round(MDDef.NetDef.NetScreenMult*2);
    if WorkingBitmap.Canvas.Pen.Width > 3 then WorkingBitmap.Canvas.Pen.Width := 3;
    if (WorkingBitmap.Canvas.Pen.Width < 1) then WorkingBitmap.Canvas.Pen.Width := 1;
    WorkingBitmap.Canvas.Ellipse(XPlotCoord(XPlotCent-PlotRad),YPlotCoord(YPlotCent-PlotRad),XPlotCoord(XPlotCent+PlotRad),YPlotCoord(YPlotCent+PlotRad));
    //if (LLcornerText <> '') then WorkingBitmap.Canvas.TextOut(1,MainDiagramSize - WorkingBitmap.Canvas.TextHeight(LLcornertext)-5 - WorkingBitmap.Canvas.TextHeight(LLcornerText), LLcornerText);
end;


procedure TNetDraw.DrawNetGrid;
var
   Dip,Strike,Rad : Integer;
begin
    with MDDef.NetDef do begin
       WorkingBitmap.Canvas.Font.Color := NetColor;
       XPlotCent := succ(PlotRad);
       YPlotCent := succ(PlotRad);

       Rad := 1;
       WorkingBitmap.Canvas.TextOut(10,WorkingBitmap.Height - NetOffset,NetTitle);
       WorkingBitmap.Canvas.Brush.Color := clNearWhite;
       WorkingBitmap.Canvas.Brush.Style := bsSolid;
       NetOutline(true);
       WorkingBitmap.Canvas.Brush.Color := clWhite;
       WorkingBitmap.Canvas.Brush.Style := bsClear;
       WorkingBitmap.Canvas.Pen.Width := NetLineWidth;
       WorkingBitmap.Canvas.Pen.Color := ConvertPlatformColorToTColor(NetLineColor);

       if MDDef.NetDef.DrawGridCircles = ngEquatorial then DrawEquatorialGrid
       else if MDDef.NetDef.DrawGridCircles = ngPolar then begin
          Dip := 90 - MDDef.NetDef.CircleGridIncrement;
          while (Dip > 0) do begin
             case MDDef.NetDef.NetUsed of
                Schmidt : Rad := round(PlotRad * Sqrt_2 * sin(0.5*Dip*DegToRad));
                Wulff   : Rad := round(PlotRad * TanDeg(0.5 * Dip));
             end;
             WorkingBitmap.Canvas.Ellipse(XPlotCoord(XPlotCent-Rad),YPlotCoord(YPlotCent-Rad),XPlotCoord(XPlotCent+Rad),YPlotCoord(YPlotCent+Rad));
             dec(Dip,MDDef.NetDef.CircleGridIncrement);
          end {while};
       end;
       if MDDef.NetDef.DrawGridCircles in [ngPolar,ngAzimuth] then begin
          Strike := 0;
          while (Strike < 180) do begin
             DrawVerticalPlane((DegToRad * Strike),NetLineColor);
             inc(Strike,MDDef.NetDef.CircleGridIncrement);
          end {while};
       end {if};

       if MDDef.NetDef.DrawGridCircles in [ngPolar,ngEquatorial] then NetOutline;

       {$IfDef NetDraw}
          WriteLineToDebugFile('TNetForm.CreateNet, center x=' + IntToStr(round(NetScreenMult*XPlotCent)) + ' y=' + IntToStr(round(NetScreenMult*YPlotCent)));
       {$EndIf}

       if MDDef.NetDef.NorthTick then begin
          {North tick and label}
          WorkingBitmap.Canvas.Pen.Width := 2;
          WorkingBitmap.Canvas.MoveTo(XPlotCoord(XPlotCent),YPlotCoord(YPlotCent-PlotRad));
          WorkingBitmap.Canvas.LineTo(XPlotCoord(XPlotCent),YPlotCoord(YPlotCent-PlotRad)-10);
          WorkingBitmap.Canvas.Font.Size := 12;
          WorkingBitmap.Canvas.Font.Style := [fsBold];
          WorkingBitmap.Canvas.TextOut(NetOffset + round(NetScreenMult*XPlotCent+5),0,'N');
       end;

       if MDDef.NetDef.CenterTick then begin
         {center tick}
         WorkingBitmap.Canvas.MoveTo(XPlotCoord(XPlotCent),YPlotCoord(YPlotCent)+10);
         WorkingBitmap.Canvas.LineTo(XPlotCoord(XPlotCent),YPlotCoord(YPlotCent)-10);
         WorkingBitmap.Canvas.MoveTo(XPlotCoord(XPlotCent)-10,YPlotCoord(YPlotCent));
         WorkingBitmap.Canvas.LineTo(XPlotCoord(XPlotCent)+10,YPlotCoord(YPlotCent));
       end;
    end;
end;


procedure TNetDraw.NewNet;
var
   i  : integer;
begin
   for i := 1 to 3 do Sum[i] := 0.0;
   NumDataPoints := 0;
   i := round(2 * ((PlotRad * MDDef.NetDef.NetScreenMult) + NetOffset)) + 10;
   if (WorkingBitmap <> Nil) then WorkingBitmap.Destroy;

   CreateBitmap(WorkingBitmap,i, i);
   MainDiagramSize := i;
   DrawNetGrid;
   //Image1.Picture.Graphic := Bitmap;
   {$IfDef NetDraw} WriteLineToDebugFile('TNetForm.NewNet,' +  BitmapSizeString(Bitmap)); {$EndIf}
end {proc NewNet};

{--------------------- Routines to Plot On Net ----------------------}

procedure TNetDraw.ZeroPoleCount;  {zero array that keeps track of plotted points}
var
   Sym : tDrawingSymbol;
   x,y : integer;
begin
   NumPlottedPoints := 0;
   FillChar(NumSubset,SizeOf(NumSubset),0);
   for x := 0 to 200 do
      for y := 0 to 200 do
         PoleCount^[x,y] := 0;
   for Sym := Cross to Dot do LabelSubset[Sym] := '';
end;


procedure TNetDraw.Betadiagram;
var
   Dip1,Strike1,DipDir1,
   Dip2,Strike2,DipDir2,
   Dip,DipDirect : float32;
   i,j,xd,yd       : integer;
   NetData       : tStringList;
   fName : PathStr;
begin
   fName := MainMapData;
   if not GetFileFromDirectory('Structural data','*.NET',FName) then exit;
   NetData := tStringList.Create;
   NetData.LoadFromFile(FName);
   NewNet;
   ZeroPoleCount;
   NumPlottedPoints := 0;
   StartProgress('Beta');
   for i := 0 to pred(NetData.Count) do begin
      if i mod 10 = 0 then UpdateProgressBar(i/NetData.Count);
      GeodataToValues(NetData.Strings[i],Dip1,Strike1,DipDir1);
      for j := succ(i) to pred(NetData.Count) do begin
         GeodataToValues(NetData.Strings[j],Dip2,Strike2,DipDir2);
         IntersectionTwoPlanes(Strike1,Strike2,Dip1,Dip2,DipDir1,DipDir2,DipDirect,Dip);
         PlotPointOnNet(LinePlot,round(Dip),round(DipDirect),ASymbol(Box,claRed,2),xd,yd);
         Inc(NumPlottedPoints);
      end {for j};
   end {for i};
   NetData.Free;
   EndProgress;
   ContourPoles;
end {proc PlotPlaneIntersections};


initialization
   NetForm := Nil;
finalization
   {$IfDef NetDraw} WriteLineToDebugFile('NetDrawProblems active in NetMainW'); {$EndIf}
   {$IfDef ContourNet}   WriteLineToDebugFile('ContourNet active in NetMainW');  {$EndIf}
   {$IfDef TrackVisual}  MessageToContinue('TrackVisual active in NetMainW');  {$EndIf}
end.


