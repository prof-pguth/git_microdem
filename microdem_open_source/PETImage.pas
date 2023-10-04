 unit Petimage;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program       }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2023 Peter L. Guth   }
{____________________________________}


{$I nevadia_defines.inc}

//these should never be defined, but it's not worth removing in case someone wants them
   //{$Define ExExif}
   //{$Define NoPatternFloodFill}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   {$IfDef Debug}
      //{$Define BMPMemInline}
      //{$Define RecordBigBitmap}
      //{$Define RecordImageOverlayProblems}
      //{$Define RecordImageResize}
      //{$Define RecordBlendBitmaps}
      //$Define RecordGetImagePartOfBitmap}
      //{$Define RecordJPEG}
      //{$Define RecordPNG}
      //{$Define RecordDipStrike}
      //{$Define RecordBitmapProblems}
      //{$Define RecordIHSmerges}
      //{$Define RecordRoamOnMapProblems}
      //{$Define CheckBMPMem}
      //{$Define RecordBitmapExt}
  {$Else}
     {$Define BMPMemInline}
  {$EndIf}
{$EndIf}


interface

uses
  System.SysUtils, System.IOUtils,System.UITypes,System.Math.Vectors,System.Types,System.UIConsts,StrUtils,

  {$IfDef VCL}
     VCL.Graphics,
     Windows,   Messages, Classes,  Controls,
     Forms, Dialogs, Menus, ExtCtrls,Printers,ClipBrd,  ExtDlgs, StdCtrls,
     Buttons, ToolWin, ComCtrls,DB,  JPEG,
  {$EndIf}

  {$IfDef NoPatternFloodFill}
  {$Else}
     Zipatone,
  {$EndIf}

  {$IfDef FMX}
     FMX.Graphics,FMX.Types,
  {$EndIf}

  {$IfDef RegisterPhoto}  //unclear if all the code for this is still available, and if it would run
     DEMPersW,
     DEMCoord,
  {$EndIf}

  PETMAR,Petmar_types;

type
   {$IfDef VCL}
   tScreenRGB = array[0..MaxScreenYMax] of PRGB;
   {$EndIf}

   tImageDoingWhat = (imDoingNothing,AddingText,DoingFloodFill,imFirstDistancePoint,imSecondDistancePoint,FirstVerticalRotate,
        {$IfDef RegisterPhoto} PickRegistrationPoint,DigitizeOnPhoto,LocatePoints, {$EndIf}
        SecondVerticalRotate,FirstHorizontalRotate,SecondHorizontalRotate,DraggingBMP,ColorCrossSection,imPickingBox);

   RecolorMethod = (NegativeMethod,NonBlackToWhite,NonWhiteToBlack,PickedColors,NoGrays,LeaveSingle,TurnRed,TurnGreen,TurnBlue,MakeGrayscale,TurnCyan,TurnMagenta,TurnYellow,StretchGrayscale,
                       MakeSubdued,DilateImage,ErodeImage,NoNonGrays,WhiteToNearWhite,SmoothFilterImage, BlueGrayscale,RedGrayscale,GreenGrayscale,TurnHLS_light,TurnHLS_sat,TurnHLS_hue);

type

tBMPMemory = class
public
   BMPHeight,BMPWidth,NumDone,NumToDo : integer;

   {$IfDef VCL}
      p1 : tScreenPRGB;
      CurrentColor : tRGBtriple;
   {$EndIf}

   {$IfDef FMX}
      BitmapData   : TBitmapData;
      inBitmap     : tMyBitmap;
      CurrentColor : tAlphaColor;
   {$EndIf}

   constructor Create(var Bitmap : tMyBitmap);
   destructor Destroy;
   procedure SetPixelColorSize(x,y,Size : integer; Color : tPlatformColor); {$IfDef BMPMemInline} inline; {$EndIf}
   procedure SetPixelColor(x,y : integer;  Color : tPlatformColor); {$IfDef BMPMemInline} inline; {$EndIf}
   procedure SetPixelRGB(x,y : integer;  r,g,b : byte); {$IfDef BMPMemInline} inline; {$EndIf}
   function GetPixelColor(x,y : integer) : tPlatformColor; {$IfDef BMPMemInline} inline; {$EndIf}
   procedure GetPixelRGB(x,y : integer; var r,g,b : byte); {$IfDef BMPMemInline} inline; {$EndIf}
   function RedChannel(x,y : integer) : byte; inline;
   function GreenChannel(x,y : integer) : byte; inline;
   function BlueChannel(x,y : integer) : byte; inline;
   procedure SetRedChannel(x,y : integer; Value : byte);  inline;
   procedure SetGreenChannel(x,y : integer; Value : byte); inline;
   procedure SetBlueChannel(x,y : integer; Value : byte);  inline;
   function OnBitmap(x,y : integer) : boolean; inline;
   function EffectivelyWhite(x,y : integer) : boolean;
   function SameColor(x,y : integer;  Color : tPlatformColor) : boolean; inline;
   procedure ColorizePixelRGB(x,y : integer; Color : tPlatFormColor);   inline;
end;


procedure CreateBitmap(var Bitmap : tMyBitmap; width,height : integer);
procedure CloneBitmap(Bitmap : tMyBitmap; var CloneBitmap : tMyBitmap);
function ValidImageFileExt(Ext : ExtStr) : boolean;
function IsJPEG(Ext : ExtStr) : boolean;
function ValidImageFileName(fName : PathStr) : boolean;
function GraphicsFilters : shortstring;
function AllowedGraphicsFilters : shortstring;

procedure SaveBitmap(Bitmap : tMyBitmap; SaveName : PathStr = '');
function LoadBitmapFromFile(fName : PathStr) : tMyBitmap;
function AnaglyphFromTwoBitmaps(fName1,FName2 : PathStr) : tMyBitmap;

procedure DrawLine(var Bitmap : tMyBitmap; x1,y1,x2,y2 : integer);  inline;
procedure BitmapCrossWithHole(var Bitmap : tMyBitmap; x,y : integer);
procedure BitmapCross(var Bitmap : tMyBitmap; x,y,Size : integer);
procedure BitmapSymbol(var Bitmap : tMyBitmap; x,y : integer; Sym : tDrawingSymbol; size: integer; color : TPlatformColor); overload;
procedure BitmapSymbol(var Bitmap : tMyBitmap; x,y : integer; Symbol : tFullSymbolDeclaration); overload;

procedure BitmapRectangle(var Bitmap : tMyBitmap; x1,y1,x2,y2 : integer);   inline;
procedure BitmapTextOut(var Bitmap : tMyBitmap; x,y : integer; TStr : shortstring); inline;
procedure StrethDrawBitmap1onBitmap2(var inBMP,NewBMP : tMyBitmap); inline;
procedure DrawBitmap1onBitmap2(var inBMP,NewBMP : tMyBitmap); inline;

function RGBtoGrayscale(r,g,b : byte) : byte;

{$IfDef VCL}
   procedure ClearBitmap(BM : tMyBitmap; Color : tColor);
{$EndIf}

{$IfDef FMX}
   procedure ClearBitmap(BM : tMyBitmap; Color : tAlphaColor);
{$EndIf}

procedure PlotOrientedLine(Bitmap : tMyBitmap; x,y : integer; VectorSize, Strike : float64; Color : tPlatformColor; LineWidth : integer = 1; ArrowHead : boolean = false; ReverseArrow : boolean = false; StartArrowFromPoint : boolean = false);
procedure PlotVector(Bitmap : tMyBitmap; xt,yt,xp,yp : integer; Color : tPlatformColor; LineWidth : integer = 1; ArrowHead : boolean = false; PointLabel : shortString = '');

procedure MakeTheBitmapSubdued(var Bitmap : tMyBitmap);
procedure MakeTheBitmapGrayScale(var Bitmap : tMyBitmap);
procedure BitmapRemoveGrays(var Bitmap : tMyBitmap; Limit : byte);
procedure BitmapRemoveNonGrays(var Bitmap : tMyBitmap; Tolerance : byte);
procedure BitmapWhiteToNearWhite(var Bitmap : tMyBitmap);

function IsBitmapMonochrome(var Bitmap : tMyBitmap) : boolean;

procedure FindImagePartOfBitmap(var Bitmap : tMyBitmap; var Left,Right,Top,Bottom : integer);

function StretchBitmapToFill(bmp : tMyBitmap; Width,Height : integer) : tMyBitmap;

function CreateThumbNailBMP(inBMP : tMyBitmap; tnHeight : integer) : tMyBitmap;

function BitmapSize(bmp : tMyBitmap) : shortstring;

procedure GetImagePartOfBitmap(var Bitmap : tMyBitmap; WhiteBoundary : boolean = true; WhiteBoundarySize : integer = 2);
function ExtractPartOfImage(var Image1 : tImage; Left,Right,Top,Bottom : integer) : tMyBitmap;


{$IfDef VCL}
      function ImageSize(image : tImage) : shortstring;
      function FormSize(Form : tForm) : shortstring;
      function FormClientSize(Form : tForm) : shortstring;

      procedure SetRedrawMode(Image1 : tImage);  inline;

      procedure RestoreBigCompositeBitmap(fName : PathStr);

      procedure Drehen90Grad(Bitmap : tMyBitmap);
      procedure Drehen180Grad(Bitmap : tMyBitmap);
      procedure Drehen270Grad(Bitmap : tMyBitmap);

      procedure SaveImageAsThumbnail(Image1 : tImage; FName : PathStr; ThumbNailHeight : integer);
      procedure SaveImageAsBMP(Image1 : tImage; SaveName : PathStr = '');
      procedure LoadBitmapInImage(Image1 : tImage; fName : PathStr);
      procedure CloneImageToBitmap(Image1 : tImage; var CloneBitmap : tMyBitmap; MakeItBlack : boolean = false);
      function CopyImageToBitmap(var Image1 : tImage; var Bitmap : tMyBitmap; ClearWhite : boolean = false) : boolean;
      procedure GrayscaleImage(var Image1 : tImage);

      function CreateProtractor(Double,ShallowAngles : boolean; FormVertExag : float64) : tMyBitmap;
      procedure PlotDipSymbol(Bitmap : tMyBitmap; x,y,VectorSize,Dip,Strike,DipDirect : integer; ThisIs : tStructureType; LabelValues : boolean; PenColor : tPlatformColor; PenWidth : integer );

      procedure AssignBitmapToClipBoard(bm1 : tMyBitmap);
      procedure AssignImageToClipBoard(Image1 : tImage);
      procedure LoadFileToClipboard(fName : PathStr);

      function RotateBitmap(OriginalBitmap : tMyBitmap; Angle : float64; AntiAliasing: Boolean; WhiteBackground : boolean = true) : tMyBitmap;
      procedure RecolorBitmap(var Bitmap : tMyBitmap; Color : tPlatformColor); overload;
      procedure RecolorBitmap(var Bitmap : PathStr; Color : tPlatformColor); overload;
      function SmartTextOut(Bitmap,TextMyBitmap : tMyBitmap; x,y : integer; TStr : shortString) : boolean;
      procedure ConvertMyBitmapToJPEG(var FName : PathStr);

      function BitmapMismatch(src1, src2 : tMyBitmap) : boolean;
      function BlendBitmaps(src1, src2 : tMyBitmap; amount: extended) : tMyBitmap;
      procedure BlendBitmapAtop(var src1 : tMyBitmap; Overlay : tMyBitmap; amount : extended);

      procedure IHSMergePurgeBitmaps(var IntensityResultBitmap1,ColorBitmap2 : tMyBitmap; DarkenWhite : boolean = false);  {takes color from bitmap2, intensity from bitmap1, and replaces bitmap1}
      procedure CreateThumbNail(fname,OutName : PathStr; tnHeight,tnQuality : integer; aLabel : ShortString = '');
      procedure MakeBitmapThumbnail(var inBMP : tMyBitmap; tnHeight : integer);

      procedure MakeBitmapNegative(var BMP : tMyBitmap; RGBTriple : tRGBTriple);
      procedure MakeGraphicsFileNegative(fName : PathStr);

      procedure MakeBitmapGreen(var BMP : tMyBitmap);
      procedure ChangeBitmapBlackWhite(var BMP : tMyBitmap; BlackReplace,WhiteReplace : tRGBTriple);
      procedure BitmapNonWhiteToBlack(var Bitmap : tMyBitmap);
      procedure SkeletonizeBitmap(var Bitmap : tMyBitmap);
      procedure FixQuadOveredge(var Bitmap : tMyBitmap);

      procedure ThreshholdGrayscale(bmp : tMyBitmap; DownToZero,Upto255 : integer);
      procedure GrayscaleNegative(bmp : tMyBitmap);

      procedure MakeBitmapStretchedGrayscale(Bitmap : tMyBitmap);
      procedure DilateTheImage(var Bitmap1 :  tMyBitmap; ImageColor : tColor);
      procedure ErodeTheImage(var Bitmap1 :  tMyBitmap; BackColor : tColor);
      procedure SmoothFilterTheImage(var Bitmap1 :  tMyBitmap);

      procedure MakeThisUpperLeftCornerOfBitmap(var Bitmap : tMyBitmap;x,y : integer);
      procedure MakeThisLowerRightCornerOfBitmap(var Bitmap : tMyBitmap;x,y : integer);
      procedure ReplaceBitmapWithSubset(var Bitmap : tMyBitmap; Left,Right,Top,Bottom : integer);

      procedure FillScanlineAddresses(Bitmap : tMyBitmap; var P3 : tScreenPRGB);
      function GrayShade(Color : TColor; var g : byte) : boolean;

      function GetGraphicsFileName(WhatFor : shortstring; var fName : PathStr) : boolean;
      function GetNewGraphicsFileName(WhatFor : shortstring; var fName : PathStr) : boolean;
      function GetNew3DGraphicsFileName(WhatFor : shortstring; var fName : PathStr) : boolean;
      procedure RenamePhotoJPEGS(PhotoDir : PathStr = ''; NameContains : shortString = '');

      //these functions take the top window, and are designed for the case where the top window is an FMX form in a VCL project.
      //They probably do not currently work in FireMonkey
      function GetScreenCapture(JustWindow : Boolean = false) : TMyBitmap;
      procedure SaveScreenCapture(RightMarginToClip : integer; var fName : PathStr; AskName : boolean = false; JustWindow : boolean = false);
      procedure CopyToWindowToClipBoard(RightMarginToClip : integer);
{$EndIf}


{$IfDef ExMovies}
{$Else}
   procedure MakeMovie(fName : PathStr);
   procedure BlendMovie(TopImage,BottomImage : tMyBitmap);
   procedure MovieFromTwoBitmaps(BMP1,BMP2 : tMyBitmap; BMP3 : tMyBitmap = Nil; BMP4 : tMyBitmap = Nil); overload;
   procedure MovieFromTwoBitmaps(f1,f2 : PathStr); overload;
{$EndIf}

procedure AllGraphsOneImage(NumCols : integer = -99; legendOnRight : boolean = false);


type
   tPalette256 = (p256LasClass,p256Gray,p256Terrain,p256Spectrum,p256RedRamp,p256GreenRamp,p256BlueRamp);

function Palette256Bitmap(Palette256 : tPalette256) : PathStr;
function FullPaletteBitmap : PathStr;
procedure RGBtoXY(r,g,b : byte; var x,y : integer);     inline;
procedure RGBtoXYFloat(r,g,b : byte; var x,y : float64);  inline;
function SetAlphaColor(rgbTriple : tRGBTriple) : TAlphaColor;  inline;
procedure HSIfromRGBTrip(RGB : tPlatformColor; var H,S,I : float32);
function CombineBitmaps(nc : integer; theFiles : tStringList; Capt : shortstring) : tMyBitmap;
procedure PutBitmapInBox(var BMP : tMyBitmap);


{$IfDef NoPatternFloodFill}
{$Else}
   procedure SimpleFillShape(Canvas : Vcl.Graphics.tCanvas; xs,ys,MinX,MinY,MaxX,MaxY : integer; CurrentPattern : tPatternRecord);
{$EndIf}


{$IfDef FMX}
   function RGB(r,g,b : byte) : tcolor;
{$EndIf}

{$IfDef MSWindows}
   procedure ScreenShot(fName : PathStr);
{$EndIf}


var
   LastPhotoRoamX,
   LastPhotoRoamY : integer;

const
   bmpx = 4096;
   bmpy = 4096;

implementation


uses
   {$IfDef ExKML}
   {$Else}
      KML_Creator,
   {$EndIf}

   {$IfDef ExVCL}
      petimage_form,
   {$EndIf}

   {$IfDef ExGraphs}
   {$Else}
      BaseGraf,
   {$EndIf}

   {$IfDef ExGeoTiff}
   {$Else}
      Geotiff,
   {$EndIf}

   {$IfDef VCL}
      Nevadia_Main,
      {$IfDef ExGIF}
      {$Else}
         GIFImg,
      {$EndIf}

      {$IfDef ExPNG}
      {$Else}
         PNGImage,
      {$EndIf}
     PetImage_form,
     PETFont,
   {$EndIf}

  {$IfDef RegisterPhoto}
     Register_Photo,
  {$EndIf}

   {$IfDef ExGIS}
   {$Else}
      Make_tables, DEMDataBase,DataBaseCreate,PetDBUtils,
   {$EndIf}

   {$IfDef ExZipatone}
   {$Else}
      sc_ColLith,
   {$EndIf}

   {$IfDef ExExif}
   {$Else}
      ccr.exif,
   {$EndIf}

   {$IfDef ExPointCloud}
   {$Else}
      Las_Lidar,
   {$EndIf}

   {$IfDef MICRODEM}
      DEMDefs,
   {$EndIf}
   BaseMap,
   PETMath;

type
   trgbArray =  array[0..MaxScreenXMax] of TRGBTriple;
   prgbArray =  ^trgbArray;


{$IfDef VCL}
   {$I petimage_vcl.inc}
{$EndIf}


function ExtractPartOfImage(var Image1 : tImage; Left,Right,Top,Bottom : integer) : tMyBitmap;
var
   bmp : tMyBitmap;
begin
   CopyImageToBitmap(Image1,bmp);
   CreateBitmap(Result,Right - Left, Bottom - Top);
   Result.Canvas.CopyRect(Rect(0,0,Result.Width,Result.Height),bmp.Canvas,Rect(Left,Top,Right,Bottom));
   bmp.Free;
end;


procedure PutBitmapInBox(var BMP : tMyBitmap);
var
   sbmp : tMyBitmap;
begin
   GetImagePartOfBitmap(BMP);
   CloneBitmap(bmp,sbmp);
   sbmp.Canvas.CopyMode := cmSrcAnd;
   sBMP.Canvas.Draw(0,0,bmp);
   BMP.Width := BMP.Width + 10;
   BMP.Height := BMP.Height + 10;
   RecolorBitmap(bmp,claWhite);
   bmp.Canvas.Pen.Color := clBlack;
   bmp.Canvas.Pen.width := 2;

   bmp.Canvas.Rectangle(2,2,BMP.Width-2,BMP.Height-2);
   bmp.Canvas.CopyMode := cmSrcAnd;
   BMP.Canvas.Draw(5,5,sbmp);
   sBMP.Free;
end;



{$IfDef MSWindows}
   procedure ScreenShot(fName : PathStr);
   //http://www.drbob42.com/uk-bug/hood-10.htm
   var
      ScreenDC: HDC;
      ScreenHandle: HWnd;
      ScreenBitmap: VCL.Graphics.TBitmap;
    begin
      ScreenHandle := GetDeskTopWindow;
      ScreenDC := GetDC(ScreenHandle);
      try
        ScreenBitmap := VCL.Graphics.TBitMap.Create;
        try
          ScreenBitmap.Width := Screen.Width;
          ScreenBitmap.Height := Screen.Height;
          BitBlt(ScreenBitmap.Canvas.Handle, 0, 0,Screen.Width, Screen.Height, ScreenDC, 0, 0, SRCCOPY);
          if fName = '' then Clipboard.Assign(ScreenBitmap)
          else ScreenBitmap.SaveToFile(fName);
        finally
          ScreenBitmap.Free
        end
      finally
        ReleaseDC(ScreenHandle, ScreenDC)
      end
    end {ScreenShot};


   procedure LoadFileToClipboard(fName : PathStr);
   var
      bmp : tMyBitmap;
   begin
      bmp := LoadBitmapFromFile(fName);
      AssignBitmapToClipBoard(bmp);
      bmp.Free;
   end;


{$EndIf}

(*
procedure AdjustContrastAndBrightness(ABitmap: TMyBitmap; AContrast,ABrightness: integer);
// AContrast given as a percentage adjustment
var
   Map: array[0..255] of byte;
   Stride,i,w,h,x,y : integer;
   PS, P: Pbyte;

   function Clamp(V: integer): integer;
   begin
      if V < 0 then Result := 0
      else if V>255 then Result := 255
      else Result := V;
   end;

begin
   // First setup map
   for i := 0 to 255 do
   Map[i] := Clamp(round((i - 127) * AContrast / 100) + 127 + ABrightness);
   // Make sure we have 24bit colors
   ABitmap.PixelFormat := pf24bit;
   // Width, Height, Scanstride
   W := ABitmap.Width;
   H := ABitmap.Height;
   if H = 0 then exit;
   Stride := 0;
   if H>= 2 then Stride := integer(ABitmap.ScanLine[1]) - integer(ABitmap.ScanLine[0]);
   // PS points to current scanline
   PS := ABitmap.ScanLine[0];
   for y := 0 to H - 1 do begin
      P := PS;
      for x := 0 to W * 3 - 1 do begin
         P^ := Map[P^];
         inc(P);
      end;
      inc(PS, Stride);
   end;
end;
*)



procedure HSIfromRGBTrip(RGB : tPlatformColor; var H,S,I : float32);
{PE&RS, Oct 1995, p.1228}
{   I: 0--255 }
{   H: 0--360 }
{   S:        }
var
  v1,v2 : float64;
  r,g,b : byte;
begin
   ConvertPlatformColorToRGB(RGB,r,g,b);
   i  :=  0.333333 * r + 0.333333 * g + 0.333333 * b;
   v1 := -0.408248 * r - 0.408248 * g + 0.816497 * b;
   v2 :=  0.408248 * r - 0.408248 * g + 0 * b;
   S := sqrt(sqr(v1) + sqr(v2));
   if abs(v1) < 0.0000001 then H := 0
   else H := HeadingOfLine(v2,v1);
end;


procedure IHSMergePurgeBitmaps(var IntensityResultBitmap1,ColorBitmap2 :  tMyBitmap;   DarkenWhite : boolean = false);
{takes color from bitmap2, intensity from bitmap1, and replaces bitmap1}
var
   x,y : integer;
   h2,l2,s2 : float32;
   BMPMemory1,BMPMemory2 : tBMPMemory;
begin
   {$IfDef RecordIHSmerges} WriteLineToDebugFile('IHSMergeBitmaps from PetImage'); {$EndIf}
   BMPMemory1 := tBMPMemory.Create(IntensityResultBitmap1);
   BMPMemory2 := tBMPMemory.Create(ColorBitmap2);
   for y := 0 to pred(IntensityResultBitmap1.Height) do if (y < ColorBitmap2.Height) then begin
      for x := 0 to pred(IntensityResultBitmap1.Width) do if (x < ColorBitmap2.Width) then  begin
         if DarkenWhite or (not BMPMemory2.SameColor(x,y,claWhite)) then begin
            HSIfromRGBTrip(BMPMemory2.GetPixelColor(x,y),H2,S2,l2);
            BMPMemory1.SetPixelColor(x,y,RGBtripFromHSI(H2,S2,BMPMemory1.RedChannel(x,y)));
         end;
      end;
   end;
   BMPMemory2.Destroy;
   BMPMemory1.Destroy;
   FreeAndNil(ColorBitmap2);
end;



function BitmapSize(bmp : tMyBitmap) : shortstring;
begin
   Result := IntToStr(bmp.Width) + 'x' + IntToStr(bmp.Height);
end;


procedure ErodeTheImage(var Bitmap1 :  tMyBitmap; BackColor : tColor);
//http://blog.ostermiller.org/dilate-and-erode
var
   i,j,x,y : integer;
   Bitmap2 : tMyBitmap;
   BackRGBTriple: tPlatformColor;
   BMPMemory1,BMPMemory2 : tBMPMemory;
begin
   BackRGBtriple := ConvertTColorToPlatformColor(BackColor);
   CreateBitmap(Bitmap2,Bitmap1.Width,Bitmap1.Height);
   DrawBitmap1onBitmap2(Bitmap1,Bitmap2);
   BMPMemory1 := tBMPMemory.Create(Bitmap1);
   BMPMemory2 := tBMPMemory.Create(Bitmap2);
   for y := 1 to pred(pred(Bitmap1.Height)) do begin
      for x := 1 to pred(pred(Bitmap1.Width)) do begin
         if BMPMemory2.SameColor(x,y,BackRGBtriple) then begin
            for i := -1 to 1 do
               for j := -1 to 1 do
                 BMPMemory1.SetPixelColor(x+i,y+j,BackRGBtriple);
         end;
      end;
   end;
   BMPMemory2.Destroy;
   BMPMemory1.Destroy;
   FreeAndNil(Bitmap2);
end;


procedure StrethDrawBitmap1onBitmap2(var inBMP,NewBMP : tMyBitmap);
begin
    {$IfDef FMX}
       if NewBMP.Canvas.BeginScene then begin
          try
             NewBMP.Canvas.DrawBitmap(inBMP,RectF(0,0,inBMP.Width,inBMP.Height),RectF(0,0,NewBMP.Width,NewBMP.Height),1);
          finally
              NewBMP.Canvas.EndScene;
          end;
       end;
    {$Else}
       NewBMP.canvas.StretchDraw(Rect(0,0,NewBMP.Width,NewBMP.Height),inbmp);
    {$EndIf}
end;


procedure DrawBitmap1onBitmap2(var inBMP,NewBMP : tMyBitmap);
begin
    {$IfDef FMX}
       if NewBMP.Canvas.BeginScene then begin
          try
             NewBMP.Canvas.DrawBitmap(inBMP,RectF(0,0,inBMP.Width,inBMP.Height),RectF(0,0,NewBMP.Width,NewBMP.Height),1);
          finally
              NewBMP.Canvas.EndScene;
          end;
        end;
    {$Else}
       NewBMP.canvas.Draw(0,0,inbmp);
    {$EndIf}
end;


function CreateThumbNailBMP(inBMP : tMyBitmap; tnHeight : integer) : tMyBitmap;
begin
   CreateBitmap(Result,round(tnHeight / inbmp.Height *inbmp.Width),tnHeight);
   StrethDrawBitmap1onBitmap2(inBMP,Result);
end;


function StretchBitmapToFill(bmp : tMyBitmap; Width,Height : integer) : tMyBitmap;
var
   zoom1,zoom2 : integer;
begin
   zoom1 := round(100 * Height / bmp.Height);
   zoom2 := round(100 * Width / bmp.Width);
   if zoom2 < zoom1 then zoom1 := zoom2;
   Result := CreateThumbNailBMP(bmp,zoom1 * bmp.Height div 100);
end;

function ImageSize(Image : tImage) : shortstring;
begin
  Result := 'Image: ' + IntToStr(Image.Width) + 'x' + IntToStr(Image.Height);
end;

function FormSize(Form : tForm) : shortstring;
begin
  Result := 'Form: ' + IntToStr(Form.Width) + 'x' + IntToStr(Form.Height);
end;


function FormClientSize(Form : tForm) : shortstring;
begin
  Result := 'Form client: ' + IntToStr(Form.ClientWidth) + 'x' + IntToStr(Form.ClientHeight);
end;


function CombineBitmaps(nc : integer; theFiles : tStringList; Capt : shortstring) : tMyBitmap;
var
   bmp : tMyBitmap;
   SingleWide,SingleHigh,
   Left,Right,Top,Bottom,
   n,nr : integer;
begin
   Result := nil;
   if (TheFiles.Count > 0) then begin
      nr := theFiles.Count div nc;
      if (nr = 0) or ((theFiles.Count mod nc) > 0) then inc(nr);
      for n := 0 to pred(theFiles.Count) do begin
         if FileExists(theFiles.Strings[n]) then begin
            bmp := LoadBitmapFromFile(theFiles.Strings[n]);
            FindImagePartOfBitmap(Bmp,Left,Right,Top,Bottom);
            if (Result = nil) then begin
               SingleWide := bmp.Width + 25;
               SingleHigh := bmp.Height + 25;
               if (Result = nil) then CreateBitmap(Result,nc * SingleWide + 10,nr * SingleHigh + 60);
            end;
            Result.Canvas.Draw( 5 + (n mod nc) * SingleWide, (n div nc) * SingleHigh + 15,bmp);
            bmp.Free;
         end;
      end;
      if (Result <> nil) then begin
         Result.Canvas.Font.Size := 14;
         Result.Canvas.TextOut(25,Result.Height - 40,Capt);
         GetImagePartOfBitmap(Result);
         {$IfDef RecordBigBitmap}  WriteLineToDebugFile('CombineBitmaps out, bmp=' + BitmapSizeString(Result)); {$EndIf}
      end;
   end;
end;


procedure AllGraphsOneImage(NumCols : integer = -99; legendOnRight : boolean = false);
var
   BottomMargin,
   i,x : integer;
   Findings : tStringlist;
   fName : PathStr;
   Bitmap,bmp : tMyBitmap;
begin
   {$IfDef RecordBigBitmap} WriteLineToDebugFile('AllGraphsOneImage in'); {$EndIf}
   Findings := tStringList.Create;
   BottomMargin := 45;
   for i := pred(WMDEM.MDIChildCount) downto 0 do begin
      if WMDEM.MDIChildren[i] is TThisBaseGraph then begin
         CopyImageToBitmap((WMDEM.MDIChildren[i] as TThisBaseGraph).Image1,Bitmap);

         if LegendOnRight then begin
            bmp := (WMDEM.MDIChildren[i] as TThisBaseGraph).MakeLegend((WMDEM.MDIChildren[i] as TThisBaseGraph).GraphDraw.LegendList,false);
            x := Bitmap.Width + 10;
            Bitmap.Width := x + bmp.Width;
            Bitmap.Canvas.Draw(x,Bitmap.Height - 10 - bmp.Height,bmp);
            bmp.Free;
         end;

         Bitmap.Height := Bitmap.Height + BottomMargin;
         fName := NextFileNumber(MDtempDir,'graph_4_biggie_','.bmp');
         Bitmap.SaveToFile(fName);
         Findings.Add(fName);
      end;
   end;
   if (Findings.Count > 0) then begin
      MakeBigBitmap(Findings,'','',NumCols);
      {$IfDef RecordBigBitmap}  WriteLineToDebugFile('AllGraphsOneImage out'); {$EndIf}
   end
   else begin
      Findings.Free;
      {$IfDef RecordBigBitmap}  WriteLineToDebugFile('No graphs found, AllGraphsOneImage out'); {$EndIf}
   end;
end;



procedure RestoreBigCompositeBitmap(fName : PathStr);
var
   bigbmp : tMyBitmap;
   ImageForm : TImageDisplayForm;
   theFiles : tStringList;
begin
   if (fName = '') then Petmar.GetFileFromDirectory('images list','*.txt',fName);
   if FileExists(fName) then begin
      theFiles := tStringList.Create;
      theFiles.LoadFromFile(fName);
      BigBMP := CombineBitmaps(MDDef.BigBM_nc, theFiles, '');
      theFiles.Destroy;
      ImageForm := TImageDisplayForm.Create(Application);
      ImageForm.LoadImage(BigBmp,true);
      ImageForm.BigBM_files := fName;
      ImageForm.ChangeColumns1.Visible := true;
   end;
end;



procedure GrayscaleImage(var Image1 : tImage);
var
   bmp : tMyBitmap;
begin
   CopyImageToBitmap(Image1,bmp);
   MakeTheBitmapGrayScale(bmp);
   Image1.Picture.Graphic := bmp;
   bmp.Free;
end;


procedure CopyToWindowToClipBoard(RightMarginToClip : integer);
var
   bmp : tMyBitmap;
begin
   bmp := PetImage.GetScreenCapture;
   bmp.Width := bmp.Width - RightMarginToClip;
   AssignBitmapToClipBoard(bmp);
   bmp.free;
end;


procedure SaveScreenCapture(RightMarginToClip : integer; var fName : PathStr; AskName : boolean = false; JustWindow : boolean = false);
var
   bmp : tMyBitmap;
begin
   bmp := PetImage.GetScreenCapture(JustWindow);
   if (fName = '') then begin
      {$IfDef ExMovie}
          GetNewGraphicsFileName('saved screen capture image',fName)
      {$Else}
         if AskName then GetNewGraphicsFileName('saved screen capture image',fName)
         else fName := Petmar.NextFileNumber(MovieDir,'screen_animation','.bmp');
      {$EndIf}
   end;
   bmp.Width := bmp.Width - RightMarginToClip;
   BMP.SaveToFile(fName);
   bmp.free;
end;


function GetScreenCapture(JustWindow : Boolean = false ) : TMyBitmap;
var
  Win: HWND;
  DC: HDC;
  WinRect: TRect;
  Width,Height: Integer;
begin
   CreateBitmap(Result,Screen.Width,Screen.Height);
   Win := GetForegroundWindow;
   if JustWindow then begin
      GetWindowRect(Win, WinRect);
      DC := GetWindowDC(Win);
   end
   else begin
      Windows.GetClientRect(Win, WinRect);
      DC := GetDC(Win);
   end;

   try
       Width := WinRect.Right - WinRect.Left;
       Height := WinRect.Bottom - WinRect.Top;
       Result.Height := Height;
       Result.Width := Width;
       BitBlt(Result.Canvas.Handle, 0, 0, Width, Height, DC, 0, 0, SRCCOPY);
   finally
      ReleaseDC(Win, DC);
   end;
end;


function SmartTextOut(Bitmap,TextMyBitmap : tMyBitmap; x,y : integer; TStr : shortString) : boolean;
var
   xp,yp,xl,yl : integer;
begin
   Result := false;
   if (TextMyBitmap <> Nil) then begin
      xl := x + Bitmap.Canvas.TextWidth(TStr);
      yl := y + Bitmap.Canvas.TextHeight(TStr);
      if (xl >= Bitmap.Width) then xl := pred(Bitmap.Width);
      if (yl >= Bitmap.Height) then xl := pred(Bitmap.Height);
      for xp := x to xl do
         for yp := y to yl do
            if (TextMyBitmap.Canvas.Pixels[xp,yp] <> clWhite) then exit;
      TextMyBitmap.Canvas.TextOut(x,y,TStr);
      TextMyBitmap.Canvas.Rectangle(x,y,x + TextMyBitmap.Canvas.TextWidth(TStr),y + TextMyBitmap.Canvas.TextHeight(TStr) );
   end;
   Bitmap.Canvas.TextOut(x,y,TStr);
   Result := true;
end;


procedure RGBtoXY(r,g,b : byte; var x,y : integer);
var
   Color : integer;
begin
   color := r + (256 * g) + (256 * 256 * b);
   x := Color mod bmpx;
   y := Color div bmpx;
end;


procedure RGBtoXYFloat(r,g,b : byte; var x,y : float64);
var
   xi,yi : integer;
begin
   RGBtoXY(r,g,b,xi,yi);
   x := xi / pred(bmpx);
   y := yi / pred(bmpx);
   x := xi / (bmpx);
   y := yi / (bmpx);
end;


function FullPaletteBitmap : PathStr;
var
   BMP : tMyBitmap;
   BMPMemory : tBMPMemory;
   x,y,r,g,b : integer;
begin
   Result := ProgramRootDir + 'full_palette.bmp';
   if Not FileExists(Result) then begin
      CreateBitmap(BMP,bmpx,bmpy);
      BMPMemory := tBMPMemory.Create(bmp);
      for r := 0 to 255 do begin
         for g := 0 to 255 do begin
            for b := 0 to 255 do begin
                RGBtoXY(r,g,b,x,y);
                BMPMemory.SetPixelRGB(x,y,r,g,b);
            end;
         end;
      end;
      BMPMemory.Destroy;
      BMP.SaveToFile(Result);
      BMP.Free;
   end;
end;


function Palette256Bitmap(Palette256 : tPalette256) : PathStr;
var
   BMP : tMyBitmap;
   BMPMemory : tBMPMemory;
   i : integer;
begin
   CreateBitmap(bmp,1,256);
   BMPMemory := tBMPMemory.Create(bmp);
   {$IfDef ExPointCloud}
   {$Else}
      if (Palette256 = p256LasClass) then begin
         Result := ProgramRootDir + 'las_palette256.bmp';
         for i := 0 to MaxLasCat do BMPMemory.SetPixelColor(0,i,LAS_RGB_colors[i]);
      end;
   {$EndIf}
   if (Palette256 = p256Gray) then begin
      Result := ProgramRootDir + 'gray_palette256.bmp';
      for i := 0 to 255 do BMPMemory.SetPixelRGB(0,i,i,i,i);
   end
   else if (Palette256 = p256RedRamp) then begin
      Result := ProgramRootDir + 'red_palette256.bmp';
      for i := 0 to 255 do BMPMemory.SetPixelRGB(0,i,i,0,0);
   end
   else if (Palette256 = p256GreenRamp) then begin
      Result := ProgramRootDir + 'green_palette256.bmp';
      for i := 0 to 255 do BMPMemory.SetPixelRGB(0,i,0,i,0);
   end
   else if (Palette256 = p256BlueRamp) then begin
      Result := ProgramRootDir + 'blue_palette256.bmp';
      for i := 0 to 255 do BMPMemory.SetPixelRGB(0,i,0,0,i);
   end
   else if (Palette256 = p256Spectrum) then begin
      {$IfDef VCL}
         Result := ProgramRootDir + 'spectrum_palette256.bmp';
         for i := 0 to 255 do BMPMemory.SetPixelColor(0,i,ConvertTColorToPlatformColor(SpectrumColorFunct(i,0,255)));
      {$EndIf}
   end
   else if (Palette256 = p256Terrain) then begin
      {$IfDef VCL}
         Result := ProgramRootDir + 'terrain_palette256.bmp';
         for i := 0 to 255 do BMPMemory.SetPixelColor(0,i,ConvertTColorToPlatformColor(TerrainTColor(i,0,255)));
      {$EndIf}
   end;
   BMPMemory.Destroy;
   if not FileExists(Result) then BMP.SaveToFile(Result);
   BMP.Free;
end;


procedure BitmapRectangle(var Bitmap : tMyBitmap; x1,y1,x2,y2 : integer);
{$IfDef FMX}
var
  p1, p2, p3, p4 : TPointF;
  MyPolygon: TPolygon; //System.Math.Vectors unit needed
begin
    // sets points that define polygon
     p1 := TPointF.Create(x1,y1);
     p2 := TPointF.Create(x1,y2);
     p3 := TPointF.Create(x2,y2);
     p4 := TPointF.Create(x2,y1);

     SetLength(MyPolygon, 4);
     MyPolygon[0] := p1;
     MyPolygon[1] := p2;
     MyPolygon[2] := p3;
     MyPolygon[3] := p4;
     Bitmap.Canvas.DrawPolygon(MyPolygon,100);
{$EndIf}

{$IfDef VCL}
begin
   Bitmap.Canvas.Rectangle(x1,y1,x2,y2);
{$EndIf}
end;


procedure BitmapSymbol(var Bitmap : tMyBitmap; x,y : integer; Sym : tDrawingSymbol; size: integer; color : TPlatformColor );
{$IfDef FMX}
var
  p1, p2, p3, p4 : TPointF;
  MyPolygon: TPolygon; //System.Math.Vectors unit needed
begin
    Bitmap.Canvas.BeginScene;
    Bitmap.Canvas.Stroke.Color := Color;
    if Sym = FilledBox then begin
       Bitmap.Canvas.Fill.Kind := TBrushKind.Solid;
       Bitmap.Canvas.Fill.Color := Color;
    end;
     // sets points that define polygon
     p1 := TPointF.Create(x-size,y+size);
     p2 := TPointF.Create(x+size,y+size);
     p3 := TPointF.Create(x+size,y-size);
     p4 := TPointF.Create(x-size,y-size);

     SetLength(MyPolygon, 4);
     MyPolygon[0] := p1;
     MyPolygon[1] := p2;
     MyPolygon[2] := p3;
     MyPolygon[3] := p4;
     if (Sym = FilledBox) then begin
        Bitmap.Canvas.FillPolygon(MyPolygon,100);
     end
     else begin
        Bitmap.Canvas.DrawPolygon(MyPolygon,100);
     end;
    Bitmap.Canvas.EndScene;
{$EndIf}
{$IfDef VCL}
begin
   ScreenSymbol(Bitmap.Canvas,x,y,Sym,size,color);
{$EndIf}
end;


procedure BitmapTextOut(var Bitmap : tMyBitmap; x,y : integer; TStr : shortstring);
begin
   {$IfDef VCL}
      Bitmap.Canvas.TextOut(x,y,TStr);
   {$EndIf}

   {$IfDef FMX}
      Bitmap.Canvas.FillText(RectF(x,y,x+Bitmap.Canvas.TextWidth(TStr),y+Bitmap.Canvas.TextHeight(TStr)), TStr, false, 100,[], TTextAlign.Leading, TTextAlign.Leading);
   {$EndIf}
end;


procedure BitmapSymbol(var Bitmap : tMyBitmap; x,y : integer; Symbol : tFullSymbolDeclaration);
begin
   BitmapSymbol(Bitmap,x,y,Symbol.DrawingSymbol,Symbol.Size,Symbol.Color);
end;


procedure SmoothFilterTheImage(var Bitmap1 :  tMyBitmap);
var
   NumSum,SumRed,SumGreen,SumBlue,
   i,j,x,y,xp,yp : integer;
   Bitmap2 : tMyBitmap;
   BMPMemory1,BMPMemory2 : tBMPMemory;
begin
   CreateBitmap(Bitmap2,Bitmap1.Width,Bitmap1.Height);
   DrawBitmap1onBitmap2(Bitmap1,Bitmap2);
   BMPMemory1 := tBMPMemory.Create(Bitmap1);
   BMPMemory2 := tBMPMemory.Create(Bitmap2);
   for y := 0 to pred(Bitmap2.Height) do begin
      for x := 0 to pred(Bitmap2.Width) do begin
         NumSum := 0;
         SumRed := 0;
         SumGreen := 0;
         SumBlue := 0;
         for i := -1 to 1 do begin
            for j := -1 to 1 do begin
               xp := x + i;
               yp := y + j;
               if BMPMemory2.OnBitmap(xp,yp) then begin
                  inc(NumSum);
                  inc(SumRed, BMPMemory2.RedChannel(xp,yp));
                  inc(SumGreen, BMPMemory2.GreenChannel(xp,yp));
                  inc(SumBlue, BMPMemory2.BlueChannel(xp,yp));
               end;
            end;
         end;
         if (NumSum > 0) then BMPMemory1.SetPixelRGB(x,y,SumRed div NumSum, SumGreen div NumSum, SumBlue div NumSum);
      end;
   end;
   BMPMemory2.Destroy;
   BMPMemory1.Destroy;
   FreeAndNil(Bitmap2);
end;


procedure BitmapWhiteToNearWhite(var Bitmap : tMyBitmap);
var
   x,y : integer;
   BMPMemory : tBMPMemory;
   Color : tPlatformColor;
begin
   BMPMemory := tBMPMemory.Create(Bitmap);
   for y := 0 to pred(Bitmap.Height) do begin
      for x := 0 to pred(Bitmap.Width) do begin
         Color := BMPMemory.GetPixelColor(x,y);
         {$IfDef VCL}
            if (Color.rgbtRed = 255) and (Color.rgbtBlue = 255) and (Color.rgbtGreen = 255) then BMPMemory.SetPixelColor(x,y,RGBTripleNearWhite);
         {$EndIf}
         {$IfDef FMX}
            if (tAlphaColorRec(Color).R = 255) and (tAlphaColorRec(Color).G = 255) and (tAlphaColorRec(Color).B = 255) then BMPMemory.SetPixelColor(x,y,RGBTripleNearWhite);
         {$EndIf}
      end;
   end;
   BMPMemory.Destroy;
end;


procedure BitmapRemoveNonGrays(var Bitmap : tMyBitmap; Tolerance : byte);
var
   x,y : integer;
   BMPMemory : tBMPMemory;
   Color : tPlatformColor;
begin
   BMPMemory := tBMPMemory.Create(Bitmap);
   for y := 0 to pred(Bitmap.Height) do begin
      for x := 0 to pred(Bitmap.Width) do begin
         Color := BMPMemory.GetPixelColor(x,y);
         {$IfDef VCL}
            if (abs(Color.rgbtRed - Color.rgbtGreen) > Tolerance) or (abs(Color.rgbtRed - Color.rgbtBlue) > Tolerance) or (abs(Color.rgbtGreen - Color.rgbtBlue) > Tolerance) then
         {$EndIf}
         {$IfDef FMX}
            if (abs(tAlphaColorRec(Color).R - tAlphaColorRec(Color).G) > Tolerance) or (abs(tAlphaColorRec(Color).R - tAlphaColorRec(Color).B) > Tolerance) or (abs(tAlphaColorRec(Color).G - tAlphaColorRec(Color).B) > Tolerance) then
         {$EndIf}
         BMPMemory.SetPixelColor(x,y,RGBTripleWhite);
      end;
   end;
   BMPMemory.Destroy;
end;


procedure BitmapRemoveGrays(var Bitmap : tMyBitmap; Limit : byte);
var
   x,y : integer;
   BMPMemory : tBMPMemory;
   r,g,b : byte;
begin
   BMPMemory := tBMPMemory.Create(Bitmap);
   for y := 0 to pred(Bitmap.Height) do begin
      for x := 0 to pred(Bitmap.Width) do begin
         BMPMemory.GetPixelRGB(x,y,r,g,b);
         if (r > Limit) or (g > Limit) or (b > Limit) then BMPMemory.SetPixelColor(x,y,RGBTripleWhite);
      end;
   end;
   BMPMemory.Destroy;
end;


function IsBitmapMonochrome(var Bitmap : tMyBitmap) : boolean;
var
   r,g,b : byte;
   x,y : integer;
   aBMPMemory : tBMPMemory;
begin
   result := true;
   aBMPMemory := tBMPMemory.Create(Bitmap);
   for y := 0 to pred(Bitmap.Height) do begin
      for x := 0 to pred(Bitmap.Width) do begin
        aBMPMemory.GetPixelRGB(x,y,r,g,b);
        if (r <> g) or (r <> b) then begin
            Result := false;
            aBMPMemory.Destroy;
            exit;
         end;
      end;
   end;
   aBMPMemory.Destroy;
end;


procedure FixQuadOveredge(var Bitmap : tMyBitmap);
var
   x,y : integer;
   BMPMemory : tBMPMemory;
begin
   BMPMemory := tBMPMemory.Create(Bitmap);
   for y := 0 to pred(Bitmap.Height) do begin
      x := 0;
      while BMPMemory.SameColor(x,y,RGBTripleBlack) do begin
         BMPMemory.SetPixelColor(x,y,RGBTripleWhite);
         inc(x);
      end;
      x := Bitmap.Width;
      while BMPMemory.SameColor(x,y,RGBTripleBlack) do begin
         BMPMemory.SetPixelColor(x,y,RGBTripleWhite);
         dec(x);
      end;
   end;
   BMPMemory.Destroy;
end;

function RGBtoGrayscale(r,g,b : byte) : byte;
begin
   Result := ValidByteRange(round(0.3 * r  + 0.59 * g  + 0.11 * b));
end;


function GrayscaleLevel(Color : tPlatformColor) : integer; inline;
begin
   {$IfDef VCL}
      Result := RGBtoGrayscale(Color.rgbtRed,Color.rgbtGreen,Color.rgbtBlue );
   {$EndIf}
   {$IfDef FMX}
      Result := RGBtoGrayscale((tAlphaColorRec(Color).R),(tAlphaColorRec(Color).G),(tAlphaColorRec(Color).B) );
   {$EndIf}
end;


procedure MakeTheBitmapGrayScale(var Bitmap : tMyBitmap);
var
   x,y,j : integer;
   BMPMemory : tBMPMemory;
   Color : tPlatformColor;
begin
   BMPMemory := tBMPMemory.Create(Bitmap);
   for y := 0 to pred(Bitmap.Height) do begin
      for x := 0 to pred(Bitmap.Width) do begin
         Color := BMPMemory.GetPixelColor(x,y);
         j := GrayscaleLevel(color);
         BMPMemory.SetPixelColor(x,y,GrayRGBtrip(j));
      end;
   end;
   BMPMemory.Destroy;
end;


procedure MakeTheBitmapSubdued(var Bitmap : tMyBitmap);
var
   x,y,j : integer;
   BMPMemory : tBMPMemory;
   Color : tPlatformColor;
begin
   BMPMemory := tBMPMemory.Create(Bitmap);
   for y := 0 to pred(Bitmap.Height) do begin
      for x := 0 to pred(Bitmap.Width) do begin
         Color := BMPMemory.GetPixelColor(x,y);
         j := GrayscaleLevel(color);
         j := ValidByteRange(128 + j div 2);
         BMPMemory.SetPixelColor(x,y,GrayRGBtrip(j));
      end;
   end;
   BMPMemory.Destroy;
end;

procedure BitmapCross(var Bitmap : tMyBitmap; x,y,Size : integer);
begin
   DrawLine(Bitmap,x-Size,y, x+Size,y);
   DrawLine(Bitmap,x,y-Size, x,y+Size);
end;

procedure DrawLine(var Bitmap : tMyBitmap; x1,y1,x2,y2 : integer);  inline;
begin
  {$IfDef VCL}
     Bitmap.Canvas.MoveTo(x1,y1);
     Bitmap.Canvas.LineTo(x2,y2);
  {$EndIf}

  {$IfDef FMX}
     Bitmap.Canvas.DrawLine(PointF(x1,y1),PointF(x2,y2),100);    //last term is the opacity
  {$EndIf}
end;


procedure BitmapCrossWithHole(var Bitmap : tMyBitmap; x,y : integer);
begin
   DrawLine(Bitmap, x+2,y,x+8,y);
   DrawLine(Bitmap, x-2,y,x-8,y);
   DrawLine(Bitmap, x,y+2,x,y+8);
   DrawLine(Bitmap, x,y-2,x,y-8);
end;




function AnaglyphFromTwoBitmaps(fName1,FName2 : PathStr) : tMyBitmap;
var
   Bitmap1,Bitmap2 : tMyBitmap;
   bp1,bp2,bp3 : tBMPMemory;
   r1,g1,b1,r2,g2,b2 : byte;
   x,y : integer;
begin
   Bitmap1 := LoadBitmapFromFile(fName1);
   Bitmap2 := LoadBitmapFromFile(fName2);
   PetImage.CreateBitmap(Result,Bitmap1.Width,Bitmap1.Height);
   bp1 := tBMPMemory.Create(Bitmap1);
   bp2 := tBMPMemory.Create(Bitmap2);
   bp3 := tBMPMemory.Create(Result);

   for y := 0 to pred(Result.Height) do begin
      if y < Bitmap2.Height then begin
         for x := 0 to pred(Result.Width) do begin
            if x < Bitmap2.Width then begin
               bp1.GetPixelRGB(x,y,r1,g1,b1);
               bp2.GetPixelRGB(x,y,r2,g2,b2);
               bp3.SetPixelRGB(x,y,r2,g1,b1);
            end;
         end;
      end;
   end;
   Bitmap1.Free;
   Bitmap2.Free;
   bp1.Destroy;
   bp2.Destroy;
   bp3.Destroy;
end;


function SetAlphaColor(rgbTriple : tRGBTriple) : TAlphaColor;
begin
   TAlphaColorRec(Result).r := rgbTriple.rgbtRed;
   TAlphaColorRec(Result).g := rgbTriple.rgbtGreen;
   TAlphaColorRec(Result).b := rgbTriple.rgbtBlue;
   TAlphaColorRec(Result).a := 255;
end;

{$IfDef FMX}
function RGB(r,g,b : byte) : tcolor;
begin
   result := 256 * 256 * b + 256 * g + r;
end;
{$EndIf}


procedure CreateBitmap(var Bitmap : tMyBitmap; width,height : integer);
begin
   {$IfDef VCL}
      Bitmap := tMyBitmap.Create;
      Bitmap.PixelFormat := pf24bit;
      Bitmap.Width := Width;
      Bitmap.Height := Height;
   {$EndIf}

   {$IfDef FMX}
      Bitmap := tbitmap.Create(width,height);
   {$EndIf}
end;


procedure CloneBitmap(Bitmap : tMyBitmap; var CloneBitmap : tMyBitmap);
begin
   CreateBitmap(CloneBitmap,Bitmap.Width,Bitmap.Height);
end;


function BitmapMismatch(src1, src2 : tMyBitmap) : boolean;
begin
   Result := (src1.Height <> src2.Height) or (src1.Width <> src2.Width)
end;


procedure SaveBitmap(Bitmap : tMyBitmap; SaveName : PathStr = ''); overload;
var
   i,j : integer;
   Ext  : ExtStr;
   UserInputName : boolean;
   {$IfDef VCL}
      {$IfDef ExJPG}
      {$Else}
         MyJPEG : TJPEGImage;
      {$EndIf}
      {$IfDef ExPNG}
      {$Else}
         MyPNG  : TPNGImage;
      {$EndIf}
      {$IfDef ExGIF}
      {$Else}
         GIF : tGIFImage;
         GifExt : TGIFGraphicControlExtension;
      {$EndIf}
   {$EndIf}
begin
   if (Bitmap = Nil) or (Bitmap.Width = 0) or (Bitmap.Height = 0) then begin
      {$IfDef RecordBitmapProblems} WriteLineToDebugFile(' problem in for SaveImageAsBMP fname=' + SaveName); {$EndIf}
      exit;
   end;
   {$If Defined(RecordBitmapProblems) or Defined(RecordBitmapExt)} WriteLineToDebugFile('in for SaveImageAsBMP ' +  BitmapSizeString(Bitmap) + '  fname=' + SaveName + ' ext=' + IntToStr(MDdef.DefaultSaveImageType));  {$EndIf}

   {$IfDef FMX}
      FixFileNameBackslashes(SaveName);
      Bitmap.SaveToFile(SaveName);
   {$EndIf}

   UserInputName := false;

   {$IfDef VCL}
      if (SaveName = '') or PathIsValid(SaveName) then begin
         if (ImageDir = MDtempDir) then ImageDir := MainMapData + 'images\';

         SaveName := ImageDir;
         GetNewGraphicsFileName('saved image',SaveName);
         ImageDir := ExtractFilePath(SaveName);
         UserInputName := true;
         {$IfDef RecordBitmapProblems} WriteLineToDebugFile('Selected  fname=' + SaveName);    {$EndIf}
      end;

      Ext := ExtractFileExt(SaveName);
      if UserInputName then ImageDir := ExtractFilePath(SaveName);
      If ExtEquals(Ext,'.BMP') then begin
         Bitmap.SaveToFile(SaveName);
         //if UserInputName then MDdef.DefaultSaveImageType := 1;
      end
      else begin
         if ExtEquals(Ext,'.GIF') then begin
            if UserInputName then MDdef.DefaultSaveImageType := 4;
            {$IfDef ExGIF}
            {$Else}
               GIF := TGIFImage.Create;
               try
                  if MDDef.TransparentGIF then begin
                     Bitmap.Canvas.Pen.Color := clWhite;
                     Bitmap.Canvas.Pen.Width := 1;
                     Bitmap.Canvas.Brush.Style := bsClear;
                     Bitmap.Canvas.Rectangle(0,0,pred(Bitmap.Width),pred(Bitmap.Height));
                     GIF.Transparent := true;
                  end;
                  GIF.Assign(Bitmap);
                  if MDDef.TransparentGIF then begin
                     i := 0;
                     GifExt := TGIFGraphicControlExtension.Create(GIF.Images[i]);
                     GifExt.Transparent := True;
                     GifExt.TransparentColorIndex := GIF.Images[i].Pixels[0,0];
                     GIF.Images[i].Extensions.Add(GifExt);
                  end;
                  {$IfDef RecordBitmapProblems} WriteLineToDebugFile('Save GIF=' + SaveName);  {$EndIf}
                  GIF.SaveToFile(SaveName);
               finally
                  GIF.Free;
               end;
            {$EndIf}
         end
         {$IfDef ExPNG}
         {$Else}
         else if ExtEquals(Ext, '.PNG') or ExtEquals(Ext, '.PNS') then  begin
            //if UserInputName then MDdef.DefaultSaveImageType := 5;
            MyPNG := TPNGImage.Create;
            MyPNG.Assign(Bitmap);

            {$IfDef RecordPNG}
               case MyPNG.Header.ColorType of
                  COLOR_PALETTE : writeLineToDebugFile('PNG bitdepth = COLOR_PALETTE');
                  COLOR_RGB      : writeLineToDebugFile('PNG bitdepth = COLOR_RGB');
                  COLOR_RGBALPHA : writeLineToDebugFile('PNG bitdepth = COLOR_RGBALPHA');
                  else writeLineToDebugFile('PNG bitdepth = something else');
               end;
               writeLineToDebugFile('Color depth = ' + IntToStr(MyPNG.Header.BitDepth) + '  Compression level = ' + IntToStr(MyPNG.CompressionLevel));
            {$EndIf}

            if MDDef.TransparentPNG then begin
               {$IfDef RecordPNG} WriteLineToDebugFile('PNG transparent = ' + IntToStr(MDDef.TransparentLevel)); {$EndIf}
               MyPNG.CreateAlpha;
               for j := 0 to pred(MyPNG.Header.Height) do begin
                  for i := 0 to pred(MyPNG.Header.Width) do begin
                    if (MyPNG.Pixels[i,j] = clWhite) then MyPNG.AlphaScanline[j]^[i] := 255
                    else MyPNG.AlphaScanline[j]^[i] := MDDef.TransparentLevel;
                  end;
               end;
            end;
            MyPNG.SaveToFile(SaveName);
            MyPNG.Free;
         end
         {$EndIf}
         else if ExtEquals(Ext, '.JPG') or ExtEquals(Ext, '.JPS') then begin
            //if UserInputName then MDdef.DefaultSaveImageType := 2;
            {$IfDef ExJPG}
            {$Else}
               MyJPEG := TJPEGImage.Create;
               {$IfDef VCL}
               if (MDdef.DefaultSaveImageType = 3) then begin
                  ReadDefault('Compression (1 [best compression] -- 100 [best image quality])',MDDef.JPEGQuality);
               end;
               {$EndIf}
               MyJPEG.CompressionQuality := MDDef.JPEGQuality;
               {$IfDef RecordJPEG} WriteLineToDebugFile('Compression=' + IntToStr(SaveQuality) + '  for ' + SaveName);  {$EndIf}
               MyJPEG.Assign(Bitmap);
               MyJPEG.SaveToFile(SaveName);
               MyJPEG.Free;
            {$EndIf}
         end;
      end;
      {$If Defined(RecordBitmapProblems) or Defined(RecordBitmapExt)}  WriteLineToDebugFile('out,ext=' + IntToStr(MDdef.DefaultSaveImageType)); {$EndIf}
   {$EndIf}
end;


{$IfDef FMX}
   function LoadBitmapFromFile(fName : PathStr) : tMyBitmap;
   var
      {$IfDef ExExif}
      {$Else}
      ExifData: TExifData;
      {$EndIf}
      Ext : ExtStr;
   begin
      Result := tBitmap.Create;
      if FileExists(fName) then begin
         Result.LoadFromFile(fName);
         {$IfDef ExExif}
         {$Else}
            Ext := ExtractFileExt(fName);

            if ExtEquals(Ext, '.JPG') or ExtEquals(Ext, '.JPEG') or ExtEquals(Ext, '.JPE') then begin
                ExifData := TExifData.Create;
                ExifData.EnsureEnumsInRange := False; //as we use case statements rather than array constants, no need to keep this property set to True
                ExifData.LoadFromGraphic(fName);
                if (not ExifData.Empty) then begin
                   if (ExifData.Orientation = toLeftBottom) then Result.Rotate(180);
                   if (ExifData.Orientation = toRightTop) then Result.Rotate(90);
                   if (ExifData.Orientation = toLeftBottom) then Result.Rotate(270);
                end;
                ExifData.Destroy;
            end;
         {$EndIf}
      end;
   end;


   procedure ClearBitmap(BM : tMyBitmap; Color : tAlphaColor);
   begin
      bm.Canvas.Clear(Color);
   end;
{$EndIf}


procedure PlotVector(Bitmap : tMyBitmap; xt,yt,xp,yp : integer;
   Color : tPlatformColor; LineWidth : integer = 1; ArrowHead : boolean = false; PointLabel : shortString = '');
const
   ArrowHeadSize = 12;
var
   x,y : integer;
   Strike,VectorSize : float64;
begin
    {$IfDef VCL}
       Bitmap.Canvas.Brush.Style := bsClear;
       Bitmap.Canvas.Pen.Width := LineWidth;
       Bitmap.Canvas.Pen.Color := ConvertPlatformColorToTColor(Color);
    {$EndIf}

    {$IfDef FMX}
       Bitmap.Canvas.Stroke.Thickness := LineWidth;
       Bitmap.Canvas.Stroke.Color := Color;
    {$EndIf}

    DrawLine(Bitmap,xp,yp,xt,yt);
    VectorSize := sqrt(sqr(xt-xp)) + sqr(yt-yp);
    if ArrowHead and (VectorSize > 0) then begin
       Strike := HeadingOfLine(xp-xt,yp-yt);
       x := round(xp - ArrowHeadSize * SinDeg(90 + Strike-75));
       y := round(yp - ArrowHeadSize * CosDeg(90 + Strike-75));
       DrawLine(Bitmap,xp,yp,x,y);
       x := round(xp + ArrowHeadSize * SinDeg(90 + Strike+75));
       y := round(yp + ArrowHeadSize * CosDeg(90 + Strike+75));
       DrawLine(Bitmap,xp,yp,x,y);
    end;
    if (PointLabel <> '') then begin
       {$IfDef VCL}
          Bitmap.Canvas.Font.Color := ConvertPlatformColorToTColor(Color);
          Bitmap.Canvas.Font.Size := 14;
       {$EndIf}
       if (Strike > 90) and (Strike < 270) then yp := round(yp - Bitmap.Canvas.TextHeight(PointLabel))
       else yp := yp + 4;
       BitmapTextOut(Bitmap,xp,yp,PointLabel);
    end;
end;

procedure PlotOrientedLine(Bitmap : tMyBitmap; x,y : integer; VectorSize, Strike : float64;
   Color : tPlatformColor; LineWidth : integer = 1; ArrowHead : boolean = false; ReverseArrow : boolean = false; StartArrowFromPoint : boolean = false);
var
   xt,yt,xp,yp : integer;
begin
   if ReverseArrow then Strike := strike + 180;
   if StartArrowFromPoint then begin
      xt := x;
      yt := y;
      xp := round(x + 2 * VectorSize * SinDeg(Strike));
      yp := round(y - 2 * VectorSize * CosDeg(Strike));
   end
   else begin
      xt := round(x - VectorSize * SinDeg(Strike));
      yt := round(y + VectorSize * CosDeg(Strike));
      xp := round(x + VectorSize * SinDeg(Strike));
      yp := round(y - VectorSize * CosDeg(Strike));
   end;
   PlotVector(Bitmap,xt,yt,xp,yp,Color,LineWidth,ArrowHead);
   ArrowEndX := xp;
   ArrowEndY := yp;
end;


procedure FindImagePartOfBitmap(var Bitmap : tMyBitmap; var  Left,Right,Top,Bottom : integer);
var
   x,y : integer;
   Done : boolean;
   BMPMemory : tBMPMemory;
begin
   {$IfDef RecordGetImagePartOfBitmap} WriteLineToDebugFile('FindImagePartOfBitmap out, old ' + BitmapSizeString(Bitmap)); {$EndIf}
   BMPMemory := tBMPMemory.Create(Bitmap);

   Left := 0;
   Done := false;
   repeat
      for y := 0 to pred(Bitmap.Height) do begin
         if not BMPMemory.EffectivelyWhite(Left,y) then begin
            Done := true;
            break;
         end;
      end;
      if not Done then inc(left);
   until Done or (Left = pred(Bitmap.Width));

   Right := pred(Bitmap.Width);
   Done := false;
   repeat
      for y := 0 to pred(Bitmap.Height) do begin
         if not BMPMemory.EffectivelyWhite(Right,y) then begin
            Done := true;
            break;
         end;
      end;
      if not Done then dec(right);
   until Done or (Right = 0);
   inc(right);

   Top := 0;
   Done := false;
   repeat
      for x := 0 to pred(Bitmap.Width) do begin
         if not BMPMemory.EffectivelyWhite(x,Top) then begin
            Done := true;
            break;
         end;
      end;
      if not Done then inc(Top);
   until Done or (Top = pred(Bitmap.height));

   Bottom := pred(Bitmap.Height);
   Done := false;
   repeat
      for x := 0 to pred(Bitmap.Width) do begin
         if not BMPMemory.EffectivelyWhite(x,Bottom) then begin
            Done := true;
            break;
         end;
      end;
      if not Done then dec(Bottom);
   until Done or (Bottom = 0);
   inc(Bottom);

   BMPMemory.Destroy;

   {$IfDef RecordGetImagePartOfBitmap}
      WriteLineToDebugFile('new ' + BitmapSizeString(Bitmap)t));
      WriteLineToDebugFile('top=' + IntToStr(Top) + '   bottom=' + IntToStr(Bottom));
      WriteLineToDebugFile('left=' + IntToStr(Left) + '   right=' + IntToStr(Right));
      WriteLineToDebugFile('FindImagePartOfBitmap');
   {$EndIf}
end;






{$IfDef NoPatternFloodFill}
{$Else}
procedure SimpleFillShape(Canvas : VCL.Graphics.tCanvas; xs,ys,MinX,MinY,MaxX,MaxY : integer;  CurrentPattern : tPatternRecord);

   procedure Fly(XStart,YStart,Incr : integer);
   var
      ElevMax,x,y : integer;

         procedure CheckAndFill(Canvas : VCL.Graphics.tCanvas; CurrentPattern : tPatternRecord; x,y : integer); inline;
         begin
           if (CurrentPattern.PatternMasks[y mod CurrentPattern.NumRows div 8,x mod CurrentPattern.NumCols] and MaskBit[y mod CurrentPattern.NumRows mod 8]) > 0 then Canvas.Pixels[x,y] := CurrentPattern.WinColor;
         end {proc};

   begin {proc Fly}
      x := xstart;
      repeat
         y := yStart;
         if (Canvas.Pixels[x,y] <> clWhite) or (x < MinX) or (x > MaxX ) then exit;
         while (Canvas.Pixels[x,y] = clWhite) and (y <= MaxY) do begin
            CheckAndFill(Canvas,CurrentPattern,x,y);
            inc(y);
         end {while};
         ElevMax := pred(y);
         y := pred(YStart);
         while (Canvas.Pixels[x,y] = clWhite) and (y >= MinY) do begin
           CheckAndFill(Canvas,CurrentPattern,x,y);
           dec(y);
         end {while};
         yStart := (ElevMax + succ(y)) div 2;
         inc(x,Incr);
      until false;
   end {proc Fly};


begin {proc SimpleFillShape}
   try
      Fly(xs,ys,1);
      Fly(pred(xs),ys,-1);
   finally
   end;
end {proc SimpleFillShape};

{$EndIf}


function GrayShade(Color : TColor; var g : byte) : boolean;
var
   r,b : byte;
begin
   GetRGBfromTColor(Color,r,g,b);
   GrayShade := (r = g) and (g = b);
end;


{$IfDef ExMovies}
{$Else}

         procedure MakeMovie(fName : PathStr);
         var
            Command : AnsiString;
         begin
            Command := ProgramRootDir + 'md_movie.exe ' +  MovieDir + ' ' + fName;
            WinExecAndWait32(Command);
         end;


         procedure MovieFromTwoBitmaps(f1,f2 : PathStr);
         var
            BMP1,BMP2 : tMyBitmap;
         begin
            BMP1 := LoadBitmapFromFile(f1);
            BMP2 := LoadBitmapFromFile(f2);
            MovieFromTwoBitmaps(BMP1,BMP2);
            BMP1.Free;
            BMP2.Free;
         end;


         procedure MovieFromTwoBitmaps(BMP1,BMP2 : tMyBitmap; BMP3 : tMyBitmap = Nil; BMP4 : tMyBitmap = Nil);
         var
            fName : PathStr;
            MovieFile : tStringList;

            procedure SaveBMP(BMP : tMyBitmap);
            begin
                fname := Petmar.NextFileNumber(MovieDir, 'tmovie_',MovieFileExt);
                MovieFile.Add(ExtractFileName(fname));
                PetImage.SaveBitmap(BMP,fName);
            end;

         begin
            if (BMP1 = nil) or (BMP2 = Nil) then exit;
            MovieFile := tStringList.Create;

            SaveBMP(BMP1);
            SaveBMP(BMP2);
            if (BMP3 <> Nil) then SaveBMP(BMP3);
            if (BMP4 <> Nil) then SaveBMP(BMP4);

            fname := Petmar.NextFileNumber(MovieDir, 'tmovie_','.mov');
            MovieFile.SaveToFile(fName);
            MovieFile.Free;
            MakeMovie(ExtractFileName(fName));
         end;


         procedure BlendMovie(TopImage,BottomImage : tMyBitmap);
         var
            MovieList : tStringList;
            i,Num : integer;
            fName : PathStr;
            Bitmap : tMyBitmap;
         begin
            Num := 255 div 5;
            i := 255;
            MovieList := tStringList.Create;
            while (i >= 0)  do  begin
               fName := 'blend' + intToStr(i) + MovieFileExt;
               Bitmap := BlendBitmaps(BottomImage,TopImage,i/255);
               PetImage.SaveBitmap(Bitmap,MDTempDir + fName);
               Bitmap.Free;
               MovieList.Add(fName);
               dec(i,Num);
            end;
            fName := System.IOUtils.TPath.Combine(MDTempDir, 'blend.mov');
            MovieList.SaveToFile(fName);
            MovieList.Free;
            MakeMovie(Fname);
         end;

{$EndIf}



{ tBMPMemory }

procedure tBMPMemory.SetPixelColorSize(x,y,Size : integer;  Color : tPlatformColor);
var
   Starter,Ender,
   xp,yp : integer;
begin
   //Starter := -( (Size-3) div 2);
   //if Starter < 1 then Starter := 1;
   //Ender := ( (Size) div 2);
   //if Ender < 1 then Ender := 1;
   //if Size = 1 then b
   Starter := 0;
   Ender := 0;
   if Size >= 2 then dec(starter);
   if size >= 3 then inc(Ender);
   if Size >= 4 then dec(starter);
   if size >= 5 then inc(Ender);


   for xp := x + Starter to x + Ender do begin
      if (xp >= 0) and (xp < BMPWidth) then begin
         for yp := y + Starter to y + Ender do begin
            if (yp >= 0) and (yp < BMPHeight) then begin
               SetPixelColor(xp,yp,Color);
            end;
         end;
      end;
   end;
end;


function tBMPMemory.GetPixelColor(x,y : integer) : tPlatformColor;
begin
   {$IfDef VCL}
      Result := p1[y]^[x];
   {$EndIf}
end;


procedure tBMPMemory.GetPixelRGB(x,y : integer; var r,g,b : byte);
begin
   {$IfDef VCL}
      r := p1[y]^[x].rgbtRed;
      g := p1[y]^[x].rgbtGreen;
      b := p1[y]^[x].rgbtBlue;
   {$EndIf}
end;

function tBMPMemory.RedChannel(x,y : integer) : byte;
begin
   {$IfDef VCL}
      Result := p1[y]^[x].rgbtRed;
   {$EndIf}
end;

function tBMPMemory.GreenChannel(x,y : integer) : byte;
begin
   {$IfDef VCL}
      Result := p1[y]^[x].rgbtGreen;
   {$EndIf}
end;


function tBMPMemory.BlueChannel(x,y : integer) : byte;
begin
   {$IfDef VCL}
      Result := p1[y]^[x].rgbtBlue;
   {$EndIf}
end;

function tBMPMemory.OnBitmap(x, y: integer): boolean;
begin
    Result := (x >= 0) and (x < BMPWidth) and (Y >= 0) and (y < BMPHeight);
end;

procedure tBMPMemory.SetPixelColor(x,y : integer;  Color : tPlatformColor);
begin
   {$IfDef VCL}
      p1[y]^[x] := Color;
   {$EndIf}

   {$IfDef FMX}
      BitmapData.SetPixel(x,y,Color);
   {$EndIf}
end;


procedure tBMPMemory.SetRedChannel(x,y : integer; Value : byte);
begin
   {$IfDef VCL}
      p1[y]^[x].rgbtRed := Value;
   {$EndIf}

   {$IfDef FMX}
   {$EndIf}
end;

procedure tBMPMemory.SetGreenChannel(x,y : integer; Value : byte);
begin
   {$IfDef VCL}
      p1[y]^[x].rgbtGreen := Value;
   {$EndIf}

   {$IfDef FMX}
   {$EndIf}
end;


procedure tBMPMemory.SetBlueChannel(x,y : integer; Value : byte);
begin
   {$IfDef VCL}
      p1[y]^[x].rgbtBlue := Value;
   {$EndIf}

   {$IfDef FMX}
   {$EndIf}
end;


procedure tBMPMemory.SetPixelRGB(x,y : integer; r,g,b : byte);
begin
   {$IfDef CheckBMPMem}
      if (x < 0) or (y < 0) or (x > pred(BMPHeight)) or (Y > pred(BMPwidth)) then begin
         exit;
      end;
   {$EndIf}

   {$IfDef VCL}
      p1[y]^[x].rgbtRed := r;
      p1[y]^[x].rgbtGreen := g;
      p1[y]^[x].rgbtBlue := b;
   {$EndIf}

   {$IfDef FMX}
   {$EndIf}
end;



procedure tBMPMemory.ColorizePixelRGB(x,y : integer; Color : tPlatFormColor);
var
   h2,l2,s2 : float32;
begin
   HSIfromRGBTrip(Color,H2,S2,l2);
   SetPixelColor(x,y,RGBtripFromHSI(H2,S2,RedChannel(x,y)));
end;


function tBMPMemory.EffectivelyWhite(x,y : integer) : boolean;  //inline;
begin
   Result := SameColor(x,y,claWhite) or SameColor(x,y,claNearWhite);
end;

function tBMPMemory.SameColor(x,y : integer;  Color : tPlatformColor) : boolean;
var
  pc : tPlatformColor;
begin
   pc := GetPixelColor(x,y);
   {$IfDef VCL}
      Result := (pc.rgbtRed = Color.rgbtRed) and (pc.rgbtGreen = Color.rgbtGreen) and (pc.rgbtBlue = Color.rgbtBlue);
   {$EndIf}

   {$IfDef FMX}
      Result := (tAlphaColorRec(pc).R = tAlphaColorRec(Color).R) and (tAlphaColorRec(pc).g = tAlphaColorRec(Color).g) and (tAlphaColorRec(pc).b = tAlphaColorRec(Color).b);
   {$EndIf}
end;

constructor tBMPMemory.Create(var Bitmap: tMyBitmap);
begin
   BMPHeight := Bitmap.Height;
   BMPWidth := Bitmap.Width;
   NumDone := 0;
   {$IfDef VCL}
      FillScanlineAddresses(bitmap,P1);
   {$EndIf}

   {$IfDef FMX}
      if not (Bitmap.Map(TMapAccess.Write, BitmapData)) then exit;
      inBitmap := Bitmap;
   {$EndIf}
end;

destructor tBMPMemory.Destroy;
begin
   {$IfDef FMX}
      inBitmap.Unmap(BitmapData);
   {$EndIf}
end;


initialization
   {$IfDef MessageStartUpUnitProblems} MessageToContinue('Startup petimage'); {$EndIf}
   LastPhotoRoamX := -1;
   LastPhotoRoamY := -1;
finalization
   {$IfDef RecordImageOverlayProblems} WriteLineToDebugFile('RecordImageOverlayProblems active in PetImage'); {$EndIf}
   {$IfDef RecordRoamOnMapProblems} WriteLineToDebugFile('RecordRoamOnMapProblems active in PetImage'); {$EndIf}
   {$IfDef RecordImageResize} WriteLineToDebugFile('RecordImageResize active in PetImage'); {$EndIf}
   {$IfDef RecordBlendBitmaps} WriteLineToDebugFile('RecordBlendBitmaps active in PetImage'); {$EndIf}
   {$IfDef RecordJPEG} WriteLineToDebugFile('RecordJPEG active in PetImage'); {$EndIf}
   {$IfDef RecordIHSmerges} WriteLineToDebugFile('RecordIHSmerges active in PetImage'); {$EndIf}
   {$IfDef RecordPNG} WriteLineToDebugFile('RecordPNG active in PetImage'); {$EndIf}
   {$IfDef RecordBitmapProblems} WriteLineToDebugFile('RecordBitmapProblems active in PetImage'); {$EndIf}
   {$IfDef RecordGetImagePartOfBitmap} WriteLineToDebugFile('RecordGetImagePartOfBitmap active in PetImage'); {$EndIf}
   {$IfDef RecordDipStrike} WriteLineToDebugFile('RecordDipStrike active in PetImage'); {$EndIf}
   {$IfDef RecordFullPalette} WriteLineToDebugFile('RecordFullPalette active in PetImage'); {$EndIf}
   {$If Defined(RecordBitmapExt)} WriteLineToDebugFile('close petimage, default ext=' + IntToStr(MDdef.DefaultSaveImageType)); {$EndIf}

   {$IfDef MessageShutdownUnitProblems} MessageToContinue('Closing petimage'); {$EndIf}
end.



















