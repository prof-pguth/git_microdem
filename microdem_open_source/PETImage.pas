 unit Petimage;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of ianMICRODEM GIS Program    }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2015 Peter L. Guth   }
{____________________________________}


{$I nevadia_defines.inc}

//these should never be defined, but it's not worth removing in case someone wants them
   //{$Define ExExif}
   //{$Define NoPatternFloodFill}

{$IfDef RecordProblems}   //normally only defined for debugging specific problems
   {$IfDef Debug}
      {$Define BMPMemInline}
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
      //{$Define RecordFullPalette}
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

  {$IfDef FMX}
     FMX.Graphics,FMX.Types,
  {$EndIf}

  {$IfDef NoPatternFloodFill}
  {$Else}
     Zipatone,
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

      procedure MakeBigBitmap(var theFiles : tStringList; Capt : shortstring; SaveName : PathStr = '');
      procedure RestoreBigCompositeBitmap(fName : PathStr);

      procedure Drehen90Grad(Bitmap : tMyBitmap);
      procedure Drehen180Grad(Bitmap : tMyBitmap);
      procedure Drehen270Grad(Bitmap : tMyBitmap);

      procedure SaveImageAsThumbnail(Image1 : tImage; FName : PathStr; ThumbNailHeight : integer);
      procedure LoadBitmapInImage(Image1 : tImage; fName : PathStr);
      procedure SaveImageAsBMP(Image1 : tImage; SaveName : PathStr = '');
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

      //these functions take the top window, and are designed for the case where the top window is an FMX form in a VCL project.
      //They probably do not currently work in FireMonkey
      function GetScreenCapture(JustWindow : Boolean = false) : TMyBitmap;
      procedure SaveScreenCapture(RightMarginToClip : integer; var fName : PathStr; AskName : boolean = false; JustWindow : boolean = false);

      procedure CopyToWindowToClipBoard(RightMarginToClip : integer);
      procedure RenamePhotoJPEGS(PhotoDir : PathStr = ''; NameContains : shortString = '');
{$EndIf}


{$IfDef ExMovies}
{$Else}
   procedure MakeMovie(fName : PathStr);
   procedure BlendMovie(TopImage,BottomImage : tMyBitmap);
   procedure MovieFromTwoBitmaps(BMP1,BMP2 : tMyBitmap; BMP3 : tMyBitmap = Nil; BMP4 : tMyBitmap = Nil); overload;
   procedure MovieFromTwoBitmaps(f1,f2 : PathStr); overload;
{$EndIf}

type
   tPalette256 = (p256LasClass,p256Gray,p256Terrain,p256Spectrum,p256RedRamp,p256GreenRamp,p256BlueRamp);

function Palette256Bitmap(Palette256 : tPalette256) : PathStr;
function FullPaletteBitmap : PathStr;
procedure RGBtoXY(r,g,b : byte; var x,y : integer);     inline;
procedure RGBtoXYFloat(r,g,b : byte; var x,y : float64);  inline;
function SetAlphaColor(rgbTriple : tRGBTriple) : TAlphaColor;  inline;
procedure HSIfromRGBTrip(RGB : tPlatformColor; var H,S,I : float32);
function CombineBitmaps(nc : byte; theFiles : tStringList; Capt : shortstring) : tMyBitmap;


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

   {$IfDef ExMovies}
   {$Else}
      //PetMovie,
   {$EndIf}

   {$IfDef MICRODEM}
      DEMDefs,
   {$EndIf}
   BaseMap,
   PETMath;

type
   trgbArray =  array[0..MaxScreenXMax] of TRGBTriple;
   prgbArray =  ^trgbArray;


      function ExtractPartOfImage(var Image1 : tImage; Left,Right,Top,Bottom : integer) : tMyBitmap;
      var
         bmp : tMyBitmap;
      begin
         CopyImageToBitmap(Image1,bmp);
         CreateBitmap(Result,Right - Left, Bottom - Top);
         Result.Canvas.CopyRect(Rect(0,0,Result.Width,Result.Height),bmp.Canvas,Rect(Left,Top,Right,Bottom));
         bmp.Free;
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
   zoom2 :=  round(100 * Width / bmp.Width);
   if zoom2 < zoom1 then zoom1 := zoom2;
   Result := CreateThumbNailBMP(bmp,zoom1 * bmp.Height div 100);
end;


{$IfDef VCL}
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


function CombineBitmaps(nc : byte; theFiles : tStringList; Capt : shortstring) : tMyBitmap;
var
   bmp : tMyBitmap;
   SingleWide,SingleHigh,
   Left,Right,Top,Bottom,
   n,nr : integer;
begin
   if (TheFiles.Count > 0) then begin
      nr := theFiles.Count div nc;
      if (nr = 0) or ((theFiles.Count mod nc) > 0) then inc(nr);
      for n := 0 to pred(theFiles.Count) do begin
         if FileExists(theFiles.Strings[n]) then begin
            bmp := LoadBitmapFromFile(theFiles.Strings[n]);
            FindImagePartOfBitmap(Bmp,Left,Right,Top,Bottom);
            //bmp.SaveToFile(MDTempDir + 'bmp-' + IntToStr(n) + '.bmp');
            if (n=0) then begin
               SingleWide := bmp.Width + 25;
               SingleHigh := bmp.Height + 25;
               CreateBitmap(Result,nc * SingleWide + 10,nr * SingleHigh + 60);
               //Result.SaveToFile(MDTempDir + 'step-0.bmp');
            end;
            Result.Canvas.Draw( 5 + (n mod nc) * SingleWide, (n div nc) * SingleHigh + 15,bmp);
            //Result.SaveToFile(MDTempDir + 'step-' + IntToStr(n) + '.bmp');
            bmp.Free;
         end;
      end;
      Result.Canvas.Font.Size := 14;
      Result.Canvas.TextOut(25,Result.Height - 40,Capt);
      //GetImagePartOfBitmap(Result);
   end;
end;


procedure RestoreBigCompositeBitmap(fName : PathStr);
var
   bigbmp : tMyBitmap;
   ImageForm : TImageDisplayForm;
   theFiles : tStringList;
begin
   if fName = '' then Petmar.GetFileFromDirectory('images list','*.txt',fName);
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


procedure MakeBigBitmap(var theFiles : tStringList; Capt : shortstring; SaveName : PathStr = '');
var
   bigbmp : tMyBitmap;
   fName : PathStr;
   ImageForm : TImageDisplayForm;
begin
   if (TheFiles.Count > 0) then begin
     BigBMP := CombineBitmaps(MDDef.BigBM_nc, theFiles, Capt);
     if (SaveName <> '') then begin
         SaveBitmap(BigBmp,SaveName);
         Bigbmp.Free;
      end
      else begin
         ImageForm := TImageDisplayForm.Create(Application);
         ImageForm.LoadImage(BigBmp,true);
         fName := Petmar.NextFileNumber(MDtempDir,'big_bmp_files_','.txt');
         theFiles.SaveToFile(fName);
         ImageForm.BigBM_Capt := Capt;
         ImageForm.BigBM_files := fName;
         ImageForm.ChangeColumns1.Visible := true;
         ImageForm.Changecolumns1Click(nil);
         //ImageForm.RedrawSpeedButton12Click(Nil);
      end;
   end;
   theFiles.Destroy;
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
{$EndIf}


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

      {$IfDef RecordFullPalette}
         procedure ThereAndBack(r,g,b : integer);
         var
            x,y : integer;
         begin
            RGBtoXY(r,g,b,x,y);
            WriteLineToDebugFile(RGBString(r,g,b) + '  ' + IntToStr(BMPMemory.RedChannel(x,y)) + '  ' + IntToStr(BMPMemory.GreenChannel(x,y)) + '  ' + IntToStr(BMPMemory.BlueChannel(x,y)) );
         end;
      {$EndIf}

   procedure Makeit;
         var
            x,y,r,g,b : integer;
   begin
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
      {$IfDef RecordFullPalette}
         WriteLineToDebugFile('fname=' + Result);
         ThereAndBack(10,10,10);
         ThereAndBack(69,2,253);
         ThereAndBack(113,198,10);
         ThereAndBack(0,10,255);
         ThereAndBack(255,10,254);
         ThereAndBack(0,0,0);
         ThereAndBack(127,127,127);
         ThereAndBack(255,255,255);
      {$EndIf}
      BMPMemory.Destroy;
      BMP.SaveToFile(Result);
      BMP.Free;
   end;


begin
   Result := ProgramRootDir + 'full_palette.bmp';
   {$IfDef RecordFullPalette}
      WriteLineToDebugFile('FullPaletteBitmap in');
      MakeIt;
   {$Else}
      if Not FileExists(Result) then Makeit;
   {$EndIf}
   {$IfDef RecordFullPalette} WriteLineToDebugFile('FullPaletteBitmap out'); {$EndIf}
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
         Result :=  ProgramRootDir + 'las_palette256.bmp';
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
      Result :=  RGBtoGrayscale((tAlphaColorRec(Color).R),(tAlphaColorRec(Color).G),(tAlphaColorRec(Color).B) );
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


{$IfDef VCL}

procedure RenamePhotoJPEGS(PhotoDir : PathStr = ''; NameContains : shortString = '');
var
   OldName,NewName,BaseName : PathStr;
   Ext : extstr;
   i,SubDirs : integer;
   TheFiles : tStringList;
   TStr : shortString;
   Ask : boolean;
begin
   {$IfDef RecordFileOps} WriteLineToDebugFile('Twmdem.RenameJPEGswithcreationtime1Click in'); {$EndIf}
   Ask := (PhotoDir = '');
   if (PhotoDir <> '') or GetDosPath('files to rename',PhotoDir) then begin
      SubDirs := 1;
      Ext := '.jpg';
      if NameContains = '' then NameContains := 'DSC_';
      if Ask then begin
         Petmar.ReadDefault('subdirectoy level',SubDirs);
         Petmar.GetString('file extension',Ext,false,ValidDosFileNameChars);
         Petmar.GetString('name starts with',NameContains,false,ValidDosFileNameChars);
      end;
      TheFiles := nil;
      Petmar.FindMatchingFiles(PhotoDir,'*' + Ext,TheFiles,SubDirs);
      StartProgress('Rename');
      for i := 0 to pred(TheFiles.Count) do begin
         if (i mod 10 = 0) then UpdateProgressBar(i/TheFiles.Count);
         OldName := TheFiles.Strings[i];
         if StrUtils.AnsiContainsText(Copy(ExtractFileNameNoExt(OldName),1,length(NameContains)),NameContains) then begin
            if NameContains[length(NameContains)] <> '_' then TStr := '_' else TStr := '';
            BaseName := NameContains + TStr + FileTimeFromFileName(OldName);
            NewName := ExtractFilePath(OldName) + BaseName + Ext;
            while FileExists(NewName) do begin
               NewName := Petmar.NextFileNumber(ExtractFilePath(OldName),BaseName + '_',Ext);
            end;
            {$IfDef RecordFileOps} WriteLineToDebugFile('OldName=' + Oldname + ' new name=' + ExtractFileName(NewName)); {$EndIf}
            System.SysUtils.RenameFile(OldName,NewName);
         end;
      end;
      TheFiles.Free;
      EndProgress;
   end;
   {$IfDef RecordFileOps} WriteLineToDebugFile('Twmdem.RenameJPEGswithcreationtime1Click out'); {$EndIf}
end;


procedure SetRedrawMode(Image1 : tImage);  inline;
begin
   Image1.Canvas.Pen.Mode := pmNotXor;
   Image1.Canvas.Pen.Color := clRed;
   Image1.Canvas.Pen.Width := 2;
   Image1.Canvas.Brush.Style := bsClear;
end;


      procedure ReplaceBitmapWithSubset(var Bitmap : tMyBitmap; Left,Right,Top,Bottom : integer);
      var
         NewBitmap : tMyBitmap;
      begin
         CreateBitmap(NewBitmap,Bitmap.Width,Bitmap.Height);
         NewBitmap.Canvas.Draw(0,0,Bitmap);
         Bitmap.Free;
         CreateBitmap(Bitmap,Right - Left, Bottom - Top);
         ClearBitmap(Bitmap,clNearWhite);
         Bitmap.Canvas.CopyRect(Rect(0,0,Bitmap.Width,Bitmap.Height),NewBitmap.Canvas,Rect(Left,Top,Right,Bottom));
         NewBitmap.Free;
      end;


      procedure MakeThisLowerRightCornerOfBitmap(var Bitmap : tMyBitmap; x,y : integer);
      begin
         ReplaceBitmapWithSubset(Bitmap,0,x,0,y);
      end;


      procedure MakeThisUpperLeftCornerOfBitmap(var Bitmap : tMyBitmap;x,y : integer);
      begin
         ReplaceBitmapWithSubset(Bitmap,x,pred(Bitmap.Width),y,pred(Bitmap.Height));
      end;

{$EndIf}


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


function IsJPEG(Ext : ExtStr) : boolean;
begin
    Result := ExtEquals(Ext, '.JPEG') or ExtEquals(Ext, '.JPG') or ExtEquals(Ext, '.JPE');
end;

function ValidImageFileExt(Ext : ExtStr) : boolean;
begin
   Result := ExtEquals(Ext, '.BMP') or
      IsJPEG(Ext) or ExtEquals(Ext, '.JPS')
      {$IfDef ExPNG}  {$Else} or ExtEquals(Ext, '.PNG') or ExtEquals(Ext,'.PNS') {$EndIf}
      {$IfDef ExGIF}  {$Else} or ExtEquals(Ext, '.GIF') {$EndIf}
      {$IfDef ExTiff} {$Else} or ExtEquals(Ext, '.TIF') {$EndIf};
end;


function ValidImageFileName(fName : PathStr) : boolean; overload;
begin
  Result := ValidImageFileExt(ExtractFileExt(fName));
end;


function GraphicsFilters : shortstring;
begin
   Result := 'BMP|*.bmp|JPEG (quick)|*.jpg|JPEG (deliberate)|*.jpg'
         {$IfDef ExGIF} {$Else} + '|GIF|*.gif' {$EndIf}
         {$IfDef ExPNG} {$Else} + '|PNG|*.png' {$EndIf};
end;

function ThreeDeeGraphicsFilters : shortstring;
begin
   Result := 'Stereo PNG|*.PNS|Stereo JPEG|*.JPS';
end;


function AllowedGraphicsFilters : shortstring;
begin
   Result := 'Graphics files|*.bmp;*.jpg;*.jpeg;*.jpe;*.tif;*.tiff'
         {$IfDef ExGIF} {$Else} + ';*.gif' {$EndIf}
         {$IfDef ExPNG} {$Else} + ';*.png' {$EndIf};
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
      {$IfDef RecordBitmapProblems} WriteLineToDebugFile(' problem in for SaveImageAsBMP fname=' + SaveName);   {$EndIf}
      exit;
   end;
   {$If Defined(RecordBitmapProblems) or Defined(RecordBitmapExt)} WriteLineToDebugFile('in for SaveImageAsBMP ' + IntToStr(Bitmap.Width) + 'x' + IntToStr(Bitmap.Height) + '  fname=' + SaveName + ' ext=' + IntToStr(MDdef.DefaultSaveImageType));  {$EndIf}

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
               {$IfDef RecordPNG} WriteLineToDebugFile('PNG transparent = ' + IntToStr(MDDef.TransparentLevel));   {$EndIf}
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
{$EndIf}


{$IfDef VCL}
function LoadBitmapFromFile(fName : PathStr) : tMyBitmap;
var
   Ext    : ExtStr;
   MyJPEG : TJPEGImage;
   {$IfDef ExGIF}
   {$Else}
      GIF : TGIFImage;
   {$EndIf}
   {$IfDef ExExif}
   {$Else}
      ExifData: TExifData;
      RegVar : tRegVars;
      MapProj  : tMapProjection;
   {$EndIf}

   {$IfDef ExPNG}
   {$Else}
      MyPNG : TPNGImage;
   {$EndIf}
   {$IfDef ExTiff}
   {$Else}
      Success : boolean;
      TiffImage : tTiffImage;
   {$EndIf}
begin
   if (not FileExists(fName)) or (fName = '') then  begin
      Result := Nil;
      exit;
   end;
   {$IfDef RecordBitmapProblems} WriteLineToDebugFile('in LoadBitmapFromFile ' + '  fname=' + fName);  {$EndIf}

   Ext := ExtractFileExt(fName);

   {$IfDef ExTiff}
   {$Else}
      if ExtEquals(Ext, '.tif') or ExtEquals(Ext, '.tiff') then begin
         MapProj := Nil;
         TiffImage := tTIFFImage.CreateGeotiff(MapProj,RegVar,true,FName,Success);
         Result := TiffImage.DisplayInBitmap;
         TiffImage.Destroy;
         FreeAndNil(MapProj);
         exit;
      end;
   {$EndIf}

   Result := tMyBitmap.Create;

   if ExtEquals(Ext, '.BMP') then begin
      Result.LoadFromFile(FName);
   end;

   if ExtEquals(Ext, '.JPG') or ExtEquals(Ext, '.JPEG') or ExtEquals(Ext, '.JPE') then begin
      try
         MyJPEG := TJPEGImage.Create;
         MyJPEG.LoadFromFile(FName);
         MyJPEG.Performance := jpBestQuality;
         Result.Assign(MyJPEG);
         MyJPEG.Free;

         {$IfDef ExExif}
         {$Else}
             ExifData := TExifData.Create;
             ExifData.EnsureEnumsInRange := False; //as we use case statements rather than array constants, no need to keep this property set to True
             //ExifData.LoadFromJPEG(fName);
             ExifData.LoadFromGraphic(fName);
             if (not ExifData.Empty) then begin
                if ExifData.Orientation in [toBottomRight,toRightTop,toLeftBottom] then begin
                   //TTiffOrientation = (toUndefined, toTopLeft {1}, toTopRight, toBottomRight {3},toBottomLeft, toLeftTop{i.e., rotated}, toRightTop {6}, toRightBottom, toLeftBottom {8});

                   if (ExifData.Orientation in [toLeftBottom,toBottomRight]) then PetImage.Drehen180Grad(Result);
                   if (ExifData.Orientation = toRightTop) then PetImage.Drehen90Grad(Result);
                   if (ExifData.Orientation = toLeftBottom) then PetImage.Drehen270Grad(Result);
                end;
             end;
             ExifData.Destroy;
          {$EndIf}
      except
         on Exception do raise;
      end;
   end;

   {$IfDef ExPNG}
   {$Else}
      if ExtEquals(Ext, '.png') then begin
         try
           MyPNG := TPNGImage.Create;
           MyPNG.LoadFromFile(fName);
           Result.Assign(MyPNG);
           MyPNG.Free;
         except
            on Exception do raise;
         end;
      end;
   {$EndIf}


   {$IfDef ExGIF}
   {$Else}
      if ExtEquals(Ext, '.gif') then begin
         GIF := TGIFImage.Create;
         GIF.LoadFromFile(fName);
         Result.Assign(GIF);
         GIF.Free;
      end;
   {$EndIf}

   Result.PixelFormat := pf24bit;
end;
{$EndIf}



{$IfDef VCL}
procedure ClearBitmap(BM : tMyBitmap; Color : tColor);
begin
   bm.Canvas.Brush.Color := Color;
   bm.Canvas.Pen.Color := Color;
   bm.Canvas.Rectangle(0,0,Bm.Width,Bm.Height);
end;
{$EndIf}

{$IfDef FMX}
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
   {$IfDef RecordGetImagePartOfBitmap} WriteLineToDebugFile('FindImagePartOfBitmap out, old Bitmap size=' + IntToStr(Bitmap.width) + 'x' + IntToStr(Bitmap.height)); {$EndIf}
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
      WriteLineToDebugFile('new Bitmap size=' + IntToStr(Bitmap.width) + 'x' + IntToStr(Bitmap.height));
      WriteLineToDebugFile('top=' + IntToStr(Top) + '   bottom=' + IntToStr(Bottom));
      WriteLineToDebugFile('left=' + IntToStr(Left) + '   right=' + IntToStr(Right));
      WriteLineToDebugFile('FindImagePartOfBitmap');
   {$EndIf}
end;



{$IfDef VCL}

procedure PlotDipSymbol(Bitmap : tMyBitmap; x,y,VectorSize,Dip,Strike,DipDirect : integer;
    ThisIs : tStructureType; LabelValues : boolean; PenColor : tPlatformColor; PenWidth : integer );
var
   xt,yt,xt2,yt2 : integer;
   TStr  : ShortString;
begin
   {$IfDef RecordDipStrike} WriteLineToDebugFile('PlotDipSymbol at x=' + IntToStr(x) + '  y=' + IntToStr(y)); {$EndIf}

   with Bitmap.Canvas do  begin
      Brush.Style := bsClear;
      Pen.Width := PenWidth;
      Pen.Color := ConvertPlatformColorToTColor(PenColor);
      if (Dip = 0) then begin
         if (ThisIs = aBedding) then begin
            Ellipse(x-VectorSize,y-VectorSize,X+VectorSize,Y+VectorSize);
            DrawLine(Bitmap,x,y-VectorSize,x,y+VectorSize);
            DrawLine(Bitmap,x-VectorSize,y,x+VectorSize,y);
         end;
      end
      else begin
         xt := round(x + VectorSize * SinDeg(Strike));
         yt := round(y - VectorSize * CosDeg(Strike));
         MoveTo(xt,yt);

         xt := round(x - VectorSize * SinDeg(Strike));
         yt := round(y + VectorSize * CosDeg(Strike));
         LineTo(xt,yt);
         if (ThisIs = aBedding) then begin
            xt := round(x + VectorSize div 3 * SinDeg(DipDirect));
            yt := round(y - VectorSize div 3 * CosDeg(DipDirect));
            {$IfDef RecordDipStrike}
            WriteLineToDebugFile('End dip symbol at x=' + IntToStr(xt) + '  y=' + IntToStr(yt));
            {$EndIf}
            DrawLine(Bitmap,x,y,xt,yt);
            if (Dip = 90) then begin
               xt := round(x + VectorSize div 3 * SinDeg(DipDirect+180));
               yt := round(y - VectorSize div 3 * CosDeg(DipDirect+180));
               DrawLine(Bitmap,x,y,xt,yt);
               exit;
            end;
         end;
         if (ThisIs = aFault) then begin
            xt := round(x + VectorSize*3 div 2 * SinDeg(DipDirect));
            yt := round(y - VectorSize*3 div 2 * CosDeg(DipDirect));
            DrawLine(Bitmap,x,y,xt,yt);
            x := round(xt - VectorSize div 2* CosDeg(DipDirect-125));
            y := round(yt - VectorSize div 2* SinDeg(DipDirect-125));
            DrawLine(Bitmap,x,y,xt,yt);
            x := round(xt + VectorSize div 2* CosDeg(DipDirect+125));
            y := round(yt + VectorSize div 2* SinDeg(DipDirect+125));
            DrawLine(Bitmap,x,y,xt,yt);
         end;
         if (ThisIs = aJoint) then begin
            xt := round(x + VectorSize div 4 * SinDeg(Strike));
            yt := round(y - VectorSize div 4 * CosDeg(Strike));
            MoveTo(xt,yt);
            xt := round(xt + VectorSize div 4 * SinDeg(DipDirect));
            yt := round(yt - VectorSize div 4 * CosDeg(DipDirect));
            LineTo(xt,yt);
            xt2 := round(x - VectorSize  div 4 * SinDeg(Strike));
            yt2 := round(y + VectorSize  div 4 * CosDeg(Strike));
            MoveTo(xt2,yt2);
            xt2 := round(xt2 + VectorSize div 4 * SinDeg(DipDirect));
            yt2 := round(yt2 - VectorSize div 4 * CosDeg(DipDirect));
            LineTo(xt2,yt2);
            LineTo(xt,yt);
            if (Dip = 90) then begin
               xt := round(x + VectorSize div 4 * SinDeg(Strike));
               yt := round(y - VectorSize div 4 * CosDeg(Strike));
               MoveTo(xt,yt);
               xt := round(xt + VectorSize div 4 * SinDeg(DipDirect+180));
               yt := round(yt - VectorSize div 4 * CosDeg(DipDirect+180));
               LineTo(xt,yt);
               xt2 := round(x - VectorSize  div 4 * SinDeg(Strike));
               yt2 := round(y + VectorSize  div 4 * CosDeg(Strike));
               MoveTo(xt2,yt2);
               xt2 := round(xt2 + VectorSize div 4 * SinDeg(DipDirect+180));
               yt2 := round(yt2 - VectorSize div 4 * CosDeg(DipDirect+180));
               LineTo(xt2,yt2);
               LineTo(xt,yt);
               exit;
            end;
         end;
         if (ThisIs = aFoliation) then begin
            xt := round(x + VectorSize div 3 * SinDeg(Strike));
            yt := round(y - VectorSize div 3 * CosDeg(Strike));
            MoveTo(xt,yt);
            xt := round(x + VectorSize div 3 * SinDeg(DipDirect));
            yt := round(y - VectorSize div 3 * CosDeg(DipDirect));
            LineTo(xt,yt);
            xt2 := round(x - VectorSize div 3 * SinDeg(Strike));
            yt2 := round(y + VectorSize div 3 * CosDeg(Strike));
            LineTo(xt2,yt2);
            if Dip = 90 then begin
               xt := round(x + VectorSize div 3 * SinDeg(Strike));
               yt := round(y - VectorSize div 3 * CosDeg(Strike));
               MoveTo(xt,yt);
               xt := round(x + VectorSize div 3 * SinDeg(DipDirect+180));
               yt := round(y - VectorSize div 3 * CosDeg(DipDirect+180));
               LineTo(xt,yt);
               xt2 := round(x - VectorSize div 3 * SinDeg(Strike));
               yt2 := round(y + VectorSize div 3 * CosDeg(Strike));
               LineTo(xt2,yt2);
               exit;
            end;
         end;

         TStr := IntToStr(Dip);
         Font.Color := ConvertPlatformColorToTColor(PenColor);
         Font.Style := [fsBold];
         case VectorSize of
            8..12 : Font.Size := 8;
            13..15 : Font.Size := 10;
            16..20 : Font.Size := 12;
            else Font.Size := 15;
         end;

         if LabelValues then begin
            {$IfDef RecordDipStrike} WriteLineToDebugFile('DipDirect=' + IntToStr(DipDirect)); {$EndIf}
            if DipDirect in [0..200] then xt := xt+3
            else xt := xt-3-TextWidth(TStr);

            if (DipDirect in [0..90]) or ( (DipDirect >=270) and (DipDirect <=360) ) then begin
               yt := yt-TextHeight(TStr);
            end
            else if (DipDirect in [91..160]) or ( (DipDirect >=201) and (DipDirect <=269) ) then begin
               yt := yt+2;
            end
            else begin
               yt := yt + 2;
            end;

            TextOut(xt,yt,TStr);
            {$IfDef RecordDipStrike} WriteLineToDebugFile('Label at x=' + IntToStr(xt) + '  y=' + IntToStr(yt)); {$EndIf}
         end;
      end;
   end;
end;


procedure FillScanlineAddresses(Bitmap : tMyBitmap; var P3 : tScreenPRGB);
var
   j : integer;
begin
   for j := 0 to pred(Bitmap.Height) do P3[j] := Bitmap.ScanLine[j];
end;


procedure DilateTheImage(var Bitmap1 : tMyBitmap; ImageColor : tColor);
var
   x,y : integer;
   p1 :  tScreenPRGB;
   p2 : prgb;
   Bitmap2 : tMyBitmap;
   ImageRGBtriple : tRGBtriple;
begin
   ImageRGBtriple := ConvertTColorToPlatformColor(ImageColor);
   CreateBitmap(Bitmap2,Bitmap1.Width,Bitmap1.Height);
   Bitmap2.Canvas.Draw(0,0,Bitmap1);
   FillScanlineAddresses(Bitmap1,P1);

   for y := 1 to pred(pred(Bitmap1.Height)) do begin
      P2 := BitMap2.ScanLine[y];
      for x := 1 to pred(pred(Bitmap1.Width)) do begin
         if SameColor(p2[x],ImageRGBtriple) then begin
            p1[pred(y)][pred(x)] := ImageRGBtriple;
            p1[pred(y)][x] := ImageRGBtriple;
            p1[pred(y)][succ(x)] := ImageRGBtriple;
            p1[y][pred(x)] := ImageRGBtriple;
            p1[y][succ(x)] := ImageRGBtriple;
            p1[succ(y)][pred(x)] := ImageRGBtriple;
            p1[succ(y)][x] := ImageRGBtriple;
            p1[succ(y)][succ(x)] := ImageRGBtriple;
         end;
      end;
   end;
   FreeAndNil(Bitmap2);
end;



procedure GrayscaleNegative(bmp : tMyBitmap);
var
   x,y : integer;
   p0 : pRGB;
begin
   for y := 0 to pred(BMP.Height) do begin
      p0 := BMP.ScanLine[y];
      for x := 0 to pred(BMP.Width) do begin
         p0[x].rgbtBlue := 255-p0[x].rgbtBlue;
         p0[x].rgbtRed := 255-p0[x].rgbtRed;
         p0[x].rgbtGreen := 255-p0[x].rgbtGreen;
      end;
   end;
end;

procedure ThreshholdGrayscale(bmp : tMyBitmap; DownToZero,Upto255 : integer);
var
   x,y : integer;
   p0 : pRGB;
begin
   for y := 0 to pred(BMP.Height) do begin
      p0 := BMP.ScanLine[y];
      for x := 0 to pred(BMP.Width) do begin
         if p0[x].rgbtBlue <= DownToZero then begin
            p0[x].rgbtBlue := 0;
            p0[x].rgbtRed := 0;
            p0[x].rgbtGreen := 0;
         end
         else if p0[x].rgbtBlue >= UpTo255 then begin
            p0[x].rgbtBlue := 255;
            p0[x].rgbtRed := 255;
            p0[x].rgbtGreen := 255;
         end;
      end;
   end;
end;


procedure MakeGraphicsFileNegative(fName : PathStr);
var
   bmp : tMyBitmap;
begin
   bmp := tMyBitmap.Create;
   BMP.LoadFromFile(fname);
   GrayscaleNegative(bmp);
   PetImage.SaveBitmap(bmp,fName);
   bmp.free;
end;


procedure MakeBitmapNegative(var BMP : tMyBitmap; RGBTriple : tRGBTriple);
var
   x,y : integer;
   p0 : pRGB;
begin
   for y := 0 to pred(BMP.Height) do  begin
      p0 := BMP.ScanLine[y];
      for x := 0 to pred(BMP.Width) do  begin
         if SameColor(p0[x],RGBTripleWhite) then p0[x] := RGBTriple
         else p0[x] := RGBTripleWhite;
      end;
   end;
end;

procedure ChangeBitmapBlackWhite(var BMP : tMyBitmap; BlackReplace,WhiteReplace : tRGBTriple);
var
   x,y : integer;
   p0 : pRGB;
begin
   for y := 0 to pred(BMP.Height) do begin
      p0 := BMP.ScanLine[y];
      for x := 0 to pred(BMP.Width) do begin
         if SameColor(p0[x],RGBTripleWhite) then p0[x] := WhiteReplace
         else p0[x] := BlackReplace;
      end;
   end;
end;


procedure AssignBitmapToClipBoard(bm1 : tMyBitmap);
begin
   Clipboard.Assign(bm1);
end;


procedure GetImagePartOfBitmap(var Bitmap : tMyBitmap; WhiteBoundary : boolean = true; WhiteBoundarySize : integer = 2);
var
   NewBitmap : tMyBitmap;
   Left,Right,Top,Bottom : integer;
begin
   {$IfDef RecordGetImagePartOfBitmap} WriteLineToDebugFile('GetImagePartOfBitmap in'); {$EndIf}
   FindImagePartOfBitmap(Bitmap,Left,Right,Top,Bottom);
   if (Right > Left) and (Bottom > Top) then begin
      if (Left > 0) or (Right < Bitmap.Width) or (Top < Bitmap.Height) or (Bottom > 0) then begin
         CreateBitmap(NewBitmap,Bitmap.Width,Bitmap.Height);
         NewBitmap.Canvas.Draw(0,0,Bitmap);
         Bitmap.Free;
         if not WhiteBoundary then WhiteBoundarySize := 0;
         CreateBitmap(Bitmap,(Right - Left)+succ(WhiteBoundarySize * 2), (Bottom - Top)+succ(WhiteBoundarySize * 2));
         ClearBitmap(Bitmap,clNearWhite);
         Bitmap.Canvas.CopyRect(Rect(WhiteBoundarySize,WhiteBoundarySize,Bitmap.Width-WhiteBoundarySize,Bitmap.Height-WhiteBoundarySize),NewBitmap.Canvas,Rect(Left,Top,Right,Bottom));
         NewBitmap.Free;
      end;
   end;
   {$IfDef RecordGetImagePartOfBitmap} WriteLineToDebugFile('Final Bitmap size=' + IntToStr(Bitmap.width) + 'x' + IntToStr(Bitmap.height)); {$EndIf}
end;

{start of image rotation}
      //http://www.efg2.com/Lab/ImageProcessing/Unit7.TXT

      procedure SpiegelnHorizontal(Bitmap:tMyBitmap);
      var
        i,j,w :  INTEGER;
         RowIn :  pRGBArray;
         RowOut:  pRGBArray;
      begin
          w := bitmap.width*sizeof(TRGBTriple);
          Getmem(rowin,w);
          for j := 0 to Bitmap.Height-1 do begin
            move(Bitmap.Scanline[j]^,rowin^,w);
            rowout := Bitmap.Scanline[j];
            for i := 0 to Bitmap.Width-1 do rowout[i] := rowin[Bitmap.Width-1-i];
          end;
          bitmap.Assign(bitmap);
          Freemem(rowin);
      end;


      procedure SpiegelnVertikal(Bitmap : tMyBitmap);
      var
         j,w  : INTEGER;
         help : tMyBitmap;
      begin
          help := tMyBitmap.Create;
          help.Width       := Bitmap.Width;
          help.Height      := Bitmap.Height;
          help.PixelFormat := Bitmap.PixelFormat;
          w := Bitmap.Width*sizeof(TRGBTriple);
          for j := 0 to Bitmap.Height-1 do move(Bitmap.Scanline[j]^,Help.Scanline[Bitmap.Height - 1 - j]^,w);
          Bitmap.Assign(help);
          help.free;
      end;

      type
         THelpRGB = packed record
                      rgb    : TRGBTriple;
                      dummy  : byte;
                   end;

      procedure Drehen270Grad(Bitmap:tMyBitmap);
      var
          aStream : TMemorystream;
          header  : tBitmapINFO;
          dc      : hDC;
          P       : ^THelpRGB;
          x,y,b,h : Integer;
          RowOut:  pRGBArray;
      BEGIN
         aStream := TMemoryStream.Create;
         aStream.SetSize(Bitmap.Height*Bitmap.Width * 4);
         with header.bmiHeader do begin
           biSize := SizeOf(tBitmapINFOHEADER);
           biWidth := Bitmap.Width;
           biHeight := Bitmap.Height;
           biPlanes := 1;
           biBitCount := 32;
           biCompression := 0;
           biSizeimage := aStream.Size;
           biXPelsPerMeter := 1;
           biYPelsPerMeter := 1;
           biClrUsed := 0;
           biClrImportant :=0;
         end;
         dc := GetDC(0);
         P  := aStream.Memory;
         GetDIBits(dc,Bitmap.Handle,0,Bitmap.Height,P,header,dib_RGB_Colors);
         ReleaseDC(0,dc);
         b := bitmap.Height;  // rotate
         h := bitmap.Width;   // rotate
         bitmap.Width := b;
         bitmap.height := h;
         for y := 0 to (h-1) do begin
           rowOut := Bitmap.ScanLine[(h-1)-y];
           P  := aStream.Memory;        // reset pointer
           inc(p,y);
           for x := (b-1) downto 0 do begin
              rowout[x] := p^.rgb;
              inc(p,h);
           end;
         end;
         aStream.Free;
      end;


      procedure Drehen90Grad(Bitmap:tMyBitmap);
      var aStream : TMemorystream;
          header  : tBitmapINFO;
          dc      : hDC;
          P       : ^THelpRGB;
          x,y,b,h : Integer;
          RowOut:  pRGBArray;
      begin
         aStream := TMemoryStream.Create;
         aStream.SetSize(Bitmap.Height*Bitmap.Width * 4);
         with header.bmiHeader do begin
           biSize := SizeOf(tBitmapINFOHEADER);
           biWidth := Bitmap.Width;
           biHeight := Bitmap.Height;
           biPlanes := 1;
           biBitCount := 32;
           biCompression := 0;
           biSizeimage := aStream.Size;
           biXPelsPerMeter := 1;
           biYPelsPerMeter := 1;
           biClrUsed := 0;
           biClrImportant := 0;
         end;
         dc := GetDC(0);
         P  := aStream.Memory;
         GetDIBits(dc,Bitmap.Handle,0,Bitmap.Height,P,header,dib_RGB_Colors);
         ReleaseDC(0,dc);
         b := bitmap.Height;  // rotate
         h := bitmap.Width;   // rotate
         bitmap.Width := b;
         bitmap.height := h;
         for y := 0 to (h-1) do  begin
           rowOut := Bitmap.ScanLine[y];
           P  := aStream.Memory;        // reset pointer
           inc(p,y);
           for x := 0 to (b-1) do  begin
              rowout[x] := p^.rgb;
              inc(p,h);
           end;
         end;
         aStream.Free;
      end;


      procedure Drehen180Grad(Bitmap:tMyBitmap);
      var i,j     :  INTEGER;
          rowIn :  pRGBArray;
          rowOut:  pRGBArray;
          help  : tMyBitmap;
      begin
         help := tMyBitmap.Create;
         help.Width  := Bitmap.Width;
         help.Height := Bitmap.Height;
         help.PixelFormat := Bitmap.PixelFormat;    // only pf24bit for now
         FOR  j := 0 TO Bitmap.Height - 1 DO BEGIN
           rowIn  := Bitmap.ScanLine[j];
           rowOut := help.ScanLine[Bitmap.Height - j - 1];
           FOR i := 0 TO Bitmap.Width - 1 DO rowOut[Bitmap.Width - i - 1] := rowIn[i]
         END;
         bitmap.assign(help);
         help.free;
      end;


{From: "Jack Sudarev" <jack.sudarev@practel.com.au> To: <EarlGlynn@att.net>  Subject: Rotate bitmap  Date: Thursday, June 22, 2000 10:23 PM  "Rotate bitmap using scanline" }


function Ceil(x : float64) : integer;
begin
  Result := succ(Trunc(x));
end;


//Bilinear interpolation
function GetSmoothColor(iOriginal, jOriginal : Double; OriginalBitmap : tMyBitmap): TRGBTriple;
var
  f0, f1, f2, f3, iFrac, jFrac : Double;
  P0, P1, P2, P3: TRGBTriple;
  P : pRGBArray;
begin
    //Get fractional parts
    iFrac := Frac(iOriginal);
    jFrac := Frac(jOriginal);
    f0 := (1 - iFrac)*(1 - jFrac);
    f1 := iFrac*(1 - jFrac);
    f2 := iFrac*jFrac;
    f3 := (1 - iFrac)*jFrac;
    //Get surrounding points
    P := OriginalBitmap.ScanLine[Trunc(jOriginal)];
    P0 := P[Trunc(iOriginal)];
    P1 := P[Ceil(iOriginal)];
    P := OriginalBitmap.ScanLine[Ceil(jOriginal)];
    P2 := P[Trunc(iOriginal)];
    P3 := P[Ceil(iOriginal)];
    //Calculate result color
    Result.rgbtRed := Round(P0.rgbtRed*f0 + P1.rgbtRed*f1 + P2.rgbtRed*f2 + P3.rgbtRed*f3);
    Result.rgbtGreen := Round(P0.rgbtGreen*f0 + P1.rgbtGreen*f1 + P2.rgbtGreen*f2 + P3.rgbtGreen*f3);
    Result.rgbtBlue := Round(P0.rgbtBlue*f0 + P1.rgbtBlue*f1 + P2.rgbtBlue*f2 + P3.rgbtBlue*f3);
end;


//Rotate bitmap to any angle
function RotateBitmap(OriginalBitmap : tMyBitmap;  Angle : float64; AntiAliasing: Boolean; WhiteBackground : boolean = true) : tMyBitmap;
var
  i, j : Integer;
  iRotationAxis, jRotationAxis,
  iOriginal, jOriginal,
  iPrime, jPrime,Miss,
  iPrimeRotated, jPrimeRotated: Integer;
  Theta,cosTheta, sinTheta,
  iPrimeRotatedA, jPrimeRotatedA,
  iOriginalA, jOriginalA: Double;
  RowOriginal,RowRotated: pRGBArray;
begin
    if WhiteBackground then Miss := 255 else Miss := 0;
    if (OriginalBitmap.PixelFormat <> pf24bit) then OriginalBitmap.PixelFormat := pf24bit; // force to 24 bits
    try
      // The size of BitmapRotated is same as BitmapOriginal.  PixelFormat
      // must also match since 24-bit GBR triplets are assumed in ScanLine.
      CloneBitmap(OriginalBitmap,Result);
      Result.Canvas.Pen.Color := Miss;
      Result.Canvas.Brush.Color := Miss;
      Result.Canvas.Brush.Style := bsSolid;
      Result.Canvas.Rectangle(0,0,Result.Width,Result.Height);

      // Axis of rotation is normally center of image
      iRotationAxis := Result.Width div 2;
      jRotationAxis := Result.Height div 2;

      // Convert degrees to radians.  Use minus sign to force clockwise rotation.
      Theta := (-Angle) * Pi/180;
      sinTheta := Sin(Theta);
      cosTheta := Cos(Theta);

      // Step through each row of rotated image.
      for j := Result.Height - 1 downto 0 do begin
        RowRotated := Result.Scanline[j];
        {
          Assume the bitmap has an even number of pixels in both dimensions and
          the axis of rotation is to be the exact middle of the image -- so this
          axis of rotation is not at the middle of any pixel.

          The transformation (i,j) to (iPrime, jPrime) puts the center of each
          pixel at odd-numbered coordinates.  The left and right sides of each
          pixel (as well as the top and bottom) then have even-numbered coordinates.

          The point (iRotationAxis, jRotationAxis) identifies the axis of rotation.

          For a 640 x 480 pixel image, the center point is (320, 240).  Pixels
          numbered (index i) 0..319 are left of this point along the "X" axis and
          pixels numbered 320..639 are right of this point.  Likewise, vertically
          pixels are numbered (index j) 0..239 above the axis of rotation and
          240..479 below the axis of rotation.

          The subtraction (i, j) - (iRotationAxis, jRotationAxis) moves the axis of
          rotation from (i, j) to (iRotationAxis, jRotationAxis), which is the
          center of the bitmap in this implementation.
        }

        jPrime := 2*(j - jRotationAxis) + 1;

        for i := Result.Width - 1 downto 0 do begin

          iPrime := 2*(i - iRotationAxis) + 1;

          // Rotate (iPrime, jPrime) to location of desired pixel
          // Note:  There is negligible difference between floating point and
          // scaled integer arithmetic here, so keep the math simple (and readable).
          if not AntiAliasing then begin
            iPrimeRotated := Round(iPrime * CosTheta - jPrime * sinTheta);
            jPrimeRotated := Round(iPrime * sinTheta + jPrime * cosTheta);

            // Transform back to pixel coordinates of image, including translation
            // of origin from axis of rotation to origin of image.
            iOriginal := (iPrimeRotated - 1) div 2 + iRotationAxis;
            jOriginal := (jPrimeRotated - 1) div 2 + jRotationAxis;

            // Make sure (iOriginal, jOriginal) is in BitmapOriginal.  If not,
            // assign white color to corner points.
            if (iOriginal >= 0) and (iOriginal <= OriginalBitmap.Width - 1) and
               (jOriginal >= 0) and (jOriginal <= OriginalBitmap.Height - 1) then begin
              // Assign pixel from rotated space to current pixel in BitmapRotated
              RowOriginal := OriginalBitmap.Scanline[jOriginal];
              RowRotated[i] := RowOriginal[iOriginal];
            end
            else begin
              RowRotated[i].rgbtBlue := Miss; // assign "corner" color
              RowRotated[i].rgbtGreen := Miss;
              RowRotated[i].rgbtRed := Miss;
            end;
          end
          else begin //Antialiasing is On

            iPrimeRotatedA := iPrime * CosTheta - jPrime * sinTheta;
            jPrimeRotatedA := iPrime * sinTheta + jPrime * cosTheta;

            // Transform back to pixel coordinates of image, including translation
            // of origin from axis of rotation to origin of image.
            iOriginalA := (iPrimeRotatedA - 1)/2 + iRotationAxis;
            jOriginalA := (jPrimeRotatedA - 1)/2 + jRotationAxis;

            // Make sure (iOriginal, jOriginal) is in BitmapOriginal.  If not,
            // assign white color to corner points.
            if (iOriginalA >= 0) and (iOriginalA <= OriginalBitmap.Width - 1) and
               (jOriginalA >= 0) and (jOriginalA <= OriginalBitmap.Height - 1) then begin
              // Assign pixel from rotated space to current pixel in BitmapRotated
              RowRotated[i] := GetSmoothColor(iOriginalA, jOriginalA, OriginalBitmap);
            end
            else begin
              RowRotated[i].rgbtRed := Miss;
              RowRotated[i].rgbtGreen := Miss;
              RowRotated[i].rgbtBlue := Miss; // assign "corner" color
            end;
          end;
        end;//for i
      end;//for j
    finally
    end;
end;

procedure RecolorBitmap(var Bitmap : Pathstr; Color : tPlatFormColor);
var
   bmp : tMyBitmap;
begin
   bmp := tMyBitmap.Create;
   bmp.LoadFromFile(Bitmap);
   RecolorBitmap(bmp,Color);
   PetImage.SaveBitmap(bmp,Bitmap);
   bmp.free;
end;


procedure RecolorBitmap(var Bitmap : tMyBitmap; Color : tPlatformColor);
{any non-white pixel will be recolored to the specified color}
var
   x,y : integer;
   p0 : prgb;
begin
   for y := 0 to pred(Bitmap.Height) do begin
      p0 := Bitmap.Scanline[y];
      for x := 0 to pred(Bitmap.Width) do begin
         if (not SameColor(P0[x],RGBTripleWhite)) then p0[x] := Color;
      end;
   end;
end;


function BlendBitmaps(src1,src2 : tMyBitmap; amount : extended) : tMyBitmap;
begin
   CreateBitMap(Result,src1.width,src1.Height);
   Result.Canvas.Draw(0,0,src1);
   BlendBitmapAtop(Result,src2,amount);
end;


procedure BlendBitmapAtop(var src1 : tMyBitmap; Overlay : tMyBitmap; amount : extended);
var
   w,h,x,y :integer;
   MemBase,MemOverlay : tBmpMemory;
   r1,g1,b1,r2,g2,b2 : byte;
begin
   if (Overlay = Nil) or (src1 = Nil) then exit;
   {$IfDef RecordBlendBitmaps}
      WriteLineToDebugFile('BlendBitmaps,  amount=' + RealToString(Amount,-12,2) + '  Source: ' + BitmapSize(src1) + '  Overlay: ' + BitmapSize(Overlay));
      src1.SaveToFile(MDtempDir + 'blend_source.bmp');
      Overlay.SaveToFile(MDtempDir + 'blend_overlay.bmp');
   {$EndIf}

   w := src1.Width;
   h := src1.Height;

   //if overlay is smaller, only try to overlay in upper left portion of source
   if Overlay.Width < w then w := Overlay.Width;
   if Overlay.Height < h then h := Overlay.Height;

   MemBase    := tBmpMemory.Create(Src1);
   MemOverlay := tBmpMemory.Create(Overlay);

   for y := 0 to (h-1) do begin
       for x := 0 to (w-1) do begin
          if not MemOverlay.SameColor(x,y,rgbTripleWhite) then begin
             MemBase.GetPixelRGB(x,y,r1,g1,b1);
             MemOverlay.GetPixelRGB(x,y,r2,g2,b2);
             r1 := ValidByteRange(r1 + round(amount * (r2-r1)));
             g1 := ValidByteRange(g1 + round(amount * (g2-g1)));
             b1 := ValidByteRange(b1 + round(amount * (b2-b1)));
             MemBase.SetPixelRGB(x,y,r1,g1,b1);
          end;
      end;
   end;
   MemBase.Destroy;
   MemOverlay.Destroy;
   {$IfDef RecordBlendBitmaps} src1.SaveToFile(MDtempDir + 'blend_result.bmp');   {$EndIf}
end;


procedure MakeBitmapThumbnail(var inBMP : tMyBitmap; tnHeight : integer);
begin
   if tnHeight < inBMP.Height then begin
      inBMP.Canvas.StretchDraw(Rect(0,0,round(tnHeight / inbmp.Height *inbmp.Width),tnHeight),inBMP);
      inBMP.Width := round(tnHeight / inbmp.Height * inbmp.Width);
      inBMP.Height := tnHeight;
   end;
end;


procedure CreateThumbNail(fname,OutName : PathStr; tnHeight,tnQuality : integer; aLabel : ShortString = '');
var
   inBMP,outBMP : tMyBitmap;
begin
   inBMP := LoadBitmapFromFile(fName);
   OutBMP := CreateThumbNailBMP(inBMP,tnHeight);
   if (aLabel <> '') then begin
      OutBMP.Canvas.Font.Size := 14;
      OutBMP.Canvas.Font.Color := clRed;
      OutBMP.Canvas.TextOut(1,1,aLabel);
   end;
   SaveBitmap(OutBMP,OutName);
   OutBMP.Free;
   InBMP.Free;
end;

{$EndIf}


{$IfDef VCL}

function GetGraphicsFileName(WhatFor : shortstring; var fName : PathStr) : boolean;
begin
   if WhatFor = '' then WhatFor := 'Image';
   
   Result := Petmar.GetFileFromDirectory(WhatFor, AllowedGraphicsFilters,fName);
end;


function GetNewGraphicsFileName(WhatFor : shortstring; var fName : PathStr) : boolean;
begin
   Result := Petmar.GetFileNameDefaultExtSaveExt(WhatFor,GraphicsFilters,FName,MDdef.DefaultSaveImageType,false);
end;


function GetNew3DGraphicsFileName(WhatFor : shortstring; var fName : PathStr) : boolean;
var
   aType : integer;
begin
   aType := 1;
   Result := Petmar.GetFileNameDefaultExtSaveExt(WhatFor,ThreeDeeGraphicsFilters,FName,aType,false);
end;


procedure ConvertMyBitmapToJPEG(var FName : PathStr);
var
   MyBMP  : tMyBitmap;
begin
   MyBMP := LoadBitmapFromFile(fName);
   SaveBitmap(MyBMP,ChangeFileExt(fName,'.jpg'));
   MyBMP.Free;
   DeleteFileIfExists(FName);
end;


procedure SaveImageAsThumbnail(Image1 : tImage; FName : PathStr; ThumbNailHeight : integer);
var
   MyBMP,NewBMP : tMyBitmap;
begin
   CopyImageToBitmap(Image1,MyBmp);
   CreateBitmap(NewBMP,round(MyBMP.Width * ThumbNailHeight / MyBMP.Height),ThumbNailHeight);
   NewBMP.Canvas.StretchDraw(Rect(0,0,NewBMP.Width,NewBMP.Height),MyBMP);
   SaveBitmap(NewBMP,fName);
   NewBMP.Free;
   MyBMP.Free;
end;


procedure CloneImageToBitmap(Image1 : tImage; var CloneBitmap : tMyBitmap; MakeItBlack : boolean = false);
begin
   CreateBitmap(CloneBitmap,Image1.Width,Image1.Height);
   if MakeItBlack then begin
      CloneBitmap.Canvas.Pen.Color := clBlack;
      CloneBitmap.Canvas.Brush.Color := clBlack;
      CloneBitmap.Canvas.Brush.Style := bsSolid;
      CloneBitmap.Canvas.Rectangle(0,0,pred(Image1.Width),pred(Image1.Height));
   end;
end;


function CopyImageToBitmap(var Image1 : tImage; var Bitmap : tMyBitmap; ClearWhite : boolean = false) : boolean;
var
  MenuStr : shortstring;
begin
   Result := false;
   if (Image1 <> Nil) and (Image1.Picture.Graphic <> Nil) then begin
      try
         {$IfDef RecordBitmapProblems} WriteLineToDebugFile('CopyImageToBitmap  Image dimensions:' + ImageHeight(Image1) + '  Picture dimensions:' + IntToStr(Image1.Picture.Graphic.Width) + 'x' + IntToStr(Image1.Picture.Graphic.Height));         {$EndIf}
         PetImage.CreateBitmap(Bitmap,Image1.Picture.Graphic.Width,Image1.Picture.Graphic.Height);
         Bitmap.Canvas.Draw(0,0,Image1.Picture.Graphic);
         if ClearWhite then begin
            GetImagePartOfBitmap(Bitmap);
         end;
         Result := true;
      except
         MenuStr := 'Bitmap too large to copy (' + ImageSize(Image1) + ')';
         MessageToContinue(MenuStr);
         {$IfDef RecordBitmapProblems} WriteLineToDebugFile(MenuStr); {$EndIf}
         Bitmap.Free;
         Bitmap := nil;
      end;
   end;
end;


procedure AssignImageToClipBoard(Image1 : tImage);
begin
   Clipboard.Assign(Image1.Picture);
end;


procedure SaveImageAsBMP(Image1 : tImage; SaveName : PathStr = ''); overload;
var
   MyBMP : tMyBitmap;
begin
   if CopyImageToBitmap(Image1,MyBMP) then begin
      SaveBitmap(MyBMP,SaveName);
      MyBMP.Free;
   end
end;


procedure LoadBitmapInImage(Image1 : tImage; fName : PathStr);
var
   BMP : tMyBitmap;
begin
   BMP := LoadBitmapFromFile(fName);
   Image1.Picture.Graphic := BMP;
   BMP.free;
end;

{$EndIf}


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


{$IfDef VCL}

procedure SkeletonizeBitmap(var Bitmap : tMyBitmap);
//http://www.planet-source-code.com/vb/scripts/ShowCode.asp?txtCodeId=1217&lngWId=7
//code by Peter Bone, an English PhD student in image processing (in 8/20/2003)
type
  TRGBTripleArray = array[0..5000] of RGBTriple;
  PRGBTripleArray = ^TRGBTripleArray;
  TPoints = array of TPoint;
Var
  LScan : PRGBTripleArray;
  Lx, Ly : integer;
  LPoints : TPoints;

    // return offset of neighbouring point
    function Neighbour(ANeighbour : byte) : TPoint;
    begin
      ANeighbour := (ANeighbour - 2) mod 8 + 2; // neighbourhood wrap-round
      Case ANeighbour of
        2 : begin Result.X :=  0; Result.Y := -1; end; //    ___________
        3 : begin Result.X :=  1; Result.Y := -1; end; //   | 9 | 2 | 3 |
        4 : begin Result.X :=  1; Result.Y :=  0; end; //   |___|___|___|
        5 : begin Result.X :=  1; Result.Y :=  1; end; //   | 8 | 1 | 4 |
        6 : begin Result.X :=  0; Result.Y :=  1; end; //   |___|___|___|
        7 : begin Result.X := -1; Result.Y :=  1; end; //   | 7 | 6 | 5 |
        8 : begin Result.X := -1; Result.Y :=  0; end; //   |___|___|___|
        9 : begin Result.X := -1; Result.Y := -1; end; //
      end;
    end;


    // thin binary image to a single pixel width skeleton by erosion
    procedure Skeletonize(var Points : TPoints ; AWidth, AHeight : integer);
    Var
      Lx, Ly, Lindex,lCount : integer;
      LB, LA, LN, LAN6, LAN4 : byte;
      LNOff : TPoint;
      LChanging : boolean;
      LRemove : array of boolean;
      LImage : array of array of byte;
    begin
      // create a 2D image from the points - LImage array
      // the LImage array will be larger than the actual image
      // and offset by 1 in x and y to prevent access violations
      // when accessing neighbouring points to those at the edge of the image
      SetLength(LImage, AWidth+3);
      for Lx := 0 to AWidth + 2 do begin
        SetLength(LImage[Lx], AHeight+3);
        for Ly := 0 to AHeight + 2 do  begin
          LImage[Lx][Ly] := 0;
        end;
      end;
      for Lindex := 0 to High(Points) do LImage[Points[Lindex].X+1][Points[Lindex].Y+1] := 1;

      SetLength(LRemove, length(Points));

      LChanging := True;
      LCount := 0;
      while LChanging do begin // stop if pixels are no longer being removed
        inc(LCount);
        LChanging := False;
        StartProgress('Skeletonize, loop ' + IntToStr(LCount));
        // loop through all points in the binary object
        for Lindex := 0 to High(Points) do begin
          if Lindex mod 100 = 0 then UpdateProgressBar(Lindex/High(Points));

          LRemove[Lindex] := False;

          Lx := Points[Lindex].X + 1;
          Ly := Points[Lindex].Y + 1;

          // calculate B, the sum of non-zero neighbours
          LB := 0;
          for LN := 2 to 9 do begin
            LNOff := Neighbour(LN);
            Inc(LB, LImage[Lx+LNOff.X][Ly+LNOff.Y]);
          end;

          if (LB < 2) or (LB > 6) then Continue; // failed B removal test

          // calculate A, the number of 0 -> 1 patterns around the neighbourhood
          LA := 0;
          for LN := 2 to 9 do begin
            LNOff := Neighbour(LN);
            if LImage[Lx+LNOff.X][Ly+LNOff.Y] = 0 then begin
              LNOff := Neighbour(LN+1);
              if LImage[Lx+LNOff.X][Ly+LNOff.Y] = 1 then Inc(LA);
            end;
            if LA > 1 then Break; // fails if LA <> 1 so no need to continue
          end;

          if LA <> 1 then Continue; // failed A removal test

          // calculate the A value for neighbour 6
          LAN6 := 0;
          for LN := 2 to 9 do begin
            LNOff := Neighbour(LN);
            if LImage[Lx+LNOff.X][Ly+LNOff.Y+1] = 0 then begin
              LNOff := Neighbour(LN+1);
              if LImage[Lx+LNOff.X][Ly+LNOff.Y+1] = 1 then Inc(LAN6);
            end;
          end;

          if (LImage[Lx][Ly+1] * LImage[Lx+1][Ly] * LImage[Lx-1][Ly] <> 0) and (LAN6 = 1) then Continue; // failed test

          // calculate the A value for neighbour 4
          LAN4 := 0;
          for LN := 2 to 9 do begin
            LNOff := Neighbour(LN);
            if LImage[Lx+LNOff.X+1][Ly+LNOff.Y] = 0 then begin
              LNOff := Neighbour(LN+1);
              if LImage[Lx+LNOff.X+1][Ly+LNOff.Y] = 1 then Inc(LAN4);
            end;
          end;

          // final removal test
          if (LImage[Lx][Ly-1] * LImage[Lx+1][Ly] * LImage[Lx][Ly+1] = 0) or (LAN4 <> 1) then begin
            // erosion cannot be done sequencially - so flag all points that
            // must be removed to remove them all at once after each pass
            LRemove[Lindex] := True;
            LChanging := True; // still removing pixels so continue for another pass
          end;
        end; // loop through points

        // remove the points that are flagged in the LRemove array
        if LChanging then begin
          Lindex := 0;
          while Lindex < length(Points) do begin
            if LRemove[Lindex] then begin
              // set value of image for this point to zero
              LImage[Points[Lindex].X+1][Points[Lindex].Y+1] := 0;
              // remove point from points list by moving the last element to the
              // points position and then reducing the length of the array by 1
              Points[Lindex] := Points[High(Points)];
              SetLength(Points, High(Points));
              // must do the same for this element in the LRemove array
              LRemove[Lindex] := LRemove[High(LRemove)];
              SetLength(LRemove, High(LRemove));
            end else Inc(Lindex);
          end;
        end;
      end; // passes loop
      EndProgress;
    end;


begin
  // create a list of points from the image
  SetLength(LPoints, 0);
  for Ly := 0 to Bitmap.Height - 1 do  begin
    LScan := Bitmap.ScanLine[Ly];
    for Lx := 0 to Bitmap.Width - 1 do  begin
      if LScan[Lx].rgbtBlue = 0 then begin
        SetLength(LPoints, length(LPoints) + 1);
        LPoints[High(LPoints)] := Point(Lx, Ly);
      end;
    end;
  end;

  // skeletonize list of points
  Skeletonize(LPoints, Bitmap.Width, Bitmap.Height);

  // draw skeleton back on image from skeletonized points list
  RecolorBitmap(Bitmap,claWhite);
  for Ly := 0 to High(LPoints) do  begin
    Bitmap.Canvas.Pixels[LPoints[Ly].X, LPoints[Ly].Y] := clblack;
  end;
end;


procedure BitmapNonWhiteToBlack(var Bitmap : tMyBitmap);
var
   x,y : integer;
   p1 : PRGB;
begin
   for y := 0 to pred(Bitmap.Height) do begin
      P1 := BitMap.ScanLine[y];
      for x := 0 to pred(Bitmap.Width) do begin
         if not SameColor(p1[x],RGBTripleWhite) then p1[x] := RGBTripleBlack;
      end;
   end;
end;



procedure MakeBitmapGreen(var BMP : tMyBitmap);
var
   x,y : integer;
   P0  : pRGB;
begin
   for y := 0 to pred(BMP.Height) do begin
      p0 := BMP.Scanline[y];
      for x := 0 to pred(BMP.Width) do begin
         P0[x].rgbtRed := 0;
         P0[x].rgbtBlue := 0;
      end;
   end;
end;


procedure MakeBitmapStretchedGrayscale(Bitmap : tMyBitmap);
const
   MinGray : byte = 75;
   MaxGray : byte = 200;
var
   x,y,j : integer;
   P0    : pRGB;
begin
   StartProgress('Recolor');
   ReadDefault('Min gray',MinGray);
   ReadDefault('Max gray',MaxGray);
   for y := 0 to pred(Bitmap.Height) do  begin
      p0 := Bitmap.Scanline[y];
      if y mod 25 = 0 then UpdateProgressBar(y/Bitmap.Height);
      for x := 0 to pred(Bitmap.Width) do  begin
         j := round(0.3 * P0[x].rgbtRed  + 0.59 * P0[x].rgbtGreen  + 0.11 * P0[x].rgbtBlue );
         j := ValidByteRange(MinGray + (MaxGray - MinGray) * j div 255);
         P0[x].rgbtRed := j;
         P0[x].rgbtGreen := j;
         P0[x].rgbtBlue := j;
      end;
   end;
   EndProgress;
end;


function CreateProtractor(Double,ShallowAngles : boolean; FormVertExag : float64) : tMyBitmap;
var
   i,StartX,StartY,LastEndY : integer;

   procedure DrawLine(InAngle : integer);
   var
      EndX,EndY,End2 : integer;
      Angle : float64;
      TStr : ShortString;
   begin
      Angle := ArcTan( SinDeg(InAngle) * FormVertExag / CosDeg(InAngle)) / DegToRad;
      EndX := StartX + round(100 * CosDeg(Angle));
      End2 := StartX - round(100 * CosDeg(Angle));
      EndY := StartY + round(100 * SinDeg(Angle));
      if (EndY > LastEndY + 15) then begin
         Result.Canvas.MoveTo(StartX,StartY);
         Result.Canvas.LineTo(EndX,EndY);
         TStr := IntToStr(InAngle) + DegSym;
         Result.Canvas.TextOut(EndX+5,EndY,TStr);
         if Double then begin
            Result.Canvas.MoveTo(StartX,StartY);
            Result.Canvas.LineTo(End2,EndY);
            Result.Canvas.TextOut(End2-5-Result.Canvas.TextWidth(TStr),EndY,TStr);
         end;
         LastEndY := EndY;
      end;
   end;

begin
   if Double then  begin
      i := 2;
      StartX := 150;
    end
    else begin
       i := 1;
       Startx := 0;
    end;

   CreateBitmap(Result,i * 150,120);
   StartY := 0;
   LastEndY := -99;
   Result.Canvas.Pen.Color := clBlack;
   Result.Canvas.Pen.Width := 1;
   Result.Canvas.Font.Name := MDDef.DefaultGraphFont.Name;
   Result.Canvas.Font.Size := MDDef.DefaultGraphFont.Size;
   Result.Canvas.Font.Style := [fsBold];
   DrawLine(0);
   if ShallowAngles then begin
      DrawLine(1);
      DrawLine(2);
   end;
   DrawLine(5);
   DrawLine(10);
   DrawLine(20);
   DrawLine(30);
   if (not ShallowAngles) then begin
      DrawLine(40);
      DrawLine(50);
      DrawLine(60);
   end;
end;
{$EndIf}


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
      p1[y]^[x].rgbtGreen :=  Value;
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
   {$IfDef RecordPNG} WriteLineToDebugFile('RecordPNG active in PetImage');{$EndIf}
   {$IfDef RecordBitmapProblems} WriteLineToDebugFile('RecordBitmapProblems active in PetImage'); {$EndIf}
   {$IfDef RecordGetImagePartOfBitmap} WriteLineToDebugFile('RecordGetImagePartOfBitmap active in PetImage'); {$EndIf}
   {$IfDef RecordDipStrike} WriteLineToDebugFile('RecordDipStrike active in PetImage'); {$EndIf}
   {$IfDef RecordFullPalette} WriteLineToDebugFile('RecordFullPalette active in PetImage'); {$EndIf}
   {$If Defined(RecordBitmapExt)} WriteLineToDebugFile('close petimage, default ext=' + IntToStr(MDdef.DefaultSaveImageType)); {$EndIf}

   {$IfDef MessageShutdownUnitProblems} MessageToContinue('Closing petimage'); {$EndIf}
end.
















