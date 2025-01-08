{$N+,E+,F+}

unit PETMATH;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{________________________________________}



{$I nevadia_defines.inc}


//{$Def IncludeFriedman}  //this is not currently working.  It is for DEMIX, which uses the Jupyter notebook

{$IfDef RecordProblems} //normally only defined for debugging specific problems
   //{$Define ExInlineConversions}
   //{$Define RecordFitProblems}
   //{$Define TrackNegStdDev}
   //{$Define TrackNAN}
   //{$Define RecordCovar}
   //{$Define RecordMatrixOps} //can really degrade performance
{$EndIf}

interface

uses
//needed for inline of the core DB functions
   //Petmar_db,
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
//end needed for inline of the core DB functions
   {$IfDef VCL}
      Graphics, VCL.Grids,
   {$EndIf}

   {$IfDef FMX}
      FMX.Graphics,
   {$EndIf}

   Types,Math,Classes,Sysutils,

   Petmar_types;


function HeadingOfLine(dx,dy : float64) : float64; inline;

//trig functions
      function CosDeg(angle : float64) : float64; inline;
      function SinDeg(angle : float64) : float64; inline;
      function TanDeg(angle : float64) : float64; inline;
      function CotDeg(angle : float64) : float64; inline
      function ATan2(a1,a2 : float64) : float64;  inline; {result in radians}
      function ArcCos(Angle : float64) : float64;
      function ArcSin(x : float64) : float64;
      function TanH(x : float64) : float64;
      function SinH(x : float64) : float64;
      function CosH(x : float64) : float64;

//dates
      function AnnualJulianDay(Year,Month,Day : integer) : integer;
      procedure JulianDay(JDay,Year : integer; var Month,Day : integer);
      function JulDay(Month,Day,Year : Integer) : LongInt;  {after Press and others, 1986, Numerical Recipes, Cambridge University Press}
      procedure CalDat(julian : LongInt; var Month,Day,Year: integer); {after Press and others, 1986, Numerical Recipes, Cambridge University Press}
      function JulianDayToCalendarDate(julian : LongInt) : shortstring;


function Log10(x : float32) : float32;

function erfc(x : float64): float64;
function erf(x : float64) : float64;
function Ninv( P : float32 ) : float32;
function IsInfinity(const d: double) : boolean;
function ArcTanDeg(val : float64) : float64;

procedure TransformCoordinates(xin,yin : float64; var xout,yout : float64; XlateArray : OneBySixFloatArray);
procedure CalculateTransformationMatrix(dx1,dy1,dx2,dy2,dx3,dy3,ux1,uy1,ux2,uy2,ux3,uy3 : float64;  var XlateArray : OneBySixFloatArray);

procedure HeapSort(n: integer; var ra: array of float64); overload;  {after Press and others, 1986, Numerical Recipes, Cambridge University Press}
procedure HeapSort(n: integer; var ra: array of float32); overload;

procedure SortStringListNumerically(var SL : tStringList);


{ SOLUTION OF N SIMULTANEOUS EQUATIONS. A IS N X N , B IS COLUMN VECTOR OF N ELEMENTS.
  A CONVERTED TO IDENTITY MATRIX. B CONTAINS SOLUTION. }

procedure SLE(var A : tTrendMatrix; var B : tTrendVector; N : integer;  Zero : float64);
procedure Jacobi(var a : tTrendMatrix; n : integer; var d : tTrendVector; var v : tTrendMatrix; var nrot : integer);
procedure Eigsrt(var d : tTrendVector; VAR v : tTrendMatrix; n: integer);

procedure InterpolateProfile(NumPairs : integer; var y,z : Array of float64; NumNewPairs : integer; var yn : Array of float64; var zn : Array of float64);
{call with yn set for the new distances along the profile, and return zn with the new elevations}
procedure InterpolateProfileGapCheck(NumPairs : integer; var y,z : Array of float32; NumNewPairs : integer; var yn : Array of float32; var zn : Array of float32;  MaxGap : float64);

procedure LongitudeAngleInRange(var Angle : float64); inline;
function CompassAngleToMathAngle(Angle : float64) : float64; {$IfDef InlineConversions} inline; {$EndIf}

function CompassAngleInRangeFloat64(Angle : float64) : float64; {$IfDef ExInlineConversions} {$Else} inline; {$EndIf}
function CompassAngleInRangeFloat32(Angle : float32) : float32; {$IfDef ExInlineConversions} {$Else} inline; {$EndIf}
function CompassAngleInRangeInteger(Angle : integer) : integer; {$IfDef ExInlineConversions} {$Else} inline; {$EndIf}

function AngularDistance(Angle1,Angle2 : float64) : float64;
function AngularDiff(Angle1,Angle2 : float64) : float64;

function AzimuthToDirection(Azimuth : float64) : ShortString;

function CompassAngleToRadians(Angle : float64) : float64; inline; {compass angles run clockwise from north; the computer uses radians running counterclockwise from east   }

procedure PlaneEquationFromPointAndNormal(x1,y1,z1 : float32; Normal: VectorType;  var XC,YC,ZC,Constant : float32);
procedure PlaneEquationFromThreePoints(var x1,y1,z1,x2,y2,z2,x3,y3,z3 : float32;  var XC,YC,ZC,Constant : float32);
procedure FindVectorNormalToPlane(DipDirect,Dip : float64; var N : VectorType);
function NearLine(const Target, Point1, Point2 : TPoint) : boolean;

procedure LatLongToCartesian(Lat,Long : float64; var A : VectorType);  {converts lat-long pair (in radians) to cartesian vector; conventions from p.110, Cox & Hart}
procedure CartesianToLatLong(A : VectorType; var Lat,Long : float64);  {converts cartesian vector representing point into lat-long pair; conventions p.110, Cox & Hart} {supplements arc tan to get correct quadrant}
procedure RotatePoint(R : MatrixType; var Init,Final : VectorType);
procedure RotationMatrix(PLat,PLong,Omega : float64; var R : MatrixType);  {calculate Rotation Matrix from pole and angular rotation, all in radians}

function ZeroSeconds(Value : float64) : boolean;

procedure DegreesToDegMinSec(Degrees : float64; var Deg,Min : integer; var Sec : float64);

function Median(var x : array of float32; n: integer; AlreadySorted : boolean = false) : float64; overload;
function Median(var x : array of float64; n: integer; AlreadySorted : boolean = false) : float64; overload;
function Mode(var x : array of float32; n: integer; binsize : float32) : float32;
function Percentile(PC : float64; var x: array of float32; n : integer; AlreadySorted : boolean = false) : float64; {after Press and others, 1986, Numerical Recipes, Cambridge University Press}

function Quantile(QWant : integer; var x : array of float64; n: integer; AlreadySorted : boolean = false) : float64; overload;
function Quantile(QWant : integer; var x : array of float32; n: integer; AlreadySorted : boolean = false) : float64; overload;


const
  MomentStr = 'NAME,MIN,PC1,PC2,PC5,Q1,MEDIAN,MEAN,Q3,PC95,PC98,PC99,MAX,AVG_DEV,STD_DEV,SKEWNESS,CURTOSIS,NPTS';


procedure Moment(var data : array of float32; var MomentVar : tMomentVar; MomentStop : tMomentStop);
procedure InitializeMomentVar(var MomentVar : tMomentVar);
procedure MomentsToStringGrid(StringGrid : tStringGrid; var OnLine,OnColumn : integer; Variable : shortString; MomentVar : tMomentVar; LongVersion : boolean = false);
procedure MomentReport(Variable : shortString; var data : bfarray32; n : integer; Title : ShortString = ''; StringGrid : tStringGrid = nil; OnLine : integer = -99; OnCol : integer = -99);
function MomentResultsToString(MomentVar : tMomentVar) : shortstring;
function ShortMomentResultsToString(MomentVar : tMomentVar) : shortstring;


procedure VarCovar(var x,y : array of float32; NPts : integer; var correlation,covar : float64);
procedure NewVarCovar(var x,y : array of float32; NPts : integer; var correlation,covar,Mean1,Mean2,StdDev1,StdDev2 : float64);


procedure Fit(var x,y : array of float32; ndata : integer; var a,b,siga,sigb,r : float32);
procedure FitOnRange(var x,y : array of float32; FirstPoint,LastPoint : integer; var ndata : integer; VAR a,b,siga,sigb,r : float32);

function VectorAverage(Num : integer; var Readings : farray; var Mag : float64) : float64;

function FracDimFromSlope1(Slope : float64) : float64;     {MEM,FFT}
function FracDimFromSlope2(Slope : float64) : float64;     {Variogram}

function DegFtoC(F : float64) : float64;
function DegCtoF(C : float64) : float64;

function GetTickInt(PixelsHigh,LabelSpacing : integer; ElevRange : float64) : float64;

function MinInArray(N : integer; vals : array of float64) : float64; overload;
function MaxInArray(N : integer; vals : array of float64) : float64; overload;
function MaxInArray(N : integer; vals : array of float32) : float64; overload;
function MinInArray(N : integer; var vals : bfarray32) : float64; overload;
function MaxInArray(N : integer; var vals : bfarray32) : float64; overload;

procedure MedianFilterArray(var Values : bfarray32; NPts,FilterSize : integer);
procedure MeanFilterArray(var Values : bfarray32; NPts,FilterSize : integer);

function MinFloat(x1,x2 : float64; x3 : float64 = 99e39; x4 : float64 = 99e39) : float64;
function MaxFloat(x1,x2 : float64; x3 : float64 = -99e39; x4 : float64 = -99e39) : float64;
procedure MinOfPairFirst(var v1,v2 : float64); overload;
procedure MinOfPairFirst(var v1,v2 : integer); overload;
procedure SwapPair(var v1,v2 : float64); overload;
procedure SwapPair32(var v1,v2 : float32);
procedure SwapPair(var v1,v2 : integer); overload;

procedure CompareValueToExtremes(value : float64; var MinV,MaxV : float64); inline; overload;
procedure CompareValueToExtremes(value : float32; var MinV,MaxV : float32); inline; overload;
procedure CompareValueToExtremes(value : integer; var MinV,MaxV : integer); inline; overload;

procedure ValueInRange(var n : integer; Min,Max : integer); inline; overload;
procedure ValueInRange(var n : float64; Min,Max : float64); inline; overload;
procedure ValueInRange(var n : float32; Min,Max : float32); inline; overload;
procedure ValueInRange(var n : word; Min,Max : word); inline; overload;

function AddDayMonthLeadingZero(Value : integer) : ShortString;

function IsBitSet(const val: word; const TheBit: Byte): Boolean;
function ClipValue(l,min,max : integer) : integer; inline;

function MissingNumericString(TStr : shortString) : boolean;

function IsLeapYear(Year : integer) : boolean;
function YearLength(Year : integer) : integer;
Function IsNumeric(s: AnsiString) : Boolean;

function RadToDegString(rads : float64) : shortstring;
function FilterSizeStr(BoxSize : integer) : shortstring;

{$IfDef IncludeFriedman}
   function Friedman(DBonTable : integer; DEMs : tStringList; Alpha : float64 = 95) : boolean;
{$EndIf}

function bicuint(y,y1,y2,y12: tElevFloatarray; x1l,x1u,x2l,x2u : integer; x1,x2: float32;  VAR ansy,ansy1,ansy2: float32) : boolean;

var
   ArrowEndX,ArrowEndY : Integer;

var
   SearchDir : PathStr;
   InputFileName : ShortString;

{$IfDef VCL}
   procedure GetFile(Message : shortstring; var LengthShort : integer;  var Series1 : bfarray32; var ValueMin,ValueMax : float64; fName : PathStr = '');
{$EndIf}

function EnoughPoints(MomentVar : tMomentVar) : boolean;
procedure LoadBFarray32(fName : PathStr; var Values : bfarray32; var npts : integer);


implementation


uses
  Petmar,DEMDataBase;

var
   LabelLength : integer;


function IsInfinity(const d : double): boolean;
// Like NaN, an INF Double value has an exponent of 7FF, but the INF
// values have a fraction field of 0. INF values can be positive or
// negative, which is specified in the high-order, sign bit.
VAR
   Overlay : Int64 ABSOLUTE d;
BEGIN
  RESULT := (Overlay AND $7FF0000000000000) = $7FF0000000000000;
END {IsInfinity};


{$IfDef IncludeFriedman}
   {$I stats_petmath.inc}
{$EndIf}


function FilterSizeStr(BoxSize : integer) : shortstring;
begin
   Result := IntToStr(BoxSize) + 'x' + IntToStr(BoxSize);
end;


function EnoughPoints(MomentVar : tMomentVar) : boolean;
begin
   Result := (MomentVar.NPts/(MomentVar.NPts + MomentVar.Missing)) < 0.5;
end;


(*
// bing results: I can try to write a pascal program for bicubic interpolation, but I cannot guarantee that it will be correct or efficient. Here is what I came up with based on some online sources²³:

//bard would only do bilinear
//ChatGPT had a mismatch in the number of parameters in the function and in the call, and the meaning


// Bicubic interpolation on a rectilinear grid
// Adapted from https://link.springer.com/article/10.1007/s11554-022-01254-8 and https://github.com/bLightZP/ImageInterpolation

type
  // A 4x4 matrix of real numbers
  TMatrix = array[0..3, 0..3] of Real;

  // A function that takes two real numbers and returns a real number
  TFunction = function(x, y: Real): Real;

const
  // The inverse matrix for bicubic interpolation
  InvMat: TMatrix = (
    (1, 0, 0, 0),
    (0, 0, 1, 0),
    (-3, 3, -2, -1),
    (2, -2, 1, 1)
  );

// A helper function to multiply two matrices
function MatMul(A, B: TMatrix): TMatrix;
var
  i, j, k: Integer;
begin
  for i := 0 to 3 do
    for j := 0 to 3 do begin
      Result[i,j] := 0;
      for k := 0 to 3 do
        Result[i,j] := Result[i,j] + A[i,k] * B[k,j];
    end;
end;

// A helper function to evaluate a cubic polynomial given its coefficients and argument
function CubicEval(C: array[0..3] of Real; x: Real): Real;
begin
  Result := C[0] + C[1]*x + C[2]*x*x + C[3]*x*x*x;
end;

// The main function that performs bicubic interpolation on a given function f
// over a rectilinear grid with step size h in both directions
// Returns the interpolated value at point (x,y)
function BicubicInterp(f: TFunction; h, x, y: Real): Real;
var
  i, j: Integer;
  x0, y0: Integer;
  fx: array[-1..2] of Real;
  fxy: array[-1..2,-1..2] of Real;
  dx: array[-1..2] of Real;
  dxy: array[-1..2,-1..2] of Real;
  a: TMatrix;
  px: array[0..3] of Real;
begin
  // Find the nearest grid point to (x,y) that is below and to the left of it
  x0 := Floor(x / h);
  y0 := Floor(y / h);

  // Calculate the function values and the derivatives at the four corners of the grid cell
  for i := -1 to 2 do begin
    fx[i] := f((x0+i)*h, y); // f(x,y) along the horizontal line through y
    for j := -1 to 2 do begin
      fxy[i,j] := f((x0+i)*h, (y0+j)*h); // f(x,y) at the grid points
      dxy[i,j] := (f((x0+i+1)*h, (y0+j)*h) - f((x0+i-1)*h, (y0+j)*h)) / (2*h); // df/dx at the grid points using central difference
    end;
    dx[i] := (fx[i+1] - fx[i-1]) / (2*h); // df/dx along the horizontal line through y using central difference
  end;

  // Construct the coefficient matrix for the bicubic polynomial using matrix multiplication
  a := MatMul(MatMul(InvMat, fxy), InvMat);

  // Evaluate the bicubic polynomial at (x,y) using Horner's method
  for i := 0 to 3 do begin
    px[i] := CubicEval(a[i], x/h-x0); // The coefficients of the polynomial in y
  end;

Result := CubicEval(px, y/h-y0); // The interpolated value at (y)
end;

```

Source: Conversation with Bing, 5/15/2023
(1) An efficient bicubic interpolation implementation for real ... - Springer. https://link.springer.com/article/10.1007/s11554-022-01254-8.
(2) GitHub - bLightZP/ImageInterpolation: High quality, pure pascal image .... https://github.com/bLightZP/ImageInterpolation.
(3) Bicubic interpolation - Wikipedia. https://en.wikipedia.org/wiki/Bicubic_interpolation.

*)



function bicuint(y,y1,y2,y12: tElevFloatarray; x1l,x1u,x2l,x2u : integer; x1,x2: float32;  VAR ansy,ansy1,ansy2: float32) : boolean;
// from Numerical Recipes, bicubic interpolation (also a spline);
type
   gl4by4 = ARRAY [1..4,1..4] OF float64;
const
   wt: ARRAY [1..16,1..16] OF float64 = ( (1,0,-3,2,0,0,0,0,-3, 0, 9, -6, 2, 0, -6, 4),
                                          (0, 0, 0, 0, 0, 0, 0, 0, 3, 0, -9, 6, -2, 0, 6, -4),
                                          (0,0,0,0,0,0,0,0,0,0,9,-6,0,0,-6,4),
                                          (0,0,3,-2,0,0,0,0,0,0,-9,6,0,0,6,-4),
                                          (0,0,0,0,1,0,-3,2,-2,0,6,-4,1,0,-3,2),
                                          (0,0,0,0,0,0,0,0,-1,0,3,-2,1,0,-3,2),
                                          (0,0,0,0,0,0,0,0,0,0,-3,2,0,0,3,-2),
                                          (0,0,0,0,0,0,3,-2,0,0,-6,4,0,0,3,-2),
                                          (0,1,-2,1,0,0,0,0,0,-3,6,-3,0,2,-4,2),
                                          (0,0,0,0,0,0,0,0,0,3,-6,3,0,-2,4,-2),
                                          (0,0,0,0,0,0,0,0,0,0,-3,3,0,0,2,-2),
                                          (0,0,-1,1,0,0,0,0,0,0,3,-3,0,0,-2,2),
                                          (0,0,0,0,0,1,-2,1,0,-2,4,-2,0,1,-2,1),
                                          (0,0,0,0,0,0,0,0,0,-1,2,-1,0,1,-2,1),
                                          (0,0,0,0,0,0,0,0,0,0,1,-1,0,0,-1,1),
                                          (0,0,0,0,0,0,-1,1,0,0,2,-2,0,0,-1,1) );
VAR
   t,u,d1,d2: float64;
   c: gl4by4;

   l,k,j,i: integer;
   xx,d1d2: float64;
   cl,x: array[1..16] of float64;
BEGIN
   d1 := x1u-x1l;
   d2 := x2u-x2l;

   // this was in bcucof(y,y1,y2,y12,d1,d2,c), but moved here for speed

   d1d2 := d1*d2;
   FOR i := 1 to 4 DO BEGIN
      x[i] := y[i];
      x[i+4] := y1[i]*d1;
      x[i+8] := y2[i]*d2;
      x[i+12] := y12[i]*d1d2;
   END;
   FOR i := 1 to 16 DO BEGIN
      xx := 0.0;
      FOR k := 1 to 16 DO xx := xx+wt[i,k]*x[k];
      cl[i] := xx;
   END;
   l := 0;
   FOR i := 1 to 4 DO begin
      FOR j := 1 to 4 DO BEGIN
          l := l+1;
          c[i,j] := cl[l]
      END
   end;
   //IF ((x1u = x1l) OR (x2u = x2l)) THEN MessageToContinue('pause in routine BCUINT - bad input');
   t := (x1-x1l)/d1;
   u := (x2-x2l)/d2;
   ansy := 0.0;
   ansy2 := 0.0;
   ansy1 := 0.0;
   FOR i := 4 DOWNTO 1 DO BEGIN
      ansy := t*ansy+((c[i,4]*u+c[i,3])*u+c[i,2])*u+c[i,1];
      ansy2 := t*ansy2+(3.0*c[i,4]*u+2.0*c[i,3])*u+c[i,2];
      ansy1 := u*ansy1+(3.0*c[4,i]*t+2.0*c[3,i])*t+c[2,i];
   END;
   ansy1 := ansy1/d1;
   ansy2 := ansy2/d2;
   Result := true;
END;


function RadToDegString(rads : float64) : shortstring;
begin
   Result :=  RealToString(rads/Petmar_types.DegToRad,-12,-6)
end;


function AzimuthToDirection(Azimuth : float64) : ShortString;
{using global constant array CompassDirection, takes an azimuth and returns nearest of eight principal cardinal directions}
begin
   Result := CompassDirection[round(Azimuth / 45)];
end {proc AzimuthToDirection};


procedure MedianFilterArray(var Values : bfarray32; NPts,FilterSize : integer);
const
   MaxFilterSize = 99;
var
   Tvals :  ^bfarray32;
   i,j,k,WinSize : integer;
   Subset : array[1..MaxFilterSize] of float32;
begin
   New(Tvals);
   if (FilterSize > MaxFilterSize) then begin
      FilterSize := MaxFilterSize;
      MessageToContinue('Filter size reduced to ' + IntToStr(FilterSize));
   end;

   Winsize := FilterSize div 2;
   for i := 0 to pred(NPts) do begin
      TVals^[i] := Values[i];
      Values[i] := MaxSmallInt;
   end;
   for i := (Winsize) to pred(NPts - Winsize) do begin
       k := 1;
       for j := (i - Winsize) to (i + WinSize) do begin
          Subset[k] := Tvals^[j];
          inc(k);
       end;
       Values[i] := Median(Subset,FilterSize);
   end;
   Dispose(TVals);
end;



procedure MeanFilterArray(var Values : bfarray32; NPts,FilterSize : integer);
const
   MaxFilterSize = 99;
var
   Tvals :  ^bfarray32;
   i,j,k,WinSize : integer;
   Sum : float64;
begin
   New(Tvals);
   if (FilterSize > MaxFilterSize) then begin
      FilterSize := MaxFilterSize;
      MessageToContinue('Filter size reduced to ' + IntToStr(FilterSize));
   end;

   Winsize := FilterSize div 2;
   for i := 0 to pred(NPts) do begin
      TVals^[i] := Values[i];
      Values[i] := MaxSmallInt;
   end;
   for i := (Winsize) to pred(NPts - Winsize) do begin
       Sum := 0;
       k := 0;
       for j := (i - Winsize) to (i + WinSize) do begin
          Sum := Sum + Tvals^[j];
          inc(k);
       end;
       Values[i] := Sum / k;
   end;
   Dispose(TVals);
end;


Function IsNumeric(s: AnsiString) : Boolean;
var
   Value : float64;
BEGIN
   Result := TryStrToFloat(s,Value);
END;


function IsLeapYear(Year : integer) : boolean;
begin
   if (year mod 4 in [1,2,3]) then Result := false
   else if (year mod 100 <> 0) then Result := true
   else if (year mod 400 <> 0) then Result := false
   else Result := true;
end;


function YearLength(Year : integer) : integer;
begin
   if IsLeapYear(Year) then Result := 366
   else Result := 365;
end;


function MissingNumericString(TStr : shortString) : boolean;
begin
   TStr := UpperCase(TStr);
   Result := (TStr = '-9999') or (TStr = '-99') or (TStr = '-99.9') or (TStr = '-999.9') or (TStr = '-999.999') or (TStr = 'INF') or (TStr = '-INF');
end;


function ClipValue(l,min,max : integer) : integer;
begin
   Result := l;
   if (l > max) then result := max;
   if (l < min) then result := min;
end;


function AngularDiff(Angle1,Angle2 : float64) : float64;
begin
   if (Angle1 < Angle2) then Result := Angle2 - Angle1
   else Result := Angle2 + 360 - Angle1;
end;

function AngularDistance(Angle1,Angle2 : float64) : float64;
begin
   Result := abs(Angle2-Angle1);
   if Result > 180 then Result := 360 - Result;
end;


function IsBitSet(const val: word; const TheBit: Byte): Boolean;
begin
   Result := (val and (1 shl TheBit)) <> 0;
end;


function MinInArray(N : integer; vals : array of float64) : float64;
var
  i : Integer;
begin
  Result := Vals[0];
  for i := 1 to pred(n) do if Vals[i] < Result then Result := Vals[i];
end;


function MaxInArray(N : integer; vals : array of float64) : float64;
var
  i : Integer;
begin
  Result := Vals[0];
  for i := 1 to pred(n) do if Vals[i] > Result then Result := Vals[i];
end;

function MaxInArray(N : integer; vals : array of float32) : float64;
var
  i : Integer;
begin
  Result := Vals[0];
  for i := 1 to pred(n) do if Vals[i] > Result then Result := Vals[i];
end;


function MinInArray(N : integer; var vals : bfarray32) : float64;
var
  i : Integer;
begin
  Result := Vals[0];
  for i := 1 to pred(n) do if Vals[i] < Result then Result := Vals[i];
end;


function MaxInArray(N : integer; var vals : bfarray32) : float64;
var
  i : Integer;
begin
  Result := Vals[0];
  for i := 1 to pred(n) do if Vals[i] > Result then Result := Vals[i];
end;


{$IfDef VCL}

procedure GetLabelLength(var InFile : textFile; var SkipLength,LabelLength : integer; xydata : boolean);
var
   i,err : integer;
   x,y : float64;
   Done : boolean;
   FName     : PathStr;
   TFile     : TextFile;
   MenuStr : ShortString;
begin
   SkipLength := 0;
   repeat
      Reset(InFile);
      if XYData then {$I-} read(Infile,x,y) {$I+}
      else {$I-} read(Infile,x); {$I+}
      Done := (IOresult = 0) and EoLn(InFile);
      if not Done then begin
         reset(Infile);
         readln(Infile,MenuStr);

         Val(ptTrim(MenuStr),x,err);
         Done := err = 0;
         if not Done then begin
            reset(Infile);
            FName := MDTempDir + 'xxxqqq.txt';
            AssignFile(TFile,FName);
            rewrite(TFile);
            writeln(TFile,'....v....X....v....X....v....X....v....X....v....L....v....X....v....X');
            for i := 1 to 10 do if not EOF(Infile) then begin
               readln(Infile,MenuStr);
               writeln(TFile,MenuStr);
            end {for i};
            closeFile(tFile);
            QuickOpenEditWindow(FName,'Data File ' + InputFileName);
            DeleteFile(FName);
            ReadDefault('Label length',LabelLength);
            Done := true;
            if XYData then ReadDefault('Skip length to second value',SkipLength);
         end;
         reset(Infile);
      end;
   until Done;
   reset(InFile);
end;


procedure GetFile(Message : shortstring; var LengthShort : integer; var Series1 : bfarray32; var ValueMin,ValueMax : float64; fName : PathStr = '');
const
   SeriesFileName : PathStr = '';
var
   i        : integer;
   DataFile : text;
   ch       : AnsiChar;
   Ext      : ExtStr;
   Dir,Name : System.ANSIString;
begin
   LengthShort := 0;
   if (fName = '') then begin
      if SeriesFilename = '' then SeriesFileName := ProgramRootDir;
      if not GetFileFromDirectory(Message,'*.TXT',SeriesFileName) then exit;
   end
   else SeriesFileName := fName;

   FSplit(SeriesFileName,Dir,Name,Ext);
   SearchDir := Dir;
   InputFileName := Name + Ext;

   assign(DataFile,SeriesFileName);
   GetLabelLength(DataFile,i,LabelLength,false);
   LengthShort := 0;

   ValueMin := 99e99;
   ValueMax := -99e99;
   StartCount('Processing file');
   while not EOF(DataFile) and (LengthShort < MaxContourPoints) do begin
      inc(LengthShort);
      if (LengthShort mod 100 = 0) then UpdateCount(LengthShort);
      for i := 1 to LabelLength do if not EOLn(DataFile) then read(DataFile,ch);
      {$I-} readln(DataFile,Series1[LengthShort]);  {$I+}
      if (IOResult = 0) then begin
         if Series1[LengthShort] > ValueMax then ValueMax := Series1[LengthShort];
         if Series1[LengthShort] < ValueMin then ValueMin := Series1[LengthShort];
      end
      else Series1[LengthShort] := MaxInt;
   end;
   if not EOF(DataFile) then MessageToContinue('Data set truncated.');
   close(DataFile);
   EndCount;
end;

{$EndIf}


function AddDayMonthLeadingZero(Value : integer) : ShortString;
begin
   Result := IntToStr(Value);
   While Length(Result) < 2 do Result := '0' + Result;
end;


procedure ValueInRange(var n : integer; Min,Max : integer);
begin
   if (n > Max) then n := Max;
   if (n < Min) then N := Min;
end;

procedure ValueInRange(var n : float64; Min,Max : float64);
begin
   if (n > Max) then n := Max;
   if (n < Min) then N := Min;
end;

procedure ValueInRange(var n : float32; Min,Max : float32);
begin
   if (n > Max) then n := Max;
   if (n < Min) then N := Min;
end;


procedure ValueInRange(var n : word; Min,Max : word);
begin
   if (n > Max) then n := Max;
   if (n < Min) then N := Min;
end;

procedure SwapPair(var v1,v2 : float64);
var
   tf : float64;
begin
   tf := v1;
   v1 := v2;
   v2 := tf;
end;

procedure SwapPair32(var v1,v2 : float32);
var
   tf : float32;
begin
   tf := v1;
   v1 := v2;
   v2 := tf;
end;


procedure SwapPair(var v1,v2 : integer);
var
   tf : integer;
begin
   tf := v1;
   v1 := v2;
   v2 := tf;
end;


procedure CompareValueToExtremes(value : float64; var MinV,MaxV : float64);
begin
   if (Value > MaxV) then MaxV := Value;
   if (Value < MinV) then MinV := Value;
end;

procedure CompareValueToExtremes(value : float32; var MinV,MaxV : float32);
begin
   if (Value > MaxV) then MaxV := Value;
   if (Value < MinV) then MinV := Value;
end;


procedure CompareValueToExtremes(value : integer; var MinV,MaxV : integer);
begin
   if (Value > MaxV) then MaxV := Value;
   if (Value < MinV) then MinV := Value;
end;

function MinFloat(x1,x2 : float64; x3 : float64 = 99e39; x4 : float64 = 99e39) : float64;
begin
   Result := x1;
   if (x2 < Result) then Result := x2;
   if (x3 < Result) then Result := x3;
   if (x4 < Result) then Result := x4;
end;

function MaxFloat(x1,x2 : float64; x3 : float64 = -99e39; x4 : float64 = -99e39) : float64;
begin
   Result := x1;
   if (x2 > Result) then Result := x2;
   if (x3 > Result) then Result := x3;
   if (x4 > Result) then Result := x4;
end;


procedure MinOfPairFirst(var v1,v2 : float64);
var
   tv : float64;
begin
   if (v2 < v1) then begin
      tv := v1;
      v1 := v2;
      v2 := tv;
   end;
end;


procedure MinOfPairFirst(var v1,v2 : integer);
var
   tv : integer;
begin
   if (v2 < v1) then begin
      tv := v1;
      v1 := v2;
      v2 := tv;
   end;
end;


function AddPoints(CONST PointA, PointB:  TPoint):  TPoint;
BEGIN
    WITH RESULT DO BEGIN
       X := PointA.X + PointB.X;
       Y := PointA.Y + PointB.Y
    END;
END {AddPoints};


function SubtractPoints(CONST PointA, PointB:  TPoint):  TPoint;
BEGIN
   WITH RESULT DO BEGIN
      X := PointA.X - PointB.X;
      Y := PointA.Y - PointB.Y
   END;
END {SubtractPoints};


type
   TLineOrientation = (loHorizontal,loVertical,loPoint);


PROCEDURE CalcLineParameters (CONST PointA, PointB : TPoint; var Slope, Intercept: DOUBLE;  VAR LineOrientation : TLineOrientation);
// Determine whether a line is ltHorizonal or ltVertical, along with the
// appropriate slope and intercept FOR point-slope line equations. These
// parameters are used to determine if a line is selected.
VAR
    Delta: TPoint;
BEGIN
  Delta := SubtractPoints(PointB, PointA);

  IF (Delta.X = 0) AND (Delta.Y = 0) THEN BEGIN
    // special CASE should never happen if iMinPixels > 0
    LineOrientation := loPoint;
    Slope := 0.0;
    Intercept := 0.0
  END
  ELSE BEGIN
    IF ABS(Delta.X) >= ABS(Delta.Y) THEN BEGIN
      // line is more horizontal than vertical. Determine values FOR
      // equation: Y = slope*X + intercept
      LineOrientation := loHorizontal;
      TRY
        Slope := Delta.Y / Delta.X        {conventional slope in geometry}
      EXCEPT
        Slope := 0.0
      END;
      Intercept := PointA.Y - PointA.X*Slope
    END
    ELSE BEGIN
      // line is more vertical than horizontal. Determine values for
      // equation: X = slope*Y + intercept
      LineOrientation := loVertical;
      TRY
        Slope := Delta.X / Delta.Y        {reciprocal of conventional slope}
      EXCEPT
        Slope := 0.0
      END;
      Intercept := PointA.X - PointA.Y*Slope;
    END

  END
END {CalcLineParameters};



//NearLine Function

// Determine if Target1 is "near" line segment between Point1 and Point2
FUNCTION NearLine(CONST Target, Point1, Point2 : TPoint): BOOLEAN;
  CONST
    LineSelectFuzz = 4; // Pixel "fuzz" used in line selection
  VAR
    Intercept : DOUBLE;
    LineOrientation: TLineOrientation;
    maxX : INTEGER;
    maxY : INTEGER;
    minX : INTEGER;
    minY : INTEGER;
    Slope : DOUBLE;
    xCalculated : INTEGER;
   yCalculated : INTEGER;
BEGIN
  RESULT := FALSE;

  // If an Endpoint is not selected, was part of line selected?
  CalcLineParameters (Point1, Point2, Slope, Intercept, LineOrientation);

  CASE LineOrientation OF
    loHorizontal:
      BEGIN
        minX := Point1.X;
        if Point2.X < MinX then MinX := Point2.X;
        maxX := Point1.X;
        if Point2.X > MaxX then MaxX := Point2.X;

        //maxX := MaxIntValue([Point1.X, Point2.X]);
        // first check if selection within horizontal range of line
        IF        (Target.X >= minX) and (Target.X <= maxX)
        THEN BEGIN
          // Since X is within range of line, now see if Y value is close
          // enough to the calculated Y value FOR the line to be selected.
          yCalculated := ROUND( Slope*Target.X + Intercept );
          IF ABS(yCalculated - Target.Y) <= LineSelectFuzz THEN RESULT := TRUE;
        END;
      END;

    loVertical:
      BEGIN
        minY := Point1.Y;
        if Point2.Y < MinY then MinY := Point2.Y;
        maxY := Point1.Y;
        if Point2.Y > MaxY then MaxY := Point2.Y;

        // first check if selection within vertical range of line
        IF  (Target.Y >= minY) AND (Target.Y <= maxY) THEN BEGIN
          // Since Y is within range of line, now see if X value is close
          // enough to the calculated X value FOR the line to be selected.
          xCalculated := ROUND( Slope*Target.Y + Intercept );
          IF ABS(xCalculated - Target.X) <= LineSelectFuzz THEN RESULT := TRUE;
        END;
      END;

    loPoint:
      // Do nothing -- should not occur
  END
END {NearLine};



function GetTickInt(PixelsHigh,LabelSpacing : integer; ElevRange : float64) : float64;
begin
   Result := 0.00001;
   if (PixelsHigh / (ElevRange) * Result) < LabelSpacing then Result := 0.0001;
   if (PixelsHigh / (ElevRange) * Result) < LabelSpacing then Result := 0.001;
   if (PixelsHigh / (ElevRange) * Result) < LabelSpacing then Result := 0.01;
   if (PixelsHigh / (ElevRange) * Result) < LabelSpacing then Result := 0.05;
   if (PixelsHigh / (ElevRange) * Result) < LabelSpacing then Result := 0.1;
   if (PixelsHigh / (ElevRange) * Result) < LabelSpacing then Result := 0.2;
   if (PixelsHigh / (ElevRange) * Result) < LabelSpacing then Result := 0.5;
   if (PixelsHigh / (ElevRange) * Result) < LabelSpacing then Result := 1;
   if (PixelsHigh / (ElevRange) * Result) < LabelSpacing then Result := 2;
   if (PixelsHigh / (ElevRange) * Result) < LabelSpacing then Result := 5;
   if (PixelsHigh / (ElevRange) * Result) < LabelSpacing then Result := 10;
   if (PixelsHigh / (ElevRange) * Result) < LabelSpacing then Result := 25;
   if (PixelsHigh / (ElevRange) * Result) < LabelSpacing then Result := 50;
   if (PixelsHigh / (ElevRange) * Result) < LabelSpacing then Result := 100;
   if (PixelsHigh / (ElevRange) * Result) < LabelSpacing then Result := 200;
   if (PixelsHigh / (ElevRange) * Result) < LabelSpacing then Result := 500;
   if (PixelsHigh / (ElevRange) * Result) < LabelSpacing then Result := 1000;
   if (PixelsHigh / (ElevRange) * Result) < LabelSpacing then Result := 2000;
   if (PixelsHigh / (ElevRange) * Result) < LabelSpacing then Result := 5000;
   if (PixelsHigh / (ElevRange) * Result) < LabelSpacing then Result := 10000;
   if (PixelsHigh / (ElevRange) * Result) < LabelSpacing then Result := 25000;
   if (PixelsHigh / (ElevRange) * Result) < LabelSpacing then Result := 50000;
   if (PixelsHigh / (ElevRange) * Result) < LabelSpacing then Result := 50000;
   if (PixelsHigh / (ElevRange) * Result) < LabelSpacing then Result := 100000;
   if (PixelsHigh / (ElevRange) * Result) < LabelSpacing then Result := 250000;
   if (PixelsHigh / (ElevRange) * Result) < LabelSpacing then Result := 500000;
   if (PixelsHigh / (ElevRange) * Result) < LabelSpacing then Result := 1000000;
end;


procedure varcovar(var x,y : array of float32; NPts : integer; var correlation,covar : float64);
var
  MeanX,MeanY,StdDevX,StdDevY : float64;
begin
   Newvarcovar(x,y,NPts,correlation,covar,MeanX,MeanY,StdDevX,StdDevY);
end;


procedure Newvarcovar(var x,y : array of float32; NPts : integer; var correlation,covar,Mean1,Mean2,StdDev1,StdDev2 : float64);
var
   i : integer;
   sum, sp : array[1..2] of float64;
   spc : float64;
begin
   spc := 0;
   for i := 1 to 2 do begin
      sum[i] := 0;
      sp[i] := 0;
   end;
   for i:= 0 to pred(NPts) do begin
       Sum[1] := Sum[1] + x[i];
       Sum[2] := Sum[2] + y[i];
       sp[1] := sp[1] + x[i] * x[i];
       sp[2] := sp[2] + y[i] * y[i];
       SPc := SPc + x[i] * y[i];
   end;

   Mean1 := (Sum[1] / NPts);
   StdDev1 := sqrt( (NPts * SP[1] - (sum[1] * sum[1]) ) / (NPts-1) / Npts );
   Mean2 := (Sum[2] / NPts);
   StdDev2 := sqrt( (NPts * SP[2] - (sum[2] * sum[2]) ) / (NPts-1) / Npts );

   Covar := (NPts * SPc - Sum[1] * Sum[2]) / NPts / pred(NPts);
   Correlation := Covar / StdDev1 / StdDev2;
   {$IfDef RecordCovar} WriteLineToDebugFile('varcovar out, covar=' + RealToString(Covar,-12,4)); {$EndIf}
end;


procedure fit(var x,y : array of float32; ndata : integer; VAR a,b,siga,sigb,r : float32);
begin
   FitOnRange(x,y,1,ndata,ndata,a,b,siga,sigb,r);
end;


procedure FitOnRange(var x,y : array of float32; FirstPoint,LastPoint : integer; var ndata : integer; var a,b,siga,sigb,r : float32);   {from Press and others, Numerical Recipes, 14.2}
//a is intercept, b is slope, with respective goodness-of-fit
VAR
   i : integer;
   sxy,sy,sxoss,syoss,sx,st2,ss,t,ty,sty2 : float64;
BEGIN
   sx := 0.0;
   sy := 0.0;
   sxy := 0;
   st2 := 0.0;
   sty2 := 0;
   b := 0.0;
   for i := pred(FirstPoint) to pred(LastPoint) do begin
      sx := sx+x[i];
      sy := sy+y[i];
   END;
   NData := succ(LastPoint - FirstPoint);
   ss := ndata;

   sxoss := sx/ss;
   syoss := sy/ndata;
   for i := pred(FirstPoint) to pred(LastPoint) do begin
      t := x[i]-sxoss;
      ty := y[i]-syoss;
      st2 := st2+t*t;
      sty2 := sty2 + ty*ty;
      b := b+t*y[i];
      sxy := sxy + t * ty;
   END;
   b := b/st2;
   a := (sy-sx*b)/ss;
   siga := sqrt((1.0+sx*sx/(ss*st2))/ss);
   sigb := sqrt(1.0/st2);
   r := sxy / sqrt(st2 * sty2);

   {$IfDef RecordFitProblems}
      WriteLineToDebugFile('PROCEDURE fit');
      WriteLineToDebugFile('b=' + RealToString(b,18,8) + '  st2=' + RealToString(st2,18,8) + ' sty2=' + RealToString(sty2,18,8));
      WriteLineToDebugFile('sx=' + RealToString(sx,18,8) + ' sy=' + RealToString(sy,18,8)  + '  sxoss=' + RealToString(sxoss,18,8) +'  syoss=' + RealToString(syoss,18,8));
   {$EndIf}
END;


function DegFtoC(F : float64) : float64;
begin
   DegFtoC := (F - 32) * 5 / 9;
end;


function DegCtoF(C : float64) : float64;
begin
   DegCtoF := (C * 9 / 5) + 32;
end;


procedure DegreesToDegMinSec(Degrees : float64; var Deg,Min : integer; var Sec : float64);
begin
   Degrees := abs(Degrees);
   Deg := round(Int(Degrees));
   Degrees := 60 * Frac(Degrees);
   Min := round(Int(Degrees));
   Sec := 60 * Frac(Degrees);
end;


function VectorAverage(Num : integer; var Readings : farray; var Mag : float64) : float64;
var
   i : integer;
   sumx,sumy : float64;
begin
   sumx := 0;
   sumy := 0;
   for i := 0 to pred(Num) do begin
      sumx := sumx + SinDeg(Readings[i]);
      sumy := sumy + CosDeg(Readings[i]);
   end;
   Result := HeadingOfLine(sumx,sumy);
   Mag := sqrt(sqr(sumx) + sqr(sumy)) / Num;
end;


function ATan2(a1,a2 : float64) : float64;
var
   Angle : float64;
begin
   if Abs(a2) < 0.0000001 then begin
      if Abs(a1) < 0 then Result := 0.5 * Pi
      else Result := 1.5 * Pi;
      exit;
   end;
   Angle := 0;
   if (a2 < 0) then Angle := Pi;
   if (a1 < 0) then Angle := Pi * 2.0;
   Result := Angle + arcTan(a1 / a2);
end;


function ArcTanDeg(val : float64) : float64;
begin
   Result := ArcTan(val) / DegToRad;
end;



function ZeroSeconds(Value : float64) : boolean;
begin
   Value := Value * 60;
   Result :=  abs(Value - round(Value)) < 0.0001;
end;

function CompassAngleToMathAngle(Angle : float64) : float64;
begin
   Result := CompassAngleInRangeFloat64(360 - (Angle - 90));
end;


procedure PlaneEquationFromThreePoints(var x1,y1,z1,x2,y2,z2,x3,y3,z3 : float32;  var XC,YC,ZC,Constant : float32);
{ returns equation of plane containing three points }
{ equation form: (XC * x)  + (YC * y) + (ZC * z) + Constant = 0  }
{ does NOT check if three points are colinear }

{ assume 4th point in plane, P, and given P1,P2, and P3 in plane}
{ 3 vectors P1P, P1P2, and P1P3 all lie in the plane}
{ vector P1P has coefficients x-x1, y-y1, z-z1}
{ vector P1P2 has coefficients x2-x1, y2-y1, z2-z1; similarly with P1P3}
{ determinant with coefficients of three vectors is zero}
{ expand determinant to get equation of plane}

var
   lnx2,lny2,lnz2,lnx3,lny3,lnz3  :  float64;
begin
   lnx2 := x2 - x1;
   lny2 := y2 - y1;
   lnz2 := z2 - z1;
   lnx3 := x3 - x1;
   lny3 := y3 - y1;
   lnz3 := z3 - z1;
   XC :=   lny2 * lnz3 - lny3 * lnz2;
   YC := -(lnx2 * lnz3 - lnx3 * lnz2);
   ZC :=   lnx2 * lny3 - lnx3 * lny2;
   Constant := XC * -x1 + YC * -y1 + ZC * -z1;
end {proc PlaneEquationFromThreePoints};


procedure PlaneEquationFromPointAndNormal(x1,y1,z1 : float32; Normal: VectorType; var XC,YC,ZC,Constant : float32);
begin
   XC := Normal[3];
   YC := Normal[2];
   ZC := Normal[1];
   Constant := XC * -x1 + YC * -y1 + ZC * -z1;
end {proc PlaneEquationFromThreePoints};


procedure FindVectorNormalToPlane(DipDirect,Dip : float64; var N : VectorType);
begin
   N[1] := cosDeg(180-Dip);
   N[2] := -cosDeg(DipDirect) * sinDeg(Dip);
   N[3] := -sinDeg(DipDirect) * sinDeg(Dip);
end;


procedure InterpolateProfileGapCheck(NumPairs : integer; var y,z : Array of float32; NumNewPairs : integer; var yn : Array of float32; var zn : Array of float32; MaxGap : float64);
{call with yn set for the new distances along the profile, and return zn with the new elevations}
var
   i,j : integer;
   f,dy : float64;
begin
   if (NumPairs < 1) then for j := 0 to pred(NumNewPairs) do zn[j] := MaxInt
   else for j := 0 to pred(NumNewPairs) do begin
      if (yn[j] > y[pred(NumPairs)]) or (yn[j] < y[1]) then zn[j] := MaxInt
      else begin
         i := pred(NumPairs);
         repeat
            dec(i);
         until (((y[i] <= yn[j]) and (y[succ(i)] >= yn[j]))) or (i = 0);
         dy := (y[i] - y[succ(i)]);
         if (abs(dy) > MaxGap) then zn[j] := MaxInt
         else begin
            f := (y[i] - yn[j]) / dy;
            zn[j] := z[i] - f * (z[i] - z[succ(i)]);
         end;
      end;
   end;
end;


procedure InterpolateProfile(NumPairs : integer; var y,z : Array of float64; NumNewPairs : integer; var yn : Array of float64; var zn : Array of float64);
{call with yn set for the new distances along the profile, and return zn with the new elevations}
var
   i,j : integer;
   f : float64;
begin
   for j := 0 to pred(NumNewPairs) do begin
      if (yn[j] > y[pred(NumPairs)] + 0.000001) or (yn[j]+ 0.000001 < y[0]) then zn[j] := MaxSmallInt
      else begin
         if (j = 0) and (abs(yn[j] - y[0]) < 0.001) then zn[0] := z[0]
         else if (j = pred(NumNewPairs)) and (abs(yn[j] - y[NumPairs]) < 0.001) then zn[j] := z[NumPairs]
         else begin
             i := pred(NumPairs);
             repeat
                dec(i);
             until (((y[i] <= yn[j]) and (y[succ(i)] >= yn[j]))) or (i = 0);
             f := (y[i] - yn[j]) / (y[i] - y[succ(i)]);
             zn[j] := z[i] - f * (z[i] - z[succ(i)]);
         end;
      end;
   end;
end;


{ SOLUTION OF N SIMULTANEOUS EQUATIONS.  A IS N X N , B IS COLUMN VECTOR OF N ELEMENTS. A CONVERTED TO IDENTITY MATRIX. B CONTAINS SOLUTION. }

    procedure SLE(var A : tTrendMatrix; var B : tTrendVector; N : integer;  Zero : float64);
    var
       Divis,Ratio : float64;
       i,j,k : integer;
    begin
       for I := 1 to N do begin
          Divis := A[I,I];
          IF ABS(Divis) < Zero then exit;
          for J := 1 to N do A[I,J] := A[I,J] / Divis;
          B[I] := B[I] / Divis;
          for J := 1 to N do begin
             IF I <> J then  begin
                Ratio := A[J,I];
                for K := 1 to N do A[J,K] := A[J,K]  - Ratio * A[I,K];
                B[J] := B[J] - Ratio*B[I];
             end {if};
          end {for j};
       end {for i};
    end;


{+++++++++++++++++++++++++++ Math Routines +++++++++++++++++++++++++++++}


function HeadingOfLine(dx,dy : float64) : float64;
{returns heading of a line, with value between 0 and 360 in degrees  values start with 0 north and proceed clockwise, geographic convention }
{modified atan2 function, deals with 0 values in dx or dy                  }
begin
   if abs(dy) < 0.000001 then
      if (dx > 0) then Result := 90 else Result := 270
   else begin
      if (abs(dx) < 0.00001) then
         if (dy < 0) then Result := 180 else Result := 0
      else begin
         Result := ArcTan(dx / dy) / DegToRad;
         if (dx < 0) and (dy < 0) then Result := Result + 180;
         if (dx > 0) and (dy < 0) then Result := Result - 180;
         if Result < 0 then Result := Result + 360;
      end;
   end;
end {proc HeadingOfLine};


function CosDeg(angle : float64) : float64; {cosine of an angle in degrees}
begin
   CosDeg := cos(angle * DegToRad);
end;

function SinDeg(angle : float64) : float64; {sine of an angle in degrees}
begin
   SinDeg := sin(angle * DegToRad);
end;


function TanRad(angle : float64) : float64; {tangent of an angle in radians; Turbo has no tangent function}
var
   Temp : float64;
begin
   temp := cos(Angle);
   if abs(temp) < 0.0000001 then tanRad := 99999999999.9
   else TanRad := sin(Angle) / temp;
end;


function TanDeg(angle : float64) : float64; {tangent of an angle in degrees; Turbo has no tangent function}
begin
   TanDeg := Math.Tan(Angle * DegToRad);
end;


function CotDeg(angle : float64) : float64; {cotangent of angle in degrees; Turbo has no contangent function}
begin
   if abs(TanDeg(Angle)) < 0.0000001 then cotDeg := 99999999999.9
   else CotDeg := 1 / tanDeg(angle);
end;


function ArcCos(Angle : float64) : float64; {arc cosine function, returns result in radians }
begin
   if ( (1 - sqr(angle)) < 0.0000001)  then ArcCos := 0
   else ArcCos := Pi / 2 - ArcTan(Angle / sqrt(1 - sqr(angle)));
end;

function ArcSin(x : float64) : float64;  {arc sine function, returns result in radians }
var
   y : float64;
begin
   if x >= 1.0 then ArcSin := Pi / 2.0
   else if x <= -1.0 then ArcSin := -Pi / 2.0
   else begin
      y := x / sqrt(1 - sqr(x));
      ArcSin := ArcTan(y);
   end;
end;


function TanH(x : float64) : float64; {hyperbolic tangent}
begin
   if x > 10 then TanH := 1 else TanH := (exp(x)-exp(-x)) / (exp(x)+exp(-x));
end;


function SinH(x: float64) : float64;
begin
   SinH := 0.5 * (exp(x) - exp(-x));
end;

function CosH(x: float64) : float64;
begin
   CosH := 0.5 * (exp(x) + exp(-x));
end;


function Log10(x : float32) : float32;
begin
   if x > 0.0 then Log10 := ln(x) / ln(10.0) else Log10 := -9999999;
end;


procedure TransformCoordinates(xin,yin : float64; var xout,yout : float64;  XlateArray : OneBySixFloatArray);
begin
    Xout := XlateArray[1]*Xin + XlateArray[2]*Yin + XlateArray[3];
    Yout := XlateArray[4]*Xin + XlateArray[5]*Yin + XlateArray[6];
end;


procedure CalculateTransformationMatrix(dx1,dy1,dx2,dy2,dx3,dy3,ux1,uy1,ux2,uy2,ux3,uy3 : float64;  var XlateArray : OneBySixFloatArray);
{   dX1,dY1,dX2,dY2,dX3,dY3 --- points from digitizer }
{   uX1,uY1,uX2,uY2,uX3,uY3 --- map points from user  }
var
   Dx12,Dy12,Dx23,Dy23,
   Ux12,Uy12,Ux23,Uy23,DeN : float64; { Coords needed for transform }
begin
   Dx12 := dX1-dX2;             { standard set up for the transform array }
   Dy12 := dY1-dY2;
   Dx23 := dX2-dX3;
   Dy23 := dY2-dY3;
   Ux12 := uX2-uX1;
   Uy12 := uY2-uY1;
   Ux23 := uX3-uX2;
   Uy23 := uY3-uY2;
   DeN := Dy12*Dx23 - Dx12*Dy23;
   XlateArray[1] := (Ux12*Dy23-Ux23*Dy12)/DeN;
   XlateArray[2] := (Ux23*Dx12-Ux12*Dx23)/DeN;
   XlateArray[3] := (dX1*(uX2*dY3-uX3*dY2)+dX2*(uX3*dY1-uX1*dY3)+ dX3*(uX1*dY2-uX2*dY1))/DeN;
   XlateArray[4] := (Uy12*Dy23-Uy23*Dy12)/DeN;
   XlateArray[5] := (Uy23*Dx12-Uy12*Dx23)/DeN;
   XlateArray[6] := (dX1*(uY2*dY3-uY3*dY2)+dX2*(uY3*dY1-uY1*dY3)+ dX3*(uY1*dY2-uY2*dY1))/DeN;
end;


function JulianDayToCalendarDate(julian : LongInt) : shortstring;
var
   Month,Day,Year: integer;
begin
   CalDat(julian,Month,Day,Year);
   Result := IntToStr(Month) + '/' + IntToStr(Day) + '/' + IntToStr(Year);
end;


function AnnualJulianDay(Year,Month,Day : integer) : integer;
begin
   Result := trunc(EncodeDate(year,month,day)) - TRUNC(EncodeDate(year,1,1)) + 1;
end;


procedure JulianDay(JDay,Year : integer; var Month,Day : integer);
var
   j : LongInt;
begin
   j := JulDay(1,1,Year);
   j := j + pred(JDay);
   CalDat(j,Month,Day,Year);
end;


function JulDay(Month,Day,Year : Integer) : LongInt;  {after Press and others, 1986, Numerical Recipes, Cambridge University Press, p.10; day 0 is very ancient; 2,440,000 was in 1968}
const
   igreg = 588829;
var
   ja,jm,jy,jul: LongInt;
begin
   if (Year < 0) then inc(Year);
   if (Month > 2) then begin
      jy := Year;
      jm := succ(Month);
   end
   else begin
      jy := pred(Year);
      jm := Month+13;
   end;
   jul := trunc(365.25*jy) + trunc(30.6001*jm) + Day + 1720995;
   if (Day+31.0*(Month+12.0*Year) >= igreg)  then begin
      ja := trunc(0.01*jy);
      jul := jul+2-ja+trunc(0.25*ja);
   end;
   julday := jul;
end;


procedure CalDat(julian : LongInt; var Month,Day,Year: integer); {after Press and others, 1986, Numerical Recipes, Cambridge University Press}
const
   Greg = 2299161;
var
   je,jd,jc,jb,jalpha,ja : LongInt;
begin
   if (julian >= Greg) then begin
      jalpha := trunc(((julian - 1867216) - 0.25) / 36524.25);
      ja := julian + 1 + jalpha - trunc(0.25 * jalpha)
   end
   else ja := julian;
   jb := ja + 1524;
   jc := trunc(6680.0 + ((jb - 2439870) - 122.1) / 365.25);
   jd := 365 * jc + trunc(0.25 * jc);
   je := trunc((jb-jd) / 30.6001);
   Day := jb - jd - trunc(30.6001 * je);
   Month := pred(je);
   if (Month > 12) then dec(Month,12);
   Year := jc - 4715;
   if (Month > 2) then dec(Year);
   if (Year <= 0) then dec(Year);
end;


function erfc(x : float64): float64; {after Press and others, 1986, Numerical Recipes, Cambridge University Press}
var
   t,t1,z,ans: real;
begin
   z := abs(x);
   t := 1.0 / (1.0 + 0.5 * z);
   t1 := t * (-0.82215223 + t * 0.17087277);
   t1 := t * (1.48851587 + t1);
   t1 := t * (-1.13520398 + t1);
   t1 := t * (0.27886807 + t1);
   t1 := t * (-0.18628806 + t1);
   t1 := t * (0.09678418 + t1);
   t1 := t * (0.37409196 + t1);
   t1 := t * (1.00002368 + t1);
   ans := t * exp(-z * z-1.26551223 + t1);
   if (x >= 0.0) then erfc := ans else erfc := 2.0 - ans;
end;


function erf(x : float64) : float64;
begin
   erf := 1.0 - erfc(x);
end;


function Ninv( P : float32 ) : float32;
{ modified from PIBSIGS.LBR, by Phil Burns, May, 1985, who indicates that Method provides about 6.5 decimal digits of accuracy. }
{required modification for probability of 1, which is arbitrarily set to 100 standard deviations}
{Finds percentage point of normal distribution; input is percent so it is immediately divided by 100}
var
   Y,Pr,Nv,Num,Den : float64;
begin { function Ninv}
   P := P * 0.01;
   Ninv := 0.0;
   if ( P > 0.5 ) then  Pr := 1.0 - P else Pr := P;
   if ( Pr >= 1.0E-20) then begin
      if ( Pr <> 0.5 ) then begin
         Y    := sqrt ( ln( 1.0 / Pr / Pr ) );
         Num  := (Y * -0.453642210148E-4 + -0.0204231210245) * Y;
         Num  := ( Num + -0.342242088547) * Y;
         Num  := ( Num - 1.0) * Y;
         Num  := (Num+ -0.322232431088 );
         Den  := (Y * 0.38560700634E-2 + 0.103537752850) * Y;
         Den  := (Den+ 0.531103462366 ) * Y;
         Den  := (Den + 0.588581570495) * Y;
         Den  := (Den + 0.0993484626060 );
         Nv   := Y +  Num / Den;
         if ( P < 0.5 ) then Ninv := -Nv  else Ninv := Nv;
      end;
   end
   else Ninv := 100.00;
end {function Ninv};



function CompassAngleToRadians(Angle : float64) : float64; {compass angles run clockwise from north; the computer uses radians running counterclockwise from east   }
begin
   CompassAngleToRadians := (90 - Angle) * DegToRad;
end;


function CompassAngleInRangeFloat64(Angle : float64) : float64;  {insures angle in range 0..360  }
begin
   Result := Angle;
   while (Result >= 360) do Result := Result - 360;
   while (Result < 0)   do Result := Result + 360;
end;

function CompassAngleInRangeFloat32(Angle : float32) : float32;  {insures angle in range 0..360  }
begin
   Result := Angle;
   while (Result >= 360) do Result := Result - 360;
   while (Result < 0) do Result := Result + 360;
end;

function CompassAngleInRangeInteger(Angle : integer) : integer;  {insures angle in range 0..360  }
begin
   Result := Angle;
   while (Result >= 360) do Result := Result - 360;
   while (Result < 0)   do Result := Result + 360;
end;


procedure LongitudeAngleInRange(var Angle : float64);
begin
   while (Angle > 180)  do Angle := Angle - 360;
   while (Angle < -180) do Angle := Angle + 360;
end;


procedure LatLongToCartesian(Lat,Long : float64; var A : VectorType);
{converts lat-long pair to cartesian vector representing the conventions from p.110, Cox & Hart}
begin
   A[1] := cos(Lat) * cos(Long);
   A[2] := cos(Lat) * sin(Long);
   A[3] := sin(Lat);
end;


procedure CartesianToLatLong(A : VectorType; var Lat,Long : float64);
{converts cartesian vector representing point into lat-long pair conventions p.110, Cox & Hart  supplements arc tan to get correct quadrant}
begin
   Lat := Math.arcsin(A[3]);
   if abs(A[1]) < 0.000001 then
      if A[2] > 0 then Long := HalfPi else Long := Pi + HalfPi
   else begin
      if (abs(A[2]) < 0.00001) then
         if A[1] < 0 then Long := Pi else Long := 0
      else begin
         Long := ArcTan(A[2] / A[1]);
         if (A[2] < 0) and (A[1] < 0) then Long := Long + Pi;
         if (A[2] > 0) and (A[1] < 0) then Long := Long - Pi;
         if Long > Pi then Long := Long - TwoPi;
      end;
   end;
end;


procedure RotatePoint(R : MatrixType; var Init,Final : VectorType);
var
   j,k : integer;
begin
   for j := 1 to 3 do begin
      Final[j] := 0.0;
      for k := 1 to 3 do Final[j] := Final[j] + R[j,k] * Init[k];
   end {for j};
end;


procedure RotationMatrix(PLat,PLong,Omega : float64; var R : MatrixType);
{calculate Rotation Matrix from pole and angular rotation, all in radians}
var
   i,j : integer;
   E   : VectorType;
   CosOmega,SinOmega : float64;
begin
   {$IfDef RecordMatrixOps} WriteLineToDebugFile('RotationMatrix for lat=' + RealToString(Plat,-8,-4) + ' long=' + RealToString(Plong,-8,-4) + ' omega=' + RealToString(Omega,-8,-4)); {$EndIf}
   LatLongToCartesian(PLat,PLong,E);
   CosOmega := cos(Omega);
   for i := 1 to 3 do
      for j := 1 to 3 do begin
         R[i,j] := E[i] * E[j] * (1.0 - CosOmega);
         if i = j then R[i,j] := R[i,j] + CosOmega;
      end;
   SinOmega := sin(Omega);
   R[1,2] := R[1,2] - E[3] * SinOmega;
   R[1,3] := R[1,3] + E[2] * SinOmega;
   R[2,1] := R[2,1] + E[3] * SinOmega;
   R[2,3] := R[2,3] - E[1] * SinOmega;
   R[3,1] := R[3,1] - E[2] * SinOmega;
   R[3,2] := R[3,2] + E[1] * SinOmega;
end;



procedure jacobi(var a : tTrendMatrix; n : integer; var d : tTrendVector; var v : tTrendMatrix; var nrot : integer);
   {A must be real symmetric matrix; elements above diagonal will be destroyed}
   {V contains normalized eigenvectors on output, in rows}
   {D will contain the eigenvalues}
   {NRot will contain the number of rotations required}
   {after  Press and others, 1986, Numerical Recipes, p.346-349; 748-749}
VAR
   j,iq,ip,i: integer;
   tresh,theta,tau,t,sm,s,h,g,c: float64;
   b,z: array [1..MaxMatrixSize] of float64;
begin
   {$IfDef RecordMatrixOps} WriteLineToDebugFile('procedure jacobi in'); {$EndIf}
   for ip := 1 to n do begin
      for iq := 1 to n do begin
         v[ip,iq] := 0.0
      end;
      v[ip,ip] := 1.0
   end;
   for ip := 1 to n do begin
      b[ip] := a[ip,ip];
      d[ip] := b[ip];
      z[ip] := 0.0
   end;
   nrot := 0;
   for i := 1 to 50 do begin
      {$IfDef RecordMatrixOps}  WriteLineToDebugFile('  jacobi i=' + IntToStr(i)); {$EndIf}
      sm := 0.0;
      for ip := 1 to n-1 do begin
         for iq := ip+1 to n do begin
            sm := sm+abs(a[ip,iq])
         end
      end;
      IF (sm = 0.0) then begin
         {$IfDef RecordMatrixOps} WriteLineToDebugFile('  jacobi exit sm=0'); {$EndIf}
         exit;
      end;
      IF (i < 4) then tresh := 0.2*sm/sqr(n)
      ELSE tresh := 0.0;
      for ip := 1 to n-1 do begin
         for iq := ip+1 to n do begin
            g := 100.0*abs(a[ip,iq]);
            IF ((i > 4) AND ((abs(d[ip])+g) = abs(d[ip]))
               AND ((abs(d[iq])+g) = abs(d[iq]))) then
               a[ip,iq] := 0.0
            ELSE IF (abs(a[ip,iq]) > tresh) then BEGIN
               h := d[iq]-d[ip];
               IF ((abs(h)+g) = abs(h)) then BEGIN
                  t := a[ip,iq]/h
               end ELSE BEGIN
                  theta := 0.5*h/a[ip,iq];
                  t := 1.0/(abs(theta)+sqrt(1.0+sqr(theta)));
                  IF (theta < 0.0) then t := -t
               end;
               c := 1.0/sqrt(1+sqr(t));
               s := t*c;
               tau := s/(1.0+c);
               h := t*a[ip,iq];
               z[ip] := z[ip]-h;
               z[iq] := z[iq]+h;
               d[ip] := d[ip]-h;
               d[iq] := d[iq]+h;
               a[ip,iq] := 0.0;
               for j := 1 to ip-1 do begin
                  g := a[j,ip];
                  h := a[j,iq];
                  a[j,ip] := g-s*(h+g*tau);
                  a[j,iq] := h+s*(g-h*tau)
               end;
               for j := ip+1 to iq-1 do begin
                  g := a[ip,j];
                  h := a[j,iq];
                  a[ip,j] := g-s*(h+g*tau);
                  a[j,iq] := h+s*(g-h*tau)
               end;
               for j := iq+1 to n do begin
                  g := a[ip,j];
                  h := a[iq,j];
                  a[ip,j] := g-s*(h+g*tau);
                  a[iq,j] := h+s*(g-h*tau)
               end;
               for j := 1 to n do begin
                  g := v[j,ip];
                  h := v[j,iq];
                  v[j,ip] := g-s*(h+g*tau);
                  v[j,iq] := h+s*(g-h*tau)
               end;
               nrot := nrot+1;
            end
         end
      end
   end;
   for ip := 1 to n do begin
      b[ip] := b[ip]+z[ip];
      d[ip] := b[ip];
      z[ip] := 0.0
   end;
   {$IfDef VCL}  Petmar.MessageToContinue('Jacobi pause; 50 iterations should not happen'); {$EndIf}
end;

procedure Eigsrt(var d : tTrendVector; VAR v : tTrendMatrix; n: integer); {after Press and others, 1986, Numerical Recipes, Cambridge University Press}
VAR
   k,j,i: integer;
   p: float64;
begin
   for i := 1 to n-1 do begin
      k := i;
      p := d[i];
      for j := i+1 to n do begin
         if (d[j] >= p) then begin
            k := j;
            p := d[j]
         end;
      end;
      if (k <> i) then begin
         d[k] := d[i];
         d[i] := p;
         for j := 1 to n do begin
            p := v[j,i];
            v[j,i] := v[j,k];
            v[j,k] := p
         end;
      end
   end
end;


procedure SortStringListNumerically(var SL : tStringList);
const
   MaxSort = 500;
var
   i,n : integer;
   TStr : shortstring;
   ra : array[1..MaxSort] of float32;
begin
   n := sl.count;
   if n <= MaxSort then begin
      for i := 1 to n do begin
         ra[i] := StrToFloat(sl.Strings[pred(i)]);
         {$IfDef RecordProblems} WriteLineToDebugFile(IntToStr(I) + '  ' + RealToString(ra[i],-18,-4)); {$EndIf}
      end;
      HeapSort(n,ra);
      sl.Clear;
      sl.Sorted := false;
      for i := 1 to n do begin
         TStr := RealToString(ra[i],-18,-5);
         sl.Add(TStr);
      end;
   end;
end;


PROCEDURE HeapSort(n: integer; var ra: array of float32);  {after Press and others, 1986, Numerical Recipes, Cambridge University Press}
VAR
   l,j,ir,i: integer;
   rra : float64;
BEGIN
   if n <= 1 then exit;
   l := (n DIV 2)+1;
   ir := n;
   WHILE true DO BEGIN
      IF (l > 1) THEN BEGIN
         l := l-1;
         rra := ra[pred(l)];
      END
      ELSE BEGIN
         rra := ra[pred(ir)];
         ra[pred(ir)] := ra[pred(1)];
         ir := ir-1;
         IF (ir = 1) THEN BEGIN
            ra[pred(1)] := rra;
            exit;
         END
      END;
      i := l;
      j := l+l;
      WHILE (j <= ir) DO BEGIN
         IF (j < ir) THEN
            IF (ra[pred(j)] < ra[pred(j+1)]) THEN j := j+1;
         IF (rra < ra[pred(j)]) THEN BEGIN
            ra[pred(i)] := ra[pred(j)];
            i := j;
            j := j+j;
         END
         ELSE j := ir+1;
      END;
      ra[pred(i)] := rra;
   END;
END;


PROCEDURE HeapSort(n: integer; var ra: array of float64);  {after Press and others, 1986, Numerical Recipes, Cambridge University Press}
VAR
   l,j,ir,i: integer;
   rra : float64;
BEGIN
   if n <= 1 then exit;
   l := (n DIV 2)+1;
   ir := n;
   WHILE true DO BEGIN
      IF (l > 1) THEN BEGIN
         l := l-1;
         rra := ra[pred(l)];
      END
      ELSE BEGIN
         rra := ra[pred(ir)];
         ra[pred(ir)] := ra[pred(1)];
         ir := ir-1;
         IF (ir = 1) THEN BEGIN
            ra[pred(1)] := rra;
            exit;
         END
      END;
      i := l;
      j := l+l;
      WHILE (j <= ir) DO BEGIN
         IF (j < ir) THEN
            IF (ra[pred(j)] < ra[pred(j+1)]) THEN j := j+1;
         IF (rra < ra[pred(j)]) THEN BEGIN
            ra[pred(i)] := ra[pred(j)];
            i := j;
            j := j+j;
         END
         ELSE j := ir+1;
      END;
      ra[pred(i)] := rra;
   END;
END;


function Quantile(QWant : integer; var x : array of float64; n: integer; AlreadySorted : boolean = false) : float64;
VAR
   n2 : integer;
BEGIN
   if (n = 1) then Result := x[0]
   else begin
      if (not AlreadySorted) then HeapSort(n,x);
      n2 := round(QWant * n / 100);
      result := x[n2];
   end;
END;

function Quantile(QWant : integer; var x : array of float32; n: integer; AlreadySorted : boolean = false) : float64;
VAR
   n2 : integer;
BEGIN
   if (n = 1) then Result := x[0]
   else begin
      if (not AlreadySorted) then HeapSort(n,x);
      n2 := round(QWant * n / 100);
      result := x[n2];
   end;
END;


function Median(var x: array of float64; n : integer; AlreadySorted : boolean = false) : float64;  {after Press and others, 1986, Numerical Recipes, Cambridge University Press}
VAR
   n2 : integer;
BEGIN
   if n = 1  then Result := x[0]
   else begin
      if (not AlreadySorted) then HeapSort(n,x);
      n2 := n div 2;
      if odd(n2) then result := x[n2]
      else result := 0.5*(x[n2-1]+x[n2]);
   end;
END;

function Median(var x: array of float32; n : integer; AlreadySorted : boolean = false) : float64; {after Press and others, 1986, Numerical Recipes, Cambridge University Press}
VAR
   n2 : integer;
BEGIN
   if n = 0 then Result := Nan
   else if n = 1 then Result := x[0]
   else begin
      if (not AlreadySorted) then HeapSort(n,x);
      n2 := n div 2;
      if odd(n2) then result := x[n2]
      else result := 0.5*(x[n2-1]+x[n2]);
   end;
END;


function Mode(var x : array of float32; n: integer; binsize : float32) : float32;
VAR
   i,MaxCount,Count : integer;
   //v,
   bin1 : float32;
BEGIN
   Result := Nan;
   MaxCount := 0;

   HeapSort(n,x);

   bin1 := round(x[0] / binsize) * binSize - 0.5 * BinSize;
   i := 0;
   while i <= pred(n) do begin
       Count := 0;
       if x[i] < bin1 + binSize then begin
          while (i <= pred(n)) and (x[i] < bin1 + binSize) do begin
             inc(Count);
             inc(i);
          end;
       end
       else inc(i);
       if (Count > MaxCount) then begin
          MaxCount := Count;
          Result := Bin1;
       end;
       Bin1 := Bin1 + BinSize;
   end;
end;


function Percentile(PC : float64; var x: array of float32; n : integer; AlreadySorted : boolean = false) : float64; {after Press and others, 1986, Numerical Recipes, Cambridge University Press}
VAR
   n2 : integer;
BEGIN
   if n = 0 then Result := Nan
   else if n = 1 then Result := x[0]
   else begin
      if (not AlreadySorted) then HeapSort(n,x);
      n2 := round(0.01 * PC * n);
      result := x[n2];
   end;
END;


procedure InitializeMomentVar(var MomentVar : tMomentVar);
begin
   MomentVar.NPts := 0;
   MomentVar.Minz := 99e99;
   MomentVar.Maxz := -99e99;
   MomentVar.Missing := 0;
   MomentVar.mean := 0.0;
   MomentVar.avg_dev := 0.0;
   MomentVar.std_dev := 0.0;
   MomentVar.svar := 0.0;
   MomentVar.skew := 0.0;
   MomentVar.curt := 0.0;
   MomentVar.rmse := 0.0;
   MomentVar.MAE := 0.0;
   MomentVar.LE90 := 0.0;
end;


procedure moment(var data : array of float32; var MomentVar : tMomentVar; MomentStop : tMomentStop);  {after Press and others, 1986, Numerical Recipes, Cambridge University Press, p.458, with additional computations}
//msAll,msAfterMean,msAfterStdDev,msBeforeMedian
VAR
   j: integer;
   s,p,s2 : float64;
BEGIN
   if (MomentVar.Npts = 0) then exit;
   if (MomentVar.Npts = 1) then begin
      MomentVar.Mean := data[0];
      exit;
   end;
   s := 0.0;
   s2 := 0.0;
   for j := 0 to pred(MomentVar.Npts) do begin
      s := s + data[j];
      s2 := s2 + sqr(data[j]);
      Petmath.CompareValueToExtremes(data[j],MomentVar.MinZ,MomentVar.MaxZ);
   end;
   MomentVar.Mean := s / MomentVar.Npts;

   {$IfDef TrackNAN}
      if IsNAN(MomentVar.Mean) then begin
         for j := 0 to pred(MomentVar.Npts) do begin
            if IsNAN(data[j]) then begin
               WriteLineToDebugFile(IntToStr(j));
            end;
         end;
      end;
   {$EndIf}

   if (MomentStop = msAfterMean) then exit;

   FOR j := 0 to pred(MomentVar.Npts) do begin
      s := data[j] - MomentVar.Mean;
      MomentVar.avg_dev := MomentVar.avg_dev + abs(s);
      p := s*s;
      MomentVar.svar := MomentVar.svar + p;
      p := p*s;
      MomentVar.skew := MomentVar.skew + p;
      p := p*s;
      MomentVar.curt := MomentVar.curt + p;
      MomentVar.MAE := MomentVar.MAE + abs(data[j]);
   END;
   MomentVar.svar := MomentVar.svar / (MomentVar.Npts-1);
   MomentVar.std_dev := sqrt(MomentVar.svar);
   {$If Defined(TrackNegStdDev)}
      if (MomentVar.std_dev < 0) then begin
         MessageToContinue('Negative standard deviation');
      end;
   {$EndIf}
   if (MomentStop = msAfterStdDev) then exit;

   MomentVar.rmse := sqrt(S2/MomentVar.Npts);
   MomentVar.mae := MomentVar.mae / MomentVar.Npts;
   MomentVar.avg_dev := MomentVar.avg_dev / MomentVar.Npts;
   IF abs(MomentVar.svar) > 0.000001 then begin
      MomentVar.skew := MomentVar.skew / (MomentVar.Npts*MomentVar.std_dev*MomentVar.std_dev*MomentVar.std_dev);
      MomentVar.curt := MomentVar.curt / (MomentVar.Npts*sqr(MomentVar.svar))-3.0;
   END;
   if (MomentStop = msBeforeMedian) then exit;
   MomentVar.median := Median(data,MomentVar.Npts);
   MomentVar.Q1 := data[round(MomentVar.NPts / 4)];
   MomentVar.Q3 := data[round(MomentVar.NPts * 3 / 4)];
   MomentVar.PC95 := data[round(MomentVar.NPts * 95 / 100)];
   MomentVar.PC98 := data[round(MomentVar.NPts * 98 / 100)];
   MomentVar.PC99 := data[round(MomentVar.NPts * 99 / 100)];
   MomentVar.PC1 := data[round(MomentVar.NPts * 1 / 100)];
   MomentVar.PC2 := data[round(MomentVar.NPts * 2 / 100)];
   MomentVar.PC5 := data[round(MomentVar.NPts * 5 / 100)];
   MomentVar.LE90 := data[round(MomentVar.NPts * 90 / 100)];
end;



procedure MomentsToStringGrid(StringGrid : tStringGrid; var OnLine,OnColumn : integer; Variable : shortString; MomentVar : tMomentVar; LongVersion : boolean = false);
var
   DM : integer;
begin
   if (MomentVar.maxz - MomentVar.minz < 0.5) then DM := 2 else DM := 1;
   StringGrid.Cells[0,OnLine] := Variable + ' n';
   StringGrid.Cells[OnColumn,OnLine] := IntToStr(MomentVar.NPts);
   inc(OnLine);
   StringGrid.Cells[0,OnLine] := Variable + ' mean';
   StringGrid.Cells[OnColumn,OnLine] := RealToString(MomentVar.mean,-18,-4*DM);
   inc(OnLine);
   StringGrid.Cells[0,OnLine] := Variable + ' std dev';
   StringGrid.Cells[OnColumn,OnLine] := RealToString(MomentVar.std_dev,-18,-4*DM);
   inc(OnLine);
   StringGrid.Cells[0,OnLine] := Variable + ' skewness';
   StringGrid.Cells[OnColumn,OnLine] := RealToString(MomentVar.skew,-18,-4*DM);
   inc(OnLine);
   StringGrid.Cells[0,OnLine] := Variable + ' kurtosis';
   StringGrid.Cells[OnColumn,OnLine] := RealToString(MomentVar.curt,-18,-4*DM);
   inc(OnLine);
   StringGrid.Cells[0,OnLine] := Variable + ' min';
   StringGrid.Cells[OnColumn,OnLine] := RealToString(MomentVar.minz,-18,-4*DM);
   inc(OnLine);
   StringGrid.Cells[0,OnLine] := Variable + ' max';
   StringGrid.Cells[OnColumn,OnLine] := RealToString(MomentVar.maxz,-18,-4*DM);
   inc(OnLine);
   StringGrid.Cells[0,OnLine] := Variable + ' median';
   StringGrid.Cells[OnColumn,OnLine] := RealToString(MomentVar.median,-18,-4*DM);
   inc(OnLine);
   if LongVersion then begin
      StringGrid.Cells[0,OnLine+8] := Variable + ' avg dev';
      StringGrid.Cells[OnColumn,OnLine+2] := RealToString(MomentVar.avg_dev,-18,-4*DM);
      inc(OnLine);
      StringGrid.Cells[0,OnLine+9] := Variable + ' Q1';
      StringGrid.Cells[OnColumn,OnLine+9] := RealToString(MomentVar.Q1,-18,-4*DM);
      inc(OnLine);
      StringGrid.Cells[0,OnLine+10] := Variable + ' Q3';
      StringGrid.Cells[OnColumn,OnLine+10] := RealToString(MomentVar.Q3,-18,-4*DM);
      inc(OnLine);
   end;
end;


procedure MomentReport(Variable : shortString; var data : bfarray32; n : integer; Title : ShortString = ''; StringGrid : tStringGrid = nil; OnLine : integer = -99; OnCol : integer = -99);
var
   MomentVar : tMomentVar;
begin
   MomentVar.Npts := n;
   Moment(data,MomentVar,msAll);
   if (StringGrid <> Nil) then MomentsToStringGrid(StringGrid,OnLine,OnCol,Variable,MomentVar);
end;


function MomentResultsToString(MomentVar : tMomentVar) : shortstring;
begin
   Result := ',' + RealToString(MomentVar.MinZ,-18,-4) + ','  + RealToString(MomentVar.PC1,-18,-6) + ','  + RealToString(MomentVar.PC2,-18,-6) + ',' + RealToString(MomentVar.PC5,-18,-6) + ',' +
       RealToString(MomentVar.Q1,-18,-6) + ',' + RealToString(MomentVar.Median,-18,-6)  + ',' + RealToString(MomentVar.Mean,-18,-6)  + ',' + RealToString(MomentVar.Q3,-18,-6) + ',' +
       RealToString(MomentVar.PC95,-18,-6)  + ',' + RealToString(MomentVar.PC98,-18,-6) + ',' + RealToString(MomentVar.PC99,-18,-6) + ',' +  RealToString(MomentVar.MaxZ,-18,-6) + ',' +
       RealToString(MomentVar.avg_dev,-18,-6) + ',' + RealToString(MomentVar.std_dev,-8,6) + ',' + RealToString(MomentVar.skew,-18,-6) + ',' +  RealToString(MomentVar.curt,-18,-6) + ',' +
       IntToStr(MomentVar.NPts);
end;


function ShortMomentResultsToString(MomentVar : tMomentVar) : shortstring;
begin
   Result := RealToString(MomentVar.Mean,-18,-4) + ','  + RealToString(MomentVar.MinZ,-18,-4) + ','  + RealToString(MomentVar.MaxZ,-18,-4) + ',' + RealToString(MomentVar.std_dev,-8,2) + ',' +  IntToStr(MomentVar.NPts);
end;


function FracDimFromSlope1(Slope : float64) : float64;
begin
   Result := (0.5 * (5 - abs(Slope)));
end;


function FracDimFromSlope2(Slope : float64) : float64;
begin
   Result := 3 - 0.5 * abs(Slope);
end;


procedure LoadBFarray32(fName : PathStr; var Values : bfarray32; var npts : integer);
//need to create values if it will be a pointer
var
    inf : file;
begin
    npts := GetFileSize(fName) div SizeOf(float32);
    AssignFile(inf,fName);
    reset(inf,sizeOf(float32));
    BlockRead(inf,values[0],Npts);
    closeFile(inf);
end;


initialization
   {$IfDef MessageStartUpProblems} MessageToContinue('Startup petmath'); {$EndIf}
finalization
end {unit}.













