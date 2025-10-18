{$F+}

unit petmar_types;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2025 Peter L. Guth  }
{________________________________________}

{$I nevadia_defines.inc}

//{$Define UseFDMemTable}


{$IfDef RecordProblems}  //normally only defined for debugging specific problems
   {$IfDef DEBUG}
      //{$Define RecordMakeDir}
      //{$Define RecordFieldPresentProblems}
      //{$Define RecordMyDataCreation}
      //{$Define RecordMyDataFilter}
      //{$Define RecordSQLite}
      //{$Define TrackCDStiming}
      //{$Define TrackInsert}
      //{$Define RecordMonitors}
      //{$Define DBrewrite}
      {$IfDef Android}
         //{$Define RecordRealToString}
      {$EndIf}
   {$EndIf}
{$EndIf}


interface

uses
   System.UITypes, System.UIConsts,System.IOUtils,System.SysUtils,

   {$IfDef VCL}
      Windows, Winspool,ComCtrls,Forms,Printers,Graphics,Buttons,
      System.Variants,
      ActiveX,
      ComObj,
   {$EndIf}

   {$IfDef FMX}
      FMX.Controls, FMX.Types, FMX.Graphics,
   {$EndIf}

   {$IfDef MSWindows}
      SHLOBJ,Registry, Messages,ShellAPI,
   {$EndIf}

   Math,Db,Classes,StrUtils;

const
   //{$IF (CompilerVersion = 36) and System.RTLVersion122} DelphiCompiler = '12.3 Athens'; {$Else} DelphiCompiler = 'Old version'; {$EndIf}
   {$IF (CompilerVersion = 36)}  DelphiCompiler = '12.3 Athens'; {$Else} DelphiCompiler = 'Old version'; {$EndIf}

{$IfDef MSWindows}
const
   WM_ClearListeningWindow = WM_USER + 408;
   WM_BroadcastLatLongMessage = WM_USER + 415;
   WM_BroadcastDrawPlane = WM_USER + 418;
{$EndIf}


const
   StartTableString = '<table border="3">';
   EndTableString   = '</table>';
   StartRowString   = '<tr>';
   EndRowString     = '</tr>';
   StartColumnString = '<td>';
   EndColumnString   = '</td>';
   StartHTMLString = '<html>';
   EndHTMLString   = '</html>';

   AOC = 'all operations completed';
   JustAfterMidnight = 'T00:00:01';
   JustBeforeMidnight = 'T23:59:01';
   MessLineBreak = #13;

   EdburgGeneralFuncsMaxObservations  = 264000;
   EdburgMaxVariables = 250;
   EdburgGeneralFuncsMaxClusters = 50;


const
   {$IfDef MSWindows}
      MaxScreenXMax = 25000;
      MaxScreenYMax = 25000;
   {$Else}
      MaxScreenXMax = 5000;
      MaxScreenYMax = 5000;
   {$EndIf}

type
   {$IfDef MSWindows}
      ShortString = System.ANSIString;
      String23  = string[23];
      String3   = string[3];   //needed for stratcol patterns

      string35  = shortstring;
      string16  = shortstring;
      String10  = shortstring;
      PathStr = System.ANSIString;
      DirStr  = System.ANSIString;
      NameStr = System.ANSIString;
      ExtStr = System.ANSIString;
   {$Else}
      string16  = string;
      String10  = string;
      String5   = array[1..5] of byte;
      String3   = string;
      AnsiChar = Char;
      AnsiString = System.String;
      ShortString = System.String;
      PathStr = System.String;
      DirStr  = System.String;
      NameStr = System.String;
      ExtStr = System.String;
   {$EndIf}

   Characters = Set of AnsiChar;
   Float64 = double;
   Float32 = single;
   tLongArray = array[0..MaxScreenXMax] of float64;
   tDateField = (dfYMDslash,dfMDYSlash,dfYMDstraight);
   tGraphPoint32 = array[1..2] of float32;


   {$IfDef VCL}
      tColorArray = array[0..15] of TColor;
      tRGBColorArray = array[0..15] of TRGBTriple;
      tMyBitmap = Graphics.tBitmap;
      tPlatformColor = TRGBTriple;
      TRGB = array[0..MaxScreenXMax] of TRGBTriple;  // max width of bitmap
      pRGB = ^TRGB;
      tScreenPRGB = array[0..MaxScreenYMax] of PRGB;
      pScreenPRGB = ^tScreenPRGB;
   {$EndIf}

   {$IfDef FMX}
      tColor = int32;
      tColorArray = array[0..15] of TColor;
      tRGBColorArray = array[0..15] of TAlphaColor;
      tPlatformColor = TAlphaColor;

      tRGBTriple = record
         rgbtBlue,
         rgbtGreen,
         rgbtRed : byte;
      end;

      tBrushStyle = byte;
      TPoint = packed record
         X: Longint;
         Y: Longint;
       end;
       tMyBitmap = FMX.Graphics.tBitmap;

       const
          Max_Path = 255;
   {$EndIf}

{$IfDef VCL}
//const
   //WinGraphColors : tColorArray = (clBlack,clRed,clBlue,clLime,clFuchsia,clPurple,clNavy,clAqua,clTeal,clOlive,clMaroon,clGreen,clYellow,clDkGray,clSilver,clWhite);
{$EndIf}



type
   tRGBLookUp = array[0..255] of TPlatformcolor;

    type
      tMyFont = packed record
         Name : shortstring;
         Size : SmallInt;
         Color : tPlatformColor;
         Italics,
         Underline,
         Bold : boolean;
      end;

   sfBoundBox = packed record
      XMin,
      YMin,
      XMax,
      YMax : float64;
   end;

   sfIntBoundBox = packed record
      XMin,
      YMin,
      XMax,
      YMax : integer;
   end;


const
   ValidDOSFileNameChars = ['a'..'z','A'..'Z','0'..'9',':','/','$','.','&','@','!','%','~',#39,'`','(',')','-','{','}','_','\'];
   ReasonableTextChars = [#32..#126,#224..#253];
   ReasonableNumericChars = ['0'..'9','-','d'..'f','D'..'F','.'];
   DBaseFieldNameChars = ['A'..'Z','0'..'9','_'];
   MonthName : array[1..12] of ShortString = ('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec');

   DegSym = #248;  //'°';
   MaskBit     : array[0..7] of byte = (128,64,32,16,8,4,2,1);
   ReverseMask : array[0..7] of byte = (127,191,223,239,247,251,253,254);
   MaxContoursPerGrid = 25;
   Offset = 3;
   FeetToMeters = 0.3048;
   DegToRad     = 0.01745329252;
   MaxSmallInt  = 32767;
   MaxLongInt   = 2147483647;

   Sqrt_2       = 1.41421356;
   TwoPi        = 2 * Pi;      //6.28318530717959;
   HalfPi       = 0.5 * pi;   //1.5707963268;
   QuarterPi    = 0.25 * Pi;
   e            = 2.71828;
   Km2Miles     = 0.621371;
   Km2NauticalMiles = 0.539957;

   CompassDirection : array[0..8] of ShortString = ('N','NE','E','SE','S','SW','W','NW','N');

   MaxFilterSize = 15;
   MaxMatrixSize = 100;
   MaxColorTableEntries = 6400;

type
   tZTableValue  = array[1..MaxColorTableEntries] of float64;
   tZTableColors = array[1..MaxColorTableEntries] of tPlatformColor;
   tZTableColors255 = array[1..255] of tPlatformColor;
   tColorTableDefinitions = record
      ZTableEntries : integer;
      ZTableValue  : array[1..256] of float64;
      ZTableColors : tZTableColors255;
      ZTableLabels : array[1..256] of ShortString;
      ZTableCount  : array[1..256] of int64;
      ZBigTableValue  : ^tZTableValue;
      ZBigTableColors : ^tZTableColors;
   end;

type
   FilterType = array[1..MaxFilterSize,1..MaxFilterSize] of integer;
   tTrendMatrix = array[1..MaxMatrixSize,1..MaxMatrixSize] of float64;
   tTrendVector = array[1..MaxMatrixSize] of float64;
   tFourFloats = array[1..4] of float64;

   tDrawingSymbol = (Cross,EX,Splat,FilledDiamond,Diamond,FilledDownTri,DownTri,FilledUpTri,UpTri,FilledCircle,Circle,Dot,Box,FilledBox,VertLine);  {order important}
   tStructureType = (aBedding,aJoint,aFoliation,aFault);

   tFullSymbolDeclaration = record
      DrawingSymbol : tDrawingSymbol;
      Color         : tPlatformColor;
      Size          : byte;
   end;

const
   LastSymbol = VertLine;
   LegGrays =0;
   LegRainbows = 1;
   LegTerrain = 2;
   LegSpectrum = 3;
   LegChloropleth = 4;

type
   ColorFunctionType = Function(Z : float32) : TColor;

   {$IfDef VCL}
      SetPointProcedure = Procedure(BitMap : tMyBitmap; x,y : integer; color : TColor);    {used for passing procedural parameters}
   {$EndIf}

   tLegendColors = integer;  //(LegGrays,LegRainbows,LegTerrain,LegSpectrum,LegChloropleth);

   tSymbols256  = array[0..255] of tFullSymbolDeclaration;
   tbytes256 = array[0..255] of byte;
   tColors256  = array[0..255] of TColor;
   tPlatformColors256  = array[0..255] of TPlatformColor;

   tDistanceUnits = (disMetric,disEnglish,disNautical);
   tLatLongMethod = (DecDegrees,DecMinutes,DecSeconds,NearestDegree,NearestMinute,NearestSecond,ShortDegrees,VeryShortDegrees,LongDegrees);
   tAngleMeasure = (amDegree,amMinute,amSecond);

type
   tJustify = (LeftJustify,RightJustify);

const
   lpNone = 0;
   lpNWMap = 1;
   lpSWMap = 2;
   lpNMap = 3;
   lpSMap = 4;
   lpNEMap = 5;
   lpSEMap = 6;
   lpCenterMap = 7;
type
   tLegendParameters = record
      DrawItem,
      HorizontalLegend : boolean;
      LegendSize,
      MapPosition : byte;
   end;

const
   bfArrayMaxSize = 13000000;
   MaxFArrayPts = 25000;
   MaxLongWord = 4294967295;
   MaxWord16 = 65535;

type
   bfarray32 = array[0..bfArrayMaxSize] of float32;
   fArray = array[0..MaxFArrayPts] of float64;
   VectorType = array[1..3] of float64;
   VectorType32 = array[1..3] of float32;
   MatrixType = array[1..3,1..3] of float64;
   CircleFreqType = array[0..360] of integer;
   ElevArray = array[1..5] of integer;  {elevations surrounding a point: 1=SW, 2=SE, 3=NE, 4=NW, 5=1=SW}
   OneBySixFloatArray = array[1..6] of float64;
   tFloatArray1000 = array[0..1000] of float32;
   tElevFloatarray= array[1..5] of float32;

{variables used for contouring;  values can be modified to change memory requirements}
const
   BiggestTriangulation = 500000;
   DoubleContourMax = 2 * BiggestTriangulation + 1;
   MaxContourPoints =  BiggestTriangulation + 3;
   MaxContours = 250;

type
   tRPnt = array[1..MaxContourPoints] of float64;
   tPointerPnt = array[1..5] of ^tRPnt;
   tMomentStop = (msAll,msAfterMean,msAfterStdDev,msBeforeMedian,msIncludeLE90);

   tMEMPowerDefaults = packed record
      NumProfiles,
      NumPoles         : SmallInt;
      ValidDataRequired,
      FirstFreq,
      LastFreq         : float32;
      LogLogPlot       : boolean;
   end;
   tMomentVar = record
      NPts,Missing : int64;
      MinZ,MaxZ,mean,avg_dev,std_dev,svar,skew,curt,median,rmse,mae,LE90,
      Q1,Q3,
      PC1,PC2,PC5,PC95,PC98,PC99 : float64;
   end;



var
   ThumbnailDir,
   ProgramRootDir,
   MDTempDir       : PathStr;
   TrilobiteComputer,
   DualMonitor : boolean;
   ParallelRowsDone,EditsDone,
   ParallelRowsToDo,
   CombinedScreenWidth : integer;

{$IfDef VCL}
   claRed,claWhite,claBlack,claLime,claBlue,claNavy,claDarkGrey,claSilver,claFuchsia,claPurple,claAqua,claTeal,claGreen,claYellow,claOlive,claMaroon,claNearWhite,claCyan : tPlatformColor;
{$EndIf}

{$IfDef FMX}
   claNearWhite : tPlatformColor;
{$EndIf}

procedure SafeMakeDir(DirName : PathStr);
function ValidPath(Path : PathStr) : boolean;
function ParsePath(FName : PathStr) : tStringList;
function IniFileName : PathStr;

function LinkImageString(f2 : ShortString) : shortstring;


{$IfDef MSWindows}
   function GetAppDataFolder : AnsiString;
{$EndIf}

{$IfDef VCL}
   function SystemMemorySize : int64;
   function HardwareString(IncludeIP : boolean = true) : AnsiString;
   function IDMonitor(theForm : Forms.tForm) : Integer;

   function GetCpuSpeed: string;
   function GetScreenColorDepth : integer;
   function GetNumberProcessors : integer;
   function GetComputerNetName : AnsiString;
   function BuildString : shortString;
   function GetVideoCardName(): String;
{$EndIf}


{$IfDef FMX}
   var
      BuildString : shortString;

   function RGB(r,g,b : byte) : integer;
{$EndIf}


function SmartMemorySizeBytes(Memory : int64) : shortstring;

function RealToString(Value : float64; Long,Decimals : integer) : ANSIstring;
function IntegerToString(Value : integer; sLength : integer = -18) : ANSIstring;  //overload;

function BeforeSpecifiedString(InputString,SubString : shortstring) : AnsiString;
function AfterSpecifiedString(InputString : shortString; SubString : shortstring) : AnsiString;
function AfterSpecifiedStringANSI(InputString : ANSIString; SubString : shortstring) : AnsiString;

function BeforeSpecifiedCharacterANSI(var InputString : ANSIString; ch : AnsiChar; TrimIt : boolean = true; CutInput : boolean = false) : AnsiString;
function BeforeSpecifiedCharacter(var InputString : String; ch : Char; TrimIt : boolean = true; CutInput : boolean = false) : String;

function AfterSpecifiedCharacter(Input : AnsiString; ch : Ansichar) : AnsiString;

function ConvertPlatformColorToTColor(aColor : tPlatformColor) : tColor;
function ConvertTColorToPlatformColor(aColor : tColor) : tPlatformColor;
procedure ConvertPlatformColorToRGB(aColor : tPlatformColor; var r,g,b : byte);


{$IfDef MSWindows}
   function BeforeSpecifiedCharacterUnicode(var InputString : String; ch : Char; TrimIt : boolean = true; CutInput : boolean = false) : String;
   function ptCopy(const S : ANSIstring; Start,Long : integer) : AnsiString;
   function ptTrim(const S : ANSIstring): ANSIstring;
{$Else}
   function ptCopy(const S : string; Start,Long : integer) : String;
   function ptTrim(const S : string): string;
{$EndIf}

function ASymbol(Sym : tDrawingSymbol;  Color : TPlatformColor;  Size : byte) : tFullSymbolDeclaration;


{$IfDef RecordProblems}
   {$IfDef Android}
   const
      StartDebugFileSharing : boolean = false;
   {$EndIf}
var
   TheDebugLog : tstringlist;
   DebugFileName  : PathStr;
   procedure OpenDebugFile;  //(ProgramName : PathStr);
   procedure ClearDebugLog;
   procedure WriteLineToDebugFile(TheLine : AnsiString);
   procedure HighlightLineToDebugFile(TheLine : AnsiString);

   procedure WriteStringsToDebugFile(HeaderLog : tStrings; LineNumbers : boolean = false);
   procedure WriteStringListToDebugFile(HeaderLog : tStringList; LineNumbers : boolean = false);
{$EndIf}

const
   MaxFieldsInDB = 300;
type
   tSetFieldType = set of TFieldType;
   Array100Boolean = array[0..MaxFieldsInDB] of boolean;
const
   StringOrIntegerField : tSetFieldType = [ftString,ftInteger,ftSmallInt,ftLargeInt];

function ByteArrayToString(values : array of byte; alen : int64 = 0) : Ansistring;

procedure StringToByteArray(TStr : shortString; var values : array of byte);


{$IfDef VCL}
   function ShortEXEName : ANSIString;
   function EXENameWithBuild : ANSIString;
{$EndIf}


{$IfDef FMX}
var
   clRed,clWhite,clBlack, clLime,clBlue,clMaroon,clPurple,clGreen,clYellow,clSilver,clGray,clTeal,clNavy,clFuchsia,clAqua,clDkGray,clOlive : TAlphaColor;
{$EndIf}



var
   AllVis : Array100Boolean;



implementation

uses
   {$IfDef VCL}
      PetImage,
   {$EndIf}

   {$IfDef ExGis}
   {$Else}
      Petmar_db,
   {$EndIf}

  {$ifDef MICRODEM}
    BaseMap, DEMDefs,  DEMDef_routines,
  {$EndIf}
   Petmar;

var
  AFormat : TFormatSettings;


{$IfDef VCL}

    function ShortEXEName : ANSIString;
    begin
       Result := UpperCase(ExtractFileNameNoExt(Application.ExeName));
    end;

    function EXENameWithBuild : ANSIString;
    begin
       Result := ShortEXEName + ' build ' + BuildString;
    end;
{$EndIf}

{$IfDef MSWindows}
   function ptTrim(const S : ANSIstring): ANSIstring;
{$Else}
   function ptTrim(const S : string): string;
{$EndIf}
 var
   I, L: Integer;
 begin
   L := Length(S);
   I := 1;
   if (L > 0) and (S[I] > ' ') and (S[L] > ' ') then Exit(S);
   while (I <= L) and (S[I] <= ' ') do Inc(I);
   if I > L then Exit('');
   while S[L] <= ' ' do Dec(L);
   Result := Copy(S, I, L - I + 1);
end;


{$IfDef MSWindows}
function ptCopy(const S : ANSIstring; Start,Long : integer) : AnsiString;
{$Else}
function ptCopy(const S : string; Start,Long : integer) : String;
{$EndIf}
begin
   {$IfDef MSWindows}
      Result := Copy(s,Start,Long);
   {$Else}
      Result := s.Substring(pred(Start),Long);
   {$EndIf}
end;


function ByteArrayToString(values : array of byte; alen : int64 = 0) : ANSIstring;
var
   i : integer;
begin
   Result := '';
   if (alen = 0) then alen := values[0];
   for I := 1 to alen do Result := Result + chr(values[i]);
end;


procedure StringToByteArray(TStr : shortString; var values : array of byte);
var
   i : integer;
begin
   {$IfDef VCL}
      for i := 1 to Length(TStr) do values[i] := ord(TStr[i]);
   {$Else}
      for i := Low(TStr) to High(TStr) do values[i] := ord(TStr[i]);
   {$EndIf}
end;

(*
function FullGetHomePath : PathStr;
begin
   Result := System.IOUtils.tPath.GetHomePath + PathDelim;
end;


function FullGetTempPath : PathStr;
begin
    Result := System.IOUtils.tPath.GetTempPath;
end;
*)


function SQLTypeDefString(ft : tFieldType; len : integer) : shortString;
begin
    case ft of
        ftString : Result := 'varchar(' + IntToStr(len) + ')';
        ftInteger : Result := 'integer';
        ftSmallInt : Result := 'smallint';
        ftFloat : Result := 'float64';
    end;
end;



function ASymbol(Sym : tDrawingSymbol; Color : TPlatformColor;  Size : byte) : tFullSymbolDeclaration;
begin
   Result.DrawingSymbol := sym;
   Result.Color         := Color;
   Result.Size          := size;
end;


function LinkImageString(f2 : ShortString) : shortstring;
begin
   Result := '<img src="' + f2 + '">';
end;


function IntegerToString(Value : integer; sLength : integer = -18) : shortstring;
begin
   Result := IntToStr(value);
   while length(Result) < sLength do Result := ' ' + Result;
end;


function RealToString(Value : float64; Long,Decimals : integer) : shortstring;
begin
   try
      Str(Value:abs(Long):abs(Decimals),Result);
      {$IfDef RecordRealToString} WriteLineToDebugFile('initial: ' + Result); {$EndIf}
      if (Decimals < 0) then begin
         while Result[Length(Result)] = '0' do Delete(Result,length(Result),1);
         if Result[Length(Result)] = '.' then Delete(Result,length(Result),1);
         {$IfDef RecordRealToString} WriteLineToDebugFile('after (Decimals < 0):' + Result); {$EndIf}
      end;
      if (Long < 0) then begin
         while Result[1] = ' ' do Delete(Result,1,1);
      end;
   except
      Result := 'NaN';
   end;
end;


function SmartMemorySizeBytes(Memory : int64) : shortstring;
begin
   if (Memory < 2048000) then Result := RealToString(memory / 1024,-12,1) + ' kb'
   else if (Memory < 50000000) then Result := RealToString(memory / 1024 / 1024,-12,1) + ' Mb'
   else if (Memory < 1000000000) then Result := IntToStr(Memory div 1024 div 1024 ) + ' Mb'
   else Result := RealToString(1.0 * Memory / 1024 / 1024 / 1024 ,-12,1) + ' Gb'
end;

{$IfDef MSWindows}

function GetAppDataFolder : AnsiString;

         function GetSpecialFolderPath(Folder: Integer; CanCreate: Boolean): string;
         // from http://stackoverflow.com/questions/471123/accessing-localapplicationdata-equivalent-in-delphi
         // Gets path of special system folders
         // call GetSpecialFolderPath (CSIDL_PERSONAL, false)       returns folder as result
         var
            FilePath: array [0..255] of char;
         begin
            SHGetSpecialFolderPath(0, @FilePath[0], FOLDER, CanCreate);
            Result := FilePath;
         end;

begin
   Result := GetSpecialFolderPath(CSIDL_APPDATA,true);
end;

{$EndIf}


{$IfDef VCL}
   {$I ..\common_code\petmar_types_vcl.inc}
{$EndIf}


procedure SafeMakeDir(DirName : PathStr);
//var
   //IntDirs : tStringList;
   //i : integer;
begin
   if (DirName = '') then begin
      {$IfDef RecordMakeDir} WriteLineToDebugFile('Petmar.SafeMakeDir called with empty directory name'); {$EndIf}
      DirName := MDtempDir + 'new_folder\';
   end;
   if not ValidPath(DirName) then begin
     {$IfDef RecordMakeDir} WriteLineToDebugFile('Petmar.SafeMakeDir try to make ' + DirName); {$EndIf}
     MkDir(DirName);
     {$IfDef RecordMakeDir} WriteLineToDebugFile('Petmar.SafeMakeDir try made OK ' + DirName); {$EndIf}
   end;
end;


function ValidPath(Path : PathStr) : boolean;
begin
   {$IfDef MSWindows}
      if length(Path)= 0 then Result := false
      else begin
         if Length(Path) <= 3 then begin
            {$IfDef MSWindows}
               Result := DiskSize(ord(Path[1]) - 64) <> -1;
            {$EndIf}
         end
         else begin
             if Path[length(Path)] = PathDelim then Delete(Path,length(Path),1);
             Result := DirectoryExists(Path);
         end;
      end;
   {$Else}
      Result := System.SysUtils.DirectoryExists(Path);
   {$EndIf}
end;


function ParsePath(FName : PathStr) : tStringList;
var
   i : integer;
begin
   Result := tStringList.Create;
   repeat
      i := 0;
      repeat
         inc(i);
      until (FName[i] in [PathDelim,':']) or (i = length(FName));
      if i = length(FName) then  Result.Add(Copy(fname,1,i))
      else if i > 1 then Result.Add(Copy(fname,1,pred(i)));
      Delete(FName,1,i);
   until (FName = '');
end;


function BeforeSpecifiedString(InputString : shortString; SubString : shortstring) : AnsiString;
var
   i : integer;
begin
   i := Pos(Substring,InputString);
   Result := Copy(InputString,1,pred(i));
end;

function AfterSpecifiedString(InputString : shortString; SubString : shortstring) : AnsiString;
var
   j : integer;
begin
   j := Pos(Substring,InputString);
   if (J = 0) then Result := ''
   else begin
      Result := InputString;
      Delete(Result,1,pred(j + Length(SubString)));
   end;
end;


function AfterSpecifiedStringANSI(InputString : ANSIString; SubString : shortstring) : AnsiString;
var
   j : integer;
begin
   j := Pos(Substring,InputString);
   if (J = 0) then Result := ''
   else begin
      Result := InputString;
      Delete(Result,1,pred(j + Length(SubString)));
   end;
end;


function BeforeSpecifiedCharacterANSI(var InputString : ANSIString; ch : AnsiChar; TrimIt : boolean = true; CutInput : boolean = false) : AnsiString;
var
   i : integer;
begin
   {$IfDef MSWindows}
      i := -1;
      repeat
         inc(i);
      until (i = length(InputString)) or (InputString[succ(i)] = ch);
      Result := (Copy(InputString,1,i));
      if TrimIt then Result := Trim(Result);
      if CutInput then begin
         Delete(InputString,1,succ(i));
         while (InputString <> '') and (InputString[1] = ch) do Delete(InputString,1,1);
      end;
   {$Else}
      Result := BeforeSpecifiedCharacter(InputString,ch,TrimIt,CutInput);
   {$EndIf}
end;


function BeforeSpecifiedCharacter(var InputString : String; ch : Char; TrimIt : boolean = true; CutInput : boolean = false) : String;
var
   i : integer;
begin
   {$IfDef MSWindows}
      i := -1;
      repeat
         inc(i);
      until (i = length(InputString)) or (InputString[succ(i)] = ch);
      Result := (Copy(InputString,1,i));
      if TrimIt then Result := Trim(Result);
      if CutInput then begin
         Delete(InputString,1,succ(i));
         while (InputString <> '') and (InputString[1] = ch) do Delete(InputString,1,1);
      end;
   {$Else}
      i := -1;
      repeat
         inc(i);
      until (i = Length(InputString)) or (InputString.Chars[succ(i)] = ch);
      Result := InputString.Substring(0,i+1);
      if TrimIt then Result := Result.Trim;
      if CutInput then begin
         TStr := InputString;
         InputString := '';
         for j := i+2 to High(Tstr) do InputString := InputString + TStr.Chars[j];
         while (InputString <> '') and (InputString.Chars[1] = ch) do InputString.Remove(0,1);
      end;
   {$EndIf}
end;


function BeforeSpecifiedCharacterUnicode(var InputString : String; ch : Char; TrimIt : boolean = true; CutInput : boolean = false) : String;
var
   i : integer;
begin
   {$IfDef MSWindows}
      i := -1;
      repeat
         inc(i);
      until (i = length(InputString)) or (InputString[succ(i)] = ch);
      Result := (Copy(InputString,1,i));
      if TrimIt then Result := Trim(Result);
      if CutInput then begin
         Delete(InputString,1,succ(i));
         while (InputString <> '') and (InputString[1] = ch) do Delete(InputString,1,1);
      end;
   {$Else}
      if StrUtils.AnsiContainsText(InputString,ch) then begin
         I := Low(InputString);
         j := 0;
         while (i <= High(InputString)) do begin
            if (InputString.Chars[i] = ch) then break;
            inc(j);
            inc(i);
         end;
         {$IfDef MacOS}
            Result := {#0 +} InputString.Substring(pred(Low(InputString)),succ(j));
            if CutInput then InputString := {#0 +} InputString.Substring(Low(InputString)+j+1,Length(InputString)-j);
         {$Else}
            Result := InputString.Substring(Low(InputString),j);
            if CutInput then InputString := InputString.Substring(Low(InputString)+j+1,Length(InputString)-j);
         {$EndIf}
      end
      else Result := InputString;
   {$EndIf}
end;


function AfterSpecifiedCharacter(Input : AnsiString; ch : Ansichar) : AnsiString;
var
   i : integer;
begin
   i := 0;
   repeat
      inc(i);
   until (i = length(Input)) or (Input[succ(i)] = ch);
   Result := Input;
   Delete(Result,1,succ(i));
end;

 (*
function MyDataPath : PathStr;
begin
  Result := TPath.GetDocumentsPath + PathDelim + 'MyMapData\';
end;
 *)


function IniFileName : PathStr;
var
   pName : shortString;
begin
   {$IfDef MSWindows}
      //pName := uppercase(ExtractFileNameNoExt(ParamStr(0)));
      pName := 'microdem';
      {$IfDef VCL}
         if (UpperCase(Copy(pName,1,8)) = 'MICRODEM') or (UpperCase(Copy(pName,1,4)) = 'MD64') or (UpperCase(Copy(pName,1,4)) = 'MD32') then pName := 'microdem';
         Result := GetAppDataFolder + PathDelim + pName + PathDelim;
      {$Else}
         Result := GetAppDataFolder + PathDelim + pName + PathDelim;
      {$EndIf}
      if not ValidPath(Result) then begin
         SafeMakeDir(Result);
      end;
      Result:= Result + pName + '.ini';
   {$Else}
       {$IfDef iOS}
          DebugFileName := TPath.GetHomePath + '/microdem.ini';
       {$Else}
         Result := TPath.GetSharedDownloadsPath;
         Result := ptCopy(Result,1,length(Result) - 8) + 'microdem/ini/microdem.ini';
       {$EndIf}
   {$EndIf}
end;


{$IfDef RecordProblems}


   procedure OpenDebugFile;
   {$IfDef VCL}
   var
       hs : AnsiString;
       ch : AnsiChar;
   {$EndIf}
   var
      TStr : shortstring;
      LastName : PathStr;
   begin
      TheDebugLog := tStringList.Create;
      TheDebugLog.Add(TimeToStr(Now,aFormat) + ' Opened Debug log: ' + DebugFileName);
      TheDebugLog.Add('Log from ' + DateToStr(Date));
      {$IfDef DEBUG}
         TStr := ' debug version';
      {$ELSE}
         TStr := ' release version';
      {$ENDIF}

      {$IfDef VCL}
         TheDebugLog.Add('EXE: ' + Application.ExeName);
         TheDebugLog.Add('Build: ' + ShortEXEName +  ' ' + BuildString + TStr);
      {$Else}
         TheDebugLog.Add('Build: ' + BuildString);
      {$EndIf}
      TheDebugLog.Add('Compiled: Delphi compiler ' + FloatToStr(System.CompilerVersion));
      {$IfDef ExGIS}
      {$Else}
         TheDebugLog.Add('DB driver: ' + DBDriver);
      {$EndIf}
      TheDebugLog.Add('INI: ' + IniFileName);
      TheDebugLog.Add('');

      {$IfDef VCL}
         TheDebugLog.Add(TimeToStr(Now,aFormat) + ' GetHardware');
         hs := HardwareString(false);
         ch := #13;
         while length(hs) > 0 do TheDebugLog.Add(BeforeSpecifiedCharacterANSI(hs,ch,true,true));
         TheDebugLog.Add('');
      {$EndIf}

      if (DebugFilename = '') then DebugFilename := 'c:\microdem\logs\microdem_debug.txt';

      if FileExists(DebugFilename) then begin
         LastName := ExtractFilePath(DebugFilename) + 'last_MD_debug_file.txt';
         RenameFile(DebugFileName,LastName);
      end;

      TheDebugLog.Add(TimeToStr(Now,aFormat) + ' GotHardware');
      TheDebugLog.SaveToFile(DebugFilename);
   end;


   procedure ClearDebugLog;
   begin
      if MDdef.MDRecordDebugLog then begin
         TheDebugLog.Clear;
         WriteLineToDebugFile('Debug log cleared');
      end;
   end;

   function OKwriteDebugLog : boolean;  inline;
   begin
      Result := MDdef.MDRecordDebugLog and (DebugFileName <> '') and (not ThreadsWorking) and (TheDebugLog <> Nil);
   end;

   procedure HighlightLineToDebugFile(TheLine : AnsiString);
   begin
      WriteLineToDebugFile('');
      WriteLineToDebugFile('**********');
      WriteLineToDebugFile(TheLine);
      WriteLineToDebugFile('**********');
   end;

   procedure WriteLineToDebugFile(TheLine : AnsiString);
   begin
      if (DebugFileName <> '') and OKwriteDebugLog then begin
         TheDebugLog.Add(TimeToStr(now,aFormat) + '  ' + TheLine);
         try
            TheDebugLog.SaveToFile(DebugFilename);
         except
            on Exception do begin end;
         end;
      end;
   end;


   procedure WriteStringListToDebugFile(HeaderLog : tStringList; LineNumbers : boolean = false);
   var
      i : integer;
      TStr : string10;
   begin
      if OKwriteDebugLog and (HeaderLog <> Nil) then begin
         try
            for i := 0 to pred(HeaderLog.Count) do begin
               if LineNumbers then TStr := IntegerToString(i,7) + ':  '
               else TStr := '     ';
               TheDebugLog.Add(TStr + HeaderLog.Strings[i]);
            end;
            TheDebugLog.SaveToFile(DebugFilename);
            ApplicationProcessMessages;
         finally
         end;
      end;
   end;


   procedure WriteStringsToDebugFile(HeaderLog : tStrings; LineNumbers : boolean = false);
   var
      i : integer;
      TStr : string10;
   begin
      if OKwriteDebugLog then try
         for i := 0 to pred(HeaderLog.Count) do begin
            if LineNumbers then TStr := IntegerToString(i,7) + ':  '
            else TStr := '';
            TheDebugLog.Add(TStr + HeaderLog.Strings[i]);
         end;
         TheDebugLog.SaveToFile(DebugFilename);
         ApplicationProcessMessages;
      finally
      end;
   end;

{$EndIf}


function ConvertPlatformColorToTColor(aColor : tPlatformColor) : tColor;
begin
   {$IfDef VCL}
      Result := RGB(aColor.rgbtRed,aColor.rgbtGreen,aColor.rgbtBlue);
   {$EndIf}

   {$IfDef FMX}
      Result := (tAlphaColorRec(aColor).R) + 256 * (tAlphaColorRec(aColor).G) + 256 * 256 * (tAlphaColorRec(aColor).B) ;
   {$EndIf}
end;

procedure ConvertPlatformColorToRGB(aColor : tPlatformColor; var r,g,b : byte);
begin
   {$IfDef VCL}
      r := aColor.rgbtRed;
      g := aColor.rgbtGreen;
      b := aColor.rgbtBlue;
   {$EndIf}

   {$IfDef FMX}
      r := (tAlphaColorRec(aColor).R);
      g := (tAlphaColorRec(aColor).G);
      b := (tAlphaColorRec(aColor).B);
   {$EndIf}
end;


function ConvertTColorToPlatformColor(aColor : tColor) : tPlatformColor;
var
   r,g,b : byte;
begin
   GetRGBfromTColor(aColor,r,g,b);
   Result := RGBtrip(r,g,b);
end;


{$IfDef FMX}

   function RGB(r,g,b : byte) : integer;
   begin
      Result := r  + g * 256 + b * 256*256;
   end;

   procedure FMXinit;
   begin
      clNearWhite := RGB(254,254,254);
      clRed := claRed;
      clWhite := claWhite;
      clBlack := claBlack;
      clLime := claLime;
      clBlue := claBlue;
      clNavy := claNavy;
      clDkGray := claDarkGrey;
      clSilver := claSilver;
      clFuchsia := claFuchsia;
      clPurple := claPurple;
      clAqua := claAqua;
      clTeal := claTeal;
      clGreen := claGreen;
      clYellow := claYellow;
      clOlive := claOlive;
      clMaroon := claMaroon;
      claNearWhite := ConvertTColorToPlatFormColor(clNearWhite);
   end;
{$EndIf}


{$IfDef VCL}
   procedure VCLinit;
   begin
      clNearWhite := RGB(254,254,254);

      claCyan := ConvertTColorToPlatFormColor(RGB(0,255,255));
      claRed := ConvertTColorToPlatFormColor(clRed);
      claWhite := ConvertTColorToPlatFormColor(clWhite);
      claNearWhite := ConvertTColorToPlatFormColor(clNearWhite);
      claBlack := ConvertTColorToPlatFormColor(clBlack);
      claLime := ConvertTColorToPlatFormColor(clLime);
      claBlue := ConvertTColorToPlatFormColor(clBlue);
      claNavy := ConvertTColorToPlatFormColor(clNavy);
      claDarkGrey := ConvertTColorToPlatFormColor(clGray);
      claSilver := ConvertTColorToPlatFormColor(clSilver);
      claFuchsia := ConvertTColorToPlatFormColor(clFuchsia);
      claPurple := ConvertTColorToPlatFormColor(clPurple);
      claAqua := ConvertTColorToPlatFormColor(clAqua);
      claTeal := ConvertTColorToPlatFormColor(clTeal);
      claGreen := ConvertTColorToPlatFormColor(clGreen);
      claYellow := ConvertTColorToPlatFormColor(clYellow);
      claOlive := ConvertTColorToPlatFormColor(clOlive);
      claMaroon := ConvertTColorToPlatFormColor(clMaroon);
   end;
{$EndIf}


initialization
   AFormat := System.SysUtils.TFormatSettings.Create;
   AFormat.LongTimeFormat := 'hh:mm:ss.zzz';
   {$IfDef MessageStartupProblems} MessageToContinue('Startup petmar_types'); {$EndIf}
   {$IfDef FMX} FMXinit; {$EndIf}
   {$IfDef VCL} VCLinit; {$EndIf}
finalization
   {$If Defined(RecordClosing)} WriteLineToDebugFile('Closing petmar_types in dbfn=' + DebugFileName); {$EndIf}
end.









