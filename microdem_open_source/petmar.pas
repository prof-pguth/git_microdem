unit petmar;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program      }
{ PETMAR Trilobite Breeding Ranch   }
{ Released under the MIT Licences   }
{ Copyright (c) 2023 Peter L. Guth  }
{___________________________________}


{$I nevadia_defines.inc}

{$IfDef FMX}
   {$Define UseIndyDownLoads}
{$EndIf}

{$IfDef RecordProblems} //normally only defined for debugging specific problems
   {$Define RecordShellExecute} //this should generally be on; if not desired, shut down in Windows defaults
   //D{$Define RecordWebDownloads}

   {$IfDef Debug}
      //{$Define RecordColorPalette}
      //{$Define RecordDialogs}
      //{$Define ShowProgressBarSize}
      //{$Define RecordFileDelection}

      //{$Define RecordHelp}
      //{$Define TrackFileDeletion}
      //{$Define RecordRandomize}
      //{$Define RecordUnzips}
      //{$Define RecordPrint}
      //{$Define TrackFormPlacement}
      //{$Define RecordFileFilter}
      //{$Define RecordStartup}
      //{$Define RecordFindFiles}
      //{$Define TempFileClose}
      //{$Define RecordBitmap}
      //{$Define RecordLegends}
      //{$Define RecordLegends}
      //{$Define RecordListPick}
      //{$Define RecordGeology}
   {$EndIf}
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
//end DB functions

   StrUtils,Classes,SysUtils,  Math,
   System.IOUtils,System.Types,System.Math.Vectors, System.UIConsts,System.UITypes,

   {$IfDef VCL}
      Windows, Winspool,ComCtrls,Forms,Printers,Graphics,Buttons,StdCtrls,WinSock,
      Controls, Dialogs, Menus,ExtCtrls,ClipBrd,FileCtrl, StdActns, HTMLHelpViewer,  System.Helpintfs,
   {$EndIf}

   {$IfDef FMX}
      FMX.Controls, FMX.Types, FMX.Graphics,
   {$EndIf}

   {$IfDef UseIndyDownLoads}
      IdComponent, IdTCPConnection, IdTCPClient, IdHTTP,
   {$EndIf}

   {$IfDef RecordTime}
      System.Diagnostics,System.TimeSpan,
   {$Endif}

   {$IfDef NoZips}
   {$Else}
      Zip,
   {$EndIf}

   {$IfDef MSWindows}
      ShlObj, ActiveX,URLMon,Registry,Messages,ShellAPI,
   {$EndIf}

   Petmar_types;

procedure InitializePetmar;
procedure Delay(ms : Cardinal);
function FileCanBeOpened(fName : PathStr) : boolean;

procedure ShowHourglassCursor; inline;
procedure ShowDefaultCursor; inline;
procedure ApplicationProcessMessages; inline;

function RemoveUnderScores(LegendStr : ANSIString) : ANSIString;
function SpacesToUnderScores(LegendStr : ANSIString) : ANSIString;
procedure FixFileNameBackslashes(var fName : PathStr);

function CreateHTML_href(fName,Display : ANSIString) : ANSIString;


var
   SevenZipfName : PathStr;

{$IfDef VCL}

function File2Trash(const FileName: string): boolean;

procedure StartThreadTimers(Capt : ShortString = ''; MaxThreads : integer = 8; Enable1 : boolean = false);
procedure EndThreadTimers;
procedure StartSingleThreadTimer(Capt : ShortString = '');

procedure InsureFormIsOnScreen(TheForm : Forms.tForm);
procedure PlaceFormInCorner(Owner,TheForm : Forms.tForm; FormPosition :  byte = lpSEMap);


procedure HardwareOnLine;
procedure PETMARAboutBox(ProgramName : ANSIstring; Modifier : ANSIstring = '');

function GetNewBMPSize(var x,y : int32; aCapt : shortstring) : boolean;

procedure ShowInNotepadPlusPlus(fName : PathStr; Capt : shortstring = '');   overload;
procedure ShowInNotepadPlusPlus(inName : tStringList; Capt : shortstring = '');  overload;

procedure ModalEditWindow(FName : PathStr; WindowCaption : ANSIstring);
procedure ModalStringListDisplay(var aList : tStringList; WindowCaption : ANSIstring; Purge : boolean = true);
procedure DisplayAndPurgeStringList(var Findings : tStringList; WindowTitle : shortString;  ListenToClear : boolean = false);
procedure DisplayString(Findings : String; WindowTitle : shortString; ListenToClear : boolean = false);
procedure QuickOpenEditWindow(FName : PathStr; WindowCaption : ANSIstring; ListenToClear : boolean = false; AddLineNumbers : boolean = false);

procedure GetDate(var Month,Day,Year : integer);
procedure GetDateAndDuration(var Month,Day,Year,Duration : integer; ShowDuration : boolean = true);

procedure GetSymbol(var DrSymbol : tDrawingSymbol; var SymSize   : byte; var WantColor  : tPlatformColor; WhatFor : shortstring = '');
procedure PickLineSizeAndColor(WhatFor : shortString; BitButton : tBitBtn; var LineColor : tPlatformColor; var LineSize :  byte); overload;
procedure PickLineSizeAndColor(WhatFor : shortString; BitButton : tBitBtn; var LineColor : tPlatformColor; var LineSize :  float32); overload;
procedure PickLineSizeAndColor(WhatFor : shortString; BitButton : tBitBtn; var LineColor : tPlatformColor; var LineSize :  integer); overload;
procedure PickSymbol(BitButton : tBitBtn; var DrSymbol : tDrawingSymbol; var SymSize : byte; var WantColor  : tPlatformColor; WhatFor : shortstring = ''); overload;
procedure PickSymbol(BitButton : tBitBtn; var Symbol : tFullSymbolDeclaration; WhatFor : shortstring = ''); Overload;

procedure PickPattern(WhatFor : shortString; var Style : tBrushStyle; var FillColor,BorderColor : tPlatformColor; var inBorderWidth : integer; BackBMP : tMyBitmap = Nil);

//Bit button routines
      procedure ColorLineWidthBitmap(var Bitmap : tMyBitmap; Color : TPlatformColor; Size : integer; Minimize : boolean = true);
      procedure DrawPatternOnBitmap(var Bitmap : tMyBitmap; FillColor,SymbolLineBorderColor : tPlatformColor; SymbolFill :  tBrushStyle; SymbolLineBorderSize : integer);

      procedure ColorLineWidthBitBtn(var BitBtn : tBitBtn; Color : tPlatformColor; Size : integer);
      procedure SymbolOnButton(var BitBtn : tBitBtn; Symbol : tDrawingSymbol; SymbolSize : integer; SymbolColor : tPlatformColor); Overload;
      procedure SymbolOnButton(var BitBtn : tBitBtn; Symbol : tFullSymbolDeclaration); Overload;
      procedure FillPatternOnBitBtn(BitBtn : tBitBtn; FillColor,SymbolLineBorderColor : tPlatformColor; SymbolFill :  tBrushStyle; SymbolLineBorderSize : integer);
      {$IfDef VCL}
         procedure ColorBitBtn(var BitBtn : tBitBtn; Color : tPlatformColor);  overload;
         procedure ColorBitBtn(var BitBtn : tBitBtn; Color : tColor);          overload;
      {$EndIf}


procedure TextOutVertical(Canvas : TCanvas; x,y : integer; Words : ShortString; ClearBack : boolean = false);
procedure CanvasTextOutAngle(c : TCanvas; x,y: Integer; d : Word; s: ShortString);
     {angle measured in tenths degree, math convention: starts with east and runs counterclockwise}

procedure DisplayNevadella(Canvas : TCanvas; xoff,yoff,size : integer; Color : TColor);
procedure CheckFormPlacement(TheForm : Forms.tForm); //MonitorDesired : integer = 0);
procedure PlaceFormAtMousePosition(TheForm : Forms.tForm);

function LocalIP: string;

function FontInstalled(Const FontName:String) : Boolean;


//drawing
   procedure CrossWithHole(Canvas : tCanvas; x,y : integer);
   procedure ScreenSymbol(Canvas : TCanvas; x,y : integer; Sym : tDrawingSymbol; size: integer; color : TPlatformColor); overload;
   procedure ScreenSymbol(Canvas : TCanvas; x,y : integer; Symbol : tFullSymbolDeclaration); overload;

      procedure StartCount(Title : ShortString);
      procedure UpdateCount(HowFar : LongInt);
      procedure EndCount;

function ExecuteFile(const FileName, Params, DefaultDir: ANSIstring): THandle;
function WinExecAndWait32(FileName : ANSIstring; Wait : boolean = true; Log : boolean = true) : integer;

function GetFromListZeroBased(InMessage : ANSIstring; var PickedNum : integer; var InList : TStringList; CanCancel : boolean = false; MultiPick : boolean = false) : boolean;
function GetFromList(InMessage : ANSIstring; var PickedNum : integer; InList : TStringList; CanCancel : boolean = false) : boolean;    overload;
function GetFromList(InMessage : ANSIstring; InList : TStringList; CanCancel : boolean = false) : ShortString; overload;
function GetMultipleFromList(InMessage : ANSIstring; var PickedNum : integer; var InList : TStringList; CanCancel : boolean = false) : boolean;

procedure GetString(Prompt : ShortString; var Input : ShortString; ForceCaps : boolean; ValidChars : characters);
procedure GetValidDBfieldname(var fName2 : shortstring);

procedure QueryTColor(var Color : tColor); overload;
procedure QueryColor(var Color : tPlatformColor); overload;
procedure QueryColor(BitBtn : tBitBtn; var Color : tPlatformColor); overload;
procedure QueryTColor(BitBtn : tBitBtn; var Color : Tcolor); overload;
procedure FillComboBoxWithColorPalettes(fName : PathStr; ComboBox1 : tComboBox);
procedure GetAngleInRadians(Message : shortstring; var Angle : float64);
procedure GetAngle(Mess : shortstring; var theAngle : float64; var AngleMeasure : tAngleMeasure);


//color routines
      function ColorString(Color : tRGBTriple) : shortString; overload;

      function GrayColorFunct(i : integer) : TColor;  overload;
      function GrayColorFunct(z,Min,Max : float64) : TColor; overload; inline;
      function SpectrumColorFunct(z,Min,Max : float64) : tColor;
      function RainbowColorFunct(z,Min,Max : float64) : TColor;
      function TerrainTColor(z,aMin,aMax : float64) : TColor;
      function ColorToHtml(DColor : TColor) : ShortString;

      function PlatformRainbowColorFunct(z,Min,Max : float64) : TPlatformColor;

      procedure tColortoHLS(Color : tColor; var h,l,s : float64);
      function HLStoTcolor(H,L,S : float64) : tcolor;
      procedure RGBtripToHLS(Color : tRGBTriple; var h,l,s : float64);

      function MonthColor(i : integer) : TPlatformColor;
      function SameColor(c1,c2 : tPlatformColor) : boolean; inline;

//legends
      function MakeColorScaleBitmap(width,height : integer; ColorScheme : tLegendColors; ColorTable : tColorTableDefinitions) : tMyBitmap;
      function HorizontalLegendOnBitmap(Colors : tColors256; Values : array of float64; Units,LegendTitle : shortstring; LegendSize : integer = 1) : tMyBitmap;
      function DefaultHorizontalLegendOnBitmap(Min,Max : float64; Units,LegendTitle : shortstring; Legend : tLegendColors; ChloroplethScheme : shortstring = ''; Reverse : boolean = false) : tMyBitmap;
      function DefaultVerticalLegendOnBitmap(Min,Max : float64; Units,LegendTitle : shortstring; Legend : tLegendColors; ChloroplethScheme : shortstring = ''; Reverse : boolean = false) : tMyBitmap;
      function VerticalLegendOnBitmap(Colors : tColors256; Values : array of float64; Units : shortstring; LegendTitle : shortstring = '') : tMyBitmap;

//font routines
      procedure LoadMyFontIntoWindowsFont(MyFont : tMyFont; WindFont : tFont);
      procedure GetMyFontFromWindowsFont(var MyFont : tMyFont; WindFont : tFont);
      procedure InitializeMyFont(var MyFont : tMyFont; Name : shortstring; Size : SmallInt; Color : tPlatformColor);
      procedure EditMyFont(var MyFont : tMyFont);
      procedure EditTFont(Font : tFont);
      function MyFontToString(MyFont : tMyFont) : ShortString;


procedure StopSplashing;

{$EndIf}


//progress routines, which do nothing in NoProgress is defined
      procedure StartProgress(Title : ShortString); //overload;
      procedure StartProgressAbortOption(Title : ShortString); //overload;
      procedure UpdateProgressBar(HowFar : float64);
      procedure EndProgress;

function AnswerIsYes(Prompt : ANSIstring) : boolean; overload;
procedure MessageToContinue(Mess : ANSIstring; SaveOutPut : boolean = true);

function CurrentTimeForFileName : PathStr;


procedure DisplayHTMLTopic(Name : AnsiString);
function DownloadFileFromWeb(WebName,LocalName : ANSIstring; ShowFailure : boolean = true) : boolean;

procedure GetSeparationCharacter(MenuStr : ANSIstring; var SepChar : ANSIchar);
procedure GetSeparationCharacterUnicode(MenuStr : string; var SepChar : char);


{$IfDef MSWindows}
   procedure StripBlanks(var St : Shortstring);  //overload;{removes blank characters from any ANSIstring}
{$Else}
   procedure StripBlanks(var St : ANSIString);  {removes blank characters from any ANSIstring}
{$EndIf}

procedure StripCharacter(var St : AnsiString; ch : AnsiChar);
procedure ReplaceCharacter(var St : AnsiString; FindChar,ReplaceChar : AnsiChar);

function RemoveLowerCase(k1 : ShortString) : shortString;
function RemoveNumbers(k1 : ShortString) : shortString;

function ValidByteRange(value : integer) : integer; inline;
function ValidByteRange254(value : integer) : integer; inline;

procedure DragDirections;

function MakeLongInt(v1,v2,v3,v4 : byte; BigEndian : boolean) : LongInt;

function NoCommas(TheString: ShortString) : shortstring;

procedure SortAndRemoveDuplicates(var StringList : tStringList);
procedure CopyStringList(FromList : tStringList; var ToList : tStringList);

function GetFromRequest(InString : ANSIstring; WantField : shortstring) : shortstring;
function GetFloatFromRequest(InString : ANSIstring; WantField : shortstring) : float64;
function GetIntegerFromRequest(InString : ANSIstring; WantField : shortstring) : integer;

function ReadFilter(var Filter : FilterType; var FilterSize,FilterLap,Sum : integer; var FilterName : PathStr; DisplayFilter : boolean = true) : boolean;

procedure ReadDefault(Prompt : ShortString; var IntVal : byte); overload;
procedure ReadDefault(Prompt : ShortString; var IntVal : int16); overload;
procedure ReadDefault(Prompt : ShortString; var IntVal : int32); overload;
procedure ReadDefault(Prompt : ShortString; var IntVal : int64); overload;
procedure ReadDefault(Prompt : ShortString; var RealVal : float64); overload;
procedure ReadDefault(Prompt : ShortString; var RealVal : float32); overload;
procedure ReadDefault(Prompt : ShortString;  var WordVal : word); overload;


function RGBtrip(r,g,b : integer; a : integer = 255) : TPlatformColor;  inline;
function GrayRGBtrip(i : integer; a : integer = 255) : TPlatformColor;  inline;

procedure ModifyRGBColor(Color : TPlatformColor; r,g,b : integer; a : integer = 255);


//file routines, file name and path names
      function GetExistingFileName(Mess,Filters : ANSIstring; var FName : PathStr) : boolean;
      function GetFileNameDefaultExt(Mess,Filters : ANSIstring; var FName : PathStr; AppendOption : boolean = false) : boolean;
      function GetFileNameDefaultExtSaveExt(Mess,Filters : ANSIstring; var FName : PathStr; var  df : Integer; AppendOption : boolean = false) : boolean;
      function GetMultipleFiles(InMessage,Masks : ANSIstring; var FilesWanted : TStringList; var DefaultFilter : byte) : boolean;
      function GetFileMultipleMask(InMessage,Masks : ANSIstring; var FileWanted : PathStr; var DefaultFilter : byte) : boolean;
      function GetMultipleDirectories(TheMessage : AnsiString; var Paths : tStringList) : boolean;
      function GetSubDirsInDirectory(BaseDir : PathStr) : tStringList;
      function GetFileSize(FileName : String) : Int64;
      function LastSubDir(Dir : AnsiString) : PathStr;
      function GetParentDirectory(path : Ansistring) : Ansistring;
      procedure StripInvalidPathNameChars(var fName : PathStr);
      function CopyFile(SourceFile,DestinationFile : string):  boolean;
      function MoveFile(SourceFile,DestinationFile : string):  boolean;
      function GetFileFromDirectory(InMessage : ANSIstring; Mask : ANSIstring;  var FileWanted : PathStr) : boolean;
      function GetDOSPath(TheMessage : AnsiString; var Path : PathStr) : boolean;
      procedure CleanUpFileName(var fname : PathStr);

      procedure FSplit(FName : PathStr; var Dir : DirStr; var Name : NameStr; var Ext : ExtStr);
      function ExtractFileNameNoExt(fName : PathStr) : NameStr;
      procedure FixForwardSlashes(var fName : PathStr);

      function FileTimeFromFileName(fName : PathStr) : ShortString;
      procedure InsureFileIsNotReadOnly(InputFileName: PathStr);
      procedure DeleteMultipleFiles(Dir : PathStr; Mask : ANSIstring);
      procedure CleanOutDirectory(BaseDir : PathStr);
      procedure DeleteFileIfExists(fName : PathStr);
      procedure FindMatchingFiles(Dir : PathStr; Mask : ANSIstring; var TheFiles : tStringList; DigLayers : integer = 0);  //;  StatusBar1 : tStatusBar = nil);
      function NextFileNumber(Path,fName : PathStr; Ext : ExtStr) : PathStr;
      function NextFilePath(Path : PathStr) : PathStr;

      function ExtEquals(Ext,Wanted : ExtStr) : boolean;
      function FileExtEquals(fName : PathStr; Wanted : ExtStr) : boolean;
      procedure UnblockFile(fName : PathStr);

      procedure RandomizeStringList(var TheList : tStringList; SpareFirst : boolean = true);


//values from strings
      function CheckEditString(Text : string; var OutValue : float64) : boolean; overload;
      function CheckEditString(Text : string; var OutValue : float32) : boolean; overload;
      function CheckEditString(Text : String; var OutValue : Word): boolean; overload;
      function CheckEditString(Text : string; var OutValue : int16) : boolean; overload;
      function CheckEditString(Text : string; var OutValue : int32) : boolean; overload;
      function CheckEditString(Text : string; var OutValue : int64) : boolean; overload;
      function CheckEditString(Text : string; var OutValue : byte) : boolean; overload;

      function GetIntegerFromString(var Input : ShortString; var Int : integer) : boolean;
      function GetRealFromString(var Input : ShortString; var Int : float64) : boolean;

//color functions
      function RainbowRGBFunct(z,MinV,MaxV : float64) : tPlatformColor; inline;
      function TerrainRGBFunct(z,Min,Max : float64) : tPlatformColor;
      function SpectrumRGBFunct(z,Min,Max : float64) : tPlatformColor;
      function OceanRGBFunct(z,Min,Max : float64) : tPlatformColor;

      function RGBtripFromHSI(Hue,Sat,Int : float64) : tPlatformColor;

      function ColorString(Color : tColor) : shortString; overload;
      function RGBString(r,g,b : SmallInt; IncludeGray : boolean = false) : ShortString;
      function ColorStringFromPlatformColor(Color : tPlatformColor) : shortString;

      procedure GetRGBfromTColor(Color : TColor; var r,g,b : byte);

      procedure RGBtoHLS(r,g,b : integer; var h,l,s : float64);
      procedure HLStoRGB(h,l,s : float64; var r,g,b : byte);

      function SelectedColorSchemeColorFunct(ColorScheme: tLegendColors; ColorTable : tColorTableDefinitions; z : float64; Min,Max : float64) : TColor;
      function ColorFromZColorTable(ZColorTable : tColorTableDefinitions; zj : float64; var k : integer) : tRGBTriple;
      {$IfDef VCL}
         function DefineColorArray(Palette : shortstring; var NumColors : integer; var ColorsTable : tZTableColors255; Reverse : boolean = false) : boolean;
      {$EndIf}


// print routines

      {$IfDef ExPrintFile}
      {$Else}
         //procedure PrintImageForceDPI(Image1 : TImage; TargetDPI : integer; ForceBW : boolean);
         procedure PrintImageToSpecifiedSize(Image1 : TImage; PrinterXInches,PrinterYInches : float64);
      {$EndIf}


function CurrentMilitaryTime(WithSeconds : boolean = false) : shortString;
function ArrayOfCharToString(LenArray : integer; var Chars : array of ANSIchar) : shortstring;


//byte order swapping
      procedure Int4Swap(var int : int32); overload;
      procedure Int4Swap(var int : cardinal); overload;
      procedure SwapToShortFloat(var FloatValue : float32);
      procedure SwapToFloat8byte(var FloatValue : Float64);

//image and clipboard routines
      {$IfDef VCL}
         procedure CopyPartOfScreenToBitmap(var Bitmap2 : tMyBitmap; Left,Top,Right,Bottom : integer);
         procedure CaptureActiveWindowToClipboard;
         procedure CaptureEntireScreenToClipboard;
      {$EndIf}

//ANSIstring routines with deg/min/sec and time
      procedure ConvertToDegMinSecString(Value : float64; OutPutMethod : tLatLongMethod;  var DegString,MinString,SecString : string10; HighAccuracy : boolean = true);
      function ConvertToDegreesString(Value : float64; OutPutMethod : tLatLongMethod; HighAccuracy : boolean = true) : shortstring;
      function LatLongDegreeToString(Lat,Long : float64; OutPutLatLongMethod : tLatLongMethod = DecDegrees) : shortstring;
      function LatLongRadToString(Lat,Long : float64; OutPutLatLongMethod : tLatLongMethod = DecDegrees) : shortstring;
      function LatToString(Lat : float64; OutPutLatLongMethod : tLatLongMethod) : shortstring;
      function LongToString(Long : float64;  OutPutLatLongMethod : tLatLongMethod) : shortstring;
      function HoursMinutesString(Time : float64; WithSec : boolean = false) : ShortString;
      function LatLongToStringForFileName(Lat,Long : float64) : shortstring;
      function LatToString3(Lat : float64) : shortstring;
      function LongToString4(Long : float64) : shortstring;

//format strings with units
      {$IfDef MICRODEM}
         function SmartDistanceMetersFormat(d : float64) : shortString;
         function SmartVolumeFormat(d : float64; DistanceUnits : tDistanceUnits = disMetric) : shortString;
         function SmartAreaFormat(d : float64) : shortString;
      {$EndIf}

      function SmartNumberPoints(NumPoints : int64) : shortstring;
      function AngleFormat(Angle : float64; Units : tAngleMeasure; ShowUnits : boolean = true) : shortstring;
      function FormatString(TheString : shortString; Len : integer; Justify : tJustify) : shortString;
      function CreateTimeString(Hour,Minute,Second : integer) : shortString;


const
   QuietActions : boolean = false;
   clBrown = 8732621;   //RGB(205,133,63);
   clMagenta = 255*256*256 + 255;

var
   RGBTripleNearWhite,RGBTripleWhite,RGBtripleAlmostBlack,RGBtripleBlack,RGBTripleRed,claBrown : tPlatformColor;
   clNearWhite,
   clNearKMLWhite,
   clAlmostBlack : tColor;
   HeavyDutyProcessing,
   DEMIXProcessing,
   LoadingFromMapLibrary,
   WantOut,WantShowProgress : boolean;
   RevisionDate : shortstring;
var
   ThreadsNumDone,ThreadsToDo : int64;
   ThreadsWorking : boolean;

   {$IfDef RecordTime}
   var
      Stopwatch,Stopwatch1,StopWatch2 : tStopWatch;
      Elapsed   : tTimeSpan;
   {$EndIf}


procedure RemoveASCII0FromFile(fName : PathStr);

function FindDriveWithFile(var fName : PathStr) : boolean;
function FindDriveWithPath(var aPath : PathStr) : boolean;


function FindFieldInStringList(Header : tStringList; FieldName : ANSIstring; SepChar : ANSIchar) : ANSIstring;
function FindIntegerFieldInStringList(Header : tStringList; FieldName : ANSIstring; SepChar : ANSIchar) : Integer;
function FindFloatFieldInStringList(Header : tStringList; FieldName : ANSIstring; SepChar : ANSIchar) : float64;

function UpdateRate(NumRecs : integer) : integer;
function TrueOrFalse(BoolVar : boolean) : shortstring;
function BitmapSizeString(Bitmap : tMyBitmap) : shortstring;


{$IfDef ExCompress}
{$Else}
   function SevenZipPresent : boolean;
   procedure Missing7ZipMessage;
   function MainGzip(var fName : PathStr; Outfile : PathStr = '') : boolean;
   function MainExtar(fName : PathStr; Outfile : PathStr = '') : boolean;
   function MainBZ2(var fName : PathStr; Outfile : PathStr = '') : boolean;
   function ZipMasterUnzip(ZipName,OutDir : ShortString) : boolean;
   function ZipMasterZipFiles(ZipName : PathStr; TheFiles : tStringList) : boolean;
   function Main7Z(var fName : PathStr; Switches : shortstring = '') : boolean;
   procedure UnzipSingleFile(var fName : PathStr);
{$EndIf}


function GetVolumeName(DriveLetter: ANSIChar): shortstring;
procedure OpenRecycleBin;

procedure WriteOpenHandlestoDebugLog(Where : shortString);


implementation

uses


{$IfDef VCL}
   Nevadia_Main,
   Thread_timers,
   TerSplsh,

   Get_angle,
   PETEd32,      {form with editor, which also displays text results}
   PETBMPSize,   {form to change the size of the bitmap and the graph}
   PETDate,
   PETMARAbout,  {about box form for Help, About menu choice}
   PETGetLine,
   PickFillPattern,
   Message_Continue,
   PETSymbol,
   PetProgr,
   PetListF,
   PETForm,      {form with common dialogs for use}
{$EndIf}

   DEMDefs,
   {$IfDef ExGIS}
   {$Else}
      petdbutils,
   {$EndIf}
   PetImage,
   PETMath;      {math library}

var
   TerrainCuts : array[0..4,1..3] of integer;



procedure WriteOpenHandlestoDebugLog(Where : shortString);

      //function GetProcessHandleCount(hProcess: THandle; var pdwHandleCount: DWORD): BOOL;  stdcall; {external} 'Kernel32.dll' name 'GetProcessHandleCount';

      function GetOpenHandles : DWORD;
      //https://stackoverflow.com/questions/4966900/delphi-causes-of-system-error-1158-no-more-system-handles-for-current-process
      begin
       if not GetProcessHandleCount(GetCurrentProcess,Result) then
           RaiseLastOSError;
      end;

begin
   WriteLineToDebugFile(Where + '  handles open=' + IntToStr(GetOpenHandles));
end;

function FindDriveWithPath(var aPath : PathStr) : boolean;
//used for data files on external hard drive, which can have different drive letters when moved
var
   ch : ANSIchar;
begin
   Result := PathIsValid(aPath);
   ch := 'a';
   if not Result then begin
      for ch := 'a' to 'z' do begin
         aPath[1] := ch;
         if PathIsValid(aPath) then begin
            Result := true;
            exit;
         end;
      end;
      Result := false;
   end;
end;


function FindDriveWithFile(var fName : PathStr) : boolean;
//used for data files on external hard drive, which can have different drive letters when moved
var
   ch : ANSIchar;
begin
   Result := FileExists(fName);
   ch := 'a';
   if not Result then begin
      for ch := 'a' to 'z' do begin
         fName[1] := ch;
         if FileExists(fName) then begin
            Result := true;
            exit;
         end;
      end;
      Result := false;
   end;
end;



function BitmapSizeString(Bitmap : tMyBitmap) : shortstring;
begin
   Result := ' Bitmap size: ' + BitmapSize(Bitmap);
end;


function FileTimeFromFileName(fName : PathStr) : ShortString;
begin
   Result := DateTimeToStr(FileDateToDateTime(FileAge(fName)));
            Petmar.ReplaceCharacter(Result,'/','_');
            Petmar.ReplaceCharacter(Result,':','_');
            Petmar.ReplaceCharacter(Result,' ','_');
end;


procedure OpenRecycleBin;
var
    recycleBinPIDL: PItemIDList;
    execInfo: TShellExecuteInfo;
    Handle : tHandle;
begin
    SHGetSpecialFolderLocation(Handle, CSIDL_BITBUCKET, recycleBinPIDL);
    with execInfo do begin
      cbSize := Sizeof(execInfo) ;
      fMask := SEE_MASK_IDLIST;
      Wnd := Handle;
      lpVerb := nil;
      lpFile := nil;
      lpParameters := nil;
      lpDirectory := nil;
      nShow := SW_SHOWNORMAL;
      hInstApp:=0;
      lpIDList := recycleBinPIDL;
    end;
    ShellExecuteEx(@execInfo) ;
end;


function GetVolumeName(DriveLetter: ANSIChar): shortstring;
//https://www.swissdelphicenter.ch/en/showcode.php?id=153
var
  dummy: DWORD;
  buffer: array[0..MAX_PATH] of Char;
  oldmode: LongInt;
begin
  oldmode := SetErrorMode(SEM_FAILCRITICALERRORS);
  try
    GetVolumeInformation(PChar(DriveLetter + ':\'),
                         buffer,
                         SizeOf(buffer),
                         nil,
                         dummy,
                         dummy,
                         nil,
                         0);
    Result := StrPas(buffer);
  finally
    SetErrorMode(oldmode);
  end;
end;


procedure UnblockFile(fName : PathStr);
begin
   {$IfDef VCL}
      if FileExists(fName) then ExecuteFile('type nul > ' + fName + ':Zone.Identifier','','');
   {$EndIf}
end;


function FileCanBeOpened(fName : PathStr) : boolean;
//  1/16/2022 this did not work in opening a database
var
   f : tHandle;
begin
      repeat
        try
            f := FileOpen(fName,fmOpenRead);
            Result := true;
        except
            on EFOpenError do begin
              if AnswerIsYes('Cannot open ' + fName + '; Close it in other program') then begin

              end
              else begin
                 Result := false;
                 exit;
              end;
            end;
        end;
      until Result;
      FileClose(f);
end;


procedure GetValidDBfieldname(var fName2 : shortstring);
begin
    repeat
       Petmar.GetString('Field name (<= 10 chars)',fName2,true,DBaseFieldNameChars);
    until length(fname2) <= 10;
end;

{$IfDef VCL}

         procedure PlaceFormInCorner(Owner,TheForm : Forms.tForm; FormPosition :  byte = lpSEMap);
         begin
            TheForm.DefaultMonitor := dmMainForm;
            if FormPosition = lpCenterMap then begin
               TheForm.Left := (Owner.ClientWidth - TheForm.Width) div 2;
               TheForm.Top := (Owner.ClientHeight - TheForm.Height) div 2;
            end
            else begin
               if (FormPosition = lpNEMap) then TheForm.Top := 0
               else TheForm.Top := Owner.ClientHeight - TheForm.Height - 25;
               TheForm.Left := Owner.ClientWidth - TheForm.Width - 10;
            end;
            if (TheForm.Top < 0) then TheForm.Top := 10;
            if (TheForm.Left < 0) then TheForm.Left := 10;
         end;

         procedure InsureFormIsOnScreen(TheForm : Forms.tForm);
         begin
            if (TheForm.Left < 50) then TheForm.Left := 50;
            if (TheForm.Left > wmdem.Width - TheForm.Width) then TheForm.Left := wmdem.Width - TheForm.Width -25;
            if (TheForm.Top < 50) then TheForm.Top := 50;
            if (TheForm.Top + TheForm.Height) > wmdem.Height then TheForm.Top := WMDEM.Height - TheForm.Height - 50;
         end;

         procedure CheckFormPlacement(TheForm : Forms.tForm);
         begin
            PlaceFormInCorner(wmdem,TheForm,lpCenterMap);
            {$IfDef TrackFormPlacement} WriteLineToDebugFile(TheForm.Caption + '  ' + FormSize(TheForm) +'  Placement: ' + IntToStr(TheForm.Left) + 'x' + IntToStr(TheForm.Top));  {$EndIf}
         end;

         procedure PlaceFormAtMousePosition(TheForm : Forms.tForm);
         begin
            TheForm.Top := Mouse.CursorPos.Y - TheForm.Height div 2;
            TheForm.Left := Mouse.CursorPos.X - TheForm.Width div 2;
            InsureFormIsOnScreen(TheForm);
         end;


         procedure ShowInNotepadPlusPlus(inName : tStringList; Capt : shortstring = '');
         var
             fName : PathStr;
         begin
            fName := MDTempDir + ExtractFileNameNoExt(Capt) + '.txt';
            inName.SaveToFile(fName);
            inName.Free;
            ShowInNotepadPlusPlus(fName);
         end;


         procedure ShowInNotepadPlusPlus(fName : PathStr; Capt : shortstring = '');
         var
            ppName,Params,DefaultDir : ANSIstring;
         begin
            ppName := 'C:\Program Files\Notepad++\notepad++.exe';
            if (not FileExists(ppName)) then ppName := 'C:\Program Files (x86)\Notepad++\notepad++.exe';
            if FileExists(ppName) then begin
               ppName := '"' + ppName + '"';
               Params := fName;
               DefaultDir := '';
               ExecuteFile(ppName, Params, DefaultDir);
            end
            else begin
               if (Capt = '') then Capt := fname;
               QuickOpenEditWindow(FName,Capt);
            end;
         end;
{$EndIf}


function TrueOrFalse(BoolVar : boolean) : shortstring;
begin
   if BoolVar then Result := 'true'
   else Result := 'false';
end;


procedure RemoveASCII0FromFile(fName : PathStr);
var
    fName2 : PathStr;
    inf,outf : file;
    i,NumRead,NumWrite,TotalSize,TotalRead : integer;
    charray,outarray : array[1..32000] of ANSIChar;
begin
    TotalSize := GetFileSize(fname);
    fName2 := MDTempDir + 'temp.txt';
    assignFile(inf,fName);
    reset(inf,1);
    assignFile(outf,fName2);
    rewrite(outf,1);

    StartProgress('Remove ASCII 0');
    TotalRead := 0;
    while not eof(inf) do begin
       BlockRead(inf,charray,32000,NumRead);
       inc(TotalRead,NumRead);
       UpdateProgressBar(TotalRead/TotalSize);
       NumWrite := 0;
       for i := 1 to Numread do begin
          if (charray[i] <> #0) then begin
             inc(NumWrite);
             OutArray[NumWrite] := charray[i];
          end;
       end;
       BlockWrite(outf,Outarray,NumWrite);
    end;
    CloseFile(inf);
    CloseFile(outf);
    CopyFile(fName2,fName);
    EndProgress;
end;



procedure CleanUpFileName(var fname : PathStr);
var
   i : integer;
begin
   for i := Length(fName) downto 1 do begin
      if Not (fName[i] in ValidDosFileNameChars) then fName[i] := '_';
   end;
end;


         function UpdateRate(NumRecs : integer) : integer;
         begin
             if NumRecs > 10000 then Result := 1000
             else if NumRecs > 1000 then Result := 100
             else if NumRecs > 100 then Result := 10
             else Result := 1;
         end;


         function CreateHTML_href(fName,Display : ANSIString) : ANSIString;
         begin
            Result := '<a href="' + fName + '">' + Display + '</a>';
         end;



         PROCEDURE WavelengthToRGB(CONST Wavelength :  float64;  VAR R,G,B:  BYTE);
         // Adapted from www.isc.tamu.edu/~astro/color.html
         // SpectraLibrary
         // Copyright (C) 1998, 2001, Earl F. Glynn, Overland Park, KS.
         // Updated June 2001 for "FrequencyToRGB" and conversion routines.
         // May be copied freely for non-commercial use.
         // E-Mail:  EarlGlynn@att.net
            CONST
              Gamma        =   0.60;
              IntensityMax = 255;
            VAR
              Blue  :  DOUBLE;
              factor:  DOUBLE;
              Green :  DOUBLE;
              Red   :  DOUBLE;

            FUNCTION Adjust(CONST Color, Factor:  DOUBLE):  INTEGER;
            BEGIN
              IF   Color = 0.0 THEN RESULT := 0     // Don't want 0^x = 1 for x <> 0
              ELSE RESULT := ROUND(IntensityMax * Math.Power(Color * Factor, Gamma))
            END {Adjust};

          BEGIN

            CASE TRUNC(Wavelength) OF
              380..439:
                BEGIN
                  Red   := -(Wavelength - 440) / (440 - 380);
                  Green := 0.0;
                  Blue  := 1.0
                END;

              440..489:
                BEGIN
                  Red   := 0.0;
                  Green := (Wavelength - 440) / (490 - 440);
                  Blue  := 1.0
                END;

              490..509:
                BEGIN
                  Red   := 0.0;
                  Green := 1.0;
                  Blue  := -(Wavelength - 510) / (510 - 490)
                END;

              510..579:
                BEGIN
                  Red   := (Wavelength - 510) / (580 - 510);
                  Green := 1.0;
                  Blue  := 0.0
                END;

              580..644:
                BEGIN
                  Red   := 1.0;
                  Green := -(Wavelength - 645) / (645 - 580);
                  Blue  := 0.0
                END;

              645..780:
                BEGIN
                  Red   := 1.0;
                  Green := 0.0;
                  Blue  := 0.0
                END;

              ELSE
                Red   := 0.0;
                Green := 0.0;
                Blue  := 0.0
            END;

            // Let intensity fall off near vision limits
            CASE TRUNC(Wavelength) OF
              380..419:  factor := 0.3 + 0.7*(Wavelength - 380) / (420 - 380);
              420..700:  factor := 1.0;
              701..780:  factor := 0.3 + 0.7*(780 - Wavelength) / (780 - 700)
              ELSE       factor := 0.0
            END;

            R := Adjust(Red,   Factor);
            G := Adjust(Green, Factor);
            B := Adjust(Blue,  Factor)
          END {WavelengthToRGB};



{$IfDef VCL}

function File2Trash(const FileName: string): boolean;
var
    fos: TSHFileOpStruct;
begin
   FillChar(fos, SizeOf(fos), 0);
   with fos do begin
     wFunc  := FO_DELETE;
     pFrom  := PChar(ExpandFileName(FileName)+#0#0);
     fFlags := FOF_ALLOWUNDO or FOF_NOCONFIRMATION or FOF_SILENT or FOF_NOERRORUI;
   end;
   Result := (0 = ShFileOperation(fos));
end;



        function DoLegendOnBitmap(Horizontal : boolean; Colors : tColors256; Values : array of float64; Units : shortstring; LegendTitle : shortstring = ''; LegendSize : integer = 1) : tMyBitmap;
         var
           Width,Height,Size,i,j,LastLabel,Dec,xloc,yloc,TopText : integer;
           Min,Max,Range,Tick,FirstTick  : float64;
           First          : boolean;
           TStr           : ShortString;
         begin
            {$IfDef RecordLegends}
               if Horizontal then TStr := 'horizontal'
               else TStr := 'vertical';
               WriteLineToDebugFile('DoLegendOnBitmap ' + TStr);
            {$EndIf}

            {$IfDef RecordDetailedLegends}
               WriteLineToDebugFile('title=' + LegendTitle + '  units=' + Units);
               WriteLineToDebugFile('MDDef.LegendBarWidth=' + IntToStr(MDDef.LegendBarWidth) + '  MDDef.LegendTickSize=' + IntToStr(MDDef.LegendTickSize));
            {$EndIf}

            Size := 1;
            PetImage.CreateBitmap(Result,10,10);
            LoadMyFontIntoWindowsFont(MDDef.LegendFont,Result.Canvas.Font);

            if Horizontal then begin
               if (LegendTitle <> '') then i := 2 else i := 1;
               Height := MDDef.LegendBarWidth + MDDef.LegendTickSize + 5 + i * Result.Canvas.TextHeight('Mj');
               Width := LegendSize * (256) + 20 + Result.Canvas.TextWidth(Units) + 1;
               {$IfDef RecordLegends} WriteLineToDebugFile('Legend: ' + IntToStr(Width) + 'x' + IntToStr(Height)); {$EndIf}
            end
            else begin
               if (LegendTitle <> '') then i := 45 else i := 20;
               Height := LegendSize * (256) + 20 +i;
               Width := 500;
            end;
            Result.Width := Width;
            Result.Height := Height;
            ClearBitmap(Result,clNearWhite);

            if (LegendTitle <> '') then  begin
               i := (Result.Width - Result.Canvas.TextWidth(LegendTitle)) div 2;
               j := Result.Height - Result.Canvas.TextHeight(LegendTitle) - 3;
               Result.Canvas.TextOut(i,j,RemoveUnderscores(LegendTitle));
               {$IfDef RecordLegends} WriteLineToDebugFile('Legend Title at: ' + IntToStr(i) + '--' + IntToStr(j)); {$EndIf}
            end;

            with Result.Canvas do begin
               Brush.Style := bsSolid;
               Brush.Color := clWhite;
               Brush.Style := bsClear;
               Min := 99e45;
               Max := -99e45;
               for i := 0 to 255 do begin
                  if abs(Values[i]) < pred(MaxInt) then begin
                     Pen.Color := Colors[i];
                     Pen.Width := LegendSize;
                     if Horizontal then begin
                        MoveTo(5 + Size * LegendSize * (256-i),1);
                        LineTo(5 + Size * LegendSize * (256-i),MDDef.LegendBarWidth);
                     end
                     else begin
                        MoveTo(1,5 + Size * LegendSize *(256-i));
                        LineTo(MDDef.LegendBarWidth,5+Size * LegendSize *(256-i));
                     end;
                     if (Min > Values[i]) then Min := Values[i];
                     if (Max < Values[i]) then Max := Values[i];
                  end;
               end;

               Range := Max - Min;
               Tick := PetMath.GetTickInt(256,25,Range);
               if (Range < 0.001) then Dec := 4
               else if (Range < 0.01) then Dec := 3
               else Dec := 2;

               if Min <= 0 then FirstTick := (trunc(Min / Tick)) * Tick
               else FirstTick := succ(trunc((Min-0.0001) / Tick)) * Tick;

               First := true;
               LastLabel := MaxInt;
               repeat
                  i := 0;
                  while (abs(Values[i] - FirstTick) > abs(Values[succ(i)] - FirstTick)) and (i < 254) do inc(i);
                  if (i < 256) then begin
                     TStr := RealToString(FirstTick,-12,-Dec);
                     Pen.Color := clBlack;
                     if First then begin
                        First := false;
                        if Horizontal then begin
                           TopText := MDDef.LegendBarWidth + MDDef.LegendTickSize + 3;
                           LastLabel := LegendSize * (256) + 5;
                           TextOut(LastLabel,TopText,Units);
                        end
                        else TStr := TStr + Units;
                     end;
                     if Horizontal then begin
                        xloc := 5 + LegendSize * (256-i)*Size;
                        if LastLabel >  (xloc -5 + TextWidth(TStr)) then begin
                           LastLabel := xloc - TextWidth(TStr) div 2;
                           if (LastLabel < 0) then LastLabel := 0;
                           TextOut(LastLabel,TopText,TStr);
                           {$IfDef RecordLegends} WriteLineToDebugFile('Tick label at: ' + IntToStr(LastLabel) + '--' + IntToStr(j)); {$EndIf}
                           Pen.Width := 3;
                        end
                        else Pen.Width := 1;
                        MoveTo(xloc,MDDef.LegendBarWidth);
                        LineTo(xloc,MDDef.LegendBarWidth + MDDef.LegendTickSize);
                     end
                     else begin
                        yloc := 5+LegendSize *256*Size-Size*i;
                        MoveTo(15,yloc);
                        LineTo(25,yloc);
                        yloc := yloc - TextHeight(TStr) div 2;
                        if yloc < 1 then yloc := 1;

                        if yloc < LastLabel - TextHeight(TStr) then begin
                           TextOut(28,yloc,TStr);
                           LastLabel := yloc;
                        end;
                     end;
                     FirstTick := FirstTick + Tick;
                  end;
               until (FirstTick > Max) or (i = 255);
            end;
            PutBitmapInBox(Result);
         end;


         function HorizontalLegendOnBitmap(Colors : tColors256; Values : array of float64; Units,LegendTitle : shortstring; LegendSize : integer = 1): tMyBitmap;
         begin
            Result := DoLegendOnBitmap(true,Colors,Values,' ' + Units,LegendTitle,LegendSize);
         end;


         function VerticalLegendOnBitmap(Colors : tColors256; Values : array of float64; Units,LegendTitle : shortstring): tMyBitmap;
         begin
            Result := DoLegendOnBitmap(false,Colors,Values,Units,LegendTitle);
         end;


         procedure MakeValuesForLegend(var Colors : tColors256; var Vals : array of float64; Min,Max : float64; Legend : tLegendColors; ChloroplethScheme : shortstring = ''; Reverse : boolean = false);
         var
            ZColorTable : tColorTableDefinitions;
            i : integer;
            x : float64;
         begin
            {$IfDef RecordLegends} WriteLineToDebugFile('MakeValuesForLegend in'); {$EndIf}
            if (Legend = LegChloropleth) then begin
               {$IfDef RecordLegends} WriteLineToDebugFile('Chloropheth ' + ChloroplethScheme); {$EndIf}
              {$IfDef ExGIS}
              {$Else}
              if DefineColorTableValues(ChloroplethScheme,Min,Max,ZColorTable,Reverse) then begin
                 for i := 0 to 255 do begin
                    Colors[i] := ConvertPlatformColorToTColor(ZColorTable.ZTableColors[1 + round(i/255 * pred(ZColorTable.ZTableEntries))]);
                 end;
              end;
              {$EndIf}
            end
            else begin
               if (Min > Max) then begin
                  x := Min;
                  Min := Max;
                  Max := x;
                  case legend of
                     LegSpectrum : for i := 0 to 255 do Colors[255-i] := ConvertPlatformColorToTColor(SpectrumRGBFunct(i,0,255));
                     LegRainbows : for i := 0 to 255 do Colors[255-i] := RainbowColorFunct(i,0,255);
                     LegGrays : for i := 0 to 255 do Colors[255-i] := RGB(i,i,i);
                     LegTerrain : for i := 0 to 255 do Colors[255-i] := TerrainTColor(i,0,255);
                  end;
               end
               else begin
                  case legend of
                     LegSpectrum : for i := 0 to 255 do Colors[i] := ConvertPlatformColorToTColor(SpectrumRGBFunct(i,0,255));
                     LegRainbows : for i := 0 to 255 do Colors[i] := RainbowColorFunct(i,0,255);
                     LegGrays : for i := 0 to 255 do Colors[i] := RGB(i,i,i);
                     LegTerrain :  for i := 0 to 255 do Colors[i] := TerrainTColor(i,0,255);
                  end;
               end;
            end;
            for i := 0 to 255 do Vals[i] := Min + (i / 255) * (Max - Min);
         end;



         function DefaultHorizontalLegendOnBitmap(Min,Max : float64; Units,LegendTitle : shortstring; Legend : tLegendColors; ChloroplethScheme : shortstring = ''; Reverse : boolean = false): tMyBitmap;
         var
            Colors : tColors256;
            Vals : array[0..255] of float64;
         begin
            MakeValuesForLegend(Colors,Vals,Min,Max,Legend,ChloroplethScheme,Reverse);
            Result := DoLegendOnBitmap(true,Colors,vals,Units,LegendTitle);
         end;


         function DefaultVerticalLegendOnBitmap(Min,Max : float64; Units,LegendTitle : shortstring; Legend : tLegendColors; ChloroplethScheme : shortstring = ''; Reverse : boolean = false): tMyBitmap;
         var
            Colors : tColors256;
            Vals : array[0..255] of float64;
         begin
            MakeValuesForLegend(Colors,Vals,Min,Max,Legend,ChloroplethScheme,Reverse);
            Result := DoLegendOnBitmap(false,Colors,vals,Units,LegendTitle);
         end;



         procedure ColorLineWidthBitmap(var Bitmap : tMyBitmap; Color : TPlatformColor; Size : integer; Minimize : boolean = true);
         begin
            Bitmap.Canvas.Pen.Color := ConvertPlatformColorToTColor(Color);
            Bitmap.Canvas.Pen.Width := Size;
            DrawLine(Bitmap,1,Bitmap.height div 2, Bitmap.Width- 1,Bitmap.height div 2);
            if Minimize then PetImage.GetImagePartOfBitmap(Bitmap);
         end;


         procedure ColorLineWidthBitBtn(var BitBtn : tBitBtn; Color : tPlatformColor; Size : integer);
         var
            Bitmap : tMyBitmap;
         begin
            CreateBitmap(Bitmap,20,BitBtn.Height - 4);
            ColorLineWidthBitmap(Bitmap,Color,Size);
            BitBtn.Glyph := Bitmap;
            Bitmap.Free;
         end;


         procedure ColorBitBtn(var BitBtn : tBitBtn; Color : tPlatformColor);
         begin
            ColorBitBtn(BitBtn,ConvertPlatformColorToTColor(Color));
         end;

         procedure ColorBitBtn(var BitBtn : tBitBtn; Color : tColor);
         var
            Bitmap : tMyBitmap;
         begin
            CreateBitmap(Bitmap,20,20);
            Bitmap.Canvas.Pen.Color := Color;
            Bitmap.Canvas.Brush.Color := Color;
            Bitmap.Canvas.Rectangle(1,1,18,18);
            BitBtn.Glyph := Bitmap;
            Bitmap.Free;
         end;


         procedure SymbolOnButton(var BitBtn : tBitBtn; Symbol : tDrawingSymbol; SymbolSize : integer; SymbolColor : tPlatformColor);
         var
            aBitmap : tMyBitmap;
         begin
            CreateBitmap(aBitmap,20,20);
            ScreenSymbol(aBitmap.Canvas,10,10,Symbol,SymbolSize,SymbolColor);
            PetImage.GetImagePartOfBitmap(aBitmap);
            BitBtn.Glyph := aBitmap;
            FreeAndNil(aBitmap);
         end;

         procedure SymbolOnButton(var BitBtn : tBitBtn; Symbol : tFullSymbolDeclaration);
         begin
            SymbolOnButton(BitBtn,Symbol.DrawingSymbol,Symbol.Size,Symbol.Color);
         end;


         function FontInstalled(Const FontName:String) : Boolean;
         Begin
            Result := Screen.Fonts.IndexOf(FontName)>=0
         End;


         function GetFromList(InMessage : ANSIstring; var PickedNum : integer; InList : TStringList; CanCancel : boolean = false) : boolean;
         begin
            PickedNum := pred(PickedNum);
            Result := GetFromListZeroBased(InMessage,PickedNum,InList,CanCancel);
         end;

         function GetFromList(InMessage : ANSIstring; InList : TStringList; CanCancel : boolean = false) : ShortString;
         var
           PickedNum : integer;
         begin
            PickedNum := 1;
            if GetFromListZeroBased(InMessage,PickedNum,InList,CanCancel) then Result := InList.Strings[PickedNum]
            else Result := '';
         end;


         function GetMultipleFromList(InMessage : ANSIstring; var PickedNum : integer; var InList : TStringList; CanCancel : boolean = false) : boolean;
         begin
            {$IfDef RecordListPick} WriteLineToDebugFile('GetMultipleFromList in, choices=' + IntToStr(InList.Count)); {$EndIf}
            PickedNum := pred(PickedNum);
            Result := GetFromListZeroBased(InMessage,PickedNum,InList,CanCancel,true);
            {$IfDef RecordListPick} WriteLineToDebugFile('GetMultipleFromList out, choices=' + IntToStr(InList.Count)); {$EndIf}
         end;


         function GetFromListZeroBased(InMessage : ANSIstring; var PickedNum : integer; var InList : TStringList; CanCancel : boolean = false; MultiPick : boolean = false) : boolean;
         {$IfDef FMX}
         begin
            Result := false;
         {$EndIf}
         {$IfDef VCL}
         var
           PetList : TPetList;
           i : integer;
         begin
            if (InList.Count = 0) then Result := false
            else begin
                PickedNum := 0;
                PetList := TPetList.Create(Application);
                PetList.Caption := InMessage + '  (' + IntToStr(InList.Count) + ' choices)';
                PetList.ListBox1.ItemIndex := PickedNum;
                for i := 0 to pred(Inlist.Count) do PetList.ListBox1.Items.Add(InList.Strings[i]);
                PetList.CancelBtn.Enabled := CanCancel;
                PetList.Panel3.Caption := PetList.ListBox1.Items[PickedNum];
                PetList.Width:= 400;
                PetList.ListBox1.MultiSelect := MultiPick;

                PetList.ShowModal;
                Result := not PetList.Cancel;
                if Result then begin
                   if (PetList.ListBox1.ItemIndex <> -1) then PickedNum := PetList.ListBox1.ItemIndex;
                   if MultiPick then begin
                      for I := pred(InList.Count) downto 0 do begin
                         if PetList.ListBox1.Selected[i] then begin
                           {$IfDef RecordListPick} WriteLineToDebugFile('Picked ' + InList.Strings[i]); {$EndIf}
                         end
                         else begin
                           {$IfDef RecordListPick} WriteLineToDebugFile('Rejected ' + InList.Strings[i]); {$EndIf}
                            Inlist.Delete(i);
                         end;
                      end;
                   end;
               end;
               PetList.Destroy;
            end;
         {$EndIf}
         end;


         procedure GetString(Prompt : Shortstring; var Input : ShortString;  ForceCaps : boolean; ValidChars : characters);
         var
            i : integer;
            OK : boolean;
         begin
            repeat
               PETMARCommonForm.Caption := 'Input String';
               PETMARCommonForm.Panel1.Caption := Prompt;
               PETMARCommonForm.Edit1.Text := Input;
               if ForceCaps then PETMARCommonForm.Edit1.CharCase := ecUpperCase
               else PETMARCommonForm.Edit1.CharCase := ecNormal;
               PETMARCommonForm.FormActivate(Nil);
               PETMARCommonForm.ShowModal;
               Input := PETMARCommonForm.Edit1.Text;
               OK := true;
               for I := Length(Input) downto 1 do if not (Input[i] in ValidChars) then begin
                  Delete(Input,i,1);
                  Ok := false;
               end;
            until OK and (Error = 0);
            if ForceCaps then Input := UpperCase(Input);
         end;

         procedure FillComboBoxWithColorPalettes(fName : PathStr; ComboBox1 : tComboBox);
         var
            I : integer;
            ColorTable : tMyData;
            DataThere : tStringList;
         begin
            if FileExists(fName) then begin
               ColorTable := tMyData.Create(fName);
               ColorTable.ApplyFilter('USE=' + QuotedStr('Y'));
               DataThere := ColorTable.UniqueEntriesInDB('PALETTE');
               for i := 0 to pred(DataThere.Count) do ComboBox1.Items.Add(DataThere.Strings[i]);
               ColorTable.Destroy;
               DataThere.Free;
            end;
         end;


         Procedure GetAngle;
         var
            get_angle_form : Tget_angle_form;
         begin
            get_angle_form := Tget_angle_form.Create(Application);
            with get_angle_form do begin
               Angle := TheAngle;
               Edit5.Text := RealToString(Angle,-18,-8);
               RadioGroup4.ItemIndex := Ord(AngleMeasure);
               Caption := Mess;
               ShowModal;
               TheAngle := Angle;
               AngleMeasure := tAngleMeasure(RadioGroup4.ItemIndex);
               Close;
            end;
         end;


         function GetFileNameDefaultExt(Mess,Filters : ANSIstring; var FName : PathStr; AppendOption : boolean = false) : boolean;   overload;
         var
            DefExt : integer;
         begin
            DefExt := 1;
            Result := GetFileNameDefaultExtSaveExt(Mess,Filters,FName,DefExt,AppendOption);
         end;


         function GetExistingFileName(Mess,Filters : ANSIstring; var FName : PathStr) : boolean;
         begin
            PETMARCommonForm.SaveDialog1.Title := Mess;
            PETMARCommonForm.SaveDialog1.Filter := Filters;
            if FileExists(fName) or PathIsValid(ExtractFilePath(fName)) then begin
               PETMARCommonForm.SaveDialog1.InitialDir := ExtractFilePath(FName);
               PETMARCommonForm.SaveDialog1.FileName := ExtractFileName(fName);
            end
            else begin
               PETMARCommonForm.SaveDialog1.InitialDir := Copy(fName,1,pred(Length(fName)));
               PETMARCommonForm.SaveDialog1.FileName := '';
            end;
            PETMARCommonForm.SaveDialog1.FilterIndex := 1;

            if PETMARCommonForm.SaveDialog1.Execute then begin
               if ExtractFileName(PETMARCommonForm.SaveDialog1.FileName) = '' then Result := false
               else begin
                  FName := PETMARCommonForm.SaveDialog1.FileName;
                  Result := true;
               end;
            end;
         end;


         function GetFileNameDefaultExtSaveExt(Mess,Filters : ANSIstring; var FName : PathStr; var df : integer; AppendOption : boolean = false) : boolean;
         var
            OK : boolean;
            tempString : ShortString;
            Ext,Extra : ExtStr;
            i : integer;
         begin
            PETMARCommonForm.SaveDialog1.Title := Mess;
            PETMARCommonForm.SaveDialog1.Filter := Filters;
            if FileExists(fName) or PathIsValid(ExtractFilePath(fName)) then begin
               PETMARCommonForm.SaveDialog1.InitialDir := ExtractFilePath(FName);
               PETMARCommonForm.SaveDialog1.FileName := ExtractFileName(fName);
            end
            else begin
               PETMARCommonForm.SaveDialog1.InitialDir := Copy(fName,1,pred(Length(fName)));
               PETMARCommonForm.SaveDialog1.FileName := '';
            end;
            PETMARCommonForm.SaveDialog1.FilterIndex := df;

            repeat
               if PETMARCommonForm.SaveDialog1.Execute then begin
                  {$IfDef RecordFileFilter} WriteLineToDebugFile('Filter index: ' + IntToStr(SaveDialog1.FilterIndex) + '  Filter: ' + Filters); {$EndIf}

                  OK := true;
                  if ExtractFileName(PETMARCommonForm.SaveDialog1.FileName) = '' then Result := false
                  else begin
                     FName := PETMARCommonForm.SaveDialog1.FileName;
                     CleanUpFileName(fName);
                     Result := true;
                     if (PETMARCommonForm.SaveDialog1.FileName = '') then OK := false
                     else begin
                        if FileExists(fName) then begin
                           tempString := '  file ' + fName;
                           if AppendOption then begin
                              OK := AnswerIsYes('append' + TempString);
                              if not OK then begin
                                 OK := AnswerIsYes('Overwrite' + TempString);
                                 if OK then DeleteFileIfExists(FName);
                              end;
                           end
                           else begin
                              OK := AnswerIsYes('Overwrite' + TempString);
                              if OK then DeleteFileIfExists(FName);
                           end;
                        end
                        else begin
                           Ext := ExtractFileExt(FName);
                           if Length(Ext) < 2 then begin
                              for I := 0 to pred(PETMARCommonForm.SaveDialog1.FilterIndex) do begin
                                 Extra := BeforeSpecifiedCharacterANSI(Filters,'*',true,true);
                                 Ext := BeforeSpecifiedCharacterANSI(Filters,'|',true,true);
                                 {$IfDef RecordFileFilter} WriteLineToDebugFile(' Ext ' + IntToStr(i) + ': ' + Ext); {$EndIf}
                              end;
                              FName := FName + Ext;
                           end;
                           df := PETMARCommonForm.SaveDialog1.FilterIndex;
                        end;
                     end;
                  end;
               end
               else begin
                  Result := false;
                  OK := true;
               end;
            until OK;
         end;

         procedure GetAngleInRadians(Message : shortstring; var Angle : float64);
         begin
            Angle := Angle / DegToRad;
            ReadDefault(Message,Angle);
            Angle := Angle * DegToRad;
         end;


         function GetMultipleFiles(InMessage,Masks : ANSIstring; var FilesWanted : TStringList; var DefaultFilter : byte) : boolean;
         var
            i : integer;
         begin
            {$If Defined(RecordDialogs)} WriteLineToDebugFile('GetMultipleFiles in, Masks=' + Masks); {$EndIf}
            PETMARCommonForm.OpenDialog1.Options := [ofAllowMultiSelect,ofEnableSizing];
            PETMARCommonForm.OpenDialog1.Title := 'Open files ' + InMessage;
            if (FilesWanted.Count = 0) then FilesWanted.Add(ProgramRootDir);
            PETMARCommonForm.OpenDialog1.FileName := ExtractFileName(FilesWanted.Strings[0]);
            PETMARCommonForm.OpenDialog1.InitialDir := ExtractFilePath(FilesWanted.Strings[0]);
            PETMARCommonForm.OpenDialog1.Filter := Masks;
            PETMARCommonForm.OpenDialog1.FilterIndex := DefaultFilter;
            {$If Defined(RecordDialogs)} WriteLineToDebugFile('GetMultipleFiles to user'); {$EndIf}

            if PETMARCommonForm.OpenDialog1.Execute then begin
               {$If Defined(RecordDialogs)} WriteLineToDebugFile('User done'); {$EndIf}
               FilesWanted.Clear;
               FilesWanted.Sorted := true;
               for i := 0 to pred(PETMARCommonForm.OpenDialog1.Files.Count) do begin
                  FilesWanted.Add(PETMARCommonForm.OpenDialog1.Files[i]);
               end;
               Result := (PETMARCommonForm.OpenDialog1.Files.Count > 0);
               DefaultFilter := PETMARCommonForm.OpenDialog1.FilterIndex;
            end
            else Result := false;
            {$If Defined(RecordDialogs)} WriteLineToDebugFile('GetMultipleFiles out'); {$EndIf}
         end;


         function GetFileMultipleMask(InMessage,Masks : ANSIstring; var FileWanted : PathStr; var DefaultFilter : byte) : boolean;
         begin
            {$If Defined(RecordDialogs)} WriteLineToDebugFile('GetFileMultipleMask in, Masks=' + Masks); {$EndIf}
            PETMARCommonForm.OpenDialog1.Options := [ofEnableSizing];
            PETMARCommonForm.OpenDialog1.Title := 'Open files ' + InMessage;
            PETMARCommonForm.OpenDialog1.FileName := ExtractFileName(FileWanted);
            PETMARCommonForm.OpenDialog1.InitialDir := ExtractFileDir(FileWanted);
            PETMARCommonForm.OpenDialog1.Filter := Masks;
            PETMARCommonForm.OpenDialog1.FilterIndex := DefaultFilter;
            if PETMARCommonForm.OpenDialog1.Execute then begin
               if (PETMARCommonForm.OpenDialog1.FileName = '') then FileWanted := ''
               else begin
                  FileWanted := ExpandFileName(PETMARCommonForm.OpenDialog1.FileName);
                  DefaultFilter := PETMARCommonForm.OpenDialog1.FilterIndex;
               end;
               Result := (FileWanted <> '');
            end
            else begin
               Result := false;
               FileWanted := '';
            end;
         end;


         function DefineColorArray(Palette : shortstring; var NumColors : integer; var ColorsTable : tZTableColors255; Reverse : boolean = false) : boolean;
         var
            ColorTable : tMyData;
            i : integer;
         begin
            NumColors := 0;
            if FileExists(ColorBrewerName) then begin
               ColorTable := TMyData.Create(ColorBrewerName);
               ColorTable.ApplyFilter('PALETTE=' + QuotedStr(Palette));
               if (ColorTable.RecordCount = 0) then begin
                  Palette := 'ColorBrewer Spectral-9';
                  ColorTable.ApplyFilter('PALETTE=' + QuotedStr(Palette));
               end;
               NumColors := ColorTable.RecordCount;
               if NumColors > 255 then NumColors := 255;

               for i := 1 to NumColors do begin
                  if (i in [1..255]) then begin
                     if Reverse then ColorsTable[NumColors - pred(i)] := ColorTable.PlatformColorFromTable
                     else ColorsTable[i] := ColorTable.PlatformColorFromTable;
                  end;
                  ColorTable.Next;
               end;
               ColorTable.Destroy;
            end;
            Result := (NumColors > 0);
         end;


         procedure EditTFont(Font : tFont);
         var
            MyFont : tMyFont;
         begin
            PETMARCommonForm.FontDialog1.Font := Font;
            if PETMARCommonForm.FontDialog1.Execute then begin
               GetMyFontFromWindowsFont(MyFont,PETMARCommonForm.FontDialog1.Font);
               LoadMyFontIntoWindowsFont(MyFont,Font);
            end;
         end;

         procedure EditMyFont(var MyFont : tMyFont);
         begin
            LoadMyFontIntoWindowsFont(MyFont,PETMARCommonForm.FontDialog1.Font);
            if PETMARCommonForm.FontDialog1.Execute then GetMyFontFromWindowsFont(MyFont,PETMARCommonForm.FontDialog1.Font);
         end;

         function MyFontToString(MyFont : tMyFont) : ShortString;
         begin
            if (MyFont.Name = '') then Result := ''
            else Result := MyFont.Name + '  pts=' + IntToStr(MyFont.Size) + '  color=' + IntToStr(ConvertPlatformColorToTColor(MyFont.Color));
         end;

         procedure LoadMyFontIntoWindowsFont(MyFont : tMyFont; WindFont : tFont);
         begin
            WindFont.Name := MyFont.Name;
            WindFont.Size := MyFont.Size;
            WindFont.Color := ConvertPlatformColorToTColor(MyFont.Color);
            WindFont.Style := [];
            if MyFont.Bold then WindFont.Style := WindFont.Style + [fsBold];
            if MyFont.Italics then WindFont.Style := WindFont.Style + [fsItalic];
            if MyFont.Underline then WindFont.Style := WindFont.Style + [fsUnderline];
         end;

         procedure GetMyFontFromWindowsFont(var MyFont : tMyFont; WindFont : tFont);
         begin
            MyFont.Name := WindFont.Name;
            MyFont.Size := WindFont.Size;
            MyFont.Color := ConvertTColorToPlatformColor(WindFont.Color);
            MyFont.Bold := fsBold in WindFont.Style;
            MyFont.Italics := fsItalic in WindFont.Style;
            MyFont.Underline := fsUnderline in WindFont.Style;
         end;


         procedure InitializeMyFont(var MyFont : tMyFont; Name : shortstring; Size : SmallInt; Color : tPlatformColor);
         begin
            MyFont.Name := Name;
            MyFont.Size := Size;
            MyFont.Color := Color;
            MyFont.Italics := false;
            MyFont.Bold := false;
            MyFont.Underline := false;
         end;

         function ColorString(Color : tRGBTriple) : shortString;
         begin
            Result := RGBString(Color.rgbtRed,Color.rgbtGreen,Color.rgbtBlue) + ' gray=' + IntToStr(round(0.3 * Color.rgbtRed  + 0.59 * Color.rgbtGreen  + 0.11 * Color.rgbtBlue ));
         end;

         function GrayColorFunct(i : integer) : TColor;
         begin
            Result := RGB(i,i,i);
         end;

         function GrayColorFunct(z,Min,Max : float64) : TColor;
         var
            zi : integer;
         begin
            zi := ValidByteRange(round(255.0 * (z - Min) / (Max - Min)));
            Result := RGB(zi,zi,zi);
         end;

         function PlatformRainbowColorFunct(z,Min,Max : float64) : TPlatformColor;
         begin
            Result := ConvertTColorToPlatformColor(RainbowColorFunct(z,Min,Max));
         end;


         function RainbowColorFunct(z,Min,Max : float64) : TColor;
         const
            FirstColor = 136;
            ColorStep = 2;
         var
            zi : integer;
         begin
            if abs(Max-Min) < 0.0001 then Result := clRed
            else begin
               zi := ValidByteRange(round(255.0 * (z - Min) / (Max - Min)));
               case zi of
                  0..42 : RainbowColorFunct := RGB(0,0,FirstColor + ColorStep * zi); {blues}
                  43..85 : RainbowColorFunct := RGB(0,FirstColor + ColorStep * (zi-43),FirstColor + ColorStep * (zi-43)); {Cyans}
                  86..128 : RainbowColorFunct := RGB(0,FirstColor + ColorStep * (zi-86),0); {greens}
                  129..171: RainbowColorFunct := RGB(FirstColor + ColorStep * (zi-129),FirstColor + ColorStep * (zi-129),0);  {yellows}
                  172..214 : RainbowColorFunct := RGB(FirstColor + ColorStep * (zi-172),0,0);  {reds}
                  215..255 : RainbowColorFunct := RGB(FirstColor + ColorStep * (zi-215),0,FirstColor + ColorStep * (zi-215));  {magentas}
                  else RainBowColorFunct := clBlack;
               end;
            end;
         end {function};



         function SameColor(c1,c2 : tPlatformColor) : boolean;
         begin
            {$IfDef VCL}
            Result := (c1.rgbtRed = c2.rgbtRed) and (c1.rgbtBlue = c2.rgbtBlue) and (c1.rgbtGreen = c2.rgbtGreen);
            {$EndIf}
            {$IfDef FMX}
            Result := (c1.r = c2.r) and (c1.g = c2.g) and (c1.b = c2.b);
            {$EndIf}
         end;

         procedure CrossWithHole(Canvas : tCanvas; x,y : integer);
         begin
            Canvas.MoveTo(x+2,y);  Canvas.LineTo(x+8,y);
            Canvas.MoveTo(x-2,y);  Canvas.LineTo(x-8,y);
            Canvas.MoveTo(x,y+2);  Canvas.LineTo(x,y+8);
            Canvas.MoveTo(x,y-2);  Canvas.LineTo(x,y-8);
         end;


         procedure TextOutVertical(Canvas : TCanvas; x,y : integer; Words : ShortString; ClearBack : boolean = false);
         begin
            CanvasTextOutAngle(Canvas,x,y,900,Words);
         end;


         procedure CanvasTextOutAngle(c : TCanvas; x,y : Integer; d : Word; s : ShortString);
         {------------------------------------------------------------------------------
           PURPOSE
             To output rotated text in the same font as the font on the supplied canvas.
             The font must be a scaleable font.

           INPUT PARAMETERS
             c - The canvas on which to output the text.
             x,y - The x,y screen coordinates you would normally supply TextOut.
             d - The angle in tenths of degrees.
             s - The text to be output to the canvas.
           HISTORY
             6/18/1995 First version written by Curtis Keisler.
         -------------------------------------------------------------------------------}
         var
           LogRec         : TLOGFONT;     {* Storage area for font information *}
           OldFontHandle,                 {* The old font handle *}
           NewFontHandle  : HFONT;        {* Temporary font handle *}
         begin
           {* Get the current font information. We only want to modify the angle *}
           GetObject(c.Font.Handle, SizeOf(LogRec), Addr(LogRec));
           try
              {* Modify angle (0.1°) between the base line of a character and the x-axis}
              LogRec.lfEscapement := d;

              {* Create a new font handle using the modified old font handle *}
              NewFontHandle := CreateFontIndirect(LogRec);

              {* Save the old font handle! We have to put it back when we are done! *}
              OldFontHandle := SelectObject(c.Handle,NewFontHandle);

              {* Output the text *}
              c.TextOut(x,y,s);
           finally
              {* Put the font back the way we found it! *}
              NewFontHandle := SelectObject(c.Handle,OldFontHandle);

              {* Delete the temporary (NewFontHandle) that we created *}
              DeleteObject(NewFontHandle);
           end;
         end; {* CanvasTextOutAngle *}


         procedure CopyPartOfScreenToBitmap(var Bitmap2 : tMyBitmap; Left,Top,Right,Bottom : integer);
         var
            Bitmap : tMyBitmap;
         begin
            Delay(100);
            CaptureActiveWindowToClipboard;
            Delay(100);
            Bitmap := tMyBitmap.Create;
            Bitmap.Assign(Clipboard);
            CreateBitmap(Bitmap2,Right - Left,Bottom - Top);
            Bitmap2.Canvas.CopyRect(Rect(0,0,Bitmap2.Width,Bitmap2.Height),Bitmap.Canvas,Rect(Left,Top,Right,Bottom));
         end;


         procedure QueryTColor(var Color : tColor);
         begin
            PETMARCommonForm.ColorDialog1.Color := Color;
            if PETMARCommonForm.ColorDialog1.Execute then Color := PETMARCommonForm.ColorDialog1.Color;
         end;

         procedure QueryTColor(BitBtn : tBitBtn; var Color : tColor);
         begin
            QueryTColor(Color);
            ColorBitBtn(BitBtn,ConvertTColorToPlatformColor(Color));
         end;


         procedure QueryColor(var Color : tPlatformColor);
         var
            acolor : tcolor;
         begin
            aColor := ConvertPlatformColorToTColor(Color);
            QueryTColor(acolor);
            Color := ConvertTColorToPlatformColor(acolor);
         end;


         procedure QueryColor(BitBtn : tBitBtn; var Color : tPlatformColor); overload;
         var
            Color1 : tColor;
         begin
            Color1 := ConvertPlatformColorToTColor(Color);
            QueryTColor(BitBtn,Color1);
            Color := ConvertTColorToPlatformColor(Color1);
         end;


         procedure HardwareOnLine;
         begin
            MessageToContinue(HardwareString,true);
         end;


         procedure DisplayHTMLTopic(Name : Ansistring);
         var
            fName : Ansistring;
         begin
            fName := ProgramRootDir + Application.HelpFile;
            if FileExists(fname) then begin
               ReplaceCharacter(Name,'\','/');
               ExecuteFile(ProgramRootDir + 'keyhh.exe','-MDhelp microdem.chm::/' + Name,ProgramRootDir);
            end
            else begin
               {$IfDef AllowUSNAhelp}
                  ReplaceCharacter(Name,'\','/');
                  fname := 'https://www.usna.edu/Users/oceano/pguth/md_help/' + Name;
                  ExecuteFile(fName,'','');
               {$EndIf}
            end;
            StopSplashing;
            {$IfDef RecordHelp} WriteLineToDebugFile('DisplayHTMLTopic ' + Name + ' from ' + fName); {$EndIf}
         end;


         procedure PickLineSizeAndColor(WhatFor : shortString; BitButton : tBitBtn; var LineColor : tPlatformColor; var LineSize :  byte);
         var
            i : integer;
         begin
            i := LineSize;
            PickLineSizeAndColor(WhatFor,BitButton,LineColor,i);
            LineSize := i;
         end;


         procedure PickLineSizeAndColor(WhatFor : shortString; BitButton : tBitBtn; var LineColor : tPlatformColor; var LineSize :  float32);
         var
            i : integer;
         begin
            i := round(LineSize);
            PickLineSizeAndColor(WhatFor,BitButton,LineColor,i);
            LineSize := i;
         end;


         procedure PickLineSizeAndColor(WhatFor : shortString; BitButton : tBitBtn; var LineColor : tPlatformColor; var LineSize :  integer);
         var
            GraphColorsForm : Tlineparamsform;
         begin
            GraphColorsForm := Tlineparamsform.Create(Application);
            with GraphColorsForm do begin
               if (WhatFor <> '') then Caption := WhatFor;
               TheColor := LineColor;
               TheSize := LineSize;
               graphcolorsform.ShowModal;
               LineColor := graphcolorsform.theColor;
               LineSize := graphcolorsform.theSize;
               if (BitBUtton <> Nil) then ColorLineWidthBitBtn(BitButton,TheColor,TheSize);
               Free;
            end;
         end;


         procedure PickSymbol(BitButton : tBitBtn; var DrSymbol : tDrawingSymbol; var SymSize : byte; var WantColor : tPlatformColor; WhatFor : shortstring = '');
         begin
            GetSymbol(DrSymbol,SymSize,WantColor,WhatFor);
            if (BitButton <> Nil) then SymbolOnButton(BitButton,DrSymbol,SymSize,WantColor);
         end;


         procedure PickSymbol(BitButton : tBitBtn; var Symbol : tFullSymbolDeclaration; WhatFor : shortstring = '');
         begin
            PickSymbol(BitButton,Symbol.DrawingSymbol,Symbol.Size,Symbol.Color,WhatFor);
         end;

         procedure PETMARAboutBox(ProgramName : ANSIstring; Modifier : ANSIstring = '');
         var
            AboutBox: tPetmarAboutBox;
            BitMap : tMyBitmap;
         begin
            StopSplashing;
            AboutBox := TPetmarAboutBox.Create(Application);
            petImage.CreateBitmap(Bitmap,110,160);
            BitMap.Canvas.Pen.Color := clYellow;
            BitMap.Canvas.Rectangle(0,0,110,160);
            BitMap.Canvas.Brush.Color := clYellow;
            BitMap.Canvas.FloodFill(5,5,clYellow,fsBorder);
            with AboutBox do begin
               DisplayNevadella(BitMap.Canvas,4,-45,1,clBlue);
               Image1.Picture.Graphic := Bitmap;
               BitMap.Free;
               ProgramIcon.Height := Application.Icon.Height;
               ProgramIcon.Width := Application.Icon.Width;
               Label2.Caption := Modifier;
               Label3.Caption := 'Database: ' + DBdriver;
               ProgramIcon.Canvas.Draw(0,0,Application.Icon);
               Version.Caption := BuildString;
               ProductName.Caption := ShortEXEName;
               ShowModal;
            end;
            AboutBox.Free;
         end;


         function LocalIP: string;
         type
            TaPInAddr = array [0..10] of PInAddr;
            PaPInAddr = ^TaPInAddr;
         var
             phe: PHostEnt;
             pptr: PaPInAddr;
             Buffer: array [0..63] of ANSIchar;
             i: Integer;
             GInitData: TWSADATA;
         begin
             WSAStartup($101, GInitData);
             Result := '';
             GetHostName(Buffer, SizeOf(Buffer));
             phe := GetHostByName(buffer);
             if phe = nil then Exit;
             pptr := PaPInAddr(Phe^.h_addr_list);
             i := 0;
             while pptr^[i] <> nil do    begin
               result := StrPas(inet_ntoa(pptr^[i]^));
               Inc(i);
             end;
             WSACleanup;
         end;



         procedure StringListToContinue(SaveOut : boolean; Mess : tStringList);
         var
            mess_cont_form: Tmess_cont_form;
            x,y,
            i,ml : integer;
         begin
            x := 0;
            y := 0;
            if SaveOut then begin
               mess_cont_form := Tmess_cont_form.Create(Application);
               ml := 0;
               for i := 0 to pred(Mess.Count) do begin
                  if Length(Mess.Strings[i]) > ml then ml := Length(Mess.Strings[i]);
                  mess_cont_form.Memo1.Lines.Add(Mess.Strings[i]);
               end;
               mess_cont_form.Height := 100 + (18 * Mess.Count);
               if (ml <  20) then mess_cont_form.Width := 200
               else mess_cont_form.Width := 20 + (8 * ml);

               if (x <> 0) and (y <> 0) then begin
                  if x + mess_cont_form.Width > Screen.Width then x := x - mess_cont_form.Width;
                  mess_cont_form.Left := x;
                  if y + mess_cont_form.Height > Screen.Height then y := y - mess_cont_form.Height;
                  mess_cont_form.Top := y;
               end;
               mess_cont_form.ShowModal;
               mess_cont_form.Free;
            end
            else begin
               if DualMonitor and (x = 0) and (y = 0) then begin
                  x := 400;
                  y := 400;
               end;
               if (x = 0) and (y = 0) then MessageDlg(Mess[0],mtInformation,[mbOK],0)
               else MessageDlgPos(Mess[0],mtInformation,[mbOK],0,x,y);
            end;
         end;


         function GetNewBMPSize(var x,y : int32;  aCapt : shortstring) : boolean;
         var
            NewBMPForm : tNewBMPForm;
         begin
            Result := false;
            NewBMPForm := TNewBMPForm.Create(Application);
            if (aCapt <> '') then NewBMPForm.Caption := aCapt;
            NewBMPForm.WidthEdit.Text := IntToStr(x);
            NewBMPForm.HeightEdit.Text := IntToStr(y);
            if (NewBMPForm.ShowModal <> idCancel) then begin
               x := StrToInt(NewBMPForm.WidthEdit.Text);
               y := StrToInt(NewBMPForm.HeightEdit.Text);
               result := true;
            end;
            NewBMPForm.Free;
         end;


         procedure QuickOpenEditWindow(FName : PathStr; WindowCaption : ANSIstring; ListenToClear : boolean = false; AddLineNumbers : boolean = false);
         var
            Max,i : integer;
            EditWindow : TPetEditF;
            S : TFileStream;
         begin
            if (FName = '') then GetFileFromDirectory('File to display','*.*',fName);
            if WindowCaption = '' then WindowCaption := ExtractFileName(fName);
            if FileExists(FName) then try
               EditWindow := TPetEditf.Create(Application);
               EditWindow.ListenToClear := ListenToClear;
               EditWindow.NoOpenNewOptions;
               with EditWindow do begin
                  FileInWindow := FName;
                  S:= TfileStream.Create(FName,fmOpenRead + fmShareDenyNone);
                  try
                     RichEdit1.Lines.LoadFromStream(S);
                  Finally
                     S.Free;
                  end;

                  if AddLineNumbers then AddLineNumbers1Click(nil);
                  EditWindow.Caption := WindowCaption;
                  with RichEdit1.Lines do if (Count < 25) then ClientHeight := 14 * Count + 100
                  else EditWindow.Height := 300;
                  Max := 0;
                  for i := 0 to pred(RichEdit1.Lines.Count) do
                     if length(RichEdit1.Lines[i]) > Max then Max := length(RichEdit1.Lines[i]);
                  if (Max < 30) then ClientWidth := 300
                  else if (Max < 64) then ClientWidth := 9 * Max
                  else begin
                     ClientWidth := 640;
                     RichEdit1.ScrollBars := ssBoth;
                  end;
               end;
            finally
            end;
         end;

         procedure ModalEditWindow(FName : PathStr; WindowCaption : ANSIstring);
         var
            EditWindow : TPetEditF;
         begin
            if FileExists(FName) then begin
               EditWindow := TPetEditf.Create(Application);
               with EditWindow do begin
                  NoOpenNewOptions;
                  FormStyle := fsNormal;
                  Visible := false;
                  FileInWindow := FName;
                  RichEdit1.Lines.LoadFromFile(FName);
                  Caption := WindowCaption;
                  Width := 640;
                  Height := 300;
                  ShowModal;
                  Close;
               end;
            end;
         end;


         procedure ModalStringListDisplay(var aList : tStringList; WindowCaption : ANSIstring; Purge : boolean = true);
         var
            fName : PathStr;
            I : Integer;
         begin
            fName := System.IOUtils.TPath.Combine(MDTempDir, 'temp_string_list.txt');
            aList.SaveToFile(fName);
            ModalEditWindow(FName,WindowCaption);
            {$IfDef TrackFileDeletion} MessageToContinue('ModalStringListDisplay call DeleteFileIfExists'); {$EndIf}
            aList.Clear;
            if Purge then aList.Free
            else begin
               aList.LoadFromFile(fName);
               for I := pred(aList.Count) downto 0 do begin
                  aList.Strings[i] := ptTrim(aList.Strings[i]);
                  if aList.Strings[i] = '' then Alist.Delete(i);
               end;
            end;
            DeleteFileIfExists(FName);
         end;


         procedure DisplayString(Findings : String; WindowTitle : shortString; ListenToClear : boolean = false);
         var
            FName : pathStr;
         begin
            fname := NextFileNumber(MDTempDir,'results_','.txt');
            tfile.WriteAllText(fName,Findings);
            QuickOpenEditWindow(FName,WindowTitle,ListenToClear);
         end;


         procedure DisplayAndPurgeStringList(var Findings : tStringList; WindowTitle : shortString; ListenToClear : boolean = false);
         var
            FName : pathStr;
         begin
            if (Findings <> Nil) then begin
               if Findings.Count > 0 then begin
                  FName := NextFileNumber(MDTempDir,'results+','.txt');
                  Findings.SaveToFile(FName);
                  QuickOpenEditWindow(FName,WindowTitle,ListenToClear);
               end;
               Findings.Free;
               Findings := Nil;
            end;
         end;

         procedure GetSymbol(var DrSymbol : tDrawingSymbol; var SymSize   : byte; var WantColor  : tPlatformColor; WhatFor : shortstring = '');
         begin
            PickSymbolForm := TPickSymbolForm.Create(Application);
            with PickSymbolForm do begin
               if (WhatFor <> '') then Caption := WhatFor;
               GetPointColor := WantColor;
               GetPointSymbol := DrSymbol;
               GetPointSize := SymSize;
               UpDown2.Position := GetPointSize;
               SymCount := ord(GetPointSymbol);
               SampleSymbol;
               if (ShowModal <> idCancel) then begin
                  WantColor := GetPointColor;
                  DrSymbol := GetPointSymbol;
                  SymSize := GetPointSize;
               end;
               Free;
            end;
         end;

         procedure GetDate(var Month,Day,Year : integer);
         var
            Duration : integer;
         begin
            GetDateAndDuration(Month,Day,Year,Duration,false);
         end;

         procedure GetDateAndDuration(var Month,Day,Year,Duration : integer; ShowDuration : boolean = true);
         var
            iMonth,iDay,iYear : word;
         begin
            if Month = -99 then begin
               DecodeDate(Now,iYear,iMonth,iDay);
               Year := iYear;
               Month := iMonth;
               Day := iDay;
            end;
            GetDateForm := TGetDateForm.Create(Application);

            with GetDateForm do begin
               Edit1.Text := IntToStr(Month);
               Edit2.Text := IntToStr(Day);
               Edit3.Text := IntToStr(Year);
               Edit4.Text := IntToStr(Duration);
               Edit4.Visible := ShowDuration;
               CheckBox1.Checked := MDDef.RiseSet;
               CheckBox2.Checked := MDDef.MoonPhase;
               ShowModal;
               CheckEditString(Edit1.Text,Month);
               if (Month > 12) then Month := 12;
               CheckEditString(Edit2.Text,Day);
               if (Day > 31) then Day := 31;
               CheckEditString(Edit3.Text,Year);
               CheckEditString(Edit4.Text,Duration);
               MDDef.RiseSet := CheckBox1.Checked;
               MDDef.MoonPhase := CheckBox2.Checked;
            end;
         end;


         procedure PickPattern(WhatFor : shortString; var Style : tBrushStyle; var FillColor,BorderColor : tPlatformColor; var inBorderWidth : integer; BackBMP : tMyBitmap = Nil);
         var
           PickFillForm : PickFillPattern.TPickFillForm;
         begin
            PickFillForm := TPickFillForm.Create(Application);
            with PickFillForm do begin
              Caption := 'Pick pattern for ' + WhatFor;
              BaseBitmap := BackBMP;
              PickStyle := Style;
              PickFillColor := FillColor;
              PickBorderColor := BorderColor;
              PickFillForm.PickBorderWidth := pred(inBorderWidth);
              ColorBitBtn(BitBtn2,FillColor);
              ColorBitBtn(BitBtn1,BorderColor);
              SpinButton1UpClick(Nil);
              ShowModal;
              Style := PickStyle;
              FillColor := PickFillColor;
              BorderColor := PickBorderColor;
              inBorderWidth := PickFillForm.PickBorderWidth;
            end;
         end;


         function SpectrumColorFunct(z,Min,Max : float64) : tColor;
         var
            r,g,b : byte;
         begin
            if (z < Min) then z := Min;
            if (z > Max) then z := Max;
            z := 380 + 400 * (z - Min) / (Max - Min);
            WavelengthToRGB(z, R,G,B);
            Result := RGB(r,g,b);
         end;


         procedure DrawPatternOnBitmap(var Bitmap : tMyBitmap; FillColor,SymbolLineBorderColor : tPlatformColor; SymbolFill : tBrushStyle; SymbolLineBorderSize : integer);
         begin
            Bitmap.Canvas.Brush.Color := ConvertPlatformColorToTColor(FillColor);
            Bitmap.Canvas.Brush.Style := SymbolFill;
            Bitmap.Canvas.Pen.Color := ConvertPlatformColorToTColor(SymbolLineBorderColor);
            Bitmap.Canvas.Pen.Width := SymbolLineBorderSize;
            Bitmap.Canvas.Rectangle(2,2,Bitmap.Width-2,Bitmap.Height-2);
         end;


         procedure FillPatternOnBitBtn(BitBtn : tBitBtn; FillColor,SymbolLineBorderColor : tPlatformColor; SymbolFill : tBrushStyle; SymbolLineBorderSize : integer);
         var
            Bitmap : tMyBitmap;
         begin
            CreateBitmap(bitmap,20,20);
            DrawPatternOnBitmap(Bitmap, FillColor,SymbolLineBorderColor,SymbolFill,SymbolLineBorderSize);
            BitBtn.Glyph := Bitmap;
            Bitmap.Free;
         end;


         procedure SimulateKeystroke(Key : byte;    extra : DWORD);
         begin
            keybd_event(Key, extra,  0,  0);
            keybd_event(Key, extra, KEYEVENTF_KEYUP, 0);
         end;


         procedure CaptureActiveWindowToClipboard;
         begin
            ApplicationProcessMessages;
            Delay(250);
            SimulateKeystroke(VK_SNAPSHOT, 1);
            Delay(250);
            ApplicationProcessMessages;
         end;


         procedure CaptureEntireScreenToClipboard;
         begin
            ApplicationProcessMessages;
            Delay(250);
            SimulateKeystroke(VK_SNAPSHOT, 0);
            Delay(250);
            ApplicationProcessMessages;
         end;


         function ColorToHtml(DColor:TColor) : ShortString;
         //from http://delphi.about.com/cs/adptips2000/a/bltip1000_3.htm
         var
           tmpRGB : TColorRef;
         begin
           tmpRGB := ColorToRGB(DColor) ;
           Result:= Format('#%.2x%.2x%.2x',[GetRValue(tmpRGB),GetGValue(tmpRGB),GetBValue(tmpRGB)]) ;
         end; {function ColorToHtml}


         function HLStoTcolor(H,L,S : float64) : tcolor;
         var
            aRGBTrip : tRGBTriple;
         begin
            aRGBTrip := RGBtripFromHSI(H,S,L);
            Result := RGB(aRGBTrip.rgbtred,aRGBTrip.rgbtgreen,aRGBTrip.rgbtblue);
         end;


         procedure RGBtripToHLS(Color : tRGBTriple; var h,l,s : float64);
         var
            r,g,b : integer;
         begin
            r := Color.rgbtRed;
            g := Color.rgbtGreen;
            b := Color.rgbtBlue;
            RGBtoHLS(r,g,b,h,l,s);
         end;

         procedure tColortoHLS(Color : tColor; var h,l,s : float64);
         var
            r,g,b : byte;
         begin
            GetRGBfromTColor(Color,r,g,b);
            RGBtoHLS(r,g,b,h,l,s);
         end;


         procedure HLStoRGB(h,l,s : float64; var r,g,b : byte);
         {Foley & Van Dam, p.619;  hue is 0..360, lightness and saturation 0..1}
         var
            m1,m2 : float64;

               function Value(n1,n2,hue : float64) : byte;
               begin
                  PetMath.ValueInRange(hue,0,360);
                  if Hue < 60 then Result := round(255 * (n1 + (n2-n1) * hue /60))
                  else if Hue < 180 then Result := round(255 * n2)
                  else if Hue < 240 then Result := round(255 * (n1 + (n2-n1) * (240-hue) /60))
                  else Result := round(255 * n1);
               end;

          begin
             PetMath.ValueInRange(l,0,1);
             PetMath.ValueInRange(s,0,1);
             if l < 0.5 then m2 := l * (l+s)
             else m2 := l + s - l*s;
             m1 := 2*l - m2;
             if s < 0.001 then begin
                r := 255;
                g := 255;
                b := 255;
             end
             else begin
                r := Value(m1,m2,h+120);
                g := Value(m1,m2,h);
                b := Value(m1,m2,h-120);
             end;
          end;


         procedure RGBtoHLS(r,g,b : integer; var h,l,s : float64);
         {Foley & Van Dam, p.618;  hue is 0..360, lightness and saturation 0..1}
         var
            rr,gr,br,min,max,rc,gc,bc : float64;
         begin
            rr := r/255;
            gr := g/255;
            br := b/255;
            min := rr;
            if (gr < min) then min := gr;
            if (br < min) then min := br;

            max := rr;
            if (gr > max) then max := gr;
            if (br > max) then max := br;
            l := 0.5 * (min + max);
            if abs(max - min) < 0.001 then begin
               s := 0;
               h := 0; {undefined really}
            end
            else begin
               if (l < 0.5) then s := (max - min) / (max + min)
               else s := (max - min) / (2- max - min);
               rc := (max -rr) / (max - min);
               gc := (max -gr) / (max - min);
               bc := (max -br) / (max - min);
               if abs(rr -max) < 0.0001 then h := bc - gc           {yellow to magenta}
               else if abs(gr -max) < 0.0001 then h := 2 + rc - bc  {cyan to yellow}
               else if abs(br -max) < 0.0001 then h := 4 + gc - rc; {magenta to yellow}
               h := 60 * h;
               if (h < 0) then h := h + 360;
            end;
         end;


         function RGBtripFromHSI(Hue,Sat,Int : float64) : tPlatformColor;
         {PE&RS, Oct 1995, p.1228}
         var
            v1,v2 : float64;
         begin
            V1 := Sat * cosDeg(Hue);
            V2 := Sat * sinDeg(Hue);
            Result := RGBtrip( round(1.0*Int - 0.408248 * V1 + 1.224745 * V2), round(1.0*Int - 0.408248 * V1 - 1.224745 * V2), round(1.0*Int + 0.816497 * V1));
         end;



function MakeColorScaleBitmap(width,height : integer; ColorScheme : tLegendColors; ColorTable : tColorTableDefinitions) : tMyBitmap;
var
   i,j : integer;
begin
   CreateBitmap(Result,width,height);
   for i := 0 to pred(width) do begin
      j := succ(trunc(ColorTable.ZTableEntries * i / Width));
      Result.Canvas.Pen.Color := ConvertPlatformColorToTColor(ColorTable.ZTableColors[j]);
      DrawLine(Result,i,0,i,pred(height));
   end;
end;

procedure GetTerrainRGBTrip(z,Min,Max : float64; var r,g,b : byte);
var
   zi : integer;

      procedure InterpolateColor(c1,c2 : integer; Dist : float64);
      begin
         r := TerrainCuts[c1,1] + round(Dist * (TerrainCuts[c2,1] - TerrainCuts[c1,1])) ;
         g := TerrainCuts[c1,2] + round(Dist * (TerrainCuts[c2,2] - TerrainCuts[c1,2])) ;
         b := TerrainCuts[c1,3] + round(Dist * (TerrainCuts[c2,3] - TerrainCuts[c1,3])) ;
      end;

begin
   zi := ValidByteRange(round(255.0 * (z - Min) / (Max - Min)));
   case zi of
      0..80 : InterpolateColor(0,1,zi/80);
      81..170 : InterpolateColor(1,2,(zi-81)/90);
      171..200 : InterpolateColor(2,3,(zi-171)/29);
      201..255 : InterpolateColor(3,4,(zi-201)/54);
      else begin
         r := 0;
         g := 0;
         b := 0;
      end;
   end;
end;


function RainbowRGBFunct(z,MinV,MaxV : float64) : tPlatformColor;
const
   ColorStep = 2;
   FirstColor = 136;
var
   zi : integer;
begin
   if (abs(MaxV-MinV) < 0.0001) then begin
      Result := RGBTrip(255,0,0);
   end
   else begin
      zi := ValidByteRange(round(255.0 * (z - MinV) / (MaxV - MinV)));
      case zi of
         0..42 : Result := RGBTrip(0, 0,FirstColor + ColorStep * zi); {blues}
         43..85 : Result := RGBTrip(0,FirstColor + ColorStep * (zi-43),FirstColor + ColorStep * (zi-43)); {Cyans}
         86..128 : Result := RGBTrip(0,FirstColor + ColorStep * (zi-86),0); {greens}
         129..171: Result := RGBTrip(FirstColor + ColorStep * (zi-129), FirstColor + ColorStep * (zi-129),0);  {yellows}
         172..214 : Result := RGBTrip(FirstColor + ColorStep * (zi-172), 0,0);  {reds}
         215..255 : Result := RGBTrip(FirstColor + ColorStep * (zi-215), 0,FirstColor + ColorStep * (zi-215));  {magentas}
         else Result := RGBTrip(0,0,0);
      end;
   end;
end {function};


function TerrainRGBFunct(z,Min,Max : float64) : tPlatformColor;
var
   r,g,b : byte;
begin
   GetTerrainRGBTrip(z,Min,Max,r,g,b);
   Result := RGBtrip(r,g,b);
end;


function OceanRGBFunct(z,Min,Max : float64) : tPlatformColor;
var
   zi : integer;
begin
   zi := ValidByteRange(round(255.0 * (z - Min) / (Max - Min)));
   if (zi < 130) then Result := RGBTrip(0,0,round(90 + zi))
   else Result := RGBTrip(0,0,round(90 + (zi-130)));
end;


         function TerrainTColor(z,aMin,aMax : float64) : TColor;
         var
            r,g,b : byte;
         begin
            GetTerrainRGBTrip(z,aMin,aMax,r,g,b);
            Result := RGB(r,g,b);
         end;


         procedure PrintImageToSpecifiedSize(Image1 : TImage; PrinterXInches,PrinterYInches : float64);
         {prints out to scale}

                procedure PrintAnImage(DestRect,SourceRect : TRect; ABitmap : tMyBitmap; PrintTitle : ShortString = '');
               var
                  Header,Bits : Pointer;
                  HeaderSize,
                  BitsSize       : cardinal;
                  Total,OnPart,
                  DivideFactor,x,y  : integer;
                  xyDestRect,xySourceRect : tRect;
               begin
                  DivideFactor := 1;
                  while ((DestRect.Right - DestRect.Left) div DivideFactor) * ((DestRect.Bottom - DestRect.Top) div DivideFactor) > 1500000 do inc(DivideFactor);
                  {$IfDef RecordPrint} WriteLineToDebugFile(' Divide factor set to ' + IntToStr(DivideFactor)); {$EndIf}
                  Printer.BeginDoc;
                  GetDIBSizes(ABitmap.Handle,HeaderSize,BitsSize);
                  GetMem(Header,HeaderSize);
                  GetMem(Bits,BitsSize);
                  try
                     GetDIB(ABitmap.Handle, ABitmap.Palette, Header^, Bits^);
                     StartProgress('Print ' + PrintTitle);
                     Total := sqr(DivideFactor);
                     OnPart := 0;
                     for x := 0 to pred(DivideFactor) do begin
                        for y := 0 to pred(DivideFactor) do begin
                           inc(OnPart);
                           UpdateProgressBar(OnPart / Total);
                           xySourceRect.Left := SourceRect.Left + x * (SourceRect.Right - SourceRect.Left) div DivideFactor;
                           xySourceRect.Right := xySourceRect.Left + (SourceRect.Right - SourceRect.Left) div DivideFactor;
                           xySourceRect.Top := SourceRect.Top + y * (SourceRect.Bottom - SourceRect.Top) div DivideFactor;
                           xySourceRect.Bottom := xySourceRect.Top + (SourceRect.Bottom - SourceRect.Top) div DivideFactor;
                           xyDestRect.Left := DestRect.Left + x * (DestRect.Right - DestRect.Left) div DivideFactor;
                           xyDestRect.Right := xyDestRect.Left + (DestRect.Right - DestRect.Left) div DivideFactor;
                           xyDestRect.Top := DestRect.Top + (pred(DivideFactor) - y)* (DestRect.Bottom - DestRect.Top) div DivideFactor;
                           xyDestRect.Bottom := xyDestRect.Top + (DestRect.Bottom - DestRect.Top) div DivideFactor;
                           StretchDIBits(Printer.Canvas.Handle, xyDestRect.Left, xyDestRect.Top,xyDestRect.Right - xyDestRect.Left, xyDestRect.Bottom - xyDestRect.Top,
                               xySourceRect.Left,xySourceRect.Top,xySourceRect.Right - xySourceRect.Left,xySourceRect.Bottom - xySourceRect.Top,Bits, tBitmapInfo(Header^),DIB_RGB_COLORS, SRCCOPY);
                        end;
                     end;
                  finally
                     FreeMem(Header, HeaderSize);
                     FreeMem(Bits, BitsSize);
                     EndProgress;
                     Printer.EndDoc;
                  end;
               end;

         var
            Bitmap  : tMyBitmap;
            PrinterXDPI,PrinterYDPI,
            sh,sw,
            PrinterHeight,PrinterWidth,x,y,
            XReps,YReps,xstart,ystart,xsize,ysize: integer;
            TargetDPI   : float64;
         begin
            PrinterWidth := GetDeviceCaps(Printer.Handle,HorzRes);
            PrinterXDPI := GetDeviceCaps(Printer.Handle,LogPixelsX);
            PrinterHeight := GetDeviceCaps(Printer.Handle,VertRes);
            PrinterYDPI := GetDeviceCaps(Printer.Handle,LogPixelsY);

            TargetDPI := Image1.Width / PrinterXInches;
            xReps := succ(trunc(PrinterXInches / (PrinterWidth / PrinterXDPI)));
            yReps := succ(trunc(PrinterYInches / (PrinterHeight / PrinterYDPI)));

            sw := round(PrinterWidth * TargetDPI / PrinterXDPI);
            sh := round(PrinterHeight * TargetDPI / PrinterYDPI);

            {$IfDef RecordPrint} WriteLineToDebugFile('In PrintImageToSpecifiedSize Sheets required: '+ IntToStr(xreps) + ' by ' + IntToStr(yreps));           {$EndIf}

            ShowHourglassCursor;
            for x := 0 to pred(XReps) do begin
               for y := 0 to pred(YReps) do begin
                  xstart := sw * x;
                  ystart := sh * y;
                  xsize := sw;
                  if xstart + xsize > Image1.Width then Xsize := abs(xstart - Image1.Width);

                  ysize := sh;
                  if ystart + ysize > Image1.Height then ysize := abs(Image1.Height - ystart);

                  CreateBitmap(Bitmap,xsize,ysize);
                  Bitmap.Canvas.CopyRect(Rect(0,0,xsize,ysize),Image1.Canvas, Rect(xstart,ystart,xstart+xsize,ystart+ysize));
                  PrintAnImage(Rect(0,0,round(PrinterWidth*xsize/sw),round(PrinterHeight*ysize/sh)),Rect(0,0,Bitmap.Width,Bitmap.Height),Bitmap);
                  Bitmap.Free;
               end;
            end;
            ShowDefaultCursor;
         end;


         procedure DisplayNevadella;
         const
            Nevadella : array[1..424,1..2] of byte = (
            (0,0),(100,100),(100,92),(99,91),(99,87),(98,86),(98,84),(97,83),
            (97,81),(96,80),(96,79),(95,78),(95,77),(94,76),(94,75),(93,74),
            (92,73),(92,72),(91,71),(90,70),(89,69),(89,68),(88,67),(87,66),
            (86,65),(85,64),(84,63),(83,62),(82,61),(81,61),(80,60),(79,59),
            (78,58),(77,58),(76,57),(75,56),(74,56),(73,55),(72,55),(71,54),
            (70,54),(67,53),(66,52),(64,52),(63,51),(59,51),(58,50),(42,50),
            (41,51),(37,51),(36,52),(34,52),(33,53),(31,53),(30,54),(29,54),
            (28,55),(27,55),(26,56),(25,56),(24,57),(23,58),(22,58),(21,59),
            (20,60),(19,61),(18,61),(17,62),(16,63),(15,64),(14,65),(13,66),
            (12,67),(11,68),(11,69),(10,70),(9,71),(8,72),(8,73),(7,74),
            (6,75),(6,76),(5,77),(5,78),(4,79),(4,80),(3,81),(3,83),
            (2,84),(2,86),(1,87),(1,91),(0,92),(0,100),(0,0),(36,64),
            (35,64),(35,65),(33,67),(34,66),(32,68),(32,69),(31,70),(31,72),
            (30,73),(30,78),(0,0),(64,64),(65,64),(65,65),(66,66),(67,67),
            (68,68),(68,69),(69,70),(69,72),(70,73),(70,78),(0,0),(0,100),
            (10,83),(0,0),(100,100),(90,83),(0,0),(10,83),(90,83),(0,0),
            (40,83),(48,63),(0,0),(60,83),(52,63),(0,0),(48,63),(52,63),
            (0,0),(46,68),(54,68),(0,0),(44,72),(56,72),(0,0),(42,76),
            (58,76),(0,0),(30,78),(36,64),(0,0),(70,78),(64,64),(0,0),
            (15,85),(85,85),(0,0),(12,90),(87,90),(0,0),(12,90),(15,85),
            (0,0),(87,90),(85,85),(0,0),(45,90),(45,85),(0,0),(55,90),
            (55,85),(0,0),(15,92),(85,92),(0,0),(12,97),(87,97),(0,0),
            (12,97),(15,92),(0,0),(85,92),(87,97),(0,0),(45,97),(45,92),
            (0,0),(55,97),(55,92),(0,0),(10,99),(90,99),(0,0),(12,105),
            (87,105),(0,0),(10,99),(8,115),(0,0),(90,99),(92,115),(0,0),
            (8,115),(12,105),(0,0),(92,115),(87,105),(0,0),(12,97),(15,92),
            (0,0),(85,92),(87,97),(0,0),(45,99),(45,105),(0,0),(55,99),
            (55,105),(0,0),(17,107),(83,107),(0,0),(19,111),(81,111),(0,0),
            (19,111),(17,107),(0,0),(81,111),(83,107),(0,0),(45,111),(45,107),
            (0,0),(55,111),(55,107),(0,0),(19,113),(81,113),(0,0),(21,117),
            (79,117),(0,0),(21,117),(19,113),(0,0),(79,117),(81,113),(0,0),
            (45,117),(45,113),(0,0),(55,117),(55,113),(0,0),(21,119),(79,119),
            (0,0),(23,123),(77,123),(0,0),(23,123),(21,119),(0,0),(77,123),
            (79,119),(0,0),(45,123),(45,119),(0,0),(55,123),(55,119),(0,0),
            (23,125),(77,125),(0,0),(25,129),(75,129),(0,0),(25,129),(23,125),
            (0,0),(75,129),(77,125),(0,0),(45,129),(45,125),(0,0),(55,129),
            (55,125),(0,0),(25,131),(75,131),(0,0),(27,135),(73,135),(0,0),
            (27,135),(25,131),(0,0),(73,135),(75,131),(0,0),(45,135),(45,131),
            (0,0),(55,135),(55,131),(0,0),(27,137),(73,137),(0,0),(29,141),
            (71,141),(0,0),(29,141),(27,137),(0,0),(71,141),(73,137),(0,0),
            (45,141),(45,137),(0,0),(55,141),(55,137),(0,0),(29,143),(71,143),
            (0,0),(31,147),(69,147),(0,0),(31,147),(29,143),(0,0),(69,147),
            (71,143),(0,0),(45,147),(45,143),(0,0),(55,147),(55,143),(0,0),
            (31,149),(69,149),(0,0),(33,153),(67,153),(0,0),(33,153),(31,149),
            (0,0),(67,153),(69,149),(0,0),(45,153),(45,149),(0,0),(55,153),
            (55,149),(0,0),(33,155),(67,155),(0,0),(35,159),(65,159),(0,0),
            (35,159),(33,155),(0,0),(65,159),(67,155),(0,0),(45,159),(45,155),
            (0,0),(55,159),(55,155),(0,0),(35,161),(65,161),(0,0),(37,165),
            (63,165),(0,0),(37,165),(35,161),(0,0),(63,165),(65,161),(0,0),
            (45,165),(45,161),(0,0),(55,165),(55,161),(0,0),(37,167),(63,167),
            (0,0),(39,171),(61,171),(0,0),(39,171),(37,167),(0,0),(61,171),
            (63,167),(0,0),(45,171),(45,167),(0,0),(55,171),(55,167),(0,0),
            (45,173),(55,173),(0,0),(45,173),(50,199),(0,0),(55,173),(50,199) );

            PTBRPts = 161;
            PTBR : array [1..PTBRpts,1..2] of byte = ( (0,0),
            (75,109),(27,109),(0,0),(42,200),(42,146),(32,146),(32,134),(65,134),
            (65,134),(65,146),(42,146),(47,146),(59,157),(60,199),(0,0),(31,126),
            (20,94),(81,94),(71,126),(31,126),(0,0),(20,83),(98,83),(98,90),(97,90),
            (97,97),(96,97),(96,103),(95,103),(95,107),(94,107),(94,110),(93,111),
            (93,113),(92,113),(92,116),(91,116),(91,118),(90,118),(90,120),(89,121),
            (89,122),(88,123),(88,124),(87,125),(87,126),(86,126),(86,127),(85,128),
            (85,129),(84,129),(84,130),(83,130),(83,131),(82,131),(82,132),(81,132),
            (81,134),(80,135),(80,136),(79,136),(79,138),(0,0),(24,134),(23,134),
            (23,133),(22,133),(22,132),(21,132),(21,131),(20,131),(20,130),(19,130),
            (19,129),(18,129),(18,128),(18,127),(17,127),(17,126),(16,125),(16,124),
            (15,124),(15,123),(14,123),(14,122),(13,121),(13,120),(12,118),(11,117),
            (10,114),(9,111),(8,108),(7,106),(6,106),(6,104),(5,104),(5,102),(4,101),
            (4,99),(3,99),(2,95),(2,90),(1,90),(1,82),(0,82),(0,66),(1,66),(1,63),
            (2,62),(2,60),(3,60),(3,59),(4,59),(4,58),(5,58),(5,58),(6,57),(7,57),
            (7,56),(9,56),(9,55),(11,55),(11,54),(13,54),(14,53),(17,53),(17,52),
            (21,52),(21,51),(31,51),(31,50),(58,50),(58,51),(79,51),(79,52),(85,52),
            (85,53),(87,53),(87,54),(89,54),(89,55),(91,55),(91,56),(92,56),(92,57),
            (93,57),(94,58),(95,58),(95,59),(96,59),(96,60),(97,61),(97,62),(98,63),
            (98,65),(99,65),(99,69),(100,69),(100,74),(1,74));

         var
            i : integer;
         begin
            with Canvas do begin
               Pen.Width := 1;
               Pen.Color := Color;
               i := 1;
               if Odd(Random(500)) then begin
                  while i <= 424 do begin
                     if (Nevadella[i,1] = 0) and (Nevadella[i,2] = 0) then begin
                        inc(i);
                        MoveTo(xoff+Size*Nevadella[i,1],yoff+Size*Nevadella[i,2])
                     end
                     else LineTo(xoff+Size*Nevadella[i,1],yoff+Size*Nevadella[i,2]);
                     inc(i);
                  end {for i};
               end
               else begin
                  Pen.Width := 3;
                  while i <= PTBRpts do begin
                     if (PTBR[i,1] = 0) and (PTBR[i,2] = 0) then begin
                        inc(i);
                        MoveTo(xoff+size*succ(PTBR[i,1]),yoff+size*PTBR[i,2]);
                     end
                     else LineTo(xoff+size*succ(PTBR[i,1]),yoff+size*PTBR[i,2]);
                     inc(i);
                  end;
               end;
            end {with};
         end;


         function MonthColor(i : integer) : TPlatformColor;
         begin
            case i of
                1 : MonthColor := RGBtrip(0,0,100) ;
                2 : MonthColor := RGBtrip(0,0,200);
                3 : MonthColor := rgbTrip(0,100,100);
                4 : MonthColor := rgbTrip(0,200,200);
                5 : MonthColor := rgbTrip(0,100,0);
                6 : MonthColor := rgbTrip(0,200,0);
                7 : MonthColor := rgbTrip(100,100,0);
                8 : MonthColor := rgbTrip(200,200,0);
                9 : MonthColor := rgbTrip(100,0,0);
               10 : MonthColor := rgbTrip(200,0,0);
               11 : MonthColor := rgbTrip(100,0,100);
               12 : MonthColor := rgbTrip(200,0,200);
               else MonthColor := claWhite;
            end;
         end;


         procedure ScreenSymbol(Canvas : TCanvas; x,y : integer; Symbol : tFullSymbolDeclaration);
         begin
            ScreenSymbol(Canvas,x,y,Symbol.DrawingSymbol,Symbol.Size,Symbol.Color);
         end;


         procedure ScreenSymbol(Canvas : TCanvas; x,y : integer; Sym : tDrawingSymbol; size: integer; color : TPlatformColor);
         var
            PointsInPolyLine,pw : integer;
            PolyLinePoints : array[0..5] of tPoint;
         begin
            try
               if (X < 0) or (y < 0) then exit;
                  pw := 1;
                  if Sym in [Ex,Splat] then begin
                     pw := succ(Size) div 2;
                     if (pw = 0) then pw := 1;
                  end;
                  if Sym in [VertLine] then pw := Size;

                  Canvas.Pen.Width := pw;
                  Canvas.Pen.Color := ConvertPlatformColorToTColor(Color);

                  if Sym in [FilledCircle,FilledBox,FilledDiamond,FilledUpTri,FilledDownTri] then begin
                     Canvas.Brush.Color := ConvertPlatformColorToTColor(Color);
                     Canvas.Brush.Style := bsSolid;
                  end
                  else Canvas.Brush.Style := bsClear;
                  case Sym of
                     FilledBox,Box : Canvas.Rectangle(x-size,y-size,x+size,y+size);
                     FilledCircle,
                     Circle  : Canvas.Ellipse(X+Size,Y+Size,X-Size,Y-Size);
                     Dot     : Canvas.Pixels[x,y] := ConvertPlatformColorToTColor(Color);
                     Cross   : begin
                                  Canvas.MoveTo(x+Size,y);  Canvas.LineTo(x-Size,y);
                                  Canvas.MoveTo(x,y+Size);  Canvas.LineTo(x,y-Size);
                               end;
                     VertLine : begin
                                  Canvas.MoveTo(x,y+Size);  Canvas.LineTo(x,y-Size);
                               end;
                     EX      : begin
                                  Canvas.MoveTo(x+Size,y+Size); Canvas.LineTo(x-Size,y-Size);
                                  Canvas.MoveTo(x-Size,y+Size); Canvas.LineTo(x+Size,y-Size);
                               end;
                     FilledDiamond,
                     Diamond : begin
                                  PointsInPolyLine := 5;
                                  PolyLinePoints[0].x := x-Size;
                                  PolyLinePoints[0].y := y;
                                  PolyLinePoints[1].x := x;
                                  PolyLinePoints[1].y := y+size;
                                  PolyLinePoints[2].x := x+size;
                                  PolyLinePoints[2].y := y;
                                  PolyLinePoints[3].x := x;
                                  PolyLinePoints[3].y := y-size;
                                  PolyLinePoints[4].x := x-size;
                                  PolyLinePoints[4].y := y;
                                  Canvas.Polygon(slice(PolyLinePoints,PointsInPolyLine))
                               end;
                     Splat   : begin
                                  Canvas.MoveTo(x+Size,y);  Canvas.LineTo(x-Size,y);
                                  Canvas.MoveTo(x,y+Size);  Canvas.LineTo(x,y-Size);
                                  Canvas.MoveTo(x+Size,y+Size); Canvas.LineTo(x-Size,y-Size);
                                  Canvas.MoveTo(x-Size,y+Size); Canvas.LineTo(x+Size,y-Size);
                               end;
                     FilledUpTri,
                     UpTri   : begin
                                  PointsInPolyLine := 4;
                                  PolyLinePoints[0].x := x-Size;
                                  PolyLinePoints[0].y := y+Size;
                                  PolyLinePoints[1].x := x;
                                  PolyLinePoints[1].y := y-size;
                                  PolyLinePoints[2].x := x+size;
                                  PolyLinePoints[2].y := y+size;
                                  PolyLinePoints[3].x := x-size;
                                  PolyLinePoints[3].y := y+Size;
                                  Canvas.Polygon(slice(PolyLinePoints,PointsInPolyLine))
                               end;
                     FilledDownTri,
                     DownTri : begin
                                  PointsInPolyLine := 4;
                                  PolyLinePoints[0].x := x-Size;
                                  PolyLinePoints[0].y := y-Size;
                                  PolyLinePoints[1].x := x;
                                  PolyLinePoints[1].y := y+size;
                                  PolyLinePoints[2].x := x+size;
                                  PolyLinePoints[2].y := y-size;
                                  PolyLinePoints[3].x := x-size;
                                  PolyLinePoints[3].y := y-Size;
                                  Canvas.Polygon(slice(PolyLinePoints,PointsInPolyLine))
                               end;
                  end {case};
                  Canvas.MoveTo(x,y);
            finally
               Canvas.Brush.Style := bsClear;
            end;
         end;


         function WinExecAndWait32(FileName : ANSIstring; Wait : boolean = true; Log : boolean = true) : integer;
         var
           zAppName : array[0..512] of char;
           zCurDir : array[0..255] of char;
           WorkDir : ANSIstring;
           StartupInfo : TStartupInfo;
           ProcessInfo : TProcessInformation;
         begin
            {$IfDef RecordShellExecute} if Log and (not HeavyDutyProcessing) then WriteLineToDebugFile('WinExecAndWait32 in, cmd=' + FileName ); {$EndIf}
            if Wait then ShowHourglassCursor;
            StrPCopy(zAppName,FileName);
            GetDir(0,WorkDir);
            StrPCopy(zCurDir,WorkDir);
            FillChar(StartupInfo,Sizeof(StartupInfo),#0);
            StartupInfo.cb := Sizeof(StartupInfo);
            StartupInfo.dwFlags := STARTF_USESHOWWINDOW;

            if MDDef.ShowWinExec then StartupInfo.wShowWindow := SW_Show
            else StartupInfo.wShowWindow := SW_Hide;

            if not CreateProcess(nil,
                zAppName,                      { pointer to command line ANSIstring }
                nil,                           { pointer to process security attributes }
                nil,                           { pointer to thread security attributes }
                false,                         { handle inheritance flag }
                CREATE_NEW_CONSOLE or          { creation flags }
                NORMAL_PRIORITY_CLASS,
                nil,                           { pointer to new environment block }
                nil,                           { pointer to current directory name }
                StartupInfo,                   { pointer to STARTUPINFO }
                ProcessInfo) then Result := -1 { pointer to PROCESS_INF }
            else begin
                if Wait then WaitforSingleObject(ProcessInfo.hProcess,INFINITE);
                Result := 0;
            end;
            ApplicationProcessMessages;
            if Wait then ShowDefaultCursor;
            {$IfDef RecordShellExecute} if Log and (not HeavyDutyProcessing) then WriteLineToDebugFile('WinExecAndWait32 out, result=' + IntToStr(Result)); {$EndIf}
         end;


         function ExecuteFile(const FileName, Params, DefaultDir : ANSIstring): THandle;
         var
           zFileName, zParams, zDir: array[0..255] of char;
           tDir : ANSIstring;
           Len  : integer;
           ShowCmd : Integer;
         begin
           tDir := DefaultDir;
           Len := length(tDir);
           if (len > 3) and (tDir[len] = '\') then Delete(tDir,len,1);
           {$IfDef RecordShellExecute} if (not HeavyDutyProcessing) then WriteLineToDebugFile('ExecuteFile ' + FileName + '  Parmams=' + Params + '  DefaultDir=' + DefaultDir); {$EndIf}
           if MDDef.ShowWinExec then ShowCmd := SW_Show
           else ShowCmd := SW_Hide;
           Result := ShellAPI.ShellExecute(Application.MainForm.Handle, nil,StrPCopy(zFileName, FileName),StrPCopy(zParams, Params),StrPCopy(zDir, tDir), ShowCmd);
         end;

         var
            ProgressCaption : ANSIstring;

         procedure StartCount(Title : ShortString);
         begin
            if (PetProgF = Nil) then PetProgF := TPetProgF.Create(Application)
            else PetProgF.Visible := true;
            PetProgF.Gauge1.Visible := true;
            PetProgF.Caption := Title;
            ProgressCaption := Title;
            PetProgF.Gauge1.Progress := 0;
            ShowHourglassCursor;
         end;

         procedure UpdateCount(HowFar : LongInt);
         begin
            if WantShowProgress and (PETProgF <> Nil) then begin
               PetProgF.Caption := ProgressCaption + '  ' + IntToStr(HowFar);
               ApplicationProcessMessages;
            end;
         end;

         procedure EndCount;
         begin
            if WantShowProgress and (PETProgF <> Nil) then begin
               PetProgF.Visible := false;
               ShowDefaultCursor;
            end;
         end;

{$EndIf}


procedure InitializePetmar;
var
   i : integer;
begin
   //to avoid problems on international systems
   FormatSettings.DecimalSeparator := '.';
   FormatSettings.ShortDateFormat := 'mm/dd/yy';

   {$IfDef VCL}
      DualMonitor := (Screen.Monitors[0].Width > 3 * Screen.Monitors[0].Height div 2) or (Screen.MonitorCount = 2);
      if (Screen.MonitorCount = 2) then CombinedScreenWidth := Screen.Monitors[0].Width + Screen.Monitors[1].Width
      else CombinedScreenWidth := Screen.Monitors[0].Width;
   {$EndIf}

   {$IfDef MSWindows}
      clNearKMLWhite := RGB(250,250,250);
      clAlmostBlack := RGB(1,1,1);

      {$IfDef VCL}
      for i := 0 to 14 do RGBColorArray[i] := ConvertTColorToPlatformColor(ElevColors[i]);
      ProgramRootDir := ExtractFilePath(ExpandFileName(Forms.Application.ExeName));
      {$EndIf}

      {$IfDef FMX}
      ProgramRootDir := ExtractFilePath(ExpandFileName(ParamStr(0)));
      {$EndIf}
   {$EndIf}

   {$IfDef Android}
      ProgramRootDir := TPath.GetDocumentsPath  + PathDelim;
   {$EndIf}

   {$IfDef iOS}
      ProgramRootDir := TPath.GetDocumentsPath  + PathDelim;
   {$EndIf}

   RGBtripleRed := RGBtrip(255,0,0);
   RGBtripleBlack := RGBtrip(0,0,0);
   RGBtripleAlmostBlack := RGBtrip(1,1,1);
   RGBtripleWhite := RGBtrip(255,255,255);
   RGBtripleNearWhite := RGBtrip(254,254,254);

   ReportMemoryLeaksOnShutdown := (DebugHook <> 0);

   WantOut := false;
   WantShowProgress := true;

   {$IfDef VCL}
      PETMARCommonForm := tPETMARCommonForm.Create(Application);
      PETMARCommonForm.DefaultMonitor := dmMainForm;
   {$EndIf}

   TerrainCuts[0,1] := 2;
   TerrainCuts[0,2] := 241;
   TerrainCuts[0,3] := 45;

   TerrainCuts[1,1] := 102;
   TerrainCuts[1,2] := 148;
   TerrainCuts[1,3] := 97;

   TerrainCuts[2,1] := 219;
   TerrainCuts[2,2] := 190;
   TerrainCuts[2,3] := 26;

   TerrainCuts[3,1] := 243;
   TerrainCuts[3,2] := 165;
   TerrainCuts[3,3] := 1;

   TerrainCuts[4,1] := 201;
   TerrainCuts[4,2] := 48;
   TerrainCuts[4,3] := 44;
end;


{$IfDef VCL}

      procedure StartThreadTimers(Capt : ShortString = ''; MaxThreads : integer = 8; Enable1 : boolean = false);
      var
         i : integer;
      begin
         if (ThreadTimers = Nil) then begin
            {$IfDef ThreadTimerCreation} WriteLineToDebugFile('Creating ThreadTimers'); {$EndIf}
            ThreadTimers := TThreadTimerForm.Create(Application);
         end;
         ThreadTimers.Caption := Capt;
         ThreadTimers.OverallGauge9.Visible := true;
         for i := 1 to MaxThreads do ThreadTimers.EnableGauge(i,false);
         if Enable1 then ThreadTimers.EnableGauge(1, true);
         ThreadTimers.ClientHeight := 70 + MaxThreads * 30;
         ThreadTimers.Gauge1.Visible := MaxThreads > 0;
         ThreadTimers.Gauge2.Visible := MaxThreads > 1;
         ThreadTimers.Gauge3.Visible := MaxThreads > 2;
         ThreadTimers.Gauge4.Visible := MaxThreads > 3;
         ThreadTimers.Gauge5.Visible := MaxThreads > 4;
         ThreadTimers.Gauge6.Visible := MaxThreads > 5;
         ThreadTimers.Gauge7.Visible := MaxThreads > 6;
         ThreadTimers.Gauge8.Visible := MaxThreads > 7;
         ThreadTimers.StatusBar1.Panels[1].Text := 'Start ' + TimeToStr(Now);
         ThreadTimers.Show;
      end;


      procedure EndThreadTimers;
      begin
         if (ThreadTimers <> Nil) then begin
            ThreadTimers.Close;
            ThreadTimers := Nil;
            {$IfDef ThreadTimerCreation} WriteLineToDebugFile('Destroying ThreadTimers'); {$EndIf}
            ApplicationProcessMessages;
         end;
      end;

      procedure StartSingleThreadTimer(Capt : string35 = '');
      begin
         StartThreadTimers(Capt,1,true);
      end;

{$EndIf}


procedure FixFileNameBackslashes(var fName : PathStr);
begin
   {$IfDef MSWindows}
   {$Else}
      ReplaceCharacter(fName,'\','/');
   {$EndIf}
end;

function RemoveUnderScores(LegendStr : ANSIString) : ANSIString;
begin
   if (LegendStr <> '') then begin
      Result := LegendStr;
      ReplaceCharacter(Result,'_',' ');
   end;
end;

function SpacesToUnderScores(LegendStr : ANSIString) : ANSIString;
begin
   if (LegendStr <> '') then begin
      Result := LegendStr;
      ReplaceCharacter(Result,' ','_');
   end;
end;


function DownloadFileFromWeb(WebName,LocalName : ANSIstring; ShowFailure : boolean = true) : boolean;
begin
   DeleteFileIfExists(LocalName);
   {$IfDef RecordWebDownloads} WriteLineToDebugFile('DownloadFileFromWeb ' + WebName + ' to ' + LocalName); {$EndIf}
   {$IfDef VCL} wmdem.SetPanelText(0,'Download ' + ExtractFileName(LocalName)); {$EndIf}
   ShowHourglassCursor;
   Result := URLMon.UrlDownloadToFileA(nil, PANSIChar(WebName),PANSIChar(LocalName), URLOSTRM_GETNEWESTVERSION, Nil) = 0;
   {$IfDef VCL} wmdem.SetPanelText(0,''); {$EndIf}
   ShowDefaultCursor;
   {$IfDef RecordWebDownloads}
      if Result then WriteLineToDebugFile('DownloadFileFromWeb success out')
      else begin
         WriteLineToDebugFile('DownloadFileFromWeb failure, ' + WebName);
         if ShowFailure then MessageToContinue('DownloadFileFromWeb failure, ' + WebName);
      end;
   {$EndIf}
end;


function DoubleQuotedStr(const S: ANSIstring): ANSIstring;
begin
   Result := '"' + s + '"';
end;


procedure ApplicationProcessMessages;
begin
   {$IfDEF VCL}
      try
         Application.ProcessMessages;
      except
         on Exception do begin end;
      end;
   {$EndIf}
end;


procedure ShowHourglassCursor;
begin
   {$IfDEF VCL}
      try
         Forms.Screen.Cursor := crHourglass;
      except
         on Exception do begin end;
      end;
   {$EndIf}
end;

procedure ShowDefaultCursor;
begin
   {$IfDef VCL}
      if not HeavyDutyProcessing then begin
        try
           Forms.Screen.Cursor := crDefault;
        except
           on Exception do begin end;
        end;
      end;
   {$EndIf}
end;


procedure RandomizeStringList(var TheList : tStringList; SpareFirst : boolean = true);
var
   TStr : AnsiString;
   i,j : integer;
begin
    {$IfDef RecordRandomize} WriteLineToDebugFile('RandomizeStringList in');  WriteStringListToDebugFile(TheList); {$EndIf}
    Randomize;
    TheList.Sorted := false;
    for I := 1 to 5 * (TheList.Count) do begin
        j := round(Random * TheList.Count);
        if (j >= 0) and (j < TheList.Count) then begin
           TStr := TheList.Strings[j];
           TheList.Delete(j);
           TheList.Add(TStr);
        end;
    end;
    {$IfDef RecordRandomize} WriteLineToDebugFile('RandomizeStringList out'); WriteStringListToDebugFile(TheList); {$EndIf}
end;

function ExtEquals(Ext,Wanted : ExtStr) : boolean;
begin
   Result := UpperCase(ext) = UpperCase(Wanted);
end;


function FileExtEquals(fName : PathStr; Wanted : ExtStr) : boolean;
begin
   Result := UpperCase(ExtractFileExt(fName)) = UpperCase(Wanted);
end;


{$IfDef ExCompress}
{$Else}

         function SevenZipPresent : boolean;
         begin
            SevenZipfName := ProgramRootDir + '7z.exe';
            Result := FileExists(SevenZipfName) and FileExists(ProgramRootDir + '7z.dll');
         end;

         procedure Missing7ZipMessage;
         begin
            MessageToContinue('Missing ' + SevenZipfName + ' and ' + ProgramRootDir + '7z.dll');
         end;


         function ZipMasterUnzip(ZipName,OutDir : ShortString) : boolean;
         var
           Zipfile : TZipFile;
         begin
            wmDEM.SetPanelText(0,'unzip ' + ExtractFileName(zipName));
            ShowHourglassCursor;
            if FileExists(zipName) then begin
               ZipFile := TZipFile.Create;
               try
                 ZipFile.ExtractZipFile(ZipName,OutDir);
               finally
                 ZipFile.Free;
               end;
            end
            else Result := false;
            wmDEM.SetPanelText(0,'');
            ShowDefaultCursor;
         end;


         procedure UnzipSingleFile(var fName : PathStr);
         var
            Dir : DirStr;
            Ext : ExtStr;
            fList : tstringList;
         begin
            Ext := UpperCase(ExtractFileExt(FName));
            if (Ext = '.KMZ') or (Ext = '.LPK') or (Ext = '.SHZ') then begin
               Dir := ExtractFilePath(fName) + SpacesToUnderScores(ExtractFileNameNoExt(fName)) + '\';
               SafeMakeDir(Dir);
               ZipMasterUnzip(fName,Dir);
               if (Ext = '.SHZ') then begin
                  Flist := Nil;
                  Petmar.FindMatchingFiles(Dir,'*.shp',fList,2);
                  fName := flist[0];
                  fList.Free;
               end;
            end
            else if (Ext = '.7Z') then begin
               ChDir(ExtractFilePath(fName));
               Main7Z(fName);
            end
            else begin
               ZipMasterUnzip(fName,ExtractFilePath(fName));
            end;
         end;


         function ZipMasterZipFiles(ZipName : PathStr; TheFiles : tStringList) : boolean;
         var
           Zipfile : TZipFile;
           i : integer;
         begin
            {$IfDef RecordZip} WriteLineToDebugFile('ZipMasterZipFiles enter'); {$EndIf}
            ZipFile := TZipFile.Create;
            try
               ZipFile.Open(ZipName,zmWrite);
               for i := 0 to pred(TheFiles.Count) do begin
                 if FileExists(TheFiles.Strings[i]) then begin
                    ZipFile.Add(TheFiles.Strings[i]);
                 end;
               end;
            finally
              ZipFile.Close;
              ZipFile.Free;
            end;
         end;


         function Main7Z(var fName : PathStr; Switches : shortstring = '') : boolean;
         var
            cmd : ShortString;
         begin
            if SevenZipPresent then begin
               {$IfDef RecordUnzips} WriteLineToDebugFile('Main72 for ' + fName); {$EndIf}
               cmd := SevenZipfName + ' x ' + fname + Switches;
               WinExecAndWait32(cmd);
            end
            else Missing7ZipMessage;
         end;


         function MainBZ2(var fName : PathStr; Outfile : PathStr = '') : boolean;
         var
            cmd : ShortString;
         begin
            if SevenZipPresent then begin
               {$IfDef RecordUnzips} WriteLineToDebugFile('MainBZ2 for ' + fName); {$EndIf}
               if (Outfile <> '') then ChDir(ExtractFilePath(OutFile))
               else ChDir(ExtractFilePath(fName));
               cmd := SevenZipfName + ' a -tbzip2 ' + fname + '.bz2 ' + fname;
               WinExecAndWait32(cmd);
            end
            else Missing7ZipMessage;
         end;


         function MainGzip(var fName : PathStr; Outfile : PathStr = '') : boolean;
         var
            cmd : ShortString;
         begin
            if SevenZipPresent then begin
               cmd := SevenZipfName + ' e ' + DoubleQuotedStr(fname);
               if (Outfile <> '') then ChDir(ExtractFilePath(OutFile))
               else ChDir(ExtractFilePath(fName));
               WinExecAndWait32(cmd);
               //if MDDef.DeleteTarGZ then File2Trash(fName);
            end
            else Missing7ZipMessage;
         end;


         function MainExtar(fName : PathStr; Outfile : PathStr = '') : boolean;
         var
            cmd : ShortString;
         begin
            if SevenZipPresent then begin
               cmd := SevenZipfName + ' e ' + DoubleQuotedStr(fname);
               if (Outfile <> '') then begin
                  ChDir(ExtractFilePath(OutFile));
                  cmd := cmd + ' -o' + DoubleQuotedStr(Outfile);
               end
               else ChDir(ExtractFilePath(fName));
               WinExecAndWait32(cmd);
               //if MDDef.DeleteTarGZ then File2Trash(fName);
            end
            else Missing7ZipMessage;
         end;

{$EndIf}


procedure Int4Swap(var int : int32);
var
   ch4,tch4 : array[1..4] of byte;
   i        : integer;
begin
   Move(int,ch4,4);
   for i := 1 to 4 do tch4[i] := ch4[5-i];
   Move(tch4,int,4);
end;

procedure Int4Swap(var int : cardinal);
var
   ch4,tch4 : array[1..4] of byte;
   i        : integer;
begin
   Move(int,ch4,4);
   for i := 1 to 4 do tch4[i] := ch4[5-i];
   Move(tch4,int,4);
end;

procedure SwapToShortFloat(var FloatValue : float32);
var
  i : integer;
  Temp : byte;
  Packet : array[0..3] of byte;
begin
  Move(FloatValue,Packet,4);
  for i := 0 to 1 do begin
     Temp := Packet[i];
     Packet[i] := Packet[3 - i];
     Packet[3 - i] := Temp;
  end;
  Move(Packet,FloatValue,4);
end;


procedure SwapToFloat8byte(var FloatValue : Float64);
var
  i : integer;
  Temp : byte;
  Packet : array[0..7] of byte;
begin
  Move(FloatValue,Packet,8);
  for i := 0 to 3 do begin
     Temp := Packet[i];
     Packet[i] := Packet[7 - i];
     Packet[7 - i] := Temp;
  end;
  Move(Packet,FloatValue,8);
end;


function GetParentDirectory(path : Ansistring) : Ansistring;
begin
  result := ExpandFileName(path + '\..');
  if Result[length(Result)] <> '\' then Result := Result +  '\';
end;


function LastSubDir(Dir : AnsiString) : PathStr;
begin
   Result := Dir;
   if length(Dir) > 0 then begin
      if Dir[Length(Dir)] <> '\' then Dir := Dir + '\';
      while StrUtils.AnsiContainsText(Dir,'\') do Result := BeforeSpecifiedCharacterANSI(Dir,'\',true,true);
   end;
end;


function ArrayOfCharToString(LenArray : integer; var Chars : array of ANSIchar) : shortstring;
var
   I : integer;
begin
   Result := '';
   for i := 0 to pred(lenArray) do if (Chars[i] = #0) then exit
   else Result := Result + Chars[i];
end;


function GetFileSize(FileName : String): Int64;
//https://engineertips.wordpress.com/2018/07/12/filesize-function-for-delphi/
var
  sr : TSearchRec;
begin
  if FindFirst(Filename, faAnyFile, sr ) = 0 then begin
     Result := Sr.Size;
  end
  else begin
     result := -1;
  end;
end;


function GetDOSPath(TheMessage : AnsiString; var Path : PathStr) : boolean;
begin
   {$IfDef VCL}
      with TFileOpenDialog.Create(nil) do
        try
          Title := 'Folder/directory for ' + TheMessage;
          Options := [fdoPickFolders];
          FileName := LastSubDir(Path);
          DefaultFolder := GetParentDirectory(Path);
          if Execute then begin
             Path := FileName;
             if Path[length(Path)] <> '\' then Path := FileName + '\';
          end
          else Path := '';
          Result := Path <> '';
        finally
          Free;
        end;
   {$EndIf}
end;


function GetMultipleDirectories(TheMessage : AnsiString; var Paths : tStringList) : boolean;
var
   fod : TFileOpenDialog;
   i : integer;
begin
   {$IfDef VCL}
      if Paths.Count = 0 then Paths.Add(MainMapData);

      fod := TFileOpenDialog.Create(nil);
      try
         fod.Title := 'Folders/directories for ' + TheMessage;
         fod.Options := [fdoPickFolders,fdoAllowMultiSelect];
         fod.FileName := LastSubDir(Paths[0]);
         fod.DefaultFolder := GetParentDirectory(Paths[0]);
         Paths.Clear;
         if fod.Execute then begin
            for i := 0 to pred(fod.Files.Count) do begin
               Paths.Add(fod.Files[i] + '\');
            end;
         end;
         Result := (Paths.Count > 0);
     finally
       fod.Free;
     end;
   {$EndIf}
end;


procedure StripInvalidPathNameChars(var fName : PathStr);
var
   i : integer;
begin
   for i := length(fName) downto 1 do
      if not (fName[i] in ValidDOSFileNameChars) then System.Delete(fName,i,1);
   for i := length(fName) downto 1 do
      if (fName[i] in [':','/']) then System.Delete(fName,i,1);
end;


procedure DeleteFileIfExists(fName : PathStr);
begin
   {$IfDef TrackFileDeletion} WriteLineToDebugFile('Try to delete ' + fName);  MessageToContinue('Try to delete ' + fName); {$EndIf}

   if (fName <> '') then begin
      fName := ExpandFileName(fName);
      if FileExists(fName) then begin
         if (UpperCase(ExtractFilePath(fName)) = 'C:\') then begin
            {$IfDef Record} MessageToContinue('DeleteFileIfExists attempt to delete ' + fname + '; submit bug report with debug log'); {$EndIf}
         end
         else begin
            InsureFileIsNotReadOnly(fName);
            SysUtils.DeleteFile(fName);
         end;
      end;
   end;
end;


procedure CleanFloatString(var Text : String);
var
   i : integer;
begin
   if Text <> '' then begin
     if Text[1] = '.' then Text := '0' + Text;
     for i := 1 to Length(Text) do if (Text[i] = FormatSettings.DecimalSeparator) then Text[i] := '.';
   end;
end;


function CheckEditString(Text : String; var OutValue : float64) : boolean;
var
   f : float64;
   err : integer;
begin
   CheckEditString := false;
   if Text <> '' then begin
     CleanFloatString(Text);
     val(ptTrim(text),f,err);
     if (err = 0) then begin
        CheckEditString := true;
        OutValue := f;
     end;
   end;
end;


function CheckEditString(Text : String; var OutValue : float32): boolean;
var
   f : float64;
   err : integer;
begin
   CheckEditString := false;
   if Text <> '' then begin
     CleanFloatString(Text);
     val(ptTrim(text),f,err);
     if (err = 0) then begin
        CheckEditString := true;
        OutValue := f;
     end;
   end
end;


function CheckEditString(Text : String; var OutValue : int16): boolean;
var
   f,err : integer;
begin
   CheckEditString := false;
   if Text <> '' then begin
      val(ptTrim(text),f,err);
      if (err = 0) then begin
         CheckEditString := true;
         OutValue := f;
      end;
   end;
end;


function CheckEditString(Text : String; var OutValue : int32): boolean;
var
   f,err : integer;
begin
   CheckEditString := false;
   if Text <> '' then begin
      val(ptTrim(text),f,err);
      if (err = 0) then begin
         CheckEditString := true;
         OutValue := f;
      end;
   end;
end;

function CheckEditString(Text : String; var OutValue : int64): boolean;
var
   f,err : integer;
begin
   CheckEditString := false;
   if Text <> '' then begin
      val(ptTrim(text),f,err);
      if (err = 0) then begin
         CheckEditString := true;
         OutValue := f;
      end;
   end;
end;


function CheckEditString(Text : String; var OutValue : Word): boolean;
var
   f,err : integer;
begin
   CheckEditString := false;
   if Text <> '' then begin
      val(ptTrim(text),f,err);
      if (err = 0) then begin
         CheckEditString := true;
         OutValue := f;
      end;
   end;
end;


function CheckEditString(Text : String; var OutValue : Byte): boolean;
var
   f : byte;
   err : integer;
begin
   CheckEditString := false;
   if Text <> '' then begin
     val(ptTrim(text),f,err);
     if (err = 0) then begin
        CheckEditString := true;
        OutValue := f;
     end;
   end;
end;


procedure ReadDefault(Prompt : ShortString; var IntVal : byte);
var
   Error : integer;
begin
   {$IfDef VCL}
      with PETMARCommonForm do repeat
         Caption := 'Input byte (0-255)';
         Panel1.Caption := Prompt;
         Edit1.Text := IntToStr(IntVal);
         PETMARCommonForm.FormActivate(Nil);
         ShowModal;
         Val(Edit1.Text,IntVal,Error);
      until (Error = 0);
   {$EndIf}
end;


procedure ReadDefault(Prompt : ShortString; var IntVal : int16);
var
   Error : integer;
begin
   {$IfDef VCL}
      with PETMARCommonForm do repeat
         Caption := 'Input Integer';
         Panel1.Caption := Prompt;
         Edit1.Text := IntToStr(IntVal);
         PETMARCommonForm.FormActivate(Nil);
         ShowModal;
         Val(Edit1.Text,IntVal,Error);
      until (Error = 0);
   {$EndIf}
end;


procedure ReadDefault(Prompt : ShortString; var IntVal : int32);
var
   Error : integer;
begin
   {$IfDef VCL}
      with PETMARCommonForm do repeat
         Caption := 'Input Integer';
         Panel1.Caption := Prompt;
         Edit1.Text := IntToStr(IntVal);
         PETMARCommonForm.FormActivate(Nil);
         ShowModal;
         Val(Edit1.Text,IntVal,Error);
      until (Error = 0);
   {$EndIf}
end;


procedure ReadDefault(Prompt : ShortString; var IntVal : int64);
var
   Error : integer;
begin
   {$IfDef VCL}
      with PETMARCommonForm do repeat
         Caption := 'Input Integer';
         Panel1.Caption := Prompt;
         Edit1.Text := IntToStr(IntVal);
         PETMARCommonForm.FormActivate(Nil);
         ShowModal;
         Val(Edit1.Text,IntVal,Error);
      until (Error = 0);
   {$EndIf}
end;


procedure ReadDefault(Prompt : ShortString;  var RealVal : float64);
begin
   {$IfDef VCL}
      with PETMARCommonForm do repeat
         Caption := 'Input Real Number';
         Panel1.Caption := Prompt;
         Edit1.Text := RealToString(RealVal,-18,-8);
         PETMARCommonForm.FormActivate(Nil);
         ShowModal;
      until CheckEditString(Edit1.Text,RealVal);
   {$EndIf}
end;


procedure ReadDefault(Prompt : ShortString;  var RealVal : float32);
var
   fl : float64;
begin
   fl := RealVal;
   ReadDefault(Prompt,fl);
   RealVal := fl;
end;

procedure ReadDefault(Prompt : ShortString;  var WordVal : word);
var
   fl : int64;
begin
   fl := WordVal;
   ReadDefault(Prompt,fl);
   WordVal := fl;
end;


function GetFileFromDirectory(InMessage : ANSIstring; Mask : ANSIstring; var FileWanted : ANSIString) : boolean;
var
   Masks : ANSIstring;
   i     : byte;
begin
   {$IfDef VCL}
      i := 1;
      if StrUtils.AnsiContainsText(Mask,'|') then Masks := Mask
      else Masks := 'Specified files(' + Mask + ')|' + Mask + '|' + ' All files (*.*)|*.*';
      Result := GetFileMultipleMask(InMessage,Masks,FileWanted,i);
   {$EndIf}
end;



function ColorFromZColorTable(ZColorTable : tColorTableDefinitions; zj : float64; var k : integer) : tRGBTriple;
var
   i : integer;
begin
   {$IfDef VCL}
      with ZColorTable do begin
         if (ZTableEntries > 256) then begin
            for i := 1 to ZTableEntries do begin
               if (zj <= zBigTableValue^[i]) or (i = ZTableEntries) then begin
                  Result := zBigTableColors^[i];
                  k := i;
                  exit;
               end;
            end;
         end
         else begin
            for i := 1 to ZTableEntries do begin
               {$IfDef RecordColorFromZColorTable} WriteLineToDebugFile('   ' + IntToStr(i) + '  ' + RealToString(zTableValue[i],-18,-4)); {$EndIf}
               if (zj <= zTableValue[i]) or (i = ZTableEntries) then begin
                  Result := zTableColors[i];
                  k := i;
                  exit;
               end;
            end;
         end;
      end;
   {$EndIf}
end;


function FindFieldInStringList(Header : tStringList; FieldName : ANSIstring; SepChar : ANSIchar) : ANSIstring;
var
   tStr : ANSIstring;
   i : integer;
begin
   Result := '0';
   for i := 0 to pred(Header.Count) do begin
      tStr := ptTrim(Header.Strings[i]);
      if (TStr <> '') and (BeforeSpecifiedCharacterANSI(tStr,SepChar,true,true) = FieldName) then begin
         Result := ptTrim(tStr);
         exit;
      end;
   end;
end;

function FindIntegerFieldInStringList(Header : tStringList; FieldName : ANSIstring; SepChar : ANSIchar) : Integer;
begin
   Result := StrToInt(FindFieldInStringList(Header,FieldName,SepChar));
end;

function FindFloatFieldInStringList(Header : tStringList; FieldName : ANSIstring; SepChar : ANSIchar) : float64;
begin
   Result := StrToFloat(FindFieldInStringList(Header,FieldName,SepChar));
end;


function RGBString(r,g,b : SmallInt; IncludeGray : boolean = false) : ShortString;
begin
   Result := 'R=' + IntToStr(r) + '  G=' + IntToStr(g) + '  B=' + IntToStr(B);
   if IncludeGray then Result := Result +  '  gray=' + IntToStr(RGBtoGrayscale(r,g,b));
end;


function ColorString(Color : tColor) : shortString;
var
   r,g,b : byte;
begin
   GetRGBfromTColor(Color,r,g,b);
   Result := RGBString(r,g,b);
end;

function ColorStringFromPlatformColor(Color : tPlatformColor) : shortString;
begin
   {$IfDef VCL}
      Result := RGBString(Color.rgbtRed, Color.rgbtGreen, Color.rgbtBlue);
   {$EndIf}
end;


function SelectedColorSchemeColorFunct(ColorScheme: tLegendColors; ColorTable : tColorTableDefinitions; z : float64; Min,Max : float64) : TColor;
var
   k : integer;
begin
   {$IfDef VCL}
      case ColorScheme of
         LegGrays      : Result := GrayColorFunct(z,Min,Max);
         LegRainbows   : Result := RainbowColorFunct(z,Min,Max);
         LegTerrain    : Result := TerrainTColor(z,Min,Max);
         LegSpectrum   : Result := SpectrumColorFunct(z,Min,Max);
         LegChloropleth : Result := ConvertPlatformColorToTColor(ColorFromZColorTable(ColorTable,z,k));
         else begin end;
      end;
   {$EndIf}
end;


procedure GetSeparationCharacter(MenuStr : ANSIstring; var SepChar : ANSIchar);

         procedure SeeIfItsThis(ch :Ansichar);
         var
            j : integer;
         begin
            if (SepChar = ' ') then begin
               for j := 1 to length(MenuStr) do if MenuStr[j] = ch then SepChar := ch;
            end;
         end;

begin
   SepChar := ' ';
   SeeIfItsThis(#9);
   SeeIfItsThis(',');
   SeeIfItsThis(';');
   SeeIfItsThis('|')
end;


procedure GetSeparationCharacterUnicode(MenuStr : string; var SepChar : char);

         procedure SeeIfItsThis(ch : char);
         var
            j : integer;
         begin
            if (SepChar = ' ') then begin
               for j := 1 to length(MenuStr) do if MenuStr[j] = ch then SepChar := ch;
            end;
         end;

begin
   SepChar := ' ';
   SeeIfItsThis(#9);
   SeeIfItsThis(',');
   SeeIfItsThis(';');
   SeeIfItsThis('|')
end;


function NoCommas(TheString: ShortString) : shortstring;
var
   i : integer;
begin
   Result := TheString;
   for i := 1 to length(Result) do if Result[i] = ',' then Result[i] := ' ';
end;

function FormatString(TheString : shortString; Len : integer; Justify : tJustify) : shortString;
begin
   if Length(TheString) < len then begin
      Result := TheString;
      if Justify = LeftJustify then begin
         while Length(Result) < len do Result := Result + ' ';
      end
      else begin
         while Length(Result) < len do Result := ' ' + Result;
      end;
   end
   else Result := Copy(TheString,1,Len);
end;


function HoursMinutesString(Time : float64; WithSec : boolean = false) : ShortString;
var
   Hour,Min,Sec : integer;
   Minutes : float64;
   TStr : string3;
begin
   if (Time < 0) then Time := Time + 24;
   if (Time > 24) then Time := Time - 24;
   if (Time >= 0) and (Time <= 24) then begin
      Hour := Trunc(Time);
      if WithSec then begin
         Minutes := (Time-Hour) * 60;
         Min := trunc(Minutes);
         Sec := round(Frac(Minutes) * 60.0);
         TStr := ':' + AddDayMonthLeadingZero(Sec);
      end
      else begin
         Min := round(Frac(Time) * 60.0);
         if (Min = 60) then begin
            inc(Hour);
            Min := 0;
         end;
         TStr := '';
      end;
      Result := IntegerTostring(Hour,2) + ':' + AddDayMonthLeadingZero(Min) + TStr;
   end
   else begin
      Result := 'xx:xx';
   end;
end;


function ReadFilter(var Filter : FilterType; var FilterSize,FilterLap,Sum : integer; var FilterName : PathStr; DisplayFilter : boolean = true) : boolean;
var
   FilterFile : text;
   i,j      : integer;
begin
   ReadFilter := false;
   if (FilterName = '') then begin
      FilterName := ProgramRootDir + 'filters\';
      if not PathIsValid(FilterName)  then FilterName := ProgramRootDir;
      {$IfDef VCL}
         if not GetFileFromDirectory('Filter','*.FIL',FilterName) then exit;
         if DisplayFilter then QuickOpenEditWindow(FilterName,'Filter ' + ExtractFileName(FilterName));
      {$EndIf}
   end;

   if FileExists(FilterName) then begin
      assignFile(FilterFile,FilterName);
      reset(FilterFile);
      sum := 0;
      FilterSize := 0;
      FillChar(Filter,SizeOf(Filter),0);
      while not EOln(FilterFile) do begin
         inc(FilterSize);
         read(FilterFile,Filter[1,FilterSize]);
         sum := sum + Filter[1,FilterSize];
         if (FilterSize > MaxFilterSize) then begin
            FilterSize := MaxFilterSize;
            MessageToContinue('Check filter file; too many lines');
            exit;
         end;
      end;
      FilterLap := FilterSize div 2;
      for i := 2 to FilterSize do
         for j := 1 to FilterSize do begin
            read(FilterFile,Filter[i,j]);
            sum := sum + Filter[i,j];
         end;
      close(FilterFile);
      ReadFilter := true;
   end;
end;


function CreateTimeString(Hour,Minute,Second : integer) : shortString;
var
   i : integer;
begin
   Result := IntToStr(Hour) + ':' + IntegerToString(Minute,2) + ':' + IntegerToString(Second,2);
   for i  := 1 to length(Result) do if Result[i] = ' ' then Result[i] := '0';
end;


function SmartNumberPoints(NumPoints : int64) : shortstring;
begin
   if (NumPoints < 1000000) then Result := IntToStr(NumPoints)
   else if (NumPoints < 1000000000) then Result := RealToString(0.000001 * NumPoints,-12,-2) + ' million'
   else Result := RealToString(0.000000001 * NumPoints,-12,-2) + ' billion';
end;


{$IfDef MICRODEM}
   function SmartDistanceMetersFormat(d : float64) : shortString;
   begin
      if MDDef.EnglishDistanceUnits = disEnglish then begin
         Result := RealToString((0.001*d * 0.62137),-12,-2) + ' miles';
      end
      else if MDDef.EnglishDistanceUnits = disNautical then begin
         Result := RealToString((0.001*d * 0.539956803),-12,-2) + ' nm';
      end
      else begin
         if d < 100 then Result := RealToString(d,-12,2) + ' m'
         else if d < 1500 then Result := RealToString(d,-12,1) + ' m'
         else if d < 2500 then Result := RealToString(d,-12,0) + ' m'
         else if d < 10000 then Result := RealToString(d,-12,2) + ' m'
         else if d < 25000 then Result := RealToString(0.001*d,-12,2) + ' km'
         else if d < 250000 then Result := RealToString(0.001*d,-12,1) + ' km'
         else Result := RealToString(0.001*d,-12,0) + ' km';
      end;
   end;


   function SmartVolumeFormat(d : float64; DistanceUnits : tDistanceUnits = disMetric) : shortString;
   begin
      if DistanceUnits in [disNautical,disEnglish] then Result := ptTrim(FloatToStrF(d  * 0.7646,ffnumber,18,2)) + ' yd³'
      else begin
         if d < 1000000 then Result := ptTrim(FloatToStrF(d,ffnumber,18,2)) + ' m³'
         else Result := ptTrim(FloatToStrF(d/1000/1000/1000,ffnumber,18,3)) + ' km³';
      end;
   end;


   function SmartAreaFormat(d : float64) : shortString;
   var
       MilesString,AcresString : shortstring;
       Acres, Miles : float64;
       Dec : integer;
   begin
      if MDDef.EnglishDistanceUnits in [disNautical] then begin
         Miles := (0.000001 * d  * 0.29155335);
         if (Miles < 2) then Dec := 4
         else if (Miles < 10) then Dec := 2
         else if Miles < 1000 then Dec := 1
         else Dec := 0;
         Result := RealToString(Miles,-12,Dec) + ' nm²';
      end
      else if MDDef.EnglishDistanceUnits in [disEnglish] then begin
         Miles := (0.000001 * d / 2.590);
         Acres := (d / 4047);
         if (Miles < 2) then Dec := 4
         else if (Miles < 10) then Dec := 2
         else if Miles < 1000 then Dec := 1
         else Dec := 0;
         MilesString := RealToString(Miles,-12,Dec) + ' miles²';
         if (Acres < 10) then Dec := 2
         else if (Acres < 100) then Dec := 1
         else Dec := 0;
         AcresString := RealToString(Acres,-12,-Dec) + ' acres';
         Result := AcresString + '  (' + MilesString + ')';
      end
      else begin
         if (d < 100) then Result := RealToString(d,-12,2) + ' m²'
         else if (d < 1500) then Result := RealToString(d,-12,1) + ' m²'
         else if (d < 100000) then Result := RealToString(d,-12,0) + ' m²'
         else if (d < 1000000) then Result := RealToString(0.000001*d,-12,4) + ' km²'
         else if (d < 10000000) then Result := RealToString(0.000001*d,-12,3) + ' km²'
         else if (d < 100000000) then Result := RealToString(0.000001*d,-12,2) + ' km²'
         else if (d < 1000000000) then Result := RealToString(0.000001*d,-12,1) + ' km²'
         else Result := RealToString(0.000001*d,-12,0) + ' km²';
      end;
      Result := ' ' + Result;
   end;

{$EndIf}


function AngleFormat(Angle : float64; Units : tAngleMeasure; ShowUnits : boolean = true) : shortstring;
begin
   case Units of
      amDegree : Result := RealToString(Angle,-18,-8) + DegSym;
      amMinute : Result := RealToString(Angle*60,-18,-5) + '''';
      amSecond : Result := RealToString(Angle*3600,-18,-2) + '"';
   end;
   if Not ShowUnits then Delete(Result,Length(Result),1);
end;


procedure ConvertToDegMinSecString(Value : float64; OutPutMethod : tLatLongMethod; var DegString,MinString,SecString : string10; HighAccuracy : boolean = true);
var
   DegVal,
   DegDec,MinDec,DecSec : integer;
   SecValue, MinValue : float64;
begin
   if HighAccuracy then begin
      {$IfDef Android}
         DegDec := 5;
         MinDec := 2;
         DecSec := 1;
      {$Else}
         DegDec := 8;
         MinDec := 3;
         DecSec := 2;
      {$EndIf}
   end
   else begin
      DegDec := 2;
      MinDec := 1;
   end;
   MinString := '';
   SecString := '';
   Value := abs(Value);
   case OutPutMethod of
      NearestDegree : Str(round(Value):3,DegString);
      LongDegrees : DegString := RealToString(Value,-15,-10);
      DecDegrees : DegString := RealToString(Value,-15,-DegDec);
      VeryShortDegrees : DegString := RealToString(Value,-10,-2);
      ShortDegrees : DegString := RealToString(Value,-10,-5);

      NearestMinute,
      NearestSecond,
      DecMinutes,
      DecSeconds : DegVal := trunc(Value);
   end;
   if (OutPutMethod in [DecMinutes,NearestMinute,NearestSecond,DecSeconds]) then begin
      MinValue := (Value - DegVal) * 60;
      SecValue := (MinValue - trunc(MinValue)) * 60;
      MinValue := trunc(MinValue);

      if (OutPutMethod in [DecMinutes,NearestMinute]) then begin
         if (OutPutMethod in [NearestMinute]) then MinDec := 0;
         MinString := RealToString(MinValue,-6,-MinDec);
         if (MinString[1] = '6') then begin
            MinString := '0';
            inc(DegVal);
         end;
         DegString := IntToStr(DegVal);
      end;

      if (OutPutMethod in [NearestSecond,DecSeconds]) then begin
         if (OutPutMethod in [NearestSecond]) then DecSec := 0;
         SecString := RealToString(SecValue,-6,-DecSec);
         if (SecString[1] = '6') then begin
            SecString := '0';
            MinValue := MinValue + 1;
         end;
         MinString := RealToString(MinValue,-6,-MinDec);
         if (MinString[1] = '6') then begin
            MinString := '0';
            inc(DegVal);
         end;
         DegString := IntToStr(DegVal);
      end;
   end;
end;


function ConvertToDegreesString(Value : float64; OutPutMethod : tLatLongMethod; HighAccuracy : boolean = true) : shortstring;
var
   DegString,MinString,SecString : string10;
begin
   ConvertToDegMinSecString(Value,OutPutMethod,DegString,MinString,SecString,HighAccuracy);
   Result := ptTrim(DegString) + DegSym;
   if (Value < 0) then Result := '-' + Result;
   if not (OutPutMethod in [DecDegrees,NearestDegree,ShortDegrees,VeryShortDegrees,LongDegrees]) then begin
      Result := Result + MinString + chr(39);
      if (OutPutMethod in [NearestSecond,DecSeconds]) and (SecString <> '') then begin
         Result := Result + SecString + '"';
      end;
   end;
end;

function LatToString3(Lat : float64) : shortstring;
begin
   Result := LatToString(Lat,ShortDegrees);
   StripCharacter(Result,DegSym);
   if length(Result) = 2 then Result := Result[1] + '0' + Result[2];
end;

function LongToString4(Long : float64) : shortstring;
begin
   LongitudeAngleInRange(Long);
   Result := LongToString(Long,ShortDegrees);
   StripCharacter(Result,DegSym);
   if length(Result) = 2 then Result := Result[1] + '00' + Result[2];
   if length(Result) = 3 then Result := Result[1] + '0' + Result[2] + Result[3];
end;

function LatLongToStringForFileName(Lat,Long : float64) : shortstring;
begin
  Result := LatToString3(Lat) + LongToString4(Long);
end;

function LatLongDegreeToString(Lat,Long : float64; OutPutLatLongMethod : tLatLongMethod = DecDegrees) : shortstring;
begin
   LongitudeAngleInRange(Long);
   LatLongDegreeToString := LatToString(Lat,OutPutLatLongMethod) + ' ' + LongToString(Long,OutPutLatLongMethod);
end;


function LatLongRadToString(Lat,Long : float64; OutPutLatLongMethod : tLatLongMethod = DecDegrees) : shortstring;
begin
   Result := LatLongDegreeToString(Lat/DegToRad,Long/DegToRad,OutPutLatLongMethod);
end;


function LatToString(Lat : float64; OutPutLatLongMethod : tLatLongMethod) : shortstring;
var
   LatHemi : ANSIchar;
begin
   if (Lat >= 0) then LatHemi := 'N' else LatHemi := 'S';
   LatToString := LatHemi + ConvertToDegreesString(abs(Lat),OutPutLatLongMethod);
end {funct LatLongToString};


function LongToString(Long : float64;  OutPutLatLongMethod : tLatLongMethod) : shortstring;
var
   LongHemi : ANSIchar;
begin
   LongHemi := 'E';
   LongitudeAngleInRange(Long);
    if MDdef.UseLongsto360 then begin
       while (Long < 0) do Long := Long + 360;
    end;
   if (Long < 0) then LongHemi := 'W';
   LongToString := LongHemi + ConvertToDegreesString(abs(Long),OutPutLatLongMethod);
end {funct LatLongToString};


function GetFloatFromRequest(InString : ANSIstring; WantField : shortstring) : float64;
var
   TStr : shortstring;
begin
   TStr := GetFromRequest(InString,WantField);
   if TStr = '' then Result := 0
   else Result := StrToFloat(TStr);
end;

function GetIntegerFromRequest(InString : ANSIstring; WantField : shortstring) : integer;
var
   TStr : shortstring;
begin
   TStr := GetFromRequest(InString,WantField);
   if (TStr = '') then Result := 0
   else Result := StrToInt(TStr);
end;

function GetFromRequest(InString : ANSIstring; WantField : shortstring) : shortstring;
var
  Field: ANSIstring;
begin
   repeat
     Field := BeforeSpecifiedCharacterANSI(InString,'=',true,true);
     Result := BeforeSpecifiedCharacterANSI(InString,'&',true,true);
     if (Field = WantField) then exit;
  until (length(InString) = 0);
  Result := '';
end;


procedure SortAndRemoveDuplicates(var StringList : tStringList);
var
   i : integer;
begin
   StringList.Sort;
   for i := pred(StringList.Count) downto 1 do
      if StringList.Strings[i] = StringList.Strings[pred(i)] then
         StringList.Delete(i);
end;


function CurrentTimeForFileName : PathStr;
var
  i : integer;
begin
   Result := DateToStr(now) + '_' + CurrentMilitaryTime(true);
   for i := 1 to length(Result) do if Result[i] in ['/',':'] then Result[i] := '_';
end;


const
  ProgressTop  : integer = -99;
  ProgressLeft : integer = -99;

procedure StartProgress(Title : ShortString);
begin
   {$IfDef VCL}
      WantOut := false;
      if WantShowProgress then begin
         if (PetProgF = Nil) then PetProgF := TPetProgF.Create(Application) ;
         PetProgF.Visible := true;
         PetProgF.Caption := Title;
         PetProgF.Gauge1.Progress := 0;
         PetProgF.Gauge1.Visible := true;
         PetProgF.BitBtn1.Visible := false;
         {$IfDef ShowProgressBarSize} WriteLineToDebugFile('Start progress, form height=' + IntToStr(PetProgF.Height ));   {$EndIf}
         if (ProgressTop >= 0) then begin
            PetProgF.Top := ProgressTop;
            PetProgF.Left := ProgressLeft;
         end;
         if PetProgF.NeedToCheckPlacement then CheckFormPlacement(PetProgF);
      end;
      ShowHourglassCursor;
   {$EndIf}
end;

procedure StartProgressAbortOption(Title : ShortString);
begin
   {$IfDef VCL}
      if WantShowProgress then begin
         StartProgress(Title);
         PetProgF.SetUpForAborting;
      end;
   {$EndIf}
end;


procedure UpdateProgressBar(HowFar : float64);
begin
   {$IfDef VCL}
      if WantShowProgress and (PETProgF <> Nil) and (not Math.IsNAN(HowFar)) and (not Math.IsInfinite(HowFar)) then begin
         ApplicationProcessMessages;
         PetProgF.Gauge1.Progress := round(100.0 * HowFar);
         if HeavyDutyProcessing then begin
            PetProgF.Top := 10;
            PetProgF.Left := 10;
         end;
         if WantOut then begin
            EndProgress;
         end;
      end;
   {$EndIf}
end;


procedure EndProgress;
begin
   {$IfDef VCL}
      {$IfDef ShowProgressBarSize} WriteLineToDebugFile('End Progress, form height=' + IntToStr(PetProgF.Height )); {$EndIf}
      if WantShowProgress and (PETProgF <> Nil) then begin
         PetProgF.Visible := false;
         ProgressTop := PetProgF.Top;
         ProgressLeft := PetProgF.Left;
      end;
      ShowDefaultCursor;
   {$EndIf}
end;


function SpectrumRGBFunct(z,Min,Max : float64) : tPlatformColor;
var
   r,g,b : byte;
begin
   if z < Min then z := Min;
   if z > Max then z := Max;
   z := 380 + 400 * (z - Min) / (Max - Min);
   WavelengthToRGB(z,r,g,b);
   Result := RGBtrip(r,g,b);
end;


procedure CopyStringList(FromList : tStringList; var ToList : tStringList);
var
   i : integer;
begin
   for i := 0 to pred(FromList.Count) do ToList.Add(FromList.Strings[i]);
end;


function NextFileNumber(Path,fName : PathStr; Ext : ExtStr) : PathStr;
var
   i : integer;
begin
    StripBlanks(fName);
    i := 0;
    repeat
       inc(i);
       Result  := System.IOUtils.TPath.Combine(Path, fName + IntToStr(i) + Ext);
    until not FileExists(Result);
end;


function NextFilePath(Path : PathStr) : PathStr;
var
   i : integer;
begin
   i := 0;
   repeat
      inc(i);
      Result := Path + '_' + IntToStr(i) + '\';
   until Not (PathIsValid(Result));
   SafeMakeDir(Result);
end;


function MakeLongInt(v1,v2,v3,v4 : byte; BigEndian : boolean) : LongInt;
var
   TLongInt,T : LongInt;
   Sign       : integer;
begin
   if BigEndian then begin
      if (v1 > 127) then begin
         v1 := 255 - v1;
         v2 := 255 - v2;
         v3 := 255 - v3;
         v4 := 255 - v4;
         Sign := -1;
      end
      else Sign := 1;
      TLongInt := V4;
      T := 1;
      TLongInt := TLongInt + (T * 256*V3);
      TLongInt := TLongInt + (T * 65536*V2);
      TLongInt := TLongInt + (T * 16777216*V1);
      MakeLongInt := Sign * TLongInt;
   end
   else begin
      TLongInt := V1;
      T := 1;
      TLongInt := TLongInt + (T * 256*V2);
      TLongInt := TLongInt + (T * 65536*V3);
      TLongInt := TLongInt + (T * 16777216*V4);
      MakeLongInt := TLongInt;
   end;
end;


procedure FindMatchingFiles(Dir : PathStr; Mask : ANSIstring; var TheFiles : tStringList; DigLayers : integer = 0 );  // StatusBar1 : tStatusBar = nil);
const
   Max = 64000;
var
   DirInfo : tSearchRec;
   Dirs : tStringList;
   Count,i,j,First,Last : integer;
   tFile : textFile;
   fName : PathStr;
begin
     {$IfDef RecordFindFiles} WriteLineToDebugFile('PETMAR.FindMatchingFiles: ' + Dir +',  mask=' + Mask); {$EndIf}
     if (TheFiles = Nil) then TheFiles := tStringList.Create
     else TheFiles.Clear;
     if (Dir = '') then exit;
     
     Dirs := tStringList.Create;
     if Dir[length(Dir)] <> '\' then Dir := Dir + '\';
     Dirs.Add(Dir);
     Last := -1;
     for i := 1 to DigLayers do begin
        First := succ(Last);
        Last := pred(Dirs.Count);
        {$IfDef RecordFindFiles} WriteLineToDebugFile(IntToStr(First) + '--' + IntToStr(Last)); {$EndIf}
        for j := First to Last do begin
           if FindFirst(Dirs[j] + '*.*',faDirectory,DirInfo) = 0 then repeat
              if (DirInfo.Attr and faDirectory <> 0) and (DirInfo.Name[1] <> '.') then begin
                 Dirs.Add(Dirs[j] + DirInfo.Name + '\');
              end;
           until FindNext(DirInfo) <> 0;
        end;
        if (Last = pred(Dirs.Count)) then break;
     end;

     {$IfDef RecordFindFiles} WriteLineToDebugFile('PETMAR.FindMatchingFiles'); {$EndIf}
     fName := System.IOUtils.TPath.Combine(MDTempDir,'templisting.txt');
     Count := 0;
     for i := pred(Dirs.Count) downto 0 do begin
        {$IfDef RecordFindFiles} WriteLineToDebugFile(Dirs.Strings[i] + Mask); {$EndIf}
        if FindFirst(Dirs.Strings[i] + Mask,faAnyFile,DirInfo) = 0 then repeat
            if (DirInfo.Attr and faDirectory = 0) then begin
               inc(Count);
               if (Count < Max) then TheFiles.Add(Dirs.Strings[i] + DirInfo.Name)
               else begin
                  if (Count = Max) then begin
                     TheFiles.SaveToFile(fname);
                     TheFiles.Clear;
                     assignFile(tFile,fName);
                     append(tfile);
                  end;
                  writeln(tfile,Dirs.Strings[i] + DirInfo.Name);
               end;
            end;
        until FindNext(DirInfo) <> 0;
        {$IfDef RecordFindFiles} WriteLineToDebugFile('Cum files: ' + IntegerToString(Count,6) + '   Dir: ' + Dirs.Strings[i]);   {$EndIf}
     end;
     {$IfDef RecordFindFiles} WriteLineToDebugFile(''); {$EndIf}
  SysUtils.FindClose(DirInfo);
  Dirs.Free;
  if (Count >= Max) then begin
     closeFile(Tfile);
     TheFiles.LoadFromFile(fName);
     SysUtils.DeleteFile(fname);
  end;
end;


function GetSubDirsInDirectory(BaseDir : PathStr) : tStringList;
var
   DirInfo : tSearchRec;
begin
   Result := tStringList.Create;
   if FindFirst(BaseDir + '*.*',faDirectory,DirInfo) = 0 then repeat
      if (DirInfo.Attr and faDirectory <> 0) and (DirInfo.Name[1] <> '.') then Result.Add(DirInfo.Name);
   until FindNext(DirInfo) <> 0;
   SysUtils.FindClose(DirInfo);
end;


procedure DeleteMultipleFiles(Dir : PathStr; Mask : ANSIstring);
var
   TheFiles : tStringList;
   i : integer;
begin
  if UpperCase(Dir) = 'C:\' then begin
     MessageToContinue('DeleteMultipleFiles attempt to delete dir ' + Dir + '; submit bug report');
  end
  else begin
     TheFiles := Nil;
     FindMatchingFiles(Dir,Mask,TheFiles);
     {$IfDef TrackFileDeletion} MessageToContinue('DeleteMultipleFiles call DeleteFileIfExists'); {$EndIf}
     if (TheFiles.Count > 0) then for i := 0 to pred(TheFiles.Count) do DeleteFileIfExists(TheFiles.Strings[i]);
     TheFiles.Free;
     ApplicationProcessMessages;
  end;
end;


procedure CleanOutDirectory(BaseDir : PathStr);
var
   SubDirs  : tStringList;
   fName : PathStr;
   i : integer;
begin
   Subdirs := GetSubDirsInDirectory(BaseDir);
   if (SubDirs.Count > 0) then for i := 0 to pred(SubDirs.Count) do begin
      fName := System.IOUtils.TPath.Combine(BaseDir, Subdirs.Strings[i] + '\');
      {$IfDef RecordFileDelection} WriteLineToDebugFile(' Deleting ' + fName); {$EndIf}
      DeleteMultipleFiles(fName,'*.*');
      if (fName <> '') and PathIsValid(fName) then begin
         try
            RmDir(fName);
         except
            on Exception do begin end;
         end;
      end;
   end;
   Subdirs.Free;
end;


procedure Delay(ms : Cardinal);
var
   TheTime : Cardinal;
begin
   {$IfDef VLC}
      TheTime := GetTickCount + ms;
      while (GetTickCount < TheTime) do ApplicationProcessMessages;
   {$Else}
      TheTime := TThread.GetTickCount + ms;
      while (TThread.GetTickCount < TheTime) do ApplicationProcessMessages;
   {$EndIf}
end;


procedure StopSplashing;
begin
   {$IfDef ExSplash}
   {$Else}
      {$IfDef VCL}
         if (MDSplashForm <> Nil) then begin
            MDStopSplashing;
            MDSplashForm := Nil;
         end;
      {$EndIf}
      ApplicationProcessMessages;
   {$EndIf}
end;


procedure InsureFileIsNotReadOnly(InputFileName : PathStr);
{$IfDef MSWindows}
var
   Attributes : word;
begin
   if FileExists(InputFileName) then begin
      Attributes := FileGetAttr(InputFileName);
      if (Attributes and faReadOnly) = faReadOnly then begin
         Attributes := Attributes and (not faReadOnly);
         FileSetAttr(InputFileName, Attributes);
      end;
      FileMode := 2;
   end;
{$Else}
begin
{$EndIf}
end;


function GetIntegerFromString(var Input : ShortString; var Int : integer) : boolean;
var
  i,error : integer;
begin
   Input := TrimLeft(Input);
   i := 1;
   while Input[i] in ['0'..'9'] do inc(i);
   Val(Copy(Input,1,i),Int,Error);
   Result := Error = 0;
   Delete(Input,1,i);
end;


function GetRealFromString(var Input : ShortString; var Int : float64) : boolean;
var
  i,error : integer;
  TStr    : Shortstring;
begin
   Input := TrimLeft(Input);
   i := 1;
   while Input[i] in ['0'..'9','.'] do inc(i);
   TStr := Copy(Input,1,i);
   StripBlanks(TStr);
   Val(TStr,Int,Error);
   Result := Error = 0;
   Delete(Input,1,i);
end;


function AnswerIsYes(Prompt : ANSIstring) : boolean;
var
   x,y : integer;
begin
   {$IfDef VCL}
      x := Mouse.CursorPos.X - 25;
      y := Mouse.CursorPos.Y - 75;
      if (x < 10) then x := 10;
      if (y < 10) then y := 10;
      if x > Screen.Monitors[0].Width - 250 then x := Screen.Monitors[0].Width - 250;
      if y > Screen.Monitors[0].Height - 250 then y := Screen.Monitors[0].Height - 250;
      Result := MessageDlgPos(Prompt + '?',mtConfirmation,[mbYes,mbNo],0,x,y) = ID_Yes;
   {$EndIf}
end;



procedure ModifyRGBColor(Color : TPlatformColor; r,g,b : integer; a : integer = 255);
begin
   {$IfDef FMX}
      if r >= 0 then TAlphaColorRec(Color).r := ValidByteRange(r);
      if g >= 0 then TAlphaColorRec(Color).g := ValidByteRange(g);
      if b >= 0 then TAlphaColorRec(Color).b := ValidByteRange(b);
      if a >= 0 then TAlphaColorRec(Color).a := ValidByteRange(a);
   {$EndIf}
   {$IfDef VCL}
      if r >= 0 then Color.rgbtRed := ValidByteRange(r);
      if g >= 0 then Color.rgbtGreen := ValidByteRange(g);
      if b >= 0 then Color.rgbtBlue := ValidByteRange(b);
   {$EndIf}
end;


function RGBtrip(r,g,b : integer; a : integer = 255) : TPlatformColor;
begin
   {$IfDef FMX}
      TAlphaColorRec(Result).r := ValidByteRange(r);
      TAlphaColorRec(Result).g := ValidByteRange(g);
      TAlphaColorRec(Result).b := ValidByteRange(b);
      TAlphaColorRec(Result).a := a;
   {$EndIf}
   {$IfDef VCL}
      Result.rgbtRed := ValidByteRange(r);
      Result.rgbtGreen := ValidByteRange(g);
      Result.rgbtBlue := ValidByteRange(b);
   {$EndIf}
end;


function GrayRGBtrip(i : integer; a : integer = 255) : TPlatformColor;
var
  ib : byte;
begin
   ib := ValidByteRange(i);
   {$IfDef VCL}
      Result.rgbtRed := ib;
      Result.rgbtGreen := ib;
      Result.rgbtBlue := ib;
   {$EndIf}
   {$IfDef FMX}
      TAlphaColorRec(Result).r := ib;
      TAlphaColorRec(Result).g := ib;
      TAlphaColorRec(Result).b := ib;
      TAlphaColorRec(Result).a := ValidByteRange(a);
   {$EndIf}
end;


function CurrentMilitaryTime(WithSeconds : boolean = false) : shortString;
var
  Time : TDateTime;
  Hour, Min, Sec, MSec : Word;
begin
   Time := GetTime;
   DecodeTime(Time,Hour, Min, Sec, MSec);
   Result := AddDayMonthLeadingZero(Hour) + AddDayMonthLeadingZero(Min);
   if WithSeconds then Result := Result + ':' + AddDayMonthLeadingZero(Sec);
end;


procedure DragDirections;
begin
   MessageToContinue('Pick NW corner; drag to SE corner' + MessLineBreak + ' with mouse button depressed.');
end;


function ValidByteRange(value : integer) : integer;
begin
   if (Value > 255) then Result := 255
   else if (Value < 0) then Result := 0
   else Result := Value;
end;

function ValidByteRange254(value : integer) : integer;
begin
   if (Value > 254) then Result := 254
   else if (Value < 0) then Result := 0
   else Result := Value;
end;

procedure GetRGBfromTColor(Color : TColor; var r,g,b : byte);
var
   Temp : Integer;
begin
   Temp := color;
   if (Temp > $02000000) then dec(Temp,$02000000);
   r := ValidByteRange(temp mod 256);
   temp := temp div 256;
   g := ValidByteRange(temp mod 256);
   temp := temp div 256;
   b := ValidByteRange(temp mod 256);
end;


procedure FixForwardSlashes(var fName : PathStr);
var
   i : integer;
begin
   for i := 1 to Length(fname) do if fName[i] = '/' then fName[i] := '\';
end;


procedure FSplit(FName : PathStr; var Dir : DirStr; var Name : NameStr; var Ext : ExtStr);
begin
   Name := ExtractFileName(FName);
   Ext := ExtractFileExt(FName);
   Delete(Name,Length(Name)-pred(Length(Ext)),Length(Ext));
   Dir := ExtractFilePath(FName);
end;


function ExtractFileNameNoExt(fName : PathStr) : NameStr;
var
   Ext : ExtStr;
begin
   Result := ExtractFileName(FName);
   Ext := ExtractFileExt(FName);
   Delete(Result,Length(Result)-pred(Length(Ext)),Length(Ext));
end;


procedure StripCharacter(var St : AnsiString; ch : AnsiChar);
var
   i   : integer;
begin
   if Length(St) > 0 then
      for i := length(St) downto 1 do if St[i] = ch then Delete(St,i,1);
end;

procedure ReplaceCharacter(var St : AnsiString; FindChar,ReplaceChar : AnsiChar);
var
   i   : integer;
begin
   if Length(St) > 0 then
      for i := length(St) downto 1 do if St[i] = FindChar then St[i] := ReplaceChar;
end;


function RemoveLowerCase(k1 : ShortString) : shortString;
var
   i   : integer;
begin
   Result := k1;
   if Length(k1) > 0 then
      for i := length(k1) downto 1 do if k1[i] in ['a'..'z'] then Delete(Result,i,1);
end;

function RemoveNumbers(k1 : ShortString) : shortString;
var
   i   : integer;
begin
   Result := k1;
   if Length(k1) > 0 then
      for i := length(k1) downto 1 do if k1[i] in ['0'..'9'] then Delete(Result,i,1);
end;


procedure StripBlanks(var St : AnsiString);
var
   i   : int16;
begin
   if Length(St) > 0 then
      for i := length(St) downto 1 do
         if St[i] = ' ' then Delete(St,i,1);
end;


procedure MessageToContinue(Mess : ANSIstring; SaveOutPut : boolean = true);
var
   TStr : tStringList;
   tMess : string;
begin
   {$IfDef VCL}
      if SaveOutPut then begin
         TStr := tStringList.Create;
         while length(Mess) > 0 do TStr.Add(BeforeSpecifiedCharacterANSI(Mess,#13,false,true));
         if (TStr.Count > 20) then begin
            DisplayAndPurgeStringList(TStr,'');
         end
         else begin
            StringListToContinue(SaveOutPut,TStr);
            TStr.Free;
         end;
      end
      else begin
         tMess := Mess;
         MessageDlg(tMess,mtInformation,[mbOK],0);
      end;
   {$EndIf}
end;


function CopyFile(SourceFile,DestinationFile: string):  boolean;
var
   Tries : integer;
begin
   tries := 0;
   repeat
      try
         {$IfDef VCL}
            Result := Windows.CopyFile(pchar(SourceFile),pchar(DestinationFile),false {Fail if Exists});
         {$EndIf}
         exit;
      except
         on Exception do inc(tries);
      end;
   until (tries > 5);
   Result := false;
end {CopyFile};


function MoveFile(SourceFile,DestinationFile: string):  boolean;
var
   Tries : integer;
begin
   tries := 0;
   repeat
      try
         {$IfDef VCL}
            Result := Windows.MoveFile(pchar(SourceFile),pchar(DestinationFile));
         {$EndIf}
         exit;
      except
         on Exception do inc(tries);
      end;
   until (tries > 5);
   Result := false;
end {CopyFile};



initialization
   {$IfDef MessageStartup} MessageToContinue('start PETMAR initialization'); {$EndIf}
   ThreadsWorking := false;
   HeavyDutyProcessing := false;
   LoadingFromMapLibrary := false;
   DEMIXProcessing := false;
   claBrown := ConvertTColorToPlatformColor(clBrown);
finalization
   {$If Defined(RecordClosing)} WriteLineToDebugFile('Closing petmar in dbfn=' + DebugFileName); {$EndIf}
   {$IfDef RecordPrint} WriteLineToDebugFile('RecordPrintProblems active in PETMAR'); {$EndIf}
   {$IfDef RecordStartup} WriteLineToDebugFile('RecordStartupProblems active in PETMAR'); {$EndIf}
   {$IfDef MessageStartup} WriteLineToDebugFile('MessageStartupProblems active in PETMAR'); {$EndIf}
   {$IfDef RecordFindFiles} WriteLineToDebugFile('RecordFindFilesProblems active in PETMAR'); {$EndIf}
   {$IfDef TempFileClose} WriteLineToDebugFile('TempFileCloseProblems active in PETMAR'); {$EndIf}
   {$IfDef RecordShellExecute} WriteLineToDebugFile('RecordShellExecute active in PETMAR'); {$EndIf}
   {$IfDef RecordBitmap} WriteLineToDebugFile('RecordBitmapProblems active in PETMAR'); {$EndIf}
   {$IfDef RecordFileFilter} WriteLineToDebugFile('RecordFileFilterProblems active in PETMAR'); {$EndIf}
   {$IfDef RecordFileDelection} WriteLineToDebugFile('RecordFileDelection active in PETMAR'); {$EndIf}
   {$IfDef RecordLegends} WriteLineToDebugFile('RecordLegends active in PETMAR'); {$EndIf}
   {$IfDef RecordListPick} WriteLineToDebugFile('RecordListPick active in PETMAR'); {$EndIf}
   {$IfDef RecordUnzips} WriteLineToDebugFile('RecordUnzips active in PETMAR'); {$EndIf}
   {$IfDef RecordHelp} WriteLineToDebugFile('Record active in PETMAR'); {$EndIf}
   {$IfDef RecordWebDownloads} WriteLineToDebugFile('RecordWebDownloads active in PETMAR'); {$EndIf}
   {$If Defined(RecordDialogs)} WriteLineToDebugFile('RecordDialogs active in PETMAR'); {$EndIf}
   {$If Defined(RecordClosing)} WriteLineToDebugFile('Closing petmar out dbfn=' + DebugFileName); {$EndIf}
end {unit}.











