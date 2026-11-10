unit petmar;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program           }
{ PETMAR Trilobite Breeding Ranch        }
{ Released under the MIT Licences        }
{ Copyright (c) 1986-2026 Peter L. Guth  }
{________________________________________}

{$I nevadia_defines.inc}


{$Define ColorRoutines}
{$Define IncludePalettesLegends}

{$IfDef FMX}
   {$Define UseIndyDownLoads}
{$EndIf}

{$IfDef RecordProblems} //normally only defined for debugging specific problems
   {$Define RecordShellExecute} //this should generally be on; if not desired, shut down in Windows defaults
   //{$Define RecordWebDownloads}

   {$IfDef Debug}
      //{$Define RecordColorPalette}
      //{$Define RecordDialogs}
      //{$Define ShowProgressBarSize}
      //{$Define RecordFileDelection}
      //{$Define RecordLegends}
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
      //{$Define RecordListPick}
      //{$Define RecordGeology}
   {$EndIf}
{$EndIf}


interface

uses

(*
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
*)

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

function RemoveUnderScores(LegendStr : ANSIString; AlsoFileNextNumber : boolean = true) : ANSIString;
function SpacesToUnderScores(LegendStr : ANSIString) : ANSIString;

function CreateHTML_href(fName,Display : ANSIString) : ANSIString;


var
   SevenZipfName : PathStr;


{$IfDef VCL}

      procedure ShowHourglassCursor; inline;
      procedure ShowDefaultCursor; inline;
      procedure ApplicationProcessMessages; inline;

      function File2Trash(const FileName: string): boolean;

      procedure StartThreadTimers(Capt : ShortString = ''; MaxThreads : integer = 8; Enable1 : boolean = false);
      procedure EndThreadTimers;
      procedure StartSingleThreadTimer(Capt : ShortString = '');

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

      procedure GetDate(var Month,Day,Year : word);
      procedure GetDateAndDuration(var Month,Day,Year,Duration : word; ShowDuration : boolean = true);

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
            procedure ColorBitBtn(var BitBtn : tBitBtn; Color : tPlatformColor);  overload;
            procedure ColorBitBtn(var BitBtn : tBitBtn; Color : tColor);          overload;


      procedure TextOutVertical(Canvas : TCanvas; x,y : integer; Words : ShortString; ClearBack : boolean = false);
      procedure CanvasTextOutAngle(c : TCanvas; x,y: Integer; d : Word; s: ShortString);
           {angle measured in tenths degree, math convention: starts with east and runs counterclockwise}

      procedure DisplayNevadella(Canvas : TCanvas; xoff,yoff,size : integer; Color : TColor);

      function LocalIP: string;

      function FontInstalled(Const FontName:String) : Boolean;


      //drawing
         procedure CrossWithHole(Canvas : tCanvas; x,y : integer);
         procedure ScreenSymbol(Canvas : TCanvas; x,y : integer; Sym : tDrawingSymbol; size: integer; color : TPlatformColor); overload; inline;
         procedure ScreenSymbol(Canvas : TCanvas; x,y : integer; Symbol : tFullSymbolDeclaration); overload; inline;

            procedure StartCount(Title : ShortString);
            procedure UpdateCount(HowFar : LongInt);
            procedure EndCount;

      function ExecuteFile(const FileName : ANSIstring; Params : ANSIstring = ''; DefaultDir : ANSIstring = '') : THandle;
      function WinExecAndWait32(FileName : ANSIstring; Wait : boolean = true; Log : boolean = true) : integer;

      function MultiSelectSingleColumnStringList(InMessage : ANSIstring; var PickedNum : integer; var InList : TStringList; CanCancel : boolean = false; MultiPick : boolean = false) : boolean;
      function GetMultipleFromList(InMessage : ANSIstring; var PickedNum : integer; var InList : TStringList; CanCancel : boolean = false) : boolean;
      function GetFromList(InMessage : ANSIstring; var PickedNum : integer; InList : TStringList; CanCancel : boolean = false) : boolean;  overload;
      function GetFromList(InMessage : ANSIstring; InList : TStringList; CanCancel : boolean = false) : ShortString; overload;

      procedure GetString(Prompt : ShortString; var Input : ShortString; ForceCaps : boolean; ValidChars : characters);
      procedure GetValidDBfieldname(var fName2 : shortstring);

      procedure QueryTColor(var Color : tColor); overload;
      procedure QueryColor(var Color : tPlatformColor); overload;
      procedure QueryColor(BitBtn : tBitBtn; var Color : tPlatformColor); overload;
      procedure QueryTColor(BitBtn : tBitBtn; var Color : Tcolor); overload;
      procedure GetAngleInRadians(Message : shortstring; var Angle : float64);
      procedure GetAngle(Mess : shortstring; var theAngle : float64; var AngleMeasure : tAngleMeasure);


      //font routines
         procedure LoadMyFontIntoWindowsFont(MyFont : tMyFont; WindFont : tFont);
         procedure GetMyFontFromWindowsFont(var MyFont : tMyFont; WindFont : tFont);
         procedure InitializeMyFont(var MyFont : tMyFont; Name : shortstring; Size : SmallInt; Color : tPlatformColor);
         procedure EditMyFont(var MyFont : tMyFont);
         procedure EditTFont(Font : tFont);
         function MyFontToString(MyFont : tMyFont) : ShortString;


      procedure StopSplashing;

//progress routines, which do nothing if NoProgress is defined
      procedure StartProgress(Title : ShortString; ProgressTop  : integer = -99; ProgressLeft : integer = -99);
      procedure StartProgressAbortOption(Title : ShortString; ProgressTop : integer = -99; ProgressLeft : integer = -99);
      procedure UpdateProgressBar(HowFar : float64);
      procedure EndProgress;

    function AnswerIsYes(Prompt : ANSIstring) : boolean; overload;
    procedure MessageToContinue(Mess : ANSIstring; SaveOutPut : boolean = true);
    procedure StringListToContinue(SaveOut : boolean; Mess : tStringList);

function YorN(What : boolean) : ANSIchar; inline;

procedure DownloadListOfFiles(Memo1 : tMemo = nil);
procedure DisplayHTMLTopic(Name : AnsiString);
procedure DragDirections;


procedure ReadDefault(Prompt : ShortString; var IntVal : byte); overload;
procedure ReadDefault(Prompt : ShortString; var IntVal : int16); overload;
procedure ReadDefault(Prompt : ShortString; var IntVal : int32); overload;
procedure ReadDefault(Prompt : ShortString; var IntVal : int64); overload;
procedure ReadDefault(Prompt : ShortString; var RealVal : float64); overload;
procedure ReadDefault(Prompt : ShortString; var RealVal : float32); overload;
procedure ReadDefault(Prompt : ShortString;  var WordVal : word); overload;

//file routines, file name and path names
   function FileCanBeOpened(fName : PathStr) : boolean;
      function GetExistingFileName(Mess,Filters : ANSIstring; var FName : PathStr) : boolean;
      function GetFileNameDefaultExt(Mess,Filters : ANSIstring; var FName : PathStr; AppendOption : boolean = false) : boolean;
      function GetFileNameDefaultExtSaveExt(Mess,Filters : ANSIstring; var FName : PathStr; var  df : Integer; AppendOption : boolean = false) : boolean;
      function GetMultipleFiles(InMessage,Masks : ANSIstring; var FilesWanted : TStringList; var DefaultFilter : byte) : boolean;
      function GetFileMultipleMask(InMessage,Masks : ANSIstring; var FileWanted : PathStr; var DefaultFilter : byte) : boolean;
      function GetMultipleDirectories(TheMessage : AnsiString; var Paths : tStringList) : boolean;
      function GetFileFromDirectory(InMessage : ANSIstring; Mask : ANSIstring;  var FileWanted : PathStr) : boolean;
      function GetDOSPath(TheMessage : AnsiString; var Path : PathStr) : boolean;
      procedure UnblockFile(fName : PathStr);

      procedure AddSuffixOrPrefixToFiles(PrefixWanted : boolean);


function BitmapSizeString(Bitmap : tMyBitmap) : shortstring;

//image and clipboard routines
      {$IfDef VCL}
         procedure CopyPartOfScreenToBitmap(var Bitmap2 : tMyBitmap; Left,Top,Right,Bottom : integer);
         procedure CaptureActiveWindowToClipboard;
         procedure CaptureEntireScreenToClipboard;
      {$EndIf}

function GetVolumeName(DriveLetter: ANSIChar): shortstring;
procedure OpenRecycleBin;

procedure WriteOpenHandlestoDebugLog(Where : shortString);
function AvailPhysMemStr : shortstring;

      function FindPath(What,NeededName : shortstring; var TheDir : PathStr) : boolean;

{$EndIf}  //vcl


{$IfDef FMX}
   function RGB(r,g,b : byte) : integer;
{$EndIf}


{$IfDef MICRODEM}
//ANSIstring routines with deg/min/sec and time
      procedure ConvertToDegMinSecString(Value : float64; OutPutMethod : tLatLongMethod;  var DegString,MinString,SecString : string10; HighAccuracy : boolean = true);
      function ConvertToDegreesString(Value : float64; OutPutMethod : tLatLongMethod; HighAccuracy : boolean = true) : shortstring;
      function LatLongDegreeToString(Lat,Long : float64; OutPutLatLongMethod : tLatLongMethod = DecDegrees) : shortstring;
      function LatLongRadToString(Lat,Long : float64; OutPutLatLongMethod : tLatLongMethod = DecDegrees) : shortstring;
      function LatToString(Lat : float64; OutPutLatLongMethod : tLatLongMethod) : shortstring;
      function LongToString(Long : float64;  OutPutLatLongMethod : tLatLongMethod) : shortstring;
      function LatLongToStringForFileName(Lat,Long : float64) : shortstring;
      function LatToString3(Lat : float64) : shortstring;
      function LongToString4(Long : float64) : shortstring;

//format strings with units
      function SmartDistanceMetersFormat(d : float64) : shortString;
      function SmartVolumeFormat(d : float64; DistanceUnits : tDistanceUnits = disMetric) : shortString;
      function SmartAreaFormat(d : float64) : shortString;
      function AngleFormat(Angle : float64; Units : tAngleMeasure; ShowUnits : boolean = true) : shortstring;
{$EndIf}

{$IfDef ColorRoutines}
      //color routines
         function ColorString(Color : tPlatformColor) : shortString; overload;
         function ColorString(Color : tColor) : shortString; overload;

         function RGBString(r,g,b : SmallInt; IncludeGray : boolean = false) : ShortString;

         function ColorToHtml(DColor : TColor) : ShortString;
         procedure tColortoHLS(Color : tColor; var h,l,s : float64);
         function HLStoTcolor(H,L,S : float64) : tcolor;
         procedure RGBtripToHLS(Color : tRGBTriple; var h,l,s : float64);

         function MonthColor(i : integer) : TPlatformColor;
         function SameColor(c1,c2 : tPlatformColor) : boolean; inline;
         function RGBtripFromHSI(Hue,Sat,Int : float64) : tPlatformColor;
         procedure GetRGBfromTColor(Color : TColor; var r,g,b : byte);

         procedure RGBtoHLS(r,g,b : integer; var h,l,s : float64);
         procedure HLStoRGB(h,l,s : float64; var r,g,b : byte);

         function GrayColorFunct(i : integer) : TColor;  overload;
         function GrayColorFunct(z,Min,Max : float64) : TColor; overload; inline;
         function SpectrumColorFunct(z,Min,Max : float64) : tColor; inline;
         function RainbowColorFunct(z,Min,Max : float64) : TColor; inline;
         function TerrainTColor(z,aMin,aMax : float64) : TColor;

         function ChloroplethPlatformColor(ColorDefTable : tColorTableDefinitions; z,aMin,aMax : float64) : TPlatformColor;
         function RainbowRGBFunct(z,MinV,MaxV : float64) : tPlatformColor; inline;
         function TerrainRGBFunct(z,Min,Max : float64) : tPlatformColor;
         function SpectrumRGBFunct(z,Min,Max : float64) : tPlatformColor; inline;
         function OceanRGBFunct(z,Min,Max : float64) : tPlatformColor; inline;

         function SelectedColorSchemeColorFunct(ColorScheme: tLegendColors; ColorTable : tColorTableDefinitions; z : float64; Min,Max : float64) : TColor;
         function ColorFromZColorTable(ZColorTable : tColorTableDefinitions; zj : float64; var k : integer) : tRGBTriple;

         function RGBtrip(r,g,b : integer; a : integer = 255) : TPlatformColor;  inline;
         function GrayRGBtrip(i : integer; a : integer = 255) : TPlatformColor;  inline;

         procedure ModifyRGBColor(Color : TPlatformColor; r,g,b : integer; a : integer = 255);
        function ConvertPlatformColorToTColor(aColor : tPlatformColor) : tColor;
        function ConvertTColorToPlatformColor(aColor : tColor) : tPlatformColor;
        procedure ConvertPlatformColorToRGB(aColor : tPlatformColor; var r,g,b : byte);
{$EndIf}


   {$If Defined(VCL) and Defined(IncludePalettesLegends)}
      //legends
         function MakeColorScaleBitmap(width,height : integer; ColorScheme : tLegendColors; ColorTable : tColorTableDefinitions) : tMyBitmap;
         function HorizontalLegendOnBitmap(Colors : tColors256; Values : array of float64; Units,LegendTitle : shortstring; LegendSize : integer = 1) : tMyBitmap;
         function DefaultHorizontalLegendOnBitmap(Min,Max : float64; Units,LegendTitle : shortstring; Legend : tLegendColors; ChloroplethScheme : shortstring = ''; Reverse : boolean = false) : tMyBitmap;
         function DefaultVerticalLegendOnBitmap(Min,Max : float64; Units,LegendTitle : shortstring; Legend : tLegendColors; ChloroplethScheme : shortstring = ''; Reverse : boolean = false) : tMyBitmap;
         function VerticalLegendOnBitmap(Colors : tColors256; Values : array of float64; Units : shortstring; LegendTitle : shortstring = '') : tMyBitmap;
         function MakeLegend(DEMsUsed,ColorsUsed : tstringList; Horizontal : boolean = true; MaxWide : integer = 2400) : tMyBitmap;
   {$EndIf}



function CurrentTimeForFileName(IncludeHours : boolean = true) : PathStr;


function DownloadFileFromWeb(WebName,LocalName : ANSIstring; ShowFailure : boolean = true) : boolean;

procedure GetSeparationCharacter(MenuStr : ANSIstring; var SepChar : ANSIchar);
procedure GetSeparationCharacterUnicode(MenuStr : string; var SepChar : char);


{$IfDef MSWindows}
   procedure StripBlanks(var St : Shortstring);
{$Else}
   procedure StripBlanks(var St : ANSIString);  {removes blank characters from any ANSIstring}
{$EndIf}

procedure StripCharacter(var St : AnsiString; ch : AnsiChar);
procedure ReplaceCharacter(var St : AnsiString; FindChar,ReplaceChar : AnsiChar);

function RemoveLowerCase(k1 : ShortString) : shortString;
function RemoveNumbers(k1 : ShortString) : shortString;

function NoCommas(TheString: ShortString) : shortstring;


function ValidByteRange(value : integer) : integer; inline;
function ValidByteRange254(value : integer) : integer; inline;

function MakeLongInt(v1,v2,v3,v4 : byte; BigEndian : boolean) : LongInt;

procedure SortAndRemoveDuplicates(var StringList : tStringList);
procedure CopyStringList(FromList : tStringList; var ToList : tStringList);
procedure RandomizeStringList(var TheList : tStringList; SpareFirst : boolean = true);
procedure RemoveFilesThatDoNotHaveString(var Files : tStringList; What : shortstring);
procedure RemoveFilesThatDoHaveString(var Files : tStringList; What : shortstring);

function GetFromRequest(InString : ANSIstring; WantField : shortstring) : shortstring;
function GetFloatFromRequest(InString : ANSIstring; WantField : shortstring) : float64;
function GetIntegerFromRequest(InString : ANSIstring; WantField : shortstring) : integer;

function ReadFilter(var Filter : FilterType; var FilterSize,FilterLap,Sum : integer; var FilterName : PathStr; DisplayFilter : boolean = true) : boolean;


//file routines, file name and path names

      function GetSubDirsInDirectory(BaseDir : PathStr) : tStringList;
      function GetFileSize(FileName : String) : Int64;
      function LastSubDir(Dir : AnsiString) : PathStr;

       procedure FixFileNameBackslashes(var fName : PathStr);

      function GetParentDirectory(path : Ansistring) : Ansistring;
      procedure StripInvalidPathNameChars(var fName : PathStr; PathOK : boolean = false);
      function CopyFile(SourceFile,DestinationFile : string):  boolean;
      function MoveFile(SourceFile,DestinationFile : string):  boolean;
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

// print routines
      {$IfDef ExPrintFile}
      {$Else}
         procedure PrintImageToSpecifiedSize(Image1 : TImage; PrinterXInches,PrinterYInches : float64);
      {$EndIf}


function ArrayOfCharToString(LenArray : integer; var Chars : array of ANSIchar) : shortstring;


//byte order swapping
      procedure Int4Swap(var int : int32); overload;
      procedure Int4Swap(var int : cardinal); overload;
      procedure SwapToShortFloat(var FloatValue : float32);
      procedure SwapToFloat8byte(var FloatValue : Float64);


//ANSIstring routines with  time
      function HoursMinutesString(Time : float64; WithSec : boolean = false) : ShortString;
      function CurrentMilitaryTime(WithSeconds : boolean = false) : shortString;
      function CreateTimeString(Hour,Minute,Second : integer) : shortString;


      function SmartNumberPoints(NumPoints : int64) : shortstring;
      function FormatString(TheString : shortString; Len : integer; Justify : tJustify) : shortString;


const
   QuietActions : boolean = false;
   DEMMergeInProgress : boolean = false;

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


function MakeStringListFromString(theString : shortstring) : tStringList;

procedure RemoveASCII0FromFile(fName : PathStr);

function FindDriveWithFile(var fName : PathStr) : boolean;
function FindDriveWithPath(var aPath : PathStr) : boolean;


function FindFieldInStringList(Header : tStringList; FieldName : ANSIstring; SepChar : ANSIchar) : ANSIstring;
function FindIntegerFieldInStringList(Header : tStringList; FieldName : ANSIstring; SepChar : ANSIchar) : Integer;
function FindFloatFieldInStringList(Header : tStringList; FieldName : ANSIstring; SepChar : ANSIchar) : float64;

function TrueOrFalse(BoolVar : boolean) : shortstring;


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



function DoubleQuotedString(str : ANSIString) : ANSIstring;
function NumberOfStringsWithSubstring(var sl : tStringList; ss : shortstring) : integer;

procedure ClearTemporaryFileGroup(var SL : tStringList; Free : boolean = false);


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
   PetImage,
{$EndIf}

   DEMDefs,

   {$IfDef ExGIS}
   {$Else}
      petdbutils, DEMdef_routines,
   {$EndIf}
   PETMath;      {math library}

var
   TerrainCuts : array[0..4,1..3] of integer;


{$If Defined(VCL) and Defined(IncludePalettesLegends)}
   {$I petmar_palettes_legends.inc}
{$EndIf}

{$IfDef ColorRoutines}
   {$I petmar_color_routines.inc}
{$EndIf}


{$IfDef VCL}
   {$I petmar_vcl.inc}
{$EndIf}



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



function YorN(What : boolean) : ANSIchar;
begin
   if What then Result := 'Y' else Result := 'N';
end;


function MakeStringListFromString(theString : shortstring) : tStringList;
begin
   Result := tStringList.Create;
   Result.Add(theString);
end;


procedure ClearTemporaryFileGroup(var SL : tStringList; Free : boolean = false);
//delete temporary files in the string list, and clear it; optionanlly free the memory
var
   i : integer;
begin
   if (sl <> Nil) then begin
      for i := 0 to pred(sl.Count) do
         if (ExtractFilePath(sl.Strings[i]) = MDTempDir) then DeleteFileIfExists(sl.Strings[i]);
      sl.Clear;
      if Free then sl.Free;
   end;
end;



function NumberOfStringsWithSubstring(var sl : tStringList; ss : shortstring) : integer;
var
   i : integer;
begin
   Result := 0;
   for I := 0 to pred(sl.count) do begin
      if StrUtils.AnsiContainsText(UpperCase(ExtractFileNameNoExt(sl.strings[i])),Uppercase(ss)) then begin
         inc(Result);
      end;
   end;
end;


function DoubleQuotedString(str : ANSIString) : ANSIstring;
begin
   Result := '"' + str + '"';
end;




function FindDriveWithPath(var aPath : PathStr) : boolean;
//used for data files on external hard drive, which can have different drive letters when moved
var
   ch : ANSIchar;
begin
   Result := ValidPath(aPath);
   if (not Result) then begin
      for ch := 'c' to 'z' do begin
         aPath[1] := ch;
         if ValidPath(aPath) then begin
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



function FileTimeFromFileName(fName : PathStr) : ShortString;
begin
   Result := DateTimeToStr(FileDateToDateTime(FileAge(fName)));
   Petmar.ReplaceCharacter(Result,'/','_');
   Petmar.ReplaceCharacter(Result,':','_');
   Petmar.ReplaceCharacter(Result,' ','_');
end;


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

    //StartProgress('Remove ASCII 0');
    TotalRead := 0;
    while not eof(inf) do begin
       BlockRead(inf,charray,32000,NumRead);
       inc(TotalRead,NumRead);
       //UpdateProgressBar(TotalRead/TotalSize);
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
    //EndProgress;
end;



procedure CleanUpFileName(var fname : PathStr);
var
   i : integer;
begin
   fName := StringReplace(fName, '>', 'over',[rfIgnoreCase,rfReplaceAll]);
   fName := StringReplace(fName, '<', 'under',[rfIgnoreCase,rfReplaceAll]);
   for i := Length(fName) downto 1 do begin
      if Not (fName[i] in ValidDosFileNameChars) then fName[i] := '_';
   end;
end;


 function CreateHTML_href(fName,Display : ANSIString) : ANSIString;
 begin
    Result := '<a href="' + fName + '">' + Display + '</a>';
 end;



procedure InitializePetmar;
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

   ReportMemoryLeaksOnShutdown := (DebugHook <> 0);

   WantOut := false;
   WantShowProgress := true;

   {$IfDef VCL}
       RGBtripleRed := RGBtrip(255,0,0);
       RGBtripleBlack := RGBtrip(0,0,0);
       RGBtripleAlmostBlack := RGBtrip(1,1,1);
       RGBtripleWhite := RGBtrip(255,255,255);
       RGBtripleNearWhite := RGBtrip(254,254,254);
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


procedure FixFileNameBackslashes(var fName : PathStr);
begin
   {$IfDef MSWindows}
   {$Else}
      ReplaceCharacter(fName,'\','/');
   {$EndIf}
end;

function RemoveUnderScores(LegendStr : ANSIString; AlsoFileNextNumber : boolean = true) : ANSIString;
begin
   Result := '';
   if (LegendStr <> '') and (Length(LegendStr) > 1) then begin
      Result := LegendStr;
      if AlsoFileNextNumber and (Result[length(Result)-1] = '_') and (Result[length(Result)] in ['1'..'9']) then Delete(Result,length(Result)-1,2);
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
   if length(Result) > 0 then begin
      if (Dir[Length(Result)] = '\') then Delete(Result,Length(Result),1);
      while StrUtils.AnsiContainsText(Result,'\') do begin
         BeforeSpecifiedCharacterANSI(Result,'\',true,true);
      end;
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


procedure StripInvalidPathNameChars(var fName : PathStr; PathOK : boolean);
var
   i : integer;
begin
   for i := length(fName) downto 1 do
      if not (fName[i] in ValidDOSFileNameChars) then System.Delete(fName,i,1);
   if not PathOK then
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
   {$IfDef VCL} if IncludeGray then Result := Result +  '  gray=' + IntToStr(RGBtoGrayscale(r,g,b));  {$EndIf}
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
      if not ValidPath(FilterName)  then FilterName := ProgramRootDir;
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
            {$IfDef VCL} MessageToContinue('Check filter file; too many lines'); {$EndIf}
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


function AngleFormat(Angle : float64; Units : tAngleMeasure; ShowUnits : boolean = true) : shortstring;
begin
   case Units of
      amDegree : Result := RealToString(Angle,-18,-8) + '°';
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
   Result := ptTrim(DegString) + '°';
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
   StripCharacter(Result,'°');
   if length(Result) = 2 then Result := Result[1] + '0' + Result[2];
end;

function LongToString4(Long : float64) : shortstring;
begin
   LongitudeAngleInRange(Long);
   Result := LongToString(Long,ShortDegrees);
   StripCharacter(Result,'°');
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


{$EndIf}



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

procedure RemoveFilesThatDoNotHaveString(var Files : tStringList; What : shortstring);
var
   i : integer;
   fName : PathStr;
begin
   for i := pred(Files.Count) downto 0 do begin
      fName := Files.Strings[i];
      if (not StrUtils.AnsiContainsText(UpperCase(fname),UpperCase(What))) then Files.Delete(i);
   end;
end;

procedure RemoveFilesThatDoHaveString(var Files : tStringList; What : shortstring);
var
   i : integer;
   fName : PathStr;
begin
   for i := pred(Files.Count) downto 0 do begin
      fName := Files.Strings[i];
      if (StrUtils.AnsiContainsText(UpperCase(fname),UpperCase(What))) then Files.Delete(i);
   end;
end;

function CurrentTimeForFileName(IncludeHours : boolean = true) : PathStr;
var
  i : integer;
begin
   Result := DateToStr(now);
   if IncludeHours then Result := Result + '_' + CurrentMilitaryTime(true);
   for i := 1 to length(Result) do if Result[i] in ['/',':'] then Result[i] := '_';
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
    CleanUpFileName(fName);
    StripInvalidPathNameChars(fName);
    if (fName[length(fName)] <> '_') then fName := fName + '_';
    i := 0;
    //Result := System.IOUtils.TPath.Combine(Path, fName + IntToStr(i) + Ext);
    //if not FileExists(Result) then begin
      repeat
         inc(i);
         Result  := System.IOUtils.TPath.Combine(Path, fName + IntToStr(i) + Ext);
      until not FileExists(Result);
    //end;
end;


function NextFilePath(Path : PathStr) : PathStr;
var
   i : integer;
begin
   if not ValidPath(Path) then begin
      Result := Path;
   end
   else begin
     i := 0;
     repeat
        inc(i);
        Result := Path + '_' + IntToStr(i) + '\';
     until Not (ValidPath(Result));
     SafeMakeDir(Result);
   end;
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

     {$IfDef RecordFindFiles} WriteLineToDebugFile('PETMAR.FindMatchingFiles, Dirs=' + IntToStr(Dirs.Count)); {$EndIf}
     Count := 0;
     for i := pred(Dirs.Count) downto 0 do begin
        {$IfDef RecordFindFiles} WriteLineToDebugFile('Looking in: ' + Dirs.Strings[i] + Mask); {$EndIf}
        if FindFirst(Dirs.Strings[i] + Mask,faAnyFile,DirInfo) = 0 then repeat
            if (DirInfo.Attr and faDirectory = 0) then begin
               inc(Count);
               if (Count < Max) then begin
                  TheFiles.Add(Dirs.Strings[i] + DirInfo.Name)
               end
               else if (Count = Max) then begin
                  fName := System.IOUtils.TPath.Combine(MDTempDir,'templisting.txt');
                  TheFiles.SaveToFile(fname);
                  TheFiles.Clear;
                  assignFile(tFile,fName);
                  append(tfile);
               end
               else if (Count > Max) then begin
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
     {$IfDef VCL} MessageToContinue('DeleteMultipleFiles attempt to delete dir ' + Dir + '; submit bug report'); {$EndIf}
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
      if (fName <> '') and ValidPath(fName) then begin
         try
            RmDir(fName);
         except
            on Exception do {$IfDef VCL} MessageToContinue('Unable to delete ' + fName); {$EndIf}
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
         DeleteFileIfExists(SourceFile);
         exit;
      except
         on Exception do inc(tries);
      end;
   until (tries > 5);
   Result := false;
end {CopyFile};



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
            end
            else Missing7ZipMessage;
         end;

{$EndIf}

{$IfDef FMX}

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
      claBrown := ConvertTColorToPlatformColor(clBrown);
   end;
{$EndIf}




initialization
   {$IfDef MessageStartup} MessageToContinue('start PETMAR initialization'); {$EndIf}
   ThreadsWorking := false;
   HeavyDutyProcessing := false;
   LoadingFromMapLibrary := false;
   DEMIXProcessing := false;
   {$IfDef FMX} FMXinit; {$EndIf}
   {$IfDef VCL} VCLinit; {$EndIf}
finalization
   {$If Defined(RecordClosing)} WriteLineToDebugFile('Closing petmar in dbfn=' + DebugFileName); {$EndIf}
end {unit}.











