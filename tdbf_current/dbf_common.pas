unit dbf_common;

interface

{$I dbf_common.inc}

uses
  SysUtils, Classes, DB
{$ifndef WINDOWS}
  , Types, dbf_wtil
{$ifdef KYLIX}
  , Libc
{$endif}  
{$endif}
  ;


const
  TDBF_MAJOR_VERSION      = 7;
  TDBF_MINOR_VERSION      = 1;
  TDBF_SUB_MINOR_VERSION  = 0;

function DbfVersionString: string;

const
  TDBF_TABLELEVEL_FOXPRO = 25;

  JulianDateDelta = 1721425; { number of days between 1.1.4714 BC and "0" }

type
  EDbfError = class (EDatabaseError)
  end;
  EDbfErrorInvalidIndex = class(EDbfError)
  end;
  EDbfWriteError = class (EDbfError)
  end;

{$ifndef SUPPORT_NATIVEINT}
type
  NativeInt = integer;
{$endif}

{$ifdef SUPPORT_TRECORDBUFFER}
  TDbfRecordBuffer = TRecordBuffer;
{$else}
  TDbfRecordBuffer = PAnsiChar;
{$endif}

{$ifdef SUPPORT_TVALUEBUFFER}
  TDbfValueBuffer = TValueBuffer;
{$else}
  TDbfValueBuffer = pointer;
{$endif}

{$ifdef SUPPORT_TRECBUF}
type
  TDbfRecBuf = DB.TRecBuf;
const
  DBfRecBufNil = 0;
{$else}
type
  TDbfRecBuf = TDbfRecordBuffer;
const
  DBfRecBufNil = nil;
{$endif}

type
  TDbfFieldType = AnsiChar;

  TXBaseVersion   = (xUnknown, xClipper, xBaseIII, xBaseIV, xBaseV, xFoxPro, xBaseVII);
  TSearchKeyType = (stEqual, stGreaterEqual, stGreater);

  TDateTimeHandling       = (dtDateTime, dtBDETimeStamp);

//-------------------------------------

  PDateTime = ^TDateTime;
{$ifdef FPC_VERSION}
  TDateTimeAlias = type TDateTime;
  TDateTimeRec = record
    case TFieldType of
      ftDate: (Date: Longint);
      ftTime: (Time: Longint);
      ftDateTime: (DateTime: TDateTimeAlias);
  end;
{$else}
  PtrInt = Longint;
{$endif}

  PSmallInt = ^SmallInt;
  PCardinal = ^Cardinal;
//  PDouble = ^Double;
  PString = ^String;
  PDateTimeRec = ^TDateTimeRec;

{$ifdef SUPPORT_INT64}
  PLargeInt = ^Int64;
{$endif}
{$ifdef DELPHI_3}
  dword = cardinal;
{$endif}

{$ifdef SUPPORT_INT64}
  TSequentialRecNo = Int64;
{$else}
  TSequentialRecNo = Integer;
{$endif}

//-------------------------------------

{$ifndef SUPPORT_FREEANDNIL}
// some procedures for the less lucky who don't have newer versions yet :-)
procedure FreeAndNil(var v);
{$endif}
procedure FreeMemAndNil(var P: Pointer);

{$ifndef SUPPORT_CHARINSET}
function CharInSet(C: AnsiChar; const CharSet: TSysCharSet): Boolean;
{$endif SUPPORT_CHARINSET}

//-------------------------------------

{$ifndef SUPPORT_PATHDELIM}
const
{$ifdef WINDOWS}
  PathDelim = '\';
{$else}
  PathDelim = '/';
{$endif}
{$endif}

{$ifndef SUPPORT_INCLTRAILPATHDELIM}
function IncludeTrailingPathDelimiter(const Path: string): string;
{$endif SUPPORT_INCLTRAILPATHDELIM}

{$ifdef SUPPORT_FORMATSETTINGS}
function TwoDigitYearCenturyWindow: word;
function DecimalSeparator: char;
{$endif SUPPORT_FORMATSETTINGS}

//-------------------------------------

function GetCompletePath(const Base, Path: string): string;
function GetCompleteFileName(const Base, FileName: string): string;
function IsFullFilePath(const Path: string): Boolean; // full means not relative
function DateTimeToBDETimeStamp(aDT: TDateTime): double;
function BDETimeStampToDateTime(aBT: double): TDateTime;
procedure FindNextName(BaseName: string; var OutName: string; var Modifier: Integer);
{$ifdef USE_CACHE}
function GetFreeMemory: Integer;
{$endif}

function SwapWordBE(const Value: word): word;
function SwapWordLE(const Value: word): word;
function SwapIntBE(const Value: dword): dword;
function SwapIntLE(const Value: dword): dword;
{$ifdef SUPPORT_INT64}
procedure SwapInt64BE(Value, Result: Pointer); register;
procedure SwapInt64LE(Value, Result: Pointer); register;
{$endif}

function TranslateString(FromCP, ToCP: Cardinal; Src, Dest: PAnsiChar; Length: Integer): Integer;

// Returns a pointer to the first occurence of Chr in Str within the first Length characters
// Does not stop at null (#0) terminator!
function MemScan(const Buffer: Pointer; Chr: Byte; Length: Integer): Pointer;

// Delphi 3 does not have a Min function
{$ifdef DELPHI_3}
{$ifndef DELPHI_4}
function Min(x, y: integer): integer;
function Max(x, y: integer): integer;
{$endif}
{$endif}

{$ifndef DELPHI_7}
type
  PPAnsiChar = ^PAnsiChar;
  PDouble = ^Double;
{$ENDIF}

implementation

{$ifdef WINDOWS}
uses
  dbf_ansistrings,
  Windows;
{$else}
uses
  dbf_ansistrings;
{$endif}

//====================================================================

function DbfVersionString: string;
begin
  Result := Format('TDbf %d.%d', [TDBF_MAJOR_VERSION, TDBF_MINOR_VERSION]);
  if TDBF_SUB_MINOR_VERSION <> 0 then
    {%H-}Result := Result + Format('.%d', [TDBF_SUB_MINOR_VERSION]);
end;

//====================================================================

function GetCompletePath(const Base, Path: string): string;
begin
  if IsFullFilePath(Path)
  then begin
    Result := Path;
  end else begin
    if Length(Base) > 0 then
      Result := ExpandFileName(IncludeTrailingPathDelimiter(Base) + Path)
    else
      Result := ExpandFileName(Path);
  end;

  // add last backslash if not present
  if Length(Result) > 0 then
    Result := IncludeTrailingPathDelimiter(Result);
end;

function IsFullFilePath(const Path: string): Boolean; // full means not relative
begin
{$ifdef WINDOWS}
  Result := Length(Path) > 1;
  if Result then
    // check for 'x:' or '\\' at start of path
    Result := ((Path[2]=':') and CharInSet(UpCase(Path[1]), ['A'..'Z']))
      or ((Path[1]='\') and (Path[2]='\'));
{$else}  // Linux
  Result := Length(Path) > 0;
  if Result then
    Result := Path[1]='/';
 {$endif}
end;

//====================================================================

function GetCompleteFileName(const Base, FileName: string): string;
var
  lpath: string;
  lfile: string;
begin
  lpath := GetCompletePath(Base, ExtractFilePath(FileName));
  lfile := ExtractFileName(FileName);
  lpath := lpath + lfile;
  result := lpath;
end;

// it seems there is no pascal function to convert an integer into a PAnsiChar???
// NOTE: in dbf_dbffile.pas there is also a convert routine, but is slightly different

function GetStrFromInt(Val: Integer; const Dst: PAnsiChar): Integer; // Was PChar
var
  Temp: array[0..10] of AnsiChar; // Was Char
  I, J: Integer;
begin
  Val := Abs(Val);
  // we'll have to store characters backwards first
  I := 0;
  J := 0;
  repeat
    Temp[I] := AnsiChar((Val mod 10) + Ord('0')); // Was Chr
    Val := Val div 10;
    Inc(I);
  until Val = 0;

  // remember number of digits
  Result := I;
  // copy value, remember: stored backwards
  repeat
    Dst[J] := Temp[I-1];
    Inc(J);
    Dec(I);
  until I = 0;
  // done!
end;

{$ifdef SUPPORT_INT64}

function GetStrFromInt64(Val: Int64; const Dst: PAnsiChar): Integer; // Was PChar
var
  Temp: array[0..19] of AnsiChar; // Was Char
  I, J: Integer;
begin
  Val := Abs(Val);
  // we'll have to store characters backwards first
  I := 0;
  J := 0;
  repeat
    Temp[I] := AnsiChar((Val mod 10) + Ord('0')); // Was Chr
    Val := Val div 10;
    Inc(I);
  until Val = 0;

  // remember number of digits
  Result := I;
  // copy value, remember: stored backwards
  repeat
    Dst[J] := Temp[I-1];
    inc(J);
    dec(I);
  until I = 0;
  // done!
end;

{$endif}

function DateTimeToBDETimeStamp(aDT: TDateTime): double;
var
  aTS: TTimeStamp;
begin
  aTS := DateTimeToTimeStamp(aDT);
  Result := TimeStampToMSecs(aTS);
end;

function BDETimeStampToDateTime(aBT: double): TDateTime;
var
  aTS: TTimeStamp;
begin
  aTS := MSecsToTimeStamp(Round(aBT));
  Result := TimeStampToDateTime(aTS);
end;

//====================================================================

{$ifndef SUPPORT_FREEANDNIL}

procedure FreeAndNil(var v);
var
  Temp: TObject;
begin
  Temp := TObject(v);
  TObject(v) := nil;
  Temp.Free;
end;

{$endif}

procedure FreeMemAndNil(var P: Pointer);
var
  Temp: Pointer;
begin
  Temp := P;
  P := nil;
  FreeMem(Temp);
end;

{$ifndef SUPPORT_CHARINSET}
function CharInSet(C: AnsiChar; const CharSet: TSysCharSet): Boolean;
begin
  Result := (c in charset)
end;
{$endif SUPPORT_CHARINSET}

//====================================================================

{$ifndef SUPPORT_INCLTRAILPATHDELIM}
{$ifdef SUPPORT_INCLTRAILBACKSLASH}

function IncludeTrailingPathDelimiter(const Path: string): string;
begin
{$ifdef WINDOWS}
  Result := IncludeTrailingBackslash(Path);
{$else}
  Result := IncludeTrailingSlash(Path);
{$endif}
end;

{$else}

function IncludeTrailingPathDelimiter(const Path: string): string;
var
  len: Integer;
begin
  Result := Path;
  len := Length(Result);
  if len = 0 then
    Result := PathDelim
  else
  if Result[len] <> PathDelim then
    Result := Result + PathDelim;
end;

{$endif}
{$endif}

function TwoDigitYearCenturyWindow: word;
begin
{$ifdef SUPPORT_FORMATSETTINGS}
  Result := FormatSettings.TwoDigitYearCenturyWindow;
{$else SUPPORT_FORMATSETTINGS}
  {$ifdef SUPPORT_TWODIGITYEARCENTURYWINDOW}
    Result := SysUtils.TwoDigitYearCenturyWindow;
  {$else SUPPORT_TWODIGITYEARCENTURYWINDOW}
    // Delphi 3 standard-behavior no change possible
    Result := 0;
  {$endif SUPPORT_TWODIGITYEARCENTURYWINDOW}
{$endif SUPPORT_FORMATSETTINGS}
end;

function DecimalSeparator: char;
begin
{$ifdef SUPPORT_FORMATSETTINGS}
  Result := FormatSettings.DecimalSeparator;
{$else SUPPORT_FORMATSETTINGS}
  Result := SysUtils.DecimalSeparator{%H-};
{$endif SUPPORT_FORMATSETTINGS}
end;

{$ifdef USE_CACHE}

function GetFreeMemory: Integer;
var
  MemStatus: TMemoryStatus;
begin
  GlobalMemoryStatus(MemStatus);
  Result := MemStatus.dwAvailPhys;
end;

{$endif}

//====================================================================
// Utility routines
//====================================================================

{$ifdef ENDIAN_LITTLE}
function SwapWordBE(const Value: word): word;
{$else}
function SwapWordLE(const Value: word): word;
{$endif}
begin
  Result := ((Value and $FF) shl 8) or ((Value shr 8) and $FF);
end;

{$ifdef ENDIAN_LITTLE}
function SwapWordLE(const Value: word): word;
{$else}
function SwapWordBE(const Value: word): word;
{$endif}
begin
  Result := Value;
end;

{$ifdef FPC}

function SwapIntBE(const Value: dword): dword;
begin
  Result := BEtoN(Value);
end;

function SwapIntLE(const Value: dword): dword;
begin
  Result := LEtoN(Value);
end;

procedure SwapInt64BE(Value, Result: Pointer);
begin
  PInt64(Result)^ := BEtoN(PInt64(Value)^);
end;

procedure SwapInt64LE(Value, Result: Pointer);
begin
  PInt64(Result)^ := LEtoN(PInt64(Value)^);
end;

{$else}
{$ifdef USE_ASSEMBLER_486_UP}

function SwapIntBE(const Value: dword): dword; register; assembler;
asm
  BSWAP EAX;
end;

procedure SwapInt64BE(Value {EAX}, Result {EDX}: Pointer); register; assembler;
asm
  MOV ECX, dword ptr [EAX] 
  MOV EAX, dword ptr [EAX + 4] 
  BSWAP ECX 
  BSWAP EAX 
  MOV dword ptr [EDX+4], ECX 
  MOV dword ptr [EDX], EAX 
end;

{$else}

function SwapIntBE(const Value: Cardinal): Cardinal;
begin
  PByteArray(@Result)[0] := PByteArray(@Value)[3];
  PByteArray(@Result)[1] := PByteArray(@Value)[2];
  PByteArray(@Result)[2] := PByteArray(@Value)[1];
  PByteArray(@Result)[3] := PByteArray(@Value)[0];
end;

procedure SwapInt64BE(Value, Result: Pointer); register;
var
  PtrResult: PByteArray;
  PtrSource: PByteArray;
begin
  // temporary storage is actually not needed, but otherwise compiler crashes (?)
  PtrResult := PByteArray(Result);
  PtrSource := PByteArray(Value);
  PtrResult[0] := PtrSource[7];
  PtrResult[1] := PtrSource[6];
  PtrResult[2] := PtrSource[5];
  PtrResult[3] := PtrSource[4];
  PtrResult[4] := PtrSource[3];
  PtrResult[5] := PtrSource[2];
  PtrResult[6] := PtrSource[1];
  PtrResult[7] := PtrSource[0];
end;

{$endif}

function SwapIntLE(const Value: dword): dword;
begin
  Result := Value;
end;

{$ifdef SUPPORT_INT64}

procedure SwapInt64LE(Value, Result: Pointer);
begin
  PInt64(Result)^ := PInt64(Value)^;
end;

{$endif}

{$endif}

function TranslateString(FromCP, ToCP: Cardinal; Src, Dest: PAnsiChar; Length: Integer): Integer;
var
  WideCharStr: array[0..1023] of WideChar;
  wideBytes: Cardinal;
begin
  if Length = -1 then
    Length := dbfStrLen(Src);
  Result := Length;
  if (FromCP = GetOEMCP) and (ToCP = GetACP) then
  begin
    {$IFDEF WINAPI_IS_UNICODE}   // Rafal Chlopek (14-03-2010):  I've commented DELPHI_2010
    OemToCharBuffA(Src, Dest, Length) // Was OemToCharBuff with PChar(Dest) cast
    {$ELSE}
    OemToCharBuff(Src, Dest, Length)
    {$ENDIF}
  end
  else
  if (FromCP = GetACP) and (ToCP = GetOEMCP) then
  begin
    {$IFDEF WINAPI_IS_UNICODE}
    CharToOemBuffA(Src, Dest, Length) // Was OemToCharBuff with PChar(Src) cast
    {$ELSE}
    CharToOemBuff(Src, Dest, Length)
    {$ENDIF}
  end
  else
  if FromCP = ToCP then
  begin
    if Src <> Dest then
      Move(Src^, Dest^, Length);
  end else begin
    // does this work on Win95/98/ME?
    wideBytes := MultiByteToWideChar(FromCP, MB_PRECOMPOSED, Src, Length, LPWSTR(@WideCharStr[0]), 1024);
    Result := WideCharToMultiByte(ToCP, 0, LPWSTR(@WideCharStr[0]), wideBytes, Dest, Length, nil, nil);
  end;
end;

procedure FindNextName(BaseName: string; var OutName: string; var Modifier: Integer);
var
  Extension: string;
begin
  Extension := ExtractFileExt(BaseName);
  BaseName := Copy(BaseName, 1, Length(BaseName)-Length(Extension));
  repeat
    Inc(Modifier);
    OutName := BaseName+'_'+IntToStr(Modifier) + Extension;
  until not FileExists(OutName);
end;

{$ifdef FPC}

function MemScan(const Buffer: Pointer; Chr: Byte; Length: Integer): Pointer;
var
  I: Integer;
begin
  I := System.IndexByte(Buffer, Length, Chr);
  if I = -1 then
    Result := nil
  else
    Result := Buffer+I;
end;

{$else}

{$ifdef USE_ASSEMBLER_486_UP}

function MemScan(const Buffer: Pointer; Chr: Byte; Length: Integer): Pointer;
asm
        PUSH    EDI
        MOV     EDI,Buffer
        MOV     AL, Chr
        MOV     ECX,Length
        REPNE   SCASB
        MOV     EAX,0
        JNE     @@1
        MOV     EAX,EDI
        DEC     EAX
@@1:    POP     EDI
end;
{$else}

// lsp: Pure Pascal implementation for x64 on Delphi XE2 and up
function MemScan(const Buffer: Pointer; Chr: Byte; Length: Integer): Pointer;
var
  p: PByte;
begin
  p := Buffer;

  while ( (Length>0) and (p^ <> Chr) ) do
  begin
    Inc(p);
    Dec(Length);
  end;

  if Length>0 then
    Result := p
  else
    Result := nil;
end;

{$ENDIF USE_ASSEMBLER_486_UP}

{$endif FPC}


{$ifdef DELPHI_3}
{$ifndef DELPHI_4}

function Min(x, y: integer): integer;
begin
  if x < y then
    result := x
  else
    result := y;
end;

function Max(x, y: integer): integer;
begin
  if x < y then
    result := y
  else
    result := x;
end;

{$endif}
{$endif}

end.



