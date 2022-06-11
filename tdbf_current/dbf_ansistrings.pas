unit dbf_ansistrings;

{$I dbf_common.inc}

interface

uses
  SysUtils;

type
{$IFDEF FPC}
  TdbfStrLen = function(Str: PAnsiChar): SizeInt;
  TdbfStrCopy = function(Dest: PAnsiChar; Source: PAnsiChar): PChar;
  TdbfStrLCopy = function(Dest: PAnsiChar; Source: PAnsiChar; MaxLen: SizeInt): PChar;
  TdbfFloatToText = function(BufferArg: PAnsiChar; Value: Extended;
    Format: TFloatFormat; Precision: Integer; Digits: Integer): LongInt;
{$IFDEF SUPPORT_FORMATSETTINGSTYPE}
  TdbfFloatToTextFmt = function(BufferArg: PAnsiChar; Value: Extended;
    Format: TFloatFormat; Precision: Integer; Digits: Integer; const FormatSettings: TFormatSettings): LongInt;
{$ENDIF}
  TdbfStrUpper = function(Str: PAnsiChar): PAnsiChar;
  TdbfStrLower = function(Str: PAnsiChar): PAnsiChar;
  TdbfStrIComp = function(S1, S2: PAnsiChar): SizeInt;
  TdbfStrLIComp = function(S1, S2: PAnsiChar; MaxLen: SizeInt): SizeInt;
  TdbfStrPos = function(Str, SubStr: PAnsiChar): PAnsiChar;
  TdbfStrLComp = function(S1, S2: PAnsiChar; MaxLen: SizeInt): SizeInt;
  TdbfStrComp = function(S1, S2: PAnsiChar): SizeInt;
  TdbfStrScan = function(Str: PAnsiChar; Chr: AnsiChar): PAnsiChar;
  TdbfTextToFloat = function(Buffer: PAnsiChar; out Value; ValueType: TFloatValue): Boolean;
{$IFDEF SUPPORT_FORMATSETTINGSTYPE}
  TdbfTextToFloatFmt = function(Buffer: PAnsiChar; out Value; ValueType: TFloatValue; const FormatSettings: TFormatSettings): Boolean;
{$ENDIF}
  TdbfStrPLCopy = function(Dest: PAnsiChar; const Source: string; MaxLen: SizeUInt): PAnsiChar;
  TdbfTrimLeft = function(const S: string): string;
  TdbfTrimRight = function(const S: string): string;
{$ELSE}
  TdbfStrLen = function(const Str: PAnsiChar): Cardinal;
  TdbfStrCopy = function(Dest: PAnsiChar; const Source: PAnsiChar): PAnsiChar;
  TdbfStrLCopy = function(Dest: PAnsiChar; const Source: PAnsiChar; MaxLen: Cardinal): PAnsiChar;
  TdbfFloatToText = function(BufferArg: PAnsiChar; const Value; ValueType: TFloatValue;
    Format: TFloatFormat; Precision, Digits: Integer): Integer;
{$IFDEF SUPPORT_FORMATSETTINGSTYPE}
  TdbfFloatToTextFmt = function(BufferArg: PAnsiChar; const Value; ValueType: TFloatValue;
    Format: TFloatFormat; Precision, Digits: Integer; const FormatSettings: TFormatSettings): Integer;
{$ENDIF}
  TdbfStrUpper = function(Str: PAnsiChar): PAnsiChar;
  TdbfStrLower = function(Str: PAnsiChar): PAnsiChar;
  TdbfStrIComp = function(const S1, S2: PAnsiChar): Integer;
  TdbfStrLIComp = function(const S1, S2: PAnsiChar; MaxLen: Cardinal): Integer;
  TdbfStrPos = function(const Str, SubStr: PAnsiChar): PAnsiChar;
  TdbfStrLComp = function(const S1, S2: PAnsiChar; MaxLen: Cardinal): Integer;
  TdbfStrComp = function(const S1, S2: PAnsiChar): Integer;
  TdbfStrScan = function(const Str: PAnsiChar; Chr: AnsiChar): PAnsiChar;
  TdbfTextToFloat = function(Buffer: PAnsiChar; var Value; ValueType: TFloatValue): Boolean;
{$IFDEF SUPPORT_FORMATSETTINGSTYPE}  
  TdbfTextToFloatFmt = function(Buffer: PAnsiChar; var Value; ValueType: TFloatValue; const FormatSettings: TFormatSettings): Boolean;
{$ENDIF}  
  TdbfStrPLCopy = function(Dest: PAnsiChar; const Source: AnsiString; MaxLen: Cardinal): PAnsiChar;
  TdbfTrimLeft = function(const S: AnsiString): AnsiString;
  TdbfTrimRight = function(const S: AnsiString): AnsiString;
{$ENDIF}

var
  dbfStrLen: TdbfStrLen{ = nil};
  dbfStrCopy: TdbfStrCopy{ = nil};
  dbfStrLCopy: TdbfStrLCopy{ = nil};
  dbfFloatToText: TdbfFloatToText{ = nil};
{$IFDEF SUPPORT_FORMATSETTINGSTYPE}
  dbfFloatToTextFmt: TdbfFloatToTextFmt{ = nil};
{$ENDIF}
  dbfStrUpper: TdbfStrUpper{ = nil};
  dbfStrLower: TdbfStrLower{ = nil};
  dbfStrIComp: TdbfStrIComp{ = nil};
  dbfStrLIComp: TdbfStrLIComp{ = nil};
  dbfStrPos: TdbfStrPos{ = nil};
  dbfStrLComp: TdbfStrLComp{ = nil};
  dbfStrComp: TdbfStrComp{ = nil};
  dbfStrScan: TdbfStrScan{ = nil};
{$IFDEF SUPPORT_FORMATSETTINGSTYPE}
  dbfTextToFloatFmt: TdbfTextToFloatFmt{ = nil};
{$ENDIF}
  dbfTextToFloat: TdbfTextToFloat{ = nil};
  dbfStrPLCopy: TdbfStrPLCopy{ = nil};
  dbfTrimLeft: TdbfTrimLeft{ = nil};
  dbfTrimRight: TdbfTrimRight{ = nil};

implementation

{$IFDEF SUPPORT_ANSISTRINGS_UNIT}
uses
  AnsiStrings;  // For XE4 and up, not for FPC
{$ELSE}
// XE2 has PAnsiChar versions of TrimLeft/TrimRight in AnsiStrings,
// the other string manipulation functions are still in SysUtils.
// It is assumed that the same applies for D2009-XE3 (but this is not tested!).
{$IFDEF WINAPI_IS_UNICODE}
uses
  AnsiStrings;
{$ENDIF}
{$ENDIF}

{$IFDEF SUPPORT_ANSISTRINGS_UNIT}

procedure Init;
begin
  dbfStrLen := AnsiStrings.StrLen;
  dbfStrCopy := AnsiStrings.StrCopy;
  dbfStrLCopy := AnsiStrings.StrLCopy;
  dbfFloatToText := AnsiStrings.FloatToText;
{$IFDEF SUPPORT_FORMATSETTINGSTYPE}
  dbfFloatToTextFmt := AnsiStrings.FloatToText;
{$ENDIF}
  dbfStrUpper := AnsiStrings.StrUpper;
  dbfStrLower := AnsiStrings.StrLower;
  dbfStrIComp := AnsiStrings.StrIComp;
  dbfStrLIComp := AnsiStrings.StrLIComp;
  dbfStrPos := AnsiStrings.StrPos;
  dbfStrLComp := AnsiStrings.StrLComp;
  dbfStrComp := AnsiStrings.StrComp;
  dbfStrScan := AnsiStrings.StrScan;
{$IFDEF SUPPORT_FORMATSETTINGSTYPE}  
  dbfTextToFloatFmt := AnsiStrings.TextToFloat;
{$ENDIF}
  dbfTextToFloat := AnsiStrings.TextToFloat;
  dbfStrPLCopy := AnsiStrings.StrPLCopy;
  dbfTrimLeft := AnsiStrings.TrimLeft;
  dbfTrimRight := AnsiStrings.TrimRight;
end;
{$ELSE}

procedure Init;
begin
  dbfStrLen := SysUtils.StrLen;
  dbfStrCopy := SysUtils.StrCopy;
  dbfStrLCopy := SysUtils.StrLCopy;
  dbfFloatToText := SysUtils.FloatToText;
{$IFDEF SUPPORT_FORMATSETTINGSTYPE}
  dbfFloatToTextFmt := SysUtils.FloatToText;
{$ENDIF}
  dbfStrUpper := SysUtils.StrUpper;
  dbfStrLower := SysUtils.StrLower;
  dbfStrIComp := SysUtils.StrIComp;
  dbfStrLIComp := SysUtils.StrLIComp;
  dbfStrPos := SysUtils.StrPos;
  dbfStrLComp := SysUtils.StrLComp;
  dbfStrComp := SysUtils.StrComp;
  dbfStrScan := SysUtils.StrScan;
{$IFDEF SUPPORT_FORMATSETTINGSTYPE}  
  dbfTextToFloatFmt := SysUtils.TextToFloat;
{$ENDIF}
  dbfTextToFloat := SysUtils.TextToFloat;
  dbfStrPLCopy := SysUtils.StrPLCopy;
{$IFDEF WINAPI_IS_UNICODE}
  dbfTrimLeft := AnsiStrings.TrimLeft;
  dbfTrimRight := AnsiStrings.TrimRight;
{$ELSE}
  dbfTrimLeft := SysUtils.TrimLeft;
  dbfTrimRight := SysUtils.TrimRight;
{$ENDIF}
end;

{$ENDIF}

initialization
  Init;
end.

