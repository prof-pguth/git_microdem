unit dbf_prssupp;

// parse support

{$BOOLEVAL OFF}

{$I dbf_common.inc}

interface

uses
  Classes;

type

  {TOCollection interfaces between OWL TCollection and VCL TList}

  TOCollection = class(TList)
  public
    procedure AtFree(Index: Integer);
    procedure FreeAll;
    procedure DoFree(Item: Pointer);
    procedure FreeItem(Item: Pointer); virtual;
    destructor Destroy; override;
  end;

  TNoOwnerCollection = class(TOCollection)
  public
    procedure FreeItem({%H-}Item: Pointer); override;
  end;

  { TSortedCollection object }

  TSortedCollection = class(TOCollection)
  public
    function Compare(Key1, Key2: Pointer): Integer; virtual; abstract;
    function IndexOf(Item: Pointer): Integer; virtual;
    procedure Add(Item: Pointer); virtual;
    procedure AddReplace(Item: Pointer); virtual;
    procedure AddList(Source: TList; FromIndex, ToIndex: Integer);
    {if duplicate then replace the duplicate else add}
    function KeyOf(Item: Pointer): Pointer; virtual;
    function Search(Key: Pointer; var Index: Integer): Boolean; virtual;
  end;

  { TStrCollection object }

  TStrCollection = class(TSortedCollection)
  public
    function Compare(Key1, Key2: Pointer): Integer; override;
    procedure FreeItem(Item: Pointer); override;
  end;

const
  DBF_POSITIVESIGN = '+';
  DBF_NEGATIVESIGN = '-';
  DBF_DECIMAL = '.';
  DBF_DECIMAL_ALTERNATE = ',';
  DBF_EXPSIGN = 'E';
  DBF_ZERO = '0';
  DBF_NINE = '9';


function IntToStrWidth(Val: {$ifdef SUPPORT_INT64}Int64{$else}Integer{$endif}; const FieldSize: Integer; const Dest: PAnsiChar; Pad: Boolean; PadChar: AnsiChar): Integer;
function FloatToStrWidth(const Val: Extended; const FieldSize, FieldPrec: Integer; const Dest: PAnsiChar; Pad: Boolean): Integer;

function StrToIntWidth(var IntValue: {$ifdef SUPPORT_INT64}Int64{$else}Integer{$endif}; Src: Pointer; Size: Integer; Default: Integer): Boolean;
function StrToInt32Width(var IntValue: Integer; Src: Pointer; Size: Integer; Default: Integer): Boolean;
function StrToFloatWidth(var FloatValue: Extended; const Src: PAnsiChar; const Size: Integer; Default: Extended): Boolean;

implementation

uses
  SysUtils,
  dbf_ansistrings;

destructor TOCollection.Destroy;
begin
  FreeAll;
  inherited Destroy;
end;

procedure TOCollection.AtFree(Index: Integer);
var
  Item: Pointer;
begin
  Item := Items[Index];
  Delete(Index);
  FreeItem(Item);
end;


procedure TOCollection.FreeAll;
var
  I: Integer;
begin
  try
    for I := 0 to Count - 1 do
      FreeItem(Items[I]);
  finally
    Count := 0;
  end;
end;

procedure TOCollection.DoFree(Item: Pointer);
begin
  AtFree(IndexOf(Item));
end;

procedure TOCollection.FreeItem(Item: Pointer);
begin
  if (Item <> nil) then
    with TObject(Item) as TObject do
      Free;
end;

{----------------------------------------------------------------virtual;
  Implementing TNoOwnerCollection
  -----------------------------------------------------------------}

procedure TNoOwnerCollection.FreeItem(Item: Pointer);
begin
end;

{ TSortedCollection }

function TSortedCollection.IndexOf(Item: Pointer): Integer;
var
  I: Integer;
begin
  IndexOf := -1;
  I := -1;
  if Search(KeyOf(Item), I) then
  begin
    while (I < Count) and (Item <> Items[I]) do
      Inc(I);
    if I < Count then IndexOf := I;
  end;
end;

procedure TSortedCollection.AddReplace(Item: Pointer);
var
  Index: Integer;
begin
  Index := -1;
  if Search(KeyOf(Item), Index) then
    Delete(Index);
  Add(Item);
end;

procedure TSortedCollection.Add(Item: Pointer);
var
  I: Integer;
begin
  I := -1;
  Search(KeyOf(Item), I);
  Insert(I, Item);
end;

procedure TSortedCollection.AddList(Source: TList; FromIndex, ToIndex: Integer);
var
  I: Integer;
begin
  for I := FromIndex to ToIndex do
    Add(Source.Items[I]);
end;

function TSortedCollection.KeyOf(Item: Pointer): Pointer;
begin
  Result := Item;
end;

function TSortedCollection.Search(Key: Pointer; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := false;
  L := 0;
  H := Count - 1;
  while L <= H do
  begin
    I := (L + H) div 2;
    C := Compare(KeyOf(Items[I]), Key);
    if C < 0 then
      L := I + 1
    else begin
      H := I - 1;
      Result := C = 0;
    end;
  end;
  Index := L;
end;

{ TStrCollection }

function TStrCollection.Compare(Key1, Key2: Pointer): Integer;
begin
  Compare := StrComp(PChar(Key1), PChar(Key2));
end;

procedure TStrCollection.FreeItem(Item: Pointer);
begin
  StrDispose(PChar(Item));
end;

{$IFDEF SUPPORT_FORMATSETTINGSTYPE}
var
  DbfFormatSettings: TFormatSettings;
{$ENDIF}

type
  TFloatResult = record
    Dest: PAnsiChar;
    P: PAnsiChar;
    FieldSize: Integer;
    FieldPrec: Integer;
    Len: Integer;
  end;

procedure FloatPutChar(var FloatResult: TFloatResult; C: AnsiChar);
begin
  Inc(FloatResult.Len);
  if FloatResult.Len <= FloatResult.FieldSize then
  begin
    FloatResult.P^ := C;
    Inc(FloatResult.P);
  end;
end;

procedure FloatReset(var FloatResult: TFloatResult);
begin
  FloatResult.P := FloatResult.Dest;
  FloatResult.Len := 0;
end;

procedure DecimalToDbfStr(var FloatResult: TFloatResult; const FloatRec: TFloatRec; Exponent: SmallInt; FieldPrec: Integer);
var
  Digit: SmallInt;
  DigitCount: SmallInt;
  DigitMin: SmallInt;
  DigitMax: SmallInt;
  DigitChar: AnsiChar;
  DecCount: Integer;
begin
  FloatReset(FloatResult);
  if FloatRec.Negative then
    FloatPutChar(FloatResult, DBF_NEGATIVESIGN);
  DigitCount := dbfStrLen(@FloatRec.Digits[0]);
  if Exponent <= 0 then
  begin
    DigitMin := Exponent;
    FloatPutChar(FloatResult, DBF_ZERO);
  end
  else
    DigitMin := Low(FloatRec.Digits);
  if Exponent > DigitCount then
    DigitMax := Exponent
  else
    DigitMax := DigitCount;
  Digit := DigitMin;
  DecCount := -1;
  while (Digit < DigitMax) or ((FieldPrec <> 0) and (DecCount < FieldPrec) and (FloatResult.Len < FloatResult.FieldSize - Ord(DecCount<0))) do
  begin
    if (Digit >= 0) and (Digit < DigitCount) then
      DigitChar := AnsiChar(FloatRec.Digits[Digit])
    else
      DigitChar := DBF_ZERO;
    if Digit=Exponent then
    begin
      FloatPutChar(FloatResult, DBF_DECIMAL);
      DecCount := 0;
    end;
    FloatPutChar(FloatResult, DigitChar);
    Inc(Digit);
    if DecCount >= 0 then
      Inc(DecCount);
  end;
end;

procedure DecimalToDbfStrFormat(var FloatResult: TFloatResult; const FloatRec: TFloatRec; Format: TFloatFormat; FieldPrec: Integer);
var
  Exponent: SmallInt;
  ExponentBuffer: array[1..5] of AnsiChar;
  Index: Byte;
begin
  if Format=ffExponent then
  begin
    DecimalToDbfStr(FloatResult, FloatRec, 1, 0);
    Exponent:= Pred(FloatRec.Exponent);
    if Exponent<>0 then
    begin
      FloatPutChar(FloatResult, DBF_EXPSIGN);
      if Exponent<0 then
      begin
        FloatPutChar(FloatResult, DBF_NEGATIVESIGN);
        Exponent:= -Exponent;
      end;
      Index:= 0;
      while Exponent<>0 do
      begin
        Inc(Index);
        ExponentBuffer[Index] := AnsiChar(Ord(DBF_ZERO) + (Exponent mod 10));
        Exponent := Exponent div 10;
      end;
      while Index>0 do
      begin
        FloatPutChar(FloatResult, ExponentBuffer[Index]);
        Dec(Index);
      end;
    end;
  end
  else
    DecimalToDbfStr(FloatResult, FloatRec, FloatRec.Exponent, FieldPrec);
end;

procedure FloatToDbfStrFormat(var FloatResult: TFloatResult; const FloatRec: TFloatRec; Format: TFloatFormat; FieldPrec: Integer; FloatValue: Extended);
var
  FloatRec2: TFloatRec;
  Precision: Integer;
begin
  DecimalToDbfStrFormat(FloatResult, FloatRec, Format, FieldPrec);
  Precision:= Integer(dbfStrLen(@FloatRec.Digits[0]));
  if FloatResult.Len > FloatResult.FieldSize then
  begin
    Precision:= Precision - (FloatResult.Len - FloatResult.FieldSize);
    if FloatRec.Exponent = FloatResult.FieldSize-Ord(FloatRec.Negative) then
      Inc(Precision);
    if Precision>0 then
    begin
      FloatToDecimal(FloatRec2, FloatValue, fvExtended, Precision, FieldPrec);
      DecimalToDbfStrFormat(FloatResult, FloatRec2, Format, FieldPrec);
      if FloatResult.Len > FloatResult.FieldSize then
        FloatResult.Len := 0;
    end
    else
       FloatResult.Len := 0;
  end;
end;

function NumberPad(const FloatResult: TFloatResult; const Dest: PAnsiChar; Pad: Boolean; PadChar: AnsiChar): Integer;
begin
  Result:= FloatResult.Len;
  if Pad and (FloatResult.Len <> FloatResult.FieldSize) then
  begin
    Move(Dest^, (Dest+FloatResult.FieldSize-FloatResult.Len)^, FloatResult.Len);
    FillChar(Dest^, FloatResult.FieldSize-FloatResult.Len, PadChar);
    Result:= FloatResult.FieldSize;
  end;
end;

function IntToStrWidth(Val: {$ifdef SUPPORT_INT64}Int64{$else}Integer{$endif}; const FieldSize: Integer; const Dest: PAnsiChar; Pad: Boolean; PadChar: AnsiChar): Integer;
var
  FloatResult: TFloatResult;
  Negative: Boolean;
  IntValue: {$ifdef SUPPORT_INT64}Int64{$else}Integer{$endif};
  Buffer: array[0..{$ifdef SUPPORT_INT64}18{$else}9{$endif}] of AnsiChar;
  P: PAnsiChar;
begin
  FillChar(Buffer{%H-}, SizeOf(Buffer), 0);
  FillChar(FloatResult{%H-}, SizeOf(FloatResult), 0);
  FloatResult.Dest := Buffer;
  FloatResult.FieldSize := FieldSize;
  FloatReset(FloatResult);
  Negative := Val<0;
  if Negative then
    IntValue := -Val
  else
    IntValue := Val;
  repeat
    FloatPutChar(FloatResult, AnsiChar(Ord(DBF_ZERO) + (IntValue mod 10)));
    IntValue := IntValue div 10;
  until IntValue = 0;
  P := FloatResult.P;
  FloatResult.Dest := Dest;
  if FloatResult.Len+Ord(Negative) > FieldSize then
  begin
    if PadChar<>DBF_ZERO then
      FloatResult.Len := FloatToStrWidth(Val, FieldSize, 0, Dest, Pad)
    else
      FloatResult.Len := 0;
  end
  else
  begin
    FloatReset(FloatResult);
    if Negative then
      FloatPutChar(FloatResult, DBF_NEGATIVESIGN);
    repeat
      Dec(P);
      FloatPutChar(FloatResult, P^);
    until P=Buffer;
  end;
  Result:= NumberPad(FloatResult, Dest, Pad, PadChar);
end;

function Int64ToStrWidth(Val: Int64; const FieldSize: Integer; const Dest: PAnsiChar; Pad: Boolean; PadChar: AnsiChar): Integer;
begin
  Result:= IntToStrWidth(Val, FieldSize, Dest, Pad, PadChar);
end;

function FloatToStrWidth(const Val: Extended; const FieldSize, FieldPrec: Integer; const Dest: PAnsiChar; Pad: Boolean): Integer;
var
  FloatResult: TFloatResult;
  FloatRec: TFloatRec;
begin
  FillChar(FloatResult{%H-}, SizeOf(FloatResult), 0);
  FloatResult.Dest := Dest;
  FloatResult.FieldSize := FieldSize;
  FloatToDecimal(FloatRec, Val, fvExtended, 15, FieldPrec);
  if FloatRec.Exponent <= 15 then
    FloatToDbfStrFormat(FloatResult, FloatRec, ffFixed, FieldPrec, Val);
  if FloatResult.Len = 0 then
    FloatToDbfStrFormat(FloatResult, FloatRec, ffExponent, FieldPrec, Val);
  Result:= NumberPad(FloatResult, Dest, Pad, ' ');
end;

function StrToIntWidth(var IntValue: {$ifdef SUPPORT_INT64}Int64{$else}Integer{$endif}; Src: Pointer; Size: Integer; Default: Integer): Boolean;
var
  P: PAnsiChar;
  Negative: Boolean;
  Digit: Byte;
  FloatValue: Extended;
begin
  P := Src;
  while (P < PAnsiChar(Src) + Size) and (P^ = ' ') do
    Inc(P);
  Dec(Size, P - Src);
  Src := P;
  Result := Size <> 0;
  if Result then
  begin
    IntValue := 0;
    Negative := False;
    case P^ of
      DBF_POSITIVESIGN: Inc(P);
      DBF_NEGATIVESIGN:
      begin
        Negative := True;
        Inc(P);
      end;
    end;
    repeat
      if P^ in [DBF_ZERO..DBF_NINE] then
      begin
        Digit := Ord(P^) - Ord(DBF_ZERO);
        if IntValue < 0 then
          Result := IntValue >= (Low(IntValue) + Digit) div 10
        else
          Result := IntValue <= (High(IntValue) - Digit) div 10;
        if Result then
          IntValue := IntValue * 10;
        if IntValue >= 0 then
          Inc(IntValue, Digit)
        else
          Dec(IntValue, Digit);
        if Negative and (IntValue <>0) then
        begin
          IntValue := -IntValue;
          Negative := False;
        end;
      end
      else
        Result := False;
      Inc(P);
    until (P = PAnsiChar(Src) + Size) or (not Result);
    if not Result then
    begin
      FloatValue := 0;
      Result := StrToFloatWidth(FloatValue, Src, Size, Default);
      if Result then
        IntValue:= Round(FloatValue);
    end;
    if not Result then
      IntValue := Default;
  end;
end;

function StrToInt32Width(var IntValue: Integer; Src: Pointer; Size: Integer; Default: Integer): Boolean;
{$ifdef SUPPORT_INT64}
var
  AIntValue: Int64;
begin
  AIntValue := 0;
  Result := StrToIntWidth(AIntValue, Src, Size, Default);
  if Result then
  begin
    Result := (AIntValue >= Low(IntValue)) and (AIntValue <= High(IntValue));
    if Result then
      IntValue := AIntValue
    else
      IntValue := Default;
  end;
{$else}
begin
  Result := StrToIntWidth(IntValue, Src, Size, Default);
{$endif}
end;

function StrToFloatWidth(var FloatValue: Extended; const Src: PAnsiChar; const Size: Integer; Default: Extended): Boolean;
var
  Buffer: array[0..20] of AnsiChar;
  i: Integer;
begin
  Result := Size < SizeOf(Buffer);
  if Result then
  begin
    FillChar(Buffer{%H-}, SizeOf(Buffer), 0);
    Move(Src^, Buffer, Size);
    Buffer[Size] := #0;
    // Normalize decimal separator
    for i:=0 to Size-1 do
      if (Buffer[i]=DBF_DECIMAL) or (Buffer[i]=DBF_DECIMAL_ALTERNATE) then
      begin
        Buffer[i] := {$ifdef SUPPORT_FORMATSETTINGSTYPE}DBF_DECIMAL{$else}DecimalSeparator{$endif};
        Break;
      end;
{$ifdef SUPPORT_FORMATSETTINGSTYPE}
    Result := dbfTextToFloatFmt(@Buffer, FloatValue, fvExtended, DbfFormatSettings);
{$else}
    Result := dbfTextToFloat(@Buffer[0], FloatValue, fvExtended);
{$endif}
  end;
  if not Result then
    FloatValue := Default;
end;

initialization
{$IFDEF SUPPORT_FORMATSETTINGSTYPE}
  FillChar(DbfFormatSettings{%H-}, SizeOf(DbfFormatSettings), 0);
  DbfFormatSettings.DecimalSeparator:= DBF_DECIMAL;
{$ENDIF}

end.

