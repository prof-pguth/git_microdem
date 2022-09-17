unit dbf_idxfile;

interface

{$I dbf_common.inc}

uses
{$ifdef WINDOWS}
  Windows,
{$else}
{$ifdef KYLIX}
  Libc,
{$endif}
  Types, dbf_wtil,
{$endif}
  SysUtils,
  Classes,
  db,
  dbf_pgfile,
{$ifdef USE_CACHE}
  dbf_pgcfile,
{$endif}
  dbf_parser,
  dbf_prsdef,
  dbf_collate,
  dbf_common;

{$ifdef _DEBUG}
{$define TDBF_INDEX_CHECK}
{$endif}
{$ifdef _ASSERTS}
{$define TDBF_INDEX_CHECK}
{$endif}

const
  MaxIndexes = 47;
  MaxIndexKeyLen = 100;

type
  // TDbfPointerList (almost) matches TDbfPointerList of older versions of Delphi,
  // up to at least D2009 and perhaps XE as well.
  // Delphi XE2 and up use dynamic arrays, not compatible with fixed length arrays:
  // TPointerList = array of Pointer;
  PDbfPointerList = ^TDbfPointerList;
  TDbfPointerList = array[0..(MaxInt div 16) - 1] of Pointer;

  EDbfIndexError = class(EDbfError);

  TIndexPage = class;
  TIndexTag = class;

  TIndexUpdateMode = (umAll, umCurrent);
  TLocaleError = (leNone, leUnknown, leTableIndexMismatch, leNotAvailable);
  TLocaleSolution = (lsNotOpen, lsNoEdit, lsBinary);
  TIndexUniqueType = (iuNormal, iuUnique, iuDistinct);

  TDbfLocaleErrorEvent = procedure(var Error: TLocaleError; var Solution: TLocaleSolution) of object;
  TDbfCompareKeysEvent = function(Key1, Key2: PAnsiChar): Integer of object;

  PDouble = ^Double;
  PInteger = ^Integer;

//===========================================================================
  TDbfIndexDef = class;
  TDbfIndexDef = class(TCollectionItem)
  protected
    FIndexName: string;
    FExpression: string;
    FOptions: TIndexOptions;
    FTemporary: Boolean;          // added at runtime

    procedure SetIndexName(NewName: string);
    procedure SetExpression(NewField: string);
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    property Temporary: Boolean read FTemporary write FTemporary;
    property Name: string read FIndexName write SetIndexName;
    property Expression: string read FExpression write SetExpression;
  published
    property IndexFile: string read FIndexName write SetIndexName;
    property SortField: string read FExpression write SetExpression;
    property Options: TIndexOptions read FOptions write FOptions;
  end;

  TDbfIndexParser = class(TDbfParser)
  private
    function GetKeyType: AnsiChar;
  protected
    FResultLen: Integer;

    function IsIndex: Boolean; override;
    procedure ValidateExpression(AExpression: string); override;
    function ExceptionClass: ExceptClass; override;
  public
    constructor Create(ADbfFile: Pointer); override;
    property ResultLen: Integer read FResultLen;
    property KeyType: AnsiChar read GetKeyType;
  end;
//===========================================================================
  TIndexFile = class;
  TIndexPageClass = class of TIndexPage;

  TIndexPage = class(TObject)
  protected
    FIndexFile: TIndexFile;
    FLowerPage: TIndexPage;
    FUpperPage: TIndexPage;
    FPageBuffer: Pointer;
    FEntry: Pointer;
    FEntryNo: Integer;
    FModified: Boolean;
    FPageNo: Integer;
    FWeight: TSequentialRecNo;

    // bracket props
    FLowBracket: Integer;               //  = FLowIndex if FPageNo = FLowPage
    FLowIndex: Integer;
    FLowPage: Integer;
    FLowPageTemp: Integer;
    FHighBracket: Integer;              //  = FHighIndex if FPageNo = FHighPage
    FHighIndex: Integer;
    FHighPage: Integer;
    FHighPageTemp: Integer;

    procedure LocalInsert(RecNo: Integer; Buffer: PAnsiChar; LowerPageNo: Integer);
    procedure LocalDelete;
    procedure Delete;

    procedure SyncLowerPage;
    procedure WritePage;
    procedure Split;

    function RecurPrev: Boolean;
    function RecurNext: Boolean;
    procedure RecurFirst;
    procedure RecurLast;

    procedure SetEntry(RecNo: Integer; AKey: PAnsiChar; LowerPageNo: Integer);
    procedure SetEntryNo(value: Integer);
    procedure SetPageNo(NewPageNo: Integer);
    procedure SetLowPage(NewPage: Integer);
    procedure SetHighPage(NewPage: Integer);
    procedure SetUpperPage(NewPage: TIndexPage);
    procedure UpdateBounds(IsInnerNode: Boolean);

  protected
    function GetEntry(AEntryNo: Integer): Pointer; virtual; abstract;
    function GetLowerPageNo: Integer; virtual; abstract;
    function GetKeyData: PAnsiChar; virtual; abstract;
    function GetNumEntries: Integer; virtual; abstract;
    function GetKeyDataFromEntry(AEntry: Integer): PAnsiChar; virtual; abstract;
    function GetRecNo: Integer; virtual; abstract;
    function GetIsInnerNode: Boolean; virtual; abstract;
    procedure IncNumEntries; virtual; abstract;
    procedure SetNumEntries(NewNum: Integer); virtual; abstract;
    procedure SetRecLowerPageNo(NewRecNo, NewPageNo: Integer); virtual; abstract;
    procedure SetRecLowerPageNoOfEntry(AEntry, NewRecNo, NewPageNo: Integer); virtual; abstract;
    procedure SetPrevBlock(NewBlock: Integer); virtual;
  public
    constructor Create(Parent: TIndexFile);
    destructor Destroy; override;

    function FindNearest(ARecNo: Integer): Integer;
    function PhysicalRecNo: Integer;
    function MatchKey: Integer;
    procedure GotoInsertEntry;

    procedure Clear;
    procedure GetNewPage;
    procedure Modified;
    procedure RecalcWeight;
    procedure UpdateWeight;
    procedure Flush;
    procedure SaveBracket;
    procedure RestoreBracket;

    property Key: PAnsiChar read GetKeyData;
    property Entry: Pointer read FEntry;
    property EntryNo: Integer read FEntryNo write SetEntryNo;
    property IndexFile: TIndexFile read FIndexFile;
    property UpperPage: TIndexPage read FUpperPage write SetUpperPage;
    property LowerPage: TIndexPage read FLowerPage;
    property PageBuffer: Pointer read FPageBuffer;
    property PageNo: Integer read FPageNo write SetPageNo;
    property Weight: TSequentialRecNo read FWeight;

    property NumEntries: Integer read GetNumEntries;
    property HighBracket: Integer read FHighBracket write FHighBracket;
    property HighIndex: Integer read FHighIndex;
    property HighPage: Integer read FHighPage write SetHighPage;
    property LowBracket: Integer read FLowBracket write FLowBracket;
    property LowIndex: Integer read FLowIndex;
    property LowPage: Integer read FLowPage write SetLowPage;
  end;
//===========================================================================
  TIndexTag = class(TObject)
  private
    FTag: Pointer;
  protected
    function  GetHeaderPageNo: Integer; virtual; abstract;
    function  GetTagName: string; virtual; abstract;
    function  GetKeyFormat: Byte; virtual; abstract;
    function  GetForwardTag1: Byte; virtual; abstract;
    function  GetForwardTag2: Byte; virtual; abstract;
    function  GetBackwardTag: Byte; virtual; abstract;
    function  GetReserved: Byte; virtual; abstract;
    function  GetKeyType: AnsiChar; virtual; abstract;
    procedure SetHeaderPageNo(NewPageNo: Integer); virtual; abstract;
    procedure SetTagName(NewName: string); virtual; abstract;
    procedure SetKeyFormat(NewFormat: Byte); virtual; abstract;
    procedure SetForwardTag1(NewTag: Byte); virtual; abstract;
    procedure SetForwardTag2(NewTag: Byte); virtual; abstract;
    procedure SetBackwardTag(NewTag: Byte); virtual; abstract;
    procedure SetReserved(NewReserved: Byte); virtual; abstract;
    procedure SetKeyType(NewType: AnsiChar); virtual; abstract;
  public
    property HeaderPageNo: Integer read GetHeaderPageNo write SetHeaderPageNo;
    property TagName: string read GetTagName write SetTagName;
    property KeyFormat:   Byte read GetKeyFormat   write SetKeyFormat;
    property ForwardTag1: Byte read GetForwardTag1 write SetForwardTag1;
    property ForwardTag2: Byte read GetForwardTag2 write SetForwardTag2;
    property BackwardTag: Byte read GetBackwardTag write SetBackwardTag;
    property Reserved: Byte read GetReserved write SetReserved;
    property KeyType: AnsiChar read GetKeyType write SetKeyType;
    property Tag: Pointer read FTag write FTag;
  end;
//===========================================================================
{$ifdef USE_CACHE}
  TIndexFile = class(TCachedFile)
{$else}
  TIndexFile = class(TPagedFile)
{$endif}
  protected
    FIndexName: string;
    FInsertError: string;
    FParsers: array[0..MaxIndexes-1] of TDbfIndexParser;
    FIndexHeaders: array[0..MaxIndexes-1] of Pointer;
    FIndexHeaderModified: array[0..MaxIndexes-1] of Boolean;
    FIndexHeader: Pointer;
    FIndexVersion: TXBaseVersion;
    FRoots: array[0..MaxIndexes-1] of TIndexPage;
    FCurrentParser: TDbfIndexParser;
    FRoot: TIndexPage;
    FMdxTag: TIndexTag;
    FTempMdxTag: TIndexTag;
    FEntryHeaderSize: Integer;
    FPageHeaderSize: Integer;
    FTagSize: Integer;
    FTagOffset: Integer;
    FHeaderPageNo: Integer;
    FSelectedIndex: Integer;
    FRangeIndex: Integer;
    FIsDescending: Boolean;
    FUniqueMode: TIndexUniqueType;
    FKeyBuffer: array[0..MaxIndexKeyLen] of AnsiChar;
    FLowBuffer: array[0..MaxIndexKeyLen] of AnsiChar;
    FHighBuffer: array[0..MaxIndexKeyLen] of AnsiChar;
    FEntryBof: Pointer;
    FEntryEof: Pointer;
    FDbfFile: Pointer;
    FCanEdit: Boolean;
    FOpened: Boolean;
    FRangeActive: Boolean;
    FUpdateMode: TIndexUpdateMode;
    FUserKey: PAnsiChar;        // find / insert key
    FUserRecNo: Integer;    // find / insert recno
    FUserBCD: array[0..11] of Byte;
    FUserNumeric: Double;
    FForceClose: Boolean;
    FForceReadOnly: Boolean;
    FCodePage: Integer;
    FCollation: PCollationTable;
    FCompareKeys: TDbfCompareKeysEvent;
    FOnLocaleError: TDbfLocaleErrorEvent;
    FProgressPosition: Integer;
    FProgressMax: Integer;

    function  GetNewPageNo: Integer;
    procedure TouchHeader(AHeader: Pointer);
    function  CreateTempFile(BaseName: string): TPagedFile;
    procedure ConstructInsertErrorMsg;
    procedure WriteIndexHeader(AIndex: Integer);
    procedure SelectIndexVars(AIndex: Integer);
    procedure CalcKeyLen;
    procedure CalcKeyProperties;
    procedure CalcRegenerateIndex;
    procedure UpdateIndexProperties;
    procedure ClearRoots;
    function  CalcTagOffset(AIndex: Integer): Pointer;
    procedure LinkTags;

    function  FindKey(AInsert: boolean): Integer;
    function  InsertKey(Buffer: TDbfRecordBuffer; RecNo: Integer): Boolean;
    procedure DeleteKey(Buffer: TDbfRecordBuffer; RecNo: Integer);
    function  InsertCurrent: Boolean;
    function  DeleteCurrent: Boolean;
    function  UpdateCurrent(PrevBuffer, NewBuffer: TDbfRecordBuffer; RecNo: Integer): Boolean;
    function  UpdateIndex(Index: Integer; PrevBuffer, NewBuffer: TDbfRecordBuffer; RecNo: Integer): Boolean;
    function  RecordIsIndexed(Buffer: TDbfRecordBuffer): Boolean;
    procedure ReadIndexes;
    procedure Resync(Relative: boolean);
    procedure ResyncRoot;
    procedure ResyncTree;
    procedure ResyncRange(KeepPosition: boolean);
    procedure ResetRange;
    procedure SetBracketLow;
    procedure SetBracketHigh;
    procedure MergeSort(List: PDbfPointerList; L, R: Integer);
    procedure MergeSort2(List, TempList: PDbfPointerList; L, R: Integer);
    procedure MergeSort3(List, TempList: PDbfPointerList; L0, L1, R0, R1: Integer);
    function  MergeSortCompare(Item1, Item2: Pointer): Integer;

    procedure WalkFirst;
    procedure WalkLast;
    function  WalkPrev: boolean;
    function  WalkNext: boolean;

    function  CompareKeysDate(Key1, Key2: PAnsiChar): Integer;
    function  CompareKeysLevel7(Key1, Key2: PAnsiChar): Integer;
    function  CompareKeysNumericNDX(Key1, Key2: PAnsiChar): Integer;
    function  CompareKeysNumericMDX(Key1, Key2: PAnsiChar): Integer;
    function  CompareKeysString(Key1, Key2: PAnsiChar): Integer;

    // property functions
    function  GetName: string;
    function  GetDbfLanguageId: Byte;
    function  GetKeyLen: Integer;
    function  GetKeyType: AnsiChar;
    function  GetExpression: string;
    function  GetPhysicalRecNo: Integer;
    function  GetSequentialRecNo: TSequentialRecNo;
    function  GetSequentialRecordCount: TSequentialRecNo;
    procedure SetSequentialRecNo(RecNo: TSequentialRecNo);
    procedure SetPhysicalRecNo(RecNo: Integer);
    procedure SetUpdateMode(NewMode: TIndexUpdateMode);
    procedure SetIndexName(const AIndexName: string);

    procedure LockIndex;
    procedure LockIndexPage;
    procedure ResyncIndex(Relative: Boolean);
    procedure UnlockIndex;
    procedure UnlockIndexPage;
    function SearchKeyInternal(Key: PAnsiChar; SearchType: TSearchKeyType): Boolean;
    function VersionPosition: TPagedFileOffset;
    function ReadVersion(PVersion: PByte): Boolean;
    procedure WriteVersion(PVersion: PByte);
    procedure CheckInvalidError;
    procedure InvalidError;
    function GetLeaf: TIndexPage;

    property Leaf: TIndexPage read GetLeaf;
  public
    constructor Create(ADbfFile: Pointer);
    destructor Destroy; override;

    procedure Open;
    procedure Close;

    procedure Clear;
    procedure Flush; override;
    procedure ClearIndex;
    procedure AddNewLevel;
    procedure InsertError;
    function  Insert(RecNo: Integer; Buffer: TDbfRecordBuffer): Boolean;
    function  Update(RecNo: Integer; PrevBuffer, NewBuffer: TDbfRecordBuffer): Boolean;
    procedure Delete(RecNo: Integer; Buffer: TDbfRecordBuffer);
    function  CheckKeyViolation(Buffer: TDbfRecordBuffer; RecNo: Integer): Boolean;
    procedure DeleteIndex(const AIndexName: string);
    procedure RepageFile;
    procedure CompactFile;
    procedure BulkLoadIndex;
    procedure BulkLoadIndexes;
    procedure PrepareRename(NewFileName: string);
    procedure CalcRegenerateIndexes;

    procedure CreateIndex(FieldDesc, TagName: string; Options: TIndexOptions);
    function  ExtractKeyFromBuffer(Buffer: TDbfRecordBuffer; RecNo: Integer): PAnsiChar;
    procedure ExtractKey(Key: PAnsiChar);
    function  CopyCurrentKey(Source, Dest: PAnsiChar): Integer;
    function  SearchKey(Key: PAnsiChar; SearchType: TSearchKeyType): Boolean;
    function  Find(RecNo: Integer; Buffer: PAnsiChar): Integer;
    function  IndexOf(const AIndexName: string): Integer;
    procedure DisableRange;
    procedure EnableRange;

    procedure GetIndexNames(const AList: TStrings);
    procedure GetIndexInfo(const AIndexName: string; IndexDef: TDbfIndexDef);
    procedure WriteHeader; override;
    procedure WriteFileHeader;

    procedure First;
    procedure Last;
    function  Next: Boolean;
    function  Prev: Boolean;

    procedure SetRange(LowRange, HighRange: PAnsiChar);
    procedure CancelRange;
    function  MatchKey(UserKey: PAnsiChar): Integer;
    function  CompareKey(Key: PAnsiChar): Integer;
    function  CompareKeys(Key1, Key2: PAnsiChar): Integer;
    function  PrepareKey(Buffer: TDbfRecordBuffer; ResultType: TExpressionType): TDbfRecordBuffer;

    property KeyLen: Integer read GetKeyLen;
    property IndexVersion: TXBaseVersion read FIndexVersion;
    property EntryHeaderSize: Integer read FEntryHeaderSize;
    property KeyType: AnsiChar read GetKeyType;

    property SequentialRecordCount: TSequentialRecNo read GetSequentialRecordCount;
    property SequentialRecNo: TSequentialRecNo read GetSequentialRecNo write SetSequentialRecNo;
    property PhysicalRecNo: Integer read GetPhysicalRecNo write SetPhysicalRecNo;
    property HeaderPageNo: Integer read FHeaderPageNo;

    property IndexHeader: Pointer read FIndexHeader;
    property EntryBof: Pointer read FEntryBof;
    property EntryEof: Pointer read FEntryEof;
    property UniqueMode: TIndexUniqueType read FUniqueMode;
    property IsDescending: Boolean read FIsDescending;

    property UpdateMode: TIndexUpdateMode read FUpdateMode write SetUpdateMode;
    property IndexName: string read FIndexName write SetIndexName;
    property Expression: string read GetExpression;

    property ForceClose: Boolean read FForceClose;
    property ForceReadOnly: Boolean read FForceReadOnly;
    property CodePage: Integer read FCodePage write FCodePage;

    property OnLocaleError: TDbfLocaleErrorEvent read FOnLocaleError write FOnLocaleError;
  end;

function IndexNameNormalize(Value: string): string;

function dbfParseIndexKey(Buffer: Pointer; KeyType: AnsiChar;
  KeyLength, SecondaryKeyType: Word; FoxPro: Boolean;
  var Value: {$ifdef SUPPORT_VARIANTS}Variant{$else}AnsiString{$endif}): Boolean;
function dbfParseIndexKeyAsString(Buffer: Pointer; KeyType: AnsiChar;
  KeyLength, SecondaryKeyType: Word; FoxPro: Boolean;
  var Value: AnsiString): Boolean;

//------------------------------------------------------------------------------
implementation

uses
{$ifdef SUPPORT_SEPARATE_VARIANTS_UNIT}
  Variants,
{$endif}
  dbf_ansistrings,
  dbf_dbffile,
  dbf_fields,
  dbf_str,
  dbf_prssupp;

function IndexNameNormalize(Value: string): string;
begin
  Result := AnsiUpperCase(Trim(Value));
end;

const
  RecBOF = 0;
  RecEOF = MaxInt;

//lcidBinary = $0A03;

  KeyFormat_Expression = $00;
  KeyFormat_Data       = $10;

  KeyFormat_Descending = $08;
  KeyFormat_String     = $10;
  KeyFormat_Distinct   = $20;
  KeyFormat_Unique     = $40;

  Unique_None          = $00;
  Unique_Unique        = $01;
  Unique_Distinct      = $21;

  IndexNameLen         = 16;
  IndexNameLenMax      = 8;

  // Value of Julian epoch (Jan 1, 4713 BCE) used in date index key
  DateJulianEpoch      = -2415019;

  // Value to display for NULL index key
  NullDisplayValue     = 'NULL';

var
  // Value of NULL in date index key
  DateNullValue: Double = 1E100;

function dbfParseIndexKey(Buffer: Pointer; KeyType: AnsiChar;
  KeyLength, SecondaryKeyType: Word; FoxPro: Boolean;
  var Value: {$ifdef SUPPORT_VARIANTS}Variant{$else}AnsiString{$endif}): Boolean;
var
{$ifdef SUPPORT_INT64}
  Int64Value: Int64;
{$endif}
  StringValue: AnsiString;
  DateTimeValue: TDateTime;
  IntValue: Integer;
  DwordValue: DWORD;
  ByteValue: Byte;
  FloatRec: TFloatRec;
  DigitCount: ShortInt;
  BCD: PByte;
  Digit: ShortInt;
  DoubleValue: Double;
begin
  Result := True;
  case KeyType of
    '@':
    begin
{$ifdef SUPPORT_INT64}
      if (SecondaryKeyType = $00) and (KeyLength = 8) then
      begin
        Int64Value := PInt64(Buffer)^;
        if Int64Value <> 0 then
        begin
          SwapInt64BE(@Int64Value, @Int64Value);
{$ifdef SUPPORT_VARIANTS}
          Value := BDETimeStampToDateTime(PDouble(@Int64Value)^);
{$else}
          Value := AnsiString(DateTimeToStr(BDETimeStampToDateTime(PDouble(
            @Int64Value)^)));
{$endif}
        end
        else
{$ifdef SUPPORT_VARIANTS}
          Value := Null;
{$else}
          Value := NullDisplayValue;
{$endif}
      end
      else
        Result := False;
{$else}
      Result := False;
{$endif}
    end;
    'C':
    begin
      if SecondaryKeyType = $00 then
      begin
        SetLength(StringValue, KeyLength);
        Move(Buffer^, PAnsiChar(StringValue)^, KeyLength);
        Value := dbfTrimRight(StringValue);
        if Value = '' then
        begin
{$ifdef SUPPORT_VARIANTS}
          Value := Null;
{$else}
          Value := NullDisplayValue;
{$endif}
        end;
      end
      else
        Result:= False;
    end;
    'D':
    begin
      if (SecondaryKeyType = $01) and (KeyLength = 8) then
      begin
        if PDouble(Buffer)^ <> DateNullValue then
        begin
          DateTimeValue := PDouble(Buffer)^ + DateJulianEpoch;
{$ifdef SUPPORT_VARIANTS}
          Value := DateTimeValue;
{$else}
          Value := DateTimeToStr(DateTimeValue);
{$endif}
        end
        else
{$ifdef SUPPORT_VARIANTS}
          Value := Null;
{$else}
          Value := NullDisplayValue;
{$endif}
      end
      else
        Result:= False;
    end;
    'I', '+':
    begin
      if (SecondaryKeyType = $00) and (KeyLength = 4) then
      begin
        if FoxPro then
{$ifdef SUPPORT_VARIANTS}
          Value := Integer(SwapIntLE(PDWORD(Buffer)^))
{$else}
          Value := IntToStr(Integer(SwapIntLE(PDWORD(Buffer)^)))
{$endif}
        else
        begin
          if PInteger(Buffer)^ <> 0 then
          begin
            DwordValue := SwapIntBE(PDWORD(Buffer)^);
            IntValue := PInteger(@DwordValue)^ xor Integer($80000000);
{$ifdef SUPPORT_VARIANTS}
            Value := IntValue;
{$else}
            Value := IntToStr(IntValue);
{$endif}
          end
          else
{$ifdef SUPPORT_VARIANTS}
            Value := Null;
{$else}
            Value := NullDisplayValue;
{$endif}
        end;
      end
      else
        Result:= False;
    end;
    'N':
    begin
      case SecondaryKeyType of
        $00:
        begin
          if KeyLength = 12 then
          begin
            ByteValue := PByte(Buffer)^;
            FillChar(FloatRec, SizeOf(FloatRec), 0);
            FloatRec.Exponent := ByteValue - $34;
            DigitCount := PShortInt(PAnsiChar(Buffer) + 1)^;
            if (DigitCount and $80) <> 0 then
            begin
              DigitCount := DigitCount and ShortInt(not $80);
              FloatRec.Negative := True;
            end;
            if (DigitCount >= 1) and (((DigitCount - 1) mod 4) = 0) then
            begin
              DigitCount := (DigitCount - 1) div 4;
              if DigitCount <= 20 then
              begin
                BCD := PByte(PAnsiChar(Buffer) + 2);
                Digit := 0;
                while (Digit < DigitCount) and
                  (Digit < High(FloatRec.Digits)) and Result do
                begin
                  if (Digit mod 2) = 0 then
                    ByteValue := BCD^ shr 4
                  else
                  begin
                    ByteValue := BCD^ and $0F;
                    Inc(BCD);
                  end;
                  if ByteValue <= 9 then
                  begin
                    if (Digit < Pred(DigitCount)) or (ByteValue <> 0) then
                      Byte(FloatRec.Digits[Digit]) := Ord('0') + ByteValue;
                  end
                  else
                    Result := False;
                  Inc(Digit);
                end;
                if Result then
                begin
                  StringValue := AnsiChar('0') + AnsiChar(DecimalSeparator) +
                    PAnsiChar(@FloatRec.Digits) + AnsiChar('E') +
                    AnsiString(IntToStr(FloatRec.Exponent));
                  if FloatRec.Negative then
                    StringValue := '-' + StringValue;
                  Result := TryStrToFloat(string(StringValue), DoubleValue);
                  if Result then
{$ifdef SUPPORT_VARIANTS}
                    Value := DoubleValue;
{$else}
                    Value := FloatToStr(DoubleValue);
{$endif}
                end;
              end
              else
                Result:= False;
            end
            else
              Result:= False;
          end
          else
            Result := False;
        end;
        $01:
        begin
          if KeyLength = 8 then
          begin
            DoubleValue := PDouble(Buffer)^;
{$ifdef SUPPORT_VARIANTS}
            Value := DoubleValue;
{$else}
            Value := FloatToStr(DoubleValue);
{$endif}
          end
          else
            Result := False;
        end;
        else
          Result := False;
      end;
    end;
    'O':
    begin
{$ifdef SUPPORT_INT64}
      if (SecondaryKeyType = $00) and (KeyLength = 8) then
      begin
        if PInt64(Buffer)^ <> 0 then
        begin
          DoubleValue := PDouble(Buffer)^;
          SwapInt64BE(@DoubleValue, @DoubleValue);
          if PInt64(@DoubleValue)^ > 0 then
            PInt64(@DoubleValue)^ := not PInt64(@DoubleValue)^
          else
            DoubleValue := -DoubleValue;
{$ifdef SUPPORT_VARIANTS}
          Value := DoubleValue;
{$else}
          Value := FloatToStr(DoubleValue);
{$endif}
        end
        else
{$ifdef SUPPORT_VARIANTS}
          Value := Null;
{$else}
          Value := NullDisplayValue;
{$endif}
      end
      else
        Result := False;
{$else}
      Result := False;
{$endif}
    end;
  end;
end;

function dbfParseIndexKeyAsString(Buffer: Pointer; KeyType: AnsiChar;
  KeyLength, SecondaryKeyType: Word; FoxPro: Boolean;
  var Value: AnsiString): Boolean;
var
  VariantValue: Variant;
begin
  {$ifdef SUPPORT_VARIANTS}
  Result := dbfParseIndexKey(Buffer, KeyType, KeyLength, SecondaryKeyType,
    FoxPro, VariantValue);
  if Result then
    Value := AnsiString(VarToStr(VariantValue));
  {$else}
  Result := dbfParseIndexKey(Buffer, KeyType, KeyLength, SecondaryKeyType,
    FoxPro, Value);
  {$endif}
end;

type

  TLCIDList = class(TList)
  public
    constructor Create;

    procedure Enumerate;
  end;

  PMdxHdr = ^rMdxHdr;
  rMdxHdr = packed record
    MdxVersion : Byte;     // 0
    Year       : Byte;     // 1
    Month      : Byte;     // 2
    Day        : Byte;     // 3
    FileName   : array[0..IndexNameLen - 1] of AnsiChar;   // 4..19
    BlockSize  : Word;     // 20..21
    BlockAdder : Word;     // 22..23
    ProdFlag   : Byte;     // 24
    NumTags    : Byte;     // 25
    TagSize    : Word;     // 26
    TagsUsed   : Word;     // 28..29
    NextTag    : Byte;     // 30
    Language   : Byte;     // 31
    NumPages   : Integer;  // 32..35
    FreePage   : Integer;  // 36..39
    BlockFree  : Integer;  // 40..43
    UpdYear    : Byte;     // 44
    UpdMonth   : Byte;     // 45
    UpdDay     : Byte;     // 46
    Reserved   : array[0..481] of Byte;  // 47..528
    TagFlag    : Byte;     // 529                   // dunno what this means but it ought to be 1  :-)
  end;

  // Tags -> I don't know what to with them
  // KeyType -> Variable position, db7 different from db4

  PMdx4Tag = ^rMdx4Tag;
  rMdx4Tag = packed record
    HeaderPageNo   : Integer;      // 0..3
    TagName        : array [0..10] of AnsiChar;  // 4..14 of Byte
    KeyFormat      : Byte;         // 15     00h: Calculated
                                   //        10h: Data Field
    ForwardTag1    : Byte;         // 16
    ForwardTag2    : Byte;         // 17
    BackwardTag    : Byte;         // 18
    Reserved       : Byte;         // 19
    KeyType        : AnsiChar;         // 20     C : Character
                                   //        N : Numerical
                                   //        D : Date
  end;

  PMdx7Tag = ^rMdx7Tag;
  rMdx7Tag = packed record
    HeaderPageNo   : Integer;      // 0..3
    TagName        : array [0..32] of AnsiChar;  // 4..36 of Byte
    KeyFormat      : Byte;         // 37     00h: Calculated
                                   //        10h: Data Field
    ForwardTag1    : Byte;         // 38
    ForwardTag2    : Byte;         // 39
    BackwardTag    : Byte;         // 40
    Reserved       : Byte;         // 41
    KeyType        : AnsiChar;         // 42     C : Character
                                   //        N : Numerical
                                   //        D : Date
  end;

  PIndexHdr = ^rIndexHdr;
  rIndexHdr = packed record
    RootPage       : Integer;  // 0..3
    NumPages       : Integer;  // 4..7
    KeyFormat      : Byte;     // 8      00h: Right, Left, DTOC
                               //        08h: Descending order
                               //        10h: String
                               //        20h: Distinct
                               //        40h: Unique
    KeyType        : AnsiChar;     // 9      C : Character
                               //        N : Numerical
                               //        D : Date
    Dummy          : Byte;     // 10
    Language       : Byte;     // 11
    KeyLen         : Word;     // 12..13
    NumKeys        : Word;     // 14..15
    sKeyType       : Word;     // 16..17 00h: DB4: C/N; DB3: C
                               //        01h: DB4: D  ; DB3: N/D
    KeyRecLen      : Word;     // 18..19 Length of key entry in page
    Version        : Byte;     // 20
    Dummy1         : Byte;     // 21
    Dummy2         : Byte;     // 22
    Unique         : Byte;     // 23
    KeyDesc        : array [0..219] of AnsiChar; // 24..243
    Dummy3         : Byte;     // 244
    ForExist       : Byte;     // 245
    KeyExist       : Byte;     // 246
    Dummy4         : Byte;     // 247
    FirstNode      : Longint;  // 248..251   first node that contains data
    LastNode       : Longint;  // 252..255   last node that contains data
                               // MDX Header has here a 506 byte block reserved
                               // and then the FILTER expression, which obviously doesn't
                               // fit in a NDX page, so we'll skip it
  end;

  PMdxEntry = ^rMdxEntry;
  rMdxEntry = packed record
    RecBlockNo: Longint;       // 0..3   either recno or blockno
    KeyData   : AnsiChar;          // 4..    first byte of data, context => length
  end;

  PMdxPage = ^rMdxPage;
  rMdxPage = packed record
    NumEntries : Integer;
    PrevBlock  : Integer;
    FirstEntry : rMdxEntry;
  end;

  PNdxEntry  = ^rNdxEntry;
  rNdxEntry  = packed record
    LowerPageNo: Integer;      //  0..3 lower page
    RecNo      : Integer;      //  4..7 recno
    KeyData    : AnsiChar;
  end;

  PNdxPage  = ^rNdxPage;
  rNdxPage  = packed record
    NumEntries: Integer;       //  0..3
    FirstEntry: rNdxEntry;
  end;

//---------------------------------------------------------------------------
  TMdxPage = class(TIndexPage)
  protected
    function GetEntry(AEntryNo: Integer): Pointer; override;
    function GetLowerPageNo: Integer; override;
    function GetKeyData: PAnsiChar; override;
    function GetNumEntries: Integer; override;
    function GetKeyDataFromEntry(AEntry: Integer): PAnsiChar; override;
    function GetRecNo: Integer; override;
    function GetIsInnerNode: Boolean; override;
    procedure IncNumEntries; override;
    procedure SetNumEntries(NewNum: Integer); override;
    procedure SetRecLowerPageNo(NewRecNo, NewPageNo: Integer); override;
    procedure SetRecLowerPageNoOfEntry(AEntry, NewRecNo, NewPageNo: Integer); override;
    procedure SetPrevBlock(NewBlock: Integer); override;
  end;
//---------------------------------------------------------------------------
  TNdxPage = class(TIndexPage)
  protected
    function GetEntry(AEntryNo: Integer): Pointer; override;
    function GetLowerPageNo: Integer; override;
    function GetKeyData: PAnsiChar; override;
    function GetNumEntries: Integer; override;
    function GetKeyDataFromEntry(AEntry: Integer): PAnsiChar; override;
    function GetRecNo: Integer; override;
    function GetIsInnerNode: Boolean; override;
    procedure IncNumEntries; override;
    procedure SetNumEntries(NewNum: Integer); override;
    procedure SetRecLowerPageNo(NewRecNo, NewPageNo: Integer); override;
    procedure SetRecLowerPageNoOfEntry(AEntry, NewRecNo, NewPageNo: Integer); override;
  end;
//---------------------------------------------------------------------------
  TMdx4Tag = class(TIndexTag)
  protected
    function  GetHeaderPageNo: Integer; override;
    function  GetTagName: string; override;
    function  GetKeyFormat: Byte; override;
    function  GetForwardTag1: Byte; override;
    function  GetForwardTag2: Byte; override;
    function  GetBackwardTag: Byte; override;
    function  GetReserved: Byte; override;
    function  GetKeyType: AnsiChar; override;
    procedure SetHeaderPageNo(NewPageNo: Integer); override;
    procedure SetTagName(NewName: string); override;
    procedure SetKeyFormat(NewFormat: Byte); override;
    procedure SetForwardTag1(NewTag: Byte); override;
    procedure SetForwardTag2(NewTag: Byte); override;
    procedure SetBackwardTag(NewTag: Byte); override;
    procedure SetReserved(NewReserved: Byte); override;
    procedure SetKeyType(NewType: AnsiChar); override;
  end;
//---------------------------------------------------------------------------
  TMdx7Tag = class(TIndexTag)
    function  GetHeaderPageNo: Integer; override;
    function  GetTagName: string; override;
    function  GetKeyFormat: Byte; override;
    function  GetForwardTag1: Byte; override;
    function  GetForwardTag2: Byte; override;
    function  GetBackwardTag: Byte; override;
    function  GetReserved: Byte; override;
    function  GetKeyType: AnsiChar; override;
    procedure SetHeaderPageNo(NewPageNo: Integer); override;
    procedure SetTagName(NewName: string); override;
    procedure SetKeyFormat(NewFormat: Byte); override;
    procedure SetForwardTag1(NewTag: Byte); override;
    procedure SetForwardTag2(NewTag: Byte); override;
    procedure SetBackwardTag(NewTag: Byte); override;
    procedure SetReserved(NewReserved: Byte); override;
    procedure SetKeyType(NewType: AnsiChar); override;
  end;

var
  Entry_Mdx_BOF: rMdxEntry;   //(RecBOF, #0);
  Entry_Mdx_EOF: rMdxEntry;   //(RecBOF, #0);
  Entry_Ndx_BOF: rNdxEntry;   //(0, RecBOF, #0);
  Entry_Ndx_EOF: rNdxEntry;   //(0, RecEOF, #0);

  LCIDList: TLCIDList;

procedure IncWordLE(var AVariable: Word; Amount: Integer);
begin
  AVariable := SwapWordLE(SwapWordLE(AVariable) + Amount);
end;

procedure IncIntLE(var AVariable: Integer; Amount: Integer);
begin
  AVariable := SwapIntLE(DWord(Integer(SwapIntLE(AVariable)) + Amount));
end;

//==========================================================
// Locale support for all versions of Delphi/C++Builder

function LocaleCallBack(LocaleString: PAnsiChar): Integer; stdcall;
begin
  LCIDList.Add({%H-}Pointer(StrToInt(String('$'+LocaleString))));
  Result := 1;
end;

constructor TLCIDList.Create;
begin
  inherited;
end;

procedure TLCIDList.Enumerate;
begin
  Clear;
  EnumSystemLocales(@LocaleCallBack, LCID_SUPPORTED);
end;

{ TIndexPage }

constructor TIndexPage.Create(Parent: TIndexFile);
begin
  FIndexFile := Parent;
  GetMem(FPageBuffer, FIndexFile.RecordSize);
  FLowerPage := nil;
  Clear;
end;

destructor TIndexPage.Destroy;
begin
  if (FLowerPage<>nil) then
    LowerPage.Free;
  WritePage;
  FreeMemAndNil(FPageBuffer);
  inherited Destroy;
end;

procedure TIndexPage.Clear;
begin
  FillChar(PAnsiChar(FPageBuffer)^, FIndexFile.RecordSize, 0);
  FreeAndNil(FLowerPage);
  FUpperPage := nil;
  FPageNo := -1;
  FEntryNo := -1;
  FWeight := 1;
  FModified := false;
  FEntry := FIndexFile.EntryBof;
  FLowPage := 0;
  FHighPage := 0;
  FLowIndex := 0;
  FHighIndex := -1;
end;

procedure TIndexPage.GetNewPage;
begin
  FPageNo := FIndexFile.GetNewPageNo;
end;

procedure TIndexPage.Modified;
begin
  FModified := true;
end;

procedure TIndexPage.LocalInsert(RecNo: Integer; Buffer: PAnsiChar; LowerPageNo: Integer);
  // *) assumes there is at least one entry free
var
  source, dest: Pointer;
  size, lNumEntries, numKeysAvail: Integer;
begin
  // check assertions
  lNumEntries := GetNumEntries;
  // if this is inner node, we can only store one less than max entries
  numKeysAvail := SwapWordLE(PIndexHdr(FIndexFile.IndexHeader)^.NumKeys) - lNumEntries;
  if FLowerPage <> nil then
    dec(numKeysAvail);
  // check if free space
  assert(numKeysAvail > 0);
  // first free up some space
  source := FEntry;
  dest := GetEntry(FEntryNo + 1);
  size := (lNumEntries - EntryNo) * SwapWordLE(PIndexHdr(FIndexFile.IndexHeader)^.KeyRecLen);
  // if 'rightmost' entry, copy pageno too
  if (FLowerPage <> nil) or (numKeysAvail > 1) then
    size := size + FIndexFile.EntryHeaderSize;
  Move(source^, dest^, size);
  // one entry added
  Inc(FHighIndex);
  IncNumEntries;
  // lNumEntries not valid from here
  SetEntry(RecNo, Buffer, LowerPageNo);
  // done!
end;

procedure TIndexPage.LocalDelete;
var
  source, dest: Pointer;
  size, lNumEntries: Integer;
begin
  // get num entries
  lNumEntries := GetNumEntries;
  // is this last entry? if it's not move entries after current one
  if EntryNo < FHighIndex then
  begin
    source := GetEntry(EntryNo + 1);
    dest := FEntry;
    size := (FHighIndex - EntryNo) * SwapWordLE(PIndexHdr(FIndexFile.IndexHeader)^.KeyRecLen);
    Move(source^, dest^, size);
  end else
  // no need to update when we're about to remove the only entry
  if (UpperPage <> nil) and (FHighIndex > FLowIndex) then
  begin
    // we are about to remove the last on this page, so update search
    // key data of parent
    EntryNo := FHighIndex - 1;
    UpperPage.SetEntry(0, GetKeyData, FPageNo);
  end;
  // one entry less now
  dec(lNumEntries);
  dec(FHighIndex);
  if lNumEntries >= 0 then
    SetNumEntries(lNumEntries);
  // zero last one out to not get confused about internal or leaf pages
  // note: need to decrease lNumEntries and HighIndex first, otherwise
  //   check on page key consistency will fail
  SetRecLowerPageNoOfEntry(FHighIndex+1, 0, 0);
  // update bracket indexes
  if FHighPage = FPageNo then
    dec(FHighBracket);
  // check if range violated
  if EntryNo > FHighIndex then
    EntryNo := FHighIndex;
  // check if still entries left, otherwise remove page from parent
  if FHighIndex = -1 then
  begin
    if UpperPage <> nil then
      UpperPage.LocalDelete;
  end;
  // go to valid record in lowerpage
  if FLowerPage <> nil then
    SyncLowerPage;
  // flag modified page
  FModified := true;
  // success!
end;

function TIndexPage.MatchKey: Integer;
  // assumes Buffer <> nil
var
  keyData: PAnsiChar;
begin
  // get key data
  keyData := GetKeyData;
  // use locale dependant compare
  Result := FIndexFile.CompareKey(keyData);
end;

function TIndexPage.FindNearest(ARecNo: Integer): Integer;
  // pre:
  //  assumes Key <> nil
  //  assumes FLowIndex <= FHighIndex + 1
  //  ARecNo = -2 -> search first key matching Key
  //  ARecNo = -3 -> search first key greater than Key
  //  ARecNo >  0 -> search key matching Key and its recno = ARecNo
  // post:
  //  Result < 0  -> key,recno smaller than current entry
  //  Result = 0  -> key,recno found, FEntryNo = found key entryno
  //  Result > 0  -> key,recno larger than current entry
var
  low, high, current: Integer;
begin
  // implement binary search, keys are sorted
  low := FLowIndex;
  high := GetNumEntries;
  // always true: Entry(FEntryNo) = FEntry
  // FHighIndex >= 0 because no-entry cases in leaves have been filtered out
  // entry HighIndex may not be bigger than rest (in inner node)
  // ARecNo = -3 -> search last recno matching key
  // need to have: low <= high
  // define low - 1 = neg.inf.
  // define high = pos.inf
  // inv1: (ARecNo<>-3) -> Entry(low-1).Key <  Key <= Entry(high).Key
  // inv2: (ARecNo =-3) -> Entry(low-1).Key <= Key <  Entry(high).Key
  // vf: high + 1 - low
  while low < high do
  begin
    current := (low + high) div 2;
    FEntry := GetEntry(current);
    // calc diff
    Result := MatchKey;
    if (Result = 0) and (ARecNo = -3) and (LowerPage = nil) then
      Result := FIndexFile.FUserRecNo - GetRecNo;
    // test if we need to go lower or higher
    // result < 0 implies key smaller than tested entry
    // result = 0 implies key equal to tested entry
    // result > 0 implies key greater than tested entry
    if (Result < 0) or ((ARecNo<>-3) and (Result=0)) then
      high := current
    else
      low := current+1;
  end;
  // high will contain first greater-or-equal key
  // ARecNo <> -3 -> Entry(high).Key will contain first key that matches    -> go to high
  // ARecNo =  -3 -> Entry(high).Key will contain first key that is greater -> go to high
  FEntryNo := -1;
  EntryNo := high;
  // calc end result: can't inspect high if lowerpage <> nil
  // if this is a leaf, we need to find specific recno
  if (LowerPage = nil) then
  begin
    if high > FHighIndex then
    begin
      Result := 1;
    end else begin
      Result := MatchKey;
      // test if we need to find a specific recno
      // result < 0 -> current key greater -> nothing found -> don't search
      if (ARecNo > 0) then
      begin
        // BLS to RecNo
        high := FHighIndex + 1;
        low := FEntryNo;
        // inv: FLowIndex <= FEntryNo <= high <= FHighIndex + 1 /\
        // (Ai: FLowIndex <= i < FEntryNo: Entry(i).RecNo <> ARecNo)
        while FEntryNo <> high do
        begin
          // FEntryNo < high, get new entry
          if low <> FEntryNo then
          begin
            FEntry := GetEntry(FEntryNo);
            // check if entry key still ok
            Result := MatchKey;
          end;
          // test if out of range or found recno
          if (Result <> 0) or (GetRecNo = ARecNo) then
            high := FEntryNo
          else begin
            // default to EOF
            inc(FEntryNo);
            Result := 1;
          end;
        end;
      end;
    end;
  end else begin
    // FLowerPage <> nil -> high contains entry, can not have empty range
    Result := 0;
  end;
end;

procedure TIndexPage.GotoInsertEntry;
  // assures we really can insert here
begin
  if FEntry = FIndexFile.EntryEof then
    FEntry := GetEntry(FEntryNo);
end;

procedure TIndexPage.SetEntry(RecNo: Integer; AKey: PAnsiChar; LowerPageNo: Integer);
var
  keyData: PAnsiChar;
{$ifdef TDBF_INDEX_CHECK}
  prevKeyData, curKeyData, nextKeyData: PAnsiChar;
{$endif}
begin
  // get num entries
  keyData := GetKeyData;
  // check valid entryno: we should be able to insert entries!
  assert((EntryNo >= 0) and (EntryNo <= FHighIndex));
  if (UpperPage <> nil) and (FEntryNo = FHighIndex) then
    UpperPage.SetEntry(0, AKey, FPageNo);
  if AKey <> nil then
    FIndexFile.CopyCurrentKey(AKey, keyData)
  else
    PAnsiChar(keyData)^ := #0;
  // set entry info
  SetRecLowerPageNo(RecNo, LowerPageNo);
  // flag we modified the page
  FModified := true;

{$ifdef TDBF_INDEX_CHECK}

    // check sorted entry sequence
    curKeyData  := GetKeyDataFromEntry(FEntryNo+0);
    // check if prior entry not greater, 'rightmost' key does not have to match
    if (FEntryNo > 0) and ((FLowerPage = nil) or (FEntryNo < FHighIndex)) then
    begin
      prevKeyData := GetKeyDataFromEntry(FEntryNo-1);
      if FIndexFile.CompareKeys(prevKeyData, curKeyData) > 0 then
        FIndexFile.CheckInvalidError;
    end;
    // check if next entry not smaller
    if ((FLowerPage = nil) and (FEntryNo < FHighIndex)) or
        ((FLowerPage <> nil) and (FEntryNo < (FHighIndex - 1))) then
    begin
      nextKeyData := GetKeyDataFromEntry(FEntryNo+1);
      if FIndexFile.CompareKeys(curKeyData, nextKeyData) > 0 then
        FIndexFile.CheckInvalidError;
    end;

{$endif}

end;

procedure TIndexPage.SetPrevBlock(NewBlock: Integer);
begin
end;

procedure TIndexPage.Split;
  // *) assumes this page is `nearly' full
var
  NewPage: TIndexPage;
  source, dest: Pointer;
  paKeyData: PAnsiChar;
  size, oldEntryNo: Integer;
  splitRight, lNumEntries, numEntriesNew: Integer;
  saveLow, saveHigh: Integer;
begin
  // get num entries
  lNumEntries := GetNumEntries;

  // avoid splitting page when possible
  if EntryNo = lNumEntries then
  begin
    splitRight := lNumEntries;
    numEntriesNew := 0;
  end
  else
  begin
    // calc split pos: split in half
    splitRight := lNumEntries div 2;
    if (FLowerPage <> nil) and (lNumEntries mod 2 = 1) then
      inc(splitRight);
    numEntriesNew := lNumEntries - splitRight;
    // check if place to insert has least entries
    if (numEntriesNew > splitRight) and (EntryNo > splitRight) then
    begin
      inc(splitRight);
      dec(numEntriesNew);
    end else if (numEntriesNew < splitRight) and (EntryNo < splitRight) then
    begin
      dec(splitRight);
      inc(numEntriesNew);
    end;
  end;
  // save current entryno
  oldEntryNo := EntryNo;
  // check if we need to save high / low bound
  if FLowPage = FPageNo then
    saveLow := FLowIndex
  else
    saveLow := -1;
  if FHighPage = FPageNo then
    saveHigh := FHighIndex
  else
    saveHigh := -1;

  // create new page
  NewPage := TIndexPageClass(ClassType).Create(FIndexFile);
  try
    // get page
    NewPage.GetNewPage;
    NewPage.SetPrevBlock(NewPage.PageNo - FIndexFile.PagesPerRecord);

    // set modified
    FModified := true;
    NewPage.FModified := true;

    // assure parent exists, if not -> create it
    if FUpperPage = nil then
      FIndexFile.AddNewLevel;

    // compute source, dest
    dest := NewPage.GetEntry(0);
    source := GetEntry(splitRight);
    size := numEntriesNew * SwapWordLE(PIndexHdr(FIndexFile.IndexHeader)^.KeyRecLen);
    // if inner node, copy rightmost entry too
    if FLowerPage <> nil then
      size := size + FIndexFile.EntryHeaderSize;
    // copy bytes
    if size <> 0 then
      Move(source^, dest^, size);
    // if not inner node, clear possible 'rightmost' entry
    if (FLowerPage = nil) and (numEntriesNew > 0) then
      SetRecLowerPageNoOfEntry(splitRight, 0, 0);

    // calc new number of entries of this page
    lNumEntries := lNumEntries - numEntriesNew;
    // if lower level, then we need adjust for new 'rightmost' node
    if FLowerPage <> nil then
    begin
      // right split, so we need 'new' rightmost node
      dec(lNumEntries);
    end;
    // store new number of nodes
    // new page is right page, so update parent to point to new right page
    NewPage.SetNumEntries(numEntriesNew);
    SetNumEntries(lNumEntries);
    // update highindex
    FHighIndex := lNumEntries;
    if FLowerPage = nil then
      dec(FHighIndex);

    // get data of last entry on this page
    assert(splitRight > 0);
    paKeyData := GetKeyDataFromEntry(splitRight - 1);

    // we know there is at least one entry free in this page and higher up
    FUpperPage.LocalInsert(0, paKeyData, FPageNo);

    // new page is right page, so update parent to point to new right page
    // we can't do this earlier: we will get lost in tree!
    FUpperPage.SetRecLowerPageNoOfEntry(FUpperPage.EntryNo+1, 0, NewPage.PageNo);

    // NOTE: UpperPage.LowerPage = Self <= inserted FPageNo, not NewPage.PageNo
  finally
    NewPage.Free;
  end;

  // go to entry we left on
  if oldEntryNo >= splitRight then
  begin
    // sync upperpage with right page
    FUpperPage.EntryNo := FUpperPage.EntryNo + 1;
    FEntryNo := oldEntryNo - splitRight;
    FEntry := GetEntry(FEntryNo);
  end else begin
    // in left page = this page
    EntryNo := oldEntryNo;
  end;

  // check if we have to save high / low bound
  // seen the fact that FHighPage = FPageNo -> EntryNo <= FHighIndex, it can in
  // theory not happen that page is advanced to right page and high bound remains
  // on left page, but we won't check for that here
  if saveLow >= splitRight then
  begin
    FLowPage := FPageNo;
    FLowIndex := saveLow - splitRight;
  end;
  if saveHigh >= splitRight then
  begin
    FHighPage := FPageNo;
    FHighIndex := saveHigh - splitRight;
  end;
end;

procedure TIndexPage.Delete;
begin
  LocalDelete;
end;

procedure TIndexPage.WritePage;
begin
  // check if we modified current page
  if FModified and (FPageNo > 0) then
  begin
    FIndexFile.WriteRecord(FPageNo, FPageBuffer);
    FModified := false;
  end;
end;

procedure TIndexPage.Flush;
begin
  WritePage;
  if FLowerPage <> nil then
    FLowerPage.Flush;
end;

procedure TIndexPage.RecalcWeight;
begin
  if FLowerPage <> nil then
  begin
    FWeight := FLowerPage.Weight * SwapWordLE(PIndexHdr(FIndexFile.IndexHeader)^.NumKeys);
  end else begin
    FWeight := 1;
  end;
  if FUpperPage <> nil then
    FUpperPage.RecalcWeight;
end;

procedure TIndexPage.UpdateWeight;
begin
  if FLowerPage <> nil then
    FLowerPage.UpdateWeight
  else
    RecalcWeight;
end;

procedure TIndexPage.SetUpperPage(NewPage: TIndexPage);
begin
  if FUpperPage <> NewPage then
  begin
    // root height changed: update weights
    FUpperPage := NewPage;
    UpdateWeight;
  end;
end;

procedure TIndexPage.SetLowPage(NewPage: Integer);
begin
  if FLowPage <> NewPage then
  begin
    FLowPage := NewPage;
    UpdateBounds(FLowerPage <> nil);
  end;
end;

procedure TIndexPage.SetHighPage(NewPage: Integer);
begin
  if FHighPage <> NewPage then
  begin
    FHighPage := NewPage;
    UpdateBounds(FLowerPage <> nil);
  end;
end;

procedure TIndexPage.UpdateBounds(IsInnerNode: Boolean);
begin
  // update low / high index range
  if FPageNo = FLowPage then
    FLowIndex := FLowBracket
  else
    FLowIndex := 0;
  if FPageNo = FHighPage then
    FHighIndex := FHighBracket
  else begin
    FHighIndex := GetNumEntries;
    if not IsInnerNode then
      dec(FHighIndex);
  end;
end;

function TMdxPage.GetIsInnerNode: Boolean;
begin
  Result := SwapIntLE(PMdxPage(FPageBuffer)^.NumEntries) < SwapWordLE(PIndexHdr(FIndexFile.IndexHeader)^.NumKeys);
  // if there is still an entry after the last one, this has to be an inner node
  if Result then
    Result := PMdxEntry(GetEntry(PMdxPage(FPageBuffer)^.NumEntries))^.RecBlockNo <> 0;
end;

function TNdxPage.GetIsInnerNode: Boolean;
begin
  Result := PNdxEntry(GetEntry(0))^.LowerPageNo <> 0;
end;

procedure TIndexPage.SetPageNo(NewPageNo: Integer);
var
  isInnerNode: Boolean;
begin
  if (NewPageNo <> FPageNo) or FIndexFile.NeedLocks then
  begin
    // save changes
    WritePage;

    // goto new page
    FPageNo := NewPageNo;
    // remind ourselves we need to load new entry when page loaded
    FEntryNo := -1;
    if (NewPageNo > 0) and (NewPageNo <= FIndexFile.RecordCount) then
    begin
      // read page from disk
      FIndexFile.ReadRecord(NewPageNo, FPageBuffer);

      // fixup descending tree
      isInnerNode := GetIsInnerNode;

      // update low / high index range
      UpdateBounds(isInnerNode);

      // read inner node if any
      if isInnerNode then
      begin
        if FLowerPage = nil then
        begin
          FLowerPage := TIndexPageClass(ClassType).Create(FIndexFile);
          FLowerPage.UpperPage := Self;
        end;
        // read first entry, don't do this sooner, not created lowerpage yet
        // don't recursively resync all lower pages
      end else if FLowerPage <> nil then
      begin
        FLowerPage.Free;
        FLowerPage := nil;
      end else begin
        // we don't have to check autoresync here because we're already at lowest level
        EntryNo := FLowIndex;
      end;
    end;
  end;
end;

procedure TIndexPage.SyncLowerPage;
  // *) assumes FLowerPage <> nil!
begin
  FLowerPage.PageNo := GetLowerPageNo;
end;

procedure TIndexPage.SetEntryNo(value: Integer);
begin
  // do not bother if no change
  if value <> FEntryNo then
  begin
    // check if out of range
    if (value < FLowIndex) then
    begin
      FEntryNo := FLowIndex - 1;
      FEntry := FIndexFile.EntryBof;
    end else if value > FHighIndex then begin
      FEntryNo := FHighIndex + 1;
      FEntry := FIndexFile.EntryEof;
    end else begin
      FEntryNo := value;
      FEntry := GetEntry(value);
      // sync lowerpage with entry
      if (FLowerPage <> nil) then
      begin
        if GetLowerPageNo = 0 then
          FIndexFile.CheckInvalidError;
        SyncLowerPage;
      end;
    end;
  end;
end;

function TIndexPage.PhysicalRecNo: Integer;
var
  entryRec: Integer;
begin
  // get num entries
  entryRec := GetRecNo;
  // check if in range
  if (FEntryNo >= FLowIndex) and (FEntryNo <= FHighIndex) then
    Result := entryRec
  else
    Result := -1;
end;

function TIndexPage.RecurPrev: Boolean;
begin
  EntryNo := EntryNo - 1;
  Result := Entry <> FIndexFile.EntryBof;
  if Result then
  begin
    if FLowerPage <> nil then
    begin
      FLowerPage.RecurLast;
      Result := FLowerPage.Entry <> FIndexFile.EntryEof;
    end;
  end else begin
    if FUpperPage<>nil then
    begin
      Result := FUpperPage.RecurPrev;
    end;
  end;
end;

function TIndexPage.RecurNext: Boolean;
begin
  EntryNo := EntryNo + 1;
  Result := Entry <> FIndexFile.EntryEof;
  if Result then
  begin
    if FLowerPage <> nil then
    begin
      FLowerPage.RecurFirst;
      Result := FLowerPage.Entry <> FIndexFile.EntryBof;
    end;
  end else begin
    if FUpperPage<>nil then
    begin
      Result := FUpperPage.RecurNext;
    end;
  end;
end;

procedure TIndexPage.RecurFirst;
begin
  EntryNo := FLowIndex;
  if FHighIndex < FLowIndex then
    FEntry := FIndexFile.EntryBOF;
  if (FLowerPage<>nil) then
    FLowerPage.RecurFirst;
end;

procedure TIndexPage.RecurLast;
begin
  EntryNo := FHighIndex;
  if FHighIndex < FLowIndex then
    FEntry := FIndexFile.EntryEOF;
  if (FLowerPage<>nil) then
    FLowerPage.RecurLast;
end;

procedure TIndexPage.SaveBracket;
begin
  FLowPageTemp := FLowPage;
  FHighPageTemp := FHighPage;
end;

procedure TIndexPage.RestoreBracket;
begin
  FLowPage := FLowPageTemp;
  FHighPage := FHighPageTemp;
end;

//==============================================================================
//============ Mdx specific access routines
//==============================================================================

function TMdxPage.GetEntry(AEntryNo: Integer): Pointer;
begin
  if (AEntryNo >= 0 ) and (AEntryNo <
    SwapWordLE(PIndexHdr(FIndexFile.IndexHeader)^.NumKeys)) then
  begin
    // get base + offset
    Result := PAnsiChar(@PMdxPage(PageBuffer)^.FirstEntry)
      + (SwapWordLE(PIndexHdr(IndexFile.IndexHeader)^.KeyRecLen) * AEntryNo);
  end
  else
  begin
    // entry out of range, return nil instead of a bad pointer
    Result := nil;
  end;
end;

function TMdxPage.GetLowerPageNo: Integer;
  // *) assumes LowerPage <> nil
begin
  Result := Integer(SwapIntLE(DWORD(PMdxEntry(Entry)^.RecBlockNo)));
end;

function TMdxPage.GetKeyData: PAnsiChar;
begin
  Result := @PMdxEntry(Entry)^.KeyData;
end;

function TMdxPage.GetNumEntries: Integer;
begin
  Result:= Integer(SwapIntLE(DWORD(PMdxPage(PageBuffer)^.NumEntries)));
  if (Result < 0) or (Result > SwapWordLE(PIndexHdr(FIndexFile.FIndexHeader)^.NumKeys)) then
  begin
    Result:= 0;
    FIndexFile.CheckInvalidError;
  end;
end;

function TMdxPage.GetKeyDataFromEntry(AEntry: Integer): PAnsiChar;
begin
  Result := @PMdxEntry(GetEntry(AEntry))^.KeyData;
end;

function TMdxPage.GetRecNo: Integer;
begin
  Result := Integer(SwapIntLE(DWORD(PMdxEntry(Entry)^.RecBlockNo)));
end;

procedure TMdxPage.SetNumEntries(NewNum: Integer);
begin
  PMdxPage(PageBuffer)^.NumEntries := SwapIntLE(NewNum);
end;

procedure TMdxPage.IncNumEntries;
begin
  IncIntLE(PMdxPage(PageBuffer)^.NumEntries, 1);
end;

procedure TMdxPage.SetRecLowerPageNo(NewRecNo, NewPageNo: Integer);
begin
  if FLowerPage = nil then
    PMdxEntry(Entry)^.RecBlockNo := SwapIntLE(NewRecNo)
  else
    PMdxEntry(Entry)^.RecBlockNo := SwapIntLE(NewPageNo);
end;

procedure TMdxPage.SetRecLowerPageNoOfEntry(AEntry, NewRecNo, NewPageNo: Integer);
begin
  if FLowerPage = nil then
    PMdxEntry(GetEntry(AEntry))^.RecBlockNo := SwapIntLE(NewRecNo)
  else
    PMdxEntry(GetEntry(AEntry))^.RecBlockNo := SwapIntLE(NewPageNo);
end;

procedure TMdxPage.SetPrevBlock(NewBlock: Integer);
begin
  PMdxPage(PageBuffer)^.PrevBlock := SwapIntLE(NewBlock);
end;

//==============================================================================
//============ Ndx specific access routines
//==============================================================================

function TNdxPage.GetEntry(AEntryNo: Integer): Pointer;
begin
  if (AEntryNo >= 0) and (AEntryNo <
    SwapWordLE(PIndexHdr(FIndexFile.IndexHeader)^.NumKeys)) then
  begin
    // get base + offset
    Result := PAnsiChar(@PNdxPage(PageBuffer)^.FirstEntry) +
      (SwapWordLE(PIndexHdr(FIndexFile.IndexHeader)^.KeyRecLen) * AEntryNo);
  end
  else
  begin
    // entry out of range, return nil instead of a bad pointer
    Result := nil;
  end;
end;

function TNdxPage.GetLowerPageNo: Integer;
  // *) assumes LowerPage <> nil
begin
  Result := SwapIntLE(PNdxEntry(Entry)^.LowerPageNo)
end;

function TNdxPage.GetRecNo: Integer;
begin
  Result := SwapIntLE(PNdxEntry(Entry)^.RecNo);
end;

function TNdxPage.GetKeyData: PAnsiChar;
begin
  Result := @PNdxEntry(Entry)^.KeyData;
end;

function TNdxPage.GetKeyDataFromEntry(AEntry: Integer): PAnsiChar;
begin
  Result := @PNdxEntry(GetEntry(AEntry))^.KeyData;
end;

function TNdxPage.GetNumEntries: Integer;
begin
  Result := SwapIntLE(PNdxPage(PageBuffer)^.NumEntries);
end;

procedure TNdxPage.IncNumEntries;
begin
  IncIntLE(PNdxPage(PageBuffer)^.NumEntries, 1);
end;

procedure TNdxPage.SetNumEntries(NewNum: Integer);
begin
  PNdxPage(PageBuffer)^.NumEntries := SwapIntLE(NewNum);
end;

procedure TNdxPage.SetRecLowerPageNo(NewRecNo, NewPageNo: Integer);
begin
  PNdxEntry(Entry)^.RecNo := SwapIntLE(NewRecNo);
  PNdxEntry(Entry)^.LowerPageNo := SwapIntLE(NewPageNo);
end;

procedure TNdxPage.SetRecLowerPageNoOfEntry(AEntry, NewRecNo, NewPageNo: Integer);
begin
  PNdxEntry(GetEntry(AEntry))^.RecNo := SwapIntLE(NewRecNo);
  PNdxEntry(GetEntry(AEntry))^.LowerPageNo := SwapIntLE(NewPageNo);
end;

//==============================================================================
//============ MDX version 4 header access routines
//==============================================================================

function TMdx4Tag.GetHeaderPageNo: Integer;
begin
  Result := SwapIntLE(PMdx4Tag(Tag)^.HeaderPageNo);
end;

function TMdx4Tag.GetTagName: string;
begin
  Result := string(PMdx4Tag(Tag)^.TagName);
end;

function TMdx4Tag.GetKeyFormat: Byte;
begin
  Result := PMdx4Tag(Tag)^.KeyFormat;
end;

function TMdx4Tag.GetForwardTag1: Byte;
begin
  Result := PMdx4Tag(Tag)^.ForwardTag1;
end;

function TMdx4Tag.GetForwardTag2: Byte;
begin
  Result := PMdx4Tag(Tag)^.ForwardTag2;
end;

function TMdx4Tag.GetBackwardTag: Byte;
begin
  Result := PMdx4Tag(Tag)^.BackwardTag;
end;

function TMdx4Tag.GetReserved: Byte;
begin
  Result := PMdx4Tag(Tag)^.Reserved;
end;

function TMdx4Tag.GetKeyType: AnsiChar;
begin
  Result := PMdx4Tag(Tag)^.KeyType;
end;

procedure TMdx4Tag.SetHeaderPageNo(NewPageNo: Integer);
begin
  PMdx4Tag(Tag)^.HeaderPageNo := SwapIntLE(NewPageNo);
end;

procedure TMdx4Tag.SetTagName(NewName: string);
begin
  dbfStrPLCopy(PMdx4Tag(Tag)^.TagName, AnsiString(NewName), 10); // Was PChar, AnsiString cast added
  PMdx4Tag(Tag)^.TagName[10] := #0;
end;

procedure TMdx4Tag.SetKeyFormat(NewFormat: Byte);
begin
  PMdx4Tag(Tag)^.KeyFormat := NewFormat;
end;

procedure TMdx4Tag.SetForwardTag1(NewTag: Byte);
begin
  PMdx4Tag(Tag)^.ForwardTag1 := NewTag;
end;

procedure TMdx4Tag.SetForwardTag2(NewTag: Byte);
begin
  PMdx4Tag(Tag)^.ForwardTag2 := NewTag;
end;

procedure TMdx4Tag.SetBackwardTag(NewTag: Byte);
begin
  PMdx4Tag(Tag)^.BackwardTag := NewTag;
end;

procedure TMdx4Tag.SetReserved(NewReserved: Byte);
begin
  PMdx4Tag(Tag)^.Reserved := NewReserved;
end;

procedure TMdx4Tag.SetKeyType(NewType: AnsiChar);
begin
  PMdx4Tag(Tag)^.KeyType := NewType;
end;

//==============================================================================
//============ MDX version 7 headertag access routines
//==============================================================================

function TMdx7Tag.GetHeaderPageNo: Integer;
begin
  Result := SwapIntLE(PMdx7Tag(Tag)^.HeaderPageNo);
end;

function TMdx7Tag.GetTagName: string;
begin
  Result := string(PMdx7Tag(Tag)^.TagName);
end;

function TMdx7Tag.GetKeyFormat: Byte;
begin
  Result := PMdx7Tag(Tag)^.KeyFormat;
end;

function TMdx7Tag.GetForwardTag1: Byte;
begin
  Result := PMdx7Tag(Tag)^.ForwardTag1;
end;

function TMdx7Tag.GetForwardTag2: Byte;
begin
  Result := PMdx7Tag(Tag)^.ForwardTag2;
end;

function TMdx7Tag.GetBackwardTag: Byte;
begin
  Result := PMdx7Tag(Tag)^.BackwardTag;
end;

function TMdx7Tag.GetReserved: Byte;
begin
  Result := PMdx7Tag(Tag)^.Reserved;
end;

function TMdx7Tag.GetKeyType: AnsiChar;
begin
  Result := PMdx7Tag(Tag)^.KeyType;
end;

procedure TMdx7Tag.SetHeaderPageNo(NewPageNo: Integer);
begin
  PMdx7Tag(Tag)^.HeaderPageNo := SwapIntLE(NewPageNo);
end;

procedure TMdx7Tag.SetTagName(NewName: string);
begin
  dbfStrPLCopy(PMdx7Tag(Tag)^.TagName, AnsiString(NewName), 32); // was PChar, AnsiString cast added
  PMdx7Tag(Tag)^.TagName[32] := #0;
end;

procedure TMdx7Tag.SetKeyFormat(NewFormat: Byte);
begin
  PMdx7Tag(Tag)^.KeyFormat := NewFormat;
end;

procedure TMdx7Tag.SetForwardTag1(NewTag: Byte);
begin
  PMdx7Tag(Tag)^.ForwardTag1 := NewTag;
end;

procedure TMdx7Tag.SetForwardTag2(NewTag: Byte);
begin
  PMdx7Tag(Tag)^.ForwardTag2 := NewTag;
end;

procedure TMdx7Tag.SetBackwardTag(NewTag: Byte);
begin
  PMdx7Tag(Tag)^.BackwardTag := NewTag;
end;

procedure TMdx7Tag.SetReserved(NewReserved: Byte);
begin
  PMdx7Tag(Tag)^.Reserved := NewReserved;
end;

procedure TMdx7Tag.SetKeyType(NewType: AnsiChar);
begin
  PMdx7Tag(Tag)^.KeyType := NewType;
end;

{ TDbfIndexParser }

constructor TDbfIndexParser.Create(ADbfFile: Pointer);
begin
  inherited Create(ADbfFile);
  CaseInsensitive := False;
end;

function TDbfIndexParser.IsIndex: Boolean;
begin
  Result := True;
end;

procedure TDbfIndexParser.ValidateExpression(AExpression: string);
var
  TempBuffer: PAnsiChar;
begin
  FResultLen := inherited ResultLen;

  if FResultLen = -1 then
  begin
    // make empty record
    GetMem(TempBuffer, TDbfFile(DbfFile).RecordSize);
    try
      TDbfFile(DbfFile).InitRecord(TempBuffer);
      FExpressionContext.ValidatingIndex := True;
      try
        FResultLen := dbfStrLen(ExtractFromBuffer(TempBuffer, -1));
      finally
        FExpressionContext.ValidatingIndex := False;
      end;
    finally
      FreeMem(TempBuffer);
    end;
  end;

  // check if expression not too long
  if FResultLen > MaxIndexKeyLen then
    raise ExceptionClass.CreateFmt(STRING_INDEX_EXPRESSION_TOO_LONG, [AExpression, FResultLen]);
end;

function TDbfIndexParser.ExceptionClass: ExceptClass;
begin
  Result := EDbfErrorInvalidIndex;
end;

function TDbfIndexParser.GetKeyType: AnsiChar;
var
  lDbfFieldDef: TDbfFieldDef;
begin
  case ResultType of
    etString: Result:= 'C';
    etInteger, etLargeInt, etFloat: Result := 'N';
    etDateTime: Result := 'D';
  else
    raise EDbfError.Create(STRING_INVALID_INDEX_TYPE);
  end;
  lDbfFieldDef:= DbfFieldDef;
  if Assigned(lDbfFieldDef) then
    if lDbfFieldDef.NativeFieldType in ['@', 'O', 'I', '+'] then
      Result:= lDbfFieldDef.NativeFieldType;
end;

//==============================================================================
//============ TIndexFile
//==============================================================================
constructor TIndexFile.Create(ADbfFile: Pointer);
var
  I: Integer;
begin
  inherited Create;

  // clear variables
  FOpened := false;
  FRangeActive := false;
  FUpdateMode := umCurrent;
  FTempMode := TDbfFile(ADbfFile).TempMode;
  FRangeIndex := -1;
  SelectIndexVars(-1);
  for I := 0 to MaxIndexes - 1 do
  begin
    FParsers[I] := nil;
    FRoots[I] := nil;
    FIndexHeaderModified[I] := false;
  end;

  // store pointer to `parent' dbf file
  FDbfFile := ADbfFile;
end;

destructor TIndexFile.Destroy;
begin
  // close file
  Close;

  // call ancestor
  inherited Destroy;
end;

procedure TIndexFile.Open;
var
  I: Integer;
  ext: string;
  localeError: TLocaleError;
  localeSolution: TLocaleSolution;
  DbfLangId: Byte;
  TagSize: Word;
  AHeaderSize: Integer;
begin
  if not FOpened then
  begin
    // open physical file
    OpenFile;

    // page offsets are not related to header length
    PageOffsetByHeader := false;

    // not selected index expression => can't edit yet
    FCanEdit := false;
    FUserKey := nil;
    FUserRecNo := -1;
    FHeaderPageNo := 0;
    FForceClose := false;
    FForceReadOnly := false;
    FMdxTag := nil;

    // get index type
    ext := UpperCase(ExtractFileExt(FileName));
    if (ext = '.MDX') then
    begin
      FEntryHeaderSize := 4;
      FPageHeaderSize := 8;
      FEntryBof := @Entry_Mdx_BOF;
      FEntryEof := @Entry_Mdx_EOF;
      HeaderSize := 2048;
      RecordSize := 1024;
      PageSize := 512;
      if FileCreated then
      begin
        FIndexVersion := TDbfFile(FDbfFile).DbfVersion;
        if FIndexVersion = xBaseIII then
          FIndexVersion := xBaseIV;
        if FIndexVersion = xBaseVII then
          TagSize := SizeOf(rMdx7Tag)
        else
          TagSize := 32;
      end else begin
        case PMdxHdr(Header)^.MdxVersion of
          3: FIndexVersion := xBaseVII;
        else
          FIndexVersion := xBaseIV;
        end;
        RecordSize := PMdxHdr(Header)^.BlockAdder;
        if (PMdxHdr(Header)^.BlockSize = 0) or (PMdxHdr(Header)^.BlockAdder = 0) then
          InvalidError;
        PageSize := PMdxHdr(Header)^.BlockAdder div PMdxHdr(Header)^.BlockSize;
        if SwapWordLE(PMdxHdr(Header)^.TagsUsed) > MaxIndexes then
          InvalidError;
        TagSize:= SwapWordLE(PMdxHdr(Header)^.TagSize);
      end;
      AHeaderSize := 512 + (Succ(MaxIndexes) * TagSize);
      if (AHeaderSize mod RecordSize)<>0 then
        Inc(AHeaderSize, RecordSize - (AHeaderSize mod RecordSize));
      HeaderSize := AHeaderSize;
      case FIndexVersion of
        xBaseVII:
          begin
            FMdxTag := TMdx7Tag.Create;
            FTempMdxTag := TMdx7Tag.Create;
          end;
      else
        FMdxTag := TMdx4Tag.Create;
        FTempMdxTag := TMdx4Tag.Create;
      end;

      // get mem for all index headers..we're going to cache these
      for I := 0 to MaxIndexes - 1 do
      begin
        GetMem(FIndexHeaders[I], RecordSize);
        FillChar(FIndexHeaders[I]^, RecordSize, 0);
      end;
      // set pointers to first index
      FIndexHeader := FIndexHeaders[0];
    end else begin
      // don't waste memory on another header block: we can just use
      // the pagedfile one, there is only one index in this file
      FIndexVersion := xBaseIII;
      FEntryHeaderSize := 8;
      FPageHeaderSize := 4;
      FEntryBof := @Entry_Ndx_BOF;
      FEntryEof := @Entry_Ndx_EOF;
      HeaderSize := 512;
      RecordSize := 512;
      // have to read header first before we can assign following vars
      FIndexHeaders[0] := Header;
      FIndexHeader := Header;
      // create default root
      FParsers[0] := TDbfIndexParser.Create(FDbfFile);
      FRoots[0] := TNdxPage.Create(Self);
      FCurrentParser := FParsers[0];
      FRoot := FRoots[0];
      FSelectedIndex := 0;
      // parse index expression
      FCurrentParser.ParseExpression(String(PIndexHdr(FIndexHeader)^.KeyDesc));
      // set index locale
      FCollation := BINARY_COLLATION;
    end;

    // determine how to open file
    if FileCreated then
    begin
      FillChar(Header^, HeaderSize, 0);
      Clear;
    end else begin
      // determine locale type
      localeError := leNone;
      if (FIndexVersion >= xBaseIV) then
      begin
        // get parent language id
        DbfLangId := GetDbfLanguageId;
        // no ID?
        if (DbfLangId = 0) { and (TDbfFile(FDbfFile).DbfVersion = xBaseIII)} then
        begin
          // if dbf is version 3, no language id, if no MDX language, use binary
          if PMdxHdr(Header)^.Language = 0 then
            FCollation := BINARY_COLLATION
          else
            FCollation := GetCollationTable(PMdxHdr(Header)^.Language);
        end else begin
          // check if MDX - DBF language id's match
          if (PMdxHdr(Header)^.Language = 0) or (PMdxHdr(Header)^.Language = DbfLangId) then
            FCollation := GetCollationTable(DbfLangId)
          else
            localeError := leTableIndexMismatch;
        end;
        // don't overwrite previous error
        if (FCollation = UNKNOWN_COLLATION) and (localeError = leNone) then
          localeError := leUnknown;
      end else begin
        // dbase III always binary?
        FCollation := BINARY_COLLATION;
      end;
      // check if selected locale is available, binary is always available...
      if (localeError <> leNone) and (FCollation <> BINARY_COLLATION) then
      begin
        if LCIDList.IndexOf(Pointer(FCollation)) < 0 then
          localeError := leNotAvailable;
      end;
      // check if locale error detected
      if localeError <> leNone then
      begin
        // provide solution, well, solution...
        localeSolution := lsNotOpen;
        // call error handler
        if Assigned(FOnLocaleError) then
          FOnLocaleError(localeError, localeSolution);
        // act to solution
        case localeSolution of
          lsNotOpen: FForceClose := true;
          lsNoEdit: FForceReadOnly := true;
        else
          { lsBinary }
          FCollation := BINARY_COLLATION;
        end;
      end;
      // now read info
      if not ForceClose then
        ReadIndexes;
    end;
    // default to update all
    UpdateMode := umAll;
    // flag open
    FOpened := true;
  end;
end;

procedure TIndexFile.Close;
var
  I: Integer;
begin
  // save headers
  if FOpened then
    Flush;

  // remove parser reference
  FCurrentParser := nil;

  // free roots
  for I := 0 to MaxIndexes - 1 do
  begin
    if FIndexVersion >= xBaseIV then
      FreeMemAndNil(FIndexHeaders[I]);
    FreeAndNil(FParsers[I]);
    FreeAndNil(FRoots[I]);
    FRoot := nil;
  end;

  // free mem
  FMdxTag.Free;
  FMdxTag := nil;
  FTempMdxTag.Free;
  FTempMdxTag := nil;

  // close physical file
  CloseFile;

  // not opened any more
  FOpened := false;
end;

procedure TIndexFile.ClearRoots;
  //
  // *) assumes FIndexVersion >= xBaseIV
  //
var
  I, prevIndex: Integer;
begin
  prevIndex := FSelectedIndex;
  for I := 0 to MaxIndexes - 1 do
  begin
    SelectIndexVars(I);
    if FRoot <> nil then
    begin
      // clear this entry
      ClearIndex;
    end;
  end;
  // reselect previously selected index
  SelectIndexVars(prevIndex);
  // deselect index
end;

procedure WriteDBFileName(Header: PMdxHdr; HdrFileName: string);
var
  HdrFileExt: string;
  lPos, lenFileName: integer;
begin
  HdrFileName := ExtractFileName(HdrFileName);
  HdrFileExt := ExtractFileExt(HdrFileName);
  if Length(HdrFileExt) > 0 then
  begin
    lPos := System.Pos(HdrFileExt, HdrFileName);
    if lPos > 0 then
      SetLength(HdrFileName, lPos - 1);
  end;
  if Length(HdrFileName) > IndexNameLenMax then
    SetLength(HdrFileName, IndexNameLenMax);
  lenFileName := Length(HdrFileName);
  Move(PAnsiChar(AnsiString(HdrFileName))^, PMdxHdr(Header)^.FileName[0], lenFileName); // Was PChar
  FillChar(PMdxHdr(Header)^.FileName[lenFileName], IndexNameLen - lenFileName, 0);
end;

procedure TIndexFile.Clear;
var
  year, month, day: Word;
  pos, prevSelIndex, pageno: Integer;
  DbfLangId: Byte;
begin
  // flush cache to prevent reading corrupted data
  Flush;
  // completely erase index
  if FIndexVersion >= xBaseIV then
  begin
    DecodeDate(Now, year, month, day);
    if FIndexVersion = xBaseVII then
      PMdxHdr(Header)^.MdxVersion := 3
    else
      PMdxHdr(Header)^.MdxVersion := 2;
    PMdxHdr(Header)^.Year := year - 1900;
    PMdxHdr(Header)^.Month := month;
    PMdxHdr(Header)^.Day := day;
    WriteDBFileName(PMdxHdr(Header), FileName);
    if FileCreated then
    begin
      PMdxHdr(Header)^.BlockSize := SwapWordLE(2);
      PMdxHdr(Header)^.BlockAdder := SwapWordLE(1024);
    end;
    PMdxHdr(Header)^.ProdFlag := 1;
    PMdxHdr(Header)^.NumTags := 48;
    if FIndexVersion = xBaseVII then
      PMdxHdr(Header)^.TagSize := SizeOf(rMdx7Tag)
    else
      PMdxHdr(Header)^.TagSize := 32;
    PMdxHdr(Header)^.NextTag := 1;
    PMdxHdr(Header)^.Language := GetDbfLanguageID;
    PMdxHdr(Header)^.NumPages := SwapIntLE(HeaderSize div PageSize);  // = 4
    TouchHeader(Header);
    PMdxHdr(Header)^.TagFlag := 1;
    // use locale id of parent
    DbfLangId := GetDbfLanguageId;
    if DbfLangId = 0 then
      FCollation := BINARY_COLLATION
    else
      FCollation := GetCollationTable(DbfLangId);
    // write index headers
    prevSelIndex := FSelectedIndex;
    for pos := 0 to SwapWordLE(PMdxHdr(Header)^.TagsUsed) - 1 do
    begin
      SelectIndexVars(pos);
      pageno := GetNewPageNo;
      FMdxTag.HeaderPageNo := SwapIntLE(pageno);
      WriteRecord(pageno, FIndexHeader);
    end;
    // reselect previously selected index
    SelectIndexVars(prevSelIndex);
    // file header done (tags are included in file header)
    WriteFileHeader;
    // clear roots
    ClearRoots;
    // init vars
    if FIndexVersion = xBaseVII then
      FTagSize := SizeOf(rMdx7Tag)
    else
      FTagSize := 32;
    FTagOffset := 544 + FTagSize - 32;
    // clear entries
    RecordCount := SwapIntLE(PMdxHdr(Header)^.NumPages);
  end else begin
    // clear single index entry
    ClearIndex;
    RecordCount := SwapIntLE(PIndexHdr(FIndexHeader)^.NumPages);
  end;
end;

procedure TIndexFile.ClearIndex;
begin
  // flush cache to prevent reading corrupted data
  Flush;
  // initially, we have no page
  PIndexHdr(FIndexHeader)^.NumPages := 0;
  // clear memory of root
  FRoot.Clear;
  // get new page for root
  FRoot.GetNewPage;
  // store new root page
  PIndexHdr(FIndexHeader)^.RootPage := SwapIntLE(FRoot.PageNo);
  PIndexHdr(FIndexHeader)^.FirstNode := SwapIntLE(FRoot.PageNo);
  // write new header
  WriteHeader;
  FRoot.Modified;
  FRoot.WritePage;
end;

procedure TIndexFile.CalcKeyLen;
var
  lKeyLen: Word;
begin
  case FCurrentParser.KeyType of
    'N':
    begin
      if FIndexVersion >= xBaseIV then
        lKeyLen := 12
      else
        lKeyLen := 8;
    end;
    'D': lKeyLen := 8;
    '@': lKeyLen := 8;
    'O': lKeyLen := 8;
    'I': lKeyLen := 4;
    '+': lKeyLen := 4;
  else
    lKeyLen:= FCurrentParser.ResultLen;
  end;
  if lKeyLen > MaxIndexKeyLen then
    raise EDbfError.CreateFmt(STRING_INDEX_EXPRESSION_TOO_LONG, [FCurrentParser.Expression, lKeyLen]);
  PIndexHdr(FIndexHeader)^.KeyLen := SwapWordLE(lKeyLen);
end;

procedure TIndexFile.CalcKeyProperties;
  // given KeyLen, this func calcs KeyRecLen and NumEntries
begin
  // now adjust keylen to align on DWORD boundaries
  PIndexHdr(FIndexHeader)^.KeyRecLen := SwapWordLE((SwapWordLE(
    PIndexHdr(FIndexHeader)^.KeyLen) + FEntryHeaderSize + 3) and not 3);
  PIndexHdr(FIndexHeader)^.NumKeys := SwapWordLE((RecordSize - FPageHeaderSize) div
    SwapWordLE(PIndexHdr(FIndexHeader)^.KeyRecLen));
end;

procedure TIndexFile.CalcRegenerateIndex;
begin
  CalcKeyLen;
  CalcKeyProperties;
end;

function TIndexFile.GetName: string;
begin
  // get suitable name of index: if tag name defined use that otherwise filename
  if FIndexVersion >= xBaseIV then
    Result := FIndexName
  else
    Result := FileName;
end;

procedure TIndexFile.CreateIndex(FieldDesc, TagName: string; Options: TIndexOptions);
var
  tagNo: Integer;
  fieldType: AnsiChar;
  TempParser: TDbfIndexParser;
begin
  // check if we have exclusive access to table
  TDbfFile(FDbfFile).CheckExclusiveAccess;
  // parse index expression; if it cannot be parsed, why bother making index?
  TempParser := TDbfIndexParser.Create(FDbfFile);
  try
    TempParser.ParseExpression(FieldDesc);
    // check if result type is correct
    fieldType := TempParser.KeyType;
  finally
    TempParser.Free;
  end;
  // select empty index
  if FIndexVersion >= xBaseIV then
  begin
    // get next entry no
    tagNo := SwapWordLE(PMdxHdr(Header)^.TagsUsed);
    // check if too many indexes
    if tagNo = MaxIndexes then
      raise EDbfError.Create(STRING_TOO_MANY_INDEXES);
    // get memory for root
    if FRoots[tagNo] = nil then
    begin
      FParsers[tagNo] := TDbfIndexParser.Create(FDbfFile);
      FRoots[tagNo] := TMdxPage.Create(Self)
    end else begin
      FreeAndNil(FRoots[tagNo].FLowerPage);
    end;
    // get pointer to index header
    FIndexHeader := FIndexHeaders[tagNo];
    // load root
    FCurrentParser := FParsers[tagNo];
    FRoot := FRoots[tagNo];
    // create new tag
    FTempMdxTag.Tag := CalcTagOffset(tagNo);
    FTempMdxTag.TagName := IndexNameNormalize(TagName);
    // if expression then calculate
    FTempMdxTag.KeyFormat := KeyFormat_Data;
    if ixExpression in Options then
      FTempMdxTag.KeyFormat := KeyFormat_Expression;
    // what use have these reference tags?
    FTempMdxTag.ForwardTag1 := 0;
    FTempMdxTag.ForwardTag2 := 0;
    FTempMdxTag.BackwardTag := 0;
    FTempMdxTag.Reserved := 2;
    FTempMdxTag.KeyType := fieldType;
    // save this part of tag, need to save before GetNewPageNo,
    // it will reread header
    WriteFileHeader;
    // store selected index
    FSelectedIndex := tagNo;
    FIndexName := TagName;
    // store new headerno
    FHeaderPageNo := GetNewPageNo;
    FTempMdxTag.HeaderPageNo := FHeaderPageNo;
    // increase number of indexes active
    IncWordLE(PMdxHdr(Header)^.TagsUsed, 1);
    PMdxHdr(Header)^.NextTag := Succ(SwapWordLE(PMdxHdr(Header)^.TagsUsed));
    // update updatemode
    UpdateMode := umAll;
    // index header updated
    WriteFileHeader;
  end;
  // clear index
  ClearIndex;

  // parse expression, we know it's parseable, we've checked that
  FCurrentParser.ParseExpression(FieldDesc);

  // looked up index expression: now we can edit
//  FIsExpression := ixExpression in Options;
  FCanEdit := not FForceReadOnly;

  // init key variables
  PIndexHdr(FIndexHeader)^.KeyFormat := 0;
  // descending
  if ixDescending in Options then
    PIndexHdr(FIndexHeader)^.KeyFormat := PIndexHdr(FIndexHeader)^.KeyFormat or KeyFormat_Descending;
  // key type
  if fieldType = 'C' then
    PIndexHdr(FIndexHeader)^.KeyFormat := PIndexHdr(FIndexHeader)^.KeyFormat or KeyFormat_String;
  PIndexHdr(FIndexHeader)^.KeyType := fieldType;
  // uniqueness
  PIndexHdr(FIndexHeader)^.Unique := Unique_None;
  if ixPrimary in Options then
  begin
    PIndexHdr(FIndexHeader)^.KeyFormat := PIndexHdr(FIndexHeader)^.KeyFormat or KeyFormat_Distinct or KeyFormat_Unique;
    PIndexHdr(FIndexHeader)^.Unique := Unique_Distinct;
  end else if ixUnique in Options then
  begin
    PIndexHdr(FIndexHeader)^.KeyFormat := PIndexHdr(FIndexHeader)^.KeyFormat or KeyFormat_Unique;
    PIndexHdr(FIndexHeader)^.Unique := Unique_Unique;
  end;
  // keylen is exact length of field
  CalcKeyLen;
  CalcKeyProperties;
  // key desc
  dbfStrPLCopy(PIndexHdr(FIndexHeader)^.KeyDesc, AnsiString(FieldDesc), 219); // Was PChar, AnsiString cast added
  PIndexHdr(FIndexHeader)^.KeyDesc[219] := #0;

  // init various
  PIndexHdr(FIndexHeader)^.Dummy := 0;
  PIndexHdr(FIndexHeader)^.Language := GetDbfLanguageID;
  case fieldType of
    'C':
      PIndexHdr(FIndexHeader)^.sKeyType := 0;
    'D':
      PIndexHdr(FIndexHeader)^.sKeyType := SwapWordLE(1);
    'N', 'F':
      if FIndexVersion >= xBaseIV then
        PIndexHdr(FIndexHeader)^.sKeyType := 0
      else
        PIndexHdr(FIndexHeader)^.sKeyType := SwapWordLE(1);
  else
    PIndexHdr(FIndexHeader)^.sKeyType := 0;
  end;

  PIndexHdr(FIndexHeader)^.Version := 1; // this is what the BDE uses for the first version (SwapWordLE not needed, Version declared as Byte)
  PIndexHdr(FIndexHeader)^.Dummy2 := 0;
  PIndexHdr(FIndexHeader)^.Dummy3 := 0;
  PIndexHdr(FIndexHeader)^.ForExist := 0;    // false
  PIndexHdr(FIndexHeader)^.KeyExist := 1;    // true
  WriteHeader;

  // update internal properties
  UpdateIndexProperties;

  // for searches / inserts / deletes
  FKeyBuffer[SwapWordLE(PIndexHdr(FIndexHeader)^.KeyLen)] := #0;
end;

procedure TIndexFile.ReadIndexes;
var
  I: Integer;
  size: Integer;

  procedure CheckHeaderIntegrity;
  begin
    if (SwapWordLE(PIndexHdr(FIndexHeader)^.KeyLen) > MaxIndexKeyLen) or (PIndexHdr(FIndexHeader)^.KeyLen = 0) then
      InvalidError;
    if Integer(SwapWordLE(PIndexHdr(FIndexHeader)^.NumKeys) * SwapWordLE(PIndexHdr(FIndexHeader)^.KeyRecLen)) > RecordSize then
      InvalidError;
  end;

begin
  // force header reread
  size := inherited ReadHeader;
  // examine all indexes
  if FIndexVersion >= xBaseIV then
  begin
    // tags are extended at beginning? tagsize is byte sized
    FTagSize := SwapWordLE(PMdxHdr(Header)^.TagSize);
    FTagOffset := 544 + FTagSize - 32;
    if FTagOffset + (SwapWordLE(PMdxHdr(Header)^.TagsUsed) * FTagSize) > size then
      InvalidError;
    if FTagOffset + (MaxIndexes * FTagSize) > HeaderSize then
      InvalidError;
    // clear all roots
    ClearRoots;
    for I := 0 to SwapWordLE(PMdxHdr(Header)^.TagsUsed) - 1 do
    begin
      // read page header
      FTempMdxTag.Tag := CalcTagOffset(I);
      ReadRecord(FTempMdxTag.HeaderPageNo, FIndexHeaders[I]);
      // select it
      FIndexHeader := FIndexHeaders[I];
      // create root if needed
      if FRoots[I] = nil then
      begin
        FParsers[I] := TDbfIndexParser.Create(FDbfFile);
        FRoots[I] := TMdxPage.Create(Self);
      end;
      // check header integrity
      CheckHeaderIntegrity;
      // read tree
      FRoots[I].PageNo := SwapIntLE(PIndexHdr(FIndexHeader)^.RootPage);
      // go to first record
      FRoots[I].RecurFirst;
      // parse expression
      try
        FParsers[I].ParseExpression(string(AnsiString(PIndexHdr(FIndexHeader)^.KeyDesc)));
      except
        {$IFDEF TDBF_IGNORE_INVALID_INDICES}
        on E: EDbfErrorInvalidIndex do
          // ignore invalid indexes so we can load tables with broken index files
        else
        {$ENDIF}
          raise;
      end;
    end;
  end else begin
    // clear root
    FRoot.Clear;
    // check recordsize constraint
    CheckHeaderIntegrity;
    // just one index: read tree
    FRoot.PageNo := SwapIntLE(PIndexHdr(FIndexHeader)^.RootPage);
    // go to first valid record
    FRoot.RecurFirst;
    // get index properties -> internal props
    UpdateIndexProperties;
  end;
end;

procedure TIndexFile.DeleteIndex(const AIndexName: string);
var
  I, found, numTags, moveItems: Integer;
  tempHeader: Pointer;
  tempRoot: TIndexPage;
  tempParser: TDbfIndexParser;
begin
  // check if we have exclusive access to table
  TDbfFile(FDbfFile).CheckExclusiveAccess;
  if FIndexVersion = xBaseIII then
  begin
    Close;
    DeleteFile;
  end else if FIndexVersion >= xBaseIV then
  begin
    // find index
    found := IndexOf(AIndexName);
    if found >= 0 then
    begin
      // just remove this tag by copying memory over it
      numTags := SwapWordLE(PMdxHdr(Header)^.TagsUsed);
      moveItems := numTags - found - 1;
      // anything to move?
      if moveItems > 0 then
      begin
        // move entries after found one
        Move((Header + FTagOffset + (found+1) * FTagSize)^,
          (Header + FTagOffset + found * FTagSize)^, moveItems * FTagSize);
        // nullify last entry
        FillChar((Header + FTagOffset + numTags * FTagSize)^, FTagSize, 0);
        // index headers, roots, leaves
        tempHeader := FIndexHeaders[found];
        tempParser := FParsers[found];
        tempRoot := FRoots[found];
        for I := 0 to moveItems - 1 do
        begin
          FIndexHeaders[found + I] := FIndexHeaders[found + I + 1];
          FParsers[found + I] := FParsers[found + I + 1];
          FRoots[found + I] := FRoots[found + I + 1];
          FIndexHeaderModified[found + I] := true;
        end;
        FIndexHeaders[found + moveItems] := tempHeader;
        FParsers[found + moveItems] := tempParser;
        FRoots[found + moveItems] := tempRoot;
        FIndexHeaderModified[found + moveItems] := false;    // non-existant header
      end;
      // one entry less left
      IncWordLE(PMdxHdr(Header)^.TagsUsed, -1);
      // ---*** numTags not valid from here ***---
      // file header changed
      WriteFileHeader;
    end;
  end;
end;

procedure TIndexFile.TouchHeader(AHeader: Pointer);
var
  year, month, day: Word;
begin
  DecodeDate(Now, year, month, day);
  PMdxHdr(AHeader)^.UpdYear := year - 1900;
  PMdxHdr(AHeader)^.UpdMonth := month;
  PMdxHdr(AHeader)^.UpdDay := day;
end;

function TIndexFile.CreateTempFile(BaseName: string): TPagedFile;
var
  lModifier: Integer;
begin
  // create temporary in-memory index file
  lModifier := 0;
  FindNextName(BaseName, BaseName, lModifier);
  Result := TPagedFile.Create;
  Result.FileName := BaseName;
  Result.Mode := pfExclusiveCreate;
  Result.AutoCreate := true;
  Result.OpenFile;
  Result.HeaderSize := HeaderSize;
  Result.RecordSize := RecordSize;
  Result.PageSize := PageSize;
  Result.PageOffsetByHeader := false;
end;

procedure TIndexFile.RepageFile;
var
  TempFile: TPagedFile;
  TempIdxHeader: PIndexHdr;
  I, newPageNo: Integer;
  prevIndex: Integer;

  function  AllocNewPageNo: Integer;
  begin
    Result := newPageNo;
    Inc(newPageNo, PagesPerRecord);
    if FIndexVersion >= xBaseIV then
      IncIntLE(PMdxHdr(TempFile.Header)^.NumPages, PagesPerRecord);
    IncIntLE(TempIdxHeader^.NumPages, PagesPerRecord);
  end;

  function WriteTree(NewPage: TIndexPage): Integer;
  var
    J: Integer;
  begin
    // get us a page so that page no's are more logically ordered
    Result := AllocNewPageNo;
    // use postorder visiting, first do all children
    if NewPage.LowerPage <> nil then
    begin
      for J := 0 to NewPage.HighIndex do
      begin
        NewPage.EntryNo := J;
        WriteTree(NewPage.LowerPage);
      end;
    end;
    // now create new page for ourselves and write
    // update page pointer in parent
    if NewPage.UpperPage <> nil then
    begin
      if FIndexVersion >= xBaseIV then
      begin
        PMdxEntry(NewPage.UpperPage.Entry)^.RecBlockNo := SwapIntLE(Result);
        // write previous node
        if FRoot = NewPage then
          PMdxPage(NewPage.PageBuffer)^.PrevBlock := 0
        else
          PMdxPage(NewPage.PageBuffer)^.PrevBlock := SwapIntLE(Result - PagesPerRecord);
      end else begin
        PNdxEntry(NewPage.UpperPage.Entry)^.LowerPageNo := SwapIntLE(Result);
      end;
    end;
    // store page
    TempFile.WriteRecord(Result, NewPage.PageBuffer);
  end;

  procedure CopySelectedIndex;
  var
    hdrPageNo: Integer;
  begin
    // copy current index settings
    Move(FIndexHeader^, TempIdxHeader^, RecordSize);
    // clear number of pages
    TempIdxHeader^.NumPages := 0;
    // allocate a page no for header
    hdrPageNo := AllocNewPageNo;
    // use recursive function to write all pages
    TempIdxHeader^.RootPage := SwapIntLE(WriteTree(FRoot));
    TempIdxHeader^.FirstNode := TempIdxHeader^.RootPage;
    // write index header now we know the root page
    TempFile.WriteRecord(hdrPageNo, TempIdxHeader);
    if FIndexVersion >= xBaseIV then
    begin
      // calculate tag offset in tempfile header
      FTempMdxTag.Tag := PAnsiChar(TempFile.Header) + (PAnsiChar(CalcTagOffset(I)) - Header); // was PChar
      FTempMdxTag.HeaderPageNo := hdrPageNo;
    end;
  end;

begin
  CheckExclusiveAccess;

  prevIndex := FSelectedIndex;
  newPageNo := HeaderSize div PageSize;
  TempFile := CreateTempFile(FileName);
  if FIndexVersion >= xBaseIV then
  begin
    // copy header
    Move(Header^, TempFile.Header^, HeaderSize);
    TouchHeader(TempFile.Header);
    // reset header
    PMdxHdr(TempFile.Header)^.NumPages := SwapIntLE(HeaderSize div PageSize);
    TempFile.WriteHeader;
    GetMem(TempIdxHeader, RecordSize);
    // now recreate indexes to that file
    for I := 0 to SwapWordLE(PMdxHdr(Header)^.TagsUsed - 1) do
    begin
      // select this index
      SelectIndexVars(I);
      CopySelectedIndex;
    end;
    FreeMem(TempIdxHeader);
  end else begin
    // indexversion = xBaseIII
    TempIdxHeader := PIndexHdr(TempFile.Header);
    CopySelectedIndex;
  end;
  TempFile.WriteHeader;
  TempFile.CloseFile;
  CloseFile;

  // rename temporary file if all went successfull
  if not TempFile.WriteError then
  begin
    SysUtils.DeleteFile(FileName);
    SysUtils.RenameFile(TempFile.FileName, FileName);
  end;

  TempFile.Free;
  DisableForceCreate;
  OpenFile;
  ReadIndexes;
  SelectIndexVars(prevIndex);
end;

procedure TIndexFile.CompactFile;
var
  TempFile: TPagedFile;
  TempIdxHeader: PIndexHdr;
  I, newPageNo: Integer;
  prevIndex: Integer;

  function  AllocNewPageNo: Integer;
  begin
    Result := newPageNo;
    Inc(newPageNo, PagesPerRecord);
    if FIndexVersion >= xBaseIV then
      IncIntLE(PMdxHdr(TempFile.Header)^.NumPages, PagesPerRecord);
    IncIntLE(TempIdxHeader^.NumPages, PagesPerRecord);
  end;

  function  CreateNewPage: TIndexPage;
  begin
    // create new page + space
    if FIndexVersion >= xBaseIV then
      Result := TMdxPage.Create(Self)
    else
      Result := TNdxPage.Create(Self);
    Result.FPageNo := AllocNewPageNo;

    // set new page properties
    Result.SetNumEntries(0);
  end;

  procedure GetNewEntry(APage: TIndexPage);
    // makes a new entry available and positions current 'pos' on it
    // NOTES: uses TIndexPage *very* carefully
    //  - may not read from self (tindexfile)
    //  - page.FLowerPage is assigned -> SyncLowerPage may *not* be called
    //  - do not set PageNo (= SetPageNo)
    //  - do not set EntryNo
  begin
    if APage.HighIndex >= SwapWordLE(PIndexHdr(FIndexHeader)^.NumKeys)-1 then
    begin
      if APage.UpperPage = nil then
      begin
        // add new upperlevel to page
        APage.FUpperPage := CreateNewPage;
        APage.UpperPage.FLowerPage := APage;
        APage.UpperPage.FEntryNo := 0;
        APage.UpperPage.FEntry := EntryEof;
        APage.UpperPage.GotoInsertEntry;
        APage.UpperPage.LocalInsert(0, APage.Key, APage.PageNo);
        // non-leaf pages need 'rightmost' key; numentries = real# - 1
        APage.UpperPage.SetNumEntries(0);
      end;

      // page done, store
      TempFile.WriteRecord(APage.FPageNo, APage.PageBuffer);

      // allocate new page
      APage.FPageNo := AllocNewPageNo;
      // clear
      APage.SetNumEntries(0);
      APage.FHighIndex := -1;
      APage.FLowIndex := 0;
      // clear 'right-most' blockno
      APage.SetRecLowerPageNoOfEntry(0, 0, 0);

      // get new entry in upper page for current new apage
      GetNewEntry(APage.UpperPage);
      APage.UpperPage.LocalInsert(0, nil, 0);
      // non-leaf pages need 'rightmost' key; numentries = real# - 1
      if APage.UpperPage.EntryNo = 0 then
        APage.UpperPage.SetNumEntries(0);
    end;
    APage.FEntryNo := APage.HighIndex+1;
    APage.FEntry := EntryEof;
    APage.GotoInsertEntry;
  end;

  procedure CopySelectedIndex;
  var
    APage: TIndexPage;
    hdrPageNo: Integer;
  begin
    // copy current index settings
    Move(FIndexHeader^, TempIdxHeader^, RecordSize);
    // clear number of pages
    TempIdxHeader^.NumPages := 0;
    // allocate a page no for header
    hdrPageNo := AllocNewPageNo;

    // copy all records
    APage := CreateNewPage;
    Leaf.RecurFirst;
    while not (FRoot.Entry = FEntryEof) do
    begin
      GetNewEntry(APage);
      APage.LocalInsert(Leaf.PhysicalRecNo, Leaf.Key, 0);
      Leaf.RecurNext;
    end;

    // flush remaining (partially filled) pages
    repeat
      TempFile.WriteRecord(APage.FPageNo, APage.PageBuffer);
      if APage.UpperPage <> nil then
        APage := APage.UpperPage
      else break;
    until false;

    // copy index header + root page
    TempIdxHeader^.RootPage := SwapIntLE(APage.PageNo);
    TempIdxHeader^.FirstNode := SwapIntLE(APage.PageNo);
    // write index header now we know the root page
    TempFile.WriteRecord(hdrPageNo, TempIdxHeader);
    if FIndexVersion >= xBaseIV then
    begin
      // calculate tag offset in tempfile header
      FTempMdxTag.Tag := PAnsiChar(TempFile.Header) + (PAnsiChar(CalcTagOffset(I)) - Header); // Was PChar
      FTempMdxTag.HeaderPageNo := hdrPageNo;
    end;
  end;

begin
  CheckExclusiveAccess;

  prevIndex := FSelectedIndex;
  newPageNo := HeaderSize div PageSize;
  TempFile := CreateTempFile(FileName);
  if FIndexVersion >= xBaseIV then
  begin
    // copy header
    Move(Header^, TempFile.Header^, HeaderSize);
    TouchHeader(TempFile.Header);
    // reset header
    PMdxHdr(TempFile.Header)^.NumPages := SwapIntLE(HeaderSize div PageSize);
    TempFile.WriteHeader;
    GetMem(TempIdxHeader, RecordSize);
    // now recreate indexes to that file
    for I := 0 to SwapWordLE(PMdxHdr(Header)^.TagsUsed) - 1 do
    begin
      // select this index
      SelectIndexVars(I);
      CopySelectedIndex;
    end;
    FreeMem(TempIdxHeader);
  end else begin
    // indexversion = xBaseIII
    TempIdxHeader := PIndexHdr(TempFile.Header);
    CopySelectedIndex;
  end;
  TempFile.WriteHeader;
  TempFile.CloseFile;
  CloseFile;

  // rename temporary file if all went successfull
  if not TempFile.WriteError then
  begin
    SysUtils.DeleteFile(FileName);
    SysUtils.RenameFile(TempFile.FileName, FileName);
  end;

  TempFile.Free;
  DisableForceCreate;
  OpenFile;
  ReadIndexes;
  SelectIndexVars(prevIndex);
end;

const
  BulkLoadMemoryTotal = $8000000; {128 MB}
  BulkLoadMemoryAllocSize = $100000; {1 MB}

procedure TIndexFile.BulkLoadIndex;
var
  ADbfFile: TDbfFile;
  BufferMax: Integer;
  BufferCount: Integer;
  EntryIndex: Integer;
  EntryCount: Integer;
  EntryMax: Integer;
  PPEntries: Pointer;
  PPEntry: Pointer;
  PEntry: PMdxEntry;
  KeyRecLen: Word;
  KeyLen: Word;
  Len: Word;
  BufferList: TList;
  Index: Integer;
  RecNoReading: Integer;
  RecordCountReading: Integer;
begin
  ADbfFile := TDbfFile(FDbfFile);
  RecordCountReading := ADbfFile.RecordCount;
  KeyRecLen := SwapWordLE(PIndexHdr(FIndexHeader)^.KeyRecLen);
  EntryMax := BulkLoadMemoryTotal div KeyRecLen;
  if EntryMax > RecordCountReading then
    EntryMax := RecordCountReading;
  BufferMax := BulkLoadMemoryAllocSize div KeyRecLen;
  GetMem(PPEntries, EntryMax * SizeOf(Pointer));
  PEntry := nil;
  try
    FillChar(PPEntries^, EntryMax * SizeOf(Pointer), 0);
    BufferList := TList.Create;
    try
      KeyLen := SwapWordLE(PIndexHdr(FIndexHeader)^.KeyLen);
      RecNoReading := 0;
      DoProgress(RecNoReading, RecordCountReading, STRING_PROGRESS_READINGRECORDS);
      while RecNoReading < RecordCountReading do
      begin
        PPEntry := PPEntries;
        EntryCount := 0;
        while (RecNoReading < RecordCountReading) and (EntryCount < EntryMax) do
        begin
          if RecNoReading < EntryMax then
          begin
            if (EntryCount mod BufferMax) = 0 then
            begin
              PEntry := PMdxEntry(PPEntry^);
              if PEntry = nil then
              begin
                BufferCount := EntryMax - EntryCount;
                if BufferCount > BufferMax then
                  BufferCount := BufferMax;
                GetMem(PEntry, BufferCount * KeyRecLen);
                try
                  BufferList.Add(PEntry);
                except
                  FreeMem(PEntry);
                  raise;
                end;
              end;
            end
            else
              Inc(PAnsiChar(PEntry), KeyRecLen);
            PMdxEntry(PPEntry^) := PEntry;
          end
          else
            PEntry := PMdxEntry(PPEntry^);
          Inc(RecNoReading);
          FillChar(PEntry^, KeyRecLen, 0);
          ADbfFile.ReadRecord(RecNoReading, ADbfFile.PrevBuffer);
          if RecordIsIndexed(ADbfFile.PrevBuffer) then
            FUserKey := ExtractKeyFromBuffer(ADbfFile.PrevBuffer, RecNoReading)
          else
            FUserKey := nil;
          if Assigned(FUserKey) then
          begin
            PEntry^.RecBlockNo := RecNoReading;
            Len := KeyLen;
            if (FCurrentParser.ResultType=etString) and (Len>FCurrentParser.ResultBufferSize) then
              Len:= FCurrentParser.ResultBufferSize;
            Move(FUserKey^, PEntry^.KeyData, Len);
          end;
          Inc(PAnsiChar(PPEntry), SizeOf(Pointer));
          Inc(EntryCount);
          DoProgress(RecNoReading, RecordCountReading, STRING_PROGRESS_READINGRECORDS);
        end;
        FProgressPosition := 0;
        FProgressMax := -1;
        DoProgress(FProgressPosition, FProgressMax, STRING_PROGRESS_SORTING_RECORDS);
        MergeSort(PPEntries, 0, Pred(EntryCount));
        FProgressPosition := 0;
        FProgressMax := EntryCount;
        DoProgress(FProgressPosition, FProgressMax, STRING_PROGRESS_WRITING_RECORDS);
        EntryIndex := 0;
        PPEntry := PPEntries;
        while EntryIndex < EntryCount do
        begin
          PEntry := PMdxEntry(PPEntry^);
          if PEntry^.RecBlockNo <> 0 then
          begin
            FUserRecNo := PEntry^.RecBlockNo;
            FUserKey := @PEntry^.KeyData;
            if not InsertCurrent then
              InsertError;
          end;
          Inc(PAnsiChar(PPEntry), SizeOf(Pointer));
          Inc(EntryIndex);
          Inc(FProgressPosition);
          DoProgress(FProgressPosition, FProgressMax, STRING_PROGRESS_WRITING_RECORDS);
        end;
      end;
    finally
      for Index:= 0 to Pred(BufferList.Count) do
        FreeMem(BufferList[Index]);
      BufferList.Free;
    end;
  finally
    FreeMem(PPEntries);
  end;
end;

procedure TIndexFile.BulkLoadIndexes;
var
  curSel: Integer;
  I: Integer;
begin
  if (FUpdateMode = umAll) or (FSelectedIndex = -1) then
  begin
    curSel := FSelectedIndex;
    try
      I := 0;
      while I < SwapWordLE(PMdxHdr(Header)^.TagsUsed) do
      begin
        SelectIndexVars(I);
        BulkLoadIndex;
        Inc(I);
      end;
    finally
      SelectIndexVars(curSel);
    end;
  end
  else
    BulkLoadIndex;
end;

procedure TIndexFile.PrepareRename(NewFileName: string);
begin
  if FIndexVersion >= xBaseIV then
  begin
    WriteDBFileName(PMdxHdr(Header), NewFileName);
    WriteFileHeader;
  end;
end;

procedure TIndexFile.CalcRegenerateIndexes;

var
  curSel: Integer;
  I: Integer;
begin
  if (FUpdateMode = umAll) or (FSelectedIndex = -1) then
  begin
    curSel := FSelectedIndex;
    try
      I := 0;
      while I < SwapWordLE(PMdxHdr(Header)^.TagsUsed) do
      begin
        SelectIndexVars(I);
        CalcRegenerateIndex;
        Inc(I);
      end;
    finally
      SelectIndexVars(curSel);
    end;
  end
  else
    CalcRegenerateIndex;
end;

function TIndexFile.GetNewPageNo: Integer;
begin
  if FIndexVersion >= xBaseIV then
  begin
    Result := SwapIntLE(PMdxHdr(Header)^.NumPages);
    IncIntLE(PMdxHdr(Header)^.NumPages, PagesPerRecord);
    WriteFileHeader;
  end else begin
    Result := SwapIntLE(PIndexHdr(FIndexHeader)^.NumPages);
    Inc(Result, PagesPerRecord);
  end;
  IncIntLE(PIndexHdr(FIndexHeader)^.NumPages, PagesPerRecord);
  // adjust high page
  PIndexHdr(FIndexHeader)^.LastNode := SwapIntLE(Result);
  WriteHeader;
end;

function TIndexFile.Insert(RecNo: Integer; Buffer: TDbfRecordBuffer): Boolean; {override;}
var
  I, curSel, count: Integer;
begin
  // check if updating all or only current
  FUserRecNo := RecNo;
  if (FUpdateMode = umAll) or (FSelectedIndex = -1) then
  begin
    // remember currently selected index
    curSel := FSelectedIndex;
    Result := true;
    I := 0;
    count := SwapWordLE(PMdxHdr(Header)^.TagsUsed);
    while I < count do
    begin
      SelectIndexVars(I);
      LockIndex;
      try
        Result := InsertKey(Buffer, RecNo);
        if not Result then
        begin
          while I > 0 do
          begin
            Dec(I);
            SelectIndexVars(I);
            DeleteKey(Buffer, RecNo);
          end;
          break;
        end;
        // check range, disabled by insert
        ResyncRange(true);
      finally
        UnlockIndex;
      end;
      Inc(I);
    end;
    // restore previous selected index
    SelectIndexVars(curSel);
  end else begin
    LockIndex;
    try
      Result := InsertKey(Buffer, RecNo);
      // check range, disabled by insert
      ResyncRange(true);
    finally
      UnlockIndex;
    end;
  end;
end;

function TIndexFile.CheckKeyViolation(Buffer: TDbfRecordBuffer; RecNo: Integer): Boolean;
var
  I, curSel: Integer;
begin
  Result := false;
  FUserRecNo := -2;
  if FIndexVersion >= xBaseIV then
  begin
    curSel := FSelectedIndex;
    for I := 0 to SwapWordLE(PMdxHdr(Header)^.TagsUsed) - 1 do
    begin
      SelectIndexVars(I);
      if FUniqueMode = iuDistinct then
      begin
        FUserKey := ExtractKeyFromBuffer(Buffer, RecNo);
        Result := FindKey(false) = 0;
        if Result then
          break;
      end;
    end;
    SelectIndexVars(curSel);
  end else begin
    if FUniqueMode = iuDistinct then
    begin
      FUserKey := ExtractKeyFromBuffer(Buffer, RecNo);
      Result := FindKey(false) = 0;
    end;
  end;
end;

function TIndexFile.PrepareKey(Buffer: TDbfRecordBuffer; ResultType: TExpressionType): TDbfRecordBuffer;
const
{$ifdef FLOATREC_DIGITS_IS_BYTE}
  ZERO_CHAR = Ord('0');
  NULL_TERMINATOR = 0;
{$else}
  ZERO_CHAR = '0';
  NULL_TERMINATOR = #0;
{$endif}
var
  FloatRec: TFloatRec;
  I, IntSrc, NumDecimals: Integer;
  ExtValue: Extended;
  BCDdigit: Byte;
{$ifdef SUPPORT_INT64}
  Int64Src: Int64;
{$endif}
begin
  // need to convert numeric?
  Result := nil;
  if PIndexHdr(FIndexHeader)^.KeyType = 'C' then
    Result := Buffer
  else
  if CharInSet(PIndexHdr(FIndexHeader)^.KeyType, ['F', 'N']) then
  begin
    if FIndexVersion = xBaseIII then
    begin
      // DB3 -> index always 8 byte float, if original integer, convert to double
      case ResultType of
        etInteger:
          begin
            FUserNumeric := PInteger(Buffer)^;
            Result := TDbfRecordBuffer(@FUserNumeric);
          end;
        etFloat: Result:= Buffer;
{$ifdef SUPPORT_INT64}
        etLargeInt:
          begin
            FUserNumeric := PLargeInt(Buffer)^;
            Result := TDbfRecordBuffer(@FUserNumeric);
          end;
{$endif}
      end;
    end else begin
      // DB4 MDX
      NumDecimals := 0;
      case ResultType of
        etInteger: 
          begin
            IntSrc := PInteger(Buffer)^;
            // handle zero differently: no decimals
            if IntSrc <> 0 then
              NumDecimals := IntToStrWidth(IntSrc, SizeOf(FloatRec.Digits), @FloatRec.Digits[0], False, #0)
            else
              NumDecimals := 0;
            FloatRec.Negative := IntSrc < 0;
          end;
{$ifdef SUPPORT_INT64}
        etLargeInt:
          begin
            Int64Src := PLargeInt(Buffer)^;
            if Int64Src <> 0 then
              NumDecimals := IntToStrWidth(Int64Src, SizeOf(FloatRec.Digits), @FloatRec.Digits[0], False, #0)
            else
              NumDecimals := 0;
            FloatRec.Negative := Int64Src < 0;
          end;
{$endif}
        etFloat:
          begin
            ExtValue := PDouble(Buffer)^;
            FloatToDecimal(FloatRec, ExtValue, {$ifndef FPC_VERSION}fvExtended,{$endif} 15, 9999);
            if ExtValue <> 0.0 then
              NumDecimals := dbfStrLen(PAnsiChar(@FloatRec.Digits[0]))
            else
              NumDecimals := 0;
            // maximum number of decimals possible to encode in BCD is 16
            if NumDecimals > 16 then
              NumDecimals := 16;
          end;
      end;

      case ResultType of
        etInteger {$ifdef SUPPORT_INT64}, etLargeInt{$endif}:
          begin
            FloatRec.Exponent := NumDecimals;
            // MDX-BCD does not count ending zeroes as `data' space length
            while (NumDecimals > 0) and (FloatRec.Digits[NumDecimals-1] = ZERO_CHAR) do
              Dec(NumDecimals);
            // null-terminate string
            FloatRec.Digits[NumDecimals] := NULL_TERMINATOR;
          end;
      end;

      // write 'header', contains number of digits before decimal separator
      FUserBCD[0] := $34 + FloatRec.Exponent;
      // clear rest of BCD
      FillChar(FUserBCD[1], SizeOf(FUserBCD)-1, 0);
      // store number of bytes used (in number of bits + 1)
      FUserBCD[1] := (((NumDecimals+1) div 2) * 8) + 1;
      // where to store decimal dot position? now implicitly in first byte
      // store negative sign
      if FloatRec.Negative then
        FUserBCD[1] := FUserBCD[1] or $80;
      // convert string to BCD
      I := 0;
      while I < NumDecimals do
      begin             
        // only one byte left?
        if I+1>=NumDecimals then
          BCDdigit := 0
        else
          BCDdigit := Byte(FloatRec.Digits[I+1]) - Byte('0');
        // pack two bytes into bcd
        FUserBCD[2+(I div 2)] := ((Byte(FloatRec.Digits[I]) - Byte('0')) shl 4) or BCDdigit;
        // goto next 2 bytes
        Inc(I, 2);
      end;

      // set result pointer to BCD
      Result := TDbfRecordBuffer(@FUserBCD[0]);
    end;
  end
  else
  if PIndexHdr(FIndexHeader)^.KeyType = 'D' then
  begin
    FUserNumeric:= PDouble(Buffer)^ - DateJulianEpoch; {Julian date}
    Result:= TDbfRecordBuffer(@FUserNumeric);
  end;
end;

function TIndexFile.ExtractKeyFromBuffer(Buffer: TDbfRecordBuffer; RecNo: Integer): PAnsiChar;
var
  KeyBuffer: PAnsiChar;
  DbfFieldDef: TDbfFieldDef;
  IsNull: Boolean;
begin
  // execute expression to get key
  if KeyType in ['@', 'O', 'I', '+'] then
  begin
    Result := nil;
    DbfFieldDef := FCurrentParser.DbfFieldDef;
    if Assigned(DbfFieldDef) then
      if DbfFieldDef.NativeFieldType = PIndexHdr(FIndexHeader)^.KeyType then
        Result := PAnsiChar(Buffer) + DbfFieldDef.Offset;
  end
  else
  begin
    IsNull := False;
    KeyBuffer := FCurrentParser.ExtractFromBuffer(PAnsiChar(Buffer), RecNo, IsNull);
    if (KeyType = 'D') and IsNull then
      PDouble(KeyBuffer)^ := DateNullValue;
    Result := PAnsiChar(PrepareKey(TDbfRecordBuffer(KeyBuffer), FCurrentParser.ResultType));
  end;
  if not Assigned(Result) then
    raise EDbfError.Create(STRING_INVALID_INDEX_TYPE);
  if not FCurrentParser.RawStringFields then
    TranslateString(GetACP, FCodePage, Result, Result, KeyLen);
end;

procedure TIndexFile.ExtractKey(Key: PAnsiChar);
begin
  Move(Leaf.Key^, Key^, KeyLen);
end;

function TIndexFile.CopyCurrentKey(Source, Dest: PAnsiChar): Integer;
begin
  Result := KeyLen;
  FillChar(Dest^, Result, 0);
  if (Source <> nil) then
  begin
    if (FCurrentParser.ResultType = etString) and (Result > FCurrentParser.ResultBufferSize) then
      Result := FCurrentParser.ResultBufferSize;
    Move(Source^, Dest^, Result);
  end
  else
    Result := 0;
  if (PIndexHdr(IndexHeader).KeyType = 'C') and (Result <> 0) then
    ExprTrailingNulsToSpace(Dest, Result);
end;

function TIndexFile.InsertKey(Buffer: {$IFDEF SUPPORT_TRECORDBUFFER}PByte{$ELSE}PAnsiChar{$ENDIF}; RecNo: Integer): boolean;
begin
  Result := true;
  // check record is indexed
  if RecordIsIndexed(Buffer) then
  begin
    // check proper index and modifiability
    if FCanEdit and (PIndexHdr(FIndexHeader)^.KeyLen <> 0) then
    begin
      // get key from buffer
      FUserKey := ExtractKeyFromBuffer(Buffer, RecNo);
      // patch through
      Result := InsertCurrent;
    end;
  end;
end;

function TIndexFile.InsertCurrent: boolean;
  // insert in current index
  // assumes: FUserKey is an OEM key
begin
  Result := true;
  // temporarily remove range to find correct location of key
  ResetRange;
  // find this record as closely as possible
  // if result = 0 then key already exists
  // if unique index, then don't insert key if already present
  if (FindKey(true) <> 0) or (FUniqueMode = iuNormal) then
  begin
    // if we found eof, write to pagebuffer
    Leaf.GotoInsertEntry;
    // insert requested entry, we know there is an entry available
    Leaf.LocalInsert(FUserRecNo, FUserKey, 0);
  end else begin
    // key already exists -> test possible key violation
    if FUniqueMode = iuDistinct then
    begin
      ConstructInsertErrorMsg;
      Result := false;
    end;
  end;
end;

procedure TIndexFile.ConstructInsertErrorMsg;
var
  Value: AnsiString;
begin
  if Length(FInsertError) > 0 then exit;
  if dbfParseIndexKeyAsString(FUserKey, KeyType, KeyLen,
    PIndexHdr(IndexHeader)^.sKeyType, TDbfFile(FDbfFile).DbfVersion = xFoxPro,
    Value) then
  begin
    if KeyType = 'C' then
      Value := '''' + Value + '''';
  end
  else
    Value := '';
  FInsertError := Format(STRING_KEY_VIOLATION, [GetName, FUserRecNo, Value]);
end;

procedure TIndexFile.InsertError;
var
  errorStr: string;
begin
  errorStr := FInsertError;
  if errorstr <> '' then
  begin
    FInsertError := '';
    raise EDbfIndexError.Create(errorStr);
  end;
end;

procedure TIndexFile.Delete(RecNo: Integer; Buffer: TdbfRecordBuffer);
var
  I, curSel: Integer;
begin
  // check if updating all or only current
  FUserRecNo := RecNo;
  if (FUpdateMode = umAll) or (FSelectedIndex = -1) then
  begin
    // remember currently selected index
    curSel := FSelectedIndex;
    for I := 0 to SwapWordLE(PMdxHdr(Header)^.TagsUsed) - 1 do
    begin
      SelectIndexVars(I);
      LockIndex;
      try
        DeleteKey(Buffer, RecNo);
        // range may be changed
        ResyncRange(true);
      finally
        UnlockIndex;
      end;
    end;
    // restore previous selected index
    SelectIndexVars(curSel);
  end else begin
    LockIndex;
    try
      DeleteKey(Buffer, RecNo);
      // range may be changed
      ResyncRange(true);
    finally
      UnlockIndex;
    end;
  end;
end;

procedure TIndexFile.DeleteKey(Buffer: TdbfRecordBuffer; RecNo: Integer);
begin
  if FCanEdit and (PIndexHdr(FIndexHeader)^.KeyLen <> 0) then
  begin
    // get key from record buffer
    FUserKey := ExtractKeyFromBuffer(Buffer, RecNo);
    // call function
    DeleteCurrent;
  end;
end;

function TIndexFile.DeleteCurrent: Boolean;
  // deletes from current index
begin
  Result := True;
  // prevent "confined" view of index while deleting
  ResetRange;
  // search correct entry to delete
  if Leaf.PhysicalRecNo <> FUserRecNo then
  begin
    Result := FindKey(false) = 0;
    if not Result then
      Result := Leaf.PhysicalRecNo = FUserRecNo;
  end;
  // delete selected entry
  if Result then
    Leaf.Delete
  else
    CheckInvalidError;
end;

function TIndexFile.UpdateIndex(Index: Integer; PrevBuffer, NewBuffer: TdbfRecordBuffer; RecNo: Integer): Boolean;
begin
  SelectIndexVars(Index);
  Result := UpdateCurrent(PrevBuffer, NewBuffer, RecNo);
end;

function TIndexFile.RecordIsIndexed(Buffer: TDbfRecordBuffer): Boolean;
begin
  if FUniqueMode = iuDistinct then
    // Distinct index excludes deleted records
    Result := PAnsiChar(Buffer)^ <> '*'
  else
    Result := True;
end;

function TIndexFile.Update(RecNo: Integer; PrevBuffer, NewBuffer: TdbfRecordBuffer): Boolean;
var
  I, curSel, count: Integer;
begin
  // check if updating all or only current
  FUserRecNo := RecNo;
  if (FUpdateMode = umAll) or (FSelectedIndex = -1) then
  begin
    // remember currently selected index
    curSel := FSelectedIndex;
    Result := true;
    I := 0;
    count := SwapWordLE(PMdxHdr(Header)^.TagsUsed);
    while I < count do
    begin
      Result := UpdateIndex(I, PrevBuffer, NewBuffer, RecNo);
      if not Result then
      begin
        // rollback updates to previous indexes
        while I > 0 do
        begin
          Dec(I);
          UpdateIndex(I, NewBuffer, PrevBuffer, RecNo);
        end;
        break;
      end;
      Inc(I);
    end;
    // restore previous selected index
    SelectIndexVars(curSel);
  end else begin
    Result := UpdateCurrent(PrevBuffer, NewBuffer, RecNo);
  end;
  // check range, disabled by delete/insert
  if (FRoot.LowPage = 0) and (FRoot.HighPage = 0) then
    ResyncRange(true);
end;

function TIndexFile.UpdateCurrent(PrevBuffer, NewBuffer: TdbfRecordBuffer; RecNo: Integer): boolean;
var
  NewKey: PAnsiChar;
  PrevKey: PAnsiChar;
  TempBuffer: array [0..MaxIndexKeyLen] of AnsiChar;
begin
  Result := true;
  // check record is indexed
  if RecordIsIndexed(PrevBuffer) or RecordIsIndexed(NewBuffer) then
  begin
    if FCanEdit and (PIndexHdr(FIndexHeader)^.KeyLen <> 0) then
    begin
      PrevKey := ExtractKeyFromBuffer(PrevBuffer, RecNo);
      FillChar(TempBuffer{%H-}, SizeOf(TempBuffer), 0);
      Move(PrevKey^, TempBuffer, SwapWordLE(PIndexHdr(FIndexHeader)^.KeyLen));
      PrevKey := @TempBuffer[0];
      NewKey := ExtractKeyFromBuffer(NewBuffer, RecNo);

      // compare to see if anything changed
      if (CompareKeys(PrevKey, NewKey) <> 0) or
        (RecordIsIndexed(PrevBuffer) <> RecordIsIndexed(NewBuffer)) then
      begin
        LockIndex;
        try
          if RecordIsIndexed(PrevBuffer) then
          begin
            FUserKey := PrevKey;
            Result := DeleteCurrent;
          end
          else
            Result := True;
          if Result and RecordIsIndexed(NewBuffer) then
          begin
            FUserKey := NewKey;
            Result := InsertCurrent;
            if (not Result) and RecordIsIndexed(PrevBuffer) then
            begin
              FUserKey := PrevKey;
              InsertCurrent;
              FUserKey := NewKey;
            end;
          end;
        finally
          UnlockIndex;
        end;
      end;
    end;
  end;
end;

procedure TIndexFile.AddNewLevel;
var
  lNewPage: TIndexPage;
  pKeyData: PAnsiChar;
begin
  // create new page + space
  if FIndexVersion >= xBaseIV then
    lNewPage := TMdxPage.Create(Self)
  else
    lNewPage := TNdxPage.Create(Self);
  lNewPage.GetNewPage;

  // modify header
  PIndexHdr(FIndexHeader)^.RootPage := SwapIntLE(lNewPage.PageNo);

  // set new page properties
  lNewPage.SetNumEntries(0);
  lNewPage.EntryNo := 0;
  lNewPage.GotoInsertEntry;
  lNewPage.SetPrevBlock(lNewPage.PageNo - PagesPerRecord);
  pKeyData := FRoot.GetKeyDataFromEntry(0);
  lNewPage.FLowerPage := FRoot;
  lNewPage.FHighIndex := 0;
  lNewPage.SetEntry(0, pKeyData, FRoot.PageNo);

  // update root pointer
  FRoot.UpperPage := lNewPage;
  FRoots[FSelectedIndex] := lNewPage;
  FRoot := lNewPage;

  // write new header
  WriteRecord(FHeaderPageNo, FIndexHeader);
end;

procedure TIndexFile.ResyncRoot;
begin
  // read header
  inherited ReadHeader;
  if FIndexVersion >= xBaseIV then
  begin
    // read header page
    inherited ReadRecord(FHeaderPageNo, FIndexHeader);
  end;
  // reread tree
  FRoot.PageNo := SwapIntLE(PIndexHdr(FIndexHeader)^.RootPage);
end;

function TIndexFile.SearchKey(Key: PAnsiChar; SearchType: TSearchKeyType): Boolean;
begin
  // resync tree
  ResyncIndex(False);

  Result := SearchKeyInternal(Key, SearchType);
end;

function TIndexFile.Find(RecNo: Integer; Buffer: PAnsiChar): Integer;
begin
  // execute find
  FUserRecNo := RecNo;
  FUserKey := Buffer;
  Result := FindKey(false);
end;

function TIndexFile.FindKey(AInsert: boolean): Integer;
//
// if you set Insert = true, you need to re-enable range after insert!!
//
var
  TempPage, NextPage: TIndexPage;
  numEntries, numKeysAvail, done, searchRecNo: Integer;
begin
  // reread index header (to discover whether root page changed)
  // if distinct or unique index -> every entry only occurs once ->
  // does not matter which recno we search -> search recno = -2 ->
  // extra info = recno
  if (FUniqueMode = iuNormal) then
  begin
    // if inserting, search last entry matching key
    if AInsert then
      searchRecNo := -3
    else
      searchRecNo := FUserRecNo
  end else begin
    searchRecNo := -2;
  end;
  // start from root
  TempPage := FRoot;
  repeat
    // find key
    done := 0;
    Result := TempPage.FindNearest(searchRecNo);
    if TempPage.LowerPage = nil then
    begin
      // if key greater than last, try next leaf
      if (Result > 0) and (searchRecNo > 0) then
      begin
        // find first parent in tree so we can advance to next item
        NextPage := TempPage;
        repeat
          NextPage := NextPage.UpperPage;
        until (NextPage = nil) or (NextPage.EntryNo < NextPage.HighIndex);
        // found page?
        if NextPage <> nil then
        begin
          // go to parent
          TempPage := NextPage;
          TempPage.EntryNo := TempPage.EntryNo + 1;
          // resync rest of tree
          TempPage.LowerPage.RecurFirst;
          // go to lower page to continue search
          TempPage := TempPage.LowerPage;
          // check if still more lowerpages
          if TempPage.LowerPage <> nil then
          begin
            // flag we need to traverse down further
            done := 2;
          end else begin
            // this is next child, we don't know if found
            done := 1;
          end;
        end;
      end;
    end else begin
      // need to traverse lower down
      done := 2;
    end;

    // check if we need to split page
    // done = 1 -> not found entry on insert path yet
    if AInsert and (done <> 1) then
    begin
      // now we are on our path to destination where entry is to be inserted
      // check if this page is full, then split it
      numEntries := TempPage.NumEntries;
      // if this is inner node, we can only store one less than max entries
      numKeysAvail := SwapWordLE(PIndexHdr(FIndexHeader)^.NumKeys) - numEntries;
      if TempPage.LowerPage <> nil then
        dec(numKeysAvail);
      // too few available -> split
      if numKeysAvail = 0 then
        TempPage.Split;
    end;

    // do we need to go lower down?
    if done = 2 then
      TempPage := TempPage.LowerPage;
  until done = 0;
end;

function TIndexFile.MatchKey(UserKey: PAnsiChar): Integer;
begin
  // BOF and EOF always false
  if Leaf.Entry = FEntryBof then
    Result := 1
  else
  if Leaf.Entry = FEntryEof then
    Result := -1
  else begin
    FUserKey := UserKey;
    Result := Leaf.MatchKey;
  end;
end;

procedure TIndexFile.SetRange(LowRange, HighRange: PAnsiChar);
begin
  Move(LowRange^, FLowBuffer[0], KeyLen);
  Move(HighRange^, FHighBuffer[0], KeyLen);
  FRangeActive := true;
  ResyncRange(true);
end;

procedure TIndexFile.SetPhysicalRecNo(RecNo: Integer);
begin
  // check if already at specified recno
  if Leaf.PhysicalRecNo = RecNo then
    exit;

  // check record actually exists
  if TDbfFile(FDbfFile).IsRecordPresent(RecNo) then
  begin
    // read buffer of this RecNo
    TDbfFile(FDbfFile).ReadRecord(RecNo, TDbfFile(FDbfFile).PrevBuffer);
    // extract key
    FUserKey := ExtractKeyFromBuffer(TDbfFile(FDbfFile).PrevBuffer, RecNo);
    // find this key
    FUserRecNo := RecNo;
    FindKey(false);
    if (FUniqueMode = iuNormal) and (Leaf.PhysicalRecNo <> RecNo) then
      CheckInvalidError;
  end;
end;

procedure TIndexFile.SetUpdateMode(NewMode: TIndexUpdateMode);
begin
  // if there is only one index, don't waste time and just set single
  if (FIndexVersion = xBaseIII) or (SwapWordLE(PMdxHdr(Header)^.TagsUsed) <= 1) then
    FUpdateMode := umCurrent
  else
    FUpdateMode := NewMode;
end;

procedure TIndexFile.WalkFirst;
begin
  // search first node
  FRoot.RecurFirst;
  // out of index - BOF
  Leaf.EntryNo := Leaf.EntryNo - 1;
end;

procedure TIndexFile.WalkLast;
begin
  // search last node
  FRoot.RecurLast;
  // out of index - EOF
  Leaf.EntryNo := Leaf.EntryNo + 1;
end;

procedure TIndexFile.First;
begin
  // resync tree
  ResyncIndex(False);
  WalkFirst;
end;

procedure TIndexFile.Last;
begin
  // resync tree
  ResyncIndex(False);
  WalkLast;
end;

procedure TIndexFile.ResyncRange(KeepPosition: boolean);
var
  Result: Boolean;
  currRecNo: TSequentialRecNo;
begin
  if not FRangeActive then
    exit;

  // disable current range if any
  //  init to 0 to suppress delphi warning
  currRecNo := 0;
  if KeepPosition then
    currRecNo := SequentialRecNo;
  ResetRange;
  // search lower bound
  Result := SearchKeyInternal(FLowBuffer, stGreaterEqual);
  if not Result then
  begin
    // not found? -> make empty range
    WalkLast;
  end;
  // set lower bound
  SetBracketLow;
  // search upper bound
  Result := SearchKeyInternal(FHighBuffer, stGreater);
  // if result true, then need to get previous item <=>
  //    last of equal/lower than key
  if Result then
  begin
    Result := WalkPrev;
    if not Result then
    begin
      // cannot go prev -> empty range
      WalkFirst;
    end;
  end else begin
    // not found -> EOF found, go EOF, then to last record
    WalkLast;
    WalkPrev;
  end;
  // set upper bound
  SetBracketHigh;
  if KeepPosition then
    SequentialRecNo := currRecNo;
end;

procedure TIndexFile.Resync(Relative: boolean);
var
  PVersion: PByte;
  Version: Byte;
begin
  if NeedLocks then
  begin
    PVersion := @PIndexHdr(FIndexHeader)^.Version;
    Version := PVersion^;
    if ReadVersion(PVersion) and (PVersion^ <> Version) then
    begin
      if not Relative then
      begin
        ResyncRoot;
        ResyncRange(false);
      end else begin
        // resyncing tree implies resyncing range
        ResyncTree;
      end;
    end;
  end;
end;

procedure TIndexFile.ResyncTree;
var
  action, recno: integer;
begin
  // if at BOF or EOF, then we need to resync by first or last
  // remember where the cursor was
  //  init to 0 to suppress delphi warning
  recno := 0;
  if Leaf.Entry = FEntryBof then
  begin
    action := 0;
  end else if Leaf.Entry = FEntryEof then begin
    action := 1;
  end else begin
    // read current key into buffer
    Move(Leaf.Key^, FKeyBuffer, SwapWordLE(PIndexHdr(FIndexHeader)^.KeyLen));
    recno := Leaf.PhysicalRecNo;
    action := 2;
  end;

  // we now know cursor position, resync possible range
  ResyncRange(false);
  
  // go to cursor position
  case action of
    0: WalkFirst;
    1: WalkLast;
    2:
    begin
      // search current in-mem key on disk
      if (Find(recno, FKeyBuffer) <> 0) then
      begin
        // houston, we've got a problem!
        // our `current' record has gone. we need to find it
        // find it by using physical recno
        PhysicalRecNo := recno;
      end;
    end;
  end;
end;

function TIndexFile.WalkPrev: boolean;
var
  curRecNo: Integer;
begin
  // save current recno, find different next!
  curRecNo := Leaf.PhysicalRecNo;
  // return false if we are at first entry
  Result := Leaf.RecurPrev;
  if Result and ((Leaf.PhysicalRecNo = curRecNo) or (not TDbfFile(FDbfFile).IsRecordPresent(Leaf.PhysicalRecNo))) then
    CheckInvalidError;
end;

function TIndexFile.WalkNext: boolean;
var
  curRecNo: Integer;
begin
  // save current recno, find different prev!
  curRecNo := Leaf.PhysicalRecNo;
  // return false if we are at last entry
  Result := Leaf.RecurNext;
  if Result and ((Leaf.PhysicalRecNo = curRecNo) or (not TDbfFile(FDbfFile).IsRecordPresent(Leaf.PhysicalRecNo))) then
    CheckInvalidError;
end;

function TIndexFile.Prev: Boolean;
begin
  // resync tree
  ResyncIndex(True);
  Result := WalkPrev;
end;

function TIndexFile.Next: Boolean;
begin
  // resync tree
  ResyncIndex(True);
  Result := WalkNext;
end;

function TIndexFile.GetKeyLen: Integer;
begin
  Result := SwapWordLE(PIndexHdr(FIndexHeader)^.KeyLen);
end;

function TIndexFile.GetKeyType: AnsiChar;
begin
  Result := PIndexHdr(FIndexHeader)^.KeyType;
end;

function TIndexFile.GetPhysicalRecNo: Integer;
begin
  Result := Leaf.PhysicalRecNo;
end;

function TIndexFile.GetSequentialRecordCount: TSequentialRecNo;
begin
  Result := FRoot.Weight * (TSequentialRecNo(FRoot.HighIndex) + 1);
end;

function TIndexFile.GetSequentialRecNo: TSequentialRecNo;
var
  TempPage: TIndexPage;
begin
  // check if at BOF or EOF, special values
  if Leaf.EntryNo < Leaf.LowIndex then begin
    Result := RecBOF;
  end else if Leaf.EntryNo > Leaf.HighIndex then begin
    Result := RecEOF;
  end else begin
    // first record is record 1
    Result := 1;
    TempPage := FRoot;
    repeat
      inc(Result, TempPage.EntryNo * TempPage.Weight);
      TempPage := TempPage.LowerPage;
    until TempPage = nil;
  end;
end;

procedure TIndexFile.SetSequentialRecNo(RecNo: TSequentialRecNo);
var
  TempPage: TIndexPage;
  gotoEntry: Integer;
begin
  if RecNo = RecBOF then
    WalkFirst
  else if RecNo = RecEOF then
    WalkLast
  else
  begin
    // use our weighting system to quickly go to a seq recno
    // recno starts at 1, entries at zero
    Dec(RecNo);
    TempPage := FRoot;
    repeat
      // don't div by zero
      assert(TempPage.Weight > 0);
      gotoEntry := RecNo div TempPage.Weight;
      RecNo := RecNo mod TempPage.Weight;
      // do we have this much entries?
      if (TempPage.HighIndex < gotoEntry) then
      begin
        // goto next entry in upper page if not
        // if recurnext fails, we have come at the end of the index
        if (TempPage.UpperPage <> nil) and TempPage.UpperPage.RecurNext then
        begin
          // lower recno to get because we skipped an entry
          TempPage.EntryNo := TempPage.LowIndex;
          RecNo := 0;
        end else begin
          // this can only happen if too big RecNo was entered, go to last
          TempPage.RecurLast;
          // terminate immediately
          TempPage := Leaf;
        end;
      end else begin
        TempPage.EntryNo := gotoEntry;
      end;
      // get lower node
      TempPage := TempPage.LowerPage;
    until TempPage = nil;
  end;
end;

procedure TIndexFile.SetBracketLow;
var
  TempPage: TIndexPage;
begin
  // set current record as lower bound
  TempPage := FRoot;
  repeat
    TempPage.LowBracket := TempPage.EntryNo;
    TempPage.LowPage := TempPage.PageNo;
    TempPage := TempPage.LowerPage;
  until TempPage = nil;
end;

procedure TIndexFile.SetBracketHigh;
var
  TempPage: TIndexPage;
begin
  // set current record as lower bound
  TempPage := FRoot;
  repeat
    TempPage.HighBracket := TempPage.EntryNo;
    TempPage.HighPage := TempPage.PageNo;
    TempPage := TempPage.LowerPage;
  until TempPage = nil;
end;

procedure TIndexFile.MergeSort(List: PDbfPointerList; L, R: Integer);
var
  TempList: PDbfPointerList;
  Size: Integer;
begin
  if L < R then
  begin
    Size:= Succ(R-L) * SizeOf(Pointer);
    GetMem(TempList, Size);
    try
      MergeSort2(List, TempList, L, R);
      Move(TempList^, List^, Size);
    finally
      FreeMem(TempList);
    end;
  end;
end;

procedure TIndexFile.MergeSort2(List, TempList: PDbfPointerList; L, R: Integer);
var
  C: Integer;
  M: Integer;
  L1: Integer;
  R0: Integer;
begin
  if L < R then
  begin
    C:= Succ(R - L);
    M:= L + Pred(C div 2);
    L1:= M;
    R0:= Succ(M);
    MergeSort2(List, TempList, L, L1);
    MergeSort2(List, TempList, R0, R);
    MergeSort3(List, TempList, L, L1, R0, R);
    Move(TempList^[L], List^[L], C * SizeOf(Pointer));
  end;
end;

procedure TIndexFile.MergeSort3(List, TempList: PDbfPointerList; L0, L1, R0, R1: Integer);
var
  I: Integer;

  procedure MergeAppend(var J: Integer);
  begin
    DoProgress(FProgressPosition, FProgressMax, STRING_PROGRESS_SORTING_RECORDS);
    TempList^[I] := List^[J];
    Inc(I);
    Inc(J);
  end;

begin
  I := L0;
  while (L0 <= L1) and (R0 <= R1) do
  begin
    if MergeSortCompare(List^[L0], List^[R0]) <= 0 then
      MergeAppend(L0)
    else
      MergeAppend(R0);
  end;
  while L0 <= L1 do
    MergeAppend(L0);
  while R0 <= R1 do
    MergeAppend(R0);
end;

function TIndexFile.MergeSortCompare(Item1, Item2: Pointer): Integer;
var
  KeyData1: PAnsiChar;
  KeyData2: PAnsiChar;
begin
  KeyData1 := @PMdxEntry(Item1).KeyData;
  KeyData2 := @PMdxEntry(Item2).KeyData;
  Result:= CompareKeys(KeyData1, KeyData2);
  Inc(FProgressPosition);
end;

procedure TIndexFile.CancelRange;
begin
  FRangeActive := false;
  ResetRange;
end;

procedure TIndexFile.ResetRange;
var
  TempPage: TIndexPage;
begin
  // disable lower + upper bound
  TempPage := FRoot;
  repeat
    // set a page the index should never reach
    TempPage.LowPage := 0;
    TempPage.HighPage := 0;
    TempPage := TempPage.LowerPage;
  until TempPage = nil;
end;

procedure TIndexFile.DisableRange;
var
  TempPage: TIndexPage;
begin
  TempPage := FRoot;
  repeat
    TempPage.SaveBracket;
    TempPage := TempPage.LowerPage;
  until TempPage = nil;
  CancelRange;
end;

procedure TIndexFile.EnableRange;
var
  TempPage: TIndexPage;
begin
  TempPage := FRoot;
  repeat
    TempPage.RestoreBracket;
    TempPage := TempPage.LowerPage;
  until TempPage = nil;
  FRangeActive := true;
end;

function MemComp(P1, P2: Pointer; const Length: Integer): Integer;
var
  I: Integer;
begin
  for I := 0 to Length - 1 do
  begin
    // still equal?
    if PByte(P1)^ <> PByte(P2)^ then
    begin
      Result := Integer(PByte(P1)^) - Integer(PByte(P2)^);
      exit;
    end;
    // go to next byte
    Inc(PByte(P1)); // Was PChar, increments 2 bytes for Delphi 2009 and up
    Inc(PByte(P2)); // Was PChar
  end;

  // memory equal
  Result := 0;
end;

function TIndexFile.CompareKeys(Key1, Key2: PAnsiChar): Integer;
begin
  // call compare routine
  Result := FCompareKeys(Key1, Key2);

  // if descending then reverse order
  if FIsDescending then
    Result := -Result;
end;

function TIndexFile.CompareKeysDate(Key1, Key2: PAnsiChar): Integer;
var
  Compare: TDateTime;
begin
  Compare := PDateTime(Key1)^ - PDateTime(Key2)^;

  // Handle NULL values
  if PDouble(Key1)^ = DateNullValue then
  begin
    if PDouble(Key2)^ <> DateNullValue then
      Compare := -1;
  end
  else
  begin
    if PDouble(Key2)^ = DateNullValue then
      Compare := 1;
  end;

  if Compare > 0 then Result := 1 else
  if Compare < 0 then Result := -1
  else Result := 0;
end;

function TIndexFile.CompareKeysLevel7(Key1, Key2: PAnsiChar): Integer;
begin
  Result := MemComp(Key1, Key2, KeyLen);
end;

function TIndexFile.CompareKeysNumericNDX(Key1, Key2: PAnsiChar): Integer;
var
  v1,v2: Double;
begin
  v1 := PDouble(Key1)^;
  v2 := PDouble(Key2)^;
  if v1 > v2 then Result := 1
  else if v1 < v2 then Result := -1
  else Result := 0;
end;

function TIndexFile.CompareKeysNumericMDX(Key1, Key2: PAnsiChar): Integer;
var
  neg1, neg2: Boolean;
begin
  // first byte - $34 contains dot position
  neg1 := (Byte(Key1[1]) and $80) <> 0;
  neg2 := (Byte(Key2[1]) and $80) <> 0;
  // check if both negative or both positive
  if neg1 = neg2 then
  begin
    // check alignment
    if Key1[0] = Key2[0] then
    begin
      // no alignment needed -> have same alignment
      Result := MemComp(Key1+2, Key2+2, 10-2);
    end else begin
      // greater 10-power implies bigger number except for zero
      if (Byte(Key1[1]) = $01) and (Byte(Key1[0]) = $34) then
        Result := -1
      else
      if (Byte(Key2[1]) = $01) and (Byte(Key2[0]) = $34) then
        Result := 1
      else
        Result := Byte(Key1[0]) - Byte(Key2[0]);
    end;
    // negate result if both negative
    if neg1 and neg2 then
      Result := -Result;
  end else if neg1 {-> not neg2} then
    Result := -1
  else { not neg1 and neg2 }
    Result := 1;
end;

function TIndexFile.CompareKeysString(Key1, Key2: PAnsiChar): Integer;
begin
  Result := DbfCompareString(FCollation, Key1, KeyLen, Key2, KeyLen);
  if Result > 0 then
    Dec(Result, 2);
end;

function TIndexFile.CompareKey(Key: PAnsiChar): Integer;
begin
  Result := CompareKeys(FUserKey, Key);
end;

function TIndexFile.IndexOf(const AIndexName: string): Integer;
  // *) assumes FIndexVersion >= xBaseIV
var
  I: Integer;
begin
  // get index of this index :-)
  Result := -1;
  for I := 0 to SwapWordLE(PMdxHdr(Header)^.TagsUsed) - 1 do
  begin
    FTempMdxTag.Tag := CalcTagOffset(I);
    if AnsiCompareText(AIndexName, FTempMdxTag.TagName) = 0 then
    begin
      Result := I;
      break;
    end;
  end;
end;

procedure TIndexFile.SetIndexName(const AIndexName: string);
var
  found: Integer;
begin
  // we can only select a different index if we are MDX
  if FIndexVersion >= xBaseIV then
  begin
    // find index
    found := IndexOf(AIndexName);
  end else
    found := 0;
  // if changing index, range is N/A anymore
  if FRangeActive and (found <> FSelectedIndex) then
  begin
    FRangeIndex := FSelectedIndex;
    DisableRange;
  end;
  // we can now select by index
  if found >= 0 then
  begin
    SelectIndexVars(found);
    if found = FRangeIndex then
    begin
      EnableRange;
      FRangeIndex := -1;
    end;
  end;
end;

procedure TIndexFile.LockIndex;
begin
  if NeedLocks then
  begin
    LockIndexPage;
    try
      Resync(False);
    except
      UnlockIndexPage;
      raise;
    end;
  end;
end;

procedure TIndexFile.LockIndexPage;
begin
  if not LockPage(0, True) then
    raise EPagedFile.Create(STRING_WRITE_ERROR);
end;

procedure TIndexFile.ResyncIndex(Relative: Boolean);
begin
  if NeedLocks then
  begin
    LockIndexPage;
    try
      Resync(Relative);
    finally
      UnlockIndexPage;
    end;
  end;
end;

procedure TIndexFile.UnlockIndex;
var
  PVersion: PByte;
begin
  if NeedLocks then
  begin
    try
      Flush;
      PVersion := @PIndexHdr(FIndexHeader).Version;
      if PVersion^ = High(PVersion^) then
        PVersion^ := 0
      else
        Inc(PVersion^);
      WriteVersion(PVersion);
    finally
      UnlockIndexPage;
    end;
  end;
end;

procedure TIndexFile.UnlockIndexPage;
begin
  UnlockPage(0);
end;

function TIndexFile.SearchKeyInternal(Key: PAnsiChar;
  SearchType: TSearchKeyType): Boolean;
var
  findres: Integer;
  currRecNo: TSequentialRecNo;
begin
  // save current position
  currRecNo := SequentialRecNo;
  // search, these are always from the root: no need for first
  findres := Find(-2, Key);
  // test result
  case SearchType of
    stEqual:
      Result := findres = 0;
    stGreaterEqual:
      Result := findres <= 0;
    stGreater:
      begin
        if findres = 0 then
        begin
          // find next record that is greater
          // NOTE: MatchKey assumes key to search for is already specified
          //   in FUserKey, it is because we have called Find
          repeat
            Result := WalkNext;
          until not Result or (MatchKey(Key) <> 0);
        end else
          Result := findres < 0;
      end;
    else
      Result := false;
  end;
  // search failed -> restore previous position
  if not Result then
  begin
    SequentialRecNo := currRecNo;
    if SearchType <> stEqual then
      FRoot.RecurLast;
  end;
end;

function TIndexFile.VersionPosition: TPagedFileOffset;
var
  PageNo: Integer;
begin
  if FIndexVersion >= xBaseIV then
    PageNo := FMdxTag.HeaderPageNo
  else
    PageNo := 0;
  Result := TPagedFileOffset(PageNo) * PageSize + {%H-}TPagedFileOffset(@PIndexHdr(nil)^.Version);
end;

function TIndexFile.ReadVersion(PVersion: PByte): Boolean;
begin
  Result := ReadBlock(PVersion, SizeOf(PVersion^), VersionPosition) = SizeOf(PVersion^);
end;

procedure TIndexFile.WriteVersion(PVersion: PByte);
begin
  WriteBlock(PVersion, SizeOf(PVersion^), VersionPosition);
end;

procedure TIndexFile.CheckInvalidError;
begin
  {$IFDEF TDBF_INDEX_CHECK}
  InvalidError;
  {$ENDIF}
end;

procedure TIndexFile.InvalidError;
begin
  FForceClose := True;
  raise EDbfIndexError.Create(STRING_INVALID_INDEX_FILE);
end;

function TIndexFile.GetLeaf: TIndexPage;
begin
  Result := FRoot;
  if Assigned(Result) then
    while Result.LowerPage <> nil do
      Result := Result.LowerPage;
end;

function TIndexFile.CalcTagOffset(AIndex: Integer): Pointer;
begin
  Result := PAnsiChar(Header) + FTagOffset + AIndex * FTagSize;
end;

procedure TIndexFile.LinkTags;
var
  AIndex : Integer;
  numTags : Integer;
  ATag : Pointer;
begin
  if FIndexVersion >= xBaseIV then
  begin
    ATag := FTempMdxTag.Tag;
    try
      numTags := SwapWordLE(PMdxHdr(Header)^.TagsUsed);
      for AIndex := 0 to Pred(numTags) do
      begin
        FTempMdxTag.Tag := CalcTagOffset(AIndex);
        FTempMdxTag.ForwardTag1 := 0;
        if AIndex = Pred(SwapWordLE(PMdxHdr(Header)^.TagsUsed)) then
          FTempMdxTag.ForwardTag2 := 0
        else
          FTempMdxTag.ForwardTag2 := Succ(Succ(AIndex));
        FTempMdxTag.BackwardTag := AIndex;
      end;
    finally
      FTempMdxTag.Tag := ATag;
    end;
  end;
end;

procedure TIndexFile.SelectIndexVars(AIndex: Integer);
  // *) assumes index is in range
begin
  if AIndex >= 0 then
  begin
    // get pointer to index header
    FIndexHeader := FIndexHeaders[AIndex];
    if Assigned(FIndexHeader) then
    begin
      // load root
      FCurrentParser := FParsers[AIndex];
      FRoot := FRoots[AIndex];
      // if xBaseIV then we need to store where pageno of current header
      if FIndexVersion >= xBaseIV then
      begin
        if AIndex < SwapWordLE(PMdxHdr(Header)^.TagsUsed) then
        begin
          FMdxTag.Tag := CalcTagOffset(AIndex);
          FIndexName := FMdxTag.TagName;
          FHeaderPageNo := FMdxTag.HeaderPageNo;
        end;
        // does dBase actually use this flag?
      end else begin
        // how does dBase III store whether it is expression?
      end;
      // retrieve properties
      UpdateIndexProperties;
    end;
  end else begin
    // not a valid index
    FIndexName := EmptyStr;
  end;
  // store selected index
  FSelectedIndex := AIndex;
  FCanEdit := not FForceReadOnly;
end;

procedure TIndexFile.UpdateIndexProperties;
begin
  // get properties
  FIsDescending := (PIndexHdr(FIndexHeader)^.KeyFormat and KeyFormat_Descending) <> 0;
  FUniqueMode := iuNormal;
  if (PIndexHdr(FIndexHeader)^.KeyFormat and KeyFormat_Unique) <> 0 then
    FUniqueMode := iuUnique;
  if (PIndexHdr(FIndexHeader)^.KeyFormat and KeyFormat_Distinct) <> 0 then
    FUniqueMode := iuDistinct;
  // select key compare routine
  if PIndexHdr(FIndexHeader)^.KeyType = 'C' then
    FCompareKeys := CompareKeysString
  else
  if PIndexHdr(FIndexHeader)^.KeyType = 'D' then
    FCompareKeys := CompareKeysDate
  else
  if CharInSet(PIndexHdr(FIndexHeader)^.KeyType, ['@', 'O', 'I', '+']) then
    FCompareKeys := CompareKeysLevel7
  else
  if FIndexVersion >= xBaseIV then
    FCompareKeys := CompareKeysNumericMDX
  else
    FCompareKeys := CompareKeysNumericNDX;
end;

procedure TIndexFile.Flush;
var
  I: Integer;
begin
  // save changes to pages
  if FIndexVersion >= xBaseIV then
  begin
    for I := 0 to MaxIndexes - 1 do
    begin
      if FIndexHeaderModified[I] then
        WriteIndexHeader(I);
      if FRoots[I] <> nil then
        FRoots[I].Flush
    end;
  end else begin
    if FRoot <> nil then
      FRoot.Flush;
  end;

  // save changes to header
  FlushHeader;

  inherited;
end;

procedure TIndexFile.GetIndexNames(const AList: TStrings);
var
  I: Integer;
begin
  // only applicable to MDX files
  if FIndexVersion >= xBaseIV then
  begin
    for I := 0 to SwapWordLE(PMdxHdr(Header)^.TagsUsed) - 1 do
    begin
      FTempMdxTag.Tag := CalcTagOffset(I);
      AList.AddObject(FTempMdxTag.TagName, Self);
    end;
  end;
end;

procedure TIndexFile.GetIndexInfo(const AIndexName: string; IndexDef: TDbfIndexDef);
var
  SaveIndexName: string;
begin
  // remember current index
  SaveIndexName := IndexName;
  // select index
  IndexName := AIndexName;
  // copy properties
  IndexDef.IndexFile := AIndexName;
  IndexDef.Expression := String(PIndexHdr(FIndexHeader)^.KeyDesc);
  IndexDef.Options := [];
  IndexDef.Temporary := true;
  if FIsDescending then
    IndexDef.Options := IndexDef.Options + [ixDescending];
  IndexDef.Options := IndexDef.Options + [ixExpression];
  case FUniqueMode of
    iuUnique: IndexDef.Options := IndexDef.Options + [ixUnique];
    iuDistinct: IndexDef.Options := IndexDef.Options + [ixPrimary];
  end;
  // reselect previous index
  IndexName := SaveIndexName;
end;

function TIndexFile.GetExpression: string;
begin
  if FCurrentParser <> nil then
    Result := FCurrentParser.Expression
  else
    Result := EmptyStr;
end;

function TIndexFile.GetDbfLanguageId: Byte;
begin
  // get language id of parent dbf
  Result := TDbfFile(FDbfFile).FileLangId;
end;

procedure TIndexFile.WriteHeader; {override;}
begin
  // if NDX, then this means file header
  if FIndexVersion >= xBaseIV then
    if NeedLocks then
      WriteIndexHeader(FSelectedIndex)
    else
      FIndexHeaderModified[FSelectedIndex] := true
  else
    WriteFileHeader;
end;

procedure TIndexFile.WriteFileHeader;
begin
  LinkTags;
  inherited WriteHeader;
end;

procedure TIndexFile.WriteIndexHeader(AIndex: Integer);
begin
  FTempMdxTag.Tag := CalcTagOffset(AIndex);
  WriteRecord(FTempMdxTag.HeaderPageNo, FIndexHeaders[AIndex]);
  FIndexHeaderModified[AIndex] := false;
end;

//==========================================================
//============ TDbfIndexDef
//==========================================================

constructor TDbfIndexDef.Create(ACollection: TCollection); {override;}
begin
  inherited Create(ACollection);
  FTemporary := false;
end;

destructor TDbfIndexDef.Destroy; {override;}
begin
  inherited Destroy;
end;

procedure TDbfIndexDef.Assign(Source: TPersistent);
begin
  // we can't do anything with it if not a TDbfIndexDef
  if Source is TDbfIndexDef then
  begin
    FIndexName := TDbfIndexDef(Source).IndexFile;
    FExpression := TDbfIndexDef(Source).Expression;
    FOptions := TDbfIndexDef(Source).Options;
  end else
    inherited;
end;

procedure TDbfIndexDef.SetIndexName(NewName: string);
begin
  FIndexName := IndexNameNormalize(NewName);
end;

procedure TDbfIndexDef.SetExpression(NewField: string);
begin
  FExpression := Trim(NewField);
end;

initialization

{
  Entry_Mdx_BOF.RecBlockNo := RecBOF;
  Entry_Mdx_BOF.KeyData := #0;

  Entry_Mdx_EOF.RecBlockNo := RecEOF;
  Entry_Mdx_EOF.KeyData := #0;

  Entry_Ndx_BOF.LowerPageNo := 0;
  Entry_Ndx_BOF.RecNo := RecBOF;
  Entry_Ndx_BOF.KeyData := #0;

  Entry_Ndx_EOF.LowerPageNo := 0;
  Entry_Ndx_EOF.RecNo := RecEOF;
  Entry_Ndx_EOF.KeyData := #0;
}

  LCIDList := TLCIDList.Create;
  LCIDList.Enumerate;

finalization

  LCIDList.Free;

end.

