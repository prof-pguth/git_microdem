unit dbf;

{ design info in dbf_reg.pas }

{$BOOLEVAL OFF}

interface

{$I dbf_common.inc}

uses
  Classes,
  Db,
  dbf_common,
  dbf_dbffile,
  dbf_parser,
  dbf_prsdef,
  dbf_cursor,
  dbf_fields,
  dbf_pgfile,
  dbf_idxfile;
// If you got a compilation error here or asking for dsgnintf.pas, then just add
// this file in your project:
// dsgnintf.pas in 'C: \Program Files\Borland\Delphi5\Source\Toolsapi\dsgnintf.pas'

{$ifdef SUPPORT_MAXLISTSIZEDEPRECATED}
const
  MaxListSize = MaxInt div 16;
{$endif}

type
//====================================================================
  pBookmarkData = ^TBookmarkData;
  TBookmarkData = record
    PhysicalRecNo: Integer;
  end;

  pDbfRecord = ^TDbfRecordHeader;
  TDbfRecordHeader = record
    BookmarkData: TBookmarkData;
    BookmarkFlag: TBookmarkFlag;
    SequentialRecNo: TSequentialRecNo;
    DeletedFlag: AnsiChar;
  end;
//====================================================================
  TDbf = class;
//====================================================================
  TDbfStorage = (stoMemory,stoFile);
  TDbfOpenMode = (omNormal,omAutoCreate,omTemporary);
  TDbfLanguageAction = (laReadOnly, laForceOEM, laForceANSI, laDefault);
  TDbfTranslationMode = (tmNoneAvailable, tmNoneNeeded, tmSimple, tmAdvanced);
  TDbfFileName = (dfDbf, dfMemo, dfIndex);
  TDbfBatchMode = (bmAppend, bmUpdate, bmAppendUpdate, bmDelete, bmCopy);
  TDbfBatchOption = (boUsePhysicalFieldNo);
  TDbfBatchOptions = set of TDbfBatchOption;
//====================================================================
  TDbfFileNames = set of TDbfFileName;
//====================================================================
  TCompareRecordEvent = procedure(Dbf: TDbf; var Accept: Boolean) of object;
  TTranslateEvent = function(Dbf: TDbf; Src, Dest: PAnsiChar; ToOem: Boolean): Integer of object;
  TLanguageWarningEvent = procedure(Dbf: TDbf; var Action: TDbfLanguageAction) of object;
  TConvertFieldEvent = procedure(Dbf: TDbf; DstField, SrcField: TField) of object;
  TBeforeAutoCreateEvent = procedure(Dbf: TDbf; var DoCreate: Boolean) of object;
//====================================================================
  // TDbfBlobStream keeps a reference count to number of references to
  // this instance. Only if FRefCount reaches zero, then the object will be
  // destructed. AddReference `clones' a reference.
  // This allows the VCL to use Free on the object to `free' that
  // particular reference.

  TDbfBlobStream = class(TMemoryStream)
  private
    FBlobField: TBlobField;
    FMode: TBlobStreamMode;
    FDirty: boolean;            { has possibly modified data, needs to be written }
    FMemoRecNo: Integer;
        { -1 : invalid contents }
        {  0 : clear, no contents }
        { >0 : data from page x }
    FReadSize: Integer;
    FRefCount: Integer;

    function  GetTransliterate: Boolean;
    procedure Translate(ToOem: Boolean);
    procedure SetMode(NewMode: TBlobStreamMode);
  public
    constructor Create(FieldVal: TField);
    destructor Destroy; override;

    function  AddReference: TDbfBlobStream;
    procedure FreeInstance; override;

    procedure Cancel;
    procedure Commit;

    property Dirty: boolean read FDirty;
    property Transliterate: Boolean read GetTransliterate;
    property MemoRecNo: Integer read FMemoRecNo write FMemoRecNo;
    property ReadSize: Integer read FReadSize write FReadSize;
    property Mode: TBlobStreamMode write SetMode;
    property BlobField: TBlobField read FBlobField;
  end;
//====================================================================
  TDbfIndexDefs = class(TCollection)
  public
    FOwner: TDbf;
   private
    function GetItem(N: Integer): TDbfIndexDef;
    procedure SetItem(N: Integer; Value: TDbfIndexDef);
   protected
    function GetOwner: TPersistent; override;
   public
    constructor Create(AOwner: TDbf);

    function  Add: TDbfIndexDef;
    function  GetIndexByName(const Name: string): TDbfIndexDef;
    function  GetIndexByField(const Name: string): TDbfIndexDef;
    procedure Update; {$ifdef SUPPORT_REINTRODUCE} reintroduce; {$endif}

    property Items[N: Integer]: TDbfIndexDef read GetItem write SetItem; default;
  end;
//====================================================================
  TDbfMasterLink = class(TDataLink)
  private
    FDetailDataSet: TDbf;
    FParser: TDbfParser;
    FFieldNames: string;
    FValidExpression: Boolean;
    FKeyTranslation: boolean;
    FOnMasterChange: TNotifyEvent;
    FOnMasterDisable: TNotifyEvent;

    function GetFieldsVal: PAnsiChar;

    procedure SetFieldNames(const Value: string);
  protected
    procedure ActiveChanged; override;
    procedure CheckBrowseMode; override;
    procedure LayoutChanged; override;
    procedure RecordChanged({%H-}Field: TField); override;

  public
    constructor Create(ADataSet: TDbf);
    destructor Destroy; override;

    property FieldNames: string read FFieldNames write SetFieldNames;
    property KeyTranslation: boolean read FKeyTranslation;
    property ValidExpression: Boolean read FValidExpression write FValidExpression;
    property FieldsVal: PAnsiChar read GetFieldsVal;
    property Parser: TDbfParser read FParser;

    property OnMasterChange: TNotifyEvent read FOnMasterChange write FOnMasterChange;
    property OnMasterDisable: TNotifyEvent read FOnMasterDisable write FOnMasterDisable;
  end;
//====================================================================
  PDbfBlobList = ^TDbfBlobList;
  TDbfBlobList = array[0..MaxListSize-1] of TDbfBlobStream;
//====================================================================
  TDbf = class(TDataSet)
  private
    FDbfFile: TDbfFile;
    FCursor: TVirtualCursor;
    FOpenMode: TDbfOpenMode;
    FStorage: TDbfStorage;
    FMasterLink: TDbfMasterLink;
    FParser: TDbfParser;
    FBlobStreams: PDbfBlobList;
    FUserStream: TStream;  // user stream to open
    FTableName: string;    // table path and file name
    FRelativePath: string;
    FAbsolutePath: string;
    FIndexName: string;
    FReadOnly: Boolean;
    FFilterBuffer: TDbfRecordBuffer;
    FTempBuffer: TDbfRecordBuffer;
    FEditingRecNo: Integer;
{$ifdef SUPPORT_VARIANTS}    
    FLocateRecNo: Integer;
{$endif}    
    FLanguageID: Byte;
    FTableLevel: Integer;
    FExclusive: Boolean;
    FShowDeleted: Boolean;
    FPosting: Boolean;
    FDisableResyncOnPost: Boolean;
    FTempExclusive: Boolean;
    FInCopyFrom: Boolean;
    FStoreDefs: Boolean;
    FCopyDateTimeAsString: Boolean;
    FFindRecordFilter: Boolean;
    FIndexFile: TIndexFile;
    FDateTimeHandling: TDateTimeHandling;
    FTranslationMode: TDbfTranslationMode;
    FIndexDefs: TDbfIndexDefs;
    FBeforeAutoCreate: TBeforeAutoCreateEvent;
    FOnTranslate: TTranslateEvent;
    FOnLanguageWarning: TLanguageWarningEvent;
    FOnLocaleError: TDbfLocaleErrorEvent;
    FOnIndexInvalid: TDbfIndexInvalidEvent;
    FOnIndexMissing: TDbfIndexMissingEvent;
    FOnCompareRecord: TNotifyEvent;
    FOnCopyDateTimeAsString: TConvertFieldEvent;
    FOnProgress: TPagedFileProgressEvent;
    FScrolling: Boolean;

    FKeyBufferLen: Integer;
    FKeyBuffer: Pointer;
    function GetKeyBuffer: PAnsiChar;
    function InitKeyBuffer(Buffer: PAnsiChar): PAnsiChar;
    procedure PostKeyBuffer({%H-}Commit: Boolean);

    function GetIndexName: string;
    function GetVersion: string;
    function GetPhysicalRecNo: Integer;
    function GetLanguageStr: string;
    function GetCodePage: Cardinal;
    function GetExactRecordCount: Integer;
    function GetPhysicalRecordCount: Integer;
    function GetKeySize: Integer;
    function GetMasterFields: string;
    function FieldDefsStored: Boolean;

    procedure SetIndexName(AIndexName: string);
    procedure SetDbfIndexDefs(const Value: TDbfIndexDefs);
    procedure SetFilePath(const Value: string);
    procedure SetTableName(const S: string);
    procedure SetVersion(const {%H-}S: string);
    procedure SetLanguageID(NewID: Byte);
    procedure SetDataSource(Value: TDataSource);
    procedure SetMasterFields(const Value: string);
    procedure SetTableLevel(const NewLevel: Integer);
    procedure SetPhysicalRecNo(const NewRecNo: Integer);

    procedure MasterChanged(Sender: TObject);
    procedure MasterDisabled(Sender: TObject);
    procedure DetermineTranslationMode;
    procedure UpdateRange;
    procedure SetShowDeleted(Value: Boolean);
    procedure GetFieldDefsFromDbfFieldDefs;
    procedure InitDbfFile(FileOpenMode: TPagedFileMode);
    function  ParseIndexName(const AIndexName: string): string;
    procedure ParseFilter(const AFilter: string);
    function  GetDbfFieldDefs: TDbfFieldDefs;
    function  ReadCurrentRecord(Buffer: TDbfRecordBuffer; var Acceptable: Boolean): TGetResult;
    function  SearchKeyBuffer(Buffer: PAnsiChar; SearchType: TSearchKeyType): Boolean;
    procedure SetRangeBuffer(LowRange: PAnsiChar; HighRange: PAnsiChar);
    procedure UpdateLock;
    function  ResyncSharedReadCurrentRecord: Boolean;

  protected
    { abstract methods }
    function  AllocRecordBuffer: TDbfRecordBuffer; override; {virtual abstract}
    procedure ClearCalcFields(Buffer: TDbfRecordBuffer); override;
    procedure FreeRecordBuffer(var Buffer: TDbfRecordBuffer); override; {virtual abstract}
    procedure GetBookmarkData(Buffer: TDbfRecordBuffer; Data: Pointer); override; {virtual abstract}
    function  GetBookmarkFlag(Buffer: TDbfRecordBuffer): TBookmarkFlag; override; {virtual abstract}
    function  GetRecord(Buffer: TDbfRecBuf; GetMode: TGetMode; {%H-}DoCheck: Boolean): TGetResult; override; {virtual abstract}
    function  GetRecordSize: Word; override; {virtual abstract}
    procedure InternalAddRecord(Buffer: {$ifdef InternalAddRecord_Wants_TRecordBuffer}TDbfRecordBuffer{$else}Pointer{$endif}; {%H-}Append: Boolean); override; {virtual abstract}
    procedure InternalClose; override; {virtual abstract}
    procedure InternalDelete; override; {virtual abstract}
    procedure InternalFirst; override; {virtual abstract}
    procedure InternalGotoBookmark(ABookmark: Pointer); override; {virtual abstract}
    procedure InternalHandleException; override; {virtual abstract}
    procedure InternalInitFieldDefs; override; {virtual abstract}
    procedure InternalInitRecord(Buffer: TDbfRecordBuffer); override; {virtual abstract}
    procedure InternalLast; override; {virtual abstract}
    function DbfDefaultFields: Boolean;
    procedure InternalOpen; override; {virtual abstract}
    procedure InternalEdit; override; {virtual}
    procedure InternalCancel; override; {virtual}
{$ifndef FPC}
{$ifndef DELPHI_3}
    procedure InternalInsert; override; {virtual}
{$endif}
{$endif}
    procedure InternalPost; override; {virtual}
    procedure InternalRefresh; override;
    procedure InternalSetToRecord(Buffer: TDbfRecordBuffer); override; {virtual abstract}
    procedure InitFieldDefs; override;
    function  IsCursorOpen: Boolean; override; {virtual abstract}
    procedure SetBookmarkFlag(Buffer: TDbfRecordBuffer; Value: TBookmarkFlag); override; {virtual abstract}
    procedure SetBookmarkData(Buffer: TDbfRecordBuffer; Data: Pointer); override; {virtual abstract}
    procedure {%H-}SetFieldData(Field: TField; Buffer: TDbfValueBuffer);
	  {$ifdef SUPPORT_OVERLOAD}overload;{$ENDIF} override; {virtual abstract}

    { virtual methods (mostly optionnal) }
    function  GetDataSource: TDataSource; {$ifndef VER1_0}override;{$endif}
    function  GetRecordCount: Integer; override; {virtual}
    function  GetRecNo: Integer; override; {virtual}
    function  GetCanModify: Boolean; override; {virtual}
    procedure SetRecNo(Value: Integer); override; {virual}
    procedure SetFiltered(Value: Boolean); override; {virtual;}
    procedure SetFilterText(const Value: String); override; {virtual;}
{$ifdef SUPPORT_DEFCHANGED}
    procedure DefChanged(Sender: TObject); override;
{$endif}
    function  FindRecord(Restart, GoForward: Boolean): Boolean; override;
    procedure DoBeforeScroll; override;
    procedure DoAfterScroll; override;

    function  GetIndexFieldNames: string; {virtual;}
    procedure SetIndexFieldNames(const Value: string); {virtual;}

{$ifdef SUPPORT_VARIANTS}
    function  LocateRecordLinear(const KeyFields: String; const KeyValues: Variant; Options: TLocateOptions): Boolean;
    function  LocateRecordIndex(const {%H-}KeyFields: String; const KeyValues: Variant; Options: TLocateOptions): Boolean;
    function  LocateRecord(const KeyFields: String; const KeyValues: Variant; Options: TLocateOptions): Boolean;
{$endif}

    procedure DoFilterRecord(var Acceptable: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetKey;
    function  GotoCommon(SearchKeyType: TSearchKeyType): Boolean;
    procedure GotoNearest;
    function  GotoKey: Boolean;
    procedure Cancel; override;
    procedure Post; override;

    { abstract methods }

    {$ifdef SUPPORT_TVALUEBUFFER_VAR}
    function GetFieldData(Field: TField; var Buffer: TDbfValueBuffer): Boolean;
      {$ifdef SUPPORT_OVERLOAD} overload; {$endif} override; {virtual abstract}
    {$else SUPPORT_TVALUEBUFFER_VAR}
    function GetFieldData(Field: TField; Buffer: TDbfValueBuffer): Boolean;
      {$ifdef SUPPORT_OVERLOAD} overload; {$endif} override; {virtual abstract}
    {$endif SUPPORT_TVALUEBUFFER_VAR}

    { virtual methods (mostly optionnal) }
    procedure Resync(Mode: TResyncMode); override;
    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override; {virtual}
{$ifdef SUPPORT_NEW_TRANSLATE}
    function Translate(Src, Dest: PAnsiChar; ToOem: Boolean): Integer; override; {virtual}
{$else}
    procedure Translate(Src, Dest: PAnsiChar; ToOem: Boolean); override; {virtual}
{$endif}

{$ifdef SUPPORT_OVERLOAD}
    {$ifdef SUPPORT_TVALUEBUFFER_VAR}
    function  GetFieldData(Field: TField; var Buffer: TDbfValueBuffer; NativeFormat: Boolean): Boolean; overload;
      {$ifdef SUPPORT_BACKWARD_FIELDDATA} override; {$endif}
    {$else SUPPORT_TVALUEBUFFER_VAR}
    function  GetFieldData(Field: TField; Buffer: TDbfValueBuffer; NativeFormat: Boolean): Boolean; overload;
      {$ifdef SUPPORT_BACKWARD_FIELDDATA} override; {$endif}
    {$endif SUPPORT_TVALUEBUFFER_VAR}
    procedure SetFieldData(Field: TField; Buffer: TDbfValueBuffer; NativeFormat: Boolean); overload;
      {$ifdef SUPPORT_BACKWARD_FIELDDATA} override; {$endif}
{$endif SUPPORT_OVERLOAD}

    function CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer; override;
    procedure CheckDbfFieldDefs(ADbfFieldDefs: TDbfFieldDefs);

    {$ifdef DELPHI_XE}
    procedure DataEvent(Event: TDataEvent; Info: NativeInt); override;
    {$else}
    procedure DataEvent(Event: TDataEvent; Info: {$ifdef FPC_VERSION}Ptrint{$else}Longint{$endif}); override;
    {$endif}

    // my own methods and properties
    // most look like ttable functions but they are not tdataset related
    // I (try to) use the same syntax to facilitate the conversion between bde and TDbf

    // index support (use same syntax as ttable but is not related)
{$ifdef SUPPORT_DEFAULT_PARAMS}
    procedure AddIndex(const AIndexName, AFields: String; Options: TIndexOptions; const {%H-}DescFields: String='');
{$else}
    procedure AddIndex(const AIndexName, AFields: String; Options: TIndexOptions);
{$endif}
    procedure RegenerateIndexes;

    procedure CancelRange;
    procedure CheckMasterRange;
{$ifdef SUPPORT_VARIANTS}
    function  SearchKey(Key: Variant; SearchType: TSearchKeyType; KeyIsANSI: boolean
      {$ifdef SUPPORT_DEFAULT_PARAMS}= false{$endif}): Boolean;
    procedure SetRange(LowRange: Variant; HighRange: Variant; KeyIsANSI: boolean
      {$ifdef SUPPORT_DEFAULT_PARAMS}= false{$endif});
{$endif}
    function  PrepareKey(Buffer: Pointer; BufferType: TExpressionType): TDbfRecordBuffer;
    function  SearchKeyPChar(Key: PAnsiChar; SearchType: TSearchKeyType; KeyIsANSI: boolean
      {$ifdef SUPPORT_DEFAULT_PARAMS}= false{$endif}): Boolean;
    procedure SetRangePChar(LowRange: PAnsiChar; HighRange: PAnsiChar; KeyIsANSI: boolean
      {$ifdef SUPPORT_DEFAULT_PARAMS}= false{$endif});
    function  GetCurrentBuffer: TDbfRecordBuffer;
    procedure ExtractKey(KeyBuffer: PAnsiChar);
    function  CompareKeys(Key1, Key2: PAnsiChar): Integer;
    procedure UpdateIndexDefs; override;
    procedure GetFileNames(Strings: TStrings; Files: TDbfFileNames); {$ifdef SUPPORT_DEFAULT_PARAMS} overload; {$endif}
{$ifdef SUPPORT_DEFAULT_PARAMS}
    function  GetFileNames(Files: TDbfFileNames  = [dfDbf]  ): string; overload;
{$else}
    function  GetFileNamesString(Files: TDbfFileNames (* = [dfDbf] *) ): string;
{$endif}
    procedure GetIndexNames(Strings: TStrings);
    procedure GetAllIndexFiles(Strings: TStrings);

    procedure TryExclusive;
    procedure EndExclusive;
    function  LockTable(const Wait: Boolean): Boolean;
    procedure UnlockTable;
    procedure OpenIndexFile(IndexFile: string);
    procedure DeleteIndex(const AIndexName: string);
    procedure CloseIndexFile(const AIndexName: string);
    procedure RepageIndexFile(const AIndexFile: string);
    procedure CompactIndexFile(const AIndexFile: string);

{$ifdef SUPPORT_VARIANTS}
    function  Lookup(const KeyFields: string; const KeyValues: Variant; const ResultFields: string): Variant; override;
    function  Locate(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions): Boolean; override;
{$endif}

    function IsSequenced: Boolean; override; // lsp
    function  IsDeleted: Boolean;
    procedure Undelete;

    procedure CreateTable;
    procedure CreateTableEx(ADbfFieldDefs: TDbfFieldDefs);
    procedure CopyFrom(DataSet: TDataSet; FileName: string; DateTimeAsString: Boolean; Level: Integer);
    procedure BatchMove(DataSet: TDataSet; FileName: string; DateTimeAsString: Boolean; Level: Integer; Mode: TDbfBatchMode; Options: TDbfBatchOptions; FieldMappings: TStrings);
    procedure RestructureTable(ADbfFieldDefs: TDbfFieldDefs; Pack: Boolean);
    procedure RestructureTable2(ADbfFieldDefs: TDbfFieldDefs; Pack, DeleteFiles: Boolean);
    procedure PackTable;
    procedure EmptyTable;
    procedure Zap;
    procedure BatchStart;
    procedure BatchUpdate;
    procedure BatchFinish;
    function DeleteMdxFile: Boolean;

{$ifndef SUPPORT_INITDEFSFROMFIELDS}
    procedure InitFieldDefsFromFields;
{$endif}

    property AbsolutePath: string read FAbsolutePath;
    property DbfFieldDefs: TDbfFieldDefs read GetDbfFieldDefs;
    property PhysicalRecNo: Integer read GetPhysicalRecNo write SetPhysicalRecNo;
    property LanguageID: Byte read FLanguageID write SetLanguageID;
    property LanguageStr: String read GetLanguageStr;
    property CodePage: Cardinal read GetCodePage;
    property ExactRecordCount: Integer read GetExactRecordCount;
    property PhysicalRecordCount: Integer read GetPhysicalRecordCount;
    property KeySize: Integer read GetKeySize;
    property DbfFile: TDbfFile read FDbfFile;
    property UserStream: TStream read FUserStream write FUserStream;
    property DisableResyncOnPost: Boolean read FDisableResyncOnPost write FDisableResyncOnPost;
    // 10.09.2007
    property CopyDateTimeAsString: Boolean read FCopyDateTimeAsString write FCopyDateTimeAsString;
    property InCopyFrom: Boolean read FInCopyFrom write FInCopyFrom;
    //

  published
    property DateTimeHandling: TDateTimeHandling
             read FDateTimeHandling write FDateTimeHandling default dtBDETimeStamp;
    property Exclusive: Boolean read FExclusive write FExclusive default false;
    property FilePath: string     read FRelativePath write SetFilePath;
    property FilePathFull: string read FAbsolutePath write SetFilePath stored false;
    property Indexes: TDbfIndexDefs read FIndexDefs write SetDbfIndexDefs stored false;
    property IndexDefs: TDbfIndexDefs read FIndexDefs write SetDbfIndexDefs;
    property IndexFieldNames: string read GetIndexFieldNames write SetIndexFieldNames stored false;
    property IndexName: string read GetIndexName write SetIndexName;
    property MasterFields: string read GetMasterFields write SetMasterFields;
    property MasterSource: TDataSource read GetDataSource write SetDataSource;
    property OpenMode: TDbfOpenMode read FOpenMode write FOpenMode default omNormal;
    property ReadOnly: Boolean read FReadOnly write FReadonly default false;
    property ShowDeleted: Boolean read FShowDeleted write SetShowDeleted default false;
    property Storage: TDbfStorage read FStorage write FStorage default stoFile;
    property StoreDefs: Boolean read FStoreDefs write FStoreDefs default False;
    property TableName: string read FTableName write SetTableName;
    property TableLevel: Integer read FTableLevel write SetTableLevel;
    property Version: string read GetVersion write SetVersion stored false;
    property BeforeAutoCreate: TBeforeAutoCreateEvent read FBeforeAutoCreate write FBeforeAutoCreate;
    property OnCompareRecord: TNotifyEvent read FOnCompareRecord write FOnCompareRecord;
    property OnLanguageWarning: TLanguageWarningEvent read FOnLanguageWarning write FOnLanguageWarning;
    property OnLocaleError: TDbfLocaleErrorEvent read FOnLocaleError write FOnLocaleError;
    property OnIndexInvalid: TDbfIndexInvalidEvent read FOnIndexInvalid write FOnIndexInvalid;
    property OnIndexMissing: TDbfIndexMissingEvent read FOnIndexMissing write FOnIndexMissing;
    property OnCopyDateTimeAsString: TConvertFieldEvent read FOnCopyDateTimeAsString write FOnCopyDateTimeAsString;
    property OnTranslate: TTranslateEvent read FOnTranslate write FOnTranslate;
    property OnProgress: TPagedFileProgressEvent read FOnProgress write FOnProgress;

    // redeclared data set properties
    property Active;
    property FieldDefs stored FieldDefsStored;
    property Filter;
    property Filtered;
    property FilterOptions;
    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeInsert;
    property AfterInsert;
    property BeforeEdit;
    property AfterEdit;
    property BeforePost;
    property AfterPost;
    property BeforeCancel;
    property AfterCancel;
    property BeforeDelete;
    property AfterDelete;
    property BeforeRefresh;
    property AfterRefresh;
    property BeforeScroll;
    property AfterScroll;
    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnFilterRecord;
    property OnNewRecord;
    property OnPostError;
  end;

  TDbf_GetBasePathFunction = function: string;

var
  DbfBasePath: TDbf_GetBasePathFunction;

implementation

uses
{$ifdef SUPPORT_GENERICS_FIELDLIST}
  System.Generics.Collections,
{$endif}
  SysUtils,
{$ifdef FPC}
  dbconst,
{$else}
  DBConsts,
{$endif}
{$ifdef WINDOWS}
  Windows,
{$else}
{$ifdef KYLIX}
  Libc,
{$endif}
  Types,
  dbf_wtil,
{$endif}
{$ifdef SUPPORT_SEPARATE_VARIANTS_UNIT}
  Variants,
{$endif}
  dbf_ansistrings,
  dbf_idxcur,
  dbf_memo,
  dbf_str;

{$ifdef FPC}
const
  // TODO: move these to DBConsts
  SNotEditing = 'Dataset not in edit or insert mode';
//SCircularDataLink = 'Circular datalinks are not allowed';
{$endif}

function TableLevelToDbfVersion(TableLevel: integer): TXBaseVersion;
begin
  case TableLevel of
    3:                      Result := xBaseIII;
    4:                      Result := xBaseIV;
    5:                      Result := xBaseV;
    7:                      Result := xBaseVII;
    TDBF_TABLELEVEL_FOXPRO: Result := xFoxPro;
  else
    Result := xUnknown;
  end;
end;

//==========================================================
//============ TDbfBlobStream
//==========================================================
constructor TDbfBlobStream.Create(FieldVal: TField);
begin
  FBlobField := FieldVal as TBlobField;
  FReadSize := 0;
  FMemoRecNo := 0;
  FRefCount := 1;
  FDirty := false;
end;

destructor TDbfBlobStream.Destroy;
begin
  // only continue destroy if all references released
  if FRefCount = 1 then
  begin
    // this is the last reference
    inherited
  end else begin
    // fire event when dirty, and the last "user" is freeing it's reference
    // tdbf always has the last reference
    if FDirty and (FRefCount = 2) then
    begin
      // a second referer to instance has changed the data, remember modified
//      TDbf(FBlobField.DataSet).SetModified(true);
      // is following better? seems to provide notification for user (from VCL)
      if not (FBlobField.DataSet.State in [dsCalcFields, dsFilter, dsNewValue]) then
        TDbf(FBlobField.DataSet).DataEvent(deFieldChange, PtrInt(FBlobField));
    end;
  end;
  Dec(FRefCount);
end;

procedure TDbfBlobStream.FreeInstance;
begin
  // only continue freeing if all references released
  if FRefCount = 0 then
    inherited;
end;

procedure TDbfBlobStream.SetMode(NewMode: TBlobStreamMode);
begin
  FMode := NewMode;
  FDirty := FDirty or (NewMode = bmWrite) or (NewMode = bmReadWrite);
end;

procedure TDbfBlobStream.Cancel;
begin
  FDirty := false;
  FMemoRecNo := -1;
end;

procedure TDbfBlobStream.Commit;
var
  Dbf: TDbf;
  Src: Pointer;
begin
  if FDirty then
  begin
    Size := Position; // Strange but it leave tailing trash bytes if I do not write that.
    Dbf := TDbf(FBlobField.DataSet);
    Translate(true);
    Dbf.FDbfFile.MemoFile.WriteMemo(FMemoRecNo, FReadSize, Self);
    if Size <> 0 then
      Src := @FMemoRecNo
    else
      Src := nil;
    Dbf.FDbfFile.SetFieldData(FBlobField.FieldNo-1, ftInteger, Src,
      @pDbfRecord(TDbf(FBlobField.DataSet).ActiveBuffer)^.DeletedFlag, false);
    FDirty := false;
  end;
end;

function TDbfBlobStream.AddReference: TDbfBlobStream;
begin
  Inc(FRefCount);
  Result := Self;
end;

function TDbfBlobStream.GetTransliterate: Boolean;
begin
  Result := FBlobField.Transliterate;
end;

procedure TDbfBlobStream.Translate(ToOem: Boolean);
var
  bytesToDo, numBytes: Integer;
  bufPos: PAnsiChar;
  saveChar: AnsiChar;
begin
  if (Transliterate) and (Size > 0) then
  begin
    // get number of bytes to be translated
    bytesToDo := Size;
    // make space for final null-terminator
    Size := Size + 1;
    bufPos := Memory;
    repeat
      // process blocks of 512 bytes
      numBytes := bytesToDo;
      if numBytes > 512 then
        numBytes := 512;
      // null-terminate memory
      saveChar := bufPos[numBytes];
      bufPos[numBytes] := #0;
      // translate memory
      TDbf(FBlobField.DataSet).Translate(bufPos, bufPos, ToOem);
      // restore char
      bufPos[numBytes] := saveChar;
      // numBytes bytes translated
      Dec(bytesToDo, numBytes);
      Inc(bufPos, numBytes);
    until bytesToDo = 0;
    // cut ending null-terminator
    Size := Size - 1;
  end;
end;

//====================================================================
// TDbf = TDataset Descendant.
//====================================================================
constructor TDbf.Create(AOwner: TComponent); {override;}
begin
  inherited;

  BookmarkSize := sizeof(TBookmarkData);
  FIndexDefs := TDbfIndexDefs.Create(Self);
  FMasterLink := TDbfMasterLink.Create(Self);
  FMasterLink.OnMasterChange := MasterChanged;
  FMasterLink.OnMasterDisable := MasterDisabled;
  FDateTimeHandling := dtBDETimeStamp;
  FStorage := stoFile;
  FOpenMode := omNormal;
  FParser := nil;
  FPosting := false;
  FReadOnly := false;
  FExclusive := false;
  FDisableResyncOnPost := false;
  FTempExclusive := false;
  FCopyDateTimeAsString := false;
  FInCopyFrom := false;
  FFindRecordFilter := false;
  FEditingRecNo := -1;
  FTableLevel := 5;
  FIndexName := EmptyStr;
  FilePath := EmptyStr;
  FTempBuffer := nil;
  FFilterBuffer := nil;
  FIndexFile := nil;
  FOnTranslate := nil;
  FOnCopyDateTimeAsString := nil;
  FLanguageID := DbfGlobals.DefaultCreateLangId; 
end;

destructor TDbf.Destroy; {override;}
var
  I: Integer;
begin
  inherited Destroy;

  if FIndexDefs <> nil then
  begin
    for I := FIndexDefs.Count - 1 downto 0 do
      TDbfIndexDef(FIndexDefs.Items[I]).Free;
    FIndexDefs.Free;
  end;
  FMasterLink.Free;

  FreeMemAndNil(FKeyBuffer);
end;

function TDbf.AllocRecordBuffer: TDbfRecordBuffer; {override virtual abstract from TDataset}
begin
  GetMem(Result, SizeOf(TDbfRecordHeader) + RecordSize + CalcFieldsSize + 1);
end;

procedure TDbf.FreeRecordBuffer(var Buffer: TDbfRecordBuffer); {override virtual abstract from TDataset}
begin
  FreeMemAndNil(Pointer(Buffer));
end;

procedure TDbf.GetBookmarkData(Buffer: TDbfRecordBuffer; Data: Pointer); {override virtual abstract from TDataset}
begin
  pBookmarkData(Data)^ := pDbfRecord(Buffer)^.BookmarkData;
  pBookmarkData(Data)^.PhysicalRecNo := SwapIntLE(DWORD(pBookmarkData(Data)^.PhysicalRecNo));
end;

function TDbf.GetBookmarkFlag(Buffer: TDbfRecordBuffer): TBookmarkFlag; {override virtual abstract from TDataset}
begin
  Result := pDbfRecord(Buffer)^.BookmarkFlag;
end;

function TDbf.GetCurrentBuffer: TDbfRecordBuffer;
begin
  case State of
    dsFilter:     Result := TDbfRecordBuffer(FFilterBuffer);
    dsCalcFields: Result := TDbfRecordBuffer(CalcBuffer);
    dsSetKey:     Result := TDbfRecordBuffer(GetKeyBuffer);
  else
    if IsEmpty then
    begin
      Result := nil;
    end else begin
      Result := TDbfRecordBuffer(ActiveBuffer);
    end;
  end;
  if Result <> nil then
    Result := @PDbfRecord(Result)^.DeletedFlag;
end;

// we don't want converted data formats, we want native :-)
// it makes coding easier in TDbfFile.GetFieldData
//  ftCurrency:
//    Delphi 3,4: BCD array
//  ftBCD:
// ftDateTime is more difficult though

{$ifdef SUPPORT_TVALUEBUFFER_VAR}
function TDbf.GetFieldData(Field: TField; var Buffer: TDbfValueBuffer): Boolean; {override virtual abstract from TDataset}
{$else SUPPORT_TVALUEBUFFER_VAR}
function TDbf.GetFieldData(Field: TField; Buffer: TDbfValueBuffer): Boolean; {override virtual abstract from TDataset}
{$endif SUPPORT_TVALUEBUFFER_VAR}
{$ifdef SUPPORT_OVERLOAD}
begin
  { calling through 'old' delphi 3 interface, use compatible/'native' format }
  Result := GetFieldData(Field, Buffer, true);
end;

{$ifdef SUPPORT_TVALUEBUFFER_VAR}
function TDbf.GetFieldData(Field: TField; var Buffer: TDbfValueBuffer; NativeFormat: Boolean): Boolean; {overload; override;}
{$else SUPPORT_TVALUEBUFFER_VAR}
function TDbf.GetFieldData(Field: TField; Buffer: TDbfValueBuffer; NativeFormat: Boolean): Boolean; {overload; override;}
{$endif SUPPORT_TVALUEBUFFER_VAR}
{$else}
const
  { no overload => delphi 3 => use compatible/'native' format }
  NativeFormat = true;
{$endif}
var
  Src: TDbfRecordBuffer;
begin
  Src := GetCurrentBuffer;
  if Src = nil then
  begin
    Result := false;
    exit;
  end;

  if Field.FieldNo>0 then
  begin
    Result := FDbfFile.GetFieldData(Field.FieldNo-1, Field.DataType, Src, Buffer, NativeFormat);
  end else begin { weird calculated fields voodoo (from dbtables).... }
    Inc(Src, Field.Offset + GetRecordSize); // Was PChar(Src)
    Result := Boolean(Src[0]);
    if Result and (Buffer <> nil) then begin
{$ifdef SUPPORT_TVALUEBUFFER}
      Move(Src[1], Buffer[0], Field.DataSize);
{$else}
      Move(Src[1], Buffer^, Field.DataSize);
{$endif}
    end;
  end;
end;

procedure TDbf.SetFieldData(Field: TField; Buffer: TDbfValueBuffer); {override virtual abstract from TDataset}
{$ifdef SUPPORT_OVERLOAD}
begin
  { calling through 'old' delphi 3 interface, use compatible/'native' format }
  SetFieldData(Field, Buffer, true);
end;

procedure TDbf.SetFieldData(Field: TField; Buffer: TDbfValueBuffer; NativeFormat: Boolean); {overload; override;}
{$else}
const
  { no overload => delphi 3 => use compatible/'native' format }
  NativeFormat = true;
{$endif}
var
  Dst: PAnsiChar;
begin
  if (Field.FieldNo >= 0) then
  begin
    if Field.ReadOnly and not (State in [dsSetKey, dsFilter]) then
      DatabaseErrorFmt({$ifdef FPC}SReadOnlyField{$else}SFieldReadOnly{$endif}, [Field.DisplayName]);
    if State = dsSetKey then
      Dst := @PDbfRecord(GetKeyBuffer)^.DeletedFlag
    else
      Dst := @PDbfRecord(ActiveBuffer)^.DeletedFlag;
    FDbfFile.SetFieldData(Field.FieldNo - 1, Field.DataType, Buffer, Dst, NativeFormat);
  end else begin    { ***** fkCalculated, fkLookup ***** }
    Dst := @PDbfRecord(CalcBuffer)^.DeletedFlag;
    Inc(Dst, RecordSize + Field.Offset); // Was PChar(Dst)
    if Buffer <> nil then begin
      Dst[0] := #1;
{$ifdef SUPPORT_TVALUEBUFFER}
      Move(Buffer[0], Dst[1], Field.DataSize);
{$else}
      Move(Buffer^, Dst[1], Field.DataSize);
{$endif}
	end else
      Dst[0] := #0;
  end;     { end of ***** fkCalculated, fkLookup ***** }
  if not (State in [dsCalcFields, dsFilter, dsNewValue]) then begin
    DataEvent(deFieldChange, PtrInt(Field));
  end;
end;

procedure TDbf.DoFilterRecord(var Acceptable: Boolean);
begin
  // check filtertext
  if Length(Filter) > 0 then
  begin
{$ifndef VER1_0}
    Acceptable := Boolean((FParser.ExtractFromBuffer(PAnsiChar(GetCurrentBuffer), PhysicalRecNo))^);
{$else}
    // strange problem
    // dbf.pas(716,19) Error: Incompatible types: got "CHAR" expected "BOOLEAN"
    Acceptable := not ((FParser.ExtractFromBuffer(GetCurrentBuffer), PhysicalRecNo)^ = #0);
{$endif}
  end;

  // check user filter
  if Acceptable and Assigned(OnFilterRecord) then
    OnFilterRecord(Self, Acceptable);
end;

function TDbf.ReadCurrentRecord(Buffer: TDbfRecordBuffer; var Acceptable: Boolean): TGetResult;
var
  lPhysicalRecNo: Integer;
  pRecord: pDbfRecord;
begin
  lPhysicalRecNo := FCursor.PhysicalRecNo;
  if (lPhysicalRecNo = 0) or not FDbfFile.IsRecordPresent(lPhysicalRecNo) then
  begin
    Result := grError;
    Acceptable := false;
  end else begin
    Result := grOK;
    pRecord := pDbfRecord(Buffer);
    FDbfFile.ReadRecord(lPhysicalRecNo, @pRecord^.DeletedFlag);
    Acceptable := (FShowDeleted or (pRecord^.DeletedFlag <> '*'))
  end;
end;

function TDbf.GetRecord(Buffer: TDbfRecBuf; GetMode: TGetMode; DoCheck: Boolean): TGetResult; {override virtual abstract from TDataset}
var
  pRecord: pDbfRecord;
  acceptable: Boolean;
  SaveState: TDataSetState;
//  s: string;
  lSequentialRecNo: TSequentialRecNo;
begin
  if FCursor = nil then
  begin
    Result := grEOF;
    exit;
  end;

  pRecord := pDbfRecord(Buffer);
  lSequentialRecNo := FCursor.SequentialRecNo;
  acceptable := false;
  repeat
    Result := grOK;
    case GetMode of
      gmNext :
        begin
          Acceptable := FCursor.Next;
          if Acceptable then begin
            Result := grOK;
          end else begin
            Result := grEOF
          end;
        end;
      gmPrior :
        begin
          Acceptable := FCursor.Prev;
          if Acceptable then begin
            Result := grOK;
          end else begin
            Result := grBOF
          end;
        end;
    end;

    if (Result = grOK) then
    begin
      Result := ReadCurrentRecord(TDbfRecordBuffer(Buffer), acceptable);
      if lSequentialRecNo = 0 then
        lSequentialRecNo := FCursor.SequentialRecNo;
    end;

    if (Result = grOK) and acceptable then
    begin
      pRecord^.BookmarkData.PhysicalRecNo := FCursor.PhysicalRecNo;
      pRecord^.BookmarkFlag := bfCurrent;
      pRecord^.SequentialRecNo := FCursor.SequentialRecNo;
      GetCalcFields(Buffer);

      if Filtered or FFindRecordFilter then
      begin
        FFilterBuffer := TDbfRecordBuffer(Buffer);
        SaveState := SetTempState(dsFilter);
        DoFilterRecord(acceptable);
        RestoreState(SaveState);
      end;
    end;

    if (GetMode = gmCurrent) and not acceptable then
      Result := grError;
  until (Result <> grOK) or acceptable;

  if Result <> grOK then
  begin
    if lSequentialRecNo <> 0 then
      FCursor.SequentialRecNo := lSequentialRecNo;
    pRecord^.BookmarkData.PhysicalRecNo := -1;
  end;
end;

function TDbf.GetRecordSize: Word; {override virtual abstract from TDataset}
begin
  Result := FDbfFile.RecordSize;
end;

procedure TDbf.InternalAddRecord(Buffer: {$ifdef InternalAddRecord_Wants_TRecordBuffer}TDbfRecordBuffer{$else}Pointer{$endif}; Append: Boolean); {override virtual abstract from TDataset}
var
  pRecord: pDbfRecord;
  newRecord: integer;
begin
  pRecord := pDbfRecord(Buffer);

  // we can not insert records in DBF files, only append
  // ignore Append parameter
  newRecord := FDbfFile.Insert(@pRecord^.DeletedFlag);
  if newRecord > 0 then
    FCursor.PhysicalRecNo := newRecord;

  // set flag that TDataSet is about to post...so we can disable resync
  FPosting := true;
end;

procedure TDbf.InternalClose; {override virtual abstract from TDataset}
var
  lIndex: TDbfIndexDef;
  I: Integer;
begin
  // clear automatically added MDX index entries
  I := 0;
  while I < FIndexDefs.Count do
  begin
    // is this an MDX index?
    lIndex := FIndexDefs.Items[I];
    if (Length(ExtractFileExt(lIndex.IndexFile)) = 0) and
      TDbfIndexDef(FIndexDefs.Items[I]).Temporary then
    begin
{$ifdef SUPPORT_DEF_DELETE}
      // delete this entry
      FIndexDefs.Delete(I);
{$else}
      // does this work? I hope so :-)
      FIndexDefs.Items[I].Free;
{$endif}
    end else begin
      // NDX entry -> goto next
      Inc(I);
    end;
  end;

  // free blobs
  if FBlobStreams <> nil then
  begin
    for I := 0 to Pred(FieldDefs.Count) do
      FBlobStreams^[I].Free;
    FreeMemAndNil(Pointer(FBlobStreams));
  end;
  FreeRecordBuffer(FTempBuffer);
  // disconnect field objects
  BindFields(false);
  // Destroy field object (if not persistent)
  if DbfDefaultFields then
    DestroyFields;

  if FParser <> nil then
    FreeAndNil(FParser);
  FreeAndNil(FCursor);
  if FDbfFile <> nil then
    FreeAndNil(FDbfFile);
end;

procedure TDbf.InternalCancel;
var
  I: Integer;
begin
  // cancel blobs
  for I := 0 to Pred(FieldDefs.Count) do
    if Assigned(FBlobStreams^[I]) then
      FBlobStreams^[I].Cancel;
  // if we have locked a record, unlock it
  if FEditingRecNo >= 0 then
  begin
    FDbfFile.UnlockPage(FEditingRecNo);
    FEditingRecNo := -1;
  end;
end;

procedure TDbf.InternalDelete; {override virtual abstract from TDataset}
var
  lRecord: pDbfRecord;
begin
  // start editing
  InternalEdit;
  SetState(dsEdit);
  // get record pointer
  lRecord := pDbfRecord(ActiveBuffer);
  // flag we deleted this record
  lRecord^.DeletedFlag := '*';
  // notify indexes this record is deleted
  FDbfFile.RecordDeleted(FEditingRecNo, @lRecord^.DeletedFlag);
  // done!
  InternalPost;
end;

procedure TDbf.InternalFirst; {override virtual abstract from TDataset}
begin
  FCursor.First;
end;

procedure TDbf.InternalGotoBookmark(ABookmark: Pointer); {override virtual abstract from TDataset}
var
  APhysicalRecNo: Integer;
begin
  APhysicalRecNo := Integer(SwapIntLE(DWORD(PBookmarkData(ABookmark)^.PhysicalRecNo)));
  if (APhysicalRecNo = 0) then begin
    First;
  end else
  if (APhysicalRecNo = MaxInt) then begin
    Last;
  end else begin
    if FCursor.PhysicalRecNo <> APhysicalRecNo then
      FCursor.PhysicalRecNo := APhysicalRecNo;
  end;
end;

procedure TDbf.InternalHandleException; {override virtual abstract from TDataset}
begin
  SysUtils.ShowException(ExceptObject, ExceptAddr);
end;

procedure TDbf.GetFieldDefsFromDbfFieldDefs;
var
  I, N: Integer;
  TempFieldDef: TDbfFieldDef;
  TempMdxFile: TIndexFile;
  BaseName, lIndexName: string;
begin
  FieldDefs.Clear;

  // get all fields
  for I := 0 to FDbfFile.FieldDefs.Count - 1 do
  begin
    TempFieldDef := FDbfFile.FieldDefs.Items[I];
    // handle duplicate field names
    N := 1;
    BaseName := string(TempFieldDef.FieldName);
    while FieldDefs.IndexOf(string(TempFieldDef.FieldName))>=0 do
    begin
      Inc(N);
      TempFieldDef.FieldName := AnsiString(BaseName + IntToStr(N));
    end;
    // add field
    if TempFieldDef.FieldType in [ftString, ftBCD, ftBytes] then
      FieldDefs.Add(string(TempFieldDef.FieldName), TempFieldDef.FieldType, TempFieldDef.Size, false)
    else
      FieldDefs.Add(string(TempFieldDef.FieldName), TempFieldDef.FieldType, 0, false);

    if TempFieldDef.FieldType = ftFloat then
      FieldDefs[I].Precision := TempFieldDef.Precision;

{$ifdef SUPPORT_FIELDDEF_ATTRIBUTES}
    // AutoInc fields are readonly
    if TempFieldDef.FieldType = ftAutoInc then
      FieldDefs[I].Attributes := [Db.faReadOnly];

    // if table has dbase lock field, then hide it
    if TempFieldDef.IsLockField then
      FieldDefs[I].Attributes := [Db.faHiddenCol];
{$endif}
  end;

  // get all (new) MDX index defs
  TempMdxFile := FDbfFile.MdxFile;
  for I := 0 to FDbfFile.IndexNames.Count - 1 do
  begin
    // is this an MDX index?
    lIndexName := FDbfFile.IndexNames.Strings[I];
    if FDbfFile.IndexNames.Objects[I] = TempMdxFile then
      if FIndexDefs.GetIndexByName(lIndexName) = nil then
        TempMdxFile.GetIndexInfo(lIndexName, FIndexDefs.Add);
  end;
end;

procedure TDbf.InitFieldDefs;
begin
  InternalInitFieldDefs;
end;

procedure TDbf.InitDbfFile(FileOpenMode: TPagedFileMode);
const
  FileModeToMemMode: array[TPagedFileMode] of TPagedFileMode =
    (pfNone, pfMemoryCreate, pfMemoryOpen, pfMemoryCreate, pfMemoryOpen,
     pfMemoryCreate, pfMemoryOpen, pfMemoryOpen);
begin
  FDbfFile := TDbfFile.Create;
  if FStorage = stoMemory then
  begin
    FDbfFile.Stream := FUserStream;
    FDbfFile.Mode := FileModeToMemMode[FileOpenMode];
  end else begin
    FDbfFile.FileName := FAbsolutePath + FTableName;
    FDbfFile.Mode := FileOpenMode;
  end;
  FDbfFile.AutoCreate := false;
  FDbfFile.DateTimeHandling := FDateTimeHandling;
  FDbfFile.OnLocaleError := FOnLocaleError;
  FDbfFile.OnIndexInvalid := FOnIndexInvalid;
  FDbfFile.OnIndexMissing := FOnIndexMissing;
end;

procedure TDbf.InternalInitFieldDefs; {override virtual abstract from TDataset}
var
  MustReleaseDbfFile: Boolean;
begin
  MustReleaseDbfFile := false;
  with FieldDefs do
  begin
    if FDbfFile = nil then
    begin
      // do not AutoCreate file
      InitDbfFile(pfReadOnly);
      FDbfFile.Open;
      MustReleaseDbfFile := true;
    end;
    GetFieldDefsFromDbfFieldDefs;
    if MustReleaseDbfFile then
      FreeAndNil(FDbfFile);
  end;
end;

procedure TDbf.InternalInitRecord(Buffer: TDbfRecordBuffer); {override virtual abstract from TDataset}
var
  pRecord: pDbfRecord;
begin
  pRecord := pDbfRecord(Buffer);
  pRecord^.BookmarkData.PhysicalRecNo := 0;
  pRecord^.BookmarkFlag := bfCurrent;
  pRecord^.SequentialRecNo := 0;
// Init Record with zero and set autoinc field with next value
  FDbfFile.InitRecord(@pRecord^.DeletedFlag);
end;

procedure TDbf.InternalLast; {override virtual abstract from TDataset}
begin
  FCursor.Last;
end;

procedure TDbf.DetermineTranslationMode;
var
  lCodePage: Cardinal;
begin
  lCodePage := FDbfFile.UseCodePage;
  if lCodePage = GetACP then
    FTranslationMode := tmNoneNeeded
  else
  if lCodePage = GetOEMCP then
    FTranslationMode := tmSimple
  // check if this code page, although non default, is installed
  else
  if DbfGlobals.CodePageInstalled(lCodePage) then
    FTranslationMode := tmAdvanced
  else
    FTranslationMode := tmNoneAvailable;
end;

function TDbf.DbfDefaultFields: Boolean;
begin
{$ifdef SUPPORT_FIELD_LIFECYCLES}
  Result := (FieldOptions.AutoCreateMode <> acExclusive) or not (lcPersistent in Fields.LifeCycles);
{$else}
  Result := DefaultFields;
{$endif}
end;

procedure TDbf.InternalOpen; {override virtual abstract from TDataset}
const
  DbfOpenMode: array[Boolean, Boolean] of TPagedFileMode =
     ((pfReadWriteOpen, pfExclusiveOpen), (pfReadOnly, pfReadOnly));
var
  lIndex: TDbfIndexDef;
  lIndexName: string;
  LanguageAction: TDbfLanguageAction;
  doCreate: Boolean;
  I: Integer;
begin
  // close current file
  FreeAndNil(FDbfFile);
  try

    // does file not exist? -> create
    if ((FStorage = stoFile) and
          not FileExists(FAbsolutePath + FTableName) and
          (FOpenMode in [omAutoCreate, omTemporary])) or
       ((FStorage = stoMemory) and (FUserStream = nil)) then
    begin
      doCreate := true;
      if Assigned(FBeforeAutoCreate) then
        FBeforeAutoCreate(Self, doCreate);
      if doCreate then
        CreateTable
      else
        exit;
    end;

    // now we know for sure the file exists
    InitDbfFile(DbfOpenMode[FReadOnly, FExclusive]);
    FDbfFile.Open;
    UpdateLock;

    // fail open?
  {$ifndef FPC}
    if FDbfFile.ForceClose then
      Abort;
  {$endif}

    // determine dbf version
    case FDbfFile.DbfVersion of
      xBaseIII: FTableLevel := 3;
      xBaseIV:  FTableLevel := 4;
      xBaseV:   FTableLevel := 5;
      xBaseVII: FTableLevel := 7;
      xFoxPro:  FTableLevel := TDBF_TABLELEVEL_FOXPRO;
    end;
    // 11.09.2007 Есди 0, например DBaseIII, будем считать из DbfGlobals
    if FDbfFile.LanguageID=0 then  begin
      FDbfFile.UseCodePage := DbfGlobals.DefaultCreateCodePage; // GETACPOEM;
      FDbfFile.FileLangId := DbfGlobals.DefaultCreateLangId;     // DbfLangId_RUS_866
    end;
    // Реальный locale из заголовка файла
    FLanguageID := FDbfFile.LanguageID;


    // build VCL fielddef list from native DBF FieldDefs
  (*
    if (FDbfFile.HeaderSize = 0) or (FDbfFile.FieldDefs.Count = 0) then
    begin
      if FieldDefs.Count > 0 then
      begin
        CreateTableFromFieldDefs;
      end else begin
        CreateTableFromFields;
      end;
    end else begin
  *)
  //    GetFieldDefsFromDbfFieldDefs;
  //  end;

  {$ifdef SUPPORT_FIELDDEFS_UPDATED}
    FieldDefs.Updated := False;
    FieldDefs.Update;
  {$else}
    InternalInitFieldDefs;
  {$endif}

    // create the fields dynamically
    if DbfDefaultFields then
      CreateFields; // Create fields from fielddefs.

    BindFields(true);

    // create array of blobstreams to store memo's in. each field is a possible blob
    FBlobStreams := AllocMem(FieldDefs.Count * SizeOf(TDbfBlobStream));

    // check codepage settings
    DetermineTranslationMode;
    if FTranslationMode = tmNoneAvailable then
    begin
      // no codepage available? ask user
      LanguageAction := laReadOnly;
      if Assigned(FOnLanguageWarning) then
        FOnLanguageWarning(Self, LanguageAction);
      case LanguageAction of
        laReadOnly: FTranslationMode := tmNoneAvailable;
        laForceOEM:
          begin
            FDbfFile.UseCodePage := GetOEMCP;
            FTranslationMode := tmSimple;
          end;
        laForceANSI:
          begin
            FDbfFile.UseCodePage := GetACP;
            FTranslationMode := tmNoneNeeded;
          end;
        laDefault:
          begin
            FDbfFile.UseCodePage := DbfGlobals.DefaultOpenCodePage;
            DetermineTranslationMode;
          end;
      end;
    end;

    // allocate a record buffer for temporary data
    FTempBuffer := AllocRecordBuffer;

    // open indexes
    for I := 0 to FIndexDefs.Count - 1 do
    begin
      lIndex := FIndexDefs.Items[I];
      lIndexName := ParseIndexName(lIndex.IndexFile);
      // if index does not exist -> create, if it does exist -> open only
      FDbfFile.OpenIndex(lIndexName, lIndex.SortField, false, lIndex.Options);
    end;

    // parse filter expression
    try
      ParseFilter(Filter);
    except
      // oops, a problem with parsing, clear filter for now
      on E: EDbfError do Filter := EmptyStr;
    end;

    SetIndexName(FIndexName);

  // SetIndexName will have made the cursor for us if no index selected :-)
  //  if FCursor = nil then FCursor := TDbfCursor.Create(FDbfFile);

    if FMasterLink.Active and Assigned(FIndexFile) then
      CheckMasterRange;
    InternalFirst;

  //  FDbfFile.SetIndex(FIndexName);
  //  FDbfFile.FIsCursorOpen := true;
  except
    InternalClose;
    raise;
  end;
end;

function TDbf.GetCodePage: Cardinal;
begin
  if FDbfFile <> nil then
    Result := FDbfFile.UseCodePage
  else
    Result := 0;
end;

function TDbf.GetLanguageStr: String;
begin
  if FDbfFile <> nil then
    Result := string(FDbfFile.LanguageStr);
end;

function TDbf.LockTable(const Wait: Boolean): Boolean;
begin
  if not(Assigned(FDbfFile) and FDbfFile.Active) then
    CheckActive;
  Result := FDbfFile.LockAllPages(Wait);
  UpdateLock;
end;

procedure TDbf.UnlockTable;
begin
  CheckActive;
  FDbfFile.UnlockAllPages;
  UpdateLock;
end;

procedure TDbf.InternalEdit;
var
  I: Integer;
begin
  // store recno we are editing
  FEditingRecNo := FCursor.PhysicalRecNo;
  // reread blobs, execute cancel -> clears remembered memo pageno,
  // causing it to reread the memo contents
  for I := 0 to Pred(FieldDefs.Count) do
    if Assigned(FBlobStreams^[I]) then
      FBlobStreams^[I].Cancel;
  // try to lock this record
  FDbfFile.LockRecord(FEditingRecNo, @pDbfRecord(ActiveBuffer)^.DeletedFlag, BufferCount = 1);
  // succeeded!
end;

{$ifndef FPC}
{$ifndef DELPHI_3}

procedure TDbf.InternalInsert; {override virtual from TDataset}
begin
  CursorPosChanged;
end;

{$endif}
{$endif}

procedure TDbf.InternalPost; {override virtual from TDataset}
var
  pRecord: pDbfRecord;
  I, newRecord: Integer;
begin
  // inherited method checks required fields
{$ifdef FPC}
  inherited;
{$else}
{$ifdef DELPHI_7} // perhaps ifdef DELPHI_6
  inherited;
{$endif}
{$endif}
  // if internalpost is called, we know we are active
  pRecord := pDbfRecord(ActiveBuffer);
  // commit blobs
  for I := 0 to Pred(FieldDefs.Count) do
    if Assigned(FBlobStreams^[I]) then
      FBlobStreams^[I].Commit;
  if State = dsEdit then
  begin
    // write changes
    FDbfFile.UnlockRecord(FEditingRecNo, @pRecord^.DeletedFlag);
    // not editing anymore
    FEditingRecNo := -1;
  end else begin
    // insert
    newRecord := FDbfFile.Insert(@pRecord^.DeletedFlag);
    if newRecord > 0 then
    begin
      FCursor.PhysicalRecNo := newRecord;
      pRecord^.BookmarkData.PhysicalRecNo := newRecord;
      pRecord^.BookmarkFlag := bfCurrent;
      pRecord^.SequentialRecNo := FCursor.SequentialRecNo;
    end;
  end;
  // set flag that TDataSet is about to post...so we can disable resync
  FPosting := true;
end;

procedure TDbf.InternalRefresh;
begin
  if Assigned(FDbfFile) then
    FDbfFile.ResyncSharedReadBuffer;
  inherited;
end;

procedure TDbf.Resync(Mode: TResyncMode);
begin
  // try to increase speed
  if not FDisableResyncOnPost or not FPosting then
    inherited;
  // clear post flag
  FPosting := false;
end;


{$ifndef SUPPORT_INITDEFSFROMFIELDS}

procedure TDbf.InitFieldDefsFromFields;
var
  I: Integer;
  F: TField;
begin
  { create fielddefs from persistent fields if needed }
  for I := 0 to FieldCount - 1 do
  begin
    F := Fields[I];
    with F do
    if FieldKind = fkData then begin
      FieldDefs.Add(FieldName,DataType,Size,Required);
    end;
  end;
end;

{$endif}

procedure TDbf.CreateTable;
begin
  CreateTableEx(nil);
end;

procedure TDbf.CheckDbfFieldDefs(ADbfFieldDefs: TDbfFieldDefs);
var
  I: Integer;
  TempDef: TDbfFieldDef;

    function FieldTypeStr(const FieldType: AnsiChar): string;
    begin
      if FieldType = #0 then
        Result := 'NULL'
      else if FieldType > #127 then
        Result := 'ASCII '+IntToStr(Byte(FieldType))
      else
        Result := ' "'+fieldType+'" ';
      Result := ' ' + Result + '(#'+IntToHex(Byte(FieldType),SizeOf(FieldType))+') '
    end;

begin
  if ADbfFieldDefs = nil then exit;

  for I := 0 to ADbfFieldDefs.Count - 1 do
  begin
    // check dbffielddefs for errors
    TempDef := ADbfFieldDefs.Items[I];
    if FTableLevel < 7 then begin
      if not CharInSet(TempDef.NativeFieldType, ['C', 'F', 'N', 'D', 'L', 'M']) then
        raise EDbfError.CreateFmt(STRING_INVALID_FIELD_TYPE,
          [FieldTypeStr(TempDef.NativeFieldType), TempDef.FieldName]);
      end;
  end;
end;

procedure TDbf.CreateTableEx(ADbfFieldDefs: TDbfFieldDefs);
var
  I: Integer;
  lIndex: TDbfIndexDef;
  lIndexName: string;
  tempFieldDefs: Boolean;
begin
  CheckInactive;
  tempFieldDefs := ADbfFieldDefs = nil;
  try
    try
      if tempFieldDefs then
      begin
        ADbfFieldDefs := TDbfFieldDefs.Create(Self);
        ADbfFieldDefs.DbfVersion := TableLevelToDbfVersion(FTableLevel);

        // get fields -> fielddefs if no fielddefs
{$ifndef FPC_VERSION}
        if FieldDefs.Count = 0 then
          InitFieldDefsFromFields;
{$endif}

        // fielddefs -> dbffielddefs
        for I := 0 to FieldDefs.Count - 1 do
        begin
          with ADbfFieldDefs.AddFieldDef do
          begin
            FieldName := AnsiString(FieldDefs.Items[I].Name);
            FieldType := FieldDefs.Items[I].DataType;
            if FieldDefs.Items[I].Size > 0 then
            begin
              Size := FieldDefs.Items[I].Size;
              Precision := FieldDefs.Items[I].Precision;
            end else begin
              SetDefaultSize;
            end;
          end;
        end;
      end;

      InitDbfFile(pfExclusiveCreate);
      FDbfFile.CopyDateTimeAsString := FInCopyFrom and FCopyDateTimeAsString;
      FDbfFile.DbfVersion := TableLevelToDbfVersion(FTableLevel);
      FDbfFile.FileLangID := FLanguageID;
      FDbfFile.Open;
      FDbfFile.FinishCreate(ADbfFieldDefs, 512);
      UpdateLock;

      // if creating memory table, copy stream pointer
      if FStorage = stoMemory then
        FUserStream := FDbfFile.Stream;

      // create all indexes
      for I := 0 to FIndexDefs.Count-1 do
      begin
        lIndex := FIndexDefs.Items[I];
        lIndexName := ParseIndexName(lIndex.IndexFile);
        FDbfFile.OpenIndex(lIndexName, lIndex.SortField, true, lIndex.Options);
      end;
    except
      // dbf file created?
      if (FDbfFile <> nil) and (FStorage = stoFile) then
      begin
        FreeAndNil(FDbfFile);
        SysUtils.DeleteFile(FAbsolutePath+FTableName);
      end;
      raise;
    end;
  finally
    // free temporary fielddefs
    if tempFieldDefs and Assigned(ADbfFieldDefs) then
      ADbfFieldDefs.Free;
    FreeAndNil(FDbfFile);
  end;
end;

procedure TDbf.EmptyTable;
begin
  Zap;
end;

procedure TDbf.Zap;
begin
  // are we active?
  CheckActive;
  FDbfFile.Zap;
end;

procedure TDbf.BatchStart;
begin
  DisableControls;
  if Assigned(FDbfFile) then
    FDbfFile.BatchStart;
  FInCopyFrom := True;
end;

procedure TDbf.BatchUpdate;
begin
  if Assigned(FDbfFile) then
    FDbfFile.BatchUpdate;
end;

procedure TDbf.BatchFinish;
begin
  FInCopyFrom := False;
  if Assigned(FDbfFile) then
    FDbfFile.BatchFinish;
  EnableControls;
end;

function TDbf.DeleteMdxFile: Boolean;
begin
  CheckActive;
  Result:= Assigned(DbfFile.MdxFile);
  if Result then
  begin
    IndexName:= '';
    DbfFile.DeleteMdxFile;
    InternalInitFieldDefs;
  end;
end;

procedure TDbf.RestructureTable(ADbfFieldDefs: TDbfFieldDefs; Pack: Boolean);
begin
  // if restructure -> rename the old dbf files
  // if pack only -> delete the old dbf files
  RestructureTable2(ADbfFieldDefs, Pack, ADbfFieldDefs = nil);
end;

procedure TDbf.RestructureTable2(ADbfFieldDefs: TDbfFieldDefs;
  Pack, DeleteFiles: Boolean);
begin
  CheckInactive;

  // check field defs for errors
  CheckDbfFieldDefs(ADbfFieldDefs);

  // open dbf file
  InitDbfFile(pfExclusiveOpen);
  FDbfFile.Open;
  UpdateLock;

  // do restructure
  try
    BatchStart;
    try
      FDbfFile.RestructureTable2(ADbfFieldDefs, Pack, DeleteFiles);
    finally
      BatchFinish;
    end;
  finally
    // close file
    FreeAndNil(FDbfFile);
  end;
end;

procedure TDbf.PackTable;
var
  oldIndexName: string;
begin
  CheckBrowseMode;
  // deselect any index while packing
  oldIndexName := IndexName;
  IndexName := EmptyStr;
  // pack
  FDbfFile.OnProgress := FOnProgress;
  try
    BatchStart;
    try
      FDbfFile.RestructureTable(nil, true);
    finally
      BatchFinish;
    end;
  finally
    FDbfFile.OnProgress := nil;
  end;
  // reselect index
  IndexName := oldIndexName;
end;

procedure TDbf.CopyFrom(DataSet: TDataSet; FileName: string;
  DateTimeAsString: Boolean; Level: Integer);
begin
  BatchMove(DataSet, FileName, DateTimeAsString, Level, bmCopy, [], nil);
end;

procedure TDbf.BatchMove(DataSet: TDataSet; FileName: string;
  DateTimeAsString: Boolean; Level: Integer; Mode: TDbfBatchMode;
  Options: TDbfBatchOptions; FieldMappings: TStrings);
var                                                                                                                                                                               // 03/08/2011 spb CR 18716
  lPhysFieldDefs, lFieldDefs: TDbfFieldDefs;                                                                                                                                      // 03/08/2011 pb  CR 18706
  lSrcField, lDestField: TField;
  I: integer;
  cur, last: Integer;
  lSourceFields: TList;
  lDestinationFields: TList;
  lSourceFieldCount : Integer;
  lDestinationFieldCount : Integer;
  SourceName: string;
  DestinationName: string;
  lSrcFieldDef, lDestFieldDef: TDbfFieldDef;
  CopyLen: Integer;
  SrcBuffer: PAnsiChar;
  DestBuffer: PAnsiChar;
  CopyBlob: Boolean;
  BlobStream: TMemoryStream;
  lBlobPageNo: Integer;

  procedure GetFieldMappingNames;
  var
    SeparatorPos: Integer;
  begin
    SeparatorPos := Pos('=', FieldMappings[I]);
    if SeparatorPos > 1 then
    begin
      SourceName := Trim(Copy(FieldMappings[I], 1, Pred(SeparatorPos)));
      DestinationName := Trim(Copy(FieldMappings[I], Succ(SeparatorPos), Length(FieldMappings[I])));
    end
    else
    begin
      SourceName := Trim(FieldMappings[I]);
      DestinationName := SourceName;
    end;
  end;

  procedure AddSourceField;
  begin
    if Assigned(lSrcField) then
      with lFieldDefs.AddFieldDef do
      begin
        if Length(lSrcField.Name) > 0 then
          FieldName := AnsiString(lSrcField.Name)
        else
          FieldName := AnsiString(lSrcField.FieldName);
        FieldType := lSrcField.DataType;
        Required := lSrcField.Required;
        if (1 <= lSrcField.FieldNo)
            and (lSrcField.FieldNo <= lPhysFieldDefs.Count) then
        begin
          Size := lPhysFieldDefs.Items[lSrcField.FieldNo-1].Size;
          Precision := lPhysFieldDefs.Items[lSrcField.FieldNo-1].Precision;
        end;
      end;
  end;

begin
//FInCopyFrom := true;
  lFieldDefs := TDbfFieldDefs.Create(nil);
  lPhysFieldDefs := TDbfFieldDefs.Create(nil);
  lSourceFields := nil;
  lDestinationFields := nil;
  try
    if Active then
      Close;
    FilePath := ExtractFilePath(FileName);
    TableName := ExtractFileName(FileName);
    FCopyDateTimeAsString := DateTimeAsString;
    TableLevel := Level;
    if not DataSet.Active then
      DataSet.Open;
    DataSet.FieldDefs.Update;
    // first get a list of physical field defintions
    // we need it for numeric precision in case source is tdbf
    if DataSet is TDbf then
    begin
      lPhysFieldDefs.Assign(TDbf(DataSet).DbfFieldDefs);
      IndexDefs.Assign(TDbf(DataSet).IndexDefs);
    end else begin
{$ifdef SUPPORT_FIELDDEF_TPERSISTENT}
      lPhysFieldDefs.Assign(DataSet.FieldDefs);
{$endif}
      IndexDefs.Clear;
    end;
    // convert list of tfields into a list of tdbffielddefs
    // so that our tfields will correspond to the source tfields
    lSourceFields := TList.Create;
    lDestinationFields := TList.Create;
    if Mode = bmCopy then
    begin
      if boUsePhysicalfieldNo in Options then
        lSourceFieldCount := DataSet.FieldDefs.Count
      else
        lSourceFieldCount := DataSet.FieldCount;
      if Assigned(FieldMappings) and (FieldMappings.Count > 0) then
        for I := 0 to Pred(FieldMappings.Count) do
        begin
          GetFieldMappingNames;
          lSrcField := DataSet.Fields.FindField(SourceName);
          AddSourceField;
          lSourceFields.Add(Pointer(lSrcField));
        end
      else
        for I := 0 to Pred(lSourceFieldCount) do
        begin
          if boUsePhysicalfieldNo in Options then
            lSrcField := DataSet.Fields.FieldByNumber(Succ(I))
          else
            lSrcField := DataSet.Fields[I];
          AddSourceField;
          lSourceFields.Add(Pointer(lSrcField));
        end;
      CreateTableEx(lFieldDefs);
      Open;
      lDestinationFieldCount := FieldDefs.Count;
      for I := 1 to lDestinationFieldCount do
        lDestinationFields.Add(Pointer(Fields.FieldByNumber(I)));
    end
    else if Mode = bmAppend then
    begin
      Open;
      lDestinationFieldCount := FieldDefs.Count;
      lSourceFields.Count := lDestinationFieldCount;
      lDestinationFields.Count := lDestinationFieldCount;
      if Assigned(FieldMappings) then
        for I := 0 to Pred(FieldMappings.Count) do
        begin
          GetFieldMappingNames;
          lDestField := Fields.FindField(DestinationName);
          lSrcField := DataSet.Fields.FindField(SourceName);
          if Assigned(lDestField) then
            if (1 <= lDestField.FieldNo) and (lDestField.FieldNo <= lDestinationFieldCount) then
              begin
                lSourceFields[Pred(lDestField.FieldNo)] := lSrcField;
                lDestinationFields[Pred(lDestField.FieldNo)] := lDestField;
              end;
        end
      else
        for I := 1 to lDestinationFieldCount do
          if I <= DataSet.FieldDefs.Count then
            begin
              lSourceFields[Pred(I)] := Pointer(DataSet.Fields.FieldByNumber(I));
              lDestinationfields[Pred(I)] := Pointer(Fields.FieldByNumber(I));
            end;
    end;
    BatchStart;
    try
      if DataSet is TDbf then
        TDbf(DataSet).BatchStart;
      try
        cur := 0;
        if Assigned(FOnProgress) and (DataSet is TDbf) then
        begin
          last := TDbf(DataSet).PhysicalRecordCount;
          FDbfFile.OnProgress := FOnProgress;
          FDbfFile.DoProgress(cur, last, STRING_PROGRESS_APPENDINGRECORDS);
        end
        else
          last := -1;
        try
          BlobStream := TMemoryStream.Create;
          try
            while not DataSet.EOF do
            begin
              Append;
              for I := 0 to Pred(lDestinationFields.Count) do
              begin
                lSrcField := TField(lSourceFields[I]);
                lDestField := TField(lDestinationFields[I]);
                if Assigned(lSrcField) and Assigned(lDestField) then
                begin
                  CopyLen := -1;
                  CopyBlob := False;
                  if DataSet is TDbf then
                  begin
                    lSrcFieldDef := TDbf(DataSet).DbfFieldDefs.Items[Pred(lSrcField.FieldNo)];
                    lDestFieldDef := DbfFieldDefs.Items[Pred(lDestField.FieldNo)];
                    if lSrcFieldDef.NativeFieldType = lDestFieldDef.NativeFieldType then
                    begin
                      if lSrcFieldDef.IsBlob then
                        CopyBlob := True
                      else
                      begin
                        if lSrcFieldDef.NativeFieldType = 'C' then
                        begin
                          if lSrcFieldDef.Size > lDestFieldDef.Size then
                            CopyLen := lDestFieldDef.Size
                          else
                            CopyLen := lSrcFieldDef.Size;
                        end
                        else
                        begin
                          if (lSrcFieldDef.Size = lDestFieldDef.Size) and (lSrcFieldDef.Precision = lDestFieldDef.Precision) then
                            CopyLen := lSrcFieldDef.Size;
                        end;
                      end;
                    end
                  end
                  else
                  begin
                    lSrcFieldDef := nil;
                    lDestFieldDef := nil;
                  end;
                  SrcBuffer := PAnsiChar(@pDbfRecord(DataSet.ActiveBuffer).DeletedFlag);
                  DestBuffer := PAnsiChar(@pDbfRecord(ActiveBuffer).DeletedFlag);
                  if CopyBlob then
                  begin
                    if FDbfFile.GetFieldDataFromDef(lSrcFieldDef, ftInteger, SrcBuffer, @lBlobPageNo, false) and (lBlobPageNo > 0) then
                    begin
                      TDbf(DataSet).FDbfFile.MemoFile.ReadMemo(lBlobPageNo, BlobStream);
                      BlobStream.Position := 0;
                      FDbfFile.MemoFile.WriteMemo(lBlobPageNo, 0, BlobStream);
                      FDbfFile.SetFieldData(lDestFieldDef.Index, ftInteger, @lBlobPageNo, DestBuffer, false);
                      BlobStream.Clear;
                    end;
                  end
                  else
                  begin
                    if CopyLen > 0 then
                    begin
                      Inc(SrcBuffer, lSrcFieldDef.Offset);
                      Inc(DestBuffer, lDestFieldDef.Offset);
                      Move(SrcBuffer^, DestBuffer^, CopyLen);
                      if (lDestFieldDef.NativeFieldType = 'C') and (TDbf(DataSet).DbfFile.DbfVersion >= xBaseVII) and (lDestFieldDef.Size > lSrcFieldDef.Size) and (not lSrcField.IsNull) then
                        FillChar((DestBuffer + CopyLen)^, lDestFieldDef.Size - lSrcFieldDef.Size, ' ');
                    end
                    else
                    begin
                      if not lSrcField.IsNull then
                      begin
                        if lSrcField.DataType = ftDateTime then
                        begin
                          if FCopyDateTimeAsString then
                          begin
                            lDestField.AsString := lSrcField.AsString;
                            if Assigned(FOnCopyDateTimeAsString) then
                              FOnCopyDateTimeAsString(Self, lDestField, lSrcField)
                          end else
                            lDestField.AsDateTime := lSrcField.AsDateTime;
                        end else
                          lDestField.Assign(lSrcField);
                      end;
                    end;
                  end;
                end;
              end;
              Post;
              if DataSet is TDbf then
                if TDbf(DataSet).IsDeleted then
                  Delete;
              DataSet.Next;
              if last >= 0 then
              begin
                if TDbf(DataSet).FCursor is TIndexCursor then
                  Inc(cur)
                else
                  cur := TDbf(DataSet).PhysicalRecNo;
                FDbfFile.DoProgress(cur, last, STRING_PROGRESS_APPENDINGRECORDS);
              end;
            end;
            if (last >= 0) and (cur < last) then
            begin
              cur := last;
              FDbfFile.DoProgress(cur, last, STRING_PROGRESS_APPENDINGRECORDS);
            end;
          finally
            BlobStream.Free;
          end;
        finally
          if last >= 0 then
            FDbfFile.OnProgress:= nil;
        end;
      finally
        if DataSet is TDbf then
          TDbf(DataSet).BatchFinish;
      end;
    finally
      BatchFinish;
    end;
    BatchUpdate;
    Close;
  finally
//{$ifdef USE_CACHE}
//  if (DataSet is TDbf) and (TDbf(DataSet).DbfFile <> nil) then
//    TDbf(DataSet).DbfFile.BufferAhead := false;
//{$endif}      
//  FInCopyFrom := false;
//  FDbfFile.InCopyFrom := False;
    lFieldDefs.Free;
    lPhysFieldDefs.Free;
    lSourceFields.Free;
    lDestinationFields.Free;
  end;
end;

function TDbf.FindRecord(Restart, GoForward: Boolean): Boolean;
var
  oldRecNo: Integer;
begin
  CheckBrowseMode;
  DoBeforeScroll;
  Result := false;
  UpdateCursorPos;
  oldRecNo := RecNo;
  try
    FFindRecordFilter := true;
    if GoForward then
    begin
      if Restart then FCursor.First;
      Result := GetRecord(TDbfRecBuf(FTempBuffer), gmNext, false) = grOK;
    end else begin
      if Restart then FCursor.Last;
      Result := GetRecord(TDbfRecBuf(FTempBuffer), gmPrior, false) = grOK;
    end;
  finally
    FFindRecordFilter := false;
    if not Result then
    begin
      RecNo := oldRecNo;
    end else begin
      CursorPosChanged;
      Resync([]);
      DoAfterScroll;
    end;
  end;
end;

procedure TDbf.DoBeforeScroll;
begin
  FScrolling := True;
  inherited DoBeforeScroll;
end;

procedure TDbf.DoAfterScroll;
begin
  inherited DoAfterScroll;
  if FScrolling and (BufferCount = 1) and (State = dsBrowse) then
    ResyncSharedReadCurrentRecord;
  FScrolling := False;
end;

{$ifdef SUPPORT_VARIANTS}

function IsSameVariant(const Value1, Value2: Variant): Boolean;
var
  LowBound: Integer;
  HighBound: Integer;
  i: Integer;
begin
  if VarIsArray(Value1) then begin
    if not VarIsArray(Value2) then
      raise EInvalidOp.Create('Cannot compare variant array with non array variant');

    LowBound := VarArrayLowBound(Value1, 1);
    if LowBound <> VarArrayLowBound(Value2, 1) then
      raise EInvalidOp.Create('Cannot compare variant arrays with different low bounds');

    HighBound := VarArrayHighBound(Value1, 1);
    if HighBound <> VarArrayHighBound(Value2, 1) then
      raise EInvalidOp.Create('Cannot compare variant arrays with different high bounds');

    Result := True;
    for i := LowBound to HighBound do begin
      if Value1[i] = Value2[i] then begin
        Result := False;
        Exit; //==>
      end;
    end;
  end else begin
    Result := Value1 = Value2;
  end;
end;

function TDbf.Lookup(const KeyFields: string; const KeyValues: Variant;
  const ResultFields: string): Variant;
var
//  OldState:  TDataSetState;
  saveRecNo: TSequentialRecNo;
  saveState: TDataSetState;
begin
  Result := Null;
  if (FCursor = nil) or VarIsNull(KeyValues) then exit;

  saveRecNo := FCursor.SequentialRecNo;
  try
    if LocateRecord(KeyFields, KeyValues, []) then
    begin
      // FFilterBuffer contains record buffer
      saveState := SetTempState(dsCalcFields);
      try
{$ifdef SUPPORT_CALCULATEFIELDS_NATIVEINT}
        CalculateFields(NativeInt(FFilterBuffer));
{$else}
        CalculateFields(FFilterBuffer);
{$endif}
        if IsSameVariant(KeyValues, FieldValues[KeyFields]) then
          Result := FieldValues[ResultFields];
      finally
        RestoreState(saveState);
      end;
    end;
  finally
    FCursor.SequentialRecNo := saveRecNo;
  end;
end;

function TDbf.Locate(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions): Boolean;
var
  saveRecNo: TSequentialRecNo;
begin
  if FCursor = nil then
  begin
    Result := false;
    exit;
  end;

  DoBeforeScroll;
  saveRecNo := FCursor.SequentialRecNo;
  FLocateRecNo := -1;
  Result := LocateRecord(KeyFields, KeyValues, Options);
  CursorPosChanged;
  if Result then
  begin
    if FLocateRecNo <> -1 then
      FCursor.PhysicalRecNo := FLocateRecNo;
    Resync([]);
    DoAfterScroll;
  end else
    FCursor.SequentialRecNo := saveRecNo;
end;

function TDbf.LocateRecordLinear(const KeyFields: String; const KeyValues: Variant;
    Options: TLocateOptions): Boolean;
var
{$ifdef SUPPORT_GENERICS_FIELDLIST}
  lstKeys              : TList<TField>;
{$else}
  lstKeys              : TList;
{$endif}
  iIndex               : Integer;
  Field                : TField;
  bMatchedData         : Boolean;
  bVarIsArray          : Boolean;
  varCompare           : Variant;

  function CompareValues: Boolean;
  var
    sCompare: String;
  begin
    if (Field.DataType = ftString) then
    begin
      sCompare := VarToStr(varCompare);
      if loCaseInsensitive in Options then
      begin
        Result := AnsiCompareText(Field.AsString,sCompare) = 0;
        if not Result and (iIndex = lstKeys.Count - 1) and (loPartialKey in Options) and
          (Length(sCompare) < Length(Field.AsString)) then
        begin
          if Length(sCompare) = 0 then
            Result := true
          else
            Result := AnsiCompareText (Copy (Field.AsString,1,Length (sCompare)),sCompare) = 0;
        end;
      end else begin
        Result := Field.AsString = sCompare;
        if not Result and (iIndex = lstKeys.Count - 1) and (loPartialKey in Options) and
          (Length (sCompare) < Length (Field.AsString)) then
        begin
          if Length (sCompare) = 0 then
            Result := true
          else
            Result := Copy(Field.AsString, 1, Length(sCompare)) = sCompare;
        end;
      end;
    end
    else
      Result := Field.Value = varCompare;
  end;

var
  SaveState: TDataSetState;
  lPhysRecNo: integer;
begin
  Result := false;
  bVarIsArray := false;
{$ifdef SUPPORT_GENERICS_FIELDLIST}
  lstKeys := TList<TField>.Create;
{$else}
  lstKeys := TList.Create;
{$endif}
  FFilterBuffer := TDbfRecordBuffer(TempBuffer);
  SaveState := SetTempState(dsFilter);
  try
    GetFieldList(lstKeys, KeyFields);
    if VarArrayDimCount(KeyValues) = 0 then
      bMatchedData := lstKeys.Count = 1
    else if VarArrayDimCount (KeyValues) = 1 then
    begin
      bMatchedData := VarArrayHighBound (KeyValues,1) + 1 = lstKeys.Count;
      bVarIsArray := true;
    end else
      bMatchedData := false;
    if bMatchedData then
    begin
      FCursor.First;
      while not Result and FCursor.Next do
      begin
        lPhysRecNo := FCursor.PhysicalRecNo;
        if (lPhysRecNo = 0) or not FDbfFile.IsRecordPresent(lPhysRecNo) then
          break;
        
        FDbfFile.ReadRecord(lPhysRecNo, @PDbfRecord(FFilterBuffer)^.DeletedFlag);
        Result := FShowDeleted or (PDbfRecord(FFilterBuffer)^.DeletedFlag <> '*');
        if Result and Filtered then
          DoFilterRecord(Result);
        
        iIndex := 0;
        while Result and (iIndex < lstKeys.Count) Do
        begin
          Field := TField (lstKeys [iIndex]);
          if bVarIsArray then
            varCompare := KeyValues [iIndex]
          else
            varCompare := KeyValues;
          Result := CompareValues;
          Inc(iIndex);
        end;
      end;
    end;
  finally
    lstKeys.Free;
    RestoreState(SaveState);
  end;
end;

function TDbf.LocateRecordIndex(const KeyFields: String; const KeyValues: Variant;
    Options: TLocateOptions): Boolean;
var
  searchFlag: TSearchKeyType;
  matchRes: Integer;
  lTempBuffer: array [0..MaxIndexKeyLen] of AnsiChar;
  acceptable, checkmatch: boolean;
begin
  if loPartialKey in Options then
    searchFlag := stGreaterEqual
  else
    searchFlag := stEqual;
  if TIndexCursor(FCursor).VariantToBuffer(KeyValues, @lTempBuffer[0]) = etString then
    Translate(@lTempBuffer[0], @lTempBuffer[0], true);
  Result := FIndexFile.SearchKey(@lTempBuffer[0], searchFlag);
  if not Result then
    exit;

  checkmatch := false;
  repeat
    acceptable := True;
    if ReadCurrentRecord(TDbfRecordBuffer(TempBuffer), acceptable) = grError then
    begin
      Result := false;
      exit;
    end;
    if acceptable then break;
    checkmatch := true;
    FCursor.Next;
  until false;

  if checkmatch then
  begin
    matchRes := TIndexCursor(FCursor).IndexFile.MatchKey(@lTempBuffer[0]);
    if loPartialKey in Options then
      Result := matchRes <= 0
    else
      Result := matchRes =  0;
  end;

  FFilterBuffer := TDbfRecordBuffer(TempBuffer);
end;

function TDbf.LocateRecord(const KeyFields: String; const KeyValues: Variant;
    Options: TLocateOptions): Boolean;
var
  lCursor, lSaveCursor: TVirtualCursor;
  lSaveIndexName, lIndexName: string;
  lIndexDef: TDbfIndexDef;
  lIndexFile, lSaveIndexFile: TIndexFile;
begin
  if not (loCaseInsensitive in Options) then
  begin
    lCursor := nil;
    lSaveCursor := nil;
    lIndexFile := nil;
    lSaveIndexFile := FIndexFile;
    if (FCursor is TIndexCursor)
      and (TIndexCursor(FCursor).IndexFile.Expression = KeyFields) then
    begin
      lCursor := FCursor;
    end else begin
      lIndexDef := FIndexDefs.GetIndexByField(KeyFields);
      if lIndexDef <> nil then
      begin
        lIndexName := ParseIndexName(lIndexDef.IndexFile);
        lIndexFile := FDbfFile.GetIndexByName(lIndexName);
        if lIndexFile <> nil then
        begin
          lSaveCursor := FCursor;
          lCursor := TIndexCursor.Create(lIndexFile);
          lSaveIndexName := lIndexFile.IndexName;
          lIndexFile.IndexName := lIndexName;
          FIndexFile := lIndexFile;
        end;
      end;
    end;
    if (lCursor <> nil) and (not TIndexCursor(lCursor).IndexFile.IsDescending) then
    begin
      FCursor := lCursor;
      Result := LocateRecordIndex(KeyFields, KeyValues, Options);
      if lSaveCursor <> nil then
      begin
        FCursor.Free;
        FCursor := lSaveCursor;
      end;
      if lIndexFile <> nil then
      begin
        FLocateRecNo := FIndexFile.PhysicalRecNo;
        lIndexFile.IndexName := lSaveIndexName;
        FIndexFile := lSaveIndexFile;
      end;
    end
    else
      Result := LocateRecordLinear(KeyFields, KeyValues, Options);    
  end
  else
    Result := LocateRecordLinear(KeyFields, KeyValues, Options);
end;

{$endif}

procedure TDbf.TryExclusive;
begin
  // are we active?
  if Active then
  begin
    // already in exclusive mode?
    FDbfFile.TryExclusive;
    // update file mode
    FExclusive := not FDbfFile.IsSharedAccess;
    FReadOnly := FDbfFile.Mode = pfReadOnly;
  end else begin
    // just set exclusive to true
    FExclusive := true;
    FReadOnly := false;
  end;
  UpdateLock;
end;

procedure TDbf.EndExclusive;
begin
  if Active then
  begin
    // call file handler
    FDbfFile.EndExclusive;
    // update file mode
    FExclusive := not FDbfFile.IsSharedAccess;
    FReadOnly := FDbfFile.Mode = pfReadOnly;
  end else begin
    // just set exclusive to false
    FExclusive := false;
  end;
  UpdateLock;
end;

function TDbf.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; {override virtual}
var
  MemoPageNo: Integer;
  MemoFieldNo: Integer;
  lBlob: TDbfBlobStream;
begin
  // check if in editing mode if user wants to write
  if (Mode = bmWrite) or (Mode = bmReadWrite) then
    if not (State in [dsEdit, dsInsert]) then
{$ifdef DELPHI_3}
      DatabaseError(SNotEditing);
{$else}
      DatabaseError(SNotEditing, Self);
{$endif}
  // already created a `placeholder' blob for this field?
  MemoFieldNo := Field.FieldNo - 1;
  if FBlobStreams^[MemoFieldNo] = nil then
    FBlobStreams^[MemoFieldNo] := TDbfBlobStream.Create(Field);
  lBlob := FBlobStreams^[MemoFieldNo].AddReference;
  // update pageno of blob <-> location where to read/write in memofile
  if FDbfFile.GetFieldData(Field.FieldNo-1, ftInteger, GetCurrentBuffer, @MemoPageNo, false) then
  begin
    // read blob? different blob?
    if (Mode = bmRead) or (Mode = bmReadWrite) then
    begin
      if MemoPageNo <> lBlob.MemoRecNo then
      begin
        FDbfFile.MemoFile.ReadMemo(MemoPageNo, lBlob);
        lBlob.ReadSize := lBlob.Size;
        lBlob.Translate(false);
      end;
    end else begin
      lBlob.Size := 0;
      lBlob.ReadSize := 0;
    end;
    lBlob.MemoRecNo := MemoPageNo;
  end else
  if not lBlob.Dirty or (Mode = bmWrite) then
  begin
    // reading and memo is empty and not written yet, or rewriting
    lBlob.Size := 0;
    lBlob.ReadSize := 0;
    lBlob.MemoRecNo := 0;
  end;
  { this is a hack, we actually need to know per user who's modifying, and who is not }
  { Mode is more like: the mode of the last "creation" }
  { if create/free is nested, then everything will be alright, i think ;-) }
  lBlob.Mode := Mode;
  { this is a hack: we actually need to know per user what it's position is }
  lBlob.Position := 0;
  Result := lBlob;
end;

{$ifdef SUPPORT_NEW_TRANSLATE}

function TDbf.Translate(Src, Dest: PAnsiChar; ToOem: Boolean): Integer; {override virtual}
var
  FromCP, ToCP: Cardinal;
begin
  if (Src <> nil) and (Dest <> nil) then
  begin
    if Assigned(FOnTranslate) then
    begin
      Result := FOnTranslate(Self, Src, Dest, ToOem);
      if Result = -1 then
        Result := dbfStrLen(Dest);
    end else begin
      if FTranslationMode <> tmNoneNeeded then
      begin
        if ToOem then
        begin
          FromCP := GetACP;
          ToCP := FDbfFile.UseCodePage;
        end else begin
          FromCP := FDbfFile.UseCodePage;
          ToCP := GetACP;
        end;
      end else begin
        FromCP := GetACP;
        ToCP := FromCP;
      end;
      Result := TranslateString(FromCP, ToCP, Src, Dest, -1);
    end;
  end else
    Result := 0;
end;

{$else}

procedure TDbf.Translate(Src, Dest: PAnsiChar; ToOem: Boolean); {override virtual}
var
  FromCP, ToCP: Cardinal;
begin
  if (Src <> nil) and (Dest <> nil) then
  begin
    if Assigned(FOnTranslate) then
    begin
      FOnTranslate(Self, Src, Dest, ToOem);
    end else begin
      if FTranslationMode <> tmNoneNeeded then
      begin
        if ToOem then
        begin
          FromCP := GetACP;
          ToCP := FDbfFile.UseCodePage;
        end else begin
          FromCP := FDbfFile.UseCodePage;
          ToCP := GetACP;
        end;
        TranslateString(FromCP, ToCP, Src, Dest, -1);
      end;
    end;
  end;
end;

{$endif}

procedure TDbf.ClearCalcFields(Buffer: TDbfRecordBuffer);
var
  lRealBuffer, lCalcBuffer: PAnsiChar;
begin
  lRealBuffer := @pDbfRecord(Buffer)^.DeletedFlag;
  lCalcBuffer := lRealBuffer + FDbfFile.RecordSize;
  FillChar(lCalcBuffer^, CalcFieldsSize, 0);
end;

procedure TDbf.InternalSetToRecord(Buffer: TDbfRecordBuffer); {override virtual abstract from TDataset}
var
  pRecord: pDbfRecord;
begin
  if Buffer <> nil then
  begin
    pRecord := pDbfRecord(Buffer);
    if pRecord^.BookmarkFlag = bfInserted then
    begin
      // do what ???
    end else begin
      FCursor.SequentialRecNo := pRecord^.SequentialRecNo;
    end;
  end;
end;

function TDbf.IsCursorOpen: Boolean; {override virtual abstract from TDataset}
begin
  Result := FCursor <> nil;
end;

function TDbf.FieldDefsStored: Boolean;
begin
  Result := StoreDefs and (FieldDefs.Count > 0);
end;

procedure TDbf.SetBookmarkFlag(Buffer: TDbfRecordBuffer; Value: TBookmarkFlag); {override virtual abstract from TDataset}
begin
  pDbfRecord(Buffer)^.BookmarkFlag := Value;
end;

procedure TDbf.SetBookmarkData(Buffer: TDbfRecordBuffer; Data: Pointer); {override virtual abstract from TDataset}
begin
  pDbfRecord(Buffer)^.BookmarkData := pBookmarkData(Data)^;
end;

// this function counts real number of records: skip deleted records, filter, etc.
// warning: is very slow, compared to GetRecordCount
function TDbf.GetExactRecordCount: Integer;
var
  prevRecNo: TSequentialRecNo;
  getRes: TGetResult;
begin
  // init vars
  Result := 0;

  // check if FCursor open
  if FCursor = nil then
    exit; 

  // store current position
  prevRecNo := FCursor.SequentialRecNo;
  FCursor.First;
  repeat
    // repeatedly retrieve next record until eof encountered
    getRes := GetRecord(TDbfRecBuf(FTempBuffer), gmNext, true);
    if getRes = grOk then
      inc(Result);
  until getRes <> grOk;
  // restore current position
  FCursor.SequentialRecNo := prevRecNo;
end;

// this functions returns the physical number of records present in file
function TDbf.GetPhysicalRecordCount: Integer;
begin
  if FDbfFile <> nil then
    Result := FDbfFile.RecordCount
  else
    Result := 0
end;

// this function is just for the grid scrollbars
// it doesn't have to be perfectly accurate, but fast.
function TDbf.GetRecordCount: Integer; {override virtual}
begin
  if FCursor <> nil then
    Result := FCursor.SequentialRecordCount
  else
    Result := 0
end;

// this function is just for the grid scrollbars
// it doesn't have to be perfectly accurate, but fast.
function TDbf.GetRecNo: Integer; {override virtual}
var
  pBuffer: pointer;
begin
  if (FCursor <> nil) and (not IsEmpty) then
  begin
    case State of
      dsFilter:     pBuffer := TDbfRecordBuffer(FFilterBuffer);
      dsCalcFields: pBuffer := TDbfRecordBuffer(CalcBuffer);
    else
      pBuffer := TDbfRecordBuffer(ActiveBuffer);
    end;
    Result := pDbfRecord(pBuffer)^.SequentialRecNo;
  end else
    Result := 0;
end;

procedure TDbf.SetRecNo(Value: Integer); {override virtual}
begin
  CheckBrowseMode;
  if Value = RecNo then
    exit;

  DoBeforeScroll;
  FCursor.SequentialRecNo := Value;
  CursorPosChanged;
  Resync([]);
  DoAfterScroll;
end;

function TDbf.GetCanModify: Boolean; {override;}
begin
  if FReadOnly or (csDesigning in ComponentState) or (not Active) then
    Result := false
  else
    Result := FTranslationMode > tmNoneAvailable;
end;

{$ifdef SUPPORT_DEFCHANGED}

procedure TDbf.DefChanged(Sender: TObject);
begin
  StoreDefs := true;
end;

{$endif}

procedure TDbf.ParseFilter(const AFilter: string);
begin
  // parser created?
  if Length(AFilter) > 0 then
  begin
    if (FParser = nil) and (FDbfFile <> nil) then
    begin
      FParser := TDbfParser.Create(FDbfFile);
      // we need truncated, translated (to ANSI) strings
      FParser.RawStringFields := false;
    end;
    // have a parser now?
    if FParser <> nil then
    begin
      // set options
      FParser.PartialMatch := not (foNoPartialCompare in FilterOptions);
      FParser.CaseInsensitive := foCaseInsensitive in FilterOptions;
      // parse expression
      FParser.ParseExpression(AFilter);
    end;
  end;
end;

procedure TDbf.SetFilterText(const Value: String);
begin
  if Value = Filter then
    exit;

  // parse
  ParseFilter(Value);

  // call dataset method
  inherited;

  // refilter dataset if filtered
  if (FDbfFile <> nil) and Filtered then Refresh;
end;

procedure TDbf.SetFiltered(Value: Boolean); {override;}
begin
  if Value = Filtered then
    exit;

  // pass on to ancestor
  inherited;

  // only refresh if active
  if FCursor <> nil then
    Refresh;
end;

procedure TDbf.SetFilePath(const Value: string);
begin
  CheckInactive;

  FRelativePath := Value;
  if Length(FRelativePath) > 0 then
       FRelativePath := IncludeTrailingPathDelimiter(FRelativePath);

  if IsFullFilePath(Value) then
  begin
    FAbsolutePath := IncludeTrailingPathDelimiter(Value);
  end else begin
    FAbsolutePath := GetCompletePath(DbfBasePath(), FRelativePath);
  end;
end;

procedure TDbf.SetTableName(const s: string);
var
  lPath: string;
begin
  FTableName := ExtractFileName(s);
  lPath := ExtractFilePath(s);
  if (Length(lPath) > 0) then
    FilePath := lPath;
  // force IDE to reread fielddefs when a different file is opened
{$ifdef SUPPORT_FIELDDEFS_UPDATED}
  FieldDefs.Updated := false;
{$else}
  // TODO ... ??
{$endif}
end;

procedure TDbf.SetDbfIndexDefs(const Value: TDbfIndexDefs);
begin
  FIndexDefs.Assign(Value);
end;

procedure TDbf.SetLanguageID(NewID: Byte);
begin
  CheckInactive;
  
  FLanguageID := NewID;
end;

procedure TDbf.SetTableLevel(const NewLevel: Integer);
begin
  if NewLevel <> FTableLevel then
  begin
    // check validity
    if not ((NewLevel = 3) or (NewLevel = 4) or (NewLevel = 5) or (NewLevel = 7) or (NewLevel = TDBF_TABLELEVEL_FOXPRO)) then
      exit;

    // can only assign tablelevel if table is closed
    CheckInactive;
    FTableLevel := NewLevel;
  end;
end;

function TDbf.GetIndexName: string;
begin
  Result := FIndexName;
end;

function TDbf.CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer;
const
  RetCodes: array[Boolean, Boolean] of ShortInt = ((2,-1),(1,0));
var
  b1,b2: Integer;
begin
  // Check for uninitialized bookmarks
  Result := RetCodes[Bookmark1 = nil, Bookmark2 = nil];
  if (Result = 2) then
  begin
    b1 := PInteger(Bookmark1)^;
    b2 := PInteger(Bookmark2)^;
    if b1 < b2 then Result := -1
    else if b1 > b2 then Result := 1
    else Result := 0;
  end;
end;

function TDbf.GetVersion: string;
begin
  Result := DbfVersionString;
end;

procedure TDbf.SetVersion(const S: string);
begin
  // What an idea...
end;

function TDbf.ParseIndexName(const AIndexName: string): string;
begin
  // if no ext, then it is a MDX tag, get complete only if it is a filename
  // MDX: get first 10 characters only
  if Length(ExtractFileExt(AIndexName)) > 0 then
    Result := GetCompleteFileName(FAbsolutePath, AIndexName)
  else
    Result := AIndexName;
end;

procedure TDbf.RegenerateIndexes;
begin
  CheckBrowseMode;
  FDbfFile.OnProgress := FOnProgress;
  try
    FDbfFile.RegenerateIndexes;
  finally
    FDbfFile.OnProgress := nil;
  end;
end;

{$ifdef SUPPORT_DEFAULT_PARAMS}
procedure TDbf.AddIndex(const AIndexName, AFields: String; Options: TIndexOptions; const DescFields: String='');
{$else}
procedure TDbf.AddIndex(const AIndexName, AFields: String; Options: TIndexOptions);
{$endif}
var
  lIndexFileName: string;
begin
  CheckActive;
  lIndexFileName := ParseIndexName(AIndexName);
  FDbfFile.OnProgress := FOnProgress;
  try
    FDbfFile.OpenIndex(lIndexFileName, AFields, true, Options);
  finally
    FDbfFile.OnProgress := FOnProgress;
  end;

  // refresh our indexdefs
  InternalInitFieldDefs;
end;

procedure TDbf.SetIndexName(AIndexName: string);
var
  lRecNo: Integer;
begin
  if (FIndexName <> AIndexName) or (not Assigned(FCursor)) then
  begin
    FIndexName := AIndexName;
    if FDbfFile = nil then
      exit;

    // get accompanying index file
    AIndexName := ParseIndexName(Trim(AIndexName));
    FIndexFile := FDbfFile.GetIndexByName(AIndexName);
    // store current lRecNo
    if FCursor = nil then
    begin
      lRecNo := 1;
    end else begin
      UpdateCursorPos;
      lRecNo := FCursor.PhysicalRecNo;
    end;
    // select new cursor
    FreeAndNil(FCursor);
    if FIndexFile <> nil then
    begin
      FCursor := TIndexCursor.Create(FIndexFile);
      // select index
      FIndexFile.IndexName := AIndexName;
      // check if can activate master link
      CheckMasterRange;
    end else begin
      FCursor := TDbfCursor.Create(FDbfFile);
      FIndexName := EmptyStr;
    end;
    // reset previous lRecNo
    FCursor.PhysicalRecNo := lRecNo;
    // refresh records
    if State = dsBrowse then
      Resync([]);
    // warn user if selecting non-existing index
    if (FCursor = nil) and (AIndexName <> EmptyStr) then
      raise EDbfError.CreateFmt(STRING_INDEX_NOT_EXIST, [AIndexName]);
  end;
end;

function TDbf.GetIndexFieldNames: string;
var
  lIndexDef: TDbfIndexDef;
begin
  lIndexDef := FIndexDefs.GetIndexByName(IndexName);
  if lIndexDef = nil then
    Result := EmptyStr
  else
    Result := lIndexDef.SortField;
end;

procedure TDbf.SetIndexFieldNames(const Value: string);
var
  lIndexDef: TDbfIndexDef;
begin
  // Exception if index not found?
  lIndexDef := FIndexDefs.GetIndexByField(Value);
  if lIndexDef = nil then
    IndexName := EmptyStr
  else
    IndexName := lIndexDef.IndexFile;
end;

procedure TDbf.DeleteIndex(const AIndexName: string);
var
  lIndexFileName: string;
  lIndexDef: TDbfIndexDef;
begin
  // extract absolute path if NDX file
  lIndexFileName := ParseIndexName(AIndexName);
  // try to delete index
  FDbfFile.DeleteIndex(lIndexFileName);

  // remove deleted index from index defs
  lIndexDef := FIndexDefs.GetIndexByName(AIndexName);
  lIndexDef.Free;
end;

procedure TDbf.OpenIndexFile(IndexFile: string);
var
  lIndexFileName: string;
begin
  CheckActive;
  // make absolute path
  lIndexFileName := GetCompleteFileName(FAbsolutePath, IndexFile);
  // open index
  FDbfFile.OpenIndex(lIndexFileName, '', false, []);
end;

procedure TDbf.CloseIndexFile(const AIndexName: string);
var
  lIndexFileName: string;
begin
  CheckActive;
  // make absolute path
  lIndexFileName := GetCompleteFileName(FAbsolutePath, AIndexName);
  // close this index
  FDbfFile.CloseIndex(lIndexFileName);
end;

procedure TDbf.RepageIndexFile(const AIndexFile: string);
begin
  if FDbfFile <> nil then
    FDbfFile.RepageIndex(ParseIndexName(AIndexFile));
end;

procedure TDbf.CompactIndexFile(const AIndexFile: string);
begin
  if FDbfFile <> nil then
    FDbfFile.CompactIndex(ParseIndexName(AIndexFile));
end;

procedure TDbf.GetFileNames(Strings: TStrings; Files: TDbfFileNames);
var
  I: Integer;
begin
  Strings.Clear;
  if FDbfFile <> nil then
  begin
    if dfDbf in Files then
      Strings.Add(FDbfFile.FileName);
    if (dfMemo in Files) and (FDbfFile.MemoFile <> nil) then
      Strings.Add(FDbfFile.MemoFile.FileName);
    if dfIndex in Files then
      for I := 0 to Pred(FDbfFile.IndexFiles.Count) do
        Strings.Add(TPagedFile(FDbfFile.IndexFiles.Items[I]).FileName);
  end else
    Strings.Add(IncludeTrailingPathDelimiter(FilePathFull) + TableName);   
end;

{$ifdef SUPPORT_DEFAULT_PARAMS}
function TDbf.GetFileNames(Files: TDbfFileNames (* = [dfDbf] *) ): string;
{$else}
function TDbf.GetFileNamesString(Files: TDbfFileNames ): string;
{$endif}
var
  sl: TStrings;
begin
  sl := TStringList.Create;
  try
    GetFileNames(sl, Files);
    Result := sl.Text;
  finally
    sl.Free;
  end;
end;



procedure TDbf.GetIndexNames(Strings: TStrings);
begin
  CheckActive;
  Strings.Assign(DbfFile.IndexNames)
end;

procedure TDbf.GetAllIndexFiles(Strings: TStrings);
var
  SR: TSearchRec;
begin
  CheckActive;
  Strings.Clear;
  if SysUtils.FindFirst(IncludeTrailingPathDelimiter(ExtractFilePath(FDbfFile.FileName))
        + '*.NDX', faAnyFile, SR) = 0 then
  begin
    repeat
      Strings.Add(SR.Name);
    until SysUtils.FindNext(SR)<>0;
    SysUtils.FindClose(SR);
  end;
end;

function TDbf.GetPhysicalRecNo: Integer;
var
  pBuffer: pointer;
begin
  // check if active, test state: if inserting, then -1
  if (FCursor <> nil) and (State <> dsInsert) and (not IsEmpty) then
  begin
//  if State = dsCalcFields then
//    pBuffer := TDbfRecordBuffer(CalcBuffer)
//  else
//    pBuffer := TDbfRecordBuffer(ActiveBuffer);
    case State of
      dsFilter:     pBuffer := TDbfRecordBuffer(FFilterBuffer);
      dsCalcFields: pBuffer := TDbfRecordBuffer(CalcBuffer);
    else
      pBuffer := TDbfRecordBuffer(ActiveBuffer);
    end;
    Result := pDbfRecord(pBuffer)^.BookmarkData.PhysicalRecNo;
  end else
    Result := -1;
end;

procedure TDbf.SetPhysicalRecNo(const NewRecNo: Integer);
begin
  // editing?
  CheckBrowseMode;
  DoBeforeScroll;
  FCursor.PhysicalRecNo := NewRecNo;
  CursorPosChanged;
  Resync([]);
  DoAfterScroll;
end;

function TDbf.GetDbfFieldDefs: TDbfFieldDefs;
begin
  if FDbfFile <> nil then
    Result := FDbfFile.FieldDefs
  else
    Result := nil;
end;

procedure TDbf.SetShowDeleted(Value: Boolean);
begin
  // test if changed
  if Value <> FShowDeleted then
  begin
    // store new value
    FShowDeleted := Value;
    // refresh view only if active
    if FCursor <> nil then
      Refresh;
  end;
end;

// IsSequenced controls scrollbar behavior for e.g. TDBGrid. When the number
// of records is unknown (e.g. when Filtered), this turns the scrollbar into
// the somewhat awkward 3-state scrollbar rather than the scrollbar with a
// range that is too wide.
function TDbf.IsSequenced: Boolean; // lsp
begin
  Result := (not Filtered);
end;

function TDbf.IsDeleted: Boolean;
var
  src: PAnsiChar;
begin
  src := PAnsiChar(GetCurrentBuffer);
  if Assigned(src) then
    Result := (src^ = '*')
  else
    Result := False;
end;

procedure TDbf.Undelete;
var
  src: TDbfRecordBuffer;
  srcptr: PAnsiChar;
begin
  if State <> dsEdit then
    inherited Edit;
  // get active buffer
  src := GetCurrentBuffer;
  srcptr := PAnsiChar(src);
  if srcptr <> nil then
  begin
    if srcptr^ = '*' then
    begin
      // notify indexes record is about to be recalled
      FDbfFile.RecordRecalled(FCursor.PhysicalRecNo, src);
      // recall record
      srcptr^ := ' ';
      FDbfFile.WriteRecord(FCursor.PhysicalRecNo, src);
    end;
  end;
end;

procedure TDbf.CancelRange;
begin
  if FIndexFile = nil then
    exit;

  // disable current range if any
  FIndexFile.CancelRange;
  // reretrieve previous and next records
  Refresh;
end;

procedure TDbf.SetRangeBuffer(LowRange: PAnsiChar; HighRange: PAnsiChar);
begin
  if FIndexFile = nil then
    exit;

  FIndexFile.SetRange(LowRange, HighRange);
  // go to first in this range
  if Active then
    inherited First;
end;

procedure TDbf.UpdateLock;
begin
  if Assigned(DbfFile) then
  begin
    DbfFile.UpdateLock;
    DisableResyncOnPost := (not DbfFile.IsSharedAccess) or DbfFile.FileLocked;
  end;
end;

function TDbf.ResyncSharedReadCurrentRecord: Boolean;
var
  Buffer: PAnsiChar;
begin
  Buffer := nil;
  Result := FDbfFile.ResyncSharedReadBuffer;
  if Result then
  begin
    Buffer := PAnsiChar(GetCurrentBuffer);
    Result := Assigned(Buffer);
  end;
  if Result then
    Result := FDbfFile.ReadRecord(PhysicalRecNo, Buffer) <> 0;
  if Result then
    DataEvent(deRecordChange, 0);
end;

{$ifdef SUPPORT_VARIANTS}

procedure TDbf.SetRange(LowRange: Variant; HighRange: Variant; KeyIsANSI: boolean);
var
  LowBuf, HighBuf: array[0..MaxIndexKeyLen] of AnsiChar;
begin
  if (FIndexFile = nil) or VarIsNull(LowRange) or VarIsNull(HighRange) then
    exit;

  // convert variants to index key type
  if (TIndexCursor(FCursor).VariantToBuffer(LowRange,  @LowBuf[0]) = etString) and KeyIsANSI then
    Translate(@LowBuf[0], @LowBuf[0], true);
  if (TIndexCursor(FCursor).VariantToBuffer(HighRange, @HighBuf[0]) = etString) and KeyIsANSI then
    Translate(@HighBuf[0], @HighBuf[0], true);
  SetRangeBuffer(@LowBuf[0], @HighBuf[0]);
end;

{$endif}

procedure TDbf.SetRangePChar(LowRange: PAnsiChar; HighRange: PAnsiChar; KeyIsANSI: boolean);
var
  LowBuf, HighBuf: array [0..MaxIndexKeyLen] of AnsiChar;
  LowPtr, HighPtr: PAnsiChar;
begin
  if FIndexFile = nil then
    exit;

  // convert to PAnsiChars
  if KeyIsANSI then
  begin
    Translate(PAnsiChar(LowRange), @LowBuf[0], true);
    Translate(PAnsiChar(HighRange), @HighBuf[0], true);
    LowRange := @LowBuf[0];
    HighRange := @HighBuf[0];
  end;
  LowPtr  := TIndexCursor(FCursor).CheckUserKey(LowRange,  @LowBuf[0]);
  HighPtr := TIndexCursor(FCursor).CheckUserKey(HighRange, @HighBuf[0]);
  SetRangeBuffer(LowPtr, HighPtr);
end;

procedure TDbf.ExtractKey(KeyBuffer: PAnsiChar);
begin
  if FCursor is TIndexCursor then
    TIndexCursor(FCursor).IndexFile.ExtractKey(KeyBuffer)
  else
    KeyBuffer^ := #0;
end;

function TDbf.CompareKeys(Key1, Key2: PAnsiChar): Integer;
begin
  if FCursor is TIndexCursor then
    Result := TIndexCursor(FCursor).IndexFile.CompareKeys(Key1, Key2)
  else
    Result := 0;
end;

function TDbf.GetKeySize: Integer;
begin
  if FCursor is TIndexCursor then
    Result := TIndexCursor(FCursor).IndexFile.KeyLen
  else
    Result := 0;
end;

{$ifdef SUPPORT_VARIANTS}

function TDbf.SearchKey(Key: Variant; SearchType: TSearchKeyType; KeyIsANSI: boolean): Boolean;
var
  TempBuffer: array [0..MaxIndexKeyLen] of AnsiChar;
begin
  if (FIndexFile = nil) or VarIsNull(Key) then
  begin
    Result := false;
    exit;
  end;

  // FIndexFile <> nil -> FCursor as TIndexCursor <> nil
  if (TIndexCursor(FCursor).VariantToBuffer(Key, @TempBuffer[0]) = etString) and KeyIsANSI then
    Translate(@TempBuffer[0], @TempBuffer[0], true);
  Result := SearchKeyBuffer(@TempBuffer[0], SearchType);
end;

{$endif}

function  TDbf.PrepareKey(Buffer: Pointer; BufferType: TExpressionType): TDbfRecordBuffer;
begin
  if FIndexFile = nil then
  begin
    Result := nil;
    exit;
  end;
  
  Result := TIndexCursor(FCursor).IndexFile.PrepareKey(Buffer, BufferType);
end;

function TDbf.SearchKeyPChar(Key: PAnsiChar; SearchType: TSearchKeyType; KeyIsANSI: boolean): Boolean;
var
  StringBuf: array [0..MaxIndexKeyLen] of AnsiChar;
begin
  if (FCursor = nil) or not (FCursor is TIndexCursor) then
  begin
    Result := false;
    exit;
  end;

  if KeyIsANSI then
  begin
    Translate(PAnsiChar(Key), @StringBuf[0], true);
    Key := @StringBuf[0];
  end;
  Result := SearchKeyBuffer(TIndexCursor(FCursor).CheckUserKey(Key, @StringBuf[0]), SearchType);
end;

function TDbf.SearchKeyBuffer(Buffer: PAnsiChar; SearchType: TSearchKeyType): Boolean;
var
  matchRes: Integer;
begin
  if FIndexFile = nil then
  begin
    Result := false;
    exit;
  end;

  CheckBrowseMode;
  Result := FIndexFile.SearchKey(Buffer, SearchType);
  { if found, then retrieve new current record }
  if Result or (SearchType <> stEqual) then
  begin
    CursorPosChanged;
    Resync([]);
    UpdateCursorPos;
    { recno could have been changed due to deleted record, check if still matches }
    if Result then
    begin
      matchRes := TIndexCursor(FCursor).IndexFile.MatchKey(Buffer);
      case SearchType of
        stEqual:        Result := matchRes =  0;
        stGreater:      Result := (not Eof) and (matchRes <  0);
        stGreaterEqual: Result := (not Eof) and (matchRes <= 0);
      end;
    end;
  end;
end;

procedure TDbf.UpdateIndexDefs;
begin
  FieldDefs.Update;
end;

{$ifdef DELPHI_XE}
procedure TDbf.DataEvent(Event: TDataEvent; Info: NativeInt);
{$else}
procedure TDbf.DataEvent(Event: TDataEvent; Info: {$ifdef FPC_VERSION}Ptrint{$else}Longint{$endif});
{$endif}
begin
  if ((Event = deDataSetChange) or (Event = deLayoutChange)) and Assigned(FDbfFile) and (not ControlsDisabled) then
    FDbfFile.ResyncSharedFlushBuffer;
  inherited;
end;

{ Master / Detail }

procedure TDbf.CheckMasterRange;
begin
  if FMasterLink.Active and FMasterLink.ValidExpression and (FIndexFile <> nil) then
    UpdateRange;
end;

procedure TDbf.UpdateRange;
var
  fieldsVal: PAnsiChar;
  tempBuffer: array[0..300] of AnsiChar;
begin
  fieldsVal := FMasterLink.FieldsVal;
  if FMasterLink.KeyTranslation then
  begin
    FMasterLink.DataSet.Translate(fieldsVal, @tempBuffer[0], false);
    fieldsVal := @tempBuffer[0];
    Translate(fieldsVal, fieldsVal, true);
  end;
  fieldsVal := PAnsiChar(TIndexCursor(FCursor).IndexFile.PrepareKey({$IFDEF SUPPORT_TRECORDBUFFER}TDbfRecordBuffer{$ENDIF}(fieldsVal), FMasterLink.Parser.ResultType));
  SetRangeBuffer(fieldsVal, fieldsVal);
end;

procedure TDbf.MasterChanged(Sender: TObject);
begin
  CheckBrowseMode;
  CheckMasterRange;
end;

procedure TDbf.MasterDisabled(Sender: TObject);
begin
  CancelRange;
end;

function TDbf.GetDataSource: TDataSource;
begin
  Result := FMasterLink.DataSource;
end;

procedure TDbf.SetDataSource(Value: TDataSource);
begin
{$ifndef FPC}
  if IsLinkedTo(Value) then
  begin
{$ifdef DELPHI_4}
    DatabaseError(SCircularDataLink, Self);
{$else}
    DatabaseError(SCircularDataLink);
{$endif}
  end;
{$endif}
  FMasterLink.DataSource := Value;
end;

function TDbf.GetMasterFields: string;
begin
  Result := FMasterLink.FieldNames;
end;

procedure TDbf.SetMasterFields(const Value: string);
begin
  FMasterLink.FieldNames := Value;
end;

function TDbf.GetKeyBuffer: PAnsiChar;
var
  Len: Integer;
begin
  Len := SizeOf(TDbfRecordHeader) + RecordSize + CalcFieldsSize + 1;
  if (FKeyBuffer = nil) then
    GetMem(FKeyBuffer, Len)
  else
    if Len <> FKeyBufferLen then
      ReAllocMem(FKeyBuffer, Len);
  FKeyBufferLen := Len;
  Result := PAnsiChar(FKeyBuffer);
end;

function TDbf.InitKeyBuffer(Buffer: PAnsiChar): PAnsiChar;
begin
  FillChar(Buffer^, RecordSize, 0);
  InitRecord(TDbfRecBuf(Buffer));
  Result := Buffer;
end;

procedure TDbf.PostKeyBuffer(Commit: boolean);
begin
  DataEvent(deCheckBrowseMode, 0);
  SetState(dsBrowse);
  DataEvent(deDataSetChange, 0);
end;

function TDbf.GotoCommon(SearchKeyType: TSearchKeyType): Boolean;
var
  Buffer: PAnsiChar;
begin
  if Assigned(FIndexFile) and (not IsEmpty) then
  begin
    Buffer := FIndexFile.ExtractKeyFromBuffer(GetCurrentBuffer, PhysicalRecNo);
    Result := SearchKeyBuffer(Buffer, SearchKeyType);
  end
  else
  begin
    Result := False;
    CheckBrowseMode;
  end;
end;

function TDbf.GotoKey: Boolean;
begin
  Result := GotoCommon(stEqual);
end;

procedure TDbf.GotoNearest;
begin
  GotoCommon(stGreaterEqual);
end;

procedure TDbf.SetKey;
begin
  CheckBrowseMode;
  SetModified(true);
  SetState(dsSetKey);
  InitKeyBuffer(GetKeyBuffer);
  DataEvent(deDataSetChange, 0);
end;

procedure TDbf.Cancel;
begin
  inherited Cancel;
  if State = dsSetKey then
    PostKeyBuffer(False);
end;

procedure TDbf.Post;
begin
  // TDataSet.Post raises exception if State = dsSetKey in Free Pascal
  if State <> dsSetKey then
    inherited Post;
  if State = dsSetKey then
    PostKeyBuffer(True);
end;

//==========================================================
//============ TDbfIndexDefs
//==========================================================
constructor TDbfIndexDefs.Create(AOwner: TDbf);
begin
  inherited Create(TDbfIndexDef);
  FOwner := AOwner;
end;

function TDbfIndexDefs.Add: TDbfIndexDef;
begin
  Result := TDbfIndexDef(inherited Add);
end;

procedure TDbfIndexDefs.SetItem(N: Integer; Value: TDbfIndexDef);
begin
  inherited SetItem(N, Value);
end;

function TDbfIndexDefs.GetItem(N: Integer): TDbfIndexDef;
begin
  Result := TDbfIndexDef(inherited GetItem(N));
end;

function TDbfIndexDefs.GetOwner: tpersistent;
begin
  Result := FOwner;
end;

function TDbfIndexDefs.GetIndexByName(const Name: string): TDbfIndexDef;
var
  I: Integer;
  lIndex: TDbfIndexDef;
  lIndexName: string;
begin
  lIndexName := IndexNameNormalize(Name);
  for I := 0 to Count-1 do
  begin
    lIndex := Items[I];
    if lIndex.IndexFile = lIndexName then
    begin
      Result := lIndex;
      exit;
    end
  end;
  Result := nil;
end;

function TDbfIndexDefs.GetIndexByField(const Name: string): TDbfIndexDef;
var
  lIndex: TDbfIndexDef;
  searchStr: string;
  i: integer;
begin
  searchStr := AnsiUpperCase(Trim(Name));
  Result := nil;
  if searchStr = EmptyStr then
    exit;

  for I := 0 to Count-1 do
  begin
    lIndex := Items[I];
    if AnsiUpperCase(Trim(lIndex.SortField)) = searchStr then
    begin
      Result := lIndex;
      exit;
    end
  end;
end;

procedure TDbfIndexDefs.Update;
begin
  if Assigned(FOwner) then
    FOwner.UpdateIndexDefs;
end;

//==========================================================
//============ TDbfMasterLink
//==========================================================

constructor TDbfMasterLink.Create(ADataSet: TDbf);
begin
  inherited Create;

  FDetailDataSet := ADataSet;
  FParser := TDbfParser.Create(nil);
  FValidExpression := false;
end;

destructor TDbfMasterLink.Destroy;
begin
  FParser.Free;

  inherited;
end;

procedure TDbfMasterLink.ActiveChanged;
begin
  if Active and (FFieldNames <> EmptyStr) then
  begin
    FValidExpression := false;
    FParser.DbfFile := (DataSet as TDbf).DbfFile;
    FParser.ParseExpression(FFieldNames);
    FKeyTranslation := TDbfFile(FParser.DbfFile).UseCodePage <>
      FDetailDataSet.DbfFile.UseCodePage;
    FValidExpression := true;
  end else begin
    FParser.ClearExpressions;
    FValidExpression := false;
  end;

  if FDetailDataSet.Active and not (csDestroying in FDetailDataSet.ComponentState) then
    if Active then
    begin
      if Assigned(FOnMasterChange) then FOnMasterChange(Self);
    end else
      if Assigned(FOnMasterDisable) then FOnMasterDisable(Self);
end;

procedure TDbfMasterLink.CheckBrowseMode;
begin
  if FDetailDataSet.Active then
    FDetailDataSet.CheckBrowseMode;
end;

procedure TDbfMasterLink.LayoutChanged;
begin
  ActiveChanged;
end;

procedure TDbfMasterLink.RecordChanged(Field: TField);
begin
  if (DataSource.State <> dsSetKey) and FDetailDataSet.Active and Assigned(FOnMasterChange) then
    FOnMasterChange(Self);
end;

procedure TDbfMasterLink.SetFieldNames(const Value: string);
begin
  if FFieldNames <> Value then
  begin
    FFieldNames := Value;
    ActiveChanged;
  end;
end;

function TDbfMasterLink.GetFieldsVal: PAnsiChar;
begin
  Result := FParser.ExtractFromBuffer(@pDbfRecord(TDbf(DataSet).ActiveBuffer)^.DeletedFlag, TDbf(DataSet).PhysicalRecNo);
end;

////////////////////////////////////////////////////////////////////////////

function ApplicationPath: string;
begin
  Result := ExtractFilePath(ParamStr(0));
end;


////////////////////////////////////////////////////////////////////////////

initialization

  DbfBasePath := ApplicationPath;

end.

