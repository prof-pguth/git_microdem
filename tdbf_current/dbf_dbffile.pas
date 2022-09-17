unit dbf_dbffile;

interface

{$I dbf_common.inc}

uses
  Classes, SysUtils,
{$ifdef WINDOWS}
  Windows,
{$else}
{$ifdef KYLIX}
  Libc, 
{$endif}  
  Types, dbf_wtil,
{$endif}
  Db,
  dbf_common,
  dbf_cursor,
  dbf_pgfile,
  dbf_fields,
  dbf_memo,
  dbf_idxfile;

//====================================================================
//=== Dbf support (first part)
//====================================================================
//  TxBaseVersion = (xUnknown,xClipper,xBaseIII,xBaseIV,xBaseV,xFoxPro,xVisualFoxPro);
//  TPagedFileMode = (pfOpen,pfCreate);
//  TDbfGetMode = (xFirst,xPrev,xCurrent, xNext, xLast);
//  TDbfGetResult = (xOK, xBOF, xEOF, xError);

type

//====================================================================
  TDbfIndexInvalidEvent = procedure(Sender: TObject; var Handled, DeleteLink: Boolean) of object;
  TDbfIndexMissingEvent = procedure(var DeleteLink: Boolean) of object;
  TUpdateNullField = (unClear, unSet);

//====================================================================
  TDbfGlobals = class;
//====================================================================

  TDbfFile = class(TPagedFile)
  protected
    FMdxFile: TIndexFile;
    FMemoFile: TMemoFile;
    FFieldDefs: TDbfFieldDefs;
    FIndexNames: TStringList;
    FIndexFiles: TList;
    FDbfVersion: TXBaseVersion;
    FPrevBuffer: TDbfRecordBuffer;
    FDefaultBuffer: TDbfRecordBuffer;
    FRecordBufferSize: Integer;
    FLockUserLen: DWORD;
    FFileCodePage: Cardinal;
    FUseCodePage: Cardinal;
    FFileLangId: Byte;
    FCountUse: Integer;
    FCurIndex: Integer;
    FForceClose: Boolean;
    FLockField: TDbfFieldDef;
    FNullField: TDbfFieldDef;
    FAutoIncPresent: Boolean;
    FCopyDateTimeAsString: Boolean;
    FDateTimeHandling: TDateTimeHandling;
    FInCopyFrom: Boolean;
    FOnLocaleError: TDbfLocaleErrorEvent;
    FOnIndexInvalid: TDbfIndexInvalidEvent;
    FOnIndexMissing: TDbfIndexMissingEvent;
    FRecordCountDirty: Boolean;

    function  HasBlob: Boolean;
    function  GetMemoExt: string;

    function GetLanguageId: Integer;
    function GetLanguageStr: AnsiString;

    procedure RecordCountFlush;
    procedure WriteEOFTerminator;
  protected
    procedure ConstructFieldDefs;
    procedure InitDefaultBuffer;
    procedure UpdateNullField(Buffer: Pointer; AFieldDef: TDbfFieldDef; Action: TUpdateNullField);
    procedure WriteLockInfo(Buffer: TDbfRecordBuffer);

  public
    constructor Create;
    destructor Destroy; override;

    procedure Open;
    procedure Close;
    procedure Zap;
    procedure DeleteMdxFile;
    procedure UpdateLock;
    procedure BatchStart;
    procedure BatchUpdate;
    procedure BatchFinish;

    procedure FinishCreate(AFieldDefs: TDbfFieldDefs; MemoSize: Integer);
    function GetIndexByName(AIndexName: string): TIndexFile;
    procedure SetRecordSize(NewSize: Integer); override;

    procedure TryExclusive; override;
    procedure EndExclusive; override;
    procedure OpenIndex(IndexName, IndexField: string; CreateIndex: Boolean; Options: TIndexOptions);
    function  DeleteIndex(const AIndexName: string): Boolean;
    procedure CloseIndex(AIndexName: string);
    procedure RepageIndex(AIndexFile: string);
    procedure CompactIndex(AIndexFile: string);
    function  Insert(Buffer: TDbfRecordBuffer): integer;
    procedure WriteHeader; override;
    procedure ApplyAutoIncToBuffer(DestBuf: TDbfRecordBuffer);     // dBase7 support. Writeback last next-autoinc value
    procedure FastPackTable;
    procedure RestructureTable(DbfFieldDefs: TDbfFieldDefs; Pack: Boolean);
    procedure RestructureTable2(DbfFieldDefs: TDbfFieldDefs; Pack, DeleteFiles: Boolean);
    procedure Rename(DestFileName: string; NewIndexFileNames: TStrings; DeleteFiles: boolean);
    function  GetFieldInfo(const FieldName: AnsiString): TDbfFieldDef;
    function  GetFieldData(Column: Integer; DataType: TFieldType; Src,Dst: Pointer; 
      NativeFormat: boolean): Boolean;
    function  GetFieldDataFromDef(AFieldDef: TDbfFieldDef; DataType: TFieldType; 
      Src, Dst: Pointer; NativeFormat: boolean): Boolean;
    procedure SetFieldData(Column: Integer; DataType: TFieldType; Src,Dst: Pointer; NativeFormat: boolean);
    procedure InitRecord(DestBuf: PAnsiChar);
    procedure PackIndex(lIndexFile: TIndexFile; AIndexName: string; CreateIndex: Boolean);
    procedure RegenerateIndexes;
    procedure LockRecord(RecNo: Integer; Buffer: TDbfRecordBuffer; Resync: Boolean);
    procedure UnlockRecord(RecNo: Integer; Buffer: TDbfRecordBuffer);
    procedure DeleteIndexFile(AIndexFile: TIndexFile);
    procedure Flush; override;

    property MemoFile: TMemoFile read FMemoFile;
    property FieldDefs: TDbfFieldDefs read FFieldDefs;
    property IndexNames: TStringList read FIndexNames;
    property IndexFiles: TList read FIndexFiles;
    property MdxFile: TIndexFile read FMdxFile;
    property LanguageId: Integer read GetLanguageId;
    property LanguageStr: AnsiString read GetLanguageStr;
    property FileCodePage: Cardinal read FFileCodePage;
    property UseCodePage: Cardinal read FUseCodePage write FUseCodePage;
    property FileLangId: Byte read FFileLangId write FFileLangId;
    property DbfVersion: TXBaseVersion read FDbfVersion write FDbfVersion;
    property PrevBuffer: TDbfRecordBuffer read FPrevBuffer;
    property ForceClose: Boolean read FForceClose;
    property CopyDateTimeAsString: Boolean read FCopyDateTimeAsString write FCopyDateTimeAsString;
    property DateTimeHandling: TDateTimeHandling read FDateTimeHandling write FDateTimeHandling;
    property InCopyFrom: Boolean write FInCopyFrom;

    property OnIndexInvalid: TDbfIndexInvalidEvent read FOnIndexInvalid write FOnIndexInvalid;
    property OnIndexMissing: TDbfIndexMissingEvent read FOnIndexMissing write FOnIndexMissing;
    property OnLocaleError: TDbfLocaleErrorEvent read FOnLocaleError write FOnLocaleError;
  end;

//====================================================================
  TDbfCursor = class(TVirtualCursor)
  protected
    FPhysicalRecNo: Integer;
  public
    constructor Create(DbfFile: TDbfFile);
    function Next: Boolean; override;
    function Prev: Boolean; override;
    procedure First; override;
    procedure Last; override;

    function GetPhysicalRecNo: Integer; override;
    procedure SetPhysicalRecNo(RecNo: Integer); override;

    function GetSequentialRecordCount: TSequentialRecNo; override;
    function GetSequentialRecNo: TSequentialRecNo; override;
    procedure SetSequentialRecNo(RecNo: TSequentialRecNo); override;
  end;

//====================================================================
  TDbfGlobals = class
  protected
    FCodePages: TList;
    FCurrencyAsBCD: Boolean;
    FDefaultOpenCodePage: Integer;
    FDefaultCreateLangId: Byte;
    FUserName: string;
    FUserNameLen: DWORD;
	
    function  GetDefaultCreateCodePage: Integer;
    procedure SetDefaultCreateCodePage(NewCodePage: Integer);
    procedure InitUserName;
  public
    constructor Create;
    destructor Destroy; override;

    function CodePageInstalled(ACodePage: Integer): Boolean;

    property CurrencyAsBCD: Boolean read FCurrencyAsBCD write FCurrencyAsBCD;
    property DefaultOpenCodePage: Integer read FDefaultOpenCodePage write FDefaultOpenCodePage;
    property DefaultCreateCodePage: Integer read GetDefaultCreateCodePage write SetDefaultCreateCodePage;
    property DefaultCreateLangId: Byte read FDefaultCreateLangId write FDefaultCreateLangId;
    property UserName: string read FUserName;
    property UserNameLen: DWORD read FUserNameLen;
  end;

var
  DbfGlobals: TDbfGlobals;

implementation

uses
{$ifndef WINDOWS}
{$ifndef FPC}
  RTLConsts,
{$else}
  BaseUnix,
{$endif}
{$endif}
{$ifdef SUPPORT_MATH_UNIT}
  Math,
{$endif}
{$IFDEF DELPHI_XE2}
  System.Types,
{$ENDIF}
  dbf_ansistrings,
  dbf_str, dbf_lang, dbf_prssupp, dbf_prsdef;

const
  SEOFTerminator = $1A;

{$I dbf_struct.inc}

//====================================================================
// TDbfFile
//====================================================================
constructor TDbfFile.Create;
begin
  // init variables first
  FFieldDefs := TDbfFieldDefs.Create(nil);
  FIndexNames := TStringList.Create;
  FIndexFiles := TList.Create;

  // now initialize inherited
  inherited;
end;

destructor TDbfFile.Destroy;
var
  I: Integer;
begin
  // close file
  Close;

  // free files
  for I := 0 to Pred(FIndexFiles.Count) do
    TPagedFile(FIndexFiles.Items[I]).Free;

  // free lists
  FreeAndNil(FIndexFiles);
  FreeAndNil(FIndexNames);
  FreeAndNil(FFieldDefs);

  // call ancestor
  inherited;
end;

procedure TDbfFile.Open;
var
  lMemoFileName: string;
  lMdxFileName: string;
  MemoFileClass: TMemoFileClass;
  I: Integer;
  deleteLink: Boolean;
  lModified: boolean;
  LangStr: PAnsiChar;
  version: byte;
  Handled: Boolean;
  EOFTerminator: Byte;
  lOffset: TPagedFileOffset;
begin
  // check if not already opened
  if not Active then
  begin
    // open requested file
    OpenFile;

    // check if we opened an already existing file
    lModified := false;
    if not FileCreated then
    begin
      HeaderSize := sizeof(rDbfHdr); // temporary
      // OH 2000-11-15 dBase7 support. I build dBase Tables with different
      // BDE dBase Level (1. without Memo, 2. with Memo)
      //                          Header Byte ($1d hex) (29 dec) -> Language driver ID.
      //  $03,$83 xBaseIII        Header Byte $1d=$00, Float -> N($13.$04) DateTime C($1E)
      //  $03,$8B xBaseIV/V       Header Byte $1d=$58, Float -> N($14.$04)
      //  $04,$8C xBaseVII        Header Byte $1d=$00  Float -> O($08)     DateTime @($08)
      //  $03,$F5 FoxPro Level 25 Header Byte $1d=$03, Float -> N($14.$04)
      // Access 97
      //  $03,$83 dBaseIII        Header Byte $1d=$00, Float -> N($13.$05) DateTime D($08)
      //  $03,$8B dBaseIV/V       Header Byte $1d=$00, Float -> N($14.$05) DateTime D($08)
      //  $03,$F5 FoxPro Level 25 Header Byte $1d=$09, Float -> N($14.$05) DateTime D($08)

      version := PDbfHdr(Header)^.VerDBF;
      case (version and $07) of
        $03:
          if LanguageID = 0 then
            FDbfVersion := xBaseIII
          else
            FDbfVersion := xBaseV;
        $04:
          FDbfVersion := xBaseVII;
        $02, $05:
          FDbfVersion := xFoxPro;
      else
        // check visual foxpro
        if ((version and $FE) = $30) or (version = $F5) or (version = $FB) then
        begin
          FDbfVersion := xFoxPro;
        end else begin
          // not a valid DBF file
          raise EDbfError.Create(STRING_INVALID_DBF_FILE);
        end;
      end;
      FFieldDefs.DbfVersion := FDbfVersion;
      RecordSize := PDbfHdr(Header)^.RecordSize;
      HeaderSize := PDbfHdr(Header)^.FullHdrSize;
      if (HeaderSize = 0) or (RecordSize = 0) then
      begin
        HeaderSize := 0;
        RecordSize := 0;
        RecordCount := 0;
        FForceClose := true;
        exit;
      end;
      // determine codepage
      if FDbfVersion >= xBaseVII then
      begin
        // cache language str
        LangStr := @PAfterHdrVII(PAnsiChar(Header) + SizeOf(rDbfHdr))^.LanguageDriverName;
        // VdBase 7 Language strings
        //  'DBWIN...' -> Charset 1252 (ansi)
        //  'DB999...' -> Code page 999, 9 any digit
        //  'DBHEBREW' -> Code page 1255 ??
        //  'FOX..999' -> Code page 999, 9 any digit
        //  'FOX..WIN' -> Charset 1252 (ansi)
        if (LangStr[0] = 'D') and (LangStr[1] = 'B') then
        begin
          if dbfStrLComp(LangStr+2, 'WIN', 3) = 0 then
            FFileCodePage := 1252
          else
          if dbfStrLComp(LangStr+2, 'HEBREW', 6) = 0 then
          begin
            FFileCodePage := 1255;
          end else begin
            StrToInt32Width(Integer(FFileCodePage), LangStr+2, 3, 0);
            if (Ord(LangStr[5]) >= Ord('0')) and (Ord(LangStr[5]) <= Ord('9')) then
              FFileCodePage := FFileCodePage * 10 + Ord(LangStr[5]) {%H-}- Ord('0');
          end;
        end else
        if dbfStrLComp(LangStr, 'FOX', 3) = 0 then
        begin
          if dbfStrLComp(LangStr+5, 'WIN', 3) = 0 then
            FFileCodePage := 1252
          else
            StrToInt32Width(Integer(FFileCodePage), LangStr+5, 3, 0);
        end else begin
          FFileCodePage := 0;
        end;
        FFileLangId := GetLangId_From_LangName(LanguageStr);
      end else begin
        // FDbfVersion <= xBaseV
        FFileLangId := PDbfHdr(Header)^.Language;
        FFileCodePage := LangId_To_CodePage[FFileLangId];
      end;
      // determine used codepage, if no codepage, then use default codepage
      FUseCodePage := FFileCodePage;
      if FUseCodePage = 0 then
        FUseCodePage := DbfGlobals.DefaultOpenCodePage;
      // get list of fields
      ConstructFieldDefs;
      // check if specified recordcount correct
      if PDbfHdr(Header)^.RecordCount <> RecordCount then
      begin
        PDbfHdr(Header)^.RecordCount := RecordCount;
        lModified := true;
      end;
      // open blob file if present
      lMemoFileName := ChangeFileExt(FileName, GetMemoExt);
      if HasBlob then
      begin
        // open blob file
        if not FileExists(lMemoFileName) then
          MemoFileClass := TNullMemoFile
        else if FDbfVersion = xFoxPro then
          MemoFileClass := TFoxProMemoFile
        else
          MemoFileClass := TDbaseMemoFile;
        FMemoFile := MemoFileClass.Create(Self);
        FMemoFile.FileName := lMemoFileName;
        FMemoFile.Mode := Mode;
        FMemoFile.AutoCreate := false;
        FMemoFile.MemoRecordSize := 0;
        FMemoFile.DbfVersion := FDbfVersion;
        FMemoFile.Open;
        // set header blob flag corresponding to field list
        if FDbfVersion <> xFoxPro then
        begin
          version := PDbfHdr(Header)^.VerDBF or $80;
          if version <> PDbfHdr(Header)^.VerDBF then begin
            PDbfHdr(Header)^.VerDBF := version;
            lModified := true;
          end;
        end;
      end else
        if FDbfVersion <> xFoxPro then
        begin
          version := PDbfHdr(Header)^.VerDBF and $7F;
          if version <> PDbfHdr(Header)^.VerDBF then begin
            PDbfHdr(Header)^.VerDBF := version;
            lModified := true;
          end;
        end;
      // check if mdx flagged
      if PDbfHdr(Header)^.MDXFlag <> 0 then
      begin
        // open mdx file if present
        lMdxFileName := ChangeFileExt(FileName, '.mdx');
        if FileExists(lMdxFileName) then
        begin
          // open file
          FMdxFile := TIndexFile.Create(Self);
          FMdxFile.FileName := lMdxFileName;
          FMdxFile.Mode := Mode;
          FMdxFile.AutoCreate := false;
          FMdxFile.OnLocaleError := FOnLocaleError;
          FMdxFile.CodePage := UseCodePage;
          try
            FMdxFile.Open;
          except
            on E: Exception do
            begin
              if (E is EDbfIndexError) or (E is EParserException) then
              begin
                FMdxFile.Close;
                Handled := False;
                deleteLink := False;
                if Assigned(FOnIndexInvalid) then
                  FOnIndexInvalid(Self, Handled, deleteLink);
                if not Handled then
                  raise;
                if deleteLink then
                begin
                  PDbfHdr(Header)^.MDXFlag := 0;
                  lModified := true;
                end;
              end
              else
                raise;
            end;
          else
            raise;
          end;
          // is index ready for use?
          if not FMdxFile.ForceClose then
          begin
            FIndexFiles.Add(FMdxFile);
            // get index tag names known
            FMdxFile.GetIndexNames(FIndexNames);
          end else begin
            // asked to close! close file
            FreeAndNil(FMdxFile);
          end;
        end else begin
          if FDbfVersion <> xFoxPro then
          begin
            // ask user
            deleteLink := true;
            if Assigned(FOnIndexMissing) then
              FOnIndexMissing(deleteLink);
            // correct flag
            if deleteLink then
            begin
              PDbfHdr(Header)^.MDXFlag := 0;
              lModified := true;
            end else
              FForceClose := true;
          end;
        end;
      end;
    end;

    // record changes
    if lModified and (Mode <> pfReadOnly) then
      WriteHeader;

    lOffset:= CalcPageOffset(Succ(RecordCount));
    if Succ(lOffset)=FCachedSize then
      if (ReadBlock(@EOFTerminator, SizeOf(EOFTerminator), lOffset)=SizeOf(EOFTerminator)) and (EOFTerminator=SEOFTerminator) then
        Dec(FCachedSize);

    // open indexes
    for I := 0 to FIndexFiles.Count - 1 do
      TIndexFile(FIndexFiles.Items[I]).Open;
  end;
end;

procedure TDbfFile.Close;
var
  MdxIndex, I: Integer;
begin
  if Active then
  begin
    // close index files first
    MdxIndex := -1;
    for I := 0 to FIndexFiles.Count - 1 do
    begin
      TIndexFile(FIndexFiles.Items[I]).Close;
      if TIndexFile(FIndexFiles.Items[I]) = FMdxFile then
        MdxIndex := I;
    end;
    // free memo file if any
    FreeAndNil(FMemoFile);

    // now we can close physical dbf file
    RecordCountFlush;
    CloseFile;

    // free FMdxFile, remove it from the FIndexFiles and Names lists
    if MdxIndex >= 0 then
      FIndexFiles.Delete(MdxIndex);
    I := 0;
    while I < FIndexNames.Count do
    begin
      if FIndexNames.Objects[I] = FMdxFile then
      begin
        FIndexNames.Delete(I);
      end else begin
        Inc(I);
      end;
    end;
    FreeAndNil(FMdxFile);
    FreeMemAndNil(Pointer(FPrevBuffer));
    FreeMemAndNil(Pointer(FDefaultBuffer));

    // reset variables
    FFileLangId := 0;
  end;
end;

procedure TDbfFile.FinishCreate(AFieldDefs: TDbfFieldDefs; MemoSize: Integer);
var
  lFieldDescIII: rFieldDescIII;
  lFieldDescVII: rFieldDescVII;
  lFieldDescPtr: Pointer;
  lFieldDef: TDbfFieldDef;
  lMemoFileName: string;
  I, lFieldOffset, lSize, lPrec: Integer;
  lHasBlob: Boolean;
  lLocaleID: LCID;

begin
  try
    // first reset file
    RecordCount := 0;
    lHasBlob := false;
    // determine codepage & locale
    if FFileLangId = 0 then
      FFileLangId := DbfGlobals.DefaultCreateLangId;
    FFileCodePage := LangId_To_CodePage[FFileLangId];
    lLocaleID := LangId_To_Locale[FFileLangId];
    FUseCodePage := FFileCodePage;
    // prepare header size
    if FDbfVersion = xBaseVII then
    begin
      // version xBaseVII without memo
      HeaderSize := SizeOf(rDbfHdr) + SizeOf(rAfterHdrVII);
      RecordSize := SizeOf(rFieldDescVII);
      FillChar(Header^, HeaderSize, #0);
      PDbfHdr(Header)^.VerDBF := $04;
      // write language string
      dbfStrPLCopy(
        @PAfterHdrVII(PAnsiChar(Header)+SizeOf(rDbfHdr))^.LanguageDriverName[32], // Was PChar!!!
        ConstructLangName(FFileCodePage, lLocaleID, false),
        63-32);
      lFieldDescPtr := @lFieldDescVII;
    end else begin
      // version xBaseIII/IV/V without memo
      HeaderSize := SizeOf(rDbfHdr) + SizeOf(rAfterHdrIII);
      RecordSize := SizeOf(rFieldDescIII);
      FillChar(Header^, HeaderSize, #0);
      if FDbfVersion = xFoxPro then
      begin
        PDbfHdr(Header)^.VerDBF := $30
      end else
        PDbfHdr(Header)^.VerDBF := $03;
      // standard language WE, dBase III no language support
      if FDbfVersion = xBaseIII then
        PDbfHdr(Header)^.Language := 0
      else
        PDbfHdr(Header)^.Language := FFileLangId;
      // init field ptr
      lFieldDescPtr := @lFieldDescIII;
    end;
    // begin writing fields
    FFieldDefs.Clear;
    // deleted mark 1 byte
    lFieldOffset := 1;
    for I := 1 to AFieldDefs.Count do
    begin
      lFieldDef := AFieldDefs.Items[I-1];

      // check if datetime conversion
      if FCopyDateTimeAsString then
        if lFieldDef.FieldType = ftDateTime then
        begin
          // convert to string
          lFieldDef.FieldType := ftString;
          lFieldDef.Size := 22;
        end;

      // update source
      lFieldDef.FieldName := AnsiString(AnsiUpperCase(string(lFieldDef.FieldName)));
      lFieldDef.Offset := lFieldOffset;
      lHasBlob := lHasBlob or lFieldDef.IsBlob;

      // apply field transformation tricks
      lSize := lFieldDef.Size;
      lPrec := lFieldDef.Precision;
      if (lFieldDef.NativeFieldType = 'C')
{$ifndef USE_LONG_CHAR_FIELDS}
          and (FDbfVersion = xFoxPro)
{$endif}
                then
      begin
        lPrec := lSize shr 8;
        lSize := lSize and $FF;
      end;

      // update temp field props
      if FDbfVersion = xBaseVII then
      begin
        FillChar(lFieldDescVII, SizeOf(lFieldDescVII), #0);
        dbfStrPLCopy(lFieldDescVII.FieldName, lFieldDef.FieldName, SizeOf(lFieldDescVII.FieldName)-1);
        lFieldDescVII.FieldType := lFieldDef.NativeFieldType;
        lFieldDescVII.FieldSize := lSize;
        lFieldDescVII.FieldPrecision := lPrec;
        lFieldDescVII.NextAutoInc := SwapIntLE(lFieldDef.AutoInc);
        //lFieldDescVII.MDXFlag := ???
      end else begin
        FillChar(lFieldDescIII, SizeOf(lFieldDescIII), #0);
        dbfStrPLCopy(lFieldDescIII.FieldName, lFieldDef.FieldName, SizeOf(lFieldDescIII.FieldName)-1);
        lFieldDescIII.FieldType := lFieldDef.NativeFieldType;
        lFieldDescIII.FieldSize := lSize;
        lFieldDescIII.FieldPrecision := lPrec;
        if FDbfVersion = xFoxPro then
          lFieldDescIII.FieldOffset := SwapIntLE(lFieldOffset);
        if (PDbfHdr(Header)^.VerDBF = $02) and CharInSet(lFieldDef.NativeFieldType, ['0', 'Y', 'T', 'O', '+']) then
          PDbfHdr(Header)^.VerDBF := $30;
        if (PDbfHdr(Header)^.VerDBF = $20) and CharInSet(lFieldDef.NativeFieldType, ['+']) then
          PDbfHdr(Header)^.VerDBF := $31;
      end;

      // update our field list
      with FFieldDefs.AddFieldDef do
      begin
        Assign(lFieldDef);
        Offset := lFieldOffset;
      end;

      // save field props
      WriteRecord(I, lFieldDescPtr);
      Inc(lFieldOffset, lFieldDef.Size);
    end;
    // end of header
    WriteChar($0D);
    Inc(FCachedSize);

    // write memo bit
    if lHasBlob then
    begin
      if FDbfVersion = xBaseIII then
        PDbfHdr(Header)^.VerDBF := PDbfHdr(Header)^.VerDBF or $80
      else
      if FDbfVersion = xFoxPro then
      begin
        if PDbfHdr(Header)^.VerDBF = $02 then
          PDbfHdr(Header)^.VerDBF := $F5;
      end else
        PDbfHdr(Header)^.VerDBF := PDbfHdr(Header)^.VerDBF or $88;
    end;

    // update header
    PDbfHdr(Header)^.RecordSize := lFieldOffset;
    PDbfHdr(Header)^.FullHdrSize := HeaderSize + RecordSize * AFieldDefs.Count + 1;
    // add empty "back-link" info, whatever it is: 
    { A 263-byte range that contains the backlink, which is the relative path of 
      an associated database (.dbc) file, information. If the first byte is 0x00, 
      the file is not associated with a database. Therefore, database files always
      contain 0x00. }
    if FDbfVersion = xFoxPro then
    begin
      Inc(PDbfHdr(Header)^.FullHdrSize, 263);
      Inc(FCachedSize, 263);
    end;

    // write dbf header to disk
    inherited WriteHeader;
  finally
    RecordSize := PDbfHdr(Header)^.RecordSize;
    HeaderSize := PDbfHdr(Header)^.FullHdrSize;

    // write full header to disk (dbf+fields)
    WriteHeader;
  end;

  if HasBlob and (FMemoFile=nil) then
  begin
    lMemoFileName := ChangeFileExt(FileName, GetMemoExt);
    if FDbfVersion = xFoxPro then
      FMemoFile := TFoxProMemoFile.Create(Self)
    else
      FMemoFile := TDbaseMemoFile.Create(Self);
    FMemoFile.FileName := lMemoFileName;
    FMemoFile.Mode := Mode;
    FMemoFile.AutoCreate := AutoCreate;
    FMemoFile.MemoRecordSize := MemoSize;
    FMemoFile.DbfVersion := FDbfVersion;
    FMemoFile.Open;
  end;
end;

function TDbfFile.HasBlob: Boolean;
var
  I: Integer;
begin
  Result := false;
  for I := 0 to FFieldDefs.Count-1 do
    if FFieldDefs.Items[I].IsBlob then 
      Result := true;
end;

function TDbfFile.GetMemoExt: string;
begin
  if FDbfVersion = xFoxPro then
    Result := '.fpt'
  else
    Result := '.dbt';
end;

procedure TDbfFile.Zap;
begin
  FlushBuffer;
  UpdateBufferSize;
  // make recordcount zero
  RecordCount := 0;
  // update recordcount
  PDbfHdr(Header)^.RecordCount := RecordCount;
  // update disk header
  WriteHeader;
  FlushHeader;
  // update indexes
  RegenerateIndexes;
end;

procedure TDbfFile.DeleteMdxFile;
begin
  PDbfHdr(Header)^.MDXFlag := 0;
  WriteHeader;
  DeleteIndexFile(MdxFile);
end;

procedure TDbfFile.BatchStart;
begin
  FInCopyFrom := True;
end;

procedure TDbfFile.BatchUpdate;
begin
  if not NeedLocks then
    RecordCountFlush;
end;

procedure TDbfFile.BatchFinish;
begin
  FInCopyFrom := False;
end;

procedure TDbfFile.UpdateLock;
begin
{$ifdef USE_CACHE}
  BufferAhead := True;
{$else}
  BufferAhead := (not IsSharedAccess) or FFileLocked;
{$endif}
end;

procedure TDbfFile.WriteHeader;
var
  SystemTime: TSystemTime;
  lDataHdr: PDbfHdr;
begin
  if (HeaderSize=0) then
    exit;

  lDataHdr := PDbfHdr(Header);
  GetLocalTime(SystemTime{%H-});
  lDataHdr^.Year := SystemTime.wYear - 1900;
  lDataHdr^.Month := SystemTime.wMonth;
  lDataHdr^.Day := SystemTime.wDay;
  inherited WriteHeader;
end;

procedure TDbfFile.ConstructFieldDefs;
var
  lHeaderSize,lFieldSize: Integer;
  lPropHdrOffset, lFieldOffset: Integer;
  lFieldDescIII: rFieldDescIII;
  lFieldDescVII: rFieldDescVII;
  lFieldPropsHdr: rFieldPropsHdr;
  lStdProp: rStdPropEntry;
  TempFieldDef: TDbfFieldDef;
  lSize,lPrec,I, lColumnCount: Integer;
  lAutoInc: Cardinal;
  dataPtr: PAnsiChar;
  lNativeFieldType: AnsiChar;
  lFieldName: AnsiString;
  lCanHoldNull: boolean;
  lCurrentNullPosition: integer;
begin
  FFieldDefs.Clear;
  if DbfVersion >= xBaseVII then
  begin
    lHeaderSize := SizeOf(rAfterHdrVII) + SizeOf(rDbfHdr);
    lFieldSize := SizeOf(rFieldDescVII);
  end else begin
    lHeaderSize := SizeOf(rAfterHdrIII) + SizeOf(rDbfHdr);
    lFieldSize := SizeOf(rFieldDescIII);
  end;
  HeaderSize := lHeaderSize;
  RecordSize := lFieldSize;

  FLockField := nil;
  FNullField := nil;
  FAutoIncPresent := false;
  lColumnCount := (PDbfHdr(Header)^.FullHdrSize - lHeaderSize) div lFieldSize;
  lFieldOffset := 1;
  lAutoInc := 0;
  I := 1;
  lCurrentNullPosition := 0;
  lCanHoldNull := false;
  try
    // there has to be minimum of one field
    repeat
      // version field info?
      if FDbfVersion >= xBaseVII then
      begin
        ReadRecord(I, @lFieldDescVII);
        lFieldName := dbfStrUpper(PAnsiChar(@lFieldDescVII.FieldName[0]));
        lSize := lFieldDescVII.FieldSize;
        lPrec := lFieldDescVII.FieldPrecision;
        lNativeFieldType := lFieldDescVII.FieldType;
        lAutoInc := SwapIntLE(lFieldDescVII.NextAutoInc);
        if lNativeFieldType = '+' then
          FAutoIncPresent := true;
      end else begin
        ReadRecord(I, @lFieldDescIII);
        lFieldName := dbfStrUpper(PAnsiChar(@lFieldDescIII.FieldName[0]));
        lSize := lFieldDescIII.FieldSize;
        lPrec := lFieldDescIII.FieldPrecision;
        lNativeFieldType := lFieldDescIII.FieldType;
        lCanHoldNull := (FDbfVersion = xFoxPro) and 
          ((lFieldDescIII.FoxProFlags and $2) <> 0) and
          (lFieldName <> '_NULLFLAGS');
      end;

      // apply field transformation tricks
      if (lNativeFieldType = 'C') 
{$ifndef USE_LONG_CHAR_FIELDS}
          and (FDbfVersion = xFoxPro) 
{$endif}
                then
      begin
        lSize := lSize + lPrec shl 8;
        lPrec := 0;
      end;

      // add field
      TempFieldDef := FFieldDefs.AddFieldDef;
      with TempFieldDef do
      begin
        FieldName := lFieldName;
        Offset := lFieldOffset;
        Size := lSize;
        Precision := lPrec;
        AutoInc := lAutoInc;
        NativeFieldType := lNativeFieldType;
        if lCanHoldNull then
        begin
          NullPosition := lCurrentNullPosition;
          inc(lCurrentNullPosition);
        end else
          NullPosition := -1;
      end;

      // check valid field:
      //  1) non-empty field name
      //  2) known field type
      //  {3) no changes have to be made to precision or size}
      if (Length(lFieldName) = 0) or (TempFieldDef.FieldType = ftUnknown) then
        raise EDbfError.Create(STRING_INVALID_DBF_FILE);

      // determine if lock field present, if present, then store additional info
      if lFieldName = '_DBASELOCK' then
      begin
        FLockField := TempFieldDef;
        FLockUserLen := lSize - 8;
        if FLockUserLen > DbfGlobals.UserNameLen then
          FLockUserLen := DbfGlobals.UserNameLen;
      end else
      if dbfStrUpper(PAnsiChar(lFieldName)) = '_NULLFLAGS' then
        FNullField := TempFieldDef;

      // goto next field
      Inc(lFieldOffset, lSize);
      Inc(I);

      // continue until header termination character found
      // or end of header reached
    until (I > lColumnCount) or (ReadChar = $0D);

    // test if not too many fields
    if FFieldDefs.Count >= 4096 then
      raise EDbfError.CreateFmt(STRING_INVALID_FIELD_COUNT, [FFieldDefs.Count]);

    // do not check FieldOffset = PDbfHdr(Header).RecordSize because additional 
    // data could be present in record

    // get current position
    lPropHdrOffset := Stream.Position;

    // dBase 7 -> read field properties, test if enough space, maybe no header
    if (FDbfVersion = xBaseVII) and (lPropHdrOffset + Sizeof(lFieldPropsHdr) <
            PDbfHdr(Header)^.FullHdrSize) then
    begin
      // read in field properties header
      ReadBlock(@lFieldPropsHdr, SizeOf(lFieldPropsHdr), lPropHdrOffset);
      // read in standard properties
      lFieldOffset := lPropHdrOffset + lFieldPropsHdr.StartStdProps;
      for I := 0 to lFieldPropsHdr.NumStdProps - 1 do
      begin
        // read property data
        ReadBlock(@lStdProp, SizeOf(lStdProp), lFieldOffset+I*SizeOf(lStdProp)){%H-};
        // is this a constraint?
        if lStdProp.FieldOffset = 0 then
        begin
          // this is a constraint...not implemented
        end else if lStdProp.FieldOffset <= FFieldDefs.Count then begin
          // get fielddef for this property
          TempFieldDef := FFieldDefs.Items[lStdProp.FieldOffset-1];
          // allocate space to store data
          TempFieldDef.AllocBuffers;
          // dataPtr = nil -> no data to retrieve
          dataPtr := nil;
          // store data
          case lStdProp.PropType of
            FieldPropType_Required: TempFieldDef.Required := true;
            FieldPropType_Default:
              begin
                dataPtr := TempFieldDef.DefaultBuf;
                TempFieldDef.HasDefault := true;
              end;
            FieldPropType_Min:
              begin
                dataPtr := TempFieldDef.MinBuf;
                TempFieldDef.HasMin := true;
              end;
            FieldPropType_Max:
              begin
                dataPtr := TempFieldDef.MaxBuf;
                TempFieldDef.HasMax := true;
              end;
          end;
          // get data for this property
          if dataPtr <> nil then
            ReadBlock(dataPtr, lStdProp.DataSize, lPropHdrOffset + lStdProp.DataOffset){%H-};
        end;
      end;
      // read custom properties...not implemented
      // read RI properties...not implemented
    end;
  finally
    HeaderSize := PDbfHdr(Header)^.FullHdrSize;
    RecordSize := PDbfHdr(Header)^.RecordSize;
  end;
end;

function TDbfFile.GetLanguageId: Integer;
begin
  Result := PDbfHdr(Header)^.Language;
end;

function TDbfFile.GetLanguageStr: AnsiString;
begin
  if FDbfVersion >= xBaseVII then
    Result := PAfterHdrVII(PAnsiChar(Header) + SizeOf(rDbfHdr))^.LanguageDriverName; // Was PChar
end;

procedure TDbfFile.RecordCountFlush;
begin
  if FRecordCountDirty then
  begin
    WriteEOFTerminator;
    PDbfHdr(Header)^.RecordCount := RecordCount;
    WriteHeader;
    FRecordCountDirty := False;
  end;
end;

procedure TDbfFile.WriteEOFTerminator;
var
  EOFTerminator: Byte;
begin
  EOFTerminator := SEOFTerminator;
  WriteBlock(@EOFTerminator, SizeOf(EOFTerminator), CalcPageOffset(RecordCount + 1));
end;

{
  I fill the holes with the last records.
  now we can do an 'in-place' pack
}
procedure TDbfFile.FastPackTable;
var
  iDel,iNormal: Integer;
  pDel,pNormal: PAnsiChar;

  function FindFirstDel: Boolean;
  begin
    while iDel<=iNormal do
    begin
      ReadRecord(iDel, pDel);
      if (PAnsiChar(pDel)^ <> ' ') then // was PChar
      begin
        Result := true;
        exit;
      end;
      Inc(iDel);
    end;
    Result := false;
  end;

  function FindLastNormal: Boolean;
  begin
    while iNormal>=iDel do
    begin
      ReadRecord(iNormal, pNormal);
      if (PAnsiChar(pNormal)^= ' ') then // was PChar
      begin
        Result := true;
        exit;
      end;
      dec(iNormal);
    end;
    Result := false;
  end;

begin
  if RecordSize < 1 then Exit;

  GetMem(pNormal, RecordSize);
  GetMem(pDel, RecordSize);
  try
    iDel := 1;
    iNormal := RecordCount;

    while FindFirstDel do
    begin
      // iDel is definitely deleted
      if FindLastNormal then
      begin
        // but is not anymore
        WriteRecord(iDel, pNormal);
        PAnsiChar(pNormal)^ := '*';
        WriteRecord(iNormal, pNormal);
      end else begin
        // Cannot found a record after iDel so iDel must be deleted
        dec(iDel);
        break;
      end;
    end;
    // FindFirstDel failed means than iDel is full
    RecordCount := iDel;
    RegenerateIndexes;
    // Pack Memofields
  finally
    FreeMem(pNormal);
    FreeMem(pDel);
  end;
end;

procedure TDbfFile.Rename(DestFileName: string; NewIndexFileNames: TStrings; DeleteFiles: boolean);
var
  lIndexFileNames: TStrings;
  lIndexFile: TIndexFile;
  NewBaseName: string;
  I: integer;
begin
  // get memory for index file list
  lIndexFileNames := TStringList.Create;
  try 
    // save index filenames
    for I := 0 to FIndexFiles.Count - 1 do
    begin
      lIndexFile := TIndexFile(IndexFiles[I]);
      lIndexFileNames.Add(lIndexFile.FileName);
      // prepare changing the dbf file name, needs changes in index files
      lIndexFile.PrepareRename(NewIndexFileNames[I]);
    end;

    // close file
    Close;

    if DeleteFiles then
    begin
      SysUtils.DeleteFile(DestFileName);
      SysUtils.DeleteFile(ChangeFileExt(DestFileName, GetMemoExt));
    end else begin
      NewBaseName:= '';
      I := 0;
      FindNextName(DestFileName, NewBaseName, I);
      SysUtils.RenameFile(DestFileName, NewBaseName);
      SysUtils.RenameFile(ChangeFileExt(DestFileName, GetMemoExt), 
        ChangeFileExt(NewBaseName, GetMemoExt));
    end;
    // delete old index files
    for I := 0 to NewIndexFileNames.Count - 1 do
      SysUtils.DeleteFile(NewIndexFileNames.Strings[I]);
    // rename the new dbf files
    SysUtils.RenameFile(FileName, DestFileName);
    SysUtils.RenameFile(ChangeFileExt(FileName, GetMemoExt), 
      ChangeFileExt(DestFileName, GetMemoExt));
    // rename new index files
    for I := 0 to NewIndexFileNames.Count - 1 do
      SysUtils.RenameFile(lIndexFileNames.Strings[I], NewIndexFileNames.Strings[I]);
  finally
    lIndexFileNames.Free;
  end;  
end;

type
  TRestructFieldInfo = record
    SourceFieldDef: TDbfFieldDef;
    SourceOffset: Integer;
    DestOffset: Integer;
    Size: Integer;
  end;

  { assume nobody has more than 8192 fields, otherwise possibly range check error }
  PRestructFieldInfo = ^TRestructFieldInfoArray;
  TRestructFieldInfoArray = array[0..8191] of TRestructFieldInfo;

procedure TDbfFile.RestructureTable(DbfFieldDefs: TDbfFieldDefs; Pack: Boolean);
begin
  // if restructure -> rename the old dbf files
  // if pack only -> delete the old dbf files
  RestructureTable2(DbfFieldDefs, Pack, DbfFieldDefs = nil);
end;

procedure TDbfFile.RestructureTable2(DbfFieldDefs: TDbfFieldDefs; Pack, DeleteFiles: Boolean);
var
  DestDbfFile: TDbfFile;
  TempIndexDef: TDbfIndexDef;
  TempIndexFile: TIndexFile;
  DestFieldDefs: TDbfFieldDefs;
  TempDstDef, TempSrcDef: TDbfFieldDef;
  OldIndexFiles: TStrings;
  IndexName, NewBaseName: string;
  I, lRecNo, lFieldNo, lFieldSize, lBlobPageNo, lWRecNo, srcOffset, dstOffset: Integer;
  pBuff, pDestBuff: TDbfRecordBuffer;
  RestructFieldInfo: PRestructFieldInfo;
  BlobStream: TMemoryStream;
  last: Integer;
  Buffer: array[0..7] of Char;
  DoubleValue: Double absolute Buffer;
  IntValue: Integer absolute Buffer;
  Int64Value: {$ifdef SUPPORT_INT64}Int64{$else}Integer{$endif};
begin
  // nothing to do?
  if (RecordSize < 1) or ((DbfFieldDefs = nil) and not Pack) then
    exit;

  // if no exclusive access, terrible things can happen!
  CheckExclusiveAccess;

  // make up some temporary filenames
  NewBaseName := '';
  lRecNo := 0;
  FindNextName(FileName, NewBaseName, lRecNo);

  // select final field definition list
  if DbfFieldDefs = nil then
  begin
    DestFieldDefs := FFieldDefs;
  end else begin
    DestFieldDefs := DbfFieldDefs;
    // copy autoinc values
    for I := 0 to DbfFieldDefs.Count - 1 do
    begin
      lFieldNo := DbfFieldDefs.Items[I].CopyFrom;
      if (lFieldNo >= 0) and (lFieldNo < FFieldDefs.Count) then
        DbfFieldDefs.Items[I].AutoInc := FFieldDefs.Items[lFieldNo].AutoInc;
    end;
  end;

  // create temporary dbf
  DestDbfFile := TDbfFile.Create;
  DestDbfFile.FileName := NewBaseName;
  DestDbfFile.AutoCreate := true;
  DestDbfFile.Mode := pfExclusiveCreate;
  DestDbfFile.OnIndexMissing := FOnIndexMissing;
  DestDbfFile.OnLocaleError := FOnLocaleError;
  DestDbfFile.DbfVersion := FDbfVersion;
  DestDbfFile.FileLangId := FileLangId;
  DestDbfFile.Open;
  // create dbf header
  if FMemoFile <> nil then
    DestDbfFile.FinishCreate(DestFieldDefs, FMemoFile.RecordSize)
  else
    DestDbfFile.FinishCreate(DestFieldDefs, 512);
  DestDbfFile.UpdateLock;

  // adjust size and offsets of fields
  GetMem(RestructFieldInfo, sizeof(TRestructFieldInfo)*DestFieldDefs.Count);
  for lFieldNo := 0 to DestFieldDefs.Count - 1 do
  begin
    TempDstDef := DestFieldDefs.Items[lFieldNo];
    if DbfFieldDefs <> nil then
    begin
      // When restructuring, caller specifies which field to copy from
      if TempDstDef.CopyFrom >= 0 then
        TempSrcDef := FFieldDefs.Items[TempDstDef.CopyFrom]
      else
        TempSrcDef := nil;
    end
    else
    begin
      // When copying, handle blob fields differently
      if TempDstDef.IsBlob then
        TempSrcDef := FFieldDefs.Items[lFieldNo]
      else
        TempSrcDef := nil;
    end;
    RestructFieldInfo[lFieldNo].SourceFieldDef := TempSrcDef;
    if Assigned(TempSrcDef) then
    begin
      if CharInSet(TempDstDef.NativeFieldType, ['F', 'N']) then
      begin
        // get minimum field length
        lFieldSize := Min(TempSrcDef.Precision, TempDstDef.Precision) +
          Min(TempSrcDef.Size - TempSrcDef.Precision,
            TempDstDef.Size - TempDstDef.Precision);
        // if one has dec separator, but other not, we lose one digit
        if (TempDstDef.Precision > 0) xor 
          (CharInSet(TempSrcDef.NativeFieldType, ['F', 'N']) and (TempSrcDef.Precision > 0)) then
          Dec(lFieldSize);
        // should not happen, but check nevertheless (maybe corrupt data)
        if lFieldSize < 0 then
          lFieldSize := 0;
        srcOffset := TempSrcDef.Size - TempSrcDef.Precision - 
          (TempDstDef.Size - TempDstDef.Precision);
        if srcOffset < 0 then
        begin
          dstOffset := -srcOffset;
          srcOffset := 0;
        end else begin
          dstOffset := 0;
        end;
      end else begin
        lFieldSize := Min(TempSrcDef.Size, TempDstDef.Size);
        srcOffset := 0;
        dstOffset := 0;
      end;
      with RestructFieldInfo[lFieldNo] do
      begin
        Size := lFieldSize;
        SourceOffset := TempSrcDef.Offset + srcOffset;
        DestOffset := TempDstDef.Offset + dstOffset;
      end;
    end;
  end;

  // add indexes
  TempIndexDef := TDbfIndexDef.Create(nil);
  for I := 0 to FIndexNames.Count - 1 do
  begin
    // get length of extension -> determines MDX or NDX
    IndexName := FIndexNames.Strings[I];
    TempIndexFile := TIndexFile(FIndexNames.Objects[I]);
    TempIndexFile.GetIndexInfo(IndexName, TempIndexDef);
    if Length(ExtractFileExt(IndexName)) > 0 then
    begin
      // NDX index, get unique file name
      lRecNo := 0;
      FindNextName(IndexName, IndexName, lRecNo);
    end;
    // add this index
    DestDbfFile.OpenIndex(IndexName, TempIndexDef.SortField, true, TempIndexDef.Options);
  end;
  TempIndexDef.Free;

  // get memory for record buffers
  GetMem(pBuff, RecordSize);
  BlobStream := TMemoryStream.Create;
  OldIndexFiles := TStringList.Create;
  // if restructure, we need memory for dest buffer, otherwise use source
  if DbfFieldDefs = nil then
    pDestBuff := pBuff
  else
    GetMem(pDestBuff, DestDbfFile.RecordSize);

  // let the games begin!
  try
    DestDbfFile.BatchStart;
    try
      lWRecNo := 1;
      last := RecordCount;
      if Pack then
        DoProgress(0, last, STRING_PROGRESS_PACKINGRECORDS);
      for lRecNo := 1 to last do
      begin
        // read record from original dbf
        ReadRecord(lRecNo, pBuff);
        // copy record?
        if ({$IFDEF SUPPORT_TRECORDBUFFER}Char(pBuff[0]){$ELSE}pBuff^{$ENDIF} <> '*') or not Pack then
        begin
          // if restructure, initialize dest
          if DbfFieldDefs <> nil then
          begin
            DestDbfFile.InitRecord(PAnsiChar(pDestBuff));
            // copy deleted mark (the first byte)
            pDestBuff^ := pBuff^;
          end;

          if (DbfFieldDefs <> nil) or (FMemoFile <> nil) then
          begin
            // copy fields
            for lFieldNo := 0 to DestFieldDefs.Count-1 do
            begin
              TempDstDef := DestFieldDefs.Items[lFieldNo];
              TempSrcDef := RestructFieldInfo[lFieldNo].SourceFieldDef;
              if Assigned(TempSrcDef) then
              begin
                // handle blob fields differently
                if TempDstDef.IsBlob and TempSrcDef.IsBlob then
                begin
                  // get current blob blockno
                  if GetFieldData(lFieldNo, ftInteger, pBuff, @lBlobPageNo, false) and (lBlobPageNo > 0) then
                  begin
                    BlobStream.Clear;
                    FMemoFile.ReadMemo(lBlobPageNo, BlobStream);
                    BlobStream.Position := 0;
                    // always append
                    DestDbfFile.FMemoFile.WriteMemo(lBlobPageNo, 0, BlobStream);
                    // write new blockno
                    DestDbfFile.SetFieldData(lFieldNo, ftInteger, @lBlobPageNo, pDestBuff, false);
                  end;
                end
                else
                begin
                  // copy content of field
                  if (TempSrcDef.NativeFieldType = TempDstDef.NativeFieldType) or
                    (CharInSet(TempSrcDef.NativeFieldType, ['F', 'N']) and CharInSet(TempDstDef.NativeFieldType, ['F', 'N'])) or
                    (CharInSet(TempSrcDef.NativeFieldType, ['+', 'I']) and CharInSet(TempDstDef.NativeFieldType, ['+', 'I'])) then
                  begin
                    // Source and destination native types are compatible
                    with RestructFieldInfo[lFieldNo] do
                      Move(pBuff[SourceOffset], pDestBuff[DestOffset], Size);
                  end
                  else
                  begin
                    // Source and destination native types are both numeric, but incompatible
                    if (TempSrcDef.FieldType in [ftFloat, ftInteger, ftAutoInc]) and (TempDstDef.FieldType in [ftFloat, ftInteger, ftAutoInc]) then
                    begin
                      if GetFieldData(lFieldNo, TempSrcDef.FieldType, pBuff, @Buffer, True) then
                      begin
                        if (TempSrcDef.FieldType = ftFloat) and (TempDstDef.FieldType in [ftInteger, ftAutoInc]) then
                        begin
                          Int64Value := Round(DoubleValue);
                          if (Int64Value >= Low(IntValue)) and (Int64Value <= High(IntValue)) then
                            IntValue := Int64Value;
                        end;
                        if (TempSrcDef.FieldType in [ftInteger, ftAutoInc]) and (TempDstDef.FieldType = ftFloat) then
                          DoubleValue := IntValue;
                        DestDbfFile.SetFieldData(lFieldNo, TempDstDef.FieldType, @Buffer, pDestBuff, True);
                      end;
                    end;
                  end;
                end;
              end;
            end;
          end;

          // write record
          DestDbfFile.WriteRecord(lWRecNo, pDestBuff);

          // go to next record
          Inc(lWRecNo);
        end;
        if Pack then
        DoProgress(lRecNo, last, STRING_PROGRESS_PACKINGRECORDS);
      end;

      BufferAhead := false;

      // save index filenames
      for I := 0 to FIndexFiles.Count - 1 do
        OldIndexFiles.Add(TIndexFile(IndexFiles[I]).FileName);

      // close dbf
      Close;

      DestDbfFile.Rename(FileName, OldIndexFiles, DeleteFiles);
    
      // we have to reinit fielddefs if restructured
      Open;

      // crop deleted records
      RecordCount := lWRecNo - 1;
      // update date/time stamp, recordcount
      FRecordCountDirty := True;
      BatchUpdate;
    finally
      DestDbfFile.BatchFinish;
    end;
  finally
    // close temporary file
    FreeAndNil(DestDbfFile);
    // free mem
    FreeAndNil(OldIndexFiles);
    FreeMem(pBuff);
    FreeAndNil(BlobStream);
    FreeMem(RestructFieldInfo);
    if DbfFieldDefs <> nil then
      FreeMem(pDestBuff);
  end;
  RegenerateIndexes;
end;

procedure TDbfFile.RegenerateIndexes;
var
  lIndexNo: Integer;
  IndexFile: TIndexFile;
begin
  // recreate every index in every file
  TryExclusive;
  try
    CheckExclusiveAccess;
    for lIndexNo := 0 to FIndexFiles.Count-1 do
    begin
      IndexFile := TIndexFile(FIndexFiles.Items[lIndexNo]);
      PackIndex(IndexFile, EmptyStr, False);
    end;
  finally
    EndExclusive;
  end;
end;

function TDbfFile.GetFieldInfo(const FieldName: AnsiString): TDbfFieldDef;
var
  I: Integer;
  lfi: TDbfFieldDef;
  AFieldName: AnsiString;
begin
  AFieldName := FieldName;
  UniqueString(AFieldName);
  AFieldName := dbfStrUpper(PAnsiChar(AFieldName));
  for I := 0 to FFieldDefs.Count-1 do
  begin
    lfi := TDbfFieldDef(FFieldDefs.Items[I]);
    if lfi.fieldName = AFieldName then
    begin
      Result := lfi;
      exit;
    end;
  end;
  Result := nil;
end;

// NOTE: Dst may be nil!
function TDbfFile.GetFieldData(Column: Integer; DataType: TFieldType; 
  Src, Dst: Pointer; NativeFormat: boolean): Boolean;
var
  TempFieldDef: TDbfFieldDef;
begin
  TempFieldDef := TDbfFieldDef(FFieldDefs.Items[Column]);
  Result := GetFieldDataFromDef(TempFieldDef, DataType, Src, Dst, NativeFormat);
end;

// NOTE: Dst may be nil!
function TDbfFile.GetFieldDataFromDef(AFieldDef: TDbfFieldDef; DataType: TFieldType;
  Src, Dst: Pointer; NativeFormat: boolean): Boolean;
var
  FieldOffset, FieldSize: Integer;
  ldd, ldm, ldy, lth, ltm, lts: Integer;
  date: TDateTime;
  timeStamp: TTimeStamp;
  asciiContents: boolean;
  IntValue: Integer;
  FloatValue: Extended;

  procedure CorrectYear(var wYear: Integer);
  var
    wD, wM, wY, CenturyBase: Word;
  begin
     if wYear >= 100 then
       Exit;
     DecodeDate(Date, wY, wm, wD);
     // use Delphi-Date-Window
     CenturyBase := wY{must be CurrentYear} - TwoDigitYearCenturyWindow;
     Inc(wYear, CenturyBase div 100 * 100);
     if (TwoDigitYearCenturyWindow > 0) and (wYear < CenturyBase) then
        Inc(wYear, 100);
  end;

  procedure SaveDateToDst;
  begin
    if not NativeFormat then
    begin
      // Delphi 5 requests a TDateTime
      PDateTime(Dst)^ := date;
    end else begin
      // Delphi 3 and 4 request a TDateTimeRec
      //  date is TTimeStamp.date
      //  datetime = msecs == BDE timestamp as we implemented it
      if DataType = ftDateTime then
      begin
        PDateTimeRec(Dst)^.DateTime := date;
      end else begin
        PLongInt(Dst)^ := DateTimeToTimeStamp(date).Date;
      end;
    end;
  end;

begin
  // test if non-nil source (record buffer)
  if Src = nil then
  begin
    Result := false;
    exit;
  end;

  // check Dst = nil, called with dst = nil to check empty field
  if (FNullField <> nil) and (Dst = nil) and (AFieldDef.NullPosition >= 0) then
  begin
    // go to byte with null flag of this field
    Src := PAnsiChar(Src) + FNullField.Offset + (AFieldDef.NullPosition shr 3);
    Result := (PByte(Src)^ and (1 shl (AFieldDef.NullPosition and $7))) <> 0;
    exit;
  end;
  
  FieldOffset := AFieldDef.Offset;
  FieldSize := AFieldDef.Size;
  Src := PAnsiChar(Src) + FieldOffset;
  asciiContents := false;
  Result := true;
  // field types that are binary and of which the fieldsize should not be truncated
  case AFieldDef.NativeFieldType of
    '+', 'I':
      begin
        if FDbfVersion <> xFoxPro then
        begin
          Result := PDWord(Src)^ <> 0;
          if Result and (Dst <> nil) then
          begin
            PDWord(Dst)^ := SwapIntBE(PDWord(Src)^);
            if Result then
              PInteger(Dst)^ := Integer(PDWord(Dst)^ xor $80000000);
          end;
        end else begin
          Result := true;
          if Dst <> nil then
            PInteger(Dst)^ := SwapIntLE(PInteger(Src)^);
        end;
      end;
    'O':
      begin
{$ifdef SUPPORT_INT64}
        Result := (PInt64(Src)^ <> 0);
        if Result and (Dst <> nil) then
        begin
          SwapInt64BE(Src, Dst);
          if PInt64(Dst)^ > 0 then
            PInt64(Dst)^ := not PInt64(Dst)^
          else
            PDouble(Dst)^ := PDouble(Dst)^ * -1;
        end;
{$endif}
      end;
    '@':
      begin
{$ifdef SUPPORT_INT64}
        Result := (PInteger(Src)^ <> 0) and (PInteger(PAnsiChar(Src)+4)^ <> 0);
        if Result and (Dst <> nil) then
        begin
          SwapInt64BE(Src, Dst);
          if FDateTimeHandling = dtBDETimeStamp then
            date := BDETimeStampToDateTime(PDouble(Dst)^)
          else
            date := PDateTime(Dst)^;
          SaveDateToDst;
        end;
{$endif}
      end;
    'T':
      begin
        // all binary zeroes -> empty datetime
{$ifdef SUPPORT_INT64}        
        Result := PInt64(Src)^ <> 0;
{$else}        
        Result := (PInteger(Src)^ <> 0) or (PInteger(PAnsiChar(Src)+4)^ <> 0);
{$endif}
        if Result and (Dst <> nil) then
        begin
          timeStamp.Date := SwapIntLE(PInteger(Src)^) - JulianDateDelta;
          timeStamp.Time := SwapIntLE(PInteger(PAnsiChar(Src)+4)^);
          date := TimeStampToDateTime(timeStamp);
          SaveDateToDst;
        end;
      end;
    'Y':
      begin
{$ifdef SUPPORT_INT64}
        Result := true;
        if Dst <> nil then
        begin
          PInt64(Dst)^ := SwapIntLE(PInt64(Src)^);
          if DataType = ftCurrency then
            PDouble(Dst)^ := PInt64(Dst)^ / 10000.0;
        end;
{$endif}
      end;
    'B':    // foxpro double
      begin
        if FDbfVersion = xFoxPro then
        begin
          Result := true;
          if Dst <> nil then
            PInt64(Dst)^ := SwapIntLE(PInt64(Src)^);
        end else
          asciiContents := true;
      end;
    'M':
      begin
        if FieldSize = 4 then
        begin
          Result := PInteger(Src)^ <> 0;
          if Dst <> nil then
            PInteger(Dst)^ := SwapIntLE(PInteger(Src)^);
        end else
          asciiContents := true;
      end;
  else
    asciiContents := true;
  end;
  if asciiContents then
  begin
    // truncate spaces at end by shortening fieldsize
    while (FieldSize > 0) and (((PAnsiChar(Src) + FieldSize - 1)^ = ' ') or ((PAnsiChar(Src) + FieldSize - 1)^ = #0)) do
      dec(FieldSize);
    // if not string field, truncate spaces at beginning too
    if DataType <> ftString then
      while (FieldSize > 0) and (PAnsiChar(Src)^ = ' ') do
      begin
        inc(PAnsiChar(Src));
        dec(FieldSize);
      end;
    // return if field is empty
    Result := FieldSize > 0;
    if Result and (Dst <> nil) then     // data not needed if Result= false or Dst=nil
      case DataType of
      ftBoolean:
        begin
          // in DBase- FileDescription lowercase t is allowed too
          // with asking for Result= true s must be longer then 0
          // else it happens an AV, maybe field is NULL
          if (PAnsiChar(Src)^ = 'T') or (PAnsiChar(Src)^ = 't') then
            PWord(Dst)^ := 1
          else
            PWord(Dst)^ := 0;
        end;
      ftSmallInt:
      begin
        IntValue := 0;
        Result := StrToInt32Width(IntValue, Src, FieldSize, 0);
        if Result then
          Result := (IntValue >= Low(SmallInt)) and (IntValue <= High(SmallInt));
        if Result then
          PSmallInt(Dst)^ := IntValue;
      end;
{$ifdef SUPPORT_INT64}
      ftLargeInt:
        Result := StrToIntWidth(PInt64(Dst)^, Src, FieldSize, 0);
{$endif}
      ftInteger:
        Result := StrToInt32Width(PInteger(Dst)^, Src, FieldSize, 0);
      ftFloat, ftCurrency:
      begin
        FloatValue := 0;
        Result := StrToFloatWidth(FloatValue, Src, FieldSize, 0);
        if Result then
          PDouble(Dst)^ := FloatValue;
      end;
      ftDate, ftDateTime:
        begin
          // get year, month, day
          ldy := 0;
          StrToInt32Width(ldy, PAnsiChar(Src) + 0, 4, 1);
          ldm := 0;
          StrToInt32Width(ldm, PAnsiChar(Src) + 4, 2, 1);
          ldd := 0;
          StrToInt32Width(ldd, PAnsiChar(Src) + 6, 2, 1);
          //if (ly<1900) or (ly>2100) then ly := 1900;
          //Year from 0001 to 9999 is possible
          //everyting else is an error, an empty string too
          //Do DateCorrection with Delphis possibillities for one or two digits
          if (ldy < 100) and (PAnsiChar(Src)[0] = #32) and (PAnsiChar(Src)[1] = #32) then
            CorrectYear(ldy);
          try
            date := EncodeDate(ldy, ldm, ldd);
          except
            date := 0;
          end;

          // time stored too?
          if (AFieldDef.FieldType = ftDateTime) and (DataType = ftDateTime) then
          begin
            // get hour, minute, second
            lth := 0;
            StrToInt32Width(lth, PAnsiChar(Src) + 8,  2, 1);
            ltm := 0;
            StrToInt32Width(ltm, PAnsiChar(Src) + 10, 2, 1);
            lts := 0;
            StrToInt32Width(lts, PAnsiChar(Src) + 12, 2, 1);
            // encode
            try
              date := date + EncodeTime(lth, ltm, lts, 0);
            except
              date := 0;
            end;
          end;

          SaveDateToDst;
        end;
      ftString:
        dbfStrLCopy(PAnsiChar(Dst), PAnsiChar(Src), FieldSize);
    end else begin
      case DataType of
      ftString:
        if Dst <> nil then
          PAnsiChar(Dst)[0] := #0;
      end;
    end;
  end;
end;

procedure TDbfFile.UpdateNullField(Buffer: Pointer; AFieldDef: TDbfFieldDef;
  Action: TUpdateNullField);
var
  NullDst: pbyte;
  Mask: byte;
begin
  // this field has null setting capability
  NullDst := PByte(PAnsiChar(Buffer) + FNullField.Offset + (AFieldDef.NullPosition shr 3)); // Was PChar
  Mask := 1 shl (AFieldDef.NullPosition and $7);
  if Action = unSet then
  begin
    // clear the field, set null flag
    NullDst^ := NullDst^ or Mask;
  end else begin
    // set field data, clear null flag
    NullDst^ := NullDst^ and not Mask;
  end;
end;

procedure TDbfFile.SetFieldData(Column: Integer; DataType: TFieldType; 
  Src, Dst: Pointer; NativeFormat: boolean);
const
  IsBlobFieldToPadChar: array[Boolean] of AnsiChar = (#32, '0'); // Was Char
  SrcNilToUpdateNullField: array[boolean] of TUpdateNullField = (unClear, unSet);
var
  FieldSize,FieldPrec: Integer;
  TempFieldDef: TDbfFieldDef;
  Len: Integer;
  IntValue: dword;
  year, month, day: Word;
  hour, minute, sec, msec: Word;
  date: TDateTime;
  timeStamp: TTimeStamp;
  asciiContents: boolean;

  procedure LoadDateFromSrc;
  begin
    if not NativeFormat then
    begin
      // Delphi 5, new format, passes a TDateTime
      date := PDateTime(Src)^;
    end else begin
      // Delphi 3 and 4, old "native" format, pass a TDateTimeRec with a time stamp
      //  date = integer
      //  datetime = msecs == BDETimeStampToDateTime as we implemented it
      if DataType = ftDateTime then
      begin
        date := PDouble(Src)^;
      end else begin
        timeStamp.Time := 0;
        timeStamp.Date := PLongInt(Src)^;
        date := TimeStampToDateTime(timeStamp);
      end;
    end;
  end;

begin
  TempFieldDef := TDbfFieldDef(FFieldDefs.Items[Column]);
  FieldSize := TempFieldDef.Size;
  FieldPrec := TempFieldDef.Precision;

  // if src = nil then write empty field
  // symmetry with above

  // foxpro has special _nullfield for flagging fields as `null'
  if (FNullField <> nil) and (TempFieldDef.NullPosition >= 0) then
    UpdateNullField(Dst, TempFieldDef, SrcNilToUpdateNullField[Src = nil]);

  // copy field data to record buffer
  Dst := PAnsiChar(Dst) + TempFieldDef.Offset;
  asciiContents := false;
  case TempFieldDef.NativeFieldType of
    '+', 'I':
      begin
        if FDbfVersion <> xFoxPro then
        begin
          if Src = nil then
            IntValue := 0
          else
            IntValue := PDWord(Src)^ xor $80000000;
          PDWord(Dst)^ := SwapIntBE(IntValue);
        end else begin
          if Src = nil then
            PDWord(Dst)^ := 0
          else
            PDWord(Dst)^ := SwapIntLE(PDWord(Src)^);
        end;
      end;
    'O':
      begin
{$ifdef SUPPORT_INT64}
        if Src = nil then
        begin
          PInt64(Dst)^ := 0;
        end else begin
          if PDouble(Src)^ < 0 then
            PInt64(Dst)^ := not PInt64(Src)^
          else
            PDouble(Dst)^ := (PDouble(Src)^) * -1;
          SwapInt64BE(Dst, Dst);
        end;
{$endif}
      end;
    '@':
      begin
{$ifdef SUPPORT_INT64}
        if Src = nil then
        begin
          PInt64(Dst)^ := 0;
        end else begin
          LoadDateFromSrc;
          if FDateTimeHandling = dtBDETimeStamp then
            date := DateTimeToBDETimeStamp(date);
          SwapInt64BE(@date, Dst);
        end;
{$endif}
      end;
    'T':
      begin
        // all binary zeroes -> empty datetime
        if Src = nil then
        begin
{$ifdef SUPPORT_INT64}
          PInt64(Dst)^ := 0;
{$else}          
          PInteger(Dst)^ := 0;
          PInteger(PAnsiChar(Dst)+4)^ := 0;
{$endif}          
        end else begin
          LoadDateFromSrc;
          timeStamp := DateTimeToTimeStamp(date);
          PInteger(Dst)^ := SwapIntLE(timeStamp.Date + JulianDateDelta);
          PInteger(PAnsiChar(Dst)+4)^ := SwapIntLE(timeStamp.Time);
        end;
      end;
    'Y':
      begin
{$ifdef SUPPORT_INT64}
        if Src = nil then
        begin
          PInt64(Dst)^ := 0;
        end else begin
          case DataType of
            ftCurrency:
              PInt64(Dst)^ := Trunc(PDouble(Src)^ * 10000);
            ftBCD:
              PCurrency(Dst)^ := PCurrency(Src)^;
          end;
          SwapInt64LE(Dst, Dst);
        end;
{$endif}
      end;
    'B':
      begin
{$ifdef SUPPORT_INT64}
        if DbfVersion = xFoxPro then
        begin
          if Src = nil then
            PDouble(Dst)^ := 0
          else
            SwapInt64LE(Src, Dst);
        end else
          asciiContents := true;
{$endif}
      end;
    'M':
      begin
        if FieldSize = 4 then
        begin
          if Src = nil then
            PInteger(Dst)^ := 0
          else
            PInteger(Dst)^ := SwapIntLE(PInteger(Src)^);
        end else
          asciiContents := true;
      end;
  else
    asciiContents := true;
  end;
  if asciiContents then
  begin
    if Src = nil then
    begin
      if (FDbfVersion >= xBaseVII) and (DataType=ftString) then
        FillChar(Dst^, FieldSize, #0)
      else
        FillChar(Dst^, FieldSize, ' ');
    end else begin
      case DataType of
        ftBoolean:
          begin
            if PWord(Src)^ <> 0 then
              PAnsiChar(Dst)^ := 'T'
            else
              PAnsiChar(Dst)^ := 'F';
          end;
        ftSmallInt:
          IntToStrWidth(PSmallInt(Src)^, FieldSize, PAnsiChar(Dst), True, #32);
{$ifdef SUPPORT_INT64}
        ftLargeInt:
          IntToStrWidth(PInt64(Src)^, FieldSize, PAnsiChar(Dst), True, #32);
{$endif}
        ftFloat, ftCurrency:
          FloatToStrWidth(PDouble(Src)^, FieldSize, FieldPrec, PAnsiChar(Dst), True);
        ftInteger:
          IntToStrWidth(PInteger(Src)^, FieldSize, PAnsiChar(Dst), True, IsBlobFieldToPadChar[TempFieldDef.IsBlob]);
        ftDate, ftDateTime:
          begin
            LoadDateFromSrc;
            // decode
            DecodeDate(date, year, month, day);
            // format is yyyymmdd
            IntToStrWidth(year,  4, PAnsiChar(Dst),   True, DBF_ZERO);
            IntToStrWidth(month, 2, PAnsiChar(Dst)+4, True, DBF_ZERO);
            IntToStrWidth(day,   2, PAnsiChar(Dst)+6, True, DBF_ZERO);
            // do time too if datetime
            if DataType = ftDateTime then
            begin
              DecodeTime(date, hour, minute, sec, msec);
              // format is hhmmss
              IntToStrWidth(hour,   2, PAnsiChar(Dst)+8,  True, DBF_ZERO);
              IntToStrWidth(minute, 2, PAnsiChar(Dst)+10, True, DBF_ZERO);
              IntToStrWidth(sec,    2, PAnsiChar(Dst)+12, True, DBF_ZERO);
            end;
          end;
        ftString:
          begin
            // copy data
            Len := dbfStrLen(PAnsiChar(Src));
            if Len > FieldSize then
              Len := FieldSize;
            Move(Src^, Dst^, Len);
            // fill remaining space with spaces
            FillChar((PAnsiChar(Dst)+Len)^, FieldSize - Len, ' ');
          end;
      end;  // case datatype
    end;
  end;
end;

procedure TDbfFile.InitDefaultBuffer;
var
  lRecordSize: integer;
  TempFieldDef: TDbfFieldDef;
  I: Integer;
begin
  lRecordSize := PDbfHdr(Header)^.RecordSize;
  // clear buffer (assume all string, fix specific fields later)
  //   note: Self.RecordSize is used for reading fielddefs too
  GetMem(FDefaultBuffer, lRecordSize+1);
  FillChar(FDefaultBuffer^, lRecordSize, ' ');
  
  // set nullflags field so that all fields are null
  if FNullField <> nil then
    FillChar(PAnsiChar(FDefaultBuffer+FNullField.Offset)^, FNullField.Size, $FF); // Was PChar

  // check binary and default fields
  for I := 0 to FFieldDefs.Count-1 do
  begin
    TempFieldDef := FFieldDefs.Items[I];
    // binary field? (foxpro memo fields are binary, but dbase not)
    if CharInSet(TempFieldDef.NativeFieldType, ['I', 'O', '@', '+', '0', 'Y'])
        or ((TempFieldDef.NativeFieldType = 'M') and (TempFieldDef.Size = 4))
        or ((FDbfVersion >= xBaseVII) and (TempFieldDef.NativeFieldType='C') and (not TempFieldDef.HasDefault)) then
      FillChar(PAnsiChar(FDefaultBuffer+TempFieldDef.Offset)^, TempFieldDef.Size, 0); // Was PChar
    // copy default value?
    if TempFieldDef.HasDefault then
    begin
      Move(TempFieldDef.DefaultBuf[0], FDefaultBuffer[TempFieldDef.Offset], TempFieldDef.Size);
      // clear the null flag, this field has a value
      if FNullField <> nil then
        UpdateNullField(FDefaultBuffer, TempFieldDef, unClear);
    end;
  end;
end;

procedure TDbfFile.InitRecord(DestBuf: PAnsiChar);
begin
  if FDefaultBuffer = nil then
    InitDefaultBuffer;
  Move(FDefaultBuffer^, DestBuf^, RecordSize);
end;

procedure TDbfFile.ApplyAutoIncToBuffer(DestBuf: TDbfRecordBuffer);
var
  TempFieldDef: TDbfFieldDef;
  I, NextVal, lAutoIncOffset: {LongWord} Cardinal;    {Delphi 3 does not know LongWord?}
begin
  if FAutoIncPresent then
  begin
    // find autoinc fields
    for I := 0 to FFieldDefs.Count-1 do
    begin
      TempFieldDef := FFieldDefs.Items[I];
      if (TempFieldDef.NativeFieldType = '+') then
      begin
        // read current auto inc, from header or field, depending on sharing
        lAutoIncOffset := sizeof(rDbfHdr) + sizeof(rAfterHdrVII) + 
          FieldDescVII_AutoIncOffset + I * sizeof(rFieldDescVII);
        if NeedLocks then
        begin
          ReadBlock(@NextVal, 4, lAutoIncOffset);
          NextVal := SwapIntLE(NextVal);
        end else
          NextVal := TempFieldDef.AutoInc;
        // store to buffer, positive = high bit on, so flip it
        PCardinal(DestBuf+TempFieldDef.Offset)^ := SwapIntBE(NextVal or $80000000);
        // increase
        if NextVal = High(NextVal) then
          NextVal := Low(NextVal)
        else
          Inc(NextVal);
        TempFieldDef.AutoInc := NextVal;
        // write new value to header buffer
        PCardinal(FHeader+lAutoIncOffset)^ := SwapIntLE(NextVal);
      end;
    end;

    // write modified header (new autoinc values) to file
    WriteHeader;
  end;
end;

procedure TDbfFile.TryExclusive;
var
  I: Integer;
begin
  inherited;

  // exclusive succeeded? open index & memo exclusive too
  if Mode in [pfMemoryCreate..pfExclusiveOpen] then
  begin
    // indexes
    for I := 0 to FIndexFiles.Count - 1 do
      TPagedFile(FIndexFiles[I]).TryExclusive;
    // memo
    if FMemoFile <> nil then
      FMemoFile.TryExclusive;
  end;
end;

procedure TDbfFile.EndExclusive;
var
  I: Integer;
begin
  // end exclusive on index & memo too
  for I := 0 to FIndexFiles.Count - 1 do
    TPagedFile(FIndexFiles[I]).EndExclusive;
  // memo
  if FMemoFile <> nil then
    FMemoFile.EndExclusive;
  // dbf file
  inherited;
end;

procedure TDbfFile.OpenIndex(IndexName, IndexField: string; CreateIndex: Boolean; Options: TIndexOptions);
  //
  // assumes IndexName is not empty
  //
const
  // memcr, memop, excr, exopen, rwcr, rwopen, rdonly
  IndexOpenMode: array[boolean, pfMemoryCreate..pfReadOnly] of TPagedFileMode =
   ((pfMemoryCreate, pfMemoryCreate, pfExclusiveOpen, pfExclusiveOpen, pfReadWriteOpen, pfReadWriteOpen,
     pfReadOnly),
    (pfMemoryCreate, pfMemoryCreate, pfExclusiveCreate, pfExclusiveCreate, pfReadWriteCreate, pfReadWriteCreate,
     pfReadOnly));
var
  lIndexFile: TIndexFile;
  lIndexFileName: string;
  createMdxFile: Boolean;
  tempExclusive: boolean;
  addedIndexFile: Integer;
  addedIndexName: Integer;
begin
  // init
  addedIndexFile := -1;
  addedIndexName := -1;
  createMdxFile := false;
  // index already opened?
  lIndexFile := GetIndexByName(IndexName);
  if (lIndexFile <> nil) and (lIndexFile = FMdxFile) and CreateIndex then
  begin
    // index already exists in MDX file
    // delete it to save space, this causes a repage
    FMdxFile.DeleteIndex(IndexName);
    // index no longer exists
    lIndexFile := nil;
  end;
  if (lIndexFile = nil) and (IndexName <> EmptyStr) then
  begin
    // check if no extension, then create MDX index
    if Length(ExtractFileExt(IndexName)) = 0 then
    begin
      // check if mdx index already opened
      if FMdxFile <> nil then
      begin
        lIndexFileName := EmptyStr;
        lIndexFile := FMdxFile;
      end else begin
        lIndexFileName := ChangeFileExt(FileName, '.mdx');
        createMdxFile := true;
      end;
    end else begin
      lIndexFileName := IndexName;
    end;
    // do we need to open / create file?
    if lIndexFileName <> EmptyStr then
    begin
      // try to open / create the file
      lIndexFile := TIndexFile.Create(Self);
      lIndexFile.FileName := lIndexFileName;
      lIndexFile.Mode := IndexOpenMode[CreateIndex, Mode];
      lIndexFile.AutoCreate := CreateIndex or (Length(IndexField) > 0);
      lIndexFile.CodePage := UseCodePage;
      lIndexFile.OnLocaleError := FOnLocaleError;
      lIndexFile.Open;
      // index file ready for use?
      if not lIndexFile.ForceClose then
      begin
        // if we had to create the index, store that info
        CreateIndex := lIndexFile.FileCreated;
        // check if trying to create empty index
        if CreateIndex and (IndexField = EmptyStr) then
        begin
          FreeAndNil(lIndexFile);
          CreateIndex := false;
          createMdxFile := false;
        end else begin
          // add new index file to list
          addedIndexFile := FIndexFiles.Add(lIndexFile);
        end;
        // created accompanying mdx file?
        if createMdxFile then
          FMdxFile := lIndexFile;
      end else begin
        // asked to close! close file
        FreeAndNil(lIndexFile);
      end;
    end;

    // check if file succesfully opened
    if lIndexFile <> nil then
    begin
      // add index to list
      addedIndexName := FIndexNames.AddObject(IndexName, lIndexFile);
    end;
  end;
  // create it or open it?
  if lIndexFile <> nil then
  begin
    if not CreateIndex then
      if lIndexFile = FMdxFile then
        CreateIndex := lIndexFile.IndexOf(IndexName) < 0;
    if CreateIndex then
    begin
      // try get exclusive mode
      tempExclusive := IsSharedAccess;
      if tempExclusive then TryExclusive;
      // always uppercase index expression
      IndexField := AnsiUpperCase(IndexField);
      try
        try
          // create index if asked
          lIndexFile.CreateIndex(IndexField, IndexName, Options);
          // add all records
          PackIndex(lIndexFile, IndexName, CreateIndex);
          // if we wanted to open index readonly, but we created it, then reopen
          if Mode = pfReadOnly then
          begin
            lIndexFile.CloseFile;
            lIndexFile.Mode := pfReadOnly;
            lIndexFile.OpenFile;
          end;
          // if mdx file just created, write changes to dbf header
          // set MDX flag to true
          if lIndexFile = FMdxFile then
          begin
            PDbfHdr(Header)^.MDXFlag := 1;
            WriteHeader;
          end;
        except
          // :-( need to undo 'damage'....
          // remove index from list(s) if just added
          if addedIndexFile >= 0 then
            FIndexFiles.Delete(addedIndexFile);
          if addedIndexName >= 0 then
            FIndexNames.Delete(addedIndexName);
          // if no file created, do not destroy!
          if addedIndexFile >= 0 then
            DeleteIndexFile(lIndexFile);
          raise;
        end;
      finally
        // return to previous mode
        if tempExclusive then EndExclusive;
      end;
    end;
  end;
end;

procedure TDbfFile.PackIndex(lIndexFile: TIndexFile; AIndexName: string; CreateIndex: Boolean);
var
  prevMode: TIndexUpdateMode;
  prevIndex: string;
{$ifdef USE_CACHE}
  cur, last: Integer;
  prevCache: Integer;
{$endif}
begin
  // save current mode in case we change it
  prevMode := lIndexFile.UpdateMode;
  prevIndex := lIndexFile.IndexName;
  // check if index specified
  if Length(AIndexName) > 0 then
  begin
    // only pack specified index, not all
    lIndexFile.IndexName := AIndexName;
    lIndexFile.UpdateMode := umCurrent;
    if not CreateIndex then
    begin
      lIndexFile.CalcRegenerateIndexes;
      lIndexFile.ClearIndex;
    end;
  end else begin
    lIndexFile.IndexName := EmptyStr;
    lIndexFile.UpdateMode := umAll;
    if not CreateIndex then
      lIndexFile.CalcRegenerateIndexes;
    lIndexFile.Clear;
  end;
  // prepare update
{$ifdef USE_CACHE}
  BufferAhead := true;
  prevCache := lIndexFile.CacheSize;
  lIndexFile.CacheSize := GetFreeMemory;
  if lIndexFile.CacheSize < 16384 * 1024 then
    lIndexFile.CacheSize := 16384 * 1024;
{$endif}
  try
    try
{$ifdef USE_CACHE}
      cur := 1;
      last := RecordCount;
      DoProgress(0, last, STRING_PROGRESS_READINGRECORDS);
      while cur <= last do
      begin
        ReadRecord(cur, FPrevBuffer);
        if not lIndexFile.Insert(cur, FPrevBuffer) then
          lIndexFile.InsertError;
        DoProgress(cur, last, STRING_PROGRESS_READINGRECORDS);
        inc(cur);
      end;
{$else}
      lIndexFile.OnProgress := FOnProgress;
      try
        lIndexFile.BulkLoadIndexes;
      finally
        lIndexFile.OnProgress := nil;
      end;
{$endif}
    except
      on E: EDbfError do
      begin
        lIndexFile.DeleteIndex(lIndexFile.IndexName);
        raise;
      end;
    end;
  finally
    // restore previous mode
{$ifdef USE_CACHE}
    BufferAhead := false;
    lIndexFile.BufferAhead := true;
{$endif}
    lIndexFile.Flush;
{$ifdef USE_CACHE}
    lIndexFile.BufferAhead := false;
    lIndexFile.CacheSize := prevCache;
{$endif}
    lIndexFile.UpdateMode := prevMode;
    lIndexFile.IndexName := prevIndex;
  end;
end;

procedure TDbfFile.RepageIndex(AIndexFile: string);
var
  lIndexNo: Integer;
begin
  // DBF MDX index?
  if Length(AIndexFile) = 0 then
  begin
    if FMdxFile <> nil then
    begin
      // repage attached mdx
      FMdxFile.RepageFile;
    end;
  end else begin
    // search index file
    lIndexNo := FIndexNames.IndexOf(AIndexFile);
    // index found?
    if lIndexNo >= 0 then
      TIndexFile(FIndexNames.Objects[lIndexNo]).RepageFile;
  end;
end;

procedure TDbfFile.CompactIndex(AIndexFile: string);
var
  lIndexNo: Integer;
begin
  // DBF MDX index?
  if Length(AIndexFile) = 0 then
  begin
    if FMdxFile <> nil then
    begin
      // repage attached mdx
      FMdxFile.CompactFile;
    end;
  end else begin
    // search index file
    lIndexNo := FIndexNames.IndexOf(AIndexFile);
    // index found?
    if lIndexNo >= 0 then
      TIndexFile(FIndexNames.Objects[lIndexNo]).CompactFile;
  end;
end;

procedure TDbfFile.CloseIndex(AIndexName: string);
var
  lIndexNo: Integer;
  lIndex: TIndexFile;
begin
  // search index file
  lIndexNo := FIndexNames.IndexOf(AIndexName);
  // don't close mdx file
  if (lIndexNo >= 0) then
  begin
    // get index pointer
    lIndex := TIndexFile(FIndexNames.Objects[lIndexNo]);
    if (lIndex <> FMdxFile) then
    begin
      // close file
      lIndex.Free;
      // remove from lists
      FIndexFiles.Remove(lIndex);
      FIndexNames.Delete(lIndexNo);
      // was this the current index?
      if (FCurIndex = lIndexNo) then
      begin
        FCurIndex := -1;
        //FCursor := FDbfCursor;
      end;
    end;
  end;
end;

function TDbfFile.DeleteIndex(const AIndexName: string): Boolean;
var
  lIndexNo: Integer;
  lIndex: TIndexFile;
  lFileName: string;
begin
  // search index file
  lIndexNo := FIndexNames.IndexOf(AIndexName);
  Result := lIndexNo >= 0;
  // found index?
  if Result then
  begin
    // can only delete indexes from MDX files
    lIndex := TIndexFile(FIndexNames.Objects[lIndexNo]);
    if lIndex = FMdxFile then
    begin
      lIndex.DeleteIndex(AIndexName);
      // remove it from the list
      FIndexNames.Delete(lIndexNo);
      // no more MDX indexes?
      lIndexNo := FIndexNames.IndexOfObject(FMdxFile);
      if lIndexNo = -1 then
      begin
        // no MDX indexes left
        lIndexNo := FIndexFiles.IndexOf(FMdxFile);
        if lIndexNo >= 0 then
          FIndexFiles.Delete(lIndexNo);
        lFileName := FMdxFile.FileName;
        FreeAndNil(FMdxFile);
        // erase file
        Sysutils.DeleteFile(lFileName);
        // clear mdx flag
        PDbfHdr(Header)^.MDXFlag := 0;
        WriteHeader;
      end;
    end else begin
      // close index first
      CloseIndex(AIndexName);
      // delete file from disk
      SysUtils.DeleteFile(AIndexName);
    end;
  end;
end;

function TDbfFile.Insert(Buffer: TDbfRecordBuffer): integer;
type
  TErrorContext = (ecNone, ecInsert, ecWriteIndex, ecWriteDbf);
var
  newRecord: Integer;
  lIndex: TIndexFile;

  procedure RollBackIndexesAndRaise(Count: Integer; ErrorContext: TErrorContext);
  var
    errorMsg: string;
    I: Integer;
  begin
    // rollback committed indexes
    for I := 0 to Count-1 do
    begin
      lIndex := TIndexFile(FIndexFiles.Items[I]);
      lIndex.Delete(newRecord, Buffer);
    end;

    // reset any dbf file error
    ResetError;

    // if part of indexes committed -> always index error msg
    // if error while rolling back index -> index error msg
    case ErrorContext of
      ecInsert: begin TIndexFile(FIndexFiles.Items[Count]).InsertError; exit; end;
      ecWriteIndex: errorMsg := STRING_WRITE_INDEX_ERROR;
      ecWriteDbf: errorMsg := STRING_WRITE_ERROR;
    end;
    raise EDbfWriteError.Create(errorMsg);
  end;

var
  I: Integer;
  error: TErrorContext;
  Locked: Boolean;
begin
  Result := 0;
  if NeedLocks and (not FileLocked) then
    if not LockPage(-2, True) then
      raise EDbfError.Create(STRING_RECORD_LOCKED);
  try
    if NeedLocks and (not FileLocked) then
    begin
      Locked := LockPage(0, False);
      if not Locked then
        raise EDbfError.Create(STRING_RECORD_LOCKED);
    end
    else
      Locked := True;
    try
      if NeedLocks and (not FileLocked) then
        if not LockPage(-1, False) then
          EDbfError.Create(STRING_RECORD_LOCKED);
      try
        // get new record index
        newRecord := RecordCount+1;
        // lock record so we can write data
        if NeedLocks and (not FileLocked) then
          if not LockPage(newRecord, False) then
            EDbfError.Create(STRING_RECORD_LOCKED);
        try
          if NeedLocks and (not FileLocked) then
            UnlockPage(0);
          Locked := False;
          // write autoinc value
          if not FInCopyFrom then
            ApplyAutoIncToBuffer(Buffer);
          error := ecNone;
          I := 0;
          while I < FIndexFiles.Count do
          begin
            lIndex := TIndexFile(FIndexFiles.Items[I]);
            if not lIndex.Insert(newRecord, Buffer) then
              error := ecInsert;
            if lIndex.WriteError then
              error := ecWriteIndex;
            if error <> ecNone then
            begin
              // if there's an index write error, I shouldn't
              // try to write the dbf header and the new record,
              // but raise an exception right away
              RollBackIndexesAndRaise(I, error);
            end;
            Inc(I);
          end;

          if NeedLocks then
          begin
            // indexes ok -> continue inserting
            // update header record count
            // read current header
            ReadHeader;
            // increase current record count
            Inc(PDbfHdr(Header)^.RecordCount);
            // write header to disk
            WriteHeader;
            // done with header
          end
          else
            FRecordCountDirty := True;

          if WriteError then
          begin
            // couldn't write header, so I shouldn't
            // even try to write the record.
            //
            // At this point I should "roll back"
            // the already written index records.
            // if this fails, I'm in deep trouble!
            RollbackIndexesAndRaise(FIndexFiles.Count, ecWriteDbf);
          end;

          // write locking info
          if FLockField <> nil then
            WriteLockInfo(Buffer);
          // write buffer to disk
          WriteRecord(newRecord, Buffer);
          if NeedLocks then
            WriteEOFTerminator;

          // error occurred while writing?
          if WriteError then
          begin
            // -- Tobias --
            // The record couldn't be written, so
            // the written index records and the
            // change to the header have to be
            // rolled back
            ReadHeader;
            Dec(PDbfHdr(Header)^.RecordCount);
            WriteHeader;
            // roll back indexes too
            RollbackIndexesAndRaise(FIndexFiles.Count, ecWriteDbf);
          end
          else
            Result := newRecord;
        finally
          if NeedLocks and (not FileLocked) then
            UnlockPage(newRecord);
        end;
      finally
        if NeedLocks and (not FileLocked) then
          UnlockPage(-1);
      end;
    finally
      if NeedLocks and (not FileLocked) and Locked then
        UnlockPage(0);
    end;
  finally
    if NeedLocks and (not FileLocked) then
      UnlockPage(-2);
  end;
end;

procedure TDbfFile.WriteLockInfo(Buffer: TDbfRecordBuffer);
//
// *) assumes FHasLockField = true
//
var
  year, month, day, hour, minute, sec, msec: Word;
  lockoffset: integer;
begin
  // increase change count
  lockoffset := FLockField.Offset;
  Inc(PWord(Buffer+lockoffset)^);
  // set time
  DecodeDate(Now(), year, month, day);
  DecodeTime(Now(), hour, minute, sec, msec);
  Buffer[lockoffset+2] := {$IFNDEF DELPHI_2009}Char{$ENDIF}(hour);
  Buffer[lockoffset+3] := {$IFNDEF DELPHI_2009}Char{$ENDIF}(minute);
  Buffer[lockoffset+4] := {$IFNDEF DELPHI_2009}Char{$ENDIF}(sec);
  // set date
  Buffer[lockoffset+5] := {$IFNDEF DELPHI_2009}Char{$ENDIF}(year - 1900);
  Buffer[lockoffset+6] := {$IFNDEF DELPHI_2009}Char{$ENDIF}(month);
  Buffer[lockoffset+7] := {$IFNDEF DELPHI_2009}Char{$ENDIF}(day);
  // set name
  FillChar(Buffer[lockoffset+8], FLockField.Size-8, ' ');
  Move(DbfGlobals.UserName[1], Buffer[lockoffset+8], FLockUserLen);
end;

procedure TDbfFile.LockRecord(RecNo: Integer; Buffer: TDbfRecordBuffer; Resync: Boolean);
var
  Locked : Boolean;
begin
  if NeedLocks and (not FileLocked) then
  begin
    if FVirtualLocks then
    begin
      if not LockPage(-2, True) then
        raise EDbfError.Create(STRING_RECORD_LOCKED);
      try
        if not LockPage(0, False) then
          raise EDbfError.Create(STRING_RECORD_LOCKED);
        try
          Locked := LockPage(RecNo, False);
        finally
          UnlockPage(0);
        end;
      finally
        UnlockPage(-2);
      end;
    end
    else
      Locked := LockPage(RecNo, False);
  end
  else
    Locked := True;
  if Locked then
  begin
    // reread data
    if Resync then
      if ResyncSharedReadBuffer then
        ReadRecord(RecNo, Buffer);
    // store previous data for updating indexes
    Move(Buffer^, FPrevBuffer^, RecordSize);
    // lock succeeded, update lock info, if field present
    if FLockField <> nil then
    begin
      // update buffer
      WriteLockInfo(Buffer);
      // write to disk
      WriteRecord(RecNo, Buffer);
    end;
  end
  else
    raise EDbfError.Create(STRING_RECORD_LOCKED);
end;

procedure TDbfFile.UnlockRecord(RecNo: Integer; Buffer: TDbfRecordBuffer);
var
  I: Integer;
  lIndex, lErrorIndex: TIndexFile;
begin
  // update indexes, possible key violation
  I := 0;
  while I < FIndexFiles.Count do
  begin
    lIndex := TIndexFile(FIndexFiles.Items[I]);
    if not lIndex.Update(RecNo, FPrevBuffer, Buffer) then
    begin
      // error -> rollback
      lErrorIndex := lIndex;
      while I > 0 do
      begin
        Dec(I);
        lIndex := TIndexFile(FIndexFiles.Items[I]);
        lIndex.Update(RecNo, Buffer, FPrevBuffer);
      end;
      lErrorIndex.InsertError;
    end;
    Inc(I);
  end;
  // write new record buffer, all keys ok
  WriteRecord(RecNo, Buffer);
  // done updating, unlock
  if NeedLocks and (not FileLocked) then
    UnlockPage(RecNo);
end;

procedure TDbfFile.DeleteIndexFile(AIndexFile: TIndexFile);
var
  Index: Integer;
begin
  for Index:= Pred(FIndexNames.Count) downto 0 do
    if FIndexNames.Objects[Index]=AIndexFile then
      FIndexNames.Delete(Index);
  FIndexFiles.Remove(AIndexFile);
  AIndexFile.Close;
  AIndexFile.DeleteFile;
  if FMdxFile = AIndexFile then
    FMdxFile := nil;
  AIndexFile.Free;
end;

procedure TDbfFile.Flush;
begin
  FlushBuffer;
  FlushHeader;
  inherited;
end;

procedure TDbfFile.SetRecordSize(NewSize: Integer);
begin
  if NewSize <> RecordSize then
  begin
    if FPrevBuffer <> nil then
      FreeMemAndNil(Pointer(FPrevBuffer));

    if NewSize > 0 then
      GetMem(FPrevBuffer, NewSize);
  end;
  inherited;
end;

function TDbfFile.GetIndexByName(AIndexName: string): TIndexFile;
var
  I: Integer;
begin
  I := FIndexNames.IndexOf(AIndexName);
  if I >= 0 then
    Result := TIndexFile(FIndexNames.Objects[I])
  else
    Result := nil;
end;

//====================================================================
// TDbfCursor
//====================================================================
constructor TDbfCursor.Create(DbfFile: TDbfFile);
begin
  inherited Create(DbfFile);
end;

function TDbfCursor.Next: Boolean;
begin
  if TDbfFile(PagedFile).IsRecordPresent(FPhysicalRecNo) then
  begin
    inc(FPhysicalRecNo);
    Result := TDbfFile(PagedFile).IsRecordPresent(FPhysicalRecNo);
  end else begin
    FPhysicalRecNo := TDbfFile(PagedFile).CachedRecordCount + 1;
    Result := false;
  end;
end;

function TDbfCursor.Prev: Boolean;
begin
  if FPhysicalRecNo > 0 then
    dec(FPhysicalRecNo)
  else
    FPhysicalRecNo := 0;
  Result := FPhysicalRecNo > 0;
end;

procedure TDbfCursor.First;
begin
  FPhysicalRecNo := 0;
end;

procedure TDbfCursor.Last;
var
  max: Integer;
begin
  max := TDbfFile(PagedFile).RecordCount;
  if max = 0 then
    FPhysicalRecNo := 0
  else
    FPhysicalRecNo := max + 1;
end;

function TDbfCursor.GetPhysicalRecNo: Integer;
begin
  Result := FPhysicalRecNo;
end;

procedure TDbfCursor.SetPhysicalRecNo(RecNo: Integer);
begin
  FPhysicalRecNo := RecNo;
end;

function TDbfCursor.GetSequentialRecordCount: TSequentialRecNo;
begin
  Result := TDbfFile(PagedFile).RecordCount;
end;

function TDbfCursor.GetSequentialRecNo: TSequentialRecNo;
begin
  Result := FPhysicalRecNo;
end;

procedure TDbfCursor.SetSequentialRecNo(RecNo: TSequentialRecNo);
begin
  FPhysicalRecNo := RecNo;
end;

// codepage enumeration procedure
var
  TempCodePageList: TList;

// LPTSTR = PWideChar for Unicode, PAnsiChar otherwise
// PChar = PWideChar for Unicode, PAnsiChar otherwise

function CodePagesProc(CodePageString: PChar): Cardinal; stdcall; // PChar intended here
{$IFNDEF WINAPI_IS_UNICODE}
var
  IntValue: Integer;
{$ENDIF}
begin
  // add codepage to list
{$IFDEF WINAPI_IS_UNICODE}
  TempCodePageList.Add(Pointer(StrToIntDef(string(CodePageString), -1))); // Avoid conversion to AnsiString
{$ELSE}
  IntValue := 0;
  if StrToInt32Width(IntValue, CodePageString, dbfStrLen(CodePageString), -1) then
    TempCodePageList.Add({%H-}Pointer(IntValue));
{$ENDIF}

  // continue enumeration
  Result := 1;
end;

//====================================================================
// TDbfGlobals
//====================================================================
constructor TDbfGlobals.Create;
begin
  FCodePages := TList.Create;
  FDefaultOpenCodePage := GetACP;
  // the following sets FDefaultCreateLangId
  DefaultCreateCodePage := GetACP;

  FCurrencyAsBCD := true;
  // determine which code pages are installed
  TempCodePageList := FCodePages;
  EnumSystemCodePages(@CodePagesProc, {CP_SUPPORTED} CP_INSTALLED);
  TempCodePageList := nil;
  InitUserName;
end;

procedure TDbfGlobals.InitUserName;
{$ifdef FPC}
{$ifndef WINDOWS}
var
  TempName: UTSName;
{$endif}
{$endif}
begin
{$ifdef WINDOWS}
  FUserNameLen := MAX_COMPUTERNAME_LENGTH+1;
  SetLength(FUserName, FUserNameLen);
  Windows.GetComputerName(PChar(FUserName), // PChar intended here, corresponds with "FUserName: string"
    {$ifdef DELPHI_3}Windows.DWORD({$endif}
      FUserNameLen
    {$ifdef DELPHI_3}){$endif}
    );
  SetLength(FUserName, FUserNameLen);
{$else}  
{$ifdef FPC}
  FpUname(TempName);
  FUserName := TempName.machine;
  FUserNameLen := Length(FUserName);
{$endif}  
{$endif}
end;

destructor TDbfGlobals.Destroy; {override;}
begin
  FCodePages.Free;
end;

function TDbfGlobals.GetDefaultCreateCodePage: Integer;
begin
  Result := LangId_To_CodePage[FDefaultCreateLangId];
end;

procedure TDbfGlobals.SetDefaultCreateCodePage(NewCodePage: Integer);
begin
  FDefaultCreateLangId := ConstructLangId(NewCodePage, GetUserDefaultLCID, false);
end;

function TDbfGlobals.CodePageInstalled(ACodePage: Integer): Boolean;
begin
  Result := FCodePages.IndexOf({%H-}Pointer(ACodePage)) >= 0;
end;

{$ifdef SUPPORT_FORMATSETTINGSTYPE}
function GetUserDefaultLocaleSettings: TFormatSettings;
begin
{$ifdef SUPPORT_FORMATSETTINGS_CREATE}
  Result := TFormatSettings.Create('');
{$else}
//  Result := TFormatSettings.Create(GetUserDefaultLCID);
{$ifdef FPC}
  Result := FormatSettings;
{$else}
  GetLocaleFormatSettings(GetUserDefaultLCID, Result);
{$endif}
{$endif}
end;
{$endif SUPPORT_FORMATSETTINGSTYPE}

initialization
  DbfGlobals := TDbfGlobals.Create;
finalization
  FreeAndNil(DbfGlobals);


(*
  Stuffs non implemented yet
  TFoxCDXHeader         = Record
    PointerRootNode     : Integer;
    PointerFreeList     : Integer;
    Reserved_8_11       : Cardinal;
    KeyLength           : Word;
    IndexOption         : Byte;
    IndexSignature      : Byte;
    Reserved_Null       : TFoxReservedNull;
    SortOrder           : Word;
    TotalExpressionLen  : Word;
    ForExpressionLen    : Word;
    Reserved_506_507    : Word;
    KeyExpressionLen    : Word;
    KeyForExpression    : TKeyForExpression;
  End;
  PFoxCDXHeader         = ^TFoxCDXHeader;

  TFoxCDXNodeCommon     = Record
    NodeAttributes      : Word;
    NumberOfKeys        : Word;
    PointerLeftNode     : Integer;
    PointerRightNode    : Integer;
  End;

  TFoxCDXNodeNonLeaf    = Record
    NodeCommon          : TFoxCDXNodeCommon;
    TempBlock           : Array [12..511] of Byte;
  End;
  PFoxCDXNodeNonLeaf    = ^TFoxCDXNodeNonLeaf;

  TFoxCDXNodeLeaf       = Packed Record
    NodeCommon          : TFoxCDXNodeCommon;
    BlockFreeSpace      : Word;
    RecordNumberMask    : Integer;
    DuplicateCountMask  : Byte;
    TrailByteCountMask  : Byte;
    RecNoBytes          : Byte;
    DuplicateCountBytes : Byte;
    TrailByteCountBytes : Byte;
    HoldingByteCount    : Byte;
    DataBlock           : TDataBlock;
  End;
  PFoxCDXNodeLeaf       = ^TFoxCDXNodeLeaf;

*)

end.

