unit dbf_pgfile;

interface

{$I dbf_common.inc}

uses
  Classes,
  SysUtils,
  dbf_common;

type
  EPagedFile = class(Exception)
  end;

  TPagedFileMode = (pfNone, pfMemoryCreate, pfMemoryOpen, pfExclusiveCreate,
    pfExclusiveOpen, pfReadWriteCreate, pfReadWriteOpen, pfReadOnly);

  // access levels:
  //
  // - memory            create
  // - exclusive         create/open
  // - read/write        create/open
  // - readonly                 open
  //
  // - memory            -*-share: N/A          -*-locks: disabled    -*-indexes: read/write
  // - exclusive_create  -*-share: exclusive    -*-locks: disabled    -*-indexes: read/write
  // - exclusive_open    -*-share: exclusive    -*-locks: disabled    -*-indexes: read/write
  // - readwrite_create  -*-share: deny none    -*-locks: enabled     -*-indexes: read/write
  // - readwrite_open    -*-share: deny none    -*-locks: enabled     -*-indexes: read/write
  // - readonly          -*-share: deny none    -*-locks: disabled    -*-indexes: readonly

  {$ifdef SUPPORT_INT64}
    TPagedFileOffset = Int64;
  {$else}
    TPagedFileOffset = Cardinal;
  {$endif}
  TPagedFileProgressEvent = procedure(Sender: TObject; Position, Max: Integer; var Aborted: Boolean; Msg: string) of object;

  TPagedFileStream = class(TFileStream)
  protected
{$ifdef SUPPORT_INT64}
    procedure SetSize(NewSize: Longint); override;
{$else}
    procedure SetSize(const NewSize: Int64); override;
{$endif}
  public
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
{$ifdef SUPPORT_INT64_SEEK}
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
{$else}
    function Seek(Offset: Longint; Origin: Word): Longint; override;
{$endif}
  end;

  TPagedFile = class(TObject)
  protected
    FStream: TStream;
    FHeaderOffset: Integer;
    FHeaderSize: Integer;
    FRecordSize: Integer;
    FPageSize: Integer;         { need for MDX, where recordsize <> pagesize }
    FRecordCount: Integer;      { actually FPageCount, but we want to keep existing code }
    FPagesPerRecord: Integer;
    FCachedSize: TPagedFileOffset;
    FCachedRecordCount: Integer;
    FHeader: PAnsiChar;
    FActive: Boolean;
    FNeedRecalc: Boolean;
    FHeaderModified: Boolean;
    FPageOffsetByHeader: Boolean;   { do pages start after header or just at BOF? }
    FMode: TPagedFileMode;
    FTempMode: TPagedFileMode;
    FUserMode: TPagedFileMode;
    FAutoCreate: Boolean;
    FNeedLocks: Boolean;
    FVirtualLocks: Boolean;
    FFileLocked: Boolean;
    FFileName: string;
    FBufferPtr: Pointer;
    FBufferAhead: Boolean;
    FBufferPage: Integer;
    FBufferOffset: TPagedFileOffset;
    FBufferSize: Integer;
    FBufferReadSize: Integer;
    FBufferMaxSize: Integer;
    FBufferModified: Boolean;
    FWriteError: Boolean;
    FOnProgress: TPagedFileProgressEvent;
    FCompatibleLockOffset: Boolean;
  protected
    procedure SetHeaderOffset(NewValue: Integer); virtual;
    procedure SetRecordSize(NewValue: Integer); virtual;
    procedure SetHeaderSize(NewValue: Integer); virtual;
    procedure SetPageSize(NewValue: Integer);
    procedure SetPageOffsetByHeader(NewValue: Boolean); virtual;
    procedure SetRecordCount(NewValue: Integer);
    procedure SetBufferAhead(NewValue: Boolean);
    procedure SetFileName(NewName: string);
    procedure SetStream(NewStream: TStream);
    function  LockSection(const Offset: TPagedFileOffset; const Length: Cardinal; const Wait: Boolean): Boolean; virtual;
    function  UnlockSection(const Offset: TPagedFileOffset; const Length: Cardinal): Boolean; virtual;
    procedure UpdateBufferSize;
    procedure RecalcPagesPerRecord;
    function ReadHeader: Integer;
    procedure FlushHeader;
    procedure FlushBuffer;
    function  ReadChar: Byte;
    procedure WriteChar(c: Byte);
    procedure CheckCachedSize(const APosition: TPagedFileOffset);
    procedure SynchronizeBuffer(IntRecNum: Integer);
    function  ReadBuffer: Boolean;
    function  Read(Buffer: Pointer; ASize: Integer): Integer;
    function  ReadBlock(const BlockPtr: Pointer; const ASize: Integer; const APosition: TPagedFileOffset): Integer;
    function  SingleReadRecord(IntRecNum: Integer; Buffer: Pointer): Integer;
    procedure WriteBlock(const BlockPtr: Pointer; const ASize: Integer; const APosition: TPagedFileOffset);
    procedure SingleWriteRecord(IntRecNum: Integer; Buffer: Pointer);
    function  GetRecordCount: Integer;
    procedure UpdateCachedSize(CurrPos: TPagedFileOffset);

    property VirtualLocks: Boolean read FVirtualLocks write FVirtualLocks;
  public
    constructor Create;
    destructor Destroy; override;

    procedure CloseFile; virtual;
    procedure OpenFile; virtual;
    procedure DeleteFile;
    procedure TryExclusive; virtual;
    procedure EndExclusive; virtual;
    procedure CheckExclusiveAccess;
    procedure DisableForceCreate;
    function  CalcLockOffset(const PageNo: Integer): TPagedFileOffset;
    function  CalcPageOffset(const PageNo: Integer): TPagedFileOffset;
    function  IsRecordPresent(IntRecNum: Integer): boolean;
    function  ReadRecord(IntRecNum: Integer; Buffer: Pointer): Integer; virtual;
    procedure WriteRecord(IntRecNum: Integer; Buffer: Pointer); virtual;
    procedure WriteHeader; virtual;
    function  FileCreated: Boolean;
    function  IsSharedAccess: Boolean;
    procedure ResetError; virtual;
    function  ResyncSharedReadBuffer: Boolean;
    function  ResyncSharedFlushBuffer: Boolean;
    procedure DoProgress(Position, Max: Integer; Msg: string);

    function  LockPage(const PageNo: Integer; const Wait: Boolean): Boolean;
    function  LockAllPages(const Wait: Boolean): Boolean;
    procedure UnlockPage(const PageNo: Integer);
    procedure UnlockAllPages;

    procedure Flush; virtual;

    property Active: Boolean read FActive;
    property AutoCreate: Boolean read FAutoCreate write FAutoCreate;   // only write when closed!
    property Mode: TPagedFileMode read FMode write FMode;              // only write when closed!
    property TempMode: TPagedFileMode read FTempMode;
    property NeedLocks: Boolean read FNeedLocks;
    property HeaderOffset: Integer read FHeaderOffset write SetHeaderOffset;
    property HeaderSize: Integer read FHeaderSize write SetHeaderSize;
    property RecordSize: Integer read FRecordSize write SetRecordSize;
    property PageSize: Integer read FPageSize write SetPageSize;
    property PagesPerRecord: Integer read FPagesPerRecord;
    property RecordCount: Integer read GetRecordCount write SetRecordCount;
    property CachedRecordCount: Integer read FCachedRecordCount;
    property PageOffsetByHeader: Boolean read FPageOffsetbyHeader write SetPageOffsetByHeader;
    property FileLocked: Boolean read FFileLocked;
    property Header: PAnsiChar read FHeader;
    property FileName: string read FFileName write SetFileName;
    property Stream: TStream read FStream write SetStream;
    property BufferAhead: Boolean read FBufferAhead write SetBufferAhead;
    property WriteError: Boolean read FWriteError;
    property OnProgress: TPagedFileProgressEvent read FOnProgress write FOnProgress;
    property CompatibleLockOffset: Boolean read FCompatibleLockOffset {$ifdef SUPPORT_INT64}write FCompatibleLockOffset{$endif};
  end;

implementation

uses
{$ifdef WINDOWS}
  Windows,
{$ifdef KYLIX}
  Libc, 
{$endif}
{$IFDEF Delphi_7}
  Types,
{$ENDIF}
{$else}
  dbf_wtil,
{$endif}
  dbf_str;

//====================================================================
// TPagedFileStream
//====================================================================
function TPagedFileStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result := FileRead(Handle,Buffer,Count);
  if Result = -1 then
    raise EPagedFile.Create(STRING_READ_ERROR);
end;

function TPagedFileStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result:=FileWrite (Handle,Buffer,Count);
  if Result = -1 then
    raise EPagedFile.Create(STRING_WRITE_ERROR);
end;

{$ifdef SUPPORT_INT64_SEEK}
function TPagedFileStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  Result := FileSeek(Handle, Offset, Ord(Origin));
  if Result = -1 then
    raise EPagedFile.Create(STRING_READ_ERROR);
end;
{$else}
function TPagedFileStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  Result := FileSeek(Handle, Offset, Ord(Origin));
end;
{$endif}

{$ifdef SUPPORT_INT64}
procedure TPagedFileStream.SetSize(NewSize: Longint);
begin
{$ifdef FPC_VERSION}
  if not FileTruncate(Handle, NewSize) then
    raise EPagedFile.Create(STRING_WRITE_ERROR);
{$else}
  inherited;
{$endif}
end;
{$else}
procedure TPagedFileStream.SetSize(const NewSize: Int64);
begin
{$ifdef FPC_VERSION}
  if not FileTruncate(Handle, NewSize) then
    raise EPagedFile.Create(STRING_WRITE_ERROR);
{$else}
  inherited;
{$endif}
end;
{$endif}

//====================================================================
// TPagedFile
//====================================================================
constructor TPagedFile.Create;
begin
  FFileName := EmptyStr;
  FHeaderOffset := 0;
  FHeaderSize := 0;
  FRecordSize := 0;
  FRecordCount := 0;
  FPageSize := 0;
  FPagesPerRecord := 0;
  FActive := false;
  FHeaderModified := false;
  FPageOffsetByHeader := true;
  FNeedLocks := false;
  FMode := pfReadOnly;
  FTempMode := pfNone;
  FAutoCreate := false;
  FVirtualLocks := true;
  FFileLocked := false;
  FCompatibleLockOffset := True;
  FHeader := nil;
  FBufferPtr := nil;
  FBufferAhead := false;
  FBufferModified := false;
  FBufferSize := 0;
  FBufferMaxSize := 0;
  FBufferOffset := 0;
  FWriteError := false;

  inherited;
end;

destructor TPagedFile.Destroy;
begin
  // close physical file
  if FFileLocked then UnlockAllPages;
  CloseFile;
  FFileLocked := false;

  // free mem
  if FHeader <> nil then
    FreeMem(FHeader);

  inherited;
end;

procedure TPagedFile.OpenFile;
var
  fileOpenMode: Word;
begin
  if FActive then exit;  

  // store user specified mode
  FUserMode := FMode;
  if not (FMode in [pfMemoryCreate, pfMemoryOpen]) then
  begin
    // test if file exists
    if not FileExists(FFileName) then
    begin
      // if auto-creating, adjust mode
      if FAutoCreate then case FMode of
        pfExclusiveOpen:             FMode := pfExclusiveCreate;
        pfReadWriteOpen, pfReadOnly: FMode := pfReadWriteCreate;
      end;
      // it seems the VCL cannot share a file that is created?
      // create file first, then open it in requested mode
      // filecreated means 'to be created' in this context ;-)
      if FileCreated then
        FileClose(FileCreate(FFileName))
      else
        raise EPagedFile.CreateFmt(STRING_FILE_NOT_FOUND,[FFileName]);
    end;
    // specify open mode
    case FMode of
      pfExclusiveCreate: fileOpenMode := fmOpenReadWrite or fmShareExclusive;
      pfExclusiveOpen:   fileOpenMode := fmOpenReadWrite or fmShareExclusive;
      pfReadWriteCreate: fileOpenMode := fmOpenReadWrite or fmShareDenyNone;
      pfReadWriteOpen:   fileOpenMode := fmOpenReadWrite or fmShareDenyNone;
    else    // => readonly
                         fileOpenMode := fmOpenRead or fmShareDenyNone;
    end;
    // open file
    FStream := TPagedFileStream.Create(FFileName, fileOpenMode);
    // if creating, then empty file
    if FileCreated then
      FStream.Size := 0;
  end else begin
    if FStream = nil then
    begin
      FMode := pfMemoryCreate;
      FStream := TMemoryStream.Create;
    end;
  end;
  // init size var
  FCachedSize := Stream.Size;
  // update whether we need locking
{$ifdef _DEBUG}
  FNeedLocks := true;
{$else}
  FNeedLocks := IsSharedAccess;
{$endif}
  FActive := true;
  // allocate memory for bufferahead
  UpdateBufferSize;
end;

procedure TPagedFile.CloseFile;
begin
  if FActive then
  begin
    FlushHeader;
    FlushBuffer;
    // don't free the user's stream
    if not (FMode in [pfMemoryOpen, pfMemoryCreate]) then
      FreeAndNil(FStream);
    // free bufferahead buffer
    FreeMemAndNil(FBufferPtr);

    // mode possibly overridden in case of auto-created file
    FMode := FUserMode;
    FActive := false;
    FCachedRecordCount := 0;
  end;
end;

procedure TPagedFile.DeleteFile;
begin
  // opened -> we can not delete
  if not FActive then
    SysUtils.DeleteFile(FileName);
end;

function TPagedFile.FileCreated: Boolean;
const
  CreationModes: array [pfNone..pfReadOnly] of Boolean =
    (false, true, false, true, false, true, false, false);
//   node, memcr, memop, excr, exopn, rwcr, rwopn, rdonly
begin
  Result := CreationModes[FMode];
end;

function TPagedFile.IsSharedAccess: Boolean;
const
  SharedAccessModes: array [pfNone..pfReadOnly] of Boolean =
    (false, false, false, false, false, true, true,  true);
//   node,  memcr, memop, excr,  exopn, rwcr, rwopn, rdonly
begin
  Result := SharedAccessModes[FMode];
end;

procedure TPagedFile.CheckExclusiveAccess;
begin
  // in-memory => exclusive access!
  if IsSharedAccess then
    raise EDbfError.Create(STRING_NEED_EXCLUSIVE_ACCESS);
end;

function TPagedFile.CalcLockOffset(const PageNo: Integer): TPagedFileOffset;
begin
  // Use the lock offset compatible with the BDE
  Result := Cardinal($EFFFFFFE);

  // Use a lock offset that does not conflict with the physical file
  {$ifdef SUPPORT_INT64}
  if not FCompatibleLockOffset then
    Result := -3; // $FFFFFFFFFFFFFFFD
  {$endif}

  // Calculate lock offset for page
  if PageNo >= 0 then
    Result := Result - Cardinal(PageNo)
  else
    Result := Result + Cardinal(-PageNo);
end;

function TPagedFile.CalcPageOffset(const PageNo: Integer): TPagedFileOffset;
begin
  if not FPageOffsetByHeader then
    Result := TPagedFileOffset(FPageSize) * PageNo
  else if PageNo = 0 then
    Result := 0
  else
    Result := TPagedFileOffset(FHeaderOffset) + FHeaderSize + (TPagedFileOffset(FPageSize) * (TPagedFileOffset(PageNo) - 1));
end;

procedure TPagedFile.CheckCachedSize(const APosition: TPagedFileOffset);
begin
  // file expanded?
  if APosition > FCachedSize then
  begin
    FCachedSize := APosition;
    FNeedRecalc := true;
  end;
end;

function TPagedFile.Read(Buffer: Pointer; ASize: Integer): Integer;
begin
  // if we cannot read due to a lock, then wait a bit
  repeat
    Result := FStream.Read(Buffer^, ASize);
    if Result = 0 then
    begin
      // translation to linux???
      if GetLastError = ERROR_LOCK_VIOLATION then
      begin
        // wait a bit until block becomes available
        Sleep(1);
      end else begin
        // return empty block
        exit;
      end;
    end else
      exit;
  until false;
end;

procedure TPagedFile.UpdateCachedSize(CurrPos: TPagedFileOffset);
begin
  // have we added a record?
  if CurrPos > FCachedSize then
  begin
    // update cached size, always at end
    repeat
      Inc(FCachedSize, FRecordSize);
      Inc(FRecordCount, PagesPerRecord);
    until FCachedSize >= CurrPos;
  end;
end;

procedure TPagedFile.FlushBuffer;
begin
  if FBufferAhead and FBufferModified then
  begin
    WriteBlock(FBufferPtr, FBufferSize, FBufferOffset);
    FBufferModified := false;
  end;
end;

function TPagedFile.SingleReadRecord(IntRecNum: Integer; Buffer: Pointer): Integer;
begin
  Result := ReadBlock(Buffer, RecordSize, CalcPageOffset(IntRecNum));
end;

procedure TPagedFile.SingleWriteRecord(IntRecNum: Integer; Buffer: Pointer);
begin
  WriteBlock(Buffer, RecordSize, CalcPageOffset(IntRecNum));
end;

procedure TPagedFile.SynchronizeBuffer(IntRecNum: Integer);
var
  BufferPageMin: Integer;
begin
  // record outside buffer, flush previous buffer
  FlushBuffer;
  // read new set of records
  if (FBufferPage >= 0) and ((IntRecNum < Pred(FBufferPage)) or (IntRecNum > FBufferPage + (FBufferSize div PageSize))) then // 10/28/2011 pb  CR 19176
    FBufferReadSize := RecordSize
  else
    FBufferReadSize := FBufferMaxSize;
  if IntRecNum < FBufferPage then
  begin
    if FPageOffsetByHeader then
      BufferPageMin := 1
    else
      BufferPageMin := 0;
    FBufferPage := (IntRecNum + 1) - (FBufferReadSize div PageSize);
    if FBufferPage < BufferPageMin then
    begin
      Dec(FBufferReadSize, (BufferPageMin - FBufferPage) * PageSize);
      FBufferPage := BufferPageMin;
    end;
  end
  else
    FBufferPage := IntRecNum;
  FBufferOffset := CalcPageOffset(FBufferPage);
  if FBufferOffset + FBufferReadSize > FCachedSize then
    FBufferReadSize := FCachedSize - FBufferOffset;
  FBufferSize := FBufferReadSize;
  ReadBuffer;
end;

function TPagedFile.ReadBuffer: Boolean;
begin
  Result := FBufferAhead and (FBufferReadSize <> 0);
  if Result then
    FBufferReadSize := ReadBlock(FBufferPtr, FBufferReadSize, FBufferOffset);
end;

function TPagedFile.IsRecordPresent(IntRecNum: Integer): boolean;
begin
  // if in shared mode, recordcount can only increase, check if recordno
  // in range for cached recordcount
  if not IsSharedAccess or (IntRecNum > FCachedRecordCount) then
    FCachedRecordCount := RecordCount;
  Result := (0 <= IntRecNum) and (IntRecNum <= FCachedRecordCount);
end;

function TPagedFile.ReadRecord(IntRecNum: Integer; Buffer: Pointer): Integer;
var
  Offset: TPagedFileOffset;
begin
  if FBufferAhead then
  begin
    Offset := (TPagedFileOffset(IntRecNum) - FBufferPage) * PageSize;
    if (FBufferPage <> -1) and (FBufferPage <= IntRecNum) and
        (Offset + RecordSize <= FBufferSize) then
    begin
      // have record in buffer, nothing to do here
    end else begin
      // need to update buffer
      SynchronizeBuffer(IntRecNum);
      // check if enough bytes read
      if RecordSize > FBufferReadSize then
      begin
        Result := 0;
        exit;
      end;
      // reset offset into buffer
      Offset := (TPagedFileOffset(IntRecNum) - FBufferPage) * PageSize;
    end;
    // now we have this record in buffer
    Move(PAnsiChar(FBufferPtr)[Offset], Buffer^, RecordSize); // Was PChar
    // successful
    Result := RecordSize;
  end else begin
    // no buffering
    Result := SingleReadRecord(IntRecNum, Buffer);
  end;
end;

procedure TPagedFile.WriteRecord(IntRecNum: Integer; Buffer: Pointer);
var
  RecEnd: TPagedFileOffset;
begin
  if FBufferAhead then
  begin
    RecEnd := (TPagedFileOffset(IntRecNum) - FBufferPage + PagesPerRecord) * PageSize;
    if (FBufferPage <> -1) and (FBufferPage <= IntRecNum) and
        (RecEnd <= FBufferMaxSize) and (RecEnd <= TPagedFileOffset(FBufferSize) + RecordSize) then
    begin
      // extend buffer?
      if RecEnd > FBufferSize then
        FBufferSize := RecEnd;
    end else begin
      // record outside buffer, need to synchronize first
      SynchronizeBuffer(IntRecNum);
      RecEnd := TPagedFileOffset(PagesPerRecord) * PageSize;
      FBufferSize := RecEnd;
    end;
    // we can write this record to buffer
    Move(Buffer^, PAnsiChar(FBufferPtr)[RecEnd-RecordSize], RecordSize);
    FBufferModified := true;
    // update cached size
    UpdateCachedSize(FBufferOffset+RecEnd);
    ResyncSharedFlushBuffer;
  end else begin
    // no buffering
    SingleWriteRecord(IntRecNum, Buffer);
    // update cached size
    UpdateCachedSize(FStream.Position);
  end;
end;

procedure TPagedFile.SetBufferAhead(NewValue: Boolean);
begin
  if FBufferAhead <> NewValue then
  begin
    FlushBuffer;
    FBufferAhead := NewValue;
    UpdateBufferSize;
  end;
end;

procedure TPagedFile.SetStream(NewStream: TStream);
begin
  if not FActive then
    FStream := NewStream;
end;

procedure TPagedFile.SetFileName(NewName: string);
begin
  if not FActive then
    FFileName := NewName;
end;

procedure TPagedFile.UpdateBufferSize;
begin
  if FBufferAhead then
  begin
    FBufferMaxSize := 65536;
    if RecordSize <> 0 then
      Dec(FBufferMaxSize, FBufferMaxSize mod RecordSize);
    if FBufferMaxSize < RecordSize then
      FBufferMaxSize := RecordSize;
  end else begin
    FBufferMaxSize := 0;
  end;

  if FBufferPtr <> nil then
    FreeMem(FBufferPtr);
  if FBufferAhead and (FBufferMaxSize <> 0) then
    GetMem(FBufferPtr, FBufferMaxSize)
  else
    FBufferPtr := nil;
  FBufferPage := -1;
  FBufferOffset := -1;
  FBufferModified := false;
end;

procedure TPagedFile.WriteHeader;
begin
  FHeaderModified := true;
  if FNeedLocks then
    FlushHeader;
end;

procedure TPagedFile.FlushHeader;
begin
  if FHeaderModified then
  begin
    FStream.Position := FHeaderOffset;
    FWriteError := (FStream.Write(FHeader^, FHeaderSize) = 0) or FWriteError;
    // test if written new header
    if FStream.Position > FCachedSize then
    begin
      // new header -> record count unknown
      FCachedSize := FStream.Position;
      FNeedRecalc := true;
    end;
    FHeaderModified := false;
  end;
end;

function TPagedFile.ReadHeader: Integer;
   { assumes header is large enough }
var
  size: TPagedFileOffset;
begin
  // save changes before reading new header
  FlushHeader;
  // check if header length zero
  if FHeaderSize <> 0 then
  begin
    // get size left in file for header
    size := FStream.Size - FHeaderOffset;
    // header start before EOF?
    if size >= 0 then
    begin
      // go to header start
      FStream.Position := FHeaderOffset;
      // whole header in file?
      if size >= FHeaderSize then
      begin
        // read header, nothing to be cleared
        Read(FHeader, FHeaderSize);
        size := FHeaderSize;
      end else begin
        // read what we can, clear rest
        Read(FHeader, size);
      end;
    end else begin
      // header start before EOF, clear header
      size := 0;
    end;
    FillChar(FHeader[size], FHeaderSize-size, 0);
    Result := size;
  end
  else
    Result := 0;
end;

procedure TPagedFile.TryExclusive;
const NewTempMode: array[pfReadWriteCreate..pfReadOnly] of TPagedFileMode =
    (pfReadWriteOpen, pfReadWriteOpen, pfReadOnly);
begin
  // already in temporary exclusive mode?
  if (FTempMode = pfNone) and IsSharedAccess then
  begin
    // save temporary mode, if now creating, then reopen non-create
    FTempMode := NewTempMode[FMode];
    // try exclusive mode
    CloseFile;
    FMode := pfExclusiveOpen;
    try
      OpenFile;
    except
      on EFOpenError do
      begin
        // we failed, reopen normally
        EndExclusive;
      end;
    end;
  end;
end;

procedure TPagedFile.EndExclusive;
begin
  // are we in temporary file mode?
  if FTempMode <> pfNone then
  begin
    CloseFile;
    FMode := FTempMode;
    FTempMode := pfNone;
    OpenFile;
  end;
end;

procedure TPagedFile.DisableForceCreate;
begin
  case FMode of
    pfExclusiveCreate: FMode := pfExclusiveOpen;
    pfReadWriteCreate: FMode := pfReadWriteOpen;
  end;
end;

procedure TPagedFile.SetHeaderOffset(NewValue: Integer);
//
// *) assumes is called right before SetHeaderSize
//
begin
  if FHeaderOffset <> NewValue then
  begin
    FlushHeader;
    FHeaderOffset := NewValue;
  end;
end;

procedure TPagedFile.SetHeaderSize(NewValue: Integer);
begin
  if FHeaderSize <> NewValue then
  begin
    FlushHeader;
    if (FHeader <> nil) and (NewValue <> 0) then
      FreeMem(FHeader);
    FHeaderSize := NewValue;
    if FHeaderSize <> 0 then
      GetMem(FHeader, FHeaderSize);
    FNeedRecalc := true;
    ReadHeader;
  end;
end;

procedure TPagedFile.SetRecordSize(NewValue: Integer);
begin
  if FRecordSize <> NewValue then
  begin
    FRecordSize := NewValue;
    FPageSize := NewValue;
    FNeedRecalc := true;
    RecalcPagesPerRecord;
  end;
end;

procedure TPagedFile.SetPageSize(NewValue: Integer);
begin
  if FPageSize <> NewValue then
  begin
    FPageSize := NewValue;
    FNeedRecalc := true;
    RecalcPagesPerRecord;
    UpdateBufferSize;
  end;
end;

procedure TPagedFile.RecalcPagesPerRecord;
begin
  if FPageSize = 0 then
    FPagesPerRecord := 0
  else
    FPagesPerRecord := FRecordSize div FPageSize;
end;

function TPagedFile.GetRecordCount: Integer;
var
  currSize: TPagedFileOffset;
begin
  // file size changed?
  if FNeedLocks then
  begin
    currSize := FStream.Size;
    if currSize <> FCachedSize then
    begin
      FCachedSize := currSize;
      FNeedRecalc := true;
    end;
  end;

  // try to optimize speed
  if FNeedRecalc then
  begin
    // no file? test flags
    if (FPageSize = 0) or not FActive then
      FRecordCount := 0
    else
    if FPageOffsetByHeader then
      FRecordCount := (FCachedSize - FHeaderSize - FHeaderOffset) div FPageSize
    else
      FRecordCount := FCachedSize div FPageSize;
    if FRecordCount < 0 then
      FRecordCount := 0;

    // count updated
    FNeedRecalc := false;
  end;
  Result := FRecordCount;
end;

procedure TPagedFile.SetRecordCount(NewValue: Integer);
begin
  if RecordCount <> NewValue then
  begin
    if FPageOffsetByHeader then
      FCachedSize := FHeaderSize + TPagedFileOffset(FHeaderOffset) + (TPagedFileOffset(FPageSize) * NewValue)
    else
      FCachedSize := TPagedFileOffset(FPageSize) * NewValue;
    FRecordCount := NewValue;
    FStream.Size := FCachedSize;
  end;
end;

procedure TPagedFile.SetPageOffsetByHeader(NewValue: Boolean);
begin
  if FPageOffsetByHeader <> NewValue then
  begin
    FPageOffsetByHeader := NewValue;
    FNeedRecalc := true;
  end;
end;

procedure TPagedFile.WriteChar(c: Byte);
begin
  FWriteError := (FStream.Write(c, 1) = 0) or FWriteError;
end;

function TPagedFile.ReadChar: Byte;
begin
  Read(@Result, 1);
end;

procedure TPagedFile.Flush;
begin
end;

function TPagedFile.ReadBlock(const BlockPtr: Pointer; const ASize: Integer; const APosition: TPagedFileOffset): Integer;
begin
  FStream.Position := APosition;
  CheckCachedSize(APosition);
  Result := Read(BlockPtr, ASize);
end;

procedure TPagedFile.WriteBlock(const BlockPtr: Pointer; const ASize: Integer; const APosition: TPagedFileOffset);
  // assumes a lock is held if necessary prior to calling this function
begin
  FStream.Position := APosition;
  CheckCachedSize(APosition);
  FWriteError := (FStream.Write(BlockPtr^, ASize) = 0) or FWriteError;
end;

procedure TPagedFile.ResetError;
begin
  FWriteError := false;
end;

function TPagedFile.ResyncSharedReadBuffer: Boolean;
begin
  Result := IsSharedAccess and (not FFileLocked);
  if Result then
    Result := ReadBuffer;
end;

function TPagedFile.ResyncSharedFlushBuffer: Boolean;
begin
  Result := IsSharedAccess;
  if Result then
    FlushBuffer;
end;

procedure TPagedFile.DoProgress(Position, Max: Integer; Msg: string);
var
  Aborted: Boolean;
begin
  Aborted:= False;
  if Assigned(FOnProgress) then
    FOnProgress(Self, Position, Max, Aborted, Msg);
  if Aborted then
    Abort;
end;

const
  FileLockSize = 2;
  RecordLockSize = 1000000000;

function TPagedFile.LockSection(const Offset: TPagedFileOffset; const Length: Cardinal; const Wait: Boolean): Boolean;
  // assumes FNeedLock = true
var
  Failed: Boolean;
begin
  // FNeedLocks => FStream is of type TFileStream
  Failed := false;
  repeat
{$ifdef SUPPORT_INT64}
    Result := LockFile(TFileStream(FStream).Handle, ULARGE_INTEGER(Offset).LowPart, ULARGE_INTEGER(Offset).HighPart, Length, 0);
{$else}
    Result := LockFile(TFileStream(FStream).Handle, Offset, 0, Length, 0);
{$endif}
    // test if lock violation, then wait a bit and try again
    if not Result and Wait then
    begin
      if (GetLastError = ERROR_LOCK_VIOLATION) then
        Sleep(10)
      else
        Failed := true;
    end;
  until Result or not Wait or Failed;
end;

function TPagedFile.UnlockSection(const Offset: TPagedFileOffset; const Length: Cardinal): Boolean;
begin
  if Assigned(FStream) then
{$ifdef SUPPORT_INT64}
    Result := UnlockFile(TFileStream(FStream).Handle, ULARGE_INTEGER(Offset).LowPart, ULARGE_INTEGER(Offset).HighPart, Length, 0)
{$else}
    Result := UnlockFile(TFileStream(FStream).Handle, Offset, 0, Length, 0)
{$endif}
  else
    Result := True;
end;

function TPagedFile.LockAllPages(const Wait: Boolean): Boolean;
var
  Offset: TPagedFileOffset;
  Length: Cardinal;
begin
  // do we need locking?
  if FNeedLocks and not FFileLocked then
  begin
    if FVirtualLocks then
    begin
      Offset := CalcLockOffset(0);
      Length := Cardinal(FileLockSize);
    end
    else
    begin
      Offset := 0;
      Length := Cardinal($7FFFFFFF);
    end;
    // lock requested section
    Result := LockSection(Offset, Length, Wait);
    if FVirtualLocks and Result then
    begin
      Result := LockSection(CalcLockOffset(0) - RecordLockSize, RecordLockSize,
        Wait);
      if Result then
        UnlockSection(CalcLockOffset(0) - RecordLockSize, RecordLockSize)
      else
        UnlockSection(Offset, Length);
    end;
    FFileLocked := Result;
  end
  else
    Result := true;
end;

procedure TPagedFile.UnlockAllPages;
var
  Offset: TPagedFileOffset;
  Length: Cardinal;
begin
  // do we need locking?
  if FNeedLocks and FFileLocked then
  begin
    if FVirtualLocks then
    begin
      Offset := CalcLockOffset(0);
      Length := Cardinal(FileLockSize);
    end
    else
    begin
      Offset := 0;
      Length := Cardinal($7FFFFFFF);
    end;
    // unlock requested section
    FFileLocked := not UnlockSection(Offset, Length);
  end;
end;

function TPagedFile.LockPage(const PageNo: Integer; const Wait: Boolean): Boolean;
var
  Offset: TPagedFileOffset;
  Length: Cardinal;
begin
  // do we need locking?
  if FNeedLocks then
  begin
    if FVirtualLocks then
    begin
      Offset := CalcLockOffset(PageNo);
      Length := 1;
    end else begin
      Offset := CalcPageOffset(PageNo);
      Length := RecordSize;
    end;
    // lock requested section
    Result := LockSection(Offset, Length, Wait);
  end else
    Result := true;
end;

procedure TPagedFile.UnlockPage(const PageNo: Integer);
var
  Offset: TPagedFileOffset;
  Length: Cardinal;
begin
  // do we need locking?
  if FNeedLocks then
  begin
    // calc offset + length
    if FVirtualLocks then
    begin
      Offset := CalcLockOffset(PageNo);
      Length := 1;
    end else begin
      Offset := CalcPageOffset(PageNo);
      Length := RecordSize;
    end;
    // unlock requested section
    // FNeedLocks => FStream is of type TFileStream
    UnlockSection(Offset, Length);
  end;
end;

end.

