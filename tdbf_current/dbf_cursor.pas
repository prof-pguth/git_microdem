unit dbf_cursor;

interface

{$I dbf_common.inc}

uses
  SysUtils,
  Classes,
  dbf_pgfile,
  dbf_common;

type

//====================================================================
  TVirtualCursor = class(TObject)
  private
    FFile: TPagedFile;

  protected
    function GetPhysicalRecNo: Integer; virtual; abstract;
    function GetSequentialRecNo: TSequentialRecNo; virtual; abstract;
    function GetSequentialRecordCount: TSequentialRecNo; virtual; abstract;
    procedure SetPhysicalRecNo(Recno: Integer); virtual; abstract;
    procedure SetSequentialRecNo(Recno: TSequentialRecNo); virtual; abstract;

  public
    constructor Create(pFile: TPagedFile);
    destructor Destroy; override;

    function  RecordSize: Integer;

    function  Next: Boolean; virtual; abstract;
    function  Prev: Boolean; virtual; abstract;
    procedure First; virtual; abstract;
    procedure Last; virtual; abstract;

    property PagedFile: TPagedFile read FFile;
    property PhysicalRecNo: Integer read GetPhysicalRecNo write SetPhysicalRecNo;
    property SequentialRecNo: TSequentialRecNo read GetSequentialRecNo write SetSequentialRecNo;
    property SequentialRecordCount: TSequentialRecNo read GetSequentialRecordCount;
  end;

implementation

constructor TVirtualCursor.Create(pFile: TPagedFile);
begin
  FFile := pFile;
end;

destructor TVirtualCursor.Destroy; {override;}
begin
end;

function TVirtualCursor.RecordSize : Integer;
begin
  if FFile = nil then
    Result := 0
  else
    Result := FFile.RecordSize;
end;

end.

