unit dbf_parser;

interface

{$I dbf_common.inc}

uses
  SysUtils,
  Classes,
{$ifdef KYLIX}
  Libc,
{$endif}
{$ifdef WINDOWS}
  Windows,
{$else}
  dbf_wtil,
{$endif}
  db,
  dbf_prscore,
  dbf_common,
  dbf_fields,
  dbf_prsdef,
  dbf_prssupp;

type

  TDbfParser = class(TCustomExpressionParser)
  private
    FDbfFile: Pointer;
    FFieldVarList: TStringList;
    FIsExpression: Boolean;       // expression or simple field?
    FFieldType: TExpressionType;
    FCaseInsensitive: Boolean;
    FRawStringFields: Boolean;
    FPartialMatch: boolean;
    FRecNoVariable: TVariable;

    function GetDbfFieldDef: TDbfFieldDef;
    function GetResultBufferSize: Integer;
    procedure SubstituteVariables(var ExprRec: PExpressionRec);
  protected
    FCurrentExpression: string;

    procedure FillExpressList; override;
    procedure HandleUnknownVariable(VarName: string); override;
    function  GetVariableInfo(VarName: AnsiString): TDbfFieldDef; virtual;
    function  CurrentExpression: string; override;
    procedure ValidateExpression({%H-}AExpression: string); virtual;
    function  GetResultType: TExpressionType; override;
    function  GetResultLen: Integer;

    procedure SetCaseInsensitive(NewInsensitive: Boolean);
    procedure SetRawStringFields(NewRawFields: Boolean);
    procedure SetPartialMatch(NewPartialMatch: boolean);
    procedure OptimizeExpr(var ExprRec: PExpressionRec); override;
  public
    constructor Create(ADbfFile: Pointer); virtual;
    destructor Destroy; override;

    procedure ClearExpressions; override;

    procedure ParseExpression(const AExpression: string); virtual;
    function ExtractFromBuffer(Buffer: PAnsiChar; RecNo: Integer): PAnsiChar; overload; virtual;
    function ExtractFromBuffer(Buffer: PAnsiChar; RecNo: Integer; var IsNull: Boolean): PAnsiChar; overload; virtual;

    property DbfFile: Pointer read FDbfFile write FDbfFile;
    property Expression: string read FCurrentExpression;
    property ResultLen: Integer read GetResultLen;
    property ResultBufferSize: Integer read GetResultBufferSize;
    property DbfFieldDef: TDbfFieldDef read GetDbfFieldDef;

    property CaseInsensitive: Boolean read FCaseInsensitive write SetCaseInsensitive;
    property RawStringFields: Boolean read FRawStringFields write SetRawStringFields;
    property PartialMatch: boolean read FPartialMatch write SetPartialMatch;
  end;

implementation

uses
  dbf,
  dbf_dbffile,
  dbf_str;

procedure FuncRecNo(Param: PExpressionRec);
begin
  PInteger(Param^.Res.MemoryPos^)^ := -1;
end;

type
// TFieldVar aids in retrieving field values from records
// in their proper type

  TFieldVar = class(TObject)
  private
    FFieldDef: TDbfFieldDef;
    FDbfFile: TDbfFile;
    FFieldName: AnsiString;
    FExprWord: TExprWord;
    FIsNull: Boolean;
    FIsNullPtr: PBoolean;
  protected
    function GetFieldVal: Pointer; virtual; abstract;
    function GetFieldType: TExpressionType; virtual; abstract;
    procedure SetExprWord(NewExprWord: TExprWord); virtual;

    property ExprWord: TExprWord read FExprWord write SetExprWord;
  public
    constructor Create(UseFieldDef: TDbfFieldDef; ADbfFile: TDbfFile);

    procedure Refresh(Buffer: PAnsiChar); virtual; abstract;

    property FieldVal: Pointer read GetFieldVal;
    property FieldDef: TDbfFieldDef read FFieldDef;
    property FieldType: TExpressionType read GetFieldType;
    property DbfFile: TDbfFile read FDbfFile;
    property FieldName: AnsiString read FFieldName;
    property IsNullPtr: PBoolean read FIsNullPtr;
  end;

  TStringFieldVar = class(TFieldVar)
  protected
    FFieldVal: PAnsiChar;
    FRawStringField: boolean;

    function GetFieldVal: Pointer; override;
    function GetFieldType: TExpressionType; override;
    procedure SetExprWord(NewExprWord: TExprWord); override;
    procedure SetRawStringField(NewRaw: boolean);
    procedure UpdateExprWord;
  public
    constructor Create(UseFieldDef: TDbfFieldDef; ADbfFile: TDbfFile);
    destructor Destroy; override;

    procedure Refresh(Buffer: PAnsiChar); override;

    property RawStringField: boolean read FRawStringField write SetRawStringField;
  end;

  TFloatFieldVar = class(TFieldVar)
  private
    FFieldVal: Double;
  protected
    function GetFieldVal: Pointer; override;
    function GetFieldType: TExpressionType; override;
  public
    procedure Refresh(Buffer: PAnsiChar); override;
  end;

  TIntegerFieldVar = class(TFieldVar)
  private
    FFieldVal: Integer;
  protected
    function GetFieldVal: Pointer; override;
    function GetFieldType: TExpressionType; override;
  public
    procedure Refresh(Buffer: PAnsiChar); override;
  end;

{$ifdef SUPPORT_INT64}
  TLargeIntFieldVar = class(TFieldVar)
  private
    FFieldVal: Int64;
  protected
    function GetFieldVal: Pointer; override;
    function GetFieldType: TExpressionType; override;
  public
    procedure Refresh(Buffer: PAnsiChar); override;
  end;
{$endif}

  TDateTimeFieldVar = class(TFieldVar)
  private
    FFieldVal: TDateTimeRec;
  protected
    function GetFieldVal: Pointer; override;
    function GetFieldType: TExpressionType; override;
  public
    procedure Refresh(Buffer: PAnsiChar); override;
  end;

  TBooleanFieldVar = class(TFieldVar)
  private
    FFieldVal: boolean;
  protected
    function GetFieldVal: Pointer; override;
    function GetFieldType: TExpressionType; override;
  public
    procedure Refresh(Buffer: PAnsiChar); override;
  end;

{ TFieldVar }

constructor TFieldVar.Create(UseFieldDef: TDbfFieldDef; ADbfFile: TDbfFile);
begin
  inherited Create;

  // store field
  FFieldDef := UseFieldDef;
  FDbfFile := ADbfFile;
  FFieldName := UseFieldDef.FieldName;
  FIsNullPtr := @FIsNull;
end;

procedure TFieldVar.SetExprWord(NewExprWord: TExprWord);
begin
  FExprWord := NewExprWord;
end;

{ TStringFieldVar }

constructor TStringFieldVar.Create(UseFieldDef: TDbfFieldDef; ADbfFile: TDbfFile);
begin
  inherited;
  FRawStringField := true;
end;

destructor TStringFieldVar.Destroy;
begin
  if not FRawStringField then
    FreeMem(FFieldVal);

  inherited;
end;

function TStringFieldVar.GetFieldVal: Pointer;
begin
  Result := @FFieldVal;
end;

function TStringFieldVar.GetFieldType: TExpressionType;
begin
  Result := etString;
end;

procedure TStringFieldVar.Refresh(Buffer: PAnsiChar);
var
  Len: Integer;
  Src: PAnsiChar;
begin
  Src := Buffer+FieldDef.Offset;
  if not FRawStringField then
  begin
    // copy field data
    Len := FieldDef.Size;
    while (Len >= 1) and ((Src[Len-1] = ' ') or (Src[Len-1] = #0)) do Dec(Len);
    // translate to ANSI
    Len := TranslateString(DbfFile.UseCodePage, GetACP, Src, FFieldVal, Len);
    FFieldVal[Len] := #0;
  end else
    FFieldVal := Src;
  FIsNull := not FDbfFile.GetFieldDataFromDef(FieldDef, FieldDef.FieldType, Buffer, nil, False);
end;

procedure TStringFieldVar.SetExprWord(NewExprWord: TExprWord);
begin
  inherited;
  UpdateExprWord;
end;

procedure TStringFieldVar.UpdateExprWord;
begin
  if FRawStringField then
    FExprWord.FixedLen := FieldDef.Size
  else
    FExprWord.FixedLen := -1;
end;

procedure TStringFieldVar.SetRawStringField(NewRaw: boolean);
begin
  if NewRaw = FRawStringField then exit;
  FRawStringField := NewRaw;
  if NewRaw then
    FreeMem(FFieldVal)
  else
    GetMem(FFieldVal, FieldDef.Size*3+1);
  UpdateExprWord;
end;

//--TFloatFieldVar-----------------------------------------------------------
function TFloatFieldVar.GetFieldVal: Pointer;
begin
  Result := @FFieldVal;
end;

function TFloatFieldVar.GetFieldType: TExpressionType;
begin
  Result := etFloat;
end;

procedure TFloatFieldVar.Refresh(Buffer: PAnsiChar);
begin
  // database width is default 64-bit double
//if not FDbfFile.GetFieldDataFromDef(FieldDef, FieldDef.FieldType, Buffer, @FFieldVal, false) then
  FIsNull := not FDbfFile.GetFieldDataFromDef(FieldDef, FieldDef.FieldType, Buffer, @FFieldVal, False);
  if FIsNull then
    FFieldVal := 0.0;
end;

//--TIntegerFieldVar----------------------------------------------------------
function TIntegerFieldVar.GetFieldVal: Pointer;
begin
  Result := @FFieldVal;
end;

function TIntegerFieldVar.GetFieldType: TExpressionType;
begin
  Result := etInteger;
end;

procedure TIntegerFieldVar.Refresh(Buffer: PAnsiChar);
begin
//FFieldVal := 0;
//FDbfFile.GetFieldDataFromDef(FieldDef, FieldDef.FieldType, Buffer, @FFieldVal, false);
  FIsNull := not FDbfFile.GetFieldDataFromDef(FieldDef, FieldDef.FieldType, Buffer, @FFieldVal, False);
  if FIsNull then
    FFieldVal := 0;
end;

{$ifdef SUPPORT_INT64}

//--TLargeIntFieldVar----------------------------------------------------------
function TLargeIntFieldVar.GetFieldVal: Pointer;
begin
  Result := @FFieldVal;
end;

function TLargeIntFieldVar.GetFieldType: TExpressionType;
begin
  Result := etLargeInt;
end;

procedure TLargeIntFieldVar.Refresh(Buffer: PAnsiChar);
begin
//if not FDbfFile.GetFieldDataFromDef(FieldDef, FieldDef.FieldType, Buffer, @FFieldVal, false) then
  FIsNull := not FDbfFile.GetFieldDataFromDef(FieldDef, FieldDef.FieldType, Buffer, @FFieldVal, False);
  if FIsNull then
    FFieldVal := 0;
end;

{$endif}

//--TDateTimeFieldVar---------------------------------------------------------
function TDateTimeFieldVar.GetFieldVal: Pointer;
begin
  Result := @FFieldVal;
end;

function TDateTimeFieldVar.GetFieldType: TExpressionType;
begin
  Result := etDateTime;
end;

procedure TDateTimeFieldVar.Refresh(Buffer: PAnsiChar);
begin
//if not FDbfFile.GetFieldDataFromDef(FieldDef, ftDateTime, Buffer, @FFieldVal, false) then
  FIsNull:= not FDbfFile.GetFieldDataFromDef(FieldDef, ftDateTime, Buffer, @FFieldVal, False);
  if FIsNull then
    FFieldVal.DateTime := 0.0;
end;

//--TBooleanFieldVar---------------------------------------------------------
function TBooleanFieldVar.GetFieldVal: Pointer;
begin
  Result := @FFieldVal;
end;

function TBooleanFieldVar.GetFieldType: TExpressionType;
begin
  Result := etBoolean;
end;

procedure TBooleanFieldVar.Refresh(Buffer: PAnsiChar);
var
  lFieldVal: word;
begin
//if FDbfFile.GetFieldDataFromDef(FieldDef, ftBoolean, Buffer, @lFieldVal, false) then
//  FFieldVal := lFieldVal <> 0
//else
//  FFieldVal := false;
  FIsNull := not FDbfFile.GetFieldDataFromDef(FieldDef, ftBoolean, Buffer, @lFieldVal, False);
  if FIsNull then
    FFieldVal := False
  else
    FFieldVal := lFieldVal <> 0;
end;

//--TRecNoVariable-----------------------------------------------------------
type
  TRecNoVariable = class(TIntegerVariable)
  private
    FRecNo: Integer;
  public
    constructor Create; reintroduce;
    procedure Refresh(RecNo: Integer);
  end;

constructor TRecNoVariable.Create;
begin
  inherited Create(EmptyStr, @FRecNo, nil, nil);
end;

procedure TRecNoVariable.Refresh(RecNo: Integer);
begin
  FRecNo := RecNo;
end;

//--TDbfParser---------------------------------------------------------------

constructor TDbfParser.Create(ADbfFile: Pointer);
begin
  FDbfFile := ADbfFile;
  FFieldVarList := TStringList.Create;
  FCaseInsensitive := true;
  FRawStringFields := true;
  inherited Create;
  if Assigned(FDbfFile) then
    FExpressionContext.DbfLangId := TDbfFile(FDbfFile).FileLangId;
end;

destructor TDbfParser.Destroy;
begin
  ClearExpressions;
  inherited;
  FreeAndNil(FFieldVarList);
  FreeAndNil(FRecNoVariable);
end;

function TDbfParser.GetResultType: TExpressionType;
begin
  // if not a real expression, return type ourself
  if FIsExpression then
    Result := inherited GetResultType
  else
    Result := FFieldType;
end;

function TDbfParser.GetResultLen: Integer;
begin
  // set result len for fixed length expressions / fields
  case ResultType of
    etBoolean:  Result := 1;
    etInteger:  Result := 4;
    etFloat:    Result := 8;
    etDateTime: Result := 8;
    etString:
    begin
      if not FIsExpression and (TStringFieldVar(FFieldVarList.Objects[0]).RawStringField) then
        Result := TStringFieldVar(FFieldVarList.Objects[0]).FieldDef.Size
      else
        Result := -1;
    end;
  else
    Result := -1;
  end;
end;

procedure TDbfParser.SetCaseInsensitive(NewInsensitive: Boolean);
begin
  if FCaseInsensitive <> NewInsensitive then
  begin
    // clear and regenerate functions
    FCaseInsensitive := NewInsensitive;
    FillExpressList;
  end;
end;

procedure TDbfParser.SetPartialMatch(NewPartialMatch: boolean);
begin
  if FPartialMatch <> NewPartialMatch then
  begin
    // refill function list
    FPartialMatch := NewPartialMatch;
    FillExpressList;
  end;
end;

procedure TDbfParser.OptimizeExpr(var ExprRec: PExpressionRec);
begin
  inherited OptimizeExpr(ExprRec);
  SubstituteVariables(ExprRec);
end;

procedure TDbfParser.SetRawStringFields(NewRawFields: Boolean);
var
  I: integer;
begin
  if FRawStringFields <> NewRawFields then
  begin
    // clear and regenerate functions, custom fields will be deleted too
    FRawStringFields := NewRawFields;
    for I := 0 to FFieldVarList.Count - 1 do
      if FFieldVarList.Objects[I] is TStringFieldVar then
        TStringFieldVar(FFieldVarList.Objects[I]).RawStringField := NewRawFields;
  end;
end;

procedure TDbfParser.FillExpressList;
var
  lExpression: string;
begin
  lExpression := FCurrentExpression;
  ClearExpressions;
  FWordsList.FreeAll;
  FWordsList.AddList(DbfWordsGeneralList, 0, DbfWordsGeneralList.Count - 1);
  if FCaseInsensitive then
  begin
    FWordsList.AddList(DbfWordsInsensGeneralList, 0, DbfWordsInsensGeneralList.Count - 1);
    if FPartialMatch then
    begin
      FWordsList.AddList(DbfWordsInsensPartialList, 0, DbfWordsInsensPartialList.Count - 1);
    end else begin
      FWordsList.AddList(DbfWordsInsensNoPartialList, 0, DbfWordsInsensNoPartialList.Count - 1);
    end;
  end else begin
    FWordsList.AddList(DbfWordsSensGeneralList, 0, DbfWordsSensGeneralList.Count - 1);
    if FPartialMatch then
    begin
      FWordsList.AddList(DbfWordsSensPartialList, 0, DbfWordsSensPartialList.Count - 1);
    end else begin
      FWordsList.AddList(DbfWordsSensNoPartialList, 0, DbfWordsSensNoPartialList.Count - 1);
    end;
  end;
  FWordsList.Add(TVaryingFunction.Create('RECNO', '', '', 0, etInteger, FuncRecNo, ''));
  if Length(lExpression) > 0 then
    ParseExpression(lExpression);
end;

function TDbfParser.GetVariableInfo(VarName: AnsiString): TDbfFieldDef;
begin
  Result := TDbfFile(FDbfFile).GetFieldInfo(VarName);
end;

procedure TDbfParser.HandleUnknownVariable(VarName: string);
var
  FieldInfo: TDbfFieldDef;
  TempFieldVar: TFieldVar;
  VariableFieldInfo: TVariableFieldInfo;
begin
  // is this variable a fieldname?
  FieldInfo := GetVariableInfo(AnsiString(VarName));
  if FieldInfo = nil then
    raise ExceptionClass.CreateFmt(STRING_PARSER_UNKNOWN_FIELD, [VarName]);

  // define field in parser
  FillChar(VariableFieldInfo{%H-}, SizeOf(VariableFieldInfo), 0);
  VariableFieldInfo.DbfFieldDef := FieldInfo;
  VariableFieldInfo.NativeFieldType := FieldInfo.NativeFieldType;
  VariableFieldInfo.Size := FieldInfo.Size;
  VariableFieldInfo.Precision := FieldInfo.Precision;
  case FieldInfo.FieldType of
    ftString:
      begin
        TempFieldVar := TStringFieldVar.Create(FieldInfo, TDbfFile(FDbfFile));
        TempFieldVar.ExprWord := DefineStringVariable(VarName, TempFieldVar.FieldVal, TempFieldVar.IsNullPtr, @VariableFieldInfo);
        TStringFieldVar(TempFieldVar).RawStringField := FRawStringFields;
      end;
    ftBoolean:
      begin
        TempFieldVar := TBooleanFieldVar.Create(FieldInfo, TDbfFile(FDbfFile));
        TempFieldVar.ExprWord := DefineBooleanVariable(VarName, TempFieldVar.FieldVal, TempFieldVar.IsNullPtr, @VariableFieldInfo);
      end;
    ftFloat:
      begin
        TempFieldVar := TFloatFieldVar.Create(FieldInfo, TDbfFile(FDbfFile));
        TempFieldVar.ExprWord := DefineFloatVariable(VarName, TempFieldVar.FieldVal, TempFieldVar.IsNullPtr, @VariableFieldInfo);
      end;
    ftAutoInc, ftInteger, ftSmallInt:
      begin
        TempFieldVar := TIntegerFieldVar.Create(FieldInfo, TDbfFile(FDbfFile));
        TempFieldVar.ExprWord := DefineIntegerVariable(VarName, TempFieldVar.FieldVal, TempFieldVar.IsNullPtr, @VariableFieldInfo);
      end;
  {$ifdef SUPPORT_INT64}
    ftLargeInt:
      begin
        TempFieldVar := TLargeIntFieldVar.Create(FieldInfo, TDbfFile(FDbfFile));
        TempFieldVar.ExprWord := DefineLargeIntVariable(VarName, TempFieldVar.FieldVal, TempFieldVar.IsNullPtr, @VariableFieldInfo);
      end;
  {$endif}
    ftDate, ftDateTime:
      begin
        TempFieldVar := TDateTimeFieldVar.Create(FieldInfo, TDbfFile(FDbfFile));
        TempFieldVar.ExprWord := DefineDateTimeVariable(VarName, TempFieldVar.FieldVal, TempFieldVar.IsNullPtr, @VariableFieldInfo);
      end;
  else
    raise ExceptionClass.CreateFmt(STRING_PARSER_INVALID_FIELDTYPE, [VarName]);
  end;

  // add to our own list
  FFieldVarList.AddObject(VarName, TempFieldVar);
end;

function TDbfParser.CurrentExpression: string;
begin
  Result := FCurrentExpression;
end;

procedure TDbfParser.ClearExpressions;
var
  I: Integer;
begin
  inherited;

  FFieldType := etUnknown;

  // test if already freed
  if FFieldVarList <> nil then
  begin
    // free field list
    for I := 0 to FFieldVarList.Count - 1 do
    begin
      // replacing with nil = undefining variable
      FWordsList.DoFree(TFieldVar(FFieldVarList.Objects[I]).FExprWord);
      TFieldVar(FFieldVarList.Objects[I]).Free;
    end;
    FFieldVarList.Clear;
  end;

  // clear expression
  FCurrentExpression := EmptyStr;
end;

procedure TDbfParser.ValidateExpression(AExpression: string);
begin
end;

procedure TDbfParser.ParseExpression(const AExpression: string);
begin
  // clear any current expression
  ClearExpressions;

  // is this a simple field or complex expression?
  FIsExpression := (GetVariableInfo(AnsiString(AExpression)) = nil);
  if FIsExpression then
  begin
    // parse requested
    CompileExpression(AExpression);
  end else begin
    // simple field, create field variable for it
    HandleUnknownVariable(AExpression);
    FFieldType := TFieldVar(FFieldVarList.Objects[0]).FieldType;
  end;

  ValidateExpression(AExpression);

  // if no errors, assign current expression
  FCurrentExpression := AExpression;
end;

function TDbfParser.ExtractFromBuffer(Buffer: PAnsiChar; RecNo: Integer): PAnsiChar;
var
  IsNull: Boolean;
begin
  IsNull := False;
  Result := ExtractFromBuffer(Buffer, RecNo, IsNull);
end;

function TDbfParser.ExtractFromBuffer(Buffer: PAnsiChar; RecNo: Integer; var IsNull: Boolean): PAnsiChar;
var
  I: Integer;
begin
  // prepare all field variables
  for I := 0 to FFieldVarList.Count - 1 do
    TFieldVar(FFieldVarList.Objects[I]).Refresh(Buffer);
  if Assigned(FRecNoVariable) then
    TRecNoVariable(FRecNoVariable).Refresh(RecNo);

  // complex expression?
  if FIsExpression then
  begin
    // execute expression
    EvaluateCurrent;
    Result := PAnsiChar(ExpResult);
    IsNull := False;
    if LastRec <> nil then
      if LastRec^.IsNullPtr <> nil then
        IsNull := LastRec^.IsNullPtr^;
  end else begin
    // simple field, get field result
    if FFieldVarList.Count <> 0 then
    begin
      Result := TFieldVar(FFieldVarList.Objects[0]).FieldVal;
      // if string then dereference
      if FFieldType = etString then
        Result := PAnsiChar(PPAnsiChar(Result)^); // Was PPChar
      IsNull := TFieldVar(FFieldVarList.Objects[0]).IsNullPtr^;
    end
    else
    begin
      Result := PAnsiChar(ExpResult);
      IsNull := False;
    end;
  end;
end;

function TDbfParser.GetDbfFieldDef: TDbfFieldDef;
var
  FieldVar: TFieldVar;
  FieldInfo: PVariableFieldInfo;
begin
  if FIsExpression then
  begin
    Result := nil;
    if Assigned(LastRec) and (LastRec^.ExprWord is TVariable) then
    begin
      FieldInfo := TVariable(LastRec^.ExprWord).FieldInfo;
      if Assigned(FieldInfo) then
        Result := FieldInfo.DbfFieldDef;
    end;
  end
  else
  begin
    FieldVar:= TFieldVar(FFieldVarList.Objects[0]);
    Result := FieldVar.FieldDef;
  end;
end;

function TDbfParser.GetResultBufferSize: Integer;
begin
  if ResultLen >= 0 then
    Result := ResultLen
  else
    Result := FExpResultSize;
end;

procedure TDbfParser.SubstituteVariables(var ExprRec: PExpressionRec);
var
  Index: Integer;
  NewExprRec: PExpressionRec;
  Variable: TVariable;
begin
  if @ExprRec.Oper = @FuncRecNo then
  begin
    NewExprRec := MakeRec;
    try
      if Assigned(FRecNoVariable) then
        Variable := FRecNoVariable
      else
        Variable := TRecNoVariable.Create;
      try
        NewExprRec.ExprWord := Variable;
        NewExprRec.Oper := NewExprRec.ExprWord.ExprFunc;
        NewExprRec.Args[0] := NewExprRec.ExprWord.AsPointer;
        NewExprRec.IsNullPtr := @NewExprRec.IsNull;
        CurrentRec := nil;
        DisposeList(ExprRec);
        ExprRec := NewExprRec;
      except
        if not Assigned(FRecNoVariable) then
          FreeAndNil(Variable);
        raise;
      end;
      FRecNoVariable:= Variable;
    except
      DisposeList(NewExprRec);
      raise;
    end;
  end
  else
  begin
    for Index := 0 to Pred(ExprRec^.ExprWord.MaxFunctionArg) do
      if ExprRec^.ArgList[Index] <> nil then
        SubstituteVariables(ExprRec^.ArgList[Index]);
  end;
end;

end.

