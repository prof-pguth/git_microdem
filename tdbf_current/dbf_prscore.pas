unit dbf_prscore;

{--------------------------------------------------------------
| TCustomExpressionParser
|
| - contains core expression parser
|
| This code is based on code from:
|
| Original author: Egbert van Nes
| With contributions of: John Bultena and Ralf Junker
| Homepage: http://www.slm.wau.nl/wkao/parseexpr.html
|
| see also: http://www.datalog.ro/delphi/parser.html
|   (Renate Schaaf (schaaf at math.usu.edu), 1993
|    Alin Flaider (aflaidar at datalog.ro), 1996
|    Version 9-10: Stefan Hoffmeister, 1996-1997)
|
|---------------------------------------------------------------}

{$BOOLEVAL OFF}

interface

{$I dbf_common.inc}

uses
  SysUtils,
  Classes,
  {$ifndef FPC_VERSION}
  Db,
  {$endif}
  Math,
  dbf_Common,
  dbf_prssupp,
  dbf_prsdef;

{$define ENG_NUMBERS}

// ENG_NUMBERS will force the use of english style numbers 8.1 instead of 8,1
//   (if the comma is your decimal separator)
// the advantage is that arguments can be separated with a comma which is
// fairly common, otherwise there is ambuigity: what does 'var1,8,4,4,5' mean?
// if you don't define ENG_NUMBERS and DecimalSeparator is a comma then
// the argument separator will be a semicolon ';'

type
  TCustomExpressionParser = class(TObject)
  private
    FHexChar: Char;
    FArgSeparator: Char;
    FDecimalSeparator: Char;
    FOptimize: Boolean;
    FConstantsList: TOCollection;
    FLastRec: PExpressionRec;
    FCurrentRec: PExpressionRec;
    FExpResult: PAnsiChar; // was PChar;
    FExpResultPos: PAnsiChar; // was PChar;

    procedure ParseString(AnExpression: string; DestCollection: TExprCollection);
    function  MakeTree(Expr: TExprCollection; FirstItem, LastItem: Integer): PExpressionRec;
    procedure MakeLinkedList(var ExprRec: PExpressionRec; Memory: PPAnsiChar;
        MemoryPos: PPAnsiChar; MemSize: PInteger); // Was PPChar
    procedure Check(AnExprList: TExprCollection);
    procedure CheckArguments(ExprRec: PExpressionRec);
    procedure RemoveConstants(var ExprRec: PExpressionRec);
    function ResultCanVary(ExprRec: PExpressionRec): Boolean;
  protected
    FExpressionContext: TExpressionContext;
    FExpResultSize: Integer;
    FWordsList: TSortedCollection;

    function MakeRec: PExpressionRec; virtual;
    procedure FillExpressList; virtual; abstract;
    procedure HandleUnknownVariable(VarName: string); virtual; abstract;

    procedure CompileExpression(AnExpression: string);
    procedure EvaluateCurrent;
    procedure DisposeList(ARec: PExpressionRec);
    procedure DisposeTree(ExprRec: PExpressionRec);
    function CurrentExpression: string; virtual; abstract;
    function GetResultType: TExpressionType; virtual;
    function IsIndex: Boolean; virtual;
    procedure OptimizeExpr(var ExprRec: PExpressionRec); virtual;
    function ExceptionClass: ExceptClass; virtual;
    procedure ReadWord(const AnExpr: string; var isConstant: Boolean; var I1, I2: Integer; Len: Integer); virtual;
    function CreateConstant(W: string): TConstant; virtual;

    property CurrentRec: PExpressionRec read FCurrentRec write FCurrentRec;
    property LastRec: PExpressionRec read FLastRec write FLastRec;
    property ExpResult: PAnsiChar read FExpResult; // Was PChar
    property ExpResultPos: PAnsiChar read FExpResultPos write FExpResultPos; // Was PChar

  public
    constructor Create;
    destructor Destroy; override;

    function DefineFloatVariable(AVarName: string; AValue: PDouble): TExprWord; overload;
    function DefineFloatVariable(AVarName: string; AValue: PDouble; AIsNullPtr: PBoolean; AFieldInfo: PVariableFieldInfo): TExprWord; overload;
    function DefineIntegerVariable(AVarName: string; AValue: PInteger): TExprWord; overload;
    function DefineIntegerVariable(AVarName: string; AValue: PInteger; AIsNullPtr: PBoolean; AFieldInfo: PVariableFieldInfo): TExprWord; overload;
//  procedure DefineSmallIntVariable(AVarName: string; AValue: PSmallInt);
{$ifdef SUPPORT_INT64}
    function DefineLargeIntVariable(AVarName: string; AValue: PLargeInt): TExprWord; overload;
    function DefineLargeIntVariable(AVarName: string; AValue: PLargeInt; AIsNullPtr: PBoolean; AFieldInfo: PVariableFieldInfo): TExprWord; overload;
{$endif}
    function DefineDateTimeVariable(AVarName: string; AValue: PDateTimeRec): TExprWord; overload;
    function DefineDateTimeVariable(AVarName: string; AValue: PDateTimeRec; AIsNullPtr: PBoolean; AFieldInfo: PVariableFieldInfo): TExprWord; overload;
    function DefineBooleanVariable(AVarName: string; AValue: PBoolean): TExprWord; overload;
    function DefineBooleanVariable(AVarName: string; AValue: PBoolean; AIsNullPtr: PBoolean; AFieldInfo: PVariableFieldInfo): TExprWord; overload;
    function DefineStringVariable(AVarName: string; AValue: PPAnsiChar): TExprWord; overload; // Was PPChar
    function DefineStringVariable(AVarName: string; AValue: PPAnsiChar; AIsNullPtr: PBoolean; AFieldInfo: PVariableFieldInfo): TExprWord; overload; // Was PPChar
    function DefineFunction(AFunctName, AShortName, ADescription, ATypeSpec: string;
        AMinFunctionArg: Integer; AResultType: TExpressionType; AFuncAddress: TExprFunc): TExprWord;
    procedure Evaluate(AnExpression: string);
    function AddExpression(AnExpression: string): Integer;
    procedure ClearExpressions; virtual;
//    procedure GetGeneratedVars(AList: TList);
    procedure GetFunctionNames(AList: TStrings);
    function GetFunctionDescription(AFunction: string): string;
    property HexChar: Char read FHexChar write FHexChar;
    property ArgSeparator: Char read FArgSeparator write FArgSeparator;
    property Optimize: Boolean read FOptimize write FOptimize;
    property ResultType: TExpressionType read GetResultType;

    //if optimize is selected, constant expressions are tried to remove
    //such as: 4*4*x is evaluated as 16*x and exp(1)-4*x is repaced by 2.17 -4*x
  end;


//--Expression functions-----------------------------------------------------

(*
procedure FuncFloatToStr(Param: PExpressionRec);
procedure FuncIntToStr_Gen(Param: PExpressionRec; Val: {$ifdef SUPPORT_INT64}Int64{$else}Integer{$endif});
procedure FuncIntToStr(Param: PExpressionRec);
{$ifdef SUPPORT_INT64}
procedure FuncInt64ToStr(Param: PExpressionRec);
{$endif}
*)
procedure FuncStr      (Param: PExpressionRec);
procedure FuncDateToStr(Param: PExpressionRec);
procedure FuncSubString(Param: PExpressionRec);
procedure FuncUppercase(Param: PExpressionRec);
procedure FuncNegative_F_F(Param: PExpressionRec);
procedure FuncNegative_I_I(Param: PExpressionRec);
{$ifdef SUPPORT_INT64}
procedure FuncNegative_L_L(Param: PExpressionRec);
{$endif}
procedure FuncLowercase(Param: PExpressionRec);
procedure FuncAdd_F_FF(Param: PExpressionRec);
procedure FuncAdd_F_FI(Param: PExpressionRec);
procedure FuncAdd_F_II(Param: PExpressionRec);
procedure FuncAdd_F_IF(Param: PExpressionRec);
{$ifdef SUPPORT_INT64}
procedure FuncAdd_F_FL(Param: PExpressionRec);
procedure FuncAdd_F_IL(Param: PExpressionRec);
procedure FuncAdd_F_LL(Param: PExpressionRec);
procedure FuncAdd_F_LF(Param: PExpressionRec);
procedure FuncAdd_F_LI(Param: PExpressionRec);
{$endif}
procedure FuncSub_F_FF(Param: PExpressionRec);
procedure FuncSub_F_FI(Param: PExpressionRec);
procedure FuncSub_F_II(Param: PExpressionRec);
procedure FuncSub_F_IF(Param: PExpressionRec);
{$ifdef SUPPORT_INT64}
procedure FuncSub_F_FL(Param: PExpressionRec);
procedure FuncSub_F_IL(Param: PExpressionRec);
procedure FuncSub_F_LL(Param: PExpressionRec);
procedure FuncSub_F_LF(Param: PExpressionRec);
procedure FuncSub_F_LI(Param: PExpressionRec);
{$endif}
procedure FuncMul_F_FF(Param: PExpressionRec);
procedure FuncMul_F_FI(Param: PExpressionRec);
procedure FuncMul_F_II(Param: PExpressionRec);
procedure FuncMul_F_IF(Param: PExpressionRec);
{$ifdef SUPPORT_INT64}
procedure FuncMul_F_FL(Param: PExpressionRec);
procedure FuncMul_F_IL(Param: PExpressionRec);
procedure FuncMul_F_LL(Param: PExpressionRec);
procedure FuncMul_F_LF(Param: PExpressionRec);
procedure FuncMul_F_LI(Param: PExpressionRec);
{$endif}
procedure FuncDiv_F_FF(Param: PExpressionRec);
procedure FuncDiv_F_FI(Param: PExpressionRec);
procedure FuncDiv_F_II(Param: PExpressionRec);
procedure FuncDiv_F_IF(Param: PExpressionRec);
{$ifdef SUPPORT_INT64}
procedure FuncDiv_F_FL(Param: PExpressionRec);
procedure FuncDiv_F_IL(Param: PExpressionRec);
procedure FuncDiv_F_LL(Param: PExpressionRec);
procedure FuncDiv_F_LF(Param: PExpressionRec);
procedure FuncDiv_F_LI(Param: PExpressionRec);
{$endif}
procedure FuncStrI_EQ(Param: PExpressionRec);
procedure FuncStrI_NEQ(Param: PExpressionRec);
procedure FuncStrI_LT(Param: PExpressionRec);
procedure FuncStrI_GT(Param: PExpressionRec);
procedure FuncStrI_LTE(Param: PExpressionRec);
procedure FuncStrI_GTE(Param: PExpressionRec);
procedure FuncStr_EQ(Param: PExpressionRec);
procedure FuncStr_NEQ(Param: PExpressionRec);
procedure FuncStr_LT(Param: PExpressionRec);
procedure FuncStr_GT(Param: PExpressionRec);
procedure FuncStr_LTE(Param: PExpressionRec);
procedure FuncStr_GTE(Param: PExpressionRec);
procedure Func_FF_EQ(Param: PExpressionRec);
procedure Func_FF_NEQ(Param: PExpressionRec);
procedure Func_FF_LT(Param: PExpressionRec);
procedure Func_FF_GT(Param: PExpressionRec);
procedure Func_FF_LTE(Param: PExpressionRec);
procedure Func_FF_GTE(Param: PExpressionRec);
procedure Func_FI_EQ(Param: PExpressionRec);
procedure Func_FI_NEQ(Param: PExpressionRec);
procedure Func_FI_LT(Param: PExpressionRec);
procedure Func_FI_GT(Param: PExpressionRec);
procedure Func_FI_LTE(Param: PExpressionRec);
procedure Func_FI_GTE(Param: PExpressionRec);
procedure Func_II_EQ(Param: PExpressionRec);
procedure Func_II_NEQ(Param: PExpressionRec);
procedure Func_II_LT(Param: PExpressionRec);
procedure Func_II_GT(Param: PExpressionRec);
procedure Func_II_LTE(Param: PExpressionRec);
procedure Func_II_GTE(Param: PExpressionRec);
procedure Func_IF_EQ(Param: PExpressionRec);
procedure Func_IF_NEQ(Param: PExpressionRec);
procedure Func_IF_LT(Param: PExpressionRec);
procedure Func_IF_GT(Param: PExpressionRec);
procedure Func_IF_LTE(Param: PExpressionRec);
procedure Func_IF_GTE(Param: PExpressionRec);
{$ifdef SUPPORT_INT64}
procedure Func_LL_EQ(Param: PExpressionRec);
procedure Func_LL_NEQ(Param: PExpressionRec);
procedure Func_LL_LT(Param: PExpressionRec);
procedure Func_LL_GT(Param: PExpressionRec);
procedure Func_LL_LTE(Param: PExpressionRec);
procedure Func_LL_GTE(Param: PExpressionRec);
procedure Func_LF_EQ(Param: PExpressionRec);
procedure Func_LF_NEQ(Param: PExpressionRec);
procedure Func_LF_LT(Param: PExpressionRec);
procedure Func_LF_GT(Param: PExpressionRec);
procedure Func_LF_LTE(Param: PExpressionRec);
procedure Func_LF_GTE(Param: PExpressionRec);
procedure Func_FL_EQ(Param: PExpressionRec);
procedure Func_FL_NEQ(Param: PExpressionRec);
procedure Func_FL_LT(Param: PExpressionRec);
procedure Func_FL_GT(Param: PExpressionRec);
procedure Func_FL_LTE(Param: PExpressionRec);
procedure Func_FL_GTE(Param: PExpressionRec);
procedure Func_LI_EQ(Param: PExpressionRec);
procedure Func_LI_NEQ(Param: PExpressionRec);
procedure Func_LI_LT(Param: PExpressionRec);
procedure Func_LI_GT(Param: PExpressionRec);
procedure Func_LI_LTE(Param: PExpressionRec);
procedure Func_LI_GTE(Param: PExpressionRec);
procedure Func_IL_EQ(Param: PExpressionRec);
procedure Func_IL_NEQ(Param: PExpressionRec);
procedure Func_IL_LT(Param: PExpressionRec);
procedure Func_IL_GT(Param: PExpressionRec);
procedure Func_IL_LTE(Param: PExpressionRec);
procedure Func_IL_GTE(Param: PExpressionRec);
{$endif}
procedure Func_AND(Param: PExpressionRec);
procedure Func_OR(Param: PExpressionRec);
procedure Func_NOT(Param: PExpressionRec);

procedure FuncAdd_S(Param: PExpressionRec);
procedure FuncSub_S(Param: PExpressionRec);

var
  DbfWordsSensGeneralList, DbfWordsInsensGeneralList: TExpressList;
  DbfWordsSensPartialList, DbfWordsInsensPartialList: TExpressList;
  DbfWordsSensNoPartialList, DbfWordsInsensNoPartialList: TExpressList;
  DbfWordsGeneralList: TExpressList;

implementation

uses
  dbf_ansistrings;

procedure LinkVariable(ExprRec: PExpressionRec);
begin
  ///with ExprRec^ do
  begin
    if ExprRec^.ExprWord.IsVariable then
    begin
      // copy pointer to variable
      ExprRec^.Args[0] := ExprRec^.ExprWord.AsPointer;
      // store length as second parameter
      ExprRec^.Args[1] := PAnsiChar(ExprRec^.ExprWord.LenAsPointer); // Was PChar
      ExprRec^.IsNullPtr := ExprRec^.ExprWord.IsNullPtr;
    end
    else
      ExprRec^.IsNullPtr := @ExprRec^.IsNull;
  end;
end;

procedure LinkVariables(ExprRec: PExpressionRec);
var
  I: integer;
begin
  with ExprRec^ do
  begin
    I := 0;
    while (I < MaxArg) and (ArgList[I] <> nil) do
    begin
      LinkVariables(ArgList[I]);
      Inc(I);
    end;
  end;
  LinkVariable(ExprRec);
end;

{ TCustomExpressionParser }

constructor TCustomExpressionParser.Create;
begin
  inherited;

  FHexChar := '$';
{$IFDEF ENG_NUMBERS}
  FDecimalSeparator := '.';
  FArgSeparator := ',';
{$ELSE}
  FDecimalSeparator := DecimalSeparator;
  if DecimalSeparator = ',' then
    FArgSeparator := ';'
  else
    FArgSeparator := ',';
{$ENDIF}
  FConstantsList := TOCollection.Create;
  FWordsList := TExpressList.Create;
  GetMem(FExpResult, ArgAllocSize);
  FExpResultPos := FExpResult;
  FExpResultSize := ArgAllocSize;
  FillChar(FExpResultPos^, FExpResultSize, 0);
  FOptimize := true;
  FillExpressList;
end;

destructor TCustomExpressionParser.Destroy;
begin
  ClearExpressions;
  FreeMem(FExpResult);
  FConstantsList.Free;
  FWordsList.Free;

  inherited;
end;

procedure TCustomExpressionParser.CompileExpression(AnExpression: string);
var
  ExpColl: TExprCollection;
  ExprTree: PExpressionRec;
begin
  if Length(AnExpression) > 0 then
  begin
    ExprTree := nil;
    ExpColl := TExprCollection.Create;
    try
      //    FCurrentExpression := anExpression;
      ParseString(AnExpression, ExpColl);
      Check(ExpColl);
      ExprTree := MakeTree(ExpColl, 0, ExpColl.Count - 1);
      FCurrentRec := nil;
      CheckArguments(ExprTree);
      LinkVariables(ExprTree);
      if Optimize then
        OptimizeExpr(ExprTree);
      // all constant expressions are evaluated and replaced by variables
      FCurrentRec := nil;
      FExpResultPos := FExpResult;
      MakeLinkedList(ExprTree, @FExpResult, @FExpResultPos, @FExpResultSize);
    except
      on E: Exception do
      begin
        DisposeTree(ExprTree);
        ExpColl.Free;
        raise;
      end;
    end;
    ExpColl.Free;
  end;
end;

procedure TCustomExpressionParser.CheckArguments(ExprRec: PExpressionRec);
var
  TempExprWord: TExprWord;
  I, error, firstFuncIndex, funcIndex: Integer;
  foundAltFunc: Boolean;

  procedure FindAlternate;
  begin
    // see if we can find another function
    if funcIndex < 0 then
    begin
      firstFuncIndex := FWordsList.IndexOf(ExprRec^.ExprWord);
      funcIndex := firstFuncIndex;
    end;
    // check if not last function
    if (0 <= funcIndex) and (funcIndex < FWordsList.Count - 1) then
    begin
      inc(funcIndex);
      TempExprWord := TExprWord(FWordsList.Items[funcIndex]);
      if FWordsList.Compare(FWordsList.KeyOf(ExprRec^.ExprWord), FWordsList.KeyOf(TempExprWord)) = 0 then
      begin
        ExprRec^.ExprWord := TempExprWord;
        ExprRec^.Oper := ExprRec^.ExprWord.ExprFunc;
        foundAltFunc := true;
      end;
    end;
  end;

  procedure InternalCheckArguments;
  begin
    I := 0;
    error := 0;
    foundAltFunc := false;
    with ExprRec^ do
    begin
      if WantsFunction <> (ExprWord.IsFunction and not ExprWord.IsOperator) then
      begin
        error := 4;
        exit;
      end;

      while (ArgList[I] <> nil) and (error = 0) do
      begin
        if I < ExprWord.MaxFunctionArg then
        begin
          // test subarguments first
          CheckArguments(ArgList[I]);

          // test if correct type
          if (ArgList[I]^.ExprWord.ResultType <> ExprCharToExprType(ExprWord.TypeSpec[I+1])) then
            error := 2;
        end;

        // goto next argument
        Inc(I);
      end;

      // test if enough parameters passed; I = num args user passed
      if (error = 0) and (I < ExprWord.MinFunctionArg) then
        error := 1;

      // test if too many parameters passed
      if (error = 0) and (I > ExprWord.MaxFunctionArg) then
        error := 3;
    end;
  end;

begin
  funcIndex := -1;
  repeat
    InternalCheckArguments;

    // error occurred?
    if error <> 0 then
      FindAlternate;
  until (error = 0) or not foundAltFunc;

  // maybe it's an undefined variable
  if (error <> 0) and (not ExprRec^.WantsFunction) and (not ExprRec^.ExprWord.IsOperator) and (firstFuncIndex >= 0) then
  begin
    HandleUnknownVariable(ExprRec^.ExprWord.Name);
    { must not add variable as first function in this set of duplicates,
      otherwise following searches will not find it }
    FWordsList.Exchange(firstFuncIndex, firstFuncIndex+1);
    ExprRec^.ExprWord := TExprWord(FWordsList.Items[firstFuncIndex+1]);
    ExprRec^.Oper := ExprRec^.ExprWord.ExprFunc;
    InternalCheckArguments;
  end;

  if (error = 0) and ((@ExprRec^.Oper = @FuncAdd_S) or (@ExprRec^.Oper = @FuncSub_S)) and (not IsIndex) then
    error := 2;

  // fatal error?
  case error of
    1: raise ExceptionClass.Create('Function or operand has too few arguments');
    2: raise ExceptionClass.Create('Argument type mismatch');
    3: raise ExceptionClass.Create('Function or operand has too many arguments');
    4: raise ExceptionClass.Create('No function with this name, remove brackets for variable');
  end;
end;

function TCustomExpressionParser.ResultCanVary(ExprRec: PExpressionRec):
  Boolean;
var
  I: Integer;
begin
  with ExprRec^ do
  begin
    Result := ExprWord.CanVary;
    if not Result then
      for I := 0 to ExprWord.MaxFunctionArg - 1 do
        if (ArgList[I] <> nil) and ResultCanVary(ArgList[I]) then
        begin
          Result := true;
          Exit;
        end
  end;
end;

procedure TCustomExpressionParser.RemoveConstants(var ExprRec: PExpressionRec);
var
  I: Integer;
begin
  if not ResultCanVary(ExprRec) then
  begin
    if not ExprRec^.ExprWord.IsVariable then
    begin
      // reset current record so that make list generates new
      FCurrentRec := nil;
      FExpResultPos := FExpResult;
      MakeLinkedList(ExprRec, @FExpResult, @FExpResultPos, @FExpResultSize);

      try
        // compute result
        EvaluateCurrent;

        // make new record to store constant in
        ExprRec := MakeRec;

        // check result type
        with ExprRec^ do
        begin
          case ResultType of
            etBoolean: ExprWord := TBooleanConstant.Create(EmptyStr, PBoolean(FExpResult)^);
            etFloat: ExprWord := TFloatConstant.CreateAsDouble(EmptyStr, PDouble(FExpResult)^);
            etInteger: ExprWord := TIntegerConstant.Create(PInteger(FExpResult)^);
{$ifdef SUPPORT_INT64}
            etLargeInt:ExprWord := TLargeIntConstant.Create(PInt64(FExpResult)^);
{$endif}
            etString: ExprWord := TStringConstant.Create(string(FExpResult)); // Added string cast
            etDateTime: ExprWord := TDateTimeConstant.Create(EmptyStr, PDateTime(FExpResult)^);
          end;

          // fill in structure
          Oper := ExprWord.ExprFunc;
          Args[0] := ExprWord.AsPointer;
          FConstantsList.Add(ExprWord);
        end;
      finally
        DisposeList(FCurrentRec);
        FCurrentRec := nil;
      end;
    end;
  end else
    with ExprRec^ do
    begin
      for I := 0 to ExprWord.MaxFunctionArg - 1 do
        if ArgList[I] <> nil then
          RemoveConstants(ArgList[I]);
    end;
end;

procedure TCustomExpressionParser.DisposeTree(ExprRec: PExpressionRec);
var
  I: Integer;
begin
  if ExprRec <> nil then
  begin
    with ExprRec^ do
    begin
      if ExprWord <> nil then
        for I := 0 to ExprWord.MaxFunctionArg - 1 do
          DisposeTree(ArgList[I]);
      if Res <> nil then
        Res.Free;
    end;
    Dispose(ExprRec);
  end;
end;

procedure TCustomExpressionParser.DisposeList(ARec: PExpressionRec);
var
  TheNext: PExpressionRec;
  I: Integer;
begin
  if ARec <> nil then
    repeat
      TheNext := ARec^.Next;
      if ARec^.Res <> nil then
        ARec^.Res.Free;
      I := 0;
      while ARec^.ArgList[I] <> nil do
      begin
        FreeMem(ARec^.Args[I]);
        Inc(I);
      end;
      Dispose(ARec);
      ARec := TheNext;
    until ARec = nil;
end;

procedure TCustomExpressionParser.MakeLinkedList(var ExprRec: PExpressionRec;
  Memory: PPAnsiChar; MemoryPos: PPAnsiChar; MemSize: PInteger); // Was PPChar
var
  I: Integer;
begin
  // test function type
  if @ExprRec^.ExprWord.ExprFunc = nil then
  begin
    // special 'no function' function
    // indicates no function is present -> we can concatenate all instances
    // we don't create new arguments...these 'fall' through
    // use destination as we got it
    I := 0;
    while ExprRec^.ArgList[I] <> nil do
    begin
      // convert arguments to list
      MakeLinkedList(ExprRec^.ArgList[I], Memory, MemoryPos, MemSize);
      // goto next argument
      Inc(I);
    end;
  end else begin
    // inc memory pointer so we know if we are first
    ExprRec^.ResetDest := MemoryPos^ = Memory^;
    Inc(MemoryPos^);
    // convert arguments to list
    I := 0;
    while ExprRec^.ArgList[I] <> nil do
    begin
      // save variable type for easy access
      ExprRec^.ArgsType[I] := ExprRec^.ArgList[I]^.ExprWord.ResultType;
      // check if we need to copy argument, variables in general do not
      // need copying, except for fixed len strings which are not
      // null-terminated
//      if ExprRec^.ArgList[I].ExprWord.NeedsCopy then
//      begin
        // get memory for argument
        GetMem(ExprRec^.Args[I], ArgAllocSize);
        ExprRec^.ArgsPos[I] := ExprRec^.Args[I];
        ExprRec^.ArgsSize[I] := ArgAllocSize;
        MakeLinkedList(ExprRec^.ArgList[I], @ExprRec^.Args[I], @ExprRec^.ArgsPos[I],
            @ExprRec^.ArgsSize[I]);
//      end else begin
        // copy reference
//        ExprRec^.Args[I] := ExprRec^.ArgList[I].Args[0];
//        ExprRec^.ArgsPos[I] := ExprRec^.Args[I];
//        ExprRec^.ArgsSize[I] := 0;
//        FreeMem(ExprRec^.ArgList[I]);
//        ExprRec^.ArgList[I] := nil;
//      end;

      // goto next argument
      Inc(I);
    end;

    // link result to target argument
    ExprRec^.Res := TDynamicType.Create(Memory, MemoryPos, MemSize);
  end;

  // link to next operation
  if FCurrentRec = nil then
  begin
    FCurrentRec := ExprRec;
    FLastRec := ExprRec;
  end else begin
    FLastRec^.Next := ExprRec;
    FLastRec := ExprRec;
  end;
end;

function TCustomExpressionParser.MakeTree(Expr: TExprCollection; 
  FirstItem, LastItem: Integer): PExpressionRec;

{
- This is the most complex routine, it breaks down the expression and makes
  a linked tree which is used for fast function evaluations
- it is implemented recursively
}

var
  I, IArg, IStart, IEnd, lPrec, brCount: Integer;
  ExprWord: TExprWord;
  Valid: Boolean;
begin
  // detect empty brackets
  I := FirstItem;
  while I < LastItem do
  begin
    if (TExprWord(Expr.Items[I]).ResultType = etLeftBracket) and (TExprWord(Expr.Items[I + 1]).ResultType = etRightBracket) then
      if not((I > 0) and TExprWord(Expr.Items[I - 1]).IsFunction) then
        raise ExceptionClass.Create('Empty parentheses');
    Inc(I);
  end;

  // remove redundant brackets
  brCount := 0;
  while (FirstItem+brCount < LastItem) and (TExprWord(
      Expr.Items[FirstItem+brCount]).ResultType = etLeftBracket) do
    Inc(brCount);
  I := LastItem;
  while (I > FirstItem) and (TExprWord(
      Expr.Items[I]).ResultType = etRightBracket) do
    Dec(I);
  // test max of start and ending brackets
  if brCount > (LastItem-I) then
    brCount := LastItem-I;
  // count number of bracket pairs completely open from start to end
  // IArg is min.brCount
  I := FirstItem + brCount;
  IArg := brCount;
  while (I <= LastItem - brCount) and (brCount > 0) do
  begin
    case TExprWord(Expr.Items[I]).ResultType of
      etLeftBracket: Inc(brCount);
      etRightBracket: 
        begin
          Dec(brCount);
          if brCount < IArg then
            IArg := brCount;
        end;
    end;
    Inc(I);
  end;
  // useful pair bracket count, is in minimum, is IArg
  brCount := IArg;
  // check if subexpression closed within (bracket level will be zero)
  if brCount > 0 then
  begin
    Inc(FirstItem, brCount);
    Dec(LastItem, brCount);
  end;

  // check for empty range
  if LastItem < FirstItem then
  begin
    Result := nil;
    exit;
  end;

  // get new record
  Result := MakeRec;

  // simple constant, variable or function?
  if LastItem = FirstItem then
  begin
    Result^.ExprWord := TExprWord(Expr.Items[FirstItem]);
    if Result^.ExprWord.ResultType <> etComma then
    begin
      Result^.Oper := Result^.ExprWord.ExprFunc;
      exit;
    end;
  end;

  // no...more complex, find operator with lowest precedence
  brCount := 0;
  IArg := 0;
  IEnd := FirstItem-1;
  lPrec := -1;
  for I := FirstItem to LastItem do
  begin
    ExprWord := TExprWord(Expr.Items[I]);
//  if (brCount = 0) and ExprWord.IsOperator and (TFunction(ExprWord).OperPrec > lPrec) then
    if (brCount = 0) and ExprWord.IsOperator and (TFunction(ExprWord).OperPrec >= lPrec) then
    begin
      IEnd := I;
      lPrec := TFunction(ExprWord).OperPrec;
    end;
    case ExprWord.ResultType of
      etLeftBracket: Inc(brCount);
      etRightBracket: Dec(brCount);
    end;
  end;

  // operator found ?
  Valid := IEnd >= FirstItem;
  if Valid then
  begin
    // save operator
    Result^.ExprWord := TExprWord(Expr.Items[IEnd]);
    Result^.Oper := Result^.ExprWord.ExprFunc;
    // recurse into left part if present
    if IEnd > FirstItem then
    begin
      Result^.ArgList[IArg] := MakeTree(Expr, FirstItem, IEnd-1);
      Inc(IArg);
    end;
    // recurse into right part if present
    Valid := IEnd < LastItem;
    if Valid then
      Result^.ArgList[IArg] := MakeTree(Expr, IEnd+1, LastItem);
  end else 
  if TExprWord(Expr.Items[FirstItem]).IsFunction then 
  begin
    // save function
    Result^.ExprWord := TExprWord(Expr.Items[FirstItem]);
    Result^.Oper := Result^.ExprWord.ExprFunc;
    Result^.WantsFunction := true;
    // parse function arguments
    IEnd := FirstItem + 1;
    IStart := IEnd;
    brCount := 0;
    Valid := TExprWord(Expr.Items[IEnd]).ResultType = etLeftBracket;
    if Valid then
    begin
      // opening bracket found, first argument expression starts at next index
      Inc(brCount);
      Inc(IStart);
      while (IEnd < LastItem) and (brCount <> 0) do
      begin
        Inc(IEnd);
        case TExprWord(Expr.Items[IEnd]).ResultType of
          etLeftBracket: Inc(brCount);
          etComma:
            if brCount = 1 then
            begin
              // argument separation found, build tree of argument expression
              Result^.ArgList[IArg] := MakeTree(Expr, IStart, IEnd-1);
              Inc(IArg);
              IStart := IEnd + 1;
            end;
          etRightBracket: Dec(brCount);
        end;
      end;

      // check for extraneous words
      Valid := IEnd = LastItem;

      // parse last argument
      if Valid then
        Result^.ArgList[IArg] := MakeTree(Expr, IStart, IEnd-1);
    end;
  end;
  if not Valid then
    raise ExceptionClass.Create('Operator/function missing');
end;

procedure TCustomExpressionParser.ParseString(AnExpression: string; DestCollection: TExprCollection);
var
  isConstant: Boolean;
  I, I1, I2, Len: Integer;
  W, S: string;
  TempWord: TExprWord;
begin
  I2 := 1;
  S := Trim(AnExpression);
  Len := Length(S);
  repeat
    isConstant := false;
    I1 := I2;
    while (I1 < Len) and (S[I1] = ' ') do
      Inc(I1);
    I2 := I1;
    ReadWord(S, isConstant, I1, I2, Len);
    W := Trim(Copy(S, I1, I2 - I1));
    if isConstant then
    begin
      if W <> '' then
      begin
        TempWord := CreateConstant(W);
        DestCollection.Add(TempWord);
        FConstantsList.Add(TempWord);
      end;
    end
    else if Length(W) > 0 then
    begin
      I := -1;
      if FWordsList.Search(PChar(W), I) then // PChar intended here
      begin
        DestCollection.Add(FWordsList.Items[I])
      end else begin
        // unknown variable -> fire event
        HandleUnknownVariable(W);
        // try to search again
        if FWordsList.Search(PChar(W), I) then // PChar intended here
        begin
          DestCollection.Add(FWordsList.Items[I])
        end else begin
          raise ExceptionClass.Create('Unknown variable '''+W+''' found.');
        end;
      end;
    end;
  until I2 > Len;
end;

procedure TCustomExpressionParser.Check(AnExprList: TExprCollection);
var
  I, J, K, L: Integer;
begin
  AnExprList.Check(ExceptionClass);
  with AnExprList do
  begin
    I := 0;
    while I < Count do
    begin
      {----CHECK ON DOUBLE MINUS OR DOUBLE PLUS----}
      if ((TExprWord(Items[I]).Name = '-') or
        (TExprWord(Items[I]).Name = '+'))
        and ((I = 0) or
        (TExprWord(Items[I - 1]).ResultType = etComma) or
        (TExprWord(Items[I - 1]).ResultType = etLeftBracket) or
        (TExprWord(Items[I - 1]).IsOperator and (TExprWord(Items[I - 1]).MaxFunctionArg
        = 2))) then
      begin
        {replace e.g. ----1 with +1}
        if TExprWord(Items[I]).Name = '-' then
          K := -1
        else
          K := 1;
        L := 1;
        while (I + L < Count) and ((TExprWord(Items[I + L]).Name = '-')
          or (TExprWord(Items[I + L]).Name = '+')) and ((I + L = 0) or
          (TExprWord(Items[I + L - 1]).ResultType = etComma) or
          (TExprWord(Items[I + L - 1]).ResultType = etLeftBracket) or
          (TExprWord(Items[I + L - 1]).IsOperator and (TExprWord(Items[I + L -
          1]).MaxFunctionArg = 2))) do
        begin
          if TExprWord(Items[I + L]).Name = '-' then
            K := -1 * K;
          Inc(L);
        end;
        if L > 0 then
        begin
          Dec(L);
          for J := I + 1 to Count - 1 - L do
            Items[J] := Items[J + L];
          Count := Count - L;
        end;
        if K = -1 then
        begin
          if FWordsList.Search(pchar('-@'), J) then // PChar intended here
            Items[I] := FWordsList.Items[J];
        end
        else if FWordsList.Search(pchar('+@'), J) then // PChar intended here
          Items[I] := FWordsList.Items[J];
      end;
      {----CHECK ON DOUBLE NOT----}
      if (TExprWord(Items[I]).Name = 'not')
        and ((I = 0) or
        (TExprWord(Items[I - 1]).ResultType = etLeftBracket) or
        TExprWord(Items[I - 1]).IsOperator) then
      begin
        {replace e.g. not not 1 with 1}
        K := -1;
        L := 1;
        while (I + L < Count) and (TExprWord(Items[I + L]).Name = 'not') and ((I
          + L = 0) or
          (TExprWord(Items[I + L - 1]).ResultType = etLeftBracket) or
          TExprWord(Items[I + L - 1]).IsOperator) do
        begin
          K := -K;
          Inc(L);
        end;
        if L > 0 then
        begin
          if K = 1 then
          begin //remove all
            for J := I to Count - 1 - L do
              Items[J] := Items[J + L];
            Count := Count - L;
          end
          else
          begin //keep one
            Dec(L);
            for J := I + 1 to Count - 1 - L do
              Items[J] := Items[J + L];
            Count := Count - L;
          end
        end;
      end;
      {-----MISC CHECKS-----}
      if (TExprWord(Items[I]).IsVariable) and ((I < Count - 1) and
        (TExprWord(Items[I + 1]).IsVariable)) then
        raise ExceptionClass.Create('Missing operator between '''+TExprWord(Items[I]).Name+''' and '''+TExprWord(Items[I + 1]).Name+'''');
      if (TExprWord(Items[I]).ResultType = etLeftBracket) and (I >= Count - 1) then
        raise ExceptionClass.Create('Missing closing bracket');
      if (TExprWord(Items[I]).ResultType = etRightBracket) and ((I < Count - 1) and
        (TExprWord(Items[I + 1]).ResultType = etLeftBracket)) then
        raise ExceptionClass.Create('Missing operator between )(');
      if (TExprWord(Items[I]).ResultType = etRightBracket) and ((I < Count - 1) and
        (TExprWord(Items[I + 1]).IsVariable)) then
        raise ExceptionClass.Create('Missing operator between ) and constant/variable');
      if (TExprWord(Items[I]).ResultType = etLeftBracket) and ((I > 0) and
        (TExprWord(Items[I - 1]).IsVariable)) then
        raise ExceptionClass.Create('Missing operator between constant/variable and (');

      {-----CHECK ON INTPOWER------}
      if (TExprWord(Items[I]).Name = '^') and ((I < Count - 1) and
          (TExprWord(Items[I + 1]).ClassType = TIntegerConstant)) then
        if FWordsList.Search(PChar('^@'), J) then // PChar intended here
          Items[I] := FWordsList.Items[J]; //use the faster intPower if possible
      Inc(I);
    end;
  end;
end;

procedure TCustomExpressionParser.EvaluateCurrent;
var
  TempRec: PExpressionRec;
begin
  if FCurrentRec <> nil then
  begin
    // get current record
    TempRec := FCurrentRec;
    // execute list
    repeat
      with TempRec^ do
      begin
        if Assigned(@Oper) then
        begin
          // do we need to reset pointer?
          if ResetDest then
            Res.Rewind;

          IsNull := False;
          Oper(TempRec);
        end;

        // goto next
        TempRec := Next;
      end;
    until TempRec = nil;
  end;
end;

function TCustomExpressionParser.DefineFunction(AFunctName, AShortName, ADescription, ATypeSpec: string;
  AMinFunctionArg: Integer; AResultType: TExpressionType; AFuncAddress: TExprFunc): TExprWord;
begin
  Result := TFunction.Create(AFunctName, AShortName, ATypeSpec, AMinFunctionArg, AResultType, AFuncAddress, ADescription);
  FWordsList.Add(Result);
end;

function TCustomExpressionParser.DefineIntegerVariable(AVarName: string; AValue: PInteger): TExprWord;
begin
  Result := DefineIntegerVariable(AVarName, AValue, nil, nil);
end;

function TCustomExpressionParser.DefineIntegerVariable(AVarName: string; AValue: PInteger; AIsNullPtr: PBoolean; AFieldInfo: PVariableFieldInfo): TExprWord;
begin
  Result := TIntegerVariable.Create(AVarName, AValue, AIsNullPtr, AFieldInfo);
  FWordsList.Add(Result);
end;

{$ifdef SUPPORT_INT64}

function TCustomExpressionParser.DefineLargeIntVariable(AVarName: string; AValue: PLargeInt): TExprWord;
begin
  Result := DefineLargeIntVariable(AVarName, AValue, nil, nil);
end;

function TCustomExpressionParser.DefineLargeIntVariable(AVarName: string; AValue: PLargeInt; AIsNullPtr: PBoolean; AFieldInfo: PVariableFieldInfo): TExprWord;
begin
  Result := TLargeIntVariable.Create(AVarName, AValue, AIsNullPtr, AFieldInfo);
  FWordsList.Add(Result);
end;

{$endif}

function TCustomExpressionParser.DefineDateTimeVariable(AVarName: string; AValue: PDateTimeRec): TExprWord;
begin
  Result := DefineDateTimeVariable(AVarName, AValue, nil, nil);
end;

function TCustomExpressionParser.DefineDateTimeVariable(AVarName: string; AValue: PDateTimeRec; AIsNullPtr: PBoolean; AFieldInfo: PVariableFieldInfo): TExprWord;
begin
  Result := TDateTimeVariable.Create(AVarName, AValue, AIsNullPtr, AFieldInfo);
  FWordsList.Add(Result);
end;

function TCustomExpressionParser.DefineBooleanVariable(AVarName: string; AValue: PBoolean): TExprWord;
begin
  Result := DefineBooleanVariable(AVarName, AValue, nil, nil);
end;

function TCustomExpressionParser.DefineBooleanVariable(AVarName: string; AValue: PBoolean; AIsNullPtr: PBoolean; AFieldInfo: PVariableFieldInfo): TExprWord;
begin
  Result := TBooleanVariable.Create(AVarName, AValue, AIsNullPtr, AFieldInfo);
  FWordsList.Add(Result);
end;

function TCustomExpressionParser.DefineFloatVariable(AVarName: string; AValue: PDouble): TExprWord;
begin
  Result := DefineFloatVariable(AVarName, AValue, nil, nil);
end;

function TCustomExpressionParser.DefineFloatVariable(AVarName: string; AValue: PDouble; AIsNullPtr: PBoolean; AFieldInfo: PVariableFieldInfo): TExprWord;
begin
  Result := TFloatVariable.Create(AVarName, AValue, AIsNullPtr, AFieldInfo);
  FWordsList.Add(Result);
end;

function TCustomExpressionParser.DefineStringVariable(AVarName: string; AValue: PPAnsiChar): TExprWord;
begin
  Result := DefineStringVariable(AVarName, AValue, nil, nil);
end;

function TCustomExpressionParser.DefineStringVariable(AVarName: string; AValue: PPAnsiChar; AIsNullPtr: PBoolean; AFieldInfo: PVariableFieldInfo): TExprWord; // Was PPChar
begin
  Result := TStringVariable.Create(AVarName, AValue, AIsNullPtr, AFieldInfo);
  FWordsList.Add(Result);
end;

{
procedure TCustomExpressionParser.GetGeneratedVars(AList: TList);
var
  I: Integer;
begin
  AList.Clear;
  with FWordsList do
    for I := 0 to Count - 1 do
    begin
      if TObject(Items[I]).ClassType = TGeneratedVariable then
        AList.Add(Items[I]);
    end;
end;
}

function TCustomExpressionParser.GetResultType: TExpressionType;
begin
  Result := etUnknown;
  if FCurrentRec <> nil then
  begin
    //LAST operand should be boolean -otherwise If(,,) doesn't work
    while (FLastRec^.Next <> nil) do
      FLastRec := FLastRec^.Next;
    if FLastRec^.ExprWord <> nil then
      Result := FLastRec^.ExprWord.ResultType;
  end;
end;

function TCustomExpressionParser.IsIndex: Boolean;
begin
  Result:= False;
end;

procedure TCustomExpressionParser.OptimizeExpr(var ExprRec: PExpressionRec);
begin
  RemoveConstants(ExprRec);
end;

function TCustomExpressionParser.ExceptionClass: ExceptClass;
begin
  Result := EParserError;
end;

procedure TCustomExpressionParser.ReadWord(const AnExpr: string;
  var isConstant: Boolean; var I1, I2: Integer; Len: Integer);
var
  I: Integer;
  OldI2: Integer;
  constChar: Char;

  procedure ReadConstant(AnExpr: string; isHex: Boolean);
  begin
    isConstant := true;

    while (I2 <= Len) and (CharInSet(AnExpr[I2], ['0'..'9']) or
      (isHex and CharInSet(AnExpr[I2], ['a'..'f', 'A'..'F']))) do
      Inc(I2);
    if I2 <= Len then
    begin
      if AnExpr[I2] = FDecimalSeparator then
      begin
        Inc(I2);
        while (I2 <= Len) and CharInSet(AnExpr[I2], ['0'..'9']) do
          Inc(I2);
      end;
      if (I2 <= Len) and (AnExpr[I2] = 'e') then
      begin
        Inc(I2);
        if (I2 <= Len) and CharInSet(AnExpr[I2], ['+', '-']) then
          Inc(I2);
        while (I2 <= Len) and CharInSet(AnExpr[I2], ['0'..'9']) do
          Inc(I2);
      end;
    end;
  end;

begin
  if I1 <= Len then
  begin
    if AnExpr[I2] = HexChar then
    begin
      Inc(I2);
      OldI2 := I2;
      ReadConstant(AnExpr, true);
      if I2 = OldI2 then
      begin
        isConstant := false;
        while (I2 <= Len) and CharInSet(AnExpr[I2], ['a'..'z', 'A'..'Z', '_', '0'..'9']) do
          Inc(I2);
      end;
    end
    else if AnExpr[I2] = FDecimalSeparator then
      ReadConstant(AnExpr, false)
    else
      // String constants can be delimited by ' or "
      // but need not be - see below
      // To use a delimiter inside the string, double it up to escape it
      case AnExpr[I2] of
        '''', '"':
          begin
            isConstant := true;
            constChar := AnExpr[I2];
            Inc(I2);
            while (I2 <= Len) do begin
              // Regular character?
              if (AnExpr[I2] <> constChar) then
                Inc(I2)
              else begin
                // we do have a const, now check for escaped consts
                if (I2 + 1 <= Len) and
                  (AnExpr[I2 + 1] = constChar) then begin
                  Inc(I2,2) //skip past, deal with duplicates later
                end else begin
                  // at the trailing delimiter
                  Inc(I2); //move past delimiter
                  break;
                end;
              end;
            end;
          end;
        // However string constants can also appear without delimiters
        'a'..'z', 'A'..'Z', '_':
          begin
            while (I2 <= Len) and CharInSet(AnExpr[I2], ['a'..'z', 'A'..'Z', '_', '0'..'9']) do
              Inc(I2);
          end;
        '>', '<':
          begin
            if (I2 <= Len) then
              Inc(I2);
            if CharInSet(AnExpr[I2], ['=', '<', '>']) then
              Inc(I2);
          end;
        '=':
          begin
            if (I2 <= Len) then
              Inc(I2);
            if CharInSet(AnExpr[I2], ['=', '<', '>']) then
              Inc(I2);
          end;
        '&':
          begin
            if (I2 <= Len) then
              Inc(I2);
            if CharInSet(AnExpr[I2], ['&']) then
              Inc(I2);
          end;
        '|':
          begin
            if (I2 <= Len) then
              Inc(I2);
            if CharInSet(AnExpr[I2], ['|']) then
              Inc(I2);
          end;
        ':':
          begin
            if (I2 <= Len) then
              Inc(I2);
            if AnExpr[I2] = '=' then
              Inc(I2);
          end;
        '!':
          begin
            if (I2 <= Len) then
              Inc(I2);
            if AnExpr[I2] = '=' then //support for !=
              Inc(I2);
          end;
        '+':
          begin
            Inc(I2);
            I := -1;
            if (I2 <= Len) then
              if (AnExpr[I2] = '+') and FWordsList.Search(PChar('++'), I) then // PChar intended here
                Inc(I2);
          end;
        '-':
          begin
            Inc(I2);
            I := -1;
            if (I2 <= Len) then
              if (AnExpr[I2] = '-') and FWordsList.Search(PChar('--'), I) then // PChar intended here
                Inc(I2);
          end;
        '^', '/', '\', '*', '(', ')', '%', '~', '$':
          Inc(I2);
        '0'..'9':
          ReadConstant(AnExpr, false);
      else
        begin
          Inc(I2);
        end;
      end;
  end;
end;

function TCustomExpressionParser.CreateConstant(W: string): TConstant;
var
  DecSep: Integer;
  AInteger: Integer;
  Code: Integer;
begin
  if W[1] = HexChar then
  begin
    // convert hexadecimal to decimal
    W[1] := '$';
    W := IntToStr({$ifdef SUPPORT_INT64}StrToInt64{$else}StrToInt{$endif}(W));
  end;
  if (W[1] = '''') or (W[1] = '"') then begin
     // StringConstant will handle any escaped quotes
    Result := TStringConstant.Create(W);
  end else begin
    DecSep := Pos(FDecimalSeparator, W);
    if (DecSep > 0) then
    begin
  {$IFDEF ENG_NUMBERS}
      // we'll have to convert FDecimalSeparator into DecimalSeparator
      // otherwise the OS will not understand what we mean
      W[DecSep] := DecimalSeparator{%H-};
  {$ENDIF}
      Result := TFloatConstant.Create(W, W)
    end else begin
      {$ifdef SUPPORT_INT64}
      Val(W, AInteger, Code);
      if Code=0 then
        Result := TIntegerConstant.Create(AInteger)
      else
        Result := TLargeIntConstant.Create(StrToInt64(W));
      {$else}
      Result := TIntegerConstant.Create(StrToInt(W))
      {$endif}
    end;
  end;
end;

function TCustomExpressionParser.MakeRec: PExpressionRec;
var
  I: Integer;
begin
  New(Result);
  Result^.Oper := nil;
  Result^.AuxData := nil;
  Result^.WantsFunction := false;
  for I := 0 to MaxArg - 1 do
  begin
    Result^.Args[I] := nil;
    Result^.ArgsPos[I] := nil;
    Result^.ArgsSize[I] := 0;
    Result^.ArgsType[I] := etUnknown;
    Result^.ArgList[I] := nil;
  end;
  Result^.Res := nil;
  Result^.Next := nil;
  Result^.ExprWord := nil;
  Result^.ResetDest := false;
  Result^.ExpressionContext := @FExpressionContext;
  Result^.IsNull := False;
  Result^.IsNullPtr := nil;
end;

procedure TCustomExpressionParser.Evaluate(AnExpression: string);
begin
  if Length(AnExpression) > 0 then
  begin
    AddExpression(AnExpression);
    EvaluateCurrent;
  end;
end;

function TCustomExpressionParser.AddExpression(AnExpression: string): Integer;
begin
  if Length(AnExpression) > 0 then
  begin
    Result := 0;
    CompileExpression(AnExpression);
  end else
    Result := -1;
  //CurrentIndex := Result;
end;

procedure TCustomExpressionParser.ClearExpressions;
begin
  DisposeList(FCurrentRec);
  FCurrentRec := nil;
  FLastRec := nil;
end;

function TCustomExpressionParser.GetFunctionDescription(AFunction: string):
  string;
var
  S: string;
  p, I: Integer;
begin
  S := AFunction;
  p := Pos('(', S);
  if p > 0 then
    S := Copy(S, 1, p - 1);
  I := -1;
  if FWordsList.Search(pchar(S), I) then // PChar intended here
    Result := TExprWord(FWordsList.Items[I]).Description
  else
    Result := EmptyStr;
end;

procedure TCustomExpressionParser.GetFunctionNames(AList: TStrings);
var
  I, J: Integer;
  S: string;
begin
  with FWordsList do
    for I := 0 to Count - 1 do
      with TExprWord(FWordsList.Items[I]) do
        if Length(Description) > 0 then
        begin
          S := Name;
          if MaxFunctionArg > 0 then
          begin
            S := S + '(';
            for J := 0 to MaxFunctionArg - 2 do
              S := S + ArgSeparator;
            S := S + ')';
          end;
          AList.Add(S);
        end;
end;


//--Expression functions-----------------------------------------------------

(*
procedure FuncFloatToStr(Param: PExpressionRec);
var
  width, numDigits, resWidth: Integer;
  extVal: Extended;
begin
  // get params;
  numDigits := 0;
  if Param^.Args[1] <> nil then
    width := PInteger(Param^.Args[1])^
  else
    width := 18;
  if Param^.Args[2] <> nil then
    numDigits := PInteger(Param^.Args[2])^;
  // convert to string
  Param^.Res.AssureSpace(width);
  extVal := PDouble(Param^.Args[0])^;
  resWidth := dbfFloatToText(Param^.Res.MemoryPos^, extVal, {$ifndef FPC_VERSION}fvExtended,{$endif} ffFixed, 18, numDigits);
  // always use dot as decimal separator
  if numDigits > 0 then
    Param^.Res.MemoryPos^[resWidth-numDigits-1] := '.';
  // result width smaller than requested width? -> add space to compensate
  if (Param^.Args[1] <> nil) and (resWidth < width) then
  begin
    // move string so that it's right-aligned
    Move(Param^.Res.MemoryPos^^, (Param^.Res.MemoryPos^)[width-resWidth], resWidth);
    // fill gap with spaces
    FillChar(Param^.Res.MemoryPos^^, width-resWidth, ' ');
    // resWidth has been padded, update
    resWidth := width;
  end else if resWidth > width then begin
    // result width more than requested width, cut
    resWidth := width;
  end;
  // advance pointer
  Inc(Param^.Res.MemoryPos^, resWidth);
  // null-terminate
  Param^.Res.MemoryPos^^ := #0;
end;

procedure FuncIntToStr_Gen(Param: PExpressionRec; Val: {$ifdef SUPPORT_INT64}Int64{$else}Integer{$endif});
var
  width: Integer;
begin
  // width specified?
  if Param^.Args[1] <> nil then
  begin
    // convert to string
    width := PInteger(Param^.Args[1])^;
{$ifdef SUPPORT_INT64}
    GetStrFromInt64_Width
{$else}
    GetStrFromInt_Width
{$endif}
      (Val, width, Param^.Res.MemoryPos^, #32); // AnsiChar cast removed
    // advance pointer
    Inc(Param^.Res.MemoryPos^, width);
    // need to add decimal?
    if Param^.Args[2] <> nil then
    begin
      // get number of digits
      width := PInteger(Param^.Args[2])^;
      // add decimal dot
      Param^.Res.MemoryPos^^ := '.';
      Inc(Param^.Res.MemoryPos^);
      // add zeroes
      FillChar(Param^.Res.MemoryPos^^, width, '0');
      // go to end
      Inc(Param^.Res.MemoryPos^, width);
    end;
  end else begin
    // convert to string
    width :=
{$ifdef SUPPORT_INT64}
      GetStrFromInt64
{$else}
      GetStrFromInt
{$endif}
        (Val, Param^.Res.MemoryPos^);
    // advance pointer
    Inc(Param^.Res.MemoryPos^, width);
  end;
  // null-terminate
  Param^.Res.MemoryPos^^ := #0;
end;

procedure FuncIntToStr(Param: PExpressionRec);
begin
  FuncIntToStr_Gen(Param, PInteger(Param^.Args[0])^);
end;

{$ifdef SUPPORT_INT64}

procedure FuncInt64ToStr(Param: PExpressionRec);
begin
  FuncIntToStr_Gen(Param, PInt64(Param^.Args[0])^);
end;

{$endif}
*)

procedure FuncStr(Param: PExpressionRec);
var
  Size: Integer;
  Precision: Integer;
  PadChar: AnsiChar;
{$ifdef SUPPORT_INT64}
  IntValue: Int64;
{$else}
  IntValue: Integer;
{$endif}
  FloatValue: Extended;
  Len: Integer;
begin
  if Param^.Args[1] <> nil then
    Size := PInteger(Param^.Args[1])^
  else
  begin
    case Param^.ArgsType[0] of
      etInteger: Size := 11;
      etLargeInt: Size := 20;
    else
      Size := 10;
    end;
  end;
  if Param^.Args[2] <> nil then
    Precision := PInteger(Param^.Args[2])^
  else
    Precision := 0;
  if Param^.Args[3] <> nil then
    PadChar := Param^.Args[0]^
  else
    PadChar := #0;
  if PadChar = #0 then
    PadChar := ' ';
  Param^.Res.AssureSpace(Succ(Size));
  if (Precision = 0) and (Param^.ArgsType[0] in [etInteger, etLargeInt]) then
  begin
{$ifdef SUPPORT_INT64}
    if Param^.ArgsType[0] = etLargeInt then
      IntValue := PInt64(Param^.Args[0])^
    else
{$endif}
      IntValue := PInteger(Param^.Args[0])^;
    Len := IntToStrWidth(IntValue, Size, Param^.Res.MemoryPos^, True, PadChar);
  end
  else
  begin
    FloatValue := PDouble(Param^.Args[0])^;
    Len := FloatToStrWidth(FloatValue, Size, Precision, Param^.Res.MemoryPos^, True);
  end;
  Inc(Param^.Res.MemoryPos^, Len);
  Param^.Res.MemoryPos^^ := #0;
end;

procedure FuncDateToStr(Param: PExpressionRec);
var
  TempStr: AnsiString;
begin
  // create in temporary string
  TempStr := AnsiString(FormatDateTime('YYYYMMDD', PDateTimeRec(Param^.Args[0])^.DateTime));
  if Param^.ArgList[0]^.IsNullPtr^ then
    FillChar(PAnsiChar(TempStr)^, Length(TempStr), ' ');
  // copy to buffer
  Param^.Res.Append(PAnsiChar(TempStr), Length(TempStr)); // Was PChar
end;

procedure FuncSubString(Param: PExpressionRec);
var
  srcLen, index, count: Integer;
begin
  srcLen := dbfStrLen(Param^.Args[0]);
  index := PInteger(Param^.Args[1])^ - 1;
  if Param^.Args[2] <> nil then
  begin
    count := PInteger(Param^.Args[2])^;
    if index + count > srcLen then
      count := srcLen - index;
  end else
    count := srcLen - index;
  Param^.Res.Append(Param^.Args[0]+index, count)
end;

procedure FuncLeftString(Param: PExpressionRec);
var
  srcLen, index, count: Integer;
begin
  srcLen := dbfStrLen(Param^.Args[0]);
  index := 0;
  count := PInteger(Param^.Args[1])^;
  if index + count > srcLen then
    count := srcLen - index;
  Param^.Res.Append(Param^.Args[0]+index, count)
end;

procedure FuncUppercase(Param: PExpressionRec);
var
  Len: integer;
  Arg0: PAnsiChar; // Was PChar
begin
  // first copy
  Arg0 := Param^.Args[0];
  Len := dbfStrLen(Arg0);
  Param^.Res.Append(Arg0, Len);
  // Append may have reallocated memory,
  // but correct for "Inc(FMemoryPos^, Length);"
  Arg0 := (Param^.Res.MemoryPos)^;
  Dec(Arg0, Len);
  // make uppercase
  dbfStrUpper(Arg0);
end;

procedure FuncLowercase(Param: PExpressionRec);
var
  Len: integer;
  Arg0: PAnsiChar; // Was PChar
begin
  // first copy
  Arg0 := Param^.Args[0];
  Len := dbfStrLen(Arg0);
  Param^.Res.Append(Arg0, Len);
  // Append may have reallocated memory,
  // but correct for "Inc(FMemoryPos^, Length);"
  Arg0 := (Param^.Res.MemoryPos)^;
  Dec(Arg0, Len);
  // make lowercase
  dbfStrLower(Arg0);
end;

procedure FuncNegative_F_F(Param: PExpressionRec);
begin
  with Param^ do
    PDouble(Res.MemoryPos^)^ := -PDouble(Args[0])^;
end;

procedure FuncNegative_I_I(Param: PExpressionRec);
begin
  with Param^ do
    PInteger(Res.MemoryPos^)^ := -PInteger(Args[0])^;
end;

{$ifdef SUPPORT_INT64}
procedure FuncNegative_L_L(Param: PExpressionRec);
begin
  with Param^ do
    PInt64(Res.MemoryPos^)^ := -PInt64(Args[0])^;
end;
{$endif}

procedure FuncAddSub_CheckNull(Param: PExpressionRec);
begin
  if (Param^.ArgList[0]^.IsNullPtr^) and (Param^.ArgList[1]^.IsNullPtr^) then
    Param^.IsNull := True;
end;

procedure FuncAdd_F_FF(Param: PExpressionRec);
begin
  PDouble(Param^.Res.MemoryPos^)^ := PDouble(Param^.Args[0])^ + PDouble(Param^.Args[1])^;
  FuncAddSub_CheckNull(Param);
end;

procedure FuncAdd_F_FI(Param: PExpressionRec);
begin
  PDouble(Param^.Res.MemoryPos^)^ := PDouble(Param^.Args[0])^ + PInteger(Param^.Args[1])^;
  FuncAddSub_CheckNull(Param);
end;

procedure FuncAdd_F_II(Param: PExpressionRec);
begin
  PInteger(Param^.Res.MemoryPos^)^ := PInteger(Param^.Args[0])^ + PInteger(Param^.Args[1])^;
  FuncAddSub_CheckNull(Param);
end;

procedure FuncAdd_F_IF(Param: PExpressionRec);
begin
  PDouble(Param^.Res.MemoryPos^)^ := PInteger(Param^.Args[0])^ + PDouble(Param^.Args[1])^;
  FuncAddSub_CheckNull(Param);
end;

{$ifdef SUPPORT_INT64}

procedure FuncAdd_F_FL(Param: PExpressionRec);
begin
  PDouble(Param^.Res.MemoryPos^)^ := PDouble(Param^.Args[0])^ + PInt64(Param^.Args[1])^;
  FuncAddSub_CheckNull(Param);
end;

procedure FuncAdd_F_IL(Param: PExpressionRec);
begin
  PInt64(Param^.Res.MemoryPos^)^ := PInteger(Param^.Args[0])^ + PInt64(Param^.Args[1])^;
  FuncAddSub_CheckNull(Param);
end;

procedure FuncAdd_F_LL(Param: PExpressionRec);
begin
  PInt64(Param^.Res.MemoryPos^)^ := PInt64(Param^.Args[0])^ + PInt64(Param^.Args[1])^;
  FuncAddSub_CheckNull(Param);
end;

procedure FuncAdd_F_LF(Param: PExpressionRec);
begin
  PDouble(Param^.Res.MemoryPos^)^ := PInt64(Param^.Args[0])^ + PDouble(Param^.Args[1])^;
  FuncAddSub_CheckNull(Param);
end;

procedure FuncAdd_F_LI(Param: PExpressionRec);
begin
  PInt64(Param^.Res.MemoryPos^)^ := PInt64(Param^.Args[0])^ + PInteger(Param^.Args[1])^;
  FuncAddSub_CheckNull(Param);
end;

{$endif}

procedure FuncSub_F_FF(Param: PExpressionRec);
begin
  PDouble(Param^.Res.MemoryPos^)^ := PDouble(Param^.Args[0])^ - PDouble(Param^.Args[1])^;
  FuncAddSub_CheckNull(Param);
end;

procedure FuncSub_F_FI(Param: PExpressionRec);
begin
  PDouble(Param^.Res.MemoryPos^)^ := PDouble(Param^.Args[0])^ - PInteger(Param^.Args[1])^;
  FuncAddSub_CheckNull(Param);
end;

procedure FuncSub_F_II(Param: PExpressionRec);
begin
  PInteger(Param^.Res.MemoryPos^)^ := PInteger(Param^.Args[0])^ - PInteger(Param^.Args[1])^;
  FuncAddSub_CheckNull(Param);
end;

procedure FuncSub_F_IF(Param: PExpressionRec);
begin
  PDouble(Param^.Res.MemoryPos^)^ := PInteger(Param^.Args[0])^ - PDouble(Param^.Args[1])^;
  FuncAddSub_CheckNull(Param);
end;

procedure FuncSub_F_DD(Param: PExpressionRec);
begin
  PDouble(Param^.Res.MemoryPos^)^ := PDateTime(Param^.Args[0])^ - PDateTime(Param^.Args[1])^;
  FuncAddSub_CheckNull(Param);
end;

{$ifdef SUPPORT_INT64}

procedure FuncSub_D_DL(Param: PExpressionRec);
begin
  PDateTime(Param^.Res.MemoryPos^)^ := PDateTime(Param^.Args[0])^ - PLargeInt(Param^.Args[1])^;
  FuncAddSub_CheckNull(Param);
end;

procedure FuncSub_F_FL(Param: PExpressionRec);
begin
  PDouble(Param^.Res.MemoryPos^)^ := PDouble(Param^.Args[0])^ - PInt64(Param^.Args[1])^;
  FuncAddSub_CheckNull(Param);
end;

procedure FuncSub_F_IL(Param: PExpressionRec);
begin
  PInt64(Param^.Res.MemoryPos^)^ := PInteger(Param^.Args[0])^ - PInt64(Param^.Args[1])^;
  FuncAddSub_CheckNull(Param);
end;

procedure FuncSub_F_LL(Param: PExpressionRec);
begin
  PInt64(Param^.Res.MemoryPos^)^ := PInt64(Param^.Args[0])^ - PInt64(Param^.Args[1])^;
  FuncAddSub_CheckNull(Param);
end;

procedure FuncSub_F_LF(Param: PExpressionRec);
begin
  PDouble(Param^.Res.MemoryPos^)^ := PInt64(Param^.Args[0])^ - PDouble(Param^.Args[1])^;
  FuncAddSub_CheckNull(Param);
end;

procedure FuncSub_F_LI(Param: PExpressionRec);
begin
  PInt64(Param^.Res.MemoryPos^)^ := PInt64(Param^.Args[0])^ - PInteger(Param^.Args[1])^;
  FuncAddSub_CheckNull(Param);
end;

{$endif}

procedure FuncMul_F_FF(Param: PExpressionRec);
begin
  PDouble(Param^.Res.MemoryPos^)^ := PDouble(Param^.Args[0])^ * PDouble(Param^.Args[1])^;
end;

procedure FuncMul_F_FI(Param: PExpressionRec);
begin
  PDouble(Param^.Res.MemoryPos^)^ := PDouble(Param^.Args[0])^ * PInteger(Param^.Args[1])^;
end;

procedure FuncMul_F_II(Param: PExpressionRec);
begin
  PInteger(Param^.Res.MemoryPos^)^ := PInteger(Param^.Args[0])^ * PInteger(Param^.Args[1])^;
end;

procedure FuncMul_F_IF(Param: PExpressionRec);
begin
  PDouble(Param^.Res.MemoryPos^)^ := PInteger(Param^.Args[0])^ * PDouble(Param^.Args[1])^;
end;

{$ifdef SUPPORT_INT64}

procedure FuncMul_F_FL(Param: PExpressionRec);
begin
  PDouble(Param^.Res.MemoryPos^)^ := PDouble(Param^.Args[0])^ * PInt64(Param^.Args[1])^;
end;

procedure FuncMul_F_IL(Param: PExpressionRec);
begin
  PInt64(Param^.Res.MemoryPos^)^ := PInteger(Param^.Args[0])^ * PInt64(Param^.Args[1])^;
end;

procedure FuncMul_F_LL(Param: PExpressionRec);
begin
  PInt64(Param^.Res.MemoryPos^)^ := PInt64(Param^.Args[0])^ * PInt64(Param^.Args[1])^;
end;

procedure FuncMul_F_LF(Param: PExpressionRec);
begin
  PDouble(Param^.Res.MemoryPos^)^ := PInt64(Param^.Args[0])^ * PDouble(Param^.Args[1])^;
end;

procedure FuncMul_F_LI(Param: PExpressionRec);
begin
  PInt64(Param^.Res.MemoryPos^)^ := PInt64(Param^.Args[0])^ * PInteger(Param^.Args[1])^;
end;

{$endif}

procedure FuncDiv_F_FF(Param: PExpressionRec);
begin
  PDouble(Param^.Res.MemoryPos^)^ := PDouble(Param^.Args[0])^ / PDouble(Param^.Args[1])^;
end;

procedure FuncDiv_F_FI(Param: PExpressionRec);
begin
  PDouble(Param^.Res.MemoryPos^)^ := PDouble(Param^.Args[0])^ / PInteger(Param^.Args[1])^;
end;

procedure FuncDiv_F_II(Param: PExpressionRec);
begin
  PInteger(Param^.Res.MemoryPos^)^ := PInteger(Param^.Args[0])^ div PInteger(Param^.Args[1])^;
end;

procedure FuncDiv_F_IF(Param: PExpressionRec);
begin
  PDouble(Param^.Res.MemoryPos^)^ := PInteger(Param^.Args[0])^ / PDouble(Param^.Args[1])^;
end;

{$ifdef SUPPORT_INT64}

procedure FuncDiv_F_FL(Param: PExpressionRec);
begin
  PDouble(Param^.Res.MemoryPos^)^ := PDouble(Param^.Args[0])^ / PInt64(Param^.Args[1])^;
end;

procedure FuncDiv_F_IL(Param: PExpressionRec);
begin
  PInt64(Param^.Res.MemoryPos^)^ := PInteger(Param^.Args[0])^ div PInt64(Param^.Args[1])^;
end;

procedure FuncDiv_F_LL(Param: PExpressionRec);
begin
  PInt64(Param^.Res.MemoryPos^)^ := PInt64(Param^.Args[0])^ div PInt64(Param^.Args[1])^;
end;

procedure FuncDiv_F_LF(Param: PExpressionRec);
begin
  PDouble(Param^.Res.MemoryPos^)^ := PInt64(Param^.Args[0])^ / PDouble(Param^.Args[1])^;
end;

procedure FuncDiv_F_LI(Param: PExpressionRec);
begin
  PInt64(Param^.Res.MemoryPos^)^ := PInt64(Param^.Args[0])^ div PInteger(Param^.Args[1])^;
end;

{$endif}

procedure FuncStrI_EQ(Param: PExpressionRec);
begin
  Param^.Res.MemoryPos^^ := AnsiChar(dbfStrIComp(Param^.Args[0], Param^.Args[1]) = 0); // Was Char
end;

procedure FuncStrIP_EQ(Param: PExpressionRec);
var
  arg0len, arg1len: integer;
  match: boolean;
  str0, str1: AnsiString; // Was string
begin
  arg1len := dbfStrLen(Param^.Args[1]);
  if Param^.Args[1][0] = '*' then
  begin
    if Param^.Args[1][arg1len-1] = '*' then
    begin
      str0 := dbfStrUpper(Param^.Args[0]);
      str1 := dbfStrUpper(Param^.Args[1]+1);
      setlength(str1, arg1len-2);
      match := Pos(str1, str0)>0; // Was AnsiPos(str0, str1) = 0
    end else begin
      arg0len := dbfStrLen(Param^.Args[0]);
      // at least length without asterisk
      match := arg0len >= arg1len - 1;
      if match then
        match := dbfStrLIComp(Param^.Args[0]+(arg0len-arg1len+1), Param^.Args[1]+1, arg1len-1) = 0;
    end;
  end else
  if Param^.Args[1][arg1len-1] = '*' then
  begin
    arg0len := dbfStrLen(Param^.Args[0]);
    match := arg0len >= arg1len - 1;
    if match then
      match := dbfStrLIComp(Param^.Args[0], Param^.Args[1], arg1len-1) = 0;
  end else begin
    match := dbfStrIComp(Param^.Args[0], Param^.Args[1]) = 0;
  end;
  Param^.Res.MemoryPos^^ := AnsiChar(match); // Was Char
end;

procedure FuncStrI_NEQ(Param: PExpressionRec);
begin
  Param^.Res.MemoryPos^^ := AnsiChar(dbfStrIComp(Param^.Args[0], Param^.Args[1]) <> 0); // Was Char
end;

procedure FuncStrI_LT(Param: PExpressionRec);
begin
  Param^.Res.MemoryPos^^ := AnsiChar(dbfStrIComp(Param^.Args[0], Param^.Args[1]) < 0); // Was Char
end;

procedure FuncStrI_GT(Param: PExpressionRec);
begin
  Param^.Res.MemoryPos^^ := AnsiChar(dbfStrIComp(Param^.Args[0], Param^.Args[1]) > 0); // Was Char
end;

procedure FuncStrI_LTE(Param: PExpressionRec);
begin
  Param^.Res.MemoryPos^^ := AnsiChar(dbfStrIComp(Param^.Args[0], Param^.Args[1]) <= 0); // Was Char
end;

procedure FuncStrI_GTE(Param: PExpressionRec);
begin
  Param^.Res.MemoryPos^^ := AnsiChar(dbfStrIComp(Param^.Args[0], Param^.Args[1]) >= 0); // Was Char
end;

procedure FuncStrP_EQ(Param: PExpressionRec);
var
  arg0len, arg1len: integer;
  match: boolean;
begin
  arg1len := dbfStrLen(Param^.Args[1]);
  if Param^.Args[1][0] = '*' then
  begin
    if Param^.Args[1][arg1len-1] = '*' then
    begin
      Param^.Args[1][arg1len-1] := #0;
      match := dbfStrPos(Param^.Args[0], Param^.Args[1]+1) <> nil;
      Param^.Args[1][arg1len-1] := '*';
    end else begin
      arg0len := dbfStrLen(Param^.Args[0]);
      // at least length without asterisk
      match := arg0len >= arg1len - 1;
      if match then
        match := dbfStrLComp(Param^.Args[0]+(arg0len-arg1len+1), Param^.Args[1]+1, arg1len-1) = 0;
    end;
  end else
  if Param^.Args[1][arg1len-1] = '*' then
  begin
    arg0len := dbfStrLen(Param^.Args[0]);
    match := arg0len >= arg1len - 1;
    if match then
      match := dbfStrLComp(Param^.Args[0], Param^.Args[1], arg1len-1) = 0;
  end else begin
    match := dbfStrComp(Param^.Args[0], Param^.Args[1]) = 0;
  end;
  Param^.Res.MemoryPos^^ := AnsiChar(match); // Was Char
end;

procedure FuncStr_EQ(Param: PExpressionRec);
begin
  Param^.Res.MemoryPos^^ := AnsiChar(dbfStrComp(Param^.Args[0], Param^.Args[1]) = 0); // Was Char
end;

procedure FuncStr_NEQ(Param: PExpressionRec);
begin
  Param^.Res.MemoryPos^^ := AnsiChar(dbfStrComp(Param^.Args[0], Param^.Args[1]) <> 0); // Was Char
end;

procedure FuncStr_LT(Param: PExpressionRec);
begin
  Param^.Res.MemoryPos^^ := AnsiChar(dbfStrComp(Param^.Args[0], Param^.Args[1]) < 0);  // Was Char
end;

procedure FuncStr_GT(Param: PExpressionRec);
begin
  Param^.Res.MemoryPos^^ := AnsiChar(dbfStrComp(Param^.Args[0], Param^.Args[1]) > 0); // Was Char
end;

procedure FuncStr_LTE(Param: PExpressionRec);
begin
  Param^.Res.MemoryPos^^ := AnsiChar(dbfStrComp(Param^.Args[0], Param^.Args[1]) <= 0); // Was Char
end;

procedure FuncStr_GTE(Param: PExpressionRec);
begin
  Param^.Res.MemoryPos^^ := AnsiChar(dbfStrComp(Param^.Args[0], Param^.Args[1]) >= 0); // Was Char
end;

procedure Func_FF_EQ(Param: PExpressionRec);
begin
  Param^.Res.MemoryPos^^ := AnsiChar(PDouble(Param^.Args[0])^   =  PDouble(Param^.Args[1])^); // Was Char
end;

procedure Func_FF_NEQ(Param: PExpressionRec);
begin
  Param^.Res.MemoryPos^^ := AnsiChar(PDouble(Param^.Args[0])^   <> PDouble(Param^.Args[1])^); // Was Char
end;

procedure Func_FF_LT(Param: PExpressionRec);
begin
  Param^.Res.MemoryPos^^ := AnsiChar(PDouble(Param^.Args[0])^   <  PDouble(Param^.Args[1])^); // Was Char
end;

procedure Func_FF_GT(Param: PExpressionRec);
begin
  Param^.Res.MemoryPos^^ := AnsiChar(PDouble(Param^.Args[0])^   >  PDouble(Param^.Args[1])^); // Was Char
end;

procedure Func_FF_LTE(Param: PExpressionRec);
begin
  Param^.Res.MemoryPos^^ := AnsiChar(PDouble(Param^.Args[0])^   <= PDouble(Param^.Args[1])^); // Was Char
end;

procedure Func_FF_GTE(Param: PExpressionRec);
begin
  Param^.Res.MemoryPos^^ := AnsiChar(PDouble(Param^.Args[0])^   >= PDouble(Param^.Args[1])^); // Was Char
end;

procedure Func_FI_EQ(Param: PExpressionRec);
begin
  Param^.Res.MemoryPos^^ := AnsiChar(PDouble(Param^.Args[0])^   =  PInteger(Param^.Args[1])^); // Was Char
end;

procedure Func_FI_NEQ(Param: PExpressionRec);
begin
  Param^.Res.MemoryPos^^ := AnsiChar(PDouble(Param^.Args[0])^   <> PInteger(Param^.Args[1])^); // Was Char
end;

procedure Func_FI_LT(Param: PExpressionRec);
begin
  Param^.Res.MemoryPos^^ := AnsiChar(PDouble(Param^.Args[0])^   <  PInteger(Param^.Args[1])^); // Was Char
end;

procedure Func_FI_GT(Param: PExpressionRec);
begin
  Param^.Res.MemoryPos^^ := AnsiChar(PDouble(Param^.Args[0])^   >  PInteger(Param^.Args[1])^); // Was Char
end;

procedure Func_FI_LTE(Param: PExpressionRec);
begin
  Param^.Res.MemoryPos^^ := AnsiChar(PDouble(Param^.Args[0])^   <= PInteger(Param^.Args[1])^); // Was Char
end;

procedure Func_FI_GTE(Param: PExpressionRec);
begin
  Param^.Res.MemoryPos^^ := AnsiChar(PDouble(Param^.Args[0])^   >= PInteger(Param^.Args[1])^); // Was Char
end;

procedure Func_II_EQ(Param: PExpressionRec);
begin
  Param^.Res.MemoryPos^^ := AnsiChar(PInteger(Param^.Args[0])^  =  PInteger(Param^.Args[1])^); // Was Char
end;

procedure Func_II_NEQ(Param: PExpressionRec);
begin
  Param^.Res.MemoryPos^^ := AnsiChar(PInteger(Param^.Args[0])^  <> PInteger(Param^.Args[1])^); // Was Char
end;

procedure Func_II_LT(Param: PExpressionRec);
begin
  Param^.Res.MemoryPos^^ := AnsiChar(PInteger(Param^.Args[0])^  <  PInteger(Param^.Args[1])^); // Was Char
end;

procedure Func_II_GT(Param: PExpressionRec);
begin
  Param^.Res.MemoryPos^^ := AnsiChar(PInteger(Param^.Args[0])^  >  PInteger(Param^.Args[1])^); // Was Char
end;

procedure Func_II_LTE(Param: PExpressionRec);
begin
  Param^.Res.MemoryPos^^ := AnsiChar(PInteger(Param^.Args[0])^  <= PInteger(Param^.Args[1])^); // Was Char
end;

procedure Func_II_GTE(Param: PExpressionRec);
begin
  Param^.Res.MemoryPos^^ := AnsiChar(PInteger(Param^.Args[0])^  >= PInteger(Param^.Args[1])^); // Was Char
end;

procedure Func_IF_EQ(Param: PExpressionRec);
begin
  Param^.Res.MemoryPos^^ := AnsiChar(PInteger(Param^.Args[0])^  =  PDouble(Param^.Args[1])^); // Was Char
end;

procedure Func_IF_NEQ(Param: PExpressionRec);
begin
  Param^.Res.MemoryPos^^ := AnsiChar(PInteger(Param^.Args[0])^  <> PDouble(Param^.Args[1])^); // Was Char
end;

procedure Func_IF_LT(Param: PExpressionRec);
begin
  Param^.Res.MemoryPos^^ := AnsiChar(PInteger(Param^.Args[0])^  <  PDouble(Param^.Args[1])^); // Was Char
end;

procedure Func_IF_GT(Param: PExpressionRec);
begin
  Param^.Res.MemoryPos^^ := AnsiChar(PInteger(Param^.Args[0])^  >  PDouble(Param^.Args[1])^); // Was Char
end;

procedure Func_IF_LTE(Param: PExpressionRec);
begin
  Param^.Res.MemoryPos^^ := AnsiChar(PInteger(Param^.Args[0])^  <= PDouble(Param^.Args[1])^); // Was Char
end;

procedure Func_IF_GTE(Param: PExpressionRec);
begin
  Param^.Res.MemoryPos^^ := AnsiChar(PInteger(Param^.Args[0])^  >= PDouble(Param^.Args[1])^); // Was Char
end;

{$ifdef SUPPORT_INT64}

procedure Func_LL_EQ(Param: PExpressionRec);
begin
  Param^.Res.MemoryPos^^ := AnsiChar(PInt64(Param^.Args[0])^    =  PInt64(Param^.Args[1])^); // Was Char
end;

procedure Func_LL_NEQ(Param: PExpressionRec);
begin
  Param^.Res.MemoryPos^^ := AnsiChar(PInt64(Param^.Args[0])^    <> PInt64(Param^.Args[1])^); // Was Char
end;

procedure Func_LL_LT(Param: PExpressionRec);
begin
  Param^.Res.MemoryPos^^ := AnsiChar(PInt64(Param^.Args[0])^    <  PInt64(Param^.Args[1])^); // Was Char
end;

procedure Func_LL_GT(Param: PExpressionRec);
begin
  Param^.Res.MemoryPos^^ := AnsiChar(PInt64(Param^.Args[0])^    >  PInt64(Param^.Args[1])^); // Was Char
end;

procedure Func_LL_LTE(Param: PExpressionRec);
begin
  Param^.Res.MemoryPos^^ := AnsiChar(PInt64(Param^.Args[0])^    <= PInt64(Param^.Args[1])^); // Was Char
end;

procedure Func_LL_GTE(Param: PExpressionRec);
begin
  Param^.Res.MemoryPos^^ := AnsiChar(PInt64(Param^.Args[0])^    >= PInt64(Param^.Args[1])^); // Was Char
end;

procedure Func_LF_EQ(Param: PExpressionRec);
begin
  Param^.Res.MemoryPos^^ := AnsiChar(PInt64(Param^.Args[0])^    =  PDouble(Param^.Args[1])^); // Was Char
end;

procedure Func_LF_NEQ(Param: PExpressionRec);
begin
  Param^.Res.MemoryPos^^ := AnsiChar(PInt64(Param^.Args[0])^    <> PDouble(Param^.Args[1])^); // Was Char
end;

procedure Func_LF_LT(Param: PExpressionRec);
begin
  Param^.Res.MemoryPos^^ := AnsiChar(PInt64(Param^.Args[0])^    <  PDouble(Param^.Args[1])^); // Was Char
end;

procedure Func_LF_GT(Param: PExpressionRec);
begin
  Param^.Res.MemoryPos^^ := AnsiChar(PInt64(Param^.Args[0])^    >  PDouble(Param^.Args[1])^); // Was Char
end;

procedure Func_LF_LTE(Param: PExpressionRec);
begin
  Param^.Res.MemoryPos^^ := AnsiChar(PInt64(Param^.Args[0])^    <= PDouble(Param^.Args[1])^); // Was Char
end;

procedure Func_LF_GTE(Param: PExpressionRec);
begin
  Param^.Res.MemoryPos^^ := AnsiChar(PInt64(Param^.Args[0])^    >= PDouble(Param^.Args[1])^); // Was Char
end;

procedure Func_FL_EQ(Param: PExpressionRec);
begin
  Param^.Res.MemoryPos^^ := AnsiChar(PDouble(Param^.Args[0])^   =  PInt64(Param^.Args[1])^); // Was Char
end;

procedure Func_FL_NEQ(Param: PExpressionRec);
begin
  Param^.Res.MemoryPos^^ := AnsiChar(PDouble(Param^.Args[0])^   <> PInt64(Param^.Args[1])^); // Was Char
end;

procedure Func_FL_LT(Param: PExpressionRec);
begin
  Param^.Res.MemoryPos^^ := AnsiChar(PDouble(Param^.Args[0])^   <  PInt64(Param^.Args[1])^); // Was Char
end;

procedure Func_FL_GT(Param: PExpressionRec);
begin
  Param^.Res.MemoryPos^^ := AnsiChar(PDouble(Param^.Args[0])^   >  PInt64(Param^.Args[1])^); // Was Char
end;

procedure Func_FL_LTE(Param: PExpressionRec);
begin
  Param^.Res.MemoryPos^^ := AnsiChar(PDouble(Param^.Args[0])^   <= PInt64(Param^.Args[1])^); // Was Char
end;

procedure Func_FL_GTE(Param: PExpressionRec);
begin
  Param^.Res.MemoryPos^^ := AnsiChar(PDouble(Param^.Args[0])^   >= PInt64(Param^.Args[1])^); // Was Char
end;

procedure Func_LI_EQ(Param: PExpressionRec);
begin
  Param^.Res.MemoryPos^^ := AnsiChar(PInt64(Param^.Args[0])^    =  PInteger(Param^.Args[1])^); // Was Char
end;

procedure Func_LI_NEQ(Param: PExpressionRec);
begin
  Param^.Res.MemoryPos^^ := AnsiChar(PInt64(Param^.Args[0])^    <> PInteger(Param^.Args[1])^); // Was Char
end;

procedure Func_LI_LT(Param: PExpressionRec);
begin
  Param^.Res.MemoryPos^^ := AnsiChar(PInt64(Param^.Args[0])^    <  PInteger(Param^.Args[1])^); // Was Char
end;

procedure Func_LI_GT(Param: PExpressionRec);
begin
  Param^.Res.MemoryPos^^ := AnsiChar(PInt64(Param^.Args[0])^    >  PInteger(Param^.Args[1])^); // Was Char
end;

procedure Func_LI_LTE(Param: PExpressionRec);
begin
  Param^.Res.MemoryPos^^ := AnsiChar(PInt64(Param^.Args[0])^    <= PInteger(Param^.Args[1])^); // Was Char
end;

procedure Func_LI_GTE(Param: PExpressionRec);
begin
  Param^.Res.MemoryPos^^ := AnsiChar(PInt64(Param^.Args[0])^    >= PInteger(Param^.Args[1])^); // Was Char
end;

procedure Func_IL_EQ(Param: PExpressionRec);
begin
  Param^.Res.MemoryPos^^ := AnsiChar(PInteger(Param^.Args[0])^  =  PInt64(Param^.Args[1])^); // Was Char
end;

procedure Func_IL_NEQ(Param: PExpressionRec);
begin
  Param^.Res.MemoryPos^^ := AnsiChar(PInteger(Param^.Args[0])^  <> PInt64(Param^.Args[1])^); // Was Char
end;

procedure Func_IL_LT(Param: PExpressionRec);
begin
  Param^.Res.MemoryPos^^ := AnsiChar(PInteger(Param^.Args[0])^  <  PInt64(Param^.Args[1])^); // Was Char
end;

procedure Func_IL_GT(Param: PExpressionRec);
begin
  Param^.Res.MemoryPos^^ := AnsiChar(PInteger(Param^.Args[0])^  >  PInt64(Param^.Args[1])^); // Was Char
end;

procedure Func_IL_LTE(Param: PExpressionRec);
begin
  Param^.Res.MemoryPos^^ := AnsiChar(PInteger(Param^.Args[0])^  <= PInt64(Param^.Args[1])^); // Was Char
end;

procedure Func_IL_GTE(Param: PExpressionRec);
begin
  Param^.Res.MemoryPos^^ := AnsiChar(PInteger(Param^.Args[0])^  >= PInt64(Param^.Args[1])^); // Was Char
end;

{$endif}

procedure Func_AND(Param: PExpressionRec);
begin
  Param^.Res.MemoryPos^^ := AnsiChar(Boolean(Param^.Args[0]^) and Boolean(Param^.Args[1]^)); // Was Char
end;

procedure Func_OR(Param: PExpressionRec);
begin
  Param^.Res.MemoryPos^^ := AnsiChar(Boolean(Param^.Args[0]^) or Boolean(Param^.Args[1]^)); // Was Char
end;

procedure Func_NOT(Param: PExpressionRec);
begin
  Param^.Res.MemoryPos^^ := AnsiChar(not Boolean(Param^.Args[0]^)); // Was Char
end;

procedure FuncAbs_I_I(Param: PExpressionRec);
begin
  PInteger(Param^.Res.MemoryPos^)^ := Abs(PInteger(Param^.Args[0])^);
end;

procedure FuncAbs_F_F(Param: PExpressionRec);
begin
  PDouble(Param^.Res.MemoryPos^)^ := Abs(PDouble(Param^.Args[0])^);
end;

{$ifdef SUPPORT_INT64}
procedure FuncAbs_F_L(Param: PExpressionRec);
begin
  PDouble(Param^.Res.MemoryPos^)^ := Abs(PLargeInt(Param^.Args[0])^);
end;
{$endif}

procedure FuncConcatenate_S(Param: PExpressionRec; Pad: Boolean);
var
  ArgIndex: Integer;
  FloatValue: Extended;
  StringValue: AnsiString;
  Buffer: array[0..19] of AnsiChar;
  Len: Integer;
  ResSource: PAnsiChar;
  ResLength: Integer;
  Arg: PAnsiChar;
  ArgType: TExpressionType;
  ArgIsNull: Boolean;
  Precision: Integer;
  Variable: TVariable;
  FieldInfo: PVariableFieldInfo;
begin
  ArgIndex:= 0;
  while (ArgIndex >= 0) and (ArgIndex < MaxArg) do
  begin
    if Assigned(Param^.ArgList[ArgIndex]) then
    begin
      ResSource := nil;
      ResLength := 0;
      Len := 0;
      Arg := Param^.Args[ArgIndex];
      ArgType := Param^.ArgsType[ArgIndex];
      ArgIsNull := Param^.ArgList[ArgIndex]^.IsNullPtr^;
      if (not ArgIsNull) or Pad then
      begin
        case ArgType of
          etString:
          begin
            ResSource := Arg;
            ResLength := ExprStrLen(Arg, Pad);
          end;
          etFloat:
          begin
            ResSource := @Buffer[0];
            ResLength := 20;
            Precision := 4;
            FloatValue := PDouble(Arg)^;
            if Param^.ArgList[ArgIndex]^.ExprWord is TVariable then
            begin
              Variable := TVariable(Param^.ArgList[ArgIndex]^.ExprWord);
              FieldInfo := Variable.FieldInfo;
              if Assigned(FieldInfo) then
              begin
                case FieldInfo.NativeFieldType of
                  'F', 'N':
                  begin
                    if ((FieldInfo.Size > 0) and (FieldInfo.Size <= ResLength)) and (FieldInfo.Precision >= 0) then
                    begin
                      ResLength := FieldInfo.Size;
                      Precision := FieldInfo.Precision;
                    end;
                  end;
                end;
              end;
            end;
            if not ArgIsNull then
              Len := FloatToStrWidth(FloatValue, ResLength, Precision, ResSource, Pad);
            if not Pad then
              ResLength := Len;
          end;
          etInteger,
          etLargeInt:
          begin
            ResSource := @Buffer[0];
            ResLength := 11;
            if not ArgIsNull then
              Len:= IntToStrWidth(PInteger(Arg)^, ResLength, ResSource, Pad, ' ');
            if not Pad then
              ResLength := Len;
          end;
          etDateTime:
          begin
            ResLength := 8;
            if ArgIsNull then
              ResSource := @Buffer[0]
            else
            begin
              StringValue := AnsiString(FormatDateTime('YYYYMMDD', PDateTime(Arg)^));
              Len := ResLength;
              ResSource := PAnsiChar(StringValue);
            end;
          end;
        end;
      end;
      if Assigned(ResSource) then
      begin
        if (ArgType <> etString) and Pad then
          FillChar(ResSource^, ResLength - Len, ' ');
        if ResLength <> 0 then
          Param^.Res.Append(ResSource, ResLength);
      end;
      Inc(ArgIndex);
    end
    else
      ArgIndex := -1;
  end;
end;

procedure FuncAdd_D_DF(Param: PExpressionRec);
begin
  PDateTime(Param^.Res.MemoryPos^)^ := PDateTime(Param^.Args[0])^ + PDouble(Param^.Args[1])^;
  FuncAddSub_CheckNull(Param);
end;

procedure FuncAdd_D_DI(Param: PExpressionRec);
begin
  PDateTime(Param^.Res.MemoryPos^)^ := PDateTime(Param^.Args[0])^ + PInteger(Param^.Args[1])^;
  FuncAddSub_CheckNull(Param);
end;

procedure FuncAdd_D_DL(Param: PExpressionRec);
begin
  PDateTime(Param^.Res.MemoryPos^)^ := PDateTime(Param^.Args[0])^ + PInt64(Param^.Args[1])^;
  FuncAddSub_CheckNull(Param);
end;

procedure FuncAdd_D_FD(Param: PExpressionRec);
begin
  PDateTime(Param^.Res.MemoryPos^)^ := PDouble(Param^.Args[0])^ + PDateTime(Param^.Args[1])^;
  FuncAddSub_CheckNull(Param);
end;

procedure FuncAdd_D_ID(Param: PExpressionRec);
begin
  PDateTime(Param^.Res.MemoryPos^)^ := PInteger(Param^.Args[0])^ + PDateTime(Param^.Args[1])^;
  FuncAddSub_CheckNull(Param);
end;

procedure FuncAdd_D_LD(Param: PExpressionRec);
begin
  PDateTime(Param^.Res.MemoryPos^)^ := PInt64(Param^.Args[0])^ + PDateTime(Param^.Args[1])^;
  FuncAddSub_CheckNull(Param);
end;

procedure FuncAdd_S(Param: PExpressionRec);
begin
  FuncConcatenate_S(Param, True);
end;

procedure FuncAsc(Param: PExpressionRec);
begin
  if ExprStrLen(Param^.Args[0], False) > 0 then
    PInteger(Param^.Res.MemoryPos^)^ := Ord(Param^.Args[0]^);
end;

procedure FuncCDOW(Param: PExpressionRec);
var
  ADate: TDateTime;
  ADayOfWeek: Word;
  TempStr: AnsiString;
begin
  ADate := PDateTimeRec(Param^.Args[0])^.DateTime;
  if ADate <> 0 then
  begin
    ADayOfWeek := DayOfWeek(ADate);
    {$ifdef DELPHI_XE}
    TempStr := AnsiString(FormatSettings.ShortDayNames[ADayOfWeek]);
    {$else}
    TempStr := AnsiString(ShortDayNames[ADayOfWeek]);
    {$endif}
  end
  else
    TempStr := '   ';
  Param^.Res.Append(PAnsiChar(TempStr), Length(TempStr));
end;

procedure FuncCeil_I_F(Param: PExpressionRec);
begin
  PInteger(Param^.Res.MemoryPos^)^ := Ceil(PDouble(Param^.Args[0])^);
end;

procedure FuncCeil_F_F(Param: PExpressionRec);
begin
  PDouble(Param^.Res.MemoryPos^)^ := Ceil(PDouble(Param^.Args[0])^);
end;

procedure FuncChr(Param: PExpressionRec);
var
  IntValue: Integer;
begin
  if Param^.ExpressionContext^.ValidatingIndex then
    IntValue:= Ord(' ')
  else
    IntValue := PInteger(Param^.Args[0])^;
  if (IntValue >= Low(Byte)) and (IntValue <= High(Byte)) then
    Param^.Res.Append(@IntValue, SizeOf(Byte));
end;

procedure FuncDate(Param: PExpressionRec);
begin
  PDateTime(Param^.Res.MemoryPos^)^ := Now;
end;

procedure FuncDay(Param: PExpressionRec);
var
  ADate: TDateTime;
  Year, Month, Day: Word;
begin
  ADate := PDateTimeRec(Param^.Args[0])^.DateTime;
  if ADate <> 0 then
  begin
    DecodeDate(ADate, Year, Month, Day);
    PInteger(Param^.Res.MemoryPos^)^ := Day;
  end
  else
    PInteger(Param^.Res.MemoryPos^)^ := 0;
end;

procedure FuncEmpty(Param: PExpressionRec);
begin
  case Param^.ArgsType[0] of
    etDateTime: PBoolean(Param^.Res.MemoryPos^)^ := PDateTime(Param^.Args[0])^ = 0;
    etFloat: PBoolean(Param^.Res.MemoryPos^)^ := PDouble(Param^.Args[0])^ = 0;
    etInteger: PBoolean(Param^.Res.MemoryPos^)^ := PInteger(Param^.Args[0])^ = 0;
    etLargeInt: PBoolean(Param^.Res.MemoryPos^)^ := PLargeInt(Param^.Args[0])^ = 0;
    etString: PBoolean(Param^.Res.MemoryPos^)^ := ExprStrLen(Param^.Args[0], False) = 0;
  end;
end;

procedure FuncIIF_S_SS(Param: PExpressionRec);
begin
  if PBoolean(Param^.Args[0])^ then
    Param^.Res.Append(Param^.Args[1], dbfStrLen(Param^.Args[1]))
  else
    Param^.Res.Append(Param^.Args[2], dbfStrLen(Param^.Args[2]));
end;

procedure FuncIIF_F_FF(Param: PExpressionRec);
begin
  if PBoolean(Param^.Args[0])^ then
    PDouble(Param^.Res.MemoryPos^)^ := PDouble(Param^.Args[1])^
  else
    PDouble(Param^.Res.MemoryPos^)^ := PDouble(Param^.Args[2])^;
end;

procedure FuncIIF_I_II(Param: PExpressionRec);
begin
  if PBoolean(Param^.Args[0])^ then
    PInteger(Param^.Res.MemoryPos^)^ := PInteger(Param^.Args[1])^
  else
    PInteger(Param^.Res.MemoryPos^)^ := PInteger(Param^.Args[2])^;
end;

procedure FuncLen_F_S(Param: PExpressionRec);
begin
  PDouble(Param^.Res.MemoryPos^)^ := dbfStrLen(Param^.Args[0]);
end;

{$ifdef SUPPORT_INT64}
procedure FuncLen_L_S(Param: PExpressionRec);
begin
  PLargeInt(Param^.Res.MemoryPos^)^ := dbfStrLen(Param^.Args[0]);
end;
{$endif}

procedure FuncLen_I_S(Param: PExpressionRec);
begin
  PInteger(Param^.Res.MemoryPos^)^ := dbfStrLen(Param^.Args[0]);
end;

procedure FuncLTrim(Param: PExpressionRec);
var
  TempStr: AnsiString;
begin
  if Param^.ExpressionContext^.ValidatingIndex then
    Param^.Res.Append(Param^.Args[0], dbfStrLen(Param^.Args[0]))
  else
  begin
    TempStr := dbfTrimLeft(Param^.Args[0]);
    Param^.Res.Append(PAnsiChar(TempStr), Length(TempStr));
  end;
end;

procedure FuncMonth(Param: PExpressionRec);
var
  ADate: TDateTime;
  Year, Month, Day: Word;
begin
  ADate := PDateTimeRec(Param^.Args[0])^.DateTime;
  if ADate <> 0 then
  begin
    DecodeDate(ADate, Year, Month, Day);
    PInteger(Param^.Res.MemoryPos^)^ := Month;
  end
  else
    PInteger(Param^.Res.MemoryPos^)^ := 0;
end;

procedure FuncProper(Param: PExpressionRec);
var
  P: PAnsiChar;
  Len: Integer;
  Index: Integer;
  NewWord: Boolean;
  Buffer: array[0..1] of AnsiChar;
begin
  P := Param^.Args[0];
  Len := dbfStrLen(P);
  NewWord := True;
  Buffer[1]:= #0;
  for Index:= 1 to Len do
  begin
    if P^ = ' ' then
      NewWord := True
    else
    begin
      Buffer[0] := P^;
      if NewWord then
      begin
        P^ := dbfStrUpper(Buffer)^;
        NewWord := False;
      end
      else
        P^ := dbfStrLower(Buffer)^;
    end;
    Inc(P);
  end;
  Param^.Res.Append(Param^.Args[0], Len);
end;

procedure FuncRight(Param: PExpressionRec);
var
  srcLen, index, count: Integer;
begin
  srcLen := dbfStrLen(Param^.Args[0]);
  count  := PInteger(Param^.Args[1])^;
  if count > srcLen then
    count := srcLen;
  if count > 0 then
  begin
    index := srcLen - count;
    Param^.Res.Append(Param^.Args[0]+index, count);
  end;
end;

procedure FuncRound_F_FF(Param: PExpressionRec);
{$IFDEF SUPPORT_ROUNDTO}
var
  TempInt: Integer;
{$ENDIF}
begin
{$IFDEF SUPPORT_ROUNDTO}
  TempInt := Trunc(PDouble(Param^.Args[1])^);
  PDouble(Param^.Res.MemoryPos^)^ := RoundTo(PDouble(Param^.Args[0])^, -TempInt);
{$ELSE}
  PDouble(Param^.Res.MemoryPos^)^ := Round(PDouble(Param^.Args[0])^); // 2nd arg to be incorporated
{$ENDIF}
end;

procedure FuncRound_F_FI(Param: PExpressionRec);
{$IFDEF SUPPORT_ROUNDTO}
var
  TempInt: integer;
{$ENDIF}
begin
{$IFDEF SUPPORT_ROUNDTO}
  TempInt := PInteger(Param^.Args[1])^;
  PDouble(Param^.Res.MemoryPos^)^ := RoundTo(PDouble(Param^.Args[0])^, -TempInt);
{$ELSE}
  PDouble(Param^.Res.MemoryPos^)^ := Round(PDouble(Param^.Args[0])^); // 2nd arg to be incorporated
{$ENDIF}
end;

procedure FuncRTrim(Param: PExpressionRec);
var
  TempStr: AnsiString;
begin
  if Param^.ExpressionContext^.ValidatingIndex then
    Param^.Res.Append(Param^.Args[0], dbfStrLen(Param^.Args[0]))
  else
  begin
    TempStr := dbfTrimRight(Param^.Args[0]);
    Param^.Res.Append(PAnsiChar(TempStr), Length(TempStr));
  end;
end;

{$I dbf_soundex.inc}
procedure FuncSoundex(Param: PExpressionRec);
var
  Src: PAnsiChar;
  Dest: AnsiString;
begin
  with Param^ do
  begin
    Src := Param^.Args[0];
    Dest := Soundex(src);
    Param^.Res.Append(PAnsiChar(Dest), Length(Dest));
  end;
end;

procedure FuncSub_S(Param: PExpressionRec);
begin
  FuncConcatenate_S(Param, Param^.ExpressionContext^.ValidatingIndex);
end;

procedure FuncVal(Param: PExpressionRec);
var
  Index: Integer;
  TempStr: AnsiString;
  AExtended: Extended;
begin
  TempStr := dbfTrimLeft(Param^.Args[0]);
  Index := 0;
  while (Index<Length(TempStr)) and (TempStr[Succ(Index)] in [DBF_ZERO..DBF_NINE, DBF_POSITIVESIGN, DBF_NEGATIVESIGN, DBF_DECIMAL, DBF_DECIMAL_ALTERNATE]) do
    Inc(Index);
  SetLength(TempStr, Index);
  case Param^.ExprWord.ResultType of
    etFloat:
    begin
      StrToFloatWidth(AExtended, PAnsiChar(TempStr), Succ(Length(TempStr)), 0);
      PDouble(Param^.Res.MemoryPos^)^ := AExtended;
    end;
    etInteger: StrToInt32Width(PInteger(Param^.Res.MemoryPos^)^, PAnsiChar(TempStr), Succ(Length(TempStr)), 0);
{$ifdef SUPPORT_INT64}
    etLargeInt: StrToIntWidth(PLargeInt(Param^.Res.MemoryPos^)^, PAnsiChar(TempStr), Succ(Length(TempStr)), 0);
{$endif}
  end;
end;

procedure FuncYear(Param: PExpressionRec);
var
  ADate: TDateTime;
  Year, Month, Day: Word;
begin
  ADate := PDateTimeRec(Param^.Args[0])^.DateTime;
  if ADate <> 0 then
  begin
    DecodeDate(ADate, Year, Month, Day);
    PInteger(Param^.Res.MemoryPos^)^ := Year;
  end
  else
    PInteger(Param^.Res.MemoryPos^)^ := 0;
end;

initialization

  DbfWordsGeneralList := TExpressList.Create;
  DbfWordsInsensGeneralList := TExpressList.Create;
  DbfWordsInsensNoPartialList := TExpressList.Create;
  DbfWordsInsensPartialList := TExpressList.Create;
  DbfWordsSensGeneralList := TExpressList.Create;
  DbfWordsSensNoPartialList := TExpressList.Create;
  DbfWordsSensPartialList := TExpressList.Create;

  with DbfWordsGeneralList do
  begin
    // basic function functionality
    Add(TLeftBracket.Create('(', nil));
    Add(TRightBracket.Create(')', nil));
    Add(TComma.Create(',', nil));

    // operators - name, param types, result type, func addr, precedence
    Add(TFunction.CreateOper('-@', 'I', etInteger, FuncNegative_I_I, 20));
    Add(TFunction.CreateOper('-@', 'F', etFloat, FuncNegative_F_F, 20));
{$ifdef SUPPORT_INT64}
    Add(TFunction.CreateOper('-@', 'L', etLargeInt, FuncNegative_L_L, 20));
{$endif}
    Add(TFunction.CreateOper('+', 'SS', etString,   nil,          40));
    Add(TFunction.CreateOper('+', 'FF', etFloat,    FuncAdd_F_FF, 40));
    Add(TFunction.CreateOper('+', 'FI', etFloat,    FuncAdd_F_FI, 40));
    Add(TFunction.CreateOper('+', 'IF', etFloat,    FuncAdd_F_IF, 40));
    Add(TFunction.CreateOper('+', 'II', etInteger,  FuncAdd_F_II, 40));
{$ifdef SUPPORT_INT64}
    Add(TFunction.CreateOper('+', 'FL', etFloat,    FuncAdd_F_FL, 40));
    Add(TFunction.CreateOper('+', 'IL', etLargeInt, FuncAdd_F_IL, 40));
    Add(TFunction.CreateOper('+', 'LF', etFloat,    FuncAdd_F_LF, 40));
    Add(TFunction.CreateOper('+', 'LL', etLargeInt, FuncAdd_F_LI, 40));
    Add(TFunction.CreateOper('+', 'LI', etLargeInt, FuncAdd_F_LL, 40));
{$endif}
    Add(TFunction.CreateOper('+', 'DF', etDateTime, FuncAdd_D_DF, 40));
    Add(TFunction.CreateOper('+', 'DI', etDateTime, FuncAdd_D_DI, 40));
    Add(TFunction.CreateOper('+', 'DL', etDateTime, FuncAdd_D_DL, 40));
    Add(TFunction.CreateOper('+', 'FD', etDateTime, FuncAdd_D_FD, 40));
    Add(TFunction.CreateOper('+', 'ID', etDateTime, FuncAdd_D_ID, 40));
    Add(TFunction.CreateOper('+', 'LD', etDateTime, FuncAdd_D_LD, 40));
    Add(TFunction.CreateOper('+', 'DS', etString,   FuncAdd_S,    40));
    Add(TFunction.CreateOper('+', 'FS', etString,   FuncAdd_S,    40));
    Add(TFunction.CreateOper('+', 'IS', etString,   FuncAdd_S,    40));
    Add(TFunction.CreateOper('+', 'LS', etString,   FuncAdd_S,    40));
    Add(TFunction.CreateOper('+', 'SD', etString,   FuncAdd_S,    40));
    Add(TFunction.CreateOper('+', 'SF', etString,   FuncAdd_S,    40));
    Add(TFunction.CreateOper('+', 'SI', etString,   FuncAdd_S,    40));
    Add(TFunction.CreateOper('+', 'SL', etString,   FuncAdd_S,    40));
    Add(TFunction.CreateOper('-', 'FF', etFloat,    FuncSub_F_FF, 40));
    Add(TFunction.CreateOper('-', 'FI', etFloat,    FuncSub_F_FI, 40));
    Add(TFunction.CreateOper('-', 'IF', etFloat,    FuncSub_F_IF, 40));
    Add(TFunction.CreateOper('-', 'II', etInteger,  FuncSub_F_II, 40));
    Add(TFunction.CreateOper('-', 'DD', etFloat,    FuncSub_F_DD, 40));
{$ifdef SUPPORT_INT64}
    Add(TFunction.CreateOper('-', 'DL', etDateTime, FuncSub_D_DL, 40));
    Add(TFunction.CreateOper('-', 'FL', etFloat,    FuncSub_F_FL, 40));
    Add(TFunction.CreateOper('-', 'IL', etLargeInt, FuncSub_F_IL, 40));
    Add(TFunction.CreateOper('-', 'LF', etFloat,    FuncSub_F_LF, 40));
    Add(TFunction.CreateOper('-', 'LL', etLargeInt, FuncSub_F_LI, 40));
    Add(TFunction.CreateOper('-', 'LI', etLargeInt, FuncSub_F_LL, 40));
{$endif}
    Add(TFunction.CreateOper('-', 'DS', etString,   FuncSub_S,    40));
    Add(TFunction.CreateOper('-', 'FS', etString,   FuncSub_S,    40));
    Add(TFunction.CreateOper('-', 'IS', etString,   FuncSub_S,    40));
    Add(TFunction.CreateOper('-', 'LS', etString,   FuncSub_S,    40));
    Add(TFunction.CreateOper('-', 'SD', etString,   FuncSub_S,    40));
    Add(TFunction.CreateOper('-', 'SF', etString,   FuncSub_S,    40));
    Add(TFunction.CreateOper('-', 'SI', etString,   FuncSub_S,    40));
    Add(TFunction.CreateOper('-', 'SL', etString,   FuncSub_S,    40));
    Add(TFunction.CreateOper('-', 'SS', etString,   FuncSub_S,    40));
    Add(TFunction.CreateOper('*', 'FF', etFloat,    FuncMul_F_FF, 40));
    Add(TFunction.CreateOper('*', 'FI', etFloat,    FuncMul_F_FI, 40));
    Add(TFunction.CreateOper('*', 'IF', etFloat,    FuncMul_F_IF, 40));
    Add(TFunction.CreateOper('*', 'II', etInteger,  FuncMul_F_II, 40));
{$ifdef SUPPORT_INT64}
    Add(TFunction.CreateOper('*', 'FL', etFloat,    FuncMul_F_FL, 40));
    Add(TFunction.CreateOper('*', 'IL', etLargeInt, FuncMul_F_IL, 40));
    Add(TFunction.CreateOper('*', 'LF', etFloat,    FuncMul_F_LF, 40));
    Add(TFunction.CreateOper('*', 'LL', etLargeInt, FuncMul_F_LI, 40));
    Add(TFunction.CreateOper('*', 'LI', etLargeInt, FuncMul_F_LL, 40));
{$endif}
    Add(TFunction.CreateOper('/', 'FF', etFloat,    FuncDiv_F_FF, 40));
    Add(TFunction.CreateOper('/', 'FI', etFloat,    FuncDiv_F_FI, 40));
    Add(TFunction.CreateOper('/', 'IF', etFloat,    FuncDiv_F_IF, 40));
    Add(TFunction.CreateOper('/', 'II', etInteger,  FuncDiv_F_II, 40));
{$ifdef SUPPORT_INT64}
    Add(TFunction.CreateOper('/', 'FL', etFloat,    FuncDiv_F_FL, 40));
    Add(TFunction.CreateOper('/', 'IL', etLargeInt, FuncDiv_F_IL, 40));
    Add(TFunction.CreateOper('/', 'LF', etFloat,    FuncDiv_F_LF, 40));
    Add(TFunction.CreateOper('/', 'LL', etLargeInt, FuncDiv_F_LI, 40));
    Add(TFunction.CreateOper('/', 'LI', etLargeInt, FuncDiv_F_LL, 40));
{$endif}

    Add(TFunction.CreateOper('=', 'FF', etBoolean, Func_FF_EQ , 80));
    Add(TFunction.CreateOper('<', 'FF', etBoolean, Func_FF_LT , 80));
    Add(TFunction.CreateOper('>', 'FF', etBoolean, Func_FF_GT , 80));
    Add(TFunction.CreateOper('<=','FF', etBoolean, Func_FF_LTE, 80));
    Add(TFunction.CreateOper('>=','FF', etBoolean, Func_FF_GTE, 80));
    Add(TFunction.CreateOper('<>','FF', etBoolean, Func_FF_NEQ, 80));
    Add(TFunction.CreateOper('=', 'FI', etBoolean, Func_FI_EQ , 80));
    Add(TFunction.CreateOper('<', 'FI', etBoolean, Func_FI_LT , 80));
    Add(TFunction.CreateOper('>', 'FI', etBoolean, Func_FI_GT , 80));
    Add(TFunction.CreateOper('<=','FI', etBoolean, Func_FI_LTE, 80));
    Add(TFunction.CreateOper('>=','FI', etBoolean, Func_FI_GTE, 80));
    Add(TFunction.CreateOper('<>','FI', etBoolean, Func_FI_NEQ, 80));
    Add(TFunction.CreateOper('=', 'II', etBoolean, Func_II_EQ , 80));
    Add(TFunction.CreateOper('<', 'II', etBoolean, Func_II_LT , 80));
    Add(TFunction.CreateOper('>', 'II', etBoolean, Func_II_GT , 80));
    Add(TFunction.CreateOper('<=','II', etBoolean, Func_II_LTE, 80));
    Add(TFunction.CreateOper('>=','II', etBoolean, Func_II_GTE, 80));
    Add(TFunction.CreateOper('<>','II', etBoolean, Func_II_NEQ, 80));
    Add(TFunction.CreateOper('=', 'IF', etBoolean, Func_IF_EQ , 80));
    Add(TFunction.CreateOper('<', 'IF', etBoolean, Func_IF_LT , 80));
    Add(TFunction.CreateOper('>', 'IF', etBoolean, Func_IF_GT , 80));
    Add(TFunction.CreateOper('<=','IF', etBoolean, Func_IF_LTE, 80));
    Add(TFunction.CreateOper('>=','IF', etBoolean, Func_IF_GTE, 80));
    Add(TFunction.CreateOper('<>','IF', etBoolean, Func_IF_NEQ, 80));
{$ifdef SUPPORT_INT64}
    Add(TFunction.CreateOper('=', 'LL', etBoolean, Func_LL_EQ , 80));
    Add(TFunction.CreateOper('<', 'LL', etBoolean, Func_LL_LT , 80));
    Add(TFunction.CreateOper('>', 'LL', etBoolean, Func_LL_GT , 80));
    Add(TFunction.CreateOper('<=','LL', etBoolean, Func_LL_LTE, 80));
    Add(TFunction.CreateOper('>=','LL', etBoolean, Func_LL_GTE, 80));
    Add(TFunction.CreateOper('<>','LL', etBoolean, Func_LL_NEQ, 80));
    Add(TFunction.CreateOper('=', 'LF', etBoolean, Func_LF_EQ , 80));
    Add(TFunction.CreateOper('<', 'LF', etBoolean, Func_LF_LT , 80));
    Add(TFunction.CreateOper('>', 'LF', etBoolean, Func_LF_GT , 80));
    Add(TFunction.CreateOper('<=','LF', etBoolean, Func_LF_LTE, 80));
    Add(TFunction.CreateOper('>=','LF', etBoolean, Func_LF_GTE, 80));
    Add(TFunction.CreateOper('<>','FI', etBoolean, Func_LF_NEQ, 80));
    Add(TFunction.CreateOper('=', 'LI', etBoolean, Func_LI_EQ , 80));
    Add(TFunction.CreateOper('<', 'LI', etBoolean, Func_LI_LT , 80));
    Add(TFunction.CreateOper('>', 'LI', etBoolean, Func_LI_GT , 80));
    Add(TFunction.CreateOper('<=','LI', etBoolean, Func_LI_LTE, 80));
    Add(TFunction.CreateOper('>=','LI', etBoolean, Func_LI_GTE, 80));
    Add(TFunction.CreateOper('<>','LI', etBoolean, Func_LI_NEQ, 80));
    Add(TFunction.CreateOper('=', 'FL', etBoolean, Func_FL_EQ , 80));
    Add(TFunction.CreateOper('<', 'FL', etBoolean, Func_FL_LT , 80));
    Add(TFunction.CreateOper('>', 'FL', etBoolean, Func_FL_GT , 80));
    Add(TFunction.CreateOper('<=','FL', etBoolean, Func_FL_LTE, 80));
    Add(TFunction.CreateOper('>=','FL', etBoolean, Func_FL_GTE, 80));
    Add(TFunction.CreateOper('<>','FL', etBoolean, Func_FL_NEQ, 80));
    Add(TFunction.CreateOper('=', 'IL', etBoolean, Func_IL_EQ , 80));
    Add(TFunction.CreateOper('<', 'IL', etBoolean, Func_IL_LT , 80));
    Add(TFunction.CreateOper('>', 'IL', etBoolean, Func_IL_GT , 80));
    Add(TFunction.CreateOper('<=','IL', etBoolean, Func_IL_LTE, 80));
    Add(TFunction.CreateOper('>=','IL', etBoolean, Func_IL_GTE, 80));
    Add(TFunction.CreateOper('<>','IL', etBoolean, Func_IL_NEQ, 80));
{$endif}

    Add(TFunction.CreateOper('NOT', 'B',  etBoolean,  Func_NOT,  85));
    Add(TFunction.CreateOper('AND', 'BB', etBoolean,  Func_AND,  90));
    Add(TFunction.CreateOper('OR',  'BB', etBoolean,  Func_OR,  100));

    // Unary plus
    Add(TFunction.CreateOper('+',   'D',  etDateTime, nil,       40));
    Add(TFunction.CreateOper('+',   'F',  etFloat,    nil,       40));
    Add(TFunction.CreateOper('+',   'I',  etInteger,  nil,       40));
    Add(TFunction.CreateOper('+',   'S',  etString,   nil,       40));
{$ifdef SUPPORT_INT64}
    Add(TFunction.CreateOper('+',   'L',  etLargeInt, nil,       40));
{$endif}

    // Functions - name, description, param types, min params, result type, Func addr
//  Add(TFunction.Create('STR',       '',      'FII', 1, etString,   FuncFloatToStr, ''));
//  Add(TFunction.Create('STR',       '',      'III', 1, etString,   FuncIntToStr, ''));
//  Add(TFunction.Create('STR',       '',      'LII', 1, etString,   FuncInt64ToStr, ''));
    Add(TFunction.Create('STR',       '',      'FIIS',1, etString,   FuncStr,       ''));
    Add(TFunction.Create('STR',       '',      'IIIS',1, etString,   FuncStr,       ''));
    Add(TFunction.Create('STR',       '',      'LIIS',1, etString,   FuncStr,       ''));
    Add(TFunction.Create('DTOS',      '',      'D',   1, etString,   FuncDateToStr, ''));
    Add(TFunction.Create('SUBSTR',    'SUBS',  'SII', 3, etString,   FuncSubString, ''));
    Add(TFunction.Create('SUBSTR',    'SUBS',  'SI',  2, etString,   FuncSubString, ''));
    Add(TFunction.Create('LEFT',      'LEFT',  'SI',  2, etString,   FuncLeftString, ''));
    Add(TFunction.Create('UPPERCASE', 'UPPER', 'S',   1, etString,   FuncUppercase, ''));
    Add(TFunction.Create('LOWERCASE', 'LOWER', 'S',   1, etString,   FuncLowercase, ''));

// More functions
    Add(TFunction.Create('ABS',       '',      'I',   1, etInteger,  FuncAbs_I_I,    ''));
    Add(TFunction.Create('ABS',       '',      'F',   1, etFloat,    FuncAbs_F_F,    ''));
{$ifdef SUPPORT_INT64}
    Add(TFunction.Create('ABS',       '',      'L',   1, etFloat,    FuncAbs_F_L,    ''));
{$endif}
    Add(TFunction.Create('ASC',       '',      'S',   1, etInteger,  FuncAsc,        ''));
    Add(TFunction.Create('CDOW',      '',      'D',   1, etString,   FuncCDOW,       ''));
    Add(TFunction.Create('CEILING',   'CEIL',  'F',   1, etInteger,  FuncCeil_I_F,   ''));
    Add(TFunction.Create('CEILING',   'CEIL',  'F',   1, etFloat,    FuncCeil_F_F,   ''));
    Add(TFunction.Create('CHR',       '',      'I',   1, etString,   FuncChr,        ''));
    Add(TFunction.Create('DATE',      '',      '',    0, etDateTime, FuncDate,       ''));
    Add(TFunction.Create('DAY',       '',      'D',   1, etInteger,  FuncDay,        ''));
    Add(TFunction.Create('EMPTY',     '',      'D',   1, etBoolean,  FuncEmpty,      ''));
    Add(TFunction.Create('EMPTY',     '',      'F',   1, etBoolean,  FuncEmpty,      ''));
    Add(TFunction.Create('EMPTY',     '',      'I',   1, etBoolean,  FuncEmpty,      ''));
{$ifdef SUPPORT_INT64}
    Add(TFunction.Create('EMPTY',     '',      'L',   1, etBoolean,  FuncEmpty,      ''));
{$endif}
    Add(TFunction.Create('EMPTY',     '',      'S',   1, etBoolean,  FuncEmpty,      ''));
    Add(TFunction.Create('IIF',       '',      'BSS', 3, etString,   FuncIIF_S_SS,   ''));
    Add(TFunction.Create('IIF',       '',      'BFF', 3, etFloat,    FuncIIF_F_FF,   ''));
    Add(TFunction.Create('IIF',       '',      'BII', 3, etInteger,  FuncIIF_I_II,   ''));
    Add(TFunction.Create('LEN',       '',      'S',   1, etInteger,  FuncLen_I_S,    ''));
{$ifdef SUPPORT_INT64}
    Add(TFunction.Create('LEN',       '',      'S',   1, etLargeInt, FuncLen_L_S,    ''));
{$endif}
    Add(TFunction.Create('LEN',       '',      'S',   1, etFloat,    FuncLen_F_S,    ''));
    Add(TFunction.Create('LTRIM',     '',      'S',   1, etString,   FuncLTrim,      ''));
    Add(TFunction.Create('MONTH',     '',      'D',   1, etInteger,  FuncMonth,      ''));
    Add(TFunction.Create('PROPER',    '',      'S',   1, etString,   FuncProper,     ''));
    Add(TFunction.Create('RIGHT',     '',      'SI',  2, etString,   FuncRight,      ''));
    Add(TFunction.Create('ROUND',     '',      'FI',  2, etFloat,    FuncRound_F_FI, ''));
    Add(TFunction.Create('ROUND',     '',      'FF',  2, etFloat,    FuncRound_F_FF, ''));
    Add(TFunction.Create('RTRIM',     '',      'S',   1, etString,   FuncRTrim,      ''));
    Add(TFunction.Create('SOUNDEX',   '',      'S',   1, etString,   FuncSoundex,    ''));
    Add(TFunction.Create('TRIM',      '',      'S',   1, etString,   FuncRTrim,      ''));
    Add(TFunction.Create('VAL',       '',      'S',   1, etFloat,    FuncVal,        ''));
    Add(TFunction.Create('VAL',       '',      'S',   1, etInteger,  FuncVal,        ''));
    Add(TFunction.Create('VAL',       '',      'S',   1, etLargeInt, FuncVal,       ''));
    Add(TFunction.Create('YEAR',      '',      'D',   1, etInteger,  FuncYear,       ''));
end;

  with DbfWordsInsensGeneralList do
  begin
    Add(TFunction.CreateOper('<', 'SS', etBoolean, FuncStrI_LT , 80));
    Add(TFunction.CreateOper('>', 'SS', etBoolean, FuncStrI_GT , 80));
    Add(TFunction.CreateOper('<=','SS', etBoolean, FuncStrI_LTE, 80));
    Add(TFunction.CreateOper('>=','SS', etBoolean, FuncStrI_GTE, 80));
    Add(TFunction.CreateOper('<>','SS', etBoolean, FuncStrI_NEQ, 80));
  end;

  with DbfWordsInsensNoPartialList do
    Add(TFunction.CreateOper('=', 'SS', etBoolean, FuncStrI_EQ , 80));

  with DbfWordsInsensPartialList do
    Add(TFunction.CreateOper('=', 'SS', etBoolean, FuncStrIP_EQ, 80));

  with DbfWordsSensGeneralList do
  begin
    Add(TFunction.CreateOper('<', 'SS', etBoolean, FuncStr_LT , 80));
    Add(TFunction.CreateOper('>', 'SS', etBoolean, FuncStr_GT , 80));
    Add(TFunction.CreateOper('<=','SS', etBoolean, FuncStr_LTE, 80));
    Add(TFunction.CreateOper('>=','SS', etBoolean, FuncStr_GTE, 80));
    Add(TFunction.CreateOper('<>','SS', etBoolean, FuncStr_NEQ, 80));
  end;

  with DbfWordsSensNoPartialList do
    Add(TFunction.CreateOper('=', 'SS', etBoolean, FuncStr_EQ , 80));

  with DbfWordsSensPartialList do
    Add(TFunction.CreateOper('=', 'SS', etBoolean, FuncStrP_EQ , 80));

finalization

  DbfWordsGeneralList.Free;
  DbfWordsInsensGeneralList.Free;
  DbfWordsInsensNoPartialList.Free;
  DbfWordsInsensPartialList.Free;
  DbfWordsSensGeneralList.Free;
  DbfWordsSensNoPartialList.Free;
  DbfWordsSensPartialList.Free;
end.

