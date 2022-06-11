//from http://www.devx.com/tips/Tip/23141

unit SQLAddDel;

interface

uses
   DB,
   dbf,
   //DBTables,
   SysUtils, Forms;

type
  TCastType = (ctSmallInt, ctInteger, ctDecimal, ctNumeric, ctFloat,
                 ctChar, ctVarChar, ctDate, ctBoolean, ctBLOB, ctTime,
                 ctTimeStamp, ctMoney, ctAutoInc, ctBytes);


procedure SQLAddField(dbName,                      {Database Name}
                      tblName,                     {Table Name}
                      fldName         : String;    {Field Name to Add}
                      fldType         : TCastType; {Field Type as described above}
                      fldLength,                   {Length of Field}
                      precisOrBlobLen,
                      scaleOrBlobType : Integer);  {Blob definition type 1 = Memo, 2 = Binary,
                                                   3 = Formatted Memo, 4 = OLE Object, 5 = Graphic}

procedure SQLDropField(dbName,                     {Database Name}
                       tblName,                    {Table Name}
                       fldName         : String);  {Field Name to Drop}
implementation

procedure SQLAddField(dbName,                      {Database Name}
                      tblName,                     {Table Name}
                      fldName         : String;    {Field Name to Add}
                      fldType         : TCastType; {Field Type as described above}
                      fldLength,                   {Length of Field}
                      precisOrBlobLen,
                      scaleOrBlobType : Integer);  {Blob definition type 1 = Memo, 2 = Binary,
                                                   3 = Formatted Memo, 4 = OLE Object, 5 = Graphic}
var
  sqlAddFld: TQuery;
  CastType : String;

begin
  case fldType of
    ctSmallInt  : CastType := 'SMALLINT';
    ctInteger   : CastType := 'INTEGER';
    ctDecimal   : CastType := 'DECIMAL(' + IntToStr(precisOrBlobLen) + ',' +
                                           IntToStr(scaleOrBlobType) + ')';
    ctNumeric   : CastType := 'NUMERIC(' + IntToStr(precisOrBlobLen) + ',' +
                                           IntToStr(scaleOrBlobType) + ')';
    ctFloat     : CastType := 'FLOAT('   + IntToStr(precisOrBlobLen) + ',' +
                                           IntToStr(scaleOrBlobType) + ')';
    ctChar      : CastType := 'CHARACTER(' + IntToStr(fldLength) + ')';
    ctVarChar   : CastType := 'VARCHAR(' + IntToStr(fldLength) + ')';
    ctDate      : CastType := 'DATE';
    ctBoolean   : CastType := 'BOOLEAN';
    ctBlob      : CastType := 'BLOB('    + IntToStr(precisOrBlobLen) + ',' +
                                           IntToStr(scaleOrBlobType) + ')';
    ctTime      : CastType := 'TIME';
    ctTimeStamp : CastType := 'TIMESTAMP';
    ctMoney     : CastType := 'MONEY';
    ctAutoInc   : CastType := 'AUTOINC';
    ctBytes     : CastType := 'BYTES(' + IntToStr(fldLength) + ')'
  end;

  sqlAddFld := TQuery.Create(Application);
  with sqlAddFld do begin
    DatabaseName := dbName;
    SQL.Add('ALTER TABLE ' + tblName + ' ADD ' + fldName + ' ' + CastType);
    try
      try
        ExecSQL;
      except
        Abort; {Just raise a silent exception}
      end;
    finally
      Free;
    end;
  end;
end;

procedure SQLDropField(dbName,                     {Database Name}
                       tblName,                    {Table Name}
                       fldName         : String);  {Field Name to Drop}
var
  sqlDrpFld : TQuery;
begin
  sqlDrpFld := TQuery.Create(Application);
  with sqlDrpFld do begin
    DatabaseName := dbName;
    SQL.Add('ALTER TABLE ' + tblName + ' DROP ' + fldName);
    try
      try
        ExecSQL;
      except
        Abort; {Just raise a silent exception}
      end;
    finally
      Free;
    end;
  end;
end;

end.