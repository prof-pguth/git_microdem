{$F+}

unit petmar_db;

{^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^}
{ Part of MICRODEM GIS Program       }
{ PETMAR Trilobite Breeding Ranch    }
{ Released under the MIT Licences    }
{ Copyright (c) 2024 Peter L. Guth   }
{____________________________________}


//The primary database format is dBase DBF, for ESRI shapefiles
//Other formats are tolerated, and have been toyed with for use with other operating systems
//  the other formats may not have been tested recently

{$I nevadia_defines.inc}


//{$Define UseFDMemTable}


{$IfDef RecordProblems}  //normally only defined for debugging specific problems
   //{$Define RecordMakeDir}
   //{$Define RecordFieldPresent}
   //{$Define DBrewrite}
   //{$Define RecordOpenDB}
   //{$Define RecordFullOpenDB}
   //{$Define RecordMyDataCreation}
   //{$Define RecordMyDataFilter}
   //{$Define RecordSQLite}
   //{$Define TrackCDStiming}
   //{$Define TrackInsert}
   //{$Define RecordMonitor}

   {$IfDef Android}
      //{$Define RecordRealToString}
   {$EndIf}
{$EndIf}

{$If Defined(UseTDBF)}
   {$Define BDELikeTables}
{$EndIf}


interface


uses
   System.UITypes, System.UIConsts,System.IOUtils,System.SysUtils,

   {$IfDef VCL}
      Windows, Winspool,ComCtrls,Forms,Printers,Graphics,Buttons,
   {$EndIf}

   {$IfDef FMX}
      FMX.Controls, FMX.Types, FMX.Graphics,
   {$EndIf}

   {$IfDef UseFireDacSQLlite}
      FireDAC.Comp.Client, FireDAC.Comp.Dataset,FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteWrapper,
   {$EndIf}

   {$IfDef UseTDBF}
      dbf,dbf_fields,dbf_dbffile,
   {$EndIf}

   {$IfDef UseTCLientDataSet}
      DBClient,
   {$EndIf}

   {$IfDef MSWindows}
      SHLOBJ,Registry, Messages,ShellAPI,
   {$EndIf}

   Math,Db,Classes,StrUtils,
   Petmar_types,PetMath;

const
   {$IfDef dBase_DefaultDBs}
      DefaultDBExt = '.dbf';
      DefaultDBMask = '*.dbf';
      DBNameMask = 'dBase|*.dbf';
   {$EndIf}

   {$IfDef UseTDBF}
      DBdriver = 'tDBF';
   {$EndIf}

   {$IfDef SQLiteDefaultDBs}
      DefaultDBExt = '.db';
      DefaultDBMask = '*.db';
      DBNameMask = 'SQLite|*.db';
      DBdriver = 'sqlite';
   {$EndIf}

   {$IfDef ClientDataSetDefaultDBs}
      DefaultDBExt = '.cds';
      DefaultDBMask = '*.cds';
      DBNameMask = 'CDS|*.cds';
      DBdriver = 'client data set';
   {$EndIf}

type
   tmydbMode = (dbmyDefault,dbmCDS,dbmForceDefault);
var
   DesiredDBMode : tmydbMode;

type
   {$IfDef UseTDBF}
      tMyTable = dbf.tDBF;
   {$EndIf}

   tMyData = class
      private
       FileChanged : boolean;

       {$IfDef UseFDMemTable}
          FDMemTable : tFDMemTable;
       {$EndIf}

       {$IfDef UseFireDacSQLlite}
          dbMain : TFDConnection;
          fdTable : tFDTable;
          FireDAC_SQL : boolean;
       {$EndIf}

       {$IfDef BDELikeTables}
          TheBDEData : tMyTable;
       {$EndIf}

       {$IfDef UseTCLientDataSet}
          TheClientDataSet : tClientDataSet;
       {$EndIf}
       {$IfDef VCL}
          procedure StartRewrite(var OldName : PathStr);
          procedure EndRewrite(var OldName,fName : PathStr);
       {$EndIf}
      public
       Filter : ANSIString;
       Filtered,
       CheckDeletes,
       DBFmovedToRAM,
       FieldWidthIssues : boolean;
       FullTableName : PathStr;
       TableName     : shortstring;
       RecsTaggedDeletion,
       FiltRecsInDB,
       TotRecsInDB : integer;
       constructor Create(var fName : PathStr; dbMode : tmydbMode = dbmyDefault);
       destructor Destroy;
        procedure ApplyFilter(TheFilter : ANSIstring); //inline;
        procedure First; inline;
        procedure Last; inline;
        procedure Next; inline;
        procedure Prior; inline;
        procedure Insert; inline;
        procedure Edit; inline;
        procedure Delete; inline;
        procedure Post; inline;
        function EOF : boolean; inline;
        function BOF : boolean; inline;
        function RecordCount : integer;  inline;
        {$IfDef ExactRecordCountFix}
           function ExactRecordCount : integer;
        {$EndIf}
        function RecNo : integer; inline;
        function FieldCount : integer; inline;
        procedure ClearAllRecords;
        procedure AssignEmpSource(EmpSource : TDataSource);
        procedure SaveToFile;
        function  DBBakDir : PathStr;
        procedure MarkRecordForDeletion;
        function PurgeDeletedRecords : boolean;

        function FieldExists(WantFieldName : ANSIString; NeedAType : boolean = false; NeedType : tFieldType = ftFloat) : boolean; inline;
        function GetFieldByNameAsFloat(FieldName : ANSIString) : float64; inline;
        function GetFieldByNameAsInteger(FieldName : ANSIString) : integer; inline;
        function GetFieldByNameAsString(FieldName : ANSIString) : ANSIString; inline;

        procedure SetFieldByNameAsFloat(FieldName : ANSIString; value : float64; MaxDec : integer = -1); inline;
        procedure SetFieldByNameAsInteger(FieldName : ANSIString; value : integer); inline;
        procedure SetFieldByNameAsString(FieldName : ANSIString; value : String); inline;
        procedure CarefullySetFloat(FieldName : ANSIString; var Value : float64; Precision : float64);  //inline;
        procedure CarefullySetFloat32(FieldName : ANSIString; var Value : float32; Precision : float32);  //inline;

        function CarefullyGetFieldByNameAsFloat64(FieldName : ANSIString; var Value : float64) : boolean; //inline;
        function CarefullyGetFieldByNameAsFloat32(FieldName : ANSIString; var Value : float32) : boolean; //inline;
        function CarefullyGetFieldByNameAsInteger(FieldName : ANSIString; var Value : integer) : boolean; //inline;

        function FieldTypeAndLength(WantFieldName : ANSIString) : AnsiString;
        function ListUniqueEntriesInDB(FieldName : ANSIString) : tStringList;
        function NumUniqueEntriesInDB(FieldName : ANSIString) : integer;
        function FieldsInDataBase(Alphabetize : boolean = false) : tStringList;
        function GetFieldName(i : integer) : ANSIString;
        function GetFieldIndex(FieldName : ANSIString) : integer;
        function GetFieldType(fName : ANSIString) : tFieldType; overload;
        function GetFieldType(i : integer) : tFieldType; overload;
        function GetFieldLength(WantFieldName : ANSIString) : integer;
        function GetFieldDataSize(I : integer) : integer;
        function GetFieldPrecision(fName : ANSIString) : integer; overload;
        function GetFieldPrecision(i : integer) : integer; overload;
        function NCountField : shortstring;
        function FieldAllBlanks(FieldName : ANSIString) : boolean;
        function FieldAllZeros(FieldName : ANSIString) : boolean;
        function FieldHasChar(FieldName : ANSIString;  ch : shortstring) : boolean;
        function GetFullFileName(var fName : PathStr) : boolean;

        procedure SetAllFieldsBlank;

        function IsNumericField(WantFieldName : ANSIString) : boolean;
        function IsFloatField(WantFieldName : ANSIString) : boolean;
        function IsIntegerField(WantFieldName : ANSIString) : boolean;
        function IsStringField(WantFieldName : ANSIString) : boolean;
        function IsStringOrIntergerField(WantFieldName : ANSIString) : boolean;

        function FieldSum(FieldDesired : shortstring) : float64;
        function FieldAverage(FieldDesired : shortstring) : float64;
        function FieldMedian(FieldDesired : shortstring) : float64;
        function FindFieldMax(FieldDesired : shortString) : float64;
        function FindFieldMin(FieldDesired : shortString) : float64;
        function FieldStdDev(FieldDesired : shortstring) : float64;
        function FindFieldRange(FieldDesired : shortString; var aMinVal,aMaxVal : float64) : boolean;
        function GetFieldStatistics(FieldDesired : ShortString) : tMomentVar;

        function MakeImageTag(ThumbnailDir : PathStr; FieldName : ANSIString) : ANSIString;
        function BoundingBoxPresent : boolean;
        function AssignLabelName : shortstring;

        procedure LengthPointRecords(var CumDist,StraightDist,LineHeading : float64);
        function ValidLatLongFromTable(var Lat,Long : float64) : boolean;
        function GetXYZFromTable(var x,y,z : float64) : boolean;

        procedure CopyRecordToEndOfTable(DeleteRecord : boolean = false);

        function InsureFieldPresentAndAdded(ft : TFieldType; FieldName : ANSIString; FieldLength : integer; FieldDecimals : integer = 0) : boolean;
        function AddBoundingBox : boolean;
        function GetRecordBoundingBox : sfBoundBox;
        procedure SetBoundingBox(bbox : sfBoundBox);
        function DeleteField(theField : shortstring) : boolean;
        function TrimField(theField : shortstring; NewLength : integer) : boolean;

        function GetTableStructure : tStringList;
        procedure SetColorFromPlatformColor(Color : tPlatformColor);
        procedure SetColorFromTColor(Color : tColor);
        function PlatformColorFromTable : tPlatformColor;
        function TColorFromTable : tColor;
        procedure GetLineColorAndWidth(var LineColor : tPlatFormColor; var LineSize : integer);
        procedure SetLineColorAndWidth(LineColor : tPlatFormColor; LineSize : integer);

        procedure DefinePointSymbol(var Symbol : tDrawingSymbol; var SymbolSize : byte; var SymbolColor : tPlatformColor); overload;
        procedure PostPointSymbol(Symbol : tDrawingSymbol; SymbolSize : byte; SymbolColor : tPlatformColor); overload;

        procedure DefinePointSymbol(var Symbol : tFullSymbolDeclaration); overload;
        procedure PostPointSymbol(Symbol : tFullSymbolDeclaration); overload;

        procedure TrimAllStringFields(JustOne : ANSIString = '');
        procedure ExportToXML(fName : PathStr);
        procedure ExportToSQLite(fName : PathStr = '');
        procedure ProgressVars(var rc,nc : integer);

        {$IfDef VCL}
           procedure DefineFontFromTable(Font : Graphics.tFont);
           procedure PostFont(aFont : Graphics.tFont);
           function PickField(Mess: ShortString; TypesAllowed : tSetFieldType) : ShortString;
        {$EndIf}
   end;



{$A-}
type
   //http://ulisse.elettra.trieste.it/services/doc/dbase/DBFstruct.htm
   tDBaseIIITableFileHeader = packed record
      ValidDesig : byte;                                         //1
      LastUpdate : array[1..3] of byte;  //year, month, day      //2-4
      RecordsInTable : int32;                                    //5-8
      BytesInHeader,                                             //9-10
      BytesInRecord : Int16;                                     //11-12
      Reserved  : array[1..20] of byte;                          //13-32
   end;
   tTableFieldDescriptor = packed record
      FieldName : array[0..10] of AnsiChar;
      FieldType : AnsiChar;
      FieldDataAddress : int32;
      FieldLength,
      FieldDecimalCount : byte;
      ReservedForLAN : array[1..2] of byte;
      WorkAreaID : byte;
      ReservedForLAN2 : array[1..2] of byte;
      SetFieldsFlag : byte;
      Reserved : array[24..31] of byte;
   end;

function SQLTypeDefString(ft : tFieldType; len : integer) : shortString;

function ftIsNumeric(ft : tFieldType) : boolean;
function ftIsInteger(ft : tFieldType) : boolean;
function ftIsJoinable(ft : tFieldType) : boolean;

const
   NumericFieldTypes = [ftFloat,ftInteger,ftSmallInt,ftLargeInt];
   RecNoFName = 'REC_ID';

{$IfDef UseFireDacSQLlite}
   procedure OpenSQLLiteFiles(fName : PathStr; var dbMain : TFDConnection; var fdTable : tfdTable; aLine : ANSIString = '');
{$EndIf}

{$IfDef BDELikeTables}
   function CreateAndOpenTable(var TheData : tMyTable; fName : PathStr) : boolean;
{$EndIf}

function IsFieldVisible(db : integer; fName : ansistring; var VisCols :  Array100Boolean) : boolean;


implementation

uses
   {$IfDef VCL}
      PetImage,
   {$EndIf}

   {$IfDef ExGIS}
   {$Else}
      DataBaseCreate,
      PETDBUtils,
      DEMDatabase,
   {$EndIf}

   BaseMap,
   DEMDefs,
   DEMDef_routines,
   Petmar;


function IsFieldVisible(db : integer; fName : ansistring; var VisCols :  Array100Boolean) : boolean;
var
   ID : integer;
begin
   ID := GISdb[DB].MyData.GetFieldIndex(fName);
   Result := (ID in [0..100]) and (VisCols[ID]);
end;



function ftIsNumeric(ft : tFieldType) : boolean;
begin
   Result := ft in NumericFieldTypes;
end;

function ftIsInteger(ft : tFieldType) : boolean;
begin
   Result := ft in [ftInteger, ftSmallInt,ftLargeInt];
end;

function ftIsJoinable(ft : tFieldType) : boolean;
begin
   Result := ft in [ftString,ftInteger, ftSmallInt,ftLargeInt];
end;

function SQLTypeDefString(ft : tFieldType; len : integer) : shortString;
begin
    case ft of
        ftString : Result := 'varchar(' + IntToStr(len) + ')';
        ftInteger : Result := 'integer';
        ftSmallInt : Result := 'smallint';
        ftFloat : Result := 'float64';
    end;
end;

function tMyData.AssignLabelName : shortstring;

   function IsItThere(fName : shortstring) : shortstring;
   begin
      if FieldExists(fName) then result := fname
      else Result := '';
   end;

begin
   Result := IsItThere('EVT_NAME');
   if Result = ''  then Result := IsItThere('AREA');
   if Result = ''  then Result := IsItThere('PLACE');
   if Result = ''  then Result := IsItThere('FEATURE');
   if Result = ''  then Result := IsItThere('FENAME');
   if Result = ''  then Result := IsItThere('BASIN_ID');
   if Result = ''  then Result := IsItThere('CATEGORY');
   if Result = ''  then Result := IsItThere('NAME');
end;


function tMyData.InsureFieldPresentAndAdded(ft : TFieldType; FieldName : ANSIString; FieldLength : integer; FieldDecimals : integer = 0) : boolean;


   procedure CheckBDETable;
   var
      fName,Oldname : PathStr;
      CreateDataBase : tCreateDataBase;
      {$IfDef RecordFullOpenDB}  Output : tStringList; {$EndIf}
   begin
      {$IfDef BDELikeTables}
         if (TheBDEdata <> Nil) then begin
            fName := ChangeFileExt(FullTableName,DefaultDBExt);
            {$IfDef RecordFieldPresent} WriteLineToDebugFile('must add field ' + FieldName + ' to dBase ' + ExtractFileName(fName) + '  len=' + IntToStr(FieldLength) + ' decs='+ IntToStr(FieldDecimals)); {$EndIf}
            First;
            StartRewrite(OldName);
            CreateDataBase := tCreateDataBase.Create(fName);
            CreateDataBase.ReadDBFHeader(OldName);
            CreateDataBase.AddAField(FieldName, ft, FieldLength, FieldDecimals);
            CreateDataBase.WriteCorrectHeader(true);
            CreateDataBase.Destroy;
            EndRewrite(OldName,fName);
            Result := true;

            {$IfDef RecordFullOpenDB}
               Output := GetTableStructure;
               WriteLineToDebugFile('Structure after reopen for ' +  TableName + '  recs=' + IntToStr(RecordCount));
               WriteStringListToDebugFile(Output);
               Output.Free;
            {$EndIf}
         end;
      {$EndIf}
   end;


   procedure CheckSQLLite;
   var
      aline : ANSIstring;
   begin
      {$IfDef UseFireDacSQLlite}
         if (dbMain <> Nil) then begin
            aline := 'ALTER TABLE ' + TableName + ' ADD COLUMN ' + FieldName + ' ' + SQLTypeDefString(ft,FieldLength);
            {$IfDef RecordFieldPresent} WriteLineToDebugFile('sql=' + aline); {$EndIf}
            dbMain.ExecSQL(aLine);
            if (fdTable <> Nil) then  begin
               fdTable.Destroy;
               fdTable := Nil;
            end;
            if (dbMain <> Nil) then begin
               dbMain.Destroy;
               dbMain := Nil;
            end;
            aLine := '';
            OpenSQLLiteFiles(FullTableName,dbMain,fdTable,aLine);
         end;
      {$EndIf}
   end;

   procedure CheckTCLientDataSet;
   {$IfDef UseTCLientDataSet}
      var
         NewClientDataSet :  tClientDataSet;
         i,{j,}flen : integer;
   {$EndIf}
   begin
      {$IfDef UseTCLientDataSet}
         if (TheClientDataSet <> Nil) then begin
            {$IfDef RecordFieldPresent} WriteLineToDebugFile('TheClientDataSet start'); {$EndIf}
             NewClientDataSet := tClientDataSet.Create(application);
             for i := 0 to pred(FieldCount) do begin
                ft := GetFieldType(i);
                if (ft in [ftString]) then fLen := GetFieldDataSize(i)
                else fLen := 0;
                NewClientDataSet.FieldDefs.Add(GetFieldName(i), ft, fLen,False );
             end;
             NewClientDataSet.CreateDataset;
             NewClientDataSet.Open;
            {$IfDef RecordFieldPresent} WriteLineToDebugFile('NewClientDataSet opened'); {$EndIf}

             TheClientDataSet.First;
             //j := 0;
             while not TheClientDataSet.eof do begin
               //inc(j);
                NewClientDataSet.Insert;
                for i := 0 to pred(FieldCount) do begin
                   NewClientDataSet.Fields[i].Assign(theClientDataSet.Fields[i]);
                end;
                TheClientDataSet.Next;
             end;
             theClientDataset.Free;
             NewClientDataSet.SaveToFile(FullTableName);
             NewClientDataSet.Free;

             TheClientDataSet := tClientDataSet.Create(Application);
             TheClientDataSet.LoadFromFile(FullTableName);
             TheClientDataSet.LogChanges := false;
             {$IfDef RecordFieldPresent} WriteLineToDebugFile('Done ' + IntToStr(j) + ' '); {$EndIf}
          end;
       {$EndIf}
      end;


begin
   FieldName := UpperCase(FieldName);
   if FieldExists(FieldName) then begin
      {$IfDef RecordFieldPresent} WriteLineToDebugFile('InsureFieldPresentAndAdded, field already present ' + FieldName + ' in ' + TableName); {$EndIf}
      Result := false;
   end
   else begin
      {$IfDef RecordFieldPresent} WriteLineToDebugFile('InsureFieldPresentAndAdded, Must Add field ' + FieldName + ' to ' + TableName); {$EndIf}
      FileChanged := true;

      CheckBDETable;
      CheckSQLlite;
      CheckTCLientDataSet
      {$IfDef UseFDMemTable}
         if (FDMemTable <> nil) then begin
            MessageToContine('Not enabled');
            exit;
         end;
      {$EndIf}
   end;
   {$IfDef RecordFieldPresent} WriteLineToDebugFile('InsureFieldPresentAndAdded out,  recs=' + IntToStr(RecordCount)); {$EndIf}
end;


function tMyData.FindFieldMax(FieldDesired : shortString) : float64;
var
   aMinVal : float64;
begin
   FindFieldRange(FieldDesired,aMinVal,Result);
end;


function tMyData.FindFieldMin(FieldDesired : shortString) : float64;
var
   aMaxVal : float64;
begin
   FindFieldRange(FieldDesired,Result,aMaxVal);
end;

function tMyData.FindFieldRange(FieldDesired : shortString; var aMinVal,aMaxVal : float64) : boolean;
var
   Num  : integer;
   fval : float64;
begin
   First;
   aMaxVal := -99e99;
   aMinVal := +99e99;
   Num := 0;
   while not eof do begin
      if CarefullyGetFieldByNameAsFloat64(FieldDesired,fval) then begin
         Petmath.CompareValueToExtremes(fVal,aMinVal,aMaxVal);
         inc(Num);
      end;
      Next;
   end;
   result := Num > 0;
end;



procedure tMyData.ProgressVars(var rc,nc : integer);
begin
   rc := FiltRecsInDB;
   nc := (rc div 20);
   if nc < 1 then nc := 1;
end;

function tMyData.GetFullFileName(var fName : PathStr) : boolean;
begin
   Result := false;
   fName := GetFieldByNameAsString('FILENAME');
   if (fName <> '') then begin
      if not FileExists(fName) then fName := ExtractFilePath(FullTableName) + fname;
      Result := FileExists(fName);
   end;
end;


 {$IfDef UseFireDacSQLlite}

procedure OpenSQLLiteFiles(fName : PathStr; var dbMain : TFDConnection; var fdTable : tfdTable; aLine : ANSIString = '');
var
   TableThere : boolean;
{$IfDef AllowMemoryDB}
//move into memory to improve performance
//http://docwiki.embarcadero.com/RADStudio/XE6/en/Using_SQLite_with_FireDAC
   MemoryDB : boolean;
   FDSQLiteBackup1 : TFDSQLiteBackup;
{$EndIf}
begin
   {$IfDef RecordMyDataCreation} WriteLineToDebugFile('OpenSQLLiteFiles for ' + fName + '   aline=' + aLine); {$EndIf}
   TableThere := FileExists(fName);

   {$IfDef AllowMemoryDB}
   MemoryDB := MDDef.AllowMemoryDB;
   {$EndIf}

   if (DBMain = Nil) then begin
      {$IfDef RecordMyDataCreation} WriteLineToDebugFile('(DBMain = Nil)'); {$EndIf}
      {$IfDef VCL}
      dbMain := TFDConnection.Create(Application);
      {$EndIf}

      {$IfDef FMX}
      dbMain := TFDConnection.Create(nil);
      {$EndIf}
      {$IfDef RecordMyDataCreation} WriteLineToDebugFile('(DBMain created)'); {$EndIf}

      dbMain.Params.Clear;
      dbMain.Params.Add('DriverID=SQLite');

      {$IfDef RecordMyDataCreation} WriteLineToDebugFile('dbMain.Params.Add DriverID=SQLite'); {$EndIf}

      {$IfDef AllowMemoryDB}
      if MemoryDB then dbMain.Params.Add('Database=:memory:')
      else
      {$EndIf}
      dbMain.Params.Add('Database=' + fName);

      dbMain.Params.Add('Password=');
      if (aLine <> '') then begin
         {$IfDef RecordMyDataCreation} WriteLineToDebugFile('ExecSQL=' + aline); {$EndIf}
         dbMain.ExecSQL(aLine);
         {$IfDef RecordMyDataCreation} WriteLineToDebugFile('ExecSQL done'); {$EndIf}
      end;
      {$IfDef RecordMyDataCreation} WriteLineToDebugFile('call dbMain.open'); {$EndIf}
      dbMain.Open;

      {$IfDef AllowMemoryDB}
         if MemoryDB then begin
            FDSQLiteBackup1.Database := fName;
            FDSQLiteBackup1.DestDatabaseObj := dbMain.CliObj;
            FDSQLiteBackup1.DestMode := smCreate;
            FDSQLiteBackup1.Backup;
         end;
      {$EndIf}
      {$IfDef RecordMyDataCreation} WriteLineToDebugFile('dbMain opened'); {$EndIf}
   end;

   if TableThere and (fdTable = Nil) then begin
      {$IfDef RecordMyDataCreation} WriteLineToDebugFile('Opening fdTable ' + fName); {$EndIf}

      {$IfDef VCL}
         fdTable := tfdTable.Create(Application);
      {$EndIf}

      {$IfDef FMX}
         fdTable := tfdTable.Create(nil);
      {$EndIf}

      {$IfDef RecordMyDataCreation} WriteLineToDebugFile('Created fdTable'); {$EndIf}

      fdTable.Connection := dbMain;
      fdTable.TableName := ExtractFileNameNoExt(fName);
      fdTable.Open;
      {$IfDef RecordMyDataCreation} WriteLineToDebugFile('fdTable opened'); {$EndIf}
   end;
end;
{$EndIf}


function TypeFromSQLiteString(TypeStr : shortstring) : tFieldType;
begin
   TypeStr  := UpperCase(TypeStr);
   if (TypeStr = 'FLOAT') then Result := ftFloat
   else if (TypeStr = 'SMALLINT') then Result := ftSmallInt
   else if (Copy(TypeStr,1,7) = 'VARCHAR') or (TypeStr = 'TEXT') then Result := ftString
   else MessageToContinue('Undefined type, ' + TypeStr);
end;


procedure tMyData.ApplyFilter(TheFilter: ANSIstring);
begin
   {$IfDef RecordMyDataFilter} WriteLineToDebugFile('tMyData.ApplyFilter=' + TheFilter); {$EndIf}

    Filtered := (TheFilter <> '');
    Filter := TheFilter;

    {$IfDef BDELikeTables}
       if (TheBDEdata <> Nil) then begin
          if MDDef.DBfilterCaseInSensitive then TheBDEdata.FilterOptions := [foCaseInsensitive]
          else TheBDEdata.FilterOptions := [];
          TheBDEdata.Filter := TheFilter;
          TheBDEdata.Filtered := true;
          TheBDEdata.First;
       end;
    {$EndIf}

    {$IfDef UseFDMemTable}
       if (FDMemTable <> nil) then begin
          if MDDef.DBfilterCaseInSensitive then FDMemTable.FilterOptions := [foCaseInsensitive]
          else FDMemTable.FilterOptions := [];
          FDMemTable.Filter := TheFilter;
          FDMemTable.Filtered := true;
       end;
    {$EndIf}

    {$IfDef UseFireDacSQLlite}
       if (fdTable <> Nil) then begin
          if MDDef.DBfilterCaseInSensitive then fdTable.FilterOptions := [foCaseInsensitive]
          else fdTable.FilterOptions := [];
          fdTable.Filter := TheFilter;
          fdTable.Filtered := true;
       end;
    {$EndIf}

   {$IfDef UseTCLientDataSet}
       if (TheClientDataSet <> Nil) then begin
          if MDDef.DBfilterCaseInSensitive then TheClientDataSet.FilterOptions := [foCaseInsensitive]
          else TheClientDataSet.FilterOptions := [];
          TheClientDataSet.Filter := TheFilter;
          TheClientDataSet.Filtered := true;
       end;
    {$EndIf}

    if Filtered then FiltRecsInDB := RecordCount
    else FiltRecsInDB := TotRecsInDB;

   {$IfDef RecordMYDataFilter} WriteLineToDebugFile('tMyData.ApplyFilter out'); {$EndIf}
end;


function tMyData.TColorFromTable : tColor;
begin
   Result := ConvertPlatformColorToTColor(PlatformColorFromTable);
(*
var
   i : integer;
begin
   Result := clRed;
   if FieldExists('COLOR') and CarefullyGetFieldByNameAsInteger('COLOR',i) then begin
      Result := i;
   end
   else if FieldExists('RED') then begin
      if IsFloatField('RED') then Result := RGB(round(255 * GetFieldByNameAsFloat('RED')),round(255 *GetFieldByNameAsFloat('GREEN')),round(255 *GetFieldByNameAsFloat('BLUE')))
      else Result := RGB(GetFieldByNameAsInteger('RED'),GetFieldByNameAsInteger('GREEN'),GetFieldByNameAsInteger('BLUE'));
   end;
*)
end;


function tMyData.PlatformColorFromTable : tPlatformColor;

         function CheckColorField(fName : shortstring) : boolean;
         var
            i : integer;
         begin
            CheckColorField := false;
            if FieldExists(fName) then begin
               if CarefullyGetFieldByNameAsInteger(fName,i) then begin
                  PlatformColorFromTable := ConvertTColorToPlatformColor(i);
                  CheckColorField := true;
               end;
            end;
         end;

         function CheckRGBFields(rf,gf,bf : shortstring) : boolean;
         begin
            Result := FieldExists(rf);
            if Result then begin
               if IsFloatField(rf) then
                  PlatformColorFromTable := RGBtrip(round(255 * GetFieldByNameAsFloat(rf)),round(255 *GetFieldByNameAsFloat(gf)),round(255 *GetFieldByNameAsFloat(bf)))
               else PlatformColorFromTable := RGBtrip(GetFieldByNameAsInteger(rf),GetFieldByNameAsInteger(gf),GetFieldByNameAsInteger(bf));
            end;
         end;

begin
   {$IfDef VCL}
      if CheckColorField('COLOR') or CheckColorField('LINE_COLOR') or CheckRGBFields('RED','GREEN','BLUE') or CheckRGBFields('r','g','b') then begin
      end
      else PlatformColorFromTable := claWhite;
   {$EndIf}
end;


procedure tMyData.First;
begin
   {$IfDef BDELikeTables}
      if (TheBDEdata <> Nil) then TheBDEdata.First;
   {$EndIf}
   {$IfDef UseTCLientDataSet}
      if (TheClientDataSet <> Nil) then TheClientDataSet.First;
   {$EndIf}
   {$IfDef UseFireDacSQLlite}
       if (fdTable <> Nil) then begin
          fdTable.First;
       end;
    {$EndIf}
    {$IfDef UseFDMemTable}
       if (FDMemTable <> nil) then begin
          FDMemTable.First;
       end;
    {$EndIf}
end;


function tMyData.GetFieldIndex(FieldName : ANSIString) : integer;
var
   i : integer;
   fName : ANSIstring;
begin
   for i := 0 to pred(FieldCount) do begin
      fName := GetFieldName(i);
      if (fName = FieldName) then begin
         Result := i;
         exit;
      end;
   end;
   Result := -1;
end;

function tMyData.GetFieldName(i : integer) : ANSIString;
begin
   {$IfDef BDELikeTables}
      if (TheBDEdata <> Nil) then Result := TheBDEdata.Fields[i].FieldName;
   {$EndIf}
   {$IfDef UseTCLientDataSet}
      if (TheClientDataSet <> Nil) then Result := TheClientDataSet.Fields[i].FieldName;
   {$EndIf}

   {$IfDef UseFireDacSQLlite}
       if (fdTable <> Nil) then begin
          Result := fdTable.Fields[i].FieldName;
       end;
    {$EndIf}
    {$IfDef UseFDMemTable}
       if (FDMemTable <> nil) then begin
          Result := fdMemTable.Fields[i].FieldName;
       end;
    {$EndIf}
end;


function tMyData.GetFieldDataSize(I : integer) : integer;
var
   i2 : integer;
   f : file;
   {$IfDef BDELikeTables}
      DBaseIIITableFileHeader : tDBaseIIITableFileHeader;
      TableFieldDescriptor : tTableFieldDescriptor;
   {$EndIf}
begin
   {$IfDef BDELikeTables}
      if (TheBDEdata <> Nil) or DBFmovedToRAM then begin
         assignFile(f,FullTableName);
         reset(f,1);
         BlockRead(f, DBaseIIITableFileHeader, SizeOf(tDBaseIIITableFileHeader));
         for i2 := 0 to i do BlockRead(f, TableFieldDescriptor, SizeOf(tTableFieldDescriptor));
         Result := TableFieldDescriptor.FieldLength;
         CloseFile(f);
      end;
   {$EndIf}
   {$IfDef UseFireDacSQLlite}
       if (fdTable <> Nil) then begin
          Result := fdTable.Fields[i].DataSize;
       end;
   {$EndIf}
   {$IfDef UseFDMemTable}
      if (FDMemTable <> nil) then begin
         Result := fdMemTable.Fields[i].DataSize;
      end;
   {$EndIf}
   {$IfDef UseTCLientDataSet}
      if (TheClientDataSet <> Nil) then Result := TheClientDataSet.Fields[i].DataSize;
   {$EndIf}
end;


constructor tMyData.Create(var fName : PathStr; dbMode : tmydbMode = dbmyDefault);
{$IfDef SQLiteDefaultDBs}
   var
      fName2 : PathStr;
{$EndIf}
{$IfDef UseTCLientDataSet}
   var
      i : integer;
{$EndIf}
{$IfDef RecordOpenDB}
   var
      Output : tStringList;
{$EndIf}

begin
   {$IfDef RecordMyDataCreation} WriteLineToDebugFile('tMyData.Create ' + fName); {$EndIf}

   FileChanged := false;
   FieldWidthIssues := false;
   Filtered := false;
   DBFmovedToRAM := false;
   Filter := '';
   RecsTaggedDeletion := 0;

   {$IfDef UseFDMemTable}
      FDMemTable := nil;
   {$EndIf}

   {$IfDef BDELikeTables}
      TheBDEdata := Nil;
   {$EndIf}

   {$IfDef UseTCLientDataSet}
      TheClientDataSet := Nil;
   {$EndIf}

   {$IfDef UseFireDacSQLlite}
      dbMain := Nil;
      fdTable := Nil;
      FireDAC_SQL := false;
   {$EndIf}

   if (FName = '') then begin
      if not GetFileFromDirectory('data base','*.dbf',fName) then exit;
   end;

   if FileExtEquals(fName,'.csv') or FileExtEquals(fName,'.txt') then begin
      OpenNumberedGISDataBase(i,fName,false);
      CloseAndNilNumberedDB(i);
      fName := ChangeFileExt(fName,'.dbf');
   end;


   if (FName <> '') and FileExists(fName) then begin
       CheckFileNameForSpaces(fName);
       FullTableName := fName;
       TableName := ExtractFileNameNoExt(fName);

       {$IfDef UseTCLientDataSet}
          if FileExtEquals(fName,'.xml') or FileExtEquals(fName,'.cds') then begin
              {$IfDef RecordMyDataCreation} WriteLineToDebugFile('call tClientDataSet.Create'); {$EndIf}
              TheClientDataSet := tClientDataSet.Create(Application);
              {$IfDef RecordMyDataCreation} WriteLineToDebugFile('call tClientDataSet.LoadFromFile'); {$EndIf}
              TheClientDataSet.LoadFromFile(FName);
              {$IfDef RecordMyDataCreation} WriteLineToDebugFile('done tClientDataSet.LoadFromFile'); {$EndIf}
              TheClientDataSet.LogChanges := false;
              InsureFieldPresentAndAdded(ftInteger,RecNoFName,8);
              {$IfDef RecordMyDataCreation} WriteLineToDebugFile('Added ' + RecNoFName); {$EndIf}
              First;
              i := 0;
              while not eof do begin
                 inc(i);
                 Edit;
                 SetFieldByNameAsInteger(RecNoFName,i);
                 Next;
              end;
              First;
              TheClientDataSet.SaveToFile(FullTableName,dfBinary);
              TotRecsInDB := TheClientDataSet.RecordCount;
              {$IfDef RecordMyDataCreation} WriteLineToDebugFile('CDS/XML set up'); {$EndIf}
          end;
       {$EndIf}

      {$IfDef SQLiteDefaultDBs}
         if FileExtEquals(fName,'.dbf') or FileExtEquals(fName,'.db') then begin
            fName2 := ChangeFileExt(fName,DefaultDBExt);
            if not FileExists(fName2) then begin
               {$IfDef RecordMyDataCreation} WriteLineToDebugFile('ConvertDBFtoSQLite'); {$EndIf}
               fName := ChangeFileExt(fName,'.dbf');
               ConvertDBFtoSQLite(fName);
            end;
            fName := fName2;
            TotRecsInDB := RecordCount;
            {$IfDef RecordMyDataCreation} WriteLineToDebugFile('changed input file to ' + fName); {$EndIf}
         end;
      {$EndIf}

      {$IfDef UseFDMemTable}
         if FileExtEquals(fName,'.adb') then begin
            FDMemTable := tFDMemTable.Create(Nil);
            FDMemTable.LoadFromFile(fName);
            FDMemTable.Open;
            TotRecordsInDB := FDMemTable.RecordCount;
         end;
      {$EndIf}

      if FileExtEquals(fName,'.dbf') then begin
         {$IfDef BDELikeTables}
            if (dbMode = dbmCDS) and MDDef.AllowMemoryLinkDB then begin
              {$IfDef TrackCDStiming} WriteLineToDebugFile('call LoadDBFtoClientDataSet ' + ExtractFileName(fName)); {$EndIf}
              LoadDBFtoClientDataSet(fName,TheClientDataSet);
              TotRecsInDB := TheClientDataSet.RecordCount;
              TheClientDataSet.LogChanges := false;
              DBFmovedToRAM := true;
              {$IfDef TrackCDStiming} WriteLineToDebugFile('done LoadDBFtoClientDataSet'); {$EndIf}
            end
            else begin
              {$IfDef RecordMyDataCreation} WriteLineToDebugFile('call CreateAndOpenTable'); {$EndIf}
              CreateAndOpenTable(Self.TheBDEdata,fName);
              TotRecsInDB := Self.TheBDEdata.RecordCount;
            end;
         {$Else}
             {$IfDef UseTCLientDataSet}
                {$IfDef RecordMyDataCreation} WriteLineToDebugFile('call LoadDBFtoClientDataSet'); {$EndIf}
                LoadDBFtoClientDataSet(fName,TheClientDataSet);
                TheClientDataSet.LogChanges := false;
             {$EndIf}
         {$EndIf}
       end;

      {$IfDef UseFireDacSQLlite}
         if FileExtEquals(fName,'.db') then begin
            {$IfDef RecordMyDataCreation} WriteLineToDebugFile('doing FireDac'); {$EndIf}
            FireDAC_SQL := true;
            FieldWidthIssues := true;
            OpenSQLLiteFiles(fName,dbMain,fdTable);
            fdTable.Last;
            TotRecsInDB := fdTable.RecordCount;
            fdTable.First;
         end;
      {$EndIf}

       {$IfDef RecordMyDataCreation}
          WriteLineToDebugFile('recs=' + IntToStr(RecordCount));
         {$IfDef UseTCLientDataSet} if (TheClientDataSet <> Nil) then WriteLineToDebugFile('Loaded tClientDataSet'); {$EndIf}
      {$EndIf}

      FiltRecsInDB := TotRecsInDB;

      {$IfDef RecordFullOpenDB}
         if (UpperCase(ExtractFilePath(fName)) <> UpperCase(MDTempDir)) then begin
            Output := GetTableStructure;
            WriteLineToDebugFile('Structure after tMyData.Create for ' +  fName,true);
            WriteStringListToDebugFile(Output);
            Output.Free;
         end;
      {$EndIf}
   end
   else begin
      MessageToContinue('DB missing ' + fName);
   end;
   {$IfDef RecordMyDataCreation} WriteLineToDebugFile('tMyData.Create out'); {$EndIf}
end;


procedure tMyData.SaveToFile;
begin
   {$IfDef UseFDMemTable}
      if (FDMemTable <> nil) then begin
         FDMemTable.SaveToFile(FullTableName);
      end;
   {$EndIf}
end;


destructor tMyData.Destroy;
begin
   {$IfDef RecordMYDataCreation} WriteLineToDebugFile('tMyData.Destroy ' + TableName); {$EndIf}
   PurgeDeletedRecords;
   SaveToFile;

   {$IfDef BDELikeTables}
      FreeAndNil(TheBDEdata);
   {$EndIf}

   {$IfDef UseTCLientDataSet}
      if (TheClientDataSet <> Nil) then begin
         if FileChanged then begin
            TheClientDataSet.SaveToFile(FullTableName,dfBinary);
            Delay(1000);
         end;
         TheClientDataSet.Destroy;
      end;
   {$EndIf}

   {$IfDef UseFireDacSQLlite}
       FreeAndNil(fdTable);
       FreeAndNil(dbMain);
   {$EndIf}

   {$IfDef UseFDMemTable}
      FreeAndNil(FDMemTable);
   {$EndIf}
end;


procedure tMyData.ClearAllRecords;
begin
   First;
   while (RecordCount > 0) do Delete;
   FileChanged := true;
end;


procedure tMyData.Edit;
begin
   FileChanged := true;
   {$IfDef BDELikeTables}
      if (TheBDEdata <> Nil) then TheBDEdata.Edit;
   {$EndIf}
   {$IfDef UseFireDacSQLlite}
      if (fdTable <> Nil) then fdTable.Edit;
   {$EndIf}

   {$IfDef UseTCLientDataSet}
      if (TheClientDataSet <> Nil) then TheClientDataSet.Edit;
   {$EndIf}
   {$IfDef UseFireDacSQLlite}
      if (fdTable <> Nil) then fdTable.Edit;
   {$EndIf}
    {$IfDef UseFDMemTable}
       if (FDMemTable <> nil) then begin
          fdMemTable.Edit;
       end;
    {$EndIf}
end;

procedure tMyData.Post;
begin
   {$IfDef BDELikeTables}
      if (TheBDEdata <> Nil) then TheBDEdata.Post;
   {$EndIf}

   {$IfDef UseTCLientDataSet}
      if (TheClientDataSet <> Nil) then TheClientDataSet.Post;
   {$EndIf}

   {$IfDef UseFireDacSQLlite}
      if (fdTable <> Nil) then fdTable.Post;
   {$EndIf}

   {$IfDef UseFDMemTable}
      if (FDMemTable <> nil) then fdMemTable.Post;
   {$EndIf}
end;

procedure tMyData.Insert;
begin
   {$IfDef TrackInsert} WriteLineToDebugFile('tMyData.Insert in'); {$EndIf}

   FileChanged := true;
   {$IfDef BDELikeTables}
      if (TheBDEdata <> Nil) then begin
          TheBDEdata.Insert;
       end;
   {$EndIf}

   {$IfDef UseTCLientDataSet}
      if (TheClientDataSet <> Nil) then TheClientDataSet.Insert;
   {$EndIf}

   {$IfDef UseFireDacSQLlite}
      if (fdTable <> Nil) then fdTable.Insert;
   {$EndIf}
   {$IfDef UseFDMemTable}
       if (FDMemTable <> nil) then begin
          fdMemTable.Insert;
       end;
   {$EndIf}

   inc(TotRecsInDB);

   {$IfDef TrackInsert} WriteLineToDebugFile('tMyData.Insert out'); {$EndIf}
end;


procedure tMyData.Delete;
begin
   FileChanged := true;
   {$IfDef BDELikeTables}
      if (TheBDEdata <> Nil) then begin
         TheBDEdata.Delete;
      end;
   {$EndIf}
   {$IfDef UseTCLientDataSet}
      if (TheClientDataSet <> Nil) then TheClientDataSet.Delete;
   {$EndIf}
   {$IfDef UseFireDacSQLlite}
      if (fdTable <> Nil) then fdTable.Delete;
   {$EndIf}
    {$IfDef UseFDMemTable}
    if (FDMemTable <> nil) then begin
       fdMemTable.Delete;
    end;
    {$EndIf}
   dec(TotRecsInDB);
end;


procedure tMyData.Prior;
begin
   {$IfDef BDELikeTables}
      if (TheBDEdata <> Nil) then TheBDEdata.Prior;
   {$EndIf}
   {$IfDef UseTCLientDataSet}
      if (TheClientDataSet <> Nil) then TheClientDataSet.Prior;
   {$EndIf}
   {$IfDef UseFireDacSQLlite}
      if (fdTable <> Nil) then fdTable.Prior;
  {$EndIf}
    {$IfDef UseFDMemTable}
       if (FDMemTable <> nil) then begin
          fdMemTable.Prior;
       end;
    {$EndIf}
end;


procedure tMyData.Next;
begin
   {$IfDef BDELikeTables}
      if (TheBDEdata <> Nil) then TheBDEdata.Next;
   {$EndIf}

   {$IfDef UseTCLientDataSet}
      if (TheClientDataSet <> Nil) then TheClientDataSet.Next;
   {$EndIf}

      {$IfDef UseFireDacSQLlite}
   if (fdTable <> Nil) then fdTable.Next;
   {$EndIf}
   {$IfDef UseFDMemTable}
   if (FDMemTable <> nil) then begin
       fdMemTable.Next;
   end;
   {$EndIf}
end;

procedure tMyData.Last;
begin
   {$IfDef BDELikeTables}
      if (TheBDEdata <> Nil) then TheBDEdata.Last;
   {$EndIf}

   {$IfDef UseTCLientDataSet}
      if (TheClientDataSet <> Nil) then TheClientDataSet.Last;
   {$EndIf}

   {$IfDef UseFireDacSQLlite}
      if (fdTable <> Nil) then fdTable.Last;
   {$EndIf}
   {$IfDef UseFDMemTable}
       if (FDMemTable <> nil) then begin
          fdMemTable.Last
       end;
   {$EndIf}
end;

function tMyData.EOF;
begin
   {$IfDef BDELikeTables}
      if (TheBDEdata <> Nil) then Result := TheBDEdata.Eof;
   {$EndIf}
   {$IfDef UseTCLientDataSet}
      if (TheClientDataSet <> Nil) then Result := TheClientDataSet.Eof;
   {$EndIf}
   {$IfDef UseFireDacSQLlite}
      if (fdTable <> Nil) then Result := fdTable.eof;
   {$EndIf}
   {$IfDef UseFDMemTable}
      if (FDMemTable <> nil) then begin
         Result := fdMemTable.eof;
      end;
  {$EndIf}
end;


function tMyData.BOF;
begin
   {$IfDef BDELikeTables}
      if (TheBDEdata <> Nil) then Result := TheBDEdata.Bof;
   {$EndIf}
   {$IfDef UseTCLientDataSet}
      if (TheClientDataSet <> Nil) then Result := TheClientDataSet.Bof;
   {$EndIf}
   {$IfDef UseFireDacSQLlite}
      if (fdTable <> Nil) then Result := fdTable.Bof;
   {$EndIf}
   {$IfDef UseFDMemTable}
      if (FDMemTable <> nil) then begin
         Result := fdMemTable.Bof;
      end;
   {$EndIf}
end;


function tMyData.RecNo : integer;
begin
   {$IfDef BDELikeTables}
      if (TheBDEdata <> Nil) then begin
         Result := TheBDEdata.RecNo;
      end;
   {$EndIf}
   {$IfDef UseTCLientDataSet}
      if (TheClientDataSet <> Nil) then Result := GetFieldByNameAsInteger(RecNoFName);  //TheClientDataSet.RecNo;
   {$EndIf}
   {$IfDef UseFireDacSQLlite}
      if (fdTable <> Nil) then Result := GetFieldByNameAsInteger(RecNoFName);
   {$EndIf}
   {$IfDef UseFDMemTable}
      if (FDMemTable <> nil) then begin
         Result := FDMemTable.RecNo;
      end;
   {$EndIf}
end;


function tMyData.CarefullyGetFieldByNameAsFloat64(FieldName : ANSIString; var Value : float64) : boolean;
var
   TStr : ANSIString;
begin
   Tstr := ptTrim(GetFieldByNameAsString(FieldName));
   Result := (TStr <> '');
   if Result then Value := StrToFloat(TStr);
end;

function tMyData.CarefullyGetFieldByNameAsFloat32(FieldName : ANSIString; var Value : float32) : boolean;
var
   TStr : ANSIString;
begin
   Tstr := ptTrim(GetFieldByNameAsString(FieldName));
   Result := (TStr <> '');
   if Result then Value := StrToFloat(TStr);
end;


function tMyData.CarefullyGetFieldByNameAsInteger(FieldName : ANSIString; var Value : integer) : boolean;
var
   TStr : ANSIString;
begin
   Tstr := ptTrim(GetFieldByNameAsString(FieldName));
   Result := (TStr <> '');
   if Result then Value := StrToInt(TStr);
end;


function tMyData.NCountField : shortstring;
begin
  Result := '';
  if FieldExists('N') then Result := 'N'
  else if FieldExists('NPTS') then Result := 'NPTS'
  else if FieldExists('COUNT') then Result := 'COUNT'
  else begin
     InsureFieldPresentAndAdded(ftInteger,'COUNT',8);
     Result := 'COUNT';
  end;
end;


function tMyData.GetFieldByNameAsFloat(FieldName : ANSIString) : float64;
begin
   {$IfDef BDELikeTables}
      if (TheBDEdata <> Nil) then Result := TheBDEdata.FieldByName(FieldName).AsFloat;
   {$EndIf}
   {$IfDef UseTCLientDataSet}
      if (TheClientDataSet <> Nil) then Result := TheClientDataSet.FieldByName(FieldName).AsFloat;
   {$EndIf}

   {$IfDef UseFireDacSQLlite}
      if (fdTable <> Nil) then Result := fdTable.FieldByName(FieldName).AsFloat;
   {$EndIf}
   {$IfDef UseFDMemTable}
       if (FDMemTable <> nil) then begin
           Result := fdMemTable.FieldByName(FieldName).AsFloat;
       end;
    {$EndIf}
end;


function tMyData.GetFieldByNameAsInteger(FieldName : ANSIString) : integer;
begin
   {$IfDef BDELikeTables}
      if (TheBDEdata <> Nil) then begin
         Result := TheBDEdata.FieldByName(FieldName).AsInteger;
      end;
   {$EndIf}
   {$IfDef UseTCLientDataSet}
      if (TheClientDataSet <> Nil) then Result := TheClientDataSet.FieldByName(FieldName).AsInteger;
   {$EndIf}
   {$IfDef UseFireDacSQLlite}
      if (fdTable <> Nil) then Result := fdTable.FieldByName(FieldName).AsInteger;
   {$EndIf}
   {$IfDef UseFDMemTable}
       if (FDMemTable <> nil) then begin
          Result := fdMemTable.FieldByName(FieldName).AsInteger;
       end;
   {$EndIf}
end;


function tMyData.GetFieldByNameAsString(FieldName : ANSIString) : ANSIString;
begin
   if (FieldName = '') then begin
      Result := '';
      exit;
   end;
   {$IfDef BDELikeTables}
      if (TheBDEdata <> Nil) then begin
         Result := TheBDEdata.FieldByName(FieldName).AsString;
      end;
   {$EndIf}
   {$IfDef UseTCLientDataSet}
      if (TheClientDataSet <> Nil) then Result := TheClientDataSet.FieldByName(FieldName).AsString;
   {$EndIf}
   {$IfDef UseFireDacSQLlite}
      if (fdTable <> Nil) then Result := fdTable.FieldByName(FieldName).AsString;
   {$EndIf}
   {$IfDef UseFDMemTable}
       if (FDMemTable <> nil) then begin
          Result := fdMemTable.FieldByName(FieldName).AsString;
       end;
   {$EndIf}
   Result := trim(Result);
end;


procedure tMyData.SetFieldByNameAsFloat(FieldName : ANSIString; value : float64; MaxDec : integer = -1);
var
   TStr : shortstring;
begin
   if Math.IsNaN(value) then exit;
   SetFieldByNameAsString(FieldName,TStr);

   if (MaxDec >= 0) then begin
      TStr := RealToString(value,-18,maxDec);
      SetFieldByNameAsString(FieldName,TStr);
   end
   else begin
      TStr := RealToString(value,-18,GetFieldPrecision(FieldName));
      SetFieldByNameAsString(FieldName,TStr);
   end;
end;


procedure tMyData.CarefullySetFloat(FieldName : ANSIString; var Value : float64; Precision : float64);  //inline;
begin
   //if (abs(Value) < Precision) then Value := 0;
   Value := round(Value / Precision) * Precision;
   SetFieldByNameAsFloat(FieldName,value);
end;

procedure tMyData.CarefullySetFloat32(FieldName : ANSIString; var Value : float32; Precision : float32);  //inline;
begin
   //if (abs(Value) < Precision) then Value := 0;
   Value := round(Value / Precision) * Precision;
   SetFieldByNameAsFloat(FieldName,value);
end;


procedure tMyData.SetFieldByNameAsString(FieldName : ANSIString; value : String);
begin
   {$IfDef BDELikeTables}
      if (TheBDEdata <> Nil) then begin
         TheBDEdata.FieldByName(FieldName).AsString := value;
         exit;
      end;
   {$EndIf}

   {$IfDef UseFireDacSQLlite}
      if (fdTable <> Nil) then begin
         fdTable.FieldByName(FieldName).AsString := value;
      end;
   {$EndIf}

   {$IfDef UseTCLientDataSet}
      if (TheClientDataSet <> Nil) then TheClientDataSet.FieldByName(FieldName).AsString := value;
   {$EndIf}

    {$IfDef UseFDMemTable}
       if (FDMemTable <> nil) then begin
          fdMemTable.FieldByName(FieldName).AsString := value;
       end;
    {$EndIf}
end;


function TMyData.ValidLatLongFromTable(var Lat,Long : float64) : boolean;
var
   TStr,TStr2: shortstring;
begin
   Lat := 0;
   Long := 0;
   Result := false;
   TStr := GetFieldByNameAsString('LAT');
   TStr2 := GetFieldByNameAsString('LONG');
   if (TStr = '') or (TStr2 = '') then exit;
   Lat := System.SysUtils.StrToFloat(TStr);
   Long := System.SysUtils.StrToFloat(TStr2);
   Result := (abs(Lat) > 0.00001) or (abs(Long) > 0.00001);
end;


procedure TMyData.LengthPointRecords(var CumDist,StraightDist,LineHeading : float64);
var
   Lat,Long,LastLat,LastLong,Dist,FirstLat,FirstLong : float64;
begin
   LastLat := 9999;
   CumDist := 0;
   First;
   while not eof do begin
      if ValidLatLongFromTable(Lat,Long) then begin
         if LastLat < 990 then begin
            VincentyCalculateDistanceBearing(LastLat,LastLong,Lat,Long,Dist,LineHeading);
            CumDist := CumDist + Dist;
         end
         else begin
            FirstLat := Lat;
            FirstLong := Long;
         end;
         LastLat := Lat;
         LastLong := Long;
         Next;
      end;
   end;
   VincentyCalculateDistanceBearing(FirstLat,FirstLong,LastLat,LastLong,StraightDist,LineHeading);
end;


function tMyData.GetXYZFromTable(var x,y,z : float64) : boolean;
begin
    Result := (GetFieldByNameAsString('X') <> '') and (GetFieldByNameAsString('Y') <> '');
    if Result then begin
       x := GetFieldByNameAsFloat('X');
       y := GetFieldByNameAsFloat('Y');
       if FieldExists('Z') and (GetFieldByNameAsString('Z') <> '') then z := GetFieldByNameAsFloat('Z');
    end;
end;


procedure tMyData.TrimAllStringFields(JustOne : ANSIString = '');
var
   i,Done,ToBeDone : integer;
   fName : ANSIString;
   ShowProg : boolean;

         procedure OneField(fName : ANSIstring);
         begin
             First;
             while not eof do begin
                Edit;
                SetFieldByNameAsString(fName,trim(GetFieldByNameAsString(fName)));
                inc(Done);
                if (Done mod 1000 = 0) then UpdateProgressBar(Done/ToBeDone);
                Next;
             end;
         end;

begin
   ShowProg := (RecordCount > MDDef.RecNumToShowDBProgress);
   if ShowProg then StartProgress('Trim DB fields');
    if (JustOne <> '') then begin
       ToBeDone := FiltRecsInDB;
       OneField(JustOne);
    end
    else begin
       ToBeDone := FiltRecsInDB * FieldCount;
       for i := 0 to pred(FieldCount) do begin
          if (GetFieldType(i) = ftString) then begin
             fName := GetFieldName(i);
             OneField(fName);
          end
          else begin
             Done := Done + FiltRecsInDB;
          end;
       end;
    end;
end;


procedure tMyData.SetColorFromPlatformColor(Color : tPlatformColor);
begin
   SetFieldByNameAsInteger('COLOR',ConvertPlatformColorToTColor(Color));
end;

procedure tMyData.SetColorFromTColor(Color : tColor);
var
   red,green,Blue : byte;

   procedure SetFields(RedName,BlueName,GreenName : shortstring);
   begin
      if IsFloatField(RedName) then begin
         SetFieldByNameAsFloat(RedName,255*Red);
         SetFieldByNameAsFloat(BlueName,255*Green);
         SetFieldByNameAsFloat(GreenName,255*Blue);
      end
      else begin
         SetFieldByNameAsInteger(RedName,Red);
         SetFieldByNameAsInteger(BlueName,Green);
         SetFieldByNameAsInteger(GreenName,Blue);
      end;
   end;

begin
   GetRGBfromTColor(Color,red,green,blue);
   if FieldExists('COLOR') then SetFieldByNameAsInteger('COLOR',Color);
   if FieldExists('RED') then SetFields('RED','GREEN','BLUE');
   if FieldExists('R') then SetFields('R','G','B');
end;


procedure tMyData.SetFieldByNameAsInteger(FieldName : ANSIString; value : integer);
begin
   {$IfDef BDELikeTables}
      if (TheBDEdata <> Nil) then TheBDEdata.FieldByName(FieldName).AsInteger := value;
   {$EndIf}
   {$IfDef UseFireDacSQLlite}
      if (fdTable <> Nil) then fdTable.FieldByName(FieldName).AsInteger := value;
   {$EndIf}
   {$IfDef UseFDMemTable}
       if (FDMemTable <> nil) then fdMemTable.FieldByName(FieldName).AsInteger := value;
   {$EndIf}

   {$IfDef UseTCLientDataSet}
      if (TheClientDataSet <> Nil) then TheClientDataSet.FieldByName(FieldName).AsInteger := value;
   {$EndIf}
end;


function tMyData.ListUniqueEntriesInDB(FieldName : ANSIString) : tStringList;
begin
   PetDBUtils.FindUniqueEntries(Self,FieldName,Result);
end;


function tMyData.NumUniqueEntriesInDB(FieldName : ANSIString) : integer;
var
   sl : tStringList;
begin
   sl := ListUniqueEntriesInDB(FieldName);
   Result := sl.count;
   sl.Free;
end;

{$IfDef ExactRecordCountFix}

function tMyData.ExactRecordCount : integer;
begin
   Result := 0;
   {$IfDef UseTDBF}
      if (TheBDEdata <> Nil) then Result := TheBDEdata.ExactRecordCount;
   {$Else}
      Result := RecordCount;
   {$EndIf}
end;
{$EndIf}

function tMyData.RecordCount : integer;
begin
   Result := 0;
   {$IfDef BDELikeTables}
      if (TheBDEdata <> Nil) then begin
         {$IfDef UseTDBF}
            Result := TheBDEdata.ExactRecordCount;
         {$Else}
            Result := RecordCount;
         {$EndIf}
      end;
   {$EndIf}

   {$IfDef UseTCLientDataSet}
      if (TheClientDataSet <> Nil) then begin
         Result := TheClientDataSet.RecordCount;
      end;
   {$EndIf}

   {$IfDef UseFireDacSQLlite}
      if (fdTable <> Nil) then Result := fdTable.RecordCount;
   {$EndIf}

    {$IfDef UseFDMemTable}
       if (FDMemTable <> nil) then begin
          Result := fdMemTable.RecordCount;
       end;
    {$EndIf}
end;


procedure tMyData.AssignEmpSource(EmpSource : TDataSource);
begin
   if (EmpSource <> Nil) then begin

      {$IfDef BDELikeTables}
         if (TheBDEdata <> Nil) then begin
            EmpSource.DataSet := TheBDEdata;
            EmpSource.Enabled := true;
         end;
      {$EndIf}
      {$IfDef UseTCLientDataSet}
         if (TheClientDataSet <> Nil) then EmpSource.DataSet := TheClientDataSet;
      {$EndIf}

      {$IfDef UseFireDacSQLlite}
         if FireDAC_SQL then EmpSource.DataSet := fdTable;
      {$EndIf}

       {$IfDef UseFDMemTable}
          if (FDMemTable <> nil) then begin
             EmpSource.DataSet := fdMemTable;
          end;
       {$EndIf}
   end;
end;

procedure tMyData.MarkRecordForDeletion;
var
   i : integer;
begin
   if FieldExists(RecNoFName) then begin
      Edit;
      i := GetFieldByNameAsInteger(RecNoFName);
      SetFieldByNameAsInteger(RecNoFName,-i);
   end;
   inc(RecsTaggedDeletion);
   Post;
end;


function tMyData.FieldAllBlanks(FieldName : ANSIString) : boolean;
begin
   Result := true;
   First;
   while not EOF do begin
      if (GetFieldByNameAsString(FieldName) <> '') then begin
         Result := false;
         exit;
      end;
      Next;
   end;
end;


function tMyData.FieldHasChar(FieldName : ANSIString; ch : shortstring) : boolean;
begin
   Result := false;
   First;
   while not EOF do begin
      if (GetFieldByNameAsString(FieldName) = ch) then begin
         Result := true;
         exit;
      end;
      Next;
   end;
end;


function tMyData.FieldAllZeros(FieldName : ANSIString) : boolean;
begin
   Result := true;
   First;
   while not EOF do begin
      if (GetFieldByNameAsString(FieldName) <> '0') then begin
         Result := false;
         exit;
      end;
      Next;
   end;
end;


function tMyData.GetFieldPrecision(fName : ANSIString) : integer;
var
   i : integer;
begin
   Result := 0;
   {$IfDef BDELikeTables}
      if (TheBDEdata <> Nil) {or DBFmovedToRAM} then begin
         for i := 0 to pred(FieldCount) do begin
            if (TheBDEdata.Fields[i].FieldName = fName) then begin
               Result := GetFieldPrecision(i);
               exit;
            end;
         end;
      end;
   {$EndIf}
end;


function tMyData.GetFieldPrecision(i : integer) : integer;
var
   f : file;
   DBaseIIITableFileHeader : tDBaseIIITableFileHeader;
   TableFieldDescriptor : tTableFieldDescriptor;
   i2 : integer;
begin
   Result := 0;
   {$IfDef BDELikeTables}
      if (TheBDEdata <> Nil) {or DBFmovedToRAM} then begin
         assignFile(f,FullTableName);
         reset(f,1);
         BlockRead(f, DBaseIIITableFileHeader, SizeOf(tDBaseIIITableFileHeader));
         for i2 := 0 to i do begin
            BlockRead(f, TableFieldDescriptor, SizeOf(tTableFieldDescriptor));
         end;
         Result := TableFieldDescriptor.FieldDecimalCount;
         CloseFile(f);
      end;
   {$EndIf}
end;


function tMyData.GetFieldType(fName : ANSIString) : tFieldType;
var
   i : integer;
begin
   {$IfDef BDELikeTables}
   if (TheBDEdata <> Nil) then begin
      for i := 0 to pred(FieldCount) do begin
         if (TheBDEdata.Fields[i].FieldName = fName) then begin
            Result := TheBDEdata.Fields[i].DataType;
            exit;
         end;
      end;
   end;
   {$EndIf}

   {$IfDef UseFireDacSQLlite}
   if (fdTable <> Nil) then begin
      for i := 0 to pred(FieldCount) do begin
         if (fdTable.Fields[i].FieldName = fName) then begin
            Result := fdTable.Fields[i].DataType;
            exit;
         end;
      end;
   end;
   {$EndIf}

    {$IfDef UseFDMemTable}
    if (FDMemTable <> nil) then begin
      for i := 0 to pred(FieldCount) do begin
         if (fdMemTable.Fields[i].FieldName = fName) then begin
            Result := fdMemTable.Fields[i].DataType;
            exit;
         end;
      end;
   end;
   {$EndIf}

   {$IfDef UseTCLientDataSet}
   if (TheClientDataSet <> Nil) then with TheClientDataSet do begin
      for i := 0 to pred(FieldCount) do begin
         if (Fields[i].FieldName = fName) then begin
            Result := Fields[i].DataType;
            exit;
         end;
      end;
   end;
   {$EndIf}
end;


function tMyData.GetFieldType(i : integer) : tFieldType;
begin
   {$IfDef BDELikeTables}
   if (TheBDEdata <> Nil) then begin
      Result := TheBDEdata.Fields[i].DataType;
      if (Result = ftFloat) then   begin
        if (GetFieldPrecision(i)) = 0 then Result := ftInteger;
      end;
   end;
   {$EndIf}

   {$IfDef UseFireDacSQLlite}
   if (fdTable <> Nil) then begin
      Result := fdTable.Fields[i].DataType;
      if (Result = ftFloat) then begin
        if (GetFieldPrecision(i)) = 0 then Result := ftInteger;
      end;
   end;
   {$EndIf}
    {$IfDef UseFDMemTable}
    if (FDMemTable <> nil) then begin
      Result := fdMemTable.Fields[i].DataType;
      if (Result = ftFloat) then begin
        if (GetFieldPrecision(i)) = 0 then Result := ftInteger;
      end;
    end;
    {$EndIf}

   {$IfDef UseTCLientDataSet}
   if (TheClientDataSet <> Nil) then with TheClientDataSet do begin
      Result := Fields[i].DataType;
   end;
   {$EndIf}

end;


function tMyData.FieldCount : integer;
begin
   {$IfDef BDELikeTables}
   if (TheBDEdata <> Nil) then Result := TheBDEdata.FieldCount;
   {$EndIf}

   {$IfDef UseFireDacSQLlite}
   if (fdTable <> Nil) then Result := fdTable.FieldCount;
   {$EndIf}
    {$IfDef UseFDMemTable}
    if (FDMemTable <> nil) then begin
       Result := fdMemTable.FieldCount;
    end;
    {$EndIf}

   {$IfDef UseTCLientDataSet}
    if (TheClientDataSet <> Nil) then Result := TheClientDataSet.FieldCount;
   {$EndIf}
end;


function tMyData.FieldsInDataBase(Alphabetize : boolean = false) : tStringList;
var
   i : integer;
begin
   Result := tStringList.Create;
   Result.Sorted := Alphabetize;
  {$IfDef BDELikeTables}
      if (TheBDEdata <> Nil) then begin
         for i := 0 to pred(FieldCount) do Result.Add(TheBDEdata.Fields[i].FieldName);
      end;
   {$EndIf}
   {$IfDef UseFireDacSQLlite}
      if (fdTable <> Nil) then begin
         for i := 0 to pred(FieldCount) do Result.Add(fdTable.Fields[i].FieldName);
      end;
   {$EndIf}

    {$IfDef UseFDMemTable}
       if (FDMemTable <> nil) then begin
           for i := 0 to pred(FieldCount) do Result.Add(fdMemTable.Fields[i].FieldName);
       end;
    {$EndIf}

   {$IfDef UseTCLientDataSet}
      if (TheClientDataSet <> Nil) then with TheClientDataSet do  begin
         for i := 0 to pred(FieldCount) do Result.Add(Fields[i].FieldName);
      end;
   {$EndIf}
 end;


function tMyData.GetFieldLength(WantFieldName : ANSIString) : integer;
var
     i : integer;
begin
   Result := -1;
   {$IfDef BDELikeTables}
      if (TheBDEdata <> Nil) then  begin
          for i := 0 to pred(FieldCount) do begin
             if (TheBDEdata.Fields[i].FieldName = WantFieldName) then begin
                Result := TheBDEdata.Fields[i].DataSize;
                exit;
             end;
          end;
      end;
   {$EndIf}

   {$IfDef UseFireDacSQLlite}
      if (fdTable <> Nil) then begin
          for i := 0 to pred(FieldCount) do begin
             if (fdTable.Fields[i].FieldName = WantFieldName) then begin
                Result := fdTable.Fields[i].DataSize;
                exit;
             end;
          end;
      end;
   {$EndIf}
    {$IfDef UseFDMemTable}
       if (FDMemTable <> nil) then  begin
          for i := 0 to pred(FieldCount) do begin
             if (fdMemTable.Fields[i].FieldName = WantFieldName) then  begin
                Result := fdMemTable.Fields[i].DataSize;
                exit;
             end;
          end;
       end;
    {$EndIf}

   {$IfDef UseTCLientDataSet}
      if (TheClientDataSet <> Nil) then with TheClientDataSet do begin
          for i := 0 to pred(FieldCount) do begin
             if (Fields[i].FieldName = WantFieldName) then begin
                Result := Fields[i].DataSize;
                exit;
             end;
          end;
      end;
   {$EndIf}
end;


function tMyData.FieldTypeAndLength(WantFieldName : ANSIString) : Ansistring;
var
   i : integer;
   TheType : tFieldType;
begin
   TheType := GetFieldType(WantFieldName);
   i := GetFieldLength(WantFieldName);
   case TheType of
      ftString : Result := 'String';
      ftSmallint : Result := 'SmallInt';
      ftInteger : Result := 'Integer';
      ftFloat : Result := 'float64';
      else Result := 'Other';
   end;
   Result := Result + ' (' + IntToStr(i) + ')';
end;


function tMyData.FieldExists(WantFieldName : ANSIString; NeedAType : boolean = false; NeedType : tFieldType = ftFloat) : boolean;
var
   i : integer;
begin
   Result := false;
   if (FieldCount = 0) then exit;

   {$IfDef BDELikeTables}
      if (TheBDEdata <> Nil) then begin
          for i := 0 to pred(FieldCount) do begin
             if (TheBDEdata.Fields[i].FieldName = WantFieldName) then begin
                if NeedAType then begin
                   if (TheBDEdata.Fields[i].DataType = NeedType) then begin
                      Result := true;
                      exit;
                   end;
                end
                else begin
                   Result := true;
                   exit;
                end;
             end;
          end;
      end;
   {$EndIf}

   {$IfDef UseFireDacSQLlite}
      if (fdTable <> Nil) then begin
         for i := 0 to pred(FieldCount) do if (fdTable.Fields[i].FieldName = WantFieldName) then begin
            if NeedAType then begin
               if (fdTable.Fields[i].DataType = NeedType) then begin
                  Result := true;
                  exit;
               end;
            end
            else begin
               Result := true;
               exit;
            end;
         end;
      end;
   {$EndIf}

    {$IfDef UseFDMemTable}
       if (FDMemTable <> nil) then begin
         for i := 0 to pred(FieldCount) do if (fdMemTable.Fields[i].FieldName = WantFieldName) then begin
            if NeedAType then begin
               if (fdMemTable.Fields[i].DataType = NeedType) then begin
                  Result := true;
                  exit;
               end;
            end
            else begin
               Result := true;
               exit;
            end;
         end;
       end;
    {$EndIf}

   {$IfDef UseTCLientDataSet}
      if (TheClientDataSet <> Nil) then with TheClientDataSet do begin
         for i := 0 to pred(FieldCount) do if (Fields[i].FieldName = WantFieldName) then  begin
            if NeedAType then begin
               if (Fields[i].DataType = NeedType) then begin
                  Result := true;
                  exit;
               end;
            end
            else  begin
               Result := true;
               exit;
            end;
         end;
      end;
   {$EndIf}
end;


function tMyData.FieldSum(FieldDesired : shortstring) : float64;
var
   z : float64;
begin
   Result := 0;
   First;
   repeat
      if CarefullyGetFieldByNameAsFloat64(FieldDesired,z) then Result := Result + z;
      Next;
   until EOF;
end;


function tMyData.FieldAverage(FieldDesired : shortstring) : float64;
begin
   Result := GetFieldStatistics(FieldDesired).Mean;
end;


function tMyData.FieldStdDev(FieldDesired : shortstring) : float64;
begin
   Result := GetFieldStatistics(FieldDesired).std_dev;
end;



function tMyData.GetFieldStatistics(FieldDesired : ShortString) : tMomentVar;
var
   zs : ^bfarray32;
   z : float64;
 begin
   New(zs);
   Result.NPts := 0;
   First;
   repeat
      if CarefullyGetFieldByNameAsFloat64(FieldDesired,z) then begin
         zs^[Result.NPts] := z;
         inc(Result.NPts);
      end;
      Next;
   until EOF;
   if (Result.Npts > 1) then begin
      moment(zs^,Result,msAll);
   end
   else InitializeMomentVar(Result);
   Dispose(zs);
   EndProgress;
end;



function tMyData.FieldMedian(FieldDesired : shortstring) : float64;
begin
   Result := GetFieldStatistics(FieldDesired).Median;
end;


function tMyData.IsNumericField(WantFieldName : ANSIString) : boolean;
begin
    Result := GetFieldType(WantFieldName) in [ftFloat, ftInteger, ftSmallInt, ftLargeInt];
end;

function tMyData.IsFloatField(WantFieldName : ANSIString) : boolean;
begin
    Result := GetFieldType(WantFieldName) in [ftFloat];
end;

function tMyData.IsIntegerField(WantFieldName : ANSIString) : boolean;
begin
    Result := GetFieldType(WantFieldName) in [ftInteger,ftSmallInt, ftLargeInt];
end;

function tMyData.IsStringField(WantFieldName : ANSIString) : boolean;
begin
    Result := GetFieldType(WantFieldName) in [ftString];
end;

function tMyData.IsStringOrIntergerField(WantFieldName : ANSIString) : boolean;
begin
    Result := GetFieldType(WantFieldName) in [ftString, ftInteger, ftSmallInt, ftLargeInt];
end;


{$IfDef VCL}

   function tMyData.PickField(Mess: ShortString; TypesAllowed : tSetFieldType) : ShortString;
   var
     FieldsInDB : tStringList;
     WantField,i  : integer;
   begin
      FieldsInDB := tStringList.Create;
      for i := 0 to pred(FieldCount) do begin
         if (GetFieldType(i) in TypesAllowed) then begin
            FieldsInDB.Add(GetFieldName(i));
         end;
      end;
      if (FieldsInDB.Count = 0) then Result := ''
      else begin
         WantField := 0;
         if MultiSelectSingleColumnStringList('Database Field for ' + Mess,WantField,FieldsInDB,true) then begin
            Result := FieldsInDB.Strings[WantField];
         end
         else Result := '';
      end;
      FieldsInDB.Free;
   end;


procedure tMyData.DefineFontFromTable(Font : Graphics.tFont);
begin
    Font.Name := GetFieldByNameAsString('FONT_NAME');
    Font.Size := GetFieldByNameAsInteger('FONT_SIZE');
    Font.Color := GetFieldByNameAsInteger('FONT_COLOR');
    Font.Style := [];
    if FieldExists('FONT_BOLD') then begin
       if GetFieldByNameAsString('FONT_BOLD') = 'B' then Font.Style := Font.Style + [fsBold];
       if GetFieldByNameAsString('FONT_ITAL') = 'I' then Font.Style := Font.Style + [fsItalic];
       if GetFieldByNameAsString('FONT_UNDER') = 'U' then Font.Style := Font.Style + [fsUnderline];
    end;
end;

procedure tMyData.PostFont(aFont : Graphics.tFont);
var
   ch : ShortString;
begin
   FileChanged := true;
   Edit;
   SetFieldByNameAsString('FONT_NAME',aFont.Name);
   SetFieldByNameAsInteger('FONT_SIZE',aFont.Size);
   SetFieldByNameAsInteger('FONT_COLOR',aFont.Color);
   if fsBold in aFont.Style then ch := 'B' else ch := '';
   SetFieldByNameAsString('FONT_BOLD',ch);
   if fsItalic in aFont.Style then ch := 'I' else ch := '';
   SetFieldByNameAsString('FONT_ITAL',ch);
   if fsUnderline in aFont.Style then ch := 'U' else ch := '';
   SetFieldByNameAsString('FONT_UNDER',ch);
end;

{$EndIf}

procedure tMyData.DefinePointSymbol(var Symbol : tDrawingSymbol; var SymbolSize : byte; var SymbolColor : tPlatformColor);
begin
   if FieldExists('SYM_TYPE') and (GetFieldByNameAsString('SYM_TYPE') <> '') then Symbol := tDrawingSymbol(GetFieldByNameAsInteger('SYM_TYPE'))
   else Symbol := FilledBox;
   if FieldExists('SYM_SIZE') and (GetFieldByNameAsString('SYM_SIZE') <> '') then SymbolSize := GetFieldByNameAsInteger('SYM_SIZE')
   else SymbolSize := 2;
   if FieldExists('SYM_COLOR') and (GetFieldByNameAsString('SYM_COLOR') <> '') then SymbolColor := ConvertTColorToPlatFormColor(GetFieldByNameAsInteger('SYM_Color'))
   else SymbolColor := claRed;
end;


procedure tMyData.PostPointSymbol(Symbol : tDrawingSymbol; SymbolSize : byte; SymbolColor : tPlatformColor);
begin
    SetFieldByNameAsInteger('SYM_TYPE',ord(Symbol));
    SetFieldByNameAsInteger('SYM_SIZE',SymbolSize);
    SetFieldByNameAsInteger('SYM_COLOR',ConvertPlatformColorToTColor(SymbolColor));
end;


procedure tMyData.DefinePointSymbol(var Symbol : tFullSymbolDeclaration);
begin
    DefinePointSymbol(Symbol.DrawingSymbol,Symbol.Size,Symbol.Color);
end;

procedure tMyData.PostPointSymbol(Symbol : tFullSymbolDeclaration);
begin
    PostPointSymbol(Symbol.DrawingSymbol,Symbol.Size,Symbol.Color);
end;



procedure tMyData.GetLineColorAndWidth(var LineColor : tPlatFormColor; var LineSize : integer);
begin
   LineColor := ConvertTColorToPlatFormColor(GetFieldByNameAsInteger('LINE_COLOR'));
   LineSize := GetFieldByNameAsInteger('LINE_WIDTH');
end;

procedure tMyData.SetLineColorAndWidth(LineColor : tPlatFormColor; LineSize : integer);
begin
   SetFieldByNameAsInteger('LINE_WIDTH',LineSize);
   SetFieldByNameAsInteger('LINE_COLOR',ConvertPlatformColorToTColor(LineColor));
end;


function tMyData.MakeImageTag(ThumbnailDir : PathStr; FieldName : ANSIString) : ANSIString;
var
   fName,tName,NewName : PathStr;
   MenuStr,TStr : shortString;
   Ext : ExtStr;
begin
   Result := '';
   fName := GetFieldByNameAsString(FieldName);
   Ext := UpperCase(ExtractFileExt(fName));
   Ext := LowerCase(Ext);

   if not(FileExists(fName)) then fName := ExtractFilePath(FullTableName) + fName;
   NewName := ExtractFileNameNoExt(fName) + Ext;
   if FileExists(fName) then begin
      if (MDDef.KMLImageOpts in [0,1]) then begin
         CopyFile(fName,ThumbNailDir + NewName);
      end;

      if (MDDef.KMLImageOpts = 0) then begin
         Result := ' <br> <img border="0" src="' + NewName + '">' + NewName + '<br>';
      end
      else begin
         tName := ThumbNailDir + 'tn-' +  NewName;
         {$IfDef KML} WriteLineToDebugFile('create thumbnail= ' + tName); {$EndIf}
         {$IfDef VCL}
         PetImage.CreateThumbNail(fName,tName,MDDef.ThumbSize,MDDef.tnQuality);
         {$EndIf}
         if (MDDef.KMLImageOpts = 1) then begin
            MenuStr := '<a href="' + NewName + '">';
            TStr := '</a>';
         end
         else begin
            MenuStr := '';
            TStr := '';
         end;
         Result := '<br>'  + MenuStr + '<img border="0" src="' + ExtractFileName(tName) +  '">' + TStr + '<br>';
      end;
   end
   else begin
      {$IfDef RecordProblems} WriteLineToDebugFile('Image file missing: ' + fName); {$EndIf}
   end;
end;

function tMyData.BoundingBoxPresent : boolean;
begin
   Result := FieldExists('LAT_HI') and FieldExists('LONG_HI') and FieldExists('LAT_LOW') and FieldExists('LONG_LOW');
end;

function tMyData.DBBakDir : PathStr;
begin
   Result := ExtractFilePath(FullTableName)  + 'db_bak' + PathDelim;
   SafeMakeDir(Result);
end;


{$IfDef VCL}
procedure tMyData.StartRewrite(var OldName : PathStr);
var
   fName : PathStr;
begin
   fName := FullTableName;
   OldName := DBbakDir + ExtractFileNameNoExt(fName) + '_bak_' + CurrentTimeForFileName + DefaultDBExt;

   {$IfDef DBrewrite} WriteLineToDebugFile('tMyData.StartRewrite OldName= ' + ExtractFileName(OldName) + '  recs=' + IntToStr(RecordCount)); {$EndIf}

   TheBDEdata.Close;
   TheBDEdata.Destroy;
   TheBDEdata := Nil;
   {$IfDef DBrewrite} WriteLineToDebugFile('TheBDEdata close and nil; now copy and delete=' + ExtractFileName(fName)); {$EndIf}

   CopyFile(fName,OldName);
   repeat
       Delay(100);
      {$IfDef DBrewrite} WriteLineToDebugFile('in copy loop ' + ExtractFileName(fName)); {$EndIf}
   until FileExists(oldName);

   repeat
       Delay(100);
       {$IfDef DBrewrite} WriteLineToDebugFile('in delete loop ' + ExtractFileName(fName)); {$EndIf}
    until (not System.SysUtils.DeleteFile(fName));
end;


procedure FillNewDBF(OldName,NewName : PathStr; CheckDeletes : boolean = false);
var
   i,j : integer;
   OldTable,NewTable : tMyData;
   TStr,NewValues : shortString;
begin
   {$IfDef DBrewrite} WriteLineToDebugFile('FillNewDBF in ' + ExtractFileName(OldName) + ' to ' + ExtractFileName(NewName)); {$EndIf}
   OldTable := tMyData.Create(OldName);
   NewTable := tMyData.Create(NewName);
   CheckDeletes := CheckDeletes and OldTable.FieldExists(RecNoFName);
   {$IfDef VCL} if ShowSatProgress then StartProgress('Fill fields ' + ExtractFileName(NewName)); {$EndIf}
   j := 0;
   while Not OldTable.eof do begin
      {$IfDef VCL} if ShowSatProgress and (j mod 100 = 0) then UpdateProgressBar(j/OldTable.TotRecsInDB); {$EndIf}
      if (not CheckDeletes) or (OldTable.GetFieldByNameAsInteger(RecNoFName) > 0)  then begin
         NewTable.Insert;
         for i := 0 to pred(OldTable.FieldCount) do begin
               tStr := OldTable.GetFieldName(i);
               if NewTable.FieldExists(tstr) then begin
                  if tStr = 'REC_ID' then begin
                     NewTable.SetFieldByNameAsInteger(TStr,j);
                  end
                  else begin
                     NewValues := OldTable.GetFieldByNameAsString(TStr);
                     if (NewValues <> '') then NewTable.SetFieldByNameAsString(TStr,NewValues);
                  end;
               end;
         end;
         NewTable.Post;
         inc(j);
      end;
      OldTable.Next;
   end;
   NewTable.Destroy;
   OldTable.Destroy;
   {$IfDef VCL} if ShowSatProgress then EndProgress; {$EndIf}
   {$IfDef DBrewrite} WriteLineToDebugFile('FillNewDBF out, recs written='  + IntToStr(j)); {$EndIf}
end;


procedure tMyData.EndRewrite(var OldName,fName : PathStr);
begin
   {$IfDef DBrewrite} WriteLineToDebugFile('tMyData.EndRewrite in, old=' + OldName + ' new=' + fName); {$EndIf}
   FillNewDBF(OldName,fName,CheckDeletes);
   {$IfDef DBrewrite} WriteLineToDebugFile('tMyData.EndRewrite all added'); {$EndIf}
   if not MDDef.SaveIntermediateDBs then begin
      if not System.SysUtils.DeleteFile(OldName) then begin
         {$IfDef RecordFieldPresent} WriteLineToDebugFile('failed to delete= ' + OldName + '  error = '+ IntToStr(GetLastError)); {$EndIf}
      end;
   end;
   CreateAndOpenTable(Self.TheBDEdata,fName);
   TotRecsInDB := Self.TheBDEdata.RecordCount;
   CheckDeletes := false;
   {$IfDef DBrewrite} WriteLineToDebugFile('tMyData.EndRewrite out'); {$EndIf}
end;
{$EndIf}


function tMyData.PurgeDeletedRecords : boolean;
begin
   if (RecsTaggedDeletion > 0) then begin
      CheckDeletes := true;
      Result := DeleteField('');
      RecsTaggedDeletion := 0;
   end
   else Result := false;
end;


function tMyData.DeleteField(theField : shortstring) : boolean;
{$IfDef BDELikeTables}
var
   fName,OldName : PathStr;
   CreateDataBase : tCreateDataBase;
begin
   {$IfDef DBrewrite} WriteLineToDebugFile('tMyData.DeleteField, field=' + theField + ' in ' + ExtractFileName(FullTableName)); {$EndIf}
   Result := false;
   if (TheBDEdata <> Nil) {and FieldExists(theField)} then begin
      StartRewrite(OldName);
      fName := FullTableName;
      CreateDataBase := tCreateDataBase.Create(fName);
      CreateDataBase.ReadDBFHeader(OldName);
      CreateDataBase.RemoveAField(theField);
      CreateDataBase.WriteCorrectHeader(true);
      EndRewrite(OldName,fName);
      Result := true;
    end;
   {$IfDef DBrewrite} WriteLineToDebugFile('tMyData.DeleteField, out'); {$EndIf}
{$Else}
begin
   Result := false;
{$EndIf}
end;


function tMyData.TrimField(theField : shortstring; NewLength : integer) : boolean;
{$IfDef BDELikeTables}
var
   fName,OldName : PathStr;
   CreateDataBase : tCreateDataBase;
begin
   {$IfDef DBrewrite} WriteLineToDebugFile('tMyData.TrimField, field=' + theField + ' in ' + ExtractFileName(FullTableName)); {$EndIf}
   Result := false;
   if (TheBDEdata <> Nil) and FieldExists(theField) then begin
      fName := FullTableName;
      StartRewrite(OldName);
      CreateDataBase := tCreateDataBase.Create(fName);
      CreateDataBase.ReadDBFHeader(OldName);
      CreateDataBase.ChangeFieldLength(theField,newLength);
      CreateDataBase.WriteCorrectHeader(true);
      CreateDataBase.Destroy;
      EndRewrite(OldName,fName);
      Result := true;
   end;
   {$IfDef DBrewrite} WriteLineToDebugFile('tMyData.TrimField, out'); {$EndIf}
{$Else}
begin
   Result := false;
{$EndIf}
end;


function tMyData.AddBoundingBox : boolean;
{$IfDef BDELikeTables}
var
   fName,OldName : PathStr;
   CreateDataBase : tCreateDataBase;
begin
   Result := false;
   if (TheBDEdata <> Nil) then begin
      fName := FullTableName;
      StartRewrite(OldName);
      CreateDataBase := tCreateDataBase.Create(fName);
      CreateDataBase.ReadDBFHeader(OldName);
      CreateDataBase.AddBoundingBoxToTable;
      CreateDataBase.WriteCorrectHeader(true);
      CreateDataBase.Destroy;
      EndRewrite(OldName,fName);
      Result := true;
   end;
{$Else}
begin
   Result := false;
{$EndIf}
end;


function tMyData.GetRecordBoundingBox : sfBoundBox;
begin
   Result.xMin := GetFieldByNameAsFloat('LONG_LOW');
   Result.xMax := GetFieldByNameAsFloat('LONG_HI');
   Result.yMin := GetFieldByNameAsFloat('LAT_LOW');
   Result.yMax := GetFieldByNameAsFloat('LAT_HI');
end;


procedure tMyData.SetBoundingBox(bbox : sfBoundBox);
begin
   SetFieldByNameAsFloat('LONG_LOW',bbox.XMin);
   SetFieldByNameAsFloat('LONG_HI',bbox.XMax);
   SetFieldByNameAsFloat('LAT_LOW',bbox.YMin);
   SetFieldByNameAsFloat('LAT_HI',bbox.YMax);
end;


procedure tMyData.SetAllFieldsBlank;
var
   i : integer;
   fName : shortstring;
begin
   Edit;
   for i := 0 to pred(FieldCount) do begin
      fName := GetFieldName(i);
      if (fName <> 'LAT') and (fName <> 'LONG') and (fName <> RecNoFName)then SetFieldByNameAsString(GetFieldName(i),'');
   end;
   SetFieldByNameAsString('LAT','');
   SetFieldByNameAsString('LONG','');
   Post;
end;

procedure tMyData.CopyRecordToEndOfTable(DeleteRecord : boolean = false);
var
   i : integer;
   TempRec : tStringList;
   TStr    : AnsiString;
begin
   {$IfDef RecordFieldPresent} WriteLineToDebugFile('tMyData.CopyRecordToEndOfTable in'); {$EndIf}

   TempRec := tStringList.Create;
   for i := 0 to pred(FieldCount) do begin
      TempRec.Add(Trim(GetFieldByNameAsString(GetFieldName(i))));
   end;
   {$IfDef RecordFieldPresent} WriteLineToDebugFile('temprec created'); {$EndIf}

   if DeleteRecord then begin
      {$IfDef RecordFieldPresent} WriteLineToDebugFile('must delete'); {$EndIf}
      Edit;
      Delete;
      {$IfDef RecordFieldPresent} WriteLineToDebugFile('record deleted'); {$EndIf}
   end;

   Last;

   {$IfDef RecordFieldPresent} WriteLineToDebugFile('after last'); {$EndIf}

   Insert;
   {$IfDef RecordFieldPresent} WriteLineToDebugFile('write new rec'); {$EndIf}
   for i := 0 to pred(FieldCount) do begin
      TStr :=  TempRec.Strings[i];
      if (TStr <> '') then SetFieldByNameAsString(GetFieldName(i),TStr);
   end;
   Post;
   TempRec.Free;
   {$IfDef RecordFieldPresent} WriteLineToDebugFile('tMyData.CopyRecordToEndOfTable out'); {$EndIf}
end;


function tMyData.GetTableStructure : tStringList;
var
   i,Size : integer;
   TStr,TStr2 : ShortString;
begin
   Result := tStringList.Create;
   Result.Add('FIELD_NAME,FIELD_TYPE,LENGTH,DECIMALS');
   for i := 0 to pred(FieldCount) do begin
      TStr := FieldTypeStr(GetFieldType(i));
      TStr2 := '';
      if (GetFieldType(i) = ftFloat) then begin
         Size := GetFieldPrecision(i);
         if (Size = 0) then TStr := 'Integer'
         else TStr2 := IntToStr(Size);
      end;
      Result.Add(GetFieldName(i) + ',' + TStr + ',' + IntToStr(GetFieldDataSize(i)) + ',' + TStr2);
   end;
end;


procedure tMyData.ExportToXML(fName : PathStr);
{$IfDef UseTCLientDataSet}
var
   ClientDataSet : TClientDataSet;
   format : TDataPacketFormat;
   fLen,i,j : integer;
   f2Name : ANSIString;
begin
   if (fName = '') then begin
      fName := ChangeFileExt(FullTableName,'.xml');
      if not GetFileNameDefaultExt('xml database file','XML file|*.xml|Client data set|*.cds',fName) then exit;
   end;
   if FileExtEquals(fName, '.XML') then format := dfXML
   else Format := dfBinary;

   if (TheClientDataSet <> Nil) then begin
       TheClientDataSet.MergeChangeLog;
       TheClientDataSet.SaveToFile(fName,Format);
   end
   else begin
       ClientDataSet := TClientDataSet.Create(Nil);
       for i := 0 to pred(FieldCount) do begin
           if (GetFieldType(i) in [ftString]) then fLen := GetFieldDataSize(I)
           else fLen := 0;
           ClientDataSet.FieldDefs.Add(GetFieldName(i),GetFieldType(i), fLen, False);
       end;
       ClientDataSet.FieldDefs.Add(RecNoFName,ftInteger,0, False);
       ClientDataSet.CreateDataset;
       ClientDataSet.Open;
       First;
       j := 0;
       if ShowSatProgress then StartProgress('Save ' + ExtractFileExt(fName));
       while not EOF do begin
          inc(j);
          if ShowSatProgress and (j mod 500 = 0) then UpdateProgressBar(j/RecordCount);
          ClientDataSet.Append;
          for i := 0 to pred(FieldCount) do begin
             f2Name := GetFieldName(i);
             ClientDataSet.FieldByName(f2Name).AsString := GetFieldByNameAsString(f2Name);
          end;
          ClientDataSet.FieldByName(RecNoFName).AsInteger := j;
          ClientDataSet.Post;
          Next;
       end;
       if ShowSatProgress then EndProgress;
       ClientDataSet.SaveToFile(fName,Format);
       ClientDataset.Free;
   end;
   {$Else}
   begin
   {$EndIf}
end;


procedure tMyData.ExportToSQLite(fName : PathStr = '');
var
   {$IfDef UseFireDacSQLlite}
   CreateDataBase : tCreateDataBase;
   NewDB : tMyData;
   {$EndIf}

   i: integer;
   TStr : shortString;
begin
    {$IfDef RecordSQLite} WriteLineToDebugFile('tMyData.ExportToSQLite in'); {$EndIf}

    if (fName = '') then fName := ChangeFileExt(FullTableName,'.db');

    if FileExists(fName) then begin
       if not AnswerIsYes('File exists; overwrite') then exit;
       System.SysUtils.DeleteFile(fName);
    end;

    {$IfDef RecordSQLite} WriteLineToDebugFile('   output= ' + fName); {$EndIf}

    {$IfDef UseFireDacSQLlite}
       {$IfDef RecordSQLite} WriteLineToDebugFile('UseFireDacSQLlite, call CreateDataBase'); {$EndIf}
       CreateDataBase := tCreateDataBase.Create(fName);

       for i := 0 to pred(FieldCount) do begin
          CreateDataBase.AddAField(GetFieldName(i), GetFieldType(i), GetFieldDataSize(I), GetFieldPrecision(i) );
       end;
       CreateDataBase.WriteCorrectHeader;

       {$IfDef RecordSQLite}
       WriteLineToDebugFile('NewDB := tMyData.Create(fName)');
       {$EndIf}

       NewDB := tMyData.Create(fName);
       First;
       while not eof do begin
          NewDB.Insert;
          for i := 0 to pred(FieldCount) do begin
             TStr := trim(GetFieldByNameAsString(GetFieldName(i)));
             NewDB.SetFieldByNameAsString(GetFieldName(i),TStr);
          end;
          NewDB.Post;
          Next;
       end;
       NewDB.Destroy;
    {$Else}
       {$IfDef RecordSQLite} WriteLineToDebugFile('UseFireDacSQLlite not allowed; nothing happens'); {$EndIf}
    {$EndIf}

    {$IfDef RecordSQLite} WriteLineToDebugFile('tMyData.ExportToSQLite out'); {$EndIf}
end;



{$IfDef BDELikeTables}

   function CreateAndOpenTable(var TheData : tMyTable; fName : PathStr) : boolean;
   var
      OK,FirstError : boolean;
      Ext : ExtStr;
   begin
      {$If Defined(ListOpenDB) or Defined(RecordOpenDBProblems)} if (UpperCase(ExtractFilePath(fName)) <> UpperCase(MDTempDir))then WriteLineToDebugFile('CreateAndOpenTable:  ' + fName); {$EndIf}
      Result := false;

      if FileExists(fName) then begin
         Ext := UpperCase(ExtractFileExt(fName));
         if ExtEquals(Ext,'.CSV') then begin
            CSVFileImportToDB(fName);
            fName := ChangeFileExt(fName,DefaultDBExt);
         end;

         if ExtEquals(Ext,DefaultDBExt) then begin
           FirstError := true;
           repeat
              InsureFileIsNotReadOnly(fName);
              TheData := tMyTable.Create(nil);
              TheData.ReadOnly := false;
              TheData.Tablename := fName;

               OK := true;
               try
                  TheData.Open;
               except
                  on Exception do begin
                     TheData.Destroy;
                     OK := false;
                     if FirstError then begin
                        RepairDBaseFieldNames(fName);
                        FirstError := false;
                     end
                     else begin
                        MessageToContinue('Corrupt DBF file ' + fName);
                        exit;
                     end;
                  end;
               end;
           until OK;
         end;

         if MDDef.DBfilterCaseInSensitive then TheData.FilterOptions := [foCaseInsensitive]
         else TheData.FilterOptions := [];
         Result := true;
      end;
   end;
{$EndIf}


initialization
finalization
   {$IfDef RecordMYDataCreation} WriteLineToDebugFile('RecordMYDataCreation active in petmar_db'); {$EndIf}
   {$IfDef RecordMYDataFilter} WriteLineToDebugFile('RecordMyDataFilter active in petmar_db'); {$EndIf}
   {$IfDef RecordSQLite} WriteLineToDebugFile('RecordSQLite active in petmar_db'); {$EndIf}
   {$IfDef TrackCDStiming} WriteLineToDebugFile('TrackCDStiming active in petmar_db'); {$EndIf}
end.


























